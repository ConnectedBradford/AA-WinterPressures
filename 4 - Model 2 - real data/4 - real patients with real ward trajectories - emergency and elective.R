## Second model using SUS data
## Models individual wards and whole hospital, all have infinite capacity
## Inputs 
##  (1) Generated frequency of admissions (time of change, rate as patients/24hrs, for elective and emergency care)
##  (2) Dataframes of patients for elective and emergency care - using SUS data
##  Tick rate is in seconds
## 
## DONE - change fixed 10 day stay to pull a real patient and use their stay
## DONE - add wards and allow patients to follow a basic pathway (presently still infinite, and all patients are emergency surgical)
## DONE - add elective admissions
## TODO - add ward capacity
## TODO - add fallback to less-preferred wards

##depends on SUSv2-byepisode elective and emergency in "2" directory

## Uses Rcpp as original approach is very slow because of the repeated filtering required to pull a real patient each time we want to admit one
##  v2 - generate all the admission intervals in advance (create a new table of interarrival times and absolute times)


set.seed(12346)
this.dir <-dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(dplyr)
library(simmer)
library(simmer.plot)
library(progress)
library(data.table)
library(Rcpp)
select<-simmer::select
library(timeDate)
library(bizdays) ##this library is way quicker than timeDate (but we have to use timeDate's calendars for some reason)
load_rmetrics_calendars(2000:2022) ##nb we only get these holidays so may need extending in future

#source("mclapply.hack.R")


## files generated in step 2. Need to ensure dates match up with the two generators!
emergency_freq <- readRDS("../Data - For Modelling/Emergency-Frequency.rds")
elective_freq <- readRDS("../Data - For Modelling/Elective-Frequency.rds")

emergency_spells <- readRDS("../Data - For Modelling/Emergency-Spells-Linked.rds")
elective_spells <- readRDS("../Data - For Modelling/Elective-Spells-Linked.rds")

combined_episodes <- readRDS("../Data - For Modelling/Both-Episodes-Linked.rds")


emergency_spells$duration<-as.numeric(emergency_spells$`_Discharge_DateTime`)-as.numeric(emergency_spells$`_SpellStart_DateTime`)

emergency_spells$duration[is.na(emergency_spells$duration)]<-4*86400
##fudge 4 day stay for anyone we don't know

elective_spells$duration<-as.numeric(elective_spells$`_Discharge_DateTime`)-as.numeric(elective_spells$`_SpellStart_DateTime`)

elective_spells$duration[is.na(elective_spells$duration)]<-4*86400
##fudge 4 day stay for anyone we don't know


combined_episodes$`_EpisodeEnd_Offset`[is.na(combined_episodes$`_EpisodeEnd_Offset`)]<-4*86400
print("* Main data loaded *")

source("Trajectories.R")
source("Wards.R")



## truncate for coding purposes
#emergency_freq<-head(emergency_freq,20)
#elective_freq<-head(elective_freq,2)





##base a progress bar on the emergency admissions as they're the biggest and most detailed
pb <- progress_bar$new(format="[:bar] :percent eta: :eta",total=nrow(emergency_freq))



##new version - generate a table of times for use later
emergency_gen_table <- function() {
  pb <- progress_bar$new(format="[:bar] :percent eta: :eta",total=nrow(emergency_freq)+1)
  last_time <- 0 ##last time we generated
  next_change <- as.numeric(emergency_freq[1,]$dateTime) ## next rate change
  cur_freq <- 0 ##current frequency (in "per day" units)
  t_skip <- 0 ##time skipped due to zero frequency, or because next admission was after a rate change
  freq_table_index <- 1 ##position in the frequency table
  gaps <- vector()
  time_at <- vector()
  
  repeat {
    while (cur_freq==0) {
      ##if frequency is ever zero (true at the start), keep skipping until we hit a positive frequency
      ##t_skip represents how long we've skipped before the current gap
      t_skip <- next_change - last_time
      cur_freq <- emergency_freq[freq_table_index,]$`_correctedN`
      next_change <- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
      freq_table_index <- freq_table_index+1
      pb$tick()
      if (freq_table_index==nrow(emergency_freq)) { break }
    }
    
    ##main generator here - rates in the files are per day, assume poisson process
    tmp_gap <- rexp(1,cur_freq/86400)+t_skip
    
    ## if the next admission would occur after a rate change, skip it. Take advantage of the fact Poisson processes have no memory to start a new one at the rate change (this has variable mathematical validity!)
    if ((tmp_gap + last_time)>next_change) {
      t_skip <- next_change - last_time
      cur_freq <- emergency_freq[freq_table_index,]$`_correctedN`
      next_change <- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
      freq_table_index <- freq_table_index+1
      pb$tick()
      #print(freq_table_index)
      if (freq_table_index==nrow(emergency_freq)) { break }
    } else {
      t_skip<-0
      last_time<-last_time+tmp_gap
      gaps <- c(gaps,tmp_gap)
      time_at <- c(time_at,last_time)
    }
    
    
  }
  spell_id<-0
  return(data.frame(gaps,time_at,spell_id))
}



##elective frequency table works differently - just a poisson mean for each day; we'll generate admissions on that day as required.
##days not in the frequency file will be ignored (ie no admissions)

elective_gen_table <- function() {
  pb <- progress_bar$new(format="[:bar] :percent eta: :eta",total=nrow(elective_freq)+1)
  cur_time <- 0 ##current time
  last_time <- 0 ##last time we generated
  cur_freq <- 0 ##current frequency (in "per day" units)
  freq_table_index <- 1 ##position in the frequency table
  gaps <- vector()
  time_at <- vector()

  repeat {
    cur_freq <- elective_freq[freq_table_index,]$`_correctedN`
    cur_time <- as.numeric(elective_freq[freq_table_index,]$dateTime)+7*3600 ##admissions at 0700
    gen_n <- rpois(1,cur_freq)
    if (gen_n>0) {
      gaps<-c(gaps,cur_time-last_time)
      time_at<-c(time_at,cur_time)
      last_time<-cur_time
      gaps<-c(gaps,rep(0,gen_n-1))
      time_at<-c(time_at,rep(cur_time,gen_n-1))
    }
    freq_table_index <- freq_table_index+1
    pb$tick()
    if (freq_table_index==nrow(elective_freq)) { break }
  }
  print("test")
  spell_id<-0
  return(data.frame(gaps,time_at,spell_id))
}


## cpp functions are hidden in here

sourceCpp("1RcppFunctions.cpp")



## function to produce a patient from an index time
## will return a patient's entire row
## presently indexed by - business day, time +/- n hours, date +/- n weeks
## old version worked but is quite slow due to the long and repeated lookups. Might be faster with data.table
## no, data.table isn't any faster
## new version uses rcpp and works on the whole table at once

searchTimeWindowEm <- as.difftime(3,units="hours") ## two hours before and after
searchDateWindowEm <- as.difftime(2,units="weeks") ## 2 weeks before and after



emergency_gen_patients <- function(emergency_table){
  ##expects emergency_spells as-is
  ##emergency_gen_table needs to contain 2KMD_date, time, and bizday
   emergency_table<-mutate(emergency_table,idx_datetime=as.POSIXct(time_at,origin="1970-01-01 00:00.00 UTC"))
   emergency_table<-mutate(emergency_table,bizday=is.bizday(as.character(idx_datetime),'Rmetrics/LONDON')) ##nb needs as.character as usual to avoid BST issues
   emergency_table<-mutate(emergency_table,y2kdate=as.Date(format(idx_datetime,"2000-%m-%d")))
   emergency_table<-mutate(emergency_table,idx_time=(as.numeric(idx_datetime) %% 86400))
  
   spell_ids<-gen_emergency_patients(emergency_table,emergency_spells,as.numeric(searchTimeWindowEm,units="secs"),as.numeric(searchDateWindowEm,units="days"))
  
   emergency_table$spell_id<-spell_ids
   return (emergency_table)
  
}


searchDateWindowEl <- as.difftime(2,units="weeks") ## 2 weeks before and after

elective_gen_patients <- function(elective_table){
  ##expects elective_spells as-is
  ##elective_gen_table needs to contain 2KMD_date and bizday (time not relevant as we're assuming 0700)
  elective_table<-mutate(elective_table,idx_datetime=as.POSIXct(time_at,origin="1970-01-01 00:00.00 UTC"))
  elective_table<-mutate(elective_table,bizday=is.bizday(as.character(idx_datetime),'Rmetrics/LONDON')) ##nb needs as.character as usual to avoid BST issues
  elective_table<-mutate(elective_table,y2kdate=as.Date(format(idx_datetime,"2000-%m-%d")))
  ##elective_table<-mutate(elective_table,idx_time=(as.numeric(idx_datetime) %% 86400))
  
  spell_ids<-gen_elective_patients(elective_table,elective_spells,as.numeric(searchDateWindowEl,units="days"))
  
  elective_table$spell_id<-spell_ids
  return (elective_table)
  
}




## helper function to pull data - actually only iterate is used
consume <- function(x) {
  i <- 0
  function() {
    i <<- i + 1
    #print(x[[i]])
    x[[i]]
  }
}

iterate  <- function() {
  i <- 0
  function() {
    i <<- i + 1
    if (i %% 1000 == 0) print(i)
    i
  }
}






simmer_wrapper <- function(i) {
  
  load_rmetrics_calendars(2000:2022) ##loaded again as the lack of Windows fork() means it's not copied
  sourceCpp("1RcppFunctions.cpp")
  emergency_table<- emergency_gen_table()
  
  elective_table<- elective_gen_table()
  print("*started*")
  print("* times generated *")
  
  emergency_table<- emergency_gen_patients(emergency_table)
  elective_table<- elective_gen_patients(elective_table)
  
  print(nrow(emergency_table))
  print(nrow(elective_table))
  
  
  ##not sure why spell_id needs treating differently below. Something to do with data types I think.
  ##I've explicitly checked this and it's correct and doesn't reuse the same spell_id over and over
  
  emergency_table$sp_row_id<-match(emergency_table$spell_id[[1]],emergency_spells$`_SpellID`)
  elective_table$sp_row_id<-match(elective_table$spell_id[[1]],elective_spells$`_SpellID`)
  
  elective_table$ep1_row_id<-elective_spells$ep1_row_id[elective_table$sp_row_id]
  emergency_table$ep1_row_id<-emergency_spells$ep1_row_id[emergency_table$sp_row_id]
  ##elective_table$ep1_row_id<-elective_spells[elective_table$sp_row_id,"ep1_row_id"]
  ##above has funny consequences for data type (like spell_id further above)
 
  print("* patients generated *")
  

  
  env<-simmer("hospital")
  

  
  ##nb patient must be in scope for the call to get_attribute
   

  
  common_patient<-trajectory() %>% 
    seize("bed") %>% 
    #    log_(function(){
    #     paste0(get_attribute(env,"end_time_ep")-get_attribute(env,"end_time_spell"))
    #   }) %>% 
    select(function() {traj_pr1[[get_attribute(env,"cur_traj")]]},"first-available") %>% 
    ## above - could concatenate pr1,pr2,pr3? Then afterwards raise flags depending on which we got?
    ## or add a "FAIL" resource to the end of each list with infinite capacity and check if we've selected that before adding more?
    seize_selected() %>% ##always successful as presently infinite
    #log_(function() {toString(min(traj_dur[[get_attribute(env,"cur_traj")]],get_attribute(env,"end_time")-now(env)))}) %>% 
    timeout(function() { min(traj_dur[[get_attribute(env,"cur_traj")]],get_attribute(env,"end_time_ep")-now(env))}) %>% 
    set_attribute(c("cur_traj","nxt_traj","end_time_ep","cur_ep_row_id"),function() {
      now<-now(env)
      end_time_ep<-get_attribute(env,"end_time_ep")
      end_time_spell<-get_attribute(env,"end_time_spell")
      start_time_spell<-get_attribute(env,"start_time")
      if (now>=end_time_ep) {
        if (now<end_time_spell) {
          ##next episode
          #cur_traj - send one from new episode (nb can ignore pretraj)
          #nxt_traj - send traj_nxt[[ new cur_traj ]]
          #end_time_ep - send _EpisodeEnd_Offset from new episode
          #cur_ep_row_id - send new row id (from epN_row_id)
          cur_ep_row_id<-get_attribute(env,"cur_ep_row_id")
          cur_ep_row_id<-as.numeric(combined_episodes[[cur_ep_row_id,"epN_row_id"]])
          if (!is.na(cur_ep_row_id)){
            cur_traj<-as.numeric(combined_episodes[[cur_ep_row_id,"traj"]])
            nxt_traj<-traj_nxt[[cur_traj]]
            end_time_ep<-as.numeric(combined_episodes[[cur_ep_row_id,"_EpisodeEnd_Offset"]])+start_time_spell
          } else {
            ##we've run out of episodes but haven't finished yet - this shouldn't really happen (but means the discharge datetime is after the end of the last episode)
            print("NA")
            cur_traj<-get_attribute(env,"cur_traj")
            nxt_traj<-traj_nxt[[cur_traj]]
            #print(paste("c",cur_traj,nxt_traj,get_attribute(env,"end_time_ep"),get_attribute(env,"end_time_spell"),cur_ep_row_id,sep=":"))
            end_time_ep<-end_time_spell
          }
          #print(paste("a",cur_traj,nxt_traj,end_time_ep,cur_ep_row_id,sep=":"))
          return(as.numeric(c(cur_traj,nxt_traj,end_time_ep,cur_ep_row_id)))
        } else {
          ## we're at our spell's end - no need for more data as we're about to finish
          return(c(0,0,0,0))
        }
        
      } else {
        #next trajectory, same episode
        #cur_traj - send nxt_traj
        #nxt_traj - send traj_nxt[[ new cur_traj ]]
        #end_time_ep - stay the same
        #cur_ep_row_id - stay the same
        cur_traj<-get_attribute(env,"nxt_traj")
        nxt_traj<-traj_nxt[[cur_traj]]
        end_time_ep<-get_attribute(env,"end_time_ep")
        cur_ep_row_id<-get_attribute(env,"cur_ep_row_id")
        #print(paste("b",cur_traj,nxt_traj,end_time_ep,cur_ep_row_id,sep=":"))
        return(as.numeric(c(cur_traj,nxt_traj,end_time_ep,cur_ep_row_id)))
      }
    }) %>% 
    release_selected() %>% ##with full model, don't do this if we already have a pr1 bed
    rollback(5,Inf,function() {now(env)<get_attribute(env,"end_time_spell")}) %>% 
    ##nb rollback includes log_ lines (so 6 if log line is included)
    release("bed")
  #log_("Out of bed")
  
  
  emergency_patient<- trajectory() %>%
    #log_("arrived") %>% 
    set_attribute("cur_ep_row_id", function() {get_attribute(env,"ep1_row_id")}) %>%
    set_attribute(c("cur_traj","nxt_traj"), function() {
      pretraj<-combined_episodes[get_attribute(env,"cur_ep_row_id"),"pretraj"]
      traj<-as.numeric(combined_episodes[get_attribute(env,"cur_ep_row_id"),"traj"])
      if (is.na(pretraj))
      {
        return (as.numeric(c(traj,traj_nxt[[traj]])))
      } else {
        return (as.numeric(c(pretraj,traj)))
      }
      
    }) %>% 
    set_attribute("end_time_ep", function() { as.numeric(combined_episodes[get_attribute(env,"cur_ep_row_id"),"_EpisodeEnd_Offset"])+now(env) }) %>%
    set_attribute("start_time", function() {now(env)}) %>% 
    set_attribute("end_time_spell", function() { as.numeric(emergency_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    join(common_patient)
    
  
  
  elective_patient<- trajectory() %>%
    #log_("arrived") %>% 
    set_attribute("cur_ep_row_id", function() {get_attribute(env,"ep1_row_id")}) %>%
    set_attribute(c("cur_traj","nxt_traj"), function() {
      pretraj<-combined_episodes[get_attribute(env,"cur_ep_row_id"),"pretraj"]
      traj<-as.numeric(combined_episodes[get_attribute(env,"cur_ep_row_id"),"traj"])
      if (is.na(pretraj))
      {
        #print(paste("c",traj,traj_nxt[[traj]],sep=":"))
        return (as.numeric(c(traj,traj_nxt[[traj]])))
      } else {
        return (as.numeric(c(pretraj,traj)))
      }

    }) %>% 
    set_attribute("end_time_ep", function() { as.numeric(combined_episodes[get_attribute(env,"cur_ep_row_id"),"_EpisodeEnd_Offset"])+now(env) }) %>%
    set_attribute("start_time", function() {now(env)}) %>% 
    set_attribute("end_time_spell", function() { as.numeric(elective_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
   # log_(function() {paste0(elective_spells[get_attribute(env,"sp_row_id"),"duration"]," ",combined_episodes[get_attribute(env,"cur_ep_row_id"),"_EpisodeEnd_Offset"])}) %>% 
    join(common_patient)
    

  env %>% 
    add_dataframe("Emergency Patient",emergency_patient,emergency_table,col_time="gaps",time="interarrival",col_attributes=c("sp_row_id","ep1_row_id")) %>% 
    add_dataframe("Elective Patient",elective_patient,elective_table,col_time="gaps",time="interarrival",col_attributes=c("sp_row_id","ep1_row_id")) %>% 
    
    add_resource("bed",capacity=Inf,queue_size=0,queue_size_strict=TRUE)
  
  for (ward in wards$Ward) {
    add_resource(env,ward,capacity=Inf,queue_size=0,queue_size_strict=TRUE)
    print(ward)
  }
  
     
    env %>% 
     run(until=1622312575	) %>% 
     wrap()
  
}

#library(parallel)
#library(pbmcapply)
##nb not parallel on windows because of lack of fork()
##parallelsugar doesn't work properly because environment isn't copied correctly
##mclapply.hack.R doesn't work for same reason

print("* Simulation started (no output) *")

#envs<-mclapply(1:4,simmer_wrapper),mc.cores=8)
#envs<-pbmclapply(1:1,simmer_wrapper,mc.cores=8)
##something very odd happening here with hospital capacity sat at around 200 and no obvious long tail on discharge
##is there a fixed duration occurring? (answer: yes) - because patient wasn't in scope so it was reusing the old env

#envs<-simmer_wrapper(1)

library("doParallel")
registerDoParallel(cores=8)

trials<-4
envs <- foreach(icount(trials), .combine=c, .noexport=c("gen_emergency_patients","gen_elective_patients"),.export=c("progress_bar"), .packages=c("dplyr","bizdays","Rcpp","simmer"), .verbose=TRUE) %dopar% {
 simmer_wrapper(1)
}

#nb if .combine=cbind then you get 17 identical replications per trial

print("* Simulation finished *")

library(simmer.plot)

resources<-get_mon_resources(envs)

resources2<-filter(resources,resource=="23")

resources2$time<-as.POSIXct(resources2$time,origin="1970-01-01 00:00.00 UTC")

#plot(resources2$time,resources2$server,col=resources2$replication,type="l",pch=".")

ggplot(resources2,aes(time,server,colour=replication)) + geom_line()

#library(ggplot2)
#ggplot(resources2,aes(x=time,y=server)) + geom_point(alpha=0.01) + stat_summary(fun.data=median_hilow, fun.args=list(conf.int=0.5),geom='smooth',se=TRUE,color='red',fill='red',alpha=0.2) 
