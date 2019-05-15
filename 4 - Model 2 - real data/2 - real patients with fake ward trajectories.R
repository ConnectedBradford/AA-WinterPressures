## Second model using SUS data
## Models individual wards and whole hospital, all have infinite capacity
## Inputs 
##  (1) Generated frequency of admissions (time of change, rate as patients/24hrs, for elective and emergency care)
##  (2) Dataframes of patients for elective and emergency care - using SUS data
##  Tick rate is in seconds
## 
## DONE - change fixed 10 day stay to pull a real patient and use their stay
## DONE - add wards and allow patients to follow a basic pathway (presently still infinite, and all patients are emergency surgical)
## TODO - add elective admissions
## TODO - add ward capacity
## TODO - add fallback to less-preferred wards

##depends on SUSv2-byepisode elective and emergency in "2" directory

## Uses Rcpp as original approach is very slow because of the repeated filtering required to pull a real patient each time we want to admit one
##  v2 - generate all the admission intervals in advance (create a new table of interarrival times and absolute times)


library(dplyr)
library(simmer)
#library(simmer.plot)
library(progress)
library(data.table)
library(Rcpp)
select<-simmer::select
library(timeDate)
library(bizdays) ##this library is way quicker than timeDate (but we have to use timeDate's calendars for some reason)
load_rmetrics_calendars(2000:2022) ##nb we only get these holidays so may need extending in future



emergency_freq <- readRDS("../Data - For Modelling/Emergency-Frequency.rds")
elective_freq <- readRDS("../Data - For Modelling/Elective-Frequency.rds")

emergency_spells <- readRDS("../Data - For Modelling/Emergency-Spells.rds")
elective_spells <- readRDS("../Data - For Modelling/Elective-Spells.rds")



emergency_spells$duration<-as.numeric(emergency_spells$`_Discharge_DateTime`)-as.numeric(emergency_spells$`_SpellStart_DateTime`)

emergency_spells$duration[is.na(emergency_spells$duration)]<-4*86400
##fudge 4 day stay for anyone we don't know


## truncate for coding purposes
emergency_freq<-head(emergency_freq,20)



print("* loaded *")


##base a progress bar on the emergency admissions as they're the biggest and most detailed
pb <- progress_bar$new(format="[:bar] :percent eta: :eta",total=nrow(emergency_freq))
# 
# 
# ## function for emergency admission frequencies
# ## needs to return inter-arrival times
# ## no longer used - we will generate the table in advance
# emergency_gen <- function() {
#   last_time <- 0 ##last time we generated
#   next_change <- as.numeric(emergency_freq[1,]$dateTime) ## next rate change
#   cur_freq <- 0 ##current frequency (in "per day" units)
#   t_skip <- 0 ##time skipped due to zero frequency, or because next admission was after a rate change
#   freq_table_index <- 1 ##position in the frequency table
#   function() {
#     repeat {
#       while (cur_freq==0) {
#         ##if frequency is ever zero (true at the start), keep skipping until we hit a positive frequency
#         ##t_skip represents how long we've skipped before the current gap
#         t_skip <<- next_change - last_time
#         cur_freq <<- emergency_freq[freq_table_index,]$`_correctedN`
#         next_change <<- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
#         freq_table_index <<- freq_table_index+1
#         pb$tick()
#         if (freq_table_index==nrow(emergency_freq)) { return(-1) }
#       }
#       
#       ##main generator here - rates in the files are per day, assume poisson process
#       tmp_gap <- rexp(1,cur_freq/86400)+t_skip
#       
#       ## if the next admission would occur after a rate change, skip it. Take advantage of the fact Poisson processes have no memory to start a new one at the rate change (this has variable mathematical validity!)
#       if ((tmp_gap + last_time)>next_change) {
#         t_skip <<- next_change - last_time
#         cur_freq <<- emergency_freq[freq_table_index,]$`_correctedN`
#         next_change <<- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
#         freq_table_index <<- freq_table_index+1
#         pb$tick()
#         #print(freq_table_index)
#         if (freq_table_index==nrow(emergency_freq)) { return(-1) }
#       } else {
#         t_skip<<-0
#         last_time<<-last_time+tmp_gap
#         return(tmp_gap)
#       }
#       
#     }
#   }
# }

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


## function to produce a patient from an index time
## will return a patient's entire row
## presently indexed by - business day, time +/- n hours, date +/- n weeks
## this version works but is quite slow due to the long and repeated lookups. Might be faster with data.table
## no, data.table isn't any faster
## new version uses rcpp and works on the whole table at once

searchTimeWindow <- as.difftime(3,units="hours") ## two hours before and after
searchDateWindow <- as.difftime(2,units="weeks") ## 2 weeks before and after
# emergency_spells_DT<-data.table(emergency_spells)
# 
# emergency_patient_v1 <- function(idx_datetime) {
#   idx_datetime<-as.POSIXct(idx_datetime,origin="1970-01-01 00:00.00 UTC")
#   bizday<-is.bizday(idx_datetime,'Rmetrics/LONDON')
#   y2kdate<-as.Date(format(idx_datetime,"2000-%m-%d"))
#   idx_time<-(as.numeric(idx_datetime) %% 86400)
#   
#   ## Time search window - see above
#   startTsearch <- idx_time - as.numeric(searchTimeWindow,units="secs")
#   endTsearch <- idx_time + as.numeric(searchTimeWindow,units="secs")
#   
#   ## Date search window - see above
#   startDsearch <- y2kdate - searchDateWindow
#   endDsearch <- y2kdate + searchDateWindow
#   
#   output=data.frame()
#   ## need to wrap around date and time to build list of potential patients
#   ## a brief timing test of the code suggests most of the time is spent doing the filtering. bind_rows and sample_n aren't that bad.
#   for (toffset in c(-86400,0,86400)) {
#     for (doffset in c(as.difftime(-365,units="days"),0,as.difftime(365,units="days"))) {
#       tempout<-filter(emergency_spells,(`_Start_Time`>(startTsearch+toffset))&(`_Start_Time`<(endTsearch+toffset))&(`_bizday`==bizday)&(`_2KMD_Date`<(endDsearch+doffset))&(`_2KMD_Date`>(startDsearch+doffset)))
#       #tempout<-emergency_spells_DT[between(`_Start_Time`,startTsearch+toffset,endTsearch+toffset) & between(`_2KMD_Date`,startDsearch+doffset,endDsearch+doffset) & `_bizday`==bizday]
#       ##both very slow
#             output<-bind_rows(output,tempout)
#       
#     }
#   }
#   patient<-sample_n(output,1)
#   ##now pick one of these at random and return it
#   ##nb some patients don't have a discharge datetime (may need correcting in base data production)
#   return(patient)
# }

sourceCpp("1RcppFunctions.cpp")

# timesdf<-emergency_gen_table()
# timesdf<-mutate(timesdf,idx_datetime=as.POSIXct(time_at,origin="1970-01-01 00:00.00 UTC"))
# timesdf<-mutate(timesdf,bizday=is.bizday(as.character(idx_datetime),'Rmetrics/LONDON')) ##nb needs as.character as usual to avoid BST issues
# timesdf<-mutate(timesdf,y2kdate=as.Date(format(idx_datetime,"2000-%m-%d")))
# timesdf<-mutate(timesdf,idx_time=(as.numeric(idx_datetime) %% 86400))


#test<-gen_emergency_patients(timesdf,emergency_spells,as.numeric(searchTimeWindow,units="secs"),as.numeric(searchDateWindow,units="days"))


emergency_gen_patients <- function(emergency_table){
  ##expects emergency_spells as-is
  ##emergency_gen_table needs to contain 2KMD_date, time, and bizday
   emergency_table<-mutate(emergency_table,idx_datetime=as.POSIXct(time_at,origin="1970-01-01 00:00.00 UTC"))
   emergency_table<-mutate(emergency_table,bizday=is.bizday(as.character(idx_datetime),'Rmetrics/LONDON')) ##nb needs as.character as usual to avoid BST issues
   emergency_table<-mutate(emergency_table,y2kdate=as.Date(format(idx_datetime,"2000-%m-%d")))
   emergency_table<-mutate(emergency_table,idx_time=(as.numeric(idx_datetime) %% 86400))
  
   spell_ids<-gen_emergency_patients(emergency_table,emergency_spells,as.numeric(searchTimeWindow,units="secs"),as.numeric(searchDateWindow,units="days"))
  
   emergency_table$spell_id<-spell_ids
   return (emergency_table)
  
}



# emergency_patient_timeout <- function(idx_datetime){
#   #tmp<-emergency_patient(idx_datetime)
#   #timeout<-(tmp$`_Discharge_DateTime`)-(tmp$`_SpellStart_DateTime`)
#   #if (is.na(timeout)) {timeout<-5*86400}
#   return(86400)
#   #return(as.numeric(timeout,units="secs"))
# }


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



## patient trajectory

## notes - select() can take a function but seize() can't
# 
# 
# emergency_table<- emergency_gen_table()
# 
# print("* times generated *")
# 
# emergency_table<- emergency_gen_patients(emergency_table)
# 
# print("* patients generated *")
# 
# ##not sure why spell_id needs treating differently below. Something to do with data types I think.
# emergency_table$sp_row_id<-match(emergency_table$spell_id[[1]],emergency_spells$`_SpellID`)
# 

## Single run - now removed as we're doing multiple
# 
# 
# patient<- trajectory() %>%
# #  set_attribute("gen_row_id",iterate()) %>% 
# #  set_attribute("sp_row_id",function() {as.numeric(emergency_table[get_attribute(env,"gen_row_id"),"sp_row_id"]) }) %>% 
#   seize("bed") %>% 
#   timeout(function() { as.numeric(emergency_spells[get_attribute(env,"sp_row_id"),"duration"]) }) %>% 
#   release("bed")
# ## create resources
# 
# env <-
#   simmer("hospital") %>% 
#   # add_resource(wards,capacity=Inf,queue_size=0,queue_size_strict=TRUE) %>% 
#   # add_generator("Patient", patient, emergency_table$gaps)
#   add_dataframe("Patient",patient,emergency_table,col_time="gaps",time="interarrival",col_attributes="sp_row_id") %>% 
#   add_resource("bed",capacity=Inf,queue_size=0,queue_size_strict=TRUE)
# 
# 
# print("* created *")
# 
# env %>% run()
# 
# 
# print("* run *")
# 
# test<-get_mon_resources(env)
# test$time<-as.POSIXct(test$time,origin="1970-01-01 00:00.00 UTC")
# 
# 
# print(plot(get_mon_resources(env),steps=TRUE))
# 
# print(plot(test$time,test$server,type="l"))

#print(plot(get_mon_resources(env),"usage",c("5 BRI","ENT DCU BRI","19 Disch Lounge BRI","12 BRI","11 BRI","8 BRI", "23 BRI", "27 BRI","22 BRI"),items="system",steps=TRUE))


wards<-c("5 BRI","ICU BRI","PCU BRI","8 BRI","20 BRI","11 BRI","21 BRI","MAU BRI","18 BRI","12 BRI","26 BRI","27 BRI","28 BRI")

## Data for various behaviours
## pr1 - best wards
## pr2 - OK wards
## pr3 - non-ideal wards
## dur - duration before moving to next stage (0=forever)
## nxt - next stage code number (active at end of duration or possibly if kicked out)

traj_pr1<-list()
traj_pr2<-list()
traj_pr3<-list()
traj_dur<-list()
traj_nxt<-list()

## 1 - emergency surgical admission
x<-1
traj_pr1[[x]]<-c("20 BRI")
traj_pr2[[x]]<-c("21 BRI","8 BRI","11 BRI")
traj_pr3[[x]]<-c("27 BRI","28 BRI","26 BRI")
traj_dur[[x]]<-24*3600
traj_nxt[[x]]<-2

## 2 - surgical patient
x<-2
traj_pr1[[x]]<-c("8 BRI","11 BRI","21 BRI")
traj_pr3[[x]]<-c("27 BRI","28 BRI","26 BRI")
traj_pr2[[x]]<-c("23 BRI","12 BRI","18 BRI")
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x


simmer_wrapper <- function(i) {
  
  
  emergency_table<- emergency_gen_table()
  
  print("* times generated *")
  
  emergency_table<- emergency_gen_patients(emergency_table)
  
  print(nrow(emergency_table))
  
  print("* patients generated *")
  
  ##not sure why spell_id needs treating differently below. Something to do with data types I think.
  emergency_table$sp_row_id<-match(emergency_table$spell_id[[1]],emergency_spells$`_SpellID`)
  
  env<-simmer("hospital")
  

  
  ##nb patient must be in scope for the call to get_attribute
  patient<- trajectory() %>%
    set_attribute("cur_traj",1) %>% 
    set_attribute("start_time", function() {now(env)}) %>% 
    set_attribute("end_time", function() { as.numeric(emergency_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    seize("bed") %>% 
    log_("In bed") %>% 
    # timeout(function() { as.numeric(emergency_spells[get_attribute(env,"sp_row_id"),"duration"]) }) %>% 
    select(function() {traj_pr1[[get_attribute(env,"cur_traj")]]},"first-available") %>% 
    ## above - could concatenate pr1,pr2,pr3? Then afterwards raise flags depending on which we got?
    ## or add a "FAIL" resource to the end of each list with infinite capacity and check if we've selected that before adding more?
    seize_selected() %>% 
    log_(function() {toString(min(traj_dur[[get_attribute(env,"cur_traj")]],get_attribute(env,"end_time")-now(env)))}) %>% 
    timeout(function() {min(traj_dur[[get_attribute(env,"cur_traj")]],get_attribute(env,"end_time")-now(env))}) %>% 
    set_attribute("cur_traj",function() {traj_nxt[[get_attribute(env,"cur_traj")]]}) %>% 
    release_selected() %>% 
    rollback(6,Inf,function() {now(env)<get_attribute(env,"end_time")}) %>% 
    release("bed") %>% 
  log_("Out of bed")
  
   env %>% 
    # add_resource(wards,capacity=Inf,queue_size=0,queue_size_strict=TRUE) %>% 
    # add_generator("Patient", patient, emergency_table$gaps)
    add_dataframe("Patient",patient,emergency_table,col_time="gaps",time="interarrival",col_attributes="sp_row_id") %>% 
    add_resource("bed",capacity=Inf,queue_size=0,queue_size_strict=TRUE)
   
   for (ward in wards) {
     add_resource(env,ward,capacity=Inf,queue_size=0,queue_size_strict=TRUE)
   }
     
     
    env %>% 
     run(until=1622312575	) %>% 
      wrap()
  
}

library(parallel)
library(pbmcapply)
##nb not parallel on windows because of lack of fork()
##parallelsugar doesn't work properly because environment isn't copied correctly


print("* Simulation started (no output) *")

#envs<-pbmclapply(1:10,simmer_wrapper,mc.cores=8)
#envs<-pbmclapply(1:1,simmer_wrapper,mc.cores=8)
##something very odd happening here with hospital capacity sat at around 200 and no obvious long tail on discharge
##is there a fixed duration occurring? (answer: yes) - because patient wasn't in scope so it was reusing the old env

envs<-simmer_wrapper()

print("* Simulation finished *")

resources<-get_mon_resources(envs)

resources2<-resources

resources2$time<-as.POSIXct(resources2$time,origin="1970-01-01 00:00.00 UTC")

plot(resources2$time,resources2$server,col=resources2$replication,pch=".")


library(ggplot2)
ggplot(resources2,aes(x=time,y=server)) + geom_point(alpha=0.01) + stat_summary(fun.data=median_hilow, fun.args=list(conf.int=0.5),geom='smooth',se=TRUE,color='red',fill='red',alpha=0.2) 
