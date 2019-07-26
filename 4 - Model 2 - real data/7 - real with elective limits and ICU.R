## Second model using SUS data
## Models individual wards and whole hospital, all have correct capacity
## Inputs 
##  (1) Generated frequency of admissions (time of change, rate as patients/24hrs, for elective and emergency care)
##  (2) Dataframes of patients for elective and emergency care - using SUS data
##  Tick rate is in seconds
## 
## DONE - change fixed 10 day stay to pull a real patient and use their stay
## DONE - add wards and allow patients to follow a basic pathway (presently still infinite, and all patients are emergency surgical)
## DONE - add elective admissions
## DONE - add correct ward capacity
## DONE - add fallback to less-preferred wards
## DONE - allow patient to have a set "current" ward rather than dropping and immediately seizing again (nb prio shows how good we think the present ward is)
## DONE - move initial ward handling into emergency and elective start trajectories, so common only handles things thereafter
## DONE - handle elective patients properly (ie don't let them in when no ward is available)
## DONE - queueing for emergency patients? WONTFIX as we'd prefer to keep their options open rather than queuing for one ward
## DONE - add critical care
## DONE - investigate critical care length of stay (seems too short) - FIXED
## TODO - investigate shortest-queue-available policy as it seems to prefer smaller wards (emailed simmer-devel group)
## TODO - open and close ward beds and wards

##depends on SUSv2-byepisode elective and emergency in "2" directory

## Uses Rcpp as original approach is very slow because of the repeated filtering required to pull a real patient each time we want to admit one
##  v2 - generate all the admission intervals in advance (create a new table of interarrival times and absolute times)


## first run has a slow rise before sitting on around 700 occupied beds later - appears that elective patients are being rejected unnecessarily (and building up for later)
## adding queueing seems to have fixed that


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


## files generated in step 2. Need to ensure dates match up with the two generators!
emergency_freq <- readRDS("../Data - For Modelling/Emergency-Frequency.rds")
elective_freq <- readRDS("../Data - For Modelling/Elective-Frequency.rds")

emergency_spells <- readRDS("../Data - For Modelling/Emergency-Spells-Linked.rds")
elective_spells <- readRDS("../Data - For Modelling/Elective-Spells-Linked.rds")

combined_episodes <- readRDS("../Data - For Modelling/Both-Episodes-Linked.rds")

critcare_segments <- readRDS("../Data - For Modelling/CritCare-Segments-Linked.rds")


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
#emergency_freq<-head(emergency_freq,200)
#plotelective_freq<-head(elective_freq,20)





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
  
  elective_table$cc1_row_id<-elective_spells$cc1_row_id[elective_table$sp_row_id]
  emergency_table$cc1_row_id<-emergency_spells$cc1_row_id[emergency_table$sp_row_id]
  elective_table$cc_start<-elective_spells$cc_start[elective_table$sp_row_id]
  emergency_table$cc_start<-emergency_spells$cc_start[emergency_table$sp_row_id]
  
  
 
  print("* patients generated *")
  

  
  env<-simmer("hospital")
  
  
  
  ##nb patient must be in scope for the call to get_attribute
  
  catch_unfinished<-trajectory() %>% 
    log_("***UNFINISHED ARRIVAL - shouldn't see yet***") %>% 
    log_(function(){paste0("21:",get_queue_count(env,"21"),"/",get_queue_size(env,"21"))})
   # set_queue_size("21",0) %>% 
   # rollback(1)
  
  set_CC_attributes<-trajectory() %>% ## 3 items in this trajectory (for rollbacks)
    set_attribute("cur_ward_idx", function() {
      match(get_selected(env),wards$Ward)
    }) %>% 
    set_attribute("in_cc",1) %>% 
    set_attribute(c("nxt_cc_row_id","start_time_cc_seg","dcr_time_cc_seg","CCTransfer"), function() {
      cur_cc_row_id<-get_attribute(env,"nxt_cc_row_id")
      CCTransfer<-critcare_segments[cur_cc_row_id,"_CCTransfer"]
      nxt_cc_row_id<-critcare_segments[cur_cc_row_id,"segN_row_id"]
      start_time_cc_seg<-if (!is.na(nxt_cc_row_id)) critcare_segments[nxt_cc_row_id,"_SegmentStart_Offset"]+get_attribute(env,"start_time") else Inf #get_attribute(env,"end_time_spell")+1
      dcr_time_cc_seg<-as.numeric(critcare_segments[cur_cc_row_id,"_SegmentDischReady_Offset"]+get_attribute(env,"start_time"))
      return(c(nxt_cc_row_id,start_time_cc_seg,dcr_time_cc_seg,CCTransfer))
    })
  
  CC_admission_delayed<-trajectory() %>% 
    ##TODO - NOT PRESENTLY USED AS RENEGE_IN RELEASES ALL RESOURCES WHEN ACTIVATED
    #log_("admission delayed 12h") %>% 
    set_global("CC_Admissions_12H_Delayed",1,mod="+") %>% ## TODO - make a separate list for W21 and ICU
    set_queue_size_selected(-1,mod="+") %>% 
    #set_prioritization(c(1,-1,-1),mod="+") %>% ##increase priority
    rollback(6) # to select in CC_admission rejected
  
  CC_admission_rejected<-trajectory() %>% 
    ## try queuing?
    #log_("cc admission rejected") %>% 
    #set_prioritization(c(1,-1,-1)) %>% ##allow displacement of electives
    select(function() { if (critcare_segments[get_attribute(env,"nxt_cc_row_id"),"_RealCritCare"]) "ICU" else c("21","ICU") },"shortest-queue-available") %>% 
    set_queue_size_selected(1,mod="+") %>% 
    #renege_in(12*3600,out=CC_admission_delayed) %>%
    ##something really funny about renege appears to be releasing my ward bed (when it reneges)
    seize_selected() %>% 
    ## we succeeded
    #renege_abort() %>% 
    #set_prioritization(c(0,-1,-1)) %>% ##back to normal prio else we'll enter queues when we should be rejected
    set_queue_size_selected(-1,mod="+") 
  ##continue=TRUE so falls back into CC_admission
  
  CC_admission<-trajectory() %>% 
    #log_("cc admission") %>% 
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  },id=1) %>% 
    #log_(function(){paste0("Ward:",get_selected(env,id=1)," ",get_seized_selected(env,id=1))}) %>% 
    select(function() { if (critcare_segments[get_attribute(env,"nxt_cc_row_id"),"_RealCritCare"]) "ICU" else c("21","ICU") },"first-available") %>% 
    seize_selected(reject=CC_admission_rejected,continue=TRUE,willqueue=FALSE) %>%
    #log_(function(){paste0("Ward:",get_selected(env,id=1)," ",get_seized_selected(env,id=1))}) %>% 
    release_selected(id=1) %>% 
    join(set_CC_attributes) %>% 
    rollback(10) ##nb log ##nb rollback includes each individual item within the joined trajectory above
  ##rollback to timeout in common_patient
    
  CC_transfer_delayed<-trajectory() %>% 
    ##TODO - not used because of issues with renege_in at present
    set_global("CC_Admissions_12H_Delayed",1,mod="+") %>% ## TODO - make a separate list for W21 and ICU
    #set_prioritization(c(1,-1,-1),mod="+") %>% ##increase priority
    rollback(3) # to renege_in in CC_transfer_rejected
  
    
  CC_transfer_rejected<-trajectory() %>% 
   # set_prioritization(c(1,-1,-1)) %>% ##allow displacement of electives
    set_queue_size_selected(1,mod="+") %>% 
    #renege_in(12*3600,out=CC_transfer_delayed) %>%
    seize_selected() %>% 
    ## we succeeded
    #renege_abort() %>% 
    #set_prioritization(c(0,-1,-1)) %>% ##back to normal prio else we'll enter queues when we should be rejected
    set_queue_size_selected(-1,mod="+")
    
  
  CC_transfer_seize<-trajectory() %>% 
    seize_selected(reject=CC_transfer_rejected,continue=TRUE,willqueue=FALSE) %>%
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  },id=1) %>% 
    release_selected(id=1) 
  
  CC_transfer<-trajectory() %>% 
    ##we're in a CC bed already
    #log_("cc transfer") %>% 
    select(function() { if (critcare_segments[get_attribute(env,"nxt_cc_row_id"),"_RealCritCare"]) "ICU" else "21" },"first-available") %>% 
    branch(function() {
      if (get_seized_selected(env)>0) 0 else 1
    },continue=TRUE,CC_transfer_seize) %>% 
    ##if we're here, we're happy with current bed
    join(set_CC_attributes)

  
  CC_discharge_delay_reset<-trajectory() %>% 
    ## TODO - not presently used due to renege_in issues
    set_global("CC_Discharges_24H_Delayed",1,mod="+") %>% 
    set_queue_size_selected(-1,mod="+") %>%
    rollback(5) ##to select in CC_discharge_delay
  
  
  CC_discharge_delay<-trajectory() %>% 

    select(function() { traj_pr1[[get_attribute(env,"cur_traj")]] },"shortest-queue-available") %>% 
    #log_(function() {paste0("cc discharge delay:",get_selected(env))}) %>% 
    set_queue_size_selected(1,mod="+") %>% 
    #renege_in(24*3600,out=CC_discharge_delay_reset) %>%
    seize_selected() %>% 
    ## we succeeded
    #renege_abort() %>% 
    set_queue_size_selected(-1,mod="+") 
    ## rolls into end of CC_discharge_to_ward
    ##OLD: must use rollback here if continue=FALSE in CC_discharge_to_ward - presently it isn't
  
  
  CC_discharge_to_ward<-trajectory() %>% 
    ##discharge from CC to a pr1 bed
    select(function() {
      traj_pr1[[get_attribute(env,"cur_traj")]]
    },"first-available") %>%
    #log_(function() {paste0("cc discharge:",get_selected(env)," ",get_queue_count_selected(env),"/",get_queue_size_selected(env))}) %>% 
    seize_selected(reject=CC_discharge_delay,continue=TRUE,willqueue=FALSE) %>% 

       select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  },id=1) %>% 
    #log_(function() {paste0("cc discharged: From ",get_selected(env,id=1)," to ",get_selected(env))}) %>% 
    release_selected(id=1) %>% 
    set_attribute("cur_ward_idx", function() {   match(get_selected(env),wards$Ward)  }) %>% 
    set_attribute("in_cc",0) %>% 
    set_attribute("dcr_time_cc_seg",function() {get_attribute(env,"end_time_spell")+1})
    ##rolls back into end of CC_discharge
  
  CC_discharge<-trajectory() %>% 
    ##either to a ward, or they're being transferred from one CC area to another
    ##if CCTransfer == 1 then do a transfer (can we use CC_admission?)
    ##if CCTransfer == 0 then transfer to a pr1 ward and queue if needed
    branch(function(){
      get_attribute(env,"CCTransfer")+1
    },continue=TRUE,CC_discharge_to_ward,CC_transfer) %>% 
     ##and update dcr_time_cc_seg to end of spell
    rollback(4) ## to timeout in common_patient
  
  
  common_seize_new_rollback<-trajectory() %>%
    set_attribute("pr",1,mod="+") %>% 
    rollback(4) ## back to branch in common_patient
  
  
  
  
  common_seize_new_ward<-trajectory() %>% 
    ##shouldn't get here with pr>3 as the rollback goes back into common_patient
    select(function() {
      pr<-get_attribute(env,"pr")
      switch(pr,
             traj_pr1[[get_attribute(env,"cur_traj")]],
             traj_pr2[[get_attribute(env,"cur_traj")]],
             traj_pr3[[get_attribute(env,"cur_traj")]])
    },"first-available",id=1) %>%
    seize_selected(reject=common_seize_new_rollback,continue=FALSE,id=1,willqueue=FALSE) %>%
    #log_(function(){paste0("RELEASE:",get_attribute(env,"cur_ward_idx")," ",wards$Ward[get_attribute(env,"cur_ward_idx")])}) %>% 
    select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  }) %>% 
    release_selected() %>% 
    set_attribute("cur_ward_idx", function() {   match(get_selected(env,id=1),wards$Ward)  })
  ##falls back into common_patient just before rollback
  
  
  common_patient<-trajectory() %>% 
    ## when we arrive here, we have a bed already (from elective or emergency)
    seize("bed") %>% 
    #log_(function() {toString(min(traj_dur[[get_attribute(env,"cur_traj")]],get_attribute(env,"end_time")-now(env)))}) %>% 
    timeout(function() { 
      max(0,
       min(traj_dur[[get_attribute(env,"cur_traj")]],
        get_attribute(env,"end_time_ep")-now(env),
        get_attribute(env,"start_time_cc_seg")-now(env), #will have moved on since admission
        get_attribute(env,"dcr_time_cc_seg")-now(env)
      )
      )
    }) %>% ## important that CC dcr is never before start_time, or we'll get stuck
    ## nb if we've been stuck on ICU for a bit, the above could turn negative (though we will have zero delay in this case). possible solution see below
    ## nb could be here simply because we've hit trajectory changeover (traj_dur) but actually we're in CC (in_cc == 1) 
    set_attribute(c("cur_traj","nxt_traj","end_time_ep","cur_ep_row_id"),function() {
      now<-now(env)
      start_time_spell<-get_attribute(env,"start_time")
      end_time_ep<-get_attribute(env,"end_time_ep")
      end_time_spell<-get_attribute(env,"end_time_spell")
      ## TODO - should we put a while here so we always finish with end_time_ep > now(). nb that we need to allow trajectories to move on still
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
            ##print("NA")
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
        #end_time_ep<-get_attribute(env,"end_time_ep") - already set
        cur_ep_row_id<-get_attribute(env,"cur_ep_row_id")
        #print(paste("b",cur_traj,nxt_traj,end_time_ep,cur_ep_row_id,sep=":"))
        return(as.numeric(c(cur_traj,nxt_traj,end_time_ep,cur_ep_row_id)))
      }
    }) %>% 
    branch(function(){
      start_time_cc_seg<-get_attribute(env,"start_time_cc_seg")
      dcr_time_cc_seg<-get_attribute(env,"dcr_time_cc_seg")
      now<-now(env)
      in_cc<-get_attribute(env,"in_cc")
      ## put multiple branches in here
      if (now>start_time_cc_seg && in_cc==1) {
        print(paste0("late ",start_time_cc_seg,":",dcr_time_cc_seg,":",now)) ##something delayed us beyond the start
      }
        
      if (now>dcr_time_cc_seg && in_cc==0) print(paste0("what? ",get_attribute(env,"end_time_spell"),":",dcr_time_cc_seg,":",now)) ##delayed so far we missed the end?
      if (get_attribute(env,"cur_traj")==0) return(0) #probably died on ICU, could have gone straight home
      if (now>=start_time_cc_seg && in_cc==0) return(1) #admit CC from ward
      if (now>=dcr_time_cc_seg && in_cc==1) return(2) #discharge from CC (to ward or more CC)
      return(0)
    },continue=FALSE,CC_admission,CC_discharge) %>% ## join CC_admission or discharge. At the end will have to roll back to set_attribute or timout - depends whether we grab a ward at the end. Probably should grab a pr1 ward.
    set_attribute("pr",1) %>% 
    ## is our current bed good enough?
    branch(function() {
      cur_ward<-wards$Ward[get_attribute(env,"cur_ward_idx")]
      cur_traj<-get_attribute(env,"cur_traj")
      if (cur_traj==0) return(0) ##ie we're quitting so don't look for more
      if (get_attribute(env,"in_cc")==1) return(0) ##we're on critical care so no need for bed search
      pr<-get_attribute(env,"pr") ##nb we only look up to 3 as we already have a bed
      if (pr==4) return(0) ##ie just stay in same bed
      if (is.na(match(cur_ward,
                      switch(pr,
                             traj_pr1[[cur_traj]],
                             traj_pr2[[cur_traj]],
                             traj_pr3[[cur_traj]])            
      ))) 1 else 0
    },continue=TRUE,common_seize_new_ward) %>% 
    rollback(5,Inf,function() {now(env)<get_attribute(env,"end_time_spell")}) %>% ## to timeout
    ##nb rollback includes log_ lines (so 7 if log line is included) - need to roll back to set pr back to 1
    simmer::select(function() { wards$Ward[get_attribute(env,"cur_ward_idx")]  }) %>% 
    ##log_("leaving") %>% 
    release_selected() %>% 
    release("bed")
  #log_("Out of bed")
  
  
  

  
  emergency_delay_and_reset<-trajectory() %>% 
    #log_("no beds for emergency patient") %>% 
    timeout(3600) %>% 
    set_attribute("pr",1) %>% 
    rollback(5) #to select in emergency_patient
  
  
  
  emergency_patient_rejected<-trajectory() %>% 
    #log_("rejected") %>%
    branch(function() {
      if (get_attribute(env,"pr")<4) 0 else 1
    },continue=FALSE,emergency_delay_and_reset) %>% 
    set_attribute("pr",1,mod="+") %>% 
    rollback(4) #to select in emergency_patient
  
  
  emergency_CC_patient_delayed<-trajectory() %>% 
    #log_("no beds for emergency patient") %>% 
    set_queue_size_selected(-1,mod="+") %>% 
    set_global("CC_Admissions_12H_Delayed",1,mod="+") %>% ## TODO - make a separate list for W21 and ICU

    #set_prioritization(c(1,-1,-1),mod="+") %>% ##increase priority
    rollback(6) # to select in emergency_CC_patient_rejected
  
  
  
  emergency_CC_patient_rejected<-trajectory() %>% 
    ## try queuing?
    #log_("joining queue") %>% 
    #set_prioritization(c(1,-1,-1)) %>% ##allow displacement of electives
    select(function() { if (critcare_segments[get_attribute(env,"cc1_row_id"),"_RealCritCare"]) "ICU" else c("21","ICU") },"shortest-queue-available") %>% 
    #log_(function() {paste0("emergency_cc_rejected:",get_selected(env)," ",get_queue_count_selected(env),"/",get_queue_size_selected(env))}) %>% 
    set_queue_size_selected(1,mod="+") %>% 
    renege_in(12*3600,out=emergency_CC_patient_delayed) %>%
    seize_selected() %>% 
    ## we succeeded
    renege_abort() %>% 
    set_queue_size_selected(-1,mod="+") 
  #set_prioritization(c(0,-1,-1))  ##back to normal prio else we'll enter queues when we should be rejected
##rolls back into emergency_CC_patient
  
  
  emergency_CC_patient<- trajectory() %>%
    #log_("cc direct admission") %>% 
    select(function() { if (critcare_segments[get_attribute(env,"cc1_row_id"),"_RealCritCare"]) "ICU" else c("21","ICU") },"first-available") %>% 
    seize_selected(reject=emergency_CC_patient_rejected,continue=TRUE,willqueue=FALSE) %>%
    join(set_CC_attributes) %>% 
    #log_("admitted successfully") %>% 
    join(common_patient)
  
  
  
  set_patient_attributes_common<-trajectory() %>% 
    set_attribute("cur_ep_row_id", function() {get_attribute(env,"ep1_row_id")}) %>%
    set_attribute("nxt_cc_row_id", function() {get_attribute(env,"cc1_row_id")}) %>%
    set_attribute("in_cc",0) %>% 
    set_attribute("CCTransfer",0) %>% 
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
    set_attribute("start_time_cc_seg",function() { 
      if (get_attribute(env,"cc1_row_id")!=0)
        as.numeric(critcare_segments[get_attribute(env,"cc1_row_id"),"_SegmentStart_Offset"])+now(env)
      else
        Inf #get_attribute(env,"end_time_spell")+1
    }) %>% ##ie start of cc seg will be after end of spell if there's no cc seg
    set_attribute("dcr_time_cc_seg",function() { 
      if (get_attribute(env,"cc1_row_id")!=0)
        as.numeric(critcare_segments[get_attribute(env,"cc1_row_id"),"_SegmentDischReady_Offset"])+now(env)
      else
        Inf #get_attribute(env,"end_time_spell")+1
    }) %>% ##ie end of cc seg will be after end of spell if there's no cc seg
    set_attribute("start_time", function() {now(env)})
    
  
  
  
  emergency_patient<- trajectory() %>%
    #log_("arrived") %>% 
    handle_unfinished(catch_unfinished) %>% 
    set_attribute("end_time_spell", function() { as.numeric(emergency_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    join(set_patient_attributes_common) %>% 
    branch(function() {
      get_attribute(env,"cc_start")
    },continue=FALSE,emergency_CC_patient) %>% 
    set_attribute("pr",1) %>% 
    select(function() {
      pr<-get_attribute(env,"pr")
      switch(pr,
             traj_pr1[[get_attribute(env,"cur_traj")]],
             traj_pr2[[get_attribute(env,"cur_traj")]],
             traj_pr3[[get_attribute(env,"cur_traj")]],
             wards$Ward) ##changed to a string vector
    },"first-available") %>% 
    ## first-available = pick the first one with a free bed, or first one with a free queue spot, or first one with non-zero capacity (errors if all have zero capacity)
    seize_selected(reject=emergency_patient_rejected,continue=FALSE,willqueue=FALSE) %>%
    ## set attribute so we know what we selected
    set_attribute("cur_ward_idx", function() {
      match(get_selected(env),wards$Ward)
    }) %>% 
    #log_(function(){paste0("Ward:",get_selected(env)," ",get_seized_selected(env))}) %>% 
    join(common_patient)
  
  
  elective_patient_come_back_next_week<-trajectory() %>%   
    set_global("Elective_OTD_Cancellations",1,mod="+") %>% 
    set_queue_size_selected(-1,mod="+") %>% 
    timeout(3600*24*7-14*3600-1) %>% ## come back slightly earlier next time, remove 12hrs as that's how long we queued for
    rollback(8) #to select in elective_patient
  
  
  elective_patient_rejected<-trajectory() %>% 
    #log_("elective cancelled on the day") %>%
    ## try queuing?
    select(function() { traj_pr1[[get_attribute(env,"cur_traj")]] },"shortest-queue-available") %>% 
    #log_(function() {paste0("elective:",get_selected(env)," ",get_queue_count_selected(env),"/",get_queue_size_selected(env))}) %>% 
    renege_in(14*3600,out=elective_patient_come_back_next_week) %>% ##allow a decent length of time as a patient could use discharge lounge etc
    set_queue_size_selected(1,mod="+") %>% 
    seize_selected() %>% 
    set_queue_size_selected(-1,mod="+") %>% 
    ## we succeeded
    renege_abort()
  # %>% log_(function() {paste0("elective2:",get_selected(env)," ",get_queue_count_selected(env),"/",get_queue_size_selected(env))})
    
##rolls back into elective_patient
  
  elective_CC_patient_come_back_next_week<-trajectory() %>%   
    set_global("Elective_OTD_Cancellations",1,mod="+") %>% 
    set_queue_size_selected(-1,mod="+") %>% 
    timeout(3600*24*7-14*3600-1) %>% ## come back slightly earlier next time, remove 12hrs as that's how long we queued for
    rollback(8) #to select in elective_CC_patient
  
  
  elective_CC_patient_rejected<-trajectory() %>% 
    ## try queuing?
    log_("joining CC queue") %>% 
    select(function() { if (critcare_segments[get_attribute(env,"cc1_row_id"),"_RealCritCare"]) "ICU" else c("21","ICU") },"shortest-queue-available") %>% 
    set_queue_size_selected(1,mod="+") %>% 
    renege_in(14*3600,out=elective_CC_patient_come_back_next_week) %>% ##allow a decent length of time as a patient could use discharge lounge etc
    seize_selected() %>% 
    ## we succeeded
    set_queue_size_selected(-1,mod="+") %>% 
    renege_abort()

##rolls back into elective_CC_patient
  
  
  elective_CC_patient<- trajectory() %>%
    #log_("cc direct admission") %>% 
    select(function() { if (critcare_segments[get_attribute(env,"cc1_row_id"),"_RealCritCare"]) "ICU" else c("21","ICU") },"first-available") %>% 
    seize_selected(reject=elective_CC_patient_rejected,continue=TRUE,willqueue=FALSE) %>%
    join(set_CC_attributes) %>% 
    join(common_patient)
    


  
  elective_patient<- trajectory() %>%
    #log_("arrived") %>% 
    handle_unfinished(catch_unfinished) %>% 
    set_attribute("end_time_spell", function() { as.numeric(elective_spells[get_attribute(env,"sp_row_id"),"duration"])+now(env) }) %>% 
    join(set_patient_attributes_common) %>% 
    # log_(function() {paste0(elective_spells[get_attribute(env,"sp_row_id"),"duration"]," ",combined_episodes[get_attribute(env,"cur_ep_row_id"),"_EpisodeEnd_Offset"])}) %>% 
    branch(function() {
      get_attribute(env,"cc_start")
    },continue=FALSE,elective_CC_patient) %>% 
    set_attribute("pr",1) %>% 
    select(function() { traj_pr1[[get_attribute(env,"cur_traj")]] },"first-available") %>% 
    ## first-available = pick the first one with a free bed, or first one with a free queue spot, or first one with non-zero capacity (errors if all have zero capacity)
    seize_selected(reject=elective_patient_rejected,continue=TRUE,willqueue=FALSE) %>%
    ## set attribute so we know what we selected
    set_attribute("cur_ward_idx", function() {
      match(get_selected(env),wards$Ward)
    }) %>% 
    #log_(function(){paste0("Ward:",get_selected(env)," ",get_seized_selected(env))}) %>% 
    join(common_patient)
  
  
  env %>% 
    add_dataframe("Emergency Patient",emergency_patient,emergency_table,col_time="gaps",time="interarrival",col_attributes=c("sp_row_id","ep1_row_id","cc1_row_id","cc_start")) %>% 
    add_dataframe("Elective Patient",elective_patient,elective_table,col_time="gaps",time="interarrival",col_attributes=c("sp_row_id","ep1_row_id","cc1_row_id","cc_start")) %>% 
    
    add_resource("bed",capacity=Inf,queue_size=0,queue_size_strict=TRUE)

    
    for (i in 1:nrow(wards)) {
      add_resource(env,wards[i,"Ward"],capacity=wards[i,"Beds"],queue_size=0,queue_size_strict=TRUE)
      print(paste0(wards[i,"Ward"],":",wards[i,"Beds"]))
    }
    
  
  
  
 # print(plot(elective_patient))
     
    env %>% 
     run(until=1632312575	) %>% 
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

envs<-simmer_wrapper(1)

print("* Simulation finished *")

library(simmer.plot)

resources<-get_mon_resources(envs)
attribs<-get_mon_attributes(envs)

resources2<-filter(resources,resource=="ICU")

resources2$time<-as.POSIXct(resources2$time,origin="1970-01-01 00:00.00 UTC")

print(plot(resources2$time,resources2$server,col=resources2$replication,type="l",pch="."))

print(plot(resources,metric="usage",c("ICU","21"),steps=TRUE,items=c("server","queue")))


attribs$time<-as.POSIXct(attribs$time,origin="1970-01-01 00:00.00 UTC")

library(ggplot2)
ggplot(resources2,aes(x=time,y=server)) + geom_point(alpha=0.01) + stat_summary(fun.data=median_hilow, fun.args=list(conf.int=0.5),geom='smooth',se=TRUE,color='red',fill='red',alpha=0.2) 
