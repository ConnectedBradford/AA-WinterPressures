## First model using SUS data
## Does not model individual wards, only whole hospital, which is assumed to have infinite capacity
## Inputs 
##  (1) Generated frequency of admissions (time of change, rate as patients/24hrs, for elective and emergency care)
##  (2) Dataframes of patients for elective and emergency care - using SUS data
##  Tick rate is in seconds
## 
## TODO - change fixed 10 day stay to pull a real patient and use their stay
## TODO - add elective admissions

library(dplyr)
library(simmer)
library(simmer.plot)
library(progress)
select<-simmer::select
library(bizdays) ##this library is way quicker than timeDate (but we have to use timeDate's calendars for some reason)
load_rmetrics_calendars(2000:2022) ##nb we only get these holidays so may need extending in future



emergency_freq <- readRDS("../Data - For Modelling/Emergency-Frequency.rds")
elective_freq <- readRDS("../Data - For Modelling/Elective-Frequency.rds")

emergency_spells <- readRDS("../Data - For Modelling/Emergency-Spells.rds")
elective_spells <- readRDS("../Data - For Modelling/Elective-Spells.rds")

print("* loaded *")


##base a progress bar on the emergency admissions as they're the biggest and most detailed
pb <- progress_bar$new(format="[:bar] :percent eta: :eta",total=nrow(emergency_freq))


## function for emergency admission frequencies
## needs to return inter-arrival times
emergency_gen <- function() {
  last_time <- 0 ##last time we generated
  next_change <- as.numeric(emergency_freq[1,]$dateTime) ## next rate change
  cur_freq <- 0 ##current frequency (in "per day" units)
  t_skip <- 0 ##time skipped due to zero frequency, or because next admission was after a rate change
  freq_table_index <- 1 ##position in the frequency table
  function() {
    repeat {
      while (cur_freq==0) {
        ##if frequency is ever zero (true at the start), keep skipping until we hit a positive frequency
        ##t_skip represents how long we've skipped before the current gap
        t_skip <<- next_change - last_time
        cur_freq <<- emergency_freq[freq_table_index,]$`_correctedN`
        next_change <<- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
        freq_table_index <<- freq_table_index+1
        pb$tick()
        if (freq_table_index==nrow(emergency_freq)) { return(-1) }
      }
      
      ##main generator here - rates in the files are per day, assume poisson process
      tmp_gap <- rexp(1,cur_freq/86400)+t_skip
      
      ## if the next admission would occur after a rate change, skip it. Take advantage of the fact Poisson processes have no memory to start a new one at the rate change (this has variable mathematical validity!)
      if ((tmp_gap + last_time)>next_change) {
        t_skip <<- next_change - last_time
        cur_freq <<- emergency_freq[freq_table_index,]$`_correctedN`
        next_change <<- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
        freq_table_index <<- freq_table_index+1
        pb$tick()
        #print(freq_table_index)
        if (freq_table_index==nrow(emergency_freq)) { return(-1) }
      } else {
        t_skip<<-0
        last_time<<-last_time+tmp_gap
        return(tmp_gap)
      }
      
    }
  }
}


## function to produce patients
## will return a patient's entire row
## presently indexed by - business day, time +/- n hours, date +/- n weeks
## this version works but is quite slow due to the long and repeated lookups. Might be faster with data.table

searchTimeWindow <- as.difftime(3,units="hours") ## two hours before and after
searchDateWindow <- as.difftime(2,units="weeks") ## 2 weeks before and after

emergency_patient <- function(idx_datetime) {
  idx_datetime<-as.POSIXct(idx_datetime,origin="1970-01-01 00:00.00 UTC")
  bizday<-is.bizday(idx_datetime,'Rmetrics/LONDON')
  y2kdate<-as.Date(format(idx_datetime,"2000-%m-%d"))
  idx_time<-(as.numeric(idx_datetime) %% 86400)
  
  ## Time search window - see above
  startTsearch <- idx_time - as.numeric(searchTimeWindow,units="secs")
  endTsearch <- idx_time + as.numeric(searchTimeWindow,units="secs")
  
  ## Date search window - see above
  startDsearch <- y2kdate - searchDateWindow
  endDsearch <- y2kdate + searchDateWindow
  
  output=data.frame()
  ## need to wrap around date and time to build list of potential patients
  for (toffset in c(-86400,0,86400)) {
    for (doffset in c(as.difftime(-365,units="days"),0,as.difftime(365,units="days"))) {
      tempout<-filter(emergency_spells,(`_Start_Time`>(startTsearch+toffset))&(`_Start_Time`<(endTsearch+toffset))&(`_bizday`==bizday)&(`_2KMD_Date`<(endDsearch+doffset))&(`_2KMD_Date`>(startDsearch+doffset)))
      output<-bind_rows(output,tempout)
      
    }
  }
  patient<-sample_n(output,1)
  ##now pick one of these at random and return it
  ##nb some patients don't have a discharge datetime (may need correcting in base data production)
  return(patient)
}

emergency_patient_timeout <- function(idx_datetime){
  tmp<-emergency_patient(idx_datetime)
  timeout<-(tmp$`_Discharge_DateTime`)-(tmp$`_SpellStart_DateTime`)
  if (is.na(timeout)) {timeout<-5*86400}
  return(as.numeric(timeout,units="secs"))
}


## helper function to pull data - actually only iterate is used
consume <- function(x) {
  i <- 0
  function() {
    i <<- i + 1
    print(x[[i]])
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


patient<- trajectory() %>%
  seize("bed") %>% 
  timeout(function() {emergency_patient_timeout(now(env))}) %>% 
  release("bed")
## create resources

env <-
  simmer("hospital") %>% 
  # add_resource(wards,capacity=Inf,queue_size=0,queue_size_strict=TRUE) %>% 
  add_generator("Patient", patient, emergency_gen())

add_resource(env,"bed",capacity=Inf,queue_size=0,queue_size_strict=TRUE)


print("* created *")

env %>% run()


print("* run *")

print(plot(get_mon_resources(env),steps=TRUE))

#print(plot(get_mon_resources(env),"usage",c("5 BRI","ENT DCU BRI","19 Disch Lounge BRI","12 BRI","11 BRI","8 BRI", "23 BRI", "27 BRI","22 BRI"),items="system",steps=TRUE))
