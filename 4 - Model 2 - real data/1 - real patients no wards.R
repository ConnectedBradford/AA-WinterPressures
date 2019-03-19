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
select<-simmer::select


emergency_freq <- readRDS("../Data - For Modelling/Emergency-Frequency.rds")
elective_freq <- readRDS("../Data - For Modelling/Elective-Frequency.rds")

emergency_spells <- readRDS("../Data - For Modelling/Emergency-Spells.rds")
elective_spells <- readRDS("../Data - For Modelling/Elective-Spells.rds")

print("* loaded *")


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
        #last_change <<- next_change
        next_change <<- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
        freq_table_index <<- freq_table_index+1
        if (freq_table_index==nrow(emergency_freq)) { return(-1) }
      }
      
      ##main generator here - rates in the files are per day, assume poisson process
      tmp_gap <- rexp(1,cur_freq/86400)+t_skip
      
      ## if the next admission would occur after a rate change, skip it. Take advantage of the fact Poisson processes have no memory to start a new one at the rate change (this has variable mathematical validity!)
      if ((tmp_gap + last_time)>next_change) {
        t_skip <<- next_change - last_time
        cur_freq <<- emergency_freq[freq_table_index,]$`_correctedN`
        #last_change <<- next_change
        next_change <<- as.numeric(emergency_freq[freq_table_index+1,]$dateTime)
        freq_table_index <<- freq_table_index+1
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
  timeout(86400*10) %>% 
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
