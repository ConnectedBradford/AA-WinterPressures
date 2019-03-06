## First model - test only, no real data, very basic
## Fixed - assumes infinite capacity for each ward, so only suitable for replicating one (real) year and not for testing counterfactuals
## Inputs 
##  (1) List of wards/resources
##  (2) Dataframe of patients: Each patient has an at time, and a list of requirements. Initially just ward(resource) and duration
##    nb must be sorted by arrival time
##  Tick rate is in seconds
## 20190206 TL


library(simmer)
library(simmer.plot)

## set up some fake data
pat_at<-c(21000,21100,21200)
pat1_req<-c("5 BRI","ICU BRI","PCU BRI","8 BRI")
pat1_dur<-c(2*60*60,3*24*60*60,4*24*60*60,10*24*60*60)
pat2_req<-c("5 BRI","8 BRI")
pat2_dur<-c(3*60*60,5*24*60*60)
pat3_req<-c("20 BRI","PCU BRI","8 BRI")
pat3_dur<-c(24*60*60,5*24*60*60,10*24*60*60)

pat_req<-list(pat1_req,pat2_req,pat3_req)
pat_dur<-list(pat1_dur,pat2_dur,pat3_dur)

wards<-c("5 BRI","ICU BRI","PCU BRI","8 BRI","20 BRI")

patients<-data.frame(pat_at)
patients$req<-pat_req
patients$dur<-pat_dur


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
    print(i)
    i
  }
}



## patient trajectory

## notes - select() can take a function but seize() can't


patient<- trajectory() %>%
  set_attribute("patient_id", iterate()) %>%
  set_attribute("segment", 1) %>%
  select(function() {patients$req[[get_attribute(env,"patient_id")]][get_attribute(env,"segment")]}) %>%
  seize_selected() %>% 
  timeout(function() {patients$dur[[get_attribute(env,"patient_id")]][get_attribute(env,"segment")]}) %>%
  release_selected() %>% 
  set_attribute("segment",1,"+") %>% 
  rollback(5,check = function() {!is.na(patients$req[[get_attribute(env,"patient_id")]][get_attribute(env,"segment")])})


## create resources

env <-
  simmer("hospital") %>% 
  # add_resource(wards,capacity=Inf,queue_size=0,queue_size_strict=TRUE) %>% 
  add_generator("Patient", patient, at(patients$pat_at))


for (ward in wards) {
  add_resource(env,ward,capacity=Inf,queue_size=0,queue_size_strict=TRUE)
}


env %>% run()

plot(get_mon_resources(env,steps=TRUE))


