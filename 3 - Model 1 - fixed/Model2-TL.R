## First model b - test only, no real data, very basic
## Fixed - assumes infinite capacity for each ward, so only suitable for replicating one (real) year and not for testing counterfactuals
## Inputs 
##  (1) List of wards/resources
##  (2) Dataframe of patients: Each patient has an at time, and a list of requirements. Initially just ward(resource) and duration
##    nb must be sorted by arrival time
##  Tick rate is in seconds
## 20190206 TL

library(dplyr)
library(simmer)
library(simmer.plot)
select<-simmer::select


## set up some fake data
## 1000 patients moving to 10 wards each


wards<-c("5 BRI","ICU BRI","PCU BRI","8 BRI","20 BRI","11 BRI","21 BRI","MAU BRI","18 BRI","12 BRI")

pat_req<-list()
pat_dur<-list()

for (i in 1:1000) {
  pat_reqn<-sample(wards,replace=TRUE,size=10)
  pat_req[[i]]<-pat_reqn
  pat_durn<-sample(7*24*60*60,replace=TRUE,size=10)+60*60
  pat_dur[[i]]<-pat_durn
}
pat_at<-runif(1000,min=0,max=365*24*60*60)

patients<-data.frame(pat_at)
patients$req<-pat_req
patients$dur<-pat_dur

patients <- dplyr::arrange(patients,pat_at)



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

print(plot(get_mon_resources(env),steps=TRUE))


