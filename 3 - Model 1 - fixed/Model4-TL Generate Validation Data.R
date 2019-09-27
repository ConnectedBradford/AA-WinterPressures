## First model c - using real data from EPR (not SUS yet), very basic
## Fixed - assumes infinite capacity for each ward, so only suitable for replicating one (real) year and not for testing counterfactuals
## Advantage of this is it's a real year for validating the proper model - this is the validation version ie only overnight stays etc
## Inputs 
##  (1) List of wards/resources
##  (2) Dataframe of patients: Each patient has an at time, and a list of requirements. Initially just ward(resource) and duration
##    nb must be sorted by arrival time
##  Tick rate is in seconds
## 20190208 TL

library(dplyr)
library(simmer)
library(simmer.plot)
select<-simmer::select


## set up some fake data
## 1000 patients moving to 10 wards each


# wards<-c("5 BRI","ICU BRI","PCU BRI","8 BRI","20 BRI","11 BRI","21 BRI","MAU BRI","18 BRI","12 BRI")
# 
# pat_req<-list()
# pat_dur<-list()
# 
# for (i in 1:1000) {
#   pat_reqn<-sample(wards,replace=TRUE,size=10)
#   pat_req[[i]]<-pat_reqn
#   pat_durn<-sample(7*24*60*60,replace=TRUE,size=10)+60*60
#   pat_dur[[i]]<-pat_durn
# }
# pat_at<-runif(1000,min=0,max=365*24*60*60)
# 
# patients<-data.frame(pat_at)
# patients$req<-pat_req
# patients$dur<-pat_dur
# 
# patients <- dplyr::arrange(patients,pat_at)

wards <- readRDS("../Data - Generated/validation_wardstays_wards.rds")
patients <- readRDS("../Data - Generated/validation_wardstays_nested.rds")

print("* loaded *")

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
  set_attribute("patient_id", iterate()) %>%
  set_attribute("segment", 1) %>%
  seize("bed") %>% 
  select(function() {patients[["data"]][[get_attribute(env,"patient_id")]][["req"]][get_attribute(env,"segment")]}) %>%
  seize_selected() %>% 
  timeout(function() {patients[["data"]][[get_attribute(env,"patient_id")]][["dur"]][get_attribute(env,"segment")]}) %>%
  release_selected() %>% 
  set_attribute("segment",1,"+") %>% 
  simmer::rollback(5,check = function() {!is.na(patients[["data"]][[get_attribute(env,"patient_id")]][["req"]][get_attribute(env,"segment")])}) %>% 
  release("bed")


## create resources

csvmon<-monitor_csv()

env <-
  simmer("hospital",mon=csvmon) %>% 
  # add_resource(wards,capacity=Inf,queue_size=0,queue_size_strict=TRUE) %>% 
  add_resource("bed",capacity=Inf) %>% 
  add_generator("Patient", patient, at(patients$at), mon=2)


for (ward in wards) {
  add_resource(env,ward,capacity=Inf,queue_size=0,queue_size_strict=TRUE)
}

print("* created *")

env %>% run()


print("* run *")

#print(plot(get_mon_resources(env),steps=TRUE))

#print(plot(get_mon_resources(env),"usage",c("5 BRI","ENT DCU BRI","19 Disch Lounge BRI","12 BRI","11 BRI","8 BRI", "23 BRI", "27 BRI","22 BRI"),items="system",steps=TRUE))

print(plot(get_mon_resources(env),"usage","bed",items="system",steps=TRUE))

#print(plot(get_mon_resources(env),"usage",items="system",steps=TRUE))

resources<-get_mon_resources(env)
arrivals<-get_mon_arrivals(env)
attributes<-get_mon_attributes(env)

print("Date range:")
print(as.POSIXct(min(resources$time),origin="1970-01-01 00:00.00 UTC"))
print(as.POSIXct(max(resources$time),origin="1970-01-01 00:00.00 UTC"))

saveRDS(resources,"../Model Outputs/validation-resources.rds")