library(simmer)

set.seed(1014)



sub1<-trajectory() %>% 
  log_("sub1A") %>% 
  timeout(1) %>% 
  log_("sub1B") %>% 
  timeout(1) %>% 
  log_("sub1C") %>% 
  timeout(1) %>% 
  log_("sub1D")


sub2<-trajectory() %>% 
  log_("sub2A") %>% 
  timeout(3)


sub3<-trajectory() %>% 
  log_("sub3A") %>% 
  timeout(1) %>% 
  log_("sub3B") %>% 
  timeout(1)

renege<-trajectory() %>% 
  log_("renege") %>% 
  rollback(3)

rejected<-trajectory() %>% 
  log_("rejected") %>% 
  rollback(3)

customer <-
  trajectory("Customer's path") %>%
  log_("A") %>% 
  timeout(1) %>% 
  seize("counter1",reject=rejected,continue=FALSE) %>% 
  log_("B") %>% 
  log_("C") %>% 
  branch(function() {sample(1:3,1)},TRUE,sub1,sub2,sub3) %>% 
  log_("D") %>% 
  rollback(3,times=1) %>% 
log_("E") %>% 
  release_all("counter1")


bank <-
  simmer("bank") %>%
  add_resource("counter1", 1) %>%
  add_resource("counter2", 1) %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10),0,0,0, -1)})

bank %>% run(until = 400)


##conclusion - rollback in a renege trajectory uses the "renegein" as point zero and ignores what the arrival has done since
## rollback in a seize reject trajectory works the same (seize is point zero)
## rollback after a branch (with continue) treats the branch as a single instruction
