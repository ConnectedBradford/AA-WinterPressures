library(simmer)

set.seed(12346)

env<-simmer()

unfinished <- trajectory() %>% 
  log_("**** UNFINISHED - SHOULD NEVER BE REACHED ****")

queuetryagain <- trajectory() %>% 
  set_queue_size("wardA",1,mod="+") %>% 
  seize("wardA") %>% 
  set_queue_size("wardA",-1,mod="+")


queuetryagainB <- trajectory() %>% 
  set_queue_size("wardA",1,mod="+") %>% 
  seize("wardA") %>% 
  set_queue_size("wardA",-1,mod="+")

patient <- trajectory() %>%
  handle_unfinished(unfinished) %>% 
  seize("wardA", reject=queuetryagain,continue=TRUE) %>%
  timeout(2) %>% 
  release("wardA")

patientB <- trajectory() %>% 
  handle_unfinished(unfinished) %>% 
  timeout(0.1) %>% 
  seize("wardA",reject=queuetryagainB,continue=TRUE) %>% 
  timeout(2) %>% 
  release("wardA")

env %>% 
  add_resource("wardA",20,queue_size=0,queue_size_strict=TRUE) %>% 
  add_generator("patientA", patient, function() {c(0,rexp(200000),-1)}) %>%
  add_generator("patientB", patientB, function() {c(0,rexp(200000),-1)}) %>%
  add_generator("patientC", patient, function() {c(0,rexp(200000),-1)}) %>%
  add_generator("patientD", patientB, function() {c(0,rep(0.9,200000),-1)}) %>%
  add_generator("patientE", patient, function() {c(0,rep(1.1,200000),-1)}) %>%
  add_generator("patientF", patientB, function() {c(0,rep(1.0,200000),-1)}) %>%
  run()

