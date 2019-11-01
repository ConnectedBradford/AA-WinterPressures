library(simmer)


env<-simmer()

    patientA <- trajectory("fill ward A") %>%
      seize("wardA") %>%
      timeout(40) %>% 
      release("wardA")
    
    patientB <- trajectory("fill ward B") %>%
      seize("wardB") %>%
      timeout(40) %>% 
      release("wardB")
    
    log_failure<-trajectory("log failure") %>% 
      log_("delay changing wards") %>% 
      rollback(2)
    
    patientC <- trajectory("which ward") %>% 
      seize("wardA") %>% 
      timeout(5) %>% 
        renege_in(10,out=log_failure) %>% ##,leaveresources=FALSE
      seize("wardB") %>% 
      renege_abort() %>% 
      release("wardA") %>% 
      timeout(40) %>% 
      release("wardB")
    
    
    env %>% 
    add_resource("wardA",20) %>% 
    add_resource("wardB",30) %>%
    add_generator("patientA", patientA, function() {c(rep(0.1,19),-1)}) %>%
    add_generator("patientB", patientB, function() {c(rep(0.1,31),-1)}) %>%
    add_generator("patientC",patientC, at(10)) %>% 
    run(until=80)
  
    
## results in: 'wardA' not previously seized