library(simmer)


env<-simmer()

    patientA <- trajectory("fill ward A") %>%
      ## add an intake activity 
      seize("wardA") %>%
      timeout(40) %>% 
      release("wardA")
    
    patientB <- trajectory("fill ward B") %>%
      ## add an intake activity 
      seize("wardB") %>%
      timeout(40) %>% 
      release("wardB")
    
    patientC <- trajectory("which ward") %>% 
      select(c("wardA","wardB"),policy="shortest-queue-available") %>% 
      log_(function() {get_selected(env)}) %>% 
      seize_selected() %>% 
      timeout(40) %>% 
      release_selected
    
    
    env %>% 
    add_resource("wardA",20) %>% 
    add_resource("wardB",30) %>%
    add_generator("patientA", patientA, function() {c(rep(0.1,24),-1)}) %>%
    add_generator("patientB", patientB, function() {c(rep(0.1,31),-1)}) %>%
    add_generator("patientC",patientC, at(10)) %>% 
    run(80)
  
    
## With 24 patients in ward A, and 31 in B - queue size should be 4 for A and 1 for B
## expected behaviour: chooses ward B as that has the shortest queue
## actual behaviour: policy chooses ward A, until queue size for A reaches 12 (ie 32 patients from the A generator)