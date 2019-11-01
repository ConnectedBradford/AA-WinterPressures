library(parallel)






                 
                 
  simmer_wrapper<-function(i) {
    
    patient <- trajectory("patients' path") %>%
      ## add an intake activity 
      seize("nurse", 1) %>%
      timeout(function() rnorm(1, 15)) %>%
      release("nurse", 1) %>%
      ## add a consultation activity
      seize("doctor", 1) %>%
      timeout(function() rnorm(1, 20)) %>%
      release("doctor", 1) %>%
      ## add a planning activity
      seize("administration", 1) %>%
      timeout(function() rnorm(1, 5)) %>%
      release("administration", 1)
    
    
  simmer("SuperDuperSim") %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", patient, function() rnorm(1, 10, 2)) %>%
    simmer::run(80) %>%
    wrap()
  }
  
 #envs <- mclapply(1:100,simmer_wrapper)
  
  
  
  library("doParallel")
  registerDoParallel(cores=8)
  
  trials<-2
  envs <- foreach(icount(trials), .combine=c, .noexport=c(),.export=c(), .packages=c("simmer"), .verbose=TRUE) %do% {
    simmer_wrapper(1)
  }
  