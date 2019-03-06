
library(simmer)
library(dplyr)

actualData <- data.frame(arrTime = c(1:10,1:5), 
                         priority = 1:3, 
                         duration = rnorm(15, 50, 5)) %>%
  dplyr::arrange(arrTime)


consume <- function(x, prio=FALSE) {
  i <- 0
  function() {
    i <<- i + 1
    #print(i) 
    if (prio) c(x[[i]], x[[i]], FALSE)
    else x[[i]]
  }
}

activityTraj <- trajectory() %>%
  set_prioritization(consume(actualData$priority, TRUE)) %>%
  set_attribute("duration", consume(actualData$duration)) %>%
  seize('worker') %>%
  timeout_from_attribute("duration") %>%
  release('worker')

env<-simmer()
workerCount <- 2


env %>%
  add_resource('worker', workerCount, Inf, preemptive = TRUE) %>%
  add_generator('worker_', activityTraj, at(actualData$arrTime)) %>%
  run()


activity_time <- get_mon_arrivals(env) %>%
  tidyr::separate(name, c("prefix", "n"), convert=TRUE) %>%
  dplyr::arrange(n) %>%
  dplyr::pull(activity_time)
all(activity_time == actualData$duration)