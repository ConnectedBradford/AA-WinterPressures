library(simmer)

set.seed(1014)


reneged <-
  trajectory("renege") %>% 
  log_("renege") %>% 
  rollback(3)

unfinished<-
  trajectory() %>% 
  ##nb arrives here with counter1 still seized
  log_("unfinished") %>% 
  release_all() %>%  ## works to release everything
  timeout(5) %>% 
  rollback(4)
 ##nb the substitution doesn't work, but rollback can work
##ollback includes this, and goes back to the handle_unfinished statement



customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  handle_unfinished(unfinished) %>% 
  log_("rollback to here?") %>% 

  seize("counter1") %>% 
  seize("counter2") %>% 
  timeout(40) %>% 
  release("counter2") %>% 
  release("counter1")

unfinished[1]<-trajectory() %>% log_("modified unfinished") ## does nothing


bank <-
  simmer("bank") %>%
  add_resource("counter2", 1, queue_size_strict=TRUE, queue_size=0) %>%
  add_resource("counter1", 10) %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10),0,0,0, -1)})

bank %>% simmer::run(until = 400)

print(plot(customer))