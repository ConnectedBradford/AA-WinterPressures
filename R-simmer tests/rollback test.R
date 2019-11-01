library(simmer)

set.seed(1014)

customer2<-
  trajectory() %>% 
  log_("C2")

subJoin<- trajectory("Joined") %>% 
  log_("item1") %>% 
  log_("item2") %>% 
  log_("item3")

reneged <-
  trajectory("renege") %>% 
  log_("renege")

sub1<-trajectory() %>% 
  log_("sub1A") %>% 
  log_("sub1B") %>% 
  join(subJoin) %>% ##for rollbacks, join counts as however many tasks are in the joined trajectory, not just one
  rollback(6)

sub2<-trajectory() %>% 
  log_("sub2A") %>% 
  rollback(3)

sub3<-trajectory() %>% 
  log_("sub3A") %>% 
  log_("sub3B") %>% 
  leave(1)

customer <-
  trajectory("Customer's path") %>%
  log_("A") %>% 
  log_("B") %>% 
  log_("C") %>% 
  branch(function() {sample(1:3,1)},TRUE,sub1,sub2,sub3)


bank <-
  simmer("bank") %>%
  add_resource("counter1", 1) %>%
  add_resource("counter2", 1) %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10),0,0,0, -1)})

#bank %>% run(until = 400)

print(plot(customer),verbose=TRUE)