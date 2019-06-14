library(simmer)

set.seed(1014)



reneged <-
  trajectory("renege") %>% 
  log_("renege")

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  set_attribute("start_time", function() {now(bank)}) %>%
  select(c("counter1", "counter2"), policy = "shortest-queue") %>%
  renege_in(2,customer) %>% 
  seize_selected() %>%
  renege_abort() %>% 
  log_(function() {paste("Waited: ", now(bank) - get_attribute(bank, "start_time"))}) %>%
  timeout(function() {rexp(1, 1/12)}) %>%
  release_selected() %>%
  log_(function() {paste("Finished: ", now(bank))})


bank <-
  simmer("bank") %>%
  add_resource("counter1", 1) %>%
  add_resource("counter2", 1) %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10),0,0,0, -1)})

bank %>% run(until = 400)