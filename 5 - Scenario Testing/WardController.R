
pressure_days<-0

ward_control_open_17Esc<-trajectory() %>% 
  #log_("Opening 17Esc") %>% 
  set_global("17Esc_Open",1) %>% 
  set_capacity("17Esc",24) %>% 
  set_global("Beds_Open",24,mod="+")

ward_control_close_17Esc<-trajectory() %>% 
  # log_("Closing 17Esc") %>% 
  set_global("17Esc_Open",0) %>% 
  set_capacity("17Esc",0) %>% 
  set_global("Beds_Open",-24,mod="+")  

ward_control_open_12Esc<-trajectory() %>% 
  #  log_("Opening 12Esc") %>% 
  set_global("12Esc_Open",1) %>% 
  set_capacity("12Esc",3) %>% 
  set_global("Beds_Open",3,mod="+")

ward_control_close_12Esc<-trajectory() %>% 
  # log_("Closing 12Esc") %>% 
  set_global("12Esc_Open",0) %>% 
  set_capacity("12Esc",0)  %>% 
  set_global("Beds_Open",-3,mod="+") 

ward_control_open_AMU4Esc<-trajectory() %>% 
  # log_("Opening AMU4Esc") %>% 
  set_global("AMU4Esc_Open",1) %>%
  set_capacity("AMU4Esc",3) %>% 
  set_global("Beds_Open",3,mod="+")

ward_control_close_AMU4Esc<-trajectory() %>% 
  # log_("Closing AMU4Esc") %>% 
  set_global("AMU4Esc_Open",0) %>%
  set_capacity("AMU4Esc",0)  %>% 
  set_global("Beds_Open",-3,mod="+")

ward_control_open_22Esc<-trajectory() %>% 
  # log_("Opening 22Esc") %>% 
  set_global("22Esc_Open",1) %>%
  set_capacity("22Esc",6) %>% 
  set_global("Beds_Open",6,mod="+")

ward_control_close_22Esc<-trajectory() %>% 
  # log_("Closing 22Esc") %>% 
  set_global("22Esc_Open",0) %>%
  set_capacity("22Esc",0) %>% 
  set_global("Beds_Open",-6,mod="+")

ward_control_open_18Esc<-trajectory() %>% 
  # log_("Opening 18Esc") %>% 
  set_global("18Esc_Open",1) %>%
  set_capacity("18Esc",2) %>% 
  set_global("Beds_Open",2,mod="+")

ward_control_close_18Esc<-trajectory() %>% 
  # log_("Closing 18Esc") %>% 
  set_global("18Esc_Open",0) %>%
  set_capacity("18Esc",0) %>% 
  set_global("Beds_Open",-2,mod="+")

ward_control<-trajectory() %>% 
  set_capacity("17Esc",0) %>% 
  #set_capacity("CDU",0) %>% 
  set_capacity("AMU4Esc",0) %>% 
  set_capacity("18Esc",0) %>% 
  set_capacity("12Esc",0) %>% 
  set_capacity("22Esc",0) %>% 
  timeout(24*3600) %>% 
  branch(function() {
    beds<-get_server_count(env,"bed")
    open17<-(get_capacity(env,"17Esc")>0)
    if (beds>(590+runif(1,0,20)) && !open17) {
      pressure_days<<-pressure_days+1
      print(pressure_days)
      if (pressure_days>6) {
        return(1)
      } else {
        return(0)
      }
    }
    pressure_days<<-0
    if (beds<590 && open17) return(2)
    return(0)
  },ward_control_open_17Esc,ward_control_close_17Esc,continue=TRUE) %>% 
  branch(function() {
    beds<-get_server_count(env,"bed")
    open12<-(get_capacity(env,"12Esc")>0)
    if (beds>(600+runif(1,0,20)) && !open12 && pressure_days>4) return(1)
    if (beds<600 && open12) return(2)
    return(0)
  },ward_control_open_12Esc,ward_control_close_12Esc,continue=TRUE) %>% 
  branch(function() {
    amu4q<-get_queue_count(env,"AMU4")
    openAMU4<-(get_capacity(env,"AMU4Esc")>0)
    if (amu4q>2 && !openAMU4) return(1)
    if (amu4q<2 && openAMU4) return(2)
    return(0)
  },ward_control_open_AMU4Esc,ward_control_close_AMU4Esc,continue=TRUE) %>% 
  branch(function() {
    beds22<-get_server_count(env,"22")
    open22<-(get_capacity(env,"22Esc")>0)
    if (beds22==24 && !open22 && pressure_days>6) return(1)
    if (beds22<24 && open22) return(2)
    return(0)
  },ward_control_open_22Esc,ward_control_close_22Esc,continue=TRUE) %>% 
  branch(function() {
    beds<-get_server_count(env,"bed")
    open18<-(get_capacity(env,"18Esc")>0)
    if (beds>600 && !open18) return(1)
    if (beds<590 && open18) return(2)
    return(0)
  },ward_control_open_18Esc,ward_control_close_18Esc,continue=TRUE) %>% 
  
  
  
  
  rollback(6) ##to timeout
