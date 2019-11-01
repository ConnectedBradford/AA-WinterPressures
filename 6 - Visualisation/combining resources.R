

combine_resources_to_xts<-function(data,resources) {
  xts<-xts()
  for (res in resources) {
    print(res)
    tmp2<-filter(data,resource==res)
    print(nrow(tmp2))
    xts2<-to.period(xts(x=tmp2$server,order.by=tmp2$rtime),"days")
    if (length(xts)>0) {
      xts_union<-na.locf(merge(xts,index(xts2)))
      xts2_union<-na.locf(merge(xts2,index(xts)))
      xts<-xts_union+xts2_union
    } else {
      xts<-xts2
    }
  }
  names(xts)<-c("Open","High","Low","Close")
  return(xts)
}


test2<-resources %>%
  filter(str_detect(resource,"Traj")) %>% 
  group_by(resource, replication) %>%
  mutate(server = diff(c(0, server))) %>%
  ungroup() %>%
  mutate(resource = ifelse(grepl("104", resource), "Respiratory", resource)) %>%
  group_by(resource, replication) %>%
  mutate(server = cumsum(server))




tmp<-head(resources,20000)
tmp$rtime<-round_date(tmp$time,unit="day")
tmp_ICU<-filter(tmp,resource=="ICU")
tmp_AMU<-filter(tmp,resource=="AMU1")
xts_ICU<-to.period(xts(x=tmp_ICU$server,order.by=tmp_ICU$rtime),"days")
xts_AMU<-to.period(xts(x=tmp_AMU$server,order.by=tmp_AMU$rtime),"days")
xts_ICU_union<-na.locf(merge(xts_ICU,index(xts_AMU)))
xts_AMU_union<-na.locf(merge(xts_AMU,index(xts_ICU)))
xts_total_union<-xts_ICU_union+xts_AMU_union
