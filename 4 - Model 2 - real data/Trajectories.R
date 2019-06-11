## All trajectories defined in here for reuse purposes


## Data for various behaviours
## pr1 - best wards
## pr2 - OK wards
## pr3 - non-ideal wards
## dur - duration before moving to next stage (0=forever)
## nxt - next stage code number (active at end of duration or possibly if kicked out)

traj_pr1<-list()
traj_pr2<-list()
traj_pr3<-list()
traj_dur<-list()
traj_nxt<-list()



# x<-(20000)
# traj_pr1[[x]]<-c()
# traj_pr2[[x]]<-c()
# traj_pr3[[x]]<-c()
# traj_dur[[x]]<-Inf
# traj_nxt[[x]]<-x
## nb lists are not sparse - ie there is rubbish stored; don't use too high a value for x!


any_medical <- c("9","15","26","23","24","22","29","31","7","8","11","26","12Esc","17Esc","ACU","CDU","AMU1","AMU4","AMU4Esc","33","3","6")
any_surgical <- c("11","12","14","18","20","21","26","27","28","8")



 ##10002=MAU
x<-10002
traj_pr1[[x]]<-c("AMU1","AMU4","AMU4Esc")
traj_pr2[[x]]<-c("ACU","17Esc")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-24*3600
traj_nxt[[x]]<-x

 ##10001=SAU
x<-10001
traj_pr1[[x]]<-c("20")
traj_pr2[[x]]<-c("21","11","8","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-24*3600
traj_nxt[[x]]<-x


##10003=Paeds
x<-10003
traj_pr1[[x]]<-c("32")
traj_pr2[[x]]<-c("30")
traj_pr3[[x]]<-c("32","30")
traj_dur[[x]]<-24*3600
traj_nxt[[x]]<-x

## 1 - Emergency Gen Surgical
x<-1
traj_pr1[[x]]<-c("8","11")
traj_pr2[[x]]<-c("20","21","12","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x


# ## 2 - Emergency Urology
x<-2
traj_pr1[[x]]<-c("14")
traj_pr2[[x]]<-c("20","21","12","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x


# ## 3 - Emergency Maxfax
x<-3
traj_pr1[[x]]<-c("18","18Esc")
traj_pr2[[x]]<-c("27")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x



# 
# ## 4 - Emergency Plastics
x<-4
traj_pr1[[x]]<-c("27")
traj_pr2[[x]]<-c("28")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x


# ## 5 - Emergency ENT
x<-5
traj_pr1[[x]]<-c("18","18Esc")
traj_pr2[[x]]<-c("27")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 6 - Emergency Orthogeriatrics
x<-6
traj_pr1[[x]]<-c("31")
traj_pr2[[x]]<-c("28","27")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 7 - Emergency Ortho
x<-7
traj_pr1[[x]]<-c("28")
traj_pr2[[x]]<-c("27","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 8 - Emergency Ophth
x<-8
traj_pr1[[x]]<-c("18","18Esc")
traj_pr2[[x]]<-c("27")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 9 - Emergency Vascular
x<-9
traj_pr1[[x]]<-c("26")
traj_pr2[[x]]<-c("21","20","8","11","27","28")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 10 - Emergency Breast
x<-10
traj_pr1[[x]]<-c("12","12Esc")
traj_pr2[[x]]<-c("21","20","8","11","27","28")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 11 - Emergency Gynae
x<-11
traj_pr1[[x]]<-c("12","12Esc")
traj_pr2[[x]]<-c("21","20","8","11","27","28")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 99 - Emergency, unknown surgical
x<-99
traj_pr1[[x]]<-c("21","20","8","11","26","27","28","12")
traj_pr2[[x]]<-any_surgical
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x


# ## 100 - Emergency General Medicine
x<-100
traj_pr1[[x]]<-c("9","ACU","CDU")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x


# ## 101 - Emergency Gastro/Hepatologyx<-100
x<-101
traj_pr1[[x]]<-c("26","11","8")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 102 - Emergency Endocrine
x<-102
traj_pr1[[x]]<-c("9","ACU","CDU")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 103 - Emergency Cardiology
x<-103
traj_pr1[[x]]<-c("22")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 104 - Emergency Respiratory
x<-104
traj_pr1[[x]]<-c("23")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 105 - Emergency ID
x<-105
traj_pr1[[x]]<-c("7")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# 
# ## 106 - Emergency Haematology
x<-106
traj_pr1[[x]]<-c("33")
traj_pr2[[x]]<-c("24","17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 107 - Emergency Stroke/Neuro
x<-107
traj_pr1[[x]]<-c("6")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 108 - Emergency Renal
x<-108
traj_pr1[[x]]<-c("15")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 109 - Emergency Oncology
x<-109
traj_pr1[[x]]<-c("24")
traj_pr2[[x]]<-c("33","17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 110 - Emergency Geriatrics/Rehab
x<-110
traj_pr1[[x]]<-c("31","29")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-7*24*3600 #move on after a week - could change?
traj_nxt[[x]]<-111

#111 - Rehab elderly (not starting trajectory)
x<-110
traj_pr1[[x]]<-c("F5","F6","31","29")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x


# ## 199 - Emergency, unknown medical
x<-199
traj_pr1[[x]]<-c("9","ACU","CDU")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x
# 
# 
# ## 200 - Emergency paediatric
x<-200
traj_pr1[[x]]<-c("30")
traj_pr2[[x]]<-c("32")
traj_pr3[[x]]<-c("30","32")
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x
# 
# ## 999 = Generic Emergency code
x<-999
traj_pr1[[x]]<-c("9","ACU","CDU")
traj_pr2[[x]]<-any_medical
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

 
# ## 1001 = Elective Gen Surgical
# `_Elective`==TRUE & (`Treatment Function Code`==100 | `Treatment Function Code`==104| `Treatment Function Code`==106) ~ 1001,
# 
# ## 1002 = Elective Vascular
# `_Elective`==TRUE & (`Treatment Function Code`==107) ~ 1002,
# 
# ## 1003 = Elective Ortho
# `_Elective`==TRUE & (`Treatment Function Code`==110) ~ 1003,
# 
# ## 1004 = Elective ENT
# `_Elective`==TRUE & (`Treatment Function Code`==120) ~ 1004,
# 
# ## 1005 = Elective MaxFax
# `_Elective`==TRUE & (`Treatment Function Code`==140|`Treatment Function Code`==144) ~ 1005,
# 
# ## 1006 = Elective Plastics
# `_Elective`==TRUE & (`Treatment Function Code`==160) ~ 1006,
# 
# ## 1007 = Elective Gynae
# `_Elective`==TRUE & (`Treatment Function Code`==502) ~ 1007,
# 
# ## 1008 = Elective Urology
# `_Elective`==TRUE & (`Treatment Function Code`==101) ~ 1008,
# 
# ## 1009 = Elective Breast
# `_Elective`==TRUE & (`Treatment Function Code`==103) ~ 1009,
# 
# ## 1010 = Elective Ophth
# `_Elective`==TRUE & (`Treatment Function Code`==130) ~ 1010,
# 
# ## 1011 = Elective Pain
# `_Elective`==TRUE & (`Treatment Function Code`==191) ~ 1011,
# 
# 
# 
# 
# ## 1100 = Elective Gen Medicine
# `_Elective`==TRUE & (`Treatment Function Code`==300) ~ 1100,
# 
# ## 1101 = Elective Gastro/Hepatology
# `_Elective`==TRUE & (`Treatment Function Code`==301|`Treatment Function Code`==306) ~ 1101,
# 
# ## 1102 = Elective Haem
# `_Elective`==TRUE & (`Treatment Function Code`==303|`Treatment Function Code`==309) ~ 1102,
# 
# ## 1103 = Elective Cardiology
# `_Elective`==TRUE & (`Treatment Function Code`==320) ~ 1103,
# 
# ## 1104 = Elective Respiratory
# `_Elective`==TRUE & (`Treatment Function Code`==340) ~ 1104,
# 
# ## 1105 = Elective Oncology
# `_Elective`==TRUE & (`Treatment Function Code`==370 | `Treatment Function Code`==503 | `Treatment Function Code`==800) ~ 1105,
# 
# ## 1106 = Elective Neurology
# `_Elective`==TRUE & (`Treatment Function Code`==400|`Treatment Function Code`==328|`Treatment Function Code`==329) ~ 1106,
# 
# ## 1107 = Elective Rheumatology
# `_Elective`==TRUE & (`Treatment Function Code`==410) ~ 1107,
# 
# ## 1108 = Elective Geriatrics
# `_Elective`==TRUE & (`Treatment Function Code`==430|`Treatment Function Code`==314) ~ 1108,
# 
# ## 1109 = Elective Endocrine
# `_Elective`==TRUE & (`Treatment Function Code`==302) ~ 1109,
# 
# ## 1110 = Elective ID
# `_Elective`==TRUE & (`Treatment Function Code`==350) ~ 1110,
# 
# ## 1111 = Elective Renal
# `_Elective`==TRUE & (`Treatment Function Code`==361) ~ 1111,
# 
# 
# ## 1200 = Elective Paediatrics
# `_Elective`==TRUE & (`Treatment Function Code`==171|`Treatment Function Code`==321|`Treatment Function Code`==420|(`Treatment Function Code`>=211 & `Treatment Function Code`<=291)) ~ 1200,
# 
# 
# ## 1000 = Generic Elective code
# `_Elective`==TRUE ~ 1000,
# 
# ))
# 

#     