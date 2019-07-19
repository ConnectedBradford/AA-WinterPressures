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
any_surgical <- c("8","11","12","14","18","SAU20","21","26","27","28")



 ##10002=MAU
x<-10002
traj_pr1[[x]]<-c("AMU1","AMU4","AMU4Esc")
traj_pr2[[x]]<-c("ACU","17Esc")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-48*3600
traj_nxt[[x]]<-x

 ##10001=SAU
x<-10001
traj_pr1[[x]]<-c("SAU20")
traj_pr2[[x]]<-c("21","11","8","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-48*3600
traj_nxt[[x]]<-x


##10003=Paeds
x<-10003
traj_pr1[[x]]<-c("32")
traj_pr2[[x]]<-c("30")
traj_pr3[[x]]<-c("32","30")
traj_dur[[x]]<-48*3600
traj_nxt[[x]]<-x

## 1 - Emergency Gen Surgical
x<-1
traj_pr1[[x]]<-c("8","11","21")
traj_pr2[[x]]<-c("SAU20","12","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x


# ## 2 - Emergency Urology
x<-2
traj_pr1[[x]]<-c("14")
traj_pr2[[x]]<-c("SAU20","21","12","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x


# ## 3 - Emergency Maxfax
x<-3
traj_pr1[[x]]<-c("18","18Esc","27")
traj_pr2[[x]]<-any_surgical
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
traj_pr2[[x]]<-any_surgical
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 6 - Emergency Orthogeriatrics
x<-6
traj_pr1[[x]]<-c("31","29")
traj_pr2[[x]]<-c("3","28","27")
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
traj_pr2[[x]]<-any_surgical
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 9 - Emergency Vascular
x<-9
traj_pr1[[x]]<-c("26")
traj_pr2[[x]]<-c("21","SAU20","27","8","11")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 10 - Emergency Breast
x<-10
traj_pr1[[x]]<-c("12","12Esc")
traj_pr2[[x]]<-c("21","SAU20","8","11")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 11 - Emergency Gynae
x<-11
traj_pr1[[x]]<-c("12","12Esc")
traj_pr2[[x]]<-c("21","SAU20","8","11")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 99 - Emergency, unknown surgical
x<-99
traj_pr1[[x]]<-c("21","SAU20","8","11","26","12")
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
traj_pr1[[x]]<-c("33","24")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
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
traj_pr1[[x]]<-c("31","29","3")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-7*24*3600 #move on after a week - could change?
traj_nxt[[x]]<-111

#111 - Rehab elderly (not starting trajectory)
x<-111
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

x<-1001
traj_pr1[[x]]<-c("8","11","21")
traj_pr2[[x]]<-c("SAU20","12","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1002 = Elective Vascular
x<-1002
traj_pr1[[x]]<-c("26","21")
traj_pr2[[x]]<-c("SAU20","8","11","27","28")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1003 = Elective Ortho
x<-1003
traj_pr1[[x]]<-c("28","27")
traj_pr2[[x]]<-c("26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1004 = Elective ENT
x<-1004
traj_pr1[[x]]<-c("18","18Esc")
traj_pr2[[x]]<-c("27")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1005 = Elective MaxFax
x<-1005
traj_pr1[[x]]<-c("18","18Esc")
traj_pr2[[x]]<-c("27")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1006 = Elective Plastics
x<-1006
traj_pr1[[x]]<-c("27","28")
traj_pr2[[x]]<-c("26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1007 = Elective Gynae
x<-1007
traj_pr1[[x]]<-c("12","12Esc")
traj_pr2[[x]]<-c("21","SAU20","8","11","27","28")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1008 = Elective Urology
x<-1008
traj_pr1[[x]]<-c("14","12","12Esc")
traj_pr2[[x]]<-c("SAU20","21","12","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1009 = Elective Breast
x<-1009
traj_pr1[[x]]<-c("12","12Esc")
traj_pr2[[x]]<-c("21","SAU20","8","11","27","28")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1010 = Elective Ophth
x<-1010
traj_pr1[[x]]<-c("18","18Esc","27")
traj_pr2[[x]]<-c("31","3")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1011 = Elective Pain
x<-1011
traj_pr1[[x]]<-c("8","11")
traj_pr2[[x]]<-c("SAU20","12","26")
traj_pr3[[x]]<-any_surgical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1100 = Elective Gen Medicine
x<-1100
traj_pr1[[x]]<-c("9","ACU","CDU")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1101 = Elective Gastro/Hepatology
x<-1101
traj_pr1[[x]]<-c("26","11","8")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1102 = Elective Haem
x<-1102
traj_pr1[[x]]<-c("33","24")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1103 = Elective Cardiology
x<-1103
traj_pr1[[x]]<-c("22")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x
# 
# ## 1104 = Elective Respiratory
x<-1104
traj_pr1[[x]]<-c("23")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1105 = Elective Oncology
x<-1105
traj_pr1[[x]]<-c("24")
traj_pr2[[x]]<-c("33","17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1106 = Elective Neurology
x<-1106
traj_pr1[[x]]<-c("6")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1107 = Elective Rheumatology
# not sure about this one
x<-1107
traj_pr1[[x]]<-c("F5","F6")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1108 = Elective Geriatrics
x<-1108
traj_pr1[[x]]<-c("31","29","3")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-7*24*3600 #move on after a week - could change?
traj_nxt[[x]]<-111


# ## 1109 = Elective Endocrine
x<-1109
traj_pr1[[x]]<-c("9","ACU","CDU")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1110 = Elective ID
x<-1110
traj_pr1[[x]]<-c("7")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1111 = Elective Renal
x<-1111
traj_pr1[[x]]<-c("15")
traj_pr2[[x]]<-c("17Esc","AMU1","AMU4","AMU4Esc","9","ACU","CDU")
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1200 = Elective Paediatrics
x<-1200
traj_pr1[[x]]<-c("30")
traj_pr2[[x]]<-c("32")
traj_pr3[[x]]<-c("30","32")
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

# ## 1000 = Generic Elective code
x<-1000
traj_pr1[[x]]<-c("8","11","21")
traj_pr2[[x]]<-any_surgical
traj_pr3[[x]]<-any_medical
traj_dur[[x]]<-Inf
traj_nxt[[x]]<-x

print("* Trajectories Loaded *")
