
wards<-read.csv("../4 - Model 2 - real data/Wards.csv", header=TRUE,sep=",")

##dataframe - Ward, Beds, Restrictions, Surge
##Ward = name
##Beds = number of beds when open
##Restrictions:
## F - female only
## P - paeds only
##Surge:
## 1 - surge area (opening defined in model, starts shut)

wards$Ward<-as.character(wards$Ward)
#for some reason we end up with the ward names as a factor variable which causes trouble when it's used in the "put the patient anywhere" rule

print("* Wards Loaded *")
