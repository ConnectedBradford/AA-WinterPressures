
wards<-read.csv("Wards.csv", header=TRUE,sep=",")

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

# for (ward in 1:nrow(wards)) {
#   print(wards[ward,"Ward"])
#   print("*")
# }


print("* Wards Loaded *")
