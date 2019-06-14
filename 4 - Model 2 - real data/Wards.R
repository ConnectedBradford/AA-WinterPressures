
wards<-read.csv("../4 - Model 2 - real data/Wards.csv", header=TRUE,sep=",")

##dataframe - Ward, Beds, Restrictions, Surge
##Ward = name
##Beds = number of beds when open
##Restrictions:
## F - female only
## P - paeds only
##Surge:
## 1 - surge area (opening defined in model, starts shut)

print("* Wards Loaded *")
