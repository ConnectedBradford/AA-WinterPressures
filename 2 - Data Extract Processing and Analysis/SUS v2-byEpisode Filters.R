## Filtering brought out into its own file for consistency


##spells<-mutate(data,bizday=isBizday(timeDate(`_SpellStart_DateTime`),holidays=holidayLONDON(2000:2019))) - really slow
#load_rmetrics_calendars(2000:2022) ##nb we only get these holidays so may need extending in future

## remove day cases? (patient classification 1 includes emergencies who left the same day)
spells<-filter(spells,`Start Date (Hospital Provider Spell)`!=`Discharge Date (From Hospital Provider Spell)`)


##A few options here - comment out!
## 21 = A&E - expect flatter curve. 22=HP - expect very peaky (something weird there!). 
#spells<-filter(spells,`Admission Method (Hospital Provider Spell)`=="28") 


spells<-filter(spells,`Admission Method (Hospital Provider Spell)`!="82") #baby born here
spells<-filter(spells,`Admission Method (Hospital Provider Spell)`!="83") #baby born outside hospital
spells<-filter(spells,`Treatment Function Code`!="422") #neonates
spells<-filter(spells,`Treatment Function Code`!="424") #neonates - well babies

#spells<-filter(spells,`Year of Birth`<(year(`_SpellStart_DateTime`)-16)) #remove kids - needs altering to check specific dates

spells<-filter(spells,`Admission Method (Hospital Provider Spell)`!="31") #maternity antepartum
spells<-filter(spells,`Admission Method (Hospital Provider Spell)`!="32") #maternity postpartum
spells<-filter(spells,`Treatment Function Code`!="560") #midwife
spells<-filter(spells,`Treatment Function Code`!="501") #obstetrics

print("* Filtered *")