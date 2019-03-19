## test how much SUS data we have
## not designed to be run! works with one of the "generate admission frequency" scripts

testAllDays<-data.frame(dat=data$`Start Date (Hospital Provider Spell)`)

testEachDay<-names(which(table(testAllDays$dat)>20))

tallyAllDays<-testAllDays %>% group_by(dat) %>% tally()

tallyEpisodeAllDAys<-spellAllDays %>% group_by(dat) %>% tally()

plot(tallyEpisodeAllDAys$dat,tallyEpisodeAllDAys$n)

plot(tallyAllDays$dat,tallyAllDays$n)

tallyAdmissionMethods<-episodes %>% group_by(`Admission Method (Hospital Provider Spell)`) %>% tally()




data<-mutate(data,postEPR=(`Start Date (Hospital Provider Spell)`>20170901))
plot(table(data$`Admission Method (Hospital Provider Spell)`,data$postEPR))
plot(addmargins(table(data$`Admission Method (Hospital Provider Spell)`,data$postEPR),1))



#other plots:

plot(denominatorout$dateTime,denominatorout$N)