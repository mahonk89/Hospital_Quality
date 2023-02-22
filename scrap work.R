## test area

head(airquality)
airquality[,c(2,4,6)] #<-c("SOLAR UPD","TEMPNEW","DAYNUMB")

outcomeorigcolnames<colnames(outcome)
colnames(outcome)<-colnames()

aqm<-split(airquality,airquality$Month)
aqm7<-aqm$`7`
aqm7$Temp

best(,outcome)
head(subset(outcome,State == "TX",))
?subset
outcome$state=="TX"


best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
rankhospital("HI","heart attack",4)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
rankhospital("NJ","pneumonia","worst")

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
rankhospital("NV","heart failure",10)
