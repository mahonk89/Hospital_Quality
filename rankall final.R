rankall<-function(outcome,num="best"){
    on.exit(setwd(initialdir))
    setwd("C:/Users/Kevin/Documents/R/Projects/Hospital_Quality/data")
    oc<-read.csv("outcome-of-care-measures.csv")
    
    for(i in c(11,17,23)){
        oc[,i]<-suppressWarnings(as.numeric(oc[,i]))
    }
    
    ocstate<-split(oc,oc$State) #splits the outcome df into groupings of states
    statelist<-names(ocstate) #vector of state abbreviations in the df
    
    stopifnot("Invalid Outcome, please input heart attack, heart failure, pneumonia" = 
                  outcome %in% c("heart attack","heart failure","pneumonia"),
              "Invalid ranking, please input best, worst, or a number" = 
                  num == "best" | num == "worst" | is.numeric(num) == TRUE) 
    
    rankedlist<-c()
    sl<-c()
    
    for(s in statelist){
        selectedstate<-ocstate[s] #subsets to the group/state selected
        selectedstate<-as.data.frame(selectedstate) #ensures data remains as data frame
        sl<-c(sl,s)
        
        if(outcome=="heart attack"){
            ranked<-selectedstate[order(selectedstate[,11],selectedstate[,2]),]
            ranked[,11]<-as.numeric(ranked[,11])
            if(is.numeric(num)==T) {
                rankedlist<-c(rankedlist,ranked[num,2])
                #if(is.na(ranked[num,11])) rankedlist<-c(rankedlist,NA)
            }
            if(!is.numeric(num)) {
                if(num=="worst"){
                    rankedlist<-ranked[which.max(ranked[,11]),2]
                }
                if(num=="best") {
                    rankedlist<-ranked[1,2]
                }
            }
            
        }
        if(outcome=="heart failure"){
            ranked<-selectedstate[order(selectedstate[,17],selectedstate[,2]),]
            ranked[,17]<-as.numeric(ranked[,17])
            if(is.numeric(num)==T) {
                rankedlist<-c(rankedlist,ranked[num,2])
                #if(is.na(ranked[num,17])) rankedlist<-c(rankedlist,NA)
            }
            if(!is.numeric(num)) {
                if(num=="worst"){
                    rankedlist<-c(rankedlist,ranked[which.max(ranked[,17]),2])
                }
                if(num=="best") {
                    rankedlist<-c(rankedlist,ranked[1,2])
                }
            }
            
        }
        if(outcome=="pneumonia"){
            ranked<-selectedstate[order(selectedstate[,23],selectedstate[,2]),]
            ranked[,23]<-as.numeric(ranked[,23])
            if(is.numeric(num)==T) {
                rankedlist<-c(rankedlist,ranked[num,2])
                #if(is.na(ranked[num,23])) rankedlist<-c(rankedlist,NA)
            }
            if(!is.numeric(num)) {
                if(num=="worst"){
                    rankedlist<-c(rankedlist,ranked[which.max(ranked[,23]),2])
                }
                if(num=="best") {
                    rankedlist<-c(rankedlist,ranked[1,2])
                }
            }
            
        }
    }
    rankedlist<-as.character(rankedlist)
    r<-as.data.frame(cbind(rankedlist,sl))
    colnames(r)<-c("hospital","state")
    return(r)
}

rankall("heart failure", 10)
rankhospital("NV","heart failure",10)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
rankhospital("NV","heart failure",10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
rankhospital("HI","heart attack",4)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
rankhospital("NJ","pneumonia","worst")