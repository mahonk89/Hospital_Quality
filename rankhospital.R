initialdir<-getwd()
rankhospital<-function(state,outcome,num="best"){
    on.exit(setwd(initialdir))
    setwd("C:/Users/Kevin/Documents/R/Projects/Hospital_Quality/data")
    oc<-read.csv("outcome-of-care-measures.csv")
    
    for(i in c(11,17,23)){
        oc[,i]<-suppressWarnings(as.numeric(oc[,i]))
    }
    
    st<-as.character(state)
    ocstate<-split(oc,oc$State) #splits the outcome df into groupings of states
    statelist<-names(ocstate) #vector of state abbreviations in the df
    
    stopifnot("Invalid State" = st %in% statelist,
              "Invalid Outcome, please input heart attack, heart failure, pneumonia" = 
                  outcome %in% c("heart attack","heart failure","pneumonia"),
              "Invalid ranking, please input best, worst, or a number" = 
                  num == "best" | num == "worst" | is.numeric(num) == TRUE) 
    #stops the function if state does not exist within the state list or 
    #if the outcome is not one listed
    
    selectedstate<-ocstate[st] #subsets to the group/state selected
    selectedstate<-as.data.frame(selectedstate) #ensures data remains as data frame
    
    colnames(selectedstate)[c(11,17,23)]<-c("Heart.Attack","Heart.Failure","Pneumonia")
    #this colnames function might be useless but I did it to try and subset via $
    
    # the following if blocks will make a new data frame 'ranked' with the data sorted
    # based on the input argument outcome and the associated column
    # essentially saying subset the rows based on the order of selectedstate col# 11
    # the rows of the entire 46 row df are locked together and then sorted by col# 11/17/23
    if(outcome=="heart attack"){
        ranked<-selectedstate[order(selectedstate[,11],selectedstate[,2]),]
        ranked[,11]<-as.numeric(ranked[,11])
        if(is.numeric(num)==T) {
            r<-ranked[num,2]
            if(max(num)>sum(!is.na(ranked[,11]))) return(NA)
        }
        if(!is.numeric(num)) {
            if(num=="worst"){
                r<-ranked[which.max(ranked[,11]),2]
            }
            if(num=="best") {
                r<-ranked[1,2]
            }
        }
        
    }
    if(outcome=="heart failure"){
        ranked<-selectedstate[order(selectedstate[,17],selectedstate[,2]),]
        ranked[,17]<-as.numeric(ranked[,17])
        if(is.numeric(num)==T) {
            r<-ranked[num,2]
            if(max(num)>sum(!is.na(ranked[,17]))) return(NA)
        }
        if(!is.numeric(num)) {
            if(num=="worst"){
                r<-ranked[which.max(ranked[,17]),2]
            }
            if(num=="best") {
                r<-ranked[1,2]
            }
        }
        
    }
    if(outcome=="pneumonia"){
        ranked<-selectedstate[order(selectedstate[,23],selectedstate[,2]),]
        ranked[,23]<-as.numeric(ranked[,23])
        if(is.numeric(num)==T) {
            r<-ranked[num,2]
            if(max(num)>sum(!is.na(ranked[,23]))) return(NA)
        }
        if(!is.numeric(num)) {
            if(num=="worst"){
                r<-ranked[which.max(ranked[,23]),2]
            }
            if(num=="best") {
                r<-ranked[1,2]
            }
        }
        
    }
    return(r)
}