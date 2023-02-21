# ranking hospitals by outcome in a state

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
            r<-cbind(ranked[num,c(2,11)],num)
            colnames(r)<-c("Hospital.Name","Mortality.Rate","Rank")
            if(max(num)>sum(!is.na(ranked[,11]))) return(NA)
        }
        if(!is.numeric(num)) {
            if(num=="worst"){
                r<-cbind(ranked[which.max(ranked[,11]),c(2,11)],"worst")
                colnames(r)<-c("Hospital.Name","Mortality.Rate","Rank")
            }
            if(num=="best") {
                r<-cbind(ranked[1,c(2,11)],"best")
                colnames(r)<-c("Hospital.Name","Mortality.Rate","Rank")
            }
        }
        
    }
    if(outcome=="heart failure"){
        ranked<-selectedstate[order(selectedstate[,17],selectedstate[,2]),]
        if(is.numeric(num)==T) {
            r<-cbind(ranked[num,c(2,17)],num)
            colnames(r)<-c("Hospital.Name","Mortality.Rate","Rank")
            if(max(num)>sum(!is.na(ranked[,17]))) return(NA)
        }
        if(!is.numeric(num)) {
            if(num=="worst"){
                r<-cbind(ranked[which.max(ranked[,17]),c(2,17)],"worst")
                colnames(r)<-c("Hospital.Name","Mortality.Rate","Rank")
            }
            if(num=="best") {
                r<-cbind(ranked[1,c(2,17)],"best")
                colnames(r)<-c("Hospital.Name","Mortality.Rate","Rank")
            }
        }
        
    }
    if(outcome=="pneumonia"){
        ranked<-selectedstate[order(selectedstate[,23],selectedstate[,2]),]
        if(is.numeric(num)==T) {
            r<-cbind(ranked[num,c(2,23)],num)
            colnames(r)<-c("Hospital.Name","Mortality.Rate","Rank")
            if(max(num)>sum(!is.na(ranked[,23]))) return(NA)
        }
        if(!is.numeric(num)) {
            if(num=="worst"){
                r<-cbind(ranked[which.max(ranked[,23]),c(2,23)],"worst")
                colnames(r)<-c("Hospital.Name","Mortality.Rate","Rank")
            }
            if(num=="best") {
                r<-cbind(ranked[1,c(2,23)],"best")
                colnames(r)<-c("Hospital.Name","Mortality.Rate","Rank")
            }
        }

    }
    return(r)
    #return(ranked[,c(2,11)])
}


rankhospital("MA","heart attack",58)
rankhospital("TX","heart failure",4)
    ## STOPPED 2/20/23
    ## Running into the issue where hospital names are not being sorted alphabetically
# in the event of a tie. Unable to solve at this time. setting order(ranked[,c(17,2)])
# does NOT solve this by ranking from 17 THEN 2 alphabetically. Need to correct. 

rankhospital("MD","heart attack","worst")
rankhospital("MN", "heart attack", 5000)

#the way worst is handled is neat. Subsetting ranked matrix using the function
# which.max applied on that column, which will notate the position/row of that max figure
# when applied to the row index will then return the hospital associated. This is being 
# used because we are allowing NAs to remain. Also could do ranked[sum(!is.na(ranked[,17])),2]
# which will return the last non-NA value aka max in an ordered list

# This if will handle if num is a non-numeric argument "best". This can be done outside
#of the ifs above since the column is already being ordered by the required outcome,
# this is the most simple way to return the correct value.

best("MA","heart attack")
ocMA<-outcomestate$MA
ocTX<-outcomestate$TX
ocMA[,c(2,11)]
ocTX[,17]<-as.numeric(ocTX)[,17]
ocTX[order(ocTX[,17]),c(2,17)]
