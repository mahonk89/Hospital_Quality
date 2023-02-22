# Ranking all hospitals in the country

rankall<-function(state,outcome,num="best"){
    on.exit(setwd(initialdir))
    setwd("C:/Users/Kevin/Documents/R/Projects/Hospital_Quality/data")
    oc<-read.csv("outcome-of-care-measures.csv")
    
    for(i in c(11,17,23)){
        oc[,i]<-suppressWarnings(as.numeric(oc[,i]))
    }
    
    st<-as.character(state)
    ocstate<-split(oc,oc$State) 
    statelist<-names(ocstate) 
    
    stopifnot("Invalid State" = st %in% statelist,
              "Invalid Outcome, please input heart attack, heart failure, pneumonia" = 
                  outcome %in% c("heart attack","heart failure","pneumonia"),
              "Invalid ranking, please input best, worst, or a number" = 
                  num == "best" | num == "worst" | is.numeric(num) == TRUE) 

    
    selectedstate<-ocstate[st] #subsets to the group/state selected
    selectedstate<-as.data.frame(selectedstate) #ensures data remains as data frame
    
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
}

# Originally I was going to just take the function rankhospital and rewrite it to
# loop over all states, however I can just loop the function within rankall now 
# and it shuold work

rankall<-function(outcome,num="best"){
    hlist<-c()
    for(s in states){
        
    sr<-rankhospital(s,outcome,num)

        if(is.na(sr)){
            hlist<-c(hlist,NA)
        } else {
            hlist<-c(hlist,sr)
        }
    }
    
    names(hlist)<-states
    listwithstate<-cbind(states,hlist)
    colnames(listwithstate)<-c("State","Hospital")
    return(listwithstate)
}

head(rankall("heart attack",20),10)
tail(rankall("pneumonia","worst"),3)
tail(rankall("heart failure"), 10)



# testing below
rhak<-rankhospital("AK","heart attack",20)
rhal<-rankhospital("AL","heart attack",20)
rhar<-rankhospital("AR","heart attack",20)
class(rhal)
rbind.data.frame(rhal,rhar,rhak)
dftest<-data.frame()
dftest
dftest<-rbind.data.frame(dftest,rep(NA,3))

## Actually must also write R code WITHOUT using rankhospital function


