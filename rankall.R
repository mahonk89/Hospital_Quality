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
