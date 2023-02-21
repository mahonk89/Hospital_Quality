initialdir<-getwd()

createdf<- function(dfname,filename){
    on.exit(setwd(initialdir))
    setwd("C:/Users/Kevin/Documents/R/Projects/Hospital_Quality/data")
    assign(dfname,read.csv(paste(filename,".csv",sep="")),envir = globalenv())
}

createdf("hospitaldata","hospital-data")
createdf("outcome","outcome-of-care-measures")
head(hospital_data_df)
head(outcome)

# Part 1:
# Plot the 30 day mortality rates for heart attack
# read the outcome data, save as outcome.

outcome[,11]<-as.numeric(outcome[,11])
hist(outcome[,11])
colnames(outcome)
class(outcome[,11])
ncol(outcome)
nrow(outcome)


# Part 2:
# Finding the best hospital in the state
# 2 hospital name
# 11 heart attack
# 17 heart failure
# 23 pneumonia
# 

best <- function(state,outcome){
    on.exit(setwd(initialdir))
    setwd("C:/Users/Kevin/Documents/R/Projects/Hospital_Quality/data")
    oc<-read.csv("outcome-of-care-measures.csv")
    
    for(i in c(11,17,23)){
        oc[,i]<-suppressWarnings(as.numeric(oc[,i]))
    }
    #for loop ensures each of the columns being worked on are numeric
    origoutcomecolnames<-colnames(oc)
    colnames(oc)[c(11,17,23)]<-c("heart attack","heart failure","pneumonia")

    st<-as.character(state)
    ocstate<-split(oc,oc$State) #splits the outcome df into groupings of states
    statelist<-names(ocstate) #vector of state abbreviations in the df
    stopifnot("Invalid State" = st %in% statelist,
              "Invalid Input NEW, please input heart attack, heart failure, pneumonia" = 
                  outcome %in% c("heart attack","heart failure","pneumonia")) 
    #stops the function if state does not exist within the state list or 
    #if the outcome is not one listed
    selectedstate<-ocstate[st] #subsets to the group/state selected
    selectedstate<-as.data.frame(selectedstate) #ensures data remains as data frame
    
    lowestmort<-0 # defining lowest mortality rate as 0, to be changed later
    ln<-0 # defining number of hospitals as 0, to be assigned later
    ocm<-c() # defining 'outcome mortality' as a null vector
    
    if(outcome=="heart attack"){
        lowestmort<-max(selectedstate[,11],na.rm = T)
        ln<-length(selectedstate[,11])
        ocm<-as.numeric(selectedstate[,11])
    } else if(outcome=="heart failure"){
        lowestmort<-max(selectedstate[,17],na.rm = T)
        ln<-length(selectedstate[,17])
        ocm<-as.numeric(selectedstate[,17])
    } else if(outcome=="pneumonia"){
        lowestmort<-max(selectedstate[,23],na.rm = T)
        ln<-length(selectedstate[,23])
        ocm<-as.numeric(selectedstate[,23])
    } 
# The if function above will set values and vectors based on the input outcome type
    # to be used in narrowing down the best hospital
#setting the hospital function as a null vector, to be filled in and replaced in the for loop

    hospital<-c()
    for(i in 1:ln){ # checking row by row what the lowest mortality rate is
        if(is.na(ocm[i])){ #skip this row if mortality rate is NA
            next
        }
        if(ocm[i]==lowestmort){ #if the next lowest mort rate is equal to lowest, include both
            hospital<-c(hospital,selectedstate[i,2])
        }
        if(ocm[i]<lowestmort){ # if the next lowest mort rate is uniquely lower, set it alone
            lowestmort<-ocm[i]
            hospital<-c(selectedstate[i,2])
        } 
    } #did not automatically handle ties yet!! will try to sort the hospital vector 
    #alphabetically then return hospital[1]
    sort(hospital)
    return(hospital[1])
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD","pneumonia")
best("BB", "heart attack")

best("MT", "heart attack")
best("SD", "heart failure")

head(best("TX","heart attack"))
best("NJ","car crash") # test for invalid outcome

#testing lowest mort pneumonia rates -- found that if block assigned [,17] for heart failure
#instead of pneumonia [,23] -- fixed and replaced.
outcomestate<-split(outcome,outcome$State)
ocMD<-as.numeric(outcomestate$MD)
ocMD[,23]<-as.numeric(ocMD[,23])
ocMD[,c(2,17)]
min(ocMD[,23])
rm(n,a)
states
states<-names(outcomestate)
#created for loop to test which state and outcome combo had a tie for the best
#value in order to correct the best() function to only return top in tie based
#on position in alphabet
for (n in 1:length(states)) {
    a<-as.character(states[n])
    l<-length(best(a,"heart attack"))
    if(l>1){print(paste(a,l,"heart attack"))}
    l<-length(best(a,"heart failure"))
    if(l>1){print(paste(a,l,"heart failure"))}
    l<-length(best(a,"pneumonia"))
    if(l>1){print(paste(a,l,"pneumonia"))}
}
