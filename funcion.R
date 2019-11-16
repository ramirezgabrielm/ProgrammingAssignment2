# best <- function(state, outcome) {
#   ## Read outcome data
#   csv_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#   csv_state <- csv_outcome$State
# 
#   ## Check that state and outcome are valid
#   
#   if(! state %in% csv_state){
#     
#     stop("invalid state")
#     
#   }
#   
#   if( outcome == "heart attack"){
#     
#     getCol <- 11
#     
#   }else if( outcome == "heart failure"){
#     
#     getCol <- 17
# 
#   }else if( outcome == "pneumonia"){
#   
#     getCol <- 23
#     
#   }else{
#     
#     stop("invalid outcome")
#     
#   }
#   ## Return hospital name in that state with lowest 30-day death
#   csv_outcome[, getCol] <- suppressWarnings(as.numeric(csv_outcome[, getCol]))
#   csv_by_state <- subset(csv_outcome, State == state)
#   low_hospital <- order(csv_by_state[,getCol])[1]
#   
#   ## rate
#   
#   csv_by_state[low_hospital,2]
#   
# }


best <- function(state, outcome) {
    ## Read outcome data
    csv_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    csv_state <- csv_outcome$State
    
    ## Check that state and outcome are valid
    
    if(! state %in% csv_state){
        
        stop("invalid state")
        
    }
    
    if( outcome == "heart attack"){
        
        getCol <- 11
        
    }else if( outcome == "heart failure"){
        
        getCol <- 17
        
    }else if( outcome == "pneumonia"){
        
        getCol <- 23
        
    }else{
        
        stop("invalid outcome")
        
    }
    ## Return hospital name in that state with lowest 30-day death
    
    
    csv_outcome[, getCol] <- suppressWarnings(as.numeric(csv_outcome[, getCol]))
    csv_by_state <- subset(csv_outcome, State == state)
    
    csv_by_state[order(csv_by_state[,getCol],csv_by_state[,2] ),2][1]
    
    ## rate
    
    
}


rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    csv_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    csv_state <- csv_outcome$State
    ## Check that state and outcome are valid
    
    
    if(! state %in% csv_state){
        
        stop("invalid state")
        
    }
    
    if( outcome == "heart attack"){
        
        getCol <- 11
        
    }else if( outcome == "heart failure"){
        
        getCol <- 17
        
    }else if( outcome == "pneumonia"){
        
        getCol <- 23
        
    }else{
        
        stop("invalid outcome")
        
    }
    ## Return hospital name in that state with the given rank

    csv_outcome[, getCol] <- suppressWarnings(as.numeric(csv_outcome[, getCol]))
    csv_by_state <- subset(csv_outcome, State == state,select=c(2,getCol))
    colnames(csv_by_state) <- c("hospital", "rate")
    csv_by_state_wo_na <- subset(csv_by_state,!is.na(rate))
    
    if(num=="best"){
        numm <- 1
    }else if(num=="worst"){
        numm <- length(csv_by_state_wo_na[,2])
        
    }else{
        numm <- num
    }
    csv_by_state_wo_na[order(csv_by_state_wo_na[,2],csv_by_state_wo_na[,1] ),1][numm]
    
    ## 30-day death rate
}



rankall <- function(outcome, num = "best") {
    
    by_num <- function(x,arg_num){
        if(arg_num=="best"){
            numm <- 1
        }else if(arg_num=="worst"){
            numm <- length(x)
            
        }else{
            numm <- arg_num
        } 
        
        x[numm]
        
        
    }
    ## Read outcome data
    csv_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    
    
    if( outcome == "heart attack"){
        
        getCol <- 11
        
    }else if( outcome == "heart failure"){
        
        getCol <- 17
        
    }else if( outcome == "pneumonia"){
        
        getCol <- 23
        
    }else{
        
        stop("invalid outcome")
        
    }
    
    csv_by_position <- csv_outcome[order(csv_outcome$State,suppressWarnings(as.numeric(csv_outcome[,getCol])),csv_outcome$Hospital.Name),c(2,7,getCol)]
    #tapply(csv_by_position$Hospital.Name,csv_by_position$State,function(x) x[num])
    colnames(csv_by_position) <- c("Hospital.Name", "State","rate")
    csv_by_position_wo_na <- subset(csv_by_position,!is.na(rate))
    
    if(num=="worst"){
    fin <- aggregate(Hospital.Name ~ State,csv_by_position_wo_na, by_num, arg_num=num)
    
    }else{
    fin <- aggregate(Hospital.Name ~ State,csv_by_position, by_num, arg_num=num)
    }
    fin
}

