rankhospital <- function(state, outcome, num = "best") {
     ##Load necessary packages
     if (!require("dplyr")) {
          install.packages("dplyr")
          library("dplyr")
     }
     
     ## Read outcome data
     MyData <- read.csv(file="outcome-of-care-measures.csv", header=TRUE, 
         sep=",", na.strings= "Not Available", stringsAsFactors=FALSE )
     
     ##rename the columns
     names(MyData)[11] <- "heart_attack"
     names(MyData)[17] <- "heart_failure"
     names(MyData)[23] <- "pneumonia"
     
     ## Check that state and outcome are valid
     ##create subsets of state factors and outcome
     MyDataStates = unique(MyData$State)
     outcomes = c("heart attack"=11, "heart failure" =17, "pneumonia"=23) 
     Avail_Outcomes = c("heart attack", "heart failure", "pneumonia")
     
     ## Check that state and outcome are valid
     if (any(MyDataStates == state & any(Avail_Outcomes == outcome))) { 
     ##browser()
          if (outcome == "heart attack") {
               subset_df <- subset(MyData, State == state & !is.na(heart_attack), select = 
                     c(2, 7, outcomes[outcome]))
               subset_df = subset_df[order(subset_df$heart_attack),]
               num_rows = nrow(subset_df)
               subset_df$rank<-c(1:num_rows)
               browser()
               print(class(num)) 
               if(!is.na(as.numeric(num))) {
                    print(subset_df[wich(subset_df$rank == num), 'Hospital.Name'])
                    print(class(num))     
                    }else if(num == "best") {
                    print(subset_df[1, 'Hospital.Name'])
                    print(class(num)) 
               }else print(subset_df[nrow(subset_df), 'Hospital.Name'])
               
               #top_rows = top_n(subset_df, -1)
               #print(top_rows$Hospital.Name)
          ##browser()
          
          }else if (outcome == "heart failure") {
               subset_df <- subset(MyData, State == state & !is.na(heart_failure), select = 
                         c(2, 7, outcomes[outcome]))
               subset_df = subset_df[order(subset_df$heart_failure),]
               num_rows = nrow(subset_df)
               subset_df$rank<-c(1:num_rows)
               if(!is.na(as.numeric(num))) {
                    print(subset_df[which(subset_df$rank == num), 'Hospital.Name'])
               }else if(num == "best") {
                    print(subset_df[1, 'Hospital.Name'])
               }else print(subset_df[nrow(subset_df), 'Hospital.Name'])
          }else if (outcome == "pneumonia") {
               subset_df <- subset(MyData, State == state & !is.na(pneumonia), select = 
                                              c(2, 7, outcomes[outcome]))
               subset_df = subset_df[order(subset_df$pneumonia),]
               num_rows = nrow(subset_df)
               subset_df$rank<-c(1:num_rows)
               if(!is.na(as.numeric(num))) {
                    print(subset_df[which(subset_df$rank == num), 'Hospital.Name'])
               }else if(num == "best") {
                    print(subset_df[1, 'Hospital.Name'])
               }else print(subset_df[nrow(subset_df), 'Hospital.Name'])
               
          
          }  
          
     }
     else if (any(MyDataStates != state & any(Avail_Outcomes == outcome))) {print("invalid state, valid outcome")}
     else if (any(Avail_Outcomes != outcome & any(MyDataStates == state))) {print("valid state, invalid outcome")}
     else {print("both state and outcome invalid")}
     
     ## Return hospital name in that state with the given rank
     ## 30-day death rate
}