
best <- function(state, outcome) {
     ## Read outcome data
     MyData <- read.csv(file="outcome-of-care-measures.csv", header=TRUE, 
          sep=",", na.strings= "Not Available", stringsAsFactors=FALSE )
     names(MyData)[11] <- "heart_attack"
     names(MyData)[17] <- "heart_failure"
     names(MyData)[23] <- "pneumonia"
     ##print(head(MyData))
     ##print(str(MyData))
     ##create subsets of state factors and outcome
     MyDataStates = unique(MyData$State)
     outcomes = c("heart attack"=11, "heart failure" =17, "pneumonia"=23) 
     
     ##print(str(MyData))
     
     ##new_df = MyData[, c(2,7,outcomes[outcome])]
     
     ##print(str(new_df))
     
     ##print(class(outcomes))
     ##Outcomes_cols = c(11, 17, 23)
     ##Outcomes_combined = data.frame(outcomes=Avail_Outcomes, col_num = Outcomes_cols)
     
    
     ##browser()
     ## Check that state and outcome are valid
     if (any(MyDataStates == state & any(Avail_Outcomes == outcome))) { 
     
          if (outcome == "heart attack") {
               heart_attack_df <- subset(MyData, State == state & !is.na(heart_attack), select = 
                    c(2, 7, outcomes[outcome]))
               top_rows = top_n(heart_attack_df, -1)
               print(top_rows$Hospital.Name)
               
          }else if (outcome == "heart failure") {
               heart_attack_df <- subset(MyData, State == state & !is.na(heart_attack), select = 
                     c(2, 7, outcomes[outcome]))
               top_rows = top_n(heart_attack_df, -1)
               print(top_rows$Hospital.Name)
               
          }else if (outcome == "pneumonia") {
               heart_attack_df <- subset(MyData, State == state & !is.na(heart_attack), select = 
                     c(2, 7, outcomes[outcome]))
               top_rows = top_n(heart_attack_df, -1)
               print(top_rows$Hospital.Name)
          }
          
          
          
     }
     else if (any(MyDataStates != state & any(Avail_Outcomes == outcome))) {print("invalid state, valid outcome")}
     else if (any(Avail_Outcomes != outcome & any(MyDataStates == state))) {print("valid state, invalid outcome")}
     else {print("both state and outcome invalid")}
                  
     
     ## Return hospital name in that state with lowest 30-day death
     ## rate   
}
