filterstate <- function(state, outcomeName) {
    fieldMap <- list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                     "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                     "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    outcomeParam <- as.character(fieldMap[outcomeName])
    filtered <- filter(outcome, State == state) %>%
        select_("Hospital.Name", op = outcomeParam) %>%
        na.omit %>%
        arrange(op, Hospital.Name)
    filtered
}