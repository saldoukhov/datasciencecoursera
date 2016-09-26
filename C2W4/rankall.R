rankall <- function(outcomeName, num = "best") {
    fieldMap <- list("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                     "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                     "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    if (num == "best")
        sf <- function(x) slice(x, 1)
    else
        if (num == "worst")        
            sf <- function(x) slice(x, n())
        else
            sf <- function(x) slice(x, num)
    outcomeParam <- as.character(fieldMap[outcomeName])
    filtered <- outcome %>%
        select_("Hospital.Name", "State", op = outcomeParam) %>%
        na.omit %>%
        group_by(State) %>%
        arrange(op, Hospital.Name) %>%
        sf
    filtered
}