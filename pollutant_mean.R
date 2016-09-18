pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <- list.files(directory, full.names = TRUE)[id]
  selpol <- function(x) {
    as.numeric(x[pollutant][[1]] %>% lapply(na.omit))
  }
  frame <- files %>% lapply(read_csv) %>% lapply(selpol)
  mean(unlist(frame), na.rm = TRUE)
}