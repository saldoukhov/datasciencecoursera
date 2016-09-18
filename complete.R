complete <- function(directory, id = 1:332) {
  files <- list.files(directory, full.names = TRUE)[id]
  nobs <- files %>% lapply(read_csv) %>% lapply(complete.cases) %>% lapply(sum)
  data.frame(id = id, nobs = unlist(nobs))
}

complete("specdata", 1:2)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)x1

