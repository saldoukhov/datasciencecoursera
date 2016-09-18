corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE)
  obs <- files %>% lapply(read_csv) %>% lapply(na.omit)
  rcount <- function (f) {
    nrow(f)
  }
  counts <- unlist(obs %>% lapply(rcount))
  cor1 <- function(f) {
    cor(f[[2]], as.numeric(f[[3]]))
  }
  unlist(obs[counts > threshold] %>% lapply(cor1))
}
