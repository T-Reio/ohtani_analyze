cm <- function(x){
  out <- c()
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      out[i] <- NA
    } else {
      out[i] <- 30.48 * x[i]
    }
  }
  out
}

km <- function(x){
  out <- c()
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      out[i] <- NA
    } else {
      out[i] <- 1.61 * x[i]
    }
  }
  out
}