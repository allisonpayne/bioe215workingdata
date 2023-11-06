first_last_ch <- function(s) {
  first_chr <- substr(s, 1, 1)
  last_chr <- substr(s, nchar(s), nchar(s))
  result <- paste(first_chr, last_chr, sep = "")
  return(result)
}

text <- "amazing!"
first_last_ch(text)

#name = first_last_chr
#keyword = function
#parameter = s
#body = the big part {...}
#when writing functions they have parameters, when calling functions theyre called arguments


#"amazing!"

library(tidyverse)

first_last_ch("Make money")

mean <- function(x, na.rm = FALSE) {
  if (na.rm) {
  x <- x[!is.na(x)]
  }
  result <- sum(x) / length(x)
  return(result)
}

x <- mean(c(1, 2, NA), TRUE)



repeat_chr <- function(s, n, separator = "_") {
  repeated <- rep(s, n)
  result <- paste(repeated, collapse = separator)
  return(result)
}

repeat_chr("a", 3, " ")



