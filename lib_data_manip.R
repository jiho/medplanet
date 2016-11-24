#
# Data manipulation functions
#

# dplyr compatible function to compute statistical summaries of a variable
sum_tbl <- function(x, var) {
  library("stringr")
  funs <- list(
    n=str_c("length(", var ,")"),
    n_NA=str_c("sum(is.na(", var ,"))"),
    min=str_c("min(",var,", na.rm=T)"),
    q25=str_c("stats::quantile(",var,", p=0.25, na.rm=T)"),
    median=str_c("stats::median(",var,", na.rm=T)"),
    mean=str_c("mean(",var,", na.rm=T)"),
    q75=str_c("stats::quantile(",var,", p=0.75, na.rm=T)"),
    max=str_c("max(",var,", na.rm=T)")
  )
  summarise_(x, .dots=funs)
}
