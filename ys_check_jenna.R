library(testthat)

parse_list_no_char <- function(ranges) {
  disc <- list()
  cont <- list()
  
  for(i in ranges) {
    if (length(i) == 2) {
      cont <- append(cont, i)
      #cont[[length(cont) + 1]] <- i
    } # if length 2
    
    else if (length(i) == 1) {
      disc <- append(disc, i)
      #disc[[length(disc) + 1]] <- i
    } # else if length 1
    
    else { # else length > 2
      stop("error: length of range > 2")
    } # else
  } # for 
  return(list(cont, disc))
} # parse_list_no_char


check_values <- function(x,values,verbose=FALSE, con = NULL, env = list()) {
  if(is.null(values)  | is.null(x)) return(TRUE)
  x <- x[!is.na(x)]
  x <- unlist(unique(x),use.names = FALSE)
  
  if(verbose | !is.null(con)) {
    valu <- values
    if(length(values) > 3) {
      valu <- c(valu[seq(3)], "...")
    }
    if(verbose) message("    values: ", paste0(valu, collapse = ','))
    if(!is.null(con)) {
      cata( "    values: ", paste0(valu, collapse = ','),file = con)
    }
  }
  if(length(x)==0) return(TRUE)
  ans <- all(x %in% values)
  if(!ans) {
    env$check_values_bad <- setdiff(x,values)  
  }
  return(ans)
}


check_range_test <- function(x,range,verbose=FALSE, con = NULL) {
library(rlang)

  if(is.null(range) | is.null(x)) return(TRUE)
  #if(length(range) !=2) return(FALSE)
  x <- x[!is.na(x)]
  if(length(x)==0) return(TRUE)
  if(verbose | !is.null(con)) {
    if(verbose) message("    range: ", paste0(range, collapse = ","))
    if(!is.null(con)) {
      cata("    range: ", paste0(range, collapse = ","),file = con)
    }
  }
  # make a list if not a list
  if (!is.list(range)) {
    range <- list(range) }
  
  # Parse entire list
  lists <- parse_list_no_char(range)
  cont <- unlist(lists[1])
  disc <- unlist(lists[2])
  
  # remove values in range first
  min <- cont[1]
  max <- cont[2]
 x <- x[!(x <= max & x >= min)]
  
  # remove values in discrete set
  x <- x[!(x %in% disc)]
  
  # if x is empty, everything was in range(s)
  empty <- (is_empty(x))
  return(empty)

} # check_range_test

AMT1 <- c(2)
AMT2 <- c(21)
AMT3 <- c(-888)
df <- data.frame(AMT1, AMT2, AMT3)

spec <- ys_load("~/Projects/yspec/test_ys_check.yml")
print(check_range_test(df$AMT3, spec$AMT3$range))




