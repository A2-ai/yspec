# check_range <- function(x,range,verbose=FALSE, con = NULL) {
#   if(is.null(range) | is.null(x)) return(TRUE)
#   if(length(range) !=2) return(FALSE)
#   x <- x[!is.na(x)]
#   if(length(x)==0) return(TRUE)
#   if(verbose | !is.null(con)) {
#     if(verbose) message("    range: ", paste0(range, collapse = ","))
#     if(!is.null(con)) {
#       cata("    range: ", paste0(range, collapse = ","),file = con)
#     }
#   }
#   x <- sort(range(x))
#   range <- sort(range)
#   x[1] >= range[1] & x[2] <= range[2]
# }

parse_list_no_char <- function(ranges) {
  disc <- list()
  cont <- list()
  
  for(i in ranges) {
    if (length(i) == 2) {
      cont[[length(cont) + 1]] <- i
    } # if length 2
    
    else if (length(i) == 1) {
      disc[[length(disc) + 1]] <- i
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


check_range <- function(x,ranges,verbose=FALSE, con = NULL) {
  if(is.null(ranges) | is.null(x)) return(TRUE)
  if(length(x)==0) return(TRUE)
  if(verbose | !is.null(con)) {
    if(verbose) message("    range: ", paste0(ranges, collapse = ","))
    if(!is.null(con)) {
      cata("    range: ", paste0(ranges, collapse = ","),file = con)
    }
  }
  
   
  # remove missing values
  x <- x[!is.na(x)]
  # make values unique
  x <- unlist(unique(x),use.names = FALSE)
  
  # get disc values and ranges
  lists <- parse_list_no_char(ranges)
  
  cont <- lists[1]
  discrete <- lists[2]
  
  # remove all discrete values from x
  x <- setdiff(x,discrete) 
  
  # get union of ranges
  # if (length(cont) == 2) {
  #   range <- union(cont[1], cont[2])
  # }
  # else if (length(cont) == 1) {
  #   range <- cont[1]
  # }
  # else {
  #   stop("error: too many ranges")
  # }
  
  # check if remaining values in range
  if (length(cont) > 1) {
    stop("error: ys_check only works with 1 cont. range right now")
  }
  x <- sort(range(x))
  cont <- sort(cont)
  x[1] >= cont[1] & x[2] <= cont[2]
  
} #check_discont_range

AMT1 <- c(2, 35)
AMT2 <- c(3, -999)
df <- data.frame(AMT1, AMT2)
df
range <- list(c(0, 20), c(40, 50))


AMT1 <- c(2, 35)
AMT2 <- c(3, -999)
df <- data.frame(AMT1, AMT2)
df
ys_check(df, spec)

#check_discont_range(df, range)

