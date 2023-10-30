cont_to_string <- function(cont) {
  return(glue::glue_collapse(cont, sep = " or "))
}

disc_to_string <- function(disc) {
  mid <- glue::glue_collapse(disc, sep = ", ")
  return(glue("{{{mid}}}"))
}

parse_list <- function(ranges) {
  disc <- list()
  cont <- list()
  
  for(i in ranges) {
    if (length(i) == 2) {
      cont[[length(cont) + 1]] <- glue("[{i[1]} to {i[2]}]")
    } # if length 2
    
    else if (length(i) == 1) {
      disc[[length(disc) + 1]] <- i
    } # else if length 1
    
    else { # else length > 2
      stop("error: length of range > 2")
    } # else
  } # for 
  return(list(cont, disc))
} # parse_list

full_string <- function(contString, discString) {
  # assume there is at least one range or value defined
  if (length(discString) == 0) {
    return(glue("valid range: {contString}"))
  } # if
  
  else if (length(contString) == 0) {
    return(glue("valid range: {discString}"))
  } # else if
  
  else { # else there are cont and disc ranges
    return(glue("valid range: {contString} or {discString}"))
  } # else
} # full_string

ranges_to_define <- function(ranges) {
  # make a list if not a list
  if (!is.list(ranges)) {
    ranges <- list(ranges) }
  
  # Parse entire list
  lists <- parse_list(ranges)
  cont <- lists[1]
  disc <- lists[2]
  
  # format ranges
  contString <- cont_to_string(cont[[1]])
  # format discrete values
  discString <- disc_to_string(disc[[1]])
  # return full formatted output
  return(full_string(contString, discString))
}

x_table_2_details <- function(x) {
  codes <- NULL
  if(.has("values", x)) {
    codes <- pack_codes(x)  
  }
  range <- NULL
  if(.has("range", x)) {
    #range <- paste0("valid range: [", x$range[1], " to ",  x$range[2], "]")  
    range <- ranges_to_define(x$range)
    #range <- ""
  }
  source <- NULL
  if(.has("source", x)) {
    source <- paste0("source: ", x$source)
  }
  details <- c(x$type,codes,range,source,x$comment) 
  paste0(details,collapse="; ")
}



x_table_2_table_row <- function(x,details_fun) {
  unit <- NULL
  if(.has("unit",x)) {
    unit <- glue::glue(" ({unit})", .envir = x)
  }
  short <- paste0(label.ycol(x,default = "short"),unit)
  ans <- tibble( 
    col = x$col, 
    label = short, 
    details = details_fun(x)
  )
  ans
}

x_table_2_df <- function(spec, 
                         row_fun = x_table_2_table_row, 
                         details_fun = x_table_2_details) {
  tab <- map_df(spec,row_fun,details_fun = details_fun)
  names(tab) <- c("Name", "Label", "Details") 
  tab
}

x_table_2 <- function(spec,
                      row_fun = x_table_2_table_row, 
                      details_fun = x_table_2_details) {
  
  tab <- x_table_2_df(spec, row_fun, details_fun)
  hlines <- which(tab[,1] != "")-1
  
  xt <- xtable(
    tab, 
    align = c("p{0cm}",
              "|p{0.8in}" , 
              ">{\\raggedright\\arraybackslash}p{2.39in}",
              ">{\\raggedright\\arraybackslash}p{2.78in}|")
  )
  add.to.row <- list(pos = list(0), command = NULL)
  command__ <- paste0(
    "\\hline\n\\endhead\n",
    "\\hline\n",
    "\\multicolumn{3}{l}",
    "{\\footnotesize Continued on next page}\n",
    "\\endfoot\n",
    "\\endlastfoot\n"
  )
  add.to.row$command <- command__
  pxt <- xtable::print.xtable(
    xt, 
    tabular.environment = "longtable", 
    latex.environments = NULL,
    include.rownames = FALSE,
    hline.after = c(-1,hlines),
    comment = FALSE,
    floating = FALSE,
    print.results = FALSE, 
    add.to.row = add.to.row,
    table.placement = "H",
    sanitize.text.function = getOption("ys.sanitize", ys_sanitize)
  )
  pxt <- glue::glue(pxt, .envir=get_meta(spec)$glue, .open = "<<", .close = ">>")
  return(pxt)  
}
