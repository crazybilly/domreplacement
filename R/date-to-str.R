#' Convert a Date to a String
#'
#' @param x something that looks like a date (doesn't technically need to be an actual date)
#'
#' @return a string formatted in YYYY-MM-DD format
#'
date_to_str  <- function(x) {

  if(!inherits(x, 'Date')) {
    x  <- as_date(x)
  }

  format(x, '%Y-%m-%d')

}
