#' Make Caption
#'
#' @description make a single Tab2 caption
#'
#' @param single_result a single result from built_tab2(), ie. one of the list items that it returns
#' @param ytd a logical - should we should YTD (TRUE) or do we want the full FY  (FALSE)
#'
#' @return a column dataframe with type and val where type is the row name and val is the numbers displayed in the caption
#' @export
make_caption  <- function(single_result, ytd = F) {

  n_rows  <- nrow(single_result)


  this_yr  <- single_result[[n_rows, 2]]
  prev_yr      <- ifelse(ytd, single_result[[n_rows-1,3]], single_result[[n_rows -1 , 2]])
  pct_vs_prev  <- ifelse(ytd, single_result[[n_rows,7]], single_result[[n_rows  , 4]])
  three_yr        <- ifelse(ytd, single_result[[n_rows,8]], single_result[[n_rows, 5]])
  pct_vs_three_yr <- ifelse(ytd, single_result[[n_rows,9]], single_result[[n_rows, 6]])

  tribble(
      ~type, ~val
      , "this_yr ", this_yr
      , "pct_vs_prev ", pct_vs_prev - 1
      , "prev_yr", prev_yr
      , "pct_vs_three_yr", pct_vs_three_yr - 1
      , "three_yr", three_yr
  )

}


#' Pluck Caption
#'
#' @description Build all the captions so you can test Tab 2
#'
#' @param tab2_results a list object built from built_tab2()
#' @param ytd a logical - should we should YTD (TRUE) or do we want the full FY  (FALSE)
#'
#' @return a list of dataframes, each with two columns, type (the rownames) and val (the numbers that should be displayed)
#' @export
pluck_captions  <- function(tab2_results, ytd = T) {

  tab2_results |>
    map(make_caption, ytd)

}
