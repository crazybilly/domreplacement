#' Pluck Tab2 Bar Numbers
#'
#' @description a simplified data frame to test all the bar numbers on Tab 2 against
#'
#' @param tab2_results the results of build_tab2()
#'
#' @return a data frame with one line per section on Tab2 and one column per fiscal year
#' @export
#'
pluck_tab2_bar_numbers  <- function(tab2_results) {

  tab2_results |>
    imap_dfr( ~ .x |>
      select(
          FISCAL_YR
        , val = 2
      ) |>
      mutate(key = .y) |>
      spread(FISCAL_YR, val)
    )

}


