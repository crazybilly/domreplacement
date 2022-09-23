#' Build Ask and Commit Trend Data
#'
#' @description build a data frame to test USM/CSM/DOM Tab #2 against.
#'
#' @param proposal_data a data frame of proposal data, built by get_proposal_data(). Filter by date, fundraiser and unit, if desired, BEFORE using this function.
#' @param metric a character strings, the type of thing to count, should be 'asks' or 'commits'
#' @param value a character to determine if we're counting proposals or totalling the value of those proposals. Should be '#' or '$'
#' @param first_fy the first fiscal year to look at. Helps to filter data down, just in case you didn't already filter by date.
#'
#' @return a dataframe to test against
#'
#' @export
#'
build_ask_commit_trends  <- function(proposal_data, metric = 'asks', value = '#', first_fy = 2015) {

  if(!str_detect(str_to_lower(metric), "ask|commit")) {
    stop("Metric should 'asks' or 'commits'")
  }

  if(!str_detect(str_to_lower(value), "#|\\$")) {
    stop("Value should '#' or '$'")
  }

  todays_num = fydaynum(today())

  the_data  <- proposal_data |>
    mutate(
        the_date = case_when(
            str_detect(str_to_lower(metric), 'ask')    ~ INITIAL_CONTRIBUTION_DT
          , str_detect(str_to_lower(metric), 'commit') ~ PROPOSAL_STOP_DT
        )
      , the_amt  = case_when(
            str_detect(str_to_lower(metric), 'ask')    ~ ASK_AMT
          , str_detect(str_to_lower(metric), 'commit') ~ GRANTED_AMT
        )
      , FISCAL_YR = fy(the_date)
      , isytd = fydaynum(the_date) <= todays_num
    ) |>
    filter(
        FISCAL_YR >= first_fy
      , FISCAL_YR <= currentFY
      , the_amt > 0
    )


  summarize_metric  <- function(df, val) {

    if(val == '#') {
      df |>
        summarize(
            total_all = n_distinct(PROPOSAL_ID, na.rm =T)
          , ytd_total = n_distinct(case_when(isytd ~ PROPOSAL_ID, T ~ na_dbl), na.rm = T)
        )
    } else {
      df |>
        group_by(PROPOSAL_ID, .add = T) |>
        summarize(
            the_amt     = max(the_amt, na.rm = T)
          , the_ytd_amt = max(case_when(isytd ~ the_amt, T ~ 0 ), na.rm = T)
        ) |>
        summarize(
            total_all = sum(the_amt, na.rm = T)
          , ytd_total = sum(the_ytd_amt, na.rm = T)
        )

    }

  }


  the_data |>
    group_by(FISCAL_YR) |>
    summarize_metric(val = value) |>
    ungroup() |>
    arrange(FISCAL_YR) |>
    mutate(
        vs_last_yr      =  total_all/lag(total_all, 1) |> percent()
      , three_yr_avg    = (slide_dbl(total_all, ~ sum(.x), .before = 3) - total_all)/3
      , vs_three_yr_avg =  total_all/three_yr_avg |> percent()
      , ytd_vs_last_yr      =  ytd_total/lag(ytd_total, 1) |> percent()
      , ytd_three_yr_avg    = (slide_dbl(ytd_total, ~ sum(.x), .before = 3) - ytd_total)/3
      , ytd_vs_three_yr_avg =  ytd_total/ytd_three_yr_avg |> percent()
    )

}

