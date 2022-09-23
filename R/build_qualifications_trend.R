#' Build Qualifications Trend Date
#'
#' @description build a data frame to test USM/CSM/DOM Tab #2 against
#'
#' @param qual_data a data frame of qualification data, build by get_qualification_history(). Filter to get the relevant proposals BEFORE using this functino.
#' @param first_fy the first fiscal year that should appear on the bar chart
#'
#' @return a dataframe to test against
#' @export
#'
build_qualifications_trend  <- function(qual_data, first_fy = 2015) {


  todays_num  <- fydaynum(today())

  qual_data |>
    mutate(
        FISCAL_YR = fy(QUALIFICATION_DATE)
      , is_ytd = fydaynum(QUALIFICATION_DATE) <= todays_num
    ) |>
    filter(
        FISCAL_YR >= first_fy
      , FISCAL_YR <= currentFY
    ) |>
    group_by(FISCAL_YR) |>
    summarize(
        n_quals = n_distinct(PROPOSAL_ID, na.rm = T)
      , n_ytd = n_distinct(ifelse(is_ytd, PROPOSAL_ID, na_dbl), na.rm = T)
    ) |>
    arrange(FISCAL_YR) |>
    mutate(
        vs_last_yr      =  n_quals/lag(n_quals, 1) |> percent()
      , three_yr_avg    = (slide_dbl(n_quals, ~ sum(.x), .before = 3) - n_quals)/3
      , vs_three_yr_avg =  n_quals/three_yr_avg |> percent()
      , ytd_vs_last_yr      =  n_ytd/lag(n_ytd, 1) |> percent()
      , ytd_three_yr_avg    = (slide_dbl(n_ytd, ~ sum(.x), .before = 3) - n_ytd)/3
      , ytd_vs_three_yr_avg =  n_ytd/ytd_three_yr_avg |> percent()
    )

}
