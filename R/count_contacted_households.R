#' Count Households Contacted (ie. Unique Visits)
#'
#' @description for use in Tab2 build_contact_trends(). Not exported.
#'
#' @param contact_data a data frame of contact data built by get_contact_reports()
#'
#' @return all the data needed to verify the numbers on the USM/CSM/DOM Replacement Tab #2
count_contacted_households  <- function(contact_data) {

  hh_contacts  <- contact_data |>
    select(
      FISCAL_YR
      , HH_CORP_ENTITY_ID
      , ALT_HH_CORP_ENTITY_ID
      , CONTACT_DATE
    ) |>
    gather(id_type, id, -FISCAL_YR, -isytd) |>
    filter(!is.na(id))
  distinct(FISCAL_YR, CONTACT_DATE, isytd)


  hh_contacts |>
    group_by(FISCAL_YR) |>
    summarize(
      n_households_all = n_distinct(id)
      , ytd_households   = n_distinct(case_when(isytd ~ id , T ~ na_dbl))
    ) |>
    arrange(FISCAL_YR) |>
    mutate(
      vs_last_yr      = n_households_all/lag(n_households_all, 1) |> percent()
      , three_yr_avg    = (slide_dbl(n_households_all, ~ sum(.x), .before = 3) - n_households_all)/3
      , vs_three_yr_avg = n_households_all/three_yr_avg |> percent()
      , ytd_vs_last_yr      = ytd_households/lag(ytd_households, 1) |> percent()
      , ytd_three_yr_avg    = (slide_dbl(ytd_households, ~ sum(.x), .before = 3) - ytd_households)/3
      , ytd_vs_three_yr_avg = ytd_households/ytd_three_yr_avg |> percent()
    )

}

