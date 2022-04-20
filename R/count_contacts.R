#' Count Contacts
#'
#' @description primarily for use in Tab 2 build_* functions. Given a local contact report dataframe (built from get_contact_reports() ), count contacts. To count visits, filter down to visits before using this function.
#'
#' @param contact_data a data frame built by get_contact_reports()
#'
#' @return all the data needed to verify the numbers on the USM/CSM/DOM Replacement Tab #2
count_contacts  <- function(contact_data) {

  contact_data |>
    group_by(FISCAL_YR) |>
    summarize(
      n_contacts_all = n_distinct(REPORT_ID)
      , ytd_contacts = n_distinct(case_when(isytd ~ REPORT_ID, T ~ na_dbl))
    ) |>
    arrange(FISCAL_YR) |>
    mutate(
      vs_last_yr      = n_contacts_all/lag(n_contacts_all, 1) |> percent()
      , three_yr_avg    = (slide_dbl(n_contacts_all, ~ sum(.x), .before = 3) - n_contacts_all)/3
      , vs_three_yr_avg = n_contacts_all/three_yr_avg |> percent()
      , ytd_vs_last_yr      = ytd_contacts/lag(ytd_contacts, 1) |> percent()
      , ytd_three_yr_avg    = (slide_dbl(ytd_contacts, ~ sum(.x), .before = 3) - ytd_contacts)/3
      , ytd_vs_three_yr_avg = ytd_contacts/ytd_three_yr_avg |> percent()
    )
}
