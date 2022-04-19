

# internal functions ------------------------------------------------------



#' Count Contacts
#'
#' @description given a local contact report dataframe (built from get_contact_reports() ), count contacts. To count visits, filter down to visits before using this function.
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



#' Count Households Contacted (ie. Unique Visits)
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


# Contact Report Trends ---------------------------------------------------


# filter the contact data by unit/fundraiser first
# filter the role data by role first
#' Build Contact Trends
#'
#' @description build a data frame to test USM/CSM/DOM Tab #2 against.
#'
#' @param contact_data a data frame of contact reports, built by get_contact_reports(). Filter by date, fundraiser and unit, if desired, BEFORE using this function.
#' @param role_data a data frame of fundraisers in the desired role, built by get_roles(). Roles should be filtered before using this function.
#' @param type a character strings, the type of visits. 'visits', 'unique households' or 'significant contacts'
#'
#' @return a dataframe to test against
#' @export
#'
build_contact_trends  <- function(contact_data, role_data, type = 'visits') {


  start_fy = currentFY - 8
  todays_num = fydaynum(today())


  the_contacts  <- contact_data |>
    filter(
        AUTHOR_ENTITY_ID %in% local(role_data$FUNDRAISER_ENTITY_ID)
      | CONTACT_CREDIT_ENTITY_ID %in% local(role_data$FUNDRAISER_ENTITY_ID)
    ) |>
    filter(
      FISCAL_YR >= start_fy
    ) |>
    mutate(
      isytd  = fydaynum(CONTACT_DATE) <= todays_num
    )



  if(str_detect(str_to_lower(type), 'sig')) {

      the_contacts |>
        filter(CONTACT_ATTITUDE_CODE == 'Y') |>
        count_contacts()

  } else if (str_detect(str_to_lower(type), 'uniq')) {

      the_contacts |>
        count_contacted_households()


  } else if (str_detect(str_to_lower(type), 'visit')) {

      the_contacts |>
        filter(CONTACT_TYPE %in% c('V', 'VIR')) |>
        count_contacts()

  } else {
    stop("type should be 'significant contacts', 'unique households' or 'visits'")
  }


}

# Asks and Commits --------------------------------------------------------

# filter date range before
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
    )


  summarize_metric  <- function(df, val) {

    if(value == '#') {
      df |>
        summarize(
            total_all = n_distinct(PROPOSAL_ID)
          , ytd_total = n_distinct(case_when(isytd ~ PROPOSAL_ID, T ~ na_dbl))
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




# qualifications ----------------------------------------------------------




