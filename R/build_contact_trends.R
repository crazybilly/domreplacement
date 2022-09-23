#' Build Contact Trends
#'
#' @description build a data frame to test USM/CSM/DOM Tab #2 against.
#'
#' @param contact_data a data frame of contact reports, built by get_contact_reports(). Filter by date, fundraiser and unit, if desired, BEFORE using this function.
#' @param role_data a data frame of fundraisers in the desired role, built by get_roles(). Roles should be filtered before using this function.
#' @param type a character strings, the type of visits. 'visits', 'unique households' or 'significant contacts'
#' @param first_fy what fiscal year should the bar charts start at
#'
#' @return a dataframe to test against
#' @export
#'
build_contact_trends  <- function(contact_data, role_data, type = 'visits', first_fy = 2015) {


  start_fy = first_fy
  todays_num = fydaynum(today())


  the_contacts  <- contact_data |>
    filter(
        AUTHOR_ENTITY_ID         %in% local(role_data$FUNDRAISER_ENTITY_ID)
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
      filter(CONTACT_TYPE %in% c('V', 'VIR')) |>
      count_contacted_households()


  } else if (str_detect(str_to_lower(type), 'visit')) {

    the_contacts |>
      filter(CONTACT_TYPE %in% c('V', 'VIR')) |>
      count_contacts()

  } else {
    stop("type should be 'significant contacts', 'unique households' or 'visits'")
  }


}
