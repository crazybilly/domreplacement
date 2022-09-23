#' Get Data For Tab 2
#'
#' @param collect a logical - should the data be collected or should we return database queries
#' @param date_range_start a string (or Date, if you have to), preferably in YYYY-MM-DD format
#' @param date_range_end a string (or Date, if you have to), preferably in YYYY-MM-DD format
#' @param unit names of units to include
#' @param fundraiser_ids entity IDs of fundraisers to include
#' @param role roles to include
#' @param role_fy which fiscal year should we look at for roles
#' @param start_fy what fiscal year should the bar chart start at
#' @param db a valid database connection
#'
#' @return a list with 3 data frames, role_data, contact_data, and proposal_data
#' @export
#'
get_tab2_data  <- function(
      collect = F
    , date_range_start = '2014-07-01'
    , date_range_end = today()
    , unit = NA
    , fundraiser_ids = NA
    , role = NA
    , role_fy  = currentFY
    , start_fy = 2015
    , db = cdw
  ) {

  date_start_str <- date_to_str(date_range_start)
  date_end_str   <- date_to_str(date_range_end)

  date_start_fy = start_fy

  role_data  <- get_roles(role = role, role_fy = role_fy)


  contact_data  <- get_contact_reports(
      date_start = date_start_str
    , date_end = date_end_str
    , db = db
    ) |>
    left_join(
      role_data |>
        mutate(in_role =1)
      , by = c('CONTACT_CREDIT_ENTITY_ID' = 'FUNDRAISER_ENTITY_ID')
    ) |>
    left_join(
      role_data |>
        mutate(in_role =1)
      , by = c('AUTHOR_ENTITY_ID' = 'FUNDRAISER_ENTITY_ID')
    ) |>
    filter(
        in_role.x == 1
      | in_role.y == 1
    ) |>
    select(-in_role.x, -in_role.y)


  proposal_data  <- get_proposal_data(db = db) |>
    semi_join(role_data, by=c('ASSIGNMENT_ENTITY_ID'= 'FUNDRAISER_ENTITY_ID'))


  # filters for unit and fundraiser --------------------------------------------
  if(!is.na(unit)) {
    contact_data  <- contact_data |>
      filter(UNIT_DESC %in% local(unit))

    proposal_data  <- proposal_data |>
      filter(UNIT_CODE_DESC %in% local(unit))
  }

  if(!is.na(fundraiser_ids)) {
    contact_data  <- contact_data |>
      filter(
        CONTACT_CREDIT_ENTITY_ID %in% local(fundraiser_ids)
        | AUTHOR_ENTITY_ID         %in% local(fundraiser_ids)
      )

    proposal_data  <- proposal_data |>
      filter(ASSIGNMENT_ENTITY_ID %in% local(fundraiser_ids))
  }


  qual_data  <- get_qualifications_history() |>
    semi_join(proposal_data, by = 'PROPOSAL_ID')



  the_results  <- list(
      role_data          = role_data
    , contact_data       = contact_data
    , proposal_data      = proposal_data
    , qual_data          = qual_data
  )

  if(collect) {
    map(the_results, ~collect(.x))
  } else {
    return(the_results)
  }


}
