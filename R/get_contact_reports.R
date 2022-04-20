#' Get Contacts Reports
#'
#' @param date_start the cutoff date, a string in YYYY-MM-DD format
#' @param date_end the cutoff date, a string in YYYY-MM-DD format
#' @param by_fundraiser_id a vector of fundraiser entity IDs. Use NA to pull all
#' @param db the database connection to use
#'
#' @return a database table of contact reports
#' @export
#'
get_contact_reports  <- function(date_start, date_end, by_fundraiser_id = NA, db = cdw) {

  d_entity_mv <- tbl(db, in_schema2("CDW", "d_entity_mv"))


  the_contacts  <- tbl(db, in_schema2("CDW", "f_contact_reports_mv")) |>
    filter(
      CONTACT_DATE >= to_date(date_start, 'YYYY-MM-DD')
      , CONTACT_DATE <= to_date(date_end  , 'YYYY-MM-DD')
    ) |>
    left_join(
      tbl(db, in_schema2("CDW", "d_date_mv")) |>
        select(CONTACT_DATE = CALENDAR_DT, FISCAL_YR)
      , by = 'CONTACT_DATE'
    ) |>
    left_join(
      d_entity_mv |>
        select(ENTITY_ID, HH_CORP_ENTITY_ID)
      , by = c('CONTACT_ENTITY_ID' = 'ENTITY_ID')
    ) |>
    left_join(
      d_entity_mv |>
        select(ENTITY_ID, ALT_HH_CORP_ENTITY_ID =  HH_CORP_ENTITY_ID)
      , by = c('CONTACT_ALT_ENTITY_ID' = 'ENTITY_ID')
    ) |>
    left_join(
      d_entity_mv |>
        select(ENTITY_ID, FUNDRAISER_NAME = REPORT_NAME)
      , by = c('CONTACT_CREDIT_ENTITY_ID' = 'ENTITY_ID')
    ) |>
    left_join(
      d_entity_mv |>
        select(ENTITY_ID, FUNDRAISER_NAME = REPORT_NAME)
      , by = c('AUTHOR_ENTITY_ID' = 'ENTITY_ID')
    ) |>
    mutate(FUNDRAISER_NAME = coalesce(FUNDRAISER_NAME.x, FUNDRAISER_NAME.y)) |>
    select(-FUNDRAISER_NAME.x, -FUNDRAISER_NAME.y)


  if(!is.na(by_fundraiser_id)) {
    the_contacts %<>%
      filter(
        AUTHOR_ENTITY_ID         %in% by_fundraiser_id
        | CONTACT_CREDIT_ENTITY_ID %in% by_fundraiser_id
      )
  }

  return(the_contacts)

}
