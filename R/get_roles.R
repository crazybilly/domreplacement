
#' Get Role Data
#'
#' @param role a vector of roles, ie. descriptions as shown in Tableau. Use NA to return all roles
#' @param role_fy a numeric, 4 digit fiscal year
#' @param db the database connections to use
#'
#' @return a database table (not collected) with one column: FUNDRAISER_ENTITY_ID
#' @export
#'
get_roles  <- function(role = 'Major Gift', role_fy = 2021, db = cdw) {

  the_roles  <- tbl(db, in_schema2("CDW", "F_CRM_GOAL")) |>
    filter(
      FISCAL_YEAR == role_fy
    )

  if(!is.na(role)) {
    the_roles |>
      filter(FUNDRAISER_ROLE %in% role)
  }

  the_roles |>
    select(FUNDRAISER_ENTITY_ID = ENTITY_ID)


}


