#' Get Qualification History
#'
#' @description Total qualifications are the first date where a proposal moves from QU in a positive direction. Positive stages for Total Qualifications include:
#' \itemize{
#'  \item{"CU"} {Cultivation}
#'  \item{"PD"} {Proposal Development}
#'  \item{"SP"} {Solicitation in Progress}
#'  \item{"GS"} {Gift Stewardship}
#'  \item{"DS"} {Donor Stewardship}
#'  \item{"RP"} {Referred to Other Program}
#'  \item{"DQ"} {Disqualified}
#'  \item{"TD"} {Turned Down}
#' }
#'
#' @param db a connection to CDW2_PROD. Note that this will fail if you don't use PROD, b/c qualification stage history depends on history tables (which are only available in PROD)
#'
#' @return an tbl_Oracle of all qualifications, with two columns: PROPOSAL_ID, QUALIFICATION_DATE
#' @export
#'
get_qualifications_history  <- function(db = cdw) {

  q_sql_path  <- system.file("sql", "qualifications.sql", package = 'domreplacement')
  dplyr::tbl(db, ucbcdw::getSQL(q_sql_path))

}


#' Get Qualififieds History
#'
#' @description Qualifiieds are the first date where a proposal moves from QU in a positive direction. Note that this is more restrictive than Total Qualifications. Positive stages for Qualifieds include:
#' \itemize{
#'  \item{"CU"} {Cultivation}
#'  \item{"PD"} {Proposal Development}
#'  \item{"SP"} {Solicitation in Progress}
#'  \item{"GS"} {Gift Stewardship}
#'  \item{"DS"} {Donor Stewardship}
#' }
#'
#' @param db a connection to CDW2_PROD. Note that this will fail if you don't use PROD, b/c qualification stage history depends on history tables (which are only available in PROD)
#'
#' @return an tbl_Oracle of all qualifications, with two columns: PROPOSAL_ID, QUALIFIED_DATE
#' @export
#'
get_qualifieds_history  <- function(db = cdw) {

  q_sql_path  <- system.file("sql", "qualifieds.sql", package = 'domreplacement')
  dplyr::tbl(db, ucbcdw::getSQL(q_sql_path))

}
#'
