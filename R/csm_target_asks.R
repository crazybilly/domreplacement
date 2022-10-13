


#' Get a CSM-like fundraiser total
#'
#' @param portfolio_q a query built from csm_build_portfolio_query()
#'
#' @return the SQL query to build a CSM-like data frame
#' @export
csm_get_fundraiser_target_asks  <- function(portfolio_q) {

 str_c(
   "with portfolio as ("
   , portfolio_q
   , ")"
   , "
      SELECT DISTINCT
      ASSIGNMENT_ENTITY_ID,
      ASSIGNMENT_NAME,
      OFFICE_CODE,
      SUM( TARGET_ASK)   TOTAL_TARGET_ASK,
      COUNT(DISTINCT CASE WHEN  TARGET_ASK_WITHOUT_ACTUAL_ASK  = 1 THEN PROPOSAL_ID END)  TOTAL_TARGET_ASK_CNT,
      COUNT(DISTINCT CASE WHEN RN3 = 1 AND TARGET_ASK_WITHOUT_ACTUAL_ASK = 1 THEN PROPOSAL_ID END) AS UNIT_TARGET_ASK_CNT,
      SUM(CASE WHEN RN3 = 1 AND TARGET_ASK_WITHOUT_ACTUAL_ASK = 1 THEN TARGET_ASK END) AS UNIT_TARGET_ASK

      FROM
      (
       SELECT DISTINCT ASSIGNMENT_ENTITY_ID,
       ASSIGNMENT_NAME,
       OFFICE_CODE,
       PROPOSAL_ID,
       NVL( SUM( CASE
          WHEN
                EXPECTED_DATE IS NOT NULL
            AND ASK_AMT = 0
            AND ORIGINAL_ASK_AMT > 0
            AND STOP_DT IS NULL
          THEN ORIGINAL_ASK_AMT
          END
        ),0) AS TARGET_ASK,

      NVL( COUNT(CASE
          WHEN
              EXPECTED_DATE IS NOT NULL
            AND ASK_AMT = 0
            AND ORIGINAL_ASK_AMT > 0
            AND  STOP_DT IS NULL
          THEN PROPOSAL_ID
          END
        ),0) AS TARGET_ASK_WITHOUT_ACTUAL_ASK,
      rn3

       FROM PORTFOLIO
      WHERE PRIMARY_IND = 'Y'
       GROUP BY ASSIGNMENT_ENTITY_ID,
      OFFICE_CODE,
      PROPOSAL_ID,
      ASSIGNMENT_NAME,
      rn3
      )
        GROUP BY ASSIGNMENT_ENTITY_ID,
      ASSIGNMENT_NAME,
       OFFICE_CODE
   "
  )


}


#' Filter a Portfolio down the the Target Asks, ie. Planned Asks
#'
#' @param portfolio_local a collected, in local memory, data frame built from csm_build_portfolio_query()
#'
#' @return a list of proposal assignments that have target asks
#' @export
csm_filter_to_target_asks  <- function(portfolio_local) {

  portfolio_local |>
    mutate(
        target_ask = case_when(
          !is.null(EXPECTED_DATE) & ASK_AMT == 0 & ORIGINAL_ASK_AMT > 0  & is.na(STOP_DT)  ~ ORIGINAL_ASK_AMT # ~ ORIGINAL ASK_AMT
        , 0 == 0 ~ 0
      )



      , target_ask_without_actual = case_when(
          !is.null(EXPECTED_DATE) & ASK_AMT == 0 & ORIGINAL_ASK_AMT > 0 & is.na(STOP_DT)  ~ PROPOSAL_ID
        , 0 == 0 ~ na_dbl
      )
    ) |>
    filter(target_ask >0)
}





#' Get Unit Totals for Target Asks
#'
#' @param unit_code a unit code
#' @param unit_desc a unit description
#'
#' @return a one-line data frame with the unit and the unit's total of target asks
#' @export
#'
csm_get_unit_target_ask_totals  <- function(unit_code, unit_desc) {

  tbl(cdw, sql(build_csm_target_asks(build_csm_portfolio_query(unit_code)))) |>
    collect() |>
    colSums2() |>
    select(UNIT_TARGET_ASK) |>
    mutate(
      unit = unit_desc
    ) |>
    select(
      unit
      , UNIT_TARGET_ASK
    )

}

