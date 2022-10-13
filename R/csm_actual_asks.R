#' CSM Actual Asks
#'
#' @param portfolio_q  a portfolio query built by csm_build_portfolio_query()
#'
#' @return a query string that can be used to pull the proposals that make up Actual Asks on the CSM
#' @export
#'
csm_get_fundraiser_actual_asks  <- function(portfolio_q) {

  str_c(
    "with portfolio as ("
    , portfolio_q
    , ")"
    , "
        select * from (
           SELECT DISTINCT
               ASSIGNMENT_ENTITY_ID,
               ASSIGNMENT_NAME,
               OFFICE_CODE,
               PROPOSAL_ID,
               NVL( SUM( CASE WHEN  INITIAL_CONTRIBUTION_DT IS NOT NULL AND ASK_AMT > 0  AND STOP_DT IS NULL THEN ASK_AMT END ),0) AS ACTUAL_ASK,
               NVL( COUNT(CASE WHEN  INITIAL_CONTRIBUTION_DT IS NOT NULL AND ASK_AMT > 0  AND  STOP_DT IS NULL THEN PROPOSAL_ID END),0) AS ACTUAL_ASK_CNT,
               rn3,


           FROM PORTFOLIO
           WHERE
              PRIMARY_IND = 'Y'
           GROUP BY
              ASSIGNMENT_ENTITY_ID,
              OFFICE_CODE,
              PROPOSAL_ID,
              ASSIGNMENT_NAME,
              rn3,
              RN4
        )
        where actual_ask_cnt = 1

    ")
}


