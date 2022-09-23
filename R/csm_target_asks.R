


#' Build a SQL Query like the CSM for the base portfolio
#'
#' @param office an office code (from assignments)
#' @param fundraiser_id fundraiser ID (numeric)
#' @param role role (from goals)
#' @param role_fy 4 digit fiscal year for roles
#' @param plan_type plan type. Leave blank for all
#'
#' @return a sql query (ie. text)
#' @export
#'
csm_build_portfolio_query  <- function(office, fundraiser_id, role , role_fy = 2022, plan_type ) {

  if(!missing(role)) {
     role  <- str_c(role, collapse = ',')
     role_str  <- glue("and GL.FUNDRAISER_ROLE in ('{role}')")
  } else {
    role_str  <- ""
  }

  if(!missing(plan_type)) {
     plan_type <- str_c(plan_type, collapse = ',')
     plan_str  <- glue("and PR.PROPOSAL_TYPE in ('{plan_type}')")
  } else {
    plan_str  <- ""
  }

  if(!missing(fundraiser_id)) {
    fundraiser_id <- str_c(fundraiser_id, collapse = ',')
    fundraiser_str  <- glue("and ASN.ASSIGNMENT_ENTITY_ID in ({fundraiser_id})")
  } else {
    fundraiser_str  <- ""
  }

  if(!missing(office)) {
    office      <- str_c(office, collapse = ',')
    office_str  <- glue("and ASN.OFFICE_CODE in ('{office}')")
  } else {
    office_str  <- ""
  }

  portfolio_q  <- glue::glue("

   SELECT DISTINCT

   ASSIGNMENT_ENTITY_ID,
   ASSIGNMENT_LAST_NAME || ', ' || ASSIGNMENT_FIRST_NAME ASSIGNMENT_NAME,
   OFFICE_CODE,
   OFFICE_DESC ,
   PR.PROPOSAL_ID,
   PR.ENTITY_ID,
   PR.PROSPECT_ID ,
   PR.PRIMARY_IND,
   PR.STOP_DT,
   PR.STAGE_CODE,
   PR.INITIAL_CONTRIBUTION_DT,
   PR.EXPECTED_DATE,
   PR.ASK_AMT,
   PR.ORIGINAL_ASK_AMT,
 --  ROW_NUMBER() OVER( PARTITION BY PR.ENTITY_ID, OFFICE_CODE, ASSIGNMENT_ENTITY_ID ORDER BY NULL) RN ,
   ROW_NUMBER() OVER( PARTITION BY PR.PROSPECT_ID, OFFICE_CODE ORDER BY NULL) RN2,
   ROW_NUMBER() OVER( PARTITION BY PR.PROPOSAL_ID, OFFICE_CODE ORDER BY NULL) RN3,
   ROW_NUMBER() OVER( PARTITION BY PR.PROPOSAL_ID, office_code, assignment_entity_id ORDER BY office_code, assignment_entity_id) rn4

      FROM CDW.F_PROPOSAL_MV PR
      INNER JOIN CDW.F_ASSIGNMENT_MV ASN ON ASN.PROPOSAL_ID = PR.PROPOSAL_ID
      INNER JOIN CDW.D_BIO_ENTITY_RECORD_TYPE_MV ENT ON ENT.ENTITY_ID = ASN.ASSIGNMENT_ENTITY_ID --AND RECORD_TYPE_CODE = 'SF'

      LEFT JOIN
      (
         SELECT DISTINCT ENTITY_ID,
         FUNDRAISER_ROLE,
         FISCAL_YEAR
      FROM
      (
        SELECT DISTINCT ENTITY_ID,
        FISCAL_YEAR,
        FUNDRAISER_ROLE,
        ROW_NUMBER() OVER (PARTITION BY ENTITY_ID ORDER BY ACTIVE DESC, CREATED_DATE DESC) GOAL_ROW
        FROM CDW.F_CRM_GOAL
        WHERE FISCAL_YEAR = {role_fy}
        )
        WHERE GOAL_ROW = 1
      )

       GL ON GL.ENTITY_ID = ASN.ASSIGNMENT_ENTITY_ID

      WHERE

          PR.ACTIVE_IND= 'Y'
      AND ASN.ACTIVE_IND = 'Y'
      AND ASN.ASSIGNMENT_TYPE = 'DO'
      AND PR.PRIMARY_IND = 'Y'
  ")

  str_c(
      portfolio_q
    , role_str
    , plan_str
    , fundraiser_str
    , office_str
    , collapse = '\n'
  )



}




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

