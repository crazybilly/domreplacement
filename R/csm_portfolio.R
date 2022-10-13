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

