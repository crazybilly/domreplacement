#' DOM qualifications
#'
#' @description Is this really the right function to use? Isn't the detail avaiable on the DOM itself?
#'
#' @param FR_ID a fundraiser's entity_id
#' @param Unit a unit code
#' @param Plan_Type plan types to include, leave '0' for all plan types
#' @param BeginDate a date, as a string in YYYY-MM-DD format. Qualification date should between BeginDate and EndDat
#' @param EndDate  a date, as a string in YYYY-MM-DD format,
#'
#' @return a string that can be used as an SQL query
#' @export
#'
dom_qual_portfolio  <- function(FR_ID, Unit, Plan_Type = '0', BeginDate = '2021-07-01', EndDate = '2022-06-30') {
  BeginDate  <-  glue("to_date('{BeginDate}', 'YYYY-MM-DD')")
  EndDate    <-  glue("to_date('{EndDate}'  , 'YYYY-MM-DD')")

  glue::glue("

        select f_proposal_mv.proposal_id,
                ASSIGNED_TO_ENTITY_ID_NBR,
                max(f_proposal_mv.prospect_id) as prospect_id,
                max(stage_code) as stage,
                max(stage_desc) as stage_desc,
                max(stage_rank_code) as stage_rank,
                max(start_dt) as proposal_begin_dt,
                max(proposal_title) as proposal_title,
                max(original_ask_amt) as target_ask,
                max(expected_date) as target_date,
                max(ask_amt) as actual_ask,
                max(initial_contribution_dt) as actual_date,
                max(granted_amt) as commit_amt,
                max(stop_dt) as commit_dt,
                max(trunc(sysdate) - proposal_stage_begin_dt) as days_in_stage,
                min(assignment_begin_dt) as assign_begin_dt,
                max(nvl(assignment_end_dt,sysdate)) as assign_end_dt
          from cdw.cads_assignment_f_v,
               cdw.f_proposal_mv
          where cads_assignment_f_v.proposal_id = f_proposal_mv.proposal_id

          and f_proposal_mv.start_dt <= {EndDate}
          and nvl(f_proposal_mv.stop_dt,sysdate) >= {BeginDate}
          and assignment_begin_dt <= {EndDate}
          and nvl(assignment_end_dt,sysdate) >= {BeginDate}
          and office_assigned_to_cd = '{Unit}'
         -- and (0 IN( {FR_ID}) or assigned_to_entity_id_nbr = ANY ({FR_ID}))
          and ( '0' in ('{FR_ID}') or ASSIGNED_TO_ENTITY_ID_NBR = any({FR_ID}))
         and ( '0' IN({Plan_Type}) or proposal_type = ANY ({Plan_Type}))
          group by f_proposal_mv.proposal_id, assigned_to_entity_id_nbr
  ")


}




#' Build a query for Total Qualifications as calculated in the DOM
#'
#' @param FR_ID a fundraiser's entity_id
#' @param Unit a unit code
#' @param Plan_Type plan types to include, leave '0' for all plan types
#' @param BeginDate a date, as a string in YYYY-MM-DD format. Qualification date should between BeginDate and EndDat
#' @param EndDate  a date, as a string in YYYY-MM-DD format
#'
#' @return a string that can be used as an SQL query
#' @export
#'
dom_total_qualifications  <- function(FR_ID, Unit, Plan_Type = '0', BeginDate = '2021-07-01', EndDate = '2022-06-30') {

  cte_portfolio <- dom_qual_portfolio(FR_ID = FR_ID, Unit = Unit, Plan_Type = Plan_Type, BeginDate = BeginDate, EndDate = EndDate)


  BeginDate  <-  glue("to_date('{BeginDate}', 'YYYY-MM-DD')")
  EndDate    <-  glue("to_date('{EndDate}'  , 'YYYY-MM-DD')")




  str_c(
    "with portfolio_proposals as ("
    , cte_portfolio
    , ")"
    , glue::glue("

        SELECT PROPOSAL_ID,
        PROSPECT_ID,
        PROSPECT_NAME,
        ENTITY_ID,
        REPLACE(PROPOSAL_ASSIGNMENTS, ') DO', ')') PROPOSAL_ASSIGNMENTS,
        STAGE_DESC,
        STAGE_CODE,
        CAPACITY_RATING_DESC,
        CASE WHEN PRIMARY_MANAGER_LAST_NM IS NOT NULL THEN PRIMARY_MANAGER_LAST_NM || ', ' || PRIMARY_MANAGER_FIRST_NM END PRIMARY_MANAGER_LAST_NM,
        MAX(DT_SORT) AS DT_SORT,
        DT_DISP,
        SUM(QUALS) AS QUALS
        FROM
        (

          SELECT DISTINCT PS.PROPOSAL_ID, PR.PROSPECT_ID, PROS.PROSPECT_NAME, PSM.PROPOSAL_ASSIGNMENTS, PR.STAGE_DESC, PR.STAGE_CODE, PSUM.CAPACITY_RATING_DESC, PRIMARY_MANAGER_LAST_NM, PRIMARY_MANAGER_FIRST_NM,
          MAX(CASE WHEN PROS.PROSPECT_ENTITY_PRIMARY_IND = 'Y' THEN PROS.ENTITY_ID END) ENTITY_ID,
          PS.STAGE_DATE,
          TO_CHAR(STAGE_DATE, 'yyyy-mm') AS DT_SORT,
          TO_CHAR(STAGE_DATE, 'FMMonth yyyy') AS DT_DISP,
          MAX(CASE WHEN PS.STAGE_CODE IN ('PD','SP','GS','DS','CU','RP','DQ','TD') THEN 1  END) AS QUALS,
          ROW_NUMBER() OVER (PARTITION BY PS.PROPOSAL_ID ORDER BY PS.PROPOSAL_STAGE_ID  DESC) RNK

        FROM PORTFOLIO_PROPOSALS
        INNER JOIN CDW.F_ASSIGNMENT_MV ASN ON ASN.PROPOSAL_ID = PORTFOLIO_PROPOSALS.PROPOSAL_ID
        INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
        INNER JOIN CDW.F_PROPOSAL_MV PR ON PR.PROPOSAL_ID = PS.PROPOSAL_ID
        INNER JOIN CDW.D_PROSPECT_MV PROS ON PROS.PROSPECT_ID = ASN.PROSPECT_ID
        INNER JOIN CDW.F_PROPOSAL_SUMMARY_MV PSM ON PSM.PROPOSAL_ID = ASN.PROPOSAL_ID
        INNER JOIN CDW.SF_ENTITY_BASED_PRSPCT_SMRY_MV PSUM ON PSUM.ENTITY_ID = PR.ENTITY_ID AND PR.PRIMARY_IND = 'Y'

        INNER JOIN
            (
            SELECT DISTINCT ASN.PROPOSAL_ID
            FROM   CDW.F_ASSIGNMENT_MV ASN
            INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
            INNER JOIN CDW.F_PROPOSAL_MV PR ON PR.PROPOSAL_ID = ASN.PROPOSAL_ID
                INNER JOIN(
                 SELECT  DISTINCT PS.PROPOSAL_ID, STAGE_DATE
                 FROM CDW.F_PROPOSAL_STAGE_MV PS
                 WHERE STAGE_CODE = 'QU' AND STAGE_DATE <= {EndDate}
                 )INR ON INR.PROPOSAL_ID = PS.PROPOSAL_ID
            WHERE PS.STAGE_CODE IN ('PD','SP','GS','DS','CU','TD','RP','DQ') AND PS.STAGE_DATE BETWEEN {BeginDate} and {EndDate}
            AND ASN.START_DATE <= PS.STAGE_DATE AND (ASN.STOP_DATE IS NULL OR ASN.STOP_DATE >= PS.STAGE_DATE)
            AND PR.PRIMARY_IND = 'Y'
            AND  OFFICE_CODE = '{Unit}'
            AND PS.STAGE_DATE > INR.STAGE_DATE
           --AND (0 IN {FR_ID} OR ASSIGNMENT_ENTITY_ID IN {FR_ID}  )
           and ( '0' in ('{FR_ID}') or ASSIGNMENT_ENTITY_ID = any({FR_ID}))
            MINUS

            SELECT DISTINCT ASN.PROPOSAL_ID
            FROM PORTFOLIO_PROPOSALS
            INNER JOIN CDW.F_ASSIGNMENT_MV ASN ON ASN.ASSIGNMENT_ENTITY_ID = PORTFOLIO_PROPOSALS.ASSIGNED_TO_ENTITY_ID_NBR
            INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
            INNER JOIN CDW.F_PROPOSAL_MV PR ON PR.PROPOSAL_ID = ASN.PROPOSAL_ID
            INNER JOIN
               (
               SELECT  DISTINCT PS.PROPOSAL_ID, STAGE_DATE
               FROM CDW.F_PROPOSAL_STAGE_MV PS
               WHERE STAGE_CODE = 'QU' AND STAGE_DATE <= {EndDate}
               )INR ON INR.PROPOSAL_ID = PS.PROPOSAL_ID
            WHERE
            PS.STAGE_CODE IN ('PD','SP','GS','DS','CU','CN','TD','RP','DQ') AND (PS.STAGE_DATE  <  {BeginDate}
            OR (ASN.START_DATE > PS.STAGE_DATE  OR ASN.STOP_DATE < PS.STAGE_DATE))
            --AND PR.PRIMARY_IND = 'Y'
            OR PR.STOP_DT < {BeginDate}
            OR(PR.STAGE_CODE = 'QU' AND PR.ACTIVE_IND <> 'Y' AND (PR.STOP_DT < {BeginDate} OR (PR.STOP_DT IS NULL AND ADD_MONTHS(PR.EXPECTED_DATE,6) < {BeginDate})))
           -- AND PS.STAGE_DATE > INR.STAGE_DATE
            )INR ON INR.PROPOSAL_ID = PS.PROPOSAL_ID
            WHERE  PS.STAGE_DATE BETWEEN {BeginDate} and {EndDate}
            AND PS.STAGE_CODE IN ('PD','SP','GS','DS','CU','RP','DQ','TD')


        GROUP BY
        PS.PROPOSAL_ID,
        STAGE_DATE,
        PS.PROPOSAL_STAGE_ID,
        PR.PROSPECT_ID,
        PROS.PROSPECT_NAME,
        PSM.PROPOSAL_ASSIGNMENTS,
        PR.STAGE_DESC,
        PR.STAGE_CODE,
        PSUM.CAPACITY_RATING_DESC,
        PRIMARY_MANAGER_LAST_NM,
        PRIMARY_MANAGER_FIRST_NM
        )
        WHERE RNK = 1
        GROUP BY DT_DISP,
        PROPOSAL_ID,
        PROSPECT_ID,
        PROSPECT_NAME,
        ENTITY_ID,
        PROPOSAL_ASSIGNMENTS,
        STAGE_DESC,
        STAGE_CODE,
        CAPACITY_RATING_DESC,
        CASE WHEN PRIMARY_MANAGER_LAST_NM IS NOT NULL THEN PRIMARY_MANAGER_LAST_NM || ', ' || PRIMARY_MANAGER_FIRST_NM END

    ")
  , sep = '\n'
  )


}


#
# dom_total_qualification_minus  <-  function(FR_ID, Unit, Plan_Type = '0', BeginDate = '2021-07-01', EndDate = '2022-06-30') {
#
#   cte_portfolio <- dom_qual_portfolio(FR_ID = FR_ID, Unit = Unit, Plan_Type = Plan_Type, BeginDate = BeginDate, EndDate = EndDate)
#
#
#   BeginDate  <-  glue("to_date('{BeginDate}', 'YYYY-MM-DD')")
#   EndDate    <-  glue("to_date('{EndDate}'  , 'YYYY-MM-DD')")
#
#
