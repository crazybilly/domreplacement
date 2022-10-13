#' CSM Maindata
#'
#' @param portfolio_q  a text string of a CSM portfolio query
#' @param start_dt  the activity start date, as a string
#' @param end_dt the activity end date, as a string
#'
#' @return a text string that can pull the maindata CTE from the Campus Sumary Metrics
#' @export
#'
csm_maindata_details  <- function(portfolio_q, start_dt = '2021-07-01', end_dt = '2022-06-30') {

  maindata_q  <-  glue("
        SELECT DISTINCT ASN.OFFICE_CODE,
        ASN.ASSIGNMENT_ENTITY_ID,
        ASN.PROPOSAL_ID,
        CASE WHEN PR.INITIAL_CONTRIBUTION_DT BETWEEN to_date('{start_dt}', 'YYYY-MM-DD') AND to_date('{end_dt}', 'YYYY-MM-DD') AND (ASN.STOP_DATE >= PR.INITIAL_CONTRIBUTION_DT OR ASN.STOP_DATE IS NULL)
        AND ASN.START_DATE <=  PR.INITIAL_CONTRIBUTION_DT  AND PR.ASK_AMT > 0 THEN PR.PROPOSAL_ID END AS ASKS,

        CASE WHEN PR.ASK_AMT > 0 AND PR.INITIAL_CONTRIBUTION_DT BETWEEN to_date('{start_dt}', 'YYYY-MM-DD') AND to_date('{end_dt}', 'YYYY-MM-DD') AND (ASN.STOP_DATE >= PR.INITIAL_CONTRIBUTION_DT OR ASN.STOP_DATE IS NULL) AND ASN.START_DATE <=      PR.INITIAL_CONTRIBUTION_DT AND PR.ASK_AMT > 0   THEN PR.ASK_AMT END AS ASK_AMT,

        CASE WHEN PR.GRANTED_AMT > 0 AND PR.STOP_DT BETWEEN to_date('{start_dt}', 'YYYY-MM-DD') AND to_date('{end_dt}', 'YYYY-MM-DD') AND (STOP_DATE >= PR.STOP_DT OR ASN.STOP_DATE IS NULL) AND ASN.START_DATE <=  PR.STOP_DT
        THEN PR.PROPOSAL_ID END AS COMMITS,

        ( CASE  WHEN
          PR.PROPOSAL_TYPE = 'ESP'
          AND PR.STOP_DT IS NOT NULL
          AND PR.GRANTED_AMT > 0
          AND (ASN.START_DATE IS NULL OR START_DATE <= PR.STOP_DT)
          AND  NVL(ASN.STOP_DATE,SYSDATE) >= PR.STOP_DT
          AND PR.STOP_DT <= to_date('{end_dt}', 'YYYY-MM-DD')
          AND DGD.GIFT_CREDIT_DT BETWEEN to_date('{start_dt}', 'YYYY-MM-DD') AND to_date('{end_dt}', 'YYYY-MM-DD')
          AND DGD.PLEDGED_BASIS_FLG = 'Y' AND DGD.DONOR_ENTITY_ID_NBR = PR.ENTITY_ID
          THEN DGD.TRANSACTION_CREDITED_AMT

          WHEN pr.proposal_type <> 'ESP' and   PR.GRANTED_AMT > 0 AND PR.STOP_DT BETWEEN to_date('{start_dt}', 'YYYY-MM-DD') AND to_date('{end_dt}', 'YYYY-MM-DD') AND (ASN.STOP_DATE >= PR.STOP_DT OR ASN.STOP_DATE IS NULL) AND ASN.START_DATE <=  PR.STOP_DT
          THEN PR.GRANTED_AMT END) AS COMMIT_AMT
        FROM PORTFOLIO
        INNER JOIN CDW.F_ASSIGNMENT_MV ASN ON ASN.ASSIGNMENT_ENTITY_ID = PORTFOLIO.ASSIGNMENT_ENTITY_ID AND ASN.OFFICE_CODE = PORTFOLIO.OFFICE_CODE
        INNER JOIN CDW.F_PROPOSAL_MV PR ON PR.PROPOSAL_ID = ASN.PROPOSAL_ID
        INNER JOIN CDW.D_BIO_ENTITY_RECORD_TYPE_MV REC ON REC.ENTITY_ID = ASN.ASSIGNMENT_ENTITY_ID
        LEFT JOIN CDW.DONOR_GIVING_DETAIL_F_V DGD ON DGD.PROPOSAL_ID = PR.PROPOSAL_ID AND DGD.DONOR_ENTITY_ID_NBR = PR.ENTITY_ID  AND DGD.GIFT_CREDIT_DT BETWEEN to_date('{start_dt}', 'YYYY-MM-DD') AND to_date('{end_dt}', 'YYYY-MM-DD')
        AND DGD.PLEDGED_BASIS_FLG = 'Y'

        WHERE
        ASSIGNMENT_TYPE = 'DO'
        AND PR.PRIMARY_IND = 'Y'
        AND (ASN.STOP_DATE >=to_date('{start_dt}', 'YYYY-MM-DD') OR STOP_DATE IS NULL)
        AND ASN.START_DATE <= to_date('{end_dt}', 'YYYY-MM-DD')
        AND PR.START_DT <=to_date('{end_dt}', 'YYYY-MM-DD')
        --AND NVL(PR.STOP_DT,SYSDATE) >= to_date('{start_dt}', 'YYYY-MM-DD') --removedJRL 20170502
        AND REC.RECORD_TYPE_CODE = 'SF'
        -- AND PR.PROPOSAL_TYPE <> 'ESP'
    ")

  str_c(
    "WITH"
    , 'PORTFOLIO as ('
    , portfolio_q
    , ') -- , maindata as ('
    , maindata_q
    , sep = '\n'
  )

}


#' Pull Contact Reports like the Campus Summary Metrics
#'
#' @param maindata_q a text string to pull query the maindata CTE, as produced by csm_maindata_details()
#' @param start_dt  the activity start date, as a string
#' @param end_dt the activity end date, as a string
#'
#' @return a text string that can be used to query the CDW to get contact reports
#' @export
csm_contacts  <- function(maindata_q,  start_dt = '2021-07-01', end_dt = '2022-06-30') {

  maindata_q <- maindata_q |>
    str_replace('-- , maindata as \\(', ' , maindata as (') |>
    str_c(')', sep = '\n')

  the_contacts_q  <- glue("


      select
        maindata.assignment_entity_id
        , cont.report_id
        , cont.contact_date
        , cont.contact_type
        , cont.contact_type_desc
        , cont.contact_purpose_code
        , cont.contact_purpose_desc
        , cont.contact_attitude_code
        , cont.contact_attitude_desc

      from maindata
      INNER JOIN CDW.F_CONTACT_REPORTS_MV CONT
        ON
              (CONT.CONTACT_CREDIT_ENTITY_ID = MAINDATA.ASSIGNMENT_ENTITY_ID OR CONT.AUTHOR_ENTITY_ID = MAINDATA.ASSIGNMENT_ENTITY_ID)
          AND (CONT.CONTACT_CREDIT_UNIT_CODE = MAINDATA.OFFICE_CODE OR CONT.UNIT_CODE = MAINDATA.OFFICE_CODE)

      -- INNER JOIN CDW.D_ENTITY_MV ENT
      --   ON (ENT.ENTITY_ID = CONT.CONTACT_ENTITY_ID  OR ENT.ENTITY_ID = CONT.CONTACT_ALT_ENTITY_ID)

      WHERE CONTACT_DATE BETWEEN to_date('{start_dt}', 'YYYY-MM-DD') AND to_date('{end_dt}', 'YYYY-MM-DD')
    ")

  str_c(
      maindata_q
    , the_contacts_q
    , sep = '\n'
  )


}


#' Qualifications Counted in the CSM
#'
#' @param portfolio_q a text string that queries the CDW for the portfolio, made by csm_build_portfolio_query()
#' @param office  the office code string. Must match whatever portfolio_q was built with
#' @param plan_type  the play type code string. Must match whatever portfolio_q was built with
#' @param start_dt  the activity start date, as a string
#' @param end_dt the activity end date, as a string
#'
#' @return a text string that can be used to query the CDW to get proposals to be counted as qualifications
#' @export
#'
csm_qualifications  <- function(portfolio_q, office = '0', plan_type = '0',  start_dt = '2021-07-01', end_dt = '2022-06-30') {

  portfolio_q  <- str_c(
    "with"
    , "PORTFOLIO as ("
    , portfolio_q
    , ")"
    , ""
    , sep = '\n'
  )

  qual_q  <-
    glue("

      /*
       SELECT DISTINCT ASSIGNMENT_ENTITY_ID, OFFICE_CODE,
        COUNT(DISTINCT PROPOSAL_ID) QUAL_CNT
        FROM
        (
      */
        SELECT DISTINCT PORTFOLIO.ASSIGNMENT_ENTITY_ID, PORTFOLIO.OFFICE_CODE, PS.PROPOSAL_ID
        FROM PORTFOLIO
        INNER JOIN CDW.F_ASSIGNMENT_MV ASN ON ASN.ASSIGNMENT_ENTITY_ID = PORTFOLIO.ASSIGNMENT_ENTITY_ID
        INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
        INNER JOIN CDW.F_PROPOSAL_MV PR ON PR.PROPOSAL_ID = ASN.PROPOSAL_ID
        INNER JOIN
          (
          SELECT  DISTINCT PS.PROPOSAL_ID, STAGE_DATE
          FROM CDW.F_PROPOSAL_STAGE_MV PS
          WHERE STAGE_CODE = 'QU' AND STAGE_DATE <= to_date('{end_dt}', 'YYYY-MM-DD')
          )INR ON INR.PROPOSAL_ID = PS.PROPOSAL_ID
        WHERE PS.STAGE_CODE IN ('PD','SP','GS','DS','CU','RP','DQ','TD') AND PS.STAGE_DATE BETWEEN to_date('{start_dt}', 'YYYY-MM-DD') AND to_date('{end_dt}', 'YYYY-MM-DD')
        AND ASN.START_DATE <= PS.STAGE_DATE AND (ASN.STOP_DATE IS NULL OR ASN.STOP_DATE >= PS.STAGE_DATE)
        AND  ('0' = '{office}'    OR ASN.OFFICE_CODE = '{office}')
        AND ( '0' = '{plan_type}' OR PR.PROPOSAL_TYPE= '{plan_type}')
        AND PS.STAGE_DATE > INR.STAGE_DATE

        MINUS

        SELECT DISTINCT PORTFOLIO.ASSIGNMENT_ENTITY_ID, PORTFOLIO.OFFICE_CODE,  PS.PROPOSAL_ID
        FROM PORTFOLIO
        INNER JOIN CDW.F_ASSIGNMENT_MV ASN ON ASN.ASSIGNMENT_ENTITY_ID = PORTFOLIO.ASSIGNMENT_ENTITY_ID
        INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
        INNER JOIN CDW.F_PROPOSAL_MV PR ON PR.PROPOSAL_ID = ASN.PROPOSAL_ID
        INNER JOIN
          (
          SELECT  DISTINCT PS.PROPOSAL_ID, STAGE_DATE
          FROM CDW.F_PROPOSAL_STAGE_MV PS
          WHERE STAGE_CODE = 'QU' AND STAGE_DATE <=  to_date('{end_dt}', 'YYYY-MM-DD')
          )INR ON INR.PROPOSAL_ID = PS.PROPOSAL_ID
        WHERE
        PS.STAGE_CODE IN ('PD','SP','GS','DS','CU','CN','RP','DQ','TD') AND (PS.STAGE_DATE  < to_date('{start_dt}', 'YYYY-MM-DD')
        OR (ASN.START_DATE > PS.STAGE_DATE  OR ASN.STOP_DATE < PS.STAGE_DATE))
        --AND PS.STAGE_DATE > INR.STAGE_DATE
        OR PR.STOP_DT < to_date('{start_dt}', 'YYYY-MM-DD')
        OR(PR.STAGE_CODE = 'QU' AND PR.ACTIVE_IND <> 'Y' AND (PR.STOP_DT <  to_date('{start_dt}', 'YYYY-MM-DD') OR (PR.STOP_DT IS NULL AND ADD_MONTHS(PR.EXPECTED_DATE,6) < to_date('{start_dt}', 'YYYY-MM-DD'))))

      /*
        )
        GROUP BY ASSIGNMENT_ENTITY_ID, OFFICE_CODE
        )
      */

  ")


  str_c(
      portfolio_q
    , qual_q
    , sep = '\n'
  )


}

#' Plans Excluded from the CSM Qualifications
#'
#' @description the CTE that counts qualifications in the CSM has a complicated subquery that is MINUS'd from the list of qualifications. To help with debugging, this function builds the query to see what's in that subquery, to help you identify why plans might not be counted.
#'
#' @param portfolio_q a text string that queries the CDW for the portfolio, made by csm_build_portfolio_query()
#' @param start_dt  the activity start date, as a string
#' @param end_dt the activity end date, as a string
#'
#' @return a text string that can be used to query the CDW for non-counted qualifications
#' @export
csm_qualification_exclusions  <- function(portfolio_q,  start_dt = '2021-07-01', end_dt = '2022-06-30') {

  str_c(
    "with"
  , 'portfolio as ('
  , portfolio_q
  , ")"
  , glue("
        SELECT
            DISTINCT
                PORTFOLIO.ASSIGNMENT_ENTITY_ID
              , PORTFOLIO.OFFICE_CODE
              ,  PS.PROPOSAL_ID

        FROM
                   PORTFOLIO
        INNER JOIN CDW.F_ASSIGNMENT_MV ASN    ON ASN.ASSIGNMENT_ENTITY_ID = PORTFOLIO.ASSIGNMENT_ENTITY_ID
        INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
        INNER JOIN CDW.F_PROPOSAL_MV PR       ON PR.PROPOSAL_ID = ASN.PROPOSAL_ID
        INNER JOIN
          (
            SELECT
              DISTINCT
                  PS.PROPOSAL_ID
                , STAGE_DATE
            FROM
              CDW.F_PROPOSAL_STAGE_MV PS
            WHERE
                  STAGE_CODE = 'QU'
              AND STAGE_DATE <=  to_date('{end_dt}', 'YYYY-MM-DD')
          ) INR -- plans started QU before unassignment
            ON INR.PROPOSAL_ID = PS.PROPOSAL_ID

        WHERE
              PS.STAGE_CODE IN ('PD','SP','GS','DS','CU','CN','RP','DQ','TD')  -- already in post-QU stage
          AND (                                                                -- got there before assig start/after plan ended
                PS.STAGE_DATE < to_date('{start_dt}', 'YYYY-MM-DD')
            OR ASN.START_DATE > PS.STAGE_DATE
            OR ASN.STOP_DATE  < PS.STAGE_DATE
          )
        --AND PS.STAGE_DATE > INR.STAGE_DATE
        OR PR.STOP_DT < to_date('{start_dt}', 'YYYY-MM-DD')                                 -- or plan ended before date range
        OR(                                                                                 -- or plan died in QU
              PR.STAGE_CODE = 'QU'
          AND PR.ACTIVE_IND <> 'Y'
          AND (
                PR.STOP_DT <  to_date('{start_dt}', 'YYYY-MM-DD')
            OR (
                  PR.STOP_DT IS NULL
              AND ADD_MONTHS(PR.EXPECTED_DATE,6) < to_date('{start_dt}', 'YYYY-MM-DD')
            )
          )
        )
  ")

  , sep = '\n'
  )


}
