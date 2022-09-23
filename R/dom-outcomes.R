

#' DOM outcomes
#'
#' @description not sure this has been tested
#'
#' @param FR_ID fundraiser entity id
#' @param Unit fundraiser's unit code
#' @param Plan_Type plan type code. '0' for all plan types
#' @param BeginDate a string of a date in YYYY-MM-DD format
#' @param EndDate a string of a date in YYYY-MM-DD format
#'
#' @return a query to pull the DOM outcomes table. Or maybe the details? Note sure
#' @export
dom_outcomes  <- function(FR_ID, Unit, Plan_Type = '0', BeginDate = '2021-07-01', EndDate = '2022-06-30') {

  FromDate <-  glue("to_date('{BeginDate}', 'YYYY-MM-DD')")
  ToDate   <-  glue("to_date('{EndDate}'  , 'YYYY-MM-DD')")



  glue::glue("

      with flags as
          (
          select distinct assignment_entity_id,
          max(case when cdw.f_assignment_mv.office_code = '{Unit}' and ( '0' IN ('{FR_ID}') or ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  then 'Y' else 'N' end) flg,
          max(case when cdw.f_assignment_mv.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  then start_date end ) unitdt,
          max(case when cdw.f_assignment_mv.office_code<> '{Unit}' and office_code is not null and  ( '0' IN ('{FR_ID}') or ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  and cdw.f_assignment_mv.active_ind = 'Y'  then 'Y' else 'N' end) flg2,
          nvl(max(case when cdw.f_assignment_mv.office_code <> '{Unit}'   and  ( '0' IN ('{FR_ID}') or ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  and cdw.f_assignment_mv.active_ind = 'Y' then start_date end ),'01-JAN-30') othunitdt
          from cdw.f_assignment_mv
          inner join cdw.f_proposal_mv prop on prop.proposal_id = cdw.f_assignment_mv.proposal_id
          where  ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type}))
          group by assignment_entity_id
          )



           select distinct assn.assignment_entity_id, assn.assignment_first_name, assn.assignment_last_name,
             count(distinct case when assn.start_date >= {FromDate} and assn.start_date <= {ToDate} and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID})) and  ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type}))  then assn.assignment_id end) as starts,
           --  count(distinct case when assn.stop_date >= {FromDate} and assn.stop_date <= {ToDate} and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  then assn.assignment_id end) as stops,
          -- count(distinct case when prop.stage_code = 'CN' and assn.stop_date >= {FromDate} and assn.stop_date <= {ToDate} and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  then assn.proposal_id end) as cn_stop,

           count(distinct case when
           (assn.start_date is null or assn.start_date <= prop.stop_dt)
          and nvl(assn.stop_date,sysdate) >= prop.stop_dt
          and prop.stop_dt between {FromDate} and {ToDate}
           and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID})) and ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type})) then assn.proposal_id end) as stops,

           count(distinct case when prop.stage_code = 'CN'  and granted_amt = 0
          and (assn.start_date is null or assn.start_date <= prop.stop_dt)
          and nvl(assn.stop_date,sysdate) >= prop.stop_dt
          and prop.stop_dt between {FromDate} and {ToDate}
           and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID})) and ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type}))  then assn.proposal_id end) as cn_stop,

           count(distinct case when prop.stage_code = 'TD'  and granted_amt = 0
          and (assn.start_date is null or assn.start_date <= prop.stop_dt)
          and nvl(assn.stop_date,sysdate) >= prop.stop_dt
          and prop.stop_dt between {FromDate} and {ToDate}
           and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID})) and  ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type}))  then assn.proposal_id end) as td_stop,

           /*count(distinct case when prop.stage_code = 'DQ'  and granted_amt = 0
          and (assn.start_date is null or assn.start_date <= prop.stop_dt)
          and nvl(assn.stop_date,sysdate) >= prop.stop_dt
          and prop.stop_dt between {FromDate} and {ToDate}
           and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID})) and ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type})) then assn.proposal_id end) as dq_stop,*/

          COUNT(DISTINCT dq.PROPOSAL_ID) dq_stop,

           count(distinct case when prop.stage_code = 'NR'  and granted_amt = 0
          and (assn.start_date is null or assn.start_date <= prop.stop_dt)
          and nvl(assn.stop_date,sysdate) >= prop.stop_dt
          and prop.stop_dt between {FromDate} and {ToDate}
           and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID})) and  ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type})) then assn.proposal_id end) as nr_stop,

           count(distinct case when prop.stage_code not in ('CN','TD','DQ','NR')  --and granted_amt = 0
          and (assn.start_date is null or assn.start_date <= prop.stop_dt)
          and nvl(assn.stop_date,sysdate) >= prop.stop_dt
          and prop.stop_dt between {FromDate} and {ToDate}
           and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID})) and ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type})) then assn.proposal_id end) as other_stop,

           count(distinct case when prop.stage_code = 'RP'--  and granted_amt = 0
          and (assn.start_date is null or assn.start_date <= prop.stop_dt)
          and nvl(assn.stop_date,sysdate) >= prop.stop_dt
          and prop.stop_dt between {FromDate} and {ToDate}
           and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID})) and ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type})) then assn.proposal_id end) as rp_stop,
          COUNT(DISTINCT QUALS.PROPOSAL_ID) QUAL_CNT,

          -- count(distinct case when prop.stage_code = 'TD' and assn.stop_date >= {FromDate} and assn.stop_date <= {ToDate} and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  then assn.assignment_id end) as td_stop,
          -- count(distinct case when prop.stage_code = 'DQ' and assn.stop_date >= {FromDate} and assn.stop_date <= {ToDate} and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  then assn.assignment_id end) as dq_stop,
          -- count(distinct case when prop.stage_code = 'NR' and assn.stop_date >= {FromDate} and assn.stop_date <= {ToDate} and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  then assn.assignment_id end) as nr_stop,
          -- count(distinct case when prop.stage_code NOT IN('CN','TD','DQ','NR')  and assn.stop_date >= {FromDate} and assn.stop_date <= {ToDate} and assn.office_code = '{Unit}'  and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))  then assn.assignment_id --end) as OTHER_stop,
            count(distinct case when (assn.STOP_DATE IS NULL OR assn.stop_date >={FromDate})  and assn.start_date <= {ToDate} and assn.office_code = '{Unit}'  and  ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID})) and ( '0'  in({Plan_Type}) or prop.proposal_type = ANY ({Plan_Type})) then assn.assignment_id end) as portfolio

          --count(DISTINCT assn.assignment_id) over (partition by assn.assignment_entity_id) as portfolio2,
          --count(distinct assignments.assignment_id)

             from cdw.f_assignment_mv assn
             inner join flags on flags.assignment_entity_id = assn.assignment_entity_id
             inner join cdw.d_bio_entity_record_type_mv ent on assn.assignment_entity_id = ent.entity_id
             inner join cdw.f_proposal_mv prop on prop.proposal_id = assn.proposal_id
             left join
            (

          SELECT DISTINCT ASN.ASSIGNMENT_ENTITY_ID, ASN.OFFICE_DESC, PS.PROPOSAL_ID
          FROM  CDW.F_ASSIGNMENT_MV ASN
          INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
          INNER JOIN cdw.f_proposal_mv pr ON pr.proposal_id = asn.proposal_id
          INNER JOIN
            (
            SELECT  DISTINCT PS.PROPOSAL_ID, PS.STAGE_DATE
            FROM CDW.F_PROPOSAL_STAGE_MV PS
            WHERE STAGE_CODE = 'QU' AND STAGE_DATE <= {ToDate}
            )INR ON INR.PROPOSAL_ID = PS.PROPOSAL_ID
          WHERE PS.STAGE_CODE IN ('PD','SP','GS','DS','CU') AND PS.STAGE_DATE BETWEEN {FromDate} AND {ToDate}
          AND ASN.START_DATE <= PS.STAGE_DATE AND (ASN.STOP_DATE IS NULL OR ASN.STOP_DATE >= PS.STAGE_DATE)
          AND  OFFICE_CODE = '{Unit}'
          --AND ( '0' = ANY{Plan_Type} OR pr.proposal_type= ANY{Plan_Type})
          and  ('0' in({Plan_Type}) or pr.proposal_type = any({Plan_Type}))
          AND PS.STAGE_DATE > INR.STAGE_DATE

          MINUS

          SELECT DISTINCT ASN.ASSIGNMENT_ENTITY_ID, ASN.OFFICE_DESC,  PS.PROPOSAL_ID
          FROM  CDW.F_ASSIGNMENT_MV ASN
          INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
          INNER JOIN CDW.F_PROPOSAL_MV PR ON PR.PROPOSAL_ID = ASN.PROPOSAL_ID
          INNER JOIN
            (
            SELECT  DISTINCT PS.PROPOSAL_ID, PS.STAGE_DATE
            FROM CDW.F_PROPOSAL_STAGE_MV PS
            WHERE STAGE_CODE = 'QU' AND PS.STAGE_DATE <=  {ToDate}
            )INR ON INR.PROPOSAL_ID = PS.PROPOSAL_ID
          WHERE
          PS.STAGE_CODE IN ('PD','SP','GS','DS','CU') AND (PS.STAGE_DATE  < {FromDate}
          OR (ASN.START_DATE > PS.STAGE_DATE  OR ASN.STOP_DATE < PS.STAGE_DATE))
          OR PR.STOP_DT < {FromDate}
          OR(PR.STAGE_CODE = 'QU' AND PR.ACTIVE_IND <> 'Y' AND (PR.STOP_DT < {FromDate} OR (PR.STOP_DT IS NULL AND ADD_MONTHS(PR.EXPECTED_DATE,6) < {FromDate})))
          --AND PS.STAGE_DATE > INR.STAGE_DATE


          )QUALS ON QUALS.ASSIGNMENT_ENTITY_ID = ASSN.ASSIGNMENT_ENTITY_ID

             left join
            (

          SELECT DISTINCT ASN.ASSIGNMENT_ENTITY_ID, ASN.OFFICE_DESC, PS.PROPOSAL_ID
          FROM  CDW.F_ASSIGNMENT_MV ASN
          INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
          INNER JOIN
            (
            SELECT  DISTINCT PS.PROPOSAL_ID, PS.STAGE_DATE
            FROM CDW.F_PROPOSAL_STAGE_MV PS
            WHERE STAGE_CODE = 'QU' AND STAGE_DATE <= {ToDate}
            )INR ON INR.PROPOSAL_ID = PS.PROPOSAL_ID
          WHERE PS.STAGE_CODE IN ('DQ') AND PS.STAGE_DATE BETWEEN {FromDate} AND {ToDate}
          AND ASN.START_DATE <= PS.STAGE_DATE AND (ASN.STOP_DATE IS NULL OR ASN.STOP_DATE >= PS.STAGE_DATE)
          AND  OFFICE_CODE = '{Unit}'
          AND PS.STAGE_DATE > INR.STAGE_DATE

          MINUS

          SELECT DISTINCT ASN.ASSIGNMENT_ENTITY_ID, ASN.OFFICE_DESC,  PS.PROPOSAL_ID
          FROM  CDW.F_ASSIGNMENT_MV ASN
          INNER JOIN CDW.F_PROPOSAL_STAGE_MV PS ON PS.PROPOSAL_ID = ASN.PROPOSAL_ID
          INNER JOIN CDW.F_PROPOSAL_MV PR ON PR.PROPOSAL_ID = ASN.PROPOSAL_ID
          INNER JOIN
            (
            SELECT  DISTINCT PS.PROPOSAL_ID, PS.STAGE_DATE
            FROM CDW.F_PROPOSAL_STAGE_MV PS
            WHERE STAGE_CODE = 'QU' AND PS.STAGE_DATE <=  {ToDate}
            )INR ON INR.PROPOSAL_ID = PS.PROPOSAL_ID
          WHERE
          PS.STAGE_CODE IN ('DQ') AND (PS.STAGE_DATE  < {FromDate}
          OR (ASN.START_DATE > PS.STAGE_DATE  OR ASN.STOP_DATE < PS.STAGE_DATE))
          OR PR.STOP_DT < {FromDate}
          OR(PR.STAGE_CODE = 'QU' AND PR.ACTIVE_IND <> 'Y' AND (PR.STOP_DT < {FromDate} OR (PR.STOP_DT IS NULL AND ADD_MONTHS(PR.EXPECTED_DATE,6) < {FromDate})))
          --AND PS.STAGE_DATE > INR.STAGE_DATE


          )dq ON dq.ASSIGNMENT_ENTITY_ID = ASSN.ASSIGNMENT_ENTITY_ID
             where assn.office_code = '{Unit}' and assn.assignment_type = 'DO'
            -- and prop.proposal_type <> 'ESP'
             and ( '0' IN ('{FR_ID}') or ASSN.ASSIGNMENT_ENTITY_ID = ANY( {FR_ID}))
             and ent.record_type_code = 'SF' and flg = 'Y' and flg2 = 'N' and unitdt > othunitdt
          --or (ent.RECORD_TYPE_CODE = 'SX' and ent.date_modified >= {FromDate}))

             group by assn.assignment_entity_id, assn.assignment_first_name, assn.assignment_last_name







  ")






}
