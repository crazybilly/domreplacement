
#' Get Proposal Data
#'
#' @param primary_prop_only a logical determining whether you want all proposal rows or only the primary proposal row
#' @param primary_entity_only a logical determinng whether you want all entities or only the primary entity in each prospect
#' @param db the database conenction to use
#'
#' @return a database table with one row per assignment, proposal, prospect and entity. Inactive assignments, proposals and prospects are included
#' @export
#'
get_proposal_data  <- function(primary_prop_only = T, primary_entity_only = T, db = cdw) {

  f_assignment_mv  <- tbl(db, in_schema2("CDW", "f_assignment_mv"))
  f_proposal_mv    <- tbl(db, in_schema2("CDW", "f_proposal_mv"))
  d_prospect_mv    <- tbl(db, in_schema2("CDW", "d_prospect_mv"))
  d_entity_mv      <- tbl(db, in_schema2("CDW", "d_entity_mv"))


  primary_prop_criteria    <- ifelse(primary_prop_only, 'Y', c('Y', 'N'))
  primary_entity_criteria  <- ifelse(primary_entity_only, 'Y', c('Y', 'N'))


  assignments  <- f_assignment_mv |>
    filter(
       ASSIGNMENT_TYPE == 'DO'
      ,!is.na(PROPOSAL_ID)
    ) |>
    rename(
      ASSIGNMENT_ACTIVE = ACTIVE_IND
      , ASSIGNMENT_START_DT = START_DATE
      , ASSIGNMENT_STOP_DT  = STOP_DATE
      , ASSIGNMENT_PROGRAM_CODE = PROGRAM_CODE
      , ASSIGNMENT_PROGRAM_DESC = PROGRAM_DESC
      , ASSIGNMENT_UNIT_CODE = UNIT_CODE
      , ASSIGNMENT_UNIT_DESC = UNIT_DESC
    ) |>
    select(-ENTITY_ID, -PROSPECT_ID)


  proposals  <-  f_proposal_mv |>
    rename(
      PROPOSAL_ACTIVE = ACTIVE_IND
      , PROPOSAL_STAGE_CODE = STAGE_CODE
      , PROPOSAL_STAGE_DESC = STAGE_DESC
      , PROPOSAL_START_DT = START_DT
      , PROPOSAL_STOP_DT  = STOP_DT
      , PROPOSAL_PROGRAM_CODE = PROGRAM_CODE
      , PROPOSAL_PROGRAM_DESC = PROGRAM_DESC
      , PROPOSAL_UNIT_CODE = UNIT_CODE
      , PROPOSAL_UNIT_DESC = UNIT_DESC
      , PROOSAL_CRM_ID = CRM_ID
    ) |>
    select(-ENTITY_ID, -CADS_ENTITY_ID)

      # for reasons that arent clear, not every proposal has a primary
      if(primary_prop_only) {
        proposals  <- proposals |>
          select(-PRIMARY_IND, -PRIMARY_IND_CNT) |>
          distinct()
      } else {
        proposals  <- proposals |>
          select(-PRIMARY_IND, -PRIMARY_IND_CNT)
      }

  prospects  <- d_prospect_mv |>
    rename(
        PROSPECT_ACTIVE = ACTIVE_IND
      , PROSPECT_START_DT = START_DATE
      , PROSPECT_STOP_DT  = STOP_DATE
      , PROSPECT_CRM_ID = CRM_ID
    ) |>
    select(-EXPECTED_DATE) |>
    # handling for missing primary
        # if you don't have have primary, and only have one row, it's primary
        # if you have multiple rows (you shouldn't), use the lowest entity id
    group_by(PROSPECT_ID) |>
    mutate(
        has_active = max(ACTIVE_IND_CNT)
      , has_primary = coalesce(max(PROSPECT_ENTITY_PRIMARY_IND), 'N')
      , n_rows = n()
      , lowest_entity = min(ENTITY_ID)
    ) |>
    ungroup() |>
    mutate(
      primary_entity_revised = case_when(
          has_primary == 'Y' ~ PROSPECT_ENTITY_PRIMARY_IND
        , n_rows == 1 ~ 'Y'
        , ENTITY_ID == lowest_entity ~ 'Y'
        , 1 == 1 ~ 'N'
      )
    ) |>
    filter(primary_entity_revised %in% primary_entity_criteria) |>
    select(-has_active, -has_primary, -n_rows, -lowest_entity)



  entities  <-  d_entity_mv |>
    select(
      ENTITY_ID
      , REPORT_NAME
      , RECORD_STATUS_CODE
      , SPOUSE_ENTITY_ID
      , CAPACITY_RATING_CODE
      , CAPACITY_RATING_DESC
    )




  # build the join
  assignments |>
    inner_join(proposals, by = 'PROPOSAL_ID') |>
    left_join(prospects, by = 'PROSPECT_ID') |>
    left_join(entities,  by = "ENTITY_ID")   |>
    select(-matches("\\.(x|y)$"))


}



