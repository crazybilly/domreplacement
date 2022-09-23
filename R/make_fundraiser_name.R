#' Make PROSPECT_DEV style Fundraiser Names
#'
#' @param db a valid CDW connection
#'
#' @return a two column tbl_sql: Entity ID and FUNDRAISER_NAME
#' @export
#'
#' @examples
make_fundraiser_name  <- function(db = cdw) {

  tbl(cdw, sql("

    SELECT
       ENTITY_ID,
       D_ENTITY_MV.LAST_NAME ||
          NVL2(TRIM(D_ENTITY_MV.PERS_SUFFIX || D_ENTITY_MV.PROF_SUFFIX), ' ' || TRIM(D_ENTITY_MV.PERS_SUFFIX || ' ' || D_ENTITY_MV.PROF_SUFFIX), NULL) ||
          NVL2(TRIM(D_ENTITY_MV.FIRST_NAME), ', ' || D_ENTITY_MV.FIRST_NAME, NULL) ||
          NVL2(TRIM(D_ENTITY_MV.MIDDLE_NAME), ' ' || SUBSTR(D_ENTITY_MV.MIDDLE_NAME,1,1) || '.', NULL) AS FUNDRAISER_NAME
    FROM CDW.D_ENTITY_MV;
  "))

}
