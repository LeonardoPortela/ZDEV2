"Name: \PR:SAPLJ_1B_NFE\FO:EPEC_ALLOWED\SE:BEGIN\EI
ENHANCEMENT 0 Z_DESVIO_GRC_TO_SIMETRYA.
*
* CASE is_header-MODEL.
*   WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_NFE.
*    select SINGLE *
*      FROM setleaf INTO @data(wl_setleaf_model_xi)
*     WHERE setname EQ 'GRC_NFE_CALL_XI_BRANCH'
*       AND valfrom EQ @is_header-BRANCH.
*   WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_CTE.
*    select SINGLE *
*      FROM setleaf INTO @wl_setleaf_model_xi
*     WHERE setname EQ 'GRC_CTE_CALL_XI_BRANCH'
*       AND valfrom EQ @is_header-BRANCH.
*   WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_MDFE.
*    select SINGLE *
*      FROM setleaf INTO @wl_setleaf_model_xi
*     WHERE setname EQ 'GRC_MDFE_CALL_XI_BRANCH'
*       AND valfrom EQ @is_header-BRANCH.
* ENDCASE.
*
* if sy-subrc is NOT INITIAL.
*    clear: cv_epec, cv_error.
*    exit.
* endif.

ENDENHANCEMENT.
