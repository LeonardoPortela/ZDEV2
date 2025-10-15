"Name: \PR:J_1BECD_MAIN\FO:FILE_DOWNLOAD\SE:BEGIN\EI
ENHANCEMENT 0 ZSPED_FISCAL_ARQ.
*
*  data: WA_ZSPED004 type ZSPED004,
*        IT_ZSPED004 type TABLE OF ZSPED004.
*  "Se local
*  IF gv_lclsv = abap_true.
*      LOOP AT gt_result into gs_result.
*          WA_ZSPED004-linha = sy-tabix.
*          WA_ZSPED004-TEXTO = gs_result.
*          APPEND WA_ZSPED004 to IT_ZSPED004.
*      ENDLOOP.
*      delete from ZSPED004.
*      MODIFY ZSPED004 from TABLE IT_ZSPED004.
*      gv_lclsv = abap_false.
*  ENDIF.
ENDENHANCEMENT.
