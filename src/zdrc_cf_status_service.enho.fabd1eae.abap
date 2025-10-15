"Name: \TY:CL_J_1BNFE_CF_STATUS_SERVICE\ME:GET_LOCATOR\SE:BEGIN\EI
ENHANCEMENT 0 ZDRC_CF_STATUS_SERVICE.

   DATA: lva_no_check_use_drc TYPE c.

   lva_no_check_use_drc = abap_true.
   EXPORT lva_no_check_use_drc FROM lva_no_check_use_drc TO MEMORY ID 'Z_NO_CHECK_USE_BRANCH_DRC'.

ENDENHANCEMENT.
