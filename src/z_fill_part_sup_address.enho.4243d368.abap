"Name: \TY:/MIGNOW/CL_MIGBP\ME:MT_FILL_PART_CUST_HEADER\SE:END\EI
ENHANCEMENT 0 Z_FILL_PART_SUP_ADDRESS.
*-US 135191-18-06-2024-#135191-RJF-inicio
  DATA(lv_ac_termo_al5bank) = abap_false.
  IMPORT lv_ac_termo_al5bank TO lv_ac_termo_al5bank FROM MEMORY ID 'AC_TERMO_AL5BANK'.
  IF lv_ac_termo_al5bank IS NOT INITIAL.
    IF im_object_task EQ 'U'
     AND sy-cprog EQ 'SAPMHTTP'
     AND sy-xprog EQ 'SAPMSSYD'.
      cs_bp_data-ensure_create-create_customer = abap_false.
    ENDIF.
  ENDIF.
*-US 135191-18-06-2024-#135191-RJF-fim
ENDENHANCEMENT.
