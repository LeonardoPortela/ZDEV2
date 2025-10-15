"Name: \TY:/MIGNOW/CL_MIGBP\ME:MT_FILL_PART_CENT_ADDRESS_CUST\SE:BEGIN\EI
ENHANCEMENT 0 Z_FILL_PART_SUP_ADDRESS.
*-US 135191-18-06-2024-#135191-RJF-inicio
  DATA(lv_ac_termo_al5bank) = abap_false.
  IMPORT lv_ac_termo_al5bank TO lv_ac_termo_al5bank FROM MEMORY ID 'AC_TERMO_AL5BANK'.
  IF lv_ac_termo_al5bank IS NOT INITIAL.
    IF im_object_task EQ 'U'
      AND gs_kna1-kunnr IS NOT INITIAL
      AND sy-cprog EQ 'SAPMHTTP'
      AND sy-xprog EQ 'SAPMSSYD'.
      EXIT.
    ENDIF.
  ENDIF.
*-US 135191-18-06-2024-#135191-RJF-fim
ENDENHANCEMENT.
