"Name: \TY:/MIGNOW/CL_MIGBP\ME:MT_FILL_PART_CUST_ADDRESS\SE:BEGIN\EI
ENHANCEMENT 0 Z_FILL_PART_SUP_ADDRESS.
*-US 135191-18-06-2024-#135191-RJF-inicio
DATA(lv_ac_termo_al5bank) = abap_false.
IMPORT lv_ac_termo_al5bank TO lv_ac_termo_al5bank FROM MEMORY ID 'AC_TERMO_AL5BANK'.

cs_bp_data-customer-central_data-central-data-j_1kftind   = is_kna1-j_1kftind.
cs_bp_data-customer-central_data-central-datax-j_1kftind  = abap_true.
cs_bp_data-customer-central_data-central-data-j_1kfrepre  = is_kna1-j_1kfrepre.
cs_bp_data-customer-central_data-central-datax-j_1kfrepre = abap_true.

IF im_object_task EQ 'U'
  and lv_ac_termo_al5bank is not initial
  AND is_kna1-kunnr IS NOT INITIAL
  AND sy-cprog EQ 'SAPMHTTP'
  AND sy-xprog EQ 'SAPMSSYD'.
  EXIT.
ENDIF.
*-US 135191-18-06-2024-#135191-RJF-fim
ENDENHANCEMENT.
