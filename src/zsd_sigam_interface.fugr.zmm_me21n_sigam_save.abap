FUNCTION zmm_me21n_sigam_save.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IM_HEADER) TYPE REF TO  IF_PURCHASE_ORDER_MM
*"----------------------------------------------------------------------

  DATA ls_187 TYPE zsdt0187.

  DATA lv_erro TYPE c.

  PERFORM f_check_process CHANGING lv_erro.

  CHECK lv_erro IS INITIAL.

  PERFORM f_save_zsdt0187 USING im_header.

  PERFORM check_email_change_lote USING im_header.

ENDFUNCTION.
