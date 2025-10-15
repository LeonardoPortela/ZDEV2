FUNCTION z_ov_job_recebimento .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN) TYPE  VBELN_VA
*"     REFERENCE(IV_ONLINE) TYPE  FLAG OPTIONAL
*"----------------------------------------------------------------------

  DATA lr_vbeln TYPE RANGE OF vbeln_va.
  DATA lv_name TYPE btcjob.
  DATA lv_count TYPE btcjobcnt.

  CHECK iv_vbeln IS NOT INITIAL.

  lr_vbeln = VALUE #( ( sign = 'I' option = 'EQ' low = iv_vbeln ) ).

  IF iv_online = abap_true.

    SUBMIT zsdr0233 WITH so_vbeln IN lr_vbeln AND RETURN.

  ELSE.

    GET TIME.
    lv_name = 'ZSDR0233_' && iv_vbeln && sy-uzeit.

    PERFORM f_job_open USING lv_name CHANGING lv_count.

    SUBMIT zsdr0233
      WITH so_vbeln IN lr_vbeln
    VIA JOB lv_name
      NUMBER lv_count AND RETURN.

    PERFORM f_job_close USING lv_name lv_count.

  ENDIF.

ENDFUNCTION.
