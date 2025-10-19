FUNCTION zmm_reverse_goods.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(VBELN) TYPE  LIKP-VBELN
*"     VALUE(BUDAT) TYPE  SY-DATLO
*"     VALUE(TCODE) TYPE  SY-TCODE
*"     VALUE(VBTYP) TYPE  LIKP-VBTYP
*"  EXPORTING
*"     VALUE(ERROR_REVERSE_GOODS_ISSUE) TYPE  CHAR1
*"  TABLES
*"      T_MESG STRUCTURE  MESG
*"  EXCEPTIONS
*"      ERROR_REVERSE_GOODS_ISSUE
*"----------------------------------------------------------------------

  CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE' "VL09  (Picking)
    EXPORTING
      i_vbeln                   = vbeln
      i_budat                   = budat
      i_tcode                   = tcode
      i_vbtyp                   = vbtyp
    TABLES
      t_mesg                    = t_mesg
    EXCEPTIONS
      error_reverse_goods_issue = 1
      OTHERS                    = 2.

  error_reverse_goods_issue = sy-subrc.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDIF.


ENDFUNCTION.
