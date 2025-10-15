FUNCTION zmm_nfse_miro_frete_estorno.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_LIFNR) TYPE  LIFNR
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"     REFERENCE(I_BELNR) TYPE  BELNR_D
*"     REFERENCE(I_GJAHR) TYPE  GJAHR
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_BELNR_OUT) TYPE  BELNR_D
*"     REFERENCE(E_GJAHR_OUT) TYPE  GJAHR
*"     REFERENCE(E_DOCNUM_CANC) TYPE  J_1BDOCNUM
*"  TABLES
*"      ET_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  DATA lv_erro TYPE c.
  DATA lv_augdt TYPE augdt.
  DATA lv_augbl TYPE augbl.

  CHECK ( i_belnr IS NOT INITIAL AND i_gjahr IS NOT INITIAL ).

  CALL FUNCTION 'Z_VERIFICA_MIRO_PAGA'
    EXPORTING
      belnr = i_belnr
      gjahr = i_gjahr
    CHANGING
      augdt = lv_augdt
      augbl = lv_augbl.

  "Miro está baixada/compensada
  IF NOT lv_augdt IS INITIAL.

    " miro baixada ou compensada
    PERFORM f_put_mensagem4
      USING 'E'
            'ZCTE_DISTRI'
            '083'
            i_belnr
            i_gjahr
            lv_augdt
            lv_augbl
   CHANGING et_return[].

    EXIT.

  ENDIF.

  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
    EXPORTING
      invoicedocnumber          = i_belnr
      fiscalyear                = i_gjahr
      reasonreversal            = 'Z1' "Estorno no período atual
    IMPORTING
      invoicedocnumber_reversal = e_belnr_out
      fiscalyear_reversal       = e_gjahr_out
    TABLES
      return                    = et_return.

  IF e_belnr_out IS NOT INITIAL AND i_commit = 'X'.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    PERFORM f_nf_doc_cancel
      USING i_docnum
   CHANGING e_docnum_canc
            et_return[]
            lv_erro.

    PERFORM f_gera_compensacao
      USING i_bukrs
            i_lifnr
            i_belnr
            i_gjahr
            e_belnr_out
            e_gjahr_out
   CHANGING lv_erro.

    IF lv_erro IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      PERFORM f_put_mensagem4
        USING sy-msgty
              sy-msgid
              sy-msgno
              sy-msgv1
              sy-msgv2
              sy-msgv3
              sy-msgv4
     CHANGING et_return[].

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ENDIF.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ENDIF.

ENDFUNCTION.
