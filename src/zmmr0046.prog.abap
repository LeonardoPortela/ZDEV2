************************************************************************************
*&                        AMAGGI                                                  &*
*&--------------------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                              &*
*& Autor....: Jaime Tassoni                                                       &*
*& Data.....: 29.04.2025                                                          &*
*& Descrição: Estorno MIRO via JOB                                                &*
************************************************************************************
REPORT zmmr0046.

******************************************************
* cariaveis
******************************************************
DATA: t_return                    TYPE TABLE OF bapiret2,
      lv_invoicedocnumber_estorno TYPE re_belnr,
      lv_fiscalyear_estorno       TYPE jahr,
      lv_sequencia                TYPE zde_seq_log,
      lc_zcl_nfe                  TYPE REF TO zcl_nfe_inbound.

******************************************************
* parametros entrada
******************************************************
PARAMETERS: p_chave  TYPE zde_chave_doc_e,
            p_docnum TYPE re_belnr,
            p_year   TYPE gjahr,
            p_revers TYPE stgrd,
            p_pgdate TYPE budat.

******************************************************
* start
******************************************************
TRY.
    CREATE OBJECT lc_zcl_nfe
      EXPORTING
        i_chave_nfe    = p_chave
        i_sem_bloqueio = abap_true.
  CATCH zcx_nfe_inbound_exception.
  CATCH zcx_cadastro.
ENDTRY.

*--------------------------------------
* estorna MIRO
*--------------------------------------
CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
  EXPORTING
    invoicedocnumber          = p_docnum
    fiscalyear                = p_year
    reasonreversal            = p_revers
    postingdate               = p_pgdate
  IMPORTING
    invoicedocnumber_reversal = lv_invoicedocnumber_estorno
    fiscalyear_reversal       = lv_fiscalyear_estorno
  TABLES
    return                    = t_return.

*--------------------------------------
* proxima sequencia log
*--------------------------------------
lv_sequencia = lc_zcl_nfe->get_sequencia_log( ).

*--------------------------------------
* montar log erros
*--------------------------------------
LOOP AT t_return INTO DATA(w_return).
  lc_zcl_nfe->set_add_log_nfe( EXPORTING i_type         = w_return-type
                                         i_id           = w_return-id
                                         i_num          = w_return-number
                                         i_message_v1   = w_return-message_v1
                                         i_message_v2   = w_return-message_v2
                                         i_message_v3   = w_return-message_v3
                                         i_message_v4   = w_return-message_v4
                                CHANGING p_lc_sequencia = lv_sequencia ).
  lv_sequencia = lv_sequencia + 1.
ENDLOOP.

*--------------------------------------
* gravar log
*--------------------------------------
lc_zcl_nfe->nfe_inbound_gravar_log( ).

IF lv_invoicedocnumber_estorno IS NOT INITIAL.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDIF.

************************************************************************************
************************************************************************************
