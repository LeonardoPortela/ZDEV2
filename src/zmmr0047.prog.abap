************************************************************************************
*&                        AMAGGI                                                  &*
*&--------------------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                              &*
*& Autor....: Jaime Tassoni                                                       &*
*& Data.....: 28.05.2025                                                          &*
*& Descrição: Efetuar DEVOLUCAO via JOB                                           &*
************************************************************************************
REPORT zmmr0047.

******************************************************
* cariaveis
******************************************************
DATA: t_return      TYPE TABLE OF bapiret2,
      w_headerdata  TYPE bapi_incinv_create_header,
      t_itemdata    TYPE TABLE OF bapi_incinv_create_item,
      lv_belnr_dev  TYPE re_belnr,
      lv_gjahr_dev  TYPE gjahr,
      lv_sequencia  TYPE zde_seq_log,
      gt_cfop       TYPE zde_po_cfop_t,
      w_nfe_inbound TYPE znfe_inbound,
      lc_zcl_nfe    TYPE REF TO zcl_nfe_inbound.

******************************************************
* parametros entrada
******************************************************
PARAMETERS: p_chave TYPE zde_chave_doc_e,
            pjsonhd TYPE string,
            pjsonit TYPE string,
            pjsoncf TYPE string.

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

w_nfe_inbound = lc_zcl_nfe->get_info_nota( ).

*--------------------------------------
* descerialize JSON
*--------------------------------------
/ui2/cl_json=>deserialize( EXPORTING json = pjsonhd CHANGING data = w_headerdata ).
/ui2/cl_json=>deserialize( EXPORTING json = pjsonit CHANGING data = t_itemdata ).
/ui2/cl_json=>deserialize( EXPORTING json = pjsoncf CHANGING data = gt_cfop ).

*--------------------------------------
* utilizado em includ ZSD_AJUSTAR_PESOS_NF
*--------------------------------------
EXPORT w_nfe_inbound TO MEMORY ID 'ZNFE_INBOUND'.

IF gt_cfop[] IS NOT INITIAL.
  EXPORT gt_cfop FROM gt_cfop TO MEMORY ID 'CFOPXML'.
ENDIF.

*--------------------------------------
* efetua DEVOLUCAO
*--------------------------------------
CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
  EXPORTING
    headerdata       = w_headerdata
  IMPORTING
    invoicedocnumber = lv_belnr_dev
    fiscalyear       = lv_gjahr_dev
  TABLES
    itemdata         = t_itemdata
    return           = t_return.

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

IF lv_belnr_dev IS NOT INITIAL.
  UPDATE zib_nfe_dist_ter SET belnr_dev  = lv_belnr_dev
                              gjahr_dev  = lv_gjahr_dev
                        WHERE chave_nfe  = p_chave.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDIF.

************************************************************************************
************************************************************************************
