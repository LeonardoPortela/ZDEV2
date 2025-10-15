* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZMMR172                                                *
* Title.......: Recebe folha de servico do coupa e cria no SAP         *
* Date........: 18/02/2022                                             *
* -------------------------------------------------------------------- *
REPORT zmmr172.

DATA: git_retorno TYPE TABLE OF zcoupa_servicos_retorno.

PARAMETERS p_id TYPE zintegrcoupa01-id_integr.

START-OF-SELECTION.

  PERFORM: f_cria_folha_de_servico.


*&---------------------------------------------------------------------*
*&      Form  f_cria_folha_de_servico
*&---------------------------------------------------------------------*
FORM f_cria_folha_de_servico.

  IF p_id IS NOT INITIAL.
    SELECT SINGLE *
      FROM zintegrcoupa01
      INTO @DATA(wa_zintegrcoupa01)
      WHERE id_integr  EQ @p_id
      AND   ident_proc EQ 'FS'
      AND   fields     NE ' '.
    IF sy-subrc = 0.
      MESSAGE |Recibo já integrado, folha { wa_zintegrcoupa01-fields }  | TYPE 'I'.
      EXIT.
    ENDIF.

  ENDIF.
  DATA: lva_metodo   TYPE string,
        lva_http_url TYPE string.

  DATA(lo_servico) = NEW zcl_integracao_recibos_coupa( ).

  IF lo_servico IS BOUND.
    IF p_id IS INITIAL.
      lva_http_url = '/receiving_transactions?exported=false&match-reference[contains]=-&fields=["id",'         &&
                        '"status","price","quantity","total","transaction_date",'      &&
                        '"type","match_reference","original_transaction_id",'          &&
                        '"voided_value","exported",'                                   &&
                        '{"order_line":'                                               &&
                        '["id","line_num","order_header_id","order_header_number"]},'  &&
                        '{"item": ["id","name","item_number"]},{"uom": ["name",'       &&
                        '"code"]},{"created_by": ["id","login","email"]},'             &&
                        '{"updated_by": ["id","login","email"]}]'.
    ELSE.
      lva_http_url = '/receiving_transactions?id=' && p_id                             &&
                        '&match-reference[contains]=-&fields=["id",'                   &&
                        '"status","price","quantity","total","transaction_date",'      &&
                        '"type","match_reference","original_transaction_id",'          &&
                        '"voided_value","exported",'                                   &&
                        '{"order_line":'                                               &&
                        '["id","line_num","order_header_id","order_header_number"]},'  &&
                        '{"item": ["id","name","item_number"]},{"uom": ["name",'       &&
                        '"code"]},{"created_by": ["id","login","email"]},'             &&
                        '{"updated_by": ["id","login","email"]}]'.
    ENDIF.


    lva_metodo = 'GET'.

    lo_servico->zif_integracao_recibos_coupa~set_ds_url( e_http_url = lva_http_url e_metodo = lva_metodo
    )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao)
                               e_integracao    = DATA(e_integracao) ).

    lo_servico->zif_integracao_recibos_coupa~get_retorno( IMPORTING e_retorno = DATA(e_retorno) ).
  ENDIF.

ENDFORM.
