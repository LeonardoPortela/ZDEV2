*&---------------------------------------------------------------------*
*& Report  ZFIR0083
*&---------------------------------------------------------------------*
*& Programa para atualizar ZIB_CONTABIL com dados das Invoices criadas
*& no IMOS
*&---------------------------------------------------------------------*

REPORT zfir0083.


START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  DATA(obj_vip) = NEW zcl_integra_vip( ).

  " 1 -> Busca novos registros na tabela ZFIT0164 para lançar na ZIB_CONTABIL
  obj_vip->set_invoice_zib_contabil( ).

  " 2 -> Buscar registros contabilizados para retorno ao IMOS.
  obj_vip->set_retorna_status( ).

  " 3 -> Retorno de documentos compensados
  obj_vip->get_simple_payment( ).

  " 4 -> Limpar tabela de mensagens com data anterior a 6 meses
  obj_vip->limpa_tabela_mensagens( ).

  " 5 -> Envia modifica~ção de FOrnecedor e Cliente
  obj_vip->send_contraparties( ).
