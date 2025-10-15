REPORT zmmr0037.

INCLUDE: zmmr0037_t01,  "Variáveis Globais
         zmmr0037_s01,  "Tela de Seleção
         zmmr0037_cd01, "Definição de Classes
         zmmr0037_ci01. "Implementação de Classes.

START-OF-SELECTION.

* "// verifica se existe um JOB em execução
  IF sy-batch IS NOT INITIAL.
    TRY.
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.
    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  DATA: lt_lifnr TYPE ace_generic_range_t.

  lt_lifnr[] = CORRESPONDING #( s_lifnr[] ) .

  CREATE OBJECT go_supplier_data_coupa
    EXPORTING
      iv_days  = p_days
      iv_lifnr = lt_lifnr.

  IF sy-batch IS INITIAL.
    CREATE OBJECT go_alv_return.
  ENDIF.

  go_supplier_data_coupa->execute( ).

  "Antes de iniciar o processamento, salva os registros com o status em branco
  go_supplier_data_coupa->go_coupa_integration_log->save_log( ).

  LOOP AT go_supplier_data_coupa->get_import_data_create( ) INTO DATA(ls_import_data).

    TRY .

        zcl_integracao_supplier_coupa=>zif_integracao_supplier_coupa~get_instance( EXPORTING
                                                                                     iv_execution_mode = '01' "Criar/Modificar Fornecedor
          )->set_init_import( EXPORTING
                               is_coupa_supplier_data_create = ls_import_data
                               is_create                     = 'X'
                             IMPORTING
                               e_retorno_integracao   = gs_integracao_return
                              ).

        gs_zcoupa_integration_key-ident_proc = 'FO'.
        gs_zcoupa_integration_key-id_integr  = ls_import_data-number.
        "Atualiza status sucesso
        go_supplier_data_coupa->go_coupa_integration_log->set_executed_status( iv_zcoupa_integration_key
                                                                                  = gs_zcoupa_integration_key ).

        gs_import_data-id_referencia = ls_import_data-number.
        gs_import_data-tp_referencia = 'COUPA_FO'.
        gs_import_data-xml           = gs_integracao_return-ds_data_retorno.

        IF go_alv_return IS BOUND.
          go_alv_return->append_new_line( iv_import_data     = gs_import_data
                                          iv_integration_log = gs_integracao_return ).
        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_integra).

        gs_import_data-id_referencia = ls_import_data-number.
        gs_import_data-tp_referencia = 'COUPA_FO'.
        gs_import_data-xml           = gs_integracao_return-ds_data_retorno.

        IF go_alv_return IS BOUND.
          go_alv_return->append_new_line( iv_import_data     = gs_import_data
                                          iv_integration_log = gs_integracao_return ).
        ENDIF.
      CATCH zcx_error INTO DATA(ex_error).

        gs_import_data-id_referencia = ls_import_data-number.
        gs_import_data-tp_referencia = 'COUPA_FO'.
        gs_import_data-xml           = gs_integracao_return-ds_data_retorno.

        IF go_alv_return IS BOUND.
          go_alv_return->append_new_line( iv_import_data     = gs_import_data
                                          iv_integration_log = gs_integracao_return ).
        ENDIF.
    ENDTRY.

  ENDLOOP.

  LOOP AT go_supplier_data_coupa->get_import_data_modify( ) INTO DATA(ls_import_data_modify).

    TRY .

        zcl_integracao_supplier_coupa=>zif_integracao_supplier_coupa~get_instance( EXPORTING
                                                                                     iv_execution_mode = '01' "Criar/Modificar Fornecedor
          )->set_init_import( EXPORTING
                               is_coupa_supplier_data_modify = ls_import_data_modify
                               is_modify                     = 'X'
                             IMPORTING
                               e_retorno_integracao   = gs_integracao_return
                              ).

        gs_zcoupa_integration_key-ident_proc = 'FO'.
        gs_zcoupa_integration_key-ident_proc = ls_import_data_modify-number.
        "Atualiza status sucesso
        go_supplier_data_coupa->go_coupa_integration_log->set_executed_status( iv_zcoupa_integration_key
                                                                                  = gs_zcoupa_integration_key ).

        gs_import_data-id_referencia = ls_import_data_modify-number.
        gs_import_data-tp_referencia = 'COUPA_FO'.
        gs_import_data-xml           = gs_integracao_return-ds_data_retorno.

        IF go_alv_return IS BOUND.
          go_alv_return->append_new_line( iv_import_data     = gs_import_data
                                          iv_integration_log = gs_integracao_return ).
        ENDIF.

      CATCH zcx_integracao INTO ex_integra.

        gs_import_data-id_referencia = ls_import_data_modify-number.
        gs_import_data-tp_referencia = 'COUPA_FO'.
        gs_import_data-xml           = gs_integracao_return-ds_data_retorno.

        IF go_alv_return IS BOUND.
          go_alv_return->append_new_line( iv_import_data     = gs_import_data
                                          iv_integration_log = gs_integracao_return ).
        ENDIF.

      CATCH zcx_error INTO ex_error.

        gs_import_data-id_referencia = ls_import_data_modify-number.
        gs_import_data-tp_referencia = 'COUPA_FO'.
        gs_import_data-xml           = gs_integracao_return-ds_data_retorno.

        IF go_alv_return IS BOUND.
          go_alv_return->append_new_line( iv_import_data     = gs_import_data
                                          iv_integration_log = gs_integracao_return ).
        ENDIF.

    ENDTRY.

  ENDLOOP.

  go_supplier_data_coupa->go_coupa_integration_log->save_log( ).

  IF go_alv_return IS BOUND.
    go_alv_return->display( ).
  ENDIF.

END-OF-SELECTION.
