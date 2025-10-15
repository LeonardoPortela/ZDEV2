REPORT zmmr0038.

DATA: xv_jobnm TYPE btcjob.
DATA: xv_stepc TYPE btcstepcnt.
DATA: vg_job      TYPE i.

INCLUDE: zmmr0038_t01,
         zmmr0038_s01,
         zmmr0038_cd01,
         zmmr0038_ci01.

START-OF-SELECTION.
  IF sy-batch EQ abap_true.

*    TRY.
*        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
*      CATCH zcx_job.
*    ENDTRY.
*    IF e_qtd GT 1.
*      LEAVE PROGRAM.
*    ENDIF.
    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobname         = xv_jobnm
        stepcount       = xv_stepc
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.

    IF xv_jobnm+0(8) NE 'COUPAREC'.
      IF xv_jobnm = 'ZMMR0038_JOB_COUPA'.
        SELECT SINGLE COUNT(*) INTO vg_job
            FROM tbtco
           WHERE jobname EQ 'ZMMR0038_JOB_COUPA'
             AND status EQ 'R'.
        IF vg_job NE 1.
          MESSAGE s016(ds) WITH 'Job esta sendo executado!' sy-cprog DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE s016(ds) WITH 'Nome deve ser->ZMMR0038_JOB_COUPA' sy-cprog DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ELSE.

      SPLIT xv_jobnm AT '|' INTO DATA(lv_desc)
                                 DATA(lv_mblnr)
                                 DATA(lv_gjahr).

      REFRESH: s_mblnr.
      IF lv_mblnr IS INITIAL.
        EXIT.
      ENDIF.
      s_mblnr-sign   = 'I'.
      s_mblnr-option = 'EQ'.
      s_mblnr-low    = lv_mblnr.
      APPEND s_mblnr.

      "
      p_days = 1.
    ENDIF.
  ENDIF.
  CREATE OBJECT go_invoice_data_coupa
    EXPORTING
      iv_days = p_days.

  IF sy-batch IS INITIAL.
    CREATE OBJECT go_alv_return.
  ENDIF.

  go_invoice_data_coupa->execute( ).

  DATA(lo_integration_log) = go_invoice_data_coupa->get_log_object( ).

  "Antes de iniciar o processamento, salva os registros com o status em branco
  lo_integration_log->save_log( ).

  LOOP AT go_invoice_data_coupa->get_import_data( ) INTO DATA(ls_import_data).

    TRY.

        zcl_integracao_recibo_coupa=>zif_integracao_recibo_coupa~get_instance(
          )->set_init_import( EXPORTING
                               is_import_data       = ls_import_data
                             IMPORTING
                               e_retorno_integracao = gs_integracao_return
                              ).

        CLEAR: gs_zcoupa_integration_key.
        CONCATENATE ls_import_data-custom_fields-id_sap sy-datum(4) INTO gs_zcoupa_integration_key-id_integr.
        gs_zcoupa_integration_key-ident_proc = 'MG'.

        "Atualiza status sucesso
        lo_integration_log->set_executed_status( iv_zcoupa_integration_key = gs_zcoupa_integration_key ).

        go_invoice_data_coupa->save_recibo_coupa_sap( iv_integration_log = gs_integracao_return
                                                      iv_import_data     = ls_import_data ).

        IF go_alv_return IS BOUND.
          go_alv_return->append_new_line( iv_import_data     = gs_zcoupa_integration_key
                                          iv_integration_log = gs_integracao_return ).
        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_integra).
        CLEAR: gs_zcoupa_integration_key.
        CONCATENATE ls_import_data-custom_fields-id_sap sy-datum(4) INTO gs_zcoupa_integration_key-id_integr.
        gs_zcoupa_integration_key-ident_proc = 'MG'.
        IF go_alv_return IS BOUND.
          go_alv_return->append_new_line( iv_import_data     = gs_zcoupa_integration_key
                                          iv_integration_log = gs_integracao_return ).
        ENDIF.
      CATCH zcx_error INTO DATA(ex_error).
        CLEAR: gs_zcoupa_integration_key.
        CONCATENATE ls_import_data-custom_fields-id_sap sy-datum(4) INTO gs_zcoupa_integration_key-id_integr.
        gs_zcoupa_integration_key-ident_proc = 'MG'.
        IF go_alv_return IS BOUND.
          go_alv_return->append_new_line( iv_import_data     = gs_zcoupa_integration_key
                                          iv_integration_log = gs_integracao_return ).
        ENDIF.
    ENDTRY.

  ENDLOOP.

  "Grava status de sucesso para registros processados com êxito.
  lo_integration_log->save_log( ).

  IF go_alv_return IS BOUND AND sy-batch EQ abap_false.
    go_alv_return->display( ).
  ENDIF.

END-OF-SELECTION.
