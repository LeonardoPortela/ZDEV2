REPORT zmmr0035.

INCLUDE: zmmr0035_t01,  "Variáveis globais.
         zmmr0035_s01,  "Tela de Seleção.
         zmmr0035_cd01, "Definição de Classes.
         zmmr0035_ci01. "Implementação de Classes.

START-OF-SELECTION.

  IF sy-batch IS NOT INITIAL.
    p_batch = sy-batch.
  ENDIF.

* "// verifica se existe um JOB em execução
  IF p_batch IS NOT INITIAL.

    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobname         = gv_job_name
        jobcount        = gv_job_count
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
    ENDIF.

    IF gv_job_name+0(9) NE 'COUPAORPM'.
      SELECT * INTO TABLE @DATA(it_tbtco)
        FROM tbtco AS j
       WHERE j~status EQ 'R'.

      IF sy-subrc IS INITIAL.
        SELECT * INTO TABLE @DATA(it_tbtcp)
          FROM tbtcp
           FOR ALL ENTRIES IN @it_tbtco
         WHERE progname EQ @sy-repid
           AND jobname  EQ @it_tbtco-jobname
           AND jobcount EQ @it_tbtco-jobcount.
        IF sy-subrc IS INITIAL.
          LOOP AT it_tbtcp INTO DATA(ls_tbtcp).
            IF ls_tbtcp-jobname EQ gv_job_name AND ls_tbtcp-jobcount NE gv_job_count.
              LEAVE PROGRAM.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE.
      SPLIT gv_job_name AT '|' INTO DATA(lv_desc)
                                    DATA(lv_ordem).
      REFRESH: s_chave,s_lookup.
      CLEAR: s_chave,s_lookup.
      "
      IF lv_ordem IS INITIAL.
        EXIT.
      ENDIF.

      p_op_obj = 'OM'.
      "
      s_lookup-sign   = 'I'.
      s_lookup-option = 'EQ'.
      s_lookup-low    = 'OC'.
      APPEND  s_lookup.
      "
      s_chave-sign   = 'I'.
      s_chave-option = 'EQ'.
      s_chave-low    = lv_ordem.
      APPEND  s_chave.

    ENDIF.
  ENDIF.

  IF p_batch IS INITIAL.
    CREATE OBJECT go_alv_return.
  ENDIF.

  LOOP AT s_lookup INTO DATA(p_lookup).
    CONCATENATE 'LCL_PROCESS_LOOKUP_VALUES_' p_lookup-low INTO gv_object_reference.

    TRY.
        CREATE OBJECT go_process_lookup_values TYPE (gv_object_reference)
          EXPORTING
            iv_days = p_days.
      CATCH cx_sy_create_object_error.
        RETURN.
    ENDTRY.

    go_process_lookup_values->execute_process( IMPORTING
                                              et_import_data = gt_import_data ).

    "Antes de iniciar o processamento, salva os registros com o status em branco
    go_process_lookup_values->go_coupa_integration_log->save_log( ).

    LOOP AT gt_import_data INTO DATA(gs_import_data).

      TRY .

          zcl_integracao_lookup_coupa=>zif_integracao_lookup_coupa~get_instance(
            )->set_init_import( EXPORTING
                                 is_coupa_import_data = gs_import_data
                               IMPORTING
                                 e_retorno_integracao = gs_integracao_return
                                ).

          CLEAR: gs_zcoupa_integration_key.
          gs_zcoupa_integration_key-id_integr  = gs_import_data-id_referencia.
          gs_zcoupa_integration_key-ident_proc = gs_import_data-tp_referencia.
          "Atualiza status sucesso
          go_process_lookup_values->go_coupa_integration_log->set_executed_status( iv_zcoupa_integration_key
                                                                                    = gs_zcoupa_integration_key ).

          IF go_alv_return IS BOUND.
            go_alv_return->append_new_line( iv_import_data     = gs_import_data
                                            iv_integration_log = gs_integracao_return ).
          ENDIF.
        CATCH zcx_integracao INTO DATA(ex_integra).
          IF go_alv_return IS BOUND.
            go_alv_return->append_new_line( iv_import_data     = gs_import_data
                                            iv_integration_log = gs_integracao_return ).
          ENDIF.
        CATCH zcx_error INTO DATA(ex_error).
          IF go_alv_return IS BOUND.
            go_alv_return->append_new_line( iv_import_data     = gs_import_data
                                            iv_integration_log = gs_integracao_return ).
          ENDIF.
      ENDTRY.

    ENDLOOP.

    go_process_lookup_values->go_coupa_integration_log->save_log( ).
  ENDLOOP.

  IF go_alv_return IS BOUND.
    go_alv_return->display( ).
  ENDIF.
