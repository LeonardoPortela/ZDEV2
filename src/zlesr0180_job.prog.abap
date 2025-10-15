*&--------------------------------------------------------------------&*
*&                        AMAGGI                                      &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 19.02.2024                                              &*
*& Descrição: Emissao Documetos Faturamento Automatico                &*
*&--------------------------------------------------------------------&*
report zlesr0180_job.

*TABLES: zlest0241.

************************************************************************
* variaveis globais
************************************************************************
data: lc_faturamento_automatico type ref to zcl_faturamento_automatico,
      t_zlest0241               type table of zlest0241,
      w_zlest0241               type zlest0241,
      t_zlest0242               type table of zlest0242,
      w_zlest0242               type zlest0242,
      t_status                  type zde_btcstatus_t.

************************************************************************
* parametros entrada
************************************************************************
selection-screen begin of block b1.
  parameters : p_ch_fat type zid_integracao,
               p_it_fat type zitem_faturamento.
selection-screen end   of block b1.

************************************************************************
*  start
************************************************************************
start-of-selection.

  free: t_status.

  append 'R' to t_status.

*---------------------------------------------
* se tem Job ativo, abandona
*---------------------------------------------
  if sy-batch = abap_true and p_ch_fat is initial.
    try .
        zcl_job=>get_job_programa_execucao(
          exporting
            i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status    " Status de Jobs
          importing
            e_quantidade = data(e_qtd) ).
      catch zcx_job.
    endtry.

    if e_qtd > 1.
      exit.
    endif.
  endif.

  if p_ch_fat is initial.
*---tratar reprocessamento ----------------
    perform f_verifica_reprocessamento.

*---reenviar mensagens OPUS ---------------
    perform f_reenvio_opus.

  else.
*---selecao -------------------------------
    perform f_selecao_dados.

*---processamento -------------------------
    perform f_processamento.
  endif.

************************************************************************
* reprocessamento por cancelamento de job
************************************************************************
form f_verifica_reprocessamento.

  data: l_data   type sy-datum,
        l_ch_ref type zch_ref.

  ranges: r_jobname for tbtcp-jobname.

  create object lc_faturamento_automatico.

  l_data = sy-datum - 2.

  free: r_jobname.
  r_jobname-sign   = 'I'.
  r_jobname-option = 'CP'.
  r_jobname-low    = 'FATURA_AUT*'.
  append r_jobname.

*-----------------------
* procura jobs cancelados
*-----------------------
  select *
    from tbtcp
    into table @data(t_tbtcp)
   where jobname in @r_jobname
     and status   = 'A'
     and sdldate >= @l_data.

  check sy-subrc = 0.

  select *
    from zlest0241
    into table t_zlest0241
     for all entries in t_tbtcp
   where nome_job  = t_tbtcp-jobname
     and cancelado = abap_false.

*--------------------------
* delete job cancelado
*--------------------------
  loop at t_tbtcp into data(w_tbtcp).
    call function 'BP_JOB_DELETE'
      exporting
        jobcount                 = w_tbtcp-jobcount
        jobname                  = w_tbtcp-jobname
      exceptions
        cant_delete_event_entry  = 1
        cant_delete_job          = 2
        cant_delete_joblog       = 3
        cant_delete_steps        = 4
        cant_delete_time_entry   = 5
        cant_derelease_successor = 6
        cant_enq_predecessor     = 7
        cant_enq_successor       = 8
        cant_enq_tbtco_entry     = 9
        cant_update_predecessor  = 10
        cant_update_successor    = 11
        commit_failed            = 12
        jobcount_missing         = 13
        jobname_missing          = 14
        job_does_not_exist       = 15
        job_is_already_running   = 16
        no_delete_authority      = 17
        others                   = 18.
  endloop.

*--------------------------
* gerar job
*--------------------------
  loop at t_zlest0241 into w_zlest0241.
    select single *
      from zlest0241
        into @data(w241)
        where ch_faturamento = @w_zlest0241-ch_faturamento
        and   job_terminado = 'X'.
    if sy-subrc = 0.
      continue.
    endif.
*---atualizar status
    update zlest0241   set selecionado      = abap_false
                     where ch_faturamento   = w_zlest0241-ch_faturamento
                       and item_faturamento = w_zlest0241-item_faturamento
                       and cancelado        = abap_false.
    commit work and wait.

*---executar Job
    try.
        lc_faturamento_automatico->set_iniciar_faturamento( i_ch_referencia  = w_zlest0241-ch_referencia ).

      catch zcx_error into data(ex_error).
    endtry.
  endloop.

endform.

************************************************************************
* reenvio mensagens ao OPUS
************************************************************************
form f_reenvio_opus.

  "*-#166749-20.02.2025-JT-inicio
*  CREATE OBJECT lc_faturamento_automatico.
*
*  SELECT *
*    FROM zlest0242
*    INTO TABLE t_zlest0242
*   WHERE enviado_sistema_origem = abap_false
*     AND cancelado              = abap_false.
*
*  SORT t_zlest0242 BY ch_faturamento.
*  DELETE ADJACENT DUPLICATES FROM t_zlest0242
*                        COMPARING ch_faturamento.
*
**---------------------------------
** reenviar as mensagens nao integradas
**---------------------------------
*  LOOP AT t_zlest0242 INTO w_zlest0242.
*    TRY.
*        lc_faturamento_automatico->set_retorno_opus( i_ch_faturamento = w_zlest0242-ch_faturamento ).
*
*      CATCH zcx_error INTO DATA(ex_error).
*    ENDTRY.
*  ENDLOOP.
  "*-#166749-20.02.2025-JT-fim

endform.

************************************************************************
* selecao dados
************************************************************************
form f_selecao_dados.

*-#157580-06.11.2024-JT-inicio
  select *
    from zlest0241
    into table t_zlest0241
   where ch_faturamento   = p_ch_fat
     and item_faturamento = p_it_fat
     and selecionado      = abap_off
     and job_terminado    = abap_off
     and finalizado       = abap_off   "*-#166749-20.02.2025-JT-inicio
     and cancelado        = abap_off.

* LOOP AT t_zlest0241    INTO w_zlest0241.
*   w_zlest0241-selecionado = abap_on.
*   MODIFY zlest0241     FROM w_zlest0241.
* ENDLOOP.
*
* COMMIT WORK AND WAIT.
*-#157580-06.11.2024-JT-fim

endform.

************************************************************************
*  Processamento
************************************************************************
form f_processamento.

  data: l_erro type char01.

  create object lc_faturamento_automatico.

  loop at t_zlest0241 into w_zlest0241.

*-#157580-06.11.2024-JT-inicio
*-- Status em Processamento
    update zlest0241   set selecionado      = abap_true
                           job_terminado    = abap_false
                     where ch_faturamento   = w_zlest0241-ch_faturamento
                       and item_faturamento = w_zlest0241-item_faturamento
                       and cancelado        = abap_false.
    commit work and wait.
*-#157580-06.11.2024-JT-fim

*-----------------------------------
*-- gerar documentos ---------------------------------------------------
*-----------------------------------
    try.
        lc_faturamento_automatico->set_emitir_documentos( i_ch_faturamento   = w_zlest0241-ch_faturamento
                                                          i_item_faturamento = w_zlest0241-item_faturamento  ).
      catch zcx_error into data(ex_error).
    endtry.

*-#157580-06.11.2024-JT-inicio
*-- Status job finalizado
    update zlest0241   set selecionado      = abap_false
                           job_terminado    = abap_true
                     where ch_faturamento   = w_zlest0241-ch_faturamento
                       and item_faturamento = w_zlest0241-item_faturamento
                       and cancelado        = abap_false.
    commit work and wait.
*-#157580-06.11.2024-JT-fim

  endloop.

endform.

************************************************************************
************************************************************************
