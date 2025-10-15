
REPORT zmmr0040_job_v2 MESSAGE-ID zjob.

************************************************************************
* tabelas
************************************************************************
TABLES: zppt0030.

************************************************************************
*& types
************************************************************************
TYPES: BEGIN OF ty_zppt0030.
         INCLUDE STRUCTURE zppt0030.
TYPES: END   OF ty_zppt0030.

TYPES: BEGIN OF ty_proc,
         werks      TYPE werks_d,
         quantidade TYPE i.
TYPES: END   OF ty_proc.

TYPES: BEGIN OF ty_filial,
         werks      TYPE zppt0030-werks,
         perc       TYPE numc3,
         quant_jobs TYPE numc3,
         criado     TYPE char1.  "*-US 130671-04.01.2024-JT
TYPES: END   OF ty_filial.

************************************************************************
*& variaveis globais
************************************************************************
DATA: vg_job                 TYPE i,
      user                   TYPE sy-uname,
      t_filial               TYPE TABLE OF ty_filial,
      w_filial               TYPE ty_filial,
      t_proc                 TYPE TABLE OF ty_proc,
      w_proc                 TYPE ty_proc,
      s_cotton               TYPE zrsdsselopts,
      t_zppt0030             TYPE TABLE OF ty_zppt0030,
      w_zppt0030             TYPE ty_zppt0030,
      t_zppt0030_grp         TYPE TABLE OF ty_zppt0030,
      w_zppt0030_grp         TYPE ty_zppt0030,
      t_zppt0030_proc        TYPE TABLE OF ty_zppt0030,
      w_zppt0030_proc        TYPE ty_zppt0030,
      t_zppt0030_sel         TYPE TABLE OF ty_zppt0030,
      w_zppt0030_sel         TYPE ty_zppt0030,
      w_zppt0031             TYPE zppt0031,
      t_zppt0037             TYPE TABLE OF zppt0037,
      w_zppt0037             TYPE zppt0037,
      job_ultimo             TYPE zppt0026-jobname,
      i_inbound              TYPE zppt016,
      qtd_processamento      TYPE i,
      qtd_itens              TYPE int4,
      l_erro                 TYPE char1,
      l_tabix                TYPE sy-tabix,
      l_jobs                 TYPE i,
      l_seq                  TYPE numc4,
      l_total_fardos         TYPE i,
      l_submit               TYPE i,
      l_totjobs              TYPE i,
      l_calc                 TYPE p DECIMALS 3,

      l_level                TYPE numc5,
      l_werks                TYPE werks_d,
      l_chave_quebra         TYPE zppt0030-chave_quebra,
      l_proc                 TYPE char1,
      l_jobname              TYPE tbtcjob-jobname,
      t_value                TYPE TABLE OF rgsb4,
      w_value                TYPE rgsb4,
      t_status               TYPE zde_btcstatus_t,
      e_quantidade           TYPE i,
      lc_qtde_times          TYPE i,
      lc_escalonar_area_aval TYPE c,
      lc_proc_pendentes      TYPE c,
      lc_show_msg            TYPE c,

      lc_wait_int_proc       TYPE i,
      print_parameters       TYPE pri_params.

"Variaveis New

DATA: git_zppt0002 TYPE TABLE OF zppt0002.

DATA: gva_lim_jobs_exec TYPE i,
      lva_finalizado    TYPE c.



************************************************************************
*  parametro ID_REFERENCIA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
  PARAMETERS: p_times TYPE i DEFAULT 1,
              p_wait  TYPE i DEFAULT 10.

  SELECT-OPTIONS : s_id_ref  FOR zppt0030-id_referencia.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* Start-Of-Selection
************************************************************************
START-OF-SELECTION.

  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd  = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.

  PERFORM f_start_processamento.

  DO p_times TIMES.

    PERFORM f_processar_fila.

    PERFORM f_check_proc_finalizados. "Retorna Processamentos finalizado para Sistema Origem(Trace Cotton)

    PERFORM f_selecao_dados.

*  ------------------------------------------------------
*   em loop ate terminar os registros selecionados
*  ------------------------------------------------------
    CLEAR: lva_finalizado.
    DO.
      PERFORM f_processa_dados CHANGING lva_finalizado.

      IF lva_finalizado = abap_true.
        EXIT.
      ENDIF.

      PERFORM f_check_proc_finalizados. "Retorna Processamentos finalizado para Sistema Origem(Trace Cotton)
      PERFORM f_processar_fila.
    ENDDO.

    WAIT UP TO p_wait SECONDS.

  ENDDO.

************************************************************************
*&-selecao dados
************************************************************************
FORM f_selecao_dados.

  DATA: lva_dt_corte TYPE sy-datum.

  CALL FUNCTION 'ZPP_FM_BENEF_ALG_0001'.  "Check se tem algum registro com processamento incompleto e restarta processamento

  lva_dt_corte = sy-datum - 30.

  CLEAR: git_zppt0002[].

  SELECT *
    FROM zppt0002 INTO TABLE git_zppt0002
   WHERE laeda                GE lva_dt_corte
     AND status_processamento EQ 'P'. "Pendente Processamento

ENDFORM.

************************************************************************
*&-processamento dos fardos
************************************************************************
FORM f_processa_dados CHANGING p_finalizado.

  FREE: p_finalizado.

  IF git_zppt0002[] IS INITIAL.
    p_finalizado = abap_true.
    EXIT.
  ENDIF.

  IF zcl_integracao_cotton_sap=>verifica_bloquear_job( ) = abap_true.
    p_finalizado = abap_true.
    EXIT.
  ENDIF.

  DATA(lit_zppt0002_grp) = git_zppt0002[].

  SORT lit_zppt0002_grp BY id_referencia .
  DELETE ADJACENT DUPLICATES FROM lit_zppt0002_grp COMPARING id_referencia.

  DATA(lva_jobs_scheduled) = zcl_integracao_cotton_sap=>get_qtde_program_exec( i_tipo     = '1'
                                                                               i_show_msg = abap_true ).

  LOOP AT lit_zppt0002_grp INTO DATA(lwa_zppt0002_grp).

    CLEAR: lwa_zppt0002_grp-jobcount_process, lwa_zppt0002_grp-jobname_process.

    PERFORM f_cria_job USING lwa_zppt0002_grp-id_referencia
                    CHANGING lwa_zppt0002_grp-jobname_process
                             lwa_zppt0002_grp-jobcount_process.

    CHECK lwa_zppt0002_grp-jobcount_process IS NOT INITIAL AND
          lwa_zppt0002_grp-jobname_process  IS NOT INITIAL.    "Prosseguir se criou JOB para a Solicitação...

    LOOP AT git_zppt0002 INTO DATA(lwa_zppt0002) WHERE id_referencia = lwa_zppt0002_grp-id_referencia.

      lwa_zppt0002-status_processamento = 'A'. "Processamento em Andamento
      lwa_zppt0002-jobcount_process     = lwa_zppt0002_grp-jobcount_process.
      lwa_zppt0002-jobname_process      = lwa_zppt0002_grp-jobname_process.
      MODIFY zppt0002 FROM lwa_zppt0002.

      DELETE git_zppt0002 WHERE acharg     = lwa_zppt0002-acharg
                            AND werks      = lwa_zppt0002-werks
                            AND id_sessao  = lwa_zppt0002-id_sessao
                            AND lgort      = lwa_zppt0002-lgort
                            AND cd_safra   = lwa_zppt0002-cd_safra.
    ENDLOOP.

    COMMIT WORK.

    IF lva_jobs_scheduled >= gva_lim_jobs_exec.
      EXIT.
    ENDIF.

  ENDLOOP.

  zcl_integracao_cotton_sap=>aguardar_job( i_tipo = '1' ).


ENDFORM.

************************************************************************
*&-criar JOBS
************************************************************************
FORM f_cria_job USING p_id_referencia
                 CHANGING c_job_name   TYPE tbtcjob-jobname
                          c_job_number TYPE tbtcjob-jobcount.


  c_job_name  = 'TRACE_CT' && '_' && p_id_referencia(22).

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = c_job_name
    IMPORTING
      jobcount         = c_job_number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH '428 - Nao consegui criar job:' && c_job_name.
  ENDIF.

  IF sy-subrc IS INITIAL.
    SUBMIT zmmr0030_job_v2 WITH p_id_ref = p_id_referencia
                           VIA JOB c_job_name NUMBER c_job_number
                           AND RETURN.
  ENDIF.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = c_job_number
        jobname              = c_job_name
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.

    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH '428 - Nao consegui fechar job:' && c_job_name.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_scheduler_job_estorno.

  DATA: number TYPE tbtcjob-jobcount,
        name   TYPE tbtcjob-jobname.

  MESSAGE '427 - verificando se ha estornos' TYPE 'S'.

  SELECT *
    FROM zppt0030
    INTO TABLE @DATA(t_0030)
      UP TO 1 ROWS
   WHERE processamento = '02'
     AND emproc_estorn = @abap_false
     AND proces_estorn = @abap_false.

  CHECK t_0030[] IS NOT INITIAL.

  name = |JOB_ESTORNO_COTTON|.
  user = sy-uname.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = name
      sdlstrtdt        = sy-datum
      sdlstrttm        = sy-uzeit
    IMPORTING
      jobcount         = number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc IS INITIAL.
    sy-uname = 'JOBADM'.

    SUBMIT zmmr0041_job VIA JOB    name
                            NUMBER number
                        AND RETURN.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = number
          jobname              = name
          strtimmed            = abap_true
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.
    ENDIF.

    sy-uname = user.
  ENDIF.

ENDFORM.

FORM f_set_limite_jobs_exec .

  gva_lim_jobs_exec = 100.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'JOB_TRACE_COTTON_LIM'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  READ TABLE t_value INTO w_value INDEX 1.
  IF sy-subrc = 0.
    gva_lim_jobs_exec = w_value-from.
  ENDIF.

  IF gva_lim_jobs_exec > 500.
    gva_lim_jobs_exec = 10.
  ENDIF.

ENDFORM.

FORM f_start_processamento .

  PERFORM f_set_limite_jobs_exec.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_proc_finalizados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_check_proc_finalizados .

  DATA: lwa_recebimento TYPE zpps0006.


  DATA: lit_zppt0002 TYPE TABLE OF zppt0002.

  CLEAR: lit_zppt0002[].

  SELECT *
    FROM zppt0002 INTO TABLE lit_zppt0002
   WHERE status_processamento       = 'C'  "Processamento Concluido
     AND status_ret_sistema_origem  = 'P'. "Retorno Sistema Origem Pendente

  CHECK lit_zppt0002[] IS NOT INITIAL.

  DATA(lit_zppt0002_agrp) = lit_zppt0002[].

  SORT lit_zppt0002_agrp BY id_referencia.
  DELETE ADJACENT DUPLICATES FROM lit_zppt0002_agrp COMPARING id_referencia.

  LOOP AT lit_zppt0002_agrp INTO DATA(lwa_zppt0002_agrp).

    lwa_recebimento-protocolo_recebimento = lwa_zppt0002_agrp-id_referencia.

    TRY.
        zcl_int_ob_ret_benef_trace_cot=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = lwa_recebimento ).
      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'S' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'S' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
    ENDTRY.

  ENDLOOP.



ENDFORM.

FORM f_processar_fila.

  DATA: lit_zppt0002_wait TYPE TABLE OF zppt0002_wait.

  DATA: lwa_zppt0002        TYPE zppt0002,
        lwa_zppt0002_exists TYPE zppt0002.

  CLEAR: lit_zppt0002_wait[].

  SELECT *
    FROM zppt0002_wait INTO TABLE lit_zppt0002_wait
   WHERE liberado_processamento = abap_false.

  SORT lit_zppt0002_wait BY timestampl.

  LOOP AT lit_zppt0002_wait INTO DATA(lwa_zppt0002_wait).
    CLEAR: lwa_zppt0002, lwa_zppt0002_exists.

    "Checar processamento em andamento para o bloco origem
    SELECT SINGLE *
      FROM zppt0002 INTO lwa_zppt0002_exists
     WHERE acharg     = lwa_zppt0002_wait-acharg
       AND werks      = lwa_zppt0002_wait-werks
       AND id_sessao  = lwa_zppt0002_wait-id_sessao
       AND lgort      = lwa_zppt0002_wait-lgort
       AND cd_safra   = lwa_zppt0002_wait-cd_safra.

    IF sy-subrc EQ 0 AND lwa_zppt0002_exists-status_ret_sistema_origem NE 'C'. "Pendente de retorno sistema Origem..
      CONTINUE.
    ENDIF.

    "Checar processamento em andamento para o bloco destino
    IF lwa_zppt0002_wait-bloco_destino IS NOT INITIAL.
      SELECT SINGLE *
        FROM zppt0002 INTO @DATA(lwa_zppt0002_bloco_proc)
       WHERE acharg     = @lwa_zppt0002_wait-acharg
         AND werks      = @lwa_zppt0002_wait-werks
         AND id_sessao  = @lwa_zppt0002_wait-id_sessao
         AND lgort      = @lwa_zppt0002_wait-bloco_destino
         AND cd_safra   = @lwa_zppt0002_wait-cd_safra.

      IF sy-subrc EQ 0 AND lwa_zppt0002_bloco_proc-status_ret_sistema_origem NE 'C'. "Pendente de retorno sistema Origem.
        CONTINUE.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING lwa_zppt0002_wait TO lwa_zppt0002.

    IF  lwa_zppt0002_exists IS NOT INITIAL.
      lwa_zppt0002-mblnr02 = lwa_zppt0002_exists-mblnr02. "Manter informação do documento de homologação Sessão
    ENDIF.

    MODIFY zppt0002 FROM lwa_zppt0002.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    lwa_zppt0002_wait-liberado_processamento = abap_true.
    MODIFY zppt0002_wait FROM lwa_zppt0002_wait.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDLOOP.

ENDFORM.

*      "Informações Cabeçalho
*      CLEAR: lwa_zppt0002_exists-cd_mensagem,
*             lwa_zppt0002_exists-prot_retorno_sistema_origem,
*             lwa_zppt0002_exists-wait_estorno_confirmation,
*             lwa_zppt0002_exists-prtnr_estorno,
*             lwa_zppt0002_exists-jobname_process,
*             lwa_zppt0002_exists-jobcount_process,
*             lwa_zppt0002_exists-chave_quebra.
*
*      lwa_zppt0002_exists-status_processamento            = 'P'. "Processamento Pendente
*      lwa_zppt0002_exists-status_ret_sistema_origem       = 'P'. "Retorno Pendente
*
*      lwa_zppt0002_exists-id_referencia                   = lwa_zppt0002_wait-id_referencia.
*      lwa_zppt0002_exists-status_registro                 = lwa_zppt0002_wait-status_registro.
*      lwa_zppt0002_exists-bldat                           = lwa_zppt0002_wait-bldat.
*      lwa_zppt0002_exists-budat                           = lwa_zppt0002_wait-budat.
*      lwa_zppt0002_exists-qtd_fardinhos_sessao            = lwa_zppt0002_wait-qtd_fardinhos_sessao.
*      lwa_zppt0002_exists-cd_safra                        = lwa_zppt0002_wait-cd_safra.
*      lwa_zppt0002_exists-peso_algodao_caroco             = lwa_zppt0002_wait-peso_algodao_caroco.
*      lwa_zppt0002_exists-menge                           = lwa_zppt0002_wait-menge.
*      lwa_zppt0002_exists-peso_caroco                     = lwa_zppt0002_wait-peso_caroco.
*      lwa_zppt0002_exists-peso_fibrilha                   = lwa_zppt0002_wait-peso_fibrilha.
*      lwa_zppt0002_exists-id_mov_sistema_origem           = lwa_zppt0002_wait-id_mov_sistema_origem.
*      lwa_zppt0002_exists-id_mov_sistema_origem_ref_int   = lwa_zppt0002_wait-id_mov_sistema_origem_ref_int.
*      lwa_zppt0002_exists-status                          = lwa_zppt0002_wait-status.
*      lwa_zppt0002_exists-laeda                           = lwa_zppt0002_wait-laeda.
*      lwa_zppt0002_exists-laehr                           = lwa_zppt0002_wait-laehr.
*
*      "Informações Detalhamento
*      lwa_zppt0002_exists-lgort                           = lwa_zppt0002_wait-lgort.
*      lwa_zppt0002_exists-qtd_bloco                       = lwa_zppt0002_wait-qtd_bloco.
*      lwa_zppt0002_exists-tipo_fardo                      = lwa_zppt0002_wait-tipo_fardo.
*      lwa_zppt0002_exists-peso_liq_atual_bloco            = lwa_zppt0002_wait-peso_liq_atual_bloco.
*      lwa_zppt0002_exists-verid                           = lwa_zppt0002_wait-verid.
*      lwa_zppt0002_exists-matnr                           = lwa_zppt0002_wait-matnr.
*      lwa_zppt0002_exists-peso_bruto                      = lwa_zppt0002_wait-peso_bruto.
*      lwa_zppt0002_exists-peso_liquido                    = lwa_zppt0002_wait-peso_liquido.
*      lwa_zppt0002_exists-qtd_fardinhos_bloco             = lwa_zppt0002_wait-qtd_fardinhos_bloco.
*      lwa_zppt0002_exists-cd_classificacao                = lwa_zppt0002_wait-cd_classificacao.
*      lwa_zppt0002_exists-dt_classificacao                = lwa_zppt0002_wait-dt_classificacao.
*
*      "Bloco Destino
*      lwa_zppt0002_exists-bloco_destino                   = lwa_zppt0002_wait-bloco_destino.
*
*      lwa_zppt0002_exists-capacidade_bloco_destino        = lwa_zppt0002_wait-capacidade_bloco_destino.
*      lwa_zppt0002_exists-bloco_destino                   = lwa_zppt0002_wait-bloco_destino.
*
*
*
*      lwa_zppt0002_exists-cd_classificacao_bloco_destino  = lwa_zppt0002_wait-cd_classificacao_bloco_destino.
*      lwa_zppt0002_exists-qtde_fardinhos_bloco_destino    = lwa_zppt0002_wait-qtde_fardinhos_bloco_destino.
*
*      MOVE-CORRESPONDING lwa_zppt0002_exists TO lwa_zppt0002.
