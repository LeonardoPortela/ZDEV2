*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 08.07.2022                                              &*
*& Descrição: Processamento Trace Cotton                              &*
*&--------------------------------------------------------------------&*
*& Projeto  : Algodão                                                 &*
*&--------------------------------------------------------------------&*
REPORT zmmr0040_job MESSAGE-ID zjob.

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
      number                 TYPE tbtcjob-jobcount,
      name                   TYPE tbtcjob-jobname,
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
      l_data                 TYPE sy-datum,
      l_level                TYPE numc5,
      l_werks                TYPE werks_d,
      l_chave_quebra         TYPE zppt0030-chave_quebra,
      l_proc                 TYPE char1,
      l_finalizado           TYPE char1,
      l_jobname              TYPE tbtcjob-jobname,
      t_value                TYPE TABLE OF rgsb4,
      w_value                TYPE rgsb4,
      t_status               TYPE zde_btcstatus_t,
      e_quantidade           TYPE i,
      lc_qtde_times          TYPE i,
      lc_escalonar_area_aval TYPE c,
      lc_proc_pendentes      TYPE c,
      lc_show_msg            TYPE c,
      lc_lim_jobs_exec       TYPE i,
      lc_wait_int_proc       TYPE i,
      print_parameters       TYPE pri_params.

************************************************************************
*  parametro ID_REFERENCIA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_id_ref  FOR zppt0030-id_referencia.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* Start-Of-Selection
************************************************************************
START-OF-SELECTION.

  FREE: l_data,
        l_finalizado,
        t_status.

* APPEND 'S' TO t_status.
  APPEND 'R' TO t_status.
* APPEND 'P' TO t_status.

*---------------------------------------------
* se Job ativo, abandona
*---------------------------------------------
  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status    " Status de Jobs
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd > 1.
      EXIT.
    ENDIF.
  ENDIF.

*---------------------------------------------
*-Efetua processaemnto Normal e Estorno
*---------------------------------------------
  FREE: l_level, l_seq.

*------------------------------------------
* Ajusta CHARG na tabela zppt0002
*------------------------------------------
  PERFORM f_ajusta_charg.  "*-#125503 = 26.10.2023 - JT

*------------------------------------------
* virifica fardos parados na etapa "em processamento"
*------------------------------------------
  PERFORM f_checa_jobs_cancelados.

*------------------------------------------
* selecao fardos
*------------------------------------------
  PERFORM f_selecao_dados USING abap_off.

*------------------------------------------
* em loop ate terminar os registros selecionados
*------------------------------------------
  DO.
    l_level = l_level + 1.

*------------------------------------------
*-- Cria JOB de estorno, caso precise
*------------------------------------------
    PERFORM f_verifica_estorno.

*------------------------------------------
*-- processamento normal dos fardinhos
*------------------------------------------
    PERFORM f_selecao_dados     USING abap_true.
    PERFORM f_processa_dados CHANGING l_finalizado.

    IF l_finalizado = abap_true.
      EXIT.
    ENDIF.
  ENDDO.

************************************************************************
*&-checa jobs cancelados
************************************************************************
FORM f_checa_jobs_cancelados.

  DATA(l_data_lim) = sy-datum.

  SELECT *
    FROM zppt0030
    INTO TABLE @DATA(t_0030_prod)
   WHERE id_referencia IN @s_id_ref
     AND emproc_normal  = @abap_true
     AND proces_normal  = @abap_false
     AND data_proc      < @l_data_lim.

  SORT t_0030_prod BY id_cotton id_referencia.
  DELETE ADJACENT DUPLICATES FROM t_0030_prod
                        COMPARING id_cotton id_referencia.

  IF t_0030_prod[] IS NOT INITIAL.
    SELECT *
      FROM zppt0002
      INTO TABLE @DATA(t_0002)
       FOR ALL ENTRIES IN @t_0030_prod
     WHERE id_cotton     = @t_0030_prod-id_cotton
       AND id_referencia = @t_0030_prod-id_referencia.
  ENDIF.

*-----------------------------
* tratar procto normal
*-----------------------------
  LOOP AT t_0030_prod INTO DATA(w_0030_prod).
    READ TABLE t_0002 INTO DATA(w_0002) WITH KEY id_cotton     = w_0030_prod-id_cotton
                                                 id_referencia = w_0030_prod-id_referencia
                                                 status        = 'R'.
    IF sy-subrc = 0.
      IF w_0030_prod-processamento = '01'.
        UPDATE zppt0002 SET status          = 'R'
                            status_registro = w_0030_prod-status_registro
                      WHERE id_cotton       = w_0030_prod-id_cotton
                        AND id_referencia   = w_0030_prod-id_referencia.
      ELSE.
        UPDATE zppt0002 SET status          = 'R'
                            status_registro = '99'
                      WHERE id_cotton       = w_0030_prod-id_cotton
                        AND id_referencia   = w_0030_prod-id_referencia.
      ENDIF.

      UPDATE zppt0030 SET emproc_normal   = abap_off
                          proces_normal   = abap_off
                    WHERE id_cotton       = w_0030_prod-id_cotton
                      AND id_referencia   = w_0030_prod-id_referencia.
    ELSE.
      UPDATE zppt0030 SET emproc_normal   = abap_on
                          proces_normal   = abap_on
                    WHERE id_cotton       = w_0030_prod-id_cotton
                      AND id_referencia   = w_0030_prod-id_referencia.
    ENDIF.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

************************************************************************
*&-ajustar charg
************************************************************************
FORM f_ajusta_charg.

  DATA: l_data TYPE datum.

  l_data = sy-datum - 30.

  SELECT *
    FROM zppt0002
    INTO TABLE @DATA(t_0002)
   WHERE laeda     >= @l_data
     AND charg      = @abap_off.

  CHECK sy-subrc = 0.

  SELECT *
    FROM zpps_ximfbf_log AS a
    INTO TABLE @DATA(t_ximfbf)
     FOR ALL ENTRIES IN @t_0002
   WHERE a~id_cotton  = @t_0002-id_cotton
     AND a~processado = 'S'    "*-CS2022000332-#84404-02.08.2022-JT-inicio
     AND NOT EXISTS ( SELECT * FROM mseg AS b WHERE b~smbln = a~mblnr
                                               AND NOT EXISTS ( SELECT * FROM mseg AS c WHERE c~smbln = b~mblnr ) ).

  SELECT *
    FROM zmmt0006 AS m1
    INTO TABLE @DATA(t_0006)
     FOR ALL ENTRIES IN @t_0002
   WHERE id_cotton     = @t_0002-id_cotton
     AND ch_referencia = ( SELECT MAX( m2~ch_referencia ) FROM zmmt0006 AS m2 WHERE m2~id_cotton = m1~id_cotton ).

  SELECT *
    FROM mseg
    INTO TABLE @DATA(t_mseg)
     FOR ALL ENTRIES IN @t_0002
   WHERE xblnr_mkpf = @t_0002-id_cotton
     AND smbln      = @abap_off
     AND xauto      = @abap_true.

  LOOP AT t_0002 INTO DATA(w_0002).

    READ TABLE t_ximfbf INTO DATA(w_ximfbf) WITH KEY id_cotton = w_0002-id_cotton.
    IF sy-subrc = 0 AND w_ximfbf-charg IS NOT INITIAL.
      w_0002-charg   = w_ximfbf-charg.
    ELSE.
      READ TABLE t_0006 INTO DATA(w_0006) WITH KEY id_cotton = w_0002-id_cotton.
      IF sy-subrc = 0 AND w_0006-batch_d IS NOT INITIAL.
        w_0002-charg = w_0006-batch_d.
      ELSE.
        READ TABLE t_mseg INTO DATA(w_mseg) WITH KEY xblnr_mkpf = w_0002-id_cotton.
        IF sy-subrc EQ 0 AND w_mseg-charg IS NOT INITIAL.
          w_0002-charg = w_mseg-charg.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY zppt0002 FROM w_0002.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

************************************************************************
*&-selecao dados
************************************************************************
FORM f_selecao_dados USING p_retorno.

*------------------------------------------
*- selecao dos fardos a processar
*------------------------------------------
  IF p_retorno = abap_off.
    SELECT *
      FROM zppt0030 INTO TABLE t_zppt0030_sel
     WHERE id_referencia IN s_id_ref
       AND processamento  = '01'
       AND emproc_normal  = abap_false
       AND proces_normal  = abap_false
     ORDER BY id_referencia.
  ELSE.
    SELECT *
      FROM zppt0030 APPENDING TABLE t_zppt0030_sel
     WHERE id_referencia IN s_id_ref
       AND processamento  = '01'
       AND emproc_normal  = abap_false
       AND proces_normal  = abap_false
       AND retorno_lock   = abap_true
    ORDER BY id_referencia.
  ENDIF.

ENDFORM.

************************************************************************
*&-processamento dos fardos
************************************************************************
FORM f_processa_dados CHANGING p_finalizado.

  FREE: p_finalizado, t_filial, l_totjobs.

*------------------------------------------
*- acabou fardinhos
*------------------------------------------
  IF t_zppt0030_sel[] IS INITIAL.
    p_finalizado = abap_true.
    EXIT.
  ENDIF.

*------------------------------------------
*- checa se jon esta bloqueado
*------------------------------------------
  IF zcl_integracao_cotton_sap=>verifica_bloquear_job( ) = abap_true.
    p_finalizado = abap_true.
    EXIT.
  ENDIF.

*------------------------------------------
*- ajusta tabela de processamento
*------------------------------------------
  t_zppt0030[] = t_zppt0030_sel[].

*------------------------------------------
*- limite de JOBS para execucao
*------------------------------------------
  lc_lim_jobs_exec = 100.

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
    lc_lim_jobs_exec = w_value-from.
  ENDIF.

  IF lc_lim_jobs_exec > 500.
    lc_lim_jobs_exec = 10.
  ENDIF.

*------------------------------------------
*- verificar como sera a distribiocao dos JOBS
*------------------------------------------
  SELECT *
    FROM zppt0037
    INTO TABLE t_zppt0037
   WHERE processo     = '1'  "Processamento normal
     AND periodo_ini <= sy-datum
     AND periodo_fim >= sy-datum.

*-US 130671-04.01.2024-JT-inicio
*------------------------------------------
* se houve algum ajuste de priorizacao por fazenda, abandona job
*------------------------------------------
  READ TABLE t_zppt0037    INTO w_zppt0037 WITH KEY reinicia_job = abap_true.
  IF sy-subrc = 0.
    UPDATE zppt0037 SET reinicia_job = abap_false.
    COMMIT WORK.
    p_finalizado = abap_true.
    EXIT.
  ENDIF.

  LOOP AT t_zppt0037 INTO w_zppt0037 WHERE perc_jobs = 0.
    LOOP AT t_zppt0030 INTO w_zppt0030 WHERE werks = w_zppt0037-werks.
      DELETE t_zppt0030 INDEX sy-tabix.
    ENDLOOP.
    LOOP AT t_zppt0030_sel INTO w_zppt0030 WHERE werks = w_zppt0037-werks.
      DELETE t_zppt0030_sel INDEX sy-tabix.
    ENDLOOP.
  ENDLOOP.

  IF t_zppt0030_sel[] IS INITIAL.
    p_finalizado = abap_true.
    EXIT.
  ENDIF.
*-US 130671-04.01.2024-JT-fim

  FREE: t_filial.

  LOOP AT t_zppt0037 INTO w_zppt0037.
    w_filial-werks      = w_zppt0037-werks.
    w_filial-perc       = w_zppt0037-perc_jobs.
    APPEND w_filial    TO t_filial.
  ENDLOOP.

*-US 130671-04.01.2024-JT-inicio
* DELETE t_filial WHERE perc = 0.
*-US 130671-04.01.2024-JT-fim

*------------------------------------------
*- calcula quantidade de JOBS por filial
*------------------------------------------
  FREE: l_totjobs.

  LOOP AT t_filial   INTO w_filial.
    w_filial-quant_jobs = lc_lim_jobs_exec * ( w_filial-perc / 100 ).
    l_totjobs           = l_totjobs        +   w_filial-quant_jobs.
    MODIFY t_filial  FROM w_filial INDEX sy-tabix.
  ENDLOOP.

*------------------------------------------
*- avalia quantos fardinhos a processar por filial
*------------------------------------------
  FREE: t_proc.

  LOOP AT t_zppt0030 INTO w_zppt0030.
    CLEAR w_proc.
    w_proc-werks        = w_zppt0030-werks.
    w_proc-quantidade   = 1.
    COLLECT w_proc   INTO t_proc.
  ENDLOOP.

  DESCRIBE TABLE t_zppt0030 LINES l_total_fardos.

*------------------------------------------
*- calcula quantidade de JOBS para outras filiais nao cadastradas
*------------------------------------------
  l_jobs = lc_lim_jobs_exec - l_totjobs.

  LOOP AT t_proc        INTO w_proc.
    READ TABLE t_filial INTO w_filial WITH KEY werks = w_proc-werks.
    IF sy-subrc <> 0.
      CLEAR w_filial.
      l_calc               = ( l_jobs * w_proc-quantidade ) / l_total_fardos.
      w_filial-werks       = w_proc-werks.
      w_filial-quant_jobs  = round( val = l_calc dec = 3 ).
      w_filial-criado      = abap_true.  "*-US 130671-04.01.2024-JT
      APPEND w_filial     TO t_filial.
    ENDIF.
  ENDLOOP.

*------------------------------------------
*- conta quantos jobs ficaram para cada filial, e realoca caso alguma filial
*- ficou sem job
*------------------------------------------
  FREE: l_totjobs.

  LOOP AT t_filial      INTO w_filial.
    l_totjobs              = l_totjobs + w_filial-quant_jobs.
  ENDLOOP.

  l_jobs                   = lc_lim_jobs_exec - l_totjobs.

  IF l_jobs > 0.
    READ TABLE t_filial INTO w_filial WITH KEY criado = abap_true. "*-US 130671-04.01.2024-JT
    IF sy-subrc = 0.
      w_filial-quant_jobs  = w_filial-quant_jobs + l_jobs.
      MODIFY t_filial   FROM w_filial INDEX sy-tabix.
    ENDIF.
  ENDIF.

*------------------------------------------
*- agrupa fardos pro processo (por ex. Producao pode ter 4 fardinhos
*- para processar no mesmo id_referencia e chave_quebra, e deve ser
*- rodado em sequencia
*------------------------------------------
  t_zppt0030_grp[] = t_zppt0030[].
  SORT t_zppt0030_grp BY id_referencia chave_quebra.
  DELETE ADJACENT DUPLICATES FROM t_zppt0030_grp
                        COMPARING id_referencia chave_quebra.

*------------------------------------------
*- Ativa cada fardinho na tabela para processamento,
*- segindo a priorizacao feita
*------------------------------------------
  LOOP AT t_filial INTO w_filial.

    FREE l_jobs.

    LOOP AT t_zppt0030_grp INTO w_zppt0030_grp WHERE werks = w_filial-werks GROUP BY ( key1 = w_zppt0030_grp-id_referencia
                                                                                       key2 = w_zppt0030_grp-chave_quebra ).
      l_jobs = l_jobs + 1.

      IF l_jobs > w_filial-quant_jobs.
        EXIT.
      ENDIF.

      LOOP AT t_zppt0030   INTO w_zppt0030 WHERE id_referencia = w_zppt0030_grp-id_referencia
                                             AND chave_quebra  = w_zppt0030_grp-chave_quebra.
        l_tabix                  = sy-tabix.
        w_zppt0030-emproc_normal = abap_true.
        w_zppt0030-retorno_lock  = abap_false.

        MODIFY zppt0030    FROM w_zppt0030.
        MODIFY t_zppt0030  FROM w_zppt0030 INDEX l_tabix.

        UPDATE zppt0002 SET id_referencia = w_zppt0030-id_referencia
                            chave_quebra  = w_zppt0030-chave_quebra
                      WHERE acharg        = w_zppt0030-acharg
                        AND werks         = w_zppt0030-werks.

*-------------------------
*------ ajusta tabela original
*-------------------------
        READ TABLE t_zppt0030_sel INTO w_zppt0030_sel WITH KEY id_referencia = w_zppt0030-id_referencia
                                                                      acharg = w_zppt0030-acharg
                                                                       werks = w_zppt0030-werks.
        IF sy-subrc = 0.
          DELETE t_zppt0030_sel INDEX sy-tabix.
        ENDIF.

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  DELETE t_zppt0030 WHERE emproc_normal = abap_false.

  COMMIT WORK AND WAIT.

*-------------------------------
*-- Criacao dos JOBS pela chave
*-------------------------------
  t_zppt0030_proc[] = t_zppt0030[].

  SORT t_zppt0030_proc BY id_referencia chave_quebra.
  DELETE ADJACENT DUPLICATES FROM t_zppt0030_proc
                        COMPARING id_referencia chave_quebra.

  MESSAGE '305 - Processa blocos por id_referencia' TYPE 'S'.
  MESSAGE '305 - Verificando Quant Jobs' TYPE 'S'.

*-------------------------------
*-- checar quantidade de jobs em execucao
*-------------------------------
  zcl_integracao_cotton_sap=>aguardar_job( i_tipo = '1' ).

*-------------------------------
*-Processa blocos por id_referencia
*-------------------------------
  LOOP AT t_zppt0030_proc INTO w_zppt0030_proc.

    l_seq = l_seq + 1.

*-------------------------------
*-- cria jobs
*-------------------------------
    PERFORM f_cria_job.

  ENDLOOP.

ENDFORM.

************************************************************************
* verificar se ha estornos
************************************************************************
FORM f_verifica_estorno.
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

************************************************************************
*&-criar JOBS
************************************************************************
FORM f_cria_job.

  l_jobname = |JOB_TRACE_COTTON|.
  name      = l_jobname && '_' && l_level && '_' && w_zppt0030_proc-werks && '_' && l_seq.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = name
    IMPORTING
      jobcount         = number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH '428 - Nao consegui criar job:' && name.
  ENDIF.

  IF sy-subrc IS INITIAL.
    SUBMIT zmmr0030_job WITH p_id_ref = w_zppt0030_proc-id_referencia
                        WITH p_chqueb = w_zppt0030_proc-chave_quebra
                        WITH p_chlock = w_zppt0030_proc-chave_lock
                        WITH p_jobnam = name
                        VIA JOB name
                     NUMBER number
                        AND RETURN.
  ENDIF.

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

    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH '428 - Nao consegui fechar job:' && name.
    ENDIF.
  ENDIF.

ENDFORM.

************************************************************************
************************************************************************
