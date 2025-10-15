*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 08.07.2022                                              &*
*& Descrição: Processamento Transferencia FArdos - Trace Cotton       &*
*&--------------------------------------------------------------------&*
*& Projeto  : Algodão                                                 &*
*&--------------------------------------------------------------------&*
REPORT zsdr0151_job MESSAGE-ID zjob.

************************************************************************
* tabelas
************************************************************************
TABLES: zsdt0330.

************************************************************************
*  parametro ID_CARGA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_carga  FOR zsdt0330-id_carga.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
*& types
************************************************************************
TYPES: BEGIN OF ty_proc,
         chave_referencia TYPE zde_id_referencia.
TYPES: END   OF ty_proc.

************************************************************************
*& variaveis globais
************************************************************************
DATA: t_zsdt0330             TYPE TABLE OF zsdt0330,
      t_0330_carga           TYPE TABLE OF zsdt0330,
      w_zsdt0330             TYPE zsdt0330,
      w_0330_carga           TYPE zsdt0330,
      t_zsdt0001             TYPE TABLE OF zsdt0001,
      w_zsdt0001             TYPE zsdt0001,
      t_jactive              TYPE TABLE OF j_1bnfe_active,
      w_jactive              TYPE j_1bnfe_active,
      w_acttab               TYPE j_1bnfe_active,
      t_proc                 TYPE TABLE OF ty_proc,
      w_proc                 TYPE ty_proc,
      l_tabix                TYPE sy-tabix,
      l_mensagem             TYPE string,
      l_jobs                 TYPE i,
      l_erro                 TYPE char01,
      l_erro_lock            TYPE c,
      is_block               TYPE char01,
      l_index                TYPE i,
      t_status               TYPE zde_btcstatus_t,
*
      l_jobname              TYPE tbtcjob-jobname,
      l_level                TYPE numc5,
      l_name                 TYPE tbtcjob-jobname,
      l_number               TYPE tbtcjob-jobcount,
      e_quantidade           TYPE i,
      lc_qtde_times          TYPE i,
      lc_escalonar_area_aval TYPE c,
      lc_proc_pendentes      TYPE c,
      lc_show_msg            TYPE c,
      lc_lim_jobs_exec       TYPE i,
      lc_wait_int_proc       TYPE i,
      print_parameters       TYPE pri_params.

*#147275-05.07.2024-JT-inicio
RANGES: lra_data_carga FOR zsdt0330-data_carga.
DATA: lva_dt_read_carga TYPE zsdt0330-data_carga.
*#147275-05.07.2024-JT-fim

************************************************************************
*&      Start-Of-Selection
************************************************************************
START-OF-SELECTION.

  FREE: t_status.

  APPEND 'R' TO t_status.

*---------------------------------------------
* se tem Job ativo, abandona
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

  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = 'ZSDR0152_JOB' " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status    " Status de Jobs
          IMPORTING
            e_quantidade = e_qtd ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd > 1.
      EXIT.
    ENDIF.
  ENDIF.

*#147275-05.07.2024-JT-inicio
  SELECT SINGLE * FROM tvarvc INTO @DATA(lwa_zsdr0153_job) WHERE name = 'ZSDR0153_JOB_DAYS_READ'.
  IF sy-subrc EQ 0.
    lva_dt_read_carga = sy-datum - lwa_zsdr0153_job-low.
    APPEND VALUE #( sign = 'I' option = 'GE' low = lva_dt_read_carga ) TO lra_data_carga.
  ENDIF.
*#147275-05.07.2024-JT-fim

*-06.11.2023 - 126769 - RBL -->
*---------------------------------------------
*-continua processamento JOBS parados
*---------------------------------------------
  PERFORM f_restarta_zmm0023.
  PERFORM f_restarta_zmm0027.
*-06.11.2023 - 126769 - RBL --<

*---------------------------------------------
*-Processamento
*---------------------------------------------
  PERFORM f_processa_dados.

  PERFORM f_check_confirmacao_cancel.

*---------------------------------------------
*-Processamento Ganho/Perda Peso
*---------------------------------------------
  PERFORM f_processa_ganho_perda.

*--------------------------------------
*-- envio de retorno de erros ao trace
*--------------------------------------
  TRY .
      zcl_trace_cotton=>zif_trace_cotton~get_instance(
         )->set_retorno_trace( ).

    CATCH zcx_integracao INTO DATA(ex_integra).
    CATCH zcx_error      INTO DATA(ex_error).
  ENDTRY.

*--------------------descomentar #126769
  TRY .
      zcl_trace_cotton=>zif_trace_cotton~get_instance(
         )->set_retorno_trace( i_tipo_operacao = 'E' ).

    CATCH zcx_integracao INTO DATA(ex_integra2).
    CATCH zcx_error      INTO DATA(ex_error2).
  ENDTRY.
*--------------------descomentar #126769

*--------------------------------------
*-- verifica reenvio de NFS
*--------------------------------------
  PERFORM f_reenvio_notas_fiscais.

************************************************************************
*&-processamento dos fardos
************************************************************************
FORM f_processa_dados.

*------------------------------------------
*- recupera ch_referencia
*------------------------------------------
  SELECT *
    FROM zsdt0330
    INTO TABLE t_zsdt0330
   WHERE id_carga     IN s_carga
     AND status_fardo IN ('0','2')
*    AND ch_referencia = abap_off
     AND cancelado     = abap_false
     AND data_carga    IN lra_data_carga.  "SD - Ganho Peso Automatico Algodao US #145369 - WPP

  LOOP AT t_zsdt0330 INTO w_zsdt0330.
    SELECT SINGLE  ch_referencia
      INTO @DATA(l_ch_referencia)
      FROM zsdt0001
     WHERE tp_movimento = 'S'
       AND nr_romaneio  = @w_zsdt0330-nr_romaneio
       AND vbeln        = @w_zsdt0330-vbeln.

    CHECK sy-subrc = 0.

    CHECK w_zsdt0330-ch_referencia NE l_ch_referencia. "SD - Ganho Peso Automatico Algodao US #145369 - WPP

    "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
    UPDATE zsdt0330 SET ch_referencia = l_ch_referencia
     WHERE id_carga      EQ w_zsdt0330-id_carga
       AND matnr         EQ w_zsdt0330-matnr
       AND werks         EQ w_zsdt0330-werks
       AND lgort         EQ w_zsdt0330-lgort
       AND acharg        EQ w_zsdt0330-acharg
       AND safra         EQ w_zsdt0330-safra
       AND seq           EQ w_zsdt0330-seq.
    "w_zsdt0330-ch_referencia = l_ch_referencia.
    "MODIFY zsdt0330       FROM w_zsdt0330.

    "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---


  ENDLOOP.

  COMMIT WORK.

  DELETE t_zsdt0330 WHERE ch_referencia IS INITIAL. "Só iniciar movimentação dos fardos se romaneio já estiver no SAP - SD - Ganho Peso Automatico Algodao US #145369 - WPP

*------------------------------------------
*- selecao dos fardos a processar
*------------------------------------------
  SELECT *
    FROM zsdt0330
    INTO TABLE t_zsdt0330
   WHERE id_carga     IN s_carga
     AND status_fardo IN ('0','2')
     AND cancelado     = abap_false.

  CHECK t_zsdt0330[] IS NOT INITIAL.

*------------------------------------------
*- para que outro job nao recupere os mesmos registros
*- ja selecionados. Fraciona processamento para liberar
*- fardos de outros centros para serem processados
*------------------------------------------
  lc_lim_jobs_exec = 100.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(wl_set_job_est_gr_lim_exec)
   WHERE setname = 'JOB_TRACE_COTTON_FAT'.

  IF sy-subrc EQ 0.
    IF wl_set_job_est_gr_lim_exec-valfrom > 0.
      lc_lim_jobs_exec = wl_set_job_est_gr_lim_exec-valfrom.
    ENDIF.
  ENDIF.

  t_0330_carga[] = t_zsdt0330[].

  SORT t_0330_carga BY chave_referencia.
  DELETE ADJACENT DUPLICATES FROM t_0330_carga
                        COMPARING chave_referencia.

*--------------------------------
* quantos jobs serao escalonados
*--------------------------------
  LOOP AT t_0330_carga INTO w_0330_carga.

    l_jobs = l_jobs + 1.

    IF l_jobs >= lc_lim_jobs_exec.
      EXIT.
    ENDIF.

    LOOP AT t_zsdt0330 INTO w_zsdt0330 WHERE chave_referencia = w_0330_carga-chave_referencia.

      l_tabix                 = sy-tabix.

      w_zsdt0330-status_fardo = '1'.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
      UPDATE zsdt0330 SET status_fardo = '1'
       WHERE id_carga      EQ w_zsdt0330-id_carga
         AND matnr         EQ w_zsdt0330-matnr
         AND werks         EQ w_zsdt0330-werks
         AND lgort         EQ w_zsdt0330-lgort
         AND acharg        EQ w_zsdt0330-acharg
         AND safra         EQ w_zsdt0330-safra
         AND seq           EQ w_zsdt0330-seq.

      "MODIFY zsdt0330      FROM w_zsdt0330.
      "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---

      MODIFY t_zsdt0330    FROM w_zsdt0330 INDEX l_tabix.

      COMMIT WORK AND WAIT.
    ENDLOOP.
  ENDLOOP.

  DELETE t_zsdt0330 WHERE status_fardo <> '1'.

  t_0330_carga[] = t_zsdt0330[].

  SORT t_0330_carga BY chave_referencia.
  DELETE ADJACENT DUPLICATES FROM t_0330_carga
                        COMPARING chave_referencia.

*-------------------------------
* criacao dos jobs
*-------------------------------
  LOOP AT t_0330_carga INTO w_0330_carga.

*-------------------------------
*-- checar quantidade de jobs em execucao
*-------------------------------
    zcl_trace_cotton=>zif_trace_cotton~set_aguardar_job( ).

*-------------------------------
*-- cria jobs
*-------------------------------
    PERFORM f_cria_job USING w_0330_carga-chave_referencia.

  ENDLOOP.

ENDFORM.


FORM f_processa_ganho_perda.

  DATA: lit_zsdt0344      TYPE TABLE OF zsdt0344,
        lva_ch_referencia TYPE zsdt0001-ch_referencia.


  "Levantar Manutençao de carga que estao pendente de geração de movimento de ganho/perda
  SELECT *
   FROM zsdt0344 AS a  INTO TABLE lit_zsdt0344
  WHERE dt_registro  IN lra_data_carga
    AND mblnr        EQ space.

  LOOP AT lit_zsdt0344 ASSIGNING FIELD-SYMBOL(<fs_zsdt0344>).

    CLEAR: lva_ch_referencia.

    DATA(_tabix) = sy-tabix.
    DATA(_delete) = abap_false.

    SELECT SINGLE  ch_referencia
      FROM zsdt0001 INTO lva_ch_referencia
     WHERE tp_movimento = 'S'
       AND nr_romaneio  = <fs_zsdt0344>-nr_romaneio
       AND vbeln        = <fs_zsdt0344>-vbeln.

    IF <fs_zsdt0344>-ch_referencia_rom NE lva_ch_referencia.
      <fs_zsdt0344>-ch_referencia_rom = lva_ch_referencia.
      MODIFY zsdt0344       FROM <fs_zsdt0344>.
      COMMIT WORK.
    ENDIF.

    IF <fs_zsdt0344>-ch_referencia_rom IS NOT INITIAL.

      SELECT SINGLE id_carga
        FROM zsdt0330  INTO @DATA(lva_id_carga)
       WHERE id_carga       EQ @<fs_zsdt0344>-id_carga
         AND seq            EQ @<fs_zsdt0344>-seq_carga
         AND ch_referencia  EQ @<fs_zsdt0344>-ch_referencia_rom
         AND status_estorno IN ( '', 'I' )
         AND status_fardo   NE '3'.

      IF sy-subrc EQ 0.
        _delete = abap_true.
      ENDIF.

    ELSE.
      _delete = abap_true.
    ENDIF.

    IF _delete EQ abap_true.
      DELETE lit_zsdt0344 INDEX _tabix.
    ENDIF.

  ENDLOOP.

  DELETE lit_zsdt0344 WHERE ch_referencia_rom IS INITIAL.

  SORT lit_zsdt0344 BY chave_referencia.
  DELETE ADJACENT DUPLICATES FROM lit_zsdt0344 COMPARING chave_referencia.


  LOOP AT lit_zsdt0344 ASSIGNING <fs_zsdt0344>.

    zcl_trace_cotton=>zif_trace_cotton~set_aguardar_job( ).

    PERFORM f_cria_job USING <fs_zsdt0344>-chave_referencia.

  ENDLOOP.


*  DATA: lit_zsdt0330_manut TYPE TABLE OF zsdt0330.
*
*  "Preencher Seq Planilha Romaneio na zsdt0344
*  SELECT *
*     FROM zsdt0344 INTO TABLE @DATA(lit_zsdt0344)
*    WHERE dt_registro IN @lra_data_carga.
*
*  LOOP AT lit_zsdt0344 ASSIGNING FIELD-SYMBOL(<fs_zsdt0344>).
*    SELECT SINGLE  ch_referencia
*      INTO @DATA(l_ch_referencia_rom)
*      FROM zsdt0001
*     WHERE tp_movimento = 'S'
*       AND nr_romaneio  = @<fs_zsdt0344>-nr_romaneio
*       AND vbeln        = @<fs_zsdt0344>-vbeln.
*
*    CHECK sy-subrc = 0 AND <fs_zsdt0344>-ch_referencia_rom NE l_ch_referencia_rom.
*
*    <fs_zsdt0344>-ch_referencia_rom = l_ch_referencia_rom.
*    MODIFY zsdt0344       FROM <fs_zsdt0344>.
*    COMMIT WORK.
*  ENDLOOP.
*
*  "Levantar Manutençao de carga que estao pendente de geração de movimento de ganho/perda
*  SELECT *
*   FROM zsdt0330 AS a    INTO TABLE lit_zsdt0330_manut
*  WHERE cancelado         EQ abap_false
*    AND data_carga        IN lra_data_carga
*
*   "Não pode ter "adição" de fardos pendentes de geração
*   AND NOT EXISTS (  SELECT id_carga
*                       FROM zsdt0330 AS b
*                      WHERE b~id_carga       EQ a~id_carga
*                        AND b~seq            EQ a~seq
*                        AND b~ch_referencia  EQ a~ch_referencia
*                        AND b~status_estorno EQ 'I'
*                        AND b~status_fardo   NE '3' )
*
*   AND NOT EXISTS ( SELECT id_carga
*                      FROM zsdt0344 AS b
*                     WHERE b~id_carga       EQ a~id_carga
*                       AND b~seq_carga      EQ a~seq
*                       AND b~ch_referencia  EQ a~ch_referencia
*                       AND b~mblnr          NE space )
*
*    AND seq  = ( SELECT MAX( seq )
*                   FROM zsdt0330 AS b
*                  WHERE b~id_carga    EQ a~id_carga ).
*
*
*  SORT lit_zsdt0330_manut BY chave_referencia.
*
*  DELETE ADJACENT DUPLICATES FROM lit_zsdt0330_manut COMPARING chave_referencia.
*
*  LOOP AT lit_zsdt0330_manut INTO w_0330_carga.
*
*    zcl_trace_cotton=>zif_trace_cotton~set_aguardar_job( ).
*
*    PERFORM f_cria_job.
*
*  ENDLOOP.

ENDFORM.


************************************************************************
*&-criar JOBS
************************************************************************
FORM f_cria_job USING p_chave_referencia TYPE zde_id_referencia.

  l_jobname = |JOB_TRACE_TRANSFERENCIA|.
  l_level   = l_level + 1.
  l_name    = l_jobname && '_' && l_level.

  "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Ini
  IF 1 = 2.
    SUBMIT zsdr0152_job WITH p_ch_ref = p_chave_referencia AND RETURN.
    EXIT.
  ENDIF.
  "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = l_name
    IMPORTING
      jobcount         = l_number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc IS INITIAL.
    SUBMIT zsdr0152_job WITH p_ch_ref = p_chave_referencia
                     VIA JOB l_name
                      NUMBER l_number
                         AND RETURN.
  ENDIF.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = l_number
        jobname              = l_name
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

ENDFORM.

************************************************************************
FORM f_cria_job_ganho_perda.

  l_jobname = |JOB_TRACE_TRANSFERENCIA|.
  l_level   = l_level + 1.
  l_name    = l_jobname && '_' && l_level.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = l_name
    IMPORTING
      jobcount         = l_number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc IS INITIAL.
    SUBMIT zsdr0152_job WITH p_idcar  = w_0330_carga-id_carga
                        WITH p_seq    = w_0330_carga-seq
                        WITH p_ch_ref = w_0330_carga-chave_referencia
                     VIA JOB l_name
                      NUMBER l_number
                         AND RETURN.
  ENDIF.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = l_number
        jobname              = l_name
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

ENDFORM.


************************************************************************
*&-reenvio de Notas Fiscais
************************************************************************
FORM f_reenvio_notas_fiscais.

*------------------------------------------
*- selecao das notas a serem reenviadas
*------------------------------------------
  SELECT *
    FROM zsdt0330
    INTO TABLE t_zsdt0330
   WHERE id_carga         IN s_carga
     AND status_nf_enviada = abap_false
     AND cancelado         = abap_false
     AND data_carga       IN lra_data_carga.  "*#147275-05.07.2024-JT

  CHECK t_zsdt0330[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0001
    INTO TABLE t_zsdt0001
     FOR ALL ENTRIES IN t_zsdt0330
   WHERE ch_referencia = t_zsdt0330-ch_referencia.

  CHECK t_zsdt0001[] IS NOT INITIAL.

  SELECT *
    FROM j_1bnfe_active
    INTO TABLE t_jactive
     FOR ALL ENTRIES IN t_zsdt0001
   WHERE docnum = t_zsdt0001-nro_nf_prod.

  LOOP AT t_jactive INTO w_jactive.
    DATA(l_task) = 'ENVIA_NF_TRACE_COTTON' && w_jactive-docnum.

    MOVE w_jactive    TO w_acttab.

    CALL FUNCTION 'ZSD_ENVIA_NF_TRACE_COTTON' STARTING NEW TASK l_task
      EXPORTING
        i_acttab = w_acttab.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_pre_processamento
*&---------------------------------------------------------------------*
FORM f_restarta_zmm0023.

  " DATA: lt_0330_save TYPE TABLE OF zsdt0330. "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>

*-------------------------------------------------------
* fase 1 do processamento
*-------------------------------------------------------
  SELECT *
    FROM zsdt0330
    INTO TABLE @DATA(lt_0330)
   WHERE status_fardo    IN ('1','2')
*    AND status_gera_lote =  '0'
     AND cancelado        = @abap_off
     AND data_carga      IN @lra_data_carga.  "*#147275-05.07.2024-JT

  CHECK sy-subrc EQ 0.

  LOOP AT lt_0330 ASSIGNING FIELD-SYMBOL(<fs_0330>) WHERE mblnr IS NOT INITIAL or mblnr_estorno is NOT INITIAL.
    DATA(lv_tabix) = sy-tabix.

*------------------------------------------------------
*-- Se mblnr e mjahr preenchido, ja coloca 3 status_fardo
*------------------------------------------------------
    IF     <fs_0330>-status_estorno <> 'D'.
      IF <fs_0330>-mblnr IS NOT INITIAL AND <fs_0330>-mjahr IS NOT INITIAL.
        <fs_0330>-status_fardo = '3'.

        "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
        "APPEND <fs_0330>      TO lt_0330_save.
        UPDATE zsdt0330 SET status_fardo = <fs_0330>-status_fardo
                      WHERE id_carga      EQ <fs_0330>-id_carga
                        AND matnr         EQ <fs_0330>-matnr
                        AND werks         EQ <fs_0330>-werks
                        AND lgort         EQ <fs_0330>-lgort
                        AND acharg        EQ <fs_0330>-acharg
                        AND safra         EQ <fs_0330>-safra
                        AND seq           EQ <fs_0330>-seq.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---

        DELETE lt_0330     INDEX lv_tabix.
        CONTINUE.
      ENDIF.
    ELSEIF <fs_0330>-status_estorno = 'D'.
      IF <fs_0330>-mblnr_estorno IS NOT INITIAL AND <fs_0330>-mjahr_estorno IS NOT INITIAL.
        <fs_0330>-status_fardo = '3'.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
        "APPEND <fs_0330>      TO lt_0330_save.
        UPDATE zsdt0330 SET status_fardo = <fs_0330>-status_fardo
                      WHERE id_carga      EQ <fs_0330>-id_carga
                        AND matnr         EQ <fs_0330>-matnr
                        AND werks         EQ <fs_0330>-werks
                        AND lgort         EQ <fs_0330>-lgort
                        AND acharg        EQ <fs_0330>-acharg
                        AND safra         EQ <fs_0330>-safra
                        AND seq           EQ <fs_0330>-seq.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---
        DELETE lt_0330     INDEX lv_tabix.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_0330[] IS NOT INITIAL.
    "Projeto Reestruturação Algodao 2024 - Ini

    SELECT *
      FROM zmmt0008
      INTO TABLE @DATA(lt_zmmt0008)
       FOR ALL ENTRIES IN @lt_0330
     WHERE werks = @lt_0330-werks
       AND lgort = @lt_0330-lgort
       AND charg = @lt_0330-acharg.

    SELECT *
      FROM zmmt0008_delete
      INTO TABLE @DATA(lt_zmmt0008_est)
       FOR ALL ENTRIES IN @lt_0330
     WHERE werks = @lt_0330-werks
       AND lgort = @lt_0330-lgort
       AND charg = @lt_0330-acharg.

    "Tratativa Registros Estorno
    "Se já existem uma movimentação de transferencia posterior a movimentação de estorno, desconsiderar esse registro de estorno
    LOOP AT lt_zmmt0008_est INTO DATA(lwa_zmmt0008_delete).

      DATA(_tabix) = sy-tabix.

      DATA(_exists_mov_posterior) = abap_false.
      DATA(_data_hora_estorno) = lwa_zmmt0008_delete-data_delete && lwa_zmmt0008_delete-hora_delete.
      LOOP AT lt_zmmt0008 INTO DATA(lwa_zsdt0008) WHERE werks  = lwa_zmmt0008_delete-werks
                                                    AND lgort  = lwa_zmmt0008_delete-lgort
                                                    AND charg  = lwa_zmmt0008_delete-charg
                                                    AND safra  = lwa_zmmt0008_delete-safra.
        DATA(_data_hora_transf) = lwa_zsdt0008-dt_registro && lwa_zsdt0008-hr_registro.

        IF _data_hora_transf > _data_hora_estorno.
          _exists_mov_posterior = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF _exists_mov_posterior = abap_true.
        DELETE lt_zmmt0008_est INDEX _tabix.
      ENDIF.
    ENDLOOP.

*    SELECT *
*      FROM mseg
*      INTO TABLE @DATA(lt_mseg)
*       FOR ALL ENTRIES IN @lt_0330
*     WHERE matnr = @lt_0330-matnr
*       AND werks = @lt_0330-werks
*       AND charg = @lt_0330-acharg
*       AND lgort = @lt_0330-lgort
*       AND bwart = 'ZA1'.
*
*    SELECT *
*      FROM mseg
*      INTO TABLE @DATA(lt_mseg_est)
*       FOR ALL ENTRIES IN @lt_0330
*     WHERE matnr = @lt_0330-matnr
*       AND werks = @lt_0330-werks
*       AND charg = @lt_0330-acharg
*       AND lgort = @lt_0330-lgort
*       AND bwart = 'ZA2'.
*
*    DELETE lt_mseg WHERE smbln <> abap_off.
*    SORT lt_mseg BY matnr werks charg lgort mblnr DESCENDING .
*    DELETE ADJACENT DUPLICATES FROM lt_mseg
*                          COMPARING matnr werks charg lgort.
*
*    DELETE lt_mseg_est WHERE smbln = abap_off.
*    SORT lt_mseg_est BY matnr werks charg lgort mblnr DESCENDING .
*    DELETE ADJACENT DUPLICATES FROM lt_mseg_est
*                          COMPARING matnr werks charg lgort.

    "Projeto Reestruturação Algodao 2024 - Fim
  ENDIF.

  LOOP AT lt_0330 ASSIGNING <fs_0330>.
    lv_tabix = sy-tabix.

    "Projeto Reestruturação Algodao 2024
    IF <fs_0330>-status_estorno <> 'D'.
      READ TABLE lt_zmmt0008 ASSIGNING FIELD-SYMBOL(<fs_zmmt0008>) WITH KEY werks = <fs_0330>-werks
                                                                            lgort = <fs_0330>-lgort
                                                                            charg = <fs_0330>-acharg
                                                                            safra = <fs_0330>-safra.
      IF sy-subrc EQ 0.
        "Se mblnr e mjahr preenchido, ja coloca 3 status_fardo
        IF <fs_zmmt0008>-mblnr IS NOT INITIAL AND <fs_zmmt0008>-mjahr IS NOT INITIAL.
          <fs_0330>-status_fardo = '3'.
          <fs_0330>-mblnr        = <fs_zmmt0008>-mblnr.
          <fs_0330>-mjahr        = <fs_zmmt0008>-mjahr.
          <fs_0330>-lgort_rec    = <fs_zmmt0008>-lgortr.

          "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
          "APPEND <fs_0330>      TO lt_0330_save.
          UPDATE zsdt0330 SET status_fardo = <fs_0330>-status_fardo
                              mblnr        = <fs_0330>-mblnr
                              mjahr        = <fs_0330>-mjahr
                              lgort_rec    = <fs_0330>-lgort_rec
                        WHERE id_carga      EQ <fs_0330>-id_carga
                          AND matnr         EQ <fs_0330>-matnr
                          AND werks         EQ <fs_0330>-werks
                          AND lgort         EQ <fs_0330>-lgort
                          AND acharg        EQ <fs_0330>-acharg
                          AND safra         EQ <fs_0330>-safra
                          AND seq           EQ <fs_0330>-seq.
          "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---



          DELETE lt_0330     INDEX lv_tabix.
          CONTINUE.
        ENDIF.
      ELSE.
        <fs_0330>-status_fardo   = '2'.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
        "APPEND <fs_0330>      TO lt_0330_save.
        UPDATE zsdt0330 SET status_fardo = <fs_0330>-status_fardo
                      WHERE id_carga      EQ <fs_0330>-id_carga
                        AND matnr         EQ <fs_0330>-matnr
                        AND werks         EQ <fs_0330>-werks
                        AND lgort         EQ <fs_0330>-lgort
                        AND acharg        EQ <fs_0330>-acharg
                        AND safra         EQ <fs_0330>-safra
                        AND seq           EQ <fs_0330>-seq.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---
        DELETE lt_0330       INDEX lv_tabix.
        CONTINUE.
      ENDIF.
    ELSEIF <fs_0330>-status_estorno = 'D'.
      READ TABLE lt_zmmt0008_est ASSIGNING FIELD-SYMBOL(<fs_zmmt0008_est>) WITH KEY werks = <fs_0330>-werks
                                                                                    lgort = <fs_0330>-lgort
                                                                                    charg = <fs_0330>-acharg
                                                                                    safra = <fs_0330>-safra.
      IF sy-subrc EQ 0.
        "Se mblnr e mjahr preenchido, ja coloca 3 status_fardo
        IF <fs_zmmt0008_est>-mblnr_estorno IS NOT INITIAL AND <fs_zmmt0008_est>-mjahr_estorno IS NOT INITIAL.
          <fs_0330>-status_fardo  = '3'.
          <fs_0330>-mblnr_estorno = <fs_zmmt0008_est>-mblnr_estorno.
          <fs_0330>-mjahr_estorno = <fs_zmmt0008_est>-mjahr_estorno.
          <fs_0330>-lgort_rec     = <fs_zmmt0008_est>-lgort.
          "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
          "APPEND <fs_0330>      TO lt_0330_save.
          UPDATE zsdt0330 SET status_fardo   = <fs_0330>-status_fardo
                              mblnr_estorno  = <fs_0330>-mblnr_estorno
                              mjahr_estorno  = <fs_0330>-mjahr_estorno
                              lgort_rec      = <fs_0330>-lgort_rec
                        WHERE id_carga      EQ <fs_0330>-id_carga
                          AND matnr         EQ <fs_0330>-matnr
                          AND werks         EQ <fs_0330>-werks
                          AND lgort         EQ <fs_0330>-lgort
                          AND acharg        EQ <fs_0330>-acharg
                          AND safra         EQ <fs_0330>-safra
                          AND seq           EQ <fs_0330>-seq.
          "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---
          DELETE lt_0330      INDEX lv_tabix.
          CONTINUE.
        ENDIF.
      ELSE.
        <fs_0330>-status_fardo   = '2'.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
        "APPEND <fs_0330>      TO lt_0330_save.
        UPDATE zsdt0330 SET status_fardo = <fs_0330>-status_fardo
                      WHERE id_carga      EQ <fs_0330>-id_carga
                        AND matnr         EQ <fs_0330>-matnr
                        AND werks         EQ <fs_0330>-werks
                        AND lgort         EQ <fs_0330>-lgort
                        AND acharg        EQ <fs_0330>-acharg
                        AND safra         EQ <fs_0330>-safra
                        AND seq           EQ <fs_0330>-seq.
        "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---
        DELETE lt_0330       INDEX lv_tabix.
        CONTINUE.
      ENDIF.
    ENDIF.


*    IF <fs_0330>-status_estorno <> 'D'.
*      READ TABLE lt_mseg ASSIGNING FIELD-SYMBOL(<fs_mseg>) WITH KEY matnr = <fs_0330>-matnr
*                                                                    werks = <fs_0330>-werks
*                                                                    charg = <fs_0330>-acharg
*                                                                    lgort = <fs_0330>-lgort.
*      IF sy-subrc EQ 0.
**------------------------------------------------------
**-- Se mblnr e mjahr preenchido, ja coloca 3 status_fardo
**------------------------------------------------------
*        IF <fs_mseg>-mblnr IS NOT INITIAL AND <fs_mseg>-mjahr IS NOT INITIAL.
*          <fs_0330>-status_fardo = '3'.
*          <fs_0330>-mblnr        = <fs_mseg>-mblnr.
*          <fs_0330>-mjahr        = <fs_mseg>-mjahr.
*          <fs_0330>-lgort_rec    = <fs_mseg>-umlgo.
*          APPEND <fs_0330>      TO lt_0330_save.
*          DELETE lt_0330     INDEX lv_tabix.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        <fs_0330>-status_fardo   = '2'.
*        APPEND <fs_0330>        TO lt_0330_save.
*        DELETE lt_0330       INDEX lv_tabix.
*        CONTINUE.
*      ENDIF.
*    ELSEIF <fs_0330>-status_estorno = 'D'.
*      READ TABLE lt_mseg_est ASSIGNING FIELD-SYMBOL(<fs_mseg_est>) WITH KEY matnr = <fs_0330>-matnr
*                                                                            werks = <fs_0330>-werks
*                                                                            charg = <fs_0330>-acharg
*                                                                            lgort = <fs_0330>-lgort.
*      IF sy-subrc EQ 0.
**------------------------------------------------------
**-- Se mblnr e mjahr preenchido, ja coloca 3 status_fardo
**------------------------------------------------------
*        IF <fs_mseg_est>-mblnr IS NOT INITIAL AND <fs_mseg_est>-mjahr IS NOT INITIAL.
*          <fs_0330>-status_fardo  = '3'.
*          <fs_0330>-mblnr_estorno = <fs_mseg_est>-mblnr.
*          <fs_0330>-mjahr_estorno = <fs_mseg_est>-mjahr.
*          <fs_0330>-lgort_rec     = <fs_mseg_est>-umlgo.
*          APPEND <fs_0330>       TO lt_0330_save.
*          DELETE lt_0330      INDEX lv_tabix.
*          CONTINUE.
*        ENDIF.
*      ELSE.
*        <fs_0330>-status_fardo   = '2'.
*        APPEND <fs_0330>        TO lt_0330_save.
*        DELETE lt_0330       INDEX lv_tabix.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
    "Projeto Reestruturação Algodao 2024 - Fim



  ENDLOOP.

  "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
*  IF lt_0330_save[] IS NOT INITIAL.
*    MODIFY zsdt0330 FROM TABLE lt_0330_save.
*  ENDIF.
  "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---


  COMMIT WORK AND WAIT.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_pre_processamento
*&---------------------------------------------------------------------*
FORM f_restarta_zmm0027.

  "DATA: lt_0330_save TYPE TABLE OF zsdt0330. "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>

  SELECT *
    FROM zsdt0330
    INTO TABLE @DATA(lt_0330)
   WHERE status_fardo      =  '3'
     AND status_gera_lote IN ('1','2')
     AND cancelado         = @abap_off
     AND data_carga       IN @lra_data_carga.  "*#147275-05.07.2024-JT

  CHECK sy-subrc EQ 0.

  SELECT *
    FROM zmmt0008
    INTO TABLE @DATA(lt_0008)
     FOR ALL ENTRIES IN @lt_0330
   WHERE werks       = @lt_0330-werks
     AND lgort       = @lt_0330-lgort
     AND charg       = @lt_0330-acharg.


  IF sy-subrc <> 0.
    FREE: lt_0008.
  ENDIF.

  LOOP AT lt_0330 ASSIGNING FIELD-SYMBOL(<fs_0330>).
    DATA(lv_tabix) = sy-tabix.

    READ TABLE lt_0008 ASSIGNING FIELD-SYMBOL(<fs_0008>) WITH KEY werks = <fs_0330>-werks
                                                                  lgort = <fs_0330>-lgort
                                                                  charg = <fs_0330>-acharg
                                                                  safra = <fs_0330>-safra.  "Projeto Reestruturação Algodao 2024
    CHECK sy-subrc EQ 0.

*------------------------------------------------------
*-- Se romaneio preenchido setar 3 no status, senao setar 2
*------------------------------------------------------
    IF <fs_0008>-nr_romaneio = <fs_0330>-nr_romaneio AND <fs_0008>-vbeln_vf IS NOT INITIAL.
      <fs_0330>-status_gera_lote = '3'.
    ELSE.

      CHECK <fs_0330>-status_gera_lote <> '2'.

      <fs_0330>-status_gera_lote = '2'.
    ENDIF.


    "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
    "APPEND <fs_0330>      TO lt_0330_save.
    UPDATE zsdt0330 SET status_gera_lote = <fs_0330>-status_gera_lote
                        WHERE id_carga      EQ <fs_0330>-id_carga
                          AND matnr         EQ <fs_0330>-matnr
                          AND werks         EQ <fs_0330>-werks
                          AND lgort         EQ <fs_0330>-lgort
                          AND acharg        EQ <fs_0330>-acharg
                          AND safra         EQ <fs_0330>-safra
                          AND seq           EQ <fs_0330>-seq.

    "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---


    DELETE lt_0330     INDEX lv_tabix.
  ENDLOOP.

  "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
*  IF lt_0330_save[] IS NOT INITIAL.
*    MODIFY zsdt0330 FROM TABLE lt_0330_save.
*  ENDIF.
  "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---


  COMMIT WORK AND WAIT.

ENDFORM.



FORM f_check_confirmacao_cancel .

  DATA: lit_zsdt0330_cancel TYPE TABLE OF zsdt0330.

  SELECT *
   FROM zsdt0330 AS a INTO TABLE lit_zsdt0330_cancel
  WHERE data_carga     IN lra_data_carga
    AND status_fardo   EQ '3'
    AND mblnr          NE space
    AND cancelado      EQ space
    AND status_estorno IN (abap_false,'I')
    AND EXISTS (
              SELECT mblnr
                FROM zsdt0330 AS b
               WHERE b~id_carga       EQ a~id_carga
                 AND b~matnr          EQ a~matnr
                 AND b~werks          EQ a~werks
                 AND b~lgort          EQ a~lgort
                 AND b~acharg         EQ a~acharg
                 AND b~safra          EQ a~safra
                 AND b~cd_sai         EQ a~cd_sai
                 AND b~seq_estornado  EQ a~seq
                 AND b~status_estorno EQ 'D' ).

  LOOP AT lit_zsdt0330_cancel INTO DATA(lwa_zsdt0330_cancel).

    UPDATE zsdt0330 SET cancelado = abap_true
      WHERE id_carga      EQ lwa_zsdt0330_cancel-id_carga
        AND matnr         EQ lwa_zsdt0330_cancel-matnr
        AND werks         EQ lwa_zsdt0330_cancel-werks
        AND lgort         EQ lwa_zsdt0330_cancel-lgort
        AND acharg        EQ lwa_zsdt0330_cancel-acharg
        AND safra         EQ lwa_zsdt0330_cancel-safra
        AND seq           EQ lwa_zsdt0330_cancel-seq.

  ENDLOOP.

  COMMIT WORK.

ENDFORM.
