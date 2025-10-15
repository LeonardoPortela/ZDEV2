*&---------------------------------------------------------------------*
*& Report ZFIR0127
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0127.

DATA: gt_status     TYPE zde_btcstatus_t,
      lt_dados_exec TYPE zsdt_compensacao_automatica.

DATA: lv_jobcount TYPE tbtcjob-jobcount,
      lv_jobname  TYPE tbtcjob-jobname.


PARAMETERS: p_bukrs TYPE bukrs NO-DISPLAY.

START-OF-SELECTION.

  APPEND 'R' TO gt_status.

*---------------------------------------------
* Se tem Job ativo, abandona
*---------------------------------------------
  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog
            i_sdldate    = sy-datum
            i_status     = gt_status
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd > 2.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_bukrs IS INITIAL.

    zcl_compensacao_automatica=>seleciona_dados(
    IMPORTING
     e_dados = DATA(lt_dados) ).

    SORT lt_dados BY bukrs.

    DATA(lt_dados_aux) = lt_dados.
    DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING bukrs.

    LOOP AT lt_dados_aux ASSIGNING FIELD-SYMBOL(<fs_dados_aux>).

      lv_jobname = 'F.13_' && <fs_dados_aux>-bukrs.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = lv_jobname
        IMPORTING
          jobcount         = lv_jobcount
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.
      IF sy-subrc IS INITIAL.
        SUBMIT zfir0127 WITH p_bukrs = <fs_dados_aux>-bukrs VIA JOB lv_jobname NUMBER lv_jobcount AND RETURN.
      ENDIF.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_jobcount
          jobname              = lv_jobname
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

    ENDLOOP.

  ELSE.

    zcl_compensacao_automatica=>seleciona_dados(
    EXPORTING
      i_bukrs = p_bukrs
    IMPORTING
     e_dados = lt_dados ).

    zcl_compensacao_automatica=>executa_compensacao(
    EXPORTING
      i_dados = lt_dados ).

  ENDIF.
