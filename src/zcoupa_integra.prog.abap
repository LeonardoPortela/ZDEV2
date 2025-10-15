*&---------------------------------------------------------------------*
*&  Include  ZCOUPA_INTEGRA
*&---------------------------------------------------------------------*

DATA: wl_setleaf TYPE setleaf,
      i_head     TYPE tbtcjob.

DATA:   wl_job_id   LIKE tbtcjob-jobcount.
DATA:   wl_jobn(32).

DATA: BEGIN OF i_steplist OCCURS 10.
        INCLUDE STRUCTURE tbtcstep.
      DATA: END OF i_steplist.
DATA : c_no(1) TYPE c . "value 'N', " Criação do job

DATA: wl_tbtcjob  TYPE  tbtcjob,
      wl_tbtcstrt TYPE  tbtcstrt.

DATA: lv_repname LIKE  rsvar-report.           " for variant handling
DATA: iv_varname LIKE  raldb-variant VALUE 'SAP_UPGRADE'.
DATA: iv_varianttext  LIKE  varit-vtext VALUE 'Upgrade variant'.
DATA: wl_subrc TYPE sy-subrc.
DATA: tt_reportparam TYPE TABLE OF  rsparams WITH HEADER LINE.

IF sy-ucomm = 'BU' AND  caufvd-aufnr IS NOT INITIAL.

  REFRESH: tt_reportparam,i_steplist.
  CLEAR: tt_reportparam, i_head.

  SELECT SINGLE *
   FROM setleaf
   INTO wl_setleaf
    WHERE setname EQ 'MAGGI_JOB_USER'.

  IF sy-subrc NE 0.
    MESSAGE text-e01 TYPE 'E'.
*  EXIT.
  ENDIF.
  CONCATENATE 'COUPAORPM' caufvd-aufnr  INTO wl_jobn SEPARATED BY '|'.

  i_head-jobname = wl_jobn. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
  i_head-sdlstrtdt = sy-datum.
  i_head-sdlstrttm = sy-uzeit + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
  i_head-stepcount = 1.

*  tt_reportparam-selname = 'S_LOOKUP'.
*  tt_reportparam-kind    = 'S'.
*  tt_reportparam-sign    = 'I'.
*  tt_reportparam-option  = 'EQ'.
*  tt_reportparam-low     = 'OC'.
*  APPEND tt_reportparam.
*  CLEAR tt_reportparam.
*  "
*  tt_reportparam-selname = 'P_OP_OBJ'.
*  tt_reportparam-kind    = 'P'.
*  tt_reportparam-sign    = 'I'.
*  tt_reportparam-option  = 'EQ'.
*  tt_reportparam-low     = 'OM'.
*  APPEND tt_reportparam.
*  CLEAR tt_reportparam.
*  "
*  tt_reportparam-selname = 'S_CHAVE'.
*  tt_reportparam-kind    = 'S'.
*  tt_reportparam-sign    = 'I'.
*  tt_reportparam-option  = 'EQ'.
*  tt_reportparam-low     = caufvd-aufnr.
*  APPEND tt_reportparam.
*  CLEAR tt_reportparam.
*  "
*  tt_reportparam-selname = 'P_BATCH'.
*  tt_reportparam-kind    = 'P'.
*  tt_reportparam-sign    = 'I'.
*  tt_reportparam-option  = 'EQ'.
*  tt_reportparam-low     = 'X'.
*  APPEND tt_reportparam.
*  CLEAR tt_reportparam.

  lv_repname = 'ZMMR0035'.
*    Write the variant first (Insert or Update)
  CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
    EXPORTING
      iv_reportname         = lv_repname
      iv_variantname        = iv_varname
      iv_varianttext        = iv_varianttext
    IMPORTING
      ev_funcrc             = wl_subrc
    TABLES
      tt_reportparam        = tt_reportparam
    EXCEPTIONS
      exist_check_failed    = 1
      update_failed         = 2
      update_not_authorized = 3
      update_no_report      = 4
      update_no_variant     = 5
      update_variant_locked = 6
      insert_failed         = 7
      insert_not_authorized = 8
      insert_no_report      = 9
      insert_variant_exists = 10
      insert_variant_locked = 11
      OTHERS                = 12.

  i_steplist-parameter = iv_varname. " Nome da variante
  i_steplist-program = 'ZMMR0035'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
  i_steplist-typ = 'A'. " Tipo de Job
  i_steplist-authcknam = wl_setleaf-valfrom.
  i_steplist-language = sy-langu.
  i_steplist-arcuser = wl_setleaf-valfrom.

  APPEND i_steplist.


  c_no = 'N'.
  CALL FUNCTION 'BP_JOB_CREATE'
    EXPORTING
      job_cr_dialog       = c_no " Coloque 'Y' se quiser ver
      job_cr_head_inp     = i_head " os valores atribuidos
    IMPORTING
      job_cr_head_out     = wl_tbtcjob
      job_cr_stdt_out     = wl_tbtcstrt
    TABLES
      job_cr_steplist     = i_steplist
    EXCEPTIONS
      cant_create_job     = 1
      invalid_dialog_type = 2
      invalid_job_data    = 3
      job_create_canceled = 4
      OTHERS              = 5.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobname   = wl_jobn
      jobcount  = wl_tbtcjob-jobcount
      strtimmed = 'X'.

ENDIF.
