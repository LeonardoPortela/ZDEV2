*&---------------------------------------------------------------------*
*&  Include  ZCOUPA_INTEGRA
*&---------------------------------------------------------------------*

"
DATA: wl_setleaf TYPE setleaf,
      i_head     TYPE tbtcjob.

DATA:   wl_jobn(32).
DATA:   wa_mkpf     TYPE mkpf.
DATA:   tmseg       TYPE ty_t_mseg.


DATA: i_steplist TYPE TABLE OF tbtcstep,
      w_steplist TYPE tbtcstep.

DATA : c_no(1) TYPE c . "value 'N', " Criação do job

DATA: wl_tbtcjob  TYPE  tbtcjob,
      wl_tbtcstrt TYPE  tbtcstrt.

DATA lv_id_integr TYPE zcoupa_id_integr.
DATA wa_zintegrcoupa01 TYPE zintegrcoupa01.
DATA: lv_repname TYPE  rsvar-report.           " for variant handling
DATA: iv_varname TYPE  raldb-variant VALUE 'SAP_UPGRADE'.

DATA: iv_varianttext  TYPE  varit-vtext VALUE 'Upgrade variant'.
DATA: wl_subrc TYPE sy-subrc.
DATA: tt_reportparam TYPE TABLE OF  rsparams .

SELECT COUNT(*)
  FROM ekko
  FOR ALL ENTRIES IN xmseg
  WHERE  ekko~ebeln = xmseg-ebeln
  AND EXISTS ( SELECT *
                FROM  setleaf
                WHERE setname EQ 'MAGGI_EMPRESAS_COUPA'
                AND    valfrom = ekko~bukrs
                AND EXISTS ( SELECT *
                              FROM setleaf
                              WHERE setname EQ 'MAGGI_PEDIDOS_COUPA'
                              AND   valfrom = ekko~bsart ) ).

IF sy-subrc = 0.
  REFRESH: tt_reportparam,i_steplist.
  CLEAR: tt_reportparam, i_head.

  SELECT SINGLE *
   FROM setleaf
   INTO wl_setleaf
    WHERE setname EQ 'MAGGI_JOB_USER'.

  IF sy-subrc NE 0.
    MESSAGE text-e01 TYPE 'E'.
*    EXIT.
  ENDIF.
  READ TABLE xmkpf INTO wa_mkpf INDEX 1.
  CONCATENATE 'COUPAREC'  wa_mkpf-mblnr wa_mkpf-mjahr   INTO wl_jobn SEPARATED BY '|'.

  i_head-jobname = wl_jobn. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
  i_head-sdlstrttm = sy-uzeit + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
  i_head-sdlstrtdt = sy-datum.
  i_head-stepcount = 1.

  lv_repname = 'ZMMR0038'.
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

  w_steplist-parameter = iv_varname. " Nome da variante
  w_steplist-program = 'ZMMR0038'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
  w_steplist-typ = 'A'. " Tipo de Job
  w_steplist-authcknam = wl_setleaf-valfrom.
  w_steplist-language = sy-langu.
  w_steplist-arcuser = wl_setleaf-valfrom.

  APPEND w_steplist TO i_steplist.


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
