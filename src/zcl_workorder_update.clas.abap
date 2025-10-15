CLASS zcl_workorder_update DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_workorder_update .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_workorder_update IMPLEMENTATION.


  METHOD if_ex_workorder_update~archive_objects.
  ENDMETHOD.


  METHOD if_ex_workorder_update~at_deletion_from_database.
  ENDMETHOD.


  METHOD if_ex_workorder_update~at_release.


  ENDMETHOD.


  METHOD if_ex_workorder_update~at_save.

    DATA: wl_setleaf TYPE setleaf,
          i_head     TYPE tbtcjob.

    DATA:   wl_jobn(32).

    DATA: i_steplist TYPE STANDARD TABLE OF tbtcstep WITH EMPTY KEY.
    DATA: wa_steplist TYPE tbtcstep.
    DATA : c_no(1) TYPE c . "value 'N', " Criação do job

    DATA: wl_tbtcjob  TYPE  tbtcjob,
          wl_tbtcstrt TYPE  tbtcstrt.

    DATA: lv_repname      TYPE  rsvar-report.           " for variant handling
    DATA: iv_varname      TYPE  raldb-variant VALUE 'SAP_UPGRADE'.
    DATA: iv_varianttext  TYPE  varit-vtext VALUE 'Upgrade variant'.
    DATA: wl_subrc        TYPE sy-subrc.
    DATA: tt_reportparam  TYPE STANDARD TABLE OF rsparams WITH EMPTY KEY.
    DATA: ls_caufv_old    TYPE caufvdb,
          lv_new_released TYPE c.

    IF is_header_dialog-aufnr IS NOT INITIAL.
      CLEAR: ls_caufv_old.

      CALL FUNCTION 'CO_BT_CAUFV_OLD_READ_WITH_KEY'
        EXPORTING
          aufnr_act     = is_header_dialog-aufnr
        IMPORTING
          caufv_old_exp = ls_caufv_old
        EXCEPTIONS
          not_found     = 1
          OTHERS        = 2.

    ENDIF.

    CHECK ls_caufv_old IS NOT INITIAL.

    FIND 'LIB' IN is_header_dialog-sttxt.
    IF sy-subrc = 0.
      lv_new_released = 'X'.
    ENDIF.

    FIND 'LIB' IN ls_caufv_old-sttxt.
    IF sy-subrc = 0.
      CLEAR lv_new_released.
    ENDIF.

    IF lv_new_released IS NOT INITIAL AND is_header_dialog-aufnr IS NOT INITIAL.

      REFRESH: tt_reportparam,i_steplist.
      CLEAR: tt_reportparam, i_head.

      SELECT SINGLE *
       FROM setleaf
       INTO wl_setleaf
        WHERE setname EQ 'MAGGI_JOB_USER'.

      IF sy-subrc NE 0.
        MESSAGE TEXT-e01 TYPE 'E'.
*  EXIT.
      ENDIF.
      CONCATENATE 'COUPAORPM' is_header_dialog-aufnr  INTO wl_jobn SEPARATED BY '|'.

      i_head-jobname    = wl_jobn. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
      i_head-sdlstrtdt  = sy-datum.
      i_head-sdlstrttm  = sy-uzeit + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
      i_head-stepcount  = 1.
      lv_repname        = 'ZMMR0035'.

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

      wa_steplist-parameter  = iv_varname. " Nome da variante
      wa_steplist-program    = 'ZMMR0035'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
      wa_steplist-typ        = 'A'. " Tipo de Job
      wa_steplist-authcknam  = wl_setleaf-valfrom.
      wa_steplist-language   = sy-langu.
      wa_steplist-arcuser    = wl_setleaf-valfrom.

      APPEND wa_steplist TO i_steplist.

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



  ENDMETHOD.


  METHOD if_ex_workorder_update~before_update.

    " FF #186238 - início
    IF sy-tcode = 'CO01'.
      DATA caufvd TYPE caufvd.

      TYPES: BEGIN OF ty_tvarvc_entry,
               werks TYPE werks_d,
               auart TYPE aufart,
             END OF ty_tvarvc_entry.

      TYPES: ty_tvarvc_tab TYPE SORTED TABLE OF ty_tvarvc_entry WITH UNIQUE KEY werks auart.

      DATA: lt_tvarvc TYPE ty_tvarvc_tab.

      SELECT low AS werks, high AS auart
        FROM tvarvc
        INTO TABLE @lt_tvarvc
        WHERE name = 'Z_ATRIBUI_CENTRO_TP_ORDEM'.

      READ TABLE it_header INTO DATA(is_header_dialog) INDEX 1.

      DATA(ls_match) = FILTER #( lt_tvarvc
        WHERE werks = is_header_dialog-werks AND auart = is_header_dialog-auart ).

      IF lines( ls_match ) = 0.
        RETURN. " Não autorizado
      ENDIF.

      DATA: lv_material TYPE bapibatchkey-material,
            lv_batch    TYPE bapibatchkey-batch,
            lv_plant    TYPE bapibatchkey-plant.

      READ TABLE it_item INTO DATA(wa_item) INDEX 1.

      lv_material = is_header_dialog-plnbez.
      lv_batch = wa_item-charg. "A definição do número do lote está no enhancement --> ZPP_ENH_COKO1_0115_LOCK_CHARG
      lv_plant = is_header_dialog-werks.

      "Verifica se o número do lote já existe, e caso afirmativo, gerar outro código.

      SELECT SINGLE charg
      FROM mcha INTO @DATA(lv_dummy)
      WHERE charg = @lv_batch.

      IF sy-subrc = 0. "Lote já utilizado

        MOVE-CORRESPONDING is_header_dialog TO caufvd.

        lv_batch = zcl_pp_util=>get_lote_codigo( is_caufvd = caufvd ).

        ASSIGN ('(SAPLCOBT)AFPO_BT[]') TO FIELD-SYMBOL(<lt_afpo_bt>).

        LOOP AT <lt_afpo_bt> ASSIGNING FIELD-SYMBOL(<afpo>).

          ASSIGN COMPONENT 'CHARG' OF STRUCTURE <afpo> TO FIELD-SYMBOL(<fs_charg>).
          IF <fs_charg> IS ASSIGNED AND lv_batch IS NOT INITIAL.
            <fs_charg> = lv_batch.
          ENDIF.
          EXIT. " já achou a única linha

        ENDLOOP.

*      <fs>-charg = lv_lote.

      ENDIF.

      CALL FUNCTION 'Z_CREATE_BATCH_UPD' IN UPDATE TASK
        EXPORTING
          material = lv_material
          batch    = lv_batch
          plant    = lv_plant
          aufnr    = is_header_dialog-aufnr.
    ENDIF.

    " FF #186238 - fim

  ENDMETHOD.


  METHOD if_ex_workorder_update~cmts_check.
  ENDMETHOD.


  METHOD if_ex_workorder_update~initialize.
  ENDMETHOD.


  METHOD if_ex_workorder_update~in_update.




  ENDMETHOD.


  METHOD if_ex_workorder_update~number_switch.
  ENDMETHOD.


  METHOD if_ex_workorder_update~reorg_status_activate.
  ENDMETHOD.


  METHOD if_ex_workorder_update~reorg_status_act_check.
  ENDMETHOD.


  METHOD if_ex_workorder_update~reorg_status_revoke.
  ENDMETHOD.
ENDCLASS.
