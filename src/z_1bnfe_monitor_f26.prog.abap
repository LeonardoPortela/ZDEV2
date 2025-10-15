*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F26
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  grid_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grid_update USING p_mode TYPE any.

  DATA lv_mode TYPE c.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  DATA: gv_par    TYPE syst_subrc,
        gv_screen TYPE char1,
        lc_nast   TYPE nast,
        it_otf    TYPE tsfotf.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

* Update IT_NFE_ALV and IT_NFE_ALV2
  LOOP AT it_active_mod INTO wa_active_mod.

    CLEAR wa_nfe_alv.
    READ TABLE it_nfe_alv INTO wa_nfe_alv
      WITH TABLE KEY docnum = wa_active_mod-docnum.
* - Set date, time & user ------------------------
    IF sy-datlo IS INITIAL.
      wa_active_mod-action_date = sy-datum.
    ELSE.
      wa_active_mod-action_date = sy-datlo.
    ENDIF.
    IF sy-timlo IS INITIAL.
      wa_active_mod-action_time = sy-uzeit.
    ELSE.
      wa_active_mod-action_time = sy-timlo.
    ENDIF.
    wa_active_mod-action_user = sy-uname.
* - Update ALV2
    MOVE-CORRESPONDING wa_active_mod TO wa_nfe_alv2_new.
* - Update ALV
    MOVE-CORRESPONDING wa_active_mod TO wa_nfe_alv.
    MODIFY it_nfe_alv FROM wa_nfe_alv INDEX sy-tabix.

    IF wa_active_mod-docnum = gf_docnum.
      IF p_mode = 'RESET'.
        lv_mode = c_x.
      ENDIF.
      PERFORM alv_history_update USING lv_mode.
    ENDIF.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    SELECT SINGLE contingencia
      INTO @DATA(lv_contingencia)
      FROM zsdt0102
     WHERE docnum = @wa_active_mod-docnum.

    IF sy-subrc = 0 AND lv_contingencia = abap_true AND ok_code = 'SET_NUM3'.
      lc_nast-objky   = wa_active_mod-docnum.
      lc_nast-tdarmod = '9'.
      PERFORM entry2 IN PROGRAM zbrmdfe_damdfe_nast USING gv_par gv_screen lc_nast CHANGING it_otf.
    ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

  ENDLOOP.

* Set action indicator and status icon
  PERFORM set_status_info.
* Check for application-log entries and set error-log icons
  PERFORM read_error_log USING 'ALL'.

  IF vg_faturamento_autom = abap_off.                       "FF #170994
    PERFORM set_screen_status_100.
  ENDIF.

  PERFORM set_log_icons.
* Update display of ALV

  IF vg_faturamento_autom = abap_off.                       "FF #170994
    CALL METHOD ctl_alv_nfe->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = c_x.
    CALL METHOD cl_gui_cfw=>flush.
    PERFORM set_scroll_info_via_id USING c_100.
  ENDIF.
ENDFORM.                    " grid_update
*&---------------------------------------------------------------------*
*&      Form  alv_history_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_history_update USING p_mode TYPE c.

* Add status texts to ALV list
  READ TABLE it_scs_text INTO wa_scs_text
    WITH KEY domvalue_l = wa_nfe_alv2_new-scssta.
  MOVE wa_scs_text-ddtext TO wa_nfe_alv2_new-scsstat.
  READ TABLE it_doc_text INTO wa_doc_text
    WITH KEY domvalue_l = wa_nfe_alv2_new-docsta.
  MOVE wa_doc_text-ddtext TO wa_nfe_alv2_new-docstat.
  IF NOT wa_nfe_alv2_new-code IS INITIAL.
    READ TABLE it_code_text INTO wa_code_text
      WITH KEY code       = wa_nfe_alv2_new-code.
    MOVE wa_code_text-text TO wa_nfe_alv2_new-codet.
  ENDIF.

  IF p_mode IS INITIAL.
    APPEND wa_nfe_alv2_new TO it_nfe_alv2.
  ELSE.
* Reversing contingency switch
    LOOP AT it_nfe_alv2 INTO wa_nfe_alv2.
      IF wa_nfe_alv2-scssta = '5'.
        DELETE it_nfe_alv2 INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Sort ALV list
  SORT it_nfe_alv2 DESCENDING BY action_date action_time.

* Update ALV distplay

  IF vg_faturamento_autom = abap_off.                       "FF #170994
    CALL METHOD ctl_alv_nfe_hist->refresh_table_display.
  ENDIF.

ENDFORM.                    " alv_history_update
