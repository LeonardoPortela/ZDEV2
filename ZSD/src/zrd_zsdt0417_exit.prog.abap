*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0417_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0417_exit.

FORM f_exit_zsdt0417_0001 CHANGING p_registro_manter TYPE any.


  DATA: ls_zsdt0417 TYPE zsdt0417_out.

  CLEAR: ls_zsdt0417.

  MOVE-CORRESPONDING p_registro_manter  TO ls_zsdt0417.

  TRY.
      ls_zsdt0417-guid = cl_system_uuid=>create_uuid_x16_static( ).
    CATCH cx_uuid_error.
      "handle exception
  ENDTRY.

  ls_zsdt0417-user_create    =  sy-uname.
  ls_zsdt0417-date_create    = sy-datum.
  ls_zsdt0417-time_create    = sy-uzeit.

  MOVE-CORRESPONDING ls_zsdt0417 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0417_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: ls_zsdt0417 TYPE zsdt0417_out.

  MOVE-CORRESPONDING p_registro_manter  TO ls_zsdt0417.

ENDFORM.

FORM f_exit_zsdt0417_0003 CHANGING p_registro_manter TYPE any.

  DATA: ls_zsdt0417 TYPE zsdt0417.

  CLEAR: ls_zsdt0417.
  MOVE-CORRESPONDING p_registro_manter  TO ls_zsdt0417.

  ls_zsdt0417-mandt =  sy-mandt.
  ls_zsdt0417-user_create =  sy-uname.
  ls_zsdt0417-date_create = sy-datum.
  ls_zsdt0417-time_create = sy-uzeit.

  MOVE-CORRESPONDING ls_zsdt0417 TO p_registro_manter.

  INSERT zsdt0417 FROM ls_zsdt0417.
  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.


ENDFORM.

FORM f_exit_zsdt0417_0004 CHANGING p_registro_manter TYPE any.
  DATA: ls_zsdt0417 TYPE zsdt0417_out.
  CLEAR ls_zsdt0417.

  MOVE-CORRESPONDING p_registro_manter  TO ls_zsdt0417.

  MOVE-CORRESPONDING ls_zsdt0417 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0417_0005 CHANGING p_registro_manter TYPE any.

ENDFORM.

FORM f_exit_zsdt0417_0006 USING p_saida TYPE any
                       CHANGING p_erro.

  DATA: wl_zsdt0417 TYPE zsdt0417,
        t_zsdt0417  TYPE TABLE OF zsdt0417.

  CLEAR wl_zsdt0417.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0417.

  UPDATE zsdt0417
    SET cancel      = abap_true
        date_cancel = sy-datum
        time_cancel = sy-uzeit
        user_cancel = sy-uname
    WHERE guid =  wl_zsdt0417-guid
      AND matkl = wl_zsdt0417-matkl
      AND mtart = wl_zsdt0417-mtart
      AND bukrs = wl_zsdt0417-bukrs.

  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.


FORM f_exit_zsdt0417_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.

ENDFORM.

FORM f_exit_zsdt0417_0009  TABLES pt_excl_toolbar
                            USING p_db_tab.

  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  FREE: it_excl_toolbar.

  wa_excl_toolbar-code = 'Modificar'.
  APPEND wa_excl_toolbar  TO it_excl_toolbar.

  pt_excl_toolbar[] = it_excl_toolbar[].

ENDFORM.
