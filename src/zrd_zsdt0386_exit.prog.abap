*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0381_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0386_exit.

FORM f_exit_zsdt0386_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0386 TYPE zsdt0386.

  CLEAR: wl_zsdt0386.

  wl_zsdt0386-user_create  = sy-uname.
  wl_zsdt0386-date_create  = sy-datum.
  wl_zsdt0386-time_create  = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0386 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0386_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zsdt0386 TYPE zsdt0386.
  CLEAR: wa_zsdt0386.

  DATA: var_answer TYPE c.

  DATA: lit_zsdt0386 TYPE TABLE OF zsdt0386.

  MOVE-CORRESPONDING p_registro_manter TO wa_zsdt0386.

  CLEAR: p_error.

  IF wa_zsdt0386-lifnr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Ponto de coleta campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0386_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zsdt0386 TYPE zsdt0386.
  MOVE-CORRESPONDING p_registro_manter TO wa_zsdt0386.

  wa_zsdt0386-user_create  = sy-uname.
  wa_zsdt0386-date_create  = sy-datum.
  wa_zsdt0386-time_create  = sy-uzeit.

  MOVE-CORRESPONDING wa_zsdt0386 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0386_0008  CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0386_OUT' AND
     p_field       = 'LIFNR'.
    p_scrtext_l    = 'Ponto de coleta'.
    p_outputlen    = 10.
  ENDIF.


ENDFORM.

FORM f_exit_zsdt0386_0013  TABLES p_tables.


ENDFORM.
