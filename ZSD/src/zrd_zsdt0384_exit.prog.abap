*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0381_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0384_exit.

FORM f_exit_zsdt0384_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0384 TYPE zsdt0384.

  CLEAR: wl_zsdt0384.

  wl_zsdt0384-user_create  = sy-uname.
  wl_zsdt0384-date_create  = sy-datum.
  wl_zsdt0384-time_create  = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0384 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0384_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zsdt0384 TYPE zsdt0384.
  CLEAR: wa_zsdt0384.

  DATA: var_answer TYPE c.

  DATA: lit_zsdt0384 TYPE TABLE OF zsdt0384.

  MOVE-CORRESPONDING p_registro_manter TO wa_zsdt0384.

  CLEAR: p_error.

  IF wa_zsdt0384-matkl IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Grupo de mercadorias campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zsdt0384-werks IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Centro campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zsdt0384-vkbur IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Escritório de vendas campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0384_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zsdt0384 TYPE zsdt0384.
  MOVE-CORRESPONDING p_registro_manter TO wa_zsdt0384.

  wa_zsdt0384-user_create  = sy-uname.
  wa_zsdt0384-date_create  = sy-datum.
  wa_zsdt0384-time_create  = sy-uzeit.

  MOVE-CORRESPONDING wa_zsdt0384 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0384_0008  CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0384_OUT' AND
     p_field       = 'MATKL'.
    p_scrtext_l    = 'Grupo de Mercadorias'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0384_OUT' AND
     p_field       = 'WERKS'.
    p_scrtext_l    = 'Centro'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0384_OUT' AND
     p_field       = 'VKBUR'.
    p_scrtext_l    = 'Escritório de Vendas'.
    p_outputlen    = 2.
  ENDIF.


ENDFORM.

FORM f_exit_zsdt0384_0013  TABLES p_tables.


ENDFORM.
