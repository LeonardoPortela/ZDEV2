*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0372_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0372_exit.

FORM f_exit_zsdt0372_0001 CHANGING p_registro_manter TYPE any.


  DATA: w_zsdt0372 TYPE zsdt0372.

  CLEAR: w_zsdt0372.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0372.
  w_zsdt0372-user_create    =  sy-uname.
  w_zsdt0372-date_create    = sy-datum.
  w_zsdt0372-time_create    = sy-uzeit.


  MOVE-CORRESPONDING w_zsdt0372 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0372_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0372 TYPE zsdt0372.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0372.

  IF w_zsdt0372-valorlimite IS INITIAL.

    p_erro = abap_true.

    MESSAGE s024(sd) WITH 'Informar valor'
                     DISPLAY LIKE 'E'.

    EXIT.

  ENDIF.

ENDFORM.

FORM f_exit_zsdt0372_0003 CHANGING p_registro_manter TYPE any.

  FIELD-SYMBOLS <fs_table> TYPE ANY TABLE.
  DATA w_zsdt0372 TYPE zsdt0372.
  DATA lv_matnr TYPE matnr18.

  CLEAR: w_zsdt0372.
  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0372.

  ASSIGN ('(ZREGISTER_DATA)<FS_IT_SAIDA>') TO <fs_table>.

  DATA(lv_lines) = lines( <fs_table> ).

  IF lv_lines > 0.
    w_zsdt0372-user_change    =  sy-uname.
    w_zsdt0372-date_change    = sy-datum.
    w_zsdt0372-time_change    = sy-uzeit.
  ENDIF.

  MOVE-CORRESPONDING w_zsdt0372 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0372_0004 CHANGING p_registro_manter TYPE any.

  DATA w_zsdt0372 TYPE zsdt0372.

  CLEAR w_zsdt0372.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0372.

  MOVE-CORRESPONDING w_zsdt0372 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0372_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0372_out TYPE zsdt0372.

  CLEAR: wl_zsdt0372_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0372_out.

  MOVE-CORRESPONDING wl_zsdt0372_out TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0372_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zsdt0372_0008 CHANGING p_col_pos
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

  IF p_field = 'VALORLIMITE'.p_scrtext_l = 'Valor'. ENDIF.
  IF p_field = 'EMAIL_01'.p_scrtext_l = 'E-mail 01'. ENDIF.
  IF p_field = 'EMAIL_02'.p_scrtext_l = 'E-mail 02'. ENDIF.
  IF p_field = 'EMAIL_03'.p_scrtext_l = 'E-mail 03'. ENDIF.
  IF p_field = 'EMAIL_04'.p_scrtext_l = 'E-mail 04'. ENDIF.

ENDFORM.

FORM f_exit_zsdt0372_0009  TABLES it_excl_toolbar
                            USING p_db_tab.

  FIELD-SYMBOLS <fs_table> TYPE ANY TABLE.

  ASSIGN ('(ZREGISTER_DATA)<FS_IT_SAIDA>') TO <fs_table>.

  DATA(lv_lines) = lines( <fs_table> ).

  IF lv_lines > 0.
    APPEND 'Novo' TO it_excl_toolbar.
  ENDIF.


ENDFORM.
