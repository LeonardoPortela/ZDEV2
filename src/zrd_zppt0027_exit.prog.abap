*&---------------------------------------------------------------------*
*& Report  ZRD_ZPPT0027_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zppt0027_exit.

FORM f_exit_zppt0027_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt0027 TYPE zppt0027.

  CLEAR: wl_zppt0027.

  wl_zppt0027-datum_reg = sy-datum.
  wl_zppt0027-uzeit_reg = sy-uzeit.
  wl_zppt0027-usnam_reg = sy-uname.

  MOVE-CORRESPONDING wl_zppt0027 TO p_registro_manter.

ENDFORM.

FORM f_exit_zppt0027_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zppt0027 TYPE zppt0027_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zppt0027.

  SELECT SINGLE *
           FROM usr02
           INTO @DATA(w_usr02)
          WHERE bname = @w_zppt0027-usuario.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Usuário informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t001w
           INTO @DATA(w_t001w)
          WHERE werks = @w_zppt0027-werks_from.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Centro "DE" informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t001w
           INTO @DATA(w_t001wb)
          WHERE werks = @w_zppt0027-werks_to.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Centro "ATÉ" informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zppt0027-werks_from > w_zppt0027-werks_to.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Centro "DE" maior que Centro "ATÉ".'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT *
    FROM zppt0027
    INTO TABLE @DATA(t_0027)
   WHERE usuario = @w_zppt0027-usuario.

  DATA(l_erro) = abap_false.

  LOOP AT t_0027 INTO DATA(w_0027).
    IF   w_zppt0027-werks_from >= w_0027-werks_from AND
         w_zppt0027-werks_from <= w_0027-werks_to.
      l_erro = abap_true.
      EXIT.
    ENDIF.
    IF   w_zppt0027-werks_to   >= w_0027-werks_from AND
         w_zppt0027-werks_to   <= w_0027-werks_to.
      l_erro = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF l_erro = abap_true.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Há sobreposição de Centros!'
                     DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM f_exit_zppt0027_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt0027    TYPE zppt0027,
        w_zppt0027_log TYPE zppt0027_log,
        l_seq          TYPE timestampl.

  CLEAR: wl_zppt0027.

  GET TIME STAMP FIELD l_seq.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zppt0027.

  wl_zppt0027-datum_reg = sy-datum.
  wl_zppt0027-uzeit_reg = sy-uzeit.
  wl_zppt0027-usnam_reg = sy-uname.

  MOVE-CORRESPONDING wl_zppt0027        TO w_zppt0027_log.

  w_zppt0027_log-seq       = l_seq.
  w_zppt0027_log-acao      = 'I'.
  w_zppt0027_log-datum_reg = sy-datum.
  w_zppt0027_log-uzeit_reg = sy-uzeit.
  w_zppt0027_log-usnam_reg = sy-uname.

  MODIFY zppt0027_log FROM w_zppt0027_log.
  COMMIT WORK.

  MOVE-CORRESPONDING wl_zppt0027 TO p_registro_manter.

ENDFORM.

FORM f_exit_zppt0027_0004 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zppt0027_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zppt0027_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

  DATA: l_seq  TYPE timestampl.

  DATA: w_zppt0027     TYPE zppt0027_out,
        w_zppt0027_log TYPE zppt0027_log.

  GET TIME STAMP FIELD l_seq.

  MOVE-CORRESPONDING p_registro_manter TO w_zppt0027.
  MOVE-CORRESPONDING w_zppt0027        TO w_zppt0027_log.

  w_zppt0027_log-seq       = l_seq.
  w_zppt0027_log-acao      = 'E'.
  w_zppt0027_log-datum_reg = sy-datum.
  w_zppt0027_log-uzeit_reg = sy-uzeit.
  w_zppt0027_log-usnam_reg = sy-uname.

  MODIFY zppt0027_log FROM w_zppt0027_log.
  COMMIT WORK.

  MOVE-CORRESPONDING w_zppt0027 TO p_registro_manter.

ENDFORM.

FORM f_exit_zppt0027_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZPPT0027_OUT' AND
     p_field       = 'USUARIO'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_OUT' AND
     p_field       = 'WERKS_FROM'.
    p_scrtext_l = 'Centro De'.
    p_outputlen = 10.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_OUT' AND
     p_field       = 'WERKS_TO'.
    p_scrtext_l = 'Centro Até'.
    p_outputlen = 10.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_OUT' AND
     p_field       = 'USNAM_REG'.
    p_scrtext_l = 'Usuário Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_OUT' AND
     p_field       = 'DATUM_REG'.
    p_scrtext_l = 'Data Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0027_OUT' AND
     p_field       = 'UZEIT_REG'.
    p_scrtext_l = 'Hora Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zppt0027_0009  TABLES pt_excl_toolbar
                            USING p_db_tab.

  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  CHECK p_db_tab = 'ZPPT0027'.

  it_excl_toolbar[] = pt_excl_toolbar[].

  wa_excl_toolbar-code = 'Modificar'.
  APPEND wa_excl_toolbar TO it_excl_toolbar.

  pt_excl_toolbar[] = it_excl_toolbar[].

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
