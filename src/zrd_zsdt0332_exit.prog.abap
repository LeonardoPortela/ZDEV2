*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0332_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0332_exit.

FORM f_exit_zsdt0332_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0332 TYPE zsdt0332.

  CLEAR: wl_zsdt0332.

  wl_zsdt0332-datum_reg = sy-datum.
  wl_zsdt0332-uzeit_reg = sy-uzeit.
  wl_zsdt0332-usnam_reg = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0332 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0332_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0332 TYPE zsdt0332_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0332.

  IF w_zsdt0332-safra < '2020'.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Safra informada está incorreta.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t001w
           INTO @DATA(w_t001w)
          WHERE werks = @w_zsdt0332-werks.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Centro informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t001l
           INTO @DATA(w_t001l)
          WHERE werks = @w_zsdt0332-werks
            AND lgort = @w_zsdt0332-lgort_origem_ini.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Depósito DE informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t001l
           INTO w_t001l
          WHERE werks = w_zsdt0332-werks
            AND lgort = w_zsdt0332-lgort_origem_fim.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Depósito ATÉ informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t001l
           INTO w_t001l
          WHERE werks = w_zsdt0332-werks
            AND lgort = w_zsdt0332-lgort_destino.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Depósito Destino informado está incorreto.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zsdt0332-lgort_origem_ini > w_zsdt0332-lgort_origem_fim.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Depósito "DE" maior que Centro "ATÉ".'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT *
    FROM zsdt0332
    INTO TABLE @DATA(t_0332)
   WHERE werks = @w_zsdt0332-werks
     AND safra = @w_zsdt0332-safra.

  DATA(l_erro) = abap_false.

  LOOP AT t_0332 INTO DATA(w_0332).
    IF   w_zsdt0332-lgort_origem_ini >= w_0332-lgort_origem_ini AND
         w_zsdt0332-lgort_origem_ini <= w_0332-lgort_origem_fim.
      l_erro = abap_true.
      EXIT.
    ENDIF.
    IF   w_zsdt0332-lgort_origem_fim   >= w_0332-lgort_origem_ini AND
         w_zsdt0332-lgort_origem_fim   <= w_0332-lgort_origem_fim.
      l_erro = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF l_erro = abap_true.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Há sobreposição de Depósitos!'
                     DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0332_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0332    TYPE zsdt0332.

  CLEAR: wl_zsdt0332.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zsdt0332.

  wl_zsdt0332-datum_reg = sy-datum.
  wl_zsdt0332-uzeit_reg = sy-uzeit.
  wl_zsdt0332-usnam_reg = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0332 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0332_0004 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zsdt0332_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zsdt0332_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

*  DATA: w_zsdt0332     TYPE zsdt0332_out.
*
*  MOVE-CORRESPONDING p_registro_manter TO w_zsdt0332.
*  MOVE-CORRESPONDING w_zsdt0332 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0332_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0332_OUT' AND
     p_field       = 'WERKS'.
    p_scrtext_l = 'Centro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0332_OUT' AND
     p_field       = 'SAFRA'.
    p_scrtext_l = 'Safra'.
    p_outputlen = 10.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0332_OUT' AND
     p_field       = 'LGORT_ORIGEM_INI'.
    p_scrtext_l = 'Depósito De'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0332_OUT' AND
     p_field       = 'LGORT_ORIGEM_FIM'.
    p_scrtext_l = 'Depósito Até'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0332_OUT' AND
     p_field       = 'LGORT_DESTINO'.
    p_scrtext_l = 'Depósito Destino'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0332_OUT' AND
     p_field       = 'USNAM_REG'.
    p_scrtext_l = 'Usuário Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0332_OUT' AND
     p_field       = 'DATUM_REG'.
    p_scrtext_l = 'Data Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0332_OUT' AND
     p_field       = 'UZEIT_REG'.
    p_scrtext_l = 'Hora Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0332_0009  TABLES pt_excl_toolbar
                            USING p_db_tab.

  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  CHECK p_db_tab = 'ZSDT0332'.

  it_excl_toolbar[] = pt_excl_toolbar[].

  wa_excl_toolbar-code = 'Modificar'.
  APPEND wa_excl_toolbar TO it_excl_toolbar.

  pt_excl_toolbar[] = it_excl_toolbar[].

ENDFORM.


FORM f_exit_zsdt0332_0017 USING p_tipo.

  FIELD-SYMBOLS: <f_werks> TYPE any.

  TYPES: BEGIN OF ty_t001l,
           werks TYPE t001l-werks,
           lgort TYPE t001l-lgort,
           lgobe TYPE t001l-lgobe,
         END OF ty_t001l.

  DATA: t_t001l TYPE TABLE OF t001l,
        w_t001l TYPE t001l,
        t_data  TYPE TABLE OF ty_t001l,
        w_data  TYPE ty_t001l.

  ASSIGN ('(ZREGISTER_DATA)<fs_wa_registro_manter>-WERKS') TO <f_werks>.
  CHECK sy-subrc = 0.

  REFRESH: t_t001l.

  SELECT * FROM t001l
           INTO TABLE t_t001l
          WHERE werks = <f_werks>.

  LOOP AT t_t001l INTO w_t001l.
    MOVE-CORRESPONDING w_t001l TO w_data.
    APPEND w_data   TO t_data.
  ENDLOOP.

  CASE p_tipo.
    WHEN '0001'.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          pvalkey          = ' '
          retfield         = 'LGORT'
          dynpprog         = sy-repid
          dynpnr           = sy-dynnr
          dynprofield      = '<FS_WA_REGISTRO_MANTER>-LGORT_ORIGEM_INI'
          callback_program = sy-repid
          value_org        = 'S'
        TABLES
          value_tab        = t_data
        EXCEPTIONS
          parameter_error  = 1
          no_values_found  = 2
          OTHERS           = 3.

    WHEN '0002'.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          pvalkey          = ' '
          retfield         = 'LGORT'
          dynpprog         = sy-repid
          dynpnr           = sy-dynnr
          dynprofield      = '<FS_WA_REGISTRO_MANTER>-LGORT_ORIGEM_FIM'
          callback_program = sy-repid
          value_org        = 'S'
        TABLES
          value_tab        = t_data
        EXCEPTIONS
          parameter_error  = 1
          no_values_found  = 2
          OTHERS           = 3.

    WHEN '0003'.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          pvalkey          = ' '
          retfield         = 'LGORT'
          dynpprog         = sy-repid
          dynpnr           = sy-dynnr
          dynprofield      = '<FS_WA_REGISTRO_MANTER>-LGORT_DESTINO'
          callback_program = sy-repid
          value_org        = 'S'
        TABLES
          value_tab        = t_data
        EXCEPTIONS
          parameter_error  = 1
          no_values_found  = 2
          OTHERS           = 3.

  ENDCASE.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    "BREAK-POINT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
