*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0271_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0271_exit.

FORM f_exit_zsdt0271_0001 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zsdt0271_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0271 TYPE zsdt0271_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0271.

  IF w_zsdt0271-filial IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Código da Filial.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zsdt0271-cod_regional IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Código da Regional.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t001w
           INTO @DATA(w_t001w)
          WHERE werks = @w_zsdt0271-filial.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Filial informada está incorreta.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM zsdt0270
           INTO @DATA(w_zsdt0270)
          WHERE cod_regional = @w_zsdt0271-cod_regional.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Regional informada está incorreta.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0271_0004 CHANGING p_registro_manter TYPE any.

  DATA: w_zsdt0271 TYPE zsdt0271_out.

  CLEAR w_zsdt0271.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0271.

  SELECT SINGLE *
           FROM t001w
           INTO @DATA(w_t001w)
          WHERE werks = @w_zsdt0271-filial.
  IF sy-subrc = 0.
    w_zsdt0271-desc_filial = w_t001w-name1.
  ENDIF.

  SELECT SINGLE *
           FROM zsdt0270
           INTO @DATA(w_zsdt0270)
          WHERE cod_regional = @w_zsdt0271-cod_regional.
  IF sy-subrc = 0.
    w_zsdt0271-desc_regional = w_zsdt0270-regional.
  ENDIF.

  MOVE-CORRESPONDING w_zsdt0271 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0271_0005 CHANGING p_registro_manter TYPE any.

  FIELD-SYMBOLS: <fs_name1> TYPE any,
                 <fs_name2> TYPE any.

  DATA: w_zsdt0271 TYPE zsdt0271_out.

  CLEAR w_zsdt0271.

  ASSIGN ('(ZREGISTER_DATA)DD03L-PRECFIELD') TO <fs_name1>.
  IF sy-subrc = 0.
    CLEAR <fs_name1>.
  ENDIF.

  ASSIGN ('(ZREGISTER_DATA)DD03L-REFFIELD') TO <fs_name2>.
  IF sy-subrc = 0.
    CLEAR <fs_name2>.
  ENDIF.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0271.

  SELECT SINGLE *
           FROM t001w
           INTO @DATA(w_t001w)
          WHERE werks = @w_zsdt0271-filial.
  IF sy-subrc = 0.
    ASSIGN ('(ZREGISTER_DATA)DD03L-PRECFIELD') TO <fs_name1>.
    IF sy-subrc = 0.
      <fs_name1> = w_t001w-name1.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
           FROM zsdt0270
           INTO @DATA(w_zsdt0270)
          WHERE cod_regional = @w_zsdt0271-cod_regional.
  IF sy-subrc = 0.
    ASSIGN ('(ZREGISTER_DATA)DD03L-REFFIELD') TO <fs_name2>.
    IF sy-subrc = 0.
      <fs_name2> =  w_zsdt0270-regional.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0271_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.
ENDFORM.

FORM f_exit_zsdt0271_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0271_OUT' AND
     p_field       = 'DESC_FILIAL'.
    p_scrtext_l = 'Descrição Filial'.
    p_outputlen = 60.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0271_OUT' AND
     p_field       = 'DESC_REGIONAL'.
    p_scrtext_l = 'Descrição Regional'.
    p_outputlen = 60.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0271_OUT' AND
     p_field       = 'COD_REGIONAL'.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0271_0009  TABLES it_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
