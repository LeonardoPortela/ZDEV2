*&---------------------------------------------------------------------*
*&  Include  ZRD_ZLEST0245_EXIT
*&---------------------------------------------------------------------*
REPORT zrd_zlest0245_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zlest0245_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0245 TYPE zlest0245.

  CLEAR: wl_zlest0245.

  wl_zlest0245-data_reg    = sy-datum.
  wl_zlest0245-hora_reg    = sy-uzeit.
  wl_zlest0245-user_reg    = sy-uname.

  MOVE-CORRESPONDING wl_zlest0245 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0245_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.

  DATA: w_zlest0245   TYPE zlest0245_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zlest0245.

  SELECT SINGLE *
           FROM t005s
           INTO @DATA(_t005s)
          WHERE land1 = 'BR'
            AND bland = @w_zlest0245-uf_ini.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'UF Inicial Incorreta!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t005s
           INTO _t005s
          WHERE land1 = 'BR'
            AND bland = w_zlest0245-uf_fim.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'UF Final Incorreta!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
           FROM t005s
           INTO _t005s
          WHERE land1 = 'BR'
            AND bland = w_zlest0245-uf_intermediaria.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'UF Intermediaria Incorreta!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING w_zlest0245 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0245_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0245    TYPE zlest0245.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zlest0245.

  wl_zlest0245-data_reg    = sy-datum.
  wl_zlest0245-hora_reg    = sy-uzeit.
  wl_zlest0245-user_reg    = sy-uname.

  MOVE-CORRESPONDING wl_zlest0245 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0245_0004 CHANGING p_saida TYPE any.

  DATA: wl_zlest0245  TYPE zlest0245_out.

  CLEAR: wl_zlest0245.

  MOVE-CORRESPONDING p_saida  TO wl_zlest0245.

  MOVE-CORRESPONDING wl_zlest0245 TO p_saida.

ENDFORM.

FORM f_exit_zlest0245_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0245    TYPE zlest0245.

  CLEAR: wl_zlest0245.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zlest0245.
  MOVE-CORRESPONDING wl_zlest0245 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0245_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zlest0245_0008 CHANGING p_col_pos
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


  IF p_ref_tabname = 'ZLEST0245_OUT' AND
     p_field       = 'UF_INI'.
    p_scrtext_l = 'UF Inicial'.
    p_outputlen = 12.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0245_OUT' AND
     p_field       = 'UF_FIM'.
    p_scrtext_l = 'UF Final'.
    p_outputlen = 12.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0245_OUT' AND
     p_field       = 'ORDEM'.
    p_scrtext_l = 'Ordem'.
    p_outputlen = 10.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0245_OUT' AND
     p_field       = 'UF_INTERMEDIARIA'.
    p_scrtext_l = 'UF Intermediária'.
    p_outputlen = 18.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0245_OUT' AND
     p_field       = 'USER_REG'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0245_OUT' AND
     p_field       = 'DATA_REG'.
    p_scrtext_l = 'Data'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0245_OUT' AND
     p_field       = 'HORA_REG'.
    p_scrtext_l = 'Hora'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0245_0009  TABLES it_excl_toolbar
                           USING p_db_tab.

* APPEND 'Modificar'    TO it_excl_toolbar.

ENDFORM.


FORM f_exit_zlest0245_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZLEST0245'
      tabfirst = 'X'.

ENDFORM.

FORM f_exit_zlest0245_0017 USING p_tipo.

ENDFORM.

FORM f4_val_valida USING p_cod TYPE help_info-dynprofld.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
