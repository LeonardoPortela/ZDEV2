*&---------------------------------------------------------------------*
*&  Include  ZRD_zlest0251_EXIT
*&---------------------------------------------------------------------*
REPORT zrd_zlest0251_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zlest0251_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0251 TYPE zlest0251.

  CLEAR: wl_zlest0251.

  wl_zlest0251-dt_registro = sy-datum.
  wl_zlest0251-hr_registro = sy-uzeit.
  wl_zlest0251-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zlest0251 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0251_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.

  DATA: w_zlest0251 TYPE zlest0251_out,
        lv_type     TYPE dd01v-datatype.

  CLEAR: lv_type.

  MOVE-CORRESPONDING p_registro_manter  TO w_zlest0251.

  IF w_zlest0251-nr_tag_strada IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Iformar Nro.TAG!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'NUMERIC_CHECK'
    EXPORTING
      string_in = w_zlest0251-nr_tag_strada
    IMPORTING
      htype     = lv_type.

  IF lv_type <> 'NUMC'.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'NRo.TAG deve conter somente Numeros!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE pc_veiculo
    INTO @DATA(_pc_veiculo)
    FROM zlest0002
   WHERE pc_veiculo = @w_zlest0251-pc_veiculo
     AND tp_veiculo = '0'.

  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Placa Informada Incorreta!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING w_zlest0251 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0251_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0251    TYPE zlest0251.

  CLEAR: wl_zlest0251.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zlest0251.

  wl_zlest0251-dt_registro = sy-datum.
  wl_zlest0251-hr_registro = sy-uzeit.
  wl_zlest0251-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zlest0251 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0251_0004 CHANGING p_saida TYPE any.

ENDFORM.

FORM f_exit_zlest0251_0005 CHANGING p_registro_manter TYPE any.

  DATA: w_zlest0251   TYPE zlest0251_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zlest0251.

ENDFORM.

FORM f_exit_zlest0251_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

  DATA: w_zlest0251   TYPE zlest0251_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zlest0251.

  UPDATE zlest0002 SET nr_tag_strada = abap_off
                 WHERE pc_veiculo    = w_zlest0251-pc_veiculo.

ENDFORM.

FORM f_exit_zlest0251_0009  TABLES it_excl_toolbar
                           USING p_db_tab.

  APPEND 'Deletar'    TO it_excl_toolbar.

ENDFORM.


FORM f_exit_zlest0251_0011 USING p_registro_manter TYPE any
                       CHANGING p_erro.

  DATA: w_zlest0251   TYPE zlest0251_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zlest0251.

  UPDATE zlest0002 SET nr_tag_strada = w_zlest0251-nr_tag_strada
                 WHERE pc_veiculo    = w_zlest0251-pc_veiculo.

ENDFORM.

FORM f_exit_zlest0251_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'zlest0251_OUT' AND "CS2023000070 - DEVK9A1RRV
     p_field       = 'PC_VEICULO'.
    p_scrtext_l = 'Placa Veículo'.
    p_outputlen = 25.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0251_OUT' AND
     p_field       = 'NR_TAG_STRADA'.
    p_scrtext_l = 'Nr.Tag Strada Pass'.
    p_outputlen = 30.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0251_OUT' AND
     p_field       = 'DT_REGISTRO'.
    p_scrtext_l = 'Data Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0251_OUT' AND
     p_field       = 'HR_REGISTRO'.
    p_scrtext_l = 'Hora Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0251_OUT' AND
     p_field       = 'US_REGISTRO'.
    p_scrtext_l = 'Usuario Registro'.
    p_outputlen = 40.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0251_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZLEST0251'
      tabfirst = 'X'.

ENDFORM.

FORM f_exit_zlest0251_0017 USING p_tipo.

  IF p_tipo = '0001'.
    PERFORM f4_val_valida USING '<FS_WA_REGISTRO_MANTER>-VALIDA'.
  ENDIF.

ENDFORM.

FORM f4_val_valida USING p_cod TYPE help_info-dynprofld.

  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.

*====>  Tabelas internas
  DATA: BEGIN OF t_val OCCURS 0,
          pc_veiculo TYPE zlest0002-pc_veiculo,
          ct_veiculo TYPE zlest0002-ct_veiculo,
          chassi     TYPE zlest0002-chassi,
          modelo     TYPE zlest0002-modelo,
        END OF t_val.

  CLEAR t_return.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  SELECT *
    FROM zlest0002
    INTO TABLE @DATA(t_0002)
    WHERE tp_veiculo = '0'.

  CHECK sy-subrc = 0.

  LOOP AT t_0002 INTO DATA(w_0002).
    t_val-pc_veiculo     = w_0002-pc_veiculo.
    t_val-ct_veiculo     = w_0002-ct_veiculo.
    t_val-chassi         = w_0002-chassi.
    t_val-modelo         = w_0002-modelo.
    APPEND t_val.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PC_VEICULO'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Placa Veiculo'
      value_org       = 'S'
    TABLES
      value_tab       = t_val
      return_tab      = t_return
      dynpfld_mapping = t_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
