*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0215_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zlest0215_exit.

FORM f_exit_zlest0215_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zlest0215 TYPE zlest0215.
  CLEAR:  wa_zlest0215.

  DATA: var_answer TYPE c.

  DATA: lit_zlest0215 TYPE TABLE OF zlest0215.

  MOVE-CORRESPONDING p_registro_manter TO wa_zlest0215.

  CLEAR: p_error.

*-CS2022000741-30.09.2024-#83334-JT-inicio
  IF wa_zlest0215-bukrs IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Empresa é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE bukrs
    INTO @DATA(_bukrs)
    FROM t001
   WHERE bukrs = @wa_zlest0215-bukrs.

  IF sy-subrc <> 0.
    p_error = abap_true.
    MESSAGE 'Empresa não Encontrada!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*-CS2022000741-30.09.2024-#83334-JT-fim

*-CS2022000741-30.09.2024-#83334-JT-inicio
  IF wa_zlest0215-id_processo IS INITIAL.
    p_error = abap_true.
    MESSAGE 'ID Processo é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0215-id_processo <> 'PIS_COF_PV_PF' AND
     wa_zlest0215-id_processo <> 'PIS_COF_PED'.
    p_error = abap_true.
    MESSAGE 'ID Processo informado incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*-CS2022000741-30.09.2024-#83334-JT-fim

*-CS2022000741-30.09.2024-#83334-JT-inicio
  IF wa_zlest0215-lifnr IS NOT INITIAL.
    SELECT SINGLE lifnr
      INTO @DATA(_lifnr)
      FROM lfa1
     WHERE lifnr = @wa_zlest0215-lifnr.

    IF sy-subrc <> 0.
      p_error = abap_true.
      MESSAGE 'Agente Frete não Encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

*  IF wa_zlest0215-lifnr IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Agente Frete é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*-CS2022000741-30.09.2024-#83334-JT-fim

  IF wa_zlest0215-id_processo = 'PIS_COF_PED' AND wa_zlest0215-kbert_pis IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Aliquota PIS é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0215-id_processo = 'PIS_COF_PED' AND wa_zlest0215-kbert_cofins IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Aliquota COFINS é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0215_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zlest0215 TYPE zlest0215.
  MOVE-CORRESPONDING p_registro_manter TO wa_zlest0215.

  wa_zlest0215-dt_registro = sy-datum.
  wa_zlest0215-hr_registro = sy-uzeit.
  wa_zlest0215-us_registro = sy-uname.

  MOVE-CORRESPONDING wa_zlest0215 TO p_registro_manter.

ENDFORM.

*-CS2022000741-30.09.2024-#83334-JT-inicio
FORM f_exit_zlest0215_0017 USING p_tipo.

  IF p_tipo = '0001'.
    PERFORM f4_val_param_espec USING '<FS_WA_REGISTRO_MANTER>-ID_PROCESSO'.
  ENDIF.

ENDFORM.

FORM f4_val_param_espec USING p_cod  TYPE help_info-dynprofld.

  TYPES: BEGIN OF ty_dados,
           id_processo TYPE zid_proc,
           descr       TYPE val_text,
         END   OF ty_dados.

*  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc,
        t_dados   TYPE TABLE OF ty_dados,
        w_dados   TYPE ty_dados,
        t_return  TYPE STANDARD TABLE OF ddshretval.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  FREE: t_dados.

  w_dados-id_processo  = 'PIS_COF_PV_PF'.
  w_dados-descr        = 'PIS/COFINS quando Parc.PV é Pessoa Fisica'.
  APPEND w_dados      TO t_dados.

  w_dados-id_processo  = 'PIS_COF_PED'.
  w_dados-descr        = 'PIS/COFINS sobre Pedagio'.
  APPEND w_dados      TO t_dados.

*  s_mapping-fldname    = 'F0001'.
*  s_mapping-dyfldname  = p_cod.
*  APPEND s_mapping    TO t_mapping.

*  s_mapping-fldname    = 'F0002'.
*  s_mapping-dyfldname  = 'DESCR'.
*  APPEND s_mapping    TO t_mapping.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ID_PROCESSO'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Processo'
      value_org       = 'S'
    TABLES
      value_tab       = t_dados
      return_tab      = t_return
*     dynpfld_mapping = t_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*-CS2022000741-30.09.2024-#83334-JT-fim

*-CS2022000741-30.09.2024-#83334-JT-inicio
FORM f_exit_zlest0215_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZLEST0215'
      tabfirst = 'X'.

ENDFORM.
*-CS2022000741-30.09.2024-#83334-JT-fim
