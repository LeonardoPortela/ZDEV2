*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0167_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0167_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zmmt0167_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0167 TYPE zmmt0167.

  CLEAR: wl_zmmt0167.

  wl_zmmt0167-data_registro = sy-datum.
  wl_zmmt0167-hora_registro = sy-uzeit.
  wl_zmmt0167-usuario = sy-uname.

  MOVE-CORRESPONDING wl_zmmt0167 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0167_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zmmt0167 TYPE zmmt0167.

  CLEAR: wl_zmmt0167.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0167.

  CLEAR: p_error.

  IF  wl_zmmt0167-branch IS INITIAL OR wl_zmmt0167-lgort IS INITIAL.

    p_error = abap_true.
    MESSAGE s032(15) DISPLAY LIKE 'E'. "Dados incompletos; preencher todos os campos
    EXIT.

  ENDIF.

ENDFORM.

FORM f_exit_zmmt0167_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0167 TYPE zmmt0167.

  CLEAR: wl_zmmt0167.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0167.

  wl_zmmt0167-data_registro = sy-datum.
  wl_zmmt0167-hora_registro = sy-uzeit.
  wl_zmmt0167-usuario = sy-uname.

  MOVE-CORRESPONDING wl_zmmt0167 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0167_0004 CHANGING p_saida TYPE any.

  DATA: wl_zmmt0167_out TYPE zmmt0167_out.

  CLEAR: wl_zmmt0167_out.

  MOVE-CORRESPONDING p_saida TO wl_zmmt0167_out.


ENDFORM.

FORM f_exit_zmmt0167_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

  IF p_db_tab = 'ZMMT0167'.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.

endform.

FORM f_exit_zmmt0167_0016 USING p_ucomm TYPE sy-ucomm
                          CHANGING p_registro_manter TYPE any
                                             p_saida TYPE any.

  DATA: wl_zmmt0167 TYPE zmmt0167.

  CLEAR: wl_zmmt0167.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0167.


  IF wl_zmmt0167-branch IS NOT INITIAL.
    SELECT SINGLE name
      FROM j_1bbranch
      INTO wl_zmmt0167-name
      WHERE branch = wl_zmmt0167-branch.
  ENDIF.

  IF wl_zmmt0167-lgort IS NOT INITIAL.

    SELECT SINGLE lgobe
      FROM t001l
      INTO wl_zmmt0167-lgobe
      WHERE lgort = wl_zmmt0167-lgort .
  ENDIF.

  MOVE-CORRESPONDING wl_zmmt0167 TO p_saida.
  MOVE-CORRESPONDING wl_zmmt0167 TO p_registro_manter.

ENDFORM.
FORM f_exit_zmmt0167_0017 USING p_tipo.

  IF p_tipo = '0001'.
    PERFORM f4_val_branch USING '<FS_WA_REGISTRO_MANTER>-BRANCH'
                                '<FS_WA_REGISTRO_MANTER>-NAME'.
  ENDIF.

  IF p_tipo = '0002'.
    PERFORM f4_val_lgort USING '<FS_WA_REGISTRO_MANTER>-LGORT'
                               '<FS_WA_REGISTRO_MANTER>-LGOBE'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_VAL
*&---------------------------------------------------------------------*
FORM f4_val_branch USING p_cod TYPE help_info-dynprofld
                         p_desc TYPE help_info-dynprofld.

*====>  Tabelas internas
  DATA: BEGIN OF t_branch OCCURS 0,
          bukrs  TYPE j_1bbranch-bukrs,
          branch TYPE j_1bbranch-branch,
          name   TYPE j_1bbranch-name,
        END OF t_branch.

  CLEAR t_return.
*  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  SELECT bukrs branch name
    FROM  j_1bbranch INTO TABLE t_branch.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0003'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'BRANCH'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Local de negócios'
        value_org       = 'S'
      TABLES
        value_tab       = t_branch
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
  ENDIF.



ENDFORM.
FORM f4_val_lgort USING p_cod TYPE help_info-dynprofld
                        p_desc TYPE help_info-dynprofld.

*====>  Tabelas internas
  DATA: BEGIN OF t_lgort OCCURS 0,
          werks	TYPE werks_d,
          lgort	TYPE lgort_d,
          lgobe	TYPE lgobe,
        END OF t_lgort.

*  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  READ TABLE t_return ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY fieldname = 'F0002'.
  IF sy-subrc = 0.

    SELECT werks lgort lgobe UP TO 500 ROWS
      FROM  t001l INTO TABLE t_lgort
      WHERE werks = <fs>-fieldval.

  ELSE.
    SELECT werks lgort lgobe UP TO 500 ROWS
      FROM  t001l INTO TABLE t_lgort.
  ENDIF.

  IF sy-subrc = 0.

    SORT t_lgort BY werks.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0003'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'LGORT'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Depósito'
        value_org       = 'S'
      TABLES
        value_tab       = t_lgort
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
  ENDIF.

ENDFORM.
