*&---------------------------------------------------------------------*
*& Report ZRD_zmail_EXIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmail_exit.

FORM f_exit_zmail_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmail TYPE zmail.

  CLEAR: wl_zmail.
  wl_zmail-usuario    = sy-uname.
  wl_zmail-zdt_atual  = sy-datum.
  wl_zmail-zhr_atual  = sy-uzeit.

  MOVE-CORRESPONDING wl_zmail TO p_registro_manter.

ENDFORM.

FORM f_exit_zmail_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

ENDFORM.

FORM f_exit_zmail_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmail    TYPE zmail.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zmail.

  wl_zmail-usuario    = sy-uname.
  wl_zmail-zdt_atual  = sy-datum.
  wl_zmail-zhr_atual  = sy-uzeit.

  MOVE-CORRESPONDING wl_zmail TO p_registro_manter.

ENDFORM.

FORM f_exit_zmail_0004 CHANGING p_saida TYPE any.

ENDFORM.

FORM f_exit_zmail_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmail    TYPE zmail.

  CLEAR: wl_zmail.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zmail.
  MOVE-CORRESPONDING wl_zmail TO p_registro_manter.

ENDFORM.

FORM f_exit_zmail_0006  USING p_registro_manter
                        CHANGING p_error.

  "Validações antes de apagar.

ENDFORM.

FORM  f_exit_zmail_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

  APPEND 'Modificar'    TO it_excl_toolbar.

ENDFORM.

FORM  f_exit_zmail_0010 TABLES p_tables.

  DATA: lt_zmail TYPE TABLE OF zmail_out.

  lt_zmail[] = p_tables[].

  SORT lt_zmail[] BY zdt_atual DESCENDING
                     zhr_atual DESCENDING.

  p_tables[] = lt_zmail[].

ENDFORM.

FORM f_exit_zmail_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ZMAIL_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZMAIL'
      tabfirst = 'X'.

ENDFORM.

FORM f_exit_zmail_0017 USING p_tipo.

  IF p_tipo = '0001'.
    PERFORM f4_val_param_espec USING '<FS_WA_REGISTRO_MANTER>-PARAM_ESPEC'.
  ENDIF.

ENDFORM.

FORM f4_val_param_espec USING p_param_espec  TYPE help_info-dynprofld.

  TYPES: BEGIN OF ty_zmail ,
           domvalue_l TYPE dd07v-domvalue_l,
           ddtext     TYPE dd07v-ddtext,
         END OF ty_zmail.

  DATA: lt_return  TYPE STANDARD TABLE OF ddshretval,
        lt_mapping TYPE STANDARD TABLE OF dselc,
        lt_zmail   TYPE TABLE OF ty_zmail,
        lt_dd07v   TYPE TABLE OF dd07v.

  DATA: ls_mapping TYPE dselc.

  CONSTANTS lc_param_espec TYPE help_info-dynprofld VALUE 'PARAM_ESPEC'.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZSDD013'
      langu         = sy-langu
    TABLES
      dd07v_tab     = lt_dd07v
    EXCEPTIONS
      illegal_input = 1.

  MOVE-CORRESPONDING lt_dd07v  TO lt_zmail.

  IF lt_zmail IS NOT INITIAL.

    ls_mapping-fldname     = 'F0001'.
    ls_mapping-dyfldname   = p_param_espec.
    APPEND ls_mapping TO lt_mapping.
    CLEAR ls_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'DOMVALUE_L'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = lc_param_espec
        window_title    = 'Paramêtro Especial'
        value_org       = 'S'
      TABLES
        value_tab       = lt_zmail
        return_tab      = lt_return
        dynpfld_mapping = lt_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

  ENDIF.

ENDFORM.

"MMSILVA - #163322 - 10.02.2025 - Inicio
FORM f_exit_zmail_0020.

  SUBMIT ZMAIL_BULK_INSERT.

ENDFORM.
"MMSILVA - #163322 - 10.02.2025 - Fim
