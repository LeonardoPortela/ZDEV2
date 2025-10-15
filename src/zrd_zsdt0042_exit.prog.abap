*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0042_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0042_exit.

FORM f_exit_zsdt0042_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0042 TYPE zsdt0042.

  CLEAR: wl_zsdt0042.


  MOVE-CORRESPONDING wl_zsdt0042 TO p_registro_manter.

ENDFORM.


FORM f_exit_zsdt0042_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.
  TYPES: BEGIN OF ty_witht,
           witht TYPE witht,
           text  TYPE char60,
         END OF ty_witht.

  DATA: t_dd07v TYPE TABLE OF dd07v,
        t_witht TYPE TABLE OF ty_witht,
        t_ret   TYPE TABLE OF ddshretval,
        v_rc    TYPE sy-subrc.

  DATA: lt_witht TYPE TABLE OF ty_witht .
  DATA: wl_zsdt0042 TYPE zsdt0042.
  CLEAR: wl_zsdt0042.
  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0042.
  CLEAR: p_error.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = 'WITHT'
      text      = 'X'
      langu     = 'P'
*     BYPASS_BUFFER        = ' '
    IMPORTING
      rc        = v_rc
    TABLES
      dd07v_tab = t_dd07v.

  LOOP AT t_dd07v INTO DATA(ls_dd07v).
    APPEND VALUE #( witht = ls_dd07v-domvalue_l text = ls_dd07v-ddtext ) TO lt_witht.
  ENDLOOP.

*  ASSIGN ('(ZREGISTER_DATA)T_WITHT[]') TO FIELD-SYMBOL(<fs_witht>).
*  APPEND LINES OF <fs_witht> TO lt_witht.
*  IF  wl_zsdt0042-witht IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Campo CTG.imp.ret.fon. é obrigatório' TYPE 'S' DISPLAY LIKE 'E' .
*    EXIT.
*  ENDIF.

*#138092 -  ITSOUZA - Inicio
*  IF <fs_witht> IS ASSIGNED.
  READ TABLE lt_witht TRANSPORTING NO FIELDS WITH KEY witht = wl_zsdt0042-witht.
  IF sy-subrc NE 0.
    p_error = abap_true.
    MESSAGE 'CTG.imp.ret.fon. desconhecido' TYPE 'S' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.
* ENDIF.
*#138092 -  ITSOUZA - Fim

  IF wl_zsdt0042-cultura IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Cultura é obrigatório'  TYPE 'S' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

  IF wl_zsdt0042-waerk IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Moeda documento é obrigatório'  TYPE 'S' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

  IF wl_zsdt0042-estado IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Região é obrigatório'  TYPE 'S' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

  IF wl_zsdt0042-val_de IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Válido desde é obrigatório'  TYPE 'S' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

  IF wl_zsdt0042-val_ate IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Válido até é obrigatório'  TYPE 'S' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

  IF wl_zsdt0042-safra IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Safra é obrigatório'  TYPE 'S' DISPLAY LIKE 'E' .
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0042 TO p_registro_manter.




ENDFORM.


FORM f_exit_zsdt0042_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zsdt0042 TYPE zsdt0042.
*
*  CLEAR: wl_zsdt0042.
*
*  MOVE-CORRESPONDING p_saida TO wl_zsdt0042.
*
*  MOVE-CORRESPONDING wl_zsdt0042 TO p_saida.


ENDFORM.

FORM f_exit_zsdt0042_0004 CHANGING p_saida TYPE any.

*  DATA: wl_zsdt0042_out TYPE zsdt0042_out.
*
*  CLEAR: wl_zsdt0042_out.
*
*  MOVE-CORRESPONDING p_saida TO wl_zsdt0042_out.
*
*  CLEAR p_saida.
*
*  MOVE-CORRESPONDING wl_zsdt0042_out TO p_saida.

ENDFORM.

FORM f_exit_zsdt0042_0005 CHANGING p_registro_manter TYPE any.
*  DATA: wl_zsdt0042_out TYPE zsdt0042_out.
*  DATA: wl_t001 TYPE  t001,
*        wl_lfa1 TYPE  lfa1.
*
*  CLEAR: wl_zsdt0042_out, wl_t001, wl_lfa1.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0042_out.
*
*
*  MOVE-CORRESPONDING wl_zsdt0042_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_zsdt0042_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zsdt0042'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.

ENDFORM.

FORM f_exit_zsdt0042_0017 USING p_tipo.

  IF p_tipo = '0001'.
    PERFORM f4_val_valida USING '<FS_WA_REGISTRO_MANTER>-WITHT'.
  ENDIF.

ENDFORM.

FORM f4_val_valida USING p_cod TYPE help_info-dynprofld.

  TYPES: BEGIN OF ty_witht,
           witht TYPE witht,
           text  TYPE char60,
         END OF ty_witht.

  DATA: t_dd07v TYPE TABLE OF dd07v,
        t_witht TYPE TABLE OF ty_witht,
        t_ret   TYPE TABLE OF ddshretval,
        v_rc    TYPE sy-subrc.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = 'WITHT'
      text      = 'X'
      langu     = 'P'
*     BYPASS_BUFFER        = ' '
    IMPORTING
      rc        = v_rc
    TABLES
      dd07v_tab = t_dd07v.

  LOOP AT t_dd07v INTO DATA(ls_dd07v).
    APPEND VALUE #( witht = ls_dd07v-domvalue_l text = ls_dd07v-ddtext ) TO t_witht.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'WITHT'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = '<FS_WA_REGISTRO_MANTER>-WITHT'
      value_org       = 'S'
    TABLES
      value_tab       = t_witht
      return_tab      = t_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
