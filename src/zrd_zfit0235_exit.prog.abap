*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0235_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0235_exit.

FORM f_exit_zfit0235_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0235 TYPE zfit0235.

  CLEAR: wl_zfit0235.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0235.

  IF wl_zfit0235-usuario IS INITIAL.
    wl_zfit0235-data      = sy-datum.
    wl_zfit0235-hora      = sy-uzeit.
    wl_zfit0235-usuario   = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0235 TO p_registro_manter.

ENDFORM.


FORM f_exit_zfit0235_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zfit0235        TYPE zfit0235,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zfit0235. " BUG - 171955 - CBRAND
  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0235.

  CLEAR: p_error.

  IF wl_zfit0235-cod_subop IS NOT INITIAL.
    CLEAR t_cod_oper_cliente[].
    SELECT SINGLE des_subop
           FROM zfit0234
           INTO wl_zfit0235-des_subop
      WHERE cod_subop EQ wl_zfit0235-cod_subop.
  ENDIF.

*** BUG - 171955 - Inicio - CBRAND
  IF wl_zfit0235-saknr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Conta Razão é campo obrigatório' TYPE 'S'.
    EXIT.
  ENDIF.
*** BUG - 171955 - Fim - CBRAND

*  IF WL_ZFIT0235-SAKNR IS NOT INITIAL.
*    SELECT SINGLE TXT50
*           FROM SKAT
*           INTO WL_ZFIT0235-DESC_SAKNR
*           WHERE SPRAS EQ 'P'
*           AND   KTOPL EQ '0050'
*           AND   SAKNR EQ WL_ZFIT0235-SAKNR.
*  ENDIF.

  MOVE-CORRESPONDING wl_zfit0235 TO p_registro_manter.

  CLEAR: p_error.

ENDFORM.


FORM f_exit_zfit0235_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zfit0235 TYPE zfit0235.
*
*  CLEAR: wl_zfit0235.
*
*  MOVE-CORRESPONDING p_saida TO wl_zfit0235.
*
*  MOVE-CORRESPONDING wl_zfit0235 TO p_saida.


ENDFORM.

FORM f_exit_zfit0235_0004 CHANGING p_saida TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zfit0235_out    TYPE zfit0235_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zfit0235_out.
  MOVE-CORRESPONDING p_saida TO wl_zfit0235_out.

  IF wl_zfit0235_out-cod_subop IS NOT INITIAL.
    CLEAR t_cod_oper_cliente[].
    SELECT SINGLE des_subop
           FROM zfit0234
           INTO wl_zfit0235_out-des_subop
      WHERE cod_subop EQ wl_zfit0235_out-cod_subop.
  ENDIF.

*  IF WL_ZFIT0235_OUT-SAKNR IS NOT INITIAL.
*    SELECT SINGLE TXT50
*           FROM SKAT
*           INTO WL_ZFIT0235_OUT-DESC_SAKNR
*           WHERE SPRAS EQ 'P'
*           AND   KTOPL EQ '0050'
*           AND   SAKNR EQ WL_ZFIT0235_OUT-SAKNR.
*  ENDIF.

  MOVE-CORRESPONDING wl_zfit0235_out TO p_saida.

ENDFORM.

FORM f_exit_zfit0235_0005 CHANGING p_registro_manter TYPE any.


  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zfit0235_out    TYPE zfit0235_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zfit0235_out.
  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0235_out.


  IF wl_zfit0235_out-cod_subop IS NOT INITIAL.
    CLEAR t_cod_oper_cliente[].
    SELECT SINGLE des_subop
           FROM zfit0234
           INTO wl_zfit0235_out-des_subop
      WHERE cod_subop EQ wl_zfit0235_out-cod_subop.
  ENDIF.

*  IF WL_ZFIT0235_OUT-SAKNR IS NOT INITIAL.
*    SELECT SINGLE TXT50
*           FROM SKAT
*           INTO WL_ZFIT0235_OUT-DESC_SAKNR
*           WHERE SPRAS EQ 'P'
*           AND   KTOPL EQ '0050'
*           AND   SAKNR EQ WL_ZFIT0235_OUT-SAKNR.
*  ENDIF.

  MOVE-CORRESPONDING wl_zfit0235_out TO p_registro_manter.



ENDFORM.
* BUG - 178687 - CBRAND - Inicio
FORM f_exit_zfit0235_0008_v2  CHANGING  p_fcat_out TYPE lvc_s_fcat.
  IF  p_fcat_out-ref_table = 'ZFIT0235_OUT' AND
     p_fcat_out-ref_field      = 'SAKNR'.

    p_fcat_out-hotspot = 'X'.
  ENDIF.
ENDFORM.
* BUG - 178687 - CBRAND - Fim
FORM  f_exit_zfit0235_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zfit0235'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.

FORM  f_exit_zfit0235_0016 USING p_ucomm CHANGING p_registro_manter  TYPE any
                                                  p_saida TYPE any.

  DATA: ls_zfit0235   TYPE zfit0235,
        return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.

  CLEAR: ls_zfit0235.

  IF p_ucomm EQ 'SAKNR'.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = 'SKA1'
        fieldname         = 'SAKNR'
        searchhelp        = 'ZSHFI_SAKNR'
      TABLES
        return_tab        = return_values
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.

    CHECK sy-subrc IS INITIAL.

    READ TABLE return_values INTO DATA(ls_values) INDEX 1.

    CHECK sy-subrc IS INITIAL.

    MOVE-CORRESPONDING p_registro_manter TO ls_zfit0235.

    ls_zfit0235-saknr = COND #( WHEN ls_zfit0235-saknr IS INITIAL THEN ls_values-fieldval ELSE |{ ls_zfit0235-saknr },{ ls_values-fieldval }| ).

    MOVE-CORRESPONDING ls_zfit0235 TO p_registro_manter.

  ENDIF.


ENDFORM.

FORM  f_exit_zfit0235_0017 USING p_tipo.

  DATA: return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = 'ZFIT0235'
      fieldname         = 'COD_SUBOP'
      searchhelp        = 'Z_COD_SUBOP'
      dynpprog          = sy-repid
      dynpnr            = sy-dynnr
      dynprofield       = '<FS_WA_REGISTRO_MANTER>-COD_SUBOP'
*     value_org         = 'S'
    TABLES
      return_tab        = return_values
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '='
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

ENDFORM.
* BUG - 178687 - CBRAND - Inicio
FORM f_exit_zfit0235_0018   USING  p_saida TYPE any
                                   p_column_id TYPE lvc_fname
                                   p_row_id TYPE lvc_index.

  DATA: wl_zfit0235_out TYPE zfit0235.
  DATA: lt_trtexts    TYPE trtexts,
        lw_trtexts    TYPE trtext,
        lv_texto(500).
  DATA lv_edit TYPE c.
  DATA lt_text_aux TYPE TABLE OF txw_note.

  IF  p_saida IS NOT INITIAL.
    wl_zfit0235_out = CORRESPONDING #( p_saida ).
    lv_texto = wl_zfit0235_out-saknr.

    CALL FUNCTION 'TR_SPLIT_TEXT'
      EXPORTING
        iv_text  = lv_texto
        iv_len   = 69
      IMPORTING
        et_lines = lt_trtexts.

    LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).
      APPEND INITIAL LINE TO lt_text_aux ASSIGNING FIELD-SYMBOL(<fs_aux>).
      <fs_aux>-line = <fs_line>.
    ENDLOOP.

    lv_edit = abap_false.

    CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
      EXPORTING
        edit_mode = lv_edit
      TABLES
        t_txwnote = lt_text_aux[].

    UNASSIGN: <fs_line>, <fs_aux>.

  ENDIF.
ENDFORM.
* BUG - 178687 - CBRAND - Fim
