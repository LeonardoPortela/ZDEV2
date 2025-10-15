*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT0102_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0208_exit.

FORM f_exit_zfit0208_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0208 TYPE zfit0208.

  CLEAR: wl_zfit0208.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0208.

  IF wl_zfit0208-usuario IS INITIAL.
    wl_zfit0208-data      = sy-datum.
    wl_zfit0208-hora      = sy-uzeit.
    wl_zfit0208-usuario   = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zfit0208 TO p_registro_manter.

ENDFORM.


FORM f_exit_zfit0208_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zfit0208        TYPE zfit0208,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zfit0208. " BUG - 171955 - CBRAND
  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0208.

  CLEAR: p_error.

  IF wl_zfit0208-cod_operacao IS NOT INITIAL.
    CLEAR t_cod_oper_cliente[].
    SELECT SINGLE des_operacao
           FROM zfit0220
           INTO wl_zfit0208-desc_cod_operacao
      WHERE cod_operacao EQ wl_zfit0208-cod_operacao.
  ENDIF.

*** BUG - 171955 - Inicio - CBRAND
  IF wl_zfit0208-saknr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Conta Razão é campo obrigatório' TYPE 'S'.
    EXIT.
  ENDIF.
*** BUG - 171955 - Fim - CBRAND

*  IF WL_ZFIT0208-SAKNR IS NOT INITIAL.
*    SELECT SINGLE TXT50
*           FROM SKAT
*           INTO WL_ZFIT0208-DESC_SAKNR
*           WHERE SPRAS EQ 'P'
*           AND   KTOPL EQ '0050'
*           AND   SAKNR EQ WL_ZFIT0208-SAKNR.
*  ENDIF.
**<<<------"189660 - NMS - INI------>>>
  DATA(cll_util_tab_zfit0208) = NEW zclfi_util_tab_zfit0208( ).
  DATA: el_zfit0208 TYPE zfit0208.

  cll_util_tab_zfit0208->verifica_chave_primaria_logica( EXPORTING ie_zfit0208 = wl_zfit0208
                                                                   iv_operacao = sy-ucomm
                                                         IMPORTING ev_msgerr_cvdup = DATA(lv_erro)
                                                                   ee_zfit0208     = el_zfit0208 ).

  IF lv_erro IS INITIAL.
    wl_zfit0208 = el_zfit0208.
    CLEAR p_error.

  ELSE.
    p_error = abap_true.
    MESSAGE lv_erro TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.
**<<<------"189660 - NMS - FIM------>>>
  MOVE-CORRESPONDING wl_zfit0208 TO p_registro_manter.

  CLEAR: p_error.

ENDFORM.


FORM f_exit_zfit0208_0003 CHANGING p_saida TYPE any.

*  DATA: wl_zfit0208 TYPE zfit0208.
*
*  CLEAR: wl_zfit0208.
*
*  MOVE-CORRESPONDING p_saida TO wl_zfit0208.
*
*  MOVE-CORRESPONDING wl_zfit0208 TO p_saida.


ENDFORM.

FORM f_exit_zfit0208_0004 CHANGING p_saida TYPE any.

  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zfit0208_out    TYPE zfit0208_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zfit0208_out.
  MOVE-CORRESPONDING p_saida TO wl_zfit0208_out.

  IF wl_zfit0208_out-cod_operacao IS NOT INITIAL.
    CLEAR t_cod_oper_cliente[].
    SELECT SINGLE des_operacao
           FROM zfit0220
           INTO wl_zfit0208_out-desc_cod_operacao
      WHERE cod_operacao EQ wl_zfit0208_out-cod_operacao.
  ENDIF.

*  IF WL_ZFIT0208_OUT-SAKNR IS NOT INITIAL.
*    SELECT SINGLE TXT50
*           FROM SKAT
*           INTO WL_ZFIT0208_OUT-DESC_SAKNR
*           WHERE SPRAS EQ 'P'
*           AND   KTOPL EQ '0050'
*           AND   SAKNR EQ WL_ZFIT0208_OUT-SAKNR.
*  ENDIF.

  MOVE-CORRESPONDING wl_zfit0208_out TO p_saida.

ENDFORM.

FORM f_exit_zfit0208_0005 CHANGING p_registro_manter TYPE any.


  TYPES: BEGIN OF ty_cod_oper_cliente,
           low  TYPE tvarvc-low,
           high TYPE tvarvc-high,
         END   OF ty_cod_oper_cliente.

  DATA: wl_zfit0208_out    TYPE zfit0208_out,
        vl_low             TYPE tvarvc-low,
        t_cod_oper_cliente TYPE TABLE OF ty_cod_oper_cliente.

  CLEAR: wl_zfit0208_out.
  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0208_out.


  IF wl_zfit0208_out-cod_operacao IS NOT INITIAL.
    CLEAR t_cod_oper_cliente[].
    SELECT SINGLE des_operacao
           FROM zfit0220
           INTO wl_zfit0208_out-desc_cod_operacao
      WHERE cod_operacao EQ wl_zfit0208_out-cod_operacao.
  ENDIF.

*  IF WL_ZFIT0208_OUT-SAKNR IS NOT INITIAL.
*    SELECT SINGLE TXT50
*           FROM SKAT
*           INTO WL_ZFIT0208_OUT-DESC_SAKNR
*           WHERE SPRAS EQ 'P'
*           AND   KTOPL EQ '0050'
*           AND   SAKNR EQ WL_ZFIT0208_OUT-SAKNR.
*  ENDIF.

  MOVE-CORRESPONDING wl_zfit0208_out TO p_registro_manter.



ENDFORM.
* BUG - 178687 - CBRAND - Inicio
FORM f_exit_zfit0208_0008_v2  CHANGING  p_fcat_out TYPE lvc_s_fcat.
  IF  p_fcat_out-ref_table = 'ZFIT0208_OUT' AND
     p_fcat_out-ref_field      = 'SAKNR'.

    p_fcat_out-hotspot = 'X'.
  ENDIF.
**<<<------"189660 - NMS - INI------>>>
  IF  p_fcat_out-ref_table = 'ZFIT0208_OUT'.
    CASE p_fcat_out-ref_field.
      WHEN 'KEYPAR'.
        p_fcat_out-no_out = abap_on.
        p_fcat_out-tech   = abap_on.

      WHEN OTHERS.
*   Do nothing
    ENDCASE.

  ENDIF.
**<<<------"189660 - NMS - FIM------>>>
ENDFORM.
* BUG - 178687 - CBRAND - Fim
FORM  f_exit_zfit0208_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'zfit0208'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.

FORM  f_exit_zfit0208_0016 USING p_ucomm CHANGING p_registro_manter  TYPE any
                                                  p_saida TYPE any.

  DATA: ls_zfit0208   TYPE zfit0208,
        return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.

  CLEAR: ls_zfit0208.

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

    MOVE-CORRESPONDING p_registro_manter TO ls_zfit0208.

    ls_zfit0208-saknr = COND #( WHEN ls_zfit0208-saknr IS INITIAL THEN ls_values-fieldval ELSE |{ ls_zfit0208-saknr },{ ls_values-fieldval }| ).

    MOVE-CORRESPONDING ls_zfit0208 TO p_registro_manter.

  ENDIF.

  IF p_ucomm EQ 'ADD'.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = 'T161'
        fieldname         = 'BSART'
        searchhelp        = 'EHFND_PC_PUR_DT'
      TABLES
        return_tab        = return_values
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.

    CHECK sy-subrc IS INITIAL.

    READ TABLE return_values INTO ls_values INDEX 1.

    CHECK sy-subrc IS INITIAL.

    MOVE-CORRESPONDING p_registro_manter TO ls_zfit0208.

    ls_zfit0208-bsart = COND #( WHEN ls_zfit0208-bsart IS INITIAL THEN ls_values-fieldval ELSE |{ ls_zfit0208-bsart },{ ls_values-fieldval }| ).

    MOVE-CORRESPONDING ls_zfit0208 TO p_registro_manter.

  ENDIF.

** BUG - 181038 - CBRAND - Inicio
  IF p_ucomm EQ 'LIFNR'.

    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = 'M_KREDA'
        fieldname         = 'LIFNR'
        searchhelp        = 'KRED_C'
      TABLES
        return_tab        = return_values
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.

    CHECK sy-subrc IS INITIAL.

    READ TABLE return_values INTO ls_values INDEX 1.

    CHECK sy-subrc IS INITIAL.

    MOVE-CORRESPONDING p_registro_manter TO ls_zfit0208.

    ls_zfit0208-lifnr = COND #( WHEN ls_zfit0208-lifnr IS INITIAL THEN ls_values-fieldval ELSE |{ ls_zfit0208-lifnr },{ ls_values-fieldval }| ).

    MOVE-CORRESPONDING ls_zfit0208 TO p_registro_manter.

  ENDIF.
** BUG - 181038 - CBRAND - Fim


ENDFORM.

FORM  f_exit_zfit0208_0017 USING p_tipo.

  DATA: return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = 'ZFIT0208'
      fieldname         = 'COD_OPERACAO'
      searchhelp        = 'Z_COD_OPERACAO'
      dynpprog          = sy-repid
      dynpnr            = sy-dynnr
      dynprofield       = '<FS_WA_REGISTRO_MANTER>-COD_OPERACAO'
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
FORM f_exit_zfit0208_0018   USING  p_saida TYPE any
                                   p_column_id TYPE lvc_fname
                                   p_row_id TYPE lvc_index.

  DATA: wl_zfit0208_out TYPE zfit0208.
  DATA: lt_trtexts    TYPE trtexts,
        lw_trtexts    TYPE trtext,
        lv_texto(500).
  DATA lv_edit TYPE c.
  DATA lt_text_aux TYPE TABLE OF txw_note.

  IF  p_saida IS NOT INITIAL.
    wl_zfit0208_out = CORRESPONDING #( p_saida ).
    lv_texto = wl_zfit0208_out-saknr.

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
