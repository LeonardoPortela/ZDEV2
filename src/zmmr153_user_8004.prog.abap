*----------------------------------------------------------------------*
***INCLUDE ZMMR153_USER_8004.
*----------------------------------------------------------------------*

DATA: container_8004  TYPE REF TO cl_gui_custom_container,
      html_peso_medio TYPE REF TO cl_gui_html_viewer.

*&---------------------------------------------------------------------*
*&      Module  STATUS_8004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_8004 OUTPUT.

  SET PF-STATUS 'PF8001'.
  SET TITLEBAR 'TL8004'.

  CLEAR ck_alterado_carga.

  LOOP AT SCREEN.
    SPLIT screen-name AT '-' INTO: DATA(str1a_8004) DATA(str2a_8004).
    IF str1a_8004 EQ 'ZDE_ZSDT0001CG_ALV' OR str1a_8004 EQ 'ZDE_ZSDT0001OD_ALV' OR str1a_8004 EQ 'ZDE_ZSDT0001OV_ALV'.

      IF str2a_8004 EQ 'NM_PESO_SUBTOTAL'.
        objeto->valida_atributo_alteravel( EXPORTING i_campo = 'NM_PESO_BRUTO' IMPORTING e_permitido = DATA(e_permitido_8004) ).
        IF e_permitido_8004 EQ abap_false.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
        CONTINUE.
      ENDIF.

      objeto->valida_atributo_alteravel( EXPORTING i_campo = CONV #( str2a_8004 ) IMPORTING e_permitido = e_permitido_8004 ).
      IF e_permitido_8004 EQ abap_false.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM gera_peso_medio.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8004_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8004_exit INPUT.

  CLEAR: ok_code, ck_add_peso.
  PERFORM limpar_8004.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_8004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_8004 .

  IF html_peso_medio IS NOT INITIAL.
    html_peso_medio->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: html_peso_medio.

  IF container_8004 IS NOT INITIAL.
    container_8004->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: container_8004.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8004 INPUT.

*   alterado por guilherme rabelo inicio.
  IF zde_zsdt0001cg_alv-nm_peso_bruto <> '0' AND zde_zsdt0001cg_alv-nm_peso_tara <> '0' AND
    zde_zsdt0001cg_alv-nm_peso_subtotal <> '0'.

    CASE ok_code.
      WHEN 'CONF'.
        CHECK ck_alterado_carga EQ abap_false.
        CLEAR: ok_code.
        ck_add_peso = abap_true.
        PERFORM limpar_8004.
        LEAVE TO SCREEN 0.
    ENDCASE.


  ELSE.

    MESSAGE 'Obrigatório informarções do Peso de Balança' TYPE 'S' DISPLAY LIKE 'S'.

  ENDIF.
*   alterado por guilherme rabelo fim

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GERA_PESO_MEDIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_peso_medio .

  IF container_8004 IS INITIAL.

    CREATE OBJECT container_8004
      EXPORTING
        container_name = 'ALVPESOMEDIO'
      EXCEPTIONS
        OTHERS         = 1.

    CREATE OBJECT html_peso_medio
      EXPORTING
        parent = container_8004.

  ENDIF.

  DATA: data_table TYPE STANDARD TABLE OF text255,
        i_urlpeso  TYPE c LENGTH 200.

  PERFORM gera_html_peso_medio.

  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      i_string         = htmlpesomedio
      i_tabline_length = 255
    TABLES
      et_table         = data_table.

  html_peso_medio->load_data(
    IMPORTING
      assigned_url           = i_urlpeso
    CHANGING
      data_table             = data_table
    EXCEPTIONS
      dp_invalid_parameter   = 1
      dp_error_general       = 2
      cntl_error             = 3
      html_syntax_notcorrect = 4
      OTHERS                 = 5
  ).

  html_peso_medio->show_url(
    EXPORTING
      url                    = i_urlpeso
    EXCEPTIONS
      cntl_error             = 1
      cnht_error_not_allowed = 2
      cnht_error_parameter   = 3
      dp_error_general       = 4
      OTHERS                 = 5
  ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERA_HTML_PESO_MEDIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_html_peso_medio .

  DATA: tx_peso_fardo_sem_emb TYPE c LENGTH 20,
        tx_peso_fardo_com_emb TYPE c LENGTH 20,
        tx_peso_embalagem     TYPE c LENGTH 20.

  DATA: lc_peso_fardo_sem_emb TYPE ty_valor,
        lc_peso_fardo_com_emb TYPE ty_valor,
        lc_peso_embalagem     TYPE ty_valor.

  PERFORM calcula_media CHANGING lc_peso_fardo_sem_emb lc_peso_fardo_com_emb lc_peso_embalagem.

  WRITE lc_peso_embalagem TO tx_peso_embalagem.
  CONDENSE tx_peso_embalagem NO-GAPS.
  WRITE lc_peso_fardo_sem_emb TO tx_peso_fardo_sem_emb.
  CONDENSE tx_peso_fardo_sem_emb NO-GAPS.
  WRITE lc_peso_fardo_com_emb TO tx_peso_fardo_com_emb.
  CONDENSE tx_peso_fardo_com_emb NO-GAPS.

  htmlpesomedio =
    '<!DOCTYPE html>' && cl_abap_char_utilities=>newline &&
    '<html>' && cl_abap_char_utilities=>newline &&
    '<head>' && cl_abap_char_utilities=>newline &&
    '<meta name="viewport" content="width=device-width, initial-scale=1">' && cl_abap_char_utilities=>newline &&
    '<style>' && cl_abap_char_utilities=>newline &&
    'body {' && cl_abap_char_utilities=>newline &&
    '  font-family: Verdana;' && cl_abap_char_utilities=>newline &&
    '  font-size: 11px;' && cl_abap_char_utilities=>newline &&
    '  padding: 4px;' && cl_abap_char_utilities=>newline &&
    '  background-color: #DFEBF5;' && cl_abap_char_utilities=>newline &&
    '  overflow: hidden;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '* {' && cl_abap_char_utilities=>newline &&
    '  box-sizing: border-box;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '.row {' && cl_abap_char_utilities=>newline &&
    '  display: -ms-flexbox;' && cl_abap_char_utilities=>newline &&
    '  display: flex;' && cl_abap_char_utilities=>newline &&
    '  -ms-flex-wrap: wrap;' && cl_abap_char_utilities=>newline &&
    '  flex-wrap: wrap;' && cl_abap_char_utilities=>newline &&
    '  margin: 0 -16px;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '.col-25 {' && cl_abap_char_utilities=>newline &&
    '  -ms-flex: 100%;' && cl_abap_char_utilities=>newline &&
    '  flex: 100%;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '.col-25 { padding: 0 16px; }' && cl_abap_char_utilities=>newline &&
    '.container {' && cl_abap_char_utilities=>newline &&
    '  background-color: #DFEBF5;' && cl_abap_char_utilities=>newline &&
    '  padding: 5px 20px 15px 20px;' && cl_abap_char_utilities=>newline &&
    '  border: 1px solid #DFEBF5;' && cl_abap_char_utilities=>newline &&
    '  border-radius: 3px;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    'a { color: #2196F3; }' && cl_abap_char_utilities=>newline &&
    'hr { border: 1px solid lightgrey; }' && cl_abap_char_utilities=>newline &&
    'span.price { float: right; color: black; }' && cl_abap_char_utilities=>newline &&
    '</style>' && cl_abap_char_utilities=>newline &&
    '</head>' && cl_abap_char_utilities=>newline &&
    '<body>' && cl_abap_char_utilities=>newline &&
    '<div class="row">' && cl_abap_char_utilities=>newline &&
    '  <div class="col-25">' && cl_abap_char_utilities=>newline &&
    '    <div class="container">' && cl_abap_char_utilities=>newline &&
    '      <h4>Peso Médio do Fardo (KG)</h4>' && cl_abap_char_utilities=>newline &&
    '      <hr>' && cl_abap_char_utilities=>newline &&
    '      <p><a href="#">Peso Bruto Fardo </a> <span class="price">' && tx_peso_fardo_com_emb && '</span></p>' && cl_abap_char_utilities=>newline &&
    '      <p><a href="#">Peso Embalagem</a> <span class="price">' && tx_peso_embalagem && '</span></p>' && cl_abap_char_utilities=>newline &&
    '      <hr>' && cl_abap_char_utilities=>newline &&
    '      <p><a href="#">Peso Líquido Fardo</a> <span class="price">' && tx_peso_fardo_sem_emb && '</span></p>' && cl_abap_char_utilities=>newline &&
    '    </div>' && cl_abap_char_utilities=>newline &&
    '  </div>' && cl_abap_char_utilities=>newline &&
    '</div>' && cl_abap_char_utilities=>newline &&
    '</body>' && cl_abap_char_utilities=>newline &&
    '</html>'.

ENDFORM.
