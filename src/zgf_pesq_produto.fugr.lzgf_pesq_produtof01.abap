*----------------------------------------------------------------------*
***INCLUDE LZGF_PESQ_PRODUTOF01.
*----------------------------------------------------------------------*
TYPES BEGIN OF ty_produto.
TYPES: matnr TYPE matnr.
TYPES: maktx TYPE maktx.
TYPES END OF ty_produto.

CLASS cl_myevent_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_sapevent FOR EVENT sapevent OF cl_gui_html_viewer IMPORTING action frame getdata postdata query_table.
ENDCLASS.


DATA: it_produtos     TYPE TABLE OF ty_produto,
      wa_produtos     TYPE ty_produto,
      lc_produto      TYPE matnr,
      lc_selecionou   TYPE c,
      ok_code         TYPE sy-ucomm,
      splitter        TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer TYPE REF TO cl_gui_container,
      html_control    TYPE REF TO cl_gui_html_viewer,
      html_pagina     TYPE string,
      myevent         TYPE cntl_simple_event,
      myevent_tab     TYPE cntl_simple_events,
      evt_receiver    TYPE REF TO cl_myevent_handler,
      container       TYPE REF TO cl_gui_container.

CLASS cl_myevent_handler IMPLEMENTATION.

  METHOD on_sapevent.
    lc_produto = action.
    lc_selecionou = abap_true.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lc_produto
      IMPORTING
        output = lc_produto.

    LEAVE TO SCREEN 0.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  PESQ_PRODUTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_GRUPO_MATERIAL  text
*----------------------------------------------------------------------*
FORM pesq_produto USING p_grupo_material TYPE wrf_range_matkl_tty CHANGING out_matnr TYPE matnr.

  CLEAR: it_produtos, it_produtos[], lc_produto, out_matnr, lc_selecionou.

  SELECT matnr INTO TABLE @DATA(it_mara)
    FROM mara
   WHERE matkl IN @p_grupo_material.

  CHECK it_mara[] IS NOT INITIAL.

  SORT it_mara BY matnr ASCENDING.

  SELECT matnr, maktx INTO TABLE @DATA(it_makt)
    FROM makt
     FOR ALL ENTRIES IN @it_mara
   WHERE matnr EQ @it_mara-matnr
     AND spras EQ @sy-langu.

  SORT it_makt BY matnr.

  LOOP AT it_mara INTO DATA(wa_mara).

    CLEAR: wa_produtos.

    READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = wa_mara-matnr BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CLEAR: wa_makt.
    ENDIF.

    wa_produtos-matnr = wa_mara-matnr.
    wa_produtos-maktx = wa_makt-maktx.
    APPEND wa_produtos TO it_produtos.

  ENDLOOP.

  CALL SCREEN 0100 STARTING AT 30 05.

  IF lc_selecionou EQ abap_true.
    out_matnr = lc_produto.
  ENDIF.

  PERFORM limpar_0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.

  CLEAR: ok_code, lc_produto, lc_selecionou.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: data_table TYPE STANDARD TABLE OF text255,
        i_url      TYPE c LENGTH 200.

  DATA: nm_field_set TYPE c LENGTH 50,
        pos          TYPE i.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF splitter IS INITIAL.

    CLEAR: data_table, data_table[].

    CREATE OBJECT container TYPE cl_gui_custom_container
      EXPORTING
        container_name = 'OB_TELA'
      EXCEPTIONS
        OTHERS         = 1.

*    CREATE OBJECT splitter
*      EXPORTING
*        parent  = container
*        rows    = 1
*        columns = 1.
*
*    CALL METHOD splitter->get_container
*      EXPORTING
*        row       = 1
*        column    = 1
*      RECEIVING
*        container = ctl_cccontainer.

    CREATE OBJECT html_control
      EXPORTING
        parent = container.

    PERFORM gera_html.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = html_pagina
        i_tabline_length = 255
      TABLES
        et_table         = data_table.

    html_control->load_data(
      IMPORTING
        assigned_url           = i_url
      CHANGING
        data_table             = data_table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5
    ).

    myevent-eventid = html_control->m_id_sapevent.
    myevent-appl_event = abap_true.
    APPEND myevent TO myevent_tab.

    html_control->set_registered_events(
      EXPORTING
        events                    = myevent_tab
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4
    ).

    CREATE OBJECT evt_receiver.
    SET HANDLER evt_receiver->on_sapevent FOR html_control.

    html_control->show_data(
      EXPORTING
        url                    = i_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5
    ).

    cl_gui_control=>set_focus(
      EXPORTING
        control           = html_control
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GERA_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_html .

  DATA: lista_material TYPE string,
        texto_href     TYPE string.

  lista_material = ''.
  LOOP AT it_produtos INTO DATA(wa_produto).
    wa_produto-maktx = zcl_string=>initialcap( CONV #( wa_produto-maktx ) ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_produto-matnr
      IMPORTING
        output = wa_produto-matnr.

    texto_href = |{ wa_produto-maktx } - { wa_produto-matnr }|.
    lista_material = lista_material && '<li><A HREF=SAPEVENT:' && wa_produto-matnr && '>' && texto_href && '</a></li>'  && cl_abap_char_utilities=>newline.

  ENDLOOP.

  html_pagina =
    '<!DOCTYPE html>' && cl_abap_char_utilities=>newline &&
    '<html>' && cl_abap_char_utilities=>newline &&
    '<head>' && cl_abap_char_utilities=>newline &&
    '<meta name="viewport" content="width=device-width, initial-scale=1">' && cl_abap_char_utilities=>newline &&
    '<style>' && cl_abap_char_utilities=>newline &&
    '* {' && cl_abap_char_utilities=>newline &&
    '  box-sizing: border-box;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '' && cl_abap_char_utilities=>newline &&

    'html, body {' && cl_abap_char_utilities=>newline &&
    '  background-color: #DFEBF5;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&

    '#myInput {' && cl_abap_char_utilities=>newline &&
    "'  background-image: url('/css/searchicon.png');
    '  background-position: 10px 12px;' && cl_abap_char_utilities=>newline &&
    '  background-repeat: no-repeat;' && cl_abap_char_utilities=>newline &&
    '  width: 100%;' && cl_abap_char_utilities=>newline &&
    '  font-size: 14px;' && cl_abap_char_utilities=>newline &&
    '  padding: 12px 20px 12px 40px;' && cl_abap_char_utilities=>newline &&
    '  border: 1px solid #ddd;' && cl_abap_char_utilities=>newline &&
    '  margin-bottom: 12px;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '' && cl_abap_char_utilities=>newline &&
    '#myUL {' && cl_abap_char_utilities=>newline &&
    '  list-style-type: none;' && cl_abap_char_utilities=>newline &&
    '  padding: 0;' && cl_abap_char_utilities=>newline &&
    '  margin: 0;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '' && cl_abap_char_utilities=>newline &&
    '#myUL li a {' && cl_abap_char_utilities=>newline &&
    '  border: 1px solid #ddd;' && cl_abap_char_utilities=>newline &&
    '  margin-top: -1px; ' && cl_abap_char_utilities=>newline &&
    '  background-color: #DFEBF5;' && cl_abap_char_utilities=>newline &&
    '  padding: 12px;' && cl_abap_char_utilities=>newline &&
    '  text-decoration: none;' && cl_abap_char_utilities=>newline &&
    '  font-size: 12px;' && cl_abap_char_utilities=>newline &&
    '  color: black;' && cl_abap_char_utilities=>newline &&
    '  display: block' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '' && cl_abap_char_utilities=>newline &&
    '#myUL li a:hover:not(.header) {' && cl_abap_char_utilities=>newline &&
    '  background-color: #eee;' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '</style>' && cl_abap_char_utilities=>newline &&

    '<script>' && cl_abap_char_utilities=>newline &&
      'document.ready( function(event) { ' && cl_abap_char_utilities=>newline &&
      '   document.getElementById("myInput").focus( );' && cl_abap_char_utilities=>newline &&
      '});' && cl_abap_char_utilities=>newline &&
      'window.scrollTo(0, 0); ' && cl_abap_char_utilities=>newline &&
    '</script>' && cl_abap_char_utilities=>newline &&

    '</head>' && cl_abap_char_utilities=>newline &&
    '<body>' && cl_abap_char_utilities=>newline &&

    '<input type="text" id="myInput" onkeyup="myFunction()" placeholder="Procurar por nome do material.." title="Pesquisar Material">' && cl_abap_char_utilities=>newline &&

    '<ul id="myUL">' && cl_abap_char_utilities=>newline &&

    lista_material && cl_abap_char_utilities=>newline &&

    '</ul>' && cl_abap_char_utilities=>newline &&

    '<script>' && cl_abap_char_utilities=>newline &&
    'function myFunction() {' && cl_abap_char_utilities=>newline &&
    '    var input, filter, ul, li, a, i, txtValue;' && cl_abap_char_utilities=>newline &&
    '    input = document.getElementById("myInput");' && cl_abap_char_utilities=>newline &&
    '    filter = input.value.toUpperCase();' && cl_abap_char_utilities=>newline &&
    '    ul = document.getElementById("myUL");' && cl_abap_char_utilities=>newline &&
    '    li = ul.getElementsByTagName("li");' && cl_abap_char_utilities=>newline &&
    '    for (i = 0; i < li.length; i++) {' && cl_abap_char_utilities=>newline &&
    '        a = li[i].getElementsByTagName("a")[0];' && cl_abap_char_utilities=>newline &&
    '        txtValue = a.textContent || a.innerText;' && cl_abap_char_utilities=>newline &&
    '        if (txtValue.toUpperCase().indexOf(filter) > -1) {' && cl_abap_char_utilities=>newline &&
    '            li[i].style.display = "";' && cl_abap_char_utilities=>newline &&
    '        } else {' && cl_abap_char_utilities=>newline &&
    '            li[i].style.display = "none";' && cl_abap_char_utilities=>newline &&
    '        }' && cl_abap_char_utilities=>newline &&
    '    }' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '</script>' && cl_abap_char_utilities=>newline &&

    '</body>' && cl_abap_char_utilities=>newline &&
    '</html>'.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_0100 .

  CLEAR: evt_receiver.

  IF html_control IS NOT INITIAL.
    html_control->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: html_control.

  IF ctl_cccontainer IS NOT INITIAL.
    ctl_cccontainer->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: ctl_cccontainer.

  IF splitter IS NOT INITIAL.
    splitter->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: splitter.

ENDFORM.
