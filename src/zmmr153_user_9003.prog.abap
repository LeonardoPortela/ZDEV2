*----------------------------------------------------------------------*
***INCLUDE ZMMR153_USER_9003.
*----------------------------------------------------------------------*

CLASS CL_MYEVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS: ON_SAPEVENT FOR EVENT SAPEVENT OF CL_GUI_HTML_VIEWER IMPORTING ACTION FRAME GETDATA POSTDATA QUERY_TABLE.
ENDCLASS.

DATA: SPLITTER_HTML_9003        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CCCONTAINER_HTML_9003 TYPE REF TO CL_GUI_CONTAINER,
      HTML_CONTROL_HTML_9003    TYPE REF TO CL_GUI_HTML_VIEWER,
      HTML_PAGINA_9003          TYPE STRING.

DATA: HTML_CONTROL_9004 TYPE REF TO CL_GUI_HTML_VIEWER,
      EVT_RECEIVER_9004 TYPE REF TO CL_MYEVENT_HANDLER,
      MYEVENT           TYPE CNTL_SIMPLE_EVENT,
      MYEVENT_TAB       TYPE CNTL_SIMPLE_EVENTS,
      HTML_CONFIRMACAO  TYPE STRING.

CLASS CL_MYEVENT_HANDLER IMPLEMENTATION.

  METHOD ON_SAPEVENT.

    DATA: LC_AUX_FARDOS TYPE ZDE_NM_FARDOS.

    CHECK ACTION EQ 'SUBMIT_FORM_AS_POST_METHOD'.

    DATA(IT_BLOCOS) = IT_BLOCOS_VINCU[].
    SORT IT_BLOCOS BY NM_BLOCO.
    DELETE ADJACENT DUPLICATES FROM IT_BLOCOS COMPARING NM_BLOCO.

    LOOP AT IT_BLOCOS ASSIGNING FIELD-SYMBOL(<FS_BLOCO>).
      <FS_BLOCO>-QT_FARDOS = 0.
      <FS_BLOCO>-QT_FARDOS = REDUCE ZDE_NM_FARDOS( INIT I TYPE ZDE_NM_FARDOS FOR FD IN IT_BLOCOS_VINCU WHERE ( NM_BLOCO = <FS_BLOCO>-NM_BLOCO )
                               NEXT I = I + FD-QT_FARDOS ).
    ENDLOOP.

    LOOP AT QUERY_TABLE INTO DATA(WA_QUERY_TABLE).

      LC_AUX_FARDOS = WA_QUERY_TABLE-VALUE.

      IF WA_QUERY_TABLE-NAME CS 'FARDOS'.
        IF ZDE_ZSDT0001NT_ALV-NR_FARDO NE LC_AUX_FARDOS.
          MESSAGE S279 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ELSEIF WA_QUERY_TABLE-NAME CS 'BLOCO'.
        DATA(TAMANHO) = ZCL_STRING=>LENGTH( TEXT = CONV #( WA_QUERY_TABLE-NAME ) ).
        TAMANHO = TAMANHO - 4.
        WA_QUERY_TABLE-NAME = WA_QUERY_TABLE-NAME+5(TAMANHO).
        READ TABLE IT_BLOCOS INTO DATA(WA_BLOCOS) WITH KEY NM_BLOCO = WA_QUERY_TABLE-NAME.
        IF SY-SUBRC IS NOT INITIAL.
          MESSAGE S281 DISPLAY LIKE 'E'.
          EXIT.
        ELSEIF WA_BLOCOS-QT_FARDOS NE LC_AUX_FARDOS.
          MESSAGE S280 DISPLAY LIKE 'E' WITH WA_QUERY_TABLE-NAME.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CK_VALIDACAO_CONFERENCIA = ABAP_TRUE .
    CK_VALIDACAO_SAIDA_AUTOM = ABAP_TRUE .

    PERFORM LIMPAR_TELA_9004.
    LEAVE TO SCREEN 0.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9003 OUTPUT.

  SET TITLEBAR 'TL9003'.

  SUPPRESS DIALOG.

  IF SPLITTER_HTML_9003 IS INITIAL.

    CREATE OBJECT SPLITTER_HTML_9003
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER_HTML_9003->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER_HTML_9003.

    CREATE OBJECT HTML_CONTROL_HTML_9003
      EXPORTING
        PARENT = CTL_CCCONTAINER_HTML_9003.

    DATA: DATA_TABLE TYPE STANDARD TABLE OF TEXT255,
          I_URL      TYPE C LENGTH 200.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        I_STRING         = ZCL_UTIL=>GET_HTML_FUNDO( )
        I_TABLINE_LENGTH = 255
      TABLES
        ET_TABLE         = DATA_TABLE.

    HTML_CONTROL_HTML_9003->LOAD_DATA(
      IMPORTING
        ASSIGNED_URL           = I_URL
      CHANGING
        DATA_TABLE             = DATA_TABLE
      EXCEPTIONS
        DP_INVALID_PARAMETER   = 1
        DP_ERROR_GENERAL       = 2
        CNTL_ERROR             = 3
        HTML_SYNTAX_NOTCORRECT = 4
        OTHERS                 = 5
    ).

    HTML_CONTROL_HTML_9003->SHOW_URL(
      EXPORTING
        URL                    = I_URL
      EXCEPTIONS
        CNTL_ERROR             = 1
        CNHT_ERROR_NOT_ALLOWED = 2
        CNHT_ERROR_PARAMETER   = 3
        DP_ERROR_GENERAL       = 4
        OTHERS                 = 5
    ).
  ENDIF.

  LEAVE TO LIST-PROCESSING.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9003 INPUT.

  CALL SCREEN 9004 STARTING AT 5 3.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9004 OUTPUT.

  SET PF-STATUS 'PF9004'.
  SET TITLEBAR 'TL9004'.

  DATA: DATA_TABLE_9004 TYPE STANDARD TABLE OF TEXT255,
        I_URL_9004      TYPE C LENGTH 200.

  CLEAR: DATA_TABLE_9004[], DATA_TABLE_9004.

  IF HTML_CONTROL_9004 IS INITIAL.

    CREATE OBJECT HTML_CONTROL_9004
      EXPORTING
        PARENT = CL_GUI_CONTAINER=>SCREEN1.

    PERFORM GERAR_HTML_CONFE.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        I_STRING         = HTML_CONFIRMACAO
        I_TABLINE_LENGTH = 255
      TABLES
        ET_TABLE         = DATA_TABLE_9004.

    HTML_CONTROL_9004->LOAD_DATA(
      IMPORTING
        ASSIGNED_URL           = I_URL_9004
      CHANGING
        DATA_TABLE             = DATA_TABLE_9004
      EXCEPTIONS
        DP_INVALID_PARAMETER   = 1
        DP_ERROR_GENERAL       = 2
        CNTL_ERROR             = 3
        HTML_SYNTAX_NOTCORRECT = 4
        OTHERS                 = 5
    ).

    MYEVENT-EVENTID = HTML_CONTROL_9004->M_ID_SAPEVENT.
    MYEVENT-APPL_EVENT = ABAP_TRUE.
    APPEND MYEVENT TO MYEVENT_TAB.

    HTML_CONTROL_9004->SET_REGISTERED_EVENTS(
      EXPORTING
        EVENTS                    = MYEVENT_TAB
      EXCEPTIONS
        CNTL_ERROR                = 1
        CNTL_SYSTEM_ERROR         = 2
        ILLEGAL_EVENT_COMBINATION = 3
        OTHERS                    = 4
    ).

    CREATE OBJECT EVT_RECEIVER_9004.
    SET HANDLER EVT_RECEIVER_9004->ON_SAPEVENT FOR HTML_CONTROL_9004.

    HTML_CONTROL_9004->SHOW_DATA(
      EXPORTING
        URL                    = I_URL_9004
      EXCEPTIONS
        CNTL_ERROR             = 1
        CNHT_ERROR_NOT_ALLOWED = 2
        CNHT_ERROR_PARAMETER   = 3
        DP_ERROR_GENERAL       = 4
        OTHERS                 = 5
    ).

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GERAR_HTML_CONFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GERAR_HTML_CONFE .

  DATA: HTML_BLOCOS TYPE STRING,
        JAVA_SCRIPT TYPE STRING.

  PERFORM ATUALIZAR_DADOS_TELA.

  DATA(IT_BLOCOS) = IT_BLOCOS_VINCU[].
  SORT IT_BLOCOS BY NM_BLOCO.
  DELETE ADJACENT DUPLICATES FROM IT_BLOCOS COMPARING NM_BLOCO.

  LOOP AT IT_BLOCOS ASSIGNING FIELD-SYMBOL(<FS_BLOCO>).
    <FS_BLOCO>-QT_FARDOS = 0.
    <FS_BLOCO>-QT_FARDOS = REDUCE ZDE_NM_FARDOS( INIT I TYPE ZDE_NM_FARDOS FOR FD IN IT_BLOCOS_VINCU WHERE ( NM_BLOCO = <FS_BLOCO>-NM_BLOCO )
                             NEXT I = I + FD-QT_FARDOS ).
  ENDLOOP.

  JAVA_SCRIPT = '<script>'  && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  'function validateForm() {'  && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  'var fardos = document.forms["myForm"]["FARDOS"].value;' &&
  'if (fardos == "") {' &&
  '  alert("Deve ser Informado a Quantidade Total de Fardos da Carga!"); ' &&
  '  document.getElementById(''FARDOS'').focus();' &&
  '  return false; ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '} ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE.

  JAVA_SCRIPT = JAVA_SCRIPT && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  'if (fardos == "") {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  alert("Deve ser Informado a Quantidade Total de Fardos da Carga!"); ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  document.getElementById(''FARDOS'').focus();' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  document.getElementById(''FARDOS'').select();' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  return false; ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '} '  && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

  'if (fardos != "' && ZDE_ZSDT0001NT_ALV-NR_FARDO && '") {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  alert("A Quantidade Informado de Fardos não Confere com a Quantidade Total de Fardos da Carga!"); ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  document.getElementById(''FARDOS'').focus();' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  document.getElementById(''FARDOS'').select();' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  return false; ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '} '.

  LOOP AT IT_BLOCOS INTO DATA(WA_BLOCOS).

    DATA(ID_BLOCO) = 'BLOCO' && WA_BLOCOS-NM_BLOCO.
    HTML_BLOCOS = HTML_BLOCOS &&
    '  <tr>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '    <td>Bloco</td>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '    <td>' && WA_BLOCOS-NM_BLOCO && '</td>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '    <td>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '       <div class="input-container">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '          <input class="input-field" type="number"  placeholder="Qtd. Fardos do Bloco" name="' && ID_BLOCO && '"  id="' && ID_BLOCO && '" value="">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '       </div>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '    </td>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  </tr>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE.

    DATA(LC_VARIAVEL) = | var { ID_BLOCO } = document.forms["myForm"]["{ ID_BLOCO }"].value; |.
    DATA(LC_ALERT) = | alert("Deve ser Informado a Quantidade do Bloco { WA_BLOCOS-NM_BLOCO }!");  |.

    JAVA_SCRIPT = JAVA_SCRIPT && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    LC_VARIAVEL &&
    'if ( ' && ID_BLOCO && ' == "") {' &&
    LC_ALERT &&
    '  document.getElementById(''' && ID_BLOCO && ''').focus();' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  document.getElementById(''' && ID_BLOCO && ''').select();' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  return false; ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '} '.

    DATA(LC_ALERT_2) = | alert("A Quantidade Informado de Fardos do Bloco { WA_BLOCOS-NM_BLOCO }  não Está de Acordo!"); |.

    JAVA_SCRIPT = JAVA_SCRIPT && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    'if ( ' && ID_BLOCO && ' != "' && WA_BLOCOS-QT_FARDOS && '") {' &&
    LC_ALERT_2 &&
    '  document.getElementById(''' && ID_BLOCO && ''').focus();' &&
    '  document.getElementById(''' && ID_BLOCO && ''').select();' &&
    '  return false; ' &&
    '} '.

  ENDLOOP.

  JAVA_SCRIPT = JAVA_SCRIPT && '  return true;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE.
  JAVA_SCRIPT = JAVA_SCRIPT && '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE.
  JAVA_SCRIPT = JAVA_SCRIPT && '</script>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE.

  HTML_CONFIRMACAO =
    '<!DOCTYPE html>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<html>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    JAVA_SCRIPT &&
    '<style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    'body {font-family: Verdana;}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '* {box-sizing: border-box;}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    'table {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  font-family: Verdana;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  border-collapse: collapse;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  width: 100%;  ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  font-size: 11px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    'tr:nth-child(even) {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  background-color: #DFEBF5;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    'td, th {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  text-align: rigth;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  padding: 4px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '.btn {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  background-color: #DFEBF5;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  font-family: Verdana;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  color: black;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  padding: 8px 8px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  border: none;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  cursor: pointer;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  width: 100%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  font-size: 11px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '.btn:focus {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  border: 1px solid black;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '.input-container {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  display: -ms-flexbox;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  display: flex;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  width: 100%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  font-size: 11px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '.input-field {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  width: 100%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  font-family: Verdana;  ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  padding: 2px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  outline: none;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  text-align: right;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  border: 1px solid #DFEBF5;  ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  font-size: 11px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '.input-field:focus {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  border: 1px solid black;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    'h4 {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  font-size: 11px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<h4>Conferência de Carga</h4>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<form name="myForm" method=post action="SAPEVENT:SUBMIT_FORM_AS_POST_METHOD" onsubmit="return validateForm()" >' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<table>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  <tr>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '    <th></th>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '    <th>Fardos:</th>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '    <th>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '       <div class="input-container">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '          <input class="input-field" type="text" name="FARDOS" id="FARDOS" placeholder="Total de Fardos" value="">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '       </div> ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '    </th>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  </tr>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    HTML_BLOCOS && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  </table>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  <p/>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  <input type="submit" value="Conferir" class="btn">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</form> ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</html>'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9004_EXIT INPUT.

  CLEAR: CK_VALIDACAO_CONFERENCIA, CK_VALIDACAO_SAIDA_AUTOM.
  PERFORM LIMPAR_TELA_9004.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TELA_9003 .

  IF HTML_CONTROL_HTML_9003 IS NOT INITIAL.
    HTML_CONTROL_HTML_9003->FREE(
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: HTML_CONTROL_HTML_9003.

  IF CTL_CCCONTAINER_HTML_9003 IS NOT INITIAL.
    CTL_CCCONTAINER_HTML_9003->FREE(
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: CTL_CCCONTAINER_HTML_9003.

  IF SPLITTER_HTML_9003 IS NOT INITIAL.
    SPLITTER_HTML_9003->FREE(
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: SPLITTER_HTML_9003.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_9004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_TELA_9004 .

  CLEAR: EVT_RECEIVER_9004.

  IF HTML_CONTROL_9004 IS NOT INITIAL.
    HTML_CONTROL_9004->FREE(
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR HTML_CONTROL_9004.

ENDFORM.
