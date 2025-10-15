*----------------------------------------------------------------------*
***INCLUDE ZMMR153_USER_8001.
*----------------------------------------------------------------------*

DATA: WA_ZSDT0001PD TYPE ZSDT0001PD.
"DATA: CONTAINER_8001 TYPE REF TO CL_GUI_CONTAINER.
DATA: CONTAINER_8001_HTML TYPE REF TO CL_GUI_CONTAINER.
"DATA: SPLITTER_8001 TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
"DATA: HTML_CONTROL_8001 TYPE REF TO CL_GUI_HTML_VIEWER.
DATA: HTMLNOTAFISCAL TYPE STRING.

DATA: LV_CHAVE_PBO     TYPE ZDE_CHAVE_NFE.
DATA: LV_CHAVE_PAI     TYPE ZDE_CHAVE_NFE.
DATA: OK_SUPPRESS_8001 TYPE C.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_8001_EXIT INPUT.

  PERFORM LIMPAR_8001.

  CLEAR: OK_CODE, CK_ADD_NOTA.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_8001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_8001 OUTPUT.

  DATA: DATA_TABLE_8001 TYPE STANDARD TABLE OF TEXT255,
        I_URL_8001      TYPE C LENGTH 200.

  IF OK_SUPPRESS_8001 EQ ABAP_TRUE.
    SUPPRESS DIALOG.
  ENDIF.

  SET PF-STATUS 'PF8001'.
  SET TITLEBAR 'TL8001'.

*  IF CONTAINER_8001 IS INITIAL.
*    CREATE OBJECT CONTAINER_8001 TYPE CL_GUI_CUSTOM_CONTAINER
*      EXPORTING
*        CONTAINER_NAME = 'ALV_PDF_NOTA'
*      EXCEPTIONS
*        OTHERS         = 1.
*
*    CREATE OBJECT SPLITTER_8001
*      EXPORTING
*        PARENT  = CONTAINER_8001
*        ROWS    = 1
*        COLUMNS = 1.
*
*    SPLITTER_8001->GET_CONTAINER(
*      EXPORTING
*        ROW       = 1
*        COLUMN    = 1
*      RECEIVING
*        CONTAINER = CONTAINER_8001_HTML
*    ).
*
*    CREATE OBJECT HTML_CONTROL_8001
*      EXPORTING
*        PARENT = CONTAINER_8001_HTML.
*  ENDIF.

  IF ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL IS INITIAL.
    ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL = ZIF_CARGA=>ST_MODEL_FISCAL_PAPEL.
  ENDIF.

  IF LV_CHAVE_PBO IS INITIAL AND LV_CHAVE_PAI IS INITIAL.
    LV_CHAVE_PAI = ZDE_ZSDT0001NT_ALV-NR_CHAVE_NFE.
  ENDIF.

  LOOP AT SCREEN.
    SPLIT SCREEN-NAME AT '-' INTO: DATA(STR1_8001) DATA(STR2_8001).
    IF STR1_8001 EQ 'ZDE_ZSDT0001NT_ALV'.

      OBJETO->VALIDA_ATRIBUTO_ALTERAVEL(
          EXPORTING
            I_CAMPO         = CONV #( STR2_8001 )
            I_MODELO_FISCAL = ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL
            I_ID_ENTRADA    = ZDE_ZSDT0001NT_ALV-ID_ENTRADA
            I_ID_EMPRESA    = ZDE_ZSDT0001CG_ALV-ID_BUKRS
          IMPORTING
            E_PERMITIDO = DATA(E_PERMITIDO_8001) ).

      IF E_PERMITIDO_8001 EQ ABAP_FALSE.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF NM_FIELD_SET_NOTA IS NOT INITIAL.
    SET CURSOR FIELD NM_FIELD_SET_NOTA OFFSET POS.
    CLEAR NM_FIELD_SET_NOTA.
  ENDIF.

  IF LV_CHAVE_PBO NE LV_CHAVE_PAI AND LV_CHAVE_PAI IS NOT INITIAL.

    LV_CHAVE_PBO = LV_CHAVE_PAI.

*    TRY .
*
*        ZCL_NFE_INBOUND=>DANFE( EXPORTING I_CHAVE_NFE = LV_CHAVE_PBO I_CHAMAR_BROWSER = ABAP_FALSE IMPORTING E_URL = DATA(E_URL) ).
*
*        DATA: LC_URL TYPE C LENGTH 250.
*        LC_URL = E_URL.
*        HTML_CONTROL_8001->SHOW_URL(
*          EXPORTING
*            URL                    = LC_URL
*          EXCEPTIONS
*            CNTL_ERROR             = 1
*            CNHT_ERROR_NOT_ALLOWED = 2
*            CNHT_ERROR_PARAMETER   = 3
*            DP_ERROR_GENERAL       = 4
*            OTHERS                 = 5
*        ).
*
*      CATCH ZCX_NFE_INBOUND_EXCEPTION.    "
*    ENDTRY.
  ELSEIF ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL NE ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.

    PERFORM GERA_HTML_DOCUMENTO.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        I_STRING         = HTMLNOTAFISCAL
        I_TABLINE_LENGTH = 255
      TABLES
        ET_TABLE         = DATA_TABLE_8001.

*    HTML_CONTROL_8001->LOAD_DATA(
*      IMPORTING
*        ASSIGNED_URL           = I_URL_8001
*      CHANGING
*        DATA_TABLE             = DATA_TABLE_8001
*      EXCEPTIONS
*        DP_INVALID_PARAMETER   = 1
*        DP_ERROR_GENERAL       = 2
*        CNTL_ERROR             = 3
*        HTML_SYNTAX_NOTCORRECT = 4
*        OTHERS                 = 5
*    ).
*
*    HTML_CONTROL_8001->SHOW_URL(
*      EXPORTING
*        URL                    = I_URL_8001
*      EXCEPTIONS
*        CNTL_ERROR             = 1
*        CNHT_ERROR_NOT_ALLOWED = 2
*        CNHT_ERROR_PARAMETER   = 3
*        DP_ERROR_GENERAL       = 4
*        OTHERS                 = 5
*    ).
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

  ENDIF.

  IF OK_SUPPRESS_8001 EQ ABAP_TRUE.
    LEAVE TO LIST-PROCESSING.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_ENTRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ENTRA INPUT.

  IF ZDE_ZSDT0001NT_ALV-ID_ENTRADA IS NOT INITIAL.
    SELECT SINGLE DS_ENTRADA INTO ZDE_ZSDT0001NT_ALV-DS_ENTRADA
      FROM ZSDT0001TETX
     WHERE ID_ENTRADA EQ ZDE_ZSDT0001NT_ALV-ID_ENTRADA.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_MODEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_MODEL INPUT.

  IF ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL NE '55' AND ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL NE '1'.
    MESSAGE E055.
  ENDIF.

  IF ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL EQ '1'.
    CLEAR: ZDE_ZSDT0001NT_ALV-NR_CHAVE_NFE.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_FORNE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_FORNE INPUT.

  CHECK ZDE_ZSDT0001NT_ALV-NR_FORNECEDOR_IE IS NOT INITIAL.

  TRY .
      OBJETO->GET_NOTA_FORNECEDOR_IE( EXPORTING I_STCD3 = CONV #( ZDE_ZSDT0001NT_ALV-NR_FORNECEDOR_IE ) IMPORTING E_NOTA = DATA(LC_NOTA_IE) ).

      "Verificar Restrição de Embargos """""""""""""""""""""""""""""""""""""
      ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
        )->SET_PARCEIRO( I_PARCEIRO = LC_NOTA_IE-ID_FORNECEDOR
        )->CK_RESTRICAO_EMBARGO( EXPORTING I_GERA_ERRO = ABAP_FALSE IMPORTING E_RESULTADO   = DATA(E_RESULTADO_RESTRI)
        ).

      IF E_RESULTADO_RESTRI-BLOQUEADO EQ ABAP_TRUE.
        PERFORM APRESENTA_RESTRICAO USING E_RESULTADO_RESTRI.
        MESSAGE E_RESULTADO_RESTRI-MOTIVO TYPE 'E'.
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      ZDE_ZSDT0001NT_ALV-ID_FORNECEDOR = LC_NOTA_IE-ID_FORNECEDOR.
      ZDE_ZSDT0001NT_ALV-DS_FORNECEDOR = LC_NOTA_IE-DS_FORNECEDOR.


    CATCH ZCX_CARGA INTO EX_CARGA.

      IF EX_CARGA->MSGID EQ ZCX_CARGA=>ZCX_FORN_SEM_PARAMETRO-MSGID.

        EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'W' I_MSGTY_DISPLAY = 'W' ).
        WA_ZSDT0001PD-ID_BRANCH    = OBJETO->CARGA-ID_BRANCH.
        WA_ZSDT0001PD-ID_BUKRS     = OBJETO->CARGA-ID_BUKRS.
        WA_ZSDT0001PD-NR_SAFRA     = OBJETO->CARGA-NR_SAFRA.

        TRY .

            ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
              )->SET_PARCEIRO_IE( I_INSC_ESTATUAL = CONV #( ZDE_ZSDT0001NT_ALV-NR_FORNECEDOR_IE )
              )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = WA_ZSDT0001PD-ID_PRODUTOR
              )->CK_ATIVO(
              )->CK_ATIVO_EMPRESA( I_EMPRESA = OBJETO->CARGA-ID_BUKRS
              ).

            INSERT INTO ZSDT0001PD VALUES WA_ZSDT0001PD.
            COMMIT WORK.

          CATCH ZCX_PARCEIROS INTO EX_PARCEIROS.
            EX_PARCEIROS->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
        ENDTRY.

      ELSE.
        EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
      ENDIF.
  ENDTRY.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_NOTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUI_INFO_NOTA INPUT.
  CK_ALTERADO_NOTA = ABAP_TRUE.
ENDMODULE.



*&---------------------------------------------------------------------*
*&      Form  APRESENTA_RESTRICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_RESULTADO_RESTRI  text
*----------------------------------------------------------------------*
FORM APRESENTA_RESTRICAO  USING E_RESTRI TYPE ZDE_PES_RESULTADO_RESTRICAO.

  HTML_PAGINA =
'<!DOCTYPE html>' &&
'<html lang="en">' &&
'<head>' &&
'<title>CSS Template</title>' &&
'<meta charset="utf-8">' &&
'<meta name="viewport" content="width=device-width, initial-scale=1">' &&
'<style>' &&
'* {' &&
'    box-sizing: border-box;' &&
'}' &&
'' &&
'body {' &&
'    font-family: Arial, Helvetica, sans-serif;' &&
'}' &&
'' &&


'header {' &&
'    background-color: #666;' &&
'    padding: 30px;' &&
'    text-align: center;' &&
'    color: white;' &&
'}' &&
'' &&

'nav {' &&
'    float: left;' &&
'    width: 30%;' &&
'    height: 300px; /* only for demonstration, should be removed */' &&
'    background: #ccc;' &&
'    padding: 20px;' &&
'}' &&


'nav ul {' &&
'    list-style-type: none;' &&
'    padding: 0;' &&
'}' &&

'article {' &&
'    float: left;' &&
'    padding: 20px;' &&
'    width: 100%;' &&
'    background-color: #f1f1f1;' &&
'    height: 300px; /* only for demonstration, should be removed */' &&
'}' &&

'section:after {' &&
'    content: "";' &&
'    display: table;' &&
'    clear: both;' &&
'}' &&

'footer {' &&
'    background-color: #777;' &&
'    padding: 10px;' &&
'    text-align: center;' &&
'    color: white;' &&
'}' &&

'@media (max-width: 600px) {' &&
'    nav, article {' &&
'        width: 100%;' &&
'        height: auto;' &&
'    }' &&
'}' &&
'</style>' &&
'</head>' &&
'<body>' &&

'<h2 style="text-align:center;color:Tomato;">Restrição de Embargo</h2>' &&

'<header>' &&
'<h4>' && E_RESTRI-NOME && '</h4>' &&
'</header>' &&

'<section>' &&
'  <article>' &&
    '<h1>' && E_RESTRI-TIPO && '</h1>' &&
    '<p>' && E_RESTRI-MOTIVO && '</p>' &&
'  </article>' &&
'</section>' &&

'<footer>' &&
'  <p>OPUS Crédito</p>' &&
'</footer>' &&

'</body>' &&
'</html>'.

  CALL SCREEN 9006 STARTING AT 05 05 ENDING AT 85 28.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_8001 INPUT.

  CLEAR: OK_SUPPRESS_8001.

  IF OK_CODE_SUPPRESS IS NOT INITIAL.
    OK_CODE = OK_CODE_SUPPRESS.
    CLEAR: OK_CODE_SUPPRESS.
  ENDIF.

  CASE OK_CODE.
    WHEN 'CHNFE'.
      CLEAR: OK_CODE.
      PERFORM INFORMAR_CHAVE_NFE.
    WHEN 'CONF'.

      IF CK_ALTERADO_NOTA EQ ABAP_TRUE.
        CLEAR: OK_CODE.

        TRY .

            IF ZDE_ZSDT0001NT_ALV-ID_FORNECEDOR IS NOT INITIAL.
              TRY .
                  OBJETO->GET_PRODUTOR_FILIAL_SAFRA(
                    EXPORTING
                      ID_PRODUTOR = ZDE_ZSDT0001NT_ALV-ID_FORNECEDOR
                      ID_NR_SAFRA = OBJETO->CARGA-NR_SAFRA
                      ID_BUKRS    = OBJETO->CARGA-ID_BUKRS
                      ID_BRANCH   = OBJETO->CARGA-ID_BRANCH
                    ).

                CATCH ZCX_CARGA INTO EX_CARGA.

                  EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'W' ).

                  OBJETO->SET_ADD_FORN_FILIAL_SAFRA(
                    EXPORTING
                      I_ID_PRODUTOR = ZDE_ZSDT0001NT_ALV-ID_FORNECEDOR    " Produtor Rural
                      I_ID_NR_SAFRA = OBJETO->CARGA-NR_SAFRA    " Safra
                      I_ID_BUKRS    = OBJETO->CARGA-ID_BUKRS    " Empresa Recebimento
                      I_ID_BRANCH   = OBJETO->CARGA-ID_BRANCH    " Filial de Recebimento
                   ).

              ENDTRY.
            ENDIF.

            OBJETO->ADD_NOTA_FISCAL( EXPORTING I_NOTA = ZDE_ZSDT0001NT_ALV IMPORTING E_NOTA = ZDE_ZSDT0001NT_ALV ).
            CK_ALTERADO_NOTA = ABAP_FALSE.
            PERFORM ATUALIZAR_DADOS_TELA.

          CATCH ZCX_PARCEIROS INTO EX_PARCEIROS.  "
            EX_PARCEIROS->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'W' ).
            PERFORM SETA_CAMPO USING EX_PARCEIROS->MSGID EX_PARCEIROS->MSGNO.

          CATCH ZCX_CARGA INTO EX_CARGA.  "
            EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'W' ).
            PERFORM SETA_CAMPO USING EX_CARGA->MSGID EX_CARGA->MSGNO.

        ENDTRY.
        EXIT.
      ENDIF.

      IF IT_NOTAS[] IS INITIAL.
        MESSAGE S264(ZCARGA) DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      CLEAR: OK_CODE.
      CK_ADD_NOTA = ABAP_TRUE.
      PERFORM LIMPAR_8001.
      LEAVE TO SCREEN 0.
    WHEN 'MODELO'.
      CLEAR: OK_CODE.

      IF ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL EQ ZIF_CARGA=>ST_MODEL_FISCAL_ELETRONICO.
        OK_SUPPRESS_8001 = ABAP_TRUE.
        OK_CODE_SUPPRESS = 'CHNFE'.
      ENDIF.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  INFORMAR_CHAVE_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INFORMAR_CHAVE_NFE .

  CHECK ZDE_ZSDT0001CG_ALV-TP_STATUS EQ ZIF_CARGA=>ST_STATUS_ABERTO.

  IF ( ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL NE '55' AND ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL IS NOT INITIAL ).
    MESSAGE S033 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: WA_ADD_NFE_9002.

  CALL SCREEN 9002 STARTING AT 40 10.

  IF WA_ADD_NFE_9002-CK_INCLUIR EQ ABAP_TRUE.
    "Entrar com a chave NF-e
    ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL      = '55'.
    ZDE_ZSDT0001NT_ALV-DT_EMISSAO         = WA_ADD_NFE_9002-DT_EMISSAO.
    ZDE_ZSDT0001NT_ALV-DS_FORNECEDOR      = WA_ADD_NFE_9002-NAME1.
    ZDE_ZSDT0001NT_ALV-ID_FORNECEDOR      = WA_ADD_NFE_9002-PARID.
    ZDE_ZSDT0001NT_ALV-NR_FORNECEDOR_IE   = WA_ADD_NFE_9002-PARID_IE.
    ZDE_ZSDT0001NT_ALV-DT_VENCIMENTO_FORM = WA_ADD_NFE_9002-DT_EMISSAO.
    ZDE_ZSDT0001NT_ALV-NR_NOTA            = WA_ADD_NFE_9002-NUMERO.
    ZDE_ZSDT0001NT_ALV-NM_SERIE           = WA_ADD_NFE_9002-SERIE.
    ZDE_ZSDT0001NT_ALV-NR_VALOR           = WA_ADD_NFE_9002-NFTOT.
    ZDE_ZSDT0001NT_ALV-NR_QUANTIDADE      = WA_ADD_NFE_9002-NTGEW.
    ZDE_ZSDT0001NT_ALV-NR_CHAVE_NFE       = WA_ADD_NFE_9002-N55_CHAVE_ACESSO.
    ZDE_ZSDT0001NT_ALV-CFOP               = WA_ADD_NFE_9002-CFOP.
    ZDE_ZSDT0001NT_ALV-NR_FARDO           = WA_ADD_NFE_9002-NR_FARDO.

    TRY .

        TRY .
            OBJETO->GET_PRODUTOR_FILIAL_SAFRA(
              EXPORTING
                ID_PRODUTOR = ZDE_ZSDT0001NT_ALV-ID_FORNECEDOR
                ID_NR_SAFRA = OBJETO->CARGA-NR_SAFRA
                ID_BUKRS    = OBJETO->CARGA-ID_BUKRS
                ID_BRANCH   = OBJETO->CARGA-ID_BRANCH
              ).

          CATCH ZCX_CARGA INTO EX_CARGA.

            EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'W' ).

            OBJETO->SET_ADD_FORN_FILIAL_SAFRA(
              EXPORTING
                I_ID_PRODUTOR = ZDE_ZSDT0001NT_ALV-ID_FORNECEDOR    " Produtor Rural
                I_ID_NR_SAFRA = OBJETO->CARGA-NR_SAFRA    " Safra
                I_ID_BUKRS    = OBJETO->CARGA-ID_BUKRS    " Empresa Recebimento
                I_ID_BRANCH   = OBJETO->CARGA-ID_BRANCH    " Filial de Recebimento
             ).

        ENDTRY.

        OBJETO->ADD_NOTA_FISCAL( EXPORTING I_NOTA = ZDE_ZSDT0001NT_ALV IMPORTING E_NOTA = ZDE_ZSDT0001NT_ALV ).
        READ TABLE IT_NOTAS WITH KEY ID_NOTA = ZDE_ZSDT0001NT_ALV-ID_NOTA ASSIGNING FIELD-SYMBOL(<FS_NOTA>).
        IF SY-SUBRC IS INITIAL.
          <FS_NOTA> = ZDE_ZSDT0001NT_ALV.
        ELSE.
          APPEND ZDE_ZSDT0001NT_ALV TO IT_NOTAS.
        ENDIF.

        LV_CHAVE_PAI = ZDE_ZSDT0001NT_ALV-NR_CHAVE_NFE.

        CK_ALTERADO_NOTA = ABAP_FALSE.

      CATCH ZCX_PARCEIROS INTO EX_PARCEIROS.    "

        EX_PARCEIROS->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).

      CATCH ZCX_CARGA INTO EX_CARGA.    "

        EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
        PERFORM SETA_CAMPO USING EX_CARGA->MSGID EX_CARGA->MSGNO.

    ENDTRY.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERA_HTML_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GERA_HTML_DOCUMENTO .

  "HTMLNOTAFISCAL = '<html><head></head><body><div><h1>text formatting</h1><p>This text is styled with</p></div></body></html>'.

  DATA(TXL_MODELO) = |Nota Fiscal Modelo { ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL } não possui visualização em PDF |.

  HTMLNOTAFISCAL =
    '<!DOCTYPE html>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<html>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    'html, body {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  height: 100%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  margin: 0;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '.full-height {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  height: 100%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  display: flex;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  background: #DFEBF5;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  font-family: Verdana;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  align-items: center;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    'p {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  margin: auto;  ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '  text-align: center;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<div class="full-height">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '<p>' && TXL_MODELO && '</p>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</div>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
    '</html>'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_8001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_8001 .

*  IF HTML_CONTROL_8001 IS NOT INITIAL.
*    HTML_CONTROL_8001->FREE(
*      EXCEPTIONS
*        CNTL_ERROR        = 1
*        CNTL_SYSTEM_ERROR = 2
*        OTHERS            = 3 ).
*  ENDIF.
*  CLEAR: HTML_CONTROL_8001.
*
*  IF CONTAINER_8001_HTML IS NOT INITIAL.
*    CONTAINER_8001_HTML->FREE(
*      EXCEPTIONS
*        CNTL_ERROR        = 1
*        CNTL_SYSTEM_ERROR = 2
*        OTHERS            = 3 ).
*  ENDIF.
*  CLEAR: CONTAINER_8001_HTML.
*
*  IF SPLITTER_8001 IS NOT INITIAL.
*    SPLITTER_8001->FREE(
*      EXCEPTIONS
*        CNTL_ERROR        = 1
*        CNTL_SYSTEM_ERROR = 2
*        OTHERS            = 3 ).
*  ENDIF.
*  CLEAR: SPLITTER_8001.
*
*  IF CONTAINER_8001 IS NOT INITIAL.
*    CONTAINER_8001->FREE(
*      EXCEPTIONS
*        CNTL_ERROR        = 1
*        CNTL_SYSTEM_ERROR = 2
*        OTHERS            = 3 ).
*  ENDIF.
*  CLEAR: CONTAINER_8001.

ENDFORM.
