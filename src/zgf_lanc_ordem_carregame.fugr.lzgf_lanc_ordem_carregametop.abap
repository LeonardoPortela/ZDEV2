FUNCTION-POOL ZGF_LANC_ORDEM_CARREGAME.     "MESSAGE-ID ..

* INCLUDE LZGF_LANC_ORDEM_CARREGAMED...      " Local class definition

TABLES: ZDE_ZSDT0001OD_ALV, ZLEST0185.

DATA: CONTAINER_0100  TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: CK_ALTERADO_FRETE TYPE C,
      HTMLLOCALIZADOR   TYPE STRING,
      LC_TIPO_FRETE     TYPE ZDE_TP_FRETE,
      OK_CODE           TYPE SY-UCOMM,
      LC_NR_SAFRA       TYPE ZDE_NR_SAFRA,
      LC_ID_BUKRS       TYPE BUKRS,
      LC_ID_BRANCH      TYPE ZDE_BRANCH_RECEB,
      LC_ID_AGENT_FRETE TYPE  ZDE_ID_AGENT_FRETE,
      LC_SELECIONOU     TYPE C.

DATA: SPLITTER_HTML1        TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      CTL_CCCONTAINER_HTML1 TYPE REF TO CL_GUI_CONTAINER,
      HTML_CONTROL_HTML1    TYPE REF TO CL_GUI_HTML_VIEWER,
      HTML_PAGINA           TYPE STRING.

*&---------------------------------------------------------------------*
*&      Form  LANCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_ID_ORDEM  text
*      <--P_E_ORDEM_ALV  text
*----------------------------------------------------------------------*
FORM LANCAMENTO
       USING I_TIPO_FRETE TYPE  ZDE_TP_FRETE
             I_FUNDO TYPE  CHAR01
       CHANGING P_E_ID_ORDEM  TYPE ZDE_ID_ORDEM
                P_E_ORDEM_ALV TYPE ZDE_ZSDT0001OD_ALV
                P_E_ID_AGENT_FRETE TYPE  ZDE_ID_AGENT_FRETE.

  LC_TIPO_FRETE = I_TIPO_FRETE.
  LC_NR_SAFRA   = ZDE_ZSDT0001OD_ALV-NR_SAFRA.
  LC_ID_BUKRS   = ZDE_ZSDT0001OD_ALV-ID_BUKRS.
  LC_ID_BRANCH  = ZDE_ZSDT0001OD_ALV-ID_BRANCH.
  LC_ID_AGENT_FRETE = P_E_ID_AGENT_FRETE.

  IF I_FUNDO EQ ABAP_TRUE.
    OK_CODE = 'OPEN'.
    CALL SCREEN 0101.
  ELSE.
    CALL SCREEN 0100 STARTING AT 50 5.
  ENDIF.

  IF LC_SELECIONOU EQ ABAP_TRUE.
    P_E_ID_ORDEM = ZDE_ZSDT0001OD_ALV-ID_ORDEM.
    P_E_ID_AGENT_FRETE = ZDE_ZSDT0001OD_ALV-ID_AGENT_FRETE.
    P_E_ORDEM_ALV = ZDE_ZSDT0001OD_ALV.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUI_INFO_FRETE INPUT.
  CK_ALTERADO_FRETE = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  CL_ABAP_BROWSER=>CLOSE_BROWSER( ).
  CLEAR: ZDE_ZSDT0001OD_ALV.
  PERFORM LIMPAR_0100.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_0100 .

  CL_ABAP_BROWSER=>CLOSE_BROWSER( ).

  IF CONTAINER_0100 IS NOT INITIAL.
    CONTAINER_0100->FREE(
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3 ).
  ENDIF.
  CLEAR: CONTAINER_0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF LC_TIPO_FRETE EQ ZIF_CARGA=>ST_TP_FRETE_CIF OR LC_TIPO_FRETE IS INITIAL.
    DATA(CK_EXIGE_OC) = ABAP_TRUE.
  ELSE.
    CK_EXIGE_OC = ABAP_FALSE.
  ENDIF.

  LOOP AT SCREEN.
    SPLIT SCREEN-NAME AT '-' INTO: DATA(STR1A_0100) DATA(STR2A_0100).
    IF STR1A_0100 EQ 'ZDE_ZSDT0001OD_ALV'.
      CASE CK_EXIGE_OC.
        WHEN ABAP_TRUE.
          IF STR2A_0100 EQ 'DS_PLACA_TRATOR'  OR
             STR2A_0100 EQ 'DS_PLACA_REBOQ_1' OR
             STR2A_0100 EQ 'DS_PLACA_REBOQ_2' OR
             STR2A_0100 EQ 'DS_PLACA_REBOQ_3' OR
             STR2A_0100 EQ 'ID_MOTORISTA'     OR
             STR2A_0100 EQ 'ID_PROPRIETARIO'.
            SCREEN-INPUT = 0.
          ENDIF.
        WHEN ABAP_FALSE.
          IF STR2A_0100 EQ 'NR_ORDEM' OR STR2A_0100 EQ 'DS_MOTORISTA'.
            SCREEN-INPUT = 0.
          ENDIF.
      ENDCASE.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

  IF ZDE_ZSDT0001OD_ALV-ID_ORDEM IS INITIAL.
    CLEAR: ZLEST0185.
  ELSE.
    IF CONTAINER_0100 IS INITIAL.
      DATA(CK_VERIFICAR_LOCALIZADOR) = ABAP_TRUE.
    ELSE.
      CK_VERIFICAR_LOCALIZADOR = ABAP_FALSE.
    ENDIF.
  ENDIF.

  IF CK_ALTERADO_FRETE EQ ABAP_TRUE OR CK_VERIFICAR_LOCALIZADOR EQ ABAP_TRUE.
    CK_ALTERADO_FRETE = ABAP_FALSE.

    IF ZDE_ZSDT0001OD_ALV-ID_ORDEM IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(WA_ZLEST0185)
        FROM ZLEST0185
       WHERE ID_ORDEM EQ @ZDE_ZSDT0001OD_ALV-ID_ORDEM.

      IF SY-SUBRC IS INITIAL.
        ZLEST0185 = WA_ZLEST0185.

        IF CONTAINER_0100 IS INITIAL.
          CREATE OBJECT CONTAINER_0100
            EXPORTING
              CONTAINER_NAME = 'LOCALIZADOR'
            EXCEPTIONS
              OTHERS         = 1.
        ENDIF.

        PERFORM GERA_HTML_LOCALIZADOR.

        CL_ABAP_BROWSER=>CLOSE_BROWSER( ).

        CL_ABAP_BROWSER=>SHOW_HTML(
         EXPORTING
           HTML_STRING = HTMLLOCALIZADOR
           MODAL       = ABAP_FALSE
           FORMAT      = CL_ABAP_BROWSER=>LANDSCAPE
           SIZE        = CL_ABAP_BROWSER=>MIDDLE
           CONTAINER   = CONTAINER_0100 ).

      ENDIF.
    ELSE.
      CLEAR: ZLEST0185.
    ENDIF.
  ENDIF.

  IF ZDE_ZSDT0001OD_ALV-ID_AGENT_FRETE IS NOT INITIAL.
    SELECT SINGLE * INTO @DATA(WA_LFA1)
      FROM LFA1
     WHERE LIFNR EQ @ZDE_ZSDT0001OD_ALV-ID_AGENT_FRETE.
    ZDE_ZSDT0001OD_ALV-DS_AGENT_FRETE = WA_LFA1-NAME1.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GERA_HTML_LOCALIZADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GERA_HTML_LOCALIZADOR .

  HTMLLOCALIZADOR =
  '<!DOCTYPE html> ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '<html>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '<head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '<style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  'html, body {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  background-color: #DFEBF5;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  overflow: hidden;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '.center {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  width: auto;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  height: auto;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  border: 2px solid black;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  padding: 2px 36px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  border-radius: 5px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  font-size: 12px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  font-family: "Verdana";  ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  border-style: dashed;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  text-align: center;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  font-weight: bold;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '.success {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  border-color: #53A855;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  color: #53A855;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '  background-color: #D9FAD9;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '}' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '</style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '</head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '<body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '<div class="center success">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  ZLEST0185-ID_LOCALIZADOR && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '</div>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '</body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
  '</html> '.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.

  SET TITLEBAR 'TL0101'.

  SUPPRESS DIALOG.

  IF SPLITTER_HTML1 IS INITIAL.

    PERFORM GERAR_HTML.

    CREATE OBJECT SPLITTER_HTML1
      EXPORTING
        PARENT  = CL_GUI_CONTAINER=>SCREEN0 "CTL_CCCONTAINER
        ROWS    = 1
        COLUMNS = 1.

    CALL METHOD SPLITTER_HTML1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CTL_CCCONTAINER_HTML1.

    CREATE OBJECT HTML_CONTROL_HTML1
      EXPORTING
        PARENT = CTL_CCCONTAINER_HTML1.

    DATA: DATA_TABLE TYPE STANDARD TABLE OF TEXT255,
          I_URL      TYPE C LENGTH 200.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        I_STRING         = HTML_PAGINA
        I_TABLINE_LENGTH = 255
      TABLES
        ET_TABLE         = DATA_TABLE.

    HTML_CONTROL_HTML1->LOAD_DATA(
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

    HTML_CONTROL_HTML1->SHOW_URL(
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
*&      Form  GERAR_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GERAR_HTML .

  DATA(IMAGEM_AMAGGI) =
  'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOAAAAAiCAYAAABGDdVeAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsQAAA7EAZUrDhsAACYxSURBVHhe7VwJfFXVmf/yspKVJATCIigoCEpVlCquaKeMiFupI67oiExB0CqCdBhaRBlaxCKtIq' &&
  'jFWtx1BKFFGGfEBSUqiyiCsgjImhAe2ffkZf7/75zz3n0vLyGBznTmN/z18N6799xzvvPtZ7mJaQTkBE7gBP4m8NnPEziBE/gboI0RsNZ+OruNQeF3frYO9Shxwi7xTMDTtc/+5qc0mGsSaz+PEYGA+Wz0+JmYRgn4zPUA2o/Tb21DwNLnC9JJkNa20OvGHskH0hbpF+04IhGw9cAz0m' &&
  'ToSQhvz/Xji2yTcPUCYT2Ydny4xnZDY4oLG69BfRgPKV3TUj3psDDy5nW0FUabQT3oj/M2DRkpVBeODvZKBPWKUF0yX7XrRk9bsaF6TZ4Nk4WnLUJ/e+5rG+Z3Pb4fiy61yQApZK9AiLZ2Gs0AnUFEwtcmhY4GGJtyP4SQGrpvlsltQqMyPBLHIoAgLC+ojNFwtLa9ihQ5ZiI6L9mnqc' &&
  't/nUOhjCPlTIQU3H1aqLKb665vn/I3ko7oBliDZ9lbnG2nBt/1N2+2Ahw7KXcUOzojjcv9bg6mnjGmSPCeu+7qEbzmfEei/WwL2mSAxngsHNODzG8dTBuuS/eci6wUWUj4x6XQgFcwVK5gxArEQ1fQNz0t9IRKYxSm9eNw46ayOCRaQR8rSG+QRiD03YygCQJVuEwu4X6A/aKoMzPPOT' &&
  '7yMzovjcGYlskI0M9swTwGsB2OyRkS+BYJ5QPr8Dvq++zDAXBGaSNMNG3eCdTpNyd7/mJPxyx/0kR4ZExDYbscqzEU1sF9HbOr19RoWS/M8MgKF011OKxH2GttRNtSUJfSBcm08DHtwb2oaU4kTHdeL2MG5hlI2ACPA05/rRCMsqBtx0R3XUXDzlrbIRqmwkNZ6/FIHB1IADzxUbRtJd' &&
  'oImKTG419fUHGbpoCRIN9CEcdCDZEgfe47ELUZ9mp6DvKEiKEsnTwcYiOlrgg5U0OHGiFhCfLSH93J4fmg7Al7vw1OPfx5Cz5vSVE5Eyp7fA/K3vObAKlBA4y4HozKEddV790UJzTUVqONBmgiVQBerga9OU/VTo2PRFnCWoTpzikdxWb8n2lLPZ8b5DEMKAxOAEAN2iJzWZJQ2C/Zpk' &&
  'qvFdlZ6zpkG6SZbfBJw4NKNMh5Txv8NscJImrAC7bFQrD9/eUVUlleK9v3+6W0xC/lleXmpkVqcqokJCbIyZ1zJS05SZJTE6RdSoqkgm3JuG/GBmocD6IODTdhMJRnna3g6MBopBA0FBwqld0H86W2pjYqDekZ2ZKbmaI0dM5Ol4T4WO3fgXQkWh7TGENRMFwPCHcnzjn6ozl0W68K9a' &&
  'iZoTwqBCcRukbVK/Rbgfocn4ObrXqfT0dJRH1OCWh83vqR4HhTzNc2o3kDVOUINygqBolZv2OPLHzjL1JYWiXXDblY7rzifGWey+FZr3k1NN3Rc9OIj9Q1yNNvLJX1X38jfbp1lp/dMUJ6pKZIOxVaVK1pBm4YXpopdJFytLMtv0iefP5VKSgrklsuHShDhw5Rxmk/GnWaMx5vCkJV8U' &&
  'kpfj/5b+/KJ+s2SZfcLjLmpivljNxMtBUN0egCrDKQXzTAavz6EnxdvfFbWf7ZFsnbdQQMT9WqRoucmlpQ+ZhK1zMNxb3GWhnUM1fap8fLmb1Okr49TpL+vU6WvqArRVWL5hhBA8eDsdf7ktWhUBbrv/pGlq3eJB9/vUO2FlvXGAf1jWYMjgaOJYARxFQpDf26dpJTumVL/1O6yLk/6C' &&
  'tZMMqQgzI0cPGGI1J92nlQ/jMvT8qLi+WRcXdJGmWitY4ifzXAGHn367Xy7Orleikhne41HBf1/IFcecGlkhubpr9nvTxfthTnR61L1JZWy6wxEyUH9fP9B+WRRU9LoIPXrYTjzKxuMnrY7ZITxWaOhugGyIaCYZoX8F0bb5B1+WUycOwjIhkn4zqYX1Qgk0acK9NuuVZSlCH18DAJxu' &&
  'Pw0TB45xwNUgLNunXaQlm+NR9hCcpWWSN90mvkpRnjZUBuOmoaAbAdGjUFpoaOEg4OgcqCu56VQYHQA0gLP9x5SK54aD5cdjaug0Z/sUy4+iyZMXq4VQy26PxgJGqlAvdYIxF1S6CsD859VRZ+vEekfa4IBCmNRbJ23mQ5D8pOPrmFFHp9H2kjX3zxasjOjBItr6rAq0+hgL/90zuyfP' &&
  'NhQ3cyuJcIc2aVoiLQDUUpLdbnwpAAqmgYCVQOGhh5wE+gvExkz3bZ8NZsOaerkUaNGoDx7Aof6iN1LsSYFq/6TJ545V3ZegSdZlG2QCzNAyhC3zR0ooFEAbGWz3GgE5GwCRgtYfeDTkuQj2bfD+eMHzo/NBzYXxcrLyx9T15c+RkMHXRh3IN6JMhK1E1v1mFEAcZSiGd//uKvZZn/K0' &&
  'nKSoTaxklCg3m2NrZRqooq5JykLvLC6IelC4zqy7Ld8ncL/zmsbk2ccTb8zfrXZv9A5t/+zyr3OR+8LrM2vi3tEOm97RL8XVJaLr8fNkZGnnaZSoAg9cZht+xEWjbAWDbgGomVXbh8w70Py4ZyKFo8mE7PW1chcrhAVs39uVzSszM6BfPoFQmriCG49ng/AGMulYF3PSrSsQcuUYkg1L' &&
  'py6RNXqkZ4NhQ6lI4wTWt+scQsw1tgHlRv0wx61ysm/U4krbPtA4OogzKV7JZ9bzwuXZUUtBfNwytq0TZTJ+O1X/zkC7nrsbdBc3dzmzw4vFOGnpYjb/7mQTvB50g4Tn7iSXVeZLO5RrDNMpSnEEmnPr/S8sDGUNJX7kf1Mplw1SXSu1dX6X1yd4kN1EkDDDkhI0lqS6rlyKEy2V1wSA' &&
  '4cOiR52/ZI3sEDzMcgqiwYLQZWUqCOwfCxUVcbzTyT/DPGz6g3+pdzZcWWQpEOnUCDTaasXPt17yK3DD5dep7SU1NNwtFBMD09cLBQdu3zS96OQtlyAA7DB4cRB5cJpzqsR6q8Pn2UKrLLjCjRn/7it7JiG4w0PQe/iDoZlNOoBpiqsmxZcb2gazgAXv1o9jip6GycDA3DayTxh2qkb1' &&
  'onefqe2ZKF4b/0yXKZvPYlqeoUJ5lwBgSNj3D1J10yQm6/aBjkVCqTXpwnH9bt1DYj2yZY/53x86Ubsrf0oP2Y7Mm0Gh0tzwF1zlevKQpV54GXl8j8xZ+JZPbV2xJAmkSBNUDtqjfL5gXT5HR4a18DBuQmuWFgKywmGmxEWjjwZ3OhfF1xDc9YBkh9hfRLrZc/Tb3dpHY2WhgjIbMi23' &&
  'XCxb+oWw/Fou/e6C+Si/9pljG+uPZaT2LYDmg49I0aYCekR2ZuADShl6BD4TOJxmGMfwKRr5e5xQhBRSXK6+TxWy+UMdddbNNnM+dhMaMibeSnQRV4OvW5xTJnxSaR7G7mYp2NOOX58vwDP5GrLzoHysg0GY8q38JBJeW4+RSdTTmGsXvXQdm2e48s/WSdrPjwQ1n70jNyXkeMXR2YoY' &&
  'sWwHnft+XVcsYdcICJGZjEdGCTqIdG4FCYSv7r2BFyIZwqQQlwLhcJ55jo+TlDdPPGNRs2aSpdU1UsHz31sNYj2EYV6o9g5nMArTbCYGLIR6SvMMAVMEBQ0zZAfhUY3+vbP5T7li/QSBUJGgoj1T+eNVR+c/lIdZSjX50pf67bZioAzgAJZ4SvjXtELkjrJp+W7dOoGa1twkXN3yFq5l' &&
  'Bf7QId0ZIBOr6EgUI1Aue3OKRgIovX75D5r8P4MuAlSwtwC1dpfG7VLZApc/6wTL26Gl+bYCklYqikdbLlSIXcNvUp2Qwj5SSbdHBV0M3HooORL0GVUY3vbqTKVC69hbGwENQmoDbO9svmoxofq6IOUtRSfN732z+ZNJYKU4+0rAa+lzxgyciWiQv/Ip8j4pbb8Tjmaq/BSA5+wvheQy' &&
  'Sd8+fPYXxQ8KDg69TxbH5hstwI48vBk5oiwwkwlfUWg1pwpVbneDkw7lPgSy8/uZOMwpz8tV+Ok81v/1HngGw3CCUjXuU0ccYCsB7RMoE8ArU0Pv8+mXDlD2UpDIEZDVPlRLQdzfgIzuN5h/dzQEe/5BjQkCETbhiixrTsiV9ialKp42jHcWDOSeUP1IB/dRibk4kXdIjOKbYSdHrXIA' &&
  'X8h0791RgiwYiVkZ4qf/xyhazauVGvTb3xXkmFE0istxmbRSNoYkpa1zFRJr38pHzXUCX90k7WNDNa2wQNkynwAqSrXOTRleRWwOlIs6BCc0XulhnzMDdA2lVZKWOvOw3pzXZwH8yjB2MkTOkoCz/YK69hLsFUR+daLSDoFRrpv61wqdgN5XLT4P5ou0S21qerETJNYkqpxtAMTMpn/u' &&
  'WCixoftwWSkkx7DaWmMPoRuJdQbyMCFDRahCG4OliDOSAXXfK2mwg2qH2lplYKOiAWpM7kz/g5b2pEIi0OwbHC+KrQVjHGc9cTS1Afxsdq5B+fxzyPUZ+LUCmMljRa0GlOlbCVUNEUTVN93sOndkdDq9e0nWlQv+RkXZ1jtAuqOb9DPovhAFZs3Wfmm/FsB32V+WXoqRnyCObGTAzj1P' &&
  'ir8UwCxGn4H1mY0rIY4Bp5DtDguMTRARkG6eV18pGfrO1LRFRmVuKJOscKd5CDiyCP3TxF53sOLkUkaIQ0lHvfmCPfI2XtgfngkzdO0MgYiaL4BjXCL6oPyNyPlqhM7zztYjVwGigL4b6zsO2Fny6XTZhjMmgwswpmV83AZ5uxBbAPkLlcsKcIJj7xAoQEZcHEftQFPWX6rT+RKTdcAo' &&
  'XBvMESoukT5hBjnlopm2AATEtUqdXzMxemahihqbCDQiPcNaC4VK7t310+nHUP+vPrStyQB+Zpukpj0cm8hWnTFKY1VMAte5HWjn8WGtBRBnROko9n3yl9OyNqVUORNGKjmgcumhiFNnwwxmiuU7nWIKpNfSUPEkY7h76XP0wbLQ+PHQ6FPWjrQfs5f4tL0TnQb5BaVtFQoLSh9JZKzv' &&
  'GLvPv5V6gLRXVOKpaL3iIThvaX/ohYNBpdsNBFC5fCGtpChR80fn6nksWjOxoAnwM9vMdi6zJS0plQBlSmN1etwVQCDpVzeQXqFRfKpH+6xfSvxo+eYXzKYxgt9/wii4nIqIb/nHw5ZaHjIPXGHOPVgPnbzD8tmEUpQtfMNzu+KKBsjHyoB/jE/9o/9x8xXsb72bfeq5HKO1fzGgrniT' &&
  'PeeFJ1+7KeZ8uU066S6iPGbRJMRbNqDeVcqGHU/BRRk2NzUZN12BbhTV1d1OScNPJUk9NVb3RXv2nSOhR7w0QFfvHJq4hoKzbu1ZWuPu1r5V9+frPOS+658ToZ1AupCz13LEVm0xxMqn8193nBVFwFbro0kca2Gg6uoqlaElQeszR86andZdVjY8G5WtlyuEwj4UF0wYjsBkHlZrtqmL' &&
  'i0Lr9CBtyPOV9CQPok18uLv5kkZ2ZnSgaitibkKnzS5AWvKRvw4RZ6SCk6g7Gzz7Ez/2BSxRK/LJg4QnojQtFQnp8wAga5H3VZH+1SIKg3Z8nHurLJeYlRJhbyl79FPln7rfJWEcBzFGDREblsQG+rsKxPelGHMlG58FlvATiUoJDNSRM1AP2JZ+xUQOWAeSyrE4zAKzbswPMwfFUeQ0' &&
  'u/nidL75M64hueU/pM5GUNo2KRNLDwDls2iw2uMDUNIkwRDR2BOEZWp/SWF0FgBBHK2xRwaOQPUz3wlXrBhrlb0xfp4uSzr9fI5o2ADjScNws2yYcwKnJoxJXXa9R0dfnpfY5G6KImV1EZNV0q6jVEggbPqPnK6nd06qZDo3qg2A8SwH8VOnJ6MoUdNPN0KuE6RJ0xc5Eqcd5XVCDzp9' &&
  'wtHVGF6QVTi98/eIfIEUYBjJwRgIQgnVmxu1qeRcrGvS1lDLwp2wulKs2BXtcR1yA/xByEq3h9UgOyFQP+0diZsg3pME3dpB1VltZGjboDx8H4QOuArBp5b/4UzKeQymlbR4eyUA0avICyGk+eINPmvSFby8GmCr8M65stN2F+xWjCuREXSUZdfjoiITIBprYuve3YQ1deqegmxyCtvF' &&
  'evXperhTrvYkR2iG+ULl25GEWEBNRWOANQgJduzqxRCeC9776H0+DCGaHKA9rq6uWU7BRprynjsffvwK0O9sXWaWaMBsodjeoWakDK+VaDzoSyUZaqo/Br0XSZJaZSMvB51+ChcmGHU/QZB2+k4nyQRnUYRsX9QUZNLrq4qMnirX84qU6jJpPVSxE1aeAuarp6zhDZ9u/ff0s+goFXkJ' &&
  'fWEVqOG39l4TMpkmW4ph38Eid+1NJFB85TkBY+fteP5QIYhNnUJYNrdYXy8VFXwzgPqwDNKh7aS8uVqS+vkA2IAkqi9fYmsjQHQ6SLwlSKFCnURYSFs8ZLH+TXTEdv/8Vs2QUjZMrJ+QQXNLg3OeiBh6DEaZj3xOqqW1fk8ExVgizknhf78DC1CdT4EQPRbjnaf2f917JwDaI/9yirDs' &&
  'ljk27STWJdFW30aboz4e5r1UHoaqiuiKINzm0SM2TK7Gd0D9GkZmyf/4guMEk8xMHoR1BwiPQ8zdI2dYwG0kcZ1SnvyXE6Qjouw/1GKS7BXFgzDwtLV25msvt6nGBPVcp/fuOYyJnDnMtb7fPVR98EPxroxHVrCvIMVO2Q0oOfin/bEvHveElKC97DNcxrgVRJl8nDxwSNyhmJ+6SBMV' &&
  '2kURVLqS6ycNuB9QlvVCM4v2PU/PP2D9VabrnkKrkkvltYJPS2ndAjQw08HwZe5YIKdNuM3um4Y71TelRkCsb9IUYwPY0BMNUcqcvrIIqaBAWlkfLh0bg+7IwOZuOVk3nuYbES5hdM3RgF6tUMmBoxpYmGaGLHtYA5ETMAaeTiuRNkQK5PNuwvUSM8BJLpJLjaOXDir5FhZsqwPrmS99' &&
  'R9kmI0TaMjUa/RiX1YRjQL1KMtod19UNjrZiw0ixSI8kunjtLFEZ1rcHkBnOR8lOkoMwPuuYUhpYMs+nSPvPHJFxqxXRqoqCWPPKDgWCmIkICOCZhvFkKGnLuOm/uqnH/3o3LGmOnGGYIHFaQ/aICWWSAgv6jSutfj7B+yY8Rl+v4CpjD3zHxa2v/kAT3xFB3qnloF5T8iXrX/Mzm8+X' &&
  'WRPR9IZtEXWur3rZTqL/8g/s1/kQw47/OSu8uvfjoqaIQONBQaGBdZaFTPf7CC0pEbLxomF3btG6zrjNDVpxFym2NL2W7pgKg5beQ46VAdH9a2g9fA88nOZnYGwjSfxsfw/u3eIpn64idwiTCsQ99rqpmtgiKjzGICVyXpjdIhMkYGRR04ztSwwcwLt1bGyZwXluqTQc+n/0aC7UYOwk' &&
  'zmOXnnPmAvKDqXtAed2kM2HKzWAwGzX1kmoyY/hRSwnYw850J5efooSUf6UcKuGoyxG6UDkuhxj6ZYuI8UiWx/aPZrupAj1eUy4ZofyuBzz9TldI6/BmO3pq2pzwBkBly6l1KqL9pQwWGkWd11tZNbKU7FlKoEKL9mC1b5WT8hQffP6Kpa3mppGZx7LVu/Wa4c/7Bccf/vZOHGItkq3a' &&
  'Vf+2529JYHNfDcLmUmILeD/r1y5GgsagV4tG7ay4ul240TZczTH8iinUi1GzvDl3GLPWTyBuG/jo56GN+3Ughjy/R9L+1iS/RqQ2ydpMQVSXzCEWlXtk6jIlNRbk1c3uP04EonjcUbEd3K5X7/QT2aeN/1N4etirKeq08jZHr5+KI/qqPqj6g56oJhYW07OAN///tv5Z01y00UtJGQVu' &&
  'YALTEnJJguMGIUIGINmLxAJBtp1P5duuhwXk4mKpJR0GzbCMk361oJGgWWTr5K54niy9L70ojIkZItc95ZLa8gCvBpTpBpiK4oYrzRgAbgBhEw6aMS7tP8uSvmJ2/OvFdGntsd6W2pzH5tjWw9UKTHyp5+8HpVbqaPuvNHj4MoYHsxq6AaBKHspC3GGKhhhVlXYwpOuhb85T1Z/g3mFj' &&
  'Bappfj7x6ubsesCgbU8SSqCPC7MVmX238xerj0y6VhYTxUbAqYaWZ6pqbyKiIfE1KRfll20YppO+txewQp6IZN3yklpIUmHOQFP5mlRBTWccWB433r3z+SDYegTp16gga0jzE3VJcoPyn807pyLxM0MAUmT5g6x6bLhr2NUlVRoXVAUKivFhDs21OXqebMJRsxGToZmRCcuDoYvaX0BW' &&
  'VCrkb3yM0A7ccUSOl3H0hunck4YhtUMvrpa0jWz6TYMml3eIdUF62XHBjhjBsmBxdZ3PyO393+H1dFR735uKaLF8Convnxg8H5nTMkgvX5bF67A7rfR528b/AI3Zpge6znjZpEeRefnrjh8TfeiZyG+ahQZAhvcD7FRQddMYTCDh1wqoy84nyt6GpFgo1yHvljRAhdkOAmfSzmTNzTIy' &&
  'CEu+a8rkIJxHJOxs1jY8AKawitRWfwbMGUe2Tt87+UpdNuk81vPKr7VqSMxUzQW1YaE6EMzIho4GAMUjdud0x89j9M6pm/V9NLHl3ybkQbXoTAXxQG9/CYMUQi77sSTelrJEf7u/Ise/Ilzi7CcBU5o5M89+f3Na2m+ut2i87lmgdP/rhiIokZV3wyZqek30VSd8IGoJz51oIkc7x2TL' &&
  'qKDWRmyZKVn2gUVQfBeRb0g0+HWgiHkaPjNz9tm3Ao/x2oPmLmeM2BkZCFRlhbW4wxVEgaHPdDQ24Pi2zOEB3cymUhvl99xnlqVF7QsFjfffJs6IqdG9WvcGuCaa4zaGeEDoya0xfN01VUwkxjDMBeLh/XQnQJOl9Z+NE2PAEPCe8/b+o4Pb3AlSWjdFagEeAKF+tNH3ejHqZGs+AEJ6' &&
  'eozwWJtO563lBPyRCqWJbIWn43ZtAaMEpzFZaHtf8eRs/oy01rDp2tB1d0j4aggqBvXe73gfkxut2hC09FR+Txnw3Xhac0KJWbT4aBe3s2t2f/3JpgxiD53Jqw4EpndjeZumiVzsk46puvuVzbDwGUox4XmXgo2vhb1uQxQK5kApoJeAuucTFCzZWFQrWRPIgQX2OTMoK/qJATLu+jR8' &&
  '70NA/7tx576qurdJGLm+ZslsrCaN/8CjaoC5BiQzUpiQp1CH8dJDXCsCxobNHA63W6T4uuUa6xK5feNNGB12gkNCousmRAh2bcPL7JKRlnWPzk1sTNy2bJESmVXnZrgkboIh/r8DsLDXZ13T7d0Fcb4OEVC5UJmU3Pz0gl6TAY/z55Zdw10oW02oWDYMSKABXehdWu8Y1mQeLQHv2Nns' &&
  'wHPP2KHSV6SoZvQOjWRPOiahk+pHlIM0m4i0psj/Tpim6rmvVWojL7dM+OG+j6NgAwtG+6XXiyTNKFJxuddFxWMHiOffMX35a4DRnDsLO7gyh6X9Cnp4VQA/NBtyjFlV3NFsrhzZkp8D7TQaSrY+YtkeXrd0gpshGm003Vi32zUDh8zvROB8kSchMhxXHQcUCeHIk6gXYYTS3ntQCVC+' &&
  '6Y52bvnv6cWbFUOZk+zCH4ZsAFLjgjrguE7f8F4eV3BFq41RRNjceloC1B3+RC4cplz9r0qEbI9JHzwUfeWijfNZTD6abrOVBvZPOCJ2VohH9avhQmKHDUZ+t8MPKomuvLbei/BQOv8rBIE1AK7eH5izVS8ZWgKT/tL9efe6okgmgqAQ8Oq7eNmtrVQTQN1mPW6uHdGbdfpK/86HyIB5' &&
  'ZZUnN1T3E7DJ0kRqZxQWM9CjjBN0ImcWa/UhGVtuaka+ryLChpIQPfW/+1BM9mwnM+9+j95v0ubZ79sVAN3VvoZGxIkPrqCeaIfCXrmal36tsWOrdimkkjw+fW0kRN8dkkswWJK0H7qMMTIS5NRCp6y68XyXNLPw6mowbuhI4DaNM3sc1eH1dZoyt/U3ALSQ8SjL0ZTsAsYtiBwm7bSV' &&
  '5hjO65rtbDBD49VlXlxqpRNwRzPpdJsPmTD7xLvnLbSA1a0YLxthFJWd2kOgZBogXQKKsaMiSeh/AD5D/mtmCN2++L3KB3qSijFvf7xr76a/Hj0nmeRRYXzQh+ujcoZm5/R/4NRsXDKbddNiLsGJwXNFga+B/fW6anZJwOKmco7OWbTG7Nd7Luv5XvyZkUjYailXhQl6lPE/CuZTA8Jk' &&
  '8ujr9hiIkC9K7B/TGIJzVDFyS40hY0wAQXDfkbpb5KUnTnKjpYy/ztFQAK746SNQFpRbOGZZEwffMsKO9zy+Gh+fZspv+gvIIozrckVIFMVSZaqmRU+KbqRBpioaQcS70++/zEkbDsIlTnk7aR9E6y8N21GuG44c23RwYkwwDKbMbABSyeToGzmvjyGrnk7kdlAWTD7GR/XUDTF+OATK' &&
  'lAs/sbArrHx/R2ETKMCXxX8at8GBK9NuhXYk0cJVfpUAkeJOCB7wX3/0SkYLtxBEzFuUAVnyxbA6lyxYOPyT88Ok8PjjMt3Y+oSIOkNOkumLzSSXDhjjS8jXHNxVz31ofm6vOafus80+s4POAilW5bkUgTZZRT3s36SMSkSmz7TlJGOqOAxleOpJNGmt75AlxJomWphvGJc+wpGbc1ET' &&
  'lfo5GsObxLVy4Jt98XGTWdMbqoyUWWbFShgXuPtbl5Iw2Wn5xrcmuCL3UTMau+O9B4xfjZ5n20cr+sfeoB8/6Y3j5WNMq6/GLz4q6eN4QgCHKhqEgmDe+rL/B+A8UayNeFOvfEPRIEQZTly9JJ18lVmN8dHw0AIhjnUL+DEk98fTUiPOa27ugX0r99L/5C39YeM/NpWfS1iQQjz8yQp6' &&
  'fco8JidDUrva0D2c65EueLTCGn83Wj97fqQXUDMIBbNEV79FUoGiH3M+e/ukxmL14H24XStqfi2pFTOZGR6MuwSQkyqGs7aa/vaIaw83ClnhKSRiTLqGNO2FCZ0TCfpxPkviO8LvvkCSY6SToSGhxXwHmKiI4xjy9G88A9ozajsjsqVswcQaRPlk+yMtPCaChuiJW8nXiONp6chsJsCX' &&
  'LkyShdEcaYuQJdUa2r1bNGD1dz/ClfR/qe7eNBnkdFBsA3QT5/6j7lPRFV/gwMMfvFv/0/JKbkS0lDdkGj884FiwI9JL3XYEnKvhC/rPw0csMQQQ+dhvcFXi9oWDQ2GihTUG418HD1TfN+paulbm7nwLqMkDx188ytM/Wk2Av2XUPOK50Bep9hmjpr4G36rmFs9pkXPpy3AwysrpPH7x' &&
  'wi15/XR/e2JObouXXzwKQ2tZ2celInWbrqI7QNRteAAVUQCPznmg9WyPln9Ze09HR57rmlEADuVyFaVEM5kRINufBMOat7Z+O8jwcxjBQiq7/+VlZt+BJ9IPSzj0ooVEWRjL99qKxY+a5Mf/ZtGAkVt1yWzJkiGWAiYxnNry00ULHg58BszJ/AvzMG9JWVKz8QfxH5i36rDqOYxZfNmz' &&
  'bIDX93qWSir8H9+8j1l54tPdoHZP/O7ah/CPUZZ4AkKHJqJsaSIPsqY2UHmtlRnogSr8UfgMLzlatEUMw0lgci+KoP+uqT0ih3XNxX7vj7c2XaPw6XkzKSodwNEkNlbIzVORtjdyfIaviQC2To+WcgKy6QL3bvBX9smsR5YCIMKzFF+zI0mL5Jy75K3kf/7VDioGRVoJtnb2vg0FAGdU' &&
  '2TMT8+S+6+frAMHXy2pCPjoat97d/flx17wYsq0MqFINCdlVgnP7v2Mr1PvkflvTrQTEnO6S51viTxVxfDtmp0bZ2lJuNcye51NdJPzLFjPMYFXWCJgaNvFxOQbj27yzt5H0lpXS38G+TlCuygpqZWkutjJf+rnXLVoCskJ7G95HTKkrzP15n6qOMK63Izfk/BIUmtrJaBvc+S07v3lg' &&
  'O7tsmGI/tMPbTbHsNMKG/QwrY//+JLGf6jIRKz9fCBxtI67mSJ9EbkS1dPwQk5VfDYwflKOdSRrwZ5UWPtOgcJ0UmYb3GTmnDXIQPzEq7R5uOESaO3+f1SqLt1BuyD4Hj5Nz84fvafCSU+JTUF6TcVz6w+RvXCzYD1TdQ0L+NyTsZXqQ76TQTxorK8TM49tXvw1A7fnuY8Czot34Mn/G' &&
  'NMO3ft1DfNt+wvkCNFZSbSedCvcxc9v8kjZFkdU6RXbid9c55vzHfJytSUiCro2Gi2KyhfIkFlZFZ3OUouohiHxai897s9svbb7VJQWiFbthbKjv0HwvvHc32yszQiZiHy9emWie+p0qVzjv6hqE4d0yUHvOR7HuzfzdW1P0wd+CcpyBcev+Mfn3I4LxfRlwt/XGxTU4wE6gb/rCQDRY' &&
  'EEKkPRz5cCZ4V5Xw0ykPDYFoLTzUJkBYWVfns1hIpYH+QSkMLCYhlwUn/pit/kzU7oypEEcig6+AZFb64hoP39CDR7y7wr3U3B+nDWFXSCyhSd7zX7NnvbQFE7G6JvMuArK3Garej2hoKWZ5SC/xqvhyf1x/E5AUNBUMPtJ8FNbM4frQJC2JxTGVoBPqI84I+28sGwM/w5XuPv0D3yhz' &&
  'BncdkXB4yiysdxh3b3DHecOzEtkWv8TXpZjLLR8C3Z2pdtj4B87RcUtMCU3/tnMvRBG3Ps/J8rv+7ABGUWUnNnVOYxp+imB9OvocO17UZLoJb24yg1fGI/upClDgGIaoBsG1kEj1rYk05hCOpty38KgtQEt6vc3I4Dsgt5NDaCzzNn02BAeOoEEc1OWB+0sB2lwfuMrU9XxolZTJ39kx' &&
  'TOc5vk6/jhVuz0DCTAf90f9uKKFFmvf5iIiqcnMsxpHMIt0DTHwNbCKbD5I0QoqnAomKMw/Qo6Al3KB2NIlBMIX3PR+oaWVoGKq0fZwumnSvGKU0iOz/2Zd/Meo+maiux44KBJMPfZuMxvFVJNU40kFNVDCCm7vh+I9tmidxSGLly1r0fxaB3NgUatMvIqj+fhkOKyNvnITwc7Wq7M4n' &&
  'nOtZR28hO/3V+/ZnNsx/tnO7jXyD8YxfscW41tK2oEC8rSjlNlZxft1MlgFEqe6bfZQOIxNkdTCq/hueCfkuANC+osOU7rUD7ZusE+LF36NjwQzGwsLyPB29xlaNvfBf2/CsvsZoXxVwP7+e/uw6E1fR0jPeSXU6oWecb2if+pMXvh+nag1VH5j40W41wioAHCGBQRtc5x4v+HAZ7ACf' &&
  'wvRci8T+AETuB/HCcM8ARO4G8Gkf8CO1XW6Qr6nvUAAAAASUVORK5CYII='.

  HTML_PAGINA =
'<!DOCTYPE HTML>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'<HTML>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'<HEAD>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'<META NAME="escolha_processo" content="width=device-width, initial-scale=1">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'<script type="text/javascript" src="https://platform.linkedin.com/badges/js/profile.js" async defer></script>' &&

'<style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  body {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    font-family: Verdana;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    color: black;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    background-color: MintCream;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  .card {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    transition: transform 0.5s;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    width: 200px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  .card:hover {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    transform: scale(1.5); ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  a {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'     text-decoration:none;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  .container {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    color: black;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  .split {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    height: 80%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    width: 50%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    position: fixed;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    z-index: 4;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    top: 0;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    overflow-x: hidden;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    overflow-y: hidden;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    padding-top: 20px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  .centered-bottom {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    position: absolute;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    bottom: 80px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    left: 50%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    transform: translate(-50%, -50%);' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  .left {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    left: 0;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  .right {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    right: 0;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  .centered {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    position: absolute;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    top: 50%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    left: 50%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    transform: translate(-50%, -50%);' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    text-align: center;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  .centered img {' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    width: 150px;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    border-radius: 45%;' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  }' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'</style>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'</head>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'<body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
' ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&

'  <div class="centered-bottom container">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'     <img src="' && IMAGEM_AMAGGI && '" style="width:100%">' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'  </div>     ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'    ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'</body>' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
'</html>'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

  CALL SCREEN 0100 STARTING AT 50 5.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  IF CK_ALTERADO_FRETE EQ ABAP_TRUE.
    CK_ALTERADO_FRETE = ABAP_FALSE.

    IF ZDE_ZSDT0001OD_ALV-NR_ORDEM IS NOT INITIAL.
      TRY .
          DATA(ORDEM) =
            ZCL_ORDEM_CARREGAMENTO=>BUSCA_ORDEM_CARREGAMENTO_NR(
              EXPORTING
                I_NR_SAFRA             = ZDE_ZSDT0001OD_ALV-NR_SAFRA    " Safra
                I_ID_BUKRS             = ZDE_ZSDT0001OD_ALV-ID_BUKRS    " Empresa Recebimento
                I_ID_BRANCH            = ZDE_ZSDT0001OD_ALV-ID_BRANCH    " Filial de Recebimento
                I_NR_ORDEM             = ZDE_ZSDT0001OD_ALV-NR_ORDEM    " Ordem de Carregamento
             ).

          IF ZDE_ZSDT0001OD_ALV-ID_AGENT_FRETE IS NOT INITIAL.
            ORDEM-ID_AGENT_FRETE = ZDE_ZSDT0001OD_ALV-ID_AGENT_FRETE.
            ORDEM-DS_AGENT_FRETE = ZDE_ZSDT0001OD_ALV-DS_AGENT_FRETE.
          ENDIF.
          ZDE_ZSDT0001OD_ALV = ORDEM.

        CATCH ZCX_ORDEM_CARREGAMENTO INTO DATA(EX_ORDEM).    "
          EX_ORDEM->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
          CLEAR: ZDE_ZSDT0001OD_ALV.
          ZDE_ZSDT0001OD_ALV-NR_SAFRA  = LC_NR_SAFRA.
          ZDE_ZSDT0001OD_ALV-ID_BUKRS  = LC_ID_BUKRS.
          ZDE_ZSDT0001OD_ALV-ID_BRANCH = LC_ID_BRANCH.
          ZDE_ZSDT0001OD_ALV-ID_AGENT_FRETE =  LC_ID_AGENT_FRETE.
          EXIT.
      ENDTRY.

    ENDIF.

    "Proprietário do Veículo
    IF ZDE_ZSDT0001OD_ALV-ID_PROPRIETARIO IS NOT INITIAL.
      CLEAR: ZDE_ZSDT0001OD_ALV-DS_PROPRIETARIO.
      SELECT SINGLE NAME1
        INTO ZDE_ZSDT0001OD_ALV-DS_PROPRIETARIO
        FROM LFA1
       WHERE LIFNR EQ ZDE_ZSDT0001OD_ALV-ID_PROPRIETARIO.
    ENDIF.

    "Agente de Frete
    IF ZDE_ZSDT0001OD_ALV-ID_AGENT_FRETE IS NOT INITIAL.
      CLEAR: ZDE_ZSDT0001OD_ALV-DS_AGENT_FRETE.
      SELECT SINGLE NAME1
        INTO ZDE_ZSDT0001OD_ALV-DS_AGENT_FRETE
        FROM LFA1
       WHERE LIFNR EQ ZDE_ZSDT0001OD_ALV-ID_AGENT_FRETE.
    ENDIF.

    "Motorista
    IF ZDE_ZSDT0001OD_ALV-ID_MOTORISTA IS NOT INITIAL.
      CLEAR: ZDE_ZSDT0001OD_ALV-DS_MOTORISTA.
      SELECT SINGLE NAME1
        INTO ZDE_ZSDT0001OD_ALV-DS_MOTORISTA
        FROM LFA1
       WHERE LIFNR EQ ZDE_ZSDT0001OD_ALV-ID_MOTORISTA.
    ENDIF.

    EXIT.

  ENDIF.

  CASE OK_CODE.
    WHEN 'CONF'.
      CLEAR: OK_CODE.
      LC_SELECIONOU = ABAP_TRUE.
      PERFORM LIMPAR_0100.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_AGENTE_FRETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUI_AGENTE_FRETE INPUT.
  CK_ALTERADO_FRETE = ABAP_TRUE.
ENDMODULE.
