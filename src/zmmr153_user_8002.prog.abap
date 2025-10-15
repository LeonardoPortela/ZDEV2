*----------------------------------------------------------------------*
***INCLUDE ZMMR153_USER_8002.
*----------------------------------------------------------------------*

DATA: CONTAINER_8002    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA: NM_FIELD_SET_8002 TYPE C LENGTH 50,
      POS_8002          TYPE I.

*&---------------------------------------------------------------------*
*&      Module  STATUS_8002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_8002 OUTPUT.

  SET PF-STATUS 'PF8001'.
  SET TITLEBAR 'TL8002' WITH WA_ORDENS_VENDA_ALV-NR_ORDEM_VENDA.

  LOOP AT SCREEN.
    SPLIT SCREEN-NAME AT '-' INTO: DATA(STR1A_8002) DATA(STR2A_8002).
    IF STR1A_8002 EQ 'ZDE_ZSDT0001CG_ALV' OR STR1A_8002 EQ 'ZDE_ZSDT0001OD_ALV'.
      "Campos não Alterável
      OBJETO->VALIDA_ATRIBUTO_ALTERAVEL( EXPORTING I_CAMPO = CONV #( STR2A_8002 ) IMPORTING E_PERMITIDO = DATA(E_PERMITIDO_8002) ).
      IF E_PERMITIDO_8002 EQ ABAP_FALSE.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF ZDE_ZSDT0001OD_ALV-ID_ORDEM IS INITIAL.
    CLEAR: ZLEST0185.
  ELSE.
    IF CONTAINER_8002 IS INITIAL.
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

        IF CONTAINER_8002 IS INITIAL.
          CREATE OBJECT CONTAINER_8002
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
           CONTAINER   = CONTAINER_8002 ).

        SET CURSOR FIELD NM_FIELD_SET_8002 OFFSET POS_8002.

      ENDIF.
    ELSE.
      CLEAR: ZLEST0185.
    ENDIF.
  ENDIF.

  IF ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE IS INITIAL.
    ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE = OBJETO->CARGA-ID_AGENT_FRETE.

    SELECT SINGLE NAME1 INTO @ZDE_ZSDT0001CG_ALV-DS_AGENT_FRETE
      FROM LFA1
     WHERE LIFNR EQ @ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_8002_EXIT INPUT.
  CLEAR: OK_CODE, CK_ADD_OC.

  PERFORM LIMPAR_8002.

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_8002 INPUT.

  CHECK CK_ALTERADO_FRETE EQ ABAP_FALSE.

  CASE OK_CODE.
    WHEN 'CONF'.
      CLEAR: OK_CODE.
      CK_ADD_OC = ABAP_TRUE.
      PERFORM LIMPAR_8002.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_NR_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDAR_NR_ORDEM INPUT.

  TRY .
      OBJETO->SET_ORDEM_CARREGAMENTO(
      EXPORTING
        I_NR_SAFRA  = ZDE_ZSDT0001CG_ALV-NR_SAFRA
        I_ID_BUKRS  = ZDE_ZSDT0001CG_ALV-ID_BUKRS
        I_ID_BRANCH = ZDE_ZSDT0001CG_ALV-ID_BRANCH
        I_NR_ORDEM  = ZDE_ZSDT0001CG_ALV-NR_ORDEM
        I_VBELN     = WA_ORDENS_VENDA_ALV-NR_ORDEM_VENDA
      IMPORTING
        E_ORDEM_CARRGAMENTO = ZDE_ZSDT0001OD_ALV
      CHANGING
        I_CARGA_ALV = ZDE_ZSDT0001CG_ALV ).

    CATCH ZCX_ORDEM_CARREGAMENTO INTO EX_ORDEM_CARREGAMENTO.
      EX_ORDEM_CARREGAMENTO->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_CARGA INTO EX_CARGA.
      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_PARCEIROS INTO EX_PARCEIROS.
      EX_PARCEIROS->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_ORDEM_CARREG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_ORDEM_CARREG INPUT.

  CHECK ZDE_ZSDT0001CG_ALV-NR_ORDEM IS NOT INITIAL.
  "CHECK ZDE_ZSDT0001OV_ALV-NR_ORDEM_VENDA IS NOT INITIAL.

  TRY .
      OBJETO->VERIF_ORDEM_CARREGAMENTO( ).
    CATCH ZCX_CARGA INTO EX_CARGA.
      EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_CARGA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ATRIBUI_INFO_FRETE INPUT.
  CK_ALTERADO_CARGA = ABAP_TRUE.
  CK_ALTERADO_FRETE = ABAP_TRUE.
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
*&      Form  LIMPAR_8002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPAR_8002 .

  CL_ABAP_BROWSER=>CLOSE_BROWSER( ).

  IF CONTAINER_8002 IS NOT INITIAL.
    CONTAINER_8002->FREE(
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3 ).
  ENDIF.
  CLEAR: CONTAINER_8002.

ENDFORM.
