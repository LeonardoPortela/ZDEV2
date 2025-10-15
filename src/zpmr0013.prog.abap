*&---------------------------------------------------------------------*
*& Report  ZPMR0013
*& Logs de transações PM
*&---------------------------------------------------------------------*
*& Analista       : Cleudo Ferreira
*& Desenvolvedor  : Marcos Faneli
*& Data           : 24.02.2015
*&---------------------------------------------------------------------*

REPORT  ZPMR0013.

** Types
TYPES: TY_ZPMR0005 TYPE ZPMR0005.

** Tabelas internas
DATA: GT_ZPMR0005 TYPE TABLE OF TY_ZPMR0005.

** Work Área
DATA: GW_ZPMR0005 TYPE ZPMR0005.

** ALV
DATA: OBJ_CONT      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_GRID      TYPE REF TO CL_GUI_ALV_GRID,

      GT_FIELDCAT   TYPE LVC_T_FCAT,
      GW_FIELDCAT   TYPE LVC_S_FCAT,

      GW_LAYOUT     TYPE LVC_S_LAYO,
      GW_STABLE     TYPE LVC_S_STBL,

      GT_FUNCTION   TYPE UI_FUNCTIONS,
      GW_FUNCTION   LIKE GT_FUNCTION WITH HEADER LINE.


DEFINE MC_PREENCHE_FIELDCAT.
  CLEAR GW_FIELDCAT.
  GW_FIELDCAT-REF_TABLE     = 'ZPMR0005'.
  GW_FIELDCAT-REF_FIELD     = &1.
  GW_FIELDCAT-FIELDNAME     = &1.
  GW_FIELDCAT-COLTEXT       = &2.
  GW_FIELDCAT-DATATYPE      = &3.
  GW_FIELDCAT-JUST          = &4.
  GW_FIELDCAT-KEY           = &5.
  APPEND GW_FIELDCAT TO GT_FIELDCAT.

END-OF-DEFINITION.


SELECTION-SCREEN: BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_TCODE FOR GW_ZPMR0005-PROGRAMA OBLIGATORY NO INTERVALS,
                P_USER  FOR GW_ZPMR0005-USUARIO,
                P_TIPO  FOR GW_ZPMR0005-TIPO_MSG,
                P_DATA  FOR GW_ZPMR0005-DATA.
SELECTION-SCREEN: END OF BLOCK A1.

START-OF-SELECTION.
  PERFORM F_SELECIONAR_DADOS.

  IF GT_ZPMR0005 IS NOT INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE 'Nenhuma informação encontrada para filtro informado.' TYPE 'I'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONAR_DADOS .
  SELECT *
    INTO TABLE GT_ZPMR0005
    FROM ZPMR0005
   WHERE PROGRAMA IN P_TCODE
     AND USUARIO  IN P_USER
     AND TIPO_MSG IN P_TIPO
     AND DATA     IN P_DATA.

ENDFORM.                    " F_SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIMIR_DADOS .

  IF OBJ_CONT IS INITIAL.
    CLEAR: GW_LAYOUT, GW_STABLE.

    GW_LAYOUT-ZEBRA      = ABAP_TRUE.
    GW_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
    GW_LAYOUT-SEL_MODE   = 'A'.
    GW_STABLE-ROW        = ABAP_TRUE.

    CREATE OBJECT OBJ_CONT
      EXPORTING
        CONTAINER_NAME = 'OBJ_CONT'.

    CREATE OBJECT OBJ_GRID
      EXPORTING
        I_PARENT = OBJ_CONT.

    PERFORM: F_MONTAR_LAYOUT.

    CALL METHOD OBJ_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GW_LAYOUT
        I_DEFAULT       = SPACE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = GT_FIELDCAT
        IT_OUTTAB       = GT_ZPMR0005.

  ELSE.
    CALL METHOD OBJ_GRID->REFRESH_TABLE_DISPLAY.

  ENDIF.

ENDFORM.                    " F_IMPRIMIR_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_MONTAR_LAYOUT.
  MC_PREENCHE_FIELDCAT:
            'USUARIO'   ''      'CHAR'      'L'   ' ',
            'PROGRAMA'  ''      'CHAR'      'L'   ' ',
            'DATA'      ''      'DATS'      'L'   ' ',
            'HORA'      'Hora'  'TIMS'      'L'   ' ',
            'TIPO_MSG'  ''      'CHAR'      'L'   ' ',
            'MENSAGEM'  ''      'CHAR'      'L'   ' '.

ENDFORM.                    "F_MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

  PERFORM F_IMPRIMIR_DADOS.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT' OR 'LEAVE' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
