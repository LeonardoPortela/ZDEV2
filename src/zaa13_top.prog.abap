*&---------------------------------------------------------------------*
*&  Include           ZAA13_TOP
*&---------------------------------------------------------------------*
REPORT ZAA13.

*=======================================================================
* TABLES
*=======================================================================
TABLES: ANLA, T001K, T001W, ZAA004, ZAA007.

*=======================================================================
* TYPES
*=======================================================================
TYPES: BEGIN OF TY_SAIDA,
          TIPO             TYPE CHAR1,
          EDICAO           TYPE CHAR1,
          COR(4)           TYPE C,
          CHECK_ANEXO      TYPE C,
          CELLSTYLES       TYPE LVC_T_STYL.
          INCLUDE STRUCTURE ZAA007.
TYPES: END OF TY_SAIDA.

*=======================================================================
* STRUCTURES E INTERNAL TABLES
*=======================================================================
DATA: TG_SAIDA      TYPE TABLE OF TY_SAIDA,
      TG_ENVIA      TYPE TABLE OF TY_SAIDA,
      TG_SAIDA_AUX  TYPE TABLE OF TY_SAIDA.

FIELD-SYMBOLS: <WG_SAIDA> LIKE LINE OF TG_SAIDA.
*=======================================================================
* VARIABLES
*=======================================================================


*=======================================================================
* ALV STRUCTURE
*=======================================================================
DATA: IT_FCAT       TYPE TABLE OF LVC_S_FCAT,
      GS_VARIANT_C  TYPE DISVARIANT.

DATA: P_TEXT                  TYPE SDYDO_TEXT_ELEMENT,
      P_TEXT_TABLE            TYPE SDYDO_TEXT_TABLE,
      WA_ALV_0001             TYPE REF TO CL_GUI_ALV_GRID,
      WA_CONTAINER_0001       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_LAYOUT               TYPE LVC_S_LAYO,
      SDYDO_TEXT_ELEMENT(255).


DATA: WL_REPID    TYPE SY-REPID,
      TL_FUNCTION TYPE UI_FUNCTIONS,
      WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE.

CLASS CL_GUI_CFW DEFINITION LOAD.

*=======================================================================
* SELECTION-SCREEN
*=======================================================================
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS  FOR T001K-BUKRS OBLIGATORY,
                P_WERKS  FOR T001W-WERKS,
                P_ANLN1  FOR ANLA-ANLN1,
                P_ANLN2  FOR ANLA-ANLN2. "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
SELECTION-SCREEN: END OF BLOCK B1.

*=======================================================================
* START
*=======================================================================
START-OF-SELECTION.

  PERFORM: BUSCA_DADOS, PREPARA_SAIDA.
  CALL SCREEN 0100.
