*&---------------------------------------------------------------------*
*&  Include           ZSDR0112_TOP
*&---------------------------------------------------------------------*

REPORT ZSDR0112.


TABLES: ZSDT0254, J_1BNFDOC.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF TY_SAIDA_0100,
         DOCNUM                 TYPE ZSDT0254-DOCNUM,
         BRANCH                 TYPE ZSDT0254-BRANCH,
         DOCDAT                 TYPE ZSDT0254-DOCDAT,
         CHAVE                  TYPE ZSDT0254-CHAVE,
         STCD1                  TYPE J_1BNFE_ACTIVE-STCD1,
         NFNUM9                 TYPE J_1BNFE_ACTIVE-NFNUM9,
         MODEL                  TYPE J_1BNFE_ACTIVE-MODEL,
         ST_INUTILIZACAO        TYPE C LENGTH 4.
TYPES END OF TY_SAIDA_0100.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_0100 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
                IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
    ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
               IMPORTING  E_OBJECT,

    HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
               IMPORTING E_UCOMM.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS LCL_EVENT_HANDLER_0100 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      CATCH_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
ENDCLASS.

DATA: OBJ_ALV_0100         TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_0100   TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: GT_CATALOG       TYPE LVC_T_FCAT,
      GW_CATALOG       TYPE LVC_S_FCAT.

DATA: OBJ_TOOLBAR_0100 TYPE REF TO LCL_ALV_TOOLBAR_0100.

* ALV field catalogs
DATA: IT_FCAT    TYPE LVC_T_FCAT,
      WA_FCAT    TYPE LVC_S_FCAT.

* ALV excluded functions
DATA: IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE.

* Alv Styles
DATA: LS_EDIT         TYPE LVC_S_STYL,
      LT_EDIT         TYPE LVC_T_STYL.

* ALV layout variant
DATA: GS_VARIANT       TYPE DISVARIANT.

* ALV layout
DATA: GS_LAYOUT        TYPE LVC_S_LAYO.

* ALV Stable
DATA: WA_STABLE        TYPE LVC_S_STBL.

DATA: IT_SELECTEDCELL  TYPE LVC_T_CELL,
      WA_SELECTEDCELL  TYPE LVC_S_CELL.

DATA: IT_SEL_ROWS         TYPE LVC_T_ROW,
      WA_SEL_ROWS         TYPE LVC_S_ROW.

DATA: GT_ESTILO   TYPE LVC_T_STYL WITH HEADER LINE,
      WL_ESTILO   TYPE LVC_S_STYL.

DATA: GT_F4  TYPE LVC_T_F4 WITH HEADER LINE.

* Objetos
DATA: C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      TY_TOOLBAR TYPE STB_BUTTON.

DATA: WA_ESTRUTURA       TYPE TY_ESTRUTURA,
      ESTRUTURA          TYPE TABLE OF TY_ESTRUTURA.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: IT_SAIDA_0100   TYPE TABLE OF TY_SAIDA_0100,
      WA_SAIDA_0100   TYPE TY_SAIDA_0100,
      TG_J_1BNFDOC    TYPE TABLE OF J_1BNFDOC      WITH HEADER LINE,
      TG_ACTIVE       TYPE TABLE OF J_1BNFE_ACTIVE WITH HEADER LINE,
      TG_ACTIVE_STRD  TYPE TABLE OF J_1BNFE_ACTIVE WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& SHDB                                                               &*
*&--------------------------------------------------------------------&*
DATA: TI_BDCDATA TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      WA_BDCDATA LIKE LINE OF TI_BDCDATA,
      TL_BDC     TYPE TABLE OF BDCDATA,
      WL_BDC     TYPE BDCDATA,
      OPT        TYPE CTU_PARAMS.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: VG_OPERACAO TYPE C LENGTH 20,
      VAR_ANSWER  TYPE C.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: C_ENVIAR_INUT_GRC TYPE C VALUE 'ENVIAR_INUT_GRC'  LENGTH 50.

*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: P_BUKRS  FOR J_1BNFDOC-BUKRS,
                  P_DOCDAT FOR J_1BNFDOC-DOCDAT OBLIGATORY.

SELECTION-SCREEN END OF BLOCK B1.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

  PARAMETERS:

    P_GER    RADIOBUTTON GROUP RB1,
    P_PEN    RADIOBUTTON GROUP RB1,
    P_AMB    RADIOBUTTON GROUP RB1 DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK B2.

START-OF-SELECTION.

  PERFORM: F_SELECIONAR_DADOS,
           F_PROCESSA_DADOS,
           F_CALL_ALV.
