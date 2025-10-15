*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_TOP
*&---------------------------------------------------------------------*

REPORT ZLESR0115.


TABLES: ZLEST0153, LFA1, KNA1.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF TY_SAIDA_0100,
         VTEXT         TYPE TZONT-VTEXT,
         DS_FORN       TYPE LFA1-NAME1,
         CNPJ_CPF_FORN TYPE LFA1-STCD1,
         DS_CLI        TYPE KNA1-NAME1,
         CNPJ_CPF_CLI  TYPE KNA1-STCD1,
         STCD3_FORN    TYPE KNA1-STCD3,
         STCD3_CLI     TYPE KNA1-STCD3,
         CAD_STANDARD  TYPE C LENGTH 4.
         INCLUDE STRUCTURE ZLEST0153.
TYPES END OF TY_SAIDA_0100.

TYPES: BEGIN OF TY_LFA1,
         LIFNR    TYPE LFA1-LIFNR,
         NAME1    TYPE LFA1-NAME1,
         STCD1    TYPE LFA1-STCD1,
         STCD2    TYPE LFA1-STCD2,
         STCD3    TYPE LFA1-STCD3,
         LAND1    TYPE LFA1-LAND1,
         LZONE    TYPE LFA1-LZONE.
TYPES END OF TY_LFA1.

TYPES: BEGIN OF TY_KNA1,
         KUNNR    TYPE KNA1-KUNNR,
         NAME1    TYPE KNA1-NAME1,
         STCD1    TYPE KNA1-STCD1,
         STCD2    TYPE KNA1-STCD2,
         STCD3    TYPE KNA1-STCD3,
         LAND1    TYPE KNA1-LAND1,
         LZONE    TYPE KNA1-LZONE.
TYPES END OF TY_KNA1.


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
DATA: IT_SAIDA_0100     TYPE TABLE OF TY_SAIDA_0100,
      WA_SAIDA_0100     TYPE TY_SAIDA_0100,
      TG_LFA1_ST        TYPE TABLE OF TY_LFA1   WITH HEADER LINE,
      TG_KNA1_ST        TYPE TABLE OF TY_KNA1   WITH HEADER LINE,
      TG_LFA1           TYPE TABLE OF TY_LFA1   WITH HEADER LINE,
      TG_KNA1           TYPE TABLE OF TY_KNA1   WITH HEADER LINE,
      TG_TZONT          TYPE TABLE OF TZONT     WITH HEADER LINE,
      TG_0153           TYPE TABLE OF ZLEST0153 WITH HEADER LINE,
      TG_0153_AUX       TYPE TABLE OF ZLEST0153 WITH HEADER LINE.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: VG_OPERACAO TYPE C LENGTH 20,
      VAR_ANSWER  TYPE C.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: C_NOVO       TYPE C VALUE 'NOVO'       LENGTH 4,
           C_DEL        TYPE C VALUE 'DEL'        LENGTH 4,
           C_CAD_ZONA   TYPE C VALUE 'CAD_ZONA'   LENGTH 8,
           C_CHANGE     TYPE C VALUE 'CHANGE'     LENGTH 6,
           C_TCODE_CONS TYPE C VALUE 'ZLES0152'   LENGTH 10,
           C_TCODE_CAD  TYPE C VALUE 'ZLES0151'   LENGTH 10.

*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_LAND1 FOR ZLEST0153-LAND1 NO INTERVALS NO-EXTENSION DEFAULT 'BR' NO-DISPLAY,
                P_ZONE1 FOR ZLEST0153-LZONE,
                P_KUNNR FOR ZLEST0153-KUNNR,
                P_ID1_D FOR KNA1-STCD1,
                P_ID2_D FOR KNA1-STCD2,
                P_ID3_D FOR KNA1-STCD3,
                P_LIFNR FOR ZLEST0153-LIFNR,
                P_ID1_K FOR LFA1-STCD1,
                P_ID2_K FOR LFA1-STCD2,
                P_ID3_K FOR LFA1-STCD3.
SELECTION-SCREEN: END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.

  CASE SY-TCODE.
    WHEN C_TCODE_CAD.
      SET TITLEBAR 'T1000'.
    WHEN C_TCODE_CONS.
      SET TITLEBAR 'T1000_C'.
  ENDCASE.

START-OF-SELECTION.

  PERFORM: F_SELECIONAR_DADOS,
           F_PROCESSA_DADOS.

 CALL SCREEN 0100.
