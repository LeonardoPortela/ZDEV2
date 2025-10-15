*&---------------------------------------------------------------------*
*&  Include           ZFIR0068_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS ICON.
**********************************************************************
*Tipo de erro
**********************************************************************

TYPES: BEGIN OF ty_msgerro,
    erro000	TYPE string,
    erro001	TYPE string,
    erro002	TYPE string,
    erro003	TYPE string,
    erro004	TYPE string,
    erro005	TYPE string,
    erro006	TYPE string,
    erro007	TYPE string,
    erro008	TYPE string,
    erro009	TYPE string,
    erro010	TYPE string,
    erro011	TYPE string,
    erro012	TYPE string,
    erro013	TYPE string,
    erro014	TYPE string,
    erro015	TYPE string,
    erro016	TYPE string,
    erro017	TYPE string,
    erro018	TYPE string,
    erro019	TYPE string,
    erro020	TYPE string,
    erro021	TYPE string,
    erro022	TYPE string,
    erro023	TYPE string,
    erro024	TYPE string,
  END OF ty_msgerro.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*

TYPES: BEGIN OF TY_DOC_PREV.
         INCLUDE STRUCTURE ZFIT0115.
TYPES  END OF TY_DOC_PREV.

TYPES: BEGIN OF TY_SAIDA_LIST,
         BUTXT   TYPE T001-BUTXT,
         DEP_RESP_DESC   TYPE ZIMP_CAD_DEPTO-DEP_RESP_DESC,
         DESCRICAO       TYPE ZFIT0109-DESCRICAO,
         VALOR           TYPE ZFIT0115-DMBTR,
         VALOR_OLD       TYPE ZFIT0115-DMBTR,
         DT_VCTO_OLD     TYPE ZFIT0115-DT_VCTO.
         INCLUDE STRUCTURE ZFIT0115.
TYPES: END OF TY_SAIDA_LIST.

TYPES: BEGIN OF TY_SAIDA_INPUT,
         BUTXT   TYPE T001-BUTXT,
         DEP_RESP_DESC   TYPE ZIMP_CAD_DEPTO-DEP_RESP_DESC,
         DESCRICAO       TYPE ZFIT0109-DESCRICAO,
         ST_CALC_SDO     TYPE ZFIT0109-ST_CALC_SDO,
         VALOR           TYPE ZFIT0115-DMBTR,
         FIELD_STYLE     TYPE LVC_T_STYL,
         STATUS          TYPE STRING.
         INCLUDE STRUCTURE ZFIT0115.
TYPES: END OF TY_SAIDA_INPUT.


TYPES: BEGIN OF TY_BUKRS_REP,
         BUKRS           TYPE T001-BUKRS,
         DT_VERSAO       TYPE ZFIT0079-DT_BASE_VERSAO,
         HORA_VERSAO     TYPE ZFIT0079-HORA_VERSAO,
         VERSAO          TYPE ZFIT0079-VERSAO,
       END OF TY_BUKRS_REP.

field-SYMBOLS: <wa_saida_input> type  TY_SAIDA_INPUT. "PSA

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_LIST DEFINITION.
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


CLASS LCL_ALV_TOOLBAR_INPUT DEFINITION.
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


CLASS LCL_EVENT_HANDLER_0101 DEFINITION.

  PUBLIC SECTION.                                           "

    CLASS-METHODS:
      USER_COMMAND    FOR EVENT USER_COMMAND  OF CL_GUI_ALV_GRID
           IMPORTING  E_UCOMM.

    CLASS-METHODS:
       ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
                        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      ON_F4            FOR EVENT ONF4                 OF CL_GUI_ALV_GRID
       IMPORTING  E_FIELDNAME
                  ES_ROW_NO
                  ER_EVENT_DATA
                  ET_BAD_CELLS
                  E_DISPLAY.

    CLASS-METHODS:
       ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                       IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.

ENDCLASS.


DATA: OBJ_ALV_LIST         TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_LIST   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV_INPUT        TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_INPUT  TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: GT_CATALOG       TYPE LVC_T_FCAT,
      GW_CATALOG       TYPE LVC_S_FCAT.

DATA: IT_SELECTED_ROWS TYPE LVC_T_ROW,
      WA_SELECTED_ROWS TYPE LVC_S_ROW.

DATA: OBJ_TOOLBAR_LIST  TYPE REF TO LCL_ALV_TOOLBAR_LIST,
      OBJ_TOOLBAR_INPUT TYPE REF TO LCL_ALV_TOOLBAR_INPUT.

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

DATA: GT_F4  TYPE LVC_T_F4 WITH HEADER LINE.

* Objetos
DATA: C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      TY_TOOLBAR TYPE STB_BUTTON.


*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: IT_SAIDA_LIST       TYPE TABLE OF TY_SAIDA_LIST,
      WA_SAIDA_LIST       TYPE TY_SAIDA_LIST,
      IT_SAIDA_INPUT      TYPE TABLE OF TY_SAIDA_INPUT,
      WA_SAIDA_INPUT      TYPE TY_SAIDA_INPUT,
      WA_DOC_PREV         TYPE TY_DOC_PREV,
      "WA_PARAM            TYPE TY_PARAM,
      WA_0115             TYPE ZFIT0115,
      WA_0079             TYPE ZFIT0079,
      TG_BUKRS_REP        TYPE TABLE OF TY_BUKRS_REP WITH HEADER LINE,
      TG_0115             TYPE TABLE OF ZFIT0115 WITH HEADER LINE,
      TG_CAD_DEPTO        TYPE TABLE OF ZIMP_CAD_DEPTO WITH HEADER LINE,
      TG_0109             TYPE TABLE OF ZFIT0109 WITH HEADER LINE,
      TG_0109_AUX         TYPE TABLE OF ZFIT0109 WITH HEADER LINE,
      TG_T001             TYPE TABLE OF T001 WITH HEADER LINE,
      aux_vbeln           TYPE VBAK-vbeln,
      WA_0109_AUX         TYPE ZFIT0109,
      wa_msgerros         type ty_msgerro,
      msg_erro            type string.


*-------------------------------------------------------------------
* Ranges
*-------------------------------------------------------------------
RANGES:  P_BUKRS     FOR T001-BUKRS,  "Empresa
         P_DEP_RESP  FOR ZIMP_CAD_DEPTO-DEP_RESP,  "Departamento
         P_DT_VCTO   FOR ZFIT0115-DT_VCTO.  "Data Vcto

RANGES:  IT_BUKRS     FOR T001-BUKRS,  "Empresa
         IT_DEP_RESP  FOR ZIMP_CAD_DEPTO-DEP_RESP,  "Departamento
         IT_DT_VCTO   FOR ZFIT0115-DT_VCTO.  "Data Vcto


*-------------------------------------------------------------------
* Variaveis
*-------------------------------------------------------------------

DATA: VG_RET_CONSULTA  TYPE C,
      VG_TX_USD_BRL    TYPE UKURS_CURR,
      VG_TX_USD_ARS    TYPE UKURS_CURR,
      VG_TX_EUR_BRL    TYPE UKURS_CURR,
      VG_TX_EUR_USD    TYPE UKURS_CURR,
      VG_SUGERE_CAMPOS TYPE C.

*---------------------------------------------------------------------*
* Constantes
*---------------------------------------------------------------------*

CONSTANTS: C_ARS VALUE 'ARS' TYPE C LENGTH 3,
           C_BRL VALUE 'BRL' TYPE C LENGTH 3,
           C_USD VALUE 'USD' TYPE C LENGTH 3.

**********************************************************************
* Erro Resultado
**********************************************************************
TYPES ctext TYPE string.
DATA: itab_erro  TYPE TABLE OF ctext,
      result_erro TYPE string.
