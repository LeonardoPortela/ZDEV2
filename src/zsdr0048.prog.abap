REPORT  ZSDR0048.

TABLES: ZSDT0094, ZSDT0040.

TYPES: BEGIN OF TY_ZSDT0094.
        INCLUDE STRUCTURE ZSDT0094.
TYPES:  CHECK(1)      TYPE C,
        FIELD_STYLE   TYPE LVC_T_STYL,
        COLOR_LINE(4) TYPE C,
        COLOR_CELL    TYPE LVC_T_SCOL.
TYPES END OF TY_ZSDT0094.

TYPES: BEGIN OF TY_TAXA,
         DATA_INICIAL TYPE ZSDT0094-DATA_REGISTRO,
         DATA_FINAL   TYPE ZSDT0094-DATA_REGISTRO,
         TIPO_TAXA    TYPE C,
         COTACAO      TYPE KURRF,
       END OF TY_TAXA.

DATA:  BEGIN OF GT_VALUES OCCURS 0,
         DOMVALUE_L TYPE DOMVALUE_L,
         DDTEXT     TYPE VAL_TEXT,
       END OF GT_VALUES.

TYPES: BEGIN OF TY_F4,
         PROGRAMA   TYPE SYCPROG,
*         NRO_SOL_OV TYPE ZSDED013,
         BEZEI      TYPE BEZEI30,
       END OF TY_F4.



*--------------------------------------------------------------------*
*  Internal Table and Work Area
*--------------------------------------------------------------------*

DATA: IT_ZSDT0094       TYPE TABLE OF TY_ZSDT0094,
      WA_ZSDT0094       TYPE TY_ZSDT0094,
      WL_ZSDT0094       TYPE ZSDT0094,
      IT_ZSDT0094_INPUT TYPE TABLE OF ZSDT0094,
      GT_ZSDT0094       TYPE TABLE OF ZSDT0094,

      IT_ADD            TYPE TABLE OF TY_ZSDT0094,
      WA_ADD            TYPE TY_ZSDT0094,

      TL_SELECTION      TYPE TABLE OF TY_ZSDT0094,
      WA_SELECTION      TYPE TY_ZSDT0094,
      SL_ZSDT0094       LIKE LINE OF IT_ZSDT0094,

      WA_TAXA           TYPE TY_TAXA,
      CX3               TYPE CHAR30,
      CONT              TYPE SY-TABIX,
      VAR_DIR           TYPE C,

      LS_DROPDOWN       TYPE LVC_S_DROP,
      LT_DROPDOWN       TYPE LVC_T_DROP,
      VAR_EXIT(1),
      COL               TYPE LVC_S_SCOL,
      COLTAB            TYPE LVC_T_SCOL,
      COLOR             TYPE LVC_S_COLO,
      LT_F4             TYPE LVC_T_F4,
      WT_F4             TYPE LVC_S_F4,

      IT_F4             TYPE TABLE OF TY_F4,
      IT_RETURN         TYPE TABLE OF DDSHRETVAL,
      WA_RETURN         LIKE LINE  OF IT_RETURN,


*-----------------------------------------------------------------*
*  Alv
*-----------------------------------------------------------------*
      OBJ_CUSTOM        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT           TYPE LVC_T_FCAT,
      IT_FCAT_0102      TYPE LVC_T_FCAT,
      WA_FCAT           TYPE LVC_S_FCAT,

      OBJ_CUSTOM_0102   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_GRID_0102     TYPE REF TO CL_GUI_ALV_GRID,

      GS_STABLE         TYPE LVC_S_STBL,
*------------------------------------------------------------------*
* ALV Layout
*------------------------------------------------------------------*
      GS_LAYOUT         TYPE LVC_S_LAYO,

*------------------------------------------------------------------*
* ALV Selection
*------------------------------------------------------------------*

      IT_SELECTED_ROWS  TYPE LVC_T_ROW,
      WA_SELECTED_ROWS  TYPE LVC_S_ROW,
      LS_EDIT           TYPE LVC_S_STYL,
      LT_EDIT           TYPE LVC_T_STYL,
      ROW_EDIT          TYPE LVC_S_STYL.

*Class Definition for ALV toolbar
*------------------------------------------------------------
CLASS: LCL_ALV_TOOLBAR DEFINITION DEFERRED.

DATA: C_ALV_TOOLBARMANAGER      TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      C_ALV_TOOLBARMANAGER_0102 TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      OBJ_TOOLBAR               TYPE REF TO LCL_ALV_TOOLBAR,
      OBJ_TOOLBAR_0102          TYPE REF TO LCL_ALV_TOOLBAR,
      WL_DESACTIVE,
      GF_FIRST_DISPLAY_0100     TYPE C VALUE 'X',
      GF_FIRST_DISPLAY_0102     TYPE C VALUE 'X',
* Declaration for toolbar buttons
      TY_TOOLBAR                TYPE STB_BUTTON,
      TY_TOOLBAR_0102           TYPE STB_BUTTON,
* ALV Layout Variant
      GS_VARIANT                TYPE DISVARIANT,
* ALV Excluded Functions
      IT_EXCLUDE_FCODE          TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE          LIKE LINE OF IT_EXCLUDE_FCODE,

      IT_FCODE_0102             TYPE UI_FUNCTIONS,
      WA_FCODE_0102             LIKE LINE OF IT_EXCLUDE_FCODE.

* Armazena a cotação do WebService
*      VAR_COTACAO     TYPE KURRF.

* Radio Button para o Tipo de taxa
SELECTION-SCREEN: BEGIN OF SCREEN 0103 AS SUBSCREEN.
PARAMETERS: C RADIOBUTTON GROUP RD1, " Tipo de Taxa Compra
            V RADIOBUTTON GROUP RD1. " Tipo de Taxa Venda
SELECTION-SCREEN: END OF SCREEN 0103.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
SELECT-OPTIONS P_SOL_OV FOR ZSDT0040-DOC_SIMULACAO OBLIGATORY.
SELECT-OPTIONS P_FIX    FOR ZSDT0094-FIXACAO.
SELECT-OPTIONS P_DATA   FOR ZSDT0094-DATA_REGISTRO.
SELECT-OPTIONS P_HORA   FOR ZSDT0094-HORA_REGISTRO.
SELECTION-SCREEN END OF BLOCK B1.

*--------------------------------------------------------------------*
* Start of Selection
*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM SELECIONAR_DADOS.
  CALL SCREEN 0100.
*--------------------------------------------------------------------*
* Includes
*--------------------------------------------------------------------*
  INCLUDE ZSDR0048_CLASS.
  INCLUDE ZSDR0048_PBO_0100.
  INCLUDE ZSDR0048_PAI_0100.
  INCLUDE ZSDR0048_FORM.

END-OF-SELECTION.
