*&---------------------------------------------------------------------*
*&  Include           ZFIS37_TOP
*&---------------------------------------------------------------------*

PROGRAM ZFIS37.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES:  BEGIN OF TY_ZFIT0145,
          LIFNR      TYPE LFA1-LIFNR,
          NAME1      TYPE LFA1-NAME1,
          DATA_ATUAL TYPE ZFIT0145-DATA_ATUAL,
          HORA_ATUAL TYPE ZFIT0145-HORA_ATUAL,
          USUARIO    TYPE ZFIT0145-USUARIO,
        END OF TY_ZFIT0145,

        BEGIN OF TY_FIELDS,
          CAMPO(30) TYPE C,
          GROUP1(5) TYPE C,
          VALUE     TYPE SY-TABIX,
          INVISIBLE TYPE SY-TABIX,
        END OF TY_FIELDS.


*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*

DATA: WG_ZFIT0145 TYPE TY_ZFIT0145,
      TG_SAIDA    TYPE TABLE OF TY_ZFIT0145,
      WG_SAIDA    TYPE TY_ZFIT0145.


** Criação de tabela dinamica
DATA: T_FIELDCATALOG TYPE LVC_T_FCAT,
      W_FIELDCATALOG TYPE LVC_S_FCAT,
      WA_LAYOUT      TYPE LVC_S_LAYO,
      WA_STABLE      TYPE LVC_S_STBL,

      TG_FIELDS      TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_MSG_RET     TYPE TABLE OF ZFIWRS0002  WITH HEADER LINE.


*Class definition for ALV toolbar
CLASS:  LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: G_CONTAINER          TYPE SCRFNAME VALUE 'CC_MOVTO',
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID1                TYPE REF TO CL_GUI_ALV_GRID,

      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,

      OBG_DOCKING          TYPE REF TO CL_GUI_DOCKING_CONTAINER,

      WA_STYLE             TYPE LVC_S_STYL,
      STYLE                TYPE LVC_T_STYL   WITH HEADER LINE,
      STYLE2               TYPE LVC_T_STYL   WITH HEADER LINE,
      GS_VARIANT_C         TYPE DISVARIANT.

*Declaration for toolbar buttons
DATA: TY_TOOLBAR TYPE STB_BUTTON.

DATA: OK-CODE       TYPE SY-UCOMM,
      TL_INDEX_ROWS TYPE LVC_T_ROW,
      WL_INDEX_ROWS TYPE LVC_S_ROW,
      X_FIELD(30).

DATA: OK_CODE         LIKE SY-UCOMM,
      WG_MENSAGEM(30),
      WG_ACAO(30).


*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:
  C_0               TYPE C VALUE '0',
  C_1               TYPE C VALUE '1',
  C_2               TYPE C VALUE '2',
  C_B               TYPE C VALUE 'B',
  C_S               TYPE C VALUE 'S',
  C_L               TYPE C VALUE 'L',
  C_X               TYPE C VALUE 'X',
  C_D               TYPE C VALUE 'D',
  C_K               TYPE C VALUE 'K',
  C_W               TYPE C VALUE 'W',
  C_F               TYPE C VALUE 'F',
  C_T               TYPE C VALUE 'T',
  C_I               TYPE C VALUE 'I',
  C_N               TYPE C VALUE 'N',
  C_H               TYPE C VALUE 'H',
  C_AG(2)           TYPE C VALUE 'AG',
  C_NE(2)           TYPE C VALUE 'NE',
  C_01(2)           TYPE C VALUE '01',
  C_30(2)           TYPE C VALUE '30',
  C_40(2)           TYPE C VALUE '40',
  C_50(4)           TYPE C VALUE '0050',
  C_76(2)           TYPE C VALUE '76',
  C_71(2)           TYPE C VALUE '71',
  C_72(2)           TYPE C VALUE '72',
  C_BR(2)           TYPE C VALUE 'BR',
  C_LF(2)           TYPE C VALUE 'LF',
  C_LR(2)           TYPE C VALUE 'LR',
  C_Z1(2)           TYPE C VALUE 'Z1',
  C_ADD(3)          TYPE C VALUE 'ADD',
  C_DEL(3)          TYPE C VALUE 'DEL',
  C_DG1(3)          TYPE C VALUE 'DG1',
  C_DG2(3)          TYPE C VALUE 'DG2',
  C_DUMMY_HEADER(3) TYPE C VALUE '099',
  C_DUMMY_ITENS(3)  TYPE C VALUE '098',
  C_EXIT(4)         TYPE C VALUE 'EXIT',
  C_ROOT(4)         TYPE C VALUE 'ROOT',
  C_MINIMIZAR(4)    TYPE C VALUE '@K2@',
  C_MAXIMIZAR(4)    TYPE C VALUE '@K1@',
  C_BACK(4)         TYPE C VALUE 'BACK',
  C_SAVE(4)         TYPE C VALUE 'SAVE',
  C_DESAT(5)        TYPE C VALUE 'DESAT',
  C_DMBTR(5)        TYPE C VALUE 'DMBTR',
  C_MODIF(5)        TYPE C VALUE 'MODIF',
  C_CANCEL(6)       TYPE C VALUE 'CANCEL',
  C_DELDOC(6)       TYPE C VALUE 'DELDOC',
  C_DCLICK(6)       TYPE C VALUE 'DCLICK',
  C_SEARCH(6)       TYPE C VALUE 'SEARCH',
  C_ATUALI(6)       TYPE C VALUE 'ATUALI',
  C_ADD_MSG(7)      TYPE C VALUE 'ADD_MSG',
  C_DEL_MSG(7)      TYPE C VALUE 'DEL_MSG',
  C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG',
  C_SAVE_MSG(8)     TYPE C VALUE 'SAVE_MSG',
  C_DISPLA(6)       TYPE C VALUE 'DISPLA',
  C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.

    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      ON_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      CONSTRUCTOR
        IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.

    IF WG_ACAO NE C_MODIF AND WG_ACAO NE C_ADD.
      WL_DESACTIVE = 1.
    ENDIF.

    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  = C_ADD.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = C_DEL.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*   variable for Toolbar Button
    TY_TOOLBAR-ICON      = ICON_VIEW_CLOSE.
    TY_TOOLBAR-FUNCTION  = C_CLOS_MSG.
    TY_TOOLBAR-DISABLED  = SPACE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.
    DATA: TG_SAIDA_AUX TYPE TABLE OF TY_ZFIT0145,
          WL_SAIDA     TYPE TY_ZFIT0145,
          WL_LINES     TYPE SY-TABIX.

    REFRESH: TG_SAIDA_AUX..

    CASE E_UCOMM.
      WHEN C_ADD.
        CALL METHOD GRID1->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TL_INDEX_ROWS.

        IF  TL_INDEX_ROWS[] IS NOT INITIAL.
          READ TABLE TL_INDEX_ROWS INTO WL_INDEX_ROWS INDEX 1.
        ENDIF.

        TG_SAIDA_AUX[] = TG_SAIDA[].
        REFRESH: TG_SAIDA.
        LOOP AT TG_SAIDA_AUX INTO WL_SAIDA.
          APPEND WL_SAIDA TO TG_SAIDA.
          IF SY-TABIX = WL_INDEX_ROWS-INDEX.
            CLEAR: WL_SAIDA.
            APPEND WL_SAIDA TO TG_SAIDA.
          ENDIF.
        ENDLOOP.
        IF WL_INDEX_ROWS-INDEX = 0.
          CLEAR: WL_SAIDA.
          APPEND WL_SAIDA TO TG_SAIDA.
        ENDIF.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.


      WHEN C_DEL.

        CALL METHOD GRID1->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TL_INDEX_ROWS.

        LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
          DELETE TG_SAIDA INDEX WL_INDEX_ROWS-INDEX.
        ENDLOOP.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.

    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD ON_DOUBLE_CLICK.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD ON_DATA_CHANGED.

    DATA: WL_LFA1  TYPE LFA1,
          LV_VALUE TYPE LVC_VALUE,
          LS_GOOD  TYPE LVC_S_MODI.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                               INTO LS_GOOD
                               WHERE FIELDNAME = 'LIFNR'.

      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      READ TABLE TG_SAIDA INTO WG_SAIDA INDEX LS_GOOD-ROW_ID.
      WL_LFA1-LIFNR = LV_VALUE.

      SELECT SINGLE *
      FROM LFA1
      INTO WL_LFA1
      WHERE LIFNR = WL_LFA1-LIFNR.

      LV_VALUE = WL_LFA1-NAME1.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'NAME1'
          I_VALUE     = LV_VALUE.

      LV_VALUE = SY-UNAME.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'USUARIO'
          I_VALUE     = LV_VALUE.

      LV_VALUE = SY-DATUM.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'DATA_ATUAL'
          I_VALUE     = LV_VALUE.

      LV_VALUE = SY-UZEIT.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'HORA_ATUAL'
          I_VALUE     = LV_VALUE.

    ENDLOOP.


  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_DATA_CHANGED_FINISHED.


  ENDMETHOD.                    "on_data_changed_finished

  METHOD ON_ONF4.

  ENDMETHOD.                    "ON_ONF4

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
