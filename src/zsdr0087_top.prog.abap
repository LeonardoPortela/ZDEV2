*&---------------------------------------------------------------------*
*&  Include           ZSDR0087_TOP
*&---------------------------------------------------------------------*

REPORT ZSDR0087.

*=======================================================================
* TABLES
*=======================================================================
TABLES: ZPARAM_CONT_FRET, TVV3T, TVAKT, T001L.

*=======================================================================
* TTYPES
*=======================================================================
TYPES: BEGIN OF TY_T006,
         MSEHI  TYPE T006-MSEHI,
       END OF TY_T006,

       BEGIN OF TY_ZLSCH,
         ZLSCH  TYPE T042Z-ZLSCH,
         TEXT1  TYPE T042Z-TEXT1,
       END OF TY_ZLSCH,

       BEGIN OF TY_ABRVW,
         ABRVW TYPE TVLVT-ABRVW,
         BEZEI TYPE TVLVT-BEZEI,
       END OF TY_ABRVW,

       BEGIN OF TY_HELP,
         ID(5)    TYPE C,
         TEXT(60) TYPE C,
       END OF TY_HELP.

*=======================================================================
* STRUCTURES E INTERNAL TABLES
*=======================================================================
DATA:   TG_ZPARAM TYPE TABLE OF ZPARAM_CONT_FRET,
        TG_SAIDA  TYPE TABLE OF ZPARAM_CONT_FRET,

        WG_ZPARAM LIKE LINE OF TG_ZPARAM,
        WG_SAIDA  LIKE LINE OF TG_SAIDA.

"tabelas para listas e ajuda de pesquisa:
DATA:   TG_UNIDADE      TYPE TABLE OF TY_T006,
        TG_ZLSCH        TYPE TABLE OF TY_ZLSCH,
        TG_TP_CALCULO   TYPE TABLE OF DD07V,
        TG_PRECO        TYPE TABLE OF DD07V,
        TG_C_DECIMAIS   TYPE TABLE OF DD07V,
        TG_PARAM_ESPEC  TYPE TABLE OF DD07V,
        TG_STATUS       TYPE TABLE OF DD07V,
        TG_ABRVW        TYPE TABLE OF TY_ABRVW.

*=======================================================================
* CONSTANTES
*=======================================================================
CONSTANTS: C_X              TYPE C VALUE 'X',
           C_ADD(3)         TYPE C VALUE 'ADD',
           C_DEL(3)         TYPE C VALUE 'DEL',
           C_EXIT(4)        TYPE C VALUE 'EXIT',
           C_BACK(4)        TYPE C VALUE 'BACK',
           C_SAVE(4)        TYPE C VALUE 'SAVE',
           C_PROCES(6)      TYPE C VALUE 'PROCES',
           C_CANCEL(6)      TYPE C VALUE 'CANCEL',
           C_CHANGE(6)      TYPE C VALUE 'CHANGE',
           C_SEARCH(6)      TYPE C VALUE 'SEARCH',
           C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE'.

*=======================================================================
* ALV
*=======================================================================
CLASS: LCL_ALV_TOOLBAR DEFINITION DEFERRED.

DATA: G_CONTAINER          TYPE SCRFNAME,
      G_GRID               TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      GT_FIELDCAT          TYPE LVC_T_FCAT,
      GS_LAYOUT            TYPE LVC_S_LAYO,
      I_SELECTED_ROWS      TYPE LVC_T_ROW,
      W_SELECTED_ROWS      TYPE LVC_S_ROW,

      WG_DISPLAY,
      WG_MENSAGEM(30),
      TG_SELECTEDCELL      TYPE LVC_T_CELL,
      WG_SELECTEDCELL      TYPE LVC_S_CELL,
      TG_SELECTEDROW       TYPE LVC_T_ROW,
      WG_SELECTEDROW       TYPE LVC_S_ROW,
      TY_TOOLBAR           TYPE STB_BUTTON,
      WA_STABLE            TYPE LVC_S_STBL,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      TG_MSG_RET           TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE.

DATA: WL_REPID    TYPE SY-REPID,
      TL_FUNCTION TYPE UI_FUNCTIONS,
      WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE.

*=======================================================================
* CLASSES
*=======================================================================
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*CONSTRUCTOR
    METHODS: CONSTRUCTOR
      IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,

        ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
          IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

ENDCLASS.

CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.

    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.

  METHOD ON_TOOLBAR.

     CLEAR TY_TOOLBAR.
*     TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
     TY_TOOLBAR-FUNCTION  = C_ADD.
     TY_TOOLBAR-TEXT      = 'Entradas Novas'.
     TY_TOOLBAR-BUTN_TYPE = 0.
     APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
     CLEAR TY_TOOLBAR.

     TY_TOOLBAR-ICON       = ICON_DELETE_ROW.
     TY_TOOLBAR-FUNCTION   = C_DEL.
     TY_TOOLBAR-TEXT       = 'Deletar'.
     TY_TOOLBAR-BUTN_TYPE  = 0.
     APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
     CLEAR TY_TOOLBAR.

     CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
*********************Add Line*********************************
      WHEN C_ADD.

*        PERFORM CARREGA_LIST.
        CALL SCREEN 0102.

*********************Delete Line*********************************
      WHEN C_DEL.
        DATA: ANS       TYPE C.

        CALL METHOD G_GRID->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TG_SELECTEDROW.


        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = 'Confirmação'
            TEXT_QUESTION         = 'Deseja eliminar a(s) linha(s) selecionada(s)?'
            TEXT_BUTTON_1         = 'Sim'
            ICON_BUTTON_1         = 'ICON_CHECKED'
            TEXT_BUTTON_2         = 'Não'
            ICON_BUTTON_2         = 'ICON_CANCEL'
            POPUP_TYPE            = 'ICON_MESSAGE_ERROR'
            DISPLAY_CANCEL_BUTTON = ''
          IMPORTING
            ANSWER                = ANS.

        CASE ANS.
          WHEN 2 OR 'A'.
            LEAVE TO CURRENT TRANSACTION.
          WHEN 1.
            PERFORM DELETA_DADOS.
        ENDCASE.

    ENDCASE.

    IF G_GRID IS NOT INITIAL.
      CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    ENDIF.


  ENDMETHOD.
ENDCLASS.               " LCL_ALV_TOOLBAR IMPLEMENTATION.

CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.

    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.

    CLASS-METHODS:
      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      ON_BUTTON_CLICK FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
        IMPORTING ES_COL_ID ES_ROW_NO.

    CLASS-METHODS:
      ON_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA
                    ET_BAD_CELLS E_DISPLAY.

ENDCLASS.

CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD ON_DOUBLE_CLICK.
    PERFORM DOUBLE_CLICK USING E_ROW E_COLUMN.
    CALL SCREEN 0102.
  ENDMETHOD.

  METHOD ON_DATA_CHANGED.

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_VALUE TYPE LVC_VALUE.

  ENDMETHOD.

  METHOD ON_DATA_CHANGED_FINISHED.

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_VALUE TYPE LVC_VALUE.

    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
      WHERE TABIX GT 0.

      READ TABLE TG_SAIDA INTO WG_SAIDA INDEX LS_GOOD-ROW_ID.

    ENDLOOP.

  ENDMETHOD.

  METHOD ON_BUTTON_CLICK.

    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.

  METHOD ON_ONF4.
* Set code for help
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  CALL SCREEN 0100.
