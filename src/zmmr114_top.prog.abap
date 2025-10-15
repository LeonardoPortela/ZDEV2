*&---------------------------------------------------------------------*
*&  Include           ZMMR114_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF TY_ITENS.
        INCLUDE           TYPE ZMMT0067.
TYPES: STATUS  TYPE ICON-ID,
       T_WERKS TYPE QC_WERKS_RANGE,
       ESTILO  TYPE LVC_T_STYL,
       TXZ01   TYPE MAKT-MAKTX,
       END OF TY_ITENS,


       BEGIN OF TY_FIELDS_STYLE,
         FIELDNAME TYPE LVC_FNAME,
       END OF TY_FIELDS_STYLE,

       BEGIN OF TY_FIELDS,
         CAMPO     TYPE CHAR30,
         GROUP1    TYPE CHAR3,
         GROUP2    TYPE CHAR3,
         VALUE     TYPE CHAR1,
         INVISIBLE TYPE CHAR1,
       END OF TY_FIELDS.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*

CONSTANTS:
  C_0              TYPE C VALUE '0',
  C_1              TYPE C VALUE '1',
  C_X              TYPE C VALUE 'X',
  C_I              TYPE C VALUE 'I',
  C_N              TYPE C VALUE 'N',
  C_NE(2)          TYPE C VALUE 'NE',
  C_ADD(3)         TYPE C VALUE 'ADD',
  C_DEL(3)         TYPE C VALUE 'DEL',
  C_DG1(3)         TYPE C VALUE 'DG1',
  C_DG2(3)         TYPE C VALUE 'DG2',
  C_EXIT(4)        TYPE C VALUE 'EXIT',
  C_BACK(4)        TYPE C VALUE 'BACK',
  C_SAVE(4)        TYPE C VALUE 'SAVE',
  C_MODIF(5)       TYPE C VALUE 'MODIF',
  C_CANCEL(6)      TYPE C VALUE 'CANCEL',
  C_DELDOC(6)      TYPE C VALUE 'DELDOC',
  C_SEARCH(6)      TYPE C VALUE 'SEARCH',
  C_DISPLA(6)      TYPE C VALUE 'DISPLA',
  C_ATUALI(6)      TYPE C VALUE 'ATUALI',
  C_CLOS_MSG(8)    TYPE C VALUE 'CLOS_MSG',
  C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE'.

************************************************************************************************************************************

*&--------------------------------------------------------------------&*
*& Variáveis  de tela                                                 &*
*&--------------------------------------------------------------------&*
RANGES: R_WERKS  FOR EKPO-WERKS.
FIELD-SYMBOLS <FS_SAIDA_ITENS> TYPE TY_ITENS.

DATA: WG_BUKRS            TYPE BUKRS,
      BTN_DISPLAY_WERKS   TYPE ICON-ID VALUE ICON_ENTER_MORE,
      C_BTN_DISPLAY_WERKS TYPE CHAR20 VALUE 'BTN_DISPLAY_WERKS',
      GT_SAIDA_ITENS      TYPE TABLE OF TY_ITENS,
      WL_SAIDA_ITENS      TYPE TY_ITENS,
      GT_ZMMT0067         TYPE TABLE OF ZMMT0067,
      WL_MAKT             TYPE MAKT,
      WL_EXCLUDE          TYPE UI_FUNCTIONS,

      GT_FCAT_0110        TYPE TABLE OF LVC_S_FCAT,
      GT_SELECTED_ROWS    TYPE LVC_T_ROW,
      GT_SELECTEDCELL     TYPE LVC_T_CELL,
      WL_SELECTED_ROWS    TYPE LVC_S_ROW,

      GT_RETURN           TYPE TABLE OF BAPIRET2,
      GT_FIELDS           TYPE TABLE OF TY_FIELDS,
      GT_FIELDS_STYLE     TYPE TABLE OF TY_FIELDS_STYLE,
      WL_FCAT             TYPE LVC_S_FCAT,
      WL_LAYOUT           TYPE LVC_S_LAYO,
      WL_STABLE           TYPE LVC_S_STBL,


      OBJ_CUSTOM_0110     TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV_0110        TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_TOOLBAR_MANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.
*&--------------------------------------------------------------------&*
*& Variáveis                                                          &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE         TYPE SY-UCOMM,
      X_FIELD(30),
      WG_ACAO(3),
      WG_MENSAGEM(30),
      VG_OP_MODE      TYPE CHAR20.

DATA: GT_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
      GT_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.


*----------------------------------------------------------------------*
*       CLASS CL_UTILS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS CL_UTILS DEFINITION.
  PUBLIC SECTION.
    DATA:  MW_WERKS           TYPE WERKS_RANG.

ENDCLASS.                    "Z_UTILS DEFINITION

*----------------------------------------------------------------------*
*       CLASS Z_UTILS IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS CL_UTILS IMPLEMENTATION.

ENDCLASS.

DATA R_UTILS TYPE REF TO CL_UTILS.

"CLASSES
*&---------------------------------------------------------------------*
*&  Include           Z_CLASS_EVENT_TOOLBAR
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_TOOLBAR DEFINITION.
  PUBLIC SECTION.
    METHODS:
      CONSTRUCTOR IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.

    CLASS-METHODS:
      SET_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT.

    CLASS-METHODS:
      GET_UCOMM FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

    CLASS-METHODS:
      ON_CLICK FOR EVENT HOTSPOT_CLICK  OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    CREATE OBJECT OBJ_TOOLBAR_MANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD SET_TOOLBAR.
    DATA: WL_TOOLBAR TYPE STB_BUTTON,
          LV_DISABLE TYPE C.

    DEFINE D_SET.
      WL_TOOLBAR-FUNCTION     = &1.
      WL_TOOLBAR-ICON         = &2.
      WL_TOOLBAR-BUTN_TYPE    = &3.
      WL_TOOLBAR-TEXT         = &4.
      WL_TOOLBAR-DISABLED     = &5.

      APPEND WL_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      CLEAR WL_TOOLBAR.
    END-OF-DEFINITION.

    IF VG_OP_MODE = C_SEARCH.
      LV_DISABLE = ABAP_TRUE.
    ENDIF.

    D_SET:
    'BTN_INSERT_ROW' ICON_INSERT_ROW 0 '' LV_DISABLE, "// insert row
    'BTN_DELETE_ROW' ICON_DELETE_ROW 0 '' LV_DISABLE, "// delete row
    ''               ''              3 '' LV_DISABLE. "// separator

*    IF ( SY-DYNNR = 0110 OR
*         SY-DYNNR = 0100 ).

*      D_SET:
*      'BTN_CLAS_CONTABIL' ICON_DETAIL 0 'Classificações contábeis' LV_DISABLE. "// class contábil
*      'BTN_SERVICOS'      ICON_TOOLS  0 'Serviços' LV_DISABLE.                 "// serviços

*    ENDIF.

    CALL METHOD OBJ_TOOLBAR_MANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "SET_TOOLBAR

  METHOD GET_UCOMM.
    DATA: VL_STATUS_HEADER TYPE C,
          LW_ZMMT0068      TYPE ZMMT0068,
          VL_ITEM          TYPE EVRTP.

    CREATE OBJECT R_UTILS.

    CASE E_UCOMM.
      WHEN 'BTN_INSERT_ROW'.
        CASE SY-DYNNR.
          WHEN  0100.
            CLEAR WL_SAIDA_ITENS.

            WL_SAIDA_ITENS-STATUS   = ICON_LIGHT_OUT.
            WL_SAIDA_ITENS-WERKS    = ICON_TE_COSTS_ASSIGN.

            APPEND LINES OF R_WERKS[] TO WL_SAIDA_ITENS-T_WERKS.
            APPEND WL_SAIDA_ITENS     TO GT_SAIDA_ITENS.

            CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
              EXPORTING
                IS_STABLE = WL_STABLE.

        ENDCASE.

      WHEN 'BTN_DELETE_ROW'.
        CLEAR: GT_SELECTED_ROWS, GT_FIELDS.

        CASE SY-DYNNR.
          WHEN 0100 .
            CALL METHOD OBJ_ALV_0110->GET_SELECTED_ROWS
              IMPORTING
                ET_INDEX_ROWS = GT_SELECTED_ROWS.

            REFRESH GT_FIELDS_STYLE.


            SORT GT_SELECTED_ROWS STABLE DESCENDING.
            LOOP AT GT_SELECTED_ROWS INTO WL_SELECTED_ROWS.
              DELETE GT_SAIDA_ITENS INDEX WL_SELECTED_ROWS-INDEX.
            ENDLOOP.

            CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
              EXPORTING
                IS_STABLE = WL_STABLE.


        ENDCASE.


    ENDCASE.


  ENDMETHOD.                    "GET_UCOMM

  METHOD ON_CLICK.

  ENDMETHOD.                    "ON_CLICK

ENDCLASS.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION

CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM,

      ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING E_MODIFIED ET_GOOD_CELLS,

      ON_BUTTON_CLICK FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
        IMPORTING ES_COL_ID ES_ROW_NO,

      HANDLE_DOUBLE_CLICK FOR EVENT NODE_DOUBLE_CLICK OF CL_GUI_ALV_TREE
        IMPORTING NODE_KEY,

      HANDLE_BUTTON_CLICK FOR EVENT BUTTON_CLICK OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME,

      ON_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD ON_DATA_CHANGED.
    DATA LS_GOOD TYPE LVC_S_MODI.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD.
      CASE SY-DYNNR.
        WHEN 0100.
          READ TABLE GT_SAIDA_ITENS INTO WL_SAIDA_ITENS INDEX LS_GOOD-ROW_ID.

          WL_SAIDA_ITENS-STATUS = ICON_LIGHT_OUT.

          CASE LS_GOOD-FIELDNAME.
            WHEN 'APROVADOR'.
              SELECT SINGLE *
                FROM USR21
                INTO   @DATA(LW_USR21)
                WHERE BNAME = @LS_GOOD-VALUE.

              IF ( SY-SUBRC IS INITIAL ).
                WL_SAIDA_ITENS-APROVADOR = LW_USR21-BNAME.
              ELSE.
                CLEAR WL_SAIDA_ITENS-APROVADOR.
                MESSAGE S836(SD) WITH TEXT-E01 DISPLAY LIKE 'E'.
              ENDIF.

              MODIFY GT_SAIDA_ITENS FROM WL_SAIDA_ITENS INDEX LS_GOOD-ROW_ID
              TRANSPORTING APROVADOR.

            WHEN 'MATNR'.

              SELECT SINGLE *
                FROM MARA
                INTO @DATA(LW_MARA)
               WHERE MATNR = @LS_GOOD-VALUE.

              SELECT SINGLE *
                FROM MAKT
                INTO WL_MAKT
               WHERE MATNR = LS_GOOD-VALUE
                 AND SPRAS = SY-LANGU.

              IF ( SY-SUBRC IS INITIAL ).
                WL_SAIDA_ITENS-MATNR = WL_MAKT-MATNR.
                WL_SAIDA_ITENS-TXZ01 = WL_MAKT-MAKTX.
              ELSE.
                CLEAR :  WL_SAIDA_ITENS-MATNR,
                         WL_SAIDA_ITENS-TXZ01.
                MESSAGE S836(SD) WITH TEXT-E03 DISPLAY LIKE 'E'.
              ENDIF.

              MODIFY GT_SAIDA_ITENS FROM WL_SAIDA_ITENS INDEX LS_GOOD-ROW_ID
              TRANSPORTING MATNR TXZ01.
          ENDCASE.

          CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WL_STABLE.

      ENDCASE.
    ENDLOOP.

    CLEAR: LS_GOOD, ER_DATA_CHANGED->MT_GOOD_CELLS.
  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_DATA_CHANGED_FINISHED.

*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        I_SCREEN   = '100'
*        I_SHOW     = SPACE
*        I_REPID    = SY-REPID
*      IMPORTING
*        E_MESSAGEM = WL_MENSAGEM
*      TABLES
*        IT_MSGS    = GT_MSG_RETURN.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD HANDLE_DOUBLE_CLICK.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD ON_BUTTON_CLICK.
    DATA: LV_DISABLED VALUE ABAP_OFF.
*          MW_WERKS    TYPE WERKS_RANG.

    CREATE OBJECT R_UTILS.
    IF ( ES_COL_ID = 'WERKS' ).
      READ TABLE GT_SAIDA_ITENS ASSIGNING <FS_SAIDA_ITENS> INDEX ES_ROW_NO-ROW_ID.

      IF ( <FS_SAIDA_ITENS>-ESTILO[] IS NOT INITIAL )
      OR ( <FS_SAIDA_ITENS>-STATUS   EQ ICON_GREEN_LIGHT ).
        LV_DISABLED = ABAP_ON.
      ENDIF.

      DATA(_OLD_CENTROS) = WL_SAIDA_ITENS-T_WERKS.
      PERFORM F_SELECTIONS_DIALOG TABLES <FS_SAIDA_ITENS>-T_WERKS
                                   USING SPACE
                                         LV_DISABLED
                                         SPACE.

      CHECK LV_DISABLED = ABAP_FALSE.
      DELETE ADJACENT DUPLICATES FROM <FS_SAIDA_ITENS>-T_WERKS.

      TRY.
          LOOP AT <FS_SAIDA_ITENS>-T_WERKS INTO R_UTILS->MW_WERKS.
            IF ( R_UTILS->MW_WERKS-LOW NOT IN R_WERKS[] ).
              RAISE EXCEPTION TYPE CX_SY_ITAB_LINE_NOT_FOUND.
            ENDIF.
          ENDLOOP.

          IF <FS_SAIDA_ITENS>-T_WERKS <> _OLD_CENTROS.
            <FS_SAIDA_ITENS>-STATUS = ICON_LIGHT_OUT.
          ENDIF.

        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          MESSAGE TEXT-E12 TYPE 'I' DISPLAY LIKE 'E'.
          <FS_SAIDA_ITENS>-T_WERKS = _OLD_CENTROS.
      ENDTRY.
    ENDIF.

    CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WL_STABLE.
  ENDMETHOD.                    "ON_BUTTON_CLICK

  METHOD HANDLE_BUTTON_CLICK.

  ENDMETHOD.                    "handle_item_double_click

  METHOD ON_ONF4.

  ENDMETHOD.                    "ON_ONF4
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

INITIALIZATION.

  CALL SCREEN 0100.
