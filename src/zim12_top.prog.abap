*&---------------------------------------------------------------------*
*&  Include           ZIM12_TOP
*&---------------------------------------------------------------------*
PROGRAM ZIM12.

INCLUDE <CL_ALV_CONTROL>.


*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TS_100'
CONSTANTS: BEGIN OF C_TS_100,
             TAB1 LIKE SY-UCOMM VALUE 'TS_100_FC1',
             TAB2 LIKE SY-UCOMM VALUE 'TS_100_FC2',
           END OF C_TS_100.
*&SPWIZARD: DATA FOR TABSTRIP 'TS_100'
CONTROLS:  TS_100 TYPE TABSTRIP.

DATA:      BEGIN OF G_TS_100,
             SUBSCREEN   LIKE SY-DYNNR,
             PROG        LIKE SY-REPID VALUE 'ZIM12',
             PRESSED_TAB LIKE SY-UCOMM VALUE C_TS_100-TAB1,
           END OF G_TS_100.


**&--------------------------------------------------------------------&*
**& Estruturas                                                         &*
**&--------------------------------------------------------------------&*
TYPES:  BEGIN OF TY_AVAL.
        INCLUDE STRUCTURE ZIM12_AVAL.
TYPES: MARK           TYPE C,
       LINHA          TYPE I,
       MODI           TYPE C,
       STYLE          TYPE LVC_T_STYL,
       STATUS(4),
       DESC_USNAM(40),
       END OF TY_AVAL.

TYPES:  BEGIN OF TY_APROV.
        INCLUDE STRUCTURE ZIM12_APROV.
TYPES: MARK           TYPE C,
       LINHA          TYPE I,
       MODI           TYPE C,
       STYLE          TYPE LVC_T_STYL,
       STATUS(4),
       DESC_USNAM(40),
       END OF TY_APROV.

TYPES:    BEGIN OF TY_FIELDS,
            CAMPO(30) TYPE C,
            GROUP1(5) TYPE C,
            VALUE     TYPE SY-TABIX,
            INVISIBLE TYPE SY-TABIX,
          END OF TY_FIELDS,

          BEGIN OF TY_USREFUS,
            BNAME     TYPE USREFUS-BNAME,
            USERALIAS TYPE USREFUS-USERALIAS,
          END OF TY_USREFUS.
*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*

DATA: OK-CODE         LIKE SY-UCOMM,
      OK_CODE         LIKE SY-UCOMM,
      WG_MENSAGEM(30),
      X_FIELD(30),
      WG_ACAO(30).

** Criação de tabela dinamica
DATA: T_FIELDCATALOG TYPE LVC_T_FCAT,
      W_FIELDCATALOG TYPE LVC_S_FCAT,
      WA_LAYOUT      TYPE LVC_S_LAYO,
      WA_STABLE      TYPE LVC_S_STBL,

      TG_FIELDS      TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_MSG_RET     TYPE TABLE OF ZFIWRS0002  WITH HEADER LINE.

DATA: TG_AVAL       TYPE TABLE OF TY_AVAL,
      TG_AVAL_AUX   TYPE TABLE OF TY_AVAL,
      WG_AVAL       TYPE TY_AVAL,
      WG_AVAL_AUX   TYPE TY_AVAL,

      TG_APROV      TYPE TABLE OF TY_APROV,
      TG_APROV_AUX  TYPE TABLE OF TY_APROV,
      WG_APROV_AUX  TYPE TY_APROV,
      WG_APROV      TYPE TY_APROV,
      TL_INDEX_ROWS TYPE LVC_T_ROW,
      WL_INDEX_ROWS TYPE LVC_S_ROW.

*Class definition for ALV toolbar
CLASS:  LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: G_CONTAINER          TYPE SCRFNAME VALUE 'CC_AVAL',
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      "
      G_CONTAINER2         TYPE SCRFNAME VALUE 'CC_APROVA',
      G_CUSTOM_CONTAINER2  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_12         TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_22         TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER2            TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID2                TYPE REF TO CL_GUI_ALV_GRID,
      "
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      OBG_TOOLBAR2         TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,

      OBG_DOCKING          TYPE REF TO CL_GUI_DOCKING_CONTAINER,

      WA_STYLE             TYPE LVC_S_STYL,
      STYLE                TYPE LVC_T_STYL   WITH HEADER LINE,

      GS_VARIANT_C         TYPE DISVARIANT.

*Declaration for toolbar buttons
DATA: TY_TOOLBAR TYPE STB_BUTTON.

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

*    IF WG_ACAO NE C_MODIF AND WG_ACAO NE C_ADD.
*      WL_DESACTIVE = 1.
*    ENDIF.
    WG_ACAO = C_ADD.  "c_modif.

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

    TY_TOOLBAR-ICON      = ICON_COPY_OBJECT.
    TY_TOOLBAR-FUNCTION  = 'COPY'.
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
    DATA: TL_ZIM12_AVAL_AUX TYPE TABLE OF  TY_AVAL,
          TL_ZIM12_AVAL_COP TYPE TABLE OF  TY_AVAL,
          WL_ZIM12_AVAL     TYPE TY_AVAL,
          WL_LINES          TYPE SY-TABIX,
          WL_LINES2         TYPE SY-TABIX.

    DATA: TL_ZIM12_APROV_AUX TYPE TABLE OF TY_APROV,
          TL_ZIM12_APROV_COP TYPE TABLE OF  TY_APROV,
          WL_ZIM12_APROV     TYPE TY_APROV.

    REFRESH: TL_ZIM12_AVAL_AUX, TL_ZIM12_APROV_AUX.
    WG_ACAO = C_ADD.  "c_modif.

    CASE E_UCOMM.
      WHEN 'COPY'.
        IF G_TS_100-SUBSCREEN = '0101'.
          CALL METHOD GRID1->GET_SELECTED_ROWS
            IMPORTING
              ET_INDEX_ROWS = TL_INDEX_ROWS.

          LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
            WL_LINES = WL_INDEX_ROWS-INDEX.
          ENDLOOP.
          WL_LINES2 = LINES( TG_AVAL ).
          TL_ZIM12_AVAL_AUX[] = TG_AVAL[].
          REFRESH: TL_ZIM12_AVAL_COP, TG_AVAL.
          LOOP AT TL_ZIM12_AVAL_AUX INTO WL_ZIM12_AVAL.
            READ TABLE TL_INDEX_ROWS INTO WL_INDEX_ROWS
                        WITH KEY INDEX = SY-TABIX.
            IF SY-SUBRC = 0.
              WL_ZIM12_AVAL-MODI = 'X'.
              REFRESH WL_ZIM12_AVAL-STYLE.
*              ADD 1 TO WL_LINES2.
*              WL_ZIM12_AVAL-LINHA = WL_LINES2.
              APPEND WL_ZIM12_AVAL TO TL_ZIM12_AVAL_COP.
            ENDIF.
          ENDLOOP.

          LOOP AT TL_ZIM12_AVAL_AUX INTO WL_ZIM12_AVAL.
            APPEND WL_ZIM12_AVAL TO TG_AVAL.
            IF SY-TABIX = WL_LINES.
              LOOP AT TL_ZIM12_AVAL_COP INTO WL_ZIM12_AVAL.
                REFRESH WL_ZIM12_AVAL-STYLE.
                ADD 1 TO WL_LINES2.
                WL_ZIM12_AVAL-LINHA = WL_LINES2.
                APPEND WL_ZIM12_AVAL TO TG_AVAL.
              ENDLOOP.
            ENDIF.
          ENDLOOP.

          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        ELSE.
          CALL METHOD GRID2->GET_SELECTED_ROWS
            IMPORTING
              ET_INDEX_ROWS = TL_INDEX_ROWS.

          LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
            WL_LINES = WL_INDEX_ROWS-INDEX.
          ENDLOOP.

          WL_LINES2 = LINES( TG_APROV ).
          TL_ZIM12_APROV_AUX[] = TG_APROV[].
          REFRESH: TL_ZIM12_APROV_COP, TG_APROV.
          LOOP AT TL_ZIM12_APROV_AUX INTO WL_ZIM12_APROV.
            READ TABLE TL_INDEX_ROWS INTO WL_INDEX_ROWS
                        WITH KEY INDEX = SY-TABIX.
            IF SY-SUBRC = 0.
              WL_ZIM12_APROV-MODI = 'X'.
              REFRESH WL_ZIM12_APROV-STYLE.
*              ADD 1 TO WL_LINES2.
*              WL_ZIM12_APROV-LINHA = WL_LINES2.
              APPEND WL_ZIM12_APROV TO TL_ZIM12_APROV_COP.
            ENDIF.
          ENDLOOP.

          LOOP AT TL_ZIM12_APROV_AUX INTO WL_ZIM12_APROV.
            APPEND WL_ZIM12_APROV TO TG_APROV.
            IF SY-TABIX = WL_LINES.
              LOOP AT TL_ZIM12_APROV_COP INTO WL_ZIM12_APROV.
                REFRESH WL_ZIM12_APROV-STYLE.
                ADD 1 TO WL_LINES2.
                WL_ZIM12_APROV-LINHA = WL_LINES2.
                APPEND WL_ZIM12_APROV TO TG_APROV.
              ENDLOOP.
            ENDIF.
          ENDLOOP.

          CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        ENDIF.
      WHEN C_ADD.
        IF G_TS_100-SUBSCREEN = '0101'.
          CALL METHOD GRID1->GET_SELECTED_ROWS
            IMPORTING
              ET_INDEX_ROWS = TL_INDEX_ROWS.
          CLEAR WL_INDEX_ROWS.
          IF TL_INDEX_ROWS[] IS NOT INITIAL.
            READ TABLE TL_INDEX_ROWS INTO WL_INDEX_ROWS INDEX 1.
          ENDIF.
          WL_LINES = LINES( TG_AVAL ).
          TL_ZIM12_AVAL_AUX[] = TG_AVAL[].
          REFRESH: TG_AVAL.
          LOOP AT TL_ZIM12_AVAL_AUX INTO WL_ZIM12_AVAL.
            APPEND WL_ZIM12_AVAL TO TG_AVAL.
            IF SY-TABIX = WL_INDEX_ROWS-INDEX.
              CLEAR: WL_ZIM12_AVAL.
              WL_ZIM12_AVAL-MODI = 'X'.
              ADD 1 TO WL_LINES.
              WL_ZIM12_AVAL-LINHA = WL_LINES.
              APPEND WL_ZIM12_AVAL TO TG_AVAL.
            ENDIF.
          ENDLOOP.
          IF WL_INDEX_ROWS-INDEX = 0.
            CLEAR: WL_ZIM12_AVAL.
            WL_ZIM12_AVAL-LINHA = WL_LINES + 1.
            APPEND WL_ZIM12_AVAL TO TG_AVAL.
          ENDIF.

          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        ELSE.
          CALL METHOD GRID2->GET_SELECTED_ROWS
            IMPORTING
              ET_INDEX_ROWS = TL_INDEX_ROWS.
          CLEAR WL_INDEX_ROWS.
          IF TL_INDEX_ROWS[] IS NOT INITIAL.
            READ TABLE TL_INDEX_ROWS INTO WL_INDEX_ROWS INDEX 1.
          ENDIF.

          WL_LINES = LINES( TG_APROV ).
          TL_ZIM12_APROV_AUX[] = TG_APROV[].
          REFRESH: TG_APROV.
          LOOP AT TL_ZIM12_APROV_AUX INTO WL_ZIM12_APROV.
            APPEND WL_ZIM12_APROV TO TG_APROV.
            IF SY-TABIX = WL_INDEX_ROWS-INDEX.
              CLEAR: WL_ZIM12_APROV.
              WL_ZIM12_APROV-MODI = 'X'.
              ADD 1 TO WL_LINES.
              WL_ZIM12_APROV-LINHA = WL_LINES.
              APPEND WL_ZIM12_APROV TO TG_APROV.
            ENDIF.
          ENDLOOP.
          IF WL_INDEX_ROWS-INDEX = 0.
            CLEAR: WL_ZIM12_APROV.
            WL_ZIM12_APROV-LINHA = WL_LINES + 1.
            APPEND WL_ZIM12_APROV TO TG_APROV.
          ENDIF.

          CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        ENDIF.
      WHEN C_DEL.
        IF G_TS_100-SUBSCREEN = '0101'.
          CALL METHOD GRID1->GET_SELECTED_ROWS
            IMPORTING
              ET_INDEX_ROWS = TL_INDEX_ROWS.

          LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
            READ TABLE TG_AVAL INTO WG_AVAL INDEX WL_INDEX_ROWS-INDEX.
            IF  WG_AVAL-MODI   IS NOT INITIAL.
              DELETE TG_AVAL INDEX WL_INDEX_ROWS-INDEX.
            ELSE.
              WG_AVAL-MODI   = 'D'.
              WG_AVAL-STATUS = ICON_DELETE.
              MODIFY TG_AVAL FROM WG_AVAL INDEX WL_INDEX_ROWS-INDEX TRANSPORTING MODI STATUS.
            ENDIF.
          ENDLOOP.

          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        ELSE.
          CALL METHOD GRID2->GET_SELECTED_ROWS
            IMPORTING
              ET_INDEX_ROWS = TL_INDEX_ROWS.

          LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
            READ TABLE TG_APROV INTO WG_APROV INDEX WL_INDEX_ROWS-INDEX.
            IF WG_APROV-MODI IS NOT INITIAL.
              DELETE TG_APROV INDEX WL_INDEX_ROWS-INDEX.
            ELSE.
              WG_APROV-MODI   = 'D'.
              WG_APROV-STATUS = ICON_DELETE.
              MODIFY TG_APROV FROM WG_APROV INDEX WL_INDEX_ROWS-INDEX TRANSPORTING MODI STATUS.
            ENDIF.
          ENDLOOP.

          CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD ON_DOUBLE_CLICK.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD ON_DATA_CHANGED.

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,

          WL_USR21 TYPE USR21,
          WL_ADRP  TYPE ADRP.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                                INTO LS_GOOD
                                WHERE FIELDNAME = 'APROVADOR'
                                OR FIELDNAME =  'GPO_CMP'
                                OR FIELDNAME =  'NIVEL'
                                OR FIELDNAME =  'BUKRS'
                                OR FIELDNAME =  'KOSTL'
                                OR FIELDNAME =  'FASE'.
      IF LS_GOOD-FIELDNAME =  'APROVADOR'.
        READ TABLE TG_AVAL INTO WG_AVAL INDEX LS_GOOD-ROW_ID.
        LV_VALUE = LS_GOOD-VALUE.
        CONDENSE LV_VALUE NO-GAPS.
*
        SELECT SINGLE *
           FROM USR21 INTO WL_USR21
          WHERE BNAME EQ LV_VALUE.

        IF SY-SUBRC = 0.
          SELECT SINGLE * FROM ADRP INTO  WL_ADRP
            WHERE PERSNUMBER = WL_USR21-PERSNUMBER.
          LV_VALUE = WL_ADRP-NAME_FIRST && '&' && WL_ADRP-NAME_LAST.
          REPLACE '&' WITH SPACE INTO LV_VALUE.
        ENDIF.

        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'DESC_USNAM'
            I_VALUE     = LV_VALUE.
      ENDIF.
      LV_VALUE = 'X'.
      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'MODI'
          I_VALUE     = LV_VALUE.


      CLEAR: WL_ADRP, LV_VALUE.

    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_DATA_CHANGED_FINISHED.

  ENDMETHOD.                    "on_data_changed_finished

  METHOD ON_ONF4.

    TYPES:    BEGIN OF TY_VALUE,
                TABNAME    TYPE DD03L-TABNAME,     "Nome da tabela
                FIELDNAME  TYPE DD03L-FIELDNAME,   "Nome de campo
                CHAR79(79) TYPE C,
              END OF TY_VALUE,

              BEGIN OF TY_FIELD,
                TABNAME   TYPE DD03L-TABNAME,     "Nome da tabela
                FIELDNAME TYPE DD03L-FIELDNAME,   "Nome de campo
                S(1)      TYPE C,
              END OF TY_FIELD.

    DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL,
          WL_RETURN_TAB TYPE DDSHRETVAL,
          TL_DSELC      TYPE TABLE OF DSELC.

    DATA:   BEGIN OF WL_VALUETAB,
              FIELD(50),
            END OF WL_VALUETAB.

    DATA: WL_INDEX         TYPE SY-TABIX,
          WL_FIELDNAME(30),
          WL_TABNAME(30),
          WL_CHAR(20),
          TL_FIELD         TYPE TABLE OF TY_FIELD,
          TL_VALUE         TYPE TABLE OF TY_VALUE,
          TL_VALUETAB      LIKE TABLE OF WL_VALUETAB,

          WL_VALUE         TYPE TY_VALUE,
          WL_FIELD         TYPE TY_FIELD.

    TYPES: BEGIN OF TY_FASE ,
             FASE        TYPE ZIM01_SOL_AP_INV-FASE,
             OBSERVACOES TYPE ZIM01_SOL_AP_INV-OBSERVACOES,
           END OF TY_FASE.

    DATA: TL_FASE TYPE TABLE OF TY_FASE,
          WL_FASE TYPE TY_FASE.
    IF E_FIELDNAME = 'FASE'.
      WL_FASE-FASE = '01'.
      WL_FASE-OBSERVACOES = 'Planejado'.
      APPEND WL_FASE TO TL_FASE.
      WL_FASE-FASE = '02'.
      WL_FASE-OBSERVACOES = 'Extra'.
      APPEND WL_FASE TO TL_FASE.
      WL_FASE-FASE = '03'.
      WL_FASE-OBSERVACOES = 'Complemento'.
      APPEND WL_FASE TO TL_FASE.
      WL_FASE-FASE = '99'.
      WL_FASE-OBSERVACOES = 'Todas'.
      APPEND WL_FASE TO TL_FASE.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'FASE'
          DYNPPROG        = SY-REPID
          DYNPNR          = SY-DYNNR
          DYNPROFIELD     = 'TG_APROV-FASE'
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = TL_FASE
          RETURN_TAB      = TL_RETURN_TAB
          DYNPFLD_MAPPING = TL_DSELC.

      READ TABLE TG_APROV INTO  WG_APROV INDEX ES_ROW_NO-ROW_ID.
      IF WG_APROV-STYLE[] IS INITIAL.
        READ TABLE TL_RETURN_TAB INTO WL_RETURN_TAB INDEX 1.
        WG_APROV-FASE = WL_RETURN_TAB-FIELDVAL.
        MODIFY  TG_APROV FROM  WG_APROV INDEX ES_ROW_NO-ROW_ID.
        CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
      ENDIF.

    ENDIF.
    "
    TYPES: BEGIN OF TY_GPO ,
             COD_GPO     TYPE ZIM010-COD_GPO,
             DESCR_GRUPO TYPE ZIM010-DESCR_GRUPO,
           END OF TY_GPO.

    DATA: TL_GPO TYPE TABLE OF TY_GPO,
          WL_GPO TYPE TY_GPO.
    IF E_FIELDNAME = 'GPO_CMP'.
      SELECT COD_GPO DESCR_GRUPO
        FROM ZIM010
        INTO TABLE TL_GPO
         ORDER BY COD_GPO.

      CLEAR  WL_INDEX.
      WL_FIELDNAME  = 'COD_GPO'.
      WL_TABNAME    = 'ZIM010'.

      LOOP AT TL_GPO INTO WL_GPO.
        WL_INDEX = SY-TABIX.
        MOVE: WL_GPO-COD_GPO TO WL_VALUETAB-FIELD.
        APPEND WL_VALUETAB   TO TL_VALUETAB.
        CLEAR:  WL_VALUETAB.

        MOVE: WL_GPO-DESCR_GRUPO  TO WL_VALUETAB-FIELD.
        APPEND WL_VALUETAB   TO TL_VALUETAB.
        CLEAR:  WL_VALUETAB.

      ENDLOOP.


      WL_FIELD-TABNAME = WL_TABNAME.
      WL_FIELD-FIELDNAME = 'COD_GPO'.
      WL_FIELD-S = 'X'.
      APPEND WL_FIELD TO TL_FIELD.

      WL_FIELD-TABNAME = WL_TABNAME.
      WL_FIELD-FIELDNAME = 'DESCR_GRUPO'.
      WL_FIELD-S = ' '.
      APPEND WL_FIELD TO TL_FIELD.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
*         cucol                     = '3'
          FIELDNAME                 = WL_FIELDNAME
          TABNAME                   = WL_TABNAME
        IMPORTING
          INDEX                     = WL_INDEX
          SELECT_VALUE              = WL_CHAR
        TABLES
          FIELDS                    = TL_FIELD
          SELECT_VALUES             = TL_VALUE
          VALUETAB                  = TL_VALUETAB
        EXCEPTIONS
          FIELD_NOT_IN_DDIC         = 001
          MORE_THEN_ONE_SELECTFIELD = 002
          NO_SELECTFIELD            = 003.

      IF SY-SUBRC IS INITIAL AND WL_INDEX GT 0.
        READ TABLE TG_AVAL INTO  WG_AVAL INDEX ES_ROW_NO-ROW_ID.
        IF WG_AVAL-STYLE[] IS INITIAL.
          READ TABLE TL_GPO INTO WL_GPO INDEX WL_INDEX.
          WG_AVAL-GPO_CMP = WL_GPO-COD_GPO.
          MODIFY  TG_AVAL FROM  WG_AVAL INDEX ES_ROW_NO-ROW_ID.
          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXPORTING
              IS_STABLE = WA_STABLE.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.                    "ON_ONF4

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
