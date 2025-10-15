*&---------------------------------------------------------------------*
*& Include ZFIR0045_TOP                                      PoolMóds.        ZFIR0045
*&
*&---------------------------------------------------------------------*

PROGRAM  ZFIR0045.
*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

TYPES:
    BEGIN OF TY_ZFIT0066,
        COD_OPER      TYPE ZFIT0066-COD_OPER,
        TP_AJUSTE     TYPE ZFIT0066-TP_AJUSTE,
        BSCHL         TYPE ZFIT0066-BSCHL,
        HKONT         TYPE ZFIT0066-HKONT,
        TXT50         TYPE SKAT-TXT50,
        SHKZG         TYPE TBSL-SHKZG,
        TX_IMP        TYPE ZFIT0066-TX_IMP,
    END OF TY_ZFIT0066,

     BEGIN OF TY_FIELDS,
        CAMPO(30) TYPE C,
        GROUP1(5) TYPE C,
        VALUE     TYPE SY-TABIX,
        INVISIBLE TYPE SY-TABIX,
    END   OF TY_FIELDS.


*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE       TYPE SY-UCOMM,
      WG_CADPAR     TYPE TY_ZFIT0066,

      TG_FIELDS             TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_MSG_RET            TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE,

      TG_SELECTEDCELL TYPE LVC_T_CELL,
      WG_SELECTEDCELL TYPE LVC_S_CELL,
      X_FIELD(30),
      WG_MENSAGEM(30),
      WG_ACAO(30),
      XMODIF(1),

      BEGIN OF TG_ITENS OCCURS 0,
        MARK(1),
        COD_OPER      TYPE ZFIT0066-COD_OPER,
        DESCR         TYPE ZFIT0063-DESCR,
        TP_OPERACAO   TYPE ZFIT0066-TP_OPERACAO,
        TP_AJUSTE     TYPE ZFIT0066-TP_AJUSTE,
        BSCHL         TYPE ZFIT0066-BSCHL,
        HKONT         TYPE ZFIT0066-HKONT,
        TXT50         TYPE SKAT-TXT50,
        SHKZG         TYPE TBSL-SHKZG,
        TX_IMP        TYPE ZFIT0066-TX_IMP,
      END OF TG_ITENS.

DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
      TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

** Criação de tabela dinamica
DATA: T_FIELDCATALOG        TYPE LVC_T_FCAT,
      W_FIELDCATALOG        TYPE LVC_S_FCAT,
      WA_LAYOUT             TYPE LVC_S_LAYO,
      WA_STABLE             TYPE LVC_S_STBL.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: G_CONTAINER          TYPE SCRFNAME VALUE 'CC_PAR',
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      C_ALV_TOOLBARMANAGER  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_0               TYPE C VALUE '0',
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
         C_COL_EXP(7)      TYPE C VALUE 'COL_EXP',
         C_DISPLA(6)       TYPE C VALUE 'DISPLA',
         C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.
***********************************************************************************
*Classes
***********************************************************************************

DATA : TY_TOOLBAR TYPE STB_BUTTON.

*Class definition for ALV toolbar
CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

DATA: OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR.

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
                IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
    ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
               IMPORTING  E_OBJECT,

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

    IF WG_ACAO NE C_MODIF.
      WL_DESACTIVE = 1.
    ENDIF.

    TY_TOOLBAR-ICON      =  ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  =  C_ADD.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.


    TY_TOOLBAR-ICON      =  ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  =  C_DEL.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
*   variable for Toolbar Button
    TY_TOOLBAR-ICON      =  ICON_VIEW_CLOSE.
    TY_TOOLBAR-FUNCTION  =  C_CLOS_MSG.
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
    DATA:  TL_ITENS_AUX LIKE TABLE OF TG_ITENS,
           WL_ITENS LIKE LINE OF TG_ITENS,
           WL_LINES TYPE SY-TABIX.
    REFRESH: TL_ITENS_AUX.



    CASE E_UCOMM.
      WHEN C_ADD.
        TL_ITENS_AUX[] = TG_ITENS[].
        REFRESH: TG_ITENS.
        WL_LINES = 1.
        LOOP AT TL_ITENS_AUX INTO WL_ITENS.
          APPEND WL_ITENS TO TG_ITENS.
          ADD 1 TO WL_LINES.
        ENDLOOP.
        CLEAR: WL_ITENS.
        APPEND WL_ITENS TO TG_ITENS.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
      WHEN C_DEL.
        CALL METHOD GRID1->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.


        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          DELETE TG_ITENS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
        ENDLOOP.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
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

*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD ON_ONF4.
    DATA WG_ITENS  LIKE LINE OF TG_ITENS.

    TYPES:  BEGIN OF TY_FIELD,
              TABNAME     TYPE DD03L-TABNAME,     "Nome da tabela
              FIELDNAME   TYPE DD03L-FIELDNAME,   "Nome de campo
              S(1)        TYPE C,
            END OF TY_FIELD,

            BEGIN OF TY_VALUE,
              TABNAME     TYPE DD03L-TABNAME,     "Nome da tabela
              FIELDNAME   TYPE DD03L-FIELDNAME,   "Nome de campo
              CHAR79(79)  TYPE C,
            END OF TY_VALUE.

    DATA:   BEGIN OF WL_VALUETAB,
                  FIELD(50),
            END OF WL_VALUETAB.

    DATA:   TL_VALUETAB       LIKE TABLE OF WL_VALUETAB,
            TL_FIELD          TYPE TABLE OF TY_FIELD,
            WL_FIELD          TYPE TY_FIELD,
            TL_VALUE          TYPE TABLE OF TY_VALUE,
            WL_VALUE          TYPE TY_VALUE,

            TL_ZFIT0063       TYPE TABLE OF ZFIT0063,
            WL_ZFIT0063       TYPE ZFIT0063,
            TL_TBSL           TYPE TABLE OF TBSL,
            WL_TBSL           TYPE TBSL,
            TL_SKAT           TYPE TABLE OF SKAT,
            WL_SKAT           TYPE SKAT,

            WL_INDEX          TYPE SY-TABIX,
            WL_CHAR(20),
            WL_FIELDNAME(30),
            WL_TABNAME(30),
            V_FLGOPER(1).

    CLEAR: WG_ITENS.
    READ TABLE TG_ITENS INTO WG_ITENS INDEX ES_ROW_NO-ROW_ID.
    CASE E_FIELDNAME.
      WHEN 'TP_OPERACAO'.
        V_FLGOPER = 'X'.
        WL_FIELDNAME  = 'COD_OPER'.
        WL_TABNAME    = 'ZFIT0063'.

        MOVE: ' ' TO WL_VALUETAB-FIELD.
        APPEND WL_VALUETAB TO TL_VALUETAB.
        MOVE: 'Sem operação' TO WL_VALUETAB-FIELD.
        APPEND WL_VALUETAB TO TL_VALUETAB.

        MOVE: 'F' TO WL_VALUETAB-FIELD.
        APPEND WL_VALUETAB TO TL_VALUETAB.
        MOVE: 'Financeiro' TO WL_VALUETAB-FIELD.
        APPEND WL_VALUETAB TO TL_VALUETAB.

        MOVE: 'O' TO WL_VALUETAB-FIELD.
        APPEND WL_VALUETAB TO TL_VALUETAB.
        MOVE: 'Operacional' TO WL_VALUETAB-FIELD.
        APPEND WL_VALUETAB TO TL_VALUETAB.


        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'COD_OPER'.
        WL_FIELD-S = 'X'.
        APPEND WL_FIELD TO TL_FIELD.

        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'DESCR'.
        WL_FIELD-S = ' '.
        APPEND WL_FIELD TO TL_FIELD.

      WHEN 'COD_OPER'.
        WL_FIELDNAME  = 'COD_OPER'.
        WL_TABNAME    = 'ZFIT0063'.

        SELECT * FROM ZFIT0063 INTO TABLE TL_ZFIT0063.

        CHECK TL_ZFIT0063 IS NOT INITIAL.
        SORT TL_ZFIT0063 BY COD_OPER.

        LOOP AT TL_ZFIT0063 INTO WL_ZFIT0063.
          MOVE: WL_ZFIT0063-COD_OPER TO WL_VALUETAB-FIELD.
          APPEND WL_VALUETAB TO TL_VALUETAB.

          MOVE: WL_ZFIT0063-DESCR TO WL_VALUETAB-FIELD.
          APPEND WL_VALUETAB TO TL_VALUETAB.

          CLEAR: WL_ZFIT0063-DESCR, WL_VALUETAB.
        ENDLOOP.

        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'COD_OPER'.
        WL_FIELD-S = 'X'.
        APPEND WL_FIELD TO TL_FIELD.

        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'DESCR'.
        WL_FIELD-S = ' '.
        APPEND WL_FIELD TO TL_FIELD.

      WHEN 'BSCHL'.
        WL_FIELDNAME  = 'BSCHL'.
        WL_TABNAME    = 'TBSL'.

        SELECT * FROM TBSL INTO TABLE TL_TBSL WHERE KOART = 'S' AND BSCHL IN ('40','50').

        CHECK TL_TBSL IS NOT INITIAL.
        SORT TL_TBSL BY BSCHL.

        LOOP AT TL_TBSL INTO WL_TBSL.
          MOVE: WL_TBSL-BSCHL TO WL_VALUETAB-FIELD.
          APPEND WL_VALUETAB TO TL_VALUETAB.

          MOVE: WL_TBSL-SHKZG TO WL_VALUETAB-FIELD.
          APPEND WL_VALUETAB TO TL_VALUETAB.

          CLEAR: WL_TBSL, WL_VALUETAB.
        ENDLOOP.

        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'BSCHL'.
        WL_FIELD-S = 'X'.
        APPEND WL_FIELD TO TL_FIELD.

        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'SHKZG'.
        WL_FIELD-S = ' '.
        APPEND WL_FIELD TO TL_FIELD.

      WHEN 'HKONT'.
        CHECK WG_ITENS-BSCHL IS NOT INITIAL.

        SELECT SINGLE * FROM TBSL
          INTO WL_TBSL
        WHERE BSCHL EQ WG_ITENS-BSCHL.

        CHECK WL_TBSL IS NOT INITIAL.
        CASE WL_TBSL-KOART.
          WHEN 'S'.
            WL_FIELDNAME  = 'SAKNR'.
            WL_TABNAME    = 'SKAT'.

            SELECT * FROM SKAT INTO TABLE TL_SKAT WHERE SPRAS EQ SY-LANGU AND KTOPL EQ '0050'.

            CHECK TL_SKAT IS NOT INITIAL.
            SORT TL_SKAT BY SAKNR.

            LOOP AT TL_SKAT INTO WL_SKAT.
              MOVE: WL_SKAT-SAKNR TO WL_VALUETAB-FIELD.
              APPEND WL_VALUETAB TO TL_VALUETAB.

              MOVE: WL_SKAT-TXT50 TO WL_VALUETAB-FIELD.
              APPEND WL_VALUETAB TO TL_VALUETAB.

              CLEAR: WL_SKAT, WL_VALUETAB.
            ENDLOOP.

            WL_FIELD-TABNAME = WL_TABNAME.
            WL_FIELD-FIELDNAME = 'SAKNR'.
            WL_FIELD-S = 'X'.
            APPEND WL_FIELD TO TL_FIELD.

            WL_FIELD-TABNAME = WL_TABNAME.
            WL_FIELD-FIELDNAME = 'TXT50'.
            WL_FIELD-S = ' '.
            APPEND WL_FIELD TO TL_FIELD.

        ENDCASE.

    ENDCASE.

    IF    WL_FIELDNAME  IS NOT INITIAL
      AND WL_TABNAME    IS NOT INITIAL
      AND TL_FIELD[]    IS NOT INITIAL
      AND TL_VALUETAB[] IS NOT INITIAL.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
*        cucol                     = '3'
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

      IF SY-SUBRC IS INITIAL.
        CASE E_FIELDNAME.
          WHEN 'COD_OPER'.
            IF V_FLGOPER NE 'X'.
              READ TABLE TL_ZFIT0063 INTO WL_ZFIT0063 INDEX WL_INDEX.
            ENDIF.
          WHEN 'BSCHL'.
            READ TABLE TL_TBSL INTO WL_TBSL INDEX WL_INDEX.
          WHEN 'HKONT'.
            CASE WL_TBSL-KOART.
              WHEN 'S'.
                READ TABLE TL_SKAT INTO WL_SKAT INDEX WL_INDEX.
            ENDCASE.
        ENDCASE.

        IF ES_ROW_NO-ROW_ID GT 0.
          READ TABLE TG_ITENS INTO WG_ITENS INDEX ES_ROW_NO-ROW_ID.
          IF SY-SUBRC IS INITIAL.
            CASE E_FIELDNAME.
              WHEN 'TP_OPERACAO'.
                IF WL_INDEX = 2.
                  MOVE 'F' TO WG_ITENS-TP_OPERACAO.
                ELSEIF WL_INDEX = 3.
                  MOVE 'O' TO WG_ITENS-TP_OPERACAO.
                ELSE.
                  CLEAR WG_ITENS-TP_OPERACAO.
                ENDIF.
              WHEN 'COD_OPER'.
                MOVE: WL_ZFIT0063-COD_OPER TO WG_ITENS-COD_OPER,
                      WL_ZFIT0063-DESCR    TO WG_ITENS-DESCR.
              WHEN 'BSCHL'.
                MOVE: WL_TBSL-BSCHL TO WG_ITENS-BSCHL.
                IF WL_TBSL-SHKZG = 'S'.
                  MOVE 'D' TO WG_ITENS-SHKZG.
                ELSE.
                  MOVE 'C' TO WG_ITENS-SHKZG.
                ENDIF.
              WHEN 'HKONT'.
                CASE WL_TBSL-KOART.
                  WHEN 'S'.
                    MOVE: WL_SKAT-SAKNR TO WG_ITENS-HKONT,
                          WL_SKAT-TXT50 TO WG_ITENS-TXT50.
                ENDCASE.
            ENDCASE.

            MODIFY TG_ITENS FROM WG_ITENS INDEX ES_ROW_NO-ROW_ID.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: WG_ITENS.

***** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "ON_ONF4

  METHOD ON_DATA_CHANGED.

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_VALUE TYPE LVC_VALUE,

          WL_ZFIT0063  TYPE ZFIT0063,
          WL_SKAT      TYPE SKAT,
          WL_TBSL      TYPE TBSL.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'TP_AJUSTE'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

*** PBI - 73761 - Inicio - CBRAND
*      CASE LV_VALUE+0(1).
*        WHEN 'A'.
*          LV_VALUE = 'ATIVO'.
*        WHEN 'P'.
*          LV_VALUE = 'PASSIVO'.
*        WHEN OTHERS.
*          LV_VALUE = ''.
*      ENDCASE.
*** PBI - 73761 - Fim - CBRAND

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'TP_AJUSTE'
          I_VALUE     = LV_VALUE.


    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'BSCHL'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE * FROM TBSL
        INTO WL_TBSL
        WHERE KOART = 'S'
        AND   BSCHL = LV_VALUE.

      IF SY-SUBRC = 0.
        IF WL_TBSL-SHKZG = 'S'.
          LV_VALUE = 'D'.
        ELSE.
          LV_VALUE = 'C'.
        ENDIF.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'SHKZG'
            I_VALUE     = LV_VALUE.
      ENDIF.

    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'HKONT'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE *
        FROM SKAT
        INTO WL_SKAT
        WHERE SPRAS EQ SY-LANGU AND KTOPL EQ '0050'
        AND   SAKNR = LV_VALUE.

      IF SY-SUBRC = 0.
        LV_VALUE = WL_SKAT-TXT50.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'TXT50'
            I_VALUE     = LV_VALUE.
      ENDIF.

    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                                INTO LS_GOOD
                                WHERE FIELDNAME = 'COD_OPER'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE *
        FROM ZFIT0063
        INTO WL_ZFIT0063
        WHERE COD_OPER = LV_VALUE.

      IF SY-SUBRC = 0.
        LV_VALUE = WL_ZFIT0063-DESCR.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'DESCR'
            I_VALUE     = LV_VALUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_DATA_CHANGED_FINISHED.

**** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    PERFORM VERIFICA_ERROS.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_PRESSED_TAB = ''
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.

  ENDMETHOD.                    "on_data_changed_finisheD


ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
