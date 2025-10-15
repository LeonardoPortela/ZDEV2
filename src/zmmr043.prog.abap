*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 28/10/2013                                              &*
*& Descrição: Cadastro de Regionais/Filiais                           &*
*& Transação: ZMM0063                                                 &*
*---------------------------------------------------------------------&*

REPORT  ZMMR043.


*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

TYPES: BEGIN OF TY_CADLAN,
        REGIONAL        TYPE ZMMT0045-REGIONAL,
     END OF TY_CADLAN,

      BEGIN OF TY_FIELDS,
        CAMPO(30) TYPE C,
        GROUP1(5) TYPE C,
        VALUE     TYPE SY-TABIX,
        INVISIBLE TYPE SY-TABIX,
    END   OF TY_FIELDS.


*&--------------------------------------------------------------------&*
*& Declaração de  Variáveis
*&--------------------------------------------------------------------&*
DATA: OK-CODE         TYPE SY-UCOMM,
      WG_CADLAN       TYPE TY_CADLAN,
      TG_SELECTEDCELL TYPE LVC_T_CELL,
      WG_SELECTEDCELL TYPE LVC_S_CELL,
      WG_MENSAGEM(30),
      X_FIELD(30),
      WG_ACAO(30),
      W_ANSWER(1).

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA:
  BEGIN OF TG_ITENS OCCURS 0,
        MARK(1),
        WERKS           TYPE T001W-WERKS,
        NAME1           TYPE T001W-NAME1,
        CAP_CORREIAS    TYPE ZMMT0045-CAP_CORREIAS,
        TP_PADRAO       TYPE ZMMT0045-TP_PADRAO,
      END OF TG_ITENS.

DATA: IT_ZMMT0045    TYPE TABLE OF ZMMT0045,
      WA_ZMMT0045    TYPE ZMMT0045,

      IT_T001W       TYPE TABLE OF T001W,
      WA_T001W       TYPE T001W.


*Class definition for ALV toolbar
CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: G_CONTAINER          TYPE SCRFNAME VALUE 'CC_FILIAL',
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBG_CONTEINER_ERR    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      G_CC_ERR             TYPE SCRFNAME VALUE 'CC_ERR',
      G_CUSTOM_CONT_DESC   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBG_DESCBOX          TYPE REF TO CL_GUI_TEXTEDIT,
      OBG_DOCKING          TYPE REF TO CL_GUI_DOCKING_CONTAINER,

      WA_STYLE             TYPE LVC_S_STYL,
      STYLE TYPE LVC_T_STYL  WITH HEADER LINE,
      STYLE2 TYPE LVC_T_STYL WITH HEADER LINE.



** Criação de tabela dinamica
DATA: T_FIELDCATALOG        TYPE LVC_T_FCAT,
      W_FIELDCATALOG        TYPE LVC_S_FCAT,
      WA_LAYOUT             TYPE LVC_S_LAYO,
      WA_STABLE             TYPE LVC_S_STBL,
      TG_FIELDS             TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      TG_MSG_RET            TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE.

*Declaration for toolbar buttons
DATA : TY_TOOLBAR TYPE STB_BUTTON.
*** TREE DE MENSAGENS.
DATA NODE_ITAB LIKE NODE_STR OCCURS 0.
DATA NODE LIKE NODE_STR.

DATA CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA SPLITTER_MSG TYPE REF TO CL_GUI_EASY_SPLITTER_CONTAINER.
DATA RIGHT TYPE REF TO CL_GUI_CONTAINER.
DATA LEFT  TYPE REF TO CL_GUI_CONTAINER.

DATA EDITOR TYPE REF TO CL_GUI_TEXTEDIT.
DATA TREE TYPE REF TO CL_GUI_SIMPLE_TREE.

DATA BEHAVIOUR_LEFT TYPE REF TO CL_DRAGDROP.
DATA BEHAVIOUR_RIGHT TYPE REF TO CL_DRAGDROP.

DATA HANDLE_TREE TYPE I.
DATA NUM_ROW TYPE I VALUE 0.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_0             TYPE C VALUE '0',
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
         C_REINICIA(8)     TYPE C VALUE 'REINICIA',
         C_DELDOC(6)       TYPE C VALUE 'DELDOC',
         C_DISPLA(6)       TYPE C VALUE 'DISPLA',
         C_DCLICK(6)       TYPE C VALUE 'DCLICK',
         C_SEARCH(6)       TYPE C VALUE 'SEARCH',
         C_ATUALI(6)       TYPE C VALUE 'ATUALI',
         C_ADD_MSG(7)      TYPE C VALUE 'ADD_MSG',
         C_DEL_MSG(7)      TYPE C VALUE 'DEL_MSG',
         C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG',
         C_SAVE_MSG(8)     TYPE C VALUE 'SAVE_MSG',
         C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
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

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD ON_ONF4.
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

            TL_T001W          TYPE TABLE OF T001W,
            WL_T001W          TYPE          T001W,

            WG_ITENS          LIKE LINE OF TG_ITENS,

            WL_INDEX          TYPE SY-TABIX,
            WL_CHAR(20),
            WL_FIELDNAME(30),
            WL_TABNAME(30).

    READ TABLE TG_ITENS INTO WG_ITENS INDEX ES_ROW_NO-ROW_ID.
    CASE E_FIELDNAME.

      WHEN 'WERKS'.
        SELECT * FROM T001W INTO TABLE TL_T001W.

        CHECK TL_T001W IS NOT INITIAL.

        WL_FIELDNAME  = 'WERKS'.
        WL_TABNAME    = 'T001W'.

        LOOP AT TL_T001W INTO WL_T001W.
          MOVE: WL_T001W-WERKS TO WL_VALUETAB-FIELD.
          APPEND WL_VALUETAB TO TL_VALUETAB.

          MOVE: WL_T001W-NAME1 TO WL_VALUETAB-FIELD.
          APPEND WL_VALUETAB TO TL_VALUETAB.

          CLEAR: WL_T001W, WL_VALUETAB.
        ENDLOOP.

        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'WERKS'.
        WL_FIELD-S = 'X'.
        APPEND WL_FIELD TO TL_FIELD.

        WL_FIELD-TABNAME = WL_TABNAME.
        WL_FIELD-FIELDNAME = 'NAME1'.
        WL_FIELD-S = ' '.
        APPEND WL_FIELD TO TL_FIELD.
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

          WHEN 'WERKS'.
            READ TABLE TL_T001W INTO WL_T001W INDEX WL_INDEX.
        ENDCASE.

        IF ES_ROW_NO-ROW_ID GT 0.
          READ TABLE TG_ITENS INTO WG_ITENS INDEX ES_ROW_NO-ROW_ID.
          IF SY-SUBRC IS INITIAL.
            CASE E_FIELDNAME.
              WHEN 'WERKS'.
                MOVE: WL_T001W-NAME1 TO WG_ITENS-NAME1,
                      WL_T001W-WERKS TO WG_ITENS-WERKS.

            ENDCASE.

            MODIFY TG_ITENS FROM WG_ITENS INDEX ES_ROW_NO-ROW_ID.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

**** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "ON_ONF4

  METHOD ON_DATA_CHANGED.

    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_VALUE TYPE LVC_VALUE,
          WL_T001W TYPE T001W.


    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'WERKS'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE *
        FROM T001W
        INTO WL_T001W
          WHERE WERKS EQ LV_VALUE.

      IF SY-SUBRC IS INITIAL.
        MOVE WL_T001W-NAME1 TO LV_VALUE.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'NAME1'
            I_VALUE     = LV_VALUE.
      ELSE.
        CLEAR: LV_VALUE.
        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
          EXPORTING
            I_ROW_ID    = LS_GOOD-ROW_ID
            I_FIELDNAME = 'WERKS'
            I_VALUE     = LV_VALUE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Centro não foi encontrado!'.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.                    "ON_DATA_CHANGED
*  METHOD on_data_changed4.
*
*  ENDMETHOD.                    "ON_DATA_CHANGED4
  METHOD ON_DATA_CHANGED_FINISHED.

*** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    "PERFORM VERIFICA_ERROS.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.

  ENDMETHOD.                    "on_data_changed_finisheD

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

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
*    IF WG_ACAO NE C_MODIF.
*      WL_DESACTIVE = 1.
*    ENDIF.

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
    DATA: TL_ITENS_AUX LIKE TABLE OF TG_ITENS,
        WL_ITENS LIKE LINE OF TG_ITENS,
        WL_LINES TYPE SY-TABIX.
    REFRESH: TL_ITENS_AUX.

    CASE E_UCOMM.
      WHEN C_ADD.
        TL_ITENS_AUX[] = TG_ITENS[].
        REFRESH: TG_ITENS.
        LOOP AT TL_ITENS_AUX INTO WL_ITENS.
          APPEND WL_ITENS TO TG_ITENS.
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

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.
  SET PF-STATUS 'Z001' EXCLUDING FCODE.
  CALL METHOD CL_GUI_CFW=>DISPATCH.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK-CODE.
    WHEN C_DELDOC.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
           EXPORTING
*         TITLEBAR                    = ' '
*         DIAGNOSE_OBJECT             = ' '
             TEXT_QUESTION               = 'Confirma a exclusão da Regional?'
             TEXT_BUTTON_1               = 'Sim'(001)
             ICON_BUTTON_1               = 'ICON_OKAY '
             TEXT_BUTTON_2               = 'Não'(002)
             ICON_BUTTON_2               = 'ICON_CANCEL'
             DEFAULT_BUTTON              = '1'
             DISPLAY_CANCEL_BUTTON       = ' '
*         USERDEFINED_F1_HELP         = ' '
             START_COLUMN                = 25
             START_ROW                   = 6
*         POPUP_TYPE                  =
*         IV_QUICKINFO_BUTTON_1       = ' '
*         IV_QUICKINFO_BUTTON_2       = ' '
          IMPORTING
            ANSWER                      = W_ANSWER
*       TABLES
*         PARAMETER                   =
          EXCEPTIONS
            TEXT_NOT_FOUND              = 1
            OTHERS                      = 2.
      .
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF W_ANSWER = '1'.
        DELETE FROM ZMMT0045 WHERE REGIONAL = WG_CADLAN-REGIONAL.
        IF SY-SUBRC IS INITIAL.
          MESSAGE S836(SD) WITH 'O documento foi excluido com sucesso!'.
        ELSE.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o documento'
                                                 'erro de processamento!'.
        ENDIF.
      ENDIF.

    WHEN C_SEARCH.
      PERFORM BUSCA_DADOS.
    WHEN C_SAVE.
      DELETE TG_ITENS WHERE  WERKS IS INITIAL.

      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.
        PERFORM GRAVA_DADOS.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0

      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = ''
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_BACK.
      CLEAR WG_ACAO.
    WHEN C_DISPLA.
      WG_ACAO = C_DISPLA.
      PERFORM LIMPA_CAMPOS.
      REFRESH: TG_FIELDS.
      PERFORM TRATA_CAMPOS USING SPACE
                           'GR2'
                              C_0       "INPUT 1     NO INPUT 0
                              C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM TRATA_CAMPOS USING SPACE
                                'GR1'
                                 C_1       "INPUT 1     NO INPUT 0
                                 C_0.      "INVISIBLE 1 VISIBLE 0
    WHEN C_ADD.
      WG_ACAO = C_MODIF.
      PERFORM LIMPA_CAMPOS.
      REFRESH: TG_FIELDS.
      PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM TRATA_CAMPOS USING SPACE
                                'GR1'
                                 C_1       "INPUT 1     NO INPUT 0
                                 C_0.      "INVISIBLE 1 VISIBLE 0


    WHEN C_CANCEL.
      CLEAR WG_ACAO.
    WHEN C_ATUALI.

    WHEN C_MODIF.
      IF WG_ACAO = C_MODIF.
        CLEAR WG_ACAO.
        REFRESH: TG_FIELDS.
        PERFORM TRATA_CAMPOS USING SPACE
                                 'GR2'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_1       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0
      ELSE.
        WG_ACAO = C_MODIF.
        PERFORM TRATA_CAMPOS USING SPACE
                                   'GR2'
                                      C_1       "INPUT 1     NO INPUT 0
                                      C_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM TRATA_CAMPOS USING SPACE
                                  'GR1'
                                   C_0       "INPUT 1     NO INPUT 0
                                   C_0.      "INVISIBLE 1 VISIBLE 0
      ENDIF.
    WHEN C_SHOW_MSGRE.
      "CLEAR wg_acao.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA: EVENT TYPE CNTL_SIMPLE_EVENT,
              EVENTS TYPE CNTL_SIMPLE_EVENTS,
              TL_FILTER           TYPE LVC_T_FILT,
              WL_FILTER           TYPE LVC_S_FILT,
              TL_FUNCTION         TYPE UI_FUNCTIONS,
              WL_FUNCTION         LIKE TL_FUNCTION WITH HEADER LINE,
              LT_F4               TYPE LVC_T_F4     WITH HEADER LINE.

  DATA: WAREF      TYPE REF TO DATA.
  IF G_CUSTOM_CONTAINER IS INITIAL.
*    WA_LAYOUT-CWIDTH_OPT = C_X.
    WA_LAYOUT-ZEBRA      = C_X.
    WA_LAYOUT-NO_ROWMARK = C_X.
    WA_STABLE-ROW        = C_X.
    WA_LAYOUT-SEL_MODE   = 'A'.
    WA_LAYOUT-CWIDTH_OPT   = 'X'.
    WA_LAYOUT-NO_TOOLBAR = SPACE.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = G_CONTAINER.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CONTAINER_1.

    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CONTAINER_1.


    PERFORM MONTAR_LAYOUT.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

*      * Register event handler
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_ITENS[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.


    LT_F4-FIELDNAME = 'WERKS'.
    LT_F4-REGISTER = 'X'.
    LT_F4-GETBEFORE = 'X'.
    LT_F4-CHNGEAFTER ='X'.
    APPEND LT_F4.

    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].

    SET HANDLER:
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_ONF4                  FOR GRID1.

*    posiciona spliter na altura x
    CALL METHOD SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 100.
  ELSE.
    PERFORM MONTAR_LAYOUT.
    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG[].

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 ''       '' 'TG_ITENS' 'WERKS'        'Centro'       '10' 'X' ' ' 'X',
        2 ''       '' 'TG_ITENS' 'NAME1'        'Descrição'    '25' ' ' ' ' ' ',
        3 ''       '' 'TG_ITENS' 'CAP_CORREIAS' 'Capacidade'   '12' 'X' ' ' ' ',
        4 'ZMMT0045'       'TP_PADRAO' 'TG_ITENS' 'TP_PADRAO'    'Tempo Médio/Min.'  '20' 'X' ' ' ' '.


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY           = ' '.
  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS         = P_COL_POS.
  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.
  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  IF P_FIELD EQ 'WERKS'.
    W_FIELDCATALOG-F4AVAILABL = C_X.
  ENDIF.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS .


*  IF WG_ACAO = C_ADD. "Novo Lançamento
*
*  ELSEIF WG_ACAO = C_DISPLA  AND WG_CADLAN-REGIONAL IS NOT INITIAL.
  IF WG_CADLAN-REGIONAL IS NOT INITIAL.
    REFRESH TG_ITENS.
    SELECT *
      FROM ZMMT0045
      INTO TABLE IT_ZMMT0045
      WHERE REGIONAL = WG_CADLAN-REGIONAL.

    IF SY-SUBRC = 0.
      READ TABLE IT_ZMMT0045 INTO WA_ZMMT0045 INDEX 1.
      WG_CADLAN-REGIONAL     = WA_ZMMT0045-REGIONAL.
      SELECT *
        FROM T001W
        INTO TABLE IT_T001W
        FOR ALL ENTRIES IN IT_ZMMT0045
        WHERE WERKS = IT_ZMMT0045-WERKS.
      SORT IT_T001W BY WERKS.
    ENDIF.
    LOOP AT IT_ZMMT0045 INTO WA_ZMMT0045.
      TG_ITENS-WERKS        = WA_ZMMT0045-WERKS.
      READ TABLE IT_T001W  INTO WA_T001W  WITH KEY WERKS = WA_ZMMT0045-WERKS BINARY SEARCH.
      TG_ITENS-NAME1        = WA_T001W-NAME1.

      TG_ITENS-CAP_CORREIAS = WA_ZMMT0045-CAP_CORREIAS.
      TG_ITENS-TP_PADRAO    = WA_ZMMT0045-TP_PADRAO.

      APPEND TG_ITENS.
      CLEAR TG_ITENS.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .
  REFRESH: TG_MSG_RET.
  CLEAR: TG_MSG_RET.

  IF WG_CADLAN-REGIONAL IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADLAN-REGIONAL'      TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Regional' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF TG_ITENS[] IS INITIAL.
    MOVE: TEXT-E02                  TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.


ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
FORM TRATA_CAMPOS  USING    P_FIELD
                            P_GROUP1
                            P_VALUE
                            P_INVISIBLE.

  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.

ENDFORM.                    " TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_CAMPOS .
  CLEAR WG_CADLAN.
  REFRESH: TG_ITENS.
ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS .
  DATA: TL_INPUT TYPE TABLE OF ZMMT0045 WITH HEADER LINE.

  LOOP AT TG_ITENS.
    TL_INPUT-MANDT        = SY-MANDT.
    TL_INPUT-WERKS        = TG_ITENS-WERKS.
    TL_INPUT-REGIONAL     = WG_CADLAN-REGIONAL.
    TL_INPUT-CAP_CORREIAS = TG_ITENS-CAP_CORREIAS.
    TL_INPUT-TP_PADRAO    = TG_ITENS-TP_PADRAO.

    APPEND TL_INPUT.
  ENDLOOP.

  MODIFY ZMMT0045 FROM TABLE TL_INPUT.

  MESSAGE S836(SD) WITH 'Regional'
                         WG_CADLAN-REGIONAL
                         ', criado/modificado com sucesso!'.

ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Module  SEARCH_REGIONAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_REGIONAL INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
              TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_REGIONAL OCCURS 0,
          REGIONAL        TYPE ZMMT0045-REGIONAL,
         END OF TL_REGIONAL.

  SELECT DISTINCT REGIONAL
    FROM ZMMT0045
    INTO TABLE TL_REGIONAL
    ORDER BY REGIONAL ASCENDING.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'REGIONAL'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZMMT0045-REGIONAL'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_REGIONAL
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_REGIONAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK-CODE.
    WHEN C_BACK.
      SET SCREEN 0.

    WHEN C_EXIT OR C_CANCEL OR 'UP'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIALIZA_TELA OUTPUT.

  IF WG_ACAO IS INITIAL.
    REFRESH: TG_FIELDS.
    PERFORM TRATA_CAMPOS USING SPACE
                               'GR2'
                                  C_0       "INPUT 1     NO INPUT 0
                                  C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM TRATA_CAMPOS USING SPACE
                              'GR1'
                               C_0       "INPUT 1     NO INPUT 0
                               C_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.

ENDMODULE.                 " INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.
  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ TG_FIELDS-CAMPO
      OR SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.
        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
*        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD."'WG_DESC_OPERACAO'.
  ENDIF.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
