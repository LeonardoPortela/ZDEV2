*&---------------------------------------------------------------------*
*& Report  ZMMR124
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR124.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES:  BEGIN OF TY_SAIDA,
          MARK(1),
          KOSTL         TYPE ZMMT0003-KOSTL,
          UNAME         TYPE ZMMT0003-UNAME,
          DT_VAL_DE     TYPE ZMMT0003-DT_VAL_DE,
          DT_VAL_ATE    TYPE ZMMT0003-DT_VAL_ATE,
          DATA_ATUAL    TYPE ZMMT0003-DATA_ATUAL,
          HORA_ATUAL    TYPE ZMMT0003-HORA_ATUAL,
          USUARIO       TYPE ZMMT0003-USUARIO,
          DEL(1),
          MOD(1),
          LINE_COLOR(4) TYPE C,
          STYLE         TYPE LVC_T_STYL,
        END OF TY_SAIDA,

        BEGIN OF TY_PAR,
          RD_ATIV TYPE  BSID-UMSKZ,
          RD_TODO TYPE  BSID-UMSKZ,
          RD_DESA TYPE  BSID-UMSKZ,
        END OF TY_PAR.


DATA: OK_CODE         LIKE SY-UCOMM,
      WG_MENSAGEM(30),
      WG_ACAO(30),
      TG_MSG_RET      TYPE TABLE OF ZFIWRS0002  WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: OK-CODE         TYPE SY-UCOMM,
      TG_SELECTEDCELL TYPE LVC_T_CELL,
      WG_SELECTEDCELL TYPE LVC_S_CELL,
      X_FIELD(30),
      WG_PAR          TYPE TY_PAR.


*Declaration for toolbar buttons
DATA: TY_TOOLBAR TYPE STB_BUTTON.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*

*Class definition for ALV toolbar
CLASS:  LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

DATA: G_CONTAINER          TYPE SCRFNAME VALUE 'CC_ALV',
      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      G_DESCBOX            TYPE SCRFNAME VALUE 'CC_OBS',
      G_CUSTOM_CONT_DESC   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBG_DESCBOX          TYPE REF TO CL_GUI_TEXTEDIT,
      OBG_DOCKING          TYPE REF TO CL_GUI_DOCKING_CONTAINER,

      WA_STYLE             TYPE LVC_S_STYL,
      STYLE                TYPE LVC_T_STYL   WITH HEADER LINE,
      STYLE2               TYPE LVC_T_STYL   WITH HEADER LINE,
      GS_VARIANT_C         TYPE DISVARIANT.

** Criação de tabela dinamica
DATA: T_FIELDCATALOG TYPE LVC_T_FCAT,
      W_FIELDCATALOG TYPE LVC_S_FCAT,
      WA_LAYOUT      TYPE LVC_S_LAYO,
      WA_STABLE      TYPE LVC_S_STBL,
      IT_SAIDA       TYPE TABLE OF TY_SAIDA,
      WA_SAIDA       TYPE TY_SAIDA.



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
  C_REFRESH(7)      TYPE C VALUE 'REFRESH',
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


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD ON_DOUBLE_CLICK.

  ENDMETHOD.                   "ON_DOUBLE_CLICK

  METHOD ON_DATA_CHANGED.
    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                      INTO LS_GOOD
                      WHERE FIELDNAME = 'DT_VAL_ATE'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX LS_GOOD-ROW_ID.
      WA_SAIDA-MOD = 'X'.
      WA_SAIDA-MARK = 'X'.
      MODIFY IT_SAIDA FROM WA_SAIDA INDEX LS_GOOD-ROW_ID TRANSPORTING MOD MARK.
    ENDLOOP.
  ENDMETHOD.

  METHOD ON_DATA_CHANGED_FINISHED.

  ENDMETHOD.

ENDCLASS.
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
    DATA: TL_SAIDA TYPE TABLE OF TY_SAIDA,
          WL_SAIDA TYPE TY_SAIDA,
          WL_LINES TYPE SY-TABIX.


    CASE E_UCOMM.
      WHEN C_ADD.
        CALL METHOD GRID1->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.
        CLEAR WG_SELECTEDCELL.
        IF TG_SELECTEDCELL[] IS NOT INITIAL.
          READ TABLE TG_SELECTEDCELL INTO WG_SELECTEDCELL INDEX 1.
        ENDIF.
*
        TL_SAIDA[] = IT_SAIDA[].
        REFRESH: IT_SAIDA.
        LOOP AT TL_SAIDA INTO WL_SAIDA.
          APPEND WL_SAIDA TO IT_SAIDA.
          IF SY-TABIX = WG_SELECTEDCELL-ROW_ID-INDEX.
            CLEAR: WL_SAIDA.
            WL_SAIDA-MARK   = 'X'.
            APPEND WL_SAIDA TO IT_SAIDA.
          ENDIF.
        ENDLOOP.
        IF WG_SELECTEDCELL-ROW_ID-INDEX = 0.
          CLEAR: WL_SAIDA.
          WL_SAIDA-MARK   = 'X'.
          APPEND WL_SAIDA TO IT_SAIDA.
        ENDIF.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
      WHEN C_DEL.
        CALL METHOD GRID1->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.

        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          READ TABLE  IT_SAIDA INTO WL_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
          IF WL_SAIDA-DEL = 'X'.
            WL_SAIDA-LINE_COLOR = ' '.
            WL_SAIDA-DEL        = ' '.
            WL_SAIDA-MARK       = ' '.
          ELSE.
            WL_SAIDA-LINE_COLOR = 'C610'.
            WL_SAIDA-DEL        = 'X'.
            WL_SAIDA-MARK       = 'X'.
          ENDIF.
          MODIFY IT_SAIDA FROM WL_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
        ENDLOOP.

        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

START-OF-SELECTION.

  WG_PAR-RD_ATIV = 'X'.
  PERFORM F_ATUALIZA.


  CALL SCREEN '0100'.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'Z001' .
  SET TITLEBAR 'Z001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA: EVENT       TYPE CNTL_SIMPLE_EVENT,
        EVENTS      TYPE CNTL_SIMPLE_EVENTS,
        TL_FILTER   TYPE LVC_T_FILT,
        WL_FILTER   TYPE LVC_S_FILT,
        TL_FUNCTION TYPE UI_FUNCTIONS,
        WL_FUNCTION LIKE TL_FUNCTION  WITH HEADER LINE.

  DATA: WAREF TYPE REF TO DATA.

  WA_STABLE-ROW = 'X'.
  WA_STABLE-COL = 'X'.

  IF G_CUSTOM_CONTAINER IS INITIAL.
    WA_LAYOUT-CWIDTH_OPT = C_X.
    WA_LAYOUT-ZEBRA       = C_X.
*    WA_LAYOUT-NO_TOOLBAR  = C_X.
    WA_LAYOUT-NO_ROWMARK  = SPACE.
    WA_LAYOUT-COL_OPT     = C_X.
    WA_STABLE-ROW         = C_X.
    WA_LAYOUT-SEL_MODE    = 'A'.
*    WA_LAYOUT-BOX_FNAME   = 'MARK'.
    WA_LAYOUT-INFO_FNAME  = 'LINE_COLOR'.
    WA_LAYOUT-STYLEFNAME = 'STYLE'.


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

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

* REGISTER EVENT HANDLER
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

    PERFORM F_MONTAR_LAYOUT.


    CLEAR WA_LAYOUT-CWIDTH_OPT.
    GS_VARIANT_C-REPORT = SY-REPID. "Enable users save own LAYOUTs

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT           = GS_VARIANT_C
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = IT_SAIDA[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.


    SET HANDLER:
      LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK          FOR GRID1,
      LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
      LCL_EVENT_HANDLER=>ON_DATA_CHANGED          FOR GRID1.

*    posiciona spliter na altura x
    CALL METHOD SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 100.
  ELSE.
    PERFORM F_MONTAR_LAYOUT.

    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG[].

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MONTAR_LAYOUT .
  REFRESH T_FIELDCATALOG.
  PERFORM F_MONTAR_ESTRUTURA USING:
       1   'CSKS'     'KOSTL'       'IT_SAIDA'  'KOSTL'      'C.Custo'             '12' 'X' ' ' ' ',
       2   'USR21'    'BNAME'       'IT_SAIDA'  'UNAME'      'Aprovador'           '15' 'X' ' ' ' ',
       3   'ZMMT0003' 'DT_VAL_DE'   'IT_SAIDA'  'DT_VAL_DE'  'Dt. Ini'             '12' 'X' ' ' ' ',
       3   'ZMMT0003' 'DT_VAL_ATE'  'IT_SAIDA'  'DT_VAL_ATE' 'Dt. Fim'             '12' 'X' ' ' ' ',
       4   'ZMMT0003' 'DATA_ATUAL'  'IT_SAIDA'  'DATA_ATUAL' 'Dt.Sistema'          '12' ' ' ' ' ' ',
       5   'ZMMT0003' 'HORA_ATUAL'  'IT_SAIDA'  'HORA_ATUAL' 'Hr.Sistema'          '12' ' ' ' ' ' ',
       6   'ZMMT0003' 'USUARIO'     'IT_SAIDA'  'USUARIO'    'Usuario'             '15' ' ' ' ' ' '.
ENDFORM.

*&------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_MONTAR_ESTRUTURA  USING  P_COL_POS   P_REF_TABNAME P_REF_FIELDNAME P_TABNAME P_FIELD
                                P_SCRTEXT_L P_OUTPUTLEN   P_EDIT          P_SUM     P_EMPHASIZE.

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.

  W_FIELDCATALOG-KEY           = ' '.
  CASE P_FIELD.
    WHEN 'KOSTL' OR 'UNAME' OR 'DT_VAL_DE' .
      W_FIELDCATALOG-KEY           = 'X'.
    WHEN OTHERS.
  ENDCASE.

  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS       = P_COL_POS.

  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  ENDIF.

  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.
ENDFORM.                    " F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE OK-CODE.

    WHEN C_SEARCH OR C_REFRESH.
      PERFORM F_ATUALIZA.

    WHEN C_SAVE.
      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.

        PERFORM: F_GRAVA_DADOS.

      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E35.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_PRESSED_TAB = 'TS_100_IMP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.


    WHEN C_DISPLA.
      WG_ACAO = C_DISPLA.

    WHEN C_ADD.
      CHECK WG_ACAO <> C_ADD.

      WG_ACAO = C_ADD.  "c_modif.


    WHEN C_ATUALI.

    WHEN C_MODIF.
      IF WG_ACAO = C_MODIF.
        CLEAR WG_ACAO.

      ELSE.
        WG_ACAO = C_MODIF.

      ENDIF.


    WHEN C_SHOW_MSGRE.
      PERFORM F_VERIFICA_ERROS.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = C_X
          I_REPID       = SY-REPID
          I_PRESSED_TAB = 'TS_100_IMP-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.
    WHEN C_CANCEL.
      CLEAR WG_ACAO.
      LEAVE PROGRAM.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GRAVA_DADOS .
  DATA: WL_INPUT TYPE ZMMT0003,
        TL_INPUT TYPE TABLE OF ZMMT0003.

  LOOP AT IT_SAIDA INTO WA_SAIDA WHERE MARK = 'X'.
    IF WA_SAIDA-DEL = 'X'.
      DELETE FROM ZMMT0003 WHERE KOSTL = WA_SAIDA-KOSTL
                      AND   UNAME = WA_SAIDA-UNAME
                      AND   DT_VAL_DE = WA_SAIDA-DT_VAL_DE.
      COMMIT WORK.
    ELSE.
      MOVE-CORRESPONDING WA_SAIDA TO WL_INPUT.
      WL_INPUT-DATA_ATUAL = SY-DATUM.
      WL_INPUT-HORA_ATUAL = SY-UZEIT.
      WL_INPUT-USUARIO = SY-UNAME.
      APPEND WL_INPUT TO TL_INPUT.
    ENDIF.
  ENDLOOP.
  MODIFY ZMMT0003 FROM TABLE TL_INPUT.
  COMMIT WORK.

  PERFORM F_ATUALIZA.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS .
  DATA: WL_LINHA(6),
        WL_ZMMT0003 TYPE ZMMT0003,
        WL_USR21    TYPE USR21,
        WL_CSKS     TYPE CSKS.

  CLEAR:    TG_MSG_RET.
  REFRESH:  TG_MSG_RET.

  LOOP AT IT_SAIDA INTO WA_SAIDA WHERE MARK = 'X'.
    WL_LINHA = SY-TABIX.
    "
    IF WA_SAIDA-DEL NE 'X'.
      IF WA_SAIDA-DT_VAL_DE GE WA_SAIDA-DT_VAL_ATE.
        CLEAR TG_MSG_RET-ABA.
        CONCATENATE 'Data FINAL deve ser maior que INICIAL   linha:' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
      IF WA_SAIDA-DT_VAL_ATE IS INITIAL.
        CLEAR TG_MSG_RET-ABA.
        CONCATENATE 'Data FINAL não informada   linha:' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.

      IF WA_SAIDA-STYLE[] IS INITIAL.
        SELECT SINGLE *
          FROM ZMMT0003
          INTO WL_ZMMT0003
          WHERE KOSTL = WA_SAIDA-KOSTL
          AND   UNAME = WA_SAIDA-UNAME
          AND   DT_VAL_DE = WA_SAIDA-DT_VAL_DE.

        IF SY-SUBRC = 0.
          CLEAR TG_MSG_RET-ABA.
          CONCATENATE 'Aprovador já cadastrado neste periodo linha:' WL_LINHA INTO  TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.
          CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.

      SELECT SINGLE *
       FROM USR21
       INTO WL_USR21
       WHERE BNAME = WA_SAIDA-UNAME.
      IF SY-SUBRC NE 0.
        CLEAR TG_MSG_RET-ABA.
        CONCATENATE 'Aprovador não existe no SAP  linha:' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ATUALIZA .
  IF WG_PAR-RD_ATIV = 'X'.
    SELECT *
     FROM ZMMT0003
     INTO CORRESPONDING FIELDS OF TABLE IT_SAIDA
    WHERE DT_VAL_DE  LE SY-DATUM
    AND   DT_VAL_ATE GE SY-DATUM.
  ENDIF.
  IF WG_PAR-RD_DESA = 'X'.
    SELECT *
     FROM ZMMT0003
     INTO CORRESPONDING FIELDS OF TABLE IT_SAIDA
    WHERE DT_VAL_ATE LT SY-DATUM
    OR    DT_VAL_DE  GT SY-DATUM.

  ENDIF.
  IF WG_PAR-RD_TODO = 'X'.
    SELECT *
      FROM ZMMT0003
      INTO CORRESPONDING FIELDS OF TABLE IT_SAIDA.
  ENDIF.

  CLEAR: WA_STYLE.
  REFRESH: STYLE.
  "
  WA_STYLE-FIELDNAME = 'KOSTL'.
  WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE .

  WA_STYLE-FIELDNAME = 'UNAME'.
  WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE .

  WA_STYLE-FIELDNAME = 'DT_VAL_DE'.
  WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE .

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    INSERT LINES OF STYLE INTO TABLE  WA_SAIDA-STYLE.
    MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX TRANSPORTING STYLE.
  ENDLOOP.
ENDFORM.
