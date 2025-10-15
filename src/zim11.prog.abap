*&---------------------------------------------------------------------*
*& Report  ZIM11
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZIM11.

TYPE-POOLS: RMDI.
INCLUDE: <ICON>.
INCLUDE <CL_ALV_CONTROL>.

TYPES: BEGIN OF TY_TELA_SEL,
         MARK,
         STATUS(4),       "type zsdt0036-loekz,
         VISAO        TYPE ZIM08_REL_INV_US-VISAO,
         BUKRS        TYPE ZIM08_REL_INV_US-ABUKRS,
         POSNR        TYPE ZIM08_REL_INV_US-POSNR,
         GJAHR        TYPE ZIM08_REL_INV_US-GJAHR,
         WAERS        TYPE ZIM08_REL_INV_US-WAERS,
         MONAT        TYPE BSID-MONAT,
         USERNAME     TYPE BTCH2170-USERNAME,
         FROM_DATE    TYPE BTCH2170-FROM_DATE,
         FROM_TIME    TYPE BTCH2170-FROM_TIME,
         STYLE2       TYPE LVC_T_STYL,
       END OF TY_TELA_SEL.

DATA: BEGIN OF GT_VALUES OCCURS 0,
        DOMVALUE_L TYPE DOMVALUE_L,
        DDTEXT TYPE VAL_TEXT,
      END OF GT_VALUES.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_X           TYPE C VALUE 'X',
           C_ADD(3)      TYPE C VALUE 'ADD',
           C_DEL(3)      TYPE C VALUE 'DEL',
           C_EXIT(4)     TYPE C VALUE 'EXIT',
           C_BACK(4)     TYPE C VALUE 'BACK',
           C_SAVE(4)     TYPE C VALUE 'SAVE',
*           c_red(4)      type c value '@0A@',
*           c_yellow(4)   type c value '@09@',
*           c_green(4)    type c value '@08@',
*           c_aguard(4)   type c value '@9R@',
           C_PROCES(6)   TYPE C VALUE 'PROCES',
           C_CANCEL(6)   TYPE C VALUE 'CANCEL',
           C_ATUALI(6)   TYPE C VALUE 'ATUALI',
           C_SEARCH(6)    TYPE C VALUE 'SEARCH',
           C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE'.

*&--------------------------------------------------------------------&*
*& Declaração de Variaveis/Tabelas/Workarea                           &*
*&--------------------------------------------------------------------&*
DATA: TG_TELA_SEL     TYPE TABLE OF TY_TELA_SEL WITH HEADER LINE,
      OK_CODE         TYPE SY-UCOMM,
      INIT,
      WG_DISPLAY,
      X_FIELD(30),
      WG_MENSAGEM(30),
      WG_OBJ(40).

DATA: TG_MSG_RET TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE.
*Declaration for toolbar buttons
DATA : TY_TOOLBAR TYPE STB_BUTTON.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
*Class definition for ALV toolbar
CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

DATA: GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      CONTAINER1           TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.

DATA: T_FIELDCATALOG        TYPE LVC_T_FCAT,
      W_FIELDCATALOG        TYPE LVC_S_FCAT,
      TG_SELECTEDCELL       TYPE LVC_T_CELL,
      WG_SELECTEDCELL       TYPE LVC_S_CELL,
*      TG_FIELDCATALOG_LCTOS TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      WA_LAYOUT             TYPE LVC_S_LAYO,
      WA_STABLE             TYPE LVC_S_STBL,
      WG_CELL TYPE LVC_S_CELL,
      TG_CELL TYPE LVC_T_CELL,
      WA_STYLE            TYPE LVC_S_STYL,
      STYLE2 TYPE LVC_T_STYL WITH HEADER LINE.

START-OF-SELECTION.
  CALL SCREEN 100.

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
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.

*  PUBLIC SECTION.
*    CLASS-METHODS:
*      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
*                      IMPORTING E_ROW E_COLUMN.

*    CLASS-METHODS:
*      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*                                   IMPORTING  E_ROW_ID E_COLUMN_ID.
*
*    CLASS-METHODS:
*     ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
*                       IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
*
*    CLASS-METHODS:
*       ON_DATA_CHANGED_FINISHED FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
*                      IMPORTING E_MODIFIED ET_GOOD_CELLS.
*    CLASS-METHODS:
*       ON_BUTTON_CLICK FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
*                      IMPORTING ES_COL_ID ES_ROW_NO.
*
*    CLASS-METHODS:
*       ON_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
*                      IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA
*                                ET_BAD_CELLS E_DISPLAY.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
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
*   Add customized toolbar buttons.
*    IF WG_DOCS-DOCNUM IS INITIAL.
*      READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
*        WITH KEY GROUP1 = 'GR1'.
*      IF SY-SUBRC IS INITIAL.
*        IF WG_FISCAL-RETORNO EQ 'S'.
*          WL_DESACTIVE = 1.
*        ELSE.
*          WL_DESACTIVE = SPACE.
*        ENDIF.
*      ELSE.
*        WL_DESACTIVE = 1.
*      ENDIF.
*    ELSE.
*      WL_DESACTIVE = 1.
*    ENDIF.
    TY_TOOLBAR-ICON      =  ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  =  C_ADD.
    IF WG_DISPLAY IS INITIAL.
      TY_TOOLBAR-DISABLED  = SPACE.
    ELSE.
      TY_TOOLBAR-DISABLED  = 1.
    ENDIF.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
*
*    IF WG_DOCS-DOCNUM IS INITIAL.
*      READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
*        WITH KEY GROUP1 = 'GR1'.
*      IF SY-SUBRC IS INITIAL.
*        WL_DESACTIVE = SPACE.
*      ELSE.
*        WL_DESACTIVE = 1.
*      ENDIF.
*    ELSE.
*      WL_DESACTIVE = 1.
*    ENDIF.
    TY_TOOLBAR-ICON      =  ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  =  C_DEL.
    IF WG_DISPLAY IS INITIAL.
      TY_TOOLBAR-DISABLED  = SPACE.
    ELSE.
      TY_TOOLBAR-DISABLED  = 1.
    ENDIF.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
*
*    TY_TOOLBAR-BUTN_TYPE = 3.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
**   variable for Toolbar Button
*    TY_TOOLBAR-ICON      =  ICON_VIEW_CLOSE.
*    TY_TOOLBAR-FUNCTION  =  C_CLOS_MSG.
*    TY_TOOLBAR-DISABLED  = SPACE.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
**   Call reorganize method of toolbar manager to
**   display the toolbar
*    call method c_alv_toolbarmanager->reorganize
*      exporting
*        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD HANDLE_USER_COMMAND.
    DATA: WL_TELA_SEL TYPE TY_TELA_SEL.
*          WL_0036 TYPE ZSDT0036.
*          WL_ITENS LIKE LINE OF TG_ITENS,
*          WL_LINES TYPE SY-TABIX.
*    REFRESH: TL_ITENS_AUX.
**   User Command Botões Incluidos
**    break abap.
*    IF P_OPERACAO IS  NOT INITIAL
*AND P_BUKRS IS    NOT INITIAL
* AND P_BRANCH IS  NOT INITIAL
*  AND P_PARVW IS  NOT INITIAL
*   AND P_PARID IS NOT INITIAL.
    CASE E_UCOMM.
*        WHEN C_CLOS_MSG.
*          IF GRID2 IS NOT INITIAL.
*            CALL METHOD GRID2->FREE.
*            FREE: CONTAINER_2, GRID2.
*          ENDIF.
**    posiciona spliter na altura x
*          IF SPLITTER IS NOT INITIAL.
*            CALL METHOD SPLITTER->SET_ROW_HEIGHT
*              EXPORTING
*                ID     = 1
*                HEIGHT = 100.
*          ENDIF.
*          LEAVE TO SCREEN 100.
      WHEN C_ADD.
*        clear: wl_saida.
*        move: s_cultu-low to wl_SAIDA-cultura.
*        move: s_safra-low to wl_SAIDA-safra.
*        APPEND wl_saida to TG_SAIDA.
        CLEAR WL_TELA_SEL.
        WL_TELA_SEL-STATUS      = ICON_LIGHT_OUT.
        WL_TELA_SEL-USERNAME    = SY-UNAME.
        WL_TELA_SEL-FROM_DATE   = SY-DATUM.
        WL_TELA_SEL-FROM_TIME   = SY-UZEIT.
        APPEND WL_TELA_SEL TO TG_TELA_SEL.
*        APPEND INITIAL LINE TO TG_TELA_SEL.
*
*          TL_ITENS_AUX[] = TG_ITENS[].
*          REFRESH: TG_ITENS.
*          LOOP AT TL_ITENS_AUX INTO WL_ITENS.
*            WL_ITENS-ITMNUM = SY-TABIX * 10.
*            APPEND WL_ITENS TO TG_ITENS.
*          ENDLOOP.
*          DESCRIBE TABLE TG_ITENS LINES WL_LINES.
*          CLEAR: WL_ITENS.
*          WL_ITENS-ITMNUM = ( WL_LINES + 1 ) * 10 .
*          APPEND WL_ITENS TO TG_ITENS.
*
*          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
*            EXPORTING
*              IS_STABLE = WA_STABLE.
      WHEN C_DEL.
        CALL METHOD GRID1->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.
*
        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          READ TABLE TG_TELA_SEL INTO WL_TELA_SEL INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
          IF WL_TELA_SEL-STATUS EQ ICON_LIGHT_OUT.
*          WL_SAIDA-ELIMINADO  = C_X.
*          WL_SAIDA-USNAM      = SY-UNAME.
*          WL_SAIDA-DATA_ATUAL = SY-DATUM.
*          WL_SAIDA-HORA_ATUAL = SY-UZEIT.
*          MODIFY TG_TELA_SEL FROM WL_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX
*            TRANSPORTING ELIMINADO USNAM DATA_ATUAL HORA_ATUAL.
            DELETE TG_TELA_SEL INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
          ELSE.
            MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'A linha selecionada já foi processada,'
                                                   'não pode ser eliminada!'.
          ENDIF.
        ENDLOOP.
*          IF WG_FISCAL-RETORNO EQ 'N'.
*            TL_ITENS_AUX[] = TG_ITENS[].
*            REFRESH: TG_ITENS.
*            LOOP AT TL_ITENS_AUX INTO WL_ITENS.
*              WL_ITENS-ITMNUM = SY-TABIX * 10.
*              APPEND WL_ITENS TO TG_ITENS.
*            ENDLOOP.
*          ENDIF.
*          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
*            EXPORTING
*              IS_STABLE = WA_STABLE.
    ENDCASE.
*    PERFORM VERIFICA_ERROS.
*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        I_SCREEN      = '100'
*        I_SHOW        = SPACE
*        I_REPID       = SY-REPID
*        I_POPUP       = 1
**            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
**            I_SET_FIELD   = 'X_FIELD'
*      IMPORTING
*        E_MESSAGEM    = WG_MENSAGEM
*      TABLES
*        IT_MSGS       = TG_MSG_RET.

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
*    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.
  IF WG_DISPLAY IS NOT INITIAL.
    APPEND C_SAVE TO FCODE.

  ENDIF.
*  PERFORM VERIFICA_ERROS.
*  CALL FUNCTION 'Z_DOC_CHECK_NEW'
*    EXPORTING
*      I_SCREEN      = '100'
*      I_SHOW        = SPACE
*      I_REPID       = SY-REPID
*      I_POPUP       = 1
**            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
**            I_SET_FIELD   = 'X_FIELD'
*    IMPORTING
*      E_MESSAGEM    = WG_MENSAGEM
*    TABLES
*      IT_MSGS       = TG_MSG_RET.
*
*  IF WG_CELL IS NOT INITIAL .
*    REFRESH: TG_CELL.
*    APPEND WG_CELL TO TG_CELL.
**          CONCATENATE wl_obj '->SET_SELECTED_CELLS' INTO wg_obj.
*    CALL METHOD GRID1->SET_SELECTED_CELLS"(wg_obj)          "(wg_msgs)=>set_selected_cells
*        EXPORTING
*          IT_CELLS = TG_CELL[].
*  ENDIF.

  SET PF-STATUS 'Z001' EXCLUDING FCODE.
  SET TITLEBAR 'Z001'.
  IF SY-SUBRC IS INITIAL.


  ENDIF.


ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS OUTPUT.
  DATA: WL_REPID     TYPE SY-REPID,
        TL_FUNCTION  TYPE UI_FUNCTIONS,
        WL_FUNCTION  LIKE TL_FUNCTION WITH HEADER LINE,
        LT_F4        TYPE LVC_T_F4 WITH HEADER LINE,
        TL_FILTER    TYPE LVC_T_FILT,
        WL_FILTER    TYPE LVC_S_FILT.

  WL_REPID = SY-REPID.

  IF CONTAINER1 IS INITIAL.
    REFRESH: TG_TELA_SEL.

    WA_LAYOUT-ZEBRA      = C_X.
*    wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-CWIDTH_OPT = C_X.
    WA_STABLE-ROW        = C_X.
    WA_LAYOUT-SEL_MODE   = 'C'.
    WA_LAYOUT-BOX_FNAME  = 'MARK'.

    CREATE OBJECT CONTAINER1
      EXPORTING
        REPID     = WL_REPID
        DYNNR     = '0100'
*        style     = container1->WS_MINIMIZEBOX
        SIDE      = CONTAINER1->DOCK_AT_TOP
        EXTENSION = 400.
*        METRIC    = 50.

*    CALL METHOD CONTAINER1->FLOAT
*       EXPORTING
*        DO_FLOAT     = 1.
**        RATIO = 95.

    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CONTAINER1.

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
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*    append wl_function to tl_function.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_MB_FILTER.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    PERFORM MONTAR_LAYOUT.
    PERFORM BUILD_DROPDOWN.

*    WA_LAYOUT-CTAB_FNAME = 'CELLCOLORS'.
    WA_LAYOUT-STYLEFNAME = 'STYLE2'.
*
*    LT_F4-FIELDNAME = 'CULTURA'.
*    LT_F4-REGISTER = 'X' .
*    LT_F4-GETBEFORE = 'X' .
*    APPEND LT_F4 .
*
*    LT_F4-FIELDNAME = 'MEINS'.
*    LT_F4-REGISTER = 'X' .
*    LT_F4-GETBEFORE = 'X' .
*    APPEND LT_F4 .

*    WL_FILTER-FIELDNAME = 'ELIMINADO'."c_dmbtr.
*    WL_FILTER-SIGN      = 'I'. "c_i.
*    WL_FILTER-OPTION    = 'NE'. "c_ne.
*    WL_FILTER-LOW       = 'X'.
*
*    APPEND WL_FILTER TO TL_FILTER.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FILTER            = TL_FILTER
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_TELA_SEL[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].

*    SET HANDLER:
*              LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK          FOR GRID1,
*              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
*              LCL_EVENT_HANDLER=>ON_DATA_CHANGED          FOR GRID1,
*              LCL_EVENT_HANDLER=>ON_BUTTON_CLICK          FOR GRID1,
*              LCL_EVENT_HANDLER=>ON_ONF4                  FOR GRID1.
  ELSE.
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
FORM MONTAR_LAYOUT.
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        0 ' '                 ' '          'TG_TELA_SEL' 'STATUS'     'Status'  '5' ' ' ' ' ' ',
        1 'ZIM08_REL_INV_US'  'VISAO'      'TG_TELA_SEL' 'VISAO'      'Visão'   '15' 'X' ' ' ' ',
        1 'BSIS'              'BUKRS'      'TG_TELA_SEL' 'BUKRS'      ' '       ' ' 'X' ' ' ' ',
        1 'ZIM08_REL_INV_US'  'POSNR'      'TG_TELA_SEL' 'POSNR'      ' '       ' ' 'X' ' ' ' ',
        1 'BSIS'              'GJAHR'      'TG_TELA_SEL' 'GJAHR'      ' '       ' ' 'X' ' ' ' ',
        1 'BSIS'              'WAERS'      'TG_TELA_SEL' 'WAERS'      ' '       ' ' 'X' ' ' ' ',
        1 'BSID'              'MONAT'      'TG_TELA_SEL' 'MONAT'      ' '       ' ' 'X' ' ' ' ',
        1 'BTCH2170'          'USERNAME'   'TG_TELA_SEL' 'USERNAME'   ' '       ' ' ' ' ' ' ' ',
        1 'BTCH2170'          'FROM_DATE'  'TG_TELA_SEL' 'FROM_DATE'  ' '       ' ' 'X' ' ' ' ',
        1 'BTCH2170'          'FROM_TIME'  'TG_TELA_SEL' 'FROM_TIME'  ' '       ' ' 'X' ' ' ' '.


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
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
*  w_fieldcatalog-key_sel       = 'X'.
  IF WG_DISPLAY IS INITIAL.
    W_FIELDCATALOG-EDIT          = P_EDIT.
  ENDIF.
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

  IF P_FIELD EQ 'STATUS'.
*    w_fieldcatalog-checkbox = c_x.
*    w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
    W_FIELDCATALOG-JUST         = 'C'.
    IF WG_DISPLAY IS INITIAL.
*      W_FIELDCATALOG-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    ELSE.
*      w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
  ENDIF.

*  IF P_FIELD EQ 'MEINS'.
**    w_fieldcatalog-checktable = 'T006'.
*    W_FIELDCATALOG-F4AVAILABL = C_X.
*    W_FIELDCATALOG-CONVEXIT = 'CUNIT'.
*  ENDIF.

  IF P_FIELD EQ 'VISAO'.
    W_FIELDCATALOG-DRDN_HNDL  = 1.
    W_FIELDCATALOG-DRDN_ALIAS  = C_X.

  ENDIF.

*  IF P_FIELD EQ 'CULTURA'.
*    W_FIELDCATALOG-F4AVAILABL  = 'X'.
*  ENDIF.
  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
*  DATA: WL_CELL TYPE LVC_S_CELL,
*        TL_CELL TYPE LVC_T_CELL,
*        WL_OBJ(30).
*
*  REFRESH: TL_CELL.
*  CLEAR: WL_CELL, WL_OBJ.
  CALL METHOD GRID1->CHECK_CHANGED_DATA.
  CASE OK_CODE.
    WHEN C_PROCES.
      PERFORM PROCESSA_LINHAS_JOB.

    WHEN C_BACK.
      LEAVE TO SCREEN 0.
    WHEN C_CANCEL.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 100.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_DROPDOWN .
  DATA:   LS_DROPDOWN      TYPE LVC_S_DRAL,
          LT_DROPDOWN      TYPE LVC_T_DRAL.


  LS_DROPDOWN-HANDLE = '1'.

  LS_DROPDOWN-INT_VALUE = '01'.
  LS_DROPDOWN-VALUE = 'Competência '.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN.

  LS_DROPDOWN-INT_VALUE = '02'.
  LS_DROPDOWN-VALUE = 'Consolidado (Compet. E Caixa)'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-INT_VALUE = '03'.
  LS_DROPDOWN-VALUE     = 'Caixa'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN.

*LS_DROPDOWN-HANDLE = '2'.
*  SELECT *
*      FROM ZSDT0038
*      INTO TABLE TL_0038.
*
*    LOOP AT TL_0038.
*        LS_DROPDOWN-VALUE = TL_0038-DESCRICAO.
*        GT_VALUES-DDTEXT =  TL_0038-DESCRICAO.
*        GT_VALUES-DOMVALUE_L = TL_0038-DESCRICAO.
*
*        APPEND: LS_DROPDOWN TO LT_DROPDOWN,
*          GT_VALUES.
*
*    ENDLOOP.
* Übergabe der Dropdown-Tabelle an ALV-Grid-Control
  CALL METHOD GRID1->SET_DROP_DOWN_TABLE
    EXPORTING
      IT_DROP_DOWN_ALIAS = LT_DROPDOWN.
ENDFORM.                    " BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_LINHAS_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSA_LINHAS_JOB .
  DATA: WL_JOBNAME TYPE TBTCJOB-JOBNAME,
        WL_IMEDIAT,
        WL_TESTE,
        WA_STYLE            TYPE LVC_S_STYL,
        STYLE2 TYPE LVC_T_STYL WITH HEADER LINE.

  DATA: SELTAB  LIKE RANGE OF ZPFE_ARQUIVO-LINHA,
    SELTAB_WA LIKE LINE OF SELTAB.

  DATA: JOBCOUNT LIKE TBTCJOB-JOBCOUNT.

  RANGES: RG_BUKRS FOR T001-BUKRS,
          RG_POSNR FOR IMAK-POSNR,
          RG_GJAHR FOR IMAK-GJAHR,
          RG_WAERS FOR ZIM08_REL_INV_US-WAERS,
          RG_MONAT FOR BSID-MONAT,
          RG_VISAO FOR ZIM08_REL_INV_US-VISAO.

  REFRESH: STYLE2.
  WA_STYLE-FIELDNAME = 'VISAO'.
  WA_STYLE-STYLE = ALV_STYLE_DISABLED..
  INSERT  WA_STYLE INTO TABLE STYLE2.

  WA_STYLE-FIELDNAME = 'BUKRS'.
  WA_STYLE-STYLE = ALV_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE2.

  WA_STYLE-FIELDNAME = 'POSNR'.
  WA_STYLE-STYLE = ALV_STYLE_DISABLED..
  INSERT  WA_STYLE INTO TABLE STYLE2.

  WA_STYLE-FIELDNAME = 'GJAHR'.
  WA_STYLE-STYLE = ALV_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE2.

  WA_STYLE-FIELDNAME = 'WAERS'.
  WA_STYLE-STYLE = ALV_STYLE_DISABLED..
  INSERT  WA_STYLE INTO TABLE STYLE2.

  WA_STYLE-FIELDNAME = 'MONAT'.
  WA_STYLE-STYLE = ALV_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE2.

  WA_STYLE-FIELDNAME = 'USERNAME'.
  WA_STYLE-STYLE = ALV_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE2.

  WA_STYLE-FIELDNAME = 'FROM_DATE'.
  WA_STYLE-STYLE = ALV_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE2.

  WA_STYLE-FIELDNAME = 'FROM_TIME'.
  WA_STYLE-STYLE = ALV_STYLE_DISABLED.
  INSERT  WA_STYLE INTO TABLE STYLE2.

*  INSERT LINES OF STYLE2 INTO TABLE WL_SAIDA-STYLE2.

  LOOP AT TG_TELA_SEL
      WHERE STATUS EQ ICON_LIGHT_OUT.
    REFRESH: RG_BUKRS, RG_POSNR, RG_GJAHR, RG_WAERS, RG_MONAT, RG_VISAO.



    IF TG_TELA_SEL-BUKRS IS NOT INITIAL.
      RG_BUKRS-SIGN    = 'I'.
      RG_BUKRS-OPTION  = 'EQ'.
      RG_BUKRS-LOW     = TG_TELA_SEL-BUKRS.

      APPEND RG_BUKRS.
      CLEAR: RG_BUKRS.
    ENDIF.

    IF TG_TELA_SEL-POSNR IS NOT INITIAL.
      RG_POSNR-SIGN    = 'I'.
      RG_POSNR-OPTION  = 'EQ'.
      RG_POSNR-LOW     = TG_TELA_SEL-POSNR.

      APPEND RG_POSNR.
      CLEAR: RG_POSNR.
    ENDIF.

    IF TG_TELA_SEL-GJAHR IS NOT INITIAL.
      RG_GJAHR-SIGN    = 'I'.
      RG_GJAHR-OPTION  = 'EQ'.
      RG_GJAHR-LOW     = TG_TELA_SEL-GJAHR.

      APPEND RG_GJAHR.
      CLEAR: RG_GJAHR.
    ELSE.
      RG_GJAHR-SIGN    = 'I'.
      RG_GJAHR-OPTION  = 'EQ'.
      RG_GJAHR-LOW     = '0000'.
      RG_GJAHR-LOW     = '9999'.

      APPEND RG_GJAHR.
      CLEAR: RG_GJAHR.
    ENDIF.

    IF TG_TELA_SEL-WAERS IS NOT INITIAL.
      RG_WAERS-SIGN    = 'I'.
      RG_WAERS-OPTION  = 'EQ'.
      RG_WAERS-LOW     = TG_TELA_SEL-WAERS.

      APPEND RG_WAERS.
      CLEAR: RG_WAERS.
    ENDIF.

    IF TG_TELA_SEL-MONAT IS NOT INITIAL.
      RG_MONAT-SIGN    = 'I'.
      RG_MONAT-OPTION  = 'EQ'.
      RG_MONAT-LOW     = TG_TELA_SEL-MONAT.

      APPEND RG_MONAT.
      CLEAR: RG_MONAT.
    ENDIF.

    IF TG_TELA_SEL-VISAO IS INITIAL.
      RG_VISAO-SIGN    = 'I'.
      RG_VISAO-OPTION  = 'EQ'.
      RG_VISAO-LOW     = '01'.

      APPEND RG_VISAO.
      CLEAR: RG_VISAO.

      RG_VISAO-SIGN    = 'I'.
      RG_VISAO-OPTION  = 'EQ'.
      RG_VISAO-LOW     = '02'.

      APPEND RG_VISAO.
      CLEAR: RG_VISAO.

      RG_VISAO-SIGN    = 'I'.
      RG_VISAO-OPTION  = 'EQ'.
      RG_VISAO-LOW     = '03'.

      APPEND RG_VISAO.
      CLEAR: RG_VISAO.
    ELSE.
      RG_VISAO-SIGN    = 'I'.
      RG_VISAO-OPTION  = 'EQ'.
      RG_VISAO-LOW     = TG_TELA_SEL-VISAO.

      APPEND RG_VISAO.
      CLEAR: RG_VISAO.
    ENDIF.

    IF TG_TELA_SEL-FROM_DATE LE SY-DATUM
   AND TG_TELA_SEL-FROM_TIME LE SY-UZEIT.
      WL_IMEDIAT = C_X.
      IF TG_TELA_SEL-WAERS EQ 'BRL'
      OR TG_TELA_SEL-WAERS IS INITIAL.

        DELETE FROM ZIM08_REL_INV2 WHERE  VISAO  IN RG_VISAO AND
                                         ( GJAHR IN RG_GJAHR
                                        OR GJAHR EQ '0000' )
                                          AND ABUKRS IN RG_BUKRS
                                          AND POSNR IN RG_POSNR.
      ENDIF.

      IF TG_TELA_SEL-WAERS EQ 'USD'
      OR TG_TELA_SEL-WAERS IS INITIAL.
        DELETE FROM ZIM08_REL_INV_US WHERE VISAO IN RG_VISAO AND
                                          ( GJAHR IN RG_GJAHR
                                         OR GJAHR EQ '0000'
                                         OR GJAHR EQ SPACE )
                                         AND ABUKRS IN RG_BUKRS
                                         AND POSNR IN RG_POSNR.
      ENDIF.

    ELSE.
      CLEAR WL_IMEDIAT.
    ENDIF.

    LOOP AT RG_VISAO.
      CONCATENATE 'IM' RG_VISAO-LOW TG_TELA_SEL-BUKRS TG_TELA_SEL-POSNR TG_TELA_SEL-GJAHR TG_TELA_SEL-WAERS TG_TELA_SEL-MONAT INTO
    WL_JOBNAME SEPARATED BY '|'.


      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          JOBNAME  = WL_JOBNAME
        IMPORTING
          JOBCOUNT = JOBCOUNT.

      SUBMIT ZIM08_NEW
               USER TG_TELA_SEL-USERNAME VIA JOB WL_JOBNAME NUMBER JOBCOUNT
               WITH S_BUKRS IN RG_BUKRS AND RETURN
               WITH S_POSNR IN RG_POSNR
               WITH S_GJAHR IN RG_GJAHR
               WITH S_WAERS IN RG_WAERS
               WITH S_MONAT IN RG_MONAT
               WITH P_IMED  = WL_IMEDIAT
               WITH P_VISAO = RG_VISAO-LOW
               WITH P_TESTE = WL_TESTE.


      IF WL_IMEDIAT IS NOT INITIAL.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            JOBCOUNT  = JOBCOUNT
            JOBNAME   = WL_JOBNAME
            STRTIMMED = C_X.



      ELSE.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            JOBCOUNT  = JOBCOUNT
            JOBNAME   = WL_JOBNAME
            SDLSTRTDT = TG_TELA_SEL-FROM_DATE
            SDLSTRTTM = TG_TELA_SEL-FROM_TIME.

      ENDIF.

    ENDLOOP.
    TG_TELA_SEL-STATUS = ICON_YELLOW_LIGHT.
    INSERT LINES OF STYLE2 INTO TABLE TG_TELA_SEL-STYLE2.
    MODIFY TG_TELA_SEL TRANSPORTING STATUS STYLE2.

  ENDLOOP.
ENDFORM.                    " PROCESSA_LINHAS_JOB
