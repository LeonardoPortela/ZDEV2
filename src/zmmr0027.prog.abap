*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Sobral                                             &*
*& Data.....: 28/05/2013                                              &*
*& Descrição: Pré – Lançamento de Plumas                              &*
*& Transação: ZMM0049                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&                                                                    &*
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Report  ZMMR0027
*&---------------------------------------------------------------------*

REPORT  ZMMR0027.

** TABLES
**----------------------------------------------------------------------
TABLES: T001W, MARA, MKAL,    "STANDARD
        ZPPT0002.

** CONSTANTS
**----------------------------------------------------------------------
CONSTANTS: C_X              TYPE C VALUE 'X',
           C_P              TYPE C VALUE 'P',
           C_ADD(3)         TYPE C VALUE 'ADD',
           C_DEL(3)         TYPE C VALUE 'DEL',
           C_EXIT(4)        TYPE C VALUE 'EXIT',
           C_BACK(4)        TYPE C VALUE 'BACK',
           C_SAVE(4)        TYPE C VALUE 'SAVE',
*           c_proces(6)      TYPE c VALUE 'PROCES',
           C_CANCEL(6)      TYPE C VALUE 'CANCEL',
           C_REFRESH(7)     TYPE C VALUE 'REFRESH',
           C_SEARCH(6)      TYPE C VALUE 'SEARCH',
           C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE',
           C_DEATE(8)       TYPE C VALUE 'BT_DEATE',
           C_LANCAMENTO(10) TYPE C VALUE 'LANCAMENTO',
           C_RELATORIO(9)   TYPE C VALUE 'RELATORIO',
           C_ENTER(4)       TYPE C VALUE 'ENTE',
           C_RESP_SIM(4)    TYPE C VALUE 'OPT1',
           C_RESP_NAO(4)    TYPE C VALUE 'OPT2',
           C_RESP_CANCEL(4) TYPE C VALUE 'INFO'.

** TYPES
**----------------------------------------------------------------------
TYPES:  BEGIN OF TY_TC_ZPPT0002,
          ZSEL(1),
          ACHARG      TYPE ZPPT0002-ACHARG,
          MENGE       TYPE ZPPT0002-MENGE,
          CHARG       TYPE ZPPT0002-CHARG,
          STATUS      TYPE ZPPT0002-STATUS,
        END OF TY_TC_ZPPT0002,

        BEGIN OF TY_SAIDA,
          MARK(1),
          ROW         TYPE I,
          ACHARG      TYPE ZPPT0002-ACHARG,
          WERKS       TYPE ZPPT0002-WERKS,
          VERID       TYPE ZPPT0002-VERID,
          MATNR       TYPE ZPPT0002-MATNR,
          MENGE       TYPE ZPPT0002-MENGE,
          CHARG       TYPE ZPPT0002-CHARG,
          BUDAT       TYPE ZPPT0002-BUDAT,
          STATUS      TYPE ZPPT0002-STATUS,
          DEL(1)      TYPE C,
        END OF TY_SAIDA.

** INTERNAL TABLES
**----------------------------------------------------------------------
DATA: IT_ZPPT0002     TYPE TABLE OF ZPPT0002,
      GT_TC_ZPPT0002  TYPE TABLE OF TY_TC_ZPPT0002,
      IT_SAIDA        TYPE TABLE OF TY_SAIDA,
      IT_MARA         TYPE TABLE OF MARA,
      IT_T001W        TYPE TABLE OF T001W,
      IT_MKAL         TYPE TABLE OF MKAL.

** WORK AREAS
**----------------------------------------------------------------------
DATA: WA_ZPPT0002     TYPE ZPPT0002,
      WG_TC_ZPPT0002  TYPE TY_TC_ZPPT0002,
      WA_SAIDA        TYPE TY_SAIDA,
      WA_MARA         TYPE MARA,
      WA_T001W        TYPE T001W,
      WA_MKAL         TYPE MKAL,
      WA_ZPPT0003     TYPE ZPPT0003,
      WG_0002         TYPE ZPPT0002.

** VARIABLES
**----------------------------------------------------------------------
DATA: VG_DE           TYPE I,
      VG_ATE          TYPE I,
      VG_MSG(30)      TYPE C,
      VG_INIT,
      WG_DISPLAY,
      VG_CONT         TYPE I,
      VG_RESPOSTA(1),
      V_PESO_DESC     TYPE ZPPT0003-PESO_DESC,
      S_PESO_DESC(10),
      GV_NUM_REG      TYPE I.

***** Funcao de Z_DOC_CHECK_NEW
DATA: X_FIELD(30),
      WG_OBJ(40),
      WG_MENSAGEM(30).
DATA: TG_MSG_RET      TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE,
      WG_CELL         TYPE LVC_S_CELL,
      TG_CELL         TYPE LVC_T_CELL.

***** SCREEN
DATA: ACHARG_D        TYPE ZPPT0002-ACHARG,
      ACHARG_A        TYPE ZPPT0002-ACHARG.

CONTROLS: TC_ZPPT0002 TYPE TABLEVIEW USING SCREEN 0100.

DATA: G_TC_ZPPT0002_LINES LIKE SY-LOOPC,
      OK_CODE             LIKE SY-UCOMM,
      GV_RESPOSTA_EXCLUIR TYPE C.

*Declaration for toolbar buttons
DATA : TY_TOOLBAR TYPE STB_BUTTON.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
*Class definition for ALV toolbar
CLASS: LCL_ALV_TOOLBAR      DEFINITION DEFERRED.

DATA: GRID1                 TYPE REF TO CL_GUI_ALV_GRID,
      CONTAINER1            TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBG_TOOLBAR           TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER  TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.

DATA: T_FIELDCATALOG        TYPE LVC_T_FCAT,
      W_FIELDCATALOG        TYPE LVC_S_FCAT,
      TG_SELECTEDCELL       TYPE LVC_T_CELL,
      WG_SELECTEDCELL       TYPE LVC_S_CELL,
      WA_LAYOUT             TYPE LVC_S_LAYO,
      WA_STABLE             TYPE LVC_S_STBL,
      WA_STYLE              TYPE LVC_S_STYL,
      STYLE2                TYPE LVC_T_STYL WITH HEADER LINE.

DATA: BEGIN OF GT_VALUES OCCURS 0,
        DOMVALUE_L  TYPE DOMVALUE_L,
        DDTEXT      TYPE VAL_TEXT,
      END OF GT_VALUES.

** SELECTION SCREEN
**----------------------------------------------------------------------
**SELECTION-SCREEN BEGIN OF BLOCK one WITH FRAME TITLE text-t01.
**
**SELECT-OPTIONS:
**  s_werks   FOR zppt0002-werks    OBLIGATORY NO-EXTENSION NO INTERVALS,
**  s_matnr   FOR zppt0002-matnr    OBLIGATORY NO-EXTENSION NO INTERVALS,
**  s_verid   FOR zppt0002-verid    OBLIGATORY NO-EXTENSION NO INTERVALS,
**  s_budat   FOR zppt0002-budat    OBLIGATORY NO-EXTENSION NO INTERVALS.
**
**SELECTION-SCREEN END OF BLOCK one.

INITIALIZATION.
  PERFORM: F_LIMPA_CAMPOS.
  CALL SCREEN 100.

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
        IMPORTING  E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.

*    CLASS-METHODS:
*      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
*        IMPORTING  e_row_id e_column_id.

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
        IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY.

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

**    BOTAO ADD
    TY_TOOLBAR-ICON       = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION   = C_ADD.

*    IF wg_display IS INITIAL.
*      ty_toolbar-disabled = space.
*    ELSE.
    TY_TOOLBAR-DISABLED = 1.
*    ENDIF.

    TY_TOOLBAR-BUTN_TYPE  = 0.

    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

**    BOTAO DEL
    TY_TOOLBAR-ICON       = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION   = C_DEL.

    IF WG_DISPLAY IS INITIAL.
      TY_TOOLBAR-DISABLED = SPACE.
    ELSE.
      TY_TOOLBAR-DISABLED = 1.
    ENDIF.

    TY_TOOLBAR-BUTN_TYPE  = 0.

    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.
    CLEAR: WA_SAIDA.

    CASE E_UCOMM.
      WHEN C_ADD.
*        wa_saida-status = c_p.
*        wa_saida-budat  = sy-datum.
*        APPEND wa_saida TO it_saida.
      WHEN C_DEL.
        CALL METHOD GRID1->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.

        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
          CASE WA_SAIDA-DEL.
            WHEN C_X.
              WA_SAIDA-DEL  = ''.
            WHEN ''.
              WA_SAIDA-DEL  = C_X.
            WHEN OTHERS.
          ENDCASE.

          MODIFY IT_SAIDA FROM WA_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX
            TRANSPORTING DEL.
        ENDLOOP.
    ENDCASE.

    PERFORM F_VERIFICA_ERROS_200.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '200'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_POPUP       = 1
*        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "zm_handle_user_command
ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD ON_DOUBLE_CLICK.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD ON_DATA_CHANGED.
**    DATA: ls_good  TYPE lvc_s_modi,
**          lv_value TYPE lvc_value.

**    CALL METHOD er_data_changed->modify_cell
**      EXPORTING
**        i_row_id    = ls_good-row_id
**        i_fieldname = ls_good-fieldname
**        i_value     = lv_value.

**    PERFORM f_verifica_erros_200.
**    CALL FUNCTION 'Z_DOC_CHECK_NEW'
**      EXPORTING
**        i_screen      = '200'
**        i_show        = space
**        i_repid       = sy-repid
**        i_popup       = 1
***        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
***        i_set_field   = 'X_FIELD'
**      IMPORTING
**        e_messagem    = wg_mensagem
**      TABLES
**        it_msgs       = tg_msg_ret.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD ON_DATA_CHANGED_FINISHED.

    PERFORM F_VERIFICA_ERROS_200.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '200'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_POPUP       = 1
*        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*        i_set_field   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
**
**    CALL METHOD grid1->refresh_table_display
**      EXPORTING
**        is_stable = wa_stable.

  ENDMETHOD.                    "on_data_changed_finisheD

  METHOD ON_BUTTON_CLICK.

**    CALL METHOD grid1->refresh_table_display
**      EXPORTING
**        is_stable = wa_stable.

  ENDMETHOD.                    "on_button_click

  METHOD ON_ONF4.
**    TYPES: BEGIN OF tyl_field,
**            tabname TYPE dd03l-tabname,       "Nome da tabela
**            fieldname TYPE dd03l-fieldname,   "Nome de campo
**            s(1) TYPE c,
**           END OF tyl_field,
**
**           BEGIN OF tyl_value,
**            tabname TYPE dd03l-tabname,       "Nome da tabela
**            fieldname TYPE dd03l-fieldname,   "Nome de campo
**            char79(79) TYPE c,
**           END OF tyl_value.
**
**    DATA: BEGIN OF wl_cultura,
**                field(50),
**          END OF wl_cultura.
**
**    DATA: BEGIN OF wl_umb,
**                field(50),
**          END OF wl_umb.
**
**    DATA: tl_cultura    LIKE TABLE OF wl_cultura.
**
**      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
**        EXPORTING
***        cucol                     = '3'
**          fieldname                 = 'CULTURA'
**          tabname                   = 'ZSDT0038'
**        IMPORTING
**          index                     = wl_index
**          select_value              = wl_char
**        TABLES
**          fields                    = tl_field
**          select_values             = tl_value
**          valuetab                  = tl_cultura
**        EXCEPTIONS
**          field_not_in_ddic         = 001
**          more_then_one_selectfield = 002
**          no_selectfield            = 003.
**
****** Método de atualização de dados na Tela
**    CALL METHOD grid1->refresh_table_display
**      EXPORTING
**        is_stable = wa_stable.

  ENDMETHOD.                                                "on_ONF4
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  PERFORM F_VERIFICA_ERROS_100.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN      = '100'
      I_SHOW        = SPACE
      I_REPID       = SY-REPID
      I_POPUP       = 0
*      i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
      I_SET_FIELD   = 'X_FIELD'
    IMPORTING
      E_MESSAGEM    = WG_MENSAGEM
    TABLES
      IT_MSGS       = TG_MSG_RET.

  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD.
  ENDIF.

  SET PF-STATUS 'Z001'.
  SET TITLEBAR  'Z001'.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM."OK_CODE.
    WHEN C_SAVE.
      PERFORM F_VERIFICA_ERROS_100.
      IF TG_MSG_RET[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = SPACE     "c_x
            I_REPID       = SY-REPID
            I_POPUP       = 0
            I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
            I_SET_FIELD   = 'X_FIELD'
            I_SET_CELL    = 'WG_CELL'
            I_SET_OBJ     = 'WG_OBJ'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ELSE.
        PERFORM:  F_ORGANIZA_DADOS_100,
                  F_GRAVA_DADOS_100.
      ENDIF.
    WHEN C_REFRESH.
      PERFORM: F_LIMPA_CAMPOS.
    WHEN C_SEARCH.

    WHEN C_DEATE.
      PERFORM: F_PREENCHE_TC_ZPPT0002.
*    WHEN c_lancamento.
*      CALL SCREEN 100.
    WHEN C_RELATORIO.
      CHECK ZPPT0002-WERKS IS NOT INITIAL AND ZPPT0002-BUDAT IS NOT INITIAL.
      CALL SCREEN 200.
    WHEN C_BACK.
      LEAVE TO SCREEN 0.
    WHEN C_CANCEL.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 100.
    WHEN C_EXIT.
      LEAVE PROGRAM.
    WHEN C_SHOW_MSGRE.
*      PERFORM f_verifica_erros_100.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '100'
          I_SHOW        = C_X
          I_REPID       = SY-REPID
          I_POPUP       = 0
          I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
          I_SET_FIELD   = 'X_FIELD'
          I_SET_CELL    = 'WG_CELL'
          I_SET_OBJ     = 'WG_OBJ'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.

*      IF x_field IS NOT INITIAL.
*        SET CURSOR FIELD x_field.
*      ENDIF.
*      WHEN OTHERS.
*        PERFORM f_verifica_erros_100.
    WHEN 'EXCLUIR'.
      CLEAR: OK_CODE, SY-UCOMM.
      PERFORM F_EXCLUIR_FARDO.
    WHEN 'IMPORTAR'.
      CLEAR: OK_CODE, SY-UCOMM.
      PERFORM F_IMPORTAR.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_TC_ZPPT0002
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_PREENCHE_TC_ZPPT0002.
  DATA: POS TYPE I.

  CHECK: ACHARG_D IS NOT INITIAL,
         ACHARG_A IS NOT INITIAL.
*         ACHARG_D+2 < ACHARG_A+2.

  CLEAR:    WG_TC_ZPPT0002, GT_TC_ZPPT0002, VG_DE, VG_ATE.
  REFRESH:  GT_TC_ZPPT0002.

  SEARCH ACHARG_D FOR './.'.
  POS = SY-FDPOS + 1.
  VG_DE = ACHARG_D+POS.

  SEARCH ACHARG_A FOR './.'.
  POS = SY-FDPOS + 1.
  VG_ATE = ACHARG_A+POS.

  WHILE VG_DE <= VG_ATE.
    WG_TC_ZPPT0002-ACHARG = VG_DE.
    SHIFT WG_TC_ZPPT0002-ACHARG LEFT DELETING LEADING SPACE.
    CONCATENATE ACHARG_D(POS) WG_TC_ZPPT0002-ACHARG INTO WG_TC_ZPPT0002-ACHARG.

    APPEND WG_TC_ZPPT0002 TO GT_TC_ZPPT0002.

    VG_DE = VG_DE + 1.
  ENDWHILE.

  CLEAR:  VG_DE, VG_ATE.  "wg_tc_zppt0002
ENDFORM.                    " F_PREENCHE_TC_ZPPT0002

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS_100
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_ORGANIZA_DADOS_100.
*  DELETE gt_tc_zppt0002 WHERE acharg  IS INITIAL
*                           OR menge   IS INITIAL
*                           OR charg   IS INITIAL.
  DATA: W_ANSWER,
        W_MSGDESC(60).

  W_MSGDESC = 'Descontar peso das embalagens, arame + saco  ??'.
  CLEAR  S_PESO_DESC.
  SELECT SINGLE *
    FROM ZPPT0003
    INTO WA_ZPPT0003
    WHERE WERKS   = ZPPT0002-WERKS
    AND   VERID   = ZPPT0002-VERID
    AND   MATNR   = ZPPT0002-MATNR.

  IF SY-SUBRC = 0.
    S_PESO_DESC = WA_ZPPT0003-PESO_DESC.
    CONDENSE S_PESO_DESC NO-GAPS.
    CONCATENATE W_MSGDESC '('  INTO W_MSGDESC SEPARATED BY SPACE.
    CONCATENATE W_MSGDESC S_PESO_DESC ') KG'  INTO W_MSGDESC.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
*         TITLEBAR                    = ' '
*         DIAGNOSE_OBJECT             = ' '
         TEXT_QUESTION               = W_MSGDESC
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

  ENDIF.

  CLEAR  V_PESO_DESC.
  SELECT SINGLE *
    FROM ZPPT0003
    INTO WA_ZPPT0003
    WHERE WERKS   = ZPPT0002-WERKS
    AND   VERID   = ZPPT0002-VERID
    AND   MATNR   = ZPPT0002-MATNR.

  IF SY-SUBRC = 0.
    V_PESO_DESC = WA_ZPPT0003-PESO_DESC.
    IF V_PESO_DESC LE 0 AND W_ANSWER = '1'.
      MESSAGE 'Peso de desconto inválido' TYPE 'I'.
      EXIT.
    ENDIF.
  ELSEIF W_ANSWER = '1'.
    MESSAGE 'Peso de desconto não cadastrado.' TYPE 'I'.
    EXIT.
  ENDIF.

  CHECK GT_TC_ZPPT0002 IS NOT INITIAL.
  SORT GT_TC_ZPPT0002 BY ACHARG.
  READ TABLE GT_TC_ZPPT0002 INTO WG_TC_ZPPT0002 WITH KEY ACHARG = '' BINARY SEARCH.
  CHECK SY-SUBRC <> 0.
  SORT GT_TC_ZPPT0002 BY MENGE.
  READ TABLE GT_TC_ZPPT0002 INTO WG_TC_ZPPT0002 WITH KEY MENGE = '' BINARY SEARCH.
  CHECK SY-SUBRC <> 0.
  SORT GT_TC_ZPPT0002 BY CHARG.
  READ TABLE GT_TC_ZPPT0002 INTO WG_TC_ZPPT0002 WITH KEY CHARG = '' BINARY SEARCH.
  CHECK SY-SUBRC <> 0.

  PERFORM F_PROGRESS USING 35 'Montando tabela.'.

  CLEAR: WG_TC_ZPPT0002, WA_ZPPT0002.
  SORT GT_TC_ZPPT0002 BY ACHARG.

  LOOP AT GT_TC_ZPPT0002 INTO WG_TC_ZPPT0002.
    WA_ZPPT0002-ACHARG  = WG_TC_ZPPT0002-ACHARG.
    IF W_ANSWER = '1'.
      WA_ZPPT0002-MENGE   = WG_TC_ZPPT0002-MENGE - V_PESO_DESC.
    ELSE.
      WA_ZPPT0002-MENGE   = WG_TC_ZPPT0002-MENGE.
    ENDIF.
    WA_ZPPT0002-CHARG   = WG_TC_ZPPT0002-CHARG.
    WA_ZPPT0002-STATUS  = C_P.

    WA_ZPPT0002-WERKS   = ZPPT0002-WERKS.
    WA_ZPPT0002-VERID   = ZPPT0002-VERID.
    WA_ZPPT0002-MATNR   = ZPPT0002-MATNR.
    WA_ZPPT0002-BUDAT   = ZPPT0002-BUDAT.

    APPEND WA_ZPPT0002 TO IT_ZPPT0002.
  ENDLOOP.

  CHECK: IT_ZPPT0002 IS NOT INITIAL.
  SORT IT_ZPPT0002 BY ACHARG.
  DELETE ADJACENT DUPLICATES FROM IT_ZPPT0002 COMPARING ACHARG.
ENDFORM.                    " F_ORGANIZA_DADOS_100

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS_100
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_GRAVA_DADOS_100.
  SORT GT_TC_ZPPT0002 BY ACHARG.

  CHECK: IT_ZPPT0002 IS NOT INITIAL.
  PERFORM F_PROGRESS USING 70 'Gravando dados.'.

  MODIFY ZPPT0002 FROM TABLE IT_ZPPT0002.

  IF SY-SUBRC IS INITIAL.
    COMMIT WORK.
    PERFORM: F_LIMPA_CAMPOS.
*    CONCATENATE 'Dados inseridos com sucesso na tabela' 'ZPPT0002'
*           INTO vg_msg SEPARATED BY space.
    MESSAGE 'Dados inseridos com sucesso na tabela ZPPT0002' TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    CONCATENATE 'Não foi possivel inserir os dados na tabela' 'ZPPT0002'
           INTO VG_MSG SEPARATED BY SPACE.
*    MESSAGE 'Não foi possivel inserir os dados na tabela ZPPT0002' TYPE 'I'.
  ENDIF.
ENDFORM.                    " F_GRAVA_DADOS_100

*&---------------------------------------------------------------------*
*&      Form  F_PROGRESS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_PROGRESS  USING VF_PERCEN VF_TEXT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = VF_PERCEN   " Veloc do relogio em %
      TEXT       = VF_TEXT.    " Texto que aparecerá.
ENDFORM.                    " F_PROGRESS

*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS_100
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS_100.
  DATA: LV_POS TYPE I.

  IF OK_CODE <> C_SHOW_MSGRE.
    CLEAR:    TG_MSG_RET.
    REFRESH:  TG_MSG_RET.

    CASE OK_CODE.
      WHEN C_SAVE OR C_ENTER.
****  Centro (WERKS)
        IF ZPPT0002-WERKS IS INITIAL.
          MOVE: 'ZPPT0002-WERKS'  TO TG_MSG_RET-FIELD.
          CONCATENATE 'Centro:' 'Obrigatório.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          SELECT SINGLE * FROM T001W WHERE WERKS = ZPPT0002-WERKS.
          IF SY-SUBRC <> 0.
            MOVE: 'ZPPT0002-WERKS'  TO TG_MSG_RET-FIELD.
            CONCATENATE 'Centro:' ZPPT0002-WERKS 'não encontrado.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.

****  Material (MATNR)
        IF ZPPT0002-MATNR IS INITIAL.
          MOVE: 'ZPPT0002-MATNR'  TO TG_MSG_RET-FIELD.
          CONCATENATE 'Material:' 'Obrigatório.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          SELECT SINGLE * FROM MARA WHERE MATNR = ZPPT0002-MATNR.
          IF SY-SUBRC <> 0.
            MOVE: 'ZPPT0002-MATNR'  TO TG_MSG_RET-FIELD.
            CONCATENATE 'Material:' ZPPT0002-MATNR 'não encontrado.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.

****  Maquina (VERID)
        IF ZPPT0002-VERID IS INITIAL.
          MOVE: 'ZPPT0002-VERID'  TO TG_MSG_RET-FIELD.
          CONCATENATE 'Maquina:' 'Obrigatória.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          IF ZPPT0002-WERKS IS INITIAL OR ZPPT0002-MATNR IS INITIAL.
            MOVE: 'ZPPT0002-VERID'  TO TG_MSG_RET-FIELD.
            CONCATENATE 'Maquina:' ZPPT0002-VERID 'não encontrada.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ELSE.
            SELECT SINGLE * FROM MKAL WHERE MATNR = ZPPT0002-MATNR AND WERKS = ZPPT0002-WERKS AND VERID = ZPPT0002-VERID.
            IF SY-SUBRC <> 0.
              MOVE: 'ZPPT0002-VERID'  TO TG_MSG_RET-FIELD.
              CONCATENATE 'Maquina:' ZPPT0002-VERID 'não encontrada.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
              APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
            ENDIF.
          ENDIF.
        ENDIF.

        IF ZPPT0002-BUDAT IS INITIAL.
          MOVE: 'ZPPT0002-BUDAT'  TO TG_MSG_RET-FIELD.
          CONCATENATE 'Data Lançamento:' 'Obrigatório.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          "validar data
        ENDIF.

**** Verificaçao de gravaçao na tabela
        IF VG_MSG IS NOT INITIAL.
          MOVE: 'ZPPT0002-WERKS'  TO TG_MSG_RET-FIELD,
                VG_MSG            TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ENDIF.

        IF GT_TC_ZPPT0002[] IS INITIAL.
          MOVE: 'WG_TC_ZPPT0002-ACHARG'  TO TG_MSG_RET-FIELD.
          CONCATENATE 'Preencher:' 'Num. Fardo,' 'Peso,' 'Fardão Origem.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          SORT GT_TC_ZPPT0002 BY ACHARG.
          READ TABLE GT_TC_ZPPT0002 INTO WG_TC_ZPPT0002 WITH KEY ACHARG = '' BINARY SEARCH.
          IF SY-SUBRC <> 0.
            SORT GT_TC_ZPPT0002 BY MENGE.
            READ TABLE GT_TC_ZPPT0002 INTO WG_TC_ZPPT0002 WITH KEY MENGE = '' BINARY SEARCH.
            IF SY-SUBRC <> 0.
              SORT GT_TC_ZPPT0002 BY CHARG.
              READ TABLE GT_TC_ZPPT0002 INTO WG_TC_ZPPT0002 WITH KEY CHARG = '' BINARY SEARCH.
            ENDIF.
          ENDIF.

          IF SY-SUBRC = 0.
            MOVE: 'WG_TC_ZPPT0002-ACHARG'  TO TG_MSG_RET-FIELD.
            CONCATENATE 'Preencher:' 'Num. Fardo,' 'Peso,' 'Fardão Origem.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ELSE.
            CLEAR: WA_ZPPT0002, IT_ZPPT0002[].

            SELECT * FROM ZPPT0002
              INTO TABLE IT_ZPPT0002
              FOR ALL ENTRIES IN GT_TC_ZPPT0002
            WHERE ACHARG EQ GT_TC_ZPPT0002-ACHARG.

            IF IT_ZPPT0002[] IS NOT INITIAL.
              LOOP AT IT_ZPPT0002 INTO WA_ZPPT0002.
                MOVE: 'WG_TC_ZPPT0002-ACHARG'  TO TG_MSG_RET-FIELD.
                CONCATENATE 'Num. Fardo:' WA_ZPPT0002-ACHARG ', já existe.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
                APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
              ENDLOOP.
            ENDIF.

            CLEAR: WA_ZPPT0002, IT_ZPPT0002[].
          ENDIF.

          SORT GT_TC_ZPPT0002 BY ACHARG.
        ENDIF.
      WHEN C_DEATE.
**** Validacao De/Ate
        IF ACHARG_D IS NOT INITIAL AND ACHARG_A IS NOT INITIAL.
          SEARCH ACHARG_D FOR './.'.
          LV_POS = SY-FDPOS + 1.
          VG_DE = ACHARG_D+LV_POS.

          SEARCH ACHARG_A FOR './.'.
          LV_POS = SY-FDPOS + 1.
          VG_ATE = ACHARG_A+LV_POS.

*          IF ACHARG_D+2 >= ACHARG_A+2.
          IF VG_DE >= VG_ATE.
            MOVE: 'ACHARG_D'  TO TG_MSG_RET-FIELD.
            CONCATENATE '"DE"' 'não pode ser maior ou igual a' '"ATE".' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ENDIF.
        ELSE.
          IF ACHARG_D IS INITIAL.
            MOVE: 'ACHARG_D'  TO TG_MSG_RET-FIELD.
          ELSE.
            MOVE: 'ACHARG_A'  TO TG_MSG_RET-FIELD.
          ENDIF.

          CONCATENATE 'Preencher os campos' '"DE" / "ATE".' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ENDIF.
      WHEN C_RELATORIO.
****  Centro (WERKS)
        IF ZPPT0002-WERKS IS INITIAL.
          MOVE: 'ZPPT0002-WERKS'  TO TG_MSG_RET-FIELD.
          CONCATENATE 'Centro:' 'Obrigatório.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          SELECT SINGLE * FROM T001W WHERE WERKS = ZPPT0002-WERKS.
          IF SY-SUBRC <> 0.
            MOVE: 'ZPPT0002-WERKS'  TO TG_MSG_RET-FIELD.
            CONCATENATE 'Centro:' ZPPT0002-WERKS 'não encontrado.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.

        IF ZPPT0002-BUDAT IS INITIAL.
          MOVE: 'ZPPT0002-BUDAT'  TO TG_MSG_RET-FIELD.
          CONCATENATE 'Data Lançamento:' 'Obrigatório.' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          "validar data
        ENDIF.
*    	WHEN OTHERS.
    ENDCASE.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
*        i_show        = C_X
        I_REPID       = SY-REPID
*        i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
  ENDIF.

ENDFORM.                    " F_VERIFICA_ERROS_100

*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_LIMPA_CAMPOS.
  CLEAR: ZPPT0002-WERKS,  ZPPT0002-MATNR,   ZPPT0002-VERID,   ZPPT0002-BUDAT,
         ACHARG_D,        ACHARG_A,         GT_TC_ZPPT0002,   WG_TC_ZPPT0002,
         OK_CODE.

  REFRESH: GT_TC_ZPPT0002.
ENDFORM.                    " F_LIMPA_CAMPOS

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_ZPPT0002'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TC_ZPPT0002_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE GT_TC_ZPPT0002 LINES TC_ZPPT0002-LINES.
  GV_NUM_REG = TC_ZPPT0002-LINES.
ENDMODULE.                    "TC_ZPPT0002_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_ZPPT0002'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TC_ZPPT0002_GET_LINES OUTPUT.
  G_TC_ZPPT0002_LINES = SY-LOOPC.
ENDMODULE.                    "TC_ZPPT0002_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_ZPPT0002'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE TC_ZPPT0002_MODIFY INPUT.
  MODIFY GT_TC_ZPPT0002 FROM WG_TC_ZPPT0002
    INDEX TC_ZPPT0002-CURRENT_LINE.
ENDMODULE.                    "TC_ZPPT0002_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC_ZPPT0002'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TC_ZPPT0002_MARK INPUT.
  DATA: G_TC_ZPPT0002_WA2 LIKE LINE OF GT_TC_ZPPT0002.

  IF TC_ZPPT0002-LINE_SEL_MODE = 1
  AND WG_TC_ZPPT0002-ZSEL = 'X'.
    LOOP AT GT_TC_ZPPT0002 INTO G_TC_ZPPT0002_WA2 WHERE ZSEL = 'X'.
      G_TC_ZPPT0002_WA2-ZSEL = ''.
      MODIFY GT_TC_ZPPT0002 FROM G_TC_ZPPT0002_WA2
        TRANSPORTING ZSEL.
    ENDLOOP.
  ENDIF.

  MODIFY GT_TC_ZPPT0002 FROM WG_TC_ZPPT0002
    INDEX TC_ZPPT0002-CURRENT_LINE
    TRANSPORTING ZSEL.
ENDMODULE.                    "TC_ZPPT0002_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_ZPPT0002'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TC_ZPPT0002_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TC_ZPPT0002'
                              'GT_TC_ZPPT0002'
                              'ZSEL'
                     CHANGING OK_CODE.
*  sy-ucomm = ok_code.
ENDMODULE.                    "TC_ZPPT0002_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM USER_OK_TC USING    P_TC_NAME    TYPE DYNFNAM
                         P_TABLE_NAME
                         P_MARK_NAME
                CHANGING P_OK         LIKE SY-UCOMM.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: L_OK              TYPE SY-UCOMM,
        L_OFFSET          TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH P_OK FOR P_TC_NAME.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  L_OFFSET = STRLEN( P_TC_NAME ) + 1.
  L_OK = P_OK+L_OFFSET.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE L_OK.
    WHEN 'INSR'.                      "insert row
      PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                        P_TABLE_NAME.
      CLEAR P_OK.

    WHEN 'DELE'.                      "delete row
      PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                        P_TABLE_NAME
                                        P_MARK_NAME.
      CLEAR P_OK.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                            L_OK.
      CLEAR P_OK.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                        P_TABLE_NAME
                                        P_MARK_NAME   .
      CLEAR P_OK.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                          P_TABLE_NAME
                                          P_MARK_NAME .
      CLEAR P_OK.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM FCODE_INSERT_ROW USING P_TC_NAME     TYPE DYNFNAM
                            P_TABLE_NAME.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_LINES_NAME       LIKE FELD-NAME.
  DATA L_SELLINE          LIKE SY-STEPL.
  DATA L_LASTLINE         TYPE I.
  DATA L_LINE             TYPE I.
  DATA L_TABLE_NAME       LIKE FELD-NAME.
  FIELD-SYMBOLS <TC>      TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>   TYPE STANDARD TABLE.
  FIELD-SYMBOLS <LINES>   TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_LINES_NAME.
  ASSIGN (L_LINES_NAME) TO <LINES>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE L_SELLINE.
  IF SY-SUBRC <> 0.                   " append line to table
    L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line                                               *
    IF L_SELLINE > <LINES>.
      <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
    ELSE.
      <TC>-TOP_LINE = 1.
    ENDIF.
  ELSE.                               " insert line into table
    L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
    L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
  <TC>-LINES = <TC>-LINES + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE L_LINE.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM FCODE_DELETE_ROW USING P_TC_NAME     TYPE DYNFNAM
                            P_TABLE_NAME
                            P_MARK_NAME.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME           LIKE FELD-NAME.
  FIELD-SYMBOLS <TC>          TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>       TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

  LOOP AT <TABLE> ASSIGNING <WA>.
*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    IF <MARK_FIELD> = 'X'.
      DELETE <TABLE> INDEX SYST-TABIX.
      IF SY-SUBRC = 0.
        <TC>-LINES = <TC>-LINES - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                      P_OK.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TC_NEW_TOP_LINE     TYPE I.
  DATA L_TC_NAME             LIKE FELD-NAME.
  DATA L_TC_LINES_NAME       LIKE FELD-NAME.
  DATA L_TC_FIELD_NAME       LIKE FELD-NAME.
  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <LINES>      TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
  ASSIGN (L_TC_LINES_NAME) TO <LINES>.

*&SPWIZARD: is no line filled?                                         *
  IF <TC>-LINES = 0.
*&SPWIZARD: yes, ...                                                   *
    L_TC_NEW_TOP_LINE = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        ENTRY_ACT             = <TC>-TOP_LINE
        ENTRY_FROM            = 1
        ENTRY_TO              = <TC>-LINES
        LAST_PAGE_FULL        = 'X'
        LOOPS                 = <LINES>
        OK_CODE               = P_OK
        OVERLAPPING           = 'X'
      IMPORTING
        ENTRY_NEW             = L_TC_NEW_TOP_LINE
      EXCEPTIONS
*        NO_ENTRY_OR_PAGE_ACT  = 01
*        NO_ENTRY_TO           = 02
*        NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS                = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD L_TC_FIELD_NAME
             AREA  L_TC_NAME.

  IF SYST-SUBRC = 0.
    IF L_TC_NAME = P_TC_NAME.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.

ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA L_TABLE_NAME           LIKE FELD-NAME.
  FIELD-SYMBOLS <TC>          TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>       TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <TABLE> ASSIGNING <WA>.
*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME           LIKE FELD-NAME.
  FIELD-SYMBOLS <TC>          TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>       TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <TABLE> ASSIGNING <WA>.
*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Module  TC_ZPPT0002_CHANGE_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_ZPPT0002_CHANGE_SCREEN OUTPUT.
  CASE SY-UCOMM.
    WHEN C_REFRESH.

    WHEN OTHERS.
      CHECK: GT_TC_ZPPT0002 IS INITIAL.
  ENDCASE.

  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'WG_TC_ZPPT0002-ACHARG'.
        SCREEN-INPUT     = 0.
*        screen-output    = 0.
*        screen-active    = 0.
        MODIFY SCREEN.
      WHEN 'WG_TC_ZPPT0002-MENGE'.
        SCREEN-INPUT     = 0.
*        screen-output    = 0.
*        screen-active    = 0.
        MODIFY SCREEN.
      WHEN 'WG_TC_ZPPT0002-CHARG'.
        SCREEN-INPUT     = 0.
*        screen-output    = 0.
*        screen-active    = 0.
        MODIFY SCREEN.
      WHEN 'WG_TC_ZPPT0002-STATUS' OR 'TXT-STATUS'.
        SCREEN-INPUT     = 0.
        SCREEN-OUTPUT    = 0.
        SCREEN-ACTIVE    = 0.
        SCREEN-INVISIBLE = 1.
        SCREEN-REQUIRED  = 0.
*      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
ENDMODULE.                 " TC_ZPPT0002_CHANGE_SCREEN  OUTPUT

************* TELA 0200*************************************************
************************************************************************
*&---------------------------------------------------------------------*
*&      Module  INICIO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INICIO OUTPUT.
  DATA: WL_VIEW_NAME  TYPE OCUS-TABLE VALUE 'ZPPT0002',
        TL_RANGETAB   TYPE TABLE OF VIMSELLIST,
        WL_LOCKUSER   TYPE SY-UNAME,
        ANSWER.

  IF VG_INIT IS INITIAL.
    CLEAR: WG_DISPLAY.
    REFRESH: TG_MSG_RET.

    CALL FUNCTION 'ENQUEUE_EZPPT0002'
      EXPORTING
        MODE_ZPPT0002        = 'E'
        MANDT                = SY-MANDT
*      ACHARG               =
        WERKS                = ZPPT0002-WERKS
*      X_ACHARG             = ' '
*      X_WERKS              = ' '
*      _SCOPE               = '2'
*      _WAIT                = ' '
*      _COLLECT             = ' '
       EXCEPTIONS
         FOREIGN_LOCK         = 1
         SYSTEM_FAILURE       = 2
         OTHERS               = 3.

    CASE SY-SUBRC.
      WHEN 1.
        WL_LOCKUSER = SY-MSGV1(12).     "HCG sy-msgv1 lost at popup call
        CALL FUNCTION 'POPUP_TO_DECIDE_LOCKED_DATA'
          EXPORTING
            I_USER               = SY-MSGV1(12)
*            I_START_COLUMN       = 9
*            I_START_ROW          = 9
          IMPORTING
            E_ANSWER             = ANSWER.
        IF ANSWER = '2'.
          MESSAGE S049(SV) WITH WL_LOCKUSER RAISING FOREIGN_LOCK.
          CALL SCREEN 100.
        ELSEIF ANSWER = '1'.
          MOVE: C_X TO WG_DISPLAY.
        ENDIF.
      WHEN 2 OR 3.
        MESSAGE E050(SV) WITH WL_VIEW_NAME RAISING SYSTEM_FAILURE.
    ENDCASE.

**    CALL FUNCTION 'VIEW_ENQUEUE'
**      EXPORTING
**        view_name        = wl_view_name
**        action           = 'E'
**        enqueue_mode     = 'E'
***        enqueue_range    = "header-subsetflag
**      TABLES
**        sellist          = tl_rangetab
**      EXCEPTIONS
**        foreign_lock     = 1
**        system_failure   = 2
**        table_not_found  = 5
**        client_reference = 7.
**    CASE sy-subrc.
**      WHEN 1.
**        wl_lockuser = sy-msgv1(12).     "HCG sy-msgv1 lost at popup call
**        CALL FUNCTION 'POPUP_TO_DECIDE_LOCKED_DATA'
**          EXPORTING
**            i_user               = sy-msgv1(12)
***            I_START_COLUMN       = 9
***            I_START_ROW          = 9
**          IMPORTING
**            e_answer             = answer.
**        IF answer = '2'.
**          MESSAGE s049(sv) WITH wl_lockuser RAISING foreign_lock.
**          EXIT.
**        ELSEIF answer = '1'.
**          MOVE: c_x TO wg_display.
**        ENDIF.
**      WHEN 2.
**        MESSAGE e050(sv) WITH wl_view_name RAISING system_failure.
**      WHEN 5.
**        MESSAGE e028(sv) WITH wl_view_name RAISING view_not_found.
**      WHEN 7.
**        MESSAGE e054(sv) WITH sy-mandt RAISING client_reference.
**    ENDCASE.

    PERFORM F_SELECIONA_DADOS_200.
    PERFORM F_ORGANIZA_DADOS_200.
    VG_INIT = C_X.
  ENDIF.
ENDMODULE.                 " INICIO  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIAR_OBJETOS OUTPUT.
  DATA: WL_REPID     TYPE SY-REPID,
        TL_FUNCTION  TYPE UI_FUNCTIONS,
        WL_FUNCTION  LIKE TL_FUNCTION WITH HEADER LINE,
        LT_F4        TYPE LVC_T_F4    WITH HEADER LINE,
        TL_FILTER    TYPE LVC_T_FILT,
        WL_FILTER    TYPE LVC_S_FILT.

  WL_REPID = SY-REPID.

  IF CONTAINER1 IS INITIAL.
    WA_LAYOUT-ZEBRA      = C_X.
*    wa_layout-cwidth_opt = c_x.
    WA_STABLE-ROW        = C_X.
    WA_LAYOUT-SEL_MODE   = 'C'.
    WA_LAYOUT-BOX_FNAME  = 'MARK'.
    WA_LAYOUT-NO_ROWMARK = C_X.

    CREATE OBJECT CONTAINER1
      EXPORTING
        REPID     = WL_REPID
        DYNNR     = '0200'
*        style     = container1->WS_MINIMIZEBOX
        SIDE      = CONTAINER1->DOCK_AT_TOP
        EXTENSION = 400.
*        metric    = 50.

    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CONTAINER1.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID1.

** Register event handler
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR           FOR GRID1.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND  FOR GRID1.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_MB_FILTER.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    PERFORM F_MONTAR_LAYOUT.
*    PERFORM f_build_dropdown.

*    wa_layout-ctab_fname = 'CELLCOLORS'.
*    wa_layout-stylefname = 'STYLE2'.

**    lt_f4-fieldname  = 'CULTURA'.
**    lt_f4-register   = 'X' .
**    lt_f4-getbefore  = 'X' .
**    append lt_f4 .
**
**    lt_f4-fieldname  = 'MEINS'.
**    lt_f4-register   = 'X' .
**    lt_f4-getbefore  = 'X' .
**    append lt_f4 .
**
**    wl_filter-fieldname = 'STATUS'.
**    wl_filter-sign      = 'I'.
**    wl_filter-option    = 'EQ'.
**    wl_filter-low       = 'P'.
**    APPEND wl_filter TO tl_filter.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_FILTER            = TL_FILTER
        IT_OUTTAB            = IT_SAIDA[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

**    call method grid1->register_f4_for_fields
**      exporting
**        it_f4 = lt_f4[].

    SET HANDLER:  LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK          FOR GRID1,
                  LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
                  LCL_EVENT_HANDLER=>ON_DATA_CHANGED          FOR GRID1,
                  LCL_EVENT_HANDLER=>ON_BUTTON_CLICK          FOR GRID1,
                  LCL_EVENT_HANDLER=>ON_ONF4                  FOR GRID1.
  ELSE.
    PERFORM F_MONTAR_LAYOUT.
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_FILTER            = TL_FILTER
        IT_OUTTAB            = IT_SAIDA[].

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
ENDMODULE.                 " CRIAR_OBJETOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  DATA: FCODE     TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.

  IF WG_DISPLAY IS NOT INITIAL.
    APPEND C_SAVE TO FCODE.
  ENDIF.

  PERFORM F_VERIFICA_ERROS_200.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN      = '200'
      I_SHOW        = SPACE
      I_REPID       = SY-REPID
      I_POPUP       = 1
*      i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*      i_set_field   = 'X_FIELD'
    IMPORTING
      E_MESSAGEM    = WG_MENSAGEM
    TABLES
      IT_MSGS       = TG_MSG_RET.

  IF WG_CELL IS NOT INITIAL.
    REFRESH: TG_CELL.
    APPEND WG_CELL TO TG_CELL.

    CALL METHOD GRID1->SET_SELECTED_CELLS"(wg_obj)    "(wg_msgs)=>set_selected_cells
        EXPORTING
          IT_CELLS = TG_CELL[].
  ENDIF.

  SET PF-STATUS 'Z001' EXCLUDING FCODE.
  SET TITLEBAR  'Z001'.
ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE SY-UCOMM.    "ok_code.
    WHEN C_SAVE.
      PERFORM: F_BT_SAVE.
    WHEN C_SHOW_MSGRE.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '200'
          I_SHOW        = C_X
          I_REPID       = SY-REPID
          I_POPUP       = 1
          I_SET_CELL    = 'WG_CELL'
          I_SET_OBJ     = 'WL_OBJ'
*          i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*          i_set_field   = 'X_FIELD'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.
    WHEN C_LANCAMENTO.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR      = 'Fardos Pré-lançados'
          TEXT_QUESTION = 'Desejar gravar as modificações'
          TEXT_BUTTON_1 = 'Sim'  "(001)
          TEXT_BUTTON_2 = 'Não'  "(002)
        IMPORTING
          ANSWER        = VG_RESPOSTA.

      CASE VG_RESPOSTA.
        WHEN '1'.   "Sim
          PERFORM:  F_BT_SAVE.
          CHECK TG_MSG_RET IS INITIAL.
          PERFORM:  F_SAIR USING C_LANCAMENTO.
        WHEN '2'.   "Não
          PERFORM:  F_SAIR USING C_LANCAMENTO.
        WHEN 'A'.   "Cancelar
      ENDCASE.

    WHEN C_BACK.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR      = 'Fardos Pré-lançados'
          TEXT_QUESTION = 'Desejar gravar as modificações'
          TEXT_BUTTON_1 = 'Sim'  "(001)
          TEXT_BUTTON_2 = 'Não'  "(002)
        IMPORTING
          ANSWER        = VG_RESPOSTA.

      CASE VG_RESPOSTA.
        WHEN '1'.   "Sim
          PERFORM:  F_BT_SAVE.
          CHECK TG_MSG_RET IS INITIAL.
          PERFORM:  F_SAIR USING C_BACK.
        WHEN '2'.   "Não
          PERFORM:  F_SAIR USING C_BACK.
        WHEN 'A'.   "Cancelar
      ENDCASE.

    WHEN C_CANCEL.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 200.
    WHEN C_EXIT.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR      = 'Fardos Pré-lançados'
          TEXT_QUESTION = 'Desejar gravar as modificações'
          TEXT_BUTTON_1 = 'Sim'  "(001)
          TEXT_BUTTON_2 = 'Não'  "(002)
        IMPORTING
          ANSWER        = VG_RESPOSTA.

      CASE VG_RESPOSTA.
        WHEN '1'.   "Sim
          PERFORM:  F_BT_SAVE.
          CHECK TG_MSG_RET IS INITIAL.
          PERFORM:  F_SAIR USING C_BACK.
        WHEN '2'.   "Não
          PERFORM:  F_SAIR USING C_EXIT..
        WHEN 'A'.   "Cancelar
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_MONTAR_LAYOUT.
  DATA: VL_ALV_CONT(10) TYPE C.
  VL_ALV_CONT = VG_CONT.

  REFRESH: T_FIELDCATALOG.

  PERFORM F_MONTAR_ESTRUTURA USING:
*   col_pos ref_tabname ref_fieldname tabname     field     scrtext_l         outputlen edit  sum   emphasize
*    0       ' '         ' '           'IT_SAIDA'  'MARK'  'MARK'              '5'       ' '   ' '   ' ',
    1       ' '         ' '           'IT_SAIDA'  'ROW'     VL_ALV_CONT       '6'       ' '   ' '   ' ',
    2       'ZPPT0002'  'ACHARG'      'IT_SAIDA'  'ACHARG'  'Num. Fardo'      '11'      ' '   ' '   ' ',
    3       'ZPPT0002'  'WERKS'       'IT_SAIDA'  'WERKS'   'Centro'          '7'       'X'   ' '   ' ',
    4       'ZPPT0002'  'MATNR'       'IT_SAIDA'  'MATNR'   'Material'        '11'      'X'   ' '   ' ',
    5       'ZPPT0002'  'BUDAT'       'IT_SAIDA'  'BUDAT'   'Data Lançamento' '16'      'X'   ' '   ' ',
    6       'ZPPT0002'  'VERID'       'IT_SAIDA'  'VERID'   'Maquina'         '8'       'X'   ' '   ' ',
    7       'ZPPT0002'  'MENGE'       'IT_SAIDA'  'MENGE'   'Peso'            '8'       'X'   ' '   ' ',
    8       'ZPPT0002'  'CHARG'       'IT_SAIDA'  'CHARG'   'Fardão Orig.'    '13'      'X'   ' '   ' ',
    9       'ZPPT0002'  'STATUS'      'IT_SAIDA'  'STATUS'  'Status'          '7'       ' '   ' '   ' ',
    10      ' '         ' '           'IT_SAIDA'  'DEL'     'Eliminar'        '9'       ' '   ' '   ' '.
ENDFORM.                    " F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_MONTAR_ESTRUTURA  USING  P_COL_POS   P_REF_TABNAME P_REF_FIELDNAME P_TABNAME P_FIELD
                                P_SCRTEXT_L P_OUTPUTLEN   P_EDIT          P_SUM     P_EMPHASIZE.

  CLEAR W_FIELDCATALOG.

  W_FIELDCATALOG-FIELDNAME    = P_FIELD.
  W_FIELDCATALOG-TABNAME      = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE    = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD    = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY          = ' '.

  IF WG_DISPLAY IS INITIAL.
    W_FIELDCATALOG-EDIT       = P_EDIT.
  ENDIF.

  W_FIELDCATALOG-DO_SUM       = P_SUM.
  W_FIELDCATALOG-COL_POS      = P_COL_POS.

  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.

  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

** Usado quando for drop-down
**  w_fieldcatalog-drdn_hndl  = 1.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " F_MONTAR_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_BUILD_DROPDOWN.
**  DATA:   ls_dropdown   TYPE lvc_s_drop,
**          lt_dropdown   TYPE lvc_t_drop.
**
**  ls_dropdown-handle = '1'.
**
**  ls_dropdown-value = gt_values-ddtext = .
**  gt_values-domvalue_l = .
**  APPEND: ls_dropdown TO lt_dropdown,
**          gt_values.
**
**  ls_dropdown-value = gt_values-ddtext = .
**  gt_values-domvalue_l = .
**  APPEND: ls_dropdown TO lt_dropdown,
**          gt_values.
**
**  ls_dropdown-value = gt_values-ddtext = .
**  gt_values-domvalue_l = .
**  APPEND: ls_dropdown TO lt_dropdown,
**          gt_values.
**
**  CALL METHOD grid1->set_drop_down_table
**    EXPORTING
**      it_drop_down = lt_dropdown.
ENDFORM.                    " F_BUILD_DROPDOWN

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_200
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS_200.
  REFRESH: IT_ZPPT0002, IT_SAIDA.

  SELECT * FROM ZPPT0002
    INTO TABLE IT_ZPPT0002
  WHERE WERKS   EQ ZPPT0002-WERKS
    AND BUDAT   EQ ZPPT0002-BUDAT
    AND STATUS  EQ C_P.

ENDFORM.                    " F_SELECIONA_DADOS_200

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS_200
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_ORGANIZA_DADOS_200.
  CHECK IT_ZPPT0002 IS NOT INITIAL.
  SORT: IT_ZPPT0002 BY ACHARG.

  CLEAR:    WA_SAIDA, WA_ZPPT0002, VG_CONT.
  REFRESH:  IT_SAIDA.
  LOOP AT IT_ZPPT0002 INTO WA_ZPPT0002.
    MOVE: WA_ZPPT0002-ACHARG  TO WA_SAIDA-ACHARG,
          WA_ZPPT0002-WERKS   TO WA_SAIDA-WERKS,
          WA_ZPPT0002-VERID   TO WA_SAIDA-VERID,
          WA_ZPPT0002-MATNR   TO WA_SAIDA-MATNR,
          WA_ZPPT0002-MENGE   TO WA_SAIDA-MENGE,
          WA_ZPPT0002-CHARG   TO WA_SAIDA-CHARG,
          WA_ZPPT0002-BUDAT   TO WA_SAIDA-BUDAT,
          WA_ZPPT0002-STATUS  TO WA_SAIDA-STATUS,

          SY-TABIX            TO WA_SAIDA-ROW.
    APPEND WA_SAIDA TO IT_SAIDA.
    VG_CONT = VG_CONT + 1.
  ENDLOOP.

  CLEAR: WA_ZPPT0002, WA_SAIDA.

ENDFORM.                    " F_ORGANIZA_DADOS_200

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS_200
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_GRAVA_DADOS_200.
**  DELETE it_saida WHERE acharg IS INITIAL
**                     OR werks  IS INITIAL
**                     OR verid  IS INITIAL
**                     OR matnr  IS INITIAL
**                     OR menge  IS INITIAL
**                     OR charg  IS INITIAL
**                     OR budat  IS INITIAL
**                     OR status IS INITIAL.

  CHECK IT_SAIDA IS NOT INITIAL.

  DATA: IT_INPUT_ZPPT0002   TYPE TABLE OF ZPPT0002,
        IT_DEL_ZPPT0002     TYPE TABLE OF ZPPT0002,
        WA_INPUT_ZPPT0002   TYPE ZPPT0002.

  REFRESH: IT_INPUT_ZPPT0002, IT_DEL_ZPPT0002.

** Monta tabela para gravar
  CLEAR: WA_INPUT_ZPPT0002, WA_SAIDA.
  LOOP AT IT_SAIDA INTO WA_SAIDA WHERE DEL NE 'X'.
    MOVE: WA_SAIDA-ACHARG TO WA_INPUT_ZPPT0002-ACHARG,
          WA_SAIDA-WERKS  TO WA_INPUT_ZPPT0002-WERKS,
          WA_SAIDA-VERID  TO WA_INPUT_ZPPT0002-VERID,
          WA_SAIDA-MATNR  TO WA_INPUT_ZPPT0002-MATNR,
          WA_SAIDA-MENGE  TO WA_INPUT_ZPPT0002-MENGE,
          WA_SAIDA-CHARG  TO WA_INPUT_ZPPT0002-CHARG,
          WA_SAIDA-BUDAT  TO WA_INPUT_ZPPT0002-BUDAT,
          WA_SAIDA-STATUS TO WA_INPUT_ZPPT0002-STATUS.
    APPEND WA_INPUT_ZPPT0002 TO IT_INPUT_ZPPT0002.
  ENDLOOP.

** Monta tabela para deletar
  CLEAR: WA_INPUT_ZPPT0002, WA_SAIDA.
  LOOP AT IT_SAIDA INTO WA_SAIDA WHERE DEL EQ 'X'.
    MOVE: WA_SAIDA-ACHARG TO WA_INPUT_ZPPT0002-ACHARG,
          WA_SAIDA-WERKS  TO WA_INPUT_ZPPT0002-WERKS,
          WA_SAIDA-VERID  TO WA_INPUT_ZPPT0002-VERID,
          WA_SAIDA-MATNR  TO WA_INPUT_ZPPT0002-MATNR,
          WA_SAIDA-MENGE  TO WA_INPUT_ZPPT0002-MENGE,
          WA_SAIDA-CHARG  TO WA_INPUT_ZPPT0002-CHARG,
          WA_SAIDA-BUDAT  TO WA_INPUT_ZPPT0002-BUDAT,
          WA_SAIDA-STATUS TO WA_INPUT_ZPPT0002-STATUS.
    APPEND WA_INPUT_ZPPT0002 TO IT_DEL_ZPPT0002.
  ENDLOOP.

  IF IT_INPUT_ZPPT0002 IS NOT INITIAL AND IT_DEL_ZPPT0002 IS NOT INITIAL.
    MODIFY ZPPT0002 FROM TABLE IT_INPUT_ZPPT0002.
    IF SY-SUBRC IS INITIAL.
      DELETE ZPPT0002 FROM TABLE IT_DEL_ZPPT0002.
      COMMIT WORK.

      MESSAGE 'Dados salvos com sucesso!' TYPE 'S'.
    ELSE.
      ROLLBACK WORK.
      CONCATENATE 'Não foi possivel salvar os dados na tabela' 'ZPPT0002'
             INTO VG_MSG SEPARATED BY SPACE.
    ENDIF.
  ELSE.
    IF IT_INPUT_ZPPT0002 IS NOT INITIAL.
      MODIFY ZPPT0002 FROM TABLE IT_INPUT_ZPPT0002.
      IF SY-SUBRC IS INITIAL.
        COMMIT WORK.
        MESSAGE 'Dados salvos com sucesso!' TYPE 'S'.
      ELSE.
        ROLLBACK WORK.
        CONCATENATE 'Não foi possivel salvar os dados na tabela' 'ZPPT0002'
               INTO VG_MSG SEPARATED BY SPACE.
      ENDIF.
    ENDIF.

    IF IT_DEL_ZPPT0002 IS NOT INITIAL.
      DELETE ZPPT0002 FROM TABLE IT_DEL_ZPPT0002.
      IF SY-SUBRC IS INITIAL.
        COMMIT WORK.
        MESSAGE 'Dados salvos com sucesso!' TYPE 'S'.
      ELSE.
        ROLLBACK WORK.
        CONCATENATE 'Não foi possivel salvar os dados na tabela' 'ZPPT0002'
               INTO VG_MSG SEPARATED BY SPACE.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_GRAVA_DADOS_200

*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS_200
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS_200.
  CHECK IT_SAIDA IS NOT INITIAL.

  DATA: WL_LINHA(6).

  IF    SY-UCOMM EQ C_SAVE
    OR  SY-UCOMM EQ C_ENTER
    OR  SY-UCOMM EQ C_RESP_SIM OR SY-UCOMM EQ C_RESP_NAO OR SY-UCOMM EQ C_RESP_CANCEL
    OR  SY-UCOMM EQ ''. "<> c_show_msgre.

    CLEAR:    TG_MSG_RET.
    REFRESH:  TG_MSG_RET, IT_MARA, IT_T001W, IT_MKAL.

**Montar tabelas internas para validar
    SELECT * FROM MARA
      INTO TABLE IT_MARA
      FOR ALL ENTRIES IN IT_SAIDA
    WHERE MATNR EQ IT_SAIDA-MATNR.

    SELECT * FROM T001W
      INTO TABLE IT_T001W
      FOR ALL ENTRIES IN IT_SAIDA
    WHERE WERKS EQ IT_SAIDA-WERKS.

    SELECT * FROM MKAL
      INTO TABLE IT_MKAL
      FOR ALL ENTRIES IN IT_SAIDA
    WHERE MATNR EQ IT_SAIDA-MATNR
      AND WERKS EQ IT_SAIDA-WERKS.
*    AND verid EQ wa_saida-verid.


    SORT: IT_MARA   BY MATNR,
          IT_T001W  BY WERKS,
          IT_MKAL   BY MATNR WERKS.

    CLEAR: WA_SAIDA.
    LOOP AT IT_SAIDA INTO WA_SAIDA.
      WL_LINHA = SY-TABIX.

****  Centro (WERKS)
      IF WA_SAIDA-WERKS IS INITIAL.
        MOVE: 'WERKS'     TO TG_MSG_RET-FIELD,
              'GRID1'     TO TG_MSG_RET-OBJ,
              WL_LINHA    TO TG_MSG_RET-TABIX.

        CONCATENATE 'CENTRO:' 'em branco.' 'LINHA:' WL_LINHA
               INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSE.
        READ TABLE IT_T001W TRANSPORTING NO FIELDS WITH KEY WERKS = WA_SAIDA-WERKS
                                                   BINARY SEARCH.
        IF SY-SUBRC <> 0.
          MOVE: 'WERKS'   TO TG_MSG_RET-FIELD,
                'GRID1'   TO TG_MSG_RET-OBJ,
                WL_LINHA  TO TG_MSG_RET-TABIX.

          CONCATENATE 'CENTRO:' WA_SAIDA-WERKS 'LINHA:' WL_LINHA
                 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.

****  Material (MATNR)
      IF WA_SAIDA-MATNR IS INITIAL.
        MOVE: 'MATNR'     TO TG_MSG_RET-FIELD,
              'GRID1'     TO TG_MSG_RET-OBJ,
              WL_LINHA    TO TG_MSG_RET-TABIX.

        CONCATENATE 'MATERIAL:' 'em branco.' 'LINHA:' WL_LINHA
               INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSE.
        READ TABLE IT_MARA TRANSPORTING NO FIELDS WITH KEY MATNR = WA_SAIDA-MATNR
                                                  BINARY SEARCH.
        IF SY-SUBRC IS NOT INITIAL.
          MOVE: 'MATNR'   TO TG_MSG_RET-FIELD,
                'GRID1'   TO TG_MSG_RET-OBJ,
                WL_LINHA  TO TG_MSG_RET-TABIX.

          CONCATENATE 'MATERIA:' WA_SAIDA-MATNR 'LINHA:' WL_LINHA
                 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.

****  Maquina (VERID)
      IF WA_SAIDA-VERID IS INITIAL.
        MOVE: 'VERID'     TO TG_MSG_RET-FIELD,
              'GRID1'     TO TG_MSG_RET-OBJ,
              WL_LINHA    TO TG_MSG_RET-TABIX.

        CONCATENATE 'MAQUINA:' 'em branco.' 'LINHA:' WL_LINHA
               INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSE.
        IF WA_SAIDA-WERKS IS INITIAL OR WA_SAIDA-MATNR IS INITIAL.
          MOVE: 'VERID'    TO TG_MSG_RET-FIELD,
                 'GRID1'   TO TG_MSG_RET-OBJ,
                 WL_LINHA  TO TG_MSG_RET-TABIX.

          CONCATENATE 'MAQUINA:' WA_SAIDA-VERID 'não encontrada.' 'LINHA:' WL_LINHA
                 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ELSE.
          READ TABLE IT_MKAL TRANSPORTING NO FIELDS WITH KEY MATNR = WA_SAIDA-MATNR
                                                             WERKS = WA_SAIDA-WERKS
                                                             VERID = WA_SAIDA-VERID
                                                    BINARY SEARCH.
          IF SY-SUBRC <> 0.
            MOVE: 'VERID'    TO TG_MSG_RET-FIELD,
                   'GRID1'   TO TG_MSG_RET-OBJ,
                   WL_LINHA  TO TG_MSG_RET-TABIX.

            CONCATENATE 'MAQUINA:' WA_SAIDA-VERID 'não encontrada.' 'LINHA:' WL_LINHA
                   INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
      ENDIF.

****  Data (BUDAT)
      IF WA_SAIDA-BUDAT IS INITIAL.
        MOVE: 'BUDAT'     TO TG_MSG_RET-FIELD,
              'GRID1'     TO TG_MSG_RET-OBJ,
              WL_LINHA    TO TG_MSG_RET-TABIX.
        CONCATENATE 'DATA LANÇAMENTO:''em branco.' 'LINHA:' WL_LINHA
               INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSE.
        "validar data
      ENDIF.

****  Peso (MENGE)
      IF WA_SAIDA-MENGE IS INITIAL.
        MOVE: 'MENGE'   TO TG_MSG_RET-FIELD,
              'GRID1'   TO TG_MSG_RET-OBJ,
              WL_LINHA  TO TG_MSG_RET-TABIX.

        CONCATENATE 'PESO:' 'em branco.' 'LINHA:' WL_LINHA
               INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.

****  Fardao Origem (CHARG)
      IF WA_SAIDA-CHARG IS INITIAL.
        MOVE: 'CHARG'   TO TG_MSG_RET-FIELD,
              'GRID1'   TO TG_MSG_RET-OBJ,
              WL_LINHA  TO TG_MSG_RET-TABIX.

        CONCATENATE 'FARDAO ORIGEM:' 'em branco.' 'LINHA:' WL_LINHA
               INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.
    ENDLOOP.

**** Verificaçao de gravaçao na tabela
    IF VG_MSG IS NOT INITIAL.
      MOVE: 'ACHARG'    TO TG_MSG_RET-FIELD,
            'GRID1'     TO TG_MSG_RET-OBJ,
            WL_LINHA    TO TG_MSG_RET-TABIX,
            VG_MSG      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '200'
*        i_show        = C_X
        I_REPID       = SY-REPID
*        i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
        I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
  ENDIF.
ENDFORM.                    " VERIFICA_ERROS_200

*&---------------------------------------------------------------------*
*&      Form  F_BT_SAVE
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_BT_SAVE.
  CALL METHOD GRID1->CHECK_CHANGED_DATA.
  PERFORM F_VERIFICA_ERROS_200.
  IF TG_MSG_RET[] IS NOT INITIAL.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '200'
        I_SHOW        = C_X
        I_REPID       = SY-REPID
        I_POPUP       = 1
        I_SET_CELL    = 'WG_CELL'
        I_SET_OBJ     = 'WL_OBJ'
*        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*        i_set_field   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.
  ELSE.
    PERFORM:  F_GRAVA_DADOS_200,
              F_SELECIONA_DADOS_200,
              F_ORGANIZA_DADOS_200.
  ENDIF.
ENDFORM.                    " F_BT_SAVE

*&---------------------------------------------------------------------*
*&      Form  F_SAIR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_SAIR USING P_BOTAO.
  CLEAR: VG_INIT.

  CALL FUNCTION 'DEQUEUE_EZPPT0002'
    EXPORTING
      MODE_ZPPT0002       = 'E'
      MANDT               = SY-MANDT
*          ACHARG              =
      WERKS               = ZPPT0002-WERKS.
*          X_ACHARG            = ' '
*          X_WERKS             = ' '
*          _SCOPE              = '3'
*          _SYNCHRON           = ' '
*          _COLLECT            = ' '.

  CASE P_BOTAO.
    WHEN C_LANCAMENTO.
      CALL SCREEN 100.
    WHEN C_BACK.
      LEAVE TO SCREEN 0.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDFORM.                    " F_SAIR

*&---------------------------------------------------------------------*
*&      Form  f_importar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_IMPORTAR.
  DATA: TL_ZPPT0002 TYPE TABLE OF TY_TC_ZPPT0002,
        WL_ZPPT0002 TYPE TY_TC_ZPPT0002.

  DATA: WL_SEQ        TYPE I,
        WL_SEQ1       TYPE C LENGTH 11,
        WL_SEQ2       TYPE C LENGTH 5,
        LV_MSG        TYPE C LENGTH 255,
        LV_FILE       TYPE RLGRAP-FILENAME,
        TL_FILE_TABLE TYPE FILETABLE,
        WL_FILE_TABLE LIKE LINE OF TL_FILE_TABLE,
        RC            TYPE I.

  DATA: T_EXCEL     LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE ,
        T_EXCEL_AUX LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE .

  FREE GT_TC_ZPPT0002.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    CHANGING
      FILE_TABLE              = TL_FILE_TABLE
      RC                      = RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  READ TABLE TL_FILE_TABLE INTO WL_FILE_TABLE INDEX 1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WL_FILE_TABLE-FILENAME
    IMPORTING
      OUTPUT = LV_FILE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = LV_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 11
      I_END_ROW               = 10000
    TABLES
      INTERN                  = T_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE 'Parâmetros inválidos.' TYPE 'E'.
    WHEN 2.
      MESSAGE 'Não foi possível ler o arquivo, verifique o caminho e tente novamente.' TYPE 'E'.
    WHEN 3.
      MESSAGE 'Erro ao importar o arquivo.' TYPE 'E'.
  ENDCASE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Atualizando Dados'.

  T_EXCEL_AUX[] = T_EXCEL[].

  SORT T_EXCEL_AUX BY ROW COL.

  CLEAR: T_EXCEL_AUX, WL_SEQ.

  LOOP AT T_EXCEL.
    CLEAR WL_ZPPT0002.

    IF T_EXCEL-ROW = T_EXCEL_AUX-ROW.
      CONTINUE.
    ENDIF.
    LOOP AT T_EXCEL_AUX WHERE ROW = T_EXCEL-ROW.
      CASE T_EXCEL_AUX-COL.
        WHEN 1.
          WL_ZPPT0002-ACHARG = T_EXCEL_AUX-VALUE.
        WHEN 6.
          TRANSLATE T_EXCEL_AUX-VALUE USING ',.'.
          WL_ZPPT0002-MENGE  = T_EXCEL_AUX-VALUE.
        WHEN 9.
          WL_ZPPT0002-CHARG  = T_EXCEL_AUX-VALUE.
      ENDCASE.
    ENDLOOP.

    ADD 1 TO WL_SEQ.

    WL_SEQ1 = WL_SEQ.

    CONCATENATE 'Gravando linha:' WL_SEQ1 INTO LV_MSG.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = LV_MSG.

    APPEND WL_ZPPT0002 TO TL_ZPPT0002.

  ENDLOOP.

  GT_TC_ZPPT0002[] = TL_ZPPT0002[].

  MESSAGE 'Importação concluída' TYPE 'S'.
ENDFORM.                    "f_importar

*&---------------------------------------------------------------------*
*&      Form  f_excluir_fardo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_EXCLUIR_FARDO.
  DATA:TL_FIELDS TYPE TABLE OF SVAL WITH HEADER LINE,
       LV_RETURN TYPE ZPPT0002-ACHARG,

       TL_ZPPT0002 TYPE TABLE OF ZPPT0002,
       WL_ZPPT0002 TYPE ZPPT0002.

  CLEAR: TL_FIELDS, TL_FIELDS[].
  TL_FIELDS-TABNAME    = 'ZPPT0002'.
  TL_FIELDS-FIELDNAME  = 'ACHARG'.
  TL_FIELDS-FIELD_OBL  = 'X'.
  APPEND TL_FIELDS.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      POPUP_TITLE     = 'Informe fardo'
    IMPORTING
      RETURNCODE      = LV_RETURN
    TABLES
      FIELDS          = TL_FIELDS
    EXCEPTIONS
      ERROR_IN_FIELDS = 1
      OTHERS          = 2.

  IF SY-SUBRC IS INITIAL.
    SELECT SINGLE *
      FROM ZPPT0002
      INTO WL_ZPPT0002
      WHERE ACHARG = TL_FIELDS-VALUE.

    IF SY-SUBRC IS INITIAL.
      MOVE-CORRESPONDING WL_ZPPT0002 TO WG_0002.
      CALL SCREEN 0300 STARTING AT 5 7.

      IF GV_RESPOSTA_EXCLUIR IS NOT INITIAL.
        DELETE ZPPT0002 FROM WL_ZPPT0002.
        IF SY-SUBRC IS INITIAL.
          MESSAGE 'Fardo excluído' TYPE 'S'.
        ELSE.
          MESSAGE 'Erro excluindo' TYPE 'E'.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'Fardo não encontrado' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    "f_excluir_fardo
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS 'Z003'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  CASE SY-UCOMM.
    WHEN 'SAVE'.
      GV_RESPOSTA_EXCLUIR = 'X'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL' OR 'BACK'.
      CLEAR GV_RESPOSTA_EXCLUIR.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
