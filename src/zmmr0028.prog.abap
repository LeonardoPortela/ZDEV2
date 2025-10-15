*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Sobral                                             &*
*& Data.....: 10/06/2013                                              &*
*& Descrição: Lançamento de Produção e classificações                 &*
*& Transação: ZMM0050                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&                                                                    &*
*&--------------------------------------------------------------------&*

*&---------------------------------------------------------------------*
*& Report  ZMMR0028
*&---------------------------------------------------------------------*

report  ZMMR0028.

** TABLES
**----------------------------------------------------------------------
tables: T001W, MARA, MKAL,    "STANDARD
        ZPPT0002.

** CONSTANTS
**----------------------------------------------------------------------
constants: C_X              type C value 'X',
           C_P              type C value 'P',
           C_ADD(3)         type C value 'ADD',
           C_DEL(3)         type C value 'DEL',
           C_EXIT(4)        type C value 'EXIT',
           C_BACK(4)        type C value 'BACK',
           C_SAVE(4)        type C value 'SAVE',
*           c_proces(6)      TYPE c VALUE 'PROCES',
           C_CANCEL(6)      type C value 'CANCEL',
           C_REFRESH(7)     type C value 'REFRESH',
           C_SEARCH(6)      type C value 'SEARCH',
           C_SHOW_MSGRE(10) type C value 'SHOW_MSGRE',
           C_LANCAR(6)      type C value 'LANCAR',
           C_CLASS(5)       type C value 'CLASS',
           C_ENTER(4)       type C value 'ENTE',
           C_RESP_SIM(4)    type C value 'OPT1',
           C_RESP_NAO(4)    type C value 'OPT2',
           C_RESP_CANCEL(4) type C value 'INFO'.

** TYPES
**----------------------------------------------------------------------
types: begin of TY_SAIDA,
         MARK,
         ROW        type I,
         NORMT      type MARA-NORMT,        "Tipo material          1
         MATNR      type MARA-MATNR,        "Material               2
         MENGE      type ZPPT0002-MENGE,    "Quantidade             3
         MEINS      type MARA-MEINS,        "UM                     4
         WERKS      type ZPPT0002-WERKS,    "Centro                 5
         VERID      type ZPPT0002-VERID,    "Versao(maquina)        6
         N_VERID    type MKAL-TEXT1,        "Nome da versão         7
         ACHARG     type ZPPT0002-ACHARG,   "Lote                   8
         ALORT      type STPO-LGORT,        "Deposito               9
         MATNR_CON  type ZPPT0002-MATNR,    "Material consumo       10
         MENGE_CON  type ZPPT0002-MENGE,    "Quantidade consumo     11
         MEINS_CON  type MARA-MEINS,        "UM consumo             12
         LGORT_CON  type STPO-LGORT,        "Deposito consumo       13
         CHARG_CON  type ZPPT0002-CHARG,    "Fardao Origem          14
         BUDAT_CON  type ZPPT0002-BUDAT,    "Data Lançamento        15
         STATUS_CON type ZPPT0002-STATUS,   "Status                 16
       end of TY_SAIDA,

       begin of TY_MSG,
         MATNR     type ZPPT0002-MATNR,
         CHARG     type ZPPT0002-ACHARG,
         MATNR_CON type ZPPT0002-MATNR,
         TIPO      type CHAR1,
         MSG       type CHAR40,
       end of TY_MSG.

** INTERNAL TABLES
**----------------------------------------------------------------------
data: IT_ZPPT0002  type table of ZPPT0002,
      IT_SAIDA     type table of TY_SAIDA,
      IT_SAIDA_AUX type table of TY_SAIDA,
      IT_MARA      type table of MARA,
      IT_T001W     type table of T001W,
      IT_MKAL      type table of MKAL,
      IT_MSG       type table of TY_MSG.

** WORK AREAS
**----------------------------------------------------------------------
data: WA_ZPPT0002  type ZPPT0002,
      WA_SAIDA     type TY_SAIDA,
      WA_SAIDA_AUX type TY_SAIDA,
      WA_MARA      type MARA,
      WA_T001W     type T001W,
      WA_MKAL      type MKAL,
      WA_MSG       type TY_MSG.

** VARIABLES
**----------------------------------------------------------------------
data: WG_DISPLAY,
      VG_INIT,
      VG_CONT         type I,
      VG_ALV_CONT(10) type C.

***** Funcao de Z_DOC_CHECK_NEW
data: X_FIELD(30),
      WG_OBJ(40),
      WG_MENSAGEM(30).
data: TG_MSG_RET type table of ZFIWRS0002 with header line,
      WG_CELL    type LVC_S_CELL,
      TG_CELL    type LVC_T_CELL.
data: IT_INDEX_ROWS type LVC_T_ROW,
      WA_INDEX_ROWS type LVC_S_ROW.

** VARIABLES BABI
data: ES_BFLUSHFLAGS   like BAPI_RM_FLG,
      ES_BFLUSHDATAGEN like BAPI_RM_DATGEN,
      ES_CONFIRMATION  like BAPI_RM_DATKEY-CONFIRMATION,
      IT_RETURN        type table of BAPIRET2,
      WA_RETURN        type BAPIRET2.

data: IT_GOODSMOVEMENTS  type table of BAPI2017_GM_ITEM_CREATE,
      WA_GOODSMOVEMENTS  type BAPI2017_GM_ITEM_CREATE,
      ES_GOODSMVT_HEADER like BAPI2017_GM_HEAD_01,
      ES_GOODSMVT_CODE   like BAPI2017_GM_CODE.

*Declaration for toolbar buttons
data : TY_TOOLBAR type STB_BUTTON.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
*Class definition for ALV toolbar
class: LCL_ALV_TOOLBAR      definition deferred.

data: GRID1                type ref to CL_GUI_ALV_GRID,
      CONTAINER1           type ref to CL_GUI_DOCKING_CONTAINER,
      OBG_TOOLBAR          type ref to LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER type ref to CL_ALV_GRID_TOOLBAR_MANAGER.

data: T_FIELDCATALOG  type LVC_T_FCAT,
      W_FIELDCATALOG  type LVC_S_FCAT,
      TG_SELECTEDCELL type LVC_T_CELL,
      WG_SELECTEDCELL type LVC_S_CELL,
      WA_LAYOUT       type LVC_S_LAYO,
      WA_STABLE       type LVC_S_STBL,
      WA_STYLE        type LVC_S_STYL,
      STYLE2          type LVC_T_STYL with header line.

data: begin of GT_VALUES occurs 0,
        DOMVALUE_L type DOMVALUE_L,
        DDTEXT     type VAL_TEXT,
      end of GT_VALUES.

** SELECTION SCREEN
**----------------------------------------------------------------------
selection-screen begin of block ONE with frame title text-T01.

  select-options:
    S_WERKS   for ZPPT0002-WERKS    obligatory no-extension no intervals,
    S_DT_LAN  for ZPPT0002-BUDAT    obligatory no-extension no intervals,
    S_DT_OV   for ZPPT0002-BUDAT    obligatory no-extension no intervals,
    S_VERID   for ZPPT0002-VERID    no-extension no intervals.

selection-screen end of block ONE.

start-of-selection.
*  PERFORM: F_LIMPA_CAMPOS.
  call screen 100.



*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class LCL_ALV_TOOLBAR definition.
  public section.
*Constructor
    methods:
      CONSTRUCTOR
        importing IO_ALV_GRID type ref to CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR for event TOOLBAR of CL_GUI_ALV_GRID
        importing E_OBJECT,

      HANDLE_USER_COMMAND for event USER_COMMAND of CL_GUI_ALV_GRID
        importing E_UCOMM.
endclass.                    "lcl_alv_toolbar DEFINITION
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
class LCL_EVENT_HANDLER definition.
  public section.

    class-methods:
      ON_DOUBLE_CLICK for event DOUBLE_CLICK of CL_GUI_ALV_GRID
        importing E_ROW E_COLUMN.

*    CLASS-METHODS:
*      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
*        IMPORTING  e_row_id e_column_id.

    class-methods:
      ON_DATA_CHANGED for event DATA_CHANGED of CL_GUI_ALV_GRID
        importing ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.

    class-methods:
      ON_DATA_CHANGED_FINISHED for event DATA_CHANGED_FINISHED of CL_GUI_ALV_GRID
        importing E_MODIFIED ET_GOOD_CELLS.

    class-methods:
      ON_BUTTON_CLICK for event BUTTON_CLICK of CL_GUI_ALV_GRID
        importing ES_COL_ID ES_ROW_NO.

    class-methods:
      ON_ONF4 for event ONF4 of CL_GUI_ALV_GRID
        importing E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY.

endclass.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class LCL_ALV_TOOLBAR implementation.
  method CONSTRUCTOR.
*   Create ALV toolbar manager instance
    create object C_ALV_TOOLBARMANAGER
      exporting
        IO_ALV_GRID = IO_ALV_GRID.
  endmethod.                    "constructor

  method ON_TOOLBAR.

**    BOTAO ADD
    TY_TOOLBAR-ICON       = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION   = C_ADD.

*    IF wg_display IS INITIAL.
*      ty_toolbar-disabled = space.
*    ELSE.
    TY_TOOLBAR-DISABLED = 1.
*    ENDIF.

    TY_TOOLBAR-BUTN_TYPE  = 0.

    append TY_TOOLBAR to E_OBJECT->MT_TOOLBAR.
    clear TY_TOOLBAR.

**    BOTAO DEL
    TY_TOOLBAR-ICON       = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION   = C_DEL.

*    IF wg_display IS INITIAL.
    TY_TOOLBAR-DISABLED = SPACE.
*    ELSE.
    TY_TOOLBAR-DISABLED = 1.
*    ENDIF.

    TY_TOOLBAR-BUTN_TYPE  = 0.

    append TY_TOOLBAR to E_OBJECT->MT_TOOLBAR.
    clear TY_TOOLBAR.

  endmethod.                    "on_toolbar

  method HANDLE_USER_COMMAND.
    clear: WA_SAIDA.

    case E_UCOMM.
      when C_ADD.

      when C_DEL.
**        CALL METHOD grid1->get_selected_cells
**          IMPORTING
**            et_cell = tg_selectedcell.
**
**        LOOP AT tg_selectedcell INTO wg_selectedcell.
**          READ TABLE it_saida INTO wa_saida INDEX wg_selectedcell-row_id-index.
**          CASE wa_saida-del.
**            WHEN c_x.
**              wa_saida-del  = ''.
**            WHEN ''.
**              wa_saida-del  = c_x.
**            WHEN OTHERS.
**          ENDCASE.
**
**          MODIFY it_saida FROM wa_saida INDEX wg_selectedcell-row_id-index
**            TRANSPORTING del.
**        ENDLOOP.
    endcase.

*    PERFORM f_verifica_erros.
*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        i_screen      = '100'
*        i_show        = space
*        i_repid       = sy-repid
*        i_popup       = 1
**        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
**        I_SET_FIELD   = 'X_FIELD'
*      IMPORTING
*        e_messagem    = wg_mensagem
*      TABLES
*        it_msgs       = tg_msg_ret.

    call method GRID1->REFRESH_TABLE_DISPLAY
      exporting
        IS_STABLE = WA_STABLE.

  endmethod.                    "zm_handle_user_command
endclass.                    "lcl_alv_toolbar IMPLEMENTATION

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
class LCL_EVENT_HANDLER implementation.
* Método de  execução para Duplo-click
  method ON_DOUBLE_CLICK.

  endmethod.                    "ON_DOUBLE_CLICK

  method ON_DATA_CHANGED.
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

  endmethod.                    "ON_DATA_CHANGED

  method ON_DATA_CHANGED_FINISHED.

*    PERFORM f_verifica_erros.
*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        i_screen      = '100'
*        i_show        = space
*        i_repid       = sy-repid
*        i_popup       = 1
**        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
**        i_set_field   = 'X_FIELD'
*      IMPORTING
*        e_messagem    = wg_mensagem
*      TABLES
*        it_msgs       = tg_msg_ret.

    perform:  F_ATUALIZA_SAIDA,
              F_VERIFICA_ERROS.

    call method GRID1->REFRESH_TABLE_DISPLAY
      exporting
        IS_STABLE = WA_STABLE.

  endmethod.                    "on_data_changed_finisheD

  method ON_BUTTON_CLICK.

**    CALL METHOD grid1->refresh_table_display
**      EXPORTING
**        is_stable = wa_stable.

  endmethod.                    "on_button_click

  method ON_ONF4.
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

  endmethod.                                                "on_ONF4
endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_VERIFICA_ERROS.
  check IT_SAIDA is not initial.

  data: WL_LINHA(6).

  if    SY-UCOMM eq C_SAVE
    or  SY-UCOMM eq C_ENTER
    or  SY-UCOMM eq C_CLASS
    or  SY-UCOMM eq C_LANCAR
*    OR  sy-ucomm EQ c_resp_sim OR sy-ucomm EQ c_resp_nao OR sy-ucomm EQ c_resp_cancel
    or  SY-UCOMM eq ''. "<> c_show_msgre.

    clear:    TG_MSG_RET, WA_MARA, WA_MKAL.
    refresh:  TG_MSG_RET, IT_MARA, IT_MKAL.

**Montar tabelas internas para validar
    if IT_MSG[] is not initial.
      data: VL_MATNR(40)     type C, "*---> 14/06/2023 - Migração S4 - JS
            VL_CHARG(10)     type C,
            VL_MATNR_CON(40) type C. "*---> 14/06/2023 - Migração S4 - JS
      loop at IT_MSG into WA_MSG.
        VL_MATNR      = WA_MSG-MATNR.
        VL_CHARG      = WA_MSG-CHARG.
        VL_MATNR_CON  = WA_MSG-MATNR_CON.


        if WA_MSG-TIPO is not initial.
          concatenate 'Retorno Bapi:'
                        'TIPO.' WA_MSG-TIPO
                        'MSG.' WA_MSG-MSG
                        'Referente.' 'Material' VL_MATNR 'Lote' VL_CHARG 'Mat. Con.' VL_MATNR_CON
                 into TG_MSG_RET-MSG separated by SPACE.
        else.
          concatenate 'MSG.' WA_MSG-MSG into TG_MSG_RET-MSG separated by SPACE.
        endif.
        append TG_MSG_RET.  clear: TG_MSG_RET.
      endloop.
    else.
      loop at IT_SAIDA into WA_SAIDA.
        WL_LINHA = SY-TABIX.
****  Tipo material (NORMT)
        if WA_SAIDA-NORMT is initial.
          move: 'NORMT'     to TG_MSG_RET-FIELD,
                'GRID1'     to TG_MSG_RET-OBJ,
                WL_LINHA    to TG_MSG_RET-TABIX.

          concatenate 'TIPO:' 'em branco.' 'LINHA:' WL_LINHA
                 into TG_MSG_RET-MSG separated by SPACE.
          append TG_MSG_RET.  clear: TG_MSG_RET.
        endif.

****  Material (MATNR)
        if WA_SAIDA-MATNR is initial.
          move: 'MATNR'     to TG_MSG_RET-FIELD,
                'GRID1'     to TG_MSG_RET-OBJ,
                WL_LINHA    to TG_MSG_RET-TABIX.

          concatenate 'MATERIAL:' 'em branco.' 'LINHA:' WL_LINHA
                 into TG_MSG_RET-MSG separated by SPACE.
          append TG_MSG_RET.  clear: TG_MSG_RET.
        endif.

****  Quantidade (MENGE)
        if WA_SAIDA-MENGE is initial.
          move: 'MENGE'   to TG_MSG_RET-FIELD,
                'GRID1'   to TG_MSG_RET-OBJ,
                WL_LINHA  to TG_MSG_RET-TABIX.

          concatenate 'PESO:' 'em branco.' 'LINHA:' WL_LINHA
                 into TG_MSG_RET-MSG separated by SPACE.
          append TG_MSG_RET.  clear: TG_MSG_RET.
        endif.

****  UM (MEINS)
        if WA_SAIDA-MEINS is initial.
          move: 'MEINS'   to TG_MSG_RET-FIELD,
                'GRID1'   to TG_MSG_RET-OBJ,
                WL_LINHA  to TG_MSG_RET-TABIX.

          concatenate 'UM:' 'em branco.' 'LINHA:' WL_LINHA
                 into TG_MSG_RET-MSG separated by SPACE.
          append TG_MSG_RET.  clear: TG_MSG_RET.
        endif.

****  Deposito (ALORT)
        if WA_SAIDA-ALORT is initial.
          move: 'ALORT'   to TG_MSG_RET-FIELD,
                'GRID1'   to TG_MSG_RET-OBJ,
                WL_LINHA  to TG_MSG_RET-TABIX.

          concatenate 'DEPOSITO:' 'em branco.' 'LINHA:' WL_LINHA
                 into TG_MSG_RET-MSG separated by SPACE.
          append TG_MSG_RET.  clear: TG_MSG_RET.
        endif.

****  Nome Versao (N_VERID)
        if WA_SAIDA-N_VERID is initial.
          move: 'N_VERID'   to TG_MSG_RET-FIELD,
                'GRID1'     to TG_MSG_RET-OBJ,
                WL_LINHA    to TG_MSG_RET-TABIX.

          concatenate 'NOME VERSAO:' 'em branco.' 'LINHA:' WL_LINHA
                 into TG_MSG_RET-MSG separated by SPACE.
          append TG_MSG_RET.  clear: TG_MSG_RET.
        endif.

****  Deposito (LGORT)
        if WA_SAIDA-LGORT_CON is initial.
          move: 'LGORT_CON' to TG_MSG_RET-FIELD,
                'GRID1'     to TG_MSG_RET-OBJ,
                WL_LINHA    to TG_MSG_RET-TABIX.

          concatenate 'DEP. CONSUMO:' 'em branco.' 'LINHA:' WL_LINHA
                 into TG_MSG_RET-MSG separated by SPACE.
          append TG_MSG_RET.  clear: TG_MSG_RET.
        endif.
      endloop.
    endif.

    call function 'Z_DOC_CHECK_NEW'
      exporting
        I_SCREEN    = '100'
*       i_show      = C_X
        I_REPID     = SY-REPID
*       i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
        I_SET_FIELD = 'X_FIELD'
      importing
        E_MESSAGEM  = WG_MENSAGEM
      tables
        IT_MSGS     = TG_MSG_RET.
  endif.

endform.                    " F_VERIFICA_ERROS

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0100 output.
  data: FCODE type table of SY-UCOMM.
  refresh: FCODE.

  if WG_DISPLAY is not initial.
    append C_SAVE to FCODE.
  endif.

*  PERFORM f_verifica_erros.
  call function 'Z_DOC_CHECK_NEW'
    exporting
      I_SCREEN   = '100'
      I_SHOW     = SPACE
      I_REPID    = SY-REPID
      I_POPUP    = 1
*     i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*     I_SET_FIELD   = 'X_FIELD'
    importing
      E_MESSAGEM = WG_MENSAGEM
    tables
      IT_MSGS    = TG_MSG_RET.

  if WG_CELL is not initial .
    refresh: TG_CELL.
    append WG_CELL to TG_CELL.
*          CONCATENATE wl_obj '->SET_SELECTED_CELLS' INTO wg_obj.
    call method GRID1->SET_SELECTED_CELLS "(wg_obj)          "(wg_msgs)=>set_selected_cells
      exporting
        IT_CELLS = TG_CELL[].
  endif.

  set pf-status 'Z001' excluding FCODE.
  set titlebar  'Z001'.
endmodule.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0100 input.
  case SY-UCOMM.    "ok_code.
    when C_SAVE or C_LANCAR.
      perform F_VERIFICA_ERROS.
      if TG_MSG_RET[] is not initial.
        call function 'Z_DOC_CHECK_NEW'
          exporting
            I_SCREEN   = '100'
            I_SHOW     = C_X
            I_REPID    = SY-REPID
            I_POPUP    = 1
            I_SET_CELL = 'WG_CELL'
            I_SET_OBJ  = 'WL_OBJ'
*           i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*           i_set_field   = 'X_FIELD'
          importing
            E_MESSAGEM = WG_MENSAGEM
          tables
            IT_MSGS    = TG_MSG_RET.
      else.
        perform: F_GRAVAR_DADOS.
      endif.
    when C_CLASS.
      perform:  F_SEL_CLASSIFICAR,
                F_VERIFICA_ERROS.
    when C_SHOW_MSGRE.
      call function 'Z_DOC_CHECK_NEW'
        exporting
          I_SCREEN   = '100'
          I_SHOW     = C_X
          I_REPID    = SY-REPID
          I_POPUP    = 1
          I_SET_CELL = 'WG_CELL'
          I_SET_OBJ  = 'WL_OBJ'
*         i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*         i_set_field   = 'X_FIELD'
        importing
          E_MESSAGEM = WG_MENSAGEM
        tables
          IT_MSGS    = TG_MSG_RET.

    when C_BACK.
**      CALL FUNCTION 'POPUP_TO_CONFIRM'
**        EXPORTING
**          TITLEBAR      = 'Fardos Pré-lançados'
**          TEXT_QUESTION = 'Desejar gravar as modificações'
**          TEXT_BUTTON_1 = 'Sim'  "(001)
**          TEXT_BUTTON_2 = 'Não'  "(002)
**        IMPORTING
**          ANSWER        = VG_RESPOSTA.
**
**      CASE VG_RESPOSTA.
**        WHEN '1'.   "Sim
**          PERFORM:  F_BT_SAVE.
**            CHECK TG_MSG_RET IS INITIAL.
**          PERFORM:  F_SAIR USING C_BACK.
**        WHEN '2'.   "Não
**          PERFORM:  F_SAIR USING C_BACK.
**        WHEN 'A'.   "Cancelar
**      ENDCASE.
      leave to screen 0.
    when C_CANCEL.
      leave list-processing and return to screen 200.
    when C_EXIT.
**      CALL FUNCTION 'POPUP_TO_CONFIRM'
**        EXPORTING
**          TITLEBAR      = 'Fardos Pré-lançados'
**          TEXT_QUESTION = 'Desejar gravar as modificações'
**          TEXT_BUTTON_1 = 'Sim'  "(001)
**          TEXT_BUTTON_2 = 'Não'  "(002)
**        IMPORTING
**          ANSWER        = VG_RESPOSTA.
**
**      CASE VG_RESPOSTA.
**        WHEN '1'.   "Sim
**          PERFORM:  F_BT_SAVE.
**            CHECK TG_MSG_RET IS INITIAL.
**          PERFORM:  F_SAIR USING C_BACK.
**        WHEN '2'.   "Não
**          PERFORM:  F_SAIR USING C_EXIT..
**        WHEN 'A'.   "Cancelar
**      ENDCASE.
      leave program.
*    WHEN OTHERS.
*      PERFORM: f_atualiza_saida.
*      PERFORM: f_verifica_erros.
  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  INICIO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module INICIO output.
  data: WL_VIEW_NAME type OCUS-TABLE value 'ZPPT0002',
        TL_RANGETAB  type table of VIMSELLIST,
        WL_LOCKUSER  type SY-UNAME,
        ANSWER.

  if VG_INIT is initial.
    clear: WG_DISPLAY.
    refresh: TG_MSG_RET.

**    CALL FUNCTION 'ENQUEUE_EZPPT0002'
**      EXPORTING
**        MODE_ZPPT0002        = 'E'
**        MANDT                = SY-MANDT
***      ACHARG               =
**        WERKS                = ZPPT0002-WERKS
***      X_ACHARG             = ' '
***      X_WERKS              = ' '
***      _SCOPE               = '2'
***      _WAIT                = ' '
***      _COLLECT             = ' '
**       EXCEPTIONS
**         FOREIGN_LOCK         = 1
**         SYSTEM_FAILURE       = 2
**         OTHERS               = 3.
**
**    CASE SY-SUBRC.
**      WHEN 1.
**        WL_LOCKUSER = SY-MSGV1(12).     "HCG sy-msgv1 lost at popup call
**        CALL FUNCTION 'POPUP_TO_DECIDE_LOCKED_DATA'
**          EXPORTING
**            I_USER               = SY-MSGV1(12)
***            I_START_COLUMN       = 9
***            I_START_ROW          = 9
**          IMPORTING
**            E_ANSWER             = ANSWER.
**        IF ANSWER = '2'.
**          MESSAGE S049(SV) WITH WL_LOCKUSER RAISING FOREIGN_LOCK.
**          CALL SCREEN 100.
**        ELSEIF ANSWER = '1'.
**          MOVE: C_X TO WG_DISPLAY.
**        ENDIF.
**      WHEN 2 OR 3.
**        MESSAGE E050(SV) WITH WL_VIEW_NAME RAISING SYSTEM_FAILURE.
**    ENDCASE.

    perform:  F_SELECIONA_DADOS,
              F_ORGANIZA_DADOS.
    VG_INIT = C_X.
  endif.

endmodule.                 " INICIO  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module CRIAR_OBJETOS output.
  data: WL_REPID    type SY-REPID,
        TL_FUNCTION type UI_FUNCTIONS,
        WL_FUNCTION like TL_FUNCTION with header line,
        LT_F4       type LVC_T_F4    with header line,
        TL_FILTER   type LVC_T_FILT,
        WL_FILTER   type LVC_S_FILT.

  WL_REPID = SY-REPID.

  if CONTAINER1 is initial.
    WA_LAYOUT-ZEBRA      = C_X.
*    wa_layout-cwidth_opt = c_x.
    WA_STABLE-ROW        = C_X.
    WA_LAYOUT-SEL_MODE   = 'A'.
    WA_LAYOUT-BOX_FNAME  = 'MARK'.
*    wa_layout-no_rowmark = c_x.

    create object CONTAINER1
      exporting
        REPID     = WL_REPID
        DYNNR     = '0100'
*       style     = container1->WS_MINIMIZEBOX
        SIDE      = CONTAINER1->DOCK_AT_TOP
        EXTENSION = 400.
*        metric    = 50.

    create object GRID1
      exporting
        I_PARENT = CONTAINER1.

    create object OBG_TOOLBAR
      exporting
        IO_ALV_GRID = GRID1.

** Register event handler
    set handler OBG_TOOLBAR->ON_TOOLBAR           for GRID1.
    set handler OBG_TOOLBAR->HANDLE_USER_COMMAND  for GRID1.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_MB_FILTER.
    append WL_FUNCTION to TL_FUNCTION.

    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_SUM.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_AVERAGE.
    append WL_FUNCTION to TL_FUNCTION.
*    wl_function = cl_gui_alv_grid=>mc_fc_count.
*    APPEND wl_function TO tl_function.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_SUBTOT.
    append WL_FUNCTION to TL_FUNCTION.
*    wl_function = cl_gui_alv_grid=>mc_fc_auf.
*    APPEND wl_function TO tl_function.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_MINIMUM.
    append WL_FUNCTION to TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_MAXIMUM.
    append WL_FUNCTION to TL_FUNCTION.

*    IF sy-ucomm = c_class.
    perform F_MONTAR_LAYOUT_EDIT.
*    ELSE.
*      PERFORM f_montar_layout.
*    ENDIF.

    call method GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      exporting
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      changing
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_FILTER            = TL_FILTER
        IT_OUTTAB            = IT_SAIDA[].

    call method GRID1->REGISTER_EDIT_EVENT
      exporting
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    call method GRID1->REGISTER_EDIT_EVENT
      exporting
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

**    call method grid1->register_f4_for_fields
**      exporting
**        it_f4 = lt_f4[].

    set handler:  LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK          for GRID1,
                  LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED for GRID1,
                  LCL_EVENT_HANDLER=>ON_DATA_CHANGED          for GRID1,
                  LCL_EVENT_HANDLER=>ON_BUTTON_CLICK          for GRID1,
                  LCL_EVENT_HANDLER=>ON_ONF4                  for GRID1.
  else.
*    IF sy-ucomm = c_class.
    perform F_MONTAR_LAYOUT_EDIT.
*    ELSE.
*      PERFORM f_montar_layout.
*    ENDIF.
    call method GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      exporting
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      changing
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_FILTER            = TL_FILTER
        IT_OUTTAB            = IT_SAIDA[].

    call method GRID1->REFRESH_TABLE_DISPLAY
      exporting
        IS_STABLE = WA_STABLE.
  endif.

endmodule.                 " CRIAR_OBJETOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_MONTAR_LAYOUT.
  clear: VG_ALV_CONT.
  VG_ALV_CONT = VG_CONT.

  refresh: T_FIELDCATALOG.

  perform F_MONTAR_ESTRUTURA using:
*   col_pos ref_tabname ref_fieldname tabname     field       scrtext_l         outputlen edit  sum   emphasize
*    0       ' '         ' '           'IT_SAIDA'  'MARK'     'MARK'             '5'       ' '   ' '   ' ',
    1       ' '         ' '           'IT_SAIDA'  'ROW'       VG_ALV_CONT       '5'       ' '   ' '   ' ',
    2       'MARA'      'NORMT'       'IT_SAIDA'  'NORMT'     'Tipo'            '10'      ' '   ' '   ' ',
    3       'MARA'      'MATNR'       'IT_SAIDA'  'MATNR'     'Material'        '11'      ' '   ' '   ' ',
    4       'ZPPT0002'  'MENGE'       'IT_SAIDA'  'MENGE'     'Quantidade'      '10'      ' '   ' '   ' ',
    5       'MARA'      'MEINS'       'IT_SAIDA'  'MEINS'     'UM'              '6'       ' '   ' '   ' ',
    6       'ZPPT0002'  'WERKS'       'IT_SAIDA'  'WERKS'     'Centro'          '6'       ' '   ' '   ' ',
    7       'ZPPT0002'  'VERID'       'IT_SAIDA'  'VERID'     'Versao'          '7'       ' '   ' '   ' ',
    8       'MKAL'      'TEXT1'       'IT_SAIDA'  'N_VERID'   'Nome versao'     '20'      ' '   ' '   ' ',
    9       'ZPPT0002'  'ACHARG'      'IT_SAIDA'  'ACHARG'    'Num. Fardo'      '11'      ' '   ' '   ' ',
    10      'STPO'      'LGORT'       'IT_SAIDA'  'ALORT'     'Deposito'        '8'       ' '   ' '   ' ',
    11      'ZPPT0002'  'MATNR'       'IT_SAIDA'  'MATNR_CON' 'Mat. Consumo'    '10'      ' '   ' '   ' ',
    12      'ZPPT0002'  'MENGE'       'IT_SAIDA'  'MENGE_CON' 'Qtd. Consumo'    '10'      ' '   ' '   ' ',
    13      'MARA'      'MEINS'       'IT_SAIDA'  'MEINS_CON' 'UM Consumo'      '6'       ' '   ' '   ' ',
    14      'STPO'      'LGORT'       'IT_SAIDA'  'LGORT_CON' 'Dept. consumo'   '8'       ' '   ' '   ' ',
    15      'ZPPT0002'  'CHARG'       'IT_SAIDA'  'CHARG_CON' 'Fardao Orig.'    '11'      ' '   ' '   ' ',
    16      'ZPPT0002'  'BUDAT'       'IT_SAIDA'  'BUDAT_CON' 'Data Lanc.'      '10'      ' '   ' '   ' '.
*    17      'ZPPT0002'  'STATUS'      'IT_SAIDA'  'STATUS_CON' 'STATUS'          '10'      ' '   ' '   ' '.
endform.                    " F_MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT_EDIT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_MONTAR_LAYOUT_EDIT.
  clear: VG_ALV_CONT.
  VG_ALV_CONT = VG_CONT.

  refresh: T_FIELDCATALOG.

  perform F_MONTAR_ESTRUTURA using:
*   col_pos ref_tabname ref_fieldname tabname     field       scrtext_l         outputlen edit  sum   emphasize
*    0       ' '         ' '           'IT_SAIDA'  'MARK'     'MARK'             '5'       ' '   ' '   ' ',
    1       ' '         ' '           'IT_SAIDA'  'ROW'       VG_ALV_CONT       '5'       ' '   ' '   ' ',
    2       'MARA'      'NORMT'       'IT_SAIDA'  'NORMT'     'Tipo'            '10'      'X'   ' '   ' ',
    3       'MARA'      'MATNR'       'IT_SAIDA'  'MATNR'     'Material'        '11'      ' '   ' '   ' ',
    4       'ZPPT0002'  'MENGE'       'IT_SAIDA'  'MENGE'     'Quantidade'      '10'      'X'   ' '   ' ',
    5       'MARA'      'MEINS'       'IT_SAIDA'  'MEINS'     'UM'              '6'       ' '   ' '   ' ',
    6       'ZPPT0002'  'WERKS'       'IT_SAIDA'  'WERKS'     'Centro'          '6'       ' '   ' '   ' ',
    7       'ZPPT0002'  'VERID'       'IT_SAIDA'  'VERID'     'Versao'          '7'       ' '   ' '   ' ',
    8       'MKAL'      'TEXT1'       'IT_SAIDA'  'N_VERID'   'Nome versao'     '20'      ' '   ' '   ' ',
    9       'ZPPT0002'  'ACHARG'      'IT_SAIDA'  'ACHARG'    'Num. Fardo'      '11'      ' '   ' '   ' ',
    10      'STPO'      'LGORT'       'IT_SAIDA'  'ALORT'     'Deposito'        '8'       'X'   ' '   ' ',
    11      'ZPPT0002'  'MATNR'       'IT_SAIDA'  'MATNR_CON' 'Mat. Consumo'    '10'      ' '   ' '   ' ',
    12      'ZPPT0002'  'MENGE'       'IT_SAIDA'  'MENGE_CON' 'Qtd. Consumo'    '10'      ' '   ' '   ' ',
    13      'MARA'      'MEINS'       'IT_SAIDA'  'MEINS_CON' 'UM Consumo'      '6'       ' '   ' '   ' ',
    14      'STPO'      'LGORT'       'IT_SAIDA'  'LGORT_CON' 'Dept. consumo'   '8'       ' '   ' '   ' ',
    15      'ZPPT0002'  'CHARG'       'IT_SAIDA'  'CHARG_CON' 'Fardao Orig.'    '11'      ' '   ' '   ' ',
    16      'ZPPT0002'  'BUDAT'       'IT_SAIDA'  'BUDAT_CON' 'Data Lanc.'      '10'      ' '   ' '   ' '.
*    17      'ZPPT0002'  'STATUS'      'IT_SAIDA'  'STATUS_CON' 'STATUS'          '10'      ' '   ' '   ' '.
endform.                    " F_MONTAR_LAYOUT_EDIT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_MONTAR_ESTRUTURA  using  P_COL_POS   P_REF_TABNAME P_REF_FIELDNAME P_TABNAME P_FIELD
                                P_SCRTEXT_L P_OUTPUTLEN   P_EDIT          P_SUM     P_EMPHASIZE.

  clear W_FIELDCATALOG.

  W_FIELDCATALOG-FIELDNAME    = P_FIELD.
  W_FIELDCATALOG-TABNAME      = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE    = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD    = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY          = ' '.

  if WG_DISPLAY is initial.
    W_FIELDCATALOG-EDIT       = P_EDIT.
  endif.

  W_FIELDCATALOG-DO_SUM       = P_SUM.
  W_FIELDCATALOG-COL_POS      = P_COL_POS.

  if P_OUTPUTLEN is not initial.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  endif.

  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

** Usado quando for drop-down
**  w_fieldcatalog-drdn_hndl  = 1.

  append W_FIELDCATALOG to T_FIELDCATALOG.
endform.                    " F_MONTAR_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_SELECIONA_DADOS.
  refresh: IT_ZPPT0002, IT_SAIDA.

  select * from ZPPT0002
    into table IT_ZPPT0002
  where WERKS   in S_WERKS
  and   VERID   in S_VERID
    and STATUS  eq C_P.
endform.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_ORGANIZA_DADOS.
  check IT_ZPPT0002 is not initial.

  sort IT_ZPPT0002 by ACHARG MATNR.

  data: IT_MKAL type table of MKAL,
        IT_MARA type table of MARA,
        IT_STPO type table of STPO.

  data: WA_MKAL type MKAL,
        WA_MARA type MARA,
        WA_STPO type STPO.

** SELECIONAR MATERIAL
  select * from MARA
    into table IT_MARA
    for all entries in IT_ZPPT0002
  where MATNR eq IT_ZPPT0002-MATNR.

** SELECIONAR DEPOSITO
  select * from STPO
    into table IT_STPO
    for all entries in IT_ZPPT0002
  where IDNRK eq IT_ZPPT0002-MATNR.

  sort: IT_MKAL by MATNR WERKS VERID,
        IT_MARA by MATNR,
        IT_STPO by IDNRK.

  clear: WA_ZPPT0002, WA_SAIDA.
  loop at IT_ZPPT0002 into WA_ZPPT0002.
    WA_SAIDA-ROW        = SY-TABIX.
    WA_SAIDA-WERKS      = WA_ZPPT0002-WERKS.
    WA_SAIDA-VERID      = WA_ZPPT0002-VERID.
    WA_SAIDA-ACHARG     = WA_ZPPT0002-ACHARG.
    WA_SAIDA-MATNR_CON  = WA_ZPPT0002-MATNR.
    WA_SAIDA-MENGE_CON  = WA_ZPPT0002-MENGE.
    WA_SAIDA-CHARG_CON  = WA_ZPPT0002-CHARG.
    WA_SAIDA-BUDAT_CON  = WA_ZPPT0002-BUDAT.
    WA_SAIDA-STATUS_CON = WA_ZPPT0002-STATUS.

    read table IT_MARA into WA_MARA with key MATNR = WA_ZPPT0002-MATNR
                                    binary search.
    WA_SAIDA-MEINS_CON  = WA_MARA-MEINS.

    read table IT_STPO into WA_STPO with key IDNRK = WA_ZPPT0002-MATNR
                                    binary search.
    WA_SAIDA-LGORT_CON  = WA_STPO-LGORT.

*    wa_saida-normt      = wa_zppt0002-normt.       "Em branco
*    wa_saida-matnr      = wa_zppt0002-matnr.       "Em branco
    WA_SAIDA-MENGE      = WA_ZPPT0002-MENGE.
*    wa_saida-meins      = wa_zppt0002-meins.       "Em branco
*    wa_saida-alort      = wa_zppt0002-alort.       "Em branco

    append WA_SAIDA to IT_SAIDA.
    clear: WA_ZPPT0002, WA_SAIDA.
    VG_CONT = VG_CONT + 1.
  endloop.

endform.                    " F_ORGANIZA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_SEL_CLASSIFICAR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_SEL_CLASSIFICAR.
  check IT_SAIDA is not initial.

  call method GRID1->GET_SELECTED_ROWS
    importing
      ET_INDEX_ROWS =
                      IT_INDEX_ROWS.

  check IT_INDEX_ROWS is not initial.

  IT_SAIDA_AUX[] = IT_SAIDA[].

  clear:    WA_SAIDA, WA_SAIDA_AUX.
  refresh:  IT_SAIDA.

  loop at IT_INDEX_ROWS into WA_INDEX_ROWS.
    read table IT_SAIDA_AUX into WA_SAIDA_AUX index WA_INDEX_ROWS-INDEX.
    if SY-SUBRC = 0.
      move-corresponding WA_SAIDA_AUX to WA_SAIDA.
      append WA_SAIDA to IT_SAIDA.
    endif.
  endloop.

endform.                    " F_SEL_CLASSIFICAR

*&---------------------------------------------------------------------*
*&      Form  F_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_GRAVAR_DADOS.
  clear: WA_MSG. refresh: IT_MSG.
  loop at IT_SAIDA into WA_SAIDA.
    clear: ES_BFLUSHFLAGS, ES_BFLUSHDATAGEN, WA_RETURN.
    refresh: IT_RETURN, IT_GOODSMOVEMENTS.

    ES_BFLUSHFLAGS-BCKFLTYPE        = '01'.

    ES_BFLUSHDATAGEN-POSTDATE       = S_DT_LAN-LOW.         "'Data de Lançamento (BUDAT)'
    ES_BFLUSHDATAGEN-DOCDATE        = S_DT_OV-LOW.          "'Data do Documento (BLDAT)'
    ES_BFLUSHDATAGEN-PRODPLANT      = WA_SAIDA-WERKS.       "'Centro (WERKS)'
    ES_BFLUSHDATAGEN-MATERIALNR     = WA_SAIDA-MATNR.       "'Material (MATNR)'
    ES_BFLUSHDATAGEN-BACKFLQUANT    = WA_SAIDA-MENGE.       "'Quantidade (ERFMG)'
    ES_BFLUSHDATAGEN-UNITOFMEASURE  = WA_SAIDA-MEINS.       "'UM (ERFME)'
    ES_BFLUSHDATAGEN-PRODVERSION    = WA_SAIDA-VERID.       "'Versão (VERID)'
    ES_BFLUSHDATAGEN-BATCH          = WA_SAIDA-ACHARG.      "'Lote (ACHARG)'
    ES_BFLUSHDATAGEN-STORAGELOC     = WA_SAIDA-ALORT.       "'Deposito (ALORT)'

* > 05/07/2023 - Migração S4 - LM
*    WA_GOODSMOVEMENTS-MATERIAL      = WA_SAIDA-MATNR_CON.   "'Material Consumo (MATNR)'

    IF STRLEN( WA_SAIDA-MATNR_CON ) > 18.
      WA_GOODSMOVEMENTS-MATERIAL_LONG = WA_SAIDA-MATNR_CON.   "'Material Consumo (MATNR) - LONG
    ELSE.
      WA_GOODSMOVEMENTS-MATERIAL      = WA_SAIDA-MATNR_CON.   "'Material Consumo (MATNR)
    ENDIF.
* > 05/07/2023 - Migração S4 - LM

    WA_GOODSMOVEMENTS-PLANT         = WA_SAIDA-WERKS.       "'Centro (WERKS)'
    WA_GOODSMOVEMENTS-STGE_LOC      = WA_SAIDA-LGORT_CON.   "'Deposito Consumo (LGORT)'
    WA_GOODSMOVEMENTS-BATCH         = WA_SAIDA-CHARG_CON.   "'Lote Consumo (CHARG)'
    WA_GOODSMOVEMENTS-MOVE_TYPE     = '261'.
    WA_GOODSMOVEMENTS-ENTRY_QNT     = WA_SAIDA-MENGE_CON.   "'Quantidade consumo (ERFME_R)'
    WA_GOODSMOVEMENTS-ENTRY_UOM     = WA_SAIDA-MEINS_CON.   "'UM consumo (ERFME)'
    append WA_GOODSMOVEMENTS to IT_GOODSMOVEMENTS.

*"  IMPORTING
*"     VALUE(BFLUSHFLAGS)   LIKE  BAPI_RM_FLG STRUCTURE  BAPI_RM_FLG
*"     VALUE(BFLUSHDATAGEN) LIKE  BAPI_RM_DATGEN STRUCTURE
*"        BAPI_RM_DATGEN
*"     VALUE(BFLUSHDATAMTS) LIKE  BAPI_RM_DATSTOCK STRUCTURE
*"        BAPI_RM_DATSTOCK OPTIONAL
*"  EXPORTING
*"     VALUE(CONFIRMATION)  LIKE  BAPI_RM_DATKEY-CONFIRMATION
*"     VALUE(RETURN)        LIKE  BAPIRET2 STRUCTURE  BAPIRET2
*"  TABLES
*"      SERIALNR STRUCTURE  BAPI_RM_DATSERIAL OPTIONAL
*"      GOODSMOVEMENTS STRUCTURE  BAPI2017_GM_ITEM_CREATE OPTIONAL

    call function 'BAPI_REPMANCONF1_CREATE_MTS' "#EC CI_USAGE_OK[2438131] unterdrückt werden
      exporting
        BFLUSHFLAGS    = ES_BFLUSHFLAGS
        BFLUSHDATAGEN  = ES_BFLUSHDATAGEN
*       BFLUSHDATAMTS  =
      importing
        CONFIRMATION   = ES_CONFIRMATION
        RETURN         = WA_RETURN
      tables
*       SERIALNR       =
        GOODSMOVEMENTS = IT_GOODSMOVEMENTS.

    if WA_RETURN-TYPE = 'E'.
      call function 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
        .
*    ELSEIF wa_return-type = 'S' OR es_confirmation IS NOT INITIAL.
    else.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          WAIT = C_X.

      update ZPPT0002
        set STATUS = 'C'
      where ACHARG  = WA_SAIDA-ACHARG
        and WERKS   = WA_SAIDA-WERKS.
      commit work and wait .
    endif.

    if WA_RETURN is not initial.
      perform: F_MONTA_MSG.
    endif.
  endloop.

  if IT_MSG[] is initial.
    perform:  F_SELECIONA_DADOS,
              F_ORGANIZA_DADOS.
  else.
    perform:  F_VERIFICA_ERROS,
              F_SELECIONA_DADOS,
              F_ORGANIZA_DADOS.
  endif.
endform.                    " F_GRAVAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_SAIDA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_ATUALIZA_SAIDA.

  call method GRID1->GET_SELECTED_CELLS
    importing
      ET_CELL = TG_SELECTEDCELL.

**Montar tabelas internas para validar
*    SELECT * FROM mara
*      INTO TABLE it_mara
*      FOR ALL ENTRIES IN it_saida
*    WHERE matnr EQ it_saida-matnr
*       OR normt EQ it_saida-normt.
*
*    SELECT * FROM mkal
*      INTO TABLE it_mkal
*      FOR ALL ENTRIES IN it_saida
*    WHERE matnr EQ it_saida-matnr
*      AND werks EQ it_saida-werks.
*
*    SORT: it_mkal BY matnr werks.
*          it_mara BY normt matnr.

  clear: WA_SAIDA.
  loop at TG_SELECTEDCELL into WG_SELECTEDCELL where COL_ID-FIELDNAME <> 'ALORT'.
    read table IT_SAIDA into WA_SAIDA index WG_SELECTEDCELL-ROW_ID-INDEX.
**    CASE wg_selectedcell-col_id-fieldname.
**      WHEN 'NORMT'.
**        wa_saida-matnr = ''.
**      WHEN 'MATNR'.
**        wa_saida-normt = ''.
**    ENDCASE.
***  LOOP AT it_saida INTO wa_saida.
**    IF wa_saida-matnr IS INITIAL OR wa_saida-normt IS INITIAL.
    if WA_SAIDA-NORMT is not initial. " AND wa_saida-matnr IS INITIAL.
      select single * from MARA into WA_MARA where NORMT = WA_SAIDA-NORMT.
*        SORT: it_mara BY normt matnr.
*        READ TABLE it_mara INTO wa_mara WITH KEY normt = wa_saida-normt BINARY SEARCH.
      if SY-SUBRC = 0.
        move: WA_MARA-MATNR to WA_SAIDA-MATNR.
      else.
        WA_SAIDA-NORMT = ''.
      endif.
    else.
**        IF wa_saida-matnr IS NOT INITIAL. "AND wa_saida-normt IS INITIAL.
**          SELECT SINGLE * FROM mara INTO wa_mara WHERE matnr = wa_saida-matnr.
***          SORT: it_mara BY matnr normt.
***          READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_saida-matnr BINARY SEARCH.
**          IF sy-subrc = 0.
**            MOVE: wa_mara-normt TO wa_saida-normt.
**          ELSE.
      WA_SAIDA-MATNR = ''.
**          ENDIF.
**        ENDIF.
    endif.
**    ENDIF.

    if WA_SAIDA-MATNR is not initial.  "wa_saida-n_verid IS INITIAL.
      select single * from MKAL into WA_MKAL where WERKS = WA_SAIDA-WERKS
                                               and MATNR = WA_SAIDA-MATNR
                                               and VERID = WA_SAIDA-VERID.
*      READ TABLE it_mkal INTO wa_mkal WITH KEY werks = wa_saida-werks
*                                               matnr = wa_saida-matnr
*                                               verid = wa_saida-verid.
      if SY-SUBRC = 0.
        move: WA_MKAL-TEXT1 to WA_SAIDA-N_VERID.
      endif.
    else.
      WA_SAIDA-N_VERID = ''.
    endif.

    if WA_SAIDA-MATNR is not initial.  "wa_saida-meins IS INITIAL.
      select single * from MARA into WA_MARA where MATNR = WA_SAIDA-MATNR.
*      READ TABLE it_mara INTO wa_mara WITH KEY normt = wa_saida-normt BINARY SEARCH.
      if SY-SUBRC = 0.
        move: WA_MARA-MEINS to WA_SAIDA-MEINS.
      endif.
    else.
      WA_SAIDA-MEINS = ''.
    endif.

    modify IT_SAIDA from WA_SAIDA index WG_SELECTEDCELL-ROW_ID-INDEX.

    clear: WA_MSG. refresh: IT_MSG.
    if WA_SAIDA-MATNR is initial or WA_SAIDA-NORMT is initial.
*      MESSAGE S000(SV) WITH 'Material ou Tipo não encontrado'.
**      wa_msg-msg = 'Material ou Tipo não encontrado'.
      WA_MSG-MSG = 'Tipo não encontrado.'.
      append WA_MSG to IT_MSG.
    endif.
  endloop.
endform.                    " F_ATUALIZA_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_MSG
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form F_MONTA_MSG.
** MONTAR MSG DE SUCESSO E ERRO
*  LOOP AT it_return INTO wa_return.
  WA_MSG-MATNR      = WA_SAIDA-MATNR.
  WA_MSG-CHARG      = WA_SAIDA-ACHARG.
  WA_MSG-MATNR_CON  = WA_SAIDA-MATNR_CON.
  WA_MSG-TIPO       = WA_RETURN-TYPE.
  WA_MSG-MSG        = WA_RETURN-MESSAGE.

  append WA_MSG to IT_MSG.
*  ENDLOOP.
endform.                    " F_MONTA_MSG
