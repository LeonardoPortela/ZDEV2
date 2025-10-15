*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 01/01/2012                                              &*
*& Descrição: Simulador de Vendas - Cadastro de produtos              &*
*& Transação: ZNFW0001                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK920021   10.01.2012                            &*
*&--------------------------------------------------------------------&*

REPORT  ZLESR0074.
TYPE-POOLS: RMDI.
INCLUDE: <ICON>.
INCLUDE <CL_ALV_CONTROL>.
TYPES: "BEGIN OF TY_T001W,
*        WERKS TYPE T001W-WERKS,
*        NAME1 TYPE T001W-NAME1,
*       END OF TY_T001W,
*
*       BEGIN OF TY_MAKT,
*         MATNR TYPE MAKT-MATNR,
*         MAKTX TYPE MAKT-MAKTX,
*       END OF TY_MAKT,
*
BEGIN OF TY_0070.
        INCLUDE TYPE ZLEST0070.
TYPES: CIDADE_ORIGEM_aux  TYPE ZLEST0044-CIDADE_ORIGEM,
       CIDADE_DESTINO_aux TYPE ZLEST0044-CIDADE_DESTINO,
       CNPJ_EMITENTE  TYPE ZLEST0044-CNPJ_EMITENTE,
       END OF TY_0070,

       BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
         STCD1 TYPE LFA1-STCD1,
       END OF TY_LFA1,

       BEGIN OF TY_INPUT,
        DT_INICIO        TYPE ZLEST0070-DT_INICIO,
        DT_FIM           TYPE ZLEST0070-DT_FIM,
        LIFNR            TYPE ZLEST0070-LIFNR,
        NAME1            TYPE LFA1-NAME1,
        DOMICILIO_ORIGEM TYPE ZLEST0070-DOMICILIO_ORIGEM,
        CIDADE_ORIGEM    TYPE ZLEST0069-CIDADE_ORIGEM,
        DOMICILIO_DESTIN TYPE ZLEST0070-DOMICILIO_DESTIN,
        CIDADE_DESTINO   TYPE ZLEST0069-CIDADE_DESTINO,
        TIPO             TYPE ZLEST0070-TIPO,
        PESO             TYPE ZLEST0070-PESO,
        UNID_MEDIDA      TYPE ZLEST0070-UNID_MEDIDA,
        MSEHL_PESO       TYPE T006A-MSEHL,
        PERC_TOLER       TYPE ZLEST0070-PERC_TOLER,
        PESO_TOLER       TYPE ZLEST0070-PESO_TOLER,
        WAERK            TYPE ZLEST0070-WAERK,
        KTEXT            TYPE TCURT-KTEXT,
        NETPR            TYPE ZLEST0070-NETPR,
        UNID_MED_NETPR   TYPE ZLEST0070-UNID_MED_NETPR,
        MSEHL_PRECO      TYPE T006A-MSEHL,
       END OF TY_INPUT,

       BEGIN OF TY_SAIDA,
*         MARK,
        STATUS(4),       "type zsdt0036-loekz,
        DT_INICIO        TYPE ZLEST0070-DT_INICIO,
        LIFNR            TYPE ZLEST0070-LIFNR,
        NAME1            TYPE LFA1-NAME1,
        DOMICILIO_ORIGEM TYPE ZLEST0070-DOMICILIO_ORIGEM,
        CIDADE_ORIGEM    TYPE ZLEST0069-CIDADE_ORIGEM,
        DOMICILIO_DESTIN TYPE ZLEST0070-DOMICILIO_DESTIN,
        CIDADE_DESTINO   TYPE ZLEST0069-CIDADE_DESTINO,
        TIPO             TYPE ZLEST0070-TIPO,
        DT_FIM           TYPE ZLEST0070-DT_FIM,
        PESO             TYPE ZLEST0070-PESO,
        UNID_MEDIDA      TYPE ZLEST0070-UNID_MEDIDA,
        PERC_TOLER       TYPE ZLEST0070-PERC_TOLER,
        PESO_TOLER       TYPE ZLEST0070-PESO_TOLER,
        WAERK            TYPE ZLEST0070-WAERK,
        NETPR            TYPE ZLEST0070-NETPR,
        UNID_MED_NETPR   TYPE ZLEST0070-UNID_MED_NETPR,
        QTE_FATURADO     TYPE ZLEST0044-PESO_BRUTO,
        QTE_AFATURA      TYPE ZLEST0044-PESO_BRUTO,
        EDIT(4),
*         CELLCOLORS   TYPE LVC_T_SCOL,
*         STYLE2       TYPE LVC_T_STYL,
*         ELIMINADO    TYPE ZSDT0036-ELIMINADO,
*         USNAM        TYPE ZSDT0036-USNAM,
*         DATA_ATUAL   TYPE ZSDT0036-DATA_ATUAL,
*         HORA_ATUAL   TYPE ZSDT0036-HORA_ATUAL,
       END OF TY_SAIDA,

       BEGIN OF TY_EDIT,
        INDEX            TYPE SY-INDEX,
        PESO             TYPE ZLEST0070-PESO,
        PERC_TOLER       TYPE ZLEST0070-PERC_TOLER,
        PESO_TOLER       TYPE ZLEST0070-PESO_TOLER,
       END OF TY_EDIT.

DATA: BEGIN OF GT_VALUES OCCURS 0,
        DOMVALUE_L TYPE DOMVALUE_L,
        DDTEXT TYPE VAL_TEXT,
      END OF GT_VALUES.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_X           TYPE C VALUE 'X',
           C_OK(2)       TYPE C VALUE 'OK',
           C_ADD(3)      TYPE C VALUE 'ADD',
           C_DEL(3)      TYPE C VALUE 'DEL',
           C_EXIT(4)     TYPE C VALUE 'EXIT',
           C_BACK(4)     TYPE C VALUE 'BACK',
           C_SAVE(4)     TYPE C VALUE 'SAVE',
           C_ENTER(5)    TYPE C VALUE 'ENTER',
           C_CLEAR(5)    TYPE C VALUE 'CLEAR',
           C_ADD_I(5)    TYPE C VALUE 'ADD_I',
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
DATA: TG_0070      TYPE TABLE OF TY_0070 WITH HEADER LINE,
      TG_0069      TYPE TABLE OF ZLEST0069 WITH HEADER LINE,
      TG_0044      TYPE TABLE OF ZLEST0044 WITH HEADER LINE,
      TG_LFA1      TYPE TABLE OF TY_LFA1 WITH HEADER LINE,
*      TG_MAKT      TYPE TABLE OF TY_MAKT WITH HEADER LINE,
      TG_SAIDA     TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      TG_SAIDA_AUX TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      WG_INPUT     TYPE TY_INPUT,
      WG_EDIT      TYPE TY_EDIT,
      OK_CODE      TYPE SY-UCOMM,
      WG_ACAO      TYPE SY-UCOMM,
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
      CONTAINER1           TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
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

TABLES: ZLEST0069, LFA1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_DATA   FOR SY-DATUM NO-EXTENSION NO INTERVALS,
                S_LIFNR  FOR LFA1-LIFNR,
                S_DOM_OR FOR ZLEST0069-DOMICILIO_ORIGEM NO INTERVALS NO-EXTENSION,
                S_TIPO   FOR TG_0070-TIPO.
SELECTION-SCREEN: END OF BLOCK B2.

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
               IMPORTING  E_OBJECT.

    CLASS-METHODS: HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
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
*      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*                                   IMPORTING  E_ROW_ID E_COLUMN_ID.
*
    CLASS-METHODS:
     ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                       IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .

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
*    TY_TOOLBAR-ICON      =  ICON_INSERT_ROW.
*    TY_TOOLBAR-FUNCTION  =  C_ADD.
*    IF WG_DISPLAY IS INITIAL.
*      TY_TOOLBAR-DISABLED  = SPACE.
*    ELSE.
*      TY_TOOLBAR-DISABLED  = 1.
*    ENDIF.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.
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
    TY_TOOLBAR-BUTN_TYPE = 3.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.
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

*    TY_TOOLBAR-ICON      =  ICON_BEN_TERMINATION.
*    TY_TOOLBAR-FUNCTION  =  C_CLEAR.
*    IF WG_DISPLAY IS INITIAL.
*      TY_TOOLBAR-DISABLED  = SPACE.
*    ELSE.
*      TY_TOOLBAR-DISABLED  = 1.
*    ENDIF.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

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
*   Call reorganize method of toolbar manager to
*   display the toolbar
*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar
  METHOD HANDLE_USER_COMMAND.
    DATA: WL_SAIDA TYPE TY_SAIDA.
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
*        CLEAR WL_SAIDA.
*        WL_SAIDA-USNAM      = SY-UNAME.
*        WL_SAIDA-DATA_ATUAL = SY-DATUM.
*        WL_SAIDA-HORA_ATUAL = SY-UZEIT.
*        APPEND WL_SAIDA TO TG_SAIDA.
*        APPEND INITIAL LINE TO TG_SAIDA.
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
        IF WG_INPUT-DT_INICIO IS NOT INITIAL
       AND WG_INPUT-DT_FIM IS NOT INITIAL
       AND WG_INPUT-LIFNR IS NOT INITIAL
       AND WG_INPUT-DOMICILIO_ORIGEM IS NOT INITIAL
       AND WG_INPUT-DOMICILIO_DESTIN IS NOT INITIAL
       AND WG_INPUT-PESO IS NOT INITIAL
       AND WG_INPUT-UNID_MEDIDA IS NOT INITIAL
       AND WG_INPUT-PERC_TOLER IS NOT INITIAL
       AND WG_INPUT-PESO_TOLER IS NOT INITIAL
       AND WG_INPUT-WAERK IS NOT INITIAL
       AND WG_INPUT-NETPR IS NOT INITIAL
       AND WG_INPUT-UNID_MED_NETPR IS NOT INITIAL.
          CLEAR: WL_SAIDA.
*          SELECT SUM( PESO )
*            FROM ZLEST0044
*            INTO WL_SAIDA-qTE_FATURADO
*             WHERE DATA GE WG_INPUT-DT_INICIO
*               AND DATA LE WG_INPUT-DT_FIM
*               AND LIFNR EQ WG_INPUT-LIFNR
*               AND CIDADE_ORIGEM EQ WG_INPUT-CIDADE_ORIGEM
*               AND CIDADE_DESTINO EQ WG_INPUT-CIDADE_DESTINO.

          READ TABLE TG_SAIDA TRANSPORTING NO FIELDS
            WITH KEY DT_INICIO        = WG_INPUT-DT_INICIO
                     LIFNR            = WG_INPUT-LIFNR
                     DOMICILIO_ORIGEM = WG_INPUT-DOMICILIO_ORIGEM
                     DOMICILIO_DESTIN = WG_INPUT-DOMICILIO_DESTIN
                     TIPO             = WG_INPUT-TIPO.
          IF SY-SUBRC IS INITIAL.
            MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Condições já existentes. Data inicial, Fornecedor'
                                                   'Origem, Destino e Tipo'.
          ELSE.
            MOVE-CORRESPONDING: WG_INPUT TO WL_SAIDA.
            WL_SAIDA-EDIT            = ICON_CHANGE_NUMBER.
            APPEND WL_SAIDA TO TG_SAIDA.
            CLEAR: WG_INPUT.
          ENDIF.
        ELSE.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'É obrigatório o preenchimento de todos os campo.'.
        ENDIF.
        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
      WHEN C_DEL.
        CALL METHOD GRID1->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.
*
        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          READ TABLE TG_SAIDA INTO WL_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

*          WL_SAIDA-ELIMINADO  = C_X.
*          WL_SAIDA-USNAM      = SY-UNAME.
*          WL_SAIDA-DATA_ATUAL = SY-DATUM.
*          WL_SAIDA-HORA_ATUAL = SY-UZEIT.
*          MODIFY TG_SAIDA FROM WL_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX
*            TRANSPORTING ELIMINADO USNAM DATA_ATUAL HORA_ATUAL.
          IF WL_SAIDA-QTE_FATURADO IS INITIAL.
            DELETE TG_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
          ELSE.
            MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Não é possivel eliminar essa condição,'
                                                   'pois já tem qtd. faturada.'.
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
        CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
    ENDCASE.
    PERFORM VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_POPUP       = 1
*            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*            I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.

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
* Método de  execução para Duplo-click
  METHOD ON_DOUBLE_CLICK.
*    data: wl_itens like line of tg_itens,
*          tl_impo_aux like table of tg_impo.
*
*
*    if e_row gt 0.
*      read table tg_itens into wl_itens index e_row.
*      if wl_itens-matnr is not initial
*      and wl_itens-werks is not initial
*      and wl_itens-menge is not initial
*      and wl_itens-netpr is not initial.
**    posiciona spliter na altura x
*        call method splitter->set_row_height
*          exporting
*            id     = 1
*            height = 50.
*
*        vg_subscreen1 = c_dummy_header.
*
*        if grid2 is not initial.
*          call method grid2->free.
*
*        endif.
*
*        free: container_2, grid2, tl_impo_aux.
*
*        call method splitter->get_container
*          exporting
*            row       = 2
*            column    = 1
*          receiving
*            container = container_2.
*        if grid2 is initial.
*          wa_layout-no_toolbar = c_x.
*          create object grid2
*            exporting
*              i_parent = container_2.
*
*          wa_layout-cwidth_opt = c_x.
**          wa_layout-grid_title = 'Impostos'.
*          condense e_row no-gaps.
*          concatenate 'Impostos do Item' '-' wl_itens-itmnum into wa_layout-grid_title separated by space.
*          perform montar_layout_impostos.
*          perform monta_impostos tables tl_impo_aux
*                                 using e_row.
*          call method grid2->set_table_for_first_display
*            exporting
*              is_layout       = wa_layout
*            changing
*              it_fieldcatalog = t_fieldcatalog[]
*              it_outtab       = tl_impo_aux[].
*
**      *** Método de atualização de dados na Tela
*          call method grid2->refresh_table_display
*            exporting
*              is_stable = wa_stable.
*        else.
**      *** Método de atualização de dados na Tela
*          call method grid2->refresh_table_display
*            exporting
*              is_stable = wa_stable.
*
*        endif.
*        wg_dg1 = c_maximizar.
*        leave to screen 100.
*      else.
**    posiciona spliter na altura x
*        call method splitter->set_row_height
*          exporting
*            id     = 1
*            height = 100.
*      endif.
*    endif.
*
*** Método de atualização de dados na Tela
**    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD ON_DATA_CHANGED.
    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_TABIX TYPE SY-TABIX,
          VL_VALUE TYPE LVC_VALUE,
*          WL_MARA  LIKE LINE OF TG_MARA,
*          WL_MAKT  LIKE LINE OF TG_MAKT,
*          WL_T001W LIKE LINE OF TG_T001W,
          WL_SAIDA LIKE LINE OF TG_SAIDA.
*          WL_CALCULO TYPE ZSDT0036-VLR_MARGEM,
*          WL_VLR_VENDA TYPE ZSDT0036-VLR_VENDA,
*          WL_VLR_CUSTO TYPE ZSDT0036-VLR_CUSTO,
*          WL_PERC_MARGEM TYPE ZSDT0036-PERC_MARGEM.

*    CLEAR: WL_MAKT, WL_MARA, WL_T001W, WL_CALCULO, WL_SAIDA, LV_VALUE,
*           WL_VLR_VENDA, WL_VLR_CUSTO, WL_PERC_MARGEM.
*
*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                             INTO LS_GOOD
*                             WHERE FIELDNAME = 'MATNR'.
*      LV_VALUE = LS_GOOD-VALUE.
*      CONDENSE LV_VALUE NO-GAPS.
**
*      SELECT SINGLE MATNR MATKL MEINS
*        FROM MARA
*        INTO WL_MARA
*          WHERE MATNR EQ LV_VALUE.
**
*      IF SY-SUBRC IS INITIAL.
*        SELECT SINGLE MATNR MAKTX
*          FROM MAKT
*          INTO WL_MAKT
*           WHERE MATNR EQ WL_MARA-MATNR.
*      ENDIF.
**
**        IF SY-SUBRC IS INITIAL.
*      MOVE: WL_MAKT-MAKTX TO LV_VALUE.
*
*      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*        EXPORTING
*          I_ROW_ID    = LS_GOOD-ROW_ID
*          I_FIELDNAME = 'MAKTX'
*          I_VALUE     = LV_VALUE.
*
**        ENDIF.
*      MOVE: WL_MARA-MATKL TO LV_VALUE.
*
*      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*        EXPORTING
*          I_ROW_ID    = LS_GOOD-ROW_ID
*          I_FIELDNAME = 'MATKL'
*          I_VALUE     = LV_VALUE.
*
**      MOVE: WL_MARA-MEINS TO LV_VALUE.
**
**      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
**        EXPORTING
**          I_ROW_ID    = LS_GOOD-ROW_ID
**          I_FIELDNAME = 'MEINS'
**          I_VALUE     = LV_VALUE.
*
*      CLEAR: WL_MAKT, WL_MARA, LV_VALUE.
*    ENDLOOP.


*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                             INTO LS_GOOD
*                             WHERE FIELDNAME = 'VLR_CUSTO'
*                                OR FIELDNAME = 'PERC_MARGEM'
*                                OR FIELDNAME = 'VLR_MARGEM'.
*
**      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*
*      LV_VALUE = LS_GOOD-VALUE.
*      CONDENSE LV_VALUE NO-GAPS.
*
*      IF LS_GOOD-FIELDNAME EQ 'VLR_CUSTO'.
*        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'PERC_MARGEM'
*          IMPORTING
*            E_VALUE     = WL_PERC_MARGEM.
***  Realiza o calculo do campo "Calculo"
**        wl_calculo = lv_value * ( wl_perc_margem / 100 ).
*        TRY.
*            WL_CALCULO = ( LV_VALUE / ( 1 - ( WL_PERC_MARGEM / 100 ) ) - LV_VALUE ).
*          CATCH CX_SY_ZERODIVIDE.
*        ENDTRY.
****  Realiza o calculo do campo "Valor da Venda"
*        WL_VLR_VENDA = WL_CALCULO + LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'VLR_MARGEM'
*            I_VALUE     = WL_CALCULO.
*
*      ELSEIF LS_GOOD-FIELDNAME EQ 'PERC_MARGEM'.
*        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'VLR_CUSTO'
*          IMPORTING
*            E_VALUE     = WL_VLR_CUSTO.
*
*        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'PERC_MARGEM'
*          IMPORTING
*            E_VALUE     = WL_PERC_MARGEM.
*
***  Realiza o calculo do campo "Margem Vlr"
*        TRY.
*            WL_CALCULO =  ( WL_VLR_CUSTO / ( 1 - ( WL_PERC_MARGEM / 100 ) ) - WL_VLR_CUSTO ).
*          CATCH CX_SY_ZERODIVIDE.
*        ENDTRY.
****  Realiza o calculo do campo "Valor da Venda"
*        WL_VLR_VENDA = WL_CALCULO + WL_VLR_CUSTO.
*
*        MOVE: WL_CALCULO TO LV_VALUE.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'VLR_MARGEM'
*            I_VALUE     = LV_VALUE.
*
*      ELSEIF LS_GOOD-FIELDNAME EQ 'VLR_MARGEM'.
*        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'VLR_CUSTO'
*          IMPORTING
*            E_VALUE     = WL_VLR_CUSTO.
*
*        CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_TABIX     = LS_GOOD-TABIX
*            I_FIELDNAME = 'VLR_MARGEM'
*          IMPORTING
*            E_VALUE     = WL_CALCULO.
*
***  Realiza o calculo do campo "Margem Perc"
*        TRY.
*            WL_PERC_MARGEM = ( WL_CALCULO * 100 ) / ( WL_CALCULO + WL_VLR_CUSTO ).
*          CATCH CX_SY_ZERODIVIDE.
*        ENDTRY.
****  Realiza o calculo do campo "Valor da Venda"
*        WL_VLR_VENDA = WL_CALCULO + WL_VLR_CUSTO.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'PERC_MARGEM'
*            I_VALUE     = WL_PERC_MARGEM.
*      ENDIF.
*
*      MOVE: WL_VLR_VENDA TO LV_VALUE.
*
*      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*        EXPORTING
*          I_ROW_ID    = LS_GOOD-ROW_ID
*          I_FIELDNAME = 'VLR_VENDA'
*          I_VALUE     = LV_VALUE.
*
*      CLEAR: WL_CALCULO, WL_SAIDA, LV_VALUE, WL_VLR_VENDA, WL_VLR_CUSTO,
*             WL_PERC_MARGEM.
*    ENDLOOP.

*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                                 INTO LS_GOOD
*                                 WHERE FIELDNAME = 'VAL_ATE'
*                                    OR FIELDNAME = 'VAL_DE'.
*
*      LV_VALUE = LS_GOOD-VALUE.
*      CONDENSE LV_VALUE NO-GAPS.
**      if ls_good-fieldname eq 'VAL_ATE'.
*      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*        EXPORTING
*          I_ROW_ID    = LS_GOOD-ROW_ID
*          I_FIELDNAME = LS_GOOD-FIELDNAME "'VAL_ATE'
*          I_VALUE     = LV_VALUE.
**      else.
*
**      endif.
*    ENDLOOP.
    PERFORM VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_POPUP       = 1
*            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*            I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.


  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD ON_DATA_CHANGED_FINISHED.
*    DATA: TL_SAIDA LIKE TABLE OF TG_SAIDA,
*          WL_SAIDA LIKE LINE OF TG_SAIDA,
*          LS_GOOD  TYPE LVC_S_MODI,
*          TL_MAKT  LIKE TABLE OF TG_MAKT,
*          WL_MAKT  LIKE LINE OF TG_MAKT,
*          TL_MARA  LIKE TABLE OF TG_MARA,
*          WL_MARA  LIKE LINE OF TG_MARA,
*          TL_T001W LIKE TABLE OF TG_T001W,
*          WL_T001W LIKE LINE  OF TG_T001W.
*
*
*    REFRESH: TL_MARA, TL_MAKT, TL_T001W, TL_SAIDA.
*
*    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
*       WHERE TABIX GT 0.
*
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*      APPEND WL_SAIDA TO TL_SAIDA.
*    ENDLOOP.
*
*    IF TL_SAIDA[] IS NOT INITIAL.
*
*      SELECT MATNR MAKTX
*        FROM MAKT
*        INTO TABLE TL_MAKT
*         FOR ALL ENTRIES IN TL_SAIDA
*         WHERE MATNR EQ TL_SAIDA-MATNR.
*
*      SELECT MATNR MATKL MEINS
*        FROM MARA
*        INTO TABLE TL_MARA
*         FOR ALL ENTRIES IN TL_SAIDA
*         WHERE MATNR EQ TL_SAIDA-MATNR.
*
*    ENDIF.
*    SORT: TL_MAKT BY MATNR,
*          TL_MARA BY MATNR.
*
*    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
*      WHERE TABIX GT 0.
*
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*      READ TABLE TL_MAKT INTO WL_MAKT
*        WITH KEY MATNR = WL_SAIDA-MATNR
*                 BINARY SEARCH.
*
*      READ TABLE TL_MARA INTO WL_MARA
*        WITH KEY MATNR = WL_SAIDA-MATNR
*                 BINARY SEARCH.
*
*
*      MOVE: WL_MAKT-MAKTX TO WL_SAIDA-MAKTX,
*            WL_MARA-MATKL TO WL_SAIDA-MATKL.
*
*      WL_SAIDA-USNAM      = SY-UNAME.
*      WL_SAIDA-DATA_ATUAL = SY-DATUM.
*      WL_SAIDA-HORA_ATUAL = SY-UZEIT.
*
*      MODIFY TG_SAIDA FROM WL_SAIDA INDEX LS_GOOD-ROW_ID.
*      CLEAR: WL_SAIDA, WL_T001W, WL_MARA, WL_MAKT.
*    ENDLOOP.
*
*    LOOP AT TG_SAIDA INTO TG_SAIDA.
*      REFRESH: STYLE2, TG_SAIDA-STYLE2.
*      IF TG_SAIDA-STATUS EQ ICON_UNLOCKED.
*        IF TG_SAIDA-VAL_ATE GE SY-DATUM.
*          WA_STYLE-FIELDNAME = 'VAL_ATE'.
*          WA_STYLE-STYLE = ALV_STYLE_COLOR_POSITIVE..
*          INSERT  WA_STYLE INTO TABLE STYLE2.
*          CLEAR: WA_STYLE.
*          WA_STYLE-FIELDNAME = 'VAL_DE'.
*          WA_STYLE-STYLE = ALV_STYLE_COLOR_POSITIVE.
*          INSERT  WA_STYLE INTO TABLE STYLE2.
*
*          INSERT LINES OF STYLE2 INTO TABLE TG_SAIDA-STYLE2.
*        ELSE.
*          CLEAR: WA_STYLE.
*          WA_STYLE-FIELDNAME = 'VAL_ATE'.
*          WA_STYLE-STYLE = ALV_STYLE_COLOR_NEGATIVE..
*          INSERT  WA_STYLE INTO TABLE STYLE2.
*          CLEAR: WA_STYLE.
*          WA_STYLE-FIELDNAME = 'VAL_DE'.
*          WA_STYLE-STYLE = ALV_STYLE_COLOR_NEGATIVE.
*          INSERT  WA_STYLE INTO TABLE STYLE2.
*          INSERT LINES OF STYLE2 INTO TABLE TG_SAIDA-STYLE2.
*        ENDIF.
*      ELSEIF TG_SAIDA-STATUS EQ ICON_LOCKED.
*        CLEAR: WA_STYLE.
*        WA_STYLE-FIELDNAME = 'VAL_ATE'.
*        WA_STYLE-STYLE =  ALV_STYLE_COLOR_HEADING..
*        INSERT  WA_STYLE INTO TABLE STYLE2.
*        CLEAR: WA_STYLE.
*        WA_STYLE-FIELDNAME = 'VAL_DE'.
*        WA_STYLE-STYLE =  ALV_STYLE_COLOR_HEADING.
*        INSERT  WA_STYLE INTO TABLE STYLE2.
*        INSERT LINES OF STYLE2 INTO TABLE TG_SAIDA-STYLE2.
*      ENDIF.
*      MODIFY TG_SAIDA FROM TG_SAIDA.
*      REFRESH: STYLE2.
*    ENDLOOP.

    PERFORM VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        I_SCREEN      = '100'
        I_SHOW        = SPACE
        I_REPID       = SY-REPID
        I_POPUP       = 1
*            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*            I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        E_MESSAGEM    = WG_MENSAGEM
      TABLES
        IT_MSGS       = TG_MSG_RET.

    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD ON_BUTTON_CLICK.
    DATA: WL_SAIDA LIKE LINE OF TG_SAIDA.
    IF ES_ROW_NO-ROW_ID GT 0.
      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX ES_ROW_NO-ROW_ID.
      CLEAR: WG_EDIT.
      WG_EDIT-PESO       = WL_SAIDA-PESO.
      WG_EDIT-PERC_TOLER = WL_SAIDA-PERC_TOLER.
      WG_EDIT-PESO_TOLER = WL_SAIDA-PESO_TOLER.
      WG_EDIT-INDEX      = ES_ROW_NO-ROW_ID.

      CALL SCREEN 200 ENDING AT 51 13 STARTING AT 3 3.
*
*      REFRESH: STYLE2, WL_SAIDA-STYLE2.
*
*      IF WL_SAIDA-STATUS EQ ICON_LOCKED.
*        MOVE ICON_UNLOCKED TO WL_SAIDA-STATUS.
*
*      ELSE.
*        MOVE ICON_LOCKED TO WL_SAIDA-STATUS.
*      ENDIF.
*
*      IF WL_SAIDA-STATUS EQ ICON_UNLOCKED.
*        IF WL_SAIDA-VAL_ATE GE SY-DATUM.
*          WA_STYLE-FIELDNAME = 'VAL_ATE'.
*          WA_STYLE-STYLE = ALV_STYLE_COLOR_POSITIVE..
*          INSERT  WA_STYLE INTO TABLE STYLE2.
*          CLEAR: WA_STYLE.
*          WA_STYLE-FIELDNAME = 'VAL_DE'.
*          WA_STYLE-STYLE = ALV_STYLE_COLOR_POSITIVE.
*          INSERT  WA_STYLE INTO TABLE STYLE2.
*
*          INSERT LINES OF STYLE2 INTO TABLE WL_SAIDA-STYLE2.
*        ELSE.
*          CLEAR: WA_STYLE.
*          WA_STYLE-FIELDNAME = 'VAL_ATE'.
*          WA_STYLE-STYLE = ALV_STYLE_COLOR_NEGATIVE..
*          INSERT  WA_STYLE INTO TABLE STYLE2.
*          CLEAR: WA_STYLE.
*          WA_STYLE-FIELDNAME = 'VAL_DE'.
*          WA_STYLE-STYLE = ALV_STYLE_COLOR_NEGATIVE.
*          INSERT  WA_STYLE INTO TABLE STYLE2.
*          INSERT LINES OF STYLE2 INTO TABLE WL_SAIDA-STYLE2.
*        ENDIF.
*      ELSEIF WL_SAIDA-STATUS EQ ICON_LOCKED.
*        CLEAR: WA_STYLE.
*        WA_STYLE-FIELDNAME = 'VAL_ATE'.
*        WA_STYLE-STYLE =  ALV_STYLE_COLOR_HEADING..
*        INSERT  WA_STYLE INTO TABLE STYLE2.
*        CLEAR: WA_STYLE.
*        WA_STYLE-FIELDNAME = 'VAL_DE'.
*        WA_STYLE-STYLE =  ALV_STYLE_COLOR_HEADING.
*        INSERT  WA_STYLE INTO TABLE STYLE2.
*        INSERT LINES OF STYLE2 INTO TABLE WL_SAIDA-STYLE2.
*      ENDIF.
*      WL_SAIDA-USNAM      = SY-UNAME.
*      WL_SAIDA-DATA_ATUAL = SY-DATUM.
*      WL_SAIDA-HORA_ATUAL = SY-UZEIT.
*      MODIFY TG_SAIDA FROM WL_SAIDA INDEX ES_ROW_NO-ROW_ID.
    ENDIF.
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDMETHOD.                    "on_button_click
  METHOD ON_ONF4.
    TYPES: BEGIN OF TYL_FIELD,
            TABNAME TYPE DD03L-TABNAME,    "Nome da tabela
            FIELDNAME TYPE DD03L-FIELDNAME,    "Nome de campo
            S(1) TYPE C,
           END OF TYL_FIELD,

           BEGIN OF TYL_VALUE,
            TABNAME TYPE DD03L-TABNAME,    "Nome da tabela
            FIELDNAME TYPE DD03L-FIELDNAME,    "Nome de campo
            CHAR79(79) TYPE C,
           END OF TYL_VALUE.

    DATA: BEGIN OF WL_LIFNR,
                FIELD(50),
          END OF WL_LIFNR.
*
*    DATA: BEGIN OF WL_UMB,
*                FIELD(50),
*          END OF WL_UMB.
    DATA: TL_LIFNR   LIKE TABLE OF WL_LIFNR,
*          TL_UMB   LIKE TABLE OF WL_UMB,
          TL_0069 TYPE  TABLE OF ZLEST0069,
*          TL_lfa1 TYPE  TABLE OF LFA1,
          WL_0069 TYPE ZLEST0069,
*          WL_lfa1 TYPE LFA1,
          WL_SAIDA LIKE LINE OF TG_SAIDA,
          TL_FIELD       TYPE TABLE OF TYL_FIELD,
          WL_FIELD       TYPE TYL_FIELD,
          TL_VALUE       TYPE TABLE OF TYL_VALUE,
          WL_VALUE       TYPE TYL_VALUE,
          WL_CHAR(20),
          WL_INDEX       TYPE SY-TABIX.

    IF E_FIELDNAME EQ 'LIFNR'.
      SELECT *
        FROM ZLEST0069
        INTO TABLE TL_0069.

      LOOP AT TL_0069 INTO WL_0069.

        MOVE: WL_0069-LIFNR TO WL_LIFNR-FIELD.
        APPEND WL_LIFNR TO TL_LIFNR.

        MOVE: WL_0069-NAME1 TO WL_LIFNR-FIELD.
        APPEND WL_LIFNR TO TL_LIFNR.

        MOVE: WL_0069-DOMICILIO_ORIGEM TO WL_LIFNR-FIELD.
        APPEND WL_LIFNR TO TL_LIFNR.

        MOVE: WL_0069-CIDADE_ORIGEM TO WL_LIFNR-FIELD.
        APPEND WL_LIFNR TO TL_LIFNR.

        MOVE: WL_0069-DOMICILIO_DESTIN TO WL_LIFNR-FIELD.
        APPEND WL_LIFNR TO TL_LIFNR.

        MOVE: WL_0069-CIDADE_DESTINO TO WL_LIFNR-FIELD.
        APPEND WL_LIFNR TO TL_LIFNR.
      ENDLOOP.

      WL_FIELD-TABNAME = 'ZLEST0069'.
      WL_FIELD-FIELDNAME = 'LIFNR'.
      WL_FIELD-S = 'X'.
      APPEND WL_FIELD TO TL_FIELD.

      WL_FIELD-TABNAME = 'ZLEST0069'.
      WL_FIELD-FIELDNAME = 'NAME1'.
      WL_FIELD-S = ' '.
      APPEND WL_FIELD TO TL_FIELD.

      WL_FIELD-TABNAME = 'ZLEST0069'.
      WL_FIELD-FIELDNAME = 'DOMICILIO_ORIGEM'.
      WL_FIELD-S = ' '.
      APPEND WL_FIELD TO TL_FIELD.

      WL_FIELD-TABNAME = 'ZLEST0069'.
      WL_FIELD-FIELDNAME = 'CIDADE_ORIGEM'.
      WL_FIELD-S = ' '.
      APPEND WL_FIELD TO TL_FIELD.

      WL_FIELD-TABNAME = 'ZLEST0069'.
      WL_FIELD-FIELDNAME = 'DOMICILIO_DESTIN'.
      WL_FIELD-S = ' '.
      APPEND WL_FIELD TO TL_FIELD.

      WL_FIELD-TABNAME = 'ZLEST0069'.
      WL_FIELD-FIELDNAME = 'CIDADE_DESTINO'.
      WL_FIELD-S = ' '.
      APPEND WL_FIELD TO TL_FIELD.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
*        cucol                     = '3'
          FIELDNAME                 = 'LIFNR'
          TABNAME                   = 'ZLEST0069'
        IMPORTING
          INDEX                     = WL_INDEX
          SELECT_VALUE              = WL_CHAR
        TABLES
          FIELDS                    = TL_FIELD
          SELECT_VALUES             = TL_VALUE
          VALUETAB                  = TL_LIFNR
        EXCEPTIONS
          FIELD_NOT_IN_DDIC         = 001
          MORE_THEN_ONE_SELECTFIELD = 002
          NO_SELECTFIELD            = 003.
*
      IF SY-SUBRC IS INITIAL.
        READ TABLE TL_0069 INTO WL_0069 INDEX WL_INDEX.
*        IF ES_ROW_NO-ROW_ID GT 0.
*          READ TABLE TG_SAIDA INTO TG_SAIDA INDEX ES_ROW_NO-ROW_ID.
*          IF SY-SUBRC IS INITIAL.
        MOVE: WL_0069-LIFNR            TO WG_INPUT-LIFNR,
              WL_0069-DOMICILIO_ORIGEM TO WG_INPUT-DOMICILIO_ORIGEM,
              WL_0069-NAME1            TO WG_INPUT-NAME1,
              WL_0069-CIDADE_ORIGEM    TO WG_INPUT-CIDADE_ORIGEM,
              WL_0069-DOMICILIO_DESTIN TO WG_INPUT-DOMICILIO_DESTIN,
              WL_0069-CIDADE_DESTINO   TO WG_INPUT-CIDADE_DESTINO.

        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
          EXPORTING
            FUNCTIONCODE           = '/00'
          EXCEPTIONS
            FUNCTION_NOT_SUPPORTED = 1.
*            MODIFY TG_SAIDA FROM TG_SAIDA INDEX ES_ROW_NO-ROW_ID.
*          ENDIF.
*        ENDIF.

      ENDIF.
    ENDIF.
*    ELSE.
*      IF ES_ROW_NO-ROW_ID GT 0.
*        READ TABLE TG_SAIDA INTO WL_SAIDA INDEX ES_ROW_NO-ROW_ID.
*
*        SELECT *
*          FROM MARM
*          INTO TABLE TL_MARM
*           WHERE MATNR EQ WL_SAIDA-MATNR.
*
*        IF SY-SUBRC IS INITIAL.
*          LOOP AT TL_MARM INTO WL_MARM.
*
*            MOVE: WL_MARM-MEINH TO WL_UMB-FIELD.
*            APPEND WL_UMB TO TL_UMB.
*
*          ENDLOOP.
*
*          WL_FIELD-TABNAME = 'ZSDT0036'.
*          WL_FIELD-FIELDNAME = 'MEINS'.
*          WL_FIELD-S = 'X'.
*          APPEND WL_FIELD TO TL_FIELD.
*
*
*          CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
*            EXPORTING
**        cucol                     = '3'
*              FIELDNAME                 = 'MEINS'
*              TABNAME                   = 'ZSDT0036'
*            IMPORTING
*              INDEX                     = WL_INDEX
*              SELECT_VALUE              = WL_CHAR
*            TABLES
*              FIELDS                    = TL_FIELD
*              SELECT_VALUES             = TL_VALUE
*              VALUETAB                  = TL_UMB
*            EXCEPTIONS
*              FIELD_NOT_IN_DDIC         = 001
*              MORE_THEN_ONE_SELECTFIELD = 002
*              NO_SELECTFIELD            = 003.
*          IF SY-SUBRC IS INITIAL.
*            READ TABLE TL_MARM INTO WL_MARM INDEX WL_INDEX.
*            IF ES_ROW_NO-ROW_ID GT 0.
*              READ TABLE TG_SAIDA INTO TG_SAIDA INDEX ES_ROW_NO-ROW_ID.
*              IF SY-SUBRC IS INITIAL.
*                MOVE: WL_MARM-MEINH TO TG_SAIDA-MEINS.
*                MODIFY TG_SAIDA FROM TG_SAIDA INDEX ES_ROW_NO-ROW_ID.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
**** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDMETHOD.                                                "on_ONF4
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
    APPEND C_SAVE    TO FCODE.
    APPEND C_ADD     TO FCODE.
    APPEND C_CANCEL  TO FCODE.

  ENDIF.
  IF WG_ACAO EQ C_ADD.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'A1'.
        SCREEN-INPUT = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'A1'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  PERFORM VERIFICA_ERROS.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      I_SCREEN      = '100'
      I_SHOW        = SPACE
      I_REPID       = SY-REPID
      I_POPUP       = 1
*            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*            I_SET_FIELD   = 'X_FIELD'
    IMPORTING
      E_MESSAGEM    = WG_MENSAGEM
    TABLES
      IT_MSGS       = TG_MSG_RET.

  IF WG_CELL IS NOT INITIAL .
    REFRESH: TG_CELL.
    APPEND WG_CELL TO TG_CELL.
*          CONCATENATE wl_obj '->SET_SELECTED_CELLS' INTO wg_obj.
    CALL METHOD GRID1->SET_SELECTED_CELLS"(wg_obj)          "(wg_msgs)=>set_selected_cells
        EXPORTING
          IT_CELLS = TG_CELL[].
  ENDIF.

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
    WA_LAYOUT-ZEBRA      = C_X.
*    wa_layout-no_rowmark = c_x.
    WA_LAYOUT-CWIDTH_OPT = C_X.
    WA_STABLE-ROW        = C_X.
    WA_LAYOUT-SEL_MODE   = 'C'.
    WA_LAYOUT-BOX_FNAME  = 'MARK'.

    CREATE OBJECT CONTAINER1
      EXPORTING
        CONTAINER_NAME = 'CC_01'.

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
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_MB_FILTER.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    PERFORM MONTAR_LAYOUT.
    PERFORM BUILD_DROPDOWN.

*    WA_LAYOUT-CTAB_FNAME = 'CELLCOLORS'.
*    WA_LAYOUT-STYLEFNAME = 'STYLE2'.

*    WL_FILTER-FIELDNAME = 'ELIMINADO'."c_dmbtr.
*    WL_FILTER-SIGN      = 'I'. "c_i.
*    WL_FILTER-OPTION    = 'NE'. "c_ne.
*    WL_FILTER-LOW       = 'X'.
*    APPEND WL_FILTER TO TL_FILTER.

    LT_F4-FIELDNAME = 'LIFNR'.
    LT_F4-REGISTER = 'X' .
    LT_F4-CHNGEAFTER = 'X' .
*    LT_F4-GETBEFORE = 'X' .
    APPEND LT_F4 .

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
        IT_FILTER            = TL_FILTER
        IT_FIELDCATALOG      = T_FIELDCATALOG[]
        IT_OUTTAB            = TG_SAIDA[].

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].

    SET HANDLER:
              LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK          FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED          FOR GRID1,
              LCL_EVENT_HANDLER=>ON_BUTTON_CLICK          FOR GRID1,
              LCL_EVENT_HANDLER=>ON_ONF4                  FOR GRID1.
  ELSE.
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT OUTPUT.
  DATA: WL_VIEW_NAME TYPE OCUS-TABLE VALUE 'ZLEST0070',
        TL_RANGETAB TYPE TABLE OF VIMSELLIST,
        WL_LOCKUSER TYPE SY-UNAME,
        ANSWER.

  IF INIT IS INITIAL.
    CLEAR: WG_DISPLAY.
*    BREAK-POINT.
    CALL FUNCTION 'VIEW_ENQUEUE'
        EXPORTING
          VIEW_NAME        = WL_VIEW_NAME
          ACTION           = 'E'
          ENQUEUE_MODE     = 'E'
*          enqueue_range    = "header-subsetflag
        TABLES
          SELLIST          = TL_RANGETAB
        EXCEPTIONS
          FOREIGN_LOCK     = 1
          SYSTEM_FAILURE   = 2
          TABLE_NOT_FOUND  = 5
          CLIENT_REFERENCE = 7.
    CASE SY-SUBRC.
      WHEN 1.
        WL_LOCKUSER = SY-MSGV1(12).     "HCG sy-msgv1 lost at popup call
        CALL FUNCTION 'POPUP_TO_DECIDE_LOCKED_DATA'
          EXPORTING
            I_USER               = SY-MSGV1(12)
*             I_START_COLUMN       = 9
*             I_START_ROW          = 9
          IMPORTING
            E_ANSWER             = ANSWER.
        IF ANSWER = '2'.
*IG: internal message 305882 2005
*            MESSAGE e049 WITH lockuser RAISING foreign_lock.
          MESSAGE S049(SV) WITH WL_LOCKUSER RAISING FOREIGN_LOCK.
          EXIT.
        ELSEIF ANSWER = '1'.
          MOVE: C_X TO WG_DISPLAY.
        ENDIF.
      WHEN 2.
        MESSAGE E050(SV) WITH WL_VIEW_NAME RAISING SYSTEM_FAILURE.
      WHEN 5.
        MESSAGE E028(SV) WITH WL_VIEW_NAME RAISING VIEW_NOT_FOUND.
      WHEN 7.
        MESSAGE E054(SV) WITH SY-MANDT RAISING CLIENT_REFERENCE.
    ENDCASE.
*    ENDIF.

    PERFORM SELECIONA_DADOS.
    PERFORM ORGANIZA_DADOS.
    INIT = C_X.
  ENDIF.
ENDMODULE.                 " INIT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .
  REFRESH: TG_SAIDA, TG_0070, TG_LFA1, TG_0069, TG_0044.

  SELECT *
    FROM ZLEST0069
    INTO TABLE TG_0069
     WHERE DOMICILIO_ORIGEM IN S_DOM_OR
       AND LIFNR            IN S_LIFNR.

  SELECT *
    FROM ZLEST0070
    INTO CORRESPONDING FIELDS OF TABLE TG_0070
     WHERE LIFNR IN S_LIFNR
       and tipo  in s_tipo.
*       AND DT_INICIO le S_DATA
*       and DT_FIM    ge S_DATA.
*

  LOOP AT TG_0070.
    IF S_DATA-LOW  NOT BETWEEN TG_0070-DT_INICIO AND TG_0070-DT_FIM
    AND S_DATA-LOW IS NOT INITIAL.
      DELETE TG_0070.
    ENDIF.
  ENDLOOP.
  IF SY-SUBRC IS INITIAL.


    SELECT LIFNR NAME1 STCD1
      FROM LFA1
      INTO TABLE TG_LFA1
       FOR ALL ENTRIES IN TG_0070
       WHERE LIFNR EQ TG_0070-LIFNR.

    LOOP AT TG_0070.
      CLEAR: TG_LFA1.
      READ TABLE TG_LFA1
        WITH KEY LIFNR = TG_0070-LIFNR.

      MOVE:  TG_0070-DOMICILIO_ORIGEM+3 TO TG_0070-CIDADE_ORIGEM_aux,
             TG_0070-DOMICILIO_DESTIN+3 TO TG_0070-CIDADE_DESTINO_aux,
             TG_LFA1-STCD1              TO TG_0070-CNPJ_EMITENTE.

      MODIFY TG_0070.
    ENDLOOP.

    SELECT *
      FROM ZLEST0044
      INTO TABLE TG_0044
      FOR ALL ENTRIES IN TG_0070
        WHERE DT_REFERENCIA  GE TG_0070-DT_INICIO
          AND DT_REFERENCIA  LE TG_0070-DT_FIM
          AND CNPJ_EMITENTE  EQ TG_0070-CNPJ_EMITENTE
          AND CIDADE_ORIGEM  EQ TG_0070-CIDADE_ORIGEM_aux
          AND CIDADE_DESTINO EQ TG_0070-CIDADE_DESTINO_aux
          AND TARIFA         EQ TG_0070-NETPR
          and NR_TRANS       ne SPACE
          and NR_FRETE       ne SPACE.


*    SELECT MATNR MATKL MEINS
*      FROM MARA
*      INTO TABLE TG_MARA
*       FOR ALL ENTRIES IN TG_0036
*       WHERE MATNR EQ TG_0036-MATNR
*         AND MATKL IN S_MATKL.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS
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
*        0 ' '          ' '                 'TG_SAIDA' 'STATUS'     'Status'  '5' ' ' ' ' ' ',
        0 ' '          ' '                 'TG_SAIDA' 'EDIT'               'Editar Tolerancia'  ' ' ' ' ' ' ' ',
        0 'ZLEST0070'  'TIPO'              'TG_SAIDA' 'TIPO'               'Tipo'  ' ' ' ' ' ' ' ',
        1 'ZLEST0070'  'DT_INICIO'         'TG_SAIDA' 'DT_INICIO'          ' '  ' ' ' ' ' ' ' ',
        2 'ZLEST0070'  'DT_FIM'            'TG_SAIDA' 'DT_FIM'             ' '  ' ' ' ' ' ' ' ',
        3 ' '  ' '             'TG_SAIDA' 'LIFNR'              'Fornecedor'  '10' ' ' ' ' ' ',
        4 'LFA1'       'NAME1'             'TG_SAIDA' 'NAME1'              ' '  ' ' ' ' ' ' ' ',
*        5 'ZLEST0070'  'DOMICILIO_ORIGEM'  'TG_SAIDA' 'DOMICILIO_ORIGEM'   'Domicilio Org.'  ' ' ' ' ' ' ' ',
        6 'ZLEST0069'  'CIDADE_ORIGEM'     'TG_SAIDA' 'CIDADE_ORIGEM'      'Cidade Org.'  ' ' ' ' ' ' ' ',
*        7 'ZLEST0070'  'DOMICILIO_DESTIN'  'TG_SAIDA' 'DOMICILIO_DESTIN'   'Domicilio Dest.'  ' ' ' ' ' ' ' ',
        8 'ZLEST0069'  'CIDADE_DESTINO'    'TG_SAIDA' 'CIDADE_DESTINO'     'Cidade Dest.'  ' ' ' ' ' ' ' ',
        9 'ZLEST0070'  'PESO'              'TG_SAIDA' 'PESO'               'Peso'  ' ' ' ' ' ' ' ',
       10 'ZLEST0070'  'UNID_MEDIDA'       'TG_SAIDA' 'UNID_MEDIDA'        'Unidade'  ' ' ' ' ' ' ' ',
       11 'ZLEST0070'  'PERC_TOLER'        'TG_SAIDA' 'PERC_TOLER'         'Perç. Tolerância '  ' ' ' ' ' ' ' ',
       12 'ZLEST0070'  'PESO_TOLER'        'TG_SAIDA' 'PESO_TOLER'         'Peso Tolerância'  ' ' ' ' ' ' ' ',
       13 'ZLEST0070'  'WAERK'             'TG_SAIDA' 'WAERK'              ' '  ' ' ' ' ' ' ' ',
       14 'ZLEST0070'  'NETPR'             'TG_SAIDA' 'NETPR'              'Preço'  ' ' ' ' ' ' ' ',
       15 'ZLEST0070'  'UNID_MED_NETPR'    'TG_SAIDA' 'UNID_MED_NETPR'     'Unidade Medida Preço'  ' ' ' ' ' ' ' ',
       15 'ZLEST0070'  'QTE_FATURADO'      'TG_SAIDA' 'QTE_FATURADO'       'Qtd. Faturada'  ' ' ' ' ' ' ' ',
       15 'ZLEST0070'  'QTE_AFATURA'       'TG_SAIDA' 'QTE_AFATURA'        'Qtd. a Fatura'  ' ' ' ' ' ' ' '.
*       15 'ZLEST0070'  'UNID_MED_NETPR'    'TG_SAIDA' 'UNID_MED_NETPR'     'Unidade Medida Preço'  ' ' ' ' ' ' ' ',.

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

  IF P_FIELD EQ 'EDIT'.
    IF WG_DISPLAY IS INITIAL.
      W_FIELDCATALOG-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
    ELSE.
      W_FIELDCATALOG-NO_OUT = C_X.
*      w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
  ENDIF.

*  IF P_FIELD EQ 'MEINS'.
**    w_fieldcatalog-checktable = 'T006'.
*    W_FIELDCATALOG-F4AVAILABL = C_X.
*    W_FIELDCATALOG-CONVEXIT = 'CUNIT'.
*  ENDIF.
*
*  IF P_FIELD EQ 'INCO1'.
*    W_FIELDCATALOG-DRDN_HNDL  = 1.
*  ENDIF.
*
  IF P_FIELD EQ 'LIFNR'.
    W_FIELDCATALOG-CHECKTABLE  = '!'.
    W_FIELDCATALOG-F4AVAILABL = 'X'.
  ENDIF.
  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: WL_CELL TYPE LVC_S_CELL,
        TL_CELL TYPE LVC_T_CELL,
        WL_OBJ(30).

  REFRESH: TL_CELL.
  CLEAR: WL_CELL, WL_OBJ.

  CASE OK_CODE.
    WHEN C_ADD.
      CLEAR: WG_INPUT.
      WG_INPUT-WAERK = 'BRL'.
      WG_ACAO = OK_CODE.
      CLEAR: OK_CODE.
    WHEN C_ADD_I.
      LCL_ALV_TOOLBAR=>HANDLE_USER_COMMAND( E_UCOMM = 'ADD' ).

      WG_ACAO = OK_CODE.
      CLEAR: OK_CODE.
    WHEN C_SAVE.
      CALL METHOD GRID1->CHECK_CHANGED_DATA.
*      PERFORM VERIFICA_ERROS.
*      IF TG_MSG_RET[] IS NOT INITIAL.
*        CALL FUNCTION 'Z_DOC_CHECK_NEW'
*          EXPORTING
*            I_SCREEN      = '100'
*            I_SHOW        = C_X
*            I_REPID       = SY-REPID
*            I_POPUP       = 1
*            I_SET_CELL    = 'WG_CELL'
*            I_SET_OBJ     = 'WL_OBJ'
**            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
**            I_SET_FIELD   = 'X_FIELD'
*          IMPORTING
*            E_MESSAGEM    = WG_MENSAGEM
*          TABLES
*            IT_MSGS       = TG_MSG_RET.
*      ELSE.
      PERFORM GRAVA_DADOS.
      PERFORM SELECIONA_DADOS.
      PERFORM ORGANIZA_DADOS.
*      ENDIF.
    WHEN C_SEARCH.
      PERFORM BUSCA_TEXTOS.
    WHEN C_SHOW_MSGRE.
      PERFORM VERIFICA_ERROS.
      IF TG_MSG_RET[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '100'
            I_SHOW        = C_X
            I_REPID       = SY-REPID
            I_POPUP       = 1
            I_SET_CELL    = 'WG_CELL'
            I_SET_OBJ     = 'WL_OBJ'
*            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.

    WHEN C_ENTER.

    WHEN C_BACK.
      LEAVE TO SCREEN 0.
    WHEN C_CANCEL.
      CLEAR: WG_INPUT, WG_ACAO.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 100.
    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZA_DADOS .

  DATA: LS_CELLCOLOR TYPE LVC_S_SCOL.
  SORT: TG_LFA1 BY LIFNR,
        TG_0069 BY LIFNR DOMICILIO_ORIGEM DOMICILIO_DESTIN.
*        TG_T001W BY WERKS.
*
  LOOP AT TG_0070.
    READ TABLE TG_LFA1
      WITH KEY LIFNR = TG_0070-LIFNR
               BINARY SEARCH.

    READ TABLE TG_0069
      WITH KEY LIFNR            = TG_0070-LIFNR
               DOMICILIO_ORIGEM = TG_0070-DOMICILIO_ORIGEM
               DOMICILIO_DESTIN = TG_0070-DOMICILIO_DESTIN
               BINARY SEARCH.
*
*    IF SY-SUBRC IS INITIAL.
*
**    read table tg_t001w
**      with key werks = tg_0036-werks
**               binary search.
*
    MOVE-CORRESPONDING: TG_0070 TO TG_SAIDA.
    MOVE-CORRESPONDING: TG_0069 TO TG_SAIDA.

    CLEAR: TG_SAIDA-QTE_FATURADO.
    LOOP AT TG_0044 WHERE DT_REFERENCIA  GE TG_0070-DT_INICIO
                      AND DT_REFERENCIA  LE TG_0070-DT_FIM
                      AND CNPJ_EMITENTE  EQ TG_0070-CNPJ_EMITENTE
                      AND CIDADE_ORIGEM  EQ TG_0070-CIDADE_ORIGEM_aux
                      AND CIDADE_DESTINO EQ TG_0070-CIDADE_DESTINO_aux
                      AND TARIFA         EQ TG_0070-NETPR.

      ADD TG_0044-PESO_BRUTO TO TG_SAIDA-QTE_FATURADO.
    ENDLOOP.
    TG_SAIDA-QTE_AFATURA     = TG_0070-PESO - TG_SAIDA-QTE_FATURADO.
    TG_SAIDA-CIDADE_ORIGEM   = TG_0069-CIDADE_ORIGEM.
    TG_SAIDA-CIDADE_DESTINO  = TG_0069-CIDADE_DESTINO.
    TG_SAIDA-EDIT            = ICON_CHANGE_NUMBER.

*
*
*      IF TG_0036-LOEKZ EQ C_X.
*        MOVE: ICON_LOCKED     TO  TG_SAIDA-STATUS.
*      ELSE.
*        MOVE: ICON_UNLOCKED    TO  TG_SAIDA-STATUS.
*      ENDIF.
*
    APPEND TG_SAIDA.
*    ENDIF.
    CLEAR: TG_SAIDA, TG_0070, TG_LFA1, TG_0069.
*
  ENDLOOP.

*  SORT: TG_SAIDA BY MATNR.
*
*  REFRESH: STYLE2, TG_SAIDA-STYLE2.
*  LOOP AT TG_SAIDA.
*    REFRESH: STYLE2, TG_SAIDA-STYLE2.
*    IF TG_SAIDA-STATUS EQ ICON_UNLOCKED.
*      IF TG_SAIDA-VAL_ATE GE SY-DATUM.
*        WA_STYLE-FIELDNAME = 'VAL_ATE'.
*        WA_STYLE-STYLE = ALV_STYLE_COLOR_POSITIVE..
*        INSERT  WA_STYLE INTO TABLE STYLE2.
*        CLEAR: WA_STYLE.
*        WA_STYLE-FIELDNAME = 'VAL_DE'.
*        WA_STYLE-STYLE = ALV_STYLE_COLOR_POSITIVE.
*        INSERT  WA_STYLE INTO TABLE STYLE2.
*
*        INSERT LINES OF STYLE2 INTO TABLE TG_SAIDA-STYLE2.
*      ELSE.
*        CLEAR: WA_STYLE.
*        WA_STYLE-FIELDNAME = 'VAL_ATE'.
*        WA_STYLE-STYLE = ALV_STYLE_COLOR_NEGATIVE..
*        INSERT  WA_STYLE INTO TABLE STYLE2.
*        CLEAR: WA_STYLE.
*        WA_STYLE-FIELDNAME = 'VAL_DE'.
*        WA_STYLE-STYLE = ALV_STYLE_COLOR_NEGATIVE.
*        INSERT  WA_STYLE INTO TABLE STYLE2.
*        INSERT LINES OF STYLE2 INTO TABLE TG_SAIDA-STYLE2.
*      ENDIF.
*    ELSEIF TG_SAIDA-STATUS EQ ICON_LOCKED.
*      CLEAR: WA_STYLE.
*      WA_STYLE-FIELDNAME = 'VAL_ATE'.
*      WA_STYLE-STYLE =  ALV_STYLE_COLOR_HEADING..
*      INSERT  WA_STYLE INTO TABLE STYLE2.
*      CLEAR: WA_STYLE.
*      WA_STYLE-FIELDNAME = 'VAL_DE'.
*      WA_STYLE-STYLE =  ALV_STYLE_COLOR_HEADING.
*      INSERT  WA_STYLE INTO TABLE STYLE2.
*      INSERT LINES OF STYLE2 INTO TABLE TG_SAIDA-STYLE2.
*    ENDIF.
*    MODIFY TG_SAIDA.
*    REFRESH: STYLE2.
*  ENDLOOP.
*
*  IF TG_SAIDA[] IS NOT INITIAL.
*    TG_SAIDA_AUX[] = TG_SAIDA[].
*  ENDIF.
ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS .
  DATA: TL_INPUT_0070 TYPE TABLE OF ZLEST0070 WITH HEADER LINE,
        TL_0070 TYPE TABLE OF ZLEST0070 WITH HEADER LINE.
*        TL_MARA TYPE TABLE OF MARA WITH HEADER LINE.
*
  CLEAR: TL_INPUT_0070." TL_0036, TL_MARA.
  REFRESH: TL_INPUT_0070. " TL_0036, TL_MARA.
*  SELECT *
*    FROM ZLEST0070
*    INTO TABLE TL_0070
*     WHERE LIFNR            IN S_LIFNR
*       AND DOMICILIO_ORIGEM IN S_DOM_OR.
*
*  IF SY-SUBRC IS INITIAL.
*    SELECT *
*      FROM MARA
*      INTO TABLE TL_MARA
*       FOR ALL ENTRIES IN TL_0036
*        WHERE MATNR EQ TL_0036-MATNR
*          AND MATKL IN S_MATKL.
*
*  ENDIF.
*
*
  DELETE FROM ZLEST0070 WHERE LIFNR             IN S_LIFNR
                           AND DOMICILIO_ORIGEM IN S_DOM_OR.

  DELETE TG_SAIDA WHERE DT_INICIO        IS INITIAL
                    AND DT_FIM           IS INITIAL
                    AND LIFNR            IS INITIAL
                    AND DOMICILIO_ORIGEM IS INITIAL
                    AND DOMICILIO_DESTIN IS INITIAL
                    AND PESO             IS INITIAL
                    AND UNID_MEDIDA      IS INITIAL
                    AND PERC_TOLER       IS INITIAL
                    AND PESO_TOLER       IS INITIAL
                    AND WAERK            IS INITIAL
                    AND NETPR            IS INITIAL
                    AND UNID_MED_NETPR   IS INITIAL
                    AND QTE_FATURADO     IS INITIAL.

  LOOP AT TG_SAIDA.
    MOVE-CORRESPONDING: TG_SAIDA TO TL_INPUT_0070.
*    move: TG_SAIDA-DOMICILIO_ORIGEM+3 to TL_INPUT_0070-CIDADE_ORIGEM,
*          TG_SAIDA-DOMICILIO_DESTIN+3 to TL_INPUT_0070-CIDADE_DESTINO.

*    IF TG_SAIDA-STATUS EQ ICON_LOCKED.
*      MOVE : C_X TO TL_INPUT_0036-LOEKZ.
*    ELSE.
*      MOVE : SPACE TO TL_INPUT_0036-LOEKZ.
*    ENDIF.
    APPEND TL_INPUT_0070.
    CLEAR: TL_INPUT_0070.

  ENDLOOP.

  MODIFY ZLEST0070 FROM TABLE TL_INPUT_0070.
  IF SY-SUBRC IS INITIAL.
    MESSAGE S836(SD) WITH 'Os dados foram salvos!'.
  ELSE.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Ocorreu um erro na gravação,'
                                           'verificar as entradas!'.
  ENDIF.

*** Rotina para encontra os itens com chave mudadas, e marcar eles pra
** eliminacao na base.
*  SORT: TG_SAIDA BY VAL_DE VAL_ATE DTVENC MATNR WAERK
*                    INCO1 SAFRA CULTURA WERKS_FORNEC.
*  LOOP AT TG_SAIDA_AUX.
*    READ TABLE TG_SAIDA TRANSPORTING NO FIELDS
*     WITH KEY VAL_DE          = TG_SAIDA_AUX-VAL_DE
*              VAL_ATE         = TG_SAIDA_AUX-VAL_ATE
*              DTVENC          = TG_SAIDA_AUX-DTVENC
*              MATNR           = TG_SAIDA_AUX-MATNR
*              WAERK           = TG_SAIDA_AUX-WAERK
*              INCO1           = TG_SAIDA_AUX-INCO1
*              SAFRA           = TG_SAIDA_AUX-SAFRA
*              CULTURA         = TG_SAIDA_AUX-CULTURA
*              WERKS_FORNEC    = TG_SAIDA_AUX-WERKS_FORNEC
*                   BINARY SEARCH.
*
*    IF SY-SUBRC IS INITIAL.
*      DELETE TG_SAIDA_AUX.
*    ENDIF.
*  ENDLOOP.
*
*  MODIFY ZSDT0036 FROM TABLE TL_INPUT_0036.
*  IF SY-SUBRC IS INITIAL.
*    LOOP AT TG_SAIDA_AUX.
*      DELETE FROM ZSDT0036 WHERE VAL_DE       = TG_SAIDA_AUX-VAL_DE
*                             AND VAL_ATE      = TG_SAIDA_AUX-VAL_ATE
*                             AND DTVENC       = TG_SAIDA_AUX-DTVENC
*                             AND MATNR        = TG_SAIDA_AUX-MATNR
*                             AND WAERK        = TG_SAIDA_AUX-WAERK
*                             AND INCO1        = TG_SAIDA_AUX-INCO1
*                             AND SAFRA        = TG_SAIDA_AUX-SAFRA
*                             AND CULTURA      = TG_SAIDA_AUX-CULTURA
*                             and werks_fornec = tg_saida_aux-werks_fornec.
*
*    ENDLOOP.
*    MESSAGE S836(SD) WITH 'Os dados foram salvos!'.
*  ELSE.
*    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Ocorreu um erro na gravação,'
*                                           'verificar as entradas!'.
*  ENDIF.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .
*  DATA:     TL_MAKT  LIKE TABLE OF TG_MAKT,
*            WL_MAKT  LIKE LINE OF TG_MAKT,
*            TL_MARA  LIKE TABLE OF TG_MARA,
*            WL_MARA  LIKE LINE OF TG_MARA,
*            TL_T001W LIKE TABLE OF T001W,
*            WL_T001W LIKE LINE  OF TL_T001W,
*            TL_T006  TYPE TABLE OF T006,
*            WL_T006  TYPE T006,
*            TL_0038  TYPE TABLE OF ZSDT0038,
*            WL_0038  TYPE ZSDT0038,
*            TL_MARM  TYPE TABLE OF MARM,
*            WL_MARM  TYPE MARM,
**            tl_t001W type table of t001W,
**            Wl_t001W LIKE LINE OF t001W,
*            TL_CELL  TYPE LVC_T_CELL,
*            WL_LINHA(6).
*
*  REFRESH: TL_MARA, TL_MAKT, TL_T001W, TG_MSG_RET, TL_T006, TL_T001W, TL_0038, TL_MARM.
*  CLEAR: WL_MARA, WL_MAKT, WL_T001W, TG_MSG_RET, WL_T006, WL_T001W, WL_0038, WL_MARM.
*
*  IF TG_SAIDA[] IS NOT INITIAL.
**    select werks name1
**      from t001w
**      into table tl_t001w
**       for all entries in tg_saida
**        where werks eq tg_saida-werks.
*
*    SELECT MATNR MAKTX
*      FROM MAKT
*      INTO TABLE TL_MAKT
*       FOR ALL ENTRIES IN TG_SAIDA
*       WHERE MATNR EQ TG_SAIDA-MATNR.
*
*    SELECT MATNR MATKL MEINS
*      FROM MARA
*      INTO TABLE TL_MARA
*       FOR ALL ENTRIES IN TG_SAIDA
*       WHERE MATNR EQ TG_SAIDA-MATNR.
*
*    SELECT *
*      FROM MARM
*      INTO TABLE TL_MARM
*       FOR ALL ENTRIES IN TG_SAIDA
*       WHERE MATNR EQ TG_SAIDA-MATNR.
*
*    SELECT *
*      FROM T006
*      INTO TABLE TL_T006
*       FOR ALL ENTRIES IN TG_SAIDA
*       WHERE MSEHI EQ TG_SAIDA-MEINS.
*
*    SELECT *
*    FROM T001W
*    INTO TABLE TL_T001W
*     FOR ALL ENTRIES IN TG_SAIDA
*     WHERE WERKS EQ TG_SAIDA-WERKS_FORNEC.
*
*    SELECT *
*      FROM ZSDT0038
*      INTO TABLE TL_0038
*       FOR ALL ENTRIES IN TG_SAIDA
*        WHERE CULTURA EQ TG_SAIDA-CULTURA.
*  ENDIF.
*
*  SORT: TL_T001W  BY WERKS,
*        TL_MAKT   BY MATNR,
*        TL_MARA   BY MATNR,
*        TL_T006   BY MSEHI,
*        TL_MARM   BY MATNR MEINH,
*        TL_T001W  BY WERKS,
*        TL_0038   BY CULTURA.
*
*  LOOP AT TG_SAIDA.
**    PERFORM get_cell TABLES Tl_cell
**                     USING 'VAL_DE'
**                           WL_LINHA.
*    WL_LINHA = SY-TABIX.
*    IF TG_SAIDA-VAL_DE IS INITIAL.
**      move: "TEXT-E01            TO TG_MSG_RET-MSG,
**            c_tab_strip_nf-tab6 to tg_msg_ret-aba.
*      MOVE: 'VAL_DE' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' VAL_DE.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
*
*    IF TG_SAIDA-VAL_ATE IS INITIAL.
**    OR TG_SAIDA-VAL_ATE LT TG_SAIDA-VAL_DE.
*      MOVE: 'VAL_ATE' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' VAL_ATE.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ELSEIF TG_SAIDA-VAL_ATE LT TG_SAIDA-VAL_DE.
*      MOVE: 'VAL_ATE' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E02 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
*
*    IF TG_SAIDA-DTVENC IS INITIAL.
*      MOVE: 'DTVENC' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
**    if tg_saida-werks is initial.
**      move: 'WERKS' to tg_msg_ret-field,
**            'GRID1'  to tg_msg_ret-obj,
**             wl_linha to tg_msg_ret-tabix.
**
**      concatenate text-e01 ' WERKS.' ' LINHA: ' wl_linha into  tg_msg_ret-msg.
**      append tg_msg_ret.
**      clear: tg_msg_ret.
**    else.
**      read table tl_t001w transporting no fields
**        with key werks = tg_saida-werks
**                 binary search.
**      if sy-subrc is not initial.
**        move: 'WERKS' to tg_msg_ret-field,
**              'GRID1'  to tg_msg_ret-obj,
**              wl_linha to tg_msg_ret-tabix.
**
**        concatenate text-e03 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
**        append tg_msg_ret.
**        clear: tg_msg_ret.
**      endif.
**    endif.
*
*    IF TG_SAIDA-MATNR IS INITIAL.
*      MOVE: 'MATNR' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' MATNR.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ELSE.
*      READ TABLE TL_MARA TRANSPORTING NO FIELDS
*              WITH KEY MATNR = TG_SAIDA-MATNR
*                       BINARY SEARCH.
*      IF SY-SUBRC IS NOT INITIAL.
*        MOVE: 'MATNR' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*        CONCATENATE TEXT-E04 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.
*    ENDIF.
*
*    IF TG_SAIDA-MEINS IS INITIAL.
*      MOVE: 'MEINS' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' MEINS.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ELSE.
*      READ TABLE TL_MARM TRANSPORTING NO FIELDS
*              WITH KEY MATNR = TG_SAIDA-MATNR
*                       MEINH = TG_SAIDA-MEINS
*                       BINARY SEARCH.
*      IF SY-SUBRC IS NOT INITIAL.
*        MOVE: 'MEINS' TO TG_MSG_RET-FIELD,
*              'GRID1'  TO TG_MSG_RET-OBJ,
*              WL_LINHA TO TG_MSG_RET-TABIX.
*
*        CONCATENATE TEXT-E05 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.
*    ENDIF.
*
*    IF TG_SAIDA-WERKS_FORNEC IS INITIAL.
*      MOVE: 'WERKS_FORNEC' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' WERKS_FORNEC.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ELSE.
*      READ TABLE TL_T001W TRANSPORTING NO FIELDS
*              WITH KEY WERKS = TG_SAIDA-WERKS_FORNEC
*                       BINARY SEARCH.
*      IF SY-SUBRC IS NOT INITIAL.
*        MOVE: 'WERKS_FORNEC' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*        CONCATENATE TEXT-E05 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.
*    ENDIF.
*
*    IF TG_SAIDA-VLR_CUSTO IS INITIAL.
*      MOVE: 'VLR_CUSTO' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' VLR_CUSTO.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
*
*    IF TG_SAIDA-PERC_MARGEM IS INITIAL.
*      MOVE: 'PERC_MARGEM' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' PERC_MARGEM.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
*
*    IF TG_SAIDA-WAERK IS INITIAL.
*      MOVE: 'WAERK' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' WAERK.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
*
*    IF TG_SAIDA-CULTURA IS NOT INITIAL.
**      MOVE: 'CULTURA' TO TG_MSG_RET-FIELD,
**            'GRID1'  TO TG_MSG_RET-OBJ,
**            WL_LINHA TO TG_MSG_RET-TABIX.
**
**      CONCATENATE TEXT-E01 ' CULTURA.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
**      APPEND TG_MSG_RET.
**      CLEAR: TG_MSG_RET.
**    ELSE.
*      READ TABLE TL_0038 INTO WL_0038
*        WITH KEY CULTURA = TG_SAIDA-CULTURA
*                   BINARY SEARCH.
*      IF SY-SUBRC IS NOT INITIAL.
*        MOVE: 'CULTURA' TO TG_MSG_RET-FIELD,
*             'GRID1'  TO TG_MSG_RET-OBJ,
*             WL_LINHA TO TG_MSG_RET-TABIX.
*
*        CONCATENATE TEXT-E06 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.
ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  GET_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_CELLS  text
*----------------------------------------------------------------------*
FORM GET_CELL TABLES  TL_CELL TYPE LVC_T_CELL
              USING WL_CELL
                    WL_TABIX.
*  REFRESH: tl_cell.
*  MOVE : wl_cell  TO tl_cell-col_id-fieldname,
*         wl_tabix TO tl_cell-row_id-index.
*
*  APPEND
*  call method grid1->set_selected_cells
*    exporting
*      it_cell = tl_cell[].

ENDFORM.                    " GET_CELL
*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_DROPDOWN .
*  DATA:   LS_DROPDOWN      TYPE LVC_S_DROP,
*          LT_DROPDOWN      TYPE LVC_T_DROP,
*          TL_0038  TYPE TABLE OF ZSDT0038 WITH HEADER LINE.
*
*
*  LS_DROPDOWN-HANDLE = '1'.
*
*  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'FOB'.
*  GT_VALUES-DOMVALUE_L = 'FOB'.
*  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
*          GT_VALUES.
*
*  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'CPT'.
*  GT_VALUES-DOMVALUE_L = 'CPT'.
*  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
*          GT_VALUES.
*
*  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'CIF'.
*  GT_VALUES-DOMVALUE_L = 'CIF'.
*  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
*          GT_VALUES.


** Übergabe der Dropdown-Tabelle an ALV-Grid-Control
*  CALL METHOD GRID1->SET_DROP_DOWN_TABLE
*    EXPORTING
*      IT_DROP_DOWN = LT_DROPDOWN.
ENDFORM.                    " BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*&      Module  GET_LIFNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LIFNR INPUT.

  LCL_EVENT_HANDLER=>ON_ONF4( E_FIELDNAME = 'LIFNR' ).
ENDMODULE.                 " GET_LIFNR  INPUT
*&---------------------------------------------------------------------*
*&      Module  REFRESH_TOLERANCIA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE REFRESH_TOLERANCIA INPUT.
  DATA: WL_FIELD(30).
  FIELD-SYMBOLS: <FS_FIELD> TYPE ANY.

  UNASSIGN <FS_FIELD>.
  GET CURSOR FIELD WL_FIELD.

  ASSIGN (WL_FIELD) TO <FS_FIELD>.
  IF SY-DYNNR EQ '0100'.
    IF WL_FIELD EQ 'WG_INPUT-PESO_TOLER'.
      CLEAR: WG_INPUT-PERC_TOLER.
    ELSE.
      CLEAR: WG_INPUT-PESO_TOLER.
    ENDIF.

    PERFORM REFRESH_TOLERANCIA USING    WG_INPUT-PESO
                               CHANGING WG_INPUT-PERC_TOLER
                                        WG_INPUT-PESO_TOLER.
  ELSE.
    IF WL_FIELD EQ 'WG_EDIT-PESO_TOLER'.
      CLEAR: WG_EDIT-PERC_TOLER.
    ELSE.
      CLEAR: WG_EDIT-PESO_TOLER.
    ENDIF.

    PERFORM REFRESH_TOLERANCIA USING    WG_EDIT-PESO
                               CHANGING WG_EDIT-PERC_TOLER
                                        WG_EDIT-PESO_TOLER.
  ENDIF.

ENDMODULE.                 " REFRESH_TOLERANCIA  INPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_TOLERANCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_INPUT_PERC_TOLER  text
*      <--P_WG_INPUT_PESO_TOLER  text
*----------------------------------------------------------------------*
FORM REFRESH_TOLERANCIA  USING    P_PESO
                         CHANGING P_PERC_TOLER
                                  P_PESO_TOLER.

  IF P_PERC_TOLER IS INITIAL.
    TRY .
        P_PERC_TOLER = ( ( P_PESO_TOLER / P_PESO ) * 100 ) .
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

  ELSE.
    P_PESO_TOLER = P_PESO * ( P_PERC_TOLER / 100 ).
  ENDIF.
ENDFORM.                    " REFRESH_TOLERANCIA
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TEXTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_TEXTOS .

  CLEAR: WG_INPUT-KTEXT.
  SELECT SINGLE KTEXT
    FROM TCURT
    INTO WG_INPUT-KTEXT
     WHERE SPRAS EQ SY-LANGU
       AND WAERS EQ WG_INPUT-WAERK.

  CLEAR: WG_INPUT-MSEHL_PESO.
  SELECT SINGLE MSEHL
    FROM T006A
    INTO WG_INPUT-MSEHL_PESO
     WHERE SPRAS EQ SY-LANGU
       AND MSEHI EQ WG_INPUT-UNID_MEDIDA.

  CLEAR: WG_INPUT-MSEHL_PRECO.
  SELECT SINGLE MSEHL
    FROM T006A
    INTO WG_INPUT-MSEHL_PRECO
     WHERE SPRAS EQ SY-LANGU
       AND MSEHI EQ WG_INPUT-UNID_MED_NETPR.
ENDFORM.                    " BUSCA_TEXTOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'Z01'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE SY-UCOMM.
    WHEN C_CANCEL OR C_EXIT.
      LEAVE TO SCREEN 0.
    WHEN C_OK.
      READ TABLE TG_SAIDA INDEX WG_EDIT-INDEX.

      TG_SAIDA-PESO_TOLER = WG_EDIT-PESO_TOLER.
      TG_SAIDA-PERC_TOLER = WG_EDIT-PERC_TOLER.
      TG_SAIDA-peso       = WG_EDIT-peso.

      MODIFY TG_SAIDA INDEX WG_EDIT-INDEX.
      IF SY-SUBRC IS INITIAL.
        MESSAGE S836(SD) WITH 'O peso de tolerância foi atualizado.'.
      ELSE.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Erro ao atualizar peso de tolerância.'.
      ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
