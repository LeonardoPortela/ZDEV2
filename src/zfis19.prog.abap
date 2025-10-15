*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 10/01/2013                                              &*
*& Descrição: Cadastro de Grupo de usuário Fechamento Mensal          &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         10.01.2012                            &*
*&--------------------------------------------------------------------&*

REPORT  ZFIS19.
TYPE-POOLS: RMDI.
INCLUDE: <ICON>.
INCLUDE <CL_ALV_CONTROL>.
TYPES: BEGIN OF TY_T001W,
        WERKS TYPE T001W-WERKS,
        NAME1 TYPE T001W-NAME1,
       END OF TY_T001W,

       BEGIN OF TY_USREFUS,
         BNAME TYPE USREFUS-BNAME,
         USERALIAS TYPE USREFUS-USERALIAS,
       END OF TY_USREFUS,

*       BEGIN OF TY_MARA,
*         MATNR TYPE MARA-MATNR,
*         MATKL TYPE MARA-MATKL,
*         MEINS TYPE MARA-MEINS,
*       END OF TY_MARA,

       BEGIN OF TY_SAIDA,
         MARK,
*         STATUS(4),       "type zsdt0036-loekz,
*         VAL_DE       TYPE ZSDT0036-VAL_DE,
*         VAL_ATE      TYPE ZSDT0036-VAL_ATE,
*         DTVENC       TYPE ZSDT0036-DTVENC,
**         werks       type zsdt0036-werks,
**         name1       type t001w-name1,
*         MATNR        TYPE ZSDT0036-MATNR,
*         CULTURA      TYPE ZSDT0036-CULTURA,
*         MAKTX        TYPE MAKT-MAKTX,
*         MEINS        TYPE ZSDT0036-MEINS,
*         WERKS_FORNEC TYPE ZSDT0036-WERKS_FORNEC,
*         MATKL        TYPE MARA-MATKL,
*         WAERK        TYPE ZSDT0036-WAERK,
*         VLR_CUSTO    TYPE ZSDT0036-VLR_CUSTO,
*         INCO1        TYPE ZSDT0036-INCO1,
*         PERC_MARGEM  TYPE ZSDT0036-PERC_MARGEM,
*         VLR_MARGEM   TYPE ZSDT0036-VLR_MARGEM,
*         VLR_VENDA    TYPE ZSDT0036-VLR_VENDA,
         GRUPO        TYPE ZFIT0032-GRUPO,
         DESC_GRUPO   TYPE ZFIT0032T-DESC_GRUPO,
         BUKRS        TYPE ZFIT0032-BUKRS,
         MITKZ        TYPE ZFIT0032-MITKZ,
         SAKNR        TYPE ZFIT0032-SAKNR,
         USNAM        TYPE ZFIT0032-USNAM,
         DESC_USNAM(40),
         DATA_LIM     TYPE ZFIT0032-DATA_LIM,
         HORA_LIM     TYPE ZFIT0032-HORA_LIM,
         CELLCOLORS   TYPE LVC_T_SCOL,
         STYLE2       TYPE LVC_T_STYL,
       END OF TY_SAIDA.

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
DATA: TG_0032 TYPE TABLE OF ZFIT0032 WITH HEADER LINE,
      TG_0032T TYPE TABLE OF ZFIT0032T WITH HEADER LINE,
*      TG_T001W TYPE TABLE OF TY_T001W WITH HEADER LINE,
*      TG_MARA  TYPE TABLE OF TY_MARA WITH HEADER LINE,
      TG_USREFUS  TYPE TABLE OF TY_USREFUS WITH HEADER LINE,
      TG_SAIDA TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      OK_CODE TYPE SY-UCOMM,
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
*    DATA: TL_ITENS_AUX LIKE TABLE OF TG_ITENS,
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
        APPEND INITIAL LINE TO TG_SAIDA.

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
          DELETE TG_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

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
          WL_USREFUS  LIKE LINE OF TG_USREFUS,
          WL_0032T  LIKE LINE OF TG_0032T,
*          WL_T001W LIKE LINE OF TG_T001W,
          WL_SAIDA LIKE LINE OF TG_SAIDA.
*          WL_CALCULO TYPE ZSDT0036-VLR_MARGEM,
*          WL_VLR_VENDA TYPE ZSDT0036-VLR_VENDA,
*          WL_VLR_CUSTO TYPE ZSDT0036-VLR_CUSTO,
*          WL_PERC_MARGEM TYPE ZSDT0036-PERC_MARGEM.
*          wl_mch1  type mch1,
*          wl_itens like line of tg_itens,
*          wl_where(30),
*          wl_marc type marc,
*          wl_mbew type mbew,
*          wl_1bbranch type j_1bbranch,
*          wl_1baa type j_1baa,
*          wl_1bapn type  j_1bapn,
*          wl_direct type j_1bapnv-direct,
*          wl_dstcat type j_1bapnv-dstcat..
*
*
**BREAK ABAP.
    CLEAR: WL_USREFUS,  WL_SAIDA, LV_VALUE.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'USNAM'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE BNAME USERALIAS
        FROM USREFUS
        INTO WL_USREFUS
         WHERE BNAME EQ LV_VALUE.

      MOVE: WL_USREFUS-USERALIAS TO LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'DESC_USNAM'
          I_VALUE     = LV_VALUE.



      CLEAR: WL_USREFUS, LV_VALUE.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'GRUPO'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.

      SELECT SINGLE *
        FROM ZFIT0032T
        INTO WL_0032T
         WHERE GRUPO EQ LV_VALUE.

      MOVE: WL_0032T-DESC_GRUPO TO LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'DESC_GRUPO'
          I_VALUE     = LV_VALUE.



      CLEAR: WL_0032T, LV_VALUE.
    ENDLOOP.
*    loop at er_data_changed->mt_good_cells
*                             into ls_good
*                             where fieldname = 'WERKS'.
*      lv_value = ls_good-value.
*      condense lv_value no-gaps.
**
*      select single werks name1
*        from t001w
*        into wl_t001w
*          where werks eq lv_value.
*
*      move: wl_t001w-name1 to lv_value.
*
*      call method er_data_changed->modify_cell
*        exporting
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'NAME1'
*          i_value     = lv_value.
*
*      clear: wl_t001w, lv_value.
*    endloop.

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
*
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

*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                             INTO LS_GOOD
*                             WHERE FIELDNAME = 'VLR_CUSTO'.
**                                OR FIELDNAME = 'VLR_MARGEM'.
*
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*
*      LV_VALUE = LS_GOOD-VALUE.
*      CONDENSE LV_VALUE NO-GAPS.
**      BREAK-POINT.
**      IF LS_GOOD-FIELDNAME EQ 'VLR_CUSTO'.
*        WL_VLR_VENDA =  WL_SAIDA-VLR_MARGEM + LV_VALUE.
**      ELSE.
**        WL_VLR_VENDA = LV_VALUE + WL_SAIDA-VLR_CUSTO.
**      ENDIF.
*
*      MOVE: WL_VLR_VENDA TO LV_VALUE.
*
*      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*        EXPORTING
*          I_ROW_ID    = LS_GOOD-ROW_ID
*          I_FIELDNAME = 'VLR_VENDA'
*          I_VALUE     = LV_VALUE.
*
*      CLEAR: WL_VLR_VENDA, WL_SAIDA, LV_VALUE.
*    ENDLOOP.
*        move: wl_mara-meins to lv_value.
*
*        call method er_data_changed->modify_cell
*          exporting
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'MEINS'
*            i_value     = lv_value.
*      else.
*        clear: lv_value.
*
*        call method er_data_changed->modify_cell
*          exporting
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'MATNR'
*            i_value     = lv_value.
*        message s836(sd) display like 'E' with 'Material selecionado não foi encontrado!'.
*      endif.
*
*    endloop.
*
*    loop at er_data_changed->mt_good_cells
*                         into ls_good
*                         where fieldname = 'WERKS'.
*      lv_value = ls_good-value.
*      condense lv_value no-gaps.
*
*      select single *
*        from t001w
*        into wl_t001w
*         where werks eq lv_value.
*
*      if sy-subrc is initial.
**      select *
*      else.
*        clear: lv_value.
*
*        call method er_data_changed->modify_cell
*          exporting
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'WERKS'
*            i_value     = lv_value.
*        message s836(sd) display like 'E' with 'Centro/Filial não encontrado.'.
*      endif.
*
*    endloop.
*    loop at er_data_changed->mt_good_cells
*                         into ls_good
*                         where fieldname = 'CHARG'.
*      lv_value = ls_good-value.
*      condense lv_value no-gaps.
*      read table tg_itens into wl_itens index ls_good-tabix.
*      if wl_itens is not initial.
*        wl_where = 'MATNR EQ WL_ITENS-MATNR'.
*      endif.
*      if lv_value is not initial.
*        .
*
*        if sy-subrc is initial.
**      select *
*        else.
*          clear: lv_value.
*
*          call method er_data_changed->modify_cell
*            exporting
*              i_row_id    = ls_good-row_id
*              i_fieldname = 'CHARG'
*              i_value     = lv_value.
*          message s836(sd) display like 'E' with 'Não existe lote para este material.'.
*        endif.
*      endif.
*    endloop.
*
**    LOOP AT er_data_changed->mt_good_cells
**                             INTO ls_good.
**
**      READ TABLE tg_itens INTO wl_itens INDEX ls_good-ROW_ID.
*****> Determina o CFOP
**      IF wg_direitos-cfop IS INITIAL.
**        SELECT SINGLE *
**          FROM marc
**          INTO wl_marc
**           WHERE matnr EQ wl_itens-matnr.
**
**        IF sy-subrc IS INITIAL.
**          SELECT SINGLE *
**            FROM mbew
**            INTO wl_mbew
**             WHERE matnr EQ wl_itens-matnr
**               AND bwkey EQ wl_itens-werks.
**
**          IF sy-subrc IS INITIAL.
**            SELECT SINGLE *
**              FROM j_1bbranch
**               INTO wl_1bbranch
**               WHERE bukrs  EQ p_bukrs
**                 AND branch EQ p_branch.
**
**            IF sy-subrc IS INITIAL.
**              SELECT SINGLE *
**                FROM j_1baa
**                INTO wl_1baa
**                 WHERE nftype EQ wg_fiscal-nftype.
**
**              IF wl_1baa-entrad EQ c_x.
**                wl_direct = c_1.
**              ELSE.
**                wl_direct = c_2.
**              ENDIF.
**
**              IF wg_direitos-indcoper EQ c_d.
**                wl_dstcat = c_0.
**
**              ELSE.
**                wl_dstcat = c_1.
**
**              ENDIF.
**
**              SELECT SINGLE *
**                FROM j_1bapn
**                INTO wl_1bapn
**                 WHERE direct EQ wl_direct
**                   AND dstcat EQ wl_dstcat
**                   AND indus3 EQ wl_marc-indus
**                   AND itmtyp EQ wg_fiscal-itmtyp
**                   AND ownpro EQ wl_mbew-ownpr
**                   AND matuse EQ wl_mbew-mtuse
**                   AND indus1 EQ wl_1bbranch-industry.
**
**              IF sy-subrc IS INITIAL.
**                lv_value = wl_1bapn-cfop.
**              ENDIF.
**            ENDIF.
**          ENDIF.
**        ENDIF.
**      ELSE.
**        lv_value = wg_direitos-cfop.
**
**      ENDIF.
**      CALL METHOD er_data_changed->modify_cell
**        EXPORTING
**          i_row_id    = ls_good-row_id
**          i_fieldname = 'CFOP'
**          i_value     = lv_value.
**
**      WL_ITENS-NETWR = wl_itens-menge * wl_itens-netpr.
**      lv_value = WL_ITENS-NETWR.
**      CONDENSE LV_VALUE.
**      CALL METHOD er_data_changed->modify_cell
**        EXPORTING
**          i_row_id    = ls_good-row_id
**          i_fieldname = 'NETWR'
**          i_value     = lv_value.
**    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD ON_DATA_CHANGED_FINISHED.
    DATA: TL_SAIDA LIKE TABLE OF TG_SAIDA,
          WL_SAIDA LIKE LINE OF TG_SAIDA,
          LS_GOOD  TYPE LVC_S_MODI,
          TL_USREFUS  LIKE TABLE OF TG_USREFUS,
          WL_USREFUS  LIKE LINE OF TG_USREFUS,
          TL_0032T  LIKE TABLE OF TG_0032T,
          WL_0032T  LIKE LINE OF TG_0032T.
*           lv_value type lvc_value,
*           wl_marc type marc,
*           wl_mbew type mbew,
*           wl_1bbranch type j_1bbranch,
*           wl_1baa type j_1baa,
*           wl_1bapn type  j_1bapn,
*           wl_direct type j_1bapnv-direct,
*           wl_dstcat type j_1bapnv-dstcat.
**    BREAK ABAP.
*
    REFRESH: TL_USREFUS, TL_SAIDA.

    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
       WHERE TABIX GT 0.

      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.

      APPEND WL_SAIDA TO TL_SAIDA.
    ENDLOOP.

    IF TL_SAIDA[] IS NOT INITIAL.
*      select werks name1
*        from t001w
*        into table tl_t001w
*         for all entries in tl_saida
*          where werks eq tl_saida-werks.

      SELECT BNAME USERALIAS
        FROM USREFUS
        INTO TABLE TL_USREFUS
         FOR ALL ENTRIES IN TL_SAIDA
         WHERE BNAME EQ TL_SAIDA-USNAM.

      SELECT *
        FROM ZFIT0032T
        INTO TABLE TL_0032T
         FOR ALL ENTRIES IN TL_SAIDA
         WHERE GRUPO EQ TL_SAIDA-GRUPO.

    ENDIF.
    SORT: TL_USREFUS BY BNAME.
    SORT: TL_0032T BY GRUPO.
*          tl_t001w by werks.

    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
      WHERE TABIX GT 0.

      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.

      READ TABLE TL_USREFUS INTO WL_USREFUS
        WITH KEY BNAME = WL_SAIDA-USNAM
                 BINARY SEARCH.

      READ TABLE TL_0032T INTO WL_0032T
        WITH KEY GRUPO = WL_SAIDA-GRUPO
                 BINARY SEARCH.


*      read table tl_t001w into wl_t001w
*        with key werks = wl_saida-werks
*                 binary search.

      MOVE: WL_USREFUS-USERALIAS TO WL_SAIDA-DESC_USNAM,
            WL_0032T-DESC_GRUPO  TO WL_SAIDA-DESC_GRUPO.
*            WL_SAIDA-MEINS TO WL_SAIDA-meins,
*            wl_t001w-name1 to wl_saida-name1.

      MODIFY TG_SAIDA FROM WL_SAIDA INDEX LS_GOOD-ROW_ID.
      CLEAR: WL_SAIDA, WL_USREFUS, WL_0032T.
    ENDLOOP.

**    refresh: style2, tg_saida-style2.
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
*      read table tg_itens into wl_itens index ls_good-row_id.
*      if sy-subrc is initial.
*
**    LOOP AT tg_itens INTO wl_itens.
****> Determina o CFOP
*        if wg_direitos-cfop is initial.
*          select single *
*            from marc
*            into wl_marc
*             where matnr eq wl_itens-matnr
*               and werks eq p_branch.
*
*          if sy-subrc is initial.
*            wl_itens-steuc = wl_marc-steuc.
*            select single *
*              from mbew
*              into wl_mbew
*               where matnr eq wl_itens-matnr
*                 and bwkey eq wl_itens-werks.
*
*            if sy-subrc is initial.
*              select single *
*                from j_1bbranch
*                 into wl_1bbranch
*                 where bukrs  eq p_bukrs
*                   and branch eq p_branch.
*
*              if sy-subrc is initial.
*                select single *
*                  from j_1baa
*                  into wl_1baa
*                   where nftype eq wg_fiscal-nftype.
*
*                if wl_1baa-entrad eq c_x.
*                  wl_direct = c_1.
*                else.
*                  wl_direct = c_2.
*                endif.
*
*                if wg_direitos-indcoper eq c_d.
*                  wl_dstcat = c_0.
*
*                else.
*                  wl_dstcat = c_1.
*
*                endif.
*
*                select single *
*                  from j_1bapn
*                  into wl_1bapn
*                   where direct eq wl_direct
*                     and dstcat eq wl_dstcat
*                     and indus3 eq wl_marc-indus
*                     and itmtyp eq wg_fiscal-itmtyp
*                     and ownpro eq wl_mbew-ownpr
*                     and matuse eq wl_mbew-mtuse
*                     and indus1 eq wl_1bbranch-industry.
*
*                if sy-subrc is initial.
*                  wl_itens-cfop = wl_1bapn-cfop.
*                else.
*                  clear: wl_itens-cfop.
*                endif.
*              else.
*                clear: wl_itens-cfop.
*              endif.
*            else.
*              clear: wl_itens-cfop.
*            endif.
*          else.
*            clear: wl_itens-cfop, wl_itens-steuc.
*          endif.
*        else.
*          select single *
*            from marc
*            into wl_marc
*             where matnr eq wl_itens-matnr
*               and werks eq p_branch.
*
*          wl_itens-steuc = wl_marc-steuc.
*          wl_itens-cfop = wg_direitos-cfop.
*        endif.
*
*        wl_itens-netwr = wl_itens-menge * wl_itens-netpr.
*        modify tg_itens from wl_itens index ls_good-row_id.
*
*      endif.
*    endloop.
*
**** Método de atualização de dados na Tela
*    call method grid1->refresh_table_display
*      exporting
*        is_stable = wa_stable.
*
*    perform verifica_erros.

  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD ON_BUTTON_CLICK.
    DATA: WL_SAIDA LIKE LINE OF TG_SAIDA.
*    es_col_id
    IF ES_ROW_NO-ROW_ID GT 0.
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX ES_ROW_NO-ROW_ID.
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
*      MODIFY TG_SAIDA FROM WL_SAIDA INDEX ES_ROW_NO-ROW_ID.
    ENDIF.
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDMETHOD.                    "on_button_click
  METHOD ON_ONF4.
*    TYPES: BEGIN OF TYL_FIELD,
*            TABNAME TYPE DD03L-TABNAME,    "Nome da tabela
*            FIELDNAME TYPE DD03L-FIELDNAME,    "Nome de campo
*            S(1) TYPE C,
*           END OF TYL_FIELD,
*
*           BEGIN OF TYL_VALUE,
*            TABNAME TYPE DD03L-TABNAME,    "Nome da tabela
*            FIELDNAME TYPE DD03L-FIELDNAME,    "Nome de campo
*            CHAR79(79) TYPE C,
*           END OF TYL_VALUE.
*
*    DATA: BEGIN OF WL_CULTURA,
*                FIELD(50),
*          END OF WL_CULTURA.
*    DATA: TL_CULTURA   LIKE TABLE OF WL_CULTURA,
*          TL_0038 TYPE  TABLE OF ZSDT0038,
*          WL_0038 TYPE ZSDT0038,
*          TL_FIELD       TYPE TABLE OF TYL_FIELD,
*          WL_FIELD       TYPE TYL_FIELD,
*          TL_VALUE       TYPE TABLE OF TYL_VALUE,
*          WL_VALUE       TYPE TYL_VALUE,
*          WL_CHAR(20),
*          WL_INDEX       TYPE SY-TABIX.
*
*
*    SELECT *
*      FROM ZSDT0038
*      INTO TABLE TL_0038.
*
*    LOOP AT TL_0038 INTO WL_0038.
*
*      MOVE: WL_0038-CULTURA TO WL_CULTURA-FIELD.
*      APPEND WL_CULTURA TO TL_CULTURA.
*
*      MOVE: WL_0038-DESCRICAO TO WL_CULTURA-FIELD.
*      APPEND WL_CULTURA TO TL_CULTURA.
*    ENDLOOP.
*
*    WL_FIELD-TABNAME = 'ZSDT0038'.
*    WL_FIELD-FIELDNAME = 'CULTURA'.
*    WL_FIELD-S = 'X'.
*    APPEND WL_FIELD TO TL_FIELD.
*
*    WL_FIELD-TABNAME = 'ZSDT0038'.
*    WL_FIELD-FIELDNAME = 'DESCRICAO'.
*    WL_FIELD-S = ' '.
*    APPEND WL_FIELD TO TL_FIELD.
*
*    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
*      EXPORTING
**        cucol                     = '3'
*        FIELDNAME                 = 'CULTURA'
*        TABNAME                   = 'ZSDT0038'
*      IMPORTING
*        INDEX                     = WL_INDEX
*        SELECT_VALUE              = WL_CHAR
*      TABLES
*        FIELDS                    = TL_FIELD
*        SELECT_VALUES             = TL_VALUE
*        VALUETAB                  = TL_CULTURA
*      EXCEPTIONS
*        FIELD_NOT_IN_DDIC         = 001
*        MORE_THEN_ONE_SELECTFIELD = 002
*        NO_SELECTFIELD            = 003.
*
*    IF SY-SUBRC IS INITIAL.
*      READ TABLE TL_0038 INTO WL_0038 INDEX WL_INDEX.
*      IF ES_ROW_NO-ROW_ID GT 0.
*        READ TABLE TG_SAIDA INTO TG_SAIDA INDEX ES_ROW_NO-ROW_ID.
*        IF SY-SUBRC IS INITIAL.
*          MOVE: WL_0038-CULTURA TO TG_SAIDA-CULTURA.
*          MODIFY TG_SAIDA FROM TG_SAIDA INDEX ES_ROW_NO-ROW_ID.
*        ENDIF.
*      ENDIF.
*    ENDIF.

**** Método de atualização de dados na Tela
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*      EXPORTING
*        FUNCTIONCODE           = '/00'
*      EXCEPTIONS
*        FUNCTION_NOT_SUPPORTED = 1.
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
    APPEND C_SAVE TO FCODE.

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
        LT_F4        TYPE LVC_T_F4 WITH HEADER LINE.

  WL_REPID = SY-REPID.

  IF CONTAINER1 IS INITIAL.
    WA_LAYOUT-ZEBRA      = C_X.
*    wa_layout-no_rowmark = c_x.
*    wa_layout-cwidth_opt = c_x.
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

    PERFORM MONTAR_LAYOUT.
*    PERFORM BUILD_DROPDOWN.

    WA_LAYOUT-CTAB_FNAME = 'CELLCOLORS'.
    WA_LAYOUT-STYLEFNAME = 'STYLE2'.

*    LT_F4-FIELDNAME = 'CULTURA'.
*    LT_F4-REGISTER = 'X' .
*    LT_F4-GETBEFORE = 'X' .
*    APPEND LT_F4 .

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
      CHANGING
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
  DATA: WL_VIEW_NAME TYPE OCUS-TABLE VALUE 'ZFIT0032',
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
*SW: interne Meldung 0000293299 1995
*         message i049 with sy-msgv1 raising foreign_lock.
*         move: s to action.
*XB: Change to new function moduls POPUP_TO_DECIDE_LOCKED_DATA -Begin
*          text = text-101.
*          REPLACE '&' WITH sy-msgv1(12) INTO text.
*          CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
*            EXPORTING
*              titel          = text-100
*              diagnosetext1  = text
*              diagnosetext2  = text-102
*              textline1      = text-103
*              defaultoption  = 'Y'
*              cancel_display = space
*            IMPORTING
*              answer         = answer.
*          IF answer = 'A' OR answer = 'N'.
*            MESSAGE e049 WITH sy-msgv1 RAISING foreign_lock.
*          ELSEIF answer = 'J'.
*            MOVE: s TO action.
*          ENDIF.
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

  SELECT *
    FROM ZFIT0032
    INTO TABLE TG_0032.

  IF SY-SUBRC IS INITIAL.
*    select werks name1
*      from t001w
*      into table tg_t001w
*       for all entries in tg_0036
*        where werks eq tg_0036-werks.

    SELECT BNAME USERALIAS
      FROM USREFUS
      INTO TABLE TG_USREFUS
       FOR ALL ENTRIES IN TG_0032
       WHERE BNAME EQ TG_0032-USNAM.

    SELECT *
      FROM ZFIT0032T
      INTO TABLE TG_0032T
       FOR ALL ENTRIES IN TG_0032
       WHERE GRUPO EQ TG_0032-GRUPO.

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
        1 'ZFIT0032'  'GRUPO'       'TG_SAIDA' 'GRUPO'       'Grupo'  '8' 'X' ' ' ' ',
        1 'ZFIT0032T' 'DESC_GRUPO'  'TG_SAIDA' 'DESC_GRUPO'  ' '  '25' ' ' ' ' ' ',
        1 'ZFIT0032'  'BUKRS'       'TG_SAIDA' 'BUKRS'       ' '  '8' 'X' ' ' ' ',
        1 'ZFIT0032'  'MITKZ'       'TG_SAIDA' 'MITKZ'       ' '  '7' 'X' ' ' ' ',
        3 'ZFIT0032'  'SAKNR'       'TG_SAIDA' 'SAKNR'       ' '  '12' 'X' '' ' ',
        3 'ZFIT0032'  'USNAM'       'TG_SAIDA' 'USNAM'       ' '  '15' 'X' '' ' ',
        4 'USREFUS'   'USERALIAS'   'TG_SAIDA' 'DESC_USNAM'  'Nome de Usuário'  ' ' ' ' ' ' ' '.
  "4 'ZFIT0032'  'DATA_LIM'    'TG_SAIDA' 'DATA_LIM'    'Data Limite'  '10' 'X' ' ' ' ',
  "4 'ZFIT0032'  'HORA_LIM'    'TG_SAIDA' 'HORA_LIM'    'Hora Limite'  '10' 'X' ' ' ' '.

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
  IF P_FIELD EQ 'GRUPO'.
    W_FIELDCATALOG-F4AVAILABL     = 'X'.
  ENDIF.
*  IF p_field EQ 'STATUS'.
**    w_fieldcatalog-checkbox = c_x.
**    w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
**    w_fieldcatalog-edit          = c_x.
*    IF wg_display IS INITIAL.
*      w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
*    ELSE.
**      w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_disabled.
*    ENDIF.
*  ENDIF.

*  IF p_field EQ 'MEINS'.
**    w_fieldcatalog-checktable = 'T006'.
**    w_fieldcatalog-F4AVAILABL = C_X.
*  ENDIF.
*
*  IF p_field EQ 'INCO1'.
*    w_fieldcatalog-drdn_hndl  = 1.
*  ENDIF.
*
*  IF p_field EQ 'CULTURA'.
*    w_fieldcatalog-f4availabl  = 'X'.
*  ENDIF.
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
    WHEN C_SAVE.
      CALL METHOD GRID1->CHECK_CHANGED_DATA.
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
      ELSE.
        PERFORM GRAVA_DADOS.
      ENDIF.
    WHEN C_SEARCH.
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

    WHEN C_BACK.
      LEAVE TO SCREEN 0.
    WHEN C_CANCEL.
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
  SORT: TG_USREFUS BY BNAME.
  SORT: TG_0032T BY GRUPO.

  LOOP AT TG_0032.
    READ TABLE TG_USREFUS
      WITH KEY BNAME = TG_0032-USNAM
               BINARY SEARCH.

    READ TABLE TG_0032T
     WITH KEY GRUPO = TG_0032-GRUPO
              BINARY SEARCH.

    MOVE: TG_0032-GRUPO          TO TG_SAIDA-GRUPO,
          TG_0032T-DESC_GRUPO    TO TG_SAIDA-DESC_GRUPO,
          TG_0032-BUKRS          TO TG_SAIDA-BUKRS,
          TG_0032-MITKZ          TO TG_SAIDA-MITKZ,
          TG_0032-SAKNR          TO TG_SAIDA-SAKNR,
          TG_0032-USNAM          TO TG_SAIDA-USNAM,
          "tg_0032-data_lim       TO tg_saida-data_lim,
          "tg_0032-hora_lim       TO tg_saida-hora_lim,
          TG_USREFUS-USERALIAS   TO TG_SAIDA-DESC_USNAM.

*    IF TG_0036-LOEKZ EQ C_X.
*      MOVE: ICON_LOCKED     TO  TG_SAIDA-STATUS.
*    ELSE.
*      MOVE: ICON_UNLOCKED    TO  TG_SAIDA-STATUS.
*    ENDIF.

    APPEND TG_SAIDA.
    CLEAR: TG_SAIDA, TG_0032, TG_USREFUS, TG_0032T.

  ENDLOOP.

*  SORT: TG_SAIDA BY MATNR.
*
*  REFRESH: STYLE2, TG_SAIDA-STYLE2.
*  LOOP AT TG_SAIDA.
*
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
  DATA: TL_INPUT_0032 TYPE TABLE OF ZFIT0032 WITH HEADER LINE.

  CLEAR: TL_INPUT_0032.
  REFRESH: TL_INPUT_0032.
  DELETE FROM ZFIT0032.
  DELETE TG_SAIDA WHERE GRUPO IS INITIAL
                    AND BUKRS  IS INITIAL
                    AND MITKZ  IS INITIAL
                    AND SAKNR  IS INITIAL
                    AND USNAM  IS INITIAL
                    AND DATA_LIM  IS INITIAL
                    AND HORA_LIM  IS INITIAL.

  LOOP AT TG_SAIDA.
    MOVE-CORRESPONDING: TG_SAIDA TO TL_INPUT_0032.
    CLEAR: TL_INPUT_0032-DATA_LIM,TL_INPUT_0032-HORA_LIM.

*    IF TG_SAIDA-STATUS EQ ICON_LOCKED.
*      MOVE : C_X TO TL_INPUT_0036-LOEKZ.
*    ELSE.
*      MOVE : SPACE TO TL_INPUT_0036-LOEKZ.
*    ENDIF.
    APPEND TL_INPUT_0032.
    CLEAR: TL_INPUT_0032.

  ENDLOOP.

  MODIFY ZFIT0032 FROM TABLE TL_INPUT_0032.
  IF SY-SUBRC IS INITIAL.
    MESSAGE S836(SD) WITH 'Os dados foram salvos!'.
  ELSE.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Ocorreu um erro na gravação,'
                                           'verificar as entradas!'.
  ENDIF.
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
  DATA:     TL_USREFUS  LIKE TABLE OF TG_USREFUS,
            WL_USREFUS  LIKE LINE OF TG_USREFUS,
            TL_0032T    LIKE TABLE OF TG_0032T,
            WL_0032T    LIKE LINE OF TG_0032T,
            TL_SKB1     TYPE TABLE OF SKB1,
            WL_SKB1     TYPE SKB1,
            TL_T001     TYPE TABLE OF T001,
            WL_T001     TYPE T001,
*            tl_t001W type table of t001W,
*            Wl_t001W LIKE LINE OF t001W,
            TL_CELL  TYPE LVC_T_CELL,
            WL_LINHA(6).

  REFRESH: TL_USREFUS, TG_MSG_RET, TL_0032T, TL_SKB1.
  CLEAR: WL_USREFUS, TG_MSG_RET, WL_0032T, WL_SKB1.

  IF TG_SAIDA[] IS NOT INITIAL.
*    select werks name1
*      from t001w
*      into table tl_t001w
*       for all entries in tg_saida
*        where werks eq tg_saida-werks.

    SELECT BNAME USERALIAS
      FROM USREFUS
      INTO TABLE TL_USREFUS
       FOR ALL ENTRIES IN TG_SAIDA
       WHERE BNAME EQ TG_SAIDA-USNAM.

    SELECT *
      FROM ZFIT0032T
      INTO TABLE TL_0032T
       FOR ALL ENTRIES IN TG_SAIDA
        WHERE GRUPO EQ TG_SAIDA-GRUPO.

    SELECT *            "#EC CI_DB_OPERATION_OK[2431747]
      FROM SKB1
      INTO TABLE TL_SKB1
       FOR ALL ENTRIES IN TG_SAIDA
        WHERE BUKRS EQ TG_SAIDA-BUKRS
          AND SAKNR EQ TG_SAIDA-SAKNR.

    SELECT *
      FROM T001
      INTO TABLE TL_T001
       FOR ALL ENTRIES IN TG_SAIDA
        WHERE BUKRS EQ TG_SAIDA-BUKRS.

  ENDIF.

  SORT: TL_USREFUS BY BNAME.
  SORT: TL_SKB1 BY BUKRS SAKNR.
  SORT: TL_0032T BY GRUPO.
  SORT: TL_T001 BY BUKRS.

  LOOP AT TG_SAIDA.
*    PERFORM get_cell TABLES Tl_cell
*                     USING 'VAL_DE'
*                           WL_LINHA.
    WL_LINHA = SY-TABIX.
    IF TG_SAIDA-GRUPO IS INITIAL.
*      move: "TEXT-E01            TO TG_MSG_RET-MSG,
*            c_tab_strip_nf-tab6 to tg_msg_ret-aba.
      MOVE: 'GRUPO' TO TG_MSG_RET-FIELD,
            'GRID1'  TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE TEXT-E01 ' GRUPO.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE TL_0032T TRANSPORTING NO FIELDS
              WITH KEY GRUPO = TG_SAIDA-GRUPO
                       BINARY SEARCH.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: 'GRUPO' TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
              WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE TEXT-E05 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.

    ENDIF.

    IF TG_SAIDA-BUKRS IS INITIAL.
*      move: "TEXT-E01            TO TG_MSG_RET-MSG,
*            c_tab_strip_nf-tab6 to tg_msg_ret-aba.
      MOVE: 'BUKRS' TO TG_MSG_RET-FIELD,
            'GRID1'  TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE TEXT-E01 ' BUKRS.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.

    ELSE.
      READ TABLE TL_T001 TRANSPORTING NO FIELDS
              WITH KEY BUKRS = TG_SAIDA-BUKRS
                       BINARY SEARCH.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: 'BUKRS' TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
              WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE TEXT-E04 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.

      ENDIF.
    ENDIF.

*    IF tg_saida-data_lim IS INITIAL.
**      move: "TEXT-E01            TO TG_MSG_RET-MSG,
**            c_tab_strip_nf-tab6 to tg_msg_ret-aba.
*      MOVE: 'DATA_LIM' TO tg_msg_ret-field,
*            'GRID1'  TO tg_msg_ret-obj,
*            wl_linha TO tg_msg_ret-tabix.
*
*      CONCATENATE text-e01 ' DATA_LIM.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.
*
    IF TG_SAIDA-SAKNR IS NOT INITIAL.
      READ TABLE TL_SKB1 TRANSPORTING NO FIELDS
             WITH KEY BUKRS = TG_SAIDA-BUKRS
                      SAKNR = TG_SAIDA-SAKNR
                      BINARY SEARCH.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: 'SAKNR' TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
              WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE TEXT-E03 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF TG_SAIDA-USNAM IS INITIAL.
*      move: "TEXT-E01            TO TG_MSG_RET-MSG,
*            c_tab_strip_nf-tab6 to tg_msg_ret-aba.
      MOVE: 'USNAM' TO TG_MSG_RET-FIELD,
            'GRID1'  TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE TEXT-E01 ' USNAM.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE TL_USREFUS TRANSPORTING NO FIELDS
        WITH KEY BNAME = TG_SAIDA-USNAM
                 BINARY SEARCH.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: 'USNAM' TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
              WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE TEXT-E02 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

  ENDLOOP.
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
  DATA:   LS_DROPDOWN      TYPE LVC_S_DROP,
          LT_DROPDOWN      TYPE LVC_T_DROP,
          TL_0038  TYPE TABLE OF ZSDT0038 WITH HEADER LINE.


  LS_DROPDOWN-HANDLE = '1'.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'FOB'.
  GT_VALUES-DOMVALUE_L = 'FOB'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'CPT'.
  GT_VALUES-DOMVALUE_L = 'CPT'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

  LS_DROPDOWN-VALUE = GT_VALUES-DDTEXT = 'CIF'.
  GT_VALUES-DOMVALUE_L = 'CIF'.
  APPEND: LS_DROPDOWN TO LT_DROPDOWN,
          GT_VALUES.

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
      IT_DROP_DOWN = LT_DROPDOWN.
ENDFORM.                    " BUILD_DROPDOWN
