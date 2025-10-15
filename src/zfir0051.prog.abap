*&---------------------------------------------------------------------*
*& Report  ZFIR0051
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFIR0051.
TYPE-POOLS: RMDI.
INCLUDE: <ICON>.
INCLUDE <CL_ALV_CONTROL>.

TYPES: BEGIN OF TY_SAIDA,
       MARK,
       SAKNR TYPE ZFIT0078-SAKNR,
       TXT50 TYPE SKAT-TXT50,
       COD_FLX TYPE ZFIT0078-COD_FLX,
       DESC_FLX TYPE ZFIT0077-DESC_FLX,
       BSCHL TYPE ZFIT0078-BSCHL,
       MATKL TYPE ZFIT0078-MATKL,
       PRCTR TYPE ZFIT0078-PRCTR,
       KIDNO TYPE ZFIT0078-KIDNO,
       ZUONR TYPE ZFIT0078-ZUONR,
       BLART TYPE ZFIT0078-BLART,
       RMVCT TYPE ZFIT0078-RMVCT,
       CTA_PART TYPE ZFIT0078-CTA_PART,
       SHKZG       TYPE ZFIT0078-SHKZG,
       LCTO_COMP   TYPE ZFIT0078-LCTO_COMP,
       BUKRS            TYPE ZFIT0078-BUKRS,
       ID_TIPO_INVOICE  TYPE ZFIT0078-ID_TIPO_INVOICE,
       DESC_INV    TYPE ZFIT0047-DESCRICAO,
       LIFNR       TYPE ZFIT0078-LIFNR,
       COD_IMPOSTO TYPE ZFIT0078-COD_IMPOSTO,
       SEQ_FLX     TYPE ZFIT0078-SEQ_FLX,
       USNAM TYPE ZFIT0078-USNAM,
       DT_ATUAL TYPE ZFIT0078-DT_ATUAL,
       HR_ATUAL TYPE ZFIT0078-HR_ATUAL,
       END OF TY_SAIDA.


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
DATA: TG_0078      TYPE TABLE OF ZFIT0078 WITH HEADER LINE,
      TG_0077      TYPE TABLE OF ZFIT0077 WITH HEADER LINE,
      TG_0047      TYPE TABLE OF ZFIT0047 WITH HEADER LINE,
      TG_SKAT      TYPE TABLE OF SKAT     WITH HEADER LINE,
      TG_SAIDA     TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      TG_SAIDA_AUX TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      OK_CODE      TYPE SY-UCOMM,
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
      GS_VARIANT_C          TYPE DISVARIANT,
      V_REPORT              LIKE SY-REPID,
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

  PUBLIC SECTION.
*    CLASS-METHODS:
*      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
*                      IMPORTING E_ROW E_COLUMN.

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
*    CLASS-METHODS:
*       ON_BUTTON_CLICK FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
*                      IMPORTING ES_COL_ID ES_ROW_NO.
*
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
    DATA: WL_SAIDA TYPE TY_SAIDA,
          WL_0078 TYPE ZFIT0078.
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
        CLEAR WL_SAIDA.
        WL_SAIDA-USNAM      = SY-UNAME.
        WL_SAIDA-DT_ATUAL = SY-DATUM.
        WL_SAIDA-HR_ATUAL = SY-UZEIT.
        APPEND WL_SAIDA TO TG_SAIDA.
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
          READ TABLE TG_SAIDA INTO WL_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.

*          WL_SAIDA-USNAM      = SY-UNAME.
*          WL_SAIDA-DATA_ATUAL = SY-DATUM.
*          WL_SAIDA-HORA_ATUAL = SY-UZEIT.
*          MODIFY TG_SAIDA FROM WL_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX
*            TRANSPORTING ELIMINADO USNAM DATA_ATUAL HORA_ATUAL.
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
*  METHOD ON_DOUBLE_CLICK.
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

*  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD ON_DATA_CHANGED.
    DATA: LS_GOOD  TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          VL_TABIX TYPE SY-TABIX,
          VL_VALUE TYPE LVC_VALUE,
          WL_SKAT  LIKE LINE OF TG_SKAT,
          WL_0077  LIKE LINE OF TG_0077,
          WL_SAIDA LIKE LINE OF TG_SAIDA.

    CLEAR: WL_SKAT, WL_0077, WL_SAIDA, LV_VALUE.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                             INTO LS_GOOD
                             WHERE FIELDNAME = 'SAKNR'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
*
      SELECT SINGLE *
        FROM SKAT
        INTO WL_SKAT
          WHERE KTOPL EQ '0050'
            AND SAKNR EQ LV_VALUE
            AND SPRAS EQ SY-LANGU.

*        IF SY-SUBRC IS INITIAL.
      MOVE: WL_SKAT-TXT50 TO LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'TXT50'
          I_VALUE     = LV_VALUE.

      CLEAR: WL_SKAT, LV_VALUE.
    ENDLOOP.

    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
                            INTO LS_GOOD
                            WHERE FIELDNAME = 'COD_FLX'.
      LV_VALUE = LS_GOOD-VALUE.
      CONDENSE LV_VALUE NO-GAPS.
*
      SELECT SINGLE *
        FROM ZFIT0077
        INTO WL_0077
          WHERE COD_FLX EQ LV_VALUE.

*        IF SY-SUBRC IS INITIAL.
      MOVE: WL_0077-DESC_FLX TO LV_VALUE.

      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
        EXPORTING
          I_ROW_ID    = LS_GOOD-ROW_ID
          I_FIELDNAME = 'DESC_FLX'
          I_VALUE     = LV_VALUE.

      CLEAR: WL_0077, LV_VALUE.
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
          TL_SKAT  LIKE TABLE OF TG_SKAT,
          WL_SKAT  LIKE LINE OF TG_SKAT,
          TL_0077  LIKE TABLE OF TG_0077,
          WL_0077  LIKE LINE OF TG_0077.
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
    REFRESH: TL_SKAT, TL_0077, TL_SAIDA.

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

      SELECT *
        FROM SKAT
        INTO TABLE TL_SKAT
         FOR ALL ENTRIES IN TL_SAIDA
         WHERE KTOPL EQ '0050'
           AND SAKNR EQ TL_SAIDA-SAKNR
           AND SPRAS EQ SY-LANGU.

      SELECT *
        FROM ZFIT0077
        INTO TABLE TL_0077
         FOR ALL ENTRIES IN TL_SAIDA
         WHERE COD_FLX EQ TL_SAIDA-COD_FLX.

    ENDIF.
    SORT: TL_SKAT BY SAKNR,
          TL_0077 BY COD_FLX.
*          tl_t001w by werks.

    LOOP AT ET_GOOD_CELLS INTO LS_GOOD
      WHERE TABIX GT 0.

      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.

      READ TABLE TL_SKAT INTO WL_SKAT
        WITH KEY SAKNR = WL_SAIDA-SAKNR
                 BINARY SEARCH.

      READ TABLE TL_0077 INTO WL_0077
        WITH KEY COD_FLX = WL_SAIDA-COD_FLX
                 BINARY SEARCH.

*      read table tl_t001w into wl_t001w
*        with key werks = wl_saida-werks
*                 binary search.

      MOVE: WL_SKAT-TXT50 TO WL_SAIDA-TXT50,
            WL_0077-DESC_FLX TO WL_SAIDA-DESC_FLX.
*            WL_SAIDA-MEINS TO WL_SAIDA-meins,
*            wl_t001w-name1 to wl_saida-name1.

      WL_SAIDA-USNAM      = SY-UNAME.
      WL_SAIDA-DT_ATUAL = SY-DATUM.
      WL_SAIDA-HR_ATUAL = SY-UZEIT.

      MODIFY TG_SAIDA FROM WL_SAIDA INDEX LS_GOOD-ROW_ID.
      CLEAR: WL_SAIDA, WL_0077, WL_SKAT.
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
*  METHOD ON_BUTTON_CLICK.
*    DATA: WL_SAIDA LIKE LINE OF TG_SAIDA.
**    es_col_id
*    IF ES_ROW_NO-ROW_ID GT 0.
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
*      WL_SAIDA-USNAM      = SY-UNAME.
*      WL_SAIDA-DATA_ATUAL = SY-DATUM.
*      WL_SAIDA-HORA_ATUAL = SY-UZEIT.
*      MODIFY TG_SAIDA FROM WL_SAIDA INDEX ES_ROW_NO-ROW_ID.
*    ENDIF.
*    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.
*  ENDMETHOD.                    "on_button_click

  METHOD ON_ONF4.

    TYPES: BEGIN OF TY_TIPO,
             ID_TIPO_INVOICE   TYPE ZFIT0047-ID_TIPO_INVOICE,
             DESCRICAO         TYPE ZFIT0047-DESCRICAO,
           END OF TY_TIPO.

    DATA: TL_TIPO        TYPE TABLE OF TY_TIPO,
          WL_TIPO        TYPE TY_TIPO.

    DATA: LT_MAP    TYPE TABLE OF DSELC,
          LS_MAP    TYPE DSELC,
          LT_RETURN TYPE TABLE OF DDSHRETVAL,
          LS_RETURN TYPE DDSHRETVAL,
          LS_STABLE TYPE LVC_S_STBL.

    FIELD-SYMBOLS <L_OUT> TYPE TY_SAIDA.

    IF E_FIELDNAME EQ 'ID_TIPO_INVOICE'.

      READ TABLE TG_SAIDA ASSIGNING <L_OUT> INDEX ES_ROW_NO-ROW_ID.

      SELECT ID_TIPO_INVOICE DESCRICAO
        FROM ZFIT0047 INTO TABLE TL_TIPO.

      SORT TL_TIPO BY DESCRICAO.

      "SET RETURN FIELD
      CLEAR LS_MAP.
      LS_MAP-FLDNAME = 'F0001'.
      LS_MAP-DYFLDNAME = 'ID_TIPO_INVOICE'.
      APPEND LS_MAP TO LT_MAP.

      CLEAR LS_MAP.
      LS_MAP-FLDNAME = 'F0002'.
      LS_MAP-DYFLDNAME = 'DESCRICAO'.
      APPEND LS_MAP TO LT_MAP.


      "CALL SEARCH HELP POPUP FUNCTION
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'ID_TIPO_INVOICE'
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = TL_TIPO
          DYNPFLD_MAPPING = LT_MAP
          RETURN_TAB      = LT_RETURN
        EXCEPTIONS
          PARAMETER_ERROR = 1
          NO_VALUES_FOUND = 2
          OTHERS          = 3.

      READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0001'.
      IF LS_RETURN IS NOT INITIAL.
        <L_OUT>-ID_TIPO_INVOICE = LS_RETURN-FIELDVAL.
        READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0002'.
        <L_OUT>-DESC_INV = LS_RETURN-FIELDVAL.
      ENDIF.

    ENDIF.


*      SELECT *
*        FROM ZSDT0038
*        INTO TABLE TL_0038.
*
*      LOOP AT TL_0038 INTO WL_0038.
*
*        MOVE: WL_0038-CULTURA TO WL_CULTURA-FIELD.
*        APPEND WL_CULTURA TO TL_CULTURA.
*
*        MOVE: WL_0038-DESCRICAO TO WL_CULTURA-FIELD.
*        APPEND WL_CULTURA TO TL_CULTURA.
*      ENDLOOP.
*
*      WL_FIELD-TABNAME = 'ZSDT0038'.
*      WL_FIELD-FIELDNAME = 'CULTURA'.
*      WL_FIELD-S = 'X'.
*      APPEND WL_FIELD TO TL_FIELD.
*
*      WL_FIELD-TABNAME = 'ZSDT0038'.
*      WL_FIELD-FIELDNAME = 'DESCRICAO'.
*      WL_FIELD-S = ' '.
*      APPEND WL_FIELD TO TL_FIELD.
*
*      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
*        EXPORTING
**        cucol                     = '3'
*          FIELDNAME                 = 'CULTURA'
*          TABNAME                   = 'ZSDT0038'
*        IMPORTING
*          INDEX                     = WL_INDEX
*          SELECT_VALUE              = WL_CHAR
*        TABLES
*          FIELDS                    = TL_FIELD
*          SELECT_VALUES             = TL_VALUE
*          VALUETAB                  = TL_CULTURA
*        EXCEPTIONS
*          FIELD_NOT_IN_DDIC         = 001
*          MORE_THEN_ONE_SELECTFIELD = 002
*          NO_SELECTFIELD            = 003.
*
*      IF SY-SUBRC IS INITIAL.
*        READ TABLE TL_0038 INTO WL_0038 INDEX WL_INDEX.
*        IF ES_ROW_NO-ROW_ID GT 0.
*          READ TABLE TG_SAIDA INTO TG_SAIDA INDEX ES_ROW_NO-ROW_ID.
*          IF SY-SUBRC IS INITIAL.
*            MOVE: WL_0038-CULTURA TO TG_SAIDA-CULTURA.
*            MODIFY TG_SAIDA FROM TG_SAIDA INDEX ES_ROW_NO-ROW_ID.
*          ENDIF.
*        ENDIF.
*      ENDIF.
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
**        MOVE: wl_0038-descricao TO wl_cultura-field.
**        APPEND wl_cultura TO tl_cultura.
*          ENDLOOP.
*
*          WL_FIELD-TABNAME = 'ZSDT0036'.
*          WL_FIELD-FIELDNAME = 'MEINS'.
*          WL_FIELD-S = 'X'.
*          APPEND WL_FIELD TO TL_FIELD.
*
**      wl_field-tabname = 'ZSDT0038'.
**      wl_field-fieldname = 'DESCRICAO'.
**      wl_field-s = ' '.
**      APPEND wl_field TO tl_field.
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
***** Método de atualização de dados na Tela
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
        LT_F4        TYPE LVC_T_F4 WITH HEADER LINE,
        TL_FILTER    TYPE LVC_T_FILT,
        WL_FILTER    TYPE LVC_S_FILT.

  WL_REPID = SY-REPID.

  IF CONTAINER1 IS INITIAL.
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
    "WL_FUNCTION = CL_GUI_ALV_GRID=>MC_MB_FILTER.
    "APPEND WL_FUNCTION TO TL_FUNCTION.

    PERFORM MONTAR_LAYOUT.
*    PERFORM BUILD_DROPDOWN.

*    WA_LAYOUT-CTAB_FNAME = 'CELLCOLORS'.
*    WA_LAYOUT-STYLEFNAME = 'STYLE2'.
*
    LT_F4-FIELDNAME = 'ID_TIPO_INVOICE'.
    LT_F4-REGISTER = 'X' .
    LT_F4-GETBEFORE = 'X' .
    APPEND LT_F4 .
*
*    LT_F4-FIELDNAME = 'MEINS'.
*    LT_F4-REGISTER = 'X' .
*    LT_F4-GETBEFORE = 'X' .
*    APPEND LT_F4 .
*
*    WL_FILTER-FIELDNAME = 'ELIMINADO'."c_dmbtr.
*    WL_FILTER-SIGN      = 'I'. "c_i.
*    WL_FILTER-OPTION    = 'NE'. "c_ne.
*    WL_FILTER-LOW       = 'X'.
*
*    APPEND WL_FILTER TO TL_FILTER.
    GS_VARIANT_C-REPORT = SY-REPID.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT           = GS_VARIANT_C
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
        I_SAVE               = 'X'
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
*              LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK          FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID1,
              LCL_EVENT_HANDLER=>ON_DATA_CHANGED          FOR GRID1,
*              LCL_EVENT_HANDLER=>ON_BUTTON_CLICK          FOR GRID1,
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
  DATA: WL_VIEW_NAME TYPE OCUS-TABLE VALUE 'ZFIT0078',
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
  REFRESH: TG_SAIDA, TG_0078, TG_SKAT, TG_0047.
  SELECT *
    FROM ZFIT0078
    INTO TABLE TG_0078.

  IF SY-SUBRC IS INITIAL.
    SELECT *
     FROM ZFIT0077
     INTO TABLE TG_0077
      FOR ALL ENTRIES IN TG_0078
       WHERE COD_FLX EQ TG_0078-COD_FLX.

    SELECT *
     FROM ZFIT0047
     INTO TABLE TG_0047
      FOR ALL ENTRIES IN TG_0078
       WHERE ID_TIPO_INVOICE EQ TG_0078-ID_TIPO_INVOICE.

    SELECT *
      FROM SKAT
       INTO TABLE TG_SKAT
       FOR ALL ENTRIES IN TG_0078
         WHERE KTOPL EQ '0050'
           AND SPRAS EQ SY-LANGU
           AND SAKNR EQ TG_0078-SAKNR.

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
        0 'ZFIT0078'            'SEQ_FLX'           'TG_SAIDA' 'SEQ_FLX'            'Seq.'        ' '   'X' ' ' ' ',
        1 'ZFIT0078'            'SAKNR'             'TG_SAIDA' 'SAKNR'              ' '           ' '   'X' ' ' ' ',
        2 'SKAT'                'TXT50'             'TG_SAIDA' 'TXT50'              ' '           '20'  ' ' ' ' ' ',
        3 'ZFIT0078'            'COD_FLX'           'TG_SAIDA' 'COD_FLX'            'Cód.Flx'     ' '   'X' ' ' ' ',
        4 'ZFIT0077'            'DESC_FLX'          'TG_SAIDA' 'DESC_FLX'           'Desc.Flx.'   '15'  ' ' ' ' ' ',
        5 'ZFIT0078'            'BSCHL'             'TG_SAIDA' 'BSCHL'              ' '           ' '   'X' ' ' ' ',
        6 'ZFIT0078'            'MATKL'             'TG_SAIDA' 'MATKL'              ' '           ' '   'X' ' ' ' ',
        7 'ZFIT0078'            'PRCTR'             'TG_SAIDA' 'PRCTR'              ' '           ' '   'X' ' ' ' ',
        8 'ZFIT0078'            'KIDNO'             'TG_SAIDA' 'KIDNO'              ' '           ' '   'X' ' ' ' ',
        9 'ZFIT0078'            'ZUONR'             'TG_SAIDA' 'ZUONR'              ' '           ' '   'X' ' ' ' ',
       10 'ZFIT0078'            'BLART'             'TG_SAIDA' 'BLART'              ' '           ' '   'X' ' ' ' ',
       11 'T856T'               'TRTYP'             'TG_SAIDA' 'RMVCT'              'Tp.Mv.'      ' '   'X' ' ' ' ',
       12 'LFA1'                'LIFNR'             'TG_SAIDA' 'LIFNR'              'Fornecedor'  '10'  'X' ' ' ' ',
       13 'ZIMP_CAD_IMPOSTO'    'COD_IMPOSTO'       'TG_SAIDA' 'COD_IMPOSTO'        'Cod.Imp.'    '10'  'X' ' ' ' ',
       14 'ZFIT0078'            'CTA_PART'          'TG_SAIDA' 'CTA_PART'           'Cta.Part.'   '07'  'X' ' ' ' ',
       15 'ZFIT0078'            'SHKZG'             'TG_SAIDA' 'SHKZG'              'C/D'         '05'  'X' ' ' ' ',
       16 'ZFIT0078'            'LCTO_COMP'         'TG_SAIDA' 'LCTO_COMP'          'Lcto.Comp.'  '11'  'X' ' ' ' ',
       17 'ZFIT0078'            'BUKRS'             'TG_SAIDA' 'BUKRS'              'Empresa'     '08'  'X' ' ' ' ',
       18 ''                    ''                  'TG_SAIDA' 'ID_TIPO_INVOICE'    'Tp.Inv.'     '08'  'X' ' ' ' ',
       18 ''                    ''                  'TG_SAIDA' 'DESC_INV'           'Desc.Inv.'   '20'  ' ' ' ' ' ',
       19 'ZFIT0078'            'USNAM'             'TG_SAIDA' 'USNAM'              'Usuário'     ' '   ' ' ' ' ' ',
       20 'ZFIT0078'            'DT_ATUAL'          'TG_SAIDA' 'DT_ATUAL'           'Data'        ' '   ' ' ' ' ' ',
       21 'ZFIT0078'            'HR_ATUAL'          'TG_SAIDA' 'HR_ATUAL'           'Hora'        ' '   ' ' ' ' ' '.

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
                            "VALUE(P_F4).

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

*  IF P_FIELD EQ 'STATUS'.
**    w_fieldcatalog-checkbox = c_x.
**    w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
**    w_fieldcatalog-edit          = c_x.
*    IF WG_DISPLAY IS INITIAL.
*      W_FIELDCATALOG-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
*    ELSE.
**      w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_disabled.
*    ENDIF.
*  ENDIF.

*  IF P_FIELD EQ 'MEINS'.
**    w_fieldcatalog-checktable = 'T006'.
*    W_FIELDCATALOG-F4AVAILABL = C_X.
*    W_FIELDCATALOG-CONVEXIT = 'CUNIT'.
*  ENDIF.
*
*  IF P_FIELD EQ 'INCO1'.
*    W_FIELDCATALOG-DRDN_HNDL  = 1.
*  ENDIF.

  IF P_FIELD EQ 'COD_FLX' OR P_FIELD EQ 'BSCHL' OR P_FIELD EQ 'ID_TIPO_INVOICE'.
    W_FIELDCATALOG-F4AVAILABL  = 'X'.
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
        PERFORM SELECIONA_DADOS.
        PERFORM ORGANIZA_DADOS.
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
  SORT: TG_0077 BY COD_FLX,
        TG_SKAT BY SAKNR,
        TG_0047 BY ID_TIPO_INVOICE.

  LOOP AT TG_0078.
    CLEAR: TG_0047.

    READ TABLE TG_0077
      WITH KEY COD_FLX = TG_0078-COD_FLX
               BINARY SEARCH.

    READ TABLE TG_SKAT
      WITH KEY SAKNR = TG_0078-SAKNR
               BINARY SEARCH.

    READ TABLE TG_0047
      WITH KEY ID_TIPO_INVOICE = TG_0078-ID_TIPO_INVOICE
               BINARY SEARCH.


*    IF SY-SUBRC IS INITIAL.

*    read table tg_t001w
*      with key werks = tg_0036-werks
*               binary search.

    MOVE: TG_0078-SAKNR             TO  TG_SAIDA-SAKNR,
          TG_SKAT-TXT50             TO  TG_SAIDA-TXT50,
          TG_0078-COD_FLX           TO  TG_SAIDA-COD_FLX,
          TG_0077-DESC_FLX          TO  TG_SAIDA-DESC_FLX,
          TG_0078-BSCHL             TO  TG_SAIDA-BSCHL,
          TG_0078-MATKL             TO  TG_SAIDA-MATKL,
          TG_0078-PRCTR             TO  TG_SAIDA-PRCTR,
          TG_0078-KIDNO             TO  TG_SAIDA-KIDNO,
          TG_0078-ZUONR             TO  TG_SAIDA-ZUONR,
          TG_0078-BLART             TO  TG_SAIDA-BLART,
          TG_0078-RMVCT             TO  TG_SAIDA-RMVCT,
          TG_0078-LIFNR             TO  TG_SAIDA-LIFNR,
          TG_0078-COD_IMPOSTO       TO  TG_SAIDA-COD_IMPOSTO,
          TG_0078-CTA_PART          TO  TG_SAIDA-CTA_PART,
          TG_0078-SHKZG             TO  TG_SAIDA-SHKZG,
          TG_0078-LCTO_COMP         TO  TG_SAIDA-LCTO_COMP,
          TG_0078-BUKRS             TO  TG_SAIDA-BUKRS,
          TG_0078-ID_TIPO_INVOICE   TO  TG_SAIDA-ID_TIPO_INVOICE,
          TG_0047-DESCRICAO         TO  TG_SAIDA-DESC_INV,
          TG_0078-SEQ_FLX           TO  TG_SAIDA-SEQ_FLX,
          TG_0078-USNAM             TO  TG_SAIDA-USNAM,
          TG_0078-DT_ATUAL          TO  TG_SAIDA-DT_ATUAL,
          TG_0078-HR_ATUAL          TO  TG_SAIDA-HR_ATUAL.


*      IF TG_0036-LOEKZ EQ C_X.
*        MOVE: ICON_LOCKED     TO  TG_SAIDA-STATUS.
*      ELSE.
*        MOVE: ICON_UNLOCKED    TO  TG_SAIDA-STATUS.
*      ENDIF.

    APPEND TG_SAIDA.
*    ENDIF.
    CLEAR: TG_SAIDA, TG_0078, TG_SKAT, TG_0077.

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

  SORT TG_SAIDA BY SEQ_FLX.

  IF TG_SAIDA[] IS NOT INITIAL.
    TG_SAIDA_AUX[] = TG_SAIDA[].
  ENDIF.
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
  DATA: TL_INPUT_0078 TYPE TABLE OF ZFIT0078 WITH HEADER LINE,
        TL_0078 TYPE TABLE OF ZFIT0078 WITH HEADER LINE.

  CLEAR: TL_INPUT_0078.
  REFRESH: TL_INPUT_0078.

*  SELECT *
*    FROM ZSDT0036
*    INTO TABLE TL_0036
*     WHERE SAFRA IN S_SAFRA
*       AND CULTURA IN S_CULTU.

*  DELETE FROM ZSDT0036 WHERE SAFRA   IN S_SAFRA
*                         AND CULTURA IN S_CULTU.

*  LOOP AT TL_0036.
*    READ TABLE TL_MARA
*      WITH KEY MATNR = TL_0036-MATNR.
*    IF SY-SUBRC IS INITIAL.
*     delete zsdt0036 from tl_0036.
*    ENDIF.
*  ENDLOOP.

  DELETE TG_SAIDA WHERE SAKNR       IS INITIAL
                    AND COD_FLX     IS INITIAL
                    AND MATKL       IS INITIAL
                    AND PRCTR       IS INITIAL
                    AND KIDNO       IS INITIAL
                    AND ZUONR       IS INITIAL
                    AND BLART       IS INITIAL.


  LOOP AT TG_SAIDA.
    MOVE-CORRESPONDING: TG_SAIDA TO TL_INPUT_0078.

    APPEND TL_INPUT_0078.
    CLEAR: TL_INPUT_0078.

  ENDLOOP.

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
  DELETE FROM ZFIT0078.
  MODIFY ZFIT0078 FROM TABLE TL_INPUT_0078.
  IF SY-SUBRC IS INITIAL.
*    LOOP AT TG_SAIDA_AUX.
*      DELETE FROM ZSDT0036 WHERE VAL_DE       = TG_SAIDA_AUX-VAL_DE
*                             AND VAL_ATE      = TG_SAIDA_AUX-VAL_ATE
*                             AND DTVENC       = TG_SAIDA_AUX-DTVENC
*                             AND MATNR        = TG_SAIDA_AUX-MATNR
*                             AND WAERK        = TG_SAIDA_AUX-WAERK
*                             AND INCO1        = TG_SAIDA_AUX-INCO1
*                             AND SAFRA        = TG_SAIDA_AUX-SAFRA
*                             AND CULTURA      = TG_SAIDA_AUX-CULTURA
*                             AND WERKS_FORNEC = TG_SAIDA_AUX-WERKS_FORNEC.
*
*    ENDLOOP.
    COMMIT WORK AND WAIT.
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
  DATA:     TL_SKAT  LIKE TABLE OF TG_SKAT,
            WL_SKAT  LIKE LINE OF TG_SKAT,
            TL_0077  LIKE TABLE OF TG_0077,
            WL_0077  LIKE LINE OF TG_0077,
*            tl_t001W type table of t001W,
*            Wl_t001W LIKE LINE OF t001W,
            TL_CELL  TYPE LVC_T_CELL,
            WL_LINHA(6).

  REFRESH: TL_SKAT, TL_0077, TG_MSG_RET.
  CLEAR: WL_SKAT, WL_0077, TG_MSG_RET.

  IF TG_SAIDA[] IS NOT INITIAL.
*    select werks name1
*      from t001w
*      into table tl_t001w
*       for all entries in tg_saida
*        where werks eq tg_saida-werks.

    SELECT *
      FROM SKAT
      INTO TABLE TL_SKAT
       FOR ALL ENTRIES IN TG_SAIDA
       WHERE SAKNR EQ TG_SAIDA-SAKNR.

    SELECT *
      FROM ZFIT0077
      INTO TABLE TL_0077
       FOR ALL ENTRIES IN TG_SAIDA
       WHERE COD_FLX EQ TG_SAIDA-COD_FLX.


  ENDIF.

  SORT: TL_SKAT   BY SAKNR,
        TL_0077   BY COD_FLX.

  LOOP AT TG_SAIDA.
*    PERFORM get_cell TABLES Tl_cell
*                     USING 'VAL_DE'
*                           WL_LINHA.
    WL_LINHA = SY-TABIX.
    IF TG_SAIDA-SAKNR IS INITIAL.
*      move: "TEXT-E01            TO TG_MSG_RET-MSG,
*            c_tab_strip_nf-tab6 to tg_msg_ret-aba.
      MOVE: 'SAKNR' TO TG_MSG_RET-FIELD,
            'GRID1'  TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE TEXT-E01 ' Cta.Razão.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE TL_SKAT TRANSPORTING NO FIELDS
              WITH KEY SAKNR = TG_SAIDA-SAKNR
                       BINARY SEARCH.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: 'SAKNR' TO TG_MSG_RET-FIELD,
              'GRID1'  TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE  'Cta.Razão.' TEXT-E04 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF TG_SAIDA-COD_FLX IS INITIAL.
*      move: "TEXT-E01            TO TG_MSG_RET-MSG,
*            c_tab_strip_nf-tab6 to tg_msg_ret-aba.
      MOVE: 'COD_FLX' TO TG_MSG_RET-FIELD,
            'GRID1'  TO TG_MSG_RET-OBJ,
            WL_LINHA TO TG_MSG_RET-TABIX.

      CONCATENATE TEXT-E01 ' Cta.Razão.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE TL_0077 TRANSPORTING NO FIELDS
              WITH KEY COD_FLX = TG_SAIDA-COD_FLX
                       BINARY SEARCH.
      IF SY-SUBRC IS NOT INITIAL.
        MOVE: 'COD_FLX' TO TG_MSG_RET-FIELD,
              'GRID1'   TO TG_MSG_RET-OBJ,
              WL_LINHA TO TG_MSG_RET-TABIX.

        CONCATENATE  'Cta.Razão.' TEXT-E04 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " VERIFICA_ERROS
