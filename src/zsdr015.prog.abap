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

REPORT  zsdr015.
TYPE-POOLS: rmdi.
INCLUDE: <icon>.
INCLUDE <cl_alv_control>.
TYPES: BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END OF ty_t001w,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         meins TYPE mara-meins,
       END OF ty_mara,

       BEGIN OF ty_saida,
         mark,
         status(4),       "type zsdt0036-loekz,
         bukrs        TYPE zsdt0036-bukrs,
         val_de       TYPE zsdt0036-val_de,
         val_ate      TYPE zsdt0036-val_ate,
         dtvenc       TYPE zsdt0036-dtvenc,
*         werks       type zsdt0036-werks,
*         name1       type t001w-name1,
         matnr        TYPE zsdt0036-matnr,
         cultura      TYPE zsdt0036-cultura,
         safra        TYPE zsdt0036-safra,
         maktx        TYPE makt-maktx,
         meins        TYPE zsdt0036-meins,
         werks_fornec TYPE zsdt0036-werks_fornec,
         matkl        TYPE mara-matkl,
         waerk        TYPE zsdt0036-waerk,
         vlr_custo    TYPE zsdt0036-vlr_custo,
         inco1        TYPE zsdt0036-inco1,
         perc_margem  TYPE zsdt0036-perc_margem,
         vlr_margem   TYPE zsdt0036-vlr_margem,
         vlr_venda    TYPE zsdt0036-vlr_venda,
         cellcolors   TYPE lvc_t_scol,
         style2       TYPE lvc_t_styl,
         eliminado    TYPE zsdt0036-eliminado,
         usnam        TYPE zsdt0036-usnam,
         data_atual   TYPE zsdt0036-data_atual,
         hora_atual   TYPE zsdt0036-hora_atual,
       END OF ty_saida.

DATA: BEGIN OF gt_values OCCURS 0,
        domvalue_l TYPE domvalue_l,
        ddtext     TYPE val_text,
      END OF gt_values.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x              TYPE c VALUE 'X',
           c_add(3)         TYPE c VALUE 'ADD',
           c_del(3)         TYPE c VALUE 'DEL',
           c_imp(3)         TYPE c VALUE 'IMP',
           c_exit(4)        TYPE c VALUE 'EXIT',
           c_back(4)        TYPE c VALUE 'BACK',
           c_save(4)        TYPE c VALUE 'SAVE',
*           c_red(4)      type c value '@0A@',
*           c_yellow(4)   type c value '@09@',
*           c_green(4)    type c value '@08@',
*           c_aguard(4)   type c value '@9R@',
           c_proces(6)      TYPE c VALUE 'PROCES',
           c_cancel(6)      TYPE c VALUE 'CANCEL',
           c_atuali(6)      TYPE c VALUE 'ATUALI',
           c_search(6)      TYPE c VALUE 'SEARCH',
           c_show_msgre(10) TYPE c VALUE 'SHOW_MSGRE',

*FF #168911 - inicio
           c_p_db_tab(8)    TYPE c VALUE 'ZSDT0269', "Tabela de Gravação dos Registros
           c_p_stcnam(12)   TYPE c VALUE 'ZSDT0269_OUT',  "Strutura Saída Tela
           c_p_scmant(04)   TYPE c VALUE '0293', "Tela Manter Registro
           c_p_title        TYPE string VALUE 'Parametros Margem Produto x Grupo Mercadoria'. " Titulo Relatório
*FF #168911 -  fim

*&--------------------------------------------------------------------&*
*& Declaração de Variaveis/Tabelas/Workarea                           &*
*&--------------------------------------------------------------------&*
DATA: tg_0036         TYPE TABLE OF zsdt0036 WITH HEADER LINE,
      tg_t001w        TYPE TABLE OF ty_t001w WITH HEADER LINE,
      tg_mara         TYPE TABLE OF ty_mara WITH HEADER LINE,
      tg_makt         TYPE TABLE OF ty_makt WITH HEADER LINE,
      tg_saida        TYPE TABLE OF ty_saida WITH HEADER LINE,
      t_saida         TYPE TABLE OF ty_saida,
      w_saida         TYPE ty_saida,
      tg_saida_aux    TYPE TABLE OF ty_saida WITH HEADER LINE,
      ok_code         TYPE sy-ucomm,
      init,
      wg_display,
      x_field(30),
      wg_mensagem(30),
      wg_obj(40).

* Início - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura
DATA: tg_0038    TYPE TABLE OF zsdt0038.
* Fim - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura

DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.
*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.

DATA: gv_bloqueado TYPE abap_bool.

DATA: lt_row_index TYPE lvc_t_row,
      ls_row_index TYPE lvc_s_row.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.

DATA: grid1                TYPE REF TO cl_gui_alv_grid,
      container1           TYPE REF TO cl_gui_docking_container,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

TABLES sscrfields.                                          "FF #168911

DATA: t_fieldcatalog  TYPE lvc_t_fcat,
      w_fieldcatalog  TYPE lvc_s_fcat,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
*      TG_FIELDCATALOG_LCTOS TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      wa_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,
      wg_cell         TYPE lvc_s_cell,
      tg_cell         TYPE lvc_t_cell,
      wa_style        TYPE lvc_s_styl,
      style2          TYPE lvc_t_styl WITH HEADER LINE.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs    FOR tg_0036-bukrs NO-EXTENSION NO INTERVALS,
* Início - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura
*                S_CULTU    FOR TG_0036-CULTURA MATCHCODE OBJECT ZSDAJ002,
                  s_cultu    FOR tg_0036-cultura MATCHCODE OBJECT zsdaj009,     "Cultura Pagamento
* Fim - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura
                  s_safra    FOR tg_0036-safra MATCHCODE OBJECT zsdaj003,
                  s_matkl    FOR tg_saida-matkl.
  "BS #169488 - inicio
  SELECTION-SCREEN SKIP 1.
  PARAMETERS: p_visual RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND rmod,
              p_editar RADIOBUTTON GROUP g1.
  "BS #169488 - fim
SELECTION-SCREEN: END OF BLOCK b1.
"FF #168911 - inicio

SELECTION-SCREEN FUNCTION KEY 1.  " Adiciona o botão de função F1

AT SELECTION-SCREEN OUTPUT.
  "BS #169488 - inicio
  IF p_visual = 'X'.
    wg_display = 'X'. " Modo Visualizar
  ELSE.
    CLEAR wg_display. " Modo Editar
  ENDIF.
  "BS #169488 - fim

INITIALIZATION.
  sscrfields-functxt_01 = 'Parâmetro Margem Grupo Mercadoria'.  " Define o texto do botão

* Validação para impedir o uso de '*' no campo empresa
AT SELECTION-SCREEN ON s_bukrs.
  FIND '*' IN s_bukrs-low.
  IF sy-subrc = 0.
    MESSAGE 'O uso de * não é permitido neste campo de seleção!' TYPE 'E'.
    CLEAR s_bukrs-low.
  ENDIF.

AT SELECTION-SCREEN.

  IF s_bukrs-low IS INITIAL AND sy-ucomm <> 'FC01'.
    MESSAGE 'Preencher campo obrigatório "Empresa"' TYPE 'E'.
    CLEAR s_bukrs-low.
  ENDIF.

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      SUBMIT zregister_data WITH p_db_tab = c_p_db_tab
                            WITH p_stcnam = c_p_stcnam
                            WITH p_scmant = c_p_scmant
                            WITH p_title  = c_p_title
                        AND RETURN.
    WHEN OTHERS.

  ENDCASE.

  "FF #168911 - fim

  DEFINE m_message.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      WHEN OTHERS.
    ENDCASE.
  END-OF-DEFINITION.

CLASS lcl_excel DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_alsmex_tabline,
             row   TYPE  kcd_ex_row_n,
             col   TYPE kcd_ex_col_n,
             value TYPE char8000,
           END OF ty_alsmex_tabline.

    TYPES: BEGIN OF ty_s_senderline,
             line(4096) TYPE c,
           END OF ty_s_senderline,
           ty_t_sender TYPE ty_s_senderline.

    DATA: it_excel TYPE TABLE OF ty_alsmex_tabline.
    DATA: excel_tab TYPE TABLE OF ty_t_sender,
          cont_col  TYPE kcd_ex_col_n,
          it_aux    TYPE TABLE OF ty_alsmex_tabline WITH DEFAULT KEY.

    METHODS:
      get_excel
        RETURNING VALUE(r_value) TYPE rlgrap-filename,
      set_excel
        IMPORTING input TYPE char128,
      exc_excel,
      col RETURNING VALUE(r_value) TYPE kcd_ex_col_n,
      convert_num
        IMPORTING input         TYPE char8000
        RETURNING VALUE(return) TYPE char15,
      convert_dt
        IMPORTING input         TYPE char8000
        RETURNING VALUE(return) TYPE datum,
      convert_mt
        IMPORTING input         TYPE char8000
        RETURNING VALUE(return) TYPE char18,
      atualiza_dados CHANGING VALUE(input) TYPE ty_saida.

    DATA: it_dados TYPE TABLE OF char8000.

    DATA: ld_separator TYPE c,
          application  TYPE ole2_object,
          workbook     TYPE ole2_object,
          range        TYPE ole2_object,
          worksheet    TYPE ole2_object,
          h_cell       TYPE ole2_object,
          h_cell1      TYPE ole2_object,
          ld_rc        TYPE i.

ENDCLASS.

DATA obj_excel TYPE REF TO lcl_excel.


CLASS lcl_excel IMPLEMENTATION.


  METHOD get_excel.

    DATA: it_file TYPE filetable,
          l_subrc TYPE i.

    cl_gui_frontend_services=>file_open_dialog(  EXPORTING default_filename = ' '
                                                           file_filter      = '*.xls'
                                                 CHANGING  file_table       = it_file
                                                           rc               = l_subrc
                                               ).
    TRY .
        r_value = it_file[ 1 ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR r_value.
    ENDTRY.

  ENDMETHOD.

  METHOD set_excel.

    CHECK input IS NOT INITIAL.

    FREE: it_excel[], excel_tab[].

    CLASS cl_abap_char_utilities DEFINITION LOAD.

    ld_separator = cl_abap_char_utilities=>horizontal_tab.

    IF application-header = space OR application-handle = -1.
      CREATE OBJECT application 'Excel.Application'.
      m_message.
    ENDIF.

    CALL METHOD OF application 'Workbooks' = workbook.
    m_message.
    CALL METHOD OF workbook 'Open' EXPORTING #1 = input.
    m_message.
    GET PROPERTY OF  application 'ACTIVESHEET' = worksheet.
    m_message.
    CALL METHOD OF worksheet 'Cells' = h_cell
        EXPORTING #1 = 1 #2 = 1.
    m_message.
    CALL METHOD OF worksheet 'Cells' = h_cell1
        EXPORTING #1 = 10000 #2 = 37.
    m_message.

    CALL METHOD  OF worksheet 'RANGE' = range
                   EXPORTING #1 = h_cell #2 = h_cell1.
    m_message.
    CALL METHOD OF range 'SELECT'.
    m_message.
    CALL METHOD OF range 'COPY'.
    m_message.

    cl_gui_frontend_services=>clipboard_import( IMPORTING data = excel_tab ).
    IF sy-subrc <> 0.
      MESSAGE a037(alsmex).
    ENDIF.

    LOOP AT excel_tab INTO DATA(wa).

      DATA(cont) = sy-tabix.
      SPLIT wa AT ld_separator INTO TABLE it_dados.
      it_aux = VALUE #( FOR ls IN it_dados (
                         row   = cont
                         col   = col( )
                         value = ls
                        ) ).
      CLEAR cont_col.
      APPEND LINES OF it_aux TO it_excel.
    ENDLOOP.

    FREE excel_tab.
    cl_gui_frontend_services=>clipboard_export(  IMPORTING data = excel_tab
                                                 CHANGING  rc   = ld_rc
                                               ).

    CALL METHOD OF application 'QUIT'.
    m_message.

    FREE OBJECT h_cell.       m_message.
    FREE OBJECT h_cell1.      m_message.
    FREE OBJECT range.        m_message.
    FREE OBJECT worksheet.    m_message.
    FREE OBJECT workbook.     m_message.
    FREE OBJECT application.  m_message.

  ENDMETHOD.

  METHOD exc_excel.

    FREE: t_saida.

    me->set_excel( me->get_excel( ) ).

    LOOP AT it_excel INTO DATA(wa_excel).

      CHECK wa_excel-row NE 1.

      CASE wa_excel-col.
        WHEN 1.  w_saida-bukrs         = s_bukrs-low.
        WHEN 2.  w_saida-val_de        = convert_dt( wa_excel-value ).
        WHEN 3.  w_saida-val_ate       = convert_dt( wa_excel-value ).
        WHEN 4.  w_saida-dtvenc        = convert_dt( wa_excel-value ).
        WHEN 5.  w_saida-matnr         = convert_mt( wa_excel-value ).
        WHEN 7.  w_saida-cultura       = wa_excel-value.
        WHEN 8.  w_saida-safra         = wa_excel-value.
        WHEN 9.  w_saida-meins         = COND #( WHEN wa_excel-value EQ 'SAC' THEN 'BAG' ELSE wa_excel-value ).
        WHEN 10. w_saida-werks_fornec  = wa_excel-value.
        WHEN 11. w_saida-inco1         = wa_excel-value.
*        WHEN 12. W_SAIDA-MATKL         = WA_EXCEL-VALUE.
        WHEN 13. w_saida-waerk         = wa_excel-value.
        WHEN 14. w_saida-vlr_custo     = convert_num( wa_excel-value ).
        WHEN 15.
          w_saida-perc_margem   = convert_num( wa_excel-value ).

          w_saida-usnam        = sy-uname.
          w_saida-data_atual   = sy-datum.
          w_saida-hora_atual   = sy-uzeit.

          atualiza_dados( CHANGING input = w_saida ).

          IF NOT w_saida-matnr IS INITIAL.
            APPEND w_saida TO t_saida.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    APPEND LINES OF t_saida TO tg_saida.

    FREE it_excel.

  ENDMETHOD.

  METHOD col.
    ADD 1 TO cont_col.
    r_value = cont_col.
  ENDMETHOD.

  METHOD convert_num.
    DATA(conv) = input.
    REPLACE ALL OCCURRENCES OF '/' IN conv WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN conv WITH ''.
    REPLACE ALL OCCURRENCES OF '.' IN conv WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN conv WITH '.'.
    CONDENSE conv NO-GAPS.
    return = conv.
  ENDMETHOD.

  METHOD convert_dt.

    DATA(conv) = input.
    REPLACE ALL OCCURRENCES OF '/' IN conv WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN conv WITH ''.
    REPLACE ALL OCCURRENCES OF '.' IN conv WITH ''.
    CONDENSE conv NO-GAPS.

    CALL FUNCTION 'CONVERT_DATE_INPUT'
      EXPORTING
        input                     = conv
      IMPORTING
        output                    = return
      EXCEPTIONS
        plausibility_check_failed = 1
        wrong_format_in_input     = 2.

  ENDMETHOD.

  METHOD convert_mt.
    DATA _matnr TYPE matnr.
    _matnr = input.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = _matnr
      IMPORTING
        output       = return
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


*    return = |{ _matnr ALPHA = IN }|.
  ENDMETHOD.

  METHOD atualiza_dados.
    DATA: _makt TYPE makt.

    CLEAR: input-maktx, input-matkl.

    SELECT SINGLE maktx
      FROM makt
      INTO ( input-maktx )
       WHERE matnr EQ input-matnr
         AND spras EQ sy-langu.

    SELECT SINGLE matkl
      FROM mara
      INTO ( input-matkl )
       WHERE matnr EQ input-matnr.


***  Realiza o calculo do campo "Calculo"
***  wl_calculo = lv_value * ( wl_perc_margem / 100 ).
    TRY.
        input-vlr_margem = ( input-vlr_custo / ( 1 - ( input-perc_margem / 100 ) ) - input-vlr_custo ).
      CATCH cx_sy_zerodivide.
    ENDTRY.

***  Realiza o calculo do campo "Valor da Venda"

    input-vlr_venda = input-vlr_margem + input-vlr_custo.

  ENDMETHOD.


ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

*    CLASS-METHODS:
*      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*                                   IMPORTING  E_ROW_ID E_COLUMN_ID.
*
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
    CLASS-METHODS:
      on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.
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
    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    IF wg_display IS INITIAL.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
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
    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    IF wg_display IS INITIAL.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


    ty_toolbar-icon      =  icon_import.
    ty_toolbar-function  =  c_imp.
    ty_toolbar-quickinfo = TEXT-i01.
    ty_toolbar-text  =  TEXT-i02.
    IF wg_display IS INITIAL.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
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
  METHOD handle_user_command.
    DATA: wl_saida TYPE ty_saida,
          wl_0036  TYPE zsdt0036.

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

    CASE e_ucomm.
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
      WHEN c_add.

*        clear: wl_saida.
*        move: s_cultu-low to wl_SAIDA-cultura.
*        move: s_safra-low to wl_SAIDA-safra.
*        APPEND wl_saida to TG_SAIDA.
        "BS #169488 - inicio
        IF p_visual IS INITIAL.

          CLEAR wl_saida.
          wl_saida-bukrs      = s_bukrs-low.
          wl_saida-usnam      = sy-uname.
          wl_saida-data_atual = sy-datum.
          wl_saida-hora_atual = sy-uzeit.
          APPEND wl_saida TO tg_saida.
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

          DATA: lv_last_row  TYPE lvc_index,
                ls_row_id    TYPE lvc_s_row,
                ls_column_id TYPE lvc_s_col.

          lv_last_row = lines( tg_saida ).

          ls_row_id-index     = lv_last_row.
          ls_column_id-fieldname = 'BUKRS'.

          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

          CALL METHOD grid1->set_current_cell_via_id
            EXPORTING
              is_row_id    = ls_row_id
              is_column_id = ls_column_id.

        ELSE.
          MESSAGE 'Ação não permitida para o modo Visualização'  TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        "BS #169488 - fim

      WHEN c_del.

*        CALL METHOD grid1->get_selected_cells
*          IMPORTING
*            et_cell = tg_selectedcell.

*        LOOP AT tg_selectedcell INTO wg_selectedcell.
*          READ TABLE tg_saida INTO wl_saida INDEX wg_selectedcell-row_id-index.
*
*          wl_saida-eliminado  = c_x.
*          wl_saida-usnam      = sy-uname.
*          wl_saida-data_atual = sy-datum.
*          wl_saida-hora_atual = sy-uzeit.
*          MODIFY tg_saida FROM wl_saida INDEX wg_selectedcell-row_id-index
*            TRANSPORTING eliminado usnam data_atual hora_atual.
**          DELETE TG_SAIDA INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
*
*        ENDLOOP.

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

        "BS #169488 - inicio
        IF p_visual IS INITIAL.

          CALL METHOD grid1->get_selected_rows
            IMPORTING
              et_index_rows = lt_row_index.

          CALL METHOD grid1->get_selected_cells
            IMPORTING
              et_cell = tg_selectedcell.
*
*          LOOP AT tg_selectedcell INTO wg_selectedcell.
*            READ TABLE tg_saida INTO wl_saida INDEX wg_selectedcell-row_id-index.
*            IF sy-subrc = 0.
*              wl_saida-eliminado  = c_x.
*              wl_saida-usnam      = sy-uname.
*              wl_saida-data_atual = sy-datum.
*              wl_saida-hora_atual = sy-uzeit.
*              MODIFY tg_saida FROM wl_saida INDEX wg_selectedcell-row_id-index
*                TRANSPORTING eliminado usnam data_atual hora_atual.
*            ENDIF.
*          ENDLOOP.

*          CALL METHOD grid1->get_selected_rows
*            IMPORTING
*              et_index_rows = lt_row_index.

          LOOP AT lt_row_index INTO ls_row_index.
            READ TABLE tg_saida INTO wl_saida INDEX ls_row_index-index.
            IF sy-subrc = 0.
              wl_saida-eliminado  = c_x.
              wl_saida-usnam      = sy-uname.
              wl_saida-data_atual = sy-datum.
              wl_saida-hora_atual = sy-uzeit.
              MODIFY tg_saida FROM wl_saida INDEX ls_row_index-index
                TRANSPORTING eliminado usnam data_atual hora_atual.
            ENDIF.
          ENDLOOP.

          DELETE tg_saida WHERE eliminado = c_x.

          CLEAR wg_mensagem.
          REFRESH tg_msg_ret.

          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        ELSE.
          MESSAGE 'Ação não permitida para o modo Visualização' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        "BS #169488 - fim
      WHEN c_imp.
        IF p_visual IS INITIAL.
          obj_excel->exc_excel( ).
        ELSE.
          MESSAGE 'Ação não permitida para o modo Visualização'  TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
    ENDCASE.

    PERFORM verifica_erros.
    "BS #169488 - inicio
    IF e_ucomm <> c_del.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen   = '100'
          i_show     = space
          i_repid    = sy-repid
          i_popup    = 1
*         i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*         I_SET_FIELD   = 'X_FIELD'
        IMPORTING
          e_messagem = wg_mensagem
        TABLES
          it_msgs    = tg_msg_ret.

      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.
    "BS #169488 - fim
*    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.
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
  METHOD on_data_changed.
    DATA: ls_good        TYPE lvc_s_modi,
          lv_value       TYPE lvc_value,
          vl_tabix       TYPE sy-tabix,
          vl_value       TYPE lvc_value,
          wl_mara        LIKE LINE OF tg_mara,
          wl_makt        LIKE LINE OF tg_makt,
          wl_t001w       LIKE LINE OF tg_t001w,
          wl_saida       LIKE LINE OF tg_saida,
          wl_calculo     TYPE zsdt0036-vlr_margem,
          wl_vlr_venda   TYPE zsdt0036-vlr_venda,
          wl_vlr_custo   TYPE zsdt0036-vlr_custo,
          wl_perc_margem TYPE zsdt0036-perc_margem.

    CLEAR: wl_makt, wl_mara, wl_t001w, wl_calculo, wl_saida, lv_value,
           wl_vlr_venda, wl_vlr_custo, wl_perc_margem.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'MATNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
*
      SELECT SINGLE matnr matkl meins
        FROM mara
        INTO wl_mara
          WHERE matnr EQ lv_value.
*
      IF sy-subrc IS INITIAL.
        SELECT SINGLE matnr maktx
          FROM makt
          INTO wl_makt
           WHERE matnr EQ wl_mara-matnr
             AND spras EQ sy-langu.
      ENDIF.
*
*        IF SY-SUBRC IS INITIAL.
      MOVE: wl_makt-maktx TO lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MAKTX'
          i_value     = lv_value.

*        ENDIF.
      MOVE: wl_mara-matkl TO lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MATKL'
          i_value     = lv_value.

      CLEAR: wl_makt, wl_mara, lv_value.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'VLR_CUSTO'
                                OR fieldname = 'PERC_MARGEM'
                                OR fieldname = 'VLR_MARGEM'.

*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.


      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      IF ls_good-fieldname EQ 'VLR_CUSTO'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'PERC_MARGEM'
          IMPORTING
            e_value     = wl_perc_margem.
**  Realiza o calculo do campo "Calculo"
*        wl_calculo = lv_value * ( wl_perc_margem / 100 ).
        TRY.
            wl_calculo = ( lv_value / ( 1 - ( wl_perc_margem / 100 ) ) - lv_value ).
          CATCH cx_sy_zerodivide.
        ENDTRY.
***  Realiza o calculo do campo "Valor da Venda"
        wl_vlr_venda = wl_calculo + lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MARGEM'
            i_value     = wl_calculo.

      ELSEIF ls_good-fieldname EQ 'PERC_MARGEM'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'VLR_CUSTO'
          IMPORTING
            e_value     = wl_vlr_custo.

        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'PERC_MARGEM'
          IMPORTING
            e_value     = wl_perc_margem.

**  Realiza o calculo do campo "Margem Vlr"
        TRY.
            wl_calculo =  ( wl_vlr_custo / ( 1 - ( wl_perc_margem / 100 ) ) - wl_vlr_custo ).
          CATCH cx_sy_zerodivide.
        ENDTRY.
***  Realiza o calculo do campo "Valor da Venda"
        wl_vlr_venda = wl_calculo + wl_vlr_custo.

        MOVE: wl_calculo TO lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MARGEM'
            i_value     = lv_value.

      ELSEIF ls_good-fieldname EQ 'VLR_MARGEM'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'VLR_CUSTO'
          IMPORTING
            e_value     = wl_vlr_custo.

        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'VLR_MARGEM'
          IMPORTING
            e_value     = wl_calculo.

**  Realiza o calculo do campo "Margem Perc"
        TRY.
            wl_perc_margem = ( wl_calculo * 100 ) / ( wl_calculo + wl_vlr_custo ).
          CATCH cx_sy_zerodivide.
        ENDTRY.
***  Realiza o calculo do campo "Valor da Venda"
        wl_vlr_venda = wl_calculo + wl_vlr_custo.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'PERC_MARGEM'
            i_value     = wl_perc_margem.
      ENDIF.

      MOVE: wl_vlr_venda TO lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_VENDA'
          i_value     = lv_value.

      CLEAR: wl_calculo, wl_saida, lv_value, wl_vlr_venda, wl_vlr_custo,
             wl_perc_margem.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                 INTO ls_good
                                 WHERE fieldname = 'VAL_ATE'
                                    OR fieldname = 'VAL_DE'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
*      if ls_good-fieldname eq 'VAL_ATE'.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = ls_good-fieldname "'VAL_ATE'
          i_value     = lv_value.
    ENDLOOP.
    PERFORM verifica_erros.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
        i_popup    = 1
      IMPORTING
        e_messagem = wg_mensagem
      TABLES
        it_msgs    = tg_msg_ret.

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
*    CLEAR: wl_makt, wl_mara, wl_t001w, wl_calculo, wl_saida, lv_value,
*           wl_vlr_venda, wl_vlr_custo, wl_perc_margem.
*
*    LOOP AT er_data_changed->mt_good_cells
*                             INTO ls_good
*                             WHERE fieldname = 'MATNR'.
*      lv_value = ls_good-value.
*      CONDENSE lv_value NO-GAPS.
*
*      SELECT SINGLE matnr matkl meins
*        FROM mara
*        INTO wl_mara
*          WHERE matnr EQ lv_value.
*
*      IF sy-subrc IS INITIAL.
*        SELECT SINGLE matnr maktx
*          FROM makt
*          INTO wl_makt
*           WHERE matnr EQ wl_mara-matnr
*             AND spras EQ sy-langu.
*      ENDIF.
*
*        IF SY-SUBRC IS INITIAL.
*      MOVE: wl_makt-maktx TO lv_value.
*
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'MAKTX'
*          i_value     = lv_value.
*
*        ENDIF.
*      MOVE: wl_mara-matkl TO lv_value.
*
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'MATKL'
*          i_value     = lv_value.
*
*      MOVE: WL_MARA-MEINS TO LV_VALUE.
*
*      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*        EXPORTING
*          I_ROW_ID    = LS_GOOD-ROW_ID
*          I_FIELDNAME = 'MEINS'
*          I_VALUE     = LV_VALUE.
*
*      CLEAR: wl_makt, wl_mara, lv_value.
*    ENDLOOP.
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
*
*    LOOP AT er_data_changed->mt_good_cells
*                             INTO ls_good
*                             WHERE fieldname = 'VLR_CUSTO'
*                                OR fieldname = 'PERC_MARGEM'
*                                OR fieldname = 'VLR_MARGEM'.
*
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*
*      lv_value = ls_good-value.
*      CONDENSE lv_value NO-GAPS.
*
*      IF ls_good-fieldname EQ 'VLR_CUSTO'.
*        CALL METHOD er_data_changed->get_cell_value
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_tabix     = ls_good-tabix
*            i_fieldname = 'PERC_MARGEM'
*          IMPORTING
*            e_value     = wl_perc_margem.
**  Realiza o calculo do campo "Calculo"
*        wl_calculo = lv_value * ( wl_perc_margem / 100 ).
*        TRY.
*            wl_calculo = ( lv_value / ( 1 - ( wl_perc_margem / 100 ) ) - lv_value ).
*          CATCH cx_sy_zerodivide.
*        ENDTRY.
***  Realiza o calculo do campo "Valor da Venda"
*        wl_vlr_venda = wl_calculo + lv_value.
*
*        CALL METHOD er_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'VLR_MARGEM'
*            i_value     = wl_calculo.
*
*      ELSEIF ls_good-fieldname EQ 'PERC_MARGEM'.
*        CALL METHOD er_data_changed->get_cell_value
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_tabix     = ls_good-tabix
*            i_fieldname = 'VLR_CUSTO'
*          IMPORTING
*            e_value     = wl_vlr_custo.
*
*        CALL METHOD er_data_changed->get_cell_value
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_tabix     = ls_good-tabix
*            i_fieldname = 'PERC_MARGEM'
*          IMPORTING
*            e_value     = wl_perc_margem.
*
**  Realiza o calculo do campo "Margem Vlr"
*        TRY.
*            wl_calculo =  ( wl_vlr_custo / ( 1 - ( wl_perc_margem / 100 ) ) - wl_vlr_custo ).
*          CATCH cx_sy_zerodivide.
*        ENDTRY.
***  Realiza o calculo do campo "Valor da Venda"
*        wl_vlr_venda = wl_calculo + wl_vlr_custo.
*
*        MOVE: wl_calculo TO lv_value.
*
*        CALL METHOD er_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'VLR_MARGEM'
*            i_value     = lv_value.
*
*      ELSEIF ls_good-fieldname EQ 'VLR_MARGEM'.
*        CALL METHOD er_data_changed->get_cell_value
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_tabix     = ls_good-tabix
*            i_fieldname = 'VLR_CUSTO'
*          IMPORTING
*            e_value     = wl_vlr_custo.
*
*        CALL METHOD er_data_changed->get_cell_value
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_tabix     = ls_good-tabix
*            i_fieldname = 'VLR_MARGEM'
*          IMPORTING
*            e_value     = wl_calculo.
*
**  Realiza o calculo do campo "Margem Perc"
*        TRY.
*            wl_perc_margem = ( wl_calculo * 100 ) / ( wl_calculo + wl_vlr_custo ).
*          CATCH cx_sy_zerodivide.
*        ENDTRY.
***  Realiza o calculo do campo "Valor da Venda"
*        wl_vlr_venda = wl_calculo + wl_vlr_custo.
*
*        CALL METHOD er_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'PERC_MARGEM'
*            i_value     = wl_perc_margem.
*      ENDIF.
*
*      MOVE: wl_vlr_venda TO lv_value.
*
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'VLR_VENDA'
*          i_value     = lv_value.
*
*      CLEAR: wl_calculo, wl_saida, lv_value, wl_vlr_venda, wl_vlr_custo,
*             wl_perc_margem.
*    ENDLOOP.
*
*    LOOP AT er_data_changed->mt_good_cells
*                                 INTO ls_good
*                                 WHERE fieldname = 'VAL_ATE'
*                                    OR fieldname = 'VAL_DE'.
*
*      lv_value = ls_good-value.
*      CONDENSE lv_value NO-GAPS.
*      if ls_good-fieldname eq 'VAL_ATE'.
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = ls_good-fieldname "'VAL_ATE'
*          i_value     = lv_value.
*      else.
*
*      endif.
*    ENDLOOP.
*    PERFORM verifica_erros.
*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        i_screen   = '100'
*        i_show     = space
*        i_repid    = sy-repid
*        i_popup    = 1
*       i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*       I_SET_FIELD   = 'X_FIELD'
*      IMPORTING
*        e_messagem = wg_mensagem
*      TABLES
*        it_msgs    = tg_msg_ret.
*
*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                             INTO LS_GOOD
*                             WHERE FIELDNAME = 'VLR_CUSTO'.
*                                OR FIELDNAME = 'VLR_MARGEM'.
*
*      READ TABLE TG_SAIDA INTO WL_SAIDA INDEX LS_GOOD-ROW_ID.
*
*
*      LV_VALUE = LS_GOOD-VALUE.
*      CONDENSE LV_VALUE NO-GAPS.
*      BREAK-POINT.
*      IF LS_GOOD-FIELDNAME EQ 'VLR_CUSTO'.
*        WL_VLR_VENDA =  WL_SAIDA-VLR_MARGEM + LV_VALUE.
*      ELSE.
*        WL_VLR_VENDA = LV_VALUE + WL_SAIDA-VLR_CUSTO.
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
*      select *
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
*      select *
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
*    LOOP AT er_data_changed->mt_good_cells
*                             INTO ls_good.
*
*      READ TABLE tg_itens INTO wl_itens INDEX ls_good-ROW_ID.
****> Determina o CFOP
*      IF wg_direitos-cfop IS INITIAL.
*        SELECT SINGLE *
*          FROM marc
*          INTO wl_marc
*           WHERE matnr EQ wl_itens-matnr.
*
*        IF sy-subrc IS INITIAL.
*          SELECT SINGLE *
*            FROM mbew
*            INTO wl_mbew
*             WHERE matnr EQ wl_itens-matnr
*               AND bwkey EQ wl_itens-werks.
*
*          IF sy-subrc IS INITIAL.
*            SELECT SINGLE *
*              FROM j_1bbranch
*               INTO wl_1bbranch
*               WHERE bukrs  EQ p_bukrs
*                 AND branch EQ p_branch.
*
*            IF sy-subrc IS INITIAL.
*              SELECT SINGLE *
*                FROM j_1baa
*                INTO wl_1baa
*                 WHERE nftype EQ wg_fiscal-nftype.
*
*              IF wl_1baa-entrad EQ c_x.
*                wl_direct = c_1.
*              ELSE.
*                wl_direct = c_2.
*              ENDIF.
*
*              IF wg_direitos-indcoper EQ c_d.
*                wl_dstcat = c_0.
*
*              ELSE.
*                wl_dstcat = c_1.
*
*              ENDIF.
*
*              SELECT SINGLE *
*                FROM j_1bapn
*                INTO wl_1bapn
*                 WHERE direct EQ wl_direct
*                   AND dstcat EQ wl_dstcat
*                   AND indus3 EQ wl_marc-indus
*                   AND itmtyp EQ wg_fiscal-itmtyp
*                   AND ownpro EQ wl_mbew-ownpr
*                   AND matuse EQ wl_mbew-mtuse
*                   AND indus1 EQ wl_1bbranch-industry.
*
*              IF sy-subrc IS INITIAL.
*                lv_value = wl_1bapn-cfop.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        lv_value = wg_direitos-cfop.
*
*      ENDIF.
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'CFOP'
*          i_value     = lv_value.
*
*      WL_ITENS-NETWR = wl_itens-menge * wl_itens-netpr.
*      lv_value = WL_ITENS-NETWR.
*      CONDENSE LV_VALUE.
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'NETWR'
*          i_value     = lv_value.
*    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD on_data_changed_finished.
    "BS #169488 - inicio
    DATA: ls_good        TYPE lvc_s_modi,
          wl_saida       LIKE LINE OF tg_saida,
          wl_mara        LIKE LINE OF tg_mara,
          wl_makt        LIKE LINE OF tg_makt,
          wl_t001w       LIKE LINE OF tg_t001w,
          wl_calculo     TYPE zsdt0036-vlr_margem,
          wl_vlr_venda   TYPE zsdt0036-vlr_venda,
          wl_vlr_custo   TYPE zsdt0036-vlr_custo,
          wl_perc_margem TYPE zsdt0036-perc_margem,
          lv_value       TYPE lvc_value.

    LOOP AT et_good_cells INTO ls_good.
      READ TABLE tg_saida INTO wl_saida INDEX ls_good-row_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE ls_good-fieldname.

        WHEN 'MATNR'.
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.

          SELECT SINGLE matnr matkl meins
            INTO wl_mara
            FROM mara
            WHERE matnr = lv_value.

          IF sy-subrc = 0.
            SELECT SINGLE maktx
              INTO wl_makt-maktx
              FROM makt
              WHERE matnr = wl_mara-matnr
                AND spras = sy-langu.

            wl_saida-matkl = wl_mara-matkl.
            wl_saida-meins = wl_mara-meins.
            wl_saida-maktx = wl_makt-maktx.
          ENDIF.

        WHEN 'VLR_CUSTO' OR 'PERC_MARGEM' OR 'VLR_MARGEM'.

          wl_vlr_custo = wl_saida-vlr_custo.
          wl_perc_margem = wl_saida-perc_margem.
          wl_calculo = wl_saida-vlr_margem.

          CASE ls_good-fieldname.
            WHEN 'VLR_CUSTO'.
              wl_vlr_custo = ls_good-value.
              TRY.
                  wl_calculo = ( wl_vlr_custo / ( 1 - ( wl_perc_margem / 100 ) ) ) - wl_vlr_custo.
                CATCH cx_sy_zerodivide.
                  wl_calculo = 0.
              ENDTRY.
              wl_vlr_venda = wl_calculo + wl_vlr_custo.

            WHEN 'PERC_MARGEM'.
              wl_perc_margem = ls_good-value.
              TRY.
                  wl_calculo = ( wl_vlr_custo / ( 1 - ( wl_perc_margem / 100 ) ) ) - wl_vlr_custo.
                CATCH cx_sy_zerodivide.
                  wl_calculo = 0.
              ENDTRY.
              wl_vlr_venda = wl_calculo + wl_vlr_custo.

            WHEN 'VLR_MARGEM'.
              wl_calculo = ls_good-value.
              TRY.
                  wl_perc_margem = ( wl_calculo * 100 ) / ( wl_calculo + wl_vlr_custo ).
                CATCH cx_sy_zerodivide.
                  wl_perc_margem = 0.
              ENDTRY.
              wl_vlr_venda = wl_calculo + wl_vlr_custo.
          ENDCASE.

          wl_saida-vlr_margem   = wl_calculo.
          wl_saida-perc_margem  = wl_perc_margem.
          wl_saida-vlr_venda    = wl_vlr_venda.

        WHEN 'VAL_DE' OR 'VAL_ATE'.
          CONDENSE ls_good-value NO-GAPS.

      ENDCASE.

      wl_saida-usnam      = sy-uname.
      wl_saida-data_atual = sy-datum.
      wl_saida-hora_atual = sy-uzeit.

      MODIFY tg_saida FROM wl_saida INDEX ls_good-row_id.

      CLEAR: wl_saida, wl_makt, wl_mara, wl_t001w, wl_calculo, wl_vlr_venda,
             wl_vlr_custo, wl_perc_margem, lv_value.
    ENDLOOP.

    LOOP AT tg_saida INTO tg_saida.
      DELETE tg_saida-style2 WHERE fieldname = 'VAL_ATE' OR fieldname = 'VAL_DE'.
      REFRESH: style2. ", tg_saida-style2.

*      CLEAR wa_style.
*      wa_style-fieldname = 'MEINS'.
*      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*      APPEND wa_style TO style2.
*
*      CLEAR wa_style.
*      wa_style-fieldname = 'MATKL'.
*      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*      APPEND wa_style TO style2.
*
*      CLEAR wa_style.
*      wa_style-fieldname = 'VLR_VENDA'.
*      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*      APPEND wa_style TO style2.

      IF tg_saida-status = icon_unlocked.
        IF tg_saida-val_ate GE sy-datum.
          wa_style-fieldname = 'VAL_ATE'.
          wa_style-style = alv_style_color_positive.
          APPEND wa_style TO style2.

          wa_style-fieldname = 'VAL_DE'.
          wa_style-style = alv_style_color_positive.
          APPEND wa_style TO style2.
        ELSE.
          wa_style-fieldname = 'VAL_ATE'.
          wa_style-style = alv_style_color_negative.
          APPEND wa_style TO style2.

          wa_style-fieldname = 'VAL_DE'.
          wa_style-style = alv_style_color_negative.
          APPEND wa_style TO style2.
        ENDIF.

      ELSEIF tg_saida-status = icon_locked.
        wa_style-fieldname = 'VAL_ATE'.
        wa_style-style = alv_style_color_heading.
        APPEND wa_style TO style2.

        wa_style-fieldname = 'VAL_DE'.
        wa_style-style = alv_style_color_heading.
        APPEND wa_style TO style2.
      ENDIF.

      INSERT LINES OF style2 INTO TABLE tg_saida-style2.
      MODIFY tg_saida FROM tg_saida.
    ENDLOOP.

    PERFORM verifica_erros.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
        i_popup    = 1
      IMPORTING
        e_messagem = wg_mensagem
      TABLES
        it_msgs    = tg_msg_ret.

    CALL METHOD grid1->get_selected_rows
      IMPORTING
        et_index_rows = lt_row_index.

    IF lt_row_index IS INITIAL.
      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.


    "BS #169488 - fim

*    DATA: tl_saida LIKE TABLE OF tg_saida,
*          wl_saida LIKE LINE OF tg_saida,
*          ls_good  TYPE lvc_s_modi,
*          tl_makt  LIKE TABLE OF tg_makt,
*          wl_makt  LIKE LINE OF tg_makt,
*          tl_mara  LIKE TABLE OF tg_mara,
*          wl_mara  LIKE LINE OF tg_mara,
*          tl_t001w LIKE TABLE OF tg_t001w,
*          wl_t001w LIKE LINE  OF tg_t001w.
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
*    REFRESH: tl_mara, tl_makt, tl_t001w, tl_saida.
*
*    LOOP AT et_good_cells INTO ls_good
*       WHERE tabix GT 0.
*
*      READ TABLE tg_saida INTO wl_saida INDEX ls_good-row_id.
*
*      APPEND wl_saida TO tl_saida.
*    ENDLOOP.
*
*    IF tl_saida[] IS NOT INITIAL.
**      select werks name1
**        from t001w
**        into table tl_t001w
**         for all entries in tl_saida
**          where werks eq tl_saida-werks.
*
*      SELECT matnr maktx
*        FROM makt
*        INTO TABLE tl_makt
*         FOR ALL ENTRIES IN tl_saida
*         WHERE matnr EQ tl_saida-matnr
*           AND spras EQ sy-langu.
*
*      SELECT matnr matkl meins
*        FROM mara
*        INTO TABLE tl_mara
*         FOR ALL ENTRIES IN tl_saida
*         WHERE matnr EQ tl_saida-matnr.
*
*    ENDIF.
*    SORT: tl_makt BY matnr,
*          tl_mara BY matnr.
**          tl_t001w by werks.
*
*    LOOP AT et_good_cells INTO ls_good
*      WHERE tabix GT 0.
*
*      READ TABLE tg_saida INTO wl_saida INDEX ls_good-row_id.
*
*      READ TABLE tl_makt INTO wl_makt
*        WITH KEY matnr = wl_saida-matnr
*                 BINARY SEARCH.
*
*      READ TABLE tl_mara INTO wl_mara
*        WITH KEY matnr = wl_saida-matnr
*                 BINARY SEARCH.
*
**      read table tl_t001w into wl_t001w
**        with key werks = wl_saida-werks
**                 binary search.
*
*      MOVE: wl_makt-maktx TO wl_saida-maktx,
*            wl_mara-matkl TO wl_saida-matkl.
**            WL_SAIDA-MEINS TO WL_SAIDA-meins,
**            wl_t001w-name1 to wl_saida-name1.
*
*      wl_saida-usnam      = sy-uname.
*      wl_saida-data_atual = sy-datum.
*      wl_saida-hora_atual = sy-uzeit.
*
*      MODIFY tg_saida FROM wl_saida INDEX ls_good-row_id.
*      CLEAR: wl_saida, wl_t001w, wl_mara, wl_makt.
*    ENDLOOP.
*
**    refresh: style2, tg_saida-style2.
*    LOOP AT tg_saida INTO tg_saida.
*      REFRESH: style2, tg_saida-style2.
*      IF tg_saida-status EQ icon_unlocked.
*        IF tg_saida-val_ate GE sy-datum.
*          wa_style-fieldname = 'VAL_ATE'.
*          wa_style-style = alv_style_color_positive..
*          INSERT  wa_style INTO TABLE style2.
*          CLEAR: wa_style.
*          wa_style-fieldname = 'VAL_DE'.
*          wa_style-style = alv_style_color_positive.
*          INSERT  wa_style INTO TABLE style2.
*
*          INSERT LINES OF style2 INTO TABLE tg_saida-style2.
*        ELSE.
*          CLEAR: wa_style.
*          wa_style-fieldname = 'VAL_ATE'.
*          wa_style-style = alv_style_color_negative..
*          INSERT  wa_style INTO TABLE style2.
*          CLEAR: wa_style.
*          wa_style-fieldname = 'VAL_DE'.
*          wa_style-style = alv_style_color_negative.
*          INSERT  wa_style INTO TABLE style2.
*          INSERT LINES OF style2 INTO TABLE tg_saida-style2.
*        ENDIF.
*      ELSEIF tg_saida-status EQ icon_locked.
*        CLEAR: wa_style.
*        wa_style-fieldname = 'VAL_ATE'.
*        wa_style-style =  alv_style_color_heading..
*        INSERT  wa_style INTO TABLE style2.
*        CLEAR: wa_style.
*        wa_style-fieldname = 'VAL_DE'.
*        wa_style-style =  alv_style_color_heading.
*        INSERT  wa_style INTO TABLE style2.
*        INSERT LINES OF style2 INTO TABLE tg_saida-style2.
*      ENDIF.
*      MODIFY tg_saida FROM tg_saida.
*      REFRESH: style2.
*    ENDLOOP.
*
*    PERFORM verifica_erros.
*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        i_screen   = '100'
*        i_show     = space
*        i_repid    = sy-repid
*        i_popup    = 1
**       i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
**       I_SET_FIELD   = 'X_FIELD'
*      IMPORTING
*        e_messagem = wg_mensagem
*      TABLES
*        it_msgs    = tg_msg_ret.
*
*    CALL METHOD grid1->refresh_table_display
*      EXPORTING
*        is_stable = wa_stable.
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
  METHOD on_button_click.
    DATA: wl_saida LIKE LINE OF tg_saida.
*    es_col_id
    IF es_row_no-row_id GT 0.
      READ TABLE tg_saida INTO wl_saida INDEX es_row_no-row_id.

      REFRESH: style2, wl_saida-style2.

      IF wl_saida-status EQ icon_locked.
        MOVE icon_unlocked TO wl_saida-status.

      ELSE.
        MOVE icon_locked TO wl_saida-status.
      ENDIF.

      IF wl_saida-status EQ icon_unlocked.
        IF wl_saida-val_ate GE sy-datum.
          wa_style-fieldname = 'VAL_ATE'.
          wa_style-style = alv_style_color_positive..
          INSERT  wa_style INTO TABLE style2.
          CLEAR: wa_style.
          wa_style-fieldname = 'VAL_DE'.
          wa_style-style = alv_style_color_positive.
          INSERT  wa_style INTO TABLE style2.

          INSERT LINES OF style2 INTO TABLE wl_saida-style2.
        ELSE.
          CLEAR: wa_style.
          wa_style-fieldname = 'VAL_ATE'.
          wa_style-style = alv_style_color_negative..
          INSERT  wa_style INTO TABLE style2.
          CLEAR: wa_style.
          wa_style-fieldname = 'VAL_DE'.
          wa_style-style = alv_style_color_negative.
          INSERT  wa_style INTO TABLE style2.
          INSERT LINES OF style2 INTO TABLE wl_saida-style2.
        ENDIF.
      ELSEIF wl_saida-status EQ icon_locked.
        CLEAR: wa_style.
        wa_style-fieldname = 'VAL_ATE'.
        wa_style-style =  alv_style_color_heading..
        INSERT  wa_style INTO TABLE style2.
        CLEAR: wa_style.
        wa_style-fieldname = 'VAL_DE'.
        wa_style-style =  alv_style_color_heading.
        INSERT  wa_style INTO TABLE style2.
        INSERT LINES OF style2 INTO TABLE wl_saida-style2.
      ENDIF.
      wl_saida-usnam      = sy-uname.
      wl_saida-data_atual = sy-datum.
      wl_saida-hora_atual = sy-uzeit.
      MODIFY tg_saida FROM wl_saida INDEX es_row_no-row_id.
    ENDIF.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "on_button_click
  METHOD on_onf4.
    TYPES: BEGIN OF tyl_field,
             tabname   TYPE dd03l-tabname,    "Nome da tabela
             fieldname TYPE dd03l-fieldname,    "Nome de campo
             s(1)      TYPE c,
           END OF tyl_field,

           BEGIN OF tyl_value,
             tabname    TYPE dd03l-tabname,    "Nome da tabela
             fieldname  TYPE dd03l-fieldname,    "Nome de campo
             char79(79) TYPE c,
           END OF tyl_value.

    DATA: BEGIN OF wl_cultura,
            field(50),
          END OF wl_cultura.

    DATA: BEGIN OF wl_umb,
            field(50),
          END OF wl_umb.
    DATA: tl_cultura  LIKE TABLE OF wl_cultura,
          tl_umb      LIKE TABLE OF wl_umb,
          tl_0038     TYPE  TABLE OF zsdt0038,
          tl_marm     TYPE  TABLE OF marm,
          wl_0038     TYPE zsdt0038,
          wl_marm     TYPE marm,
          wl_saida    LIKE LINE OF tg_saida,
          tl_field    TYPE TABLE OF tyl_field,
          wl_field    TYPE tyl_field,
          tl_value    TYPE TABLE OF tyl_value,
          wl_value    TYPE tyl_value,
          wl_char(20),
          wl_index    TYPE sy-tabix.

    IF e_fieldname EQ 'CULTURA'.
* Início - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura
      SELECT *
        FROM zsdt0038
        INTO TABLE tl_0038.

      "Considerar Somente Culturas habilitadas para Pagamento
      tl_0038[] = tg_0038[].
      DELETE tl_0038 WHERE pagamento IS INITIAL.
* Fim - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura

      LOOP AT tl_0038 INTO wl_0038.

        MOVE: wl_0038-cultura TO wl_cultura-field.
        APPEND wl_cultura TO tl_cultura.

        MOVE: wl_0038-descricao TO wl_cultura-field.
        APPEND wl_cultura TO tl_cultura.
      ENDLOOP.

      wl_field-tabname = 'ZSDT0038'.
      wl_field-fieldname = 'CULTURA'.
      wl_field-s = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname = 'ZSDT0038'.
      wl_field-fieldname = 'DESCRICAO'.
      wl_field-s = ' '.
      APPEND wl_field TO tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
*         cucol                     = '3'
          fieldname                 = 'CULTURA'
          tabname                   = 'ZSDT0038'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_cultura
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        IF wl_index IS NOT INITIAL.                       "Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura
          READ TABLE tl_0038 INTO wl_0038 INDEX wl_index.
          IF es_row_no-row_id GT 0.
            READ TABLE tg_saida INTO tg_saida INDEX es_row_no-row_id.
            IF sy-subrc IS INITIAL.
              MOVE: wl_0038-cultura TO tg_saida-cultura.
              MODIFY tg_saida FROM tg_saida INDEX es_row_no-row_id.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF es_row_no-row_id GT 0.
        READ TABLE tg_saida INTO wl_saida INDEX es_row_no-row_id.

        SELECT *
          FROM marm
          INTO TABLE tl_marm
           WHERE matnr EQ wl_saida-matnr.

        IF sy-subrc IS INITIAL.
          LOOP AT tl_marm INTO wl_marm.

            MOVE: wl_marm-meinh TO wl_umb-field.
            APPEND wl_umb TO tl_umb.

*        MOVE: wl_0038-descricao TO wl_cultura-field.
*        APPEND wl_cultura TO tl_cultura.
          ENDLOOP.

          wl_field-tabname = 'ZSDT0036'.
          wl_field-fieldname = 'MEINS'.
          wl_field-s = 'X'.
          APPEND wl_field TO tl_field.

*      wl_field-tabname = 'ZSDT0038'.
*      wl_field-fieldname = 'DESCRICAO'.
*      wl_field-s = ' '.
*      APPEND wl_field TO tl_field.

          CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
            EXPORTING
*             cucol                     = '3'
              fieldname                 = 'MEINS'
              tabname                   = 'ZSDT0036'
            IMPORTING
              index                     = wl_index
              select_value              = wl_char
            TABLES
              fields                    = tl_field
              select_values             = tl_value
              valuetab                  = tl_umb
            EXCEPTIONS
              field_not_in_ddic         = 001
              more_then_one_selectfield = 002
              no_selectfield            = 003.
          IF sy-subrc IS INITIAL.
            READ TABLE tl_marm INTO wl_marm INDEX wl_index.
            IF es_row_no-row_id GT 0.
              READ TABLE tg_saida INTO tg_saida INDEX es_row_no-row_id.
              IF sy-subrc IS INITIAL.
                MOVE: wl_marm-meinh TO tg_saida-meins.
                MODIFY tg_saida FROM tg_saida INDEX es_row_no-row_id.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

**** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
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
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.
  IF wg_display IS NOT INITIAL.
    APPEND c_save TO fcode.

  ENDIF.
  PERFORM verifica_erros.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen   = '100'
      i_show     = space
      i_repid    = sy-repid
      i_popup    = 1
*     i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*     I_SET_FIELD   = 'X_FIELD'
    IMPORTING
      e_messagem = wg_mensagem
    TABLES
      it_msgs    = tg_msg_ret.

  IF wg_cell IS NOT INITIAL .
    REFRESH: tg_cell.
    APPEND wg_cell TO tg_cell.
*          CONCATENATE wl_obj '->SET_SELECTED_CELLS' INTO wg_obj.
    CALL METHOD grid1->set_selected_cells "(wg_obj)          "(wg_msgs)=>set_selected_cells
      EXPORTING
        it_cells = tg_cell[].
  ENDIF.

  SET PF-STATUS 'Z001' EXCLUDING fcode.
  SET TITLEBAR 'Z001'.
  IF sy-subrc IS INITIAL.


  ENDIF.


ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: wl_repid    TYPE sy-repid,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        lt_f4       TYPE lvc_t_f4 WITH HEADER LINE,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt.

  wl_repid = sy-repid.

  IF container1 IS INITIAL.
    wa_layout-zebra      = c_x.
*    wa_layout-no_rowmark = c_x.
    wa_layout-cwidth_opt = c_x.
    wa_stable-row        = c_x.
    "BS #169488 - inicio
    wa_layout-sel_mode   = 'D'.
    "BS #169488 - fim
    wa_layout-box_fname  = 'MARK'.

    "BS #169488 - inicio
*Desativa edição em modo Visualizar
    IF p_visual IS NOT INITIAL.
      wa_layout-edit = abap_false.
    ELSE.
      wa_layout-edit = abap_true.
    ENDIF.
    "BS #169488 - fim

    CREATE OBJECT container1
      EXPORTING
        repid     = wl_repid
        dynnr     = '0100'
*       style     = container1->WS_MINIMIZEBOX
        side      = container1->dock_at_top
        extension = 400.
*        METRIC    = 50.

*    CALL METHOD CONTAINER1->FLOAT
*       EXPORTING
*        DO_FLOAT     = 1.
**        RATIO = 95.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*   *Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_mb_filter.
    APPEND wl_function TO tl_function.

    PERFORM montar_layout.
    PERFORM build_dropdown.

    wa_layout-ctab_fname = 'CELLCOLORS'.
    wa_layout-stylefname = 'STYLE2'.

    lt_f4-fieldname = 'CULTURA'.
    lt_f4-register = 'X' .
    lt_f4-getbefore = 'X' .
    APPEND lt_f4 .

    lt_f4-fieldname = 'MEINS'.
    lt_f4-register = 'X' .
    lt_f4-getbefore = 'X' .
    APPEND lt_f4 .

    wl_filter-fieldname = 'ELIMINADO'."c_dmbtr.
    wl_filter-sign      = 'I'. "c_i.
    wl_filter-option    = 'NE'. "c_ne.
    wl_filter-low       = 'X'.

    APPEND wl_filter TO tl_filter.

    "BS #169488 - inicio
    IF p_visual IS INITIAL.
      PERFORM lock_table.                                   "FF #171337

      IF gv_bloqueado = abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.
    "BS #169488 - fim

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_saida[].

    CALL METHOD grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid1->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER:
              lcl_event_handler=>on_double_click          FOR grid1,
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_data_changed          FOR grid1,
              lcl_event_handler=>on_button_click          FOR grid1,
              lcl_event_handler=>on_onf4                  FOR grid1.
  ELSE.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
    "BS #169488 - inicio
    CALL METHOD grid1->set_toolbar_interactive.
    "BS #169488 - fim
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init OUTPUT.
  DATA: wl_view_name TYPE ocus-table VALUE 'ZSDT0036',
        tl_rangetab  TYPE TABLE OF vimsellist,
        wl_lockuser  TYPE sy-uname,
        answer.

  IF init IS INITIAL.
    IF p_visual IS INITIAL.
      CLEAR: wg_display.
    ENDIF.

    "FF #168911 - inicio
*    CALL FUNCTION 'VIEW_ENQUEUE'
*      EXPORTING
*        view_name        = wl_view_name
*        action           = 'E'
*        enqueue_mode     = 'E'
**       enqueue_range    = "header-subsetflag
*      TABLES
*        sellist          = tl_rangetab
*      EXCEPTIONS
*        foreign_lock     = 1
*        system_failure   = 2
*        table_not_found  = 5
*        client_reference = 7.
*    CASE sy-subrc.
*      WHEN 1.
**SW: interne Meldung 0000293299 1995
**         message i049 with sy-msgv1 raising foreign_lock.
**         move: s to action.
**XB: Change to new function moduls POPUP_TO_DECIDE_LOCKED_DATA -Begin
**          text = text-101.
**          REPLACE '&' WITH sy-msgv1(12) INTO text.
**          CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
**            EXPORTING
**              titel          = text-100
**              diagnosetext1  = text
**              diagnosetext2  = text-102
**              textline1      = text-103
**              defaultoption  = 'Y'
**              cancel_display = space
**            IMPORTING
**              answer         = answer.
**          IF answer = 'A' OR answer = 'N'.
**            MESSAGE e049 WITH sy-msgv1 RAISING foreign_lock.
**          ELSEIF answer = 'J'.
**            MOVE: s TO action.
**          ENDIF.
*        wl_lockuser = sy-msgv1(12).     "HCG sy-msgv1 lost at popup call
*        CALL FUNCTION 'POPUP_TO_DECIDE_LOCKED_DATA'
*          EXPORTING
*            i_user   = sy-msgv1(12)
**           I_START_COLUMN       = 9
**           I_START_ROW          = 9
*          IMPORTING
*            e_answer = answer.
*        IF answer = '2'.
**IG: internal message 305882 2005
**            MESSAGE e049 WITH lockuser RAISING foreign_lock.
*          MESSAGE s049(sv) WITH wl_lockuser RAISING foreign_lock.
*          EXIT.
*        ELSEIF answer = '1'.
*          MOVE: c_x TO wg_display.
*        ENDIF.
*      WHEN 2.
*        MESSAGE e050(sv) WITH wl_view_name RAISING system_failure.
*      WHEN 5.
*        MESSAGE e028(sv) WITH wl_view_name RAISING view_not_found.
*      WHEN 7.
*        MESSAGE e054(sv) WITH sy-mandt RAISING client_reference.
*    ENDCASE.
**    ENDIF.

    "FF #168911 - fim




    PERFORM seleciona_dados.
    PERFORM organiza_dados.
    init = c_x.
  ENDIF.
ENDMODULE.                 " INIT  OUTPUT

FORM lock_table.

  "BS #169488 - inicio
  DATA: lv_safra TYPE zsdt0036-safra,
        lv_matkl TYPE mara-matkl.

  LOOP AT s_safra INTO DATA(wa_safra).
    LOOP AT s_matkl INTO DATA(wa_matkl).

*      lv_safra = wa_safra-low.
*      lv_matkl = wa_matkl-low.

      DATA: lt_safra TYPE STANDARD TABLE OF zsdt0036-safra WITH EMPTY KEY.
      DATA(lv_safra_desc) = |Safra: { wa_safra-low }|.

      IF wa_safra-option = 'BT' AND wa_safra-high IS NOT INITIAL.
        lv_safra_desc = |Safra: { wa_safra-low } até { wa_safra-high }|.
      ENDIF.

      IF wa_safra-option = 'BT'.
        DO 1 + wa_safra-high - wa_safra-low TIMES.
          APPEND wa_safra-low + sy-index - 1 TO lt_safra.
        ENDDO.
      ELSE.
        APPEND wa_safra-low TO lt_safra.
      ENDIF.

      DATA: lt_matkl TYPE STANDARD TABLE OF mara-matkl WITH EMPTY KEY.
      DATA(lv_matkl_desc) = |Grupo: { wa_matkl-low }|.

      IF wa_matkl-option = 'BT' AND wa_matkl-high IS NOT INITIAL.
        lv_matkl_desc = |Grupo: { wa_matkl-low } até { wa_matkl-high }|.
      ENDIF.

      IF wa_matkl-option = 'BT'.
        DO 1 + wa_matkl-high - wa_matkl-low TIMES.
          APPEND wa_matkl-low + sy-index - 1 TO lt_matkl.
        ENDDO.
      ELSE.
        APPEND wa_matkl-low + sy-index TO lt_matkl.
      ENDIF.

      LOOP AT lt_safra INTO lv_safra.
        LOOP AT lt_matkl INTO lv_matkl.

          CALL FUNCTION 'ENQUEUE_EZSDR015'
            EXPORTING
              bukrs          = s_bukrs-low
              safra          = lv_safra
              matkl          = lv_matkl
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc <> 0.
            DATA(l_user_lock) = sy-msgv1.

            DELETE tg_saida WHERE bukrs = s_bukrs-low
                            AND   safra = lv_safra
                            AND   matkl = lv_matkl.

            DATA(l_msg) = |O usuário { l_user_lock } já está executando dados para: | && |Empresa: { s_bukrs-low }, { lv_safra_desc }, { lv_matkl_desc }|.
            MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E'.

            gv_bloqueado = abap_true.
            RETURN.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ENDLOOP.
  ENDLOOP.
  "BS #169488 - fim

*  CALL FUNCTION 'ENQUEUE_EZSDR015'
*    EXPORTING
*      bukrs          = s_bukrs-low
*      safra          = s_safra-low
*      matkl          = s_matkl-low
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
*
*  IF sy-subrc <> 0.
*    DATA(l_user_lock) = sy-msgv1.
*
*    DELETE tg_saida WHERE bukrs = s_bukrs-low.
*
*    DATA(l_msg) = |O usuário { l_user_lock } já está executando dados para o filtro com a empresa { s_bukrs-low }|.
*    MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*
*  ENDIF.

ENDFORM.

FORM unlock_table.

  CALL FUNCTION 'DEQUEUE_EZSDR015'
    EXPORTING
      bukrs          = s_bukrs-low
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .
  REFRESH: tg_saida, tg_0036, tg_makt, tg_mara.

  SELECT *
    FROM zsdt0036
    INTO TABLE tg_0036
     WHERE bukrs   IN s_bukrs
       AND cultura IN s_cultu
       AND safra   IN s_safra.

  IF sy-subrc IS INITIAL.
*    select werks name1
*      from t001w
*      into table tg_t001w
*       for all entries in tg_0036
*        where werks eq tg_0036-werks.

    SELECT matnr maktx
      FROM makt
      INTO TABLE tg_makt
       FOR ALL ENTRIES IN tg_0036
       WHERE matnr EQ tg_0036-matnr
         AND spras EQ sy-langu.

    SELECT matnr matkl meins
      FROM mara
      INTO TABLE tg_mara
       FOR ALL ENTRIES IN tg_0036
       WHERE matnr EQ tg_0036-matnr
         AND matkl IN s_matkl.
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
FORM montar_layout.
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
*        0 ' '         ' '            'TG_SAIDA' 'MARK'           'Selecionar'       '1' ' ' ' ' ' ',
        0 ' '         ' '            'TG_SAIDA' 'STATUS'         'Status'           '5' ' ' ' ' ' ',
        1 'ZSDT0036'  'BUKRS'        'TG_SAIDA' 'BUKRS'          'Empresa'          '4' ' ' ' ' ' ',
        1 'ZSDT0036'  'VAL_DE'       'TG_SAIDA' 'VAL_DE'         ''                 ' ' 'X' ' ' ' ',
        1 'ZSDT0036'  'VAL_ATE'      'TG_SAIDA' 'VAL_ATE'        ''                 ' ' 'X' ' ' ' ',
        1 'ZSDT0036'  'DTVENC'       'TG_SAIDA' 'DTVENC'         'Data Vencimento'  ' ' 'X' ' ' ' ',
        3 'ZSDT0036'  'MATNR'        'TG_SAIDA' 'MATNR'          ''                 ' ' 'X' ' ' ' ',
        4 'MAKT'      'MAKTX'        'TG_SAIDA' 'MAKTX'          ''                 ' ' ' ' ' ' ' ',
        4 ' '         ' '            'TG_SAIDA' 'CULTURA'        'Cultura'          ' ' 'X' ' ' ' ',
        4 'ZSDT0036'  'SAFRA'        'TG_SAIDA' 'SAFRA'          'Safra'            ' ' 'X' ' ' ' ',
        5 ' '         ' '            'TG_SAIDA' 'MEINS'          'UMB'              ' ' 'X' ' ' ' ',
        5 'ZSDT0036'  'WERKS_FORNEC' 'TG_SAIDA' 'WERKS_FORNEC'   'Centro Fornec.'   ' ' 'X' ' ' ' ',
        5 'ZSDT0036'  'INCO1'        'TG_SAIDA' 'INCO1'          'Tipo frete'       ' ' 'X' ' ' ' ',
        5 'MARA'      'MATKL'        'TG_SAIDA' 'MATKL'          ''                 ' ' ' ' ' ' ' ',
        5 'ZSDT0036'  'WAERK'        'TG_SAIDA' 'WAERK'          ''                 ' ' 'X' ' ' ' ',
        5 'ZSDT0036'  'VLR_CUSTO'    'TG_SAIDA' 'VLR_CUSTO'      'Vlr.Custo'        ' ' 'X' ' ' ' ',
        6 'ZSDT0036'  'PERC_MARGEM'  'TG_SAIDA' 'PERC_MARGEM'    'Margem %'         ' ' 'X' ' ' ' ',
        7 'ZSDT0036'  'VLR_MARGEM'   'TG_SAIDA' 'VLR_MARGEM'     'Margem Vlr.'      ' ' 'X' ' ' ' ',
        8 'ZSDT0036'  'VLR_VENDA'    'TG_SAIDA' 'VLR_VENDA'      'Vlr.Venda'        ' ' ' ' ' ' ' '.

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
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
*  w_fieldcatalog-key_sel       = 'X'.
  IF wg_display IS INITIAL.
    w_fieldcatalog-edit          = p_edit.
  ENDIF.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  IF p_field EQ 'STATUS'.
*    w_fieldcatalog-checkbox = c_x.
*    w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
*    w_fieldcatalog-edit          = c_x.
    IF wg_display IS INITIAL.
      w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
    ELSE.
*      w_fieldcatalog-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
  ENDIF.

  IF p_field EQ 'MEINS'.
*    w_fieldcatalog-checktable = 'T006'.
    w_fieldcatalog-f4availabl = c_x.
    w_fieldcatalog-convexit = 'CUNIT'.
  ENDIF.

  IF p_field EQ 'INCO1'.
    w_fieldcatalog-drdn_hndl  = 1.
  ENDIF.

  IF p_field EQ 'CULTURA'.
    w_fieldcatalog-f4availabl  = 'X'.
  ENDIF.

* Início - Sara Oikawa - Set/2020 - 44726
  IF p_field EQ 'WAERK'.
    w_fieldcatalog-f4availabl  = 'X'.
  ENDIF.
* Fim - Sara Oikawa - Set/2020 - 44726

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: wl_cell    TYPE lvc_s_cell,
        tl_cell    TYPE lvc_t_cell,
        wl_obj(30).

  REFRESH: tl_cell.
  CLEAR: wl_cell, wl_obj.

  CASE ok_code.
    WHEN c_save.
      IF p_visual IS INITIAL.

        CALL METHOD grid1->check_changed_data.
        PERFORM verifica_erros.
        IF tg_msg_ret[] IS NOT INITIAL.
          CALL FUNCTION 'Z_DOC_CHECK_NEW'
            EXPORTING
              i_screen   = '100'
              i_show     = c_x
              i_repid    = sy-repid
              i_popup    = 1
              i_set_cell = 'WG_CELL'
              i_set_obj  = 'WL_OBJ'
*             i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*             I_SET_FIELD   = 'X_FIELD'
            IMPORTING
              e_messagem = wg_mensagem
            TABLES
              it_msgs    = tg_msg_ret.
        ELSE.
          PERFORM grava_dados.
          PERFORM seleciona_dados.
          PERFORM organiza_dados.
        ENDIF.
      ELSE.
        MESSAGE 'Ação não permitida para o modo Visualização'  TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    WHEN c_search.
    WHEN c_show_msgre.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen   = '100'
            i_show     = c_x
            i_repid    = sy-repid
            i_popup    = 1
            i_set_cell = 'WG_CELL'
            i_set_obj  = 'WL_OBJ'
*           i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*           I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            e_messagem = wg_mensagem
          TABLES
            it_msgs    = tg_msg_ret.
      ENDIF.

    WHEN c_back.
      LEAVE TO SCREEN 0.
    WHEN c_cancel.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 100.
    WHEN c_exit.
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
FORM organiza_dados .
  DATA: ls_cellcolor TYPE lvc_s_scol.
  SORT: tg_makt BY matnr,
        tg_mara BY matnr,
        tg_t001w BY werks.

  LOOP AT tg_0036.
    READ TABLE tg_makt
      WITH KEY matnr = tg_0036-matnr
               BINARY SEARCH.

    READ TABLE tg_mara
      WITH KEY matnr = tg_0036-matnr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.

*    read table tg_t001w
*      with key werks = tg_0036-werks
*               binary search.

      MOVE: tg_0036-val_de       TO  tg_saida-val_de,
            tg_0036-val_ate      TO  tg_saida-val_ate,
            tg_0036-dtvenc       TO  tg_saida-dtvenc,
            tg_0036-matnr        TO  tg_saida-matnr,
            tg_makt-maktx        TO  tg_saida-maktx,
            tg_mara-matkl        TO  tg_saida-matkl,
            tg_0036-cultura      TO  tg_saida-cultura,
            tg_0036-safra        TO  tg_saida-safra,
            tg_0036-waerk        TO  tg_saida-waerk,
            tg_0036-meins        TO  tg_saida-meins,
            tg_0036-werks_fornec TO  tg_saida-werks_fornec,
            tg_0036-vlr_custo    TO  tg_saida-vlr_custo,
            tg_0036-inco1        TO  tg_saida-inco1,
            tg_0036-perc_margem  TO  tg_saida-perc_margem,
            tg_0036-vlr_margem   TO  tg_saida-vlr_margem,
            tg_0036-vlr_venda    TO  tg_saida-vlr_venda,
            tg_0036-eliminado    TO  tg_saida-eliminado,
            tg_0036-usnam        TO  tg_saida-usnam,
            tg_0036-data_atual   TO  tg_saida-data_atual,
            tg_0036-hora_atual   TO  tg_saida-hora_atual,
            tg_0036-bukrs        TO  tg_saida-bukrs.

      IF tg_0036-loekz EQ c_x.
        MOVE: icon_locked     TO  tg_saida-status.
      ELSE.
        MOVE: icon_unlocked    TO  tg_saida-status.
      ENDIF.

      APPEND tg_saida.
    ENDIF.
    CLEAR: tg_saida, tg_0036, tg_makt, tg_mara, tg_t001w.

  ENDLOOP.

  SORT: tg_saida BY matnr.

  REFRESH: style2, tg_saida-style2.
  LOOP AT tg_saida.
    REFRESH: style2, tg_saida-style2.

    CLEAR wa_style.
    wa_style-fieldname = 'MEINS'.
    wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT  wa_style INTO TABLE style2.

    CLEAR: wa_style.
    wa_style-fieldname = 'MATKL'.
    wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT  wa_style INTO TABLE style2.

    CLEAR: wa_style.
    wa_style-fieldname = 'VLR_VENDA'.
    wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT  wa_style INTO TABLE style2.

    IF tg_saida-status EQ icon_unlocked.
      IF tg_saida-val_ate GE sy-datum.
        wa_style-fieldname = 'VAL_ATE'.
        wa_style-style = alv_style_color_positive..
        INSERT  wa_style INTO TABLE style2.
        CLEAR: wa_style.
        wa_style-fieldname = 'VAL_DE'.
        wa_style-style = alv_style_color_positive.
        INSERT  wa_style INTO TABLE style2.

        INSERT LINES OF style2 INTO TABLE tg_saida-style2.
      ELSE.
        CLEAR: wa_style.
        wa_style-fieldname = 'VAL_ATE'.
        wa_style-style = alv_style_color_negative..
        INSERT  wa_style INTO TABLE style2.
        CLEAR: wa_style.
        wa_style-fieldname = 'VAL_DE'.
        wa_style-style = alv_style_color_negative.
        INSERT  wa_style INTO TABLE style2.
        INSERT LINES OF style2 INTO TABLE tg_saida-style2.
      ENDIF.
    ELSEIF tg_saida-status EQ icon_locked.
      CLEAR: wa_style.
      wa_style-fieldname = 'VAL_ATE'.
      wa_style-style =  alv_style_color_heading..
      INSERT  wa_style INTO TABLE style2.
      CLEAR: wa_style.
      wa_style-fieldname = 'VAL_DE'.
      wa_style-style =  alv_style_color_heading.
      INSERT  wa_style INTO TABLE style2.
      INSERT LINES OF style2 INTO TABLE tg_saida-style2.
    ENDIF.
    MODIFY tg_saida.
    REFRESH: style2.
  ENDLOOP.

  IF tg_saida[] IS NOT INITIAL.
    tg_saida_aux[] = tg_saida[].
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
FORM grava_dados .
  DATA: tl_input_0036 TYPE TABLE OF zsdt0036 WITH HEADER LINE,
        tl_0036       TYPE TABLE OF zsdt0036 WITH HEADER LINE,
        tl_mara       TYPE TABLE OF mara WITH HEADER LINE.

  CLEAR: tl_input_0036, tl_0036, tl_mara.
  REFRESH: tl_input_0036, tl_0036, tl_mara.
  SELECT *
    FROM zsdt0036
    INTO TABLE tl_0036
     WHERE safra IN s_safra
       AND cultura IN s_cultu.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM mara
      INTO TABLE tl_mara
       FOR ALL ENTRIES IN tl_0036
        WHERE matnr EQ tl_0036-matnr
          AND matkl IN s_matkl.

  ENDIF.
*  DELETE FROM ZSDT0036 WHERE SAFRA   IN S_SAFRA
*                         AND CULTURA IN S_CULTU.

*  LOOP AT TL_0036.
*    READ TABLE TL_MARA
*      WITH KEY MATNR = TL_0036-MATNR.
*    IF SY-SUBRC IS INITIAL.
*     delete zsdt0036 from tl_0036.
*    ENDIF.
*  ENDLOOP.

  DELETE tg_saida WHERE val_de      IS INITIAL
                    AND val_ate     IS INITIAL
*                    and werks       is initial
*                    and name1       is initial
                    AND matnr       IS INITIAL
                    AND maktx       IS INITIAL
                    AND matkl       IS INITIAL
                    AND vlr_custo   IS INITIAL
*                    and inco1       is initial
                    AND perc_margem IS INITIAL
                    AND vlr_margem  IS INITIAL
                    AND vlr_venda   IS INITIAL.

  LOOP AT tg_saida.
    MOVE-CORRESPONDING: tg_saida TO tl_input_0036.

    IF tg_saida-status EQ icon_locked.
      MOVE : c_x TO tl_input_0036-loekz.
    ELSE.
      MOVE : space TO tl_input_0036-loekz.
    ENDIF.
    APPEND tl_input_0036.
    CLEAR: tl_input_0036.

  ENDLOOP.

** Rotina para encontra os itens com chave mudadas, e marcar eles pra
* eliminacao na base.
  SORT: tg_saida BY val_de val_ate dtvenc matnr waerk
                    inco1 safra cultura werks_fornec.
  LOOP AT tg_saida_aux.
    READ TABLE tg_saida TRANSPORTING NO FIELDS
     WITH KEY val_de          = tg_saida_aux-val_de
              val_ate         = tg_saida_aux-val_ate
              dtvenc          = tg_saida_aux-dtvenc
              matnr           = tg_saida_aux-matnr
              waerk           = tg_saida_aux-waerk
              inco1           = tg_saida_aux-inco1
              safra           = tg_saida_aux-safra
              cultura         = tg_saida_aux-cultura
              werks_fornec    = tg_saida_aux-werks_fornec
                   BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      DELETE tg_saida_aux.
    ENDIF.
  ENDLOOP.

  MODIFY zsdt0036 FROM TABLE tl_input_0036.
  IF sy-subrc IS INITIAL.
    LOOP AT tg_saida_aux.
      DELETE FROM zsdt0036 WHERE val_de       = tg_saida_aux-val_de
                             AND val_ate      = tg_saida_aux-val_ate
                             AND dtvenc       = tg_saida_aux-dtvenc
                             AND matnr        = tg_saida_aux-matnr
                             AND waerk        = tg_saida_aux-waerk
                             AND inco1        = tg_saida_aux-inco1
                             AND safra        = tg_saida_aux-safra
                             AND cultura      = tg_saida_aux-cultura
                             AND werks_fornec = tg_saida_aux-werks_fornec.

    ENDLOOP.
    MESSAGE s836(sd) WITH 'Os dados foram salvos!'.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ocorreu um erro na gravação,'
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
FORM verifica_erros .
  DATA: tl_makt     LIKE TABLE OF tg_makt,
        wl_makt     LIKE LINE OF tg_makt,
        tl_mara     LIKE TABLE OF tg_mara,
        wl_mara     LIKE LINE OF tg_mara,
        tl_t001w    LIKE TABLE OF t001w,
        wl_t001w    LIKE LINE  OF tl_t001w,
        tl_t006     TYPE TABLE OF t006,
        wl_t006     TYPE t006,
        tl_0038     TYPE TABLE OF zsdt0038,
        wl_0038     TYPE zsdt0038,
        tl_marm     TYPE TABLE OF marm,
        wl_marm     TYPE marm,
*            tl_t001W type table of t001W,
*            Wl_t001W LIKE LINE OF t001W,
        tl_cell     TYPE lvc_t_cell,
        wl_linha(6).

  REFRESH: tl_mara, tl_makt, tl_t001w, tg_msg_ret, tl_t006, tl_t001w, tl_0038, tl_marm.
  CLEAR: wl_mara, wl_makt, wl_t001w, tg_msg_ret, wl_t006, wl_t001w, wl_0038, wl_marm.

  IF tg_saida[] IS NOT INITIAL.
*    select werks name1
*      from t001w
*      into table tl_t001w
*       for all entries in tg_saida
*        where werks eq tg_saida-werks.

    SELECT matnr maktx
      FROM makt
      INTO TABLE tl_makt
       FOR ALL ENTRIES IN tg_saida
       WHERE matnr EQ tg_saida-matnr
         AND spras EQ sy-langu.

    SELECT matnr matkl meins
      FROM mara
      INTO TABLE tl_mara
       FOR ALL ENTRIES IN tg_saida
       WHERE matnr EQ tg_saida-matnr.

    SELECT *
      FROM marm
      INTO TABLE tl_marm
       FOR ALL ENTRIES IN tg_saida
       WHERE matnr EQ tg_saida-matnr.

    SELECT *
      FROM t006
      INTO TABLE tl_t006
       FOR ALL ENTRIES IN tg_saida
       WHERE msehi EQ tg_saida-meins.

    SELECT *
    FROM t001w
    INTO TABLE tl_t001w
     FOR ALL ENTRIES IN tg_saida
     WHERE werks EQ tg_saida-werks_fornec.

* Início - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura
*    SELECT *
*      FROM zsdt0038
*      INTO TABLE tl_0038
*       FOR ALL ENTRIES IN tg_saida
*        WHERE cultura EQ tg_saida-cultura.
* Fim - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura

  ENDIF.

  SORT: tl_t001w  BY werks,
        tl_makt   BY matnr,
        tl_mara   BY matnr,
        tl_t006   BY msehi,
        tl_marm   BY matnr meinh,
        tl_t001w  BY werks,
        tl_0038   BY cultura.

  LOOP AT tg_saida.
*    PERFORM get_cell TABLES Tl_cell
*                     USING 'VAL_DE'
*                           WL_LINHA.
    wl_linha = sy-tabix.
    IF tg_saida-val_de IS INITIAL.
*      move: "TEXT-E01            TO TG_MSG_RET-MSG,
*            c_tab_strip_nf-tab6 to tg_msg_ret-aba.
      MOVE: 'VAL_DE' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e01 ' VAL_DE.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_saida-val_ate IS INITIAL.
*    OR TG_SAIDA-VAL_ATE LT TG_SAIDA-VAL_DE.
      MOVE: 'VAL_ATE' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e01 ' VAL_ATE.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF tg_saida-val_ate LT tg_saida-val_de.
      MOVE: 'VAL_ATE' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e02 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_saida-dtvenc IS INITIAL.
      MOVE: 'DTVENC' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e01 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
*    if tg_saida-werks is initial.
*      move: 'WERKS' to tg_msg_ret-field,
*            'GRID1'  to tg_msg_ret-obj,
*             wl_linha to tg_msg_ret-tabix.
*
*      concatenate text-e01 ' WERKS.' ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*      append tg_msg_ret.
*      clear: tg_msg_ret.
*    else.
*      read table tl_t001w transporting no fields
*        with key werks = tg_saida-werks
*                 binary search.
*      if sy-subrc is not initial.
*        move: 'WERKS' to tg_msg_ret-field,
*              'GRID1'  to tg_msg_ret-obj,
*              wl_linha to tg_msg_ret-tabix.
*
*        concatenate text-e03 ' LINHA: ' wl_linha into  tg_msg_ret-msg.
*        append tg_msg_ret.
*        clear: tg_msg_ret.
*      endif.
*    endif.

    IF tg_saida-matnr IS INITIAL.
      MOVE: 'MATNR' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e01 ' MATNR.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_mara TRANSPORTING NO FIELDS
              WITH KEY matnr = tg_saida-matnr
                       BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        MOVE: 'MATNR' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

        CONCATENATE TEXT-e04 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF tg_saida-meins IS INITIAL.
      MOVE: 'MEINS' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e01 ' MEINS.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_marm TRANSPORTING NO FIELDS
              WITH KEY matnr = tg_saida-matnr
                       meinh = tg_saida-meins
                       BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        MOVE: 'MEINS' TO tg_msg_ret-field,
              'GRID1'  TO tg_msg_ret-obj,
              wl_linha TO tg_msg_ret-tabix.

        CONCATENATE TEXT-e05 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF tg_saida-werks_fornec IS INITIAL.
      MOVE: 'WERKS_FORNEC' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e01 ' WERKS_FORNEC.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_t001w TRANSPORTING NO FIELDS
              WITH KEY werks = tg_saida-werks_fornec
                       BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        MOVE: 'WERKS_FORNEC' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

        CONCATENATE TEXT-e07 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF tg_saida-vlr_custo IS INITIAL.
      MOVE: 'VLR_CUSTO' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e01 ' VLR_CUSTO.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_saida-perc_margem IS INITIAL.
      MOVE: 'PERC_MARGEM' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e01 ' PERC_MARGEM.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_saida-waerk IS INITIAL.
      MOVE: 'WAERK' TO tg_msg_ret-field,
            'GRID1'  TO tg_msg_ret-obj,
            wl_linha TO tg_msg_ret-tabix.

      CONCATENATE TEXT-e01 ' WAERK.' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_saida-cultura IS NOT INITIAL.
*      MOVE: 'CULTURA' TO TG_MSG_RET-FIELD,
*            'GRID1'  TO TG_MSG_RET-OBJ,
*            WL_LINHA TO TG_MSG_RET-TABIX.
*
*      CONCATENATE TEXT-E01 ' CULTURA.' ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ELSE.

* Início - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura
*      READ TABLE tl_0038 INTO wl_0038
*        WITH KEY cultura = tg_saida-cultura
*                   BINARY SEARCH.
*      IF sy-subrc IS NOT INITIAL.
*        MOVE: 'CULTURA' TO tg_msg_ret-field,
*             'GRID1'  TO tg_msg_ret-obj,
*             wl_linha TO tg_msg_ret-tabix.
*
*        CONCATENATE text-e06 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
*        APPEND tg_msg_ret.
*        CLEAR: tg_msg_ret.
*      ENDIF.
      READ TABLE tg_0038 INTO wl_0038
        WITH KEY cultura = tg_saida-cultura
                   BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        MOVE: 'CULTURA' TO tg_msg_ret-field,
             'GRID1'  TO tg_msg_ret-obj,
             wl_linha TO tg_msg_ret-tabix.
        "Cultura de Pagamento informada não existe
        CONCATENATE TEXT-e08 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ELSE.
        READ TABLE tg_0038 INTO wl_0038
         WITH KEY cultura   = tg_saida-cultura
                  pagamento = c_x
                  BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          MOVE: 'CULTURA' TO tg_msg_ret-field,
               'GRID1'  TO tg_msg_ret-obj,
               wl_linha TO tg_msg_ret-tabix.
          "Cultura informada não habilitada como Cultura de Pagamento
          CONCATENATE TEXT-e09 TEXT-e10 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      ENDIF.
* Fim - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura

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
FORM get_cell TABLES  tl_cell TYPE lvc_t_cell
              USING wl_cell
                    wl_tabix.
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
FORM build_dropdown .
  DATA: ls_dropdown TYPE lvc_s_drop,
        lt_dropdown TYPE lvc_t_drop,
        tl_0038     TYPE TABLE OF zsdt0038 WITH HEADER LINE.


  ls_dropdown-handle = '1'.

  ls_dropdown-value = gt_values-ddtext = 'FOB'.
  gt_values-domvalue_l = 'FOB'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

  ls_dropdown-value = gt_values-ddtext = 'CPT'.
  gt_values-domvalue_l = 'CPT'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

  ls_dropdown-value = gt_values-ddtext = 'CIF'.
  gt_values-domvalue_l = 'CIF'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

  ls_dropdown-value = gt_values-ddtext = 'CFR'.
  gt_values-domvalue_l = 'CFR'.
  APPEND: ls_dropdown TO lt_dropdown,
          gt_values.

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
  CALL METHOD grid1->set_drop_down_table
    EXPORTING
      it_drop_down = lt_dropdown.
ENDFORM.                    " BUILD_DROPDOWN

* Início - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_CULTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_valida_cultura.

  DATA: wl_0038 TYPE zsdt0038.

  SELECT *
    FROM zsdt0038
    INTO TABLE tg_0038.

  SORT tg_0038 BY cultura ASCENDING pagamento DESCENDING.

  IF s_cultu-low IS NOT INITIAL.
    READ TABLE  tg_0038 INTO wl_0038 WITH KEY cultura = s_cultu-low BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      "Cultura de Pagamento informada não existe
      MESSAGE i836(sd) DISPLAY LIKE 'E' WITH TEXT-e08.
      LEAVE LIST-PROCESSING.
    ELSE.
      READ TABLE  tg_0038  INTO wl_0038 WITH KEY cultura   = s_cultu-low
                                                 pagamento = 'X'
                                        BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        "Cultura informada não habilitada como Cultura de Pagamento
        MESSAGE i836(sd) DISPLAY LIKE 'E' WITH TEXT-e09 TEXT-e10.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.

  IF s_cultu-high IS NOT INITIAL.
    READ TABLE  tg_0038 INTO wl_0038 WITH KEY cultura = s_cultu-high.
    IF sy-subrc IS NOT INITIAL.
      "Cultura de Pagamento informada não existe
      MESSAGE i836(sd) DISPLAY LIKE 'E' WITH TEXT-e08.
      LEAVE LIST-PROCESSING.
    ELSE.
      READ TABLE  tg_0038 INTO wl_0038 WITH KEY cultura = s_cultu-high
                                                 pagamento = 'X'
                                        BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        "Cultura informada não habilitada como Cultura de Pagamento
        MESSAGE i836(sd) DISPLAY LIKE 'E' WITH TEXT-e09 TEXT-e10.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
* Fim - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura

START-OF-SELECTION.
  AUTHORITY-CHECK OBJECT 'M_MATE_BUK' ID 'BUKRS' FIELD s_bukrs-low.
  IF sy-subrc IS INITIAL.

    obj_excel = NEW lcl_excel( ).
* Início - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura
    PERFORM zf_valida_cultura.
* Fim - Sara Oikawa - Set/2020 - PBI 42348 - Adequação do Campo Cultura

    CALL SCREEN 100.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Usuário sem Permissão para Empresa ' s_bukrs-low.
    EXIT.
  ENDIF.

  PERFORM unlock_table.                                     "FF #171337
