*&---------------------------------------------------------------------*
*& Report  ZGLT067
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zglt067 MESSAGE-ID zfi.

TABLES: zsaldo_cta_moeda, bsik, sscrfields.

TYPES: BEGIN OF ty_zde_mov_fornecedor.
         INCLUDE STRUCTURE zde_mov_fornecedor.
TYPES:  style TYPE lvc_t_styl,
       END OF ty_zde_mov_fornecedor.

DATA: functxt    TYPE smp_dyntxt,
      psaknr_aux TYPE fagl_range_t_racct.

DATA: tl_parametros TYPE ustyp_t_parameters,
      wl_parametros TYPE ustyp_parameters.

DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      c_alv_toolbarma_0308 TYPE REF TO cl_alv_grid_toolbar_manager,
      it_mov_cta_banco     TYPE TABLE OF zde_mov_cta_banco WITH HEADER LINE,
      it_zgl_saldo         TYPE TABLE OF zglt101_saldo WITH HEADER LINE,
      wa_mov_cta_banco     TYPE zde_mov_cta_banco,
      ck_alteracao         TYPE c LENGTH 1,
      ck_tela_baixa        TYPE c LENGTH 1,
      copia                TYPE zde_saldo_cta_banco,
      vg_comp              TYPE c,
      vg_text_008          TYPE char20,
      vg_text_009          TYPE char20,
      vg_text_010          TYPE char20.

DATA: wa_conta_data      LIKE zde_cab_extrato_banco,
      lc_arquivo_extrato TYPE c LENGTH 1.

CONSTANTS: ck_varios_cliente TYPE c LENGTH 1 VALUE abap_false.

INCLUDE zglt067_0002.

INCLUDE zglt067_0003.

INCLUDE zglt067_0304.

DATA: it_bsxs  TYPE TABLE OF bsis WITH HEADER LINE,
      it_bsxs2 TYPE TABLE OF bsis WITH HEADER LINE,
      it_bsxk  TYPE TABLE OF bsik WITH HEADER LINE,
      it_bsxd  TYPE TABLE OF bsid WITH HEADER LINE,
      it_bseg  TYPE TABLE OF bseg WITH HEADER LINE,
      it_bkpf  TYPE TABLE OF bkpf WITH HEADER LINE.

***********************************************************************
* LOCAL CLASSES
***********************************************************************
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

  ENDMETHOD.                    "on_toolbar
  "
  METHOD handle_user_command.

    FIELD-SYMBOLS: <fsmov> TYPE zde_mov_cta_banco.

    CASE e_ucomm.
      WHEN 'MARCAR'.
        LOOP AT it_mov_cta_banco ASSIGNING <fsmov>.
          <fsmov>-ck_marcado = 'X'.
        ENDLOOP.
        PERFORM calcular_saldo.
        CLEAR sy-ucomm.
        LEAVE TO SCREEN 0001.
      WHEN 'DESMARCAR'.
        LOOP AT it_mov_cta_banco ASSIGNING <fsmov>.
          <fsmov>-ck_marcado = ' '.
        ENDLOOP.
        PERFORM calcular_saldo.
        LEAVE TO SCREEN 0001.
        CLEAR sy-ucomm.
      WHEN 'LOCK'.
        PERFORM gravar_dados USING abap_false.
        PERFORM busca_dados.
        MESSAGE s027.
        CLEAR sy-ucomm.
        LEAVE TO SCREEN 0001.
      WHEN 'UNLOCK'.
        PERFORM excluir_dados.
        PERFORM busca_dados.
        MESSAGE s028.
        CLEAR sy-ucomm.
        LEAVE TO SCREEN 0001.
    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS. "lcl_alv_toolbar IMPLEMENTATION

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler  TYPE REF TO lcl_event_handler.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD data_changed_finished.
    PERFORM data_changed_finished USING e_modified et_good_cells.
  ENDMETHOD.                    "DATA_CHANGED_FINISHED
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_ALV
*&---------------------------------------------------------------------*
FORM user_command_alv  USING p_ucomm TYPE sy-ucomm.

ENDFORM.                    " USER_COMMAND_ALV

*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed_finished  USING  e_modified    TYPE char01
                                   et_good_cells TYPE lvc_t_modi.
  CHECK e_modified IS NOT INITIAL.
  PERFORM calcular_saldo.
  LEAVE TO SCREEN 0001.
ENDFORM.                    " DATA_CHANGED_FINISHED


DATA: ctl_alv_0102       TYPE REF TO cl_gui_alv_grid,
      ctl_con_0102       TYPE REF TO cl_gui_custom_container,
      gs_lay_0102        TYPE lvc_s_layo,
      gs_var_0102        TYPE disvariant,
      gs_scroll_col_0102 TYPE lvc_s_col,
      gs_scroll_row_0102 TYPE lvc_s_roid,
      it_catalog_0102    TYPE lvc_t_fcat,
      obg_toolbar_0102   TYPE REF TO lcl_alv_toolbar.

DATA: it_selected_0102 TYPE lvc_t_row,
      wa_selected_0102 TYPE lvc_s_row.

DATA: it_exclude_0102 TYPE ui_functions,
      wa_exclude_0102 LIKE LINE OF it_exclude_0102.

CONSTANTS: tl_0101 TYPE sydynnr VALUE '0101',
           tl_0301 TYPE sydynnr VALUE '0301',
           tl_0302 TYPE sydynnr VALUE '0302',
           tl_0307 TYPE sydynnr VALUE '0307',
           tl_0102 TYPE sydynnr VALUE '0102'.

SELECTION-SCREEN BEGIN OF BLOCK z102 WITH FRAME.
  PARAMETERS : pbukrs TYPE skb1-bukrs OBLIGATORY, " DEFAULT '0202',
               pdata  TYPE zde_dt_ref_banco OBLIGATORY DEFAULT sy-datum.
  SELECT-OPTIONS: pacont FOR zsaldo_cta_moeda-saknr.
SELECTION-SCREEN END OF BLOCK z102.

SELECTION-SCREEN: FUNCTION KEY 1.

INITIALIZATION.
  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = tl_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
  ENDIF.

  READ TABLE tl_parametros INTO wl_parametros
   WITH KEY parid = 'BUK'.

  IF sy-subrc = 0.
    pbukrs = wl_parametros-parva.
  ENDIF.

  functxt-icon_id   = icon_import.
  functxt-quickinfo = 'Importar Arquivo'.
  functxt-icon_text = 'Importar'.
  sscrfields-functxt_01 = functxt.

AT SELECTION-SCREEN.

  DATA: it_file_table	TYPE filetable,
        wa_filename   TYPE file_table,
        lc_rc	        TYPE i.


  CASE sscrfields-ucomm.
    WHEN 'FC01'.

      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          window_title            = 'Importar Arquivo de Banco - Padrão MT940'
          file_filter             = 'Files TXT (*.TXT)|*.TXT|'
          multiselection          = abap_false
          initial_directory       = 'C:\Maggi'
        CHANGING
          file_table              = it_file_table
          rc                      = lc_rc
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      READ TABLE it_file_table INDEX 1 INTO wa_filename.

      PERFORM leitura_arquivo USING wa_filename sy-subrc.

  ENDCASE.

START-OF-SELECTION.

  CLEAR: psaknr_aux.

  LOOP AT pacont.
    APPEND pacont TO psaknr_aux.
  ENDLOOP.

  p_sel_data = pdata.
  PERFORM popula_contas USING pbukrs pdata psaknr_aux.

  IF it_selecao[] IS INITIAL.
    MESSAGE s031.
  ENDIF.

END-OF-SELECTION.

  IF it_selecao[] IS NOT INITIAL.
    CALL SCREEN 0002.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  CALCULAR_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM calcular_saldo .

  zde_saldo_cta_banco-saldofinal = zde_saldo_cta_banco-saldoda + zde_saldo_cta_banco-saldod.
  "Diferênça
  zde_saldo_cta_banco-saldodif   = ( zde_saldo_cta_banco-saldofinal * -1 ) + zde_saldo_cta_banco-saldoex.

ENDFORM.                    " CALCULAR_SALDO


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0001_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0001_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  DATA: it_ucomm TYPE TABLE OF sy-ucomm.

  CLEAR: it_ucomm.

  IF ck_alteracao EQ abap_false.
    APPEND 'SAVE' TO it_ucomm.
  ENDIF.

  IF wa_conta_data IS INITIAL.
    APPEND 'ARQUIVO' TO it_ucomm.
  ENDIF.

  APPEND 'BAIXAR_DOC' TO it_ucomm.

  SET PF-STATUS 'PF0001' EXCLUDING it_ucomm.
  SET TITLEBAR 'TL0001' WITH zde_saldo_cta_banco-budat.

  SET PARAMETER ID: 'BUK' FIELD zde_saldo_cta_banco-bukrs.

  PERFORM calcular_saldo.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.

  DATA: lc_text_date TYPE c LENGTH 10.

  IF ctl_con_0102 IS INITIAL.

    CREATE OBJECT ctl_con_0102
      EXPORTING
        container_name = 'ALV_DOCS'.

    CREATE OBJECT ctl_alv_0102
      EXPORTING
        i_parent = ctl_con_0102.

    CREATE OBJECT obg_toolbar_0102
      EXPORTING
        io_alv_grid = ctl_alv_0102.

    "    WL_DESACTIVE = TRUE.

*    IF CK_AUTORIZACAO_09 EQ TRUE.
*      WL_DESACTIVE = FALSE.
*    ENDIF.

    SET HANDLER obg_toolbar_0102->on_toolbar FOR ctl_alv_0102.
    SET HANDLER obg_toolbar_0102->handle_user_command FOR ctl_alv_0102.

    PERFORM fill_it_fieldcatalog_0102.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0102.
*   Set layout parameters for ALV grid

    gs_lay_0102-zebra      = 'X'.

*    WRITE pdata TO lc_text_date.
*
*    CONCATENATE TEXT-003 '-' LC_TEXT_DATE INTO GS_LAY_0102-GRID_TITLE SEPARATED BY SPACE.

*    CLEAR gs_lay_0102-grid_title.

    APPEND '&LOCAL&CUT'           TO it_exclude_0102.
    APPEND '&LOCAL&INSERT_ROW'    TO it_exclude_0102.
    APPEND '&LOCAL&MOVE_ROW'      TO it_exclude_0102.
    APPEND '&LOCAL&PASTE'         TO it_exclude_0102.
    APPEND '&LOCAL&PASTE_NEW_ROW' TO it_exclude_0102.
    APPEND '&LOCAL&UNDO'          TO it_exclude_0102.
    APPEND '&VARI_ADMIN'          TO it_exclude_0102.
    APPEND '&LOCAL&APPEND'        TO it_exclude_0102.
    APPEND '&LOCAL&COPY'          TO it_exclude_0102.
    APPEND '&LOCAL&COPY_ROW'      TO it_exclude_0102.
    APPEND '&VLOTUS'              TO it_exclude_0102.
    APPEND '&AQW'                 TO it_exclude_0102.
    APPEND '&PRINT'               TO it_exclude_0102.
    APPEND '&MB_SUM'              TO it_exclude_0102.
    APPEND '&AVERAGE'             TO it_exclude_0102.
    APPEND '&MB_VIEW'             TO it_exclude_0102.
    APPEND '&MB_EXPORT'           TO it_exclude_0102.
    APPEND '&MB_FILTER'           TO it_exclude_0102.
    APPEND '&GRAPH'               TO it_exclude_0102.
    APPEND '&INFO'                TO it_exclude_0102.
    APPEND '&LOCAL&DELETE_ROW'    TO it_exclude_0102.
    APPEND '&CHECK'               TO it_exclude_0102.

    CALL METHOD ctl_alv_0102->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0102
        is_variant           = gs_var_0102
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0102
      CHANGING
        it_fieldcatalog      = it_catalog_0102        "IT_EXCEPT_QINFO = IT_HINTS
        it_outtab            = it_mov_cta_banco[].

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->data_changed_finished FOR ctl_alv_0102.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv_0102.

    CALL METHOD ctl_alv_0102->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_0102->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0102->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0102
      es_row_no   = gs_scroll_row_0102.

ENDMODULE.                 " STATUS_0102  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  GERAR_MOV_CONTA_BANCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_mov_conta_banco .

  DATA: it_kna1 TYPE TABLE OF kna1 WITH HEADER LINE,
        it_lfa1 TYPE TABLE OF lfa1 WITH HEADER LINE,
        e_x001  LIKE x001.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = zde_saldo_cta_banco-bukrs
    IMPORTING
      e_x001  = e_x001.

  LOOP AT it_bseg.
    CASE it_bseg-koart.
      WHEN 'K'.
        it_bsxk-gjahr = it_bseg-gjahr.
        it_bsxk-belnr	= it_bseg-belnr.
        it_bsxk-buzei = it_bseg-buzei.
        it_bsxk-lifnr = it_bseg-lifnr.
        APPEND it_bsxk.
      WHEN 'D'.
        it_bsxd-gjahr = it_bseg-gjahr.
        it_bsxd-belnr	= it_bseg-belnr.
        it_bsxd-buzei = it_bseg-buzei.
        it_bsxd-kunnr = it_bseg-kunnr.
        APPEND it_bsxd.
    ENDCASE.
  ENDLOOP.

  SORT it_bsxk BY gjahr belnr	buzei.
  SORT it_bsxd BY gjahr belnr	buzei.

  "Forncedor
  IF it_bsxk[] IS NOT INITIAL.
    SELECT * INTO TABLE it_lfa1
      FROM lfa1
       FOR ALL ENTRIES IN it_bsxk
     WHERE lifnr EQ it_bsxk-lifnr.

    SORT it_lfa1 BY lifnr.
  ENDIF.

  "Forncedor
  IF it_bsxd[] IS NOT INITIAL.
    SELECT * INTO TABLE it_kna1
      FROM kna1
       FOR ALL ENTRIES IN it_bsxd
     WHERE kunnr EQ it_bsxd-kunnr.

    SORT it_kna1 BY kunnr.
  ENDIF.

  CLEAR: it_mov_cta_banco[].

  "
  LOOP AT it_bsxs2.

    CLEAR: it_mov_cta_banco.

    it_mov_cta_banco-budat = it_bsxs2-budat.
    it_mov_cta_banco-bldat = it_bsxs2-bldat.
    it_mov_cta_banco-gjahr = it_bsxs2-gjahr.
    it_mov_cta_banco-belnr = it_bsxs2-belnr.
    it_mov_cta_banco-buzei = it_bsxs2-buzei.
    it_mov_cta_banco-xblnr = it_bsxs2-xblnr.
    it_mov_cta_banco-waers = it_bsxs2-waers.
    it_mov_cta_banco-shkzg = it_bsxs2-shkzg.
*---> 05/07/2023 - Migração S4 - DL
    SORT it_bkpf BY gjahr belnr.
    SORT it_bseg BY gjahr belnr.
*<--- 05/07/2023 - Migração S4 - DL
    READ TABLE it_bkpf WITH KEY gjahr = it_bsxs2-gjahr
                                belnr	= it_bsxs2-belnr BINARY SEARCH.
    IF ( sy-subrc IS INITIAL ) AND ( it_bkpf-stblg IS NOT INITIAL ).
      it_mov_cta_banco-xstov = 'X'.
      it_mov_cta_banco-stblg = it_bkpf-stblg.
      it_mov_cta_banco-stjah = it_bkpf-stjah.
    ENDIF.

    it_mov_cta_banco-wrbtr = it_bsxs2-wrbtr.
    it_mov_cta_banco-sgtxt = it_bsxs2-sgtxt.

    READ TABLE it_bseg WITH KEY gjahr = it_bsxs2-gjahr
                                belnr	= it_bsxs2-belnr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CASE it_bseg-koart.
        WHEN 'K'.
          it_mov_cta_banco-lifnr = it_bseg-lifnr.
          it_mov_cta_banco-parid = it_bseg-lifnr.
          READ TABLE it_lfa1 WITH KEY lifnr = it_bseg-lifnr BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            it_mov_cta_banco-name1 = it_lfa1-name1.
          ENDIF.
        WHEN 'D'.
          it_mov_cta_banco-kunnr = it_bseg-kunnr.
          it_mov_cta_banco-parid = it_bseg-kunnr.
          READ TABLE it_kna1 WITH KEY kunnr = it_bseg-kunnr BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            it_mov_cta_banco-name1 = it_kna1-name1.
          ENDIF.
      ENDCASE.
    ENDIF.

    CASE it_bsxs2-shkzg.
      WHEN 'H'.
        IF wa_t001-waers EQ zde_saldo_cta_banco-waers.
*---> 10/06/2023 - Migração S4 - JS
*          "Moeda Empresa - Montante em moeda interna
*          it_mov_cta_banco-dmbtrh = it_bsxs2-dmbtr.
*        ELSEIF e_x001-hwae2 EQ zde_saldo_cta_banco-waers.
*          "Moeda Forte   - Montante na 2ª moeda interna
*          it_mov_cta_banco-dmbtrh = it_bsxs2-dmbe2.
*        ELSEIF e_x001-hwae3 EQ zde_saldo_cta_banco-waers.
*          "Moeda Grupo   - Montante na 3ª moeda interna
*          it_mov_cta_banco-dmbtrh = it_bsxs2-dmbe3.
*        ELSE.
*          it_mov_cta_banco-dmbtrh = it_bsxs2-wrbtr.

          "Moeda Empresa - Montante em moeda interna
          it_mov_cta_banco-dmbtrh = CONV #( it_bsxs2-dmbtr ).
        ELSEIF e_x001-hwae2 EQ zde_saldo_cta_banco-waers.
          "Moeda Forte   - Montante na 2ª moeda interna
          it_mov_cta_banco-dmbtrh = CONV #( it_bsxs2-dmbe2 ).
        ELSEIF e_x001-hwae3 EQ zde_saldo_cta_banco-waers.
          "Moeda Grupo   - Montante na 3ª moeda interna
          it_mov_cta_banco-dmbtrh = CONV #( it_bsxs2-dmbe3 ).
        ELSE.
          it_mov_cta_banco-dmbtrh = CONV #( it_bsxs2-wrbtr ).
*<--- 10/06/2023 - Migração S4 - JS
        ENDIF.
      WHEN 'S'.
        IF wa_t001-waers EQ zde_saldo_cta_banco-waers.
*---> 10/06/2023 - Migração S4 - JS
*          "Moeda Empresa - Montante em moeda interna
*          it_mov_cta_banco-dmbtrs = it_bsxs2-dmbtr.
*        ELSEIF e_x001-hwae2 EQ zde_saldo_cta_banco-waers.
*          "Moeda Forte   - Montante na 2ª moeda interna
*          it_mov_cta_banco-dmbtrs = it_bsxs2-dmbe2.
*        ELSEIF e_x001-hwae3 EQ zde_saldo_cta_banco-waers.
*          "Moeda Grupo   - Montante na 3ª moeda interna
*          it_mov_cta_banco-dmbtrs = it_bsxs2-dmbe3.
*        ELSE.
*          it_mov_cta_banco-dmbtrs = it_bsxs2-wrbtr.
          "Moeda Empresa - Montante em moeda interna
          it_mov_cta_banco-dmbtrs = CONV #( it_bsxs2-dmbtr ).
        ELSEIF e_x001-hwae2 EQ zde_saldo_cta_banco-waers.
          "Moeda Forte   - Montante na 2ª moeda interna
          it_mov_cta_banco-dmbtrs = CONV #( it_bsxs2-dmbe2 ).
        ELSEIF e_x001-hwae3 EQ zde_saldo_cta_banco-waers.
          "Moeda Grupo   - Montante na 3ª moeda interna
          it_mov_cta_banco-dmbtrs = CONV #( it_bsxs2-dmbe3 ).
        ELSE.
          it_mov_cta_banco-dmbtrs = CONV #( it_bsxs2-wrbtr ).
*<--- 10/06/2023 - Migração S4 - JS
        ENDIF.
    ENDCASE.

    APPEND it_mov_cta_banco.

  ENDLOOP.

ENDFORM.                    " GERAR_MOV_CONTA_BANCO

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0102 .

  gs_var_0102-report      = sy-repid.
  gs_var_0102-handle      = '0102'.
  gs_var_0102-log_group   = abap_false.
  gs_var_0102-username    = abap_false.
  gs_var_0102-variant     = abap_false.
  gs_var_0102-text        = abap_false.
  gs_var_0102-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0102

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0102 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0102> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_MOV_CTA_BANCO'
    CHANGING
      ct_fieldcat      = it_catalog_0102.

  lc_col_pos = 1.

  LOOP AT it_catalog_0102 ASSIGNING <fs_cat_0102>.
    <fs_cat_0102>-col_pos = lc_col_pos.
    <fs_cat_0102>-tabname = 'IT_MOV_CTA_BANCO'.
    ADD 1 TO lc_col_pos.
    CASE <fs_cat_0102>-fieldname.
      WHEN 'CK_MARCADO'.

      WHEN 'BELNR' OR 'STBLG'.
        <fs_cat_0102>-hotspot = 'X'.
      WHEN 'DMBTRH' OR 'DMBTRS' OR 'WRBTR'.
        <fs_cat_0102>-do_sum    = 'X'.
        <fs_cat_0102>-outputlen = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0102
FORM input_data CHANGING p_data.

  DATA: tab_pop TYPE TABLE OF sval,
        wa_pop  TYPE sval,
        input   TYPE string.

  wa_pop-tabname   = 'BKPF'.
  wa_pop-fieldname = 'BUDAT'.
  wa_pop-value     = p_data.
  APPEND wa_pop TO tab_pop.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-017
*     START_COLUMN    = '1'
*     START_ROW       = '1'
    TABLES
      fields          = tab_pop
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  READ TABLE tab_pop INTO wa_pop INDEX 1.
  IF sy-subrc = 0.
    p_data = wa_pop-value.   " Contains input value entered
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_dados .

  DATA: vg_mes_pos        TYPE i,
        vg_saldo_mi       TYPE hslvt12,
        refe1             TYPE hslvt12,
        lc_ryear          TYPE gjahr,
        it_saldos         TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
        it_saldos_2       TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
        it_saldos_3       TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
        wa_t001           TYPE t001,
        wa_moedas_empresa TYPE x001.

  DATA: wa_contas   TYPE zlc_emp_contas,
        lc_pdata    TYPE sydatum,
        lc_pdata_01 TYPE sydatum,
        lc_pdata_30 TYPE sydatum,
        it_contas   TYPE zct_emp_contas.

  RANGES: it_koart FOR bseg-koart.

  CLEAR: it_bseg[].

  SELECT SINGLE * INTO wa_t001
    FROM t001
   WHERE bukrs EQ zde_saldo_cta_banco-bukrs.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs = wa_t001-bukrs
    IMPORTING
      e_x001  = wa_moedas_empresa.

  wa_contas-bukrs = zde_saldo_cta_banco-bukrs.
  wa_contas-saknr = zde_saldo_cta_banco-saknr.
  APPEND wa_contas TO it_contas.

  IF ( zde_saldo_cta_banco-waers EQ wa_t001-waers OR
       zde_saldo_cta_banco-waers EQ wa_moedas_empresa-hwae2 OR
       zde_saldo_cta_banco-waers EQ wa_moedas_empresa-hwae3 ).

    CALL FUNCTION 'OIUREP_MONTH_FIRST_LAST'
      EXPORTING
        i_date      = zde_saldo_cta_banco-budat
      IMPORTING
        e_first_day = lc_pdata_01
        e_last_day  = lc_pdata_30
      EXCEPTIONS
        wrong_date  = 1
        OTHERS      = 2.

    lc_pdata = lc_pdata_01 - 1.

    lc_ryear = lc_pdata(4).

    "Saldo do Mês Anterior
    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear         = lc_ryear
        contas        = it_contas
        p_gerar_todas = 'X'
      TABLES
        it_saldos     = it_saldos
        it_saldos_2   = it_saldos_2
        it_saldos_3   = it_saldos_3
      EXCEPTIONS
        moeda_nao_adm = 1
        erro_ledger   = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lc_pdata = zde_saldo_cta_banco-budat - 1.

    CLEAR: it_saldo_contas[].

    IF lc_pdata_01 NE zde_saldo_cta_banco-budat.
      "Saldo do 1º dia do mês até dia atual - 1
      CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
        EXPORTING
          p_dt_inicial    = lc_pdata_01
          p_dt_posicao    = lc_pdata
          contas          = it_contas
          waers           = zde_saldo_cta_banco-waers
        TABLES
          it_saldo_contas = it_saldo_contas.
    ENDIF.

    CLEAR: it_saldo_contas2[].

    CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
      EXPORTING
        p_dt_inicial    = zde_saldo_cta_banco-budat
        p_dt_posicao    = zde_saldo_cta_banco-budat
        contas          = it_contas
        waers           = zde_saldo_cta_banco-waers
        p_dt_eq         = 'X'
      TABLES
        it_bsxs         = it_bsxs2
        it_bsxk         = it_bsxk
        it_bsxd         = it_bsxd
        it_saldo_contas = it_saldo_contas2.

  ELSE.

    lc_pdata = zde_saldo_cta_banco-budat - 1.

    CLEAR: it_saldo_contas[].

    CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
      EXPORTING
        p_dt_posicao    = lc_pdata
        contas          = it_contas
        waers           = zde_saldo_cta_banco-waers
      TABLES
        it_saldo_contas = it_saldo_contas.

    CLEAR: it_saldo_contas2[].

    CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
      EXPORTING
        p_dt_inicial    = zde_saldo_cta_banco-budat
        p_dt_posicao    = zde_saldo_cta_banco-budat
        contas          = it_contas
        waers           = zde_saldo_cta_banco-waers
        p_dt_eq         = 'X'
      TABLES
        it_bsxs         = it_bsxs2
        it_bsxk         = it_bsxk
        it_bsxd         = it_bsxd
        it_saldo_contas = it_saldo_contas2.

  ENDIF.

  IF it_bsxs2[] IS NOT INITIAL.

    it_koart-sign   = 'I'.
    it_koart-option = 'EQ'.
    it_koart-low    = 'D'.
    it_koart-high   = 'D'.
    APPEND it_koart.


    it_koart-low    = 'K'.
    it_koart-high   = 'K'.
    APPEND it_koart.
* ---> S4 Migration - 15/06/2023 - MA
*    SELECT * INTO TABLE it_bseg
*      FROM bseg
*       FOR ALL ENTRIES IN it_bsxs2
*     WHERE bukrs EQ it_bsxs2-bukrs
*       AND belnr EQ it_bsxs2-belnr
*       AND gjahr EQ it_bsxs2-gjahr
*       AND koart IN it_koart.

    DATA lt_fields TYPE fagl_t_field.
    DATA: lt_bseg TYPE TABLE OF bseg,
          t_bseg  TYPE TABLE OF bseg.





    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
*---> 28/08/2023 - Ticket MG-6425 - GT - Início
*        it_for_all_entries = it_bsxs2
        it_for_all_entries = it_bsxs2[]
*<--- 28/08/2023 - Ticket MG-6425 - GT - Fim
        i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
*       IT_FIELDLIST       = LT_FIELDS
      IMPORTING
        et_bseg            = lt_bseg
      EXCEPTIONS
        not_found          = 1.

    DELETE lt_bseg WHERE koart NOT IN it_koart.

    IF sy-subrc = 0 AND lines( lt_bseg ) > 0.
      MOVE-CORRESPONDING lt_bseg TO it_bseg[].
      sy-dbcnt = lines( lt_bseg ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.
* <--- S4 Migration - 15/06/2023 - MA

    SORT it_bsxk BY gjahr belnr.

    SELECT * INTO TABLE it_bkpf
      FROM bkpf
       FOR ALL ENTRIES IN it_bsxs2
     WHERE bukrs EQ it_bsxs2-bukrs
       AND belnr EQ it_bsxs2-belnr
       AND gjahr EQ it_bsxs2-gjahr.

    SORT it_bsxk BY gjahr belnr.

  ENDIF.

  zde_saldo_cta_banco-saldoda  = 0.
  zde_saldo_cta_banco-saldod   = 0.
  zde_saldo_cta_banco-saldodif = 0.

  LOOP AT it_saldo_contas.
    CASE it_saldo_contas-rldnr.
      WHEN 'H'. "Credito
        zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda - it_saldo_contas-slvt.
      WHEN 'S'. "Débito
        zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda + it_saldo_contas-slvt.
    ENDCASE.
  ENDLOOP.

  "Saldo do Mês Anterior """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  lc_pdata    = lc_pdata_01 - 1.
  vg_mes_pos  = lc_pdata+4(2).

  IF vg_mes_pos EQ 12.
    vg_mes_pos = 16. "Considerar Periodos Extras 13,14,16,16
  ENDIF.

  vg_saldo_mi = 0.

  IF zde_saldo_cta_banco-waers EQ wa_t001-waers.
    "1ª Moeda da Empresa
    LOOP AT it_saldos WHERE racct EQ zde_saldo_cta_banco-saknr.
      vg_saldo_mi = it_saldos-slvt.
      DO vg_mes_pos TIMES
        VARYING refe1 FROM it_saldos-sl01 NEXT it_saldos-sl02.
        ADD refe1 TO vg_saldo_mi.
      ENDDO.
    ENDLOOP.
  ELSE.
    CASE zde_saldo_cta_banco-waers.
      WHEN wa_moedas_empresa-hwae2.
        "2ª Moeda da Empresa
        LOOP AT it_saldos_2 WHERE racct EQ zde_saldo_cta_banco-saknr.
          vg_saldo_mi = it_saldos_2-slvt.
          DO vg_mes_pos TIMES
            VARYING refe1 FROM it_saldos_2-sl01 NEXT it_saldos_2-sl02.
            ADD refe1 TO vg_saldo_mi.
          ENDDO.
        ENDLOOP.
      WHEN wa_moedas_empresa-hwae3.
        "3ª Moeda da Empresa
        LOOP AT it_saldos_3 WHERE racct EQ zde_saldo_cta_banco-saknr.
          vg_saldo_mi = it_saldos_3-slvt.
          DO vg_mes_pos TIMES
            VARYING refe1 FROM it_saldos_3-sl01 NEXT it_saldos_3-sl02.
            ADD refe1 TO vg_saldo_mi.
          ENDDO.
        ENDLOOP.
      WHEN OTHERS.
        "1ª Moeda da Empresa
        LOOP AT it_saldos WHERE racct EQ zde_saldo_cta_banco-saknr.
          vg_saldo_mi = it_saldos-slvt.
          DO vg_mes_pos TIMES
            VARYING refe1 FROM it_saldos-sl01 NEXT it_saldos-sl02.
            ADD refe1 TO vg_saldo_mi.
          ENDDO.
        ENDLOOP.
    ENDCASE.
  ENDIF.
  zde_saldo_cta_banco-saldoda = zde_saldo_cta_banco-saldoda + vg_saldo_mi.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  LOOP AT it_saldo_contas2.
    CASE it_saldo_contas2-rldnr.
      WHEN 'H'. "Credito
        zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod - it_saldo_contas2-slvt.
      WHEN 'S'. "Débito
        zde_saldo_cta_banco-saldod = zde_saldo_cta_banco-saldod + it_saldo_contas2-slvt.
    ENDCASE.
  ENDLOOP.

  zde_saldo_cta_banco-saldofinal = zde_saldo_cta_banco-saldoda + zde_saldo_cta_banco-saldod.

  PERFORM gerar_mov_conta_banco.

  PERFORM atualiza_dados_gravados.

  PERFORM calcular_saldo.

ENDFORM.                    " BUSCA_DADOS

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA: BEGIN OF ti_bdcdata OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA: END OF ti_bdcdata.

  CASE ok_code.
    WHEN 'ESTORNO'."ALRS
      DATA: it_selected_rows TYPE lvc_t_row,
            wa_selected_rows TYPE lvc_s_row.

      CALL METHOD ctl_alv_0102->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      CHECK it_selected_rows IS NOT INITIAL.

      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE it_mov_cta_banco INDEX wa_selected_rows-index.
        IF sy-subrc IS INITIAL.
          PERFORM estorno_fbra USING it_mov_cta_banco.
          PERFORM busca_dados.
          CLEAR ok_code.
          EXIT. "somente 1
        ENDIF.
      ENDLOOP.

    WHEN 'TROCAR_D'.
      p_datanova = zde_saldo_cta_banco-budat.
      DATA(_saknr) = zde_saldo_cta_banco-saknr.
      PERFORM input_data CHANGING p_datanova.
      zde_saldo_cta_banco-budat = p_datanova.
      p_sel_data = p_datanova.
      pdata      = p_datanova.
      PERFORM popula_contas USING p_sel_bukrs p_sel_data p_sel_contas.
      "
      READ TABLE it_selecao WITH KEY saknr = _saknr INTO zde_saldo_cta_banco.
      "
      PERFORM busca_dados.
      CLEAR ok_code.
    WHEN 'ATUALIZAR'.
      PERFORM busca_dados.
      CLEAR ok_code.

    WHEN 'TLOTE'.
      PERFORM gerar_lote_documentos.
      "CALL TRANSACTION 'ZGL016'.
      CLEAR ok_code.

    WHEN 'COMPENSAR'.
      "CALL TRANSACTION 'F-53'.
      ok_code = 'ATUALI'.
      MOVE zde_saldo_cta_banco TO copia.
      CALL SCREEN '0003'.
      CLEAR ok_code.

    WHEN 'BAIXAR_DOC'.

      CLEAR ok_code.

      CLEAR ti_bdcdata.
      MOVE:  'ZGLT067'       TO ti_bdcdata-program,
             '4008'	         TO ti_bdcdata-dynpro,
             'X'             TO ti_bdcdata-dynbegin.
      APPEND ti_bdcdata.

      CLEAR ti_bdcdata.
      MOVE: 'BDC_OKCODE'     TO    ti_bdcdata-fnam,
            '=CONFIRMA'      TO    ti_bdcdata-fval.
      APPEND ti_bdcdata.

      CLEAR ti_bdcdata.
      MOVE: 'BSIS-BUDAT'     TO    ti_bdcdata-fnam.
      CONCATENATE pdata+6(2)
                  pdata+4(2)
                  pdata(4) INTO ti_bdcdata-fval.
      APPEND ti_bdcdata.

      CALL TRANSACTION 'ZFIS38' USING ti_bdcdata MODE 'E'.
      PERFORM busca_dados.

    WHEN 'SAVE'.
      PERFORM gravar_dados  USING abap_false.
      CLEAR: ok_code.
    WHEN 'ARQUIVO'.
      PERFORM lancamento_extrato.

  ENDCASE.

  IF ck_alteracao EQ abap_true.
    PERFORM gravar_dados  USING abap_false.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_DADOS_GRAVADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualiza_dados_gravados .

  DATA: wa_dados TYPE zsaldo_cta_banco,
        it_dados TYPE TABLE OF zsaldo_cta_mov WITH HEADER LINE.

  FIELD-SYMBOLS: <fsmov> TYPE zde_mov_cta_banco.

  SELECT SINGLE * INTO wa_dados
    FROM zsaldo_cta_banco
   WHERE bukrs EQ zde_saldo_cta_banco-bukrs
     AND saknr EQ zde_saldo_cta_banco-saknr
     AND budat EQ zde_saldo_cta_banco-budat
     AND waers EQ zde_saldo_cta_banco-waers.

  IF sy-subrc IS INITIAL.
    zde_saldo_cta_banco-saldoex    = wa_dados-saldoex.
  ENDIF.

ENDFORM.                    " ATUALIZA_DADOS_GRAVADOS

*&---------------------------------------------------------------------*
*&      Form  GRAVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gravar_dados USING p_mensagem.

  DATA: wa_dados TYPE zsaldo_cta_banco,
        it_dados TYPE TABLE OF zsaldo_cta_mov WITH HEADER LINE.

  PERFORM calcular_saldo.

  DELETE FROM zsaldo_cta_banco
   WHERE bukrs EQ zde_saldo_cta_banco-bukrs
     AND saknr EQ zde_saldo_cta_banco-saknr
     AND budat EQ pdata
     AND waers EQ zde_saldo_cta_banco-waers.

  IF p_sel_data IS INITIAL.
    p_sel_data = pdata.
  ENDIF.

  CLEAR: wa_dados.
  wa_dados-bukrs      = zde_saldo_cta_banco-bukrs.
  wa_dados-saknr      = zde_saldo_cta_banco-saknr.
  wa_dados-budat      = p_sel_data. "pdata.
  wa_dados-waers      = zde_saldo_cta_banco-waers.
  wa_dados-saldoda    = zde_saldo_cta_banco-saldoda.
  wa_dados-saldod     = zde_saldo_cta_banco-saldod.
  wa_dados-saldoex    = zde_saldo_cta_banco-saldoex.
  wa_dados-saldodif   = zde_saldo_cta_banco-saldodif.
  wa_dados-saldofinal = zde_saldo_cta_banco-saldofinal.
  wa_dados-erdat      = sy-datum.
  wa_dados-erzet      = sy-uzeit.
  wa_dados-ernam      = sy-uname.

  MODIFY zsaldo_cta_banco FROM wa_dados.
  COMMIT WORK.

  IF p_mensagem IS INITIAL.
    MESSAGE s027.
  ENDIF.

  ck_alteracao = abap_false.

ENDFORM.                    " GRAVAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excluir_dados .

  DELETE FROM zsaldo_cta_banco
   WHERE bukrs EQ zde_saldo_cta_banco-bukrs
     AND saknr EQ zde_saldo_cta_banco-saknr
     AND budat EQ pdata
     AND waers EQ zde_saldo_cta_banco-waers.

  DELETE FROM zsaldo_cta_mov
   WHERE bukrs EQ zde_saldo_cta_banco-bukrs
     AND saknr EQ zde_saldo_cta_banco-saknr
     AND budat EQ pdata
     AND waers EQ zde_saldo_cta_banco-waers.

ENDFORM.                    " EXCLUIR_DADOS

*&---------------------------------------------------------------------*
*&      Form  CHAMA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM chama_tela .
  PERFORM busca_dados.
*  CALL SCREEN 0001.
ENDFORM.                    " CHAMA_TELA

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
           USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  IF row_id GT 0.
    READ TABLE it_mov_cta_banco INDEX row_id INTO wa_mov_cta_banco.

    CASE fieldname.
      WHEN 'BELNR'.
        PERFORM chama_tela_fb03 USING zde_saldo_cta_banco-bukrs wa_mov_cta_banco-gjahr wa_mov_cta_banco-belnr.
      WHEN 'STBLG'.
        PERFORM chama_tela_fb03 USING zde_saldo_cta_banco-bukrs wa_mov_cta_banco-stjah wa_mov_cta_banco-stblg.
    ENDCASE.
  ENDIF.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  CHAMA_TELA_FB03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM chama_tela_fb03  USING  p_bukrs TYPE zde_saldo_cta_banco-bukrs
                             p_gjahr TYPE zde_mov_cta_banco-gjahr
                             p_belnr TYPE zde_mov_cta_banco-belnr.

  CHECK p_belnr IS NOT INITIAL AND p_gjahr IS NOT INITIAL.

  SET PARAMETER ID: 'BLN' FIELD p_belnr,
                    'BUK' FIELD p_bukrs,
                    'GJR' FIELD p_gjahr.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

ENDFORM.                    " CHAMA_TELA_FB03

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_EXTRATO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_extrato INPUT.
  ck_alteracao = abap_true.
ENDMODULE.                 " ALTEROU_EXTRATO  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  LOOP AT SCREEN.
    IF ck_tela_baixa EQ abap_true.
      IF screen-name EQ 'ZDE_SALDO_CTA_BANCO-SALDOEX'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " STATUS_0101  OUTPUT

INCLUDE zglt067_leitura_arquivo.

INCLUDE zglt067_0004.

INCLUDE zglt067_5001.
