*----------------------------------------------------------------------*
***INCLUDE LZGF_PESQ_INSTRU_OV_ALGOF01.
*----------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    DATA: validar_data  TYPE c,
          error_in_data TYPE c,
          ls_good       TYPE lvc_s_modi,
          lv_value      TYPE lvc_value.

    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.

    METHODS data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
    METHODS data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

  PRIVATE SECTION.
    TYPES: ddshretval_table TYPE TABLE OF ddshretval.

    METHODS: perform_semantic_checks
      IMPORTING
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                    "lcl_event_handler DEFINITION


DATA: cccontainer     TYPE REF TO cl_gui_custom_container,
      ctl_alv         TYPE REF TO cl_gui_alv_grid,
      splitter        TYPE REF TO cl_gui_splitter_container,
      cccontainera    TYPE REF TO cl_gui_container,
      it_fieldcatalog TYPE lvc_t_fcat,
      gs_variant      TYPE disvariant,
      gs_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,
      event_handler   TYPE REF TO lcl_event_handler.

DATA: splitter_html1        TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer_html1 TYPE REF TO cl_gui_container,
      html_control_html1    TYPE REF TO cl_gui_html_viewer.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.
  ENDMETHOD.                    "on_data_chaged

  METHOD data_changed_finished.

    IF e_modified IS NOT INITIAL.

      wa_stable-row = abap_true.
      wa_stable-col = abap_true.
      CALL METHOD ctl_alv->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD perform_semantic_checks.

    DATA: lc_qt_informada TYPE zde_nm_fardos.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(ls_good)
      WHERE fieldname EQ 'QT_FARDOS'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE it_ordens_venda ASSIGNING FIELD-SYMBOL(<fs_ordem>) INDEX ls_good-row_id.

      IF ls_good-value IS INITIAL.
        CONTINUE.
      ENDIF.
      MOVE lv_value TO lc_qt_informada.

      DATA(saldo) = <fs_ordem>-total_fardos - <fs_ordem>-total_fardos_cg.

      IF saldo LT lc_qt_informada.

        error_in_data = abap_true.

        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = zcx_carga=>zcx_ov_saldo_fardo-msgid
            i_msgno     = zcx_carga=>zcx_ov_saldo_fardo-msgno
            i_msgty     = 'E'
            i_msgv1     = lv_value
            i_fieldname = ls_good-fieldname
            i_row_id    = ls_good-row_id.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.                    "perform_semantic_checks

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  PESQ_ORDEM_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM filtrar_ordens_venda USING i_instrucao TYPE zde_zsded030_ranges_t
                             i_nr_ordem_venda TYPE zde_vbeln_ranges_t
                             i_vstel TYPE shp_vstel_range_t
                             i_charg TYPE zde_charg_ranges_t
                             i_status TYPE zde_zsded021_ranges_t
                             i_matnr TYPE zde_mgv_matnr_range_t
                          CHANGING p_algodao_alv_t TYPE zsdt0001ov_algodao_alv_t.

  CONSTANTS: cs_line_color_a TYPE c LENGTH 4 VALUE 'C200',
             cs_line_color_l TYPE c LENGTH 4 VALUE 'C500',
             cs_line_color_d TYPE c LENGTH 4 VALUE 'C601',
             cs_line_color_r TYPE c LENGTH 4 VALUE 'C600',
             cs_line_color_p TYPE c LENGTH 4 VALUE 'C200'.

  DATA: ln_edicao TYPE lvc_s_scol.

  ln_edicao = VALUE #( fname = 'QT_FARDOS' color = VALUE #( col = 5 int = 0 inv = 0 ) ).

  CLEAR: p_algodao_alv_t, p_algodao_alv_t[].

  SELECT it~instrucao,
         vk~auart  AS tipo_ordem,
         vi~vstel  AS local_negocio,
         vi~lgort  AS deposito,
         vi~charg  AS lote,
         vi~vbeln  AS ordem_venda,
         it~status AS status,
         CASE WHEN it~status = 'A' THEN 'Aguardando Sol. de Aprovação'
              WHEN it~status = 'L' THEN 'Liberado'
              WHEN it~status = 'D' THEN 'Deletado'
              WHEN it~status = 'R' THEN 'Reprovado'
              WHEN it~status = 'P' THEN 'Aguardando Aprovação'
         END AS ds_status,
         vi~matnr,
         vi~arktx,
         vi~volum	AS total_fardos,
         vi~voleh AS unidade,
         it~nro_sol_ov
    INTO TABLE @DATA(it_tab)
    FROM zsdt0066 AS it
   INNER JOIN vbak AS vk ON vk~vbeln EQ it~vbeln
   INNER JOIN vbap AS vi ON vi~vbeln EQ it~vbeln
   WHERE it~instrucao IN @i_instrucao
     AND it~vbeln  IN @i_nr_ordem_venda
     AND it~status IN @i_status
     AND vi~charg  IN @i_charg
     AND vi~vstel  IN @i_vstel
     AND vi~matnr  IN @i_matnr
     AND it~vbeln  NE @space.

  CHECK sy-subrc IS INITIAL.


  SELECT ov~id_carga, ov~nr_ordem_venda, ov~qt_fardos AS qt_carga
    INTO TABLE @DATA(it_carga)
    FROM zsdt0001ov AS ov
   INNER JOIN zsdt0001cg AS cg ON cg~id_carga EQ ov~id_carga
     FOR ALL ENTRIES IN @it_tab
   WHERE ov~nr_ordem_venda   EQ @it_tab-ordem_venda
     AND cg~tp_status        NE @zif_carga=>st_status_cancelada
     AND cg~tp_carga         EQ @zif_carga=>st_tp_carga_entrada_fob
     AND cg~tp_produto_carga EQ @zif_carga=>st_tp_produto_carga_algodao.

  SELECT ov~vbeln, ov~vgbel, ov~volum AS qt_remessa
    INTO TABLE @DATA(it_remessa)
    FROM lips AS ov
    FOR ALL ENTRIES IN @it_tab
   WHERE ov~vgbel EQ @it_tab-ordem_venda.

  IF it_remessa[] IS NOT INITIAL.
    SELECT ov~vbeln, ov~vgbel, ov~aubel, ov~volum AS qt_fatura
      INTO TABLE @DATA(it_fatura)
      FROM vbrp AS ov
      FOR ALL ENTRIES IN @it_remessa
     WHERE ov~aubel EQ @it_remessa-vgbel
       AND ov~vgbel EQ @it_remessa-vbeln AND draft = @space .
  ENDIF.

  SELECT vp~vbeln, lf~name1
    INTO TABLE @DATA(it_cole)
    FROM vbpa AS vp
    INNER JOIN lfa1 AS lf ON lf~lifnr EQ vp~lifnr
     FOR ALL ENTRIES IN @it_tab
   WHERE vbeln EQ @it_tab-ordem_venda
     AND parvw EQ 'PC'.

  SORT it_cole BY vbeln.

  LOOP AT it_tab INTO DATA(wa_tab).
    CLEAR: wa_ordens_venda.

    wa_ordens_venda-nr_ordem_venda  = wa_tab-ordem_venda.
    wa_ordens_venda-ds_instrucao    = wa_tab-instrucao.
    wa_ordens_venda-auart           = wa_tab-tipo_ordem.
    wa_ordens_venda-lgort           = wa_tab-deposito.
    wa_ordens_venda-status          = wa_tab-status.
    wa_ordens_venda-ds_status       = wa_tab-ds_status.
    wa_ordens_venda-matnr           = wa_tab-matnr.
    wa_ordens_venda-arktx           = wa_tab-arktx.
    wa_ordens_venda-total_fardos    = wa_tab-total_fardos.
    wa_ordens_venda-voleh           = wa_tab-unidade.
    wa_ordens_venda-total_fardos_cg = 0.
    wa_ordens_venda-total_fardos_rm = 0.
    wa_ordens_venda-total_fardos_ft = 0.
    wa_ordens_venda-qt_fardos       = 0.

    READ TABLE it_cole INTO DATA(wa_cole) WITH KEY vbeln = wa_tab-ordem_venda BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_ordens_venda-ds_ponto_coleta = wa_cole-name1.
    ENDIF.

    wa_ordens_venda-total_fardos_cg =
    REDUCE zde_total_fardos_carga(
      INIT i TYPE zde_total_fardos_carga
        FOR tb IN it_carga WHERE ( nr_ordem_venda EQ wa_tab-ordem_venda )
          NEXT i = i + tb-qt_carga ).

    wa_ordens_venda-total_fardos_rm =
    REDUCE zde_total_fardos_remessa(
      INIT i TYPE zde_total_fardos_remessa
        FOR rm IN it_remessa WHERE ( vgbel EQ wa_tab-ordem_venda )
          NEXT i = i + rm-qt_remessa ).

    wa_ordens_venda-total_fardos_ft =
      REDUCE zde_total_fardos_faturado(
        INIT i TYPE zde_total_fardos_faturado
          FOR ft IN it_fatura WHERE ( aubel EQ wa_tab-ordem_venda )
            NEXT i = i + ft-qt_fatura ).

    DATA(qtd_fardos) = wa_tab-total_fardos - wa_ordens_venda-total_fardos_cg.
    IF qtd_fardos GT 0.
      wa_ordens_venda-qt_fardos = qtd_fardos.
    ENDIF.

    CASE wa_tab-status.
      WHEN 'A'.
        wa_ordens_venda-line_color = cs_line_color_a.
      WHEN 'L'.
        wa_ordens_venda-line_color = cs_line_color_l.
      WHEN 'D'.
        wa_ordens_venda-line_color = cs_line_color_d.
      WHEN 'R'.
        wa_ordens_venda-line_color = cs_line_color_r.
      WHEN 'P'.
        wa_ordens_venda-line_color = cs_line_color_p.
    ENDCASE.

    APPEND ln_edicao TO wa_ordens_venda-color_cell.

    APPEND wa_ordens_venda TO p_algodao_alv_t.

  ENDLOOP.

ENDFORM.


FORM pesq_ordem_venda  USING i_fundo TYPE char01
                             i_vstel TYPE vstel
                             i_charg TYPE charg_d
                    CHANGING p_algodao_alv TYPE zsdt0001ov_algodao_alv_t.

  lc_vstel = i_vstel.
  lc_charg = i_charg.

  CLEAR: lc_selecionou.

  IF i_fundo EQ abap_true.

    ok_code = 'OPEN'.
    CALL SCREEN 0102.

    IF lc_selecionou EQ abap_true.
      p_algodao_alv[] = it_ordens_selec[].
    ENDIF.

  ELSE.

    CALL SCREEN 0100 STARTING AT 5 3.

    IF lc_selecionou EQ abap_true.
      p_algodao_alv[] = it_ordens_selec[].
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF cccontainer IS INITIAL.

    CREATE OBJECT cccontainer
      EXPORTING
        container_name = 'OB_ALV'.

    CREATE OBJECT splitter
      EXPORTING
        parent  = cccontainer
        rows    = 1
        columns = 1.

    cccontainera = splitter->get_container( row = 1 column = 1 ).

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = cccontainera.

    PERFORM fill_it_fieldcatalog.

    PERFORM fill_gs_variant.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_ordens_venda[].

    CALL METHOD ctl_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv.
    SET HANDLER event_handler->handle_double_click  FOR ctl_alv.
    SET HANDLER event_handler->data_changed FOR ctl_alv.
    SET HANDLER event_handler->data_changed_finished FOR ctl_alv.

  ENDIF.

  wa_stable-row = abap_true.
  wa_stable-col = abap_true.

  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable      = wa_stable
      i_soft_refresh = abap_true.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  CLEAR: wa_ordens_venda, lc_selecionou.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'PESQ'.
      CLEAR: ok_code.
      PERFORM pesquisa_ordem_vendas.
    WHEN 'SELE'.
      CLEAR: ok_code.
      PERFORM seleciona_ordem_vendas.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_ORDEM_VENDAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pesquisa_ordem_vendas .

  DATA:
    i_instr TYPE zde_zsded030_ranges_t,
    i_ordem TYPE zde_vbeln_ranges_t,
    i_matnr TYPE zde_mgv_matnr_range_t,
    i_vstel TYPE shp_vstel_range_t,
    i_charg TYPE zde_charg_ranges_t,
    i_statu TYPE zde_zsded021_ranges_t.

  "Instrução
  i_instr = edsinstr[].

  "Ordem de venda
  i_ordem = eidordem[].

  "Material
  i_matnr = eidmatnr[].

  "Local de expedição/local de recebimento de mercadoria
  i_vstel = VALUE #( sign = 'I' option = 'EQ' ( low = lc_vstel high = lc_vstel ) ).

  "Depósito
  i_charg = VALUE #( sign = 'I' option = 'EQ' ( low = lc_charg high = lc_charg ) ).

  PERFORM filtrar_ordens_venda USING i_instr i_ordem i_vstel i_charg i_statu i_matnr CHANGING it_ordens_venda.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSDT0001OV_ALGODAO_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.


  LOOP AT it_fieldcatalog ASSIGNING <fs_cat>.

    <fs_cat>-tabname = 'ZSDT0001OV_ALGODAO_ALV'.

    CASE <fs_cat>-fieldname.
      WHEN 'ID_CARGA' OR 'NM_PESO_BRUTO' OR 'NM_PESO_TARA' OR 'NM_PESO_SUBTOTAL' OR 'NR_ROMANEIO_SAI' OR 'CH_REFERENCIA_SAI' OR
           'NM_QTD_EMBALAGENS' OR 'LINE_COLOR' OR 'COLOR_CELL' OR 'STYLE' OR 'ID_ORDEM' OR 'NM_PESO_EMBALAGEM' OR 'NM_PESO_LIQUIDO' OR
           'NR_ORDEM'.
        <fs_cat>-no_out = abap_true.
      WHEN 'DS_PONTO_COLETA'.
        <fs_cat>-outputlen = 20.
      WHEN 'QT_FARDOS'.
        <fs_cat>-outputlen = 15.
        <fs_cat>-do_sum  = abap_true.
        <fs_cat>-edit    = abap_true.
      WHEN 'TOTAL_FARDOS' OR 'TOTAL_FARDOS_CG' OR 'TOTAL_FARDOS_RM' OR 'TOTAL_FARDOS_FT'.
        <fs_cat>-outputlen = 15.
        <fs_cat>-do_sum  = abap_true.
      WHEN 'NR_ORDEM_VENDA'.
        <fs_cat>-hotspot = abap_true.
    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

  gs_layout-sel_mode   = 'A'.
  gs_layout-edit_mode  = 'X'.
  gs_layout-zebra      = abap_false.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-no_toolbar = abap_true.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-stylefname = 'STYLE'.
  gs_layout-ctab_fname = 'COLOR_CELL'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_ordens_venda INDEX row_id INTO wa_ordens_venda.

  CASE fieldname.
    WHEN 'NR_ORDEM_VENDA'.
      zcl_ordem_venda=>zif_ordem_venda~open( wa_ordens_venda-nr_ordem_venda ).
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click  USING p_row TYPE lvc_s_row.

  DATA: lc_row TYPE lvc_t_row.

  CHECK p_row-index IS NOT INITIAL.

  IF p_row-rowtype IS INITIAL.

    APPEND p_row TO lc_row.

    CALL METHOD ctl_alv->set_selected_rows
      EXPORTING
        it_index_rows = lc_row.

    PERFORM seleciona_ordem_vendas.

  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_ORDEM_VENDAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_ordem_vendas .

  ctl_alv->get_selected_rows( IMPORTING et_index_rows = DATA(et_index_rows) ).

  IF et_index_rows[] IS INITIAL.
    MESSAGE 'Deve ser selecionado no mínimo uma ordem de venda' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: it_ordens_selec, it_ordens_selec[].

  LOOP AT et_index_rows INTO DATA(wa_row).
    READ TABLE it_ordens_venda INDEX wa_row-index INTO wa_ordens_venda.
    APPEND wa_ordens_venda TO it_ordens_selec.
  ENDLOOP.

  lc_selecionou = abap_true.
  LEAVE TO SCREEN 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.

*  SET PF-STATUS 'xxxxxxxx'.
  SET TITLEBAR 'TL0102'.

  SUPPRESS DIALOG.

  IF splitter_html1 IS INITIAL.

    CREATE OBJECT splitter_html1
      EXPORTING
        parent  = cl_gui_container=>screen0 "CTL_CCCONTAINER
        rows    = 1
        columns = 1.

    CALL METHOD splitter_html1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = ctl_cccontainer_html1.

    CREATE OBJECT html_control_html1
      EXPORTING
        parent = ctl_cccontainer_html1.

    DATA: data_table TYPE STANDARD TABLE OF text255,
          i_url      TYPE c LENGTH 200.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = zcl_util=>get_html_fundo( )
        i_tabline_length = 255
      TABLES
        et_table         = data_table.

    html_control_html1->load_data(
      IMPORTING
        assigned_url           = i_url
      CHANGING
        data_table             = data_table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5
    ).

    html_control_html1->show_url(
      EXPORTING
        url                    = i_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5
    ).
  ENDIF.

  LEAVE TO LIST-PROCESSING.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.

  CALL SCREEN 0100 STARTING AT 5 3.

  LEAVE TO SCREEN 0.

ENDMODULE.
