*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0103 .
*----------------------------------------------------------------------*

*       CLASS LCL_ALV_TOOLBAR_N55 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_n55 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

DATA: ctl_alv_n55          TYPE REF TO cl_gui_alv_grid,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: it_selected_n55 TYPE lvc_t_row,
      wa_selected_n55 TYPE lvc_s_row.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_N55 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_n55 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    DATA(ck_permissao) = zcl_cte_dist_g=>autorizado( EXPORTING i_cd_chave_cte = zib_cte_dist_ter-cd_chave_cte i_tp_aprovacao = '08' ).

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = text-005.
    ty_toolbar-butn_type = 0.
    IF zib_cte_dist_ter-ck_finalizado IS NOT INITIAL OR ck_permissao IS INITIAL.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Editar Nota Fiscal
    ty_toolbar-icon      = icon_change.
    ty_toolbar-function  = 'EDT'.
    ty_toolbar-quickinfo = text-019.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = abap_true.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR: ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = text-006.
    ty_toolbar-butn_type = 0.
    IF zib_cte_dist_ter-ck_finalizado IS NOT INITIAL OR ck_permissao IS INITIAL.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Rateio de Documento Eletrônico
    ty_toolbar-icon      = icon_wf_workitem_error.
    ty_toolbar-function  = 'RNFE'.
    ty_toolbar-quickinfo = text-014.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_selected_n55, it_selected_n55[].

    CALL METHOD ctl_alv_n55->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_n55.

    CLEAR: it_cte_n55_sel[].

    LOOP AT it_selected_n55 INTO wa_selected_n55.
      READ TABLE it_cte_n55 INTO wa_cte_n55 INDEX wa_selected_n55-index.
      APPEND wa_cte_n55 TO it_cte_n55_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_nfe_processo.
      WHEN 'EDT'.
        "PERFORM EDITAR_NFE_PROCESSO.
      WHEN 'DEL'.
        PERFORM deletar_nfe_processo.
      WHEN 'RNFE'.
        PERFORM mostra_rateio_doc_n55.
    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION


DATA: ctl_con_n55       TYPE REF TO cl_gui_custom_container,
      gs_lay_n55        TYPE lvc_s_layo,
      gs_var_n55        TYPE disvariant,
      gs_scroll_col_n55 TYPE lvc_s_col,
      gs_scroll_row_n55 TYPE lvc_s_roid,
      it_catalog_n55    TYPE lvc_t_fcat,
      obg_toolbar_n55   TYPE REF TO lcl_alv_toolbar_n55.

DATA: it_exclude_n55  TYPE ui_functions,
      wa_exclude_n55  LIKE LINE OF it_exclude_fcode,
      wa_add_nfe_0103 TYPE ty_add_nfe.

*&--------------------------------------------------------------------&*
*& Classes Locais                                                     &*
*&--------------------------------------------------------------------&*

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_n55 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler_n55      TYPE REF TO lcl_event_handler_n55.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_n55 IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_n55 USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.

  IF ctl_con_n55 IS INITIAL.

    CREATE OBJECT ctl_con_n55
      EXPORTING
        container_name = 'ALV_N55'.

    CREATE OBJECT ctl_alv_n55
      EXPORTING
        i_parent = ctl_con_n55.

    CREATE OBJECT obg_toolbar_n55
      EXPORTING
        io_alv_grid = ctl_alv_n55.

    SET HANDLER obg_toolbar_n55->on_toolbar FOR ctl_alv_n55.
    SET HANDLER obg_toolbar_n55->handle_user_command FOR ctl_alv_n55.

    PERFORM fill_it_fieldcatalog_n55.
*   Fill info for layout variant

    PERFORM fill_gs_variant_n55.
*   Set layout parameters for ALV grid

    gs_lay_n55-sel_mode   = 'A'.
    gs_lay_n55-zebra      = abap_true.

    CALL METHOD ctl_alv_n55->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_n55
        is_variant           = gs_var_n55
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_n55
      CHANGING
        it_fieldcatalog      = it_catalog_n55
        it_outtab            = it_cte_n55.

    CALL METHOD ctl_alv_n55->refresh_table_display.

    CREATE OBJECT event_handler_n55.
    SET HANDLER event_handler_n55->handle_hotspot_click
            FOR ctl_alv_n55.

  ELSE.
    CALL METHOD ctl_alv_n55->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_n55->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_n55
      es_row_no   = gs_scroll_row_n55.

ENDMODULE.                 " STATUS_0103  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_N55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_n55 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_n55> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_CTE_DIST_N55_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_n55.

  lc_col_pos = 1.

  LOOP AT it_catalog_n55 ASSIGNING <fs_cat_n55>.
    <fs_cat_n55>-col_pos = lc_col_pos.
    <fs_cat_n55>-tabname = 'IT_CTE_N55'.
    ADD 1 TO lc_col_pos.
    CASE <fs_cat_n55>-fieldname.
      WHEN 'IC_EDITAR'.
        <fs_cat_n55>-key     = abap_true.
        <fs_cat_n55>-hotspot = abap_true.
        <fs_cat_n55>-just    = 'C'.
      WHEN 'DOCNUM_NFE' OR 'TKNUM' OR 'FKNUM' OR 'LBLNI' OR 'EBELN' OR 'BELNR' OR 'VBELN_VF' OR 'VBELN_VL' OR 'VBELN_RE' OR 'MBLNR'.
        <fs_cat_n55>-hotspot = abap_true.
      WHEN 'IC_DADOSC'.
        <fs_cat_n55>-key     = abap_true.
        <fs_cat_n55>-hotspot = abap_true.
        <fs_cat_n55>-just    = 'C'.
      WHEN 'ZVLR_FRETE' OR 'ZVLR_MERCADORIA' OR 'ZVLR_QUEBRA' OR 'ZVLR_PERDA' OR 'ZVLR_LIQ_PAGAR'.
        <fs_cat_n55>-do_sum  = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_N55

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_N55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_n55 .

  gs_var_n55-report      = sy-repid.
  gs_var_n55-handle      = '0004'.
  gs_var_n55-log_group   = abap_false.
  gs_var_n55-username    = abap_false.
  gs_var_n55-variant     = abap_false.
  gs_var_n55-text        = abap_false.
  gs_var_n55-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_N55

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_N55
*&---------------------------------------------------------------------*
FORM handle_hotspot_click_n55
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_cte_n55 INDEX row_id INTO wa_cte_n55.

  CASE fieldname.
    WHEN 'IC_EDITAR'.
      PERFORM editar_linha_nota_n55 USING row_id CHANGING wa_cte_n55.
    WHEN 'IC_DADOSC'.
      PERFORM mostrar_dados_chegada USING wa_cte_n55-docnum_nfe.
    WHEN 'DOCNUM_NFE'.
      PERFORM mostrar_doc_fiscal USING wa_cte_n55-docnum_nfe.
    WHEN 'TKNUM' .
      PERFORM mostrar_doc_transporte USING wa_cte_n55-tknum.
    WHEN 'FKNUM' .
      PERFORM mostrar_doc_custo_trans USING wa_cte_n55-fknum.
    WHEN 'LBLNI' .
      "PERFORM MOSTRAR_ USING WA_CTE_N55-FKNUM.
    WHEN 'EBELN' .
      PERFORM mostrar_pedido USING wa_cte_n55-ebeln.
    WHEN 'BELNR' .
      PERFORM mostrar_fatura USING wa_cte_n55-belnr wa_cte_n55-gjahr.
    WHEN 'VBELN_VF' .
      PERFORM mostrar_fatura_vf USING wa_cte_n55-vbeln_vf.
    WHEN 'VBELN_VL' .
      PERFORM mostrar_remessa_vl USING wa_cte_n55-vbeln_vl.
    WHEN 'VBELN_RE'.
      PERFORM mostrar_fatura USING wa_cte_n55-vbeln_re wa_cte_n55-gjahr_re.
    WHEN 'MBLNR'.
      PERFORM mostrar_doc_material USING wa_cte_n55-mblnr wa_cte_n55-mjahr.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_NFE_PROCESSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM incluir_nfe_processo .

  DATA: wa_n55 TYPE zib_cte_dist_n55.

  CLEAR: wa_add_nfe_0103.
  wa_add_nfe_0103-cd_chave_cte = zib_cte_dist_ter-cd_chave_cte.
  CALL SCREEN 0303 STARTING AT 15 01.

  IF wa_add_nfe_0103-ck_incluir EQ abap_true.
    wa_n55-cd_chave_cte     = wa_add_nfe_0103-cd_chave_cte.
    wa_n55-n55_chave_acesso = wa_add_nfe_0103-n55_chave_acesso.
    wa_n55-ck_inc_manual    = abap_true.
    MODIFY zib_cte_dist_n55 FROM wa_n55.
    COMMIT WORK.

    CALL METHOD obj_cte->ler_dados_xi
      EXPORTING
        p_chave_cte = wa_add_nfe_0103-cd_chave_cte.

    SELECT SINGLE * INTO zib_cte_dist_ter
      FROM zib_cte_dist_ter
     WHERE cd_chave_cte EQ wa_add_nfe_0103-cd_chave_cte.

    PERFORM busca_info_notas_cte_cte.
  ENDIF.

  CLEAR: ok_code.

ENDFORM.                    " INCLUIR_NFE_PROCESSO

*&---------------------------------------------------------------------*
*&      Form  DELETAR_NFE_PROCESSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_nfe_processo .

  DATA: ck_alt_nfe TYPE char01.

  IF it_cte_n55_sel[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  ck_alt_nfe = abap_false.

  LOOP AT it_cte_n55_sel INTO wa_cte_n55.
    "IF WA_CTE_N55-CK_INC_MANUAL IS INITIAL.
    "  MESSAGE S097.
    "ELSE.
    DELETE FROM zib_cte_dist_n55
     WHERE cd_chave_cte     EQ wa_cte_n55-cd_chave_cte
       AND n55_chave_acesso EQ wa_cte_n55-n55_chave_acesso.

    ck_alt_nfe = abap_true.
    "ENDIF.
  ENDLOOP.

  IF ck_alt_nfe EQ abap_true.
    COMMIT WORK.

    CALL METHOD obj_cte->ler_dados_xi
      EXPORTING
        p_chave_cte = zib_cte_dist_ter-cd_chave_cte.

    SELECT SINGLE * INTO zib_cte_dist_ter
      FROM zib_cte_dist_ter
     WHERE cd_chave_cte EQ zib_cte_dist_ter-cd_chave_cte.

    PERFORM busca_info_notas_cte_cte.
  ENDIF.

ENDFORM.                    " DELETAR_NFE_PROCESSO

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_N55  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info_n55 INPUT.

  CALL METHOD ctl_alv_n55->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_n55
      es_row_no   = gs_scroll_row_n55.

ENDMODULE.                 " GET_SCROLL_INFO_N55  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_N55  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows_n55 INPUT.

  CLEAR it_selected_n55.

  CALL METHOD ctl_alv_n55->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_n55.

  CLEAR: it_selected_n55[], it_cte_n55_sel[].

  LOOP AT it_selected_n55 INTO wa_selected_n55.
    READ TABLE it_cte_n55 INTO wa_cte_n55 INDEX wa_selected_n55-index.
    APPEND wa_cte_n55 TO it_cte_n55_sel.
  ENDLOOP.

ENDMODULE.                 " GET_SELECTED_ROWS_N55  INPUT

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_RATEIO_DOC_N55
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostra_rateio_doc_n55 .

  IF it_cte_n55_sel[] IS INITIAL.
    MESSAGE s022.
    RETURN.
  ENDIF.

  READ TABLE it_cte_n55_sel INTO wa_cte_n55 INDEX 1.

  CLEAR: it_cte_d55[].

  SELECT * INTO TABLE it_cte_d55
    FROM zib_cte_dist_d55
   WHERE cd_chave_cte     EQ wa_cte_n55-cd_chave_cte
     AND n55_chave_acesso EQ wa_cte_n55-n55_chave_acesso.

  IF it_cte_d55[] IS NOT INITIAL.
    CALL SCREEN 0109 STARTING AT 30 5.
  ELSE.
    MESSAGE s129.
  ENDIF.

ENDFORM.                    " MOSTRA_RATEIO_DOC_N55
