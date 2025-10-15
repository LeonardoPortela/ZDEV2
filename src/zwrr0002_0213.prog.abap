*----------------------------------------------------------------------*
***INCLUDE ZWRR0002_0213.
*----------------------------------------------------------------------*

*       CLASS LCL_ALV_TOOLBAR_0213 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_0213 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

DATA: ctl_alv_0213              TYPE REF TO cl_gui_alv_grid,
      c_alv_toolbarmanager_0213 TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: it_selected_0213 TYPE lvc_t_row,
      wa_selected_0213 TYPE lvc_s_row,
      tg_docrefs_sel   TYPE TABLE OF ty_doc_refs WITH HEADER LINE,
      wa_docrefs       TYPE ty_doc_refs.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_0213 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_0213 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager_0213
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    IF wg_docs-docnum IS INITIAL.
      ty_toolbar-disabled  = abap_false.
    ELSE.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-007.
    ty_toolbar-butn_type = 0.
    IF wg_docs-docnum IS INITIAL.
      ty_toolbar-disabled  = abap_false.
    ELSE.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager_0213->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_selected_0213, it_selected_0213[].

    CALL METHOD ctl_alv_0213->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_0213.

    CLEAR: tg_docrefs_sel[].

    LOOP AT it_selected_0213 INTO wa_selected_0213.
      READ TABLE tg_docrefs INTO wa_docrefs INDEX wa_selected_0213-index.
      APPEND wa_docrefs TO tg_docrefs_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_doc_processo.
      WHEN 'DEL'.
        PERFORM deletar_doc_processo.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0213 IMPLEMENTATION

DATA: ctl_con_0213       TYPE REF TO cl_gui_custom_container,
      gs_lay_0213        TYPE lvc_s_layo,
      gs_var_0213        TYPE disvariant,
      gs_scroll_col_0213 TYPE lvc_s_col,
      gs_scroll_row_0213 TYPE lvc_s_roid,
      it_catalog_0213    TYPE lvc_t_fcat,
      obg_toolbar_0213   TYPE REF TO lcl_alv_toolbar_0213.

DATA: it_exclude_0213 TYPE ui_functions,
      wa_exclude_0213 LIKE LINE OF it_exclude_0213.


DATA: ok_confirmado     TYPE char01,
      wa_lanc_fiscal    TYPE j_1bnfdoc,
      ck_alterou_docnum TYPE char01.

*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS_0213  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos_0213 OUTPUT.

  IF ctl_con_0213 IS INITIAL.

    CREATE OBJECT ctl_con_0213
      EXPORTING
        container_name = 'ALV_DOC_REF'.

    CREATE OBJECT ctl_alv_0213
      EXPORTING
        i_parent = ctl_con_0213.

    CREATE OBJECT obg_toolbar_0213
      EXPORTING
        io_alv_grid = ctl_alv_0213.

    SET HANDLER obg_toolbar_0213->on_toolbar FOR ctl_alv_0213.
    SET HANDLER obg_toolbar_0213->handle_user_command FOR ctl_alv_0213.

    PERFORM fill_it_fieldcatalog_0213.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0213.
*   Set layout parameters for ALV grid

    gs_lay_0213-sel_mode   = 'A'.
    gs_lay_0213-zebra      = abap_true.
    gs_lay_0213-grid_title = TEXT-001.

    CALL METHOD ctl_alv_0213->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0213
        is_variant           = gs_var_0213
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0213
      CHANGING
        it_fieldcatalog      = it_catalog_0213
        it_outtab            = tg_docrefs[].

    CALL METHOD ctl_alv_0213->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_0213->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0213->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0213
      es_row_no   = gs_scroll_row_0213.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_DOC_PROCESSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_doc_processo .

  DATA: vw_fiscal TYPE j_1bnfdoc.

  ok_confirmado = abap_false.

  CALL SCREEN 0301 STARTING AT 15 01.

  IF ok_confirmado EQ abap_true.
    SELECT SINGLE * INTO vw_fiscal
      FROM j_1bnfdoc
     WHERE docnum EQ wa_lanc_fiscal-docnum.

    tg_docrefs-docnum = vw_fiscal-docnum.
    tg_docrefs-docdat = vw_fiscal-docdat.
    tg_docrefs-model  = vw_fiscal-model .
    tg_docrefs-series = vw_fiscal-series.
    tg_docrefs-nfenum = vw_fiscal-nfenum.
    tg_docrefs-nftot  = vw_fiscal-nftot.
    APPEND tg_docrefs.

    LEAVE TO SCREEN 0100.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_DOC_PROCESSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_doc_processo .

  LOOP AT tg_docrefs_sel.
    DELETE tg_docrefs WHERE docnum = tg_docrefs_sel-docnum.
  ENDLOOP.

  LEAVE TO SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0213
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0213 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0213> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFIWRT0020_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_0213.

  lc_col_pos = 1.

  LOOP AT it_catalog_0213 ASSIGNING <fs_cat_0213>.
    CASE <fs_cat_0213>-fieldname.
      WHEN 'NFTOT'.
        <fs_cat_0213>-scrtext_m = <fs_cat_0213>-scrtext_s = <fs_cat_0213>-scrtext_l = 'Valor'.
        <fs_cat_0213>-do_sum = abap_true.
        <fs_cat_0213>-outputlen = '15'.
    ENDCASE.

*    <FS_CAT_0213>-COL_POS = LC_COL_POS.
*    <FS_CAT_0213>-TABNAME = 'IT_CTE_N55'.
*    ADD 1 TO LC_COL_POS.
*    CASE <FS_CAT_0213>-FIELDNAME.
*      WHEN 'IC_EDITAR'.
*        <FS_CAT_0213>-KEY     = ABAP_TRUE.
*        <FS_CAT_0213>-HOTSPOT = ABAP_TRUE.
*        <FS_CAT_0213>-JUST    = 'C'.
*      WHEN 'DOCNUM_NFE' OR 'TKNUM' OR 'FKNUM' OR 'LBLNI' OR 'EBELN' OR 'BELNR' OR 'VBELN_VF' OR 'VBELN_VL' OR 'VBELN_RE' OR 'MBLNR'.
*        <FS_CAT_0213>-HOTSPOT = ABAP_TRUE.
*      WHEN 'ZVLR_FRETE' OR 'ZVLR_MERCADORIA' OR 'ZVLR_QUEBRA' OR 'ZVLR_PERDA' OR 'ZVLR_LIQ_PAGAR'.
*        <FS_CAT_0213>-DO_SUM  = ABAP_TRUE.
*    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0213
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0213 .

  gs_var_0213-report      = sy-repid.
  gs_var_0213-handle      = '0213'.
  gs_var_0213-log_group   = abap_false.
  gs_var_0213-username    = abap_false.
  gs_var_0213-variant     = abap_false.
  gs_var_0213-text        = abap_false.
  gs_var_0213-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0301 INPUT.
  CASE ok_code.
    WHEN 'CONFIRMAR'.

      IF wa_lanc_fiscal-docnum EQ wg_docs-docnum.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Documento' wa_lanc_fiscal-docnum 'não pode ser informado!'.
        EXIT.
      ENDIF.

      SELECT SINGLE * INTO wa_lanc_fiscal FROM j_1bnfdoc WHERE docnum EQ wa_lanc_fiscal-docnum.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Documento' wa_lanc_fiscal-docnum 'não encontrado!'.
        EXIT.
      ENDIF.

      IF wa_lanc_fiscal-cancel EQ abap_true.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Documento' wa_lanc_fiscal-docnum 'está cancelado!'.
        EXIT.
      ENDIF.

      IF ck_alterou_docnum EQ abap_true.
        ck_alterou_docnum = abap_false.
        EXIT.
      ENDIF.
      ok_confirmado = abap_true.
      CLEAR: ok_code.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0301 OUTPUT.

  SET PF-STATUS 'PF0301'.
  SET TITLEBAR 'TL0301'.

  IF wa_lanc_fiscal-docnum IS NOT INITIAL.
    SELECT SINGLE * INTO wa_lanc_fiscal FROM j_1bnfdoc WHERE docnum EQ wa_lanc_fiscal-docnum.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0301_exit INPUT.
  ok_confirmado = abap_false.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_DOCNUM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_docnum INPUT.
  ck_alterou_docnum = abap_true.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALIDA_TAXLW1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_taxlw1 INPUT.

*  CHECK w_zfiwrt0001-complement_icms = 'S'.
*
*  SELECT SINGLE *
*    INTO @DATA(w_batl1)
*    FROM j_1batl1
*   WHERE taxlaw = @wg_direitos-taxlw1.
*
*  IF sy-subrc <> 0.
*    MESSAGE e024(sd) WITH 'Direito fiscal ICMS não encontrado!'.
*  ENDIF.

ENDMODULE.

MODULE valida_taxlw2 INPUT.

*  CHECK w_zfiwrt0001-complement_icms = 'S'.
*
*  SELECT SINGLE *
*    INTO @DATA(w_batl2)
*    FROM j_1batl2
*   WHERE taxlaw = @wg_direitos-taxlw2.
*
*  IF sy-subrc <> 0.
*    MESSAGE e024(sd) WITH 'Direito fiscal IPI não encontrado!'.
*  ENDIF.

ENDMODULE.

MODULE valida_taxlw4 INPUT.

*  CHECK w_zfiwrt0001-complement_icms = 'S'.
*
*  SELECT SINGLE *
*    INTO @DATA(w_batl4a)
*    FROM j_1batl4a
*   WHERE taxlaw = @wg_direitos-taxlw4.
*
*  IF sy-subrc <> 0.
*    MESSAGE e024(sd) WITH 'Lei tributária COFINS não encontrado!'.
*  ENDIF.

ENDMODULE.

MODULE valida_taxlw5 INPUT.

*  CHECK w_zfiwrt0001-complement_icms = 'S'.
*
*  SELECT SINGLE *
*    INTO @DATA(w_batl5)
*    FROM j_1batl5
*   WHERE taxlaw = @wg_direitos-taxlw5.
*
*  IF sy-subrc <> 0.
*    MESSAGE e024(sd) WITH 'Lei tributária PIS não encontrado!'.
*  ENDIF.

ENDMODULE.
