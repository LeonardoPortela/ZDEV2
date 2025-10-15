*----------------------------------------------------------------------*
***INCLUDE ZGLT067_5001 .
*----------------------------------------------------------------------*

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_5001 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler_5001  TYPE REF TO lcl_event_handler_5001.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_5001 IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_5001 USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "DATA_CHANGED_FINISHED
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_5001
*&---------------------------------------------------------------------*
FORM handle_hotspot_click_5001
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  DATA: conta TYPE fagl_range_racct.

  IF row_id GT 0.
    READ TABLE it_conta_data INDEX row_id INTO wa_conta_data.

    CASE fieldname.
      WHEN 'CONTA'.
        "IF WA_CONTA_DATA-CONTA IS NOT INITIAL.
        CLEAR: psaknr_aux, psaknr_aux[].
        MOVE: wa_conta_data-bukrs TO pbukrs,
              wa_conta_data-data TO pdata.
        CLEAR: psaknr_aux, psaknr_aux[], conta.
        conta-sign   = 'I'.
        conta-option = 'EQ'.
        conta-low    = wa_conta_data-conta.
        conta-high   = wa_conta_data-conta.
        APPEND conta TO psaknr_aux.

        PERFORM popula_contas USING pbukrs pdata psaknr_aux.

        CHECK it_selecao[] IS NOT INITIAL.

        READ TABLE it_selecao INDEX 1 INTO zde_saldo_cta_banco.
        PERFORM chama_tela.
        "ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_5001

DATA: ctl_alv_5001       TYPE REF TO cl_gui_alv_grid,
      ctl_con_5001       TYPE REF TO cl_gui_custom_container,
      gs_lay_5001        TYPE lvc_s_layo,
      gs_var_5001        TYPE disvariant,
      gs_scroll_col_5001 TYPE lvc_s_col,
      gs_scroll_row_5001 TYPE lvc_s_roid,
      it_catalog_5001    TYPE lvc_t_fcat.

DATA: it_selected_5001 TYPE lvc_t_row,
      wa_selected_5001 TYPE lvc_s_row.

DATA: it_exclude_5001 TYPE ui_functions,
      wa_exclude_5001 LIKE LINE OF it_exclude_5001.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5001 OUTPUT.

  DATA: fs_sort_5001 TYPE lvc_s_sort,
        gt_sort_5001 TYPE lvc_t_sort.

  SET PF-STATUS 'PF5001'.
  SET TITLEBAR 'TL5001'.

  IF ctl_con_5001 IS INITIAL.

    CREATE OBJECT ctl_con_5001
      EXPORTING
        container_name = 'ALV_5001'.

    CREATE OBJECT ctl_alv_5001
      EXPORTING
        i_parent = ctl_con_5001.

    PERFORM fill_it_fieldcatalog_5001.
*   Fill info for layout variant

    PERFORM fill_gs_variant_5001.
*   Set layout parameters for ALV grid

    "GS_LAY_0102-SEL_MODE   = 'A'.
    gs_lay_5001-zebra = 'X'.

    CLEAR: fs_sort_5001.
    fs_sort_5001-spos       = 1.     "first sorting key
    fs_sort_5001-fieldname  = 'CONTA_NR'. "fieldname for sort
    fs_sort_5001-up         = 'X'. "sort ascending
    INSERT fs_sort_5001 INTO TABLE gt_sort_5001. "insert to sort table

    CALL METHOD ctl_alv_5001->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_5001
        is_variant           = gs_var_5001
        it_toolbar_excluding = it_exclude_5001
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_catalog_5001        "IT_EXCEPT_QINFO = IT_HINTS
        it_outtab            = it_conta_data[]
        it_sort              = gt_sort_5001[].

    CREATE OBJECT event_handler_5001.
    SET HANDLER event_handler_5001->handle_hotspot_click FOR ctl_alv_5001.

    CALL METHOD ctl_alv_5001->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_5001->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_5001->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_5001
      es_row_no   = gs_scroll_row_5001.

ENDMODULE.                 " STATUS_5001  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_5001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_5001 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_5001> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_CAB_EXTRATO_BANCO'
    CHANGING
      ct_fieldcat      = it_catalog_5001.

  lc_col_pos = 1.

  LOOP AT it_catalog_5001 ASSIGNING <fs_cat_5001>.
    IF <fs_cat_5001>-fieldname EQ 'CONTA'.
      <fs_cat_5001>-hotspot = abap_true.
    ENDIF.

    CASE <fs_cat_5001>-datatype.
      WHEN 'QUAN' OR 'CURR' OR 'DEC'.
        <fs_cat_5001>-do_sum    = abap_true.
        <fs_cat_5001>-outputlen = 20.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_5001

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_5001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_5001 .

  gs_var_5001-report      = sy-repid.
  gs_var_5001-handle      = '5001'.
  gs_var_5001-log_group   = abap_false.
  gs_var_5001-username    = abap_false.
  gs_var_5001-variant     = abap_false.
  gs_var_5001-text        = abap_false.
  gs_var_5001-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_5001

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5001 INPUT.
  CLEAR: wa_conta_data.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_5001  INPUT

*&---------------------------------------------------------------------*
*&      Form  LANCAMENTO_EXTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM lancamento_extrato .

  DATA: posicao TYPE sy-tabix.

  CLEAR: it_mov_lct_banco[].

  LOOP AT it_tabela INTO wa_tabela WHERE tag25-tag25_1 EQ wa_conta_data-conta_nr AND tag EQ ':61:'.
    CLEAR: it_mov_lct_banco.

    it_mov_lct_banco-fcurr = zde_saldo_cta_banco-waers.
    it_mov_lct_banco-bukrs = zde_saldo_cta_banco-bukrs.
    it_mov_lct_banco-tcurr = zde_saldo_cta_banco-waers.

    CALL FUNCTION 'REPLACE_STRING'
      EXPORTING
        i_searchstring   = ','
        i_replacestring  = '.'
      CHANGING
        io_string        = wa_tabela-tag61-tag61_5
      EXCEPTIONS
        string_not_found = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF wa_tabela-tag61-tag61_3 EQ 'D'.
      it_mov_lct_banco-dmbtr = wa_tabela-tag61-tag61_5.
      it_mov_lct_banco-dmbtr = it_mov_lct_banco-dmbtr * -1.
    ELSE.
      it_mov_lct_banco-dmbtr = wa_tabela-tag61-tag61_5.
    ENDIF.

    "Não fazer mais nada abaixo desta linha dentro do loop.
    posicao = sy-tabix + 1.
    READ TABLE it_tabela INTO wa_tabela INDEX posicao.
    IF sy-subrc IS INITIAL AND wa_tabela-tag EQ ':86:'.
      IF wa_tabela-tag86-tag86_1 = '999'.
        it_mov_lct_banco-sgtxt = wa_tabela-tag86-tag86_2.
      ELSE.
        CONCATENATE wa_tabela-tag86-tag86_00
                    wa_tabela-tag86-tag86_30t
                    wa_tabela-tag86-tag86_30
                    wa_tabela-tag86-tag86_31
               INTO it_mov_lct_banco-sgtxt.
      ENDIF.
    ENDIF.
    APPEND it_mov_lct_banco.
  ENDLOOP.

  IF it_mov_lct_banco[] IS NOT INITIAL.
    PERFORM popula_info_disp_lote.

    lc_arquivo_extrato = abap_true.
    CLEAR sd_compen_cop.
    vg_text_010 = TEXT-018.
    CALL SCREEN 0304 STARTING AT 10 3.
    lc_arquivo_extrato = abap_false.
  ENDIF.

ENDFORM.                    " LANCAMENTO_EXTRATO
