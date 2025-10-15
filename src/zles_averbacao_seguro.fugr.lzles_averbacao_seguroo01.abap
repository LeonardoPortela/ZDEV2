*----------------------------------------------------------------------*
***INCLUDE LZLES_AVERBACAO_SEGUROO01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001'.
  CLEAR: gs_layout.
  IF vg_tp_rel EQ 'A'.

    PERFORM fm_alv_01.

  ELSE.

    PERFORM fm_alv_02.

  ENDIF.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0001
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0001 .

  DATA: lc_col_pos  TYPE lvc_colpos.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZLEST0143_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.


  LOOP AT it_fieldcatalog ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    IF <ls_fcat>-fieldname EQ 'CNPJ_BUPLA'.
      <ls_fcat>-convexit    = ''.
      <ls_fcat>-domname     = 'CHAR_20'.
      <ls_fcat>-reptext     = 'Cnpj Emissor CTE'.
      <ls_fcat>-outputlen   = 20.
      <ls_fcat>-inttype     = 'C'.
      <ls_fcat>-ref_table   = ''.
      <ls_fcat>-ref_field   = ''.
      <ls_fcat>-datatype    = 'CHAR'.
      <ls_fcat>-intlen      = '000020'.
      <ls_fcat>-coltext     = 'Cnpj Emissor CTE'.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0001
*&---------------------------------------------------------------------*
FORM fill_gs_variant_0001 .
  CLEAR: gs_variant.
  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0001'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows_0001 INPUT.

  CLEAR it_selected_rows.

  CALL METHOD g_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR it_zlest0143_sel[].

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_zlest0143 INTO wa_zlest0143 INDEX wa_selected_rows-index.
    APPEND wa_zlest0143 TO it_zlest0143_sel.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.


*023  Sem Acesso para Averbar Seguro! Objeto ZACSEGURO Campo ZAC_SEGURO=1
*024  Sem Acesso para Token! Objeto ZACSEGURO Campo ZAC_SEGURO=2
*025  Sem Acesso para WebService! Objeto ZACSEGURO Campo ZAC_SEGURO=3
*026  Sem Acesso para Diretório! Objeto ZACSEGURO Campo ZAC_SEGURO=4

  CASE ok_code.
    WHEN 'ATUALIZAR'.
      CLEAR: ok_code.
      PERFORM atualizar_averbacao.
    WHEN 'PNOVO'.
      CLEAR: ok_code.
      PERFORM novo_averbacao.
    WHEN 'PABRIR'.
      CLEAR: ok_code.
      PERFORM abrir_averbacao.
    WHEN 'PEDITAR'.
      CLEAR: ok_code.
      PERFORM editar_averbacao.
    WHEN 'PEXCLUIR'.
      CLEAR: ok_code.
      PERFORM excluir_averbacao.
    WHEN 'AUTORIZAR'.
      CLEAR: ok_code.
      AUTHORITY-CHECK OBJECT 'ZACSEGURO' ID 'ZAC_SEGURO' FIELD '1'.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE s023 DISPLAY LIKE 'E'.
      ELSE.
        PERFORM autorizar_averbacao.
      ENDIF.
    WHEN 'AUT_CANCEL'.
      CLEAR: ok_code.
      AUTHORITY-CHECK OBJECT 'ZACSEGURO' ID 'ZAC_SEGURO' FIELD '5'.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE s029 DISPLAY LIKE 'E'.
      ELSE.
        PERFORM cancelar_averbacao.
      ENDIF.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_AVERBACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualizar_averbacao .

  DATA: objeto TYPE REF TO zcl_averbacao_seguro.

  CREATE OBJECT objeto.

  objeto->zif_pesquisa~pesquisar( EXPORTING i_filtros = lc_filtro_43 IMPORTING e_registros = it_zlest0143 ).

  CLEAR: objeto.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  NOVO_AVERBACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM novo_averbacao .

  DATA: i_gravou TYPE char01.

  CALL FUNCTION 'ZLES_CADASTRO_AVSEG_CAD'
    EXPORTING
      i_consulta = abap_false
    IMPORTING
      i_gravou   = i_gravou.

  IF i_gravou EQ abap_true.
    PERFORM atualizar_averbacao.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ABRIR_AVERBACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM abrir_averbacao .

  IF it_zlest0143_sel[] IS INITIAL.
    MESSAGE s015.
    RETURN.
  ENDIF.

  READ TABLE it_zlest0143_sel INDEX 1 INTO DATA(wa_zlest0143_sel).

  CALL FUNCTION 'ZLES_CADASTRO_AVSEG_CAD'
    EXPORTING
      i_consulta     = abap_true
      i_cd_averbacao = wa_zlest0143_sel-cd_averbacao.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_AVERBACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM editar_averbacao .

  DATA: i_gravou TYPE char01.

  IF it_zlest0143_sel[] IS INITIAL.
    MESSAGE s015.
    RETURN.
  ENDIF.

  READ TABLE it_zlest0143_sel INDEX 1 INTO DATA(wa_zlest0143_sel).

  CALL FUNCTION 'ZLES_CADASTRO_AVSEG_CAD'
    EXPORTING
      i_consulta     = abap_false
      i_cd_averbacao = wa_zlest0143_sel-cd_averbacao
    IMPORTING
      i_gravou       = i_gravou.

  IF i_gravou EQ abap_true.
    PERFORM atualizar_averbacao.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_AVERBACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excluir_averbacao .

  DATA: obj_excluir TYPE REF TO zcl_averbacao_seguro,
        p_excluiu   TYPE c LENGTH 1.

  DATA: answer TYPE c LENGTH 1.

  IF it_zlest0143_sel[] IS INITIAL.
    MESSAGE s015.
    RETURN.
  ENDIF.

  READ TABLE it_zlest0143_sel INDEX 1 INTO DATA(wa_zlest0143_sel).

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = text-009
      textline1 = text-013
      textline2 = text-011
    IMPORTING
      answer    = answer.

  CASE answer.
    WHEN 'J'.
      CREATE OBJECT obj_excluir.
      obj_excluir->set_registro( i_id_registro = wa_zlest0143_sel-cd_averbacao ).
      p_excluiu = obj_excluir->excluir_registro( ).

      CLEAR: obj_excluir.

      IF p_excluiu EQ abap_true.
        PERFORM atualizar_averbacao.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CADASTRO_AVERBACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cadastro_averbacao .

  ck_novo_aveb = abap_false.
  ck_alterou_aveb = abap_false.
  obj_averbacao->get_registro( IMPORTING e_registro = wa_zlest0143 ).

  IF wa_zlest0143-cd_averbacao IS INITIAL.
    ck_novo_aveb = abap_true.
  ELSE.
    MOVE wa_zlest0143 TO zlest0143.
  ENDIF.

  CALL SCREEN 1000 STARTING AT 50 01.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.

  CHECK ck_alterou_aveb IS INITIAL.

  CASE ok_code.
    WHEN 'GRAVAR'.
      CLEAR: ok_code.
      IF obj_averbacao->gravar_registro( ) EQ abap_true.
        ck_gravado = abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.

  CLEAR: it_ucomm.

  IF ck_consulta EQ abap_true.
    APPEND ok_gravar TO it_ucomm.
  ENDIF.

  SET PF-STATUS 'PF2000' EXCLUDING it_ucomm.

  IF ck_consulta = abap_true.
    SET TITLEBAR 'TL1000' WITH text-001.
  ELSE.
    DATA(lc_averbacao) = obj_averbacao->get_averbacao( ).

    IF lc_averbacao-cd_averbacao IS INITIAL.
      SET TITLEBAR 'TL1000' WITH text-003.
    ELSE.
      SET TITLEBAR 'TL1000' WITH text-002.
    ENDIF.
  ENDIF.

  IF ck_alterou_aveb EQ abap_true.
    ck_alterou_aveb = abap_false.
  ENDIF.

  IF ck_consulta EQ abap_true.
    LOOP AT SCREEN .
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name(09) EQ 'ZLEST0143'.
        SPLIT screen-name AT '-' INTO str1 str2.
        i_campo = str2.
        IF obj_averbacao->valida_atributo_alteravel( EXPORTING i_campo = i_campo ) EQ abap_true.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_AVERBACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_averbacao INPUT.
  ck_alterou_aveb = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_ZLEST0143_DOCNUM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_zlest0143_docnum INPUT.
  TRY .
      obj_averbacao->set_docnum( i_docnum = zlest0143-docnum ).
    CATCH zcx_averbacao_seguro INTO DATA(cx_averbacao_seguro).
      cx_averbacao_seguro->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUIR_ZLEST0143_CD_SEG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribuir_zlest0143_cd_seg INPUT.
  "OBJ_AVERBACAO->SET_CD_SEGURADORA( I_CD_SEGURADORA = ZLEST0143-CD_SEGURADORA ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  AUTORIZAR_AVERBACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM autorizar_averbacao .

  IF it_zlest0143_sel[] IS INITIAL.
    MESSAGE s015.
    RETURN.
  ENDIF.

*  READ TABLE IT_ZLEST0143_SEL INDEX 1 INTO DATA(WA_ZLEST0143_SEL).
  LOOP AT it_zlest0143_sel INTO DATA(wa_zlest0143_sel).
    TRY.
        zcl_averbacao_seguro=>emitir_averbacao_cte( i_docnum = wa_zlest0143_sel-docnum ).
      CATCH zcx_averbacao_seguro INTO DATA(cx_averbacao_seguro).
        cx_averbacao_seguro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_arquivo INTO DATA(cx_arquivo).
        cx_arquivo->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_cadastro INTO DATA(cx_cadastro).
        cx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

  PERFORM atualizar_averbacao.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CANCELAR_AVERBACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cancelar_averbacao .

  IF it_zlest0143_sel[] IS INITIAL.
    MESSAGE s015.
    RETURN.
  ENDIF.

  READ TABLE it_zlest0143_sel INDEX 1 INTO DATA(wa_zlest0143_sel).

  TRY.
      zcl_averbacao_seguro=>cancelar_averbacao_cte( i_docnum = wa_zlest0143_sel-docnum ).
    CATCH zcx_averbacao_seguro INTO DATA(cx_averbacao_seguro).
      cx_averbacao_seguro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_arquivo INTO DATA(cx_arquivo).
      cx_arquivo->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_cadastro INTO DATA(cx_cadastro).
      cx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

  PERFORM atualizar_averbacao.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0004 .

  DATA: lc_col_pos  TYPE lvc_colpos.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZLEST0143_ALV_SINT'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_s.

  LOOP AT it_fieldcatalog_s ASSIGNING FIELD-SYMBOL(<l_fcat>).
    IF <l_fcat>-fieldname EQ 'ZTOT_VIAG' OR <l_fcat>-fieldname EQ 'ZTOT_V_TRANSP' OR <l_fcat>-fieldname EQ 'ZVLOR_TOTAL'.
      <l_fcat>-outputlen   = 30.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0001_S
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0001_s .
  CLEAR: gs_variant_s-report.
  gs_variant_s-report      = sy-repid.
  gs_variant_s-handle      = '0001'.
  gs_variant_s-log_group   = abap_false.
  gs_variant_s-username    = abap_false.
  gs_variant_s-variant     = abap_false.
  gs_variant_s-text        = abap_false.
  gs_variant_s-dependvars  = abap_false.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ALV_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_alv_01 .

FREE: dg_splitter, dg_splitter_2, table_element, table_element2, dg_dyndoc_id,  column, column_1, column_2, dg_html_cntrl.

*  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_html
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html1.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_html2.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_html2.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CREATE OBJECT g_alv
      EXPORTING
        i_parent = dg_parent_alv.


*   Set layout parameters for ALV grid
    "GS_LAYOUT-GRID_TITLE = TEXT-100.
    gs_layout-sel_mode   = 'A'.
    gs_layout-zebra      = 'X'.

    gs_layout-cwidth_opt = 'X'.
    gs_layout-col_opt    = 'X'.
    PERFORM fill_it_fieldcatalog_0001.
    PERFORM fill_gs_variant_0001.

    CALL METHOD g_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_zlest0143[].

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_align = 'CENTER'
        sap_style = cl_dd_document=>heading.

*    p_text = text-008.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.

    "SDYDO_TEXT_ELEMENT = ''.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD table_element2->add_column
      IMPORTING
        column = column_2.

    CALL METHOD table_element2->set_column_style
      EXPORTING
        col_no       = 2
        sap_align    = 'LEFT'
        sap_fontsize = cl_dd_document=>medium.

    CLEAR: p_text_table.

*PBUKRS	Sociedad
    "SDYDO_TEXT_ELEMENT = 'Departamentos NF-e Inbound'.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    CALL METHOD column_2->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_html1
      EXCEPTIONS
        html_display_error = 1.
*  ENDIF.


  CALL METHOD g_alv->refresh_table_display.

  CALL METHOD g_alv->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col
      es_row_no   = gs_scroll_row.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ALV_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_alv_02 .
  FREE: dg_splitter, dg_splitter_2, table_element, table_element2, dg_dyndoc_id,  column, column_1, column_2, dg_html_cntrl.

*  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_html
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html1.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_html2.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_html2.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv_2.

    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CREATE OBJECT g_alv2
      EXPORTING
        i_parent = dg_parent_alv_2.


    gs_layout-sel_mode   = 'A'.
    gs_layout-zebra      = 'X'.


    PERFORM fill_it_fieldcatalog_0004.
    PERFORM fill_gs_variant_0001_s.

    CALL METHOD g_alv2->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant_s
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_s
        it_outtab            = it_zlest0143_alv_sint[].

     CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_align = 'CENTER'
        sap_style = cl_dd_document=>heading.

*    p_text = text-008.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.

    "SDYDO_TEXT_ELEMENT = ''.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD table_element2->add_column
      IMPORTING
        column = column_2.

    CALL METHOD table_element2->set_column_style
      EXPORTING
        col_no       = 2
        sap_align    = 'LEFT'
        sap_fontsize = cl_dd_document=>medium.

    CLEAR: p_text_table.

*PBUKRS	Sociedad
    "SDYDO_TEXT_ELEMENT = 'Departamentos NF-e Inbound'.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    CALL METHOD column_2->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_html1
      EXCEPTIONS
        html_display_error = 1.


  CALL METHOD g_alv2->refresh_table_display.

  CALL METHOD g_alv2->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_
      es_row_no   = gs_scroll_row_.
ENDFORM.
