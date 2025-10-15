*----------------------------------------------------------------------*
***INCLUDE LZGFSD0005F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_9000
*&---------------------------------------------------------------------*
FORM f_refresh_9000 .

  CLEAR: go_cc_9000_01,
         "go_alv_9000_01,
         gt_alv_9000_01,

         go_cc_9000_02,
         "go_alv_9000_02,
         gt_alv_9000_02,

         gv_9000_canc,
         gt_j_1btxic3v.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_9000_INIT_01
*&---------------------------------------------------------------------*
FORM f_alv_9000_init_01 .

  DATA lt_fieldcat TYPE lvc_t_fcat.
  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  "DATA lo_events TYPE REF TO cl_salv_events_table.
  "DATA lo_handle TYPE REF TO lcl_event_handler.

  IF go_cc_9000_01 IS INITIAL.

    CREATE OBJECT go_cc_9000_01
      EXPORTING
        container_name              = 'CC_ALV_01'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
      MESSAGE i000(d2) WITH 'The custom control could not be created'.
      RETURN.
    ENDIF.

  ENDIF.

  lw_layout-sel_mode = 'A'.

  lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.
  lw_layout-no_toolbar = 'X'.
  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.
  lw_layout-info_fname = 'ASSOCIADO_COLOR'.

  IF go_alv_9000_01 IS INITIAL.

    CREATE OBJECT go_alv_9000_01
      EXPORTING
        i_parent = go_cc_9000_01.

    PERFORM f_monta_fieldcat USING 'ZSDS085' CHANGING lt_fieldcat.
    "PERFORM f_fieldcat_modi USING 'STATUS_ATU' 'CHECKBOX' 'X' CHANGING lt_fieldcat.

    PERFORM f_coluna_edita2 USING 'DOC_SIMULACAO' 'N°.Simu' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'VBELN' 'N°.OV' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'POSNR' 'Item' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'MATNR' 'Material' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'MAKTX' 'Descrição' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'VKBUR' 'Escr.Venda' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'WERKS' 'Centro Fat' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'KWERT_ICMI' 'Total OV' CHANGING lt_fieldcat.
    " 19.09.2024 - RAMON -- #101482 -->
    PERFORM f_coluna_edita2 USING 'KBETR_ICBS' 'Base ICMS' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'KBETR_ICVA' 'Taxa ICMS' CHANGING lt_fieldcat.
    " 19.09.2024 - RAMON -- #101482 --<
    PERFORM f_coluna_edita2 USING 'VLR_LIQ' 'PreçoLiq' CHANGING lt_fieldcat.

    "PERFORM f_coluna_edita2 USING 'STATUS_ATU' 'Atualizado' CHANGING lt_fieldcat.

    DELETE lt_fieldcat WHERE fieldname = 'KNUMV'.
    DELETE lt_fieldcat WHERE fieldname = 'KWERT_RB00'.
    DELETE lt_fieldcat WHERE fieldname = 'KWERT_PR00'.

    DELETE lt_fieldcat WHERE fieldname = 'EXTWG'.
    DELETE lt_fieldcat WHERE fieldname = 'ERDAT'.
    DELETE lt_fieldcat WHERE fieldname = 'KWMENG'.
    DELETE lt_fieldcat WHERE fieldname = 'KWMENG_PR00'.
    DELETE lt_fieldcat WHERE fieldname = 'GEWEI_PR00'.
    DELETE lt_fieldcat WHERE fieldname = 'KUNNR'.
    DELETE lt_fieldcat WHERE fieldname = 'GEWEI'.
    DELETE lt_fieldcat WHERE fieldname = 'WAERK'.
    DELETE lt_fieldcat WHERE fieldname = 'CONF_ICBS'.
    DELETE lt_fieldcat WHERE fieldname = 'CONF_ICVA'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_ICMS'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_ICVA'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_ICMS_NOVO'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_ICVA_NOVO'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_LIQ_NOVO'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_DIFERENCA'.
    DELETE lt_fieldcat WHERE fieldname = 'SHIPFROM'.
    DELETE lt_fieldcat WHERE fieldname = 'SHIPTO'.
    DELETE lt_fieldcat WHERE fieldname = 'ASSOCIADO'.
    DELETE lt_fieldcat WHERE fieldname = 'ASSOCIADO_COLOR'.
    DELETE lt_fieldcat WHERE fieldname = 'OBJNR'.
    DELETE lt_fieldcat WHERE fieldname = 'STATUS_ATU'.
    DELETE lt_fieldcat WHERE fieldname = 'GRUOP'.

    lw_variant-report       = sy-repid.
    lw_variant-username     = sy-uname.

    " Configuration for first display.
    CALL METHOD go_alv_9000_01->set_table_for_first_display
      EXPORTING
        is_layout       = lw_layout
        is_variant      = lw_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_alv_9000_01
        it_fieldcatalog = lt_fieldcat.

  ELSE.

    CALL METHOD go_alv_9000_01->set_frontend_layout
      EXPORTING
        is_layout = lw_layout.

    CALL METHOD go_alv_9000_01->refresh_table_display( ).

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_MODI
*&---------------------------------------------------------------------*
FORM f_fieldcat_modi USING p_fieldname TYPE slis_fieldname
                           p_column TYPE c
                           p_value TYPE any
                  CHANGING p_field_cat TYPE lvc_t_fcat.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  DATA(lv_name) = '<FS_FCAT>-' && p_column.

  ASSIGN (lv_name) TO FIELD-SYMBOL(<fs_colum>).

  CHECK sy-subrc EQ 0.

  <fs_colum> = p_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_9000_INIT_02
*&---------------------------------------------------------------------*
FORM f_alv_9000_init_02 .

  DATA lt_fieldcat TYPE lvc_t_fcat.
  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  "DATA lo_events TYPE REF TO cl_salv_events_table.
  "DATA lo_handle TYPE REF TO lcl_event_handler.

  IF go_cc_9000_02 IS INITIAL.

    CREATE OBJECT go_cc_9000_02
      EXPORTING
        container_name              = 'CC_ALV_02'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
      MESSAGE i000(d2) WITH 'The custom control could not be created'.
      RETURN.
    ENDIF.

  ENDIF.

  lw_layout-sel_mode = 'A'.

  lw_layout-col_opt = 'X'.
  lw_layout-cwidth_opt = 'X'.
  lw_layout-no_toolbar = 'X'.
  lw_layout-box_fname = 'SELEC'.
  lw_layout-zebra = 'X'.
  lw_layout-info_fname = 'ASSOCIADO_COLOR'.

  IF go_alv_9000_02 IS INITIAL.

    CREATE OBJECT go_alv_9000_02
      EXPORTING
        i_parent = go_cc_9000_02.

    PERFORM f_monta_fieldcat USING 'ZSDS085' CHANGING lt_fieldcat.

    "PERFORM f_fieldcat_modi USING 'STATUS_ATU' 'CHECKBOX' 'X' CHANGING lt_fieldcat.

    PERFORM f_coluna_edita2 USING 'DOC_SIMULACAO' 'N°.Simu' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'VBELN' 'N°.OV' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'POSNR' 'Item' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'MATNR' 'Material' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'MAKTX' 'Descrição' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'VKBUR' 'Escr.Venda' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'WERKS' 'Centro Fat' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'KWERT_ICMI' 'Total OV' CHANGING lt_fieldcat.
    " 19.09.2024 - RAMON -- #101482 -->
    PERFORM f_coluna_edita2 USING 'CONF_ICBS' 'Base ICMS' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'CONF_ICVA' 'Taxa ICMS' CHANGING lt_fieldcat.
    " 19.09.2024 - RAMON -- #101482 --<
    PERFORM f_coluna_edita2 USING 'VLR_LIQ_NOVO' 'PreçoLiq' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'VLR_DIFERENCA' 'Diferença' CHANGING lt_fieldcat.

    DELETE lt_fieldcat WHERE fieldname = 'KNUMV'.
    DELETE lt_fieldcat WHERE fieldname = 'KWERT_RB00'.
    DELETE lt_fieldcat WHERE fieldname = 'KWERT_PR00'.

    DELETE lt_fieldcat WHERE fieldname = 'EXTWG'.
    DELETE lt_fieldcat WHERE fieldname = 'ERDAT'.
    DELETE lt_fieldcat WHERE fieldname = 'KWMENG'.
    DELETE lt_fieldcat WHERE fieldname = 'KWMENG_PR00'.
    DELETE lt_fieldcat WHERE fieldname = 'GEWEI_PR00'.
    DELETE lt_fieldcat WHERE fieldname = 'GEWEI'.
    DELETE lt_fieldcat WHERE fieldname = 'KUNNR'.
    DELETE lt_fieldcat WHERE fieldname = 'WAERK'.
    DELETE lt_fieldcat WHERE fieldname = 'KBETR_ICBS'.
    DELETE lt_fieldcat WHERE fieldname = 'KBETR_ICVA'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_ICMS'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_ICVA'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_ICMS_NOVO'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_ICVA_NOVO'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_LIQ'.
    DELETE lt_fieldcat WHERE fieldname = 'STATUS_ATU'.
    DELETE lt_fieldcat WHERE fieldname = 'SHIPFROM'.
    DELETE lt_fieldcat WHERE fieldname = 'SHIPTO'.
    DELETE lt_fieldcat WHERE fieldname = 'ASSOCIADO'.
    DELETE lt_fieldcat WHERE fieldname = 'ASSOCIADO_COLOR'.
    DELETE lt_fieldcat WHERE fieldname = 'VLR_DIFERENCA'.
    DELETE lt_fieldcat WHERE fieldname = 'OBJNR'.
    DELETE lt_fieldcat WHERE fieldname = 'STATUS_ATU'.
    DELETE lt_fieldcat WHERE fieldname = 'GRUOP'.

    lw_variant-report       = sy-repid.
    lw_variant-username     = sy-uname.

    " Configuration for first display.
    CALL METHOD go_alv_9000_02->set_table_for_first_display
      EXPORTING
        is_layout       = lw_layout
        is_variant      = lw_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_alv_9000_02
        it_fieldcatalog = lt_fieldcat.

  ELSE.

    CALL METHOD go_alv_9000_02->set_frontend_layout
      EXPORTING
        is_layout = lw_layout.

    CALL METHOD go_alv_9000_02->refresh_table_display( ).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT_4000_1
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat USING p_struct TYPE tabname
                   CHANGING p_field_cat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_struct
    CHANGING
      ct_fieldcat            = p_field_cat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita2  USING p_fieldname TYPE slis_fieldname
                            p_text_s TYPE scrtext_s
                   CHANGING p_field_cat TYPE lvc_t_fcat.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-scrtext_s  = p_text_s.
  <fs_cat>-scrtext_m = p_text_s.
  <fs_cat>-scrtext_l = p_text_s.
  <fs_cat>-reptext = p_text_s.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ALV_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atualiza_alv_9000 .

  PERFORM f_alv_refresh_grid_9000 USING 'X' 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_GRID
*&---------------------------------------------------------------------*
FORM f_alv_refresh_grid_9000 USING p_01 TYPE c
                                   p_02 TYPE c.

  IF p_01 IS NOT INITIAL.

    IF go_alv_9000_01 IS NOT INITIAL.
      go_alv_9000_01->refresh_table_display( ).
    ENDIF.

  ENDIF.

  IF p_02 IS NOT INITIAL.

    IF go_alv_9000_02 IS NOT INITIAL.
      go_alv_9000_02->refresh_table_display( ).
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SEARCH_9000
*&---------------------------------------------------------------------*
FORM f_search_9000 .

  IF so_erdat[] IS INITIAL AND so_vbeln[] IS INITIAL.
    MESSAGE 'Preencher data' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: gt_alv_9000_01[], gt_alv_9000_02[].

  CALL FUNCTION 'ZSDMF_INSUMOS_GET_OVS'
    EXPORTING
      ir_vbeln_va = so_vbeln[]
      ir_erdat    = so_erdat[]
    TABLES
      et_ovs      = gt_alv_9000_01.

  IF gt_alv_9000_01[] IS INITIAL.
    MESSAGE 'Dados não encontrado' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
FORM f_get_selected_rows USING uv_alv TYPE c
                      CHANGING ct_rows TYPE lvc_t_roid.

  CASE uv_alv.
    WHEN '01'.

      CALL METHOD go_alv_9000_01->get_selected_rows
        IMPORTING
          et_row_no = ct_rows.

    WHEN '02'.

      CALL METHOD go_alv_9000_02->get_selected_rows
        IMPORTING
          et_row_no = ct_rows.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ASSOCIAR_DADOS
*&---------------------------------------------------------------------*
FORM f_associar_dados USING uv_alv_de TYPE c
                            uv_alv_para TYPE c.

  DATA lv_tab_de TYPE c LENGTH 30.
  DATA lv_tab_para TYPE c LENGTH 30.

  DATA lt_rows TYPE lvc_t_roid.
  FIELD-SYMBOLS <fs_de_tab> TYPE zsdc085.
  FIELD-SYMBOLS <fs_para_tab> TYPE zsdc085.

  PERFORM f_get_selected_rows USING uv_alv_de CHANGING lt_rows.

  lv_tab_de = 'GT_ALV_9000_' && uv_alv_de.
  lv_tab_para = 'GT_ALV_9000_' && uv_alv_para.

  ASSIGN (lv_tab_de) TO <fs_de_tab>.
  ASSIGN (lv_tab_para) TO <fs_para_tab>.

  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).

    READ TABLE <fs_de_tab> ASSIGNING FIELD-SYMBOL(<fs_de>) INDEX <fs_rows>-row_id.

    CHECK sy-subrc EQ 0.

    CHECK <fs_de>-status_atu IS INITIAL.

    IF uv_alv_de = '01' AND <fs_de>-associado IS INITIAL.

      <fs_de>-associado = abap_true.
      <fs_de>-associado_color = 'C601'.

      APPEND <fs_de> TO <fs_para_tab>.

      CONTINUE.

    ENDIF.

    IF uv_alv_de = '02' AND <fs_de>-associado IS NOT INITIAL.

      READ TABLE <fs_para_tab> ASSIGNING FIELD-SYMBOL(<fs_para>)
        WITH KEY doc_simulacao = <fs_de>-doc_simulacao
                 vbeln  = <fs_de>-vbeln
                 posnr = <fs_de>-posnr.

      IF sy-subrc EQ 0.

        <fs_para>-associado_color = space.
        <fs_de>-associado_color = space.

        <fs_para>-associado = abap_false.
        <fs_de>-associado = abap_false.

      ENDIF.

    ENDIF.

  ENDLOOP.

  IF uv_alv_para = '02'.
    LOOP AT <fs_para_tab> ASSIGNING <fs_para>.
      <fs_para>-associado_color = space.
    ENDLOOP.
  ENDIF.

  IF uv_alv_de = '02'.
    DELETE <fs_de_tab> WHERE associado IS INITIAL.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_UPDT_ASSOCIADOS
*&---------------------------------------------------------------------*
FORM f_updt_associados .

  LOOP AT gt_alv_9000_02 ASSIGNING FIELD-SYMBOL(<fs_alv_02>).

    "PERFORM f_aplicar_j1btax CHANGING <fs_alv_02>.

*    <fs_alv_02>-taxa_kbetr_icms = <fs_alv_02>-novo_kbetr_icva.
*    <fs_alv_02>-base_kbetr_icms = <fs_alv_02>-novo_kbetr_icbs.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_APLICAR_J1BTAX
*&---------------------------------------------------------------------*
FORM f_aplicar_j1btax CHANGING cs_alv TYPE zsds085.

  PERFORM f_get_j1btax_tab.

  LOOP AT gt_j_1btxic3v ASSIGNING FIELD-SYMBOL(<fs_tax>)
      WHERE gruop IN gr_gruop_in
        AND shipfrom = cs_alv-shipfrom
        AND shipto = cs_alv-shipto
        AND value = cs_alv-extwg.
    EXIT.
  ENDLOOP.

  CHECK <fs_tax> IS ASSIGNED.

  cs_alv-gruop = <fs_tax>-gruop.
  cs_alv-conf_icbs = <fs_tax>-base.
  cs_alv-conf_icva = <fs_tax>-rate.
  cs_alv-vlr_icms_novo = cs_alv-kwert_icmi * ( cs_alv-conf_icbs / 100 ).
  cs_alv-vlr_icva_novo = cs_alv-vlr_icms_novo * ( cs_alv-conf_icva / 100 ).
  cs_alv-vlr_liq_novo = cs_alv-kwert_icmi - cs_alv-vlr_icva_novo.
  cs_alv-vlr_diferenca = cs_alv-vlr_liq - cs_alv-vlr_liq_novo.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_APLICAR_J1BTAX
*&---------------------------------------------------------------------*
FORM f_aplicar_j1btax2 CHANGING cs_alv TYPE zsds085.

  DATA lv_field TYPE c LENGTH 30.
  DATA lv_field2 TYPE c LENGTH 30.
  DATA lv_field3 TYPE c LENGTH 30.

  PERFORM f_get_j1btax_tab.

  LOOP AT gt_j_1btxgruop ASSIGNING FIELD-SYMBOL(<fs_grp>).

    CHECK <fs_grp>-field IS NOT INITIAL.

    PERFORM f_get_field USING '' cs_alv <fs_grp> CHANGING lv_field.

    PERFORM f_get_field USING '2' cs_alv <fs_grp> CHANGING lv_field2.

    PERFORM f_get_field USING '3' cs_alv <fs_grp> CHANGING lv_field3.

    READ TABLE gt_j_1btxic3v ASSIGNING FIELD-SYMBOL(<fs_tax>)
      WITH KEY shipfrom = cs_alv-shipfrom
               shipto = cs_alv-shipto
               gruop = <fs_grp>-gruop
               value = lv_field
               value2 = lv_field2
               value3 = lv_field3.

    CHECK sy-subrc EQ 0.

    EXIT.

  ENDLOOP.

  CHECK <fs_tax> IS ASSIGNED.

  cs_alv-gruop = <fs_tax>-gruop.
  cs_alv-conf_icbs = <fs_tax>-base.
  cs_alv-conf_icva = <fs_tax>-rate.
  cs_alv-vlr_icms_novo = cs_alv-kwert_icmi * ( cs_alv-conf_icbs / 100 ).
  cs_alv-vlr_icva_novo = cs_alv-vlr_icms_novo * ( cs_alv-conf_icva / 100 ).
  cs_alv-vlr_liq_novo = cs_alv-kwert_icmi - cs_alv-vlr_icva_novo.
  cs_alv-vlr_diferenca = cs_alv-vlr_liq - cs_alv-vlr_liq_novo.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_J1BTAX_TAB
*&---------------------------------------------------------------------*
FORM f_get_j1btax_tab .

  IF gv_cache_j1btax = space OR ( gt_j_1btxgruop IS INITIAL OR gt_j_1btxic3v IS INITIAL ).

    SELECT * FROM j_1btxgruop
      INTO TABLE gt_j_1btxgruop
        WHERE caller = 'SD'.

    SELECT * FROM j_1btxic3
      INTO TABLE gt_j_1btxic3v
        WHERE land1 = 'BR'.
    "AND gruop IN gr_gruop
    "AND value IN ('100','104').

    gv_cache_j1btax = abap_true.

    SORT gt_j_1btxic3v BY gruop ASCENDING.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVAR_9000
*&---------------------------------------------------------------------*
FORM f_gravar_9000 CHANGING cv_erro.

  DATA lv_answer.

  CLEAR cv_erro.

  IF lines( gt_alv_9000_02 ) = 0.
    MESSAGE 'Nenhum registro para ser associado' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM f_popup_to_confirm
    USING 'Confirmar execução?'
 CHANGING lv_answer.

  IF lv_answer <> '1'.
    cv_erro = 'X'.
    EXIT.
  ENDIF.

  PERFORM f_atualiza_price.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm USING p_question TYPE c
                     CHANGING p_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = sy-title
      text_question  = p_question
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

ENDFORM.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_PRICE
*&---------------------------------------------------------------------*
FORM f_atualiza_price .

  DATA lt_insert TYPE TABLE OF zsds085.
  DATA lv_erro TYPE c.

  CLEAR gt_bapiret2.

  LOOP AT gt_alv_9000_02 INTO DATA(ls_9000_02).

    READ TABLE gt_alv_9000_02 ASSIGNING FIELD-SYMBOL(<fs_alv>) INDEX sy-tabix.

    CHECK sy-subrc EQ 0.

    AT NEW objnr.
      CLEAR: lt_insert.
    ENDAT.

    APPEND INITIAL LINE TO lt_insert ASSIGNING FIELD-SYMBOL(<fs_insert>).

    MOVE-CORRESPONDING ls_9000_02 TO <fs_insert>.

    AT END OF objnr.

      PERFORM f_bapi_change_ov_2
        USING <fs_insert>
     CHANGING lv_erro gt_bapiret2[].

      IF lv_erro IS INITIAL.

        " 11.09.2024 - 101482- RAMON -->
        "PERFORM f_cria_reg_j USING lt_insert.
        " 11.09.2024 - 101482 - RAMON --<
      ENDIF.

    ENDAT.

  ENDLOOP.

  " retira mensagens tecnicas, deixa só confirmações ou erro
  DELETE gt_bapiret2 WHERE number = 233.

  PERFORM f_mensagem_exibe_popup USING gt_bapiret2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BAPI_CHANGE_OV
*&---------------------------------------------------------------------*
FORM f_bapi_change_ov USING us_alv_02 TYPE zsds085
                   CHANGING cv_erro TYPE c
                            ct_ret2 TYPE bapiret2_t.

  DATA lt_tkomk TYPE TABLE OF komk.
  DATA lt_tkomv TYPE TABLE OF komv.

  DATA ls_header TYPE bapisdh1.
  DATA ls_logic TYPE bapisdls.
  DATA ls_headerx TYPE bapisdh1x.
  DATA lt_ret TYPE TABLE OF bapiret2.
  DATA lv_dif TYPE kwert.
  DATA lv_valor_coef TYPE kwert.
  DATA lv_rb00_novo TYPE kwert.

  DATA lt_item TYPE TABLE OF bapisditm.
  DATA lt_itemx TYPE TABLE OF bapisditmx.

  DATA lt_cond_in TYPE TABLE OF bapicond.
  DATA lt_condx_in TYPE TABLE OF bapicondx.
  DATA lv_coeficiente TYPE kurrf.

  FIELD-SYMBOLS <fs_konv> TYPE ANY TABLE.
  FIELD-SYMBOLS <fs_komk> TYPE ANY TABLE.

  CLEAR: ls_header, ls_logic, lt_item, lt_itemx, lt_ret, cv_erro.

  BREAK rblima.

  ls_headerx-updateflag = 'U'.
  ls_logic-cond_handl   = 'X'.
  ls_logic-pricing = 'G'.

  lt_item = VALUE #( ( itm_number  = us_alv_02-posnr price_date = sy-datum  ) ).
  lt_itemx = VALUE #( ( itm_number  = us_alv_02-posnr price_date = 'X' updateflag = 'U') ).

  " 02 - Atualiza CONDIÇÕES
  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = us_alv_02-vbeln
      order_header_in  = ls_header
      order_header_inx = ls_headerx
      logic_switch     = ls_logic
    TABLES
      order_item_in    = lt_item
      order_item_inx   = lt_itemx
      return           = lt_ret.

  PERFORM f_bapi_confirm_process CHANGING lt_ret cv_erro.

  CHECK cv_erro IS INITIAL.

  ls_headerx-updateflag = 'U'.
  ls_logic-cond_handl   = 'X'.

  SELECT SINGLE * FROM v_konv
    INTO @DATA(ls_icmi_novo)
      WHERE knumv = @us_alv_02-knumv
        AND kposn = @us_alv_02-posnr
        AND kschl = 'ICMI'.

  " acha diferença
  lv_dif = us_alv_02-kwert_icmi - ls_icmi_novo-kwert.

  "CHECK 1 = 2." teste ramon

  CHECK lv_dif <> 0.

  TRY .
      " acha coeficiente
      lv_coeficiente = zcl_solicitacao_ov=>get_imposto(
        _direcao = 'O'
        _vbeln   = us_alv_02-vbeln
        _posnr   = us_alv_02-posnr
      ).
    CATCH cx_sy_zerodivide.

      lv_coeficiente = 0.

  ENDTRY.


  IF lv_coeficiente > 0.

    lv_valor_coef = lv_dif * lv_coeficiente.

    lv_rb00_novo = lv_valor_coef + us_alv_02-kwert_rb00.

  ELSE.

    lv_rb00_novo = 0.

  ENDIF.

  CLEAR: lt_cond_in, lt_condx_in, lt_ret.

  ls_headerx-updateflag = 'U'.
  ls_logic-cond_handl   = 'X'.
  ls_logic-pricing = space.

  APPEND VALUE #(
                itm_number = us_alv_02-posnr
                cond_count = '01'
                cond_type  = 'RB00'
                cond_value = lv_rb00_novo
                "cond_unit  = ls_9000_02-gewei
                currency   = us_alv_02-waerk
              ) TO lt_cond_in[].

  APPEND VALUE #(
                  itm_number = us_alv_02-posnr
                  cond_count = '01'
                  cond_type  = 'RB00'
                  cond_value = abap_true
                  cond_unit  = abap_false
                  updateflag = 'U'
                  currency   = abap_true
                ) TO lt_condx_in[].


  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = us_alv_02-vbeln
      order_header_in  = ls_header
      order_header_inx = ls_headerx
      logic_switch     = ls_logic
    TABLES
      return           = lt_ret
*     ORDER_ITEM_IN    =
*     ORDER_ITEM_INX   =
      conditions_in    = lt_cond_in
      conditions_inx   = lt_condx_in.

  PERFORM f_bapi_confirm_process CHANGING lt_ret cv_erro.

  APPEND LINES OF lt_ret TO ct_ret2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BAPI_CHANGE_OV_2
*&---------------------------------------------------------------------*
FORM f_bapi_change_ov_2 USING us_alv_02 TYPE zsds085
                   CHANGING cv_erro TYPE c
                            ct_ret2 TYPE bapiret2_t.

  BREAK rblima.

  CALL FUNCTION 'ZSDMF_INSUMOS_ATU_IMPOSTOS_OV'
    EXPORTING
      iv_vbeln      = us_alv_02-vbeln
      iv_posnr      = us_alv_02-posnr
      iv_matnr      = us_alv_02-matnr
      iv_qtde_venda = us_alv_02-kwmeng
      iv_un_venda   = us_alv_02-gewei
      iv_un_preco   = us_alv_02-gewei_pr00
      iv_icbs_novo  = CONV bapikbetr1( us_alv_02-conf_icbs )
      iv_icva_novo  = CONV bapikbetr1( us_alv_02-conf_icva )
      iv_icmi_atual = CONV bapikbetr1( us_alv_02-kwert_icmi )
    TABLES
      et_return     = ct_ret2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_bapi_confirm_process
*&---------------------------------------------------------------------*
FORM f_bapi_confirm_process CHANGING ct_return TYPE bapiret2_tt
                                     cv_erro TYPE c.

  READ TABLE ct_return TRANSPORTING NO FIELDS
    WITH KEY type = 'E'.

  IF sy-subrc NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    cv_erro = abap_false.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    cv_erro = abap_true.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CRIA_REG_J
*&---------------------------------------------------------------------*
FORM f_cria_reg_j USING ut_tab TYPE zsdc085.

  DATA lt_0090 TYPE TABLE OF zsdt0090.
  DATA lv_seq TYPE numc4.

  LOOP AT ut_tab ASSIGNING FIELD-SYMBOL(<fs_line>).

    APPEND INITIAL LINE TO lt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>).

    IF lv_seq IS INITIAL.

      PERFORM f_get_next_seq
        USING <fs_line>-doc_simulacao
     CHANGING lv_seq.

    ELSE.

      ADD 1 TO lv_seq.

    ENDIF.

    <fs_0090>-doc_simulacao = <fs_line>-doc_simulacao.
    <fs_0090>-sequencia = lv_seq.
    <fs_0090>-auart = 'ZFTE'.
    <fs_0090>-vbelv = <fs_line>-vbeln.
    "<fs_0090>-vbeln = <fs_line>-vbeln.
    <fs_0090>-posnn = <fs_line>-posnr.
    <fs_0090>-posnv = <fs_line>-posnr.
    <fs_0090>-zmeng = <fs_line>-kwmeng.
    <fs_0090>-zieme = <fs_line>-gewei.
    <fs_0090>-kmein = <fs_line>-gewei.
    <fs_0090>-matnr = <fs_line>-matnr.
    <fs_0090>-matkl = <fs_line>-maktx.
    <fs_0090>-werks = <fs_line>-werks.
    <fs_0090>-categoria = 'J'.
    <fs_0090>-usnam = sy-uname.
    <fs_0090>-data_atual = sy-datum.
    <fs_0090>-hora_atual = sy-uzeit.

  ENDLOOP.

  CHECK lt_0090 IS NOT INITIAL.

  MODIFY zsdt0090 FROM TABLE lt_0090.

  COMMIT WORK AND WAIT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SEQ_TRAVA
*&---------------------------------------------------------------------*
FORM f_get_next_seq USING uv_doc_simu TYPE zsded003
                CHANGING cv_seq TYPE numc4.

  SELECT MAX( sequencia ) AS max FROM zsdt0090
    INTO @DATA(lv_max)
    WHERE doc_simulacao = @uv_doc_simu.

  cv_seq = lv_max + 1.

  UNPACK cv_seq TO cv_seq.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_EXIBE_POPUP
*&---------------------------------------------------------------------*
FORM f_mensagem_exibe_popup USING p_bapiret2_tab TYPE bapiret2_t.

  DATA: l_lines TYPE i.

  DESCRIBE TABLE p_bapiret2_tab LINES l_lines.

  IF l_lines <= 1 OR sy-batch = 'X'.

    LOOP AT p_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_ret2>).

      MESSAGE ID <fs_ret2>-id
            TYPE 'S'
          NUMBER <fs_ret2>-number
            WITH <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4 DISPLAY LIKE <fs_ret2>-type.

    ENDLOOP.

  ELSE.

    CALL FUNCTION 'MESSAGES_INITIALIZE'.

    LOOP AT p_bapiret2_tab ASSIGNING <fs_ret2>.

      IF <fs_ret2>-id IS INITIAL.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      ENDIF.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = <fs_ret2>-id
          "EXCEPTION_IF_NOT_ACTIVE  = 'X'
          msgty                  = <fs_ret2>-type
          msgv1                  = <fs_ret2>-message_v1
          msgv2                  = <fs_ret2>-message_v2
          msgv3                  = <fs_ret2>-message_v3
          msgv4                  = <fs_ret2>-message_v4
          txtnr                  = <fs_ret2>-number
          "ZEILE                    = ' '
          "IMPORTING
          "ACT_SEVERITY             =
          "MAX_SEVERITY             =
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.     "#EC CI_SUBRC

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4
        OTHERS    = 5.     "#EC CI_SUBRC

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        "CORRECTIONS_OPTION          = ' '
        "CORRECTIONS_FUNC_TEXT       = ' '
        "LINE_FROM                   = ' '
        "LINE_TO                     = ' '
        "OBJECT                      = ' '
        "SEND_IF_ONE                 = ' '
        batch_list_type     = 'B'
        show_linno          = ' '
        show_linno_text     = 'X'
        show_linno_text_len = '3'
        i_use_grid          = ' '
        i_amodal_window     = ' '
        "MSG_SELECT_FUNC             = ' '
        "MSG_SELECT_FUNC_TEXT        = ' '
        "IMPORTING
        "CORRECTIONS_WANTED          =
        "E_EXIT_COMMAND              =
        "MSG_SELECTED                =
      EXCEPTIONS
        inconsistent_range  = 1
        no_messages         = 2
        OTHERS              = 3.     "#EC CI_SUBRC

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_retira_faturados
*&---------------------------------------------------------------------*
FORM f_retira_faturados CHANGING ct_items TYPE zsdc086.

  DATA(lt_aux) = ct_items.

  DELETE ADJACENT DUPLICATES FROM ct_items COMPARING vbeln posnr matnr.

  LOOP AT ct_items ASSIGNING FIELD-SYMBOL(<fs_item>).

    DATA(lv_index) = sy-tabix.

    " se tiver algum registro como 'J', está faturado, tem que sair
    IF line_exists( lt_aux[ vbeln = <fs_item>-vbeln posnr = <fs_item>-posnr vbtyp_n = 'J' ] ).
      DELETE ct_items INDEX lv_index.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_SET
*&---------------------------------------------------------------------*
FORM f_carrega_set  USING p_setnr TYPE c
                    CHANGING p_set_tab TYPE rgsbv_tab.

  DATA lv_setnr TYPE c LENGTH 50.

  lv_setnr = '0000' && p_setnr.

  CALL FUNCTION 'G_SET_FETCH'
    EXPORTING
      setnr           = lv_setnr
    TABLES
      set_lines_basic = p_set_tab
    EXCEPTIONS
      no_authority    = 1
      set_is_broken   = 2
      set_not_found   = 3
      OTHERS          = 4.

  IF sy-subrc <> 0.
    "p_erro = 'X'.
    "EXIT.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_carrega_grupos
*&---------------------------------------------------------------------*
FORM f_carrega_grupos USING ut_set_in TYPE rgsbv_tab
                            ut_set_ex TYPE rgsbv_tab.

  CLEAR: gr_gruop, gr_gruop_in, gr_gruop_exc.

  LOOP AT ut_set_in ASSIGNING FIELD-SYMBOL(<fs_set>).

    APPEND INITIAL LINE TO gr_gruop ASSIGNING FIELD-SYMBOL(<fs_gruop>).

    <fs_gruop>-sign = 'I'.
    <fs_gruop>-option = 'EQ'.
    <fs_gruop>-low = <fs_set>-from.

    APPEND <fs_gruop> TO gr_gruop_in.

  ENDLOOP.

  LOOP AT ut_set_ex ASSIGNING <fs_set>.

    APPEND INITIAL LINE TO gr_gruop ASSIGNING <fs_gruop>.

    <fs_gruop>-sign = 'I'.
    <fs_gruop>-option = 'EQ'.
    <fs_gruop>-low = <fs_set>-from.

    APPEND <fs_gruop> TO gr_gruop_exc.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_aplicar_regra_ex
*&---------------------------------------------------------------------*
FORM f_aplicar_regra_ex USING ut_tab TYPE rgsbv_tab
                     CHANGING cs_alv TYPE zsds085.

  DATA lv_field TYPE c LENGTH 40.

  LOOP AT ut_tab ASSIGNING FIELD-SYMBOL(<fs_tab>).

    CHECK cs_alv-gruop = <fs_tab>-from.

    READ TABLE gt_j_1btxic3v ASSIGNING FIELD-SYMBOL(<fs_line>)
      WITH KEY shipfrom = cs_alv-shipfrom
               shipto   = cs_alv-shipto
               gruop    = cs_alv-gruop.

    CHECK sy-subrc EQ 0.

    lv_field = 'CS_ALV-' && <fs_tab>-title.

    ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_field>).

    CHECK sy-subrc EQ 0.

    CHECK <fs_line>-value2 = <fs_field>.

    cs_alv-status_atu = gc_conf.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_zera_rb00
*&---------------------------------------------------------------------*
FORM f_bapi_zera_rb00 USING cv_vbeln TYPE vbeln
                            cv_posnr TYPE posnr
                            cv_waerk TYPE waerk
                   CHANGING cv_erro TYPE c
                            ct_ret2 TYPE bapiret2_t.

  DATA ls_header TYPE bapisdh1.
  DATA ls_logic TYPE bapisdls.
  DATA ls_headerx TYPE bapisdh1x.

  DATA lt_ret TYPE TABLE OF bapiret2.
  DATA lt_item TYPE TABLE OF bapisditm.
  DATA lt_itemx TYPE TABLE OF bapisditmx.

  DATA lt_cond_in TYPE TABLE OF bapicond.
  DATA lt_condx_in TYPE TABLE OF bapicondx.

  ls_headerx-updateflag = 'U'.
  ls_logic-cond_handl   = 'X'.
  ls_logic-pricing = space.

  APPEND VALUE #(
                itm_number = cv_posnr
                cond_count = '01'
                cond_type  = 'RB00'
                cond_value = 0
                "cond_unit  = ls_9000_02-gewei
                currency   = cv_waerk
              ) TO lt_cond_in[].

  APPEND VALUE #(
                  itm_number = cv_posnr
                  cond_count = '01'
                  cond_type  = 'RB00'
                  cond_value = abap_true
                  cond_unit  = abap_false
                  updateflag = 'U'
                  currency   = abap_true
                ) TO lt_condx_in[].


  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = cv_vbeln
      order_header_in  = ls_header
      order_header_inx = ls_headerx
      logic_switch     = ls_logic
    TABLES
      return           = lt_ret
*     ORDER_ITEM_IN    =
*     ORDER_ITEM_INX   =
      conditions_in    = lt_cond_in
      conditions_inx   = lt_condx_in.

  PERFORM f_bapi_confirm_process CHANGING lt_ret cv_erro.

  APPEND LINES OF lt_ret TO ct_ret2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_field
*&---------------------------------------------------------------------*
FORM f_get_field USING uv_field_code TYPE c
                       us_alv TYPE zsds085
                       us_line TYPE j_1btxgruop
              CHANGING cv_field TYPE c.

  DATA lv_fieldname TYPE c LENGTH 20.
  DATA lv_grp_name TYPE c LENGTH 20.

  CLEAR cv_field.

  lv_grp_name = 'US_LINE-FIELD' && uv_field_code.

  ASSIGN (lv_grp_name) TO FIELD-SYMBOL(<fs_grp_field>).

  CHECK <fs_grp_field> IS ASSIGNED.

  CHECK <fs_grp_field> IS NOT INITIAL.

  lv_fieldname = 'US_ALV-' && <fs_grp_field>.

  ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<fs_return>).

  CHECK <fs_return> IS ASSIGNED.

  CHECK <fs_return> IS NOT INITIAL.

  cv_field = <fs_return>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preenche_x
*&---------------------------------------------------------------------*
FORM f_preenche_x USING us_data TYPE any
                        uv_upt_flag TYPE updkz_d
               CHANGING cs_datax TYPE any.

  DATA: datatype TYPE REF TO cl_abap_datadescr,
        field(5) TYPE c.
  DATA: linetype TYPE REF TO cl_abap_structdescr,
        mystruc  TYPE spfli.

  DATA lv_length TYPE i.

  ASSIGN ('US_DATA') TO FIELD-SYMBOL(<fs_data>).

  CHECK sy-subrc EQ 0.

  linetype ?= cl_abap_typedescr=>describe_by_data( <fs_data> ).

  LOOP AT linetype->components ASSIGNING FIELD-SYMBOL(<fs_component>).

    DATA(lv_campo) = 'US_DATA-' && <fs_component>-name.
    DATA(lv_campox) = 'CS_DATAX-' && <fs_component>-name.

    ASSIGN (lv_campo) TO FIELD-SYMBOL(<fs_campo>).

    CHECK sy-subrc EQ 0.

    ASSIGN (lv_campox) TO FIELD-SYMBOL(<fs_campox>).

    CHECK <fs_campox> IS ASSIGNED.

    IF <fs_campo> IS NOT INITIAL.

      DESCRIBE FIELD <fs_campox> LENGTH lv_length IN CHARACTER MODE.

      IF lv_length = 1.
        <fs_campox> = 'X'.
      ELSE.
        <fs_campox> = <fs_campo>.
      ENDIF.

    ENDIF.

  ENDLOOP.

  CHECK uv_upt_flag IS NOT INITIAL.

  lv_campox = 'CS_DATAX-UPDATEFLAG'.

  ASSIGN (lv_campox) TO <fs_campox>.

  CHECK sy-subrc EQ 0.

  <fs_campox> = uv_upt_flag.


ENDFORM.
