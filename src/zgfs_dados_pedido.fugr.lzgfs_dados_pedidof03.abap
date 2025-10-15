*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDO04.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_1600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1600 OUTPUT.

  DATA: it_pf1600 TYPE TABLE OF sy-ucomm.

  IF sb_tela_1600 IS INITIAL.
    sb_tela_1600 = tl_tela_1601.
  ENDIF.

  SET PF-STATUS 'PF1600' EXCLUDING it_pf1600.
  SET TITLEBAR 'TL1600'.

  IF cl_grid_1601 IS NOT INITIAL.
    CALL METHOD cl_grid_1601->refresh_table_display
      EXPORTING
        is_stable = wa_stable_1601.
  ENDIF.

  IF cl_grid_1602 IS NOT INITIAL.
    CALL METHOD cl_grid_1602->refresh_table_display
      EXPORTING
        is_stable = wa_stable_1602.
  ENDIF.

  IF cl_grid_1603 IS NOT INITIAL.
    CALL METHOD cl_grid_1603->refresh_table_display
      EXPORTING
        is_stable = wa_stable_1603.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1601  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1601 OUTPUT.

  IF sb_tela_1601 IS INITIAL.
    sb_tela_1601 = tl_tela_1602.
  ENDIF.

  "Criar ALV Itens
  IF container_1601 IS INITIAL.

    CLEAR wa_layout_1601.
    wa_layout_1601-zebra      = abap_true.
    wa_stable_1601-row        = abap_true.
    wa_stable_1601-col        = abap_true.
    wa_layout_1601-stylefname = 'STYLE'.
    wa_layout_1601-sel_mode   = 'A'.
    wa_layout_1601-cwidth_opt = ' '.
    wa_layout_1601-col_opt    = ' '.
    wa_layout_1601-info_fname = 'LINE_COLOR'.
    wa_layout_1601-ctab_fname = 'COLOR_CELL'.
    wa_layout_1601-no_toolbar = abap_false.

    CREATE OBJECT container_1601
      EXPORTING
        container_name = 'ALV_ITENS'.

    PERFORM fill_it_fieldcatalog_1601.

*   Fill info for layout variant
    PERFORM fill_gs_variant_1601.

    CREATE OBJECT cl_grid_1601
      EXPORTING
        i_parent = container_1601.

    CREATE OBJECT obg_toolbar_1601
      EXPORTING
        io_alv_grid = cl_grid_1601.

    cl_grid_1601->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    cl_grid_1601->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    SET HANDLER obg_toolbar_1601->on_toolbar FOR cl_grid_1601.
    SET HANDLER obg_toolbar_1601->handle_user_command FOR cl_grid_1601.

    CREATE OBJECT event_receiver_1601.
    SET HANDLER:  event_receiver_1601->handle_double_click_1601  FOR cl_grid_1601.
    SET HANDLER:  event_receiver_1601->handle_data_changed_1601  FOR cl_grid_1601.

    CALL METHOD cl_grid_1601->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_1601
        i_save                        = 'A'
        is_layout                     = wa_layout_1601
        it_toolbar_excluding          = it_function_1601
      CHANGING
        it_outtab                     = it_itens_alv[]
        it_fieldcatalog               = it_fieldcat_1601
        it_sort                       = it_sort_1601
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  CALL METHOD cl_grid_1601->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1601.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1600 INPUT.

  DATA: e_gerou      TYPE  char01,
        e_mblnr      TYPE  mblnr,
        e_mjahr      TYPE  mjahr,
        e_docnum     TYPE  j_1bdocnum,
        e_mblnr_dev  TYPE  mblnr,
        e_mjahr_dev  TYPE  mjahr,
        e_docnum_dev TYPE  j_1bdocnum,
        e_belnr_dev  TYPE  re_belnr,
        e_gjahr_dev  TYPE  gjahr,
        l_valida_qtd TYPE string,
        l_erro       TYPE char01,
        ls_opt       TYPE ctu_params.

  FREE: l_erro,
        gt_log_remessa[].

  CASE ok_code.

    WHEN 'GERAR_REMESSA'.
      PERFORM f_validar_dados CHANGING l_erro.
      IF l_erro = abap_false.
        PERFORM f_gerar_remessa_novo.
        PERFORM f_exibi_popup_erro.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*----------------------------------------------------------------------*
* validar dados
*----------------------------------------------------------------------*
FORM f_validar_dados CHANGING p_erro.

  DATA: l_quant      TYPE j_1bnetqty,
        l_tot_menge  TYPE ekpo-menge,
        nlinhas      TYPE i,
        v_due_date   TYPE ledat,
        v_deliv_numb TYPE bapishpdelivnumb-deliv_numb,
        v_erro       TYPE c LENGTH 1,
        v_vgpos      TYPE lips-vgpos,
        v_tabix      TYPE sy-tabix,
        v_route1     TYPE ekpv-route,
        v_route2     TYPE ekpv-route,
        w_ekpo1      TYPE ekpo,
        w_ekpo2      TYPE ekpo,
        w_eket1      TYPE eket,
        w_eket2      TYPE eket.

  DELETE it_lotes_alv_t WHERE chave_nfe IS INITIAL.

  LOOP AT it_itens_alv.

    SELECT SINGLE *
      FROM ekko
      INTO @DATA(_ekko)
     WHERE ebeln = @it_itens_alv-ebeln
       AND bsart = 'ZUB'.

    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH TEXT-003  DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE menge
      FROM ekpo
      INTO @DATA(l_ekpo_menge)
     WHERE ebeln EQ @it_itens_alv-ebeln
       AND ebelp EQ @it_itens_alv-ebelp.

    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH TEXT-003  DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT SUM( menge )
      FROM ekbe
      INTO @DATA(l_ekbe_menge)
     WHERE ebeln EQ @it_itens_alv-ebeln
       AND ebelp EQ @it_itens_alv-ebelp
       AND vgabe EQ '8'.

    l_tot_menge = l_ekpo_menge - l_ekbe_menge.
    IF it_itens_alv-menge GT l_tot_menge.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH TEXT-002  DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    FREE: l_quant.
    LOOP AT it_lotes_alv_t INTO DATA(w_lotes) WHERE chave_nfe = it_itens_alv-chave_nfe.
      l_quant = l_quant + w_lotes-menge.
    ENDLOOP.

    IF it_lotes_alv_t[] IS NOT INITIAL AND l_quant <> it_itens_alv-menge.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Quantidades na Particao do Lote esta Incorreta!' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.

  LOOP AT it_itens_alv WHERE NOT ebeln IS INITIAL.
  ENDLOOP.

  SELECT SINGLE route FROM ekpv INTO v_route1
          WHERE ebeln = it_itens_alv-ebeln
            AND ebelp = it_itens_alv-ebelp.

  SELECT SINGLE * FROM ekpo INTO w_ekpo1
          WHERE ebeln = it_itens_alv-ebeln
            AND ebelp = it_itens_alv-ebelp.

  SELECT SINGLE * FROM eket INTO w_eket1
          WHERE ebeln = it_itens_alv-ebeln
            AND ebelp = it_itens_alv-ebelp.

  LOOP AT it_itens_alv.

    SELECT SINGLE route FROM ekpv INTO v_route2
            WHERE ebeln = it_itens_alv-ebeln
              AND ebelp = it_itens_alv-ebelp.

    IF v_route1 NE v_route2.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH TEXT-004 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM ekpo INTO w_ekpo2
            WHERE ebeln = it_itens_alv-ebeln
              AND ebelp = it_itens_alv-ebelp.

    IF  w_ekpo1-inco1 NE  w_ekpo2-inco1 OR
        w_ekpo1-inco2 NE  w_ekpo2-inco2.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH TEXT-005 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM eket INTO w_eket2
            WHERE ebeln = it_itens_alv-ebeln
              AND ebelp = it_itens_alv-ebelp.

*   IF w_eket1-eindt NE w_eket2-eindt.
*     p_erro = abap_true.
*     MESSAGE s024(sd) WITH TEXT-006 DISPLAY LIKE 'E'.
*     RETURN.
*   ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1601
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1601 .

  wa_variant_1601-report      = sy-repid.
  wa_variant_1601-handle      = '1601'.
  wa_variant_1601-log_group   = abap_false.
  wa_variant_1601-username    = abap_false.
  wa_variant_1601-variant     = abap_false.
  wa_variant_1601-text        = abap_false.
  wa_variant_1601-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1601
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1601 .

  DATA: i_contador_2 TYPE lvc_colpos.

  CLEAR: it_fieldcat_1601[], it_fieldcat_1601.

* CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*   EXPORTING
*     i_structure_name = 'ZDE_NFE_DIST_ITM_ALV'
*   CHANGING
*     ct_fieldcat      = it_fieldcat_1601.

  wa_fieldcat_1601-fieldname = 'EBELN'.
  APPEND wa_fieldcat_1601 TO it_fieldcat_1601.
  wa_fieldcat_1601-fieldname = 'EBELP'.
  APPEND wa_fieldcat_1601 TO it_fieldcat_1601.
  wa_fieldcat_1601-fieldname = 'MENGE'.
  APPEND wa_fieldcat_1601 TO it_fieldcat_1601.
  wa_fieldcat_1601-fieldname = 'MEINS'.
  APPEND wa_fieldcat_1601 TO it_fieldcat_1601.
  wa_fieldcat_1601-fieldname = 'MATNR'.
  APPEND wa_fieldcat_1601 TO it_fieldcat_1601.
  wa_fieldcat_1601-fieldname = 'MAKTX'.
  APPEND wa_fieldcat_1601 TO it_fieldcat_1601.
  wa_fieldcat_1601-fieldname = 'LGORT'.
  APPEND wa_fieldcat_1601 TO it_fieldcat_1601.
  wa_fieldcat_1601-fieldname = 'CHARG'.
  APPEND wa_fieldcat_1601 TO it_fieldcat_1601.

  LOOP AT it_fieldcat_1601 ASSIGNING FIELD-SYMBOL(<fs_1601>).
    <fs_1601>-edit = abap_false.
    CASE <fs_1601>-fieldname.
      WHEN 'EBELN'.
        <fs_1601>-col_pos = 1.
        <fs_1601>-outputlen = 10.
        <fs_1601>-coltext   = 'Pedido'.
        <fs_1601>-edit      = abap_true.
      WHEN 'EBELP'.
        <fs_1601>-col_pos   = 2.
        <fs_1601>-outputlen = 12.
        <fs_1601>-coltext   = 'Item Pedido'.
        <fs_1601>-edit      = abap_true.
      WHEN 'MENGE'.
        <fs_1601>-col_pos   = 3.
        <fs_1601>-outputlen = 15.
        <fs_1601>-coltext   = 'Quantidade'.
        <fs_1601>-ref_table = 'EKPO'.
        <fs_1601>-ref_field = 'MENGE'.
        <fs_1601>-edit      = abap_true.
      WHEN 'MEINS'.
        <fs_1601>-col_pos = 4.
        <fs_1601>-outputlen = 08.
        <fs_1601>-coltext   = 'Un.Med'.
      WHEN 'MATNR'.
        <fs_1601>-col_pos = 5.
        <fs_1601>-outputlen = 12.
        <fs_1601>-coltext   = 'Material'.
        <fs_1601>-ref_table = 'MARA'.
        <fs_1601>-ref_field = 'MATNR'.
      WHEN 'MAKTX'.
        <fs_1601>-col_pos = 5.
        <fs_1601>-outputlen = 40.
        <fs_1601>-coltext   = 'Descrição'.
      WHEN 'LGORT'.
        <fs_1601>-col_pos = 6.
        <fs_1601>-outputlen = 12.
        <fs_1601>-coltext   = 'Deposito'.
      WHEN 'CHARG'.
        <fs_1601>-col_pos = 7.
        <fs_1601>-outputlen = 12.
        <fs_1601>-coltext   = 'Lote'.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_1601
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM double_click_1601  USING    p_e_row     TYPE lvc_s_row
                                 p_e_column  TYPE lvc_s_col
                                 p_es_row_no TYPE lvc_s_roid.

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl,
        w_item             TYPE zde_nfe_dist_itm_alv,
        w_lote             TYPE zib_nfe_dist_lot,
        t_lote             TYPE zib_nfe_dist_lot_t.

  CLEAR: wa_itens_sel_lote.
  CLEAR: it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u, it_lotes_alv_u, it_lotes_alv_u[].

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  IF p_e_row-rowtype IS INITIAL.

    READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX p_e_row-index.
    CHECK <fs_item> IS ASSIGNED.

    wa_itens_sel_lote = <fs_item>.

    IF <fs_item>-erro = abap_true.
      MESSAGE s024(sd) WITH 'Linha Selecionada está com Erro!' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CHECK <fs_item>-chave_nfe IS NOT INITIAL.

    FREE: <fs_item>-style.
    wa_style-fieldname = 'EBELN'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
    INSERT wa_style INTO TABLE <fs_item>-style[].
    wa_style-fieldname = 'EBELP'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
    INSERT wa_style INTO TABLE <fs_item>-style[].
    wa_style-fieldname = 'MENGE'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
    INSERT wa_style INTO TABLE <fs_item>-style[].

*   CALL METHOD cl_grid_1601->refresh_table_display
*     EXPORTING
*       is_stable      = gs_alv_refres_cond
*       i_soft_refresh = abap_true.

    READ TABLE it_lotes_alv_t WITH KEY chave_nfe = <fs_item>-chave_nfe
                                       prod_item = <fs_item>-prod_item
                              INTO DATA(wa_lotes_alv).
    IF sy-subrc IS NOT INITIAL.
      MOVE-CORRESPONDING <fs_item> TO w_item.
      lc_frete_remessa_trans->add_item( w_item ).
      PERFORM get_info_tela.
      PERFORM incluir_lote.
    ELSE.
      MOVE-CORRESPONDING <fs_item> TO w_item.
      lc_frete_remessa_trans->add_item( w_item ).

      LOOP AT  it_lotes_alv_t          INTO wa_lotes_alv WHERE chave_nfe = <fs_item>-chave_nfe
                                                           AND prod_item = <fs_item>-prod_item.
        MOVE-CORRESPONDING wa_lotes_alv  TO w_lote.
        APPEND w_lote      TO t_lote.
      ENDLOOP.
      lc_frete_remessa_trans->add_lote( t_lote ).

      PERFORM get_info_tela.
      LOOP AT it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_limpar>) WHERE chave_nfe NE <fs_item>-chave_nfe
                                                                  OR prod_item NE <fs_item>-prod_item.
        CLEAR: <fs_limpar>-line_color.
      ENDLOOP.

      READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item2>) INDEX p_e_row-index.
      IF ( sy-subrc IS INITIAL ). " AND ( <fs_item2>-line_color NE cs_line_color_selecionada ).
        <fs_item2>-line_color = cs_line_color_selecionada.
        PERFORM setar_lotes_linha USING <fs_item2>.
        "LEAVE TO SCREEN 1600.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL METHOD cl_grid_1602->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1602.

  CALL METHOD cl_grid_1603->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1603.

ENDFORM.

FORM data_changed_1601 USING  rr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_mod_cells       TYPE lvc_s_modi,
        ls_cells           TYPE lvc_s_modi,
        l_matnr            TYPE matnr,
        l_ebeln            TYPE ebeln,
        l_ebelp	           TYPE ebelp,
        l_menge            TYPE ekpo-menge,
        l_meins            TYPE j_1bnetunt,
        l_netpr            TYPE j_1bnetpri,
        l_netwr            TYPE j_1bnetval,
        w_ekko             TYPE ekko,
        w_ekpo             TYPE ekpo,
        w_ekbe             TYPE ekbe,
        w_eket             TYPE eket,
        w_mara             TYPE mara,
        w_marc             TYPE marc,
        w_makt             TYPE makt,
        vl_tot_menge       TYPE ekpo-menge,
        vl_ekbe_menge      TYPE ekpo-menge,
        w_zib_nfe_dist_itm TYPE zib_nfe_dist_itm.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.

    READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX ls_mod_cells-row_id.

    CASE ls_mod_cells-fieldname.

      WHEN 'EBELN' OR 'EBELP'.
        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'EBELN' "ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_ebeln.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'EBELP' "ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_ebelp.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'MENGE' "ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_menge.

        IF l_ebeln IS NOT INITIAL AND l_ebelp IS NOT INITIAL.
          CLEAR: w_ekko, w_ekpo, w_ekbe, w_mara, w_marc, w_makt, vl_ekbe_menge.

          SELECT SINGLE * INTO w_ekko FROM ekko WHERE ebeln EQ l_ebeln.
          SELECT SINGLE * INTO w_ekpo FROM ekpo WHERE ebeln EQ l_ebeln
                                                  AND ebelp EQ l_ebelp.

          IF w_ekpo IS INITIAL.
            MESSAGE s024(sd) WITH 'Pedido/Item não Localizado!' DISPLAY LIKE 'E'.
            <fs_item>-erro = abap_true.
            EXIT.
          ELSEIF w_ekko-bsart <> 'ZUB'.
            MESSAGE s024(sd) WITH 'Pedido deve ser Tipo ZUB!' DISPLAY LIKE 'E'.
            <fs_item>-erro = abap_true.
            EXIT.
          ELSE.
            <fs_item>-erro = abap_false.
          ENDIF.

          SELECT SINGLE * INTO w_eket FROM eket WHERE ebeln EQ l_ebeln
                                                  AND ebelp EQ l_ebelp.
          SELECT SINGLE * INTO w_mara FROM mara WHERE matnr EQ w_ekpo-matnr.

          SELECT SINGLE * INTO w_makt FROM makt WHERE spras EQ sy-langu
                                                  AND matnr EQ w_mara-matnr.

          SELECT SINGLE * INTO w_marc FROM marc WHERE matnr EQ w_ekpo-matnr
                                                  AND werks EQ w_ekpo-werks.
          IF w_marc-xchpf = abap_off.
            MESSAGE i024(sd) WITH 'Material não exige Lote!'. " DISPLAY LIKE 'E'.
            <fs_item>-erro = abap_true.
          ENDIF.

          SELECT SUM( menge )
                 FROM ekbe
                 INTO vl_ekbe_menge
                WHERE ebeln EQ l_ebeln
                  AND ebelp EQ l_ebelp
                  AND vgabe EQ '8'.

*         CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
*           EXPORTING
*             input  = w_mara-matnr
*           IMPORTING
*             output = w_mara-matnr.

          IF <fs_item> IS ASSIGNED.
            <fs_item>-chave_nfe = w_ekko-ebeln &&  w_ekpo-ebelp.
            <fs_item>-prod_item = 1.
            <fs_item>-ebeln     = w_ekko-ebeln.
            <fs_item>-ebelp     = w_ekpo-ebelp.
            <fs_item>-meins     = w_ekpo-meins.
            <fs_item>-matnr     = w_mara-matnr.
            <fs_item>-maktx     = w_makt-maktx.
            <fs_item>-charg     = w_eket-charg.
            <fs_item>-lgort     = w_ekpo-lgort.
            <fs_item>-menge     = 0.

            vl_tot_menge        = w_ekpo-menge - vl_ekbe_menge.

            IF l_menge LT vl_tot_menge AND l_menge GT 0.
              <fs_item>-menge = l_menge.
            ELSE.
              <fs_item>-menge = vl_tot_menge.
            ENDIF.

            CALL METHOD rr_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_mod_cells-row_id
                i_fieldname = 'MENGE' "ls_mod_cells-fieldname
                i_value     = <fs_item>-menge.
          ENDIF.
        ENDIF.

      WHEN 'MENGE'.
        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'EBELN' "ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_ebeln.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'EBELP' "ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_ebelp.

        CALL METHOD rr_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'MENGE' "ls_mod_cells-fieldname
          IMPORTING
            e_value     = l_menge.

        IF l_ebeln IS NOT INITIAL AND l_ebelp IS NOT INITIAL.
          CLEAR: w_ekko, w_ekpo, w_ekbe, w_mara, w_makt, vl_ekbe_menge.

          SELECT SINGLE * INTO w_ekko FROM ekko WHERE ebeln EQ l_ebeln.
          SELECT SINGLE * INTO w_ekpo FROM ekpo WHERE ebeln EQ l_ebeln
                                                  AND ebelp EQ l_ebelp.
          SELECT SINGLE * INTO w_ekbe FROM ekbe WHERE ebeln EQ l_ebeln
                                                  AND ebelp EQ l_ebelp.

          vl_tot_menge = w_ekpo-menge - vl_ekbe_menge.

          IF l_menge LT vl_tot_menge AND l_menge GT 0.
            <fs_item>-menge = l_menge.
          ELSE.
            <fs_item>-menge = vl_tot_menge.
          ENDIF.

          CALL METHOD rr_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_mod_cells-row_id
              i_fieldname = 'MENGE' "ls_mod_cells-fieldname
              i_value     = <fs_item>-menge.

        ENDIF.
    ENDCASE.
  ENDLOOP.

  CALL METHOD cl_grid_1601->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1601.

ENDFORM.                               " data_changed_1502

*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_1601
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM double_click_1603  USING    p_e_row     TYPE lvc_s_row
                                 p_e_column  TYPE lvc_s_col
                                 p_es_row_no TYPE lvc_s_roid.
  IF p_e_row-rowtype IS INITIAL.
    READ TABLE it_lotes_alv_u INDEX p_e_row-index INTO DATA(wa_lotes_alv_u).
    PERFORM selecionar_lote USING wa_itens_sel_lote-chave_nfe wa_itens_sel_lote-prod_item wa_lotes_alv_u-cd_lote_item abap_true.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SETAR_LOTES_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ITEM>  text
*----------------------------------------------------------------------*
FORM setar_lotes_linha  USING  p_item TYPE ty_itens_alv.

  MOVE p_item TO wa_itens_sel_lote.

  "Seleciona o primeiro lote do item
  READ TABLE it_lotes_alv_t WITH KEY chave_nfe = p_item-chave_nfe
                                     prod_item = p_item-prod_item
                          INTO DATA(wa_lotes_alv).
  IF sy-subrc IS INITIAL.
    PERFORM selecionar_lote USING p_item-chave_nfe p_item-prod_item wa_lotes_alv-cd_lote_item abap_true.
  ELSE.
    "Não tem um Lote
    PERFORM incluir_lote.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1602  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1602 OUTPUT.

  DATA: it_lote_caract TYPE zib_nfe_dist_lca_t,
        wa_lote_caract TYPE zib_nfe_dist_lca.

  CLEAR: it_lote_caract, wa_lote_caract.

  IF ck_alterou_lote_info EQ abap_true OR ck_alterou_lote EQ abap_true.
    CLEAR: it_lote_caract.
    LOOP AT it_carac_alv_u.
      MOVE-CORRESPONDING it_carac_alv_u TO wa_lote_caract.
      APPEND wa_lote_caract TO it_lote_caract.
    ENDLOOP.

    lc_frete_remessa_trans->set_lote_item( CHANGING i_lote = zib_nfe_dist_lot i_lote_caract = it_lote_caract ).

    READ TABLE it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_lote>) WITH KEY cd_lote_item = zib_nfe_dist_lot-cd_lote_item.
    IF <fs_lote> IS ASSIGNED.
      MOVE-CORRESPONDING zib_nfe_dist_lot TO <fs_lote>.
    ENDIF.

    READ TABLE it_lotes_alv_t ASSIGNING FIELD-SYMBOL(<fs_loteu>) WITH KEY cd_lote_item = zib_nfe_dist_lot-cd_lote_item.
    IF <fs_loteu> IS ASSIGNED.
      MOVE-CORRESPONDING zib_nfe_dist_lot TO <fs_loteu>.
    ENDIF.

    CLEAR: it_carac_alv_u[].

    LOOP AT it_lote_caract INTO DATA(rt_metodo).
      MOVE-CORRESPONDING rt_metodo TO it_carac_alv_u.
      READ TABLE it_carac_alv ASSIGNING FIELD-SYMBOL(<fs_carac>) WITH KEY cd_lote_item = rt_metodo-cd_lote_item
                                                                          atinn        = rt_metodo-atinn.
      MOVE-CORRESPONDING rt_metodo TO <fs_carac>.
      APPEND it_carac_alv_u.
    ENDLOOP.
    ck_alterou_lote_info = abap_false.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name(16) EQ 'ZIB_NFE_DIST_LOT'.
      SPLIT screen-name AT '-' INTO DATA(str1_1602) DATA(str2_1602).
      i_campo = str2_1602.
      IF ( i_campo = 'CHARG' OR i_campo = 'MENGE' ) AND it_lotes_alv_t[] IS NOT INITIAL. " AND zib_nfe_dist_lot-menge IS NOT INITIAL.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  ck_alterou_lote = abap_false.

  IF ck_alterou_lote EQ abap_true.

    IF cl_grid_1602 IS NOT INITIAL.
      cl_grid_1602->free( ).
    ENDIF.
    CLEAR: cl_grid_1602.

    IF cl_grid_1603 IS NOT INITIAL.
      cl_grid_1603->free( ).
    ENDIF.
    CLEAR: cl_grid_1603.

    IF container_1602 IS NOT INITIAL.
      container_1602->free( ).
    ENDIF.
    CLEAR: container_1602.

    IF container_1603 IS NOT INITIAL.
      container_1603->free( ).
    ENDIF.
    CLEAR: container_1603.

    ck_alterou_lote = abap_false.
  ENDIF.

  "Lotes
  IF container_1603 IS INITIAL.
    CLEAR wa_layout_1603.
    wa_layout_1603-zebra      = abap_true.
    wa_stable_1603-row        = abap_true.
    wa_stable_1603-col        = abap_true.
    wa_layout_1603-stylefname = 'STYLE'.
    wa_layout_1603-sel_mode   = 'A'.
*    WA_LAYOUT_1603-CWIDTH_OPT = 'X'.
    wa_layout_1603-col_opt    = 'X'.
    wa_layout_1603-info_fname = 'LINE_COLOR'.
    wa_layout_1603-ctab_fname = 'COLOR_CELL'.
    wa_layout_1603-no_toolbar = abap_false.

    CREATE OBJECT container_1603
      EXPORTING
        container_name = 'ALV_LOTES'.

    PERFORM fill_it_fieldcatalog_1603.

*   Fill info for layout variant
    PERFORM fill_gs_variant_1603.

    CREATE OBJECT cl_grid_1603
      EXPORTING
        i_parent = container_1603.

    CREATE OBJECT obg_toolbar_1603
      EXPORTING
        io_alv_grid = cl_grid_1603.

    SET HANDLER obg_toolbar_1603->on_toolbar FOR cl_grid_1603.
    SET HANDLER obg_toolbar_1603->handle_user_command FOR cl_grid_1603.

    CREATE OBJECT event_receiver_1603.
    SET HANDLER: event_receiver_1603->handle_double_click_1603  FOR cl_grid_1603,
                 event_receiver_1603->handle_hotspot_click_1603 FOR cl_grid_1603.

    CALL METHOD cl_grid_1603->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_1603
        i_save                        = 'A'
        is_layout                     = wa_layout_1603
        it_toolbar_excluding          = it_function_1603
      CHANGING
        it_outtab                     = it_lotes_alv_u[]
        it_fieldcatalog               = it_fieldcat_1603
        it_sort                       = it_sort_1603
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  "Características
  IF container_1602 IS INITIAL.
    CLEAR wa_layout_1602.
    wa_layout_1602-zebra      = abap_true.
    wa_stable_1602-row        = abap_true.
    wa_stable_1602-col        = abap_true.
    wa_layout_1602-stylefname = 'STYLE'.
    wa_layout_1602-sel_mode   = 'A'.
    wa_layout_1602-cwidth_opt = ' '.
    wa_layout_1602-col_opt    = ' '.
    wa_layout_1602-info_fname = 'LINE_COLOR'.
    wa_layout_1602-ctab_fname = 'COLOR_CELL'.
    wa_layout_1602-no_toolbar = abap_true.

    CREATE OBJECT container_1602
      EXPORTING
        container_name = 'ALV_CARACTERISTICAS'.

    PERFORM fill_it_fieldcatalog_1602.

*   Fill info for layout variant
    PERFORM fill_gs_variant_1602.

    CREATE OBJECT cl_grid_1602
      EXPORTING
        i_parent = container_1602.

    cl_grid_1602->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    cl_grid_1602->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_check             TO it_function_1602.
    APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_function_1602.

    CREATE OBJECT event_receiver_1602.
    SET HANDLER event_receiver_1602->handle_data_changed_1602 FOR cl_grid_1602.

    CALL METHOD cl_grid_1602->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_1602
        i_save                        = 'A'
        is_layout                     = wa_layout_1602
        it_toolbar_excluding          = it_function_1602
      CHANGING
        it_outtab                     = it_carac_alv_u[]
        it_fieldcatalog               = it_fieldcat_1602
        it_sort                       = it_sort_1602
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  CALL METHOD cl_grid_1603->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1603.

  CALL METHOD cl_grid_1602->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1602.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1602 .

  wa_variant_1602-report      = sy-repid.
  wa_variant_1602-handle      = '1602'.
  wa_variant_1602-log_group   = abap_false.
  wa_variant_1602-username    = abap_false.
  wa_variant_1602-variant     = abap_false.
  wa_variant_1602-text        = abap_false.
  wa_variant_1602-dependvars  = abap_false.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1603
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1603 .

  wa_variant_1603-report      = sy-repid.
  wa_variant_1603-handle      = '1603'.
  wa_variant_1603-log_group   = abap_false.
  wa_variant_1603-username    = abap_false.
  wa_variant_1603-variant     = abap_false.
  wa_variant_1603-text        = abap_false.
  wa_variant_1603-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1602 .

  DATA: i_contador_2 TYPE lvc_colpos,
        i_campo	     TYPE name_feld,
        i_prod_item	 TYPE j_1bitmnum,
        i_lote       TYPE charg_d.

  CLEAR: it_fieldcat_1602[], it_fieldcat_1602.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIB_NFE_DIST_LCA'
    CHANGING
      ct_fieldcat      = it_fieldcat_1602.

  i_contador_2 = 3.
  i_campo     = 'VFDAT'.
  i_prod_item = wa_itens_sel_lote-prod_item.
  i_lote      = zib_nfe_dist_lot-charg.

  DATA(ck_altera) = abap_true.

  LOOP AT it_fieldcat_1602 ASSIGNING FIELD-SYMBOL(<fs_1602>).
    <fs_1602>-edit = abap_false.
    CASE <fs_1602>-fieldname.
      WHEN 'SMBEZ'.
        <fs_1602>-col_pos   = 1.
        <fs_1602>-outputlen = 15.
      WHEN 'ATWRT'.
        <fs_1602>-edit      = ck_altera.
        <fs_1602>-col_pos   = 2.
        <fs_1602>-outputlen = 25.
      WHEN OTHERS.
        <fs_1602>-no_out  = abap_true.
        <fs_1602>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1603 .

  DATA: i_contador_2 TYPE lvc_colpos,
        wa_coluna    TYPE lvc_s_fcat.

  CLEAR: it_fieldcat_1603[], it_fieldcat_1603.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIB_NFE_DIST_LOT'
    CHANGING
      ct_fieldcat      = it_fieldcat_1603.

  wa_coluna-fieldname = 'INDEA'.
  APPEND wa_coluna TO it_fieldcat_1603.

  i_contador_2 = 3.

  LOOP AT it_fieldcat_1603 ASSIGNING FIELD-SYMBOL(<fs_1603>).
    <fs_1603>-edit = abap_false.
    CASE <fs_1603>-fieldname.
      WHEN 'CHARG'.
        <fs_1603>-col_pos   = 1.
        <fs_1603>-outputlen = 20.
      WHEN 'PROD_QTD_COMERCI'.
        <fs_1603>-edit      = abap_false.
        <fs_1603>-col_pos   = 2.
        <fs_1603>-outputlen = 30.
      WHEN 'INDEA'.
*        <FS_1603>-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
        <fs_1603>-scrtext_l = 'Indea'.
        <fs_1603>-scrtext_m = 'Indea'.
        <fs_1603>-scrtext_s = 'Indea'.
        <fs_1603>-icon      = abap_true.
        <fs_1603>-hotspot   = abap_true.
        <fs_1603>-just      = ' '.
        <fs_1603>-col_pos   = 23.
        <fs_1603>-outputlen = 06.
      WHEN OTHERS.
        <fs_1603>-no_out  = abap_true.
        <fs_1603>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1600_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1600_exit INPUT.

*  DATA: l_chave_e      TYPE char25. " RIM CS1029457 ANB 30.09.2022
  DATA: l_chave_e       TYPE char29. " RIM CS1029457 ANB 30.09.2022

*-IR063194 - 22.07.2021 - JT - inicio
  SELECT * FROM zsdt0082
           INTO TABLE @DATA(t_0082)
          WHERE nro_sol   = @ck_nrosol
            AND seq       = @ck_seq.

  DELETE t_0082 WHERE NOT ( dt_canc  IS     INITIAL
                      AND   dt_liber IS NOT INITIAL ).

  READ TABLE t_0082 INTO DATA(w_0082) INDEX 1.

  IF sy-subrc = 0.
    "//Dequeue register;
    CONCATENATE w_0082-nro_sol
                w_0082-seq
                w_0082-vbeln
                w_0082-posnr INTO l_chave_e.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = l_chave_e.
  ENDIF.
*-IR063194 - 22.07.2021 - JT - fim

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualiza_tela_1602 .

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  IF cl_grid_1601 IS NOT INITIAL.
    CALL METHOD cl_grid_1601->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.
  ENDIF.

  IF cl_grid_1602 IS NOT INITIAL.
    CALL METHOD cl_grid_1602->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.
  ENDIF.

  IF cl_grid_1603 IS NOT INITIAL.
    CALL METHOD cl_grid_1603->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " ATUALIZA_TELA

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_lote INPUT.

  ck_alterou_lote = abap_true.

  SELECT SINGLE *
    INTO @DATA(_mchb)
    FROM mchb
   WHERE matnr = @wa_itens_sel_lote-matnr
     AND charg = @zib_nfe_dist_lot-charg.

  IF sy-subrc <> 0.
    MESSAGE e024(sd) WITH 'Lote não Pertence ao Material!'.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_LOTE_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_lote_info INPUT.

  ck_alterou_lote_info = abap_true.

  IF zib_nfe_dist_lot-menge > wa_itens_sel_lote-menge.
    MESSAGE e024(sd) WITH TEXT-002.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed_1602  USING  e_onf4           TYPE char01
                               e_onf4_after     TYPE char01
                               e_onf4_before    TYPE char01
                               e_ucomm          TYPE sy-ucomm
                               er_data_changed  TYPE REF TO cl_alv_changed_data_protocol.

  DATA: lc_atwrt  TYPE atwrt,
        i_caract  TYPE zib_nfe_dist_lca_t,
        wa_caract TYPE zib_nfe_dist_lca.

  CLEAR: i_caract.

  LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_mod_cells).

    READ TABLE it_carac_alv_u ASSIGNING FIELD-SYMBOL(<fs_caracu>) INDEX ls_mod_cells-row_id.
    READ TABLE it_carac_alv   ASSIGNING FIELD-SYMBOL(<fs_carac>) WITH KEY cd_lote_item = <fs_caracu>-cd_lote_item
                                                                          atinn        = <fs_caracu>-atinn.
    CASE ls_mod_cells-fieldname.
      WHEN  'ATWRT'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = lc_atwrt.

        IF lc_atwrt NE <fs_caracu>-atwrt.
          <fs_caracu>-atwrt = lc_atwrt.
          <fs_carac>-atwrt  = lc_atwrt.
          MOVE-CORRESPONDING <fs_carac> TO wa_caract.
          APPEND wa_caract TO i_caract.
        ENDIF.
    ENDCASE.

    IF i_caract IS NOT INITIAL.
      lc_frete_remessa_trans->set_lote_item( CHANGING i_lote = zib_nfe_dist_lot i_lote_caract = i_caract ).
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_E_ROW_INDEX  text
*      -->P_1002   text
*----------------------------------------------------------------------*
FORM selecionar_lote  USING p_chave_nfe     TYPE zde_chave_doc_e
                            p_prod_item	    TYPE j_1bitmnum
                            p_cd_lote_item  TYPE zde_cd_lote_item
                            p_atualiza_tela TYPE char01.

  CLEAR: it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u, it_lotes_alv_u, it_lotes_alv_u[].

  LOOP AT it_lotes_alv_t WHERE chave_nfe EQ p_chave_nfe
                           AND prod_item EQ p_prod_item.
    it_lotes_alv_t-indea = icon_display_more.
    APPEND it_lotes_alv_t TO it_lotes_alv_u.
  ENDLOOP.

  READ TABLE it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_lotes>) WITH KEY cd_lote_item = p_cd_lote_item.

*  cd_lote_item = p_cd_lote_item.
  CHECK sy-subrc IS INITIAL.

  LOOP AT it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_limpar>) WHERE chave_nfe NE p_chave_nfe
                                                                OR prod_item NE p_prod_item.
    CLEAR: <fs_limpar>-line_color.
  ENDLOOP.

  IF <fs_lotes>-line_color NE cs_line_color_selecionada.

    <fs_lotes>-line_color = cs_line_color_selecionada.

    "Tem um Lote
    MOVE-CORRESPONDING <fs_lotes> TO zib_nfe_dist_lot.

    LOOP AT it_carac_alv INTO DATA(wa_carac_alv) WHERE cd_lote_item = zib_nfe_dist_lot-cd_lote_item.
      APPEND wa_carac_alv TO it_carac_alv_u.
    ENDLOOP.

  ENDIF.

  PERFORM atualiza_tela_1602.

  IF p_atualiza_tela EQ abap_true.
    LEAVE TO SCREEN 1600.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_lote .

  DATA: e_caracteristicas	TYPE zib_nfe_dist_lca_t,
        wa_carac_alv      TYPE ty_itens_carac_alv,
        wa_lotes_alv      TYPE ty_itens_lotes_alv,
        p_index           TYPE lvc_index.

  IF wa_itens_sel_lote IS INITIAL.
    MESSAGE s083.
    EXIT.
  ENDIF.

  TRY.
      DATA(r_lote) = lc_frete_remessa_trans->add_lote_item( EXPORTING i_chave_nfe       = wa_itens_sel_lote-chave_nfe
                                                                      i_prod_item       = wa_itens_sel_lote-prod_item
                                                            IMPORTING e_caracteristicas = e_caracteristicas ).
      "Alimenta Tabelas com dados base
      CLEAR: wa_lotes_alv.
      MOVE-CORRESPONDING r_lote TO wa_lotes_alv.
      APPEND wa_lotes_alv TO it_lotes_alv_t.

      LOOP AT e_caracteristicas INTO DATA(wa_caracteristicas) WHERE cd_lote_item EQ r_lote-cd_lote_item.
        CLEAR: wa_carac_alv.
        MOVE-CORRESPONDING wa_caracteristicas TO wa_carac_alv.
        APPEND wa_carac_alv TO it_carac_alv.
      ENDLOOP.

      READ TABLE it_lotes_alv_t WITH KEY chave_nfe    = wa_lotes_alv-chave_nfe
                                         cd_lote_item = wa_lotes_alv-cd_lote_item.
      MOVE sy-tabix TO p_index.
      PERFORM selecionar_lote USING wa_itens_sel_lote-chave_nfe wa_itens_sel_lote-prod_item r_lote-cd_lote_item abap_true.

    CATCH zcx_nfe_inbound_exception INTO ex_nfe_inbound.
      ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E').
      EXIT.
    CATCH zcx_charg_exception INTO ex_charg.
      ex_charg->published_erro( i_msgty = 'S' i_msgty_display = 'E').
      EXIT.
    CATCH zcx_cadastro INTO ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E').
      EXIT.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_lote .

  DATA: p_index  TYPE lvc_index.

  IF it_lotes_alv_sel[] IS INITIAL.
    MESSAGE s082.
    EXIT.
  ENDIF.

  LOOP AT it_lotes_alv_sel.
    READ TABLE it_itens_alv INTO DATA(wa_item_lote) WITH KEY chave_nfe = it_lotes_alv_sel-chave_nfe
                                                             prod_item = it_lotes_alv_sel-prod_item.
    IF sy-subrc = 0.
    ENDIF.
    lc_frete_remessa_trans->excluir_lote_item( i_cd_lote_item = it_lotes_alv_sel-cd_lote_item ).
    DELETE it_lotes_alv_u WHERE cd_lote_item = it_lotes_alv_sel-cd_lote_item.
  ENDLOOP.

  CLEAR: it_lotes, it_lotes_c, it_lotes_alv_t, it_lotes_alv_t[], it_lotes_c, it_lotes_c[], it_carac_alv_u[], zib_nfe_dist_lot, it_carac_alv_u.
  DATA(lc_info_nfe) = lc_frete_remessa_trans->get_info_nota( ).
  MOVE-CORRESPONDING lc_info_nfe-nfe_base TO zib_nfe_dist_ter.
  MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_inbound_alv.
  MOVE-CORRESPONDING lc_info_nfe-nfe_alv  TO zde_nfe_dist_alv.
  MOVE lc_info_nfe-nfe_base-lotes   TO it_lotes.
  MOVE lc_info_nfe-nfe_base-lotes_c TO it_lotes_c.

  LOOP AT it_lotes INTO DATA(wa_lotes).
    MOVE-CORRESPONDING wa_lotes TO it_lotes_alv_t.
    APPEND it_lotes_alv_t.
  ENDLOOP.

  LOOP AT it_lotes_c INTO DATA(wa_lotes_c).
    MOVE-CORRESPONDING wa_lotes_c TO it_carac_alv.
    APPEND it_carac_alv.
  ENDLOOP.

  READ TABLE it_itens_alv INTO DATA(wa_item_selecionado) WITH KEY line_color = cs_line_color_selecionada.
  IF sy-subrc IS INITIAL.
    READ TABLE it_lotes_alv_t WITH KEY prod_item = wa_item_selecionado-prod_item INTO DATA(wa_lc_lote).
    IF sy-subrc IS INITIAL.
      p_index = sy-tabix.
      PERFORM selecionar_lote USING wa_item_selecionado-chave_nfe wa_item_selecionado-prod_item wa_lc_lote-cd_lote_item abap_false.
    ENDIF.
  ENDIF.

  CALL METHOD cl_grid_1601->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1601.

  CALL METHOD cl_grid_1602->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1602.

  CALL METHOD cl_grid_1603->refresh_table_display
    EXPORTING
      is_stable = wa_stable_1603.

  LEAVE TO SCREEN 1600.

ENDFORM.
