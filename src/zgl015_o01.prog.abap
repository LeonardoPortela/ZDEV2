*&---------------------------------------------------------------------*
*&  Include           ZGL015_O01
*&---------------------------------------------------------------------*

*&SPWIZARD: OUTPUT MODULE FOR TS 'TS_100'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE ts_100_active_tab_set OUTPUT.
  ts_100-activetab = g_ts_100-pressed_tab.
  CASE g_ts_100-pressed_tab.
    WHEN c_ts_100-tab1.
      IF wg_colaps EQ '@K1@'.
        g_ts_100-subscreen = '0101'.
      ELSE.
        g_ts_100-subscreen = '0111'.
      ENDIF.
    WHEN c_ts_100-tab2.
      g_ts_100-subscreen = '0102'.
    WHEN c_ts_100-tab3.
      g_ts_100-subscreen = '0103'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TS_100_ACTIVE_TAB_SET OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode     TYPE TABLE OF sy-ucomm,
        lv_zgl019 TYPE c.

  REFRESH: fcode.

  IF wg_acao IS INITIAL.
    APPEND c_save   TO fcode.
    APPEND c_deldoc TO fcode.
  ELSEIF wg_zglt035-doc_lcto IS INITIAL.
    APPEND c_deldoc TO fcode.
  ENDIF.

  IF wg_zglt035-bldat IS INITIAL OR wg_zglt035-budat IS INITIAL OR wg_zglt035-monat IS INITIAL OR wg_zglt035-moeda_doc IS INITIAL.
    CALL METHOD grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.
  ELSE.
    CALL METHOD grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ENDIF.

*  SELECT *
*    FROM zglt037
*    INTO @DATA(ls_user)
*    UP TO 1 ROWS
*    WHERE aprovador = @sy-uname.
*    ENDSELECT.
  IF sy-tcode = 'ZGL016A'.
    APPEND c_save TO fcode.
    APPEND c_deldoc TO fcode.
    APPEND c_add    TO fcode.
    APPEND c_displa TO fcode.
    APPEND c_modif  TO fcode.
    APPEND 'LOTE'   TO fcode.
    APPEND 'LIBER' TO fcode.
    APPEND 'COPIA' TO fcode.
  ENDIF.


  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.

  IF sy-tcode = 'ZGL016A'.
    SET TITLEBAR 'Z002'.
  ELSE.
    SET TITLEBAR 'Z001'.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  F_INICIAR_TELA  OUTPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE f_iniciar_tela OUTPUT.

  IF sy-calld = 'X' AND  wg_zglt035-doc_lcto IS INITIAL.
    IF vg_chamada NE 'X'.
      PERFORM f_limpa_campos.
      REFRESH: tg_fields.
      GET PARAMETER ID 'LOT' FIELD  wg_zglt034-lote.
      IF wg_zglt034-lote IS INITIAL.
        PERFORM f_trata_campos USING  space
                                        'GR2'
                                        c_0       "INPUT 1     NO INPUT 0
                                        c_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM f_trata_campos USING  space
                                        'GR1'
                                        c_0       "INPUT 1     NO INPUT 0
                                        c_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM f_trata_campos USING  space
                                        'GR3'
                                        c_1       "INPUT 1     NO INPUT 0
                                        c_0.      "INVISIBLE 1 VISIBLE 0
        GET PARAMETER ID 'BLN' FIELD  wg_zglt035-doc_lcto.
      ELSE.
        vg_chamada = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF wg_acao IS INITIAL.
    CONCATENATE icon_header  TEXT-b03 INTO wg_menslot.
    CONCATENATE icon_resubmission  TEXT-b04 INTO wg_menslib.

    titulo = TEXT-l01.
    wgzglt034-lote = TEXT-l02.
    wgzglt000-tp_lcto = TEXT-l03.
    wgzglt000-bukrs = TEXT-l04.
    wgzglt000-moeda_doc = TEXT-l05.
    txt_ap_fiscal = TEXT-l06.
    wgzglt000-xblnr = TEXT-l07.
    txt_prov = TEXT-l08.
    txtper = TEXT-l09.
    txt_dt_doc = TEXT-l10.
    txt_dt_lcto = TEXT-l11.
    wgzglt000-bktxt = TEXT-l12.
    wgzglt035-doc_lcto = TEXT-l13.
    txt_doc_cont = TEXT-l14.
    txt_doc_cont_e = TEXT-l31.
    txt_doc_reversao = TEXT-l32."130130 - CS2023000969 Gisele Follmann PSA
    wgzglt000-blart = TEXT-l15.
    txt-taxa = TEXT-l16.
    txt-taxa_i = TEXT-l25.
    wgzglt000-moeda_interna = TEXT-l17.
    wgzglt000-moeda_forte = TEXT-l18.
    wgzglt000-moeda_grupo = TEXT-l19.
    wgzglt035-st_lc_moeda = TEXT-l20.
    wgzglt035-dt_doc  = TEXT-l21.
    wgzglt035-dt_lcto = TEXT-l21.
    wgzglt035-prov_est = TEXT-l24.
    wgzglt035-dt_doc_ult_mes = TEXT-l22.
    wgzglt035-dt_lcto_ult_mes1 = TEXT-l22 .
    wgzglt035-st_fecha = TEXT-l23.
    wgzglt035-st_agrupa = TEXT-l27.
    wgzglt035-moeda_gp_hist = TEXT-l26.
    wgzglt035_usnam = TEXT-l28.
    wgzglt035_dt_entrada = TEXT-l29.
    wgzglt035_hr_entrada = TEXT-l30.

    ts_100_tab1 = TEXT-t01.
    ts_100_tab2 = TEXT-t02.
    ts_100_tab3 = TEXT-t03.
    btn_visao = TEXT-b01.

    IF x_visao = 'X'.
      CLEAR x_visao.
*        BTN_VISAO = '@KU@ Visão de Razão'.
      btn_visao = TEXT-b01.
    ELSE.
      x_visao = 'X'.
*        BTN_VISAO = '@KU@ Visão de Entrada'.
      btn_visao = TEXT-b02.
    ENDIF.

    REFRESH: tg_fields.
    PERFORM f_trata_campos USING  space
                                  'GR2'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0

*    PERFORM F_TRATA_CAMPOS USING  SPACE
*                                  'GR1'
*                                  C_0       "INPUT 1     NO INPUT 0
*                                  C_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM f_trata_campos USING  space
                                  'GR3'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0

    IF vg_chamada = 'X'.
      CLEAR x_visao.
      wg_acao = c_add.
      vg_carrega = 'X'.
      PERFORM f_trata_campos USING  space
                                'GR1'
                                c_1       "INPUT 1     NO INPUT 0
                                c_0.      "INVISIBLE 1 VISIBLE 0
    ELSE.
      CLEAR vg_carrega.
      wg_acao = c_displa.
      PERFORM f_trata_campos USING  space
                                'GR1'
                                c_0       "INPUT 1     NO INPUT 0
                                c_0.      "INVISIBLE 1 VISIBLE 0
    ENDIF.
  ELSE.
    CLEAR vg_carrega.
  ENDIF.

ENDMODULE.                 " F_INICIAR_TELA  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE trata_fields OUTPUT.
  DATA: hoje   TYPE sy-datum,
        ultimo TYPE sy-datum,
        dias   TYPE i.
  IF wg_acao = c_modif AND wg_zglt035-tp_lcto IS NOT INITIAL AND wg_zglt035-monat IS INITIAL.
    hoje = sy-datum.
    CALL FUNCTION 'FKK_LAST_DAY_OF_MONTH'
      EXPORTING
        day_in            = hoje
      IMPORTING
        last_day_of_month = ultimo
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
  ENDIF.
  LOOP AT tg_fields.
    LOOP AT SCREEN.
      IF    screen-name   EQ tg_fields-campo
        OR  screen-group1 EQ tg_fields-group1.

        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        MODIFY SCREEN.
      ENDIF.

      IF    screen-name = 'WG_ZGLT035-TP_LCTO'.
        IF wg_acao = 'COPY'.
          screen-input     = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF    screen-name = 'WG_ZGLT034-LOTE'.
        IF wg_acao = 'COPY'.
          screen-input     = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF wg_acao = c_modif AND wg_zglt035-tp_lcto IS NOT INITIAL.
        IF    screen-name = 'WG_ZGLT035-BUDAT'.
          IF wg_zglt035-dt_lcto = 'X'. " Informar Data
            screen-input     = 1.
          ELSE.
            IF wg_zglt035-monat IS INITIAL.
              wg_zglt035-budat = ultimo.
            ENDIF.
            screen-input     = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.

        IF    screen-name = 'WG_ZGLT035-BLDAT'.
          IF wg_zglt035-dt_doc = 'X'. " Informar Data
            screen-input     = 1.
          ELSE.
            IF wg_zglt035-monat IS INITIAL.
              wg_zglt035-bldat = ultimo.
            ENDIF.
            screen-input     = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.

        IF    screen-name = 'WG_ZGLT035-PROV_EST' OR
              screen-name = 'WG_ZGLT035-MOEDA_GP_HIST'.
          IF wg_zglt035-tp_lcto GT 0.
            screen-input     = 0.
          ELSE.
            screen-input     = 1.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

  IF wg_colaps EQ '@K1@'.
    wg_sub01 = c_0051.
    LOOP AT SCREEN.
      IF screen-group4 EQ 'B1'.
        screen-active    = 0.
        screen-invisible = 1.

        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'TITULO'.
        screen-invisible = 0.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSE.
    wg_sub01 = c_0052.
    LOOP AT SCREEN.
      IF screen-group4 EQ 'B1'.
        screen-active    = 1.
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field.
  ENDIF.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function  WITH HEADER LINE,
        lt_f4       TYPE lvc_t_f4     WITH HEADER LINE.


  DATA: waref TYPE REF TO data.

  IF g_custom_container IS INITIAL.
    wa_layout-cwidth_opt  = c_x.
    wa_layout-zebra       = c_x.
    wa_layout-no_toolbar  = ''. "C_X.

    wa_layout-no_rowmark  = c_x.
*    wa_layout-col_opt     = C_X.
    wa_stable-row         = c_x.
    wa_stable-col         = c_x.
    wa_layout-sel_mode    = 'A'.
    wa_layout-box_fname   = 'MARK'.
    wa_layout-stylefname = 'STYLE'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

** Register event handler
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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    PERFORM f_montar_layout USING space space.
*    PERFORM f_montar_help.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = tg_fieldcatalog[]
        it_outtab            = tg_zglt036[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    lt_f4-fieldname = 'BVTYP'.
    lt_f4-register = 'X'.
    lt_f4-getbefore = 'X'.
    lt_f4-chngeafter ='X'.
    APPEND lt_f4.

    lt_f4-fieldname = 'DIVISAO'.
    lt_f4-register = 'X'.
    lt_f4-getbefore = 'X'.
    lt_f4-chngeafter ='X'.
    APPEND lt_f4.

    lt_f4-fieldname = 'HBKID'.
    lt_f4-register = 'X'.
    lt_f4-getbefore = 'X'.
    lt_f4-chngeafter ='X'.
    APPEND lt_f4.

    lt_f4-fieldname = 'HKONT'.
    lt_f4-register = 'X'.
    lt_f4-getbefore = 'X'.
    lt_f4-chngeafter ='X'.
    APPEND lt_f4.

    lt_f4-fieldname = 'TAX_CODE'.
    lt_f4-register = 'X'.
    lt_f4-getbefore = 'X'.
    lt_f4-chngeafter ='X'.
    APPEND lt_f4.

    lt_f4-fieldname = 'UMSKZ'.
    lt_f4-register = 'X'.
    lt_f4-getbefore = 'X'.
    lt_f4-chngeafter ='X'.
    APPEND lt_f4.

    lt_f4-fieldname = 'ZLSCH'.
    lt_f4-register = 'X'.
    lt_f4-getbefore = 'X'.
    lt_f4-chngeafter ='X'.
    APPEND lt_f4.

    lt_f4-fieldname = 'ZLSPR'.
    lt_f4-register = 'X'.
    lt_f4-getbefore = 'X'.
    lt_f4-chngeafter ='X'.
    APPEND lt_f4.


    CALL METHOD grid1->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER:
      lcl_event_handler=>on_hotspot_click         FOR grid1,
      lcl_event_handler=>on_double_click          FOR grid1,
      lcl_event_handler=>on_data_changed_finished FOR grid1,
      lcl_event_handler=>on_data_changed          FOR grid1,
      lcl_event_handler=>on_onf4                  FOR grid1.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
    IF wg_acao = c_modif OR  wg_acao = 'COPY'.
      PERFORM f_montar_layout USING c_x x_visao.
    ELSE.
      PERFORM f_montar_layout USING space x_visao.
    ENDIF.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = tg_fieldcatalog[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "GRID3
  IF obg_conteiner_err IS INITIAL.
    CREATE OBJECT obg_conteiner_err
      EXPORTING
        container_name = 'CC_LOG'.


    CREATE OBJECT grid3
      EXPORTING
        i_parent = obg_conteiner_err.


    PERFORM montar_layout_err.

    REFRESH: tl_function.
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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = tg_fieldcatalog[]
        it_outtab            = it_zib_contabil_err[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


  IF g_custom_cont_desc IS INITIAL.
    CREATE OBJECT g_custom_cont_desc
      EXPORTING
        container_name = g_descbox.

    IF g_custom_cont_desc IS NOT INITIAL.
      CREATE OBJECT obg_descbox
        EXPORTING
          parent            = g_custom_cont_desc
          wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = 72
          max_number_chars  = 350.

      CALL METHOD obg_descbox->set_toolbar_mode
        EXPORTING
          toolbar_mode = '0'.

      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'Z002'.
  SET TITLEBAR '0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_VAT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_vat OUTPUT.
*  DATA: "EVENT TYPE CNTL_SIMPLE_EVENT,
*        TL_FILTER           TYPE LVC_T_FILT,
*        WL_FILTER           TYPE LVC_S_FILT,
*        TL_FUNCTION         TYPE UI_FUNCTIONS,
*        WL_FUNCTION         LIKE TL_FUNCTION WITH HEADER LINE.
  "GRID4
  IF obg_conteiner_vat IS INITIAL.
    CREATE OBJECT obg_conteiner_vat
      EXPORTING
        container_name = 'CC_VAT'.


    CREATE OBJECT grid4
      EXPORTING
        i_parent = obg_conteiner_vat.


    PERFORM montar_layout_vat.

    REFRESH: tl_function.
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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.

    CLEAR wa_layout.
    wa_layout-no_toolbar = ' '.
    wa_layout-grid_title = 'VAT'.
    PERFORM montar_layout_vat.
    gs_variant_c-report = sy-repid.
    CALL METHOD grid4->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_c
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_save               = 'X'
        i_default            = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = tg_fieldcatalog[]
        it_outtab            = tg_vat[].

    PERFORM montar_layout_vat.
    CALL METHOD grid4->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = tg_fieldcatalog[].
    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ELSE.
    CLEAR wa_layout.
    wa_layout-no_toolbar = ' '.
    wa_layout-grid_title = 'VAT'.
    PERFORM montar_layout_vat.
    CALL METHOD grid4->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = tg_fieldcatalog[].

    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS_VAT  OUTPUT

MODULE cria_objetos_obj OUTPUT.
*  DATA: "EVENT TYPE CNTL_SIMPLE_EVENT,
*        TL_FILTER           TYPE LVC_T_FILT,
*        WL_FILTER           TYPE LVC_S_FILT,
*        TL_FUNCTION         TYPE UI_FUNCTIONS,
*        WL_FUNCTION         LIKE TL_FUNCTION WITH HEADER LINE.
  "GRID5
  DATA lt_f4_obj       TYPE lvc_t_f4     WITH HEADER LINE.
  IF obg_conteiner_obj IS INITIAL.
    CREATE OBJECT obg_conteiner_obj
      EXPORTING
        container_name = 'CC_OBJ'.


    CREATE OBJECT grid5
      EXPORTING
        i_parent = obg_conteiner_obj.


    PERFORM montar_layout_obj.

    REFRESH: tl_function.
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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.

    CLEAR wa_layout.
    wa_layout-no_toolbar = ' '.
    wa_layout-grid_title = 'OBJ'.
    PERFORM montar_layout_obj.
    gs_variant_c-report = sy-repid.
    CALL METHOD grid5->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_c
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_save               = 'X'
        i_default            = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = tg_fieldcatalog[]
        it_outtab            = tl_obj_aux[].

    CALL METHOD grid5->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid5->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    REFRESH lt_f4_obj.

    lt_f4_obj-fieldname = 'VORNR'.
    lt_f4_obj-register = 'X'.
    lt_f4_obj-getbefore = 'X'.
    lt_f4_obj-chngeafter ='X'.
    APPEND lt_f4_obj.

    CALL METHOD grid5->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4_obj[].


    SET HANDLER:
    lcl_event_handler=>on_data_changed_pop      FOR grid5,
    lcl_event_handler=>on_onf4                  FOR grid5.

    PERFORM montar_layout_obj.
    CALL METHOD grid5->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = tg_fieldcatalog[].
    CALL METHOD grid5->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ELSE.
    CLEAR wa_layout.
    wa_layout-no_toolbar = ' '.
    wa_layout-grid_title = 'OBJ'.
    PERFORM montar_layout_obj.
    CALL METHOD grid5->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = tg_fieldcatalog[].

    CALL METHOD grid5->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS_VAT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'Z002'.
  SET TITLEBAR '0300'.
ENDMODULE.
