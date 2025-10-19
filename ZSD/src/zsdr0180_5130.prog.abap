*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5130
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_solicitacao_5130,
         check          TYPE char1,
         matnr          TYPE mara-matnr,
         maktx          TYPE makt-maktx,
         saldo          TYPE zsdt0082-qte_sol,
         qtd_vinc       TYPE zsdt0082-qte_sol,
         meins          TYPE vbap-meins,
         wrkst          TYPE mara-wrkst,
         kunnr          TYPE vbpa-kunnr,
         name1          TYPE kna1-name1,
         inco1          TYPE vbkd-inco1,
         brgew          TYPE mara-brgew,
         lifnr          TYPE lfa1-lifnr,
         cod_loc_emb    TYPE zsdt0131-cod_loc_emb,
         local_embarq   TYPE zsdt0131-local_embarq,
         armazem        TYPE zsdt0132-armazem,
         transportadora TYPE zsdt0132-transportadora,
         transp_resp    TYPE zsdt0132-transp_resp,
         cor(4)         TYPE c.
         INCLUDE      STRUCTURE zsdt0082.
TYPES: cellstyles     TYPE lvc_t_styl.
TYPES: END OF ty_solicitacao_5130.

TYPES: BEGIN OF ty_cliente_5130,
         nro_sol     TYPE zsdt0131-nro_sol,
         seq         TYPE zsdt0082-seq,
         kunnr       TYPE zsdt0131-kunnr,
         name1       TYPE kna1-name1,
         seq_entrega TYPE zsdt0130-seq_entrega,
         nr_rot1     TYPE zsdt0130-nr_rot,
         obs         TYPE char7,
         nr_rot2     TYPE zsdt0130-nr_rot,
         cellstyles  TYPE lvc_t_styl.
TYPES: END OF ty_cliente_5130.

DATA: it_sol_5130     TYPE STANDARD TABLE OF ty_solicitacao_5130,
      it_cliente_5130 TYPE STANDARD TABLE OF ty_cliente_5130.

DATA: g_custom_container_5130      TYPE REF TO cl_gui_custom_container,
      dg_splitter_1_5130           TYPE REF TO cl_gui_splitter_container,
      dg_parent_1_5130             TYPE REF TO cl_gui_container,
      dg_parent_2_5130             TYPE REF TO cl_gui_container,
      ctl_alv1_5130                TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5130                TYPE REF TO cl_gui_alv_grid,
      gs_layout_5130_alv1          TYPE lvc_s_layo,
      gs_layout_5130_alv2          TYPE lvc_s_layo,
      it_fieldcatalog_sol_5130     TYPE lvc_t_fcat,
      it_fieldcatalog_cliente_5130 TYPE lvc_t_fcat,
      it_exclude_5130              TYPE ui_functions,
      it_stable_5130               TYPE lvc_s_stbl.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5130 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

      toolbar_5130 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5130 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      data_changed_finished_5130 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      handle_hotspot_click_5130  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      handle_button_click_5130 FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no,

      on_f4_5130 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5130 IMPLEMENTATION.

  METHOD toolbar_5130.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'DEL'.
    wa_tool-icon     = '@18@'.
    wa_tool-quickinfo = 'Eliminar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD user_command_5130.

    IF e_ucomm = 'DEL'.

      DATA: it_sel_rows_5130 TYPE lvc_t_row,
            wa_sel_rows_5130 TYPE lvc_s_row,
            wa_sol_5130      TYPE ty_solicitacao_5130,
            wa_cliente_5130  TYPE ty_cliente_5130,
            vl_cont          TYPE i,
            chave            TYPE zde_chave_sol.

      CALL METHOD ctl_alv1_5130->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows_5130.

      LOOP AT it_sol_5130 INTO wa_sol_5130.
        READ TABLE it_sel_rows_5130 INTO wa_sel_rows_5130 WITH KEY index = sy-tabix."INDEX SY-TABIX.
        IF sy-subrc IS INITIAL.
          wa_sol_5130-name1 = 'DELETE'.
          MODIFY it_sol_5130 FROM wa_sol_5130 INDEX wa_sel_rows_5130-index.

          CONCATENATE wa_sol_5130-nro_sol wa_sol_5130-seq wa_sol_5130-vbeln wa_sol_5130-posnr INTO chave.

          CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
            EXPORTING
              chave = chave.

        ENDIF.
      ENDLOOP.

      LOOP AT it_cliente_5130 INTO wa_cliente_5130.
        vl_cont = vl_cont + 1.
        READ TABLE it_sol_5130 INTO wa_sol_5130 WITH KEY nro_sol = wa_cliente_5130-nro_sol
                                                         name1   = 'DELETE'.
        IF sy-subrc IS INITIAL.
          wa_cliente_5130-name1 = 'DELETE'.
          MODIFY it_cliente_5130 FROM wa_cliente_5130 INDEX vl_cont.
        ENDIF.
      ENDLOOP.

      DELETE it_sol_5130 WHERE name1 EQ 'DELETE'.
      DELETE it_cliente_5130 WHERE name1 EQ 'DELETE'.

      PERFORM atualiza_header_lote_5130.
      CLEAR: vl_cont.

      vg_subt_lote = '5130'.
      LEAVE TO SCREEN 5000.

    ENDIF.
  ENDMETHOD.                    "USER_COMMAND

  METHOD data_changed_finished_5130.

    DATA: wa_good_cells TYPE lvc_s_modi.

    READ TABLE et_good_cells INTO wa_good_cells INDEX 1.

    IF e_modified EQ abap_true.
      IF wa_good_cells-fieldname EQ 'QTD_VINC'.
        PERFORM atualiza_header_lote_5130.
        vg_subt_lote = '5130'.
        LEAVE TO SCREEN 5000.
      ELSEIF wa_good_cells-fieldname EQ 'SEQ_ENTREGA'.
        PERFORM atualiza_seq_entrega_5130 USING wa_good_cells.
        vg_subt_lote = '5130'.
        LEAVE TO SCREEN 5000.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD handle_hotspot_click_5130.

    DATA: wa_cliente_5130_anexo TYPE ty_cliente_5130.

    DATA: wa_bor TYPE borident.

    DATA: wl_name  TYPE thead-tdname.

    IF e_column_id-fieldname EQ 'NR_ROT2'.
      READ TABLE it_cliente_5130 INTO wa_cliente_5130_anexo INDEX e_row_id-index.
      IF sy-subrc IS INITIAL.
        wa_bor-objkey = wa_cliente_5130_anexo-nr_rot2.
        wa_bor-objtype = 'ZSDR0061'.
        PERFORM chama_anexo USING wa_bor.
      ENDIF.

    ELSEIF e_column_id-fieldname EQ 'NR_ROT1'.

      READ TABLE it_cliente_5130 INTO wa_cliente_5130_anexo INDEX e_row_id-index.
      IF sy-subrc IS INITIAL.
        wl_name = wa_cliente_5130_anexo-nr_rot1.
        PERFORM chama_texto_rot USING wl_name.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_button_click_5130.

    DATA: wa_cliente_5130 TYPE ty_cliente_5130.
    DATA: wl_name  TYPE thead-tdname.

    IF es_col_id-fieldname EQ 'OBS'.
      READ TABLE it_cliente_5130 INTO wa_cliente_5130 INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL.
        CONCATENATE wa_cliente_5130-nro_sol '001' INTO wl_name.
        PERFORM chama_texto_obs USING wl_name.
      ENDIF.
    ENDIF.

  ENDMETHOD .

  METHOD on_f4_5130.

    PERFORM f4_local_embarque USING er_event_data es_row_no.

  ENDMETHOD.                                                "on_f4

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5130  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_5130 OUTPUT.

*  IF p_filial IS NOT INITIAL.
*    wa_header_lote-inco1 = p_inco1-low.
*  ELSEIF p_corplt IS NOT INITIAL.
*    wa_header_lote-inco1 = c_inco1-low.
*  ENDIF.

  LOOP AT SCREEN.
    IF wa_header_lote-inco1 NE 'FOB'.
      IF wa_header_lote-inco1 EQ 'CIF' OR wa_header_lote-inco1 EQ 'CPT'.
        IF screen-name NE 'F8' AND screen-name NE 'WA_HEADER_LOTE-DT_ENTREGA'.
          screen-input = 0.
        ENDIF.
      ELSE.
        IF screen-name NE 'F8'.
          screen-input = 0.
        ENDIF.
      ENDIF.
    ENDIF.
    IF screen-name EQ 'WA_HEADER_LOTE-WRKST'.
      IF it_sol_5130 IS NOT INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

  IF g_custom_container_5130 IS INITIAL.

    CREATE OBJECT g_custom_container_5130
      EXPORTING
        container_name              = 'CONTAINER5130'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT dg_splitter_1_5130
      EXPORTING
        parent  = g_custom_container_5130
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1_5130->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5130.

    CALL METHOD dg_splitter_1_5130->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5130.

    CALL METHOD dg_splitter_1_5130->set_row_mode
      EXPORTING
        mode = dg_splitter_1_5130->mode_relative.

    CALL METHOD dg_splitter_1_5130->set_row_height
      EXPORTING
        id     = 1
        height = 65.

    CALL METHOD dg_splitter_1_5130->set_row_height
      EXPORTING
        id     = 2
        height = 35.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5130 USING:
          01 'NRO_SOL'       'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          02 'VBELN'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem',
          03 'POSNR'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          04 'AUART'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tipo OV',
          05 'VKBUR'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Vendas',
          06 'VKORG'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Org. Vendas',
          07 'SPART'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Setor Atv',
          08 'WERKS'         'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Centro Forn',
          09 'MATNR'         'MARA'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Material',
          10 'MAKTX'         'MAKT'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Descrição',
          11 'SALDO'         ''          ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Saldo à Formar Lote',
          12 'QTD_VINC'      'ZSDT0131'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. do Lote',
          13 'MEINS'         'VBAP'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
          14 'LIFNR'         'ZSDT0132'  'C310'  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Fornecedor',
          15 'COD_LOC_EMB'   'ZSDT0131'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Loc. Emb.',
          16 'LOCAL_EMBARQ'  'ZSDT0131'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Loc. Emb.',
          17 'ARMAZEM'        'ZSDT0132' ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Armazém.',
          18 'TRANSPORTADORA' 'ZSDT0132' ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transportadora.',
          19 'TRANSP_RESP'    'ZSDT0132' ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transp. Resp.'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_cliente_5130 USING:
           01 'NRO_SOL'      'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
           02 'KUNNR'        'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód Cliente',
           03 'NAME1'        'LFA1'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nome',
           04 'SEQ_ENTREGA'  ''          ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Seq Entrega',
           05 'NR_ROT1'      ''          ' '     ' '  ' '  'X'   'X'   ' '   ' '   ' '   'Roteiro',
           06 'OBS'          ''          ' '     ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Observação',
           07 'NR_ROT2'      ''          ' '     ' '  ' '  'X'   'X'   ' '   ' '   ' '   'Doc Anexo'.

    gs_layout_5130_alv1-sel_mode   = 'A'.
    gs_layout_5130_alv1-stylefname = 'CELLSTYLES'.
    gs_layout_5130_alv1-cwidth_opt = 'X'.
    gs_layout_5130_alv1-grid_title = 'Ordens com Solicitação de Entrega'.
    gs_layout_5130_alv1-smalltitle = 'X'.
    gs_layout_5130_alv2-sel_mode   = 'A'.
    gs_layout_5130_alv2-stylefname = 'CELLSTYLES'.
    gs_layout_5130_alv2-cwidth_opt = 'X'.
    gs_layout_5130_alv2-grid_title = 'Clientes das Ordens'.
    gs_layout_5130_alv2-smalltitle = 'X'.

    CREATE OBJECT ctl_alv1_5130
      EXPORTING
        i_parent = dg_parent_1_5130.           "ALV Solicitações

    CREATE OBJECT ctl_alv2_5130
      EXPORTING
        i_parent = dg_parent_2_5130.           "ALV Clientes

    PERFORM excluir_botoes CHANGING it_exclude_5130.
    PERFORM registrar_f4_5130.

    SET HANDLER:
      lcl_event_handler_5130=>toolbar_5130 FOR ctl_alv1_5130,
      lcl_event_handler_5130=>user_command_5130 FOR ctl_alv1_5130,
      lcl_event_handler_5130=>data_changed_finished_5130 FOR ctl_alv1_5130,
      lcl_event_handler_5130=>on_f4_5130 FOR ctl_alv1_5130.

    CALL METHOD ctl_alv1_5130->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5130_alv1
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5130
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_sol_5130
        it_outtab            = it_sol_5130.

    SET HANDLER:
      lcl_event_handler_5130=>data_changed_finished_5130 FOR ctl_alv2_5130,
      lcl_event_handler_5130=>handle_hotspot_click_5130 FOR ctl_alv2_5130,
      lcl_event_handler_5130=>handle_button_click_5130 FOR ctl_alv2_5130.

    CALL METHOD ctl_alv2_5130->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5130_alv2
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5130
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_cliente_5130
        it_outtab            = it_cliente_5130.

    CALL METHOD ctl_alv1_5130->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    CALL METHOD ctl_alv2_5130->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    it_stable_5130-row = 'X'.
    it_stable_5130-col = 'X'.

    CALL METHOD ctl_alv1_5130->refresh_table_display
      EXPORTING
        is_stable = it_stable_5130.

    CALL METHOD ctl_alv2_5130->refresh_table_display
      EXPORTING
        is_stable = it_stable_5130.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5130  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5130 INPUT.

  DATA: vl_check    TYPE char1.

  ctl_alv1_5130->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'F85130'.
      CALL SCREEN 5131 STARTING AT 5 5 ENDING AT 160 30.
    WHEN 'SAVE'.
      CLEAR: vl_check.
      PERFORM check_parametro_5130 CHANGING vl_check.
      IF vl_check IS INITIAL.
        PERFORM salva_lote_5130.
        PERFORM clear_5130.
      ENDIF.
    WHEN 'ENTER'.
      PERFORM completa_motorista CHANGING wa_header_lote.
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SALVA_LOTE_5130
*&---------------------------------------------------------------------*
FORM salva_lote_5130.

  DATA: wa_zsdt0129 TYPE zsdt0129,
        wa_zsdt0130 TYPE zsdt0130,
        wa_zsdt0131 TYPE zsdt0131,
        wa_zsdt0082 TYPE zsdt0082,
        vl_erro     TYPE char1.

  DATA: wa_sol_5130     TYPE ty_solicitacao_5130,
        wa_cliente_5130 TYPE ty_cliente_5130.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZSD_INS_LT'
    IMPORTING
      number                  = wa_zsdt0129-nro_lote
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  wa_zsdt0129-inco1         = wa_header_lote-inco1.
  wa_zsdt0129-marca         = wa_header_lote-wrkst.
  wa_zsdt0129-ctg_transp    = wa_header_lote-ctg_transp.
  wa_zsdt0129-qtd_total_kg  = wa_header_lote-qtd_total_kg.
  wa_zsdt0129-placa_cav     = wa_header_lote-placa_cav.
  wa_zsdt0129-placa_car1    = wa_header_lote-placa_car1.
  wa_zsdt0129-placa_car2    = wa_header_lote-placa_car2.
*-CS2019001891 - JT - 04.02.2021 - inicio
  wa_zsdt0129-placa_car3    = wa_header_lote-placa_car3.
*-CS2019001891 - JT - 04.02.2021 - fim
  wa_zsdt0129-motorista     = wa_header_lote-motorista.
  wa_zsdt0129-dt_entrega    = wa_header_lote-dt_entrega.
  wa_zsdt0129-status        = '0'.
  wa_zsdt0129-usnam         = sy-uname.
  wa_zsdt0129-data_atual    = sy-datum.
  wa_zsdt0129-hora_atual    = sy-uzeit.

  MODIFY zsdt0129 FROM wa_zsdt0129.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE TEXT-017 TYPE 'S' DISPLAY LIKE 'E'.
    vl_erro = abap_true.
  ENDIF.

  LOOP AT it_sol_5130 INTO wa_sol_5130.

    wa_zsdt0130-nro_sol  = wa_sol_5130-nro_sol.
    wa_zsdt0130-seq      = wa_sol_5130-seq.
    wa_zsdt0130-nro_lote = wa_zsdt0129-nro_lote.

    READ TABLE it_cliente_5130 INTO wa_cliente_5130 WITH KEY nro_sol = wa_sol_5130-nro_sol
                                                             seq     = wa_sol_5130-seq.

    IF sy-subrc IS INITIAL.
      wa_zsdt0130-kunnr        = wa_cliente_5130-kunnr.
      wa_zsdt0130-nr_rot       = wa_cliente_5130-nr_rot1.
      wa_zsdt0130-seq_entrega  = wa_cliente_5130-seq_entrega.
    ENDIF.

    wa_zsdt0130-status     = '0'.
    wa_zsdt0130-usnam      = sy-uname.
    wa_zsdt0130-data_atual = sy-datum.
    wa_zsdt0130-hora_atual = sy-uzeit.

    wa_zsdt0131-nro_lote     = wa_zsdt0129-nro_lote.
    wa_zsdt0131-nro_sol      = wa_sol_5130-nro_sol.
    wa_zsdt0131-seq          = wa_sol_5130-seq.
    wa_zsdt0131-kunnr        = wa_cliente_5130-kunnr.
    wa_zsdt0131-vbeln        = wa_sol_5130-vbeln.
    wa_zsdt0131-posnr        = wa_sol_5130-posnr.
    wa_zsdt0131-spart        = wa_sol_5130-spart.
    wa_zsdt0131-vkorg        = wa_sol_5130-vkorg.
    wa_zsdt0131-vkbur        = wa_sol_5130-vkbur.
    wa_zsdt0131-werks        = wa_sol_5130-werks.
    wa_zsdt0131-auart        = wa_sol_5130-auart.
    wa_zsdt0131-matnr        = wa_sol_5130-matnr.
    wa_zsdt0131-brgew        = wa_sol_5130-brgew.
    wa_zsdt0131-qtd_vinc     = wa_sol_5130-qtd_vinc.
    wa_zsdt0131-um           = wa_sol_5130-meins.
    wa_zsdt0131-qtd_emkg     = wa_sol_5130-qtd_vinc * wa_sol_5130-brgew.
    wa_zsdt0131-cod_loc_emb  = wa_sol_5130-cod_loc_emb.
    wa_zsdt0131-local_embarq = wa_sol_5130-local_embarq.
    wa_zsdt0131-status       = '0'.
    wa_zsdt0131-usnam        = sy-uname.
    wa_zsdt0131-data_atual   = sy-datum.
    wa_zsdt0131-hora_atual   = sy-uzeit.

    MODIFY zsdt0130 FROM wa_zsdt0130.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE TEXT-018 TYPE 'S' DISPLAY LIKE 'E'.
      vl_erro = abap_true.
    ENDIF.

    MODIFY zsdt0131 FROM wa_zsdt0131.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE TEXT-019 TYPE 'S' DISPLAY LIKE 'E'.
      vl_erro = abap_true.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0082
      INTO wa_zsdt0082
      WHERE nro_sol = wa_sol_5130-nro_sol
        AND seq     = wa_sol_5130-seq
        AND vbeln   = wa_sol_5130-vbeln
        AND posnr   = wa_sol_5130-posnr.

    IF sy-subrc IS INITIAL.
      wa_zsdt0082-status = '5'.
    ENDIF.

    MODIFY zsdt0082 FROM wa_zsdt0082.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE TEXT-020 TYPE 'S' DISPLAY LIKE 'E'.
      vl_erro = abap_true.
    ENDIF.

  ENDLOOP.

  IF vl_erro IS INITIAL.
    PERFORM desbloqueia_sol_5130.
    PERFORM clear_5130.
    MESSAGE s000(z_fi) WITH 'Lote' wa_zsdt0129-nro_lote 'salvo com sucesso' .
  ENDIF.

  CLEAR: wa_zsdt0129, wa_zsdt0130, wa_zsdt0131, wa_zsdt0082.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_HEADER_LOTE_5130
*&---------------------------------------------------------------------*
FORM atualiza_header_lote_5130.

  DATA: wa_sol_5130 TYPE ty_solicitacao_5130.

  CLEAR: wa_header_lote-qtd_total_kg.

  LOOP AT it_sol_5130 INTO wa_sol_5130.
    wa_header_lote-qtd_total_kg = wa_header_lote-qtd_total_kg + wa_sol_5130-qtd_vinc * wa_sol_5130-brgew.
  ENDLOOP.

  IF wa_header_lote-qtd_total_kg GT 0 AND wa_header_lote-qtd_total_kg LE 23000.
    wa_header_lote-ctg_transp_d = 'AVULSO'.
    wa_header_lote-ctg_transp = 'A'.
  ELSEIF wa_header_lote-qtd_total_kg GT 23000 AND wa_header_lote-qtd_total_kg LE 27000.
    wa_header_lote-ctg_transp_d = 'VEÍCULO 27'.
    wa_header_lote-ctg_transp = 'B'.
  ELSEIF wa_header_lote-qtd_total_kg GT 27000 AND wa_header_lote-qtd_total_kg LE 32000.
    wa_header_lote-ctg_transp_d = 'VEÍCULO 32'.
    wa_header_lote-ctg_transp = 'C'.
  ELSEIF wa_header_lote-qtd_total_kg GT 32000 AND wa_header_lote-qtd_total_kg LE 37000.
    wa_header_lote-ctg_transp_d = 'VEÍCULO 37'.
    wa_header_lote-ctg_transp = 'D'.
  ELSEIF wa_header_lote-qtd_total_kg GT 37000 AND wa_header_lote-qtd_total_kg LE 50000.
    wa_header_lote-ctg_transp_d = 'VEÍCULO 50'.
    wa_header_lote-ctg_transp = 'E'.
  ELSE.
    wa_header_lote-ctg_transp_d = '----------'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_PARAMETRO_5130
*&---------------------------------------------------------------------*
FORM check_parametro_5130 CHANGING vl_check.

  DATA: wa_sol_5130           TYPE ty_solicitacao_5130,
        wa_cliente_5130       TYPE ty_cliente_5130,
        it_cliente_5130_check TYPE STANDARD TABLE OF ty_cliente_5130,
        it_sol_5130_check     TYPE STANDARD TABLE OF ty_solicitacao_5130,
        vl_lines              TYPE i.

  "Check do Total KG > 50.000 Kg
  IF wa_header_lote-qtd_total_kg GT 50000.
    MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
    "Check cabeçalho em branco
  ELSEIF ( wa_header_lote-placa_cav    IS INITIAL OR
           wa_header_lote-motorista    IS INITIAL OR
           wa_header_lote-wrkst        IS INITIAL )
          AND wa_header_lote-inco1 EQ 'FOB'.
    MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
    "Check Placas
  ELSEIF wa_header_lote-placa_cav IS NOT INITIAL AND wa_header_lote-inco1 NE 'FOB'.
    PERFORM valida_placa_sementes USING wa_header_lote-placa_cav '0'
                                  CHANGING vl_check.

    IF wa_header_lote-placa_car1 IS NOT INITIAL.
      PERFORM valida_placa_sementes USING wa_header_lote-placa_car1 '1'
                                    CHANGING vl_check.
    ENDIF.

    IF wa_header_lote-placa_car2 IS NOT INITIAL.
      PERFORM valida_placa_sementes USING wa_header_lote-placa_car2 '1'
                                    CHANGING vl_check.
    ENDIF.

*-CS2019001891 - JT - 04.02.2021 - inicio
    IF wa_header_lote-placa_car3 IS NOT INITIAL.
      PERFORM valida_placa_sementes USING wa_header_lote-placa_car3 '1'
                                    CHANGING vl_check.
    ENDIF.
*-CS2019001891 - JT - 04.02.2021 - fim

  ENDIF.

  SELECT *
    FROM zsdt0132
    INTO TABLE @DATA(t_0132)
      FOR ALL ENTRIES IN @it_sol_5130
      WHERE nr_rot EQ @it_sol_5130-cod_loc_emb.
  IF sy-subrc IS INITIAL.
    DATA(t_0132_aux) = t_0132.
    SORT t_0132_aux BY transp_resp.
    DELETE ADJACENT DUPLICATES FROM t_0132_aux COMPARING transp_resp.
    IF lines( t_0132_aux ) > 1.
      MESSAGE TEXT-133 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ENDIF.

    t_0132_aux = t_0132.
    SORT t_0132_aux BY armazem.
    DELETE ADJACENT DUPLICATES FROM t_0132_aux COMPARING armazem.
    IF lines( t_0132_aux ) > 1.
      MESSAGE TEXT-134 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ENDIF.

    t_0132_aux = t_0132.
    SORT t_0132_aux BY transportadora.
    DELETE ADJACENT DUPLICATES FROM t_0132_aux COMPARING transportadora.
    IF lines( t_0132_aux ) > 1.
      MESSAGE TEXT-135 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ENDIF.

  ENDIF.

  "Check lote sem solicitação
  IF it_sol_5130 IS INITIAL.
    MESSAGE TEXT-014 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
  ELSE.
    LOOP AT it_sol_5130 INTO wa_sol_5130.
      "Check solicitação com mais volume solicitado do que disponível
      IF wa_sol_5130-qtd_vinc > wa_sol_5130-saldo.
        MESSAGE TEXT-015 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        "Check solicitação com mais volume solicitado menor ou igual a zero
      ELSEIF wa_sol_5130-qtd_vinc LE 0.
        MESSAGE TEXT-015 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDLOOP.

    "cHECK SE POSSUI MAIS DE UM VKBUR POR LOTE
    it_sol_5130_check = it_sol_5130.
    SORT it_sol_5130_check BY vkbur.
    DELETE ADJACENT DUPLICATES FROM it_sol_5130_check COMPARING vkbur.
    DESCRIBE TABLE it_sol_5130_check LINES vl_lines.

    IF vl_lines NE 1.
      MESSAGE TEXT-121 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ENDIF.

    LOOP AT it_cliente_5130 INTO wa_cliente_5130.
      "Check solicitação sem sequência de entrega
      IF wa_cliente_5130-seq_entrega IS INITIAL.
        MESSAGE TEXT-016 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDLOOP.
    "Check solicitação sem sequência correta
    it_cliente_5130_check = it_cliente_5130.
    SORT it_cliente_5130_check BY seq_entrega ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_cliente_5130_check COMPARING seq_entrega.
    LOOP AT it_cliente_5130_check INTO wa_cliente_5130.
      IF wa_cliente_5130-seq_entrega NE sy-tabix.
        MESSAGE TEXT-029 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_5130
*&---------------------------------------------------------------------*
FORM clear_5130 .
  CLEAR: it_sol_5130, it_cliente_5130, wa_header_lote.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_SEQ_ENTREGA_5130
*&---------------------------------------------------------------------*
FORM atualiza_seq_entrega_5130 USING wa_good_cells TYPE lvc_s_modi.

  DATA: vl_roteiro      TYPE zsdt0130-nr_rot,
        wa_cliente_5130 TYPE ty_cliente_5130.

  READ TABLE it_cliente_5130 INTO wa_cliente_5130 INDEX wa_good_cells-row_id.

  vl_roteiro = wa_cliente_5130-nr_rot1.

  LOOP AT it_cliente_5130 INTO wa_cliente_5130 WHERE nr_rot1 EQ vl_roteiro.
    wa_cliente_5130-seq_entrega = wa_good_cells-value.
    MODIFY it_cliente_5130 FROM wa_cliente_5130 INDEX sy-tabix.
  ENDLOOP.

  SORT it_cliente_5130 BY seq_entrega ASCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_F4_5130
*&---------------------------------------------------------------------*
FORM registrar_f4_5130 .

  DATA: gs_f4      TYPE lvc_s_f4,
        gt_f4_5130 TYPE lvc_t_f4.

  gs_f4-fieldname  = 'LIFNR'.
  gs_f4-register   = 'X'.
  gs_f4-getbefore  = space.
  gs_f4-chngeafter = space.
  APPEND gs_f4 TO gt_f4_5130.

  CALL METHOD ctl_alv1_5130->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_5130.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_SOL_5130
*&---------------------------------------------------------------------*
FORM desbloqueia_sol_5130.

  DATA: wa_sol_5130 TYPE ty_solicitacao_5130,
        chave       TYPE zde_chave_sol.


  LOOP AT it_sol_5130 INTO wa_sol_5130.

    CONCATENATE wa_sol_5130-nro_sol wa_sol_5130-seq wa_sol_5130-vbeln wa_sol_5130-posnr INTO chave.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = chave.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  REQUEST_MARCA_SEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE request_marca_sem INPUT.

  DATA: it_f4_wrkst_aux  TYPE STANDARD TABLE OF ty_solicitacao_5130,
        it_f4            TYPE STANDARD TABLE OF ty_f4_wrkst_sem,
        wa_f4_wrkst_aux  TYPE ty_solicitacao_5130,
        wa_f4            TYPE ty_f4_wrkst_sem,
        it_zsdt0131_aux  TYPE STANDARD TABLE OF zsdt0131,
        wa_zsdt0131_aux  TYPE zsdt0131,
        it_return        TYPE STANDARD TABLE OF ddshretval,
        wa_return        TYPE ddshretval,
        vl_cont          TYPE i,
        vl_qtd_marca_sem TYPE zsdt0131-qtd_vinc,
        it_vbkd          TYPE STANDARD TABLE OF vbkd,
        wa_vbkd          TYPE vbkd.

  CLEAR: vl_cont.

*  IF p_filial IS NOT INITIAL.
*
*    SELECT vbap~matnr
*             mara~meins
*             mara~wrkst
*             zsdt0082~mandt
*             zsdt0082~nro_sol
*             zsdt0082~seq
*             zsdt0082~vbeln
*             zsdt0082~posnr
*             zsdt0082~seq_lib
*             zsdt0082~vkorg
*             zsdt0082~spart
*             zsdt0082~vkgrp
*             zsdt0082~vkbur
*             zsdt0082~auart
*             vbap~werks
*             zsdt0082~qte_sol
*             zsdt0082~dt_liber
*             zsdt0082~usuario_lib
*             zsdt0082~qte_lib
*             zsdt0082~status
*             zsdt0082~dt_entrega
*             mara~brgew
*             zsdt0082~nr_rot
*        INTO CORRESPONDING FIELDS OF TABLE it_f4_wrkst_aux
*        FROM zsdt0082
*        INNER JOIN vbap ON zsdt0082~vbeln = vbap~vbeln AND
*                           zsdt0082~posnr = vbap~posnr
*        INNER JOIN mara ON mara~matnr = vbap~matnr
*        INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
*          WHERE zsdt0082~vkbur  IN p_vkbur
*            AND zsdt0082~vkorg  IN p_vkorg
*            AND zsdt0082~spart  EQ p_spart
*            AND vbak~kunnr IN p_kunnr
*            AND zsdt0082~seq    NE 1
*            AND ( zsdt0082~status EQ 2 OR
*                  zsdt0082~status EQ 5 ).
*
*  ELSEIF p_corplt IS NOT INITIAL OR
*         p_corpcg IS NOT INITIAL OR
*         p_corppt IS NOT INITIAL.
*
*    SELECT vbap~matnr
*           mara~meins
*           mara~wrkst
*           zsdt0082~mandt
*           zsdt0082~nro_sol
*           zsdt0082~seq
*           zsdt0082~vbeln
*           zsdt0082~posnr
*           zsdt0082~seq_lib
*           zsdt0082~vkorg
*           zsdt0082~spart
*           zsdt0082~vkgrp
*           zsdt0082~vkbur
*           zsdt0082~auart
*           vbap~werks
*           zsdt0082~qte_sol
*           zsdt0082~dt_liber
*           zsdt0082~usuario_lib
*           zsdt0082~qte_lib
*           zsdt0082~status
*           zsdt0082~dt_entrega
*           mara~brgew
*           zsdt0082~nr_rot
*      INTO CORRESPONDING FIELDS OF TABLE it_f4_wrkst_aux
*      FROM zsdt0082
*      INNER JOIN vbap ON zsdt0082~vbeln = vbap~vbeln AND
*                         zsdt0082~posnr = vbap~posnr
*      INNER JOIN mara ON mara~matnr = vbap~matnr
*      INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
*        WHERE zsdt0082~vkbur  IN c_vkbur
*          AND zsdt0082~vkorg  IN c_vkorg
*          AND zsdt0082~spart  EQ c_spart
*          AND vbak~kunnr IN c_kunnr
*          AND zsdt0082~seq    NE 1
*          AND ( zsdt0082~status EQ 2 OR
*                zsdt0082~status EQ 5 ).
*
*  ENDIF.

  IF it_f4_wrkst_aux IS NOT INITIAL.

    SELECT *
      FROM vbkd
      INTO TABLE it_vbkd
      FOR ALL ENTRIES IN it_f4_wrkst_aux
      WHERE vbeln EQ it_f4_wrkst_aux-vbeln.

    SELECT *
       FROM zsdt0131
       INTO TABLE it_zsdt0131_aux
       FOR ALL ENTRIES IN it_f4_wrkst_aux
       WHERE nro_sol EQ it_f4_wrkst_aux-nro_sol
         AND seq     EQ it_f4_wrkst_aux-seq
         AND status  NE 'X'.

  ENDIF.

  LOOP AT it_f4_wrkst_aux INTO wa_f4_wrkst_aux.

    vl_cont = vl_cont + 1.

    READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_f4_wrkst_aux-vbeln.
    IF sy-subrc IS INITIAL.
      wa_f4_wrkst_aux-inco1 = wa_vbkd-inco1.
    ENDIF.

    LOOP AT it_zsdt0131_aux INTO wa_zsdt0131_aux WHERE nro_sol EQ wa_f4_wrkst_aux-nro_sol.
      vl_qtd_marca_sem = vl_qtd_marca_sem + wa_zsdt0131_aux-qtd_vinc.
    ENDLOOP.

    wa_f4_wrkst_aux-saldo = wa_f4_wrkst_aux-qte_lib - vl_qtd_marca_sem.

    IF wa_f4_wrkst_aux-saldo LE 0.
      wa_f4_wrkst_aux-inco1 = '999'.
    ENDIF.

    CLEAR: vl_qtd_marca_sem.

    MODIFY it_f4_wrkst_aux FROM wa_f4_wrkst_aux INDEX vl_cont.

  ENDLOOP.

  DELETE it_f4_wrkst_aux WHERE inco1 EQ '999' OR inco1 NE wa_header_lote-inco1.

  SORT it_f4_wrkst_aux BY wrkst DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_f4_wrkst_aux COMPARING wrkst.

  CLEAR: it_f4.

  LOOP AT it_f4_wrkst_aux INTO wa_f4_wrkst_aux.
    wa_f4-wrkst = wa_f4_wrkst_aux-wrkst.
    APPEND wa_f4 TO it_f4.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield   = 'WRKST'
*     PVALKEY    = ' '
*     DYNPPROG   = ' '
*     DYNPNR     = ' '
*     DYNPROFIELD            = ' '
*     STEPL      = 0
*     WINDOW_TITLE           =
*     VALUE      = ' '
      value_org  = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY    = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB   =
* IMPORTING
*     USER_RESET =
    TABLES
      value_tab  = it_f4
*     FIELD_TAB  =
      return_tab = it_return.

  IF sy-subrc EQ 0.
    READ TABLE it_return INTO wa_return INDEX 1.
    WRITE wa_return-fieldval TO wa_header_lote-wrkst.
  ENDIF.

ENDMODULE.
