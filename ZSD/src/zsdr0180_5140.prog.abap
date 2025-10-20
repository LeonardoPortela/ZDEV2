*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5140
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_sol_5140,
         check          TYPE char1,
         matnr          TYPE mara-matnr,
         maktx          TYPE makt-maktx,
         saldo          TYPE zsdt0082-qte_sol,
         qtd_vinc       TYPE zsdt0082-qte_sol,
         meins          TYPE zsdt0131-um, "VBAP-MEINS,
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
         um             TYPE zsdt0131-um,
         cor(4)         TYPE c.
         INCLUDE STRUCTURE zsdt0082.
TYPES: cellstyles     TYPE lvc_t_styl.
TYPES: END OF ty_sol_5140.

TYPES: BEGIN OF ty_cliente_5140,
         nro_sol     TYPE zsdt0131-nro_sol,
         seq         TYPE zsdt0082-seq,
         kunnr       TYPE zsdt0131-kunnr,
         name1       TYPE kna1-name1,
         seq_entrega TYPE zsdt0130-seq_entrega,
         nr_rot1     TYPE zsdt0130-nr_rot,
         obs         TYPE char7,
         nr_rot2     TYPE zsdt0130-nr_rot,
         antig       TYPE char1,
         cellstyles  TYPE lvc_t_styl.
TYPES: END OF ty_cliente_5140.

DATA: it_sol_5140     TYPE STANDARD TABLE OF ty_sol_5140,
      it_cliente_5140 TYPE STANDARD TABLE OF ty_cliente_5140.

DATA: g_custom_container_5140      TYPE REF TO cl_gui_custom_container,
      dg_splitter_1_5140           TYPE REF TO cl_gui_splitter_container,
      dg_parent_1_5140             TYPE REF TO cl_gui_container,
      dg_parent_2_5140             TYPE REF TO cl_gui_container,
      ctl_alv1_5140                TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5140                TYPE REF TO cl_gui_alv_grid,
      gs_layout_5140_alv1          TYPE lvc_s_layo,
      gs_layout_5140_alv2          TYPE lvc_s_layo,
      it_fieldcatalog_sol_5140     TYPE lvc_t_fcat,
      it_fieldcatalog_cliente_5140 TYPE lvc_t_fcat,
      it_exclude_5140              TYPE ui_functions,
      it_stable_5140               TYPE lvc_s_stbl.

DATA: vl_status TYPE char1.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5140 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      handle_data_changed_5140 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm
          sender,

      toolbar_5140 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5140 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      data_changed_finished_5140 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      handle_hotspot_click  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      handle_button_click_5140 FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no,

      on_f4_5140 FOR EVENT onf4 OF cl_gui_alv_grid
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
CLASS lcl_event_handler_5140 IMPLEMENTATION.

  METHOD handle_data_changed_5140.

    DATA: wa_mod_cells TYPE lvc_s_modi.
    DATA: wa_sol_5140 TYPE ty_sol_5140,
          wa_zsdt0131 TYPE zsdt0131.
    DATA: vl_qtd_vinc    TYPE zsdt0131-qtd_vinc.

    LOOP AT er_data_changed->mt_good_cells INTO wa_mod_cells.

      CASE wa_mod_cells-fieldname.
        WHEN 'QTD_VINC'.

          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = wa_mod_cells-row_id
              i_fieldname = wa_mod_cells-fieldname
            IMPORTING
              e_value     = vl_qtd_vinc.

          READ TABLE it_sol_5140 INTO wa_sol_5140 INDEX wa_mod_cells-row_id.

          SELECT SINGLE *
            FROM zsdt0131
            INTO wa_zsdt0131
            WHERE nro_sol EQ wa_sol_5140-nro_sol
              AND seq EQ wa_sol_5140-seq.

          IF wa_sol_5140-saldo LT vl_qtd_vinc.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = wa_mod_cells-row_id
                i_fieldname = wa_mod_cells-fieldname
                i_value     = wa_sol_5140-qtd_vinc.

            CALL METHOD er_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = 'SU'
                i_msgno     = '000'
                i_msgty     = 'E'
                i_msgv1     = 'Quantidade Impossível'
                i_fieldname = wa_mod_cells-fieldname
                i_row_id    = wa_mod_cells-row_id.

          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.



  ENDMETHOD.                    "handle_data_changed

  METHOD toolbar_5140.

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

  METHOD user_command_5140.

    IF e_ucomm = 'DEL'.

      DATA: it_selected_rows TYPE lvc_t_row,
            wa_selected_rows TYPE lvc_s_row,
            wa_sol_5140      TYPE ty_sol_5140,
            wa_clientes_5140 TYPE ty_cliente_5140,
            vl_cont          TYPE i,
            chave            TYPE zde_chave_sol.

      CALL METHOD ctl_alv1_5140->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_sol_5140 INTO wa_sol_5140.
        READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix."INDEX SY-TABIX.
        IF sy-subrc IS INITIAL.
          wa_sol_5140-name1 = 'DELETE'.
          MODIFY it_sol_5140 FROM wa_sol_5140 INDEX wa_selected_rows-index.

          CONCATENATE wa_sol_5140-nro_sol wa_sol_5140-seq wa_sol_5140-vbeln wa_sol_5140-posnr INTO chave.

          CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
            EXPORTING
              chave = chave.

        ENDIF.
      ENDLOOP.

      LOOP AT it_cliente_5140 INTO wa_clientes_5140.
        vl_cont = vl_cont + 1.
        READ TABLE it_sol_5140 INTO wa_sol_5140 WITH KEY nro_sol = wa_clientes_5140-nro_sol
                                                         name1   = 'DELETE'.
        IF sy-subrc IS INITIAL.
          wa_clientes_5140-name1 = 'DELETE'.
          MODIFY it_cliente_5140 FROM wa_clientes_5140 INDEX vl_cont.
        ENDIF.
      ENDLOOP.

      DELETE it_sol_5140 WHERE name1 EQ 'DELETE'.
      DELETE it_cliente_5140 WHERE name1 EQ 'DELETE'.

      PERFORM atualiza_header_5140.
      CLEAR: vl_cont.

      vg_subt_lote = '5140'.
      LEAVE TO SCREEN 5000.

    ENDIF.
  ENDMETHOD.                    "USER_COMMAND

  METHOD data_changed_finished_5140.

    DATA: wa_good_cells TYPE lvc_s_modi.

    READ TABLE et_good_cells INTO wa_good_cells INDEX 1.

    IF e_modified EQ abap_true.
      IF wa_good_cells-fieldname EQ 'QTD_VINC'.
        PERFORM atualiza_header_5140.
        vg_subt_lote = '5140'.
        LEAVE TO SCREEN 5000.
      ELSEIF wa_good_cells-fieldname EQ 'SEQ_ENTREGA'.
        PERFORM atualiza_seq_entrega_5140 USING wa_good_cells.
        vg_subt_lote = '5140'.
        LEAVE TO SCREEN 5000.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD handle_hotspot_click.

    DATA: wa_clientes_5140_anexo TYPE ty_cliente_5140.
    DATA: wa_bor TYPE borident.

    DATA: wl_name  TYPE thead-tdname.

    IF e_column_id-fieldname EQ 'NR_ROT2'.

      READ TABLE it_cliente_5140 INTO wa_clientes_5140_anexo INDEX e_row_id-index.
      IF sy-subrc IS INITIAL.
        wa_bor-objkey = wa_clientes_5140_anexo-nr_rot2.
        wa_bor-objtype = 'ZSDR0061'.
        PERFORM chama_anexo USING wa_bor.
      ENDIF.

    ELSEIF e_column_id-fieldname EQ 'NR_ROT1'.

      READ TABLE it_cliente_5140 INTO wa_clientes_5140_anexo INDEX e_row_id-index.
      IF sy-subrc IS INITIAL.
        wl_name = wa_clientes_5140_anexo-nr_rot1.
        PERFORM chama_texto_rot USING wl_name.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

  METHOD handle_button_click_5140.

    DATA: wa_clientes_5140_obs TYPE ty_cliente_5140.
    DATA: wl_name  TYPE thead-tdname.

    IF es_col_id-fieldname EQ 'OBS'.

      READ TABLE it_cliente_5140 INTO wa_clientes_5140_obs INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL.
        CONCATENATE wa_clientes_5140_obs-nro_sol '001' INTO wl_name.
        PERFORM chama_texto_obs USING wl_name.
      ENDIF.
    ENDIF.

  ENDMETHOD .

  METHOD on_f4_5140.

    PERFORM f4_local_embarque USING er_event_data es_row_no.

  ENDMETHOD.                                                "on_f4

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5140  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_5140 OUTPUT.

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
      IF it_sol_5140 IS NOT INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
    ENDIF.
    IF screen-group1 EQ 'EX1' AND wa_header_lote IS INITIAL.
      screen-input = 0.
    ENDIF.






    MODIFY SCREEN.

  ENDLOOP.

  IF g_custom_container_5140 IS INITIAL.

    CREATE OBJECT g_custom_container_5140
      EXPORTING
        container_name              = 'CONTAINER5140'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1_5140
      EXPORTING
        parent  = g_custom_container_5140
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1_5140->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5140.

    CALL METHOD dg_splitter_1_5140->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5140.

    CALL METHOD dg_splitter_1_5140->set_row_mode
      EXPORTING
        mode = dg_splitter_1_5140->mode_relative.

    CALL METHOD dg_splitter_1_5140->set_row_height
      EXPORTING
        id     = 1
        height = 65.

    CALL METHOD dg_splitter_1_5140->set_row_height
      EXPORTING
        id     = 2
        height = 35.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5140 USING:
          01 'NRO_SOL'      'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Solic',
          02 'VBELN'        'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem',
          03 'POSNR'        'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item',
          04 'AUART'        'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tipo OV',
          05 'VKORG'        'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Org. Vendas',
          06 'VKBUR'        'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Vendas',
          07 'SPART'        'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Setor Atv',
          08 'WERKS'        'ZSDT0082'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Centro Forn',
          09 'MATNR'        'MARA'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Material',
          10 'MAKTX'        'MAKT'      ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Descrição',
          11 'SALDO'        ''          ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Saldo à Formar Lote',
          12 'QTD_VINC'     'ZSDT0131'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. do Lote',
          13 'UM'           'ZSDT0131'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
          14 'LIFNR'        'ZSDT0132'  'C310'  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Fornecedor',
          15 'COD_LOC_EMB'  'ZSDT0131'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Loc. Emb.',
          16 'LOCAL_EMBARQ' 'ZSDT0131'  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Loc. Emb.',
          17 'ARMAZEM'        'ZSDT0132' ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Armazém.',
          18 'TRANSPORTADORA' 'ZSDT0132' ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transportadora.',
          19 'TRANSP_RESP'    'ZSDT0132' ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Transp. Resp.'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_cliente_5140 USING:
           01 'NRO_SOL'      'ZSDT0082'        ''  ' '     ''     ' '   'X'   ' '   ' '   ' '   'Nro Sol',
           02 'KUNNR'        'ZSDT0082'        ''  ' '     ''     ' '   'X'   ' '   ' '   ' '   'Cód Cliente',
           03 'NAME1'        'LFA1'            ''  ' '     ''     ' '   'X'   ' '   ' '   ' '   'Nome',
           04 'SEQ_ENTREGA'  ''        ''  'X'     ''     ' '   'X'   ' '   ' '   ' '   'Seq Entrega',
           05 'NR_ROT1'      ''        ''  ' '     ''     'X'   'X'   ' '   ' '   ' '   'Roteiro',
           06 'OBS'          ''                ''  ' '     'X'     ' '   'X'   ' '   ' '   ' '   'Observação',
           07 'NR_ROT2'      ''        ''  ' '     ''     'X'   'X'   ' '   ' '   ' '   'Doc Anexo'.

    gs_layout_5140_alv1-sel_mode   = 'A'.
    gs_layout_5140_alv1-stylefname = 'CELLSTYLES'.
    gs_layout_5140_alv1-cwidth_opt = 'X'.
    gs_layout_5140_alv1-grid_title = 'Ordens com Solicitação de Entrega'.
    gs_layout_5140_alv1-smalltitle = 'X'.
    gs_layout_5140_alv2-sel_mode   = 'A'.
    gs_layout_5140_alv2-stylefname = 'CELLSTYLES'.
    gs_layout_5140_alv2-cwidth_opt = 'X'.
    gs_layout_5140_alv2-grid_title = 'Clientes das Ordens'.
    gs_layout_5140_alv2-smalltitle = 'X'.

    CREATE OBJECT ctl_alv1_5140
      EXPORTING
        i_parent = dg_parent_1_5140.           "ALV Lote

    CREATE OBJECT ctl_alv2_5140
      EXPORTING
        i_parent = dg_parent_2_5140.           "ALV Oderm

    PERFORM excluir_botoes CHANGING it_exclude_5140.
    PERFORM registrar_f4_5140.

    SET HANDLER:
      lcl_event_handler_5140=>toolbar_5140 FOR ctl_alv1_5140,
      lcl_event_handler_5140=>user_command_5140 FOR ctl_alv1_5140,
      lcl_event_handler_5140=>handle_data_changed_5140 FOR ctl_alv1_5140,
      lcl_event_handler_5140=>data_changed_finished_5140 FOR ctl_alv1_5140,
      lcl_event_handler_5140=>on_f4_5140 FOR ctl_alv1_5140.

    CALL METHOD ctl_alv1_5140->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5140_alv1
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5140
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_sol_5140
        it_outtab            = it_sol_5140.

    SET HANDLER:
      lcl_event_handler_5140=>handle_data_changed_5140 FOR ctl_alv2_5140,
      lcl_event_handler_5140=>data_changed_finished_5140 FOR ctl_alv2_5140,
      lcl_event_handler_5140=>handle_hotspot_click FOR ctl_alv2_5140,
      lcl_event_handler_5140=>handle_button_click_5140 FOR ctl_alv2_5140.

    CALL METHOD ctl_alv2_5140->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5140_alv2
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5140
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_cliente_5140
        it_outtab            = it_cliente_5140.

    CALL METHOD ctl_alv1_5140->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    CALL METHOD ctl_alv2_5140->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    it_stable_5140-row = 'X'.
    it_stable_5140-col = 'X'.

    CALL METHOD ctl_alv1_5140->refresh_table_display
      EXPORTING
        is_stable = it_stable_5140.

    CALL METHOD ctl_alv2_5140->refresh_table_display
      EXPORTING
        is_stable = it_stable_5140.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5140  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5140 INPUT.

  ctl_alv1_5140->check_changed_data( ).

  CASE sy-ucomm.
    WHEN 'F85140'.
      CALL SCREEN 5141 STARTING AT 5 5 ENDING AT 160 30.
    WHEN 'SAVE'.
      CLEAR: vl_check.
      PERFORM check_parametro_5140 CHANGING vl_check.
      IF vl_check IS INITIAL.
        PERFORM salva_lote_5140.
        PERFORM clear_5140.
      ENDIF.
    WHEN 'DELETELOTE'.
      PERFORM delete_lote.
    WHEN 'ENTER'.
      PERFORM completa_motorista CHANGING wa_header_lote.
*      PERFORM COMPLETA_MOTORISTA_5140.
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

**&---------------------------------------------------------------------*
**&      Form  SALVA_LOTE
**&---------------------------------------------------------------------*
FORM salva_lote_5140.

  DATA: wa_zsdt0129         TYPE zsdt0129,
        wa_zsdt0130         TYPE zsdt0130,
        wa_zsdt0131         TYPE zsdt0131,
        it_zsdt0131_ex      TYPE STANDARD TABLE OF zsdt0131,
        it_zsdt0131_retorno TYPE STANDARD TABLE OF zsdt0131,
        wa_zsdt0131_ex      TYPE zsdt0131,
        it_zsdt0130_ex      TYPE STANDARD TABLE OF zsdt0130,
        wa_zsdt0130_ex      TYPE zsdt0130,
        wa_zsdt0082         TYPE zsdt0082,
        wa_zsdt0082_retorno TYPE zsdt0082,
        vl_erro             TYPE char1.

  DATA: wa_sol_5140      TYPE ty_sol_5140,
        wa_clientes_5140 TYPE ty_cliente_5140.



  wa_zsdt0129-nro_lote      = vg_lote_editar.
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
  wa_zsdt0129-status        = vl_status.
  wa_zsdt0129-usnam         = sy-uname.
  wa_zsdt0129-data_atual    = sy-datum.
  wa_zsdt0129-hora_atual    = sy-uzeit.

  MODIFY zsdt0129 FROM wa_zsdt0129.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE TEXT-017 TYPE 'S' DISPLAY LIKE 'E'.
    vl_erro = abap_true.
  ENDIF.

  LOOP AT it_sol_5140 INTO wa_sol_5140.

    wa_zsdt0130-nro_sol = wa_sol_5140-nro_sol.
    wa_zsdt0130-seq = wa_sol_5140-seq.
    wa_zsdt0130-nro_lote = wa_zsdt0129-nro_lote.

    READ TABLE it_cliente_5140 INTO wa_clientes_5140 WITH KEY nro_sol = wa_sol_5140-nro_sol
                                                              seq     = wa_sol_5140-seq.

    IF sy-subrc IS INITIAL.
      wa_zsdt0130-kunnr = wa_clientes_5140-kunnr.
      wa_zsdt0130-seq_entrega  = wa_clientes_5140-seq_entrega.
      wa_zsdt0130-nr_rot = wa_clientes_5140-nr_rot1.
    ENDIF.

    wa_zsdt0130-status = vl_status.
    wa_zsdt0130-usnam = sy-uname.
    wa_zsdt0130-data_atual = sy-datum.
    wa_zsdt0130-hora_atual = sy-uzeit.

    wa_zsdt0131-nro_lote   = wa_zsdt0129-nro_lote.
    wa_zsdt0131-nro_sol    = wa_sol_5140-nro_sol.
    wa_zsdt0131-seq        = wa_sol_5140-seq.
    wa_zsdt0131-kunnr      = wa_clientes_5140-kunnr.
    wa_zsdt0131-vbeln      = wa_sol_5140-vbeln.
    wa_zsdt0131-posnr      = wa_sol_5140-posnr.
    wa_zsdt0131-auart      = wa_sol_5140-auart.
    wa_zsdt0131-spart      = wa_sol_5140-spart.
    wa_zsdt0131-vkorg      = wa_sol_5140-vkorg.
    wa_zsdt0131-vkbur      = wa_sol_5140-vkbur.
    wa_zsdt0131-werks      = wa_sol_5140-werks.
    wa_zsdt0131-matnr      = wa_sol_5140-matnr.
    wa_zsdt0131-qtd_vinc   = wa_sol_5140-qtd_vinc.
    wa_zsdt0131-um         = wa_sol_5140-um."WA_SOL_5140-MEINS.
    wa_zsdt0131-brgew      = wa_sol_5140-brgew.
    wa_zsdt0131-qtd_emkg   = wa_sol_5140-qtd_vinc * wa_sol_5140-brgew. "WA_HEADER_LOTE-QTD_TOTAL_KG.
    wa_zsdt0131-cod_loc_emb = wa_sol_5140-cod_loc_emb.
    wa_zsdt0131-local_embarq = wa_sol_5140-local_embarq.
    wa_zsdt0131-status     = vl_status.
    wa_zsdt0131-usnam      = sy-uname.
    wa_zsdt0131-data_atual = sy-datum.
    wa_zsdt0131-hora_atual = sy-uzeit.

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
      WHERE nro_sol = wa_sol_5140-nro_sol
        AND seq     = wa_sol_5140-seq
        AND vbeln   = wa_sol_5140-vbeln
        AND posnr   = wa_sol_5140-posnr.

    IF sy-subrc IS INITIAL.
      wa_zsdt0082-status = '5'.
    ENDIF.

    MODIFY zsdt0082 FROM wa_zsdt0082.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE TEXT-020 TYPE 'S' DISPLAY LIKE 'E'.
      vl_erro = abap_true.
    ENDIF.

  ENDLOOP.

  SELECT *
    FROM zsdt0131
    INTO TABLE it_zsdt0131_ex
    WHERE nro_lote EQ vg_lote_editar.

  LOOP AT it_zsdt0131_ex INTO wa_zsdt0131_ex.
    READ TABLE it_sol_5140 INTO wa_sol_5140 WITH KEY nro_sol = wa_zsdt0131_ex-nro_sol
                                                     seq     = wa_zsdt0131_ex-seq.
    IF sy-subrc IS NOT INITIAL AND wa_zsdt0131_ex-status NE 'X'.

      CLEAR: it_zsdt0131_retorno.

      SELECT *
        FROM zsdt0131
        INTO TABLE it_zsdt0131_retorno
        WHERE nro_sol  EQ wa_zsdt0131_ex-nro_sol
          AND seq      EQ wa_zsdt0131_ex-seq
          AND nro_lote NE wa_zsdt0131_ex-nro_lote
          AND status   NE 'X'.

      IF it_zsdt0131_retorno IS INITIAL.

        CLEAR: wa_zsdt0082_retorno.

        SELECT SINGLE *
          FROM zsdt0082
          INTO wa_zsdt0082_retorno
          WHERE nro_sol EQ wa_zsdt0131_ex-nro_sol
            AND seq     EQ wa_zsdt0131_ex-seq
            AND vbeln   EQ wa_zsdt0131_ex-vbeln
            AND posnr   EQ wa_zsdt0131_ex-posnr.

        IF wa_zsdt0082_retorno IS NOT INITIAL.
          IF wa_zsdt0082_retorno-status EQ 5.
            wa_zsdt0082_retorno-status = 2.
            MODIFY zsdt0082 FROM wa_zsdt0082_retorno.
          ENDIF.
        ENDIF.

      ENDIF.


      wa_zsdt0131_ex-status = 'X'.
      wa_zsdt0131_ex-user_canc = sy-uname.
      wa_zsdt0131_ex-dt_canc = sy-datum.
      wa_zsdt0131_ex-hr_can = sy-uzeit.
      MODIFY zsdt0131 FROM wa_zsdt0131_ex.
    ENDIF.
  ENDLOOP.

  SELECT *
      FROM zsdt0130
      INTO TABLE it_zsdt0130_ex
      WHERE nro_lote EQ vg_lote_editar.

  LOOP AT it_zsdt0130_ex INTO wa_zsdt0130_ex.
    READ TABLE it_cliente_5140 INTO wa_clientes_5140 WITH KEY nro_sol = wa_zsdt0130_ex-nro_sol.
    IF sy-subrc IS NOT INITIAL AND wa_zsdt0130_ex-status NE 'X'.
      wa_zsdt0130_ex-status = 'X'.
      MODIFY zsdt0130 FROM wa_zsdt0130_ex.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'ZDENQUEUE_SD_LOTE_INSUMOS'
    EXPORTING
      chave = vg_lote_editar.

  IF vl_erro IS INITIAL.
    PERFORM desbloqueia_sol_5140.
    PERFORM clear_5140.
    MESSAGE s000(z_fi) WITH 'Lote' wa_zsdt0129-nro_lote 'salvo com sucesso' .
  ENDIF.

  CLEAR: wa_zsdt0129, wa_zsdt0130, wa_zsdt0131, wa_zsdt0082.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_PARAMETRO_5140
*&---------------------------------------------------------------------*
FORM check_parametro_5140 CHANGING vl_check.

  DATA: wa_sol_5140           TYPE ty_sol_5140,
        wa_cliente_5140       TYPE ty_cliente_5140,
        it_cliente_5140_check TYPE STANDARD TABLE OF ty_cliente_5140,
        it_sol_5140_check     TYPE STANDARD TABLE OF ty_sol_5140,
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
    "Check de Placas
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
    FOR ALL ENTRIES IN @it_sol_5140
    WHERE nr_rot EQ @it_sol_5140-cod_loc_emb.
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
  IF it_sol_5140 IS INITIAL.
    MESSAGE TEXT-014 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
  ELSE.
    LOOP AT it_sol_5140 INTO wa_sol_5140.
      "Check solicitação com mais volume solicitado do que disponível
      IF wa_sol_5140-qtd_vinc > wa_sol_5140-saldo.
        MESSAGE TEXT-015 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        "Check solicitação com mais volume solicitado menor ou igual a zero
      ELSEIF wa_sol_5140-qtd_vinc LE 0.
        MESSAGE TEXT-015 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDLOOP.

    "cHECK SE POSSUI MAIS DE UM VKBUR POR LOTE
    it_sol_5140_check = it_sol_5140.
    SORT it_sol_5140_check BY vkbur.
    DELETE ADJACENT DUPLICATES FROM it_sol_5140_check COMPARING vkbur.
    DESCRIBE TABLE it_sol_5140_check LINES vl_lines.

    IF vl_lines NE 1.
      MESSAGE TEXT-121 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ENDIF.

    LOOP AT it_cliente_5140 INTO wa_cliente_5140.
      "Check solicitação sem sequência de entrega
      IF wa_cliente_5140-seq_entrega IS INITIAL.
        MESSAGE TEXT-016 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDLOOP.

    "Check solicitação sem sequência correta
    it_cliente_5140_check = it_cliente_5140.
    SORT it_cliente_5140_check BY seq_entrega ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_cliente_5140_check COMPARING seq_entrega.
    LOOP AT it_cliente_5140_check INTO wa_cliente_5140.
      IF wa_cliente_5140-seq_entrega NE sy-tabix.
        MESSAGE TEXT-029 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_5140
*&---------------------------------------------------------------------*
FORM clear_5140 .
  CLEAR: it_sol_5140, it_cliente_5140, wa_header_lote.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_HEADER_5140
*&---------------------------------------------------------------------*
FORM completa_header_5140 .

  DATA: wa_zsdt0129     TYPE zsdt0129,
        it_zsdt0130     TYPE STANDARD TABLE OF zsdt0130,
        it_zsdt0132     TYPE STANDARD TABLE OF zsdt0132,
        wa_zsdt0130     TYPE zsdt0130,
        wa_zsdt0132     TYPE zsdt0132,
        wa_sol_5140     TYPE ty_sol_5140,
        it_zsdt0082     TYPE STANDARD TABLE OF zsdt0082,
        it_zsdt0131     TYPE STANDARD TABLE OF zsdt0131,
        wa_zsdt0082     TYPE zsdt0082,
        wa_zsdt0131     TYPE zsdt0131,
        wa_cliente_5140 TYPE ty_cliente_5140,
        it_lfa1         TYPE STANDARD TABLE OF lfa1,
        wa_lfa1         TYPE lfa1,
        it_kna1         TYPE STANDARD TABLE OF kna1,
        wa_kna1         TYPE kna1,
        it_makt         TYPE STANDARD TABLE OF makt,
        wa_makt         TYPE makt,
        vl_cont         TYPE i.

  DATA: ls_style TYPE lvc_s_styl,
        wl_name  TYPE thead-tdname,
        tg_texto TYPE STANDARD TABLE OF tline.

  SELECT SINGLE *
    FROM zsdt0129
    INTO wa_zsdt0129
    WHERE nro_lote EQ vg_lote_editar.

  SELECT *
      FROM zsdt0131
      INTO CORRESPONDING FIELDS OF TABLE it_sol_5140
      WHERE nro_lote EQ vg_lote_editar
        AND status NE 'X'.

  SELECT *
      FROM zsdt0130
      INTO CORRESPONDING FIELDS OF TABLE it_cliente_5140
      WHERE nro_lote EQ vg_lote_editar
        AND status NE 'X'.

  SELECT *
    FROM lfa1
    INTO TABLE it_lfa1
    WHERE lifnr EQ wa_zsdt0129-motorista.

  SELECT *
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_sol_5140
    WHERE matnr EQ it_sol_5140-matnr.

  SELECT *
    FROM zsdt0132
    INTO TABLE it_zsdt0132
    FOR ALL ENTRIES IN it_sol_5140
    WHERE nr_rot EQ it_sol_5140-cod_loc_emb.

  SELECT *
    FROM zsdt0082
    INTO TABLE it_zsdt0082
    FOR ALL ENTRIES IN it_sol_5140
    WHERE nro_sol EQ it_sol_5140-nro_sol
      AND seq EQ it_sol_5140-seq.

  IF it_zsdt0082 IS NOT INITIAL.

    SELECT *
      FROM zsdt0131
      INTO TABLE it_zsdt0131
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE nro_sol EQ it_zsdt0082-nro_sol
        AND status NE 'X'.

  ENDIF.

  SELECT *
    FROM zsdt0130
    INTO TABLE it_zsdt0130
    WHERE nro_lote EQ vg_lote_editar
      AND status NE 'X'.

  SELECT *
    FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_cliente_5140
    WHERE kunnr EQ it_cliente_5140-kunnr.

  "Atualiza Cabeçalho
  CLEAR: vl_status.

  wa_header_lote-marca                = wa_zsdt0129-marca.
  wa_header_lote-placa_cav            = wa_zsdt0129-placa_cav.
  wa_header_lote-placa_car1           = wa_zsdt0129-placa_car1.
  wa_header_lote-placa_car2           = wa_zsdt0129-placa_car2.
*-CS2019001891 - JT - 04.02.2021 - inicio
  wa_header_lote-placa_car3           = wa_zsdt0129-placa_car3.
*-CS2019001891 - JT - 04.02.2021 - fim
  wa_header_lote-motorista            = wa_zsdt0129-motorista.

  READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_header_lote-motorista.
  IF sy-subrc IS INITIAL.
    wa_header_lote-mot_desc = wa_lfa1-name1.
    wa_header_lote-stcd2 = wa_lfa1-stcd2.
    wa_header_lote-telf1 = wa_lfa1-telf1.
  ENDIF.

  wa_header_lote-qtd_total_kg         = wa_zsdt0129-qtd_total_kg.
  wa_header_lote-ctg_transp           = wa_zsdt0129-ctg_transp.
  vl_status = wa_zsdt0129-status.

  IF wa_zsdt0129-ctg_transp EQ 'A'.
    wa_header_lote-ctg_transp_d = 'AVULSO'.
  ELSEIF wa_zsdt0129-ctg_transp EQ 'B'.
    wa_header_lote-ctg_transp_d = 'VEÍCULO 27'.
  ELSEIF wa_zsdt0129-ctg_transp EQ 'C'.
    wa_header_lote-ctg_transp_d = 'VEÍCULO 32'.
  ELSEIF wa_zsdt0129-ctg_transp EQ 'D'.
    wa_header_lote-ctg_transp_d = 'VEÍCULO 37'.
  ELSEIF wa_zsdt0129-ctg_transp EQ 'E'.
    wa_header_lote-ctg_transp_d = 'VEÍCULO 50'.
  ENDIF.

  wa_header_lote-inco1                = wa_zsdt0129-inco1.
  wa_header_lote-wrkst                = wa_zsdt0129-marca.
  wa_header_lote-dt_entrega           = wa_zsdt0129-dt_entrega.

  "Atualiza Solicitações
  LOOP AT it_sol_5140 INTO wa_sol_5140.
    vl_cont = vl_cont + 1.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_sol_5140-matnr.
    IF sy-subrc IS INITIAL.
      wa_sol_5140-maktx = wa_makt-maktx.
    ENDIF.

    READ TABLE it_zsdt0132 INTO wa_zsdt0132 WITH KEY nr_rot = wa_sol_5140-cod_loc_emb.
    IF sy-subrc IS INITIAL.
      wa_sol_5140-lifnr          = wa_zsdt0132-lifnr.
      wa_sol_5140-armazem        = wa_zsdt0132-armazem.
      wa_sol_5140-transportadora = wa_zsdt0132-transportadora.
      wa_sol_5140-transp_resp    = wa_zsdt0132-transp_resp.
    ENDIF.

    READ TABLE it_zsdt0082 INTO wa_zsdt0082 WITH KEY nro_sol = wa_sol_5140-nro_sol
                                                     seq     = wa_sol_5140-seq.

    LOOP AT it_zsdt0131 INTO wa_zsdt0131 WHERE nro_sol EQ wa_zsdt0082-nro_sol.
      vl_qtd_usada = vl_qtd_usada + wa_zsdt0131-qtd_vinc.
    ENDLOOP.

    wa_sol_5140-saldo = wa_zsdt0082-qte_lib - vl_qtd_usada + wa_sol_5140-qtd_vinc.

    MODIFY it_sol_5140 FROM wa_sol_5140 INDEX vl_cont.
    CLEAR: vl_qtd_usada.
  ENDLOOP.

  CLEAR: vl_cont.

  "Atualiza Cliente
  LOOP AT it_cliente_5140 INTO wa_cliente_5140.
    vl_cont = vl_cont + 1.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_cliente_5140-kunnr.
    IF sy-subrc IS INITIAL.
      wa_cliente_5140-name1 = wa_kna1-name1.
    ENDIF.

    READ TABLE it_zsdt0130 INTO wa_zsdt0130 WITH KEY nro_sol  = wa_cliente_5140-nro_sol
                                                     seq      = wa_cliente_5140-seq.
    IF sy-subrc IS INITIAL.
      wa_cliente_5140-nr_rot1 = wa_zsdt0130-nr_rot.
      wa_cliente_5140-nr_rot2 = wa_zsdt0130-nr_rot.
    ENDIF.

    ls_style-fieldname = 'OBS'.
    ls_style-style = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_style TO wa_cliente_5140-cellstyles.

    CONCATENATE wa_cliente_5140-nro_sol '001' INTO wl_name.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'OBSE'
        language                = sy-langu
        name                    = wl_name
        object                  = 'ZTEXTO'
      TABLES
        lines                   = tg_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF tg_texto IS INITIAL.
      wa_cliente_5140-obs = '@1F@'.
    ELSE.
      wa_cliente_5140-obs = '@1E@'.
    ENDIF.

    MODIFY it_cliente_5140 FROM wa_cliente_5140 INDEX vl_cont.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETE_LOTE
*&---------------------------------------------------------------------*
FORM delete_lote .

  DATA: it_zsdt0129        TYPE STANDARD TABLE OF zsdt0129,
        it_zsdt0130        TYPE STANDARD TABLE OF zsdt0130,
        it_zsdt0131        TYPE STANDARD TABLE OF zsdt0131,
        it_zsdt0131_status TYPE STANDARD TABLE OF zsdt0131,
        wa_zsdt0082        TYPE zsdt0082,
        wa_zsdt0129        TYPE zsdt0129,
        wa_zsdt0130        TYPE zsdt0130,
        wa_zsdt0131        TYPE zsdt0131,
        answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Excluir Lote'
      text_question         = 'O Lote será excluído. Prosseguir?'
      text_button_1         = 'Sim'(023)
      text_button_2         = 'Não'(024)
      default_button        = '1'
      display_cancel_button = ' '
    IMPORTING
      answer                = answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF answer = '1'.

    SELECT *
      FROM zsdt0129
      INTO TABLE it_zsdt0129
      WHERE nro_lote EQ vg_lote_editar.

    SELECT *
      FROM zsdt0130
      INTO TABLE it_zsdt0130
      WHERE nro_lote EQ vg_lote_editar
        AND status NE 'X'.

    SELECT *
      FROM zsdt0131
      INTO TABLE it_zsdt0131
      WHERE nro_lote EQ vg_lote_editar
        AND status NE 'X'.

    "Seleciona solicitações que não devem ser retornadas para status '2' na ZSDT0082
    SELECT *
      FROM zsdt0131
      INTO TABLE it_zsdt0131_status
      FOR ALL ENTRIES IN it_zsdt0131
      WHERE nro_sol  = it_zsdt0131-nro_sol
        AND seq      = it_zsdt0131-seq
        AND vbeln    = it_zsdt0131-vbeln
        AND posnr    = it_zsdt0131-posnr
        AND status   NE 'X'
        AND nro_lote NE it_zsdt0131-nro_lote.

    LOOP AT it_zsdt0129 INTO wa_zsdt0129.
      wa_zsdt0129-user_canc = sy-uname.
      wa_zsdt0129-dt_canc   = sy-datum.
      wa_zsdt0129-hr_can    = sy-uzeit.
      wa_zsdt0129-status    = 'X'.
      MODIFY zsdt0129 FROM wa_zsdt0129.
    ENDLOOP.

    LOOP AT it_zsdt0130 INTO wa_zsdt0130.
      wa_zsdt0130-status = 'X'.
      MODIFY zsdt0130 FROM wa_zsdt0130.
    ENDLOOP.

    LOOP AT it_zsdt0131 INTO wa_zsdt0131.

      READ TABLE it_zsdt0131_status WITH KEY nro_sol = wa_zsdt0131-nro_sol
                                             seq     = wa_zsdt0131-seq
                                             vbeln   = wa_zsdt0131-vbeln
                                             posnr   = wa_zsdt0131-posnr TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE *
          FROM zsdt0082
          INTO wa_zsdt0082
          WHERE nro_sol = wa_zsdt0131-nro_sol
            AND seq     = wa_zsdt0131-seq
            AND vbeln   = wa_zsdt0131-vbeln
            AND posnr   = wa_zsdt0131-posnr.

        IF sy-subrc IS INITIAL.
          wa_zsdt0082-status = '2'.
          MODIFY zsdt0082 FROM wa_zsdt0082.
        ENDIF.

      ENDIF.

      wa_zsdt0131-user_canc = sy-uname.
      wa_zsdt0131-dt_canc   = sy-datum.
      wa_zsdt0131-hr_can    = sy-uzeit.
      wa_zsdt0131-status    = 'X'.

      MODIFY zsdt0131 FROM wa_zsdt0131.

    ENDLOOP.

    PERFORM desbloqueia_sol_5140.
    PERFORM clear_5140.
    CLEAR: vg_lote_editar.
    MESSAGE TEXT-025 TYPE 'S'.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_HEADER_5140
*&---------------------------------------------------------------------*
FORM atualiza_header_5140.

  DATA: wa_sol_5140 TYPE ty_sol_5140.

  CLEAR: wa_header_lote-qtd_total_kg.

  LOOP AT it_sol_5140 INTO wa_sol_5140.
    wa_header_lote-qtd_total_kg = wa_header_lote-qtd_total_kg + wa_sol_5140-qtd_vinc * wa_sol_5140-brgew.
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
*&      Form  ATUALIZA_SEQ_ENTREGA_5140
*&---------------------------------------------------------------------*
FORM atualiza_seq_entrega_5140 USING wa_good_cells TYPE lvc_s_modi.

  DATA: vl_roteiro      TYPE zsdt0130-nr_rot,
        wa_cliente_5140 TYPE ty_cliente_5140.

  READ TABLE it_cliente_5140 INTO wa_cliente_5140 INDEX wa_good_cells-row_id.

  vl_roteiro = wa_cliente_5140-nr_rot1.

  LOOP AT it_cliente_5140 INTO wa_cliente_5140 WHERE nr_rot1 EQ vl_roteiro.
    wa_cliente_5140-seq_entrega = wa_good_cells-value.
    MODIFY it_cliente_5140 FROM wa_cliente_5140 INDEX sy-tabix.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_F4_5140
*&---------------------------------------------------------------------*
FORM registrar_f4_5140 .

  DATA: gs_f4      TYPE lvc_s_f4,
        gt_f4_5140 TYPE lvc_t_f4.

  gs_f4-fieldname  = 'LIFNR'.
  gs_f4-register   = 'X'.
  gs_f4-getbefore  = space.
  gs_f4-chngeafter = space.
  APPEND gs_f4 TO gt_f4_5140.

  CALL METHOD ctl_alv1_5140->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_5140.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_SOL_5140
*&---------------------------------------------------------------------*
FORM desbloqueia_sol_5140.

  DATA: wa_sol_5140 TYPE ty_sol_5140,
        chave       TYPE zde_chave_sol.


  LOOP AT it_sol_5140 INTO wa_sol_5140.

    CONCATENATE wa_sol_5140-nro_sol wa_sol_5140-seq wa_sol_5140-vbeln wa_sol_5140-posnr INTO chave.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = chave.

  ENDLOOP.

ENDFORM.
