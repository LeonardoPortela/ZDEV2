*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5620
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_sol_5620,
         bezei    TYPE tvkbt-bezei,
         kunnr    TYPE kna1-kunnr,
         name1    TYPE kna1-name1,
         matnr    TYPE makt-matnr,
         maktx    TYPE makt-maktx,
         qtd_a_eb TYPE zsdt0137-qtd_vinc,
         qtdvc    TYPE zsdt0082-qte_lib,
         qtdeb    TYPE zsdt0082-qte_lib,
         meins    TYPE vbap-meins,
         inco1    TYPE vbkd-inco1,
         icone    TYPE char6,
         cor(4)   TYPE c.
         INCLUDE STRUCTURE zsdt0082.
       TYPES: END OF ty_sol_5620.

TYPES: BEGIN OF ty_caminhao_5620,
         edit         TYPE char1,
         icone        TYPE char6,
         name1        TYPE lfa1-name1,
         name2        TYPE lfa1-name1,
         dpto_col     TYPE lfa1-name1,
         dpto_ent     TYPE kna1-name1,
         demiss_cte   TYPE lfa1-name1,
         nr_romaneio  TYPE zsdt0001-nr_romaneio,
         aut_embarque TYPE zde_aut_embarque,
         name_pto_col TYPE lfa1-name1,
         cor(4)       TYPE c.
         INCLUDE    STRUCTURE zsdt0138.
         TYPES: cellstyles   TYPE lvc_t_styl.
TYPES: END OF ty_caminhao_5620.

DATA: it_sol_5620          TYPE STANDARD TABLE OF ty_sol_5620,
      it_caminhao_5620     TYPE STANDARD TABLE OF ty_caminhao_5620,
      it_caminhao_aux_5620 TYPE STANDARD TABLE OF ty_caminhao_5620.

DATA: g_custom_container_5620       TYPE REF TO cl_gui_custom_container,
      dg_splitter_1_5620            TYPE REF TO cl_gui_splitter_container,
      dg_parent_1_5620              TYPE REF TO cl_gui_container,
      dg_parent_2_5620              TYPE REF TO cl_gui_container,
      ctl_alv1_5620                 TYPE REF TO cl_gui_alv_grid,
      ctl_alv2_5620                 TYPE REF TO cl_gui_alv_grid,
      gs_layout_5620_alv1           TYPE lvc_s_layo,
      gs_layout_5620_alv2           TYPE lvc_s_layo,
      it_fieldcatalog_sol_5620      TYPE lvc_t_fcat,
      it_fieldcatalog_caminhao_5620 TYPE lvc_t_fcat,
      it_exclude_5620               TYPE ui_functions,
      it_sort_sol_5620              TYPE lvc_t_sort,
      it_sort_caminhao_5620         TYPE lvc_t_sort.

DATA: vg_sol_5620   TYPE ty_sol_5620.
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler_5620 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      data_changed_finished_5620 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      on_double_click_5620 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      toolbar_5620 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      toolbar_caminhao_5620 FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command_5620 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_f4_5620 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display,

*-----CS2019001891 - 08.03.2021 - JT - inicio
      handle_hotspot_click_5620 FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.
*-----CS2019001891 - 08.03.2021 - JT - fim

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler_5620 IMPLEMENTATION.

  METHOD data_changed_finished_5620.

    DATA: wa_good_cells    TYPE lvc_s_modi,
          wa_caminhao_5620 TYPE ty_caminhao_5620,
          wa_lfa1          TYPE lfa1,
          wa_zlest0002     TYPE zlest0002,
          vl_renavam       TYPE zlest0002-cd_renavam..

    LOOP AT et_good_cells INTO wa_good_cells.
      IF e_modified EQ abap_true.
        IF wa_good_cells-fieldname EQ 'COD_TRANSPORTADORA'.

          READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX wa_good_cells-row_id.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
            WHERE lifnr EQ wa_caminhao_5620-cod_transportadora.

          IF sy-subrc IS INITIAL.
            wa_caminhao_5620-name1 = wa_lfa1-name1.
            MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX wa_good_cells-row_id.
          ENDIF.

          CALL METHOD ctl_alv2_5620->refresh_table_display
            EXPORTING
              is_stable = _stable.

        ELSEIF wa_good_cells-fieldname EQ 'MOTORISTA'.

          READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX wa_good_cells-row_id.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
            WHERE lifnr EQ wa_caminhao_5620-motorista.

          IF sy-subrc IS INITIAL.
            wa_caminhao_5620-name2 = wa_lfa1-name1.
            MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX wa_good_cells-row_id.
          ENDIF.

          CALL METHOD ctl_alv2_5620->refresh_table_display
            EXPORTING
              is_stable = _stable.

*-----CS2019001891 - 08.03.2021 - JT - inicio
        ELSEIF wa_good_cells-fieldname EQ 'PTO_COL'.

          READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX wa_good_cells-row_id.

          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
            WHERE lifnr EQ wa_caminhao_5620-pto_col.

          IF sy-subrc IS INITIAL.
            wa_caminhao_5620-name_pto_col = wa_lfa1-name1.
            MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX wa_good_cells-row_id.
          ENDIF.

          CALL METHOD ctl_alv2_5620->refresh_table_display
            EXPORTING
              is_stable = _stable.
*-----CS2019001891 - 08.03.2021 - JT - fim
        ENDIF.

        "-- CS2021000663 25-06-2021 tirar obrigatoriedade de informar placa se
        "tipo de frete for igual CFR ou CPT

        IF vg_sol_5520 IS NOT INITIAL AND ( vg_sol_5520-inco1 NE 'CPT' AND vg_sol_5520-inco1 NE 'CFR').
          IF wa_good_cells-fieldname EQ 'PLACA_CAV'.

            READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX wa_good_cells-row_id.

            IF wa_caminhao_5620 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '0'
                                              CHANGING wa_caminhao_5620-placa_cav
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv2_5620->refresh_table_display
                  EXPORTING
                    is_stable = _stable.
              ENDIF.
            ENDIF.

          ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAR1'.

            READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX wa_good_cells-row_id.

            IF wa_caminhao_5620 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '1'
                                              CHANGING wa_caminhao_5620-placa_car1
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv2_5620->refresh_table_display
                  EXPORTING
                    is_stable = _stable.
              ENDIF.
            ENDIF.

          ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAR2'.

            READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX wa_good_cells-row_id.

            IF wa_caminhao_5620 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '1'
                                              CHANGING wa_caminhao_5620-placa_car2
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv2_5620->refresh_table_display
                  EXPORTING
                    is_stable = _stable.
              ENDIF.
            ENDIF.

          ELSEIF wa_good_cells-fieldname EQ 'PLACA_CAR3'.

            READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX wa_good_cells-row_id.

            IF wa_caminhao_5620 IS NOT INITIAL.
              PERFORM valida_placa_sementes_2 USING '1'
                                              CHANGING wa_caminhao_5620-placa_car3
                                                       vl_check.
              IF vl_check IS NOT INITIAL.
                MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX wa_good_cells-row_id.
                CALL METHOD ctl_alv2_5620->refresh_table_display
                  EXPORTING
                    is_stable = _stable.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_double_click_5620.

    PERFORM click_em_solic_5620 USING e_row-index.
    PERFORM bloqueia_linhas_caminhao_5620.

    CALL METHOD ctl_alv1_5620->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5620_alv1.

    gs_layout_5620_alv1-cwidth_opt = abap_true.

    CALL METHOD ctl_alv1_5620->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5620_alv1.

    CALL METHOD ctl_alv2_5620->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5620_alv2.

    gs_layout_5620_alv2-cwidth_opt = abap_true.

    CALL METHOD ctl_alv2_5620->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5620_alv2.

    "CALL METHOD CTL_ALV1_5620->REFRESH_TABLE_DISPLAY.
    CALL METHOD ctl_alv2_5620->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

*-----CS2019001891 - 08.03.2021 - JT - inicio
  METHOD handle_hotspot_click_5620.

    DATA: wa_sol_5620_det      TYPE ty_sol_5620.

    IF e_column_id-fieldname EQ 'ICONE'.

      READ TABLE it_sol_5620   INTO wa_sol_5620_det INDEX e_row_id.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'ZSD_PEDIDOS_IMPORTACAO'
          EXPORTING
            i_nro_sol     = wa_sol_5620_det-nro_sol
            i_seq         = wa_sol_5620_det-seq
            i_filial_resp = 'TROO'.
      ENDIF.
    ENDIF.

  ENDMETHOD.
*-----CS2019001891 - 08.03.2021 - JT - fim

  METHOD toolbar_5620.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'TROCA'.
    wa_tool-icon      = '@4O@'.
    wa_tool-quickinfo = 'Confirmar Troca de Nota'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD toolbar_caminhao_5620.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ADD_ROW'.
    wa_tool-icon     = '@17@'.
    wa_tool-quickinfo = 'Adicionar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'DEL_ROW'.
    wa_tool-icon     = '@18@'.
    wa_tool-quickinfo = 'Eliminar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'EDIT_ROW'.
    wa_tool-icon     = '@0Z@'.
    wa_tool-quickinfo = 'Editar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'SAVE_FRETE'.
    wa_tool-icon     = '@2L@'.
    wa_tool-quickinfo = 'Salvar'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'AUTORIZACAO'.
    wa_tool-icon      = '@96@'.
    wa_tool-quickinfo = 'Aut. de Embarque'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'AVISO'.
    wa_tool-icon     = '@PG@'.
    wa_tool-quickinfo = 'Gerar Aviso de Recebimento'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ENTRADA'.
    wa_tool-icon     = '@KA@'.
    wa_tool-quickinfo = 'Entrada de Estoque'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ROMANEIO'.
    wa_tool-icon     = '@0Q@'.
    wa_tool-quickinfo = 'Gerar Romaneio'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'EXROMANEIO'.
    wa_tool-icon     = '@VI@'.
    wa_tool-quickinfo = 'Estornar Romaneio'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

  METHOD user_command_5620.

    DATA: it_selected_rows TYPE lvc_t_row,
          wa_selected_rows TYPE lvc_s_row,
          wa_sol_5620      TYPE ty_sol_5620,
          wa_caminhao_5620 TYPE ty_caminhao_5620,
          vl_check_rom     TYPE char1,
          wa_row           TYPE lvc_s_row,
          vl_lines         TYPE i.

    IF e_ucomm = 'ADD_ROW'.

      IF vg_sol_5620 IS NOT INITIAL.

        CLEAR: wa_caminhao_5620.
        wa_caminhao_5620-edit    = abap_true.
        wa_caminhao_5620-nro_sol = vg_sol_5620-nro_sol.
        wa_caminhao_5620-seq     = vg_sol_5620-seq.
        wa_caminhao_5620-um      = vg_sol_5620-meins.

        APPEND wa_caminhao_5620 TO it_caminhao_5620.
        PERFORM bloqueia_linhas_caminhao_5620.
        CALL METHOD ctl_alv2_5620->refresh_table_display
          EXPORTING
            is_stable = _stable.

      ENDIF.

    ELSEIF e_ucomm = 'DEL_ROW'.

      IF vg_sol_5620 IS NOT INITIAL.

        CLEAR: it_selected_rows, wa_selected_rows.

        CALL METHOD ctl_alv2_5620->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        LOOP AT it_selected_rows INTO wa_selected_rows.
          READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX wa_selected_rows-index.
          IF sy-subrc IS INITIAL.
            IF wa_caminhao_5620-status NE 2.
              wa_caminhao_5620-name1 = 'DELETE'.
              MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX wa_selected_rows-index.
              CLEAR: wa_caminhao_5620.
            ENDIF.
          ENDIF.
        ENDLOOP.

        DELETE it_caminhao_5620 WHERE name1 EQ 'DELETE'.
        CALL METHOD ctl_alv2_5620->refresh_table_display
          EXPORTING
            is_stable = _stable.

      ENDIF.

    ELSEIF e_ucomm = 'EDIT_ROW'.

      IF vg_sol_5620 IS NOT INITIAL.

        CLEAR: it_selected_rows, wa_selected_rows.

        CALL METHOD ctl_alv2_5620->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        LOOP AT it_selected_rows INTO wa_selected_rows.
          READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX wa_selected_rows-index.
          IF sy-subrc IS INITIAL.
            IF wa_caminhao_5620-status NE 2.
              MOVE abap_true TO wa_caminhao_5620-edit.
              MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX wa_selected_rows-index..
              CLEAR: wa_caminhao_5620.
            ENDIF.
          ENDIF.
        ENDLOOP.

        PERFORM bloqueia_linhas_caminhao_5620.
        CALL METHOD ctl_alv2_5620->refresh_table_display
          EXPORTING
            is_stable = _stable.

      ENDIF.

    ELSEIF e_ucomm = 'SAVE_FRETE'.

      PERFORM salva_frete_5620.

    ELSEIF e_ucomm = 'ROMANEIO'.

      CLEAR: it_selected_rows, wa_selected_rows, it_caminhao_aux_5620, vl_check.

      CALL METHOD ctl_alv2_5620->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      DESCRIBE TABLE it_selected_rows LINES vl_lines.

      IF vl_lines NE 1.
        MESSAGE text-103 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.

        LOOP AT it_caminhao_5620 INTO wa_caminhao_5620.
          READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
          IF sy-subrc IS INITIAL.
            IF wa_caminhao_5620-status EQ 1.
              IF wa_caminhao_5620-nfenum IS INITIAL OR wa_caminhao_5620-series IS INITIAL OR
                 wa_caminhao_5620-netwr IS INITIAL OR wa_caminhao_5620-docdat_nf IS INITIAL.
                MOVE abap_true TO vl_check.
                MESSAGE text-104 TYPE 'S' DISPLAY LIKE 'E'.
              ELSEIF wa_caminhao_5620-edit EQ abap_true.
                MOVE abap_true TO vl_check.
                MESSAGE text-122 TYPE 'S' DISPLAY LIKE 'E'.
              ELSE.
                APPEND wa_caminhao_5620 TO it_caminhao_aux_5620.
              ENDIF.
            ELSE.
              MOVE abap_true TO vl_check.
              MESSAGE text-105 TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF vl_check IS INITIAL.
          IF it_caminhao_aux_5620 IS NOT INITIAL.
            PERFORM salva_romaneios_5620 CHANGING vl_check_rom.

            LOOP AT it_sol_5620 INTO wa_sol_5620 WHERE nro_sol EQ vg_sol_5620-nro_sol
                                                   AND seq     EQ vg_sol_5620-seq.
              wa_row-index = sy-tabix.
            ENDLOOP.

            CALL METHOD on_double_click_5620
              EXPORTING
                e_row = wa_row.

          ELSE.
            MESSAGE text-054 TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.

      ENDIF.

    ELSEIF e_ucomm = 'EXROMANEIO'.

      CLEAR: it_selected_rows, wa_selected_rows, it_caminhao_aux_5620.

      CALL METHOD ctl_alv2_5620->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      LOOP AT it_caminhao_5620 INTO wa_caminhao_5620.
        READ TABLE it_selected_rows INTO wa_selected_rows WITH KEY index = sy-tabix.
        IF sy-subrc IS INITIAL.
          IF wa_caminhao_5620-status EQ 2.
            APPEND wa_caminhao_5620 TO it_caminhao_aux_5620.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF it_caminhao_aux_5620 IS NOT INITIAL.
        PERFORM delete_romaneios_5620.
      ELSE.
        MESSAGE text-054 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

      LOOP AT it_sol_5620 INTO wa_sol_5620 WHERE nro_sol EQ vg_sol_5620-nro_sol
                                             AND seq     EQ vg_sol_5620-seq.
        wa_row-index = sy-tabix.
      ENDLOOP.

      CALL METHOD on_double_click_5620
        EXPORTING
          e_row = wa_row.

    ELSEIF e_ucomm = 'AUTORIZACAO'.

      FREE it_selected_rows.
      CLEAR: wa_selected_rows.

      CALL METHOD ctl_alv2_5620->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      IF lines( it_selected_rows ) EQ 1.
        TRY.
            wa_caminhao_5620 = it_caminhao_5620[ it_selected_rows[ 1 ]-index ].

            wa_5522-nro_sol      = wa_caminhao_5620-nro_sol.
            wa_5522-seq_cam      = wa_caminhao_5620-seq_cam.
            wa_5522-seq          = wa_caminhao_5620-seq.
            wa_5522-filial_resp  = wa_caminhao_5620-filial_resp.
            wa_5522-adiantamento = wa_caminhao_5620-adiantamento.
            wa_5522-observacao   = wa_caminhao_5620-observacao.

*           CALL SCREEN 5522 STARTING AT 5 5 ENDING AT 75 10.
*--nova chamada para CS2019001891 - inicio
            CALL SCREEN 5522 STARTING AT 5 5 ENDING AT 75 20.
*--nova chamada para CS2019001891 - fim

          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ELSE.
        IF it_selected_rows IS INITIAL.
          MESSAGE 'Nenhuma Linha Selecionada!' TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE 'Selecione somente uma Linha!' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.

    ELSEIF e_ucomm = 'TROCA'.

      CALL SCREEN '7000'.

    ENDIF.

  ENDMETHOD.                    "USER_COMMAND

  METHOD on_f4_5620.

    DATA: it_ret_5620      TYPE STANDARD TABLE OF ddshretval,
          it_f4_5620       TYPE STANDARD TABLE OF ty_f4_pedido,
          wa_caminhao_5620 TYPE ty_caminhao_5620.

    DATA: wa_ret  TYPE ddshretval,
          wa_modi TYPE lvc_s_modi.


    DATA: lv_ebeln TYPE  ebeln,
          lv_ebelp TYPE  ebelp.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA : it_fmap TYPE STANDARD TABLE OF dselc,
           wa_fmap TYPE dselc.

    READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 INDEX es_row_no-row_id.
    IF sy-subrc IS INITIAL AND wa_caminhao_5620-edit EQ abap_true.

      SELECT COUNT( * )
        FROM zsdt0137
        INTO @DATA(lv_cont)
        WHERE nro_sol      EQ @vg_sol_5620-nro_sol
          AND matnr        EQ @vg_sol_5620-matnr
          AND seq          EQ @vg_sol_5620-seq
*-----CS2019001891 - 08.03.2021 - JT - inicio
          AND filial_resp  EQ 'TROO'
          AND ped_imp      EQ 'X'.
*-----CS2019001891 - 08.03.2021 - JT - fim

      IF lv_cont = 0.
        wa_fmap-fldname = 'F0001'.
        wa_fmap-dyfldname = 'EBELN'.
        APPEND wa_fmap TO it_fmap.
        wa_fmap-fldname = 'F0002'.
        wa_fmap-dyfldname = 'EBELP'.
        APPEND wa_fmap TO it_fmap.

        SELECT *
          FROM zsdt0062
          INTO CORRESPONDING FIELDS OF TABLE it_f4_5620
          WHERE vbeln EQ vg_sol_5620-vbeln
            AND matnr EQ vg_sol_5620-matnr
           AND nro_sol EQ vg_sol_5620-nro_sol
           AND seq EQ vg_sol_5620-seq
            AND status NE 'E'.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'EBELN'
            window_title    = 'Lista de Pedidos'(002)
            value_org       = 'S'
            dynprofield     = 'EBELN'
          TABLES
            value_tab       = it_f4_5620
            return_tab      = it_ret_5620
            dynpfld_mapping = it_fmap
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc = 0.
          ASSIGN er_event_data->m_data->* TO <itab>.
          READ TABLE it_ret_5620 INTO wa_ret INDEX 1.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'EBELN'.
          wa_modi-value     = wa_ret-fieldval.
          APPEND wa_modi TO <itab>.
          READ TABLE it_ret_5620 INTO wa_ret INDEX 2.
          wa_modi-fieldname = 'EBELP'.
          wa_modi-value     = wa_ret-fieldval.
          APPEND wa_modi TO <itab>.
        ENDIF.

        er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)

      ELSE.

        CALL FUNCTION 'ZSD_PEDIDOS_IMPORTACAO'
          EXPORTING
            i_nro_sol     = vg_sol_5620-nro_sol
            i_seq         = vg_sol_5620-seq
            i_filial_resp = 'TROO'
            i_selecao     = 'X'
          IMPORTING
            e_ebeln       = lv_ebeln
            e_ebelp       = lv_ebelp.

        IF lv_ebeln IS NOT INITIAL.
          ASSIGN er_event_data->m_data->* TO <itab>.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'EBELN'.
          wa_modi-value     = lv_ebeln.
          APPEND wa_modi TO <itab>.
          wa_modi-fieldname = 'EBELP'.
          wa_modi-value     = lv_ebelp.
          APPEND wa_modi TO <itab>.
        ENDIF.

        er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_5620  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5620 OUTPUT.

  PERFORM seleciona_sol_5620.
  PERFORM alv_sol_5620.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5620  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5620 INPUT.

  CASE sy-ucomm.
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 80 20.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5620_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5620_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SELECT_LOTE
*&---------------------------------------------------------------------*
FORM seleciona_sol_5620.

  DATA: it_makt            TYPE STANDARD TABLE OF makt,
        it_kna1            TYPE STANDARD TABLE OF kna1,
        it_lfa1            TYPE STANDARD TABLE OF lfa1,
        it_tvkbt           TYPE STANDARD TABLE OF tvkbt,
        it_zsdt0062        TYPE STANDARD TABLE OF zsdt0062,
        it_zsdt0137        TYPE STANDARD TABLE OF zsdt0137,
        it_zsdt0138        TYPE STANDARD TABLE OF zsdt0138,
        it_zsdt0138_pt_col TYPE STANDARD TABLE OF zsdt0138,
        wa_zsdt0137        TYPE zsdt0137,
        wa_zsdt0138        TYPE zsdt0138,
        wa_makt            TYPE makt,
        wa_kna1            TYPE kna1,
        wa_lfa1            TYPE lfa1,
        wa_tvkbt           TYPE tvkbt,
        wa_zsdt0062        TYPE zsdt0062,
        wa_sol_5620        TYPE ty_sol_5620,
        vl_cont            TYPE i.

  SELECT zsdt0082~nro_sol
         zsdt0082~seq
         zsdt0082~vkbur
         zsdt0082~vbeln
         zsdt0082~posnr
         zsdt0082~vkorg
         zsdt0082~spart
         vbap~matnr
         zsdt0082~werks
         vbap~meins
         zsdt0082~nr_rot
         vbpa~kunnr
         vbkd~inco1
    FROM zsdt0082
    INNER JOIN vbap ON zsdt0082~vbeln = vbap~vbeln AND zsdt0082~posnr = vbap~posnr
    INNER JOIN vbkd ON vbap~vbeln = vbkd~vbeln
    INNER JOIN vbpa ON vbkd~vbeln = vbpa~vbeln
    INTO CORRESPONDING FIELDS OF TABLE it_sol_5620
    WHERE zsdt0082~vkorg    IN o_vkorg
      AND zsdt0082~spart    EQ o_spart
      AND zsdt0082~nro_sol  IN o_nrsol
      AND zsdt0082~dt_liber IN o_datas
      AND zsdt0082~vbeln    IN o_ovcor
      AND zsdt0082~seq NE 1
      AND vbkd~inco1        IN o_inco1
      AND vbpa~kunnr        IN o_kunnr
      AND vbpa~parvw        EQ 'AG'
      AND ( zsdt0082~status EQ 2 OR
            zsdt0082~status EQ 5 )
      AND EXISTS ( SELECT *
                     FROM zsdt0137
                    WHERE nro_sol     EQ zsdt0082~nro_sol
                      AND seq         EQ zsdt0082~seq
                      AND filial_resp EQ 'TROO'
                      AND qtd_vinc    GT 0
                      AND status      NE 'X' ).

  IF it_sol_5620 IS NOT INITIAL.

    SELECT *
      FROM tvkbt
      INTO TABLE it_tvkbt
      FOR ALL ENTRIES IN it_sol_5620
      WHERE vkbur EQ it_sol_5620-vkbur
        AND spras EQ sy-langu.

    SELECT *
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_sol_5620
      WHERE kunnr EQ it_sol_5620-kunnr.

    SELECT *
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_sol_5620
      WHERE matnr EQ it_sol_5620-matnr.

    SELECT *
      FROM zsdt0137
      INTO TABLE it_zsdt0137
      FOR ALL ENTRIES IN it_sol_5620
      WHERE nro_sol     EQ it_sol_5620-nro_sol
        AND seq         EQ it_sol_5620-seq
        AND filial_resp EQ 'TROO'.

    SELECT *
      FROM zsdt0138
      INTO TABLE it_zsdt0138
      FOR ALL ENTRIES IN it_sol_5620
      WHERE nro_sol     EQ it_sol_5620-nro_sol
        AND seq         EQ it_sol_5620-seq
        AND filial_resp EQ 'TROO'
        AND status      NE 'X'.

  ENDIF.

  LOOP AT it_sol_5620 INTO wa_sol_5620.

    vl_cont = vl_cont + 1.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_sol_5620-matnr.
    IF sy-subrc IS INITIAL.
      wa_sol_5620-maktx = wa_makt-maktx.
    ENDIF.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_sol_5620-kunnr.
    IF sy-subrc IS INITIAL.
      wa_sol_5620-name1 = wa_kna1-name1.
    ENDIF.

    READ TABLE it_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_sol_5620-vkbur.
    IF sy-subrc IS INITIAL.
      wa_sol_5620-bezei = wa_tvkbt-bezei.
    ENDIF.

    READ TABLE it_zsdt0137 INTO wa_zsdt0137 WITH KEY nro_sol = wa_sol_5620-nro_sol
                                                         seq = wa_sol_5620-seq.
    IF sy-subrc IS INITIAL.
      wa_sol_5620-qtd_a_eb = wa_zsdt0137-qtd_vinc.
*      WA_SOL_5620-OSERV = WA_ZSDT0137-VBELN.
    ENDIF.

    LOOP AT it_zsdt0138 INTO wa_zsdt0138 WHERE nro_sol EQ wa_sol_5620-nro_sol
                                           AND seq     EQ wa_sol_5620-seq.
      wa_sol_5620-qtdeb = wa_sol_5620-qtdeb + wa_zsdt0138-qtd_embarq.
    ENDLOOP.

    wa_sol_5620-icone = '@CC@'.

    MODIFY it_sol_5620 FROM wa_sol_5620 INDEX vl_cont.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_SOL_5620
*&---------------------------------------------------------------------*
FORM alv_sol_5620.

  IF g_custom_container_5620 IS INITIAL.

    CREATE OBJECT g_custom_container_5620
      EXPORTING
        container_name              = 'CONTAINER5620'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT dg_splitter_1_5620
      EXPORTING
        parent  = g_custom_container_5620
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1_5620->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_5620.

    CALL METHOD dg_splitter_1_5620->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_2_5620.

    CALL METHOD dg_splitter_1_5620->set_row_mode
      EXPORTING
        mode = dg_splitter_1_5620->mode_relative.

    CALL METHOD dg_splitter_1_5620->set_row_height
      EXPORTING
        id     = 1
        height = 50.

    CALL METHOD dg_splitter_1_5620->set_row_height
      EXPORTING
        id     = 2
        height = 50.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5620 USING:
          01 'NRO_SOL'        'ZSDT0082'      ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Sol.',
          02 'VKBUR'          'ZSDT0082'      ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Esc. Venda',
          03 'BEZEI'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Esc. Venda',
          04 'KUNNR'          'VBPA'          ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Cliente',
          05 'NAME1'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cliente',
          06 'VBELN'          'ZSDT0082'      ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
          07 'POSNR'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item'.
    IF p_fcorp  IS NOT INITIAL.
      PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5620 USING:
*-----CS2019001891 - 08.03.2021 - JT - inicio
      08 'ICONE'          ' '             ' '  ' '  ' '  'X'   'X'   ' '   ' '   ' '   'P.Imp.'.
*-----CS2019001891 - 08.03.2021 - JT - fim
    ENDIF.
    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_sol_5620 USING:
              09 'MATNR'          'MAKT'          ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Produto',
              10 'MAKTX'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Produto',
              11 'WERKS'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Centro Forn.',
              12 'QTD_A_EB'       ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. Solic.',
              13 'QTDEB'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd. Embarc.',
              14 'MEINS'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
              15 'INCO1'          ' '             ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Tp. Frete',
              16 'NR_ROT'         'ZSDT0082'      ' '  ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Roteiro'.

    PERFORM fill_it_fieldcatalog TABLES it_fieldcatalog_caminhao_5620 USING:
          01 'ICONE'                ' '         ' '     ' '  'X'  ' '   'X'   ' '   ' '   ' '   'Status',
          02 'NRO_SOL'              ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nro Sol',
          03 'EBELN'                ' '         'C310'  ' '  ' '  ' '   'X'   ' '   ' '   'X'   'Pedido Vinc.',
          04 'EBELP'                ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Item Ped.',
          05 'PTO_COL'              'ZSDT0138'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Ponto Coleta',
          06 'NAME_PTO_COL'         ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Descrição Ponto Coleta',
          07 'COD_TRANSPORTADORA'   'ZSDT0138'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Prop. Veículo',
          08 'NAME1'                ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Propietário',
          09 'PRECO_FRETE'          'ZSDT0138'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Vlr. Frete',
          10 'PLACA_CAV'            ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Cavalo',
          11 'PLACA_CAR1'           ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Carreta I',
          12 'PLACA_CAR2'           ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Carreta II',
          13 'PLACA_CAR3'           ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Plc. Carreta III',
          14 'MOTORISTA'            'ZSDT0138'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Cód. Motorista',
          15 'NAME2'                ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Motorista',
          16 'QTD_EMBARQ'           ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Qtd.',
          17 'UM'                   ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'U.M.',
          18 'NFENUM'               ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'NF Fornc.',
          19 'SERIES'               ' '         ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Série',
          20 'NETWR'                'ZSDT0001'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Valor NF',
          21 'DOCDAT_NF'            'ZSDT0138'  ' '     'X'  ' '  ' '   'X'   ' '   ' '   ' '   'Data Emissão NF',
          22 'NR_ROMANEIO'          ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Romaneio',
          23 'AUT_EMBARQUE'         ' '         ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Autorização Embarque'.


    gs_layout_5620_alv1-sel_mode   = 'A'.
    gs_layout_5620_alv1-stylefname = 'CELLSTYLES'.
    gs_layout_5620_alv1-cwidth_opt = 'X'.
    gs_layout_5620_alv1-info_fname = 'COR'.
    gs_layout_5620_alv1-smalltitle = 'X'.
    gs_layout_5620_alv1-grid_title = 'Solicitações de Ordem'.
    gs_layout_5620_alv2-sel_mode   = 'A'.
    gs_layout_5620_alv2-stylefname = 'CELLSTYLES'.
    gs_layout_5620_alv2-cwidth_opt = 'X'.
    gs_layout_5620_alv2-info_fname = 'COR'.
    gs_layout_5620_alv2-smalltitle = 'X'.
    gs_layout_5620_alv2-grid_title = 'Embarques'.

    PERFORM sort USING 'NRO_SOL' CHANGING it_sort_sol_5620.

    CREATE OBJECT ctl_alv1_5620
      EXPORTING
        i_parent = dg_parent_1_5620.           "ALV Solicitação

    CREATE OBJECT ctl_alv2_5620
      EXPORTING
        i_parent = dg_parent_2_5620.           "ALV Caminhão

    PERFORM excluir_botoes CHANGING it_exclude_5620.
    PERFORM registrar_f4_5620.

    SET HANDLER:
      lcl_event_handler_5620=>on_double_click_5620 FOR ctl_alv1_5620,
      lcl_event_handler_5620=>user_command_5620 FOR ctl_alv1_5620,
      lcl_event_handler_5620=>toolbar_5620 FOR ctl_alv1_5620,
*-----CS2019001891 - 08.03.2021 - JT - inicio
      lcl_event_handler_5620=>handle_hotspot_click_5620 FOR ctl_alv1_5620.
*-----CS2019001891 - 08.03.2021 - JT - fim

    CALL METHOD ctl_alv1_5620->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_5620_alv1
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_sol_5620
        it_outtab       = it_sol_5620
        it_sort         = it_sort_sol_5620.

    SET HANDLER:
      lcl_event_handler_5620=>toolbar_caminhao_5620 FOR ctl_alv2_5620,
      lcl_event_handler_5620=>user_command_5620 FOR ctl_alv2_5620,
       lcl_event_handler_5620=>data_changed_finished_5620 FOR ctl_alv2_5620,
      lcl_event_handler_5620=>on_f4_5620 FOR ctl_alv2_5620.

    CALL METHOD ctl_alv2_5620->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_5620_alv2
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_5620
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_caminhao_5620
        it_outtab            = it_caminhao_5620
        it_sort              = it_sort_caminhao_5620.

    CALL METHOD ctl_alv2_5620->register_edit_event  "PARA REGISTRAR EVENTO NA ALV
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ELSE.

    CALL METHOD ctl_alv1_5620->get_frontend_layout
      IMPORTING
        es_layout = gs_layout_5620_alv1.

    gs_layout_5620_alv1-cwidth_opt = abap_true.

    CALL METHOD ctl_alv1_5620->set_frontend_layout
      EXPORTING
        is_layout = gs_layout_5620_alv1.

    CALL METHOD ctl_alv1_5620->refresh_table_display
      EXPORTING
        is_stable = _stable.
    CALL METHOD ctl_alv2_5620->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_5620
*&---------------------------------------------------------------------*
FORM clear_5620 .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLICK_EM_SOLIC
*&---------------------------------------------------------------------*
FORM click_em_solic_5620  USING    p_e_row_index.

  DATA: wa_caminhao_5620 TYPE ty_caminhao_5620,
        wa_sol_5620      TYPE ty_sol_5620,
        it_lfa1          TYPE STANDARD TABLE OF lfa1,
        wa_lfa1          TYPE lfa1,
        it_zsdt0001      TYPE STANDARD TABLE OF zsdt0001,
        wa_zsdt0001      TYPE zsdt0001,
        vl_cont          TYPE i,
        chave            TYPE char29.

  CLEAR: it_caminhao_5620. "VG_SOL_5620

  LOOP AT it_sol_5620 INTO wa_sol_5620 WHERE nro_sol EQ vg_sol_5620-nro_sol
                                         AND seq     EQ vg_sol_5620-seq
                                         AND vbeln   EQ vg_sol_5620-vbeln
                                         AND posnr   EQ vg_sol_5620-posnr.

    vl_cont = sy-tabix.

    CONCATENATE wa_sol_5620-nro_sol wa_sol_5620-seq wa_sol_5620-vbeln wa_sol_5620-posnr INTO chave.

    CALL FUNCTION 'ZDENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave = chave.

    CLEAR: wa_sol_5620-cor.
    MODIFY it_sol_5620 FROM wa_sol_5620 INDEX vl_cont.

  ENDLOOP.

  CLEAR: vg_sol_5620, vl_cont.

  READ TABLE it_sol_5620 INTO wa_sol_5620 INDEX p_e_row_index.
  IF sy-subrc IS INITIAL.

    CONCATENATE wa_sol_5620-nro_sol wa_sol_5620-seq wa_sol_5620-vbeln wa_sol_5620-posnr INTO chave.

    CALL FUNCTION 'ZENQUEUE_SD_SOL_INSUMOS'
      EXPORTING
        chave          = chave
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ELSE.

      wa_sol_5620-cor = 'C300'.
      vg_sol_5620 = wa_sol_5620.
      MODIFY it_sol_5620 FROM wa_sol_5620 INDEX p_e_row_index.

      SELECT *
        FROM zsdt0138
        INTO CORRESPONDING FIELDS OF TABLE it_caminhao_5620
        WHERE nro_sol   EQ vg_sol_5620-nro_sol
        AND seq         EQ vg_sol_5620-seq
        AND filial_resp EQ 'TROO'
        AND status      NE 'X'.

      IF it_caminhao_5620 IS NOT INITIAL.

        SELECT *
          FROM lfa1
          INTO TABLE it_lfa1
          FOR ALL ENTRIES IN it_caminhao_5620
          WHERE lifnr EQ it_caminhao_5620-cod_transportadora.

        SELECT *
          FROM lfa1
          APPENDING TABLE it_lfa1
          FOR ALL ENTRIES IN it_caminhao_5620
          WHERE lifnr EQ it_caminhao_5620-motorista.

        SELECT *
          FROM zsdt0001
          INTO TABLE it_zsdt0001
          FOR ALL ENTRIES IN it_caminhao_5620
          WHERE ch_referencia EQ it_caminhao_5620-ch_referencia.


        it_caminhao_aux_5620[] = it_caminhao_5620[].
        SORT it_caminhao_aux_5620 BY pto_col.
        DELETE it_caminhao_aux_5620 WHERE pto_col IS INITIAL.
        IF it_caminhao_aux_5620[] IS NOT INITIAL.
          SELECT * APPENDING TABLE it_lfa1
            FROM lfa1
            FOR ALL ENTRIES IN it_caminhao_aux_5620
            WHERE lifnr = it_caminhao_aux_5620-pto_col.
        ENDIF.

      ENDIF.

      LOOP AT it_caminhao_5620 INTO wa_caminhao_5620.

        vl_cont = vl_cont + 1.

        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_caminhao_5620-cod_transportadora.
        IF sy-subrc IS INITIAL.
          wa_caminhao_5620-name1 = wa_lfa1-name1.
        ENDIF.

        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_caminhao_5620-motorista.
        IF sy-subrc IS INITIAL.
          wa_caminhao_5620-name2 = wa_lfa1-name1.
        ENDIF.

        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_caminhao_5620-pto_col.
        IF sy-subrc IS INITIAL.
          wa_caminhao_5620-pto_col = wa_lfa1-lifnr.
          wa_caminhao_5620-name_pto_col = wa_lfa1-name1.
        ENDIF.

        IF wa_caminhao_5620-status EQ 1.
          wa_caminhao_5620-icone = '@5B@'.
        ELSEIF wa_caminhao_5620-status EQ 2.
          wa_caminhao_5620-icone = '@0Q@'.
        ENDIF.

        READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY ch_referencia = wa_caminhao_5620-ch_referencia.
        IF sy-subrc IS INITIAL.
          wa_caminhao_5620-nr_romaneio = wa_zsdt0001-nr_romaneio.
          IF wa_zsdt0001-st_proc EQ '99'.
            wa_caminhao_5620-icone = '@01@'.
          ENDIF.
        ENDIF.

        CONCATENATE wa_caminhao_5620-nro_sol '/' wa_caminhao_5620-seq '-' wa_caminhao_5620-seq_cam
               INTO DATA(v_autoriz).
        CONCATENATE v_autoriz wa_caminhao_5620-filial_resp
               INTO wa_caminhao_5620-aut_embarque
               SEPARATED BY space.

        MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX vl_cont.

      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BLOQUEIA_LINHAS_CAMINHAO_5620
*&---------------------------------------------------------------------*
FORM bloqueia_linhas_caminhao_5620.
  DATA: vl_cont          TYPE i,
        it_celltab       TYPE lvc_t_styl,
        wa_caminhao_5620 TYPE ty_caminhao_5620.

  LOOP AT it_caminhao_5620 INTO wa_caminhao_5620.
    vl_cont = vl_cont + 1.
    CLEAR: it_celltab, wa_caminhao_5620-cellstyles.
    PERFORM fill_celltab_5620 USING wa_caminhao_5620-edit
                         CHANGING it_celltab.
    CLEAR wa_caminhao_5620-cellstyles.
    INSERT LINES OF it_celltab INTO TABLE wa_caminhao_5620-cellstyles.
    MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX vl_cont.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB_5620
*&---------------------------------------------------------------------*
FORM fill_celltab_5620  USING    p_edit
                        CHANGING p_it_celltab TYPE lvc_t_styl.

  DATA: wa_celltab TYPE lvc_s_styl,
        status     TYPE raw4.

  IF p_edit EQ abap_true.
    status = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    status = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  wa_celltab-fieldname = 'FILIAL_RESP'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'QTD_VINC'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'COD_TRANSPORTADORA'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PRECO_FRETE'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PLACA_CAV'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PLACA_CAR1'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PLACA_CAR2'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PLACA_CAR3'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'MOTORISTA'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'QTD_EMBARQ'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'NFENUM'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'SERIES'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'NETWR'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'DOCDAT_NF'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.
  wa_celltab-fieldname = 'PTO_COL'.
  wa_celltab-style = status.
  INSERT wa_celltab INTO TABLE p_it_celltab.

ENDFORM.                               " FILL_CELLTAB

*&---------------------------------------------------------------------*
*&      Form  REGISTRAR_F4_5420
*&---------------------------------------------------------------------*
FORM registrar_f4_5620 .

  DATA: gs_f4      TYPE lvc_s_f4,
        gt_f4_5620 TYPE lvc_t_f4.

  gs_f4-fieldname  = 'EBELN'.
  gs_f4-register   = 'X'.
  gs_f4-getbefore  = space.
  gs_f4-chngeafter = space.
  APPEND gs_f4 TO gt_f4_5620.

  CALL METHOD ctl_alv2_5620->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4_5620.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SALVA_ROMANEIOS
*&---------------------------------------------------------------------*
FORM salva_romaneios_5620 CHANGING vl_check_rom.

  DATA: it_zsdt0040      TYPE STANDARD TABLE OF zsdt0040,
        it_zsdt0041      TYPE STANDARD TABLE OF zsdt0041,
        it_zsdt0090      TYPE STANDARD TABLE OF zsdt0090,
        it_zsdt0136      TYPE STANDARD TABLE OF zsdt0136,
        it_zsdt0001      TYPE STANDARD TABLE OF zsdt0001,
        it_zsdt0082      TYPE STANDARD TABLE OF zsdt0082,
        it_vbpa          TYPE STANDARD TABLE OF vbpa,
        it_vbap          TYPE STANDARD TABLE OF vbap,
        it_vbkd          TYPE STANDARD TABLE OF vbkd,
        wa_caminhao_5620 TYPE ty_caminhao_5620,
        wa_zsdt0041      TYPE zsdt0041,
        wa_zsdt0040      TYPE zsdt0040,
        wa_zsdt0090      TYPE zsdt0090,
        wa_zsdt0001      TYPE zsdt0001,
        wa_zsdt0136      TYPE zsdt0136,
        wa_zsdt0082      TYPE zsdt0082,
        wa_vbkd          TYPE vbkd,
        wa_vbpa          TYPE vbpa,
        wa_vbap          TYPE vbap,
        chave            TYPE char8,
        vl_doc_simulacao TYPE zsdt0041-doc_simulacao,
        vl_safra         TYPE zsdt0040-safra,
        it_zsdt0138      TYPE STANDARD TABLE OF zsdt0138,
        wa_zsdt0138      TYPE zsdt0138,
        vl_cont          TYPE i,
        vl_gravou        TYPE char1.


  SELECT *
    FROM zsdt0082
    INTO CORRESPONDING FIELDS OF TABLE it_zsdt0082
    FOR ALL ENTRIES IN it_caminhao_aux_5620
    WHERE status NE 'X'
      AND nro_sol EQ it_caminhao_aux_5620-nro_sol
      AND seq     EQ it_caminhao_aux_5620-seq.

  IF it_zsdt0082 IS NOT INITIAL.

    SORT it_zsdt0082 BY nro_sol seq ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_zsdt0082 COMPARING nro_sol seq.

    SELECT *
      FROM zsdt0041
      INTO TABLE it_zsdt0041
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE vbeln EQ it_zsdt0082-vbeln.

    SELECT *
      FROM zsdt0090
      INTO TABLE it_zsdt0090
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE vbeln EQ it_zsdt0082-vbeln.

    IF it_zsdt0041 IS NOT INITIAL.

      SELECT *
        FROM zsdt0040
        INTO TABLE it_zsdt0040
        FOR ALL ENTRIES IN it_zsdt0041
        WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

    ENDIF.

    IF it_zsdt0090 IS NOT INITIAL.

      SELECT *
        FROM zsdt0040
        APPENDING TABLE it_zsdt0040
        FOR ALL ENTRIES IN it_zsdt0090
        WHERE doc_simulacao EQ it_zsdt0090-doc_simulacao.

    ENDIF.

    SELECT *
      FROM zsdt0136
      INTO TABLE it_zsdt0136
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE werks EQ it_zsdt0082-werks.

  ENDIF.

  LOOP AT it_zsdt0082 INTO wa_zsdt0082.
    READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_zsdt0082-vbeln.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY vbeln = wa_zsdt0082-vbeln.
      IF sy-subrc IS INITIAL.
        vl_doc_simulacao = wa_zsdt0090-doc_simulacao.
      ENDIF.
    ELSE.
      vl_doc_simulacao = wa_zsdt0041-doc_simulacao.
    ENDIF.

    IF vl_doc_simulacao IS NOT INITIAL.
      READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = vl_doc_simulacao.
      IF sy-subrc IS INITIAL.
        vl_safra = wa_zsdt0040-safra.
      ENDIF.
    ENDIF.

    READ TABLE it_zsdt0136 WITH KEY werks = wa_zsdt0082-werks
                                    safra = vl_safra TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE text-055 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check_rom = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF vl_check_rom IS INITIAL.

    CREATE OBJECT zcl_romaneio.

    SELECT *
       FROM vbpa
       INTO TABLE it_vbpa
       FOR ALL ENTRIES IN it_zsdt0082
       WHERE vbeln EQ it_zsdt0082-vbeln
         AND ( parvw EQ 'PC' OR
               parvw EQ 'AG' ).

    SELECT *
      FROM vbkd
      INTO TABLE it_vbkd
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE vbeln EQ it_zsdt0082-vbeln.

    SELECT *
      FROM vbap
      INTO TABLE it_vbap
      FOR ALL ENTRIES IN it_zsdt0082
      WHERE vbeln EQ it_zsdt0082-vbeln
        AND posnr EQ it_zsdt0082-posnr.

    LOOP AT it_caminhao_aux_5620 INTO wa_caminhao_5620.

      vl_cont = vl_cont + 1.

      wa_zsdt0001-tp_movimento = 'S'.
      READ TABLE it_zsdt0082 INTO wa_zsdt0082 WITH KEY nro_sol = wa_caminhao_5620-nro_sol
                                                       seq     = wa_caminhao_5620-seq.
      IF sy-subrc IS INITIAL.
        wa_zsdt0001-vbeln = wa_zsdt0082-vbeln.
        wa_zsdt0001-bukrs = wa_zsdt0082-vkorg.
        wa_zsdt0001-branch = wa_zsdt0082-werks.

        READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_zsdt0082-vbeln.
        IF sy-subrc IS INITIAL.
          IF wa_vbkd-inco1 EQ 'FOB'.
            wa_zsdt0001-tp_frete = 'F'.
          ELSE.
            wa_zsdt0001-tp_frete = 'C'.
          ENDIF.
        ENDIF.

        READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_zsdt0082-vbeln
                                                       parvw = 'PC'.
        IF sy-subrc IS INITIAL.
          wa_zsdt0001-parid = wa_vbpa-lifnr.
        ENDIF.

        READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = wa_zsdt0082-vbeln
                                                 parvw = 'AG'.
        IF sy-subrc IS INITIAL.
          wa_zsdt0001-id_cli_dest = wa_vbpa-kunnr.
        ENDIF.

        READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_zsdt0082-vbeln
                                                 posnr = wa_zsdt0082-posnr.
        IF sy-subrc IS INITIAL.
          wa_zsdt0001-matnr = wa_vbap-matnr.
        ENDIF.

        READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_zsdt0082-vbeln.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY vbelv = wa_zsdt0082-vbeln.
          IF sy-subrc IS INITIAL.
            vl_doc_simulacao = wa_zsdt0090-doc_simulacao.
          ENDIF.
        ELSE.
          vl_doc_simulacao = wa_zsdt0041-doc_simulacao.
        ENDIF.

        IF vl_doc_simulacao IS NOT INITIAL.
          READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = vl_doc_simulacao.
          IF sy-subrc IS INITIAL.
            vl_safra = wa_zsdt0040-safra.
          ENDIF.
        ENDIF.

      ENDIF.

      wa_zsdt0001-dt_movimento = sy-datum.
      wa_zsdt0001-peso_liq = wa_caminhao_5620-qtd_embarq.
      wa_zsdt0001-peso_fiscal = wa_caminhao_5620-qtd_embarq.
      wa_zsdt0001-placa_cav = wa_caminhao_5620-placa_cav.
      wa_zsdt0001-placa_car1 = wa_caminhao_5620-placa_car1.
      wa_zsdt0001-placa_car2 = wa_caminhao_5620-placa_car2.
      wa_zsdt0001-placa_car3 = wa_caminhao_5620-placa_car3.
      wa_zsdt0001-motorista = wa_caminhao_5620-motorista.
      wa_zsdt0001-id_interface = '51'.

      IF wa_vbkd-inco1 NE 'CPT' AND wa_vbkd-inco1 NE 'CFR'.
        wa_zsdt0001-agente_frete = '0000000116'.
      ENDIF.

*      wa_zsdt0001-agente_frete = '0000000116'.
      wa_zsdt0001-nr_safra = vl_safra.

      zcl_romaneio->zif_cadastro~novo_registro( ).
      zcl_romaneio->set_cabecalho_romaneio( i_romaneio = wa_zsdt0001 ).

      TRY.
          zcl_romaneio->zif_cadastro~gravar_registro( RECEIVING i_gravou = vl_gravou ).
        CATCH zcx_cadastro INTO zcx_cadastro.
          zcx_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

      IF vl_gravou IS INITIAL.
        MOVE abap_true TO vl_check_rom.
      ELSE.

        SELECT SINGLE *
                          FROM zsdt0138
                          INTO wa_zsdt0138
                          WHERE seq_cam EQ wa_caminhao_5620-seq_cam
                            AND nro_sol EQ wa_caminhao_5620-nro_sol
                            AND seq     EQ wa_caminhao_5620-seq
                            AND filial_resp EQ 'TROO'.

        zcl_romaneio->get_registro( IMPORTING e_registro = wa_zsdt0001 ).

        wa_zsdt0138-ch_referencia = wa_zsdt0001-ch_referencia.
        wa_zsdt0138-status = wa_zsdt0138-status + 1.
        MODIFY zsdt0138 FROM wa_zsdt0138.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETE_ROMANEIOS_5620
*&---------------------------------------------------------------------*
FORM delete_romaneios_5620 .

  DATA: vl_check         TYPE char1,
        it_zsdt0138      TYPE STANDARD TABLE OF zsdt0138,
        it_zsdt0001      TYPE STANDARD TABLE OF zsdt0001,
        wa_zsdt0138      TYPE zsdt0138,
        wa_zsdt0001      TYPE zsdt0001,
        wa_caminhao_5620 TYPE ty_caminhao_5620.

  SELECT *
    FROM zsdt0001
    INTO TABLE it_zsdt0001
    FOR ALL ENTRIES IN it_caminhao_aux_5620
    WHERE ch_referencia EQ it_caminhao_aux_5620-ch_referencia.

  "Check se há algum romaneio com
  LOOP AT it_zsdt0001 INTO wa_zsdt0001.
    IF wa_zsdt0001-doc_rem IS NOT INITIAL.
      vl_check = abap_true.
      MESSAGE s000(z_fi) WITH 'Romaneio' wa_zsdt0001-ch_referencia 'já possuí remessa' DISPLAY LIKE 'E'.
    ENDIF.
  ENDLOOP.

  IF vl_check IS INITIAL.

    LOOP AT it_zsdt0001 INTO wa_zsdt0001.

      SELECT SINGLE *
        FROM zsdt0138
        INTO wa_zsdt0138
        WHERE ch_referencia EQ wa_zsdt0001-ch_referencia.

      CLEAR: wa_zsdt0138-ch_referencia.
      wa_zsdt0138-status = wa_zsdt0138-status - 1.
      MODIFY zsdt0138 FROM wa_zsdt0138.
      DELETE zsdt0001 FROM wa_zsdt0001.
    ENDLOOP.

    MESSAGE text-059 TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVA_FRETE_5620
*&---------------------------------------------------------------------*
FORM salva_frete_5620 .

  DATA: vl_check_initial  TYPE char1,
        wa_caminhao_5620  TYPE ty_caminhao_5620,
        vl_qtd_vinc_camin TYPE zsdt0138-qtd_embarq,
        wa_sol_5620       TYPE ty_sol_5620,
        vl_seq_cam        TYPE zsdt0138-seq_cam,
        wa_zsdt0138       TYPE zsdt0138,
        it_zsdt0138       TYPE STANDARD TABLE OF zsdt0138,
        wa_row            TYPE lvc_s_row.

  CLEAR: vl_check_initial.

  IF vg_sol_5620 IS NOT INITIAL.
    "Check se não está salvando linhas em branco


    IF vg_sol_5620-inco1 NE 'CPT' AND vg_sol_5620-inco1 NE 'CFR'.

      LOOP AT it_caminhao_5620 INTO wa_caminhao_5620.

        IF wa_caminhao_5620-edit EQ 'X' AND ( wa_caminhao_5620-ebeln              IS INITIAL OR
         wa_caminhao_5620-cod_transportadora IS INITIAL OR
         wa_caminhao_5620-preco_frete        IS INITIAL OR
         wa_caminhao_5620-placa_cav          IS INITIAL OR
         wa_caminhao_5620-pto_col            IS INITIAL OR
         wa_caminhao_5620-motorista          IS INITIAL OR
         wa_caminhao_5620-qtd_embarq         IS INITIAL ).
          vl_check_initial = abap_true.
          MESSAGE text-053 TYPE 'S' DISPLAY LIKE 'E'.

        ENDIF.

        vl_qtd_vinc_camin = vl_qtd_vinc_camin + wa_caminhao_5620-qtd_embarq.

      ENDLOOP.

    ELSE.

      LOOP AT it_caminhao_5620 INTO wa_caminhao_5620.

        IF wa_caminhao_5620-edit EQ 'X' AND ( wa_caminhao_5620-ebeln              IS INITIAL OR
         wa_caminhao_5620-cod_transportadora IS INITIAL OR
       wa_caminhao_5620-preco_frete        IS INITIAL OR
       wa_caminhao_5620-pto_col            IS INITIAL OR
       wa_caminhao_5620-qtd_embarq         IS INITIAL ).
          vl_check_initial = abap_true.
          MESSAGE text-053 TYPE 'S' DISPLAY LIKE 'E'.

        ENDIF.

        vl_qtd_vinc_camin = vl_qtd_vinc_camin + wa_caminhao_5620-qtd_embarq.

      ENDLOOP.

    ENDIF.

    "Check se tem embarcado mais que o permitido
    IF vl_qtd_vinc_camin GT vg_sol_5620-qtd_a_eb.
      vl_check_initial = abap_true.
      MESSAGE text-048 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    IF vl_check_initial NE abap_true.

      "Atualizando a tabela de Solicitação
      LOOP AT it_sol_5620 INTO wa_sol_5620 WHERE nro_sol EQ vg_sol_5620-nro_sol AND
                                                     seq EQ vg_sol_5620-seq.
        wa_sol_5620-qtdeb = vl_qtd_vinc_camin.
        MODIFY it_sol_5620 FROM wa_sol_5620 INDEX sy-tabix.
      ENDLOOP.

      SELECT MAX( seq_cam )
        FROM zsdt0138
        INTO vl_seq_cam
        WHERE filial_resp EQ 'TROO'
          AND seq         EQ vg_sol_5620-seq
          AND nro_sol     EQ vg_sol_5620-nro_sol.

      CLEAR: wa_zsdt0138.

      LOOP AT it_caminhao_5620 INTO wa_caminhao_5620 WHERE edit EQ abap_true.

        IF wa_caminhao_5620-seq_cam IS INITIAL.
          wa_zsdt0138-seq_cam          = vl_seq_cam + 1.
          wa_caminhao_5620-seq_cam     = vl_seq_cam + 1.
          vl_seq_cam                   = vl_seq_cam + 1.
        ELSE.
          wa_zsdt0138-seq_cam          = wa_caminhao_5620-seq_cam.
        ENDIF.

        wa_zsdt0138-nro_sol            = vg_sol_5620-nro_sol.
        wa_zsdt0138-seq                = vg_sol_5620-seq.
        wa_zsdt0138-filial_resp        = 'TROO'.
        wa_zsdt0138-ebeln              = wa_caminhao_5620-ebeln.
        wa_zsdt0138-ebelp              = wa_caminhao_5620-ebelp.
        wa_zsdt0138-cod_transportadora = wa_caminhao_5620-cod_transportadora.
        wa_zsdt0138-preco_frete        = wa_caminhao_5620-preco_frete.
        wa_zsdt0138-placa_cav          = wa_caminhao_5620-placa_cav.
        wa_zsdt0138-pto_col            = wa_caminhao_5620-pto_col.
        wa_zsdt0138-placa_car1         = wa_caminhao_5620-placa_car1.
        wa_zsdt0138-placa_car2         = wa_caminhao_5620-placa_car2.
        wa_zsdt0138-placa_car3         = wa_caminhao_5620-placa_car3.
        wa_zsdt0138-motorista          = wa_caminhao_5620-motorista.
        wa_zsdt0138-qtd_embarq         = wa_caminhao_5620-qtd_embarq.
        wa_zsdt0138-um                 = wa_caminhao_5620-um.
        wa_zsdt0138-nfenum             = wa_caminhao_5620-nfenum.
        wa_zsdt0138-series             = wa_caminhao_5620-series.
        wa_zsdt0138-netwr              = wa_caminhao_5620-netwr.
        wa_zsdt0138-docdat_nf          = wa_caminhao_5620-docdat_nf.
        wa_zsdt0138-usnam              = sy-uname.
        wa_zsdt0138-status             = '1'.
        wa_zsdt0138-data_atual         = sy-datum.
        wa_zsdt0138-hora_atual         = sy-uzeit.
        MODIFY zsdt0138 FROM wa_zsdt0138.
        wa_caminhao_5620-edit = abap_false.
        MODIFY it_caminhao_5620 FROM wa_caminhao_5620 INDEX sy-tabix.

      ENDLOOP.

      SELECT *
        FROM zsdt0138
        INTO TABLE it_zsdt0138
        WHERE nro_sol     EQ vg_sol_5620-nro_sol
          AND seq         EQ vg_sol_5620-seq
          AND filial_resp EQ 'TROO'.

      LOOP AT it_zsdt0138 INTO wa_zsdt0138.
        READ TABLE it_caminhao_5620 INTO wa_caminhao_5620 WITH KEY nro_sol = wa_zsdt0138-nro_sol
                                                                       seq = wa_zsdt0138-seq
                                                                   seq_cam = wa_zsdt0138-seq_cam.
        IF sy-subrc IS NOT INITIAL.
          wa_zsdt0138-status = 'X'.
          wa_zsdt0138-user_canc = sy-uname.
          wa_zsdt0138-dt_canc = sy-datum.
          wa_zsdt0138-hr_can = sy-uzeit.
          MODIFY zsdt0138 FROM wa_zsdt0138.
        ENDIF.

      ENDLOOP.

      CLEAR: vl_qtd_vinc_camin.

      LOOP AT it_sol_5620 INTO wa_sol_5620 WHERE nro_sol EQ vg_sol_5620-nro_sol
                                                 AND seq     EQ vg_sol_5620-seq.
        wa_row-index = sy-tabix.
      ENDLOOP.

      CALL METHOD lcl_event_handler_5620=>on_double_click_5620
        EXPORTING
          e_row = wa_row.

      MESSAGE text-049 TYPE 'S'.

      CALL METHOD ctl_alv1_5620->refresh_table_display
        EXPORTING
          is_stable = _stable.

    ENDIF.

  ENDIF.

ENDFORM.
