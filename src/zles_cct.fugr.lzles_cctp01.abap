*----------------------------------------------------------------------*
***INCLUDE LZLES_CCTP01.
*----------------------------------------------------------------------*


CLASS lcl_alv_toolbar_0120 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    IF ( entrega_control-modo NE c_entrega_view ).

      ty_toolbar-icon      = icon_insert_row.
      ty_toolbar-function  = c_novo.
      ty_toolbar-text      = 'Inserir'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      = icon_delete_row.
      ty_toolbar-function  = c_del.
      ty_toolbar-text      = 'Deletar'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

    ENDIF.

    ty_toolbar-icon      = icon_import_transport_request.
    ty_toolbar-function  = c_doc_carga.
    ty_toolbar-text      = 'Documentos de Carga'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

    CASE e_ucomm.
      WHEN c_doc_carga.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0120->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

        READ TABLE it_saida_0120 INTO wa_saida_0120 INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.

        IF wa_saida_0120-numero_due IS INITIAL.
          MESSAGE 'Número da DU-e não foi informado!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF strlen( wa_saida_0120-numero_due ) NE 14.
          MESSAGE 'Número da DU-e é inválido!' TYPE 'S'.
          EXIT.
        ENDIF.

        "Monta Saída com os Registros Vinculados ao Item
        CLEAR: it_saida_0122_itm[].
        LOOP AT it_saida_0122 INTO wa_saida_0122 WHERE numero_due  EQ wa_saida_0120-numero_due.
          CLEAR: wa_saida_0122_itm, gt_estilo[].

          MOVE-CORRESPONDING wa_saida_0122 TO wa_saida_0122_itm.

          CLEAR: wa_saida_0122_itm-estilo[].

          IF wa_saida_0122_itm-peso_bruto_total IS NOT INITIAL.
            wl_estilo-fieldname = 'PESO_BRUTO_TOTAL'.
            wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
            APPEND wl_estilo TO gt_estilo.
          ENDIF.

          INSERT LINES OF gt_estilo INTO TABLE wa_saida_0122_itm-estilo.

          APPEND wa_saida_0122_itm TO it_saida_0122_itm.
        ENDLOOP.

        CALL SCREEN 0122 STARTING AT 12 05 ENDING AT 115 18.

        "Atualiza saída geral com os Registros Atualizados
        DELETE it_saida_0122 WHERE numero_due  EQ wa_saida_0120-numero_due.

        LOOP AT it_saida_0122_itm INTO wa_saida_0122_itm WHERE numero_due  EQ wa_saida_0120-numero_due.
          CLEAR: wa_saida_0122.
          MOVE-CORRESPONDING wa_saida_0122_itm TO wa_saida_0122.
          APPEND wa_saida_0122 TO it_saida_0122.
        ENDLOOP.

        LEAVE TO SCREEN 0100.

      WHEN c_novo.
        CLEAR: wa_saida_0120.
        APPEND wa_saida_0120 TO it_saida_0120.
        PERFORM f_refresh_alv USING '0120'.

      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0120->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Deseja realmente deletar o registro?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        CHECK var_answer EQ '1'.

        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

        READ TABLE it_saida_0120 INTO wa_saida_0120 INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.

        DELETE it_saida_0120 INDEX wa_sel_rows-index.

        IF sy-subrc = 0.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: f_refresh_alv USING '0120'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_alv_toolbar_0122 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    IF ( entrega_control-modo NE c_entrega_view ).

      ty_toolbar-icon      = icon_insert_row.
      ty_toolbar-function  = c_novo.
      ty_toolbar-text      = 'Inserir'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      = icon_delete_row.
      ty_toolbar-function  = c_del.
      ty_toolbar-text      = 'Deletar'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

    ENDIF.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

    CASE e_ucomm.
      WHEN c_novo.
        CLEAR: wa_saida_0122_itm, gt_estilo[].

        IF entrega_default-tipo_carga IS NOT INITIAL.
          wa_saida_0122_itm-tipo_carga = entrega_default-tipo_carga.
        ENDIF.

        IF entrega_default-tipo_granel IS NOT INITIAL.
          wa_saida_0122_itm-tipo_granel = entrega_default-tipo_granel.
        ENDIF.

        IF entrega_default-unid_medida IS NOT INITIAL.
          wa_saida_0122_itm-unid_medida = entrega_default-unid_medida.
        ENDIF.

        wa_saida_0122_itm-numero_due = wa_saida_0120-numero_due.

        IF wa_saida_0120-numero_due IS NOT INITIAL.
          SELECT SINGLE *
            FROM zsdt0170 INTO @DATA(_wl_0170)
           WHERE numero_due       EQ @wa_saida_0120-numero_due
             AND tp_due           EQ '1' "Sem NF-e
             AND status           EQ '1' "Registrado no Portal
             AND bloqueio_interno EQ @abap_false.

          IF sy-subrc EQ 0.
            SELECT *
              FROM zsdt0172 INTO TABLE @DATA(_tg_0172)
             WHERE id_due EQ @_wl_0170-id_due.

            IF lines( _tg_0172[] ) EQ 1.
              READ TABLE _tg_0172 INTO DATA(_wl_0172) INDEX 1.
              IF ( sy-subrc EQ 0 ) AND ( _wl_0172-peso_liq_total > 0 ).
                wa_saida_0122_itm-peso_bruto_total = _wl_0172-peso_liq_total.

                "WL_ESTILO-FIELDNAME = 'PESO_BRUTO_TOTAL'.
                "WL_ESTILO-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
                "APPEND WL_ESTILO TO GT_ESTILO.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        INSERT LINES OF gt_estilo INTO TABLE wa_saida_0122_itm-estilo.

        APPEND wa_saida_0122_itm TO it_saida_0122_itm.
        PERFORM f_refresh_alv USING '0122'.
      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0122->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Deseja realmente deletar o registro?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        CHECK var_answer EQ '1'.

        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

        READ TABLE it_saida_0122_itm INTO wa_saida_0122_itm INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.

        DELETE it_saida_0122_itm INDEX wa_sel_rows-index.

        IF sy-subrc = 0.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: f_refresh_alv USING '0122'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler_0120 IMPLEMENTATION.

  METHOD on_data_changed_finished.
**** CS2019001041 - Ajustes na ZLES0147 - Inicio - CBRAND
    DATA: v_tabix   TYPE sy-tabix,
          wa_estilo TYPE lvc_s_styl.


    LOOP AT it_saida_0120 INTO wa_saida_0120 .

      v_tabix = sy-tabix.

      IF wa_saida_0120-numero_due IS NOT INITIAL.

        SELECT SINGLE *
          FROM zsdt0170 INTO @DATA(_wl_0170_aux)
         WHERE numero_due       EQ @wa_saida_0120-numero_due.

        IF sy-subrc = 0.
          CLEAR: wa_saida_0120-estilo.
          wa_estilo-fieldname = 'DS_NOME_TRANSPOR'.
          wa_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
          INSERT wa_estilo INTO TABLE wa_saida_0120-estilo.

          wa_estilo-fieldname = 'CNPJ_DECLARANTE'.
          wa_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
          INSERT wa_estilo INTO TABLE wa_saida_0120-estilo.


          CLEAR: wa_saida_0120-ds_nome_transpor,
                 wa_saida_0120-cnpj_declarante.

          MODIFY it_saida_0120 FROM wa_saida_0120 INDEX v_tabix.

        ELSE.
          CLEAR: wa_saida_0120-estilo.
          wa_estilo-fieldname = 'DS_NOME_TRANSPOR'.
          wa_estilo-style     = cl_gui_alv_grid=>mc_style_enabled.
          INSERT wa_estilo INTO TABLE wa_saida_0120-estilo.

          wa_estilo-fieldname = 'CNPJ_DECLARANTE'.
          wa_estilo-style     = cl_gui_alv_grid=>mc_style_enabled.
          INSERT wa_estilo INTO TABLE wa_saida_0120-estilo.

          MODIFY it_saida_0120 FROM wa_saida_0120 INDEX v_tabix.

        ENDIF.

        IF strlen( wa_saida_0120-numero_due ) NE 14.
          MESSAGE 'Número da DU-e é inválido!' TYPE 'S'.
          EXIT.
        ENDIF.

      ENDIF.
    ENDLOOP.

    CALL METHOD refresh_grid.

**** CS2019001041 - Ajustes na ZLES0147 - Fim - CBRAND
  ENDMETHOD.

  METHOD on_data_changed.

    DATA: it_good_cells_aux TYPE lvc_t_modi,
          v_numero_due      TYPE zsdt0170-numero_due,
          v_tabix           TYPE sy-tabix,
          wa_estilo         TYPE lvc_s_styl.

    it_good_cells_aux = er_data_changed->mt_good_cells.

    SORT it_good_cells_aux BY row_id.
    DELETE ADJACENT DUPLICATES FROM it_good_cells_aux COMPARING row_id.

    LOOP AT it_good_cells_aux INTO DATA(ls_good).

      v_tabix = sy-tabix.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.

    ENDLOOP.
  ENDMETHOD.

  METHOD refresh_grid.

    DATA:
      l_scroll_row_no   TYPE lvc_s_roid,
      l_scroll_row_info TYPE lvc_s_row,
      l_scroll_col_info TYPE lvc_s_col,
      l_cell_row_no     TYPE lvc_s_roid,
      l_cell_row_id     TYPE lvc_s_row,
      l_cell_col_id     TYPE lvc_s_col,

      mt_sel_cells      TYPE lvc_t_ceno,
      mt_sel_rows       TYPE lvc_t_row.

* Save the scroll info
    obj_alv_0120->get_scroll_info_via_id(
    IMPORTING
      es_row_no   = l_scroll_row_no
      es_row_info = l_scroll_row_info
      es_col_info = l_scroll_col_info
      ).

* Save info about last selected cell
    obj_alv_0120->get_current_cell(
      IMPORTING
        es_row_id = l_cell_row_id
        es_col_id = l_cell_col_id
        es_row_no = l_cell_row_no ).

* Save info about selected rows
    obj_alv_0120->get_selected_rows(
      IMPORTING
        et_index_rows = mt_sel_rows ).

* If no row is selected save info about selected cells
    IF mt_sel_rows[] IS INITIAL.
      obj_alv_0120->get_selected_cells_id(
        IMPORTING
          et_cells = mt_sel_cells
      ).
    ENDIF.

* ALV Grid refresh
    obj_alv_0120->refresh_table_display( i_soft_refresh = i_soft ).

* Restore the saved selection
    "IF i_set_selected = abap_true.
    IF mt_sel_cells[] IS NOT INITIAL.
      obj_alv_0120->set_selected_cells_id( it_cells = mt_sel_cells   ).
    ELSE.
      obj_alv_0120->set_selected_rows(
        it_index_rows            = mt_sel_rows
      ).
    ENDIF.
    "ENDIF.

* Restore previously saved scroll position
    obj_alv_0120->set_scroll_info_via_id(
      is_row_info = l_scroll_row_info
      is_col_info = l_scroll_col_info
      is_row_no   = l_scroll_row_no
    ).

* Set focus on previously selected cell
    IF i_set_current = abap_true.
      obj_alv_0120->set_current_cell_via_id(
        is_row_id    = l_cell_row_id
        is_column_id = l_cell_col_id
        is_row_no    = l_cell_row_no ).
    ENDIF.

    REFRESH: mt_sel_rows[], mt_sel_cells[].


  ENDMETHOD.


ENDCLASS.


CLASS lcl_event_handler_0122 IMPLEMENTATION.

  METHOD on_data_changed_finished.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: it_good_cells_aux TYPE lvc_t_modi,
          lv_value          TYPE lvc_value.

    it_good_cells_aux = er_data_changed->mt_good_cells.

    SORT it_good_cells_aux BY row_id.
    DELETE ADJACENT DUPLICATES FROM it_good_cells_aux COMPARING row_id.

    LOOP AT it_good_cells_aux INTO DATA(ls_good).

*      CLEAR: LV_VALUE.
*      IF LS_GOOD-FIELDNAME = 'PESO_BRUTO_TOTAL'.
*        LV_VALUE = LS_GOOD-VALUE.
*        CONDENSE LV_VALUE NO-GAPS.
*
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'PESO_BRUTO_ENTREGUE'
*            I_VALUE     = LV_VALUE.
*      ENDIF.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
