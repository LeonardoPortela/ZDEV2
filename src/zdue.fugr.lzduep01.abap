*----------------------------------------------------------------------*
***INCLUDE LZDUEP01.
*----------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0120 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    IF ( due_control-modo NE c_due_view ).

      ty_toolbar-icon      = icon_create.
      ty_toolbar-function  = c_novo.
      ty_toolbar-text      = 'Novo'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      = icon_change.
      ty_toolbar-function  = c_change.
      ty_toolbar-text      = 'Modificar'.
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

    IF ( due_control-modo EQ c_due_view ).
      ty_toolbar-icon      = icon_display.
      ty_toolbar-function  = c_view.
      ty_toolbar-text      = 'Visualizar'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    ty_toolbar-icon      = icon_transfer_structure.
    ty_toolbar-function  = c_pais_dst.
    ty_toolbar-text      = 'Países Destino'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_short_message.
    ty_toolbar-function  = c_lpco_item .
    ty_toolbar-text      = 'LPCO Item'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

* Inicio - falheiros - 14.11.2022
    ty_toolbar-icon      = icon_viewer_optical_archive.
    ty_toolbar-function  = c_drawnback .
    ty_toolbar-text      = 'Drawnback'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
* Fim - falheiros - 14.11.2022



    IF cab_due-tp_due = '2'. "Com NF-e
      ty_toolbar-icon      = icon_viewer_optical_archive.
      ty_toolbar-function  = c_fat_ref .
      ty_toolbar-text      = 'Notas Fiscais Item'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      IF due_control-modo EQ c_due_view.
        ty_toolbar-icon      = icon_viewer_optical_archive.
        ty_toolbar-function  = c_fat_ref_due.
        ty_toolbar-text      = 'Notas Fiscais DU-e'.
        ty_toolbar-butn_type = 0.
        APPEND ty_toolbar TO e_object->mt_toolbar.
        CLEAR ty_toolbar.

        ty_toolbar-icon      = icon_workflow_external_event.
        ty_toolbar-function  = c_fat_cons_pre_acd.
        ty_toolbar-text      = 'Consulta NF-e Pré ACD'.
        ty_toolbar-butn_type = 0.
        APPEND ty_toolbar TO e_object->mt_toolbar.
        CLEAR ty_toolbar.
      ENDIF.

      IF cab_due-lcto_avulso EQ abap_true.
        IF ( due_control-modo NE c_due_view ) AND
           ( ( cab_due-performance   EQ abap_true ) OR
             ( cab_due-emb_container EQ abap_true ) ).

          IF cab_due-preenchimento_auto EQ abap_false.
            ty_toolbar-icon      = icon_position.
            ty_toolbar-function  = c_fill_fat_ref.
            ty_toolbar-text      = 'Preencher Notas Fiscais Item'.
            ty_toolbar-butn_type = 0.
            APPEND ty_toolbar TO e_object->mt_toolbar.
            CLEAR ty_toolbar.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: wl_branch TYPE ty_branch.

    CASE e_ucomm.
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
          LEAVE TO SCREEN 0100.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
        ENDIF.

      WHEN c_novo.

        vg_operacao_0120 = e_ucomm.

        CLEAR: zsdt0172.

        PERFORM f_get_max_id_item TABLES it_saida_0120
                                  CHANGING zsdt0172-id_due_item.

        PERFORM f_get_filial_matriz USING cab_due-bukrs
                                 CHANGING wl_branch.

        "Dados Exportador
        IF wl_branch IS NOT INITIAL.
          zsdt0172-exportador_cnpj       = wl_branch-cnpj.
          zsdt0172-exportador_nome       = wl_branch-name.
          zsdt0172-exportador_country    = wl_branch-country.
          zsdt0172-exportador_region     = wl_branch-region.
          CONCATENATE wl_branch-street wl_branch-city2 '/' wl_branch-city1
                 INTO zsdt0172-exportador_cpl_end SEPARATED BY space.
        ENDIF.

        IF due_default-fatura_tp_codigo IS NOT INITIAL.
          zsdt0172-fatura_tp_codigo = due_default-fatura_tp_codigo.
        ENDIF.

        IF due_default-fatura_motivo_dispensa_nf IS NOT INITIAL.
          zsdt0172-fatura_motivo_dispensa_nf = due_default-fatura_motivo_dispensa_nf.
        ENDIF.

        IF due_default-codigo_cond_venda IS NOT INITIAL.
          zsdt0172-codigo_cond_venda = due_default-codigo_cond_venda.
        ENDIF.

        IF due_default-codigo_enquadramento IS NOT INITIAL.
          zsdt0172-codigo_enquadramento = due_default-codigo_enquadramento.
        ENDIF.

        CALL SCREEN 0121 STARTING AT 12 05 ENDING AT 115 25.

        LEAVE TO SCREEN 0100.

      WHEN c_change OR c_view.

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

        CLEAR: zsdt0172.

        MOVE-CORRESPONDING wa_saida_0120 TO zsdt0172.

        vg_operacao_0120 = e_ucomm.

        CALL SCREEN 0121 STARTING AT 12 05 ENDING AT 115 25.

        LEAVE TO SCREEN 0100.

      WHEN c_pais_dst.

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

        "Monta Saída com os Registros Vinculados ao Item
        CLEAR: it_saida_0122_itm[].
        LOOP AT it_saida_0122 INTO wa_saida_0122 WHERE id_due_item  EQ wa_saida_0120-id_due_item.
          CLEAR: wa_saida_0122_itm.
          MOVE-CORRESPONDING wa_saida_0122 TO wa_saida_0122_itm.
          APPEND wa_saida_0122_itm TO it_saida_0122_itm.
        ENDLOOP.

        CALL SCREEN 0122 STARTING AT 12 05 ENDING AT 115 25.

        "Atualiza saída geral com os Registros Atualizados
        DELETE it_saida_0122 WHERE id_due_item  EQ wa_saida_0120-id_due_item.

        LOOP AT it_saida_0122_itm INTO wa_saida_0122_itm WHERE id_due_item  EQ wa_saida_0120-id_due_item.
          CLEAR: wa_saida_0122.
          MOVE-CORRESPONDING wa_saida_0122_itm TO wa_saida_0122.
          APPEND wa_saida_0122 TO it_saida_0122.
        ENDLOOP.

        LEAVE TO SCREEN 0100.

      WHEN c_fat_ref.

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

        "Monta Saída com os Registros Vinculados ao Item
        CLEAR: it_saida_0123_itm[].
        LOOP AT it_saida_0123 INTO wa_saida_0123 WHERE id_due_item  EQ wa_saida_0120-id_due_item.
          CLEAR: wa_saida_0123_itm.
          MOVE-CORRESPONDING wa_saida_0123 TO wa_saida_0123_itm.
          APPEND wa_saida_0123_itm TO it_saida_0123_itm.
        ENDLOOP.

        CALL SCREEN 0123 STARTING AT 12 05 ENDING AT 179 25.

        "Atualiza saída geral com os Registros Atualizados
        DELETE it_saida_0123 WHERE id_due_item  EQ wa_saida_0120-id_due_item.

        LOOP AT it_saida_0123_itm INTO wa_saida_0123_itm WHERE id_due_item  EQ wa_saida_0120-id_due_item.
          CLEAR: wa_saida_0123.
          MOVE-CORRESPONDING wa_saida_0123_itm TO wa_saida_0123.
          APPEND wa_saida_0123 TO it_saida_0123.
        ENDLOOP.

        LEAVE TO SCREEN 0100.

      WHEN c_fat_ref_due.

        "Monta Saída com os Registros Vinculados a DU-e
        CLEAR: it_saida_0123_itm[].
        LOOP AT it_saida_0123 INTO wa_saida_0123.
          CLEAR: wa_saida_0123_itm.
          MOVE-CORRESPONDING wa_saida_0123 TO wa_saida_0123_itm.
          APPEND wa_saida_0123_itm TO it_saida_0123_itm.
        ENDLOOP.

        CALL SCREEN 0123 STARTING AT 12 05 ENDING AT 179 25.

        LEAVE TO SCREEN 0100.

      WHEN c_fill_fat_ref.

        PERFORM f_preencher_nf_item_due_avulso.

        LEAVE TO SCREEN 0100.

      WHEN c_lpco_item.

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

        DATA(_enq_lpco) = abap_false.
        CALL FUNCTION 'ZDUE_CHECK_ENQUADRAMENTO_LPCO'
          EXPORTING
            i_codigo_enquadramento = wa_saida_0120-codigo_enquadramento
          IMPORTING
            e_enq_lpco             = _enq_lpco.

        IF _enq_lpco EQ abap_false.
          MESSAGE 'Enquadramento do Item, não permite inclusão de LPCO!' TYPE 'I'.
          RETURN.
        ENDIF.

        "Monta Saída com os Registros Vinculados ao Item
        CLEAR: it_saida_0125_itm[].
        LOOP AT it_saida_0125 INTO wa_saida_0125 WHERE id_due_item  EQ wa_saida_0120-id_due_item.
          CLEAR: wa_saida_0125_itm.
          MOVE-CORRESPONDING wa_saida_0125 TO wa_saida_0125_itm.
          APPEND wa_saida_0125_itm TO it_saida_0125_itm.
        ENDLOOP.

        CALL SCREEN 0125 STARTING AT 12 05 ENDING AT 115 25.

        "Atualiza saída geral com os Registros Atualizados
        DELETE it_saida_0125 WHERE id_due_item  EQ wa_saida_0120-id_due_item.

        LOOP AT it_saida_0125_itm INTO wa_saida_0125_itm WHERE id_due_item  EQ wa_saida_0120-id_due_item.
          CLEAR: wa_saida_0125.
          MOVE-CORRESPONDING wa_saida_0125_itm TO wa_saida_0125.
          APPEND wa_saida_0125 TO it_saida_0125.
        ENDLOOP.

        LEAVE TO SCREEN 0100.

      WHEN c_fat_cons_pre_acd.

        PERFORM f_fat_cons_pre_acd.

* Inicio - falheiros - 14.11.2022
      WHEN c_drawnback.

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

        IF sy-subrc = 0.

          CALL SCREEN 0126 STARTING AT 12 05 ENDING AT 115 25.

        ENDIF.

* Fim - falheiros - 14.11.2022
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler_0120 IMPLEMENTATION.

  METHOD catch_hotspot.

    CASE e_column_id.
      WHEN 'X'.

    ENDCASE.

  ENDMETHOD.

  METHOD on_data_changed_finished.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_alv_toolbar_0122 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    IF ( due_control-modo NE c_due_view ).

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
        CLEAR: wa_saida_0122_itm.

        wa_saida_0122_itm-ue_exportada  = wa_saida_0120-ue_exportada.
        APPEND wa_saida_0122_itm TO it_saida_0122_itm.

        CALL METHOD obj_alv_0122->check_changed_data.

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


CLASS lcl_alv_toolbar_0123 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    IF ( due_control-modo NE c_due_view ).

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

      CALL METHOD c_alv_toolbarmanager->reorganize
        EXPORTING
          io_alv_toolbar = e_object.

    ENDIF.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

    CASE e_ucomm.
      WHEN c_novo.
        CLEAR: wa_saida_0123_itm.

        IF due_default-fatura_tp_codigo IS NOT INITIAL.
          wa_saida_0123_itm-fatura_tp_codigo = due_default-fatura_tp_codigo.
        ENDIF.

        wa_saida_0123_itm-ue_exportada  = wa_saida_0120-ue_exportada.

        APPEND wa_saida_0123_itm TO it_saida_0123_itm.
        PERFORM f_refresh_alv USING '0123'.
      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0123->get_selected_rows
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

        READ TABLE it_saida_0123_itm INTO wa_saida_0123_itm INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.

        DELETE it_saida_0123_itm INDEX wa_sel_rows-index.

        IF sy-subrc = 0.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: f_refresh_alv USING '0123'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_alv_toolbar_0125 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    IF ( due_control-modo NE c_due_view ).

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

      CALL METHOD c_alv_toolbarmanager->reorganize
        EXPORTING
          io_alv_toolbar = e_object.

    ENDIF.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

    CASE e_ucomm.
      WHEN c_novo.
        CLEAR: wa_saida_0125_itm.

        APPEND wa_saida_0125_itm TO it_saida_0125_itm.
        PERFORM f_refresh_alv USING '0125'.
      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0125->get_selected_rows
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

        READ TABLE it_saida_0125_itm INTO wa_saida_0125_itm INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.

        DELETE it_saida_0125_itm INDEX wa_sel_rows-index.

        IF sy-subrc = 0.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: f_refresh_alv USING '0125'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION



CLASS lcl_event_handler_0122 IMPLEMENTATION.

  METHOD on_data_changed_finished.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: v_meins_in  TYPE mara-meins,
          v_meins_out TYPE mara-meins,
          v_qtde      TYPE j1b_nf_xml_item-qtrib.

    DATA: it_good_cells_aux TYPE lvc_t_modi,
          lv_value          TYPE lvc_value.

    it_good_cells_aux = er_data_changed->mt_good_cells.

    "SORT IT_GOOD_CELLS_AUX BY ROW_ID.
    "DELETE ADJACENT DUPLICATES FROM IT_GOOD_CELLS_AUX COMPARING ROW_ID.

    LOOP AT it_good_cells_aux INTO DATA(ls_good).

      READ TABLE it_saida_0122_itm INTO wa_saida_0122_itm INDEX ls_good-row_id.

      CHECK sy-subrc EQ 0.

      IF ( ls_good-fieldname = 'PESO_LIQ_TOTAL' ) AND ( wa_saida_0122_itm-ue_exportada IS NOT INITIAL ).
        lv_value = ls_good-value.
        CONDENSE lv_value NO-GAPS.

        v_qtde      = lv_value.

        IF ( v_qtde > 0 ).
          v_meins_in  = 'KG'.
          v_meins_out = wa_saida_0122_itm-ue_exportada.

          IF v_meins_out = 'TON'.
            v_meins_out = 'TO'.
          ENDIF.

          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              i_matnr              = wa_saida_0120-matnr
              i_in_me              = v_meins_in
              i_out_me             = v_meins_out
              i_menge              = v_qtde
            IMPORTING
              e_menge              = v_qtde
            EXCEPTIONS
              error_in_application = 1
              error                = 2
              OTHERS               = 3.

          IF sy-subrc = 0.
            lv_value = v_qtde.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'DESTINO_QTDE_UE_EXPORTADA'
                i_value     = lv_value.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.


CLASS lcl_event_handler_0123 IMPLEMENTATION.

  METHOD on_data_changed_finished.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: v_chave_nfe    TYPE zde_chave_nfe,
          v_chave_nff    TYPE zde_chave_nff,
          v_retorno_proc TYPE zde_retorno_proc,
          wl_doc_fat_ref TYPE ty_doc_fat_ref.

    DATA: it_good_cells_aux TYPE lvc_t_modi,
          lv_value          TYPE lvc_value.

    it_good_cells_aux = er_data_changed->mt_good_cells.


    LOOP AT it_good_cells_aux INTO DATA(ls_good).

      READ TABLE it_saida_0123_itm ASSIGNING FIELD-SYMBOL(<fs_saida_0123_itm>) INDEX ls_good-row_id.

      CHECK sy-subrc EQ 0.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      CASE ls_good-fieldname.
        WHEN 'PESO_LIQ_TOTAL'.

          CHECK cab_due-lcto_avulso EQ abap_true.
          CHECK <fs_saida_0123_itm>-ue_exportada IS NOT INITIAL.

          CLEAR: wl_doc_fat_ref.
          wl_doc_fat_ref-meins        = 'KG'.
          wl_doc_fat_ref-menge        = lv_value.
          wl_doc_fat_ref-ue_exportada = <fs_saida_0123_itm>-ue_exportada.
          wl_doc_fat_ref-matnr        = wa_saida_0120-matnr.

          PERFORM f_conv_ue_exp_fat CHANGING wl_doc_fat_ref.

          lv_value = wl_doc_fat_ref-qtde_ue_exportada.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'QTDE_UE_EXPORTADA'
              i_value     = lv_value.

        WHEN 'DOCNUM'.

          CHECK cab_due-lcto_avulso EQ abap_true.
          CHECK <fs_saida_0123_itm>-ue_exportada IS NOT INITIAL.

          <fs_saida_0123_itm>-docnum = lv_value.

          CALL FUNCTION 'ZCCT_MONTA_CHAVE_DOCUMENTO'
            EXPORTING
              i_docnum             = <fs_saida_0123_itm>-docnum
              i_ck_entrada_propria = abap_true
            IMPORTING
              e_chave_nfe          = v_chave_nfe
              e_chave_nff          = v_chave_nff
              e_emissor_cnpj       = <fs_saida_0123_itm>-emissor_cnpj
              e_emissor_cpf        = <fs_saida_0123_itm>-emissor_cpf
              e_emissor_ie         = <fs_saida_0123_itm>-emissor_ie
              e_regio              = <fs_saida_0123_itm>-regio
              e_nfyear             = <fs_saida_0123_itm>-nfyear
              e_nfmonth            = <fs_saida_0123_itm>-nfmonth
              e_model              = <fs_saida_0123_itm>-model
              e_serie              = <fs_saida_0123_itm>-serie
              e_nfnum9             = <fs_saida_0123_itm>-nfnum9
              e_nfnum              = <fs_saida_0123_itm>-nfnum
              e_docnum9            = <fs_saida_0123_itm>-docnum9
              e_cdv                = <fs_saida_0123_itm>-cdv
              e_retorno            = v_retorno_proc.

          IF v_retorno_proc-type EQ 'E'.
            MESSAGE v_retorno_proc-texto TYPE 'S'.
            RETURN.
          ENDIF.

          IF v_chave_nfe IS NOT INITIAL.
            <fs_saida_0123_itm>-id_fatura_ref = v_chave_nfe.
          ELSE.
            <fs_saida_0123_itm>-id_fatura_ref = v_chave_nff.
          ENDIF.

          IF <fs_saida_0123_itm>-id_fatura_ref IS INITIAL.
            MESSAGE s084.
            RETURN.
          ENDIF.

          <fs_saida_0123_itm>-id_fatura          = 1.

          IF <fs_saida_0123_itm>-emissor_cnpj IS NOT INITIAL.
            <fs_saida_0123_itm>-emissor_cnpj_cpf = <fs_saida_0123_itm>-emissor_cnpj.
          ELSEIF <fs_saida_0123_itm>-emissor_cpf IS NOT INITIAL.
            <fs_saida_0123_itm>-emissor_cnpj_cpf = <fs_saida_0123_itm>-emissor_cpf.
          ENDIF.

          IF ( <fs_saida_0123_itm>-nfnum9 IS INITIAL ) AND ( <fs_saida_0123_itm>-nfnum IS NOT INITIAL ).
            <fs_saida_0123_itm>-nfnum9 = wa_saida_0123-nfnum.
          ENDIF.

          lv_value = <fs_saida_0123_itm>-emissor_ie.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'EMISSOR_IE'
              i_value     = lv_value.

          lv_value = <fs_saida_0123_itm>-model.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'MODEL'
              i_value     = lv_value.

          lv_value = <fs_saida_0123_itm>-serie.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'SERIE'
              i_value     = lv_value.

          lv_value = <fs_saida_0123_itm>-nfnum9.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'NFNUM9'
              i_value     = lv_value.

          lv_value = <fs_saida_0123_itm>-id_fatura_ref.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'ID_FATURA_REF'
              i_value     = lv_value.

          lv_value = <fs_saida_0123_itm>-emissor_cnpj_cpf.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'EMISSOR_CNPJ_CPF'
              i_value     = lv_value.
      ENDCASE.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.
    ENDLOOP.


  ENDMETHOD.

ENDCLASS.


CLASS lcl_event_handler_0125 IMPLEMENTATION.

  METHOD on_data_changed_finished.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: v_meins_in  TYPE mara-meins,
          v_meins_out TYPE mara-meins,
          v_qtde      TYPE j1b_nf_xml_item-qtrib.

    DATA: it_good_cells_aux TYPE lvc_t_modi,
          lv_value          TYPE lvc_value.

    it_good_cells_aux = er_data_changed->mt_good_cells.

    LOOP AT it_good_cells_aux INTO DATA(ls_good).

      READ TABLE it_saida_0125_itm INTO wa_saida_0125_itm INDEX ls_good-row_id.

      CHECK sy-subrc EQ 0.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.
