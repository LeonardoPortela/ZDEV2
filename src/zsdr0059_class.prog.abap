*&---------------------------------------------------------------------*
*&  Include           ZSDR0059_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_print.
    ty_toolbar-function  = 'DANFE'.
    ty_toolbar-text      = 'DANFE'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*-CS2020001253 - 16.07.2021 - JT - inicio
    ty_toolbar-icon      = icon_xml_doc.
    ty_toolbar-function  = 'VISAO'.
    ty_toolbar-text      = 'Detalhe NF-e'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*-CS2020001253 - 16.07.2021 - JT - fim

*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'DANFE'.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0100->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        CHECK it_sel_rows IS NOT INITIAL.

        LOOP AT it_sel_rows INTO wa_sel_rows.
          READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
          CHECK sy-subrc = 0.

          IF wa_saida_0100-model NE '55'.
            MESSAGE 'Documento não é um DANFE' TYPE 'S'.
            CONTINUE.
          ENDIF.

          TRY.
              zcl_nfe_inbound=>danfe( i_chave_nfe = wa_saida_0100-chave ).
            CATCH zcx_nfe_inbound_exception INTO DATA(ex_nfe_inbound).
              ex_nfe_inbound->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          ENDTRY.
        ENDLOOP.

*-CS2020001253 - 16.07.2021 - JT - inicio
      WHEN 'VISAO'.
        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0100->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        CHECK it_sel_rows IS NOT INITIAL.

        DESCRIBE TABLE it_sel_rows LINES DATA(l_lines).
        IF l_lines > 1.
          MESSAGE 'Selecionar somente uma linha' TYPE 'S'.
          EXIT.
        ENDIF.

        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
        CHECK sy-subrc = 0.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.
        CHECK sy-subrc = 0.

        IF wa_saida_0100-model NE '55'.
          MESSAGE 'Documento não é um DANFE' TYPE 'S'.
          EXIT.
        ENDIF.

        CALL FUNCTION 'Z_SHOW_DETALHAMENTO_NFE'
          EXPORTING
            i_chave_nfe = wa_saida_0100-chave
          EXCEPTIONS
            no_found    = 1
            OTHERS      = 2.

        IF sy-subrc <> 0.
          MESSAGE 'XML não foi localizado.' TYPE 'S'.
          EXIT.
        ENDIF.
*-CS2020001253 - 16.07.2021 - JT - fim

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "


CLASS lcl_event_handler_0100 IMPLEMENTATION.                 "

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING '0100' e_row_id e_column_id es_row_no.
  ENDMETHOD.

ENDCLASS.           "lcl_event_handler_0102 IMPLEMENTA

CLASS lcl_alv_toolbar_0105 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

*    TY_TOOLBAR-ICON      = ICON_REFRESH.
*    TY_TOOLBAR-FUNCTION  = 'REFRESH'.
*    TY_TOOLBAR-TEXT      = 'Atualizar'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN ''.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "


CLASS lcl_event_handler_0105 IMPLEMENTATION.                 "

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING '0105' e_row_id e_column_id es_row_no.
  ENDMETHOD.

ENDCLASS.           "lcl_event_handler_0102 IMPLEMENTA

CLASS lcl_timer IMPLEMENTATION.
  METHOD handle_finished.


    CALL METHOD obj_alv_0105->get_current_cell
      IMPORTING
        es_row_id = vg_row
        es_col_id = vg_col.

    PERFORM f_selecionar_manifestos USING wa_saida_0100.

    CALL METHOD ob_timer->run.

    LEAVE TO SCREEN 0105.

  ENDMETHOD.                    "handle_finished
ENDCLASS.                    "lcl_receiver IMPLEMENTATION
