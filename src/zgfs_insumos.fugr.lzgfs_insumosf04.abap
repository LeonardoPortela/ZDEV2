*----------------------------------------------------------------------*
***INCLUDE LZGFS_INSUMOSF04.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      toolbar               FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.

*    READ TABLE t_anali INTO w_anali INDEX e_row_id-index.
*
*    CASE e_column_id.
*      WHEN 'DOCUMENTO'.
*        PERFORM f_gerar_documento          USING w_anali-id_documento
*                                                 w_anali-tpdoc
*                                                 w_anali-status_cod.
*        PERFORM f_selecao_dados            USING w_zsdt0310-nr_venda.
*        PERFORM f_processa_dados_analitico.
*    ENDCASE.
*
*    CALL METHOD g_grid->refresh_table_display
*      EXPORTING
*        is_stable = w_stable.

    CASE e_column_id.
      WHEN 'MENSAGEM'.
        READ TABLE t_log INTO w_log INDEX e_row_id-index.

        IF sy-subrc = 0.
          CALL FUNCTION 'ZSD_INSUMOS_EXIBIR_MSG'
            EXPORTING
              i_mensagem = w_log-mensagem.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ENDMETHOD.

  METHOD toolbar.

  ENDMETHOD.

ENDCLASS.
