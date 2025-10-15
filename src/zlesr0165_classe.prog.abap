*----------------------------------------------------------------------*
***INCLUDE ZLESR0165_STATUS_0100O01.
*----------------------------------------------------------------------*
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

    DATA: l_mesg TYPE string.

    READ TABLE t_ajustes INTO w_ajustes INDEX e_row_id.

    CASE e_column_id.
      WHEN 'STATUS'.
        l_mesg = w_ajustes-msg_integracao.

        CALL FUNCTION 'ZSD_INSUMOS_EXIBIR_MSG'
          EXPORTING
            i_mensagem = l_mesg.

      WHEN 'GO_TELA'.
        PERFORM f_executa_tela     USING w_ajustes-chvid.

      WHEN 'REENVIA'.
        CHECK w_ajustes-status_integracao <> 'S'.

        PERFORM f_enviar_dados_tip USING w_ajustes-nucontrato
                                         w_ajustes-chvid.
        lv_tela = 'C'.
        PERFORM f_clear_dados.
        PERFORM f_selecao_dados CHANGING lv_erro.
        IF lv_erro = abap_false.
          PERFORM f_montar_dados.
          PERFORM f_init_alv.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.
    CASE e_ucomm.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD toolbar.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
