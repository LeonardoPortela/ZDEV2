*&---------------------------------------------------------------------*
*& Include          ZMMR0045_CLASSE
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
        IMPORTING e_object,

      toolbar_item          FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

    DATA: w_good   TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          lv_maktx TYPE makt-maktx,
          lv_meins TYPE mara-meins,
          lv_netwr TYPE zi_mm_boleta_comp_it_info-netwr.

    LOOP AT er_data_changed->mt_good_cells INTO w_good WHERE fieldname = 'MATNR'.
      READ TABLE t_saida_item INTO w_saida_item INDEX w_good-row_id.

      lv_maktx = abap_off.
      lv_value = w_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE maktx
        INTO     lv_maktx
        FROM makt
       WHERE matnr = lv_value
         AND spras = sy-langu.

      IF sy-subrc = 0.
        lv_value = lv_maktx.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = w_good-row_id
            i_fieldname = 'MAKTX'
            i_value     = lv_value.

        w_saida_item-maktx     = lv_value.
        MODIFY t_saida_item FROM w_saida_item INDEX w_good-row_id.
      ENDIF.

      lv_meins = abap_off.
      lv_value = w_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE meins
        INTO     lv_meins
        FROM mara
       WHERE matnr = lv_value.

      IF sy-subrc = 0.
        lv_value = lv_meins.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = w_good-row_id
            i_fieldname = 'MEINS'
            i_value     = lv_value.

        w_saida_item-meins     = lv_value.
        MODIFY t_saida_item FROM w_saida_item INDEX w_good-row_id.
      ENDIF.
    ENDLOOP.


    LOOP AT er_data_changed->mt_good_cells INTO w_good WHERE fieldname = 'MENGE'.
      READ TABLE t_saida_item INTO w_saida_item INDEX w_good-row_id.

      lv_value = w_good-value.
      CONDENSE lv_value NO-GAPS.

      lv_netwr = lv_value * w_saida_item-netpr.
      lv_value = lv_netwr.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = w_good-row_id
          i_fieldname = 'NETWR'
          i_value     = lv_value.

      w_saida_item-netwr     = lv_value.
      MODIFY t_saida_item FROM w_saida_item INDEX w_good-row_id.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO w_good WHERE fieldname = 'NETPR'.
      READ TABLE t_saida_item INTO w_saida_item INDEX w_good-row_id.

      lv_value = w_good-value.
      CONDENSE lv_value NO-GAPS.

      lv_netwr = w_saida_item-menge * lv_value.
      lv_value = lv_netwr.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = w_good-row_id
          i_fieldname = 'NETWR'
          i_value     = lv_value.

      w_saida_item-netwr     = lv_value.
      MODIFY t_saida_item FROM w_saida_item INDEX w_good-row_id.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO w_good.

      READ TABLE t_saida_item INTO w_saida_item INDEX w_good-row_id.
      w_saida_item-editado       = abap_true.
      MODIFY t_saida_item     FROM w_saida_item INDEX w_good-row_id.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_hotspot_click.

    CHECK e_row_id-rowtype IS INITIAL.

    READ TABLE t_saida_item INTO w_saida_item INDEX e_row_id.

    CASE e_column_id.
      WHEN 'NROSOLCP'.
        CHECK w_saida_item-nrosolcp IS NOT INITIAL.

        SET PARAMETER ID 'SOLI_51' FIELD w_saida_item-nrosolcp.
        CALL TRANSACTION 'ZMM0149' AND SKIP FIRST SCREEN.
    ENDCASE.

  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.

    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_index_rows = t_rows_head.

    CASE e_ucomm.
      WHEN 'VISUALIZAR'.
        PERFORM f_manutencao_boleta USING abap_false.

      WHEN 'EDITAR'.
        PERFORM f_manutencao_boleta USING abap_true.

      WHEN 'ELABORAR'.
        PERFORM f_retorno_elaboracao.

      WHEN 'INSERT' OR 'DELETE' OR 'CHANGE' OR 'COPIAR' OR 'SALVAR'.
        PERFORM f_manutencao_itens  USING e_ucomm.

      WHEN 'CRIARSOLIC_HEADER'.
        PERFORM f_criar_solicitacao USING 'HEADER'.

      WHEN 'CRIARSOLIC_ITEM'.
        PERFORM f_criar_solicitacao USING 'ITEM'.

      WHEN OTHERS.
    ENDCASE.

    IF lines( t_rows_head ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows_head.
    ENDIF.

  ENDMETHOD.

  METHOD toolbar.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool     TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function    = 'VISUALIZAR'.
    wa_tool-icon        = icon_display_text.
    wa_tool-disabled    = abap_false.
    wa_tool-quickinfo   = 'Visualizar Boleta'.
    wa_tool-text        = 'Visualizar Boleta'.
    APPEND wa_tool     TO e_object->mt_toolbar.

    IF p_submit = abap_false.
      wa_tool-function  = 'EDITAR'.
      wa_tool-icon      = icon_change_text.
      wa_tool-disabled  = abap_false.
      wa_tool-quickinfo = 'Editar Boleta'.
      wa_tool-text      = 'Editar Boleta'.
      APPEND wa_tool   TO e_object->mt_toolbar.

      wa_tool-function  = 'ELABORAR'.
      wa_tool-icon      = icon_pdir_back_switch.
      wa_tool-disabled  = abap_false.
      wa_tool-quickinfo = 'Retornar p/ Elaboração'.
      wa_tool-text      = 'Retornar p/ Elaboração'.
      APPEND wa_tool   TO e_object->mt_toolbar.

      wa_tool-function  = 'CRIARSOLIC_HEADER'.
      wa_tool-icon      = icon_create.
      wa_tool-disabled  = abap_false.
      wa_tool-quickinfo = 'Criar Solicitação'.
      wa_tool-text      = 'Criar Solicitação'.
      APPEND wa_tool   TO e_object->mt_toolbar.
    ENDIF.

  ENDMETHOD.

  METHOD toolbar_item.

    DATA wa_tool TYPE stb_button.

    FREE: e_object->mt_toolbar.

    wa_tool-function  = 'INSERT'.
    wa_tool-icon      = icon_insert_row.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Inserir'.
    wa_tool-text      = 'Inserir'.
    APPEND wa_tool TO e_object->mt_toolbar.

    wa_tool-function  = 'COPIAR'.
    wa_tool-icon      = icon_system_copy.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Copiar'.
    wa_tool-text      = 'Copiar'.
    APPEND wa_tool TO e_object->mt_toolbar.

    wa_tool-function  = 'DELETE'.
    wa_tool-icon      = icon_delete_row.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Eliminar'.
    wa_tool-text      = 'Eliminar'.
    APPEND wa_tool TO e_object->mt_toolbar.

    wa_tool-function  = 'CHANGE'.
    wa_tool-icon      = icon_change_text.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Editar'.
    wa_tool-text      = 'Editar'.
    APPEND wa_tool TO e_object->mt_toolbar.

    wa_tool-function  = 'SALVAR'.
    wa_tool-icon      = icon_system_save.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Salvar'.
    wa_tool-text      = 'Salvar'.
    APPEND wa_tool TO e_object->mt_toolbar.

    wa_tool-function  = 'CRIARSOLIC_ITEM'.
    wa_tool-icon      = icon_create.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Criar Solicitação'.
    wa_tool-text      = 'Criar Solicitação'.
    APPEND wa_tool TO e_object->mt_toolbar.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
