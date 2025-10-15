*&---------------------------------------------------------------------*
*& Include          ZPMR0090_CLS
*&---------------------------------------------------------------------*

CLASS gcl_alv_event DEFINITION.

  PUBLIC SECTION.
    METHODS: on_f4
      FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname
                e_fieldvalue
                es_row_no
                er_event_data
                et_bad_cells
                e_display,

      handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

ENDCLASS.


CLASS gcl_alv_event IMPLEMENTATION.

  METHOD on_f4.
    PERFORM on_f4 USING e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.                                                "on_f4

  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.

        WHEN 'PREPARACAO'   OR
             'DESMONTAGEM'  OR
             'MONTAGEM'     OR
             'CALDERARIA'   OR
             'PEDIDO_PECAS' OR
             'MONTAGEM_MEC_GERAL' OR
             'ELETRICA'           OR
             'PNEUS_RODAS' OR
             'LUBRIFICACAO' OR
             'TESTES_FINAIS' OR
             'REFORMA' OR
             'REFORMA_GERAL' OR
             'FREIOS'.

          READ TABLE gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX ls_good-row_id.
          IF sy-subrc IS INITIAL.
            DATA(lv_total) = <fs_saida>-total + ls_good-value.

            IF lv_total > 100.
              MESSAGE 'Total não pode exceder 100%, favor corrigir' TYPE 'S' DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'TOTAL'
                i_value     = lv_total.

          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
