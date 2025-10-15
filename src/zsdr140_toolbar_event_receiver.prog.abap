*******************************************************************************************
* classes / implementacao
*******************************************************************************************
CLASS lcl_event DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event2 DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed4 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.

    CLASS-METHODS:
      handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

ENDCLASS.

CLASS lcl_event_handler2 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed4 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.

ENDCLASS.

*----------------------------------------------------------------------*
*   INCLUDE BCALV_TOOLBAR_EVENT_RECEIVER                               *
*----------------------------------------------------------------------*

CLASS lcl_toolbar_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS: on_function_selected
                FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING fcode,

      on_toolbar_dropdown
                  FOR EVENT dropdown_clicked OF cl_gui_toolbar
        IMPORTING fcode
                  posx
                  posy.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_toolbar_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_toolbar_event_receiver IMPLEMENTATION.

  METHOD on_function_selected.

  ENDMETHOD.

  METHOD on_toolbar_dropdown.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD handle_top_of_page.
  ENDMETHOD.

  METHOD on_data_changed4.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          wl_name1 TYPE makt-maktx,
          l_matkl  TYPE matkl,
          l_werks  TYPE werks_ext,
          l_name1  TYPE lfa1-name1,
          l_ort01  TYPE lfa1-ort01,
          l_regio  TYPE lfa1-regio.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE t_matkl INTO w_matkl INDEX ls_good-row_id.

      CASE ls_good-fieldname.
        WHEN 'MATKL'.
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_matkl  = lv_value.

          READ TABLE t_matkl INTO w_matkl WITH KEY matkl = l_matkl.
          IF sy-subrc = 0.
            MESSAGE s024(sd) WITH text-104 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          SELECT SINGLE spart
            INTO @DATA(l_spart)
            FROM t023
           WHERE matkl = @l_matkl.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH text-101 DISPLAY LIKE 'E'.
          ELSE.
            SELECT SINGLE wgbez60
              INTO @DATA(l_wgbez)
              FROM t023t
             WHERE spras = @sy-langu
               AND matkl = @l_matkl.
            IF sy-subrc <> 0.
              CLEAR l_wgbez.
            ENDIF.

            lv_value = l_wgbez.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'WGBEZ'
                i_value     = lv_value.

            w_matkl-wgbez     = l_wgbez.
            w_matkl-modif     = abap_true.
            MODIFY t_matkl FROM w_matkl INDEX ls_good-row_id.
          ENDIF.

        WHEN 'QTD_DIAS_VENCIMENTO' OR
             'BLOQ_LOTES_VENCIDOS' OR
             'LOTE_VENCMTO_PROXIMO'.
          w_matkl-modif     = abap_true.
          MODIFY t_matkl FROM w_matkl INDEX ls_good-row_id.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_on_f1.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_handler2 IMPLEMENTATION.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed4.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          wl_name1 TYPE makt-maktx,
          l_matnr  TYPE matnr,
          l_werks  TYPE werks_ext,
          l_name1  TYPE lfa1-name1,
          l_ort01  TYPE lfa1-ort01,
          l_regio  TYPE lfa1-regio.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE t_matnr INTO w_matnr INDEX ls_good-row_id.

      CASE ls_good-fieldname.
        WHEN 'MATNR'.
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_matnr  = lv_value.

          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = l_matnr
            IMPORTING
              output       = l_matnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.

          READ TABLE t_matnr INTO w_matnr WITH KEY matnr = l_matnr.
          IF sy-subrc = 0.
            MESSAGE s024(sd) WITH text-105 DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          SELECT SINGLE matnr, matkl
            INTO @DATA(w_mara)
            FROM mara
           WHERE matnr = @l_matnr.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH text-103 DISPLAY LIKE 'E'.
          ELSE.
            SELECT SINGLE maktx
              INTO @DATA(l_maktx)
              FROM makt
             WHERE matnr = @w_mara-matnr
               AND spras = @sy-langu.
            IF sy-subrc <> 0.
              CLEAR l_maktx.
            ENDIF.

            SELECT SINGLE wgbez60
              INTO @DATA(l_wgbez)
              FROM t023t
             WHERE spras = @sy-langu
               AND matkl = @w_mara-matkl.
            IF sy-subrc <> 0.
              CLEAR l_wgbez.
            ENDIF.

            lv_value = l_maktx.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'MAKTX'
                i_value     = lv_value.

            lv_value = l_wgbez.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'WGBEZ'
                i_value     = lv_value.

            w_matnr-maktx     = l_maktx.
            w_matnr-wgbez     = l_wgbez.
            w_matnr-modif     = abap_true.
            MODIFY t_matnr FROM w_matnr INDEX ls_good-row_id.
          ENDIF.

        WHEN 'QTD_DIAS_VENCIMENTO' OR
             'BLOQ_LOTES_VENCIDOS' OR
             'LOTE_VENCMTO_PROXIMO'.
          w_matnr-modif     = abap_true.
          MODIFY t_matnr FROM w_matnr INDEX ls_good-row_id.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_on_f1.
  ENDMETHOD.

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event IMPLEMENTATION.

  METHOD toolbar.

*    FREE e_object->mt_toolbar.

    CLEAR w_tool.
    w_tool-function = 'INSERT'. "cl_gui_alv_grid=>mc_fc_loc_insert_row.
    w_tool-text     = ''.
    w_tool-icon     = '@17@'.
    APPEND w_tool TO e_object->mt_toolbar.

    CLEAR w_tool.
    w_tool-function = 'DELETE'. "cl_gui_alv_grid=>mc_fc_loc_delete_row.
    w_tool-text     = ''.
    w_tool-icon     = '@18@'.
    APPEND w_tool TO e_object->mt_toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    CASE e_ucomm.

      WHEN 'INSERT'.
        INSERT INITIAL LINE INTO t_matkl INDEX 1.
        CLEAR w_matkl.
        w_matkl-criado    = abap_true.

        FREE w_matkl-celltab.
        t_estilo =  VALUE #( ( fieldname = 'MATKL'      style = cl_gui_alv_grid=>mc_style_enabled ) ).
        INSERT LINES OF t_estilo INTO TABLE w_matkl-celltab.

        MODIFY t_matkl FROM w_matkl INDEX 1.

      WHEN 'DELETE'.
        FREE: t_del_rows.

        CALL METHOD g_grid->check_changed_data.
        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = t_del_rows.

        IF t_del_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH text-102 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        SORT t_del_rows BY index DESCENDING.

        LOOP AT t_del_rows INTO w_del_rows.
          READ TABLE t_matkl  INTO w_matkl INDEX w_del_rows-index.
          IF sy-subrc = 0.
            APPEND w_matkl    TO t_matkl_del.
            DELETE t_matkl INDEX w_del_rows-index.
          ENDIF.
        ENDLOOP.

    ENDCASE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event2 IMPLEMENTATION.

  METHOD toolbar.

*    FREE e_object->mt_toolbar.

    CLEAR w_tool.
    w_tool-function = 'INSERT'. "cl_gui_alv_grid=>mc_fc_loc_insert_row.
    w_tool-text     = ''.
    w_tool-icon     = '@17@'.
    APPEND w_tool TO e_object->mt_toolbar.

    CLEAR w_tool.
    w_tool-function = 'DELETE'. "cl_gui_alv_grid=>mc_fc_loc_delete_row.
    w_tool-text     = ''.
    w_tool-icon     = '@18@'.
    APPEND w_tool TO e_object->mt_toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    CASE e_ucomm.

      WHEN 'INSERT'.
        INSERT INITIAL LINE INTO t_matnr INDEX 1.
        CLEAR w_matnr.
        w_matnr-criado    = abap_true.

        FREE w_matnr-celltab.
        t_estilo =  VALUE #( ( fieldname = 'MATNR'      style = cl_gui_alv_grid=>mc_style_enabled ) ).
        INSERT LINES OF t_estilo INTO TABLE w_matnr-celltab.

        MODIFY t_matnr FROM w_matnr INDEX 1.

      WHEN 'DELETE'.
        FREE: t_del_rows.

        CALL METHOD g_grid2->check_changed_data.
        CALL METHOD g_grid2->get_selected_rows
          IMPORTING
            et_index_rows = t_del_rows.

        IF t_del_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH text-102 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        SORT t_del_rows BY index DESCENDING.

        LOOP AT t_del_rows INTO w_del_rows.
          READ TABLE t_matnr  INTO w_matnr INDEX w_del_rows-index.
          IF sy-subrc = 0.
            APPEND w_matnr    TO t_matnr_del.
            DELETE t_matnr INDEX w_del_rows-index.
          ENDIF.
        ENDLOOP.

    ENDCASE.

    CALL METHOD g_grid2->refresh_table_display.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.
