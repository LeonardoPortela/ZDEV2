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
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .
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

    FREE: t_armaz,
          w_zsdt0259,
          g_assin_eletro,
          g_assin_manual.

    g_index_259 = e_row-index.

    READ TABLE t_zsdt0259 INTO w_zsdt0259 INDEX g_index_259.

    LOOP AT t_zsdt0266          INTO w_zsdt0266 WHERE cpf = w_zsdt0259-cpf.
*-CS2021000218-03.10.2022-#91289-JT-inicio
      CLEAR w_t001w.
      READ TABLE t_t001w        INTO w_t001w WITH KEY werks = w_zsdt0266-werks
                                             BINARY SEARCH.
      MOVE-CORRESPONDING w_t001w  TO w_armaz.
      MOVE w_zsdt0266-werks       TO w_armaz-werks.
*-CS2021000218-03.10.2022-#91289-JT-fim
      APPEND w_armaz              TO t_armaz.
    ENDLOOP.

    DO 20 TIMES.
      CLEAR w_armaz.
      APPEND w_armaz              TO t_armaz.
    ENDDO.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = w_zsdt0259-cpf
      IMPORTING
        output = g_cpf.

    g_nome    = w_zsdt0259-nome.
    g_email   = w_zsdt0259-email.

*-CS2021000218-03.10.2022-#91289-JT-inicio
    IF w_zsdt0259-ass_eletronica = abap_true.
      g_assin_eletro = abap_true.
    ELSE.
      g_assin_manual = abap_true.
    ENDIF.
*-CS2021000218-03.10.2022-#91289-JT-fim

    CALL SCREEN 200.

    LEAVE TO SCREEN 0.

  ENDMETHOD.

  METHOD on_data_changed4.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          wl_name1 TYPE makt-maktx,
          l_lifnr  TYPE lifnr,
          l_werks  TYPE werks_ext,
          l_name1  TYPE lfa1-name1,
          l_ort01  TYPE lfa1-ort01,
          l_regio  TYPE lfa1-regio.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good
                             WHERE fieldname = 'WERKS'. "*-CS2021000218-03.10.2022-#91289-JT-inicio

      CLEAR: l_werks, l_name1, l_ort01, l_regio.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      IF w_zsdt0259-status = 'N'.
        EXIT.
      ENDIF.

      READ TABLE t_t001w INTO w_t001w WITH KEY werks = lv_value "*-CS2021000218-03.10.2022-#91289-JT-inicio
                                      BINARY SEARCH.
      IF sy-subrc <> 0.
        MESSAGE s024(sd) WITH text-100 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      READ TABLE t_zsdt0266 INTO w_zsdt0266
                            WITH KEY cpf   = w_zsdt0259-cpf
                                     werks = lv_value. "*-CS2021000218-03.10.2022-#91289-JT-inicio
      IF sy-subrc = 0.
        EXIT.
      ENDIF.

      w_zsdt0266-cpf         = w_zsdt0259-cpf.
      w_zsdt0266-werks       = lv_value.       "*-CS2021000218-03.10.2022-#91289-JT-inicio
      APPEND w_zsdt0266     TO t_zsdt0266.

      lv_value = w_t001w-name1.  "*-CS2021000218-03.10.2022-#91289-JT-inicio
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NAME1'
          i_value     = lv_value.

      lv_value = w_t001w-ort01.  "*-CS2021000218-03.10.2022-#91289-JT-inicio
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'ORT01'
          i_value     = lv_value.

      lv_value = w_t001w-regio.  "*-CS2021000218-03.10.2022-#91289-JT-inicio
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'REGIO'
          i_value     = lv_value.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event IMPLEMENTATION.

  METHOD toolbar.

    FREE e_object->mt_toolbar.

*    IF l_edit = abap_true.
*      CLEAR w_tool.
*      w_tool-function = 'INSERT'. "cl_gui_alv_grid=>mc_fc_loc_insert_row.
*      w_tool-text     = ''.
*      w_tool-icon     = '@17@'.
*      APPEND w_tool TO e_object->mt_toolbar.
*
*      CLEAR w_tool.
*      w_tool-function = 'DELETE'. "cl_gui_alv_grid=>mc_fc_loc_delete_row.
*      w_tool-text     = ''.
*      w_tool-icon     = '@18@'.
*      APPEND w_tool TO e_object->mt_toolbar.
*    ENDIF.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    CALL METHOD g_grid->refresh_table_display.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event2 IMPLEMENTATION.

  METHOD toolbar.

    FREE e_object->mt_toolbar.

    IF l_edit = abap_true.
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
    ENDIF.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    CASE e_ucomm.

      WHEN 'INSERT'.
        DESCRIBE TABLE t_armaz LINES l_lines.
        READ TABLE t_armaz INTO w_armaz INDEX l_lines.
        CLEAR w_armaz.
        APPEND w_armaz  TO t_armaz.

      WHEN 'DELETE'.

        FREE: t_del_rows.

        CALL METHOD g_grid2->check_changed_data.
        CALL METHOD g_grid2->get_selected_rows
          IMPORTING
            et_index_rows = t_del_rows.

        SORT t_del_rows BY index DESCENDING.

        LOOP AT t_del_rows INTO w_del_rows.
          READ TABLE t_armaz    INTO w_armaz INDEX w_del_rows-index.
          READ TABLE t_zsdt0266 INTO w_zsdt0266
                                WITH KEY cpf     = w_zsdt0259-cpf
                                         werks   = w_armaz-werks.  "*-CS2021000218-03.10.2022-#91289-JT-inicio
          IF sy-subrc = 0.
*-CS2021000218-03.10.2022-#91289-JT-inicio
*           DELETE t_zsdt0266 INDEX sy-tabix.
            w_zsdt0266-werks     = abap_off.
            MODIFY t_zsdt0266 FROM w_zsdt0266 INDEX sy-tabix TRANSPORTING werks.
*-CS2021000218-03.10.2022-#91289-JT-fim
          ENDIF.
          DELETE t_armaz INDEX w_del_rows-index.
        ENDLOOP.

    ENDCASE.

    CALL METHOD g_grid2->refresh_table_display.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.
