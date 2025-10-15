*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

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


  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

    CASE e_ucomm.
      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0100->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
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

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.

        SELECT SINGLE *
          FROM zsdt0006 INTO @DATA(_wl_zsdt0006)
         WHERE auart  EQ @wa_saida_0100-auart
           AND branch EQ @wa_saida_0100-branch.

        IF ( sy-subrc             EQ 0           ) AND
           ( wa_saida_0100-auart  IS NOT INITIAL ).
          DELETE FROM zsdt0006 WHERE auart  = wa_saida_0100-auart
                                 AND branch = wa_saida_0100-branch.
        ELSE.
          sy-subrc = 0.
        ENDIF.

        IF sy-subrc = 0.
          DELETE it_saida_0100 WHERE auart = wa_saida_0100-auart AND branch = wa_saida_0100-branch.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: f_refresh_alv USING '0100'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      WHEN c_novo.

        CLEAR: wa_saida_0100.

        APPEND wa_saida_0100 TO it_saida_0100.
        PERFORM f_refresh_alv USING '0100'.

        CALL METHOD obj_alv_0100->check_changed_data( ).

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_alv_toolbar_0120 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

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

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

    CASE e_ucomm.
      WHEN c_novo.
        CLEAR: wa_saida_0120.

        wa_saida_0120-auart = wa_saida_0100-auart.

        APPEND wa_saida_0120 TO it_saida_0120.
        PERFORM f_refresh_alv USING '0120'.

        CALL METHOD obj_alv_0100->check_changed_data( ).

      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0120->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'  DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
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

        SELECT SINGLE *
          FROM zsdt0294 INTO @DATA(_wl_zsdt0294)
         WHERE auart  = @wa_saida_0120-auart
           AND branch = @wa_saida_0120-branch.

        IF ( sy-subrc             EQ 0           ) AND
           ( wa_saida_0120-auart  IS NOT INITIAL ) AND
           ( wa_saida_0120-branch IS NOT INITIAL ).
          DELETE FROM zsdt0294 WHERE auart  = wa_saida_0120-auart
                                 AND branch = wa_saida_0120-branch.
        ELSE.
          sy-subrc = 0.
        ENDIF.

        IF sy-subrc = 0.
          DELETE it_saida_0120 WHERE auart  = wa_saida_0120-auart
                                 AND branch = wa_saida_0120-branch.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: f_refresh_alv USING '0120'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'  DISPLAY LIKE 'E'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_alv_toolbar_0130 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

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

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer    TYPE c,
          t_0287        TYPE TABLE OF zsdt0287,
          lv_count      TYPE i,
          lt_saida_0130 TYPE TABLE OF ty_saida_0130.

    CASE e_ucomm.
      WHEN c_novo.

*       DESCRIBE TABLE it_saida_0130 LINES lv_count.

*-BUG 65582-17.05.2022-JT-inicio
        FREE: lv_count.
*       SELECT *
*         FROM zsdt0287
*         INTO TABLE @DATA(t_287).
*
*       SORT t_287 BY id_obs DESCENDING.
*       READ TABLE t_287 INTO DATA(w_287) INDEX 1.
*       IF sy-subrc = 0.
*         lv_count = w_287-id_obs.
*       ELSE.
*         lv_count = 0.
*       ENDIF.
*-BUG 65582-17.05.2022-JT-inicio

        CLEAR: wa_saida_0130.
*       wa_saida_0130-id_obs = lv_count + 1.  "*-BUG 65582-17.05.2022-JT-inicio

        APPEND wa_saida_0130 TO it_saida_0130.
        PERFORM f_refresh_alv USING '0130'.

        CALL METHOD obj_alv_0100->check_changed_data( ).

      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0130->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'  DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

*        IF lines( it_sel_rows ) NE 1.
*          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'  DISPLAY LIKE 'E'.
*          EXIT.
*        ENDIF.

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

        LOOP AT it_sel_rows INTO wa_sel_rows.

*        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

          READ TABLE it_saida_0130 INTO wa_saida_0130 INDEX wa_sel_rows-index.

          IF ( sy-subrc IS NOT INITIAL ).
            CONTINUE.
          ENDIF.
*        CHECK sy-subrc = 0.
          APPEND wa_saida_0130 TO lt_saida_0130.
        ENDLOOP.

        SELECT *
          FROM zsdt0287
          INTO TABLE @DATA(_tl_zsdt0287)
          FOR ALL ENTRIES IN @lt_saida_0130
          WHERE id_obs EQ @lt_saida_0130-id_obs.

        IF ( sy-subrc IS INITIAL ).
          SELECT  *
            FROM zsdt0294
            INTO TABLE @DATA(_tl_zsdt0294)
            FOR ALL ENTRIES IN @_tl_zsdt0287
            WHERE id_obs    EQ @_tl_zsdt0287-id_obs
              AND cancelado NE @abap_true.

*        SELECT SINGLE *
*          FROM zsdt0287 INTO @DATA(_wl_zsdt0287)
*         WHERE id_obs  = @wa_saida_0130-id_obs.
          "  AND BRANCH = @WA_SAIDA_0130-BRANCH.

*        IF ( sy-subrc             EQ 0           ) AND
*           ( _wl_zsdt0287-id_obs  IS NOT INITIAL )." AND.
*
*          SELECT SINGLE *
*            FROM ZSDT0294
*            INTO @DATA(_wl_ZSDT0294)
*            WHERE id_obs    EQ @wa_saida_0130-id_obs
*              and CANCELADO eq @abap_true.

*          IF _wl_ZSDT0294 IS INITIAL.
          SORT _tl_zsdt0287 BY id_obs.
          SORT _tl_zsdt0294 BY id_obs.
          LOOP AT lt_saida_0130 INTO wa_saida_0130.
            READ TABLE _tl_zsdt0287 INTO DATA(_wl_zsdt0287) WITH KEY id_obs = wa_saida_0130-id_obs BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              READ TABLE _tl_zsdt0294 INTO DATA(_wl_zsdt0294) WITH KEY id_obs = _wl_zsdt0287-id_obs BINARY SEARCH.
              IF sy-subrc IS NOT INITIAL.

                _wl_zsdt0287-cancelado =  'X'.
                _wl_zsdt0287-dt_delete =  sy-datum.
                _wl_zsdt0287-hr_delete =  sy-uzeit.
                _wl_zsdt0287-us_delete =  sy-uname.

                PERFORM f_grava_zsdt0287 TABLES t_0287
                                          USING _wl_zsdt0287.

                " DELETE FROM zsdt0287 WHERE id_obs  = wa_saida_0130-id_obs.
*           MODIFY zsdt0287 FROM _wl_zsdt0287.
              ELSE.

*                MESSAGE 'Observação sendo utilizada, desvincular no botão "Parametrizar Observações' TYPE 'I'.
                MESSAGE s836(sd) DISPLAY LIKE 'E' WITH |Observação do ID { _wl_zsdt0287-id_obs } está vinculada ao tipo|
                                                       |de OV { _wl_zsdt0294-auart } na filial { _wl_zsdt0294-branch }!|.
                DATA(lv_message) = abap_false.
                EXIT.
              ENDIF.
            ENDIF.
*        ENDIF.

            " IF sy-subrc = 0.
            IF _wl_zsdt0294 IS INITIAL.
              DELETE it_saida_0130 WHERE id_obs  = wa_saida_0130-id_obs.
              lv_message = abap_true.
              "  AND BRANCH = WA_SAIDA_0130-BRANCH.
*          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
*          PERFORM: f_refresh_alv USING '0130'.
*        ELSE.
*          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'  DISPLAY LIKE 'E'.
            ENDIF.
            FREE: _wl_zsdt0294, _wl_zsdt0287.
          ENDLOOP.
          IF lv_message IS NOT INITIAL.
            MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
            PERFORM: f_refresh_alv USING '0130'.
*          ELSE.
*            MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S'  DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_alv_toolbar_0140 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

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

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.
    DATA: _wl_zsdt0294 TYPE zsdt0294.

    CASE e_ucomm.
      WHEN c_novo.
        CLEAR: wa_saida_0140.

        wa_saida_0140-branch =  wa_saida_0100-branch.
        wa_saida_0140-auart =  wa_saida_0100-auart.
        APPEND wa_saida_0140 TO it_saida_0140.
        PERFORM f_refresh_alv USING '0140'.

        CALL METHOD obj_alv_0100->check_changed_data( ).

      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0140->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
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

        READ TABLE it_saida_0140 INTO wa_saida_0140 INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.
*        SELECT SINGLE *
*               FROM ZSDT0294
*               INTO @DATA(_wl_ZSDT0294)
*               WHERE id_obs EQ   @wa_saida_0140-id_obs
*          AND branch EQ  @wa_saida_0140-branch
*          AND auart EQ @wa_saida_0140-auart
*          AND matnr  EQ @wa_saida_0140-matnr.

        MOVE-CORRESPONDING wa_saida_0140 TO _wl_zsdt0294.

        IF _wl_zsdt0294 IS NOT INITIAL.
          _wl_zsdt0294-cancelado =  'X'.
          _wl_zsdt0294-dt_delete =  sy-datum.
          _wl_zsdt0294-hr_delete =  sy-uzeit.
          _wl_zsdt0294-us_delete =  sy-uname.

          " DELETE FROM zsdt0287 WHERE id_obs  = wa_saida_0130-id_obs.
          MODIFY zsdt0294 FROM _wl_zsdt0294.
        ENDIF.


        IF sy-subrc = 0.
          DELETE it_saida_0140 WHERE id_obs  = wa_saida_0140-id_obs
                                 AND branch  = wa_saida_0140-branch
                                 AND auart   = wa_saida_0140-auart
                                 AND matkl   = wa_saida_0140-matkl "*-CS2022000324-25.08.2022-#84903-JT-inicio
                                 AND matnr   = wa_saida_0140-matnr   .
          "  AND BRANCH = WA_SAIDA_0130-BRANCH.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: f_refresh_alv USING '0140'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_alv_toolbar_0150 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

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

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.
    DATA: _wl_zsdt0294 TYPE zsdt0294.

    CASE e_ucomm.
      WHEN c_novo.
        CLEAR: wa_saida_0150.

        wa_saida_0150-branch =  wa_saida_0100-branch.
        wa_saida_0150-auart  =  abap_off.
        APPEND wa_saida_0150 TO it_saida_0150.
        PERFORM f_refresh_alv USING '0150'.

        CALL METHOD obj_alv_0100->check_changed_data( ).

      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv_0150->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
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

        READ TABLE it_saida_0150 INTO wa_saida_0150 INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.
*        SELECT SINGLE *
*               FROM ZSDT0294
*               INTO @DATA(_wl_ZSDT0294)
*               WHERE id_obs EQ   @wa_saida_0140-id_obs
*          AND branch EQ  @wa_saida_0140-branch
*          AND auart EQ @wa_saida_0140-auart
*          AND matnr  EQ @wa_saida_0140-matnr.

        MOVE-CORRESPONDING wa_saida_0150 TO _wl_zsdt0294.

        IF _wl_zsdt0294 IS NOT INITIAL.
          _wl_zsdt0294-cancelado =  'X'.
          _wl_zsdt0294-dt_delete =  sy-datum.
          _wl_zsdt0294-hr_delete =  sy-uzeit.
          _wl_zsdt0294-us_delete =  sy-uname.

          " DELETE FROM zsdt0287 WHERE id_obs  = wa_saida_0130-id_obs.
          MODIFY zsdt0294 FROM _wl_zsdt0294.
        ENDIF.


        IF sy-subrc = 0.
          DELETE it_saida_0150 WHERE id_obs  = wa_saida_0150-id_obs
                                 AND branch = wa_saida_0150-branch
                                 AND auart = wa_saida_0150-auart
                                 AND matkl = wa_saida_0150-matkl  "*-CS2022000324-25.08.2022-#84903-JT-inicio
                                 AND matnr = wa_saida_0150-matnr.
          "  AND BRANCH = WA_SAIDA_0130-BRANCH.
          MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
          PERFORM: f_refresh_alv USING '0150'.
        ELSE.
          MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler_0100 IMPLEMENTATION.

  METHOD catch_hotspot.

    CASE e_column_id.
      WHEN 'X'.

    ENDCASE.

  ENDMETHOD.

  METHOD on_data_changed_finished.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: it_good_cells_aux TYPE lvc_t_modi.

    it_good_cells_aux = er_data_changed->mt_good_cells.

    LOOP AT it_good_cells_aux INTO DATA(ls_good).
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.

CLASS lcl_event_handler_0120 IMPLEMENTATION.

  METHOD on_data_changed_finished.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: it_good_cells_aux TYPE lvc_t_modi.

    it_good_cells_aux = er_data_changed->mt_good_cells.

    LOOP AT it_good_cells_aux INTO DATA(ls_good).
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.

CLASS lcl_event_handler_0130 IMPLEMENTATION.

  METHOD on_data_changed_finished.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: it_good_cells_aux TYPE lvc_t_modi.

    it_good_cells_aux = er_data_changed->mt_good_cells.

    LOOP AT it_good_cells_aux INTO DATA(ls_good).
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler_0140 IMPLEMENTATION.

  METHOD on_data_changed_finished.

    l_erro = abap_false.

    LOOP AT it_saida_0140 INTO wa_saida_0140 WHERE ck_modify EQ abap_true.

      IF wa_saida_0140-matnr IS NOT INITIAL.
        SELECT SINGLE maktg
          FROM  makt
          INTO wa_saida_0140-maktg
          WHERE matnr EQ wa_saida_0140-matnr
          AND spras EQ 'P'.

        IF sy-subrc EQ 0 .
          MODIFY it_saida_0140 FROM wa_saida_0140.

          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
        ELSE.
*         wa_saida_0140-matnr = ' '.
          wa_saida_0140-maktg = ' '.
          l_erro              = abap_true.
          MODIFY it_saida_0140 FROM wa_saida_0140.

          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
          MESSAGE 'Material Informado não existe' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        wa_saida_0140-maktg = ' '.
        MODIFY it_saida_0140 FROM wa_saida_0140.

        CALL METHOD obj_alv_0140->refresh_table_display
          EXPORTING
            is_stable      = wa_stable
            i_soft_refresh = 'X'.
      ENDIF.

*-CS2022000324-25.08.2022-#84903-JT-inicio
      IF wa_saida_0140-matkl IS NOT INITIAL.
        SELECT SINGLE wgbez
          FROM t023t
          INTO wa_saida_0140-wgbez
         WHERE spras = sy-langu
           AND matkl = wa_saida_0140-matkl.

        IF sy-subrc EQ 0 .
          MODIFY it_saida_0140 FROM wa_saida_0140.

          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
        ELSE.
*         wa_saida_0140-matkl = ' '.
          wa_saida_0140-wgbez = ' '.
          l_erro              = abap_true.
          MODIFY it_saida_0140 FROM wa_saida_0140.

          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
          MESSAGE 'Grupo de Material Informado não existe' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        wa_saida_0140-wgbez = ' '.
        MODIFY it_saida_0140 FROM wa_saida_0140.

        CALL METHOD obj_alv_0140->refresh_table_display
          EXPORTING
            is_stable      = wa_stable
            i_soft_refresh = 'X'.
      ENDIF.

      IF wa_saida_0140-matkl IS NOT INITIAL AND
         wa_saida_0140-matnr IS NOT INITIAL.
*       wa_saida_0140-matnr = ' '.
*       wa_saida_0140-maktg = ' '.
*       MODIFY it_saida_0140 FROM wa_saida_0140.
        l_erro              = abap_true.

        CALL METHOD obj_alv_0140->refresh_table_display
          EXPORTING
            is_stable      = wa_stable
            i_soft_refresh = 'X'.
        MESSAGE 'Informe apenas Grupo de Mercadoria ou Cod. Material.' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
*-CS2022000324-25.08.2022-#84903-JT-fim

      IF wa_saida_0140-id_obs IS NOT INITIAL.
        SELECT SINGLE observ
           FROM  zsdt0287
           INTO wa_saida_0140-observ_287
           WHERE id_obs EQ wa_saida_0140-id_obs
             AND cancelado NE 'X'.
        IF sy-subrc EQ 0 .
          MODIFY it_saida_0140 FROM wa_saida_0140.

          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
        ELSE.
*         wa_saida_0140-id_obs = ' '.
          wa_saida_0140-observ_287 = ' '.
          l_erro              = abap_true.
          MODIFY it_saida_0140 FROM wa_saida_0140.

          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.

          MESSAGE 'ID Observação não existe ou está cancelado!' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        l_erro              = abap_true.
        CALL METHOD obj_alv_0140->refresh_table_display
          EXPORTING
            is_stable      = wa_stable
            i_soft_refresh = 'X'.
        MESSAGE 'Informar Uma Observação!' TYPE 'S' DISPLAY LIKE 'E'.

      ENDIF.

      IF wa_saida_0140-branch IS NOT INITIAL.

        SELECT SINGLE branch
          FROM j_1bbranch
          INTO wa_saida_0140-branch
          WHERE branch = wa_saida_0140-branch.

        IF sy-subrc IS NOT INITIAL.
*         wa_saida_0140-branch = ' '.
          l_erro              = abap_true.
          MODIFY it_saida_0140 FROM wa_saida_0140.

          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.

          MESSAGE 'Filial Não encontrada' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      ENDIF.

      IF wa_saida_0140-ck_modify = 'X'.

        IF lines( it_saida_0140 ) NE 1.

          IF wa_saida_0140-id_obs IS NOT INITIAL AND
             wa_saida_0140-branch IS NOT INITIAL AND
             wa_saida_0140-auart IS NOT INITIAL  AND
             wa_saida_0140-matnr IS NOT INITIAL  AND
             wa_saida_0140-matkl IS NOT INITIAL.   "*-CS2022000324-25.08.2022-#84903-JT-inicio

            READ TABLE it_saida_0140 INTO DATA(wa_existe)  WITH KEY   id_obs = wa_saida_0140-id_obs
                                                    branch = wa_saida_0140-branch
                                                    auart = wa_saida_0140-auart
                                                    matkl = wa_saida_0140-matkl  "*-CS2022000324-25.08.2022-#84903-JT-inicio
                                                    matnr = wa_saida_0140-matnr
                                                    ck_modify = ' '.


            "DATA(wa_existe) = it_saida_0140_aux[ id_obs = wa_saida_0140-id_obs branch = wa_saida_0140-branch auart = wa_saida_0140-auart matnr = wa_saida_0140-matnr ].
            IF wa_existe IS NOT INITIAL ." AND wa_existe-CK_MODIFY NE 'X'.

              DELETE it_saida_0140 WHERE ck_modify = 'X'.
              MESSAGE 'Registro Duplicado não permitido' TYPE 'S'  DISPLAY LIKE 'E'.
              CLEAR:wa_existe.
              l_erro              = abap_true.

              CALL METHOD obj_alv_0140->refresh_table_display
                EXPORTING
                  is_stable      = wa_stable
                  i_soft_refresh = 'X'.

            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: it_good_cells_aux TYPE lvc_t_modi.
    it_good_cells_aux = er_data_changed->mt_good_cells.

    LOOP AT it_good_cells_aux INTO DATA(ls_good).
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler_0150 IMPLEMENTATION.

  METHOD on_data_changed_finished.

    l_erro = abap_false.

    LOOP AT it_saida_0150 INTO wa_saida_0150 WHERE ck_modify EQ abap_true.

      IF wa_saida_0150-matnr IS NOT INITIAL.
        SELECT SINGLE maktg
          FROM  makt
          INTO wa_saida_0150-maktg
          WHERE matnr EQ wa_saida_0150-matnr
          AND spras EQ 'P'.

        IF sy-subrc EQ 0 .
          MODIFY it_saida_0150 FROM wa_saida_0150.

          CALL METHOD obj_alv_0150->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
        ELSE.
*         wa_saida_0150-matnr = ' '.
          wa_saida_0150-maktg = ' '.
          l_erro              = abap_true.
          MODIFY it_saida_0150 FROM wa_saida_0150.

          CALL METHOD obj_alv_0150->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
          MESSAGE 'Material Informado não existe' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        wa_saida_0150-maktg = ' '.
        MODIFY it_saida_0150 FROM wa_saida_0150.

        CALL METHOD obj_alv_0150->refresh_table_display
          EXPORTING
            is_stable      = wa_stable
            i_soft_refresh = 'X'.
      ENDIF.

*-CS2022000324-25.08.2022-#84903-JT-inicio
      IF wa_saida_0150-matkl IS NOT INITIAL.
        SELECT SINGLE wgbez
          FROM t023t
          INTO wa_saida_0150-wgbez
         WHERE spras = sy-langu
           AND matkl = wa_saida_0150-matkl.

        IF sy-subrc EQ 0 .
          MODIFY it_saida_0150 FROM wa_saida_0150.

          CALL METHOD obj_alv_0150->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
        ELSE.
*         wa_saida_0150-matkl = ' '.
          wa_saida_0150-wgbez = ' '.
          l_erro              = abap_true.
          MODIFY it_saida_0150 FROM wa_saida_0150.

          CALL METHOD obj_alv_0150->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
          MESSAGE 'Grupo de Material Informado não existe' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        wa_saida_0150-wgbez = ' '.
        MODIFY it_saida_0150 FROM wa_saida_0150.

        CALL METHOD obj_alv_0150->refresh_table_display
          EXPORTING
            is_stable      = wa_stable
            i_soft_refresh = 'X'.
      ENDIF.

      IF wa_saida_0150-matkl IS NOT INITIAL AND
         wa_saida_0150-matnr IS NOT INITIAL.
*       wa_saida_0150-matnr = ' '.
*       wa_saida_0150-maktg = ' '.
*       MODIFY it_saida_0150 FROM wa_saida_0150.
        l_erro              = abap_true.

        CALL METHOD obj_alv_0150->refresh_table_display
          EXPORTING
            is_stable      = wa_stable
            i_soft_refresh = 'X'.
        MESSAGE 'Informe apenas Grupo de Mercadoria ou Cod. Material.' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
*-CS2022000324-25.08.2022-#84903-JT-fim

      IF wa_saida_0150-id_obs IS NOT INITIAL.
        SELECT SINGLE observ
           FROM  zsdt0287
           INTO wa_saida_0150-observ_287
           WHERE id_obs EQ wa_saida_0150-id_obs.
        IF sy-subrc EQ 0 .
          MODIFY it_saida_0150 FROM wa_saida_0150.

          CALL METHOD obj_alv_0150->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.
        ELSE.
          wa_saida_0150-id_obs = ' '.
          wa_saida_0150-observ_287 = ' '.
          MODIFY it_saida_0150 FROM wa_saida_0150.

          CALL METHOD obj_alv_0150->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.

          MESSAGE 'ID Observação não existe' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP -->>>
      "ELSE.
      ELSEIF wa_saida_0150-inf_add_prod EQ abap_false.
      "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP <<---

        l_erro              = abap_true.
        CALL METHOD obj_alv_0150->refresh_table_display
          EXPORTING
            is_stable      = wa_stable
            i_soft_refresh = 'X'.
        MESSAGE 'Informar Uma Observação!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.


      IF wa_saida_0150-branch IS NOT INITIAL.

        SELECT SINGLE branch
          FROM j_1bbranch
          INTO wa_saida_0150-branch
          WHERE branch = wa_saida_0150-branch.

        IF sy-subrc IS NOT INITIAL.
*         wa_saida_0150-branch = ' '.
          l_erro              = abap_true.
          MODIFY it_saida_0150 FROM wa_saida_0150.

          CALL METHOD obj_alv_0150->refresh_table_display
            EXPORTING
              is_stable      = wa_stable
              i_soft_refresh = 'X'.

          MESSAGE 'Filial Não encontrada' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      ENDIF.

      IF wa_saida_0150-ck_modify = 'X'.

        IF lines( it_saida_0150 ) NE 1.

          IF wa_saida_0150-id_obs IS NOT INITIAL AND
             wa_saida_0150-branch IS NOT INITIAL AND
             wa_saida_0150-auart IS NOT INITIAL AND
             wa_saida_0150-matkl IS NOT INITIAL AND  "*-CS2022000324-25.08.2022-#84903-JT-inicio
             wa_saida_0150-matnr IS NOT INITIAL.

            READ TABLE it_saida_0150 INTO DATA(wa_existe)  WITH KEY   id_obs = wa_saida_0150-id_obs
                                                    branch = wa_saida_0150-branch
                                                    auart = wa_saida_0150-auart
                                                    matkl = wa_saida_0150-matkl "*-CS2022000324-25.08.2022-#84903-JT-inicio
                                                    matnr = wa_saida_0150-matnr
                                                    ck_modify = ' '.


            "DATA(wa_existe) = it_saida_0150_aux[ id_obs = wa_saida_0140-id_obs branch = wa_saida_0140-branch auart = wa_saida_0140-auart matnr = wa_saida_0140-matnr ].
            IF wa_existe IS NOT INITIAL ." AND wa_existe-CK_MODIFY NE 'X'.

              DELETE it_saida_0150 WHERE ck_modify = 'X'.
              MESSAGE 'Registro Duplicado não permitido' TYPE 'S'  DISPLAY LIKE 'E'.
              CLEAR:wa_existe.
              l_erro              = abap_true.

              CALL METHOD obj_alv_0150->refresh_table_display
                EXPORTING
                  is_stable      = wa_stable
                  i_soft_refresh = 'X'.

            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: it_good_cells_aux TYPE lvc_t_modi.
    it_good_cells_aux = er_data_changed->mt_good_cells.

    LOOP AT it_good_cells_aux INTO DATA(ls_good).
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CK_MODIFY'
          i_value     = 'X'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
