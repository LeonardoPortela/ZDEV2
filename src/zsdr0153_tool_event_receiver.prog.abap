*&---------------------------------------------------------------------*
*&  Include           ZSDR0153_TOOL_EVENT_RECEIVER
*&---------------------------------------------------------------------*


*******************************************************************************************
* classes / implementacao
*******************************************************************************************
CLASS lcl_event DEFINITION .
  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.

ENDCLASS.

CLASS lcl_event_0102 DEFINITION .
  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.

CLASS lcl_event_0103 DEFINITION .
  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.


CLASS lcl_event_0103_1 DEFINITION .
  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.

CLASS lcl_event_0103_2 DEFINITION .
  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.

CLASS lcl_event_0103_3 DEFINITION .
  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.

*** Inicio - Rubenilson  - 19.06.24 - #150257
CLASS lcl_event_0105 DEFINITION .
  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.

CLASS lcl_event_0106 DEFINITION .
  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.
*** Fim - Rubenilson  - 19.06.24 - #150257

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.

    CLASS-METHODS:
      handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

    CLASS-METHODS:
      on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.

CLASS lcl_event_handler_0102 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.
    CLASS-METHODS:
      on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.

CLASS lcl_event_handler_0103 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.

    CLASS-METHODS:
      on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.


ENDCLASS.

CLASS lcl_event_handler_0103_1 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.

    CLASS-METHODS:
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.

CLASS lcl_event_handler_0103_2 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.


    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.

    CLASS-METHODS:
      on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.


ENDCLASS.

CLASS lcl_event_handler_0103_3 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.

    CLASS-METHODS:
      on_f4   FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.


ENDCLASS.

*** Inicio - Rubenilson  - 19.06.24 - #150257
CLASS lcl_event_handler_0105 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.
    CLASS-METHODS:
      on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.

CLASS lcl_event_handler_0106 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      handle_on_f1 FOR EVENT onf1 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.
    CLASS-METHODS:
      on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.

*** Fim - Rubenilson  - 19.06.24 - #150257
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

  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          l_ktokd  TYPE t077x-ktokd.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      READ TABLE it_grupo_contas_alv INTO DATA(lwa_grupo_contas_alv) INDEX ls_good-row_id.
      CASE ls_good-fieldname.
        WHEN 'KTOKD'.
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_ktokd  = lv_value.

          SELECT SINGLE ktokd
            INTO @DATA(lva_ktokd)
            FROM t077x
           WHERE ktokd = @l_ktokd
             AND spras = @sy-langu.

          IF sy-subrc <> 0.
*** Inicio - Rubenilson - 19.06.24 - #140377
            SELECT SINGLE ktokk
              INTO lva_ktokd
              FROM t077y
             WHERE ktokk = l_ktokd
               AND spras = sy-langu.
            IF sy-subrc IS NOT INITIAL.
*** Fim - Rubenilson - 19.06.24 - #140377
              MESSAGE s024(sd) WITH TEXT-102 DISPLAY LIKE 'E'.
              MOVE: TEXT-102    TO wa_msg_ret-msg,
              'KTOKD'           TO wa_msg_ret-field,
              ls_good-row_id    TO wa_msg_ret-tabix.
              APPEND wa_msg_ret TO it_msg_ret.
              CLEAR: wa_msg_ret.
            ENDIF.

          ELSE.

            "  DELETE it_msg_ret WHERE field = 'KTOKD' AND tabix = ls_good-row_id.

          ENDIF.

          "WHEN 'KTOKD'.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD on_data_changed_finished.
    PERFORM f_verifica_erros.
    PERFORM f_show_erro.
  ENDMETHOD.
  METHOD handle_on_f1.
  ENDMETHOD.
  METHOD on_f4.
    PERFORM on_f4 USING e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_handler_0102 IMPLEMENTATION.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed.

    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          vl_value   TYPE lvc_value,
          l_akont    TYPE zsdt0317-akont,
          l_zuawa    TYPE zsdt0317-zuawa,
          l_fdgrv    TYPE zsdt0317-fdgrv,
          l_zwels    TYPE zsdt0317-zwels,
          l_zterm    TYPE zsdt0317-zterm,
          l_zbukr    TYPE bukrs,
          l_char(50) TYPE c,
          it_t042e   TYPE TABLE OF t042e.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE it_grupo_contas_alv_aux INTO DATA(lwa_grupo_contas_alv) INDEX ls_good-row_id.
      READ TABLE it_grupo_contas_alv INTO DATA(lwa_grupo_contas_0101) WITH KEY id = lwa_grupo_contas_alv-id.
      DATA(lva_tabix) = sy-tabix.

      CASE ls_good-fieldname.
        WHEN 'AKONT'. "'Cta. Conciliação'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_akont  = lv_value.
          lwa_grupo_contas_alv-modif     = abap_true.
          MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.

          SELECT SINGLE saknr
            INTO @DATA(lva_akont)
            FROM skb1
           WHERE bukrs = @vg_bukrs
             AND saknr = @l_akont.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-105 DISPLAY LIKE 'E'.
            MOVE: TEXT-105    TO wa_msg_ret-msg,
            'AKONT'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.

        WHEN 'ZUAWA'." 'Chave de Ordenação'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_zuawa  = lv_value.
          lwa_grupo_contas_alv-modif     = abap_true.
          MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          SELECT SINGLE zuawa
            INTO @DATA(lva_zuawa)
            FROM  tzun
           WHERE zuawa = @l_zuawa.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-106 DISPLAY LIKE 'E'.
            MOVE: TEXT-106    TO wa_msg_ret-msg,
            'ZUAWA'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            " MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.
        WHEN 'FDGRV'. " 'Grp.Adm. Tesour.'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_fdgrv  = lv_value.
          lwa_grupo_contas_alv-modif     = abap_true.
          MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.

          SELECT SINGLE grupp
            INTO @DATA(lva_fdgrv)
            FROM  t035
           WHERE grupp = @l_fdgrv.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-107 DISPLAY LIKE 'E'.
            MOVE: TEXT-107    TO wa_msg_ret-msg,
            'FDGRV'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            "MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.

        WHEN 'ZWELS'. "'Cond. Pagamento'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_zwels  = lv_value.
          lwa_grupo_contas_alv-modif     = abap_true.

          CONDENSE l_zwels NO-GAPS.
          l_char = l_zwels.

          SELECT SINGLE zbukr FROM t042 INTO l_zbukr
            WHERE bukrs = vg_bukrs.

          SELECT * FROM t042e INTO TABLE it_t042e WHERE zbukr = l_zbukr.

          WHILE l_char NE space.

            SELECT * FROM t042z INTO TABLE @DATA(lit_t042z) WHERE land1 = 'BR'
                                                     AND   zlsch = @l_char(1).
            IF sy-subrc NE 0.

              CONCATENATE 'Forma de pagamento' l_char 'para o país BR não prevista' INTO wa_msg_ret-msg SEPARATED BY space.
              MESSAGE s024(sd) WITH wa_msg_ret-msg  DISPLAY LIKE 'E'.

              MOVE:
              'ZWELS'           TO wa_msg_ret-field.
              APPEND wa_msg_ret TO it_msg_ret.
              CLEAR: wa_msg_ret.


            ENDIF.

            READ TABLE it_t042e TRANSPORTING NO FIELDS WITH KEY zbukr  = l_zbukr
                                                                zlsch  = l_char(1).
            IF sy-subrc NE 0 AND lines( it_t042e ) NE 0.
              CONCATENATE 'Forma de pagamento' l_char 'para o país BR não prevista' INTO wa_msg_ret-msg SEPARATED BY space.
              MESSAGE s024(sd) WITH wa_msg_ret-msg  DISPLAY LIKE 'E'.

              MOVE:
                         'ZWELS'           TO wa_msg_ret-field.
              APPEND wa_msg_ret TO it_msg_ret.
              CLEAR: wa_msg_ret.

            ENDIF.
            SHIFT l_char.
          ENDWHILE.
          MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
        WHEN 'ZTERM'. "'Condição Pagamento'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_zterm  = lv_value.
          lwa_grupo_contas_alv-modif     = abap_true.
          MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.

          SELECT SINGLE zterm
            INTO @DATA(lva_zterm)
            FROM  t052
           WHERE zterm = @l_zterm.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-126 DISPLAY LIKE 'E'.
            MOVE: TEXT-126    TO wa_msg_ret-msg,
            'ZTERM'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
*            lwa_grupo_contas_alv-zterm = lva_zterm.
*            MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.
      ENDCASE.
      MODIFY it_grupo_contas_alv FROM lwa_grupo_contas_alv INDEX lva_tabix.
    ENDLOOP.


  ENDMETHOD.
  METHOD on_data_changed_finished.
    PERFORM f_verifica_erros.
    PERFORM f_show_erro.
  ENDMETHOD.
  METHOD handle_on_f1.
  ENDMETHOD.
  METHOD on_f4.
    PERFORM on_f4_0102 USING e_fieldname
                             e_fieldvalue
                             es_row_no
                             er_event_data
                             et_bad_cells
                             e_display.
  ENDMETHOD.
ENDCLASS.

*** Inicio - Rubenilson  - 19.06.24 - #150257
CLASS lcl_event_handler_0105 IMPLEMENTATION.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed.

    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          vl_value   TYPE lvc_value,
          l_akont    TYPE zsdt0317-akont,
          l_zuawa    TYPE zsdt0317-zuawa,
          l_fdgrv    TYPE zsdt0317-fdgrv,
          l_zwels    TYPE zsdt0317-zwels,
          l_zterm    TYPE zsdt0317-zterm,
          l_qland    TYPE zsdt0341-qland,
          l_zbukr    TYPE bukrs,
          l_char(50) TYPE c,
          it_t042e   TYPE TABLE OF t042e.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE it_empresa_fornec_alv_aux INTO DATA(lwa_empresa_fornec_alv) INDEX ls_good-row_id.
      READ TABLE it_empresa_fornec_alv     INTO DATA(lwa_empresa_fornec_0101) WITH KEY id = lwa_empresa_fornec_alv-id.
      DATA(lva_tabix) = sy-tabix.

      CASE ls_good-fieldname.
        WHEN 'AKONT'. "'Cta. Conciliação'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_akont  = lv_value.
          lwa_empresa_fornec_alv-modif = abap_true.
          lwa_empresa_fornec_alv-akont = l_akont.
          MODIFY it_empresa_fornec_alv_aux FROM lwa_empresa_fornec_alv INDEX ls_good-row_id.

          SELECT SINGLE saknr
            INTO @DATA(lva_akont)
            FROM skb1
           WHERE bukrs = @vg_bukrs
             AND saknr = @l_akont.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-105 DISPLAY LIKE 'E'.
            MOVE: TEXT-105    TO wa_msg_ret-msg,
            'AKONT'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.

        WHEN 'ZUAWA'." 'Chave de Ordenação'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_zuawa  = lv_value.
          lwa_empresa_fornec_alv-modif = abap_true.
          MODIFY it_empresa_fornec_alv_aux FROM lwa_empresa_fornec_alv INDEX ls_good-row_id.
          SELECT SINGLE zuawa
            INTO @DATA(lva_zuawa)
            FROM  tzun
           WHERE zuawa = @l_zuawa.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-106 DISPLAY LIKE 'E'.
            MOVE: TEXT-106    TO wa_msg_ret-msg,
            'ZUAWA'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            " MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.
        WHEN 'FDGRV'. " 'Grp.Adm. Tesour.'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_fdgrv  = lv_value.
          lwa_empresa_fornec_alv-modif     = abap_true.
          lwa_empresa_fornec_alv-fdgrv = l_fdgrv.
          MODIFY it_empresa_fornec_alv_aux FROM lwa_empresa_fornec_alv INDEX ls_good-row_id.

          SELECT SINGLE grupp
            INTO @DATA(lva_fdgrv)
            FROM  t035
           WHERE grupp = @l_fdgrv.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-107 DISPLAY LIKE 'E'.
            MOVE: TEXT-107    TO wa_msg_ret-msg,
            'FDGRV'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            "MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.

        WHEN 'ZWELS'. "'Cond. Pagamento'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_zwels  = lv_value.
          lwa_empresa_fornec_alv-modif     = abap_true.
          lwa_empresa_fornec_alv-zwels = l_zwels.
          CONDENSE l_zwels NO-GAPS.
          l_char = l_zwels.

          SELECT SINGLE zbukr FROM t042 INTO l_zbukr
            WHERE bukrs = vg_bukrs.

          SELECT * FROM t042e INTO TABLE it_t042e WHERE zbukr = l_zbukr.

          WHILE l_char NE space.

            SELECT * FROM t042z INTO TABLE @DATA(lit_t042z) WHERE land1 = 'BR'
                                                     AND   zlsch = @l_char(1).
            IF sy-subrc NE 0.

              CONCATENATE 'Forma de pagamento' l_char 'para o país BR não prevista' INTO wa_msg_ret-msg SEPARATED BY space.
              MESSAGE s024(sd) WITH wa_msg_ret-msg  DISPLAY LIKE 'E'.

              MOVE:
              'ZWELS'           TO wa_msg_ret-field.
              APPEND wa_msg_ret TO it_msg_ret.
              CLEAR: wa_msg_ret.


            ENDIF.

            READ TABLE it_t042e TRANSPORTING NO FIELDS WITH KEY zbukr  = l_zbukr
                                                                zlsch  = l_char(1).
            IF sy-subrc NE 0 AND lines( it_t042e ) NE 0.
              CONCATENATE 'Forma de pagamento' l_char 'para o país BR não prevista' INTO wa_msg_ret-msg SEPARATED BY space.
              MESSAGE s024(sd) WITH wa_msg_ret-msg  DISPLAY LIKE 'E'.

              MOVE:
                         'ZWELS'           TO wa_msg_ret-field.
              APPEND wa_msg_ret TO it_msg_ret.
              CLEAR: wa_msg_ret.

            ENDIF.
            SHIFT l_char.
          ENDWHILE.
          MODIFY it_empresa_fornec_alv_aux FROM lwa_empresa_fornec_alv INDEX ls_good-row_id.
        WHEN 'ZTERM'. "'Condição Pagamento'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_zterm  = lv_value.
          lwa_empresa_fornec_alv-modif     = abap_true.
          lwa_empresa_fornec_alv-zterm = l_zterm.
          MODIFY it_empresa_fornec_alv_aux FROM lwa_empresa_fornec_alv INDEX ls_good-row_id.

          SELECT SINGLE zterm
            INTO @DATA(lva_zterm)
            FROM  t052
           WHERE zterm = @l_zterm.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-126 DISPLAY LIKE 'E'.
            MOVE: TEXT-126    TO wa_msg_ret-msg,
            'ZTERM'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            " MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.

        WHEN 'QLAND'.
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_qland  = lv_value.
          lwa_empresa_fornec_alv-modif     = abap_true.
          lwa_empresa_fornec_alv-qland = l_qland.
          MODIFY it_empresa_fornec_alv_aux FROM lwa_empresa_fornec_alv INDEX ls_good-row_id.

          SELECT qland
            FROM t005r
            INTO TABLE @DATA(lt_qland)
          WHERE spras = @sy-langu
            AND qland = @l_qland.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE s024(sd) WITH TEXT-129 DISPLAY LIKE 'E'.
            MOVE: TEXT-129    TO wa_msg_ret-msg,
            'QLAND'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.

      ENDCASE.


      MODIFY it_empresa_fornec_alv FROM lwa_empresa_fornec_alv INDEX lva_tabix.
    ENDLOOP.


  ENDMETHOD.
  METHOD on_data_changed_finished.
    PERFORM f_verifica_erros.
    PERFORM f_show_erro.
  ENDMETHOD.
  METHOD handle_on_f1.
  ENDMETHOD.
  METHOD on_f4.
    PERFORM on_f4_0105 USING e_fieldname
                             e_fieldvalue
                             es_row_no
                             er_event_data
                             et_bad_cells
                             e_display.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler_0106 IMPLEMENTATION.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed.

    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          vl_value   TYPE lvc_value,
          l_ekorg    TYPE lfm1-ekorg,
          l_waers    TYPE lfm1-waers,
          l_fdgrv    TYPE zsdt0317-fdgrv,
          l_zwels    TYPE zsdt0317-zwels,
          l_zterm    TYPE zsdt0317-zterm,
          l_inco1    TYPE lfm1-inco1,
          l_zbukr    TYPE bukrs,
          l_char(50) TYPE c,
          it_t042e   TYPE TABLE OF t042e,
          l_vsbed    TYPE lfm1-vsbed.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE it_dados_compra_alv_aux INTO DATA(lwa_dados_compra_alv) INDEX ls_good-row_id.
*      READ TABLE it_dados_compra_alv     INTO DATA(lwa_dados_compra_0101) WITH KEY id = lwa_dados_compra_alv-id.
      DATA(lva_tabix) = sy-tabix.

      CASE ls_good-fieldname.
        WHEN 'EKORG'. "'Cta. Conciliação'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_ekorg  = lv_value.
          lwa_dados_compra_alv-modif     = abap_true.
          lwa_dados_compra_alv-ekorg = l_ekorg.
          MODIFY it_dados_compra_alv_aux FROM lwa_dados_compra_alv INDEX ls_good-row_id.

          SELECT SINGLE ekorg
            INTO @DATA(lva_ekorg)
            FROM t024e
           WHERE ekorg = @l_ekorg.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-130 DISPLAY LIKE 'E'.
            MOVE: TEXT-105    TO wa_msg_ret-msg,
            'EKORG'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.

        WHEN 'WAERS'." 'Chave de Ordenação'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_waers  = lv_value.
          lwa_dados_compra_alv-modif = abap_true.
          lwa_dados_compra_alv-waers = l_waers.
          MODIFY it_dados_compra_alv_aux FROM lwa_dados_compra_alv INDEX ls_good-row_id.

          SELECT SINGLE waers
            INTO @DATA(lva_waers)
            FROM  tcurc
           WHERE waers = @l_waers.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-131 DISPLAY LIKE 'E'.
            MOVE: TEXT-106    TO wa_msg_ret-msg,
            'WAERS'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            " MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.

        WHEN 'ZTERM'. "'Condição Pagamento'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_zterm  = lv_value.
          lwa_dados_compra_alv-modif     = abap_true.
          MODIFY it_dados_compra_alv_aux FROM lwa_dados_compra_alv INDEX ls_good-row_id.

          SELECT SINGLE zterm
            INTO @DATA(lva_zterm)
            FROM  t052
           WHERE zterm = @l_zterm.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-126 DISPLAY LIKE 'E'.
            MOVE: TEXT-126    TO wa_msg_ret-msg,
            'ZTERM'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            " MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.

        WHEN 'INCO1'." 'Chave de Ordenação'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_inco1  = lv_value.
          lwa_dados_compra_alv-modif = abap_true.
          lwa_dados_compra_alv-inco1 = l_waers.
          MODIFY it_dados_compra_alv_aux FROM lwa_dados_compra_alv INDEX ls_good-row_id.

          SELECT SINGLE inco1
            INTO @DATA(lva_inco1)
            FROM  tinc
           WHERE inco1 = @l_inco1.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-132 DISPLAY LIKE 'E'.
            MOVE: TEXT-106    TO wa_msg_ret-msg,
            'INCO1'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            " MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.

        WHEN 'VSBED'." 'Chave de Ordenação'
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_vsbed  = lv_value.
          lwa_dados_compra_alv-modif = abap_true.
          lwa_dados_compra_alv-vsbed = l_vsbed.
          MODIFY it_dados_compra_alv_aux FROM lwa_dados_compra_alv INDEX ls_good-row_id.

          SELECT SINGLE vsbed
            INTO @DATA(lva_vsbed)
            FROM  tvsb
           WHERE vsbed = @l_vsbed.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-133 DISPLAY LIKE 'E'.
            MOVE: TEXT-106    TO wa_msg_ret-msg,
            'VSBED'           TO wa_msg_ret-field,
            ls_good-row_id    TO wa_msg_ret-tabix.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            " MODIFY it_grupo_contas_alv_aux FROM lwa_grupo_contas_alv INDEX ls_good-row_id.
          ENDIF.

      ENDCASE.
      MODIFY it_dados_compra_alv FROM lwa_dados_compra_alv INDEX lva_tabix.
    ENDLOOP.


  ENDMETHOD.
  METHOD on_data_changed_finished.
    PERFORM f_verifica_erros.
    PERFORM f_show_erro.
  ENDMETHOD.
  METHOD handle_on_f1.
  ENDMETHOD.
  METHOD on_f4.
    PERFORM on_f4_0106 USING e_fieldname
                             e_fieldvalue
                             es_row_no
                             er_event_data
                             et_bad_cells
                             e_display.
  ENDMETHOD.
ENDCLASS.
*** Fim - Rubenilson  - 19.06.24 - #150257

CLASS lcl_event_handler_0103 IMPLEMENTATION.
  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          l_vtweg  TYPE  tvtwt-vtweg.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      READ TABLE it_canal_dist_alv INTO DATA(lwa_canal_dist_alv) INDEX ls_good-row_id.
      CASE ls_good-fieldname.
        WHEN 'VTWEG'. "Canal de Distribuição
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_vtweg = lv_value.

          SELECT SINGLE vtext
            FROM tvtwt
            INTO @DATA(l_vtext)
             WHERE vtweg EQ @l_vtweg
               AND spras EQ @sy-langu.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-103 DISPLAY LIKE 'E'.
          ELSE.
            lv_value = l_vtext.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'VTEXT'
                i_value     = lv_value.

            lwa_canal_dist_alv-vtweg   = l_vtweg.
            lwa_canal_dist_alv-vtext   = l_vtext.

            MODIFY it_canal_dist_alv FROM lwa_canal_dist_alv INDEX ls_good-row_id TRANSPORTING vtweg vtext .
          ENDIF.
      ENDCASE.

      READ TABLE it_canal_dist_alv_aux INTO DATA(lwa_dist_alv_aux) WITH KEY id        = lwa_canal_dist_alv-id
                                                                            seq_canal = lwa_canal_dist_alv-seq_canal.
      IF sy-subrc = 0.
        MODIFY  it_canal_dist_alv_aux  FROM lwa_canal_dist_alv INDEX sy-tabix TRANSPORTING vtweg vtext .
      ELSE.
        APPEND lwa_canal_dist_alv TO  it_canal_dist_alv_aux.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD on_data_changed_finished.
    PERFORM f_verifica_erros.
    PERFORM f_show_erro.
  ENDMETHOD.

  METHOD handle_on_f1.
  ENDMETHOD.

  METHOD on_f4.
    PERFORM on_f4_0103 USING e_fieldname
                             e_fieldvalue
                             es_row_no
                             er_event_data
                             et_bad_cells
                              e_display.
  ENDMETHOD.


ENDCLASS.

CLASS lcl_event_handler_0103_1 IMPLEMENTATION.
  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          l_bukrs  TYPE t001-bukrs,
          l_ktokd  TYPE t077x-ktokd,
          l_spart  TYPE tspa-spart.


    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE it_set_atv_alv INTO DATA(lwa_set_atv_alv) INDEX ls_good-row_id.

      CASE ls_good-fieldname.
        WHEN 'SPART' .
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_spart  = lv_value.

          SELECT SINGLE vtext
            FROM tspat
            INTO @DATA(l_vtext)
             WHERE spart = @l_spart
             AND spras EQ @sy-langu.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-109 DISPLAY LIKE 'E'.
            MOVE: TEXT-109    TO wa_msg_ret-msg,
            'SPART'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ELSE.
            lv_value = l_vtext.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'VTEXT'
                i_value     = lv_value.

            lwa_set_atv_alv-spart     = l_spart.
            lwa_set_atv_alv-vtext     = l_vtext.
            MODIFY it_set_atv_alv FROM lwa_set_atv_alv INDEX ls_good-row_id TRANSPORTING spart vtext.
          ENDIF.
      ENDCASE.
      READ TABLE it_set_atv_alv_aux INTO DATA(lwa_atv_alv_aux) WITH KEY id        = lwa_set_atv_alv-id
                                                                        seq_canal = lwa_set_atv_alv-seq_canal
                                                                        seq_setor = lwa_set_atv_alv-seq_setor.
      IF sy-subrc = 0.
        MODIFY it_set_atv_alv_aux  FROM lwa_set_atv_alv INDEX sy-tabix TRANSPORTING spart vtext .
      ELSE.
        APPEND lwa_set_atv_alv TO it_set_atv_alv_aux.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD on_data_changed_finished.
    PERFORM f_verifica_erros.
    PERFORM f_show_erro.
  ENDMETHOD.
  METHOD handle_on_f1.
  ENDMETHOD.
  METHOD on_f4.
    PERFORM on_f4_0103_1 USING  e_fieldname
                                e_fieldvalue
                                es_row_no
                                er_event_data
                                et_bad_cells
                                e_display.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler_0103_2 IMPLEMENTATION.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          l_bukrs  TYPE t001-bukrs,
          l_ktokd  TYPE t077x-ktokd,
          l_parvw  TYPE tpar-parvw.


*    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
*
*      READ TABLE it_parceiros_alv INTO DATA(lwa_parceiros_alv) INDEX ls_good-row_id.
*
*      CASE ls_good-fieldname.
*        WHEN 'PARVW' .
*          lv_value = ls_good-value.
*          CONDENSE lv_value NO-GAPS.
*          l_parvw  = lv_value.
*
*          SELECT SINGLE vtext
*            INTO @DATA(l_vtext)
*            FROM tpart
*           WHERE parvw EQ @l_parvw
*            AND spras EQ @sy-langu.
*
*          IF sy-subrc <> 0.
*            MESSAGE s024(sd) WITH text-110 DISPLAY LIKE 'E'.
*
*            MOVE: text-110    TO wa_msg_ret-msg,
*            'PARVW'           TO wa_msg_ret-field.
*            APPEND wa_msg_ret TO it_msg_ret.
*            CLEAR: wa_msg_ret.
*          ELSE.
*            lv_value = l_vtext.
*            CALL METHOD er_data_changed->modify_cell
*              EXPORTING
*                i_row_id    = ls_good-row_id
*                i_fieldname = 'VTEXT'
*                i_value     = lv_value.
*
*            lwa_parceiros_alv-vtext     = l_vtext.
*
*            MODIFY it_parceiros_alv FROM lwa_parceiros_alv INDEX ls_good-row_id.
*          ENDIF.
*
*      ENDCASE.
*    ENDLOOP.
  ENDMETHOD.
  METHOD on_data_changed_finished.
    PERFORM f_verifica_erros.
    PERFORM f_show_erro.
  ENDMETHOD.
  METHOD handle_on_f1.
  ENDMETHOD.
  METHOD on_f4.
    PERFORM on_f4_0103_2 USING e_fieldname
                               e_fieldvalue
                               es_row_no
                               er_event_data
                              et_bad_cells
                               e_display.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_event_handler_0103_3 IMPLEMENTATION.
  METHOD on_double_click.
  ENDMETHOD.
  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          l_waers  TYPE tcurc-waers,
          l_versg  TYPE knvv-versg,
          l_kalks  TYPE knvv-kalks,
          l_lprio  TYPE tprio-lprio,
          l_vsbed  TYPE tvsb-vsbed,
          l_perfk  TYPE tfact-ident,
          l_ktgrd  TYPE tvkt-ktgrd,
          l_tatyp  TYPE  t685a-kschl,
          l_taxkd  TYPE tskd-taxkd,
          l_kztlf  TYPE kztlf.

    "LOOP AT er_data_changed->mt_good_cells INTO ls_good.
    LOOP AT er_data_changed->mt_mod_cells INTO ls_good.

      READ TABLE it_area_venda_alv INTO DATA(lwa_area_venda_alv) INDEX ls_good-row_id.

      CASE ls_good-fieldname.
        WHEN  'WAERS'.
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_waers  = lv_value.
          lwa_area_venda_alv-modif     = abap_true.
          lwa_area_venda_alv-waers     = l_waers.

          SELECT SINGLE waers
            INTO @DATA(lva_waers)
            FROM tcurc
           WHERE waers = @l_waers..

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-111 DISPLAY LIKE 'E'.
            MOVE: TEXT-111   TO wa_msg_ret-msg,
            'WAERS'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.
          MODIFY it_area_venda_alv FROM lwa_area_venda_alv  INDEX ls_good-row_id TRANSPORTING waers modif.

        WHEN 'VERSG'.
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_versg  = lv_value.
          lwa_area_venda_alv-modif     = abap_true.
          lwa_area_venda_alv-versg     = l_versg.

          SELECT SINGLE stgku
           INTO @DATA(lva_versg)
            FROM  tvsdt
              WHERE stgku = @l_versg
               AND spras = @sy-langu.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-127 DISPLAY LIKE 'E'.
            MOVE: TEXT-127    TO wa_msg_ret-msg,
            'VERSG'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.

          MODIFY it_area_venda_alv FROM lwa_area_venda_alv  INDEX ls_good-row_id TRANSPORTING versg modif.

        WHEN 'KALKS'.
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_kalks  = lv_value.
          lwa_area_venda_alv-modif     = abap_true.
          lwa_area_venda_alv-kalks     = l_kalks.

          SELECT SINGLE kalks
            INTO @DATA(lva_kalks)
          FROM  tvkdt
            WHERE kalks = @l_kalks
              AND spras = @sy-langu.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-128 DISPLAY LIKE 'E'.
            MOVE: TEXT-128    TO wa_msg_ret-msg,
            'KALKS'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.


          MODIFY it_area_venda_alv FROM lwa_area_venda_alv  INDEX ls_good-row_id TRANSPORTING kalks modif.

        WHEN   'LPRIO' .
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_lprio  = lv_value.
          lwa_area_venda_alv-modif  = abap_true.
          lwa_area_venda_alv-lprio  =  l_lprio.

          SELECT SINGLE lprio
            INTO @DATA(lva_lprio)
            FROM tprio
           WHERE lprio = @l_lprio.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-112 DISPLAY LIKE 'E'.
            MOVE: TEXT-112    TO wa_msg_ret-msg,
            'LPRIO'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.
          MODIFY it_area_venda_alv FROM lwa_area_venda_alv  INDEX ls_good-row_id TRANSPORTING lprio modif.

        WHEN   'VSBED' .
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_vsbed  = lv_value.
          lwa_area_venda_alv-modif = abap_true.
          lwa_area_venda_alv-vsbed = l_vsbed.

          SELECT SINGLE vsbed
            INTO @DATA(lva_vsbed)
            FROM tvsb
           WHERE vsbed = @l_vsbed.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-113 DISPLAY LIKE 'E'.
            MOVE: TEXT-113    TO wa_msg_ret-msg,
            'VSBED'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.
          MODIFY it_area_venda_alv FROM lwa_area_venda_alv  INDEX ls_good-row_id TRANSPORTING vsbed modif.

        WHEN   'KZAZU' . "FLAG VErificar
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          lwa_area_venda_alv-modif     = abap_true.
          MODIFY it_area_venda_alv FROM lwa_area_venda_alv  INDEX ls_good-row_id.

        WHEN   'KZTLF' . "Elemento de dados
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_kztlf  = lv_value.
          lwa_area_venda_alv-modif     = abap_true.
          lwa_area_venda_alv-kztlf = l_kztlf.

          DATA: it_dd07v TYPE STANDARD TABLE OF dd07v.
          CALL FUNCTION 'GET_DOMAIN_VALUES'
            EXPORTING
              domname         = 'KZTLF'
            TABLES
              values_tab      = it_dd07v
            EXCEPTIONS
              no_values_found = 1
              OTHERS          = 2.
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.

          READ TABLE it_dd07v INTO DATA(lwa_dd07v) WITH KEY domvalue_l = l_kztlf.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-125 DISPLAY LIKE 'E'.
            MOVE: TEXT-125    TO wa_msg_ret-msg,
            'KZTLF'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.
          MODIFY it_area_venda_alv FROM lwa_area_venda_alv  INDEX ls_good-row_id TRANSPORTING kztlf modif.
        WHEN   'PERFK' .
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_perfk  = lv_value.
          lwa_area_venda_alv-modif     = abap_true.
          lwa_area_venda_alv-perfk     = l_perfk.
          SELECT SINGLE ident
            INTO @DATA(lva_perfk)
            FROM tfact
           WHERE ident = @l_perfk
             AND spras = @sy-langu.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-123 DISPLAY LIKE 'E'.
            MOVE: TEXT-123    TO wa_msg_ret-msg,
            'PERFK'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.
          MODIFY it_area_venda_alv FROM lwa_area_venda_alv INDEX ls_good-row_id TRANSPORTING perfk modif.
        WHEN   'KTGRD' .
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_ktgrd  = lv_value.
          lwa_area_venda_alv-modif     = abap_true.
          lwa_area_venda_alv-ktgrd     = l_ktgrd.

          SELECT SINGLE ktgrd
            INTO @DATA(lva_ktgrd)
            FROM  tvkt
           WHERE ktgrd = @l_ktgrd.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-114 DISPLAY LIKE 'E'.
            MOVE: TEXT-114   TO wa_msg_ret-msg,
            'KTGRD'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.
          MODIFY it_area_venda_alv FROM lwa_area_venda_alv INDEX ls_good-row_id TRANSPORTING ktgrd modif.

        WHEN   'TATYP' .
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_tatyp  = lv_value.
          lwa_area_venda_alv-modif     = abap_true.
          lwa_area_venda_alv-tatyp     = l_tatyp.

          SELECT SINGLE kschl
            INTO @DATA(lva_tatyp)
              FROM t685a
          WHERE kappl = 'V'
            AND kschl = @l_tatyp .

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-124 DISPLAY LIKE 'E'.
            MOVE: TEXT-124    TO wa_msg_ret-msg,
            'TATYP'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.
          MODIFY it_area_venda_alv FROM lwa_area_venda_alv  INDEX ls_good-row_id TRANSPORTING tatyp modif.

        WHEN   'TAXKD' .
          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.
          l_taxkd  = lv_value.
          lwa_area_venda_alv-modif     = abap_true.
          lwa_area_venda_alv-taxkd     = l_taxkd.

          SELECT SINGLE taxkd
            INTO @DATA(lva_taxkd)
            FROM  tskd
           WHERE taxkd = @l_taxkd.

          IF sy-subrc <> 0.
            MESSAGE s024(sd) WITH TEXT-115 DISPLAY LIKE 'E'.
            MOVE: TEXT-115    TO wa_msg_ret-msg,
            'TAXKD'           TO wa_msg_ret-field.
            APPEND wa_msg_ret TO it_msg_ret.
            CLEAR: wa_msg_ret.
          ENDIF.
          MODIFY it_area_venda_alv FROM lwa_area_venda_alv INDEX ls_good-row_id TRANSPORTING taxkd.

      ENDCASE.
      READ TABLE it_area_venda_alv_aux INTO DATA(lwa_venda_alv_aux) WITH KEY id             = lwa_area_venda_alv-id
                                                                         seq_canal      = lwa_area_venda_alv-seq_canal
                                                                         seq_area_venda = lwa_area_venda_alv-seq_area_venda.
      IF sy-subrc = 0.
        MODIFY it_area_venda_alv_aux FROM lwa_area_venda_alv INDEX sy-tabix  .
      ELSE.
        APPEND lwa_area_venda_alv  TO it_area_venda_alv_aux.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD on_data_changed_finished.
    PERFORM f_verifica_erros.
    PERFORM f_show_erro.
  ENDMETHOD.
  METHOD handle_on_f1.
  ENDMETHOD.
  METHOD on_f4.
    PERFORM on_f4_0103_3 USING e_fieldname
                               e_fieldvalue
                               es_row_no
                               er_event_data
                               et_bad_cells
                               e_display.
  ENDMETHOD.
ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event IMPLEMENTATION.
  METHOD toolbar.
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

    DATA: lva_id TYPE zsdt0317-id.
    CASE e_ucomm.
      WHEN 'INSERT'.

        SORT it_grupo_contas_alv BY id.

        SELECT  MAX( id )
           INTO lva_id
           FROM zsdt0317.

        SORT it_grupo_contas_alv BY id  DESCENDING.

        READ TABLE it_grupo_contas_alv INTO DATA(lwa_seq_c) INDEX 1.

        IF lwa_seq_c-id > lva_id.
          lva_id = lwa_seq_c-id + 1.
        ELSE.
          lva_id = lva_id + 1.
        ENDIF.

        INSERT INITIAL LINE INTO it_grupo_contas_alv INDEX 1.
*** Inicio - Rubenilson  - 19.06.24 - #150257
        INSERT INITIAL LINE INTO it_empresa_fornec_alv INDEX 1.
        INSERT INITIAL LINE INTO it_dados_compra_alv INDEX 1.
*** Fim - Rubenilson  - 19.06.24 - #150257

        CLEAR wa_grupo_contas_alv.
        wa_grupo_contas_alv-id        = lva_id.
        wa_grupo_contas_alv-bukrs     = vg_bukrs.
        wa_grupo_contas_alv-criado    = abap_true.
*** Inicio - Rubenilson - 19.06.24 - #140377
        MOVE-CORRESPONDING wa_grupo_contas_alv TO wa_empresa_fornec_alv.
        MOVE-CORRESPONDING wa_grupo_contas_alv TO wa_dados_compra_alv.
*** Fim - Rubenilson - 19.06.24 - #140377

        FREE wa_grupo_contas_alv-celltab.
        t_estilo =  VALUE #( ( fieldname = 'KTOKD' style = cl_gui_alv_grid=>mc_style_enabled ) ).
        INSERT LINES OF t_estilo INTO TABLE wa_grupo_contas_alv-celltab.

        MODIFY it_grupo_contas_alv   FROM wa_grupo_contas_alv   INDEX 1.
*** Inicio - Rubenilson  - 19.06.24 - #150257
        MODIFY it_empresa_fornec_alv FROM wa_empresa_fornec_alv INDEX 1.
        MODIFY it_dados_compra_alv   FROM wa_dados_compra_alv   INDEX 1.
*** Fim - Rubenilson  - 19.06.24 - #150257
      WHEN 'DELETE'.
        FREE: t_del_rows.

        CALL METHOD g_grid_grupo_c->check_changed_data.
        CALL METHOD g_grid_grupo_c->get_selected_rows
          IMPORTING
            et_index_rows = t_del_rows.

        IF t_del_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        SORT t_del_rows BY index DESCENDING.

        IF t_del_rows[] IS NOT INITIAL.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Confirmação'
              text_question         = 'Deseja eliminar os dados selecionados?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              default_button        = '1'
              display_cancel_button = ''
            IMPORTING
              answer                = var_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          IF var_answer EQ '1'.

            CLEAR: it_grupo_contas_del[],
                   it_canal_dist_del[].

            LOOP AT t_del_rows INTO w_del_rows.
              READ TABLE it_grupo_contas_alv  INTO wa_grupo_contas_alv INDEX w_del_rows-index.

              IF sy-subrc = 0.

                vg_usuario_cancel = sy-uname.
                vg_data_cancel = sy-datum.
                vg_hora_cancel = sy-uzeit.

                MOVE-CORRESPONDING wa_grupo_contas_alv TO wa_grupo_contas.

                SELECT SINGLE id
                   INTO lva_id
                   FROM zsdt0317
                  WHERE id = wa_grupo_contas_alv-id.

                IF sy-subrc = 0.
                  wa_grupo_contas-cancelado = 'X'.
                  wa_grupo_contas-usuario_cancel = vg_usuario_cancel.
                  wa_grupo_contas-data_cancel    =  vg_data_cancel.
                  wa_grupo_contas-hora_cancel    =  vg_hora_cancel.

                  MODIFY zsdt0317 FROM  wa_grupo_contas.
                  COMMIT WORK.
                  DELETE it_grupo_contas WHERE id = wa_grupo_contas_alv-id.
                ENDIF.

                DELETE it_grupo_contas_alv INDEX w_del_rows-index.
                CLEAR: wa_grupo_contas.
*** Inicio - Rubenilson  - 19.06.24 - #150257
                SELECT SINGLE *
                   INTO wa_empresa_fornec
                   FROM zsdt0341
                  WHERE id = wa_grupo_contas_alv-id.

                IF sy-subrc = 0.
                  wa_empresa_fornec-cancelado = 'X'.
                  wa_empresa_fornec-usuario_cancel =  vg_usuario_cancel.
                  wa_empresa_fornec-data_cancel    =  vg_data_cancel.
                  wa_empresa_fornec-hora_cancel    =  vg_hora_cancel.

                  MODIFY zsdt0341 FROM  wa_empresa_fornec.
                  COMMIT WORK.
                  DELETE it_empresa_fornec WHERE id = wa_grupo_contas_alv-id.
                  DELETE it_empresa_fornec_alv_aux WHERE id = wa_grupo_contas_alv-id.
                ENDIF.

                DELETE it_empresa_fornec_alv INDEX w_del_rows-index.
                CLEAR: wa_empresa_fornec.

*** Fim - Rubenilson  - 19.06.24 - #150257

                SELECT *
                   FROM zsdt0319
                  INTO TABLE @DATA(it_canal_dist_del)
                   WHERE id = @wa_grupo_contas_alv-id
                     AND cancelado <> 'X'.

                IF sy-subrc = 0.

                  LOOP AT it_canal_dist_del INTO DATA(wa_canal_dist_del).

                    CLEAR: wa_canal_dist.
                    MOVE-CORRESPONDING  wa_canal_dist_del TO  wa_canal_dist.

                    wa_canal_dist-cancelado = 'X'.
                    wa_canal_dist-usuario_cancel =  vg_usuario_cancel.
                    wa_canal_dist-data_cancel    =  vg_data_cancel.
                    wa_canal_dist-hora_cancel    =  vg_hora_cancel.


                    MODIFY zsdt0319 FROM  wa_canal_dist.
                    COMMIT WORK.

                  ENDLOOP.

                  DELETE  it_canal_dist_alv WHERE id = wa_canal_dist_alv-id.
                  DELETE  it_canal_dist_alv_aux WHERE id = wa_canal_dist_alv-id.
                  DELETE  it_canal_dist WHERE id = wa_canal_dist_alv-id.

                ENDIF.

                SELECT *
                   FROM zsdt0320
                INTO TABLE @DATA(it_set_atv_del)
                 WHERE id = @wa_grupo_contas_alv-id
                   AND cancelado <> 'X'.

                IF sy-subrc = 0.
                  LOOP AT it_set_atv_del INTO DATA(wa_set_atv_del).

                    CLEAR: wa_canal_dist.
                    MOVE-CORRESPONDING  wa_set_atv_del TO  wa_set_atv.

                    wa_set_atv-cancelado = 'X'.
                    wa_set_atv-usuario_cancel = vg_usuario_cancel.
                    wa_set_atv-data_cancel    =  vg_data_cancel.
                    wa_set_atv-hora_cancel    =  vg_hora_cancel.

                    MODIFY zsdt0320 FROM wa_set_atv.
                    COMMIT WORK.

                  ENDLOOP.

                  DELETE  it_set_atv_alv WHERE id = wa_canal_dist_alv-id.
                  DELETE  it_set_atv_alv_aux WHERE id = wa_canal_dist_alv-id.
                  DELETE  it_set_atv WHERE id = wa_canal_dist_alv-id.

                ENDIF.

                SELECT *
                   FROM zsdt0322
                INTO TABLE @DATA(it_area_venda_del)
                 WHERE id = @wa_grupo_contas_alv-id
                   AND cancelado <> 'X'.

                IF sy-subrc = 0.
                  LOOP AT it_area_venda_del INTO DATA(wa_area_venda_del).

                    CLEAR: wa_area_venda.
                    MOVE-CORRESPONDING wa_area_venda_del TO wa_area_venda.


                    wa_area_venda-cancelado = 'X'.
                    wa_area_venda-usuario_cancel = vg_usuario_cancel.
                    wa_area_venda-data_cancel   =  vg_data_cancel.
                    wa_area_venda-hora_cancel   =  vg_hora_cancel.

                    MODIFY zsdt0322 FROM wa_area_venda.
                    COMMIT WORK.

                  ENDLOOP.

                  DELETE  it_area_venda_alv     WHERE id = wa_canal_dist_alv-id.
                  DELETE  it_area_venda_alv_aux WHERE id = wa_canal_dist_alv-id.
                  DELETE  it_area_venda         WHERE id = wa_canal_dist_alv-id.

                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
    ENDCASE.
*
    CALL METHOD g_grid_grupo_c->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.                    "USER_COMMAND

  METHOD on_double_click.
    IF e_row-index IS NOT INITIAL  AND it_msg_ret[] IS  INITIAL.
      CLEAR: it_grupo_contas_alv_aux[].
      READ TABLE it_grupo_contas_alv INDEX e_row-index INTO wa_grupo_contas_alv.

      IF wa_grupo_contas_alv-criado <> 'X'.
        FREE wa_grupo_contas_alv-celltab.
        t_estilo =  VALUE #( ( fieldname =  'KTOKD'  style = cl_gui_alv_grid=>mc_style_disabled ) ).
      ENDIF.
      APPEND wa_grupo_contas_alv TO it_grupo_contas_alv_aux.
      PERFORM troca_aba_0102 USING space.
      LEAVE TO SCREEN 0100.
    ELSE.
      PERFORM f_show_erro.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_0102 IMPLEMENTATION.
  METHOD toolbar.
  ENDMETHOD.             "DISPLAY

  METHOD user_command.
  ENDMETHOD.                    "USER_COMMAND

  METHOD on_double_click.
    IF e_row-index IS NOT INITIAL AND it_msg_ret[] IS  INITIAL.
      READ TABLE it_grupo_contas_alv_aux INDEX e_row-index INTO wa_grupo_contas_alv.
      PERFORM troca_aba_0103 USING space.
      LEAVE TO SCREEN 0100.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*** Inicio - Rubenilson  - 19.06.24 - #150257
CLASS lcl_event_0105 IMPLEMENTATION.
  METHOD toolbar.
  ENDMETHOD.             "DISPLAY

  METHOD user_command.
  ENDMETHOD.                    "USER_COMMAND

  METHOD on_double_click.
    IF e_row-index IS NOT INITIAL AND it_msg_ret[] IS  INITIAL.
      READ TABLE it_grupo_contas_alv_aux INDEX e_row-index INTO wa_grupo_contas_alv.
      PERFORM troca_aba_0103 USING space.
      LEAVE TO SCREEN 0100.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_0106 IMPLEMENTATION.
  METHOD toolbar.
  ENDMETHOD.             "DISPLAY

  METHOD user_command.
  ENDMETHOD.                    "USER_COMMAND

  METHOD on_double_click.
*    IF e_row-index IS NOT INITIAL AND it_msg_ret[] IS  INITIAL.
*      READ TABLE it_grupo_contas_alv_aux INDEX e_row-index INTO wa_grupo_contas_alv.
*      PERFORM troca_aba_0103 USING space.
*      LEAVE TO SCREEN 0100.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.

*** Fim - Rubenilson  - 19.06.24 - #150257

CLASS lcl_event_0103 IMPLEMENTATION.
  METHOD toolbar.
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

  METHOD user_command.
    DATA:  lva_seq_canal TYPE zsdt0319-seq_canal.
    CASE e_ucomm.

      WHEN 'INSERT'.
        CLEAR: wa_canal_dist_alv.

        SELECT  MAX( seq_canal )
           INTO lva_seq_canal
           FROM zsdt0319
          WHERE id = wa_grupo_contas_alv-id.

        SORT it_canal_dist_alv BY id seq_canal DESCENDING.

        READ TABLE it_canal_dist_alv INTO DATA(lwa_seq_c) WITH KEY  id = wa_grupo_contas_alv-id.

        IF lwa_seq_c-seq_canal > lva_seq_canal.
          lva_seq_canal = lwa_seq_c-seq_canal + 1.
        ELSE.
          lva_seq_canal = lva_seq_canal + 1.
        ENDIF.


        INSERT INITIAL LINE INTO it_canal_dist_alv INDEX 1.

        wa_canal_dist_alv-id = wa_grupo_contas_alv-id.
        wa_canal_dist_alv-seq_canal = lva_seq_canal.
        wa_canal_dist_alv-criado    = abap_true.

        FREE wa_canal_dist_alv-celltab.
        t_estilo =  VALUE #( ( fieldname = 'VTWEG' style = cl_gui_alv_grid=>mc_style_enabled ) ).
        "( fieldname = 'VTEXT' style = cl_gui_alv_grid=>mc_style_disabled ) ).

        INSERT LINES OF t_estilo INTO TABLE wa_canal_dist_alv-celltab.

        MODIFY it_canal_dist_alv FROM wa_canal_dist_alv INDEX 1.

      WHEN 'DELETE'.
        FREE: t_del_rows.

        CALL METHOD g_grid_distrib->check_changed_data.
        CALL METHOD g_grid_distrib->get_selected_rows
          IMPORTING
            et_index_rows = t_del_rows.

        IF t_del_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        SORT t_del_rows BY index DESCENDING.

        IF t_del_rows[] IS NOT INITIAL.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Confirmação'
              text_question         = 'Deseja eliminar os dados selecionados?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              default_button        = '1'
              display_cancel_button = ''
            IMPORTING
              answer                = var_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          IF var_answer EQ '1'.

            LOOP AT t_del_rows INTO w_del_rows.
              CLEAR: wa_canal_dist_alv, wa_set_atv_alv, wa_area_venda_alv.
              READ TABLE it_canal_dist_alv  INTO wa_canal_dist_alv INDEX w_del_rows-index.

              vg_usuario_cancel = sy-uname.
              vg_data_cancel = sy-datum.
              vg_hora_cancel = sy-uzeit.

              CLEAR: wa_canal_dist.
              MOVE-CORRESPONDING wa_canal_dist_alv TO wa_canal_dist.

              SELECT SINGLE seq_canal
               INTO lva_seq_canal
               FROM zsdt0319
              WHERE id             = wa_canal_dist_alv-id
                AND seq_canal      = wa_canal_dist_alv-seq_canal.

              IF sy-subrc = 0.
                wa_canal_dist-cancelado = 'X'.
                wa_canal_dist-usuario_cancel =  vg_usuario_cancel.
                wa_canal_dist-data_cancel    =  vg_data_cancel.
                wa_canal_dist-hora_cancel    =  vg_hora_cancel.

                MODIFY zsdt0319 FROM  wa_canal_dist.

                DELETE it_canal_dist WHERE id = wa_canal_dist_alv-id
                                       AND seq_canal = wa_canal_dist_alv-seq_canal.
              ENDIF.

              DELETE  it_canal_dist_alv INDEX w_del_rows-index.

              IF wa_canal_dist_alv-criado = 'X'.
                DELETE  it_canal_dist_alv_aux WHERE id = wa_canal_dist_alv-id
                                                AND seq_canal = wa_canal_dist_alv-seq_canal.
              ENDIF.

              SELECT *
                  FROM zsdt0320
                 INTO TABLE @DATA(it_set_atv_del)
                  WHERE id = @wa_canal_dist_alv-id
                     AND seq_canal = @wa_canal_dist_alv-seq_canal
                     AND cancelado <> 'X'.

              IF sy-subrc = 0.
                LOOP AT it_set_atv_del INTO DATA(wa_set_atv_del).

                  CLEAR: wa_canal_dist.
                  MOVE-CORRESPONDING  wa_set_atv_del TO  wa_set_atv.

                  wa_set_atv-cancelado = 'X'.
                  wa_set_atv-usuario_cancel = vg_usuario_cancel.
                  wa_set_atv-data_cancel    =  vg_data_cancel.
                  wa_set_atv-hora_cancel    =  vg_hora_cancel.

                  MODIFY zsdt0320 FROM wa_set_atv.
                  COMMIT WORK.

                ENDLOOP.

                DELETE  it_set_atv_alv WHERE  id = wa_canal_dist_alv-id
                                          AND seq_canal = wa_canal_dist_alv-seq_canal.
                DELETE  it_set_atv_alv_aux WHERE  id = wa_canal_dist_alv-id
                                              AND seq_canal = wa_canal_dist_alv-seq_canal.

                DELETE  it_set_atv  WHERE  id = wa_canal_dist_alv-id
                                            AND seq_canal = wa_canal_dist_alv-seq_canal.
              ENDIF.

              SELECT *
                 FROM zsdt0322
              INTO TABLE @DATA(it_area_venda_del)
               WHERE id = @wa_canal_dist_alv-id
                 AND  seq_canal = @wa_canal_dist_alv-seq_canal
                 AND cancelado <> 'X'.

              IF sy-subrc = 0.
                LOOP AT it_area_venda_del INTO DATA(wa_area_venda_del).

                  CLEAR: wa_area_venda.
                  MOVE-CORRESPONDING wa_area_venda_del TO wa_area_venda.


                  wa_area_venda-cancelado = 'X'.
                  wa_area_venda-usuario_cancel = vg_usuario_cancel.
                  wa_area_venda-data_cancel   =  vg_data_cancel.
                  wa_area_venda-hora_cancel   =  vg_hora_cancel.

                  MODIFY zsdt0322 FROM wa_area_venda.
                  COMMIT WORK.

                ENDLOOP.

                DELETE  it_area_venda_alv     WHERE  id = wa_canal_dist_alv-id
                                                 AND seq_canal = wa_canal_dist_alv-seq_canal.

                DELETE  it_area_venda_alv_aux WHERE  id = wa_canal_dist_alv-id
                                                 AND seq_canal = wa_canal_dist_alv-seq_canal.

                DELETE  it_area_venda         WHERE  id = wa_canal_dist_alv-id
                                                 AND seq_canal = wa_canal_dist_alv-seq_canal.
              ENDIF.

              LOOP AT it_area_venda_alv INTO wa_area_venda_alv WHERE id = wa_canal_dist_alv-id
                                                                     AND seq_canal = wa_canal_dist_alv-seq_canal.
                CLEAR: wa_area_venda.
                MOVE-CORRESPONDING wa_area_venda_alv TO wa_area_venda.

                SELECT SINGLE seq_area_venda
                  INTO @DATA(lva_seq_area_venda)
                  FROM zsdt0322
                 WHERE id        = @wa_canal_dist_alv-id
                   AND seq_canal = @wa_canal_dist_alv-seq_canal.

                IF sy-subrc = 0.

                  wa_area_venda-cancelado = 'X'.
                  wa_area_venda-usuario_cancel = vg_usuario_cancel.
                  wa_area_venda-data_cancel   =  vg_data_cancel.
                  wa_area_venda-hora_cancel   =  vg_hora_cancel.

                  MODIFY zsdt0322 FROM wa_area_venda.

                  DELETE  it_area_venda WHERE id = wa_canal_dist_alv-id
                           AND seq_canal = wa_canal_dist_alv-seq_canal.

                ENDIF.
                DELETE  it_area_venda_alv WHERE id = wa_canal_dist_alv-id
                                           AND seq_canal = wa_canal_dist_alv-seq_canal.

                IF wa_area_venda_alv-criado = 'X'.
                  DELETE  it_area_venda_alv_aux WHERE id = wa_canal_dist_alv-id
                             AND seq_canal = wa_canal_dist_alv-seq_canal.
                ENDIF.
              ENDLOOP.
*** Fim dados que estão na tabela
            ENDLOOP.
            COMMIT WORK.
          ENDIF.
        ENDIF.
    ENDCASE.

    CALL METHOD g_grid_distrib->refresh_table_display.
    CALL METHOD g_grid_atividade->refresh_table_display.
    CALL METHOD g_grid_adicionais->refresh_table_display.


  ENDMETHOD.                    "USER_COMMAND

  METHOD on_double_click.
    IF e_row-index IS NOT INITIAL.
      READ TABLE it_canal_dist_alv INDEX e_row-index INTO wa_canal_dist_alv.
      PERFORM fill_0103 USING space.
      LEAVE TO SCREEN 0100.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_0103_1 IMPLEMENTATION.
  METHOD toolbar.
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

  METHOD user_command.
    CASE e_ucomm.
      WHEN 'INSERT'.
        IF it_msg_ret[] IS INITIAL.
          IF wa_canal_dist_alv IS NOT INITIAL.
            CLEAR: wa_set_atv_alv.
            SORT it_set_atv BY id seq_canal.

            READ TABLE it_grupo_contas_alv_aux INTO DATA(lwa_grupo_contas) WITH KEY id  = wa_canal_dist_alv-id.

            SELECT  MAX( seq_setor )
             INTO @DATA(lva_seq_setor)
             FROM zsdt0320
            WHERE id = @wa_grupo_contas_alv-id
              AND seq_canal = @wa_canal_dist_alv-seq_canal .

            SORT it_set_atv_alv BY id seq_canal seq_setor DESCENDING.

            READ TABLE it_set_atv_alv INTO DATA(lwa_seq_c) WITH KEY  id = wa_grupo_contas_alv-id.

            IF lwa_seq_c-seq_setor > lva_seq_setor.
              lva_seq_setor = lwa_seq_c-seq_setor + 1.
            ELSE.
              lva_seq_setor = lva_seq_setor + 1.
            ENDIF.

            INSERT INITIAL LINE INTO it_set_atv_alv INDEX 1.

            wa_set_atv_alv-id        =  wa_canal_dist_alv-id.
            wa_set_atv_alv-seq_canal = wa_canal_dist_alv-seq_canal.
            wa_set_atv_alv-seq_setor = lva_seq_setor.
            wa_set_atv_alv-bukrs     = vg_bukrs.
            wa_set_atv_alv-ktokd     = lwa_grupo_contas-ktokd.
            wa_set_atv_alv-criado    = abap_true.

            FREE wa_set_atv_alv-celltab.
            t_estilo =  VALUE #( ( fieldname = 'ID' style = cl_gui_alv_grid=>mc_style_enabled ) ).
            t_estilo =  VALUE #( ( fieldname = 'SEQ_CANAL' style = cl_gui_alv_grid=>mc_style_enabled ) ).
            t_estilo =  VALUE #( ( fieldname = 'SEQ_SETOR' style = cl_gui_alv_grid=>mc_style_enabled ) ).

            INSERT LINES OF t_estilo INTO TABLE wa_set_atv_alv-celltab.
            MODIFY it_set_atv_alv FROM wa_set_atv_alv INDEX 1.
            CLEAR: lwa_grupo_contas.
          ENDIF.
        ELSE.
          PERFORM f_show_erro.
        ENDIF.
      WHEN 'DELETE'.
        FREE: t_del_rows.
        CALL METHOD g_grid_atividade->check_changed_data.
        CALL METHOD g_grid_atividade->get_selected_rows
          IMPORTING
            et_index_rows = t_del_rows.

        IF t_del_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        SORT t_del_rows BY index DESCENDING.

        IF t_del_rows[] IS NOT INITIAL.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Confirmação'
              text_question         = 'Deseja eliminar os dados selecionados?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              default_button        = '1'
              display_cancel_button = ''
            IMPORTING
              answer                = var_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          IF var_answer EQ '1'.

            LOOP AT t_del_rows INTO w_del_rows.
*** Eliminar dados que estão na tela
*             READ TABLE it_set_atv_alv_aux  INTO wa_set_atv_alv INDEX w_del_rows-index.
*              IF sy-subrc = 0.
*
*                vg_usuario_cancel = sy-uname.
*                vg_data_cancel = sy-datum.
*                vg_hora_cancel = sy-uzeit.
*
*                MOVE-CORRESPONDING wa_set_atv_alv TO wa_set_atv.
*
*                SELECT SINGLE seq_setor
*                   INTO lva_seq_setor
*                   FROM zsdt0320
*                  WHERE id = wa_set_atv_alv-id
*                    AND seq_canal = wa_set_atv_alv-seq_canal
*                    AND seq_setor = wa_set_atv_alv-seq_setor.
*
*                IF sy-subrc = 0.
*                  wa_set_atv-cancelado = 'X'.
*                  wa_set_atv-usuario_cancel = vg_usuario_cancel.
*                  wa_set_atv-data_cancel    =  vg_data_cancel.
*                  wa_set_atv-hora_cancel    =  vg_hora_cancel.
*
*                  MODIFY zsdt0320 FROM wa_set_atv.
*                ENDIF.
*
*              DELETE it_set_atv_alv_aux  INDEX w_del_rows-index.
*              DELETE it_set_atv_alv   WHERE id =  wa_set_atv_alv-id AND
*                                          seq_canal = wa_set_atv_alv-seq_canal AND
*                                          seq_setor = wa_set_atv_alv-seq_setor.
*              CLEAR: wa_set_atv.
*** Eliminar dados que estão na tabela
              CLEAR: wa_set_atv_alv.
              READ TABLE it_set_atv_alv  INTO wa_set_atv_alv INDEX w_del_rows-index.
              IF sy-subrc = 0.

                vg_usuario_cancel = sy-uname.
                vg_data_cancel = sy-datum.
                vg_hora_cancel = sy-uzeit.

                CLEAR:  wa_set_atv.
                MOVE-CORRESPONDING wa_set_atv_alv TO wa_set_atv.

                SELECT SINGLE seq_setor
                   INTO lva_seq_setor
                   FROM zsdt0320
                  WHERE id = wa_set_atv_alv-id
                    AND seq_canal = wa_set_atv_alv-seq_canal
                    AND seq_setor = wa_set_atv_alv-seq_setor.

                IF sy-subrc = 0.
                  wa_set_atv-cancelado = 'X'.
                  wa_set_atv-usuario_cancel = vg_usuario_cancel.
                  wa_set_atv-data_cancel    =  vg_data_cancel.
                  wa_set_atv-hora_cancel    =  vg_hora_cancel.

                  MODIFY zsdt0320 FROM wa_set_atv.

                  DELETE  it_set_atv WHERE id = wa_set_atv_alv-id
                                       AND seq_canal =  wa_set_atv_alv-seq_canal
                                       AND seq_setor = wa_set_atv_alv-seq_setor.


                ENDIF.

                DELETE it_set_atv_alv  INDEX w_del_rows-index.

                IF  wa_set_atv_alv-criado = 'X'.
                  DELETE  it_set_atv_alv_aux WHERE id = wa_set_atv_alv-id
                                               AND seq_canal =  wa_set_atv_alv-seq_canal
                                               AND seq_setor = wa_set_atv_alv-seq_setor.
                ENDIF.

                CLEAR: wa_set_atv.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
    ENDCASE.

    CALL METHOD g_grid_atividade->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.                    "USER_COMMAND

  METHOD on_double_click.
    IF e_row-index IS NOT INITIAL.
      READ TABLE it_set_atv_alv INDEX e_row-index INTO wa_set_atv_alv.
      "PERFORM fill_0103 USING space.
      "LEAVE TO SCREEN 0103.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_0103_2 IMPLEMENTATION.
  METHOD toolbar.
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

  METHOD user_command.
    DATA: lva_seq_parceiro TYPE zsdt0321-seq_parceiro.

  ENDMETHOD.                    "USER_COMMAND

  METHOD on_double_click.
    IF e_row-index IS NOT INITIAL.
      "READ TABLE it_parceiros_alv INDEX e_row-index INTO wa_parceiros_alv.  "wa_set_atv_alv. 0904
      "PERFORM fill_0103 USING space.
      "LEAVE TO SCREEN 0103.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_0103_3 IMPLEMENTATION.
  METHOD toolbar.
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

  METHOD user_command.
    CASE e_ucomm.
      WHEN 'INSERT'.

        IF wa_canal_dist_alv IS NOT INITIAL.
          IF it_area_venda_alv[] IS INITIAL.
            CLEAR: wa_area_venda_alv.
            INSERT INITIAL LINE INTO it_area_venda_alv  INDEX 1.

            wa_area_venda_alv-id             = wa_canal_dist_alv-id.
            wa_area_venda_alv-seq_canal      = wa_canal_dist_alv-seq_canal.
            wa_area_venda_alv-seq_area_venda = 1.
            wa_area_venda_alv-criado         = abap_true.

            FREE wa_area_venda_alv-celltab.
            t_estilo =  VALUE #( ( fieldname = 'KTOKD' style = cl_gui_alv_grid=>mc_style_enabled ) ).
            INSERT LINES OF t_estilo INTO TABLE wa_area_venda_alv-celltab.

            MODIFY it_area_venda_alv FROM wa_area_venda_alv  INDEX 1.
          ENDIF.
        ENDIF.
      WHEN 'DELETE'.
        FREE: t_del_rows.

        CALL METHOD g_grid_adicionais->check_changed_data.
        CALL METHOD g_grid_adicionais->get_selected_rows
          IMPORTING
            et_index_rows = t_del_rows.

        IF t_del_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        SORT t_del_rows BY index DESCENDING.

        IF t_del_rows[] IS NOT INITIAL.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Confirmação'
              text_question         = 'Deseja eliminar os dados selecionados?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              default_button        = '1'
              display_cancel_button = ''
            IMPORTING
              answer                = var_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          IF var_answer EQ '1'.

*            LOOP AT t_del_rows INTO w_del_rows.
*              READ TABLE it_area_venda_alv_aux INTO wa_area_venda_alv  INDEX w_del_rows-index.
*              IF sy-subrc = 0.
*
*                vg_usuario_cancel = sy-uname.
*                vg_data_cancel = sy-datum.
*                vg_hora_cancel = sy-uzeit.
*
*                MOVE-CORRESPONDING wa_area_venda_alv TO wa_area_venda.
*
*                SELECT SINGLE seq_area_venda
*                         INTO @DATA(lva_seq_area_venda)
*                         FROM zsdt0322
*                        WHERE id             = @wa_area_venda_alv-id
*                          AND seq_canal      = @wa_area_venda_alv-seq_canal
*                          AND seq_area_venda = @wa_area_venda_alv-seq_area_venda .
*
*                IF sy-subrc = 0.
*
*                  wa_area_venda-cancelado = 'X'.
*                  wa_area_venda-usuario_cancel = vg_usuario_cancel.
*                  wa_area_venda-data_cancel   =  vg_data_cancel.
*                  wa_area_venda-hora_cancel   =  vg_hora_cancel.
*
*                  MODIFY zsdt0322 FROM wa_area_venda.
*
*                ENDIF.
*
*                DELETE it_area_venda WHERE id = wa_area_venda_alv-id
*                                       AND seq_canal = wa_area_venda_alv-seq_canal
*                                       AND seq_area_venda = wa_area_venda_alv-seq_area_venda .
*
*                DELETE it_area_venda_alv_aux  INDEX w_del_rows-index.
*
*
*                DELETE it_area_venda_alv   WHERE id = wa_area_venda_alv-id
*                                                 AND seq_canal = wa_area_venda_alv-seq_canal
*                                                 AND seq_area_venda = wa_area_venda_alv-seq_area_venda .
*
*                CLEAR: wa_area_venda.
*              ENDIF.

            LOOP AT t_del_rows INTO w_del_rows.
              READ TABLE it_area_venda_alv INTO wa_area_venda_alv  INDEX w_del_rows-index.
              IF sy-subrc = 0.
                vg_usuario_cancel = sy-uname.
                vg_data_cancel = sy-datum.
                vg_hora_cancel = sy-uzeit.

                MOVE-CORRESPONDING wa_area_venda_alv TO wa_area_venda.

                SELECT SINGLE seq_area_venda
                         INTO @DATA(lva_seq_area_venda)
                         FROM zsdt0322
                        WHERE id             = @wa_area_venda_alv-id
                          AND seq_canal      = @wa_area_venda_alv-seq_canal
                          AND seq_area_venda = @wa_area_venda_alv-seq_area_venda .

                IF sy-subrc = 0.

                  wa_area_venda-cancelado = 'X'.
                  wa_area_venda-usuario_cancel = vg_usuario_cancel.
                  wa_area_venda-data_cancel   =  vg_data_cancel.
                  wa_area_venda-hora_cancel   =  vg_hora_cancel.

                  MODIFY zsdt0322 FROM wa_area_venda.

                  DELETE it_area_venda  WHERE id = wa_area_venda_alv-id
                                   AND seq_canal = wa_area_venda_alv-seq_canal
                                   AND seq_area_venda = wa_area_venda_alv-seq_area_venda .



                ENDIF.

*                DELETE it_area_venda WHERE id = wa_area_venda_alv-id
*                                       AND seq_canal = wa_area_venda_alv-seq_canal
*                                       AND seq_area_venda = wa_area_venda_alv-seq_area_venda .

                DELETE it_area_venda_alv  INDEX w_del_rows-index.

                IF  wa_set_atv_alv-criado = 'X'.
                  DELETE it_area_venda_alv_aux   WHERE id = wa_area_venda_alv-id
                                                   AND seq_canal = wa_area_venda_alv-seq_canal
                                                   AND seq_area_venda = wa_area_venda_alv-seq_area_venda .
                ENDIF.
                CLEAR: wa_area_venda.
              ENDIF.

            ENDLOOP.
          ENDIF.
        ENDIF.

    ENDCASE.
*
    CALL METHOD g_grid_adicionais->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDMETHOD.                    "USER_COMMAND

  METHOD on_double_click.
    IF e_row-index IS NOT INITIAL.
      READ TABLE it_area_venda_alv INDEX e_row-index INTO wa_area_venda_alv .
      "PERFORM fill_0103 USING space.
      "LEAVE TO SCREEN 0103.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
