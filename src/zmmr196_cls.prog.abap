*&---------------------------------------------------------------------*
*& Include          ZMMR196_CLS
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm, " RJF - IR153816
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells, " RJF - IR153816
      catch_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.

ENDCLASS.
CLASS lcl_application_f4 DEFINITION.

  PUBLIC SECTION.
    METHODS:

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

*    methods: reset.
*    methods: show_f4.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD catch_hotspot.
    PERFORM f_monta_docs USING e_row_id-index e_column_id.
  ENDMETHOD.
*-US 153816-22-10-2024-#153816-RJF-Inicio
  METHOD on_data_changed_finished.

    CHECK e_modified IS NOT INITIAL.
    CALL METHOD o_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.

  METHOD on_data_changed.

    LOOP AT er_data_changed->mt_mod_cells INTO DATA(ls_good)
      WHERE fieldname EQ 'URGENCIA_NECES'.

      DATA(vl_un) = t_saida[ ls_good-row_id ]-urgencia_neces.

      IF ls_good-value IS NOT INITIAL.
        READ TABLE it_dd07t INTO DATA(wa_dd07t) WITH KEY domvalue_l(2) = ls_good-value(2).
        IF sy-subrc IS NOT INITIAL OR ls_good-error IS NOT INITIAL.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Urgência Necessidade(UN), entrar um valor válido!'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
*-US 153816-22-10-2024-#153816-RJF-Fim

ENDCLASS.

CLASS lcl_application_f4 IMPLEMENTATION.

*§2. Implement an event handler method for event ONF4.

  METHOD on_f4.

* Save event parameter as global attributes of this class
* (maybe solved differently if you use a function module!)
*    f4_params-c_fieldname = e_fieldname.
*    f4_params-cs_row_no = es_row_no.
*    f4_params-cr_event_data = er_event_data.
*    f4_params-ct_bad_cells = et_bad_cells.
*    f4_params-c_display = e_display.
********************************************************************** 152612 CS2024000814 - Regra contratos A definir ZMM0223 PSA
    DATA: lt_parameter     TYPE STANDARD TABLE OF  bapiparam,
          lt_return        TYPE STANDARD TABLE OF  bapiret2,
          _mostra_itens(1) TYPE c.
    CLEAR: _mostra_itens.
    FREE: lt_parameter,lt_return.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username  = sy-uname
      TABLES
        parameter = lt_parameter
        return    = lt_return.

    READ TABLE lt_parameter ASSIGNING FIELD-SYMBOL(<_find_param>) WITH KEY parid = 'ZPARAM_ZMM0223_CONTR'.

    IF sy-subrc = 0.
      _mostra_itens = abap_true.
    ELSE.
      _mostra_itens = abap_false.
    ENDIF.
**********************************************************************
    PERFORM f_monta_f4_contratos USING es_row_no-row_id _mostra_itens.

    er_event_data->m_event_handled = 'X'.
  ENDMETHOD.

ENDCLASS.

DATA: o_container    TYPE REF TO cl_gui_custom_container,
      g_onf4         TYPE REF TO lcl_application_f4,
      event_receiver TYPE REF TO lcl_event_receiver.                                         "on_f4
