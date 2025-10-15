*----------------------------------------------------------------------*
*   INCLUDE BCALV_TOOLBAR_EVENT_RECEIVER                               *
*----------------------------------------------------------------------*

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
    DATA: ls_sflight TYPE sflight.
    CASE fcode.
      WHEN 'DELETE'.
      WHEN 'INSERT_LC'.
      WHEN 'INSERT_FC'.
      WHEN 'INSERT_FS'.
      WHEN 'INSERT_LS'.
      WHEN 'INSERT_NS'.
    ENDCASE.
*   update frontend

    CALL METHOD tree1->frontend_update.
  ENDMETHOD.

  METHOD on_toolbar_dropdown.
* create contextmenu
    DATA: l_menu       TYPE REF TO cl_ctmenu,
          l_fc_handled TYPE as4flag.

    CREATE OBJECT l_menu.
    CLEAR l_fc_handled.

    CASE fcode.
      WHEN 'INSERT_LC'.
        l_fc_handled = 'X'.
*       insert as last child
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_LC'
            text  = 'Insert New Line as Last Child'.        "#EC NOTEXT
*       insert as first child
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_FC'
            text  = 'Insert New Line as First Child'.       "#EC NOTEXT
*       insert as next sibling
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_NS'
            text  = 'Insert New Line as Next Sibling'.      "#EC NOTEXT
*       insert as last sibling
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_LS'
            text  = 'Insert New Line as Last Sibling'.      "#EC NOTEXT
*       insert as first sibling
        CALL METHOD l_menu->add_function
          EXPORTING
            fcode = 'INSERT_FS'
            text  = 'Insert New Line as First Sibling'.     "#EC NOTEXT
    ENDCASE.

* show dropdownbox
    IF l_fc_handled = 'X'.
      CALL METHOD mr_toolbar->track_context_menu
        EXPORTING
          context_menu = l_menu
          posx         = posx
          posy         = posy.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.

    DATA: l_erro TYPE char01.

    CHECK l_seleciona = abap_true.

    READ TABLE t_saida  INTO w_saida  INDEX e_row-index.

    IF sy-subrc = 0.
      PERFORM f_validar CHANGING l_erro.  "*-CS2024000522-18.07.2024-JT-#143588
      IF l_erro = abap_false.             "*-CS2024000522-18.07.2024-JT-#143588
        PERFORM f_salva.
        CALL METHOD g_custom_container->free.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD on_data_changed4.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          wl_name1 TYPE makt-maktx,
          l_ebeln  TYPE ebeln,
          l_ebelp  TYPE ebelp,
          l_matnr  TYPE ekpo-matnr,
          l_maktx  TYPE makt-maktx.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good
                             WHERE fieldname = 'PC_VEICULO'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE t_transp INTO w_transp INDEX ls_good-row_id.

      CLEAR w_zlest0002.
      SELECT SINGLE *
        INTO w_zlest0002
        FROM zlest0002
       WHERE pc_veiculo = lv_value.

      CLEAR w_lfa1.
      SELECT SINGLE *
        INTO w_lfa1
        FROM lfa1
       WHERE lifnr = w_zlest0002-proprietario.

      lv_value = w_zlest0002-cd_cidade.
      w_transp-cd_cidade = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CD_CIDADE'
          i_value     = lv_value.

      lv_value = w_zlest0002-cd_uf.
      w_transp-cd_uf  = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CD_UF'
          i_value     = lv_value.

      lv_value = w_zlest0002-cd_renavam.
      w_transp-cd_renavam = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CD_RENAVAM'
          i_value     = lv_value.

      lv_value = w_zlest0002-proprietario.
      w_transp-proprietario = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'PROPRIETARIO'
          i_value     = lv_value.

      lv_value = w_zlest0002-tp_veiculo.
      w_transp-tp_veiculo = lv_value.

** BUG - 98417 - Inicio - CBRAND
      IF w_zlest0002 IS INITIAL.
        IF w_transp-tipo_placa = 'Placa Cavalo'.
          w_zlest0002-tp_veiculo = 0.
        ELSE.
          w_zlest0002-tp_veiculo = 1.
        ENDIF.
        CONDENSE w_zlest0002-tp_veiculo NO-GAPS.
      ENDIF.
** BUG - 98417 - Fim - CBRAND

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_VEICULO'
          i_value     = lv_value.

      lv_value = w_zlest0002-tp_veiculo.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TP_VEICULO'
          i_value     = lv_value.

      lv_value = w_lfa1-name1.
      w_transp-des_proprietario = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DES_PROPRIETARIO'
          i_value     = lv_value.

      IF     w_lfa1-stcd1 IS NOT INITIAL.
        lv_value = w_lfa1-stcd1.
      ELSEIF w_lfa1-stcd2 IS NOT INITIAL.
        lv_value = w_lfa1-stcd2.
      ENDIF.

      w_transp-cnpj_cpf_prop =  lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CNPJ_CPF_PROP'
          i_value     = lv_value.

      MODIFY t_transp FROM w_transp INDEX ls_good-row_id.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
