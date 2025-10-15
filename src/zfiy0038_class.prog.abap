*&---------------------------------------------------------------------*
*&  Include           ZFIY0038_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_event_receiver_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    ty_toolbar-icon      = icon_generate.
    ty_toolbar-function  = c_sol_inter.
    ty_toolbar-text      = 'Solicitud de intercambio'(007).
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_cancel.
    ty_toolbar-function  = c_anul_aplic.
    ty_toolbar-text      = 'Anular Aplicação Câmbio'(008).
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN c_sol_inter.
        PERFORM f_solic_intercambio.
      WHEN c_anul_aplic.
        PERFORM f_anula_aplicacao.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD handle_data_changed.
    PERFORM f_check_data_0100 USING er_data_changed.
  ENDMETHOD.

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_receiver_0101 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_receiver_0200 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_system_save.
    ty_toolbar-function  = c_save_fchto.
    ty_toolbar-text      = 'Salvar'(022).
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_generate.
    ty_toolbar-function  = c_ger_contab.
    ty_toolbar-text      = 'Gerar Contábil'(023).
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN c_save_fchto.
        PERFORM f_save_fchto.
      WHEN c_ger_contab.
        PERFORM f_gera_contabil.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD handle_data_changed.
    PERFORM f_check_data_0200 USING er_data_changed.
    CALL METHOD semantic_checks( er_data_changed ).
  ENDMETHOD.

  METHOD semantic_checks.
    DATA: ls_good       TYPE lvc_s_modi.

    DATA: lit_rows TYPE lvc_t_row.
    DATA: lwa_selected_line TYPE lvc_s_row,
          lf_row_index      TYPE int4.

    CLEAR gva_error_in_data.

    CALL METHOD obj_alv_0200->get_selected_rows
      IMPORTING
        et_index_rows = lit_rows.

    LOOP AT lit_rows INTO lwa_selected_line.

      lf_row_index = lwa_selected_line-index.

      READ TABLE git_saida_0200 INTO gwa_saida_0200 INDEX lf_row_index.

      IF  gwa_saida_0200-cod_fechto IS INITIAL.
        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = '0K'
            i_msgno     = '000'
            i_msgty     = 'E'
            i_msgv1     = 'Campo Obrigatório'(024)
            i_fieldname = 'COD_FECHTO'
            i_row_id    = lf_row_index.

        gva_error_in_data = 'X'.
      ENDIF.

      IF gwa_saida_0200-nro_liq_cb IS INITIAL.

        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = '0K'
            i_msgno     = '000'
            i_msgty     = 'E'
            i_msgv1     = 'Campo Obrigatório'(024)
            i_fieldname = 'NRO_LIQ_CB'
            i_row_id    = lf_row_index.

        gva_error_in_data = 'X'.
      ENDIF.

      IF gwa_saida_0200-tp_lcto IS INITIAL.

        CALL METHOD pr_data_changed->add_protocol_entry
          EXPORTING
            i_msgid     = '0K'
            i_msgno     = '000'
            i_msgty     = 'E'
            i_msgv1     = 'Campo Obrigatório'(024)
            i_fieldname = 'TP_LCTO'
            i_row_id    = lf_row_index.

        gva_error_in_data = 'X'.
      ENDIF.

    ENDLOOP.

    IF  gva_error_in_data = 'X'.
      CALL METHOD pr_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.

  METHOD on_f4.
    PERFORM f_on_f4_0200 USING e_fieldname
                               e_fieldvalue
                               es_row_no
                               er_event_data
                               et_bad_cells
                               e_display.
  ENDMETHOD.

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
