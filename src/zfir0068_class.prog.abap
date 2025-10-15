*&---------------------------------------------------------------------*
*&  Include           ZFIR0068_CLASS
*&---------------------------------------------------------------------*


CLASS lcl_alv_toolbar_list IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    ty_toolbar-icon      = icon_system_save.
    ty_toolbar-function  = 'SAVE'.
    ty_toolbar-text      = 'Salvar'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_wizard.
    ty_toolbar-function  = 'INSR'.
    ty_toolbar-text      = 'Incluir Lcto.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_storno.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-text      = 'Excluir Lcto.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'SAVE'.
        PERFORM salvar_alteracoes.
      WHEN 'INSR'.
        CALL SCREEN 0101 STARTING AT 02 02 ENDING AT 160 20 .
        CALL SCREEN 0100.
      WHEN 'DEL'.
        PERFORM estornar_lcto.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_alv_toolbar_input IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
  ENDMETHOD.                    "HANDLE_USER_COMMAND


ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler_0101 IMPLEMENTATION.                "

  METHOD user_command.
  ENDMETHOD.                    "ON_F4

  METHOD on_data_changed_finished.

    PERFORM atualiza_dados_prev.

  ENDMETHOD.


  METHOD on_f4.

    "TYPES
    TYPES: BEGIN OF ty_par_flx,
             codigo    TYPE zfit0109-codigo,
             descricao TYPE zfit0109-descricao,
           END OF ty_par_flx.

    DATA: lt_map     TYPE TABLE OF dselc,
          ls_map     TYPE dselc,
          lt_return  TYPE TABLE OF ddshretval,
          ls_return  TYPE ddshretval,
          ls_stable  TYPE lvc_s_stbl,

          lt_par_flx TYPE TABLE OF ty_par_flx,
          ls_par_flx TYPE ty_par_flx.

    DATA: vl_tp_doc_aux(30) TYPE c,
          vl_tp_ped_aux(40) TYPE c,
          vl_tp_ov_aux(40)  TYPE c.

    "FIELD-SYMBOLS
    FIELD-SYMBOLS: <l_out> TYPE ty_saida_input. " ALV TABLE LINE

    " CHECK WHICH FIELD RAISE F4 EVENT
    CASE e_fieldname.
      WHEN 'CODIGO_FLX'.

        " READ CURRENT LINE
        READ TABLE it_saida_input ASSIGNING <l_out> INDEX es_row_no-row_id.

        "LOAD F4 DATA
        SELECT codigo descricao
          INTO TABLE lt_par_flx
          FROM zfit0109
         WHERE tp_prev IN ('M','S').

        SORT lt_par_flx BY codigo.

        "SET RETURN FIELD
        CLEAR ls_map.
        ls_map-fldname = 'F0001'.
        ls_map-dyfldname = 'CODIGO'.
        APPEND ls_map TO lt_map.

        "SET RETURN FIELD
        CLEAR ls_map.
        ls_map-fldname = 'F0002'.
        ls_map-dyfldname = 'DESCRICAO'.
        APPEND ls_map TO lt_map.

        "SET RETURN FIELD
        CLEAR ls_map.
        ls_map-fldname = 'F0003'.
        ls_map-dyfldname = 'CLAS_FLX'.
        APPEND ls_map TO lt_map.

        " CALL SEARCH HELP POPUP FUNCTION
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CODIGO'
            value_org       = 'S'
          TABLES
            value_tab       = lt_par_flx
            dynpfld_mapping = lt_map
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.


        " READ SELECTED F4 VALUE
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'.
        IF ls_return IS NOT INITIAL.
          <l_out>-codigo_flx = ls_return-fieldval.

          CLEAR: wa_0109_aux.
          SELECT SINGLE *
            INTO wa_0109_aux
            FROM zfit0109
           WHERE codigo = <l_out>-codigo_flx.

          IF sy-subrc = 0.
            <l_out>-tp_prev = wa_0109_aux-tp_prev.

            IF wa_0109_aux-tp_prev = 'S'. "Saldo Inicial.
              <l_out>-dt_vcto = sy-datum.
            ENDIF.
          ENDIF.

        ENDIF.

        " READ SELECTED F4 VALUE
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0002'.
        IF ls_return IS NOT INITIAL.
          <l_out>-descricao = ls_return-fieldval.
        ENDIF.

        " READ SELECTED F4 VALUE
        READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0003'.
        IF ls_return IS NOT INITIAL.
          <l_out>-clas_flx = ls_return-fieldval.
        ENDIF.

    ENDCASE.

    ls_stable = ''. " SET STABLE REFRESH FOR ROW AND COLUMN

    " ALV REFRESH
    CALL METHOD obj_alv_input->refresh_table_display
      EXPORTING
        is_stable      = ls_stable
        i_soft_refresh = 'X'
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.

    "AVOID POSSIBLE STANDARD SEARCH HELP
    er_event_data->m_event_handled = 'X'.

  ENDMETHOD.                    "ON_F4


  METHOD on_data_changed.
  ENDMETHOD.                    "ON_DATA_CHANGED


ENDCLASS.           "lcl_event_handler_0102 IMPLEMENTATION
