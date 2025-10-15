*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    PERFORM f_exit_0009 TABLES it_excl_toolbar.

    READ TABLE it_excl_toolbar INTO wa_excl_toolbar WITH KEY code = 'Novo'.
    IF sy-subrc <> 0.
      ty_toolbar-icon      = icon_create.
      ty_toolbar-function  = c_novo.
      ty_toolbar-text      = 'Novo'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    READ TABLE it_excl_toolbar INTO wa_excl_toolbar WITH KEY code = 'Modificar'.
    IF sy-subrc <> 0.
      ty_toolbar-icon      = icon_change.
      ty_toolbar-function  = c_change.
      ty_toolbar-text      = 'Modificar'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    READ TABLE it_excl_toolbar INTO wa_excl_toolbar WITH KEY code =  'Deletar'.
    IF sy-subrc <> 0.
      ty_toolbar-icon      = icon_delete_row.
      ty_toolbar-function  = c_del.
      ty_toolbar-text      = 'Deletar'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    IF p_popup IS NOT INITIAL.
      ty_toolbar-icon      = icon_wizard.
      ty_toolbar-function  = c_func_02.
      ty_toolbar-text      = p_ti_01.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ELSEIF p_ti_01 IS NOT INITIAL.
      ty_toolbar-icon      = icon_wizard.
      ty_toolbar-function  = c_func_01.
      ty_toolbar-text      = p_ti_01.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    "US 172204 - Ajuste Sub Cadastro Zregister_data - WPP --->>
    if p_ti_03 IS NOT INITIAL.
      ty_toolbar-icon      = icon_wizard.
      ty_toolbar-function  = c_func_03.
      ty_toolbar-text      = p_ti_03.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.
    "US 172204 - Ajuste Sub Cadastro Zregister_data - WPP <<---

    IF p_act_01 IS NOT INITIAL.
      ty_toolbar-icon      = icon_wizard.
      ty_toolbar-function  = c_action_01.
      ty_toolbar-text      = p_act_01.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

*   MMSILVA - 11.02.2025 - #163322 - Inicio
    IF p_act_02 IS NOT INITIAL.
      ty_toolbar-icon      = icon_wizard.
      ty_toolbar-function  = c_action_02.
      ty_toolbar-text      = p_act_02.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.
*   MMSILVA - 11.02.2025 - #163322 - Fim

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

    CASE e_ucomm.
      WHEN c_del.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF p_maxdel IS NOT INITIAL.
          DATA(_max_reg_del) = p_maxdel.
        ELSE.
          _max_reg_del = 1.
        ENDIF.

        DATA(_lines_sel) = lines( it_sel_rows ).

        IF _lines_sel > _max_reg_del.
          MESSAGE |Só é possivel selecionar no máximo { _max_reg_del } registros para eliminação!| TYPE 'S'.
          EXIT.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Deseja realmente deletar o(s) registro(s) selecionado(s)?'
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

        DATA(_refresh_selecao) = abap_false.

        LOOP AT it_sel_rows INTO wa_sel_rows.

          READ TABLE <fs_it_saida> INTO <fs_wa_saida> INDEX wa_sel_rows-index.

          CHECK sy-subrc = 0.

          DATA(_break)   = abap_false.
          PERFORM f_exit_0014 USING <fs_wa_saida> CHANGING _break.

          IF _break = abap_true.
            PERFORM: f_selecionar_dados,
                     f_processa_dados.

            LEAVE TO SCREEN 0001.
          ENDIF.

          DATA(_error) = abap_false.
          PERFORM f_exit_0006 USING <fs_wa_saida> CHANGING _error.

          CHECK _error = abap_false.

          "Criar condição dinamica
          PERFORM f_get_cond_chave USING '<FS_WA_SAIDA>'
                                CHANGING vg_cond.

          CHECK vg_cond-where_tab[] IS NOT INITIAL.

*-CS2025000249-17.04.2025-#173311-JT-inicio
          IF p_nosave = abap_false.
            DELETE FROM (p_db_tab) WHERE (vg_cond-where_tab).
          ELSE.
            sy-subrc = 0.
          ENDIF.
*-CS2025000249-17.04.2025-#173311-JT-fim

          IF sy-subrc = 0.
            MESSAGE 'Registro deletado com sucesso!' TYPE 'S'.
            _refresh_selecao = abap_true.
          ELSE.
            MESSAGE 'Houve um erro ao deletar o registro!' TYPE 'I'.
          ENDIF.

        ENDLOOP.

        IF  _refresh_selecao IS NOT INITIAL.
          PERFORM: f_selecionar_dados,
                   f_processa_dados.

          LEAVE TO SCREEN 0001.
        ENDIF.

      WHEN c_novo.

        vg_operacao = e_ucomm.

        CLEAR: <fs_wa_registro_manter>, <fs_wa_saida>.

        PERFORM f_exit_0001 CHANGING <fs_wa_registro_manter>.
        PERFORM f_exit_0012 CHANGING <fs_wa_registro_manter> <fs_wa_saida>.

        CALL SCREEN p_scmant STARTING AT 10 05.

        PERFORM: f_selecionar_dados,
                 f_processa_dados.

        LEAVE TO SCREEN 0001.

      WHEN c_change.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF lines( it_sel_rows ) NE 1.
          MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

        READ TABLE <fs_it_saida> INTO <fs_wa_saida> INDEX wa_sel_rows-index.
*        PERFORM F_EXIT_0001 CHANGING <FS_WA_REGISTRO_MANTER>.

        CHECK sy-subrc = 0.

        CLEAR: <fs_wa_registro_manter>.
        "DATA: WS_SAIDA TYPE ZMMT0111.

        "MOVE-CORRESPONDING <FS_WA_SAIDA> TO WS_SAIDA.
        "WS_SAIDA-DT_REGISTRO = SY-DATUM.
        "WS_SAIDA-HR_REGISTRO = SY-UZEIT.
        "WS_SAIDA-US_REGISTRO = SY-UNAME.

        "MOVE-CORRESPONDING WS_SAIDA TO <FS_WA_REGISTRO_MANTER>.

        MOVE-CORRESPONDING <fs_wa_saida> TO <fs_wa_registro_manter>.

        vg_operacao = e_ucomm.

        CALL SCREEN p_scmant STARTING AT 10 05.

        PERFORM: f_selecionar_dados,
                 f_processa_dados.

        LEAVE TO SCREEN 0001.

      WHEN c_func_01.

        SUBMIT zregister_data WITH p_db_tab = p_db_01
                              WITH p_stcnam = p_stc_01
                              WITH p_scmant = p_sc_01
                              WITH p_title  = p_ti_01
                              AND RETURN.

      "US 172204 - Ajuste Sub Cadastro Zregister_data - WPP --->>
      WHEN c_func_03.

        SUBMIT zregister_data WITH p_db_tab = p_db_03
                              WITH p_stcnam = p_stc_03
                              WITH p_scmant = p_sc_03
                              WITH p_title  = p_ti_03
                              AND RETURN.
     "US 172204 - Ajuste Sub Cadastro Zregister_data - WPP <<---

      WHEN c_func_02.

        CALL FUNCTION 'ZREGISTER_DATA'
          EXPORTING
            i_db_tab    = p_db_01
            i_stcnam    = p_stc_01
            i_title     = p_ti_01
            i_start_lin = p_stalin
            i_start_col = p_stacol
            i_end_lin   = p_endlin
            i_end_col   = p_endcol
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid
                TYPE sy-msgty
              NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN c_action_01.

        FIELD-SYMBOLS: <fs_lit_saida> TYPE table.

        "Monta Estrutura Saida
        CREATE DATA d_it_saida TYPE TABLE OF (p_stcnam).
        ASSIGN d_it_saida->* TO  <fs_lit_saida>.

        CLEAR: it_sel_rows[], wa_sel_rows.

        CALL METHOD obj_alv->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF it_sel_rows[] IS INITIAL.
          MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        LOOP AT it_sel_rows INTO wa_sel_rows.

          READ TABLE <fs_it_saida> INTO <fs_wa_saida> INDEX wa_sel_rows-index.
          CHECK sy-subrc EQ 0.

          APPEND  <fs_wa_saida> TO <fs_lit_saida>.

        ENDLOOP.

        PERFORM f_exit_0013 TABLES <fs_lit_saida>.

*        PERFORM f_exit_0020 CHANGING _refresh_selecao. "-US 145924-10-07-2024-#145924-RJF

*        IF  _refresh_selecao IS NOT INITIAL.           "-US 145924-10-07-2024-#145924-RJF
        PERFORM: f_selecionar_dados,                 "-US 145924-10-07-2024-#145924-RJF
                 f_processa_dados.                   "-US 145924-10-07-2024-#145924-RJF
*        ENDIF.                                         "-US 145924-10-07-2024-#145924-RJF

        LEAVE TO SCREEN 0001.

*     MMSILVA - 11.02.2025 - #163322 - Inicio
      WHEN c_action_02.

        PERFORM f_exit_0020.

*     MMSILVA - 11.02.2025 - #163322 - Fim

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler IMPLEMENTATION.
  METHOD catch_hotspot.

    IF e_row_id-index IS NOT INITIAL. " Ini - RJF - CS2024000342 ZFIT0189 #138578 13.05.2024
      READ TABLE <fs_it_saida> INTO <fs_wa_saida> INDEX e_row_id-index.
      CHECK sy-subrc EQ 0.

      PERFORM f_exit_0018  USING <fs_wa_saida>           " RJF
                                 e_column_id-fieldname   " RJF
                                 e_row_id-index.         " RJF
    ENDIF. " Fim - RJF - CS2024000342 ZFIT0189 #138578 13.05.2024

    CASE e_column_id.
      WHEN 'X'.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
