*&---------------------------------------------------------------------*
*&  Include           ZSDR0117_PAI
*&---------------------------------------------------------------------*

MODULE pai_0100 INPUT.


  CASE sy-ucomm.
    WHEN c_new_bol.
      PERFORM f_new_boletim_producao.
    WHEN c_edit_bol.
      PERFORM f_edit_boletim_producao.
    WHEN c_save_bol.
      PERFORM f_gravar_boletim_producao.
    WHEN c_search_bol.
      PERFORM f_search_boletim_producao.
    WHEN c_del_bol.
      PERFORM f_del_boletim_producao.
    WHEN c_canc_edit.
      PERFORM f_cancel_edicao_bol.
    WHEN c_aprov_bol.
      PERFORM f_aprovar_boletim_producao.
    WHEN c_desaprov_bol.
      PERFORM f_desaprovar_boletim_producao.
    WHEN c_atua_bol.
      PERFORM: f_atualizar_saldos,
               f_leave_to_screen.
    WHEN c_enable_tim.
      PERFORM: f_enable_time,
               f_leave_to_screen.
    WHEN c_disable_tim.
      PERFORM: f_disable_time,
               f_leave_to_screen.
    WHEN bt_dados_ads. "Mostrar informações dados adicionais do boletim.
      PERFORM fm_call_screen_adc.
    WHEN c_sel_tp_boletim.
      PERFORM fm_sel_boletim.
    WHEN 'BACK'.

      DATA: var_answer TYPE c.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja realmente sair da transação?'
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

      LEAVE PROGRAM.

    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'MIN_DET_BOL'.

      IF wg_cab_boletim_prod-id_boletim IS INITIAL.
        MESSAGE 'Nenhum Boletim foi selecionado!' TYPE 'W'.
        RETURN.
      ENDIF.

      IF ( vg_operacao_bol EQ c_edit_bol ) OR
         ( vg_operacao_bol EQ c_new_bol  ).
        MESSAGE 'Modo Edição! Operação não permitida!' TYPE 'W'.
        RETURN.
      ENDIF.

      CALL SCREEN 0120.
    WHEN 'SHOW_MSG'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN   = '100'
            I_SHOW     = 'X'
            I_REPID    = SY-REPID
          IMPORTING
            E_MESSAGEM = WA_MENSAGEM
          TABLES
            IT_MSGS    = IT_MSG_RETURN.

    WHEN OTHERS.

      IF it_saida_0100_01[] IS INITIAL.
        IF wg_cab_boletim_prod-id_boletim IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wg_cab_boletim_prod-id_boletim
            IMPORTING
              output = wg_cab_boletim_prod-id_boletim.


          PERFORM f_free_alv USING: '0120_01', '0120_02'.
          PERFORM f_set_operacao_boletim USING c_view_bol.
          PERFORM f_load_boletim USING wg_cab_boletim_prod-id_boletim.
          LEAVE TO SCREEN 0100.
        ENDIF.
      ENDIF.

  ENDCASE.


ENDMODULE.

MODULE pai_0120 INPUT.

  CASE sy-ucomm.
    WHEN c_atua_bol.
      PERFORM: f_atualizar_saldos,
               f_leave_to_screen.
    WHEN c_enable_tim.
      PERFORM: f_enable_time,
               f_leave_to_screen.
    WHEN c_disable_tim.
      PERFORM: f_disable_time,
               f_leave_to_screen.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'MAX_DET_BOL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.

MODULE pai_0121 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0124  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0124 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'OK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
