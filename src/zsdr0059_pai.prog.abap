*&---------------------------------------------------------------------*
*&  Include           ZSDR0059_PAI
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.

  DATA: w_restr(1) TYPE c VALUE 0.

  CASE sy-ucomm.
    WHEN 'PROC_MAN'.

      SUBMIT zfis38 WITH p_exec = 'X' AND RETURN.

      MESSAGE 'Processamento realizado!' TYPE 'S'.

    WHEN 'MANIFESTO'.

      CLEAR it_sel_rows.
      CALL METHOD obj_alv_0100->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      CHECK it_sel_rows[] IS NOT INITIAL.

      IF lines( it_sel_rows[] ) = 0.
        MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      CLEAR: zsdt0127, wa_saida_0100.

      LOOP AT it_sel_rows INTO wa_sel_rows.

        READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

        CHECK sy-subrc = 0.

        IF wa_saida_0100-model NE '55' AND ( p_mnf1 = abap_true    OR
                                             p_mnf2 = abap_true    OR
                                             p_mnf3 = abap_true    OR
                                             p_mnf4 = abap_true    OR
                                             p_mnf5 = abap_true ). "*-CS2022000243-#76365-26.04.2022-JT-inicio
          MESSAGE |Manifesto só permitido para NF-e! Chave: { wa_saida_0100-chave } | TYPE 'S'.
          RETURN.
        ENDIF.
*---> CS0993761 / IR096524
        IF wa_saida_0100-migo EQ space AND  wa_saida_0100-miro EQ space.
          w_restr = 3.
        ELSE.
          IF wa_saida_0100-migo EQ space.
            w_restr = 1.
          ENDIF.
          IF  wa_saida_0100-miro EQ space.
            w_restr = 2.
          ENDIF.
        ENDIF.
*<--- CS0993761 / IR096524
*        IF wa_saida_0100-migo IS INITIAL AND wa_saida_0100-miro IS INITIAL AND  wa_saida_0100-docnum IS INITIAL AND
*            ( wa_saida_0100-cd_operacao EQ '210240' OR wa_saida_0100-cd_operacao EQ '210220' ).
*          MESSAGE |Não permitido realizar o manifesto | TYPE 'S'.
*          RETURN.
*        ENDIF.

*-CS2022000243-#76365-26.04.2022-JT-inicio
*       IF p_mnf6 = abap_true AND wa_saida_0100-docnum IS NOT INITIAL.
*         MESSAGE |Não permitido realizar o manifesto | TYPE 'S'.
*         RETURN.
*       ENDIF.
*-CS2022000243-#76365-26.04.2022-JT-fim

      ENDLOOP.

      EXPORT w_restr TO MEMORY ID 'Restric'.

      CLEAR: vg_timer_ativo.

      IF ob_timer IS NOT INITIAL.
        CALL METHOD ob_timer->cancel.
      ENDIF.

      CLEAR: it_saida_0105[].

      IF lines( it_sel_rows[] ) = 1.
        PERFORM f_selecionar_manifestos USING wa_saida_0100.
      ENDIF.

      CALL SCREEN 0105 STARTING AT 02 02 ENDING AT 154 20.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

MODULE user_command_0105 INPUT.
  CASE sy-ucomm.
    WHEN 'GRAVAR'.
      PERFORM f_gravar_manifesto.
    WHEN 'EMITIR'.
      PERFORM f_emitir_manifesto.
      CALL METHOD obj_alv_0105->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
      LEAVE TO SCREEN 0105.

*#127333 - 04.12.2023 - JT - inicio
    WHEN 'ANULAR_REJECT'.
      PERFORM f_anular_rejeicao_manifesto.
      CALL METHOD obj_alv_0105->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
      LEAVE TO SCREEN 0105.

    WHEN 'ANULAR_CONF_OP'.
      PERFORM f_anular_conf_oper_manifesto.

      CALL METHOD obj_alv_0105->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
      LEAVE TO SCREEN 0105.

*#127333 - 04.12.2023 - JT - fim

    WHEN 'REFRESH'.
      CLEAR: vg_row, vg_col.

      CALL METHOD obj_alv_0105->get_current_cell
        IMPORTING
          es_row_id = vg_row
          es_col_id = vg_col.

      PERFORM f_selecionar_manifestos USING wa_saida_0100.

*      IF vg_timer_ativo IS INITIAL.
*        CALL METHOD ob_timer->run.
*        vg_timer_ativo = 'X'.
*      ENDIF.

      CALL METHOD obj_alv_0105->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      LEAVE TO SCREEN 0105.

    WHEN 'FECHAR'.
      LEAVE TO SCREEN 0.

    WHEN 'AUTH_MANU'.
      PERFORM fm_modifica_evento_doc.
  ENDCASE.
ENDMODULE.
