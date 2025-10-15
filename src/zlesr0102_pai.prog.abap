*&---------------------------------------------------------------------*
*&  Include           ZLESR0102_PAI
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.
  CASE  ok-code.
    WHEN 'CK_FAT_ECC'. "Faturamento Contingencia
      PERFORM f_check_faturamento_ecc.
    WHEN 'RECU'.
      PERFORM f_repare_docs_romaneio_sel.

      REFRESH it_saida.
      CALL METHOD cl_grid->refresh_table_display.
      PERFORM: f_seleciona_dados, " Form seleciona dados
               f_saida. " Form de saida
      CALL METHOD cl_grid->refresh_table_display.

    WHEN 'CARTA'.
      PERFORM f_call_carta_correcao.
    WHEN 'EX_TRA'.
      PERFORM f_call_dados_transp.
    WHEN 'DACTE'.
      PERFORM f_call_danfe_dacte USING '2'.
    WHEN 'DANFE'.
      PERFORM f_call_danfe_dacte USING '1'.
    WHEN 'EST_CTE'.
      PERFORM f_call_estorno_cte.
    WHEN 'EST_NFE'.
      PERFORM f_call_estorno_nfe.
    WHEN 'REFRESH'.
      REFRESH it_saida.
      CALL METHOD cl_grid->refresh_table_display.
      PERFORM: f_seleciona_dados, " Form seleciona dados
               f_saida. " Form de saida
      CALL METHOD cl_grid->refresh_table_display.
**      Ajuste - CS2020000637 - 11/09/2020 - PL/CSB - INICIO -
    WHEN 'DOWN'.
      PERFORM f_call_files USING 'X' CHANGING pdf_result.
    WHEN 'EMAIL'.
      PERFORM f_call_email.
**      Ajuste - CS2020000637 - 11/09/2020 - PL/CSB - FIM

*-CS2021000117 - 28.04.2021 - JT - inicio
    WHEN 'ORD_CARREG'.
      PERFORM f_ordem_carregamento.
*-CS2021000117 - 28.04.2021 - JT - inicio

*-CS2021000656 - 14.05.2021 - JT - inicio
    WHEN 'SELO'.
      PERFORM f_imprimir_selo.
*-CS2021000656 - 14.05.2021 - JT - inicio
* Início - CS0979621 - RSI 27.05.2022

*-CS2021000218-16.11.2022-#90706-JT-inicio
    WHEN 'RA_ASSINA'.
      PERFORM f_imprimir_ra_assinada.
*-CS2021000218-16.11.2022-#90706-JT-fim

*-CS2023000189-26.05.2023-#108752-JT-inicio
    WHEN 'ROM_ALGD'.
      PERFORM f_impr_romaneio_algodao.
*-CS2023000189-26.05.2023-#108752-JT-fim

    WHEN 'NOTA_PARC'.
*      PERFORM f_call_nota_parceiro. "Include ZLESR0102_PAI em uso atualmente
      CLEAR: tl_rows[].
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.
      IF lines( tl_rows[] ) NE 1.
        MESSAGE 'Selecione uma linha para exibir Nota Parceiro' TYPE 'I'.
        EXIT.
      ENDIF.
      CLEAR: sl_rows, wa_saida.
      READ TABLE tl_rows INTO sl_rows INDEX 1.
      READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.
      CHECK sy-subrc = 0.
*    -----------------------------------------
*     Chamar programa para listar Nota Parceiro
*    -----------------------------------------
      SUBMIT zsdr0138
            WITH p_vbeln = wa_saida-fatura AND RETURN.
* FIM - CS0979621 - RSI 27.05.2022
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

MODULE user_command_exit INPUT.
  CASE ok-code.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'UP'.
      SET SCREEN 0.
    WHEN 'CANCEL'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT

MODULE user_command_0200 INPUT.
  CASE  sy-ucomm.
    WHEN 'SEARCH'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_mot-motorista
        IMPORTING
          output = wa_mot-motorista.

      SELECT SINGLE la~lifnr la~name1 la~stcd2 lb~zsabe la~stcd3 lb~eikto
        FROM lfa1 AS la
        INNER JOIN lfb1 AS lb ON lb~lifnr = la~lifnr AND lb~bukrs = p_bukrs
      INTO wa_mot
      WHERE la~lifnr = wa_mot-motorista.
      IF sy-subrc NE 0.
        CLEAR wa_mot.
      ENDIF.
    WHEN 'ALTM'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = 'Confirma a alteração de Motorista'
          text_button_1         = 'Sim'(100)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Não'(101)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
          start_column          = 25
          start_row             = 6
        IMPORTING
          answer                = w_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF w_answer = '1'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_mot-motorista
          IMPORTING
            output = wa_mot-motorista.
        UPDATE zsdt0001 SET motorista = wa_mot-motorista
         WHERE ch_referencia = wa_saida-ch_referencia.
      ENDIF.

    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

MODULE user_command_0300 INPUT.
  CASE sy-ucomm .
    WHEN 'SEARCH'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ag_frete-transpor2
        IMPORTING
          output = wa_ag_frete-transpor2.

      SELECT SINGLE *
        FROM lfa1 INTO @DATA(wa_agente)
       WHERE lifnr = @wa_ag_frete-transpor2.

      CLEAR txt_correc.
      IF sy-subrc = 0.
        wa_ag_frete-name2      = wa_agente-name1.
        wa_ag_frete-cnpj2      = wa_agente-stcd1.
        wa_ag_frete-inscr2     = wa_agente-stcd3.

        REFRESH: tg_editor.

        CONCATENATE 'Considerar dados corretos do transportador:CNPJ' wa_ag_frete-cnpj2
        'Razão Social' wa_ag_frete-name2
        'Inscr.Est.' wa_ag_frete-inscr2
        INTO txt_correc SEPARATED BY space.

        CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
        wl_cont = strlen( txt_correc ).
        wl_cont_aux = wl_cont / 72.

        DO.
          MOVE: txt_correc+wl_cont_aux2(72) TO wg_editor-line.
          ADD 72 TO wl_cont_aux2.
          APPEND wg_editor TO tg_editor.

          IF wl_cont_aux2 GT wl_cont.
            EXIT.

          ENDIF.
        ENDDO.

        CALL METHOD obg_descbox->delete_text.

        CALL METHOD obg_descbox->set_text_as_r3table
          EXPORTING
            table = tg_editor.
        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 1.

      ENDIF.
    WHEN 'SAIR'.
      REFRESH: tg_editor.
      CLEAR wa_ag_frete.
      LEAVE TO SCREEN 0.
    WHEN 'BTNOK'.
      IF txt_correc IS NOT INITIAL.
        PERFORM f_lanc_carta_correcao.
        REFRESH: tg_editor.
        CLEAR wa_ag_frete.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT

MODULE user_command_0200_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'ENVIAR'.
      IF s_email IS INITIAL.
        MESSAGE 'Informar um e-mail para o destinatário.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        PERFORM gera_email.
*----CS2021000508 - 07.06.2021 - JT - inicio
*       MESSAGE 'Email(s) enviado(s) com sucesso!' TYPE 'I' DISPLAY LIKE 'S'.
*----CS2021000508 - 07.06.2021 - JT - fim
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.
