***ZSDR0084PAI

MODULE exit_screen INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

MODULE user_command_0100 INPUT.
ENDMODULE.


MODULE user_command_0102 INPUT.

  CLEAR ok_code.
  ok_code = sy-ucomm.

  CASE ok_code.

      " 26.09.2022 - RAMON - 19450 -->
    WHEN 'CHCK'.

      PERFORM f_fill_check_box.

      PERFORM f_after_check_box.

      " 26.09.2022 - RAMON - 19450 --<

      " 05.05.2022 - RAMON LIMA - RECLIKE - 76054 --->
    WHEN 'SELECAOPR'.

*      PERFORM f_tela_go_flux
*        USING 'X'
*     CHANGING lv_erro.
*
*      IF lv_erro IS NOT INITIAL.
*        LEAVE TO SCREEN 0.
*      ENDIF.

      " 05.05.2022 - RAMON LIMA - RECLIKE - 76054 ---<

    WHEN 'NEW'.

      IF ( wa_zsdt0158-tipo = 'I' ) OR ( wa_zsdt0158-tipo EQ 'E' ).
        MESSAGE text-004 TYPE 'I'.
      ELSE.

        PERFORM limpa_dados.

        wa_saida-sequencial = '$00000001'.
        wa_zsdt0158-tipo = 'I'.

      ENDIF.

    WHEN 'ALTERA_ZONA_PC'.

      IF ( wa_lfa1_pc-name1 IS NOT INITIAL ).
        PERFORM altera_zona_pc USING wa_lfa1_pc-lifnr.
      ENDIF.

    WHEN 'ALTERA_ZONA_LR'.

      IF ( wa_kna1-name1 IS NOT INITIAL ).
        PERFORM altera_zona_lr.
      ENDIF.

    WHEN 'CHECK_Z1'. "--->CHECK PARA CLONAR O LR PARA O PORTO(Z1)

      IF ( wa_kna1-name1 IS NOT INITIAL ).
        PERFORM preenche_z1.
      ELSE.
        MESSAGE text-007 TYPE 'I'.
      ENDIF.

    WHEN 'CHECK_DCO'.

      IF ( wa_lfa1_pc-name1 IS NOT INITIAL ) AND
         ( wa_makt-maktx    IS NOT INITIAL ) AND
         ( wa_t001w-werks   IS NOT INITIAL ).

        PERFORM preenche_doc.

      ELSE.
        MESSAGE text-011 TYPE 'I'.
      ENDIF.

    WHEN 'INCO1'. "---->VALIDA O TIPO DE FRETE SELECIONADO

      IF ( tinct-inco1 IS NOT INITIAL ).
        PERFORM valida_tp_frete.
      ELSE.
        MESSAGE text-012 TYPE 'I'.
      ENDIF.

    WHEN 'SAVE'.

      " 01.08.2022 - RAMON - 84173 -->
      IF wa_saida-indefinido = 'X'.
        MESSAGE 'Tipo está indefinido' TYPE 'I'.
        EXIT.
      ENDIF.
      " 01.08.2022 - RAMON - 84173 --<

      PERFORM salvar_solicitacao.

    WHEN 'EDITAR'.

      IF ( wa_saida-sequencial IS NOT INITIAL ).

        DATA(_msg_erro) = COND string( WHEN ( wa_zsdt0158_saida-status EQ 'L' ) THEN text-026
                                       WHEN ( wa_zsdt0158-tipo EQ 'I' )         THEN text-028
                                       WHEN ( wa_zsdt0158-tipo EQ 'E' )         THEN text-029 ).

        IF ( _msg_erro IS NOT INITIAL ).
          MESSAGE _msg_erro TYPE 'I'.
          EXIT.
        ENDIF.

        SELECT SINGLE * FROM zsdt0158 INTO @DATA(w_zsdt0158) WHERE sequencial = @wa_saida-sequencial.

        PERFORM pf_check_filial USING w_zsdt0158-filial vl_subrc.

        CHECK vl_subrc IS INITIAL.

        wa_zsdt0158-tipo = 'E'.

      ELSE.
        MESSAGE text-027 TYPE 'I'.
        EXIT.
      ENDIF.

    WHEN 'ATUALIZAR'.

      IF ( wa_saida-sequencial IS INITIAL ).
        MESSAGE text-024 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        PERFORM busca_dados_ov.
      ENDIF.

    WHEN 'ENVIAR'.

      _msg_erro = COND string(
          WHEN ( wa_zsdt0158-tipo EQ 'I' )            THEN text-028
          WHEN ( wa_zsdt0158-tipo EQ 'E' )            THEN text-029
          WHEN ( wa_zsdt0158-tipo IS INITIAL AND wa_zsdt0158_saida-status EQ 'L' ) THEN text-030 ).

      IF ( _msg_erro IS NOT INITIAL ).
        MESSAGE _msg_erro TYPE 'I'. EXIT.
      ELSE.
        PERFORM envia_solicitacao.
        PERFORM busca_dados_ov.
      ENDIF.

    WHEN 'LISTAR'.

      PERFORM busca_solicitacoes.

    WHEN 'ON_BTN_OV'.

      SUBMIT zsdr0022 WITH psolici EQ wa_saida-nro_sol_ov.

    WHEN 'ON_BTN_PD'.

      IF wa_saida-ebeln IS NOT INITIAL.

        CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
          EXPORTING
            i_ebeln = wa_saida-ebeln.

      ENDIF.

    WHEN 'DELETAR'.

      DATA: answ_del TYPE c.

      IF ( wa_t001w-name1 IS INITIAL ).
        MESSAGE text-035 TYPE 'I'.
        EXIT.
      ENDIF.

      IF ( wa_zsdt0158_saida-status EQ 'L' ).
        MESSAGE text-035 TYPE 'I'.
        EXIT.
      ELSEIF ( wa_zsdt0158_saida-status EQ 'G' ).

        CLEAR w_zsdt0158.
        SELECT SINGLE * FROM zsdt0158 INTO w_zsdt0158 WHERE sequencial = wa_saida-sequencial.
        PERFORM pf_check_filial USING w_zsdt0158-filial vl_subrc.
        CHECK vl_subrc IS INITIAL.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar      = 'Confirmação'
            text_question = 'Deseja eliminar a solicitação?'
            text_button_1 = 'Sim'
            icon_button_1 = 'ICON_CHECKED'
            text_button_2 = 'Nao'
            icon_button_2 = 'ICON_CANCEL'
            popup_type    = 'ICON_MESSAGE_ERROR'
          IMPORTING
            answer        = answ_del.

        IF ( answ_del EQ 1 ).

          IF ( it_id_compra[] IS NOT INITIAL ).
            DATA(_id_compra) = it_id_compra[ 1 ].

            DELETE FROM zsdt0158_id
                WHERE sequencial EQ wa_saida-sequencial
                " 03.05.2022 - RAMON LIMA - RECLIKE -->
                  "AND id_compra  EQ _id_compra-id_compra.
                  AND id_compra  EQ _id_compra-nu_compra.
            " 03.05.2022 - RAMON LIMA - RECLIKE --<

            "03.05.2022 - RAMON LIMA - RECLIKE -->
*            UPDATE zsdt0187 SET ov_ped = ''
*                WHERE id_compra = _id_compra-id_compra.
            "03.05.2022 - RAMON LIMA - RECLIKE -->

          ENDIF.

          DELETE FROM zsdt0158 WHERE sequencial EQ wa_saida-sequencial.

          MESSAGE text-037 TYPE 'S'.

          PERFORM limpa_dados.

        ENDIF.

      ENDIF.

    WHEN 'BUSCA_ID'.

      PERFORM busca_id_compra.

    WHEN 'LIMPA_ID'.

      CLEAR: it_id_compra[].
      obj_alv->refresh_table_display( ).

    WHEN 'PR_AFIXAR'.

      DATA: tl_justificativa TYPE catsxt_longtext_itab.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = 'Justificativa para Produto à Fixar'
        CHANGING
          ch_text  = tl_justificativa[].

      IF ( tl_justificativa[] IS NOT INITIAL ).

        wa_zsdt0158_saida-obs_pr_produto  = tl_justificativa[ 1 ].
        pr_produto_a                      = abap_true.
        wa_id_compra-unidade              = 'KG'.

      ELSE.
        MESSAGE 'Favor, informar justificativa para produto à fixar!' TYPE 'I'.
      ENDIF.

    WHEN 'PR_FIXO'.

      CLEAR: pr_produto_f, tl_justificativa[].

    WHEN 'QUANT'.

      CHECK ( wa_saida-nro_sol_ov IS NOT INITIAL ).

      PERFORM busca_preco_pauta.

      wa_zsdt0158_qt-vlr_pauta = wa_konp-kbetr.
      wa_zsdt0158_qt-unidade   = 'KG'.

      CALL SCREEN 0104 STARTING AT 01 01 ENDING AT 48 06.
*** CS2020000143 inicio
    WHEN 'MESSAGE'.

      DATA: it_texto TYPE catsxt_longtext_itab,
            wa_texto TYPE LINE OF catsxt_longtext_itab,
            v_cont   TYPE sy-tabix,
            v_pos    TYPE sy-tabix,
            v_line   TYPE sy-tabix.

      IF  wa_zsdt0158_saida-mensagem  IS NOT INITIAL.

        CLEAR: it_texto[], v_pos , v_cont,  v_line.

        "REPLACE ALL OCCURRENCES OF REGEX '[^[:print:]]'  IN wa_zsdt0158_saida-mensagem WITH ' '.

        v_cont = strlen( wa_zsdt0158_saida-mensagem ).

        WHILE v_pos < v_cont.
          v_line = v_cont - v_pos.

          IF v_line >= 72.
            v_line = 72.
          ENDIF.

          wa_texto = wa_zsdt0158_saida-mensagem+v_pos(v_line).
          ADD 72 TO v_pos.
          IF wa_texto IS NOT INITIAL.
            APPEND wa_texto TO it_texto.
          ENDIF.
          CLEAR: wa_texto.
        ENDWHILE.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Texto de Observação'
            im_display_mode = 'X'
          CHANGING
            ch_text         = it_texto[].
      ENDIF.

*** CS2020000143 fim
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  HELP_NRO_SOL_OV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_nro_sol_ov INPUT.
  PERFORM f4_busca_solicitacoes.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  MAIN_TAB_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
MODULE main_tab_active_tab_get INPUT.

  IF ( sy-ucomm = c_main_tab-tab1 ) OR ( sy-ucomm = c_main_tab-tab2 ).

    i_main_tab-pressed_tab  = COND #( WHEN sy-ucomm = c_main_tab-tab1 THEN c_main_tab-tab1
                                      WHEN sy-ucomm = c_main_tab-tab2 THEN c_main_tab-tab2  ).
    i_main_tab-subscreen    = COND #( WHEN sy-ucomm = c_main_tab-tab1 THEN '0102'
                                      WHEN sy-ucomm = c_main_tab-tab2 THEN '0103' ).
    PERFORM limpa_dados.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.
  CLEAR ok_code.

  ok_code = sy-ucomm.

  CASE ok_code.

    WHEN 'NEW'. "--->CRIAR NOVO RETISTRO/SOLICITAÇÃO

      IF ( wa_zsdt0158-tipo = 'I' ) OR ( wa_zsdt0158-tipo EQ 'E' ).
        MESSAGE text-004 TYPE 'I'. EXIT.
      ELSE.

        PERFORM limpa_dados.

        wa_saida-sequencial = '$00000001'.
        wa_zsdt0158-tipo    = 'I'.

      ENDIF.

    WHEN 'ALTERA_ZONA_PC'.

      IF ( wa_lfa1_pc-name1 IS NOT INITIAL ).
        PERFORM altera_zona_pc USING wa_lfa1_pc-lifnr.
      ENDIF.

    WHEN 'ALTERA_ZONA_LR'.

      IF ( wa_kna1-name1 IS NOT INITIAL ).
        PERFORM altera_zona_lr.
      ENDIF.

    WHEN 'SAVE'.

      "PERFORM organiza_dados_pt.

    WHEN 'EDITAR'.

      IF ( wa_saida-nro_sol_ov IS NOT INITIAL ).

        _msg_erro = COND string(
            WHEN ( wa_zsdt0158_saida-status EQ 'L' )    THEN text-026
            WHEN ( wa_zsdt0158-tipo EQ 'I' )            THEN text-028
            WHEN ( wa_zsdt0158-tipo EQ 'E' )            THEN text-029 ).

        IF ( _msg_erro IS NOT INITIAL ).
          MESSAGE _msg_erro TYPE 'I'. EXIT.
        ELSEIF ( wa_zsdt0158-tipo IS INITIAL ).
          wa_zsdt0158-tipo = 'E'.
        ENDIF.

      ELSE.
        MESSAGE text-027 TYPE 'I'.
        EXIT.
      ENDIF.

    WHEN 'ATUALIZAR'.

      IF ( wa_saida-sequencial IS INITIAL ).
        MESSAGE text-024 TYPE 'I'. EXIT.
      ELSE.
        PERFORM busca_dados_ov.
      ENDIF.

    WHEN 'ENVIAR'.

      _msg_erro = COND string(
        WHEN ( wa_zsdt0158-tipo EQ 'I' )            THEN text-028
        WHEN ( wa_zsdt0158-tipo EQ 'E' )            THEN text-029
        WHEN ( wa_zsdt0158-tipo IS INITIAL AND wa_zsdt0158_saida-status EQ 'L' ) THEN text-030 ).

      IF ( _msg_erro IS NOT INITIAL ).
        MESSAGE _msg_erro TYPE 'I'. EXIT.
      ELSE.
        PERFORM envia_solicitacao.
        PERFORM busca_dados_ov.
      ENDIF.

    WHEN 'DELETAR'.

      PERFORM deleta_solicitacao.

    WHEN 'LISTAR'.

      PERFORM busca_solicitacoes.

    WHEN 'ON_BTN_OV'.

      SUBMIT zsdr0022 WITH psolici EQ wa_saida-nro_sol_ov.


  ENDCASE.

ENDMODULE.


MODULE user_command_0104 INPUT.

  IF ( sy-ucomm = 'SAVE' ).

    IF wa_zsdt0158_saida-tp_solicitacao = 'O'.
      PERFORM altera_quantidade_ov.
    ELSE.
      PERFORM altera_quantidade_ped.
    ENDIF.

  ELSE.

    LEAVE TO SCREEN 0.

  ENDIF.

ENDMODULE.
