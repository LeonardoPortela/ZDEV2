*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 22/09/2025                                              &*
*& Descrição: Serviço de Frete de Terceiros                           &*
*& Transação: ZLES0223 (Prest. Serv. Frete Terceiros)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |22/09/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_user_command_0100_exit INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR "Voltar
         'EEND'.   "Finalizar
      CASE sy-dynnr.
        WHEN 100. "Prestação de Serviço Frete de Terceiros
          LEAVE PROGRAM.

        WHEN 200. "Tela de Criação e Manutenção do frete
          CLEAR: *zlest0255, *kna1, gv_butxt, gv_wgbez60, gv_maktx, gv_text1, gv_ktext, gv_zone_pc, gv_zone_lr, tg_editor.
          LEAVE TO SCREEN 100.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

    WHEN 'CANC'. "Cancelar
      CASE sy-dynnr.
        WHEN 100. "Prestação de Serviço Frete de Terceiros
          LEAVE PROGRAM.

        WHEN 300. "Popup de texto do Motivo de Cancelar/Finalizar Frete Teceiro
          IF gv_acao EQ 'MTVO'. "Visualizar Motivo de Cancelar/Finalizar Frete Teceiro
            IF gv_ucomm IS INITIAL.
              sy-ucomm = gv_acao.

            ELSE.
              sy-ucomm = gv_acao = gv_ucomm.

            ENDIF.

            CLEAR: tg_editor2, gv_ucomm.

          ELSE.
            CLEAR: *zlest0255, tg_editor2.
            sy-ucomm = gv_acao.

          ENDIF.

          LEAVE TO SCREEN 0.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'PESQ'.   "Pesquisa Frete
* Busca os dados da pesquisa do frete de terceiro.
      PERFORM zf_select_data_frete.

    WHEN 'NEW'.    "Novo Frete
      gv_acao = sy-ucomm.
      CLEAR *zlest0255.
* Chama a Tela de Criação e Manutenção do frete.
      CALL SCREEN '0200'.

    WHEN 'SHOW'.   "Exibir Frete
      gv_acao = sy-ucomm.
* Exibir os dados da pesquisa do frete de terceiro em tela.
      PERFORM zf_show_data_frete_scr USING sy-ucomm.

    WHEN 'EDIT'.   "Editar Frete
      gv_acao = sy-ucomm.
* Edita os dados da pesquisa do frete de terceiro em tela.
      PERFORM zf_edit_data_frete_scr USING sy-ucomm.

    WHEN 'CANCEL' OR "Cancelar Frete
         'FINAL'.    "Finalizar Frete
      gv_acao = sy-ucomm.
* Cancelamento/Finalização do Frete de Terceiro.
      PERFORM zf_cancel_final_data_frete.

    WHEN 'SAVE'.   "Salvar Frete / Salvar Texto do Motivo de Cancelar/Finalizar Frete Teceiro
      CASE sy-dynnr.
        WHEN 0200. "Tela de Criação e Manutenção do frete
* Verifica campos da tela de Manutenção do Frete.
          PERFORM zf_chek_fields_scr_0200.
* Verifica se ocorreu algum erro de validação na tela 200.
          IF gv_erro IS INITIAL.
            CLEAR: *zlest0255, *kna1, gv_wgbez60, gv_maktx, gv_text1, gv_ktext, gv_zone_pc, gv_zone_lr, sy-ucomm, gv_acao, tg_editor.
            LEAVE TO SCREEN 100.

          ENDIF.

        WHEN 0300. "Popup de texto do Motivo de Cancelar/Finalizar Frete Teceiro
* Salvar texto do motivo de Cancelamento/Finalização do Frete de Terceiro.
          PERFORM zf_salva_texto_canc_final USING gv_acao
                                                  *zlest0255.
          CLEAR: *zlest0255, tg_editor2, gv_acao.
          LEAVE TO SCREEN 0.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

    WHEN 'QUANT'.  "Alterar Quantidade
* Popup de Alteração de Quantidade do Frete.
      PERFORM zf_altera_qtd_frete_popup.

    WHEN 'MTVO'.   "Visualizar Motivo de Cancelamento/Finalização
      CASE sy-dynnr.
        WHEN 0100. "Prestação de Serviço Frete de Terceiros
          gv_acao = sy-ucomm.

        WHEN 0200. "Tela de Criação e Manutenção do frete
          gv_ucomm = gv_acao.
          gv_acao = sy-ucomm.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

* Visualizar Motivo de Cancelamento/Finalização do Frete de Terceiro.
      PERFORM zf_motivo_data_frete.

    WHEN 'GRPMRC'. "Parâmetro Grp. Mercadoria
* Chama a parametrização do Grupo de Mercadoria.
      PERFORM zf_param_grp_mercadoria.

    WHEN 'ORVD'.   "Gerar Ordem de Venda
* Verifica campos da tela de Manutenção do Frete.
      PERFORM zf_chek_fields_scr_0200.
* Geração da ordem de venda do Frete de Terceiro.
      PERFORM zf_gera_ordem_venda USING *zlest0255.
      sy-ucomm = gv_acao.

    WHEN 'PICK'.    "Duplo Clic tela 200
      IF sy-cucol BETWEEN 161 AND 170 AND
         sy-curow EQ 3.
        SET PARAMETER ID 'AUN' FIELD *zlest0255-vbeln.
* Exibir ordens do cliente
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        SET PARAMETER ID 'AUN' FIELD space.
        sy-ucomm = gv_acao.

      ENDIF.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_SRH_TP_FILIAL_TARSP  INPUT
*&---------------------------------------------------------------------*
*       Ajuda de pesquisa do Filial de Transporte
*----------------------------------------------------------------------*
MODULE zm_srh_tp_filial_tarsp INPUT.

* Ajuda de pesquisa do Filial de Transporte.
  PERFORM zf_srh_tp_filial_tarsp.

ENDMODULE.
