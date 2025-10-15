*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 10/10/2025                                              &*
*& Descrição: Serviço de Frete de Terceiros                           &*
*& Transação: ZLES0224 (Prest. Serv. Frete - Faturar)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |10/10/2025 |Desenvolvimento Inicial.       &*
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
*****          CLEAR: *zlest0255, *kna1, gv_butxt, gv_wgbez60, gv_maktx, gv_text1, gv_ktext, gv_zone_pc, gv_zone_lr, tg_editor.
          LEAVE TO SCREEN 100.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

    WHEN 'CANC'. "Cancelar
      CASE sy-dynnr.
        WHEN 100. "Prestação de Serviço Frete de Terceiros
          LEAVE PROGRAM.

        WHEN 300. "Popup de texto do Motivo de Cancelar/Finalizar Frete Teceiro
*****          IF gv_acao EQ 'MTVO'. "Visualizar Motivo de Cancelar/Finalizar Frete Teceiro
*****            IF gv_ucomm IS INITIAL.
*****              sy-ucomm = gv_acao.
*****
*****            ELSE.
*****              sy-ucomm = gv_acao = gv_ucomm.
*****
*****            ENDIF.
*****
*****            CLEAR: tg_editor2, gv_ucomm.
*****
*****          ELSE.
*****            CLEAR: *zlest0255, tg_editor2.
*****            sy-ucomm = gv_acao.
*****
*****          ENDIF.

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
    WHEN 'PESQ'.   "Pesquisa
* Busca os dados da pesquisa do frete faturado.
      PERFORM zf_select_data_frete_fat.

    WHEN 'NEW'.    "Novo Frete Faturado
      gv_acao = sy-ucomm.
      CLEAR *zlest0257.
* Chama a Tela de Criação e Manutenção do frete faturado.
      CALL SCREEN '0200'.

    WHEN 'SHOW'.   "Exibir Frete Faturado
      gv_acao = sy-ucomm.
** Exibir os dados da pesquisa do frete fraturado em tela.
*      PERFORM zf_show_data_frete_fat_scr USING sy-ucomm.

    WHEN 'EDIT'.   "Editar Frete Faturado
      gv_acao = sy-ucomm.
** Edita os dados da pesquisa do frete faturado em tela.
*      PERFORM zf_edit_data_frete_fat_scr USING sy-ucomm.

    WHEN 'CANCEL'. "Cancelar Frete Faturado
      gv_acao = sy-ucomm.
** Cancelamento do Frete Faturado.
*      PERFORM zf_cancel_data_frete_fat.

    WHEN 'SAVE'.   "Salvar Frete Faturado


    WHEN OTHERS.
*   Do nothing
  ENDCASE.
ENDMODULE.
