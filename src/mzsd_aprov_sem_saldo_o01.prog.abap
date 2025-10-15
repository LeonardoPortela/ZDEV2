*&-------------------------------------------------------------------------------------------------------*
*& Método         : MZSD_APROV_SEM_SALDO_O01 (Include)                                                   *
*& Chamado        : USER STORY 169312                                                                    *
*& Data           : 21/03/2025                                                                           *
*& Especificado   : Leonardo Portela                                                                     *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 21/03/2025|DEVK9A1XAW |NSEGATIN       | Aprovar NFL sem Saldo a Vincular. Desenvolvimento inicial.    *
*--------------------------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*& Carrega a Barra de Ferramenta, Título da tela e demias configurações
*&---------------------------------------------------------------------*
MODULE zm_status_9000 OUTPUT.

  SET PF-STATUS 'ZSDPFST_9000'.
  SET TITLEBAR 'ZSDTIT_9000'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_SBSC_SET_ACTIVE_TAB OUTPUT
*&---------------------------------------------------------------------*
*& Configuração de ativação da Guis de tela.
*&---------------------------------------------------------------------*
MODULE zm_sbsc_set_active_tab OUTPUT.

* Alimenta o Controle de tela de guias com a guia ativada.
  gts_9000-activetab = eg_sbsc_9000-pressed_tab.
* Verifica qual guia de Controle de tela de guias foi acionada.
  CASE eg_sbsc_9000-pressed_tab.
    WHEN cg_sb_9000-t1_pend. "Lista NFL Pendente
      eg_sbsc_9000-subscreen = cg_sb_9000-scr_pen.

    WHEN cg_sb_9000-t2_apre. "Lista NFL Aprovadas/Reprovadas
      eg_sbsc_9000-subscreen = cg_sb_9000-scr_a_r.

    WHEN OTHERS.
* Do nothing.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_SELECIONA_MONTA_DADOS OUTPUT
*&---------------------------------------------------------------------*
*& Seleciona Dados, Exibe e monta o Editor do Motivo
*&---------------------------------------------------------------------*
MODULE zm_seleciona_monta_dados OUTPUT.

* Exibe dados selecionados processados
  PERFORM zf_exibe_dados.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_9310 OUTPUT
*&---------------------------------------------------------------------*
*& Carrega a Barra de Ferramenta e Título da tela e demias configurações
*&---------------------------------------------------------------------*
MODULE zm_status_9310 OUTPUT.

  SET PF-STATUS 'ZSDPFST_9310'.
  SET TITLEBAR 'ZSDTIT_9310'.
* Prepara editor de texto para Motivo Aprovado/Reprovado.
  PERFORM zf_textedit_motivo TABLES tg_vinc_f_apv_pen
                              USING 'CUSTC_TXTEDITOR'
                                    cl_gui_textedit=>false
                                    cl_gui_textedit=>true
                                    cl_gui_textedit=>true
                                    79.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
*& Carrega a Barra de Ferramenta, Título da tela e demias configurações
*&---------------------------------------------------------------------*
MODULE zm_status_9400 OUTPUT.

* Prepara editor de texto para Motivo Aprovado/Reprovado.
  PERFORM zf_textedit_motivo TABLES tg_vinc_f_apv_a_r
                              USING 'CUSTC_TXTEDITOR2'
                                    cl_gui_textedit=>true
                                    cl_gui_textedit=>false
                                    cl_gui_textedit=>false
                                    57.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_9300 OUTPUT
*&---------------------------------------------------------------------*
*& Carrega a Barra de Ferramenta, Título da tela e demias configurações
*&---------------------------------------------------------------------*
MODULE zm_status_9300 OUTPUT.

* Validação do usuário como Aprovador.
  IF tg_aprovador_buk IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'BTN'.
        screen-input = 0.
        MODIFY SCREEN.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDMODULE.
