*&-------------------------------------------------------------------------------------------------------*
*& Método         : MZSD_APROV_SEM_SALDO_I01 (Include)                                                   *
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
*&      Module  ZM_USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       Acionamento do botão pelo usuário
*----------------------------------------------------------------------*
MODULE zm_user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.

    WHEN cg_sb_9000-t1_pend. "Aprovação NFL Pendentes
      eg_sbsc_9000-subscreen   = cg_sb_9000-scr_pen.
      eg_sbsc_9000-pressed_tab = cg_sb_9000-t1_pend.

    WHEN cg_sb_9000-t2_apre. "Aprovação NFL Aprovadas/Reprovadas
      eg_sbsc_9000-subscreen   = cg_sb_9000-scr_a_r.
      eg_sbsc_9000-pressed_tab = cg_sb_9000-t2_apre.

    WHEN 'REFRESH'. "Atualizar dados
* Seleciona dados para processamento
      PERFORM: zf_seleciona_dados.

    WHEN 'APNFL' OR 'RPNFL'. "Aprovar ou Reprovar Nota FL
      vg_status = sy-ucomm(1).
* Chama a tela Popup de texto do Motivo.
      CALL SCREEN '9310' STARTING AT 7 7 ENDING AT 88 11.
      IF sy-ucomm EQ 'SAVE'.
* Seleciona dados para processamento
        PERFORM: zf_seleciona_dados.

      ENDIF.

    WHEN OTHERS.
* Do nothing
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_9310  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_user_command_9310 INPUT.

  CASE sy-ucomm.
    WHEN 'SAVE'.
* Salva o conteúdo do editor na tabela.
      PERFORM zf_editor_save.
      LEAVE TO SCREEN 0.

    WHEN 'CANC'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
