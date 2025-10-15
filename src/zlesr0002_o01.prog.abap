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
*& Module ZM_STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zm_status_0100 OUTPUT.

  DATA: tl_exclud TYPE TABLE OF syucomm.

  btn_pesq = TEXT-009. "Pesquisar Frete

  IF tg_frete[] IS INITIAL.
    APPEND 'SAVE'   TO tl_exclud.
    APPEND 'SHOW'   TO tl_exclud.
    APPEND 'EDIT'   TO tl_exclud.
    APPEND 'CANCEL' TO tl_exclud.
    APPEND 'FINAL'  TO tl_exclud.
    APPEND 'QUANT'  TO tl_exclud.
    APPEND 'MTVO'   TO tl_exclud.

  ELSE.
    CLEAR tl_exclud[].

  ENDIF.

  SET PF-STATUS 'ZLESPF_PRT_SRV' EXCLUDING tl_exclud.
  SET TITLEBAR 'ZLES_PFS_TITLE'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_0102 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zm_status_0102 OUTPUT.

* Exibe os dados selecionado da pesquisa.
  PERFORM zf_show_data_frete.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zm_status_0200 OUTPUT.

  CLEAR tl_exclud.

  APPEND 'SHOW'   TO tl_exclud.
  APPEND 'EDIT'   TO tl_exclud.
  APPEND 'CANCEL' TO tl_exclud.
  APPEND 'FINAL'  TO tl_exclud.
  APPEND 'NEW'    TO tl_exclud.
  APPEND 'QUANT'  TO tl_exclud.
  APPEND 'GRPMRC' TO tl_exclud.

  IF *zlest0255-status EQ '001'.
    APPEND 'MTVO' TO tl_exclud.

  ENDIF.
* Verifica campos da tela de Manutenção do Frete.
  PERFORM zf_chek_fields_scr_0200.
* Cria o editor da Observação do Frete.
  PERFORM zf_tela_edit_observacao.
  CASE sy-ucomm.
    WHEN 'NEW'.   "Frete Novo
      APPEND 'MTVO' TO tl_exclud.
      DATA(vl_texto) = 'Criar    '.

    WHEN 'SHOW'.   "Exibir Frete
* BPO da tela 0200 botão Exibir.
      PERFORM zf_status_0200_show TABLES   tl_exclud
                                  CHANGING vl_texto.

    WHEN 'EDIT'.   "Editar Frete
* BPO da tela 0200 botão Editar.
      PERFORM zf_status_0200_edit CHANGING vl_texto.

    WHEN 'CANCEL'. "Cancelar Frete
      vl_texto = 'Cancelar'.

    WHEN 'FINAL'.  "Finalizar Frete
      vl_texto = 'Finalizar'.

    WHEN 'ENTE'.   "Enter na tela 200
      CASE gv_acao.
        WHEN 'SHOW'.   "Exibir Frete
* BPO da tela 0200 botão Exibir.
          PERFORM zf_status_0200_show TABLES tl_exclud
                                      USING  vl_texto.

        WHEN 'EDIT'.   "Editar Frete
* BPO da tela 0200 botão Editar.
          PERFORM zf_status_0200_edit CHANGING vl_texto.

        WHEN 'NEW'.
          APPEND 'MTVO' TO tl_exclud.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

  SET PF-STATUS 'ZLESPF_PRT_SRV' EXCLUDING tl_exclud.
  SET TITLEBAR 'ZLES_PFS_TITLE2' WITH vl_texto.
  CLEAR tl_exclud.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zm_status_0300 OUTPUT.

  CASE sy-ucomm.
    WHEN 'CANCEL'. "Cancelar Frete
      IF *zlest0255-status NE '001'.
        LEAVE TO SCREEN 0.

      ENDIF.

      vl_texto = 'Cancelar '.

    WHEN 'FINAL'.  "Finalizar Frete
      IF *zlest0255-status NE '001'.
        LEAVE TO SCREEN 0.

      ENDIF.

      vl_texto = 'Finalizar'.

    WHEN 'MTVO'.   "Motivo do Cancelamento/Finalização do Frete de Terceiro
      vl_texto = 'Vis. Mtvo'.
      CLEAR tl_exclud.
      APPEND 'SAVE' TO tl_exclud.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

  IF sy-ucomm NE 'MTVO'.   "Motivo do Cancelamento/Finalização do Frete de Terceiro
    gv_ucomm = gv_acao.
    gv_acao  = sy-ucomm.

  ENDIF.
* Cria o editor do Cancela/Finaliza Frete de Terceiro.
  PERFORM zf_tela_edit_canc_final USING sy-ucomm.
* Carrega o texto Observação do Frete lido da função READ_TEXT.
  CALL METHOD gcl_canc_final->set_text_as_r3table
    EXPORTING
      table = tg_editor2.

  SET PF-STATUS 'ZLESPF_CAN_FIN' EXCLUDING tl_exclud.
  SET TITLEBAR 'ZLES_PFS_TITLE2' WITH vl_texto.

ENDMODULE.
