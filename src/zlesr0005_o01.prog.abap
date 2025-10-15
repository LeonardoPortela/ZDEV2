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
*& Module ZM_STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zm_status_0100 OUTPUT.

  DATA: tl_exclud TYPE TABLE OF syucomm.

  btn_pesq = TEXT-011. "Pesquisar

  IF tg_frete_fat[] IS INITIAL.
    APPEND 'SAVE'   TO tl_exclud.
    APPEND 'SHOW'   TO tl_exclud.
    APPEND 'EDIT'   TO tl_exclud.
    APPEND 'CANCEL' TO tl_exclud.

  ELSE.
    CLEAR tl_exclud[].

  ENDIF.

  SET PF-STATUS 'ZLESPF_FRET_FAT' EXCLUDING tl_exclud.
  SET TITLEBAR 'ZLES_PFS_TITLE'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_0102 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zm_status_0102 OUTPUT.

* Exibe os dados selecionado da pesquisa.
  PERFORM zf_show_data_frete_fat.

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
  APPEND 'NEW'    TO tl_exclud.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'OBL'.
      screen-required = 2.
      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.
* Verifica campos da tela de Manutenção do Frete.
  PERFORM zf_chek_fields_scr_0200.

  CASE sy-ucomm.
    WHEN 'NEW'.   "Frete Novo
      DATA(vl_texto) = 'Criar    '.

    WHEN 'SHOW'.   "Exibir Frete
* BPO da tela 0200 botão Exibir.
***      PERFORM zf_status_0200_show TABLES   tl_exclud
***                                  CHANGING vl_texto.

    WHEN 'EDIT'.   "Editar Frete
* BPO da tela 0200 botão Editar.
***      PERFORM zf_status_0200_edit CHANGING vl_texto.

    WHEN 'CANCEL'. "Cancelar Frete
      vl_texto = 'Cancelar'.

    WHEN 'FINAL'.  "Finalizar Frete
      vl_texto = 'Finalizar'.

    WHEN 'ENTE'.   "Enter na tela 200
      CASE gv_acao.
        WHEN 'SHOW'.   "Exibir Frete
* BPO da tela 0200 botão Exibir.
***          PERFORM zf_status_0200_show TABLES tl_exclud
***                                      USING  vl_texto.

        WHEN 'EDIT'.   "Editar Frete
* BPO da tela 0200 botão Editar.
***          PERFORM zf_status_0200_edit CHANGING vl_texto.

        WHEN OTHERS.
*       Do nothing
      ENDCASE.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

  SET PF-STATUS 'ZLESPF_FRET_FAT' EXCLUDING tl_exclud.
  SET TITLEBAR 'ZLES_PFS_TITLE2' WITH vl_texto.
  CLEAR tl_exclud.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_0210 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zm_status_0210 OUTPUT.

* Exibe os dados do Conjunto Veícular.
  PERFORM zf_show_data_conj_veic.

ENDMODULE.
