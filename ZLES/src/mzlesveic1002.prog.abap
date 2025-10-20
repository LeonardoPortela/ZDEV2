*&---------------------------------------------------------------------*
*&  Include           MZLESVEIC1002
*&---------------------------------------------------------------------*

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_PES_VEICULO'. DO NOT CHANGE THIS L
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tab_pes_veiculo_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_veiculo_tela LINES tab_pes_veiculo-lines.
  IF go_myobject IS NOT INITIAL.
    CALL METHOD go_myobject->unpublish.
  ENDIF.
ENDMODULE.                    "TAB_PES_VEICULO_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_PES_VEICULO'. DO NOT CHANGE THIS LIN
*&SPWIZARD: MARK TABLE
MODULE tab_pes_veiculo_mark INPUT.
  DATA: g_tab_pes_veiculo_wa2 LIKE LINE OF it_veiculo_tela.
  IF tab_pes_veiculo-line_sel_mode = 1
  AND it_veiculo_tela-mark = 'X'.
    LOOP AT it_veiculo_tela INTO g_tab_pes_veiculo_wa2
      WHERE mark = 'X'.
      g_tab_pes_veiculo_wa2-mark = ''.
      MODIFY it_veiculo_tela
        FROM g_tab_pes_veiculo_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY it_veiculo_tela
    INDEX tab_pes_veiculo-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TAB_PES_VEICULO_MARK INPUT


*&SPWIZARD: INPUT MODUL FOR TC 'TAB_PES_VEICULO'. DO NOT CHANGE THIS LIN
*&SPWIZARD: MARK TABLE
*MODULE tab_pes_veiculo_nm_fornec INPUT.
*  "DATA: g_tab_pes_veiculo_wa2 LIKE LINE OF it_veiculo_tela.
*
*  LOOP AT it_veiculo_tela INTO g_tab_pes_veiculo_wa2.
*
*    NOME_FORNEC = ''.
*    select single name1
*      into NOME_FORNEC
*      from lfa1
*     where lifnr = g_tab_pes_veiculo_wa2-PROPRIETARIO.
*
*    MODIFY it_veiculo_tela
*      FROM g_tab_pes_veiculo_wa2
*      TRANSPORTING nm_fornec.
*  ENDLOOP.
*
*  MODIFY it_veiculo_tela
*    INDEX tab_pes_veiculo-current_line
*    TRANSPORTING mark.
*ENDMODULE.                    "TAB_PES_VEICULO_MARK INPUT
*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1002 INPUT.
  CASE ok_code.
    WHEN 'CADASTRO'.
      CLEAR: wa_zlest0002_mem.
      READ TABLE it_veiculo_tela INTO wa_veiculo_tela WITH KEY mark = 'X'.
      IF sy-subrc EQ 0.
        vg_2001 = 'CONS'.
        MOVE-CORRESPONDING wa_veiculo_tela TO zlest0002.

        nome_fornec = ''.
        bahns       = ''.
        SELECT SINGLE name1 bahns stcd1 stcd2
          INTO (nome_fornec,bahns,cnpj_forn, cpf_forn)
          FROM lfa1
         WHERE lifnr = zlest0002-proprietario.

        "Memorizar dados veiculo.
        MOVE-CORRESPONDING zlest0002 TO wa_zlest0002_mem.

        ls_object-objkey = zlest0002-pc_veiculo.            "'AAA8297'.
        CALL SCREEN 2001. "STARTING AT 04 02 ENDING AT 95 30.
        PERFORM pesquisar_veiculos.

      ELSE.
        MESSAGE 'Selecione um veículo!' TYPE 'I'.
      ENDIF.
      CLEAR: ok_code.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1002  INPUT

*&---------------------------------------------------------------------*
*&      Form  CANCELAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cancelar .
  CLEAR: vg_alterou, vg_pesquis.
  MOVE-CORRESPONDING wa_veiculo_tela TO zlest0002.
ENDFORM.                    " CANCELAR
