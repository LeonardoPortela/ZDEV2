*----------------------------------------------------------------------*
***INCLUDE MZLESVEIC0001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'PF0001'.
  SET TITLEBAR  'TI0001'.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  CASE ok_code.
    WHEN 'PESQ1001'.
      PERFORM pesquisar_veiculos.
      CLEAR: ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'NOVO'.
      vg_2001 = 'NOVO'.
      CLEAR zlest0002.
      vg_pesquis = 'X'.
      CALL SCREEN 2001." STARTING AT 04 02 ENDING AT 95 30.
      PERFORM pesquisar_veiculos.
      CLEAR: ok_code.
    WHEN 'DELETE'.
      PERFORM apagar_registro.
      CLEAR: ok_code.
    WHEN 'GERA_EXCEL'.
      PERFORM gerar_excel.
      CLEAR: ok_code.
    WHEN 'CARD_PED'.
      PERFORM cadastrar_cartao_pedagio.

    WHEN 'LOG'.
      PERFORM fm_log.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Form  PESQUISAR_VEICULOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pesquisar_veiculos .

  CLEAR: it_veiculos[], it_veiculo_tela[] .

  CALL FUNCTION 'Z_RETORNA_VEICULOS'
    EXPORTING
      rt_placa    = b_placa[]
      rt_propr    = b_propr[]
      rt_cidad    = b_cidad[]
      rt_estad    = b_estad[]
    TABLES
      it_veiculos = it_veiculos
    EXCEPTIONS
      erro_vazio  = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

    SORT it_veiculos BY pc_veiculo.

    LOOP AT it_veiculos INTO wa_veiculos.
      MOVE-CORRESPONDING wa_veiculos TO wa_veiculo_tela.
      CLEAR wa_veiculo_tela-mark.
      CLEAR wa_veiculo_tela-name1.
      IF NOT wa_veiculo_tela-proprietario IS INITIAL.
        SELECT SINGLE name1 INTO wa_veiculo_tela-name1
          FROM lfa1
         WHERE lifnr EQ wa_veiculo_tela-proprietario.
      ENDIF.
      APPEND wa_veiculo_tela TO it_veiculo_tela.
    ENDLOOP.



  ENDIF.

ENDFORM.                    " PESQUISAR_VEICULOS

*&---------------------------------------------------------------------*
*&      Form  APAGAR_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM apagar_registro .

  DATA: answer           TYPE c LENGTH 1,
        wa_veiculo_placa TYPE  zsdt0001.


  READ TABLE it_veiculo_tela INTO wa_veiculo_tela WITH KEY mark = 'X'.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING wa_veiculo_tela TO zlest0002.

    SELECT *
       FROM zsdt0001
       INTO wa_veiculo_placa
       WHERE placa_cav EQ wa_veiculo_tela-pc_veiculo.

    ENDSELECT.

    IF wa_veiculo_placa-placa_cav IS NOT INITIAL.
      MESSAGE text-001 TYPE 'E'.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = 'Atenção!'
        textline1 = 'Dados foram alterados.'
        textline2 = 'Deseja salvar?'
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN 'J'.
        DELETE FROM zlest0002 WHERE pc_veiculo EQ wa_veiculo_tela-pc_veiculo.
        PERFORM pesquisar_veiculos.
      WHEN 'N'.
        CLEAR vg_alterou.
      WHEN 'A'.
        EXIT.
    ENDCASE.

  ELSE.
    MESSAGE 'Selecione um veículo!' TYPE 'I'.
  ENDIF.

ENDFORM.                    " APAGAR_REGISTRO

*&---------------------------------------------------------------------*
*&      Form  GERAR_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gerar_excel .

  DATA: msg TYPE string.

  msg = sy-datum.

  CONCATENATE msg+6(2) '/' msg+4(2) '/' msg(4) INTO msg.
  CONCATENATE 'Data: ' msg INTO msg SEPARATED BY space.

  CALL FUNCTION 'Z_GERA_EXCEL'
    EXPORTING
      titulo1 = 'Relação de Veículos'
      titulo2 = msg
      tabname = 'ZLEST0002'
    TABLES
      tabela  = it_veiculos.

ENDFORM.                    " GERAR_EXCEL

*&---------------------------------------------------------------------*
*&      Form  CADASTRAR_CARTAO_PEDAGIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cadastrar_cartao_pedagio .

  READ TABLE it_veiculo_tela INTO wa_veiculo_tela WITH KEY mark = 'X'.
  IF sy-subrc EQ 0.
    IF wa_veiculo_tela-tp_veiculo EQ 0.
      CLEAR: it_cards[].
      PERFORM pesquisar_cards.
      CALL SCREEN 3000 STARTING AT 10 10.
    ELSE.
      MESSAGE i094 WITH wa_veiculo_tela-pc_veiculo.
    ENDIF.
  ELSE.
    MESSAGE 'Selecione um veículo!' TYPE 'I'.
  ENDIF.

ENDFORM.                    " CADASTRAR_CARTAO_PEDAGIO
*&---------------------------------------------------------------------*
*&      Module  F_EQUNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_equnr INPUT.



ENDMODULE.
