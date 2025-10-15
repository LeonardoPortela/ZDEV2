*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I01
*&---------------------------------------------------------------------*
MODULE exit_command INPUT.
  IF sy-calld = 'X' AND ( sy-tcode = 'ZNFE' OR sy-tcode = 'ZCTE' ).
    IF it_nfe_alv[] IS NOT INITIAL.
      READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX 1.
      SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_nfe_alv-status.
    ENDIF.
  ENDIF.
  CASE ok_code.
    WHEN 'BACK'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " exit_command  INPUT
**<<<------"188425 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       Executa a ação do usuário na tela
*----------------------------------------------------------------------*
MODULE zm_user_command_0110 INPUT.

* Verifica se foi confirmado.
  IF sy-ucomm EQ 'CAN_YES'.
* Verifica se houve alteração nas informações da Guia Agropecuária.
    IF eg_guia_agro_ori NE eg_guia_agro.
      eg_guia_agro-docnum = wa_nfe_alv-docnum.
* Salva dados da Guia de Agropecuária.
      PERFORM zf_salva_guia_agropecuaria USING eg_guia_agro.

    ENDIF.

  ENDIF.

  LEAVE TO SCREEN 0.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_0110_EXIT  INPUT
*&---------------------------------------------------------------------*
*       Executa a ação do usuário na tela quanto a sair
*----------------------------------------------------------------------*
MODULE zm_user_command_0110_exit INPUT.

  CHECK sy-ucomm EQ 'QUIT'.
  LEAVE TO SCREEN 0.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_SRH_TP_GUIA_AGRO  INPUT
*&---------------------------------------------------------------------*
*       Ajuda de pesquisa do Tipo da Guia Agropecuária
*----------------------------------------------------------------------*
MODULE zm_srh_tp_guia_agro INPUT.

* Ajuda de pesquisa do Tipo da Guia Agropecuária.
  PERFORM zf_srh_tp_guia_agro.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_SRH_TP_GUIA_AGRO  INPUT
*&---------------------------------------------------------------------*
*       Ajuda de pesquisa da UF da Guia Agropecuária
*----------------------------------------------------------------------*
MODULE zm_srh_uf_guia_agro INPUT.

* Ajuda de pesquisa da UF da Guia Agropecuária.
  PERFORM zf_srh_uf_guia_agro.

ENDMODULE.
**<<<------"188425 - NMS - FIM------>>>
