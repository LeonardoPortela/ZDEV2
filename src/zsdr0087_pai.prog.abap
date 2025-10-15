*&---------------------------------------------------------------------*
*&  Include           ZSDR0087_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_screen INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: wl_refresh TYPE c VALUE 'X'.

  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM grava_dados.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.

  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM grava_dados.
    WHEN 'BUSCAR'.
      PERFORM verifica_erros.
  ENDCASE.

ENDMODULE.

***************F4_SEARCH_HELP*********************

MODULE help_wg_saida-zieme INPUT.
  PERFORM f4_busca_unidade.
ENDMODULE.

MODULE help_wg_saida-zlsch INPUT.
  PERFORM f4_busca_zlsch."BUSCA FORMA DE PAGAMENTO
ENDMODULE.

MODULE help_wg_saida-vkaus INPUT.
  PERFORM f4_busca_abrvw.
ENDMODULE.

MODULE help_wg_saida-tipo_calc INPUT.
  PERFORM f4_busca_tp_calc.
ENDMODULE.

MODULE help_wg_saida-preco INPUT.
  PERFORM f4_busca_preco.
ENDMODULE.

MODULE help_wg_saida-c_decimais INPUT.
  PERFORM f4_busca_c_decimais.
ENDMODULE.

MODULE help_wg_saida-param_espec INPUT.
  PERFORM f4_busca_param_espec.
ENDMODULE.

MODULE help_wg_saida-status INPUT.
  PERFORM f4_busca_saida-status.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HELP_WG_SAIDA-KUNNR  INPUT
*&---------------------------------------------------------------------*
MODULE help_wg_saida-kunnr INPUT.
  PERFORM f4_busca_saida-kunnr.
ENDMODULE.
