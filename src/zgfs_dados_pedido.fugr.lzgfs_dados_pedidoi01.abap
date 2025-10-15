*----------------------------------------------------------------------*
***INCLUDE LZGFS_DADOS_PEDIDOI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: vl_valida_qtd TYPE string.

  CASE ok_code.

    WHEN 'GERAR'.
      CLEAR: gt_log_remessa[], vl_valida_qtd.
      PERFORM f_valida_qtd CHANGING vl_valida_qtd.
      IF vl_valida_qtd EQ abap_false.
        PERFORM f_gerar_remessa.
        PERFORM f_exibi_popup_erro.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE s024(sd) WITH vl_valida_qtd DISPLAY LIKE 'E'.
      ENDIF.

    WHEN '&CANCEL'.

      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      "CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

  ENDCASE.
  CLEAR ok_code.


ENDMODULE.
