*----------------------------------------------------------------------*
***INCLUDE LZGFS_CCTI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F_CHECAR_LISTA_DE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_checar_lista_de INPUT.

  IF g_dt_lista_de  IS INITIAL.
    MESSAGE e000(fb) WITH text-100.
  ENDIF.

ENDMODULE.

MODULE f_checar_lista_ate INPUT.

  IF g_dt_lista_ate IS INITIAL.
    MESSAGE e000(fb) WITH text-100.
  ENDIF.

ENDMODULE.

MODULE f_checar_lista INPUT.

  IF g_dt_lista_de  > g_dt_lista_ate.
    MESSAGE e000(fb) WITH text-100.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE ok_code.
    WHEN '&LISTAR'.
      PERFORM f_selecao.
      PERFORM f_monta_saida.
    WHEN '&REPRO'.
      PERFORM f_reprocessar.
    WHEN 'EXIT' OR 'BACK' OR 'CANCEL' OR
         '&OK'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
*     LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR ok_code.

ENDMODULE.
