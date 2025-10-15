*----------------------------------------------------------------------*
***INCLUDE LZGFSD0005I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE ok_code_9000.
    WHEN 'SEARCH'.

      PERFORM f_search_9000.

      PERFORM f_atualiza_alv_9000.

    WHEN 'CONFIRMAR'.

      PERFORM f_gravar_9000 CHANGING gv_erro_9000.

      IF gv_erro_9000 IS INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'CANCEL'.

      "PERFORM f_free_alv_4000.

      gv_9000_canc = 'X'.

      LEAVE TO SCREEN 0 .


    WHEN 'ASSIGN'.

      PERFORM f_associar_dados USING '01' '02'.
      PERFORM f_updt_associados.

      PERFORM f_atualiza_alv_9000.

    WHEN 'UNASSIGNED'.

      PERFORM f_associar_dados USING '02' '01'.

      PERFORM f_atualiza_alv_9000.

  ENDCASE.

ENDMODULE.
