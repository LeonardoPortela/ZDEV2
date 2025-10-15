*----------------------------------------------------------------------*
***INCLUDE ZLESR0165_USER_COMMAND_0110I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.

  DATA: lv_erro_0110 TYPE char01.

  CASE ok_code_0110.
    WHEN '&CONFIRMAR'.
      PERFORM f_valida_tela_0110 CHANGING lv_erro_0110.
      IF lv_erro_0110 = abap_false.
        PERFORM f_confirma_tela_0110.
        tl_0110_cop   = tl_0110.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN '&VOLTAR'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CLEAR ok_code_0110.

ENDMODULE.
