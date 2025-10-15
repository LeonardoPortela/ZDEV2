*----------------------------------------------------------------------*
***INCLUDE ZMMR0045_STATUS_0300O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  CLEAR ok_code3.

  SET PF-STATUS 'ZMMR0045_C'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE ok_code3.

    WHEN 'SALVAR_MOTIVO'.
      PERFORM f_validar_motivo_retorno CHANGING lv_erro.
      IF lv_erro = abap_false.
        PERFORM f_salvar_motivo_retorno.
        PERFORM f_selecao_dados.
        PERFORM f_processa_dados.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'VOLTAR_MOTIVO'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'VOLTAR'.
      LEAVE TO SCREEN 0.

  ENDCASE.

  FREE ok_code3.

ENDMODULE.
