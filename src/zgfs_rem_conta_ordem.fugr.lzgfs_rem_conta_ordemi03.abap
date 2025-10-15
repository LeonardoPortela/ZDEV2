*----------------------------------------------------------------------*
***INCLUDE LZGFS_REM_CONTA_ORDEMI03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0120 INPUT.

  CASE ok_code.
    WHEN '&VOLTAR'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
