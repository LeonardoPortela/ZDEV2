*----------------------------------------------------------------------*
***INCLUDE LZGFS_HVII01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN '&CONFIRMA'.
      l_confirma = abap_true.

      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

    WHEN '&CANCELA'.
      l_confirma = abap_false.

      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      l_confirma = abap_false.

      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
