*----------------------------------------------------------------------*
***INCLUDE LZSDG010I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CALL METHOD obj_container->free.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      CALL METHOD obj_container->free.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      CALL METHOD obj_container->free.
      LEAVE TO SCREEN 0.
    WHEN 'VOLTAR'.
      CALL METHOD obj_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
