*----------------------------------------------------------------------*
***INCLUDE LZGFSD006I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  user_command_8001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_8001 INPUT.

  gv_ucomm_8001 = sy-ucomm.

  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM f_save_8001.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_7001  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_7001 INPUT.

  gv_ucomm_7001 = sy-ucomm.

  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM f_save_7001.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  PERFORM f_dispach.

ENDMODULE.
