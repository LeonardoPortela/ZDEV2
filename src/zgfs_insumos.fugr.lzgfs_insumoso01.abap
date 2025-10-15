*----------------------------------------------------------------------*
***INCLUDE LZGFS_INSUMOSO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0600 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0600 OUTPUT.

  SET PF-STATUS 'PF0600'.
  SET TITLEBAR  'TB0600'.

  PERFORM f_criar_editor.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0600 INPUT.

  CASE ok_code3.
    WHEN 'SEGUIR' OR 'BACK' OR 'EXIT' OR 'CANCEL'.
      IF container_editor IS NOT INITIAL.
        CALL METHOD container_editor->free.
        CLEAR container_editor.
      ENDIF.
*     IF cl_editor IS NOT INITIAL.
*       CALL METHOD cl_editor->free.
*     ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
