*&---------------------------------------------------------------------*
*& Include          ZFIS43_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'LEAVE' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      IF rb_conf IS NOT INITIAL.
        PERFORM f_salvar_conf.
      ELSEIF rb_desc IS NOT INITIAL.
        PERFORM f_salvar_desc.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
