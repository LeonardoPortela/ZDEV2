*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_PAI
*&---------------------------------------------------------------------*



MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
*     LEAVE PROGRAM.
    WHEN 'REFRESH'.
      PERFORM: f_selecionar_dados,
               f_processa_dados,
               f_refresh_alv USING '0100'.

  ENDCASE.

ENDMODULE.
