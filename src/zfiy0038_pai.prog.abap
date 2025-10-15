*&---------------------------------------------------------------------*
*&  Include           ZFIY0038_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CANCEL_0100  INPUT
*&---------------------------------------------------------------------*
MODULE cancel_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      "PERFORM f_refresh_alv USING 0100.
      "SET SCREEN 1000.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      PERFORM f_refresh_alv USING 0101.
      PERFORM: f_selecionar_dados,
               f_processa_dados.

      LEAVE TO SCREEN 0100.

    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'GER_AUTOMA'.
      PERFORM f_gera_aplic_autom.
    WHEN 'GER_MANUAL'.
      PERFORM f_gera_aplic_manual.
    WHEN 'SALVAR'.
      PERFORM f_save_aplic_permis.
    WHEN 'ANULAR'.
      PERFORM f_aula_aplic_permis.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      "SET SCREEN 1000.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      "SET SCREEN 1000.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
