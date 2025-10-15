*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_PAI
*&---------------------------------------------------------------------*



MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'EXEC'.
      PERFORM: f_selecionar_dados,
               f_processa_dados,
               f_refresh_alv USING '0100'.

*-CS2021000386 - 28.04.2021 - JT - inicio
    WHEN 'MOVTO'.
      CLEAR l_erro.
      PERFORM f_movimentar_deposito CHANGING l_erro.

      IF l_erro = abap_false.
        PERFORM:  f_selecionar_dados,
                  f_processa_dados,
                  f_refresh_alv USING '0100'.
      ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim

  ENDCASE.

ENDMODULE.
