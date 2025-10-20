*----------------------------------------------------------------------*
***INCLUDE LZSDT0345O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module BLOQUEIA_COLUNAS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE bloqueia_colunas OUTPUT.

  LOOP AT SCREEN.
    IF screen-name EQ 'ZSDT0345-TIPO_TRANSPORTE' AND sy-ucomm <> 'NEWL' .
      screen-input = 0.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

  LOOP AT SCREEN.

    CASE screen-name.
      WHEN 'ZSDT0345-CANCELADO' OR
           'ZSDT0345-USER_CANCEL' OR
           'ZSDT0345-DATA_CANCEL' OR
           'ZSDT0345-HORA_CANCEL' .

      screen-invisible = 1.  " Oculta o campo
      screen-active = 0.     " Desativa o campo

        MODIFY SCREEN.

      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDMODULE.
