*----------------------------------------------------------------------*
***INCLUDE LZSD01I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_9000 input.

  CASE sy-ucomm.
* Se usuário selecionou o botão SALVAR
    WHEN 'SAVE'.
      PERFORM zresgata_texto_container.

* Finaliza o programa
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

endmodule.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_9001 input.

  CASE sy-ucomm.
* Se usuário selecionou o botão SALVAR
    WHEN 'SAVE'.
      PERFORM zresgata_texto_container_ap.

* Finaliza o programa
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

endmodule.                 " USER_COMMAND_9001  INPUT
