*&---------------------------------------------------------------------*
*&  Include           ZPPR0011_PAI
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'INSER'.
      PERFORM zf_insere.
    WHEN 'SAVE'.
      PERFORM zf_gravar.
    WHEN 'DEL'.
      PERFORM zf_delete.
    WHEN 'REFRESH'.
      CLEAR wa_saida.
      PERFORM seleciona_dados.
      PERFORM call_scrren.
    WHEN 'ENTER'.
      "Verifica se existe classe cadastrada.

*     IF wa_saida-class IS INITIAL.  "*-CS2022000332-#83225-26.07.2022-JT-inicio
      IF wa_saida-clint IS NOT INITIAL.
        PERFORM verific_class.
      ENDIF.
*     ENDIF.

      IF wa_saida-atinn IS NOT INITIAL.
        PERFORM verific_caracts.
      ELSE.
        CLEAR wa_saida-atnam.
      ENDIF.
  ENDCASE.
ENDMODULE.
