*----------------------------------------------------------------------*
***INCLUDE ZPMR0090_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  DATA: lt_zpmt0080 TYPE TABLE OF zpmt0080.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'LEAVE'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      MOVE-CORRESPONDING gt_saida TO lt_zpmt0080.

      DELETE lt_zpmt0080 WHERE total < 100 OR
                               total > 100.
      IF sy-subrc IS INITIAL.
        MESSAGE 'Existem registros com total maior ou menor que 100%' TYPE 'E'.
        EXIT.
      ENDIF.

      MODIFY zpmt0080 FROM TABLE lt_zpmt0080.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
        MESSAGE 'Registros salvos com sucesso!' TYPE 'S'.
      ENDIF.
    WHEN 'OK'.
      BREAK-POINT.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
