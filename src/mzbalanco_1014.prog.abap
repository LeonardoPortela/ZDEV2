*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1014 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1014_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1014_EXIT INPUT.
  CLEAR: IT_ZGLT048.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_1014_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1014  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1014 INPUT.

  CASE OK_CODE_1014.
    WHEN OK_CONF.
      MOVE-CORRESPONDING IT_ZGLT048 TO WA_ZGLT047_ALTERADO.
      MODIFY ZGLT048 FROM IT_ZGLT048.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1014  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1014  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1014 OUTPUT.
  SET PF-STATUS 'PF1003'.
  SET TITLEBAR 'TL1014'.

  CLEAR: WA_ZGLT047, WA_ZGLT047_ALTERADO.

  IF IT_ZGLT048-NIVELSUM IS NOT INITIAL.
    SELECT SINGLE * INTO WA_ZGLT047
      FROM ZGLT047
     WHERE VERSN EQ IT_ZGLT048-VERSN
       AND NIVEL EQ IT_ZGLT048-NIVELSUM.
  ENDIF.

ENDMODULE.                 " STATUS_1014  OUTPUT
