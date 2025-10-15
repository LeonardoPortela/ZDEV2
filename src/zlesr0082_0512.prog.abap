*----------------------------------------------------------------------*
***INCLUDE ZLESR0082_0512.
*----------------------------------------------------------------------*

TABLES: ZLEST0137.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0512  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0512 OUTPUT.

  SET PF-STATUS 'PF0512'.
  SET TITLEBAR 'TL0512'.

  SELECT SINGLE * INTO ZLEST0137 FROM ZLEST0137.

  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
      WHEN 'A'.
        IF ZLEST0137-CK_CONSULTA IS INITIAL.
          SCREEN-INPUT = '1'.
          MODIFY SCREEN.
        ELSE.
          SCREEN-INPUT = '0'.
          MODIFY SCREEN.
        ENDIF.
      WHEN 'B'.
        IF ZLEST0137-CK_CONSULTA IS INITIAL.
          SCREEN-INPUT = '0'.
          MODIFY SCREEN.
        ELSE.
          SCREEN-INPUT = '1'.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0512_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0512_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0512  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0512 INPUT.

  CASE OK_CODE.
    WHEN 'HABI'.
      ZLEST0137-CK_CONSULTA = ABAP_TRUE.
      MODIFY ZLEST0137.
      COMMIT WORK.
      CLEAR: OK_CODE.
    WHEN 'DESA'.
      ZLEST0137-CK_CONSULTA = ABAP_FALSE.
      MODIFY ZLEST0137.
      COMMIT WORK.
      CLEAR: OK_CODE.
  ENDCASE.

ENDMODULE.
