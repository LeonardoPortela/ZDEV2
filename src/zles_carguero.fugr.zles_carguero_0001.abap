FUNCTION ZLES_CARGUERO_0001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

  SELECT SINGLE * INTO @DATA(WA_ZLEST0183) FROM ZLEST0183
    WHERE BUKRS EQ @SPACE.

  IF SY-SUBRC IS INITIAL.

    UPDATE ZLEST0183
       SET BUKRS = '0001'
     WHERE BUKRS EQ @SPACE.

    UPDATE ZLEST0184
       SET BUKRS = '0001'
     WHERE BUKRS EQ @SPACE.

  ENDIF.

  CALL SCREEN 0001.

ENDFUNCTION.
