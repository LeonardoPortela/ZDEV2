*----------------------------------------------------------------------*
***INCLUDE LZLESF0077F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.

  DATA: WL_J_1BAGNT TYPE J_1BAGNT.

  SELECT SINGLE * FROM J_1BAGNT
    INTO WL_J_1BAGNT
  WHERE CFOP EQ ZLEST0077-CFOP
    AND SPRAS EQ 'P'.

  IF ( SY-SUBRC EQ 0 ).

    ZLEST0077-CFOTXT = WL_J_1BAGNT-CFOTXT.


  ENDIF.

ENDFORM.                    "FETCH_VALUE
