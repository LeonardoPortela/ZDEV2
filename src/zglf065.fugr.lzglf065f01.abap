*----------------------------------------------------------------------*
***INCLUDE LZGLF065F01 .
*----------------------------------------------------------------------*

FORM F_ADD_SNUM.
  DATA: GT_ZGLT065 TYPE TABLE OF ZGLT065,
        V_LINES TYPE SY-TABIX.

  SELECT *
    FROM ZGLT065
    INTO TABLE GT_ZGLT065.

  DESCRIBE TABLE GT_ZGLT065 LINES V_LINES.
  V_LINES = V_LINES + 1.

  ZGLT065-SEQ_MOD = V_LINES.
ENDFORM.                    "F_ADD_SNUM
