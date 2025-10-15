*----------------------------------------------------------------------*
***INCLUDE LZGLF064F01 .
*----------------------------------------------------------------------*

FORM F_ADD_SNUM.

  DATA: GT_ZGLT064 TYPE TABLE OF ZGLT064,
        V_LINES TYPE SY-TABIX.

  SELECT *
    FROM ZGLT064
    INTO TABLE GT_ZGLT064.

  DESCRIBE TABLE GT_ZGLT064 LINES V_LINES.
  V_LINES = V_LINES + 1.

  ZGLT064-SEQ_TIPO = V_LINES.

ENDFORM.                    "f_add_snum
