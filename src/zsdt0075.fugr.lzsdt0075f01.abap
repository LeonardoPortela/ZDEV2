*----------------------------------------------------------------------*
***INCLUDE LZSDT0075F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  DATA WA_NAME1 TYPE KNA1-NAME1.

  SELECT SINGLE NAME1
    FROM KNA1
    INTO WA_NAME1
    WHERE KUNNR = ZSDT0075-KUNNR.

  CHECK SY-SUBRC = 0.

  ZSDT0075-NAME1 = WA_NAME1.
ENDFORM.                    "fech_value
