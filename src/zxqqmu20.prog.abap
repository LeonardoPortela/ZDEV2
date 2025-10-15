*&---------------------------------------------------------------------*
*&  Include           ZXQQMU20
*&---------------------------------------------------------------------*


DATA: IT_IFLO TYPE STANDARD TABLE OF IFLO.

"Consistência para saber se local é sempre do tipo E para nots Y5
IF I_VIQMEL-QMART EQ 'Y5'.
    SELECT *
      FROM IFLO
      INTO TABLE IT_IFLO
      WHERE SPRAS EQ SY-LANGU
      AND FLTYP EQ 'E'.

    READ TABLE IT_IFLO TRANSPORTING NO FIELDS WITH KEY TPLNR = I_VIQMEL-TPLNR.
    IF SY-SUBRC NE 0.
      MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
      RAISE EXIT_FROM_SAVE.
    ENDIF.
ENDIF.
