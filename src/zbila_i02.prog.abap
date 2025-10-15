*----------------------------------------------------------------------*
* INCLUDE BILA&I02, used in RFBILA00                                   *
* mit Unterprogramm für den Formulardruck                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FORM FILL_SCR_DATA                                                   *
*----------------------------------------------------------------------*
* Füllen der ITAB für die Datenübergabe an das Druckprogramm           *
*----------------------------------------------------------------------*
*  -->  VALUE(TYP)                                                     *
*  -->  VALUE(ERGSL)                                                   *
*----------------------------------------------------------------------*
FORM FILL_SCR_DATA USING VALUE(TYP) VALUE(ERGSL).
  CLEAR SCR_DATA.
  SCR_DATA-GSBER = SORT-GSB1.
  SCR_DATA-BUKRS = SORT-BUK1.
  SCR_DATA-ERGSL = ERGSL.
  SCR_DATA-TYP   = TYP.
  IF TYP <> 'A'.
    SCR_DATA-BSUM   = I011Q-BSUM.
    SCR_DATA-BSUM2  = I011Q-BSUM2.
    IF REPWAERS = SPACE.
      SCR_DATA-WAERS  = CTYP_WAERS.
      SCR_DATA-WAERS2 = CTYP_WAERS2.
    ELSE.
      SCR_DATA-WAERS  = REPWAERS.
      SCR_DATA-WAERS2 = REPWAERS.
    ENDIF.
    SCR_DATA-VSUM  = I011Q-VSUM.
    SCR_DATA-VSUM2  = I011Q-VSUM2.
*     Berechnung absolute Abweichung
*     ------------------------------
      SCR_DATA-SALDO = I011Q-BSUM - I011Q-VSUM.
      SCR_DATA-SALDO2 = I011Q-BSUM2 - I011Q-VSUM2.

*     Berechnung relative Abweichung
*     ------------------------------
    CLEAR:  SCR_DATA-RELAB.
    IF I011Q-VSUM NE 0.                " <= keine Division durch Null
      PERFORM REL_ABW_BERECHNEN USING  I011Q-BSUM
                                       I011Q-VSUM
                                       I011Q-BSUM2
                                       I011Q-VSUM2
                              CHANGING SCR_DATA-RELAB
                                       SCR_DATA-RELAB2.
    ENDIF.
    SCR_DATA-STAR = I011Q-STAR.
  ENDIF.
  READ TABLE SCR_DATA WITH KEY SCR_DATA(19) BINARY SEARCH.
  IF SY-SUBRC <> 0.
    INSERT SCR_DATA INDEX SY-TABIX.
  ELSE.
    MODIFY SCR_DATA INDEX SY-TABIX.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* FORM CHECK_FORM                                                     *
*---------------------------------------------------------------------*
*  check of formular is active                                        *
*---------------------------------------------------------------------*
FORM CHECK_FORM USING VALUE(SCR_FORM).

  data: l_text(120) type c.
  DATA BEGIN OF SELECTIONS OCCURS 5.
          INCLUDE STRUCTURE THEAD.
  DATA END OF SELECTIONS.

  DATA: ENTRIES LIKE SY-TFILL.

  DATA: ACTIVE(1)   VALUE ' ',
        TRANS(1)    VALUE ' '.

  CALL FUNCTION 'SELECT_FORM'
       EXPORTING
*           CLIENT          = SY-MANDT
            FORM            = SCR_FORM
            LANGUAGE        = BILASPRA
            STATUS          = '*'
            THROUGHCLIENT   = ' '
            THROUGHLANGUAGE = ' '
       IMPORTING
            ENTRIES         = ENTRIES
       TABLES
            SELECTIONS      = SELECTIONS.

  IF ENTRIES = 0.                      " Kein Formular vorhanden
    l_text = text-115.
    REPLACE '$FORM' WITH SCR_FORM INTO l_TEXT.
    REPLACE '$SPRA' WITH BILASPRA INTO l_TEXT.
    condense l_text.
    message e600 with l_text.
  ENDIF.
  LOOP AT SELECTIONS.
    CASE SELECTIONS-TDSPRAS.
      WHEN SELECTIONS-TDOSPRAS.
        IF SELECTIONS-TDNAME+16(3) = 'SAP'.
          ACTIVE = 'X'.
        ENDIF.
      WHEN BILASPRA.
        TRANS = SELECTIONS-TDTRANSTAT.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  IF ACTIVE = 'X' OR TRANS = '1'.
    l_text = text-116.
    REPLACE '$FORM' WITH SCR_FORM INTO l_TEXT.
    IF ACTIVE = 'X'.
      REPLACE '$SPRA' WITH SELECTIONS-TDOSPRAS INTO l_TEXT.
    ELSE.
      REPLACE '$SPRA' WITH SELECTIONS-TDSPRAS INTO l_TEXT.
    ENDIF.
    condense l_text.
    message e600 with l_text.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* FORM SCR_CHECK_PRINTER                                              *
*---------------------------------------------------------------------*
* checks of printer                                                   *
*---------------------------------------------------------------------*
FORM SCR_CHECK_PRINTER USING VALUE(SCR_DEVI).
  data: l_text(120) type c.
  SELECT SINGLE * FROM TSP03
    WHERE PADEST EQ SCR_DEVI.
  IF SY-SUBRC NE 0.
    l_text = text-117.
    REPLACE '$PRINTER' WITH SCR_DEVI INTO l_TEXT.
    condense l_text.
    message e600 with l_text.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
* Form  SCR_CHECK_PRINTER_FILLED                                      *
*---------------------------------------------------------------------*
* check in batch mode and formular printer field must be filled       *
*---------------------------------------------------------------------*
FORM SCR_CHECK_PRINTER_FILLED.
  IF SCR_DEVI =  SPACE.
    message e600 with text-118.
    stop.
  ENDIF.
ENDFORM.                               " CHECK_PRINTER_FILLED
