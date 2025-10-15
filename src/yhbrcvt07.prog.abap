*&---------------------------------------------------------------------*
*&  Include           YHBRCVT07
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      Form  RE7BRTR
*----------------------------------------------------------------------*
FORM RE7BRTR USING    $TRANS       LIKE T7BRTR-TRANS
                      $BEGDA       LIKE PN-ENDDA
                      $ENDDA       LIKE PN-ENDDA
                      $VALTT       LIKE PC207-BETRG
                      $DAYS        TYPE I.

  DATA: DAYS    TYPE I,
        FERI    TYPE I,
        AUSE    TYPE I,
        FACTOR  LIKE I7BRTR-VALTT.

  PERFORM GET_7BRTR.

* check $trans <> i7brtr.              " and the dates?

  IF I7BRTR-TRANS <> $TRANS  OR  I7BRTR-ENDDA < $ENDDA  OR
     I7BRTR-BEGDA >  $BEGDA.

    LOOP AT I7BRTR WHERE TRANS = $TRANS
                            AND BEGDA <= $ENDDA   "   pn-endda
                            AND ENDDA => $BEGDA.  "   pn-begda.

*// Proportion dates - split
       IF I7BRTR-ENDDA <= $ENDDA AND I7BRTR-BEGDA >= $BEGDA.
          PERFORM NUMBER_DAYS USING I7BRTR-BEGDA I7BRTR-ENDDA DAYS AUSE FERI.
       ELSEIF I7BRTR-ENDDA <= $ENDDA.
          PERFORM NUMBER_DAYS USING $BEGDA I7BRTR-ENDDA DAYS AUSE FERI.
       ELSEIF I7BRTR-BEGDA >= $BEGDA.
          PERFORM NUMBER_DAYS USING I7BRTR-BEGDA $ENDDA DAYS AUSE FERI.
       ELSE.
          DAYS = $DAYS.                          "// No split
       ENDIF.

       FACTOR = FACTOR + I7BRTR-VALTT * DAYS.

    ENDLOOP.

    IF SY-SUBRC <> 0.
      WRITE $BEGDA TO BEG_DAT DD/MM/YY.
      WRITE $ENDDA TO END_DAT DD/MM/YY.
      PERFORM APPEND_ERROR USING SPACE
                                 'E'
                                 '006'
                                 $TRANS
                                 BEG_DAT
                                 END_DAT
                                 SPACE.
    ELSE.
      MOVE FACTOR TO $VALTT.
    ENDIF.

  ELSE.
    DAYS = $DAYS.
    FACTOR = I7BRTR-VALTT * DAYS.
    MOVE FACTOR TO $VALTT.
  ENDIF.

ENDFORM.                               " RE7BRTR

*----------------------------------------------------------------------*
*      Form  GET_7BRTR
*----------------------------------------------------------------------*
FORM GET_7BRTR.

  DATA: TAB_LINES TYPE I VALUE 0.

  DESCRIBE TABLE I7BRTR LINES TAB_LINES.
  CHECK TAB_LINES = 0.
  SELECT * FROM T7BRTR INTO TABLE I7BRTR.

ENDFORM.                               " GET_7BRTR

*----------------------------------------------------------------------*
*      Form  RE001P
*----------------------------------------------------------------------*
FORM RE001P USING    P_COLL_TAB_WERKS
                     P_COLL_TAB_BTRTL
            CHANGING P_L_BTRTL_TEXT.

*  check t001p-werks ne p_coll_tab_werks
*     or t001p-btrtl ne p_coll_tab_btrtl.

  SELECT SINGLE * FROM T001P WHERE WERKS EQ P_COLL_TAB_WERKS
                               AND BTRTL EQ P_COLL_TAB_BTRTL.
  IF SY-SUBRC <> 0.
    T001P-WERKS = P_COLL_TAB_WERKS.
    T001P-BTRTL = P_COLL_TAB_BTRTL.
    T001P-BTEXT = '***************'.
    CLEAR T001P-MOLGA.
    PERFORM APPEND_ERROR USING SPACE
                               'W'
                               '007'
                               P_COLL_TAB_WERKS
                               P_COLL_TAB_BTRTL
                               SPACE
                               SPACE.
  ENDIF.

  P_L_BTRTL_TEXT = T001P-BTEXT.

ENDFORM.                               " RE001P


*----------------------------------------------------------------------*
*      Form  RE501T
*----------------------------------------------------------------------*
FORM RE501T USING    P_COLL_TAB_PERSG
            CHANGING P_L_PERSG_TEXT.

*  check t001p-werks ne p_coll_tab_werks
*     or t001p-btrtl ne p_coll_tab_btrtl.

  SELECT SINGLE * FROM T501T INTO @DATA(w501T) WHERE SPRSL EQ 'PT'
                                                 AND PERSG EQ @P_COLL_TAB_PERSG.
  IF SY-SUBRC <> 0.
  ENDIF.

  P_L_PERSG_TEXT = w501T-PTEXT.

ENDFORM.                               " RE001P


*----------------------------------------------------------------------*
*      Form  RE7BRTP
*----------------------------------------------------------------------*
*      Reads the transportation description
*----------------------------------------------------------------------*
FORM RE7BRTP USING    P_P0410-TRANS
             CHANGING Q0410-TDESC.

  SELECT SINGLE * FROM  T7BRTP
         WHERE  SPRSL       = SY-LANGU
         AND    TRANS       = P_P0410-TRANS    .

  IF SY-SUBRC <> 0.
    PERFORM APPEND_ERROR USING SPACE
                               'W'
                               '008'
                               P_P0410-TRANS
                               SPACE
                               SPACE
                               SPACE.
    CLEAR T7BRTP.
  ENDIF.

  MOVE-CORRESPONDING T7BRTP TO Q0410.

ENDFORM.                               " RE7BRTP

*&---------------------------------------------------------------------*
*&      Form  GET_CURRENCY
*&---------------------------------------------------------------------*
*       Gets the currency
*----------------------------------------------------------------------*
*      -->P_P0001_WERKS  text
*      <--P_T500C_WAERS  text
*----------------------------------------------------------------------*
FORM GET_CURRENCY USING    P0001_WERKS
                  CHANGING T500C-WAERS.

    SELECT SINGLE * FROM  T500P
           WHERE  PERSA       = P0001-WERKS.

    IF SY-SUBRC <> 0.
      PERFORM APPEND_ERROR USING SPACE
                                 'E'
                                 '009'
                                 P0001_WERKS
                                 SPACE
                                 SPACE
                                 SPACE.
    ENDIF.

    SELECT * FROM  T500C
           WHERE  LAND1       =  T500P-LAND1
           AND    ENDDA       >= PN-ENDDA
           AND    BEGDA       <= PN-BEGDA.
      EXIT.
    ENDSELECT.

    IF SY-SUBRC <> 0.
      PERFORM APPEND_ERROR USING SPACE
                                 'E'
                                 '010'
                                 T500P-LAND1
                                 SPACE
                                 SPACE
                                 SPACE.
    ENDIF.

ENDFORM.                    " GET_CURRENCY

*----------------------------------------------------------------------*
*       FORM COMPANY_TEXT_GET
*----------------------------------------------------------------------*
FORM COMPANYCODE_TEXT_GET USING VALUE(BUKRS) CHANGING TEXT.

  CALL FUNCTION 'HRCA_COMPANYCODE_GETDETAIL'
       EXPORTING
            COMPANYCODE = BUKRS
            LANGUAGE    = SY-LANGU
       IMPORTING
            COMP_NAME   = TEXT
       EXCEPTIONS
            NOT_FOUND   = 1
            OTHERS      = 2.

  IF SY-SUBRC <> 0.
    PERFORM APPEND_ERROR USING SPACE
                               'W'
                               '011'
                               BUKRS
                               SPACE
                               SPACE
                               SPACE.
    CLEAR TEXT.
  ENDIF.

ENDFORM.                               " COMPANY_TEXT_GET

*----------------------------------------------------------------------*
*       Form  REICON
*----------------------------------------------------------------------*
FORM REICON USING W_NAME_ICON.

  SELECT * FROM  ICON
         WHERE  NAME        = W_NAME_ICON.
    EXIT.
  ENDSELECT.

  IF SY-SUBRC <> 0.
    PERFORM APPEND_ERROR USING SPACE
                               'W'
                               '019'
                               SY-SUBRC
                               SPACE
                               SPACE
                               SPACE.
    CLEAR W_ICON.
  ELSE.
    MOVE ICON-ID    TO W_ICON.
  ENDIF.

ENDFORM.                    " REICON
