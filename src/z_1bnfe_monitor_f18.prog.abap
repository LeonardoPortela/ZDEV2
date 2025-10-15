*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F18
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_log_icons
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_LOG_ICONS.

  LOOP AT IT_NFE_ALV INTO WA_NFE_ALV.
    INDEX = SY-TABIX.
    READ TABLE IT_LOG_HEADER INTO WA_LOG_HEADER
       WITH KEY EXTNUMBER = WA_NFE_ALV-DOCNUM.
    IF SY-SUBRC = 0.
      WA_NFE_ALV-ERRLOG = ICON_DEFECT.
    ELSE.
      WA_NFE_ALV-ERRLOG = ICON_LED_GREEN.
    ENDIF.

    MODIFY IT_NFE_ALV FROM WA_NFE_ALV INDEX INDEX.
  ENDLOOP.

ENDFORM.                    " set_log_icons
*&---------------------------------------------------------------------*
*&      Form  set_status_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_STATUS_INFO .

  DATA LS_ACTTAB TYPE J_1BNFE_ACTIVE.

  IF SY-TCODE EQ 'ZMDFE' AND IT_NFE_ALV[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(IT_ZSDT0102)
      FROM ZSDT0102
       FOR ALL ENTRIES IN @IT_NFE_ALV
     WHERE DOCNUM EQ @IT_NFE_ALV-DOCNUM.

    SORT IT_ZSDT0102 BY DOCNUM.

    SELECT * INTO TABLE @DATA(IT_EVENTS)
      FROM J_1BNFE_EVENT
       FOR ALL ENTRIES IN @IT_NFE_ALV
     WHERE DOCNUM    EQ @IT_NFE_ALV-DOCNUM
       AND INT_EVENT EQ 'EV_ENC'.

    SORT IT_EVENTS BY DOCNUM.
  ENDIF.

  LOOP AT IT_NFE_ALV INTO WA_NFE_ALV.

    INDEX = SY-TABIX.

    CLEAR LS_ACTTAB.
    MOVE-CORRESPONDING WA_NFE_ALV TO LS_ACTTAB.
* - Set indicator: "Required Action" -------------
    CALL FUNCTION 'J_1B_NFE_SET_ACTION_INDICATOR'
      EXPORTING
        IS_ACTTAB = LS_ACTTAB
      IMPORTING
        ES_ACTTAB = LS_ACTTAB.

    MOVE-CORRESPONDING LS_ACTTAB TO WA_NFE_ALV.
* - Set status icon

    IF SY-TCODE EQ 'ZMDFE'.
*1  A encerrar
*2  Solicitado Encerramento
*3  Encerrado
*4  Erro Encerramento
      CLEAR: WA_NFE_ALV-ENCERRADO.
      READ TABLE IT_ZSDT0102 INTO DATA(WA_ZSDT0102) WITH KEY DOCNUM = WA_NFE_ALV-DOCNUM BINARY SEARCH.
      IF SY-SUBRC IS INITIAL AND WA_NFE_ALV-CANCEL NE ABAP_TRUE AND WA_NFE_ALV-ACTION_REQU CA 'C' AND WA_NFE_ALV-DOCSTA EQ '1'.
        READ TABLE IT_EVENTS INTO DATA(WA_EVENTS) WITH KEY DOCNUM = WA_NFE_ALV-DOCNUM BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          CASE WA_EVENTS-DOCSTA.
            WHEN ' '.
              WA_NFE_ALV-ENCERRADO = ICON_WARNING.
            WHEN '1'.
              WA_NFE_ALV-ENCERRADO = ICON_COMPLETE.
            WHEN '2'.
              WA_NFE_ALV-ENCERRADO = ICON_ALERT.
          ENDCASE.
        ELSE.
          WA_NFE_ALV-ENCERRADO = ICON_TRANSPORT.
        ENDIF.
      ENDIF.
    ENDIF.

    PERFORM SET_STATUS_ICONS CHANGING WA_NFE_ALV.

    IF SY-TCODE NE 'ZMDFE'.

      SELECT SINGLE * INTO @DATA(WA_ZSDT0230)
        FROM ZSDT0230
       WHERE DOCNUM EQ @WA_NFE_ACTIVE-DOCNUM.

      IF SY-SUBRC IS INITIAL.
        WA_NFE_ALV-DT_ENVIO = WA_ZSDT0230-DT_REGISTRO.
        WA_NFE_ALV-HR_ENVIO = WA_ZSDT0230-HR_REGISTRO.
        WA_NFE_ALV-DS_EMAIL = WA_ZSDT0230-EMAIL.
      ENDIF.

    ENDIF.

    MODIFY IT_NFE_ALV FROM WA_NFE_ALV INDEX INDEX.
  ENDLOOP.

ENDFORM.                    " set_status_info
