*----------------------------------------------------------------------*
* Include ZFIY0022_FORM
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Form  F_SELECCIONA_DATOS
*----------------------------------------------------------------------*
FORM F_SELECCIONA_DATOS .

  SELECT BUKRS BELNR GJAHR RLDNR
    INTO TABLE T_BKPF
    FROM BKPF
   WHERE BUKRS IN SO_BUKRS
     AND GJAHR IN SO_GJAHR
     AND BELNR IN SO_BELNR
     AND BLART IN SO_BLART
     AND BUDAT IN SO_BUDAT.

  IF SY-SUBRC NE 0.
    MESSAGE S011(PC) WITH TEXT-M00.
  ENDIF.

ENDFORM.                    " F_SELECCIONA_DATOS
*----------------------------------------------------------------------*
* Form  F_GENERA_PDF
*----------------------------------------------------------------------*
FORM F_GENERA_PDF .

  LOOP AT T_BKPF INTO E_BKPF.

    SUBMIT ZFIY0003
      WITH BR_BUKRS-LOW = E_BKPF-BUKRS
      WITH BR_BELNR-LOW = E_BKPF-BELNR
      WITH BR_GJAHR-LOW = E_BKPF-GJAHR
      WITH P_CERTIF     = SPACE
      WITH P_PATH       = P_PATH
      WITH S_COPY       = '1'
      WITH P_IMPR       = 'LOCL'
      AND  RETURN.

    SUBMIT ZFIY0004
      WITH BR_BUKRS-LOW = E_BKPF-BUKRS
      WITH BR_BELNR-LOW = E_BKPF-BELNR
      WITH BR_GJAHR-LOW = E_BKPF-GJAHR
      WITH P_PATH       = P_PATH
      WITH S_COPY       = '1'
      WITH P_IMPR       = 'LOCL'
      AND  RETURN.

  ENDLOOP.

ENDFORM.                    " F_GENERA_PDF
