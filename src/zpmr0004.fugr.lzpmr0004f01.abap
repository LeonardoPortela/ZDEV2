*----------------------------------------------------------------------*
***INCLUDE LZPMR0004F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  DATA: TL_0004 TYPE TABLE OF ZPMR0004 WITH HEADER LINE.

  DATA: LV_MSG   TYPE BAPI_MSG.",

  SELECT *
    INTO TABLE TL_0004
    FROM ZPMR0004.

  LOOP AT EXTRACT.
    IF EXTRACT+6(1) EQ 'L'.
      CONTINUE.
    ENDIF.

    READ TABLE TL_0004 WITH KEY EQTYP = EXTRACT+3(1).
    IF SY-SUBRC IS INITIAL.
      IF TL_0004-ORDEM_EQUIP NE EXTRACT+4(1)
      OR TL_0004-ORDEM_CC    NE EXTRACT+5(1).
        CONCATENATE 'Modif. >> Categoria: ' EXTRACT+3(1) ' -> Ord.Equip.: "' EXTRACT+4(1) '" -> Ord.CC.: "' EXTRACT+5(1) '"' INTO LV_MSG.

        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          EXPORTING
            I_TP_MSG   = 'W'
            I_MENSAGEM = LV_MSG
            I_TCODE    = 'ZPM0024'.

      ENDIF.

    ELSE.
      CONCATENATE 'Inser. >> Categoria: ' EXTRACT+3(1) ' -> Ord.Equip.: "' EXTRACT+4(1) '" -> Ord.CC.: "' EXTRACT+5(1) '"' INTO LV_MSG.

      CALL FUNCTION 'Z_GRAVA_LOG_PM'
        EXPORTING
          I_TP_MSG   = 'W'
          I_MENSAGEM = LV_MSG
          I_TCODE    = 'ZPM0024'.

    ENDIF.

  ENDLOOP.

  LOOP AT TL_LOG.
    CONCATENATE 'Remov. >>> Categoria: ' TL_LOG-EQTYP INTO LV_MSG.

    CALL FUNCTION 'Z_GRAVA_LOG_PM'
      EXPORTING
        I_TP_MSG   = 'W'
        I_MENSAGEM = LV_MSG
        I_TCODE    = 'ZPM0024'.

  ENDLOOP.

ENDFORM.                    "FETCH_VALUE

*&---------------------------------------------------------------------*
*&      Form  DELETE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DELETE_VALUE.
  FREE TL_LOG.

  LOOP AT EXTRACT.
    IF EXTRACT+7(1) EQ 'M'.
      TL_LOG-EQTYP = EXTRACT+3(1).
      APPEND TL_LOG.
    ELSE.
      CONTINUE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "DELETE_VALUE
