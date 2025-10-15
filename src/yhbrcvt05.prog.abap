*&---------------------------------------------------------------------*
*&  Include           YHBRCVT05
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      Form  NUMBER_DAYS
*----------------------------------------------------------------------*
FORM NUMBER_DAYS USING    P_P0410_BEGDA
                          P_P0410_ENDDA
                          P_DAYS
                          P_AUSE
                          P_FERI.

  DATA: DAYS TYPE I,
        ausencia type i,
        ferias type i,
        abs_lin type i.

  DATA: BEGIN OF AB_PSP OCCURS 33.
          INCLUDE STRUCTURE PC2BA.
  DATA: END   OF AB_PSP.

  DATA: CONSIDER_P2003 VALUE 'N'.  " substitutions won't be considered

  clear : ausencia, days.


  DATA R_SUBTY TYPE RANGE OF SUBTY.
  DATA l_subty TYPE SUBTY.
  DATA ABSENCES TYPE RANGE OF SUBTY.

  CLEAR l_subty.

  REFRESH ABSENCES.

  SELECT *
    FROM tvarvc
    INTO @DATA(wTvarvc)
    WHERE name EQ 'ZHRBN_FERI_AUS'.

    APPEND VALUE #( sign = wTvarvc-sign OPTION = wTvarvc-OPTI LOW = wTvarvc-LOW high = wTvarvc-HIGH  ) TO R_SUBTY.

  ENDSELECT.


  SELECT *
    FROM tvarvc
    INTO wTvarvc
    WHERE name EQ 'ZHRBN_VT_AUSENCIAS'.

    APPEND VALUE #( sign = wTvarvc-sign OPTION = wTvarvc-OPTI LOW = wTvarvc-LOW high = wTvarvc-HIGH  ) TO ABSENCES.

  ENDSELECT.

  DATA T_DT_INI_ANT TYPE DATUM.
  DATA T_DT_FIM_ANT TYPE DATUM.

  T_DT_FIM_ANT = P_P0410_BEGDA - 1.
  T_DT_INI_ANT = |{ T_DT_FIM_ANT(6) }01|.

  T_DT_FIM_ANT = T_DT_INI_ANT - 1.
  T_DT_INI_ANT = |{ T_DT_FIM_ANT(6) }01|.


  PERFORM BUILD_PWS_FOR_GENPS
          TABLES AB_PSP
          USING  P_P0410_BEGDA
                 P_P0410_ENDDA
                 CONSIDER_P2003.

  LOOP AT AB_PSP WHERE STDAZ > 0 AND
                       TAGTY = 0 .
    DAYS = DAYS + 1.
  ENDLOOP.

* If absences table is empty, it's not necessary to loop over p2001.
* This means that all absences will be taken into account for the
* tickets calculation.
  describe table absences lines abs_lin.
  if abs_lin is initial.
    move days to p_days.
    exit.
  endif.

*   Ausências no mês anterior.
  LOOP AT P2001
  WHERE
  ALLDF = 'X'
  AND BEGDA <= T_DT_FIM_ANT
  AND ENDDA >= T_DT_INI_ANT
  AND SUBTY IN ABSENCES.
      LOOP AT AB_PSP WHERE STDAZ > 0 AND
                           TAGTY = 0 AND
                           DATUM >= P2001-BEGDA AND
                           DATUM <= P2001-ENDDA.
        ausencia = ausencia + 1.
      ENDLOOP.
  ENDLOOP.


* Férias
  LOOP AT P2001
  WHERE
  ALLDF = 'X'
  AND BEGDA <= P_P0410_ENDDA
  AND ENDDA >= P_P0410_BEGDA
  AND SUBTY IN R_SUBTY.
      LOOP AT AB_PSP WHERE STDAZ > 0 AND
                           TAGTY = 0 AND
                           DATUM >= P2001-BEGDA AND
                           DATUM <= P2001-ENDDA.
        ferias = ferias + 1.
      ENDLOOP.
  ENDLOOP.

  days = days - ausencia - ferias.

  if days lt 0.
    days = 0.
  endif.

MOVE DAYS TO P_DAYS.
MOVE ferias TO P_FERI.
MOVE ausencia to P_AUSE.

ENDFORM.                               " NUMBER_DAYS

*----------------------------------------------------------------------*
*    Form BUILD_PWS_FOR_GENPS
*    (New for 4.0C to replace old form build_psp)
*    (Copyed from include RPCISS00 - program HBRCALC0)
*----------------------------------------------------------------------*
FORM BUILD_PWS_FOR_GENPS TABLES PSP
                         USING  BEGDA
                                ENDDA
                                CONSIDER_P2003.

DATA: M2003 LIKE P2003 OCCURS 1,
      MIN_BEGDA LIKE PN-BEGDA,
      MAX_ENDDA LIKE PN-ENDDA,
      YES VALUE 'Y'.
*      no  value 'N'.

  REFRESH PSP.

  IF CONSIDER_P2003 = YES.
    IF BEGDA < PN-BEGDA OR ENDDA > PN-ENDDA.
      IF BEGDA < PN-BEGDA.
        MIN_BEGDA = BEGDA.
      ELSE.
        MIN_BEGDA = PN-BEGDA.
      ENDIF.
      IF ENDDA > PN-ENDDA.
        MAX_ENDDA = ENDDA.
      ELSE.
        MAX_ENDDA = PN-ENDDA.
      ENDIF.
      RP_READ_INFOTYPE PERNR-PERNR 2003 P2003 MIN_BEGDA MAX_ENDDA.
      PERFORM CONVERT_P2003_CURRENCY(HBRCALC0)
                                     USING MIN_BEGDA MAX_ENDDA.
    ENDIF.
    M2003[] = P2003[].
  ELSE.                                      "CONSIDER_P2003 = NO
    MIN_BEGDA = BEGDA.
    MAX_ENDDA = ENDDA.
  ENDIF.

  PERFORM init_payroll_log(hbrcalc0).

  PERFORM BUILD_PWS(HBRCALC0) TABLES PSP
                           P0000
                           P0001
                           P0002
                           P0007
                           P2001
                           P2002
                           M2003
                    USING  MIN_BEGDA
                           MAX_ENDDA.

ENDFORM.                                 " BUILD_PWS_FOR_GENPS
