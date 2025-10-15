"Name: \PR:SAPLCOMK\FO:CHECK_MATSTAT_FOR_PURCH\SE:BEGIN\EI
ENHANCEMENT 0 Z_IW_MAT_OBSOLETO_PM_COMPRAS.

  DATA: WA_CAUFVD TYPE CAUFVD,
        CK_COND   TYPE C.

  FIELD-SYMBOLS: <CAUFVD> TYPE ANY.

  IF SY-TCODE EQ 'IW31' OR SY-TCODE EQ 'IW32' OR SY-TCODE EQ 'IW34' OR SY-TCODE EQ 'IW21' OR SY-TCODE EQ 'IW22'.

      ASSIGN ('(SAPLCOIH)CAUFVD') TO <CAUFVD>.

      IF <CAUFVD> IS ASSIGNED.
        WA_CAUFVD = <CAUFVD>.
        IF ( WA_CAUFVD-STTXT(3) EQ 'LIB' OR WA_CAUFVD-STTXT(3) EQ 'ABE' ) AND WA_CAUFVD-AUTYP EQ '30'.
          MOVE ABAP_TRUE TO CK_COND.
        ENDIF.
      ENDIF.

  ENDIF.

  IF CK_COND EQ ABAP_TRUE.

    DATA: T141WA_2    LIKE T141,
          MTSTB_TMP_2 LIKE T141T-MTSTB.

    CLEAR E_SUBRC.

*  ------- Pruefen Status ---- ( Werksebene ) ---------------------------*
    IF I_MSFCV-MMSTA NE SPACE.
      IF I_MSFCV-MMSTD IS INITIAL OR I_MSFCV-MMSTD LE SY-DATLO.
        CALL FUNCTION 'CO_TA_T141_READ'
          EXPORTING
            T141_MMSTA = I_MSFCV-MMSTA
          IMPORTING
            T141WA     = T141WA_2
          EXCEPTIONS
            NOT_FOUND  = 1
            OTHERS     = 2.
        IF SY-SUBRC NE 0.
          MESSAGE E052(ME) WITH I_MSFCV-MMSTA
                                I_MTCOM-MATNR.
        ENDIF.
        CALL FUNCTION 'CO_TA_T141T_READ'
          EXPORTING
            MMSTA     = I_MSFCV-MMSTA
            SPRAS     = SY-LANGU
          IMPORTING
            TEXT      = MTSTB_TMP_2
          EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
*  ------- Modificação CS2016001207
        clear T141WA_2-DEINK.
*  ------- Modificação CS2016001207
        CASE T141WA_2-DEINK.
          WHEN 'B'.                      "Error
            MESSAGE I053(ME) WITH MTSTB_TMP_2
                                  I_MTCOM-MATNR.
            E_SUBRC = 4.
          WHEN 'A'.                      "Dialog
            MESSAGE S054(ME)
                    WITH I_MTCOM-MATNR
                         I_MTCOM-WERKS
                         MTSTB_TMP_2.
        ENDCASE.
      ENDIF.
    ENDIF.

*  ------- Pruefen Status ---- ( Mandantenebene ) -----------------------*
    IF I_MSFCV-MSTAE NE SPACE.
      IF I_MSFCV-MMSTD IS INITIAL OR I_MSFCV-MMSTD LE SY-DATLO.
        CALL FUNCTION 'CO_TA_T141_READ'
          EXPORTING
            T141_MMSTA = I_MSFCV-MSTAE
          IMPORTING
            T141WA     = T141WA_2
          EXCEPTIONS
            NOT_FOUND  = 1
            OTHERS     = 2.
        IF SY-SUBRC NE 0.
          MESSAGE E052(ME) WITH I_MSFCV-MSTAE I_MTCOM-MATNR.
        ENDIF.
        CALL FUNCTION 'CO_TA_T141T_READ'
          EXPORTING
            MMSTA     = I_MSFCV-MSTAE
            SPRAS     = SY-LANGU
          IMPORTING
            TEXT      = MTSTB_TMP_2
          EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
*Inicio de alteração - fmartins - CS1044340 - 07/12/2022
**  ------- Modificação CS2017000674
*        CLEAR T141WA_2-DEINK.
**  ------- Modificação CS2017000674
*Fim de alteração - fmartins - CS1044340 - 07/12/2022
        CASE T141WA_2-DEINK.
          WHEN 'B'.                      "Error
            MESSAGE I053(ME) WITH MTSTB_TMP_2 I_MTCOM-MATNR.
            E_SUBRC = 4.
          WHEN 'A'.                      "Dialog
            MESSAGE S054(ME) WITH I_MTCOM-MATNR I_MTCOM-WERKS MTSTB_TMP_2.
        ENDCASE.
      ENDIF.
    ENDIF.

    CHECK 1 = 2.

  ENDIF.

ENDENHANCEMENT.
