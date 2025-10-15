FUNCTION Z_LES_DOLAR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DATA) TYPE  SY-DATUM
*"     REFERENCE(P_KURST) TYPE  KURST
*"     REFERENCE(P_WAERK) TYPE  WAERK
*"     REFERENCE(P_TCURR) TYPE  TCURR_CURR
*"  EXPORTING
*"     REFERENCE(E_VLR_DOLAR) TYPE  UKURS_CURR
*"----------------------------------------------------------------------

  DATA: TRUE     TYPE C VALUE 0,
        WL_TCURR TYPE TCURR,
        V_DATA   TYPE C LENGTH 10,
        DATA     TYPE SY-DATUM,
        DIA      TYPE C LENGTH 2,
        MES      TYPE C LENGTH 2,
        ANO      TYPE C LENGTH 4.

  WHILE TRUE EQ 0.

    CLEAR: DATA, WL_TCURR.

    IF ( V_DATA IS INITIAL ).
      CONCATENATE P_DATA+6(2) P_DATA+4(2) P_DATA(4) INTO V_DATA.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = V_DATA
      IMPORTING
        OUTPUT = DATA.

    SELECT  SINGLE * FROM TCURR
      INTO WL_TCURR
      WHERE KURST EQ P_KURST
        AND FCURR EQ P_WAERK
        AND TCURR EQ P_TCURR
        AND GDATU EQ DATA.

    IF ( SY-SUBRC EQ 0 ).
      E_VLR_DOLAR = WL_TCURR-UKURS.
      TRUE = 1.
    ELSE.

      CASE V_DATA(2).

        WHEN: 01.

          CLEAR: DIA, MES, ANO, DATA.
          DATA = V_DATA.

          DIA = 31.
          MES = DATA+2(2) - 1.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = MES
            IMPORTING
              OUTPUT = MES.

          ANO = DATA+4(4).

          CLEAR: V_DATA.
          CONCATENATE DIA MES ANO INTO V_DATA.

        WHEN OTHERS.

          CLEAR: DIA, MES, ANO, DATA.
          DATA = V_DATA.

          DIA = DATA(2) - 1.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = DIA
            IMPORTING
              OUTPUT = DIA.

          MES = DATA+2(2).
          ANO = DATA+4(4).
          CLEAR: V_DATA.
          CONCATENATE DIA MES ANO INTO V_DATA.

      ENDCASE.

    ENDIF.

  ENDWHILE.

ENDFUNCTION.
