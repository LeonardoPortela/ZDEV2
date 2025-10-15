*----------------------------------------------------------------------*
*   INCLUDE LMB_BUS2017FC0                                             *
*----------------------------------------------------------------------*
*   Währung wird nur einmal bestimmt: dies ist nur erforderlich,
*   bei MB1x-Transaktionen und wenn externe Beträge gefüllt sind.
*   In MB1x-Transaktionen wird unterschiedliche Währung abgelehnt.
*   T_IMSEG-WAERS wird später wieder initialisiert, da diese nicht
*   extern mitgegeben werden darf. Hier jedoch für BAPI-Konvertierung
*   erforderlich.
*----------------------------------------------------------------------*
FORM CURRENCY_DETERMINATION TABLES   LOC_BAPIRET2 STRUCTURE BAPIRET2
                            USING    LOC_BAPIGMITEM STRUCTURE
                                     BAPI2017_GM_ITEM_CREATE
                                     LOC_ROW
                            CHANGING LOC_CURRENCY.
    IF ( LOC_BAPIGMITEM-MVT_IND IS INITIAL AND
       ( NOT LOC_BAPIGMITEM-AMOUNT_LC IS INITIAL OR
         NOT LOC_BAPIGMITEM-AMOUNT_SV IS INITIAL ) ) OR
       ( LOC_BAPIGMITEM-MVT_IND = 'B' AND
         NOT LOC_BAPIGMITEM-AMOUNT_SV IS INITIAL ).
      CALL FUNCTION 'READ_CURRENCY_OF_COMPANY'
         EXPORTING
              WERKS            = LOC_BAPIGMITEM-PLANT
         IMPORTING
              WAERS            = LOC_CURRENCY
         EXCEPTIONS
              NO_VALID_PLANT   = 1
              PLANT_NOT_FOUND  = 2
              BWKEY_NOT_FOUND  = 3
              BWKEY_WRONG_CALL = 4
              BUKRS_NOT_FOUND  = 5
              BUKRS_WRONG_CALL = 6
              ERROR_MESSAGE    = 100
              OTHERS           = 7.
      IF SY-SUBRC <> 0.
        GLOBAL_ERROR = TRUE.
        PERFORM SY_MSG_TO_BAPIRET2 TABLES LOC_BAPIRET2
                                   USING  LOC_ROW
                                          'GOODSMVT_ITEM'.
      ENDIF.
    ENDIF.
ENDFORM.
