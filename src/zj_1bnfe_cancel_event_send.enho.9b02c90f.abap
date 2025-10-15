"Name: \FU:J_1BNFE_CANCEL_EVENT_SEND\SE:BEGIN\EI
ENHANCEMENT 0 ZJ_1BNFE_CANCEL_EVENT_SEND.
*
  CONSTANTS: gc_model_mdfe TYPE j_1bmodel VALUE '58'.              "1890145

  IF IS_ACTTAB-model EQ gc_model_mdfe.

    CALL FUNCTION 'Z_J_1BMFE_CANCEL_EVENT_SEND'
      EXPORTING
        IV_ACCESS_KEY                = IV_ACCESS_KEY
        IS_ACTTAB                    = IS_ACTTAB
        IV_EVENT_GROUP               = IV_EVENT_GROUP
        IV_REASON                    = IV_REASON
        IV_RESEND                    = IV_RESEND
        IV_AUTHCODE                  = IV_AUTHCODE
        IV_ACTTAB_LINES              = IV_ACTTAB_LINES
        IV_INDEX_ACTTAB              = IV_INDEX_ACTTAB
        IV_NFTYPE                    = IV_NFTYPE
        IV_NFE_VERSION               = IV_NFE_VERSION
      IMPORTING
        EV_MSSTAT                    = EV_MSSTAT
        EV_RFCDEST                   = EV_RFCDEST
        ES_BAPIRET2                  = ES_BAPIRET2
     EXCEPTIONS
       RFC_ERROR                    = 1
       CLOUD_CONNECTION_ERROR       = 2
       OTHERS                       = 3
              .

    IF SY-SUBRC IS NOT INITIAL.
      PERFORM dequeue_event USING is_acttab-docnum.
      RAISE rfc_error.
    ENDIF.

    CHECK 1 EQ 2.

  ENDIF.


ENDENHANCEMENT.
