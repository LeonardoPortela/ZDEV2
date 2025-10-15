*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F27
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  send_nfe_again
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_NFE_AGAIN .

  DATA: CALL_SUBRC TYPE C,
        VL_OK      TYPE C LENGTH 1.

  IF IT_SELECTED_ROWS IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
    RETURN.
  ENDIF.

* Check authorization
  IF GF_AUTHORIZATION_NFE_35 IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.
  ENDIF.

* Re-Sending of selected NF-es
  CLEAR SUBRC.
  REFRESH IT_ACTIVE_MOD.
  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.

    READ TABLE IT_NFE_ALV INTO WA_NFE_ALV INDEX WA_SELECTED_ROWS-INDEX.

    IF SY-TCODE EQ 'ZCTE'.
      CALL METHOD ZCL_REPOM_VIAGEM_VPR=>VERIFICA_CUSTO_VI
        EXPORTING
          I_DOCNUM_CTE = WA_NFE_ALV-DOCNUM
        EXCEPTIONS
          CUSTO_VI     = 1
          OTHERS       = 2.
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.

    PERFORM Z_VERIFICA_FATURA_CANCELADA USING WA_NFE_ALV-DOCNUM CHANGING VL_OK.

    IF VL_OK EQ 'N'.
      MESSAGE 'a fatura desta nf esta cancelada !' TYPE 'E' .
      RETURN.
    ENDIF.

    PERFORM CHECK_RESEND_ALLOWED USING SUBRC C_X.
*                                                              "1340272
* check if NF-e is already numbered when check_resend_allowed  "1340272
* was OK (subrc=0) - otherwise process is not allowed          "1340272
    IF SUBRC IS INITIAL.                                    "1340272
      PERFORM CHECK_NUMBERED_NFE USING WA_NFE_ALV-DOCNUM    "1340272
                                       WA_NFE_ALV-NFNUM9    "1340272
                                       C_2                  "1340272
                                 CHANGING SUBRC.            "1340272
    ENDIF.                                                  "1340272
*                                                              "1340272
*   Call messaging system when resend is allowed
    IF SUBRC IS INITIAL.
      CALL FUNCTION 'J_1B_NFE_SEND_C_NFE'
        EXPORTING
          IV_DOCNUM           = WA_NFE_ALV-DOCNUM
          IV_RESEND           = C_X
        IMPORTING
          ES_ACTIVE_MOD       = WA_ACTIVE_MOD
        EXCEPTIONS
          NOT_SENT            = 1
          NOT_ALLOWED_TO_SEND = 2
          OTHERS              = 3.

      IF SY-SUBRC <> 0.
        CALL_SUBRC = C_X.
      ELSE.
        APPEND WA_ACTIVE_MOD TO IT_ACTIVE_MOD.
      ENDIF.
    ELSE.
      CALL_SUBRC = SUBRC.
    ENDIF.
  ENDLOOP.

* Update ALV display
  PERFORM GRID_UPDATE USING SPACE.

  IF NOT CALL_SUBRC IS INITIAL.
    CASE SY-TCODE.
      WHEN 'ZNFE'.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024'.
      WHEN 'ZCTE'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER '004'.
    ENDCASE.
  ELSE.
    CASE SY-TCODE.
      WHEN 'ZNFE'.
        MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '027'.
      WHEN 'ZCTE'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'S' NUMBER '003'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " send_nfe_again

*&---------------------------------------------------------------------*
*&      Form  check_resend_allowed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0051   text
*----------------------------------------------------------------------*
FORM CHECK_RESEND_ALLOWED USING P_CHECK TYPE CHAR1
                                P_MODE  TYPE CHAR1.

  CLEAR P_CHECK.

* Check SCS
  CASE P_MODE.                                              "1161951
    WHEN C_X.             "resend NF-e                      "1161951
      IF WA_NFE_ALV-SCSSTA NA '0'.                          "1161951
        P_CHECK = WA_NFE_ALV-SCSSTA.                        "1161951
                                                            "1161951
        SY-MSGTY = 'E'.                                     "1161951
        SY-MSGID = 'J1B_NFE'.                               "1161951
        SY-MSGNO = '081'.                                   "1161951
        SY-MSGV1 = WA_NFE_ALV-DOCNUM.                       "1161951
        SY-MSGV2 = P_CHECK.                                 "1161951
                                                            "1161951
        CALL FUNCTION 'J_1B_NFE_ERROR_PROTOKOLL'            "1161951
          EXPORTING                                         "1161951
            I_DOCNUM = WA_NFE_ALV-DOCNUM.                   "1161951
      ENDIF.                                                "1161951
      IF NOT WA_NFE_ALV-DOCSTA IS INITIAL.                  "1161951
        P_CHECK = WA_NFE_ALV-DOCSTA.                        "1161951
                                                            "1161951
        SY-MSGTY = 'E'.                                     "1161951
        SY-MSGID = 'J1B_NFE'.                               "1161951
        SY-MSGNO = '082'.                                   "1161951
        SY-MSGV1 = WA_NFE_ALV-DOCNUM.                       "1161951
        SY-MSGV2 = P_CHECK.                                 "1161951
                                                            "1161951
        CALL FUNCTION 'J_1B_NFE_ERROR_PROTOKOLL'            "1161951
          EXPORTING                                         "1161951
            I_DOCNUM = WA_NFE_ALV-DOCNUM.                   "1161951
      ENDIF.                                                "1161951
    WHEN SPACE.             "request cancellation again     "1161951
      IF WA_NFE_ALV-SCSSTA NA '13'.                         "1161951
        P_CHECK = WA_NFE_ALV-SCSSTA.                        "1161951

        SY-MSGTY = 'E'.
        SY-MSGID = 'J1B_NFE'.
        SY-MSGNO = '074'.
        SY-MSGV1 = WA_NFE_ALV-DOCNUM.
        SY-MSGV2 = P_CHECK.

        CALL FUNCTION 'J_1B_NFE_ERROR_PROTOKOLL'
          EXPORTING
            I_DOCNUM = WA_NFE_ALV-DOCNUM.
      ENDIF.                                                "1161951
  ENDCASE.                                                  "1161951


  CHECK P_CHECK IS INITIAL.

* Check if Messaging System already received a request
  CASE WA_NFE_ALV-SCSSTA.
    WHEN C_SCS_0.
      IF WA_NFE_ALV-MSSTAT = C_MSS_A.
        P_CHECK = C_MSS_A.
      ENDIF.
    WHEN C_SCS_1.
      IF WA_NFE_ALV-MSSTAT = C_MSS_B.
        P_CHECK = C_MSS_B.
      ENDIF.
    WHEN C_SCS_3.
      IF WA_NFE_ALV-MSSTAT = C_MSS_C.
        P_CHECK = C_MSS_C.
      ENDIF.
  ENDCASE.

  IF NOT P_CHECK IS INITIAL.
*   Set message with error analysis
    SY-MSGTY = 'E'.
    SY-MSGID = 'J1B_NFE'.
    SY-MSGNO = '073'.
    SY-MSGV1 = WA_NFE_ALV-DOCNUM.
    SY-MSGV2 = P_CHECK.

    CALL FUNCTION 'J_1B_NFE_ERROR_PROTOKOLL'
      EXPORTING
        I_DOCNUM = WA_NFE_ALV-DOCNUM.
  ENDIF.

ENDFORM.                    " check_resend_allowed

*&---------------------------------------------------------------------*
*&      Form  REQUEST_CANCELLATION_AGAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
* New with note 1161951
FORM REQUEST_CANCELLATION_AGAIN .

  DATA: WA_ACTIVE_LOG TYPE J_1BNFE_ACTIVE.

* Check authorization
  IF GF_AUTHORIZATION_NFE_35 IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.
  ENDIF.

  CASE SY-TCODE.
    WHEN 'ZNFE'.
      AUTHORITY-CHECK OBJECT 'ZSDCANCNFE' ID 'Z_CANC_NFE' FIELD '1'.
    WHEN 'ZCTE'.
      AUTHORITY-CHECK OBJECT 'ZSDCANCCTE' ID 'Z_CANC_CTE' FIELD '1'.
  ENDCASE.

  IF SY-SUBRC NE 0.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.
    RETURN.
  ENDIF.

  CLEAR SUBRC.
  CLEAR IT_ALV_ERROR.
  REFRESH IT_ACTIVE_MOD.

* Check if it is allowed to send the cancellation requests again
  LOOP AT IT_SELECTED_ROWS INTO WA_SELECTED_ROWS.
    READ TABLE IT_NFE_ALV INTO WA_NFE_ALV INDEX WA_SELECTED_ROWS-INDEX.

    PERFORM CHECK_RESEND_ALLOWED USING SUBRC SPACE.

    SELECT SINGLE * INTO @DATA(WA_J_1BNFE_ACTIVE)
      FROM J_1BNFE_ACTIVE WHERE DOCNUM EQ @WA_NFE_ALV-DOCNUM.

    IF WA_J_1BNFE_ACTIVE-DOCSTA EQ '1'.
      CASE WA_J_1BNFE_ACTIVE-MODEL.
        WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_CTE.
          AUTHORITY-CHECK OBJECT 'ZSDCANCCTE' ID 'Z_CANC_CTE' FIELD '1'.
        WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_NFE.
          AUTHORITY-CHECK OBJECT 'ZSDCANCNFE' ID 'Z_CANC_NFE' FIELD '1'.
        WHEN ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~AT_ST_MODEL_MDFE.
          AUTHORITY-CHECK OBJECT 'ZSDCANCMFE' ID 'Z_CANC_MFE' FIELD '1'.
      ENDCASE.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '041'.
      ENDIF.
    ENDIF.


  ENDLOOP.

  IF NOT SUBRC IS INITIAL.
    PERFORM GRID_UPDATE USING SPACE.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024'.
  ELSE.
* Resend cancellation request for selected NF-es

    CASE SY-TCODE.
      WHEN 'ZCTE'.
        LOOP AT IT_ALV_SELECTION ASSIGNING FIELD-SYMBOL(<FS_ALV_SEL>).
          IF <FS_ALV_SEL>-REASON IS INITIAL.

            SELECT SINGLE *
              FROM ZCTE_IDENTIFICA INTO @DATA(WL_IDENTIFICA)
             WHERE DOCNUM EQ @<FS_ALV_SEL>-DOCNUM.

            IF ( SY-SUBRC EQ 0 ) AND ( WL_IDENTIFICA-REASON IS NOT INITIAL ).
              <FS_ALV_SEL>-REASON  = WL_IDENTIFICA-REASON.
              <FS_ALV_SEL>-REASON1 = WL_IDENTIFICA-REASON1.
              <FS_ALV_SEL>-REASON2 = WL_IDENTIFICA-REASON2.
              <FS_ALV_SEL>-REASON3 = WL_IDENTIFICA-REASON3.
              <FS_ALV_SEL>-REASON4 = WL_IDENTIFICA-REASON4.
            ENDIF.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    CALL FUNCTION 'J_1B_NFE_SEND_REQUESTS'
      EXPORTING
        IV_RESEND     = C_X
      TABLES
        IT_ACTTAB     = IT_ALV_SELECTION
        ET_ERRTAB     = IT_ALV_ERROR
        ET_ACTIVE_MOD = IT_ACTIVE_MOD
        IT_ACTTAB_MOD = IT_ALV_SELECTION_MOD
      EXCEPTIONS
        STATUS_ERROR  = 1
        RFC_FAILURE   = 2
        OTHERS        = 3.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      LOOP AT IT_ALV_SELECTION INTO WA_ACTIVE_LOG.
        MOVE-CORRESPONDING WA_ACTIVE_LOG TO WA_NFE_ALV.
        PERFORM REGISTRA_ENVIO_DATA USING WA_NFE_ALV C_X.
      ENDLOOP.
    ENDIF.
  ENDIF.

* COMMIT has been moved inside function J_1B_NFE_SEND_REQUESTS "1259918
*  IF it_alv_error IS INITIAL.                                 "1259918
*    COMMIT WORK.                                              "1259918
*  ENDIF.                                                      "1259918


* Update ALV display
  PERFORM GRID_UPDATE USING SPACE.

  IF NOT IT_ALV_ERROR IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024'.
  ELSE.
    MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '027'.
  ENDIF.

ENDFORM.                    " REQUEST_CANCELLATION_AGAIN
