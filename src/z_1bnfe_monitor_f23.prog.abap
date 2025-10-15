*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F23
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  check_authorization_profile
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_AUTHORIZATION_PROFILE .

  DATA: TL_PARAMETROS TYPE USTYP_T_PARAMETERS.

* Authorization for activity 85 (reverse) to
* -> Request an authorization to cancel an authorized NF-e
* -> Request an authorization to cancel a rejected NF-e
* -> Set Contingency per Region or per Business Place
  AUTHORITY-CHECK OBJECT 'F_NFBA'
                  ID 'ACTVT' FIELD '85'.
  IF SY-SUBRC = 0.
    GF_AUTHORIZATION_NFE_85 = C_X.
  ENDIF.

* Authorization for activity 35 (re-print) to
* -> Send NF-e with SCS ' ' (not sent)
* -> Set Contingency per NF-e
* -> Reset contingency per NF-e
* -> Delete NF-e log entries
  AUTHORITY-CHECK OBJECT 'F_NFBA'
                  ID 'ACTVT' FIELD '35'.
  IF SY-SUBRC = 0.
    GF_AUTHORIZATION_NFE_35 = C_X.
  ENDIF.

* Authorization for activity 03 (display) to
* -> display NF-e
  AUTHORITY-CHECK OBJECT 'F_NFBA'
                  ID 'ACTVT' FIELD '03'.
  IF SY-SUBRC = 0.
    GF_AUTHORIZATION_NFE_03 = C_X.
  ENDIF.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      USER_NAME           = SY-UNAME
    TABLES
      USER_PARAMETERS     = TL_PARAMETROS
    EXCEPTIONS
      USER_NAME_NOT_EXIST = 1
      OTHERS              = 2.

  READ TABLE TL_PARAMETROS WITH KEY PARID = 'ZENC_MDFE' TRANSPORTING NO FIELDS.
  IF SY-SUBRC IS INITIAL.
    GF_AUTHORIZATION_MDF_01 = C_X.
  ENDIF.

  READ TABLE TL_PARAMETROS WITH KEY PARID = 'ZEMI_MDFE' TRANSPORTING NO FIELDS.
  IF SY-SUBRC IS INITIAL.
    GF_AUTHORIZATION_MDF_02 = C_X.
  ENDIF.

ENDFORM.                    " check_authorization_profile
