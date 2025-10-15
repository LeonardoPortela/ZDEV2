*----------------------------------------------------------------------*
***INCLUDE ZHCMR_PA0013_AD_ENVIAR_EMAIF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZHCMT0007_AD  text
*      <--P_WA_ZHCMT0007_AD_CK_EXECUTADO  text
*----------------------------------------------------------------------*
FORM ENVIAR_EMAIL  USING    P_ZHCMT0007_AD TYPE ZHCMT0007_AD
                   CHANGING CK_EXECUTADO TYPE CHAR01
                            PMAIL TYPE STRING.

*  DATA: LO_CREATE_MAIL TYPE REF TO CL_CRM_EMAIL_DATA,
*        LV_ACTIVITY    TYPE SYSUUID_X,
*        LS_MAIL_BODY   TYPE CRMS_EMAIL_MIME_STRUC,
*        LT_MAIL_BODY   TYPE CRMT_EMAIL_MIME_STRUC,
*        LS_RECEP       TYPE CRMS_EMAIL_RECIPIENT,
*        LT_TO          TYPE CRMT_EMAIL_RECIPIENTS.

  IF WA_ZHCMT0007_AD-DS_EMAIL IS INITIAL.
    SELECT SINGLE * INTO @DATA(WA_0007)
      FROM ZHCMT0007
     WHERE PERNR EQ @WA_ZHCMT0007_AD-PERNR.

    IF SY-SUBRC IS INITIAL AND WA_0007-SUP_EMAIL IS NOT INITIAL.
      PMAIL = WA_0007-SUP_EMAIL.
      TRANSLATE PMAIL TO LOWER CASE.
      WA_ZHCMT0007_AD-DS_EMAIL = PMAIL.
    ENDIF.
  ENDIF.

  IF WA_ZHCMT0007_AD-DS_EMAIL IS NOT INITIAL.

*    CREATE OBJECT LO_CREATE_MAIL.
*
*    LO_CREATE_MAIL->SUBJECT  = 'SAP HCM - Usuário Active Directory'.
*
*    CLEAR LS_MAIL_BODY.
*    LS_MAIL_BODY-CONTENT_ASCII = P_ZHCMT0007_AD-TX_EMAIL.
*    LS_MAIL_BODY-MIME_TYPE     = 'text/html'.
*    APPEND  LS_MAIL_BODY TO LT_MAIL_BODY.
*    MOVE LT_MAIL_BODY TO LO_CREATE_MAIL->BODY.
*
*    IF SY-SYSID EQ 'PRD'.
*      LS_RECEP-ADDRESS = P_ZHCMT0007_AD-DS_EMAIL.
*      APPEND LS_RECEP TO LT_TO.
*      MOVE LT_TO TO LO_CREATE_MAIL->TO.
*    ELSE.
*      LS_RECEP-ADDRESS = 'suporte.sap@amaggi.com.br'.
*      APPEND LS_RECEP TO LT_TO.
*      MOVE LT_TO TO LO_CREATE_MAIL->TO.
*    ENDIF.
*
*    CLEAR LS_RECEP.
*    "LS_RECEP-ADDRESS = 'suporte.sap@amaggi.com.br'.
*    "MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.
*
*    CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL
*      EXPORTING
*        IV_MAIL_DATA       = LO_CREATE_MAIL
*      RECEIVING
*        EV_SEND_REQUEST_ID = LV_ACTIVITY.
*
*    CLEAR: LO_CREATE_MAIL.
*
*    CK_EXECUTADO = ABAP_TRUE.

    DATA: LT_CONTENTS  TYPE SOLI_TAB,
          LC_USER_MAIL TYPE UNAME,
          LV_SUBJECT   TYPE SO_OBJ_DES,
          IT_EMAIL     TYPE TABLE OF ADR6.

    TRY .
        LC_USER_MAIL = ZCL_JOB=>GET_USER_JOB( ).
        LV_SUBJECT   = |SAP HCM - Usuário Active Directory - { SY-SYSID }|.
        CLEAR: IT_EMAIL[].

        IF SY-SYSID EQ 'PRD'.
          APPEND VALUE #( SMTP_ADDR = P_ZHCMT0007_AD-DS_EMAIL ) TO IT_EMAIL.
          "APPEND VALUE #( SMTP_ADDR = 'tatiane.coutinho@amaggi.com.br' ) TO IT_EMAIL.
          "APPEND VALUE #( SMTP_ADDR = 'debora.arruda@amaggi.com.br' ) TO IT_EMAIL.
          "APPEND VALUE #( SMTP_ADDR = 'carolini.santos@amaggi.com.br' ) TO IT_EMAIL.
        ELSE.
          APPEND VALUE #( SMTP_ADDR = 'suporte.sap@amaggi.com.br' ) TO IT_EMAIL.
        ENDIF.

        ZCL_STRING=>STRING_TO_TABLE(
          EXPORTING
            I_STRING      = P_ZHCMT0007_AD-TX_EMAIL
            I_LINE_LENGTH = 255
          IMPORTING
            E_TABLE       = LT_CONTENTS
        ).

        CALL FUNCTION 'Z_SEND_MAIL'
          IMPORTING
            P_OUTENVIADO = CK_EXECUTADO
          TABLES
            LT_CONTENTS  = LT_CONTENTS
            LT_SMTPADR   = IT_EMAIL
          CHANGING
            I_USER       = LC_USER_MAIL
            I_SUBJECT    = LV_SUBJECT.

      CATCH ZCX_JOB.
    ENDTRY.

  ENDIF.

ENDFORM.
