*&---------------------------------------------------------------------*
*& Report  ZBASIS_LIBERA_FUNCAO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZBASIS_LIBERA_FUNCAO.

DATA: LC_CT_CDHDR          TYPE CDHDR_TAB,
      I_TAB_CONVERTED_DATA TYPE TRUXS_T_TEXT_DATA,
      I_INSTANCENUMBER     LIKE INSTANZ-SYSTEMNR,
      RFCSI_EXPORT         LIKE RFCSI.

PARAMETERS: PUNAME  TYPE UNAME OBLIGATORY,
            PFUNCAO TYPE AGR_NAME OBLIGATORY,
            PFROMD  TYPE MENU_DATE,
            PTOD    TYPE MENU_DATE,
            PFROMT  TYPE ZDE_TIME_INICIO,
            PTOT    TYPE ZDE_TIME_FINAL,
            PREMOVE TYPE CHAR01,
            PIDSOL  TYPE ZDE_SOLI_FUNCAO.

START-OF-SELECTION.

  CASE PREMOVE.
    WHEN ABAP_FALSE.

      CALL FUNCTION 'ZBASIS_SOL_ACESSO_FUNCAO'
        EXPORTING
          I_UNAME    = PUNAME
          I_AGR_NAME = PFUNCAO
          I_FROM_DAT = PFROMD
          I_TO_DAT   = PTOD
          I_IDSOL    = PIDSOL.

    WHEN ABAP_TRUE.

      CALL FUNCTION 'ZBASIS_SOL_ACESSO_FUNCAO_DEL'
        EXPORTING
          I_UNAME    = PUNAME
          I_AGR_NAME = PFUNCAO
          I_IDSOL    = PIDSOL.

*      "Gerar RSSCD100
*      SELECT SINGLE * INTO @DATA(WA_ZBASIS_USER_ADDF)
*        FROM ZBASIS_USER_ADDF
*       WHERE ID_SOLICITACAO EQ @PIDSOL.
*
*      IF SY-SUBRC IS INITIAL.
*
*        CALL FUNCTION 'CHANGEDOCUMENT_READ_ALL'
*          EXPORTING
*            I_OBJECTCLASS              = '*'
*            I_DATE_OF_CHANGE           = WA_ZBASIS_USER_ADDF-FROM_DAT
*            I_TIME_OF_CHANGE           = WA_ZBASIS_USER_ADDF-FROM_TIME
*            I_DATE_UNTIL               = WA_ZBASIS_USER_ADDF-TO_DAT
*            I_TIME_UNTIL               = '235959'
*            I_USERNAME                 = WA_ZBASIS_USER_ADDF-UNAME
*          CHANGING
*            CT_CDHDR                   = LC_CT_CDHDR
*          EXCEPTIONS
*            MISSING_INPUT_OBJECTCLASS  = 1
*            MISSING_INPUT_HEADER       = 2
*            NO_POSITION_FOUND          = 3
*            WRONG_ACCESS_TO_ARCHIVE    = 4
*            TIME_ZONE_CONVERSION_ERROR = 5
*            READ_TOO_MANY_ENTRIES      = 6
*            OTHERS                     = 7.
*
*        IF SY-SUBRC IS NOT INITIAL.
*
*          CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
*            EXPORTING
*              I_LINE_HEADER        = ABAP_TRUE
*              I_FIELD_SEPERATOR    = ';'
*            TABLES
*              I_TAB_SAP_DATA       = LC_CT_CDHDR
*            CHANGING
*              I_TAB_CONVERTED_DATA = I_TAB_CONVERTED_DATA
*            EXCEPTIONS
*              CONVERSION_FAILED    = 1
*              OTHERS               = 2.
*
*          IF SY-SUBRC IS INITIAL.
*
*            DATA: LO_CREATE_MAIL  TYPE REF TO CL_CRM_EMAIL_DATA,
*                  LS_MAIL_BODY    TYPE CRMS_EMAIL_MIME_STRUC,
*                  I_HTML_ENTRA    TYPE STRING,
*                  LS_RECEP        TYPE CRMS_EMAIL_RECIPIENT,
*                  LV_ACTIVITY     TYPE SYSUUID_X,
*                  LC_TP_TEXT      TYPE LVC_T_1022,
*                  OUTPUT_LENGTH   TYPE I,
*                  LT_BINARY       TYPE STANDARD TABLE OF BAPICONTEN,
*                  LV_HTML_XSTRING TYPE XSTRING,
*                  CT_FIELDCAT     TYPE LVC_T_FCAT,
*                  LC_TEXT_CAP     TYPE STRING.
*
*            CREATE OBJECT LO_CREATE_MAIL.
*            CLEAR: LO_CREATE_MAIL->SUBJECT.
*            CONCATENATE 'Log de Usuário' WA_ZBASIS_USER_ADDF-SE_RECORDID INTO LO_CREATE_MAIL->SUBJECT SEPARATED BY SPACE.
*
*            CLEAR LS_MAIL_BODY.
*            LS_MAIL_BODY-CONTENT_ASCII = I_HTML_ENTRA.
*            LS_MAIL_BODY-MIME_TYPE     = 'text/html'.
*            APPEND  LS_MAIL_BODY TO LO_CREATE_MAIL->BODY.
*
*            CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*              EXPORTING
*                I_STRUCTURE_NAME = 'CDHDR'
*              CHANGING
*                CT_FIELDCAT      = CT_FIELDCAT.
*
*            LC_TEXT_CAP = ''.
*            LOOP AT CT_FIELDCAT INTO DATA(WA_FIELDCAT).
*              IF LC_TEXT_CAP IS INITIAL.
*                LC_TEXT_CAP = WA_FIELDCAT-SCRTEXT_L.
*              ELSE.
*                CONCATENATE LC_TEXT_CAP ';' WA_FIELDCAT-SCRTEXT_L INTO LC_TEXT_CAP.
*              ENDIF.
*            ENDLOOP.
*            LC_TEXT_CAP = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = LC_TEXT_CAP ).
*            APPEND LC_TEXT_CAP TO LC_TP_TEXT.
*
*            LOOP AT I_TAB_CONVERTED_DATA INTO DATA(WA_TAB_CONVERTED_DATA).
*              APPEND WA_TAB_CONVERTED_DATA TO LC_TP_TEXT.
*            ENDLOOP.
*
*            CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
*              EXPORTING
*                MIMETYPE      = 'text/html'
*              IMPORTING
*                OUTPUT_LENGTH = OUTPUT_LENGTH
*              TABLES
*                TEXT_TAB      = LC_TP_TEXT
*                BINARY_TAB    = LT_BINARY
*              EXCEPTIONS
*                FAILED        = 1
*                OTHERS        = 2.
*
*            IF SY-SUBRC IS INITIAL.
*              LOOP AT LT_BINARY INTO DATA(WA_BINARY).
*                CONCATENATE LV_HTML_XSTRING WA_BINARY-LINE INTO LV_HTML_XSTRING IN BYTE MODE.
*              ENDLOOP.
*              CLEAR LS_MAIL_BODY.
*              LS_MAIL_BODY-IS_ATTACHMENT = 'X'.
*              LS_MAIL_BODY-FILE_NAME     = 'Arquivo.csv'.
*              LS_MAIL_BODY-MIME_TYPE     = 'application/csv'.
*              LS_MAIL_BODY-CONTENT_BIN   = LV_HTML_XSTRING.
*              APPEND LS_MAIL_BODY TO LO_CREATE_MAIL->BODY.
*            ENDIF.
*
*            CLEAR LS_RECEP.
*            LS_RECEP-ADDRESS = 'marcus.barbara@amaggi.com.br'.
*            APPEND LS_RECEP TO LO_CREATE_MAIL->TO.
*
*            CLEAR LS_RECEP.
*            LS_RECEP-ADDRESS = 'marcus.barbara@amaggi.com.br'.
*            MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.
*
*            CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL
*              EXPORTING
*                IV_MAIL_DATA       = LO_CREATE_MAIL
*              RECEIVING
*                EV_SEND_REQUEST_ID = LV_ACTIVITY.
*          ENDIF.
*        ENDIF.
*
*        CALL FUNCTION 'GET_SYSTEM_NUMBER'
*          IMPORTING
*            INSTANCENUMBER = I_INSTANCENUMBER.
*
*        CONCATENATE SY-HOST '_' SY-SYSID '_' I_INSTANCENUMBER INTO DATA(I_DESTINATION).
*        CONDENSE I_DESTINATION NO-GAPS.
*
*        CALL FUNCTION 'RFC_GET_SYSTEM_INFO'
*          EXPORTING
*            DESTINATION             = I_DESTINATION
*          IMPORTING
*            RFCSI_EXPORT            = RFCSI_EXPORT
**           RFC_LOGIN_COMPLETE      =
**           DIALOG_USER_TYPE        =
**           CURRENT_RESOURCES       =
**           MAXIMAL_RESOURCES       =
**           RECOMMENDED_DELAY       =
**           DEST_COMMUNICATION_MESSAGE       =
**           DEST_SYSTEM_MESSAGE     =
*          EXCEPTIONS
*            AUTHORITY_NOT_AVAILABLE = 1
*            OTHERS                  = 2.
*
*        IF SY-SUBRC IS NOT INITIAL.
*
*        ENDIF.
*
*        DATA: LC_SELECTION       LIKE RSLGSEL,
*              LC_SELECTION_AUDIT LIKE RSAUSEL,
*              LC_FILE_NO_OLD     LIKE RSLGETAB-FILE_NO,
*              LC_SYSLOG_IN_TABLE TYPE TABLE OF RSAUETAB2.
*
*        CALL FUNCTION 'RSAU_READ_FILE' DESTINATION RFCSI_EXPORT-RFCDEST
*          EXPORTING
*            SELECTION       = LC_SELECTION
*            SELECTION_AUDIT = LC_SELECTION_AUDIT
*            FILE_NO_OLD     = LC_FILE_NO_OLD
**         IMPORTING
**           COUNTERS        =
**           END_INFO        =
**           END_REASON      =
**           FILE_NO_NEW     =
*          TABLES
*            SYSLOG_IN_TABLE = LC_SYSLOG_IN_TABLE.
*
*        "Gerar SM20
*        "RSSCD100
*      ENDIF.

  ENDCASE.
