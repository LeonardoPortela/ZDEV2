*&---------------------------------------------------------------------*
*& Report  ZFIR0082
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIR0082.

TABLES: TCURR.

PARAMETERS: P_TRUE    TYPE C  NO-DISPLAY,
            P_FALSE   TYPE C  NO-DISPLAY,
            P_HORA(8) TYPE C NO-DISPLAY.

DATA: V_TRUE    TYPE C,
      V_FALSE   TYPE C,
      V_HORA(8) TYPE C.


DATA: LO_CREATE_MAIL TYPE REF TO CL_CRM_EMAIL_DATA,
      LT_TO          TYPE CRMT_EMAIL_RECIPIENTS,
      LT_COPY        TYPE CRMT_EMAIL_RECIPIENTS,
      LS_RECEP       TYPE CRMS_EMAIL_RECIPIENT,
      LT_MAIL_BODY   TYPE CRMT_EMAIL_MIME_STRUC,
      LS_MAIL_BODY   TYPE CRMS_EMAIL_MIME_STRUC,
      LV_ACTIVITY    TYPE SYSUUID_X.

DATA: IT_TCURR   TYPE TABLE OF TCURR,
      LC_DATAT   TYPE CHAR08,
      LC_GDATU   TYPE GDATU_INV,
      P_DATA_AUX TYPE CHAR10,
      T_HTML     TYPE STRING,
      TX_CAMBIO  TYPE CHAR12,
      T_HOST     TYPE SY-HOST.

DATA: LC_DATA          TYPE SY-DATUM,
      V_DT_COTACAO(10) TYPE C.

RANGES: R_DATA_UP FOR SY-DATUM.


START-OF-SELECTION.

  CHECK SY-SYSID EQ 'PRD'.


  REFRESH R_DATA_UP.
  IMPORT R_DATA_UP FROM MEMORY ID 'DATA_UP'.
  DELETE FROM MEMORY ID 'DATA_UP'.

  IMPORT V_TRUE    FROM MEMORY ID 'P_TRUE'.
  IMPORT V_FALSE   FROM MEMORY ID 'P_FALSE'.
  IMPORT V_HORA    FROM MEMORY ID 'P_HORA'.

  IF V_HORA IS NOT INITIAL.
    P_HORA = V_HORA.
    DELETE FROM MEMORY ID 'P_HORA'.
  ENDIF.

  READ TABLE R_DATA_UP INDEX 1.
  CONCATENATE R_DATA_UP-LOW+6(2) '/' R_DATA_UP-LOW+4(2) '/' R_DATA_UP-LOW(4) INTO V_DT_COTACAO.

  IF V_TRUE IS NOT INITIAL.

    P_TRUE =  V_TRUE.
    DELETE FROM MEMORY ID 'P_TRUE'.

    PERFORM Z_ENVIA_EMAIL_T.

  ELSEIF V_FALSE IS NOT INITIAL.

    P_FALSE =  V_FALSE.
    DELETE FROM MEMORY ID 'P_FALSE'.

    PERFORM Z_ENVIA_EMAIL_F.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_ENVIA_EMAIL_T
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_ENVIA_EMAIL_T .

  CREATE OBJECT LO_CREATE_MAIL.



  RANGES: R_GDATU   FOR SCURR-GDATU,
          R_KURST   FOR TCURR-KURST,
          R_FCURR   FOR TCURR-FCURR,
          R_TCURR   FOR TCURR-TCURR.

  T_HOST = SY-HOST.

  LOOP AT R_DATA_UP.
    R_GDATU-SIGN   = 'I'.
    R_GDATU-OPTION = 'EQ'.

    LC_DATA = R_DATA_UP-LOW + 1.
    CONCATENATE LC_DATA+6(2) LC_DATA+4(2) LC_DATA(4) INTO LC_DATAT.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = LC_DATAT
      IMPORTING
        OUTPUT = R_GDATU-LOW.

    APPEND R_GDATU.

    CLEAR: LC_DATA, LC_DATAT.
  ENDLOOP.


  R_KURST-SIGN   = 'I'.
  R_KURST-OPTION = 'EQ'.
  R_KURST-LOW    = 'B'.
  APPEND R_KURST.
  R_KURST-LOW    = 'G'.
  APPEND R_KURST.
  R_KURST-LOW    = 'M'.
  APPEND R_KURST.
  R_KURST-LOW    = 'EURX'.
  APPEND R_KURST.


  R_FCURR-SIGN    = 'I'.
  R_FCURR-OPTION  = 'EQ'.
  R_FCURR-LOW     = 'BRL'.
  APPEND  R_FCURR.
  R_FCURR-LOW     = 'EUR'.
  APPEND  R_FCURR.
  R_FCURR-LOW     = 'GBP'.
  APPEND  R_FCURR.
  R_FCURR-LOW     = 'USD'.
  APPEND  R_FCURR.


  R_TCURR-SIGN    = 'I'.
  R_TCURR-OPTION  = 'EQ'.
  R_TCURR-LOW     = 'BRL'.
  APPEND R_TCURR.
  R_TCURR-LOW     = 'EUR'.
  APPEND R_TCURR.
  R_TCURR-LOW     = 'GBP'.
  APPEND R_TCURR.
  R_TCURR-LOW     = 'USD'.
  APPEND R_TCURR.


  SELECT * FROM TCURR INTO TABLE IT_TCURR
    WHERE GDATU IN R_GDATU
     AND  KURST IN R_KURST
     AND  FCURR IN R_FCURR
     AND  TCURR IN R_TCURR.

  SORT IT_TCURR BY GDATU DESCENDING.

  IF IT_TCURR IS NOT INITIAL.

    CLEAR T_HTML.
    CONCATENATE T_HTML '<html>'  INTO T_HTML.
    CONCATENATE T_HTML '<head>'  INTO T_HTML.
    CONCATENATE T_HTML '</head>' INTO T_HTML.
    CONCATENATE T_HTML '<body>'  INTO T_HTML.

    TRANSLATE T_HOST TO UPPER CASE.
    CONCATENATE T_HTML '<div align=center><font face=Verdana size=4>' T_HOST '</font></div>'            INTO T_HTML.
    CONCATENATE T_HTML '<div align=center><font face=Verdana size=3>Taxa de Câmbio - PTAX</font></div>' INTO T_HTML.
    CONCATENATE T_HTML '<div align=center><font face=Verdana size=3>Fonte: Banco Central do Brasil</font></div>' INTO T_HTML.
    CONCATENATE T_HTML '<div align=center><font face=Verdana size=3>Data Cotação: ' V_DT_COTACAO ' - Horário Liberação:  ' P_HORA INTO T_HTML SEPARATED BY SPACE.
    CONCATENATE T_HTML '<br><br>'                               INTO T_HTML.
    CONCATENATE T_HTML '<align=left>&nbsp;</div>'               INTO T_HTML.
    CONCATENATE T_HTML '<table border=1 align=center>'          INTO T_HTML.
    CONCATENATE T_HTML '<tr><font face=Verdana size=1><strong>' INTO T_HTML.
    CONCATENATE T_HTML '<td align=center>Data</td>'             INTO T_HTML.
    CONCATENATE T_HTML '<td align=center>Categoria</td>'        INTO T_HTML.
    CONCATENATE T_HTML '<td align=center>De Moeda</td>'         INTO T_HTML.
    CONCATENATE T_HTML '<td align=center>Para Moeda</td>'       INTO T_HTML.
    CONCATENATE T_HTML '<td align=center>Taxa Câmbio</td>'      INTO T_HTML.
    CONCATENATE T_HTML '</td>' INTO T_HTML.

    LOOP AT IT_TCURR INTO DATA(WA_TCURR).

      CONCATENATE T_HTML '<tr><font face=Verdana size=1>' INTO T_HTML.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          INPUT  = WA_TCURR-GDATU
        IMPORTING
          OUTPUT = P_DATA_AUX.

      CONCATENATE T_HTML '<td align=center>' P_DATA_AUX     '</td>' INTO T_HTML.
      CONCATENATE T_HTML '<td align=center>' WA_TCURR-KURST '</td>' INTO T_HTML.
      CONCATENATE T_HTML '<td align=center>' WA_TCURR-FCURR '</td>' INTO T_HTML.
      CONCATENATE T_HTML '<td align=center>' WA_TCURR-TCURR '</td>' INTO T_HTML.

      CALL FUNCTION 'CONVERSION_EXIT_EXCRT_OUTPUT'
        EXPORTING
          INPUT  = WA_TCURR-UKURS
        IMPORTING
          OUTPUT = TX_CAMBIO.

      REPLACE ALL OCCURRENCES OF  '/'  IN  TX_CAMBIO WITH ' ' IGNORING CASE.

      CONCATENATE T_HTML '<td align=right>' TX_CAMBIO '</td>' INTO T_HTML.
      CONCATENATE T_HTML '</font></tr>' INTO T_HTML.

      CLEAR WA_TCURR.
    ENDLOOP.

    CONCATENATE T_HTML '</table>' INTO T_HTML.
    CONCATENATE T_HTML '</body>'  INTO T_HTML.
    CONCATENATE T_HTML '</html>'  INTO T_HTML.

    LO_CREATE_MAIL->SUBJECT =  'Atualização Taxa Câmbio – PTAX (SAP)'.

    CLEAR LS_MAIL_BODY.
    LS_MAIL_BODY-CONTENT_ASCII = T_HTML.
    LS_MAIL_BODY-MIME_TYPE     = 'text/html'.
    APPEND LS_MAIL_BODY TO LT_MAIL_BODY.

    MOVE LT_MAIL_BODY TO LO_CREATE_MAIL->BODY.

    CLEAR LS_RECEP.
    LS_RECEP-ADDRESS = 'csc.financeiro@amaggi.com.br'.
    APPEND LS_RECEP TO LT_TO.
    MOVE LT_TO TO LO_CREATE_MAIL->TO.

    LS_RECEP-ADDRESS = 'suporte.sap@amaggi.com.br'.
    APPEND LS_RECEP TO LT_COPY.

    MOVE LT_COPY TO LO_CREATE_MAIL->COPY.

    CLEAR LS_RECEP.
    LS_RECEP-ADDRESS = ''.
    MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.

    CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL
      EXPORTING
        IV_MAIL_DATA       = LO_CREATE_MAIL
      RECEIVING
        EV_SEND_REQUEST_ID = LV_ACTIVITY.

    COMMIT WORK.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ENVIA_EMAIL_F
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_ENVIA_EMAIL_F .

  CREATE OBJECT LO_CREATE_MAIL.


  CLEAR: T_HTML.
  CONCATENATE T_HTML '<html>'   INTO T_HTML.
  CONCATENATE T_HTML '<head>'  INTO T_HTML.
  CONCATENATE T_HTML '</head>' INTO T_HTML.
  CONCATENATE T_HTML '<body>'  INTO T_HTML.

  CONCATENATE T_HTML '<div align=center><font face=Verdana size=5> TAXA DE CÂMBIO - PTAX(Banco Central do Brasil) - NÃO ATUALIZADO NO SAP</font></div>' INTO T_HTML.
  CONCATENATE T_HTML '<br><br><br>' INTO T_HTML.
  CONCATENATE T_HTML '<div align=left><font face=Verdana size=2> Comunicamos que até o momento a Taxa de Câmbio - PTAX do Banco central do Brasil não foi atualizado'
                     'no SAP na transação OB08 para a data de ' V_DT_COTACAO '.</font></div>' INTO T_HTML SEPARATED BY SPACE.
  CONCATENATE T_HTML '<br><br>' INTO T_HTML.
  CONCATENATE T_HTML '<div align=left><font face=Verdana size=2> A orientação é entrar manualmente na transação OB08 e atualizar as cotações para esta data. </font></div>' INTO T_HTML.

  CONCATENATE T_HTML '</body>'  INTO T_HTML.
  CONCATENATE T_HTML '</html>'   INTO T_HTML.

  LO_CREATE_MAIL->SUBJECT =  'NÃO ATUALIZADO - Taxa Câmbio – PTAX (SAP - OB08)'.

  CLEAR LS_MAIL_BODY.
  LS_MAIL_BODY-CONTENT_ASCII = T_HTML.
  LS_MAIL_BODY-MIME_TYPE     = 'text/html'.
  APPEND LS_MAIL_BODY TO LT_MAIL_BODY.

  MOVE LT_MAIL_BODY TO LO_CREATE_MAIL->BODY.

  CLEAR LS_RECEP.

  LS_RECEP-ADDRESS = 'csc.financeiro@amaggi.com.br'.
  APPEND LS_RECEP TO LT_TO.
  MOVE LT_TO TO LO_CREATE_MAIL->TO.

  LS_RECEP-ADDRESS = 'suporte.sap@amaggi.com.br'.
  APPEND LS_RECEP TO LT_COPY.

  MOVE LT_COPY TO LO_CREATE_MAIL->COPY.

  CLEAR LS_RECEP.
  LS_RECEP-ADDRESS = ''.
  MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.

  CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL
    EXPORTING
      IV_MAIL_DATA       = LO_CREATE_MAIL
    RECEIVING
      EV_SEND_REQUEST_ID = LV_ACTIVITY.

  COMMIT WORK.

ENDFORM.
