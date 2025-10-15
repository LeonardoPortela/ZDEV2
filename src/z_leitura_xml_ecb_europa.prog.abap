*&---------------------------------------------------------------------*
*& Report  Z_LEITURA_XML_ECB_EUROPA
*&
*&---------------------------------------------------------------------*
*&  Este programa lê o XML do Banco Central Europeu e cadastra o indice
*&  do dolar (http://www.ecb.europa.eu)
*&---------------------------------------------------------------------*

REPORT  Z_LEITURA_XML_ECB_EUROPA.

DATA: E_RESULTADO  TYPE STRING.

DATA: IT_TCURR          TYPE TABLE OF ZMME_TCURR WITH HEADER LINE,
      IT_TCURR_AUX      TYPE TABLE OF ZMME_TCURR WITH HEADER LINE,
      IT_RETORNO        TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      WA_TCURR          TYPE TCURR,
      UKURS_GBP	        TYPE UKURS_CURR,
      UKURS_USD	        TYPE UKURS_CURR,
      UKURS    	        TYPE UKURS_CURR,
      UKURS_AUX         TYPE I,
      WA_ZFIT0097       TYPE ZFIT0097,
      LC_DATA_XML       TYPE ZFI_ZMME_TCURR-DT_ARQUIVO_XML,
      IT_ZFI_ZMME_TCURR TYPE TABLE OF ZFI_ZMME_TCURR WITH HEADER LINE,
      WA_USR01          TYPE USR01,
      LC_PATTERN        TYPE C LENGTH 1,
      LC_SUBSTITUTE     TYPE C LENGTH 1,
      LC_DATAT          TYPE CHAR08,
      LC_GDATU          TYPE GDATU_INV,
      I_WOTNR           TYPE P.

DATA: ADDRESS_TO   TYPE STRING,
      ADDRESS_FROM TYPE STRING.

FIELD-SYMBOLS: <FS_TCURR> TYPE ZMME_TCURR.

RANGES: PCAD FOR  TCURR-GDATU.

DATA: IF_XML           TYPE REF TO IF_IXML,
      IF_DOCUMENT      TYPE REF TO IF_IXML_DOCUMENT,
      IF_STREAMFACTORY TYPE REF TO IF_IXML_STREAM_FACTORY,
      IF_STREAM        TYPE REF TO IF_IXML_ISTREAM,
      IF_XML_PARSER    TYPE REF TO IF_IXML_PARSER,
      IF_NODE          TYPE REF TO IF_IXML_NODE,
      ITERATOR         TYPE REF TO IF_IXML_NODE_ITERATOR,
      TAG_NAME         TYPE STRING,
      VALOR_DOM        TYPE STRING,
      VALOR_LINE       TYPE STRING,
      PREFIX_DOM       TYPE STRING,
      IF_MAP           TYPE REF TO IF_IXML_NAMED_NODE_MAP,
      CONTEXT          TYPE REF TO IF_IXML_NAMESPACE_CONTEXT,
      RVAL             TYPE STRING,
      DATA_XML         TYPE C LENGTH 10.


SELECT SINGLE * INTO WA_USR01 FROM USR01 WHERE BNAME EQ SY-UNAME.

*   1.234.567,89
* X	1,234,567.89
* Y	1 234 567,89

CASE WA_USR01-DCPFM.
  WHEN SPACE.
    LC_PATTERN    = '.'.
    LC_SUBSTITUTE = ','.
  WHEN 'X'.
    LC_PATTERN    = ','.
    LC_SUBSTITUTE = '.'.
  WHEN 'Y'.
    LC_PATTERN    = ' '.
    LC_SUBSTITUTE = ','.
ENDCASE.

PERFORM VERIFICA_ONTEM_FERIADO.

CALL FUNCTION 'DAY_IN_WEEK'
  EXPORTING
    DATUM = SY-DATUM
  IMPORTING
    WOTNR = I_WOTNR.

DATA(LC_FERIADO) = ABAP_FALSE.
DATA: LC_DATA_FERIADO TYPE SY-DATUM.

IF I_WOTNR EQ 1. "Seguda

  "Segunda-Feira
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATUM "Seguna
      DAYS      = 3
      MONTHS    = 0
      YEARS     = 0
      SIGNUM    = '-'
    IMPORTING
      CALC_DATE = LC_DATA_FERIADO. "Sexta

  WA_ZFIT0097-LAND1 = 'NL'.

  SELECT SINGLE * INTO WA_ZFIT0097
    FROM ZFIT0097
   WHERE LAND1    EQ WA_ZFIT0097-LAND1
     AND FERIADO  EQ LC_DATA_FERIADO.

  IF SY-SUBRC IS INITIAL.
    LC_FERIADO = ABAP_TRUE.
  ENDIF.

ENDIF.

IF ( I_WOTNR EQ 6 OR I_WOTNR EQ 7 ) OR ( LC_FERIADO EQ ABAP_TRUE ).

  CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) INTO LC_DATAT.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      INPUT  = LC_DATAT
    IMPORTING
      OUTPUT = LC_GDATU.

  PERFORM VERIFICA_FALTA USING LC_GDATU.

  IF SY-SUBRC IS NOT INITIAL.

    SELECT * INTO TABLE IT_ZFI_ZMME_TCURR
      FROM ZFI_ZMME_TCURR
     WHERE LAND1 EQ 'NL'
       AND GDATU EQ SY-DATUM.

    CLEAR: IT_TCURR[].

    LOOP AT IT_ZFI_ZMME_TCURR.

      IT_TCURR-KURST = IT_ZFI_ZMME_TCURR-KURST.  "1 Tipo KURST_CURR
      IT_TCURR-FCURR = IT_ZFI_ZMME_TCURR-FCURR.  "1 Tipo FCURR_CURR
      IT_TCURR-TCURR = IT_ZFI_ZMME_TCURR-TCURR.  "1 Tipo TCURR_CURR
      IT_TCURR-GDATU = IT_ZFI_ZMME_TCURR-GDATU.  "1 Tipo DATUM

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = IT_TCURR-GDATU
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
          SIGNUM    = '-'
        IMPORTING
          CALC_DATE = IT_TCURR-GDATU.

      MOVE IT_ZFI_ZMME_TCURR-UKURS TO IT_TCURR-UKURS.
      CONDENSE IT_TCURR-UKURS NO-GAPS.

      CALL FUNCTION 'STRING_REPLACE'
        EXPORTING
          PATTERN    = LC_PATTERN
          SUBSTITUTE = LC_SUBSTITUTE
        CHANGING
          TEXT       = IT_TCURR-UKURS.

      APPEND IT_TCURR.

    ENDLOOP.

    IF IT_TCURR[] IS NOT INITIAL.
      CALL FUNCTION 'Z_FI_INBOUND_INDICE_FINANCEIRO'
        TABLES
          IT_TCURR   = IT_TCURR
          IT_RETORNO = IT_RETORNO.
    ENDIF.
  ENDIF.
ENDIF.

CLEAR: I_WOTNR,
       LC_GDATU,
       IT_TCURR[],
       IT_TCURR,
       IT_ZFI_ZMME_TCURR,
       IT_ZFI_ZMME_TCURR[],
       IT_RETORNO,
       IT_RETORNO[],
       LC_DATAT.

IF SY-SYSID EQ 'PRD'.
  ADDRESS_TO = 'finance@amaggi.eu'.
  "ADDRESS_FROM = 'finance@amaggi.eu'.
ELSE.
  ADDRESS_TO = 'suporte.sap@amaggi.com.br'.
  "ADDRESS_FROM = 'suporte.sap@amaggi.com.br'.
ENDIF.

CALL METHOD ZCL_LER_URL=>LER_URL
  EXPORTING
    I_URL                      = 'http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml'
  RECEIVING
    E_TEXTO                    = E_RESULTADO
  EXCEPTIONS
    ARGUMENT_NOT_FOUND         = 1
    PLUGIN_NOT_ACTIVE          = 2
    INTERNAL_ERROR             = 3
    HTTP_COMMUNICATION_FAILURE = 4
    HTTP_INVALID_STATE         = 5
    HTTP_PROCESSING_FAILED     = 6
    HTTP_INVALID_TIMEOUT       = 7
    OUTROS                     = 8
    OTHERS                     = 9.

IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

IF_XML = CL_IXML=>CREATE( ).
IF_DOCUMENT      = IF_XML->CREATE_DOCUMENT( ).
IF_STREAMFACTORY = IF_XML->CREATE_STREAM_FACTORY( ).
IF_STREAM        = IF_STREAMFACTORY->CREATE_ISTREAM_STRING( E_RESULTADO ).

IF_XML_PARSER    = IF_XML->CREATE_PARSER( STREAM_FACTORY = IF_STREAMFACTORY
                                          ISTREAM        = IF_STREAM
                                          DOCUMENT       = IF_DOCUMENT ).
IF_XML_PARSER->PARSE( ).

IF_NODE ?= IF_DOCUMENT->GET_ROOT_ELEMENT( ).

IF NOT ( IF_NODE IS INITIAL ).

  ITERATOR = IF_NODE->CREATE_ITERATOR( ).
  IF_NODE  = ITERATOR->GET_NEXT( ).

  WHILE NOT IF_NODE IS INITIAL.
    CASE IF_NODE->GET_TYPE( ).
      WHEN: IF_IXML_NODE=>CO_NODE_ELEMENT.
        TAG_NAME  = IF_NODE->GET_NAME( ).
        IF_MAP    = IF_NODE->GET_ATTRIBUTES( ).
        VALOR_DOM = IF_NODE->GET_VALUE( ).
        IF TAG_NAME EQ 'Cube'.

          DATA: COUNT_DOM TYPE I,
                INDEX_DOM TYPE I,
                IF_ATTR   TYPE REF TO IF_IXML_NODE,
                NAME_DOM  TYPE STRING.

          COUNT_DOM = IF_MAP->GET_LENGTH( ).

          DO COUNT_DOM TIMES.
            INDEX_DOM  = SY-INDEX - 1.
            IF_ATTR    = IF_MAP->GET_ITEM( INDEX_DOM ).
            NAME_DOM   = IF_ATTR->GET_NAME( ).

            CASE NAME_DOM.
              WHEN 'time'.
                VALOR_DOM  = IF_ATTR->GET_VALUE( ).
                DATA_XML = VALOR_DOM.
                CONCATENATE VALOR_DOM(4) VALOR_DOM+5(2) VALOR_DOM+8(2) INTO VALOR_DOM.
                MOVE VALOR_DOM TO IT_TCURR-GDATU.
              WHEN 'currency'.
                VALOR_DOM  = IF_ATTR->GET_VALUE( ).
                MOVE VALOR_DOM TO IT_TCURR-FCURR.
                MOVE 'EUR'     TO IT_TCURR-TCURR.
              WHEN 'rate'.
                VALOR_DOM  = IF_ATTR->GET_VALUE( ).
                "Se for USD já cadastra o USD para Euro.
                IF IT_TCURR-FCURR EQ 'USD'.
                  TRY.
                      MOVE VALOR_DOM TO UKURS_USD.
                    CATCH CX_SY_CONVERSION_OVERFLOW.
                      IT_TCURR-UKURS = 0.
                  ENDTRY.
                  MOVE VALOR_DOM TO IT_TCURR-UKURS.
                  IT_TCURR-KURST = 'G'.
                  APPEND IT_TCURR.
                  IT_TCURR-KURST = 'M'.
                  APPEND IT_TCURR.
                  IT_TCURR-KURST = 'B'.
                  APPEND IT_TCURR.
                ENDIF.

                "Se for GBS guarda o indice para fazer conta.
                IF IT_TCURR-FCURR EQ 'GBP'.
                  TRY.
                      MOVE VALOR_DOM TO UKURS_GBP.
                    CATCH CX_SY_CONVERSION_OVERFLOW.
                      IT_TCURR-UKURS = 0.
                  ENDTRY.
                ENDIF.
            ENDCASE.
          ENDDO.
        ENDIF.

    ENDCASE.
    IF_NODE = ITERATOR->GET_NEXT( ).
  ENDWHILE.

  IF UKURS_USD NE 0 AND UKURS_GBP NE 0.

    MOVE 'USD' TO IT_TCURR-TCURR.
    MOVE 'GBP' TO IT_TCURR-FCURR.

    UKURS = UKURS_USD / UKURS_GBP.
    UKURS_AUX = UKURS * 10000.
    UKURS     = UKURS_AUX / 10000.

    MOVE UKURS TO IT_TCURR-UKURS.
    IT_TCURR-KURST = 'G'.
    APPEND IT_TCURR.
    IT_TCURR-KURST = 'M'.
    APPEND IT_TCURR.
    IT_TCURR-KURST = 'B'.
    APPEND IT_TCURR.

    MOVE 'EUR' TO IT_TCURR-FCURR.
    MOVE 'GBP' TO IT_TCURR-TCURR.

    UKURS = UKURS / UKURS_USD.
    UKURS_AUX = UKURS * 10000.
    UKURS     = UKURS_AUX / 10000.

    MOVE UKURS TO IT_TCURR-UKURS.
    IT_TCURR-KURST = 'G'.
    APPEND IT_TCURR.
    IT_TCURR-KURST = 'M'.
    APPEND IT_TCURR.
    IT_TCURR-KURST = 'B'.
    APPEND IT_TCURR.

  ENDIF.

  CHECK IT_TCURR[] IS NOT INITIAL.

  DATA: LC_DATA  TYPE SY-DATUM,
        LC_DATAF TYPE SY-DATUM,
        LC_DATA1 TYPE SY-DATUM,
        LC_DATA2 TYPE SY-DATUM,
        LC_DATA3 TYPE SY-DATUM.

  LC_DATA     = IT_TCURR-GDATU.
  LC_DATA_XML = IT_TCURR-GDATU.

  CALL FUNCTION 'DAY_IN_WEEK'
    EXPORTING
      DATUM = IT_TCURR-GDATU
    IMPORTING
      WOTNR = I_WOTNR.

  CASE I_WOTNR.
    WHEN 1 OR 2 OR 3. "Segunda --> Terça / Terça --> Quarta / Quarta --> Quinta

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = LC_DATA
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = LC_DATA.

      LC_DATA1 = LC_DATA.
      LC_DATA2 = LC_DATA.

    WHEN 4. "Quinta  --> Sexta > Sábado > Domingo > Segunda-Feira*

      "Sabado
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = IT_TCURR-GDATU
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = LC_DATA.

      LC_DATA1 = LC_DATA.

      LOOP AT IT_TCURR.
        MOVE-CORRESPONDING IT_TCURR TO IT_TCURR_AUX.
        IT_TCURR_AUX-GDATU = LC_DATA1.
        APPEND IT_TCURR_AUX.
      ENDLOOP.

      "Domingo
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = IT_TCURR-GDATU
          DAYS      = 2
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = LC_DATA2.

      LOOP AT IT_TCURR.
        MOVE-CORRESPONDING IT_TCURR TO IT_TCURR_AUX.
        IT_TCURR_AUX-GDATU = LC_DATA2.
        APPEND IT_TCURR_AUX.
      ENDLOOP.

      "Sexta é feriado?
      "Se Sexta é feriado então será o indice de segunda feira também
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = IT_TCURR-GDATU "Quinta
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = LC_DATA3.  "Sexta

      WA_ZFIT0097-LAND1 = 'NL'.

      SELECT SINGLE * INTO WA_ZFIT0097
        FROM ZFIT0097
       WHERE LAND1    EQ WA_ZFIT0097-LAND1
         AND FERIADO  EQ LC_DATA3. "Sexta

      IF SY-SUBRC IS INITIAL.

        "Segunda-Feira
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            DATE      = IT_TCURR-GDATU "Quinta
            DAYS      = 3
            MONTHS    = 0
            YEARS     = 0
          IMPORTING
            CALC_DATE = LC_DATA3. "Segunda

        LOOP AT IT_TCURR.
          MOVE-CORRESPONDING IT_TCURR TO IT_TCURR_AUX.
          IT_TCURR_AUX-GDATU = LC_DATA3. "Segunda
          APPEND IT_TCURR_AUX.
        ENDLOOP.

      ENDIF.

    WHEN 5. "Sexta   --> Segunda

      "Domingo
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = IT_TCURR-GDATU
          DAYS      = 2
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = LC_DATA.

      "Segunda
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = LC_DATA
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = LC_DATA1.

      LOOP AT IT_TCURR ASSIGNING <FS_TCURR>.
        <FS_TCURR>-GDATU = LC_DATA.
      ENDLOOP.

      LC_DATA  = LC_DATA1.
      LC_DATA2 = LC_DATA1.

  ENDCASE.

  "Próximo dia é Feriado?
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = LC_DATA2
      DAYS      = 1
      MONTHS    = 0
      YEARS     = 0
    IMPORTING
      CALC_DATE = LC_DATAF.

  WA_ZFIT0097-LAND1 = 'NL'.

  SELECT SINGLE * INTO WA_ZFIT0097
    FROM ZFIT0097
   WHERE LAND1    EQ WA_ZFIT0097-LAND1
     AND FERIADO  EQ LC_DATAF.

  IF SY-SUBRC IS INITIAL.

    CALL FUNCTION 'DAY_IN_WEEK'
      EXPORTING
        DATUM = LC_DATAF
      IMPORTING
        WOTNR = I_WOTNR.

    LOOP AT IT_TCURR.
      MOVE-CORRESPONDING IT_TCURR TO IT_TCURR_AUX.
      IT_TCURR_AUX-GDATU = LC_DATA2.
      APPEND IT_TCURR_AUX.
    ENDLOOP.

  ELSE.
    CLEAR WA_ZFIT0097.
  ENDIF.

  "Copia Datas Extras para Tabela Principal
  LOOP AT IT_TCURR_AUX.
    APPEND IT_TCURR_AUX TO IT_TCURR.
  ENDLOOP.

  CLEAR: LC_DATA.

  "Grava Indices Coletados
  IF IT_TCURR[] IS NOT INITIAL.

    CLEAR: IT_ZFI_ZMME_TCURR[].
    LOOP AT IT_TCURR.
      MOVE-CORRESPONDING IT_TCURR TO IT_ZFI_ZMME_TCURR.
      MOVE IT_TCURR-UKURS TO IT_ZFI_ZMME_TCURR-UKURS.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = IT_ZFI_ZMME_TCURR-GDATU
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = IT_ZFI_ZMME_TCURR-GDATU.

      IF LC_DATA IS INITIAL.
        LC_DATA = IT_ZFI_ZMME_TCURR-GDATU.
      ENDIF.

      IT_ZFI_ZMME_TCURR-LAND1 = 'NL'.
      IT_ZFI_ZMME_TCURR-DT_ATUALIZACAO = SY-DATUM.
      IT_ZFI_ZMME_TCURR-HR_ATUALIZACAO = SY-UZEIT.
      IT_ZFI_ZMME_TCURR-DT_ARQUIVO_XML = LC_DATA_XML.
      APPEND IT_ZFI_ZMME_TCURR.
    ENDLOOP.

    IF IT_ZFI_ZMME_TCURR[] IS NOT INITIAL.
      MODIFY ZFI_ZMME_TCURR FROM TABLE IT_ZFI_ZMME_TCURR.
      COMMIT WORK.
    ENDIF.

  ENDIF.

  CLEAR: IT_ZFI_ZMME_TCURR[], IT_TCURR[], IT_ZFI_ZMME_TCURR, IT_TCURR.


  IF LC_DATA IS INITIAL.

    LC_DATA  = SY-DATUM.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        DATE      = LC_DATA
        DAYS      = 1
        MONTHS    = 0
        YEARS     = 0
      IMPORTING
        CALC_DATE = LC_DATA.
  ENDIF.

  CONCATENATE LC_DATA+6(2) LC_DATA+4(2) LC_DATA(4) INTO LC_DATAT.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      INPUT  = LC_DATAT
    IMPORTING
      OUTPUT = LC_GDATU.

  PERFORM VERIFICA_FALTA USING LC_GDATU.

  CHECK SY-SUBRC IS NOT INITIAL.

  SELECT * INTO TABLE IT_ZFI_ZMME_TCURR
    FROM ZFI_ZMME_TCURR
   WHERE LAND1 EQ 'NL'
     AND GDATU EQ LC_DATA.

  LOOP AT IT_ZFI_ZMME_TCURR.

    IT_TCURR-KURST = IT_ZFI_ZMME_TCURR-KURST.  "1 Tipo KURST_CURR
    IT_TCURR-FCURR = IT_ZFI_ZMME_TCURR-FCURR.  "1 Tipo FCURR_CURR
    IT_TCURR-TCURR = IT_ZFI_ZMME_TCURR-TCURR.  "1 Tipo TCURR_CURR
    IT_TCURR-GDATU = IT_ZFI_ZMME_TCURR-GDATU.  "1 Tipo DATUM

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        DATE      = IT_TCURR-GDATU
        DAYS      = 1
        MONTHS    = 0
        YEARS     = 0
        SIGNUM    = '-'
      IMPORTING
        CALC_DATE = IT_TCURR-GDATU.

    MOVE IT_ZFI_ZMME_TCURR-UKURS TO IT_TCURR-UKURS.
    CONDENSE IT_TCURR-UKURS NO-GAPS.

    CALL FUNCTION 'STRING_REPLACE'
      EXPORTING
        PATTERN    = LC_PATTERN
        SUBSTITUTE = LC_SUBSTITUTE
      CHANGING
        TEXT       = IT_TCURR-UKURS.

    APPEND IT_TCURR.

  ENDLOOP.

  IF IT_TCURR[] IS NOT INITIAL.

    CALL FUNCTION 'Z_FI_INBOUND_INDICE_FINANCEIRO'
      TABLES
        IT_TCURR   = IT_TCURR
        IT_RETORNO = IT_RETORNO.

    PCAD-SIGN   = 'I'.
    PCAD-OPTION = 'EQ'.
    PCAD-LOW    = LC_GDATU.
    PCAD-HIGH   = LC_GDATU.
    APPEND PCAD.

    PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU LC_DATA.

    PERFORM VERIFICAR_NAO_CADASTRADOS.

  ENDIF.

*  IF IT_TCURR[] IS NOT INITIAL.
*
*    CALL FUNCTION 'Z_FI_INBOUND_INDICE_FINANCEIRO'
*      TABLES
*        IT_TCURR   = IT_TCURR
*        IT_RETORNO = IT_RETORNO.
*
*    PCAD-SIGN   = 'I'.
*    PCAD-OPTION = 'EQ'.
*    PCAD-LOW     = LC_GDATU.
*    PCAD-HIGH    = LC_GDATU.
*    APPEND PCAD.
*
*    PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU LC_DATA.
*
*    IF I_WOTNR EQ 4. " (Quinta)
*
*      "Sabado
*      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*        EXPORTING
*          DATE      = LC_DATA1
*          DAYS      = 1
*          MONTHS    = 0
*          YEARS     = 0
*        IMPORTING
*          CALC_DATE = LC_DATA1.
*
*      CONCATENATE LC_DATA1+6(2) LC_DATA1+4(2) LC_DATA1(4) INTO LC_DATA.
*
*      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*        EXPORTING
*          INPUT  = LC_DATA
*        IMPORTING
*          OUTPUT = LC_GDATU.
*
*      PCAD-SIGN   = 'I'.
*      PCAD-OPTION = 'EQ'.
*      PCAD-LOW     = LC_GDATU.
*      PCAD-HIGH    = LC_GDATU.
*      APPEND PCAD.
*
*      PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU LC_DATA1.
*
*      "Domingo
*      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*        EXPORTING
*          DATE      = LC_DATA1
*          DAYS      = 1
*          MONTHS    = 0
*          YEARS     = 0
*        IMPORTING
*          CALC_DATE = LC_DATA2.
*
*      CONCATENATE LC_DATA2+6(2) LC_DATA2+4(2) LC_DATA2(4) INTO LC_DATA.
*
*      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*        EXPORTING
*          INPUT  = LC_DATA
*        IMPORTING
*          OUTPUT = LC_GDATU.
*
*      PCAD-SIGN   = 'I'.
*      PCAD-OPTION = 'EQ'.
*      PCAD-LOW     = LC_GDATU.
*      PCAD-HIGH    = LC_GDATU.
*      APPEND PCAD.
*
*      PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU LC_DATA2.
*
*    ENDIF.
*
*    "Feriado
*    IF WA_ZFIT0097 IS NOT INITIAL.
*      CONCATENATE WA_ZFIT0097-FERIADO+6(2) WA_ZFIT0097-FERIADO+4(2) WA_ZFIT0097-FERIADO(4) INTO LC_DATA.
*
*      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*        EXPORTING
*          INPUT  = LC_DATA
*        IMPORTING
*          OUTPUT = LC_GDATU.
*
*      PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU WA_ZFIT0097-FERIADO.
*    ENDIF.
*
*  ENDIF.
*
*  PERFORM VERIFICAR_NAO_CADASTRADOS.

ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GERAR_E_MAIL_CADASTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GERAR_E_MAIL_CADASTRO  USING  P_GDATU TYPE GDATU_INV P_DATA TYPE SY-DATUM .

  DATA: LO_CREATE_MAIL TYPE REF TO CL_CRM_EMAIL_DATA,
        LT_TO          TYPE CRMT_EMAIL_RECIPIENTS,
        LT_COPY        TYPE CRMT_EMAIL_RECIPIENTS,
        LS_RECEP       TYPE CRMS_EMAIL_RECIPIENT,
        LT_MAIL_BODY   TYPE CRMT_EMAIL_MIME_STRUC,
        LS_MAIL_BODY   TYPE CRMS_EMAIL_MIME_STRUC,
        LV_ACTIVITY    TYPE SYSUUID_X,
        P_DATA_AUX     TYPE CHAR10,
        STR_AUX        TYPE CHAR12,
        VALOR          TYPE F.

  DATA: I_HTML_ENTRA TYPE STRING,
        I_HTML_SAIDA TYPE STRING.

  DATA: IT_TCURR TYPE TABLE OF TCURR WITH HEADER LINE.

  RANGES: RG_KURST FOR TCURR-KURST.
  RANGES: RG_FCURR FOR TCURR-FCURR.
  RANGES: RG_TCURR FOR TCURR-TCURR.

  "Cotação
  RG_KURST-SIGN    = 'I'.
  RG_KURST-OPTION  = 'EQ'.
  RG_KURST-LOW     = 'M'.
  RG_KURST-HIGH    = 'M'.
  APPEND RG_KURST.

  RG_KURST-LOW     = 'G'.
  RG_KURST-HIGH    = 'G'.
  APPEND RG_KURST.

  RG_KURST-LOW     = 'B'.
  RG_KURST-HIGH    = 'B'.
  APPEND RG_KURST.

  "Moeda de procedência
  RG_FCURR-SIGN    = 'I'.
  RG_FCURR-OPTION  = 'EQ'.
  RG_FCURR-LOW     = 'EUR'.
  RG_FCURR-HIGH    = 'EUR'.
  APPEND RG_FCURR.

  RG_FCURR-LOW     = 'USD'.
  RG_FCURR-HIGH    = 'USD'.
  APPEND RG_FCURR.

  RG_FCURR-LOW     = 'GBP'.
  RG_FCURR-HIGH    = 'GBP'.
  APPEND RG_FCURR.

  "Moeda de destino
  RG_TCURR-SIGN    = 'I'.
  RG_TCURR-OPTION  = 'EQ'.
  RG_TCURR-LOW     = 'EUR'.
  RG_TCURR-HIGH    = 'EUR'.
  APPEND RG_TCURR.

  RG_TCURR-LOW     = 'USD'.
  RG_TCURR-HIGH    = 'USD'.
  APPEND RG_TCURR.

  RG_TCURR-LOW     = 'GBP'.
  RG_TCURR-HIGH    = 'GBP'.
  APPEND RG_TCURR.

  SELECT * INTO TABLE IT_TCURR
    FROM TCURR
   WHERE GDATU EQ P_GDATU
     AND KURST IN RG_KURST
     AND FCURR IN RG_FCURR
     AND TCURR IN RG_TCURR.

  IF IT_TCURR[] IS NOT INITIAL.

    CLEAR: I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<html>'      INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<head>'      INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '</head>'     INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<body>'      INTO I_HTML_ENTRA.
    I_HTML_SAIDA = SY-HOST.
    TRANSLATE I_HTML_SAIDA TO UPPER CASE.
    CONCATENATE I_HTML_ENTRA '<DIV align=center><FONT face=Verdana size=4>' I_HTML_SAIDA '</FONT></DIV>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<DIV align=center><FONT face=Verdana size=3>Exchange Reference Rates</FONT></DIV>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<DIV align=center><a href="http://www.ecb.europa.eu/stats/exchange/eurofxref/html/index.en.html">EUROPEAN CENTRAL BANK</a></DIV>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.

    CONCATENATE I_HTML_ENTRA '<TABLE border=1 align=center>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1><STRONG>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TD align=center>Date</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TD align=center>Exchange Rate Type</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TD align=center>From currency</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TD align=center>To currency</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TD align=center>Exchange Rate</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '</TR>' INTO I_HTML_ENTRA.

    LOOP AT IT_TCURR.
      CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1>' INTO I_HTML_ENTRA.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          INPUT  = IT_TCURR-GDATU
        IMPORTING
          OUTPUT = P_DATA_AUX.
      "Date
      CONCATENATE I_HTML_ENTRA '<TD align=center>' P_DATA_AUX '</TD>' INTO I_HTML_ENTRA.
      "Exchange Rate Type
      CONCATENATE I_HTML_ENTRA '<TD align=center>' IT_TCURR-KURST '</TD>' INTO I_HTML_ENTRA.
      "From currency
      CONCATENATE I_HTML_ENTRA '<TD align=center>' IT_TCURR-FCURR '</TD>' INTO I_HTML_ENTRA.
      "To currency
      CONCATENATE I_HTML_ENTRA '<TD align=center>' IT_TCURR-TCURR '</TD>' INTO I_HTML_ENTRA.
      "Exchange Rate
      CALL FUNCTION 'CONVERSION_EXIT_EXCRT_OUTPUT'
        EXPORTING
          INPUT  = IT_TCURR-UKURS
        IMPORTING
          OUTPUT = STR_AUX.

      CONCATENATE I_HTML_ENTRA '<TD align=right>' STR_AUX        '</TD>' INTO I_HTML_ENTRA.
      CONCATENATE I_HTML_ENTRA '</FONT></TR>' INTO I_HTML_ENTRA.
    ENDLOOP.

    CONCATENATE I_HTML_ENTRA '</TABLE>' INTO I_HTML_ENTRA.

    CONCATENATE I_HTML_ENTRA '</body>'     INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '</html>'     INTO I_HTML_ENTRA.

    MOVE I_HTML_ENTRA TO I_HTML_SAIDA.

    CREATE OBJECT LO_CREATE_MAIL.

    "moving the subject.
    CONCATENATE 'Reference rates' P_DATA_AUX INTO LO_CREATE_MAIL->SUBJECT SEPARATED BY SPACE.

    CLEAR LS_MAIL_BODY.
    LS_MAIL_BODY-CONTENT_ASCII = I_HTML_SAIDA.
    LS_MAIL_BODY-MIME_TYPE     = 'text/html'.
    APPEND  LS_MAIL_BODY TO LT_MAIL_BODY.

    "moving the body
    MOVE LT_MAIL_BODY TO LO_CREATE_MAIL->BODY.

    "filling to email address
    CLEAR LS_RECEP.
    LS_RECEP-ADDRESS = ADDRESS_TO.
    APPEND LS_RECEP TO LT_TO.
    MOVE LT_TO TO LO_CREATE_MAIL->TO.

    LS_RECEP-ADDRESS = 'suporte.sap@amaggi.com.br'.
    APPEND LS_RECEP TO LT_COPY.

    LS_RECEP-ADDRESS = 'controladoria.europa@amaggi.com.br'.
    APPEND LS_RECEP TO LT_COPY.

    MOVE LT_COPY TO LO_CREATE_MAIL->COPY.

    CLEAR LS_RECEP.
    LS_RECEP-ADDRESS = ADDRESS_FROM.
    MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.

    "send the email.
    "calling method to send the email
    CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL
      EXPORTING
        IV_MAIL_DATA       = LO_CREATE_MAIL
      RECEIVING
        EV_SEND_REQUEST_ID = LV_ACTIVITY.

    COMMIT WORK.

  ENDIF.

ENDFORM.                    " GERAR_E_MAIL_CADASTRO

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_NAO_CADASTRADOS
*&---------------------------------------------------------------------*
*       Verifica todas a matriz de indice a ser cadastrada se realmente
*       foi cadastrada
*----------------------------------------------------------------------*
FORM VERIFICAR_NAO_CADASTRADOS .

  DATA: LO_CREATE_MAIL TYPE REF TO CL_CRM_EMAIL_DATA,
        LT_TO          TYPE CRMT_EMAIL_RECIPIENTS,
        LT_COPY        TYPE CRMT_EMAIL_RECIPIENTS,
        LS_RECEP       TYPE CRMS_EMAIL_RECIPIENT,
        LT_MAIL_BODY   TYPE CRMT_EMAIL_MIME_STRUC,
        LS_MAIL_BODY   TYPE CRMS_EMAIL_MIME_STRUC,
        LV_ACTIVITY    TYPE SYSUUID_X,
        P_DATA_AUX     TYPE CHAR10,
        STR_AUX        TYPE CHAR12,
        VALOR          TYPE F.

  DATA: I_HTML_ENTRA TYPE STRING,
        I_HTML_SAIDA TYPE STRING,
        LC_MESSAGE   TYPE BAPIRET2-MESSAGE.

  DATA: IT_TCURR_CAD TYPE TABLE OF TCURR WITH HEADER LINE,
        IT_TCURR_ERR TYPE TABLE OF TCURR WITH HEADER LINE.

  RANGES: RG_KURST FOR TCURR-KURST.
  RANGES: RG_FCURR FOR TCURR-FCURR.
  RANGES: RG_TCURR FOR TCURR-TCURR.

  "Cotação
  RG_KURST-SIGN    = 'I'.
  RG_KURST-OPTION  = 'EQ'.
  RG_KURST-LOW     = 'M'.
  RG_KURST-HIGH    = 'M'.
  APPEND RG_KURST.
  RG_KURST-LOW     = 'G'.
  RG_KURST-HIGH    = 'G'.
  APPEND RG_KURST.
  RG_KURST-LOW     = 'B'.
  RG_KURST-HIGH    = 'B'.
  APPEND RG_KURST.

  "Moeda de procedência
  RG_FCURR-SIGN    = 'I'.
  RG_FCURR-OPTION  = 'EQ'.
  RG_FCURR-LOW     = 'EUR'.
  RG_FCURR-HIGH    = 'EUR'.
  APPEND RG_FCURR.
  RG_FCURR-LOW     = 'USD'.
  RG_FCURR-HIGH    = 'USD'.
  APPEND RG_FCURR.
  RG_FCURR-LOW     = 'GBP'.
  RG_FCURR-HIGH    = 'GBP'.
  APPEND RG_FCURR.

  "Moeda de destino
  RG_TCURR-SIGN    = 'I'.
  RG_TCURR-OPTION  = 'EQ'.
  RG_TCURR-LOW     = 'EUR'.
  RG_TCURR-HIGH    = 'EUR'.
  APPEND RG_TCURR.
  RG_TCURR-LOW     = 'USD'.
  RG_TCURR-HIGH    = 'USD'.
  APPEND RG_TCURR.
  RG_TCURR-LOW     = 'GBP'.
  RG_TCURR-HIGH    = 'GBP'.
  APPEND RG_TCURR.

  SELECT * INTO TABLE IT_TCURR_CAD
    FROM TCURR
   WHERE GDATU IN PCAD
     AND KURST IN RG_KURST
     AND FCURR IN RG_FCURR
     AND TCURR IN RG_TCURR.

  SORT IT_TCURR_CAD BY GDATU KURST FCURR TCURR.

  "Dias
  LOOP AT PCAD.
    "Cotacao
    LOOP AT RG_KURST.
      "Moeda Origem
      LOOP AT RG_FCURR.
        "Moeda Destino
        LOOP AT RG_TCURR.
          READ TABLE IT_TCURR_CAD WITH KEY GDATU = PCAD-LOW
                                           KURST = RG_KURST-LOW
                                           FCURR = RG_FCURR-LOW
                                           TCURR = RG_TCURR-LOW.
          IF ( SY-SUBRC IS NOT INITIAL ) AND ( RG_FCURR-LOW NE RG_TCURR-LOW ).
            IT_TCURR_ERR-GDATU = PCAD-LOW.
            IT_TCURR_ERR-KURST = RG_KURST-LOW.
            IT_TCURR_ERR-FCURR = RG_FCURR-LOW.
            IT_TCURR_ERR-TCURR = RG_TCURR-LOW.
            APPEND IT_TCURR_ERR.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  IF IT_TCURR_ERR[] IS NOT INITIAL.
    CLEAR: I_HTML_ENTRA.

    CONCATENATE I_HTML_ENTRA '<html>'      INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<head>'      INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '</head>'     INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<body>'      INTO I_HTML_ENTRA.

    I_HTML_SAIDA = SY-HOST.
    TRANSLATE I_HTML_SAIDA TO UPPER CASE.
    CONCATENATE I_HTML_ENTRA '<DIV align=center><FONT face=Verdana size=4>' I_HTML_SAIDA '</FONT></DIV>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<DIV align=center><FONT face=Verdana size=3>Exchange Reference Rates</FONT></DIV>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<DIV align=center><FONT face=Verdana size=3 color=#FF0000>ERROR</FONT></DIV>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<DIV align=center><a href="http://www.ecb.europa.eu/stats/exchange/eurofxref/html/index.en.html">EUROPEAN CENTRAL BANK</a></DIV>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.

    CONCATENATE I_HTML_ENTRA '<TABLE border=1 align=center>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1><STRONG>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TD align=center>Date</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TD align=center>Exchange Rate Type</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TD align=center>From currency</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<TD align=center>To currency</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '</TR>' INTO I_HTML_ENTRA.

    LOOP AT IT_TCURR_ERR.
      CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1>' INTO I_HTML_ENTRA.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          INPUT  = IT_TCURR_ERR-GDATU
        IMPORTING
          OUTPUT = P_DATA_AUX.
      "Date
      CONCATENATE I_HTML_ENTRA '<TD align=center>' P_DATA_AUX '</TD>' INTO I_HTML_ENTRA.
      "Exchange Rate Type
      CONCATENATE I_HTML_ENTRA '<TD align=center>' IT_TCURR_ERR-KURST '</TD>' INTO I_HTML_ENTRA.
      "From currency
      CONCATENATE I_HTML_ENTRA '<TD align=center>' IT_TCURR_ERR-FCURR '</TD>' INTO I_HTML_ENTRA.
      "To currency
      CONCATENATE I_HTML_ENTRA '<TD align=center>' IT_TCURR_ERR-TCURR '</TD>' INTO I_HTML_ENTRA.

      CONCATENATE I_HTML_ENTRA '</FONT></TR>' INTO I_HTML_ENTRA.
    ENDLOOP.

    CONCATENATE I_HTML_ENTRA '</TABLE>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
    "Montagem de erros do SHDB
    IF IT_RETORNO[] IS NOT INITIAL.
      CONCATENATE I_HTML_ENTRA '<TABLE border=1 align=center>' INTO I_HTML_ENTRA.
      CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1><STRONG>' INTO I_HTML_ENTRA.
      CONCATENATE I_HTML_ENTRA '<TD align=center>Message Error</TD>' INTO I_HTML_ENTRA.
      CONCATENATE I_HTML_ENTRA '</TR>' INTO I_HTML_ENTRA.

      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING
          INPUT  = 'EN'
        IMPORTING
          OUTPUT = SY-LANGU.

      DELETE IT_RETORNO WHERE MSGTYP EQ 'S'.
      DELETE IT_RETORNO WHERE MSGTYP EQ 'E' AND MSGID EQ 'SV' AND MSGNR EQ '009'.

      SORT IT_RETORNO BY MSGID MSGTYP MSGNR.
      DELETE ADJACENT DUPLICATES FROM IT_RETORNO COMPARING MSGID MSGTYP MSGNR.

      IF IT_RETORNO[] IS INITIAL.
        CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1>' INTO I_HTML_ENTRA.
        CONCATENATE I_HTML_ENTRA '<TD align=center><font face="Verdana" color="#FF0000">There is no error message</font></TD>' INTO I_HTML_ENTRA.
        CONCATENATE I_HTML_ENTRA '</FONT></TR>' INTO I_HTML_ENTRA.
      ENDIF.

      LOOP AT IT_RETORNO.
        CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1>' INTO I_HTML_ENTRA.
        MOVE: IT_RETORNO-MSGID  TO SY-MSGID,
              IT_RETORNO-MSGTYP TO SY-MSGTY,
              IT_RETORNO-MSGNR  TO SY-MSGNO,
              IT_RETORNO-MSGV1  TO SY-MSGV1,
              IT_RETORNO-MSGV2  TO SY-MSGV2,
              IT_RETORNO-MSGV3  TO SY-MSGV3,
              IT_RETORNO-MSGV4  TO SY-MSGV4.

        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            LANGUAGE = SY-LANGU
            MSG_ID   = SY-MSGID
            MSG_NO   = IT_RETORNO-MSGNR
            MSG_VAR1 = SY-MSGV1
            MSG_VAR2 = SY-MSGV2
            MSG_VAR3 = SY-MSGV3
            MSG_VAR4 = SY-MSGV4
          IMPORTING
            MSG_TEXT = LC_MESSAGE.

        CONCATENATE I_HTML_ENTRA '<TD align=center>' LC_MESSAGE '</TD>' INTO I_HTML_ENTRA.
        CONCATENATE I_HTML_ENTRA '</FONT></TR>' INTO I_HTML_ENTRA.
      ENDLOOP.
      CONCATENATE I_HTML_ENTRA '</TABLE>' INTO I_HTML_ENTRA.
    ENDIF.

    CONCATENATE I_HTML_ENTRA '</body>'     INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '</html>'     INTO I_HTML_ENTRA.

    MOVE I_HTML_ENTRA TO I_HTML_SAIDA.

    CREATE OBJECT LO_CREATE_MAIL.

    CLEAR: LO_CREATE_MAIL->SUBJECT.

    "moving the subject.
    LOOP AT PCAD.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          INPUT  = PCAD-LOW
        IMPORTING
          OUTPUT = P_DATA_AUX.

      IF LO_CREATE_MAIL->SUBJECT IS INITIAL.
        CONCATENATE 'Reference rates error:' P_DATA_AUX INTO LO_CREATE_MAIL->SUBJECT SEPARATED BY SPACE.
      ELSE.
        CONCATENATE LO_CREATE_MAIL->SUBJECT P_DATA_AUX INTO LO_CREATE_MAIL->SUBJECT SEPARATED BY SPACE.
      ENDIF.

    ENDLOOP.

    CLEAR LS_MAIL_BODY.
    LS_MAIL_BODY-CONTENT_ASCII = I_HTML_SAIDA.
    LS_MAIL_BODY-MIME_TYPE     = 'text/html'.
    APPEND  LS_MAIL_BODY TO LT_MAIL_BODY.

    "moving the body
    MOVE LT_MAIL_BODY TO LO_CREATE_MAIL->BODY.

    "filling to email address
    CLEAR LS_RECEP.
    LS_RECEP-ADDRESS = 'suporte.sap@amaggi.com.br'.
    APPEND LS_RECEP TO LT_TO.
    MOVE LT_TO TO LO_CREATE_MAIL->TO.

    LS_RECEP-ADDRESS = 'controladoria.europa@amaggi.com.br'.
    APPEND LS_RECEP TO LT_COPY.

    MOVE LT_COPY TO LO_CREATE_MAIL->COPY.


    CLEAR LS_RECEP.
    LS_RECEP-ADDRESS = ADDRESS_FROM.
    MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.

    "send the email.
    "calling method to send the email
    CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL
      EXPORTING
        IV_MAIL_DATA       = LO_CREATE_MAIL
      RECEIVING
        EV_SEND_REQUEST_ID = LV_ACTIVITY.

    COMMIT WORK.

  ENDIF.

ENDFORM.                    " VERIFICAR_NAO_CADASTRADOS

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_FALTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LC_GDATU  text
*----------------------------------------------------------------------*
FORM VERIFICA_FALTA  USING  P_GDATU TYPE GDATU_INV.

  DATA: IT_TCURR_VER TYPE TABLE OF TCURR,
        LINHAS       TYPE I.

  RANGES: RG_KURST FOR TCURR-KURST.
  RANGES: RG_FCURR FOR TCURR-FCURR.
  RANGES: RG_TCURR FOR TCURR-TCURR.

  "Cotação
  RG_KURST-SIGN    = 'I'.
  RG_KURST-OPTION  = 'EQ'.
  RG_KURST-LOW     = 'M'.
  RG_KURST-HIGH    = 'M'.
  APPEND RG_KURST.
  RG_KURST-LOW     = 'G'.
  RG_KURST-HIGH    = 'G'.
  APPEND RG_KURST.
  RG_KURST-LOW     = 'B'.
  RG_KURST-HIGH    = 'B'.
  APPEND RG_KURST.

  "Moeda de procedência
  RG_FCURR-SIGN    = 'I'.
  RG_FCURR-OPTION  = 'EQ'.
  RG_FCURR-LOW     = 'EUR'.
  RG_FCURR-HIGH    = 'EUR'.
  APPEND RG_FCURR.
  RG_FCURR-LOW     = 'USD'.
  RG_FCURR-HIGH    = 'USD'.
  APPEND RG_FCURR.
  RG_FCURR-LOW     = 'GBP'.
  RG_FCURR-HIGH    = 'GBP'.
  APPEND RG_FCURR.

  "Moeda de destino
  RG_TCURR-SIGN    = 'I'.
  RG_TCURR-OPTION  = 'EQ'.
  RG_TCURR-LOW     = 'EUR'.
  RG_TCURR-HIGH    = 'EUR'.
  APPEND RG_TCURR.
  RG_TCURR-LOW     = 'USD'.
  RG_TCURR-HIGH    = 'USD'.
  APPEND RG_TCURR.
  RG_TCURR-LOW     = 'GBP'.
  RG_TCURR-HIGH    = 'GBP'.
  APPEND RG_TCURR.

  SELECT * INTO TABLE IT_TCURR_VER
    FROM TCURR
   WHERE GDATU EQ P_GDATU
     AND KURST IN RG_KURST
     AND FCURR IN RG_FCURR
     AND TCURR IN RG_TCURR.

  DESCRIBE TABLE IT_TCURR_VER LINES LINHAS.

  IF LINHAS EQ 18.
    SY-SUBRC = 0.
  ELSE.
    SY-SUBRC = 4.
  ENDIF.

ENDFORM.                    " VERIFICA_FALTA

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ONTEM_FERIADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ONTEM_FERIADO .

  "Ontem foi feriado?
  DATA(LC_ONTEM) = SY-DATUM.
  DATA(LC_HOJE)  = SY-DATUM.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = LC_HOJE
      DAYS      = 1
      MONTHS    = 0
      SIGNUM    = '-'
      YEARS     = 0
    IMPORTING
      CALC_DATE = LC_ONTEM.

  SELECT SINGLE * INTO WA_ZFIT0097
    FROM ZFIT0097
   WHERE LAND1    EQ 'NL'
     AND FERIADO  EQ LC_ONTEM.

  "Foi feriado
  IF SY-SUBRC IS INITIAL.

    CONCATENATE LC_HOJE+6(2) LC_HOJE+4(2) LC_HOJE(4) INTO LC_DATAT.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = LC_DATAT
      IMPORTING
        OUTPUT = LC_GDATU.

    "Verifica se já foi cadastrado para hoje
    PERFORM VERIFICA_FALTA USING LC_GDATU.

    "Se não foi
    IF SY-SUBRC IS NOT INITIAL.

      "Busca indice de Ontem
      SELECT * INTO TABLE IT_ZFI_ZMME_TCURR
        FROM ZFI_ZMME_TCURR
       WHERE LAND1 EQ 'NL'
         AND GDATU EQ LC_ONTEM.

      CLEAR: IT_TCURR[].

      CHECK IT_ZFI_ZMME_TCURR[] IS NOT INITIAL.

      LOOP AT IT_ZFI_ZMME_TCURR ASSIGNING FIELD-SYMBOL(<FS_ONTEM>).
        <FS_ONTEM>-GDATU = LC_HOJE.
        <FS_ONTEM>-DT_ATUALIZACAO = SY-DATUM.
        <FS_ONTEM>-HR_ATUALIZACAO = SY-UZEIT.

        IT_TCURR-KURST = <FS_ONTEM>-KURST.  "1 Tipo KURST_CURR
        IT_TCURR-FCURR = <FS_ONTEM>-FCURR.  "1 Tipo FCURR_CURR
        IT_TCURR-TCURR = <FS_ONTEM>-TCURR.  "1 Tipo TCURR_CURR
        IT_TCURR-GDATU = <FS_ONTEM>-GDATU.
        MOVE <FS_ONTEM>-UKURS TO IT_TCURR-UKURS.
        CONDENSE IT_TCURR-UKURS NO-GAPS.
        CALL FUNCTION 'STRING_REPLACE'
          EXPORTING
            PATTERN    = LC_PATTERN
            SUBSTITUTE = LC_SUBSTITUTE
          CHANGING
            TEXT       = IT_TCURR-UKURS.
        APPEND IT_TCURR.
      ENDLOOP.

      IF IT_ZFI_ZMME_TCURR[] IS NOT INITIAL.
        MODIFY ZFI_ZMME_TCURR FROM TABLE IT_ZFI_ZMME_TCURR.
        COMMIT WORK.
      ENDIF.

      CALL FUNCTION 'DAY_IN_WEEK'
        EXPORTING
          DATUM = LC_HOJE
        IMPORTING
          WOTNR = I_WOTNR.

      IF I_WOTNR EQ '5'.

        "Sabado
        LOOP AT IT_ZFI_ZMME_TCURR ASSIGNING <FS_ONTEM>.
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              DATE      = <FS_ONTEM>-GDATU
              DAYS      = 1
              MONTHS    = 0
              SIGNUM    = '+'
              YEARS     = 0
            IMPORTING
              CALC_DATE = <FS_ONTEM>-GDATU.
        ENDLOOP.

        IF IT_ZFI_ZMME_TCURR[] IS NOT INITIAL.
          MODIFY ZFI_ZMME_TCURR FROM TABLE IT_ZFI_ZMME_TCURR.
          COMMIT WORK.
        ENDIF.

        "Domingo
        LOOP AT IT_ZFI_ZMME_TCURR ASSIGNING <FS_ONTEM>.
          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              DATE      = <FS_ONTEM>-GDATU
              DAYS      = 1
              MONTHS    = 0
              SIGNUM    = '+'
              YEARS     = 0
            IMPORTING
              CALC_DATE = <FS_ONTEM>-GDATU.
        ENDLOOP.

        IF IT_ZFI_ZMME_TCURR[] IS NOT INITIAL.
          MODIFY ZFI_ZMME_TCURR FROM TABLE IT_ZFI_ZMME_TCURR.
          COMMIT WORK.
        ENDIF.

      ENDIF.


      IF IT_TCURR[] IS NOT INITIAL.

        LOOP AT IT_TCURR ASSIGNING FIELD-SYMBOL(<FS_TCURR>).

          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              DATE      = <FS_TCURR>-GDATU
              DAYS      = 1
              MONTHS    = 0
              SIGNUM    = '-'
              YEARS     = 0
            IMPORTING
              CALC_DATE = <FS_TCURR>-GDATU.

        ENDLOOP.

        CALL FUNCTION 'Z_FI_INBOUND_INDICE_FINANCEIRO'
          TABLES
            IT_TCURR   = IT_TCURR
            IT_RETORNO = IT_RETORNO.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
