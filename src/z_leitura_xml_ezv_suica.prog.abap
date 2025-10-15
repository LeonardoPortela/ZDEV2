*&---------------------------------------------------------------------*
*& Report  Z_LEITURA_XML_EZV_SUICA
*&
*&---------------------------------------------------------------------*
*&  Este programa lê o XML do Eidgenössische Zollverwaltung EZV e cadastra o indice
*&  do dolar (http://www.ezv.admin.ch/)
*&---------------------------------------------------------------------*

REPORT  Z_LEITURA_XML_EZV_SUICA.

TYPES: BEGIN OF TY_STRING,
         STR(25) TYPE C,
       END OF TY_STRING.
DATA IT_STRING TYPE TABLE OF TY_STRING.
DATA WA_STRING TYPE TY_STRING .

DATA: E_RESULTADO  TYPE STRING.

FIELD-SYMBOLS: <FS_TCURR> TYPE ZMME_TCURR.

DATA: IT_TCURR     TYPE TABLE OF ZMME_TCURR WITH HEADER LINE,
      IT_TCURR_AUX TYPE TABLE OF ZMME_TCURR WITH HEADER LINE,
      IT_RETORNO   TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      WA_TCURR     TYPE TCURR,

      UKURS_EUR	   TYPE UKURS_CURR,
      UKURS_BRL	   TYPE UKURS_CURR,
      UKURS_USD    TYPE UKURS_CURR,
      UKURS_GBP    TYPE UKURS_CURR,
      TEXTO_UKURS  TYPE C LENGTH 9,

      FATOR_EUR	   TYPE I,
      FATOR_BRL	   TYPE I,
      FATOR_USD    TYPE I,
      FATOR_GBP    TYPE I,
      FATOR        TYPE I,
      WA_ZFIT0097  TYPE ZFIT0097,
      WA_ZFIT00972 TYPE ZFIT0097.

DATA: I_WOTNR   TYPE P,
      LC_DATA   TYPE SY-DATUM,
      LC_DATAF  TYPE SY-DATUM,
      LC_DATAF2 TYPE SY-DATUM,
      LC_DATA1  TYPE SY-DATUM,
      LC_DATA2  TYPE SY-DATUM,
      LC_GDATU  TYPE GDATU_INV.

DATA: ADDRESS_TO   TYPE STRING,
      ADDRESS_FROM TYPE STRING.

RANGES: PCAD FOR  TCURR-GDATU.

DATA: IF_XML           TYPE REF TO IF_IXML,
      IF_DOCUMENT      TYPE REF TO IF_IXML_DOCUMENT,
      IF_STREAMFACTORY TYPE REF TO IF_IXML_STREAM_FACTORY,
      IF_STREAM        TYPE REF TO IF_IXML_ISTREAM,
      IF_XML_PARSER    TYPE REF TO IF_IXML_PARSER,
      IF_NODE          TYPE REF TO IF_IXML_NODE,
      ITERATOR         TYPE REF TO IF_IXML_NODE_ITERATOR,
      IF_NODE_FILHO    TYPE REF TO IF_IXML_NODE,
      IF_NODE_LIST     TYPE REF TO IF_IXML_NODE_LIST,
      ITERATOR_LIST    TYPE REF TO IF_IXML_NODE_ITERATOR,
      TAG_NAME         TYPE STRING,
      VALOR_DOM        TYPE STRING,
      VALOR_LINE       TYPE STRING,
      PREFIX_DOM       TYPE STRING,
      IF_MAP           TYPE REF TO IF_IXML_NAMED_NODE_MAP,
      CONTEXT          TYPE REF TO IF_IXML_NAMESPACE_CONTEXT,
      RVAL             TYPE STRING,
      DATA_XML         TYPE C LENGTH 10.

CALL METHOD ZCL_LER_URL=>LER_URL
  EXPORTING
    I_URL                      = 'http://www.afd.admin.ch/publicdb/newdb/mwst_kurse/wechselkurse.php'
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
        TRANSLATE TAG_NAME TO UPPER CASE.

        IF TAG_NAME EQ 'DATUM'.
          CONCATENATE VALOR_DOM(4) VALOR_DOM+5(2) VALOR_DOM+8(2) INTO WA_TCURR-GDATU.
        ENDIF.

        IF TAG_NAME EQ 'DEVISE'.
          DATA: COUNT_DOM TYPE I,
                INDEX_DOM TYPE I,
                IF_ATTR   TYPE REF TO IF_IXML_NODE,
                NAME_DOM  TYPE STRING.

          COUNT_DOM = IF_MAP->GET_LENGTH( ).

          DO COUNT_DOM TIMES.
            INDEX_DOM  = SY-INDEX - 1.

            IF_ATTR    = IF_MAP->GET_ITEM( INDEX_DOM ).
            NAME_DOM   = IF_ATTR->GET_NAME( ).
            VALOR_DOM  = IF_ATTR->GET_VALUE( ).
            TRANSLATE VALOR_DOM TO UPPER CASE.

            CASE VALOR_DOM.
              WHEN 'EUR'.
                IF_NODE_LIST = IF_NODE->GET_CHILDREN( ).
                ITERATOR_LIST = IF_NODE_LIST->CREATE_ITERATOR( ).
                IF_NODE_FILHO = ITERATOR_LIST->GET_NEXT( ).
                WHILE NOT IF_NODE_FILHO IS INITIAL.
                  TAG_NAME = IF_NODE_FILHO->GET_NAME( ).
                  TRANSLATE TAG_NAME TO UPPER CASE.

                  IF TAG_NAME EQ 'WAEHRUNG'.
                    VALOR_DOM  = IF_NODE_FILHO->GET_VALUE( ).
                    CLEAR: IT_STRING.
                    SPLIT VALOR_DOM AT ' ' INTO TABLE IT_STRING.
                    READ TABLE IT_STRING INTO WA_STRING INDEX 1.
                    MOVE WA_STRING-STR TO FATOR_EUR.
                  ENDIF.

                  IF TAG_NAME EQ 'KURS'.
                    VALOR_DOM  = IF_NODE_FILHO->GET_VALUE( ).
                    TRY.
                        MOVE VALOR_DOM TO UKURS_EUR.
                      CATCH CX_SY_CONVERSION_OVERFLOW.
                    ENDTRY.
                  ENDIF.
                  IF_NODE_FILHO = ITERATOR_LIST->GET_NEXT( ).
                ENDWHILE.
              WHEN 'BRL'.
                IF_NODE_LIST = IF_NODE->GET_CHILDREN( ).
                ITERATOR_LIST = IF_NODE_LIST->CREATE_ITERATOR( ).
                IF_NODE_FILHO = ITERATOR_LIST->GET_NEXT( ).
                WHILE NOT IF_NODE_FILHO IS INITIAL.
                  TAG_NAME = IF_NODE_FILHO->GET_NAME( ).
                  TRANSLATE TAG_NAME TO UPPER CASE.

                  IF TAG_NAME EQ 'WAEHRUNG'.
                    VALOR_DOM  = IF_NODE_FILHO->GET_VALUE( ).
                    CLEAR: IT_STRING.
                    SPLIT VALOR_DOM AT ' ' INTO TABLE IT_STRING.
                    READ TABLE IT_STRING INTO WA_STRING INDEX 1.
                    MOVE WA_STRING-STR TO FATOR_BRL.
                  ENDIF.

                  IF TAG_NAME EQ 'KURS'.
                    VALOR_DOM  = IF_NODE_FILHO->GET_VALUE( ).
                    TRY.
                        MOVE VALOR_DOM TO UKURS_BRL.
                      CATCH CX_SY_CONVERSION_OVERFLOW.
                    ENDTRY.
                  ENDIF.

                  IF_NODE_FILHO = ITERATOR_LIST->GET_NEXT( ).
                ENDWHILE.
              WHEN 'USD'.
                IF_NODE_LIST = IF_NODE->GET_CHILDREN( ).
                ITERATOR_LIST = IF_NODE_LIST->CREATE_ITERATOR( ).
                IF_NODE_FILHO = ITERATOR_LIST->GET_NEXT( ).
                WHILE NOT IF_NODE_FILHO IS INITIAL.
                  TAG_NAME = IF_NODE_FILHO->GET_NAME( ).
                  TRANSLATE TAG_NAME TO UPPER CASE.

                  IF TAG_NAME EQ 'WAEHRUNG'.
                    VALOR_DOM  = IF_NODE_FILHO->GET_VALUE( ).
                    CLEAR: IT_STRING.
                    SPLIT VALOR_DOM AT ' ' INTO TABLE IT_STRING.
                    READ TABLE IT_STRING INTO WA_STRING INDEX 1.
                    MOVE WA_STRING-STR TO FATOR_USD.
                  ENDIF.

                  IF TAG_NAME EQ 'KURS'.
                    VALOR_DOM  = IF_NODE_FILHO->GET_VALUE( ).
                    TRY.
                        MOVE VALOR_DOM TO UKURS_USD.
                      CATCH CX_SY_CONVERSION_OVERFLOW.
                    ENDTRY.
                  ENDIF.

                  IF_NODE_FILHO = ITERATOR_LIST->GET_NEXT( ).
                ENDWHILE.
              WHEN 'GBP'.
                IF_NODE_LIST = IF_NODE->GET_CHILDREN( ).
                ITERATOR_LIST = IF_NODE_LIST->CREATE_ITERATOR( ).
                IF_NODE_FILHO = ITERATOR_LIST->GET_NEXT( ).
                WHILE NOT IF_NODE_FILHO IS INITIAL.
                  TAG_NAME = IF_NODE_FILHO->GET_NAME( ).
                  TRANSLATE TAG_NAME TO UPPER CASE.

                  IF TAG_NAME EQ 'WAEHRUNG'.
                    VALOR_DOM  = IF_NODE_FILHO->GET_VALUE( ).
                    CLEAR: IT_STRING.
                    SPLIT VALOR_DOM AT ' ' INTO TABLE IT_STRING.
                    READ TABLE IT_STRING INTO WA_STRING INDEX 1.
                    MOVE WA_STRING-STR TO FATOR_GBP.
                  ENDIF.

                  IF TAG_NAME EQ 'KURS'.
                    VALOR_DOM  = IF_NODE_FILHO->GET_VALUE( ).
                    TRY.
                        MOVE VALOR_DOM TO UKURS_GBP.
                      CATCH CX_SY_CONVERSION_OVERFLOW.
                    ENDTRY.
                  ENDIF.

                  IF_NODE_FILHO = ITERATOR_LIST->GET_NEXT( ).
                ENDWHILE.
            ENDCASE.
          ENDDO.
        ENDIF.

    ENDCASE.
    IF_NODE = ITERATOR->GET_NEXT( ).
  ENDWHILE.

  FATOR = ( UKURS_EUR / FATOR_EUR ) * 100000 * FATOR_EUR.
  UKURS_EUR = FATOR / ( 100000 * FATOR_EUR ).

  FATOR = ( UKURS_USD / FATOR_USD ) * 100000 * FATOR_USD.
  UKURS_USD = FATOR / ( 100000 * FATOR_USD ).

  FATOR = ( UKURS_GBP / FATOR_GBP ) * 100000 * FATOR_GBP.
  UKURS_GBP = FATOR / ( 100000 * FATOR_GBP ).

  FATOR = ( UKURS_BRL / FATOR_BRL ) * 100000 * FATOR_BRL.
  UKURS_BRL = FATOR / ( 100000 * FATOR_BRL ).

  "Gran Bretagna """""""""""""""""""""""""""""""""""
  IF UKURS_GBP NE 0.
    CLEAR: IT_TCURR.
    MOVE: 'GBP'     TO IT_TCURR-TCURR,
          'CHF'     TO IT_TCURR-FCURR,
          'B'       TO IT_TCURR-KURST,
          UKURS_GBP TO IT_TCURR-UKURS,
          WA_TCURR-GDATU TO IT_TCURR-GDATU.
    APPEND IT_TCURR.

    MOVE: 'CHF'     TO IT_TCURR-TCURR,
          'GBP'     TO IT_TCURR-FCURR,
          'B'       TO IT_TCURR-KURST,
          UKURS_GBP TO IT_TCURR-UKURS,
          WA_TCURR-GDATU TO IT_TCURR-GDATU.
    APPEND IT_TCURR.
  ENDIF.

  "Euro """""""""""""""""""""""""""""""""""""""""""
  IF UKURS_EUR NE 0.
    CLEAR: IT_TCURR.
    MOVE: 'EUR'     TO IT_TCURR-TCURR,
          'CHF'     TO IT_TCURR-FCURR,
          'B'       TO IT_TCURR-KURST,
          UKURS_EUR TO IT_TCURR-UKURS,
          WA_TCURR-GDATU TO IT_TCURR-GDATU.
    APPEND IT_TCURR.

    MOVE: 'CHF'     TO IT_TCURR-TCURR,
          'EUR'     TO IT_TCURR-FCURR,
          'B'       TO IT_TCURR-KURST,
          UKURS_EUR TO IT_TCURR-UKURS,
          WA_TCURR-GDATU TO IT_TCURR-GDATU.
    APPEND IT_TCURR.
  ENDIF.

  "Dólar """"""""""""""""""""""""""""""""""""""""""
  IF UKURS_USD NE 0.
    CLEAR: IT_TCURR.
    MOVE: 'USD'     TO IT_TCURR-TCURR,
          'CHF'     TO IT_TCURR-FCURR,
          'B'       TO IT_TCURR-KURST,
          UKURS_USD TO IT_TCURR-UKURS,
          WA_TCURR-GDATU TO IT_TCURR-GDATU.
    APPEND IT_TCURR.
    MOVE 'G' TO IT_TCURR-KURST.
    APPEND IT_TCURR.
    MOVE 'M' TO IT_TCURR-KURST.
    APPEND IT_TCURR.

    MOVE: 'CHF'     TO IT_TCURR-TCURR,
          'USD'     TO IT_TCURR-FCURR,
          'B'       TO IT_TCURR-KURST,
          UKURS_USD TO IT_TCURR-UKURS,
          WA_TCURR-GDATU TO IT_TCURR-GDATU.
    APPEND IT_TCURR.
    MOVE 'G' TO IT_TCURR-KURST.
    APPEND IT_TCURR.
    MOVE 'M' TO IT_TCURR-KURST.
    APPEND IT_TCURR.
  ENDIF.

  "Real """""""""""""""""""""""""""""""""""""""""""
  IF UKURS_BRL NE 0.
    CLEAR: IT_TCURR.
    MOVE: 'BRL'     TO IT_TCURR-TCURR,
          'CHF'     TO IT_TCURR-FCURR,
          'B'       TO IT_TCURR-KURST,
          UKURS_BRL TO IT_TCURR-UKURS,
          WA_TCURR-GDATU TO IT_TCURR-GDATU.
    APPEND IT_TCURR.
    MOVE 'G' TO IT_TCURR-KURST.
    APPEND IT_TCURR.
    MOVE 'M' TO IT_TCURR-KURST.
    APPEND IT_TCURR.

    MOVE: 'CHF'     TO IT_TCURR-TCURR,
          'BRL'     TO IT_TCURR-FCURR,
          'B'       TO IT_TCURR-KURST,
          UKURS_BRL TO IT_TCURR-UKURS,
          WA_TCURR-GDATU TO IT_TCURR-GDATU.
    APPEND IT_TCURR.
    MOVE 'G' TO IT_TCURR-KURST.
    APPEND IT_TCURR.
    MOVE 'M' TO IT_TCURR-KURST.
    APPEND IT_TCURR.
  ENDIF.

  CHECK IT_TCURR[] IS NOT INITIAL.

  "02.05.2016
  MOVE WA_TCURR-GDATU TO LC_DATA.

  CALL FUNCTION 'DAY_IN_WEEK'
    EXPORTING
      DATUM = LC_DATA
    IMPORTING
      WOTNR = I_WOTNR.

  "Gera Indice para Sabado e Domingo
  IF I_WOTNR EQ 5.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        DATE      = LC_DATA
        DAYS      = 1
        MONTHS    = 0
        YEARS     = 0
      IMPORTING
        CALC_DATE = LC_DATA1.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        DATE      = LC_DATA
        DAYS      = 2
        MONTHS    = 0
        YEARS     = 0
      IMPORTING
        CALC_DATE = LC_DATA2.

    "Sabado
    LOOP AT IT_TCURR.
      IT_TCURR-GDATU = LC_DATA1.
      APPEND IT_TCURR TO IT_TCURR_AUX.
    ENDLOOP.

    "Domingo
    LOOP AT IT_TCURR.
      IT_TCURR-GDATU = LC_DATA2.
      APPEND IT_TCURR TO IT_TCURR_AUX.
    ENDLOOP.

    "Feriado
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        DATE      = LC_DATA
        DAYS      = 3
        MONTHS    = 0
        YEARS     = 0
      IMPORTING
        CALC_DATE = LC_DATAF.

    WA_ZFIT0097-LAND1 = 'CH'.

    SELECT SINGLE * INTO WA_ZFIT0097
      FROM ZFIT0097
     WHERE LAND1    EQ WA_ZFIT0097-LAND1
       AND FERIADO  EQ LC_DATAF.

    IF SY-SUBRC IS INITIAL.
      LOOP AT IT_TCURR.
        IT_TCURR-GDATU = LC_DATAF.
        APPEND IT_TCURR TO IT_TCURR_AUX.
      ENDLOOP.
    ELSE.
      CLEAR WA_ZFIT0097.
    ENDIF.

    LOOP AT IT_TCURR_AUX.
      APPEND IT_TCURR_AUX TO IT_TCURR.
    ENDLOOP.

  ELSE.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        DATE      = LC_DATA
        DAYS      = 1
        MONTHS    = 0
        YEARS     = 0
      IMPORTING
        CALC_DATE = LC_DATAF.

    WA_ZFIT0097-LAND1 = 'CH'.

    SELECT SINGLE * INTO WA_ZFIT0097
      FROM ZFIT0097
     WHERE LAND1    EQ WA_ZFIT0097-LAND1
       AND FERIADO  EQ LC_DATAF.

    IF SY-SUBRC IS INITIAL.
      LOOP AT IT_TCURR.
        IT_TCURR-GDATU = LC_DATAF.
        APPEND IT_TCURR TO IT_TCURR_AUX.
      ENDLOOP.

      CALL FUNCTION 'DAY_IN_WEEK'
        EXPORTING
          DATUM = LC_DATAF
        IMPORTING
          WOTNR = I_WOTNR.

      "Gera Indice para Domingo e Segunda
      IF I_WOTNR EQ 5.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            DATE      = LC_DATAF
            DAYS      = 1
            MONTHS    = 0
            YEARS     = 0
          IMPORTING
            CALC_DATE = LC_DATA1.

        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            DATE      = LC_DATAF
            DAYS      = 2
            MONTHS    = 0
            YEARS     = 0
          IMPORTING
            CALC_DATE = LC_DATA2.

        "Sabado
        LOOP AT IT_TCURR.
          IT_TCURR-GDATU = LC_DATA1.
          APPEND IT_TCURR TO IT_TCURR_AUX.
        ENDLOOP.

        "Domingo
        LOOP AT IT_TCURR.
          IT_TCURR-GDATU = LC_DATA2.
          APPEND IT_TCURR TO IT_TCURR_AUX.
        ENDLOOP.

        "Feriado
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            DATE      = LC_DATAF
            DAYS      = 3
            MONTHS    = 0
            YEARS     = 0
          IMPORTING
            CALC_DATE = LC_DATAF2.

        WA_ZFIT00972-LAND1 = 'CH'.

        SELECT SINGLE * INTO WA_ZFIT00972
          FROM ZFIT0097
         WHERE LAND1    EQ WA_ZFIT00972-LAND1
           AND FERIADO  EQ LC_DATAF2.

        IF SY-SUBRC IS INITIAL.
          LOOP AT IT_TCURR.
            IT_TCURR-GDATU = LC_DATAF2.
            APPEND IT_TCURR TO IT_TCURR_AUX.
          ENDLOOP.
        ELSE.
          CLEAR WA_ZFIT00972.
        ENDIF.

      ENDIF.

      LOOP AT IT_TCURR_AUX.
        APPEND IT_TCURR_AUX TO IT_TCURR.
      ENDLOOP.

    ELSE.
      CLEAR WA_ZFIT0097.
    ENDIF.

  ENDIF.

  CONCATENATE LC_DATA+6(2) LC_DATA+4(2) LC_DATA(4) INTO LC_DATA.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      INPUT  = LC_DATA
    IMPORTING
      OUTPUT = LC_GDATU.

  PERFORM VERIFICA_FALTA USING LC_GDATU.

  CHECK SY-SUBRC IS NOT INITIAL.

  IF IT_TCURR[] IS NOT INITIAL.

    CLEAR: IT_TCURR_AUX[].
    MOVE IT_TCURR[] TO IT_TCURR_AUX[].

    LOOP AT IT_TCURR_AUX ASSIGNING <FS_TCURR>.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = <FS_TCURR>-GDATU
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
          SIGNUM    = '-'
        IMPORTING
          CALC_DATE = <FS_TCURR>-GDATU.

    ENDLOOP.

    CALL FUNCTION 'Z_FI_INBOUND_INDICE_FINANCEIRO'
      TABLES
        IT_TCURR   = IT_TCURR_AUX
        IT_RETORNO = IT_RETORNO.

    PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU LC_DATA.

    IF I_WOTNR EQ 5.
      CONCATENATE LC_DATA1+6(2) LC_DATA1+4(2) LC_DATA1(4) INTO LC_DATA.
      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          INPUT  = LC_DATA
        IMPORTING
          OUTPUT = LC_GDATU.
      PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU LC_DATA1.

      CONCATENATE LC_DATA2+6(2) LC_DATA2+4(2) LC_DATA2(4) INTO LC_DATA.
      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          INPUT  = LC_DATA
        IMPORTING
          OUTPUT = LC_GDATU.
      PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU LC_DATA2.
    ENDIF.

    IF WA_ZFIT0097 IS NOT INITIAL.
      CONCATENATE WA_ZFIT0097-FERIADO+6(2) WA_ZFIT0097-FERIADO+4(2) WA_ZFIT0097-FERIADO(4) INTO LC_DATA.
      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          INPUT  = LC_DATA
        IMPORTING
          OUTPUT = LC_GDATU.
      PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU WA_ZFIT0097-FERIADO.
    ENDIF.

    IF WA_ZFIT00972 IS NOT INITIAL.
      CONCATENATE WA_ZFIT00972-FERIADO+6(2) WA_ZFIT00972-FERIADO+4(2) WA_ZFIT00972-FERIADO(4) INTO LC_DATA.
      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          INPUT  = LC_DATA
        IMPORTING
          OUTPUT = LC_GDATU.
      PERFORM GERAR_E_MAIL_CADASTRO USING LC_GDATU WA_ZFIT0097-FERIADO.
    ENDIF.

  ENDIF.

  PERFORM VERIFICAR_NAO_CADASTRADOS.

ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GERAR_E_MAIL_CADASTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GERAR_E_MAIL_CADASTRO  USING  P_GDATU TYPE GDATU_INV P_DATA TYPE SY-DATUM .

  DATA: LO_CREATE_MAIL      TYPE REF TO CL_CRM_EMAIL_DATA,
        LT_TO               TYPE CRMT_EMAIL_RECIPIENTS,
        LT_COPY             TYPE CRMT_EMAIL_RECIPIENTS,
        LS_RECEP            TYPE CRMS_EMAIL_RECIPIENT,
        LT_MAIL_BODY        TYPE CRMT_EMAIL_MIME_STRUC,
        LS_MAIL_BODY        TYPE CRMS_EMAIL_MIME_STRUC,
        LV_ACTIVITY         TYPE SYSUUID_X,
        P_DATA_AUX          TYPE CHAR10,
        STR_AUX             TYPE CHAR12,
        VALOR               TYPE F,
        IT_DATAS            TYPE TABLE OF ISCAL_DAY,
        P_DATA_FINAL_SEMANA TYPE DATUM.

  DATA: I_HTML_ENTRA TYPE STRING,
        I_HTML_SAIDA TYPE STRING.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
    EXPORTING
      INPUT  = P_GDATU
    IMPORTING
      OUTPUT = P_DATA_AUX.

  CONCATENATE P_DATA_AUX+6(4) P_DATA_AUX+3(2) P_DATA_AUX(2) INTO P_DATA_FINAL_SEMANA.

  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      HOLIDAY_CALENDAR = SPACE
      FACTORY_CALENDAR = 'ZT'
      DATE_FROM        = P_DATA_FINAL_SEMANA
      DATE_TO          = P_DATA_FINAL_SEMANA
    TABLES
      HOLIDAYS         = IT_DATAS
    EXCEPTIONS
      OTHERS           = 1.

  DESCRIBE TABLE IT_DATAS LINES DATA(QT_LINHAS).

  CHECK QT_LINHAS EQ 0.

  PCAD-SIGN   = 'I'.
  PCAD-OPTION = 'EQ'.
  PCAD-LOW     = P_GDATU.
  PCAD-HIGH    = P_GDATU.
  APPEND PCAD.

  DATA: IT_TCURR_MAIL TYPE TABLE OF TCURR WITH HEADER LINE.

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
  RG_FCURR-LOW     = 'BRL'.
  RG_FCURR-HIGH    = 'BRL'.
  APPEND RG_FCURR.

  "Moeda de destino
  RG_TCURR-SIGN    = 'I'.
  RG_TCURR-OPTION  = 'EQ'.
  RG_TCURR-LOW     = 'CHF'.
  RG_TCURR-HIGH    = 'CHF'.
  APPEND RG_TCURR.

  CLEAR: IT_TCURR_MAIL[].

  SELECT * INTO TABLE IT_TCURR_MAIL
    FROM TCURR
   WHERE GDATU EQ P_GDATU
     AND KURST IN RG_KURST
     AND FCURR IN RG_FCURR
     AND TCURR IN RG_TCURR.

  SELECT * APPENDING TABLE IT_TCURR_MAIL
    FROM TCURR
   WHERE GDATU EQ P_GDATU
     AND KURST IN RG_KURST
     AND TCURR IN RG_FCURR
     AND FCURR IN RG_TCURR.

  CLEAR: I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<html>'      INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<head>'      INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '</head>'     INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<body>'      INTO I_HTML_ENTRA.
  I_HTML_SAIDA = SY-HOST.
  TRANSLATE I_HTML_SAIDA TO UPPER CASE.
  CONCATENATE I_HTML_ENTRA '<DIV align=center><FONT face=Verdana size=4>' I_HTML_SAIDA '</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=center><FONT face=Verdana size=3>Exchange Reference Rates</FONT></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=center><a href="http://www.ezv.admin.ch/zollinfo_firmen/04203/04304/index.html?lang=fr">Administration fédérale des douanes AFD</a></DIV>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.

  CONCATENATE I_HTML_ENTRA '<TABLE border=1 align=center>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1><STRONG>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<TD align=center>Date</TD>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<TD align=center>Exchange Rate Type</TD>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<TD align=center>From currency</TD>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<TD align=center>To currency</TD>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '<TD align=center>Exchange Rate</TD>' INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '</TR>' INTO I_HTML_ENTRA.

  LOOP AT IT_TCURR_MAIL.
    CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1>' INTO I_HTML_ENTRA.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
      EXPORTING
        INPUT  = IT_TCURR_MAIL-GDATU
      IMPORTING
        OUTPUT = P_DATA_AUX.
    "Date
    CONCATENATE I_HTML_ENTRA '<TD align=center>' P_DATA_AUX '</TD>' INTO I_HTML_ENTRA.
    "Exchange Rate Type
    CONCATENATE I_HTML_ENTRA '<TD align=center>' IT_TCURR_MAIL-KURST '</TD>' INTO I_HTML_ENTRA.
    "From currency
    CONCATENATE I_HTML_ENTRA '<TD align=center>' IT_TCURR_MAIL-FCURR '</TD>' INTO I_HTML_ENTRA.
    "To currency
    CONCATENATE I_HTML_ENTRA '<TD align=center>' IT_TCURR_MAIL-TCURR '</TD>' INTO I_HTML_ENTRA.
    "Exchange Rate
    CALL FUNCTION 'CONVERSION_EXIT_EXCRT_OUTPUT'
      EXPORTING
        INPUT  = IT_TCURR_MAIL-UKURS
      IMPORTING
        OUTPUT = STR_AUX.

    CONCATENATE I_HTML_ENTRA '<TD align=right>' STR_AUX        '</TD>' INTO I_HTML_ENTRA.
    CONCATENATE I_HTML_ENTRA '</FONT></TR>' INTO I_HTML_ENTRA.
  ENDLOOP.

  CONCATENATE I_HTML_ENTRA '</TABLE>' INTO I_HTML_ENTRA.

  CONCATENATE I_HTML_ENTRA '</body>'  INTO I_HTML_ENTRA.
  CONCATENATE I_HTML_ENTRA '</html>'  INTO I_HTML_ENTRA.

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



  IF SY-SYSID EQ 'PRD'.

    ADDRESS_TO = 'fernanda.rodrigues@amaggi.ch'.
    ADDRESS_FROM = 'suporte.sap@amaggi.com.br'.

    LS_RECEP-ADDRESS = 'jacqueline.coelho@amaggi.com.br'.
    APPEND LS_RECEP TO LT_COPY.
    LS_RECEP-ADDRESS = 'claudia.cardoso@amaggi.ch'.
    APPEND LS_RECEP TO LT_COPY.
    LS_RECEP-ADDRESS = 'marcus.barbara@amaggi.com.br'.
    APPEND LS_RECEP TO LT_COPY.
    LS_RECEP-ADDRESS = 'larissa.hapanchuk@amaggi.com.br'.
    APPEND LS_RECEP TO LT_COPY.
    MOVE LT_COPY TO LO_CREATE_MAIL->COPY.

  ELSE.
    ADDRESS_TO = 'marcus.barbara@amaggi.com.br'.
    ADDRESS_FROM = 'marcus.barbara@amaggi.com.br'.
  ENDIF.

  "filling to email address
  CLEAR LS_RECEP.
  LS_RECEP-ADDRESS = ADDRESS_TO.
  APPEND LS_RECEP TO LT_TO.
  MOVE LT_TO TO LO_CREATE_MAIL->TO.

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
  RG_FCURR-LOW     = 'USD'.
  RG_FCURR-HIGH    = 'USD'.
  APPEND RG_FCURR.
  RG_FCURR-LOW     = 'BRL'.
  RG_FCURR-HIGH    = 'BRL'.
  APPEND RG_FCURR.

  "Moeda de destino
  RG_TCURR-SIGN    = 'I'.
  RG_TCURR-OPTION  = 'EQ'.
  RG_TCURR-LOW     = 'CHF'.
  RG_TCURR-HIGH    = 'CHF'.
  APPEND RG_TCURR.

  SELECT * INTO TABLE IT_TCURR_CAD
    FROM TCURR
   WHERE GDATU IN PCAD
     AND KURST IN RG_KURST
     AND FCURR IN RG_FCURR
     AND TCURR IN RG_TCURR.

  SELECT * APPENDING TABLE IT_TCURR_CAD
    FROM TCURR
   WHERE GDATU IN PCAD
     AND KURST IN RG_KURST
     AND FCURR IN RG_TCURR
     AND TCURR IN RG_FCURR.

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
                                           TCURR = RG_TCURR-LOW BINARY SEARCH.
          IF ( SY-SUBRC IS NOT INITIAL ) AND ( RG_FCURR-LOW NE RG_TCURR-LOW ).
            IT_TCURR_ERR-GDATU = PCAD-LOW.
            IT_TCURR_ERR-KURST = RG_KURST-LOW.
            IT_TCURR_ERR-FCURR = RG_FCURR-LOW.
            IT_TCURR_ERR-TCURR = RG_TCURR-LOW.
            APPEND IT_TCURR_ERR.
          ENDIF.

          READ TABLE IT_TCURR_CAD WITH KEY GDATU = PCAD-LOW
                                           KURST = RG_KURST-LOW
                                           FCURR = RG_TCURR-LOW
                                           TCURR = RG_FCURR-LOW BINARY SEARCH.
          IF ( SY-SUBRC IS NOT INITIAL ) AND ( RG_FCURR-LOW NE RG_TCURR-LOW ).
            IT_TCURR_ERR-GDATU = PCAD-LOW.
            IT_TCURR_ERR-KURST = RG_KURST-LOW.
            IT_TCURR_ERR-FCURR = RG_TCURR-LOW.
            IT_TCURR_ERR-TCURR = RG_FCURR-LOW.
            APPEND IT_TCURR_ERR.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  CLEAR: RG_KURST[].
  "Cotação
  RG_KURST-SIGN    = 'I'.
  RG_KURST-OPTION  = 'EQ'.
  RG_KURST-LOW     = 'B'.
  RG_KURST-HIGH    = 'B'.
  APPEND RG_KURST.

  CLEAR: RG_FCURR[].
  "Moeda de procedência
  RG_FCURR-SIGN    = 'I'.
  RG_FCURR-OPTION  = 'EQ'.
  RG_FCURR-LOW     = 'EUR'.
  RG_FCURR-HIGH    = 'EUR'.
  APPEND RG_FCURR.
  RG_FCURR-LOW     = 'GBP'.
  RG_FCURR-HIGH    = 'GBP'.
  APPEND RG_FCURR.

  CLEAR: RG_TCURR[].
  "Moeda de destino
  RG_TCURR-SIGN    = 'I'.
  RG_TCURR-OPTION  = 'EQ'.
  RG_TCURR-LOW     = 'CHF'.
  RG_TCURR-HIGH    = 'CHF'.
  APPEND RG_TCURR.

  CLEAR: IT_TCURR_CAD[].

  SELECT * INTO TABLE IT_TCURR_CAD
    FROM TCURR
   WHERE GDATU IN PCAD
     AND KURST IN RG_KURST
     AND FCURR IN RG_FCURR
     AND TCURR IN RG_TCURR.

  SELECT * APPENDING TABLE IT_TCURR_CAD
    FROM TCURR
   WHERE GDATU IN PCAD
     AND KURST IN RG_KURST
     AND FCURR IN RG_TCURR
     AND TCURR IN RG_FCURR.

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

          READ TABLE IT_TCURR_CAD WITH KEY GDATU = PCAD-LOW
                                           KURST = RG_KURST-LOW
                                           FCURR = RG_TCURR-LOW
                                           TCURR = RG_FCURR-LOW.
          IF ( SY-SUBRC IS NOT INITIAL ) AND ( RG_FCURR-LOW NE RG_TCURR-LOW ).
            IT_TCURR_ERR-GDATU = PCAD-LOW.
            IT_TCURR_ERR-KURST = RG_KURST-LOW.
            IT_TCURR_ERR-FCURR = RG_TCURR-LOW.
            IT_TCURR_ERR-TCURR = RG_FCURR-LOW.
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
    CONCATENATE I_HTML_ENTRA '<DIV align=center><a href="http://www.ezv.admin.ch/zollinfo_firmen/04203/04304/index.html?lang=fr">Administration fédérale des douanes AFD</a></DIV>' INTO I_HTML_ENTRA.
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

    IF SY-SYSID EQ 'PRD'.

      ADDRESS_TO = 'fernanda.rodrigues@amaggi.ch'.
      ADDRESS_FROM = 'suporte.sap@amaggi.com.br'.

      "filling to email address - copia
      LS_RECEP-ADDRESS = 'jacqueline.coelho@amaggi.com.br'.
      APPEND LS_RECEP TO LT_COPY.
      LS_RECEP-ADDRESS = 'claudia.cardoso@amaggi.ch'.
      APPEND LS_RECEP TO LT_COPY.
      LS_RECEP-ADDRESS = 'marcus.barbara@amaggi.com.br'.
      APPEND LS_RECEP TO LT_COPY.
      LS_RECEP-ADDRESS = 'larissa.hapanchuk@amaggi.com.br'.
      APPEND LS_RECEP TO LT_COPY.
      MOVE LT_COPY TO LO_CREATE_MAIL->COPY.

    ELSE.

      ADDRESS_TO = 'marcus.barbara@amaggi.com.br'.
      ADDRESS_FROM = 'marcus.barbara@amaggi.com.br'.

    ENDIF.

    "filling to email address
    CLEAR LS_RECEP.
    LS_RECEP-ADDRESS = ADDRESS_TO.
    APPEND LS_RECEP TO LT_TO.
    MOVE LT_TO TO LO_CREATE_MAIL->TO.

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
  RG_FCURR-LOW     = 'BRL'.
  RG_FCURR-HIGH    = 'BRL'.
  APPEND RG_FCURR.

  "Moeda de destino
  RG_TCURR-SIGN    = 'I'.
  RG_TCURR-OPTION  = 'EQ'.
  RG_TCURR-LOW     = 'CHF'.
  RG_TCURR-HIGH    = 'CHF'.
  APPEND RG_TCURR.

  SELECT * INTO TABLE IT_TCURR_VER
    FROM TCURR
   WHERE GDATU EQ P_GDATU
     AND KURST IN RG_KURST
     AND FCURR IN RG_FCURR
     AND TCURR IN RG_TCURR.

  SELECT * APPENDING TABLE IT_TCURR_VER
    FROM TCURR
   WHERE GDATU EQ P_GDATU
     AND KURST IN RG_KURST
     AND FCURR IN RG_TCURR
     AND TCURR IN RG_FCURR.

  DESCRIBE TABLE IT_TCURR_VER LINES LINHAS.

  IF LINHAS EQ 16.
    SY-SUBRC = 0.
  ELSE.
    SY-SUBRC = 4.
  ENDIF.

ENDFORM.                    " VERIFICA_FALTA
