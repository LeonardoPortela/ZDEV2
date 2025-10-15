*&---------------------------------------------------------------------*
*& Report  ZFIR079
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIR0081.

TYPES: BEGIN OF TY_VALUES,
         COTACAOCOMPRA   TYPE UKURS_CURR,
         COTACAOVENDA    TYPE UKURS_CURR,
         DATAHORACOTACAO TYPE STRING, ": "2020-03-03 10:10:18.371",
         TIPOBOLETIM     TYPE STRING,
       END OF TY_VALUES,
       IT_VALUES TYPE TABLE OF TY_VALUES WITH DEFAULT KEY.

TYPES: BEGIN OF TY_DADOS,
         VALUE TYPE IT_VALUES,
       END OF TY_DADOS.

TYPES: BEGIN OF TY_PARAM,
         MOEDA      TYPE STRING,
         DT_COTACAO TYPE STRING,
       END OF   TY_PARAM.

TYPES: BEGIN OF TY_TCURR_VALUES,
         LC_HORA_UP(8) TYPE C,
         LC_DATA_UP    TYPE SY-DATUM,
         KURST         TYPE ZMME_TCURR-KURST,
         MOEDA         TYPE ZMME_TCURR-TCURR,
         TAXA_COMPRA   TYPE UKURS_CURR,
         TAXA_VENDA    TYPE UKURS_CURR,
       END OF TY_TCURR_VALUES.


DATA: IT_DADOS        TYPE TABLE OF TY_DADOS,
      WA_DADOS        TYPE TY_DADOS,
      IT_TCURR        TYPE TABLE OF ZMME_TCURR,
      IT_TCURR_VALUES TYPE TABLE OF TY_TCURR_VALUES,
      WA_TCURR_VALUES TYPE TY_TCURR_VALUES,
      IT_RETORNO      TYPE TABLE OF BDCMSGCOLL,
      IT_PARAM        TYPE TABLE OF TY_PARAM,
      WA_PARAM        TYPE TY_PARAM.

DATA: E_WOTNR TYPE P.

DATA: TAXA_C(12)      TYPE C,
      TAXA_V(12)      TYPE C,
      TOTAL           TYPE UKURS_CURR,
      VTAXAM(12)      TYPE C,
      LC_DATA_UP      TYPE SY-DATUM,
      LC_HORA_UP(8)   TYPE C,
      LC_DATA         TYPE SY-DATUM,
      LC_DATA1        TYPE SY-DATUM,
      LC_DATA2        TYPE SY-DATUM,
      "   LC_DATA_F       TYPE SCAL-DATE,
      LC_DATA_F       TYPE SY-DATUM,
      WORKINGDAY_FLAG TYPE SCAL-INDICATOR,
      LVA_COTACAO_NOT_FOUND TYPE C LENGTH 50,
      IT_DATAS        TYPE TABLE OF  ISCAL_DAY.

RANGES R_DATA_UP FOR SY-DATUM.

DATA: OB_WEB_SERVICE TYPE REF TO  ZCL_WEBSERVICE,
      E_REASON       TYPE STRING,
      E_HTTP         TYPE REF TO  IF_HTTP_CLIENT,
      E_XML          TYPE STRING,
      E_URL          TYPE STRING VALUE 'https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/CotacaoMoedaDia(moeda=@moeda,dataCotacao=@dataCotacao)?'.

DATA: JSON_RETORNO TYPE STRING,
      E_JSON       TYPE STRING,
      E_ID         TYPE STRING.

DATA: VDTCOTACAO TYPE STRING.


DATA: V_SDLSTRTDT LIKE  TBTCJOB-SDLSTRTDT,
      V_SDLSTRTTM LIKE  TBTCJOB-SDLSTRTTM.
DATA: NUMBER           TYPE TBTCJOB-JOBCOUNT,
      NAME             TYPE TBTCJOB-JOBNAME VALUE 'MAGGI_ZFIR0081',
      PRINT_PARAMETERS TYPE PRI_PARAMS.


START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF SY-BATCH EQ ABAP_TRUE.
    TRY .
        ZCL_JOB=>GET_CK_PROGRAM_EXECUCAO( EXPORTING I_NOME_PROGRAM = SY-CPROG IMPORTING E_QTD = DATA(E_QTD) ).
      CATCH ZCX_JOB.
        E_QTD = 1.
    ENDTRY.

    IF E_QTD GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.


  "Validando se o job ja foi rodado e nao deu dump

  SELECT SINGLE * FROM TBTCO INTO @DATA(WA_TBTCO)
    WHERE JOBNAME   EQ @NAME
    AND   SDLSTRTDT EQ @SY-DATUM
    AND   STATUS    IN ( 'F', 'P', 'S' ).

  IF SY-SUBRC NE 0.

    V_SDLSTRTDT  = SY-DATUM.
    V_SDLSTRTTM  = SY-UZEIT + 1200.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        JOBNAME          = NAME
        SDLSTRTDT        = V_SDLSTRTDT
        SDLSTRTTM        = V_SDLSTRTTM
      IMPORTING
        JOBCOUNT         = NUMBER
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.

    IF SY-SUBRC IS INITIAL.
      SUBMIT ('ZFIR0081') TO SAP-SPOOL
                       SPOOL PARAMETERS PRINT_PARAMETERS
                       WITHOUT SPOOL DYNPRO
                       VIA JOB NAME NUMBER NUMBER
                       AND RETURN.

      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            JOBCOUNT             = NUMBER
            JOBNAME              = NAME
            STRTIMMED            = ABAP_TRUE
          EXCEPTIONS
            CANT_START_IMMEDIATE = 1
            INVALID_STARTDATE    = 2
            JOBNAME_MISSING      = 3
            JOB_CLOSE_FAILED     = 4
            JOB_NOSTEPS          = 5
            JOB_NOTEX            = 6
            LOCK_FAILED          = 7
            OTHERS               = 8.
      ENDIF.
    ENDIF.


    CALL FUNCTION 'DAY_IN_WEEK'
      EXPORTING
        DATUM = SY-DATUM
      IMPORTING
        WOTNR = E_WOTNR.

    CASE E_WOTNR.
      WHEN 1 OR 2 OR 3 OR 4 OR 5.

        DO 7 TIMES.

          PERFORM: Z_BUSCA_TAXA.

          "Verifica se foi retornado todas as cotações esperadas..
          CLEAR: LVA_COTACAO_NOT_FOUND.

          "Moeda EUR.
          READ TABLE IT_TCURR_VALUES  WITH KEY MOEDA = 'EUR' TRANSPORTING NO FIELDS.
          IF SY-SUBRC NE 0.
            LVA_COTACAO_NOT_FOUND = 'EUR'.
          ENDIF.

          READ TABLE IT_TCURR_VALUES  WITH KEY MOEDA = 'GBP' TRANSPORTING NO FIELDS.
          IF SY-SUBRC NE 0.
            LVA_COTACAO_NOT_FOUND = 'GBP'.
          ENDIF.

          READ TABLE IT_TCURR_VALUES  WITH KEY MOEDA = 'USD' TRANSPORTING NO FIELDS.
          IF SY-SUBRC NE 0.
            LVA_COTACAO_NOT_FOUND = 'USD'.
          ENDIF.

          IF LVA_COTACAO_NOT_FOUND IS INITIAL.
            EXIT.
          ELSE.
            IF IT_TCURR_VALUES[] IS INITIAL.
              MESSAGE 'Nenhuma taxa encontrada! Aguardando 30 minutos para nova busca!' TYPE 'S'.
            ELSE.
              MESSAGE |Taxa: { LVA_COTACAO_NOT_FOUND } não encontrada! Aguardando 30 minutos para nova busca!| TYPE 'S'.
            ENDIF.
            WAIT UP TO 1800 SECONDS. "Esperar uma hora
          ENDIF.
        ENDDO.

        PERFORM: Z_TRATA_DADOS.

        PERFORM Z_VERIFICA_CAD_TAXA.
    ENDCASE.

  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_TAXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_BUSCA_TAXA .


  CREATE OBJECT OB_WEB_SERVICE.

  CLEAR: VDTCOTACAO, IT_PARAM[], IT_TCURR_VALUES[].

  "MES/DIA/ANO
  CONCATENATE '#' SY-DATUM+4(2) '-'  SY-DATUM+6(2)   '-' SY-DATUM+0(4) '#'  INTO  VDTCOTACAO.
  REPLACE ALL OCCURRENCES OF '#' IN  VDTCOTACAO WITH |'| IGNORING CASE.


  WA_PARAM-MOEDA      = |'EUR'|.
  WA_PARAM-DT_COTACAO = VDTCOTACAO.
  APPEND WA_PARAM TO IT_PARAM.
  WA_PARAM-MOEDA      = |'GBP'|.
  APPEND WA_PARAM TO IT_PARAM.
  WA_PARAM-MOEDA      = |'USD'|.
  APPEND WA_PARAM TO IT_PARAM.
  CLEAR WA_PARAM.

  LOOP AT  IT_PARAM INTO WA_PARAM.

    E_URL = 'https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/CotacaoMoedaDia(moeda=@moeda,dataCotacao=@dataCotacao)?'.
    E_URL = E_URL && '@moeda=' &&  WA_PARAM-MOEDA && '&@dataCotacao=' && WA_PARAM-DT_COTACAO && '&$top=100&$format=json&$select=cotacaoCompra,cotacaoVenda,dataHoraCotacao,tipoBoletim'.

    CL_HTTP_CLIENT=>CREATE_BY_URL(
      EXPORTING
        URL                = E_URL
        IMPORTING
        CLIENT             = E_HTTP
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3 ).


    CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~request_method'
        VALUE = 'GET'.


    CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~server_protocol'
        VALUE = 'HTTP/1.1'.


    CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Content-Type'
        VALUE = 'application/json; charset=UTF-8'.


    CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Accept'
        VALUE = 'application/json; charset=UTF-8'.

    CLEAR JSON_RETORNO.
    OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
        EXPORTING
          I_HTTP                     = E_HTTP
          I_XML                      = E_XML
        IMPORTING
          E_REASON                   = E_REASON
        RECEIVING
          E_RESULTADO                = JSON_RETORNO
        EXCEPTIONS
          HTTP_COMMUNICATION_FAILURE = 1
          HTTP_INVALID_STATE         = 2
          HTTP_PROCESSING_FAILED     = 3
          HTTP_INVALID_TIMEOUT       = 4
          OTHERS                     = 5  ).

    IF  JSON_RETORNO IS NOT INITIAL.
      /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = WA_DADOS ).

      LOOP AT WA_DADOS-VALUE INTO DATA(WA_VALUES)
        WHERE TIPOBOLETIM EQ 'Fechamento PTAX'.

        REPLACE ALL OCCURRENCES OF |'| IN  WA_PARAM-MOEDA WITH '' IGNORING CASE.

        CONCATENATE WA_VALUES-DATAHORACOTACAO(4) WA_VALUES-DATAHORACOTACAO+5(2) WA_VALUES-DATAHORACOTACAO+8(2) INTO WA_TCURR_VALUES-LC_DATA_UP.
        WA_TCURR_VALUES-LC_HORA_UP =  WA_VALUES-DATAHORACOTACAO+11(8).

        WA_TCURR_VALUES-MOEDA       = WA_PARAM-MOEDA.
        WA_TCURR_VALUES-TAXA_COMPRA = WA_VALUES-COTACAOCOMPRA.
        WA_TCURR_VALUES-TAXA_VENDA  = WA_VALUES-COTACAOVENDA.

        APPEND WA_TCURR_VALUES TO IT_TCURR_VALUES.
        CLEAR: WA_TCURR_VALUES.
      ENDLOOP.
    ENDIF.

    CLEAR: WA_PARAM, E_URL.
    FREE WA_DADOS.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_TRATA_DADOS.

  REFRESH: IT_RETORNO, IT_TCURR, R_DATA_UP.

  CLEAR: LC_HORA_UP, LC_DATA_UP, E_WOTNR.


  IF IT_TCURR_VALUES IS NOT INITIAL.

    LOOP AT IT_TCURR_VALUES INTO WA_TCURR_VALUES.

      LC_HORA_UP = WA_TCURR_VALUES-LC_HORA_UP.

      TOTAL = ( ( WA_TCURR_VALUES-TAXA_VENDA + WA_TCURR_VALUES-TAXA_COMPRA ) / 2 ).
      WRITE TOTAL TO VTAXAM.
      WRITE WA_TCURR_VALUES-TAXA_COMPRA TO TAXA_C .
      WRITE WA_TCURR_VALUES-TAXA_VENDA  TO TAXA_V.

      CONDENSE: VTAXAM, TAXA_C, TAXA_V.

      "Venda
      PERFORM Z_PREENCHE_TCURR USING 'B'  WA_TCURR_VALUES-MOEDA WA_TCURR_VALUES-LC_DATA_UP TAXA_V.
      "Compra
      PERFORM Z_PREENCHE_TCURR USING 'G'  WA_TCURR_VALUES-MOEDA WA_TCURR_VALUES-LC_DATA_UP TAXA_C.
      "Médio
      PERFORM Z_PREENCHE_TCURR USING 'M'  WA_TCURR_VALUES-MOEDA WA_TCURR_VALUES-LC_DATA_UP VTAXAM.

      IF WA_TCURR_VALUES-MOEDA EQ 'EUR'.
        "Venda
        PERFORM Z_PREENCHE_TCURR USING 'EURX' WA_TCURR_VALUES-MOEDA WA_TCURR_VALUES-LC_DATA_UP TAXA_V.
      ENDIF.

      R_DATA_UP-SIGN    = 'I'.
      R_DATA_UP-OPTION  = 'EQ'.
      R_DATA_UP-LOW     = WA_TCURR_VALUES-LC_DATA_UP.
      APPEND  R_DATA_UP.


      "Verifica se é feriado
      LC_DATA_F = WA_TCURR_VALUES-LC_DATA_UP + 1.

      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          HOLIDAY_CALENDAR = 'MG'
          FACTORY_CALENDAR = 'ZT'
          DATE_FROM        = LC_DATA_F
          DATE_TO          = LC_DATA_F
        TABLES
          HOLIDAYS         = IT_DATAS.

      READ TABLE IT_DATAS INTO DATA(WA_DATAS) WITH KEY FREEDAY = 'X'.

      IF SY-SUBRC EQ 0 .
        "Venda
        PERFORM Z_PREENCHE_TCURR USING 'B' WA_TCURR_VALUES-MOEDA LC_DATA_F TAXA_V.
        "Compra
        PERFORM Z_PREENCHE_TCURR USING 'G' WA_TCURR_VALUES-MOEDA LC_DATA_F TAXA_C.
        "Médio
        PERFORM Z_PREENCHE_TCURR USING 'M' WA_TCURR_VALUES-MOEDA LC_DATA_F VTAXAM.

        IF WA_TCURR_VALUES-MOEDA EQ 'EUR'.
          "Venda
          PERFORM Z_PREENCHE_TCURR USING 'EURX' WA_TCURR_VALUES-MOEDA LC_DATA_F TAXA_V.
        ENDIF.

        R_DATA_UP-SIGN    = 'I'.
        R_DATA_UP-OPTION  = 'EQ'.
        R_DATA_UP-LOW     = LC_DATA_F.
        APPEND  R_DATA_UP.

        "Feriado com final de semana
        PERFORM Z_ATUALIZA_FINAL_SEMANA USING  LC_DATA_F.
      ENDIF.

      "Final de semana
      PERFORM Z_ATUALIZA_FINAL_SEMANA USING   WA_TCURR_VALUES-LC_DATA_UP.

      CLEAR:  TAXA_C, TAXA_V, VTAXAM,  LC_DATA, LC_DATA1, LC_DATA2.
    ENDLOOP.

    CALL FUNCTION 'Z_FI_INBOUND_INDICE_FINANCEIRO'
      TABLES
        IT_TCURR   = IT_TCURR
        IT_RETORNO = IT_RETORNO.

  ELSE.

    R_DATA_UP-SIGN    = 'I'.
    R_DATA_UP-OPTION  = 'EQ'.
    R_DATA_UP-LOW     = SY-DATUM.
    APPEND  R_DATA_UP.
  ENDIF.

ENDFORM.

FORM Z_VERIFICA_CAD_TAXA.

  DATA: IT_TCURR_VER TYPE TABLE OF TCURR,
        LC_DATA2     TYPE SY-DATUM,
        LC_DATAT     TYPE CHAR08,
        LINHAS       TYPE I.

  RANGES R_GDATU FOR SCURR-GDATU.
  RANGES R_KURST FOR TCURR-KURST.
  RANGES R_FCURR FOR TCURR-FCURR.
  RANGES R_TCURR FOR TCURR-TCURR.


  LOOP AT R_DATA_UP.
    R_GDATU-SIGN   = 'I'.
    R_GDATU-OPTION = 'EQ'.

    LC_DATA2 = R_DATA_UP-LOW + 1.
    CONCATENATE LC_DATA2+6(2) LC_DATA2+4(2)  LC_DATA2(4) INTO LC_DATAT.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = LC_DATAT
      IMPORTING
        OUTPUT = R_GDATU-LOW.

    APPEND R_GDATU.
    CLEAR: LC_DATA2, LC_DATAT.
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
  R_FCURR-LOW     = 'USD'.
  APPEND  R_FCURR.
  R_FCURR-LOW     = 'EUR'.
  APPEND  R_FCURR.
  R_FCURR-LOW     = 'GBP'.
  APPEND  R_FCURR.

  R_TCURR-SIGN    = 'I'.
  R_TCURR-OPTION  = 'EQ'.
  R_TCURR-LOW     = 'BRL'.
  APPEND R_TCURR.
  R_TCURR-LOW     = 'USD'.
  APPEND R_TCURR.
  R_TCURR-LOW     = 'EUR'.
  APPEND R_TCURR.
  R_TCURR-LOW     = 'GBP'.
  APPEND R_TCURR.


  SELECT * FROM TCURR INTO TABLE IT_TCURR_VER
    WHERE GDATU IN R_GDATU
     AND  KURST IN R_KURST
     AND  FCURR IN R_FCURR
     AND  TCURR IN R_TCURR.

  DESCRIBE TABLE IT_TCURR_VER LINES LINHAS.

  IF LINHAS >= 6.
    EXPORT V_TRUE    FROM ABAP_TRUE    TO MEMORY ID 'P_TRUE'.
    EXPORT V_HORA    FROM LC_HORA_UP   TO MEMORY ID 'P_HORA'.
  ELSE.
    EXPORT V_FALSE   FROM ABAP_TRUE    TO MEMORY ID 'P_FALSE'.
  ENDIF.


  FREE MEMORY ID 'DATA_UP'.
  EXPORT R_DATA_UP TO MEMORY ID 'DATA_UP'.

  SUBMIT ZFIR0082 AND RETURN.

ENDFORM.


FORM Z_PREENCHE_TCURR USING E_TIPO  E_TCURR  E_GDATU E_UKURS.

  DATA: W_TCURR TYPE ZMME_TCURR.

  CASE E_TIPO.
    WHEN 'B'.
      W_TCURR-KURST = 'B'.
      W_TCURR-FCURR = 'BRL'.
      W_TCURR-TCURR = E_TCURR.
      W_TCURR-GDATU = E_GDATU.
      W_TCURR-UKURS = E_UKURS.
      APPEND W_TCURR TO IT_TCURR.

      W_TCURR-FCURR = E_TCURR.
      W_TCURR-TCURR = 'BRL'.
      APPEND W_TCURR TO IT_TCURR.
    WHEN 'G'.
      W_TCURR-KURST = 'G'.
      W_TCURR-FCURR = 'BRL'.
      W_TCURR-TCURR = E_TCURR.
      W_TCURR-GDATU = E_GDATU.
      W_TCURR-UKURS = E_UKURS.
      APPEND W_TCURR TO IT_TCURR.

      W_TCURR-FCURR = E_TCURR.
      W_TCURR-TCURR = 'BRL'.
      APPEND W_TCURR TO IT_TCURR.

    WHEN 'M'.
      W_TCURR-KURST = 'M'.
      W_TCURR-FCURR = 'BRL'.
      W_TCURR-TCURR = E_TCURR.
      W_TCURR-GDATU = E_GDATU.
      W_TCURR-UKURS = E_UKURS.
      APPEND W_TCURR TO IT_TCURR.

      W_TCURR-FCURR = E_TCURR.
      W_TCURR-TCURR = 'BRL'. "irá passar
      APPEND W_TCURR TO IT_TCURR.

    WHEN 'EURX'.
      W_TCURR-KURST = 'EURX'.
      W_TCURR-FCURR = 'BRL'.
      W_TCURR-TCURR = E_TCURR.
      W_TCURR-GDATU = E_GDATU.
      W_TCURR-UKURS = E_UKURS.
      APPEND W_TCURR TO IT_TCURR.

      W_TCURR-FCURR = E_TCURR.
      W_TCURR-TCURR = 'BRL'. "irá passar
      APPEND W_TCURR TO IT_TCURR.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZA_FINAL_SEMANA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TCURR_VALUES_LC_DATA_UP  text
*----------------------------------------------------------------------*
FORM Z_ATUALIZA_FINAL_SEMANA  USING LC_DATA.

  DATA: LC_DATA_F    TYPE SY-DATUM,
        IT_DATAS     TYPE TABLE OF  ISCAL_DAY.


  "Final de semana
  CALL FUNCTION 'DAY_IN_WEEK'
    EXPORTING
      "DATUM = WA_TCURR_VALUES-LC_DATA_UP
      DATUM = LC_DATA
    IMPORTING
      WOTNR = E_WOTNR.

  CASE E_WOTNR. "Caso for sexta-Feira
    WHEN 5.
      "domingo
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = LC_DATA "WA_TCURR_VALUES-LC_DATA_UP
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = LC_DATA1.

      "Venda
      PERFORM Z_PREENCHE_TCURR USING 'B' WA_TCURR_VALUES-MOEDA LC_DATA1 TAXA_V.
      "Compra BRL-USD
      PERFORM Z_PREENCHE_TCURR USING 'G' WA_TCURR_VALUES-MOEDA LC_DATA1 TAXA_C.
      "Médio
      PERFORM Z_PREENCHE_TCURR USING 'M' WA_TCURR_VALUES-MOEDA LC_DATA1 VTAXAM.

      IF WA_TCURR_VALUES-MOEDA EQ 'EUR'.
        "Venda
        PERFORM Z_PREENCHE_TCURR USING 'EURX' WA_TCURR_VALUES-MOEDA LC_DATA1 TAXA_V.
      ENDIF.


      "segunda
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = LC_DATA1
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = LC_DATA2.

      "Venda
      PERFORM Z_PREENCHE_TCURR USING 'B'  WA_TCURR_VALUES-MOEDA LC_DATA2 TAXA_V.
      "Compra BRL-USD
      PERFORM Z_PREENCHE_TCURR USING 'G'  WA_TCURR_VALUES-MOEDA LC_DATA2 TAXA_C.
      "Médio
      PERFORM Z_PREENCHE_TCURR USING 'M'  WA_TCURR_VALUES-MOEDA LC_DATA2 VTAXAM.

      IF WA_TCURR_VALUES-MOEDA EQ 'EUR'.
        "Venda
        PERFORM Z_PREENCHE_TCURR USING 'EURX' WA_TCURR_VALUES-MOEDA LC_DATA2 TAXA_V.
      ENDIF.

      "Verifica se segunda é feriado, caso sim replicar cotação
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = LC_DATA2
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = LC_DATA_F.

      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          HOLIDAY_CALENDAR = 'MG'
          FACTORY_CALENDAR = 'ZT'
          DATE_FROM        = LC_DATA_F
          DATE_TO          = LC_DATA_F
        TABLES
          HOLIDAYS         = IT_DATAS.

      READ TABLE IT_DATAS INTO DATA(WA_DATAS) WITH KEY FREEDAY = 'X'.

      IF SY-SUBRC EQ 0 .

        "Venda
        PERFORM Z_PREENCHE_TCURR USING 'B'  WA_TCURR_VALUES-MOEDA LC_DATA_F TAXA_V.
        "Compra BRL-USD
        PERFORM Z_PREENCHE_TCURR USING 'G'  WA_TCURR_VALUES-MOEDA LC_DATA_F TAXA_C.
        "Médio
        PERFORM Z_PREENCHE_TCURR USING 'M'  WA_TCURR_VALUES-MOEDA LC_DATA_F VTAXAM.

        IF WA_TCURR_VALUES-MOEDA EQ 'EUR'.
          "Venda
          PERFORM Z_PREENCHE_TCURR USING 'EURX' WA_TCURR_VALUES-MOEDA LC_DATA_F TAXA_V.
        ENDIF.

        R_DATA_UP-SIGN    = 'I'.
        R_DATA_UP-OPTION  = 'EQ'.
        R_DATA_UP-LOW     = LC_DATA_F.
        APPEND  R_DATA_UP.
      ENDIF.


      R_DATA_UP-SIGN    = 'I'.
      R_DATA_UP-OPTION  = 'EQ'.
      R_DATA_UP-LOW     = LC_DATA1.
      APPEND  R_DATA_UP.
      R_DATA_UP-LOW     = LC_DATA2.
      APPEND  R_DATA_UP.
  ENDCASE.

ENDFORM.
