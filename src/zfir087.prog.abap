*&---------------------------------------------------------------------*
*& Report  ZFIR079
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir087.

TYPES: BEGIN OF ty_values,
         cotacaocompra   TYPE ukurs_curr,
         cotacaovenda    TYPE ukurs_curr,
         datahoracotacao TYPE string, ": "2020-03-03 10:10:18.371",
         tipoboletim     TYPE string,
       END OF ty_values,
       it_values TYPE TABLE OF ty_values WITH DEFAULT KEY.

TYPES: BEGIN OF ty_dados,
         value TYPE it_values,
       END OF ty_dados.

TYPES: BEGIN OF ty_param,
         moeda      TYPE string,
         dt_cotacao TYPE string,
       END OF   ty_param.

TYPES: BEGIN OF ty_tcurr_values,
         lc_hora_up(8) TYPE c,
         lc_data_up    TYPE sy-datum,
         kurst         TYPE zmme_tcurr-kurst,
         moeda         TYPE zmme_tcurr-tcurr,
         taxa_compra   TYPE ukurs_curr,
         taxa_venda    TYPE ukurs_curr,
       END OF ty_tcurr_values.

TYPES: BEGIN OF ty_result,
         status  TYPE string,
         info    TYPE string,
         content TYPE zfite0003,
       END OF ty_result.


DATA: ws_dados        TYPE zfite0002,
      ws_dados_aux    TYPE ty_result,
*      it_result       TYPE data,
*      it_dados        TYPE TABLE OF zfite0001,
*      wa_dados        TYPE zfite0003,
      it_tcurr        TYPE TABLE OF zmme_tcurr,
      it_tcurr_values TYPE TABLE OF ty_tcurr_values,
      wa_tcurr_values TYPE ty_tcurr_values,
      it_retorno      TYPE TABLE OF bdcmsgcoll,
      it_param        TYPE TABLE OF ty_param,
      wa_param        TYPE ty_param.

DATA: e_wotnr TYPE p.

DATA: taxa_c(12)      TYPE c,
      taxa_v(12)      TYPE c,
      total           TYPE ukurs_curr,
      vtaxam(12)      TYPE c,
      lc_data_up      TYPE sy-datum,
      lc_hora_up(8)   TYPE c,
      lc_data         TYPE sy-datum,
      lc_data1        TYPE sy-datum,
      lc_data2        TYPE sy-datum,
      "   LC_DATA_F       TYPE SCAL-DATE,
      lc_data_f       TYPE sy-datum,
      workingday_flag TYPE scal-indicator,
      it_datas        TYPE TABLE OF  iscal_day.

RANGES r_data_up FOR sy-datum.

DATA: holiday_calendar TYPE scal-hcalid,
      factory_calendar TYPE scal-fcalid.

DATA: ob_web_service TYPE REF TO  zcl_webservice,
      e_reason       TYPE string,
      e_http         TYPE REF TO  if_http_client,
      e_xml          TYPE string,
      e_url          TYPE string VALUE 'http://seapi.amaggi.com.br:5099/PtaxArgentina/Ultima'.
*      e_url          TYPE string VALUE 'https://services.matba.com.ar/restservices/api/cotizaciones/tipos-de-cambio?fecha='.


DATA: json_retorno TYPE string,
      e_json       TYPE string,
      e_id         TYPE string.

DATA: vdtcotacao TYPE string.


DATA: v_sdlstrtdt LIKE  tbtcjob-sdlstrtdt,
      v_sdlstrttm LIKE  tbtcjob-sdlstrttm.
DATA: number           TYPE tbtcjob-jobcount,
      name             TYPE tbtcjob-jobname VALUE 'JOB_ZFIR087_PTAX_ARG',
      print_parameters TYPE pri_params.
DATA: t_vfd TYPE datum.

START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  DATA(lc_hoje) = |{ sy-datum+6(2) }{ sy-datum+4(2) }{ sy-datum(4) }|.

* FGM - valida se a taxa já esta na base
  CALL FUNCTION 'READ_EXCHANGE_RATE'
    EXPORTING
      client           = sy-mandt
      date             = sy-datum
      foreign_currency = 'USD'
      local_currency   = 'ARS'
      type_of_rate     = 'BCRA'
      exact_date       = 'X'
    IMPORTING
      valid_from_date  = t_vfd
    EXCEPTIONS
      no_rate_found    = 1
      no_factors_found = 2
      no_spread_found  = 3
      derived_2_times  = 4
      overflow         = 5
      zero_rate        = 6
      OTHERS           = 7.

  IF sy-subrc EQ 0.
    MESSAGE |Taxa cambio já cadastrada para a data de { lc_hoje } | TYPE 'S'.
    EXIT.
  ELSE.
    MESSAGE |Taxa de cambio ainda NÃO foi cadastrada para a data de { lc_hoje } | TYPE 'S'.
  ENDIF.

  "Validando se o job ja foi rodado e nao deu dump

  SELECT SINGLE * FROM tbtco INTO @DATA(wa_tbtco)
    WHERE jobname   EQ @name
    AND   sdlstrtdt EQ @sy-datum
    AND   status    IN ( 'P', 'S' ).

* FGM - 'F' = JOB Concluído - isso impede o job rode mais de 1x ao dia.
*    AND   status    IN ( 'F', 'P', 'S' ).

  IF sy-subrc NE 0.

    v_sdlstrtdt  = sy-datum.
    v_sdlstrttm  = sy-uzeit + 900.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = name
        sdlstrtdt        = v_sdlstrtdt
        sdlstrttm        = v_sdlstrttm
      IMPORTING
        jobcount         = number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc IS INITIAL.
      SUBMIT ('ZFIR087') TO SAP-SPOOL
                       SPOOL PARAMETERS print_parameters
                       WITHOUT SPOOL DYNPRO
                       VIA JOB name NUMBER number
                       AND RETURN.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = number
            jobname              = name
            strtimmed            = abap_true
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.
      ENDIF.
    ENDIF.


    CALL FUNCTION 'DAY_IN_WEEK'
      EXPORTING
        datum = sy-datum
      IMPORTING
        wotnr = e_wotnr.

    CASE e_wotnr.
      WHEN 1 OR 2 OR 3 OR 4 OR 5.

        DO 48 TIMES.

          DATA(lc_atual) = |{ sy-datum+6(2) }{ sy-datum+4(2) }{ sy-datum(4) }|.
          IF lc_hoje NE lc_atual.
            MESSAGE 'Não foi encontrada a taxa. JOB será encerrado.' TYPE 'S'.
            CLEAR it_tcurr_values.
            EXIT.
          ENDIF.

          PERFORM: z_busca_taxa.
          IF it_tcurr_values[] IS NOT INITIAL.
* FGM - Antes se programa encontrasse a taxa mesmo que do dia anterior finalizava
*            EXIT.
            READ TABLE it_tcurr_values INTO wa_tcurr_values INDEX 1.
            IF wa_tcurr_values-lc_data_up = sy-datum.
              EXIT.
            ELSE.
              MESSAGE 'Não foi encontrada a taxa do dia! Aguardando 30 minutos para nova busca!' TYPE 'S'.
              WAIT UP TO 1800 SECONDS. "Esperar uma hora
            ENDIF.
          ELSE.
            MESSAGE 'Nenhuma taxa encontrada! Aguardando 30 minutos para nova busca!' TYPE 'S'.
            WAIT UP TO 1800 SECONDS. "Esperar uma hora
          ENDIF.

        ENDDO.

        PERFORM: z_trata_dados.

        PERFORM z_verifica_cad_taxa.
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
FORM z_busca_taxa .
  DATA: parametro TYPE string,
* ---> S4 Migration - 19/06/2023 - JS
*        dia       TYPE char02,
        dia(02)   TYPE c,
* <--- S4 Migration - 19/06/2023 - JS
        ano       TYPE char04,
* ---> S4 Migration - 09/07/2023 - FC
        "mes       TYPE char02.
        mes       TYPE char2.
* <--- S4 Migration - 09/07/2023 - FC


  CREATE OBJECT ob_web_service.
  FREE: it_tcurr_values.
  CLEAR: vdtcotacao, it_param[], it_tcurr_values[], parametro, dia, ano, mes.

  dia = sy-datum+6(2).
  mes = sy-datum+4(2).
  ano = sy-datum(4).


  wa_param-moeda      = |ARS|.
  APPEND wa_param TO it_param.
  CLEAR wa_param.

  LOOP AT  it_param INTO wa_param.

*    parametro = dia && '%2F' && mes && '%2F' && ano.

*    e_url = 'https://services.matba.com.ar/restservices/api/cotizaciones/tipos-de-cambio?fecha=' && parametro.
*    e_url = e_url && '@moeda=' &&  wa_param-moeda && '&@dataCotacao=' && wa_param-dt_cotacao && '&$top=100&$format=json&$select=cotacaoCompra,cotacaoVenda,dataHoraCotacao,tipoBoletim'.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = e_url
        IMPORTING
        client             = e_http
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3 ).


    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'GET'.


    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.


    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json; charset=UTF-8'.


    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = 'Accept'
        value = 'application/json; charset=UTF-8'.

    CLEAR json_retorno.
    ob_web_service->zif_webservice~consultar(
        EXPORTING
          i_http                     = e_http
          i_xml                      = e_xml
        IMPORTING
          e_reason                   = e_reason
        RECEIVING
          e_resultado                = json_retorno
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5  ).

    IF  json_retorno IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = ws_dados ).
      IF ws_dados-content IS NOT INITIAL.

*      DATA(wa_dados_conten) = ws_dados-content.
*
*      READ TABLE it_dados INTO DATA(ls_dados) INDEX 1.
        LOOP AT ws_dados-content INTO DATA(wa_dados_conten).
*        WHERE descripcion EQ 'Cambio Oficial'.
*
*        REPLACE ALL OCCURRENCES OF |,| IN  wa_dados-tasavtanominal WITH '.' IGNORING CASE.
*
          CONCATENATE wa_dados_conten-data(4) wa_dados_conten-data+5(2) wa_dados_conten-data+8(2) INTO wa_tcurr_values-lc_data_up.
*        wa_tcurr_values-lc_data_up = wa_tcurr_values-lc_data_up - 1.
          wa_tcurr_values-lc_data_up = wa_tcurr_values-lc_data_up.
          wa_tcurr_values-moeda       = wa_param-moeda.

*** Stefanini - IR247099 - 22/08/2025 - LAZAROSR - Início de Alteração
          REPLACE ALL OCCURRENCES OF ',' IN wa_dados_conten-taxa WITH '.'.
*** Stefanini - IR247099 - 22/08/2025 - LAZAROSR - Fim de Alteração

          wa_tcurr_values-taxa_compra = wa_dados_conten-taxa.
          wa_tcurr_values-taxa_venda  = wa_dados_conten-taxa.
*
          MESSAGE |Encontrada taxa para data { wa_tcurr_values-lc_data_up } moeda { wa_tcurr_values-moeda } valor { wa_tcurr_values-taxa_compra } | TYPE 'S'.
          APPEND wa_tcurr_values TO it_tcurr_values.
          CLEAR: wa_tcurr_values.
        ENDLOOP.
      ELSE.
        /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = ws_dados_aux ).

        CONCATENATE ws_dados_aux-content-data(4) ws_dados_aux-content-data+5(2) ws_dados_aux-content-data+8(2) INTO wa_tcurr_values-lc_data_up.
*        wa_tcurr_values-lc_data_up = wa_tcurr_values-lc_data_up - 1.
        wa_tcurr_values-lc_data_up  = wa_tcurr_values-lc_data_up.
        wa_tcurr_values-moeda       = wa_param-moeda.

*** Stefanini - IR247099 - 22/08/2025 - LAZAROSR - Início de Alteração
        REPLACE ALL OCCURRENCES OF ',' IN wa_dados_conten-taxa WITH '.'.
*** Stefanini - IR247099 - 22/08/2025 - LAZAROSR - Fim de Alteração

        wa_tcurr_values-taxa_compra = ws_dados_aux-content-taxa.
        wa_tcurr_values-taxa_venda  = ws_dados_aux-content-taxa.
*
        MESSAGE |Encontrada taxa para data { wa_tcurr_values-lc_data_up } moeda { wa_tcurr_values-moeda } valor { wa_tcurr_values-taxa_compra } | TYPE 'S'.
        APPEND wa_tcurr_values TO it_tcurr_values.
        CLEAR: wa_tcurr_values.
      ENDIF.
    ENDIF.
*
    CLEAR: wa_param, e_url.
    CLEAR ws_dados.

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
FORM z_trata_dados.

  REFRESH: it_retorno, it_tcurr, r_data_up.

  CLEAR: lc_hora_up, lc_data_up, e_wotnr.


  IF it_tcurr_values IS NOT INITIAL.

    LOOP AT it_tcurr_values INTO wa_tcurr_values.

      lc_hora_up = wa_tcurr_values-lc_hora_up.

      total = ( ( wa_tcurr_values-taxa_venda + wa_tcurr_values-taxa_compra ) / 2 ).
*      WRITE total TO vtaxam.
*      WRITE wa_tcurr_values-taxa_compra TO taxa_c .
*      WRITE wa_tcurr_values-taxa_venda  TO taxa_v.
      vtaxam = total.
      taxa_c = wa_tcurr_values-taxa_compra.
      taxa_v = wa_tcurr_values-taxa_venda.

*** Stefanini - IR247099 - 22/08/2025 - LAZAROSR - Início de Alteração
      REPLACE ALL OCCURRENCES OF '.' IN vtaxam WITH ','.
      REPLACE ALL OCCURRENCES OF '.' IN taxa_c WITH ','.
      REPLACE ALL OCCURRENCES OF '.' IN taxa_v WITH ','.
*** Stefanini - IR247099 - 22/08/2025 - LAZAROSR - Fim de Alteração

      CONDENSE: vtaxam, taxa_c, taxa_v.

      PERFORM z_preenche_tcurr USING 'BCRA' wa_tcurr_values-moeda wa_tcurr_values-lc_data_up vtaxam.
*
      r_data_up-sign    = 'I'.
      r_data_up-option  = 'EQ'.
      r_data_up-low     = wa_tcurr_values-lc_data_up.
      APPEND  r_data_up.


      "Verifica se é feriado
      lc_data_f = wa_tcurr_values-lc_data_up.

      holiday_calendar = 'MG'.
      factory_calendar = 'ZT'.
      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          holiday_calendar = holiday_calendar
          factory_calendar = factory_calendar
          date_from        = lc_data_f
          date_to          = lc_data_f
        TABLES
          holidays         = it_datas.

      READ TABLE it_datas INTO DATA(wa_datas) WITH KEY freeday = 'X'.

      IF sy-subrc EQ 0 .
        PERFORM z_preenche_tcurr USING 'BCRA' wa_tcurr_values-moeda lc_data_f taxa_v.

        r_data_up-sign    = 'I'.
        r_data_up-option  = 'EQ'.
        r_data_up-low     = lc_data_f.
        APPEND  r_data_up.

        "Feriado com final de semana
        PERFORM z_atualiza_final_semana USING  lc_data_f.
      ENDIF.

      "Final de semana
      PERFORM z_atualiza_final_semana USING   wa_tcurr_values-lc_data_up.

      CLEAR:  taxa_c, taxa_v, vtaxam,  lc_data, lc_data1, lc_data2.
    ENDLOOP.

    CALL FUNCTION 'Z_FI_INBOUND_INDICE_FIN_ARG'
      TABLES
        it_tcurr   = it_tcurr
        it_retorno = it_retorno.

  ELSE.

    r_data_up-sign    = 'I'.
    r_data_up-option  = 'EQ'.
    r_data_up-low     = sy-datum.
    APPEND  r_data_up.
  ENDIF.

ENDFORM.

FORM z_verifica_cad_taxa.

  DATA: it_tcurr_ver TYPE TABLE OF tcurr,
        lc_data2     TYPE sy-datum,
        lc_datat     TYPE char08,
        linhas       TYPE i.

  RANGES r_gdatu FOR scurr-gdatu.
  RANGES r_kurst FOR tcurr-kurst.
  RANGES r_fcurr FOR tcurr-fcurr.
  RANGES r_tcurr FOR tcurr-tcurr.


  LOOP AT r_data_up.
    r_gdatu-sign   = 'I'.
    r_gdatu-option = 'EQ'.

    lc_data2 = r_data_up-low.
    CONCATENATE lc_data2+6(2) lc_data2+4(2)  lc_data2(4) INTO lc_datat.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lc_datat
      IMPORTING
        output = r_gdatu-low.

    APPEND r_gdatu.
    CLEAR: lc_data2, lc_datat.
  ENDLOOP.


  r_kurst-sign   = 'I'.
  r_kurst-option = 'EQ'.
  r_kurst-low    = 'BCRA'.
  APPEND r_kurst.

  r_fcurr-sign    = 'I'.
  r_fcurr-option  = 'EQ'.
  r_fcurr-low     = 'ARS'.
  APPEND  r_fcurr.

  r_tcurr-sign    = 'I'.
  r_tcurr-option  = 'EQ'.
  r_tcurr-low     = 'USD'.
  APPEND r_tcurr.

  r_fcurr-sign    = 'I'.
  r_fcurr-option  = 'EQ'.
  r_fcurr-low     = 'USD'.
  APPEND  r_fcurr.

  r_tcurr-sign    = 'I'.
  r_tcurr-option  = 'EQ'.
  r_tcurr-low     = 'ARS'.
  APPEND r_tcurr.


  SELECT * FROM tcurr INTO TABLE it_tcurr_ver
    WHERE gdatu IN r_gdatu
     AND  kurst IN r_kurst
     AND  fcurr IN r_fcurr
     AND  tcurr IN r_tcurr.

  DESCRIBE TABLE it_tcurr_ver LINES linhas.

  IF linhas >= 1.
    EXPORT v_true    FROM abap_true    TO MEMORY ID 'P_TRUE'.
    EXPORT v_hora    FROM lc_hora_up   TO MEMORY ID 'P_HORA'.
  ELSE.
    EXPORT v_false   FROM abap_true    TO MEMORY ID 'P_FALSE'.
  ENDIF.


  FREE MEMORY ID 'DATA_UP'.
  EXPORT r_data_up TO MEMORY ID 'DATA_UP'.

  SUBMIT zfir088 AND RETURN.

ENDFORM.


FORM z_preenche_tcurr USING e_tipo  e_tcurr  e_gdatu e_ukurs.

  DATA: w_tcurr TYPE zmme_tcurr.

  CASE e_tipo.
    WHEN 'BCRA'.
      w_tcurr-kurst = 'BCRA'.
      w_tcurr-fcurr = 'ARS'.
      w_tcurr-tcurr = 'USD'.
      w_tcurr-gdatu = e_gdatu.
      w_tcurr-ukurs = e_ukurs.
      APPEND w_tcurr TO it_tcurr.

      w_tcurr-fcurr = 'USD'.
      w_tcurr-tcurr = 'ARS'. "irá passar
      APPEND w_tcurr TO it_tcurr.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZA_FINAL_SEMANA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_TCURR_VALUES_LC_DATA_UP  text
*----------------------------------------------------------------------*
FORM z_atualiza_final_semana  USING lc_data.

  DATA: lc_data_f TYPE sy-datum,
        it_datas  TYPE TABLE OF  iscal_day.


  "Final de semana
  CALL FUNCTION 'DAY_IN_WEEK'
    EXPORTING
      "DATUM = WA_TCURR_VALUES-LC_DATA_UP
      datum = lc_data
    IMPORTING
      wotnr = e_wotnr.

  CASE e_wotnr. "Caso for sexta-Feira
    WHEN 5.
      "domingo
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = lc_data "WA_TCURR_VALUES-LC_DATA_UP
          days      = 1
          months    = 0
          years     = 0
        IMPORTING
          calc_date = lc_data1.

      PERFORM z_preenche_tcurr USING 'BCRA' wa_tcurr_values-moeda lc_data1 taxa_v.


      "segunda
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = lc_data1
          days      = 1
          months    = 0
          years     = 0
        IMPORTING
          calc_date = lc_data2.

      PERFORM z_preenche_tcurr USING 'BCRA'  wa_tcurr_values-moeda lc_data2 taxa_v.

      "Verifica se segunda é feriado, caso sim replicar cotação
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = lc_data2
          days      = 1
          months    = 0
          years     = 0
        IMPORTING
          calc_date = lc_data_f.

      holiday_calendar = 'MG'.
      factory_calendar = 'ZT'.

      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          holiday_calendar = holiday_calendar
          factory_calendar = factory_calendar
          date_from        = lc_data_f
          date_to          = lc_data_f
        TABLES
          holidays         = it_datas.

      READ TABLE it_datas INTO DATA(wa_datas) WITH KEY freeday = 'X'.

      IF sy-subrc EQ 0 .

        PERFORM z_preenche_tcurr USING 'BCRA'  wa_tcurr_values-moeda lc_data_f taxa_v.
*
        r_data_up-sign    = 'I'.
        r_data_up-option  = 'EQ'.
        r_data_up-low     = lc_data_f.
        APPEND  r_data_up.
      ENDIF.


      r_data_up-sign    = 'I'.
      r_data_up-option  = 'EQ'.
      r_data_up-low     = lc_data1.
      APPEND  r_data_up.
      r_data_up-low     = lc_data2.
      APPEND  r_data_up.
  ENDCASE.

ENDFORM.
