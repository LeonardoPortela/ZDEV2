*&---------------------------------------------------------------------*
*& Report  Z_LEITURA_XML_EZV_SUICA
*&
*&---------------------------------------------------------------------*
*&  Este programa lê o XML do Eidgenössische Zollverwaltung EZV e cadastra o indice
*&  do dolar (http://www.ezv.admin.ch/)
*&---------------------------------------------------------------------*

REPORT  z_leitura_xml_ezv_suica_2.

TYPES: BEGIN OF ty_string,
         str(25) TYPE c,
       END OF ty_string.
DATA it_string TYPE TABLE OF ty_string.
DATA wa_string TYPE ty_string .

DATA: e_resultado  TYPE string.

FIELD-SYMBOLS: <fs_tcurr> TYPE zmme_tcurr.

DATA: it_tcurr     TYPE TABLE OF zmme_tcurr WITH HEADER LINE,
      it_tcurr_aux TYPE TABLE OF zmme_tcurr WITH HEADER LINE,
      it_retorno   TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wa_tcurr     TYPE tcurr,

      ukurs_eur	   TYPE ukurs_curr,
      ukurs_brl	   TYPE ukurs_curr,
      ukurs_sgd    TYPE ukurs_curr,
      ukurs_usd    TYPE ukurs_curr,
      ukurs_gbp    TYPE ukurs_curr,
      ukurs_aux    TYPE i,
      ukurs        TYPE ukurs_curr,
      texto_ukurs  TYPE c LENGTH 9,
      qt_str1      TYPE c LENGTH 50,
      mod_str2     TYPE c LENGTH 50,

      fator_eur	   TYPE i,
      fator_brl	   TYPE i,
      fator_sgd	   TYPE i,
      fator_usd    TYPE i,
      fator_gbp    TYPE i,
      fator        TYPE i,
      wa_zfit0097  TYPE zfit0097,
      wa_zfit00972 TYPE zfit0097.

DATA: i_wotnr   TYPE p,
      lc_data   TYPE sy-datum,
      lc_dataf  TYPE sy-datum,
      lc_dataf2 TYPE sy-datum,
      lc_data1  TYPE sy-datum,
      lc_data2  TYPE sy-datum,
      lc_gdatu  TYPE gdatu_inv.

DATA: address_to   TYPE string,
      address_from TYPE string.

RANGES: pcad FOR  tcurr-gdatu.

DATA: if_xml           TYPE REF TO if_ixml,
      if_document      TYPE REF TO if_ixml_document,
      if_streamfactory TYPE REF TO if_ixml_stream_factory,
      if_stream        TYPE REF TO if_ixml_istream,
      if_xml_parser    TYPE REF TO if_ixml_parser,
      if_node          TYPE REF TO if_ixml_node,
      iterator         TYPE REF TO if_ixml_node_iterator,
      if_node_filho    TYPE REF TO if_ixml_node,
      if_node_list     TYPE REF TO if_ixml_node_list,
      iterator_list    TYPE REF TO if_ixml_node_iterator,
      tag_name         TYPE string,
      valor_dom        TYPE string,
      valor_line       TYPE string,
      prefix_dom       TYPE string,
      if_map           TYPE REF TO if_ixml_named_node_map,
      context          TYPE REF TO if_ixml_namespace_context,
      rval             TYPE string,
      data_xml         TYPE c LENGTH 10.

"DATA: IT_INDICES TYPE ZDE_DATA_EZV_COTACAO.
DATA: it_indices TYPE zde_data_ezv_cotacao_v2.

DATA: t_element_array TYPE zde_element_array_t.
CLEAR: ukurs_eur, ukurs_USD, ukurs_BRL, ukurs_gbp, ukurs_sgd.
CLEAR: fator_brl, fator_USD, fator_BRL, fator_gbp, fator_sgd.

CALL METHOD zcl_ler_url=>ler_url
  EXPORTING
    i_url                      = 'https://www.backend-rates.bazg.admin.ch/api/xmldaily' "'http://www.pwebapps.ezv.admin.ch/apps/rates/rate/getjson?activeSearchType=today'
  RECEIVING
    e_texto                    = e_resultado
  EXCEPTIONS
    argument_not_found         = 1
    plugin_not_active          = 2
    internal_error             = 3
    http_communication_failure = 4
    http_invalid_state         = 5
    http_processing_failed     = 6
    http_invalid_timeout       = 7
    outros                     = 8
    OTHERS                     = 9.

IF sy-subrc IS NOT INITIAL.
  IF sy-msgno IS NOT INITIAL AND sy-msgid IS INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  RETURN.
ENDIF.

"/UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = E_RESULTADO CHANGING DATA = IT_INDICES ).

APPEND 'devise' TO t_element_array.
DATA(_json) = zcl_string=>xml_to_json( i_xml           =  e_resultado
                                       i_element_array =  t_element_array ).

/ui2/cl_json=>deserialize( EXPORTING json = _json  CHANGING data = it_indices ).


LOOP AT it_indices-wechselkurse-devise INTO DATA(wa_indice).

  "Data
  "CONCATENATE WA_INDICE-RATEDATE(4) WA_INDICE-RATEDATE+5(2) WA_INDICE-RATEDATE+8(2) INTO WA_TCURR-GDATU.
  CONCATENATE it_indices-wechselkurse-datum+6(4) it_indices-wechselkurse-datum+3(2) it_indices-wechselkurse-datum(2) INTO wa_tcurr-gdatu.

  CASE zcl_string=>upper( wa_indice-a_code ).
    WHEN 'EUR'.

      "Cotação EUR
      TRY.
          MOVE wa_indice-kurs TO ukurs_eur.
        CATCH cx_sy_conversion_overflow.
          CONTINUE.
      ENDTRY.

      "Fator EUR
      SPLIT wa_indice-waehrung AT space INTO: qt_str1 mod_str2.
      MOVE qt_str1 TO fator_eur.

    WHEN 'BRL'.

      "Cotação BRL
      TRY.
          MOVE wa_indice-kurs TO ukurs_brl.
        CATCH cx_sy_conversion_overflow.
          CONTINUE.
      ENDTRY.

      "Fator BRL
      SPLIT wa_indice-waehrung AT space INTO: qt_str1 mod_str2.
      MOVE qt_str1 TO fator_brl.

    WHEN 'SGD'.

      "Cotação SGD
      TRY.
          MOVE wa_indice-kurs TO ukurs_sgd.
        CATCH cx_sy_conversion_overflow.
          CONTINUE.
      ENDTRY.

      "Fator BRL
      SPLIT wa_indice-waehrung AT space INTO: qt_str1 mod_str2.
      MOVE qt_str1 TO fator_sgd.

    WHEN 'USD'.

      "Cotação USD
      TRY.
          MOVE wa_indice-kurs TO ukurs_usd.
        CATCH cx_sy_conversion_overflow.
          CONTINUE.
      ENDTRY.

      "Fator USD
      SPLIT wa_indice-waehrung AT space INTO: qt_str1 mod_str2.
      MOVE qt_str1 TO fator_usd.

    WHEN 'GBP'.

      "Cotação GBP
      TRY.
          MOVE wa_indice-kurs TO ukurs_gbp.
        CATCH cx_sy_conversion_overflow.
          CONTINUE.
      ENDTRY.

      "Fator GBP
      SPLIT wa_indice-waehrung AT space INTO: qt_str1 mod_str2.
      MOVE qt_str1 TO fator_gbp.

  ENDCASE.

ENDLOOP.

fator = ( ukurs_eur / fator_eur ) * 100000 * fator_eur.
ukurs_eur = fator / ( 100000 * fator_eur ).

fator = ( ukurs_usd / fator_usd ) * 100000 * fator_usd.
ukurs_usd = fator / ( 100000 * fator_usd ).

fator = ( ukurs_gbp / fator_gbp ) * 100000 * fator_gbp.
ukurs_gbp = fator / ( 100000 * fator_gbp ).

fator = ( ukurs_brl / fator_brl ) * 100000 * fator_brl.
ukurs_brl = fator / ( 100000 * fator_brl ).

fator = ( ukurs_sgd / fator_sgd ) * 100000 * fator_sgd.
ukurs_sgd = fator / ( 100000 * fator_sgd ).

"Gran Bretagna """""""""""""""""""""""""""""""""""
IF ukurs_gbp NE 0.
  CLEAR: it_tcurr.
  MOVE: 'GBP'     TO it_tcurr-tcurr,
        'CHF'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_gbp TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.

  MOVE: 'CHF'     TO it_tcurr-tcurr,
        'GBP'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_gbp TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
ENDIF.

"Euro """""""""""""""""""""""""""""""""""""""""""
IF ukurs_eur NE 0.
  CLEAR: it_tcurr.
  MOVE: 'EUR'     TO it_tcurr-tcurr,
        'CHF'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_eur TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.

  MOVE: 'CHF'     TO it_tcurr-tcurr,
        'EUR'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_eur TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
ENDIF.

"Dólar """"""""""""""""""""""""""""""""""""""""""
IF ukurs_usd NE 0.
  CLEAR: it_tcurr.
  MOVE: 'USD'     TO it_tcurr-tcurr,
        'CHF'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_usd TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.

  MOVE: 'CHF'     TO it_tcurr-tcurr,
        'USD'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_usd TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.
ENDIF.

"Real """""""""""""""""""""""""""""""""""""""""""
IF ukurs_brl NE 0.
  CLEAR: it_tcurr.
  MOVE: 'BRL'     TO it_tcurr-tcurr,
        'CHF'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_brl TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.

  MOVE: 'CHF'     TO it_tcurr-tcurr,
        'BRL'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_brl TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.
ENDIF.

"Singapura => Franco suíço
IF ukurs_sgd NE 0.
  CLEAR: it_tcurr.
  MOVE: 'SGD'     TO it_tcurr-tcurr,
        'CHF'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_sgd TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.

  MOVE: 'CHF'     TO it_tcurr-tcurr,
        'SGD'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs_sgd TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.
ENDIF.

"Singapura => Dolar
IF ukurs_sgd NE 0 AND ukurs_usd NE 0.

  ukurs = ukurs_sgd / ukurs_usd.
*  ukurs_aux = ukurs * 10000.
*  ukurs     = ukurs_aux / 10000.

  CLEAR: it_tcurr.
  MOVE: 'SGD'     TO it_tcurr-tcurr,
        'USD'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.

  CLEAR: it_tcurr.
  MOVE: 'USD'     TO it_tcurr-tcurr,
        'SGD'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.
ENDIF.

"Singapura => Real
CLEAR: ukurs_aux, ukurs.
IF ukurs_sgd NE 0 AND ukurs_brl NE 0.

  ukurs = ukurs_sgd / ukurs_brl.
*  ukurs_aux = ukurs * 10000.
*  ukurs     = ukurs_aux / 10000.

  CLEAR: it_tcurr.
  MOVE: 'SGD'     TO it_tcurr-tcurr,
        'BRL'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.

  CLEAR: it_tcurr.
  MOVE: 'BRL'     TO it_tcurr-tcurr,
        'SGD'     TO it_tcurr-fcurr,
        'B'       TO it_tcurr-kurst,
        ukurs TO it_tcurr-ukurs,
        wa_tcurr-gdatu TO it_tcurr-gdatu.
  APPEND it_tcurr.
  MOVE 'G' TO it_tcurr-kurst.
  APPEND it_tcurr.
  MOVE 'M' TO it_tcurr-kurst.
  APPEND it_tcurr.
ENDIF.

CHECK it_tcurr[] IS NOT INITIAL.

"02.05.2016
MOVE wa_tcurr-gdatu TO lc_data.

CALL FUNCTION 'DAY_IN_WEEK'
  EXPORTING
    datum = lc_data
  IMPORTING
    wotnr = i_wotnr.

"Gera Indice para Sabado e Domingo
IF i_wotnr EQ 5.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = lc_data
      days      = 1
      months    = 0
      years     = 0
    IMPORTING
      calc_date = lc_data1.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = lc_data
      days      = 2
      months    = 0
      years     = 0
    IMPORTING
      calc_date = lc_data2.

  "Sabado
  LOOP AT it_tcurr.
    it_tcurr-gdatu = lc_data1.
    APPEND it_tcurr TO it_tcurr_aux.
  ENDLOOP.

  "Domingo
  LOOP AT it_tcurr.
    it_tcurr-gdatu = lc_data2.
    APPEND it_tcurr TO it_tcurr_aux.
  ENDLOOP.

  "Feriado
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = lc_data
      days      = 3
      months    = 0
      years     = 0
    IMPORTING
      calc_date = lc_dataf.

  wa_zfit0097-land1 = 'CH'.

  SELECT SINGLE * INTO wa_zfit0097
    FROM zfit0097
   WHERE land1    EQ wa_zfit0097-land1
     AND feriado  EQ lc_dataf.

  IF sy-subrc IS INITIAL.
    LOOP AT it_tcurr.
      it_tcurr-gdatu = lc_dataf.
      APPEND it_tcurr TO it_tcurr_aux.
    ENDLOOP.
  ELSE.
    CLEAR wa_zfit0097.
  ENDIF.

  LOOP AT it_tcurr_aux.
    APPEND it_tcurr_aux TO it_tcurr.
  ENDLOOP.

ELSE.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = lc_data
      days      = 1
      months    = 0
      years     = 0
    IMPORTING
      calc_date = lc_dataf.

  wa_zfit0097-land1 = 'CH'.

  SELECT SINGLE * INTO wa_zfit0097
    FROM zfit0097
   WHERE land1    EQ wa_zfit0097-land1
     AND feriado  EQ lc_dataf.

  IF sy-subrc IS INITIAL.
    LOOP AT it_tcurr.
      it_tcurr-gdatu = lc_dataf.
      APPEND it_tcurr TO it_tcurr_aux.
    ENDLOOP.

    CALL FUNCTION 'DAY_IN_WEEK'
      EXPORTING
        datum = lc_dataf
      IMPORTING
        wotnr = i_wotnr.

    "Gera Indice para Domingo e Segunda
    IF i_wotnr EQ 5.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = lc_dataf
          days      = 1
          months    = 0
          years     = 0
        IMPORTING
          calc_date = lc_data1.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = lc_dataf
          days      = 2
          months    = 0
          years     = 0
        IMPORTING
          calc_date = lc_data2.

      "Sabado
      LOOP AT it_tcurr.
        it_tcurr-gdatu = lc_data1.
        APPEND it_tcurr TO it_tcurr_aux.
      ENDLOOP.

      "Domingo
      LOOP AT it_tcurr.
        it_tcurr-gdatu = lc_data2.
        APPEND it_tcurr TO it_tcurr_aux.
      ENDLOOP.

      "Feriado
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = lc_dataf
          days      = 3
          months    = 0
          years     = 0
        IMPORTING
          calc_date = lc_dataf2.

      wa_zfit00972-land1 = 'CH'.

      SELECT SINGLE * INTO wa_zfit00972
        FROM zfit0097
       WHERE land1    EQ wa_zfit00972-land1
         AND feriado  EQ lc_dataf2.

      IF sy-subrc IS INITIAL.
        LOOP AT it_tcurr.
          it_tcurr-gdatu = lc_dataf2.
          APPEND it_tcurr TO it_tcurr_aux.
        ENDLOOP.
      ELSE.
        CLEAR wa_zfit00972.
      ENDIF.

    ENDIF.

    LOOP AT it_tcurr_aux.
      APPEND it_tcurr_aux TO it_tcurr.
    ENDLOOP.

  ELSE.
    CLEAR wa_zfit0097.
  ENDIF.

ENDIF.


CONCATENATE lc_data+6(2) lc_data+4(2) lc_data(4) INTO lc_data.

CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
  EXPORTING
    input  = lc_data
  IMPORTING
    output = lc_gdatu.

PERFORM verifica_falta USING lc_gdatu.

CHECK sy-subrc IS NOT INITIAL.

IF it_tcurr[] IS NOT INITIAL.

  CLEAR: it_tcurr_aux[].
  MOVE it_tcurr[] TO it_tcurr_aux[].

  LOOP AT it_tcurr_aux ASSIGNING <fs_tcurr>.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = <fs_tcurr>-gdatu
        days      = 1
        months    = 0
        years     = 0
        signum    = '-'
      IMPORTING
        calc_date = <fs_tcurr>-gdatu.

  ENDLOOP.

  CALL FUNCTION 'Z_FI_INBOUND_INDICE_FINANCEIRO'
    TABLES
      it_tcurr   = it_tcurr_aux
      it_retorno = it_retorno.

  PERFORM gerar_e_mail_cadastro USING lc_gdatu lc_data.

  IF i_wotnr EQ 5.
    CONCATENATE lc_data1+6(2) lc_data1+4(2) lc_data1(4) INTO lc_data.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lc_data
      IMPORTING
        output = lc_gdatu.
    PERFORM gerar_e_mail_cadastro USING lc_gdatu lc_data1.

    CONCATENATE lc_data2+6(2) lc_data2+4(2) lc_data2(4) INTO lc_data.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lc_data
      IMPORTING
        output = lc_gdatu.
    PERFORM gerar_e_mail_cadastro USING lc_gdatu lc_data2.
  ENDIF.

  IF wa_zfit0097 IS NOT INITIAL.
    CONCATENATE wa_zfit0097-feriado+6(2) wa_zfit0097-feriado+4(2) wa_zfit0097-feriado(4) INTO lc_data.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lc_data
      IMPORTING
        output = lc_gdatu.
    PERFORM gerar_e_mail_cadastro USING lc_gdatu wa_zfit0097-feriado.
  ENDIF.

  IF wa_zfit00972 IS NOT INITIAL.
    CONCATENATE wa_zfit00972-feriado+6(2) wa_zfit00972-feriado+4(2) wa_zfit00972-feriado(4) INTO lc_data.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lc_data
      IMPORTING
        output = lc_gdatu.
    PERFORM gerar_e_mail_cadastro USING lc_gdatu wa_zfit0097-feriado.
  ENDIF.

  CALL FUNCTION 'ZFI_VIP_EXCHANGE_RATES'.

ENDIF.

PERFORM verificar_nao_cadastrados.


*&---------------------------------------------------------------------*
*&      Form  GERAR_E_MAIL_CADASTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gerar_e_mail_cadastro  USING  p_gdatu TYPE gdatu_inv p_data TYPE sy-datum .

  DATA: lo_create_mail      TYPE REF TO cl_crm_email_data,
        lt_to               TYPE crmt_email_recipients,
        lt_copy             TYPE crmt_email_recipients,
        ls_recep            TYPE crms_email_recipient,
        lt_mail_body        TYPE crmt_email_mime_struc,
        ls_mail_body        TYPE crms_email_mime_struc,
        lv_activity         TYPE sysuuid_x,
        p_data_aux          TYPE char10,
        str_aux             TYPE char12,
        valor               TYPE f,
        it_datas            TYPE TABLE OF iscal_day,
        p_data_final_semana TYPE datum.

  DATA: i_html_entra TYPE string,
        i_html_saida TYPE string.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
    EXPORTING
      input  = p_gdatu
    IMPORTING
      output = p_data_aux.

  CONCATENATE p_data_aux+6(4) p_data_aux+3(2) p_data_aux(2) INTO p_data_final_semana.

  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      holiday_calendar = space
      factory_calendar = 'ZT'
      date_from        = p_data_final_semana
      date_to          = p_data_final_semana
    TABLES
      holidays         = it_datas
    EXCEPTIONS
      OTHERS           = 1.

  DESCRIBE TABLE it_datas LINES DATA(qt_linhas).

  CHECK qt_linhas EQ 0.

  pcad-sign   = 'I'.
  pcad-option = 'EQ'.
  pcad-low     = p_gdatu.
  pcad-high    = p_gdatu.
  APPEND pcad.

  DATA: it_tcurr_mail TYPE TABLE OF tcurr WITH HEADER LINE.

  RANGES: rg_kurst FOR tcurr-kurst.
  RANGES: rg_fcurr FOR tcurr-fcurr.
  RANGES: rg_tcurr FOR tcurr-tcurr.

  "Cotação
  rg_kurst-sign    = 'I'.
  rg_kurst-option  = 'EQ'.
  rg_kurst-low     = 'M'.
  rg_kurst-high    = 'M'.
  APPEND rg_kurst.
  rg_kurst-low     = 'G'.
  rg_kurst-high    = 'G'.
  APPEND rg_kurst.
  rg_kurst-low     = 'B'.
  rg_kurst-high    = 'B'.
  APPEND rg_kurst.

  "Moeda de procedência
  rg_fcurr-sign    = 'I'.
  rg_fcurr-option  = 'EQ'.
  rg_fcurr-low     = 'EUR'.
  rg_fcurr-high    = 'EUR'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'USD'.
  rg_fcurr-high    = 'USD'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'GBP'.
  rg_fcurr-high    = 'GBP'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'BRL'.
  rg_fcurr-high    = 'BRL'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'SGD'.
  rg_fcurr-high    = 'SGD'.
  APPEND rg_fcurr.

  "Moeda de destino
  rg_tcurr-sign    = 'I'.
  rg_tcurr-option  = 'EQ'.
  rg_tcurr-low     = 'CHF'.
  rg_tcurr-high    = 'CHF'.
  APPEND rg_tcurr.

  "Moeda de destino
  rg_tcurr-sign    = 'I'.
  rg_tcurr-option  = 'EQ'.
  rg_tcurr-low     = 'SGD'.
  rg_tcurr-high    = 'SGD'.
  APPEND rg_tcurr.

  CLEAR: it_tcurr_mail[].

  SELECT * INTO TABLE it_tcurr_mail
    FROM tcurr
   WHERE gdatu EQ p_gdatu
     AND kurst IN rg_kurst
     AND fcurr IN rg_fcurr
     AND tcurr IN rg_tcurr.

  SELECT * APPENDING TABLE it_tcurr_mail
    FROM tcurr
   WHERE gdatu EQ p_gdatu
     AND kurst IN rg_kurst
     AND tcurr IN rg_fcurr
     AND fcurr IN rg_tcurr.


*  SORT it_tcurr_mail[] BY gdatu kurst fcurr tcurr.


  CLEAR: i_html_entra.
  CONCATENATE i_html_entra '<html>'      INTO i_html_entra.
  CONCATENATE i_html_entra '<head>'      INTO i_html_entra.
  CONCATENATE i_html_entra '</head>'     INTO i_html_entra.
  CONCATENATE i_html_entra '<body>'      INTO i_html_entra.
  i_html_saida = sy-host.
  TRANSLATE i_html_saida TO UPPER CASE.
  CONCATENATE i_html_entra '<DIV align=center><FONT face=Verdana size=4>' i_html_saida '</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=center><FONT face=Verdana size=3>Exchange Reference Rates</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=center><a href="https://www.ezv.admin.ch/ezv/fr/home.html">Administration fédérale des douanes AFD</a></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.

  CONCATENATE i_html_entra '<TABLE border=1 align=center>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1><STRONG>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>Date</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>Exchange Rate Type</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>From currency</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>To currency</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>Exchange Rate</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '</TR>' INTO i_html_entra.

  LOOP AT it_tcurr_mail.
    CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1>' INTO i_html_entra.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
      EXPORTING
        input  = it_tcurr_mail-gdatu
      IMPORTING
        output = p_data_aux.
    "Date
    CONCATENATE i_html_entra '<TD align=center>' p_data_aux '</TD>' INTO i_html_entra.
    "Exchange Rate Type
    CONCATENATE i_html_entra '<TD align=center>' it_tcurr_mail-kurst '</TD>' INTO i_html_entra.
    "From currency
    CONCATENATE i_html_entra '<TD align=center>' it_tcurr_mail-fcurr '</TD>' INTO i_html_entra.
    "To currency
    CONCATENATE i_html_entra '<TD align=center>' it_tcurr_mail-tcurr '</TD>' INTO i_html_entra.
    "Exchange Rate
    CALL FUNCTION 'CONVERSION_EXIT_EXCRT_OUTPUT'
      EXPORTING
        input  = it_tcurr_mail-ukurs
      IMPORTING
        output = str_aux.

    CONCATENATE i_html_entra '<TD align=right>' str_aux        '</TD>' INTO i_html_entra.
    CONCATENATE i_html_entra '</FONT></TR>' INTO i_html_entra.
  ENDLOOP.

  CONCATENATE i_html_entra '</TABLE>' INTO i_html_entra.

  CONCATENATE i_html_entra '</body>'  INTO i_html_entra.
  CONCATENATE i_html_entra '</html>'  INTO i_html_entra.

  MOVE i_html_entra TO i_html_saida.

  CREATE OBJECT lo_create_mail.

  "moving the subject.
  CONCATENATE 'Reference rates' p_data_aux INTO lo_create_mail->subject SEPARATED BY space.

  CLEAR ls_mail_body.
  ls_mail_body-content_ascii = i_html_saida.
  ls_mail_body-mime_type     = 'text/html'.
  APPEND  ls_mail_body TO lt_mail_body.

  "moving the body
  MOVE lt_mail_body TO lo_create_mail->body.



  IF sy-sysid EQ 'PRD'.

    "ADDRESS_TO = 'fernanda.rodrigues@amaggi.ch'.
    address_to = 'accounting@amaggi.ch'.
    "ADDRESS_FROM = 'suporte.sap@amaggi.com.br'.

    "LS_RECEP-ADDRESS = 'jacqueline.coelho@amaggi.com.br'.
    "APPEND LS_RECEP TO LT_COPY.
    "LS_RECEP-ADDRESS = 'claudia.cardoso@amaggi.ch'.
    "APPEND LS_RECEP TO LT_COPY.
    ls_recep-address = 'suporte.sap@amaggi.com.br'.
    APPEND ls_recep TO lt_copy.
    "LS_RECEP-ADDRESS = 'larissa.hapanchuk@amaggi.com.br'.
    ls_recep-address = 'controladoria.europa@amaggi.com.br'.
    APPEND ls_recep TO lt_copy.
    MOVE lt_copy TO lo_create_mail->copy.

  ELSE.
    address_to   = 'accounting@amaggi.ch'.
    ls_recep-address = 'kelly.martins@amaggi.com.br'.
    APPEND ls_recep TO lt_copy.
    ls_recep-address = 'anderson.oenning@amaggi.com.br'.
    APPEND ls_recep TO lt_copy.
    ls_recep-address = 'leila.mara@amaggi.com.br'.
    APPEND ls_recep TO lt_copy.
    MOVE lt_copy TO lo_create_mail->copy.
  ENDIF.

  "filling to email address
  CLEAR ls_recep.
  ls_recep-address = address_to.
  APPEND ls_recep TO lt_to.
  MOVE lt_to TO lo_create_mail->to.

  CLEAR ls_recep.
  ls_recep-address = address_from.
  MOVE ls_recep TO lo_create_mail->from.

  "send the email.
  "calling method to send the email
  CALL METHOD cl_crm_email_utility_base=>send_email
    EXPORTING
      iv_mail_data       = lo_create_mail
    RECEIVING
      ev_send_request_id = lv_activity.

  COMMIT WORK.

ENDFORM.                    " GERAR_E_MAIL_CADASTRO

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_NAO_CADASTRADOS
*&---------------------------------------------------------------------*
*       Verifica todas a matriz de indice a ser cadastrada se realmente
*       foi cadastrada
*----------------------------------------------------------------------*
FORM verificar_nao_cadastrados .

  DATA: lo_create_mail TYPE REF TO cl_crm_email_data,
        lt_to          TYPE crmt_email_recipients,
        lt_copy        TYPE crmt_email_recipients,
        ls_recep       TYPE crms_email_recipient,
        lt_mail_body   TYPE crmt_email_mime_struc,
        ls_mail_body   TYPE crms_email_mime_struc,
        lv_activity    TYPE sysuuid_x,
        p_data_aux     TYPE char10,
        str_aux        TYPE char12,
        valor          TYPE f.

  DATA: i_html_entra TYPE string,
        i_html_saida TYPE string,
        lc_message   TYPE bapiret2-message.

  DATA: it_tcurr_cad TYPE TABLE OF tcurr WITH HEADER LINE,
        it_tcurr_err TYPE TABLE OF tcurr WITH HEADER LINE.

  RANGES: rg_kurst FOR tcurr-kurst.
  RANGES: rg_fcurr FOR tcurr-fcurr.
  RANGES: rg_tcurr FOR tcurr-tcurr.

  "Cotação
  rg_kurst-sign    = 'I'.
  rg_kurst-option  = 'EQ'.
  rg_kurst-low     = 'M'.
  rg_kurst-high    = 'M'.
  APPEND rg_kurst.
  rg_kurst-low     = 'G'.
  rg_kurst-high    = 'G'.
  APPEND rg_kurst.
  rg_kurst-low     = 'B'.
  rg_kurst-high    = 'B'.
  APPEND rg_kurst.

  "Moeda de procedência
  rg_fcurr-sign    = 'I'.
  rg_fcurr-option  = 'EQ'.
  rg_fcurr-low     = 'USD'.
  rg_fcurr-high    = 'USD'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'BRL'.
  rg_fcurr-high    = 'BRL'.
  APPEND rg_fcurr.


  "Moeda de destino
  rg_tcurr-sign    = 'I'.
  rg_tcurr-option  = 'EQ'.
  rg_tcurr-low     = 'CHF'.
  rg_tcurr-high    = 'CHF'.
  APPEND rg_fcurr.

  "Moeda de destino Sigapura.
  rg_tcurr-sign    = 'I'.
  rg_tcurr-option  = 'EQ'.
  rg_tcurr-low     = 'SGD'.
  rg_tcurr-high    = 'SGD'.
  APPEND rg_fcurr.


  "Moeda de procedência
  rg_fcurr-sign    = 'I'.
  rg_fcurr-option  = 'EQ'.
  rg_fcurr-low     = 'USD'.
  rg_fcurr-high    = 'USD'.
  APPEND rg_tcurr.
  rg_fcurr-low     = 'BRL'.
  rg_fcurr-high    = 'BRL'.
  APPEND rg_tcurr.
  rg_fcurr-low     = 'SGD'.
  rg_fcurr-high    = 'SGD'.
  APPEND rg_tcurr.

  "Moeda de destino
  rg_tcurr-sign    = 'I'.
  rg_tcurr-option  = 'EQ'.
  rg_tcurr-low     = 'CHF'.
  rg_tcurr-high    = 'CHF'.
  APPEND rg_tcurr.


  SELECT * INTO TABLE it_tcurr_cad
    FROM tcurr
   WHERE gdatu IN pcad
     AND kurst IN rg_kurst
     AND fcurr IN rg_fcurr
     AND tcurr IN rg_tcurr.

  SELECT * APPENDING TABLE it_tcurr_cad
    FROM tcurr
   WHERE gdatu IN pcad
     AND kurst IN rg_kurst
     AND fcurr IN rg_tcurr
     AND tcurr IN rg_fcurr.

  SORT it_tcurr_cad BY gdatu kurst fcurr tcurr.

  "Dias
  LOOP AT pcad.
    "Cotacao
    LOOP AT rg_kurst.
      "Moeda Origem
      LOOP AT rg_fcurr.
        "Moeda Destino
        LOOP AT rg_tcurr.

          READ TABLE it_tcurr_cad WITH KEY gdatu = pcad-low
                                           kurst = rg_kurst-low
                                           fcurr = rg_fcurr-low
                                           tcurr = rg_tcurr-low BINARY SEARCH.
          IF ( sy-subrc IS NOT INITIAL ) AND ( rg_fcurr-low NE rg_tcurr-low ).
            it_tcurr_err-gdatu = pcad-low.
            it_tcurr_err-kurst = rg_kurst-low.
            it_tcurr_err-fcurr = rg_fcurr-low.
            it_tcurr_err-tcurr = rg_tcurr-low.
            APPEND it_tcurr_err.
          ENDIF.

          READ TABLE it_tcurr_cad WITH KEY gdatu = pcad-low
                                           kurst = rg_kurst-low
                                           fcurr = rg_tcurr-low
                                           tcurr = rg_fcurr-low BINARY SEARCH.
          IF ( sy-subrc IS NOT INITIAL ) AND ( rg_fcurr-low NE rg_tcurr-low ).
            it_tcurr_err-gdatu = pcad-low.
            it_tcurr_err-kurst = rg_kurst-low.
            it_tcurr_err-fcurr = rg_tcurr-low.
            it_tcurr_err-tcurr = rg_fcurr-low.
            APPEND it_tcurr_err.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  CLEAR: rg_kurst[].
  "Cotação
  rg_kurst-sign    = 'I'.
  rg_kurst-option  = 'EQ'.
  rg_kurst-low     = 'B'.
  rg_kurst-high    = 'B'.
  APPEND rg_kurst.

  CLEAR: rg_fcurr[].
  "Moeda de procedência
  rg_fcurr-sign    = 'I'.
  rg_fcurr-option  = 'EQ'.
  rg_fcurr-low     = 'EUR'.
  rg_fcurr-high    = 'EUR'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'GBP'.
  rg_fcurr-high    = 'GBP'.
  APPEND rg_fcurr.

  CLEAR: rg_tcurr[].
  "Moeda de destino
  rg_tcurr-sign    = 'I'.
  rg_tcurr-option  = 'EQ'.
  rg_tcurr-low     = 'CHF'.
  rg_tcurr-high    = 'CHF'.
  APPEND rg_tcurr.

  CLEAR: it_tcurr_cad[].

  SELECT * INTO TABLE it_tcurr_cad
    FROM tcurr
   WHERE gdatu IN pcad
     AND kurst IN rg_kurst
     AND fcurr IN rg_fcurr
     AND tcurr IN rg_tcurr.

  SELECT * APPENDING TABLE it_tcurr_cad
    FROM tcurr
   WHERE gdatu IN pcad
     AND kurst IN rg_kurst
     AND fcurr IN rg_tcurr
     AND tcurr IN rg_fcurr.

  SORT it_tcurr_cad BY gdatu kurst fcurr tcurr.

  "Dias
  LOOP AT pcad.
    "Cotacao
    LOOP AT rg_kurst.
      "Moeda Origem
      LOOP AT rg_fcurr.
        "Moeda Destino
        LOOP AT rg_tcurr.

          READ TABLE it_tcurr_cad WITH KEY gdatu = pcad-low
                                           kurst = rg_kurst-low
                                           fcurr = rg_fcurr-low
                                           tcurr = rg_tcurr-low.
          IF ( sy-subrc IS NOT INITIAL ) AND ( rg_fcurr-low NE rg_tcurr-low ).
            it_tcurr_err-gdatu = pcad-low.
            it_tcurr_err-kurst = rg_kurst-low.
            it_tcurr_err-fcurr = rg_fcurr-low.
            it_tcurr_err-tcurr = rg_tcurr-low.
            APPEND it_tcurr_err.
          ENDIF.

          READ TABLE it_tcurr_cad WITH KEY gdatu = pcad-low
                                           kurst = rg_kurst-low
                                           fcurr = rg_tcurr-low
                                           tcurr = rg_fcurr-low.
          IF ( sy-subrc IS NOT INITIAL ) AND ( rg_fcurr-low NE rg_tcurr-low ).
            it_tcurr_err-gdatu = pcad-low.
            it_tcurr_err-kurst = rg_kurst-low.
            it_tcurr_err-fcurr = rg_tcurr-low.
            it_tcurr_err-tcurr = rg_fcurr-low.
            APPEND it_tcurr_err.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  IF it_tcurr_err[] IS NOT INITIAL.
    CLEAR: i_html_entra.

    CONCATENATE i_html_entra '<html>'      INTO i_html_entra.
    CONCATENATE i_html_entra '<head>'      INTO i_html_entra.
    CONCATENATE i_html_entra '</head>'     INTO i_html_entra.
    CONCATENATE i_html_entra '<body>'      INTO i_html_entra.

    i_html_saida = sy-host.
    TRANSLATE i_html_saida TO UPPER CASE.
    CONCATENATE i_html_entra '<DIV align=center><FONT face=Verdana size=4>' i_html_saida '</FONT></DIV>' INTO i_html_entra.
    CONCATENATE i_html_entra '<DIV align=center><FONT face=Verdana size=3>Exchange Reference Rates</FONT></DIV>' INTO i_html_entra.
    CONCATENATE i_html_entra '<DIV align=center><FONT face=Verdana size=3 color=#FF0000>ERROR</FONT></DIV>' INTO i_html_entra.
    CONCATENATE i_html_entra '<DIV align=center><a href="http://www.ezv.admin.ch/zollinfo_firmen/04203/04304/index.html?lang=fr">Administration fédérale des douanes AFD</a></DIV>' INTO i_html_entra.
    CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.

    CONCATENATE i_html_entra '<TABLE border=1 align=center>' INTO i_html_entra.
    CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1><STRONG>' INTO i_html_entra.
    CONCATENATE i_html_entra '<TD align=center>Date</TD>' INTO i_html_entra.
    CONCATENATE i_html_entra '<TD align=center>Exchange Rate Type</TD>' INTO i_html_entra.
    CONCATENATE i_html_entra '<TD align=center>From currency</TD>' INTO i_html_entra.
    CONCATENATE i_html_entra '<TD align=center>To currency</TD>' INTO i_html_entra.
    CONCATENATE i_html_entra '</TR>' INTO i_html_entra.

    LOOP AT it_tcurr_err.
      CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1>' INTO i_html_entra.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = it_tcurr_err-gdatu
        IMPORTING
          output = p_data_aux.
      "Date
      CONCATENATE i_html_entra '<TD align=center>' p_data_aux '</TD>' INTO i_html_entra.
      "Exchange Rate Type
      CONCATENATE i_html_entra '<TD align=center>' it_tcurr_err-kurst '</TD>' INTO i_html_entra.
      "From currency
      CONCATENATE i_html_entra '<TD align=center>' it_tcurr_err-fcurr '</TD>' INTO i_html_entra.
      "To currency
      CONCATENATE i_html_entra '<TD align=center>' it_tcurr_err-tcurr '</TD>' INTO i_html_entra.

      CONCATENATE i_html_entra '</FONT></TR>' INTO i_html_entra.
    ENDLOOP.

    CONCATENATE i_html_entra '</TABLE>' INTO i_html_entra.
    CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
    CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
    "Montagem de erros do SHDB
    IF it_retorno[] IS NOT INITIAL.
      CONCATENATE i_html_entra '<TABLE border=1 align=center>' INTO i_html_entra.
      CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1><STRONG>' INTO i_html_entra.
      CONCATENATE i_html_entra '<TD align=center>Message Error</TD>' INTO i_html_entra.
      CONCATENATE i_html_entra '</TR>' INTO i_html_entra.

      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING
          input  = 'EN'
        IMPORTING
          output = sy-langu.

      DELETE it_retorno WHERE msgtyp EQ 'S'.
      DELETE it_retorno WHERE msgtyp EQ 'E' AND msgid EQ 'SV' AND msgnr EQ '009'.

      SORT it_retorno BY msgid msgtyp msgnr.
      DELETE ADJACENT DUPLICATES FROM it_retorno COMPARING msgid msgtyp msgnr.

      IF it_retorno[] IS INITIAL.
        CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center><font face="Verdana" color="#FF0000">There is no error message</font></TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '</FONT></TR>' INTO i_html_entra.
      ENDIF.

      LOOP AT it_retorno.
        CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1>' INTO i_html_entra.
        MOVE: it_retorno-msgid  TO sy-msgid,
              it_retorno-msgtyp TO sy-msgty,
              it_retorno-msgnr  TO sy-msgno,
              it_retorno-msgv1  TO sy-msgv1,
              it_retorno-msgv2  TO sy-msgv2,
              it_retorno-msgv3  TO sy-msgv3,
              it_retorno-msgv4  TO sy-msgv4.

        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            language = sy-langu
            msg_id   = sy-msgid
            msg_no   = it_retorno-msgnr
            msg_var1 = sy-msgv1
            msg_var2 = sy-msgv2
            msg_var3 = sy-msgv3
            msg_var4 = sy-msgv4
          IMPORTING
            msg_text = lc_message.

        CONCATENATE i_html_entra '<TD align=center>' lc_message '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '</FONT></TR>' INTO i_html_entra.
      ENDLOOP.
      CONCATENATE i_html_entra '</TABLE>' INTO i_html_entra.
    ENDIF.

    CONCATENATE i_html_entra '</body>'     INTO i_html_entra.
    CONCATENATE i_html_entra '</html>'     INTO i_html_entra.

    MOVE i_html_entra TO i_html_saida.

    CREATE OBJECT lo_create_mail.

    CLEAR: lo_create_mail->subject.

    "moving the subject.
    LOOP AT pcad.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = pcad-low
        IMPORTING
          output = p_data_aux.

      IF lo_create_mail->subject IS INITIAL.
        CONCATENATE 'Reference rates error:' p_data_aux INTO lo_create_mail->subject SEPARATED BY space.
      ELSE.
        CONCATENATE lo_create_mail->subject p_data_aux INTO lo_create_mail->subject SEPARATED BY space.
      ENDIF.

    ENDLOOP.

    CLEAR ls_mail_body.
    ls_mail_body-content_ascii = i_html_saida.
    ls_mail_body-mime_type     = 'text/html'.
    APPEND  ls_mail_body TO lt_mail_body.

    "moving the body
    MOVE lt_mail_body TO lo_create_mail->body.

    IF sy-sysid EQ 'PRD'.

      "ADDRESS_TO = 'fernanda.rodrigues@amaggi.ch'.
      address_to = 'accounting@amaggi.ch'.
      address_from = 'suporte.sap@amaggi.com.br'.

      "filling to email address - copia
      "LS_RECEP-ADDRESS = 'jacqueline.coelho@amaggi.com.br'.
      "APPEND LS_RECEP TO LT_COPY.
      "LS_RECEP-ADDRESS = 'claudia.cardoso@amaggi.ch'.
      "APPEND LS_RECEP TO LT_COPY.
      ls_recep-address = 'suporte.sap@amaggi.com.br'.
      APPEND ls_recep TO lt_copy.
      "LS_RECEP-ADDRESS = 'larissa.hapanchuk@amaggi.com.br'.
      ls_recep-address = 'controladoria.europa@amaggi.com.br'.
      APPEND ls_recep TO lt_copy.
      MOVE lt_copy TO lo_create_mail->copy.

    ELSE.
      address_to   = 'accounting@amaggi.ch'.
      ls_recep-address = 'kelly.martins@amaggi.com.br'.
      APPEND ls_recep TO lt_copy.
      ls_recep-address = 'anderson.oenning@amaggi.com.br'.
      APPEND ls_recep TO lt_copy.
      ls_recep-address = 'leila.mara@amaggi.com.br'.
      APPEND ls_recep TO lt_copy.
      MOVE lt_copy TO lo_create_mail->copy.

    ENDIF.

    "filling to email address
    CLEAR ls_recep.
    ls_recep-address = address_to.
    APPEND ls_recep TO lt_to.
    MOVE lt_to TO lo_create_mail->to.

    CLEAR ls_recep.
    ls_recep-address = address_from.
    MOVE ls_recep TO lo_create_mail->from.

    "send the email.
    "calling method to send the email
    CALL METHOD cl_crm_email_utility_base=>send_email
      EXPORTING
        iv_mail_data       = lo_create_mail
      RECEIVING
        ev_send_request_id = lv_activity.

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
FORM verifica_falta  USING  p_gdatu TYPE gdatu_inv.

  DATA: it_tcurr_ver TYPE TABLE OF tcurr,
        linhas       TYPE i.

  RANGES: rg_kurst FOR tcurr-kurst.
  RANGES: rg_fcurr FOR tcurr-fcurr.
  RANGES: rg_tcurr FOR tcurr-tcurr.

  "Cotação
  rg_kurst-sign    = 'I'.
  rg_kurst-option  = 'EQ'.
  rg_kurst-low     = 'M'.
  rg_kurst-high    = 'M'.
  APPEND rg_kurst.
  rg_kurst-low     = 'G'.
  rg_kurst-high    = 'G'.
  APPEND rg_kurst.
  rg_kurst-low     = 'B'.
  rg_kurst-high    = 'B'.
  APPEND rg_kurst.

  "Moeda de procedência
  rg_fcurr-sign    = 'I'.
  rg_fcurr-option  = 'EQ'.
  rg_fcurr-low     = 'EUR'.
  rg_fcurr-high    = 'EUR'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'USD'.
  rg_fcurr-high    = 'USD'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'GBP'.
  rg_fcurr-high    = 'GBP'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'BRL'.
  rg_fcurr-high    = 'BRL'.
  APPEND rg_fcurr.
  rg_fcurr-low     = 'SGD'.
  rg_fcurr-high    = 'SGD'.
  APPEND rg_fcurr.

  "Moeda de destino
  rg_tcurr-sign    = 'I'.
  rg_tcurr-option  = 'EQ'.
  rg_tcurr-low     = 'CHF'.
  rg_tcurr-high    = 'CHF'.
  APPEND rg_tcurr.

  SELECT * INTO TABLE it_tcurr_ver
    FROM tcurr
   WHERE gdatu EQ p_gdatu
     AND kurst IN rg_kurst
     AND fcurr IN rg_fcurr
     AND tcurr IN rg_tcurr.

  SELECT * APPENDING TABLE it_tcurr_ver
    FROM tcurr
   WHERE gdatu EQ p_gdatu
     AND kurst IN rg_kurst
     AND fcurr IN rg_tcurr
     AND tcurr IN rg_fcurr.

  DESCRIBE TABLE it_tcurr_ver LINES linhas.

  IF linhas EQ 22.
    sy-subrc = 0.
  ELSE.
    sy-subrc = 4.
  ENDIF.

ENDFORM.                    " VERIFICA_FALTA
