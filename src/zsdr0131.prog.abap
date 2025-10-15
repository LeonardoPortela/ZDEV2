*&---------------------------------------------------------------------*
*& Report  ZSDR0131                                                    *&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : JOB Buscar dados de Propostas no sistema Opus Crédito   *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT zsdr0131.

*******************************************************************************************
* Tabelas
*******************************************************************************************
TABLES: zsdt0272,
        zsdt0273,
        zsdt0274,
        zsdt0275.

*******************************************************************************************
* Types
*******************************************************************************************
TYPES: BEGIN OF ty_emitente,
         cpfcnpj TYPE string.
TYPES: END   OF ty_emitente.

TYPES: BEGIN OF ty_safracultura,
         safra   TYPE string,
         cultura TYPE string.
TYPES: END   OF ty_safracultura.

TYPES: BEGIN OF ty_documentosimulacao,
         documento TYPE string.
TYPES: END   OF ty_documentosimulacao.

DATA: t_emitente           TYPE TABLE OF ty_emitente,
      t_safracultura       TYPE TABLE OF ty_safracultura,
      t_documentosimulacao TYPE TABLE OF ty_documentosimulacao.

TYPES: BEGIN OF ty_xml_limite,
         numeroproposta    TYPE string,
         datacriacao       TYPE string,
         emitente          LIKE t_emitente,
         estagio           TYPE string,
         safracultura      LIKE t_safracultura,
         dataultimaatuacao TYPE string.
TYPES: END   OF ty_xml_limite.

TYPES: BEGIN OF ty_xml_operacao,
         numeroproposta     TYPE string,
         propostalimite     TYPE string,
         estagio            TYPE string,
         datacriacao        TYPE string,
         safra              TYPE string,
         cultura            TYPE string,
         emitente           LIKE t_emitente,
         efetivada          TYPE string,
         documentosimulacao LIKE t_documentosimulacao,
         dataultimaatuacao  TYPE string.
TYPES: END   OF ty_xml_operacao.

*******************************************************************************************
* Variaveis
*******************************************************************************************
DATA: w_zauth_webservice TYPE zauth_webservice,
      l_url              TYPE ui_src_url,
      l_xml              TYPE string,
      l_xvalor           TYPE string,
      l_dtaux            TYPE string,
      l_dt_ini           TYPE char8,
      l_dt_fim           TYPE char8,
      l_pesq             TYPE char1,
      l_cdata_retorno    TYPE string,
      l_return_code      TYPE i,
      l_erro             TYPE c,
      t_xml_limite       TYPE TABLE OF ty_xml_limite,
      t_xml_operacao     TYPE TABLE OF ty_xml_operacao,
*
      t_zsdt0272         TYPE TABLE OF zsdt0272,
      t_zsdt0273         TYPE TABLE OF zsdt0273,
      t_zsdt0274         TYPE TABLE OF zsdt0274,
      t_zsdt0275         TYPE TABLE OF zsdt0275,
      t_zsdt0038         TYPE TABLE OF zsdt0038,
      w_zsdt0272         TYPE zsdt0272,
      w_zsdt0273         TYPE zsdt0273,
      w_zsdt0274         TYPE zsdt0274,
      w_zsdt0275         TYPE zsdt0275,
*
      http_client        TYPE REF TO if_http_client,
      xml_return         TYPE REF TO cl_xml_document.

*******************************************************************************************
* Tela selecao
*******************************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_data   FOR sy-datum  OBLIGATORY.
PARAMETERS    : p_cria AS CHECKBOX,
                p_modi AS CHECKBOX.
SELECTION-SCREEN: END   OF BLOCK b1.

*******************************************************************************************
* DEFINE: montagem do XML chamada
*******************************************************************************************
DEFINE conc_xml.
  CLEAR: l_xvalor.
  l_xvalor = &1.
  CONCATENATE l_xml l_xvalor INTO l_xml.
END-OF-DEFINITION.

*******************************************************************************************
* Processamento
*******************************************************************************************
START-OF-SELECTION.

  FREE: l_erro.

*---------------------------------------------
* Para Execução em backgound (jobs)
*---------------------------------------------
  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao(
                   EXPORTING i_nome_program = sy-cprog
                   IMPORTING e_qtd          = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd > 1.
      EXIT.
    ENDIF.
  ENDIF.

*---------------------------------------------
* selecao tabelas
*---------------------------------------------
  PERFORM f_selecao.

*---------------------------------------------
* JSON OPUS limite
*---------------------------------------------
  IF p_cria = abap_true.
    PERFORM f_consulta_opus_limite    USING 'E'
                                   CHANGING l_erro.
    CHECK l_erro IS INITIAL.
  ENDIF.

  IF p_modi = abap_true.
    PERFORM f_consulta_opus_limite    USING 'U'
                                   CHANGING l_erro.
    CHECK l_erro IS INITIAL.
  ENDIF.

*---------------------------------------------
* JSON OPUS operacao
*---------------------------------------------
  IF p_cria = abap_true.
    PERFORM f_consulta_opus_operacao    USING 'E'
                                     CHANGING l_erro.
    CHECK l_erro IS INITIAL.
  ENDIF.

  IF p_modi = abap_true.
    PERFORM f_consulta_opus_operacao    USING 'U'
                                     CHANGING l_erro.
    CHECK l_erro IS INITIAL.
  ENDIF.

  IF t_zsdt0272[] IS INITIAL.
    MESSAGE s024(sd) WITH text-010.
    STOP.
  ENDIF.

*---------------------------------------------
* Gravar tabelas
*---------------------------------------------
  PERFORM f_gravar_tabelas.

  MESSAGE s024(sd) WITH text-011.

*******************************************************************************************
* selecao tabelas
*******************************************************************************************
FORM f_selecao.

  FREE: t_zsdt0272,
        t_zsdt0273,
        t_zsdt0274,
        t_zsdt0275.

  SELECT *
    FROM zsdt0038
    INTO TABLE t_zsdt0038.

  SORT t_zsdt0038 BY descricao.

ENDFORM.

*******************************************************************************************
* gravar tabelas
*******************************************************************************************
FORM f_gravar_tabelas.

  SORT t_zsdt0272 BY nr_proposta.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0272
                        COMPARING nr_proposta.

  SORT t_zsdt0273 BY nr_proposta cpf_cnpj.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0273
                        COMPARING nr_proposta cpf_cnpj.

  SORT t_zsdt0274 BY nr_proposta safra cultura.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0274
                        COMPARING nr_proposta safra cultura.

  SORT t_zsdt0275 BY nr_proposta doc_simulacao.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0275
                        COMPARING nr_proposta doc_simulacao.

*----------------------------------
* Gerar tabelas
*----------------------------------
  LOOP AT t_zsdt0272 INTO w_zsdt0272.

*---elilina dados antigos
    DELETE FROM zsdt0272 WHERE nr_proposta = w_zsdt0272-nr_proposta.
    DELETE FROM zsdt0273 WHERE nr_proposta = w_zsdt0272-nr_proposta.
    DELETE FROM zsdt0274 WHERE nr_proposta = w_zsdt0272-nr_proposta.
    DELETE FROM zsdt0275 WHERE nr_proposta = w_zsdt0272-nr_proposta.

    LOOP AT t_zsdt0273 INTO w_zsdt0273 WHERE nr_proposta = w_zsdt0272-nr_proposta.
      INSERT zsdt0273  FROM w_zsdt0273.
    ENDLOOP.

    LOOP AT t_zsdt0274 INTO w_zsdt0274 WHERE nr_proposta = w_zsdt0272-nr_proposta.
      MODIFY zsdt0274  FROM w_zsdt0274.
    ENDLOOP.

    LOOP AT t_zsdt0275 INTO w_zsdt0275 WHERE nr_proposta = w_zsdt0272-nr_proposta.
      INSERT zsdt0275  FROM w_zsdt0275.
    ENDLOOP.

    MODIFY zsdt0272    FROM w_zsdt0272.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

*******************************************************************************************
* PESQUISA OPUS limite
*******************************************************************************************
FORM f_consulta_opus_limite    USING p_modo
                            CHANGING p_erro.

  FREE: l_url,
        l_xml,
        p_erro.

  PERFORM f_recupera_url   USING 'OPUS_CREDITO_PLIMITE'
                                 p_modo
                        CHANGING l_url.
  CHECK l_url IS NOT INITIAL.

  PERFORM f_monta_xml   CHANGING l_xml.
  PERFORM f_trata_xml   CHANGING l_xml.

  PERFORM f_consultar_json USING 'OPUS_CREDITO_PLIMITE'
                                 l_url
                                 l_xml
                        CHANGING p_erro.

  PERFORM f_processar_dados_limite.

ENDFORM.

*******************************************************************************************
* PESQUISA OPUS operacao
*******************************************************************************************
FORM f_consulta_opus_operacao    USING p_modo
                              CHANGING p_erro.

  FREE: l_url,
        l_xml,
        p_erro.

  PERFORM f_recupera_url   USING 'OPUS_CREDITO_POPERACAO'
                                 p_modo
                        CHANGING l_url.
  CHECK l_url IS NOT INITIAL.

  PERFORM f_monta_xml   CHANGING l_xml.
  PERFORM f_trata_xml   CHANGING l_xml.

  PERFORM f_consultar_json USING 'OPUS_CREDITO_POPERACAO'
                                 l_url
                                 l_xml
                        CHANGING p_erro.

  PERFORM f_processar_dados_operacao.

ENDFORM.

*******************************************************************************************
* consulta JSON
*******************************************************************************************
FORM f_consultar_json  USING p_servico
                             p_url
                             p_xml
                    CHANGING p_erro.

  FREE: l_cdata_retorno.

*---------------------------------------
*-Call service
*---------------------------------------
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = CONV #( l_url )
      ssl_id             = 'DFAULT'
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  PERFORM f_set_token     USING 'URL_TOKEN_OPUS_API' "p_servico
                       CHANGING http_client.

  CHECK http_client IS NOT INITIAL.

  CALL METHOD http_client->request->set_cdata
    EXPORTING
      data   = p_xml
      offset = 0
      length = strlen( p_xml ).

  CALL METHOD http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4.

  CASE sy-subrc.
    WHEN 1.
      http_client->close( ).
      MESSAGE s024(sd) WITH text-004  p_url DISPLAY LIKE 'E'.
      p_erro = abap_true.
      EXIT.
    WHEN 2.
      http_client->close( ).
      MESSAGE s024(sd) WITH text-005  p_url DISPLAY LIKE 'E'..
      p_erro = abap_true.
      EXIT.
    WHEN 3.
      http_client->close( ).
      MESSAGE s024(sd) WITH text-006  p_url DISPLAY LIKE 'E'.
      p_erro = abap_true.
      EXIT.
    WHEN 4.
      http_client->close( ).
      MESSAGE s024(sd) WITH text-007  p_url DISPLAY LIKE 'E'.
      p_erro = abap_true.
      EXIT.
  ENDCASE.

  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3.

  CASE sy-subrc.
    WHEN 1.
      http_client->close( ).
      MESSAGE s024(sd) WITH text-004  p_url DISPLAY LIKE 'E'.
      p_erro = abap_true.
      EXIT.
    WHEN 2.
      http_client->close( ).
      MESSAGE s024(sd) WITH text-005  p_url DISPLAY LIKE 'E'.
      p_erro = abap_true.
      EXIT.
    WHEN 3.
      http_client->close( ).
      MESSAGE s024(sd) WITH text-006  p_url DISPLAY LIKE 'E'.
      p_erro = abap_true.
      EXIT.
  ENDCASE.

  CREATE OBJECT xml_return.

  CALL METHOD xml_return->parse_string
    EXPORTING
      stream = http_client->response->get_cdata( ).

  http_client->response->get_status( IMPORTING code = l_return_code ).

  l_cdata_retorno = http_client->response->get_cdata( ).

* IF l_return_code <> '200'.
*   MESSAGE e024(sd) WITH text-008  p_url.
*   EXIT.
* ENDIF.

ENDFORM.

*******************************************************************************************
* processar dados recebidos
*******************************************************************************************
FORM f_processar_dados_limite.

  FREE: t_xml_limite.

*-------------------------------------------------
* descerializa json
*-------------------------------------------------
  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = l_cdata_retorno
    CHANGING
      data = t_xml_limite.

*-------------------------------------------------
* cria tabelas
*-------------------------------------------------
  LOOP AT t_xml_limite INTO DATA(w_xml_limite).

    FREE: w_zsdt0272.

    CONCATENATE w_xml_limite-datacriacao+6(4)
                w_xml_limite-datacriacao+3(2)
                w_xml_limite-datacriacao(2)
           INTO DATA(l_dt_criacao).

    CONCATENATE w_xml_limite-datacriacao+11(2)
                w_xml_limite-datacriacao+14(2)
                w_xml_limite-datacriacao+17(2)
           INTO DATA(l_hr_criacao).

    CONCATENATE w_xml_limite-dataultimaatuacao+6(4)
                w_xml_limite-dataultimaatuacao+3(2)
                w_xml_limite-dataultimaatuacao(2)
           INTO DATA(l_dt_ultimaatuacao).

    CONCATENATE w_xml_limite-dataultimaatuacao+11(2)
                w_xml_limite-dataultimaatuacao+14(2)
                w_xml_limite-dataultimaatuacao+17(2)
           INTO DATA(l_hr_ultimaatuacao).

    w_zsdt0272-mandt       = sy-mandt.
    w_zsdt0272-nr_proposta = w_xml_limite-numeroproposta.
    w_zsdt0272-tp_proposta = '1'.
    w_zsdt0272-dt_criacao  = l_dt_criacao.
    w_zsdt0272-hr_criacao  = l_hr_criacao.
    w_zsdt0272-estagio     = w_xml_limite-estagio.
    w_zsdt0272-dt_atuacao  = l_dt_ultimaatuacao.
    w_zsdt0272-hr_atuacao  = l_hr_ultimaatuacao.

*   IF sy-batch = abap_true.
    w_zsdt0272-dt_job      = sy-datum.
    w_zsdt0272-hr_job      = sy-uzeit.
*   ENDIF.

*-------------------------------------------------
*---emitente
*-------------------------------------------------
    t_emitente[] = w_xml_limite-emitente[].

    LOOP AT t_emitente INTO DATA(w_emitente).
      FREE: w_zsdt0273.

      w_zsdt0273-mandt       = sy-mandt.
      w_zsdt0273-nr_proposta = w_xml_limite-numeroproposta.
      w_zsdt0273-cpf_cnpj    = w_emitente-cpfcnpj.
      APPEND w_zsdt0273     TO t_zsdt0273.
    ENDLOOP.

*-------------------------------------------------
*---safra / cultura
*-------------------------------------------------
    t_safracultura[] = w_xml_limite-safracultura[].

    LOOP AT t_safracultura INTO DATA(w_safracultura).
      FREE: w_zsdt0274.

      READ TABLE t_zsdt0038 INTO DATA(w_zsdt0038)
                            WITH KEY descricao = w_safracultura-cultura
                            BINARY SEARCH.
      IF sy-subrc = 0.
        w_zsdt0274-cod_cultura = w_zsdt0038-cultura.
      ENDIF.

      w_zsdt0274-mandt         = sy-mandt.
      w_zsdt0274-nr_proposta   = w_xml_limite-numeroproposta.
      w_zsdt0274-safra         = w_safracultura-safra.
      w_zsdt0274-cultura       = w_safracultura-cultura.
      APPEND w_zsdt0274       TO t_zsdt0274.
    ENDLOOP.

    APPEND w_zsdt0272         TO t_zsdt0272.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* processar dados recebidos
*******************************************************************************************
FORM f_processar_dados_operacao.

  FREE: t_xml_operacao.

*-------------------------------------------------
* descerializa json
*-------------------------------------------------

  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = l_cdata_retorno
    CHANGING
      data = t_xml_operacao.

*-------------------------------------------------
* cria tabelas
*-------------------------------------------------
  LOOP AT t_xml_operacao INTO DATA(w_xml_operacao).

    FREE: w_zsdt0272,
          w_zsdt0274.

    CONCATENATE w_xml_operacao-datacriacao+6(4)
                w_xml_operacao-datacriacao+3(2)
                w_xml_operacao-datacriacao(2)
           INTO DATA(l_dt_criacao).

    CONCATENATE w_xml_operacao-datacriacao+11(2)
                w_xml_operacao-datacriacao+14(2)
                w_xml_operacao-datacriacao+17(2)
           INTO DATA(l_hr_criacao).

    CONCATENATE w_xml_operacao-dataultimaatuacao+6(4)
                w_xml_operacao-dataultimaatuacao+3(2)
                w_xml_operacao-dataultimaatuacao(2)
           INTO DATA(l_dt_ultimaatuacao).

    CONCATENATE w_xml_operacao-dataultimaatuacao+11(2)
                w_xml_operacao-dataultimaatuacao+14(2)
                w_xml_operacao-dataultimaatuacao+17(2)
           INTO DATA(l_hr_ultimaatuacao).

    w_zsdt0272-mandt           = sy-mandt.
    w_zsdt0272-nr_proposta     = w_xml_operacao-numeroproposta.
    w_zsdt0272-nr_proposta_ref = w_xml_operacao-propostalimite.
    w_zsdt0272-tp_proposta     = '2'.
    w_zsdt0272-dt_criacao      = l_dt_criacao.
    w_zsdt0272-hr_criacao      = l_hr_criacao.
    w_zsdt0272-estagio         = w_xml_operacao-estagio.
    w_zsdt0272-efetivada       = w_xml_operacao-efetivada.
    w_zsdt0272-dt_atuacao      = l_dt_ultimaatuacao.
    w_zsdt0272-hr_atuacao      = l_hr_ultimaatuacao.

*   IF sy-batch = abap_true.
    w_zsdt0272-dt_job          = sy-datum.
    w_zsdt0272-hr_job          = sy-uzeit.
*   ENDIF.

*-------------------------------------------------
*---emitente
*-------------------------------------------------
    t_emitente[] = w_xml_operacao-emitente[].

    LOOP AT t_emitente INTO DATA(w_emitente).
      FREE: w_zsdt0273.

      w_zsdt0273-mandt       = sy-mandt.
      w_zsdt0273-nr_proposta = w_xml_operacao-numeroproposta.
      w_zsdt0273-cpf_cnpj    = w_emitente-cpfcnpj.
      APPEND w_zsdt0273     TO t_zsdt0273.
    ENDLOOP.

*-------------------------------------------------
*---Safra / cultura
*-------------------------------------------------
    READ TABLE t_zsdt0038 INTO DATA(w_zsdt0038)
                          WITH KEY descricao = w_xml_operacao-cultura
                          BINARY SEARCH.
    IF sy-subrc = 0.
      w_zsdt0274-cod_cultura = w_zsdt0038-cultura.
    ENDIF.

    w_zsdt0274-mandt         = sy-mandt.
    w_zsdt0274-nr_proposta   = w_xml_operacao-numeroproposta.
    w_zsdt0274-safra         = w_xml_operacao-safra.
    w_zsdt0274-cultura       = w_xml_operacao-cultura.
    APPEND w_zsdt0274       TO t_zsdt0274.

*-------------------------------------------------
*---documento simulacao
*-------------------------------------------------
    t_documentosimulacao[] = w_xml_operacao-documentosimulacao[].

    LOOP AT t_documentosimulacao INTO DATA(w_documentosimulacao).
      FREE: w_zsdt0275.

      w_zsdt0275-mandt          = sy-mandt.
      w_zsdt0275-nr_proposta    = w_xml_operacao-numeroproposta.
      w_zsdt0275-doc_simulacao  = w_documentosimulacao-documento.
      APPEND w_zsdt0275        TO t_zsdt0275.
    ENDLOOP.

    APPEND w_zsdt0272          TO t_zsdt0272.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* ver URL
*******************************************************************************************
FORM f_recupera_url    USING p_servico
                             p_modo
                    CHANGING p_url.

  CLEAR: w_zauth_webservice,
         p_url.

  SELECT SINGLE *
    FROM zauth_webservice
    INTO w_zauth_webservice
   WHERE service = p_servico.

  IF sy-subrc = 0.
    p_url = w_zauth_webservice-url.
  ENDIF.

  CONCATENATE s_data-low+6(2) s_data-low+4(2) s_data-low(4)
         INTO l_dt_ini.

  CONCATENATE s_data-high+6(2) s_data-high+4(2) s_data-high(4)
         INTO l_dt_fim.

  IF l_dt_fim = '00000000'.
    l_dt_fim = l_dt_ini.
  ENDIF.

  CONCATENATE p_url l_dt_ini '/' l_dt_fim '/' p_modo
         INTO p_url.

ENDFORM.

*******************************************************************************************
* montar XML
*******************************************************************************************
FORM f_monta_xml CHANGING p_xml.

*  FREE: l_xml.
*
*  conc_xml '{'.
*  conc_xml '"periodo": {'.
*  conc_xml '"data_inicio": "'.
*  l_dtaux = s_data-low(4)  && '-' && s_data-low+4(2)  && '-' && s_data-low+6(2)  && 'T00:00:00",'.
*  conc_xml l_dtaux .
*  conc_xml '"data_fim": "'.
*  l_dtaux = s_data-high(4) && '-' && s_data-high+4(2) && '-' && s_data-high+6(2) && 'T23:59:59"'.
*  conc_xml l_dtaux .
*  conc_xml '}'.
*
*  p_xml = l_xml.
*
ENDFORM.

*******************************************************************************************
* tratar  XML
*******************************************************************************************
FORM f_trata_xml CHANGING p_xml.

  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN p_xml WITH 'a' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN p_xml WITH 'e' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'í'     IN p_xml WITH 'i' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN p_xml WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN p_xml WITH 'u' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN p_xml WITH 'c' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '&'     IN p_xml WITH '&#38;'.
  REPLACE ALL OCCURRENCES OF        ''''    IN p_xml WITH '&#39;'.
  REPLACE ALL OCCURRENCES OF        'º'     IN p_xml WITH 'o' IGNORING CASE.

ENDFORM.

*******************************************************************************************
* tratar TOKEN
*******************************************************************************************
FORM f_set_token     USING p_servico
                  CHANGING c_if_http_client TYPE REF TO if_http_client.

  DATA: lv_value      TYPE string.
  DATA: lv_value_user TYPE string.
  DATA: lv_value_pass TYPE string.
  DATA: lv_value_gest TYPE string.
  DATA: v_url               TYPE ui_src_url.
  DATA: v_token             TYPE string.

  DATA: ex_webservice_token TYPE REF TO zcl_webservice,
        lc_endereco         TYPE string.

  TYPES BEGIN OF ty_retorno_msg.
  TYPES message TYPE string.
  TYPES END OF ty_retorno_msg.

  TYPES BEGIN OF ty_json_retorno.
  TYPES: access_token       TYPE string.
  TYPES: token_type         TYPE string.
  TYPES END OF ty_json_retorno.

  DATA: lc_mensagem TYPE ty_retorno_msg,
        lc_retorno  TYPE ty_json_retorno.

  CREATE OBJECT ex_webservice_token.

  SELECT SINGLE *
           INTO @DATA(ls_zauth_webservice)
           FROM zauth_webservice
          WHERE service = @p_servico.

  IF sy-subrc = 0.
    CREATE OBJECT ex_webservice_token.
    ex_webservice_token->at_url = ls_zauth_webservice-url.
    DATA(var_http) = ex_webservice_token->url( i_url = CONV #( ex_webservice_token->at_url ) ).
    ex_webservice_token->zif_webservice~abrir_conexao( i_http = var_http ).

    CALL METHOD var_http->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'GET'.

    CALL METHOD var_http->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.

    CALL METHOD var_http->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json'.

    lv_value_user = ls_zauth_webservice-username.
    var_http->request->set_form_field( EXPORTING name = 'username' value = lv_value_user ).

    lv_value_pass = ls_zauth_webservice-password.
    var_http->request->set_form_field( EXPORTING name = 'password' value = lv_value_pass ).

    lv_value_gest = ls_zauth_webservice-add01.
    var_http->request->set_form_field( EXPORTING name = 'cod_gestao' value = lv_value_gest ).

    DATA text_form TYPE string.
*   text_form = '{"username":"' && lv_value_user && '","password":"' && lv_value_pass && '","cod_gestao":' && lv_value_gest && '}'.
    text_form = 'grant_type=password&username=' && lv_value_user && '&password=' && lv_value_pass.

    ex_webservice_token->zif_webservice~consultar(
      EXPORTING
        i_http                     = var_http
        i_xml                      = text_form
      IMPORTING
        e_code                     = DATA(e_code)
        e_reason                   = DATA(e_reason)
      RECEIVING
        e_resultado                = DATA(token_retorno)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    CASE sy-subrc.
      WHEN 1 OR 5.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
      WHEN 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      WHEN 3.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      WHEN 4.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.

    IF e_code <> 200.
      DATA: lc_texto TYPE c LENGTH 200.

      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json = token_retorno
        CHANGING
          data = lc_mensagem.

      lc_texto = lc_mensagem-message.
      sy-msgv1 = lc_texto+000(50).
      sy-msgv2 = lc_texto+050(50).
      sy-msgv3 = lc_texto+100(50).
      sy-msgv4 = lc_texto+150(50).

      MESSAGE i028(zsimetrya) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lwa_message_token).
      CONCATENATE 'Rumo Token:' lwa_message_token INTO lwa_message_token SEPARATED BY space.
      MESSAGE lwa_message_token TYPE 'I'.
    ELSE.
      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json = token_retorno
        CHANGING
          data = lc_retorno.

      CONCATENATE 'bearer' lc_retorno-access_token
             INTO v_token SEPARATED BY space.

      CALL METHOD c_if_http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'GET'.

      CALL METHOD c_if_http_client->request->set_header_field
        EXPORTING
          name  = '~server_protocol'
          value = 'HTTP/1.1'.

      CALL METHOD c_if_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'application/json'.

      CALL METHOD c_if_http_client->request->set_header_field
        EXPORTING
          name  = 'Authorization'
          value = v_token.
    ENDIF.
  ENDIF.

ENDFORM.

*******************************************************************************************
*******************************************************************************************
