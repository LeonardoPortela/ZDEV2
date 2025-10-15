class ZCL_AGRIQ_DOCUMENTS_STEP_01 definition
  public
  inheriting from ZCL_DEFAULT_DOCUMENTS_STEP_01
  final
  create public .

public section.

  data AT_RECEITA_KEY type ZRECEITAKEY .
  data AT_PDF_RECEITA_AGRIQ type XSTRING .
  data AT_ZSDT0218 type ZSDT0218 .
  data AT_ZSDT0259 type ZSDT0259 .
  data AT_JSON type STRING .
  data AT_PAGINA_ASSINATURA type NUMC2 .

  methods CONSTRUCTOR .
  methods GET_FORMATED_DATE
    importing
      !IV_DATE type SY-DATUM
    returning
      value(R_DATE_S) type STRING .

  methods AFTER_SENDING
    redefinition .
  methods BEFORE_SENDING
    redefinition .
  methods GET_DOCUMENTS
    redefinition .
  methods SEND_TO_APPROVAL
    redefinition .
  methods SET_SYSTEM
    redefinition .
  methods ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG
    redefinition .
  methods LOG_API_SEND
    redefinition .
protected section.
private section.

  data AT_DOC_TAB type ZINS_ADIGITAL_CONTRATOS .
  data GV_CURRENT_ID type ZIN_ID_REFERENCIA .
  data GV_PARAMS type STRING .

  methods CONVERT_BOOLEAN
    importing
      !IV_PARAM type MSGTX
    returning
      value(RV_RETURN) type MSGTX .
  methods SEND_TO_DESTINATION
    importing
      !I_DOCUMENT type ZINS_ADIGITAL_CONTRACT
      !IT_ANEXOS type ZINS_ADIGITAL_ATTACHMENTS
    returning
      value(R_ADIGITAL) type ZINT_ASSINA01 .
  methods GET_ASSINANTES
    importing
      !I_ID type STRING
    returning
      value(R_RELACAO) type STRING .
  methods GET_ASSINANTES_EXTRAS
    importing
      !IW_DADOS_CONTRATO type ZINS_ADIGITAL_CONTRACT
    returning
      value(R_RELACAO) type STRING .
  methods GET_ATTACHMENTS
    importing
      !I_REFERENCE type STRING
    returning
      value(R_ATTACHMENTS) type ZINS_ADIGITAL_ATTACHMENTS .
  methods GET_ATTACHMENTS2
    importing
      !I_REFERENCE type STRING
    returning
      value(R_ATTACHMENTS) type ZINS_ADIGITAL_ATTACHMENTS .
  methods FORMAT_DATE_BRY
    importing
      !I_DATE type STRING
    returning
      value(R_DATE) type STRING .
  methods CPF_BY_EMAIL
    importing
      !I_EMAIL type STRING
    returning
      value(R_CPF) type STRING .
  methods GET_CONTRACT_URL
    importing
      !ID_CONTRATO type ZIN_ID_REFERENCIA
    returning
      value(R_URL) type STRING .
  methods LOOKUP_VALUES_EXECUTE
    importing
      !I_LOOKUP_ID type STRING
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO .
  methods MESSAGE_LENGTH
    importing
      !IV_XFILE type XSTRING .
  methods GET_RESPONSAVEL_ASSINATURA
    returning
      value(R_RESPON) type MSGTX .
  methods GET_SYSTEM_VAR
    importing
      !I_VARNAME type STRING
    returning
      value(R_RETVAR) type STRING .
  methods SET_ENVIAR_PDF_BRY .
  methods GET_CHAVE_COLETA_BRY
    importing
      !IT_ANEXOS type ZINS_ADIGITAL_ATTACHMENTS .
  methods SET_POSICAO_ASSINATURA_BRY
    importing
      !IT_ANEXOS type ZINS_ADIGITAL_ATTACHMENTS .
  methods SET_JSON
    importing
      !P1 type STRING optional
      !P2 type STRING optional
      !P3 type STRING optional .
  methods SET_JSON_ASSINATURA .
ENDCLASS.



CLASS ZCL_AGRIQ_DOCUMENTS_STEP_01 IMPLEMENTATION.


  METHOD after_sending.

*    TRY.
*
*        DATA lv_body TYPE string.
*        DATA lv_text TYPE string.
*
*        CHECK i_adigital IS NOT INITIAL.
*
*        MODIFY zint_assina01 FROM i_adigital.
*
**------------------
**-      zsdt0218
**------------------
*        UPDATE zsdt0218 SET chave_workflow   = i_adigital-chave_coleta
*                      WHERE numeroreceita    = me->at_zsdt0218-numeroreceita
*                        AND numeropedido     = me->at_zsdt0218-numeropedido
*                        AND cpfrt            = me->at_zsdt0218-cpfrt
*                        AND receitakey       = me->at_zsdt0218-receitakey.
*
*        COMMIT WORK.
*
*      CATCH zcx_integracao .
*      CATCH zcx_error .
*    ENDTRY.

  ENDMETHOD.


  method BEFORE_SENDING.

    r_attachments = me->get_attachments( i_reference ).

  endmethod.


  METHOD constructor.

    super->constructor( ).

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_agriq.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    me->at_service = 'AGRIQ_INT_ASSINATURA_DIGITAL'.

  ENDMETHOD.


  method CONVERT_BOOLEAN.

*    DATA lv_param TYPE string.
*
*    IF iv_param IS INITIAL.
*      rv_return = 'false'.
*      EXIT.
*    ENDIF.
*
*    lv_param = iv_param.
*
*    TRANSLATE lv_param TO UPPER CASE.
*
*    lv_param = zcl_string2=>remove_spec_char( lv_param ).
*
*    IF lv_param = 'NAO' OR lv_param = 'FALSE'.
*      rv_return = 'false'.
*      EXIT.
*    ENDIF.
*
*    IF lv_param = 'SIM' OR lv_param = 'TRUE'.
*      rv_return = 'true'.
*      EXIT.
*    ENDIF.

  endmethod.


  method CPF_BY_EMAIL.
  endmethod.


  method FORMAT_DATE_BRY.

    DATA lv_datum TYPE sy-datum.
    DATA lv_string TYPE c LENGTH 20.
    DATA lv_inicial TYPE c.

    DATA(lv_date) = i_date.

    IF i_date IS NOT INITIAL.

      lv_string = i_date(10).

      REPLACE ALL OCCURRENCES OF '-' IN lv_string WITH ''.
      CONDENSE lv_string NO-GAPS.

      lv_datum = lv_string.

      IF lv_datum < sy-datum.
        lv_inicial = 'X'.
      ENDIF.

    ELSE.
      lv_inicial = 'X'.
    ENDIF.

    IF lv_inicial = 'X'.

      lv_date = '&1-&2-&3T&4:&5:&6-04:00'.

      REPLACE '&1' IN lv_date WITH sy-datum(4).
      REPLACE '&2' IN lv_date WITH sy-datum+4(2).
      REPLACE '&3' IN lv_date WITH sy-datum+6(2).
      REPLACE '&4' IN lv_date WITH sy-uzeit(2).
      REPLACE '&5' IN lv_date WITH sy-datum+2(2).
      REPLACE '&6' IN lv_date WITH sy-datum+4(2).

    ENDIF.

    REPLACE FIRST OCCURRENCE OF '-' IN lv_date WITH space.
    REPLACE FIRST OCCURRENCE OF '-' IN lv_date WITH space.
    REPLACE ALL OCCURRENCES OF ':' IN lv_date WITH space.

    CONDENSE lv_date NO-GAPS.

    r_date = lv_date.

  endmethod.


  method GET_ASSINANTES.
  endmethod.


  method GET_ASSINANTES_EXTRAS.
  endmethod.


  method GET_ATTACHMENTS.

*    DATA: lv_text         TYPE string,
*          t_element_array TYPE zde_element_array_t.
*
*    APPEND INITIAL LINE TO r_attachments-attachments-attachment ASSIGNING FIELD-SYMBOL(<fs_attach>).
*
*    <fs_attach>-id               = '001'.
*    <fs_attach>-createdat        = format_date_bry( ).
*    <fs_attach>-updatedat        = format_date_bry( ).
*    <fs_attach>-type             = 'AttachmentFile'.
*    <fs_attach>-intent           = 'Internal'.
*    <fs_attach>-file             = 'C:/temp/Receita_Agronomica.pdf'.
*    <fs_attach>-fileurl          = 'htps://temp/Receita_Agronomica.pdf'.
*    <fs_attach>-file_name        = 'Receita_Agronomica.pdf'.
*    <fs_attach>-application_type = 'application/pdf'.
*    <fs_attach>-xfile            = me->at_pdf_receita_agriq.
*
*    message_length( <fs_attach>-xfile ).
*
  endmethod.


  method GET_ATTACHMENTS2.
  endmethod.


  method GET_CHAVE_COLETA_BRY.

*    DATA: lo_envio      TYPE REF TO zcl_integracao_bry_adigital,
*          lw_envio      TYPE zins_dados_bry_dados,
*          lt_sucesso    TYPE zins_dados_bry_json_coleta_t,
*          lw_sucesso    TYPE zins_dados_bry_json_coleta,
*          lw_erro       TYPE zins_dados_bry_json_erro,
*          lv_receitakey TYPE string,
*          lv_response   TYPE string.
*
*    FREE: me->at_bapiret2_tab.
*
*    lv_receitakey = me->at_zsdt0218-receitakey.
*
**-----------------------------
**-- recupera chave coleta
**-----------------------------
*    SELECT SINGLE *
*      FROM zsdt0218
*      INTO me->at_zsdt0218
*     WHERE numeroreceita  = me->at_zsdt0218-numeroreceita
*       AND numeropedido   = me->at_zsdt0218-numeropedido
*       AND cpfrt          = me->at_zsdt0218-cpfrt
*       AND receitakey     = me->at_zsdt0218-receitakey
*       AND cancelada      = abap_off.
*
*    CHECK me->at_zsdt0218-chave_workflow  IS NOT INITIAL.
*    CHECK me->at_zsdt0218-chave_documento IS INITIAL.
*
**---------------------------
**-- montar campos API
**---------------------------
**    lw_envio-metodo_envio     = 'GET'.
**   lw_envio-endpoint_coletas = me->at_zsdt0218-chave_workflow.
**    lw_envio-id_referencia    = me->at_zsdt0218-receitakey.
*
**---------------------------
**-- Envio BRY --------------
**---------------------------
*    TRY.
*        CREATE OBJECT lo_envio
*          EXPORTING
*            it_anexos     = it_anexos
*            i_dados_envio = lw_envio.
*
*        lv_response = lo_envio->zif_integracao_bry_adigital~enviar_bry( ).
*
*        IF lv_response CS 'status'.
*          /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*                                     CHANGING  data = lw_erro ).
*          me->set_message( i_message = lw_erro-mensagens[ 1 ]-mensagem i_doc_id = lv_receitakey ).
*          me->log_update( ).
*          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_erro-mensagens[ 1 ]-mensagem ).
*        ELSE.
*          /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*                                     CHANGING  data = lt_sucesso ).
*
**------------------
**-------- chave documento
**------------------
*          READ TABLE lt_sucesso INTO lw_sucesso INDEX 1.
*
*          UPDATE zsdt0218 SET chave_documento  = lw_sucesso-chavedocumento
*                        WHERE numeroreceita    = me->at_zsdt0218-numeroreceita
*                          AND numeropedido     = me->at_zsdt0218-numeropedido
*                          AND cpfrt            = me->at_zsdt0218-cpfrt
*                          AND receitakey       = me->at_zsdt0218-receitakey.
*          COMMIT WORK.
*        ENDIF.
*
*        IF lw_erro IS NOT INITIAL.
*          me->set_message( i_message =  lw_erro-mensagens[ 1 ]-mensagem i_doc_id = lv_receitakey ).
*          me->log_update( ).
*          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_erro-mensagens[ 1 ]-mensagem ).
*        ENDIF.
*
*      CATCH zcx_integracao INTO DATA(ex_int).
*        me->set_message( i_message = ex_int->get_longtext( ) i_doc_id = lv_receitakey ).
*        me->log_update( ).
*        zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro Integração com a Bry' ).
*
*      CATCH zcx_error INTO DATA(ex_erro).
*        me->set_message( i_message = ex_erro->get_longtext( ) i_doc_id = lv_receitakey ).
*        me->log_update( ).
*        zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro Integração com a Bry' ).
*    ENDTRY.
*
  endmethod.


  method GET_CONTRACT_URL.

    DATA(lv_rev) = me->at_auth_ws-url.

    REPLACE 'api/' IN lv_rev WITH space.

    r_url = lv_rev && 'contracts/show/' && id_contrato && '#summary'.

  endmethod.


  METHOD get_documents.
*    DATA: l_tamanho TYPE i,
*          t_bin     TYPE TABLE OF char80,
*          l_string  TYPE string,
*          l_page    TYPE numc2.
*
*    DATA(l_receitakey)   = ir_id_ref_range[ 1 ]-low.
*
*    FREE: me->at_zsdt0218,
*          me->at_zsdt0259.
*
**   SELECT SINGLE *
**     FROM zsdt0218
**     INTO me->at_zsdt0218
**    WHERE receitakey = l_receitakey
**      AND cancelada  = abap_off.
*
*    SELECT *
*      FROM zsdt0218
*      INTO TABLE @DATA(t_0218)
*     WHERE receitakey = @l_receitakey
*       AND cancelada  = @abap_off.
*
*    SORT t_0218 BY receitakey data_atual DESCENDING hora_atual DESCENDING.
*    DELETE ADJACENT DUPLICATES FROM t_0218
*                          COMPARING receitakey.
*
*    READ TABLE t_0218 INTO me->at_zsdt0218 INDEX 1.
*
*    SELECT SINGLE *
*      FROM zsdt0259
*      INTO me->at_zsdt0259
*     WHERE cpf           = me->at_zsdt0218-cpfrt.
*
*    CHECK me->at_zsdt0218-chave_workflow IS INITIAL.
*
**-------------------------------
**-- id processo para LOG
**-------------------------------
*    me->at_id_processo = me->at_zsdt0218-receitakey.
*
**-------------------------------
**-- Executa API - Recuperar PDF Receita
**-------------------------------
*    TRY .
*        zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
*           )->set_exec_agriq( EXPORTING i_metodo            = 'LER_PDF'
*                                        i_receitakey        = me->at_zsdt0218-receitakey
*                              IMPORTING e_pdf_receita_agriq = me->at_pdf_receita_agriq ).
*
*      CATCH zcx_integracao INTO DATA(ex_integra).
*        zcx_error=>zif_error~gera_erro_geral( i_texto = 'Não foi possivel obter PDF Receituário!' ).
*
*      CATCH zcx_error      INTO DATA(ex_error).
*        zcx_error=>zif_error~gera_erro_geral( i_texto = 'Não foi possivel obter PDF Receituário!' ).
*    ENDTRY.
*
**-------------------------------
**-- Verifica quantas paginas tem o PDF para posicionar
**-- corretamente a assinatura no documento
**-------------------------------
*    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*      EXPORTING
*        buffer        = me->at_pdf_receita_agriq
*      IMPORTING
*        output_length = l_tamanho
*      TABLES
*        binary_tab    = t_bin.
*
*    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
*      EXPORTING
*        input_length = l_tamanho
*      IMPORTING
*        text_buffer  = l_string
*      TABLES
*        binary_tab   = t_bin
*      EXCEPTIONS
*        failed       = 1
*        OTHERS       = 2.
*
**-------------------------------
**-- Pagina onde posiciona a assinatura
**-------------------------------
*    SPLIT l_string AT '/Count' INTO TABLE DATA(t_dados).
*
*    DESCRIBE TABLE t_dados LINES DATA(l_lines).
*    READ TABLE t_dados INTO DATA(w_dados) INDEX l_lines.
*    CONDENSE w_dados.
*
*    l_page = w_dados(1) - 1.
*
*    me->at_pagina_assinatura = l_page.
*
  ENDMETHOD.


  method GET_FORMATED_DATE.

    r_date_s = iv_date(4) && '-' && iv_date+4(2) && '-' && iv_date+6(2).

  endmethod.


  method GET_RESPONSAVEL_ASSINATURA.

*    DATA lt_set_tab TYPE rgsbv_tab.
*
*    CLEAR r_respon.
*
*    SELECT SINGLE low FROM tvarvc
*      INTO r_respon
*        WHERE name = 'MAGGI_CNPJ_CPF_RESPONSAVEL_BRY'.
*
*    IF sy-subrc NE 0.
*      CLEAR r_respon.
*    ENDIF.

  endmethod.


  method GET_SYSTEM_VAR.

    CASE sy-sysid.
      WHEN 'QAS'.

        CASE i_varname.
          WHEN 'APPROVERID'.
            r_retvar = '28'.
        ENDCASE.

      WHEN OTHERS.

        CASE i_varname.
          WHEN 'APPROVERID'.
            r_retvar = '12'.
        ENDCASE.

    ENDCASE.

  endmethod.


  METHOD log_api_send.
  ENDMETHOD.


  method LOOKUP_VALUES_EXECUTE.
*    DATA lv_params TYPE string.
*
*    CHECK i_lookup_id IS NOT INITIAL.
*
*    lv_params = 'lookup_values/' && i_lookup_id.
*
*    r_integracao = me->execute_service( i_params = lv_params i_method = 'GET').
  endmethod.


  method MESSAGE_LENGTH.

*    DATA lv_id TYPE string.
*
*    DATA lv_char TYPE c LENGTH 30.
*
*    DATA lv_mess TYPE string.
*
*    DATA lv_div TYPE i VALUE 1000000.
*
*    DATA lv_mb TYPE p DECIMALS 3.
*
*    DATA(lv_length) = xstrlen( iv_xfile ).
*
*    lv_mb = lv_length / lv_div.
*
*    WRITE lv_mb TO lv_char LEFT-JUSTIFIED.
*
*    lv_mess = `Arquivo com tamanho ` && lv_char && ` MB`.
*
*    lv_id = me->at_zsdt0218-receitakey.
*
*    CALL METHOD me->set_message
*      EXPORTING
*        i_message = lv_mess
*        i_msgty   = 'W'
*        i_doc_id  = lv_id.

  endmethod.


  method SEND_TO_APPROVAL.

*----------------------------------
*-- Enviar PDF
*----------------------------------
*    me->set_enviar_pdf_bry( ).
*
**----------------------------------
**-- REcuperar chave coleta PDF enviado
**----------------------------------
*    me->get_chave_coleta_bry( ).
*
**----------------------------------
**-- Posiionar local da assinatura
**----------------------------------
*    me->set_posicao_assinatura_bry( ).

  endmethod.


  method SEND_TO_DESTINATION.

*    DATA: lo_envio      TYPE REF TO zcl_integracao_bry_adigital,
*          lw_envio      TYPE zins_dados_bry_dados,
*          lw_sucesso    TYPE zins_dados_bry_json,
*          lw_erro       TYPE zins_dados_bry_json_erro,
*          lv_response   TYPE string,
*          lv_mess       TYPE string,
*          lv_assina     TYPE string,
*          lv_receitakey TYPE string,
*          lv_data       TYPE datum.
*
*    lv_data       = sy-datum + 15.
*    lv_receitakey = me->at_zsdt0218-receitakey.
*
**---------------------------
**- relacao assinantes
**---------------------------
*    lv_assina                            = me->at_zsdt0218-cpfrt && '-' &&
*                                           me->at_zsdt0259-nome  && '-' &&
*                                           me->at_zsdt0259-email && '-' && 'ASSINATURA_ELETRONICA'.
*
**---------------------------
**-- montar campos API
**---------------------------
*    lw_envio-nomecoleta                  = 'Receita:'  && me->at_zsdt0218-numeroreceita && '-' &&
*                                           'Filial:'   && me->at_zsdt0218-numeropedido  && '-' &&
*                                           'Agronomo:' && me->at_zsdt0259-nome.
*    lw_envio-datalimite                  = lv_data && 'T000000-0400'.
*    lw_envio-descricao                   = 'Assinar Receita Agronomica'.
*    lw_envio-padraoassinatura            = 'PDF'.
*    lw_envio-exigirdownload              = 'true'.
*    lw_envio-proibirrejeicao             = 'true'.
*    lw_envio-assinaturasequencial        = 'false'.
*    lw_envio-agrupardocumentos           = 'false'.
*    lw_envio-local_assinatura            = 'MANUAL'.
*    lw_envio-relacao_assinantes          = lv_assina.
*    lw_envio-exigirdownload              = 'true'.
*    lw_envio-codigoresponsavel           = abap_off. "get_responsavel_assinatura( ).
**   lw_envio-configuracaolocalassinatura = 'MANUAL'.
**    lw_envio-id_referencia               = me->at_zsdt0218-receitakey.
*
**---------------------------
**-- Envio BRY --------------
**---------------------------
*    TRY.
*        CREATE OBJECT lo_envio
*          EXPORTING
*            it_anexos     = it_anexos
*            i_dados_envio = lw_envio.
*
*        lv_response = lo_envio->zif_integracao_bry_adigital~enviar_bry( ).
*
*        IF lv_response CS 'status'.
*          /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*                                     CHANGING  data = lw_erro ).
*          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_erro-mensagens[ 1 ]-mensagem ).
*        ELSE.
*          /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*                                     CHANGING  data = lw_sucesso ).
*        ENDIF.
*
*        IF lw_erro IS NOT INITIAL.
*          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_erro-mensagens[ 1 ]-mensagem ).
*        ENDIF.
*
*        CHECK lw_sucesso IS NOT INITIAL.
*
**---------------------------
**------ Estrutura tabela ZINT_ASSINA01
**---------------------------
**        r_adigital-id_referencia = me->at_zsdt0218-receitakey.
*        r_adigital-id_processo   = at_system_id.
*        r_adigital-etapa         = '01'. "<- ENVIADO AO BRY
*        r_adigital-nome          = lw_envio-nomecoleta.
*        r_adigital-chave_coleta  = lw_sucesso-chaveworkflow.
*        r_adigital-log_date      = sy-datum.
*        r_adigital-log_uzeit     = sy-uzeit.
*        r_adigital-log_name      = sy-uname.
*
*      CATCH zcx_integracao INTO DATA(ex_int).
*        me->set_message( i_message = ex_int->get_longtext( ) i_doc_id = lv_receitakey ).
*
*      CATCH zcx_error INTO DATA(ex_erro).
*        me->set_message( i_message = ex_erro->get_longtext( ) i_doc_id = lv_receitakey ).
*
*    ENDTRY.
*
  endmethod.


  method SET_ENVIAR_PDF_BRY.

*    DATA: lv_text         TYPE string,
*          lv_receitakey   TYPE string,
*          t_element_array TYPE zde_element_array_t,
*          lw_anexos       TYPE zins_adigital_attachments.
*
*    FREE: me->at_bapiret2_tab.
*
*    CHECK me->at_zsdt0218-chave_workflow IS INITIAL.
*
*    lv_receitakey = me->at_zsdt0218-receitakey.
*
*    TRY.
*        DATA(lw_assina) = me->send_to_destination( EXPORTING it_anexos  = me->before_sending( lv_receitakey ) ).
*
*        IF lw_assina-chave_coleta IS INITIAL.
*          lv_text = `Documento: ` && lv_receitakey && ` não gerou chave no sistema destino`.
*          me->set_message( i_message = lv_text  i_doc_id = lv_receitakey ).
*          me->log_update( ).
*          zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro na Integralção com Bry' ).
*        ELSE.
*          me->after_sending( lw_assina ).
*          lv_text = `Documento: ` && lv_receitakey && ` enviado, coleta: ` && lw_assina-chave_coleta.
*          me->set_message( EXPORTING i_message = lv_text i_msgty = 'S' i_doc_id = lv_receitakey ).
*          me->log_update( ).
*        ENDIF.
*
*        COMMIT WORK AND WAIT.
*
*      CATCH zcx_integracao INTO DATA(ex_int).
*        me->set_message( EXPORTING i_message = ex_int->get_longtext( preserve_newlines = 'X' ) i_doc_id = lv_receitakey ).
*        me->log_update( ).
*        zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro na Integralção com Bry' ).
*
*      CATCH zcx_error INTO DATA(ex_erro).
*        me->set_message( EXPORTING i_message = ex_erro->get_longtext( preserve_newlines = 'X' ) i_doc_id = lv_receitakey ).
*        me->log_update( ).
*        zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro na Integralção com Bry' ).
*    ENDTRY.
*
  endmethod.


  method SET_JSON.

    IF p1 = '{' OR p1 = '[' OR p1 = ']'.
      me->at_json = me->at_json && p1.
      EXIT.
    ENDIF.

    IF p1 = '}'.
      me->at_json = me->at_json && p1.
      EXIT.
    ENDIF.

    at_json = at_json && '"' && p1 && '"' && ':'.

    IF     p3 = '#'.
      at_json = at_json && '"' && p2 && '"' && ','.
    ELSEIF p3 = '@'.
      at_json = at_json &&        p2.
    ELSE.
      at_json = at_json &&        p2 &&        ','.
    ENDIF.

  endmethod.


  method SET_JSON_ASSINATURA.

*    DATA: t_set    TYPE TABLE OF rgsb4,
*          w_set    TYPE rgsb4,
*          l_pagina TYPE i,
*          l_eixo_x TYPE i,
*          l_eixo_y TYPE i.
*
*    FREE: me->at_json.
*
*    l_pagina = 1.
*    l_eixo_x = 490.
*    l_eixo_y = 930.
*
**-----------------------------------------
**-- posicao assinatura no documento
**-----------------------------------------
*    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*      EXPORTING
*        class           = '0000'
*        setnr           = 'POSICAO_ASSINATURA_BRY'
*        no_descriptions = abap_off
*      TABLES
*        set_values      = t_set
*      EXCEPTIONS
*        set_not_found   = 1
*        OTHERS          = 2.
*
*    LOOP AT t_set INTO w_set.
*      CASE w_set-from.
*        WHEN 'PAGINA'.
*          l_pagina = w_set-title.
*        WHEN 'EIXOX'.
*          l_eixo_x = w_set-title.
*        WHEN 'EIXOY'.
*          l_eixo_y = w_set-title.
*      ENDCASE.
*    ENDLOOP.
*
*    IF me->at_pagina_assinatura IS NOT INITIAL.
*      l_pagina = me->at_pagina_assinatura.
*    ENDIF.
*
**-----------------------------------------
**-- saida JSON
**-----------------------------------------
*    set_json( p1 = '[').
*    set_json( p1 = '{' ).
*    set_json( p1 = 'pagina'               p2 = CONV #( l_pagina )                         p3 = ' ').
*    set_json( p1 = 'eixoX'                p2 = CONV #( l_eixo_x )                         p3 = ' ' ).
*    set_json( p1 = 'eixoY'                p2 = CONV #( l_eixo_y )                         p3 = ' ' ).
*    set_json( p1 = 'documento'            p2 = CONV #( me->at_zsdt0218-chave_documento )  p3 = '#' ).
*    set_json( p1 = 'participante'         p2 = CONV #( me->at_zsdt0218-cpfrt )            p3 = '#' ).
*    set_json( p1 = 'participanteId'       p2 = CONV #( 'null' )                           p3 = ' ' ).
*    set_json( p1 = 'incluirNomeColeta'    p2 = CONV #( 'false' )                          p3 = ' ' ).
*    set_json( p1 = 'imagemAssinaturaId'   p2 = CONV #( 'null' )                           p3 = ' ' ).
*    set_json( p1 = 'indiceExecucaoGrupo'  p2 = CONV #( 'null' )                           p3 = '@' ).
*    set_json( p1 = '}' ).
*    set_json( p1 = ']' ).
*
  endmethod.


  method SET_POSICAO_ASSINATURA_BRY.

*    DATA: lo_envio      TYPE REF TO zcl_integracao_bry_adigital,
*          lw_envio      TYPE zins_dados_bry_dados,
*          lw_sucesso    TYPE zins_dados_bry_json,
*          lw_erro       TYPE zins_dados_bry_json_erro,
*          lv_receitakey TYPE string,
*          lv_response   TYPE string.
*
*    FREE: me->at_bapiret2_tab.
*
*    lv_receitakey = me->at_zsdt0218-receitakey.
*
**-----------------------------
**-- recupera chave coleta
**-----------------------------
*    SELECT SINGLE *
*      FROM zsdt0218
*      INTO me->at_zsdt0218
*     WHERE numeroreceita  = me->at_zsdt0218-numeroreceita
*       AND numeropedido   = me->at_zsdt0218-numeropedido
*       AND cpfrt          = me->at_zsdt0218-cpfrt
*       AND receitakey     = me->at_zsdt0218-receitakey
*       AND cancelada      = abap_off.
*
*    CHECK me->at_zsdt0218-chave_workflow   IS NOT INITIAL.
*    CHECK me->at_zsdt0218-chave_documento  IS NOT INITIAL.
*    CHECK me->at_zsdt0218-chave_assinatura IS INITIAL.
*
**---------------------------
**-- montar JSON assinatura
**---------------------------
*    me->set_json_assinatura( ).
*
**---------------------------
**-- montar campos API
**---------------------------
**    lw_envio-metodo_envio                 = 'POST'.
**    lw_envio-endpoint_posicao_assina      = me->at_zsdt0218-chave_workflow.
**    lw_envio-endpoint_posicao_assina_json = me->at_json.
**    lw_envio-id_referencia                = me->at_zsdt0218-receitakey.
*
**---------------------------
**-- Envio BRY --------------
**---------------------------
*    TRY.
*        CREATE OBJECT lo_envio
*          EXPORTING
*            it_anexos     = it_anexos
*            i_dados_envio = lw_envio.
*
*        lv_response = lo_envio->zif_integracao_bry_adigital~enviar_bry( ).
*
*        IF lv_response CS 'status'.
*          /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*                                     CHANGING  data = lw_erro ).
*
*          me->set_message( i_message = lw_erro-mensagens[ 1 ]-mensagem i_doc_id = lv_receitakey ).
*          me->log_update( ).
*          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_erro-mensagens[ 1 ]-mensagem ).
*        ELSE.
*          /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*                                     CHANGING  data = lw_sucesso ).
**------------------
**-------- chave documento
**------------------
*          UPDATE zsdt0218 SET chave_assinatura = lw_sucesso-chaveworkflow
*                        WHERE numeroreceita    = me->at_zsdt0218-numeroreceita
*                          AND numeropedido     = me->at_zsdt0218-numeropedido
*                          AND cpfrt            = me->at_zsdt0218-cpfrt
*                          AND receitakey       = me->at_zsdt0218-receitakey.
*          COMMIT WORK.
*        ENDIF.
*
*        IF lw_erro IS NOT INITIAL.
*          me->set_message( i_message =  lw_erro-mensagens[ 1 ]-mensagem i_doc_id = lv_receitakey ).
*          me->log_update( ).
*          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_erro-mensagens[ 1 ]-mensagem ).
*        ENDIF.
*
*      CATCH zcx_integracao INTO DATA(ex_int).
*        me->set_message( i_message = ex_int->get_longtext( ) i_doc_id = lv_receitakey ).
*        me->log_update( ).
*        zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro Integração com a Bry' ).
*
*      CATCH zcx_error INTO DATA(ex_erro).
*        me->set_message( i_message = ex_erro->get_longtext( ) i_doc_id = lv_receitakey ).
*        me->log_update( ).
*        zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro Integração com a Bry' ).
*    ENDTRY.
*
  endmethod.


  METHOD set_system.

    me->at_system_id = '02'. " --> AgriQ

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    CHECK me->zif_integracao_inject~at_header_fields IS INITIAL.

    TRY .

        " debug
        IF sy-uname = 'RBLIMA' OR sy-sysid = 'DEV'.
          APPEND VALUE #( name = 'x-coupa-api-key' value = 'be82ead986a55cbb33155ab2c7661641307a8b8b' ) TO me->zif_integracao_inject~at_header_fields.
        ELSE.

          CAST zcl_integracao_token_coupa(
                 zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
                   )->get_token( )
               )->zif_integracao_inject~get_header_request_http(
            IMPORTING
              e_header_fields = DATA(e_header_fields) ).


          me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

        ENDIF.

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
            msgid  = ex_erro->zif_error~msgid
            msgno  = ex_erro->zif_error~msgno
            msgty  = 'E'
            msgv1  = ex_erro->zif_error~msgv1
            msgv2  = ex_erro->zif_error~msgv2
            msgv3  = ex_erro->zif_error~msgv3
            msgv4  = ex_erro->zif_error~msgv4.

    ENDTRY.

    APPEND VALUE #( name = 'Accept' value = 'application/xml' ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
