CLASS zcl_integ_rodv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_rodov .
    INTERFACES zif_integracao_inject .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEG_RODV IMPLEMENTATION.


  METHOD zif_integracao_inject~get_header_request_http.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_rodov~get_id_referencia.
  ENDMETHOD.


  METHOD zif_integracao_rodov~get_instance.
    IF zif_integracao_rodov~at_if_integracao_rodov IS NOT BOUND.
      CREATE OBJECT zif_integracao_rodov~at_if_integracao_rodov
        TYPE zcl_integ_rodv.
    ENDIF.
    r_if_integracao_rodv = zif_integracao_rodov~at_if_integracao_rodov.
  ENDMETHOD.


  METHOD zif_integracao_rodov~get_int_rod.

    TYPES BEGIN OF ty_retorno_token.
    TYPES: access_token TYPE string.
    TYPES: username TYPE string.
    TYPES END OF ty_retorno_token.

    DATA: tl_texto      TYPE catsxt_longtext_itab,
          tl_token      TYPE string,
          lc_token      TYPE ty_retorno_token,
          e_reason      TYPE string,
          json_retorno  TYPE string,
          i_url_destino TYPE string,
          i_url_token   TYPE string,
          i_signum      TYPE t5a4a-split VALUE '+',
          r_data        TYPE p0001-begda.

    DATA: gw_comp TYPE zpme0020.
    DATA: gt_comp TYPE TABLE OF zpme0020.

    DATA: http_client     TYPE REF TO if_http_client,
          return_code     TYPE i,
          v_service       TYPE /ui2/service_name,
          v_cdata_retorno TYPE string,
          v_url           TYPE ui_src_url.


    DATA: ob_web_service TYPE REF TO zcl_webservice.
    CREATE OBJECT ob_web_service.

    SELECT SINGLE * FROM zciot_webservice   INTO @DATA(ws_zciot_webservice) WHERE tipo EQ '8' AND servico = @i_servico.
    DATA(date_ini) = |{ me->zif_integracao_rodov~at_dt_chegada+6(2) }{ me->zif_integracao_rodov~at_dt_chegada+4(2) }{ me->zif_integracao_rodov~at_dt_chegada(4) }|.
    r_data = me->zif_integracao_rodov~at_dt_chegada.
*    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*      EXPORTING
*        date      = r_data
*        days      = 1
*        months    = 0
*        years     = 0
*        signum    = i_signum
*      IMPORTING
*        calc_date = r_data.

    DATA(date_fim) = |{ r_data+6(2) }{ r_data+4(2) }{ r_data(4) }|.


    DATA(url_get) = ws_zciot_webservice-url && date_ini && '/' && date_fim && '/' && me->zif_integracao_rodov~at_sig_terminal.
    DATA(var_http_get) = ob_web_service->url( EXPORTING i_url = CONV #( url_get ) ).

    TRY.
        i_url_destino = url_get.
        i_url_token   = ws_zciot_webservice-url_token.
        ob_web_service->zif_webservice~get_token_opus(
           EXPORTING
             i_url_destino              = i_url_destino
             i_url_token                = i_url_token
             i_servico                  = i_servico   "*-#160251-03.12.2024-#160251-JT-inicio
           IMPORTING
             e_token                    = DATA(e_token)
           EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state         = 2
             http_processing_failed     = 3
             http_invalid_timeout       = 4
             OTHERS                     = 5
         ).
      CATCH zcx_integracao INTO DATA(ex_integra).
        ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO DATA(ex_error).    "  "
        ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
*      construindo bearer
*    CONCATENATE 'Bearer' me->zif_integracao_rodov~at_token INTO DATA(bearer_token) SEPARATED BY space.
    DATA(bearer_token) = e_token.
    CALL METHOD var_http_get->request->set_header_field
      EXPORTING
        name  = 'Authorization'
        value = bearer_token.

    CALL METHOD var_http_get->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'GET'.

    CALL METHOD var_http_get->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json'.

    CALL METHOD var_http_get->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.

*    ob_web_service->zif_webservice~abrir_conexao( i_http = var_http_get ).

    TRY .
        CLEAR json_retorno.
        ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = var_http_get
            IMPORTING
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).


        IF json_retorno IS NOT INITIAL.
          FREE: gw_comp.
          /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = e_data ).

        ENDIF.

      CATCH zcx_integracao INTO ex_integra.
        ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO ex_error.    "  "
        ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_integracao_rodov~get_json.
    r_if_integracao_rodov = me.

*
*
*    me->zif_integracao_rodov~monta_json( EXPORTING i_dados = me->zif_integracao_rodov~at_combust RECEIVING e_json = DATA(lc_json) ).
*    e_json = lc_json.
  ENDMETHOD.


  METHOD zif_integracao_rodov~monta_json.

    DATA: wc_json  TYPE string,
          w_json   TYPE string,
          zqtde    TYPE sy-index,
          zvirgula TYPE char1.
    CLEAR: wc_json.

    CLEAR: zqtde.
    DESCRIBE TABLE i_dados LINES zqtde.

    LOOP AT i_dados ASSIGNING FIELD-SYMBOL(<_dados>).
* Construindo JSON.

      IF zqtde = sy-tabix.
        zvirgula = ' '.
      ELSE.
        zvirgula = ','.
      ENDIF.

*      wc_json = |{ wc_json }{ <_dados>-movi_codigo }{ zvirgula } |.
    ENDLOOP.

    e_json = |[ { wc_json } ]|.
  ENDMETHOD.


  METHOD zif_integracao_rodov~preparar_dados_gravacao.

    me->zif_integracao_rodov~at_ok = abap_false.

    LOOP AT me->zif_integracao_rodov~at_dados_descarga ASSIGNING FIELD-SYMBOL(<fs_dados_descarga>).

      IF <fs_dados_descarga>-id_registro IS INITIAL.
        "Gerar ID registro
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZREG_L1_WS'
          IMPORTING
            number                  = <fs_dados_descarga>-id_registro
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        IF ( sy-subrc NE 0 ) OR ( <fs_dados_descarga>-id_registro IS INITIAL ).
*          MESSAGE S008.
          RETURN.
        ENDIF.
      ENDIF.

      LOOP AT <fs_dados_descarga>-notas ASSIGNING FIELD-SYMBOL(<fs_dados_nf>).
        <fs_dados_nf>-id_registro = <fs_dados_descarga>-id_registro.
      ENDLOOP.

      <fs_dados_descarga>-srv_integracao = me->zif_integracao_rodov~at_srv_integracao.
      <fs_dados_descarga>-dt_registro = sy-datum.
      <fs_dados_descarga>-hr_registro = sy-uzeit.

    ENDLOOP.

    me->zif_integracao_rodov~at_ok = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_rodov~put_int_rod.
  ENDMETHOD.


  METHOD zif_integracao_rodov~salvar_dados.

    DATA: wl_zlest0174 TYPE zlest0174,
          it_zlest0174 TYPE TABLE OF zlest0174,
          wl_zlest0175 TYPE zlest0175,
          it_zlest0175 TYPE TABLE OF zlest0175.

    DATA: it_zlest0175_aux TYPE TABLE OF zde_zlest0175_tmp,
          wa_zlest0175_aux TYPE zde_zlest0175_tmp,
          it_zlest0174_aux TYPE TABLE OF zlest0174,
          wa_zlest0174_aux TYPE zlest0174.


    DATA: vg_input TYPE char01.
*    R_GRAVOU = ABAP_FALSE.

    IF ( me->zif_integracao_rodov~at_dados_descarga[] IS INITIAL ).
*      MESSAGE s009.
      RETURN.
    ENDIF.

    me->zif_integracao_rodov~preparar_dados_gravacao( ).

    CHECK me->zif_integracao_rodov~at_ok EQ abap_true.

    CLEAR: it_zlest0174[], it_zlest0175[].

    FREE: it_zlest0175_aux.
    LOOP AT me->zif_integracao_rodov~at_dados_descarga ASSIGNING FIELD-SYMBOL(<ws_descarga>).
      APPEND LINES OF <ws_descarga>-notas[] TO it_zlest0175_aux[].
    ENDLOOP.

    SELECT * FROM zlest0175 AS a
    INTO TABLE @DATA(tg_zlest0175)
    FOR ALL ENTRIES IN @it_zlest0175_aux[]
    WHERE chave_nf EQ @it_zlest0175_aux-chave_nf.

    LOOP AT me->zif_integracao_rodov~at_dados_descarga INTO DATA(_wl_descarga).

      LOOP AT _wl_descarga-notas INTO DATA(_wl_nf) WHERE id_registro = _wl_descarga-id_registro.
        READ TABLE tg_zlest0175 INTO DATA(ws_zles00175) WITH KEY chave_nf = _wl_nf-chave_nf.
        IF sy-subrc NE 0.
          CLEAR: wl_zlest0175.
          MOVE-CORRESPONDING _wl_nf TO wl_zlest0175.
          APPEND wl_zlest0175 TO it_zlest0175.
          CLEAR: ws_zles00175.
          vg_input = abap_true.
          ENDIF.
        ENDLOOP.

        IF vg_input EQ abap_true.
          CLEAR: wl_zlest0174.
          MOVE-CORRESPONDING _wl_descarga TO wl_zlest0174.
          APPEND wl_zlest0174 TO it_zlest0174.
        ENDIF.
      ENDLOOP.
      FREE: tg_zlest0175.

      IF ( it_zlest0174[] IS NOT INITIAL ).
        MODIFY zlest0174 FROM TABLE it_zlest0174.
      ENDIF.

      IF ( it_zlest0175[] IS NOT INITIAL ).
        MODIFY zlest0175 FROM TABLE it_zlest0175.
      ENDIF.

*    r_gravou = abap_true.

      COMMIT WORK.

    ENDMETHOD.


  METHOD zif_integracao_rodov~set_cnpj_terminal.
    zif_integracao_rodov~at_cnpj_terminal = i_cnpj_terminal.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_data_chegada.
    zif_integracao_rodov~at_dt_chegada = i_data_chegada.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_desc_terminal.
    zif_integracao_rodov~at_desc_terminal = i_text.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_ds_data.
    "Incluir Texto JSON para integração
    r_if_integracao_rodov = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_ds_url.

    r_if_integracao_rodv = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '8'
       AND servico EQ 'RV'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'RV'
                            attr2 = me->zif_integracao_rodov~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_integracao_rodov~at_servico ).
    ENDIF.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method'  value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'     value = 'application/json; charset=UTF-8' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Token'            value = wa_webservice-senha ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token          = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url && '01012022/10102022/R'.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'GET'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   =  ''.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_ds_url_post.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_id_referencia.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_int_dados.

    r_if_integracao_rodov = me.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_rodov~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_dados  ).
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_rodov = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_sigla_terminal.
    zif_integracao_rodov~at_sigla_terminal = i_sigla_terminal.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_sig_terminal.
    zif_integracao_rodov~at_sig_terminal = i_sig_terminal.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_srv_integracao.
    zif_integracao_rodov~at_srv_integracao = i_srv_integracao.
  ENDMETHOD.


  METHOD zif_integracao_rodov~set_token.

    TYPES BEGIN OF ty_retorno_token.
    TYPES: access_token TYPE string.
    TYPES: username TYPE string.
    TYPES END OF ty_retorno_token.



    DATA: ob_web_service TYPE REF TO zcl_webservice.
    CREATE OBJECT ob_web_service.

    DATA: lc_json            TYPE string,
          wc_json            TYPE string,
          e_reason           TYPE string,
          json_retorno       TYPE string,
          i_url_destino      TYPE string,
          i_url_token        TYPE string,
          lc_token           TYPE ty_retorno_token,
          ls_ztpm_token_s360 TYPE ztpm_token_s360.

    DATA: lt_ls_ztpm_token_s360 TYPE ztpm_token_s360.

    DATA: tl_texto      TYPE catsxt_longtext_itab.
    DATA: tl_token      TYPE string.

    SELECT SINGLE * FROM zciot_webservice INTO @DATA(ws_zciot_webservice) WHERE tipo EQ '8' AND servico EQ 'T9'.
    SELECT SINGLE * FROM zciot_webservice INTO @DATA(_ws_zciot_webservice) WHERE tipo EQ '8' AND servico EQ 'RV'.

    CLEAR: lc_json.
*    lc_json = '{"|grant_type=password&username":' && '"' && ws_zciot_webservice-usuario &&'",' && '"password":' && '"' && ws_zciot_webservice-senha && '"}'.
    DATA(text_form) = |grant_type=password&username={ ws_zciot_webservice-usuario }&password={ ws_zciot_webservice-senha }|.

*   Acessando a classe WEBSERVICE para validar o tipo de serviço cadastrado ZLES0096
    TRY .
        ob_web_service->set_servico( i_servico = 'T9' ).
        ob_web_service->set_tipo( i_tipo = '8' ).
      CATCH zcx_webservice INTO DATA(lc_exception).
    ENDTRY.


    TRY .
        DATA(var_http) = ob_web_service->url( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        DATA(lc_uri) = ob_web_service->get_uri(  ).
      CATCH zcx_webservice INTO lc_exception.
    ENDTRY.


    TRY.
        ob_web_service->zif_webservice~abrir_conexao( i_http = var_http ).
        ob_web_service->zif_webservice~consultar(
            EXPORTING
              i_http                     = var_http
              i_xml                      = text_form
            IMPORTING
              e_code                     = DATA(e_code)
              e_reason                   = e_reason
            RECEIVING
              e_resultado                = json_retorno
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5
          ).

        CHECK json_retorno IS NOT INITIAL.
        CLEAR: lc_token.
        /ui2/cl_json=>deserialize( EXPORTING json = json_retorno CHANGING data = lc_token ).


        IF lc_token IS NOT INITIAL.
          me->zif_integracao_rodov~at_token = lc_token-access_token.
        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_integra).
        ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO DATA(ex_error).    "  "
        ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_integracao_rodov~trata_retorno.

    "Inicio USER STORY 74070 - Anderson Oenning / 02/05/2022
    TYPES: BEGIN OF ty_notafiscal,
             numeronotafiscal     TYPE string,
             serienotafiscal      TYPE string,
             dataemissao          TYPE string,
             quantidade           TYPE string,
             valor                TYPE string,
             descricaoobservacao  TYPE string,
             entreganfi           TYPE string,
             numerocfop           TYPE string,
             codigochaveacessonfe TYPE string,
             notaeletronica       TYPE string,
           END OF ty_notafiscal.

    TYPES: BEGIN OF ty_tgg,
             in_protocolo_integracao(100),
             nr_cnpj_empresa(14),
             dt_hora_chegada(08),
             dt_abertura(08),
             dt_fechamento(08),
             nr_cpf_motorista(11),
             ds_placa(7),
             nr_peso_bruto                TYPE zde_peso_bruto,
             nr_peso_tara                 TYPE zde_peso_tara,
             nr_peso_liquido              TYPE zde_peso_liq_total,
             nr_codigo_chave_nfe          TYPE zde_chave_nf,
             nr_nota_fiscal               TYPE j_1bnfnum9,
             id_serie_nota_fiscal         TYPE j_1bseries,
             dt_emissao_nota_fiscal(08),
             nr_quantidade_nota_fiscal    TYPE zde_peso_descarga,
             nr_peso_subtotal             TYPE zde_peso_descarga,
             notafiscal                   TYPE ty_notafiscal,
             pesonf                       TYPE zde_peso_liq_total,
             TIPOPRODUTO                  TYPE ZDE_TP_TRANSGENIA,
           END OF ty_tgg.

    DATA it_tgg TYPE TABLE OF ty_tgg.
    DATA wa_tgg TYPE ty_tgg.

    DATA: return_code      TYPE i,
          e_resultado      TYPE string,
          v_node_name      TYPE string,
          v_node_value     TYPE string,
          v_msg_show       TYPE c LENGTH 200,

          it_zlest0174     TYPE TABLE OF zlest0174,

          it_zlest0174_tmp TYPE TABLE OF zde_zlest0174_tmp,
          wl_zlest0174_tmp TYPE zde_zlest0174_tmp,

          it_zlest0175_tmp TYPE TABLE OF zde_zlest0175_tmp,
          wl_zlest0175_tmp TYPE zde_zlest0175_tmp,

          v_cont_reg_tmp   TYPE zlest0174-id_registro,

          v_protocolo_rec  TYPE zlest0174-protocolo_rec,
          v_recebedor_cnpj TYPE zlest0174-recebedor_cnpj,
          v_recebedor_name TYPE zlest0174-recebedor_name.

    CLEAR:   v_protocolo_rec, v_recebedor_cnpj, v_recebedor_name.
    CLEAR:   v_cont_reg_tmp,
             it_zlest0174_tmp[], wl_zlest0174_tmp,
             it_zlest0175_tmp[], wl_zlest0175_tmp.


    IF i_dados IS NOT INITIAL.
      LOOP AT i_dados ASSIGNING FIELD-SYMBOL(<ls_dados>).

*-#137148-01.04.2024-JT-inicio
        CHECK <ls_dados>-cnpjempresa = zif_integracao_rodov~at_cnpj_terminal.
*-#137148-01.04.2024-JT-fim

        wa_tgg-in_protocolo_integracao         =  <ls_dados>-protocolointegracao.
        wa_tgg-nr_cnpj_empresa                 = <ls_dados>-cnpjempresa.

        IF <ls_dados>-datahorachegada IS NOT INITIAL.
          wa_tgg-dt_hora_chegada                 = |{ <ls_dados>-datahorachegada+6(4) }{ <ls_dados>-datahorachegada+3(2) }{ <ls_dados>-datahorachegada(2) }|. "14/01/2021
        ENDIF.

        IF <ls_dados>-dataabertura IS NOT INITIAL.
          wa_tgg-dt_abertura                     = |{ <ls_dados>-dataabertura+6(4) }{ <ls_dados>-dataabertura+3(2) }{ <ls_dados>-dataabertura(2) }|.
        ENDIF.

        IF <ls_dados>-datafechamento IS NOT INITIAL.
          wa_tgg-dt_fechamento                   = |{ <ls_dados>-datafechamento+6(4) }{ <ls_dados>-datafechamento+3(2) }{ <ls_dados>-datafechamento(2) }|.
        ENDIF.

*---------------------------------------------------Inicio CS2024000433 / aoenning.
        wa_tgg-TIPOPRODUTO                     = <ls_dados>-tipoProduto.
*---------------------------------------------------Fim CS2024000433 / aoenning.

        wa_tgg-nr_cpf_motorista                = <ls_dados>-cpfmotorista.
        wa_tgg-ds_placa                        = <ls_dados>-placa.
        wa_tgg-nr_peso_bruto                   = <ls_dados>-numeropesobruto.
        wa_tgg-nr_peso_tara                    = <ls_dados>-numeropesotara.
        wa_tgg-nr_peso_liquido                 = <ls_dados>-numeropesoliquido.
        wa_tgg-nr_codigo_chave_nfe             = <ls_dados>-notafiscal-codigochaveacessonfe.
        wa_tgg-nr_nota_fiscal                  = <ls_dados>-notafiscal-numeronotafiscal.
        wa_tgg-id_serie_nota_fiscal            = <ls_dados>-notafiscal-serienotafiscal.
        wa_tgg-pesonf                          = <ls_dados>-notafiscal-quantidade.

        IF <ls_dados>-notafiscal-dataemissao IS NOT INITIAL.
          wa_tgg-dt_emissao_nota_fiscal          = |{ <ls_dados>-notafiscal-dataemissao+6(4) }{ <ls_dados>-notafiscal-dataemissao+3(2) }{ <ls_dados>-notafiscal-dataemissao(2) }|.
        ENDIF.

        wa_tgg-nr_quantidade_nota_fiscal       = <ls_dados>-quantidade.
        wa_tgg-nr_peso_subtotal                = <ls_dados>-numeropesosubtotal.

        APPEND wa_tgg TO it_tgg.
        CLEAR: wa_tgg.
      ENDLOOP.
    ENDIF.
*
*    IF it_tgg IS NOT INITIAL.
*      SELECT *
*        FROM zlest0174
*        INTO TABLE it_zlest0174
*        FOR ALL ENTRIES IN it_tgg
*        WHERE protocolo_rec = it_tgg-in_protocolo_integracao.
*      SORT it_zlest0174 BY protocolo_rec.
*    ELSE.
*      RETURN.
*    ENDIF.

    LOOP AT it_tgg INTO wa_tgg.
*      READ TABLE it_zlest0174 INTO DATA(w_0174) WITH KEY protocolo_rec = wa_tgg-in_protocolo_integracao.
*      IF sy-subrc = 0.
*        CONTINUE.
*      ENDIF.
      "Gerar ID Temporario
      ADD 1 TO v_cont_reg_tmp.
      wl_zlest0174_tmp-id_tmp                               = v_cont_reg_tmp.
      "
      wl_zlest0174_tmp-srv_integracao                       = me->zif_integracao_rodov~at_srv_integracao.
      wl_zlest0174_tmp-protocolo_rec                        = wa_tgg-in_protocolo_integracao.
      wl_zlest0174_tmp-sigla_terminal_transb                = me->zif_integracao_rodov~at_sigla_terminal.
      wl_zlest0174_tmp-terminal_transb                      = me->zif_integracao_rodov~at_desc_terminal.
      wl_zlest0174_tmp-cnpj_terminal_transb                 = wa_tgg-nr_cnpj_empresa.

*---------------------------------------------------Inicio CS2024000433 / aoenning.
      wl_zlest0174_tmp-TP_TRANSGENIA                      = wa_tgg-TIPOPRODUTO.
*---------------------------------------------------Fim CS2024000433 / aoenning.



      CLEAR: wl_zlest0174_tmp-terminal_transb, wl_zlest0174_tmp-ds_terminal_transb.

*-#137148-01.04.2024-JT-inicio
      SELECT name1 ort01
        INTO (wl_zlest0174_tmp-terminal_transb, wl_zlest0174_tmp-ds_terminal_transb)
       UP TO 1 ROWS
        FROM lfa1
       WHERE stcd1 = wl_zlest0174_tmp-cnpj_terminal_transb.
      ENDSELECT.
*-#137148-01.04.2024-JT-fim

      wl_zlest0174_tmp-nm_fantasia_terminal_destino         = abap_off. "me->zif_integracao_rodov~at_desc_terminal.
      wl_zlest0174_tmp-rz_social_terminal_destino           = abap_off. "me->zif_integracao_rodov~at_desc_terminal.
      wl_zlest0174_tmp-cnpj_terminal_destino                = abap_off.  "' '.
      wl_zlest0174_tmp-dt_chegada                           = wa_tgg-dt_hora_chegada.
      wl_zlest0174_tmp-dt_entrada                           = wa_tgg-dt_abertura.
      wl_zlest0174_tmp-dt_saida                             = wa_tgg-dt_fechamento.
      wl_zlest0174_tmp-nome_motorista                       = wa_tgg-nr_cpf_motorista.
      wl_zlest0174_tmp-placa                                = wa_tgg-ds_placa.
      wl_zlest0174_tmp-peso_bruto                           = wa_tgg-nr_peso_bruto.
      wl_zlest0174_tmp-peso_tara                            = wa_tgg-nr_peso_tara.
      wl_zlest0174_tmp-peso_liquido                         = wa_tgg-nr_peso_liquido.
      APPEND wl_zlest0174_tmp TO it_zlest0174_tmp.
      "
      wl_zlest0175_tmp-id_tmp                               = v_cont_reg_tmp.
      wl_zlest0175_tmp-chave_nf                             = wa_tgg-nr_codigo_chave_nfe.
      wl_zlest0175_tmp-model                                = '55'.
      wl_zlest0175_tmp-nfenum                               = wa_tgg-nr_nota_fiscal.
      wl_zlest0175_tmp-series                               = wa_tgg-id_serie_nota_fiscal.
      wl_zlest0175_tmp-stcd1                                = wl_zlest0175_tmp-chave_nf+6(14).
      wl_zlest0175_tmp-docdat                               = wa_tgg-dt_emissao_nota_fiscal.
      wl_zlest0175_tmp-docnum                               = ''.
      wl_zlest0175_tmp-peso_declarado                       = wa_tgg-pesonf.
      wl_zlest0175_tmp-peso_descarga                        = wa_tgg-nr_peso_subtotal.
      APPEND wl_zlest0175_tmp TO it_zlest0175_tmp.
      CLEAR: wl_zlest0175_tmp, wl_zlest0175_tmp.
    ENDLOOP.

    "Processar Dados
    LOOP AT it_zlest0174_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0174_tmp>).
      "Carregar Notas
      LOOP AT it_zlest0175_tmp ASSIGNING FIELD-SYMBOL(<fs_zlest0175_tmp>) WHERE id_tmp EQ <fs_zlest0174_tmp>-id_tmp.
        APPEND <fs_zlest0175_tmp> TO <fs_zlest0174_tmp>-notas.
      ENDLOOP.
    ENDLOOP.

    me->zif_integracao_rodov~at_dados_descarga[] = it_zlest0174_tmp[].

    me->zif_integracao_rodov~at_ok = abap_true.



    "Fim USER STORY 74070 - Anderson Oenning / 02/05/2022
  ENDMETHOD.
ENDCLASS.
