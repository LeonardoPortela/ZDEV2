class ZCL_COUPA_DOCUMENTS_STEP_01 definition
  public
  inheriting from ZCL_DEFAULT_DOCUMENTS_STEP_01
  final
  create public .

public section.

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
  methods LOG_API_SEND
    redefinition .
  methods SEND_PROCESS_LOG
    redefinition .
  methods SEND_TO_APPROVAL
    redefinition .
  methods SET_MESSAGE
    redefinition .
  methods SET_SYSTEM
    redefinition .
  methods ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG
    redefinition .
protected section.
private section.

  data AT_DOC_TAB type ZINS_ADIGITAL_CONTRATOS .
  data GV_CURRENT_ID type ZIN_ID_REFERENCIA .
  data GV_PARAMS type STRING .
  data AT_ASSINANTES_STRING type STRING .
  data AT_ANEXOS_STRING type STRING .

  methods CHECK_CPF
    importing
      !IV_CPF type MSGXX
    returning
      value(RV_INVALIDO) type STRING .
  methods REMOVE_TAGS_CONF
    changing
      !CV_XML type STRING .
  methods SEND_EMAIL
    importing
      !I_LOG_TAB type ZINC_ASSINA02 .
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
      !I_DATE type STRING optional
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
  methods GET_DAYS_TO_SEARCH
    returning
      value(R_DAYS) type INT4 .
  methods FORMATA_ASSINATURA
    importing
      !IV_IN type STRING
    returning
      value(RV_OUT) type STRING .
  methods REMOVE_TAG
    importing
      !IV_TAG type STRING
    changing
      !CV_XML type STRING .
  methods GET_CONTRACT_OFFSET
    importing
      !IV_PARAMS type STRING
    returning
      value(RT_CONTRACTS) type ZINS_ADIGITAL_CONTRATOS
    raising
      ZCX_ERROR .
ENDCLASS.



CLASS ZCL_COUPA_DOCUMENTS_STEP_01 IMPLEMENTATION.


  METHOD after_sending.

    TRY.

        DATA lv_body TYPE string.
        DATA lv_text TYPE string.

        CHECK i_adigital IS NOT INITIAL.

        MODIFY zint_assina01 FROM i_adigital.

      CATCH zcx_integracao .
      CATCH zcx_error .
    ENDTRY.

  ENDMETHOD.


  METHOD before_sending.

    TRY.

        "IF 1 = 2."TESTE
        r_attachments = me->get_attachments( i_reference ).
        "ELSE.
        DATA(lw_attach_normal) = me->get_attachments2( i_reference ).
        "ENDIF.

        IF lw_attach_normal-attachments-attachment IS NOT INITIAL.
          APPEND LINES OF lw_attach_normal-attachments-attachment TO r_attachments-attachments-attachment.
        ENDIF.


        LOOP AT r_attachments-attachments-attachment ASSIGNING FIELD-SYMBOL(<fs_attach>).

          at_anexos_string = at_anexos_string && ';' && <fs_attach>-file_name.

        ENDLOOP.

        at_anexos_string = at_anexos_string+1.

      CATCH zcx_integracao .
      CATCH zcx_error .

    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_coupa_adigital.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    me->at_service = 'COUPA_INT_ASSINATURA_DIGITAL'.

  ENDMETHOD.


  METHOD convert_boolean.

    DATA lv_param TYPE string.

    IF iv_param IS INITIAL.
      rv_return = 'false'.
      EXIT.
    ENDIF.

    lv_param = iv_param.

    TRANSLATE lv_param TO UPPER CASE.

    lv_param = zcl_string2=>remove_spec_char( lv_param ).

    IF lv_param = 'NAO' OR lv_param = 'FALSE'.
      rv_return = 'false'.
      EXIT.
    ENDIF.

    IF lv_param = 'SIM' OR lv_param = 'TRUE'.
      rv_return = 'true'.
      EXIT.
    ENDIF.

  ENDMETHOD.


  method cpf_by_email.

    data lv_mess type string.

    data: lv_cpf type string,
          zemail type string.

    clear r_cpf.

    if i_email = 'daniel.filippo@pwc.com' and sy-sysid = 'QAS'.
      r_cpf = '15706790701'.
      exit.
    endif.

    if i_email = 'ramon.lima@reclike.com.br' and sy-sysid = 'QAS'.
      r_cpf = '05332137974'.
      exit.
    endif.

    check i_email is not initial.

    select single cpf_nr from zhcmt0007
      into lv_cpf
        where email_ad = i_email.
**Ajuste referente melhoria USER STORY 172605 / AOENNING
    if sy-subrc ne 0.
      clear: zemail.
      zemail = i_email.
      translate zemail to upper case. "Transformar variavel em maiusculos para pesquisar na tabela.
      select single cpf_nr from zhcmt0007
      into lv_cpf
        where email_ad = zemail.
    endif.
**Ajuste referente melhoria USER STORY 172605 / AOENNING

    if sy-subrc eq 0.

      replace all occurrences of '.' in lv_cpf with space.
      replace all occurrences of '-' in lv_cpf with space.

      condense lv_cpf no-gaps.

      r_cpf = lv_cpf.

    else.

      lv_mess = `CPF para o email: ` && i_email && ` não foi encontrado`.

      "zcx_error=>zif_error~gera_erro_geral( i_texto = lv_mess ).

      me->set_message( lv_mess ).

    endif.

  endmethod.


  METHOD formata_assinatura.

    CHECK iv_in IS NOT INITIAL.

    rv_out = iv_in.

    REPLACE ALL OCCURRENCES OF REGEX `[\t\v\n\r]` IN rv_out WITH ';'.
    REPLACE ALL OCCURRENCES OF ',' IN rv_out WITH '#'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_out WITH space.

    WHILE rv_out CP '*;;*'.
      REPLACE ALL OCCURRENCES OF ';;' IN rv_out WITH ';'.

      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

    ENDWHILE.

    WHILE rv_out CP '*##*'.

      REPLACE ALL OCCURRENCES OF '##' IN rv_out WITH '#'.


      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

    ENDWHILE.

    DATA(lv_tam) = strlen( rv_out ).

    lv_tam = lv_tam - 1.

    CHECK lv_tam > 0.

    IF rv_out+lv_tam(1) NE ';'.

      rv_out = rv_out && ';'.

    ENDIF.

    REPLACE 'ASSINATURA_ELETRONICAr' IN rv_out WITH 'ASSINATURA_ELETRONICA'.

  ENDMETHOD.


  METHOD format_date_bry.

    DATA lv_datum TYPE sy-datum.
    DATA lv_uzeit TYPE syuzeit.
    DATA lv_string TYPE c LENGTH 20.
    DATA lv_inicial TYPE c.

    DATA(lv_date) = i_date.

    IF i_date IS INITIAL.

      lv_datum = sy-datum.
      lv_uzeit = sy-uzeit.

    ELSE.

      TRY .
          lv_datum = i_date(8).
        CATCH cx_sy_range_out_of_bounds.
          lv_datum = sy-datum.
      ENDTRY.

      TRY .
          lv_uzeit = i_date+8(6).
        CATCH cx_sy_range_out_of_bounds.
          lv_uzeit = sy-uzeit.
      ENDTRY.

      IF lv_uzeit IS INITIAL.
        lv_uzeit = sy-uzeit.
      ENDIF.

    ENDIF.

    lv_date = '&1-&2-&3T&4:&5:&6-04:00'.

    REPLACE '&1' IN lv_date WITH lv_datum(4).
    REPLACE '&2' IN lv_date WITH lv_datum+4(2).
    REPLACE '&3' IN lv_date WITH lv_datum+6(2).
    REPLACE '&4' IN lv_date WITH lv_uzeit(2).
    REPLACE '&5' IN lv_date WITH lv_uzeit+2(2).
    REPLACE '&6' IN lv_date WITH lv_uzeit+4(2).

    REPLACE FIRST OCCURRENCE OF '-' IN lv_date WITH space.
    REPLACE FIRST OCCURRENCE OF '-' IN lv_date WITH space.
    REPLACE ALL OCCURRENCES OF ':' IN lv_date WITH space.

    CONDENSE lv_date NO-GAPS.

    r_date = lv_date.

  ENDMETHOD.


  METHOD get_assinantes.

    DATA lv_params TYPE string.
    DATA lv_erro TYPE c.
    DATA lv_message TYPE string.

    DATA lo_xml TYPE REF TO cl_xml_document.

    CHECK i_id IS NOT INITIAL.

    TRY.

        lv_params = 'lookup_values/' && i_id.

        DATA(lw_int) = me->execute_service( i_params = lv_params i_method = 'GET').

        CHECK lw_int-nm_code = 200 OR lw_int-nm_code = 201 OR lw_int-nm_code = 202.

        CREATE OBJECT lo_xml.

        CHECK lo_xml->parse_xstring( lw_int-ds_data_xstring ) = 0.

        DATA(lv_rel_assin) = lo_xml->find_simple_element( 'relacao-assinantes' ).

        CHECK lv_rel_assin IS NOT INITIAL.

        r_relacao = formata_assinatura( lv_rel_assin ).

*        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_rel_assin WITH space.
*
*        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_rel_assin WITH ';'.
*
**        REPLACE ALL OCCURRENCES OF `"` IN lv_rel_assin WITH space.
**        REPLACE ALL OCCURRENCES OF `'` IN lv_rel_assin WITH space.
**
**        REPLACE ALL OCCURRENCES OF ',' IN lv_rel_assin WITH '#'.
**        REPLACE ALL OCCURRENCES OF `# ` IN lv_rel_assin WITH '#'.
**        REPLACE ALL OCCURRENCES OF `#;` IN lv_rel_assin WITH ';'.
*
*        IF lv_rel_assin NA ';'.
*          r_relacao = lv_rel_assin && ';'.
*        ELSE.
*          r_relacao = lv_rel_assin.
*        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->set_message( ex_int->get_longtext( ) ).

      CATCH zcx_error INTO DATA(ex_erro).
        me->set_message( ex_erro->get_longtext( ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_assinantes_extras.

    DATA lv_tem_comprador TYPE flag." 08.11.2024 - RAMON - 154395
    DATA lv_mess TYPE string.
    DATA lo_xml TYPE REF TO cl_xml_document.
    DATA lv_email TYPE string.
    DATA lv_cpf   TYPE string.
    DATA lv_nome TYPE string.

    TRY .

        IF iw_dados_contrato-customfields-compradorresponsvel IS NOT INITIAL.

          " ------------------ DADOS DO COMPRADOR
          IF iw_dados_contrato-customfields-compradorresponsvel-customfields-email IS INITIAL.

            DATA(lw_lookup) = me->lookup_values_execute( iw_dados_contrato-customfields-compradorresponsvel-id ).

            IF lw_lookup-nm_code < 206.

              CREATE OBJECT lo_xml.

              IF lo_xml->parse_xstring( lw_lookup-ds_data_xstring ) = 0.
                lv_email = lo_xml->find_simple_element( 'email' ).
              ENDIF.

            ENDIF.

          ELSE.

            lv_email = iw_dados_contrato-customfields-compradorresponsvel-customfields-email.

          ENDIF.

          " busca no HR
          lv_cpf = me->cpf_by_email( lv_email ).

          IF lv_email IS INITIAL OR lv_cpf IS INITIAL.

            lv_mess = `Contrato: ` && iw_dados_contrato-id && `, sem dados de email para comprador`.

            me->set_message( lv_mess ).

          ELSE.

            lv_tem_comprador = abap_true." 08.11.2024 - RAMON - 154395

            r_relacao =  lv_cpf && '#' && iw_dados_contrato-customfields-compradorresponsvel-name
                         && '#' && lv_email && '#' && 'ASSINATURA_ELETRONICA' && '#.#true#false;'. "'CERTIFICADO_DIGITAL' && ';'." 08.11.2024 - RAMON - 154395

          ENDIF.

        ELSE.

          " ------------------ DADOS DO CREATED-BY

          lv_email = iw_dados_contrato-createdby-email.
          lv_cpf = me->cpf_by_email( lv_email ).
          lv_nome = iw_dados_contrato-createdby-fullname.

          IF lv_cpf IS NOT INITIAL.

            r_relacao =  r_relacao && lv_cpf && '#' && lv_nome
                                   && '#' && lv_email && '#' && 'ASSINATURA_ELETRONICA' && '#.#true#false;'. "'CERTIFICADO_DIGITAL' && ';'." 08.11.2024 - RAMON - 154395 -->

          ENDIF.

        ENDIF.

        " 08.11.2024 - RAMON - 154395 -->

        " ------------------ DADOS ESCRITORIO DE CONTRATO

        IF iw_dados_contrato-customfields-escritoriodecontrato IS NOT INITIAL.

          IF iw_dados_contrato-customfields-escritoriodecontrato-customfields-email IS INITIAL.

            lw_lookup = me->lookup_values_execute( iw_dados_contrato-customfields-escritoriodecontrato-id ).

            IF lw_lookup-nm_code < 206.

              CREATE OBJECT lo_xml.

              IF lo_xml->parse_xstring( lw_lookup-ds_data_xstring ) = 0.
                lv_email = lo_xml->find_simple_element( 'email' ).
              ENDIF.

            ENDIF.

          ELSE.

            lv_email = iw_dados_contrato-customfields-escritoriodecontrato-customfields-email.

          ENDIF.

          " busca no HR
          lv_cpf = me->cpf_by_email( lv_email ).

          IF lv_email IS INITIAL OR lv_cpf IS INITIAL.

            lv_mess = `Contrato: ` && iw_dados_contrato-id && `, sem dados de email para comprador`.

            me->set_message( lv_mess ).

          ELSE.

            IF lv_tem_comprador = abap_true.

              r_relacao =  r_relacao && lv_cpf && '#' && iw_dados_contrato-customfields-escritoriodecontrato-name
                           && '#' && lv_email && '#' && 'ASSINATURA_ELETRONICA' && '#.#false#false;'. "'CERTIFICADO_DIGITAL' && ';'
            ELSE.
              r_relacao =  r_relacao && lv_cpf && '#' && iw_dados_contrato-customfields-escritoriodecontrato-name
             && '#' && lv_email && '#' && 'ASSINATURA_ELETRONICA' && '#.#true#false;'. "'CERTIFICADO_DIGITAL' && ';'
            ENDIF.

          ENDIF.

        ENDIF.

        " 08.11.2024 - RAMON - 154395 --<

        " ------------------ DADOS DO GESTOR CONTRATO

        lv_email = iw_dados_contrato-customfields-gestorcontrato-email.

        lv_cpf = me->cpf_by_email( lv_email ).
        lv_nome = iw_dados_contrato-customfields-gestorcontrato-fullname.

        IF lv_cpf IS NOT INITIAL.

          r_relacao =  r_relacao && lv_cpf && '#' && lv_nome
                                 && '#' && lv_email && '#' && 'ASSINATURA_ELETRONICA' && '#.#true#false;'. "'CERTIFICADO_DIGITAL' && ';'." 08.11.2024 - RAMON - 154395 -->

        ENDIF.

        " ------------------ DADOS RESPONSAVEL JURIDICO
        DATA(lv_juridico) = iw_dados_contrato-customfields-responsaveljuridico-customfields-relacaoassinantes.

        r_relacao = r_relacao && formata_assinatura( lv_juridico ).

*        IF lv_juridico IS NOT INITIAL.
*
*          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_juridico WITH ';'.
*
*          REPLACE ALL OCCURRENCES OF `"` IN lv_juridico WITH space.
*          REPLACE ALL OCCURRENCES OF `'` IN lv_juridico WITH space.
*
*          REPLACE ALL OCCURRENCES OF ',' IN lv_juridico WITH '#'.
*          REPLACE ALL OCCURRENCES OF `# ` IN lv_juridico WITH '#'.
*          REPLACE ALL OCCURRENCES OF `#;` IN lv_juridico WITH space.
*          r_relacao = r_relacao && lv_juridico.
*
*        ENDIF.

        " ------------------ DADOS SUPPLIER
*        DATA(lv_supplier) = iw_dados_contrato-supplier-customfields-relaodeassinantes.  "IR157154
        DATA(lv_supplier) = iw_dados_contrato-supplier-customfields-relacaodeassinantes. "IR157154
*-CS1082160-#RIMINI-05.03.2023-BEGIN
        REPLACE ALL OCCURRENCES OF 'Eamp;' IN lv_supplier WITH '&'.
*-CS1082160-#RIMINI-05.03.2023-END
        r_relacao = r_relacao && formata_assinatura( lv_supplier ).

*        IF lv_supplier IS NOT INITIAL.
*
*          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_supplier WITH ';'.
*
*          REPLACE ALL OCCURRENCES OF `"` IN lv_supplier WITH space.
*          REPLACE ALL OCCURRENCES OF `'` IN lv_supplier WITH space.
*
*          REPLACE ALL OCCURRENCES OF ',' IN lv_supplier WITH '#'.
*          REPLACE ALL OCCURRENCES OF `# ` IN lv_supplier WITH '#'.
*          REPLACE ALL OCCURRENCES OF `AL#` IN lv_supplier WITH 'AL'.
*          REPLACE ALL OCCURRENCES OF `CA#` IN lv_supplier WITH 'CA'.
*
*          REPLACE ALL OCCURRENCES OF `AL;` IN lv_supplier WITH 'AL'.
*          REPLACE ALL OCCURRENCES OF `CA;` IN lv_supplier WITH 'CA'.
*
*          REPLACE ALL OCCURRENCES OF `AL` IN lv_supplier WITH 'AL;'.
*          REPLACE ALL OCCURRENCES OF `CA` IN lv_supplier WITH 'CA;'.
*
*          r_relacao = r_relacao && lv_supplier.
*
*        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->set_message( i_message = ex_int->get_longtext( ) i_doc_id =  iw_dados_contrato-id ).

      CATCH zcx_error INTO DATA(ex_erro).
        me->set_message( i_message = ex_erro->get_longtext( ) i_doc_id =  iw_dados_contrato-id ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_attachments.

    DATA lv_text TYPE string.

    DATA t_element_array  TYPE zde_element_array_t.

    gv_params = 'contracts/' && i_reference && '/retrieve_legal_agreement'.

    TRY.

        DATA(lw_return) = me->execute_service( i_params = gv_params i_method = 'GET' ).

        IF lw_return-nm_code > 206.

          lv_text = `Requisição de legal agreement: ` && i_reference && 'falhou'.

          me->set_message( lv_text ).

          EXIT.

        ENDIF.

        "APPEND 'attachment' TO t_element_array.

        "zcl_string2=>xml_to_table( EXPORTING i_xml = lw_return-ds_data_retorno i_element_array = t_element_array IMPORTING r_data = r_attachments ).

        APPEND INITIAL LINE TO r_attachments-attachments-attachment ASSIGNING FIELD-SYMBOL(<fs_attach>).

        <fs_attach>-id = '001'.
        <fs_attach>-createdat = format_date_bry( ).
        <fs_attach>-updatedat = format_date_bry( ).
        <fs_attach>-type = 'AttachmentFile'.
        <fs_attach>-intent = 'Internal'.
        <fs_attach>-file = 'C:/temp/file.pdf'.
        <fs_attach>-fileurl = 'htps://temp/file.pdf'.
        <fs_attach>-file_name = 'file.pdf'.
        <fs_attach>-application_type = 'application/pdf'.
        <fs_attach>-xfile = lw_return-ds_data_xstring.

        message_length( <fs_attach>-xfile ).

      CATCH zcx_integracao INTO DATA(ex_int).
        me->set_message( ex_int->get_longtext( ) ).
      CATCH zcx_error INTO DATA(ex_erro).
        me->set_message( ex_erro->get_longtext( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_attachments2.

    DATA lv_text TYPE string.

    DATA t_element_array  TYPE zde_element_array_t.

    gv_params = 'contracts/' && i_reference && '/attachments'.

    TRY.
        " // RECUPERA RELAÇÃO DE ANEXOS --
        DATA(lw_return) = me->execute_service( i_params = gv_params i_method = 'GET' ).

        IF lw_return-nm_code > 206.

          "lv_text = 'Requisição de anexos do contrato:' && i_reference && 'falhou'.

          "zcx_error=>zif_error~gera_erro_geral( i_texto =  lv_text ).

        ENDIF.

        APPEND 'attachment' TO t_element_array.

        zcl_string2=>xml_to_table( EXPORTING i_xml = lw_return-ds_data_retorno i_element_array = t_element_array IMPORTING r_data = r_attachments ).

        LOOP AT r_attachments-attachments-attachment ASSIGNING FIELD-SYMBOL(<fs_anexo>).

          <fs_anexo>-file = zcl_string2=>remove_spec_char( <fs_anexo>-file ).

          <fs_anexo>-fileurl = zcl_string2=>remove_spec_char( <fs_anexo>-fileurl ).

          zcl_string2=>file_properties( EXPORTING i_file_path = <fs_anexo>-fileurl IMPORTING e_application_type = <fs_anexo>-application_type e_file_name = <fs_anexo>-file_name ).

          CHECK <fs_anexo>-id IS NOT INITIAL AND <fs_anexo>-xfile IS INITIAL.

          gv_params = 'contracts/' && i_reference && '/attachments/' && <fs_anexo>-id.

          <fs_anexo>-xfile = me->execute_service( i_params = gv_params i_method = 'GET' )-ds_data_xstring.

          message_length( <fs_anexo>-xfile ).

        ENDLOOP.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->set_message( ex_int->get_longtext( ) ).
        EXIT.

      CATCH zcx_error INTO DATA(ex_erro).
        me->set_message( ex_erro->get_longtext( ) ).
        EXIT.

    ENDTRY.

  ENDMETHOD.


  METHOD get_contract_url.

    DATA(lv_rev) = me->at_auth_ws-url.

    REPLACE 'api/' IN lv_rev WITH space.

    r_url = lv_rev && 'contracts/show/' && id_contrato && '#summary'.

  ENDMETHOD.


  METHOD get_days_to_search.

    CLEAR r_days.

    " foi mudado por causa do set, pq só podia CPF e pode ser CNPJ também
    SELECT SINGLE low FROM tvarvc
      INTO @DATA(lv_days)
        WHERE name = 'COUPA_AD_INTERVALO_DIAS'.

    IF lv_days IS NOT INITIAL.

      TRY.
          r_days = lv_days.
        CATCH cx_sy_conversion_no_number.
          r_days = 60.
      ENDTRY.

    ENDIF.

    IF r_days IS INITIAL.
      r_days = 60.
    ENDIF.

  ENDMETHOD.


  METHOD get_documents.

    DATA lv_date TYPE sy-datum.

    DATA t_element_array  TYPE zde_element_array_t.

    CLEAR me->at_bapiret2_tab.

    lv_date = sy-datum.

*    DATA(lv_days) = get_days_to_search( ).
*
*    IF iv_params IS NOT INITIAL.
*      lv_days = iv_params.
*    ENDIF.

*    SUBTRACT lv_days FROM lv_date.

*    DATA(lv_date_s) = get_formated_date( lv_date ).

*    DATA(lv_sap_filter) = '&filter=contrato_sap'.

    " 28.02.2023 - Correção filtro SAP -->
    "gv_params = 'contracts/?status=pending_approval&created_at[gt]=' && lv_date_s.
*    gv_params = 'contracts/?status=pending_approval&created_at[gt]=' && lv_date_s && lv_sap_filter.
    " 28.02.2023 - Correção filtro SAP --<

    TRY .

*        IF sy-uname = 'RBLIMA'.
        me->at_doc_tab = get_contract_offset( iv_params ).

*        ELSE.
*
*          DATA(lw_return) = me->execute_service( i_params = gv_params i_method = 'GET' ).
*
*          IF lw_return-nm_code > 206.
*
*            zcx_error=>zif_error~gera_erro_geral( i_texto = 'Requisição de contratos com problema' ).
*
*          ELSEIF lw_return-nm_code = '404'.
*
*            zcx_error=>zif_error~gera_erro_geral( i_texto = 'Sem contratos para o periodo selecionado' ).
*
*          ENDIF.
*
*          APPEND 'contract' TO t_element_array.
*          APPEND 'relacaoassinantes' TO t_element_array.
*          APPEND 'relacaoassinante' TO t_element_array.
*
*          REPLACE ALL OCCURRENCES OF '&' IN lw_return-ds_data_retorno WITH 'E'.
*
*          " 01.03.2023 - Ramon - removedor de tag -->
*          me->remove_tags_conf( CHANGING cv_xml = lw_return-ds_data_retorno ).
*          " 01.03.2023 - Ramon - removedor de tag --<
*
*          zcl_string2=>xml_to_table( EXPORTING i_xml = lw_return-ds_data_retorno i_element_array = t_element_array IMPORTING r_data = me->at_doc_tab ).
*
*          DELETE me->at_doc_tab-contracts-contract WHERE currentapproval-approverid <> me->get_system_var( 'APPROVERID' ).
*
*        ENDIF.

        IF ir_id_ref_range IS NOT INITIAL.
          DELETE me->at_doc_tab-contracts-contract WHERE id NOT IN ir_id_ref_range.
        ENDIF.

        IF me->at_doc_tab-contracts-contract IS NOT INITIAL.

          DATA lr_ref TYPE RANGE OF zin_id_referencia.

          LOOP AT me->at_doc_tab-contracts-contract ASSIGNING FIELD-SYMBOL(<fs_docs>).

            gv_current_id = <fs_docs>-id.

            APPEND 'IEQ' && <fs_docs>-id TO lr_ref.

            " 13.04.2022 - alteração na estrutura do xml vindo do coupa ->
            "<fs_docs>-customfields-relacaoassinantes_old = <fs_docs>-customfields-relacaoassinantes-relacaoassinante-customfields-relacaoassinantes.

            LOOP AT <fs_docs>-customfields-relacaoassinantes ASSIGNING FIELD-SYMBOL(<fs_assina>).

              " 10.03.2023 - RAMON - Correção TAG relação de assinantes -->
              LOOP AT <fs_assina>-relacaoassinante ASSIGNING FIELD-SYMBOL(<fs_assinaturas>).

                DATA(lv_lista) = me->get_assinantes( <fs_assinaturas>-id ).

                <fs_docs>-customfields-relacaoassinantes_old = <fs_docs>-customfields-relacaoassinantes_old && lv_lista.

              ENDLOOP.

*              DATA(lv_lista) = me->get_assinantes( <fs_assina>-relacaoassinante-id ).
*
*              <fs_docs>-customfields-relacaoassinantes_old = <fs_docs>-customfields-relacaoassinantes_old && lv_lista.

              " 10.03.2023 - RAMON - Correção TAG relação de assinantes --<



            ENDLOOP.

            <fs_docs>-customfields-relacaoassinantes_old = <fs_docs>-customfields-relacaoassinantes_old && me->get_assinantes_extras( <fs_docs> ).

            <fs_docs>-customfields-exigirdownload_old = <fs_docs>-customfields-exigirdownload-name.
            <fs_docs>-customfields-proibirrejeio_old = <fs_docs>-customfields-proibirrejeicao-name.
            <fs_docs>-customfields-assinaturasequencial_old = <fs_docs>-customfields-assinaturasequencial-name.
            <fs_docs>-customfields-agrupardocumentos_old = <fs_docs>-customfields-agrupardocumentos-name.

            " 13.04.2022 - alteração na estrutura do xml vindo do coupa -<

            <fs_docs>-customfields-relacaoassinantes_old = formata_assinatura( <fs_docs>-customfields-relacaoassinantes_old ).

            " DEBUG TESTE RAMON -->
            "BREAK RBLIMA.
            "IF 1 = 2.
            "<fs_docs>-customfields-relacaoassinantes_old = '05332137974-RAMON LIMA-ramon.lima@reclike.com.br-ASSINATURA_ELETRONICA'.
            "ENDIF.
            " DEBUG TESTE RAMON --<

          ENDLOOP.

          SELECT * FROM zint_assina01
            INTO TABLE @DATA(lt_adigital)
                WHERE id_referencia IN @lr_ref
                  AND id_processo = '01'
                  AND etapa <> '99'. " < -- OU SEJA SE NÃO EXCLUI OS QUE FORAM RECUSADOS

          IF sy-subrc EQ 0.

            LOOP AT lt_adigital ASSIGNING FIELD-SYMBOL(<fs_digital>).
              DELETE me->at_doc_tab-contracts-contract WHERE id = <fs_digital>-id_referencia.
            ENDLOOP.

          ENDIF.

        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->set_message( ex_int->get_longtext( ) ).

      CATCH zcx_error INTO DATA(ex_erro).
        me->set_message( ex_erro->get_longtext( ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_formated_date.

    r_date_s = iv_date(4) && '-' && iv_date+4(2) && '-' && iv_date+6(2).

  ENDMETHOD.


  METHOD get_responsavel_assinatura.

    DATA lt_set_tab TYPE rgsbv_tab.

    CLEAR r_respon.

    " foi mudado por causa do set, pq só podia CPF e pode ser CNPJ também
    SELECT SINGLE low FROM tvarvc
      INTO r_respon
        WHERE name = 'MAGGI_CNPJ_CPF_RESPONSAVEL_BRY'.

    IF sy-subrc NE 0.
      CLEAR r_respon.
    ENDIF.

  ENDMETHOD.


  METHOD get_system_var.

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

  ENDMETHOD.


  METHOD log_api_send.

    DATA lv_params TYPE string.
    DATA lv_url_cont TYPE string.
    DATA lv_template TYPE string.
    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    DATA t_element_array  TYPE zde_element_array_t.

    lv_params = 'requisitions/' && me->at_auth_ws-add01 &&'/comments'.

    lv_template = `<comment><comments>@CLMS - Jurídico - Erro de integração na assinatura do contrato: ` &&
                  `#ID_CONTRATO Resultado: Assinatura recusada no sistema de destino; Favor verificar. #URL_CONTRATO</comments></comment>`.

    TRY.

        "send_email( i_log_tab ).

        CHECK i_log_tab IS NOT INITIAL.

        "LOOP AT i_log_tab ASSIGNING FIELD-SYMBOL(<fs_log>).

        lv_body = lv_template.

        lv_url_cont = get_contract_url( i_log_tab[ 1 ]-id_referencia ).

*          READ TABLE me->zif_integracao_adigital_get~at_doc_tab-contracts-contract ASSIGNING FIELD-SYMBOL(<fs_contract>)
*            WITH KEY id = <fs_log>-id_referencia.
*
*          CHECK <fs_contract> IS ASSIGNED.

        "REPLACE '#DESCRI_CONTRATO' IN lv_body WITH <fs_contract>-name.
        REPLACE ALL OCCURRENCES OF '#ID_CONTRATO' IN lv_body WITH i_log_tab[ 1 ]-id_referencia.

        REPLACE '#MSG_ERRO' IN lv_body WITH 'Assinatura foi recusada no sistema de destino'.
        REPLACE '#URL_CONTRATO' IN lv_body WITH lv_url_cont.

        DATA(lw_return) = me->execute_service( i_params = lv_params i_body = lv_body i_method = 'POST' ).

        IF lw_return-nm_code > 206.

          lv_text = `Inserção de comentarios para: ` && i_log_tab[ 1 ]-id_referencia && 'falhou'.

          zcx_error=>zif_error~gera_erro_geral( i_texto =  lv_text ).

        ENDIF.

        "ENDLOOP.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->set_message( ex_int->get_longtext( ) ).
        EXIT.

      CATCH zcx_error INTO DATA(ex_erro).
        me->set_message( ex_erro->get_longtext( ) ).
        EXIT.

    ENDTRY.


  ENDMETHOD.


  METHOD lookup_values_execute.

    DATA lv_params TYPE string.

    CHECK i_lookup_id IS NOT INITIAL.

    lv_params = 'lookup_values/' && i_lookup_id.

    r_integracao = me->execute_service( i_params = lv_params i_method = 'GET').

  ENDMETHOD.


  METHOD message_length.

    DATA lv_id TYPE string.

    DATA lv_char TYPE c LENGTH 30.

    DATA lv_mess TYPE string.

    DATA lv_div TYPE i VALUE 1000000.

    DATA lv_mb TYPE p DECIMALS 3.

    DATA(lv_length) = xstrlen( iv_xfile ).

    lv_mb = lv_length / lv_div.

    WRITE lv_mb TO lv_char LEFT-JUSTIFIED.

    lv_mess = `Arquivo com tamanho ` && lv_char && ` MB`.

    lv_id = me->gv_current_id.

    CALL METHOD me->set_message
      EXPORTING
        i_message = lv_mess
        i_msgty   = 'W'
        i_doc_id  = lv_id.

  ENDMETHOD.


  METHOD remove_tag.

    DATA lo_xml_doc TYPE REF TO cl_xml_document.

    CREATE OBJECT lo_xml_doc.

    CHECK lo_xml_doc->parse_string( stream = cv_xml ) = 0.

    DATA(lo_node) = lo_xml_doc->find_node( iv_tag ).

    WHILE lo_node IS BOUND.

      DATA(lv_nome) = lo_node->get_name( ).

      lo_node->remove_node( ).

      CALL METHOD lo_xml_doc->render_2_string
        EXPORTING
          pretty_print = 'X'
        IMPORTING
          stream       = cv_xml.

      lo_node = lo_xml_doc->find_node( iv_tag ).

    ENDWHILE.

  ENDMETHOD.


  METHOD remove_tags_conf.

    SELECT low FROM tvarvc
      INTO TABLE @DATA(lt_tags)
        WHERE name = 'COUPA_AD_REMOVE_TAG'
          AND low <> @space.

    LOOP AT lt_tags ASSIGNING FIELD-SYMBOL(<fs_tag>).

      TRANSLATE <fs_tag>-low TO LOWER CASE.

      remove_tag( EXPORTING iv_tag = CONV #( <fs_tag>-low ) CHANGING cv_xml = cv_xml ).

    ENDLOOP.

  ENDMETHOD.


  METHOD send_email.

    DATA lt_assinantes TYPE TABLE OF zinc_dados_partici_form.
    DATA lt_anexos TYPE TABLE OF eew_string.
    DATA lw_values TYPE zinc_dados_partici_form.

    DATA lv_id_name TYPE string.

    DATA lt_html TYPE html_table.
    DATA lv_subject TYPE string.

    CHECK i_log_tab IS NOT INITIAL.

    " manda mensagem de log se nao coletou corretamnte
    SELECT COUNT(*) FROM zint_assina01
      WHERE id_processo = gv_current_id
        AND chave_coleta <> space.

    CHECK sy-subrc NE 0.

    SPLIT at_assinantes_string AT `;` INTO TABLE lt_assinantes.
    SPLIT at_anexos_string AT `;` INTO TABLE lt_anexos.

    READ TABLE at_doc_tab-contracts-contract ASSIGNING FIELD-SYMBOL(<fs_contract>)
      WITH KEY id = gv_current_id.

    CHECK sy-subrc EQ 0.

    DATA(lv_url_contr) = get_contract_url( CONV #( at_id_processo )  ).

    lv_id_name = `<a href="&3"> &1 - &2 </a>` .

    REPLACE `&1` IN lv_id_name WITH <fs_contract>-id.
    REPLACE `&2` IN lv_id_name WITH <fs_contract>-name.
    REPLACE `&3 ` IN lv_id_name WITH lv_url_contr.

    REPLACE `&1` IN lv_subject WITH lv_id_name.

    lv_subject = `Assinatura Digital SAP: Contrato ` && lv_id_name && ` não foi enviado a BRY`.

    APPEND INITIAL LINE TO lt_html ASSIGNING FIELD-SYMBOL(<fs_html>).

    <fs_html>-line = `<p><strong>Prezado,</strong></p><p><strong>Contrato:</strong>` && lv_id_name && `</p>` &&
    `<p>Não foi corretamente processado pelo SAP, com isso não foi enviado a BRY.</p>`.

    APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.

    <fs_html>-line =  `<p><strong>Retorno:</strong></p>`.

    LOOP AT i_log_tab ASSIGNING FIELD-SYMBOL(<fs_log>) WHERE msgty NE 'W' AND id_referencia = gv_current_id.

      APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.

      <fs_html>-line = `<p>` && <fs_log>-message && `</p>`.

      REPLACE ALL OCCURRENCES OF 'u0027' IN <fs_html>-line WITH space.

    ENDLOOP.

    APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
    <fs_html>-line = `<p><strong>Dados do contrato:</strong></p>`.

    "---------------------------------------------------------------------------
    APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
    <fs_html>-line = `<p>Assinantes coletados:</p>`.

    APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
    <fs_html>-line = `<ul>`.

    LOOP AT lt_assinantes ASSIGNING FIELD-SYMBOL(<fs_assinantes>).

      APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
      <fs_html>-line = `<li>` && <fs_assinantes>-value1 && `</li>`.

    ENDLOOP.
    "---------------------------------------------------------------------------

    APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
    <fs_html>-line = `<p>&nbsp;</p><p>Assinantes, formatos pelo SAP, <span style="text-decoration: underline;">verificar se todos os ` &&
                     ` campos estão dentro da definição</span></p>`.


    LOOP AT lt_assinantes ASSIGNING <fs_assinantes>.

      SPLIT <fs_assinantes>-value1 AT '#'
        INTO <fs_assinantes>-value1
             <fs_assinantes>-value2
             <fs_assinantes>-value3
             <fs_assinantes>-value4.

      APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `<ul><li>CPF: ` && <fs_assinantes>-value1 && `</li>`.
      APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `<li>Nome: ` && <fs_assinantes>-value2 && `</li>`.
      APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `<li>Email: ` && <fs_assinantes>-value3 && `</li>`.
      APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `<li>Tipo da assinatura: ` && <fs_assinantes>-value4 && `</li>`.
      APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `</ul><p>-------------------------------------------</p>`.

    ENDLOOP.

    "---------------------------------------------------------------------------

    APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
    <fs_html>-line = `<p>Nome de arquivos anexos:</p>`.

    " anexos
    LOOP AT lt_anexos ASSIGNING FIELD-SYMBOL(<fs_anexo>).

      APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
      <fs_html>-line = `<li>` && <fs_anexo> && `</li>`.

    ENDLOOP.

    APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
    <fs_html>-line = `</ul><p>Favor verificar.</p><p><em>*Mensagem enviada automaticamente pelo SAP*</em></p>`.

    CALL METHOD zcl_send_email=>send_static
      EXPORTING
        i_receivers = `COUPA_AD_EMAIL_ERRO`
        i_subject   = lv_subject
        i_body      = lt_html.

  ENDMETHOD.


  METHOD send_process_log.


    TRY.

        DATA lt_assinantes TYPE TABLE OF zinc_dados_partici_form.
        "DATA lt_anexos TYPE TABLE OF eew_string.
        DATA lw_values TYPE zinc_dados_partici_form.

        DATA lv_id_name TYPE string.

        DATA lt_html TYPE html_table.
        DATA lv_subject TYPE string.

        READ TABLE at_bapiret2_tab TRANSPORTING NO FIELDS
          WITH KEY parameter = is_document-id
                   type = 'E'.

        CHECK sy-subrc EQ 0.

        " manda mensagem de log se nao coletou corretamnte
        SELECT COUNT(*) FROM zint_assina01
          WHERE id_processo = is_document-id
            AND chave_coleta <> space.

        CHECK sy-subrc NE 0.

        SPLIT is_document-customfields-relacaoassinantes_old AT `;` INTO TABLE lt_assinantes.

        DATA(lv_url_contr) = get_contract_url( CONV #( is_document-id )  ).

        lv_id_name = ` &1 - &2` .

        REPLACE `&1` IN lv_id_name WITH is_document-id.
        REPLACE `&2` IN lv_id_name WITH is_document-name.


        REPLACE `&1` IN lv_subject WITH lv_id_name.

        lv_subject = `Assinatura Digital SAP: Contrato ` && lv_id_name && ` não foi enviado a BRY`.

        APPEND INITIAL LINE TO lt_html ASSIGNING FIELD-SYMBOL(<fs_html>).

        <fs_html>-line = `<p><strong>Prezado,</strong></p><p><strong>Contrato:</strong><a href="&3">` && lv_id_name && ` </a></p>` &&
        `<p>Não foi corretamente processado pelo SAP, com isso não foi enviado a BRY.</p>`.


        APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
        <fs_html>-line = `<p><strong>Retorno:</strong></p>`.

        REPLACE `&3` IN <fs_html>-line WITH lv_url_contr.

        LOOP AT at_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_log>) WHERE type NE 'W' AND parameter = is_document-id.

          APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.

          <fs_html>-line = `<p>` && <fs_log>-message && `</p>`.

          REPLACE ALL OCCURRENCES OF 'u0027' IN <fs_html>-line WITH space.

        ENDLOOP.

        APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
        <fs_html>-line = `<p><strong>Dados do contrato:</strong></p>`.

        APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
        <fs_html>-line = `<p>Assinantes coletados:</p>`.

        APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
        <fs_html>-line = `<ul>`.

        LOOP AT lt_assinantes ASSIGNING FIELD-SYMBOL(<fs_assinantes>).

          APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
          <fs_html>-line = `<li>` && <fs_assinantes>-value1 && `</li>`.

        ENDLOOP.

        APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
        <fs_html>-line = `</ul>`.

        "---------------------------------------------------------------------------

        APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
        <fs_html>-line = `<p>&nbsp;</p><p>Assinantes, formatos pelo SAP, <span style="text-decoration: underline;">verificar se todos os ` &&
                         ` campos estão dentro da definição</span></p>`.


        LOOP AT lt_assinantes ASSIGNING <fs_assinantes>.

          SPLIT <fs_assinantes>-value1 AT '#'
            INTO <fs_assinantes>-value1
                 <fs_assinantes>-value2
                 <fs_assinantes>-value3
                 <fs_assinantes>-value4.

          DATA(lv_invalido) = check_cpf( <fs_assinantes>-value1 ).

          APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `<ul><li>CPF: ` && <fs_assinantes>-value1 && ` <span style="color: #ff0000;"><strong>` && lv_invalido && `</strong></span></li>`.
          APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `<li>Nome: ` && <fs_assinantes>-value2 && `</li>`.
          APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `<li>Email: ` && <fs_assinantes>-value3 && `</li>`.
          APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `<li>Tipo da assinatura: ` && <fs_assinantes>-value4 && `</li>`.
          APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.<fs_html>-line = `</ul><p>-------------------------------------------</p>`.

        ENDLOOP.

        "---------------------------------------------------------------------------


        APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
        <fs_html>-line = `<p>Nome de arquivos anexos:</p>`.

        " anexos
        LOOP AT it_anexos-attachments-attachment ASSIGNING FIELD-SYMBOL(<fs_anexo>).

          APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
          <fs_html>-line = `<li>` && <fs_anexo>-file_name && `</li>`.

        ENDLOOP.

        APPEND INITIAL LINE TO lt_html ASSIGNING <fs_html>.
        <fs_html>-line = `</ul><p>Favor verificar.</p><p><em>*Mensagem enviada automaticamente pelo SAP*</em></p>`.

        CALL METHOD zcl_send_email=>send_static
          EXPORTING
            i_receivers = `COUPA_AD_EMAIL_ERRO`
            i_subject   = lv_subject
            i_body      = lt_html.

    ENDTRY.

  ENDMETHOD.


  METHOD send_to_approval.

    DATA lv_text TYPE string.

    DATA t_element_array  TYPE zde_element_array_t.
    DATA lw_anexos TYPE zins_adigital_attachments.

    LOOP AT me->at_doc_tab-contracts-contract ASSIGNING FIELD-SYMBOL(<fs_documento>).

      gv_current_id = <fs_documento>-id.

      IF <fs_documento>-customfields-relacaoassinantes_old IS INITIAL.

        lv_text = `Documento: ` && <fs_documento>-id && ` não tem assinantes`.

        me->set_message( i_message = lv_text  i_doc_id = <fs_documento>-id ).

        CONTINUE. "<- se nao tiver anexo, vai para o proximo

      ENDIF.

      TRY.

          DATA(lt_anexos) = me->before_sending( <fs_documento>-id ).

          DATA(lw_assina) = me->send_to_destination(
            EXPORTING i_document = <fs_documento> it_anexos =  lt_anexos ).

          IF lw_assina-chave_coleta IS INITIAL.

            lv_text = `Documento: ` && <fs_documento>-id && ` não gerou chave no sistema destino`.

            me->set_message( i_message = lv_text  i_doc_id = <fs_documento>-id ).

          ELSE.

            me->after_sending( lw_assina ).

            lv_text = `Documento: ` && <fs_documento>-id && ` enviado, coleta: ` && lw_assina-chave_coleta.

            me->set_message( EXPORTING i_message = lv_text i_msgty = 'S' i_doc_id = <fs_documento>-id ).

            COMMIT WORK AND WAIT.

          ENDIF.

          CALL METHOD me->send_process_log
            EXPORTING
              is_document = <fs_documento>
              it_anexos   = lt_anexos.

        CATCH zcx_integracao INTO DATA(ex_int).
          me->set_message( EXPORTING i_message = ex_int->get_longtext( preserve_newlines = 'X' ) i_doc_id = <fs_documento>-id  ).
          CONTINUE.

        CATCH zcx_error INTO DATA(ex_erro).
          me->set_message( EXPORTING i_message = ex_erro->get_longtext( preserve_newlines = 'X' ) i_doc_id = <fs_documento>-id  ).
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD send_to_destination.

    DATA lo_envio TYPE REF TO zcl_integracao_bry_adigital.
    DATA lw_envio TYPE zins_dados_bry_dados.

    DATA lw_sucesso TYPE zins_dados_bry_json.
    DATA lw_erro TYPE zins_dados_bry_json_erro.

    DATA lv_response TYPE string.
    DATA lv_mess TYPE string.

    DATA lv_date_i TYPE string.
    DATA lv_date TYPE sy-datum.

    TRY.

        lw_envio-nomecoleta = i_document-name.

        IF sy-sysid = 'QAS'.
          lw_envio-nomecoleta = i_document-name && ` (enviado por ambiente de testes - sem validade)`.
        ENDIF.

        " 03.07.2024 - CS2024000566 - RAMON -->

*        IF i_document-customfields-datalimiteassinaturas IS NOT INITIAL.
*          lw_envio-datalimite = format_date_bry( i_document-customfields-datalimiteassinaturas ).
*        ELSEIF i_document-enddate IS NOT INITIAL.
*          lw_envio-datalimite = format_date_bry( i_document-enddate ).
*        ELSE.
*          lw_envio-datalimite = format_date_bry( '' ).
*        ENDIF.

        lv_date = sy-datum + 365.

        lv_date_i = lv_date.

        lv_date_i = lv_date_i && '000000'.

        lw_envio-datalimite = format_date_bry( lv_date_i ).

        " 03.07.2024 - CS2024000566 - RAMON --<

        lw_envio-descricao = i_document-name.
        lw_envio-padraoassinatura = 'PDF'.
        lw_envio-exigirdownload = i_document-customfields-exigirdownload_old.
        lw_envio-proibirrejeicao = i_document-customfields-proibirrejeio_old.
        lw_envio-assinaturasequencial = i_document-customfields-assinaturasequencial_old.
        lw_envio-agrupardocumentos = i_document-customfields-agrupardocumentos_old.

        lw_envio-relacao_assinantes = i_document-customfields-relacaoassinantes_old.

        at_assinantes_string = lw_envio-relacao_assinantes.

        lw_envio-exigirdownload = convert_boolean( lw_envio-exigirdownload ).

*        IF lw_envio-exigirdownload IS INITIAL.
*          lw_envio-exigirdownload = 'false'.
*        ENDIF.

        " 03.07.2024 - CS2024000566 - RAMON -->
        "lw_envio-proibirrejeicao = convert_boolean( lw_envio-proibirrejeicao ).
        lw_envio-proibirrejeicao = convert_boolean( 'FALSE' ).
        " 03.07.2024 - CS2024000566 - RAMON --<

*        IF lw_envio-proibirrejeicao IS INITIAL.
*          lw_envio-proibirrejeicao = 'false'.
*        ENDIF.

        lw_envio-assinaturasequencial = convert_boolean( lw_envio-assinaturasequencial ).

*        IF lw_envio-assinaturasequencial IS INITIAL.
*          lw_envio-assinaturasequencial = 'false'.
*        ENDIF.

*        lw_envio-agrupardocumentos = convert_boolean( lw_envio-agrupardocumentos ).
*
*        "IF lw_envio-agrupardocumentos IS INITIAL.
*        "lw_envio-agrupardocumentos = 'false'.
*        "ENDIF.

        lw_envio-agrupardocumentos = 'true'.

        lw_envio-codigoresponsavel = get_responsavel_assinatura( ).

        IF lw_envio-codigoresponsavel IS INITIAL.
          lw_envio-local_assinatura = 'AUTOMATICA'.
        ELSE.
          lw_envio-local_assinatura = 'AUTOMATICA_EDITAVEL'.
        ENDIF.

        "lw_envio-configuracaolocalassinatura = 'AUTOMATICA_EDITAVEL'.

        CREATE OBJECT lo_envio
          EXPORTING
            it_anexos     = it_anexos
            i_dados_envio = lw_envio.

        lv_response = lo_envio->zif_integracao_bry_adigital~enviar_bry( ).

        IF lv_response CS 'status'.

          /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                CHANGING data = lw_erro ).

          me->set_message( lw_erro-mensagens[ 1 ]-chavei18n ).
          me->set_message( lw_erro-mensagens[ 1 ]-mensagem ).
          "zcx_error=>zif_error~gera_erro_geral( i_texto = lw_erro-mensagens[ 1 ]-mensagem ).

        ELSE.

          /ui2/cl_json=>deserialize( EXPORTING json = lv_response
              CHANGING data = lw_sucesso ).

        ENDIF.

        IF lw_sucesso IS INITIAL AND lw_erro-mensagens[] IS INITIAL.
          me->set_message( lv_response ).
        ENDIF.


        CHECK lw_sucesso IS NOT INITIAL.

        r_adigital-id_referencia = i_document-id.
        r_adigital-id_processo = at_system_id.
        r_adigital-etapa = '01'. "<- ENVIADO AO BRY
        r_adigital-nome = i_document-name.
        r_adigital-chave_coleta = lw_sucesso-chaveworkflow.
        r_adigital-log_date = sy-datum.
        r_adigital-log_uzeit = sy-uzeit.
        r_adigital-log_name = sy-uname.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->set_message( ex_int->get_longtext( ) ).

      CATCH zcx_error INTO DATA(ex_erro).
        me->set_message( ex_erro->get_longtext( ) ).

    ENDTRY.
  ENDMETHOD.


  METHOD set_message.

    DATA lv_id TYPE string.

    IF i_doc_id IS INITIAL.
      lv_id = gv_current_id.
    ELSE.
      lv_id = i_doc_id.
    ENDIF.

    CALL METHOD super->set_message
      EXPORTING
        i_message = i_message
        i_msgty   = i_msgty
        i_doc_id  = lv_id.

  ENDMETHOD.


  METHOD set_system.

    me->at_system_id = '01'. " --> COUPA

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    CHECK me->zif_integracao_inject~at_header_fields IS INITIAL.

    TRY .


        CAST zcl_integracao_token_coupa(
               zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
                 )->get_token( )
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).


        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

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


  METHOD check_cpf.

    DATA lv_cpf TYPE string.

    lv_cpf = iv_cpf.

    " Remove os caracteres não numéricos
    TRANSLATE lv_cpf USING '.-'.
    REPLACE ALL OCCURRENCES OF '-' IN lv_cpf WITH ''.

    " Verifica se o CPF possui 11 dígitos
    IF strlen( lv_cpf ) EQ 11.
      " calcula o primeiro dígito verificador
      DATA(lv_dv1) = ( lv_cpf+0(1) * 10 +
                       lv_cpf+1(1) * 9 +
                       lv_cpf+2(1) * 8 +
                       lv_cpf+3(1) * 7 +
                       lv_cpf+4(1) * 6 +
                       lv_cpf+5(1) * 5 +
                       lv_cpf+6(1) * 4 +
                       lv_cpf+7(1) * 3 +
                       lv_cpf+8(1) * 2 ) MOD 11.
      IF lv_dv1 < 2.
        lv_dv1 = 0.
      ELSE.
        lv_dv1 = 11 - lv_dv1.
      ENDIF.

      " calcula o segundo dígito verificador
      DATA(lv_dv2) = ( lv_cpf+0(1) * 11 +
                       lv_cpf+1(1) * 10 +
                       lv_cpf+2(1) * 9 +
                       lv_cpf+3(1) * 8 +
                       lv_cpf+4(1) * 7 +
                       lv_cpf+5(1) * 6 +
                       lv_cpf+6(1) * 5 +
                       lv_cpf+7(1) * 4 +
                       lv_cpf+8(1) * 3 +
                       lv_dv1 * 2 ) MOD 11.
      IF lv_dv2 < 2.
        lv_dv2 = 0.
      ELSE.
        lv_dv2 = 11 - lv_dv2.
      ENDIF.

      " verifica se os dígitos verificadores são iguais aos informados
      IF lv_cpf+9(1) EQ lv_dv1 AND lv_cpf+10(1) EQ lv_dv2.
        CLEAR rv_invalido.
      ELSE.
        rv_invalido = 'Inválido'.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_contract_offset.

    DATA t_element_array  TYPE zde_element_array_t.
    DATA lt_contracts TYPE zins_adigital_contratos.

    DATA(lv_offset) = 0.

    DATA(lv_date) = sy-datum.

    DATA(lv_days) = get_days_to_search( ).

    IF iv_params IS NOT INITIAL.
      lv_days = iv_params.
    ENDIF.

    CLEAR rt_contracts-contracts-contract[].
    CLEAR me->at_bapiret2_tab.

    APPEND 'contract' TO t_element_array.
    APPEND 'relacaoassinantes' TO t_element_array.
    APPEND 'relacaoassinante' TO t_element_array.

    SUBTRACT lv_days FROM lv_date.

    DATA(lv_date_s) = get_formated_date( lv_date ).

    DATA(lv_sap_filter) = '&filter=contrato_sap'.

    DO.

      CLEAR lt_contracts-contracts-contract[].

      gv_params = 'contracts/?offset=' && lv_offset && '&status=pending_approval&created_at[gt]=' && lv_date_s && lv_sap_filter.

      DATA(lw_return) = me->execute_service( i_params = gv_params i_method = 'GET' ).

      IF lw_return-nm_code > 206.
        EXIT.
      ELSEIF lw_return-nm_code = '404'.
        EXIT.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '&' IN lw_return-ds_data_retorno WITH 'E'.

      me->remove_tags_conf( CHANGING cv_xml = lw_return-ds_data_retorno ).

      zcl_string2=>xml_to_table( EXPORTING i_xml = lw_return-ds_data_retorno i_element_array = t_element_array IMPORTING r_data = lt_contracts ).

      ADD 50 TO lv_offset.

      APPEND LINES OF lt_contracts-contracts-contract TO rt_contracts-contracts-contract.

    ENDDO.

    break rblima. " para aqui para nao deixar executar a linha 57 em ambiente de teste.....

    DELETE rt_contracts-contracts-contract WHERE currentapproval-approverid <> me->get_system_var( 'APPROVERID' ).

    SORT rt_contracts-contracts-contract BY id ASCENDING.

    DELETE ADJACENT DUPLICATES FROM rt_contracts-contracts-contract COMPARING id.

    IF rt_contracts-contracts-contract IS INITIAL.
      zcx_error=>zif_error~gera_erro_geral( i_texto = 'Sem contratos para o periodo selecionado' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
