class ZCL_COUPA_DOCUMENTS_STEP_02 definition
  public
  inheriting from ZCL_DEFAULT_DOCUMENTS_STEP_02
  create public .

public section.

  methods CONSTRUCTOR .
  methods PROCESS_SIGNED_CONTRACT
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE optional
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods PROCESS_REFUSED_CONTRACT
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE optional .
  methods SEND_DOC_ASSINADO
    importing
      !I_DOC_ASSINADO type ZINS_DOCS_ASSINA .
  methods BEFORE_PUBLISHING
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA .
  methods APPROVE_ID_EXECUTE
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods AFTER_PUBLISHING
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA .

  methods PUBLISH_CONTRACT
    redefinition .
  methods SET_SYSTEM
    redefinition .
  methods ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG
    redefinition .
protected section.
private section.

  methods UPDATE_WORKFLOW_KEY
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA .
  methods UPDATE_STATUS
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA
      !I_NEW_STATUS type STRING .
  methods EXECUTE_APPROVALS
    importing
      !I_REFERENCIA type ZIN_ID_REFERENCIA
    returning
      value(R_APPROVAL_ID) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
ENDCLASS.



CLASS ZCL_COUPA_DOCUMENTS_STEP_02 IMPLEMENTATION.


  METHOD after_publishing.

    DATA lv_params TYPE string.
    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    CHECK i_document-chavedocumento IS NOT INITIAL.

    DATA lv_message TYPE string.

    TRY.

        update_workflow_key( i_document ).
        "approve_id_execute( i_document ).

      CATCH zcx_integracao INTO DATA(ex_int).

        lv_message = `Tentativa 2 de aprovar aprovadores falhou para o contrato: ` && i_document-id_referencia && `Exceção: ` && ex_int->get_longtext( ).
        me->set_message( lv_message ).

      CATCH zcx_error INTO DATA(ex_erro).

        lv_message = `Tentativa 2 de aprovar aprovadores falhou para o contrato: ` && i_document-id_referencia && `Exceção: ` && ex_erro->get_longtext( ).
        me->set_message( lv_message ).

    ENDTRY.

  ENDMETHOD.


  METHOD approve_id_execute.

    DATA lv_params TYPE string.
    DATA lv_erro TYPE c.
    DATA lv_message TYPE string.

    DATA lo_xml TYPE REF TO cl_xml_document.

    DO.

      IF lv_erro = 'X'.
        EXIT.
      ENDIF.

      "lv_params = 'contracts/' && i_document-id_referencia && '?&fields=[{"current_approval":["approver-id"]}]'.
      lv_params = 'contracts/' && i_document-id_referencia && '?&fields=[{"current_approval":["id"]}]'.

      lv_erro = 'X'. "< - se passar pelos checks, vai tirar o 'X'.

      DATA(lw_int) = me->execute_service( i_params = lv_params i_method = 'GET').

      CHECK lw_int-nm_code = 200 OR lw_int-nm_code = 201 OR lw_int-nm_code = 202.

      CREATE OBJECT lo_xml.

      CHECK lo_xml->parse_xstring( lw_int-ds_data_xstring ) = 0.

      "DATA(lv_approval_id) = lo_xml->find_simple_element( 'approver-id' ).
      DATA(lv_approval_id) = lo_xml->find_simple_element( 'id' ).

      CHECK lv_approval_id IS NOT INITIAL.

      lv_params = 'approvals/' && lv_approval_id && '/approve?reason=Aprovado'.

      lw_int = me->execute_service( i_params = lv_params i_method = 'PUT').

      CHECK lw_int-nm_code = 200 OR lw_int-nm_code = 201 OR lw_int-nm_code = 202.

      lv_erro = ''.

    ENDDO.


  ENDMETHOD.


  METHOD before_publishing.

    DATA lv_message TYPE string.

    TRY.

        "update_workflow_key( i_document ).
        approve_id_execute( i_document ).

        update_status( i_document = i_document i_new_status = 'ccc_signature_created' ).

        update_status( i_document = i_document i_new_status = 'complete' ).

      CATCH zcx_integracao INTO DATA(ex_int).

        lv_message = `Tentativa 1 de aprovar aprovadores falhou para o contrato: ` && i_document-id_referencia && `Exceção: ` && ex_int->get_longtext( ).
        me->set_message( lv_message ).

      CATCH zcx_error INTO DATA(ex_erro).

        lv_message = `Tentativa 1 de aprovar aprovadores falhou para o contrato: ` && i_document-id_referencia && `Exceção: ` && ex_erro->get_longtext( ).
        me->set_message( lv_message ).

    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    me->at_service = 'COUPA_INT_ASSINATURA_DIGITAL'.
    me->zif_integracao_inject~at_id_interface    =  zif_integracao=>at_id_interface_coupa_adigital.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    IF me->at_service IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->at_auth_ws
          WHERE service = me->at_service .

    ENDIF.

    IF me->at_auth_ws IS INITIAL.
      "RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD execute_approvals.

    DATA lv_params TYPE string.
    "DATA lv_message TYPE string.

    DATA lo_xml TYPE REF TO cl_xml_document.

    lv_params = 'contracts/' && i_referencia && '?&fields=[{"current_approval":["id"]}]'.

    DATA(lw_int) = me->execute_service( i_params = lv_params i_method = 'GET').

    CHECK lw_int-nm_code = 200 OR lw_int-nm_code = 201 OR lw_int-nm_code = 202.

    CREATE OBJECT lo_xml.

    CHECK lo_xml->parse_xstring( lw_int-ds_data_xstring ) = 0.

    r_approval_id = lo_xml->find_simple_element( 'id' ).

  ENDMETHOD.


  METHOD process_refused_contract.

    DATA lv_mess TYPE string.
    DATA lv_params TYPE string.

    SELECT * FROM zint_assina01
      INTO TABLE @DATA(lt_assina)
        WHERE etapa = '98' "<-- documentos recusados
          AND id_processo = '01'.

    LOOP AT lt_assina ASSIGNING FIELD-SYMBOL(<fs_docs>).

      TRY.

          DATA(lv_id_approve) = execute_approvals( EXPORTING i_referencia = <fs_docs>-id_referencia ).

          lv_params = '/approvals/' && lv_id_approve && '/reject?reason=ReasonForFailure'.

          DATA(lw_return) = me->execute_service( i_params = lv_params i_method = 'PUT').

          IF lw_return-nm_code < 206.

            lv_mess = `Recusa do contrato: ` && <fs_docs>-id_referencia && ` foi realizada no COUPA`.

            me->set_message( EXPORTING i_message = lv_mess i_msgty = 'S' ).

            UPDATE zint_assina01 SET etapa = '99'
              log_date = sy-datum
              log_uzeit = sy-uzeit
              log_name = sy-uname
            WHERE id_referencia = <fs_docs>-id_referencia.

            COMMIT WORK AND WAIT.

          ELSE.

            lv_mess = `Recusa do contrato: ` && <fs_docs>-id_referencia && ` retornou erro: ` && lw_return-ds_data_retorno.
            me->set_message( lv_mess ).

          ENDIF.

        CATCH zcx_integracao INTO DATA(ex_int) .

          lv_mess = `Recusa do contrato: ` && <fs_docs>-id_referencia && ` retornou exceção: ` && ex_int->get_longtext( ).

          me->set_message( lv_mess ).

        CATCH zcx_error INTO DATA(ex_erro) .

          lv_mess = `Recusa do contrato: ` && <fs_docs>-id_referencia && ` retornou exceção: ` && ex_erro->get_longtext( ).

          me->set_message( lv_mess ).

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD process_signed_contract.

    DATA lt_cab TYPE TABLE OF zins_docs_ref.
    DATA lw_cab TYPE zins_docs_ref.
    DATA lo_object TYPE REF TO zcl_integracao_bry_assina_get.
    DATA lv_text TYPE string.
    DATA lt_contracts TYPE zinc_docs_assina.
    DATA lt_enviar TYPE zinc_docs_assina.
    DATA lv_params TYPE string.
    DATA lv_body TYPE string.

    CREATE OBJECT lo_object.

    lt_contracts = lo_object->zif_integracao_bry_assina_get~get_docs_assinados( ir_id_ref_range = ir_id_ref_range ).

    APPEND LINES OF lo_object->zif_integracao_bry_assina_get~get_messages( ) TO me->at_bapiret2_tab.

    CHECK lt_contracts IS NOT INITIAL.

    LOOP AT lt_contracts ASSIGNING FIELD-SYMBOL(<fs_contract>).
      lw_cab-id_referencia = <fs_contract>-id_referencia.
      COLLECT lw_cab INTO lt_cab.
    ENDLOOP.

    LOOP AT lt_cab ASSIGNING FIELD-SYMBOL(<fs_cab>).

      CLEAR lt_enviar.

      LOOP AT lt_contracts ASSIGNING <fs_contract> WHERE id_referencia = <fs_cab>-id_referencia.

        send_doc_assinado( <fs_contract> ).

      ENDLOOP.

      TRY.

          " antes de publicar
          me->before_publishing( <fs_contract> ).

          "{{URL}}/contracts/:id/update_published
          TRY.

              lv_params = 'contracts/' && <fs_contract>-id_referencia.

              lv_body = '<?xml version="1.0" encoding="UTF-8"?><contract><status>published</status></contract>'.

              DATA(lw_int) = me->execute_service( i_params = lv_params i_body = lv_body   i_method = 'PUT').

            CATCH zcx_integracao INTO DATA(ex_int).

            CATCH zcx_error INTO DATA(ex_erro).

          ENDTRY.

          me->after_publishing( <fs_contract> ).

          UPDATE zint_assina01 SET etapa = '03' "<-- processo encerrado finalizada
              log_date = sy-datum
              log_uzeit = sy-uzeit
              log_name = sy-uname
            WHERE id_referencia = <fs_contract>-id_referencia.

          COMMIT WORK AND WAIT.

          lv_text = `Contrato: ` && <fs_contract>-id_referencia && ` foi finalizado com sucesso`.

          me->set_message( EXPORTING i_msgty = 'S' i_message = lv_text ).

        CATCH zcx_integracao INTO ex_int.
          me->set_message( ex_int->get_longtext( ) ).
          CONTINUE.
        CATCH zcx_error INTO ex_erro.
          me->set_message( ex_erro->get_longtext( ) ).
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD publish_contract.

    process_signed_contract( ir_id_ref_range[] ).

    process_refused_contract( ir_id_ref_range[] ).

  ENDMETHOD.


  METHOD send_doc_assinado.

    DATA lt_multi TYPE zde_multipart_field_t.
    DATA lv_params TYPE string.
    DATA lv_id_ref TYPE zin_id_referencia.

    CHECK i_doc_assinado IS NOT INITIAL.

    TRY .

        lv_id_ref = i_doc_assinado-id_referencia.

        lv_params = 'contracts/' && lv_id_ref && '/attachments'.

        "LOOP AT i_docs_assinados ASSIGNING FIELD-SYMBOL(I_DOC_ASSINADO).

        DATA(lv_value) = 'form-data; name="attachment[file]"; filename="' && i_doc_assinado-nome && '"'.

        APPEND VALUE #( header_field = 'content-disposition' header_value = lv_value  ) TO lt_multi.

        IF i_doc_assinado-file IS NOT INITIAL.
          APPEND VALUE #( header_field = 'content-type' header_value = 'application/pdf' value = i_doc_assinado-file  ) TO lt_multi.
        ELSEIF i_doc_assinado-xfile IS NOT INITIAL.
          APPEND VALUE #( header_field = 'content-type' header_value = 'application/pdf' xvalue = i_doc_assinado-xfile  ) TO lt_multi.
        ENDIF.

        APPEND VALUE #( header_field = 'content-disposition' header_value = 'form-data; name="attachment[type]"' value = 'file' ) TO lt_multi.

        "ENDLOOP.

        DATA(lw_int) = me->execute_multipart( i_params = lv_params i_multi_tab = lt_multi ).

        IF lw_int-nm_code > 205.
          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_int-ds_data_retorno ).
        ENDIF.

      CATCH zcx_integracao INTO DATA(lo_integ).
        me->set_message( EXPORTING i_message = lo_integ->get_longtext( ) ).
      CATCH zcx_error INTO DATA(lo_erro).
        me->set_message( EXPORTING i_message = lo_erro->get_longtext( ) ).
    ENDTRY.

  ENDMETHOD.


  method SET_SYSTEM.

    at_system_id = '01'.

  endmethod.


  METHOD update_status.

    DATA lv_params TYPE string.
    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    lv_params = 'contracts/' && i_document-id_referencia && '/' && i_new_status.

    " MENSAGEM EM CASO DE ERRO
    lv_text = `Para o documento: ` && i_document-id_referencia && ` não foi possivel alterar o status para ` && i_new_status.

    TRY.

        DATA(lw_return) = me->execute_service( i_params = lv_params i_method = 'PUT' i_body = lv_body ).

        IF lw_return-nm_code > 205.

          me->set_message( lv_text ).

        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int) .

        me->set_message( lv_text ).

      CATCH zcx_error INTO DATA(ex_erro) .
        me->set_message( lv_text ).

    ENDTRY.

  ENDMETHOD.


  METHOD update_workflow_key.


    DATA lv_params TYPE string.
    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    " --- atualizando a chave de assinatura no documento -------"
    lv_params = 'contracts/' && i_document-id_referencia && '/?fields=["id",{"custom_fields":["chave-coleta-assinatura"]}]'.

    lv_body = '<?xml version="1.0" encoding="UTF-8"?><contract><custom-fields>' &&
              '<chave-coleta-assinatura>' && i_document-chavedocumento && '</chave-coleta-assinatura>' &&
              '</custom-fields></contract>'.

    " MENSAGEM EM CASO DE ERRO
    lv_text = `Para o documento: ` && i_document-id_referencia && ` não foi possivel adicionar a chave de coleta`.

    TRY.

        DATA(lw_return) = me->execute_service( i_params = lv_params i_method = 'PUT' i_body = lv_body ).

        IF lw_return-nm_code > 205.

          me->set_message( lv_text ).

        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int) .

        me->set_message( lv_text ).

      CATCH zcx_error INTO DATA(ex_erro) .
        me->set_message( lv_text ).

    ENDTRY.

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
