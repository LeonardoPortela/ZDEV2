class ZCL_INT_OB_SAFRA_CRT_CRG_TRACK definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants LC_SERVICO_VEIC type /UI2/SERVICE_NAME value 'CARGA_SAFRA_CONTROL_TRACK_VEICULOS' ##NO_TEXT.
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '302' ##NO_TEXT.
  constants:
    lc_servico TYPE c LENGTH 30 value 'CARGA_SAFRA_CONTROL_TRACK' ##NO_TEXT.
  constants:
    lc_authorization TYPE c LENGTH 13 value 'Authorization' ##NO_TEXT.
  constants:
    lc_erro          TYPE c LENGTH 01 value 'E' ##NO_TEXT.
  constants:
    lc_interface TYPE c LENGTH 09 value 'interface' ##NO_TEXT.
  data AT_TIPO type CHAR01 .
  data AT_BODY_TRANSP type ZDE_SAFRACONTROL_CARGO_TRANSP .
  data AT_BODY_ANEXOS type ZDE_SAFRACONTROL_CARGO_ANEXOS .
  data AT_BODY_VEICUL type ZDE_SAFRACONTROL_CARGO_VEICULO .
  data AT_DS_BODY type STRING .
  data AT_TYPE_PUT type STRING .
  data AT_EXTERNALID type ZDE_SAFRA_EXTID_CONTROL .
  constants:
    lc_content_type  TYPE c LENGTH 04 value 'JSON' ##NO_TEXT.
  data AT_ENDPOINT type STRING .
  data AT_METODO_HTTP type STRING .
  data AT_NRO_CG type ZNRO_CG .
  data AT_ID_REFERENCIA type STRING .
  constants C_TRUE type STRING value 'true' ##NO_TEXT.
  constants C_FALSE type STRING value 'false' ##NO_TEXT.
  constants C_NFE type STRING value 'NFE' ##NO_TEXT.
  constants C_AUT_EMB type STRING value 'AUT_EMB' ##NO_TEXT.
  data AT_ZSDT0133 type ZSDT0133 .
  data AT_ZSDT0129 type ZSDT0129 .
  data AT_ZLEST0002 type ZLEST0002 .
  data AT_LFA1_TRANSP type LFA1 .
  data AT_DADOS_INPUT type ZDE_INT_OB_TRACKING .

  methods CONSTRUCTOR .
  methods SET_METODO_HTTP
    importing
      !I_METODO type STRING .
  methods GET_METODO_HTTP
    returning
      value(E_METODO) type STRING .
  methods GET_EXTERNAL_ID
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_EXTERNAL_ID) type STRING .
  methods SET_MONTAR_JSON_TRANSPORT
    returning
      value(R_DATA) type ZDE_SAFRACONTROL_CARGO_TRANSP .
  methods SET_MONTAR_JSON_ANEXOS
    returning
      value(R_DATA) type ZDE_SAFRACONTROL_CARGO_ANEXOS .
  methods SET_MONTAR_JSON_VEICULO
    returning
      value(R_DATA) type ZDE_SAFRACONTROL_CARGO_VEICULO .
protected section.
private section.

  data AT_ID_INC type STRING .
  data AT_LFA1_MOTORI type LFA1 .
ENDCLASS.



CLASS ZCL_INT_OB_SAFRA_CRT_CRG_TRACK IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = me->lc_interface.
  ENDMETHOD.


  METHOD get_external_id.

    r_external_id = |{ i_nro_cg ALPHA = OUT }|.

    CONDENSE r_external_id NO-GAPS.

  ENDMETHOD.


  METHOD GET_METODO_HTTP.
    e_metodo = at_metodo_http.
  ENDMETHOD.


  METHOD SET_METODO_HTTP.
    at_metodo_http = i_metodo.
  ENDMETHOD.


  METHOD set_montar_json_anexos.

    TYPES: BEGIN OF ty_tabnfs,
             docnum       TYPE j_1bdocnum,
             chave_nfe    TYPE zde_chave_doc_e,
             xml_notafisc TYPE string,
             pdf_notafisc TYPE string.
    TYPES: END   OF ty_tabnfs.

    DATA: lv_obj_key         TYPE sibflporb-instid,
          lv_document_id     TYPE sofolenti1-doc_id,
          lv_document_data   TYPE sofolenti1,
          lv_xstring         TYPE xstring,
          lv_pdf_autoriza    TYPE string,
          lv_name_autoriza   TYPE string,
          lv_docnum          TYPE j_1bdocnum,
          lv_xml_out         TYPE xstring,
          lv_pdf_out         TYPE xstring,
          lv_xml_notafisc    TYPE string,
          lv_pdf_notafisc    TYPE string,
          lv_chave_nfe       TYPE zde_chave_doc_e,
          lv_integra_nf      TYPE char01,
          lv_integra_aut_emb TYPE char01,
          lv_document_type   TYPE string,
          w_attachment       TYPE zde_safra_control_anexos,
          t_cont_hex         TYPE TABLE OF solix,
          t_tabnfs           TYPE TABLE OF ty_tabnfs,
          w_tabnfs           TYPE ty_tabnfs,
          w_zsdt0422         TYPE zsdt0422,
          t_romaneios        TYPE zless0006_t,
          t_anexos           TYPE TABLE OF bdn_con,
          t_tvarv            TYPE TABLE OF tvarvc,
          zcl_util           TYPE REF TO zcl_util.

    CREATE OBJECT zcl_util.

    FREE: r_data, lv_xstring, t_anexos, lv_pdf_autoriza, lv_docnum, lv_xml_notafisc, lv_pdf_notafisc, lv_chave_nfe,
          lv_xml_out, lv_pdf_out, lv_pdf_autoriza, lv_integra_nf, t_tabnfs.

    lv_integra_nf      = abap_true.
    lv_integra_aut_emb = abap_true.

    SELECT *
      FROM tvarvc
      INTO TABLE t_tvarv
     WHERE name = 'SAFRA_TRACKING'.

*------------------------------------------------------------
*-- anexos carga
*------------------------------------------------------------
    lv_obj_key = me->at_zsdt0133-nro_cg.

    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        classname          = 'ZSDT0112'
        objkey             = lv_obj_key
        client             = sy-mandt
      TABLES
        gos_connections    = t_anexos
      EXCEPTIONS
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3
        OTHERS             = 4.

    DELETE t_anexos WHERE NOT ( descript CS 'Autoriza' ).
    SORT   t_anexos BY crea_time DESCENDING.

    READ TABLE t_anexos INTO DATA(_anexos) INDEX 1.

    IF sy-subrc = 0.
      lv_document_id = _anexos-loio_id.

      CALL FUNCTION 'SO_DOCUMENT_READ_API1'
        EXPORTING
          document_id                = lv_document_id
        IMPORTING
          document_data              = lv_document_data
        TABLES
          contents_hex               = t_cont_hex
        EXCEPTIONS
          document_id_not_exist      = 1
          operation_no_authorization = 2
          x_error                    = 3
          OTHERS                     = 4.

      CLEAR: lv_xstring.

      LOOP AT t_cont_hex INTO DATA(_cont_hex).
        lv_xstring = lv_xstring && _cont_hex-line.
      ENDLOOP.

      lv_name_autoriza  = _anexos-descript.
      lv_pdf_autoriza   = zcl_string=>xstring_to_base64( lv_xstring ).
    ENDIF.

*------------------------------------------------------------
*-- NF-s carga
*------------------------------------------------------------
    CALL METHOD zcl_carga_saida_insumos=>busca_dados_carga
      EXPORTING
        i_nr_carga_single = me->at_zsdt0133-nro_cg
      IMPORTING
        e_romaneios       = t_romaneios.

    LOOP AT t_romaneios INTO DATA(_romaneios).
      FREE: lv_xml_out, lv_xml_notafisc, lv_pdf_out, lv_pdf_notafisc.

      IF _romaneios-nro_nf_prod IS INITIAL.
        lv_integra_nf = abap_false.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM j_1bnfe_active INTO @DATA(lwa_active)
       WHERE docnum EQ @_romaneios-nro_nf_prod.

      IF NOT ( sy-subrc EQ 0 AND lwa_active-docsta EQ '1' AND lwa_active-scssta NE '2' ).
        lv_integra_nf = abap_false.
        EXIT.
      ENDIF.


      lv_docnum    =  _romaneios-nro_nf_prod.
      lv_chave_nfe = zcl_util->get_chave_nfe( lv_docnum ).

      TRY.
          CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
            EXPORTING
              i_docnum = lv_docnum
              i_tipo   = 'XML'
            IMPORTING
              out      = lv_xml_out.

          lv_xml_notafisc = zcl_string=>xstring_to_base64( lv_xml_out ).

          CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
            EXPORTING
              i_docnum = lv_docnum
              i_tipo   = 'PDF'
            IMPORTING
              out      = lv_pdf_out.

          lv_pdf_notafisc = zcl_string=>xstring_to_base64( lv_pdf_out ).

        CATCH zcx_doc_eletronico.
      ENDTRY.

      IF lv_xml_notafisc IS INITIAL OR lv_pdf_notafisc IS INITIAL.
        lv_integra_nf = abap_false.
        EXIT.
      ENDIF.

      w_tabnfs-docnum       = lv_docnum.
      w_tabnfs-chave_nfe    = lv_chave_nfe.
      w_tabnfs-xml_notafisc = lv_xml_notafisc.
      w_tabnfs-pdf_notafisc = lv_pdf_notafisc.
      APPEND w_tabnfs      TO t_tabnfs.
    ENDLOOP.

    IF t_romaneios[] IS INITIAL.
      lv_integra_nf = abap_false.
    ENDIF.

*------------------------------------------------------------
*-- elimina chars especiais
*------------------------------------------------------------
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lv_name_autoriza
      IMPORTING
        outtext           = lv_name_autoriza
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.

*------------------------------------------------------------
*-- montar jaon
*------------------------------------------------------------

*---Autorizacao Embarque
    SELECT SINGLE *
      INTO @DATA(_0422)
      FROM zsdt0422
     WHERE nro_cg     = @me->at_zsdt0133-nro_cg
       AND tipo_anexo = @c_aut_emb
       AND integrado  = @abap_true.

    IF sy-subrc NE 0.

      IF lv_pdf_autoriza IS NOT INITIAL.
        READ TABLE t_tvarv INTO DATA(w_tvarv) WITH KEY low = 'AUTORIZACAO_EMBARQUE'.
        lv_document_type = COND #( WHEN sy-subrc = 0 THEN w_tvarv-high ELSE 'autorizacaoEmbarque' ).

        CLEAR w_attachment.
        w_attachment-document_type     = lv_document_type.
        w_attachment-content_type      = 'application/pdf'.
        w_attachment-file_base64       = lv_pdf_autoriza.
        w_attachment-file_name         = lv_name_autoriza.
        w_attachment-external_id       = lv_name_autoriza.
*     APPEND me->at_id_referencia   TO  w_attachment-sales_order_item_external_id.
        CONDENSE: w_attachment-external_id NO-GAPS, w_attachment-file_name NO-GAPS.
        APPEND w_attachment           TO r_data-attachments.

        CLEAR w_zsdt0422.
        w_zsdt0422-nro_cg              = me->at_zsdt0133-nro_cg.
        w_zsdt0422-tipo_anexo          = c_aut_emb.
        w_zsdt0422-anexo_gerado        = abap_true.
        MODIFY zsdt0422             FROM w_zsdt0422.
        COMMIT WORK AND WAIT.
      ELSE.
        CLEAR w_zsdt0422.
        w_zsdt0422-nro_cg              = me->at_zsdt0133-nro_cg.
        w_zsdt0422-tipo_anexo          = c_aut_emb.
        w_zsdt0422-anexo_gerado        = abap_false.
        MODIFY zsdt0422             FROM w_zsdt0422.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.

*---Nota fiscal --------------------------------------------------
    SELECT SINGLE *
      INTO @_0422
      FROM zsdt0422
     WHERE nro_cg     = @me->at_zsdt0133-nro_cg
       AND tipo_anexo = @c_nfe
       AND integrado  = @abap_true.

    IF sy-subrc NE 0.

      IF lv_integra_nf = abap_true.
        LOOP AT t_tabnfs     INTO w_tabnfs.

*-------PDF fiscal
          READ TABLE t_tvarv INTO w_tvarv  WITH KEY low = 'NF-E_DANFE'.
          lv_document_type = COND #( WHEN sy-subrc = 0 THEN w_tvarv-high ELSE 'nfeDanfe' ).

          CLEAR w_attachment.
          w_attachment-document_type   = lv_document_type.
          w_attachment-content_type    = 'application/pdf'.
          w_attachment-file_base64     = w_tabnfs-pdf_notafisc.
          w_attachment-file_name       = w_tabnfs-chave_nfe && '_carga_' && |{ me->at_zsdt0133-nro_cg ALPHA = OUT }| && '.pdf'.
          w_attachment-external_id     = 'pdf_' && w_tabnfs-chave_nfe && '_' && |{ me->at_zsdt0133-nro_cg ALPHA = OUT }|.
          CONDENSE: w_attachment-external_id NO-GAPS, w_attachment-file_name NO-GAPS.
*       APPEND me->at_id_referencia TO  w_attachment-sales_order_item_external_id.
          APPEND w_attachment         TO r_data-attachments.

*-------XML fiscal
          READ TABLE t_tvarv INTO w_tvarv  WITH KEY low = 'NF-E_XML'.
          lv_document_type = COND #( WHEN sy-subrc = 0 THEN w_tvarv-high ELSE 'nfeXml' ).

          CLEAR w_attachment.
          w_attachment-document_type   = lv_document_type.
          w_attachment-content_type    = 'application/xml'.
          w_attachment-file_base64     = w_tabnfs-xml_notafisc.
          w_attachment-file_name       = w_tabnfs-chave_nfe && '_carga_' && |{ me->at_zsdt0133-nro_cg ALPHA = OUT }| && '.xml'.
          w_attachment-external_id     = 'xml_' && w_tabnfs-chave_nfe && '_' && |{ me->at_zsdt0133-nro_cg ALPHA = OUT }|.
          CONDENSE: w_attachment-external_id NO-GAPS, w_attachment-file_name NO-GAPS.
*       APPEND me->at_id_referencia TO  w_attachment-sales_order_item_external_id.
          APPEND w_attachment         TO r_data-attachments.
        ENDLOOP.

        CLEAR w_zsdt0422.
        w_zsdt0422-nro_cg              = me->at_zsdt0133-nro_cg.
        w_zsdt0422-tipo_anexo          = c_nfe.
        w_zsdt0422-anexo_gerado        = abap_true.
        MODIFY zsdt0422             FROM w_zsdt0422.
        COMMIT WORK AND WAIT.
      ELSE.
        CLEAR w_zsdt0422.
        w_zsdt0422-nro_cg              = me->at_zsdt0133-nro_cg.
        w_zsdt0422-tipo_anexo          = c_nfe.
        w_zsdt0422-anexo_gerado        = abap_false.
        MODIFY zsdt0422             FROM w_zsdt0422.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD set_montar_json_transport.

    DATA: lc_integra_safra TYPE REF TO zcl_int_ob_safra_crt_contact.

    CREATE OBJECT lc_integra_safra.

    FREE: r_data, me->at_body_transp.

    me->at_body_transp-withdrawal_by                           = 'transportadora'.
    me->at_body_transp-transport_company-identification_number = me->at_lfa1_transp-stcd1.
    me->at_body_transp-transport_company-name                  = me->at_lfa1_transp-name1.
    me->at_body_transp-transport_company-external_id           = lc_integra_safra->get_external_id( i_parceiro = me->at_lfa1_transp-lifnr i_tipo_parceiro = 'F' ).
*   me->at_body_transp-transport_company-external_id           = |{ me->at_lfa1_transp-lifnr ALPHA = OUT }|.
*
    me->at_body_transp-driver-name                             = me->at_lfa1_motori-name1.
    me->at_body_transp-driver-identification_number            = me->at_lfa1_motori-stcd2.
    me->at_body_transp-driver-phone                            = me->at_lfa1_motori-telf1.
    me->at_body_transp-driver-external_id                      = lc_integra_safra->get_external_id( i_parceiro = me->at_lfa1_motori-lifnr i_tipo_parceiro = 'F' ).
*   me->at_body_transp-driver-external_id                      = |{ me->at_lfa1_motori-lifnr ALPHA = OUT }|.
*
    me->at_body_transp-vehicle-license_plate                   = me->at_zlest0002-pc_veiculo.
    me->at_body_transp-vehicle-state                           = me->at_zlest0002-taxjurcode(2).
    me->at_body_transp-vehicle-type                            = me->at_zsdt0133-ds_conjunto_transp.

    CONDENSE me->at_body_transp-transport_company-external_id          NO-GAPS.
    CONDENSE me->at_body_transp-transport_company-identification_number.
    CONDENSE me->at_body_transp-transport_company-name.
    CONDENSE me->at_body_transp-driver-identification_number.
    CONDENSE me->at_body_transp-driver-phone.
    CONDENSE me->at_body_transp-driver-external_id.
    CONDENSE me->at_body_transp-driver-name.
    CONDENSE me->at_body_transp-vehicle-license_plate.
    CONDENSE me->at_body_transp-vehicle-state.

    r_data = me->at_body_transp.

  ENDMETHOD.


  METHOD set_montar_json_veiculo.

    FREE: r_data, me->at_body_veicul.

    me->at_body_veicul-external_id      = me->at_zlest0002-pc_veiculo.
    me->at_body_veicul-license_plate    = me->at_zlest0002-pc_veiculo.
    me->at_body_veicul-brand            = me->at_zsdt0133-ds_conjunto_transp.
    me->at_body_veicul-model            = me->at_zsdt0133-ds_conjunto_transp.
    me->at_body_veicul-color            = 'branco'.
    me->at_body_veicul-type             = me->at_zsdt0133-ds_conjunto_transp.
    me->at_body_veicul-state            = me->at_zlest0002-taxjurcode(2).
    me->at_body_veicul-truck_type_id    = me->at_zsdt0133-ds_conjunto_transp.

    CONDENSE me->at_body_veicul-external_id          NO-GAPS.
    CONDENSE me->at_body_veicul-license_plate.
    CONDENSE me->at_body_veicul-brand.
    CONDENSE me->at_body_veicul-model.
    CONDENSE me->at_body_veicul-color.
    CONDENSE me->at_body_veicul-type.
    CONDENSE me->at_body_veicul-state.
    CONDENSE me->at_body_veicul-truck_type_id.

    r_data = me->at_body_veicul.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~BUILD_INFO_REQUEST.

    r_if_integracao_outbound = me.

*    IF at_metodo_http EQ zif_integracao_inject=>co_request_method_post.
*      MOVE-CORRESPONDING i_info_request TO at_body.
*    ELSE.
*      MOVE-CORRESPONDING i_info_request TO at_externalid.
*    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_outbound~execute_request.

    r_if_integracao_outbound = me.

    me->at_dados_input   = i_info_request.
    me->at_nro_cg        = me->at_dados_input-nro_cg.
    me->at_metodo_http   = me->at_dados_input-metodo.
    me->at_endpoint      = me->at_dados_input-endpoint.
    me->at_id_referencia = me->get_external_id(  me->at_dados_input-nro_cg ).


    "// Inclui Json na Mensagem a Ser Enviada
    me->zif_integracao_outbound~build_info_request( i_info_request = i_info_request
      )->get_data( IMPORTING e_data = DATA(lc_data) ).

    CASE me->at_endpoint.
      WHEN 'ANEXOS'.
        CHECK me->at_body_anexos IS NOT INITIAL AND lc_data is NOT INITIAL.
    ENDCASE.

    me->zif_integracao_outbound~build_info_request(
      )->set_data( EXPORTING i_data = lc_data
      )->set_url(
      )->set_id_referencia(
      )->send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
      ).

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_data.

    CLEAR: e_data.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      INTO @me->at_zsdt0133
      FROM zsdt0133
     WHERE nro_cg = @me->at_dados_input-nro_cg.

    CHECK sy-subrc = 0 AND me->at_zsdt0133-id_carga_safra_control IS NOT INITIAL.

    me->at_id_inc = me->at_zsdt0133-id_carga_safra_control.

    CASE me->at_endpoint.
      WHEN 'ANEXOS'.
        CHECK me->at_zsdt0133-dt_autorizacao_embarque IS NOT INITIAL.
    ENDCASE.

    SELECT SINGLE *
      INTO @me->at_zsdt0129
      FROM zsdt0129
     WHERE nro_cg = @me->at_dados_input-nro_cg.

    SELECT SINGLE *
      INTO @me->at_zlest0002
      FROM zlest0002
     WHERE pc_veiculo = @me->at_zsdt0129-placa_cav.

    SELECT SINGLE *
      INTO @me->at_lfa1_transp
      FROM lfa1
     WHERE lifnr = @me->at_zsdt0133-cod_transportadora.

    SELECT SINGLE *
      INTO @me->at_lfa1_motori
      FROM lfa1
     WHERE lifnr = @me->at_zsdt0129-motorista.

    CASE me->at_endpoint.
      WHEN 'TRANSPORT'.
        me->zif_integracao_inject~at_id_interface      = '302'.
        me->at_body_transp = set_montar_json_transport( ).
        /ui2/cl_json=>serialize( EXPORTING data = me->at_body_transp pretty_name = /ui2/cl_json=>pretty_mode-camel_case RECEIVING r_json = e_data ).

      WHEN 'VEICULO'.
        me->zif_integracao_inject~at_id_interface      = '306'.
        me->at_body_veicul = set_montar_json_veiculo( ).
        /ui2/cl_json=>serialize( EXPORTING data = me->at_body_veicul pretty_name = /ui2/cl_json=>pretty_mode-camel_case RECEIVING r_json = e_data ).

      WHEN 'ANEXOS'.
        me->zif_integracao_inject~at_id_interface      = '305'.
        me->at_body_anexos = set_montar_json_anexos( ).
        /ui2/cl_json=>serialize( EXPORTING data = me->at_body_anexos pretty_name = /ui2/cl_json=>pretty_mode-camel_case RECEIVING r_json = e_data ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound   = me.

    CASE me->at_endpoint.
      WHEN 'TRANSPORT'.
        e_referencia-tp_referencia = 'OB_SAFRA_TRACK_TRANSPORT'.
        e_referencia-id_referencia = me->at_id_referencia.

      WHEN 'ANEXOS'.
        e_referencia-tp_referencia = 'OB_SAFRA_TRACK_ANEXOS'.
        e_referencia-id_referencia = me->at_id_referencia.

      WHEN 'VEICULO'.
        e_referencia-tp_referencia = 'OB_SAFRA_TRACK_VEICULO'.
        e_referencia-id_referencia = me->at_id_referencia.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.
    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_safra_crt_crg_track.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.
  ENDMETHOD.


  METHOD zif_integracao_outbound~send_msg.
    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_outbound = me.

    CREATE OBJECT lc_integrar.

    "// Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg( IMPORTING e_integracao = e_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.
  ENDMETHOD.


  METHOD zif_integracao_outbound~set_data.

    r_if_integracao_outbound = me.

    FREE: me->at_ds_body.

    CASE me->at_metodo_http.

      WHEN zif_integracao_inject=>co_request_method_post.
        me->at_ds_body                = i_data.
        me->at_externalid-external_id = me->at_id_referencia.

      WHEN zif_integracao_inject=>co_request_method_put.
        me->at_ds_body                = i_data.
        me->at_externalid-external_id = me->at_id_referencia.

      WHEN zif_integracao_inject=>co_request_method_get.
        me->at_externalid-external_id = me->at_id_referencia.
    ENDCASE.

    CONDENSE me->at_externalid-external_id NO-GAPS.

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_id_referencia.
    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_integracao_outbound~set_url.

    r_if_integracao_outbound = me.

    DATA(lv_metodo)     = at_metodo_http.
    DATA(lv_externalid) = at_externalid-external_id.
    DATA(lv_force)      = at_externalid-force.
    DATA(lv_type)       = at_externalid-type.
    DATA(lv_upsert)     = at_externalid-upsert.

    CASE me->at_endpoint.
      WHEN 'TRANSPORT' OR 'ANEXOS'.
        SELECT SINGLE *
          FROM zauth_webservice
          INTO @DATA(lwa_webservice)
         WHERE service EQ @lc_servico.

      WHEN 'VEICULO'.
        SELECT SINGLE *
          FROM zauth_webservice
          INTO @lwa_webservice
         WHERE service EQ @lc_servico_veic.
    ENDCASE.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = CONV #( TEXT-001 ) "// Serviço não configurado:
                            attr2 = CONV #( me->lc_servico ) )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = me->lc_erro "// E
          msgv1  = CONV #( TEXT-001 ) "// Serviço não configurado:
          msgv2  = CONV #( me->lc_servico ).
    ENDIF.

    CASE lv_metodo.
      WHEN zif_integracao_inject=>co_request_method_post.
        CASE me->at_endpoint.
          WHEN 'TRANSPORT'.
            DATA(v_url) = |{ lwa_webservice-url }/{ me->at_id_inc }/{ lwa_webservice-add01 } |.

          WHEN 'VEICULO'.
            v_url       = |{ lwa_webservice-url }|.

          WHEN 'ANEXOS'.
            v_url       = |{ lwa_webservice-url }/{ me->at_id_inc }/{ lwa_webservice-add02 } |.
        ENDCASE.
    ENDCASE.

    CLEAR: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http = VALUE #( ds_formato            = me->lc_content_type
                                                              ds_content_type       = lwa_webservice-content_type
                                                              ds_url                = v_url
                                                              ds_body               = me->at_ds_body
*                                                             ds_url_token          = lwa_webservice-token
                                                              ds_metodo             = lv_metodo
                                                              ds_not_content_length = abap_false ).

    "// Informação do header
    APPEND VALUE #( name = me->lc_authorization value = lwa_webservice-token )     TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
