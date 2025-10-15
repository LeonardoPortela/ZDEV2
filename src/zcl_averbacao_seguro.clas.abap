class ZCL_AVERBACAO_SEGURO definition
  public
  inheriting from ZCL_MONTA_XML
  final
  create public .

public section.

  interfaces ZIF_CADASTRO .
  interfaces ZIF_PESQUISA .

  aliases CK_ALTEROU
    for ZIF_CADASTRO~CK_ALTEROU .
  aliases EXCLUIR_REGISTRO
    for ZIF_CADASTRO~EXCLUIR_REGISTRO .
  aliases GET_REGISTRO
    for ZIF_CADASTRO~GET_REGISTRO .
  aliases GRAVAR_REGISTRO
    for ZIF_CADASTRO~GRAVAR_REGISTRO .
  aliases LIMPAR_REGISTRO
    for ZIF_CADASTRO~LIMPAR_REGISTRO .
  aliases NOVO_REGISTRO
    for ZIF_CADASTRO~NOVO_REGISTRO .
  aliases SET_REGISTRO
    for ZIF_CADASTRO~SET_REGISTRO .
  aliases VALIDAR_EXCLUSAO
    for ZIF_CADASTRO~VALIDAR_EXCLUSAO .
  aliases VALIDAR_REGISTRO
    for ZIF_CADASTRO~VALIDAR_REGISTRO .
  aliases VALIDA_ATRIBUTO_ALTERAVEL
    for ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL .

  data CK_SALVAR_XML_LOCAL type CHAR01 .
  constants TP_DOCUMENTO_NFE type ZDE_TP_DOC_AVERB value 'NF-E' ##NO_TEXT.
  constants TP_DOCUMENTO_CTE type ZDE_TP_DOC_AVERB value 'CT-E' ##NO_TEXT.
  constants TP_DOCUMENTO_MDFE type ZDE_TP_DOC_AVERB value 'MDF-E' ##NO_TEXT.
  data CANCELANDO type CHAR01 .

  class-methods CK_DOCNUM_SEGURO
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_CANCELAR type CHAR01 default ' '
    exporting
      !E_J_1BNFDOC type J_1BNFDOC
      !E_ZLEST0115 type ZLEST0115
    returning
      value(R_VALIDADO) type CHAR01
    raising
      ZCX_AVERBACAO_SEGURO .
  class-methods GET_FILE_XML_CTE
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_LOCAL type CHAR01 default ' '
      !I_CANCELAR type CHAR01 default ' '
    returning
      value(R_ARQUIVO) type ZDE_WALINE_STRING_T
    raising
      ZCX_AVERBACAO_SEGURO
      ZCX_ARQUIVO
      ZCX_DOC_ELETRONICO .
  methods SET_XML_CTE
    importing
      !I_LOCAL type CHAR01 default ' '
      !I_CANCELAR type CHAR01 default ' '
    raising
      ZCX_AVERBACAO_SEGURO
      ZCX_ARQUIVO
      ZCX_DOC_ELETRONICO .
  methods SET_XML_NFE
    importing
      !I_LOCAL type CHAR01 optional
      !I_CANCELAR type CHAR01 optional
    raising
      ZCX_SQL_API_PYTHON .
  methods AVERBAR_SEGURO
    importing
      !I_CANCELAR type CHAR01 default ' '
    raising
      ZCX_AVERBACAO_SEGURO
      ZCX_ARQUIVO
      ZCX_DOC_ELETRONICO .
  methods SET_CK_BUSCAR_LOCAL
    importing
      !I_CK_BUSCAR_LOCAL type CHAR01 .
  methods GET_AVERBACAO
    returning
      value(R_ZLEST0143) type ZLEST0143 .
  methods SET_DOCNUM
    importing
      !I_DOCNUM type J_1BDOCNUM
    raising
      ZCX_AVERBACAO_SEGURO .
  class-methods CANCELAR_AVERBACAO_CTE
    importing
      !I_DOCNUM type J_1BDOCNUM
    exporting
      value(E_ZLEST0143) type ZLEST0143
    returning
      value(R_AUTORIZADO) type CHAR01
    raising
      ZCX_AVERBACAO_SEGURO
      ZCX_ARQUIVO
      ZCX_CADASTRO .
  class-methods EMITIR_AVERBACAO_CTE
    importing
      !I_DOCNUM type J_1BDOCNUM
    exporting
      value(E_ZLEST0143) type ZLEST0143
    returning
      value(R_AUTORIZADO) type CHAR01
    raising
      ZCX_AVERBACAO_SEGURO
      ZCX_ARQUIVO
      ZCX_CADASTRO
      ZCX_DOC_ELETRONICO .
  class-methods GET_INFO_SEGURADORA
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_DOCDAT type ERDAT optional
    returning
      value(E_ZLEST0115) type ZLEST0115 .
  methods GET_INFO_SEGURADORA_NFE
    importing
      !IT_DOCNUM type J_1BNFE_T_DOCNUM
      !I_DOCDAT type ERDAT optional
    returning
      value(E_ZLEST0115) type ZLEST0115 .
protected section.
private section.

  data AVERBACAO type ZLEST0143 .
  data TEXTO_DOCUMENTO type STRING .
  data CK_BUSCAR_LOCAL type CHAR01 .

  methods SET_CD_SEGURADORA
    importing
      !I_CD_SEGURADORA type LIFNR .
  methods SET_CD_TOKEN
    importing
      !I_CD_TOKEN type ZDE_CD_TOKEN .
  methods SET_TP_DOCUMENTO
    importing
      !I_TP_DOCUMENTO type ZDE_TP_DOC_AVERB .
  methods SET_NR_AVERBACAO
    importing
      !I_NR_AVERBACAO type ZDE_NR_AVERBACAO .
  methods SET_NR_PROTOCOLO
    importing
      !I_NR_PROTOCOLO type ZDE_NR_PROTOCOLO .
  methods SET_NR_SEQUENCIA
    importing
      !I_NR_SEQUENCIA type ZDE_NR_SEQUENCIA .
  methods SET_DT_AUTORIZACAO
    importing
      !I_DT_AUTORIZACAO type ZDE_DT_AUTORIZA .
  methods SET_HR_AUTORIZACAO
    importing
      !I_HR_AUTORIZACAO type ZDE_HR_AUTORIZA .
  methods SET_CANCEL_AVERBA
    importing
      !I_CANCEL_AVERBACAO type ZDE_CANCEL_AVERBA .
  methods SET_NR_PROTOCOLO_CAN
    importing
      !I_NR_PROTOCOLO_CAN type ZDE_NR_PROTOCOLO .
  methods SET_NR_SEQUENCIA_CAN
    importing
      !I_NR_SEQUENCIA_CAN type ZDE_NR_SEQUENCIA .
  methods SET_DT_AUTORIZACAO_CAN
    importing
      !I_DT_AUTORIZACAO_CAN type ZDE_DT_AUTORIZA .
  methods SET_HR_AUTORIZACAO_CAN
    importing
      !I_HR_AUTORIZACAO_CAN type ZDE_HR_AUTORIZA .
  methods SET_XML_CANCELAMENTO
    raising
      ZCX_AVERBACAO_SEGURO
      ZCX_ARQUIVO
      ZCX_DOC_ELETRONICO .
ENDCLASS.



CLASS ZCL_AVERBACAO_SEGURO IMPLEMENTATION.


  METHOD averbar_seguro.

    DATA: lc_xml_ret       TYPE REF TO cl_xml_document,
          i_nr_protocolo   TYPE zde_nr_protocolo,
          i_nr_averbacao   TYPE zde_nr_averbacao,
          i_nr_sequencia   TYPE zde_nr_sequencia,
          i_dt_autorizacao TYPE zde_dt_autoriza,
          i_hr_autorizacao TYPE zde_hr_autoriza,
          lc_texto         TYPE c LENGTH 200.

    DATA: i_name_file TYPE string,
          xml_input   TYPE string.

    SELECT SINGLE * INTO @DATA(wa_zlest0144) FROM zlest0144 WHERE cd_seguradora EQ @me->averbacao-cd_seguradora.

    IF sy-subrc IS NOT INITIAL.

      MESSAGE ID zcx_averbacao_seguro=>zcx_sem_webservice-msgid
         TYPE 'S'
         NUMBER zcx_averbacao_seguro=>zcx_sem_webservice-msgno
         WITH me->averbacao-cd_seguradora
        DISPLAY LIKE 'E'.

      RAISE EXCEPTION TYPE zcx_averbacao_seguro
        EXPORTING
          textid    = VALUE #( msgid = zcx_averbacao_seguro=>zcx_sem_webservice-msgid
                               msgno = zcx_averbacao_seguro=>zcx_sem_webservice-msgno
                               attr1 = CONV #( me->averbacao-cd_seguradora ) )
          msgty     = 'E'
          msgid     = zcx_averbacao_seguro=>zcx_sem_webservice-msgid
          msgno     = zcx_averbacao_seguro=>zcx_sem_webservice-msgno
          msgv1     = CONV #( me->averbacao-cd_seguradora )
          transacao = 'ZLES0148'.
    ENDIF.

    if i_cancelar eq abap_true.

      select single *
        from zlest0143 INTO @DATA(lwa_zlest0143)
       WHERE docnum eq @me->averbacao-docnum.

      IF ( sy-subrc eq 0 ) AND ( lwa_zlest0143-cd_token is NOT INITIAL ).
        select SINGLE *
          from zlest0145 INTO @DATA(wa_zlest0145)
         where cd_token eq @lwa_zlest0143-cd_token.
      else.
        sy-subrc = 4.
      endif.

    else.

      SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc) FROM j_1bnfdoc WHERE docnum EQ @me->averbacao-docnum.

      CASE wa_j_1bnfdoc-model.
        WHEN zif_doc_eletronico=>at_st_model_cte.

          SELECT SINGLE * INTO @wa_zlest0145
            FROM zlest0145
           WHERE bukrs       EQ @wa_j_1bnfdoc-bukrs
             AND branch      EQ @wa_j_1bnfdoc-branch
             AND dt_validade GE @sy-datum.

        WHEN zif_doc_eletronico=>at_st_model_nfe.

          "Somente emite pela nota fiscal a averbação por Veículo Próprio e Mercadoria Própria
          zcl_faturamento=>zif_faturamento~get_instance(
            )->get_processo_emissao_docs(
                 EXPORTING
                   i_docnum = me->averbacao-docnum
                 IMPORTING
                   e_uf_origem_mercadoria = DATA(e_uf_origem_mercadoria)
                   e_placa_cavalo         = DATA(e_placa_cavalo)
            )->get_agente_frete(
                 EXPORTING
                   i_placa                = e_placa_cavalo
                   i_uf_origem_mercadoria = e_uf_origem_mercadoria
                   i_bukrs                = wa_j_1bnfdoc-bukrs "CS2022000236 - 25.02.2022 - JT - fim
                 IMPORTING
                   e_agente_frete         = DATA(e_agente_frete)    " Nº conta do fornecedor
            ).

          SELECT SINGLE * INTO @wa_zlest0145
            FROM zlest0145 AS a
           WHERE bukrs       EQ @wa_j_1bnfdoc-bukrs
             AND branch      EQ @e_agente_frete+6(4)
             AND dt_validade GE @sy-datum
             AND dt_validade EQ ( SELECT MIN( b~dt_validade )
                                    FROM zlest0145 AS b
                                   WHERE b~bukrs       EQ @wa_j_1bnfdoc-bukrs
                                     AND b~branch      EQ @e_agente_frete+6(4)
                                     AND b~dt_validade GE @sy-datum ).
      ENDCASE.

    endif.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID zcx_averbacao_seguro=>zcx_token_nao_existe-msgid
         TYPE 'S'
         NUMBER zcx_averbacao_seguro=>zcx_token_nao_existe-msgno
         WITH wa_j_1bnfdoc-bukrs wa_j_1bnfdoc-branch sy-datum
        DISPLAY LIKE 'E'.

      RAISE EXCEPTION TYPE zcx_averbacao_seguro
        EXPORTING
          textid    = VALUE #( msgid = zcx_averbacao_seguro=>zcx_token_nao_existe-msgid
                               msgno = zcx_averbacao_seguro=>zcx_token_nao_existe-msgno
                               attr1 = CONV #( wa_j_1bnfdoc-bukrs )
                               attr2 = CONV #( wa_j_1bnfdoc-branch )
                               attr3 = CONV #( sy-datum ) )
          msgty     = 'E'
          msgid     = zcx_averbacao_seguro=>zcx_token_nao_existe-msgid
          msgno     = zcx_averbacao_seguro=>zcx_token_nao_existe-msgno
          msgv1     = CONV #( wa_j_1bnfdoc-bukrs )
          msgv2     = CONV #( wa_j_1bnfdoc-branch )
          msgv3     = CONV #( sy-datum )
          transacao = 'ZLES0148'.
    ENDIF.

    me->set_cd_token( i_cd_token = wa_zlest0145-cd_token ).

    "57	Conhecimento de Transporte eletrônico (CT-e) Modelo 57
    "55	Nota Fiscal de Consumidor Eletrônica
    "58	Manifesto Eletrônico de Documentos Fiscais (MDF-e) Model 58

    IF I_CANCELAR EQ ABAP_TRUE.
       me->set_xml_cancelamento( ).
    ELSE.
      CASE wa_j_1bnfdoc-model.
        WHEN 55.
          me->set_tp_documento( i_tp_documento = zcl_averbacao_seguro=>tp_documento_nfe ).
          me->set_xml_nfe( i_local = me->ck_buscar_local i_cancelar = i_cancelar ).
        WHEN 57.
          me->set_tp_documento( i_tp_documento = zcl_averbacao_seguro=>tp_documento_cte ).
          me->set_xml_cte( i_local = me->ck_buscar_local i_cancelar = i_cancelar ).
        WHEN 58.
          me->set_tp_documento( i_tp_documento = zcl_averbacao_seguro=>tp_documento_mdfe ).
          "ME->SET_XML_MDFE( I_LOCAL = ME->CK_BUSCAR_LOCAL ).
      ENDCASE.
    ENDIF.

    me->zif_cadastro~gravar_registro( ).

    zcl_averbacao_seguro=>ck_docnum_seguro(
       EXPORTING
         i_docnum   = me->averbacao-docnum
         i_cancelar = i_cancelar ).

    "Check Contingência
    IF i_cancelar IS INITIAL.
      SELECT SINGLE * INTO @DATA(_wl_zlest0162) FROM zlest0162 WHERE ck_contingencia EQ @abap_true.
      IF sy-subrc = 0.
        i_nr_averbacao = '99999'.
        me->set_nr_averbacao( i_nr_averbacao = i_nr_averbacao ).
        me->zif_cadastro~gravar_registro( ).
        EXIT.
      ENDIF.
    ENDIF.
    "Fim Check Contingência

    me->limpar( ).

    me->ctna(  texto = '<?xml version="1.0"?>' ).
    me->ctna(  texto = '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' ).
    me->ctna_separador( texto = 'xmlns:xsd="http://www.w3.org/2001/XMLSchema"' separador = space ).
    me->ctna_separador( texto = 'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'  separador = space ).
    me->ctna_separador( texto = 'xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/">'  separador = space ).

    me->ctna(  texto = '<SOAP-ENV:Body SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' ).
    me->ctna(  texto = '<NS1:AverbaXML xmlns:NS1="urn:aAverbaIntf-IAverba">' ).

    "<TOKEN XSI:TYPE="xsd:string">77294254001670QRC180101.TESTE</Token>
    me->ctnab( tag = 'Token xsi:type="xsd:string"' ).
    me->ctna(  texto = wa_zlest0145-nr_token ).
    me->ctnfe( tag = 'Token').

    "<ID XSI:TYPE="xsd:string">CTE.XML</ID>
    me->ctnab( tag = 'ID XSI:TYPE="xsd:string"' ).
    CASE me->averbacao-tp_documento.
      WHEN 'CT-E'.
        me->ctna(  texto = 'CTE.XML' ).
      WHEN 'MDF-E'.
        me->ctna(  texto = 'MDFE.XML' ).
      WHEN 'NF-E'.
        me->ctna(  texto = 'NFE.XML' ).
    ENDCASE.
    me->ctnfe( tag = 'ID').

    "<XML XSI:TYPE="xsd:string"> <![CDATA[]]> </XML>
    me->ctnab( tag = 'XML XSI:TYPE="xsd:string"' ).
    me->ctna(  texto = '<![CDATA[' ).
    me->ctna(  texto = me->texto_documento ).
    me->ctna(  texto = ']]>').
    me->ctnfe( tag = 'XML').

    me->ctna(   EXPORTING texto = '</NS1:AverbaXML>').
    me->ctna(   EXPORTING texto = '</SOAP-ENV:Body>').
    me->ctna(   EXPORTING texto = '</SOAP-ENV:Envelope>').

    IF me->ck_salvar_xml_local EQ abap_true.
      CONCATENATE 'C:\Maggi\XML\AverbaSeguro' me->averbacao-docnum '.xml' INTO i_name_file.
      me->salva_xml( i_name_file = i_name_file ).
    ENDIF.

    TRY .
        DATA(var_http) = me->url( i_url = wa_zlest0144-uri ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
      CATCH zcx_webservice INTO DATA(lc_exception).
        DATA(lc_msg) = lc_exception->get_text( ).

        MESSAGE ID lc_exception->if_t100_message~t100key-msgid
           TYPE 'S'
           NUMBER lc_exception->if_t100_message~t100key-msgno
           WITH lc_exception->if_t100_message~t100key-attr1
                lc_exception->if_t100_message~t100key-attr2
                lc_exception->if_t100_message~t100key-attr3
                lc_exception->if_t100_message~t100key-attr4
          DISPLAY LIKE 'E'.

        RAISE EXCEPTION TYPE zcx_averbacao_seguro
          EXPORTING
            textid = lc_exception->if_t100_message~t100key
            msgty  = 'E'
            msgno  = lc_exception->if_t100_message~t100key-msgno
            msgv1  = CONV #( lc_exception->if_t100_message~t100key-attr1 )
            msgv2  = CONV #( lc_exception->if_t100_message~t100key-attr2 )
            msgv3  = CONV #( lc_exception->if_t100_message~t100key-attr3 )
            msgv4  = CONV #( lc_exception->if_t100_message~t100key-attr4 )
            msgid  = lc_exception->if_t100_message~t100key-msgid.
    ENDTRY.

    me->zif_webservice~abrir_conexao( i_http = var_http ).

    me->get_xml( IMPORTING e_xml_texto = xml_input ).

    me->zif_webservice~consultar(
      EXPORTING
        i_http                     = var_http
        i_xml                      = xml_input
        i_not_content_length       = abap_true
      RECEIVING
        e_resultado                =  DATA(xml_retorno)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    IF sy-subrc IS NOT INITIAL.

      MESSAGE ID sy-msgid
         TYPE 'S'
         NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

      RAISE EXCEPTION TYPE zcx_averbacao_seguro
        EXPORTING
          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
          msgty  = 'E'
          msgno  = sy-msgno
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4
          msgid  = sy-msgid.
    ENDIF.

    IF me->ck_salvar_xml_local EQ abap_true.
      CONCATENATE 'C:\Maggi\XML\AverbaSeguroRet' me->averbacao-docnum '.xml' INTO i_name_file.
      me->salva_xml( i_name_file = i_name_file i_xml = xml_retorno ).
    ENDIF.

    CREATE OBJECT lc_xml_ret.

    DATA(lc_tamanho)    = lc_xml_ret->parse_string( EXPORTING stream  = xml_retorno ).
    DATA(lc_xml_return) = lc_xml_ret->find_node( EXPORTING name = 'return' ).

    IF lc_xml_return IS INITIAL.
      "I_NR_AVERBACAO = '99999'.
      IF i_cancelar IS INITIAL.
        me->set_nr_averbacao( i_nr_averbacao = i_nr_averbacao ).
      ENDIF.
      me->zif_cadastro~gravar_registro( ).
      EXIT.
    ENDIF.

    xml_retorno = lc_xml_return->get_value( ).
    lc_tamanho = lc_xml_ret->parse_string( stream  = xml_retorno ).

    DATA(lc_xml_req)    = lc_xml_ret->find_node( EXPORTING name = 'Request' ).
    DATA(lc_xml_node)   = lc_xml_ret->find_node( EXPORTING name = 'Response' ).

    IF lc_xml_req IS NOT INITIAL.

      DATA(lc_iterator_req) = lc_xml_req->create_iterator( ).
      DATA(lc_xml_node_req) = lc_iterator_req->get_next( ).

      WHILE NOT lc_xml_node_req IS INITIAL.

        CASE lc_xml_node_req->get_type( ).
          WHEN: if_ixml_node=>co_node_element.
            DATA(lc_string) = lc_xml_node_req->get_name( ).
            DATA(lc_valor)  = lc_xml_node_req->get_value( ).
            CASE lc_string.
              WHEN 'Sequence'.
                i_nr_sequencia = lc_valor.
                IF i_cancelar IS INITIAL.
                  me->set_nr_sequencia( i_nr_sequencia = i_nr_sequencia	).
                ELSE.
                  me->set_nr_sequencia_can( i_nr_sequencia_can = i_nr_sequencia	).
                ENDIF.
              WHEN 'DateTime'.
                CONCATENATE lc_valor(4) lc_valor+5(2) lc_valor+8(2) INTO i_dt_autorizacao.
                lc_valor = lc_valor+11(8).
                CONCATENATE lc_valor(2) lc_valor+3(2) lc_valor+6(2) INTO i_hr_autorizacao.
                IF i_cancelar IS INITIAL.
                  me->set_dt_autorizacao( i_dt_autorizacao = i_dt_autorizacao ).
                  me->set_hr_autorizacao( i_hr_autorizacao = i_hr_autorizacao ).
                ELSE.
                  me->set_dt_autorizacao_can( i_dt_autorizacao_can = i_dt_autorizacao ).
                  me->set_hr_autorizacao_can( i_hr_autorizacao_can = i_hr_autorizacao ).
                ENDIF.
            ENDCASE.
        ENDCASE.

        lc_xml_node_req = lc_iterator_req->get_next( ).
      ENDWHILE.

    ENDIF.

    IF lc_xml_node IS NOT INITIAL.

      DATA(lc_iterator_doc) = lc_xml_node->create_iterator( ).
      DATA(lc_xml_node_doc) = lc_iterator_doc->get_next( ).

      WHILE NOT lc_xml_node_doc IS INITIAL.

        CASE lc_xml_node_doc->get_type( ).
          WHEN: if_ixml_node=>co_node_element.
            lc_string = lc_xml_node_doc->get_name( ).
            lc_valor  = lc_xml_node_doc->get_value( ).
            CASE lc_string.
              WHEN 'Averbacao'.
                i_nr_averbacao = lc_valor.
                IF i_cancelar IS INITIAL.
                  me->set_nr_averbacao( i_nr_averbacao = i_nr_averbacao ).
                ENDIF.
              WHEN 'Protocolo'.
                i_nr_protocolo = lc_valor.
                IF i_cancelar IS INITIAL.
                  me->set_nr_protocolo( i_nr_protocolo = i_nr_protocolo ).
                ELSE.
                  me->set_nr_protocolo_can( i_nr_protocolo_can = i_nr_protocolo ).
                ENDIF.
              WHEN 'Erro'.
                lc_texto = lc_valor.

                CONCATENATE '(WSC) -' lc_texto INTO lc_texto SEPARATED BY space.

                sy-msgv1 = lc_texto+000(50).
                sy-msgv2 = lc_texto+050(50).
                sy-msgv3 = lc_texto+100(50).
                sy-msgv4 = lc_texto+150(50).
                lc_xml_ret->free( ).
                CLEAR: lc_xml_ret.

                MESSAGE ID zcx_averbacao_seguro=>zcx_erro_generico-msgid
                   TYPE 'S'
                   NUMBER zcx_averbacao_seguro=>zcx_erro_generico-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

                RAISE EXCEPTION TYPE zcx_averbacao_seguro
                  EXPORTING
                    textid = VALUE #( msgid = zcx_averbacao_seguro=>zcx_erro_generico-msgid
                                      msgno = zcx_averbacao_seguro=>zcx_erro_generico-msgno
                                      attr1 = CONV #( sy-msgv1 )
                                      attr2 = CONV #( sy-msgv2 )
                                      attr3 = CONV #( sy-msgv3 )
                                      attr4 = CONV #( sy-msgv4 ) )
                    msgty  = 'E'
                    msgid  = zcx_averbacao_seguro=>zcx_erro_generico-msgid
                    msgno  = zcx_averbacao_seguro=>zcx_erro_generico-msgno
                    msgv1  = sy-msgv1
                    msgv2  = sy-msgv2
                    msgv3  = sy-msgv3
                    msgv4  = sy-msgv4.
            ENDCASE.
        ENDCASE.

        lc_xml_node_doc = lc_iterator_doc->get_next( ).
      ENDWHILE.

    ENDIF.

    IF lc_xml_req IS INITIAL AND lc_xml_node IS INITIAL.
      lc_texto = xml_retorno.
      sy-msgv1 = lc_texto+000(50).
      sy-msgv2 = lc_texto+050(50).
      sy-msgv3 = lc_texto+100(50).
      sy-msgv4 = lc_texto+150(50).
      lc_xml_ret->free( ).
      CLEAR: lc_xml_ret.

      MESSAGE ID zcx_averbacao_seguro=>zcx_erro_generico-msgid
         TYPE 'S'
         NUMBER zcx_averbacao_seguro=>zcx_erro_generico-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

      RAISE EXCEPTION TYPE zcx_averbacao_seguro
        EXPORTING
          textid = VALUE #( msgid = zcx_averbacao_seguro=>zcx_erro_generico-msgid
                            msgno = zcx_averbacao_seguro=>zcx_erro_generico-msgno
                            attr1 = CONV #( sy-msgv1 )
                            attr2 = CONV #( sy-msgv2 )
                            attr3 = CONV #( sy-msgv3 )
                            attr4 = CONV #( sy-msgv4 ) )
          msgty  = 'E'
          msgid  = zcx_averbacao_seguro=>zcx_erro_generico-msgid
          msgno  = zcx_averbacao_seguro=>zcx_erro_generico-msgno
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    IF i_cancelar IS NOT INITIAL.

    ENDIF.

    me->zif_cadastro~gravar_registro( ).
    lc_xml_ret->free( ).
    CLEAR: lc_xml_ret.

  ENDMETHOD.


  METHOD CANCELAR_AVERBACAO_CTE.

    DATA: OBJ_AVERBACAO TYPE REF TO ZCL_AVERBACAO_SEGURO,
          WA_J_1BNFDOC  TYPE J_1BNFDOC.

    R_AUTORIZADO = ABAP_FALSE.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0143) FROM ZLEST0143 WHERE DOCNUM EQ @I_DOCNUM.

    IF SY-SUBRC IS INITIAL.

      CREATE OBJECT OBJ_AVERBACAO.
      OBJ_AVERBACAO->CANCELANDO = ABAP_TRUE.
      OBJ_AVERBACAO->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = WA_ZLEST0143-CD_AVERBACAO ).
      WA_ZLEST0143 = OBJ_AVERBACAO->GET_AVERBACAO( ).

      IF WA_ZLEST0143-NR_PROTOCOLO IS NOT INITIAL AND WA_ZLEST0143-NR_PROTOCOLO NE '99999' AND E_ZLEST0143-NR_PROTOCOLO_CAN IS INITIAL.
        OBJ_AVERBACAO->AVERBAR_SEGURO( I_CANCELAR = ABAP_TRUE ).
        E_ZLEST0143 = OBJ_AVERBACAO->GET_AVERBACAO( ).

        IF E_ZLEST0143-NR_PROTOCOLO_CAN IS NOT INITIAL.
          R_AUTORIZADO = ABAP_TRUE.
        ELSE.
*          RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
*            EXPORTING
*              TEXTID = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGID
*                                MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGNO
*                                ATTR1 = CONV #( WA_J_1BNFDOC-NFENUM ) )
*              MSGID  = ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGID
*              MSGNO  = ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGNO
*              MSGTY  = 'E'
*              MSGV1  = CONV #( WA_J_1BNFDOC-NFENUM ).
        ENDIF.
      ELSE.
        R_AUTORIZADO = ABAP_TRUE.
      ENDIF.

      CLEAR: OBJ_AVERBACAO.

    ENDIF.

  ENDMETHOD.


  METHOD CK_DOCNUM_SEGURO.

    DATA: P_AUTORIZADO TYPE CHAR01.

    CLEAR: E_ZLEST0115.

    R_VALIDADO = ABAP_FALSE.

    IF I_CANCELAR EQ ABAP_TRUE.
      SELECT SINGLE *
        FROM ZLEST0143 INTO @DATA(LWA_ZLEST0143)
       WHERE DOCNUM EQ @I_DOCNUM.

      IF SY-SUBRC EQ 0.
        R_VALIDADO = ABAP_TRUE.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO E_J_1BNFDOC
      FROM J_1BNFDOC
     WHERE DOCNUM EQ I_DOCNUM.

    TRY .
        ZCL_FATURAMENTO=>ZIF_FATURAMENTO~GET_INSTANCE(
          )->GET_PROCESSO_EMISSAO_DOCS( EXPORTING I_DOCNUM = I_DOCNUM
          )->GET_CK_AVERBA_SEGURO( EXPORTING I_DOCNUM = I_DOCNUM
          ).
      CATCH ZCX_FATURAMENTO.    "
        RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGID
                              MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGNO )
            MSGID  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGID
            MSGNO  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGNO
            MSGTY  = 'E'.
      CATCH ZCX_ERROR.    " .
        RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGID
                              MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGNO )
            MSGID  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGID
            MSGNO  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGNO
            MSGTY  = 'E'.
    ENDTRY.

    IF E_J_1BNFDOC-CANCEL EQ ABAP_TRUE AND I_CANCELAR EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGID
                            MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGNO )
          MSGID  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGID
          MSGNO  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_ERRADO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    CALL FUNCTION 'Z_NFE_CTE_AUTORIZADO'
      EXPORTING
        P_DOCNUM      = I_DOCNUM
      IMPORTING
        P_AUTORIZADO  = P_AUTORIZADO
      EXCEPTIONS
        CANCELADO     = 1
        NAO_CANCELADO = 2
        PENDENTE      = 3
        NAO_CONCLUIDO = 4
        NAO_EXISTE    = 5
        OTHERS        = 6.

    IF SY-SUBRC IS NOT INITIAL AND I_CANCELAR EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    E_ZLEST0115 = ZCL_AVERBACAO_SEGURO=>GET_INFO_SEGURADORA( I_DOCNUM = I_DOCNUM ).

    CASE E_J_1BNFDOC-MODEL.
      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.

        SELECT SINGLE * INTO @DATA(WA_ZCTE_IDENTIFICA)
          FROM ZCTE_IDENTIFICA
         WHERE DOCNUM EQ @I_DOCNUM.

        IF SY-SUBRC IS NOT INITIAL. "OR WA_ZCTE_IDENTIFICA-VLR_SEGURO IS INITIAL.
          RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_VALOR_SEGURO-MSGID
                                MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_VALOR_SEGURO-MSGNO )
              MSGID  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_VALOR_SEGURO-MSGID
              MSGNO  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_VALOR_SEGURO-MSGNO
              MSGTY  = 'E'.
        ENDIF.

        "008  Não encontrado registro de Seguradora p/ esta CT-e!
        "009  Registro de Seguro não possui uma Seguradora (Código Fornecedor)!

        SELECT SINGLE * INTO @DATA(WA_ZCTE_SEGURO)
          FROM ZCTE_SEGURO
         WHERE DOCNUM EQ @I_DOCNUM.

        IF SY-SUBRC IS NOT INITIAL.
          RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_REGISTRO_SEG-MSGID
                                MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_REGISTRO_SEG-MSGNO )
              MSGID  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_REGISTRO_SEG-MSGID
              MSGNO  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_REGISTRO_SEG-MSGNO
              MSGTY  = 'E'.
        ENDIF.

      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.

    ENDCASE.

    IF E_ZLEST0115-CD_FORNECEDOR IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_SEGURADORA-MSGID
                            MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_SEGURADORA-MSGNO )
          MSGID  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_SEGURADORA-MSGID
          MSGNO  = ZCX_AVERBACAO_SEGURO=>ZCX_DOC_SEM_SEGURADORA-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    R_VALIDADO = ABAP_TRUE.

  ENDMETHOD.


  METHOD EMITIR_AVERBACAO_CTE.

    DATA: OBJ_AVERBACAO TYPE REF TO ZCL_AVERBACAO_SEGURO,
          WA_J_1BNFDOC  TYPE J_1BNFDOC.

    R_AUTORIZADO = ABAP_FALSE.

    DATA(R_VALIDADO) = ZCL_AVERBACAO_SEGURO=>CK_DOCNUM_SEGURO( EXPORTING I_DOCNUM   = I_DOCNUM IMPORTING E_J_1BNFDOC = WA_J_1BNFDOC ).

    CREATE OBJECT OBJ_AVERBACAO.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0143) FROM ZLEST0143 WHERE DOCNUM EQ @I_DOCNUM.

    IF SY-SUBRC IS INITIAL.
      OBJ_AVERBACAO->ZIF_CADASTRO~SET_REGISTRO( I_ID_REGISTRO = WA_ZLEST0143-CD_AVERBACAO ).
      WA_ZLEST0143 = OBJ_AVERBACAO->GET_AVERBACAO( ).
    ELSE.
      OBJ_AVERBACAO->SET_DOCNUM( I_DOCNUM = I_DOCNUM ).
    ENDIF.

    IF WA_ZLEST0143-NR_PROTOCOLO IS INITIAL OR WA_ZLEST0143-NR_AVERBACAO EQ '99999'.
      OBJ_AVERBACAO->AVERBAR_SEGURO( ).
    ENDIF.

    E_ZLEST0143 = OBJ_AVERBACAO->GET_AVERBACAO( ).

    IF E_ZLEST0143-NR_AVERBACAO IS NOT INITIAL.
      R_AUTORIZADO = ABAP_TRUE.
    ELSE.

      MESSAGE ID ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGID
         TYPE 'S'
         NUMBER ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGNO
         WITH WA_J_1BNFDOC-NFENUM DISPLAY LIKE 'E'.

      RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGID
                            MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGNO
                            ATTR1 = CONV #( WA_J_1BNFDOC-NFENUM ) )
          MSGID  = ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGID
          MSGNO  = ZCX_AVERBACAO_SEGURO=>ZCX_NAO_AUTORIZADO-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( WA_J_1BNFDOC-NFENUM ).
    ENDIF.

    CLEAR: OBJ_AVERBACAO.

  ENDMETHOD.


  METHOD GET_AVERBACAO.
    R_ZLEST0143 = ME->AVERBACAO.
  ENDMETHOD.


  METHOD GET_FILE_XML_CTE.

*    DATA: DIRECTORY  TYPE STRING.

    DATA: ARQUIVO             TYPE REF TO ZCL_ARQUIVO,
          LC_DS_URL_DANFE_OUT TYPE  CHAR100.

    CLEAR: R_ARQUIVO.

*    SELECT SINGLE * INTO @DATA(WA_ZLEST0150) FROM ZLEST0150.
*
*    IF SY-SUBRC IS NOT INITIAL .
*      RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
*        EXPORTING
*          TEXTID    = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO-MSGID
*                               MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO-MSGNO )
*          MSGTY     = 'E'
*          MSGNO     = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO-MSGNO
*          MSGID     = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO-MSGID
*          TRANSACAO = 'ZLES0148'.
*    ENDIF.
*
*    IF SY-BATCH EQ ABAP_TRUE.
*
*      IF WA_ZLEST0150-DIR_XML_UNIX IS INITIAL.
*        RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
*          EXPORTING
*            TEXTID    = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_REDE_UNIX-MSGID
*                                 MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_REDE_UNIX-MSGNO )
*            MSGTY     = 'E'
*            MSGNO     = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_REDE_UNIX-MSGNO
*            MSGID     = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_REDE_UNIX-MSGID
*            TRANSACAO = 'ZLES0148'.
*      ENDIF.
*      DIRECTORY = WA_ZLEST0150-DIR_XML_UNIX.
*
*    ELSE.
*      CASE I_LOCAL.
*        WHEN ABAP_FALSE.
*
*          IF WA_ZLEST0150-DIR_XML_WIN_REDE IS INITIAL.
*            RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
*              EXPORTING
*                TEXTID    = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_REDE_WIN-MSGID
*                                     MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_REDE_WIN-MSGNO )
*                MSGTY     = 'E'
*                MSGNO     = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_REDE_WIN-MSGNO
*                MSGID     = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_REDE_WIN-MSGID
*                TRANSACAO = 'ZLES0148'.
*          ENDIF.
*          DIRECTORY = WA_ZLEST0150-DIR_XML_WIN_REDE.
*
*        WHEN ABAP_TRUE.
*
*          IF WA_ZLEST0150-DIR_XML_WIN IS INITIAL.
*            RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
*              EXPORTING
*                TEXTID    = VALUE #( MSGID = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_LC_WIN-MSGID
*                                     MSGNO = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_LC_WIN-MSGNO )
*                MSGTY     = 'E'
*                MSGNO     = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_LC_WIN-MSGNO
*                MSGID     = ZCX_AVERBACAO_SEGURO=>ZCX_SEM_DIRETORIO_LC_WIN-MSGID
*                TRANSACAO = 'ZLES0148'.
*          ENDIF.
*          DIRECTORY = WA_ZLEST0150-DIR_XML_WIN.
*
*      ENDCASE.
*
*    ENDIF.
*
*    CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST( EXPORTING DIRECTORY = DIRECTORY
*        EXCEPTIONS
*          CNTL_ERROR           = 1
*          ERROR_NO_GUI         = 2
*          WRONG_PARAMETER      = 3
*          NOT_SUPPORTED_BY_GUI = 4
*          OTHERS               = 5 ).
*
*    IF SY-SUBRC IS NOT INITIAL.
*      RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
*        EXPORTING
*          TEXTID = VALUE #( MSGID = SY-MSGID
*                            MSGNO = SY-MSGNO
*                            ATTR1 = CONV #( SY-MSGV1 )
*                            ATTR2 = CONV #( SY-MSGV2 )
*                            ATTR3 = CONV #( SY-MSGV3 )
*                            ATTR4 = CONV #( SY-MSGV4 ) )
*          MSGTY  = SY-MSGTY
*          MSGNO  = SY-MSGNO
*          MSGID  = SY-MSGID
*          MSGV1  = SY-MSGV1
*          MSGV2  = SY-MSGV2
*          MSGV3  = SY-MSGV3
*          MSGV4  = SY-MSGV4.
*    ENDIF.
*
    DATA(R_VALIDADO) = ZCL_AVERBACAO_SEGURO=>CK_DOCNUM_SEGURO( I_DOCNUM = I_DOCNUM I_CANCELAR = I_CANCELAR ).
*
    IF R_VALIDADO EQ ABAP_TRUE.
*      SELECT SINGLE * INTO @DATA(WA_ACTIVE) FROM J_1BNFE_ACTIVE WHERE DOCNUM EQ @I_DOCNUM.
*      IF I_CANCELAR IS INITIAL.
*        CONCATENATE DIRECTORY 'CTe'
*                    WA_ACTIVE-REGIO WA_ACTIVE-NFYEAR WA_ACTIVE-NFMONTH WA_ACTIVE-STCD1 WA_ACTIVE-MODEL WA_ACTIVE-SERIE WA_ACTIVE-NFNUM9 WA_ACTIVE-DOCNUM9 WA_ACTIVE-CDV '.xml'
*               INTO DIRECTORY.
*      ELSE.
*        CONCATENATE DIRECTORY 'CTe'
*                    WA_ACTIVE-REGIO WA_ACTIVE-NFYEAR WA_ACTIVE-NFMONTH WA_ACTIVE-STCD1 WA_ACTIVE-MODEL WA_ACTIVE-SERIE WA_ACTIVE-NFNUM9 WA_ACTIVE-DOCNUM9 WA_ACTIVE-CDV 'Cancel.xml'
*               INTO DIRECTORY.
*      ENDIF.
*
*      R_ARQUIVO = ZCL_ARQUIVO=>GET_FILE( I_FILENAME = DIRECTORY ).

*      SELECT SINGLE * INTO @DATA(WA_J_1BNFE_ACTIVE)
*        FROM J_1BNFE_ACTIVE
*       WHERE DOCNUM EQ @I_DOCNUM.
*
*      IF WA_J_1BNFE_ACTIVE-ACTIVE_SERVICE IS INITIAL.

      ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = I_DOCNUM
        )->SET_REGISTRO( EXPORTING I_DOCNUM       = I_DOCNUM
                                   I_SEM_BLOQUEIO = ABAP_TRUE
        ")->GET_URLS_DOCS( IMPORTING E_LINK_XML  = DATA(E_LINK_XML)
        )->GET_XML_GRC( IMPORTING E_XML_STRING = DATA(E_TEXTO) E_RETORNO = DATA(E_RETORNO)
        ).

      IF E_RETORNO IS INITIAL.
        MESSAGE S031 WITH I_DOCNUM.
      ELSE.
        MESSAGE S032 WITH I_DOCNUM.

        CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
          EXPORTING
            DOC_NUMERO       = I_DOCNUM
            IMPRIMIR         = ABAP_FALSE
          CHANGING
            DS_URL_DANFE_OUT = LC_DS_URL_DANFE_OUT
          EXCEPTIONS
            NAO_LOCALIZADO   = 1
            OTHERS           = 2.

        IF SY-SUBRC IS NOT INITIAL.
          RAISE EXCEPTION TYPE ZCX_AVERBACAO_SEGURO
            EXPORTING
              TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
              MSGTY  = SY-MSGTY
              MSGNO  = SY-MSGNO
              MSGID  = SY-MSGID
              MSGV1  = SY-MSGV1
              MSGV2  = SY-MSGV2
              MSGV3  = SY-MSGV3
              MSGV4  = SY-MSGV4.
        ENDIF.

        IF LC_DS_URL_DANFE_OUT CS 'SAP'.

          ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = I_DOCNUM
            )->SET_REGISTRO( EXPORTING I_DOCNUM = I_DOCNUM
            ")->GET_URLS_DOCS( IMPORTING E_LINK_XML  = DATA(E_LINK_XML)
            )->GET_XML_GRC( IMPORTING E_XML_STRING = E_TEXTO E_RETORNO = E_RETORNO
            ).

          IF E_RETORNO IS INITIAL.
            MESSAGE S031 WITH I_DOCNUM.
          ELSE.
            MESSAGE S032 WITH I_DOCNUM.
          ENDIF.

          "CREATE OBJECT ARQUIVO.
          "MESSAGE E_LINK_XML TYPE 'S'.
          "ARQUIVO->GET_FILE_URI_GET( EXPORTING I_URI = E_LINK_XML I_CONTENT_TYPE = 'application/download' IMPORTING E_TEXTO = DATA(E_TEXTO) ).
          "CLEAR: ARQUIVO.

        ELSE.

          CREATE OBJECT ARQUIVO.
          CONCATENATE LC_DS_URL_DANFE_OUT ',&xml=true' INTO DATA(LC_URI).
          ARQUIVO->GET_FILE_URI_GET( EXPORTING I_URI = LC_URI I_CONTENT_TYPE = 'application/download' IMPORTING E_TEXTO = E_TEXTO ).
          CLEAR: ARQUIVO.

        ENDIF.

      ENDIF.

      APPEND E_TEXTO TO R_ARQUIVO.

    ENDIF.

  ENDMETHOD.


  METHOD GET_INFO_SEGURADORA.

    DATA: RINICIO TYPE RANGE OF ZDE_DT_INI_VIGEN,
          RFINAL  TYPE RANGE OF ZDE_DT_FIM_VIGEN.

    CLEAR: E_ZLEST0115.

    SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
      FROM J_1BNFDOC
     WHERE DOCNUM EQ @I_DOCNUM.

    CHECK SY-SUBRC IS INITIAL.

    "" Documento Normal
    SELECT SINGLE * INTO @DATA(WA_J_1BNFLIN)
      FROM J_1BNFLIN
     WHERE DOCNUM EQ @I_DOCNUM.

    CHECK SY-SUBRC IS INITIAL.

    CASE WA_J_1BNFDOC-MODEL.
      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.

      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.

        "Fatura do Serviço
        SELECT SINGLE * INTO @DATA(WA_FATURA_SERVICO)
          FROM VBRP
         WHERE VBELN EQ @WA_J_1BNFLIN-REFKEY(10)
           AND POSNR EQ @WA_J_1BNFLIN-REFITM.

        CHECK SY-SUBRC IS INITIAL.

        "Ordem de Venda
        SELECT SINGLE * INTO @DATA(WA_ORDEM_VENDA)
          FROM VBAK
         WHERE VBELN EQ @WA_FATURA_SERVICO-AUBEL.

        CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO @DATA(WA_VTTP)
          FROM VTTP
        WHERE TKNUM EQ @WA_ORDEM_VENDA-TKNUM.

        CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO @DATA(WA_LIPS)
          FROM LIPS
        WHERE VBELN EQ @WA_VTTP-VBELN.

        CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO @DATA(WA_TTDS)
          FROM TTDS
         WHERE TPLST EQ @WA_LIPS-WERKS.

        CHECK SY-SUBRC IS INITIAL.

        WA_J_1BNFDOC-BUKRS = WA_TTDS-BUKRS.
        WA_J_1BNFLIN-MATKL = WA_LIPS-MATKL.

    ENDCASE.

    SELECT * INTO TABLE @DATA(IT_ZLEST0116)
      FROM ZLEST0116
     WHERE CD_EMPRESA EQ @WA_J_1BNFDOC-BUKRS
       AND CD_GRUPO   EQ @WA_J_1BNFLIN-MATKL.

    IF SY-SUBRC IS INITIAL.
"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - INICIO
*      RINICIO = VALUE #( SIGN = 'I' OPTION = 'LE' ( LOW = SY-DATUM HIGH = SY-DATUM ) ).
*      RFINAL  = VALUE #( SIGN = 'I' OPTION = 'GE' ( LOW = SY-DATUM HIGH = SY-DATUM ) ).
   if I_DOCDAT is not INITIAL.
      RINICIO = VALUE #( SIGN = 'I' OPTION = 'LE' ( LOW = I_DOCDAT HIGH = I_DOCDAT ) ).
      RFINAL  = VALUE #( SIGN = 'I' OPTION = 'GE' ( LOW = I_DOCDAT HIGH = I_DOCDAT ) ).
   else.
      RINICIO = VALUE #( SIGN = 'I' OPTION = 'LE' ( LOW = SY-DATUM HIGH = SY-DATUM ) ).
      RFINAL  = VALUE #( SIGN = 'I' OPTION = 'GE' ( LOW = SY-DATUM HIGH = SY-DATUM ) ).
   endif.

"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - FIM

      SELECT * INTO TABLE @DATA(IT_ZLEST0115)
        FROM ZLEST0115
         FOR ALL ENTRIES IN @IT_ZLEST0116
       WHERE CD_APOLICE    EQ @IT_ZLEST0116-CD_APOLICE
         AND DT_INICIO     IN @RINICIO
         AND DT_FINAL      IN @RFINAL
         AND CK_EXCLUIDO   EQ @SPACE.

      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_ZLEST0115 INDEX 1 INTO E_ZLEST0115.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  method SET_CANCEL_AVERBA.

    IF ME->AVERBACAO-CANCEL_AVERBACAO NE I_CANCEL_AVERBACAO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-CANCEL_AVERBACAO = I_CANCEL_AVERBACAO.

  endmethod.


  method SET_CD_SEGURADORA.

    IF ME->AVERBACAO-CD_SEGURADORA NE I_CD_SEGURADORA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-CD_SEGURADORA = I_CD_SEGURADORA.

  endmethod.


  METHOD SET_CD_TOKEN.

    IF ME->AVERBACAO-CD_TOKEN NE I_CD_TOKEN.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-CD_TOKEN = I_CD_TOKEN.

  ENDMETHOD.


  METHOD SET_CK_BUSCAR_LOCAL.
    ME->CK_BUSCAR_LOCAL = I_CK_BUSCAR_LOCAL.
  ENDMETHOD.


  METHOD SET_DOCNUM.

    IF ME->AVERBACAO-DOCNUM NE I_DOCNUM.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ZCL_AVERBACAO_SEGURO=>CK_DOCNUM_SEGURO( EXPORTING I_DOCNUM = I_DOCNUM IMPORTING E_ZLEST0115 = DATA(E_ZLEST0115) ).
    ME->AVERBACAO-DOCNUM = I_DOCNUM.
    ME->SET_CD_SEGURADORA( EXPORTING I_CD_SEGURADORA = E_ZLEST0115-CD_FORNECEDOR ).

  ENDMETHOD.


  method SET_DT_AUTORIZACAO.

    IF ME->AVERBACAO-DT_AUTORIZACAO NE I_DT_AUTORIZACAO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-DT_AUTORIZACAO = I_DT_AUTORIZACAO.

  endmethod.


  METHOD SET_DT_AUTORIZACAO_CAN.

    IF ME->AVERBACAO-DT_AUTORIZACAO_CAN NE I_DT_AUTORIZACAO_CAN.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-DT_AUTORIZACAO_CAN = I_DT_AUTORIZACAO_CAN.

  ENDMETHOD.


  method SET_HR_AUTORIZACAO.

    IF ME->AVERBACAO-HR_AUTORIZACAO NE I_HR_AUTORIZACAO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-HR_AUTORIZACAO = I_HR_AUTORIZACAO.

  endmethod.


  METHOD SET_HR_AUTORIZACAO_CAN.

    IF ME->AVERBACAO-HR_AUTORIZACAO_CAN NE I_HR_AUTORIZACAO_CAN.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-HR_AUTORIZACAO_CAN = I_HR_AUTORIZACAO_CAN.

  ENDMETHOD.


  METHOD SET_NR_AVERBACAO.

    IF ME->AVERBACAO-NR_AVERBACAO NE I_NR_AVERBACAO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-NR_AVERBACAO = I_NR_AVERBACAO.

  ENDMETHOD.


  method SET_NR_PROTOCOLO.

    IF ME->AVERBACAO-NR_PROTOCOLO NE I_NR_PROTOCOLO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-NR_PROTOCOLO = I_NR_PROTOCOLO.

  endmethod.


  METHOD SET_NR_PROTOCOLO_CAN.

    IF ME->AVERBACAO-NR_PROTOCOLO_CAN NE I_NR_PROTOCOLO_CAN.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-NR_PROTOCOLO_CAN = I_NR_PROTOCOLO_CAN.
    IF I_NR_PROTOCOLO_CAN IS NOT INITIAL.
      ME->AVERBACAO-CANCEL_AVERBACAO = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  method SET_NR_SEQUENCIA.

    IF ME->AVERBACAO-NR_SEQUENCIA NE I_NR_SEQUENCIA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-NR_SEQUENCIA = I_NR_SEQUENCIA.

  endmethod.


  METHOD SET_NR_SEQUENCIA_CAN.

    IF ME->AVERBACAO-NR_SEQUENCIA_CAN NE I_NR_SEQUENCIA_CAN.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-NR_SEQUENCIA_CAN = I_NR_SEQUENCIA_CAN.

  ENDMETHOD.


  METHOD SET_TP_DOCUMENTO.

    IF ME->AVERBACAO-TP_DOCUMENTO NE I_TP_DOCUMENTO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AVERBACAO-TP_DOCUMENTO = I_TP_DOCUMENTO.

  ENDMETHOD.


  method SET_XML_CANCELAMENTO.

    CLEAR: ME->TEXTO_DOCUMENTO.

    ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = ME->AVERBACAO-DOCNUM
      )->SET_REGISTRO( EXPORTING I_DOCNUM       = ME->AVERBACAO-DOCNUM
                                 I_SEM_BLOQUEIO = ABAP_TRUE
      )->GET_XML_EVENTO_GRC( EXPORTING
                                I_TPEVENTO = '110111'
                             IMPORTING
                                E_XML_STRING = DATA(E_TEXTO)
                                E_RETORNO    = DATA(E_SUBRC)
      ).

    IF E_SUBRC IS INITIAL.
      ME->TEXTO_DOCUMENTO = E_TEXTO.
      MESSAGE S031 WITH ME->AVERBACAO-DOCNUM.
    ELSE.
      MESSAGE S032 WITH ME->AVERBACAO-DOCNUM.
    ENDIF.


  endmethod.


  METHOD SET_XML_CTE.

    TRY .
        MESSAGE S030 WITH ME->AVERBACAO-DOCNUM.
        DATA(R_ARQUIVO) = ZCL_AVERBACAO_SEGURO=>GET_FILE_XML_CTE( I_DOCNUM  = ME->AVERBACAO-DOCNUM  I_LOCAL = I_LOCAL I_CANCELAR = I_CANCELAR ).
      CATCH ZCX_ARQUIVO.
        WAIT UP TO 3 SECONDS.
        MESSAGE S030 WITH ME->AVERBACAO-DOCNUM.
        R_ARQUIVO = ZCL_AVERBACAO_SEGURO=>GET_FILE_XML_CTE( I_DOCNUM  = ME->AVERBACAO-DOCNUM  I_LOCAL = I_LOCAL I_CANCELAR = I_CANCELAR ).
    ENDTRY.

    CLEAR: ME->TEXTO_DOCUMENTO.

    LOOP AT R_ARQUIVO INTO DATA(WA_ARQUIVO).
      CONCATENATE ME->TEXTO_DOCUMENTO WA_ARQUIVO INTO ME->TEXTO_DOCUMENTO.
    ENDLOOP.

  ENDMETHOD.


  METHOD SET_XML_NFE.

    DATA: ARQUIVO TYPE REF TO ZCL_ARQUIVO.

    SELECT SINGLE * INTO @DATA(WA_J)
      FROM J_1BNFE_ACTIVE
     WHERE DOCNUM EQ @ME->AVERBACAO-DOCNUM.

    SELECT SINGLE * INTO @DATA(WA_ZIB_NFE)
      FROM ZIB_NFE
     WHERE DOCNUM EQ @ME->AVERBACAO-DOCNUM.

    CHECK WA_ZIB_NFE-DS_URL_DANFE IS NOT INITIAL.

    MESSAGE S030 WITH ME->AVERBACAO-DOCNUM.

    IF ( WA_ZIB_NFE-DS_URL_DANFE CS 'SIMETRYA' ) OR ( WA_ZIB_NFE-DS_URL_DANFE CS '172.12.12.139'  ). "Nego até a morte que fui eu

      DATA(LC_CHAVE) = WA_J-REGIO && WA_J-NFYEAR && WA_J-NFMONTH && WA_J-STCD1 && WA_J-MODEL && WA_J-SERIE && WA_J-NFNUM9 && WA_J-DOCNUM9 && WA_J-CDV.

      ZCL_SQL_API_PYTHON=>ZIF_SQL_API_PYTHON~GET_INSTANCE(
        )->GET_INSTANCE(
        )->GETXMLNFE( EXPORTING I_CHAVE = LC_CHAVE IMPORTING E_XML = ME->TEXTO_DOCUMENTO
        ).

    ELSE.

      ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = ME->AVERBACAO-DOCNUM
        )->SET_REGISTRO( EXPORTING I_DOCNUM       = ME->AVERBACAO-DOCNUM
                                   I_SEM_BLOQUEIO = ABAP_TRUE
        )->GET_URLS_DOCS( IMPORTING E_LINK_XML  = DATA(E_LINK_XML) ).

      CREATE OBJECT ARQUIVO.
      MESSAGE E_LINK_XML TYPE 'S'.
      ARQUIVO->GET_FILE_URI_GET( EXPORTING I_URI = E_LINK_XML I_CONTENT_TYPE = 'application/download' IMPORTING E_TEXTO = ME->TEXTO_DOCUMENTO ).
      CLEAR: ARQUIVO.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~EXCLUIR_REGISTRO.

    I_EXCLUIU = ABAP_FALSE.

    IF ME->VALIDAR_EXCLUSAO( ) EQ ABAP_TRUE.

      DELETE FROM ZLEST0143 WHERE DOCNUM EQ ME->AVERBACAO-DOCNUM.
      COMMIT WORK.
      I_EXCLUIU = ABAP_TRUE.
      MESSAGE S002.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~GET_REGISTRO.

    MOVE-CORRESPONDING ME->AVERBACAO TO E_REGISTRO.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~GRAVAR_REGISTRO.

    DATA: LC_ITEM TYPE ZSDT0001_ITEM-CD_ITEM.

    I_GRAVOU = ABAP_FALSE.

    IF ME->CK_ALTEROU EQ ABAP_TRUE.

      IF ME->VALIDAR_REGISTRO( ) EQ ABAP_TRUE.

        IF ME->AVERBACAO-CD_AVERBACAO IS INITIAL.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              NR_RANGE_NR             = '01'
              OBJECT                  = 'ZLESAVSEG'
            IMPORTING
              NUMBER                  = ME->AVERBACAO-CD_AVERBACAO
            EXCEPTIONS
              INTERVAL_NOT_FOUND      = 1
              NUMBER_RANGE_NOT_INTERN = 2
              OBJECT_NOT_FOUND        = 3
              QUANTITY_IS_0           = 4
              QUANTITY_IS_NOT_1       = 5
              INTERVAL_OVERFLOW       = 6
              BUFFER_OVERFLOW         = 7
              OTHERS                  = 8.

          IF SY-SUBRC IS NOT INITIAL.
            RAISE EXCEPTION TYPE ZCX_CADASTRO
              EXPORTING
                TEXTID = VALUE #( MSGID = SY-MSGID
                                  MSGNO = SY-MSGNO
                                  ATTR1 = CONV #( SY-MSGV1 )
                                  ATTR2 = CONV #( SY-MSGV2 )
                                  ATTR3 = CONV #( SY-MSGV3 )
                                  ATTR4 = CONV #( SY-MSGV4 ) )
                MSGID  = SY-MSGID
                MSGNO  = SY-MSGNO
                MSGTY  = 'E'
                MSGV1  = SY-MSGV1
                MSGV2  = SY-MSGV2
                MSGV3  = SY-MSGV3
                MSGV4  = SY-MSGV4.
          ENDIF.

        ENDIF.
        ME->AVERBACAO-DT_CADASTRO = SY-DATUM.
        ME->AVERBACAO-HR_CADASTRO = SY-UZEIT.
        ME->AVERBACAO-US_CADASTRO = SY-UNAME.
        MODIFY ZLEST0143 FROM ME->AVERBACAO.
        COMMIT WORK.
        ME->CK_ALTEROU = ABAP_FALSE.
        I_GRAVOU = ABAP_TRUE.
        MESSAGE S001.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~LIMPAR_REGISTRO.

    CLEAR: ME->AVERBACAO.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~NOVO_REGISTRO.
    ME->LIMPAR_REGISTRO( ).
  ENDMETHOD.


  METHOD ZIF_CADASTRO~SET_REGISTRO.

    ME->LIMPAR_REGISTRO( ).

    SELECT SINGLE * INTO ME->AVERBACAO FROM ZLEST0143 WHERE CD_AVERBACAO EQ I_ID_REGISTRO.

    ME->CK_ALTEROU = ABAP_FALSE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_EXCLUSAO.

    E_VALIDOU = ABAP_FALSE.

    MESSAGE S003 DISPLAY LIKE 'E'.
    EXIT.

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_REGISTRO.

    E_VALIDOU = ABAP_FALSE.

    IF ME->AVERBACAO-DOCNUM IS INITIAL.
      MESSAGE S004 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0143)
      FROM ZLEST0143
     WHERE DOCNUM EQ @ME->AVERBACAO-DOCNUM.

    IF SY-SUBRC IS INITIAL AND WA_ZLEST0143-CD_AVERBACAO NE ME->AVERBACAO-CD_AVERBACAO.
      MESSAGE S005 WITH WA_ZLEST0143-CD_AVERBACAO DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    TRY.
        ZCL_AVERBACAO_SEGURO=>CK_DOCNUM_SEGURO( I_DOCNUM = ME->AVERBACAO-DOCNUM I_CANCELAR = ME->CANCELANDO ).
      CATCH ZCX_AVERBACAO_SEGURO INTO DATA(CX_AVERBACAO_SEGURO).
        CX_AVERBACAO_SEGURO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
        EXIT.
    ENDTRY.

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL.

    R_PERMITIDO = ABAP_FALSE.

    IF I_CAMPO EQ 'DOCNUM' .
      R_PERMITIDO = ABAP_TRUE.
    ENDIF.

    CHECK ME->AVERBACAO-CD_AVERBACAO IS NOT INITIAL.

    IF I_CAMPO EQ 'DOCNUM' .
      R_PERMITIDO = ABAP_FALSE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_pesquisa~pesquisar.



    DATA: lc_filtro     TYPE zde_zlest0143_filtro,
          wa_retorno    TYPE zde_zlest0143_alv,
          wa_retorno_s  TYPE zde_zlest0143_alv_sint,
          tp_rel        TYPE char01,
          lc_retorno_s  TYPE zde_zlest0143_alv_sint_t,
          lc_retorno    TYPE zde_zlest0143_alv_t,
          vg_quantidade TYPE p DECIMALS 0,
          qtd_viag      TYPE p.

    DATA: t_j_1bnflin TYPE TABLE OF j_1bnflin.
"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - INICIO
    data: E_ZLEST0115 Type  ZLEST0115,
          e_docnum TYPE J_1BNFE_T_DOCNUM.
     data: rg_vbeln TYPE RANGE OF vbeln,
          wa_vbeln LIKE LINE  OF rg_vbeln.
"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - FIM
    MOVE i_filtros TO lc_filtro.

    FREE: t_j_1bnflin.

    CLEAR: e_registros, e_pesquisou.

    CLEAR: tp_rel.
    READ TABLE lc_filtro-tp_relatorio INTO DATA(ws_fil_tp_rel) INDEX 1.
    IF sy-subrc EQ 0.
      tp_rel = ws_fil_tp_rel-low.
    ENDIF.

    SELECT av~cd_averbacao,
           av~docnum,
           av~cd_seguradora,
           av~cd_token,
           av~tp_documento,
           av~dt_cadastro,
           av~hr_cadastro,
           av~us_cadastro,
           av~nr_averbacao,
           av~nr_protocolo,
           av~nr_sequencia,
           av~dt_autorizacao,
           av~hr_autorizacao,
           av~cancel_averbacao,
           av~nr_protocolo_can,
           av~nr_sequencia_can,
           av~dt_autorizacao_can,
           av~hr_autorizacao_can,
           ct~tknum,
           ct~ufini,
           ct~nmunini,
           ct~uffim,
           ct~nmunfim,
           sg~xseg,
           sg~napol,
           dc~cancel,
           dc~branch,
           dc~bukrs,
           dc~cnpj_bupla,
           co~quantidade,
           co~unidade,
           co~vlr_pedagio,
           co~vlr_impostos,
           co~vlr_frete,
           co~vlr_adiantamento,
           co~vlr_triagem,
           co~vlr_saldo,
           co~vlr_seguro
      INTO TABLE @DATA(it_tab)
      FROM zlest0143 AS av
      LEFT JOIN zcte_identifica AS ct ON ct~docnum = av~docnum
      LEFT JOIN zcte_seguro     AS sg ON sg~docnum = av~docnum
      INNER JOIN j_1bnfdoc      AS dc ON dc~docnum = av~docnum
      LEFT JOIN zcte_ciot       AS co ON co~docnum = av~docnum
    WHERE av~cd_seguradora IN @lc_filtro-cd_seguradora
      AND av~dt_cadastro   IN @lc_filtro-dt_cadastro
      AND av~us_cadastro   IN @lc_filtro-us_cadastro
      AND av~docnum        IN @lc_filtro-docnum.
*      AND dc~bukrs         IN @lc_filtro-bukrs
*      AND dc~branch        IN @lc_filtro-branch.

    CHECK it_tab[] IS NOT INITIAL.


    IF lc_filtro-bukrs IS NOT INITIAL.
      DELETE it_tab WHERE bukrs NOT IN lc_filtro-bukrs.
    ENDIF.

    CHECK it_tab[] IS NOT INITIAL.

    IF lc_filtro-branch IS NOT INITIAL.
      DELETE it_tab WHERE branch NOT IN lc_filtro-branch.
    ENDIF.

    CHECK it_tab[] IS NOT INITIAL.
"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - INICIO
 "1.1 - busca do doc de transporte (TKNUM) associada a NF-e

SELECT * INTO TABLE @DATA(it_J_1BNFLIN)
      FROM J_1BNFLIN
       FOR ALL ENTRIES IN @it_tab
     WHERE docnum EQ @it_tab-docnum.

  if sy-SUBRC is INITIAL.

  loop at it_J_1BNFLIN into data(wa_).
     WA_VBELN-sign   = 'I'.
      wa_VBELN-option = 'EQ'.
      wa_VBELN-low    = wa_-REFKEY.
      APPEND wa_VBELN TO rg_VBELN.
    ENDLOOP.
SELECT * INTO TABLE @DATA(it_VBFA)
      FROM VBFA
     WHERE VBELN in @rg_VBELN  and
           VBTYP_N = 'M'
           and VBTYP_V = 'J'.
   if sy-SUBRC is INITIAL.
SELECT * INTO TABLE @DATA(it_VTTP)
      FROM VTTP
       FOR ALL ENTRIES IN @it_VBFA
     WHERE VBELN = @it_VBFA-VBELV  .

if sy-SUBRC is INITIAL.

SELECT * INTO TABLE @DATA(it_VTTk)
      FROM VTTK
       FOR ALL ENTRIES IN @it_VTTP
     WHERE TKNUM = @it_VTTP-TKNUM
  and  VSART = '01' .
  if sy-SUBRC is INITIAL.

*    1.2 - busca parceiros PC ( Ponto Coleta) e LR ( local entrega)
SELECT * INTO TABLE @DATA(it_VTPA)
      FROM VTPA
       FOR ALL ENTRIES IN @it_VTTK
     WHERE vbeln = @it_VTTK-TKNUM and
   PARVW in ('PC' , 'LR') .
if sy-SUBRC is INITIAL.

SELECT * INTO TABLE @DATA(it_LFA1)
      FROM LFA1
       FOR ALL ENTRIES IN @it_VTPA
     WHERE LIFNR = @it_VTPA-LIFNR
  and  LAND1 = 'BR' .

  SELECT * INTO TABLE @DATA(it_KNA1)
      FROM KNA1
       FOR ALL ENTRIES IN @it_VTPA
     WHERE KUNNR = @it_VTPA-KUNNR
  and  LAND1 = 'BR' .

endif.

SELECT * INTO TABLE @DATA(it_VFKP)
      FROM VFKP
       FOR ALL ENTRIES IN @it_VTTK
     WHERE REBEL = @it_VTTK-TKNUM and
   FKPTY = 'Z001' and
   REFTY = '8'.
  if sy-SUBRC is INITIAL.

    SELECT * INTO TABLE @DATA(it_PRCD_ELEMENTS)
      FROM PRCD_ELEMENTS
       FOR ALL ENTRIES IN @it_VFKP
     WHERE KNUMV = @it_VFKP-KNUMV .

endif.
endif.
  endif.
    endif.
    ENDIF.
"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - fim

    SELECT * INTO TABLE @DATA(it_mercadoria)
      FROM zcte_info_nota
       FOR ALL ENTRIES IN @it_tab
     WHERE docnum EQ @it_tab-docnum.

    IF it_mercadoria[] IS NOT INITIAL.

      SELECT * INTO TABLE @DATA(it_mara)
        FROM mara
         FOR ALL ENTRIES IN @it_mercadoria
       WHERE matnr EQ @it_mercadoria-material.

      IF it_mara[] IS NOT INITIAL.

        SELECT * INTO TABLE @DATA(it_t023t)
          FROM t023t
           FOR ALL ENTRIES IN @it_mara
         WHERE spras EQ @sy-langu
           AND matkl EQ @it_mara-matkl.

      ENDIF.

    ENDIF.

    SORT it_mara  BY matnr.
    SORT it_t023t BY matkl.

    DATA: lc_matnr TYPE matnr.

    SELECT * FROM j_1bnflin INTO TABLE t_j_1bnflin FOR ALL ENTRIES IN it_tab WHERE docnum EQ it_tab-docnum.
    IF sy-subrc EQ 0.
      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @it_t023t
      FROM t023t
      FOR ALL ENTRIES IN @t_j_1bnflin
      WHERE spras EQ @sy-langu
      AND matkl EQ @t_j_1bnflin-matkl.

      IF sy-subrc EQ 0.
        SORT it_t023t BY matkl.
        DELETE ADJACENT DUPLICATES FROM it_t023t COMPARING matkl.
      ENDIF.
    ENDIF.
"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - INICIO
*loop AT it_tab INTO DATA(wa_tab1).
*  append wa_tab1-DOCNUM TO E_DOCNUM.
*  ENDLOOP.

   " E_ZLEST0115 = me->GET_INFO_SEGURADORA_nfe( IT_DOCNUM = E_DOCNUM ).
"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - FIM

    LOOP AT it_tab INTO DATA(wa_tab).

      CLEAR: wa_retorno.

      MOVE-CORRESPONDING wa_tab TO wa_retorno.


      wa_retorno-vlr_mercadoria = 0.
      CLEAR: lc_matnr.

      LOOP AT it_mercadoria INTO DATA(wa_mercadoria) WHERE docnum EQ wa_tab-docnum.
        ADD wa_mercadoria-vl_nota_fiscal TO wa_retorno-vlr_mercadoria.

        IF lc_matnr IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        lc_matnr = wa_mercadoria-material.
        READ TABLE it_mara WITH KEY matnr = lc_matnr INTO DATA(wa_mara) BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE it_t023t WITH KEY matkl = wa_mara-matkl INTO DATA(wa_t023t) BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            wa_retorno-matkl = wa_t023t-matkl.
            wa_retorno-wgbez = wa_t023t-wgbez.
          ENDIF.
        ENDIF.

      ENDLOOP.

      IF wa_retorno-quantidade IS INITIAL.
        LOOP AT t_j_1bnflin INTO DATA(ws_flin) WHERE docnum EQ wa_tab-docnum.
          ADD ws_flin-netwr TO wa_retorno-vlr_mercadoria.
          ADD ws_flin-menge TO wa_retorno-quantidade.

          IF lc_matnr IS NOT INITIAL.
            CONTINUE.
          ENDIF.

          lc_matnr = ws_flin-matnr.
          READ TABLE it_mara WITH KEY matnr = lc_matnr INTO wa_mara BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            wa_retorno-unidade = ws_flin-meins.
            READ TABLE it_t023t WITH KEY matkl = wa_mara-matkl INTO wa_t023t BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wa_retorno-matkl = wa_t023t-matkl.
              wa_retorno-wgbez = wa_t023t-wgbez.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDIF.

      "Verificar grupo de mercadoria referente NFe.
      IF wa_tab-tp_documento EQ 'NF-E'.
        READ TABLE t_j_1bnflin INTO DATA(ws_lin) WITH KEY docnum =  wa_tab-docnum.
        IF sy-subrc EQ 0.
          wa_retorno-matkl = ws_lin-matkl.
          READ TABLE it_t023t WITH KEY matkl = wa_retorno-matkl INTO wa_t023t BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            wa_retorno-wgbez = wa_t023t-wgbez.
          ENDIF.
        ENDIF.
"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - INICIO
        "1.1 - busca do doc de transporte (TKNUM) associada a NF-e
        READ TABLE IT_J_1BNFLIN into data(w_J_1BNFLIN) WITH KEY docnum =  wa_tab-docnum.
        if sy-SUBRC is INITIAL.
          READ TABLE it_VBFA into data(w_VBFA) WITH KEY VBELN = w_J_1BNFLIN-REFKEY.
          if sy-SUBRC is INITIAL.
             READ TABLE it_VTTP into data(w_VTTP) WITH KEY VBELN = w_VBFA-VBELV.
             if sy-SUBRC is INITIAL.
                READ TABLE it_VTTK into data(w_VTTK) WITH KEY tknum = w_VTTP-tknum.
                if sy-SUBRC is INITIAL.
                      wa_retorno-TKNUM = w_VTTP-tknum.
                      "1.2 - busca parceiros PC ( Ponto Coleta) e LR ( local entrega)
                      loop at it_VTPA into data(w_VTPA) where vbeln = w_VTTK-tknum.
                          case w_VTPA-PARVW.
                          WHEN 'PC'.
                              READ TABLE it_LFA1 into data(w_LFA1) WITH KEY lifnr = w_vtpa-LIFNR.
                              if sy-SUBRC is INITIAL.
                                wa_retorno-ufini  = w_LFA1-REGIO.
                                wa_retorno-nmunini = w_LFA1-ORT01.
                              endif.
                         when  'LR'.
                              READ TABLE it_KNA1 into data(w_KNA1) WITH KEY KUNNR = w_vtpa-KUNNR.
                              if sy-SUBRC is INITIAL.
                                wa_retorno-UFFIM  = w_KNA1-REGIO.
                                wa_retorno-NMUNFIM = w_KNA1-ORT01.
                              endif.
                        ENDCASE.
                      endloop.
                      "1.3 - busca valores da VI
                      READ TABLE it_VFKP into data(w_VFKP) WITH KEY rebel = w_VTTK-TKNUM.
                       if sy-SUBRC is INITIAL.

*Quando PRCD_ELEMENTS-KSCHL = 'ZFRE'  pegar o valor de PRCD_ELEMENTS-KWERT  e guardar em it_tab_vlr_frete
*Quando PRCD_ELEMENTS-KSCHL = 'ZIOF'  pegar o valor de PRCD_ELEMENTS-KWERT  e guardar em it_tab_vlr_impostos
*Quando PRCD_ELEMENTS-KSCHL = 'ZSEG'  pegar o valor de PRCD_ELEMENTS-KWERT  e guardar em it_tab_vlr_seguro
*Quando PRCD_ELEMENTS-KSCHL = 'ZPED'  pegar o valor de PRCD_ELEMENTS-KWERT  e guardar em it_tab_vlr_pedagio
*Quando PRCD_ELEMENTS-KSCHL = 'ZADM'  pegar o valor de PRCD_ELEMENTS-KWERT  e guardar em it_tab_vlr_adiantamento
*Quando PRCD_ELEMENTS-KSCHL = 'ZVCT'  pegar o valor de PRCD_ELEMENTS-KWERT  e guardar em it_tab_vlr_triagem

                         loop at it_PRCD_ELEMENTS into data(w_PRCD_ELEMENTS) WHERE KNUMV  = w_VFKP-KNUMV.
                            case w_PRCD_ELEMENTS-KSCHL.
                              WHEN 'ZFRE'.
                                wa_retorno-vlr_frete = w_PRCD_ELEMENTS-KWERT.
                              WHEN 'ZIOF'.
                                wa_retorno-vlr_impostos = w_PRCD_ELEMENTS-KWERT.
                              WHEN 'ZSEG'.
                                wa_retorno-vlr_seguro = w_PRCD_ELEMENTS-KWERT.
                              WHEN  'ZPED'.
                                wa_retorno-vlr_pedagio = w_PRCD_ELEMENTS-KWERT.
                              WHEN 'ZADM'.
                                wa_retorno-vlr_adiantamento = w_PRCD_ELEMENTS-KWERT.
                              WHEN 'ZVCT'.
                                wa_retorno-vlr_triagem = w_PRCD_ELEMENTS-KWERT.
                            ENDCASE.
                        ENDLOOP.
                        wa_retorno-vlr_saldo = ( wa_retorno-vlr_frete - wa_retorno-vlr_adiantamento ).

                       endif.
                endif.
             endif.
          endif.
        endif.

         E_ZLEST0115 = ZCL_AVERBACAO_SEGURO=>GET_INFO_SEGURADORA( I_DOCNUM = wa_tab-docnum
                                                                  I_DOCDAT = wa_tab-DT_AUTORIZACAO ).
     if E_ZLEST0115 is not INITIAL.

       SELECT single name1 from lfa1 into @data(V_nameSeguradora) where LIFNR eq @E_ZLEST0115-CD_FORNECEDOR.

       wa_retorno-XSEG = V_nameSeguradora.

       wa_retorno-NAPOL = E_ZLEST0115-NR_APOLICE.
       ENDIF.

      endif.
"131285 IR166516 - ZLES0148 - Dados para averbação sobre NF-e - BG - FIM
      APPEND wa_retorno TO lc_retorno.
      CLEAR: wa_t023t, ws_lin.
    ENDLOOP.

    e_registros = lc_retorno.
    e_pesquisou = abap_true.


    "=====================================================================================================================
    "Sintético.
    "=====================================================================================================================

    "Analitíco.
    IF tp_rel EQ 'S'.
      CLEAR: wa_t023t.
      FREE: e_registros.
      SORT lc_retorno BY bukrs branch matkl unidade.
      DATA(lc_retorno_aux) = lc_retorno.
      DELETE ADJACENT DUPLICATES FROM lc_retorno_aux COMPARING bukrs branch matkl unidade.

      LOOP AT lc_retorno_aux INTO DATA(ws_ret).
        MOVE-CORRESPONDING ws_ret TO  wa_retorno_s.
        LOOP AT lc_retorno INTO wa_retorno WHERE bukrs    EQ ws_ret-bukrs
                                            AND  branch   EQ ws_ret-branch
                                            AND  matkl    EQ ws_ret-matkl
                                            AND  unidade  EQ ws_ret-unidade.


          ADD 1 TO wa_retorno_s-ztot_viag.

          CLEAR: vg_quantidade.
*          IF wa_retorno-unidade EQ 'TO'.
*            wa_retorno-unidade = 'KG'.
*            vg_quantidade = ( wa_retorno-quantidade * 1000 ). "Converter em kg
*          ELSE.
          IF wa_retorno-quantidade IS NOT INITIAL.
            vg_quantidade = wa_retorno-quantidade.
            ADD vg_quantidade TO wa_retorno_s-ztot_v_transp.
          ENDIF.
*          ENDIF.

          IF wa_retorno-vlr_mercadoria IS NOT INITIAL.
            ADD wa_retorno-vlr_mercadoria TO wa_retorno_s-zvlor_total.
          ENDIF.


          READ TABLE it_t023t WITH KEY matkl = ws_ret-matkl INTO wa_t023t BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            wa_retorno_s-wgbez = wa_t023t-wgbez.
          ENDIF.
        ENDLOOP.

        CONDENSE wa_retorno_s-ztot_viag.

        APPEND wa_retorno_s TO lc_retorno_s.
        CLEAR: qtd_viag,  wa_retorno_s, wa_t023t, vg_quantidade.
      ENDLOOP.

      e_registros_s = lc_retorno_s.
      e_pesquisou = abap_true.
    ENDIF.




  ENDMETHOD.


  method GET_INFO_SEGURADORA_NFE.

    DATA: RINICIO TYPE RANGE OF ZDE_DT_INI_VIGEN,
          RFINAL  TYPE RANGE OF ZDE_DT_FIM_VIGEN.

    CLEAR: E_ZLEST0115.

    SELECT  * INTO table @DATA(it_J_1BNFDOC)
      FROM J_1BNFDOC
      FOR ALL ENTRIES IN @IT_DOCNUM
     WHERE DOCNUM EQ @IT_DOCNUM-DOCNUM.

    CHECK SY-SUBRC IS INITIAL.

    "" Documento Normal
    SELECT  * INTO table @DATA(IT_J_1BNFLIN)
      FROM J_1BNFLIN
     FOR ALL ENTRIES IN @IT_DOCNUM
     WHERE DOCNUM EQ @IT_DOCNUM-DOCNUM.

    CHECK SY-SUBRC IS INITIAL.

loop at it_J_1BNFDOC INTO data(WA_J_1BNFDOC).
READ TABLE IT_J_1BNFLIN  into data(WA_J_1BNFLIN) WITH KEY DOCNUM = WA_J_1BNFDOC-DOCNUM.
    if sy-SUBRC is INITIAL.
    CASE WA_J_1BNFDOC-MODEL.
      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.

      WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.


        "Fatura do Serviço
        SELECT SINGLE * INTO @DATA(WA_FATURA_SERVICO)
          FROM VBRP
         WHERE VBELN EQ @WA_J_1BNFLIN-REFKEY(10)
           AND POSNR EQ @WA_J_1BNFLIN-REFITM.

        CHECK SY-SUBRC IS INITIAL.

        "Ordem de Venda
        SELECT SINGLE * INTO @DATA(WA_ORDEM_VENDA)
          FROM VBAK
         WHERE VBELN EQ @WA_FATURA_SERVICO-AUBEL.

        CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO @DATA(WA_VTTP)
          FROM VTTP
        WHERE TKNUM EQ @WA_ORDEM_VENDA-TKNUM.

        CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO @DATA(WA_LIPS)
          FROM LIPS
        WHERE VBELN EQ @WA_VTTP-VBELN.

        CHECK SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO @DATA(WA_TTDS)
          FROM TTDS
         WHERE TPLST EQ @WA_LIPS-WERKS.

        CHECK SY-SUBRC IS INITIAL.

        WA_J_1BNFDOC-BUKRS = WA_TTDS-BUKRS.
        WA_J_1BNFLIN-MATKL = WA_LIPS-MATKL.

    ENDCASE.

    SELECT * INTO TABLE @DATA(IT_ZLEST0116)
      FROM ZLEST0116
     WHERE CD_EMPRESA EQ @WA_J_1BNFDOC-BUKRS
       AND CD_GRUPO   EQ @WA_J_1BNFLIN-MATKL.

    IF SY-SUBRC IS INITIAL.

   if I_DOCDAT is not INITIAL.
      RINICIO = VALUE #( SIGN = 'I' OPTION = 'LE' ( LOW = I_DOCDAT HIGH = I_DOCDAT ) ).
      RFINAL  = VALUE #( SIGN = 'I' OPTION = 'GE' ( LOW = I_DOCDAT HIGH = I_DOCDAT ) ).
   else.
      RINICIO = VALUE #( SIGN = 'I' OPTION = 'LE' ( LOW = SY-DATUM HIGH = SY-DATUM ) ).
      RFINAL  = VALUE #( SIGN = 'I' OPTION = 'GE' ( LOW = SY-DATUM HIGH = SY-DATUM ) ).
   endif.

      SELECT * INTO TABLE @DATA(IT_ZLEST0115)
        FROM ZLEST0115
         FOR ALL ENTRIES IN @IT_ZLEST0116
       WHERE CD_APOLICE    EQ @IT_ZLEST0116-CD_APOLICE
         AND DT_INICIO     IN @RINICIO
         AND DT_FINAL      IN @RFINAL
         AND CK_EXCLUIDO   EQ @SPACE.

      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_ZLEST0115 INDEX 1 INTO E_ZLEST0115.
      ENDIF.

    ENDIF.
    endif.
ENDLOOP.
  endmethod.


  method ZIF_CADASTRO~CHECK_ENV_APROV_TAXA.

  endmethod.
ENDCLASS.
