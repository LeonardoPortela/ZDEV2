class ZCL_INFO_NF_SE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INFO_NF_SE .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INFO_NF_SE IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_sap_se_info_nf.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_inject~at_referencia-tp_referencia = 'SAP SE INFO NF'.

  ENDMETHOD.


  method ZIF_INFO_NF_SE~GET_INSTANCE.
    IF zif_info_nf_se~at_if_info_nf_se IS NOT BOUND.
      CREATE OBJECT zif_info_nf_se~at_if_info_nf_se TYPE zcl_info_nf_se.
    ENDIF.

    R_INFO_NF_SE = zif_info_nf_se~at_if_info_nf_se.
  endmethod.


  method ZIF_INFO_NF_SE~SET_DS_DATA.


     DATA: i_inbound       TYPE ZDE_INFO_NFE_SE,
          t_element_array	TYPE zde_element_array_t.

    "Incluir Texto JSON para integração
    r_if_info_nf_se = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

*    APPEND 'belnr' TO t_element_array.
*    APPEND 'gjahr' TO t_element_array.
*    APPEND 'bukrs' TO t_element_array.
*    APPEND 'stgrd' TO t_element_array.
*    APPEND 'budat' TO t_element_array.

IF i_info-ds_body IS NOT INITIAL.
        /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = i_inbound ).
ENDIF.

*    me->zif_integracao_inject~at_referencia-id_referencia = i_inbound-belnr && '-' &&
*                                                            i_inbound-gjahr && '-' &&
*                                                            i_inbound-bukrs && '-' &&
*                                                            i_inbound-stgrd && '-' &&
*                                                            i_inbound-budat && '-'.


  endmethod.


  method ZIF_INFO_NF_SE~SET_SEND_MSG.


    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_if_info_nf_se = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = 'info_nf_se'.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->ZIF_INFO_NF_SE~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno(
           IMPORTING
             e_data_retorno = DATA(e_data_retorno)
             e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    e_msg = e_data_retorno.
    e_protocolo = zcl_string=>lpad( i_str  = CONV #( e_integracao-id_integracao ) i_qtd  = 15 i_char = '0'  ).

    CLEAR: lc_integracao.

  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    DATA: i_inbound       TYPE ZDE_INFO_NFE_SE,
          t_element_array	TYPE zde_element_array_t,
          l_objectid      TYPE cdhdr-objectid,
          l_tcode         TYPE cdhdr-tcode,
          l_utime         TYPE cdhdr-utime,
          l_udate         TYPE cdhdr-udate,
          l_username      TYPE cdhdr-username,
          e_retorno       TYPE string,
          e_erro          TYPE string .





    e_sucesso              = abap_false.
    e_nm_code              = abap_false.
    r_if_integracao_inject = me.

    APPEND 'CHAVE_NFE'      TO t_element_array.
    APPEND 'TIPO_NOTA'       TO t_element_array.
    APPEND 'DIRECAO'       TO t_element_array.



IF i_msg_inbound IS NOT INITIAL.
        /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = i_inbound ).
ENDIF.
*----------------------------
*-- formatacao campos
*----------------------------
    CONDENSE i_inbound-chave_nfe  NO-GAPS.


    CALL FUNCTION 'ZSD_INFO_NF_SE'
      EXPORTING
        chave_nfe       = i_inbound-chave_nfe
        tipo_nota       = i_inbound-tipo_nota
        direcao         = i_inbound-direcao
     IMPORTING
       E_RETORNO       = e_retorno
       e_erro          = e_erro
              .


if e_erro is INITIAL.
    e_msg_outbound = e_retorno.
     e_sucesso      = abap_true.
    else.
      e_msg_outbound = '{ "mensagem" : "'     && e_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
'"ID_MSG" : "' && '404'    && '"' && cl_abap_char_utilities=>newline && '}'.
    ENDIF.



  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
  endmethod.
ENDCLASS.
