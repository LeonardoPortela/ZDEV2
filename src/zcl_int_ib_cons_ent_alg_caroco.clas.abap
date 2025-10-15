CLASS zcl_int_ib_cons_ent_alg_caroco DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    DATA:
      BEGIN OF zde_data_request ,
        safra         TYPE zsdt0001-nr_safra,
        filial        TYPE zsdt0001-branch,
        datamovimento TYPE zsdt0001-dt_movimento,
        romaneio      TYPE zsdt0001-nr_romaneio,
      END OF zde_data_request .

    DATA:
      BEGIN OF zde_data_response,
        romaneio TYPE zsdt0001,
      END OF zde_data_response .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '215' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_ENT_ALG_CAROCO IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  METHOD ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND.

    DATA: lwa_data_request LIKE zde_data_request.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_request ).

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    IF lwa_data_request-safra IS INITIAL.
      r_msg_erro = 'Safra não foi informada!'.
      RETURN.
    ENDIF.

    IF lwa_data_request-filial IS INITIAL.
      r_msg_erro = 'Filial não foi informada!'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    TYPES: BEGIN OF ty_zpps_ximfbf_log,
             obj_key       TYPE zpps_ximfbf_log-obj_key,
             qteprod       TYPE zpps_ximfbf_log-qteprod,
             mblnr         TYPE zpps_ximfbf_log-mblnr,
             dtmvto        TYPE zpps_ximfbf_log-dtmvto,
             matnr        TYPE zpps_ximfbf_log-matnr,
             ch_referencia TYPE zsdt0001-ch_referencia,
           END OF ty_zpps_ximfbf_log,

           BEGIN OF ty_zmmt0006,
             ch_referencia     TYPE zmmt0006-ch_referencia,
             erfmg             TYPE zmmt0006-erfmg,
             doc_material      TYPE zmmt0006-doc_material,
             budat             TYPE zmmt0006-budat,
             matnr             TYPE zmmt0006-matnr,
             ch_referencia_rom TYPE zmmt0006-ch_referencia,
           END OF ty_zmmt0006,

           BEGIN OF ty_mseg,
             mblnr TYPE mseg-mblnr,
             mjahr TYPE mseg-mjahr,
             menge TYPE mseg-menge,
           END OF ty_mseg,

           BEGIN OF ty_zsdt0001,
             ch_referencia TYPE zsdt0001-ch_referencia,
             nr_romaneio   TYPE zsdt0001-nr_romaneio,
             dt_movimento  TYPE zsdt0001-dt_movimento,
           END OF ty_zsdt0001.

    TYPES: BEGIN OF ty_romaneio,
             seq_planilha_romaneio TYPE zsdt0001-ch_referencia,
             nro_romaneio          TYPE zsdt0001-nr_romaneio,
             data_movimento        TYPE zsdt0001-dt_movimento,
             peso_liquido          TYPE zsdt0001-peso_liq,
             tipo_entrada          TYPE zsdt0001-tp_movimento,
             doc_material_sap      TYPE zsdt0001-doc_material,
           END OF ty_romaneio.

    DATA: lit_romaneios TYPE TABLE OF ty_romaneio.

    TYPES: BEGIN OF ty_response,
             romaneios LIKE lit_romaneios,
           END OF ty_response.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response TYPE ty_response.

    DATA: lit_zpps_ximfbf_log TYPE TABLE OF ty_zpps_ximfbf_log,
          lit_zsdt0001        TYPE TABLE OF ty_zsdt0001,
          lit_zmmt0006        TYPE TABLE OF ty_zmmt0006,
          lit_mseg            TYPE TABLE OF ty_mseg.


    DATA: lra_nro_romaneio   TYPE RANGE OF zsdt0001-nr_romaneio,
          lra_data_movimento TYPE RANGE OF zsdt0001-dt_movimento,
          lra_matnr          TYPE RANGE OF zsdt0001-matnr.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lwa_data_response.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).
    ENDIF.

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound =  i_msg_inbound IMPORTING e_status_code  =  DATA(_status_code)  RECEIVING r_msg_erro = e_msg_erro ).

    IF e_msg_erro IS NOT INITIAL.

      IF _status_code IS INITIAL .
        _status_code = '400'. "Bad Request
      ENDIF.

      e_sucesso      = abap_true.
      e_nm_code      = _status_code.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.

    IF lwa_data_request-datamovimento IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-datamovimento ) TO lra_data_movimento.
    ENDIF.

    IF lwa_data_request-romaneio IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-romaneio ) TO lra_nro_romaneio.
    ENDIF.

    SELECT *
     FROM tvarvc INTO TABLE @DATA(git_matkl_alg_caroco)
     WHERE name = 'MAGGI_GR_ALGODAO_CAROCO'.

    IF git_matkl_alg_caroco[] IS NOT INITIAL.
      SELECT matnr
        FROM mara INTO TABLE @DATA(lit_mara_alg_caroco)
        FOR ALL ENTRIES IN @git_matkl_alg_caroco
       WHERE matkl = @git_matkl_alg_caroco-low(9).
    ENDIF.

    LOOP AT lit_mara_alg_caroco INTO DATA(lwa_mara).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_mara-matnr ) TO lra_matnr.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input         = lwa_mara-matnr
        IMPORTING
          OUTPUT        = lwa_mara-matnr.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_mara-matnr ) TO lra_matnr.

    ENDLOOP.

*-------------------------------------------------------------------------------------*
*   Seleção Entrada Produção Propria
*-------------------------------------------------------------------------------------*
    DATA(lva_charg) = lwa_data_request-safra && '_' && lwa_data_request-filial.

    IF lra_matnr[] IS NOT INITIAL.
      SELECT obj_key qteprod mblnr dtmvto matnr
        FROM zpps_ximfbf_log AS a INTO TABLE lit_zpps_ximfbf_log
       WHERE werks            EQ lwa_data_request-filial
         AND charg            EQ lva_charg
         AND matnr            IN lra_matnr
         AND zst_atlz         EQ 'I'
         AND id_interface     EQ 'S' "Entrada Produção Sigam
         AND mblnr            NE space
         AND NOT EXISTS (  SELECT mblnr
                             FROM mseg AS b
                            WHERE b~smbln EQ a~mblnr ).
    ENDIF.

    LOOP AT lit_zpps_ximfbf_log ASSIGNING FIELD-SYMBOL(<fs_zpps_ximfbf_log>).
      <fs_zpps_ximfbf_log>-ch_referencia = <fs_zpps_ximfbf_log>-obj_key(11).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fs_zpps_ximfbf_log>-matnr
        IMPORTING
          output = <fs_zpps_ximfbf_log>-matnr.
    ENDLOOP.

    IF lit_zpps_ximfbf_log[] IS NOT INITIAL.
      SELECT ch_referencia nr_romaneio dt_movimento
        FROM zsdt0001 INTO TABLE lit_zsdt0001
         FOR ALL ENTRIES IN lit_zpps_ximfbf_log
       WHERE ch_referencia = lit_zpps_ximfbf_log-ch_referencia.

      SELECT mblnr mjahr menge
        FROM mseg INTO TABLE lit_mseg
         FOR ALL ENTRIES IN lit_zpps_ximfbf_log
       WHERE mblnr = lit_zpps_ximfbf_log-mblnr
         AND mjahr = lit_zpps_ximfbf_log-dtmvto(4).

    ENDIF.

    DELETE lit_zsdt0001        WHERE nr_romaneio  NOT IN lra_nro_romaneio.
    DELETE lit_zsdt0001        WHERE dt_movimento NOT IN lra_data_movimento.

    SORT lit_zsdt0001 BY ch_referencia.
    SORT lit_mseg     BY mblnr mjahr.

    LOOP AT lit_zpps_ximfbf_log INTO DATA(lwa_zpps_ximfbf_log).

      READ TABLE lit_zsdt0001 INTO DATA(lwa_zsdt0001) WITH KEY ch_referencia = lwa_zpps_ximfbf_log-ch_referencia BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      READ TABLE lit_mseg INTO DATA(lwa_mseg) WITH KEY mblnr = lwa_zpps_ximfbf_log-mblnr
                                                       mjahr = lwa_zpps_ximfbf_log-dtmvto(4) BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO lwa_data_response-romaneios ASSIGNING FIELD-SYMBOL(<fs_entrada_rom>).

      <fs_entrada_rom>-seq_planilha_romaneio  = lwa_zsdt0001-ch_referencia.
      <fs_entrada_rom>-nro_romaneio           = lwa_zsdt0001-nr_romaneio.
      <fs_entrada_rom>-data_movimento         = lwa_zsdt0001-dt_movimento.
      <fs_entrada_rom>-peso_liquido           = lwa_mseg-menge.
      <fs_entrada_rom>-tipo_entrada           = 'P'.
      <fs_entrada_rom>-doc_material_sap       = lwa_zpps_ximfbf_log-mblnr.

    ENDLOOP.

*-------------------------------------------------------------------------------------*
*   Seleção Entrada por Transferencia
*-------------------------------------------------------------------------------------*
    CLEAR: lit_zsdt0001[], lit_mseg[].

    IF lra_matnr[] IS NOT INITIAL.
      SELECT ch_referencia erfmg doc_material budat matnr
       FROM zmmt0006 AS a INTO TABLE lit_zmmt0006
      WHERE werks_d      EQ lwa_data_request-filial
        AND batch        EQ lwa_data_request-safra
        AND matnr        IN lra_matnr
        AND doc_material NE space
        AND NOT EXISTS (  SELECT mblnr
                            FROM mseg AS b
                           WHERE b~smbln EQ a~doc_material ).
    ENDIF.

    LOOP AT lit_zmmt0006 ASSIGNING FIELD-SYMBOL(<fs_zmmt0006>).
      <fs_zmmt0006>-ch_referencia_rom = <fs_zmmt0006>-ch_referencia(11).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fs_zmmt0006>-matnr
        IMPORTING
          output = <fs_zmmt0006>-matnr.

    ENDLOOP.

    IF lit_zmmt0006[] IS NOT INITIAL.
      SELECT ch_referencia nr_romaneio dt_movimento
        FROM zsdt0001 APPENDING TABLE lit_zsdt0001
         FOR ALL ENTRIES IN lit_zmmt0006
       WHERE ch_referencia = lit_zmmt0006-ch_referencia_rom.

      SELECT mblnr mjahr menge
        FROM mseg INTO TABLE lit_mseg
         FOR ALL ENTRIES IN lit_zmmt0006
       WHERE mblnr = lit_zmmt0006-doc_material
         AND mjahr = lit_zmmt0006-budat(4).
    ENDIF.

    DELETE lit_zsdt0001        WHERE nr_romaneio  NOT IN lra_nro_romaneio.
    DELETE lit_zsdt0001        WHERE dt_movimento NOT IN lra_data_movimento.

    SORT lit_zsdt0001 BY ch_referencia.
    SORT lit_mseg     BY mblnr mjahr.

    LOOP AT lit_zmmt0006 INTO DATA(lwa_zmmt0006).

      READ TABLE lit_zsdt0001 INTO lwa_zsdt0001 WITH KEY ch_referencia = lwa_zmmt0006-ch_referencia_rom BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      READ TABLE lit_mseg INTO lwa_mseg WITH KEY mblnr = lwa_zmmt0006-doc_material
                                                 mjahr = lwa_zmmt0006-budat(4) BINARY SEARCH.
      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO lwa_data_response-romaneios ASSIGNING <fs_entrada_rom>.

      <fs_entrada_rom>-seq_planilha_romaneio  = lwa_zsdt0001-ch_referencia.
      <fs_entrada_rom>-nro_romaneio           = lwa_zsdt0001-nr_romaneio.
      <fs_entrada_rom>-data_movimento         = lwa_zsdt0001-dt_movimento.
      <fs_entrada_rom>-peso_liquido           = lwa_mseg-menge.
      <fs_entrada_rom>-tipo_entrada           = 'T'.
      <fs_entrada_rom>-doc_material_sap       = lwa_zmmt0006-doc_material.

    ENDLOOP.

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data        = lwa_data_response
                                                        pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.
