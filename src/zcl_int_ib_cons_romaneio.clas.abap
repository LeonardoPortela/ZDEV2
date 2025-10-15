CLASS zcl_int_ib_cons_romaneio DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

*    TYPES:
*      BEGIN OF ty_ref,
*        referencia TYPE zch_ref,
*      END OF ty_ref .
*    TYPES:
*      BEGIN OF ty_rem,
*        remessa TYPE vbeln,
*      END OF ty_rem .

    DATA:
          tb_referencia TYPE TABLE OF zch_ref.
    DATA:
      tb_remessa TYPE TABLE OF vbeln .

    TYPES:
      BEGIN OF ty_group1,
        ch_referencia LIKE tb_referencia,
        doc_remessa   like tb_remessa,
      END OF ty_group1 .
    TYPES:
      BEGIN OF ty_group2,
        nr_safra     TYPE zsdt0001-nr_safra,
        bukrs        TYPE zsdt0001-bukrs,
        branch       TYPE zsdt0001-branch,
        tp_movimento TYPE zsdt0001-tp_movimento,
        nr_romaneio  TYPE zsdt0001-nr_romaneio,
        vbeln        TYPE zsdt0001-vbeln,
        matnr        TYPE zsdt0001-matnr,
        status       TYPE zsdt0001-status,
      END OF ty_group2 ,

      BEGIN OF ty_grouping_options,
        sum_peso_liq TYPE char1,
      END OF ty_grouping_options.

    DATA:
      BEGIN OF zde_data_request ,
        group_select1 TYPE ty_group1,
        group_select2 TYPE ty_group2,
        grouping_options TYPE ty_grouping_options,
        romaneios_vinculados TYPE flag,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        romaneio TYPE zsdt0001,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '119' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_ROMANEIO IMPLEMENTATION.


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


  METHOD zif_integracao_inbound~validar_dados_inbound.

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
    IF lwa_data_request-group_select1 IS INITIAL AND lwa_data_request-group_select2 IS INITIAL .
      r_msg_erro = 'Nenhum filtro foi informado!'.
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

    DATA: BEGIN OF ty_roman_vinc,
            ch_referencia TYPE zsdt0001-ch_referencia,
            nr_romaneio   TYPE zsdt0001-nr_romaneio,
            peso_liq      TYPE zsdt0001-peso_liq,
            peso_fiscal   TYPE zsdt0001-peso_fiscal,
            netwr         TYPE zsdt0001-netwr,
          END OF ty_roman_vinc.

    DATA: tb_roman_vinc LIKE TABLE OF ty_roman_vinc.

    TYPES:
      BEGIN OF ty_referencia,
        referencia TYPE zch_ref,
      END OF ty_referencia,

      BEGIN OF ty_remessa,
        remessa TYPE vbeln,
      END OF ty_remessa,

      BEGIN OF ty_tipo_s,
        bukrs        TYPE zsdt0001-bukrs,
        branch       TYPE zsdt0001-branch,
        nr_safra     TYPE zsdt0001-nr_safra,
        tp_movimento TYPE zsdt0001-tp_movimento,
        nr_romaneio  TYPE zsdt0001-nr_romaneio,
      END OF ty_tipo_s,

      BEGIN OF ty_tipo_e,
        bukrs         TYPE zsdt0001-bukrs,
        branch        TYPE zsdt0001-branch,
        nr_safra      TYPE zsdt0001-nr_safra,
        tp_movimento  TYPE zsdt0001-tp_movimento,
        id_referencia TYPE zsdt0001-id_referencia,
      END OF ty_tipo_e,

      BEGIN OF ty_response.
        INCLUDE TYPE zsdt0001.
        TYPES romaneios_vinc LIKE tb_roman_vinc.
    TYPES END OF ty_response.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response TYPE TABLE OF ty_response.

    DATA: lra_nr_safra     TYPE RANGE OF zsdt0001-nr_safra,
          lra_bukrs        TYPE RANGE OF zsdt0001-bukrs,
          lra_branch       TYPE RANGE OF zsdt0001-branch,
          lra_tp_movimento TYPE RANGE OF zsdt0001-tp_movimento,
          lra_nr_romaneio  TYPE RANGE OF zsdt0001-nr_romaneio,
          lra_vbeln        TYPE RANGE OF zsdt0001-vbeln,
          lra_matnr        TYPE RANGE OF zsdt0001-matnr,
          lra_status       TYPE RANGE OF zsdt0001-status,
          lt_referencia    TYPE TABLE OF ty_referencia,
          lt_remessa       TYPE TABLE OF ty_remessa,
          lt_tipo_s        TYPE TABLE OF ty_tipo_s,
          lt_tipo_e        TYPE TABLE OF ty_tipo_e,
          lv_romaneio      TYPE zsdt0001-nr_romaneio,
          lv_id_referencia TYPE zsdt0001-id_referencia.

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

    IF lwa_data_request-group_select2-nr_safra IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-group_select2-nr_safra ) TO lra_nr_safra.
    ENDIF.

    IF lwa_data_request-group_select2-bukrs IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-group_select2-bukrs ) TO lra_bukrs.
    ENDIF.

    IF lwa_data_request-group_select2-branch IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-group_select2-branch ) TO lra_branch.
    ENDIF.

    IF lwa_data_request-group_select2-tp_movimento IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-group_select2-tp_movimento ) TO lra_tp_movimento.
    ENDIF.

    IF lwa_data_request-group_select2-nr_romaneio IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-group_select2-nr_romaneio ) TO lra_nr_romaneio.
    ENDIF.

    IF lwa_data_request-group_select2-vbeln IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-group_select2-vbeln ) TO lra_vbeln.
    ENDIF.

    IF lwa_data_request-group_select2-matnr IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-group_select2-matnr ) TO lra_matnr.
    ENDIF.

    IF lwa_data_request-group_select2-status IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-group_select2-status ) TO lra_status.
    ENDIF.

    IF lra_status       IS NOT INITIAL OR
       lra_matnr        IS NOT INITIAL OR
       lra_vbeln        IS NOT INITIAL OR
       lra_nr_romaneio  IS NOT INITIAL OR
       lra_tp_movimento IS NOT INITIAL OR
       lra_branch       IS NOT INITIAL OR
       lra_bukrs        IS NOT INITIAL OR
       lra_nr_safra     IS NOT INITIAL.

      IF lwa_data_request-grouping_options-sum_peso_liq IS NOT INITIAL.

        SELECT SUM( peso_liq ) AS peso_liq
          FROM zsdt0001
          INTO CORRESPONDING FIELDS OF TABLE lwa_data_response
          WHERE nr_safra       IN lra_nr_safra
            AND bukrs          IN lra_bukrs
            AND branch         IN lra_branch
            AND tp_movimento   IN lra_tp_movimento
            AND nr_romaneio    IN lra_nr_romaneio
            AND vbeln          IN lra_vbeln
            AND matnr          IN lra_matnr
            AND status         IN lra_status.

      ELSE.

        SELECT *
          FROM zsdt0001
          INTO CORRESPONDING FIELDS OF TABLE lwa_data_response
          WHERE nr_safra       IN lra_nr_safra
            AND bukrs          IN lra_bukrs
            AND branch         IN lra_branch
            AND tp_movimento   IN lra_tp_movimento
            AND nr_romaneio    IN lra_nr_romaneio
            AND vbeln          IN lra_vbeln
            AND matnr          IN lra_matnr
            AND status         IN lra_status.
      ENDIF.

    ENDIF.

    lt_referencia = lwa_data_request-group_select1-ch_referencia.

    DELETE lt_referencia WHERE referencia IS INITIAL.

    IF lt_referencia[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0001
        APPENDING CORRESPONDING FIELDS OF TABLE lwa_data_response
        FOR ALL ENTRIES IN lt_referencia
        WHERE ch_referencia = lt_referencia-referencia.
    ENDIF.

    lt_remessa = lwa_data_request-group_select1-doc_remessa.

    DELETE lt_remessa WHERE remessa IS INITIAL.

    IF lt_remessa IS NOT INITIAL.

      SELECT *
        FROM zsdt0001
        APPENDING CORRESPONDING FIELDS OF TABLE lwa_data_response
        FOR ALL ENTRIES IN lt_remessa
        WHERE doc_rem = lt_remessa-remessa
          AND bukrs          IN lra_bukrs
          AND branch         IN lra_branch
          AND tp_movimento   IN lra_tp_movimento
          AND nr_romaneio    IN lra_nr_romaneio
          AND vbeln          IN lra_vbeln
          AND matnr          IN lra_matnr
          AND status         IN lra_status.

    ENDIF.

    IF lwa_data_response IS NOT INITIAL.
      IF lwa_data_request-romaneios_vinculados IS NOT INITIAL.
        LOOP AT lwa_data_response ASSIGNING FIELD-SYMBOL(<fs_data_response>).
          IF <fs_data_response>-tp_movimento EQ 'S'.
            APPEND INITIAL LINE TO lt_tipo_s ASSIGNING FIELD-SYMBOL(<fs_tipo_s>).

            <fs_tipo_s>-bukrs         = <fs_data_response>-bukrs        .
            <fs_tipo_s>-branch        = <fs_data_response>-branch       .
            <fs_tipo_s>-nr_safra      = <fs_data_response>-nr_safra     .
            <fs_tipo_s>-tp_movimento  = 'E' .
            <fs_tipo_s>-nr_romaneio   = <fs_data_response>-id_referencia.

          ELSEIF <fs_data_response>-tp_movimento EQ 'E'.
            APPEND INITIAL LINE TO lt_tipo_e ASSIGNING FIELD-SYMBOL(<fs_tipo_e>).

            <fs_tipo_e>-bukrs         = <fs_data_response>-bukrs        .
            <fs_tipo_e>-branch        = <fs_data_response>-branch       .
            <fs_tipo_e>-nr_safra      = <fs_data_response>-nr_safra     .
            <fs_tipo_e>-tp_movimento  = 'S' .
            <fs_tipo_e>-id_referencia = <fs_data_response>-nr_romaneio.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = <fs_tipo_e>-id_referencia
              IMPORTING
                output = <fs_tipo_e>-id_referencia.

          ENDIF.

        ENDLOOP.

        IF lt_tipo_s IS NOT INITIAL.
          SORT lt_tipo_s.
          DELETE ADJACENT DUPLICATES FROM lt_tipo_s COMPARING ALL FIELDS.

          SELECT *
            FROM zsdt0001
            INTO TABLE @DATA(lt_roman_vinc1)
            FOR ALL ENTRIES IN @lt_tipo_s
            WHERE bukrs         = @lt_tipo_s-bukrs
              AND branch        = @lt_tipo_s-branch
              AND nr_safra      = @lt_tipo_s-nr_safra
              AND tp_movimento  = @lt_tipo_s-tp_movimento
              AND nr_romaneio   = @lt_tipo_s-nr_romaneio.

          IF sy-subrc IS INITIAL.
            SORT lt_roman_vinc1 BY bukrs branch nr_safra tp_movimento nr_romaneio.

            LOOP AT lwa_data_response ASSIGNING <fs_data_response> WHERE tp_movimento = 'S'.

              lv_romaneio = <fs_data_response>-id_referencia.

              READ TABLE lt_roman_vinc1 TRANSPORTING NO FIELDS
              WITH KEY bukrs        = <fs_data_response>-bukrs
                       branch       = <fs_data_response>-branch
                       nr_safra     = <fs_data_response>-nr_safra
                       tp_movimento = 'E'
                       nr_romaneio  = lv_romaneio
               BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                LOOP AT lt_roman_vinc1 ASSIGNING FIELD-SYMBOL(<fs_roman_vinc1>) FROM sy-tabix.

                  IF <fs_roman_vinc1>-bukrs        <> <fs_data_response>-bukrs OR
                     <fs_roman_vinc1>-branch       <> <fs_data_response>-branch OR
                     <fs_roman_vinc1>-nr_safra     <> <fs_data_response>-nr_safra OR
                     <fs_roman_vinc1>-tp_movimento <> 'E' OR
                     <fs_roman_vinc1>-nr_romaneio  <> lv_romaneio.
                    EXIT.
                  ENDIF.

                  APPEND INITIAL LINE TO <fs_data_response>-romaneios_vinc ASSIGNING FIELD-SYMBOL(<fs_romaneios_vinc>).

                  <fs_romaneios_vinc>-ch_referencia = <fs_roman_vinc1>-ch_referencia.
                  <fs_romaneios_vinc>-nr_romaneio   = <fs_roman_vinc1>-nr_romaneio.
                  <fs_romaneios_vinc>-peso_liq      = <fs_roman_vinc1>-peso_liq.
                  <fs_romaneios_vinc>-peso_fiscal   = <fs_roman_vinc1>-peso_fiscal.
                  <fs_romaneios_vinc>-netwr         = <fs_roman_vinc1>-netwr.

                ENDLOOP.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

        IF lt_tipo_e IS NOT INITIAL.
          SORT lt_tipo_e.
          DELETE ADJACENT DUPLICATES FROM lt_tipo_e COMPARING ALL FIELDS.

          SELECT *
            FROM zsdt0001
            INTO TABLE @DATA(lt_roman_vinc2)
            FOR ALL ENTRIES IN @lt_tipo_e
            WHERE bukrs         = @lt_tipo_e-bukrs
              AND branch        = @lt_tipo_e-branch
              AND nr_safra      = @lt_tipo_e-nr_safra
              AND tp_movimento  = @lt_tipo_e-tp_movimento
              AND id_referencia = @lt_tipo_e-id_referencia.

          IF sy-subrc IS INITIAL.
            SORT lt_roman_vinc2 BY bukrs branch nr_safra tp_movimento id_referencia.

            LOOP AT lwa_data_response ASSIGNING <fs_data_response> WHERE tp_movimento = 'E'.

              lv_id_referencia = <fs_data_response>-nr_romaneio.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = lv_id_referencia
                IMPORTING
                  output = lv_id_referencia.

              READ TABLE lt_roman_vinc2 TRANSPORTING NO FIELDS
              WITH KEY bukrs        = <fs_data_response>-bukrs
                       branch       = <fs_data_response>-branch
                       nr_safra     = <fs_data_response>-nr_safra
                       tp_movimento = 'S'
                       id_referencia = lv_id_referencia
               BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                LOOP AT lt_roman_vinc2 ASSIGNING FIELD-SYMBOL(<fs_roman_vinc2>) FROM sy-tabix.

                  IF <fs_roman_vinc2>-bukrs          <> <fs_data_response>-bukrs OR
                     <fs_roman_vinc2>-branch         <> <fs_data_response>-branch OR
                     <fs_roman_vinc2>-nr_safra       <> <fs_data_response>-nr_safra OR
                     <fs_roman_vinc2>-tp_movimento   <> 'S' OR
                     <fs_roman_vinc2>-id_referencia  <> lv_id_referencia.
                    EXIT.
                  ENDIF.

                  APPEND INITIAL LINE TO <fs_data_response>-romaneios_vinc ASSIGNING <fs_romaneios_vinc>.

                  <fs_romaneios_vinc>-ch_referencia = <fs_roman_vinc2>-ch_referencia.
                  <fs_romaneios_vinc>-nr_romaneio   = <fs_roman_vinc2>-nr_romaneio.
                  <fs_romaneios_vinc>-peso_liq      = <fs_roman_vinc2>-peso_liq.
                  <fs_romaneios_vinc>-peso_fiscal   = <fs_roman_vinc2>-peso_fiscal.
                  <fs_romaneios_vinc>-netwr         = <fs_roman_vinc2>-netwr.

                ENDLOOP.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).

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
