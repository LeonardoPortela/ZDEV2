CLASS zcl_int_ib_cria_equipamento_se DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_dados_obrigatorios,
        grupo               TYPE ktokk,
        regiao              TYPE regio,
        ie                  TYPE stcd3,
        cnpj                TYPE stcd1,
        cpf                 TYPE stcd2,
        data_nasc           TYPE char10,
        nis_pis             TYPE kraus_cm,
        nome                TYPE name1_gp,
        rntrc_antt          TYPE bahns,
        emissor_nota_fiscal TYPE char1,
      END OF ty_dados_obrigatorios .
    TYPES:
      BEGIN OF ty_dados_banco,
        id          TYPE char4,
        regiao      TYPE char2,
        chave_banco TYPE bu_bankk,
        conta       TYPE bu_bankn,
      END OF ty_dados_banco .
    TYPES:
      tb_empresas TYPE TABLE OF bukrs .
    TYPES:
      BEGIN OF ty_endereco,
        nome   TYPE addr1_data-name1,
        cep    TYPE ad_pstcd1,
        pais   TYPE land1,
        regiao TYPE addr1_data-region,
      END OF ty_endereco .

    DATA:
      tb_erros  TYPE TABLE OF bapiret2-message .
    DATA:
      BEGIN OF zde_data_request,
        empresa      TYPE bukrs,
        classe       TYPE itobattr-klasse,
        tipo_veiculo TYPE itob-eqart,
        fabricante   TYPE itob-herst,
        modelo       TYPE itob-typbz,
        numero_serie TYPE itob-serge,
        pais         TYPE itob-herld,
        ano          TYPE itob-baujj,
        centro       TYPE itob-swerk,
        centro_custo TYPE itob-kostl,
        imobilizado  TYPE fleet-zzimobilizado,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        equipamento TYPE equi-equnr,
        mensagem    TYPE string,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '247' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_int_ib_cria_equipamento_se IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD zif_integracao_inbound~configure_server.

    DATA: lva_reason TYPE string,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          output = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code        = lva_code
        IMPORTING
          e_desc_status = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = CONV #( lva_code )
          reason = CONV #( lva_reason )
       ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inbound~processar_requisicao.

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

  ENDMETHOD.


  METHOD zif_integracao_inbound~set_data.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.
*
    DATA: lwa_data_request LIKE zde_data_request,
          wa_tx            TYPE zsdt0327tx,
          lv_message       TYPE string,
          lva_type         TYPE dd01v-datatype,
          lv_objnr         TYPE fleet-objnr.

    DATA: lr_chave_banco  TYPE RANGE OF bnka-bankl,
          lr_chave_banco2 TYPE RANGE OF bnka-bankl.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'LK'.
      wa_tx-msg_processamento = 'Metodo informado não previsto!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

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
    IF lwa_data_request IS INITIAL.
      r_msg_erro = 'Nenhum filtro foi informado!'.
      RETURN.
    ELSEIF lwa_data_request-ano IS INITIAL OR
           lwa_data_request-centro IS INITIAL OR
           lwa_data_request-centro_custo IS INITIAL OR
           lwa_data_request-classe IS INITIAL OR
           lwa_data_request-empresa IS INITIAL OR
           lwa_data_request-fabricante IS INITIAL OR
           lwa_data_request-imobilizado IS INITIAL OR
           lwa_data_request-modelo IS INITIAL OR
           lwa_data_request-numero_serie IS INITIAL OR
           lwa_data_request-pais IS INITIAL OR
           lwa_data_request-tipo_veiculo IS INITIAL.

      r_msg_erro = 'Necessário preencher todos os campos!'.
      RETURN.

    ENDIF.

    "FF #188919 - inicio

    "---Valida Centro de custo

    DATA(lv_len) = strlen( lwa_data_request-centro_custo ) - 4.

    IF lv_len >= 0.
      DATA(lv_last4) = lwa_data_request-centro_custo+lv_len(4).
    ELSE.
      lv_last4 = lwa_data_request-centro_custo.
    ENDIF.

    SELECT SINGLE *
    FROM zpmt0001
    WHERE eqart  = @lwa_data_request-tipo_veiculo
      AND kostlg = @lv_last4
      INTO @DATA(dummy).

    IF sy-subrc <> 0.

      r_msg_erro = 'Centro de custo incorreto para a categoria do equipamento.'.
      RETURN.

    ENDIF.


    "----Check tipo de veículo

    DATA lv_tp_veiculo TYPE zval.
    lv_tp_veiculo = lwa_data_request-tipo_veiculo.

    CONDENSE lv_tp_veiculo NO-GAPS.


    SELECT *
      FROM ztparam
      INTO @DATA(ls_dummy)
      UP TO 1 ROWS
      WHERE param = 'TP_IMPLEM'
        AND zval  = @lv_tp_veiculo.
    ENDSELECT.

    IF sy-subrc <> 0. "Se achar dados no select acima, não validar o tq comb, pois é um veículo sem tanque.
      "FF #188919 - fim

*** Inicio - Rubenilson - 14.01.25 - BUG163330
      SELECT tq_comb
        FROM zpmr0001
        INTO @DATA(lv_tq_comb)
        UP TO 1 ROWS
        WHERE herst      = @lwa_data_request-fabricante
          AND typbz      = @lwa_data_request-modelo
          AND class_oper = @lwa_data_request-tipo_veiculo
          AND tq_comb <> ' '.
      ENDSELECT.
      IF sy-subrc IS NOT INITIAL.
        r_msg_erro = 'Não há informação de tq comb cadastrado na ZPM0017'.
        RETURN.
      ENDIF.

    ENDIF.
*** Fim - Rubenilson - 14.01.25 - BUG163330

    IF lwa_data_request-imobilizado IS NOT INITIAL.
      SELECT objnr
        FROM fleet
        INTO lv_objnr
        UP TO 1 ROWS
        WHERE zzimobilizado = lwa_data_request-imobilizado.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        SELECT a~equnr,b~eqktx
          FROM equi AS a
          INNER JOIN eqkt AS b
          ON a~equnr = b~equnr
          INTO @DATA(ls_equi)
          UP TO 1 ROWS
          WHERE a~objnr = @lv_objnr.
        ENDSELECT.
        IF sy-subrc IS INITIAL.

          DATA(lv_equnr) = | { ls_equi-equnr ALPHA =  IN } |.

          CONCATENATE 'Imobilizado já vinculado:' lv_equnr ls_equi-eqktx INTO r_msg_erro.

          RETURN.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.


  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response,
          ls_addr1          TYPE addr1_data,
          ls_lfa1           TYPE lfa1,
          ls_kna1           TYPE kna1,
          wa_tx             TYPE zsdt0327tx,
          lt_tx             TYPE TABLE OF zsdt0327tx,
          ls_itobattr       TYPE itobattr,
          ls_equi_aux       TYPE equi,
          ls_eqkt_aux       TYPE eqkt,
          ls_equz_aux       TYPE equz,
          ls_iloa_aux       TYPE iloa,
          ls_fleet_aux      TYPE fleet,
          ls_equi_aux2      TYPE equi,
          ls_eqkt_aux2      TYPE eqkt,
          ls_equz_aux2      TYPE equz,
          ls_iloa_aux2      TYPE iloa,
          ls_fleet_aux2     TYPE fleet.


    DATA:
      lra_mblnr TYPE RANGE OF mblnr,
      lra_mjahr TYPE RANGE OF mjahr,
      lra_budat TYPE RANGE OF budat,
      lra_bldat TYPE RANGE OF bldat,
      lra_xblnr TYPE RANGE OF xblnr,
      lra_smbln TYPE RANGE OF mblnr,
      lra_smblp TYPE RANGE OF mblpo,
      lra_mat   TYPE RANGE OF mblnr.

    DATA: lr_stat TYPE RANGE OF jest-stat.

    lr_stat = VALUE #(
      ( sign = 'I' option = 'EQ' low = 'I0098' )
      ( sign = 'I' option = 'EQ' low = 'I0320' )
      ( sign = 'I' option = 'EQ' low = 'I0076' )
    ).

    DATA: gwa_datageneral     TYPE bapi_itob,
          gwa_data_generalx   TYPE bapi_itobx,
          gwa_datafleet       TYPE bapi_fleet,
          gwa_data_fleetx     TYPE bapi_fleetx,
          gwa_datageneralexp  TYPE bapi_itob,
          gwa_dataspecificexp TYPE bapi_itob_eq_only,
          gwa_datainstall     TYPE bapi_itob_eq_install,
          gwa_return          TYPE bapiret2,
          gwa_datafleetexp    TYPE bapi_fleet,
          gwa_externalnumber  TYPE bapi_itob_parms-equipment,
          git_xtensionin      TYPE TABLE OF bapiparex,
          gwa_dataspecific    TYPE bapi_itob_eq_only,
          gva_valid_date      TYPE sy-datum,
          lva_equipment       TYPE bapi_itob_parms-equipment,
          lv_ktx01            TYPE ktx01,
          lv_external         TYPE  bapi_itob_parms-equipment,
          lt_values           TYPE TABLE OF bapi1003_alloc_values_num,
          lt_values2          TYPE TABLE OF bapi1003_alloc_values_char,
          lt_values3          TYPE TABLE OF bapi1003_alloc_values_curr,
          lt_return           TYPE TABLE OF bapiret2,
          lv_objkey           TYPE bapi1003_key-object.

    DATA: lr_equnr  TYPE RANGE OF equi-equnr,
          lv_seq    TYPE numc4,
          lv_qtd    TYPE sy-tabix,
          lv_status TYPE c.

    DATA: lv_cont    TYPE i,
          lv_obj_ant TYPE iflo-objnr.

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

    gwa_datageneral-comp_code = lwa_data_request-empresa.
    gwa_dataspecific-equicatgry = '1'.
    gwa_datageneral-objecttype = lwa_data_request-tipo_veiculo.
    gwa_datageneral-authgrp = '0001'.
    gwa_datageneral-constmonth = sy-datum+4(2).
    gwa_datageneral-manfacture = lwa_data_request-fabricante.
    gwa_datageneral-manmodel = lwa_data_request-modelo.
    gwa_datageneral-manparno = '1'.
    gwa_datageneral-manserno = lwa_data_request-numero_serie.
    gwa_datageneral-mancountry = lwa_data_request-pais.
    gwa_datageneral-constyear = lwa_data_request-ano.
    gwa_datageneral-maintplant = lwa_data_request-centro.
    gwa_datageneral-abcindic = 'A'. "Ajuste de 'C' para 'A' - MMSILVA - 01.07.2025 - Solicitado pelo Gabriel Costa
    SHIFT lwa_data_request-centro_custo RIGHT DELETING TRAILING space.
    TRANSLATE lwa_data_request-centro_custo USING ' 0'.
    gwa_datageneral-costcenter = lwa_data_request-centro_custo.
    gwa_datageneral-sortfield = 'AMBIENTALMENTE CRITICO'.
    gwa_datafleet-fleet_num = lwa_data_request-imobilizado.
    gwa_datafleet-license_num = 'N/A'.
    gwa_datafleet-card_num = 'N/A'.
    gwa_datafleet-chassis_num = lwa_data_request-numero_serie.


    "Bug solto 184317 - Alterar forma de buscar o local de instalação - RGA - ini
    SELECT if~tplnr, je~objnr, je~stat, je~inact
      FROM iflo AS if
      INNER JOIN jest AS je
      ON if~objnr = je~objnr
      INTO TABLE @DATA(lt_iflo)
      WHERE if~iwerk = @lwa_data_request-centro
        AND if~fltyp = 'Y'.

    DELETE lt_iflo WHERE stat NOT IN lr_stat.

    SORT lt_iflo.

    DATA(lt_iflo_val) = lt_iflo.

    "elimina objetos inválidos (status = I0320)
    LOOP AT lt_iflo INTO DATA(ls_iflo).

      READ TABLE lt_iflo WITH KEY objnr = ls_iflo-objnr
                                  stat  = 'I0320'
                                  inact = '' INTO DATA(ls_iflo_del).
      IF sy-subrc EQ 0.
        DELETE lt_iflo_val WHERE objnr = ls_iflo_del-objnr.
      ENDIF.

      CLEAR ls_iflo_del.
      READ TABLE lt_iflo WITH KEY objnr = ls_iflo-objnr
                                  stat  = 'I0076'
                                  inact = '' INTO ls_iflo_del.
      IF sy-subrc EQ 0.
        DELETE lt_iflo_val WHERE objnr = ls_iflo_del-objnr.
      ENDIF.

    ENDLOOP.

    IF lines( lt_iflo_val ) > 0.

      CLEAR: lv_cont, lv_obj_ant.

      SORT lt_iflo BY objnr.
      SORT lt_iflo_val BY objnr.

      DELETE lt_iflo WHERE inact = 'X'.

      LOOP AT lt_iflo INTO ls_iflo.


        READ TABLE lt_iflo_val WITH KEY objnr = ls_iflo-objnr
                                        stat  = 'I0098' TRANSPORTING NO FIELDS.

        IF sy-subrc EQ 0.

          "verificar se o objnr já foi lido
*          DATA(lv_tabix_ant) = lv_tabix - 1.
*
*          IF lv_tabix_ant > 0.
*
*            READ TABLE lt_iflo_val WITH KEY objnr = ls_iflo-objnr
*                                            stat  = 'I0098' INTO DATA(ls_iflo_ant).
*            IF sy-subrc eq 0.
*
*              IF ls_iflo-objnr NE .
*
*              ENDIF.
*
*            ENDIF.
*
*
*          ENDIF.

          lv_cont = lv_cont + 1.

          "local válido
          DATA(lv_loc_valido) = abap_true.

          DATA(lv_tpnlr) = ls_iflo-tplnr.

        ENDIF.

      ENDLOOP.

      IF lv_loc_valido = abap_false.

        e_nm_code   = '400'.
        lwa_data_response-mensagem = 'Centro não possui local de instalação válido, favor verificar'.
        e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).
        RETURN.

      ELSE.

        IF lv_cont > 1.

          e_nm_code   = '400'.
          lwa_data_response-mensagem = 'Centro possui mais de um local de instalação válido, favor verificar'.

          e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).
          RETURN.

        ENDIF.

      ENDIF.

    ELSE.

      e_nm_code   = '400'.
      lwa_data_response-mensagem = 'Centro não possui local de instalação válido, favor verificar'.

      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).
      RETURN.

    ENDIF.

    "Bug solto 184317 - Alterar forma de buscar o local de instalação - RGA - fim

    SELECT objid
      FROM crhd
      INTO @DATA(lv_objid)
      UP TO 1 ROWS
      WHERE arbpl = 'OFICINA'
        AND werks = @lwa_data_request-centro.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      gwa_datageneral-work_ctr = lv_objid.
    ENDIF.

    SELECT tq_comb,rbnr
      FROM zpmr0001
      INTO @DATA(ls_zpmr0001)
      UP TO 1 ROWS
      WHERE herst      = @lwa_data_request-fabricante
        AND typbz      = @lwa_data_request-modelo
        AND class_oper = @lwa_data_request-tipo_veiculo. "BUG - 181493 - CBRAND
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      gwa_datafleet-key_num  = ls_zpmr0001-tq_comb.
      gwa_datageneral-catprofile = ls_zpmr0001-rbnr.
      gwa_datageneral-plangroup = 'FRO'.
    ENDIF.

    gwa_datafleet-num_axle = '2'.

*** Montagem do texto do equipamento
    zcl_pm_data_equipament=>zif_pm_data_equipament~get_instance(
    )->set_eartx( i_eqart =  lwa_data_request-tipo_veiculo
    )->set_desc_fabricante( i_herst =  lwa_data_request-fabricante
    )->set_desc_modelo( i_typbz =  lwa_data_request-modelo
    )->set_desc_eqpto( IMPORTING e_ktx01 = lv_ktx01 ).

    gwa_datageneral-descript = lv_ktx01.

*** Início - Montagem do número do equipamento
    APPEND INITIAL LINE TO lr_equnr ASSIGNING FIELD-SYMBOL(<fs_equnr>).
    <fs_equnr>-sign = 'I'.
    <fs_equnr>-option = 'CP'.
    CONCATENATE '1' lwa_data_request-tipo_veiculo '****' INTO <fs_equnr>-low.

    SHIFT <fs_equnr>-low RIGHT DELETING TRAILING ' '.
    TRANSLATE <fs_equnr>-low USING ' 0'.


    SELECT MAX( equnr )
      FROM equi
      INTO @DATA(lv_equnr)
      WHERE equnr IN @lr_equnr.
    IF sy-subrc IS INITIAL AND lv_equnr IS NOT INITIAL.

      lv_qtd = strlen( lv_equnr ).
      lv_qtd = lv_qtd - 4.
      lv_seq = lv_equnr+lv_qtd.

    ENDIF.

    lv_seq = lv_seq + 1.

    CONCATENATE '1' lwa_data_request-tipo_veiculo lv_seq INTO lv_external.
    SHIFT lv_external RIGHT DELETING TRAILING ' '.
    TRANSLATE lv_external USING ' 0'.
*** Fim - Montagem do número do equipamento

    gwa_dataspecific-techid = lv_external.

    gwa_datainstall-funcloc = lv_tpnlr. "lwa_data_request-centro && '.FRO'.

    CALL FUNCTION 'ZBAPI_EQUI_CREATE'
      EXPORTING
        external_number   = lv_external
        data_general      = gwa_datageneral
        data_specific     = gwa_dataspecific
        data_fleet        = gwa_datafleet
        valid_date        = gva_valid_date
        data_install      = gwa_datainstall
      IMPORTING
        equipment         = lva_equipment
        data_general_exp  = gwa_datageneralexp
        data_specific_exp = gwa_dataspecificexp
        data_fleet_exp    = gwa_datafleetexp
        return            = gwa_return
      EXCEPTIONS
        OTHERS            = 01.
    CASE gwa_return-type.
      WHEN space.            " OK

        COMMIT WORK AND WAIT.

*** Atualização do equipamento com o número da classe
        lv_objkey = lva_equipment.

        CALL FUNCTION 'BAPI_OBJCL_CHANGE'
          EXPORTING
            objectkey          = lv_objkey
            objecttable        = 'EQUI'
            classnum           = lwa_data_request-classe
            classtype          = '002'
          IMPORTING
            classif_status     = lv_status
          TABLES
            allocvaluesnumnew  = lt_values
            allocvaluescharnew = lt_values2
            allocvaluescurrnew = lt_values3
            return             = lt_return.
        IF NOT line_exists( lt_return[ type = 'E' ] ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.

        CALL FUNCTION 'EQUIPMENT_READ'
          EXPORTING
            equi_no        = lva_equipment
          IMPORTING
            equi           = ls_equi_aux
            eqkt           = ls_eqkt_aux
            equz           = ls_equz_aux
            iloa           = ls_iloa_aux
            fleet          = ls_fleet_aux
          EXCEPTIONS
            auth_no_begrp  = 1
            auth_no_iwerk  = 2
            auth_no_swerk  = 3
            eqkt_not_found = 4
            equi_not_found = 5
            equz_not_found = 6
            iloa_not_found = 7
            auth_no_ingrp  = 8
            auth_no_kostl  = 9
            err_handle     = 10
            lock_failure   = 11
            auth_no_badi   = 12
            OTHERS         = 13.
        IF sy-subrc = 0.

          MOVE-CORRESPONDING ls_equi_aux TO  ls_equi_aux2.
          MOVE-CORRESPONDING ls_eqkt_aux TO  ls_eqkt_aux2.
          MOVE-CORRESPONDING ls_equz_aux TO  ls_equz_aux2.
          MOVE-CORRESPONDING ls_iloa_aux TO  ls_iloa_aux2.
          MOVE-CORRESPONDING ls_fleet_aux TO ls_fleet_aux2.

          ls_fleet_aux2-zzimobilizado = lwa_data_request-imobilizado.
*          ls_iloa_aux2-tplnr = lwa_data_request-centro && '.FRO'.

*** Atualização do equipamento com os números do imobilzado e local de instalação
          CALL FUNCTION 'EQUIPMENT_UPDATE'
            EXPORTING
              *heqkt    = ls_eqkt_aux
              *hequi    = ls_equi_aux
              *hequz    = ls_equz_aux
              *hiloa    = ls_iloa_aux
              *hfleet   = ls_fleet_aux
              heqkt     = ls_eqkt_aux2
              hequi     = ls_equi_aux2
              hequz     = ls_equz_aux2
              hiloa     = ls_iloa_aux2
              hfleet    = ls_fleet_aux2
              haktyp    = 'V'
              hx_xreftp = 'M'.

          COMMIT WORK AND WAIT.

        ENDIF.

        e_sucesso   = abap_true.
        e_nm_code   = '200'.
        e_msg_erro  = 'Ok'.

        lwa_data_response-equipamento = lva_equipment.

        CONCATENATE 'Equipamento criado:' lva_equipment INTO lwa_data_response-mensagem SEPARATED BY space.

        SELECT SINGLE *
          FROM equi
          INTO @DATA(ls_equi)
          WHERE equnr = @lva_equipment.
        IF sy-subrc IS INITIAL.

          SELECT *
            FROM equz
            INTO @DATA(ls_equz)
            UP TO 1 ROWS
            WHERE equnr = @lva_equipment.
          ENDSELECT.

          SELECT SINGLE *
            FROM fleet
            INTO @DATA(ls_fleet)
            WHERE objnr = @ls_equi-objnr.

          SELECT iloan
            FROM itob
            INTO @DATA(lv_iloan)
            UP TO 1 ROWS
            WHERE equnr = @lva_equipment.
          ENDSELECT.
          IF sy-subrc IS INITIAL.
            SELECT SINGLE *
              FROM iloa
              INTO @DATA(ls_iloa)
              WHERE iloan = @lv_iloan.
          ENDIF.
        ENDIF.

        ls_itobattr-klasse = lwa_data_request-classe.

*** Criação de ordem de planejamento, planos de manutenção e pontos de medição
        zcl_pm_data_equipament=>zif_pm_data_equipament~get_instance(
            )->set_pos_contador( i_pos_contador = ls_fleet-zzhor_odom_inicial
            )->set_data_eqpto( i_equi = ls_equi i_equz = ls_equz i_iloa = ls_iloa i_itobattr = ls_itobattr i_fleet = ls_fleet ).

      WHEN OTHERS.
        e_nm_code   = '400'.
        lwa_data_response-mensagem = gwa_return-message.
    ENDCASE.

    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_parametro.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
