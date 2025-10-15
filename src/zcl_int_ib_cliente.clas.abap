CLASS zcl_int_ib_cliente DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:

      BEGIN OF ty_erros,
        mensagem TYPE char100,
      END OF ty_erros.

      data tl_erro TYPE  TABLE OF ty_erros.

      types:

      BEGIN OF ty_erro,
        idorigem TYPE  char20,
        erros    like tl_erro,
      END OF ty_erro.

    DATA:
      BEGIN OF zde_data_response,
        erros TYPE TABLE OF ty_erro,
      END OF zde_data_response .

    METHODS constructor .
protected section.
private section.

  data AT_ID_INTERFACE type ZDE_ID_INTERFACE value '156' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_INT_IB_CLIENTE IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_inbound. "Out
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_assincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'CADASTRO_CLIENTE'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

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

  ENDMETHOD.


  METHOD zif_integracao_inbound~processar_requisicao.

    DATA: lc_integracao TYPE REF TO zcl_integracao,
          lva_msg_erro  TYPE string.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->free(
      ).

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound = me->zif_integracao_inject~at_info_request_http-ds_body
                                                      IMPORTING e_status_code  = DATA(_status_code)
                                                      RECEIVING  r_msg_erro = lva_msg_erro
                                                                                  ).
    IF lva_msg_erro IS NOT INITIAL.

      IF _status_code IS INITIAL .
        _status_code = '400'. "Bad Request
      ENDIF.

      me->zif_integracao_inbound~at_zintegracao_log-nm_code = _status_code.

      e_msg = lva_msg_erro.

*      e_msg = '{  "error": "'        && lva_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
*              '   "status_code" : "' && _status_code  && '" '  && cl_abap_char_utilities=>newline &&
*                 '}'.

      UPDATE zintegracao SET ck_processado   = abap_true
                             ck_integrado    = abap_true
                             ds_data_retorno =  e_msg
                       WHERE id_integracao = me->zif_integracao_inbound~at_id_integracao.
      COMMIT WORK.

    ELSE.
      me->zif_integracao_inbound~at_zintegracao_log-nm_code = '200'.
      e_msg = '{"SUCESSO":"OK"}'.
    ENDIF.

    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_inbound.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA: w_data_inbound   TYPE zsdt0325_inb_cliente,
          lit_data_inbound TYPE zsdt0325_inbcli_t,
          w_tx             TYPE zsdt0327tx,
          lva_kna1         TYPE kna1,
          lva_tabix        TYPE sy-tabix,
          lva_duplic_flg   TYPE c,
          lva_kunnret      TYPE kna1-kunnr,
          lva_erro         TYPE string,
          lva_type         TYPE dd01v-datatype,
          l_erro(1).

    DATA: lwa_data_response_erro LIKE zde_data_response.


    TYPES:
      BEGIN OF ty_erro,
        mensagem TYPE char100,
      END OF ty_erro.

    DATA: it_mensagem TYPE TABLE OF  ty_erro,
          wa_mensagem TYPE  ty_erro.

    CLEAR: r_msg_erro.

    IF ( me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'POST' AND  me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'PUT' ).
      r_msg_erro = 'Metodo informado não reconhecido!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_data_inbound ).


    "FIELD-SYMBOLS: <fs_erro> TYPE lit_data_inbound.

    LOOP AT lit_data_inbound INTO DATA(lwa_data_inbound).

      APPEND INITIAL LINE TO lwa_data_response_erro-erros ASSIGNING FIELD-SYMBOL(<fs_erro>).
*
*      "IDOrigem
      IF lwa_data_inbound-idorigem IS INITIAL.
        wa_mensagem = 'idOrigem deve estar preenchido.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.
        <fs_erro>-idorigem = lwa_data_inbound-idorigem.
      ENDIF.

      "OrigemCad
      IF lwa_data_inbound-origemcadastro IS INITIAL.
        wa_mensagem = 'origemCadastro deve estar preenchido.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSEIF lwa_data_inbound-origemcadastro NE 'EC'.
        wa_mensagem = 'origemCadastro incorreto'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ENDIF.
*
*      "genero
      IF lwa_data_inbound-genero IS NOT INITIAL AND ( lwa_data_inbound-genero NE 'MASCULINO' AND lwa_data_inbound-genero NE 'FEMININO'  AND lwa_data_inbound-genero NE 'EMPRESA' ).
        wa_mensagem = 'genero deve ser MASCULINO, FEMININO ou EMPRESA'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ENDIF.
*
*      "dtNascimento
      IF lwa_data_inbound-datanascimento IS NOT INITIAL.
        "validar formato
        DATA(length2) = strlen( lwa_data_inbound-datanascimento ).
        IF length2 <> 10.
          wa_mensagem = 'dataNascimento deve estar no formato YYYY-MM-DD.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          CLEAR: wa_mensagem.
          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-datanascimento(4)
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'dataNascimento deve estar no formato YYYY-MM-DD.'.
            l_erro = 'X'.
            "APPEND wa_mensagem TO <fs_erro>-erros.
          ENDIF.

          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-datanascimento+5(2)
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'dataNascimento deve estar no formato YYYY-MM-DD.'.
            l_erro = 'X'.
            "APPEND wa_mensagem TO <fs_erro>-erros.
          ENDIF.

          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-datanascimento+8(2)
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'dataNascimento deve estar no formato YYYY-MM-DD.'.
            l_erro = 'X'.
            "APPEND wa_mensagem TO <fs_erro>-erros.
          ENDIF.

          IF lwa_data_inbound-datanascimento+4(1) <> '-' OR lwa_data_inbound-datanascimento+7(1) <> '-'.
            wa_mensagem =  'dataNascimento  deve estar no formato YYYY-MM-DD.'.
            l_erro = 'X'.
            "APPEND wa_mensagem TO <fs_erro>-erros.
          ENDIF.

          IF wa_mensagem IS NOT INITIAL.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.
        "  wa_mensagem =  'Data está no formato incorreto (formato esperado (YYYY-MM-DD)'.
      ENDIF.
*
*      "CNPJ
      IF lwa_data_inbound-cnpj IS NOT INITIAL AND lwa_data_inbound-cnpj  CA '/.,:;"[]\{}|<>?/'.
        wa_mensagem =  'cnpj deve ser informado apenas os números.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.

        CLEAR: lva_type.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = lwa_data_inbound-cnpj
          IMPORTING
            htype     = lva_type.

        IF lva_type NE 'NUMC'.
          wa_mensagem =  'cnpj deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.
      ENDIF.
*
*      "CPF
      IF lwa_data_inbound-cpf IS NOT INITIAL AND lwa_data_inbound-cpf  CA '/.,:;"[]\{}|<>?/'.

        wa_mensagem = 'cpf deve ser informado apenas os números.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.
        CLEAR: lva_type.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = lwa_data_inbound-cpf
          IMPORTING
            htype     = lva_type.

        IF lva_type NE 'NUMC'.

          wa_mensagem = 'cpf deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.

        ENDIF.
      ENDIF.
**
*      "Inscrição Estadual
      IF ( lwa_data_inbound-grupoconta EQ 'ZCPF' OR  lwa_data_inbound-grupoconta EQ 'ZCPJ' OR lwa_data_inbound-grupoconta EQ 'ZCFJ' OR lwa_data_inbound-grupoconta EQ 'ZCNJ' )
        AND lwa_data_inbound-inscestadual IS INITIAL.
        wa_mensagem =  'inscEstadual deve estar preenchido.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.
        IF lwa_data_inbound-inscestadual IS NOT INITIAL AND lwa_data_inbound-inscestadual  CA '/.,:;"[]\{}|<>?/'.
          wa_mensagem =  'inscEstadual deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-inscestadual
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'inscEstadual deve ser informado apenas os números.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
*
*
*      " InscricaoMunicipal
      IF lwa_data_inbound-inscmunicipal IS NOT INITIAL AND lwa_data_inbound-inscmunicipal  CA '/.,:;"[]\{}|<>?/'.
        wa_mensagem =  'inscMunicipal deve ser informado apenas os números.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.
        CLEAR: lva_type.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = lwa_data_inbound-inscmunicipal
          IMPORTING
            htype     = lva_type.

        IF lva_type NE 'NUMC'.
          wa_mensagem =  'inscMunicipal deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.
      ENDIF.
*
*      "rg
      IF lwa_data_inbound-rg IS NOT INITIAL.
        IF lwa_data_inbound-rg  CA '/.,:;"[]\{}|<>?/'.
          wa_mensagem =  'rg  deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-rg
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'rg deve ser informado apenas os números.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.

        ENDIF.

        IF lwa_data_inbound-orgexp IS INITIAL.
          wa_mensagem =  'orgexp deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        IF lwa_data_inbound-ufexp IS INITIAL.
          wa_mensagem =  'ufexp deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          "verifica se a UF está cadastrada no SAP.Tabela T005S-BLAND
          SELECT SINGLE * FROM t005s
            INTO @DATA(wl_t005s)
            WHERE bland = @lwa_data_inbound-ufexp
            and land1 eq @lwa_data_inbound-pais.
          IF sy-subrc IS NOT INITIAL.
            CONCATENATE 'ufexp não cadastrado nos dados mestres do sistema SAP para o País' lwa_data_inbound-pais into wa_mensagem SEPARATED BY space.

            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
*
*
      IF me->zif_integracao_inject~at_info_request_http-ds_metodo EQ 'POST'.

        "GrupoConta
        IF lwa_data_inbound-grupoconta IS INITIAL.
          wa_mensagem =  'grupoConta deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE. "validar se o que foi informado está na tabela do SAP Tabela T077X-KTOKD
          SELECT SINGLE * FROM  t077x
            INTO @DATA(wl_t077x)
            WHERE ktokd = @lwa_data_inbound-grupoconta.

          IF sy-subrc IS NOT INITIAL.
            wa_mensagem =   'grupoConta não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

        "Empresa
        IF lwa_data_inbound-empresa IS INITIAL.
          wa_mensagem =  'empresa deve estar preenchida.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.

        ELSE . "validar se o que foi informado está na tabela do SAP Tabela T001-BUKRS
          SELECT SINGLE * FROM  t001
            INTO @DATA(wl_t001)
            WHERE bukrs = @lwa_data_inbound-empresa.

          IF sy-subrc IS NOT INITIAL.

            wa_mensagem =  'empresa não cadastrada nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

        "SetorAtividade
        IF lwa_data_inbound-setoratividade IS NOT INITIAL.
          " validar se o que foi informado está na tabela do SAP TSPA-SPART

          SELECT SINGLE * FROM  tspa
            INTO @DATA(wl_tspa)
            WHERE spart = @lwa_data_inbound-setoratividade.

          IF sy-subrc IS NOT INITIAL.

            wa_mensagem =  'setorAtividade não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

        "nome
        IF lwa_data_inbound-nome IS INITIAL.
          wa_mensagem =  'nome deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        " endereco
        IF lwa_data_inbound-endereco IS INITIAL .
          wa_mensagem =  'endereco deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        "cidade
        IF lwa_data_inbound-cidade IS INITIAL .
          wa_mensagem =  'cidade deve estar preenchida.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        "bairro
        IF lwa_data_inbound-bairro  IS INITIAL .
          wa_mensagem =  'bairro deve estar preenchido.' .
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        "uf
        IF lwa_data_inbound-uf  IS INITIAL.
          wa_mensagem =  'uf deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          SELECT  * FROM t005u
            INTO TABLE @DATA(lit_t005u)
             WHERE spras = @sy-langu
               AND land1 = @lwa_data_inbound-pais
               AND bland = @lwa_data_inbound-uf.

          IF sy-subrc <> 0.
            " concatenate   'Código da região incorreto '  lwa_data_inbound-uf  'para pais' lwa_data_inbound-pais into wa_mensagem .
            "wa_mensagem = 'uf não cadastrado nos dados mestres do sistema SAP.'.
            CONCATENATE 'uf não cadastrado nos dados mestres do sistema SAP para o País' lwa_data_inbound-pais into wa_mensagem SEPARATED BY space.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.

          ENDIF.
        ENDIF.

        "Cep
        IF lwa_data_inbound-cep  IS INITIAL.
          wa_mensagem =  'cep deve ser informado.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.

          DATA(length) = strlen( lwa_data_inbound-cep ).
          IF length <> 9.
            "CONCATENATE 'Código Postal'   lwa_data_inbound-cep  'deve ter comprimento 9' into wa_mensagem .
            wa_mensagem = 'cep  deve estar no formato XXXXX-XXX.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ELSE.
            CLEAR wa_mensagem.
            IF lwa_data_inbound-cep+5(1) <> '-'.
              wa_mensagem = 'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.

            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = lwa_data_inbound-cep(5)
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              wa_mensagem =  'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = lwa_data_inbound-cep+6(3)
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              wa_mensagem =  'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.

            IF wa_mensagem IS NOT INITIAL.
              APPEND wa_mensagem TO <fs_erro>-erros.
              l_erro = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.

        "Pais
        IF lwa_data_inbound-pais  IS INITIAL.
          wa_mensagem =  'pais deve estar preenchido'.".'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.

          SELECT * FROM t005t
            INTO TABLE @DATA(lit_t005)
             WHERE spras EQ @sy-langu
               AND land1 EQ @lwa_data_inbound-pais.

          IF sy-subrc <> 0.
            wa_mensagem =  'pais não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

      ENDIF.

      "-------------------------- PUT ------------------------------

      IF me->zif_integracao_inject~at_info_request_http-ds_metodo EQ 'PUT'.

        "Uf
        IF lwa_data_inbound-uf  IS NOT INITIAL.

          SELECT  * FROM t005u
            INTO TABLE @DATA(lit_t005u_1)
             WHERE spras = @sy-langu
               "AND land1 = @lwa_data_inbound-pais
               AND bland = @lwa_data_inbound-uf.

          IF sy-subrc <> 0.
            "concatenate  'Código da região incorreto '  lwa_data_inbound-uf  'para pais'  lwa_data_inbound-pais into wa_mensagem.
            wa_mensagem = 'uf não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.

          ENDIF.
        ENDIF.

        "Cep
        IF lwa_data_inbound-cep  IS INITIAL.
          wa_mensagem =  'cep deve ser informado.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.

          DATA(length3) = strlen( lwa_data_inbound-cep ).
          IF length3 <> 9.
            "CONCATENATE 'Código Postal'   lwa_data_inbound-cep  'deve ter comprimento 9' into wa_mensagem .
            wa_mensagem = 'cep  deve estar no formato XXXXX-XXX.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ELSE.
            CLEAR wa_mensagem.
            IF lwa_data_inbound-cep+5(1) <> '-'.
              wa_mensagem = 'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.

            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = lwa_data_inbound-cep(5)
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              wa_mensagem =  'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = lwa_data_inbound-cep+6(3)
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              wa_mensagem =  'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.

            IF wa_mensagem IS NOT INITIAL.
              APPEND wa_mensagem TO <fs_erro>-erros.
              l_erro = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.

        "Pais
        IF lwa_data_inbound-pais  IS NOT INITIAL.
          "validar se está cadastrado nno SAP T005-LAND1
          SELECT SINGLE * FROM t005
            INTO @DATA(t_kna1)
            WHERE land1 = @lwa_data_inbound-pais.
          IF sy-subrc IS NOT INITIAL.
            wa_mensagem =  'pais não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.

        ENDIF.

        "ID Parceiro SAP
        IF lwa_data_inbound-idpartnersap IS INITIAL.
          wa_mensagem =  'idPartnerSap deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          "valida se tem no SAP
          SELECT SINGLE * FROM kna1
            INTO @DATA(t_kna11)
            WHERE kunnr = @lwa_data_inbound-idpartnersap.
          IF sy-subrc IS NOT INITIAL.
            wa_mensagem = 'idPartnerSap não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

      ENDIF.

*      IF wa_mensagem IS NOT INITIAL.
*        l_erro = 'X'.
*      ENDIF.


**** Código do cliente que chegou já existe.
**      IF lwa_data_inbound-idpartnersap IS NOT INITIAL.
**
**        SELECT SINGLE *
**          FROM kna1
**          INTO @DATA(lit_kna1)
**          WHERE kunnr EQ @lwa_data_inbound-idpartnersap .
**
**        IF sy-subrc = 0.
**          CONCATENATE 'Codigo cliente incorreto' lwa_data_inbound-idpartnersap
**                              INTO r_msg_erro  SEPARATED BY space.
**
**          w_tx-id_integracao     = i_id_integracao.
**          w_tx-id_origem           = lwa_data_inbound-idorigem.
**          w_tx-origem_cadastro   = 'EC'.
**          w_tx-msg_processamento = r_msg_erro.
**          w_tx-status_proc       = 'E'.
**          w_tx-id_cli_processado = ''.
**
**          MODIFY zsdt0327tx FROM w_tx.
**          CLEAR: w_tx.
**          COMMIT WORK.
**
**        ENDIF.
**      ENDIF.

*
    ENDLOOP.


    IF l_erro IS NOT INITIAL.
      r_msg_erro = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response_erro ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    r_if_integracao_inject = me.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    r_if_integracao_inject = me.

    SELECT  *
      FROM zsdt0327tx
      INTO TABLE @DATA(lit_zsdt0327tx)
      WHERE id_integracao =  @i_id_integracao
       AND status_proc = 'S' .

*** Classe que devolve o código do cliente:
    CHECK sy-subrc = 0.
    LOOP AT lit_zsdt0327tx INTO DATA(lwa_zsdt0327tx).
      CASE lwa_zsdt0327tx-origem_cadastro.
        WHEN 'EC'. "Ecommerce

          zcl_int_ob_cliente=>zif_int_ob_cliente~get_instance(
             )->set_cliente_confirmar( EXPORTING
                i_id_cliente   = lwa_zsdt0327tx-id_cli_processado
                i_id_origem    = lwa_zsdt0327tx-id_origem ) .

          e_sucesso =  abap_true.

        WHEN 'SF'. "Softexpert
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

    DATA: lit_data_inbound TYPE zsdt0325_inbcli_t,
          l_tipo_msg       TYPE bapi_mtype,
          l_mesg           TYPE string.

    DATA: wa_proc_ret    TYPE zsdt0318.

    DATA: wa_regcity TYPE j_1btreg_city,
          it_regcity LIKE STANDARD TABLE OF wa_regcity.

    DATA: lva_country    TYPE j_1btreg_city-country,
          lva_region     TYPE j_1btreg_city-region,
          lva_pstcd_from TYPE j_1btreg_city-pstcd_from,
          lva_pstcd_to   TYPE j_1btreg_city-pstcd_to,
          lva_erro(1)    TYPE c.

    DATA: wa_tx           TYPE zsdt0327tx.

    DATA: lc_msg_ret_canc TYPE string,
          lc_data_view    TYPE string,
          lc_erro         TYPE char1.


    DATA: lva_kna1       TYPE kna1,
          lva_duplic_flg TYPE c,
          lva_up_cpf     TYPE c,
          lva_kunnret    TYPE kna1-kunnr,
          lva_metodo     TYPE zintegracao-ds_metodo,
          lc_texto       TYPE string.


    DATA: e_json TYPE string.

    FIELD-SYMBOLS <icone>    LIKE icon_checked.

    DATA: lva_type TYPE dd01v-datatype.

*----------------------------------------------------------------------*
* Declaração para função SD_CUSTOMER_MAINTAIN_ALL
*----------------------------------------------------------------------*
    DATA: vg_kna1       TYPE kna1,
          vg_knb1       TYPE knb1,
          vg_knvv       TYPE knvv,
          vg_bapiaddr1  TYPE bapiaddr1,
          vg_bapiaddr2  TYPE bapiaddr2,
          vg_kunnr      TYPE kna1-kunnr,
          vg_return     TYPE kna1-kunnr,
          vg_tabix      TYPE sy-tabix,
          vg_duplic_flg TYPE c,
          vg_kunnret    TYPE kna1-kunnr,
          vg_integra    TYPE zintegracao.

    DATA: it_knvi TYPE TABLE OF fknvi,
          wa_knvi TYPE knvi.

    DATA: vg_message(220)  TYPE c,
          vg_mess_tab(256) TYPE c.

    r_if_integracao_inject = me.

    CHECK i_msg IS NOT INITIAL.

    /ui2/cl_json=>deserialize( EXPORTING json = i_msg CHANGING data = lit_data_inbound ).

    CLEAR: lva_erro.

    IF lit_data_inbound[] IS NOT INITIAL.

      LOOP AT  lit_data_inbound INTO DATA(w_data_inbound).

        CLEAR: vg_integra, lva_metodo.
        SELECT SINGLE *
          FROM zintegracao
        INTO CORRESPONDING FIELDS OF vg_integra
          WHERE id_integracao EQ i_id_integracao
             AND id_interface = '156'.
        IF sy-subrc = 0.
          lva_metodo = vg_integra-ds_metodo.
        ELSE.

          wa_tx-id_integracao     = i_id_integracao.
          wa_tx-id_origem         = w_data_inbound-idorigem.
          wa_tx-origem_cadastro   = 'EC'.
          wa_tx-msg_processamento = 'Método Integração não encontrado'.
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.

          MODIFY zsdt0327tx FROM wa_tx.
          CLEAR: wa_tx.
          COMMIT WORK.
          lva_erro = 'X'.
          CONTINUE.

        ENDIF.

* VALIDAR CPF / RG / INSCRICAO COM CARACTERES ESPECIAIS
        IF w_data_inbound-cpf IS NOT  INITIAL.
          IF w_data_inbound-cpf CA '/.,:;"[]\{}|<>?/'.
            CONCATENATE 'Caractere Especial não é permitido para o CPF:'  w_data_inbound-cpf
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-cpf
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              CONCATENATE 'String não permitido no campo CPF:'  w_data_inbound-cpf
                 INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.

          ENDIF.
        ENDIF.

        IF  w_data_inbound-cnpj IS NOT INITIAL.
          IF w_data_inbound-cnpj  CA '/.,:;"[]\{}|<>?/'.
            CONCATENATE 'Caractere Especial não é permitido para o CNPJ:'  w_data_inbound-cnpj
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-cnpj
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.


              CONCATENATE 'String não permitido no campo CNPJ:'  w_data_inbound-cnpj
                 INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        IF w_data_inbound-rg IS NOT INITIAL.
          IF w_data_inbound-rg  CA '/.,:;"[]\{}|<>?/'.
            CONCATENATE 'Caractere Especial não é permitido para o RG:'  w_data_inbound-rg
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-rg
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.

              CONCATENATE 'String não permitido no campo RG:'  w_data_inbound-rg
                 INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        IF w_data_inbound-inscestadual IS NOT INITIAL.

          IF w_data_inbound-inscestadual CA '/.,:;"[]\{}|<>?/'.

            CONCATENATE 'Caractere Especial não é permitido para Insc.Estadual:' w_data_inbound-inscestadual
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-inscestadual
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              CONCATENATE 'String não permitido no campo Insc.Estadual:'  w_data_inbound-inscestadual
               INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        IF w_data_inbound-inscmunicipal IS NOT INITIAL.
          IF w_data_inbound-inscmunicipal  CA '/.,:;"[]\{}|<>?/'.
            CONCATENATE 'Caractere Especial não é permitido para Insc.Municipal:'  w_data_inbound-inscmunicipal
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-inscmunicipal
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.

              CONCATENATE 'String não permitido no campo Insc.Municipal:' w_data_inbound-inscmunicipal
               INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

* Validar CEP
        IF    w_data_inbound-cep IS NOT INITIAL.

          DATA(length) = strlen( w_data_inbound-cep ).
          IF length <> 9.
            CONCATENATE 'Código Postal' w_data_inbound-cep 'deve ter comprimento 9'
               INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.

          ENDIF.
        ENDIF.

* Validar PAIS
        IF w_data_inbound-pais IS NOT INITIAL.
          SELECT * FROM t005t
            INTO TABLE @DATA(lit_t005)
             WHERE spras EQ @sy-langu
               AND land1 EQ @w_data_inbound-pais.

          IF sy-subrc <> 0.
            CONCATENATE 'Código do Pais incorreto' w_data_inbound-pais
              INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ENDIF.
        ENDIF.
* Validar UF
        IF w_data_inbound-uf IS NOT INITIAL.

          SELECT  * FROM t005u
            INTO TABLE @DATA(lit_t005u)
             WHERE spras = @sy-langu
               AND land1 = @w_data_inbound-pais
               AND bland = @w_data_inbound-uf.

          IF sy-subrc <> 0.
            CONCATENATE 'Código da região incorreto ' w_data_inbound-uf 'para pais' w_data_inbound-pais
              INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ENDIF.
        ENDIF.

*--------------------------------------
*-- INSERIR
*--------------------------------------

        IF lva_metodo = 'POST'.

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida CPF / CNJ / Inscrição Estadual
*-----------------------------------------------------------------------------------------------------------------------*
          CLEAR: lva_kna1, lva_duplic_flg,lva_kunnret,  lva_up_cpf.

          lva_kna1-kunnr = w_data_inbound-idpartnersap.
          lva_kna1-stcd1 = w_data_inbound-cnpj.
          lva_kna1-stcd2 = w_data_inbound-cpf.
          lva_kna1-stcd3 = w_data_inbound-inscestadual.
          lva_kna1-ktokd = w_data_inbound-grupoconta.
          lva_kna1-stcd4 = w_data_inbound-inscmunicipal.

          CONCATENATE w_data_inbound-rg '-' w_data_inbound-orgexp '/' w_data_inbound-ufexp INTO DATA(lva_rg).
          CONDENSE lva_rg NO-GAPS.
          lva_kna1-stcd4 = lva_rg.


*  Não deixar duplicar cpf
          IF ( 'ZCNF ZCFU ZCFF' CS lva_kna1-ktokd ).
            PERFORM f_consistir_cpf IN PROGRAM saplxf04 USING lva_kna1-kunnr
                                                    lva_kna1-stcd2
                                           CHANGING lva_duplic_flg
                                                    lva_kunnret.

          ELSEIF ( 'ZCNJ ZCPJ ZCIC ZCFJ' CS lva_kna1-ktokd ).
*  Não deixar duplicar CNPJ
            PERFORM f_consistir_cnpj IN PROGRAM saplxf04 USING lva_kna1-kunnr
                                                     lva_kna1-stcd1
                                            CHANGING lva_duplic_flg
                                                     lva_kunnret.
          ELSEIF ( lva_kna1-ktokd EQ 'ZCPF' ).
*  Não deixar duplicar CPF + Inscrição estadual juntos
            PERFORM f_consistir_cpf_insc IN PROGRAM saplxf04 USING lva_kna1-kunnr
                                                         lva_kna1-stcd2
                                                         lva_kna1-stcd3
                                                CHANGING lva_duplic_flg
                                                         lva_kunnret.
          ENDIF.

          IF ( NOT lva_duplic_flg IS INITIAL ).

            lva_up_cpf = 'X'.

            CONCATENATE 'Duplicidade de CNPJ ou CPF com o cliente' lva_kunnret
                                  INTO vg_message SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-id_origem         =  w_data_inbound-idorigem.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'S'.
            wa_tx-id_cli_processado = lva_kunnret.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.

          ELSE.

            CLEAR: lva_country,
                   lva_region  ,
                   lva_pstcd_from,
                   lva_pstcd_to.

            lva_country      = w_data_inbound-pais.
            lva_region       = w_data_inbound-uf.
            lva_pstcd_from   = w_data_inbound-cep.
            lva_pstcd_to     = w_data_inbound-cep.

            CLEAR: it_regcity.
            SELECT * FROM j_1btreg_city
                   INTO TABLE it_regcity
                  WHERE ( country    EQ  lva_country  )
                    AND ( region     EQ  lva_region   )
                    AND ( pstcd_from LE  lva_pstcd_from )
                    AND ( pstcd_to   GE  lva_pstcd_to   ).

***  VERIFICO SE É PESSOA FÍSICA OU JURIDICA.
            CLEAR: vg_bapiaddr1,
                   vg_bapiaddr2,
                   vg_kna1.

            IF w_data_inbound-grupoconta = 'ZCPF'." - CLIENTE PRODUTOR PESSOA FISICA
              vg_kna1-stkzn = 'X'.
              IF w_data_inbound-genero = 'MASCULINO'.
                vg_bapiaddr1-formofaddr = '0002'.
                vg_bapiaddr2-title_p = '0002'.
              ELSE.
                vg_bapiaddr1-formofaddr = '0001'.
                vg_bapiaddr2-title_p = '0001'.
              ENDIF.
            ELSE.
              IF w_data_inbound-grupoconta = 'ZCPJ'. " - CLIENTE PRODUTOR PESSOA JURIDICA
                vg_kna1-stkzn = ''.
                vg_bapiaddr1-formofaddr = '0003'.
                vg_bapiaddr2-title_p = '0003'.
              ELSE.
                vg_bapiaddr1-formofaddr = ''.
                vg_bapiaddr2-title_p = ''.
              ENDIF.
            ENDIF.

* PARAMETRIZAÇÃO

            SELECT * FROM zsdt0317
              INTO TABLE @DATA(it_zsdt0317)
              WHERE cancelado <> 'X'
               AND ktokd = @w_data_inbound-grupoconta
               AND bukrs = @w_data_inbound-empresa.

            SELECT * FROM zsdt0319
              INTO TABLE @DATA(it_zsdt0319)
              FOR ALL ENTRIES IN @it_zsdt0317
            WHERE cancelado <> 'X'
              AND id = @it_zsdt0317-id .

            SELECT * FROM zsdt0320
              INTO TABLE @DATA(it_zsdt0320)
              FOR ALL ENTRIES IN @it_zsdt0319
              WHERE id =  @it_zsdt0319-id
               AND seq_canal = @it_zsdt0319-seq_canal
               AND cancelado <> 'X'.

            SELECT * FROM zsdt0322
              INTO TABLE @DATA(it_zsdt0322)
              FOR ALL ENTRIES IN @it_zsdt0319
              WHERE id =  @it_zsdt0319-id
               AND  seq_canal = @it_zsdt0319-seq_canal
               AND  cancelado <> 'X'.

* MESTRE DE CLIENTES (PARTE GERAL)
            vg_kna1-ktokd = w_data_inbound-grupoconta .
            vg_kna1-land1 = w_data_inbound-pais.
            vg_kna1-sortl = w_data_inbound-nome+0(10).
            vg_kna1-name1 = w_data_inbound-nome.
            vg_kna1-ort01 = w_data_inbound-cidade.
            vg_kna1-pstlz = w_data_inbound-cep.
            vg_kna1-regio = w_data_inbound-uf.
            vg_kna1-stras = w_data_inbound-endereco.
            vg_kna1-anred = 'PT.'.
            vg_kna1-spras = sy-langu.
            vg_kna1-erdat = sy-datum.
            vg_kna1-ernam = sy-uname.
            vg_kna1-stcd1 = w_data_inbound-cnpj.
            vg_kna1-stcd2 = w_data_inbound-cpf.
            vg_kna1-stcd3 = w_data_inbound-inscestadual.
            vg_kna1-stcd4 = w_data_inbound-inscmunicipal.
            vg_kna1-stcd4 = lva_kna1-stcd4. " w_data_inbound-inscmunicipal.
            vg_kna1-telf1 = w_data_inbound-telefonefixo.
            vg_kna1-telf2 = w_data_inbound-telefonemovel.
            CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO vg_kna1-telfx.


* MESTRE DE CLIENTES (EMPRESA)
* Cabeçalho dos Parâmetros para o Cadastro Cliente
            SORT it_zsdt0317 BY bukrs.

            LOOP AT it_zsdt0317 INTO DATA(lwa_zsdt0317).

              CLEAR: vg_knb1.
              vg_knb1-bukrs = lwa_zsdt0317-bukrs.
              vg_knb1-akont = lwa_zsdt0317-akont.
              vg_knb1-zuawa = lwa_zsdt0317-zuawa.
              vg_knb1-fdgrv = lwa_zsdt0317-fdgrv.
              vg_knb1-zterm = lwa_zsdt0317-zterm.
              vg_knb1-zwels = lwa_zsdt0317-zwels.

              LOOP AT it_zsdt0319 INTO DATA(lwa_zsdt0319) WHERE id = lwa_zsdt0317-id.

* MESTRE DE CLIENTES: DADOS DE VENDAS E DISTRIBUIÇÃO
                CLEAR: vg_knvv.
                vg_knvv-vkorg    = lwa_zsdt0317-bukrs.
                vg_knvv-vtweg    = lwa_zsdt0319-vtweg.

                LOOP AT it_zsdt0320 INTO DATA(lwa_zsdt0320) WHERE id = lwa_zsdt0319-id
                                                             AND  seq_canal = lwa_zsdt0319-seq_canal .


                  READ TABLE it_zsdt0322 INTO DATA(lwa_zsdt0322) WITH KEY id = lwa_zsdt0319-id
                                                                   seq_canal = lwa_zsdt0319-seq_canal.
                  vg_knvv-spart    = lwa_zsdt0320-spart.
                  vg_knvv-kalks    = lwa_zsdt0322-kalks.
                  vg_knvv-kdgrp    = '01'.
                  vg_knvv-waers    = lwa_zsdt0322-waers.
                  vg_knvv-ktgrd    = lwa_zsdt0322-ktgrd.
                  vg_knvv-versg    = lwa_zsdt0322-versg.
                  vg_knvv-lprio    = lwa_zsdt0322-lprio.
                  vg_knvv-vsbed    = lwa_zsdt0322-vsbed.
                  vg_knvv-kzazu    = lwa_zsdt0322-kzazu.
                  vg_knvv-kztlf    = lwa_zsdt0322-kztlf.
                  vg_knvv-perfk    = lwa_zsdt0322-perfk.

* MESTRE DE CLIENTES - INDICADORES DE IMPOSTOS
                  CLEAR: it_knvi[], wa_knvi.
                  IF vg_kunnr IS NOT INITIAL.
                    wa_knvi-kunnr = vg_kunnr.
                  ENDIF.

                  wa_knvi-aland = 'BR'.               "tax country
                  wa_knvi-tatyp = lwa_zsdt0322-tatyp. "tax category
                  wa_knvi-taxkd = lwa_zsdt0322-taxkd. "tax classification
                  APPEND wa_knvi TO it_knvi.
                  CLEAR:  wa_knvi.

* ESTRUTURA DE REFERÊNCIA BAPI PARA ENDEREÇOS (ORG./FIRMA)
                  vg_bapiaddr1-sort1      = w_data_inbound-nome+0(10).
                  vg_bapiaddr1-name       = w_data_inbound-nome.
                  vg_bapiaddr1-city       = w_data_inbound-cidade.
                  vg_bapiaddr1-postl_cod1 = w_data_inbound-cep.
                  vg_bapiaddr1-street     = w_data_inbound-endereco.
                  vg_bapiaddr1-house_no   = w_data_inbound-nrendereco.
                  vg_bapiaddr1-country    = w_data_inbound-pais.
                  vg_bapiaddr1-langu      = sy-langu.
                  vg_bapiaddr1-region     = w_data_inbound-uf.
                  vg_bapiaddr1-tel1_numbr = w_data_inbound-telefonefixo.
                  vg_bapiaddr1-district   = w_data_inbound-bairro.
                  CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO vg_bapiaddr1-fax_number.

                  LOOP AT it_regcity INTO wa_regcity
                                       WHERE ( country    EQ lva_country        )
                                         AND ( region     EQ lva_region         )
                                         AND ( pstcd_from LE lva_pstcd_from     )
                                         AND ( pstcd_to   GE lva_pstcd_to       ).

                    vg_bapiaddr1-taxjurcode = wa_regcity-taxjurcode.
                    vg_bapiaddr2-taxjurcode = wa_regcity-taxjurcode.
                    vg_kna1-locco = wa_regcity-taxjurcode.
                    EXIT.
                  ENDLOOP.

                  vg_bapiaddr2-lastname    = w_data_inbound-nome.
                  vg_bapiaddr2-fullname    = w_data_inbound-nome.
                  vg_bapiaddr2-city        = w_data_inbound-cidade.
                  vg_bapiaddr2-postl_cod1  = w_data_inbound-cep.
                  vg_bapiaddr2-street      = w_data_inbound-endereco.
                  vg_bapiaddr2-country     = w_data_inbound-pais.
                  vg_bapiaddr2-region      = w_data_inbound-uf.
                  vg_bapiaddr2-tel1_numbr  = w_data_inbound-telefonefixo.
                  vg_bapiaddr2-e_mail      = w_data_inbound-email.
                  vg_bapiaddr2-langu_p     = sy-langu.
                  vg_bapiaddr2-district    = w_data_inbound-bairro.
                  vg_bapiaddr2-house_no   = w_data_inbound-nrendereco.
                  CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO vg_bapiaddr2-fax_number.

                  vg_kna1-kunnr = vg_kunnr.
                  vg_knb1-kunnr = vg_kunnr.
                  vg_knb1-bukrs = lwa_zsdt0317-bukrs.

                  IF vg_kunnr IS NOT INITIAL.
                    vg_knb1-kunnr = vg_kunnr.
                    vg_knvv-kunnr = vg_kunnr.

                    SELECT SINGLE *
                      FROM kna1
                      INTO CORRESPONDING FIELDS OF vg_kna1
                      WHERE kunnr EQ vg_kunnr.

                    CLEAR: vg_bapiaddr1,
                           vg_bapiaddr2.
                  ENDIF.

                  CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
                    EXPORTING
                      i_kna1                     = vg_kna1
                      i_knb1                     = vg_knb1
                      i_knvv                     = vg_knvv
                      i_bapiaddr1                = vg_bapiaddr1
                      i_bapiaddr2                = vg_bapiaddr2
                      i_maintain_address_by_kna1 = 'X'
                      pi_postflag                = 'X'
                    IMPORTING
                      e_kunnr                    = vg_return
                    TABLES
                      t_xknvi                    = it_knvi
                    EXCEPTIONS
                      client_error               = 1
                      kna1_incomplete            = 2
                      knb1_incomplete            = 3
                      knb5_incomplete            = 4
                      knvv_incomplete            = 5
                      kunnr_not_unique           = 6
                      sales_area_not_unique      = 7
                      sales_area_not_valid       = 8
                      insert_update_conflict     = 9
                      number_assignment_error    = 10
                      number_not_in_range        = 11
                      number_range_not_extern    = 12
                      number_range_not_intern    = 13
                      account_group_not_valid    = 14
                      parnr_invalid              = 15
                      bank_address_invalid       = 16
                      tax_data_not_valid         = 17
                      no_authority               = 18
                      company_code_not_unique    = 19
                      dunning_data_not_valid     = 20
                      knb1_reference_invalid     = 21
                      cam_error                  = 22
                      OTHERS                     = 23.

                  IF sy-subrc <> 0.
                    CASE sy-subrc.
                      WHEN  1. vg_message = 'Erro nos dados do cliente'.
                      WHEN  2. vg_message = 'Dados da estrutura kna1 incompletos!'.
                      WHEN  3. vg_message = 'Dados da estrutura knb1 incompletos!'.
                      WHEN  4. vg_message = 'Dados da estrutura knb5 incompletos!'.
                      WHEN  5. vg_message = 'Dados da estrutura knvv incompletos!'.
                      WHEN  6. vg_message = 'Cliente já cadastrado'.
                      WHEN  7. vg_message = 'Área de vendas já cadastrada'.
                      WHEN  8. vg_message = 'Dados da área de vendas inválidos!'.
                      WHEN  9. vg_message = 'Erro durante o insert ou update dos dados!'.
                      WHEN 10. vg_message = 'Erro na atribuição do ID cliente!'.
                      WHEN 11. vg_message = 'ID cliente fora do range estipulado!'.
                      WHEN 12. vg_message = 'Range para ID Cliente não é externo!'.
                      WHEN 13. vg_message = 'Range para ID Cliente não é interno!'.
                      WHEN 14. vg_message = 'Grupo de contas inválido!'.
                      WHEN 15. vg_message = 'Parceiro de negócio inválido!'.
                      WHEN 16. vg_message = 'Dados de banco inválido !'.
                      WHEN 17. vg_message = 'Dados de Classificação Fisc inválidos!'.
                      WHEN 18. vg_message = 'Sem autorização!'.
                      WHEN 19. vg_message = 'Empresa já atribuida!'.
                      WHEN 20. vg_message = 'Dados inválidos!'.
                      WHEN 21. vg_message = 'Dados de referência knb1 inválidos!'.
                      WHEN 22. vg_message = 'CAM Error!'.
                      WHEN 23. vg_message = 'Erro desconhecido!'.
                    ENDCASE.

                    "e_sucesso      = abap_false.
                    lva_erro = 'X'.

                    CONCATENATE 'Cliente não criado pelo motivo:!' vg_message  vg_return
                                 INTO vg_message SEPARATED BY space.

                    wa_tx-id_integracao     = i_id_integracao.
                    wa_tx-origem_cadastro   = 'EC'.
                    wa_tx-id_origem          =  w_data_inbound-idorigem.
                    wa_tx-msg_processamento = vg_message.
                    wa_tx-status_proc       = 'E'.
                    wa_tx-id_cli_processado = vg_kunnr.

                    MODIFY zsdt0327tx FROM wa_tx.
                    CLEAR: wa_tx.
                    COMMIT WORK.

                  ELSE.

                    COMMIT WORK AND WAIT.
                    WAIT UP TO 1 SECONDS.

                    IF  vg_kunnr IS INITIAL.

                      vg_kunnr = vg_return.
                      w_data_inbound-idpartnersap = vg_kunnr.

** Salvar celular
                      DATA: lit_tel  TYPE TABLE OF bapiadtel,
                            lwa_tel  TYPE bapiadtel,
                            lit_telx TYPE TABLE OF bapiadtelx,
                            lwa_telx TYPE bapiadtelx.

                      DATA: bapi_kunnr TYPE bapi4001_1-objkey.

                      lwa_tel-telephone = w_data_inbound-telefonemovel.
                      lwa_tel-r_3_user = 3. "Default Mobile..Use this to mention different types of telf2
                      lwa_telx-updateflag = 'I'.
                      APPEND lwa_tel TO lit_tel.
                      APPEND lwa_telx TO lit_telx.

                      bapi_kunnr =  vg_kunnr.

** Salvar email auxiliar.
                      DATA: i_bapiadsmtp  TYPE TABLE OF bapiadsmtp,
                            lwa_1         LIKE LINE OF i_bapiadsmtp,
                            i_bapiadsmt_x TYPE TABLE OF bapiadsmtx,
                            lwa_2         LIKE LINE OF i_bapiadsmt_x.


                      "lwa_1-std_no = 'X'.
                      lwa_1-e_mail = w_data_inbound-emailaux.
                      APPEND lwa_1 TO i_bapiadsmtp.
                      lwa_2-updateflag = 'I'.
                      APPEND lwa_2 TO i_bapiadsmt_x.



                      CALL FUNCTION 'BAPI_ADDRESSORG_CHANGE'
                        EXPORTING
                          obj_type     = 'KNA1'
                          obj_id       = bapi_kunnr
                          save_address = 'X'
                        TABLES
                          bapiadtel    = lit_tel
                          bapiadtel_x  = lit_telx
                          bapiadsmtp   = i_bapiadsmtp
                          bapiadsmt_x  = i_bapiadsmt_x.


                      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                        EXPORTING
                          wait = 'X'.

                    ENDIF.

                    "e_sucesso      = abap_true.

*** Stefanini - IR201659 - 16/10/2024 - LAZAROSR - Início de Alteração
                    IF vg_kunnr IS NOT INITIAL.
*** Stefanini - IR201659 - 16/10/2024 - LAZAROSR - Fim de Alteração

                      CONCATENATE 'Cliente' vg_kunnr 'criado com sucesso!'
                                            INTO vg_message SEPARATED BY space.

                      wa_tx-id_integracao     = i_id_integracao.
                      wa_tx-origem_cadastro   = 'EC'.
                      wa_tx-id_origem          =  w_data_inbound-idorigem.
                      wa_tx-msg_processamento = vg_message.
                      wa_tx-status_proc       = 'S'.
                      wa_tx-id_cli_processado = vg_kunnr.

*** Stefanini - IR201659 - 16/10/2024 - LAZAROSR - Início de Alteração
                    ELSE.

                      lva_erro = 'X'.

                      wa_tx-id_integracao     = i_id_integracao.
                      wa_tx-origem_cadastro   = 'EC'.
                      wa_tx-id_origem          = w_data_inbound-idorigem.
                      wa_tx-msg_processamento = 'Não foi possível criar o cliente.'.
                      wa_tx-status_proc       = 'E'.
                      wa_tx-id_cli_processado = vg_kunnr.

                    ENDIF.
*** Stefanini - IR201659 - 16/10/2024 - LAZAROSR - Fim de Alteração

                    MODIFY zsdt0327tx FROM wa_tx.
                    CLEAR: wa_tx.
                    COMMIT WORK.
                  ENDIF.
                ENDLOOP.
              ENDLOOP.
            ENDLOOP.
          ENDIF.
        ENDIF.

*** UPDATE
        IF ( lva_metodo = 'PUT' ) OR ( lva_up_cpf = 'X' ) .
          CLEAR:  bapi_kunnr.

          IF lva_metodo = 'PUT'.
            bapi_kunnr =  w_data_inbound-idpartnersap.
          ELSE.
            bapi_kunnr =  lva_kunnret.
          ENDIF.

          IF bapi_kunnr IS INITIAL.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = 'UPDATE - Código cliente não encontrado'.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.

          ELSE.

            CLEAR: vg_kna1.
            SELECT SINGLE *
              FROM kna1
              INTO CORRESPONDING FIELDS OF vg_kna1
              WHERE kunnr EQ bapi_kunnr.


            CLEAR: vg_bapiaddr1,
                   vg_bapiaddr2.

            IF w_data_inbound-grupoconta = 'ZCPF'." - CLIENTE PRODUTOR PESSOA FISICA
              vg_kna1-stkzn = 'X'.
              IF w_data_inbound-genero = 'MASCULINO'.
                vg_bapiaddr1-formofaddr = '0002'.
                vg_bapiaddr2-title_p = '0002'.
              ELSE.
                vg_bapiaddr1-formofaddr = '0001'.
                vg_bapiaddr2-title_p = '0001'.
              ENDIF.
            ELSE.
              IF w_data_inbound-grupoconta = 'ZCPJ'. " - CLIENTE PRODUTOR PESSOA JURIDICA
                vg_kna1-stkzn = ''.
                vg_bapiaddr1-formofaddr = '0003'.
                vg_bapiaddr2-title_p = '0003'.
              ELSE.
                "Erro"
              ENDIF.
            ENDIF.

*** Enviar mensagem avisando que é diferente o grupo de conta
            IF w_data_inbound-grupoconta <> vg_kna1-ktokd.
              CONCATENATE 'Cliente com grupo de conta diferente:!' w_data_inbound-grupoconta '/' vg_kna1-ktokd
                             INTO vg_message SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-id_origem          =  w_data_inbound-idorigem.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = vg_kunnr.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.

            ENDIF.

            SELECT SINGLE * FROM kna1 INTO vg_kna1 WHERE kunnr EQ w_data_inbound-idpartnersap.



            vg_kna1-name1   = w_data_inbound-nome.
            vg_kna1-ort02   = w_data_inbound-cidade.
            vg_kna1-pstlz   = w_data_inbound-cep.
            vg_kna1-regio   = w_data_inbound-uf.
            vg_kna1-stras   = w_data_inbound-endereco.
            vg_kna1-erdat   = sy-datum.
            vg_kna1-ernam   = sy-uname.
            vg_kna1-telf1 = w_data_inbound-telefonefixo.
            vg_kna1-telf2 = w_data_inbound-telefonemovel.
            CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO vg_kna1-telfx.


* ESTRUTURA DE REFERÊNCIA BAPI PARA ENDEREÇOS (ORG./FIRMA)
*            vg_bapiaddr1-sort1       = w_data_inbound-nome+0(10).
*            vg_bapiaddr1-name        = w_data_inbound-nome.
*            vg_bapiaddr1-city        = w_data_inbound-cidade.
*            vg_bapiaddr1-postl_cod1  = w_data_inbound-cep.
*            vg_bapiaddr1-street      = w_data_inbound-endereco.
*            vg_bapiaddr1-house_no    = w_data_inbound-nrendereco.
*            vg_bapiaddr1-country     = w_data_inbound-pais.
*            vg_bapiaddr1-region      = w_data_inbound-uf.
*            vg_bapiaddr1-tel1_numbr  = w_data_inbound-telefonefixo.
*            vg_bapiaddr1-district    = w_data_inbound-bairro.
*
*
*            vg_bapiaddr2-lastname    = w_data_inbound-nome.
*            vg_bapiaddr2-fullname    = w_data_inbound-nome.
*            vg_bapiaddr2-city        = w_data_inbound-cidade.
*            vg_bapiaddr2-postl_cod1  = w_data_inbound-cep.
*            vg_bapiaddr2-street      = w_data_inbound-endereco.
*            vg_bapiaddr2-country     = w_data_inbound-pais.
*            vg_bapiaddr2-region      = w_data_inbound-uf.
*            vg_bapiaddr2-tel1_numbr  = w_data_inbound-telefonefixo.
*            vg_bapiaddr2-e_mail      = w_data_inbound-email.
*            vg_bapiaddr2-langu_p     = sy-langu.
*            vg_bapiaddr2-district    = w_data_inbound-bairro.
*            vg_bapiaddr2-house_no   = w_data_inbound-nrendereco.

            SELECT SINGLE * FROM adrc INTO CORRESPONDING FIELDS OF vg_bapiaddr1 WHERE addrnumber = vg_kna1-adrnr AND date_to >= sy-datum.

            vg_bapiaddr1-sort1      = w_data_inbound-nome+0(10).
            vg_bapiaddr1-name       = w_data_inbound-nome.
            vg_bapiaddr1-city       = w_data_inbound-cidade.
            vg_bapiaddr1-postl_cod1 = w_data_inbound-cep.
            vg_bapiaddr1-street     = w_data_inbound-endereco.
            vg_bapiaddr1-house_no   = w_data_inbound-nrendereco.
            vg_bapiaddr1-country    = w_data_inbound-pais.
            vg_bapiaddr1-langu      = sy-langu.
            vg_bapiaddr1-region     = w_data_inbound-uf.
            vg_bapiaddr1-tel1_numbr = w_data_inbound-telefonefixo.
            vg_bapiaddr1-district   = w_data_inbound-bairro.
            CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO vg_bapiaddr1-fax_number.

            SELECT SINGLE * FROM adrc INTO CORRESPONDING FIELDS OF vg_bapiaddr2 WHERE addrnumber = vg_kna1-adrnr AND date_to >= sy-datum.

            CLEAR: lva_country,
                              lva_region  ,
                              lva_pstcd_from,
                              lva_pstcd_to.

            lva_country      = w_data_inbound-pais.
            lva_region       = w_data_inbound-uf.
            lva_pstcd_from   = w_data_inbound-cep.
            lva_pstcd_to     = w_data_inbound-cep.

            CLEAR: it_regcity.
            SELECT * FROM j_1btreg_city
                   INTO TABLE it_regcity
                  WHERE ( country    EQ  lva_country  )
                    AND ( region     EQ  lva_region   )
                    AND ( pstcd_from LE  lva_pstcd_from )
                    AND ( pstcd_to   GE  lva_pstcd_to   ).

            LOOP AT it_regcity INTO wa_regcity
                                 WHERE ( country    EQ lva_country        )
                                   AND ( region     EQ lva_region         )
                                   AND ( pstcd_from LE lva_pstcd_from     )
                                   AND ( pstcd_to   GE lva_pstcd_to       ).

              vg_bapiaddr1-taxjurcode = wa_regcity-taxjurcode.
              vg_bapiaddr2-taxjurcode = wa_regcity-taxjurcode.
              vg_kna1-locco = wa_regcity-taxjurcode.
              EXIT.
            ENDLOOP.

            vg_bapiaddr2-lastname    = w_data_inbound-nome.
            vg_bapiaddr2-fullname    = w_data_inbound-nome.
            vg_bapiaddr2-city        = w_data_inbound-cidade.
            vg_bapiaddr2-postl_cod1  = w_data_inbound-cep.
            vg_bapiaddr2-street      = w_data_inbound-endereco.
            vg_bapiaddr2-country     = w_data_inbound-pais.
            vg_bapiaddr2-region      = w_data_inbound-uf.
            vg_bapiaddr2-tel1_numbr  = w_data_inbound-telefonefixo.
            vg_bapiaddr2-e_mail      = w_data_inbound-email.
            vg_bapiaddr2-langu_p     = sy-langu.
            vg_bapiaddr2-district    = w_data_inbound-bairro.
            vg_bapiaddr2-house_no   = w_data_inbound-nrendereco.
            CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO vg_bapiaddr2-fax_number.

            "vg_kna1-kunnr = vg_kunnr.
            "vg_knb1-kunnr = vg_kunnr.
            "vg_knb1-bukrs = lwa_zsdt0317-bukrs.

            "IF vg_kunnr IS NOT INITIAL.
            "vg_knb1-kunnr = vg_kunnr.
            "vg_knvv-kunnr = vg_kunnr.

            "SELECT SINGLE *
            "  FROM kna1
            "  INTO CORRESPONDING FIELDS OF vg_kna1
            "  WHERE kunnr EQ vg_kunnr.

            "CLEAR: vg_bapiaddr1,
            "        vg_bapiaddr2.
            " ENDIF.


            CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
              EXPORTING
                i_kna1                     = vg_kna1
                i_bapiaddr1                = vg_bapiaddr1
                i_bapiaddr2                = vg_bapiaddr2
                i_maintain_address_by_kna1 = 'X'
                pi_postflag                = 'X'
              IMPORTING
                e_kunnr                    = vg_return
              EXCEPTIONS
                client_error               = 1
                kna1_incomplete            = 2
                knb1_incomplete            = 3
                knb5_incomplete            = 4
                knvv_incomplete            = 5
                kunnr_not_unique           = 6
                sales_area_not_unique      = 7
                sales_area_not_valid       = 8
                insert_update_conflict     = 9
                number_assignment_error    = 10
                number_not_in_range        = 11
                number_range_not_extern    = 12
                number_range_not_intern    = 13
                account_group_not_valid    = 14
                parnr_invalid              = 15
                bank_address_invalid       = 16
                tax_data_not_valid         = 17
                no_authority               = 18
                company_code_not_unique    = 19
                dunning_data_not_valid     = 20
                knb1_reference_invalid     = 21
                cam_error                  = 22
                OTHERS                     = 23.

            IF sy-subrc <> 0.
              CASE sy-subrc.
                WHEN  1. vg_message = 'Erro nos dados do cliente'.
                WHEN  2. vg_message = 'Dados da estrutura kna1 incompletos!'.
                WHEN  3. vg_message = 'Dados da estrutura knb1 incompletos!'.
                WHEN  4. vg_message = 'Dados da estrutura knb5 incompletos!'.
                WHEN  5. vg_message = 'Dados da estrutura knvv incompletos!'.
                WHEN  6. vg_message = 'Cliente já cadastrado'.
                WHEN  7. vg_message = 'Área de vendas já cadastrada'.
                WHEN  8. vg_message = 'Dados da área de vendas inválidos!'.
                WHEN  9.
                  vg_message =
                            'Erro durante o insert ou update dos dados!'.
                WHEN 10. vg_message = 'Erro na atribuição do ID cliente!'.
                WHEN 11. vg_message = 'ID cliente fora do range estipulado!'.
                WHEN 12. vg_message = 'Range para ID Cliente não é externo!'.
                WHEN 13. vg_message = 'Range para ID Cliente não é interno!'.
                WHEN 14. vg_message = 'Grupo de contas inválido!'.
                WHEN 15. vg_message = 'Parceiro de negócio inválido!'.
                WHEN 16. vg_message = 'Dados de banco inválido !'.
                WHEN 17. vg_message = 'Dados de Classificação Fisc inválidos!'.
                WHEN 18. vg_message = 'Sem autorização!'.
                WHEN 19. vg_message = 'Empresa já atribuida!'.
                WHEN 20. vg_message = 'Dados inválidos!'.
                WHEN 21. vg_message = 'Dados de referência knb1 inválidos!'.
                WHEN 22. vg_message = 'CAM Error!'.
                WHEN 23. vg_message = 'Erro desconhecido!'.
              ENDCASE.

              "e_sucesso      = abap_false.
              lva_erro = 'X'.
              CONCATENATE 'Cliente não criado pelo motivo:!' vg_message  vg_return
                             INTO vg_message SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-id_origem          =  w_data_inbound-idorigem.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = vg_kunnr.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.

            ELSE.

              COMMIT WORK AND WAIT.
              WAIT UP TO 1 SECONDS.

              CLEAR:  lit_tel[],
                      lwa_tel,
                      lit_telx[],
                      lwa_telx.


              lwa_tel-telephone = w_data_inbound-telefonemovel.
              lwa_tel-r_3_user = 3. "Default Mobile..Use this to mention different types of telf2
              lwa_telx-updateflag = 'I'.
              APPEND lwa_tel TO lit_tel.
              APPEND lwa_telx TO lit_telx.

              "lwa_1-std_no = 'X'.
              lwa_1-e_mail = w_data_inbound-emailaux.
              APPEND lwa_1 TO i_bapiadsmtp.
              lwa_2-updateflag = 'I'.
              APPEND lwa_2 TO i_bapiadsmt_x.

              CALL FUNCTION 'BAPI_ADDRESSORG_CHANGE'
                EXPORTING
                  obj_type     = 'KNA1'
                  obj_id       = bapi_kunnr
                  save_address = 'X'
                TABLES
                  bapiadtel    = lit_tel
                  bapiadtel_x  = lit_telx
                  bapiadsmtp   = i_bapiadsmtp
                  bapiadsmt_x  = i_bapiadsmt_x.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              CONCATENATE 'Cliente' bapi_kunnr 'atualizado com sucesso!'
                                    INTO vg_message SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         =  w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'S'.
              wa_tx-id_cli_processado = bapi_kunnr.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR: it_zsdt0317[], it_zsdt0319[], it_zsdt0320[], it_zsdt0322[].

      ENDLOOP.

    ELSE.
      wa_tx-id_integracao     = i_id_integracao.
      wa_tx-id_origem         = w_data_inbound-idorigem.
      wa_tx-origem_cadastro   = 'EC'.
      wa_tx-msg_processamento = 'Erro na desserialização JSON'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.
      lva_erro = 'X'.
    ENDIF.

    IF lva_erro IS NOT INITIAL.
      e_sucesso      = abap_false.
    ELSE.
      e_sucesso      = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
