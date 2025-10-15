CLASS zcl_trace_cotton DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_trace_cotton .
    INTERFACES zif_integracao_inbound .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TRACE_COTTON IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_trace_cotton.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'TRACE_COTTON'.

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
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_inbound.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA: w_zsdt0330 TYPE zsdt0330,
          w_retorno  TYPE zsdt0331,
          l_erro     TYPE char1,
          l_em_proc  TYPE char1,
          l_tabix    TYPE sy-tabix.

    DATA: lit_zsdt0330_check TYPE TABLE OF zsdt0330.

    FREE: r_msg_erro, l_em_proc, l_erro.

    IF me->zif_trace_cotton~at_info_request_http-ds_metodo NE 'POST'.
      r_msg_erro = 'Metodo informado não reconhecido!'.
      RETURN.
    ENDIF.

*----------------------------------------
*---- checa se lagum fardo ja esta sendo processado
*----------------------------------------
    READ TABLE me->zif_trace_cotton~at_zsdt0330 INTO w_zsdt0330 INDEX 1.

    SELECT *
      FROM zsdt0330 INTO TABLE lit_zsdt0330_check
     WHERE id_carga      = w_zsdt0330-id_carga
       AND cancelado     = abap_false.

    LOOP AT lit_zsdt0330_check ASSIGNING FIELD-SYMBOL(<fs_zsdt0330_check>).
      CASE <fs_zsdt0330_check>-status_estorno.
        WHEN 'D'.
          IF <fs_zsdt0330_check>-status_fardo <> '3'.
            l_em_proc = abap_true.
            EXIT.
          ENDIF.
        WHEN OTHERS.
          l_em_proc = abap_true.
          EXIT.
      ENDCASE.
    ENDLOOP.
*--------------------------------------
*-- validar
*--------------------------------------

    DATA(lit_fardos_romaneio) = me->zif_trace_cotton~at_zsdt0330[].

    LOOP AT me->zif_trace_cotton~at_zsdt0330 INTO w_zsdt0330.

      l_tabix = sy-tabix.

      MOVE-CORRESPONDING w_zsdt0330            TO w_retorno.

      IF l_em_proc = abap_true.
        w_retorno-mensagem = r_msg_erro = 'Bloco ja esta sendo Processado! Nao permite mais alteracoes.'.
        me->zif_trace_cotton~set_gravar_retorno( w_retorno ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.

      SELECT matnr
        INTO w_zsdt0330-matnr
        FROM mara
          UP TO 1 ROWS
       WHERE normt = w_zsdt0330-normt
         AND mtart = 'ZFER'.
      ENDSELECT.

      IF sy-subrc <> 0.
        w_retorno-mensagem = r_msg_erro = 'Material nao existe no SAP!'.
        me->zif_trace_cotton~set_gravar_retorno( w_retorno ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ELSE.
        w_retorno-matnr =  w_zsdt0330-matnr.
      ENDIF.

      SELECT werks
        INTO @DATA(l_werks)
        FROM t001w
          UP TO 1 ROWS
       WHERE werks = @w_zsdt0330-werks.
      ENDSELECT.

      IF sy-subrc <> 0.
        w_retorno-mensagem = r_msg_erro = 'Filial nao existe no SAP!'.
        me->zif_trace_cotton~set_gravar_retorno( w_retorno ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.

      SELECT lgort
        INTO @DATA(w_t001l)
        FROM t001l
          UP TO 1 ROWS
       WHERE werks = @l_werks
         AND lgort = @w_zsdt0330-lgort.
      ENDSELECT.

      IF sy-subrc <> 0.
        w_retorno-mensagem = r_msg_erro = 'Deposito nao existe no SAP!'.
        me->zif_trace_cotton~set_gravar_retorno( w_retorno ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.

      IF w_zsdt0330-ch_referencia IS INITIAL.
        w_retorno-mensagem = r_msg_erro =  |Romaneio de saida número: { w_zsdt0330-nr_romaneio } para a Ordem Venda: { w_zsdt0330-vbeln } não localizado no SAP!|.
        me->zif_trace_cotton~set_gravar_retorno( w_retorno ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0001ovro INTO @DATA(lwa_zsdt0001ovro)
       WHERE nr_ordem_venda  EQ @w_zsdt0330-vbeln
         AND nr_romaneio_sai EQ @w_zsdt0330-nr_romaneio.

      IF sy-subrc NE 0.
        w_retorno-mensagem = r_msg_erro = |Quantidade de fardos do romaneio { w_zsdt0330-nr_romaneio } para a Ordem Venda: { w_zsdt0330-vbeln } não identificada no OPUS|.
        me->zif_trace_cotton~set_gravar_retorno( w_retorno ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.

      DATA(_count_fardos) = 0.
      LOOP AT lit_fardos_romaneio INTO DATA(lwa_fardo_romaneio) WHERE vbeln       = w_zsdt0330-vbeln
                                                                  AND nr_romaneio = w_zsdt0330-nr_romaneio.
        ADD 1 TO _count_fardos.
      ENDLOOP.

      IF lwa_zsdt0001ovro-nm_qtd_embalagens NE _count_fardos.
        w_retorno-mensagem = |Quantidade de fardos do romaneio { w_zsdt0330-nr_romaneio } para a Ordem Venda: { w_zsdt0330-vbeln } divergente do OPUS!|.
        w_retorno-mensagem = |{ w_retorno-mensagem } Quantidade OPUS: { lwa_zsdt0001ovro-nm_qtd_embalagens } - Quantidade Carregamento: { _count_fardos }|.

        r_msg_erro = w_retorno-mensagem.

        me->zif_trace_cotton~set_gravar_retorno( w_retorno ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.


      "Projeto Reestruturação Algodao 2024
*      SELECT *
*        FROM zppt0002
*        INTO TABLE @DATA(t_0002)
*       WHERE cd_sai = @w_zsdt0330-cd_sai.
*
*      IF sy-subrc = 0.
*        SORT t_0002 BY budat DESCENDING.
*        READ TABLE t_0002 INTO DATA(w_0002) INDEX 1.
*        w_zsdt0330-acharg = w_0002-acharg.
*      ELSE.
*        w_retorno-mensagem = 'Fardo nao encontrado no SAP!'.
*        me->zif_trace_cotton~set_gravar_retorno( w_retorno ).
*        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
*        l_erro = abap_true.
*        CONTINUE.
*      ENDIF.
*
*      SELECT SINGLE charg
*        FROM mchb
*        INTO @DATA(w_mchb)
*       WHERE matnr  = @w_zsdt0330-matnr
*         AND werks  = @w_zsdt0330-werks
*         AND lgort  = @w_zsdt0330-lgort
*         AND charg  = @w_zsdt0330-acharg.
*
*      IF sy-subrc <> 0.
*        w_retorno-mensagem = 'Fardo nao encontrado no SAP!'.
*        me->zif_trace_cotton~set_gravar_retorno( w_retorno ).
*        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
*        l_erro = abap_true.
*        CONTINUE.
*      ENDIF.
      "Projeto Reestruturação Algodao 2024

      MODIFY me->zif_trace_cotton~at_zsdt0330 FROM w_zsdt0330 INDEX l_tabix TRANSPORTING matnr acharg.

    ENDLOOP.

    IF l_erro = abap_true.
      LOOP AT me->zif_trace_cotton~at_zsdt0330 INTO w_zsdt0330.
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX sy-tabix.
      ENDLOOP.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_form_request_http.

    r_if_integracao_inject = me.
    e_form_fields = me->zif_integracao_inject~at_form_fields.

  ENDMETHOD.


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


  METHOD zif_integracao_inject~set_form_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_form_fields = i_form_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: t_data_inbound       TYPE zsdt0330_trace_t,
          w_data_inbound       TYPE zsdt0330_trace,
          t_zsdt0330           TYPE TABLE OF zsdt0330,
          w_zsdt0330           TYPE zsdt0330,
          w_instrucoesembarque TYPE zsde0065,
          w_ordemvenda         TYPE zsde0052,
          w_lotes              TYPE zsde0053,
          w_fardinhos          TYPE zsde0055,
          l_tabix              TYPE sy-tabix,
          l_seq                TYPE timestampl.

    r_if_integracao_inject = me.

    IF     me->zif_integracao_inject~at_info_request_http-ds_url CS 'receive'.
*-----------------------------
* --- carregamento normal
*-----------------------------
      zcl_trace_cotton=>zif_trace_cotton~get_instance(
         )->set_receive_carregamento( EXPORTING i_at_info_request_http = me->zif_integracao_inject~at_info_request_http
                                                i_msg_inbound          = i_msg_inbound
                                                i_msg_completa         = i_msg_completa
                                                i_id_integracao        = i_id_integracao
                                      IMPORTING e_msg_outbound         = e_msg_outbound
                                                e_sucesso              = e_sucesso
                                                e_nm_code              = e_nm_code
                                                e_msg_erro             = e_msg_erro  ).

    ELSEIF me->zif_integracao_inject~at_info_request_http-ds_url CS 'estorno'.
*-----------------------------
* --- estorno carregamento
*-----------------------------
      zcl_trace_cotton=>zif_trace_cotton~get_instance(
         )->set_estorno_carregamento( EXPORTING i_at_info_request_http = me->zif_integracao_inject~at_info_request_http
                                                i_msg_inbound          = i_msg_inbound
                                                i_msg_completa         = i_msg_completa
                                                i_id_integracao        = i_id_integracao
                                      IMPORTING e_msg_outbound         = e_msg_outbound
                                                e_sucesso              = e_sucesso
                                                e_nm_code              = e_nm_code
                                                e_msg_erro             = e_msg_erro  ).
    ENDIF.

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


  METHOD zif_trace_cotton~get_id_referencia.

    r_if_trace_cotton          = me.
    e_referencia-tp_referencia = me->zif_trace_cotton~at_tp_referencia.
    e_referencia-id_referencia = me->zif_trace_cotton~at_id_referencia.

  ENDMETHOD.


  METHOD zif_trace_cotton~get_instance.

    IF zif_trace_cotton~at_if_trace_cotton IS NOT BOUND.
      CREATE OBJECT zif_trace_cotton~at_if_trace_cotton
        TYPE zcl_trace_cotton.
    ENDIF.
    r_if_trace_cotton = zif_trace_cotton~at_if_trace_cotton.

  ENDMETHOD.


  METHOD zif_trace_cotton~get_metodo.
  ENDMETHOD.


  METHOD zif_trace_cotton~get_qtde_program_exec.


    DATA: lc_quantidade_int    TYPE i,
          lc_quantidade_str_19 TYPE c LENGTH 10,
          lc_quantidade_str_20 TYPE c LENGTH 10,
          t_status             TYPE zde_btcstatus_t.

    FREE: e_quantidade.

    APPEND 'R' TO t_status.

*-----------------------------------------------------------------------------*
* Get Jobs do programa ZMMR030( Criação Registros ) em Execução
*-----------------------------------------------------------------------------*
    TRY.
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = zif_trace_cotton~c_zsdr0151_job   " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status   " Status de Jobs
          IMPORTING
            e_quantidade = lc_quantidade_int ).
      CATCH zcx_job.
    ENDTRY.

    ADD lc_quantidade_int TO e_quantidade.

    WRITE lc_quantidade_int TO lc_quantidade_str_19.
    CONDENSE lc_quantidade_str_19 NO-GAPS.

*-----------------------------------------------------------------------------*
* Get Jobs do programa ZMMR030( Criação Registros ) em Execução
*-----------------------------------------------------------------------------*
    TRY.
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = zif_trace_cotton~c_zsdr0152_job   " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status   " Status de Jobs
          IMPORTING
            e_quantidade = lc_quantidade_int ).
      CATCH zcx_job.
    ENDTRY.

    ADD lc_quantidade_int TO e_quantidade.

    WRITE lc_quantidade_int TO lc_quantidade_str_19.
    CONDENSE lc_quantidade_str_20 NO-GAPS.

    IF i_show_msg EQ abap_true.
      MESSAGE s007(zjob) WITH zif_trace_cotton~c_zsdr0151_job
                              lc_quantidade_str_19
                              zif_trace_cotton~c_zsdr0152_job
                              lc_quantidade_str_20.
    ENDIF.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_aguardar_job.

    DATA: lc_lim_jobs_exec TYPE i,
          lc_qtde_times    TYPE i,
          lc_show_msg      TYPE c,
          e_quantidade     TYPE i.

    lc_lim_jobs_exec = 10.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(wl_set_job_est_gr_lim_exec)
     WHERE setname = 'JOB_TRACE_COTTON'.

    IF sy-subrc EQ 0.
      IF wl_set_job_est_gr_lim_exec-valfrom > 0.
        lc_lim_jobs_exec = wl_set_job_est_gr_lim_exec-valfrom.
      ENDIF.
    ENDIF.

*-------------------------------
*---- checar quantidade de jobs em execucao
*-------------------------------
    e_quantidade = zcl_trace_cotton=>zif_trace_cotton~get_qtde_program_exec( abap_true ).

    lc_qtde_times = 0.
    WHILE e_quantidade >= lc_lim_jobs_exec.
      WAIT UP TO 1 SECONDS.
      lc_show_msg = abap_false.
      IF lc_qtde_times = 10.
        lc_qtde_times = 0.
        lc_show_msg   = abap_true.
      ENDIF.
      e_quantidade = zcl_trace_cotton=>zif_trace_cotton~get_qtde_program_exec( lc_show_msg ).
      ADD 1 TO lc_qtde_times.
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_cadastra_contratos.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_trace_cotton                     = me.
    me->zif_trace_cotton~at_id_contrato   = i_id_contrato.
    me->zif_trace_cotton~at_objecttable   = 'ZSDT0143'.
    me->zif_trace_cotton~at_tp_referencia = 'TRACE COTTON-Contratos'.
    me->zif_trace_cotton~at_id_referencia = i_id_contrato.

*-------------------------------
*-- instrucao
*-------------------------------
    CLEAR me->zif_trace_cotton~at_zsdt0143.

    SELECT SINGLE *
      FROM zsdt0143
      INTO me->zif_trace_cotton~at_zsdt0143
     WHERE id_contrato   = me->zif_trace_cotton~at_id_contrato.

    IF sy-subrc <> 0.
      me->zif_trace_cotton~set_mensagem( '03' ).
    ENDIF.

*-------------------------------
*--Monta JSON
*-------------------------------
    me->zif_trace_cotton~set_json_criar_contrato( ).

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_exec_trace( EXPORTING i_metodo = 'CADASTRA_CONTRATO' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_cadastra_instrucao.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_trace_cotton                     = me.
    me->zif_trace_cotton~at_zseq_inst     = i_zseq_inst.
    me->zif_trace_cotton~at_objek         = i_objek.
    me->zif_trace_cotton~at_objecttable   = i_objecttable.
    me->zif_trace_cotton~at_tp_referencia = 'TRACE COTTON-Instrução'.
    me->zif_trace_cotton~at_id_referencia = i_zseq_inst && '/' && i_objek.

*-------------------------------
*-- instrucao
*-------------------------------
    CLEAR me->zif_trace_cotton~at_zsdt0045.

    SELECT SINGLE *
      FROM zsdt0045
      INTO me->zif_trace_cotton~at_zsdt0045
     WHERE zseq_inst     = me->zif_trace_cotton~at_zseq_inst
       AND objek         = me->zif_trace_cotton~at_objek
       AND objecttable   = me->zif_trace_cotton~at_objecttable.

    IF sy-subrc <> 0.
      me->zif_trace_cotton~set_mensagem( '01' ).
    ENDIF.

*-------------------------------
*--Monta JSON
*-------------------------------
    me->zif_trace_cotton~set_json_criar_docto( ).

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_exec_trace( EXPORTING i_metodo = 'CADASTRA_INSTRUCAO' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_cadastra_ordem_venda.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_trace_cotton                     = me.
    me->zif_trace_cotton~at_nro_sol_ov    = i_nro_sol_ov.
    me->zif_trace_cotton~at_posnr         = i_posnr.
    me->zif_trace_cotton~at_tp_referencia = 'TRACE COTTON-Ordem Venda'.
    me->zif_trace_cotton~at_id_referencia = i_nro_sol_ov && '/' && i_posnr.

*-------------------------------
*-- Ordem VEnda
*-------------------------------
    CLEAR me->zif_trace_cotton~at_zsdt0066.

    SELECT SINGLE *
      FROM zsdt0066
      INTO me->zif_trace_cotton~at_zsdt0066
     WHERE nro_sol_ov    = me->zif_trace_cotton~at_nro_sol_ov
       AND posnr         = me->zif_trace_cotton~at_posnr
       AND status        = 'L'.  "*-CS2023000189-04.09.2023-#122555-JT

    IF ( sy-subrc <> 0 ) OR
       ( sy-subrc  = 0  AND me->zif_trace_cotton~at_zsdt0066-vbeln IS INITIAL ).
      SELECT SINGLE *
        FROM zsdt0053
        INTO CORRESPONDING FIELDS OF me->zif_trace_cotton~at_zsdt0066
       WHERE nro_sol_ov    = me->zif_trace_cotton~at_nro_sol_ov
         AND posnr         = me->zif_trace_cotton~at_posnr.

      SELECT SINGLE *
        FROM zsdt0213
        INTO @DATA(w_0213)
       WHERE nro_sol_ov    = @me->zif_trace_cotton~at_nro_sol_ov
         AND posnr         = @me->zif_trace_cotton~at_posnr.

      me->zif_trace_cotton~at_zsdt0066-charg_ori = w_0213-lgort.
    ENDIF.

    IF ( sy-subrc <> 0 ) OR
       ( sy-subrc  = 0  AND me->zif_trace_cotton~at_zsdt0066-vbeln IS INITIAL ).
      me->zif_trace_cotton~set_mensagem( '04' ).
    ENDIF.

*-------------------------------
*--Monta JSON
*-------------------------------
    me->zif_trace_cotton~set_json_criar_ordem_venda( ).

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_exec_trace( EXPORTING i_metodo = 'CADASTRA_ORDEM_VENDA' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_ds_data.

    r_if_trace_cotton = me.

*---------------------------------------
*---types
*---------------------------------------
    TYPES BEGIN OF ty_retorno.
    TYPES: access_token TYPE string.
    TYPES: expires_in TYPE string.
    TYPES: token_type TYPE string.
    TYPES END OF ty_retorno.

*---------------------------------------
*---workarea
*---------------------------------------
    DATA: l_access_token TYPE string,
          l_token_type   TYPE string,
          l_expires_in   TYPE string,
          l_token        TYPE string,
          l_json         TYPE string,
          lc_retorno     TYPE ty_retorno.

    FREE: me->zif_integracao_inject~at_header_fields,
          me->zif_integracao_inject~at_form_fields.

    CASE me->zif_trace_cotton~at_metodo.
      WHEN 'TOKEN'.
        l_json = 'grant_type='     && me->zif_trace_cotton~at_webservice-username &&
                 '&client_id='     && me->zif_trace_cotton~at_webservice-password &&
                 '&client_secret=' && me->zif_trace_cotton~at_webservice-add01    &&
                 '&resource='      && me->zif_trace_cotton~at_webservice-add02.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/x-www-form-urlencoded'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = l_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

      WHEN 'CADASTRA_INSTRUCAO'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access_token.
        l_token_type   = lc_retorno-token_type.
        l_expires_in   = lc_retorno-expires_in.
        l_token        = |{ l_token_type } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )   TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json;charset=UTF-8'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_trace_cotton~at_json.
*       me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

*--------------------------
*------ verifica metodo de chamada
*--------------------------
        SELECT SINGLE *
          FROM zsdt0327
          INTO @DATA(w_0327)
         WHERE zseq_inst   = @me->zif_trace_cotton~at_zsdt0045-zseq_inst
           AND objek       = @me->zif_trace_cotton~at_zsdt0045-objek
           AND objecttable = @me->zif_trace_cotton~at_zsdt0045-objecttable
           AND lgort       = @me->zif_trace_cotton~at_zsdt0045-charg
           AND tipo_msg    = 'S'.

        IF sy-subrc <> 0.
          SELECT SINGLE *
            FROM zsdt0327
            INTO w_0327
           WHERE zseq_inst   = me->zif_trace_cotton~at_zsdt0045-zseq_inst
             AND objek       = me->zif_trace_cotton~at_zsdt0045-objek
             AND objecttable = me->zif_trace_cotton~at_zsdt0045-objecttable
             AND lgort       = abap_off
             AND tipo_msg    = 'S'.
        ENDIF.

        me->zif_integracao_inject~at_info_request_http-ds_metodo       = COND #( WHEN sy-subrc = 0 THEN 'PUT'
                                                                                                   ELSE 'POST' ).

      WHEN 'EXCLUIR_INSTRUCAO'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access_token.
        l_token_type   = lc_retorno-token_type.
        l_expires_in   = lc_retorno-expires_in.
        l_token        = |{ l_token_type } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )   TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json;charset=UTF-8'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_trace_cotton~at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'DELETE'.

      WHEN 'CADASTRA_ORDEM_VENDA'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access_token.
        l_token_type   = lc_retorno-token_type.
        l_expires_in   = lc_retorno-expires_in.
        l_token        = |{ l_token_type } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )   TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_trace_cotton~at_json.
*       me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

*--------------------------
*------ verifica metodo de chamada
*--------------------------
*        SELECT SINGLE *
*          FROM zsdt0327
*          INTO @DATA(w_0327x)
*         WHERE nro_sol_ov  = @me->zif_trace_cotton~at_zsdt0066-nro_sol_ov
*           AND posnr       = @me->zif_trace_cotton~at_zsdt0066-posnr
*           AND tipo_msg    = 'S'.
*       me->zif_integracao_inject~at_info_request_http-ds_metodo       = COND #( WHEN sy-subrc = 0 THEN 'PUT'
*                                                                                                  ELSE 'POST' ).
*       me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

        SELECT SINGLE *
          FROM zsdt0213_trace
          INTO @DATA(w_0213_trace)
         WHERE nro_sol_ov  = @me->zif_trace_cotton~at_zsdt0066-nro_sol_ov
           AND posnr       = @me->zif_trace_cotton~at_zsdt0066-posnr
           AND vbeln       = @me->zif_trace_cotton~at_zsdt0066-vbeln.  "*-IR 189210-14.01.2025-#163685-JT-inicio

        me->zif_integracao_inject~at_info_request_http-ds_metodo       = COND #( WHEN sy-subrc = 0 AND w_0213_trace-integrado = abap_true  THEN 'PUT'
                                                                                                                                           ELSE 'POST' ).

      WHEN 'EXCLUIR_ORDEM_VENDA'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access_token.
        l_token_type   = lc_retorno-token_type.
        l_expires_in   = lc_retorno-expires_in.
        l_token        = |{ l_token_type } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )   TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_trace_cotton~at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'DELETE'.

      WHEN 'CADASTRA_CONTRATO'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access_token.
        l_token_type   = lc_retorno-token_type.
        l_expires_in   = lc_retorno-expires_in.
        l_token        = |{ l_token_type } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )   TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_trace_cotton~at_json.
*       me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

*--------------------------
*------ verifica metodo de chamada
*--------------------------
        SELECT SINGLE *
          FROM zsdt0143
          INTO @DATA(w_0143)
         WHERE id_contrato = @me->zif_trace_cotton~at_zsdt0143-id_contrato.

        me->zif_integracao_inject~at_info_request_http-ds_metodo       = COND #( WHEN w_0143-t_cotton = abap_on THEN 'PUT'
                                                                                                                ELSE 'POST' ).
      WHEN 'EXCLUIR_CONTRATO'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access_token.
        l_token_type   = lc_retorno-token_type.
        l_expires_in   = lc_retorno-expires_in.
        l_token        = |{ l_token_type } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )   TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_trace_cotton~at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'DELETE'.

      WHEN 'RETORNO_TRACE' OR 'ENVIAR_NOTAFISCAL' OR 'CANCELAR_NOTAFISCAL'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access_token.
        l_token_type   = lc_retorno-token_type.
        l_expires_in   = lc_retorno-expires_in.
        l_token        = |{ l_token_type } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )   TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_trace_cotton~at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'PUT'.

      WHEN 'RETORNO_TRACE_ESTORNO'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access_token.
        l_token_type   = lc_retorno-token_type.
        l_expires_in   = lc_retorno-expires_in.
        l_token        = |{ l_token_type } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )   TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_trace_cotton~at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_ds_url.

    r_if_trace_cotton = me.

    DATA: l_url     TYPE string,
          l_data    TYPE string,
          l_ativo   TYPE string,
          l_servico TYPE string.

    CASE i_metodo.
      WHEN 'TOKEN'.
        l_servico = 'TRACE_COTTON_TOKEN'.
      WHEN 'CADASTRA_INSTRUCAO'.
        l_servico = 'TRACE_COTTON_INSTRUCAO'.
      WHEN 'EXCLUIR_INSTRUCAO'.
        l_servico = 'TRACE_COTTON_INSTRUCAO'.
      WHEN 'CADASTRA_ORDEM_VENDA'.
        l_servico = 'TRACE_COTTON_ORDEM_VENDA'.
      WHEN 'EXCLUIR_ORDEM_VENDA'.
        l_servico = 'TRACE_COTTON_ORDEM_VENDA'.
      WHEN 'CADASTRA_CONTRATO'        .
        l_servico = 'TRACE_COTTON_CONTRATOS'.
      WHEN 'EXCLUIR_CONTRATO'        .
        l_servico = 'TRACE_COTTON_CONTRATOS'.
      WHEN 'RETORNO_TRACE' OR 'ENVIAR_NOTAFISCAL'.
        l_servico = 'TRACE_COTTON_RETORNO'.
      WHEN 'RETORNO_TRACE_ESTORNO'.
        l_servico = 'TRACE_COTTON_RETORNO_ESTORNO'.
      WHEN 'CANCELAR_NOTAFISCAL'.
        l_servico = 'TRACE_COTTON_NF_CANCELA'.
    ENDCASE.

    SELECT SINGLE *
             FROM zauth_webservice
             INTO @DATA(wa_webservice)
            WHERE service = @l_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'T'
                            attr2 = 'TC' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'T'
          msgv2  = 'TC'.
    ENDIF.

    me->zif_trace_cotton~at_webservice = wa_webservice.
    me->zif_trace_cotton~at_metodo     = i_metodo.

    CASE i_metodo.
      WHEN 'TOKEN'.
        l_url = wa_webservice-url.

      WHEN 'CADASTRA_INSTRUCAO'.
        l_url = wa_webservice-url.

      WHEN 'EXCLUIR_INSTRUCAO'.
        l_url = wa_webservice-url.

      WHEN 'CADASTRA_ORDEM_VENDA'.
        l_url = wa_webservice-url.

      WHEN 'EXCLUIR_ORDEM_VENDA'.
        l_url = wa_webservice-url.

      WHEN 'CADASTRA_CONTRATO'.
        l_url = wa_webservice-url.

      WHEN 'EXCLUIR_CONTRATO'.
        l_url = wa_webservice-url.

      WHEN 'RETORNO_TRACE' OR 'RETORNO_TRACE_ESTORNO' OR 'ENVIAR_NOTAFISCAL'.
        l_url = wa_webservice-url && me->zif_trace_cotton~at_id_carga && wa_webservice-add01.

      WHEN 'CANCELAR_NOTAFISCAL'.
        l_url = wa_webservice-url.

    ENDCASE.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
*   me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token        = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url              = l_url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_trace_cotton~set_id_referencia( ).

  ENDMETHOD.


  METHOD zif_trace_cotton~set_envia_nf_cancelada.

    TYPES: BEGIN OF ty_jlin,
             docnum TYPE j_1bnflin-docnum,
             refkey TYPE j_1bnflin-refkey,
             vbeln  TYPE vbrk-vbeln.
    TYPES: END OF ty_jlin.

    DATA: w_notasfiscais TYPE zsde0073,
          t_jlin         TYPE TABLE OF ty_jlin,
          w_jlin         TYPE ty_jlin,
          l_tenta        TYPE i,
          l_error1       TYPE char1,
          l_error2       TYPE char1.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_trace_cotton                     = me.
    me->zif_trace_cotton~at_tp_referencia = 'TRACE COTTON-Nt.Fiscal'.
    me->zif_trace_cotton~at_id_referencia = i_docnum.

*-------------------------------
*-- seleciona carregamento
*-------------------------------
    SELECT *
      FROM zsdt0330
      INTO TABLE me->zif_trace_cotton~at_zsdt0330
     WHERE docnum     = i_docnum
       AND cancelado  = abap_off.

    IF sy-subrc <> 0.
      SELECT *
        FROM zsdt0330
        INTO TABLE me->zif_trace_cotton~at_zsdt0330
       WHERE docnum_cancelado = i_docnum
         AND cancelado        = abap_off.
    ENDIF.

    CHECK sy-subrc = 0.

    READ TABLE me->zif_trace_cotton~at_zsdt0330 INTO DATA(w_0330) INDEX 1.
    me->zif_trace_cotton~at_id_carga = w_0330-id_carga.

*---------------------------------------------
*-- acessar NFe
*---------------------------------------------
    SELECT SINGLE docnum, nfenum, series
      INTO @DATA(w_jdoc)
      FROM j_1bnfdoc
     WHERE docnum = @i_docnum.

    CHECK sy-subrc = 0.

*-----------------------------------
*-- monta estrutura
*-----------------------------------
    w_notasfiscais-carregamentoid                 = me->zif_trace_cotton~at_id_carga.
    w_notasfiscais-numeronotafiscal               = w_jdoc-nfenum.
    w_notasfiscais-referenciaintegracaoordemvenda = w_0330-vbeln_fluxo.

*-----------------------------------
*-- monta JSON
*-----------------------------------
    zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_notasfiscais
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

*-----------------------------------
*-- Atualiza NF
*-----------------------------------
    LOOP AT me->zif_trace_cotton~at_zsdt0330  INTO w_0330.
      w_0330-docnum                              = abap_off.
      w_0330-docnum_cancelado                    = i_docnum.
      MODIFY me->zif_trace_cotton~at_zsdt0330 FROM w_0330 INDEX sy-tabix TRANSPORTING docnum docnum_cancelado.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
      UPDATE zsdt0330 set docnum              = abap_off
                          docnum_cancelado    = i_docnum
                    WHERE id_carga      EQ w_0330-id_carga
                      AND matnr         EQ w_0330-matnr
                      AND werks         EQ w_0330-werks
                      AND lgort         EQ w_0330-lgort
                      AND acharg        EQ w_0330-acharg
                      AND safra         EQ w_0330-safra
                      AND seq           EQ w_0330-seq.
       "MODIFY zsdt0330                         FROM w_0330.
       "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---
    ENDLOOP.

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_exec_trace( EXPORTING i_metodo = 'CANCELAR_NOTAFISCAL' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        l_error1 = abap_true.

      CATCH zcx_error INTO DATA(ex_error).
        l_error2 = abap_true.

    ENDTRY.

*-------------------------------------
* envia excecao erro
*-------------------------------------
    CASE abap_true.

      WHEN l_error1.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).


      WHEN l_error2.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).

    ENDCASE.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_envia_nota_fiscal.

    TYPES: BEGIN OF ty_jlin,
             docnum TYPE j_1bnflin-docnum,
             refkey TYPE j_1bnflin-refkey,
             vbeln  TYPE vbrk-vbeln.
    TYPES: END OF ty_jlin.

    DATA: w_notas        TYPE zsdt0330_retorno,
          w_notasfiscais TYPE zsde0056,
          t_notasfiscais TYPE zsde0056_t,
          t_jlin         TYPE TABLE OF ty_jlin,
          w_jlin         TYPE ty_jlin,
          l_tenta        TYPE i,
          l_error1       TYPE char1,
          l_error2       TYPE char1.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_trace_cotton                     = me.
    me->zif_trace_cotton~at_tp_referencia = 'TRACE COTTON-Nt.Fiscal'.
    me->zif_trace_cotton~at_id_referencia = i_docnum.

*-------------------------------
*-- selecao romaneio
*-------------------------------
    CLEAR me->zif_trace_cotton~at_zsdt0001.

*-------------------------------
*-- Busca NF
*-------------------------------
    FREE: l_tenta.

    DO.
      l_tenta = l_tenta + 1.

      IF l_tenta > 200.
        EXIT.
      ENDIF.

      SELECT *
        INTO me->zif_trace_cotton~at_zsdt0001
        FROM zsdt0001
          UP TO 1 ROWS
       WHERE nro_nf_prod = i_docnum.
      ENDSELECT.

      IF sy-subrc <> 0.
        WAIT UP TO 5 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    CHECK me->zif_trace_cotton~at_zsdt0001-ch_referencia IS NOT INITIAL.

*-------------------------------
*-- é NF de romaneio algodao
*-------------------------------
    SELECT *
      FROM zsdt0330
      INTO TABLE me->zif_trace_cotton~at_zsdt0330
     WHERE ch_referencia = me->zif_trace_cotton~at_zsdt0001-ch_referencia
       AND cancelado     = abap_off.

    CHECK sy-subrc = 0.

    READ TABLE me->zif_trace_cotton~at_zsdt0330 INTO DATA(w_0330) INDEX 1.
    me->zif_trace_cotton~at_id_carga = w_0330-id_carga.

*---------------------------------------------
*-- acessar NFe
*---------------------------------------------
    SELECT SINGLE docnum, nfenum, series
      INTO @DATA(w_jdoc)
      FROM j_1bnfdoc
     WHERE docnum = @i_docnum.

    CHECK sy-subrc = 0.

    SELECT SINGLE docnum, cancel
      INTO @DATA(w_active)
      FROM j_1bnfe_active
     WHERE docnum = @i_docnum.

    CHECK sy-subrc = 0.
    CHECK w_active-cancel = abap_false.

*-------------------------------
*-- buscar OV
*-------------------------------
    SELECT docnum refkey
      FROM j_1bnflin
      INTO TABLE t_jlin
     WHERE docnum = i_docnum.

    LOOP AT t_jlin  INTO w_jlin.
      w_jlin-vbeln     = w_jlin-refkey.
      MODIFY t_jlin FROM w_jlin INDEX sy-tabix.
    ENDLOOP.

    SELECT vbeln
      FROM vbrk
      INTO TABLE @DATA(t_vbrk)
       FOR ALL ENTRIES IN @t_jlin
     WHERE vbeln = @t_jlin-vbeln
       AND fksto = @abap_off.

    CHECK sy-subrc = 0.

*-------------------------------
*-- fluxo documento
*-------------------------------
    SELECT *
      FROM vbfa
      INTO TABLE @DATA(t_vbfa)
       FOR ALL ENTRIES IN @t_vbrk
     WHERE vbeln = @t_vbrk-vbeln
       AND vbtyp_n = 'M'
       AND vbtyp_v = 'C'.

*-------------------------------
*-- monta estrutura json
*-------------------------------
    FREE: t_notasfiscais.

    w_notas-sucesso                                  = abap_true.

    LOOP AT t_vbfa INTO DATA(w_vbfa).
      w_notasfiscais-numero                          = w_jdoc-nfenum.
      w_notasfiscais-ordemvenda-numero               = w_vbfa-vbelv.
      w_notasfiscais-ordemvenda-referenciaintegracao = w_vbfa-vbelv.
      APPEND w_notasfiscais                         TO t_notasfiscais.
    ENDLOOP.

    w_notas-notasfiscais[]                           = t_notasfiscais[].

*-----------------------------------
*-- monta JSON
*-----------------------------------
    zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_notas
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

*-----------------------------------
*-- Atualiza NF
*-----------------------------------
    LOOP AT me->zif_trace_cotton~at_zsdt0330  INTO w_0330.
      w_0330-docnum                              = i_docnum.
      w_0330-vbeln_fluxo                         = w_vbfa-vbelv.
      w_0330-docnum_cancelado                    = abap_off.
      MODIFY me->zif_trace_cotton~at_zsdt0330 FROM w_0330 INDEX sy-tabix TRANSPORTING docnum docnum_cancelado vbeln_fluxo.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
       UPDATE zsdt0330 set docnum           = w_0330-docnum
                           vbeln_fluxo      = w_0330-vbeln_fluxo
                           docnum_cancelado = w_0330-docnum_cancelado
                    WHERE id_carga      EQ w_0330-id_carga
                      AND matnr         EQ w_0330-matnr
                      AND werks         EQ w_0330-werks
                      AND lgort         EQ w_0330-lgort
                      AND acharg        EQ w_0330-acharg
                      AND safra         EQ w_0330-safra
                      AND seq           EQ w_0330-seq.
      "MODIFY zsdt0330                         FROM w_0330.
      "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---

    ENDLOOP.

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_exec_trace( EXPORTING i_metodo = 'ENVIAR_NOTAFISCAL' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        l_error1 = abap_true.

      CATCH zcx_error INTO DATA(ex_error).
        l_error2 = abap_true.

    ENDTRY.

*-------------------------------------
* envia excecao erro
*-------------------------------------
    CASE abap_true.

      WHEN l_error1.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).


      WHEN l_error2.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).

    ENDCASE.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_estorno_carregamento.

    TYPES: BEGIN OF ty_quant,
             tipo             TYPE char1,
             id_carga         TYPE zsdt0330-id_carga,
             qtd_fardos_carga TYPE i.
    TYPES: END OF ty_quant.


    DATA: t_zsdt0330             TYPE TABLE OF zsdt0330,
          w_zsdt0330             TYPE zsdt0330,
          t_quant                TYPE TABLE OF ty_quant,
          w_quant                TYPE ty_quant,
          w_fardinhosadicionados TYPE zsde0181,
          w_fardinhosremovidos   TYPE zsde0181,
          w_retorno              TYPE zsdt0331,
          l_tabix                TYPE sy-tabix,
          l_seq                  TYPE timestampl.

    r_if_trace_cotton = me.

    me->zif_trace_cotton~at_info_request_http = i_at_info_request_http.

    FREE: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = me->zif_trace_cotton~at_data_estorno ).
    ENDIF.

*--------------------------------
*   construir tabela
*--------------------------------
    FREE: t_zsdt0330, t_quant, w_zsdt0330.

    TRANSLATE me->zif_trace_cotton~at_data_estorno-carregamentoid TO UPPER CASE.
    CONDENSE me->zif_trace_cotton~at_data_estorno-carregamentoid.

    "SD - Ganho Peso Automatico Algodao US #145369 - WPP
    DATA(_aprovador_estorno) = me->zif_trace_cotton~at_data_estorno-aprovadorestorno.
    IF me->zif_trace_cotton~at_data_estorno-dataaprovacaoestorno IS NOT INITIAL.
      DATA(_data_aprovacao_estorno) = me->zif_trace_cotton~at_data_estorno-dataaprovacaoestorno+00(04) &&
                                      me->zif_trace_cotton~at_data_estorno-dataaprovacaoestorno+05(02) &&
                                      me->zif_trace_cotton~at_data_estorno-dataaprovacaoestorno+08(02).
    ENDIF.
    "SD - Ganho Peso Automatico Algodao US #145369 - WPP

    LOOP AT me->zif_trace_cotton~at_data_estorno-fardinhosadicionados INTO w_fardinhosadicionados.

      w_zsdt0330-id_carga       = me->zif_trace_cotton~at_data_estorno-carregamentoid.
      w_zsdt0330-placa_cav      = me->zif_trace_cotton~at_data_estorno-veiculoplaca.
      w_zsdt0330-cpf_motorista  = me->zif_trace_cotton~at_data_estorno-motoristacpf.
      w_zsdt0330-vbeln          = |{ w_fardinhosadicionados-lote-ordemvenda-numero ALPHA = IN }|.
      w_zsdt0330-nr_romaneio    = w_fardinhosadicionados-lote-ordemvenda-romaneio-numero.

*-----------------------------
*---- busca ch referencia
*-----------------------------
      SELECT SINGLE  ch_referencia
        INTO @DATA(l_ch_referencia)
        FROM zsdt0001
       WHERE tp_movimento = 'S'
         AND nr_romaneio  = @w_zsdt0330-nr_romaneio
         AND vbeln        = @w_zsdt0330-vbeln.

      w_zsdt0330-ch_referencia  = l_ch_referencia.
      w_zsdt0330-kunnr          = |{ w_fardinhosadicionados-lote-ordemvenda-instrucaoembarque-cliente-referenciaintegracao ALPHA = IN }|.
      w_zsdt0330-lgort          = w_fardinhosadicionados-lote-numero.
      w_zsdt0330-safra          = w_fardinhosadicionados-lote-safraano.
      w_zsdt0330-normt          = w_fardinhosadicionados-lote-classificacao.

      w_zsdt0330-werks          = COND #( WHEN w_fardinhosadicionados-lote-fazendabeneficiamento-codigo(2) = w_fardinhosadicionados-lote-fazendaorigem-codigo(2)
                                          THEN w_fardinhosadicionados-lote-fazendabeneficiamento-codigo
                                          ELSE w_fardinhosadicionados-lote-fazendaorigem-codigo ).
      w_zsdt0330-cd_sai         = w_fardinhosadicionados-codigosai.
      w_zsdt0330-acharg         = w_fardinhosadicionados-numerofardocompleto.
      w_zsdt0330-status_estorno = 'I'.
      w_zsdt0330-user_estorno   = sy-uname.
      w_zsdt0330-data_estorno   = sy-datum.
      w_zsdt0330-hora_estorno   = sy-uzeit.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP
      w_zsdt0330-aprovador_estorno      =  _aprovador_estorno.
      w_zsdt0330-data_aprovacao_estorno =  _data_aprovacao_estorno.
      "SD - Ganho Peso Automatico Algodao US #145369 - WPP

      APPEND w_zsdt0330        TO t_zsdt0330.

      w_quant-tipo              = 'I'.
      w_quant-id_carga          = w_zsdt0330-id_carga.
      w_quant-qtd_fardos_carga  = 1.
      COLLECT w_quant       INTO t_quant.
    ENDLOOP.

    FREE: w_zsdt0330.

    LOOP AT me->zif_trace_cotton~at_data_estorno-fardinhosremovidos   INTO w_fardinhosremovidos.
      w_zsdt0330-id_carga       = me->zif_trace_cotton~at_data_estorno-carregamentoid.
      w_zsdt0330-placa_cav      = me->zif_trace_cotton~at_data_estorno-veiculoplaca.
      w_zsdt0330-cpf_motorista  = me->zif_trace_cotton~at_data_estorno-motoristacpf.
      w_zsdt0330-vbeln          = |{ w_fardinhosremovidos-lote-ordemvenda-numero ALPHA = IN }|.
      w_zsdt0330-nr_romaneio    = w_fardinhosremovidos-lote-ordemvenda-romaneio-numero.

*-----------------------------
*---- busca ch referencia
*-----------------------------
      SELECT SINGLE  ch_referencia
        INTO @DATA(l_ch_referencia2)
        FROM zsdt0001
       WHERE tp_movimento = 'S'
         AND nr_romaneio  = @w_zsdt0330-nr_romaneio
         AND vbeln        = @w_zsdt0330-vbeln.

      w_zsdt0330-ch_referencia  = l_ch_referencia2.
      w_zsdt0330-kunnr          = |{ w_fardinhosremovidos-lote-ordemvenda-instrucaoembarque-cliente-referenciaintegracao ALPHA = IN }|.
      w_zsdt0330-lgort          = w_fardinhosremovidos-lote-numero.
      w_zsdt0330-safra          = w_fardinhosremovidos-lote-safraano.
      w_zsdt0330-normt          = w_fardinhosremovidos-lote-classificacao.

      w_zsdt0330-werks          = COND #( WHEN w_fardinhosremovidos-lote-fazendabeneficiamento-codigo(2) = w_fardinhosremovidos-lote-fazendaorigem-codigo(2)
                                          THEN w_fardinhosremovidos-lote-fazendabeneficiamento-codigo
                                          ELSE w_fardinhosremovidos-lote-fazendaorigem-codigo ).

      w_zsdt0330-cd_sai         = w_fardinhosremovidos-codigosai.
      w_zsdt0330-acharg         = w_fardinhosremovidos-numerofardocompleto.

      w_zsdt0330-status_estorno = 'D'.
      w_zsdt0330-user_estorno   = sy-uname.
      w_zsdt0330-data_estorno   = sy-datum.
      w_zsdt0330-hora_estorno   = sy-uzeit.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP
      w_zsdt0330-aprovador_estorno      =  _aprovador_estorno.
      w_zsdt0330-data_aprovacao_estorno =  _data_aprovacao_estorno.
      "SD - Ganho Peso Automatico Algodao US #145369 - WPP


      APPEND w_zsdt0330        TO t_zsdt0330.

      w_quant-tipo              = 'D'.
      w_quant-id_carga          = w_zsdt0330-id_carga.
      w_quant-qtd_fardos_carga  = 1.
      COLLECT w_quant       INTO t_quant.
    ENDLOOP.

*--------------------------------
*   Ajusta quantidade de fardos
*--------------------------------
    LOOP AT t_zsdt0330      INTO w_zsdt0330 WHERE status_estorno = 'I'.
      l_tabix = sy-tabix.
      READ TABLE t_quant    INTO w_quant WITH KEY tipo     = 'I'
                                                  id_carga = w_zsdt0330-id_carga.
      CHECK sy-subrc = 0.
      w_zsdt0330-qtd_fardos_carga = w_quant-qtd_fardos_carga.
      MODIFY t_zsdt0330     FROM w_zsdt0330 INDEX l_tabix.
    ENDLOOP.

    LOOP AT t_zsdt0330      INTO w_zsdt0330 WHERE status_estorno = 'D'.
      l_tabix = sy-tabix.
      READ TABLE t_quant    INTO w_quant WITH KEY tipo     = 'D'
                                                  id_carga = w_zsdt0330-id_carga.
      CHECK sy-subrc = 0.
      w_zsdt0330-qtd_fardos_carga = w_quant-qtd_fardos_carga.
      MODIFY t_zsdt0330     FROM w_zsdt0330 INDEX l_tabix.
    ENDLOOP.

*--------------------------------
*   set tabela interna
*--------------------------------
    me->zif_trace_cotton~at_zsdt0330[] = t_zsdt0330[].

*--------------------------------
*   cancela mensagens anteriores
*--------------------------------
    UPDATE zsdt0331 SET cancelado = abap_true
                  WHERE id_carga      = me->zif_trace_cotton~at_data_estorno-carregamentoid
                    AND tipo_operacao = 'E'.
    COMMIT WORK.

*--------------------------------
*   valida os dados
*--------------------------------
    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_validar_dados_estorno( IMPORTING e_msg_erro = e_msg_erro ).

    IF e_msg_erro IS NOT INITIAL.
      e_sucesso      = abap_true.
      e_nm_code      = '200'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.

*--------------------------------------
*-- envio de retorno de erros ao trace
*--------------------------------------
      TRY .
          zcl_trace_cotton=>zif_trace_cotton~get_instance(
             )->set_retorno_trace( i_id_carga      = w_zsdt0330-id_carga
                                   i_tipo_operacao = 'E' ).

        CATCH zcx_integracao INTO DATA(ex_integra).
        CATCH zcx_error      INTO DATA(ex_error).
      ENDTRY.

      RETURN.
    ENDIF.

*--------------------------------------
*-- Processar
*--------------------------------------
    "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Ini
    GET TIME STAMP FIELD l_seq. "Gravar um mesmo SEQ para todos os registros, para identificar os registroso recebidos nessa requisição
    "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

    LOOP AT me->zif_trace_cotton~at_zsdt0330 ASSIGNING FIELD-SYMBOL(<fs_zsdt0330>).

      l_tabix = sy-tabix.

      MOVE-CORRESPONDING <fs_zsdt0330> TO w_retorno.

*----------------------------------------
*---- atualiza tabela
*----------------------------------------

      <fs_zsdt0330>-mandt               = sy-mandt.
      <fs_zsdt0330>-seq                 = l_seq.
      <fs_zsdt0330>-status_fardo        = '0'.
      <fs_zsdt0330>-status_gera_lote    = '0'.
      <fs_zsdt0330>-chave_referencia    = <fs_zsdt0330>-matnr.
      <fs_zsdt0330>-user_carga          = sy-uname.
      <fs_zsdt0330>-data_carga          = sy-datum.
      <fs_zsdt0330>-hora_carga          = sy-uzeit.
      MODIFY zsdt0330             FROM <fs_zsdt0330>.

      COMMIT WORK AND WAIT.

    ENDLOOP.

    me->zif_trace_cotton~add_fila_gera_sobra_perda( i_zsdt0330_t = me->zif_trace_cotton~at_zsdt0330 ). "SD - Ganho Peso Automatico Algodao US #145369 - WPP

*--------------------------------------
*-- Retorno processamento
*--------------------------------------
    SELECT SINGLE *
             FROM zsdt0331
             INTO @DATA(t_0331)
            WHERE id_carga      = @w_zsdt0330-id_carga
              AND selecionado   = @abap_false
              AND enviado_trace = @abap_false
              AND tipo_operacao = 'E'
              AND cancelado     = @abap_false.

    IF sy-subrc = 0.
      IF t_0331-mensagem = abap_off.
        e_sucesso      = abap_true.
        e_nm_code      = '200'.
        e_msg_erro     = 'Ok'.
        e_msg_outbound = ' { "protocolo" : "'   && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                         '   "status_code" : "' && e_nm_code                                             &&
                         '" '                   && cl_abap_char_utilities=>newline &&
                         ' }'.
      ELSE.
        e_sucesso      = abap_true.
        e_msg_erro     = 'Há Ocorrência de erros'.
        e_nm_code      = '200'.
        e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                         '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                         '}'.
      ENDIF.
    ELSE.
      e_sucesso      = abap_true.
      e_nm_code      = '200'.
      e_msg_erro     = 'Ok'.
      e_msg_outbound = ' { "protocolo" : "'   && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code                                             &&
                       '" '                   && cl_abap_char_utilities=>newline &&
                       ' }'.
    ENDIF.

*--------------------------------------
*-- envio de retorno de erros ao trace
*--------------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_retorno_trace( i_id_carga      = w_zsdt0330-id_carga
                                 i_tipo_operacao = 'E' ).

      CATCH zcx_integracao INTO ex_integra.
      CATCH zcx_error      INTO ex_error.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_exclui_contratos.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_trace_cotton                     = me.
    me->zif_trace_cotton~at_id_contrato   = i_id_contrato.
    me->zif_trace_cotton~at_objecttable   = 'ZSDT0143'.
    me->zif_trace_cotton~at_tp_referencia = 'TRACE COTTON-Contratos'.
    me->zif_trace_cotton~at_id_referencia = i_id_contrato.

*-------------------------------
*-- instrucao
*-------------------------------
    CLEAR me->zif_trace_cotton~at_zsdt0143.

    SELECT SINGLE *
      FROM zsdt0143
      INTO me->zif_trace_cotton~at_zsdt0143
     WHERE id_contrato   = me->zif_trace_cotton~at_id_contrato.

    IF sy-subrc <> 0.
      me->zif_trace_cotton~set_mensagem( '03' ).
    ENDIF.

*-------------------------------
*--Monta JSON
*-------------------------------
    me->zif_trace_cotton~set_json_exclui_contrato( ).

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_exec_trace( EXPORTING i_metodo = 'EXCLUIR_CONTRATO' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_exclui_instrucao.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_trace_cotton                     = me.
    me->zif_trace_cotton~at_zseq_inst     = i_zseq_inst.
    me->zif_trace_cotton~at_objek         = i_objek.
    me->zif_trace_cotton~at_objecttable   = i_objecttable.
    me->zif_trace_cotton~at_tp_referencia = 'TRACE COTTON-Instrução'.
    me->zif_trace_cotton~at_id_referencia = i_zseq_inst && '/' && i_objek.

*-------------------------------
*-- instrucao
*-------------------------------
    CLEAR me->zif_trace_cotton~at_zsdt0045.

    SELECT SINGLE *
      FROM zsdt0045
      INTO me->zif_trace_cotton~at_zsdt0045
     WHERE zseq_inst     = me->zif_trace_cotton~at_zseq_inst
       AND objek         = me->zif_trace_cotton~at_objek
       AND objecttable   = me->zif_trace_cotton~at_objecttable.

    IF sy-subrc <> 0.
      me->zif_trace_cotton~set_mensagem( '01' ).
    ENDIF.

*-------------------------------
*--Monta JSON
*-------------------------------
    me->zif_trace_cotton~set_json_exclui_docto( ).

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_exec_trace( EXPORTING i_metodo = 'EXCLUIR_INSTRUCAO' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_exclui_ordem_venda.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_trace_cotton                     = me.
    me->zif_trace_cotton~at_nro_sol_ov    = i_nro_sol_ov.
    me->zif_trace_cotton~at_posnr         = i_posnr.
    me->zif_trace_cotton~at_vbeln         = i_vbeln.
    me->zif_trace_cotton~at_tp_referencia = 'TRACE COTTON-Ordem Venda'.
    me->zif_trace_cotton~at_id_referencia = i_nro_sol_ov && '/' && i_posnr.

*-------------------------------
*-- Ordem VEnda
*-------------------------------
    CLEAR me->zif_trace_cotton~at_zsdt0066.

    SELECT SINGLE *
      FROM zsdt0066
      INTO me->zif_trace_cotton~at_zsdt0066
     WHERE nro_sol_ov    = me->zif_trace_cotton~at_nro_sol_ov
       AND posnr         = me->zif_trace_cotton~at_posnr.
*      AND status        = 'L'.  "*-CS2023000189-04.09.2023-#122555-JT

    IF sy-subrc <> 0.
      SELECT SINGLE *
        FROM zsdt0053
        INTO CORRESPONDING FIELDS OF me->zif_trace_cotton~at_zsdt0066
       WHERE nro_sol_ov    = me->zif_trace_cotton~at_nro_sol_ov
         AND posnr         = me->zif_trace_cotton~at_posnr.

      SELECT SINGLE *
        FROM zsdt0213
        INTO @DATA(w_0213)
       WHERE nro_sol_ov    = @me->zif_trace_cotton~at_nro_sol_ov
         AND posnr         = @me->zif_trace_cotton~at_posnr.

      me->zif_trace_cotton~at_zsdt0066-charg_ori = w_0213-lgort.
    ENDIF.

    IF sy-subrc <> 0.
      me->zif_trace_cotton~set_mensagem( '04' ).
    ENDIF.

*-------------------------------
*--Monta JSON
*-------------------------------
    me->zif_trace_cotton~set_json_exclui_ordem_venda( ).

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_exec_trace( EXPORTING i_metodo = 'EXCLUIR_ORDEM_VENDA' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_exec_trace.

*---------------------------------------
* types
*---------------------------------------
    TYPES: BEGIN OF ty_metodo,
             metodo TYPE string.
    TYPES: END   OF ty_metodo.

*-IR 189210-14.01.2025-#163685-JT-inicio
    TYPES: BEGIN OF ty_refer,
             referenciaintegracaoordemvenda TYPE STANDARD TABLE OF string WITH EMPTY KEY,
             carregamento                   TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    TYPES: END OF ty_refer.

    TYPES: BEGIN OF ty_retorno_ov,
             errors TYPE ty_refer.
    TYPES: END   OF ty_retorno_ov.
*-IR 189210-14.01.2025-#163685-JT-fim

*---------------------------------------
* workarea
*---------------------------------------
    DATA: lc_integracao      TYPE zintegracao,
          lc_mensagem        TYPE zintegracao_log,
          l_error            TYPE c,
          l_mesg             TYPE string,
          l_mesg_retorno     TYPE string,
          t_metodo           TYPE TABLE OF ty_metodo,
          w_metodo           TYPE ty_metodo,
          lc_retorno         TYPE zsde0040,
          t_empre            TYPE TABLE OF rgsb4,
          w_empre            TYPE rgsb4,
          w_zsdt0330         TYPE zsdt0330,
          w_zsdt0331         TYPE zsdt0331,
          w_zsdt0213_trace   TYPE zsdt0213_trace,
          w_zsdt0213_integra TYPE zsdt0213_integra,
          w_retorno_ov       TYPE ty_retorno_ov.          "*-IR 189210-14.01.2025-#163685-JT

*---------------------------------------
* inicio processo
*---------------------------------------
    r_if_trace_cotton = me.

    FREE: t_metodo,
          lc_integracao,
          l_error,
          me->zif_integracao_inject~at_header_fields,
          me->zif_integracao_inject~at_info_request_http.

    me->zif_integracao_inject~at_tp_integracao  = zif_integracao=>at_tp_integracao_outbound.

*---------------------------------------
*-- verifica se empresa envia ao trace
*---------------------------------------
    CASE i_metodo.
      WHEN 'CADASTRA_INSTRUCAO' OR 'CADASTRA_ORDEM_VENDA'.

        CALL FUNCTION 'G_SET_GET_ALL_VALUES'
          EXPORTING
            class         = '0000'
            setnr         = 'ZSDT0121_BUKRS_TRACE'
          TABLES
            set_values    = t_empre
          EXCEPTIONS
            set_not_found = 1
            OTHERS        = 2.

        READ TABLE t_empre INTO w_empre WITH KEY from = me->zif_trace_cotton~at_zsdt0045-bukrs.

        CHECK sy-subrc = 0.
    ENDCASE.

*---------------------------------------
* metodos chamada
*---------------------------------------
    w_metodo-metodo   = 'TOKEN'.
    APPEND w_metodo  TO t_metodo.
    w_metodo-metodo   = i_metodo.
    APPEND w_metodo  TO t_metodo.
*---------------------------------------

*---------------------------------------
*---buscar token / metodo
*---------------------------------------
    LOOP AT t_metodo INTO w_metodo.

      TRY.
          me->zif_trace_cotton~set_ds_url(
                             EXPORTING i_metodo        = w_metodo-metodo
            )->set_ds_data(  EXPORTING i_integracao    = lc_integracao
            )->set_send_msg( IMPORTING e_id_integracao = DATA(lc_id_integracao)
                                       e_integracao	   = lc_integracao
                                       e_mensagem      = lc_mensagem
            ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          l_error        = abap_true.
          l_mesg         = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.
          l_mesg_retorno = me->zif_trace_cotton~set_tratar_mensagem( EXPORTING i_json_retorno = lc_mensagem
                                                                               i_mensagem     = l_mesg ).

          CASE w_metodo-metodo.
            WHEN 'CADASTRA_INSTRUCAO'.
              me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = CONV #( me->zif_trace_cotton~at_zsdt0045-zseq_inst )
                                                             i_objek       = CONV #( me->zif_trace_cotton~at_zsdt0045-objek )
                                                             i_objecttable = CONV #( me->zif_trace_cotton~at_zsdt0045-objecttable )
                                                             i_lgort       = CONV #( me->zif_trace_cotton~at_zsdt0045-charg )
                                                             i_tipo_msg    = 'E'
                                                             i_mensagem    = l_mesg_retorno ).

            WHEN 'CADASTRA_ORDEM_VENDA'.
*-IR 189210-14.01.2025-#163685-JT-inicio
              /ui2/cl_json=>deserialize( EXPORTING json = lc_mensagem-ds_data_retorno
                                          CHANGING data = w_retorno_ov ).
              READ TABLE w_retorno_ov-errors-referenciaintegracaoordemvenda INTO DATA(_mensagem) INDEX 1.
              IF sy-subrc = 0.
                l_mesg_retorno = _mensagem.
              ELSE.
                READ TABLE w_retorno_ov-errors-carregamento INTO _mensagem INDEX 1.
                IF sy-subrc = 0.
                  l_mesg_retorno = _mensagem.
                ENDIF.
              ENDIF.
*-IR 189210-14.01.2025-#163685-JT-fim

              me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst    = me->zif_trace_cotton~at_zseq_inst
                                                             i_objek        = me->zif_trace_cotton~at_objek
                                                             i_objecttable  = me->zif_trace_cotton~at_objecttable
                                                             i_id_contrato  = me->zif_trace_cotton~at_zsdt0066-vbeln
                                                             i_nro_sol_ov   = me->zif_trace_cotton~at_nro_sol_ov
                                                             i_posnr        = me->zif_trace_cotton~at_posnr
                                                             i_tipo_integra = 'OV'
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg_retorno ).

              w_zsdt0213_integra-mandt        = sy-mandt.
              w_zsdt0213_integra-nro_sol_ov   = me->zif_trace_cotton~at_nro_sol_ov.
              w_zsdt0213_integra-posnr        = me->zif_trace_cotton~at_posnr.
              w_zsdt0213_integra-vbeln        = me->zif_trace_cotton~at_zsdt0066-vbeln.
              w_zsdt0213_integra-metodo       = me->zif_integracao_inject~at_info_request_http-ds_metodo.
              w_zsdt0213_integra-integrado    = abap_false.
              w_zsdt0213_integra-data_reg     = sy-datum.
              w_zsdt0213_integra-hora_reg     = sy-uzeit.
              w_zsdt0213_integra-user_reg     = sy-uname.
              w_zsdt0213_integra-tenta_envio  = 1.              "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-data_envio   = sy-datum.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-hora_envio   = sy-uzeit.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-user_envio   = sy-uname.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-mesg_retorno = l_mesg_retorno. "*-IR 189210-14.01.2025-#163685-JT
              MODIFY zsdt0213_integra      FROM w_zsdt0213_integra.

            WHEN 'EXCLUIR_ORDEM_VENDA'.
*-IR 189210-14.01.2025-#163685-JT-inicio
              /ui2/cl_json=>deserialize( EXPORTING json = lc_mensagem-ds_data_retorno
                                          CHANGING data = w_retorno_ov ).
              READ TABLE w_retorno_ov-errors-referenciaintegracaoordemvenda INTO _mensagem INDEX 1.
              IF sy-subrc = 0.
                l_mesg_retorno = _mensagem.
              ELSE.
                READ TABLE w_retorno_ov-errors-carregamento INTO _mensagem INDEX 1.
                IF sy-subrc = 0.
                  l_mesg_retorno = _mensagem.
                ENDIF.
              ENDIF.
*-IR 189210-14.01.2025-#163685-JT-fim

              me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst    = me->zif_trace_cotton~at_zseq_inst
                                                             i_objek        = me->zif_trace_cotton~at_objek
                                                             i_objecttable  = me->zif_trace_cotton~at_objecttable
                                                             i_id_contrato  = me->zif_trace_cotton~at_vbeln
                                                             i_nro_sol_ov   = me->zif_trace_cotton~at_nro_sol_ov
                                                             i_posnr        = me->zif_trace_cotton~at_posnr
                                                             i_tipo_integra = 'OV'
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg_retorno ).

              w_zsdt0213_integra-mandt        = sy-mandt.
              w_zsdt0213_integra-nro_sol_ov   = me->zif_trace_cotton~at_nro_sol_ov.
              w_zsdt0213_integra-posnr        = me->zif_trace_cotton~at_posnr.
              w_zsdt0213_integra-vbeln        = me->zif_trace_cotton~at_vbeln.
              w_zsdt0213_integra-metodo       = me->zif_integracao_inject~at_info_request_http-ds_metodo.
              w_zsdt0213_integra-integrado    = abap_false.
              w_zsdt0213_integra-data_reg     = sy-datum.
              w_zsdt0213_integra-hora_reg     = sy-uzeit.
              w_zsdt0213_integra-user_reg     = sy-uname.
              w_zsdt0213_integra-tenta_envio  = 1.              "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-data_envio   = sy-datum.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-hora_envio   = sy-uzeit.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-user_envio   = sy-uname.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-mesg_retorno = l_mesg_retorno. "*-IR 189210-14.01.2025-#163685-JT
              MODIFY zsdt0213_integra      FROM w_zsdt0213_integra.

              UPDATE zsdt0213_trace   SET integrado  = abap_false
                                    WHERE nro_sol_ov = me->zif_trace_cotton~at_nro_sol_ov
                                      AND posnr      = me->zif_trace_cotton~at_posnr.

            WHEN 'RETORNO_TRACE' OR 'RETORNO_TRACE_ESTORNO'.
              LOOP AT me->zif_trace_cotton~at_zsdt0331 INTO w_zsdt0331 WHERE id_carga = me->zif_trace_cotton~at_id_carga.
                w_zsdt0331-selecionado    = abap_false.
                w_zsdt0331-enviado_trace  = abap_false.
                w_zsdt0331-mensagem_trace = l_mesg_retorno.
                MODIFY zsdt0331        FROM w_zsdt0331.
              ENDLOOP.

            WHEN 'ENVIAR_NOTAFISCAL' OR 'CANCELAR_NOTAFISCAL'.
              LOOP AT me->zif_trace_cotton~at_zsdt0330 INTO w_zsdt0330.
                me->zif_trace_cotton~set_gravar_log( EXPORTING i_id_carga     = w_zsdt0330-id_carga
                                                               i_matnr        = w_zsdt0330-matnr
                                                               i_werks        = w_zsdt0330-werks
                                                               i_lgort        = w_zsdt0330-lgort
                                                               i_acharg       = w_zsdt0330-acharg
                                                               i_safra        = w_zsdt0330-safra
                                                               i_tipo_integra = 'NF'
                                                               i_tipo_msg     = 'E'
                                                               i_mensagem     = l_mesg_retorno ).


                "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
                UPDATE zsdt0330 SET status_nf_enviada = abap_off
                 WHERE id_carga      EQ w_zsdt0330-id_carga
                   AND matnr         EQ w_zsdt0330-matnr
                   AND werks         EQ w_zsdt0330-werks
                   AND lgort         EQ w_zsdt0330-lgort
                   AND acharg        EQ w_zsdt0330-acharg
                   AND safra         EQ w_zsdt0330-safra
                   AND seq           EQ w_zsdt0330-seq.


*                w_zsdt0330-status_nf_enviada = abap_off.
*                MODIFY zsdt0330           FROM w_zsdt0330.
                "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---

              ENDLOOP.

            WHEN OTHERS.
              me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = me->zif_trace_cotton~at_zseq_inst
                                                             i_objek       = me->zif_trace_cotton~at_objek
                                                             i_objecttable = me->zif_trace_cotton~at_objecttable
                                                             i_id_contrato = me->zif_trace_cotton~at_id_contrato
                                                             i_nro_sol_ov  = me->zif_trace_cotton~at_nro_sol_ov
                                                             i_posnr       = me->zif_trace_cotton~at_posnr
                                                             i_tipo_msg    = 'E'
                                                             i_mensagem    = l_mesg_retorno ).
          ENDCASE.

          COMMIT WORK.

          RAISE EXCEPTION TYPE zcx_integracao
            EXPORTING
              textid = VALUE #( msgid = ex_integra->msgid
                                msgno = ex_integra->msgno
                                attr1 = CONV #( ex_integra->msgv1 )
                                attr2 = CONV #( ex_integra->msgv2 )
                                attr3 = CONV #( ex_integra->msgv3 )
                                attr4 = CONV #( ex_integra->msgv4 ) )
              msgid  = ex_integra->msgid
              msgno  = ex_integra->msgno
              msgty  = 'E'
              msgv1  = CONV #( ex_integra->msgv1 )
              msgv2  = CONV #( ex_integra->msgv2 )
              msgv3  = CONV #( ex_integra->msgv3 )
              msgv4  = CONV #( ex_integra->msgv4 ).

        CATCH zcx_error      INTO DATA(ex_error).    "  "
          l_error        = abap_true.
          l_mesg         = ex_error->msgv1 && '-' && ex_error->msgv2 && '-' && ex_error->msgv3 && '-' && ex_error->msgv4.
          l_mesg_retorno = me->zif_trace_cotton~set_tratar_mensagem( EXPORTING i_json_retorno = lc_mensagem
                                                                               i_mensagem     = l_mesg ).

          CASE w_metodo-metodo.
            WHEN 'CADASTRA_INSTRUCAO'.
              me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = CONV #( me->zif_trace_cotton~at_zsdt0045-zseq_inst )
                                                             i_objek       = CONV #( me->zif_trace_cotton~at_zsdt0045-objek )
                                                             i_objecttable = CONV #( me->zif_trace_cotton~at_zsdt0045-objecttable )
                                                             i_lgort       = CONV #( me->zif_trace_cotton~at_zsdt0045-charg )
                                                             i_tipo_msg    = 'E'
                                                             i_mensagem    = l_mesg_retorno ).

            WHEN 'CADASTRA_ORDEM_VENDA'.
*-IR 189210-14.01.2025-#163685-JT-inicio
              /ui2/cl_json=>deserialize( EXPORTING json = lc_mensagem-ds_data_retorno
                                          CHANGING data = w_retorno_ov ).
              READ TABLE w_retorno_ov-errors-referenciaintegracaoordemvenda INTO _mensagem INDEX 1.
              IF sy-subrc = 0.
                l_mesg_retorno = _mensagem.
              ELSE.
                READ TABLE w_retorno_ov-errors-carregamento INTO _mensagem INDEX 1.
                IF sy-subrc = 0.
                  l_mesg_retorno = _mensagem.
                ENDIF.
              ENDIF.
*-IR 189210-14.01.2025-#163685-JT-fim

              me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst    = me->zif_trace_cotton~at_zseq_inst
                                                             i_objek        = me->zif_trace_cotton~at_objek
                                                             i_objecttable  = me->zif_trace_cotton~at_objecttable
                                                             i_id_contrato  = me->zif_trace_cotton~at_zsdt0066-vbeln
                                                             i_nro_sol_ov   = me->zif_trace_cotton~at_nro_sol_ov
                                                             i_posnr        = me->zif_trace_cotton~at_posnr
                                                             i_tipo_integra = 'OV'
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg_retorno ).

              w_zsdt0213_integra-mandt        = sy-mandt.
              w_zsdt0213_integra-nro_sol_ov   = me->zif_trace_cotton~at_nro_sol_ov.
              w_zsdt0213_integra-posnr        = me->zif_trace_cotton~at_posnr.
              w_zsdt0213_integra-vbeln        = me->zif_trace_cotton~at_zsdt0066-vbeln.
              w_zsdt0213_integra-metodo       = me->zif_integracao_inject~at_info_request_http-ds_metodo.
              w_zsdt0213_integra-integrado    = abap_false.
              w_zsdt0213_integra-data_reg     = sy-datum.
              w_zsdt0213_integra-hora_reg     = sy-uzeit.
              w_zsdt0213_integra-user_reg     = sy-uname.
              w_zsdt0213_integra-tenta_envio  = 1.              "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-data_envio   = sy-datum.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-hora_envio   = sy-uzeit.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-user_envio   = sy-uname.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-mesg_retorno = l_mesg_retorno. "*-IR 189210-14.01.2025-#163685-JT
              MODIFY zsdt0213_integra      FROM w_zsdt0213_integra.

            WHEN 'EXCLUIR_ORDEM_VENDA'.
*-IR 189210-14.01.2025-#163685-JT-inicio
              /ui2/cl_json=>deserialize( EXPORTING json = lc_mensagem-ds_data_retorno
                                          CHANGING data = w_retorno_ov ).
              READ TABLE w_retorno_ov-errors-referenciaintegracaoordemvenda INTO _mensagem INDEX 1.
              IF sy-subrc = 0.
                l_mesg_retorno = _mensagem.
              ELSE.
                READ TABLE w_retorno_ov-errors-carregamento INTO _mensagem INDEX 1.
                IF sy-subrc = 0.
                  l_mesg_retorno = _mensagem.
                ENDIF.
              ENDIF.
*-IR 189210-14.01.2025-#163685-JT-fim

              me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst    = me->zif_trace_cotton~at_zseq_inst
                                                             i_objek        = me->zif_trace_cotton~at_objek
                                                             i_objecttable  = me->zif_trace_cotton~at_objecttable
                                                             i_id_contrato  = me->zif_trace_cotton~at_vbeln
                                                             i_nro_sol_ov   = me->zif_trace_cotton~at_nro_sol_ov
                                                             i_posnr        = me->zif_trace_cotton~at_posnr
                                                             i_tipo_integra = 'OV'
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg_retorno ).

              w_zsdt0213_integra-mandt        = sy-mandt.
              w_zsdt0213_integra-nro_sol_ov   = me->zif_trace_cotton~at_nro_sol_ov.
              w_zsdt0213_integra-posnr        = me->zif_trace_cotton~at_posnr.
              w_zsdt0213_integra-vbeln        = me->zif_trace_cotton~at_vbeln.
              w_zsdt0213_integra-metodo       = me->zif_integracao_inject~at_info_request_http-ds_metodo.
              w_zsdt0213_integra-integrado    = abap_false.
              w_zsdt0213_integra-data_reg     = sy-datum.
              w_zsdt0213_integra-hora_reg     = sy-uzeit.
              w_zsdt0213_integra-user_reg     = sy-uname.
              w_zsdt0213_integra-tenta_envio  = 1.              "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-data_envio   = sy-datum.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-hora_envio   = sy-uzeit.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-user_envio   = sy-uname.       "*-IR 189210-14.01.2025-#163685-JT
              w_zsdt0213_integra-mesg_retorno = l_mesg_retorno. "*-IR 189210-14.01.2025-#163685-JT
              MODIFY zsdt0213_integra      FROM w_zsdt0213_integra.

              UPDATE zsdt0213_trace   SET integrado  = abap_false
                                    WHERE nro_sol_ov = me->zif_trace_cotton~at_nro_sol_ov
                                      AND posnr      = me->zif_trace_cotton~at_posnr.

            WHEN 'RETORNO_TRACE' OR 'RETORNO_TRACE_ESTORNO'.
              LOOP AT me->zif_trace_cotton~at_zsdt0331 INTO w_zsdt0331 WHERE id_carga = me->zif_trace_cotton~at_id_carga.
                w_zsdt0331-selecionado    = abap_false.
                w_zsdt0331-enviado_trace  = abap_false.
                w_zsdt0331-mensagem_trace = l_mesg_retorno.
                MODIFY zsdt0331        FROM w_zsdt0331.
              ENDLOOP.

            WHEN 'ENVIAR_NOTAFISCAL' OR 'CANCELAR_NOTAFISCAL'.
              LOOP AT me->zif_trace_cotton~at_zsdt0330 INTO w_zsdt0330.
                me->zif_trace_cotton~set_gravar_log( EXPORTING i_id_carga     = w_zsdt0330-id_carga
                                                               i_matnr        = w_zsdt0330-matnr
                                                               i_werks        = w_zsdt0330-werks
                                                               i_lgort        = w_zsdt0330-lgort
                                                               i_acharg       = w_zsdt0330-acharg
                                                               i_safra        = w_zsdt0330-safra
                                                               i_tipo_integra = 'NF'
                                                               i_tipo_msg     = 'E'
                                                               i_mensagem     = l_mesg_retorno ).



                "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
                UPDATE zsdt0330 SET status_nf_enviada = abap_off
                 WHERE id_carga      EQ w_zsdt0330-id_carga
                   AND matnr         EQ w_zsdt0330-matnr
                   AND werks         EQ w_zsdt0330-werks
                   AND lgort         EQ w_zsdt0330-lgort
                   AND acharg        EQ w_zsdt0330-acharg
                   AND safra         EQ w_zsdt0330-safra
                   AND seq           EQ w_zsdt0330-seq.

*                w_zsdt0330-status_nf_enviada = abap_off.
*                MODIFY zsdt0330           FROM w_zsdt0330.

                "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---


              ENDLOOP.

            WHEN OTHERS.
              me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = me->zif_trace_cotton~at_zseq_inst
                                                             i_objek       = me->zif_trace_cotton~at_objek
                                                             i_objecttable = me->zif_trace_cotton~at_objecttable
                                                             i_id_contrato = me->zif_trace_cotton~at_id_contrato
                                                             i_nro_sol_ov  = me->zif_trace_cotton~at_nro_sol_ov
                                                             i_posnr       = me->zif_trace_cotton~at_posnr
                                                             i_tipo_msg    = 'E'
                                                             i_mensagem    = l_mesg_retorno ).
          ENDCASE.

          COMMIT WORK.

          RAISE EXCEPTION TYPE zcx_error
            EXPORTING
              textid = VALUE #( msgid = ex_error->msgid
                                msgno = ex_error->msgno
                                attr1 = CONV #( ex_error->msgv1 )
                                attr2 = CONV #( ex_error->msgv2 )
                                attr3 = CONV #( ex_error->msgv3 )
                                attr4 = CONV #( ex_error->msgv4 ) )
              msgid  = ex_error->msgid
              msgno  = ex_error->msgno
              msgty  = 'E'
              msgv1  = CONV #( ex_error->msgv1 )
              msgv2  = CONV #( ex_error->msgv2 )
              msgv3  = CONV #( ex_error->msgv3 )
              msgv4  = CONV #( ex_error->msgv4 ).
      ENDTRY.

      CHECK l_error = abap_false.

*---------------------------------------
*---- avalia retorno JSON
*---------------------------------------
      CASE w_metodo-metodo.

        WHEN 'CADASTRA_INSTRUCAO'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = lc_retorno ).
          IF lc_retorno-status IS INITIAL.
            l_mesg_retorno = 'Instrução integrada com Sucesso ao Trace Cotton'.

            me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = CONV #( me->zif_trace_cotton~at_zsdt0045-zseq_inst )
                                                           i_objek       = CONV #( me->zif_trace_cotton~at_zsdt0045-objek )
                                                           i_objecttable = CONV #( me->zif_trace_cotton~at_zsdt0045-objecttable )
                                                           i_lgort       = CONV #( me->zif_trace_cotton~at_zsdt0045-charg )
                                                           i_tipo_msg    = 'S'
                                                           i_metodo_http = CONV #( lc_integracao-ds_metodo ) "Projeto Reestruturação Algodao 2024
                                                           i_mensagem    = l_mesg_retorno ).

*           me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = me->zif_trace_cotton~at_zseq_inst
*                                                          i_objek       = me->zif_trace_cotton~at_objek
*                                                          i_objecttable = me->zif_trace_cotton~at_objecttable
*                                                          i_id_contrato = me->zif_trace_cotton~at_id_contrato
*                                                          i_tipo_msg    = 'S'
*                                                          i_mensagem    = l_mesg_retorno ).

            COMMIT WORK.
          ENDIF.

        WHEN 'EXCLUIR_INSTRUCAO'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = lc_retorno ).
          IF lc_retorno-status IS INITIAL.
            l_mesg_retorno = 'Instrução Excluída com Sucesso ao Trace Cotton'.

            me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = me->zif_trace_cotton~at_zseq_inst
                                                           i_objek       = me->zif_trace_cotton~at_objek
                                                           i_objecttable = me->zif_trace_cotton~at_objecttable
                                                           i_id_contrato = me->zif_trace_cotton~at_id_contrato
                                                           i_tipo_msg    = 'S'
                                                           i_metodo_http = CONV #( lc_integracao-ds_metodo ) "Projeto Reestruturação Algodao 2024
                                                           i_mensagem    = l_mesg_retorno ).

            COMMIT WORK.
          ENDIF.

        WHEN 'CADASTRA_ORDEM_VENDA'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = lc_retorno ).
          IF lc_retorno-status IS INITIAL.
            l_mesg_retorno = 'Ordem de Venda integrada com Sucesso ao Trace Cotton'.

            me->zif_trace_cotton~set_gravar_log( EXPORTING i_nro_sol_ov  = me->zif_trace_cotton~at_nro_sol_ov
                                                           i_posnr       = me->zif_trace_cotton~at_posnr
                                                           i_id_contrato = me->zif_trace_cotton~at_zsdt0066-vbeln
                                                           i_tipo_msg    = 'S'
                                                           i_metodo_http = CONV #( lc_integracao-ds_metodo ) "Projeto Reestruturação Algodao 2024
                                                           i_mensagem    = l_mesg_retorno ).

            w_zsdt0213_trace-mandt      = sy-mandt.
            w_zsdt0213_trace-nro_sol_ov = me->zif_trace_cotton~at_nro_sol_ov.
            w_zsdt0213_trace-posnr      = me->zif_trace_cotton~at_posnr.
            w_zsdt0213_trace-vbeln      = me->zif_trace_cotton~at_zsdt0066-vbeln.
            w_zsdt0213_trace-integrado  = abap_true.
            w_zsdt0213_trace-data_reg   = sy-datum.
            w_zsdt0213_trace-hora_reg   = sy-uzeit.
            w_zsdt0213_trace-user_reg   = sy-uname.
            MODIFY zsdt0213_trace    FROM w_zsdt0213_trace.

*-IR 189210-14.01.2025-#163685-JT-inicio
            w_zsdt0213_integra-mandt      = sy-mandt.
            w_zsdt0213_integra-nro_sol_ov = me->zif_trace_cotton~at_nro_sol_ov.
            w_zsdt0213_integra-posnr      = me->zif_trace_cotton~at_posnr.
            w_zsdt0213_integra-vbeln      = me->zif_trace_cotton~at_zsdt0066-vbeln.
            w_zsdt0213_integra-metodo     = me->zif_integracao_inject~at_info_request_http-ds_metodo.
            w_zsdt0213_integra-integrado  = abap_true.
            w_zsdt0213_integra-data_reg   = sy-datum.
            w_zsdt0213_integra-hora_reg   = sy-uzeit.
            w_zsdt0213_integra-user_reg   = sy-uname.
            w_zsdt0213_integra-tenta_envio = 1.
            w_zsdt0213_integra-data_envio = sy-datum.
            w_zsdt0213_integra-hora_envio = sy-uzeit.
            w_zsdt0213_integra-user_envio = sy-uname.
            MODIFY zsdt0213_integra    FROM w_zsdt0213_integra.

*           UPDATE zsdt0213_integra SET integrado  = abap_true
*                                 WHERE nro_sol_ov = me->zif_trace_cotton~at_nro_sol_ov
*                                   AND posnr      = me->zif_trace_cotton~at_posnr
*                                   AND vbeln      = me->zif_trace_cotton~at_zsdt0066-vbeln.
*-IR 189210-14.01.2025-#163685-JT-inicio

            COMMIT WORK.
          ENDIF.

        WHEN 'EXCLUIR_ORDEM_VENDA'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = lc_retorno ).
          IF lc_retorno-status IS INITIAL.
            l_mesg_retorno = 'Ordem de Venda Excluída com Sucesso ao Trace Cotton'.

            me->zif_trace_cotton~set_gravar_log( EXPORTING i_nro_sol_ov  = me->zif_trace_cotton~at_nro_sol_ov
                                                           i_posnr       = me->zif_trace_cotton~at_posnr
                                                           i_id_contrato = me->zif_trace_cotton~at_vbeln
                                                           i_tipo_msg    = 'S'
                                                           i_metodo_http = CONV #( lc_integracao-ds_metodo ) "Projeto Reestruturação Algodao 2024
                                                           i_mensagem    = l_mesg_retorno ).

            UPDATE zsdt0213_trace   SET integrado  = abap_false
                                  WHERE nro_sol_ov = me->zif_trace_cotton~at_nro_sol_ov
                                    AND posnr      = me->zif_trace_cotton~at_posnr.

*-IR 189210-14.01.2025-#163685-JT-inicio
            w_zsdt0213_integra-mandt      = sy-mandt.
            w_zsdt0213_integra-nro_sol_ov = me->zif_trace_cotton~at_nro_sol_ov.
            w_zsdt0213_integra-posnr      = me->zif_trace_cotton~at_posnr.
            w_zsdt0213_integra-vbeln      = me->zif_trace_cotton~at_zsdt0066-vbeln.
            w_zsdt0213_integra-metodo     = me->zif_integracao_inject~at_info_request_http-ds_metodo.
            w_zsdt0213_integra-integrado  = abap_true.
            w_zsdt0213_integra-data_reg   = sy-datum.
            w_zsdt0213_integra-hora_reg   = sy-uzeit.
            w_zsdt0213_integra-user_reg   = sy-uname.
            w_zsdt0213_integra-tenta_envio = 1.
            w_zsdt0213_integra-data_envio = sy-datum.
            w_zsdt0213_integra-hora_envio = sy-uzeit.
            w_zsdt0213_integra-user_envio = sy-uname.
            MODIFY zsdt0213_integra    FROM w_zsdt0213_integra.

*           UPDATE zsdt0213_integra SET integrado  = abap_true
*                                 WHERE nro_sol_ov = me->zif_trace_cotton~at_nro_sol_ov
*                                   AND posnr      = me->zif_trace_cotton~at_posnr
*                                   AND vbeln      = me->zif_trace_cotton~at_vbeln.
*-IR 189210-14.01.2025-#163685-JT-fim
            COMMIT WORK.
          ENDIF.

        WHEN 'CADASTRA_CONTRATO'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = lc_retorno ).
          IF lc_retorno-status IS INITIAL.

            UPDATE zsdt0143 SET t_cotton    = abap_true
                                t_data      = sy-datum
                                t_hora      = sy-uzeit
                                t_usuario   = sy-uname
                          WHERE id_contrato = me->zif_trace_cotton~at_id_contrato.

            l_mesg_retorno = 'Contrato integrado com Sucesso ao Trace Cotton'.

            me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = me->zif_trace_cotton~at_zseq_inst
                                                           i_objek       = me->zif_trace_cotton~at_objek
                                                           i_objecttable = me->zif_trace_cotton~at_objecttable
                                                           i_id_contrato = me->zif_trace_cotton~at_id_contrato
                                                           i_tipo_msg    = 'S'
                                                           i_metodo_http = CONV #( lc_integracao-ds_metodo ) "Projeto Reestruturação Algodao 2024
                                                           i_mensagem    = l_mesg_retorno ).
            COMMIT WORK.
          ENDIF.

        WHEN 'EXCLUIR_CONTRATO'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = lc_retorno ).
          IF lc_retorno-status IS INITIAL.
            UPDATE zsdt0143 SET cancelado    = abap_true
                                sts_con      = 'C'
                                c_usnam      = sy-uname
                                c_data_atual = sy-datum
                                c_hora_atual = sy-uzeit
                                t_cotton     = abap_true
                                t_usuario    = sy-uname
                                t_data       = sy-datum
                                t_hora       = sy-uzeit
                          WHERE id_contrato  = me->zif_trace_cotton~at_id_contrato
                            AND cancelado    = abap_false.

            l_mesg_retorno = 'Contrato Excluido com Sucesso ao Trace Cotton'.

            me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = me->zif_trace_cotton~at_zseq_inst
                                                           i_objek       = me->zif_trace_cotton~at_objek
                                                           i_objecttable = me->zif_trace_cotton~at_objecttable
                                                           i_id_contrato = me->zif_trace_cotton~at_id_contrato
                                                           i_tipo_msg    = 'S'
                                                           i_metodo_http = CONV #( lc_integracao-ds_metodo ) "Projeto Reestruturação Algodao 2024
                                                           i_mensagem    = l_mesg_retorno ).
            COMMIT WORK.
          ENDIF.

        WHEN 'RETORNO_TRACE' OR 'RETORNO_TRACE_ESTORNO'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = lc_retorno ).
          IF lc_retorno-status IS INITIAL.
            LOOP AT me->zif_trace_cotton~at_zsdt0331 INTO w_zsdt0331 WHERE id_carga = me->zif_trace_cotton~at_id_carga.
              w_zsdt0331-selecionado   = abap_true.
              w_zsdt0331-enviado_trace = abap_true.
              MODIFY zsdt0331       FROM w_zsdt0331.
            ENDLOOP.

            COMMIT WORK.
          ENDIF.

        WHEN 'ENVIAR_NOTAFISCAL' OR 'CANCELAR_NOTAFISCAL'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = lc_retorno ).
          IF lc_retorno-status IS INITIAL.
            l_mesg_retorno = 'Nota Fiscal integrada com Sucesso ao Trace Cotton'.

            LOOP AT me->zif_trace_cotton~at_zsdt0330 INTO w_zsdt0330.
              me->zif_trace_cotton~set_gravar_log( EXPORTING i_id_carga     = w_zsdt0330-id_carga
                                                             i_matnr        = w_zsdt0330-matnr
                                                             i_werks        = w_zsdt0330-werks
                                                             i_lgort        = w_zsdt0330-lgort
                                                             i_acharg       = w_zsdt0330-acharg
                                                             i_safra        = w_zsdt0330-safra
                                                             i_tipo_integra = 'NF'
                                                             i_tipo_msg     = 'S'
                                                             i_metodo_http  = CONV #( lc_integracao-ds_metodo ) "Projeto Reestruturação Algodao 2024
                                                             i_mensagem     = l_mesg_retorno ).


              "SD - Ganho Peso Automatico Algodao US #145369 - WPP --->>
              UPDATE zsdt0330 SET status_nf_enviada = abap_true
               WHERE id_carga      EQ w_zsdt0330-id_carga
                 AND matnr         EQ w_zsdt0330-matnr
                 AND werks         EQ w_zsdt0330-werks
                 AND lgort         EQ w_zsdt0330-lgort
                 AND acharg        EQ w_zsdt0330-acharg
                 AND safra         EQ w_zsdt0330-safra
                 AND seq           EQ w_zsdt0330-seq.

*              w_zsdt0330-status_nf_enviada = abap_true.
*              MODIFY zsdt0330           FROM w_zsdt0330.
              "SD - Ganho Peso Automatico Algodao US #145369 - WPP <<---

            ENDLOOP.

            COMMIT WORK.
          ENDIF.
      ENDCASE.

    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_formata_data.
  ENDMETHOD.


  METHOD zif_trace_cotton~set_gravar_log.

    DATA: w_zsdt0327 TYPE zsdt0327,
          l_seq      TYPE timestampl.

    r_if_trace_cotton = me.

*-------------------------------
*-- grava log
*-------------------------------
    CLEAR w_zsdt0327.

    IF i_tipo_integra  IS NOT INITIAL.
      w_zsdt0327-tipo_integra    = i_tipo_integra.
    ELSEIF i_zseq_inst IS NOT INITIAL AND i_id_contrato IS     INITIAL AND i_nro_sol_ov IS     INITIAL.
      w_zsdt0327-tipo_integra    = 'IN'.
    ELSEIF i_zseq_inst IS     INITIAL AND i_id_contrato IS NOT INITIAL AND i_nro_sol_ov IS     INITIAL.
      w_zsdt0327-tipo_integra    = 'CO'.
    ELSEIF i_zseq_inst IS     INITIAL AND i_id_contrato IS     INITIAL AND i_nro_sol_ov IS NOT INITIAL.
      w_zsdt0327-tipo_integra    = 'OV'.
    ELSEIF i_id_carga  IS NOT INITIAL.
      w_zsdt0327-tipo_integra    = 'FB'.
    ELSE.
      EXIT.
    ENDIF.

    DO.
      GET TIME STAMP FIELD l_seq.

      SELECT SINGLE seq
              INTO @DATA(_seq)
              FROM zsdt0327
             WHERE zseq_inst   = @i_zseq_inst
               AND objek       = @i_objek
               AND objecttable = @i_objecttable
               AND id_contrato = @i_id_contrato
               AND nro_sol_ov  = @i_nro_sol_ov
               AND posnr       = @i_posnr
               AND id_carga    = @i_id_carga
               AND matnr       = @i_matnr
               AND werks       = @i_werks
               AND lgort       = @i_lgort
               AND acharg      = @i_acharg
               AND safra       = @i_safra
               AND seq         = @l_seq.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.

    w_zsdt0327-mandt             = sy-mandt.
    w_zsdt0327-zseq_inst         = i_zseq_inst.
    w_zsdt0327-objek             = i_objek.
    w_zsdt0327-objecttable       = i_objecttable.
    w_zsdt0327-id_contrato       = i_id_contrato.
    w_zsdt0327-nro_sol_ov        = i_nro_sol_ov.
    w_zsdt0327-posnr             = i_posnr.
    w_zsdt0327-id_carga          = i_id_carga.
    w_zsdt0327-matnr             = i_matnr.
    w_zsdt0327-werks             = i_werks.
    w_zsdt0327-lgort             = i_lgort.
    w_zsdt0327-acharg            = i_acharg.
    w_zsdt0327-safra             = i_safra.
    w_zsdt0327-seq               = l_seq.
    w_zsdt0327-metodo_http       = i_metodo_http. "Projeto Reestrutuçao Algodao 2024
*   w_zsdt0327-tipo_msg          = COND #( WHEN i_tipo_msg = abap_off THEN 'S'
*                                                                     ELSE i_tipo_msg ).
    w_zsdt0327-mensagem          = i_mensagem.
    w_zsdt0327-usname            = sy-uname.
    w_zsdt0327-data              = sy-datum.
    w_zsdt0327-hora              = sy-uzeit.

    IF i_tipo_msg = abap_off.
      w_zsdt0327-tipo_msg = 'S'.
    ELSEIF i_mensagem CS 'Já existe uma Ordem de Venda'  AND w_zsdt0327-tipo_integra = 'OV'.
      w_zsdt0327-tipo_msg        = 'S'.
    ELSE.
      w_zsdt0327-tipo_msg        = i_tipo_msg.
    ENDIF.

    MODIFY zsdt0327           FROM w_zsdt0327.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_gravar_retorno.

    DATA: w_zsdt0331 TYPE zsdt0331,
          l_seq      TYPE timestampl.

    r_if_trace_cotton = me.

    GET TIME STAMP FIELD l_seq.

*-------------------------------
*-- grava log
*-------------------------------
    CLEAR w_zsdt0331.

    w_zsdt0331-mandt             = sy-mandt.
    w_zsdt0331-id_carga          = i_retorno-id_carga.
    w_zsdt0331-matnr             = i_retorno-matnr.
    w_zsdt0331-werks             = i_retorno-werks.
    w_zsdt0331-lgort             = i_retorno-lgort.
    w_zsdt0331-cd_sai            = i_retorno-cd_sai.
    w_zsdt0331-safra             = i_retorno-safra.
    w_zsdt0331-seq               = l_seq.
    w_zsdt0331-tipo_operacao     = i_tipo_operacao.
    w_zsdt0331-mensagem          = i_retorno-mensagem.
    w_zsdt0331-usname            = sy-uname.
    w_zsdt0331-data              = sy-datum.
    w_zsdt0331-hora              = sy-uzeit.

    MODIFY zsdt0331           FROM w_zsdt0331.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_header.
  ENDMETHOD.


  METHOD zif_trace_cotton~set_id_referencia.

    "Incluir Chave insumos
    r_if_trace_cotton = me.
    me->zif_trace_cotton~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_trace_cotton~set_json_criar_contrato.

    DATA: w_json       TYPE zsde0044,
          w_kna1       TYPE kna1,
          w_kna1_final TYPE kna1,
          w_adrc       TYPE adrc,
          w_mara       TYPE mara,
          l_safra(2)   TYPE c.

    r_if_trace_cotton = me.

    CLEAR: w_kna1, w_mara, w_adrc.

    SELECT SINGLE *
      FROM kna1
      INTO w_kna1
     WHERE kunnr = me->zif_trace_cotton~at_zsdt0143-cliente.

    SELECT SINGLE *
      FROM kna1
      INTO w_kna1_final
     WHERE kunnr = me->zif_trace_cotton~at_zsdt0143-cliente_final.

    SELECT SINGLE *
      FROM adrc
      INTO w_adrc
     WHERE addrnumber  = w_kna1-adrnr
       AND date_to    >= sy-datum.

    SELECT SINGLE *
      FROM mara
      INTO w_mara
     WHERE matnr = me->zif_trace_cotton~at_zsdt0143-tipo_padrao.

*---------------------------
*-- monta jsoncontrato
*---------------------------
    w_json-referenciaintegracaocontrato = me->zif_trace_cotton~at_zsdt0143-id_contrato.
    w_json-referenciaintegracaoempresa  = me->zif_trace_cotton~at_zsdt0143-empresa.
    w_json-referenciaintegracaocliente  = me->zif_trace_cotton~at_zsdt0143-cliente.
    w_json-contrato                     = me->zif_trace_cotton~at_zsdt0143-contrato.
    w_json-codigoacts                   = me->zif_trace_cotton~at_zsdt0143-acts.  "*-CS2023000189-04.04.2023-#108693-JT
    w_json-nomecliente                  = COND #( WHEN w_adrc IS NOT INITIAL THEN w_adrc-name1 && w_adrc-name2
                                                                             ELSE w_kna1-name1 ).

    IF w_kna1-stcd1 IS NOT INITIAL.
      w_json-cnpj                       = w_kna1-stcd1.
    ELSE.
      w_json-cnpj                       = w_kna1-stcd2.
    ENDIF.

    w_json-classificacao                = w_mara-normt.

    l_safra                             = me->zif_trace_cotton~at_zsdt0143-safra+2(2) - 1.

    CONCATENATE l_safra '/' me->zif_trace_cotton~at_zsdt0143-safra+2(2)
           INTO w_json-safra.
    CONCATENATE me->zif_trace_cotton~at_zsdt0143-de_embarque+6(2)  '/'
                me->zif_trace_cotton~at_zsdt0143-de_embarque+4(2)  '/'
                me->zif_trace_cotton~at_zsdt0143-de_embarque(4)
           INTO w_json-datainicial.
    CONCATENATE me->zif_trace_cotton~at_zsdt0143-ate_embarque+6(2) '/'
                me->zif_trace_cotton~at_zsdt0143-ate_embarque+4(2) '/'
                me->zif_trace_cotton~at_zsdt0143-ate_embarque(4)
           INTO w_json-datafinal.

    w_json-quantidade                   = me->zif_trace_cotton~at_zsdt0143-quatidade / 1000.
    w_json-codclientefinal = me->zif_trace_cotton~at_zsdt0143-cliente_final.
    w_json-nomeclientefinal = w_kna1_final-name1.
    w_json-intercompany     = me->zif_trace_cotton~at_zsdt0143-intercompany.

*-----------------------------------
*-- monta JSON
*-----------------------------------
    zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_json
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.


  METHOD zif_trace_cotton~set_json_criar_docto.

    DATA: w_json       TYPE zsde0037,
          w_0143       TYPE zsdt0143,
          l_safra1     TYPE ajahr,
          l_safra2     TYPE ajahr,
          l_observacao TYPE char100.

    r_if_trace_cotton = me.

    CLEAR w_0143.

    SELECT SINGLE *
      FROM zsdt0143
      INTO w_0143
     WHERE contrato   = me->zif_trace_cotton~at_zsdt0045-contrato
       AND safra      = me->zif_trace_cotton~at_zsdt0045-safra
       AND empresa    = me->zif_trace_cotton~at_zsdt0045-bukrs
       AND cancelado <> abap_true.

    IF sy-subrc <> 0.
      me->zif_trace_cotton~set_mensagem( '02' ).
    ENDIF.

    l_safra1 = me->zif_trace_cotton~at_zsdt0045-safra - 1.
    l_safra2 = me->zif_trace_cotton~at_zsdt0045-safra.

*---------------------------
*-- instrucao
*---------------------------
    w_json-instrucao-referenciaintegracaocliente = w_0143-cliente.
    w_json-instrucao-volumelimite                = me->zif_trace_cotton~at_zsdt0045-peso_max.
    w_json-instrucao-datainicio                  = me->zif_trace_cotton~at_zsdt0045-data_in_porto+6(2) && '/' &&
                                                   me->zif_trace_cotton~at_zsdt0045-data_in_porto+4(2) && '/' &&
                                                   me->zif_trace_cotton~at_zsdt0045-data_in_porto(4).
    w_json-instrucao-datatermino                 = me->zif_trace_cotton~at_zsdt0045-data_porto+6(2)    && '/' &&
                                                   me->zif_trace_cotton~at_zsdt0045-data_porto+4(2)    && '/' &&
                                                   me->zif_trace_cotton~at_zsdt0045-data_porto(4).
    w_json-instrucao-safra                       = l_safra1+2(2) && '/' && l_safra2+2(2).
    w_json-instrucao-codigo                      = me->zif_trace_cotton~at_zsdt0045-instrucao.
    w_json-instrucao-terminalestufagem           = me->zif_trace_cotton~at_zsdt0045-terminal_estuf.
*   w_json-instrucao-observacao                  = me->zif_trace_cotton~at_zsdt0045-observacao.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = me->zif_trace_cotton~at_zsdt0045-observacao
      IMPORTING
        outtext           = w_json-instrucao-observacao
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.

*---------------------------
*-- lote
*---------------------------
    w_json-lote-referenciaintegracao             = me->zif_trace_cotton~at_zsdt0045-zseq_inst.
    w_json-lote-safra                            = w_json-instrucao-safra.
    w_json-lote-codigofazenda                    = me->zif_trace_cotton~at_zsdt0045-werks.
    w_json-lote-numero                           = me->zif_trace_cotton~at_zsdt0045-charg.
    w_json-lote-quantidadefardinhosreservados    = me->zif_trace_cotton~at_zsdt0045-quantidade.

*-----------------------------------
*-- monta JSON
*-----------------------------------
    zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_json
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    REPLACE ALL OCCURRENCES OF '"volumelimite":0' IN zif_trace_cotton~at_json WITH '"volumelimite":null'.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_json_criar_ordem_venda.

    TYPES: BEGIN OF ty_0213.
             INCLUDE TYPE zsdt0213.
    TYPES:   charg TYPE zsdt0045-charg.
    TYPES: END   OF ty_0213.

    DATA: w_json  TYPE zsde0048,
          w_lotes TYPE zsde0049,
          t_lotes TYPE zsde0049_t,
          w_0045  TYPE zsdt0045,
          w_mara  TYPE mara,
          t_0045  TYPE TABLE OF zsdt0045,
          t_0328  TYPE TABLE OF zsdt0328,
          t_0213  TYPE TABLE OF ty_0213,
          w_0328  TYPE zsdt0328,
          w_0213  TYPE ty_0213.

    r_if_trace_cotton = me.

    FREE: w_json, w_mara, t_lotes, w_0045, t_0328, t_0213, t_0045.

    SELECT SINGLE *
      FROM zsdt0066
      INTO me->zif_trace_cotton~at_zsdt0066
     WHERE nro_sol_ov    = me->zif_trace_cotton~at_nro_sol_ov
       AND posnr         = me->zif_trace_cotton~at_posnr
       AND status        = 'L'.  "*-CS2023000189-04.09.2023-#122555-JT

    IF ( sy-subrc <> 0 ) OR
       ( sy-subrc  = 0  AND me->zif_trace_cotton~at_zsdt0066-vbeln IS INITIAL ).
*------------------------------
*---- OV ZFEX
*------------------------------
      SELECT SINGLE *
        FROM zsdt0053
        INTO CORRESPONDING FIELDS OF me->zif_trace_cotton~at_zsdt0066
       WHERE nro_sol_ov    = me->zif_trace_cotton~at_nro_sol_ov
         AND posnr         = me->zif_trace_cotton~at_posnr.

      SELECT SINGLE auart
               INTO @DATA(l_auart)
               FROM vbak
              WHERE vbeln = @me->zif_trace_cotton~at_zsdt0066-vbeln.

      IF sy-subrc = 0 AND l_auart <> 'ZFEX'.
        me->zif_trace_cotton~set_mensagem( '06' ).
      ENDIF.

      SELECT *
        FROM zsdt0213
        INTO TABLE t_0213
       WHERE nro_sol_ov   = me->zif_trace_cotton~at_zsdt0066-nro_sol_ov
         AND posnr        = me->zif_trace_cotton~at_zsdt0066-posnr
         AND status       NE 'X'.

      IF sy-subrc <> 0.
        me->zif_trace_cotton~set_mensagem( '05' ).
      ENDIF.

      LOOP AT t_0213   INTO w_0213.
        w_0213-charg      = w_0213-lgort.
        MODIFY t_0213  FROM w_0213 INDEX sy-tabix.
      ENDLOOP.

      SELECT *
        FROM zsdt0045
        INTO TABLE t_0045
         FOR ALL ENTRIES IN t_0213
       WHERE objek       = me->zif_trace_cotton~at_zsdt0066-nro_sol_ov
         AND objecttable = 'ZSDT0051'
         AND instrucao   = me->zif_trace_cotton~at_zsdt0066-instrucao
         AND charg       = t_0213-charg.

      READ TABLE t_0045 INTO w_0045 INDEX 1.

      IF sy-subrc <> 0.
        me->zif_trace_cotton~set_mensagem( '05' ).
      ENDIF.

      me->zif_trace_cotton~at_zsdt0045 = w_0045.

    ELSE.
*------------------------------
*---- buscar lote/instrucao
*------------------------------
      SELECT SINGLE *
        FROM zsdt0045
        INTO w_0045
       WHERE objek       = me->zif_trace_cotton~at_zsdt0066-nro_sol_ov
         AND objecttable = 'ZSDT0051'
         AND matnr       = me->zif_trace_cotton~at_zsdt0066-matnr
         AND werks       = me->zif_trace_cotton~at_zsdt0066-werks
         AND safra       = me->zif_trace_cotton~at_zsdt0066-charg
         AND charg       = me->zif_trace_cotton~at_zsdt0066-charg_ori.

      IF sy-subrc <> 0.
        SELECT *
          FROM zsdt0045
          INTO TABLE t_0045
         WHERE objek       = me->zif_trace_cotton~at_zsdt0066-nro_sol_ov
           AND objecttable = 'ZSDT0051'
           AND matnr       = me->zif_trace_cotton~at_zsdt0066-matnr
           AND werks       = me->zif_trace_cotton~at_zsdt0066-werks
           AND safra       = me->zif_trace_cotton~at_zsdt0066-charg
           AND instrucao   = me->zif_trace_cotton~at_zsdt0066-instrucao.

        SORT t_0045 BY zseq_inst DESCENDING.
        READ TABLE t_0045 INTO w_0045 INDEX 1.
      ENDIF.

      IF sy-subrc <> 0.
        me->zif_trace_cotton~set_mensagem( '05' ).
      ENDIF.

      me->zif_trace_cotton~at_zsdt0045 = w_0045.

      SELECT *
        FROM zsdt0328
        INTO TABLE t_0328
       WHERE nro_sol_ov = me->zif_trace_cotton~at_zsdt0066-nro_sol_ov
         AND posnr      = me->zif_trace_cotton~at_zsdt0066-posnr
         AND cancelado  = abap_off.  "*-CS2023000189-04.09.2023-#122555-JT
    ENDIF.

*---------------------------
*-- material
*---------------------------
    SELECT SINGLE *
      FROM mara
      INTO w_mara
     WHERE matnr   = me->zif_trace_cotton~at_zsdt0066-matnr.

*---------------------------
*-- requisicao OV
*---------------------------
    w_json-referenciaintegracaoordemvenda = me->zif_trace_cotton~at_zsdt0066-vbeln.
    w_json-numeroordemvenda               = me->zif_trace_cotton~at_zsdt0066-vbeln.
    w_lotes-classificacao                 = w_mara-normt.

    IF     t_0213[] IS NOT INITIAL.
      LOOP AT t_0045  INTO w_0045.
        w_lotes-referenciaintegracao      = w_0045-zseq_inst.
        APPEND w_lotes  TO t_lotes.
      ENDLOOP.
    ELSEIF t_0328[] IS NOT INITIAL.
      LOOP AT t_0328  INTO w_0328.
        w_lotes-referenciaintegracao      = w_0328-zseq_inst.
        APPEND w_lotes  TO t_lotes.
      ENDLOOP.
    ELSE.
      w_lotes-referenciaintegracao        = w_0045-zseq_inst.
      APPEND w_lotes  TO t_lotes.
    ENDIF.

    w_json-instrucaolotes[] = t_lotes[].

*-----------------------------------
*-- monta JSON
*-----------------------------------
    zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_json
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.


  METHOD zif_trace_cotton~set_json_exclui_contrato.

    DATA: w_json     TYPE zsde0045,
          l_safra(2) TYPE c.

    r_if_trace_cotton = me.

    l_safra                     =  me->zif_trace_cotton~at_zsdt0143-safra+2(2) - 1.

*---------------------------
*-- monta jsoncontrato
*---------------------------
    w_json-contrato             =  me->zif_trace_cotton~at_zsdt0143-contrato.
    w_json-referenciaintegracao =  me->zif_trace_cotton~at_zsdt0143-id_contrato.
    CONCATENATE l_safra '/' me->zif_trace_cotton~at_zsdt0143-safra+2(2)
           INTO w_json-safra.

*-----------------------------------
*-- monta JSON
*-----------------------------------
    zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_json
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.


  METHOD zif_trace_cotton~set_json_exclui_docto.

    DATA: w_json   TYPE zsde0046,
          l_safra1 TYPE ajahr,
          l_safra2 TYPE ajahr.

    r_if_trace_cotton = me.

    l_safra1 = me->zif_trace_cotton~at_zsdt0045-safra - 1.
    l_safra2 = me->zif_trace_cotton~at_zsdt0045-safra.

*---------------------------
*-- instrucao
*---------------------------
    w_json-referenciaintegracao = me->zif_trace_cotton~at_zsdt0045-zseq_inst.
    w_json-safra                = l_safra1+2(2) && '/' && l_safra2+2(2).

*-----------------------------------
*-- monta JSON
*-----------------------------------
    zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_json
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.


  METHOD zif_trace_cotton~set_json_exclui_ordem_venda.

    DATA: w_json TYPE zsde0050,
          w_0045 TYPE zsdt0045.

    r_if_trace_cotton = me.

    FREE: w_json, w_0045.

*------------------------------
* buscar lote/instrucao
*------------------------------
    SELECT SINGLE *
      FROM zsdt0045
      INTO w_0045
     WHERE objek       = me->zif_trace_cotton~at_zsdt0066-nro_sol_ov
       AND objecttable = 'ZSDT0051'
       AND matnr       = me->zif_trace_cotton~at_zsdt0066-matnr
       AND werks       = me->zif_trace_cotton~at_zsdt0066-werks
       AND safra       = me->zif_trace_cotton~at_zsdt0066-charg
       AND charg       = me->zif_trace_cotton~at_zsdt0066-charg_ori.

    IF sy-subrc <> 0.
      me->zif_trace_cotton~set_mensagem( '05' ).
    ENDIF.

*---------------------------
*-- requisicao OV
*---------------------------
    w_json-referenciaintegracaoinstrucaol = w_0045-zseq_inst.
    w_json-referenciaintegracaoordemvenda = me->zif_trace_cotton~at_vbeln.

*-----------------------------------
*-- monta JSON
*-----------------------------------
    zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_json
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    REPLACE ALL OCCURRENCES OF '"referenciaintegracaoinstrucaol"' IN zif_trace_cotton~at_json WITH '"referenciaIntegracaoInstrucaoLote"'.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_mensagem.

    DATA: l_mesg TYPE string.

    r_if_trace_cotton = me.

    CASE i_cod_msg.
      WHEN '01'.
        l_mesg = 'Interno SAP: Instrução não foi localizada.'.
      WHEN '02'.
        l_mesg = 'Interno SAP: Contrato não foi encontrado!'.
      WHEN '03'.
        l_mesg = 'Interno SAP: Contrato (ZSDT0118) não foi encontrado!'.
      WHEN '04'.
        l_mesg = 'Interno SAP: Ordem de VEnda (ZSDT0066) não foi encontrado!'.
      WHEN '05'.
        l_mesg = 'Interno SAP: Não encontrada Instrução para a Ordem de Venda!'.
      WHEN '06'.
        l_mesg = 'Interno SAP: Ordem Venda não é ZFEX!'.
    ENDCASE.

    me->zif_trace_cotton~set_gravar_log( EXPORTING i_zseq_inst   = me->zif_trace_cotton~at_zseq_inst
                                                   i_objek       = me->zif_trace_cotton~at_objek
                                                   i_objecttable = me->zif_trace_cotton~at_objecttable
                                                   i_id_contrato = me->zif_trace_cotton~at_id_contrato
                                                   i_nro_sol_ov  = me->zif_trace_cotton~at_nro_sol_ov
                                                   i_posnr       = me->zif_trace_cotton~at_posnr
                                                   i_tipo_msg    = 'E'
                                                   i_mensagem    = l_mesg ).

    RAISE EXCEPTION TYPE zcx_error
      EXPORTING
        textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                          msgno = zcx_error=>zcx_erro_geral-msgno
                          attr1 = CONV #( l_mesg ) )
        msgid  = zcx_error=>zcx_erro_geral-msgid
        msgno  = zcx_error=>zcx_erro_geral-msgno
        msgty  = 'E'
        msgv1  = CONV #( l_mesg ).

  ENDMETHOD.


  METHOD zif_trace_cotton~set_receive_carregamento.

    DATA: t_data_inbound       TYPE zsdt0330_trace_t,
          w_data_inbound       TYPE zsdt0330_trace,
          t_zsdt0330           TYPE TABLE OF zsdt0330,
          w_zsdt0330           TYPE zsdt0330,
          w_instrucoesembarque TYPE zsde0065,
          w_ordemvenda         TYPE zsde0052,
          w_lotes              TYPE zsde0053,
          w_fardinhos          TYPE zsde0055,
          w_retorno            TYPE zsdt0331,
          l_tabix              TYPE sy-tabix,
          l_seq                TYPE timestampl.

    r_if_trace_cotton = me.

    me->zif_trace_cotton~at_info_request_http = i_at_info_request_http.

    FREE: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = me->zif_trace_cotton~at_data_inbound ).
    ENDIF.

*--------------------------------
*   construir tabela
*--------------------------------
    FREE: t_zsdt0330, w_zsdt0330.

    TRANSLATE me->zif_trace_cotton~at_data_inbound-carregamentoid TO UPPER CASE.
    CONDENSE me->zif_trace_cotton~at_data_inbound-carregamentoid.

    w_zsdt0330-id_carga         = me->zif_trace_cotton~at_data_inbound-carregamentoid.
    w_zsdt0330-peso_bruto_carga = me->zif_trace_cotton~at_data_inbound-pesobruto.
    w_zsdt0330-peso_liq_carga   = me->zif_trace_cotton~at_data_inbound-pesoliquido.
    w_zsdt0330-placa_cav        = me->zif_trace_cotton~at_data_inbound-veiculoplaca.
    w_zsdt0330-cpf_motorista    = me->zif_trace_cotton~at_data_inbound-motoristacpf.
    w_zsdt0330-qtd_fardos_carga = me->zif_trace_cotton~at_data_inbound-quantidadefardinhos.

    LOOP AT me->zif_trace_cotton~at_data_inbound-instrucoesembarque INTO w_instrucoesembarque.
      w_zsdt0330-vbeln          = |{ w_instrucoesembarque-ordemvenda-numero ALPHA = IN }|.
      w_zsdt0330-nr_romaneio    = w_instrucoesembarque-ordemvenda-romaneio-numero.

*-----------------------------
*---- busca ch referencia
*-----------------------------
      SELECT SINGLE  ch_referencia
        INTO @DATA(l_ch_referencia)
        FROM zsdt0001
       WHERE tp_movimento = 'S'
         AND nr_romaneio  = @w_zsdt0330-nr_romaneio
         AND vbeln        = @w_zsdt0330-vbeln.

      IF sy-subrc EQ 0.
        w_zsdt0330-ch_referencia = l_ch_referencia. "w_instrucoesembarque-ordemvenda-referenciaintegracao.
      ELSE.
        CLEAR: w_zsdt0330-ch_referencia.
      ENDIF.

      w_zsdt0330-kunnr          = |{ w_instrucoesembarque-cliente-referenciaintegracao ALPHA = IN }|.

      LOOP AT w_instrucoesembarque-ordemvenda-lotes INTO w_lotes.
        w_zsdt0330-lgort        = w_lotes-numero.
        w_zsdt0330-safra        = w_lotes-safraano.
        w_zsdt0330-normt        = w_lotes-classificacao.
*       w_zsdt0330-werks        = w_lotes-fazendabeneficiamento-codigo. " w_lotes-fazendaorigem-codigo.
        w_zsdt0330-werks        = COND #( WHEN w_lotes-fazendabeneficiamento-codigo(2) = w_lotes-fazendaorigem-codigo(2)
                                               THEN w_lotes-fazendabeneficiamento-codigo
                                               ELSE w_lotes-fazendaorigem-codigo ).

        LOOP AT w_lotes-fardinhos  INTO w_fardinhos.
          w_zsdt0330-cd_sai     = w_fardinhos-codigosai.
          w_zsdt0330-acharg     = w_fardinhos-numerofardocompleto.
          APPEND w_zsdt0330    TO t_zsdt0330.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    me->zif_trace_cotton~at_zsdt0330[] = t_zsdt0330[].

*--------------------------------
*   cancela mensagens anteriores
*--------------------------------
    UPDATE zsdt0331 SET cancelado     = abap_true
                  WHERE id_carga      = me->zif_trace_cotton~at_data_inbound-carregamentoid
                    AND tipo_operacao = abap_off.
    COMMIT WORK.

*--------------------------------
*   valida os dados
*--------------------------------
    e_msg_erro = me->zif_integracao_inbound~validar_dados_inbound( i_data_inbound = i_msg_inbound  ).

    IF e_msg_erro IS NOT INITIAL.
      e_sucesso      = abap_true.
      e_nm_code      = '200'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.

*--------------------------------------
*-- envio de retorno de erros ao trace
*--------------------------------------
      TRY .
          zcl_trace_cotton=>zif_trace_cotton~get_instance(
             )->set_retorno_trace( i_id_carga = w_zsdt0330-id_carga ).

        CATCH zcx_integracao INTO DATA(ex_integra).
        CATCH zcx_error      INTO DATA(ex_error).
      ENDTRY.

      RETURN.
    ENDIF.

*--------------------------------
*   verifica se ja houve carregamento para o mesmo ID.
*   se sim, cancela o antigo e registra o novo
*--------------------------------
    UPDATE zsdt0330 SET cancelado = abap_true
                  WHERE id_carga  = me->zif_trace_cotton~at_data_inbound-carregamentoid.
    COMMIT WORK.

*--------------------------------------
*-- Processar
*--------------------------------------

    "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Ini
    GET TIME STAMP FIELD l_seq. "Gravar um mesmo SEQ para todos os registros, para identificar os registroso recebidos nessa requisição
    "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim

    LOOP AT me->zif_trace_cotton~at_zsdt0330 ASSIGNING FIELD-SYMBOL(<fs_zsdt0330>).

      l_tabix = sy-tabix.

      MOVE-CORRESPONDING <fs_zsdt0330> TO w_retorno.

*----------------------------------------
*---- atualiza tabela
*----------------------------------------
      "GET TIME STAMP FIELD l_seq.     "Manutenção Ganho/Perda Peso Algodão -Carregamento Automatico

      <fs_zsdt0330>-mandt               = sy-mandt.
      <fs_zsdt0330>-seq                 = l_seq.
      <fs_zsdt0330>-status_fardo        = '0'.
      <fs_zsdt0330>-status_gera_lote    = '0'.
      <fs_zsdt0330>-chave_referencia    = <fs_zsdt0330>-matnr.
      <fs_zsdt0330>-user_carga          = sy-uname.
      <fs_zsdt0330>-data_carga          = sy-datum.
      <fs_zsdt0330>-hora_carga          = sy-uzeit.
      MODIFY zsdt0330             FROM <fs_zsdt0330>.

      COMMIT WORK AND WAIT.

    ENDLOOP.

    me->zif_trace_cotton~add_fila_gera_sobra_perda( i_zsdt0330_t = me->zif_trace_cotton~at_zsdt0330 ). "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Ini


*--------------------------------------
*-- Retorno processamento
*--------------------------------------
    SELECT SINGLE *
             FROM zsdt0331
             INTO @DATA(t_0331)
            WHERE id_carga      = @w_zsdt0330-id_carga
              AND selecionado   = @abap_false
              AND enviado_trace = @abap_false
              AND tipo_operacao = @abap_off
              AND cancelado     = @abap_false.

    IF sy-subrc = 0.
      e_sucesso      = abap_true.
      e_msg_erro     = 'Há Ocorrência de erros'.
      e_nm_code      = '200'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
    ELSE.
      e_sucesso      = abap_true.
      e_nm_code      = '200'.
      e_msg_erro     = 'Ok'.
      e_msg_outbound = ' { "protocolo" : "'   && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code                                             &&
                       '" '                   && cl_abap_char_utilities=>newline &&
                       ' }'.
    ENDIF.

*--------------------------------------
*-- envio de retorno de erros ao trace
*--------------------------------------
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_retorno_trace( i_id_carga = w_zsdt0330-id_carga ).

      CATCH zcx_integracao INTO ex_integra.
      CATCH zcx_error      INTO ex_error.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_retorno_trace.

    DATA: t_zsdt0331_grp TYPE TABLE OF zsdt0331,
          w_zsdt0331_grp TYPE zsdt0331,
          w_zsdt0331     TYPE zsdt0331,
          w_retorno      TYPE zsdt0330_retorno,
          w_ret_estorno  TYPE zsdt0330_ret_estorno,
          w_erros        TYPE zsde0058,
          w_err_estorno  TYPE zsde0090,
          l_metodo       TYPE string,
          l_error1       TYPE char1,
          l_error2       TYPE char1,
          r_id_carga     TYPE RANGE OF zsdt0330-id_carga,
          l_id_carga     LIKE LINE OF r_id_carga.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_trace_cotton                     = me.
    me->zif_trace_cotton~at_tp_referencia = 'TRACE COTTON-Retorno'.

*-------------------------------
*-- range
*-------------------------------
    FREE: me->zif_trace_cotton~at_zsdt0331, r_id_carga, l_error1, l_error2.

    IF i_id_carga IS NOT INITIAL.
      l_id_carga-sign     = 'I'.
      l_id_carga-option   = 'EQ'.
      l_id_carga-low      = i_id_carga.
      APPEND l_id_carga  TO r_id_carga.
    ENDIF.

*-------------------------------
*-- selecao fardos com erro
*-------------------------------
    SELECT *
      FROM zsdt0331
      INTO TABLE me->zif_trace_cotton~at_zsdt0331
     WHERE id_carga      IN r_id_carga
       AND selecionado    = abap_false
       AND enviado_trace  = abap_false
       AND tipo_operacao  = i_tipo_operacao
       AND cancelado      = abap_false.

    CHECK sy-subrc = 0.

    t_zsdt0331_grp[] = me->zif_trace_cotton~at_zsdt0331[].

    SORT t_zsdt0331_grp BY id_carga.
    DELETE ADJACENT DUPLICATES FROM t_zsdt0331_grp
                            COMPARING id_carga.

*-------------------------------
*-- status selecionado
*-------------------------------
    LOOP AT t_zsdt0331_grp INTO w_zsdt0331_grp.

      me->zif_trace_cotton~at_id_carga = w_zsdt0331_grp-id_carga.

      LOOP AT me->zif_trace_cotton~at_zsdt0331 INTO w_zsdt0331 WHERE id_carga = w_zsdt0331_grp-id_carga.
        w_zsdt0331-selecionado = abap_true.
        MODIFY zsdt0331     FROM w_zsdt0331.
      ENDLOOP.

      COMMIT WORK.

*-------------------------------
*---- monta json
*-------------------------------
      FREE: w_retorno, w_ret_estorno.

      IF i_tipo_operacao = abap_off. "Carregamento normal
        w_retorno-sucesso       = abap_false.

        LOOP AT me->zif_trace_cotton~at_zsdt0331 INTO w_zsdt0331 WHERE id_carga = w_zsdt0331_grp-id_carga.
          w_erros-codigo        = sy-tabix.
          w_erros-entidade      = 'Fardinho'.
          w_erros-identificador = w_zsdt0331-cd_sai.
          w_erros-mensagem      = w_zsdt0331-mensagem.
          APPEND w_erros       TO w_retorno-erros.
        ENDLOOP.

        me->zif_trace_cotton~at_retorno = w_retorno.

*-----------------------------------
*------ monta JSON
*-----------------------------------
        zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_retorno
                                                            pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

      ELSE.  "Estorno de Carregamento
        LOOP AT me->zif_trace_cotton~at_zsdt0331 INTO w_zsdt0331 WHERE id_carga = w_zsdt0331_grp-id_carga.
          IF w_zsdt0331-mensagem      = abap_off.
            w_ret_estorno-sucesso     = abap_true.
          ELSE.
            w_ret_estorno-sucesso     = abap_false.
            w_err_estorno-codigosai = w_zsdt0331-cd_sai.
            w_err_estorno-mensagem  = w_zsdt0331-mensagem.
            APPEND w_err_estorno   TO w_ret_estorno-erros.
          ENDIF.
        ENDLOOP.

        me->zif_trace_cotton~at_ret_estorno = w_ret_estorno.

*-----------------------------------
*------ monta JSON
*-----------------------------------
        zif_trace_cotton~at_json = /ui2/cl_json=>serialize( data        = w_ret_estorno
                                                            pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
      ENDIF.

*-------------------------------
*---- Executa API
*-------------------------------
      l_metodo = COND #( WHEN i_tipo_operacao = 'E' THEN 'RETORNO_TRACE_ESTORNO'
                                                    ELSE 'RETORNO_TRACE' ).

      TRY .
          zcl_trace_cotton=>zif_trace_cotton~get_instance(
             )->set_exec_trace( EXPORTING i_metodo = l_metodo ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          l_error1 = abap_true.

        CATCH zcx_error INTO DATA(ex_error).
          l_error2 = abap_true.

      ENDTRY.
    ENDLOOP.

*-------------------------------------
* envia excecao erro
*-------------------------------------
    CASE abap_true.

      WHEN l_error1.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).


      WHEN l_error2.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).

    ENDCASE.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_send_msg.

    TYPES BEGIN OF ty_retorno.
    TYPES: access_token TYPE string.
    TYPES: expires_in TYPE string.
    TYPES: token_type TYPE string.
    TYPES END OF ty_retorno.

    DATA: lc_integrar    TYPE REF TO zcl_integracao,
          lc_retorno     TYPE ty_retorno,
          l_access_token TYPE string,
          l_token_type   TYPE string,
          l_expires_in   TYPE string,
          l_force        TYPE char01.

    r_if_trace_cotton = me.

    FREE: e_mensagem.

    CREATE OBJECT lc_integrar.

    lc_integrar->zif_integracao~at_form_fields = me->zif_integracao_inject~at_form_fields.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg(      IMPORTING e_id_integracao  = e_id_integracao
      )->set_outbound_msg( IMPORTING e_mensagem       = e_mensagem
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro(     IMPORTING e_integracao     = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_tratar_mensagem.

    DATA: lc_retorno TYPE zsde0040,
          lc_json    TYPE string,
          l_campo    TYPE string,
          t_json     TYPE TABLE OF string.

    FREE: e_mensagem_retorno.

    /ui2/cl_json=>deserialize( EXPORTING json = i_json_retorno-ds_data_retorno
                               CHANGING  data = lc_retorno ).

    IF lc_retorno-status IS INITIAL.
      e_mensagem_retorno = i_mensagem.
      EXIT.
    ENDIF.

*----------------------------
*-- tratar msg da api
*----------------------------
    SPLIT i_json_retorno-ds_data_retorno AT '"' INTO TABLE t_json.

    LOOP AT t_json INTO DATA(w_json).
      IF     w_json    = 'errors' OR w_json    = ''  OR w_json    = 'title' OR w_json = 'status' OR
             w_json(1) = '{'      OR w_json(1) = ':' OR w_json(1) = '['     OR
             w_json(1) = '}'      OR w_json(1) = ',' OR w_json(1) = ']'.
        CONTINUE.
      ELSEIF w_json    = 'detail'.
        EXIT.
      ENDIF.
      e_mensagem_retorno = e_mensagem_retorno && w_json && '|'.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_trace_cotton~set_validar_dados_estorno.

    TYPES: BEGIN OF ty_vbrp,
             vbeln  TYPE vbrp-vbeln,
             vgbel  TYPE vbrp-vgbel,
             matnr  TYPE vbrp-matnr,
             refkey TYPE j_1bnflin-refkey,
           END OF ty_vbrp.

    DATA: w_zsdt0330 TYPE zsdt0330,
          w_retorno  TYPE zsdt0331,
          l_erro     TYPE char1,
          l_tabix    TYPE sy-tabix,
          tg_vbrp    TYPE TABLE OF ty_vbrp.

    r_if_trace_cotton = me.

    FREE: e_msg_erro, l_erro.

    IF me->zif_trace_cotton~at_info_request_http-ds_metodo NE 'POST'.
      e_msg_erro = 'Metodo informado não reconhecido!'.
      RETURN.
    ENDIF.

*----------------------------------------
*-- valida info recebidas
*----------------------------------------
    LOOP AT me->zif_trace_cotton~at_zsdt0330 INTO w_zsdt0330.

      l_tabix = sy-tabix.

      MOVE-CORRESPONDING w_zsdt0330            TO w_retorno.

      SELECT SINGLE *
        FROM zsdt0330
        INTO @DATA(w_0330_nf)
       WHERE id_carga      = @w_zsdt0330-id_carga
         AND cancelado     = @abap_false.

      IF sy-subrc <> 0.
        SELECT SINGLE *
          FROM zsdt0330
          INTO w_0330_nf
         WHERE id_carga    = w_zsdt0330-id_carga.
      ENDIF.

      IF sy-subrc <> 0.
        w_retorno-mensagem = e_msg_erro = 'Carregamento não foi encontrado no SAP!'.
        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.

      "SD - Ganho Peso Automatico Algodao US #145369 - WPP
      SELECT SINGLE *
        FROM zsdt0330 INTO @DATA(w_0330_aux)
       WHERE id_carga      EQ @w_zsdt0330-id_carga
         AND status_fardo  NE '3'
         AND cancelado     EQ @abap_false.

      IF sy-subrc EQ 0.
        w_retorno-mensagem = e_msg_erro = 'Carregamento esta em processamento no SAP! Aguardar finalização do processamento!'.
        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.
      "SD - Ganho Peso Automatico Algodao US #145369 - WPP


      SELECT SINGLE id_carga,status_fardo
        FROM zsdt0330
        INTO @DATA(w_0330)
       WHERE id_carga       = @w_zsdt0330-id_carga
         AND cd_sai         = @w_zsdt0330-cd_sai
         AND cancelado      = @abap_false
         AND status_estorno NOT IN ( 'D' ).

      IF     sy-subrc <> 0 AND w_zsdt0330-status_estorno = 'D'.
        w_retorno-mensagem =  e_msg_erro = 'Fardinho não encontrado neste Carregamento!'.
        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ELSEIF sy-subrc = 0  AND w_zsdt0330-status_estorno = 'I'.
        w_retorno-mensagem =  e_msg_erro = 'Fardinho já existe neste Carregamento!'.
        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.

*------------------------------------
*---- valida NF ativa, nao permite estorno
*------------------------------------
      IF w_0330_nf IS NOT INITIAL AND
         w_zsdt0330-data_aprovacao_estorno IS INITIAL. "SD - Ganho Peso Automatico Algodao US #145369 - WPP

        SELECT nr_romaneio, vbeln, branch, doc_rem, id_cli_dest
          FROM zsdt0001
          INTO TABLE @DATA(tg_zsdt0001)
         WHERE tp_movimento  = 'S'
           AND ch_referencia = @w_zsdt0330-ch_referencia
           AND branch        = @w_zsdt0330-werks
           AND nr_safra      = @w_zsdt0330-safra.

        IF sy-subrc = 0.
          SELECT vbrp~vbeln, vgbel, matnr
            FROM vbrp
           INNER JOIN vbrk ON vbrk~vbeln = vbrp~vbeln
            INTO TABLE @tg_vbrp
             FOR ALL ENTRIES IN @tg_zsdt0001
           WHERE vgbel = @tg_zsdt0001-doc_rem
             AND fksto = @abap_off.

          IF sy-subrc = 0.
            LOOP AT tg_vbrp  INTO DATA(wg_vbrp).
              wg_vbrp-refkey    = wg_vbrp-vbeln.
              MODIFY tg_vbrp FROM wg_vbrp.
            ENDLOOP.

            SELECT docnum, refkey
              FROM j_1bnflin
              INTO TABLE @DATA(tg_lin)
               FOR ALL ENTRIES IN @tg_vbrp
             WHERE refkey EQ @tg_vbrp-refkey.

            IF sy-subrc IS INITIAL.
              SELECT *
                FROM j_1bnfdoc
                INTO TABLE @DATA(tg_doc)
                 FOR ALL ENTRIES IN @tg_lin
               WHERE docnum  = @tg_lin-docnum
                 AND cancel  = @abap_off.

              IF sy-subrc = 0.
                w_retorno-mensagem =  e_msg_erro = 'Carregamento ja esta com NF emitida! NF deve ser Estornada!'.
                me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
                DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
                l_erro = abap_true.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

*------------------------------------
*---- outras validacoes
*------------------------------------
      SELECT matnr
        INTO w_zsdt0330-matnr
        FROM mara
          UP TO 1 ROWS
       WHERE normt = w_zsdt0330-normt
         AND mtart = 'ZFER'.
      ENDSELECT.

      IF sy-subrc <> 0.
        w_retorno-mensagem =  e_msg_erro = 'Material nao existe no SAP!'.
        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ELSE.
        w_retorno-matnr =  w_zsdt0330-matnr.
      ENDIF.

      SELECT werks
        INTO @DATA(l_werks)
        FROM t001w
          UP TO 1 ROWS
       WHERE werks = @w_zsdt0330-werks.
      ENDSELECT.

      IF sy-subrc <> 0.
        w_retorno-mensagem =  e_msg_erro = 'Filial nao existe no SAP!'.
        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.

      SELECT lgort
        INTO @DATA(w_t001l)
        FROM t001l
          UP TO 1 ROWS
       WHERE werks = @l_werks
         AND lgort = @w_zsdt0330-lgort.
      ENDSELECT.

      IF sy-subrc <> 0.
        w_retorno-mensagem =  e_msg_erro = 'Deposito nao existe no SAP!'.
        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
        l_erro = abap_true.
        CONTINUE.
      ENDIF.

      "Projeto Reestruturação Algodao 2024
*      SELECT *
*        FROM zppt0002
*        INTO TABLE @DATA(t_0002)
*       WHERE cd_sai = @w_zsdt0330-cd_sai.
*
*      IF sy-subrc = 0.
*        SORT t_0002 BY budat.
*        READ TABLE t_0002 INTO DATA(w_0002) INDEX 1.
*        w_zsdt0330-acharg = w_0002-acharg.
*      ELSE.
*        w_retorno-mensagem = 'Fardo nao encontrado no SAP!'.
*        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
*        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
*        l_erro = abap_true.
*        CONTINUE.
*      ENDIF.
*
*      SELECT SINGLE charg
*        FROM mchb
*        INTO @DATA(w_mchb)
*       WHERE matnr  = @w_zsdt0330-matnr
*         AND werks  = @w_zsdt0330-werks
*         AND lgort  = @w_zsdt0330-lgort
*         AND charg  = @w_zsdt0330-acharg.
*
*      IF sy-subrc <> 0.
*        w_retorno-mensagem = 'Fardo nao encontrado no SAP!'.
*        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
*        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
*        l_erro = abap_true.
*        CONTINUE.
*      ENDIF.
      "Projeto Reestruturação Algodao 2024

      MODIFY me->zif_trace_cotton~at_zsdt0330 FROM w_zsdt0330 INDEX l_tabix TRANSPORTING matnr.
    ENDLOOP.

*-- nao havendo erro, regitra status OK para envio ao trace
    IF l_erro = abap_false.
      w_retorno-mensagem = abap_off.
      me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
    ELSE.
      LOOP AT me->zif_trace_cotton~at_zsdt0330 INTO w_zsdt0330.
        l_tabix = sy-tabix.
        MOVE-CORRESPONDING w_zsdt0330            TO w_retorno.
        w_retorno-mensagem =  'Há fardos com erro no Bloco! Acao recusada!'.
        me->zif_trace_cotton~set_gravar_retorno( i_retorno = w_retorno i_tipo_operacao = 'E' ).
        DELETE me->zif_trace_cotton~at_zsdt0330 INDEX l_tabix.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD zif_trace_cotton~add_fila_gera_sobra_perda.

    DATA: lwa_zsdt0344 TYPE zsdt0344.

    DATA(lit_zsdt330_grp_romaneio) = i_zsdt0330_t.
    SORT  lit_zsdt330_grp_romaneio BY nr_romaneio vbeln.
    DELETE ADJACENT DUPLICATES FROM lit_zsdt330_grp_romaneio COMPARING nr_romaneio vbeln.
    DELETE lit_zsdt330_grp_romaneio WHERE nr_romaneio IS INITIAL OR vbeln IS INITIAL.

    LOOP AT lit_zsdt330_grp_romaneio INTO DATA(lwa_zsdt0330_romaneio).
      lwa_zsdt0344-id_carga           = lwa_zsdt0330_romaneio-id_carga.
      lwa_zsdt0344-seq_carga          = lwa_zsdt0330_romaneio-seq.
      lwa_zsdt0344-vbeln              = lwa_zsdt0330_romaneio-vbeln.
      lwa_zsdt0344-nr_romaneio        = lwa_zsdt0330_romaneio-nr_romaneio.
      lwa_zsdt0344-ch_referencia_rom  = lwa_zsdt0330_romaneio-ch_referencia.
      lwa_zsdt0344-chave_referencia   = lwa_zsdt0330_romaneio-chave_referencia.
      lwa_zsdt0344-dt_registro        = sy-datum.
      lwa_zsdt0344-hr_registro        = sy-uzeit.
      lwa_zsdt0344-us_registro        = sy-uname.
      MODIFY zsdt0344 FROM lwa_zsdt0344.
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
