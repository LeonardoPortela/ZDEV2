
REPORT  zmmr0030_job_v2  MESSAGE-ID ztracecotton.
TYPE-POOLS vrm.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  PARAMETERS    : p_id_ref TYPE zde_id_referencia.
SELECTION-SCREEN END OF BLOCK b1.

** CONSTANTS
**----------------------------------------------------------------------
CONSTANTS: c_x TYPE c VALUE 'X',
           c_p TYPE c VALUE 'P'.

** TYPES
**----------------------------------------------------------------------


** INTERNAL TABLES
**----------------------------------------------------------------------
DATA: it_zppt0002_classificar TYPE TABLE OF zppt0002,
      it_zppt0002_fardao      TYPE TABLE OF zppt0002,
      it_zppt0002_transf      TYPE TABLE OF zppt0002,
      it_zppt0002_estorno     TYPE TABLE OF zppt0002.

DATA: git_zmmt0025            TYPE TABLE OF zmmt0025.
DATA: git_safra_lote          TYPE TABLE OF zmme_cl.

DATA: gva_msg_aux      TYPE string,
      gva_error        TYPE c,
      gva_possui_saldo TYPE c.

*&---------------------------------------------------------------------*
*&      Start-Of-Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  CHECK p_id_ref IS NOT INITIAL.

  PERFORM: f_seleciona_dados,
           f_fardao,
           f_fardinho,
           f_transferencia,
           f_estorno,
           f_envia_trace.

FORM f_seleciona_dados.

  REFRESH: it_zppt0002_fardao, it_zppt0002_classificar, it_zppt0002_transf, it_zppt0002_estorno, git_zmmt0025.

  PERFORM f_get_outros_registros.
  PERFORM f_get_registros_fardao.       "Registros de Homologação Sessão
  PERFORM f_get_regitros_classificacao. "Registros Classificação Fardinho
  PERFORM f_get_registros_transferencia."Registros Transf. Bloco e Material
  PERFORM f_get_registros_estorno.      "Registros Transf. Bloco e Material


ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_FARDAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_fardao.

  DATA: lwa_bflushflags   LIKE bapi_rm_flg,
        lwa_bflushdatagen LIKE bapi_rm_datgen,
        lva_confirmation  LIKE bapi_rm_datkey-confirmation.

  DATA: lit_goodsmovements TYPE TABLE OF bapi2017_gm_item_create,
        lwa_goodsmovements TYPE bapi2017_gm_item_create.

  DATA: lit_itens_lista_tec	TYPE TABLE OF mdpm,
        lwa_mkal            TYPE mkal,
        lit_return          TYPE TABLE OF bapiret2,
        lwa_return          TYPE bapiret2.

  DATA: lva_is_block         TYPE char01.

  DATA: lva_erro_confirmacao     TYPE c,
        lva_estornou_confirmacao TYPE c.

  CHECK it_zppt0002_fardao IS NOT INITIAL.

  DATA(it_zppt0002_fardao_proc) = it_zppt0002_fardao[].

  SORT it_zppt0002_fardao_proc BY werks id_sessao.
  DELETE ADJACENT DUPLICATES FROM it_zppt0002_fardao_proc COMPARING werks id_sessao.

  LOOP AT it_zppt0002_fardao_proc INTO DATA(lwa_zppt0002).

    CLEAR: lva_erro_confirmacao, lva_estornou_confirmacao.

*--------------------------------------------------------------------------------------------*
*   Validações
*--------------------------------------------------------------------------------------------*
    PERFORM f_fardao_start_proc TABLES lit_return
                                       lit_itens_lista_tec
                                 USING lwa_zppt0002
                              CHANGING lwa_mkal.

    IF lit_return[] IS NOT INITIAL.
      PERFORM: f_registra_processamento TABLES lit_return
                                         USING lwa_zppt0002.
      CONTINUE.
    ENDIF.

    CHECK lwa_zppt0002-wait_estorno_confirmation = abap_false. "Não prosseguir se for registro aguardando estorno de confirmação incorreta

*--------------------------------------------------------------------------------------------*
*   Preenchimento Dados BAPI
*--------------------------------------------------------------------------------------------*
    CLEAR: lwa_bflushflags, lwa_bflushdatagen, lwa_return, lit_goodsmovements[], lit_return[].

    lwa_bflushflags-bckfltype        = '01'.
    lwa_bflushdatagen-docheadertxt   = lwa_zppt0002-id_mov_sistema_origem_ref_int.
    lwa_bflushdatagen-postdate       = lwa_zppt0002-budat.
    lwa_bflushdatagen-docdate        = lwa_zppt0002-bldat.
    lwa_bflushdatagen-prodplant      = lwa_zppt0002-werks.      "Centro
    lwa_bflushdatagen-materialnr     = lwa_zppt0002-matnr.      "Material Produção - Pluma em Elaboração
    lwa_bflushdatagen-backflquant    = lwa_zppt0002-menge.      "Quantidade
    lwa_bflushdatagen-unitofmeasure  = 'KG'.                                    "UM
    lwa_bflushdatagen-prodversion    = lwa_zppt0002-verid.      "Versão
    lwa_bflushdatagen-batch          = lwa_zppt0002-charg.      "Lote
    lwa_bflushdatagen-storageloc     = COND #( WHEN lwa_mkal-elpro IS NOT INITIAL THEN lwa_mkal-elpro ELSE lwa_mkal-alort ).

    LOOP AT lit_itens_lista_tec INTO DATA(lwa_item_lista_tec).

      lwa_goodsmovements-material      = lwa_item_lista_tec-matnr.             "Material (MATNR)
      lwa_goodsmovements-entry_uom     = lwa_item_lista_tec-erfme.             "UM
      lwa_goodsmovements-stge_loc      = lwa_item_lista_tec-lgpro.             "Deposito Consumo (LGORT)'
      lwa_goodsmovements-plant         = lwa_zppt0002-werks.   "Centro (WERKS)'

      CASE lwa_item_lista_tec-shkzg.
        WHEN 'H'."Saida Estoque

          "ALGODAO CAROCO PROD PROPRIA
          lwa_goodsmovements-move_type     = '261'.
          lwa_goodsmovements-batch         = lwa_zppt0002-charg.
          lwa_goodsmovements-entry_qnt     = lwa_zppt0002-peso_algodao_caroco.

        WHEN 'S'."Entrada Estoque

          lwa_goodsmovements-move_type     = '531'.
          lwa_goodsmovements-batch         = lwa_zppt0002-charg.

          IF ( lwa_item_lista_tec-matnr = '000000000000120166' ) AND "120166 - FIBRILHA
             ( lwa_zppt0002-peso_fibrilha GT 0 ).

            lwa_goodsmovements-entry_qnt = lwa_zppt0002-peso_fibrilha.

          ELSEIF ( lwa_item_lista_tec-matnr = '000000000000120168' ) AND "120168 - CAROÇO ALGODAO PROD PROPRIA
                 ( lwa_zppt0002-peso_caroco GT 0 ).

            lwa_goodsmovements-entry_qnt = lwa_zppt0002-peso_caroco.

          ENDIF.
      ENDCASE.

      APPEND lwa_goodsmovements TO lit_goodsmovements.

    ENDLOOP.

    DO 6 TIMES.

      DATA(_index) = sy-index.

      CLEAR: lwa_return, lva_is_block, lva_confirmation.

      CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          bflushflags    = lwa_bflushflags
          bflushdatagen  = lwa_bflushdatagen
        IMPORTING
          confirmation   = lva_confirmation
          return         = lwa_return
        TABLES
          goodsmovements = lit_goodsmovements.

      IF lva_confirmation IS INITIAL.

        CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
          EXPORTING
            id       = lwa_return-id
            number   = lwa_return-number
          IMPORTING
            is_block = lva_is_block.

        IF lva_is_block IS NOT INITIAL AND _index LE 5.
          MESSAGE 'Confirmação não criada por erro de bloqueio' TYPE 'S'.
          WAIT UP TO 2 SECONDS.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          APPEND lwa_return TO lit_return.
          EXIT.
        ENDIF.

      ELSE.

        PERFORM f_after_entrada_fardao TABLES lit_goodsmovements
                                              lit_return
                                        USING lva_confirmation
                                              abap_false
                                     CHANGING lwa_zppt0002
                                              lva_erro_confirmacao.

        IF lva_erro_confirmacao EQ abap_true.
          PERFORM f_estorno_confirmacao TABLES lit_return
                                         USING lva_confirmation
                                               lwa_zppt0002
                                      CHANGING lva_estornou_confirmacao.

        ENDIF.

        EXIT.
      ENDIF.

    ENDDO.

    IF ( lva_erro_confirmacao     EQ abap_true  )   AND  "Deu erro ao gerar a confirmação
       ( lva_estornou_confirmacao EQ abap_false ).       "Não conseguiu estornar a confirmação gerada incorretamente
      CONTINUE. "Não registrar processamento de erro e tentar a confirmação na proximo processamento
    ENDIF.

    IF lva_is_block IS NOT INITIAL. "Se nao conseguiu criar movimentação por bloqueio de registro, nao marcar registro com erro e tentar reprocessaar novamente
      PERFORM f_set_registro_for_reproc TABLES lit_return USING lwa_zppt0002 '02'.
      CONTINUE.
    ENDIF.

    "Registrar processamento sucesso ou erro
    PERFORM f_registra_processamento TABLES lit_return USING lwa_zppt0002.

  ENDLOOP.

ENDFORM .

FORM f_transferencia .

  DATA: lva_is_block    TYPE char01.

  DATA: lwa_mara_origem  TYPE mara,
        lwa_mara_destino TYPE mara.

  DATA: lwa_header   TYPE bapi2017_gm_head_01,
        lva_code     TYPE bapi2017_gm_code,
        lva_material TYPE bapi2017_gm_head_ret-mat_doc,
        lva_year     TYPE bapi2017_gm_head_ret-doc_year.


  DATA: lit_goodsmvt_item TYPE TABLE OF bapi2017_gm_item_create,
        lit_return        TYPE TABLE OF bapiret2,
        lwa_return        TYPE bapiret2,
        lwa_goodsmvt_item TYPE bapi2017_gm_item_create.

  "Pega informações do lote do fardinho no estoque
  LOOP AT it_zppt0002_transf INTO DATA(lwa_zppt0002).

    PERFORM f_start_proc_transf TABLES lit_return
                                 USING lwa_zppt0002
                              CHANGING lwa_mara_origem
                                       lwa_mara_destino.
    IF lit_return[] IS NOT INITIAL.
      PERFORM: f_registra_processamento TABLES lit_return
                                         USING lwa_zppt0002.
      CONTINUE.
    ENDIF.

*-------------------------------------------------------------------------------------*
*   Preenchimento Cabeçalho
*-------------------------------------------------------------------------------------*
    CLEAR: lwa_header.

    lva_code = '06'.

    lwa_header-pstng_date = sy-datum.
    lwa_header-doc_date   = sy-datum.
    lwa_header-header_txt = lwa_zppt0002-id_mov_sistema_origem_ref_int.

*-------------------------------------------------------------------------------------*
*   Preenchimento Itens
*-------------------------------------------------------------------------------------*

    CLEAR: lwa_goodsmvt_item, lit_return[].

    lwa_goodsmvt_item-material   = lwa_mara_origem-matnr.
    lwa_goodsmvt_item-plant      = lwa_zppt0002-werks.
    lwa_goodsmvt_item-stge_loc   = lwa_zppt0002-lgort.
    lwa_goodsmvt_item-batch      = lwa_zppt0002-charg.
    lwa_goodsmvt_item-entry_qnt  = lwa_zppt0002-menge.
    lwa_goodsmvt_item-move_plant = lwa_zppt0002-werks.
    lwa_goodsmvt_item-move_type  = '309'.
    lwa_goodsmvt_item-move_stloc = lwa_zppt0002-bloco_destino.
    lwa_goodsmvt_item-move_mat   = lwa_mara_destino-matnr.
    lwa_goodsmvt_item-move_batch = lwa_zppt0002-charg.

    APPEND lwa_goodsmvt_item TO lit_goodsmvt_item.

    DO 6 TIMES.
      CLEAR: lva_is_block.

      FREE: lit_return.

      DATA(_index) = sy-index.

      CLEAR: lva_material, lva_year.
      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = lwa_header
          goodsmvt_code    = lva_code
        IMPORTING
          materialdocument = lva_material
          matdocumentyear  = lva_year
        TABLES
          goodsmvt_item    = lit_goodsmvt_item
          return           = lit_return.

      READ TABLE lit_return INTO lwa_return WITH KEY type = 'E'.

      IF sy-subrc = 0.
        CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
          EXPORTING
            id       = lwa_return-id
            number   = lwa_return-number
          IMPORTING
            is_block = lva_is_block.

        IF lva_is_block IS NOT INITIAL AND _index LE 5.
          MESSAGE 'Transferencia não criada por erro de bloqueio' TYPE 'S'.
          WAIT UP TO 2 SECONDS.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          EXIT.
        ENDIF.
      ELSE.
        PERFORM f_after_transferencia TABLES lit_goodsmvt_item
                                             lit_return
                                       USING lva_material
                                             lva_year
                                    CHANGING lwa_zppt0002.

        PERFORM f_check_change_safra_lote USING lwa_mara_destino-matnr lwa_zppt0002-charg lwa_zppt0002-cd_safra.

        EXIT.
      ENDIF.
    ENDDO.

    IF lva_is_block IS NOT INITIAL. "Se nao conseguiu criar movimentação por bloqueio de registro, nao marcar registro com erro e tentar reprocessaar novamente
      PERFORM f_set_registro_for_reproc TABLES lit_return USING lwa_zppt0002 '02'.
      CONTINUE.
    ENDIF.

    "Registrar processamento sucesso ou erro
    PERFORM f_registra_processamento TABLES lit_return USING lwa_zppt0002.


  ENDLOOP.

ENDFORM.                    " F_TRANSFERENCIA



FORM f_envia_trace.

  DATA: lwa_recebimento TYPE zpps0006.

  MESSAGE  'Form: f_envia_trace' TYPE 'S'.

  CHECK p_id_ref IS NOT INITIAL.

  lwa_recebimento-protocolo_recebimento = p_id_ref.

  TRY.
      zcl_int_ob_ret_benef_trace_cot=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = lwa_recebimento ).
    CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
      MESSAGE ID lwa_zcx_integracao->msgid TYPE 'S' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
    CATCH zcx_error INTO DATA(zcx_error).
      MESSAGE ID zcx_error->msgid TYPE 'S' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
  ENDTRY.

ENDFORM.

FORM f_conversion_matnr  USING    p_matnr_in
                         CHANGING c_matnr_out.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = p_matnr_in
    IMPORTING
      output = c_matnr_out.

ENDFORM.

FORM f_set_id_zppt0006 USING p_zppt0002 TYPE zppt0002
                    CHANGING c_id       TYPE zppt0006-id.

  CLEAR: c_id.

  SELECT MAX( id ) INTO c_id
    FROM zppt0006
   WHERE werks      = p_zppt0002-werks
     AND charg      = p_zppt0002-charg
     AND acharg     = p_zppt0002-acharg
     AND id_sessao  = p_zppt0002-id_sessao
     AND lgort      = p_zppt0002-lgort
     AND cd_safra   = p_zppt0002-cd_safra.

  ADD 1 TO c_id.


ENDFORM.


FORM f_registra_processamento TABLES t_return STRUCTURE bapiret2
                               USING p_zppt0002  TYPE zppt0002.


  DATA: lwa_zppt0006 TYPE zppt0006,
        lit_zppt0006 TYPE TABLE OF zppt0006.

  DATA: lva_status_registro TYPE zppt0002-status_registro.
  DATA: lva_mensagem_proc   TYPE zppt0002-cd_mensagem.

  CLEAR: lit_zppt0006[], lva_status_registro.

  DATA(lva_sucesso) = abap_false.

  "Registrar Processamento de Sucesso ou Erro
  LOOP AT t_return INTO DATA(lwa_return).

    CLEAR lwa_zppt0006.

    PERFORM f_set_id_zppt0006 USING p_zppt0002 CHANGING lwa_zppt0006-id.

    "Campos Chaves
    lwa_zppt0006-werks                            = p_zppt0002-werks.
    lwa_zppt0006-charg                            = p_zppt0002-charg.
    lwa_zppt0006-acharg                           = p_zppt0002-acharg.
    lwa_zppt0006-id_sessao                        = p_zppt0002-id_sessao.
    lwa_zppt0006-lgort                            = p_zppt0002-lgort.
    lwa_zppt0006-cd_safra                         = p_zppt0002-cd_safra.

    "Demais campos
    lwa_zppt0006-data                             = sy-datum.
    lwa_zppt0006-hora                             = sy-uzeit.
    lwa_zppt0006-msgnr                            = lwa_return-number.
    lwa_zppt0006-msgid                            = lwa_return-id.
    lwa_zppt0006-status_msg                       = lwa_return-type.
    lwa_zppt0006-cd_mensagem                      = lwa_return-message.
    lwa_zppt0006-matnr                            = p_zppt0002-matnr.
    lwa_zppt0006-verid                            = p_zppt0002-verid.
    lwa_zppt0006-budat                            = p_zppt0002-budat.
    lwa_zppt0006-id_mov_sistema_origem            = p_zppt0002-id_mov_sistema_origem.
    lwa_zppt0006-id_mov_sistema_origem_ref_int    = p_zppt0002-id_mov_sistema_origem_ref_int.
    lwa_zppt0006-id_referencia2                   = p_zppt0002-id_referencia.
    lwa_zppt0006-mblnr                            = p_zppt0002-mblnr.
    lwa_zppt0006-mjahr                            = p_zppt0002-mjahr.
    lwa_zppt0006-flag_envio                       = 'R'.
    lwa_zppt0006-menge                            = p_zppt0002-menge.
    lwa_zppt0006-peso_algodao_caroco              = p_zppt0002-peso_algodao_caroco.
    lwa_zppt0006-peso_caroco                      = p_zppt0002-peso_caroco.
    lwa_zppt0006-peso_fibrilha                    = p_zppt0002-peso_fibrilha.
    lwa_zppt0006-peso_bruto                       = p_zppt0002-peso_bruto.
    lwa_zppt0006-peso_liquido                     = p_zppt0002-peso_liquido.
    lwa_zppt0006-qtd_fardinhos_sessao             = p_zppt0002-qtd_fardinhos_sessao.
    lwa_zppt0006-cd_classificacao                 = p_zppt0002-cd_classificacao.
    lwa_zppt0006-bloco_destino                    = p_zppt0002-bloco_destino.
    lwa_zppt0006-cd_classificacao_bloco_destino   = p_zppt0002-cd_classificacao_bloco_destino.

    lva_mensagem_proc                             = lwa_return-message.

    IF lwa_return-type = 'S'.
      lva_sucesso = abap_true.
    ENDIF.

    CASE p_zppt0002-status_registro.
      WHEN '01'. "Processamento Homologação Sessão

        CASE lwa_return-type.
          WHEN 'E'.
            lva_status_registro  =  'E2'. "Erro Homologação Sessão
          WHEN 'S'.
            lva_status_registro  =  '02'. "Sucesso Homologação Sessão
        ENDCASE.

      WHEN '03'. "Classificação

        CASE lwa_return-type.
          WHEN 'E'.
            lva_status_registro  = 'E4'. "Erro Classificação
          WHEN 'S'.
            lva_status_registro  = '04'. "Sucesso Classificação
        ENDCASE.

        lwa_zppt0006-budat                   = p_zppt0002-dt_classificacao.
        lwa_zppt0006-matnr                   = p_zppt0002-matnr_classificacao.

      WHEN '07'. "Troca Bloco/Troca Material

        CASE lwa_return-type.
          WHEN 'E'.
            lva_status_registro  = 'E8'. "Erro Troca Bloco
          WHEN 'S'.
            lva_status_registro  = '08'. "Sucesso Troca Bloco
        ENDCASE.

      WHEN '99'. "Estorno

        CASE lwa_return-type.
          WHEN 'E'.
            lva_status_registro  = 'E6'. "Erro Estorno
          WHEN 'S'.
            lva_status_registro  = '06'. "Sucesso Estorno
        ENDCASE.

      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    lwa_zppt0006-status_registro         = lva_status_registro.

    APPEND lwa_zppt0006 TO lit_zppt0006.

  ENDLOOP.

  CHECK lva_status_registro IS NOT INITIAL.

  MODIFY zppt0006 FROM TABLE lit_zppt0006.

  IF lva_sucesso EQ abap_true.
    PERFORM f_atualiza_registro_bloco USING p_zppt0002.
  ENDIF.

  UPDATE zppt0002 SET status               = 'P'
                      status_processamento = 'C' "Processamento Concluido
                      status_registro      = lva_status_registro
                      cd_mensagem          = lva_mensagem_proc
                WHERE acharg     = p_zppt0002-acharg
                  AND werks      = p_zppt0002-werks
                  AND id_sessao  = p_zppt0002-id_sessao
                  AND lgort      = p_zppt0002-lgort
                  AND cd_safra   = p_zppt0002-cd_safra.

  COMMIT WORK AND WAIT .

ENDFORM.                    " F_MONTA_MSG

FORM f_fardao_start_proc TABLES t_return STRUCTURE bapiret2
                                t_itens_lista_tecnica STRUCTURE mdpm
                          USING p_zppt0002 TYPE zppt0002
                       CHANGING c_mkal     TYPE mkal.

  DATA: lit_mkpf_exists TYPE TABLE OF mkpf.

  DATA: lwa_plaf   TYPE plaf,
        lwa_mt61d  LIKE mt61d,
        lva_cm61m  LIKE cm61m,
        lva_xscrap TYPE xfeld,
        lva_eselid LIKE af61z-selid.

  CLEAR: t_return[], t_itens_lista_tecnica.

  MESSAGE 'Start Processamento Fardão...' TYPE 'S'.

  IF ( p_zppt0002-wait_estorno_confirmation EQ abap_true ). "Confirmação gerada incorretamente... Tentar estornar antes de retornar para o Trace
    DATA(_estornou_confirmacao) = abap_false.
    PERFORM f_estorno_confirmacao TABLES t_return
                                   USING p_zppt0002-prtnr_estorno
                                         p_zppt0002
                                CHANGING _estornou_confirmacao.
    RETURN.
  ENDIF.

  "Validar se movimentação já foi realizada
  PERFORM f_check_mov_realizada TABLES t_return USING p_zppt0002.
  CHECK t_return[] IS INITIAL.

  IF p_zppt0002-peso_algodao_caroco IS INITIAL.
    gva_msg_aux = |Peso Algodao em Caroco não informado !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF p_zppt0002-peso_caroco IS INITIAL.
    gva_msg_aux = |Peso Caroco de Algodao não informado !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF p_zppt0002-peso_fibrilha IS INITIAL.
    gva_msg_aux = |Peso Fibrilha não informado !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF p_zppt0002-menge IS INITIAL.
    gva_msg_aux = |Peso Pluma em Elaboração não informado !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE cslid
    FROM t399d INTO lva_eselid
   WHERE werks EQ p_zppt0002-werks.

  IF sy-subrc NE 0 OR lva_eselid IS INITIAL.
    gva_msg_aux = |Não foi possivel obter os parâmetros de controle para MRP Filial: { p_zppt0002-werks } !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  CLEAR: c_mkal.
  SELECT SINGLE *
    FROM mkal INTO c_mkal
   WHERE werks = p_zppt0002-werks
     AND matnr = p_zppt0002-matnr
     AND verid = p_zppt0002-verid.

  IF sy-subrc NE 0.
    gva_msg_aux = |Versão de produção não encontrada! Centro: { p_zppt0002-werks } Material: { p_zppt0002-matnr } Versão: { p_zppt0002-verid } !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  CLEAR: lwa_plaf.
  lwa_plaf-matnr = p_zppt0002-matnr.
  lwa_plaf-plwrk = p_zppt0002-werks.
  lwa_plaf-pwwrk = p_zppt0002-werks.
  lwa_plaf-gsmng = p_zppt0002-menge.
  lwa_plaf-psttr = p_zppt0002-budat.
  lwa_plaf-verid = p_zppt0002-verid.

  CALL FUNCTION 'MD_AUFLOESUNG_PLANAUFTRAG'
    EXPORTING
      eplaf         = lwa_plaf
      emt61d        = lwa_mt61d
      eselid        = lva_eselid
      ecm61m        = lva_cm61m
      eno_scrap     = lva_xscrap
    IMPORTING
      iplaf         = lwa_plaf
    TABLES
      mdpmx         = t_itens_lista_tecnica
    EXCEPTIONS
      error_message = 1.

  IF sy-subrc NE 0 OR t_itens_lista_tecnica[] IS INITIAL.
    gva_msg_aux = 'Não foi possivel obter a lista tecnica para produção!'.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_itens_lista_tecnica INTO DATA(lwa_item_lista_tec).

    CASE lwa_item_lista_tec-shkzg.
      WHEN 'H'. "Movimento Saida

        PERFORM f_check_saldo_consumo TABLES t_return
                                       USING lwa_item_lista_tec-matnr         "Material
                                             p_zppt0002-werks                 "Centro
                                             lwa_item_lista_tec-lgpro         "Deposito
                                             p_zppt0002-charg                 "Lote
                                             p_zppt0002-peso_algodao_caroco   "Quantidade Consumo
                                    CHANGING gva_possui_saldo. "Possui Saldo?

        IF gva_possui_saldo IS INITIAL.
          RETURN.
        ENDIF.

      WHEN 'S'. "Movimento Entrada
    ENDCASE.
  ENDLOOP.

  READ TABLE t_itens_lista_tecnica WITH KEY shkzg = 'H'.
  IF sy-subrc NE 0.
    gva_msg_aux = 'Lista Tecnica sem movimento de baixa de estoque!'.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF lines( t_itens_lista_tecnica ) NE 3.
    gva_msg_aux = 'Lista Tecnica com parametrização de movimentação incompleta!'.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

ENDFORM.


FORM f_add_mensagem_return TABLES t_return STRUCTURE bapiret2
                            USING p_mensagem TYPE string
                                  p_type     TYPE c.

  DATA: lwa_return  TYPE bapiret2.

  lwa_return-type    = p_type.
  lwa_return-number  = 2.
  lwa_return-message = p_mensagem.
  APPEND lwa_return TO t_return.

ENDFORM.

FORM f_after_entrada_fardao TABLES t_itens_goods      STRUCTURE bapi2017_gm_item_create
                                   t_return           STRUCTURE bapiret2
                             USING p_confirmation     TYPE bapi_rm_datkey-confirmation
                                   p_estorno          TYPE c
                          CHANGING c_zppt0002         TYPE zppt0002
                                   c_erro_confirmacao TYPE c.

  DATA: lwa_return   TYPE bapiret2.
  DATA: lva_material TYPE bapi2017_gm_item_create-material.

  DATA: lit_mseg TYPE TABLE OF mseg.

  DATA: lwa_blpp         LIKE blpp.
  DATA: lwa_blpp_estorno LIKE blpp.

  DATA: lva_confirmation_es TYPE bapi_rm_datkey-cancconfirmation.

  CLEAR: c_erro_confirmacao.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = c_x.

  DO 10 TIMES.
    CLEAR: lwa_blpp.
    SELECT SINGLE *
      FROM blpp INTO lwa_blpp
     WHERE prtnr = p_confirmation
       AND prtps = '0001'.

    IF sy-subrc EQ 0.
      EXIT.
    ELSE.
      WAIT UP TO 2 SECONDS.
    ENDIF.
  ENDDO.

  IF lwa_blpp IS INITIAL.
    c_erro_confirmacao = abap_true.
    RETURN.
  ENDIF.

  CLEAR: lit_mseg[].

  SELECT *
    FROM mseg INTO TABLE lit_mseg
   WHERE mblnr EQ lwa_blpp-belnr
     AND mjahr EQ c_zppt0002-budat+0(4).

  READ TABLE lit_mseg INTO DATA(lwa_mseg) INDEX 1.
  IF sy-subrc NE 0 OR lwa_mseg-mblnr IS INITIAL.
    c_erro_confirmacao = abap_true.
    RETURN.
  ENDIF.

  DATA(_movimento_completo) = abap_true.

  IF p_estorno EQ abap_true.

    DATA(lit_mseg_aux) = lit_mseg[].
    DELETE lit_mseg_aux WHERE  bwart NE '532'.
    IF lines( lit_mseg_aux ) NE 2.
      _movimento_completo = abap_false.
    ENDIF.

    READ TABLE lit_mseg WITH KEY bwart = '132' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      _movimento_completo = abap_false.
    ENDIF.

  ELSE.
    READ TABLE lit_mseg WITH KEY bwart = '261' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      _movimento_completo = abap_false.
    ENDIF.
  ENDIF.

  IF lines( lit_mseg ) NE 4.
    _movimento_completo = abap_false.
  ENDIF.

  IF p_estorno EQ abap_true.
    DATA(lva_status_registro) = '06'.
  ELSE.
    lva_status_registro = '02'.
  ENDIF.

  IF _movimento_completo = abap_true.

    c_zppt0002-mblnr     = lwa_mseg-mblnr.
    c_zppt0002-mjahr     = lwa_mseg-mjahr.
    c_zppt0002-mblnr02   = lwa_mseg-mblnr.
    c_zppt0002-mjahr02   = lwa_mseg-mjahr.

    UPDATE zppt0002
       SET status_registro = lva_status_registro
           mblnr           = lwa_mseg-mblnr
           mjahr           = lwa_mseg-mjahr
           mblnr02         = lwa_mseg-mblnr
           mjahr02         = lwa_mseg-mjahr
     WHERE acharg      = c_zppt0002-acharg
       AND werks       = c_zppt0002-werks
       AND id_sessao   = c_zppt0002-id_sessao
       AND lgort       = c_zppt0002-lgort
       AND cd_safra    = c_zppt0002-cd_safra.

    COMMIT WORK AND WAIT.

    "Desbloqueio
    LOOP AT t_itens_goods INTO DATA(lva_item_goods).
      CALL FUNCTION 'DEQUEUE_EMMARCE'
        EXPORTING
          matnr = lva_item_goods-material
          werks = lva_item_goods-plant.
    ENDLOOP.

    IF p_estorno EQ abap_true.
      gva_msg_aux = |Documento Estorno de Fardão gerado: { lwa_blpp-belnr } |.
    ELSE.
      gva_msg_aux = |Documento de Fardão gerado: { lwa_blpp-belnr } |.
    ENDIF.

    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'S'.
    RETURN.

  ELSE.
    c_erro_confirmacao = abap_true.
  ENDIF.


ENDFORM.

FORM f_fardinho_start_proc TABLES t_return STRUCTURE bapiret2
                         CHANGING c_zppt0002 TYPE zppt0002
                                  c_mkal_origem   TYPE mkal
                                  c_mkal_destino  TYPE mkal
                                  c_mara_origem   TYPE mara
                                  c_mara_destino  TYPE mara.

  DATA: lit_mkpf_exists TYPE TABLE OF mkpf.

  CLEAR: t_return[], c_mkal_origem, c_mkal_destino, c_mara_origem ,c_mara_destino.

  MESSAGE 'Start Processamento Fardinho...' TYPE 'S'.

  IF ( c_zppt0002-wait_estorno_confirmation EQ abap_true ). "Confirmação gerada incorretamente... Tentar estornar antes de retornar para o Trace
    DATA(_estornou_confirmacao) = abap_false.
    PERFORM f_estorno_confirmacao TABLES t_return
                                   USING c_zppt0002-prtnr_estorno
                                         c_zppt0002
                                CHANGING _estornou_confirmacao.
    RETURN.
  ENDIF.

  "Validar se movimentação já foi realizada
  PERFORM f_check_mov_realizada TABLES t_return USING c_zppt0002.
  CHECK t_return[] IS INITIAL.

  SELECT SINGLE *
    FROM mara INTO c_mara_origem
   WHERE matnr EQ c_zppt0002-matnr.

  IF sy-subrc NE 0.
    gva_msg_aux = |Cadastro de material { c_zppt0002-matnr } não encontrado!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF c_zppt0002-cd_classificacao IS INITIAL.
    gva_msg_aux = |Cod. Classificação não informado!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM mara INTO c_mara_destino
   WHERE normt = c_zppt0002-cd_classificacao
     AND mtart = 'ZFER'.

  IF sy-subrc NE 0.
    gva_msg_aux = |Cadastro de material para classificação { c_zppt0002-cd_classificacao } não encontrado!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  c_zppt0002-matnr_classificacao = c_mara_destino-matnr.

  "Checar se tem saldo no bloco com um material diferente da classificação do Trace
  PERFORM f_bloco_permite_class_material TABLES t_return
                                          USING c_mara_destino-matnr
                                                c_zppt0002-werks
                                                c_zppt0002-lgort
                                                c_zppt0002-charg
                                       CHANGING gva_error.
  CHECK gva_error IS INITIAL.

  SELECT SINGLE *
    FROM mkal INTO c_mkal_origem
   WHERE werks = c_zppt0002-werks
     AND matnr = c_zppt0002-matnr
     AND verid = c_zppt0002-verid.

  IF sy-subrc NE 0.
    gva_msg_aux = |Versão de produção não encontrada! Centro: { c_zppt0002-werks } Material: { c_zppt0002-matnr } Versão: { c_zppt0002-verid } !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM mkal INTO c_mkal_destino
   WHERE werks = c_zppt0002-werks
     AND matnr = c_mara_destino-matnr
     AND verid = c_zppt0002-verid.

  IF sy-subrc NE 0.
    gva_msg_aux = |Versão de produção não encontrada! Centro: { c_zppt0002-werks } Material: { c_mara_destino-matnr } Versão: { c_zppt0002-verid } !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF c_zppt0002-charg IS INITIAL.
    gva_msg_aux = |Lote não informado para o item!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF c_mkal_origem-elpro IS NOT INITIAL.
    DATA(_deposito_check)  = c_mkal_origem-elpro.
  ELSE.
    _deposito_check  = c_mkal_origem-alort.
  ENDIF.

  PERFORM f_check_saldo_consumo TABLES t_return
                                 USING c_mara_origem-matnr  "Material
                                       c_zppt0002-werks     "Centro
                                       _deposito_check      "Deposito
                                       c_zppt0002-charg     "Lote
                                       c_zppt0002-menge     "Quantidade Consumo
                              CHANGING gva_possui_saldo.    "Possui Saldo?

  IF gva_possui_saldo IS INITIAL.
    RETURN.
  ENDIF.

  "Verificar se apos a movimentação, o peso do bloco no SAP ficará com o mesmo peso do Trace Cotton
  PERFORM f_check_peso_bloco_sap_trace  TABLES t_return
                                         USING c_mara_destino-matnr
                                               c_zppt0002-werks
                                               c_zppt0002-lgort
                                               c_zppt0002-charg
                                               c_zppt0002-cd_safra
                                               c_zppt0002-menge
                                               c_zppt0002-peso_liq_atual_bloco
                                               '+'
                                      CHANGING gva_error.
  IF gva_error IS NOT INITIAL.
    RETURN.
  ENDIF.

ENDFORM.


FORM f_fardinho_estorno_start_proc TABLES t_return STRUCTURE bapiret2
                             CHANGING c_zppt0002 TYPE zppt0002
                                      c_mkal_origem   TYPE mkal
                                      c_mkal_destino  TYPE mkal
                                      c_mara_origem   TYPE mara
                                      c_mara_destino  TYPE mara.

  DATA: lit_mkpf_exists TYPE TABLE OF mkpf.

  CLEAR: t_return[], c_mkal_origem, c_mkal_destino, c_mara_origem ,c_mara_destino.

  MESSAGE 'Start Processamento Fardinho Estorno...' TYPE 'S'.

  IF ( c_zppt0002-wait_estorno_confirmation EQ abap_true ). "Confirmação gerada incorretamente... Tentar estornar antes de retornar para o Trace
    DATA(_estornou_confirmacao) = abap_false.
    PERFORM f_estorno_confirmacao TABLES t_return
                                   USING c_zppt0002-prtnr_estorno
                                         c_zppt0002
                                CHANGING _estornou_confirmacao.
    RETURN.
  ENDIF.

  "Validar se movimentação já foi realizada
  PERFORM f_check_mov_realizada TABLES t_return USING c_zppt0002.
  CHECK t_return[] IS INITIAL.

  SELECT SINGLE *
    FROM mara INTO c_mara_origem
   WHERE matnr EQ c_zppt0002-matnr.

  IF sy-subrc NE 0.
    gva_msg_aux = |Cadastro de material { c_zppt0002-matnr } não encontrado!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF c_zppt0002-cd_classificacao IS INITIAL.
    gva_msg_aux = |Cod. Classificação não informado!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM mara INTO c_mara_destino
   WHERE normt = c_zppt0002-cd_classificacao
     AND mtart = 'ZFER'.

  IF sy-subrc NE 0.
    gva_msg_aux = |Cadastro de material para classificação { c_zppt0002-cd_classificacao } não encontrado!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  c_zppt0002-matnr_classificacao = c_mara_destino-matnr.

  SELECT SINGLE *
    FROM mkal INTO c_mkal_origem
   WHERE werks = c_zppt0002-werks
     AND matnr = c_zppt0002-matnr
     AND verid = c_zppt0002-verid.

  IF sy-subrc NE 0.
    gva_msg_aux = |Versão de produção não encontrada! Centro: { c_zppt0002-werks } Material: { c_zppt0002-matnr } Versão: { c_zppt0002-verid } !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM mkal INTO c_mkal_destino
   WHERE werks = c_zppt0002-werks
     AND matnr = c_mara_destino-matnr
     AND verid = c_zppt0002-verid.

  IF sy-subrc NE 0.
    gva_msg_aux = |Versão de produção não encontrada! Centro: { c_zppt0002-werks } Material: { c_mara_destino-matnr } Versão: { c_zppt0002-verid } !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF c_zppt0002-charg IS INITIAL.
    gva_msg_aux = |Lote não informado para o item!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  PERFORM f_check_saldo_consumo TABLES t_return
                                 USING c_mara_destino-matnr "Material
                                       c_zppt0002-werks     "Centro
                                       c_zppt0002-lgort     "Deposito
                                       c_zppt0002-charg     "Lote
                                       c_zppt0002-menge     "Quantidade Consumo
                              CHANGING gva_possui_saldo.    "Possui Saldo?

  IF gva_possui_saldo IS INITIAL.
    RETURN.
  ENDIF.

ENDFORM.

FORM f_fardinho.

  DATA: lva_is_block      TYPE char01.

  DATA: lwa_bflushflags   LIKE bapi_rm_flg,
        lwa_bflushdatagen LIKE bapi_rm_datgen,
        lva_confirmation  LIKE bapi_rm_datkey-confirmation.

  DATA: lit_goodsmovements TYPE TABLE OF bapi2017_gm_item_create,
        lwa_goodsmovements TYPE bapi2017_gm_item_create.

  DATA: lit_return TYPE TABLE OF bapiret2,
        lwa_return TYPE bapiret2.


  DATA: lwa_mkal_origem  TYPE mkal,
        lwa_mkal_destino TYPE mkal,
        lwa_mara_origem  TYPE mara,
        lwa_mara_destino TYPE mara.

  DATA: lva_erro_confirmacao     TYPE c,
        lva_estornou_confirmacao TYPE c.

  LOOP AT it_zppt0002_classificar INTO DATA(lwa_zppt0002).

    CLEAR: lit_goodsmovements[], lwa_bflushflags, lwa_bflushdatagen, lwa_return, lva_erro_confirmacao, lva_estornou_confirmacao.

    PERFORM f_fardinho_start_proc TABLES lit_return
                                CHANGING lwa_zppt0002
                                         lwa_mkal_origem
                                         lwa_mkal_destino
                                         lwa_mara_origem
                                         lwa_mara_destino.
    IF lit_return[] IS NOT INITIAL.
      PERFORM: f_registra_processamento TABLES lit_return
                                         USING lwa_zppt0002.
      CONTINUE.
    ENDIF.

    CHECK lwa_zppt0002-wait_estorno_confirmation = abap_false. "Não prosseguir se for registro aguardando estorno de confirmação incorreta

    lwa_bflushflags-bckfltype        = '01'.
    lwa_bflushdatagen-docheadertxt   = lwa_zppt0002-id_mov_sistema_origem_ref_int.
    lwa_bflushdatagen-postdate       = lwa_zppt0002-dt_classificacao.
    lwa_bflushdatagen-docdate        = lwa_zppt0002-dt_classificacao.
    lwa_bflushdatagen-prodplant      = lwa_zppt0002-werks.
    lwa_bflushdatagen-materialnr     = lwa_mara_destino-matnr.
    lwa_bflushdatagen-backflquant    = lwa_zppt0002-menge.
    lwa_bflushdatagen-unitofmeasure  = lwa_mara_destino-meins.
    lwa_bflushdatagen-prodversion    = lwa_zppt0002-verid.
    lwa_bflushdatagen-batch          = lwa_zppt0002-charg.
    lwa_bflushdatagen-storageloc     = lwa_zppt0002-lgort.

    CLEAR: lwa_goodsmovements.
    lwa_goodsmovements-material      = lwa_zppt0002-matnr.
    lwa_goodsmovements-plant         = lwa_zppt0002-werks.
    lwa_goodsmovements-stge_loc      = COND #( WHEN lwa_mkal_origem-elpro IS NOT INITIAL THEN lwa_mkal_origem-elpro ELSE lwa_mkal_origem-alort ).
    lwa_goodsmovements-batch         = lwa_zppt0002-charg.
    lwa_goodsmovements-move_type     = '261'.
    lwa_goodsmovements-entry_qnt     = lwa_zppt0002-menge.
    lwa_goodsmovements-entry_uom     = lwa_mara_origem-meins.
    APPEND lwa_goodsmovements TO lit_goodsmovements.


    DO 6 TIMES.

      DATA(_index) = sy-index.

      CLEAR: lva_is_block, lwa_return, lva_confirmation.

      CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'
        EXPORTING
          bflushflags    = lwa_bflushflags
          bflushdatagen  = lwa_bflushdatagen
        IMPORTING
          confirmation   = lva_confirmation
          return         = lwa_return
        TABLES
          goodsmovements = lit_goodsmovements.

      IF lva_confirmation IS INITIAL.

        CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
          EXPORTING
            id       = lwa_return-id
            number   = lwa_return-number
          IMPORTING
            is_block = lva_is_block.

        IF lva_is_block IS NOT INITIAL AND _index LE 5.
          MESSAGE 'Confirmação não criada por erro de bloqueio' TYPE 'S'.
          WAIT UP TO 2 SECONDS.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          APPEND lwa_return TO lit_return.
          EXIT.
        ENDIF.

      ELSE.
        PERFORM f_after_entrada_fardinho TABLES lit_goodsmovements
                                                lit_return
                                          USING lva_confirmation
                                                abap_false
                                       CHANGING lwa_zppt0002
                                                lva_erro_confirmacao.

        IF lva_erro_confirmacao EQ abap_true.
          PERFORM f_estorno_confirmacao TABLES lit_return
                                         USING lva_confirmation
                                               lwa_zppt0002
                                      CHANGING lva_estornou_confirmacao.

        ENDIF.

        EXIT.
      ENDIF.
    ENDDO.

    IF ( lva_erro_confirmacao     EQ abap_true  )   AND  "Deu erro ao gerar a confirmação
       ( lva_estornou_confirmacao EQ abap_false ).       "Não conseguiu estornar a confirmação gerada incorretamente
      CONTINUE. "Não registrar processamento de erro e tentar a confirmação na proximo processamento
    ENDIF.

    IF lva_is_block IS NOT INITIAL. "Se nao conseguiu criar movimentação por bloqueio de registro, nao marcar registro com erro e tentar reprocessaar novamente
      PERFORM f_set_registro_for_reproc TABLES lit_return USING lwa_zppt0002 '02'.
      CONTINUE.
    ENDIF.

    "Registrar processamento sucesso ou erro
    PERFORM f_registra_processamento TABLES lit_return USING lwa_zppt0002.

  ENDLOOP.

ENDFORM.


FORM f_after_entrada_fardinho TABLES t_itens_goods      STRUCTURE bapi2017_gm_item_create
                                     t_return           STRUCTURE bapiret2
                               USING p_confirmation     TYPE bapi_rm_datkey-confirmation
                                     p_estorno          TYPE c
                            CHANGING c_zppt0002         TYPE zppt0002
                                     c_erro_confirmacao TYPE c.


  DATA: lva_material TYPE bapi2017_gm_item_create-material.

  DATA: lit_mseg TYPE TABLE OF mseg.

  DATA: lwa_return TYPE bapiret2.

  DATA: lwa_blpp         LIKE blpp.
  DATA: lwa_blpp_estorno LIKE blpp.

  DATA: lva_confirmation_es TYPE bapi_rm_datkey-cancconfirmation.

  CLEAR: c_erro_confirmacao.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = c_x.

  DO 10 TIMES.
    CLEAR: lwa_blpp.
    SELECT SINGLE *
      FROM blpp INTO lwa_blpp
     WHERE prtnr = p_confirmation
       AND prtps = '0001'.

    IF sy-subrc EQ 0.
      EXIT.
    ELSE.
      WAIT UP TO 2 SECONDS.
    ENDIF.
  ENDDO.

  IF lwa_blpp IS INITIAL.
    c_erro_confirmacao = abap_true.
    RETURN.
  ENDIF.

  CLEAR: lit_mseg[].

  SELECT *
    FROM mseg INTO TABLE lit_mseg
   WHERE mblnr EQ lwa_blpp-belnr
     AND mjahr EQ c_zppt0002-dt_classificacao+0(4).

  READ TABLE lit_mseg INTO DATA(lwa_mseg) INDEX 1.
  IF sy-subrc NE 0 OR lwa_mseg-mblnr IS INITIAL.
    c_erro_confirmacao = abap_true.
    RETURN.
  ENDIF.

  DATA(_tp_mov_baixa) = '261'.
  IF p_estorno EQ abap_true.
    _tp_mov_baixa = '132'.
  ENDIF.

  DATA(_movimento_completo) = abap_true.
  READ TABLE lit_mseg WITH KEY bwart = _tp_mov_baixa TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    _movimento_completo = abap_false.
  ELSEIF lines( lit_mseg ) NE 2.
    _movimento_completo = abap_false.
  ENDIF.


  IF p_estorno EQ abap_true.
    DATA(lva_status_registro) = '06'.
  ELSE.
    lva_status_registro = '04'.
  ENDIF.

  IF _movimento_completo = abap_true.

    c_zppt0002-mblnr     = lwa_mseg-mblnr.
    c_zppt0002-mjahr     = lwa_mseg-mjahr.

    UPDATE zppt0002
       SET status_registro = lva_status_registro
           mblnr           = lwa_mseg-mblnr
           mjahr           = lwa_mseg-mjahr
     WHERE acharg      = c_zppt0002-acharg
       AND werks       = c_zppt0002-werks
       AND id_sessao   = c_zppt0002-id_sessao
       AND lgort       = c_zppt0002-lgort
       AND cd_safra    = c_zppt0002-cd_safra.

    COMMIT WORK AND WAIT.

    "Desbloqueio
    LOOP AT t_itens_goods INTO DATA(lva_item_goods).
      CALL FUNCTION 'DEQUEUE_EMMARCE'
        EXPORTING
          matnr = lva_item_goods-material
          werks = lva_item_goods-plant.
    ENDLOOP.

    IF p_estorno EQ abap_true.
      gva_msg_aux = |Documento de estorno de classificação gerado: { lwa_blpp-belnr } |.
    ELSE.
      gva_msg_aux = |Documento de classificação gerado: { lwa_blpp-belnr } |.
    ENDIF.

    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'S'.

    PERFORM f_check_change_safra_lote USING c_zppt0002-matnr_classificacao c_zppt0002-charg c_zppt0002-cd_safra.

    RETURN.

  ELSE.
    c_erro_confirmacao = abap_true.
  ENDIF.


ENDFORM.

FORM f_get_registros_fardao.

  SELECT *
    FROM zppt0002 INTO TABLE it_zppt0002_fardao
  WHERE id_referencia    EQ p_id_ref
    AND status_registro  EQ '01'
    AND status           EQ 'R'.

  DELETE it_zppt0002_fardao WHERE id_sessao IS INITIAL.

ENDFORM.


FORM f_get_regitros_classificacao.

  SELECT * FROM zppt0002
    INTO TABLE it_zppt0002_classificar
  WHERE id_referencia    EQ p_id_ref
    AND status_registro  EQ '03'
    AND status           EQ 'R'.

  DELETE it_zppt0002_classificar WHERE dt_classificacao IS INITIAL.


ENDFORM.

FORM f_get_registros_transferencia.

  "Transferencia de Bloco/Material - 07 / 08 SUCESS0 - E8 Erro

  SELECT *
    FROM zppt0002 INTO TABLE it_zppt0002_transf
  WHERE id_referencia   EQ p_id_ref
    AND status_registro IN ( '07' )
    AND status          EQ 'R'.

ENDFORM.

FORM f_get_registros_estorno.

  SELECT *
    FROM zppt0002 INTO TABLE it_zppt0002_estorno
  WHERE id_referencia   EQ p_id_ref
    AND status_registro EQ '99'
    AND status          EQ 'R'.

ENDFORM.


FORM f_start_proc_transf TABLES t_return STRUCTURE bapiret2
                          USING p_zppt0002 TYPE zppt0002
                       CHANGING c_mara_origem  TYPE mara
                                c_mara_destino TYPE mara.

  DATA: lwa_mchb_origem TYPE mchb.

  CLEAR: t_return[], c_mara_origem, c_mara_destino.

  MESSAGE 'Start Processamento Transferencia...' TYPE 'S'.

  "Validar se movimentação já foi realizada
  PERFORM f_check_mov_realizada TABLES t_return USING p_zppt0002.
  CHECK t_return[] IS INITIAL.

  PERFORM f_get_saldo_bloco TABLES t_return
                             USING p_zppt0002-werks
                                   p_zppt0002-lgort
                                   p_zppt0002-charg
                          CHANGING gva_error
                                   lwa_mchb_origem
                                   c_mara_origem.

  CHECK gva_error IS INITIAL.

  SELECT SINGLE *
    FROM mara INTO c_mara_destino
   WHERE normt = p_zppt0002-cd_classificacao_bloco_destino
     AND mtart = 'ZFER'.

  IF sy-subrc NE 0.
    gva_msg_aux = |Cadastro de material para classificação { p_zppt0002-cd_classificacao_bloco_destino } não encontrado!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

*-------------------------------------------------------------------------------------------------------------*
* Validações Bloco Origem
*-------------------------------------------------------------------------------------------------------------*

  "Verificar se apos a movimentação, o peso do bloco Origem no SAP ficará com a mesmo peso do trace cotton
  PERFORM f_check_peso_bloco_sap_trace  TABLES t_return
                                         USING c_mara_origem-matnr
                                               p_zppt0002-werks
                                               p_zppt0002-lgort
                                               p_zppt0002-charg
                                               p_zppt0002-cd_safra
                                               p_zppt0002-menge
                                               p_zppt0002-peso_liq_atual_bloco
                                               '-'
                                      CHANGING gva_error.
  CHECK gva_error IS INITIAL.

*-------------------------------------------------------------------------------------------------------------*
* Validações Bloco Destino
*-------------------------------------------------------------------------------------------------------------*

  IF p_zppt0002-lgort NE p_zppt0002-bloco_destino.

    "Checar se tem saldo no bloco destino com um material diferente da classificação do Trace
    PERFORM f_bloco_permite_class_material TABLES t_return
                                            USING c_mara_destino-matnr
                                                  p_zppt0002-werks
                                                  p_zppt0002-bloco_destino
                                                  p_zppt0002-charg
                                         CHANGING gva_error.
    CHECK gva_error IS INITIAL.

  ELSEIF ( p_zppt0002-lgort EQ p_zppt0002-bloco_destino ).

    IF c_mara_origem-matnr EQ c_mara_destino-matnr.
      gva_msg_aux = |Troca não permtida! Bloco: { p_zppt0002-lgort } / Material Atual: { c_mara_origem-matnr } / Material Destino: { c_mara_destino-matnr } |.
      PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
      RETURN.
    ENDIF.

    IF ( p_zppt0002-menge NE lwa_mchb_origem-clabs ).
      "Se tiver trocando o tipo do bloco, tem que trocar a quantidade total
      gva_msg_aux = |Não é permitida uma troca parcial de tipo! Bloco: { p_zppt0002-lgort } / Quantidade Saldo: { lwa_mchb_origem-clabs } / Quantidade Troca: { p_zppt0002-menge } |.
      PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
      RETURN.
    ENDIF.

  ENDIF.

  "Verificar se apos a movimentação, o peso do bloco Destino no SAP ficará com o mesmo peso do Trace Cotton
  PERFORM f_check_peso_bloco_sap_trace  TABLES t_return
                                         USING c_mara_destino-matnr
                                               p_zppt0002-werks
                                               p_zppt0002-bloco_destino
                                               p_zppt0002-charg
                                               p_zppt0002-cd_safra
                                               p_zppt0002-menge
                                               p_zppt0002-peso_liq_atual_bloco_destino
                                               '+'
                                      CHANGING gva_error.
  CHECK gva_error IS INITIAL.

*-------------------------------------------------------------------------------------------------------------*
* Validações Saldo Consumo
*-------------------------------------------------------------------------------------------------------------*
  PERFORM f_check_saldo_consumo TABLES t_return
                                 USING c_mara_origem-matnr              "Material
                                       p_zppt0002-werks                 "Centro
                                       p_zppt0002-lgort                 "Deposito
                                       p_zppt0002-charg                 "Lote
                                       p_zppt0002-menge                 "Quantidade Consumo
                              CHANGING gva_possui_saldo. "Possui Saldo?

  IF gva_possui_saldo IS INITIAL.
    RETURN.
  ENDIF.



*  CASE p_zppt0002-status_registro.
*    WHEN '07'. "Troca Bloco / Troca Material
*
*
*      IF p_zppt0002-cd_classificacao NE p_zppt0002-cd_classificacao_bloco_destino. "Se classificação origem for diferente de classificação destino
*        gva_msg_aux = |Classificação do bloco origem { p_zppt0002-cd_classificacao } diferente do bloco destino: { p_zppt0002-cd_classificacao_bloco_destino }!|.
*        PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
*        RETURN.
*      ENDIF.
*
*
*    WHEN '09'. "Troca Material
*
*      "Se quantidade da troca for diferente do saldo atual do bloco, não deve permitir, pois a quantidade deve ser trocada na sua totalidade
*      IF p_zppt0002-menge NE lwa_mchb-clabs.
*        gva_msg_aux = |Não é possivel realizar uma troca de quantidade parcial do bloco. Saldo Bloco { lwa_mchb-clabs } / Quantidade para troca: { p_zppt0002-menge }!|.
*        PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
*        RETURN.
*      ENDIF.
*
*      SELECT SINGLE *
*        FROM mara INTO c_mara_destino
*       WHERE normt = p_zppt0002-cd_classificacao
*         AND mtart = 'ZFER'.
*
*      IF sy-subrc NE 0.
*        gva_msg_aux = |Cadastro de material para classificação { p_zppt0002-cd_classificacao } não encontrado!|.
*        PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
*        RETURN.
*      ENDIF.
*
*    WHEN OTHERS.
*      EXIT.
*  ENDCASE.


ENDFORM.

FORM f_after_transferencia TABLES t_itens_goods  STRUCTURE bapi2017_gm_item_create
                                  t_return       STRUCTURE bapiret2
                            USING p_material     TYPE bapi2017_gm_head_ret-mat_doc
                                  p_year         TYPE bapi2017_gm_head_ret-doc_year
                         CHANGING c_zppt0002     TYPE zppt0002.

  DATA: lva_material TYPE bapi2017_gm_item_create-material.

  DATA: lit_mseg TYPE TABLE OF mseg.

  DATA: lwa_blpp         LIKE blpp.
  DATA: lwa_blpp_estorno LIKE blpp.

  DATA: lva_confirmation_es TYPE bapi_rm_datkey-cancconfirmation.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = c_x.

  gva_msg_aux = | { p_material } / {  p_year }  |.
  CASE c_zppt0002-status_registro.
    WHEN '07'.
      DATA(lva_status_registro) = '08'.
      gva_msg_aux = |Documento { p_material } / {  p_year } Transf. de material/bloco gerado|.
    WHEN '09'.
      lva_status_registro = '10'.
      gva_msg_aux = |Documento { p_material } / {  p_year } Transf. de material/bloco Gerado|.
    WHEN OTHERS.
  ENDCASE.

  c_zppt0002-mblnr       = p_material.
  c_zppt0002-mjahr       = p_year.
  c_zppt0002-cd_mensagem = gva_msg_aux.

  UPDATE zppt0002 SET status_registro = lva_status_registro
                      mblnr           = p_material
                      mjahr           = p_year
                WHERE acharg      = c_zppt0002-acharg
                  AND werks       = c_zppt0002-werks
                  AND id_sessao   = c_zppt0002-id_sessao
                  AND lgort       = c_zppt0002-lgort
                  AND cd_safra    = c_zppt0002-cd_safra.

  PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'S'.
  RETURN.


ENDFORM.

FORM f_check_mov_realizada TABLES t_return STRUCTURE bapiret2
                            USING p_zppt0002 TYPE zppt0002.

  DATA: lit_mkpf_exists TYPE TABLE OF mkpf.

  CLEAR: lit_mkpf_exists[].

  IF p_zppt0002-id_mov_sistema_origem_ref_int IS INITIAL.
    gva_msg_aux = |Registro sem Id. Movimentação Interna!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  SELECT *
    FROM mkpf AS c INTO TABLE lit_mkpf_exists
   WHERE bktxt EQ p_zppt0002-id_mov_sistema_origem_ref_int
     "Eliminando documentos de estorno
     AND NOT EXISTS ( SELECT i~mblnr
                        FROM mseg AS i
                       WHERE i~mblnr EQ c~mblnr
                         AND i~smbln NE space
                         AND i~smbln NE '0000000000' )
    AND NOT EXISTS ( SELECT i~mblnr
                       FROM mseg AS i
                      WHERE i~smbln EQ c~mblnr ).

  READ TABLE lit_mkpf_exists INTO DATA(lwa_mkpf_exists) INDEX 1.

  IF sy-subrc EQ 0 AND lit_mkpf_exists[] IS NOT INITIAL AND lwa_mkpf_exists-mblnr IS NOT INITIAL.
    gva_msg_aux = |Movimentação Id: { p_zppt0002-id_mov_sistema_origem_ref_int } já foi realizada! Doc.Material { lwa_mkpf_exists-mblnr } / { lwa_mkpf_exists-mjahr }|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

ENDFORM.

FORM f_estorno.

  DATA: lit_zppt0002_check TYPE TABLE OF zppt0002,
        lit_return_erros   TYPE TABLE OF bapiret2.

*-----------------------------------------------------------------------------*
* Validar Possiveis erros antes de iniciar o estorno da sessão - Inicio
*-----------------------------------------------------------------------------*
  CLEAR: lit_return_erros[].

  PERFORM f_fardinho_estorno TABLES lit_return_erros
                              USING abap_true. "Simulação Erros Estorno
  IF lit_return_erros[] IS INITIAL.
    PERFORM f_fardao_estorno TABLES lit_return_erros
                              USING abap_true. "Simulação Erros Estorno
  ENDIF.

  IF lit_return_erros[] IS NOT INITIAL.
    CLEAR: lit_zppt0002_check[].

    SELECT *
      FROM zppt0002 INTO TABLE lit_zppt0002_check
     WHERE id_referencia   EQ p_id_ref.

    LOOP AT lit_zppt0002_check INTO DATA(lwa_zppt0002_check).
      PERFORM f_registra_processamento TABLES lit_return_erros USING lwa_zppt0002_check.
    ENDLOOP.

    RETURN.
  ENDIF.

*-----------------------------------------------------------------------------*
* Iiniciar processamento estorno caso náo encontrar erro na pre validação
*-----------------------------------------------------------------------------*

  PERFORM f_fardinho_estorno TABLES lit_return_erros
                              USING abap_false. "Simulação Erros Estorno

  CLEAR: lit_zppt0002_check[].
  SELECT *
    FROM zppt0002 INTO TABLE lit_zppt0002_check
   WHERE id_referencia   EQ p_id_ref.

  LOOP AT lit_zppt0002_check INTO lwa_zppt0002_check WHERE id_sessao IS INITIAL
                                                       AND ( status_processamento NE 'C' OR  "Existe processamento de estorno de classificação não concluido
                                                             status_registro      NE '06' ). "Existe documento de estorno que não deu sucesso
    RETURN. "Não prosseguir para estorno da sessão caso ainda tem classificação para estornar...
  ENDLOOP.

  PERFORM f_fardao_estorno TABLES lit_return_erros
                            USING abap_false. "Simulação Erros Estorno


ENDFORM.

FORM f_fardinho_estorno TABLES t_return STRUCTURE bapiret2
                         USING p_simula_erro_estorno.

  DATA: lva_is_block      TYPE char01.

  DATA: lwa_bflushflags   LIKE bapi_rm_flg,
        lwa_bflushdatagen LIKE bapi_rm_datgen,
        lva_confirmation  LIKE bapi_rm_datkey-confirmation.

  DATA: lit_goodsmovements TYPE TABLE OF bapi2017_gm_item_create,
        lwa_goodsmovements TYPE bapi2017_gm_item_create.

  DATA: lit_return TYPE TABLE OF bapiret2,
        lwa_return TYPE bapiret2.

  DATA: lva_erro_confirmacao     TYPE c,
        lva_estornou_confirmacao TYPE c.

  DATA: lwa_mkal_origem  TYPE mkal,
        lwa_mkal_destino TYPE mkal,
        lwa_mara_origem  TYPE mara,
        lwa_mara_destino TYPE mara.

  DATA(lit_fardinhos_estorno) = it_zppt0002_estorno[].
  DELETE lit_fardinhos_estorno WHERE id_sessao IS NOT INITIAL.

  IF p_simula_erro_estorno EQ abap_true.
    CLEAR: t_return[].

    LOOP AT lit_fardinhos_estorno INTO DATA(lwa_zppt0002).
      PERFORM f_fardinho_estorno_start_proc TABLES t_return
                                          CHANGING lwa_zppt0002
                                                   lwa_mkal_origem
                                                   lwa_mkal_destino
                                                   lwa_mara_origem
                                                   lwa_mara_destino.
      IF t_return[] IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDLOOP.
    RETURN.
  ENDIF.

  LOOP AT lit_fardinhos_estorno INTO lwa_zppt0002.

    CLEAR: lit_goodsmovements[], lwa_bflushflags, lwa_bflushdatagen, lwa_return,  lva_erro_confirmacao, lva_estornou_confirmacao.

    PERFORM f_fardinho_estorno_start_proc TABLES lit_return
                                        CHANGING lwa_zppt0002
                                                 lwa_mkal_origem
                                                 lwa_mkal_destino
                                                 lwa_mara_origem
                                                 lwa_mara_destino.
    IF lit_return[] IS NOT INITIAL.
      PERFORM f_set_registro_for_reproc TABLES lit_return USING lwa_zppt0002 '01'.
      PERFORM f_email_alerta_erro_estorno USING lwa_zppt0002.
      CONTINUE.
    ENDIF.

    CHECK lwa_zppt0002-wait_estorno_confirmation = abap_false. "Não prosseguir se for registro aguardando estorno de confirmação incorreta

    lwa_bflushflags-bckfltype        = '21'.
    lwa_bflushdatagen-docheadertxt   = lwa_zppt0002-id_mov_sistema_origem_ref_int.
    lwa_bflushdatagen-postdate       = sy-datum.
    lwa_bflushdatagen-docdate        = sy-datum.
    lwa_bflushdatagen-prodplant      = lwa_zppt0002-werks.
    lwa_bflushdatagen-materialnr     = lwa_mara_destino-matnr.
    lwa_bflushdatagen-backflquant    = lwa_zppt0002-menge.
    lwa_bflushdatagen-unitofmeasure  = lwa_mara_destino-meins.
    lwa_bflushdatagen-prodversion    = lwa_zppt0002-verid.
    lwa_bflushdatagen-batch          = lwa_zppt0002-charg.
    lwa_bflushdatagen-storageloc     = lwa_zppt0002-lgort.

    CLEAR: lwa_goodsmovements.
    lwa_goodsmovements-material      = lwa_zppt0002-matnr.
    lwa_goodsmovements-plant         = lwa_zppt0002-werks.
    lwa_goodsmovements-stge_loc      = COND #( WHEN lwa_mkal_origem-elpro IS NOT INITIAL THEN lwa_mkal_origem-elpro ELSE lwa_mkal_origem-alort )..
    lwa_goodsmovements-batch         = lwa_zppt0002-charg.
    lwa_goodsmovements-move_type     = '262'.
    lwa_goodsmovements-entry_qnt     = lwa_zppt0002-menge.
    lwa_goodsmovements-entry_uom     = lwa_mara_origem-meins.
    APPEND lwa_goodsmovements TO lit_goodsmovements.


    DO 6 TIMES.

      DATA(_index) = sy-index.

      CLEAR: lwa_return, lva_is_block, lva_confirmation.

      CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'
        EXPORTING
          bflushflags    = lwa_bflushflags
          bflushdatagen  = lwa_bflushdatagen
        IMPORTING
          confirmation   = lva_confirmation
          return         = lwa_return
        TABLES
          goodsmovements = lit_goodsmovements.

      IF lva_confirmation IS INITIAL.

        CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
          EXPORTING
            id       = lwa_return-id
            number   = lwa_return-number
          IMPORTING
            is_block = lva_is_block.

        IF lva_is_block IS NOT INITIAL AND _index LE 5.
          MESSAGE 'Confirmação não criada por erro de bloqueio' TYPE 'S'.
          WAIT UP TO 2 SECONDS.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          APPEND lwa_return TO lit_return.
          EXIT.
        ENDIF.

      ELSE.
        PERFORM f_after_entrada_fardinho TABLES lit_goodsmovements
                                                lit_return
                                          USING lva_confirmation
                                                abap_true
                                       CHANGING lwa_zppt0002
                                                lva_erro_confirmacao.


        IF lva_erro_confirmacao EQ abap_true.

          PERFORM f_estorno_confirmacao TABLES lit_return
                                         USING lva_confirmation
                                               lwa_zppt0002
                                      CHANGING lva_estornou_confirmacao.

        ENDIF.

        EXIT.
      ENDIF.
    ENDDO.

    IF ( lva_erro_confirmacao     EQ abap_true  )   AND  "Deu erro ao gerar a confirmação
       ( lva_estornou_confirmacao EQ abap_false ).       "Não conseguiu estornar a confirmação gerada incorretamente
      CONTINUE. "Não registrar processamento de erro e tentar a confirmação na proximo processamento
    ENDIF.

    IF lva_is_block IS NOT INITIAL. "Se nao conseguiu criar movimentação por bloqueio de registro, nao marcar registro com erro e tentar reprocessaar novamente
      PERFORM f_set_registro_for_reproc TABLES lit_return USING lwa_zppt0002 '02'.
      CONTINUE.
    ENDIF.

    IF lva_confirmation IS INITIAL.  "Se pode prosseguir se conseguir estornar... se nao conseguir estornar , será disparado email para area de negocio resolver o problema
      PERFORM f_set_registro_for_reproc TABLES lit_return USING lwa_zppt0002 '01'.
      PERFORM f_email_alerta_erro_estorno USING lwa_zppt0002.
      CONTINUE.
    ENDIF.

    "Registrar processamento sucesso ou erro
    PERFORM f_registra_processamento TABLES lit_return USING lwa_zppt0002.

  ENDLOOP.

ENDFORM.                    " f_estorno_fardinhos

FORM f_fardao_estorno TABLES t_return STRUCTURE bapiret2
                       USING p_simula_erro_estorno.

  DATA: lwa_bflushflags   LIKE bapi_rm_flg,
        lwa_bflushdatagen LIKE bapi_rm_datgen,
        lva_confirmation  LIKE bapi_rm_datkey-confirmation.

  DATA: lit_goodsmovements TYPE TABLE OF bapi2017_gm_item_create,
        lwa_goodsmovements TYPE bapi2017_gm_item_create.

  DATA: lit_itens_lista_tec	TYPE TABLE OF mdpm,
        lwa_mkal            TYPE mkal,
        lit_return          TYPE TABLE OF bapiret2,
        lwa_return          TYPE bapiret2.

  DATA: lva_is_block         TYPE char01.

  DATA: lva_erro_confirmacao     TYPE c,
        lva_estornou_confirmacao TYPE c.

  DATA(lit_fardao_estorno) = it_zppt0002_estorno[].
  DELETE lit_fardao_estorno WHERE id_sessao IS INITIAL.

  IF p_simula_erro_estorno EQ abap_true.
    CLEAR: t_return[].

    LOOP AT lit_fardao_estorno INTO DATA(lwa_zppt0002).
      PERFORM f_fardao_estorno_start_proc TABLES t_return
                                                 lit_itens_lista_tec
                                           USING lwa_zppt0002
                                                 p_simula_erro_estorno
                                        CHANGING lwa_mkal.
      IF t_return[] IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDLOOP.
    RETURN.
  ENDIF.

  LOOP AT lit_fardao_estorno INTO lwa_zppt0002.

    CLEAR: lva_erro_confirmacao, lva_estornou_confirmacao.

*--------------------------------------------------------------------------------------------*
*   Validações
*--------------------------------------------------------------------------------------------*
    PERFORM f_fardao_estorno_start_proc TABLES lit_return
                                               lit_itens_lista_tec
                                         USING lwa_zppt0002
                                               p_simula_erro_estorno
                                      CHANGING lwa_mkal.

    IF lit_return[] IS NOT INITIAL.
      PERFORM f_set_registro_for_reproc TABLES lit_return USING lwa_zppt0002 '01'.
      PERFORM f_email_alerta_erro_estorno USING lwa_zppt0002.
      CONTINUE.
    ENDIF.

    CHECK lwa_zppt0002-wait_estorno_confirmation = abap_false. "Não prosseguir se for registro aguardando estorno de confirmação incorreta

*--------------------------------------------------------------------------------------------*
*   Preenchimento Dados BAPI
*--------------------------------------------------------------------------------------------*
    CLEAR: lwa_bflushflags, lwa_bflushdatagen, lwa_return, lit_goodsmovements[], lit_return[].

    lwa_bflushflags-bckfltype        = '21'.
    lwa_bflushdatagen-docheadertxt   = lwa_zppt0002-id_mov_sistema_origem_ref_int.
    lwa_bflushdatagen-postdate       = lwa_zppt0002-budat.
    lwa_bflushdatagen-docdate        = lwa_zppt0002-bldat.
    lwa_bflushdatagen-prodplant      = lwa_zppt0002-werks.      "Centro
    lwa_bflushdatagen-materialnr     = lwa_zppt0002-matnr.      "Material Produção - Pluma em Elaboração
    lwa_bflushdatagen-backflquant    = lwa_zppt0002-menge.      "Quantidade
    lwa_bflushdatagen-unitofmeasure  = 'KG'.                                    "UM
    lwa_bflushdatagen-prodversion    = lwa_zppt0002-verid.      "Versão
    lwa_bflushdatagen-batch          = lwa_zppt0002-charg.      "Lote
    lwa_bflushdatagen-storageloc     = COND #( WHEN lwa_mkal-elpro IS NOT INITIAL THEN lwa_mkal-elpro ELSE lwa_mkal-alort ).

    LOOP AT lit_itens_lista_tec INTO DATA(lwa_item_lista_tec).

      lwa_goodsmovements-material      = lwa_item_lista_tec-matnr.             "Material (MATNR)
      lwa_goodsmovements-entry_uom     = lwa_item_lista_tec-erfme.             "UM
      lwa_goodsmovements-stge_loc      = lwa_item_lista_tec-lgpro.             "Deposito Consumo (LGORT)'
      lwa_goodsmovements-plant         = lwa_zppt0002-werks.   "Centro (WERKS)'

      CASE lwa_item_lista_tec-shkzg.
        WHEN 'S'."Entrada Estoque


          "ALGODAO CAROCO PROD PROPRIA
          lwa_goodsmovements-move_type     = '262'.
          lwa_goodsmovements-batch         = lwa_zppt0002-charg.
          lwa_goodsmovements-entry_qnt     = lwa_zppt0002-peso_algodao_caroco.

        WHEN 'H'."Saida Estoque

          lwa_goodsmovements-move_type     = '532'.
          lwa_goodsmovements-batch         = lwa_zppt0002-charg.

          IF ( lwa_item_lista_tec-matnr = '000000000000120166' ) AND "120166 - FIBRILHA
             ( lwa_zppt0002-peso_fibrilha GT 0 ).

            lwa_goodsmovements-entry_qnt = lwa_zppt0002-peso_fibrilha.

          ELSEIF ( lwa_item_lista_tec-matnr = '000000000000120168' ) AND "120168 - CAROÇO ALGODAO PROD PROPRIA
                 ( lwa_zppt0002-peso_caroco GT 0 ).

            lwa_goodsmovements-entry_qnt = lwa_zppt0002-peso_caroco.

          ENDIF.
      ENDCASE.

      APPEND lwa_goodsmovements TO lit_goodsmovements.

    ENDLOOP.

    DO 6 TIMES.

      DATA(_index) = sy-index.

      CLEAR: lwa_return, lva_is_block, lva_confirmation.

      CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          bflushflags    = lwa_bflushflags
          bflushdatagen  = lwa_bflushdatagen
        IMPORTING
          confirmation   = lva_confirmation
          return         = lwa_return
        TABLES
          goodsmovements = lit_goodsmovements.

      IF lva_confirmation IS INITIAL.

        CALL FUNCTION 'ZMM_CHECK_MENSAGEM_BLOQUEIO'
          EXPORTING
            id       = lwa_return-id
            number   = lwa_return-number
          IMPORTING
            is_block = lva_is_block.

        IF lva_is_block IS NOT INITIAL AND _index LE 5.
          MESSAGE 'Confirmação não criada por erro de bloqueio' TYPE 'S'.
          WAIT UP TO 2 SECONDS.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          APPEND lwa_return TO lit_return.
          EXIT.
        ENDIF.

      ELSE.
        PERFORM f_after_entrada_fardao TABLES lit_goodsmovements
                                              lit_return
                                        USING lva_confirmation
                                              abap_true
                                     CHANGING lwa_zppt0002
                                              lva_erro_confirmacao.

        IF lva_erro_confirmacao EQ abap_true.

          PERFORM f_estorno_confirmacao TABLES lit_return
                                         USING lva_confirmation
                                               lwa_zppt0002
                                      CHANGING lva_estornou_confirmacao.

        ENDIF.

        EXIT.
      ENDIF.

    ENDDO.

    IF ( lva_erro_confirmacao     EQ abap_true  )   AND  "Deu erro ao gerar a confirmação
       ( lva_estornou_confirmacao EQ abap_false ).       "Não conseguiu estornar a confirmação gerada incorretamente
      CONTINUE. "Não registrar processamento de erro e tentar a confirmação na proximo processamento
    ENDIF.

    IF lva_is_block IS NOT INITIAL. "Se nao conseguiu criar movimentação por bloqueio de registro, nao marcar registro com erro e tentar reprocessaar novamente
      PERFORM f_set_registro_for_reproc TABLES lit_return USING lwa_zppt0002 '02'.
      CONTINUE.
    ENDIF.

    IF lva_confirmation IS INITIAL.  "Se pode prosseguir se conseguir estornar... se nao conseguir estornar , será disparado email para area de negocio resolver o problema
      PERFORM f_set_registro_for_reproc TABLES lit_return USING lwa_zppt0002 '01'.
      PERFORM f_email_alerta_erro_estorno USING lwa_zppt0002.
      CONTINUE.
    ENDIF.

    "Registrar processamento sucesso ou erro
    PERFORM f_registra_processamento TABLES lit_return USING lwa_zppt0002.

  ENDLOOP.

ENDFORM .


FORM f_fardao_estorno_start_proc TABLES t_return              STRUCTURE bapiret2
                                        t_itens_lista_tecnica STRUCTURE mdpm
                                  USING p_zppt0002 TYPE zppt0002
                                        p_simula_erro_estorno
                               CHANGING c_mkal     TYPE mkal.

  DATA: lit_mkpf_exists TYPE TABLE OF mkpf.

  DATA: lwa_plaf   TYPE plaf,
        lwa_mt61d  LIKE mt61d,
        lva_cm61m  LIKE cm61m,
        lva_xscrap TYPE xfeld,
        lva_eselid LIKE af61z-selid.

  DATA: lva_menge_consumo TYPE mchb-clabs,
        lva_charg_consumo TYPE mchb-charg.

  CLEAR: t_return[], t_itens_lista_tecnica.

  MESSAGE 'Start Processamento Fardão Estorno...' TYPE 'S'.

  IF ( p_zppt0002-wait_estorno_confirmation EQ abap_true ). "Confirmação gerada incorretamente... Tentar estornar antes de retornar para o Trace
    DATA(_estornou_confirmacao) = abap_false.
    PERFORM f_estorno_confirmacao TABLES t_return
                                   USING p_zppt0002-prtnr_estorno
                                         p_zppt0002
                                CHANGING _estornou_confirmacao.
    RETURN.
  ENDIF.

  "Validar se movimentação já foi realizada
  PERFORM f_check_mov_realizada TABLES t_return USING p_zppt0002.
  CHECK t_return[] IS INITIAL.

  IF p_zppt0002-peso_algodao_caroco IS INITIAL.
    gva_msg_aux = |Peso Algodao em Caroco não informado !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF p_zppt0002-peso_caroco IS INITIAL.
    gva_msg_aux = |Peso Caroco de Algodao não informado !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF p_zppt0002-peso_fibrilha IS INITIAL.
    gva_msg_aux = |Peso Fibrilha não informado !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF p_zppt0002-menge IS INITIAL.
    gva_msg_aux = |Peso Pluma em Elaboração não informado !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE cslid
    FROM t399d INTO lva_eselid
   WHERE werks EQ p_zppt0002-werks.

  IF sy-subrc NE 0 OR lva_eselid IS INITIAL.
    gva_msg_aux = |Não foi possivel obter os parâmetros de controle para MRP Filial: { p_zppt0002-werks } !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  CLEAR: c_mkal.
  SELECT SINGLE *
    FROM mkal INTO c_mkal
   WHERE werks = p_zppt0002-werks
     AND matnr = p_zppt0002-matnr
     AND verid = p_zppt0002-verid.

  IF sy-subrc NE 0.
    gva_msg_aux = |Versão de produção não encontrada! Centro: { p_zppt0002-werks } Material: { p_zppt0002-matnr } Versão: { p_zppt0002-verid } !|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.


  CLEAR: lwa_plaf.
  lwa_plaf-matnr = p_zppt0002-matnr.
  lwa_plaf-plwrk = p_zppt0002-werks.
  lwa_plaf-pwwrk = p_zppt0002-werks.
  lwa_plaf-gsmng = p_zppt0002-menge.
  lwa_plaf-psttr = p_zppt0002-budat.
  lwa_plaf-verid = p_zppt0002-verid.

  CALL FUNCTION 'MD_AUFLOESUNG_PLANAUFTRAG'
    EXPORTING
      eplaf         = lwa_plaf
      emt61d        = lwa_mt61d
      eselid        = lva_eselid
      ecm61m        = lva_cm61m
      eno_scrap     = lva_xscrap
    IMPORTING
      iplaf         = lwa_plaf
    TABLES
      mdpmx         = t_itens_lista_tecnica
    EXCEPTIONS
      error_message = 1.

  IF sy-subrc NE 0 OR t_itens_lista_tecnica[] IS INITIAL.
    gva_msg_aux = 'Não foi possivel obter a lista tecnica para produção!'.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  "Verifica estoque Pluma em Elaboração para estorno
  IF p_simula_erro_estorno EQ abap_false.
    DATA(_deposito_check) = COND #( WHEN c_mkal-elpro IS NOT INITIAL THEN c_mkal-elpro ELSE c_mkal-alort ).

    PERFORM f_check_saldo_consumo TABLES t_return
                                    USING p_zppt0002-matnr  "Material
                                          p_zppt0002-werks  "Centro
                                          _deposito_check   "Deposito
                                          p_zppt0002-charg  "Lote
                                          p_zppt0002-menge  "Quantidade Consumo
                                 CHANGING gva_possui_saldo. "Possui Saldo?

    IF gva_possui_saldo IS INITIAL.
      RETURN.
    ENDIF.
  ENDIF.

  "Verifica estoque Pluma Caroco e Fibrilha
  LOOP AT t_itens_lista_tecnica ASSIGNING FIELD-SYMBOL(<fs_item_lista_tec>).

    "Inverte Sinal
    <fs_item_lista_tec>-shkzg = COND #( WHEN <fs_item_lista_tec>-shkzg = 'H' THEN 'S' ELSE 'H' ).

    CASE <fs_item_lista_tec>-shkzg.
      WHEN 'H'. "Movimento Saida

        CLEAR: lva_charg_consumo, lva_menge_consumo.

        CASE <fs_item_lista_tec>-matnr.
          WHEN '000000000000120166'.  "FIBRILHA.

            lva_charg_consumo = p_zppt0002-charg.
            lva_menge_consumo = p_zppt0002-peso_fibrilha.

          WHEN '000000000000120168'.  "CAROÇO ALGODAO PROD PROPRIA.

            lva_charg_consumo = p_zppt0002-charg.
            lva_menge_consumo = p_zppt0002-peso_caroco.

          WHEN OTHERS.
            gva_msg_aux = |Não previsto baixa de estoque para o material: { <fs_item_lista_tec>-matnr } |.
            PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
            RETURN.
        ENDCASE.

        PERFORM f_check_saldo_consumo TABLES t_return
                                       USING <fs_item_lista_tec>-matnr
                                             p_zppt0002-werks
                                             <fs_item_lista_tec>-lgpro
                                             lva_charg_consumo
                                             lva_menge_consumo
                                    CHANGING gva_possui_saldo.

        IF gva_possui_saldo IS INITIAL.
          RETURN.
        ENDIF.

      WHEN 'S'. "Movimento Entrada
    ENDCASE.
  ENDLOOP.

  DATA(lit_itens_aux) = t_itens_lista_tecnica[].
  DELETE lit_itens_aux WHERE shkzg NE 'H'.

  IF lines( lit_itens_aux ) NE 2.
    DATA(_lines) = lines( lit_itens_aux ).
    gva_msg_aux = |Lista Tecnica com { _lines } movimentos de baixa! Deveria haver 2 movimentos de baixa!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF lines( t_itens_lista_tecnica ) NE 3.
    gva_msg_aux = 'Lista Tecnica com parametrização de movimentação incompleta!'.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

ENDFORM.

FORM f_set_registro_to_estorno USING p_zppt0002 TYPE zppt0002
                                     p_confirmation TYPE zppt0002-prtnr_estorno.

  CHECK p_confirmation IS NOT INITIAL AND p_zppt0002 IS NOT INITIAL.

  SELECT SINGLE *
    FROM zppt0002 INTO @DATA(lwa_zppt002)
   WHERE acharg      = @p_zppt0002-acharg
     AND werks       = @p_zppt0002-werks
     AND id_sessao   = @p_zppt0002-id_sessao
     AND lgort       = @p_zppt0002-lgort
     AND cd_safra    = @p_zppt0002-cd_safra.

  CHECK lwa_zppt002-wait_estorno_confirmation EQ abap_false.

  UPDATE zppt0002
     SET wait_estorno_confirmation = abap_true
         prtnr_estorno             = p_confirmation
   WHERE acharg      = p_zppt0002-acharg
     AND werks       = p_zppt0002-werks
     AND id_sessao   = p_zppt0002-id_sessao
     AND lgort       = p_zppt0002-lgort
     AND cd_safra    = p_zppt0002-cd_safra.

  COMMIT WORK.

ENDFORM.

FORM f_estorno_confirmacao TABLES t_return       STRUCTURE bapiret2
                            USING p_confirmation TYPE zppt0002-prtnr_estorno
                                  p_zppt0002     TYPE zppt0002
                         CHANGING c_estornado    TYPE c.

  DATA: lva_confirmation_es TYPE bapi_rm_datkey-cancconfirmation,
        lwa_return          TYPE bapiret2.

  MESSAGE 'Estornar confirmação...' TYPE 'S'.

  CHECK p_confirmation IS NOT INITIAL AND p_zppt0002 IS NOT INITIAL.

  CLEAR: lva_confirmation_es, c_estornado.

  CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
    EXPORTING
      confirmation     = p_confirmation
    IMPORTING
      cancconfirmation = lva_confirmation_es
      return           = lwa_return.

  IF lva_confirmation_es IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    c_estornado = abap_true.

    gva_msg_aux = |Documento de confirmação gerado/estornado: { p_confirmation }  /  Doc.Estorno: { lva_confirmation_es } |.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.

  ELSE.
    PERFORM f_set_registro_to_estorno USING p_zppt0002 p_confirmation.
    RETURN.
  ENDIF.


ENDFORM.

FORM f_check_saldo_consumo  TABLES t_return STRUCTURE bapiret2
                             USING p_matnr
                                   p_werks
                                   p_lgort
                                   p_charg
                                   p_consumo
                          CHANGING c_possui_saldo.

  DATA: lva_saldo   TYPE mchb-clabs.
  DATA: lva_matnr   TYPE mchb-matnr.

  CLEAR: c_possui_saldo.

  SELECT SINGLE clabs
    FROM mchb INTO lva_saldo
   WHERE matnr = p_matnr
     AND werks = p_werks
     AND lgort = p_lgort
     AND charg = p_charg.

  lva_matnr = p_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = lva_matnr
    IMPORTING
      output = lva_matnr.

  IF p_consumo > lva_saldo.
    gva_msg_aux = |Material: { lva_matnr } Centro: { p_werks } Deposito: { p_lgort }|.
    gva_msg_aux = |{ gva_msg_aux } Lote: { p_charg } com saldo insuficiente para consumo! Saldo: { lva_saldo } - Consumo: { p_consumo }|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ELSE.
    c_possui_saldo = abap_true.
  ENDIF.

ENDFORM.


FORM f_check_peso_bloco_sap_trace  TABLES t_return STRUCTURE bapiret2
                                    USING p_matnr
                                          p_werks
                                          p_lgort
                                          p_charg
                                          p_safra
                                          p_qtde_movimentacao
                                          p_peso_bloco_trace
                                          p_operador
                                 CHANGING c_error.

  DATA: lva_saldo_mb51             TYPE mchb-clabs.
  DATA: lva_qtde_movimentacao      TYPE mchb-clabs.
  DATA: lva_qtde_embarcada_zmm0023 TYPE zmmt0008-menge.
  DATA: lva_qtde_total_bloco       TYPE zmmt0008-menge.

  DATA: lva_matnr   TYPE mchb-matnr.

  CLEAR: c_error, lva_saldo_mb51 , lva_qtde_embarcada_zmm0023.

  SELECT SINGLE clabs
    FROM mchb INTO lva_saldo_mb51
   WHERE matnr = p_matnr
     AND werks = p_werks
     AND lgort = p_lgort
     AND charg = p_charg.

  SELECT SUM( menge )
    FROM zmmt0008 INTO lva_qtde_embarcada_zmm0023
   WHERE werks = p_werks
     AND lgort = p_lgort
     AND safra = p_safra.

  CASE p_operador.
    WHEN '+'.
      lva_qtde_movimentacao = abs( p_qtde_movimentacao ).
    WHEN '-'.
      lva_qtde_movimentacao = abs( p_qtde_movimentacao ) * -1.
    WHEN OTHERS.
      c_error = abap_true.
      gva_msg_aux = |'Operador nao previsto!' |.
      PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
      RETURN.
  ENDCASE.

  lva_qtde_total_bloco =  lva_saldo_mb51 + lva_qtde_embarcada_zmm0023 + lva_qtde_movimentacao.

  lva_matnr = p_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = lva_matnr
    IMPORTING
      output = lva_matnr.

  IF lva_qtde_total_bloco NE p_peso_bloco_trace.
    c_error = abap_true.
    gva_msg_aux = |Com essa movimentação, o Bloco: { p_lgort } ficará com Peso Total: { lva_qtde_total_bloco } no SAP, e no TraceCotton com Peso Total: { p_peso_bloco_trace } |.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

ENDFORM.


*FORM f_change_reg_bloco_destino  USING p_zppt0002 TYPE zppt0002.
*
*
*  SELECT SINGLE *
*    FROM zppt0002 INTO @DATA(lwa_zppt0002)
*   WHERE acharg    = @p_zppt0002-acharg
*     AND werks     = @p_zppt0002-werks
*     AND id_sessao = @p_zppt0002-id_sessao
*     AND lgort     = @p_zppt0002-lgort
*     AND cd_safra  = @p_zppt0002-cd_safra.
*
*  CHECK sy-subrc EQ 0 AND lwa_zppt0002-bloco_destino IS NOT INITIAL.
*
*  lwa_zppt0002-lgort                      = lwa_zppt0002-bloco_destino.
*  lwa_zppt0002-cd_classificacao           = lwa_zppt0002-cd_classificacao_bloco_destino.
*  lwa_zppt0002-qtd_fardinhos_bloco        = lwa_zppt0002-qtde_fardinhos_bloco_destino.
*  lwa_zppt0002-cd_mensagem                = p_zppt0002-cd_mensagem.
*  lwa_zppt0002-status_processamento       = 'C'. "Concluido
*  lwa_zppt0002-status_ret_sistema_origem  = 'C'. "Concluido
*  lwa_zppt0002-status                     = 'P'.
*
*  CLEAR: lwa_zppt0002-bloco_destino,
*         lwa_zppt0002-cd_classificacao_bloco_destino,
*         lwa_zppt0002-qtde_fardinhos_bloco_destino,
*         lwa_zppt0002-id_referencia.
*
*  SELECT SINGLE *
*    FROM zppt0002 INTO @DATA(lwa_zppt0002_check)
*   WHERE acharg    = @lwa_zppt0002-acharg
*     AND werks     = @lwa_zppt0002-werks
*     AND id_sessao = @lwa_zppt0002-id_sessao
*     AND lgort     = @lwa_zppt0002-lgort
*     AND cd_safra  = @lwa_zppt0002-cd_safra.
*
*  IF sy-subrc NE 0.
*    MODIFY zppt0002 FROM lwa_zppt0002.
*  ELSE.
*    UPDATE zppt0002 SET qtd_fardinhos_bloco = lwa_zppt0002-qtd_fardinhos_bloco
*     WHERE acharg    = lwa_zppt0002-acharg
*       AND werks     = lwa_zppt0002-werks
*       AND id_sessao = lwa_zppt0002-id_sessao
*       AND lgort     = lwa_zppt0002-lgort
*       AND cd_safra  = lwa_zppt0002-cd_safra.
*  ENDIF.
*
*  COMMIT WORK.
*
*ENDFORM.


FORM f_check_change_safra_lote USING p_matnr
                                     p_charg
                                     p_safra.

  DATA: lwa_safra_lote        TYPE zmme_cl.

  DATA: lva_objecttable TYPE bapi1003_key-objecttable,
        lva_classnum    TYPE bapi1003_key-classnum,
        lva_classtype   TYPE bapi1003_key-classtype,
        lit_allocvalues TYPE TABLE OF  bapi1003_alloc_values_char,
        lwa_allocvalues TYPE bapi1003_alloc_values_char,
        lit_allocnum    TYPE TABLE OF  bapi1003_alloc_values_num,
        lit_alloccur    TYPE TABLE OF  bapi1003_alloc_values_curr,
        lit_return2     TYPE TABLE OF bapiret2,
        lva_objek       TYPE inob-objek,
        lva_objek2      TYPE objnum.

  DATA: lva_safra TYPE mchb-charg.

  CHECK p_matnr  IS NOT INITIAL AND
        p_charg IS NOT INITIAL AND
        p_safra  IS NOT INITIAL.

  CLEAR: lwa_safra_lote.

  READ TABLE git_safra_lote INTO lwa_safra_lote WITH KEY matnr = p_matnr
                                                                 charg = p_charg.
  IF sy-subrc NE 0.
    CLEAR: lva_safra.
    PERFORM f_get_safra_lote USING p_matnr p_charg CHANGING lva_safra.

    lwa_safra_lote-matnr = p_matnr.
    lwa_safra_lote-charg = p_charg.
    lwa_safra_lote-atwrt = lva_safra.
    APPEND lwa_safra_lote TO git_safra_lote.
  ENDIF.

  CHECK p_safra NE lwa_safra_lote-atwrt. "Se caracteristica safra do material/lote estiver diferente do trace, devemos atualizar no msc2n

  lva_objecttable = 'MCH1'.
  lva_classnum    = 'FARDOES'.
  lva_classtype   = '023'.
  lva_objek       = p_matnr && p_charg.

  REFRESH lit_allocvalues.

  lwa_allocvalues-charact        = 'SAFRA'.
  lwa_allocvalues-value_char     = p_safra.
  lwa_allocvalues-value_neutral  = p_safra.
  lwa_allocvalues-charact_descr  = 'Safra'.
  APPEND lwa_allocvalues TO lit_allocvalues.

  IF strlen( lva_objek ) > 50.
    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      EXPORTING
        objecttable        = lva_objecttable
        classnum           = lva_classnum
        classtype          = lva_classtype
        objectkey_long     = lva_objek
      TABLES
        allocvaluesnumnew  = lit_allocnum
        allocvaluescharnew = lit_allocvalues
        allocvaluescurrnew = lit_alloccur
        return             = lit_return2.
  ELSE.
    lva_objek2 = lva_objek.

    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      EXPORTING
        objecttable        = lva_objecttable
        classnum           = lva_classnum
        classtype          = lva_classtype
        objectkey          = lva_objek2
      TABLES
        allocvaluesnumnew  = lit_allocnum
        allocvaluescharnew = lit_allocvalues
        allocvaluescurrnew = lit_alloccur
        return             = lit_return2.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  READ TABLE git_safra_lote ASSIGNING FIELD-SYMBOL(<fs_safra_lote>) WITH KEY matnr = p_matnr
                                                                                     charg = p_charg.
  CHECK sy-subrc EQ 0.

  CLEAR: lva_safra.
  PERFORM f_get_safra_lote USING p_matnr p_charg CHANGING lva_safra.

  <fs_safra_lote>-atwrt = lva_safra.

ENDFORM.

FORM f_get_outros_registros .
  SELECT *
    FROM zmmt0025 INTO TABLE git_zmmt0025.
ENDFORM.

FORM f_get_safra_lote USING p_matnr
                            p_charg
                   CHANGING c_safra.


  DATA: lit_matnr  TYPE TABLE OF zmme_cl,
        lit_return TYPE TABLE OF zmme_cl.

  CLEAR: c_safra.

  CHECK p_matnr  IS NOT INITIAL AND
        p_charg  IS NOT INITIAL.

  CLEAR: lit_matnr[], lit_return[].

  READ TABLE git_zmmt0025 INTO DATA(lwa_zmmt0025) WITH KEY atnam = 'SAFRA'.
  CHECK sy-subrc EQ 0 AND lwa_zmmt0025-atinn IS NOT INITIAL.

  APPEND VALUE #( matnr = p_matnr
                  charg = p_charg
                  atinn = lwa_zmmt0025-atinn ) TO lit_matnr.

  CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
    TABLES
      t_matnr  = lit_matnr
      t_return = lit_return
    EXCEPTIONS
      erro4    = 1.

  CHECK sy-subrc EQ 0.

  READ TABLE lit_return INTO DATA(lwa_safra_material) INDEX 1.
  CHECK sy-subrc EQ 0 AND lwa_safra_material-atwrt IS NOT INITIAL.

  c_safra = lwa_safra_material-atwrt.

ENDFORM.

FORM f_atualiza_registro_bloco USING p_zppt0002 TYPE zppt0002.

  DATA: lwa_zppt0040 TYPE zppt0040.

*------------------------------------------------------------------------------------------*
* Atualiza Bloco Origem
*------------------------------------------------------------------------------------------*

  CHECK p_zppt0002-id_sessao IS INITIAL AND p_zppt0002-lgort IS NOT INITIAL.

  CLEAR: lwa_zppt0040.
  SELECT SINGLE *
    FROM zppt0040 INTO lwa_zppt0040
   WHERE werks  = p_zppt0002-werks
     AND lgort  = p_zppt0002-lgort
     AND safra  = p_zppt0002-cd_safra.

  IF sy-subrc NE 0.
    lwa_zppt0040-werks             = p_zppt0002-werks.
    lwa_zppt0040-lgort             = p_zppt0002-lgort.
    lwa_zppt0040-safra             = p_zppt0002-cd_safra.
  ENDIF.

  lwa_zppt0040-qtd_fardinhos     = p_zppt0002-qtd_fardinhos_bloco.
  lwa_zppt0040-capacidade_bloco  = p_zppt0002-qtd_bloco.
  lwa_zppt0040-tipo_fardo        = p_zppt0002-tipo_fardo.
  lwa_zppt0040-cd_classificacao  = p_zppt0002-cd_classificacao.
  lwa_zppt0040-peso_liquido      = p_zppt0002-peso_liq_atual_bloco.
  lwa_zppt0040-dt_registro       = sy-datum.
  lwa_zppt0040-hr_registro       = sy-uzeit.

  MODIFY zppt0040 FROM lwa_zppt0040.

*------------------------------------------------------------------------------------------*
* Atualiza Bloco Destino
*------------------------------------------------------------------------------------------*

  CHECK p_zppt0002-bloco_destino IS NOT INITIAL.

  CLEAR: lwa_zppt0040.
  SELECT SINGLE *
    FROM zppt0040 INTO lwa_zppt0040
   WHERE werks  = p_zppt0002-werks
     AND lgort  = p_zppt0002-bloco_destino
     AND safra  = p_zppt0002-cd_safra.

  IF sy-subrc NE 0.
    lwa_zppt0040-werks           = p_zppt0002-werks.
    lwa_zppt0040-lgort           = p_zppt0002-bloco_destino.
    lwa_zppt0040-safra           = p_zppt0002-cd_safra.
  ENDIF.

  lwa_zppt0040-qtd_fardinhos     = p_zppt0002-qtde_fardinhos_bloco_destino.
  lwa_zppt0040-capacidade_bloco  = p_zppt0002-capacidade_bloco_destino.
  lwa_zppt0040-tipo_fardo        = p_zppt0002-tipo_fardo_bloco_destino.
  lwa_zppt0040-cd_classificacao  = p_zppt0002-cd_classificacao_bloco_destino.
  lwa_zppt0040-peso_liquido      = p_zppt0002-peso_liq_atual_bloco_destino.
  lwa_zppt0040-dt_registro       = sy-datum.
  lwa_zppt0040-hr_registro       = sy-uzeit.

  MODIFY zppt0040 FROM lwa_zppt0040.

  COMMIT WORK.


ENDFORM.

FORM f_bloco_permite_class_material  TABLES t_return STRUCTURE bapiret2
                                      USING p_matnr
                                            p_werks
                                            p_lgort
                                            p_charg
                                   CHANGING c_error.

  CLEAR: c_error.

  "Checar se tem saldo no bloco destino com um material diferente da classificação do Trace
  SELECT SINGLE *
    FROM mchb INTO @DATA(lwa_mchb_check)
   WHERE matnr NE @p_matnr
     AND werks EQ @p_werks
     AND lgort EQ @p_lgort
     AND charg EQ @p_charg
     AND clabs GT 0.

  IF sy-subrc EQ 0.
    c_error = abap_true.

    SELECT SINGLE *
      FROM mara INTO @DATA(lwa_mara)
     WHERE matnr EQ @lwa_mchb_check-matnr.

    gva_msg_aux = |Bloco { p_lgort } contem saldo no material { lwa_mchb_check-matnr } do tipo: { lwa_mara-normt }! Operação não permitida|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.


  SELECT SINGLE *
    FROM zmmt0008 INTO @DATA(lwa_zmmt0008_check)
   WHERE werks EQ @p_werks
     AND lgort EQ @p_lgort
     AND safra EQ @p_charg(4)
     AND matnr NE @p_matnr.

  IF sy-subrc EQ 0.
    c_error = abap_true.

    SELECT SINGLE *
      FROM mara INTO @DATA(lwa_mara_zmmt0008)
     WHERE matnr EQ @lwa_zmmt0008_check-matnr.

    gva_msg_aux = |Bloco { p_lgort } ja teve saldo embarcado no material { lwa_zmmt0008_check-matnr } do tipo: { lwa_mara_zmmt0008-normt }! Operação não permitida|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.


ENDFORM.

FORM f_get_saldo_bloco TABLES t_return STRUCTURE bapiret2
                        USING p_werks
                              p_lgort
                              p_charg
                     CHANGING c_error
                              c_mchb TYPE mchb
                              c_mara TYPE mara.

  DATA: lit_mchb TYPE TABLE OF mchb.

  CLEAR: c_error, c_mchb, c_mara, lit_mchb[].

  SELECT *
    FROM mchb INTO TABLE lit_mchb
   WHERE werks = p_werks
     AND lgort = p_lgort
     AND charg = p_charg
     AND clabs > 0.

  IF lit_mchb[] IS INITIAL.
    c_error = abap_true.
    gva_msg_aux = |Bloco: { p_lgort } Centro: { p_werks } Lote: { p_charg } sem saldo para consumo!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  IF lines( lit_mchb[] ) > 1.
    c_error = abap_true.
    gva_msg_aux = |Bloco: { p_lgort } Centro: { p_werks } Lote: { p_charg } tem saldo em mais de um material!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  READ TABLE lit_mchb INTO c_mchb INDEX 1.
  IF sy-subrc NE 0 OR c_mchb-matnr IS INITIAL.
    c_error = abap_true.
    gva_msg_aux = |Bloco: { p_lgort } Centro: { p_werks } Lote: { p_charg } sem saldo para consumo!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM mara INTO c_mara
   WHERE matnr EQ c_mchb-matnr.

  IF sy-subrc NE 0.
    c_error = abap_true.
    gva_msg_aux = |Cadastro de material: { c_mchb-matnr } não encontrado!|.
    PERFORM f_add_mensagem_return TABLES t_return USING gva_msg_aux 'E'.
    RETURN.
  ENDIF.


ENDFORM.

FORM f_set_registro_for_reproc TABLES t_return STRUCTURE bapiret2
                                USING p_zppt0002 TYPE zppt0002
                                      p_motivo   TYPE zppt0002-motivo_reprocessamento.

  CHECK p_zppt0002 IS NOT INITIAL.

  SELECT SINGLE *
    FROM zppt0002 INTO @DATA(lwa_zppt0002)
   WHERE acharg      = @p_zppt0002-acharg
     AND werks       = @p_zppt0002-werks
     AND id_sessao   = @p_zppt0002-id_sessao
     AND lgort       = @p_zppt0002-lgort
     AND cd_safra    = @p_zppt0002-cd_safra.

  READ TABLE t_return INTO DATA(lwa_return) WITH KEY type = 'E'.

  CHECK lwa_zppt0002-motivo_reprocessamento NE p_motivo or
        lwa_zppt0002-cd_mensagem            NE lwa_return-message.

  UPDATE zppt0002
     SET motivo_reprocessamento = p_motivo
         cd_mensagem            = lwa_return-message
   WHERE acharg      = p_zppt0002-acharg
     AND werks       = p_zppt0002-werks
     AND id_sessao   = p_zppt0002-id_sessao
     AND lgort       = p_zppt0002-lgort
     AND cd_safra    = p_zppt0002-cd_safra.

  p_zppt0002-cd_mensagem = lwa_return-message.

  COMMIT WORK.

ENDFORM.

FORM f_email_alerta_erro_estorno  USING p_zppt0002 TYPE zppt0002.

  DATA: lva_texto_corpo TYPE string,
        lv_title_email  TYPE string.

  lv_title_email = |SAP { sy-sysid } - ERRO ESTORNO CLASSIFICAÇÃO BENEFICIAMENTO - TRACE COTTON|.
  lva_texto_corpo = |Houve um erro no estorno de uma classificação: Filial: { p_zppt0002-werks } Bloco: { p_zppt0002-lgort } Classificação: { p_zppt0002-cd_classificacao } Versão: { p_zppt0002-verid } |.
  lva_texto_corpo = |{ lva_texto_corpo } - Erro: { p_zppt0002-cd_mensagem } |.
  lva_texto_corpo = |{ lva_texto_corpo } - Protocolo: { p_zppt0002-id_referencia } |.

  zcl_trace_cotton_utils=>disparar_email_alerta(
    i_zppt0002          = p_zppt0002
    i_titulo_email      = lv_title_email
    i_texto_corpo_email = lva_texto_corpo
  ).

ENDFORM.
