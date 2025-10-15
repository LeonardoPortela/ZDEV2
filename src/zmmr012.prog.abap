*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZMMR011                                                 *
* Descrição  : Rotina de Transferência de Saldo de Estoque de A Fixar  *
*              para Fixo                                               *
* Módulo     : MM                               Transação: NA          *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Marcus Scauri Santos                   Data: 30/07/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*
REPORT  zmmr012.

CONSTANTS cc_prefix_header_et    TYPE c LENGTH 50 VALUE 'CHV_ROM-'.

*----------------------------------------------------------------------*
* Tabelas globais
*----------------------------------------------------------------------*
DATA: t_zmmt0006           TYPE TABLE OF zmmt0006,
      git_mara             TYPE TABLE OF mara, "Projeto Restruturação Algodao - WPP
      git_matkl_alg_caroco TYPE TABLE OF tvarvc, "Projeto Restruturação Algodao - WPP
      t_return             TYPE TABLE OF bapiret2,
      t_log                TYPE TABLE OF zfie_ret_document,
      t_status             TYPE zde_btcstatus_t,
      t_goodsmvt_item      TYPE TABLE OF bapi2017_gm_item_create.

*----------------------------------------------------------------------*
* Estruturas globais
*----------------------------------------------------------------------*
DATA: wa_zmmt0006        TYPE zmmt0006,
      wa_zmmt0006_err    TYPE zmmt0006_err,
      wa_return          TYPE bapiret2,
      wa_log             TYPE zfie_ret_document,
      wa_goodsmvt_header TYPE bapi2017_gm_head_01,
      wa_goodsmvt_item   TYPE bapi2017_gm_item_create,
      wa_code            TYPE bapi2017_gm_code.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: c_n       TYPE c VALUE 'N',
           c_x       TYPE c VALUE 'X',
           c_s       TYPE c VALUE 'S',
           c_e       TYPE c VALUE 'E',
           c_05(2)   TYPE c VALUE '05',
           c_06(2)   TYPE c VALUE '06',
           c_mm(2)   TYPE c VALUE 'MM',
           c_899(3)  TYPE c VALUE '899',
           c_mb1c(4) TYPE c VALUE 'MB1C',
           c_mb11(4) TYPE c VALUE 'MB11'.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: v_message TYPE symsgv.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM: zf_seleciona_dados,
           zf_executa_bapi,
           zf_envia_msg_legado.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona dados para o batch input
*----------------------------------------------------------------------*
FORM zf_seleciona_dados.

  REFRESH t_zmmt0006.

  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd  = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.

  SELECT *
    FROM zmmt0006
    INTO TABLE t_zmmt0006
   WHERE tcode         = c_mb11
     AND rg_atualizado = c_n.

  "Projeto Restruturação Algodao - WPP
  LOOP AT t_zmmt0006 ASSIGNING FIELD-SYMBOL(<fs_zmmt0006>).

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = <fs_zmmt0006>-matnr
      IMPORTING
        output = <fs_zmmt0006>-matnr.

  ENDLOOP.

  IF t_zmmt0006[] IS NOT INITIAL.
    SELECT matnr matkl
      FROM mara INTO CORRESPONDING FIELDS OF TABLE git_mara
      FOR ALL ENTRIES IN t_zmmt0006
     WHERE matnr = t_zmmt0006-matnr.
  ENDIF.

  SELECT *
    FROM tvarvc INTO TABLE git_matkl_alg_caroco
   WHERE name = 'MAGGI_GR_ALGODAO_CAROCO'.
  "Projeto Restruturação Algodao - WPP



ENDFORM.                    " ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_EXECUTA_BAPI
*&---------------------------------------------------------------------*
*       Executa a BAPI para a transação MB1C
*----------------------------------------------------------------------*
FORM zf_executa_bapi .

  DATA: wa_zmmt0006_docs   TYPE zmmt0006_docs.
  DATA: lwa_zsdt0001       TYPE zsdt0001.
  DATA: lva_error          TYPE c,
        lva_algodao_caroco TYPE c.

  DATA: vl_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc,
        vl_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year.

  LOOP AT t_zmmt0006 INTO wa_zmmt0006.

    CLEAR: t_return[], lva_error, lva_algodao_caroco, lwa_zsdt0001.

    wa_zmmt0006-rg_atualizado = c_s.
    MODIFY zmmt0006 FROM wa_zmmt0006.
    COMMIT WORK.

    PERFORM f_inicia_transferencia USING wa_zmmt0006 CHANGING lva_error
                                                              lva_algodao_caroco
                                                              lwa_zsdt0001.
    CHECK lva_error = abap_false.

*Atribui valores aos parametros a serem usados na BAPI
    wa_code-gm_code               = c_06.

    wa_goodsmvt_header-pstng_date = wa_zmmt0006-budat.
    wa_goodsmvt_header-doc_date	  = wa_zmmt0006-bldat.
    wa_goodsmvt_header-header_txt = wa_zmmt0006-ch_referencia.
    wa_goodsmvt_header-ref_doc_no = wa_zmmt0006-doc_referencia.

    wa_goodsmvt_item-material     = wa_zmmt0006-matnr.
    wa_goodsmvt_item-move_plant   = wa_zmmt0006-werks_d.
    wa_goodsmvt_item-plant        = wa_zmmt0006-werks.
    wa_goodsmvt_item-move_stloc   = wa_zmmt0006-lgort.
    wa_goodsmvt_item-stge_loc     = wa_zmmt0006-lgort_o.

    IF wa_goodsmvt_item-stge_loc  IS INITIAL .
      wa_goodsmvt_item-stge_loc = wa_zmmt0006-lgort.
    ENDIF.

    wa_goodsmvt_item-batch        = wa_zmmt0006-batch.
    wa_goodsmvt_item-move_batch   = wa_zmmt0006-batch_d.

    IF lva_algodao_caroco = abap_true.
      wa_goodsmvt_item-move_batch = lwa_zsdt0001-nr_safra && '_' && lwa_zsdt0001-branch.
      wa_zmmt0006-batch_d = wa_goodsmvt_item-move_batch.
      wa_goodsmvt_header-header_txt = cc_prefix_header_et && lwa_zsdt0001-ch_referencia(11).
      wa_goodsmvt_header-ref_doc_no = lwa_zsdt0001-nr_romaneio.
    ENDIF.

    IF wa_goodsmvt_item-move_batch IS INITIAL.
      wa_goodsmvt_item-move_batch   = wa_goodsmvt_item-batch.
    ENDIF.

    wa_goodsmvt_item-move_type    = wa_zmmt0006-bwart.
    wa_goodsmvt_item-entry_qnt   	= wa_zmmt0006-erfmg.
    wa_goodsmvt_item-item_text    = wa_zmmt0006-sgtxt.
    wa_goodsmvt_item-amount_lc 	  = wa_zmmt0006-exbwr.
    wa_goodsmvt_item-tax_code     = wa_zmmt0006-tax_code.
    APPEND wa_goodsmvt_item TO t_goodsmvt_item.

    CLEAR: vl_mat_doc, vl_matdocumentyear.

*Exeuta a BAPI
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = wa_goodsmvt_header
        goodsmvt_code    = wa_code
      IMPORTING
        materialdocument = vl_mat_doc
        matdocumentyear  = vl_matdocumentyear
      TABLES
        goodsmvt_item    = t_goodsmvt_item
        return           = t_return.

*Verifica se a BAPI foi executada sem erros
    READ TABLE t_return INTO wa_return WITH KEY type = c_e.
    IF sy-subrc NE 0.
      READ TABLE t_return INTO wa_return WITH KEY type = c_s.

      IF ( vl_mat_doc IS NOT INITIAL ).

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = c_x.

        UPDATE  zmmt0006 SET batch_d       = wa_zmmt0006-batch_d
                             doc_material  = vl_mat_doc
                       WHERE ch_referencia = wa_zmmt0006-ch_referencia.
        COMMIT WORK.

*Cria mensagem de retorno em caso de sucesso
        CLEAR wa_return.

*Mensagem: O documento <nr_doc> para o exercício <ano>
*          foi criado com sucesso.
        CONCATENATE TEXT-001 vl_mat_doc
                    TEXT-002 vl_matdocumentyear
                    TEXT-003
                    INTO v_message
                    SEPARATED BY space.

        wa_return-type       = c_s.
        wa_return-id         = c_mm.
        wa_return-number     = c_899.
        wa_return-message_v1 = vl_mat_doc.
        wa_return-message    = v_message.

        APPEND wa_return TO t_return.

        "Registra Vinculo
        CLEAR wa_zmmt0006_docs.
        wa_zmmt0006_docs-ch_referencia = wa_zmmt0006-ch_referencia.
        wa_zmmt0006_docs-bukrs         = wa_zmmt0006-bukrs.
        wa_zmmt0006_docs-mat_doc       = vl_mat_doc.
        wa_zmmt0006_docs-doc_year      = vl_matdocumentyear.
        MODIFY zmmt0006_docs FROM wa_zmmt0006_docs.
        COMMIT WORK.

      ELSE.
        PERFORM zf_gera_log_erro USING wa_zmmt0006-ch_referencia
                                       wa_zmmt0006-id_interface.
      ENDIF.
    ELSE.
      PERFORM zf_gera_log_erro USING wa_zmmt0006-ch_referencia
                                     wa_zmmt0006-id_interface.
    ENDIF.

*Rotina para tratar mensagens
    PERFORM zf_trata_mensagens USING wa_zmmt0006-ch_referencia
                                     wa_zmmt0006-id_interface.

    CLEAR:   wa_zmmt0006, wa_goodsmvt_item, wa_goodsmvt_item.
    REFRESH: t_return, t_goodsmvt_item.

  ENDLOOP.

ENDFORM.                    " ZF_EXECUTA_BAPI

*-#125503 = 26.10.2023 - JT - inicio
FORM zf_gera_log_erro USING p_ch_referencia
                            p_id_interface.

  DATA: nr_item  LIKE zmmt0006_err-nr_item.

  DELETE FROM zmmt0006_err WHERE ch_referencia  = p_ch_referencia.

  SORT t_return BY type .

  CLEAR: nr_item.

  LOOP AT t_return INTO wa_return WHERE type = c_e.
    CLEAR wa_zmmt0006_err.

    ADD 1 TO nr_item.
    wa_zmmt0006_err-ch_referencia  = p_ch_referencia.
    wa_zmmt0006_err-nr_item        = nr_item.
    wa_zmmt0006_err-interface      = p_id_interface.
    wa_zmmt0006_err-dt_atualizacao = sy-datum.
    wa_zmmt0006_err-hr_atualizacao = sy-uzeit.
    wa_zmmt0006_err-type           = wa_return-type.
    wa_zmmt0006_err-id             = wa_return-id   .
    wa_zmmt0006_err-num            = wa_return-number.
    wa_zmmt0006_err-message        = wa_return-message.
    wa_zmmt0006_err-message_v1     = wa_return-message_v1.
    wa_zmmt0006_err-message_v2     = wa_return-message_v2.
    wa_zmmt0006_err-message_v3     = wa_return-message_v3.
    wa_zmmt0006_err-message_v4     = wa_return-message_v4.

    MODIFY zmmt0006_err         FROM wa_zmmt0006_err.
  ENDLOOP.

  " FREE t_return.

ENDFORM.
*-#125503 = 26.10.2023 - JT - fim

*&---------------------------------------------------------------------*
*&      Form  ZF_TRATA_MENSAGENS
*&---------------------------------------------------------------------*
*       Trata mensagem de erro
*----------------------------------------------------------------------*
FORM zf_trata_mensagens USING p_ch_referencia TYPE zch_refe
                              p_id_interface  TYPE any.

  IF NOT t_return[] IS INITIAL.
    CLEAR wa_return.

    LOOP AT t_return INTO wa_return.

*Preenche campos para enviar msg
      wa_log-obj_key        = p_ch_referencia.
      wa_log-interface      = p_id_interface.
      wa_log-dt_atualizacao = sy-datum.
      wa_log-hr_atualizacao = sy-uzeit.
      wa_log-type           = wa_return-type.
      wa_log-id             = wa_return-id.
      wa_log-num            = wa_return-number.
      wa_log-message        = wa_return-message.
      wa_log-message_v1     = wa_return-message_v1.
      wa_log-message_v2     = wa_return-message_v2.
      wa_log-message_v3     = wa_return-message_v3.
      wa_log-message_v4     = wa_return-message_v4.
      wa_log-info_adicional_1  = 'ZMMR012'.

      APPEND wa_log TO t_log.
      CLEAR wa_log.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " ZF_TRATA_MENSAGENS
*&---------------------------------------------------------------------*
*&      Form  ZF_ENVIA_MSG_LEGADO
*&---------------------------------------------------------------------*
*       Envia msg para legado
*----------------------------------------------------------------------*
FORM zf_envia_msg_legado .

  CHECK t_log[] IS NOT INITIAL.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        outreturn = t_log.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        outreturn = t_log.
  ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
  COMMIT WORK.

ENDFORM.                    " ZF_ENVIA_MSG_LEGADO

FORM f_inicia_transferencia  USING p_zmmt0006 TYPE zmmt0006
                          CHANGING c_error
                                   c_algodao_caroco
                                   c_zsdt0001 TYPE zsdt0001.

  DATA: wa_zmmt0006_docs TYPE zmmt0006_docs.

  DATA: lit_mkpf_exists TYPE TABLE OF mkpf.

  CLEAR: c_error, c_algodao_caroco, c_zsdt0001.

  "Verifica Duplicidade
  CLEAR wa_zmmt0006_docs.

  SELECT SINGLE *
    FROM zmmt0006_docs
    INTO wa_zmmt0006_docs
   WHERE ch_referencia = p_zmmt0006-ch_referencia.

  IF sy-subrc IS INITIAL.

    c_error = abap_true.

    CLEAR wa_log.
    wa_log-message         = |Chave de Referência: { p_zmmt0006-ch_referencia } gerado anteriormente!|.
    wa_log-obj_key        = p_zmmt0006-ch_referencia.
    wa_log-interface      = p_zmmt0006-id_interface.
    wa_log-dt_atualizacao = sy-datum.
    wa_log-hr_atualizacao = sy-uzeit.
    wa_log-type           = 'S'.
    wa_log-id             = 'MM'.
    wa_log-num            = '899'.
    wa_log-message_v1     = wa_zmmt0006_docs-mat_doc.
    wa_log-message_v2     = space.
    wa_log-message_v3     = space.
    wa_log-message_v4     = space.

    APPEND wa_log TO t_log.

    RETURN.
  ENDIF.

  READ TABLE git_mara INTO DATA(lwa_mara) WITH KEY matnr = p_zmmt0006-matnr.
  IF sy-subrc NE 0.
    c_error = abap_true.

    CLEAR wa_log.
    wa_log-message        = |Cadastro Material não encontrado para a Chave de Referência: { p_zmmt0006-ch_referencia }|.
    wa_log-obj_key        = p_zmmt0006-ch_referencia.
    wa_log-interface      = p_zmmt0006-id_interface.
    wa_log-dt_atualizacao = sy-datum.
    wa_log-hr_atualizacao = sy-uzeit.
    wa_log-type           = 'E'.
    wa_log-id             = 'MM'.
    wa_log-num            = '899'.

    APPEND wa_log TO t_log.

    RETURN.
  ENDIF.

  READ TABLE git_matkl_alg_caroco WITH KEY low = lwa_mara-matkl TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    c_algodao_caroco = abap_true.

    PERFORM f_valida_transf_alg_caroco  USING p_zmmt0006
                                     CHANGING c_error
                                              c_zsdt0001.
    CHECK c_error IS INITIAL.

    UPDATE  zmmt0006 SET ch_referencia_rom = c_zsdt0001-ch_referencia
                         algodao_caroco    = abap_true
     WHERE ch_referencia = p_zmmt0006-ch_referencia.

    COMMIT WORK.
  ENDIF.


ENDFORM.

FORM f_valida_transf_alg_caroco  USING    p_zmmt0006 TYPE zmmt0006
                                 CHANGING c_error
                                          c_zsdt0001 TYPE zsdt0001.

  DATA: lit_mkpf_exists TYPE TABLE OF mkpf,
        lva_bktxt       TYPE mkpf-bktxt.

  CLEAR: c_error, c_zsdt0001.

  SELECT SINGLE *
    FROM zsdt0001 INTO c_zsdt0001
   WHERE ch_referencia = p_zmmt0006-ch_referencia(11).

  IF sy-subrc NE 0 OR c_zsdt0001-tp_movimento NE 'E'.

    c_error = abap_true.

    CLEAR wa_log.

    wa_log-message        = |Romaneio de entrada não encontrado para a Chave de Referência: { p_zmmt0006-ch_referencia } |.
    wa_log-obj_key        = p_zmmt0006-ch_referencia.
    wa_log-interface      = p_zmmt0006-id_interface.
    wa_log-dt_atualizacao = sy-datum.
    wa_log-hr_atualizacao = sy-uzeit.
    wa_log-type           = 'E'.
    wa_log-id             = 'MM'.
    wa_log-num            = '899'.

    APPEND wa_log TO t_log.

    RETURN.
  ENDIF.

  lva_bktxt = cc_prefix_header_et && c_zsdt0001-ch_referencia.

  SELECT a~mblnr a~mjahr
    FROM mkpf AS a INTO CORRESPONDING FIELDS OF TABLE lit_mkpf_exists
   WHERE bktxt = lva_bktxt
     AND NOT EXISTS (  SELECT mblnr
                         FROM mseg AS b
                        WHERE b~smbln = a~mblnr
                          AND b~sjahr = a~mjahr )
     AND NOT EXISTS ( SELECT mblnr
                        FROM mseg AS b
                       WHERE b~mblnr = a~mblnr
                         AND b~smbln NE space ).

  LOOP AT lit_mkpf_exists INTO DATA(lwa_mkpf_exists).

    c_error = abap_true.

    CLEAR wa_log.

    wa_log-message        = |Documento Transferencia: { lwa_mkpf_exists-mblnr } / { lwa_mkpf_exists-mjahr } já gerado para o romaneio entrada Chv.Referencia: { c_zsdt0001-ch_referencia } |.
    wa_log-obj_key        = p_zmmt0006-ch_referencia.
    wa_log-interface      = p_zmmt0006-id_interface.
    wa_log-dt_atualizacao = sy-datum.
    wa_log-hr_atualizacao = sy-uzeit.
    wa_log-type           = 'E'.
    wa_log-id             = 'MM'.
    wa_log-num            = '899'.

    APPEND wa_log TO t_log.

    RETURN.
  ENDLOOP.


ENDFORM.
