*&---------------------------------------------------------------------*
*& Report  ZMMYR0001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmmyr0001.
*----------------------------------------------------------------------*
* Tabelas globais
*----------------------------------------------------------------------*
DATA: t_zmmt0006      TYPE TABLE OF zmmt0006,
      t_return        TYPE TABLE OF bapiret2,
      t_msg           TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      t_bdcdata       TYPE TABLE OF bdcdata,
      t_log           TYPE TABLE OF zfie_ret_document,
      t_goodsmvt_item TYPE TABLE OF bapi2017_gm_item_create.

*----------------------------------------------------------------------*
* Estruturas globais
*----------------------------------------------------------------------*
DATA: wa_zmmt0006        TYPE zmmt0006,
      wa_zmmt0006_docs   TYPE zmmt0006_docs,
      wa_zmmt0006_err    TYPE zmmt0006_err,
      wa_return          TYPE bapiret2,
      wa_log             TYPE zfie_ret_document,
      wa_goodsmvt_header TYPE bapi2017_gm_head_01,
      wa_goodsmvt_item   TYPE bapi2017_gm_item_create,
      wa_code            TYPE bapi2017_gm_code,
      wa_goodsmvt_ret    TYPE bapi2017_gm_head_ret.


*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: c_n       TYPE c VALUE 'N',
           c_x       TYPE c VALUE 'X',
           c_s       TYPE c VALUE 'S',
           c_e       TYPE c VALUE 'E',
           c_04(2)   TYPE c VALUE '04',
           c_05(2)   TYPE c VALUE '05',
           c_mm(2)   TYPE c VALUE 'MM',
           c_899(3)  TYPE c VALUE '899',
           c_mb1b(4) TYPE c VALUE 'MB1B',
           c_mbst(4) TYPE c VALUE 'MBST'.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: v_message          TYPE c LENGTH 220, " SYMSGV,
      vg_mode            TYPE c,
      vg_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc,
      vg_pstng_date      TYPE bapi2017_gm_head_02-pstng_date,
      vg_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM zf_seleciona_dados.
  IF t_zmmt0006[] IS NOT INITIAL.
    PERFORM zf_executa_bapi.
    IF t_log[] IS NOT INITIAL.
      PERFORM zf_envia_msg_legado.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona dados para o batch input
*----------------------------------------------------------------------*
FORM zf_seleciona_dados.
  REFRESH t_zmmt0006.

  DATA: vg_job TYPE i.

  SELECT SINGLE COUNT(*) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'PRESTAMO_DEVOLUCAO_MB1B'
     AND status EQ 'R'.

  IF ( vg_job EQ 1 ).

    SELECT *
      FROM zmmt0006
      INTO TABLE t_zmmt0006
      WHERE ( tcode = c_mb1b OR tcode =  c_mbst )
        AND rg_atualizado = c_n.
  ENDIF.

ENDFORM.                    " ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_EXECUTA_BAPI
*&---------------------------------------------------------------------*
*       Executa a BAPI para a transação MB1C
*----------------------------------------------------------------------*
FORM zf_executa_bapi .

  vg_mode = c_n.
*Percorre a tabela de ajuste de grãos
  LOOP AT t_zmmt0006 INTO wa_zmmt0006.

    CLEAR wa_zmmt0006_docs.

    SELECT SINGLE *
      FROM zmmt0006_docs
      INTO wa_zmmt0006_docs
     WHERE ch_referencia = wa_zmmt0006-ch_referencia.

    IF sy-subrc IS INITIAL.
      CLEAR wa_log.

      CONCATENATE 'Chave de Referência:' wa_zmmt0006-ch_referencia 'gerado anteriormente!' INTO wa_log-message SEPARATED BY space.

      wa_log-obj_key        = wa_zmmt0006-ch_referencia.
      wa_log-interface      = wa_zmmt0006-id_interface.
      wa_log-dt_atualizacao = sy-datum.
      wa_log-hr_atualizacao = sy-uzeit.
      wa_log-type           = 'S'.
      wa_log-id             = 'MM'.
      wa_log-num            = '899'.
      wa_log-message_v1     = wa_zmmt0006_docs-mat_doc.
      wa_log-message_v2     = wa_zmmt0006_docs-stprs.
      wa_log-message_v3     = space.
      wa_log-message_v4     = space.

      APPEND wa_log TO t_log.

      wa_zmmt0006-rg_atualizado = c_s.
      MODIFY zmmt0006 FROM wa_zmmt0006.
      COMMIT WORK.

      CONTINUE.

    ELSE.

      IF ( wa_zmmt0006-id_interface = '36' ) OR ( wa_zmmt0006-id_interface = '62' ).
        PERFORM gera_movimento.
      ELSE.
        PERFORM estorna_movimento.
      ENDIF.
    ENDIF.

    wa_zmmt0006-rg_atualizado = c_s.
    MODIFY zmmt0006 FROM wa_zmmt0006.
    COMMIT WORK.

*Rotina para tratar mensagens

    CLEAR:   wa_zmmt0006, wa_goodsmvt_item, wa_goodsmvt_item.
    REFRESH: t_return, t_goodsmvt_item.

  ENDLOOP.

ENDFORM.                    " ZF_EXECUTA_BAPI
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

      APPEND wa_log TO t_log.
      CLEAR wa_log.
    ENDLOOP.
  ENDIF.
  REFRESH t_return.
ENDFORM.                    " ZF_TRATA_MENSAGENS
*&---------------------------------------------------------------------*
*&      Form  ZF_ENVIA_MSG_LEGADO
*&---------------------------------------------------------------------*
*       Envia msg para legado
*----------------------------------------------------------------------*
FORM zf_envia_msg_legado .

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*  CALL FUNCTION 'Z_FI_OUTBOUND_RETURN'
*    IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_RETURN'
*    TABLES
*      outreturn = t_log.

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
*&---------------------------------------------------------------------*
*&      Form  GERA_MOVIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_movimento .
  DATA: vl_kalnr TYPE ckmlhd-kalnr,
        vl_stprs TYPE ckmlcr-stprs,
        nr_item  LIKE zmmt0006_err-nr_item.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = wa_zmmt0006-matnr
    IMPORTING
      output       = wa_zmmt0006-matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*Atribui valores aos parametros a serem usados na BAPI
  wa_goodsmvt_header-pstng_date = wa_zmmt0006-budat.
  wa_goodsmvt_header-doc_date	  = wa_zmmt0006-bldat.
  wa_goodsmvt_header-header_txt = wa_zmmt0006-sgtxt."WA_ZMMT0006-CH_REFERENCIA.
  wa_goodsmvt_header-ref_doc_no = wa_zmmt0006-sgtxt.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
  "  WA_GOODSMVT_ITEM-MATERIAL     = WA_ZMMT0006-MATNR.
  DATA(v_len1) = strlen( wa_zmmt0006-matnr ).
  IF v_len1 > 18.
    wa_goodsmvt_item-material_long = wa_zmmt0006-matnr .
  ELSE.
    wa_goodsmvt_item-material = wa_zmmt0006-matnr .
  ENDIF.
*<-- 16.06.2023 - Migration S4 – MIGNOW – End
  wa_goodsmvt_item-move_plant   = wa_zmmt0006-werks_d.
  wa_goodsmvt_item-plant        = wa_zmmt0006-werks.
  wa_goodsmvt_item-move_stloc   = wa_zmmt0006-lgort.
  wa_goodsmvt_item-stge_loc     = wa_zmmt0006-lgort_o.

  IF wa_goodsmvt_item-stge_loc IS INITIAL .
    wa_goodsmvt_item-stge_loc = wa_zmmt0006-lgort.
  ENDIF.

  wa_goodsmvt_item-batch        = wa_zmmt0006-batch.
  wa_goodsmvt_item-move_batch   = wa_zmmt0006-batch.
  wa_goodsmvt_item-move_type    = wa_zmmt0006-bwart.
  wa_goodsmvt_item-entry_qnt   	= wa_zmmt0006-erfmg.
  wa_goodsmvt_item-item_text    = wa_zmmt0006-sgtxt.
  IF wa_zmmt0006-exbwr <> '0.01' .
    wa_goodsmvt_item-amount_lc 	  = wa_zmmt0006-exbwr.
  ELSE.
    CLEAR wa_goodsmvt_item-amount_lc.
  ENDIF.

  " Para contrato Recibido = ID_FORNECEDOR_SAP
  "  Para contrato Otorgado = ID_CLIENTE_SAP
  IF wa_zmmt0006-sgtxt(3) = 'REC'.
    wa_goodsmvt_item-vendor     = wa_zmmt0006-id_parceiro.
  ELSE.
    wa_goodsmvt_item-customer   = wa_zmmt0006-id_parceiro.
  ENDIF.
*GM_CODE - TCODE - Description
*01 - MB01 - Goods receipt for purchase order
*02 - MB31 - Goods receipt for production order
*03 - MB1A - Goods issue
*04 - MB1B - Transfer posting
*05 - MB1C - Other goods receipt
*06 - MB11 - Reversal of goods movements
*07 - MB04 - Subsequent adjustment with regard to a subcontract order
*http://wiki.scn.sap.com/wiki/display/ERPSCM/How+To+Goods+Movements+with+BAPI
  wa_code-gm_code               = c_04.

  APPEND wa_goodsmvt_item TO t_goodsmvt_item.

*Exeuta a BAPI
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      goodsmvt_header  = wa_goodsmvt_header
      goodsmvt_code    = wa_code
    IMPORTING
      materialdocument = vg_mat_doc
      matdocumentyear  = vg_matdocumentyear
    TABLES
      goodsmvt_item    = t_goodsmvt_item
      return           = t_return.

*Verifica se a BAPI foi executada sem erros
  READ TABLE t_return INTO wa_return WITH KEY type = c_e.
  IF sy-subrc NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.


    UPDATE mseg SET xblnr_mkpf = wa_zmmt0006-sgtxt WHERE mblnr = vg_mat_doc.

*Cria mensagem de retorno em caso de sucesso
    CLEAR wa_return.

*Mensagem: O documento <nr_doc> para o exercício <ano>
*        foi criado com sucesso.
    IF wa_zmmt0006-exbwr = '0.01' .
      "Recupera o valor standard para que os proximos lançãmentos tenham o mesmo valor
      SELECT SINGLE kalnr
        INTO vl_kalnr
        FROM ckmlhd
       WHERE matnr  = wa_zmmt0006-matnr
         AND bwkey  = wa_zmmt0006-werks .

* ---> S4 Migration - 09/07/2023 - MA
*      SELECT SINGLE STPRS
*        INTO VL_STPRS
*        FROM CKMLCR
*       WHERE KALNR = VL_KALNR
*         AND BDATJ = WA_ZMMT0006-BUDAT(4)
*         AND POPER = WA_ZMMT0006-BUDAT+4(2)
*         AND CURTP = '10'.

      DATA: wa_kalnr  TYPE ckmv0_matobj_str,
            lt_kalnr  TYPE ckmv0_matobj_tbl,
            lt_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE.

      DATA: lv_bdatj_1 TYPE  ckmlpp-bdatj,
            lv_poper_1 TYPE  ckmlpp-poper,
            lv_jahrper TYPE mldoc-jahrper.

      TYPES lr_waers_type TYPE RANGE OF waers.

      DATA : lr_waers TYPE lr_waers_type.

      lv_bdatj_1 = wa_zmmt0006-budat(4).
      lv_poper_1 = wa_zmmt0006-budat+4(2).

      wa_kalnr-kalnr = vl_kalnr.
      APPEND wa_kalnr TO lt_kalnr.

      CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
        EXPORTING
          i_bdatj_1               = lv_bdatj_1
          i_poper_1               = lv_poper_1
        TABLES
          t_kalnr                 = lt_kalnr
*         t_ckmlpp                = lt_ckmlpp
          t_ckmlcr                = lt_ckmlcr
        EXCEPTIONS
          no_data_found           = 1
          input_data_inconsistent = 2
          buffer_inconsistent     = 3
          OTHERS                  = 4.

      DELETE lt_ckmlcr WHERE curtp NE '10'.

      SORT lt_ckmlcr BY kalnr bdatj poper untper curtp.

      READ TABLE lt_ckmlcr INTO DATA(ls_ckmlcr) INDEX 1.

      IF sy-subrc = 0 .
        MOVE ls_ckmlcr-stprs TO vl_stprs.
      ENDIF.
* <--- S4 Migration - 09/07/2023 - MA

    ELSE.
      vl_stprs = '0.01'.
    ENDIF.

    CONCATENATE TEXT-001 vg_mat_doc
                TEXT-002 vg_matdocumentyear
                TEXT-003
                INTO v_message
                SEPARATED BY space.

    wa_return-type       = c_s.
    wa_return-id         = c_mm.
    wa_return-number     = c_899.
    wa_return-message_v1 = vg_mat_doc.
    wa_return-message_v2 = vl_stprs."Retornar o valor do unitário.
    wa_return-message    = v_message.

    APPEND wa_return TO t_return.

    CLEAR wa_zmmt0006_docs.

    wa_zmmt0006_docs-ch_referencia = wa_zmmt0006-ch_referencia.
    wa_zmmt0006_docs-bukrs         = wa_zmmt0006-bukrs.
    wa_zmmt0006_docs-mat_doc       = vg_mat_doc .
    wa_zmmt0006_docs-doc_year      = vg_matdocumentyear .
    wa_zmmt0006_docs-stprs         = vl_stprs.

    MODIFY zmmt0006_docs FROM wa_zmmt0006_docs.

    COMMIT WORK.

  ELSE.
    DELETE FROM zmmt0006_err WHERE ch_referencia  = wa_zmmt0006-ch_referencia.

    SORT t_return BY type .

    CLEAR: nr_item.

    LOOP AT t_return INTO wa_return WHERE type = c_e.

      CLEAR wa_zmmt0006_err.
      ADD 1 TO nr_item.
      wa_zmmt0006_err-ch_referencia  = wa_zmmt0006-ch_referencia.
      wa_zmmt0006_err-nr_item        = nr_item.
      wa_zmmt0006_err-interface      = wa_zmmt0006-id_interface.
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

      MODIFY zmmt0006_err FROM wa_zmmt0006_err.

    ENDLOOP.

  ENDIF.

  PERFORM zf_trata_mensagens USING wa_zmmt0006-ch_referencia
                                   wa_zmmt0006-id_interface.

ENDFORM.                    " GERA_MOVIMENTO

*&---------------------------------------------------------------------*
*&      Form  ESTORNA_MOVIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM estorna_movimento .
  DATA : vl_data TYPE c LENGTH 10,
         nr_item LIKE zmmt0006_err-nr_item.

  vg_mat_doc         = wa_zmmt0006-sgtxt(10).
  vg_matdocumentyear = wa_zmmt0006-bldat(4).  "ALRS 29.01.2024
  vg_pstng_date      = wa_zmmt0006-budat.


  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument    = vg_mat_doc
      matdocumentyear     = vg_matdocumentyear
      goodsmvt_pstng_date = vg_pstng_date
    IMPORTING
      goodsmvt_headret    = wa_goodsmvt_ret
    TABLES
      return              = t_return.


  READ TABLE t_return INTO wa_return WITH KEY type = c_e.
  IF sy-subrc NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    CLEAR wa_zmmt0006_docs.

    wa_zmmt0006_docs-ch_referencia = wa_zmmt0006-ch_referencia.
    wa_zmmt0006_docs-bukrs         = wa_zmmt0006-bukrs.
    wa_zmmt0006_docs-mat_doc       = wa_goodsmvt_ret .
    wa_zmmt0006_docs-doc_year      = vg_matdocumentyear.

    MODIFY zmmt0006_docs FROM wa_zmmt0006_docs.
    COMMIT WORK.

    CONCATENATE TEXT-001 vg_mat_doc
                TEXT-002 vg_matdocumentyear
                TEXT-005 wa_goodsmvt_ret-mat_doc
                INTO v_message
                SEPARATED BY space.

    wa_return-type       = c_s.
    wa_return-id         = c_mm.
    wa_return-number     = c_899.
    wa_return-message_v1 = wa_goodsmvt_ret-mat_doc.
    wa_return-message_v2 = vg_mat_doc.

    wa_return-message    = v_message.

    APPEND wa_return TO t_return.

  ELSE.

    DELETE FROM zmmt0006_err WHERE ch_referencia  = wa_zmmt0006-ch_referencia.

    SORT t_return BY type .

    CLEAR: nr_item.

    LOOP AT t_return INTO wa_return WHERE type = c_e.

      CLEAR wa_zmmt0006_err.
      ADD 1 TO nr_item.
      wa_zmmt0006_err-ch_referencia  = wa_zmmt0006-ch_referencia.
      wa_zmmt0006_err-nr_item        = nr_item.
      wa_zmmt0006_err-interface      = wa_zmmt0006-id_interface.
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

      MODIFY zmmt0006_err FROM wa_zmmt0006_err.

    ENDLOOP.

    COMMIT WORK.
  ENDIF.



  PERFORM zf_trata_mensagens USING wa_zmmt0006-ch_referencia
                                   wa_zmmt0006-id_interface.

ENDFORM.                    " ESTORNA_MOVIMENTO

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_DBC
*&---------------------------------------------------------------------*
*       Preenche tabela BDCData
*----------------------------------------------------------------------*
FORM z_preenche_dbc  TABLES it_bdcdata STRUCTURE bdcdata
                     USING  p_dynbegin TYPE any
                            p_name     TYPE any
                            p_value    TYPE any.

  DATA: wa_bdcdata TYPE bdcdata.

  IF p_dynbegin = 'X'.
    MOVE: p_name      TO wa_bdcdata-program,
          p_value     TO wa_bdcdata-dynpro,
          p_dynbegin  TO wa_bdcdata-dynbegin.
    APPEND wa_bdcdata TO t_bdcdata.
  ELSE.
    MOVE: p_name      TO wa_bdcdata-fnam,
          p_value     TO wa_bdcdata-fval.
    APPEND wa_bdcdata TO t_bdcdata.
  ENDIF.
  CLEAR: wa_bdcdata.
ENDFORM.                    " Z_PREENCHE_DBC
