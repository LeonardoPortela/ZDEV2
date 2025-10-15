FUNCTION z_xi_validation.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_GEO_RETURN_ERROR) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_GEO_RETURN_SUCESS) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_SIGAM_AP) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_SIGAM_AR) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_SIGAM_CUSTOMER) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_SIGAM_INDICE) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_SIGAM_RETURN) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_SIGAM_VENDOR) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_SIGAM_LOTE_COMPRA) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(P_XRT_REA) TYPE  CHAR1 OPTIONAL
*"----------------------------------------------------------------------


*XI_GEO_RETURN_ERROR---------------------------------------------------
  DATA z_geo_return_error LIKE zmme_return_error
                                  OCCURS 0 WITH HEADER LINE.

*--> 24.08.2023 17:27:29 - Migração S4 – ML - Início
  DATA: lv_rfc TYPE rfcdest,
        lv_fm  TYPE rs38l_fnam.
*<-- 24.08.2023 17:27:29 - Migração S4 – ML – Fim

  z_geo_return_error-idbol = 1000.
  z_geo_return_error-nrobol = 1000.
  z_geo_return_error-fgorigem = 'I'.
  z_geo_return_error-bwart = '201'.
  z_geo_return_error-erdat = '20070611'.
  z_geo_return_error-erzet = '123040'.
  z_geo_return_error-type = 'E'.
  z_geo_return_error-id = 'CLS'.
  z_geo_return_error-nromsg = 054.
  z_geo_return_error-message = 'teste message'.
  z_geo_return_error-message_v1 = 'param1'.
  z_geo_return_error-message_v2 = 'param2'.
  z_geo_return_error-message_v3 = 'param3'.
  z_geo_return_error-message_v4 = 'param4'.
  z_geo_return_error-dtproc  =  sy-datum.
  z_geo_return_error-hrproc  = '172338'.
  APPEND z_geo_return_error.

*XI_GEO_RETURN_SUCESS--------------------------------------------
  DATA z_geo_return_sucess LIKE zmme_return_sucess
                                    OCCURS 0 WITH HEADER LINE.

  z_geo_return_sucess-idbol = 1000.
  z_geo_return_sucess-nrobol = 1000.
  z_geo_return_sucess-fgorigem = 'I'.
  z_geo_return_sucess-bwart = '201'.
  z_geo_return_sucess-erdat = '20070611'.
  z_geo_return_sucess-erzet = '123040'.
  z_geo_return_sucess-id_matgeo = 23232.
  z_geo_return_sucess-matnr = 39022.
  z_geo_return_sucess-docmat = '32323'.
  z_geo_return_sucess-dmbtr = 20.
  z_geo_return_sucess-dmbe2 = 12.
  z_geo_return_sucess-dmbe3 = 4.
  z_geo_return_sucess-dtproc  =  sy-datum.
  z_geo_return_sucess-hrproc  = '172338'.
  APPEND z_geo_return_sucess.


*XI_SIGAM_AP--------------------------------------------
  DATA z_sigam_ap LIKE zfie_items_payment
                                    OCCURS 0 WITH HEADER LINE.
  z_sigam_ap-obj_key = '32231'.
  z_sigam_ap-belnr = '3223'.
  z_sigam_ap-augdt = sy-datum.
  z_sigam_ap-augbl = '432'.
  z_sigam_ap-nebtr = 23.
  z_sigam_ap-wrbtr = 43.
  z_sigam_ap-wskto = 32.
  z_sigam_ap-st_atualizacao = '2'.
  z_sigam_ap-dt_atualizacao = sy-datum.
  z_sigam_ap-hr_atualizacao = '172338'.
  z_sigam_ap-cd_transacao = 'TCODE'.
  APPEND z_sigam_ap.

*XI_SIGAM_AR
  DATA z_sigam_ar LIKE zfie_items_payment
         OCCURS 0 WITH HEADER LINE.

  z_sigam_ar-obj_key = '32231'.
  z_sigam_ar-belnr = '3223'.
  z_sigam_ar-augdt = sy-datum.
  z_sigam_ar-augbl = '432'.
  z_sigam_ar-nebtr = 23.
  z_sigam_ar-wrbtr = 43.
  z_sigam_ar-wskto = 32.
  z_sigam_ar-st_atualizacao = '2'.
  z_sigam_ar-dt_atualizacao = sy-datum.
  z_sigam_ar-hr_atualizacao = '172338'.
  z_sigam_ar-cd_transacao = 'TCODE'.
  APPEND z_sigam_ar.

*XI_SIGAM_CUSTOMER
  DATA z_sigam_customer LIKE zfie_customer
         OCCURS 0 WITH HEADER LINE.
  z_sigam_customer-id_cliente = '32'.
  z_sigam_customer-empresa = '001'.
  z_sigam_customer-gr_conta = '324'.
  z_sigam_customer-tx_tratamento = '4324'.
  z_sigam_customer-descricao = 'fd'.
  z_sigam_customer-st_atualizacao = '3'.
  z_sigam_customer-dt_atualizacao = sy-datum.
  z_sigam_customer-hr_atualizacao = '172338'.
  APPEND z_sigam_customer.

  DATA z_sigam_customer_bank LIKE zfie_bank
         OCCURS 0 WITH HEADER LINE.
  z_sigam_customer_bank-id_cliente = '32'.
  z_sigam_customer_bank-dt_atualizacao = sy-datum.
  z_sigam_customer_bank-hr_atualizacao = '172338'.
  z_sigam_customer_bank-cd_pais_banco = '32'.
  z_sigam_customer_bank-ch_banco = '4324'.
  z_sigam_customer_bank-nu_conta_bancaria = '3422'.
  z_sigam_customer_bank-st_atualizacao = '4'.
  APPEND z_sigam_customer_bank.

*XI_SIGAM_VENDOR
  DATA z_sigam_vendor LIKE zfie_vendor
           OCCURS 0 WITH HEADER LINE.
  z_sigam_vendor-id_fornecedor = '32'.
  z_sigam_vendor-empresa = '001'.
  z_sigam_vendor-gr_conta = '324'.
  z_sigam_vendor-tx_tratamento = '4324'.
  z_sigam_vendor-descricao = 'fd'.
  z_sigam_vendor-st_atualizacao = '3'.
  z_sigam_vendor-dt_atualizacao = sy-datum.
  z_sigam_vendor-hr_atualizacao = '172338'.
  APPEND z_sigam_vendor.

  DATA z_sigam_vendor_bank LIKE zfie_bank
         OCCURS 0 WITH HEADER LINE.
  z_sigam_vendor_bank-id_cliente = '32'.
  z_sigam_vendor_bank-dt_atualizacao = sy-datum.
  z_sigam_vendor_bank-hr_atualizacao = '172338'.
  z_sigam_vendor_bank-cd_pais_banco = '32'.
  z_sigam_vendor_bank-ch_banco = '4324'.
  z_sigam_vendor_bank-nu_conta_bancaria = '3422'.
  z_sigam_vendor_bank-st_atualizacao = '4'.
  APPEND z_sigam_vendor_bank.

*XI_SIGAM_INDICE
  DATA z_sigam_indice LIKE zmme_tcurr
         OCCURS 0 WITH HEADER LINE.
  z_sigam_indice-kurst = '32'.
  z_sigam_indice-fcurr = '21'.
  z_sigam_indice-tcurr = '33'.
  z_sigam_indice-gdatu = sy-datum.
  z_sigam_indice-ukurs = '432'.
  z_sigam_indice-ffact = 12.
  z_sigam_indice-tfact = 9.
  z_sigam_indice-atividade = '1'.
  z_sigam_indice-dt_atualizacao = sy-datum.
  z_sigam_indice-hr_atualizacao = '172338'.
  APPEND z_sigam_indice.

*XI_SIGAM_RETURN
  DATA z_sigam_return LIKE zfie_ret_document
         OCCURS 0 WITH HEADER LINE.
  z_sigam_return-obj_key = '323'.
  z_sigam_return-interface = 50.
  z_sigam_return-dt_atualizacao = sy-datum.
  z_sigam_return-hr_atualizacao = '172338'.
  z_sigam_return-type = 'T'.
  z_sigam_return-id = '001'.
  z_sigam_return-num = 12.
  z_sigam_return-message = 'teste xi'.
  APPEND z_sigam_return.

*XI_SIGAM_LOTE_COMPRA
  DATA z_sigam_return_lote LIKE zgl002_comp_f44
         OCCURS 0 WITH HEADER LINE.
  z_sigam_return_lote-mandt = sy-mandt.
  z_sigam_return_lote-bukrs = '0001'.
  z_sigam_return_lote-lote = '080101000125'.
  z_sigam_return_lote-status = 'S'.
  z_sigam_return_lote-sgtxt = 'teste xi'.
  APPEND z_sigam_return_lote.

*XI_XRT_AR_REA
  DATA z_movxrt_rea LIKE zfie_mov_xrt
         OCCURS 0 WITH HEADER LINE.
  z_movxrt_rea-acresc_blqto   = 0.
  z_movxrt_rea-age_codigo     = 'XX'.
  z_movxrt_rea-age_emit       = 'XX'.
  z_movxrt_rea-autor_0_por    = 'XXXXXXXXXXX'.
  z_movxrt_rea-autor_1_por    = 'XXXXXXXXXXX'.
  z_movxrt_rea-autor_0_em     = sy-datum.
  z_movxrt_rea-autor_1_em     = sy-datum.
  z_movxrt_rea-ban_codigo     = 'XXXXXX'.
  z_movxrt_rea-ban_emitente   = 'XXXXXX'.
  z_movxrt_rea-cec_codigo     = 'XXXXXX'.
  z_movxrt_rea-cnc_emitente   = 'XXXXXX'.
  z_movxrt_rea-cnt_codigo     = 'XXXXXX'.
  z_movxrt_rea-cnt_emitente   = 'XXXXXX'.
  z_movxrt_rea-codigo_barras  = 'XXXXXX'.
  z_movxrt_rea-conta_contabil = 'XXXXXX'.
  z_movxrt_rea-ct_ctbl_transf = 'XXXXXX'.
  z_movxrt_rea-dt_bloqueto    = sy-datum.
  z_movxrt_rea-dt_caixa       = sy-datum.
  z_movxrt_rea-dt_competencia = sy-datum.
  z_movxrt_rea-dt_contabil    = sy-datum.
  z_movxrt_rea-dt_efetivacao  = sy-datum.
  z_movxrt_rea-dt_pagamento   = sy-datum.
  z_movxrt_rea-desc_blqt      = 0.
  z_movxrt_rea-desc2_blqt     = 0.
  z_movxrt_rea-descr_blqt     = 'XXXXXX'.
  z_movxrt_rea-dff_01 = 'XXXXXX'.
  z_movxrt_rea-dff_02 = 'XXXXXX'.
  z_movxrt_rea-dff_03 = 'XXXXXX'.
  z_movxrt_rea-dff_04 = 'XXXXXX'.
  z_movxrt_rea-dff_05 = 'XXXXXX'.
  z_movxrt_rea-doc_origem = 'XXXXXX'.
  z_movxrt_rea-doc_pagador  = 'XXXXXX'.
  z_movxrt_rea-emitido_flag = 'X'.
  z_movxrt_rea-historico  = 'XXXXXX'.
  z_movxrt_rea-ind_codigo_ccc = 'XXXXXX'.
  z_movxrt_rea-ind_codigo_real  = 'XXXXXX'.
  z_movxrt_rea-inf_contabeis  = 'XXXXXX'.
  z_movxrt_rea-job_id = 0.
  z_movxrt_rea-lot_numero = 0.
  z_movxrt_rea-man_aut  = 'X'.
  z_movxrt_rea-mmi_codigo = 'XXXXXX'.
  z_movxrt_rea-mmi_ent_sai  = 'X'.
  z_movxrt_rea-mmi_reciprocidade  = 'XXXXXX'.
  z_movxrt_rea-mmi_transferencia  = 'XXXXXX'.
  z_movxrt_rea-multa_blqto  = 0.
  z_movxrt_rea-mva_contador = 0.
  z_movxrt_rea-mva_contador_par = 0.
  z_movxrt_rea-origem_contabil  = 'XXXXXX'.
  z_movxrt_rea-origem_pk  = 'XXXXXX'.
  z_movxrt_rea-origem_processo  = 'XXXXXX'.
  z_movxrt_rea-origem_sistema = 'XXXXXX'.
  z_movxrt_rea-pfj_cep  = 'XXXXXX'.
  z_movxrt_rea-pfj_cidade = 'XXXXXX'.
  z_movxrt_rea-pfj_codigo = 'XXXXXX'.
  z_movxrt_rea-pfj_complemento  = 'XXXXXX'.
  z_movxrt_rea-pfj_cpf  = 'XXXXXX'.
  z_movxrt_rea-pfj_descricao  = 'XXXXXX'.
  z_movxrt_rea-pfj_emitente = 'XXXXXX'.
  z_movxrt_rea-pfj_endereco = 'XXXXXX'.
  z_movxrt_rea-pfj_nome = 'XXXXXX'.
  z_movxrt_rea-pfj_numero = 'XXXXXX'.
  z_movxrt_rea-pfj_orig_dest  = 'XXXXXX'.
  z_movxrt_rea-pfj_tipo = 'X'.
  z_movxrt_rea-pfj_uf = 'XX'.
  z_movxrt_rea-prefix_doc_pag = 'XXXXXX'.
  z_movxrt_rea-status = 'XXXXXX'.
  z_movxrt_rea-status_aprovacao = 'XXXXXX'.
  z_movxrt_rea-sol_id = 0.
  z_movxrt_rea-tx_conv_cnt  = 0.
  z_movxrt_rea-tx_conv_cnt_fixa = 'X'.
  z_movxrt_rea-tx_conv_corr = 0.
  z_movxrt_rea-tx_conv_corr_fixa  = 'X'.
  z_movxrt_rea-tdo_codigo = 'XXXXXX'.
  z_movxrt_rea-tdp_codigo = 'XXXXXX'.
  z_movxrt_rea-temperatura  = 0.
  z_movxrt_rea-tipo_bloqueio  = 'X'.
  z_movxrt_rea-valor  = 0.
  z_movxrt_rea-valor_bloqueto = 0.
  z_movxrt_rea-valor_original = 0.

  APPEND z_movxrt_rea.


*XI_GEO_RETURN_ERROR
  IF p_geo_return_error IS NOT INITIAL.

    CALL FUNCTION 'Z_MM_GRAVA_BOL_ERROR'
      TABLES
        return_error = z_geo_return_error.

*    call function 'Z_MM_OUTBOUND_BOL_ERROR'
*      in background task destination 'XI_GEO_RETURN_ERROR'
*      tables
*        return_error = z_geo_return_error.
    COMMIT WORK.
  ENDIF.

*XI_GEO_RETURN_SUCESS
  IF p_geo_return_sucess IS NOT INITIAL.

    CALL FUNCTION 'Z_MM_GRAVA_BOL_SUCESS'
      TABLES
        return_sucess = z_geo_return_sucess.

*    call function 'Z_MM_OUTBOUND_BOL_SUCESS'
*      in background task destination 'XI_GEO_RETURN_SUCESS'
*      tables
*        return_sucess = z_geo_return_sucess.

    COMMIT WORK.
  ENDIF.


*XI_SIGAM_AP
  IF p_sigam_ap IS NOT INITIAL.
*--> 24.08.2023 17:28:12 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_PAYMENT_AP'
*      IN BACKGROUND TASK DESTINATION 'XI_SIGAM_AP'
*      TABLES
*        outpayment = z_sigam_ap.
*    COMMIT WORK.

    lv_fm = 'Z_FI_OUTBOUND_PAYMENT_AP'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = lv_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION lv_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        TABLES
          outpayment = z_sigam_ap.
    ELSE.
      CALL FUNCTION lv_fm IN BACKGROUND TASK
        TABLES
          outpayment = z_sigam_ap.
    ENDIF.

    COMMIT WORK.
*<-- 24.08.2023 17:28:12 - Migração S4 – ML – Fim
  ENDIF.


*XI_SIGAM_AR
  IF p_sigam_ar IS NOT INITIAL.
*--> 24.08.2023 17:30:17 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_PAYMENT_AR'
*      IN BACKGROUND TASK DESTINATION 'XI_SIGAM_AR'
*      TABLES
*        outpayment = z_sigam_ar.
*    COMMIT WORK.
    lv_fm = 'Z_FI_OUTBOUND_PAYMENT_AR'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = lv_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION lv_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        TABLES
          outpayment = z_sigam_ar.
    ELSE.
      CALL FUNCTION lv_fm IN BACKGROUND TASK
        TABLES
          outpayment = z_sigam_ar.
    ENDIF.

    COMMIT WORK.
*<-- 24.08.2023 17:30:17 - Migração S4 – ML – Fim
  ENDIF.

*XI_SIGAM_CUSTOMER
  IF p_sigam_customer IS NOT INITIAL.


*--> 23.08.2023 01:52:49 - Migração S4 – ML - Início
*    call function 'Z_FI_OUTBOUND_CUSTOMER'
*      in background task destination 'XI_SIGAM_CUSTOMER'
*      tables
*        outcustomer = z_sigam_customer
*        outbank     = z_sigam_customer_bank.
*    commit work.

    lv_fm = 'Z_FI_OUTBOUND_CUSTOMER'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = lv_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION lv_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        TABLES
          outcustomer = z_sigam_customer
          outbank     = z_sigam_customer_bank.
    ELSE.
      CALL FUNCTION lv_fm IN BACKGROUND TASK
        TABLES
          outcustomer = z_sigam_customer
          outbank     = z_sigam_customer_bank.
    ENDIF.

    COMMIT WORK.

*<-- 23.08.2023 01:52:49 - Migração S4 – ML – Fim
  ENDIF.


*XI_SIGAM_VENDOR
  IF p_sigam_vendor IS NOT INITIAL.
*--> 24.08.2023 17:53:05 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_VENDOR'
*      IN BACKGROUND TASK DESTINATION 'XI_SIGAM_VENDOR'
*      TABLES
*        outvendor = z_sigam_vendor
*        outbank   = z_sigam_vendor_bank.
*    COMMIT WORK.

  lv_fm = 'Z_FI_OUTBOUND_VENDOR'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = lv_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION lv_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        outvendor = z_sigam_vendor
        outbank   = z_sigam_vendor_bank.
  ELSE.
    CALL FUNCTION lv_fm IN BACKGROUND TASK
      TABLES
        outvendor = z_sigam_vendor
        outbank   = z_sigam_vendor_bank.
  ENDIF.

  COMMIT WORK.
*<-- 24.08.2023 17:53:05 - Migração S4 – ML – Fim
  ENDIF.

*XI_SIGAM_INDICE
  IF p_sigam_indice IS NOT INITIAL.
    CALL FUNCTION 'Z_FI_OUTBOUND_CAMBIO'
      IN BACKGROUND TASK DESTINATION 'XI_SIGAM_INDICE'
      TABLES
        t_tcurr = z_sigam_indice.
    COMMIT WORK.
  ENDIF.


*XI_SIGAM_RETURN
  IF p_sigam_return IS NOT INITIAL.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN'
*      IN BACKGROUND TASK DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = z_sigam_return.

    lv_fm =  'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = lv_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION lv_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = z_sigam_return.
    ELSE.
      CALL FUNCTION lv_fm IN BACKGROUND TASK
        TABLES
          outreturn = z_sigam_return.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

    COMMIT WORK.
  ENDIF.

*XI_SIGAM_LOTE_COMPRA
  IF p_sigam_lote_compra IS NOT INITIAL.

    CALL FUNCTION 'Z_FI_OUTBOUND_LOTE_COMPRA_ADD'
      TABLES
        return = z_sigam_return_lote.

    COMMIT WORK.
  ENDIF.

*XI_XRT_AR_REA
  IF p_xrt_rea IS NOT INITIAL.
    CALL FUNCTION 'Z_FI_OUTBOUND_APAR_XRT_REA' IN BACKGROUND TASK
      DESTINATION 'XI_XRT_AR_REA'
      TABLES
        outdocument = z_movxrt_rea.
    COMMIT WORK.
  ENDIF.


ENDFUNCTION.
