FUNCTION zpp_inbound_entrada_producao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_XI_MFBF TYPE  ZPPT_XIMFBF
*"----------------------------------------------------------------------

  CONSTANTS: cc_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

  DATA: lv_rfc TYPE rfcdest.
  DATA: lwa_zpps_ximfbf_log TYPE zpps_ximfbf_log.

  DATA: lwa_return_log_mfpf TYPE zfie_ret_document,
        lit_return_log_mfpf TYPE TABLE OF zfie_ret_document INITIAL SIZE 0.

  LOOP AT t_xi_mfbf INTO DATA(lwa_xi_mfbf).

    IF lwa_xi_mfbf-zst_atlz = 'X'. "Entrada Produção Origem Transferencia
       lwa_xi_mfbf-nr_romaneio = lwa_xi_mfbf-mblnr.
       CLEAR: lwa_xi_mfbf-mblnr.
    ENDIF.

    SELECT SINGLE *
      FROM zpps_ximfbf_log INTO lwa_zpps_ximfbf_log
     WHERE obj_key = lwa_xi_mfbf-obj_key.

    IF sy-subrc EQ 0.

      IF lwa_zpps_ximfbf_log-processado EQ 'P'. "Se registro se encontra em processamento, ignorar o recebimento do mesmo...  P = Em processamento
        CONTINUE.
      ENDIF.

      IF ( lwa_zpps_ximfbf_log-zst_atlz = 'I' or lwa_zpps_ximfbf_log-zst_atlz = 'X' ) AND lwa_zpps_ximfbf_log-mblnr IS NOT INITIAL .
        CLEAR: lwa_return_log_mfpf.
        lwa_return_log_mfpf-obj_key          = lwa_zpps_ximfbf_log-obj_key.
        lwa_return_log_mfpf-dt_atualizacao   = sy-datum.
        lwa_return_log_mfpf-hr_atualizacao   = sy-uzeit.
        lwa_return_log_mfpf-interface        = '09'.
        lwa_return_log_mfpf-message          = |Confirmação { lwa_zpps_ximfbf_log-confirmation } - Documento de material { lwa_zpps_ximfbf_log-mblnr } / { lwa_zpps_ximfbf_log-dtmvto(4) } gerado com sucesso..|.
        lwa_return_log_mfpf-message_v1       = lwa_zpps_ximfbf_log-confirmation.
        lwa_return_log_mfpf-message_v2       = lwa_zpps_ximfbf_log-mblnr.
        lwa_return_log_mfpf-message_v3       = lwa_zpps_ximfbf_log-dtmvto(4).
        lwa_return_log_mfpf-type             = 'S'.
        lwa_return_log_mfpf-id               = 'Z01'.
        lwa_return_log_mfpf-num              = '000'.
        lwa_return_log_mfpf-info_adicional_1 = 'Z_PP_INBOUND_MFBF'.
        APPEND lwa_return_log_mfpf TO lit_return_log_mfpf.
        CONTINUE.
      ENDIF.

    ENDIF.

    CLEAR: lwa_zpps_ximfbf_log.
    MOVE-CORRESPONDING lwa_xi_mfbf TO lwa_zpps_ximfbf_log.
    lwa_zpps_ximfbf_log-mandt         = sy-mandt.
    lwa_zpps_ximfbf_log-data          = sy-datum.
    lwa_zpps_ximfbf_log-hora          = sy-uzeit.
    lwa_zpps_ximfbf_log-zrg_atulizado = 'N'.
    lwa_zpps_ximfbf_log-processado    = 'N'.

    CALL FUNCTION 'ENQUEUE_EZPPS_XIMFBF_LOG'
      EXPORTING
        obj_key   = lwa_xi_mfbf-obj_key
        nrobol    = lwa_xi_mfbf-nrobol
        fgorigem  = lwa_xi_mfbf-fgorigem
        werks     = lwa_xi_mfbf-werks
        matnr     = lwa_xi_mfbf-matnr
        cd_ccusto = lwa_xi_mfbf-cd_ccusto
        charg     = lwa_xi_mfbf-charg
      EXCEPTIONS
        OTHERS    = 3.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lwa_zpps_ximfbf_log-matnr
      IMPORTING
        output = lwa_zpps_ximfbf_log-matnr.

    MODIFY zpps_ximfbf_log FROM lwa_zpps_ximfbf_log.


    CALL FUNCTION 'DEQUEUE_EZPPS_XIMFBF_LOG'
      EXPORTING
        obj_key   = lwa_xi_mfbf-obj_key
        nrobol    = lwa_xi_mfbf-nrobol
        fgorigem  = lwa_xi_mfbf-fgorigem
        werks     = lwa_xi_mfbf-werks
        matnr     = lwa_xi_mfbf-matnr
        cd_ccusto = lwa_xi_mfbf-cd_ccusto
        charg     = lwa_xi_mfbf-charg.

  ENDLOOP.

  IF lit_return_log_mfpf[] IS NOT INITIAL.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = cc_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION cc_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = lit_return_log_mfpf.
    ELSE.
      CALL FUNCTION cc_fm IN BACKGROUND TASK
        TABLES
          outreturn = lit_return_log_mfpf.
    ENDIF.
  ENDIF.

  COMMIT WORK.

ENDFUNCTION.
