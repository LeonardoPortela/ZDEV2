FUNCTION z_fi_nf_cancel_link.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_BUKRS) LIKE  BSEG-BUKRS
*"     VALUE(I_BELNR) LIKE  BSEG-BELNR
*"     VALUE(I_GJAHR) LIKE  BSEG-GJAHR
*"     VALUE(I_BUKRS_E) LIKE  BSEG-BUKRS
*"     VALUE(I_BELNR_E) LIKE  BSEG-BELNR
*"     VALUE(I_GJAHR_E) LIKE  BSEG-GJAHR
*"     VALUE(I_TCODE) LIKE  BKPF-TCODE
*"  EXPORTING
*"     VALUE(NFNUM) LIKE  J_1BNFDOC-NFNUM
*"----------------------------------------------------------------------

*> Verificar se o documento de estorno foi criado corretamente

  DATA: vl_found TYPE i,
        vl_belnr LIKE bseg-belnr,
        vl_gjahr LIKE bseg-gjahr,
        vl_tcode LIKE wa_reverse_ret-cd_transacao.

*--> 25.08.2023 11:10:18 - Migração S4 – ML - Início
  DATA: lv_rfc TYPE rfcdest,
        lv_fm TYPE rs38l_fnam.
*<-- 25.08.2023 11:10:18 - Migração S4 – ML – Fim

  REFRESH r_gjhar.
  CLEAR r_gjhar.
  r_gjhar-sign = 'I'.
  r_gjhar-option = 'BT'.
  r_gjhar-low  = '2008'.
  r_gjhar-high = '2011'.
  APPEND r_gjhar.

  vg_datum = sy-datum.
  vg_uzeit = sy-uzeit.

*> Verifica se a primeira possibilidade já foi criada na BKPF
  CLEAR vl_found.

  IF i_gjahr_e IS INITIAL.

    DO 3 TIMES.
*    wait up to 10 seconds.
      SELECT SINGLE mandt bukrs belnr gjahr awkey
        FROM bkpf
        INTO wa_bkpf
       WHERE ( bukrs EQ i_bukrs_e )
         AND ( belnr EQ i_belnr_e )
         AND ( gjahr IN r_gjhar ).

      CHECK ( sy-subrc EQ 0 ).
      vl_found = 1.
      EXIT.
    ENDDO.

  ELSE.
    SELECT SINGLE mandt bukrs belnr gjahr awkey
      FROM bkpf
      INTO wa_bkpf
     WHERE ( bukrs EQ i_bukrs_e )
       AND ( belnr EQ i_belnr_e )
       AND ( gjahr EQ i_gjahr_e ).

    CHECK ( sy-subrc EQ 0 ).
    vl_found = 1.
  ENDIF.

  CHECK ( NOT vl_found IS INITIAL ).

*> Verifica se o documento contábil estornado tem algum vínculo
*> com alguma nota fiscal.
  SELECT bukrs belnr gjahr docnum
    FROM j_1bnfdoc UP TO 1 ROWS
    INTO wa_nota
   WHERE ( bukrs EQ i_bukrs )
     AND ( belnr EQ i_belnr )
     AND ( gjahr IN r_gjhar )
    ORDER BY bukrs belnr gjahr.
  ENDSELECT.

  IF ( sy-subrc NE 0 ).
    CLEAR wa_nota.
    IF ( 'FB01 FB05' CS i_tcode ).
      vl_tcode = 'FB08'.
    ELSE.
      vl_tcode = 'FBRA'.
    ENDIF.

  ELSE.
*> Cancela o vínculo entre a nota fiscal e o documento contábil.
    REFRESH: it_message.
    CLEAR  : vl_belnr, vl_gjahr.
    vl_tcode = 'J1B2'.

    CALL FUNCTION 'Z_FI_DOCUMENT_LINK_WITH_NF'
      EXPORTING
        i_docnum = wa_nota-docnum
        i_belnr  = vl_belnr
        i_gjahr  = vl_gjahr
      TABLES
        message  = it_message.

    READ TABLE it_message INTO wa_message
                        WITH KEY msgtyp = 'E'.
    IF ( sy-subrc EQ 0 ).
      REFRESH it_reverse_log.
      CLEAR wa_reverse_log.
      wa_reverse_log-obj_key_estorno = wa_bkpf-awkey.
      wa_reverse_log-bukrs           = i_bukrs.
      wa_reverse_log-belnr           = i_belnr.
      wa_reverse_log-gjahr           = i_gjahr.
      wa_reverse_log-docnum          = wa_nota-docnum.
      wa_reverse_log-dt_atualizacao  = vg_datum.
      wa_reverse_log-hr_atualizacao  = vg_uzeit.
      wa_reverse_log-cd_transacao    = vl_tcode.

      LOOP AT it_message INTO wa_message.
        sy-msgid = wa_message-msgid.
        sy-msgno = wa_message-msgnr.
        sy-msgv1 = wa_message-msgv1.
        sy-msgv2 = wa_message-msgv2.
        sy-msgv3 = wa_message-msgv3.
        sy-msgv4 = wa_message-msgv4.

        CALL FUNCTION 'CUTC_GET_MESSAGE'
          EXPORTING
            msg_id      = sy-msgid
            msg_no      = sy-msgno
            msg_arg1    = sy-msgv1
            msg_arg2    = sy-msgv2
            msg_arg3    = sy-msgv3
            msg_arg4    = sy-msgv4
            language    = sy-langu
          IMPORTING
            raw_message = vg_messtab.

        wa_reverse_log-type           = wa_message-msgtyp.
        wa_reverse_log-id             = wa_message-msgid.
        wa_reverse_log-num            = wa_message-msgnr.
        wa_reverse_log-message        = vg_messtab.
        wa_reverse_log-message_v1     = wa_message-msgv1.
        wa_reverse_log-message_v2     = wa_message-msgv2.
        wa_reverse_log-message_v3     = wa_message-msgv3.
        APPEND wa_reverse_log TO it_reverse_log.
      ENDLOOP.
*--> 25.08.2023 11:08:54 - Migração S4 – ML - Início
*> Executa a rotina outbound para estornos não realizados
*      CALL FUNCTION 'Z_FI_OUTBOUND_REVERSE_LOG' IN BACKGROUND TASK
*        DESTINATION 'XI_SIGAM_REVERSE_LOG'
*        AS SEPARATE UNIT
*        TABLES
*          outreverse = it_reverse_log.
*      COMMIT WORK.

      lv_fm = 'Z_FI_OUTBOUND_REVERSE_LOG'.

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
          outreverse = it_reverse_log.
      ELSE.
        CALL FUNCTION lv_fm IN BACKGROUND TASK
          TABLES
          outreverse = it_reverse_log.
      ENDIF.

      COMMIT WORK.
*<-- 25.08.2023 11:08:54 - Migração S4 – ML – Fim

      RETURN.
    ENDIF.
  ENDIF.

  REFRESH it_reverse_ret.
  CLEAR wa_reverse_ret.
  wa_reverse_ret-obj_key_estorno = wa_bkpf-awkey.
  wa_reverse_ret-bukrs           = i_bukrs.
  wa_reverse_ret-belnr           = i_belnr.
  wa_reverse_ret-gjahr           = i_gjahr.
  wa_reverse_ret-docnum          = wa_nota-docnum.
  wa_reverse_ret-dt_atualizacao  = vg_datum.
  wa_reverse_ret-hr_atualizacao  = vg_uzeit.
  wa_reverse_ret-cd_transacao    = vl_tcode.
  wa_reverse_ret-bukrs_e         = i_bukrs_e.
  wa_reverse_ret-belnr_e         = i_belnr_e.
  wa_reverse_ret-gjahr_e         = i_gjahr_e.
  APPEND wa_reverse_ret TO it_reverse_ret.

*--> 24.08.2023 18:56:14 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_REVERSE' IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_REVERSE'
*    AS SEPARATE UNIT
*    TABLES
*      outreverse = it_reverse_ret.
*    COMMIT WORK.

  lv_fm = 'Z_FI_OUTBOUND_REVERSE'.

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
        outreverse = it_reverse_ret.
  ELSE.
    CALL FUNCTION lv_fm IN BACKGROUND TASK
      TABLES
        outreverse = it_reverse_ret.
  ENDIF.

  COMMIT WORK.
*<-- 24.08.2023 18:56:14 - Migração S4 – ML – Fim


ENDFUNCTION.
