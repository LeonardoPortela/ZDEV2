FUNCTION z_fi_reverse_document_post .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_REVERSE STRUCTURE  ZFIE_REVERSE_DOCUMENT
*"----------------------------------------------------------------------

  REFRESH: it_reverse_ret, it_reverse_log.

  DATA: t_bdc LIKE bdcdata OCCURS 0 WITH HEADER LINE.

  DATA: v_mode TYPE c.

*--> 24.08.2023 18:54:32 - Migração S4 – ML - Início
  DATA: lv_rfc TYPE rfcdest,
        lv_fm  TYPE rs38l_fnam.
*<-- 24.08.2023 18:54:32 - Migração S4 – ML – Fim



  LOOP AT it_reverse INTO wa_reverse.

*> Verifica a existência do documento através da chv referencia(AWKEY)
    CLEAR: vg_awkey, vg_awtyp, vg_awsys, vg_bukrs, vg_belnr,
           vg_gjahr, vg_mandt.
    SELECT bukrs belnr gjahr awtyp awkey awsys mandt
             FROM bkpf UP TO 1 ROWS
             INTO (vg_bukrs, vg_belnr, vg_gjahr,
                   vg_awtyp, vg_awkey, vg_awsys, vg_mandt)
            WHERE ( bukrs EQ wa_reverse-bukrs )
              AND ( belnr EQ wa_reverse-belnr )
              AND ( gjahr EQ wa_reverse-gjahr )
            ORDER BY PRIMARY KEY.
    ENDSELECT.

    IF ( sy-subrc NE 0 ).
      append_reverse_log 'FB08' 'E' 'Z01' '008'
             vg_awkey ''  ''.
      CONTINUE.
    ENDIF.

    REFRESH it_message.

    REFRESH t_bdc.
    t_bdc-program = 'SAPMF05A'.
    t_bdc-dynpro  = '0105'.
    t_bdc-dynbegin = 'X'.
    APPEND t_bdc.

    CLEAR t_bdc.
    t_bdc-fnam = 'RF05A-BELNS'.
    t_bdc-fval = wa_reverse-belnr.
    APPEND t_bdc.

    CLEAR t_bdc.
    t_bdc-fnam = 'BKPF-BUKRS'.
    t_bdc-fval = wa_reverse-bukrs.
    APPEND t_bdc.

    CLEAR t_bdc.
    t_bdc-fnam = 'RF05A-GJAHS'.
    t_bdc-fval = wa_reverse-gjahr.
    APPEND t_bdc.

    CLEAR t_bdc.
    t_bdc-fnam = 'UF05A-STGRD'.
    t_bdc-fval = '01'.
    APPEND t_bdc.

    CLEAR t_bdc.
    t_bdc-fnam = 'BDC_OKCODE'.
    t_bdc-fval = '=BU'.
    APPEND t_bdc.

    v_mode = 'N'.
    CALL TRANSACTION 'FB08' USING t_bdc
                     MODE v_mode
                     UPDATE 'S'
                     MESSAGES INTO it_message.


    READ TABLE it_message INTO wa_message WITH KEY msgtyp = 'E'.
    IF ( sy-subrc EQ 0 ).
*>  Grava erros em caso de insucesso
      LOOP AT it_message INTO wa_message.
        append_reverse_log 'FB08' wa_message-msgtyp  wa_message-msgid
                            wa_message-msgnr     wa_message-msgv1
                            wa_message-msgv2 wa_message-msgv3.
      ENDLOOP.

    ELSE.
      CLEAR wa_reverse_ret.
      MOVE-CORRESPONDING wa_reverse TO wa_reverse_ret.
*>  Executa o cancelamento da nota fiscal como na J1B3
      CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
        EXPORTING
          doc_number               = wa_reverse-docnum
          ref_type                 = space
          ref_key                  = space
        IMPORTING
          doc_number               = wa_reverse_ret-docnum_e
        EXCEPTIONS
          document_not_found       = 1
          cancel_not_possible      = 2
          nf_cancel_type_not_found = 3
          database_problem         = 4
          docum_lock               = 5
          OTHERS                   = 6.

      IF ( sy-subrc NE 0 ).
*>  Grava erros em caso de insucesso
        append_reverse_log 'J1B3' sy-msgty sy-msgid sy-msgno
                                  sy-msgv1 sy-msgv2 sy-msgv3.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          IMPORTING
            return = wa_bapiret.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        wa_reverse_ret-cd_transacao = 'BAPI'.
*> Verifica a existência do documento de estorno criado
        CLEAR: vg_awkey, vg_awtyp, vg_awsys.
        SELECT awtyp awkey awsys bukrs belnr gjahr
          FROM bkpf UP TO 1 ROWS
          INTO (vg_awtyp, vg_awkey, vg_awsys, wa_reverse_ret-bukrs_e,
           wa_reverse_ret-belnr_e, wa_reverse_ret-gjahr_e)
         WHERE ( awtyp EQ wa_reversal-obj_type )
           AND ( awkey EQ wa_reversal-obj_key  )
      ORDER BY awtyp awkey awsys.
        ENDSELECT.

        APPEND wa_reverse_ret TO it_reverse_ret.

      ENDIF.

    ENDIF.
  ENDLOOP.

*> Executa a rotina outbound para Estornos realizados com sucesso
  IF ( NOT it_reverse_ret[] IS INITIAL ).
*--> 24.08.2023 18:51:49 - Migração S4 – ML - Início
*  CALL FUNCTION 'Z_FI_OUTBOUND_REVERSE' IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_REVERSE'
*    AS SEPARATE UNIT
*    TABLES
*      outreverse = it_reverse_ret.
*  COMMIT WORK.

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
*<-- 24.08.2023 18:51:49 - Migração S4 – ML – Fim
  ENDIF.
*> Executa a rotina outbound para estornos não realizados
  IF ( NOT it_reverse_log[] IS INITIAL ).
*--> 24.08.2023 18:52:27 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_REVERSE_LOG' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_REVERSE_LOG'
*      AS SEPARATE UNIT
*      TABLES
*        outreverse = it_reverse_log.
*    COMMIT WORK.

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
       AS SEPARATE UNIT
       TABLES
         outreverse = it_reverse_log.
   ELSE.
     CALL FUNCTION lv_fm IN BACKGROUND TASK
       TABLES
         outreverse = it_reverse_log.
   ENDIF.

   COMMIT WORK.
*<-- 24.08.2023 18:52:27 - Migração S4 – ML – Fim
  ENDIF.

ENDFUNCTION.
