  FUNCTION z_sd_inbound_bl.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      TB_ZNOM_CONHEC STRUCTURE  ZNOM_CONHEC
*"----------------------------------------------------------------------

    DATA: BEGIN OF t_log OCCURS 0.
            INCLUDE STRUCTURE zfie_ret_document.
    DATA: END OF t_log.

    DATA: BEGIN OF wa_znom_conhec.
            INCLUDE STRUCTURE znom_conhec.
    DATA: END OF wa_znom_conhec.



    DATA: pid_nomeacao_tran LIKE znom_conhec-id_nomeacao_tran,
          pid_conhec        LIKE znom_conhec-id_conhec,
          pnr_dde           LIKE zdde-nr_dde,
          pid_nr_nota_exp   LIKE zdoc_memorando-nr_nota_exp.


    DATA: it_zdoc_memorando TYPE TABLE OF zdoc_memorando,
          wa_zdoc_memorando TYPE zdoc_memorando.

    CLEAR: it_zdoc_memorando.

    LOOP AT tb_znom_conhec.

      SELECT SINGLE id_nomeacao_tran
        INTO pid_nomeacao_tran
        FROM znom_transporte
       WHERE id_nomeacao_tran EQ tb_znom_conhec-id_nomeacao_tran.

      IF sy-subrc EQ 0.

        wa_znom_conhec = tb_znom_conhec.

* Solicitação conforme chamado 106626 CSB
        SELECT SINGLE nr_dde INTO pnr_dde
          FROM zdde
         WHERE id_nomeacao_tran EQ pid_nomeacao_tran.

*        SELECT NR_NOTA_EXP INTO PID_NR_NOTA_EXP
*          FROM ZDOC_MEMORANDO
*         WHERE NR_DDE EQ PNR_DDE.

        SELECT *
          FROM zdoc_memorando
           INTO TABLE it_zdoc_memorando
         WHERE nr_dde EQ pnr_dde.

        IF sy-subrc EQ 0.
          LOOP AT it_zdoc_memorando INTO wa_zdoc_memorando.
            UPDATE zdoc_memo_nomeac SET
                     dt_conhec        = wa_znom_conhec-dt_data
               WHERE nr_nota_exp        EQ wa_zdoc_memorando-nr_nota_exp
                 AND nr_conhec          EQ wa_znom_conhec-nr_conhec.
          ENDLOOP.
          CLEAR: wa_zdoc_memorando.
        ENDIF.
* Fim chamado 106626

        IF tb_znom_conhec-in_status_comex EQ 'S'.
          wa_znom_conhec-in_status_comex := ' '.
        ELSE.
          wa_znom_conhec-in_status_comex := 'X'.
        ENDIF.

        SELECT SINGLE id_nomeacao_tran
          INTO pid_nomeacao_tran
          FROM znom_conhec
         WHERE id_nomeacao_tran EQ tb_znom_conhec-id_nomeacao_tran
           AND id_conhec        EQ tb_znom_conhec-id_conhec.

        IF sy-subrc EQ 0.
          UPDATE znom_conhec SET
                 dt_data         = wa_znom_conhec-dt_data
                 nr_qtde         = wa_znom_conhec-nr_qtde
                 nr_conhec       = wa_znom_conhec-nr_conhec
                 sg_pais_destino = wa_znom_conhec-sg_pais_destino
                 ds_tipo         = wa_znom_conhec-ds_tipo
                 in_status_comex = wa_znom_conhec-in_status_comex
           WHERE id_conhec        EQ wa_znom_conhec-id_conhec
             AND id_nomeacao_tran EQ wa_znom_conhec-id_nomeacao_tran.
        ELSE.
          INSERT INTO znom_conhec VALUES wa_znom_conhec.
        ENDIF.

        t_log-obj_key        = tb_znom_conhec-id_conhec.
        t_log-interface      = 18.
        t_log-dt_atualizacao = sy-datum.
        t_log-hr_atualizacao = sy-uzeit.
        t_log-type           = 'S'.
        t_log-id             = 'BL-NOMEACAO'.
        t_log-num            = 0.
        t_log-message        = 'BL Registrada com sucesso '.
        t_log-message_v1     = tb_znom_conhec-id_conhec.
        t_log-message_v2     = tb_znom_conhec-id_nomeacao_tran.
        APPEND t_log.


      ELSE.
        t_log-obj_key        = tb_znom_conhec-id_conhec.
        t_log-interface      = 18.
        t_log-dt_atualizacao = sy-datum.
        t_log-hr_atualizacao = sy-uzeit.
        t_log-type           = 'E'.
        t_log-id             = 'BL-NOMEACAO'.
        t_log-num            = 0.
        CONCATENATE 'Não foi localizado a nomeação: ' tb_znom_conhec-id_nomeacao_tran INTO t_log-message SEPARATED BY space.
        t_log-message_v1     = tb_znom_conhec-id_conhec.
        t_log-message_v2     = tb_znom_conhec-id_nomeacao_tran.
        APPEND t_log.
      ENDIF.


    ENDLOOP.

    IF NOT t_log[] IS INITIAL.
* ---> S4 Migration - 28/08/2023 - JGP - inicio
*      CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*        DESTINATION 'XI_SIGAM_RETURN'
*        TABLES
*          outreturn = t_log[].

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
            outreturn = t_log[].
      ELSE.
        CALL FUNCTION c_fm IN BACKGROUND TASK
          TABLES
            outreturn = t_log[].
      ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

    ENDIF.

    COMMIT WORK.

  ENDFUNCTION.
