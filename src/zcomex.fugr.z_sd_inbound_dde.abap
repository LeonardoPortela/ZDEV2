FUNCTION z_sd_inbound_dde.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      TB_ZDDE STRUCTURE  ZDDE
*"      TB_ZDDE_APLICACAO STRUCTURE  ZDDE_APLICACAO
*"----------------------------------------------------------------------

  DATA: BEGIN OF t_log OCCURS 0.
          INCLUDE STRUCTURE zfie_ret_document.
  DATA: END OF t_log.

  DATA: BEGIN OF wa_zdde_aplicacao.
          INCLUDE STRUCTURE zdde_aplicacao.
  DATA: END OF wa_zdde_aplicacao.

  DATA: BEGIN OF wa_zdde.
          INCLUDE STRUCTURE zdde.
  DATA: END OF wa_zdde.

  DATA: pid_nomeacao_tran LIKE znom_transporte-id_nomeacao_tran,
        pid_registro_expo LIKE zreg_exportacao-id_registro_expo,
        pid_dde           LIKE zdde-id_dde,
        perro             TYPE c,
        it_zdde_aplicacao LIKE STANDARD TABLE OF wa_zdde_aplicacao.

  LOOP AT tb_zdde.

    SELECT SINGLE id_nomeacao_tran
      INTO pid_nomeacao_tran
      FROM znom_transporte
     WHERE id_nomeacao_tran EQ tb_zdde-id_nomeacao_tran.

    IF sy-subrc EQ 0.
      perro = 'N'.
      CLEAR it_zdde_aplicacao.

      LOOP AT tb_zdde_aplicacao WHERE id_dde = tb_zdde-id_dde.

        SELECT SINGLE id_registro_expo
          INTO pid_registro_expo
          FROM zreg_exportacao
         WHERE id_registro_expo EQ tb_zdde_aplicacao-id_registro_expo.

        IF sy-subrc EQ 0.
          CLEAR wa_zdde_aplicacao.
          wa_zdde_aplicacao-id_dde           = tb_zdde_aplicacao-id_dde.
          wa_zdde_aplicacao-id_registro_expo = tb_zdde_aplicacao-id_registro_expo.
          APPEND wa_zdde_aplicacao TO it_zdde_aplicacao.

        ELSE.
          perro = 'S'.
          t_log-obj_key        = tb_zdde-id_dde.
          t_log-interface      = 17.
          t_log-dt_atualizacao = sy-datum.
          t_log-hr_atualizacao = sy-uzeit.
          t_log-type           = 'E'.
          t_log-id             = 'DDE-RE'.
          t_log-num            = 0.
          CONCATENATE 'Não foi localizado o RE: ' tb_zdde_aplicacao-id_registro_expo INTO t_log-message SEPARATED BY space.
          t_log-message_v1     = tb_zdde-id_dde.
          t_log-message_v2     = tb_zdde_aplicacao-id_registro_expo.
          APPEND t_log.
        ENDIF.

      ENDLOOP.

      IF perro EQ 'N'.

        wa_zdde = tb_zdde.

        IF tb_zdde-in_status_comex EQ 'S'.
          wa_zdde-in_status_comex := ' '.
        ELSE.
          wa_zdde-in_status_comex := 'X'.
        ENDIF.

        MODIFY zdde FROM wa_zdde.

        DELETE FROM zdde_aplicacao WHERE id_dde EQ wa_zdde-id_dde.

        LOOP AT it_zdde_aplicacao INTO wa_zdde_aplicacao.

          DELETE FROM zdde_aplicacao WHERE id_registro_expo EQ wa_zdde_aplicacao-id_registro_expo
                                       AND id_dde           NE wa_zdde_aplicacao-id_dde.

          MODIFY zdde_aplicacao FROM wa_zdde_aplicacao.

          CALL FUNCTION 'Z_MEMO_ATUALIZA_RE'
            EXPORTING
              pzdde_aplicacao = wa_zdde_aplicacao.

          t_log-obj_key        = tb_zdde-id_dde.
          t_log-interface      = 17.
          t_log-dt_atualizacao = sy-datum.
          t_log-hr_atualizacao = sy-uzeit.
          t_log-type           = 'S'.
          t_log-id             = 'DDE-NOMEACAO'.
          t_log-num            = 0.
          t_log-message = 'DDE Registrado com sucesso. '.
          t_log-message_v1     = tb_zdde-id_dde.
          t_log-message_v2     = wa_zdde_aplicacao-ID_REGISTRO_EXPO.
          APPEND t_log.


        ENDLOOP.

      ENDIF.

    ELSE.
      t_log-obj_key        = tb_zdde-id_dde.
      t_log-interface      = 17.
      t_log-dt_atualizacao = sy-datum.
      t_log-hr_atualizacao = sy-uzeit.
      t_log-type           = 'E'.
      t_log-id             = 'DDE-NOMEACAO'.
      t_log-num            = 0.
      CONCATENATE 'Não foi localizado a nomeação: ' tb_zdde-id_nomeacao_tran INTO t_log-message SEPARATED BY space.
      t_log-message_v1     = tb_zdde-id_dde.
      t_log-message_v2     = tb_zdde-id_nomeacao_tran.
      APPEND t_log.
    ENDIF.

  ENDLOOP.

  IF NOT t_log[] IS INITIAL.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = t_log[].

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
