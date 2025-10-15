FUNCTION z_sd_inbound_nom_transporte.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_ZNOM_TRANSPORTE STRUCTURE  ZNOM_TRANSPORTE
*"----------------------------------------------------------------------
  DATA: p_id_nomeacao_tran LIKE znom_transporte-id_nomeacao_tran,
        p_in_status_comex  LIKE znom_transporte-in_status_comex.

  DATA: BEGIN OF t_log OCCURS 0.
          INCLUDE STRUCTURE zfie_ret_document.
  DATA: END OF t_log.

  DATA: BEGIN OF wa_znom_transporte.
          INCLUDE STRUCTURE znom_transporte.
  DATA: END OF wa_znom_transporte.

  LOOP AT t_znom_transporte.

    IF t_znom_transporte-in_status_comex EQ 'S'.
      p_in_status_comex := ' '.
    ELSE.
      p_in_status_comex := 'X'.
    ENDIF.

    SELECT SINGLE id_nomeacao_tran
      INTO p_id_nomeacao_tran
      FROM znom_transporte
     WHERE id_nomeacao_tran EQ t_znom_transporte-id_nomeacao_tran.

    IF sy-subrc EQ 0.
      IF p_in_status_comex EQ 'X'.
        UPDATE znom_transporte SET in_status_comex = p_in_status_comex
         WHERE id_nomeacao_tran EQ t_znom_transporte-id_nomeacao_tran.
      ELSE.
        UPDATE znom_transporte SET
            nr_ano           = t_znom_transporte-nr_ano
            nr_mes           = t_znom_transporte-nr_mes
            ds_porto         = t_znom_transporte-ds_porto
            ds_terminal      = t_znom_transporte-ds_terminal
            id_transporte    = t_znom_transporte-id_transporte
            ds_nome_transpor = t_znom_transporte-ds_nome_transpor
            nr_qtde_nomeada  = t_znom_transporte-nr_qtde_nomeada
            nr_viagem        = t_znom_transporte-nr_viagem
            in_status_comex  = p_in_status_comex
         WHERE id_nomeacao_tran EQ t_znom_transporte-id_nomeacao_tran.
      ENDIF.

      t_log-message        = 'Nomeação atualizado com sucesso '.

    ELSE.
      wa_znom_transporte-id_nomeacao_tran = t_znom_transporte-id_nomeacao_tran.
      wa_znom_transporte-nr_ano           = t_znom_transporte-nr_ano.
      wa_znom_transporte-nr_mes           = t_znom_transporte-nr_mes.
      wa_znom_transporte-ds_porto         = t_znom_transporte-ds_porto.
      wa_znom_transporte-ds_terminal      = t_znom_transporte-ds_terminal.
      wa_znom_transporte-id_transporte    = t_znom_transporte-id_transporte.
      wa_znom_transporte-ds_nome_transpor = t_znom_transporte-ds_nome_transpor.
      wa_znom_transporte-nr_qtde_nomeada  = t_znom_transporte-nr_qtde_nomeada.
      wa_znom_transporte-nr_viagem        = t_znom_transporte-nr_viagem.
      wa_znom_transporte-in_status_comex  = p_in_status_comex.
      INSERT INTO znom_transporte VALUES wa_znom_transporte.

      t_log-message        = 'Nomeação inserida com sucesso '.
    ENDIF.

    t_log-obj_key        = t_znom_transporte-id_nomeacao_tran.
    t_log-interface      = 34.
    t_log-dt_atualizacao = sy-datum.
    t_log-hr_atualizacao = sy-uzeit.
    t_log-type           = 'S'.
    t_log-id             = 'NOM-TRANSP'.
    t_log-num            = 0.

    t_log-message_v1     = t_znom_transporte-id_nomeacao_tran.
    APPEND t_log.

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
          outreturn = t_log.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = t_log.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

  ENDIF.

  COMMIT WORK.

ENDFUNCTION.
