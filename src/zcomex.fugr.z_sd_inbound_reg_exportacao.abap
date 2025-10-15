FUNCTION z_sd_inbound_reg_exportacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_ZREG_EXPORTACAO STRUCTURE  ZREG_EXPORTACAO
*"----------------------------------------------------------------------

  DATA: p_id_registro_expo LIKE zreg_exportacao-id_registro_expo,
        p_in_performance   LIKE zreg_exportacao-in_performance.

  DATA: BEGIN OF t_log OCCURS 0.
          INCLUDE STRUCTURE zfie_ret_document.
  DATA: END OF t_log.

  DATA: BEGIN OF wa_zreg_exportacao.
          INCLUDE STRUCTURE zreg_exportacao.
  DATA: END OF wa_zreg_exportacao.

  LOOP AT it_zreg_exportacao.

    IF it_zreg_exportacao-in_performance EQ '1'.
      p_in_performance = 'X'.
    ELSE.
      p_in_performance = ' '.
    ENDIF.

    SELECT SINGLE id_registro_expo INTO p_id_registro_expo
      FROM zreg_exportacao
     WHERE id_registro_expo EQ it_zreg_exportacao-id_registro_expo.

    IF sy-subrc EQ 0.
      IF it_zreg_exportacao-in_status_comex EQ 'S'.
        UPDATE zreg_exportacao
           SET nr_registro_expo = it_zreg_exportacao-nr_registro_expo
               id_exportador    = it_zreg_exportacao-id_exportador
               id_importador    = it_zreg_exportacao-id_importador
               cd_material      = it_zreg_exportacao-cd_material
               dt_registro_expo = it_zreg_exportacao-dt_registro_expo
               nr_qtde          = it_zreg_exportacao-nr_qtde
               nr_valor_tm      = it_zreg_exportacao-nr_valor_tm
               nr_valor_total   = it_zreg_exportacao-nr_valor_total
               id_registro_vend = it_zreg_exportacao-id_registro_vend
               id_nomeacao_tran = it_zreg_exportacao-id_nomeacao_tran
               in_performance   = p_in_performance
               id_pais_destino  = it_zreg_exportacao-id_pais_destino
               in_status_comex  = ' '
         WHERE id_registro_expo EQ p_id_registro_expo.
      ELSE.
        UPDATE zreg_exportacao
           SET in_status_comex  = 'X'
         WHERE id_registro_expo EQ p_id_registro_expo.
      ENDIF.
      t_log-message        = 'Registro de Exportação atualizado com sucesso '.
    ELSE.
      CLEAR wa_zreg_exportacao.
      wa_zreg_exportacao-id_registro_expo = it_zreg_exportacao-id_registro_expo.
      wa_zreg_exportacao-nr_registro_expo = it_zreg_exportacao-nr_registro_expo.
      wa_zreg_exportacao-id_exportador    = it_zreg_exportacao-id_exportador.
      wa_zreg_exportacao-id_importador    = it_zreg_exportacao-id_importador.
      wa_zreg_exportacao-cd_material      = it_zreg_exportacao-cd_material.
      wa_zreg_exportacao-dt_registro_expo = it_zreg_exportacao-dt_registro_expo.
      wa_zreg_exportacao-nr_qtde          = it_zreg_exportacao-nr_qtde.
      wa_zreg_exportacao-nr_valor_tm      = it_zreg_exportacao-nr_valor_tm.
      wa_zreg_exportacao-nr_valor_total   = it_zreg_exportacao-nr_valor_total.
      wa_zreg_exportacao-id_registro_vend = it_zreg_exportacao-id_registro_vend.
      wa_zreg_exportacao-id_nomeacao_tran = it_zreg_exportacao-id_nomeacao_tran.
      wa_zreg_exportacao-in_performance   = p_in_performance.
      wa_zreg_exportacao-id_pais_destino  = it_zreg_exportacao-id_pais_destino.
      IF it_zreg_exportacao-in_status_comex EQ 'S'.
        wa_zreg_exportacao-in_status_comex = ' '.
      ELSE.
        wa_zreg_exportacao-in_status_comex = 'X'.
      ENDIF.
      INSERT INTO zreg_exportacao VALUES wa_zreg_exportacao.

      t_log-message        = 'Registro de Exportação inserido com sucesso '.

    ENDIF.

    t_log-obj_key        = it_zreg_exportacao-id_registro_expo.
    t_log-interface      = 35.
    t_log-dt_atualizacao = sy-datum.
    t_log-hr_atualizacao = sy-uzeit.
    t_log-type           = 'S'.
    t_log-id             = 'REG-EXP'.
    t_log-num            = 0.
    t_log-message_v1     = it_zreg_exportacao-id_registro_expo.
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
