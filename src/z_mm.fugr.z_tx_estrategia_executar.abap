FUNCTION z_tx_estrategia_executar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"     VALUE(OK) TYPE  CHAR01
*"  TABLES
*"      T_LOTES STRUCTURE  ZMMT0149
*"      T_ESTRA STRUCTURE  ZMM_ESTRATEGIA_TAXA
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: icon.


  DATA: tg_estra   TYPE TABLE OF zmm_estrategia_taxa,
        wg_cadlote TYPE zmmt0149.

  DATA: wl_estra       LIKE LINE OF tg_estra,
        wl_estra2      LIKE LINE OF tg_estra,
        w_estra        TYPE         zmm_estrategia_taxa,
        w_estra2       TYPE         zmm_estrategia_taxa,
        wl_input_estra TYPE zmmt0151,
        flag_undo(1),
        linha_estra    TYPE sy-tabix,
        ult_linha      TYPE sy-tabix,
        e_row_id       TYPE sy-tabix.

  ok = abap_false.

  LOOP AT t_estra.
    MOVE-CORRESPONDING t_estra TO wl_estra.
    MOVE t_estra-chave_nfe TO wl_estra-chave_nfe.
    APPEND wl_estra TO tg_estra.
  ENDLOOP.

  SORT tg_estra BY nivel aprovador.
  SORT t_estra  BY nivel aprovador.

  LOOP AT tg_estra INTO wl_estra WHERE aprovador = v_usuario.
    IF sy-subrc = 0.
      IF e_row_id IS NOT INITIAL AND sy-tabix NE e_row_id + 1.
        EXIT.
      ELSEIF e_row_id IS NOT INITIAL AND sy-tabix EQ e_row_id + 1.
        IF wl_estra-opcoes NE icon_system_undo.
          MOVE icon_set_state TO wl_estra-opcoes.
        ENDIF.
        e_row_id = sy-tabix.
      ELSE.
        e_row_id = sy-tabix.
      ENDIF.

      READ TABLE t_lotes INDEX 1.
*      wg_cadlote-empresa  = t_lotes-empresa.
*      CONCATENATE  t_lotes-lote'-'  INTO wg_cadlote-lote.
*      wg_cadlote-usuario  = v_usuario .
*      wg_cadlote-total    = t_lotes-total.
*      wg_cadlote-dep_resp = t_lotes-dep_resp+0(2).
*      wg_cadlote-data     = t_lotes-dt_venc.

      IF wl_estra-opcoes NE icon_set_state AND wl_estra-opcoes NE icon_system_undo AND wl_estra-opcoes NE icon_reject .
        msg =  'Opção inválida para processamento!'.
        EXIT.
      ENDIF.

      IF  wl_estra-opcoes = icon_set_state.
        flag_undo = 'S'.
        linha_estra =  e_row_id.
        LOOP AT tg_estra INTO wl_estra2.
          ult_linha = sy-tabix.
          IF wl_estra2-opcoes = icon_set_state AND sy-tabix LT e_row_id.
            flag_undo = 'N'.
          ENDIF.
        ENDLOOP.

        IF flag_undo = 'S'.

          SELECT SINGLE * FROM zmmt0149 INTO @DATA(ws_zmmt0149) WHERE  chave_nfe = @wl_estra-chave_nfe.
          IF sy-subrc EQ 0.
            wl_input_estra-info_wkurs = ws_zmmt0149-info_wkurs.
            wl_input_estra-calc_wkurs = ws_zmmt0149-calc_wkurs.
            wl_input_estra-desvio     = ws_zmmt0149-desvio    .
          ENDIF.

          wl_input_estra-mandt       = sy-mandt.
          wl_input_estra-chave_nfe   = wl_estra-chave_nfe.
          wl_input_estra-nivel       = wl_estra-nivel.
          wl_input_estra-aprovador   = wl_estra-aprovador.
          wl_input_estra-valor_de    = wl_estra-valor_de.
          wl_input_estra-valor_ate   = wl_estra-valor_ate.
          wl_input_estra-data_atual  = sy-datum.
          wl_input_estra-hora_atual  = sy-uzeit.
          wl_input_estra-usuario     = sy-uname.
          wl_input_estra-motivo      = t_lotes-motivo.


          MODIFY zmmt0151 FROM wl_input_estra.
          CLEAR wl_input_estra.

          UPDATE zmmt0149 SET motivo = t_lotes-motivo
            WHERE chave_nfe = wl_estra-chave_nfe.
          COMMIT WORK.


          IF ult_linha = linha_estra.

            UPDATE zmmt0149 SET status = 'A'
            WHERE chave_nfe = wl_estra-chave_nfe.
            COMMIT WORK.

            LOOP AT tg_estra INTO wl_estra.
              wl_estra-opcoes = ' ' .
              wl_estra-estado = icon_checked .
              MODIFY tg_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
            ENDLOOP.
            msg = 'Processamento concluído com sucesso'.
            ok = abap_true.
          ELSE.
            wl_estra-opcoes = icon_system_undo .
            wl_estra-estado = icon_checked .
            MODIFY tg_estra FROM wl_estra INDEX e_row_id.
            msg = 'Processamento concluído com sucesso'.
            ok = abap_true.
          ENDIF.
        ELSE.
          msg = 'Devem ser aprovadas as estratégias anteriores'.
        ENDIF.
      ELSEIF  wl_estra-opcoes = icon_system_undo .
        flag_undo = 'S'.
        linha_estra =  e_row_id.
        LOOP AT tg_estra INTO wl_estra2.
          IF wl_estra2-opcoes = icon_system_undo AND sy-tabix GT e_row_id
            AND wl_estra2-aprovador NE wl_estra-aprovador.
            flag_undo = 'N'.
          ENDIF.
          IF wl_estra2-opcoes = icon_message_critical.
            msg = 'Solicitação totalmente liberada'.
            flag_undo = 'N'.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF flag_undo = 'S'.
          DELETE  FROM zmmt0151
              WHERE chave_nfe  = wl_estra-chave_nfe
              AND   nivel      = wl_estra-nivel
              AND   aprovador  = wl_estra-aprovador.
          wl_estra-estado = icon_led_yellow  .
          wl_estra-opcoes = icon_set_state.
          MODIFY tg_estra FROM wl_estra INDEX e_row_id.
        ELSE.
          msg = 'Devem ser reiniciadas as estratégias posteriores'.
        ENDIF.
      ELSEIF wl_estra-opcoes = icon_reject.
        UPDATE zmmt0149 SET status = 'R' "149
              WHERE chave_nfe = wl_estra-chave_nfe.
        COMMIT WORK.

      ENDIF.
      LOOP AT tg_estra INTO wl_estra.
        IF e_row_id = sy-tabix.
          MOVE-CORRESPONDING wl_estra TO w_estra.
          MODIFY t_estra FROM w_estra INDEX sy-tabix TRANSPORTING opcoes estado.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  CLEAR: ws_zmmt0149.

ENDFUNCTION.
