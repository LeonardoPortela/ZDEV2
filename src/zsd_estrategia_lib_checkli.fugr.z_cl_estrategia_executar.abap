FUNCTION z_cl_estrategia_executar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_USUARIO) TYPE  SY-UNAME DEFAULT SY-UNAME
*"  EXPORTING
*"     VALUE(E_MSG) TYPE  CHAR50
*"     VALUE(E_OK) TYPE  CHAR01
*"  TABLES
*"      T_CHECKLISTS STRUCTURE  ZSD_SIM_CHECKLIST_EST
*"      T_ESTRA STRUCTURE  ZSD_ESTRATEGIA_CHECKLIST
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: icon.


  DATA: tg_estra   TYPE TABLE OF zsd_estrategia_checklist,
        wg_cadlote TYPE zsd_sim_checklist_est.

  DATA: wl_estra       LIKE LINE OF tg_estra,
        wl_estra2      LIKE LINE OF tg_estra,
        w_estra        TYPE         zsd_estrategia_checklist,
        w_estra2       TYPE         zsd_estrategia_checklist,
        wl_input_estra TYPE zsdt0387,
        flag_undo(1),
        linha_estra    TYPE sy-tabix,
        ult_linha      TYPE sy-tabix,
        e_row_id       TYPE sy-tabix.

  e_ok = abap_false.

  LOOP AT t_estra.
    MOVE-CORRESPONDING t_estra TO wl_estra.
    MOVE t_estra-doc_simulacao TO wl_estra-doc_simulacao.
    APPEND wl_estra TO tg_estra.
  ENDLOOP.

  SORT tg_estra BY nivel aprovador.
  SORT t_estra  BY nivel aprovador.

*  DATA(lv_cnt_aprov) = REDUCE i( INIT x = 0 FOR wa IN t_estra WHERE ( aprovador = i_usuario ) NEXT x = x + 1 ).
*  DATA(lv_cnt_todos) = lines( t_estra ).

  LOOP AT tg_estra INTO wl_estra WHERE aprovador = i_usuario.

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

      READ TABLE t_checklists INDEX 1.

      IF wl_estra-opcoes NE icon_set_state AND wl_estra-opcoes NE icon_system_undo AND wl_estra-opcoes NE icon_reject .
        e_msg =  'Opção inválida para processamento!'.
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

          SELECT SINGLE * FROM zsdt0381
              INTO @DATA(ws_zsd_sim_checklist_est)
          WHERE  doc_simulacao = @wl_estra-doc_simulacao.

          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING ws_zsd_sim_checklist_est TO wl_input_estra.
          ENDIF.

          wl_input_estra-mandt       = sy-mandt.
          wl_input_estra-doc_simulacao   = wl_estra-doc_simulacao.
          wl_input_estra-nivel       = wl_estra-nivel.
          wl_input_estra-aprovador   = wl_estra-aprovador.

          wl_input_estra-data_atual  = sy-datum.
          wl_input_estra-hora_atual  = sy-uzeit.
          wl_input_estra-usuario     = sy-uname.
          "wl_input_estra-motivo      = t_checklists-motivo.
          wl_input_estra-vkbur      = t_checklists-vkbur.


          MODIFY zsdt0387 FROM wl_input_estra.
          CLEAR wl_input_estra.

*          UPDATE zsdt0385 SET motivo = t_checklists-motivo
*            WHERE doc_simulacao = wl_estra-doc_simulacao.
*          COMMIT WORK.


          IF ult_linha = linha_estra.

            UPDATE zsdt0381 SET status = '04'
                                date_change = sy-datum
                                time_create = sy-uzeit
                                user_change = sy-uname
            WHERE doc_simulacao = wl_estra-doc_simulacao.

            COMMIT WORK.

            LOOP AT tg_estra INTO wl_estra.
              wl_estra-opcoes = ' ' .
              wl_estra-estado = icon_checked .
              MODIFY tg_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
            ENDLOOP.
            e_msg = 'Processamento concluído com sucesso'.
            e_ok = abap_true.
          ELSE.
            wl_estra-opcoes = icon_system_undo .
            wl_estra-estado = icon_checked .
            MODIFY tg_estra FROM wl_estra INDEX e_row_id.
            e_msg = 'Processamento concluído com sucesso'.
            e_ok = abap_true.
          ENDIF.
        ELSE.
          e_msg = 'Devem ser aprovadas as estratégias anteriores'.
        ENDIF.
      ELSEIF  wl_estra-opcoes = icon_system_undo .
        flag_undo = 'S'.
        linha_estra =  e_row_id.
        LOOP AT tg_estra INTO wl_estra2.
          IF wl_estra2-opcoes = icon_system_undo AND sy-tabix GT e_row_id
            AND wl_estra2-aprovador NE wl_estra-aprovador.
            flag_undo = 'N'.
          ENDIF.
          IF wl_estra2-opcoes = icon_message_critical or wl_estra2-opcoes is INITIAL . " 14.05.2025 - RAMON
            e_msg = 'Solicitação totalmente liberada'.
            flag_undo = 'N'.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF flag_undo = 'S'.
          DELETE  FROM zsdt0387
              WHERE doc_simulacao  = wl_estra-doc_simulacao
              AND   nivel      = wl_estra-nivel
              AND   aprovador  = wl_estra-aprovador.
          wl_estra-estado = icon_led_yellow  .
          wl_estra-opcoes = icon_set_state.
          MODIFY tg_estra FROM wl_estra INDEX e_row_id.
        ELSE.
          e_msg = 'Devem ser reiniciadas as estratégias posteriores'.
        ENDIF.
      ELSEIF wl_estra-opcoes = icon_reject.
        UPDATE zsdt0381 SET status = '03' "149
              WHERE doc_simulacao = wl_estra-doc_simulacao.
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

*    IF lv_cnt_aprov = lv_cnt_todos.
*      EXIT.
*    ENDIF.

  ENDLOOP.

  CLEAR: ws_zsd_sim_checklist_est.

ENDFUNCTION.
