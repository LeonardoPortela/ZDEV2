FUNCTION z_ov_estrategia_executar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     VALUE(E_MSG) TYPE  CHAR50
*"     VALUE(E_OK) TYPE  CHAR01
*"  TABLES
*"      T_ORDENS STRUCTURE  ZSD_ORD_VENDAS_EST
*"      T_ESTRA STRUCTURE  ZSD_ESTRATEGIA_OV
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: icon.

  TYPES:  BEGIN OF ty_estra.
            INCLUDE TYPE zsd_estrategia_ov.
  TYPES:  END OF ty_estra.

  TYPES:  BEGIN OF ty_cad_ordem.
            INCLUDE TYPE zsd_cad_ov_est.
  TYPES:  END OF ty_cad_ordem.

  DATA: tg_estra     TYPE TABLE OF ty_estra,
        wg_cad_ordem TYPE ty_cad_ordem.

  DATA: wl_estra       LIKE zsd_estrategia_ov,
        wl_input_estra TYPE zsdt0142,
        flag_undo(1),
        linha_estra    TYPE sy-tabix,
        ult_linha      TYPE sy-tabix,
        e_row_id       TYPE sy-tabix.

  DATA lv_usuario TYPE sy-uname.

  e_ok = abap_false.

  lv_usuario = i_usuario.

  LOOP AT t_estra.
    CLEAR: wl_estra.
    MOVE-CORRESPONDING t_estra TO wl_estra.
    APPEND wl_estra TO tg_estra.
  ENDLOOP.

  SORT tg_estra BY nivel aprovador.
  SORT t_estra  BY nivel aprovador.

  LOOP AT tg_estra INTO wl_estra WHERE aprovador = lv_usuario.

    "Buscar Index Estratégia Aprovação
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

    READ TABLE t_ordens INDEX 1.

    wg_cad_ordem-empresa  = t_ordens-empresa.
    wg_cad_ordem-vbeln    = t_ordens-vbeln.
    wg_cad_ordem-seq    = t_ordens-seq. " 17.07.2025 - RAMON - 174339
    wg_cad_ordem-usuario  = lv_usuario.
    wg_cad_ordem-netwr    = t_ordens-netwr.

    IF ( wl_estra-opcoes NE icon_set_state   ) AND
       ( wl_estra-opcoes NE icon_system_undo ) AND
       ( wl_estra-opcoes NE icon_reject      ).
      e_msg =  'Opção inválida para processamento!'.
      EXIT.
    ENDIF.

    IF lv_usuario NE wl_estra-aprovador.
      e_msg =  'Usuário não é o aprovador deste nível!'.
      EXIT.
    ENDIF.

    IF wl_estra-opcoes = icon_set_state. "Aprovar
      flag_undo = 'S'.

      linha_estra =  e_row_id.

      LOOP AT tg_estra INTO DATA(wl_estra_aux).

        ult_linha = sy-tabix. "Atribuindo ultimo indice da Estratégia

        "Verificar se aprovação anterior foi executada
        IF ( wl_estra_aux-opcoes = icon_set_state ) AND ( sy-tabix LT e_row_id ).
          flag_undo = 'N'.
        ENDIF.

      ENDLOOP.

      IF flag_undo = 'S'.

        CLEAR wl_input_estra.

        wl_input_estra-mandt       = sy-mandt.
        wl_input_estra-bukrs       = wl_estra-bukrs.
        wl_input_estra-vbeln       = wl_estra-vbeln.
        wl_input_estra-seq         = wl_estra-seq. "165578 - 15.07.2025 - RAMON -->
        wl_input_estra-nivel       = wl_estra-nivel.
        wl_input_estra-aprovador   = wl_estra-aprovador.
        wl_input_estra-valor_de    = wl_estra-valor_de.
        wl_input_estra-valor_ate   = wl_estra-valor_ate.

        " 23.07.2025 - RAMON - 183689 -->
        wl_input_estra-vlr_foto_acum = t_ordens-vlr_acumulado.
        " 23.07.2025 - RAMON - 183689 --<


        wl_input_estra-data_atual  = sy-datum.
        wl_input_estra-hora_atual  = sy-uzeit.
        wl_input_estra-usuario     = sy-uname.

        MODIFY zsdt0142 FROM wl_input_estra.

        IF ult_linha = linha_estra.

          UPDATE zsdt0116
             SET status_workflow = 'A'
                 user_apv        = lv_usuario
                 dt_apv          = sy-datum
                 hr_apv          = sy-uzeit
           WHERE vbeln  = wl_estra-vbeln
             AND seq = wl_estra-seq "165578 - 15.07.2025 - RAMON -->
             AND status = ''.

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
          PERFORM envia_email(zsdr0069) TABLES tg_estra USING wg_cad_ordem e_row_id .
          e_msg = 'Processamento concluído com sucesso'.
          e_ok = abap_true.
        ENDIF.
      ELSE.
        e_msg = 'Devem ser aprovadas as estratégias anteriores'.
      ENDIF.
    ELSEIF  wl_estra-opcoes = icon_system_undo . "Anular

      flag_undo = 'S'.
      linha_estra =  e_row_id.
      LOOP AT tg_estra INTO wl_estra_aux.
        IF ( wl_estra_aux-opcoes    EQ icon_system_undo   ) AND
           ( sy-tabix               GT e_row_id           ) AND
           ( wl_estra_aux-aprovador NE wl_estra-aprovador ).
          flag_undo = 'N'.
        ENDIF.
        IF wl_estra_aux-opcoes = icon_message_critical.
          e_msg = 'Solicitação totalmente liberada'.
          flag_undo = 'N'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF flag_undo = 'S'.

        DELETE FROM zsdt0142
         WHERE bukrs      = wl_estra-bukrs
           AND vbeln      = wl_estra-vbeln
           AND seq = wl_estra-seq " 17.07.2025 - RAMON - 174339
           AND nivel      = wl_estra-nivel
           AND aprovador  = wl_estra-aprovador.

        wl_estra-estado = icon_led_yellow  .
        wl_estra-opcoes = icon_set_state.
        MODIFY tg_estra FROM wl_estra INDEX e_row_id.
      ELSE.
        e_msg = 'Devem ser reiniciadas as estratégias posteriores'.
      ENDIF.

    ELSEIF wl_estra-opcoes = icon_reject. " Rejeitar

      UPDATE zsdt0116
         SET status_workflow = 'R'
             user_apv        = lv_usuario
             dt_apv          = sy-datum
             hr_apv          = sy-uzeit
       WHERE vbeln  = wl_estra-vbeln
         AND vbeln  = wl_estra-seq
         AND status = ''.

      COMMIT WORK.
      e_msg = 'Processamento concluído com sucesso'.
      e_ok = abap_true.

    ENDIF.

    LOOP AT tg_estra INTO wl_estra_aux.
      IF e_row_id = sy-tabix.
        CLEAR: wl_estra.
        MOVE-CORRESPONDING wl_estra_aux TO wl_estra.
        MODIFY t_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDFUNCTION.
