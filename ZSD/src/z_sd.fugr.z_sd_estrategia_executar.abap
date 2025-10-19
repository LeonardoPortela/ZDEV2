FUNCTION z_sd_estrategia_executar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"     VALUE(OK) TYPE  CHAR01
*"  TABLES
*"      T_ORDENS STRUCTURE  ZSD_ROMA_IMP
*"      T_ESTRA STRUCTURE  ZSD_ESTRATEGIA_SD
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: icon.

  TYPES:
    BEGIN OF ty_estra.
      INCLUDE STRUCTURE zsd_estrategia_sd.
      TYPES:  mark TYPE c,
    END OF ty_estra.

  DATA tg_ordens TYPE TABLE OF zsd_roma_imp WITH HEADER LINE.


*********************************************************************************************
* Variáveis
*********************************************************************************************
  DATA: xtotal       TYPE zsdt0151-total,
        vvalor_ate   TYPE zsdt0152-valor_ate,
        vflag(1),
        vflg_ico(1),
        flag_undo(1),
        e_row_id     TYPE sy-tabix,
        linha_estra  TYPE sy-tabix,
        ult_linha    TYPE sy-tabix.

*********************************************************************************************
* Tabelas / Workarea
*********************************************************************************************
  DATA: it_zsdt0151 TYPE TABLE OF zsdt0151,
        it_zsdt0152 TYPE TABLE OF zsdt0152,
        it_zsdt0153 TYPE TABLE OF zsdt0153,
        it_t001w    TYPE TABLE OF t001w,
        it_estra    TYPE TABLE OF ty_estra.

  DATA: wa_zsdt0151    TYPE zsdt0151,
        wa_zsdt0152    TYPE zsdt0152,
        wa_zsdt0153    TYPE zsdt0153,
        wa_t001w       TYPE t001w,
        wa_zsdt0001    TYPE zsdt0001,
        wa_estra       TYPE ty_estra,
        w_estra        TYPE zsd_estrategia_sd,
        wl_input_estra TYPE zsdt0153.

  DATA:  tg_estra    TYPE TABLE OF ty_estra.
  DATA: wl_estra  LIKE LINE OF tg_estra,
        wl_estra2 LIKE LINE OF tg_estra.
  DATA   wg_ordens   TYPE zsd_roma_imp.

  ok = abap_false.

  LOOP AT t_estra.
    MOVE-CORRESPONDING t_estra TO wl_estra.
    APPEND wl_estra TO tg_estra.
  ENDLOOP.

  SORT tg_estra BY nivel aprovador.
  SORT t_estra BY nivel aprovador.

  LOOP AT tg_estra INTO wl_estra WHERE aprovador = v_usuario.                 "

    IF e_row_id IS NOT INITIAL AND sy-tabix NE e_row_id + 1.                "
      EXIT.                                                                 "
    ELSEIF e_row_id IS NOT INITIAL AND sy-tabix EQ e_row_id + 1.            "
      IF wl_estra-opcoes NE icon_system_undo.                               "
        MOVE icon_set_state TO wl_estra-opcoes.                             "
      ENDIF.
      e_row_id = sy-tabix.                                                  "
    ELSE.
      e_row_id = sy-tabix.
    ENDIF.
    READ TABLE t_ordens INDEX 1.
    wg_ordens-empresa  = t_ordens-empresa.
    CONCATENATE  t_ordens-vbeln  '-'  INTO wg_ordens-vbeln.
*    WG_ORDENS-USUARIO  = V_USUARIO .
    wg_ordens-total     = t_ordens-total.
    wg_ordens-total_est = t_ordens-total_est.

    "
    IF wl_estra-opcoes NE icon_set_state  AND
      wl_estra-opcoes NE icon_system_undo AND
      wl_estra-opcoes NE icon_cancel      AND
      wl_estra-opcoes NE icon_led_red     AND
      wl_estra-opcoes NE icon_erase       AND
      wl_estra-opcoes NE icon_reject .
      msg =  'Opção inválida para processamento!'.
      EXIT.
    ENDIF.

    IF v_usuario NE wl_estra-aprovador.
      msg =  'Usuário não é o aprovador deste nível!'.
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
        REPLACE ALL OCCURRENCES OF        '.'     IN wl_estra-valor_de WITH ' ' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF        ','     IN wl_estra-valor_de WITH '.' IGNORING CASE.
        CONDENSE wl_estra-valor_de NO-GAPS.
        "
        REPLACE ALL OCCURRENCES OF        '.'     IN wl_estra-valor_ate WITH ' ' IGNORING CASE.
        REPLACE ALL OCCURRENCES OF        ','     IN wl_estra-valor_ate WITH '.' IGNORING CASE.
        CONDENSE wl_estra-valor_ate NO-GAPS.

        wl_input_estra-mandt          = sy-mandt.
        wl_input_estra-lote           = wl_estra-lote.
*        WL_INPUT_ESTRA-VKORG          = WG_ORDENS-VKORG.
        wl_input_estra-werks          = wl_estra-werks.
        wl_input_estra-vbeln          = wl_estra-vbeln.
        wl_input_estra-ch_referencia  = wl_estra-ch_referencia.
        wl_input_estra-nr_romaneio    = wl_estra-nr_romaneio.
        wl_input_estra-nivel          = wl_estra-nivel.
        wl_input_estra-aprovador      = wl_estra-aprovador.
        wl_input_estra-valor_de       = wl_estra-valor_de.
        wl_input_estra-valor_ate      = wl_estra-valor_ate.
        wl_input_estra-waers          = wl_estra-waers.
        wl_input_estra-data_atual     = sy-datum.
        wl_input_estra-hora_atual     = sy-uzeit.
        wl_input_estra-usuario        = sy-uname.
        MODIFY zsdt0153 FROM wl_input_estra.
*        CLEAR WL_INPUT_ESTRA.
        IF ult_linha = linha_estra.
          IF wg_ordens-total_est GT wl_input_estra-valor_ate.
*            MSG = 'Estratégia incompleta, contacte o Helpdesk'.
            msg = 'Não encontrato proximo nível para aprovação, cadastrar'.
          ELSE.
            UPDATE zsdt0151 SET status = 'A'
            WHERE lote = wl_estra-lote.
            COMMIT WORK.
            LOOP AT tg_estra INTO wl_estra.
              wl_estra-opcoes = ' ' .
              wl_estra-estado = icon_checked .
              MODIFY tg_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
            ENDLOOP.
            msg = 'Processamento concluído com sucesso'.
            ok = abap_true.
          ENDIF.
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
          AND wl_estra2-aprovador NE wl_estra-aprovador.                        "/Modificação CS2016000820/
          flag_undo = 'N'.
        ENDIF.
        IF wl_estra2-opcoes = icon_message_critical.
          msg = 'Solicitação totalmente liberada'.
          flag_undo = 'N'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF flag_undo = 'S'.
        DELETE  FROM zsdt0153
            WHERE lote            = wl_estra-lote
            AND   nivel           = wl_estra-nivel
            AND   aprovador       = wl_estra-aprovador.
        wl_estra-estado = icon_led_yellow  .
        wl_estra-opcoes = icon_set_state.
        MODIFY tg_estra FROM wl_estra INDEX e_row_id.
      ELSE.
        msg = 'Devem ser reiniciadas as estratégias posteriores'.
      ENDIF.
    ELSEIF wl_estra-opcoes = icon_reject OR wl_estra-opcoes = icon_erase.
      UPDATE zsdt0151 SET status = 'R'
            WHERE lote           = wl_estra-lote.
      COMMIT WORK.
      msg = 'Processamento concluído com sucesso'.
      ok = abap_true.
    ELSEIF wl_estra-opcoes = icon_cancel OR wl_estra-opcoes = icon_led_red.
      DELETE FROM zsdt0151
            WHERE lote           = wl_estra-lote.
      COMMIT WORK.
      msg = 'Processamento concluído com sucesso'.
      ok = abap_true.
    ENDIF.
    LOOP AT tg_estra INTO wl_estra.
      IF e_row_id = sy-tabix.
        MOVE-CORRESPONDING wl_estra TO w_estra.
        MODIFY t_estra FROM w_estra INDEX sy-tabix TRANSPORTING opcoes estado.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDFUNCTION.
