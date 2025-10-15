FUNCTION z_estrategia_executar_isen_jur.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     VALUE(E_MSG) TYPE  CHAR50
*"     VALUE(E_OK) TYPE  CHAR01
*"  TABLES
*"      T_ORDENS STRUCTURE  ZSD_ORDENS
*"      T_ESTRA STRUCTURE  ZSD_ESTRATEGIA_OV
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: icon.

  TYPES:  BEGIN OF ty_estra.
            INCLUDE TYPE zsd_estrategia_ov.
  TYPES:  END OF ty_estra.

  TYPES: BEGIN OF ty_cad_ordem,
           empresa     TYPE  char30,
           vbeln       TYPE  vbeln,
           usuario     TYPE  usnam,
           netwr       TYPE  netwr,
           data        TYPE  erdat,
           waerk       TYPE  waerk,
           filial      TYPE  werks_d,
           netwr_usd   TYPE  netwr, "146630 - RGA
           cd_sol_isen TYPE zed_cod_solict_isencao, "146630 - RGA
         END OF ty_cad_ordem.

  DATA: tg_estra     TYPE TABLE OF ty_estra,
        wg_cad_ordem TYPE ty_cad_ordem.

  DATA: wl_estra       LIKE zsd_estrategia_ov,
        wl_input_estra TYPE zsdt0337,
        flag_undo(1),
        linha_estra    TYPE sy-tabix,
        ult_linha      TYPE sy-tabix,
        e_row_id       TYPE sy-tabix,
        lt_ordens      TYPE TABLE OF zsd_ord_vendas_est_isen_juros.


  DATA wa_ordens LIKE LINE OF t_ordens.

  e_ok = abap_false.


  LOOP AT t_estra.
    CLEAR: wl_estra.
    MOVE-CORRESPONDING t_estra TO wl_estra.
    APPEND wl_estra TO tg_estra.
  ENDLOOP.

  SORT tg_estra BY nivel aprovador.
  SORT t_estra  BY nivel aprovador.

  LOOP AT tg_estra INTO wl_estra WHERE aprovador = i_usuario.

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

    READ TABLE t_ordens INTO wa_ordens INDEX 1.

    wg_cad_ordem-empresa  = wa_ordens-empresa.
    wg_cad_ordem-vbeln    = wa_ordens-ov_principal.
    wg_cad_ordem-usuario  = i_usuario.
    wg_cad_ordem-netwr    = wa_ordens-netwr.
    wg_cad_ordem-filial   = wa_ordens-escvenda.
    wg_cad_ordem-cd_sol_isen   = wa_ordens-cd_sol_isen."SMC-24-06-2025


    IF ( wl_estra-opcoes NE icon_set_state   ) AND
       ( wl_estra-opcoes NE icon_system_undo ) AND
       ( wl_estra-opcoes NE icon_reject      ).
      e_msg =  'Opção inválida para processamento!'.
      EXIT.
    ENDIF.

    IF i_usuario NE wl_estra-aprovador.
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

        wl_input_estra-mandt             = sy-mandt.
        wl_input_estra-bukrs             = wl_estra-bukrs.
        wl_input_estra-vbeln             = wl_estra-vbeln.
        wl_input_estra-nivel             = wl_estra-nivel.
        wl_input_estra-aprovador         = wl_estra-aprovador.
        wl_input_estra-cod_solict_isencao   = wa_ordens-cd_sol_isen."146630 - SMC
        wl_input_estra-valor_moeda_doc   = wa_ordens-netwr_usd. "146630 - SMC
        wl_input_estra-valor_brl         = wa_ordens-netwr. "146630 - RGA
        wl_input_estra-moeda_doc         = wa_ordens-moeda. "146630 - SMC
        wl_input_estra-valor_de          = wl_estra-valor_de.
        wl_input_estra-valor_ate         = wl_estra-valor_ate.
        wl_input_estra-data_atual        = sy-datum.
        wl_input_estra-hora_atual        = sy-uzeit.
        wl_input_estra-usuario           = sy-uname.
        wl_input_estra-status_apr        = '3'. "146630 - RGA

        MODIFY zsdt0337 FROM wl_input_estra.
        COMMIT WORK.

        IF ult_linha = linha_estra.

          UPDATE zfit186
             SET status_solicit  = '4' "146630 - RGA
*                 usuario_solicit = i_usuario "189279 - SMC
*                 data            = sy-datum "189279 - SMC
*                 hora            = sy-uzeit "189279 - SMC
           WHERE ov_principal  = wl_estra-vbeln
           AND cd_sol_isen = wl_input_estra-cod_solict_isencao. "146630 - SMC

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
          PERFORM envia_email(zsdr0162) TABLES tg_estra USING wg_cad_ordem e_row_id .
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

        DELETE FROM zsdt0337
         WHERE bukrs      = wl_estra-bukrs
           AND vbeln      = wl_estra-vbeln
           AND nivel      = wl_estra-nivel
           AND aprovador  = wl_estra-aprovador.

        wl_estra-estado = icon_led_yellow  .
        wl_estra-opcoes = icon_set_state.
        MODIFY tg_estra FROM wl_estra INDEX e_row_id.
      ELSE.
        e_msg = 'Devem ser reiniciadas as estratégias posteriores'.
      ENDIF.

    ELSEIF wl_estra-opcoes = icon_reject. " Rejeitar

      CLEAR wl_input_estra.

      wl_input_estra-mandt       = sy-mandt.
      wl_input_estra-bukrs       = wl_estra-bukrs.
      wl_input_estra-vbeln       = wl_estra-vbeln.
      wl_input_estra-nivel       = wl_estra-nivel.
      wl_input_estra-aprovador   = wl_estra-aprovador.

*--------Inicio #140708 APP FIORI - Aprovação de isenção de juros ZFIS66 / PANF
      wl_input_estra-cod_solict_isencao   = wa_ordens-cd_sol_isen.
      wl_input_estra-valor_moeda_doc   = wa_ordens-netwr_usd. "146630 - RGA
      wl_input_estra-valor_brl   = wa_ordens-netwr. "146630 - SMC
      wl_input_estra-moeda_doc   = wa_ordens-moeda. "146630 - SMC
*--------Fim #140708 APP FIORI - Aprovação de isenção de juros ZFIS66 / PANF

      wl_input_estra-valor_de    = wl_estra-valor_de.
      wl_input_estra-valor_ate   = wl_estra-valor_ate.
      wl_input_estra-data_atual  = sy-datum.
      wl_input_estra-hora_atual  = sy-uzeit.
      wl_input_estra-usuario     = sy-uname.
      wl_input_estra-status_apr  = '4'. "146630 - CS2024000604  - RGA
      MODIFY zsdt0337 FROM wl_input_estra.
      COMMIT WORK.

      UPDATE zfit186
         SET status_solicit  = '5' "146630 - CS2024000604  - RGA
*             usuario_solicit = i_usuario "189279 - SMC
*             data            = sy-datum "189279 - SMC
*             hora            = sy-uzeit "189279 - SMC
       WHERE ov_principal  = wl_estra-vbeln
       AND cd_sol_isen = wl_input_estra-cod_solict_isencao. "146630 - SMC.

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
