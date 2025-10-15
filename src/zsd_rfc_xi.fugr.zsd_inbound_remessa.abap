*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: Maggi                                                   *
* Autor......: Daniela Machado                                         *
* Data.......: 14.07.2010                                              *
* Descrição  : Capturar dados do sistema OPUS para carregar a tabela   *
*              ZSDT0001.                                               *
* Projeto....: Maggi - Projeto Evoluir                                 *
* Cód Espec..: GAP_SD03                                                *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:  .                                                      *
*----------------------------------------------------------------------*

FUNCTION zsd_inbound_remessa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IB_ROMANEIO TYPE  ZSDT001 OPTIONAL
*"----------------------------------------------------------------------
  INCLUDE zafl_macros.
**  ***initialize logger. It should be always on the top of the FUNCTION.
  /afl/log_init.

  REFRESH: it_log,
           it_zsdt0001.

  DATA: tl_zsdt0001      TYPE TABLE OF zsdt0001,
        wa_zsdt0001_aux  TYPE zsdt0001,
        tl_zsdt000x      TYPE TABLE OF zsdt0001,
        vl_index         TYPE syindex,
        vl_texto         TYPE char100,
        vl_docnum_aquav  TYPE zsdt0001-docnum_aquav,
        vl_ct_aquav      TYPE zsdt0001-ct_aquav,
        vl_st_cct        TYPE zsdt0001-st_cct,
        "VL_ID_CARGA      TYPE ZSDT0001-ID_CARGA,
        wl_zlest0142_del TYPE zlest0142,
        wl_zlest0104     TYPE zlest0104.

  DATA: zcl_cct_control_nf TYPE REF TO zcl_cct_control_nf.

  DATA: wa_ib_romaneio TYPE zsds001,
        wa_zsdt0001    TYPE zsdt0001.

* Preenche a estrutura da tabela ZSDT0001 com os valores inseridos
* através da tavela IB_ROMANEIO
  "--MOVE  TO .
  "TL_IB_ROMANEIO[] = IB_ROMANEIO[].
  REFRESH it_zsdt0001.

  LOOP AT ib_romaneio INTO wa_ib_romaneio.
    CLEAR wa_zsdt0001.
    MOVE-CORRESPONDING  wa_ib_romaneio TO wa_zsdt0001.

    IF wa_zsdt0001-id_ordem IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_zsdt0001od)
        FROM zsdt0001od
       WHERE id_ordem EQ @wa_zsdt0001-id_ordem.

      IF sy-subrc IS INITIAL.
        wa_zsdt0001-placa_cav  = wa_zsdt0001od-ds_placa_trator.
        wa_zsdt0001-placa_car1 = wa_zsdt0001od-ds_placa_reboq_1.
        wa_zsdt0001-placa_car2 = wa_zsdt0001od-ds_placa_reboq_2.
        wa_zsdt0001-placa_car3 = wa_zsdt0001od-ds_placa_reboq_3.
      ENDIF.

    ENDIF.

    "Atribuir alguns dados que não podem ser apagados pela interface
    SELECT SINGLE * INTO @DATA(wa_zsdt0001_exists)
      FROM zsdt0001
     WHERE ch_referencia EQ @wa_zsdt0001-ch_referencia.

    IF sy-subrc EQ 0.
      wa_zsdt0001-dt_criacao            = wa_zsdt0001_exists-dt_criacao.
      wa_zsdt0001-hr_criacao            = wa_zsdt0001_exists-hr_criacao.
      wa_zsdt0001-dt_atualizacao        = wa_zsdt0001_exists-dt_atualizacao.
      wa_zsdt0001-hr_atualizacao        = wa_zsdt0001_exists-hr_atualizacao.
      wa_zsdt0001-fat_contingencia_ecc  = wa_zsdt0001_exists-fat_contingencia_ecc.
    ENDIF.
    "Fim atribuição

    IF wa_zsdt0001-dt_criacao IS INITIAL AND
       wa_zsdt0001-hr_criacao IS INITIAL.
      wa_zsdt0001-dt_criacao = sy-datum.
      wa_zsdt0001-hr_criacao = sy-uzeit.

      wa_zsdt0001-dt_atualizacao = sy-datum.
      wa_zsdt0001-hr_atualizacao = sy-uzeit.
    ELSE.
      wa_zsdt0001-dt_atualizacao = sy-datum.
      wa_zsdt0001-hr_atualizacao = sy-uzeit.
    ENDIF.

    "Issue 156798 - Tratativa para nao receber duplicidade Romaneio - WPP -->>>

    "Caso seja um range da ZMM0127 não é para aceitar, registro gerado somente por WebService
    IF wa_zsdt0001-nr_romaneio GE 000700000 AND
       wa_zsdt0001-nr_romaneio LE 000799999 .
      CONTINUE.
    ENDIF.

    DATA(r_msg_error) = zcl_int_ib_carga_opus=>check_duplicidade_recebimento( i_zsdt0001 = wa_zsdt0001 ).
    IF r_msg_error IS NOT INITIAL.
      wa_log-obj_key        = wa_zsdt0001-ch_referencia.
      wa_log-interface      = c_14.
      wa_log-dt_atualizacao = sy-datum.
      wa_log-hr_atualizacao = sy-uzeit.
      wa_log-id             = c_sd.
      wa_log-num            = c_897.
      wa_log-type           = c_e.
      wa_log-message        = r_msg_error.
      APPEND wa_log TO it_log.
      CONTINUE. "Não receber os dados desse romaneio
    ENDIF.
    "Issue 156798 - Tratativa para nao receber duplicidade Romaneio - WPP <<---

**>>>>>BUG SOLTO 170899 / AOENNING / 20-03-2025
    r_msg_error = zcl_int_ib_carga_opus=>check_dados_ov_ped_romaneio( i_zsdt0001 = wa_zsdt0001 ).
    IF r_msg_error IS NOT INITIAL.
      wa_log-obj_key        = wa_zsdt0001-ch_referencia.
      wa_log-interface      = c_14.
      wa_log-dt_atualizacao = sy-datum.
      wa_log-hr_atualizacao = sy-uzeit.
      wa_log-id             = c_sd.
      wa_log-num            = c_897.
      wa_log-type           = c_e.
      wa_log-message        = r_msg_error.
      APPEND wa_log TO it_log.
      CONTINUE. "Não receber os dados desse romaneio
    ENDIF.
**>>>>>bug solto 170899 / aoenning / 20-03-2025

    APPEND wa_zsdt0001 TO it_zsdt0001.
  ENDLOOP.

  "LES - Duplicidade Romaneio IR260948 - WPP - Ini
  SORT it_zsdt0001 BY bukrs branch nr_safra nr_romaneio tp_movimento id_interface.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0001 COMPARING bukrs branch nr_safra nr_romaneio tp_movimento id_interface.
  "LES - Duplicidade Romaneio IR260948 - WPP - Fim

  IF NOT it_zsdt0001[] IS INITIAL.

*  Verifica se já Existe Romaneio com Remessa Emitida
    SELECT *
      FROM zsdt0001
      INTO TABLE tl_zsdt0001
       FOR ALL ENTRIES IN it_zsdt0001
     WHERE ch_referencia EQ it_zsdt0001-ch_referencia
       AND status        NE space.

    SORT tl_zsdt0001 BY ch_referencia ASCENDING.

*    IF sy-subrc IS INITIAL.
    LOOP AT it_zsdt0001 INTO wa_zsdt0001.

      vl_index = sy-tabix.

      CLEAR: vl_docnum_aquav, vl_ct_aquav, vl_st_cct. ", VL_ID_CARGA.

      SELECT SINGLE docnum_aquav ct_aquav st_cct
                    "ID_CARGA
             INTO (vl_docnum_aquav,vl_ct_aquav, vl_st_cct
                   ", VL_ID_CARGA
                   )
        FROM zsdt0001
       WHERE ch_referencia = wa_zsdt0001-ch_referencia.

*      IF SY-SUBRC IS NOT INITIAL.
*        "Se Encontrar um Romaneio Temporário da ZMM0127 o Mesmo deve vir por WebService
*        SELECT SINGLE ID_CARGA INTO (VL_ID_CARGA)
*          FROM ZSDT0001
*         WHERE CH_REFERENCIA NE WA_ZSDT0001-CH_REFERENCIA
*           AND TP_MOVIMENTO  EQ WA_ZSDT0001-TP_MOVIMENTO
*           AND NR_ROMANEIO   EQ WA_ZSDT0001-NR_ROMANEIO
*           AND NR_SAFRA      EQ WA_ZSDT0001-NR_SAFRA
*           AND BUKRS         EQ WA_ZSDT0001-BUKRS
*           AND BRANCH        EQ WA_ZSDT0001-BRANCH
*           AND ID_CARGA      NE SPACE.
*      ENDIF.

      "Caso seja um range da ZMM0127 não é para aceitar, registro gerado somente por WebService
      IF wa_zsdt0001-nr_romaneio GE 000700000 AND
         wa_zsdt0001-nr_romaneio LE 000799999 .
        DELETE it_zsdt0001 INDEX vl_index.
        CONTINUE.
      ENDIF.

      "se ja tiver o VL_DOCNUM_AQUAV enão não pode mais atualizar.
      IF vl_docnum_aquav  <> '0000000000'  OR vl_ct_aquav = 'X' OR vl_st_cct = '02'. " OR VL_ID_CARGA IS NOT INITIAL.
        DELETE it_zsdt0001 INDEX vl_index.
        CONTINUE.
      ENDIF.

*     Preenche valores para exibição de mensagens
      wa_log-obj_key        = wa_zsdt0001-ch_referencia.
      wa_log-interface      = c_14.
      wa_log-dt_atualizacao = sy-datum.
      wa_log-hr_atualizacao = sy-uzeit.
      wa_log-id             = c_sd.
      wa_log-num            = c_897.
      "Verifica na tabela de romaneios usados se o romaneio do teste consta nessa tabela
      READ TABLE tl_zsdt0001 INTO wa_zsdt0001_aux WITH KEY ch_referencia = wa_zsdt0001-ch_referencia BINARY SEARCH.

      "Se não encontrou o romaneio na tabela de romaneio usado então atualizar dos dados ou excluir quando
      "for cancelamento do OPUS
      IF NOT sy-subrc IS INITIAL.
        wa_log-type = c_s.
        IF wa_zsdt0001-tp_movimento EQ 'D'.

          FREE zcl_cct_control_nf.
          CREATE OBJECT zcl_cct_control_nf.
          DATA(_atrib_nf) = zcl_cct_control_nf->atribuir_nf_rom( EXPORTING i_ch_referencia = wa_zsdt0001-ch_referencia
                                                                 IMPORTING e_nota_fiscal   = wl_zlest0142_del ).
          IF ( _atrib_nf EQ abap_true ) AND ( wl_zlest0142_del IS NOT INITIAL  ).
            zcl_cct_control_nf->remover_nf_cct( i_zlest0142 = wl_zlest0142_del ).
          ENDIF.

          DELETE it_zsdt0001 INDEX vl_index.
          DELETE FROM zsdt0001 WHERE ch_referencia = wa_zsdt0001-ch_referencia.

          CONCATENATE TEXT-s01 wa_zsdt0001-ch_referencia TEXT-s03 INTO wa_log-message SEPARATED BY space.
        ELSE.
*       text-s01 -> Chave de referência
*       text-s02 -> gravada com sucesso na tabela ZSDT0001.
          CONCATENATE TEXT-s01 wa_zsdt0001-ch_referencia TEXT-s02 INTO wa_log-message SEPARATED BY space.
        ENDIF.
      ELSE.
        DELETE it_zsdt0001 INDEX vl_index.
        wa_log-type = c_e.
*       text-s01 -> Chave de referência
*       text-e03 -> com remessa emitida, não pode ser alterado.
*       text-e02 ->Romaneio
        CONCATENATE wa_zsdt0001-ch_referencia TEXT-e02 INTO vl_texto SEPARATED BY space.
        CONCATENATE TEXT-s01 vl_texto TEXT-e03 INTO wa_log-message SEPARATED BY space.

        IF wa_zsdt0001_aux-id_referencia IS INITIAL AND wa_zsdt0001-id_referencia IS NOT INITIAL.
          UPDATE zsdt0001
             SET id_referencia = wa_zsdt0001-id_referencia
           WHERE ch_referencia = wa_zsdt0001-ch_referencia.
        ENDIF.

        IF wa_zsdt0001_aux-ch_refer_ent IS INITIAL AND wa_zsdt0001-ch_refer_ent IS NOT INITIAL.
          UPDATE zsdt0001
             SET ch_refer_ent = wa_zsdt0001-ch_refer_ent
           WHERE ch_referencia = wa_zsdt0001-ch_referencia.
        ENDIF.

      ENDIF.

      APPEND wa_log TO it_log.

      CLEAR: wa_log, vl_texto.
    ENDLOOP.
*  ELSE.
*    wa_log-type           = c_e.
**     text-e01 -> Erro ao gravar dados na tabela ZSDT0001.
*    wa_log-message = text-e01.
*    APPEND wa_log TO it_log.
*    CLEAR wa_log.
*    ROLLBACK WORK.
*  ENDIF.
  ENDIF.

  IF NOT it_zsdt0001[] IS INITIAL.
*   Atualiza dados na tabela ZSDT0001
    LOOP AT it_zsdt0001 INTO wa_zsdt0001.

      "Verifica se Romaneio será utilizado na ZLES0077(Aquaviario)
      SELECT SINGLE *
        FROM zlest0104 INTO wl_zlest0104
       WHERE local_descarga = wa_zsdt0001-local_descarga
         AND bukrs          = wa_zsdt0001-bukrs
         AND branch         = wa_zsdt0001-branch.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
          EXPORTING
            i_bukrs        = wa_zsdt0001-bukrs
            i_branch       = wa_zsdt0001-branch
            i_lifnr        = wa_zsdt0001-parid
            i_peso         = wa_zsdt0001-peso_subtotal
          IMPORTING
            e_peso_retido  = wa_zsdt0001-peso_retido_est
            e_peso_liquido = wa_zsdt0001-peso_liqret_est.
      ENDIF.

      MODIFY zsdt0001 FROM wa_zsdt0001.

      FREE zcl_cct_control_nf.
      CREATE OBJECT zcl_cct_control_nf.
      DATA(_atrib) = zcl_cct_control_nf->atribuir_nf_rom( i_ch_referencia = wa_zsdt0001-ch_referencia ).
      IF _atrib EQ abap_true.
        zcl_cct_control_nf->disp_nf_cct( ).
      ENDIF.

    ENDLOOP.
  ENDIF.

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados na tabela ZSDT0001
  IF NOT it_log[] IS INITIAL.
*--> 26.09.2023 17:57:57 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = it_log[].

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
          outreturn = it_log[].
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = it_log[].
    ENDIF.
*<-- 26.09.2023 17:57:57 - Migração S4 – ML – Fim

  ENDIF.

  COMMIT WORK.

**save logs. It should be always on the bottom of the FUNCTION.
  /afl/save.

ENDFUNCTION.
