FUNCTION z_ov_popup_liberacao_saldo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_SIMU) TYPE  ZSDED003
*"     REFERENCE(IV_VBELN) TYPE  VBELN_VA
*"     REFERENCE(IV_TPSIM) TYPE  CHAR02
*"     REFERENCE(IV_WAERK) TYPE  WAERK
*"     REFERENCE(IV_KWERT) TYPE  NETWR_AK
*"     REFERENCE(IV_KWERT_M) TYPE  NETWR_AK
*"     REFERENCE(IV_KURSF) TYPE  KURSF
*"     REFERENCE(IV_TESTE) TYPE  FLAG OPTIONAL
*"     REFERENCE(IV_SEM_TELA) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_ZSDT0116) TYPE  ZSDS_ZSDT0116_TAB
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"     REFERENCE(ET_MESSAGE) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA lv_vlr_wf TYPE netwr_ap.
  DATA lv_msgx TYPE string.
  DATA lv_just_wf TYPE zsde_just_wf.
  DATA lv_ret.

  PERFORM f_refresh_all.

  zsds_tela_calculo_aprovacao-vbeln = iv_vbeln.
  zsds_tela_calculo_aprovacao-waerk = 'USD'.
  zsds_tela_calculo_aprovacao-doc_simulacao = iv_simu.
  zsds_tela_calculo_aprovacao-vlr_ttl_lib = iv_kwert.

  PERFORM f_busca_vlr_fin USING iv_simu CHANGING zsds_tela_calculo_aprovacao-vlr_ttl_fn.

  IF iv_tpsim = 'VP'. " 08.08.2025 - Buscar saldo somente para venda a prazo

    PERFORM f_preenche_saldos USING iv_vbeln iv_simu.

  ENDIF.

  PERFORM f_atualiza_campos.

  PERFORM f_atualiza_vlr_fin.

  " 08.08.2025 - Verificar se tem saldo financeiro,
  " não pode enviar nada para o WF, quando nao for VP -->
  SELECT COUNT(*) FROM tvarvc WHERE name = 'ZSDT0100_TPSIM' AND low = iv_tpsim.
*  IF iv_tpsim <> 'VP' AND zsds_tela_calculo_aprovacao-vlr_ttl_wf > 0.
  IF sy-subrc IS NOT INITIAL AND zsds_tela_calculo_aprovacao-vlr_ttl_wf > 0.
    ev_erro = abap_true.

    lv_msgx = `Venda sem Recebimento Financeiro, e a condição de pagamento não permite o envio para workflow.`
      && ` Em caso de dúvida, entrar em contato com a equipe do insumos corporativo.`.

    PERFORM f_mensagem_insere_txt TABLES et_message USING 'E' lv_msgx.

    EXIT.

  ENDIF.
  " 08.08.2025 <--

  CALL SCREEN 9000 STARTING AT 25 1.

  IF gv_ucomm = 'CANCEL'.
    ev_erro = abap_true.
    EXIT.
  ENDIF.

  IF zsds_tela_calculo_aprovacao-vlr_ttl_wf > 0.
    PERFORM f_get_text USING 'Justificativa' CHANGING lv_just_wf.
  ENDIF.

  LOOP AT gt_saldos ASSIGNING FIELD-SYMBOL(<fs_saldos>).

    IF <fs_saldos>-saldo_usado > 0.

      PERFORM f_cria_nova_linha
        USING iv_vbeln
              iv_teste
              'A' "<---- aprovado
              <fs_saldos>-origem
              iv_waerk
              iv_kursf
              <fs_saldos>-saldo_usado
              <fs_saldos>-id_sap
              ''
     CHANGING et_zsdt0116 et_message.

    ENDIF.

  ENDLOOP.

  IF zsds_tela_calculo_aprovacao-vlr_ttl_lib_fn > 0.

    PERFORM f_cria_nova_linha
      USING iv_vbeln
            iv_teste
            'A' "<---- aprovado
            'F' "<-- Liberação saldo financeiro
            iv_waerk
            iv_kursf
            zsds_tela_calculo_aprovacao-vlr_ttl_lib_fn
            '0'
            lv_just_wf
   CHANGING et_zsdt0116 et_message.

  ENDIF.

  IF zsds_tela_calculo_aprovacao-vlr_ttl_wf > 0.

    PERFORM f_cria_nova_linha
      USING iv_vbeln
            iv_teste
            'L' "<---- Liberado para o WF
            'Z' "<-- Liberação pela ZSDT0117
            iv_waerk
            iv_kursf
            zsds_tela_calculo_aprovacao-vlr_ttl_wf
            '0'
            lv_just_wf
   CHANGING et_zsdt0116 et_message.

  ENDIF.

ENDFUNCTION.
