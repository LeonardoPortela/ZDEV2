FUNCTION z_ov_liberacao_embarque .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_SIMU) TYPE  ZSDED003
*"     REFERENCE(IV_BUKRS) TYPE  BUKRS
*"     REFERENCE(IV_TPSIM) TYPE  CHAR2 DEFAULT 'VP'
*"     REFERENCE(IV_WAERK) TYPE  WAERK
*"     REFERENCE(IV_KURSF) TYPE  KURSF
*"     REFERENCE(IV_VKBUR) TYPE  VKBUR OPTIONAL
*"     REFERENCE(IV_VBELN) TYPE  VBELN_VA
*"     REFERENCE(IV_KWERT) TYPE  NETWR_AK
*"     REFERENCE(IV_KWERT_M) TYPE  NETWR_AK
*"     REFERENCE(IV_TESTE) TYPE  FLAG OPTIONAL
*"     REFERENCE(IV_POPUP) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_ZSDT0116) TYPE  ZSDS_ZSDT0116_TAB
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"     REFERENCE(ET_MESSAGE) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA lv_msgx TYPE c LENGTH 100.
  DATA lv_saldo TYPE netwr_ak.
  DATA lv_ret.
  DATA lv_sem_sap.
  DATA lv_erro_checkl.

  CHECK iv_simu IS NOT INITIAL AND iv_vbeln IS NOT INITIAL.

  IF iv_vkbur <> '0101' AND iv_bukrs = '0001'.

    SELECT COUNT(*) FROM tvarvc WHERE name = 'ZSDT0100_TPSIM' AND low = iv_tpsim.

    " valida se existir na stvarv
*    IF sy-dbcnt > 0.
    IF iv_tpsim EQ 'VP'.

      PERFORM f_existe_checklist
        USING iv_simu
              iv_vbeln
       CHANGING lv_erro_checkl.

      CASE lv_erro_checkl.
        WHEN '1'. "------------------------------------------ 1 - sem checklist
          lv_msgx = `Simulador ` && iv_simu && ` não possui checklist ativo`.
          et_message = VALUE bapiret2_tab(  ( type = 'E' id = 'DS' number = '016' message = lv_msgx ) ).
          ev_erro = abap_true.
          EXIT.
        WHEN '2'. "------------------------------------------ 2 - com checklist, mas com inconformidade
          lv_msgx = `Checklist com inconformidade, aguardando aprovação`.
          et_message = VALUE bapiret2_tab(  ( type = 'E' id = 'DS' number = '016' message = lv_msgx ) ).
          ev_erro = abap_true.
*      PERFORM f_popup_to_confirm USING `Checklist com inconformidade, visualizar?` CHANGING lv_ret.
*
*      IF lv_ret = '1'.
*        CALL FUNCTION 'ZSDMF_TELA_RESPONDER_CHECKLIST'
*          EXPORTING
*            iv_simulador     = iv_simu
*            iv_edit          = abap_true
*            iv_mostra_inconf = abap_true.
*      ENDIF.

          EXIT.

        WHEN OTHERS." space - com checklist e ok
      ENDCASE.

    ENDIF.

    " com popup mostra opções para o usuário
    IF iv_popup = abap_true.

      PERFORM f_com_popup
        USING iv_simu
              iv_vbeln
              iv_tpsim
              iv_kwert
              iv_kwert_m
              iv_waerk
              iv_kursf
              iv_teste
     CHANGING ev_erro
              et_zsdt0116[]
              et_message[].

      " sem popup, faz divisão automatico - ( essa opção nao esta no escopo, mas se precisar está disponivel )
    ELSE.

      PERFORM f_sem_popup
        USING iv_simu
              iv_vbeln
              iv_waerk
              iv_kwert
              iv_kwert_m
              iv_kursf
              iv_teste
     CHANGING ev_erro
              et_zsdt0116[]
              et_message[].

    ENDIF.

  ELSE.

    lv_msgx = |Para o escritório { iv_vkbur } será aprovado automaticamente, confirma ?|.

    PERFORM f_popup_to_confirm USING lv_msgx CHANGING lv_ret.

    IF lv_ret <> '1'.
      ev_erro = abap_true.
      EXIT.
    ENDIF.

    PERFORM f_cria_nova_linha
       USING iv_vbeln
             iv_teste
             'A' "<- Aprova automatico
             'Z' "<- Origem Z, porém Aprovado automaticamente
             iv_waerk
             iv_kursf
             iv_kwert
             '0'
             ''
      CHANGING et_zsdt0116[] et_message[].

  ENDIF.

  IF iv_waerk <> 'USD'.

    PERFORM f_fix_round
      USING iv_kwert_m
   CHANGING et_zsdt0116 .

  ENDIF.

ENDFUNCTION.
