FUNCTION Z_OV_NOVA_LIBERACAO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_SIMU) TYPE  ZSDED003
*"     REFERENCE(IV_TPSIM) TYPE  CHAR2 DEFAULT 'VP'
*"     REFERENCE(IV_VKBUR) TYPE  VKBUR OPTIONAL
*"     REFERENCE(IV_VBELN) TYPE  VBELN_VA
*"     REFERENCE(IV_KWERT) TYPE  NETWR_AK
*"     REFERENCE(IV_TESTE) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_ZSDT0116) TYPE  ZSDS_ZSDT0116_TAB
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"     REFERENCE(ET_MESSAGE) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA LV_VLR_FIN TYPE NETWR_AP.
  DATA LV_LIMITE TYPE NETWR_AP.
  DATA LV_VLR_SOLICITADO TYPE NETWR_AP.
  DATA LV_VLR_AUX TYPE NETWR_AP.
  DATA LV_MSGX TYPE C LENGTH 100.
  DATA LV_SALDO TYPE NETWR_AK.
  DATA LV_RET.
  DATA LV_SEM_SAP.
  DATA LV_ERRO_CHECKL.
  DATA LV_JUST_WF TYPE C LENGTH 255.

  CHECK IV_SIMU IS NOT INITIAL AND IV_VBELN IS NOT INITIAL.

  IF IV_VKBUR <> '0101'.

    SELECT COUNT(*) FROM TVARVC WHERE NAME = 'ZSDT0100_TPSIM' AND LOW = IV_TPSIM.

    " valida se existir na stvarv
*    IF sy-dbcnt > 0.
    IF IV_TPSIM EQ 'VP'.

      PERFORM F_EXISTE_CHECKLIST
        USING IV_SIMU
              IV_VBELN
       CHANGING LV_ERRO_CHECKL.

      CASE LV_ERRO_CHECKL.
        WHEN '1'. "------------------------------------------ 1 - sem checklist
          LV_MSGX = `Simulador ` && IV_SIMU && ` não possui checklist ativo`.
          ET_MESSAGE = VALUE BAPIRET2_TAB(  ( TYPE = 'E' ID = 'DS' NUMBER = '016' MESSAGE = LV_MSGX ) ).
          EV_ERRO = ABAP_TRUE.
          EXIT.
        WHEN '2'. "------------------------------------------ 2 - com checklist, mas com inconformidade
          LV_MSGX = `Checklist com inconformidade, aguardando aprovação`.
          ET_MESSAGE = VALUE BAPIRET2_TAB(  ( TYPE = 'E' ID = 'DS' NUMBER = '016' MESSAGE = LV_MSGX ) ).
          EV_ERRO = ABAP_TRUE.
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

    LV_MSGX = |Deseja enviar O.V { IV_VBELN } para aprovação por Workflow?|.

    PERFORM F_POPUP_TO_CONFIRM USING LV_MSGX CHANGING LV_RET.

    IF LV_RET <> '1'.
      EV_ERRO = ABAP_TRUE.
      EXIT.
    ENDIF.

    PERFORM F_GET_TEXT USING 'Justificativa' CHANGING LV_JUST_WF.

    SELECT SINGLE * FROM ZSD_IN_EST_LIMITE_OV_01
      INTO @DATA(LS_LIMITE)
        WHERE DOC_SIMULACAO = @IV_SIMU
          AND VBELN = @IV_VBELN.

    IF SY-SUBRC NE 0.

      EV_ERRO = ABAP_TRUE.
      ET_MESSAGE = VALUE BAPIRET2_TAB(  ( TYPE = 'E' ID = 'DS' NUMBER = '016' MESSAGE = 'Sem saldo cadastrado' ) ).
      EXIT.

    ENDIF.

    " buscar saldo financeiro lv_vlr_fin = ( recebido das demais ovs simuladores ) -  ( quanto eu ja aprovei dessas ov na 116 )
    PERFORM F_BUSCA_VLR_FIN USING IV_SIMU CHANGING LV_VLR_FIN.

    " muda de variavel para ir descontando quanto foi sendo usado....
    LV_VLR_SOLICITADO = IV_KWERT.

    " enquanto tiver valor solicitado
    WHILE LV_VLR_SOLICITADO > 0.

      CLEAR LV_VLR_AUX.

      " se tem saldo financeiro
      IF LV_VLR_FIN > 0.

        " x = saldo financeiro menos valor solicitado
        LV_VLR_AUX = LV_VLR_FIN - LV_VLR_SOLICITADO.

        " se x ficar maior que zero, então posso descontar integral o valor solicitado
        IF LV_VLR_AUX > 0.

          PERFORM F_CRIA_NOVA_LINHA
           USING IV_VBELN
                 IV_TESTE
                 'A' "<- Aprovado direto sem WF
                 'F' "<- usando o limite do FINANCEIRO
                 LV_VLR_SOLICITADO
        CHANGING ET_ZSDT0116[] ET_MESSAGE[].

          LV_VLR_SOLICITADO = 0.

          " se ficar negativo, significa que vou usar o saldo integral do financeiro
        ELSE.

          LV_LIMITE = LV_VLR_FIN.

          PERFORM F_CRIA_NOVA_LINHA
              USING IV_VBELN
                    IV_TESTE
                    'A' "<- Aprovado direto sem WF
                    'F' "<-- usando o limite do FINANCEIRO
                    LV_LIMITE
           CHANGING ET_ZSDT0116[] ET_MESSAGE[].

          " retiro o que foi usado do limite FINANCEIRO
          SUBTRACT LV_VLR_FIN FROM LV_VLR_SOLICITADO.

          " atualiza o saldo virtual do FINANCEIRO para 0
          LV_VLR_FIN = 0.

        ENDIF.

        " se tiver saldo do sap
      ELSEIF LS_LIMITE-VALOR_LIMITE_DISPONIVEL_SAP > 0.

        " x = saldo financeiro menos valor solicitado
        LV_VLR_AUX = LS_LIMITE-VALOR_LIMITE_DISPONIVEL_SAP - LV_VLR_SOLICITADO.

        " se x ficar maior que zero, então posso descontar integral o valor solicitado
        IF LV_VLR_AUX > 0.

          PERFORM F_CRIA_NOVA_LINHA
           USING IV_VBELN
                 IV_TESTE
                 'A' "<- Aprovado direto sem WF
                 'S' "<- usando o limite do SAP
                 LV_VLR_SOLICITADO
        CHANGING ET_ZSDT0116[] ET_MESSAGE[].

          LV_VLR_SOLICITADO = 0.

          " se ficar negativo, significa que vou usar o saldo integral do sap
        ELSE.

          " se ficar negativo, então uso o saldo total do sap
          LV_LIMITE = LS_LIMITE-VALOR_LIMITE_DISPONIVEL_SAP.

          PERFORM F_CRIA_NOVA_LINHA
              USING IV_VBELN
                    IV_TESTE
                    'A' "<- Aprovado direto sem WF
                    'S' "<-- usando o limite do sap
                    LV_LIMITE
           CHANGING ET_ZSDT0116[] ET_MESSAGE[].

          " retiro o que foi usado do limite SAP
          SUBTRACT LS_LIMITE-VALOR_LIMITE_DISPONIVEL_SAP FROM LV_VLR_SOLICITADO.

          " atualiza o saldo virtual do sap para 0
          LS_LIMITE-VALOR_LIMITE_DISPONIVEL_SAP = 0.

        ENDIF.

        " se tiver limite do opus
      ELSEIF LS_LIMITE-VALOR_LIMITE_DISPONIVEL_OPUS > 0.

        LV_VLR_AUX = LS_LIMITE-VALOR_LIMITE_DISPONIVEL_OPUS - LV_VLR_SOLICITADO.

        " se x ficar maior que zero, então posso descontar integral o valor solicitado
        IF LV_VLR_AUX > 0.

          PERFORM F_CRIA_NOVA_LINHA
           USING IV_VBELN
                 IV_TESTE
                 'A' "<- Aprovado direto sem WF
                 'O' "<- usando o limite do OPUS
                 LV_VLR_SOLICITADO
        CHANGING ET_ZSDT0116[] ET_MESSAGE[]..

          LV_VLR_SOLICITADO = 0.

          " se ficar negativo, significa que vou usar o saldo integral do opus
        ELSE.

          LV_LIMITE = LS_LIMITE-VALOR_LIMITE_DISPONIVEL_OPUS.

          PERFORM F_CRIA_NOVA_LINHA
              USING IV_VBELN
                    IV_TESTE
                    'A' "<- Aprovado direto sem WF
                    'O' "<-- usando o limite do OPUS
                    LV_LIMITE
           CHANGING ET_ZSDT0116[] ET_MESSAGE[].

          " retiro o que foi usado do limite OPUS
          SUBTRACT LS_LIMITE-VALOR_LIMITE_DISPONIVEL_OPUS FROM LV_VLR_SOLICITADO.

          " atualiza o saldo virtual do OPUS para 0
          LS_LIMITE-VALOR_LIMITE_DISPONIVEL_OPUS = 0.

        ENDIF.

        " não tem saldo nos limites, entao enviamos para aprovação
      ELSE.

        PERFORM F_CRIA_NOVA_LINHA
         USING IV_VBELN
               IV_TESTE
               'L' "<- Liberado WF
               'Z' "<- usando aprovação via ZSDT0117
               LV_VLR_SOLICITADO
      CHANGING ET_ZSDT0116[] ET_MESSAGE[].

        LV_VLR_SOLICITADO = 0.

      ENDIF.

    ENDWHILE.

  ELSE.

    PERFORM F_CRIA_NOVA_LINHA
       USING IV_VBELN
             IV_TESTE
             'A' "<- Aprova automatico
             ' ' "<- Sem origem
             IV_KWERT
      CHANGING ET_ZSDT0116[] ET_MESSAGE[].

  ENDIF.

ENDFUNCTION.
