FUNCTION Z_IM_ESTRATEGIA_EXECUTAR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"     VALUE(OK) TYPE  CHAR01
*"  TABLES
*"      T_LOTES STRUCTURE  ZFI_GRU_INV
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_ZIM
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: ICON.

  DATA:   TG_ESTRA   TYPE TABLE OF ZFI_ESTRATEGIA_ZIM.

  DATA: WL_ESTRA        LIKE LINE OF TG_ESTRA,
        WL_ESTRA2       LIKE LINE OF TG_ESTRA,
        W_ESTRA         TYPE          ZFI_ESTRATEGIA_ZIM,
        WL_INPUT_ESTRA  TYPE          ZIM12_APROV_INV,
        WL_INPUT_ESTRA2 TYPE          ZIM12_AVAL_INV,
        VSEQ(10)        TYPE P,
        FLAG_UNDO(1),
        LINHA_ESTRA     TYPE SY-TABIX,
        ULT_LINHA       TYPE SY-TABIX,
        E_ROW_ID        TYPE SY-TABIX.

  OK = ABAP_FALSE.

  LOOP AT T_ESTRA.
    MOVE-CORRESPONDING T_ESTRA TO WL_ESTRA.
    APPEND WL_ESTRA TO TG_ESTRA.
  ENDLOOP.

  SORT TG_ESTRA BY NIVEL APROVADOR.
  SORT T_ESTRA BY NIVEL APROVADOR.


  READ TABLE T_LOTES INDEX 1.
  LOOP AT TG_ESTRA INTO WL_ESTRA WHERE APROVADOR = V_USUARIO.
    "
    IF WL_ESTRA-OPCOES NE ICON_SET_STATE AND WL_ESTRA-OPCOES NE ICON_SYSTEM_UNDO AND WL_ESTRA-OPCOES NE ICON_REJECT .
      MSG =  'Opção inválida para processamento!'.
      EXIT.
    ENDIF.

    IF SY-TCODE = 'ZIM06' AND WL_ESTRA-NIVELC IS NOT INITIAL.
      READ TABLE T_ESTRA INTO WL_ESTRA2 WITH KEY NIVEL = WL_ESTRA-NIVELC.
      IF V_USUARIO NE WL_ESTRA2-APROVADOR.
        MSG =  'Usuário não é o aprovador deste nível!'.
        EXIT.
      ENDIF.
      MOVE-CORRESPONDING WL_ESTRA2 TO WL_ESTRA.
    ENDIF.
    E_ROW_ID  = SY-TABIX.
    IF  WL_ESTRA-OPCOES = ICON_SET_STATE.
      FLAG_UNDO = 'S'.
      LINHA_ESTRA =  E_ROW_ID.
      LOOP AT TG_ESTRA INTO WL_ESTRA2.
        ULT_LINHA = SY-TABIX.
        IF WL_ESTRA2-OPCOES = ICON_SET_STATE AND SY-TABIX LT E_ROW_ID.
          FLAG_UNDO = 'N'.
        ENDIF.
      ENDLOOP.
      IF FLAG_UNDO = 'S'.
        IF T_LOTES-GPO_CMP NE '3'.
          IF T_LOTES-LOTE IS INITIAL.
            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                NR_RANGE_NR = '01'
                OBJECT      = 'ZID_LOTEIV'
              IMPORTING
                NUMBER      = VSEQ.
            T_LOTES-LOTE = VSEQ.
            MODIFY T_LOTES INDEX 1 TRANSPORTING LOTE.
          ENDIF.
          WL_INPUT_ESTRA-MANDT       = SY-MANDT.
          WL_INPUT_ESTRA-BUKRS       = T_LOTES-EMPRESA+0(4).
          WL_INPUT_ESTRA-WERKS       = WL_ESTRA-WERKS.
          WL_INPUT_ESTRA-KOSTL       = WL_ESTRA-KOSTL.
          WL_INPUT_ESTRA-FASE        = WL_ESTRA-FASE.
          WL_INPUT_ESTRA-NIVEL       = WL_ESTRA-NIVEL.
          WL_INPUT_ESTRA-LOTE        = T_LOTES-LOTE.
          WL_INPUT_ESTRA-APROVADOR   = WL_ESTRA-APROVADOR.
          WL_INPUT_ESTRA-DATA_APROV  = SY-DATUM.
          WL_INPUT_ESTRA-HORA_APROV  = SY-UZEIT.
          WL_INPUT_ESTRA-TOTAL       = T_LOTES-TOTAL.
          WL_INPUT_ESTRA-TOTAL_USD   = T_LOTES-TOTAL_USD.
          WL_INPUT_ESTRA-DATA_ATUAL  = SY-DATUM.
          WL_INPUT_ESTRA-HORA_ATUAL  = SY-UZEIT.
          WL_INPUT_ESTRA-USUARIO     = SY-UNAME.
          MODIFY ZIM12_APROV_INV FROM WL_INPUT_ESTRA.
          "Atualiza lote
          UPDATE ZIM01_SOL_AP_INV SET LOTE = T_LOTES-LOTE
           WHERE BUKRS  = T_LOTES-EMPRESA+0(4)
           AND   ANO    = T_LOTES-ANO
           AND   GSBER  = WL_ESTRA-WERKS
           AND   SAFRA  = WL_ESTRA-SAFRA
           AND   SAFRA2 = WL_ESTRA-SAFRA2
           AND   KOSTL  = WL_ESTRA-KOSTL
           AND   BUZEI  = WL_ESTRA-BUZEI
           AND   FASE   = T_LOTES-FASE
           AND   LOTE   = 0
           AND   ( ( STATUS_APROV = ' ' AND COD_GPO NE 3 ) OR STATUS_APROV = '9' ).
        ELSE.
          IF T_LOTES-LOTE IS INITIAL.
            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                NR_RANGE_NR = '01'
                OBJECT      = 'ZID_LOTEIV'
              IMPORTING
                NUMBER      = VSEQ.
            T_LOTES-LOTE = VSEQ.
            MODIFY T_LOTES INDEX 1 TRANSPORTING LOTE.
          ENDIF.
          WL_INPUT_ESTRA2-MANDT       = SY-MANDT.
          WL_INPUT_ESTRA2-BUKRS       = T_LOTES-EMPRESA+0(4).
          WL_INPUT_ESTRA2-NIVEL       = WL_ESTRA-NIVEL.
          WL_INPUT_ESTRA2-APROVADOR   = WL_ESTRA-APROVADOR.
          WL_INPUT_ESTRA2-DATA_APROV  = SY-DATUM.
          WL_INPUT_ESTRA2-HORA_APROV  = SY-UZEIT.
          WL_INPUT_ESTRA2-LOTE        = T_LOTES-LOTE.
          WL_INPUT_ESTRA2-TOTAL       = T_LOTES-TOTAL.
          WL_INPUT_ESTRA2-TOTAL_USD   = T_LOTES-TOTAL_USD.
          WL_INPUT_ESTRA2-DATA_ATUAL  = SY-DATUM.
          WL_INPUT_ESTRA2-HORA_ATUAL  = SY-UZEIT.
          WL_INPUT_ESTRA2-USUARIO     = SY-UNAME.
          MODIFY ZIM12_AVAL_INV FROM WL_INPUT_ESTRA2.
          "Atualiza lote AVALIAÇÃO
          UPDATE ZIM01_SOL_AP_INV SET LOTE3 = T_LOTES-LOTE
           WHERE BUKRS = T_LOTES-EMPRESA+0(4)
           AND   ANO   = T_LOTES-ANO
           AND   GSBER = WL_ESTRA-WERKS
           AND   SAFRA  = WL_ESTRA-SAFRA
           AND   SAFRA2 = WL_ESTRA-SAFRA2
           AND   KOSTL  = WL_ESTRA-KOSTL
           AND   BUZEI  = WL_ESTRA-BUZEI
           AND   FASE  = T_LOTES-FASE
           AND   LOTE3 = 0
           AND   ( STATUS_APROV = ' ' AND COD_GPO EQ 3 ).
        ENDIF.
        COMMIT WORK.
        WAIT UP TO 2 SECONDS.

        IF ULT_LINHA = LINHA_ESTRA.
          IF T_LOTES-GPO_CMP NE '3'.
            UPDATE ZIM01_SOL_AP_INV SET STATUS_APROV = '1'
                           DATA_APROV   = WL_INPUT_ESTRA-DATA_APROV
                           HORA_APROV   = WL_INPUT_ESTRA-HORA_APROV
            WHERE BUKRS  = T_LOTES-EMPRESA+0(4)
            AND   GSBER  = WL_ESTRA-WERKS
            AND   SAFRA  = WL_ESTRA-SAFRA
            AND   SAFRA2 = WL_ESTRA-SAFRA2
            AND   KOSTL  = WL_ESTRA-KOSTL
            AND   BUZEI  = WL_ESTRA-BUZEI
            AND   FASE   = T_LOTES-FASE
            AND   LOTE   = T_LOTES-LOTE.

          ELSE.
            UPDATE ZIM01_SOL_AP_INV SET STATUS_APROV = '9'
                                        DATA_APROV   = WL_INPUT_ESTRA2-DATA_APROV
                                        HORA_APROV   = WL_INPUT_ESTRA2-HORA_APROV
           WHERE BUKRS  = T_LOTES-EMPRESA+0(4)
           AND   SAFRA  = WL_ESTRA-SAFRA
           AND   SAFRA2 = WL_ESTRA-SAFRA2
           AND   KOSTL  = WL_ESTRA-KOSTL
           AND   BUZEI  = WL_ESTRA-BUZEI
           AND   FASE   = T_LOTES-FASE
           AND   LOTE3  = T_LOTES-LOTE.
          ENDIF.
          COMMIT WORK.
          LOOP AT TG_ESTRA INTO WL_ESTRA.
            WL_ESTRA-OPCOES = ' ' .
            WL_ESTRA-ESTADO = ICON_CHECKED .
            MODIFY TG_ESTRA FROM WL_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES ESTADO.
          ENDLOOP.
          MSG = 'Processamento concluído com sucesso'.
          OK = ABAP_TRUE.
        ELSE.
          WL_ESTRA-OPCOES = ICON_SYSTEM_UNDO .
          WL_ESTRA-ESTADO = ICON_CHECKED .
          MODIFY TG_ESTRA FROM WL_ESTRA INDEX E_ROW_ID.
          MSG = 'Processamento concluído com sucesso'.
          OK = ABAP_TRUE.
        ENDIF.
      ELSE.
        MSG = 'Devem ser aprovadas as estratégias anteriores'.
      ENDIF.
    ELSEIF  WL_ESTRA-OPCOES = ICON_SYSTEM_UNDO .
      FLAG_UNDO = 'S'.
      LINHA_ESTRA =  E_ROW_ID.
      LOOP AT TG_ESTRA INTO WL_ESTRA2.
        IF WL_ESTRA2-OPCOES = ICON_SYSTEM_UNDO AND SY-TABIX GT E_ROW_ID
          AND WL_ESTRA2-APROVADOR NE WL_ESTRA-APROVADOR.                        "/Modificação CS2016000820/
          FLAG_UNDO = 'N'.
        ENDIF.
        IF WL_ESTRA2-OPCOES = ICON_MESSAGE_CRITICAL.
          MSG = 'Solicitação totalmente liberada'.
          FLAG_UNDO = 'N'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF FLAG_UNDO = 'S'.
        IF T_LOTES-GPO_CMP NE '3'.
          UPDATE ZIM01_SOL_AP_INV SET STATUS_APROV = ' '
                 WHERE BUKRS  = T_LOTES-EMPRESA+0(4)
                 AND   ANO    = WL_ESTRA-ANO
                 AND   GSBER  = WL_ESTRA-WERKS
                 AND   SAFRA  = WL_ESTRA-SAFRA
                 AND   SAFRA2 = WL_ESTRA-SAFRA2
                 AND   KOSTL  = WL_ESTRA-KOSTL
                 AND   BUZEI  = WL_ESTRA-BUZEI
                 AND   FASE   = T_LOTES-FASE
                 AND   LOTE   = T_LOTES-LOTE
                 AND   STATUS_APROV = '1'.
          DELETE  FROM ZIM12_APROV_INV
              WHERE BUKRS      = T_LOTES-EMPRESA+0(4)
              AND   WERKS      = WL_ESTRA-WERKS
              AND   KOSTL      = WL_ESTRA-KOSTL
              AND   NIVEL      = WL_ESTRA-NIVEL
              AND   APROVADOR  = WL_ESTRA-APROVADOR
              AND   LOTE       = WL_ESTRA-LOTE.
        ELSE.
          DELETE  FROM ZIM12_AVAL_INV
               WHERE BUKRS      = T_LOTES-EMPRESA+0(4)
               AND   NIVEL      = WL_ESTRA-NIVEL
               AND   APROVADOR  = WL_ESTRA-APROVADOR
               AND   LOTE       = WL_ESTRA-LOTE.
        ENDIF.
        COMMIT WORK.
        WL_ESTRA-ESTADO = ICON_LED_YELLOW.
        WL_ESTRA-OPCOES = ICON_SET_STATE.
        MODIFY TG_ESTRA FROM WL_ESTRA INDEX E_ROW_ID.
      ELSE.
        MSG = 'Devem ser reiniciadas as estratégias posteriores'.
      ENDIF.
    ELSEIF WL_ESTRA-OPCOES = ICON_REJECT.
      IF T_LOTES-GPO_CMP NE '3'.
        UPDATE ZIM01_SOL_AP_INV SET STATUS_APROV = '2'
                                    DATA_APROV   = SY-DATUM
                                    HORA_APROV   = SY-UZEIT
                                    USUARIO      = SY-UNAME
                  WHERE BUKRS  = T_LOTES-EMPRESA+0(4)
                  AND   ANO    = WL_ESTRA-ANO
                  AND   GSBER  = WL_ESTRA-WERKS
                  AND   SAFRA  = WL_ESTRA-SAFRA
                  AND   SAFRA2 = WL_ESTRA-SAFRA2
                  AND   KOSTL  = WL_ESTRA-KOSTL
                  AND   BUZEI  = WL_ESTRA-BUZEI
                  AND   FASE   = T_LOTES-FASE
                  AND   LOTE   = T_LOTES-LOTE
                  AND   ( STATUS_APROV = ' ' OR STATUS_APROV = '9' ).
      ELSE.
        UPDATE ZIM01_SOL_AP_INV SET STATUS_APROV = '2'
                                  DATA_APROV   = SY-DATUM
                                  HORA_APROV   = SY-UZEIT
                                  USUARIO      = SY-UNAME
                WHERE BUKRS = T_LOTES-EMPRESA+0(4)
                AND   ANO    = WL_ESTRA-ANO
                AND   SAFRA  = WL_ESTRA-SAFRA
                AND   SAFRA2 = WL_ESTRA-SAFRA2
                AND   KOSTL  = WL_ESTRA-KOSTL
                AND   BUZEI  = WL_ESTRA-BUZEI
                AND   FASE  = T_LOTES-FASE
                AND   LOTE3 = T_LOTES-LOTE
                AND   ( STATUS_APROV = ' ' OR STATUS_APROV = '9' ).
      ENDIF.
      COMMIT WORK.
      MSG = 'Processamento concluído com sucesso'.
      OK = ABAP_TRUE.
    ENDIF.
    LOOP AT TG_ESTRA INTO WL_ESTRA.
      IF E_ROW_ID = SY-TABIX.
        MOVE-CORRESPONDING WL_ESTRA TO W_ESTRA.
        MODIFY T_ESTRA FROM W_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES ESTADO.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDFUNCTION.
