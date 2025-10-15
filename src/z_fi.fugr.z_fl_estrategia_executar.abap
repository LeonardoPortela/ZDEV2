FUNCTION Z_FL_ESTRATEGIA_EXECUTAR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(V_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     REFERENCE(MSG) TYPE  CHAR50
*"     REFERENCE(OK) TYPE  CHAR01
*"  TABLES
*"      T_LOTES STRUCTURE  ZFI_LOTES_FOL
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_FOL
*"----------------------------------------------------------------------

  TYPE-POOLS: ICON.

  DATA:   TG_ESTRA   TYPE TABLE OF ZFI_ESTRATEGIA_FOL.

  DATA:  WL_ESTRA       LIKE LINE OF TG_ESTRA,
         WL_ESTRA2      LIKE LINE OF TG_ESTRA,
         W_ESTRA        TYPE         ZFI_ESTRATEGIA_FOL,
         WL_INPUT_ESTRA TYPE ZHCMT_PY_0006,
         FLAG_UNDO(1),
         LINHA_ESTRA    TYPE SY-TABIX,
         ULT_LINHA      TYPE SY-TABIX,
         E_ROW_ID       TYPE SY-TABIX.

  OK = ABAP_FALSE.

  LOOP AT T_ESTRA.
    MOVE-CORRESPONDING T_ESTRA TO WL_ESTRA.
    MOVE T_ESTRA-LOTE TO WL_ESTRA-LOTE.
    APPEND WL_ESTRA TO TG_ESTRA.
  ENDLOOP.

  SORT TG_ESTRA BY NIVEL APROVADOR.
  SORT T_ESTRA BY NIVEL APROVADOR.

  LOOP AT TG_ESTRA INTO WL_ESTRA WHERE APROVADOR = V_USUARIO.

    IF E_ROW_ID IS NOT INITIAL AND SY-TABIX NE E_ROW_ID + 1.                "/Modificação CS2016000820/
      EXIT.                                                                 "/Modificação CS2016000820/
    ELSEIF E_ROW_ID IS NOT INITIAL AND SY-TABIX EQ E_ROW_ID + 1.            "/Modificação CS2016000820/
      IF WL_ESTRA-OPCOES NE ICON_SYSTEM_UNDO.                               "/Modificação CS2016000820/
        MOVE ICON_SET_STATE TO WL_ESTRA-OPCOES.                             "/Modificação CS2016000820/
      ENDIF.
      E_ROW_ID = SY-TABIX.                                                  "/Modificação CS2016000820/
    ELSE.
      E_ROW_ID = SY-TABIX.
    ENDIF.
    READ TABLE T_LOTES INDEX 1.
    "
    IF WL_ESTRA-OPCOES NE ICON_SET_STATE AND WL_ESTRA-OPCOES NE ICON_SYSTEM_UNDO AND WL_ESTRA-OPCOES NE ICON_REJECT .
      MSG =  'Opção inválida para processamento!'.
      EXIT.
    ENDIF.

    IF V_USUARIO NE WL_ESTRA-APROVADOR.
      MSG =  'Usuário não é o aprovador deste nível!'.
      EXIT.
    ENDIF.
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
        WL_INPUT_ESTRA-MANDT       = SY-MANDT.
        WL_INPUT_ESTRA-BUKRS       = WL_ESTRA-BUKRS.
        WL_INPUT_ESTRA-LOTE        = WL_ESTRA-LOTE.
        WL_INPUT_ESTRA-NIVEL       = WL_ESTRA-NIVEL.
        WL_INPUT_ESTRA-APROVADOR   = WL_ESTRA-APROVADOR.
        WL_INPUT_ESTRA-DATA_ATUAL  = SY-DATUM.
        WL_INPUT_ESTRA-HORA_ATUAL  = SY-UZEIT.
        WL_INPUT_ESTRA-USUARIO     = SY-UNAME.
        MODIFY ZHCMT_PY_0006 FROM WL_INPUT_ESTRA.
        CLEAR WL_INPUT_ESTRA.
        IF ULT_LINHA = LINHA_ESTRA.
          UPDATE ZHCMT_PY_0004 SET STATUS = 'APV'
          WHERE LOTE = WL_ESTRA-LOTE.
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
*            PERFORM ENVIA_EMAIL(ZGL017) TABLES TG_ESTRA USING WG_CADLOTE E_ROW_ID .
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
        DELETE  FROM ZHCMT_PY_0006
            WHERE BUKRS      = WL_ESTRA-BUKRS
            AND   LOTE       = WL_ESTRA-LOTE
            AND   NIVEL      = WL_ESTRA-NIVEL
            AND   APROVADOR  = WL_ESTRA-APROVADOR.
        WL_ESTRA-ESTADO = ICON_LED_YELLOW  .
        WL_ESTRA-OPCOES = ICON_SET_STATE.
        MODIFY TG_ESTRA FROM WL_ESTRA INDEX E_ROW_ID.
      ELSE.
        MSG = 'Devem ser reiniciadas as estratégias posteriores'.
      ENDIF.
    ELSEIF WL_ESTRA-OPCOES = ICON_REJECT.
      UPDATE ZHCMT_PY_0004 SET STATUS = 'RJT'
            WHERE LOTE = WL_ESTRA-LOTE.
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
