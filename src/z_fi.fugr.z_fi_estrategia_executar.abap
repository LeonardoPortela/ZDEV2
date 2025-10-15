FUNCTION Z_FI_ESTRATEGIA_EXECUTAR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"     VALUE(OK) TYPE  CHAR01
*"  TABLES
*"      T_LOTES STRUCTURE  ZFI_LOTES_IMP
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_IMP
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: ICON.

  TYPES: BEGIN OF TY_ESTRA ,
           BUKRS     TYPE ZIMP_LOTES_APROV-BUKRS,
           LOTE      TYPE ZIMP_LOTES_APROV-LOTE,
           VALOR_DE  TYPE ZIMP_APROVADOR-VALOR_DE,
           VALOR_ATE TYPE ZIMP_APROVADOR-VALOR_ATE,
           APROVADOR TYPE ZIMP_APROVADOR-APROVADOR,
           NIVEL     TYPE ZIMP_APROVADOR-NIVEL,
           ESTADO(4),
           OPCOES(4),
         END OF TY_ESTRA,

         BEGIN OF TY_CADLOTE,
           EMPRESA(30) TYPE C,
           LOTE(50)    TYPE C,
           USUARIO(20) TYPE C,
           TOTAL       TYPE ZIMP_LANC_IMP_CT-VALOR_IMP,
           DEP_RESP(2),
           DATA(10),
         END OF TY_CADLOTE.

  DATA:   TG_ESTRA   TYPE TABLE OF TY_ESTRA,
          WG_CADLOTE TYPE TY_CADLOTE.

  DATA:  WL_ESTRA       LIKE LINE OF TG_ESTRA,
         WL_ESTRA2      LIKE LINE OF TG_ESTRA,
         W_ESTRA        TYPE          ZFI_ESTRATEGIA_IMP,
         WL_INPUT_ESTRA TYPE ZIMP_LOTES_APROV,
         FLAG_UNDO(1),
         LINHA_ESTRA    TYPE SY-TABIX,
         ULT_LINHA      TYPE SY-TABIX,
         E_ROW_ID       TYPE SY-TABIX.

  OK = ABAP_FALSE.

  LOOP AT T_ESTRA.
    MOVE-CORRESPONDING T_ESTRA TO WL_ESTRA.
    APPEND WL_ESTRA TO TG_ESTRA.
  ENDLOOP.

  SORT TG_ESTRA BY NIVEL APROVADOR.
  SORT T_ESTRA BY NIVEL APROVADOR.

  LOOP AT TG_ESTRA INTO WL_ESTRA WHERE APROVADOR = V_USUARIO.                 "/Modificação CS2016000820/
    "READ TABLE TG_ESTRA INTO WL_ESTRA WITH KEY  APROVADOR = V_USUARIO.
    IF SY-SUBRC = 0.
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
      WG_CADLOTE-EMPRESA  = T_LOTES-EMPRESA.
      CONCATENATE  T_LOTES-LOTE '-'  INTO WG_CADLOTE-LOTE.
      WG_CADLOTE-USUARIO  = '' .
      WG_CADLOTE-TOTAL    = T_LOTES-TOTAL.
      WG_CADLOTE-DEP_RESP = T_LOTES-DEP_RESP+0(2).
      WG_CADLOTE-DATA     = T_LOTES-DT_VENC.
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
          WL_INPUT_ESTRA-VALOR_DE    = WL_ESTRA-VALOR_DE.
          WL_INPUT_ESTRA-VALOR_ATE   = WL_ESTRA-VALOR_ATE.
          WL_INPUT_ESTRA-DATA_ATUAL  = SY-DATUM.
          WL_INPUT_ESTRA-HORA_ATUAL  = SY-UZEIT.
          WL_INPUT_ESTRA-USUARIO     = SY-UNAME.
          MODIFY ZIMP_LOTES_APROV FROM WL_INPUT_ESTRA.
          CLEAR WL_INPUT_ESTRA.
          IF ULT_LINHA = LINHA_ESTRA.
            PERFORM GRAVA_ZIB(ZIMP56) USING WL_ESTRA-LOTE.

            UPDATE ZIMP_CAD_LOTE SET STATUS_LOTE = 'A'
            WHERE LOTE = WL_ESTRA-LOTE.
            COMMIT WORK.
            "MSG =  'Última liberação, lote liberado'..
            LOOP AT TG_ESTRA INTO WL_ESTRA.
              WL_ESTRA-OPCOES = ' ' .
              WL_ESTRA-ESTADO = ICON_CHECKED .
              MODIFY TG_ESTRA FROM WL_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES ESTADO.
            ENDLOOP.
            PERFORM ENVIA_EMAIL_FIM(ZIMP56) USING WG_CADLOTE.
            MSG = 'Processamento concluído com sucesso'.
            OK = ABAP_TRUE.
          ELSE.
            WL_ESTRA-OPCOES = ICON_SYSTEM_UNDO .
            WL_ESTRA-ESTADO = ICON_CHECKED .
            MODIFY TG_ESTRA FROM WL_ESTRA INDEX E_ROW_ID.
            PERFORM ENVIA_EMAIL(ZIMP56) TABLES TG_ESTRA USING WG_CADLOTE E_ROW_ID .
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
            MSG = 'Lote totalmente liberado'.
            FLAG_UNDO = 'N'.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF FLAG_UNDO = 'S'.
          DELETE  FROM ZIMP_LOTES_APROV
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
      "Pferraz - 17/10/23 - Status diferente app e transação
        UPDATE ZIMP_CAD_LOTE SET STATUS_LOTE = 'L'
*        UPDATE ZIMP_CAD_LOTE SET STATUS_LOTE = 'R'
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
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
