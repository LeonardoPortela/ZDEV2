FUNCTION Z_FRETE_ESTRATEGIA_EXECUTAR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_ORDENS STRUCTURE  ZFRE_ORDENS_APROV
*"      T_ESTRA STRUCTURE  ZFRE_ESTRATEGIA_ZGL
*"----------------------------------------------------------------------


  TYPE-POOLS: ICON.

  TYPES: BEGIN OF TY_ESTRA ,
           BUKRS     TYPE ZLEST0157-BUKRS,
           ORDEM     TYPE ZLEST0157-ORDEM,
           ID_ORDEM  TYPE ZLEST0157-ID_ORDEM,
           VALOR_DE  TYPE ZLEST0156-VALOR_DE,
           VALOR_ATE TYPE ZLEST0156-VALOR_ATE,
           APROVADOR TYPE ZLEST0156-APROVADOR,
           NIVEL     TYPE ZLEST0156-NIVEL,
           ESTADO(4),
           OPCOES(4),
         END OF TY_ESTRA,

         BEGIN OF TY_CADORDEM,
           EMPRESA(30)  TYPE C,
           ORDEM(50)    TYPE C,
           ID_ORDEM(50) TYPE C,
           USUARIO(20)  TYPE C,
           TOTAL        TYPE ZGLT036-VLR_MOEDA_INT,
           DATA(10),
         END OF TY_CADORDEM.

  DATA:   TG_ESTRA    TYPE TABLE OF TY_ESTRA,
          WG_CADORDEM TYPE TY_CADORDEM.

  DATA:  WL_ESTRA       LIKE LINE OF TG_ESTRA,
         WL_ESTRA2      LIKE LINE OF TG_ESTRA,
         W_ESTRA        TYPE ZFRE_ESTRATEGIA_ZGL,
         WL_INPUT_ESTRA TYPE ZLEST0157,
         WL_ZGLT035     TYPE ZGLT035,
         FLAG_UNDO(1),
         LINHA_ESTRA    TYPE SY-TABIX,
         ULT_LINHA      TYPE SY-TABIX,
         E_ROW_ID       TYPE SY-TABIX.

  LOOP AT T_ESTRA.
    MOVE-CORRESPONDING T_ESTRA TO WL_ESTRA.
    MOVE T_ESTRA-ORDEM    TO WL_ESTRA-ORDEM.
    MOVE T_ESTRA-ID_ORDEM TO WL_ESTRA-ID_ORDEM.
    APPEND WL_ESTRA TO TG_ESTRA.
  ENDLOOP.

  SORT TG_ESTRA BY NIVEL APROVADOR.
  SORT T_ESTRA  BY NIVEL APROVADOR.

  LOOP AT TG_ESTRA INTO WL_ESTRA WHERE APROVADOR = V_USUARIO.                 "/Modificação CS2016000820/
    "READ TABLE TG_ESTRA INTO WL_ESTRA WITH KEY  APROVADOR = V_USUARIO.       "/Modificação CS2016000820/
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
      READ TABLE T_ORDENS INDEX 1.
      WG_CADORDEM-EMPRESA  = T_ORDENS-EMPRESA.
      CONCATENATE  T_ORDENS-ORDEM'-'  INTO WG_CADORDEM-ORDEM.
      WG_CADORDEM-ID_ORDEM = T_ORDENS-ID_ORDEM .
      WG_CADORDEM-USUARIO  = V_USUARIO .
      WG_CADORDEM-TOTAL    = T_ORDENS-TOTAL.
      "WG_CADORDEM-DATA     = T_ORDENS-DT_VENC.
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

          SELECT SINGLE * INTO @DATA(WA_ZLEST0155)
            FROM ZLEST0155
           WHERE ID_ORDEM EQ @WL_ESTRA-ID_ORDEM.

*          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*          "Verificar se Possui Viagem Carguero e Enviar Aletração de Preço""""""""""""""""""""""""""
*          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*          TRY .
*              ZCL_ORDEM_CARREGAMENTO=>GET_INSTANCE(
*                )->SET_ORDEM( I_ID_ORDEM = WL_ESTRA-ID_ORDEM
*                )->SET_AUTORIZAR_PRECO_CARGUERO( I_PRECO = WA_ZLEST0155-VLR_FRETE_NEG
*                ).
*            CATCH ZCX_ORDEM_CARREGAMENTO INTO DATA(EX_ORDEM).
*              EX_ORDEM->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
*              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*              MSG = ZCL_STRING=>CONCAT( S1 = 'CARGUERO:' S2 = MTEXT SP = SPACE ).
*              EXIT.
*          ENDTRY.
*          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          WL_INPUT_ESTRA-MANDT       = SY-MANDT.
          WL_INPUT_ESTRA-BUKRS       = WL_ESTRA-BUKRS.
          WL_INPUT_ESTRA-ORDEM       = WL_ESTRA-ORDEM.
          WL_INPUT_ESTRA-ID_ORDEM    = WL_ESTRA-ID_ORDEM.
          WL_INPUT_ESTRA-NIVEL       = WL_ESTRA-NIVEL.
          WL_INPUT_ESTRA-APROVADOR   = WL_ESTRA-APROVADOR.
          WL_INPUT_ESTRA-VALOR_DE    = WL_ESTRA-VALOR_DE.
          WL_INPUT_ESTRA-VALOR_ATE   = WL_ESTRA-VALOR_ATE.
          WL_INPUT_ESTRA-DATA_ATUAL  = SY-DATUM.
          WL_INPUT_ESTRA-HORA_ATUAL  = SY-UZEIT.
          WL_INPUT_ESTRA-USUARIO     = SY-UNAME.
          MODIFY ZLEST0157 FROM WL_INPUT_ESTRA.
          CLEAR WL_INPUT_ESTRA.
          IF ULT_LINHA = LINHA_ESTRA.
            "ALRS
            UPDATE ZLEST0155 SET STATUS_APROV = '1' DT_MOD = SY-DATUM BNAME = SY-UNAME
             WHERE NR_ORDEM = WL_ESTRA-ORDEM
               AND ID_ORDEM = WL_ESTRA-ID_ORDEM.
            COMMIT WORK.
            LOOP AT TG_ESTRA INTO WL_ESTRA.
              WL_ESTRA-OPCOES = ' ' .
              WL_ESTRA-ESTADO = ICON_CHECKED .
              MODIFY TG_ESTRA FROM WL_ESTRA INDEX SY-TABIX TRANSPORTING OPCOES ESTADO.
            ENDLOOP.
            MSG = 'Processamento concluído com sucesso'.
          ELSE.
            WL_ESTRA-OPCOES = ICON_SYSTEM_UNDO .
            WL_ESTRA-ESTADO = ICON_CHECKED .
            MODIFY TG_ESTRA FROM WL_ESTRA INDEX E_ROW_ID.
            MSG = 'Processamento concluído com sucesso'.
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
          DELETE  FROM ZLEST0157
           WHERE BUKRS      = WL_ESTRA-BUKRS
             AND ORDEM      = WL_ESTRA-ORDEM
             AND ID_ORDEM   = WL_ESTRA-ID_ORDEM
             AND NIVEL      = WL_ESTRA-NIVEL
             AND APROVADOR  = WL_ESTRA-APROVADOR.
          WL_ESTRA-ESTADO = ICON_LED_YELLOW  .
          WL_ESTRA-OPCOES = ICON_SET_STATE.
          MODIFY TG_ESTRA FROM WL_ESTRA INDEX E_ROW_ID.
        ELSE.
          MSG = 'Devem ser reiniciadas as estratégias posteriores'.
        ENDIF.
      ELSEIF WL_ESTRA-OPCOES = ICON_REJECT.
        UPDATE ZLEST0155 SET STATUS_APROV = '2' DT_MOD = SY-DATUM BNAME = SY-UNAME
         WHERE NR_ORDEM = WL_ESTRA-ORDEM
           AND ID_ORDEM = WL_ESTRA-ID_ORDEM.
        COMMIT WORK.

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
