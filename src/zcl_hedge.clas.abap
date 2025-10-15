class ZCL_HEDGE definition
  public
  final
  create public .

public section.

  methods RUN .
protected section.
private section.

  data:
    R_TPVENDA TYPE RANGE OF ZSDED012 .
  constants AT_MOEDA type WAERK value 'BRL' ##NO_TEXT.
  constants:
    BEGIN OF STATUS,
      LIBERADO         TYPE ZSDED021 VALUE 'L',
      ENCERRADO        TYPE ZSDED021 VALUE 'E',
      RECUSA_DEVOLUCAO TYPE ZSDED021 VALUE 'Y',
      COMPLEMENTO      TYPE ZSDED021 VALUE 'W',
    END OF STATUS .
  constants:
    BEGIN OF D_STATUS,
      LIBERADO(9)          VALUE 'Liberação',
      ENCERRADO(12)        VALUE 'Encerramento',
      RECUSA_DEVOLUCAO(16) VALUE 'Recusa/Devolução',
      COMPLEMENTO(11)      VALUE 'Complemento',
      ALTERARQTD(23)       VALUE 'Alteração de Quantidade',
      EDICAO(6)            VALUE 'Edição',
      FRETE(5)             VALUE 'Frete',
    END OF D_STATUS .
  constants:
    BEGIN OF TIPO,
      LIBERAR(3)      VALUE 'LIB',
      FRAME(3)        VALUE 'FRA',
      FRETE(3)        VALUE 'FRE',
      EDITAR(3)       VALUE 'EDI',
      DELETAR(3)      VALUE 'DEL',
      EDICAOFRAME(3)  VALUE 'EDF',
      ENCERRAMENTO(3) VALUE 'ENC',
      CALCULAFRETE(3) VALUE 'LOG',
      EDITARFRAME(3)  VALUE 'EDF',
    END OF TIPO .
  constants:
    BEGIN OF TCODE,
      ZS62 TYPE SYTCODE VALUE 'ZSDT0062',
      ZS66 TYPE SYTCODE VALUE 'ZSDT0066',
      VF01 TYPE SYTCODE VALUE 'VF01',
      VF11 TYPE SYTCODE VALUE 'VF11',
    END OF TCODE .
  constants:
    BEGIN OF UCOMM,
      ZLI TYPE SYUCOMM VALUE 'LIBERAR',
      ZMO TYPE SYUCOMM VALUE 'MODRED',
    END OF UCOMM .
  constants:
    BEGIN OF PROGRAM,
      RV60AFZZ TYPE SYCPROG VALUE 'SAPMV60A',
    END OF PROGRAM .
  constants:
    BEGIN OF AUART,
      ZCPV TYPE AUART VALUE 'ZCPV',
    END OF AUART .
  data AT_DATAINICIAL type SY-DATUM .

  methods GET_DADOS_51 .
  methods SET_TP_VENDA_SIMPLES
    importing
      !I_0051 type ZSDT0051 optional .
  methods SET_TP_VENDA_FRAME
    importing
      !I_0051 type ZSDT0051 optional .
  methods SET_DEVOLUCAO_COMPLEMENTO
    importing
      !I_0053 type ZSDT0053 .
  methods SET_VF11_DEVOLUCAO_COMPLEMENTO
    importing
      !I_0053 type ZSDT0053 .
  methods GET_DADOS_OV .
  methods GET_DADOS_40 .
  methods GET_DADOS_90 .
ENDCLASS.



CLASS ZCL_HEDGE IMPLEMENTATION.


  METHOD GET_DADOS_40.

    DATA(OBJ_WEB_TX_CURVA) = NEW ZCL_WEBSERVICE_TX_CURVA( ).

    SELECT *
      FROM ZSDT0040
      INTO TABLE @DATA(IT_0040)
      WHERE DATA_ATUAL GE @AT_DATAINICIAL
        AND JOB EQ @ABAP_FALSE.

    LOOP AT IT_0040 INTO DATA(WA_0040).

      DATA(DOC) = WA_0040-DOC_SIMULACAO.
      DOC = |{ DOC ALPHA = OUT }|.

      CALL FUNCTION 'ENQUEUE_EZSDT0040_JOB'
        EXPORTING
          DOC_SIMULACAO  = DOC
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE S000 WITH |S.V bloqueada-{ WA_0040-DOC_SIMULACAO }|.
        CONTINUE.
      ENDIF.

      CHECK WA_0040-TPSIM NE 'PM'. " // A condição de Pagamento Permuta não dispara Hedge

      SY-CPROG = 'ZSDR016'.
      CASE WA_0040-STATUS.
        WHEN 'A'.
          IF WA_0040-WAERK EQ 'BRL'
         AND WA_0040-TPSIM NE 'BN'.

            CALL METHOD OBJ_WEB_TX_CURVA->HEDGE_INSUMOS
              EXPORTING
                I_NUMERO = WA_0040-DOC_SIMULACAO
                I_TIPO   = 'VDI'.

          ENDIF.

          CALL METHOD OBJ_WEB_TX_CURVA->HEDGE_INSUMOS
            EXPORTING
              I_NUMERO = WA_0040-DOC_SIMULACAO
              I_TIPO   = 'FRI'.

        WHEN 'R' OR 'B'.

          CALL METHOD OBJ_WEB_TX_CURVA->HEDGE_INSUMOS
            EXPORTING
              I_NUMERO = WA_0040-DOC_SIMULACAO
              I_TIPO   = 'EST'.

      ENDCASE.

      CALL FUNCTION 'DEQUEUE_EZSDT0040_JOB'
        EXPORTING
          DOC_SIMULACAO = DOC.

    ENDLOOP.


  ENDMETHOD.


  METHOD GET_DADOS_51.

    DATA(OBJ_TX_CURVA) = NEW ZCL_TAXA_CURVA( ).

    R_TPVENDA = OBJ_TX_CURVA->GET_AUART( 'MAGGI_ZSDT0062_HEDGE' ).

    SELECT *
      FROM ZSDT0051
      INTO TABLE @DATA(IT_0051)
      WHERE DATA_ATUAL GE @AT_DATAINICIAL
        AND WAERK EQ @AT_MOEDA
        AND TP_VENDA IN @R_TPVENDA
        AND JOB EQ @ABAP_FALSE.

    LOOP AT IT_0051 INTO DATA(WA_0051).

      CALL FUNCTION 'ENQUEUE_EZSDT0051'
        EXPORTING
          NRO_SOL_OV     = WA_0051-NRO_SOL_OV
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE S000 WITH |S.V bloqueada-{ WA_0051-NRO_SOL_OV }|.
        CONTINUE.
      ENDIF.

      ME->SET_TP_VENDA_SIMPLES( WA_0051 ).

      ME->SET_TP_VENDA_FRAME( WA_0051 ).

      UPDATE ZSDT0051
              SET JOB = ABAP_TRUE
           WHERE NRO_SOL_OV EQ WA_0051-NRO_SOL_OV.

      UPDATE ZSDT0053
              SET JOB = ABAP_TRUE
           WHERE NRO_SOL_OV EQ WA_0051-NRO_SOL_OV
             AND STATUS EQ 'E'
             AND JOB EQ 'E'.

      CALL FUNCTION 'DEQUEUE_EZSDT0051'
        EXPORTING
          NRO_SOL_OV = WA_0051-NRO_SOL_OV.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_DADOS_90.

    DATA(OBJ_WEB_TX_CURVA) = NEW ZCL_WEBSERVICE_TX_CURVA( ).

    DATA: VL_TRAVA_CAMBIO  TYPE C,
          VL_ORIG_TRV_CAM  TYPE C,
          VL_OV_PRECEDENTE TYPE VBELN,
          VL_QTDE_EQUAL    TYPE C.

    SELECT *
      FROM ZSDT0090
      INTO TABLE @DATA(IT_0090)
      WHERE DATA_ATUAL GE @AT_DATAINICIAL
        AND JOB EQ @ABAP_FALSE.

    LOOP AT IT_0090 INTO DATA(W_0090).

      CLEAR: VL_TRAVA_CAMBIO, VL_ORIG_TRV_CAM, VL_OV_PRECEDENTE.

      SELECT SINGLE *
        FROM ZSDT0040
        INTO @DATA(W_0040)
        WHERE DOC_SIMULACAO EQ @W_0090-DOC_SIMULACAO.

      IF W_0040-TPSIM NE 'PM'.
        IF W_0040-TPSIM NE 'BN'.

          IF W_0040-WAERK EQ 'USD'.
            CALL FUNCTION 'ZSDMF001_CHECK_OV_TRAVA_CAMBIO'
              EXPORTING
                I_DOC_SIMULACAO     = W_0090-DOC_SIMULACAO
                I_VBELN             = W_0090-VBELV
              CHANGING
                C_TRAVA_CAMBIO      = VL_TRAVA_CAMBIO
                C_TRAVA_CAMBIO_PREC = VL_ORIG_TRV_CAM
                C_OV_PRECEDENTE     = VL_OV_PRECEDENTE.
          ENDIF.

          CALL FUNCTION 'ZSDMF001_COMPARE_UNIT_MAT'
            EXPORTING
              I_MATNR_01 = W_0090-MATNR
              I_MENGE_01 = W_0090-ZMENG
              I_MATNR_02 = W_0090-MATNRV
              I_MENGE_02 = W_0090-ZMENGV
            IMPORTING
              E_EQUAL    = VL_QTDE_EQUAL.

          IF ( W_0040-WAERK EQ 'BRL' ) OR

             ( ( W_0040-WAERK EQ 'USD' ) AND
               ( VL_TRAVA_CAMBIO IS NOT INITIAL ) AND
               ( W_0090-CATEGORIA NE 'P' ) ).

            CASE W_0090-CATEGORIA.
              WHEN 'A'." OR 'E' OR 'O'.
                CALL METHOD ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS
                  EXPORTING
                    I_ACAO = 'ALTERAR' " Ação da Operação
                    I_0090 = W_0090    " Simulador de Vendas – Controle de Transferencias de OV
                    I_TIPO = 'VDI'.    " Campo de 3 bytes de comprimento

              WHEN OTHERS.

                IF ( 'U_M' CS W_0090-CATEGORIA            ) AND
                   ( W_0090-MATKLV        EQ W_0090-MATKL ) AND
                   ( W_0090-MATKLV        NE '658445'     ) AND
                   ( VL_QTDE_EQUAL        EQ ABAP_FALSE  ).

                  CALL METHOD ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS
                    EXPORTING
*                     I_ACAO = H_UCOMM " Ação da Operação
                      I_TIPO = 'VDI' " Campo de 3 bytes de comprimento
                      I_0090 = W_0090   " Simulador de Vendas – Controle de Transferencias de OV
                      I_DIR  = CONV #( W_0090-CATEGORIA ).  " Código de uma posição
                ELSEIF W_0090-MATKLV NE W_0090-MATKL.
                  CALL METHOD ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS
                    EXPORTING
*                     I_ACAO = H_UCOMM   " Ação da Operação
                      I_TIPO = 'VDI'   " Campo de 3 bytes de comprimento
                      I_0090 = W_0090.  " Simulador de Vendas – Controle de Transferencias de OV
                ELSEIF W_0090-NETPR NE W_0090-NETPRV. " REDISTRIBUIÇÃO
                  IF W_0090-CATEGORIA EQ 'R'.
                    CALL METHOD ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS
                      EXPORTING
*                       I_ACAO = ''   " Ação da Operação
                        I_TIPO = 'VDI'   " Campo de 3 bytes de comprimento
                        I_0090 = W_0090. " Simulador de Vendas – Controle de Transferencias de OV
                  ENDIF.
                ENDIF.

            ENDCASE.
          ELSEIF ( W_0040-WAERK EQ 'USD' ).
            IF W_0090-CATEGORIA EQ 'P'.

              IF VL_ORIG_TRV_CAM IS NOT INITIAL.
                CALL METHOD ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS
                  EXPORTING
                    I_NUMERO = W_0090-DOC_SIMULACAO  " Numero do documento de simulação de venda
                    I_TIPO   = 'EST'  " Campo de 3 bytes de comprimento
                    I_0090   = W_0090  " Simulador de Vendas – Controle de Transferencias de OV
                    I_VBELN  = VL_OV_PRECEDENTE   " Nº documento de vendas e distribuição
                    I_DIR    = 'J'.   " Código de uma posição
              ENDIF.

              CALL METHOD ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS
                EXPORTING
*                  I_ACAO = H_UCOMM  " Ação da Operação
                  I_TIPO = 'VDI'  " Campo de 3 bytes de comprimento
                  I_0090 = W_0090.   " Simulador de Vendas – Controle de Transferencias de OV
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD GET_DADOS_OV.

    DATA(OBJ_TX_CURVA) = NEW ZCL_TAXA_CURVA( ).
    DATA: R_TODOS TYPE RANGE OF AUART.

    R_TODOS = OBJ_TX_CURVA->GET_AUART( 'TODOS' ).

*   "// Complemento e Devolução
    SELECT B~*
      FROM ZSDT0051 AS A
      INNER JOIN ZSDT0053 AS B ON A~NRO_SOL_OV EQ B~NRO_SOL_OV
      INTO TABLE @DATA(IT_0053)
      WHERE B~DATA_ATUAL GE @AT_DATAINICIAL
        AND A~WAERK EQ @AT_MOEDA
        AND A~TP_VENDA IN @R_TPVENDA
*        AND A~JOB EQ @ABAP_FALSE
        AND B~JOB IN ( 'Y', 'W' ).

    LOOP AT IT_0053 INTO DATA(WA_0053).

      ME->SET_DEVOLUCAO_COMPLEMENTO( WA_0053 ).

      UPDATE ZSDT0053
                 SET JOB = ABAP_TRUE
                 WHERE NRO_SOL_OV EQ WA_0053-NRO_SOL_OV
                   AND POSNR EQ WA_0053-POSNR
                   AND VBELN EQ WA_0053-VBELN.
    ENDLOOP.

    "// Complemento - Devolução
    SELECT *
      FROM VBAK
      INTO TABLE @DATA(IT_VBAK)
      WHERE ERDAT GE @AT_DATAINICIAL
        AND AUART IN @R_TODOS.

    LOOP AT IT_VBAK INTO DATA(WA_VBAK).

      SELECT COUNT(*)
        FROM ZSDT0094
        WHERE VBELN EQ WA_VBAK-VBELN.

      DATA(_SUBRC) = SY-SUBRC.

      SELECT B~*
        FROM ZSDT0051 AS A
        INNER JOIN ZSDT0053 AS B ON A~NRO_SOL_OV EQ B~NRO_SOL_OV
        INTO TABLE @IT_0053
        WHERE B~VBELN EQ @WA_VBAK-VBELN
*          AND A~JOB EQ @ABAP_FALSE
          AND A~TP_VENDA IN @R_TPVENDA.

      SELECT B~*
        FROM ZSDT0051 AS A
        INNER JOIN ZSDT0100 AS B ON A~NRO_SOL_OV EQ B~NRO_SOL_OV
        APPENDING CORRESPONDING FIELDS OF TABLE @IT_0053
        WHERE B~VBELN EQ @WA_VBAK-VBELN
*          AND A~JOB EQ @ABAP_FALSE
          AND A~TP_VENDA IN @R_TPVENDA.

      LOOP AT IT_0053 INTO WA_0053.
        IF _SUBRC IS NOT INITIAL.
          ME->SET_DEVOLUCAO_COMPLEMENTO( WA_0053 ).
        ELSE.
*          CONTINUE.
          "// Estorno VF11
          SELECT COUNT(*)
            FROM VBFA
            WHERE VBELV EQ WA_VBAK-VBELN
              AND VBTYP_N EQ 'S'.
*            AND VBTYP_V EQ 'C'.

          IF SY-SUBRC IS INITIAL.
            ME->SET_VF11_DEVOLUCAO_COMPLEMENTO( WA_0053 ).
          ENDIF.
        ENDIF.
        UPDATE ZSDT0053
                   SET JOB = ABAP_TRUE
                   WHERE NRO_SOL_OV EQ WA_0053-NRO_SOL_OV
                     AND POSNR EQ WA_0053-POSNR
                     AND VBELN EQ WA_0053-VBELN.
      ENDLOOP.

    ENDLOOP.


  ENDMETHOD.


  METHOD RUN.

    AT_DATAINICIAL = SY-DATUM - 1.

*   //" Get dados da ZSDT0051 para realizar o processamento do Hedge
*    ME->GET_DADOS_51( ).

*   //" Get O.Vs de Complemento e Devolução para processamento do Hedge
*    ME->GET_DADOS_OV( ).

*   //" Get dados da ZSDT0040 para realizar o processamento do Hedge
*    ME->GET_DADOS_40( ).

*   //" Get dados da ZSDT0090 para realizar o processamento do Hedge
*    ME->GET_DADOS_90( ).

  ENDMETHOD.


  METHOD SET_DEVOLUCAO_COMPLEMENTO.

    DATA(OBJ_WEB_TX_CURVA) = NEW ZCL_WEBSERVICE_TX_CURVA( ).

    SY-CPROG = PROGRAM-RV60AFZZ.
    SY-TCODE = TCODE-VF01.

    SELECT SINGLE WAERK
      FROM ZSDT0051
      INTO @DATA(_MOEDA)
      WHERE NRO_SOL_OV EQ @I_0053-NRO_SOL_OV
      AND PARAM_ESPEC EQ 'M'.

    IF SY-SUBRC IS NOT INITIAL.
      TRY.
          OBJ_WEB_TX_CURVA->EXECUTAR(
                                        I_NUMERO  = I_0053-NRO_SOL_OV
                                        I_TIPO    = TIPO-ENCERRAMENTO
                                        I_TCODE   = TCODE-VF01
                                        I_STATUS  = COND #( WHEN I_0053-JOB EQ STATUS-RECUSA_DEVOLUCAO THEN STATUS-RECUSA_DEVOLUCAO ELSE ABAP_FALSE )
                                        I_AUART   = COND #( WHEN I_0053-JOB EQ STATUS-RECUSA_DEVOLUCAO THEN ABAP_FALSE ELSE AUART-ZCPV )
                                        I_VBELN   = I_0053-VBELN
                                    ).
        CATCH ZCX_WEBSERVICE INTO DATA(CX_EXC).
          DATA(_MSG) = CX_EXC->GET_TEXT( ).
          MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH _MSG.
      ENDTRY.
    ELSE.

      SELECT SINGLE AUART
        FROM VBAK
        INTO @DATA(VL_AUART)
        WHERE VBELN EQ @I_0053-VBELN.

      TRY.
          OBJ_WEB_TX_CURVA->EXECUTAR(
                                      I_NUMERO  = I_0053-NRO_SOL_OV
                                      I_TIPO    = TIPO-FRAME
                                      I_FIXACAO = I_0053-FIXACAO
                                      I_TCODE   = TCODE-VF01
                                      I_VBELN   = I_0053-VBELN
                                      I_AUART   = VL_AUART
                                    ).
          IF _MOEDA EQ AT_MOEDA.
            SY-TCODE = TCODE-VF01.
            OBJ_WEB_TX_CURVA->EXECUTAR(
                                        I_NUMERO  = I_0053-NRO_SOL_OV
                                        I_TIPO    = TIPO-FRETE
                                        I_FIXACAO = I_0053-FIXACAO
                                        I_TCODE   = TCODE-VF01
                                        I_STATUS  = ABAP_TRUE
                                        I_VBELN   = I_0053-VBELN
                                        I_AUART   = VL_AUART
                                      ).
          ENDIF.
        CATCH ZCX_WEBSERVICE INTO CX_EXC.
          _MSG = CX_EXC->GET_TEXT( ).
          MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH _MSG.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD SET_TP_VENDA_FRAME.
*   "// Será disparado somente para o Parametro especial tipo 'M'
*   "// Sem O.V
*   "// O Disparo da Venda e do Frete é feita separado
*   "// O disparo do Frete quando não tem O.V é realizado se o Status_ITM for ' ' ou 'F'
*   "// Caso o Status_ITM for 'F' é verificado se Existe disparo para a fixação informada caso não exista dispara o HEDGE
*   "// Caso o Status_IMT for ' ' é verificado se Existe disparo para a fixação informada caso não exista continua da mesma forma se existir é lançado um disparo de estorno
*   "// Com O.V
*   "// O disparo da Venda e do Frete é feito Juntos
*   "// É verificado na 53 se existe O.V
*   "// É chamado um metodo de VDA com o SY-UCOMM MODRED para quer seja feito a bruxaria la dentro.

    CHECK I_0051-PARAM_ESPEC EQ 'M'.  "// Parametro especial eq 'M'

    MESSAGE S000 WITH |Hedge - Inicio - { I_0051-NRO_SOL_OV }|.

    DATA(OBJ_WEB_TX_CURVA) = NEW ZCL_WEBSERVICE_TX_CURVA( ).

    SELECT *
      FROM ZSDT0053
      INTO TABLE @DATA(IT_0053)
      WHERE NRO_SOL_OV EQ @I_0051-NRO_SOL_OV.

    DATA(IT_0053_AUX) = IT_0053.
    DELETE IT_0053_AUX WHERE VBELN EQ ' '.

    IF IT_0053_AUX IS INITIAL. "// sem O.V
      MESSAGE S000 WITH |{ D_STATUS-LIBERADO } - { I_0051-NRO_SOL_OV }|.
      TRY.
          CALL METHOD OBJ_WEB_TX_CURVA->EXECUTAR
            EXPORTING
              I_NUMERO = I_0051-NRO_SOL_OV  " Numero de Solicitação de Ordem de Venda
              I_TIPO   = TIPO-FRAME         " Nº item do Tipo de Solicitação
              I_TCODE  = TCODE-ZS62.         " Código de transação atual

        CATCH ZCX_WEBSERVICE INTO DATA(CX_EXC).
          DATA(_MSG) = CX_EXC->GET_TEXT( ).
          MESSAGE S000 WITH _MSG.
      ENDTRY.
    ELSE. "// com O.V

      SELECT *
        FROM ZSDT0053
        INTO TABLE @DATA(IT_053)
          WHERE NRO_SOL_OV EQ @I_0051-NRO_SOL_OV
            AND VBELN NE ' '
            AND STATUS EQ @STATUS-ENCERRADO
            AND ZMENG NE 0.

      IF SY-SUBRC IS INITIAL.
        LOOP AT IT_053 INTO DATA(W_053).
          CASE I_0051-WAERK.
            WHEN 'BRL'.
              MESSAGE S000 WITH |{ D_STATUS-ENCERRADO } - { I_0051-NRO_SOL_OV } - { W_053-FIXACAO }|.
              TRY.
                  CALL METHOD OBJ_WEB_TX_CURVA->EXECUTAR
                    EXPORTING
                      I_NUMERO  = I_0051-NRO_SOL_OV " Numero de Solicitação de Ordem de Venda
                      I_TIPO    = TIPO-EDITAR       " Nº item do Tipo de Solicitação
                      I_FIXACAO = W_053-FIXACAO     " Nº item do documento de vendas e distribuição
                      I_TCODE   = TCODE-ZS62.        " Código de transação atual

                  CALL METHOD OBJ_WEB_TX_CURVA->EXECUTAR
                    EXPORTING
                      I_NUMERO  = I_0051-NRO_SOL_OV  " Numero de Solicitação de Ordem de Venda
                      I_TIPO    = TIPO-FRAME         " Nº item do Tipo de Solicitação
                      I_FIXACAO = W_053-FIXACAO      " Nº item do documento de vendas e distribuição
                      I_TCODE   = TCODE-ZS66.         " Código de transação atual

                  CALL METHOD OBJ_WEB_TX_CURVA->EXECUTAR
                    EXPORTING
                      I_NUMERO  = I_0051-NRO_SOL_OV   " Numero de Solicitação de Ordem de Venda
                      I_TIPO    = TIPO-FRETE          " Nº item do Tipo de Solicitação
                      I_FIXACAO = W_053-FIXACAO       " Nº item do documento de vendas e distribuição
                      I_STATUS  = ABAP_TRUE
                      I_TCODE   = TCODE-ZS62.          " Código de transação atual

                CATCH ZCX_WEBSERVICE INTO CX_EXC.
                  _MSG = CX_EXC->GET_TEXT( ).
                  MESSAGE S000 WITH _MSG.
              ENDTRY.
            WHEN 'USD'.
              MESSAGE S000 WITH |{ D_STATUS-ENCERRADO } - { I_0051-NRO_SOL_OV } - { W_053-FIXACAO }|.
              TRY.
                  CALL METHOD OBJ_WEB_TX_CURVA->EXECUTAR
                    EXPORTING
                      I_NUMERO  = I_0051-NRO_SOL_OV " Numero de Solicitação de Ordem de Venda
                      I_TIPO    = TIPO-CALCULAFRETE " Nº item do Tipo de Solicitação
                      I_FIXACAO = W_053-FIXACAO     " Nº item do documento de vendas e distribuição
                      I_TCODE   = TCODE-ZS62.        " Código de transação atual

                CATCH ZCX_WEBSERVICE INTO CX_EXC.
                  _MSG = CX_EXC->GET_TEXT( ).
                  MESSAGE S000 WITH _MSG.
              ENDTRY.
          ENDCASE.
        ENDLOOP.
        EXIT.
      ELSE.
        "// Alteração de Quantidade
        "// O disparo da Venda para a ação auteração de quantidade dispara tbm o frete.
        MESSAGE S000 WITH |{ D_STATUS-ALTERARQTD } - { I_0051-NRO_SOL_OV }|.
        TRY.
            CALL METHOD OBJ_WEB_TX_CURVA->EXECUTAR
              EXPORTING
                I_NUMERO = I_0051-NRO_SOL_OV " Numero de Solicitação de Ordem de Venda
                I_TIPO   = TIPO-FRAME " Nº item do Tipo de Solicitação
                I_UCOMM  = UCOMM-ZMO " Código de função que acionou o PAI
                I_TCODE  = TCODE-ZS62. " Código de transação atual
          CATCH ZCX_WEBSERVICE INTO CX_EXC.
            _MSG = CX_EXC->GET_TEXT( ).
            MESSAGE S000 WITH _MSG.
        ENDTRY.
      ENDIF.
    ENDIF.

    LOOP AT IT_0053 INTO DATA(WA_0053).

      SELECT COUNT(*)
        FROM ZSDT0094
          WHERE NRO_SOL_OV EQ I_0051-NRO_SOL_OV
            AND FIXACAO EQ WA_0053-FIXACAO
            AND TIPO EQ TIPO-FRETE
            AND ESTORNO EQ 0.

      IF SY-SUBRC IS NOT INITIAL.
        IF WA_0053-STATUS_ITM EQ 'F'.
          MESSAGE S000 WITH |{ D_STATUS-FRETE } - { I_0051-NRO_SOL_OV } - { WA_0053-FIXACAO }|.
          TRY.
              CALL METHOD OBJ_WEB_TX_CURVA->EXECUTAR
                EXPORTING
                  I_NUMERO  = I_0051-NRO_SOL_OV " Numero de Solicitação de Ordem de Venda
                  I_TIPO    = TIPO-FRETE        " Nº item do Tipo de Solicitação
                  I_FIXACAO = WA_0053-FIXACAO   " Nº item do documento de vendas e distribuição
                  I_STATUS  = WA_0053-STATUS_ITM
                  I_TCODE   = TCODE-ZS62.        " Código de transação atual

            CATCH ZCX_WEBSERVICE INTO CX_EXC.
              _MSG = CX_EXC->GET_TEXT( ).
              MESSAGE S000 WITH _MSG.
          ENDTRY.
        ENDIF.
      ELSE.
        IF WA_0053-STATUS_ITM IS INITIAL.
          MESSAGE S000 WITH |{ D_STATUS-FRETE } - { I_0051-NRO_SOL_OV } - { WA_0053-FIXACAO }|.
          TRY.

              CALL METHOD OBJ_WEB_TX_CURVA->EXECUTAR
                EXPORTING
                  I_NUMERO  = I_0051-NRO_SOL_OV   " Numero de Solicitação de Ordem de Venda
                  I_TIPO    = TIPO-FRETE          " Nº item do Tipo de Solicitação
                  I_FIXACAO = WA_0053-FIXACAO     " Nº item do documento de vendas e distribuição
                  I_STATUS  = WA_0053-STATUS_ITM
                  I_TCODE   = TCODE-ZS62.          "  Código de transação atual

            CATCH ZCX_WEBSERVICE INTO CX_EXC.
              _MSG = CX_EXC->GET_TEXT( ).
              MESSAGE S000 WITH _MSG.
          ENDTRY.
        ENDIF.
      ENDIF.

    ENDLOOP.

    MESSAGE S000 WITH |Hedge - Fim - { I_0051-NRO_SOL_OV }|.

  ENDMETHOD.


  METHOD SET_TP_VENDA_SIMPLES.

    CHECK I_0051-PARAM_ESPEC NE 'M'.  "// Parametro especial ne 'M'

    MESSAGE S000 WITH |Hedge - Inicio - { I_0051-NRO_SOL_OV }|.

    DATA(OBJ_WEB_TX_CURVA) = NEW ZCL_WEBSERVICE_TX_CURVA( ).

    SELECT COUNT(*)
      FROM ZSDT0094
        WHERE NRO_SOL_OV EQ I_0051-NRO_SOL_OV.

    IF SY-SUBRC IS INITIAL. "//ZSDT0094

      IF I_0051-STATUS EQ STATUS-LIBERADO. "// Status eq 'L'

        SELECT COUNT(*)
          FROM ZSDT0053
            WHERE NRO_SOL_OV EQ @I_0051-NRO_SOL_OV
              AND VBELN NE ' '.

        IF SY-SUBRC IS NOT INITIAL.
          "// Dispato quando não existit O.V
          MESSAGE S000 WITH |{ D_STATUS-EDICAO } - { I_0051-NRO_SOL_OV }|.
          TRY.
              OBJ_WEB_TX_CURVA->EXECUTAR( I_NUMERO = I_0051-NRO_SOL_OV
                                          I_TIPO   = TIPO-EDITAR
                                          I_TCODE  = TCODE-ZS62
                                         ).

              OBJ_WEB_TX_CURVA->EXECUTAR( I_NUMERO = I_0051-NRO_SOL_OV
                                          I_TIPO   = TIPO-LIBERAR
                                          I_TCODE  = TCODE-ZS62
                                         ).

            CATCH ZCX_WEBSERVICE INTO DATA(CX_EXC).
              DATA(_MSG) = CX_EXC->GET_TEXT( ).
              MESSAGE S000 WITH _MSG.
          ENDTRY.
        ELSE.

          SELECT COUNT(*)
            FROM ZSDT0053
              WHERE NRO_SOL_OV EQ I_0051-NRO_SOL_OV
                AND VBELN NE ' '
                AND STATUS EQ STATUS-ENCERRADO
                AND JOB EQ STATUS-ENCERRADO.

          IF SY-SUBRC IS INITIAL.
            MESSAGE S000 WITH |{ D_STATUS-ENCERRADO } - { I_0051-NRO_SOL_OV }|.
            TRY.
                OBJ_WEB_TX_CURVA->EXECUTAR( I_NUMERO = I_0051-NRO_SOL_OV
                                            I_TIPO   = TIPO-EDITAR
                                            I_TCODE  = TCODE-ZS62
                                           ).

                OBJ_WEB_TX_CURVA->EXECUTAR( I_NUMERO = I_0051-NRO_SOL_OV
                                            I_TIPO   = TIPO-ENCERRAMENTO
                                            I_TCODE  = TCODE-ZS62
                                          ).

              CATCH ZCX_WEBSERVICE INTO CX_EXC.
                _MSG = CX_EXC->GET_TEXT( ).
                MESSAGE S000 WITH _MSG.
            ENDTRY.
          ELSE.
            "// Alteração de Quantidade
            MESSAGE S000 WITH |{ D_STATUS-ALTERARQTD } - { I_0051-NRO_SOL_OV }|.
            TRY.
                OBJ_WEB_TX_CURVA->EXECUTAR( I_NUMERO = I_0051-NRO_SOL_OV
                                            I_TIPO   = TIPO-EDITAR "'EDI'
                                            I_UCOMM  = UCOMM-ZLI "'LIBERAR'
                                            I_TCODE  = TCODE-ZS62
                                          ).
              CATCH ZCX_WEBSERVICE INTO CX_EXC.
                _MSG = CX_EXC->GET_TEXT( ).
                MESSAGE S000 WITH _MSG.
            ENDTRY.
          ENDIF.

        ENDIF.
      ELSE. "// Status eq 'L'
        MESSAGE S000 WITH |{ D_STATUS-EDICAO } - { I_0051-NRO_SOL_OV }|.
        TRY.
            OBJ_WEB_TX_CURVA->EXECUTAR( I_NUMERO =  I_0051-NRO_SOL_OV
                                        I_TIPO   = TIPO-EDITAR
                                        I_TCODE  = TCODE-ZS62
                                       ).
          CATCH ZCX_WEBSERVICE INTO CX_EXC.
            _MSG = CX_EXC->GET_TEXT( ).
            MESSAGE S000 WITH _MSG.
        ENDTRY.
      ENDIF. "// Status eq 'L'

    ELSE.  "//ZSDT0094
      IF I_0051-STATUS EQ STATUS-LIBERADO.
        MESSAGE S000 WITH |{ D_STATUS-LIBERADO } - { I_0051-NRO_SOL_OV }|.
        TRY.
            OBJ_WEB_TX_CURVA->EXECUTAR( I_NUMERO =  I_0051-NRO_SOL_OV
                                        I_TIPO   = TIPO-LIBERAR
                                        I_TCODE  = TCODE-ZS62
                                       ).
          CATCH ZCX_WEBSERVICE INTO CX_EXC.
            _MSG = CX_EXC->GET_TEXT( ).
            MESSAGE S000 WITH _MSG.
        ENDTRY.
      ENDIF.
    ENDIF. "//ZSDT0094

    MESSAGE S000 WITH |Hedge - Fim - { I_0051-NRO_SOL_OV }|.

  ENDMETHOD.


  METHOD SET_VF11_DEVOLUCAO_COMPLEMENTO.

    DATA(OBJ_WEB_TX_CURVA) = NEW ZCL_WEBSERVICE_TX_CURVA( ).

    SY-TCODE = TCODE-VF11.

    SELECT SINGLE WAERK
      FROM ZSDT0051
      INTO @DATA(_MOEDA)
      WHERE NRO_SOL_OV EQ @I_0053-NRO_SOL_OV
      AND PARAM_ESPEC EQ 'M'.

    IF SY-SUBRC IS NOT INITIAL.

      TRY.
          OBJ_WEB_TX_CURVA->EXECUTAR(
                                      I_NUMERO = I_0053-NRO_SOL_OV
                                      I_TIPO   = TIPO-EDITAR
                                      I_TCODE  = TCODE-VF11
                                      I_VBELN  = I_0053-VBELN
                                     ).

        CATCH ZCX_WEBSERVICE INTO DATA(CX_EXC).
          DATA(_MSG) = CX_EXC->GET_TEXT( ).
          MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH _MSG.
      ENDTRY.

    ELSE.

      SELECT SINGLE AUART
        FROM VBAK
         INTO @DATA(VL_AUART)
         WHERE VBELN EQ @I_0053-VBELN.

      TRY.
          OBJ_WEB_TX_CURVA->EXECUTAR(
                                      I_NUMERO  = I_0053-NRO_SOL_OV
                                      I_TIPO    = TIPO-EDITARFRAME
                                      I_STATUS  = I_0053-STATUS
                                      I_FIXACAO = I_0053-FIXACAO
                                      I_TCODE   = TCODE-VF11
                                      I_VBELN   = I_0053-VBELN
                                      I_AUART   = VL_AUART
                                    ).
        CATCH ZCX_WEBSERVICE INTO CX_EXC.
          _MSG = CX_EXC->GET_TEXT( ).
          MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH _MSG.
      ENDTRY.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
