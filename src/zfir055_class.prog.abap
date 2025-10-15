*----------------------------------------------------------------------*
***INCLUDE ZFIR055_CLASS .
*----------------------------------------------------------------------*

*       CLASS ZCL_LOTEAMENTO DEFINITION
CLASS ZCL_LOTEAMENTO DEFINITION.
  PUBLIC SECTION.
    METHODS: BUSCA_CAMPOS,
             BUSCA_DADOS,
             INSERE_DADOS,
             ATUALIZA_DADOS,
             DELETA_DADOS,
             BLOCK_CAMPOS,
             SALVAR_DADOS,
             MONTAR_LAY_0104,
             CRIAR_ALV_0104,
             MONTAR_LAY_0102,
             CRIAR_ALV_0102,
             GERAR_PARCELAS,
             ELIMINAR_PARCELA,
             ESTORNAR_PARCELAS,
             ATUALIZA_VENDA,
             LOOP_SCREEN,
             GERAR_DOCUMENTOS,
             GERAR_DOC_CONTABIL,
             REFRESH_DOC_CONTABIL,
             ESTORNO_DOC_CONTABIL,
             GERAR_BOLETO,
             GERAR_JUROS,
             Z_RETORNA_STATUS_ZIB IMPORTING I_DOC_LCTO TYPE NUM10 I_ANO_LCTO TYPE NUM4
                                  EXPORTING E_ZIBCHV TYPE ZIB_CONTABIL_CHV E_ZIBERR TYPE ZIB_CONTABIL_ERR.


ENDCLASS.                    "ZCL_LOTEAMENTO DEFINITION

*       CLASS ZCL_LOTEAMENTO IMPLEMENTATION
CLASS ZCL_LOTEAMENTO IMPLEMENTATION.

  METHOD BUSCA_CAMPOS.

    DATA: WA_TOPO_AUX TYPE ZFIT0098,
          WA_EMPRESA  TYPE T001.

    CLEAR: WA_EMPRESA,  WA_TOPO-DES_EMPRESA,
           WA_TOPO_AUX, WA_TOPO-NOME_LOTEAMENTO.

    MOVE: WA_TOPO-LOTEAMENTO   TO WA_TERRENO-LOTEAMENTO,
          WA_TOPO-EMPRESA      TO WA_TERRENO-EMPRESA,
          WA_TOPO-NRO_TERRENO  TO WA_TERRENO-NRO_TERRENO,
          WA_TOPO-NRO_QUADRA   TO WA_TERRENO-NRO_QUADRA.

    SELECT SINGLE * FROM ZFIT0098 INTO WA_TOPO_AUX WHERE CODIGO EQ WA_TOPO-LOTEAMENTO.
    IF SY-SUBRC IS INITIAL.
      MOVE WA_TOPO_AUX-NOME_LOTEAMENTO TO WA_TOPO-NOME_LOTEAMENTO.
    ENDIF.

    IF WA_VENDA-DATA_VENDA IS INITIAL.
      MOVE SY-DATUM TO WA_VENDA-DATA_VENDA.

    ENDIF.

    SELECT SINGLE * FROM T001 INTO WA_EMPRESA WHERE BUKRS EQ WA_TOPO-EMPRESA.
    CHECK NOT WA_EMPRESA IS INITIAL.
    MOVE WA_EMPRESA-BUTXT TO WA_TOPO-DES_EMPRESA.

    CLEAR: WA_KNA1.
    SELECT SINGLE *
      INTO WA_KNA1
      FROM KNA1
     WHERE KUNNR = WA_VENDA-CLIENTE.

    VG_DESC_CLIENTE = WA_KNA1-NAME1.

  ENDMETHOD.                    "BUSCA_DADOS

  METHOD BUSCA_DADOS.

    DATA: WA_0099 TYPE ZFIT0099,
          WA_0100 TYPE ZFIT0100.

    CLEAR: WA_TERRENO, WA_VENDA.

    SELECT SINGLE * FROM ZFIT0099 INTO WA_0099
      WHERE EMPRESA     EQ WA_TOPO-EMPRESA
        AND LOTEAMENTO  EQ WA_TOPO-LOTEAMENTO
        AND NRO_TERRENO EQ WA_TOPO-NRO_TERRENO
        AND NRO_QUADRA  EQ WA_TOPO-NRO_QUADRA.

    CHECK SY-SUBRC EQ 0.

    MOVE-CORRESPONDING WA_0099 TO WA_TERRENO.

    CHECK NOT WA_TERRENO IS INITIAL.

    SELECT SINGLE * FROM ZFIT0100 INTO WA_0100
      WHERE EMPRESA     EQ WA_TERRENO-EMPRESA
        AND LOTEAMENTO  EQ WA_TERRENO-LOTEAMENTO
        AND NRO_TERRENO EQ WA_TERRENO-NRO_TERRENO
        AND NRO_QUADRA  EQ WA_TERRENO-NRO_QUADRA.

    MOVE-CORRESPONDING WA_0099 TO WA_VENDA.
    MOVE-CORRESPONDING WA_0100 TO WA_VENDA.


  ENDMETHOD.                    "BUSCA_DADOS

  METHOD INSERE_DADOS.
    FREE: WA_TOPO, WA_TERRENO, WA_VENDA, IT_CR.
    PERFORM LIMPA_DADOS.
    "ME->BLOCK_CAMPOS( ).
  ENDMETHOD.                    "INSERE_DADOS

  METHOD ATUALIZA_DADOS.
    CHECK NOT WA_TERRENO IS INITIAL.
    "ME->BLOCK_CAMPOS( ).
  ENDMETHOD.                    "ATUALIZA_DADOS

  METHOD DELETA_DADOS.

    "ME->BLOCK_CAMPOS( ).
    SELECT SINGLE *
      INTO WA_ZFIT0101
      FROM ZFIT0101
     WHERE EMPRESA     = WA_TERRENO-EMPRESA
       AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA.

    IF SY-SUBRC = 0.
      MESSAGE 'Já existe parcelas lançadas! Operação não permitida!' TYPE 'S'.
      CLEAR ACAO.
      EXIT.
    ENDIF.

    DELETE FROM ZFIT0099
      WHERE EMPRESA     EQ WA_TERRENO-EMPRESA
        AND LOTEAMENTO  EQ WA_TERRENO-LOTEAMENTO
        AND NRO_TERRENO EQ WA_TERRENO-NRO_TERRENO
        AND NRO_QUADRA  EQ WA_TERRENO-NRO_QUADRA.

    IF SY-SUBRC NE 0.
      CLEAR ACAO.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao excluir os dados!' TYPE 'S'.
      EXIT.
    ENDIF.

    DELETE FROM ZFIT0100
      WHERE EMPRESA     EQ WA_TERRENO-EMPRESA
        AND LOTEAMENTO  EQ WA_TERRENO-LOTEAMENTO
        AND NRO_TERRENO EQ WA_TERRENO-NRO_TERRENO
        AND NRO_QUADRA  EQ WA_TERRENO-NRO_QUADRA.

    DELETE FROM ZFIT0104
      WHERE EMPRESA     EQ WA_TERRENO-EMPRESA
        AND LOTEAMENTO  EQ WA_TERRENO-LOTEAMENTO
        AND NRO_TERRENO EQ WA_TERRENO-NRO_TERRENO
        AND NRO_QUADRA  EQ WA_TERRENO-NRO_QUADRA.

    COMMIT WORK.

    PERFORM LIMPA_DADOS.

    MESSAGE 'Exclusão efetuada com sucesso!' TYPE 'S'.


  ENDMETHOD.                    "DELETA_DADOS

  METHOD BLOCK_CAMPOS.

    CASE ACAO.
      WHEN 'NEW' OR 'EDI'.

        "IF ACAO EQ 'EDI' AND WA_TERRENO IS INITIAL.
        "  ACAO = 'SAVE'.
        "ELSE.
        "  ME->LOOP_SCREEN( ).
        "ENDIF.

    ENDCASE.

  ENDMETHOD.                    "BLOCK_CAMPOS

  METHOD SALVAR_DADOS.

    CASE ACAO.
      WHEN 'NEW'.
        IF WA_TOPO-NRO_QUADRA IS INITIAL.
          MESSAGE 'Informe o número da Quadra!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF WA_TOPO-NRO_TERRENO IS INITIAL.
          MESSAGE 'Informe o número do Terreno!' TYPE 'S'.
          EXIT.
        ENDIF.

        MOVE-CORRESPONDING WA_TOPO TO WA_TERRENO.
        CALL METHOD OBJ_CUSTOM_EDITOR->GET_TEXT_AS_STREAM( IMPORTING TEXT = GT_EDITOR ).
        READ TABLE GT_EDITOR INTO GS_EDITOR INDEX 1.
        WA_TERRENO-OBSERVACOES = GS_EDITOR-LINE.

        "Vericação Registro já existe.
        CLEAR: WA_ZFIT0099.
        SELECT SINGLE *
          INTO WA_ZFIT0099
          FROM ZFIT0099
         WHERE EMPRESA     = WA_TERRENO-EMPRESA
           AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
           AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
           AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA.

        IF SY-SUBRC = 0.
          MESSAGE 'Dados do terreno já existente! Operação não permitida!' TYPE 'S'.
          EXIT.
        ENDIF.

        WA_TERRENO-USUARIO_REGISTRO = SY-UNAME.
        WA_TERRENO-DATA_REGISTRO    = SY-DATUM.
        WA_TERRENO-HORA_REGISTRO    = SY-UZEIT.

        CLEAR: WA_TERRENO-VALOR_TOTAL.
        WA_TERRENO-VALOR_TOTAL = WA_TERRENO-VALOR + WA_TERRENO-VALOR_AGREGADO.

        INSERT ZFIT0099 FROM WA_TERRENO.

        MOVE-CORRESPONDING WA_TERRENO TO WA_VENDA.
        INSERT ZFIT0100 FROM WA_VENDA.

        PERFORM GRAVAR_EVENTOS_VENDA.

      WHEN 'EDI'.
        CHECK NOT WA_TERRENO IS INITIAL.

        IF WA_TOPO-NRO_QUADRA IS INITIAL.
          MESSAGE 'Informe o número da Quadra!' TYPE 'S'.
          EXIT.
        ENDIF.

        IF WA_TOPO-NRO_TERRENO IS INITIAL.
          MESSAGE 'Informe o número do Terreno!' TYPE 'S'.
          EXIT.
        ENDIF.

        MOVE-CORRESPONDING WA_TOPO TO WA_TERRENO.
        CALL METHOD OBJ_CUSTOM_EDITOR->GET_TEXT_AS_STREAM( IMPORTING TEXT = GT_EDITOR ).
        READ TABLE GT_EDITOR INTO GS_EDITOR INDEX 1.
        WA_TERRENO-OBSERVACOES = GS_EDITOR-LINE.

        CLEAR: WA_TERRENO-VALOR_TOTAL.
        WA_TERRENO-VALOR_TOTAL = WA_TERRENO-VALOR + WA_TERRENO-VALOR_AGREGADO.

        MODIFY ZFIT0099 FROM WA_TERRENO.

        SELECT SINGLE *
          INTO WA_ZFIT0101
          FROM ZFIT0101
         WHERE EMPRESA     = WA_TERRENO-EMPRESA
           AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
           AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
           AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA.

        IF SY-SUBRC NE 0.
          MODIFY ZFIT0100 FROM WA_VENDA.
        ENDIF.

        PERFORM GRAVAR_EVENTOS_VENDA.

      WHEN OTHERS.
        PERFORM GRAVAR_EVENTOS_VENDA.

    ENDCASE.

    CLEAR: WA_KNA1.
    SELECT SINGLE *
      INTO WA_KNA1
      FROM KNA1
     WHERE KUNNR = WA_VENDA-CLIENTE.

    VG_DESC_CLIENTE = WA_KNA1-NAME1.

    MOVE 'SAVE' TO ACAO.
    "ME->BLOCK_CAMPOS( ).
  ENDMETHOD.                    "SALVAR_DADOS

  METHOD CRIAR_ALV_0104.

    FREE: IT_FCAT, WA_FCAT.

    ME->MONTAR_LAY_0104( ).

    DEFINE ALV_0104.

      WA_FCAT-HOTSPOT   = &1.
      WA_FCAT-REF_TABLE = &2.
      WA_FCAT-REF_FIELD = &3.
      WA_FCAT-TABNAME   = &4.
      WA_FCAT-FIELDNAME = &5.
      WA_FCAT-SCRTEXT_L = &6.
      WA_FCAT-SCRTEXT_M = &6.
      WA_FCAT-SCRTEXT_S = &6.
      WA_FCAT-COLTEXT   = &6.
      WA_FCAT-REPTEXT   = &6.
      WA_FCAT-NO_ZERO   = &7.
      WA_FCAT-OUTPUTLEN = &8.
      WA_FCAT-EDIT      = &9.
      WA_FCAT-JUST      = ''.

      IF ( &5 = 'STATUS'      ) OR
         ( &5 = 'NRO_PARCELA' ).
        WA_FCAT-JUST = 'C'.
      ENDIF.

      APPEND WA_FCAT TO IT_FCAT.
      CLEAR WA_FCAT.
    END-OF-DEFINITION.

    ALV_0104:
          ''   ''         ''                 'IT_CR' 'NRO_PARCELA'       'Nro Parcela'      ' '  '11'  ' ',
          ''   ''         ''                 'IT_CR' 'TIPO'              'Tipo'             ' '  '05'  ' ',
          ''   ''         ''                 'IT_CR' 'STATUS'            'Status'           ' '  '06'  ' ',
          ''   ''         ''                 'IT_CR' 'DT_VENCIMENTO'     'Dt. Vencimento'   ' '  '14'  'X',
          ''   'ZFIT0101' 'VALOR'            'IT_CR' 'VALOR'             'Valor Venda R$'   ' '  '15'  'X',
          'X'  ''         ''                 'IT_CR' 'DOC_CONTABIL'      'Doc. Contabil'    ' '  '15'  ' ',
          'X'  ''         ''                 'IT_CR' 'DOC_COMPENSACAO'   'Doc. Compensação' ' '  '16'  ' ',
          ''   ''         ''                 'IT_CR' 'DT_LIQUIDACAO'     'Dt. Liquidação'   ' '  '13'  ' ',
          'X'  ''         ''                 'IT_CR' 'LOTE'              'Lote ZGL'         ' '  '08'  ' ',
          'X'  ''         ''                 'IT_CR' 'DOC_LCTO'          'Doc. Lcto ZGL'    ' '  '15'  ' '.


  ENDMETHOD.                    "CRIAR_ALV_0104

  METHOD MONTAR_LAY_0104.

    CLEAR: WA_LAYOUT, WA_VARIANTE.

    WA_LAYOUT-ZEBRA      = ABAP_TRUE.
    "WA_LAYOUT-NO_ROWINS  = ABAP_TRUE.
    WA_LAYOUT-STYLEFNAME = 'FIELD_STYLE'.
    "WA_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
    "WA_LAYOUT-SEL_MODE   = 'C'.
    "WA_STABLE-ROW        = ABAP_TRUE.

    WA_VARIANTE-REPORT  = SY-REPID.

  ENDMETHOD.                    "MONTAR_LAY


  METHOD CRIAR_ALV_0102.

    FREE: IT_FCAT, WA_FCAT.

    ME->MONTAR_LAY_0102( ).

    DEFINE ALV_0102.

      WA_FCAT-HOTSPOT   = &1.
      WA_FCAT-REF_TABLE = &2.
      WA_FCAT-REF_FIELD = &3.
      WA_FCAT-TABNAME   = &4.
      WA_FCAT-FIELDNAME = &5.
      WA_FCAT-SCRTEXT_L = &6.
      WA_FCAT-SCRTEXT_M = &6.
      WA_FCAT-SCRTEXT_S = &6.
      WA_FCAT-COLTEXT   = &6.
      WA_FCAT-REPTEXT   = &6.
      WA_FCAT-NO_ZERO   = &7.
      WA_FCAT-OUTPUTLEN = &8.
      WA_FCAT-EDIT      = &9.
      WA_FCAT-JUST       = ''.
      WA_FCAT-F4AVAILABL = ''.

      IF ( &5 = 'COD_EV' ).
        WA_FCAT-F4AVAILABL = 'X'.
      ENDIF.

      APPEND WA_FCAT TO IT_FCAT.
      CLEAR WA_FCAT.
    END-OF-DEFINITION.

    ALV_0102:
          ''   'ZFIT0102' 'COD_EV'           'IT_EVENTOS_VENDA' 'COD_EV'            'Evento'           ' '  '06'  'X',
          ''   ''         ''                 'IT_EVENTOS_VENDA' 'NOME_EVENTO'       'Nome'             ' '  '35'  ' ',
          ''   'ZFIT0104' 'VALOR'            'IT_EVENTOS_VENDA' 'VALOR'             'Valor R$'         ' '  '14'  'X',
          ''   'ZFIT0104' 'COND_PGTO'        'IT_EVENTOS_VENDA' 'COND_PGTO'         'Cond. Pag.'       ' '  '10'  'X',
          ''   'ZFIT0104' 'FORMA_PGTO'       'IT_EVENTOS_VENDA' 'FORMA_PGTO'        'Forma Pag.'       ' '  '10'  'X',
          ''   'ZFIT0104' 'QTD_PARCELAS'     'IT_EVENTOS_VENDA' 'QTD_PARCELAS'      'Qte. Parcelas'    ' '  '13'  'X',
          ''   'ZFIT0104' 'DATA_VENC'        'IT_EVENTOS_VENDA' 'DATA_VENC'         'Data 1ª Parcela'  ' '  '15'  'X'.
*

  ENDMETHOD.                    "CRIAR_ALV_0104

  METHOD MONTAR_LAY_0102.

    CLEAR: WA_LAYOUT, WA_VARIANTE.

    WA_LAYOUT-ZEBRA      = ABAP_TRUE.
    "WA_LAYOUT-NO_ROWINS  = ABAP_TRUE.
    WA_LAYOUT-STYLEFNAME = 'FIELD_STYLE'.
*    WA_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*    WA_LAYOUT-SEL_MODE   = 'C'.
*    WA_STABLE-ROW        = ABAP_TRUE.

    WA_VARIANTE-REPORT  = SY-REPID.

  ENDMETHOD.                    "MONTAR_LAY



  METHOD GERAR_PARCELAS.

    DATA: DATA_INI        TYPE SY-DATUM,
          CONT            TYPE I VALUE 0,
          VAR_ANSWER      TYPE C,
          VL_COD_EV       TYPE ZFIT0104-COD_EV,
          VL_VALOR_TOT    TYPE DMBTR,
          VL_VALOR_DIF    TYPE DMBTR,
          VL_VALOR_EVENTO TYPE ZFIT0104-VALOR.

    FREE IT_CR.

    CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

    CALL METHOD OBJ_ALV_EV_VENDA->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    CHECK IT_SEL_ROWS IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = 'Confirmação'
        TEXT_QUESTION         = 'Confirma geração das parcelas?'
        TEXT_BUTTON_1         = 'Sim'
        TEXT_BUTTON_2         = 'Não'
        DEFAULT_BUTTON        = '1'
        DISPLAY_CANCEL_BUTTON = ''
      IMPORTING
        ANSWER                = VAR_ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND        = 1
        OTHERS                = 2.

    CHECK VAR_ANSWER EQ '1'.


    LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

      READ TABLE IT_EVENTOS_VENDA INTO WA_EVENTOS_VENDA INDEX WA_SEL_ROWS-INDEX.

      CLEAR: VL_VALOR_TOT, CONT, VL_COD_EV.

      DATA_INI  =  WA_EVENTOS_VENDA-DATA_VENC.
      VL_COD_EV =  WA_EVENTOS_VENDA-COD_EV.

      IF VL_COD_EV IS INITIAL.
        ROLLBACK WORK.
        MESSAGE 'Evento não encontrado para a Venda!' TYPE 'S'.
        RETURN.
      ENDIF.

      IF WA_EVENTOS_VENDA-TIPO IS INITIAL.
        ROLLBACK WORK.
        MESSAGE 'Tipo não encontrado para a Venda!' TYPE 'S'.
        RETURN.
      ENDIF.

      IF WA_EVENTOS_VENDA-TP_LCTO IS INITIAL.
        ROLLBACK WORK.
        MESSAGE 'Tipo de Lcto não encontrado para a Venda!' TYPE 'S'.
        RETURN.
      ENDIF.

      CLEAR: WA_ZFIT0101.
      SELECT SINGLE *
        INTO WA_ZFIT0101
        FROM ZFIT0101
       WHERE EMPRESA     = WA_TERRENO-EMPRESA
         AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
         AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
         AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA
         AND COD_EV      = WA_EVENTOS_VENDA-COD_EV.

      IF SY-SUBRC = 0.

        OBJ_LOTEAMENTO->ATUALIZA_VENDA( ).

        MESSAGE 'Parcelas já foram geradas!' TYPE 'S'.
        RETURN.
      ENDIF.


      VL_VALOR_EVENTO =  WA_EVENTOS_VENDA-VALOR.

      WHILE WA_EVENTOS_VENDA-QTD_PARCELAS GT CONT.

        CONT = CONT + 1.

        WA_CR-NRO_PARCELA     = CONT.
        WA_CR-COD_EV          = WA_EVENTOS_VENDA-COD_EV.
        WA_CR-TIPO            = WA_EVENTOS_VENDA-TIPO.
        WA_CR-TP_LCTO         = WA_EVENTOS_VENDA-TP_LCTO.
        WA_CR-DT_VENCIMENTO   = DATA_INI.
        WA_CR-VALOR           = VL_VALOR_EVENTO / WA_EVENTOS_VENDA-QTD_PARCELAS.
        WA_CR-DOC_CONTABIL    = ''.
        WA_CR-DOC_COMPENSACAO = ''.
        WA_CR-DT_LIQUIDACAO   = ''.
        ADD WA_CR-VALOR TO VL_VALOR_TOT.

        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            DATE      = DATA_INI
            DAYS      = '30'
            MONTHS    = '00'
            SIGNUM    = '+'
            YEARS     = '00'
          IMPORTING
            CALC_DATE = DATA_INI.

        APPEND WA_CR TO IT_CR.

        CLEAR WA_CR.

      ENDWHILE.

      VL_VALOR_DIF = VL_VALOR_EVENTO - VL_VALOR_TOT.

      IF ( VL_VALOR_DIF NE 0 ).
        READ TABLE IT_CR INTO WA_CR WITH KEY COD_EV = VL_COD_EV.
        WA_CR-VALOR = WA_CR-VALOR + VL_VALOR_DIF.
        MODIFY IT_CR FROM WA_CR INDEX 1.
      ENDIF.

      MOVE-CORRESPONDING WA_EVENTOS_VENDA TO WA_ZFIT0104.

      MOVE WA_TERRENO-EMPRESA     TO WA_ZFIT0104-EMPRESA.
      MOVE WA_TERRENO-LOTEAMENTO  TO WA_ZFIT0104-LOTEAMENTO.
      MOVE WA_TERRENO-NRO_TERRENO TO WA_ZFIT0104-NRO_TERRENO.
      MOVE WA_TERRENO-NRO_QUADRA  TO WA_ZFIT0104-NRO_QUADRA.

      MODIFY ZFIT0104 FROM WA_ZFIT0104.

      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gerar a(s) parcela(s)!' TYPE 'S'.
        EXIT.
      ENDIF.

      CLEAR: WA_EVENTOS_VENDA, WA_ZFIT0104.

    ENDLOOP.

    "Grava Parcela ZFIT0101
    LOOP AT IT_CR INTO WA_CR.

      CLEAR: WA_ZFIT0101.
      MOVE WA_TERRENO-EMPRESA     TO WA_ZFIT0101-EMPRESA.
      MOVE WA_TERRENO-LOTEAMENTO  TO WA_ZFIT0101-LOTEAMENTO.
      MOVE WA_TERRENO-NRO_TERRENO TO WA_ZFIT0101-NRO_TERRENO.
      MOVE WA_TERRENO-NRO_QUADRA  TO WA_ZFIT0101-NRO_QUADRA.
      MOVE WA_CR-NRO_PARCELA      TO WA_ZFIT0101-NRO_PARC.
      MOVE WA_CR-DT_VENCIMENTO    TO WA_ZFIT0101-DATA_VENC.
      MOVE WA_CR-VALOR            TO WA_ZFIT0101-VALOR.
      MOVE WA_CR-TIPO             TO WA_ZFIT0101-TIPO.
      MOVE WA_CR-COD_EV           TO WA_ZFIT0101-COD_EV.
      MOVE WA_CR-TP_LCTO          TO WA_ZFIT0101-TP_LCTO.

      "Atribui numero Sequencial para Parcela que irá compor Codigo Barras Boleto.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR                   = '01'
          OBJECT                        = 'ZPARC_LOT'
        IMPORTING
          NUMBER                        = WA_ZFIT0101-NUM_PARC_SEQ
                                  .
      IF ( SY-SUBRC <> 0 ) OR ( WA_ZFIT0101-NUM_PARC_SEQ IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE 'Não foi possivel determinar o número sequencial da Parcela!' TYPE 'S'.
        RETURN.
      ENDIF.

      MODIFY ZFIT0101 FROM WA_ZFIT0101.

      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gerar a(s) parcela(s)!' TYPE 'S'.
        EXIT.
      ENDIF.

    ENDLOOP.

    "CALL METHOD OBJ_ALV_EV_VENDA->SET_READY_FOR_INPUT
    "  EXPORTING
    "    I_READY_FOR_INPUT = 0.

    COMMIT WORK.

    OBJ_LOTEAMENTO->ATUALIZA_VENDA( ).

    MESSAGE 'Geração da(s) parcela(s) concluída com sucesso!' TYPE 'S'.


  ENDMETHOD.                    "GERAR_PARCELAS

  METHOD ELIMINAR_PARCELA.

    DATA VAR_ANSWER TYPE C.

    FIELD-SYMBOLS: <SAIDA> TYPE TY_CONTAS_RECEBER.

    CALL METHOD WA_ALV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    CHECK NOT IT_SEL_ROWS IS INITIAL.

    IF ( LINES( IT_SEL_ROWS ) NE 1 ).
      MESSAGE S836(SD) WITH TEXT-007.
      EXIT.
    ENDIF.

    READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.
    READ TABLE IT_CR ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

    IF <SAIDA>-TIPO NE 'JR'.
      MESSAGE S836(SD) WITH TEXT-011.
      RETURN.
    ENDIF.

    IF <SAIDA>-DOC_CONTABIL IS NOT INITIAL.
      MESSAGE S836(SD) WITH TEXT-010.
      RETURN.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = 'Confirmação'
        TEXT_QUESTION         = 'Confirma exclusão da parcela?'
        TEXT_BUTTON_1         = 'Sim'
        TEXT_BUTTON_2         = 'Não'
        DEFAULT_BUTTON        = '1'
        DISPLAY_CANCEL_BUTTON = ''
      IMPORTING
        ANSWER                = VAR_ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND        = 1
        OTHERS                = 2.

    CHECK VAR_ANSWER EQ '1'.

    CLEAR: WA_ZFIT0101.
    SELECT SINGLE *
      INTO WA_ZFIT0101
      FROM ZFIT0101
     WHERE EMPRESA     = WA_TERRENO-EMPRESA
       AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA
       AND NRO_PARC    = <SAIDA>-NRO_PARCELA
       AND TIPO        = <SAIDA>-TIPO
       AND BELNR       = ''.

    IF SY-SUBRC = 0.
      DELETE ZFIT0101 FROM WA_ZFIT0101.

      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao estornar a(s) parcela(s)!' TYPE 'S'.
        RETURN.
      ENDIF.

    ELSE.
      ROLLBACK WORK.
      MESSAGE 'Não foi posssivel eliminar a parcela!' TYPE 'S'.
      RETURN.
    ENDIF.

    COMMIT WORK.

    MESSAGE 'Parcela eliminada com sucesso!' TYPE 'S'.

    OBJ_LOTEAMENTO->ATUALIZA_VENDA( ).

    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "ELIMINAR_PARCELAS


   METHOD ESTORNAR_PARCELAS.

    DATA: VAR_ANSWER    TYPE C.

    CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

    CALL METHOD OBJ_ALV_EV_VENDA->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    CHECK IT_SEL_ROWS IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = 'Confirmação'
        TEXT_QUESTION         = 'Confirma exclusão das parcelas?'
        TEXT_BUTTON_1         = 'Sim'
        TEXT_BUTTON_2         = 'Não'
        DEFAULT_BUTTON        = '1'
        DISPLAY_CANCEL_BUTTON = ''
      IMPORTING
        ANSWER                = VAR_ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND        = 1
        OTHERS                = 2.

    CHECK VAR_ANSWER EQ '1'.

    OBJ_LOTEAMENTO->REFRESH_DOC_CONTABIL( ).

    LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.

      READ TABLE IT_EVENTOS_VENDA INTO WA_EVENTOS_VENDA INDEX WA_SEL_ROWS-INDEX.

      CLEAR: WA_ZFIT0101.
      SELECT SINGLE *
        INTO WA_ZFIT0101
        FROM ZFIT0101
       WHERE EMPRESA     EQ WA_VENDA-EMPRESA
         AND LOTEAMENTO  EQ WA_VENDA-LOTEAMENTO
         AND NRO_TERRENO EQ WA_VENDA-NRO_TERRENO
         AND NRO_QUADRA  EQ WA_VENDA-NRO_QUADRA
         AND COD_EV      EQ WA_EVENTOS_VENDA-COD_EV
         AND BELNR       NE ''.

      IF SY-SUBRC = 0.
        ROLLBACK WORK.
        MESSAGE 'Já existe parcelas com movimentação contabil! Operação não permitida!' TYPE 'S'.
        RETURN.
      ENDIF.

      CLEAR: WA_ZFIT0101.
      SELECT SINGLE *
        INTO WA_ZFIT0101
        FROM ZFIT0101
       WHERE EMPRESA     EQ WA_VENDA-EMPRESA
         AND LOTEAMENTO  EQ WA_VENDA-LOTEAMENTO
         AND NRO_TERRENO EQ WA_VENDA-NRO_TERRENO
         AND NRO_QUADRA  EQ WA_VENDA-NRO_QUADRA
         AND COD_EV      EQ WA_EVENTOS_VENDA-COD_EV
         AND STATUS      EQ '@B4@'  "Gerado Doc. Lcto e sem retorno(Sucesso/Erro) da geração do Doc. Contabil
         AND ( ( DOC_LCTO NE '' ) OR
               ( LOTE     NE '' ) ).

      IF SY-SUBRC = 0.
        ROLLBACK WORK.
        MESSAGE 'Já existe parcelas com movimentação contabil em processamento! Operação não permitida!' TYPE 'S'.
        RETURN.
      ENDIF.

      REFRESH: IT_ZFIT0101.
      SELECT *
        FROM ZFIT0101
        INTO TABLE IT_ZFIT0101
       WHERE EMPRESA     EQ WA_VENDA-EMPRESA
         AND LOTEAMENTO  EQ WA_VENDA-LOTEAMENTO
         AND NRO_TERRENO EQ WA_VENDA-NRO_TERRENO
         AND NRO_QUADRA  EQ WA_VENDA-NRO_QUADRA
         AND COD_EV      EQ WA_EVENTOS_VENDA-COD_EV
         AND BELNR       EQ ''.

      LOOP AT IT_ZFIT0101 INTO WA_ZFIT0101.

        DELETE ZFIT0101 FROM WA_ZFIT0101.

        IF SY-SUBRC NE 0.
          ROLLBACK WORK.
          MESSAGE 'Houve um erro ao estornar a(s) parcela(s)!' TYPE 'S'.
          RETURN.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    OBJ_LOTEAMENTO->ATUALIZA_VENDA( ).

    MESSAGE 'Estorno concluído com sucesso!' TYPE 'S'.

  ENDMETHOD.                    "ESTORNAR_PARCELAS

  METHOD ATUALIZA_VENDA.

    DATA: WA_BKPF TYPE BKPF.

    CLEAR: VG_MODIFY_PARC.
    REFRESH: IT_ZFIT0101, IT_CR.

    SELECT *
      FROM ZFIT0101
      INTO TABLE IT_ZFIT0101
     WHERE EMPRESA     = WA_TERRENO-EMPRESA
       AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA.

    LOOP AT IT_ZFIT0101 INTO WA_ZFIT0101.

      IF ( WA_ZFIT0101-BELNR IS NOT INITIAL ).

        CLEAR: WA_BKPF.
        SELECT SINGLE *
          INTO WA_BKPF
          FROM BKPF
         WHERE BELNR = WA_ZFIT0101-BELNR
           AND BUKRS = WA_ZFIT0101-EMPRESA
           AND STGRD NE ''.

        IF SY-SUBRC = 0.
          CONTINUE.
        ENDIF.

      ENDIF.

      WA_CR-NRO_PARCELA     = WA_ZFIT0101-NRO_PARC.
      WA_CR-TIPO            = WA_ZFIT0101-TIPO.
      WA_CR-DT_VENCIMENTO   = WA_ZFIT0101-DATA_VENC.
      WA_CR-VALOR           = WA_ZFIT0101-VALOR.
      WA_CR-DOC_CONTABIL    = WA_ZFIT0101-BELNR.
      WA_CR-STATUS          = WA_ZFIT0101-STATUS.
      WA_CR-LOTE            = WA_ZFIT0101-LOTE.
      WA_CR-DOC_LCTO        = WA_ZFIT0101-DOC_LCTO.
      WA_CR-EMPRESA         = WA_ZFIT0101-EMPRESA.
      WA_CR-DT_LCTO_CTB     = WA_ZFIT0101-DT_LCTO_CTB.
      WA_CR-COD_EV          = WA_ZFIT0101-COD_EV.
      WA_CR-TP_LCTO         = WA_ZFIT0101-TP_LCTO.
      WA_CR-NUM_PARC_SEQ    = WA_ZFIT0101-NUM_PARC_SEQ.

      CLEAR: WA_BSAD, WA_CR-DOC_COMPENSACAO, WA_CR-DT_LIQUIDACAO.
      SELECT SINGLE *
        INTO WA_BSAD
        FROM BSAD
       WHERE BELNR = WA_CR-DOC_CONTABIL
         AND BUKRS = WA_VENDA-EMPRESA.

      IF SY-SUBRC = 0.
        WA_CR-DT_LIQUIDACAO   = WA_BSAD-AUGDT.
        WA_CR-DOC_COMPENSACAO = WA_BSAD-AUGBL.
      ENDIF.

      APPEND WA_CR TO IT_CR.
      CLEAR WA_CR.

    ENDLOOP.

    SORT: IT_CR BY TIPO.

    REFRESH: IT_ZFIT0104, IT_EVENTOS_VENDA.
    SELECT *
      FROM ZFIT0104
      INTO TABLE IT_ZFIT0104
     WHERE EMPRESA     = WA_TERRENO-EMPRESA
       AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA.

    LOOP AT IT_ZFIT0104 INTO WA_ZFIT0104.
      MOVE-CORRESPONDING WA_ZFIT0104 TO WA_EVENTOS_VENDA.

      CLEAR: WA_ZFIT0102.
      SELECT SINGLE *
        INTO WA_ZFIT0102
        FROM ZFIT0102
       WHERE COD_EV = WA_EVENTOS_VENDA-COD_EV.

      WA_EVENTOS_VENDA-NOME_EVENTO = WA_ZFIT0102-NOME_EVENTO.
      WA_EVENTOS_VENDA-TIPO        = WA_ZFIT0102-TIPO.
      WA_EVENTOS_VENDA-TP_LCTO     = WA_ZFIT0102-TP_LCTO.

      APPEND WA_EVENTOS_VENDA TO IT_EVENTOS_VENDA.
    ENDLOOP.

    PERFORM CHANGE_ROWS USING ''.

  ENDMETHOD.                    "ATUALIZA_VENDA



  METHOD LOOP_SCREEN.
    LOOP AT SCREEN.

      CASE ACAO.
        WHEN 'VIEW'.

          CASE SCREEN-NAME.

            WHEN 'WA_TOPO-EMPRESA'     OR
                 'WA_TOPO-LOTEAMENTO'  OR
                 'WA_TOPO-NRO_QUADRA'  OR
                 'WA_TOPO-NRO_TERRENO' OR
                 'BTN_PESQ_QD'.

              SCREEN-INPUT = 1.
              MODIFY SCREEN.

            WHEN 'TAB_TERRENO' OR
                 'TAB_VENDA' OR
                 'TAB_C_R' OR
                 'TAB_LOTEAMENTO'.

            WHEN OTHERS.

              SCREEN-INPUT = 0.
              MODIFY SCREEN.

          ENDCASE.

        WHEN 'NEW'.

          CASE SCREEN-NAME.

            WHEN 'WA_TERRENO-USUARIO_REGISTRO'  OR
                 'WA_TERRENO-DATA_REGISTRO' OR
                 'WA_TERRENO-HORA_REGISTRO' OR
                 'WA_TERRENO-VALOR_TOTAL'.

               SCREEN-INPUT = 0.
               MODIFY SCREEN.

            WHEN 'TAB_TERRENO' OR
                 'TAB_VENDA' OR
                 'TAB_C_R' OR
                 'TAB_LOTEAMENTO'.

            WHEN OTHERS.

              SCREEN-INPUT = 1.
              MODIFY SCREEN.

          ENDCASE.

        WHEN 'EDI'.

           CASE SCREEN-NAME.

             WHEN 'WA_TOPO-EMPRESA'     OR
                  'WA_TOPO-LOTEAMENTO'  OR
                  'WA_TOPO-NRO_QUADRA'  OR
                  'WA_TOPO-NRO_TERRENO'.

               SCREEN-INPUT = 0.
               MODIFY SCREEN.

             WHEN 'TAB_TERRENO' OR
                  'TAB_VENDA' OR
                  'TAB_C_R' OR
                  'TAB_LOTEAMENTO'.

             WHEN 'WA_VENDA-VALOR_VENDA' OR
                  'WA_VENDA-COND_PGTO' OR
                  'WA_VENDA-QTD_PARCELAS' OR
                  'WA_VENDA-CLIENTE' OR
                  'WA_VENDA-DATA_VENDA' OR
                  'WA_VENDA-FORMA_PGTO' OR
                  'WA_VENDA-DATA_PARCELA_INI'.

              "Caso exista Parcelas geradas, bloqueia dados da específico de Parcelamento.
              IF WA_TERRENO IS NOT INITIAL.

                SELECT SINGLE *
                  INTO WA_ZFIT0101
                  FROM ZFIT0101
                 WHERE EMPRESA     = WA_TERRENO-EMPRESA
                   AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
                   AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
                   AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA.

                IF SY-SUBRC = 0.
                  SCREEN-INPUT = 0.
                  MODIFY SCREEN.
                ELSE.
                  SCREEN-INPUT = 1.
                  MODIFY SCREEN.
                ENDIF.

              ELSE.
                SCREEN-INPUT = 1.
                MODIFY SCREEN.
              ENDIF.

           ENDCASE.

        WHEN OTHERS.

          CASE SCREEN-NAME.

            WHEN 'WA_TERRENO-USUARIO_REGISTRO'  OR
                 'WA_TERRENO-DATA_REGISTRO' OR
                 'WA_TERRENO-HORA_REGISTRO' OR
                 'WA_TERRENO-VALOR_TOTAL'.

              SCREEN-INPUT = 0.
              MODIFY SCREEN.

            WHEN 'TAB_TERRENO' OR
                 'TAB_VENDA' OR
                 'TAB_C_R' OR
                 'TAB_LOTEAMENTO'.


            WHEN OTHERS.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
          ENDCASE.

       ENDCASE.

    ENDLOOP.
  ENDMETHOD.                    "LOOP_SCREEN

  METHOD GERAR_DOCUMENTOS.

*    WRITE SY-DATUM TO WL_DATA.
*    IF WA_VENDA-COND_PGTO EQ 'V'.
*      WRITE  WA_VENDA-VALOR_VENDA TO WL_VALOR.
*      CONDENSE WL_VALOR NO-GAPS.
*    ENDIF.
**wl_ZLSCH = 'D'.
*    CONCATENATE 'Lot.' WA_TERRENO-MATRICULA INTO WL_BKTXT.
*
*    PERFORM F_PREENCHER_DYNPRO USING:
*           'X' 'SAPMF05A'                      '0113',
*           ' ' 'BKPF-BLDAT'                    WL_DATA,
*           ' ' 'BKPF-BLART'                    'DZ',
*           ' ' 'BKPF-BUKRS'                    WA_TERRENO-EMPRESA,
*           ' ' 'BKPF-BUDAT'                    WL_DATA,
*           ' ' 'BKPF-MONAT'                    WL_DATA+3(2),
*           ' ' 'BKPF-WAERS'                    'USD',
*           ' ' 'BKPF-BKTXT'                    WL_BKTXT,
*           ' ' 'BKPF-XBLNR'                    WL_BKTXT,
*           ' ' 'RF05A-NEWKO'                   WA_VENDA-CLIENTE,
*           ' ' 'RF05A-ZUMSK'                   'A',
*           ' ' 'BDC_OKCODE'                    '/00'.
*
*
*    WRITE WA_VENDA-DATA_PARCELA_INI TO  WL_DATA.
*    PERFORM F_PREENCHER_DYNPRO USING:
*           'X' 'SAPMF05A'                      '0304',
*           ' ' 'BSEG-WRBTR'                    WL_VALOR,
*           ' ' 'BSEG-GSBER'                    '0101',
*           ' ' 'BSEG-ZUONR'                    'PERFOMACE',
*           ' ' 'BSEG-ZFBDT'                    WL_DATA,
*           ' ' 'BDC_OKCODE'                    '=ZK'.
*
*    PERFORM F_PREENCHER_DYNPRO USING:
*           'X' 'SAPMF05A'                      '0331',
*           ' ' 'BSEG-HBKID'                    'BBRA',
*           ' ' 'BDC_OKCODE'                    '=BU'.
*
*    OPT-DISMODE = 'N'.
*    OPT-DEFSIZE = 'X'.
*    CALL TRANSACTION 'F-37' USING TG_BDC
*      OPTIONS FROM OPT.
**      MESSAGES INTO TG_MSG.
*
**    TE_RETURN-NRO_SOL_OV = TI_NRO_SOL_OV-NRO_SOL_OV.
**    TE_RETURN-POSNR = TL_PGT_ANT-POSNR.
**    TE_RETURN-ZMENG = TL_ITENS-ZMENG.
**    TE_RETURN-VALDT = TL_PGT_ANT-VALDT.
**    TE_RETURN-VLRTOT = TL_PGT_ANT-DMBTR.
**          TE_RETURN-VBELN = TL_ITENS-VBELN.

  ENDMETHOD.                    "GERAR_DOCUMENTOS

  METHOD GERAR_DOC_CONTABIL.

    FIELD-SYMBOLS: <SAIDA> TYPE TY_CONTAS_RECEBER.

    DATA: E_NUM_LOTE    TYPE ZLOTE_NUM,
          WL_ZGLT031    TYPE ZGLT031,
          GT_ZGL032_AUX TYPE TABLE OF ZGLT032,
          GT_ZGLT032    TYPE TABLE OF ZGLT032,
          WL_ZGLT032    TYPE ZGLT032,
          WL_ZFIT0098   TYPE ZFIT0098,
          GT_ZGLT036    TYPE TABLE OF ZGLT036,
          WL_ZGLT036    TYPE ZGLT036,
          DP_RESP       TYPE CHAR2,
          WL_ZGLT035    TYPE ZGLT035,
          VL_TIPO_LCTO  TYPE ZFIT0101-TP_LCTO,
          VL_COD_EV     TYPE ZFIT0101-COD_EV,
          VL_ZUONR      TYPE STRING,
          VL_DESC_LOTE  TYPE STRING,
          VL_DESC_QD    TYPE STRING,
          VL_DESC_TERRENO TYPE STRING,
          VL_VALOR_PARCELA TYPE ZFIT0101-VALOR,
          VL_GSBER         TYPE ZGLT036-GSBER.


    CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.

    CALL METHOD WA_ALV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    CHECK IT_SEL_ROWS IS NOT INITIAL.

    IF WA_VENDA-CLIENTE IS INITIAL.
      MESSAGE 'Cliente da Venda não informado!' TYPE 'S'.
      RETURN.
    ENDIF.

    CLEAR: VL_ZUONR, VL_DESC_LOTE,VL_DESC_QD,VL_DESC_TERRENO.

    VL_DESC_LOTE    = WA_TERRENO-LOTEAMENTO.
    VL_DESC_QD      = WA_TERRENO-NRO_QUADRA.
    VL_DESC_TERRENO = WA_TERRENO-NRO_TERRENO.

    CONCATENATE 'LT.' VL_DESC_LOTE  '-'
                'QD.' VL_DESC_QD    '-'
                'TE.' VL_DESC_TERRENO INTO VL_ZUONR.

    LOOP AT IT_SEL_ROWS INTO WA_SEL_ROWS.
      READ TABLE IT_CR ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

      CLEAR WA_ZFIT0101.
      SELECT SINGLE *
        INTO WA_ZFIT0101
        FROM ZFIT0101
       WHERE EMPRESA     = WA_TERRENO-EMPRESA
         AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
         AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
         AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA
         AND NRO_PARC    = <SAIDA>-NRO_PARCELA
         AND COD_EV      = <SAIDA>-COD_EV.

      IF ( WA_ZFIT0101-EMPRESA     IS INITIAL ) OR
         ( WA_ZFIT0101-LOTEAMENTO  IS INITIAL ) OR
         ( WA_ZFIT0101-NRO_TERRENO IS INITIAL ) OR
         ( WA_ZFIT0101-NRO_QUADRA  IS INITIAL ) OR
         ( WA_ZFIT0101-NRO_PARC    IS INITIAL ) OR
         ( WA_ZFIT0101-TIPO        IS INITIAL ).

        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gerar a documento contábil referente a parcela!' TYPE 'S'.
        RETURN.
      ENDIF.

      VL_TIPO_LCTO = WA_ZFIT0101-TP_LCTO.
      VL_COD_EV    = WA_ZFIT0101-COD_EV.

      IF ( VL_TIPO_LCTO IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE 'Tipo Lcto não encontrado!' TYPE 'S'.
        RETURN.
      ENDIF.

      IF ( VL_COD_EV IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE 'Tipo Lcto não encontrado!' TYPE 'S'.
        RETURN.
      ENDIF.

      IF <SAIDA>-DOC_CONTABIL IS NOT INITIAL .
        CONTINUE.
      ENDIF.

      CLEAR: WL_ZGLT031, WL_ZGLT035.
      SELECT SINGLE * FROM ZGLT031 INTO WL_ZGLT031 WHERE TP_LCTO EQ VL_TIPO_LCTO.
      IF NOT WL_ZGLT031 IS INITIAL.

        FREE: GT_ZGL032_AUX, GT_ZGLT032.
        SELECT * FROM ZGLT032 INTO TABLE GT_ZGL032_AUX WHERE TP_LCTO EQ VL_TIPO_LCTO.

        LOOP AT GT_ZGL032_AUX INTO WL_ZGLT032 WHERE TP_LCTO = VL_TIPO_LCTO.

          CLEAR: WA_ZFIT0103.
          SELECT SINGLE *
            INTO WA_ZFIT0103
            FROM ZFIT0103
           WHERE COD_EV = <SAIDA>-COD_EV
             AND CONTA  = WL_ZGLT032-HKONT.

          IF ( SY-SUBRC = 0 ) OR
             ( WL_ZGLT032-BSCHL EQ 01 ) .
             APPEND WL_ZGLT032 TO GT_ZGLT032.
          ENDIF.

        ENDLOOP.

        IF NOT GT_ZGLT032 IS INITIAL.

          MOVE WL_ZGLT031-DPTO_RESP TO DP_RESP.

          CALL METHOD ZCL_GERAR_LOTE=>CREATE_LOTE
            EXPORTING
              I_BUKRS      = WA_TERRENO-EMPRESA
              I_DESCR_LOTE = 'VD.TERRENOS-CID'
              I_DEP_RESP   = DP_RESP
              I_USER_RESP  = SY-UNAME
            IMPORTING
              E_NUM_LOTE   = WL_ZGLT035-LOTE.

          MOVE: WL_ZGLT035-LOTE           TO <SAIDA>-LOTE,
                WA_TERRENO-EMPRESA        TO WL_ZGLT035-BUKRS,
                VL_TIPO_LCTO              TO WL_ZGLT035-TP_LCTO,
                DP_RESP                   TO WL_ZGLT035-DPTO_RESP,
                'BRL'                     TO WL_ZGLT035-MOEDA_DOC,
                WL_ZGLT031-ST_LC_MOEDA    TO WL_ZGLT035-ST_LC_MOEDA,
                WL_ZGLT031-MOEDA_INT_HIST TO WL_ZGLT035-MOEDA_INT_HIST,
                WL_ZGLT031-MOEDA_FT_HIST  TO WL_ZGLT035-MOEDA_FT_HIST,
                WL_ZGLT031-MOEDA_GP_HIST  TO WL_ZGLT035-MOEDA_GP_HIST,
                WL_ZGLT031-BLART          TO WL_ZGLT035-BLART,
                'VD.TERRENOS-CID'         TO WL_ZGLT035-XBLNR,
                WL_ZGLT031-BKTXT          TO WL_ZGLT035-BKTXT,
                SY-DATUM                  TO WL_ZGLT035-BUDAT,
                SY-DATUM                  TO WL_ZGLT035-BLDAT,
                SY-DATUM                  TO WL_ZGLT035-DT_LCTO,
                WL_ZGLT031-PROV_EST       TO WL_ZGLT035-PROV_EST,
                WL_ZGLT031-ST_AP_FISCAL   TO WL_ZGLT035-ST_AP_FISCAL,
                SY-DATUM+4(2)             TO WL_ZGLT035-MONAT,
                SY-DATUM(4)               TO WL_ZGLT035-GJAHR,
                SY-UNAME                  TO WL_ZGLT035-USNAM,
                SY-DATUM                  TO WL_ZGLT035-DT_ENTRADA,
                SY-UZEIT                  TO WL_ZGLT035-HR_ENTRADA.

          CLEAR: GT_ZGLT036.
          LOOP AT GT_ZGLT032 INTO WL_ZGLT032.

            CLEAR: VL_GSBER.
            CONCATENATE WA_TERRENO-EMPRESA+2(2) '01' VL_GSBER INTO VL_GSBER.

            MOVE: SY-TABIX              TO WL_ZGLT036-SEQITEM,
                  WL_ZGLT032-TP_LCTO    TO WL_ZGLT036-TP_LCTO,
                  WL_ZGLT032-BSCHL      TO WL_ZGLT036-BSCHL,
                  VL_ZUONR              TO WL_ZGLT036-ZUONR,
                  VL_GSBER              TO WL_ZGLT036-GSBER,
                  <SAIDA>-DT_VENCIMENTO TO WL_ZGLT036-DT_VCT,
                  'BBRA'                TO WL_ZGLT036-HBKID,
                  'D'                   TO WL_ZGLT036-ZLSCH.

            CLEAR: VL_VALOR_PARCELA.
            IF ( WL_ZGLT032-BSCHL EQ 01 ).
              WL_ZGLT036-HKONT = WA_VENDA-CLIENTE.
              VL_VALOR_PARCELA = <SAIDA>-VALOR.
            ELSE.
              WL_ZGLT036-HKONT = WL_ZGLT032-HKONT.

              CLEAR: WA_ZFIT0103, VL_VALOR_PARCELA.
              SELECT SINGLE *
                INTO WA_ZFIT0103
                FROM ZFIT0103
               WHERE COD_EV = VL_COD_EV
                 AND CONTA  = WL_ZGLT032-HKONT.

              IF ( WA_ZFIT0103-BASE_IMPOSTO IS NOT INITIAL ) AND
                 ( WA_ZFIT0103-TX_IMPOSTO > 0 ).
                VL_VALOR_PARCELA =  ( <SAIDA>-VALOR * WA_ZFIT0103-TX_IMPOSTO ) / 100.
              ELSE.
                VL_VALOR_PARCELA =   <SAIDA>-VALOR.
              ENDIF.

            ENDIF.

            MOVE: ABS( VL_VALOR_PARCELA ) TO WL_ZGLT036-VLR_MOEDA_DOC,
                  ABS( VL_VALOR_PARCELA ) TO WL_ZGLT036-VLR_MOEDA_INT,
                  ABS( VL_VALOR_PARCELA ) TO WL_ZGLT036-VLR_MOEDA_FORTE.

            APPEND WL_ZGLT036 TO GT_ZGLT036.

            CLEAR: WL_ZGLT036, WL_ZGLT032.

          ENDLOOP.

          CALL METHOD ZCL_GERAR_LOTE=>CONTABILIZAR_LOTE( CHANGING
                                                         I_ZGLT036 =  GT_ZGLT036
                                                         I_ZGLT035 =  WL_ZGLT035 ).

          MOVE: '@B4@'              TO <SAIDA>-STATUS,
                WL_ZGLT035-LOTE     TO <SAIDA>-LOTE,
                WL_ZGLT035-DOC_LCTO TO <SAIDA>-DOC_LCTO.

          WA_ZFIT0101-STATUS      = <SAIDA>-STATUS.
          WA_ZFIT0101-LOTE        = <SAIDA>-LOTE.
          WA_ZFIT0101-DOC_LCTO    = <SAIDA>-DOC_LCTO.
          WA_ZFIT0101-DT_LCTO_CTB = SY-DATUM.

          MODIFY ZFIT0101 FROM WA_ZFIT0101.

        ELSE.
          MESSAGE S836(SD) WITH TEXT-003.
        ENDIF.
      ELSE.
        MESSAGE S836(SD) WITH TEXT-004.
      ENDIF.

    ENDLOOP.

    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "GERAR_DOC_CONTABIL

   METHOD REFRESH_DOC_CONTABIL.

    FIELD-SYMBOLS: <SAIDA> TYPE TY_CONTAS_RECEBER.

    DATA: WL_ZGLT034   TYPE ZGLT034,
          WL_ZGLT035   TYPE ZGLT035,
          WL_ZIB_CHAVE TYPE ZIB_CONTABIL_CHV,
          WL_ZIB_ERRO  TYPE ZIB_CONTABIL_ERR.


    CHECK WA_ALV IS NOT INITIAL.

    UNASSIGN <SAIDA>.
    CHECK NOT IT_CR IS INITIAL.

    LOOP AT IT_CR ASSIGNING <SAIDA>.

      IF <SAIDA>-DOC_CONTABIL IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR WA_ZFIT0101.
      SELECT SINGLE *
        INTO WA_ZFIT0101
        FROM ZFIT0101
       WHERE EMPRESA     = WA_TERRENO-EMPRESA
         AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
         AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
         AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA
         AND NRO_PARC    = <SAIDA>-NRO_PARCELA
         AND COD_EV      = <SAIDA>-COD_EV
         AND TIPO        = <SAIDA>-TIPO.

      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.

      CASE <SAIDA>-STATUS.
        WHEN '@B4@' OR '@2K@' OR '@02@'.

          SELECT SINGLE *
            FROM ZGLT034
            INTO WL_ZGLT034
           WHERE BUKRS = <SAIDA>-EMPRESA
             AND LOTE  = <SAIDA>-LOTE.

          IF SY-SUBRC NE 0.
            MOVE ABAP_FALSE TO <SAIDA>-LOTE.
            MOVE ABAP_FALSE TO <SAIDA>-DOC_LCTO.
            MOVE ABAP_FALSE TO <SAIDA>-DOC_CONTABIL.
            MOVE '@08@'     TO <SAIDA>-STATUS.

            MOVE ABAP_FALSE TO WA_ZFIT0101-LOTE.
            MOVE ABAP_FALSE TO WA_ZFIT0101-DOC_LCTO.
            MOVE ABAP_FALSE TO WA_ZFIT0101-BELNR.
            MOVE '@08@'     TO WA_ZFIT0101-STATUS.

            MODIFY ZFIT0101 FROM WA_ZFIT0101.

            CONTINUE.
          ENDIF.

          "Verifica se Doc. Lcto foi estornado.
          SELECT SINGLE *
            FROM ZGLT035
            INTO WL_ZGLT035
           WHERE BUKRS    = <SAIDA>-EMPRESA
             AND DOC_LCTO = <SAIDA>-DOC_LCTO
             AND TP_LCTO  = <SAIDA>-TP_LCTO.

          IF ( SY-SUBRC EQ 0 ) AND ( WL_ZGLT035-LOEKZ EQ 'X' ).
            MOVE ABAP_FALSE TO <SAIDA>-DOC_LCTO.
            MOVE ABAP_FALSE TO <SAIDA>-DOC_CONTABIL.
            MOVE ABAP_FALSE TO <SAIDA>-LOTE.
            MOVE '@08@'     TO <SAIDA>-STATUS.

            MOVE ABAP_FALSE TO WA_ZFIT0101-DOC_LCTO.
            MOVE ABAP_FALSE TO WA_ZFIT0101-BELNR.
            MOVE ABAP_FALSE TO WA_ZFIT0101-LOTE.
            MOVE '@08@'     TO WA_ZFIT0101-STATUS.

            MODIFY ZFIT0101 FROM WA_ZFIT0101.

            CONTINUE.
          ENDIF.

          ME->Z_RETORNA_STATUS_ZIB( EXPORTING
                                      I_DOC_LCTO = <SAIDA>-DOC_LCTO
                                      I_ANO_LCTO = WL_ZGLT034-DATA_ATUAL(4)
                                    IMPORTING
                                      E_ZIBCHV   = WL_ZIB_CHAVE
                                      E_ZIBERR   = WL_ZIB_ERRO ).

          IF ( WL_ZIB_CHAVE IS NOT INITIAL ).
            MOVE: '@2K@'             TO <SAIDA>-STATUS,
                  WL_ZIB_CHAVE-BELNR TO <SAIDA>-DOC_CONTABIL.

            WA_ZFIT0101-STATUS = <SAIDA>-STATUS.
            WA_ZFIT0101-BELNR  = <SAIDA>-DOC_CONTABIL.
            MODIFY ZFIT0101 FROM WA_ZFIT0101.

            MESSAGE S836(SD) WITH TEXT-001.
          ELSEIF ( WL_ZIB_ERRO IS NOT INITIAL ).
            MOVE '@02@' TO <SAIDA>-STATUS.

            WA_ZFIT0101-STATUS = <SAIDA>-STATUS.
            MODIFY ZFIT0101 FROM WA_ZFIT0101.

            MESSAGE S836(SD) WITH TEXT-002.
          ENDIF.

      ENDCASE.
    ENDLOOP.

    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "REFRESH_DOC_CONTABIL

  METHOD Z_RETORNA_STATUS_ZIB.

    DATA V_OBJKEY    TYPE CHAR20.
    CLEAR: E_ZIBCHV, E_ZIBERR.

    CONCATENATE 'ZGL17' I_DOC_LCTO I_ANO_LCTO INTO V_OBJKEY.

    SELECT SINGLE *
      FROM ZIB_CONTABIL_CHV
      INTO E_ZIBCHV
     WHERE OBJ_KEY = V_OBJKEY.

    IF ( SY-SUBRC IS NOT INITIAL ).

      SELECT SINGLE *
        FROM ZIB_CONTABIL_ERR
        INTO E_ZIBERR
       WHERE OBJ_KEY = V_OBJKEY.
    ENDIF.

  ENDMETHOD.                    "z_retorna_status_zib

  METHOD ESTORNO_DOC_CONTABIL.

    FIELD-SYMBOLS: <SAIDA> TYPE TY_CONTAS_RECEBER.

    CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.
    UNASSIGN <SAIDA>.

    DATA: IT_MSG     TYPE TABLE OF BDCMSGCOLL," WITH HEADER LINE,
          WA_MSG     TYPE BDCMSGCOLL.

    CALL METHOD WA_ALV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    CHECK NOT IT_SEL_ROWS IS INITIAL.

    IF ( LINES( IT_SEL_ROWS ) NE 1 ).
      MESSAGE S836(SD) WITH TEXT-007.
      EXIT.
    ENDIF.

    READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.
    READ TABLE IT_CR ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

    CHECK <SAIDA>-DOC_CONTABIL IS NOT INITIAL.

    CLEAR: WA_BSAD.
    SELECT SINGLE *
      INTO WA_BSAD
      FROM BSAD
     WHERE BELNR = <SAIDA>-DOC_CONTABIL
       AND BUKRS = <SAIDA>-EMPRESA.

    IF SY-SUBRC = 0.
      MESSAGE 'Documento de Compensação deve ser cancelado primeiro!' TYPE 'S'.
      RETURN.
    ENDIF.

    CLEAR WA_ZFIT0101.
    SELECT SINGLE *
      INTO WA_ZFIT0101
      FROM ZFIT0101
     WHERE EMPRESA     = WA_TERRENO-EMPRESA
       AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA
       AND NRO_PARC    = <SAIDA>-NRO_PARCELA.

    IF ( WA_ZFIT0101-EMPRESA     IS INITIAL ) OR
       ( WA_ZFIT0101-LOTEAMENTO  IS INITIAL ) OR
       ( WA_ZFIT0101-NRO_TERRENO IS INITIAL ) OR
       ( WA_ZFIT0101-NRO_QUADRA  IS INITIAL ) OR
       ( WA_ZFIT0101-NRO_PARC    IS INITIAL ) OR
       ( <SAIDA>-DT_LCTO_CTB     IS INITIAL ).

      ROLLBACK WORK.
      MESSAGE S836(SD) WITH TEXT-006.
      RETURN.
    ENDIF.


    FREE: IT_DTA.
    DEFINE SHDB.
      CLEAR IT_DTA.
      WA_DTA-PROGRAM   = &1.
      WA_DTA-DYNPRO    = &2.
      WA_DTA-DYNBEGIN  = &3.
      WA_DTA-FNAM      = &4.
      WA_DTA-FVAL      = &5.
      APPEND WA_DTA TO IT_DTA.
    END-OF-DEFINITION.

    SHDB:
    'SAPMF05A' '0105' 'X'  ' '           ' ',
    ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
    ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
    ' '        ' '    ' '  'RF05A-BELNS' <SAIDA>-DOC_CONTABIL,
    ' '        ' '    ' '  'BKPF-BUKRS'  <SAIDA>-EMPRESA,
    ' '        ' '    ' '  'RF05A-GJAHS' <SAIDA>-DT_LCTO_CTB(4),
    ' '        ' '    ' '  'UF05A-STGRD' '01'.

    OPT-DISMODE = 'E'.
    CALL TRANSACTION 'FB08' USING IT_DTA OPTIONS FROM OPT.

    IF SY-SUBRC IS INITIAL.
      MOVE ABAP_FALSE TO <SAIDA>-LOTE.
      MOVE ABAP_FALSE TO <SAIDA>-DOC_LCTO.
      MOVE ABAP_FALSE TO <SAIDA>-DOC_CONTABIL.
      MOVE '@08@'     TO <SAIDA>-STATUS.

      MOVE ABAP_FALSE TO WA_ZFIT0101-LOTE.
      MOVE ABAP_FALSE TO WA_ZFIT0101-DOC_LCTO.
      MOVE ABAP_FALSE TO WA_ZFIT0101-BELNR.
      MOVE '@08@'     TO WA_ZFIT0101-STATUS.

      MODIFY ZFIT0101 FROM WA_ZFIT0101.

    ENDIF.

    ME->ATUALIZA_VENDA( ).

    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
  ENDMETHOD.                    "ESTORNO_DOC_CONTABIL

  METHOD GERAR_BOLETO.

    FIELD-SYMBOLS: <SAIDA> TYPE TY_CONTAS_RECEBER.

    DATA: IT_ZFIT0048   TYPE TABLE OF TY_ZFIT0048,
          IT_T045T      TYPE TABLE OF TY_T045T,
          IT_T012K      TYPE TABLE OF TY_T012K,
          IT_T012       TYPE TABLE OF TY_T012,
          IT_LFA1       TYPE TABLE OF TY_LFA1,
          IT_KNA1       TYPE TABLE OF TY_KNA1,
          IT_SAIDA      TYPE TABLE OF ZFI_BOLETO.

    DATA: WA_BKPF       TYPE TY_BKPF,
          WA_BSID       TYPE TY_BSID,
          WA_ZFIT0048   TYPE TY_ZFIT0048,
          WA_T045T      TYPE TY_T045T,
          WA_T012K      TYPE TY_T012K,
          WA_T012       TYPE TY_T012,
          WA_LFA1       TYPE TY_LFA1,
          WA_KNA1       TYPE TY_KNA1,
          WA_SAIDA      TYPE ZFI_BOLETO.

    DATA: VDOCNUM TYPE J_1BNFDOC-DOCNUM,
          VL_FORM      TYPE TDSFNAME    ,
          VL_NAME      TYPE RS38L_FNAM  ,
          VDV_NOSSO(1),
          VAR_TOTAL_TRIB  TYPE J_1BTAXVAL,
          VAR_TOTAL_TRIB2 TYPE J_1BTAXVAL,
          VAR_TOTAL_DESON TYPE J_1BTAXVAL,
          VAR_TOTAL_DESC  TYPE J_1BTAXVAL,
          VAR_TOTAL_OUTROS TYPE J_1BTAXVAL,
          VAR_SUFRAMA     TYPE C.

    DATA: P_WA_SAIDA  TYPE ZFI_BOLETO,
          P_EMAIL(100),
          T_DOC_NUMERO TYPE J_1BDOCNUM.


    DATA: WL_CONT              TYPE SY-TABIX,
          WL_CONT_AUX          TYPE SY-TABIX,
          WL_CONT_AUX2         TYPE SY-TABIX,
          VAR_BANKL            TYPE T012-BANKL,
          WL_BSEG              TYPE BSEG,
          V_XREF2              TYPE BSEG-XREF2,
          V_XREF3              TYPE BSEG-XREF3,
          V_TIPO(1).


    DATA: VL_BUKRS TYPE BSID-BUKRS,
          VL_GSBER TYPE LFA1-LIFNR,
          VL_KUNNR TYPE BSID-KUNNR,
          VL_HBKID TYPE BSID-HBKID.

    DATA: VINSTRUCAO TYPE ZFIT0048-INSTRUCAO.

    FREE: I_OTF, I_TLINE, V_BIN_FILESIZE, I_RECORD, WA_BUFFER.

    CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.
    UNASSIGN <SAIDA>.

    DATA: IT_MSG     TYPE TABLE OF BDCMSGCOLL," WITH HEADER LINE,
          WA_MSG     TYPE BDCMSGCOLL.

    CALL METHOD WA_ALV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    CHECK NOT IT_SEL_ROWS IS INITIAL.

    IF ( LINES( IT_SEL_ROWS ) NE 1 ).
      MESSAGE S836(SD) WITH TEXT-007.
      EXIT.
    ENDIF.

    READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.
    READ TABLE IT_CR ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

    VL_BUKRS = WA_TERRENO-EMPRESA.
    CONCATENATE WA_TERRENO-EMPRESA+2(2) '01' INTO VL_GSBER.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = VL_GSBER
      IMPORTING
        OUTPUT = VL_GSBER.


    VL_KUNNR = WA_VENDA-CLIENTE.
    VL_HBKID = 'BBRA'.
    VINSTRUCAO = '01'.

    IF <SAIDA>-NUM_PARC_SEQ IS INITIAL.
      MESSAGE 'Numero sequencial da parcela não gerado!' TYPE 'S'.
      EXIT.
    ENDIF.

    IF VL_BUKRS IS INITIAL.
      MESSAGE 'Empresa não encontrada!' TYPE 'S'.
      EXIT.
    ENDIF.

    IF VL_GSBER IS INITIAL.
      MESSAGE 'Divisão não encontrada!' TYPE 'S'.
      EXIT.
    ENDIF.

    IF VL_KUNNR IS INITIAL.
      MESSAGE 'Cliente não encontrado!' TYPE 'S'.
      EXIT.
    ENDIF.

    "------------------------------------------------------------------
    "Processamento
    "-------------------------------------------------------------------
    SELECT BUKRS WERKS KUNNR TXT_INSTRUCAO
      FROM ZFIT0048
      INTO TABLE IT_ZFIT0048
     WHERE INSTRUCAO = VINSTRUCAO
       AND BUKRS     = VL_BUKRS
       AND WERKS     = VL_GSBER
       AND KUNNR     = VL_KUNNR.

    IF IT_ZFIT0048[] IS INITIAL.
      SELECT BUKRS WERKS KUNNR TXT_INSTRUCAO
        FROM ZFIT0048
        INTO TABLE IT_ZFIT0048
       WHERE INSTRUCAO = VINSTRUCAO
         AND BUKRS     = VL_BUKRS
         AND WERKS     = VL_GSBER.

      IF IT_ZFIT0048[] IS INITIAL.
        SELECT BUKRS WERKS KUNNR TXT_INSTRUCAO
          FROM ZFIT0048
          INTO TABLE IT_ZFIT0048
         WHERE INSTRUCAO = VINSTRUCAO
           AND BUKRS     = VL_BUKRS.

        IF IT_ZFIT0048[] IS INITIAL.
          SELECT BUKRS WERKS KUNNR TXT_INSTRUCAO
            FROM ZFIT0048
            INTO TABLE IT_ZFIT0048
           WHERE INSTRUCAO = VINSTRUCAO.
        ENDIF.

      ENDIF.

    ENDIF.

    SELECT BUKRS HBKID DTAID
      FROM T045T
      INTO TABLE IT_T045T
     WHERE BUKRS  = VL_BUKRS
       AND ZLSCH  = 'D'
       AND HBKID  = 'BBRA'.

    SORT IT_T045T BY BUKRS HBKID.
SELECT BUKRS HBKID BANKN BKONT
      FROM T012K
      INTO TABLE IT_T012K
     WHERE BUKRS  = VL_BUKRS
       AND HBKID  = 'BBRA' ORDER BY PRIMARY KEY .

    SELECT BUKRS HBKID BANKL
      FROM T012
      INTO TABLE IT_T012
     WHERE BUKRS  = VL_BUKRS
       AND HBKID  = 'BBRA' ORDER BY PRIMARY KEY .

    "Empresa
    SELECT LIFNR NAME1 STRAS ORT02 PSTLZ ORT01 REGIO STCD1
      FROM LFA1
      INTO TABLE IT_LFA1
     WHERE LIFNR = VL_GSBER ORDER BY PRIMARY KEY .

    "Filial
    SELECT LIFNR NAME1 STRAS ORT02 PSTLZ ORT01 REGIO STCD1
      FROM LFA1
      APPENDING TABLE IT_LFA1
     WHERE LIFNR = VL_GSBER ORDER BY PRIMARY KEY .

    "Cliente
    SELECT KUNNR NAME1 STRAS ORT02 PSTLZ ORT01 REGIO STCD1
      FROM KNA1
      INTO TABLE IT_KNA1
    WHERE KUNNR = VL_KUNNR ORDER BY PRIMARY KEY .


    READ TABLE IT_T045T INTO WA_T045T WITH KEY BUKRS = VL_BUKRS
                                               HBKID = VL_HBKID BINARY SEARCH.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_T045T-DTAID
      IMPORTING
        OUTPUT = WA_T045T-DTAID.

    WA_SAIDA-DTAID         = WA_T045T-DTAID+0(7).
    CONCATENATE WA_SAIDA-DTAID <SAIDA>-NUM_PARC_SEQ INTO WA_SAIDA-DTAID.

    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = VL_GSBER BINARY SEARCH.
    WA_SAIDA-XENAME1       = WA_LFA1-NAME1.
    WA_SAIDA-XESTRAS       = WA_LFA1-STRAS.
    WA_SAIDA-XEORT02       = WA_LFA1-ORT02.
    WA_SAIDA-XEORT01       = WA_LFA1-ORT01.
    WA_SAIDA-XEREGIO       = WA_LFA1-REGIO.
    WA_SAIDA-XEPSTLZ       = WA_LFA1-PSTLZ.
    WA_SAIDA-XESTCD1       = WA_LFA1-STCD1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = <SAIDA>-NUM_PARC_SEQ
      IMPORTING
        OUTPUT = WA_SAIDA-NFENUM.

    "WA_SAIDA-NFENUM        = <SAIDA>-NUM_PARC_SEQ.
    "WA_SAIDA-REFKEY        = .

    WA_SAIDA-ZBD1T         = <SAIDA>-DT_VENCIMENTO.
    WA_SAIDA-DMBTR         = <SAIDA>-VALOR.

    IF IT_ZFIT0048[] IS NOT INITIAL.
      READ TABLE IT_ZFIT0048 INTO WA_ZFIT0048 INDEX 1.
      CLEAR: WL_CONT_AUX2, WL_CONT_AUX, WL_CONT.
      WL_CONT = STRLEN( WA_ZFIT0048-TXT_INSTRUCAO ).
      WL_CONT_AUX = WL_CONT / 79.
      DO.
        IF SY-INDEX = 1.
          WA_SAIDA-TXT_INSTRUCAO1 = WA_ZFIT0048-TXT_INSTRUCAO+WL_CONT_AUX2.
        ELSEIF SY-INDEX = 2.
          WA_SAIDA-TXT_INSTRUCAO2 = WA_ZFIT0048-TXT_INSTRUCAO+WL_CONT_AUX2.
        ELSEIF SY-INDEX = 3.
          WA_SAIDA-TXT_INSTRUCAO3 = WA_ZFIT0048-TXT_INSTRUCAO+WL_CONT_AUX2.
        ELSEIF SY-INDEX = 4.
          WA_SAIDA-TXT_INSTRUCAO4 = WA_ZFIT0048-TXT_INSTRUCAO+WL_CONT_AUX2.
        ELSEIF SY-INDEX = 5.
          WA_SAIDA-TXT_INSTRUCAO5 = WA_ZFIT0048-TXT_INSTRUCAO+WL_CONT_AUX2.
        ELSEIF SY-INDEX = 6.
          WA_SAIDA-TXT_INSTRUCAO6 = WA_ZFIT0048-TXT_INSTRUCAO+WL_CONT_AUX2.
        ENDIF.
        ADD 80 TO WL_CONT_AUX2.
        IF WL_CONT_AUX2 GT WL_CONT.
          EXIT.

        ENDIF.
      ENDDO.
    ENDIF.

    WA_SAIDA-DATA_SIST     = SY-DATUM.

    READ TABLE IT_T012 INTO WA_T012 WITH KEY BUKRS = VL_BUKRS
                                             HBKID = VL_HBKID BINARY SEARCH.

    VAR_BANKL              = WA_T012-BANKL+4(10). "Agencia
    CONDENSE VAR_BANKL NO-GAPS.
    CONCATENATE VAR_BANKL '-' '7' INTO VAR_BANKL SEPARATED BY SPACE.
    WA_SAIDA-BANKL+0(10)   = VAR_BANKL.

    READ TABLE IT_T012K INTO WA_T012K WITH KEY BUKRS = VL_BUKRS
                                               HBKID = VL_HBKID BINARY SEARCH.
    WA_SAIDA-BANKN         = WA_T012K-BANKN.
    WA_SAIDA-BKONT         = WA_T012K-BKONT.

    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = VL_KUNNR BINARY SEARCH.
    WA_SAIDA-NAME1         = WA_KNA1-NAME1.
    WA_SAIDA-STCD1         = WA_KNA1-STCD1.
    WA_SAIDA-STRAS         = WA_KNA1-STRAS.
    WA_SAIDA-ORT01         = WA_KNA1-ORT01.
    WA_SAIDA-REGIO         = WA_KNA1-REGIO.
    WA_SAIDA-PSTLZ         = WA_KNA1-PSTLZ.

    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = VL_GSBER BINARY SEARCH.
    WA_SAIDA-XFNAME1       = WA_LFA1-NAME1.
    WA_SAIDA-XFSTCD1       = WA_LFA1-STCD1.

    PERFORM CALCULA_COD_BARRAS USING WA_SAIDA-DMBTR WA_SAIDA-ZBD1T WA_SAIDA-DTAID CHANGING VDV_NOSSO WA_SAIDA.

*    SELECT SINGLE *
*      FROM BSEG
*      INTO WL_BSEG
*     WHERE BUKRS = WA_BSID-BUKRS
*       AND BELNR = WA_BSID-BELNR
*       AND GJAHR = WA_BSID-GJAHR
*       AND BUZEI = '001'.

*    V_XREF2 = WA_J_1BNFDOC-NFENUM.
*    V_XREF3 = WA_SAIDA-DTAID.
*    IF WL_BSEG-XREF3 NE V_XREF3.
*      V_TIPO = 'N'.
*      PERFORM EXECUTA_SHDB_FB02 USING WA_BSID-BUKRS WA_BSID-BELNR WA_BSID-GJAHR V_XREF2 V_XREF3 V_TIPO.
*    ENDIF.

    APPEND WA_SAIDA TO IT_SAIDA.

    "Call Impressão.
    LOOP AT IT_SAIDA INTO WA_SAIDA.
      PERFORM F_IMPRIME_SMART USING WA_SAIDA.
    ENDLOOP.

  ENDMETHOD.

  METHOD GERAR_JUROS.

    DATA: VAR_ANSWER TYPE C.

    DATA: VL_COD_EV  TYPE ZFIT0101-COD_EV,
          VL_TP_LCTO TYPE ZFIT0101-TP_LCTO.


    FIELD-SYMBOLS: <SAIDA> TYPE TY_CONTAS_RECEBER.

    CLEAR: IT_SEL_ROWS[], WA_SEL_ROWS.
    UNASSIGN <SAIDA>.

    DATA: IT_MSG     TYPE TABLE OF BDCMSGCOLL," WITH HEADER LINE,
          WA_MSG     TYPE BDCMSGCOLL.

    CALL METHOD WA_ALV->GET_SELECTED_ROWS
      IMPORTING
        ET_INDEX_ROWS = IT_SEL_ROWS.

    CHECK NOT IT_SEL_ROWS IS INITIAL.

    IF ( LINES( IT_SEL_ROWS ) NE 1 ).
      MESSAGE S836(SD) WITH TEXT-007.
      EXIT.
    ENDIF.

    READ TABLE IT_SEL_ROWS INTO WA_SEL_ROWS INDEX 1.
    READ TABLE IT_CR ASSIGNING <SAIDA> INDEX WA_SEL_ROWS-INDEX.

    CLEAR: WA_ZFIT0102, VL_COD_EV,VL_TP_LCTO.
    SELECT SINGLE *
      INTO WA_ZFIT0102
      FROM ZFIT0102
     WHERE TIPO = 'JRO'.

    VL_COD_EV  = WA_ZFIT0102-COD_EV.
    VL_TP_LCTO = WA_ZFIT0102-TP_LCTO.

    IF VL_COD_EV IS INITIAL .
      MESSAGE 'Código Evento de Juros não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF VL_TP_LCTO IS INITIAL .
      MESSAGE 'Tipo Lancamento de Juros não encontrado!' TYPE 'S'.
      RETURN.
    ENDIF.

    CLEAR WA_ZFIT0101.
    SELECT SINGLE *
      INTO WA_ZFIT0101
      FROM ZFIT0101
     WHERE EMPRESA     = WA_TERRENO-EMPRESA
       AND LOTEAMENTO  = WA_TERRENO-LOTEAMENTO
       AND NRO_TERRENO = WA_TERRENO-NRO_TERRENO
       AND NRO_QUADRA  = WA_TERRENO-NRO_QUADRA
       AND NRO_PARC    = <SAIDA>-NRO_PARCELA
       AND TIPO        = 'JR'.

    IF SY-SUBRC = 0 .
      MESSAGE 'Parcela de juros já existente. Favor estorna-la primeiro!' TYPE 'S'.
      RETURN.
    ENDIF.

    CLEAR: WA_BSAD.
    SELECT SINGLE *
      INTO WA_BSAD
      FROM BSAD
     WHERE BELNR = <SAIDA>-DOC_CONTABIL
       AND BUKRS = <SAIDA>-EMPRESA.

    IF SY-SUBRC = 0.
      MESSAGE S836(SD) WITH TEXT-005.
      RETURN.
    ENDIF.

   CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = 'Confirmação'
        TEXT_QUESTION         = 'Confirma geração das parcela de Juros?'
        TEXT_BUTTON_1         = 'Sim'
        TEXT_BUTTON_2         = 'Não'
        DEFAULT_BUTTON        = '1'
        DISPLAY_CANCEL_BUTTON = ''
      IMPORTING
        ANSWER                = VAR_ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND        = 1
        OTHERS                = 2.

    CHECK VAR_ANSWER EQ '1'.

    CLEAR: WA_ZFIT0101.
    MOVE WA_TERRENO-EMPRESA        TO WA_ZFIT0101-EMPRESA.
    MOVE WA_TERRENO-LOTEAMENTO     TO WA_ZFIT0101-LOTEAMENTO.
    MOVE WA_TERRENO-NRO_TERRENO    TO WA_ZFIT0101-NRO_TERRENO.
    MOVE WA_TERRENO-NRO_QUADRA     TO WA_ZFIT0101-NRO_QUADRA.
    MOVE <SAIDA>-NRO_PARCELA       TO WA_ZFIT0101-NRO_PARC.
    MOVE 'JR'                      TO WA_ZFIT0101-TIPO.
    MOVE WA_ZFIT0101_AUX-DATA_VENC TO WA_ZFIT0101-DATA_VENC.
    MOVE WA_ZFIT0101_AUX-TX_JUROS  TO WA_ZFIT0101-TX_JUROS.
    MOVE WA_ZFIT0101_AUX-VALOR_JUROS TO WA_ZFIT0101-VALOR_JUROS.
    MOVE VL_COD_EV                 TO WA_ZFIT0101-COD_EV.
    MOVE VL_TP_LCTO                TO WA_ZFIT0101-TP_LCTO.

    "Atribui numero Sequencial para Parcela que irá compor Codigo Barras Boleto.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR                   = '01'
        OBJECT                        = 'ZPARC_LOT'
      IMPORTING
        NUMBER                        = WA_ZFIT0101-NUM_PARC_SEQ
                                .
    IF ( SY-SUBRC <> 0 ) OR ( WA_ZFIT0101-NUM_PARC_SEQ IS INITIAL ).
      ROLLBACK WORK.
      MESSAGE 'Não foi possivel determinar o número sequencial da Parcela!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF WA_ZFIT0101_AUX-TX_JUROS > 0.
      WA_ZFIT0101-VALOR = ( <SAIDA>-VALOR * WA_ZFIT0101_AUX-TX_JUROS ) / 100.
    ELSE.
      WA_ZFIT0101-VALOR = WA_ZFIT0101_AUX-VALOR_JUROS.
    ENDIF.

    MODIFY ZFIT0101 FROM WA_ZFIT0101.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gerar a(s) parcela(s)!' TYPE 'S'.
      EXIT.
    ENDIF.

    COMMIT WORK.

    MESSAGE 'Geração da(s) parcela(s) de juros concluída com sucesso!' TYPE 'S'.

    ME->ATUALIZA_VENDA( ).

    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
  ENDMETHOD.                    "GERAR_JUROS


ENDCLASS.                    "ZCL_LOTEAMENTO IMPLEMENTATION



* CLASS LCL_AlV_Toolbar DEFINITION
* ALV Event Handler
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR
                IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
    ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
               IMPORTING  E_OBJECT,

    HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
               IMPORTING E_UCOMM.

ENDCLASS.

CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_CHANGE.
    TY_TOOLBAR-FUNCTION  = 'MODIFY'.
    TY_TOOLBAR-QUICKINFO = 'Modificar'.
    TY_TOOLBAR-TEXT      = ''.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_SYSTEM_SAVE.
    TY_TOOLBAR-FUNCTION  = 'SAVE'.
    TY_TOOLBAR-QUICKINFO = 'Salvar'.
    TY_TOOLBAR-TEXT      = ''.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_CANCEL.
    TY_TOOLBAR-FUNCTION  = 'CANCELAR'.
    TY_TOOLBAR-QUICKINFO = 'Cancelar'.
    TY_TOOLBAR-TEXT      = ''.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_WIZARD.
    TY_TOOLBAR-FUNCTION  = 'GERAR_CONTABIL'.
    TY_TOOLBAR-QUICKINFO = 'Gerar Documento Contábil'.
    TY_TOOLBAR-TEXT      = 'Gerar Contábil'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_STORNO.
    TY_TOOLBAR-FUNCTION  = 'ESTORNAR_CONTABIL'.
    TY_TOOLBAR-QUICKINFO = 'Estornar Documento Contábil'.
    TY_TOOLBAR-TEXT      = 'Estornar Contábil'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_REFRESH.
    TY_TOOLBAR-FUNCTION  = 'ATUALIZAR'.
    TY_TOOLBAR-QUICKINFO = 'Atualizar'.
    TY_TOOLBAR-TEXT      = 'Atualizar'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_PROFIT_CENTER.
    TY_TOOLBAR-FUNCTION  = 'GERAR_BOLETO'.
    TY_TOOLBAR-QUICKINFO = 'Gerar Boleto'.
    TY_TOOLBAR-TEXT      = 'Gerar Boleto'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_PROFIT_CENTER.
    TY_TOOLBAR-FUNCTION  = 'GERAR_JUROS'.
    TY_TOOLBAR-QUICKINFO = 'Gerar Juros'.
    TY_TOOLBAR-TEXT      = 'Gerar Juros'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_STORNO.
    TY_TOOLBAR-FUNCTION  = 'ELIMINAR_PARCELA'.
    TY_TOOLBAR-QUICKINFO = 'Elimimar Parc. Juros'.
    TY_TOOLBAR-TEXT      = 'Elimimar Parc. Juros'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*   Call reorganize method of toolbar manager to
*   display the toolbar
    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.
    DATA: P_RESP.
    DATA: P_QUESTION TYPE C LENGTH 255.

    CASE E_UCOMM.
      WHEN 'MODIFY'.
        OBJ_LOTEAMENTO->ATUALIZA_VENDA( ).
        PERFORM CHANGE_ROWS USING 'X'.
        VG_MODIFY_PARC = 'X'.
      WHEN 'SAVE'.
        CHECK VG_MODIFY_PARC IS NOT INITIAL.
        PERFORM SALVAR_PARCELAS.
      WHEN 'CANCELAR'.
        OBJ_LOTEAMENTO->ATUALIZA_VENDA( ).
        PERFORM CHANGE_ROWS USING ''.
        CLEAR: VG_MODIFY_PARC.
      WHEN 'GERAR_CONTABIL'.
        CHECK VG_MODIFY_PARC IS INITIAL.
        OBJ_LOTEAMENTO->GERAR_DOC_CONTABIL( ).
      WHEN 'ATUALIZAR'.
        CHECK VG_MODIFY_PARC IS INITIAL.
        OBJ_LOTEAMENTO->REFRESH_DOC_CONTABIL( ).
        OBJ_LOTEAMENTO->ATUALIZA_VENDA( ).
      WHEN 'ESTORNAR_CONTABIL'.
        CHECK VG_MODIFY_PARC IS INITIAL.
        OBJ_LOTEAMENTO->ESTORNO_DOC_CONTABIL( ).
      WHEN 'GERAR_BOLETO'.
        CHECK VG_MODIFY_PARC IS INITIAL.
        OBJ_LOTEAMENTO->GERAR_BOLETO( ).
      WHEN 'GERAR_JUROS'.
        CHECK VG_MODIFY_PARC IS INITIAL.
        PERFORM LANCAR_JUROS.
      WHEN 'ELIMINAR_PARCELA'.
        CHECK VG_MODIFY_PARC IS INITIAL.
        OBJ_LOTEAMENTO->ELIMINAR_PARCELA( ).
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND


ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


* CLASS LCL_AlV_Toolbar DEFINITION
* ALV Event Handler
CLASS LCL_ALV_TOOLBAR_EVENTOS DEFINITION.
  PUBLIC SECTION.
    METHODS: CONSTRUCTOR
                IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
    ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
               IMPORTING  E_OBJECT,

    HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
               IMPORTING E_UCOMM.

ENDCLASS.

CLASS LCL_ALV_TOOLBAR_EVENTOS IMPLEMENTATION.

  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER_EV
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.

    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_DELETE_ROW..
    TY_TOOLBAR-FUNCTION  = 'DELETAR_EVENTO'.
    TY_TOOLBAR-QUICKINFO = ''.
    TY_TOOLBAR-TEXT      = ''.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR TY_TOOLBAR.
    TY_TOOLBAR-ICON      = ICON_EXECUTE_OBJECT.
    TY_TOOLBAR-FUNCTION  = 'GERAR_PARCELAS'.
    TY_TOOLBAR-QUICKINFO = 'Gerar Parcelas'.
    TY_TOOLBAR-TEXT      = 'Gerar Parcelas'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_CANCEL.
    TY_TOOLBAR-FUNCTION  = 'ELIMINAR_PARCELAS'.
    TY_TOOLBAR-QUICKINFO = 'Eliminar Parcelas'.
    TY_TOOLBAR-TEXT      = 'Eliminar Parcelas'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.


*    TY_TOOLBAR-ICON      = ICON_REFRESH.
*    TY_TOOLBAR-FUNCTION  = 'ATUALIZAR'.
*    TY_TOOLBAR-QUICKINFO = 'Atualizar'.
*    TY_TOOLBAR-TEXT      = 'Atualizar'.
*    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

*   Call reorganize method of toolbar manager to
*   display the toolbar
    CALL METHOD C_ALV_TOOLBARMANAGER_EV->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN 'GERAR_PARCELAS'.
        OBJ_LOTEAMENTO->GERAR_PARCELAS( ).
      WHEN 'ELIMINAR_PARCELAS'.
        OBJ_LOTEAMENTO->ESTORNAR_PARCELAS( ).
      WHEN 'DELETAR_EVENTO'.
        PERFORM DELETAR_EVENTO.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND


ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION



*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS: HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "handle_hotspot_click


ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0106 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER_0102 DEFINITION.

  PUBLIC SECTION.                                           "
    CLASS-METHODS:
    ON_F4                      FOR EVENT ONF4                 OF CL_GUI_ALV_GRID
       IMPORTING  E_FIELDNAME
                  ES_ROW_NO
                  ER_EVENT_DATA
                  ET_BAD_CELLS
                  E_DISPLAY.

    CLASS-METHODS:
       ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                       IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM.

    "
ENDCLASS.               "lcl_event_handler_0102 DEFINITION  "

"DATA: EVENT_HANDLER_0106 TYPE REF TO LCL_EVENT_HANDLER_0106. "

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0103 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER_0102 IMPLEMENTATION.                "

  METHOD ON_F4.

    "TYPES
    TYPES: BEGIN OF TY_EVENTO,
             COD_EV      TYPE ZFIT0102-COD_EV,
             NOME_EVENTO TYPE ZFIT0102-NOME_EVENTO,
           END OF TY_EVENTO.

    DATA: LT_EVENTO TYPE TABLE OF TY_EVENTO,
          LS_EVENTO TYPE TY_EVENTO,
          LT_MAP    TYPE TABLE OF DSELC,
          LS_MAP    TYPE DSELC,
          LT_RETURN TYPE TABLE OF DDSHRETVAL,
          LS_RETURN TYPE DDSHRETVAL,
          LS_STABLE TYPE LVC_S_STBL.

    "FIELD-SYMBOLS
    FIELD-SYMBOLS: <L_OUT> TYPE TY_EVENTOS_VENDA. " ALV TABLE LINE


    " CHECK WHICH FIELD RAISE F4 EVENT
    CASE E_FIELDNAME.
      WHEN 'COD_EV'.

      " READ CURRENT LINE
      READ TABLE IT_EVENTOS_VENDA ASSIGNING <L_OUT> INDEX ES_ROW_NO-ROW_ID.

      "LOAD F4 DATA
      SELECT COD_EV NOME_EVENTO
        FROM ZFIT0102
        INTO TABLE LT_EVENTO.

      "SET RETURN FIELD
      CLEAR LS_MAP.
      LS_MAP-FLDNAME = 'F0001'.
      LS_MAP-DYFLDNAME = 'COD_EV'.
      APPEND LS_MAP TO LT_MAP.

      " CALL SEARCH HELP POPUP FUNCTION
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          RETFIELD        = 'COD_EV'
          VALUE_ORG       = 'S'
        TABLES
          VALUE_TAB       = LT_EVENTO
          DYNPFLD_MAPPING = LT_MAP
          RETURN_TAB      = LT_RETURN
        EXCEPTIONS
          PARAMETER_ERROR = 1
          NO_VALUES_FOUND = 2
          OTHERS          = 3.


      " READ SELECTED F4 VALUE
      READ TABLE LT_RETURN INTO LS_RETURN WITH KEY FIELDNAME = 'F0001'.
      IF LS_RETURN IS NOT INITIAL.
        " UPDATE ALV TABLE
        <L_OUT>-COD_EV = LS_RETURN-FIELDVAL.

        CLEAR: WA_ZFIT0102.
        SELECT SINGLE *
          INTO WA_ZFIT0102
          FROM ZFIT0102
         WHERE COD_EV = <L_OUT>-COD_EV.

        <L_OUT>-NOME_EVENTO = WA_ZFIT0102-NOME_EVENTO.
        <L_OUT>-TIPO        = WA_ZFIT0102-TIPO.
        <L_OUT>-TP_LCTO     = WA_ZFIT0102-TP_LCTO.


      ENDIF.
    ENDCASE.

    LS_STABLE = ''. " SET STABLE REFRESH FOR ROW AND COLUMN

    " ALV REFRESH
    CALL METHOD OBJ_ALV_EV_VENDA->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE      = LS_STABLE
        I_SOFT_REFRESH = 'X'
      EXCEPTIONS
        FINISHED       = 1
        OTHERS         = 2.

    " AVOID POSSIBLE STANDARD SEARCH HELP
    ER_EVENT_DATA->M_EVENT_HANDLED = 'X'.

  ENDMETHOD.                    "ON_F4


  METHOD ON_DATA_CHANGED.


  ENDMETHOD.                    "ON_DATA_CHANGED


ENDCLASS.           "lcl_event_handler_0102 IMPLEMENTATION
