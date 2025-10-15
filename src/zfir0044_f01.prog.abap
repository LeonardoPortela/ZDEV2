*----------------------------------------------------------------------*
***INCLUDE ZFIR0044_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS .
  DATA: WL_ZFIT0065 TYPE ZFIT0065,
        WL_ZFIT0063 TYPE ZFIT0063,
        WL_T001     TYPE T001.

  DATA: VBUKRS      TYPE ZFIT0065-BUKRS,
        VNRO_CTO    TYPE ZFIT0065-NRO_CTO,
        VNRO_PAR    TYPE ZFIT0065-NRO_PAR.

  CLEAR XEXISTE.
  VBUKRS      = WG_CADCTO-BUKRS.
  VNRO_CTO    = WG_CADCTO-NRO_CTO.
  VNRO_PAR    = WG_CADCTO-NRO_PAR.
  SELECT SINGLE *
    FROM ZFIT0065
    INTO WL_ZFIT0065
    WHERE BUKRS    = VBUKRS
    AND   NRO_CTO  = VNRO_CTO
    AND   NRO_PAR  = VNRO_PAR.

  IF SY-SUBRC = 0.
    XEXISTE = 'X'.
  ENDIF.

  IF WG_ACAO = C_DISPLA OR WG_ACAO IS INITIAL OR WG_ACAO = C_ADD.
    IF SY-SUBRC = 0.
      XEXISTE = 'X'.
      CLEAR WG_CADCTO.
      MOVE-CORRESPONDING WL_ZFIT0065 TO WG_CADCTO.
      IF WG_ACAO IS INITIAL.
        WG_ACAO = C_DISPLA.
      ENDIF.
    ENDIF.
    WG_CADCTO-BUKRS   = VBUKRS.
    WG_CADCTO-NRO_CTO = VNRO_CTO.
    WG_CADCTO-NRO_PAR = VNRO_PAR.
  ENDIF.

  IF WG_CADCTO-COD_OPER IS NOT INITIAL.
    SELECT SINGLE *
        FROM ZFIT0063
        INTO WL_ZFIT0063
        WHERE COD_OPER = WG_CADCTO-COD_OPER.
    IF SY-SUBRC = 0.
      WG_CADCTO-DESCR_OPER = WL_ZFIT0063-DESCR.
    ENDIF.

  ENDIF.

  IF WG_CADCTO-BUKRS IS NOT INITIAL.
    SELECT SINGLE *
        FROM T001
        INTO WL_T001
        WHERE BUKRS = WG_CADCTO-BUKRS.

    IF SY-SUBRC = 0.
      WG_CADCTO-BUTXT  = WL_T001-BUTXT.
    ENDIF.
  ENDIF.

  IF WG_CADCTO-POSICAO IS NOT INITIAL.
    IF WG_CADCTO-POSICAO = 'C'.
      WG_CADCTO-DESCR_POS   = 'Compra'.
    ENDIF.
    IF WG_CADCTO-POSICAO = 'V'.
      WG_CADCTO-DESCR_POS   = 'Venda'.
    ENDIF.
  ENDIF.

  IF WG_CADCTO-TP_OPERACAO IS NOT INITIAL.
    IF WG_CADCTO-TP_OPERACAO = 'O'.
      WG_CADCTO-DESCR_TP   = 'Operacional'.
    ENDIF.
    IF WG_CADCTO-TP_OPERACAO = 'F'.
      WG_CADCTO-DESCR_TP   = 'Financeiro'.
    ENDIF.
  ENDIF.

  IF WG_CADCTO-TP_TX_ATIVA IS NOT INITIAL.
    IF WG_CADCTO-TP_TX_ATIVA = '1'.
      WG_CADCTO-DESCR_ATIVA   = 'Compostos - 252'.
    ENDIF.
    IF WG_CADCTO-TP_TX_ATIVA = '2'.
      WG_CADCTO-DESCR_ATIVA   = 'Simples - 360'.
    ENDIF.
    IF WG_CADCTO-TP_TX_ATIVA = '3'.
      WG_CADCTO-DESCR_ATIVA   = 'Composto - 360'.
    ENDIF.
    IF WG_CADCTO-TP_TX_ATIVA = '4'.
      WG_CADCTO-DESCR_ATIVA   = 'Juros Simples (365)'.
    ENDIF.
  ENDIF.

  IF WG_CADCTO-TP_TX_PASSIVA IS NOT INITIAL.
    IF WG_CADCTO-TP_TX_PASSIVA = '1'.
      WG_CADCTO-DESCR_PASSIVA   = 'Compostos - 252'.
    ENDIF.
    IF WG_CADCTO-TP_TX_PASSIVA = '2'.
      WG_CADCTO-DESCR_PASSIVA   = 'Simples - 360'.
    ENDIF.
    IF WG_CADCTO-TP_TX_PASSIVA = '3'.
      WG_CADCTO-DESCR_PASSIVA   = 'Composto - 360'.
    ENDIF.
  ENDIF.

ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERIFICA_ERROS .
  REFRESH: TG_MSG_RET.
  CLEAR: TG_MSG_RET.

  DATA: WL_ZFIT0063 TYPE ZFIT0063,
        WL_ZFIT0068 TYPE ZFIT0068,
        WL_TCURC    TYPE TCURC,
        WL_T001     TYPE T001.


  IF WG_CADCTO-BUKRS IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADCTO-BUKRS'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Empresa' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
     FROM T001
     INTO WL_T001
     WHERE BUKRS = WG_CADCTO-BUKRS.

    IF SY-SUBRC NE 0.
      MOVE: TEXT-E02                  TO TG_MSG_RET-MSG,
        'WG_CADCTO-BUKRS'         TO TG_MSG_RET-FIELD.
      CONCATENATE  TG_MSG_RET-MSG 'Empresa' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WG_CADCTO-NRO_CTO IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADCTO-NRO_CTO'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Contrato' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_CADCTO-NRO_PAR IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADCTO-NRO_PAR'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Parcela' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_CADCTO-BANCO IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADCTO-BANCO'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Agente Financeiro' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
      FROM ZFIT0068
      INTO WL_ZFIT0068
      WHERE BANCO = WG_CADCTO-BANCO.

    IF SY-SUBRC NE 0.
      MOVE: TEXT-E02                  TO TG_MSG_RET-MSG,
        'WG_CADCTO-BANCO'         TO TG_MSG_RET-FIELD.
      CONCATENATE  TG_MSG_RET-MSG 'Agente Financeiro' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

  ENDIF.

  IF WG_CADCTO-COD_OPER IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
          'WG_CADCTO-COD_OPER'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Código Operação' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
      FROM ZFIT0063
      INTO WL_ZFIT0063
      WHERE COD_OPER = WG_CADCTO-COD_OPER.

    IF SY-SUBRC NE 0.
      MOVE: TEXT-E02                  TO TG_MSG_RET-MSG,
        'WG_CADCTO-COD_OPER'         TO TG_MSG_RET-FIELD.
      CONCATENATE  TG_MSG_RET-MSG 'Código Operação' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

  ENDIF.

*  IF WG_CADCTO-BANCO IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*          'WG_CADCTO-BANCO'         TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Agente Financeiro' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

  IF WG_CADCTO-POSICAO IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*          'WG_CADCTO-POSICAO'         TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Posição' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
  ELSEIF NOT 'V_C' CS WG_CADCTO-POSICAO.
    MOVE: TEXT-E02                  TO TG_MSG_RET-MSG,
         'WG_CADCTO-POSICAO'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Posição' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

*  IF WG_CADCTO-NATUREZA_CTO IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-NATUREZA_CTO'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Natureza Contrato' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

  IF WG_CADCTO-TP_OPERACAO IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TP_OPERACAO'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Tipo Operação' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
  ELSEIF NOT 'O_F' CS WG_CADCTO-TP_OPERACAO.
    MOVE: TEXT-E02                  TO TG_MSG_RET-MSG,
         'WG_CADCTO-TP_OPERACAO'         TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Tipo Operação' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

*  IF WG_CADCTO-DT_INICIO_CTO IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-DT_INICIO_CTO'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Dt.Inicio Contrato' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

  IF WG_CADCTO-DT_FIM_CTO IS INITIAL .
    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
        'WG_CADCTO-DT_FIM_CTO'    TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Dt.Vencimento Contrato' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_CADCTO-DT_INICIO_CTO GT WG_CADCTO-DT_FIM_CTO .
    MOVE: 'Data Inicio maior que'                  TO TG_MSG_RET-MSG,
        'WG_CADCTO-DT_FIM_CTO'    TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Dt.Vencimento Contrato' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

*  IF WG_CADCTO-VLR_CTO_R IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-VLR_CTO_R'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Valor Contrato  R$' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

*  IF WG_CADCTO-VLR_CTO_US IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-VLR_CTO_US'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Valor Contrato  US$' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

  IF WG_CADCTO-MOEDA IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-MOEDA'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Moeda do Contrato' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
         FROM TCURC
         INTO WL_TCURC
          WHERE  WAERS EQ WG_CADCTO-MOEDA.
    IF SY-SUBRC NE 0.
      CONCATENATE TEXT-E02 ' Moeda' INTO  TG_MSG_RET-MSG.
      MOVE 'WG_CADCTO-MOEDA'         TO TG_MSG_RET-FIELD.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

*  IF WG_CADCTO-TX_CAMBIO_FUT IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TX_CAMBIO_FUT'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Tx. Dólar Futura' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

  IF WG_CADCTO-TP_TX_ATIVA IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TP_TX_ATIVA'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Tipo Taxa – (Ativa)' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
  ELSEIF NOT '1_2_3_4_5' CS WG_CADCTO-TP_TX_ATIVA.
    MOVE: TEXT-E02                  TO TG_MSG_RET-MSG,
         'WG_CADCTO-TP_TX_ATIVA'    TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Tipo Taxa – (Ativa)' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

*  IF WG_CADCTO-TX_CAMBIO_IN_OP IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TX_CAMBIO_IN_OP'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Tx.dolar Inicio Operação' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

*  IF WG_CADCTO-TX_JROS_ATIVA IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TX_JROS_ATIVA'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Tx.Juros- (Ativa)' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

  IF WG_CADCTO-TP_TX_PASSIVA IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TP_TX_PASSIVA'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Tipo Taxa – (Passiva)' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
  ELSEIF NOT '1_2_3' CS WG_CADCTO-TP_TX_PASSIVA.
    MOVE: TEXT-E02                  TO TG_MSG_RET-MSG,
         'WG_CADCTO-TP_TX_PASSIVA'    TO TG_MSG_RET-FIELD.
    CONCATENATE  TG_MSG_RET-MSG 'Tipo Taxa – (Passiva)' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

*  IF WG_CADCTO-TX_JROS_PASSIVA IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TX_JROS_PASSIVA'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Taxa Juros (Passiva)' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

*  IF WG_CADCTO-TX_ACIMA_IND IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TX_ACIMA_IND'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Tx.Acima Indexador%' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.
*
*  IF WG_CADCTO-INDEX_PASSIVO IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-INDEX_PASSIVO'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Indexador (Passivo)' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.
*
*  IF WG_CADCTO-TX_INDEX_PASSIVO IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TX_INDEX_PASSIVO'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG '% Indexador (Passivo)' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.
*
*  IF WG_CADCTO-TX_AC_INDEX_PASS IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*        'WG_CADCTO-TX_AC_INDEX_PASS'    TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Tx.Acima Indexador% (Passivo)' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.


ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAVA_DADOS .
  DATA WL_INPUT_ZFIT0065 TYPE ZFIT0065.

  MOVE-CORRESPONDING WG_CADCTO TO WL_INPUT_ZFIT0065.
  MOVE: SY-MANDT TO WL_INPUT_ZFIT0065-MANDT,
        SY-UNAME TO WL_INPUT_ZFIT0065-USNAM,
        SY-DATUM TO WL_INPUT_ZFIT0065-DATA_ATUAL,
        SY-UZEIT TO WL_INPUT_ZFIT0065-HORA_ATUAL.

  MODIFY ZFIT0065 FROM       WL_INPUT_ZFIT0065.

  MESSAGE S836(SD) WITH 'Contrato'
                         WG_CADCTO-NRO_CTO
                         ', criado/modificado com sucesso!'.

ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*      -->P_0129   text
*      -->P_C_0  text
*      -->P_C_0  text
*----------------------------------------------------------------------*
FORM TRATA_CAMPOS  USING    P_FIELD
                            P_GROUP1
                            P_VALUE
                            P_INVISIBLE.

  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.

ENDFORM.                    " TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIMPA_CAMPOS .
  DATA: VNRO_CTO          TYPE ZFIT0065-NRO_CTO,
        VNRO_PAR          TYPE ZFIT0065-NRO_PAR,
        VBUKRS            TYPE ZFIT0065-BUKRS,
        VBUTXT            TYPE T001-BUTXT.

  VBUKRS   = WG_CADCTO-BUKRS.
  VNRO_CTO = WG_CADCTO-NRO_CTO.
  VNRO_PAR = WG_CADCTO-NRO_PAR.
  VBUTXT   = WG_CADCTO-BUTXT.

  CLEAR: WG_CADCTO.

  WG_CADCTO-BUKRS   = VBUKRS.
  WG_CADCTO-NRO_CTO = VNRO_CTO.
  WG_CADCTO-NRO_PAR = VNRO_PAR.
  WG_CADCTO-BUTXT   = VBUTXT.

ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  ELIMINAR_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ELIMINAR_CONTRATO .
  DELETE FROM ZFIT0065 WHERE BUKRS    = WG_CADCTO-BUKRS
                       AND   NRO_CTO  = WG_CADCTO-NRO_CTO
                       AND   NRO_PAR  = WG_CADCTO-NRO_PAR.
  IF SY-SUBRC = 0.
    MESSAGE S836(SD) WITH 'Contrato'
                           WG_CADCTO-NRO_CTO
                           ', eliminado !'.
  ELSE.
    MESSAGE S836(SD) WITH 'Contrato'
                           WG_CADCTO-NRO_CTO
                           ', erro ao eliminar!'.
  ENDIF.
ENDFORM.                    " ELIMINAR_CONTRATO
