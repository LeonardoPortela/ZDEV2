*----------------------------------------------------------------------*
***INCLUDE LZXML_FERROF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LE_ARQUIVO USING LT_DATA .

  DATA : LCL_XML_DOC TYPE REF TO CL_XML_DOCUMENT,
         V_NODE      TYPE REF TO IF_IXML_NODE,
         V_ITERATOR  TYPE REF TO IF_IXML_NODE_ITERATOR,
         V_NODEMAP   TYPE REF TO IF_IXML_NAMED_NODE_MAP,
         V_ATTR      TYPE REF TO IF_IXML_NODE,
         V_COUNT     TYPE I,
         V_INDEX     TYPE I,
         V_NAME      TYPE STRING,
         V_PREFIX    TYPE STRING,
         V_VALUE     TYPE STRING,
         V_TAG       TYPE STRING,
         V_SUB_TAG   TYPE STRING,
         V_SUB       TYPE CHAR1,
         V_CHAR      TYPE CHAR2,
         V_SUBRC     TYPE SYSUBRC.

  CREATE OBJECT LCL_XML_DOC.

  CALL METHOD LCL_XML_DOC->PARSE_TABLE
    EXPORTING
      TABLE   = LT_DATA
    RECEIVING
      RETCODE = V_SUBRC.

  CHECK V_SUBRC = 0.

  V_NODE = LCL_XML_DOC->M_DOCUMENT.

  CHECK NOT V_NODE IS INITIAL.

  V_ITERATOR = V_NODE->CREATE_ITERATOR( ).
  V_NODE = V_ITERATOR->GET_NEXT( ).

  WHILE NOT V_NODE IS INITIAL.

    CASE V_NODE->GET_TYPE( ).

      WHEN IF_IXML_NODE=>CO_NODE_ELEMENT.

        V_NAME = V_NODE->GET_NAME( ).
        V_NODEMAP = V_NODE->GET_ATTRIBUTES( ).

        IF NOT V_NODEMAP IS INITIAL.
* attributes
          V_COUNT = V_NODEMAP->GET_LENGTH( ).

          DO V_COUNT TIMES.

            V_INDEX = SY-INDEX - 1.
            V_ATTR = V_NODEMAP->GET_ITEM( V_INDEX ).
            V_NAME = V_ATTR->GET_NAME( ).
            V_PREFIX = V_ATTR->GET_NAMESPACE_PREFIX( ).
            V_VALUE = V_ATTR->GET_VALUE( ).

            V_TAG = V_NODE->GET_NAME( ).
          ENDDO.

          PERFORM GET_SUBTAG USING : V_NAME CHANGING V_SUB.
          IF V_SUB EQ 'X'.
            V_SUB_TAG = V_NAME.
          ENDIF.
        ENDIF.

      WHEN IF_IXML_NODE=>CO_NODE_TEXT OR IF_IXML_NODE=>CO_NODE_CDATA_SECTION.
* text node
        V_VALUE = V_NODE->GET_VALUE( ).

        MOVE V_VALUE TO V_CHAR.

        IF V_CHAR <> CL_ABAP_CHAR_UTILITIES=>CR_LF.
          ST_XML_TAB-TAG     = V_TAG.
          ST_XML_TAB-NAME     = V_NAME.
          ST_XML_TAB-VALUE    = V_VALUE.
          ST_XML_TAB-SUB_TAG = V_SUB_TAG.

          APPEND ST_XML_TAB TO XML_TAB.

          CLEAR ST_XML_TAB.

        ENDIF.

    ENDCASE.
* advance to next node
    V_NODE = V_ITERATOR->GET_NEXT( ).

  ENDWHILE.


ENDFORM.                    " LE_ARQUIVO


*&---------------------------------------------------------------------*
*&      Form  TRANSF_ARQUIVO_TABELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TRANSF_ARQUIVO_TABELA .

  DATA : V_SALDO_C        TYPE STRING,
         V_QUANTIDADE_C   TYPE STRING,
         WL_CNPJ          TYPE ZLEST0035-CNPJ,
         WL_CNPJ_1        TYPE ZLEST0035-CNPJ,
         WL_CNPJ_2        TYPE ZLEST0035-CNPJ,
         V_CAMPO_AUX(50),
         TL_NFS           TYPE TABLE OF ZLESE0001 WITH HEADER LINE,
         WL_PESO          TYPE BRGEW,
         WL_PESO_AUX      TYPE BRGEW,
         WL_PESO_R(20),
         WL_PESO_T(20),
         WL_TOMADOR       TYPE ZLEST0035-CNPJ,
         TL_LFA1          TYPE TABLE OF LFA1 WITH HEADER LINE,
         WL_CNPJ_C        TYPE ZLEST0035-CNPJ,
         WL_NR_NF_C       TYPE ZLEST0041-NR_NF,
         WL_SERIE_C       TYPE ZLEST0041-SERIE,
         WL_TIPO(1),
         WL_TABIX         TYPE SY-TABIX,
         WL_LFA1          TYPE LFA1,
         TL_0044          TYPE TABLE OF ZLEST0044 WITH HEADER LINE,
         WL_PESO_FATURADO TYPE ZLEST0044-PESO_BRUTO,
         WL_PESO_AUX2     TYPE ZLEST0044-PESO_BRUTO,
         WL_ERRO,
         WL_PESO_AUX3(20),
         WL_TARIFA(20).
*         wa             type swxml_nitm,
*         tabela         type swxmlnodes,
*         node           type ref to if_ixml_node,
*         noder          type ref to if_ixml_node,
*         v_valor        type string,
*         v_valor2       type ref to if_ixml_named_node_map,
*         v_valor4       type ref to if_ixml_node.

  DATA: I_INICIO TYPE J_1BTXJCD,
        I_FINAL  TYPE J_1BTXJCD.

  CLEAR :ST_ZLEST0045, V_ERRO, ST_ZLEST0044.
  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'refCTE'.
  VCHAVE_CTE = ST_XML_TAB-VALUE.
  IF SY-SUBRC IS NOT INITIAL.
    READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'chCTe' TAG = 'infProt'.
    IF SY-SUBRC IS NOT INITIAL.
      READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'chCTe' TAG = 'protCTe'.
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE 'Verificar versão XML do fornecedor'  TYPE 'E'.
        STOP.
      ENDIF.
    ENDIF.

    VCHAVE_CTE = ST_XML_TAB-VALUE.
  ENDIF.
  ST_ZLEST0044-CHAVE_CTE  := VCHAVE_CTE.

  "CNPJ
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = VCHAVE_CTE+6(14)
    IMPORTING
      OUTPUT = V_CNPJ_ALL.

  SELECT SINGLE LIFNR
    INTO VCOD_FORNECEDOR
    FROM LFA1
   WHERE STCD1 = V_CNPJ_ALL.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'nCT'.
  ST_ZLEST0044-NR_CTE  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'serie'.
  ST_ZLEST0044-SERIE  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'dhEmi'.

  CONCATENATE ST_XML_TAB-VALUE(4) ST_XML_TAB-VALUE+5(2) ST_XML_TAB-VALUE+8(2) INTO ST_ZLEST0044-DATA_EMISAO.

** Data de emisao do custo de frete ( VI01 )
  REFRESH: TG_NODE_PATH.
  TG_NODE_PATH-PATH = '/cteProc/CTe/infCte'.
  APPEND TG_NODE_PATH.

  TG_NODE_PATH-PATH = '/cteProc/CTe/infCte/compl/ObsCont'.
  APPEND TG_NODE_PATH.
  CLEAR: WG_DATA_VI.
  PERFORM GET_VALUE_XML TABLES TG_NODE_PATH
                         USING 'xCampo'
                               'DATA:'
                      CHANGING WG_DATA_VI.
  IF WG_DATA_VI IS INITIAL.
    WG_DATA_VI = ST_ZLEST0044-DATA_EMISAO.
  ELSE.
    TRANSLATE WG_DATA_VI USING '/ '.
    CONDENSE WG_DATA_VI NO-GAPS.
    CONCATENATE WG_DATA_VI+4(4) WG_DATA_VI+2(2) WG_DATA_VI(2) INTO WG_DATA_VI.
    MOVE: WG_DATA_VI TO ST_ZLEST0044-DT_REFERENCIA.

  ENDIF.
**<
**> Pega data de vencimento do XML
  CLEAR: V_CAMPO_AUX.
  PERFORM GET_VALUE_XML TABLES TG_NODE_PATH
                           USING 'xCampo'
                                 'VENCIMENTO:'
                        CHANGING V_CAMPO_AUX.

  TRANSLATE V_CAMPO_AUX USING '/ '.
  CONDENSE V_CAMPO_AUX NO-GAPS.
  CONCATENATE V_CAMPO_AUX+4(4) V_CAMPO_AUX+2(2) V_CAMPO_AUX(2) INTO ST_ZLEST0044-DT_VENC.
**<


  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'nProt'.
  ST_ZLEST0044-NR_PROTOCOLO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'CFOP'.
  ST_ZLEST0044-CFOP  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'natOp'.
  ST_ZLEST0044-NAT_OPER  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'mod'.
  ST_ZLEST0044-MODELO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'forPag'.
  ST_ZLEST0044-FORMA_PGTO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'tpCTe'.
  ST_ZLEST0044-TP_CTE  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'tpServ'.
  ST_ZLEST0044-TP_SERVICO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'cMunIni'.
  ST_ZLEST0044-CIDADE_ORIGEM  := ST_XML_TAB-VALUE.


  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'UFIni'.
  ST_ZLEST0044-UF_ORIGEM  := ST_XML_TAB-VALUE.

*  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'cMunFim'.
*  ST_ZLEST0044-CIDADE_DESTINO  := ST_XML_TAB-VALUE.
  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'cMun' TAG = 'ObsCont' SUB_TAG = 'enderReceb'.
  ST_ZLEST0044-CIDADE_DESTINO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'UFFim'.
  ST_ZLEST0044-UF_DESTINO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'toma' SUB_TAG = 'toma4'.
  ST_ZLEST0044-TOMADOR  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY TAG = 'ObsCont' SUB_TAG = 'vPrest' NAME = 'xNome' VALUE = 'TARIFA'.
  IF SY-SUBRC IS NOT INITIAL.
    READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY TAG = 'ObsCont' SUB_TAG = 'vPrest' NAME = 'xNome' VALUE = 'Tarifa'.
  ENDIF.
  IF SY-SUBRC IS INITIAL.
    WL_TABIX = SY-TABIX + 1.
    READ TABLE XML_TAB INTO ST_XML_TAB INDEX WL_TABIX.
    IF ST_XML_TAB-TAG     EQ 'ObsCont'
   AND ST_XML_TAB-SUB_TAG EQ 'vPrest'
   AND ST_XML_TAB-NAME    EQ 'vComp'.
      MOVE  ST_XML_TAB-VALUE TO ST_ZLEST0044-TARIFA.


    ENDIF.
  ENDIF.




  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'xNome' SUB_TAG = 'emit'.
  ST_ZLEST0044-EMITENTE  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'CNPJ' SUB_TAG = 'emit'.
  ST_ZLEST0044-CNPJ_EMITENTE  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'IE' SUB_TAG = 'emit'.
  ST_ZLEST0044-IE_EMITENTE  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'xNome' SUB_TAG = 'rem'.
  ST_ZLEST0044-REMETENTE  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'CNPJ' SUB_TAG = 'rem'.
  ST_ZLEST0044-CNPJ_REMETENTE  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'IE' SUB_TAG = 'rem'.
  ST_ZLEST0044-IE_REMETENTE  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'xNome' SUB_TAG = 'dest'.
  ST_ZLEST0044-DESTINATARIO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'CNPJ' SUB_TAG = 'dest'.
  ST_ZLEST0044-CNPJ_DESTINATARI  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'IE' SUB_TAG = 'dest'.
  ST_ZLEST0044-IE_DESTINATARIO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'xNome' SUB_TAG = 'exped'.
  ST_ZLEST0044-EXPEDIDOR  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'CNPJ' SUB_TAG = 'exped'.
  ST_ZLEST0044-CNPJ_EXPEDIDOR  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'IE' SUB_TAG = 'exped'.
  ST_ZLEST0044-IE_EXPEDIDOR := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'xNome' SUB_TAG = 'receb'.
  ST_ZLEST0044-RECEBEDOR  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'CNPJ' SUB_TAG = 'receb'.
  ST_ZLEST0044-CNPJ_RECEBEDOR  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'IE' SUB_TAG = 'receb'.
  ST_ZLEST0044-IE_RECEBEDOR  := ST_XML_TAB-VALUE.

******

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'toma' SUB_TAG = 'ide'.
  IF SY-SUBRC IS INITIAL.
    WL_TIPO  := ST_XML_TAB-VALUE.
  ELSE.
    CLEAR: WL_TIPO.
  ENDIF.
  CASE WL_TIPO.
    WHEN '0'.
      ST_ZLEST0044-CNPJ_TOMADOR = ST_ZLEST0044-CNPJ_REMETENTE.
      ST_ZLEST0044-IE_TOMADOR   = ST_ZLEST0044-IE_REMETENTE.
    WHEN '1'.
      ST_ZLEST0044-CNPJ_TOMADOR = ST_ZLEST0044-CNPJ_EXPEDIDOR.
      ST_ZLEST0044-IE_TOMADOR   = ST_ZLEST0044-IE_EXPEDIDOR.
    WHEN '2'.
      ST_ZLEST0044-CNPJ_TOMADOR = ST_ZLEST0044-CNPJ_RECEBEDOR.
      ST_ZLEST0044-IE_TOMADOR   = ST_ZLEST0044-IE_RECEBEDOR.
    WHEN '3'.
      ST_ZLEST0044-CNPJ_TOMADOR = ST_ZLEST0044-CNPJ_DESTINATARI.
      ST_ZLEST0044-IE_TOMADOR   = ST_ZLEST0044-IE_DESTINATARIO.
    WHEN OTHERS.
      READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'CNPJ' SUB_TAG = 'toma4'.
      ST_ZLEST0044-CNPJ_TOMADOR  := ST_XML_TAB-VALUE.

      READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'IE' SUB_TAG = 'toma4'.
      ST_ZLEST0044-IE_TOMADOR  := ST_XML_TAB-VALUE.

  ENDCASE.
****
  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'CST'.
  ST_ZLEST0044-SIT_TRIB  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'proPred'.
  ST_ZLEST0044-PRODUTO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'xObs'.
  ST_ZLEST0044-OBSERVACAO  := ST_XML_TAB-VALUE.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'qCarga'.

  PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE
                                CHANGING ST_ZLEST0044-PESO_BRUTO.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'vCarga'.

  PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE
                                CHANGING ST_ZLEST0044-VLR_MERC.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = 'vFrete'.
  PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE
                                CHANGING ST_ZLEST0044-VLR_SERV.

  READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY NAME = ''.
  PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE
                                CHANGING ST_ZLEST0044-VLR_REC.

  ST_ZLEST0044-CANCELADO = ''.
  ST_ZLEST0044-DATA      = SY-DATUM.
  ST_ZLEST0044-HORA      = SY-UZEIT.
  ST_ZLEST0044-USUARIO   = SY-UNAME.
  "st_zlest0044-status   = 'L'.

  CLEAR: WL_PESO, WL_PESO_AUX.

  LOOP AT XML_TAB INTO ST_XML_TAB
     WHERE TAG     EQ 'infModal'
       AND SUB_TAG EQ 'ratNFe'
       AND NAME    EQ 'pesoRat'.

    PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE
                                    CHANGING WL_PESO_AUX.
    ADD WL_PESO_AUX TO WL_PESO.

  ENDLOOP.

  LOOP AT XML_TAB INTO ST_XML_TAB
     WHERE TAG     EQ 'ObsCont'
       AND SUB_TAG EQ 'infNFe'
       AND NAME    EQ 'qtdRat'.

    PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE
                                    CHANGING WL_PESO_AUX.
    ADD WL_PESO_AUX TO WL_PESO.

  ENDLOOP.


  CLEAR: WL_PESO_AUX.

  READ TABLE XML_TAB INTO ST_XML_TAB
    WITH KEY TAG     = 'infModal'
             SUB_TAG = 'ferrov'
             NAME    = 'pesoR'.
  IF SY-SUBRC IS INITIAL.
    PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE
                                  CHANGING WL_PESO_AUX.
  ENDIF.


  "Validação da Tarifa para o XML da Vale.
  IF  ST_ZLEST0044-TARIFA  IS INITIAL.
    READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY TAG  = 'ObsCont' SUB_TAG = 'vPrest' NAME    = 'vComp'.
    ST_ZLEST0044-TARIFA = ( ST_XML_TAB-VALUE / WL_PESO_AUX ).
  ENDIF.

  IF WL_PESO NE WL_PESO_AUX.
    WL_PESO_R = WL_PESO_AUX.
    SHIFT WL_PESO_R LEFT DELETING LEADING '0'.
    CONDENSE WL_PESO_R NO-GAPS.

    WL_PESO_T = WL_PESO.
    SHIFT WL_PESO_T LEFT DELETING LEADING '0'.
    CONDENSE WL_PESO_T NO-GAPS.

    CONCATENATE 'Peso real :' WL_PESO_R 'difere peso rateio:' WL_PESO_T  INTO V_MSG_PROC SEPARATED BY SPACE.
    PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                             V_SERIE_NF_CHAVE
                                             V_CNPJ_CHAVE
                                             V_NUM_NF_CHAVE
                                             V_MSG_PROC
                                             'E'
                                             VCHAVE_CTE
                                             VCOD_FORNECEDOR.


  ENDIF.
*  READ TABLE XML_TAB INTO ST_XML_TAB
*    WITH KEY TAG     = 'infCte'
*             SUB_TAG = 'toma4'
*             NAME    = 'CNPJ'.

*  IF SY-SUBRC IS INITIAL.
  WL_TOMADOR = ST_ZLEST0044-CNPJ_TOMADOR.
  LOOP AT XML_TAB INTO ST_XML_TAB
      WHERE TAG EQ 'infModal'
        AND SUB_TAG EQ 'ratNFe'
        AND NAME    EQ 'chave'.

    IF ST_XML_TAB-VALUE+6(14) NE WL_TOMADOR.
      WL_CNPJ_C = ST_XML_TAB-VALUE+6(14).
      WL_NR_NF_C = ST_XML_TAB-VALUE+25(9).
      WL_SERIE_C = ST_XML_TAB-VALUE+22(3).
      CALL FUNCTION 'Z_LES_BUSCA_TIPO_CNPJ'
        EXPORTING
          I_CNPJ  = WL_CNPJ_C
          I_NR_NF = WL_NR_NF_C
          I_SERIE = WL_SERIE_C
*         IMPORTING
*         E_TIPO_CNPJ       =
        TABLES
          TE_LFA1 = TL_LFA1.

      READ TABLE TL_LFA1
        WITH KEY STCD1 = WL_TOMADOR.

      IF SY-SUBRC IS NOT INITIAL.
        READ TABLE TL_LFA1 INDEX 1.
        CONCATENATE 'Tomador:'  WL_TOMADOR  'diferente'  TL_LFA1-STCD1 INTO V_MSG_PROC SEPARATED BY SPACE.
        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                                 V_SERIE_NF_CHAVE
                                                 V_CNPJ_CHAVE
                                                 V_NUM_NF_CHAVE
                                                 V_MSG_PROC
                                                 'E'
                                                 VCHAVE_CTE
                                                 VCOD_FORNECEDOR.
      ENDIF.

    ENDIF.
  ENDLOOP.
*  ENDIF.

  PERFORM VALIDA_CTE.

  "-------->Rateios<--------
  DATA: VAR_TABIX   TYPE SY-TABIX.

  "Inicio - Ajuste para arrumar a cagada que o programador fez abaixo.
  CLEAR: XML_TAB_NOTAS[].
  LOOP AT XML_TAB INTO ST_XML_TAB
    WHERE TAG     EQ 'ObsCont'
      AND SUB_TAG EQ 'infNFe'
      AND NAME    EQ 'chave'.

    VAR_TABIX = SY-TABIX.

    ST_XML_TAB_NOTAS-TAG     = ST_XML_TAB-TAG.
    ST_XML_TAB_NOTAS-SUB_TAG = ST_XML_TAB-SUB_TAG.
    ST_XML_TAB_NOTAS-NAME    = ST_XML_TAB-NAME.
    ST_XML_TAB_NOTAS-VALUE   = ST_XML_TAB-VALUE.

    APPEND ST_XML_TAB_NOTAS TO XML_TAB_NOTAS.
    DELETE XML_TAB INDEX VAR_TABIX.

    READ TABLE XML_TAB INTO ST_XML_TAB WITH KEY TAG     = 'ObsCont'
                                                SUB_TAG = 'infNFe'
                                                NAME    = 'qtdRat'.

    IF ( SY-SUBRC EQ 0 ).
      VAR_TABIX = SY-TABIX.
      ST_XML_TAB_NOTAS-TAG     = ST_XML_TAB-TAG.
      ST_XML_TAB_NOTAS-SUB_TAG = ST_XML_TAB-SUB_TAG.
      ST_XML_TAB_NOTAS-NAME    = ST_XML_TAB-NAME.
      ST_XML_TAB_NOTAS-VALUE   = ST_XML_TAB-VALUE.


      APPEND ST_XML_TAB_NOTAS TO XML_TAB_NOTAS.
      DELETE XML_TAB INDEX VAR_TABIX.
    ENDIF.

    CLEAR: VAR_TABIX, ST_XML_TAB_NOTAS, ST_XML_TAB.

  ENDLOOP.

  DELETE XML_TAB WHERE TAG <> 'infModal' .

  LOOP AT XML_TAB_NOTAS INTO ST_XML_TAB_NOTAS.
    ST_XML_TAB-TAG     = 'infModal'.
    ST_XML_TAB-SUB_TAG = ST_XML_TAB_NOTAS-SUB_TAG.
    ST_XML_TAB-NAME    = ST_XML_TAB_NOTAS-NAME.
    ST_XML_TAB-VALUE   = ST_XML_TAB_NOTAS-VALUE.

    APPEND ST_XML_TAB TO XML_TAB.
    CLEAR:  ST_XML_TAB, ST_XML_TAB_NOTAS.
  ENDLOOP.
  "FIM - Ajuste para arrumar a cagada que o programador fez abaixo.

  CLEAR: V_CHAVE, V_VAGAO, V_TP_VAGAO, V_PESO_REAL, V_PESO_RATEADO.

  REFRESH: T_ITEMDATA.
  CLEAR :ST_ZLEST0045.

  LOOP AT XML_TAB INTO ST_XML_TAB .

    IF ST_XML_TAB-NAME = 'chave'.
      V_CHAVE  = ST_XML_TAB-VALUE.
    ENDIF.

    IF ST_XML_TAB-NAME = 'nVag'.
      V_VAGAO  = ST_XML_TAB-VALUE.

    ENDIF.

    IF ST_XML_TAB-NAME = 'tpVag'.
      V_TP_VAGAO  = ST_XML_TAB-VALUE.
    ENDIF.


    IF ST_XML_TAB-NAME = 'pesoR'.
      PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE CHANGING V_PESO_REAL.
    ENDIF.

    IF ST_XML_TAB-NAME = 'pesoRat'.
      PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE CHANGING V_PESO_RATEADO.
    ENDIF.

    IF ST_XML_TAB-NAME = 'qtdRat'.
      PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE CHANGING V_PESO_RATEADO.
    ENDIF.



    IF V_CHAVE IS NOT INITIAL
               AND V_VAGAO        IS NOT INITIAL
               AND V_TP_VAGAO     IS NOT INITIAL
               AND V_PESO_REAL    IS NOT INITIAL
               AND V_PESO_RATEADO IS NOT INITIAL.

*      IF TL_0070[] IS NOT INITIAL.
*        READ TABLE T
*      ENDIF.

      ST_ZLEST0045-CHAVE_CTE    = VCHAVE_CTE.
      ST_ZLEST0045-CHAVE        = V_CHAVE.
      ST_ZLEST0045-NR_VAGAO     = V_VAGAO.
      ST_ZLEST0045-TP_VAGAO     = V_TP_VAGAO.
      ST_ZLEST0045-PESO_REAL    = V_PESO_REAL * 1000.
      ST_ZLEST0045-PESO_RATEADO = V_PESO_RATEADO * 1000.

      V_CD_UF_CHAVE    = V_CHAVE(2).
      V_ANO_MES_EMIS   = V_CHAVE+2(4).

      CLEAR : V_CNPJ_CHAVE,V_MOD_NF_CHAVE,V_SERIE_NF_CHAVE, V_NUM_NF_CHAVE, V_CD_UF_CHAVE, V_ANO_MES_EMIS, V_UTILIZADO.

      "CNPJ
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = V_CHAVE+6(14)
        IMPORTING
          OUTPUT = V_CNPJ_CHAVE.

      "Modelo
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = V_CHAVE+20(2)
        IMPORTING
          OUTPUT = V_MOD_NF_CHAVE.

      "Serie
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = V_CHAVE+22(3)
        IMPORTING
          OUTPUT = V_SERIE_NF_CHAVE.

      "Numero NF
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = V_CHAVE+25(9)
        IMPORTING
          OUTPUT = V_NUM_NF_CHAVE.



      CLEAR : ST_LFA1, ST_LFA2, V_DOCNUM, TL_NFS, WL_CNPJ.
      REFRESH: TL_NFS.

      MOVE: V_CNPJ_CHAVE TO WL_CNPJ.
      MOVE: V_SERIE_NF_CHAVE TO TL_NFS-SERIE_NF_I,
            V_NUM_NF_CHAVE   TO TL_NFS-NR_NF_I.

      APPEND TL_NFS.

      CALL FUNCTION 'Z_LES_REMETENTE_TERCEIRO'
        EXPORTING
          CNPJ              = WL_CNPJ
        TABLES
          TI_NFS            = TL_NFS
        EXCEPTIONS
          REMETENTE_PROPRIO = 1
          NFS_NOT_FOUND     = 2
          OTHERS            = 3.

      CASE SY-SUBRC.
        WHEN '0'.
          CLEAR: TL_NFS.
          READ TABLE TL_NFS INDEX 1.
          IF TL_NFS-NR_NF_E IS NOT INITIAL
          AND TL_NFS-SERIE_NF_E IS NOT INITIAL.
            MOVE: TL_NFS-NR_NF_E    TO V_NUM_NF_CHAVE,
                  TL_NFS-SERIE_NF_E TO V_SERIE_NF_CHAVE,
                  V_CNPJ_ALL        TO WL_CNPJ_1,
                  V_CNPJ_CHAVE      TO WL_CNPJ_2,
*                  TL_NFS-CNPJ_E     TO WL_CNPJ_2,
                  TL_NFS-CNPJ_E     TO V_CNPJ_ALL,
                  TL_NFS-CNPJ_E     TO V_CNPJ_CHAVE.

          ENDIF.
        WHEN '1'.
*           DO NOTHING !
        WHEN '2'.
      ENDCASE.


      SELECT SINGLE LIFNR
                    STCD1
        INTO ST_LFA1
        FROM LFA1
       WHERE STCD1 EQ V_CNPJ_ALL.

      SELECT SINGLE LIFNR
                    STCD1
        INTO ST_LFA2
        FROM LFA1
       WHERE STCD1 EQ V_CNPJ_CHAVE.

      IF SY-SUBRC IS INITIAL.
        CLEAR: V_BUKRS.
*        SELECT SINGLE VKORG
*          FROM T001W
*          INTO V_BUKRS
*           WHERE WERKS EQ ST_LFA2-LIFNR+6(4).
        SELECT SINGLE BUKRS
           FROM T001K
           INTO V_BUKRS
            WHERE BWKEY EQ ST_LFA2-LIFNR+6(4).
      ENDIF.

      SELECT SINGLE DOCNUM BUKRS BRANCH
        FROM J_1BNFDOC
        INTO (V_DOCNUM,V_BUKRS,V_BRANCH )
       WHERE "PARID  EQ ST_LFA2-LIFNR
             BRANCH EQ ST_LFA2-LIFNR+6(4)
         AND MODEL  EQ V_MOD_NF_CHAVE
         AND SERIES EQ V_SERIE_NF_CHAVE
         AND NFENUM EQ V_NUM_NF_CHAVE
         AND BUKRS  EQ V_BUKRS
         AND DIRECT EQ 2.

      IF SY-SUBRC IS NOT INITIAL .

        SELECT SINGLE DOCNUM BUKRS BRANCH
          FROM J_1BNFDOC
          INTO (V_DOCNUM,V_BUKRS,V_BRANCH )
         WHERE "PARID  EQ ST_LFA2-LIFNR
               BRANCH EQ ST_LFA2-LIFNR+6(4)
           AND MODEL  EQ V_MOD_NF_CHAVE
           AND SERIES EQ V_SERIE_NF_CHAVE+2(1)
           AND NFENUM EQ V_NUM_NF_CHAVE
          AND BUKRS  EQ V_BUKRS
          AND DIRECT EQ 2.

      ENDIF.

      "IF SY-SUBRC IS INITIAL.
      IF NOT V_DOCNUM IS INITIAL.
        CLEAR : V_MSG_PROC, ST_ZLEST0035, V_SALDO, WL_CNPJ.

        IF WL_CNPJ_1 IS NOT INITIAL
        AND WL_CNPJ_2 IS NOT INITIAL.
          MOVE: WL_CNPJ_2    TO V_CNPJ_CHAVE,
                WL_CNPJ_1    TO V_CNPJ_ALL.

        ENDIF.

        MOVE:  V_CNPJ_CHAVE TO WL_CNPJ.
*        perform zverifica_saldo using v_docnum v_cnpj_all changing v_saldo.
        CALL FUNCTION 'Z_LES_BUSCA_SALDO_FERROVIARIO'
          EXPORTING
            NR_NF              = V_NUM_NF_CHAVE
            SERIE_NF           = V_SERIE_NF_CHAVE
            CNPJ               = WL_CNPJ
*           docnum             = v_docnum
          IMPORTING
            SALDO              = V_SALDO
          EXCEPTIONS
            QTD_CHEG_NOT_FOUND = 1.
*             OTHERS                   = 2

        IF SY-SUBRC <> 0.
          CLEAR: V_SALDO.
        ENDIF.

        "Erro sem Saldo
        IF ST_ZLEST0045-PESO_RATEADO > V_SALDO.

          V_SALDO_C = V_SALDO.
          V_QUANTIDADE_C = ST_ZLEST0045-PESO_RATEADO.

          CONCATENATE TEXT-008 V_NUM_NF_CHAVE TEXT-009 V_SALDO_C TEXT-010 V_QUANTIDADE_C INTO V_MSG_PROC.

          PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                         V_SERIE_NF_CHAVE
                                         V_CNPJ_CHAVE
                                         V_NUM_NF_CHAVE
                                         V_MSG_PROC
                                         'E'
                                         VCHAVE_CTE
                                         VCOD_FORNECEDOR.
        ELSE.
          ST_ZLEST0035-SALDO = ST_ZLEST0035-SALDO - ST_ZLEST0045-PESO_RATEADO.

        ENDIF.


        PERFORM ZF_PREENCHE_ITENS_BAPI.

        ST_ZLEST0045-DOCNUM = V_DOCNUM.
        ST_ZLEST0045-BUKRS  = V_BUKRS .
        ST_ZLEST0045-BRANCH = V_BRANCH.

        CLEAR : V_PESO_RATEADO, V_CHAVE.

        APPEND ST_ZLEST0045 TO T_ZLEST0045.
        "Nos casos de remessa por conta e ordem
      ELSE.

        SELECT SINGLE *
          FROM ZLEST0041
          INTO ST_ZLEST0041
         WHERE NR_NF_PROPRIA EQ V_NUM_NF_CHAVE
           AND COD_CLIENTE   EQ ST_LFA1-LIFNR.

        IF SY-SUBRC IS INITIAL.
          CLEAR: WL_CNPJ.
          MOVE: WL_CNPJ_2    TO V_CNPJ_CHAVE,
                WL_CNPJ_1    TO V_CNPJ_ALL,
                V_CNPJ_CHAVE TO WL_CNPJ.
*          perform zverifica_saldo using st_zlest0041-docnum v_cnpj_all changing v_saldo.

          CALL FUNCTION 'Z_LES_BUSCA_SALDO_FERROVIARIO'
            EXPORTING
              NR_NF              = V_NUM_NF_CHAVE
              SERIE_NF           = V_SERIE_NF_CHAVE
              CNPJ               = WL_CNPJ
              DOCNUM             = V_DOCNUM
            IMPORTING
              SALDO              = V_SALDO
            EXCEPTIONS
              QTD_CHEG_NOT_FOUND = 1.
*             OTHERS                   = 2

          IF SY-SUBRC <> 0.
            CLEAR: V_SALDO.
          ENDIF.

          IF ST_ZLEST0045-PESO_RATEADO > V_SALDO.

            V_SALDO_C = V_SALDO.
            V_QUANTIDADE_C = ST_ZLEST0045-PESO_RATEADO.

            CONCATENATE TEXT-008 V_NUM_NF_CHAVE TEXT-009 V_SALDO_C TEXT-010 V_QUANTIDADE_C INTO V_MSG_PROC.

            PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                           V_SERIE_NF_CHAVE
                                           V_CNPJ_CHAVE
                                           V_NUM_NF_CHAVE
                                           TEXT-008
                                           'E'
                                           VCHAVE_CTE
                                           VCOD_FORNECEDOR.
          ENDIF.


          ST_ZLEST0045-DOCNUM = ST_ZLEST0041-DOCNUM.
          ST_ZLEST0045-BRANCH  = ST_ZLEST0041-CENTRO_COMPRADOR.

          SELECT SINGLE BUKRS
            FROM  J_1BBRANCH
            INTO ST_ZLEST0045-BRANCH
           WHERE BRANCH = ST_ZLEST0045-BRANCH.

          APPEND ST_ZLEST0045 TO T_ZLEST0045.

        ELSE.

          PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                         V_SERIE_NF_CHAVE
                                         V_CNPJ_CHAVE
                                         V_NUM_NF_CHAVE
                                         TEXT-001
                                         'E'
                                         VCHAVE_CTE
                                         VCOD_FORNECEDOR.
        ENDIF.

      ENDIF.

      IF ST_ZLEST0035 IS NOT INITIAL .
        APPEND ST_ZLEST0035 TO T_ZLEST0035.
      ENDIF.

    ENDIF.
  ENDLOOP.

  ST_ZLEST0044-BUKRS  = ST_ZLEST0045-BUKRS.
  ST_ZLEST0044-BRANCH = ST_ZLEST0045-BRANCH.
  ST_ZLEST0044-STATUS = 'L'.

  SELECT SINGLE * INTO WL_LFA1
    FROM LFA1
   WHERE STCD1 EQ ST_ZLEST0044-CNPJ_EMITENTE
     AND STCD3 EQ ST_ZLEST0044-IE_EMITENTE.

  IF SY-SUBRC IS NOT INITIAL.
    WL_ERRO = C_X.
    CONCATENATE 'Fornecedor com CNPJ e IE não encontrado:' ST_ZLEST0044-CNPJ_EMITENTE ST_ZLEST0044-IE_EMITENTE INTO V_MSG_PROC SEPARATED BY SPACE.
    PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE V_SERIE_NF_CHAVE V_CNPJ_CHAVE V_NUM_NF_CHAVE V_MSG_PROC 'E' VCHAVE_CTE VCOD_FORNECEDOR.
  ELSE.
    CONCATENATE ST_ZLEST0044-UF_ORIGEM  ST_ZLEST0044-CIDADE_ORIGEM  INTO I_INICIO SEPARATED BY SPACE.
    CONCATENATE ST_ZLEST0044-UF_DESTINO ST_ZLEST0044-CIDADE_DESTINO INTO I_FINAL  SEPARATED BY SPACE.

    CALL METHOD ZCL_CTE_DIST_G=>VERIFICA_VOLUME_PRECO_FERRO
      EXPORTING
        I_FORNECEDOR          = WL_LFA1-LIFNR
        I_INICIO              = I_INICIO
        I_FINAL               = I_FINAL
        I_VLR_TARIFA          = ST_ZLEST0044-TARIFA
        I_TOMADOR             = ST_ZLEST0044-BUKRS
        I_DT_REFERENCIA       = ST_ZLEST0044-DT_REFERENCIA
        I_QTD_FATURAR         = ST_ZLEST0044-PESO_BRUTO
        I_TOMADOR_CENTRO      = ST_ZLEST0044-BRANCH
      EXCEPTIONS
        SEM_ITINERARIO        = 1
        SEM_VOLUME            = 2
        SEM_VOLUME_EMPRESA    = 3
        SEM_VOLUME_DISPONIVEL = 4
        OTHERS                = 5.

    IF SY-SUBRC IS NOT INITIAL.
      WL_ERRO = C_X.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO V_MSG_PROC.
      PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE V_SERIE_NF_CHAVE V_CNPJ_CHAVE V_NUM_NF_CHAVE V_MSG_PROC 'E' VCHAVE_CTE VCOD_FORNECEDOR.
    ENDIF.
  ENDIF.

*  IF SY-SUBRC IS INITIAL.
*
*    SELECT SINGLE *
*      FROM ZLEST0069
*      INTO WL_0069
*       WHERE LIFNR EQ WL_LFA1-LIFNR.
*
*    IF SY-SUBRC IS INITIAL.
*      SELECT  *
*        FROM ZLEST0070
*        INTO TABLE TL_0070
*         WHERE LIFNR          EQ WL_LFA1-LIFNR
*           AND CIDADE_ORIGEM  EQ ST_ZLEST0044-CIDADE_ORIGEM
*           AND CIDADE_DESTINO EQ ST_ZLEST0044-CIDADE_DESTINO
*           AND WAERK          EQ 'BRL'
*           AND NETPR          EQ ST_ZLEST0044-TARIFA
*           AND UNID_MED_NETPR EQ 'TO'
*           AND ( DT_INICIO     LE WG_DATA_VI
*            AND DT_FIM         GE WG_DATA_VI ).
*
*      IF SY-SUBRC IS INITIAL.
*        READ TABLE XML_TAB INTO ST_XML_TAB
*          WITH KEY NAME = 'pesoR'.
*        IF SY-SUBRC IS INITIAL.
*          PERFORM CONVERTE_CHAR_DECIMAL USING ST_XML_TAB-VALUE CHANGING V_PESO_REAL.
*          MULTIPLY V_PESO_REAL BY 1000.
*
*          SELECT SUM( PESO_BRUTO )
*           FROM ZLEST0044
*           INTO WL_PESO_FATURADO
**           FOR ALL ENTRIES IN TL_0070
*             WHERE DT_REFERENCIA  EQ WG_DATA_VI
*               AND CNPJ_EMITENTE  EQ WL_LFA1-STCD1
*               AND CIDADE_ORIGEM  EQ ST_ZLEST0044-CIDADE_ORIGEM
*               AND CIDADE_DESTINO EQ ST_ZLEST0044-CIDADE_DESTINO
*               AND TARIFA         EQ ST_ZLEST0044-TARIFA
*               AND NR_TRANS       NE SPACE
*               AND NR_FRETE       NE SPACE.
*
*          WL_ERRO = C_X.
*          LOOP AT TL_0070.
*            CLEAR: WL_PESO_AUX2.
*            WL_PESO_AUX2 = TL_0070-PESO + TL_0070-PESO_TOLER - WL_PESO_FATURADO.
*
*            IF V_PESO_REAL GT WL_PESO_AUX2.
*              CONTINUE.
*            ELSE.
*              CLEAR: WL_ERRO.
*              EXIT.
*            ENDIF.
*          ENDLOOP.
*
*          IF WL_ERRO IS NOT INITIAL.
*            WRITE V_PESO_REAL TO  WL_PESO_AUX3.
*            CONDENSE WL_PESO_AUX3 NO-GAPS.
*            CONCATENATE 'Peso Real:' WL_PESO_AUX3 'menor que o saldo a faturar.' INTO V_MSG_PROC SEPARATED BY SPACE.
*            PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
*                                                   V_SERIE_NF_CHAVE
*                                                   V_CNPJ_CHAVE
*                                                   V_NUM_NF_CHAVE
*                                                   V_MSG_PROC
*                                                   'E'
*                                                   VCHAVE_CTE
*                                                   VCOD_FORNECEDOR.
*          ENDIF.
*
*        ENDIF.
*
*      ELSE.
*        WRITE ST_ZLEST0044-TARIFA TO WL_TARIFA.
*        CONDENSE WL_TARIFA NO-GAPS.
*        CONCATENATE 'Tarifa:' WL_TARIFA  ', Fornecedor:'  WL_LFA1-LIFNR ', Origem:' ST_ZLEST0044-CIDADE_ORIGEM
*        ', Destino:' ST_ZLEST0044-CIDADE_DESTINO 'não localizada na transação ZLES0082.'  INTO V_MSG_PROC SEPARATED BY SPACE.
*        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
*                                               V_SERIE_NF_CHAVE
*                                               V_CNPJ_CHAVE
*                                               V_NUM_NF_CHAVE
*                                               V_MSG_PROC
*                                               'E'
*                                               VCHAVE_CTE
*                                               VCOD_FORNECEDOR.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.

  IF V_ERRO NE 'X'.
    MODIFY ZLEST0044 FROM ST_ZLEST0044.
    MODIFY ZLEST0045 FROM TABLE T_ZLEST0045.
  ENDIF.

ENDFORM.                    " TRANSF_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  GET_SUBTAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_NAME  text
*      <--P_V_SUB  text
*----------------------------------------------------------------------*
FORM GET_SUBTAG  USING    P_V_NAME
                 CHANGING P_V_SUB.
  P_V_SUB := ''.
  IF P_V_NAME EQ 'ide' OR P_V_NAME EQ 'toma4' OR P_V_NAME EQ 'enderToma' OR P_V_NAME EQ 'compl' OR P_V_NAME EQ 'emit' OR
     P_V_NAME EQ 'enderEmit' OR P_V_NAME EQ 'rem' OR P_V_NAME EQ 'enderReme' OR P_V_NAME EQ 'infNFe' OR
     P_V_NAME EQ 'exped' OR P_V_NAME EQ 'enderExped' OR P_V_NAME EQ 'receb' OR P_V_NAME EQ 'enderReceb' OR
     P_V_NAME EQ 'dest' OR P_V_NAME EQ 'enderDest' OR P_V_NAME EQ 'vPrest' OR P_V_NAME EQ 'imp' OR P_V_NAME EQ 'infCarga' OR
     P_V_NAME EQ 'ferrov' OR P_V_NAME EQ 'detvag' OR P_V_NAME EQ 'ratNFe' .
    P_V_SUB := 'X'.
  ENDIF.

ENDFORM.                    " GET_SUBTAG

**&---------------------------------------------------------------------*
**&      Form  converte_char_decimal
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_VALOR      text
**      -->P_VALOR_DEC  text
**----------------------------------------------------------------------*
FORM CONVERTE_CHAR_DECIMAL  USING  P_VALOR
                         CHANGING P_VALOR_DEC.

  DATA: LC_VALOR TYPE CHAR30.

  CLEAR P_VALOR_DEC.
  LC_VALOR = P_VALOR.

*  replace all occurrences of regex '[^0-9,]' in lc_valor
*                             with ''.
  REPLACE ALL OCCURRENCES OF '.'
                                 IN LC_VALOR
                             WITH ','.

  CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
    EXPORTING
      I_CHAR         = LC_VALOR
    IMPORTING
*     E_FLOAT        =
      E_PACKED       = P_VALOR_DEC
    EXCEPTIONS
      INVALID_NUMBER = 1
      OTHERS         = 2.

ENDFORM.                    "converte_char_decimal


**&---------------------------------------------------------------------*
**&      Form  CONVERTE_DECIMAL_CHAR
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_VALOR      text
**      -->P_VALOR_DEC  text
**----------------------------------------------------------------------*
*FORM CONVERTE_DECIMAL_CHAR  USING  P_VALOR_DEC
*                         CHANGING P_VALOR.
*
*  DATA: LC_VALOR   TYPE decimals,
*        LC_VALOR_2 TYPE p.
*
*  CLEAR P_VALOR.
*  LC_VALOR = P_VALOR_DEC.
*
*  CALL FUNCTION 'OIU_ME_NUMBER_TO_CHAR'
*   EXPORTING
*     I_FLOAT                = LC_VALOR
*   IMPORTING
*     E_CHAR                 = P_VALOR
*   EXCEPTIONS
*     CONVERSION_ERROR       = 1
*     OTHERS                 = 2.
*
*CALL FUNCTION 'OIU_ME_NUMBER_TO_CHAR'
*   EXPORTING
*     I_PACKED               = LC_VALOR_2
*   IMPORTING
*     E_CHAR                 = P_VALOR
*   EXCEPTIONS
*     CONVERSION_ERROR       = 1
*     OTHERS                 = 2.
*
*
*
*ENDFORM.                    "converte_char_decimal

*&---------------------------------------------------------------------*
*&      Form  z_monta_mensagem
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DACTE    text
*      -->P_SERIE    text
*      -->P_CGC      text
*      -->P_NF       text
*      -->P_MSG      text
*      -->P_TIPO     text
*----------------------------------------------------------------------*
FORM Z_MONTA_MENSAGEM USING P_DACTE TYPE C
                            P_SERIE TYPE C
                            P_CGC   TYPE N
                            P_NF    TYPE C
                            P_MSG   TYPE C
                            P_TIPO  TYPE C
                            P_CHAVE TYPE C
                            P_FORN  TYPE C.

  CLEAR ST_MENSAGENS.
  ST_MENSAGENS-DACTE          = P_DACTE.
  ST_MENSAGENS-SERIE_DESPACHO = P_SERIE.
  ST_MENSAGENS-CGC_REMETENTE  = P_CGC.
  ST_MENSAGENS-NF             = P_NF.
  ST_MENSAGENS-MESSAGEM       = P_MSG.
  ST_MENSAGENS-TP_MSG         = P_TIPO.
  ST_MENSAGENS-CHAVE_CTE      = P_CHAVE.
  ST_MENSAGENS-COD_FORNEC     = P_FORN.


  APPEND ST_MENSAGENS TO T_MENSAGENS.
  IF P_TIPO EQ 'E'.
    V_ERRO = 'X'.
  ENDIF.
ENDFORM.                    " Z_MONTA_ERRO


*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHE_HEADER_BAPI
*&---------------------------------------------------------------------*
*       Preenche Estrutura BAPI-HEADER
*----------------------------------------------------------------------*
FORM ZF_PREENCHE_HEADER_BAPI .

  DATA : NR_NF TYPE C LENGTH 9.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = ST_ZLEST0044-NR_CTE
    IMPORTING
      OUTPUT = NR_NF.


  SHIFT NR_NF LEFT DELETING LEADING '0'.
  CLEAR ST_HEADERDATA.

  ST_HEADERDATA-SHIPMENT_TYPE        = C_Z003.

  SELECT SINGLE LIFNR STCD1
    FROM LFA1
    INTO ST_INF_RECEB
   WHERE STCD1 EQ ST_ZLEST0044-CNPJ_EMITENTE.

  IF ST_ZLEST0041-CENTRO_COMPRADOR IS INITIAL.
    ST_HEADERDATA-SERVICE_AGENT_ID     = ST_INF_RECEB-LIFNR.
    ST_HEADERDATA-TRANS_PLAN_PT        = ST_ZLEST0045-BRANCH.
  ELSE.
    ST_HEADERDATA-SERVICE_AGENT_ID     = ST_LFA1-LIFNR.
    ST_HEADERDATA-TRANS_PLAN_PT        = ST_ZLEST0041-CENTRO_COMPRADOR.
  ENDIF.

  ST_HEADERDATA-SERVICE_LEVEL        = C_1.
  ST_HEADERDATA-SHIPPING_TYPE        = C_02.
  ST_HEADERDATA-EXTERNAL_ID_1        = NR_NF."wa_zlest0006-nr_fatura.
  ST_HEADERDATA-STATUS_PLAN          = C_X.
  ST_HEADERDATA-STATUS_CHECKIN       = C_X.
  ST_HEADERDATA-STATUS_LOAD_START    = C_X.
*  ST_HEADERDATA-STATUS_LOAD_END      = C_X.
*  ST_HEADERDATA-STATUS_COMPL         = C_X.
*  ST_HEADERDATA-STATUS_SHPMNT_START  = C_X.
*  ST_HEADERDATA-STATUS_SHPMNT_END    = C_X.

  "st_headerdata-tendering_carrier_track_id = wa_zlest0006-nr_fatura.
  ST_HEADERDATA-TIME_TRAVEL          = C_30.
  ST_HEADERDATA-TIME_TOTAL           = C_1.
  ST_HEADERDATA-TIME_UNIT            = C_H.
  ST_HEADERDATA-SPECIAL_PROCEDURE_ID = C_0001.
  ST_HEADERDATA-SHPMNT_COST_REL      = C_X.

ENDFORM.                    " ZF_PREENCHE_HEADER_BAPI



*&---------------------------------------------------------------------*
*&      Form  ZF_STAGE_DATA_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_STAGE_DATA_BAPI .

  FIELD-SYMBOLS: <FS_KNOTE> TYPE ANY.
  DATA: TL_1BNFNAD TYPE TABLE OF J_1BNFNAD WITH HEADER LINE,
        TL_VTTK    TYPE TABLE OF VTTK WITH HEADER LINE,
        WL_TVKN    TYPE TVKN.

  REFRESH: T_VTTS     ,
           TL_VTTK     ,
           T_VTTP     ,
           T_VBPA     ,
           TL_1BNFNAD ,
           T_STAGEDATA.

  CLEAR: WL_TVKN.

  ASSIGN ('(ZLESI0009)P_KNOTE') TO <FS_KNOTE>.

  SELECT *
    FROM VTTP
    INTO TABLE T_VTTP
     FOR ALL ENTRIES IN T_ITEMDATA
   WHERE VBELN EQ T_ITEMDATA-DELIVERY.

  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM VTTK
      INTO TABLE TL_VTTK
       FOR ALL ENTRIES IN T_VTTP
       WHERE TKNUM EQ T_VTTP-TKNUM
         AND VSART EQ '01'.


  ENDIF.

  IF NOT SY-SUBRC IS INITIAL.


    SELECT *
      FROM J_1BNFNAD
      INTO TABLE TL_1BNFNAD
       WHERE DOCNUM EQ V_DOCNUM
         AND ( PARVW  EQ 'LR'
          OR   PARVW  EQ 'Z1' ).


    IF SY-SUBRC IS INITIAL.
      SORT: TL_1BNFNAD BY PARVW.
      READ TABLE TL_1BNFNAD
        WITH KEY PARVW = 'LR'
                 BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE *
          FROM TVKN
          INTO WL_TVKN
           WHERE KUNNR EQ TL_1BNFNAD-PARID.

      ENDIF.

      IF <FS_KNOTE> IS ASSIGNED
      AND <FS_KNOTE> IS NOT INITIAL.
        WL_TVKN-KNOTE = <FS_KNOTE>.
      ENDIF.
      IF WL_TVKN IS INITIAL.
        IF <FS_KNOTE> IS ASSIGNED.
          WL_TVKN-KNOTE = <FS_KNOTE>.
        ENDIF.
        IF WL_TVKN-KNOTE IS INITIAL.
          PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                            V_SERIE_NF_CHAVE
                                            V_CNPJ_CHAVE
                                            V_NUM_NF_CHAVE
                                            'Erro ao determinar o local de partida'
                                            'E'
                                            VCHAVE_CTE
                                            VCOD_FORNECEDOR.
          EXIT.
        ENDIF.
      ENDIF.

    ELSE.
      PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                     V_SERIE_NF_CHAVE
                                     V_CNPJ_CHAVE
                                     V_NUM_NF_CHAVE
                                     TEXT-003
                                     'E'
                                     VCHAVE_CTE
                                     VCOD_FORNECEDOR.
      EXIT.
    ENDIF.
  ENDIF.

  IF T_VTTP[] IS INITIAL.
    SY-SUBRC = 4.
  ELSE.
    SELECT *
      FROM VBPA
      INTO TABLE T_VBPA
      FOR ALL ENTRIES IN T_VTTP
    WHERE VBELN EQ T_VTTP-VBELN
      AND LIFNR NE SPACE
      AND PARVW EQ 'Z1'.
  ENDIF.

  IF NOT SY-SUBRC IS INITIAL.
    READ TABLE TL_1BNFNAD
      WITH KEY PARVW = 'Z1'
               BINARY SEARCH.

    IF SY-SUBRC IS NOT INITIAL.
      PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                     V_SERIE_NF_CHAVE
                                     V_CNPJ_CHAVE
                                     V_NUM_NF_CHAVE
                                     TEXT-004
                                     'E'
                                     VCHAVE_CTE
                                     VCOD_FORNECEDOR.
      EXIT.


    ENDIF.
  ENDIF.
  IF <FS_KNOTE> IS ASSIGNED
  AND <FS_KNOTE> IS NOT INITIAL.
    WL_TVKN-KNOTE = <FS_KNOTE>.
  ENDIF.

  IF WL_TVKN IS INITIAL.
    SELECT *
      FROM VTTS
      INTO TABLE T_VTTS
      FOR ALL ENTRIES IN T_VTTP
    WHERE TKNUM EQ T_VTTP-TKNUM.

    IF NOT SY-SUBRC IS INITIAL.
      PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                               V_SERIE_NF_CHAVE
                               V_CNPJ_CHAVE
                               V_NUM_NF_CHAVE
                               TEXT-005
                               'E'
                               VCHAVE_CTE
                               VCOD_FORNECEDOR.
      EXIT.
    ENDIF.
  ENDIF.

*   Local de entrega - Fornecedor
  READ TABLE T_VBPA INDEX 1.
  IF SY-SUBRC IS INITIAL.
    ST_STAGEDATA-DEST_SUPPL = T_VBPA-LIFNR.
  ELSE.
    READ TABLE TL_1BNFNAD
      WITH KEY PARVW = 'Z1'
               BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      ST_STAGEDATA-DEST_SUPPL = TL_1BNFNAD-PARID.
    ELSE.
      CLEAR ST_STAGEDATA-DEST_SUPPL.
    ENDIF.
  ENDIF.

*   Determinação do local de origem
  READ TABLE T_VTTS INDEX 1.
  ST_STAGEDATA-STAGE_CAT  = C_1.
  ST_STAGEDATA-STAGE_SEQ  = C_0001.
  IF WL_TVKN-KNOTE IS NOT INITIAL.
    ST_STAGEDATA-ORG_POINT  = WL_TVKN-KNOTE.
  ELSE.
    ST_STAGEDATA-ORG_POINT  = T_VTTS-KNOTZ.
  ENDIF.
  IF ST_STAGEDATA-ORG_POINT IS INITIAL.
    ST_STAGEDATA-ORG_CUST = T_VTTS-KUNNZ.
  ENDIF.
  IF ST_STAGEDATA-ORG_CUST IS INITIAL.
    ST_STAGEDATA-ORG_SUPPL = T_VTTS-LIFNZ.
  ENDIF.

  APPEND ST_STAGEDATA TO T_STAGEDATA.

ENDFORM.                    " ZF_STAGE_DATA_BAPI


*&---------------------------------------------------------------------*
*&      Form  zf_preenche_itens_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_PREENCHE_ITENS_BAPI .

  REFRESH: T_VBFA.

  SELECT SINGLE REFKEY REFITM
    FROM J_1BNFLIN
    INTO (V_VBELN,V_POSNN)
   WHERE DOCNUM EQ V_DOCNUM.


  SELECT VBELV
         POSNV
         VBELN
         POSNN
         VBTYP_V
         VBTYP_N
    FROM VBFA
    INTO TABLE T_VBFA
   WHERE VBELN EQ V_VBELN
     AND POSNN EQ V_POSNN.

  DELETE T_VBFA WHERE VBTYP_N NE C_M
                   OR VBTYP_V NE C_J.

  IF T_VBFA[] IS INITIAL.
    PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                   V_SERIE_NF_CHAVE
                                   V_CNPJ_CHAVE
                                   V_NUM_NF_CHAVE
                                   TEXT-002
                                   'E'
                                   VCHAVE_CTE
                                   VCOD_FORNECEDOR.
  ENDIF.

  LOOP AT T_VBFA INTO ST_VBFA.

    ST_ITEMDATA-DELIVERY  = ST_VBFA-VBELV.
    ST_ITEMDATA-ITENERARY = ST_VBFA-POSNV.

    APPEND ST_ITEMDATA TO T_ITEMDATA.

  ENDLOOP.

ENDFORM.                    " ZF_PREENCHE_ITENS_BAPI

*&---------------------------------------------------------------------*
*&      Form  ZF_EXECUTA_BAPI
*&---------------------------------------------------------------------*
*       BAPI Criação de Transporte
*----------------------------------------------------------------------*
FORM ZF_GERAR_VT_VI.
  DATA : TL_KONV  TYPE TABLE OF KONV WITH HEADER LINE,
         WL_TOTAL TYPE KONV-KBETR.

*Reverte o saldo da DACT na tabela ZLEST0035
  TYPES: BEGIN OF TYL_0045.
          INCLUDE TYPE ZLEST0045.
  TYPES:  NR_NF    TYPE ZLEST0035-NR_NF,
          SERIE_NF TYPE ZLEST0035-SERIE_NF,
          CNPJ     TYPE ZLEST0035-CNPJ,
          END OF TYL_0045.

  DATA: TL_0044 TYPE TABLE OF ZLEST0044 WITH HEADER LINE,
        TL_0045 TYPE TABLE OF TYL_0045 WITH HEADER LINE,
        TL_0035 TYPE TABLE OF ZLEST0035 WITH HEADER LINE.
*        tl_vtfa type table of vtfa with header line.

  CLEAR: ST_HEADERDATA, V_TKNUM.
  REFRESH: TL_0044, TL_0045, TL_0035,  T_STAGEDATA, T_RETURN.


  PERFORM ZF_PREENCHE_HEADER_BAPI.
  PERFORM ZF_STAGE_DATA_BAPI.

  CLEAR : WL_TOTAL.
  REFRESH: TL_KONV.

  IF NOT T_STAGEDATA[] IS INITIAL.

    CLEAR V_TKNUM.
    "------>Gera o Transporte <------
    CALL FUNCTION 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        HEADERDATA = ST_HEADERDATA
      IMPORTING
        TRANSPORT  = V_TKNUM
      TABLES
        ITEMDATA   = T_ITEMDATA
        STAGEDATA  = T_STAGEDATA
        RETURN     = T_RETURN.

    IF V_TKNUM IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      DELETE T_RETURN WHERE TYPE NE C_E.

      CLEAR ST_ZLEST0008.
      LOOP AT T_RETURN INTO ST_RETURN.

        PERFORM ZF_GET_MAX_LOG_CTRL USING SY-REPID
                                 CHANGING ST_ZLEST0008-CONT
                                          ST_ZLEST0008-IDCTRL.

        ST_ZLEST0008-MSGID   = ST_RETURN-ID.
        ST_ZLEST0008-MSGNR   = ST_RETURN-NUMBER.
        ST_ZLEST0008-MSGV1   = ST_RETURN-MESSAGE.
        ST_ZLEST0008-MSGTYP  = ST_RETURN-TYPE.
        ST_ZLEST0008-TCODE   = SY-CPROG.
        ST_ZLEST0008-MSGSPRA = SY-LANGU.
        ST_ZLEST0008-DATA    = SY-DATUM.
        ST_ZLEST0008-HORA    = SY-UZEIT.
        ST_ZLEST0008-USUARIO = SY-UNAME.

        APPEND ST_ZLEST0008 TO T_ZLEST0008.

        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                       V_SERIE_NF_CHAVE
                                       V_CNPJ_CHAVE
                                       V_NUM_NF_CHAVE
                                       ST_RETURN-MESSAGE
                                       'S'
                                       VCHAVE_CTE
                                       VCOD_FORNECEDOR.

      ENDLOOP.

    ELSE.

      CLEAR V_MENSAGEM.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = C_X.


      CONCATENATE TEXT-006
                  V_TKNUM
             INTO V_MSG SEPARATED BY SPACE.


      PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                     V_SERIE_NF_CHAVE
                                     V_CNPJ_CHAVE
                                     V_NUM_NF_CHAVE
                                     V_MSG
                                     'S'
                                     VCHAVE_CTE
                                     VCOD_FORNECEDOR.

      CLEAR : ST_HEADERDATAACTION, ST_HEADERDATA2.
      REFRESH: T_RETURN.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = V_TKNUM
        IMPORTING
          OUTPUT = ST_HEADERDATA2-SHIPMENT_NUM.

      ST_HEADERDATA2-STATUS_LOAD_END     = 'X'.
      ST_HEADERDATA2-STATUS_COMPL        = 'X'.
      ST_HEADERDATA2-STATUS_SHPMNT_START = 'X'.
      ST_HEADERDATA2-STATUS_SHPMNT_END   = 'X'.


      ST_HEADERDATAACTION-STATUS_LOAD_END     = 'C'.
      ST_HEADERDATAACTION-STATUS_COMPL        = 'C'.
      ST_HEADERDATAACTION-STATUS_SHPMNT_START = 'C'.
      ST_HEADERDATAACTION-STATUS_SHPMNT_END   = 'C'.

      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
        EXPORTING
          HEADERDATA       = ST_HEADERDATA2
          HEADERDATAACTION = ST_HEADERDATAACTION
        TABLES
          RETURN           = T_RETURN.


      DELETE T_RETURN WHERE TYPE NE 'E'.

      IF T_RETURN[] IS NOT INITIAL.
        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                     V_SERIE_NF_CHAVE
                                     V_CNPJ_CHAVE
                                     V_NUM_NF_CHAVE
                                     T_RETURN-MESSAGE
                                     'E'
                                     VCHAVE_CTE
                                     VCOD_FORNECEDOR.


      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      ST_ZLEST0044-STATUS   = C_B.
      ST_ZLEST0044-NR_TRANS = V_TKNUM.

      REFRESH: T_BDC.

*      CONCATENATE  ST_ZLEST0044-DATA_EMISAO+6(2) '.'  ST_ZLEST0044-DATA_EMISAO+4(2) '.'  ST_ZLEST0044-DATA_EMISAO(4) INTO  V_DATA.
      CONCATENATE  WG_DATA_VI+6(2) '.'  WG_DATA_VI+4(2) '.'  WG_DATA_VI(4) INTO  V_DATA.
      "CONCATENATE wa_zlest0006-emissao+6(2) '.' wa_zlest0006-emissao+4(2) '.' wa_zlest0006-emissao(4) INTO  wdata.

      PERFORM ZF_BDC USING: 'X' 'SAPMV54A'   '0010',
                            ' ' 'BDC_CURSOR' 'VTTK-TKNUM',
                            ' ' 'BDC_OKCODE' '=UEBP',
                            ' ' 'VTTK-TKNUM' V_TKNUM,
                            ' ' 'BDC_CURSOR' 'VFKK-PRSDT',
                            ' ' 'BDC_OKCODE' '=UEBP',
                            ' ' 'VFKK-PRSDT' V_DATA.

      PERFORM ZF_BDC USING: 'X' 'SAPMV54A'   '0030',
                            ' ' 'BDC_CURSOR' 'VFKK-FKNUM',
                            ' ' 'BDC_OKCODE' '=SICH',
                            ' ' 'BDC_SUBSCR' 'SAPMV54A'.

      V_MODE = C_N.
      "------>Gera o Custo<------
      CALL TRANSACTION C_VI01
         USING T_BDC
         MODE   V_MODE
         UPDATE C_S
         MESSAGES INTO T_MSG.

      COMMIT WORK AND WAIT.

      SELECT SINGLE *
        INTO  ST_VTFA
        FROM  VTFA
      WHERE VBELV   EQ V_TKNUM
        AND VBTYP_V EQ '8'
        AND VBTYP_N EQ 'a'.

      V_FKNUM = ST_VTFA-VBELN.

      SELECT SINGLE *
        FROM VFKP
        INTO ST_VFKP
       WHERE FKNUM = V_FKNUM.

      IF SY-SUBRC IS INITIAL.
*---> 05/07/2022 - Migração S4 - DG
*   SELECT *
*       FROM KONV
*        INTO TABLE TL_KONV
*      WHERE KNUMV EQ ST_VFKP-KNUMV
*        AND KAPPL EQ 'F'
*        AND KSCHL EQ 'ZFRE'.

   SELECT *
       FROM v_KONV
        INTO TABLE @data(TL_KONV_aux)
      WHERE KNUMV EQ @ST_VFKP-KNUMV
        AND KAPPL EQ 'F'
        AND KSCHL EQ 'ZFRE'.

     move-corresponding TL_KONV_aux[] to TL_KONV[].
*<--- 05/07/2022 - Migração S4 - DG


        CLEAR: WL_TOTAL.
        LOOP AT TL_KONV.
          ADD TL_KONV-KWERT TO WL_TOTAL.
        ENDLOOP.
      ENDIF.

      V_VLR_DIF = ABS( ST_ZLEST0044-VLR_SERV - WL_TOTAL ). "st_vfkp-netwr ).

      SELECT SINGLE VALFROM
        INTO V_MAXDIF
        FROM SETLEAF
       WHERE SETNAME EQ 'ZMAXDIF'.

      MOVE V_MAXDIF TO V_MAXDIV.

      IF ( V_VLR_DIF > V_MAXDIV )  .

        V_VLR_FATURA = ST_ZLEST0044-VLR_SERV.
        V_NETWR      = ST_VFKP-NETWR.

        CONCATENATE  'Valor da fatura diferente do custo! Cte' ':'
                     ST_ZLEST0044-NR_CTE
                     ' ; Valor Fatura: '
                     V_VLR_FATURA
                     '; Valor Custo: '
                     V_NETWR INTO V_MSG_PROC.

        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                             V_SERIE_NF_CHAVE
                             V_CNPJ_CHAVE
                             V_NUM_NF_CHAVE
                             V_MSG_PROC
                             'E'
                             VCHAVE_CTE
                             VCOD_FORNECEDOR.

**** Estorna custos
        REFRESH: T_BDC.
        PERFORM ZF_BDC USING: 'X' 'SAPMV54A'   '0020',
                              ' ' 'BDC_OKCODE' '=UEBP',
                              ' ' 'VFKK-FKNUM' V_FKNUM.

        PERFORM ZF_BDC USING: 'X' 'SAPMV54A'   '0030',
                              ' ' 'BDC_CURSOR' 'VFKP-FKPOS(01)',
                              ' ' 'BDC_OKCODE' '=PDET'.

        PERFORM ZF_BDC USING: 'X' 'SAPMV54A'   '0040',
                              ' ' 'BDC_OKCODE' '=PABR',
                              ' ' 'VFKP-POSTX' ST_VFKP-POSTX.

        PERFORM ZF_BDC USING: 'X' 'SAPMV54A'   '0040',
                              ' ' 'BDC_OKCODE' '=SICH',
                              ' ' 'VFKP-POSTX' ST_VFKP-POSTX,
                              ' ' 'VFKPD-SLSTOR' 'X'.

        V_MODE = C_N.
        "------>Gera o Custo<------
        CALL TRANSACTION C_VI02
           USING T_BDC
           MODE   V_MODE
           UPDATE C_S
           MESSAGES INTO T_MSG.

        REFRESH: T_BDC.
***    Elimina custos
        PERFORM ZF_BDC USING:  'X' 'SAPMV54A'   '0020',
                               ' ' 'BDC_OKCODE' '=UEBP',
                               ' ' 'VFKK-FKNUM' V_FKNUM.

        PERFORM ZF_BDC USING: 'X' 'SAPMV54A'   '0030',
                              ' ' 'BDC_OKCODE' '/ELOES'.

        V_MODE = C_N.
        "------>Gera o Custo<------
        CALL TRANSACTION C_VI02
           USING T_BDC
           MODE   V_MODE
           UPDATE C_S
           MESSAGES INTO T_MSG.
**** Recomponhe o saldo
        CLEAR: ST_ZLEST0045, ST_ZLEST0035.
        LOOP AT T_ZLEST0035 INTO ST_ZLEST0035.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = ST_ZLEST0035-SERIE_NF
            IMPORTING
              OUTPUT = ST_ZLEST0035-SERIE_NF.

          READ TABLE T_ZLEST0045 INTO ST_ZLEST0045
            WITH KEY CHAVE+25(9)  = ST_ZLEST0035-NR_NF
                     CHAVE+22(3)  = ST_ZLEST0035-SERIE_NF
                     CHAVE+6(14)  = ST_ZLEST0035-CNPJ.

          IF SY-SUBRC IS INITIAL.
            ADD ST_ZLEST0045-PESO_RATEADO TO ST_ZLEST0035-SALDO.
            MODIFY T_ZLEST0035 FROM ST_ZLEST0035.

          ENDIF.
          CLEAR: ST_ZLEST0045, ST_ZLEST0035.
        ENDLOOP.


        MOVE: SPACE TO ST_ZLEST0044-NR_TRANS,
              SPACE TO ST_ZLEST0044-NR_FRETE.

**** elimina VT
        PERFORM ELIMINA_VT TABLES T_ITEMDATA
                           USING V_TKNUM.

        CONCATENATE  'O documento de transporte' V_TKNUM ', foi estornado' INTO V_MSG_PROC SEPARATED BY SPACE.

        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                     V_SERIE_NF_CHAVE
                                     V_CNPJ_CHAVE
                                     V_NUM_NF_CHAVE
                                     V_MSG_PROC
                                     'S'
                                     VCHAVE_CTE
                                     VCOD_FORNECEDOR.
*        refresh: t_bdc.
*        perform zf_bdc using:  'X' 'SAPMV56A'   '1011',
*                               ' ' 'BDC_OKCODE' '=MM_TKAL',
*                               ' ' 'VTTK-TKNUM' v_tknum.
*
*        perform zf_bdc using:  'X' 'SAPMV56A'   '1020',
*                               ' ' 'BDC_OKCODE' '=MM_TRLP'.
*
*        perform zf_bdc using:  'X' 'SAPLSPO1'   '0300',
*                               ' ' 'BDC_OKCODE' '=YES'.
*        v_mode = c_n.
*        "------>Elimina VT<------
*        call transaction c_vT02N
*           using t_bdc
*           mode   v_mode
*           update c_s
*           messages into t_msg.



        "CONCATENATE  'Cte' ':' ST_ZLEST0044-NR_CTE ' ;' INTO V_MENSAGEM.
        "CONCATENATE  'Valor Fatura: ' V_VLR_FATURA '; Valor Custo: ' V_NETWR INTO V_MENSAGEM2.

*        MESSAGE I000(Z01) WITH 'Valor da fatura diferente do valor '
*                               'calculado no custo! '
*                               V_MENSAGEM
*                               V_MENSAGEM2.

      ENDIF.

      IF ST_VTFA IS NOT INITIAL
      AND V_ERRO IS INITIAL.

        ST_ZLEST0044-NR_FRETE = V_FKNUM.

        "modify ti_zlest0006 from wa_zlest0006 index v_tabix.
        CONCATENATE TEXT-007
                    V_FKNUM
               INTO V_MSG SEPARATED BY SPACE.

        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                       V_SERIE_NF_CHAVE
                                       V_CNPJ_CHAVE
                                       V_NUM_NF_CHAVE
                                       V_MSG
                                       'S'
                                       VCHAVE_CTE
                                       VCOD_FORNECEDOR.


        "Verificar tabela de Saldo
*        if not t_save[] is initial.
*          modify zlest0035 from table t_save.
*        endif.
        ST_ZLEST0044-STATUS = 'B'.
        MODIFY ZLEST0044 FROM ST_ZLEST0044 .
        MODIFY ZLEST0035 FROM TABLE T_ZLEST0035.

        CLEAR: ST_MSG.
      ELSE.
        "--------->Caso haja algum erro estorna o transporte<---------
        DELETE T_MSG WHERE MSGTYP NE C_E.
        CLEAR ST_ZLEST0008.

        LOOP AT T_MSG INTO ST_MSG.

          MESSAGE ID     ST_MSG-MSGID
                  TYPE   ST_MSG-MSGTYP
                  NUMBER ST_MSG-MSGNR
                  WITH ST_MSG-MSGV1 ST_MSG-MSGV2 ST_MSG-MSGV3 ST_MSG-MSGV4
            INTO V_MENSAGEM.

          PERFORM ZF_GET_MAX_LOG_CTRL USING SY-REPID
                                   CHANGING ST_ZLEST0008-CONT
                                            ST_ZLEST0008-IDCTRL.

          ST_ZLEST0008-TCODE   = SY-CPROG.
          ST_ZLEST0008-MSGTYP  = ST_MSG-MSGTYP.
          ST_ZLEST0008-MSGSPRA = SY-LANGU.
          ST_ZLEST0008-MSGID   = ST_MSG-MSGID.
          ST_ZLEST0008-MSGNR   = ST_MSG-MSGNR.
          ST_ZLEST0008-MSGV1   = V_MENSAGEM.
          ST_ZLEST0008-DATA    = SY-DATUM.
          ST_ZLEST0008-HORA    = SY-UZEIT.
          ST_ZLEST0008-USUARIO = SY-UNAME.

          APPEND ST_ZLEST0008 TO T_ZLEST0008.

**** elimina VT
          PERFORM ELIMINA_VT TABLES T_ITEMDATA
                             USING V_TKNUM.

*          clear : st_headerdataaction, st_headerdata.
*
*          clear : st_headerdata2.
*          call function 'CONVERSION_EXIT_ALPHA_INPUT'
*            exporting
*              input  = v_tknum
*            importing
*              output = st_headerdata2-shipment_num.
*
*          st_headerdataaction-shipment_num = 'D'.
*          st_headerdataaction-service_agent_id = 'D'.
*
*          call function 'BAPI_SHIPMENT_CHANGE'
*            exporting
*              headerdata       = st_headerdata2
*              headerdataaction = st_headerdataaction
*            tables
*              return           = t_return.
*
*          call function 'BAPI_TRANSACTION_COMMIT'
*            exporting
*              wait = 'X'.


          PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                         V_SERIE_NF_CHAVE
                                         V_CNPJ_CHAVE
                                         V_NUM_NF_CHAVE
                                         ST_ZLEST0008-MSGV1
                                         'E'
                                         VCHAVE_CTE
                                         VCOD_FORNECEDOR.

        ENDLOOP.

      ENDIF.

*      IF NOT t_save[] IS INITIAL.
*        MODIFY zlest0035 FROM TABLE t_save.
*      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " ZF_EXECUTA_BAPI

*&---------------------------------------------------------------------*
*&      Form  ZF_ATUALIZA_DADOS
*&---------------------------------------------------------------------*
*       Atualiza dados de Log e Processamento
*----------------------------------------------------------------------*
FORM ZF_GET_MAX_LOG_CTRL  USING P_CHAVE
                       CHANGING S_CONT
                                S_VERSION.

  STATICS: LV_FILENAME TYPE EPSFILNAM VALUE %_MAXCHAR,
           LV_VERSION  TYPE ZIDCTRL,
           LV_VCONT    TYPE NUMC10.
  DATA:    LV_CHAVE     TYPE EPSFILNAM.

  CLEAR: S_CONT,
         S_VERSION.

  CONCATENATE P_CHAVE '_' SY-TCODE '_' SY-DATUM
         INTO LV_CHAVE.

  IF LV_FILENAME <> LV_CHAVE.

    LV_FILENAME = LV_CHAVE.

    SELECT MAX( IDCTRL ) MAX( CONT )
      INTO (LV_VERSION, LV_VCONT)
      FROM ZLEST0008
     WHERE FILENAME = LV_FILENAME
      GROUP BY IDCTRL.
    ENDSELECT.

    IF SY-SUBRC IS INITIAL.
      IF LV_VCONT >= '9999999998'.
        ADD 1 TO LV_VERSION.
        LV_VCONT   = 0.
      ENDIF.
    ELSE.
      LV_VERSION = 1.
      LV_VCONT   = 0.
    ENDIF.

  ELSE.
    IF LV_VCONT >= '9999999998'.
      ADD 1 TO LV_VERSION.
      CLEAR LV_VCONT.
    ENDIF.
  ENDIF.

  ADD 1 TO LV_VCONT.

  S_CONT    = LV_VCONT.
  S_VERSION = LV_VERSION.

ENDFORM.                    " ZF_EXECUTA_BAPI

*&---------------------------------------------------------------------*
*&      Form  zf_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DYNBEGIN Tela Inicial
*      -->P_NAME     Campo
*      -->P_VALUE    Valor
*----------------------------------------------------------------------*
FORM ZF_BDC USING P_DYNBEGIN TYPE ANY
                  P_NAME     TYPE ANY
                  P_VALUE    TYPE ANY.



  IF P_DYNBEGIN EQ C_X.
    ST_BDC-PROGRAM  = P_NAME.
    ST_BDC-DYNPRO   = P_VALUE.
    ST_BDC-DYNBEGIN = P_DYNBEGIN.

    APPEND ST_BDC TO T_BDC.
  ELSE.
    ST_BDC-FNAM = P_NAME.
    ST_BDC-FVAL = P_VALUE.

    APPEND ST_BDC
      TO T_BDC.
  ENDIF.

  CLEAR ST_BDC.
ENDFORM.                    " ZF_BDC
*&---------------------------------------------------------------------*
*&      Form  ZGERA_MIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZSIMULAR_DADOS_MIRO .

  DATA: V_RBLGP      TYPE RBLGP,
        V_TABIX_ITEM TYPE SY-TABIX,
        P_BUKRS      TYPE BUKRS,
        P_LIFNR      TYPE LIFNR,
        P_VALOR      TYPE NETWR_FP,
        P_BVTYP      TYPE BVTYP,
        P_COTACAO    TYPE UKURSM,
        V_RETIDO     TYPE NETWR_FP,
        V_NFTYPE     TYPE J_1BNFTYPE,
        V_XBLNR      TYPE XBLNR1.



  PERFORM ZSELECIONA_DADOS_MIRO.

  SORT T_DADOS BY TDLNR NR_CONHEC SERIES.
  "SORT T_ZLEST0021 BY SHTYP TCODE OPERFRETE. "Tp.transp, transação e oper.lancto.

  CLEAR: T_HEADERDATA_MIRO[],
         T_HEADERDATA_MIRO,
         T_ITEMDATA_MIRO[],
         T_IMPOSTOS[],
         T_IMPOSTOS_RET_B[].

  "Cabeçalhos
  LOOP AT T_DADOS INTO ST_DADOS.

    PERFORM BUSCA_CONTAS USING ST_DADOS-BUDAT.
    SORT T_ZLEST0021 BY SHTYP TCODE OPERFRETE. "Tp.transp, transação e oper.lancto.

    CONCATENATE ST_ZLEST0044-NR_CTE '-' ST_ZLEST0044-SERIE INTO V_XBLNR.

    V_NFTYPE = C_C1.

    CLEAR: ST_HEADERDATA_MIRO.
    ST_HEADERDATA_MIRO-INVOICE_IND    = C_X.
    ST_HEADERDATA_MIRO-DOC_TYPE       = C_FT.
    ST_HEADERDATA_MIRO-DOC_DATE       = ST_DADOS-ZDT_CONHEC.
    ST_HEADERDATA_MIRO-PSTNG_DATE     = ST_DADOS-ZDT_MOV.
    ST_HEADERDATA_MIRO-BLINE_DATE     = ST_DADOS-ZDT_VENCTO.

    CONCATENATE ST_ZLEST0044-NR_CTE '-' ST_ZLEST0044-SERIE INTO ST_HEADERDATA_MIRO-REF_DOC_NO.

    ST_HEADERDATA_MIRO-NR_CONHEC      = ST_DADOS-NR_CONHEC.
    ST_HEADERDATA_MIRO-SERIES         = ST_DADOS-SERIES.
    ST_HEADERDATA_MIRO-COMP_CODE      = ST_DADOS-BUKRS.
    ST_HEADERDATA_MIRO-DIFF_INV       = ST_DADOS-TDLNR.
    ST_HEADERDATA_MIRO-CURRENCY       = ST_DADOS-WAERS.
    ST_HEADERDATA_MIRO-HEADER_TXT     = 'Frete Terceiro'.
    ST_HEADERDATA_MIRO-PMNT_BLOCK     = 'A'.
    ST_HEADERDATA_MIRO-PMNTTRMS       = C_0004.
    ST_HEADERDATA_MIRO-DEL_COSTS_TAXC = ST_DADOS-IVA.
    ST_HEADERDATA_MIRO-GROSS_AMOUNT   = 0.
    ST_HEADERDATA_MIRO-ALLOC_NMBR     = ST_DADOS-EBELN.
    ST_HEADERDATA_MIRO-BUS_AREA       = ST_DADOS-WERKS.
    ST_HEADERDATA_MIRO-CALC_TAX_IND   = C_X.
    "Usado para controle de documento eletrônico ou não
    ST_HEADERDATA_MIRO-GOODS_AFFECTED = ST_DADOS-NFE.
    ST_HEADERDATA_MIRO-PARTNER_BK     = ST_DADOS-BVTYP.

    V_RBLGP = 1.
    ST_IMPOSTOS-REF_DOC_NO       = ST_HEADERDATA_MIRO-REF_DOC_NO.
    ST_IMPOSTOS-TAX_CODE         = ST_DADOS-IVA.
    ST_IMPOSTOS-TAX_AMOUNT       = ST_DADOS-VALOR_ICMS + ST_DADOS-VALOR_PIS + ST_DADOS-VALOR_COFINS.
    ST_IMPOSTOS-TAX_BASE_AMOUNT  = ST_HEADERDATA_MIRO-GROSS_AMOUNT.
    APPEND ST_IMPOSTOS TO T_IMPOSTOS.

    V_RETIDO = 0.

    "Itens do cabeçalho
    LOOP AT T_DADOS INTO ST_DADOS WHERE TDLNR     = ST_DADOS-TDLNR
                                    AND NR_CONHEC = ST_DADOS-NR_CONHEC
                                    AND SERIES    = ST_DADOS-SERIES
                                    AND NFE       = ST_DADOS-NFE.

      CHECK ST_DADOS-RE_BELNR IS INITIAL.
      V_TABIX_ITEM = SY-TABIX.
      "append st_dados to ti_dados_miro.
      CLEAR: ST_ITEMDATA_MIRO.

      "Agrupadores
      CONCATENATE ST_ZLEST0044-NR_CTE '-' ST_ZLEST0044-SERIE INTO ST_ITEMDATA_MIRO-REF_DOC_NO.

      ST_ITEMDATA_MIRO-DIFF_INV         = ST_DADOS-TDLNR.
      ST_ITEMDATA_MIRO-ZVLR_QUEBRA      = ST_DADOS-ZVLR_QUEBRA.
      ST_ITEMDATA_MIRO-ZVLR_PERDA       = ST_DADOS-ZVLR_PERDA.
      ST_ITEMDATA_MIRO-VALOR_PEDAGIO    = ST_DADOS-VALOR_PEDAGIO.
      ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM = V_RBLGP.
      ST_ITEMDATA_MIRO-PO_NUMBER        = ST_DADOS-EBELN.
      ST_ITEMDATA_MIRO-PO_ITEM          = ST_DADOS-EBELP.
      ST_ITEMDATA_MIRO-REF_DOC          = ST_DADOS-LBLNI.
      ST_ITEMDATA_MIRO-REF_DOC_YEAR     = ST_DADOS-LFGJA.
      ST_ITEMDATA_MIRO-TAX_CODE         = ST_DADOS-IVA.
      ST_ITEMDATA_MIRO-DMBTR            = ST_DADOS-DMBTR.
      ST_ITEMDATA_MIRO-ITEM_AMOUNT      = ST_DADOS-DMBTR_DOC -
                                          ( ST_DADOS-VALOR_ICMS + ST_DADOS-VALOR_PIS + ST_DADOS-VALOR_COFINS ) -
                                          ST_DADOS-VALOR_PEDAGIO.
      ST_ITEMDATA_MIRO-SHEET_NO         = ST_DADOS-LBLNI.

      "Somatória de valores de documentos selecionados
      APPEND ST_ITEMDATA_MIRO TO T_ITEMDATA_MIRO.

      "Atualiza item do documento
      ST_DADOS-RE_ITEM         = V_RBLGP.
      MODIFY T_DADOS INDEX V_TABIX_ITEM FROM ST_DADOS TRANSPORTING RE_ITEM.

      ST_HEADERDATA_MIRO-GROSS_AMOUNT   = ST_HEADERDATA_MIRO-GROSS_AMOUNT + ST_DADOS-ZVLR_LIQ_PAGAR.

      "Incrementa 1 no item
      ADD V_1 TO V_RBLGP.

      LOOP AT T_IMPOSTOS_RETIDOS INTO ST_IMPOSTOS_RETIDOS.
        READ TABLE T_IMPOSTOS_RET_B INTO ST_IMPOSTOS_RET_B WITH KEY REF_DOC_NO  = ST_HEADERDATA_MIRO-REF_DOC_NO
                                                                    WI_TAX_TYPE = ST_IMPOSTOS_RETIDOS-WITHT
                                                                    WI_TAX_CODE = ST_IMPOSTOS_RETIDOS-WT_WITHCD.
        IF SY-SUBRC IS INITIAL.
          ST_IMPOSTOS_RET_B-WI_TAX_BASE = ST_IMPOSTOS_RET_B-WI_TAX_BASE + ST_IMPOSTOS_RETIDOS-BASE.
          ST_IMPOSTOS_RET_B-WI_TAX_AMT  = ST_IMPOSTOS_RET_B-WI_TAX_AMT  + ST_IMPOSTOS_RETIDOS-TAXVAL.
          MODIFY T_IMPOSTOS_RET_B INDEX SY-TABIX FROM ST_IMPOSTOS_RET_B TRANSPORTING WI_TAX_BASE WI_TAX_AMT.
        ELSE.
          ST_IMPOSTOS_RET_B-REF_DOC_NO  = ST_HEADERDATA_MIRO-REF_DOC_NO.
          ST_IMPOSTOS_RET_B-WI_TAX_TYPE = ST_IMPOSTOS_RETIDOS-WITHT.
          ST_IMPOSTOS_RET_B-WI_TAX_CODE = ST_IMPOSTOS_RETIDOS-WT_WITHCD.
          ST_IMPOSTOS_RET_B-WI_TAX_BASE = ST_IMPOSTOS_RETIDOS-BASE.
          ST_IMPOSTOS_RET_B-WI_TAX_AMT  = ST_IMPOSTOS_RETIDOS-TAXVAL.
          APPEND ST_IMPOSTOS_RET_B TO T_IMPOSTOS_RET_B.
        ENDIF.
        V_RETIDO = V_RETIDO + ST_IMPOSTOS_RETIDOS-TAXVAL.
      ENDLOOP.

    ENDLOOP.

    IF ST_HEADERDATA_MIRO-GROSS_AMOUNT GT 0.
      IF ST_DADOS-WAERS EQ 'BRL'.
        P_COTACAO = 1.
      ELSE.
        P_COTACAO = ST_DADOS-KURST.
      ENDIF.

      P_VALOR = ST_HEADERDATA_MIRO-GROSS_AMOUNT.
      P_LIFNR = ST_HEADERDATA_MIRO-DIFF_INV.
      P_BUKRS = ST_DADOS-BUKRS.
      P_BVTYP = ST_DADOS-BVTYP.

      CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
        EXPORTING
          P_BUKRS           = P_BUKRS
          P_LIFNR           = P_LIFNR
          P_VALOR           = P_VALOR
          P_COTACAO         = P_COTACAO
          P_BVTYP           = P_BVTYP
        IMPORTING
          P_FORMA_PAGAMENTO = ST_HEADERDATA_MIRO-PYMT_METH
          P_PRINC_BNC_EMP   = ST_HEADERDATA_MIRO-HOUSEBANKID
        EXCEPTIONS
          NAO_FORNECEDOR    = 1
          FORNECEDOR_CONTA  = 2
          FORNECEDOR_BANCO  = 3
          FAIXA_VALOR       = 4
          OTHERS            = 5.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      PERFORM MONTA_CONTA_FRETE USING ST_HEADERDATA_MIRO ST_DADOS.
      ST_HEADERDATA_MIRO-GROSS_AMOUNT = ST_HEADERDATA_MIRO-GROSS_AMOUNT + V_RETIDO.

      APPEND ST_HEADERDATA_MIRO TO T_HEADERDATA_MIRO.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ZGERA_MIRO

*&---------------------------------------------------------------------*
*&      Form  ZSELECIONA_DADOS_MIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZSELECIONA_DADOS_MIRO .

  SELECT V~TDLNR
         L~NAME1
         V~SHTYP
         V~EXTI1
         V~EXTI2
         Z~TKNUM
         Z~FKNUM
         F~BUDAT
         Z~BUKRS
         Z~WERKS
         Z~WAERS
         Z~KBETR
         Z~KURST
         Z~NFENUM
         Z~SERIES
         Z~ZDT_MOV
         Z~ZDT_VENCTO
         Z~NR_CONHEC
         Z~ZDT_CONHEC
         Z~ZPESO_DESTINO
         Z~ZDT_CHEGADA
         Z~KALSM
         Z~IVA
         Z~NFE
         Z~EBELN
         Z~EBELP
         Z~LBLNI
         Z~LFGJA
         Z~ZPESO_ORIGEM
         Z~GEWEI
         Z~DMBTR
         Z~DMBTR_DOC
         Z~ZPESO_DIFERENCA
         Z~ZQUEBRA
         Z~ZPERDA
         Z~ZVLR_QUEBRA
         Z~ZVLR_PERDA
         Z~ZVLR_LIQ_PAGAR
         Z~MATNR
         Z~BVTYP
         Z~MATNS
         Z~RE_BELNR
         Z~RE_GJAHR
         Z~RE_ITEM
         Z~EN_DOCNUM
         Z~REGIO_EMISSOR
         Z~REGIO_RECEPTOR
         Z~BASE_ICMS
         Z~BASE_PIS
         Z~BASE_COFINS
         Z~RATE_ICMS
         Z~RATE_PIS
         Z~RATE_COFINS
         Z~VALOR_ICMS
         Z~VALOR_PIS
         Z~VALOR_COFINS
         Z~VALOR_PEDAGIO
         Z~DOCNUM
         Z~VALOR_MERCADORIA
    INTO TABLE T_DADOS
    FROM ZLEST0034 AS Z
   "inner join zlest0034 as z3 on z3~tknum = z~tknum and z3~fknum = z~fknum
   INNER JOIN ESSR AS F ON F~LBLNI = Z~LBLNI
   INNER JOIN VTTK AS V ON V~TKNUM = Z~TKNUM
   "inner join ekko as e on e~ebeln = z~ebeln
   "inner join ekpo as p on p~ebeln = z~ebeln and p~ebelp = z~ebelp
   INNER JOIN LFA1 AS L ON L~LIFNR = V~TDLNR
   WHERE Z~TKNUM EQ V_TKNUM
     AND Z~FKNUM EQ V_FKNUM
     "and z~add03 eq v_000001
     .

  SELECT *
    INTO TABLE T_IMPOSTOS_RETIDOS
    FROM ZLES0043_IMP_RET
   WHERE TKNUM EQ V_TKNUM.

ENDFORM.                    " ZSELECIONA_DADOS_MIRO

FORM BUSCA_CONTAS USING P_DT_REFERENCIA TYPE BUDAT.

  CLEAR: T_ZLEST0021[], T_ZLEST0021.

  DATA: RGVEICU TYPE RANGE OF ZDE_TP_PROP_VEICULO_CTB.
  RGVEICU = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = SPACE HIGH = SPACE ) ( LOW = '0' HIGH = '0' ) ).

  TRY .
      ZCL_CONTROLE_CONTA_RAZAO=>GET_INSTANCE(
        )->GET_CONTA_RAZAO(
        EXPORTING
          I_SHTYP           = C_Z003    " Tipo de transporte
          I_TCODE           = CONV #( C_MIRO )   " Código de transação
          I_FATURA          = C_T    " Emissor da fatura - Fretes
          I_TP_EMISSOR      = C_T    " Tipo de emissor
          I_OPERFRETE_RANGE = VALUE #( SIGN = 'I' OPTION = 'EQ'
                                     ( LOW = '2'  HIGH = '2'  ) ( LOW = '15' HIGH = '15' )
                                     ( LOW = '14' HIGH = '14' ) ( LOW = '16' HIGH = '16' )
                                     ( LOW = '17' HIGH = '17' ) ( LOW = '18' HIGH = '18' )
                                     ( LOW = '19' HIGH = '19' ) ( LOW = '20' HIGH = '20' ) )    " Ranges Operação de lançamento no razão - Frete
          I_TP_VEICULO      = RGVEICU    " Tipo de Proprietário de Veículo para Contabilização
          I_DT_REFERENCIA   = P_DT_REFERENCIA    " Data de lançamento no documento
        IMPORTING
          E_IT_ZLEST0021    = T_ZLEST0021[]    " Controle de desterminação conta razão
      ).
    CATCH ZCX_CONTROLE_CONTA_RAZAO.    "
  ENDTRY.

*  "Busca parâmetros contábeis
*  SELECT *
*    INTO TABLE T_ZLEST0021
*    FROM ZLEST0021
*   WHERE SHTYP      EQ C_Z003
*     AND TCODE      EQ C_MIRO
*     AND FATURA     EQ C_T
*     AND TP_EMISSOR EQ C_T
*     AND TP_VEICULO IN RGVEICU
*     AND OPERFRETE  IN (C_2,C_15,C_14,C_16,C_17,C_18,C_19,C_20).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTA_CONTA_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MONTA_CONTA_FRETE  USING    WA_CABEC    TYPE TY_BAPI_INCINV_CREATE_HEADER
                                 WA_DADOS    TYPE TY_DADOS.

  DATA: CONTA_RAZAO  TYPE ZLEST0021-RAZAOCRED,
        VL_DEB_CRED  TYPE C,
        VL_CONTABIL  TYPE BAPIWRBTR,
        VL_DIF_NETWR TYPE BAPIWRBTR.

  LOOP AT T_ITEMDATA_MIRO INTO ST_ITEMDATA_MIRO WHERE REF_DOC_NO EQ WA_CABEC-REF_DOC_NO.

* Alimentar a tabela com dados do conta razão - Transitória do Frete
    READ TABLE T_ZLEST0021 INTO ST_ZLEST0021 WITH KEY SHTYP = WA_DADOS-SHTYP TCODE = C_MIRO OPERFRETE = C_2 BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.

    VL_DIF_NETWR = ST_ITEMDATA_MIRO-DMBTR.

    DO 2 TIMES.
      IF SY-INDEX = 1.
        CONTA_RAZAO = ST_ZLEST0021-RAZAOCRED.
        VL_DEB_CRED = C_H.
      ELSE.
        CONTA_RAZAO = ST_ZLEST0021-RAZAODEB.
        VL_DEB_CRED = C_S.
      ENDIF.
      IF VL_DEB_CRED EQ C_S.
        PERFORM Z_ALIMENTAR_GLACCOUNT USING ST_ITEMDATA_MIRO-REF_DOC_NO
                                            ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM
                                            CONTA_RAZAO
                                            ST_ITEMDATA_MIRO-ITEM_AMOUNT
                                            VL_DEB_CRED
                                            WA_CABEC-COMP_CODE
                                            WA_CABEC-BUS_AREA
                                            ST_ZLEST0021-OPERFRETE
                                            TEXT-025
                                            WA_DADOS-EXTI1.
      ENDIF.
      IF VL_DEB_CRED EQ C_H.
        VL_CONTABIL = ST_ITEMDATA_MIRO-DMBTR.
        PERFORM Z_ALIMENTAR_GLACCOUNT USING ST_ITEMDATA_MIRO-REF_DOC_NO
                                            ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM
                                            CONTA_RAZAO
                                            VL_CONTABIL
                                            VL_DEB_CRED
                                            WA_CABEC-COMP_CODE
                                            WA_CABEC-BUS_AREA
                                            ST_ZLEST0021-OPERFRETE
                                            TEXT-034
                                            WA_DADOS-EXTI1.
      ENDIF.
    ENDDO.

    VL_DIF_NETWR = VL_DIF_NETWR - ST_ITEMDATA_MIRO-ITEM_AMOUNT.

    IF VL_DIF_NETWR GT 0.
      READ TABLE T_ZLEST0021 INTO ST_ZLEST0021 WITH KEY SHTYP = WA_DADOS-SHTYP TCODE = C_MIRO OPERFRETE = C_19 BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        VL_DEB_CRED = C_S.
        CONTA_RAZAO = ST_ZLEST0021-RAZAODEB.
        PERFORM Z_ALIMENTAR_GLACCOUNT USING ST_ITEMDATA_MIRO-REF_DOC_NO
                                            ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM
                                            CONTA_RAZAO
                                            VL_DIF_NETWR
                                            VL_DEB_CRED
                                            WA_CABEC-COMP_CODE
                                            WA_CABEC-BUS_AREA
                                            ST_ZLEST0021-OPERFRETE
                                            TEXT-035
                                            WA_DADOS-EXTI1.
      ENDIF.
    ELSEIF VL_DIF_NETWR LT 0.
      READ TABLE T_ZLEST0021 INTO ST_ZLEST0021 WITH KEY SHTYP = WA_DADOS-SHTYP TCODE = C_MIRO OPERFRETE = C_20 BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        VL_DEB_CRED = C_H.
        CONTA_RAZAO = ST_ZLEST0021-RAZAOCRED.
        VL_DIF_NETWR = VL_DIF_NETWR * -1.
        PERFORM Z_ALIMENTAR_GLACCOUNT USING ST_ITEMDATA_MIRO-REF_DOC_NO
                                            ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM
                                            CONTA_RAZAO
                                            VL_DIF_NETWR
                                            VL_DEB_CRED
                                            WA_CABEC-COMP_CODE
                                            WA_CABEC-BUS_AREA
                                            ST_ZLEST0021-OPERFRETE
                                            TEXT-036
                                            WA_DADOS-EXTI1.
      ENDIF.
    ENDIF.

  ENDLOOP.

  "Alimentar a tabela com dados do conta razão - Transitória do Frete - Pedágio
  READ TABLE T_ZLEST0021 INTO ST_ZLEST0021 WITH KEY SHTYP = WA_DADOS-SHTYP TCODE = C_MIRO OPERFRETE = C_14 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    LOOP AT T_ITEMDATA_MIRO INTO ST_ITEMDATA_MIRO WHERE REF_DOC_NO EQ WA_CABEC-REF_DOC_NO.
      IF ST_ITEMDATA_MIRO-VALOR_PEDAGIO GT 0.
        VL_CONTABIL = ST_ITEMDATA_MIRO-VALOR_PEDAGIO.
        CONTA_RAZAO = ST_ZLEST0021-RAZAODEB.
        VL_DEB_CRED = C_S.
        PERFORM Z_ALIMENTAR_GLACCOUNT USING ST_ITEMDATA_MIRO-REF_DOC_NO
                                            ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM
                                            CONTA_RAZAO
                                            VL_CONTABIL
                                            VL_DEB_CRED
                                            WA_CABEC-COMP_CODE
                                            WA_CABEC-BUS_AREA
                                            ST_ZLEST0021-OPERFRETE
                                            TEXT-037
                                            WA_DADOS-EXTI1.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Alimentar a tabela com dados do conta razão - Transitória do Frete - Quebra
  READ TABLE T_ZLEST0021 INTO ST_ZLEST0021 WITH KEY SHTYP = WA_DADOS-SHTYP TCODE = C_MIRO OPERFRETE = C_16 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    LOOP AT T_ITEMDATA_MIRO INTO ST_ITEMDATA_MIRO WHERE REF_DOC_NO EQ WA_CABEC-REF_DOC_NO.
      IF ST_ITEMDATA_MIRO-ZVLR_QUEBRA GT 0.
        VL_CONTABIL = ST_ITEMDATA_MIRO-ZVLR_QUEBRA.
        CONTA_RAZAO = ST_ZLEST0021-RAZAOCRED.
        VL_DEB_CRED = C_H.
        PERFORM Z_ALIMENTAR_GLACCOUNT USING ST_ITEMDATA_MIRO-REF_DOC_NO
                                            ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM
                                            CONTA_RAZAO
                                            VL_CONTABIL
                                            VL_DEB_CRED
                                            WA_CABEC-COMP_CODE
                                            WA_CABEC-BUS_AREA
                                            ST_ZLEST0021-OPERFRETE
                                            TEXT-031
                                            WA_DADOS-EXTI1.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Alimentar a tabela com dados do conta razão - Transitória do Frete - Sobra
  READ TABLE T_ZLEST0021 INTO ST_ZLEST0021 WITH KEY SHTYP = WA_DADOS-SHTYP TCODE = C_MIRO OPERFRETE = C_17 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    LOOP AT T_ITEMDATA_MIRO INTO ST_ITEMDATA_MIRO WHERE REF_DOC_NO EQ WA_CABEC-REF_DOC_NO.
      IF ST_ITEMDATA_MIRO-ZVLR_QUEBRA LT 0.
        VL_CONTABIL = ST_ITEMDATA_MIRO-ZVLR_QUEBRA * -1.
        CONTA_RAZAO = ST_ZLEST0021-RAZAODEB.
        VL_DEB_CRED = C_S.
        PERFORM Z_ALIMENTAR_GLACCOUNT USING ST_ITEMDATA_MIRO-REF_DOC_NO
                                            ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM
                                            CONTA_RAZAO
                                            VL_CONTABIL
                                            VL_DEB_CRED
                                            WA_CABEC-COMP_CODE
                                            WA_CABEC-BUS_AREA
                                            ST_ZLEST0021-OPERFRETE
                                            TEXT-033
                                            WA_DADOS-EXTI1.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Alimentar a tabela com dados do conta razão - Transitória do Frete - Perda
  READ TABLE T_ZLEST0021 INTO ST_ZLEST0021 WITH KEY SHTYP = WA_DADOS-SHTYP TCODE = C_MIRO OPERFRETE = C_18 BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    LOOP AT T_ITEMDATA_MIRO INTO ST_ITEMDATA_MIRO WHERE REF_DOC_NO EQ WA_CABEC-REF_DOC_NO.
      IF ST_ITEMDATA_MIRO-ZVLR_PERDA GT 0.
        VL_CONTABIL = ST_ITEMDATA_MIRO-ZVLR_PERDA.
        CONTA_RAZAO = ST_ZLEST0021-RAZAOCRED.
        VL_DEB_CRED = C_H.
        PERFORM Z_ALIMENTAR_GLACCOUNT USING ST_ITEMDATA_MIRO-REF_DOC_NO
                                            ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM
                                            CONTA_RAZAO
                                            VL_CONTABIL
                                            VL_DEB_CRED
                                            WA_CABEC-COMP_CODE
                                            WA_CABEC-BUS_AREA
                                            ST_ZLEST0021-OPERFRETE
                                            TEXT-032
                                            WA_DADOS-EXTI1.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " MONTA_CONTA_FRETE

*&---------------------------------------------------------------------*
*&      Form  Z_ALIMENTAR_GLACCOUNT
*&---------------------------------------------------------------------*
*       Gera registos
*----------------------------------------------------------------------*
FORM Z_ALIMENTAR_GLACCOUNT  USING    REF_DOC_NO	 TYPE XBLNR
                                     P_ITEM      TYPE RBLGP
                                     P_RAZAO     TYPE SAKNR
                                     P_NETWR     TYPE BAPIWRBTR
                                     P_DEB_CRED  TYPE SHKZG
                                     P_BUKRS     TYPE BUKRS
                                     P_BUS_AREA  TYPE WERKS_D
                                     P_OPERFRETE TYPE ZOPERFRETE
                                     P_TEXTO     TYPE SGTXT
                                     P_EXTI1     TYPE EXTI1.

  IF P_NETWR GT 0.
    CLEAR ST_GLACCOUNTDATA.
    ST_GLACCOUNTDATA-REF_DOC_NO       = REF_DOC_NO.
    ST_GLACCOUNTDATA-INVOICE_DOC_ITEM = P_ITEM.
    ST_GLACCOUNTDATA-GL_ACCOUNT       = P_RAZAO.
    ST_GLACCOUNTDATA-ITEM_AMOUNT      = P_NETWR.
    ST_GLACCOUNTDATA-DB_CR_IND        = P_DEB_CRED.
    ST_GLACCOUNTDATA-COMP_CODE        = P_BUKRS.
    ST_GLACCOUNTDATA-BUS_AREA         = P_BUS_AREA.
    CONCATENATE P_TEXTO P_EXTI1 INTO ST_GLACCOUNTDATA-ITEM_TEXT SEPARATED BY SPACE.
    APPEND ST_GLACCOUNTDATA TO T_GLACCOUNTDATA.
  ENDIF.

ENDFORM.                    " Z_ALIMENTAR_GLACCOUNT

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_MIRO
*&---------------------------------------------------------------------*
*       Gerar Miro
*----------------------------------------------------------------------*
FORM ZGERAR_MIRO .

  DATA: WA_CABECALHO TYPE BAPI_INCINV_CREATE_HEADER,
        WA_ITENS     TYPE BAPI_INCINV_CREATE_ITEM,
        WA_IMP_RET   TYPE BAPI_INCINV_CREATE_WITHTAX,
        IT_ITENS     TYPE TABLE OF BAPI_INCINV_CREATE_ITEM,
        IT_IMP_RET   TYPE TABLE OF BAPI_INCINV_CREATE_WITHTAX,
        IT_CONTAS    TYPE TABLE OF BAPI_INCINV_CREATE_GL_ACCOUNT,
        IT_DADOS     TYPE TABLE OF TY_DADOS,
        IT_TAXDATA   TYPE TABLE OF BAPI_INCINV_CREATE_TAX,
        WA_TAXDATA   TYPE BAPI_INCINV_CREATE_TAX,
        WA_DADOS     TYPE TY_DADOS,
        WA_ZLEST0034 TYPE ZLEST0034,
        NUR_MIRO     TYPE BAPI_INCINV_FLD-INV_DOC_NO,
        ANO_MIRO     TYPE BAPI_INCINV_FLD-FISC_YEAR,
        WA_CONTAS    TYPE BAPI_INCINV_CREATE_GL_ACCOUNT,
        IT_RETURN    TYPE TABLE OF BAPIRET2,
        WA_RETURN    TYPE BAPIRET2,
        V_MENSAGEM   TYPE C LENGTH 50,
        IT_DADOS_AUX TYPE TABLE OF TY_DADOS,
        VG_TABIX     TYPE SY-TABIX.


  "Busca Informações Miro
  PERFORM ZSIMULAR_DADOS_MIRO.

  CLEAR: T_EVENTOS[].

  LOOP AT T_HEADERDATA_MIRO INTO ST_HEADERDATA_MIRO.

    MOVE-CORRESPONDING ST_HEADERDATA_MIRO TO WA_CABECALHO.

    CLEAR: IT_ITENS, IT_CONTAS, IT_RETURN, IT_TAXDATA.
    CLEAR: IT_ITENS[], IT_CONTAS[], IT_RETURN[], IT_TAXDATA[], IT_IMP_RET[].

    IT_DADOS[] = T_DADOS[].

    DELETE IT_DADOS WHERE NR_CONHEC NE ST_HEADERDATA_MIRO-NR_CONHEC
                       OR SERIES NE ST_HEADERDATA_MIRO-SERIES
                       OR TDLNR NE ST_HEADERDATA_MIRO-DIFF_INV
                       OR NFE NE ST_HEADERDATA_MIRO-GOODS_AFFECTED.

    CLEAR: WA_CABECALHO-GOODS_AFFECTED.

    "Seleciona os itens e as contas
    LOOP AT T_ITEMDATA_MIRO INTO ST_ITEMDATA_MIRO WHERE REF_DOC_NO  = WA_CABECALHO-REF_DOC_NO AND DIFF_INV EQ WA_CABECALHO-DIFF_INV.
      MOVE-CORRESPONDING ST_ITEMDATA_MIRO TO WA_ITENS.
      APPEND WA_ITENS TO IT_ITENS.
      LOOP AT T_GLACCOUNTDATA INTO ST_GLACCOUNTDATA WHERE REF_DOC_NO  = WA_CABECALHO-REF_DOC_NO
                                                       AND INVOICE_DOC_ITEM EQ ST_ITEMDATA_MIRO-INVOICE_DOC_ITEM.
        MOVE-CORRESPONDING ST_GLACCOUNTDATA TO WA_CONTAS.
        APPEND WA_CONTAS TO IT_CONTAS.
      ENDLOOP.
    ENDLOOP.

    LOOP AT T_IMPOSTOS_RET_B INTO ST_IMPOSTOS_RET_B.
      MOVE-CORRESPONDING ST_IMPOSTOS_RET_B TO WA_IMP_RET.
      WA_IMP_RET-SPLIT_KEY           = C_000001.
      WA_IMP_RET-WI_TAX_WITHHELD_AMT = WA_IMP_RET-WI_TAX_AMT.
      APPEND WA_IMP_RET TO IT_IMP_RET.
    ENDLOOP.

*    LOOP AT it_impostos INTO wa_impostos WHERE ref_doc_no  = wa_cabecalho-ref_doc_no.
*      MOVE-CORRESPONDING wa_impostos TO wa_taxdata.
*      APPEND wa_taxdata TO it_taxdata.
*    ENDLOOP.

    "Criar o documento de revisão de fatura através da bapi da MIRO
    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        HEADERDATA       = WA_CABECALHO
      IMPORTING
        INVOICEDOCNUMBER = NUR_MIRO
        FISCALYEAR       = ANO_MIRO
      TABLES
        ITEMDATA         = IT_ITENS
        GLACCOUNTDATA    = IT_CONTAS
        WITHTAXDATA      = IT_IMP_RET
        RETURN           = IT_RETURN.        "taxdata = it_taxdata

    IF IT_RETURN[] IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CLEAR ST_EVENTOS.
      ST_EVENTOS-ICONE    = ICON_LED_GREEN.
      CONCATENATE '(Miro) Documento' NUR_MIRO 'gerado para o ano de' ANO_MIRO '!' INTO ST_EVENTOS-MSG_TEXT SEPARATED BY SPACE.
      APPEND ST_EVENTOS TO T_EVENTOS.

      "Atualizar documento da miro.
      LOOP AT IT_DADOS INTO WA_DADOS.
        LOOP AT T_DADOS INTO ST_DADOS WHERE RE_BELNR IS INITIAL
                                        AND RE_GJAHR IS INITIAL
                                        AND NR_CONHEC EQ WA_DADOS-NR_CONHEC
                                        AND SERIES    EQ WA_DADOS-SERIES
                                        AND TDLNR     EQ WA_DADOS-TDLNR.
          VG_TABIX = SY-TABIX.
          ST_DADOS-RE_BELNR = NUR_MIRO.
          ST_DADOS-RE_GJAHR = ANO_MIRO.
          "Atualizar ti_dados e salvar
          SELECT SINGLE * INTO WA_ZLEST0034
            FROM ZLEST0034
           WHERE TKNUM EQ ST_DADOS-TKNUM.
          WA_ZLEST0034-RE_BELNR = ST_DADOS-RE_BELNR.
          WA_ZLEST0034-RE_GJAHR = ST_DADOS-RE_GJAHR.
          MODIFY ZLEST0034 FROM WA_ZLEST0034.

          CALL FUNCTION 'Z_LES_REVISAO_FATURA_TERC'
            EXPORTING
              P_ZLEST0034 = WA_ZLEST0034.

          COMMIT WORK.

          MODIFY T_DADOS INDEX VG_TABIX FROM ST_DADOS TRANSPORTING RE_BELNR RE_GJAHR.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      LOOP AT IT_RETURN[] INTO WA_RETURN.
        CONCATENATE 'Miro: ' WA_RETURN-MESSAGE INTO V_MENSAGEM.
        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                       V_SERIE_NF_CHAVE
                                       V_CNPJ_CHAVE
                                       V_NUM_NF_CHAVE
                                       V_MENSAGEM
                                       'E'
                                       VCHAVE_CTE
                                       VCOD_FORNECEDOR.

      ENDLOOP.


    ENDIF.

    "perform popula_log tables it_return.

    CLEAR: WA_CABECALHO.

  ENDLOOP.

  " perform z_gerar_fiscal.

ENDFORM.                    " Z_GERAR_MIRO


*&---------------------------------------------------------------------*
*&      Form  zgerar_miro_shdb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZGERAR_MIRO_SHDB.


  PERFORM ZF_BDC USING: 'X' 'ZLESR0014'    '1000',

                        ' ' 'BDC_CURSOR'   'S_FKNUM-LOW',
                        ' ' 'BDC_OKCODE'   '=ONLI',
                        ' ' 'RA1'          'X',
                        ' ' 'RA5'          'X',
                        ' ' 'S_TKNUM-LOW'  V_TKNUM,
                        ' ' 'S_FKNUM-LOW'  V_FKNUM,

                        ' ' 'BDC_CURSOR'   'S_ERDAT-LOW',
                        ' ' 'BDC_OKCODE'   '=ONLI',

                        ' ' 'RA1'          'X',
                        ' ' 'RA5'          'X',
                        ' ' 'S_TKNUM-LOW'  '2349',
                        ' ' 'S_FKNUM-LOW'  '2061',
                        ' ' 'S_ERDAT-LOW'  V_DATA.


  V_MODE = C_N.
  "------>Pesquisa miro<------
  CALL TRANSACTION 'ZLES0043'
     USING T_BDC
     MODE   V_MODE
     UPDATE C_S
     MESSAGES INTO T_MSG.

  COMMIT WORK AND WAIT.

ENDFORM.                    "zgerar_miro_shdb
*&---------------------------------------------------------------------*
*&      Form  ZGRAVA_MENSAGENS_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZGRAVA_MENSAGENS_TAB .
  DATA: V_IDCTRL TYPE ZLEST0008-IDCTRL.

  DELETE ADJACENT DUPLICATES FROM T_MENSAGENS COMPARING ALL FIELDS.

  DELETE FROM ZLEST0008 WHERE FILENAME = VCHAVE_CTE.

  V_IDCTRL = 1.

  LOOP AT T_MENSAGENS INTO ST_MENSAGENS.
    ST_ZLEST0008-FILENAME = ST_MENSAGENS-CHAVE_CTE.
    ST_ZLEST0008-IDCTRL   = V_IDCTRL.
    ST_ZLEST0008-TCODE    = SY-CPROG.
    ST_ZLEST0008-MSGTYP   = ST_MENSAGENS-TP_MSG.
    ST_ZLEST0008-MSGSPRA  = SY-LANGU.
    ST_ZLEST0008-MSGV1    = ST_MENSAGENS-MESSAGEM.
    ST_ZLEST0008-DATA     = SY-DATUM.
    ST_ZLEST0008-HORA     = SY-UZEIT.
    ST_ZLEST0008-USUARIO  = SY-UNAME.
    ST_ZLEST0008-LOTE     = ST_MENSAGENS-DACTE.
    ST_ZLEST0008-CONT     = VCOD_FORNECEDOR.

    MODIFY ZLEST0008 FROM ST_ZLEST0008.

    V_IDCTRL = V_IDCTRL + 1.

  ENDLOOP.

ENDFORM.                    " ZGRAVA_MENSAGENS_TAB
*&---------------------------------------------------------------------*
*&      Form  ZVERIFICA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DOCNUM  text
*----------------------------------------------------------------------*
FORM ZVERIFICA_SALDO USING P_DOCNUM TYPE J_1BDOCNUM
                           P_CNPJ   TYPE J_1BCGC
                  CHANGING P_SALDO.
  CLEAR: ST_ZLEST0035.
  SELECT SINGLE *
    INTO ST_ZLEST0035
    FROM ZLEST0035
   WHERE DOCNUM EQ P_DOCNUM.

  IF SY-SUBRC IS NOT INITIAL .
*    clear st_zlest0035.
*
*    select single *
*      into st_j_1bnfdoc
*      from j_1bnfdoc
*     where docnum eq p_docnum.
*
*    st_zlest0035-nr_nf      = st_j_1bnfdoc-nfenum.
*    st_zlest0035-serie_nf   = st_j_1bnfdoc-series.
*    st_zlest0035-cnpj       = p_cnpj.
*    st_zlest0035-docnum     = st_j_1bnfdoc-docnum.
*    st_zlest0035-werks      = st_j_1bnfdoc-branch.
*    st_zlest0035-qtd_nf     = st_j_1bnfdoc-ntgew.
*    st_zlest0035-qtd_cheg   = st_j_1bnfdoc-ntgew.
*    st_zlest0035-dtachegada = st_j_1bnfdoc-pstdat.
*    st_zlest0035-saldo      = st_j_1bnfdoc-ntgew.
*
*    modify zlest0035 from st_zlest0035.

  ENDIF.

  P_SALDO = ST_ZLEST0035-SALDO.

ENDFORM.                    " ZVERIFICA_SALDO
*&---------------------------------------------------------------------*
*&      Form  VALIDA_CTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VALIDA_CTE .
  DATA : SL_ZLEST0044   TYPE ZLEST0044.

  SELECT SINGLE *
    INTO SL_ZLEST0044
    FROM ZLEST0044
   WHERE CHAVE_CTE = ST_ZLEST0044-CHAVE_CTE.

  IF SY-SUBRC IS INITIAL.
    IF SL_ZLEST0044-CANCELADO EQ 'X' .
      PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                       V_SERIE_NF_CHAVE
                                       V_CNPJ_CHAVE
                                       V_NUM_NF_CHAVE
                                       TEXT-013
                                       'E'
                                       VCHAVE_CTE
                                       VCOD_FORNECEDOR.

    ENDIF.

    " Custo
    IF SL_ZLEST0044-NR_FRETE IS NOT INITIAL .

      SELECT SINGLE *
        INTO  ST_VTFA
        FROM  VTFA
       WHERE VBELV   EQ SL_ZLEST0044-NR_FRETE
         AND VBTYP_V EQ '8'
         AND VBTYP_N EQ 'a'.

      IF SY-SUBRC IS INITIAL.
        CONCATENATE TEXT-012 SL_ZLEST0044-NR_FRETE INTO V_MSG_PROC.

        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                       V_SERIE_NF_CHAVE
                                       V_CNPJ_CHAVE
                                       V_NUM_NF_CHAVE
                                       V_MSG_PROC
                                       'E'
                                       VCHAVE_CTE
                                       VCOD_FORNECEDOR.

      ENDIF.

    ENDIF.

    "Transporte
    IF SL_ZLEST0044-NR_TRANS IS NOT INITIAL .
      SELECT SINGLE *
        INTO ST_VTTK
        FROM VTTK
       WHERE TKNUM EQ SL_ZLEST0044-NR_TRANS.

      IF  SY-SUBRC IS INITIAL.
        CONCATENATE TEXT-011 SL_ZLEST0044-NR_TRANS INTO V_MSG_PROC.

        PERFORM Z_MONTA_MENSAGEM USING ST_ZLEST0044-NR_CTE
                                       V_SERIE_NF_CHAVE
                                       V_CNPJ_CHAVE
                                       V_NUM_NF_CHAVE
                                       V_MSG_PROC
                                       'E'
                                       VCHAVE_CTE
                                       VCOD_FORNECEDOR.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " VALIDA_CTE
*&---------------------------------------------------------------------*
*&      Form  ELIMINA_VT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ITEMDATA  text
*      -->P_V_TKNUM  text
*----------------------------------------------------------------------*
FORM ELIMINA_VT  TABLES   TL_ITEMDATA
                 USING    P_TKNUM.
  CLEAR : ST_HEADERDATAACTION, ST_HEADERDATA, T_ITEMDATAACTION, ST_HEADERDATA2.
  REFRESH: T_ITEMDATAACTION.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_TKNUM
    IMPORTING
      OUTPUT = ST_HEADERDATA2-SHIPMENT_NUM.

  ST_HEADERDATAACTION-SHIPMENT_NUM = 'D'.
  ST_HEADERDATAACTION-SERVICE_AGENT_ID = 'D'.

  LOOP AT TL_ITEMDATA INTO ST_ITEMDATA.
    MOVE: 'D' TO T_ITEMDATAACTION-DELIVERY,
          'D' TO T_ITEMDATAACTION-ITENERARY.

    APPEND T_ITEMDATAACTION.
    CLEAR: T_ITEMDATAACTION.

  ENDLOOP.
*
  CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
    EXPORTING
      HEADERDATA       = ST_HEADERDATA2
      HEADERDATAACTION = ST_HEADERDATAACTION
    TABLES
      ITEMDATA         = TL_ITEMDATA
      ITEMDATAACTION   = T_ITEMDATAACTION
      RETURN           = T_RETURN.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.

ENDFORM.                    " ELIMINA_VT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VALUE_XML TABLES  P_NODE_PATH STRUCTURE TG_NODE_PATH
                 USING  P_NAMED_ITEM
                        P_VALUE_ITEM
               CHANGING E_VALUE.
  FIELD-SYMBOLS: <FS_CAMPO1> TYPE ANY,
                 <FS_CAMPO2> TYPE ANY.

  DATA: WL_CAMPO(50),
        WL_CAMPO2(50),
        WL_CAMPO_AUX  TYPE STRING,
        WL_PATH       TYPE RLGRAP-FILENAME.

  DATA: XML          TYPE REF TO CL_XML_DOCUMENT,
        NODE         TYPE REF TO IF_IXML_NODE,
        V_VALOR      TYPE STRING,
        V_VALOR2     TYPE STRING,
        V_VALOR3     TYPE REF TO IF_IXML_NAMED_NODE_MAP,
        V_VALOR4     TYPE REF TO IF_IXML_NODE,
        TABELA       TYPE SWXMLNODES,
        TABELA2      TYPE SWXMLNODES,
        WA           TYPE SWXML_NITM,
        WL_NODE_PATH TYPE STRING.

  UNASSIGN: <FS_CAMPO1>, <FS_CAMPO2>.

  CLEAR: WL_NODE_PATH.
  ASSIGN ('(ZLESI0009)P_INPUT') TO <FS_CAMPO1>.
  IF <FS_CAMPO1> IS ASSIGNED.
    WL_CAMPO = <FS_CAMPO1>.
  ENDIF.

  ASSIGN ('(ZLESI0009)ST_FILES_DOC-PATHNAME') TO <FS_CAMPO2>.
  IF <FS_CAMPO2> IS ASSIGNED.
    WL_CAMPO2 = <FS_CAMPO2>.
  ENDIF.

  CONCATENATE WL_CAMPO WL_CAMPO2 INTO WL_CAMPO_AUX.
  CREATE OBJECT XML.

  WL_PATH = WL_CAMPO_AUX.
  CALL METHOD XML->IMPORT_FROM_FILE
    EXPORTING
      FILENAME = WL_PATH. "'c:\Consulta_Distancia.txt'.

*  CALL METHOD xml->find_node
*    EXPORTING
*      name = 'infNFe versao'
*    RECEIVING
*      node = noder.

  CALL METHOD XML->FIND_NODE_TABLE
    IMPORTING
      T_NODES = TABELA.
  LOOP AT TABELA INTO WA.

    CALL METHOD XML->GET_NODE_PATH
      EXPORTING
        NODE = WA-NODE
      RECEIVING
        PATH = V_VALOR2.
    READ TABLE P_NODE_PATH INDEX 1.
*    condense p_node_path no-gaps.
    IF V_VALOR2 EQ P_NODE_PATH-PATH. "'/cteProc/CTe/infCte'.
      CLEAR: V_VALOR2.
      CALL METHOD XML->FIND_NODE_TABLE
        EXPORTING
          ROOT    = WA-NODE
        IMPORTING
          T_NODES = TABELA2.
      LOOP AT TABELA2 INTO WA.
        CLEAR :V_VALOR2.
        CALL METHOD XML->GET_NODE_PATH
          EXPORTING
            NODE = WA-NODE
          RECEIVING
            PATH = V_VALOR2.
        READ TABLE P_NODE_PATH INDEX 2.
*        condense p_node_path no-gaps.
        IF V_VALOR2 EQ P_NODE_PATH-PATH. "'/cteProc/CTe/infCte/compl/ObsCont'.
          CLEAR: V_VALOR3.
          CALL METHOD WA-NODE->GET_ATTRIBUTES
            RECEIVING
              RVAL = V_VALOR3.

          CALL METHOD V_VALOR3->GET_NAMED_ITEM
            EXPORTING
              NAME = P_NAMED_ITEM "'xCampo'
            RECEIVING
              RVAL = V_VALOR4.

          CALL METHOD V_VALOR4->GET_VALUE
            RECEIVING
              RVAL = V_VALOR2.

          IF V_VALOR2 EQ P_VALUE_ITEM. "'DATA:'.
            CALL METHOD WA-NODE->GET_VALUE
              RECEIVING
                RVAL = E_VALUE.
*            clear: v_FOUND.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF E_VALUE IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " BUSCA_DATA
