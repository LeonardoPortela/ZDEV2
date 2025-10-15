*&---------------------------------------------------------------------*
*& Report  ZFIR0037
*&
*&---------------------------------------------------------------------*
*&TITULO: MTM – Mercado Interno – Atualização de Preço
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 30.10.2013
*&TRANSAÇÃO: ZFI0035
*&---------------------------------------------------------------------*
REPORT  ZFIR0037.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZSDT0051, ZFIT0052.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_ZSDT0051.
        INCLUDE STRUCTURE ZSDT0051.
TYPES:  ME_ANO TYPE ZFIT0049-ME_ANO,
END OF TY_ZSDT0051.

TYPES: BEGIN OF TY_ZSDT0059.
        INCLUDE STRUCTURE ZSDT0059.
TYPES:  ME_ANO          TYPE ZFIT0049-ME_ANO,
        MATNR           TYPE ZSDT0053-MATNR,
        CLIENTE_ORIGEM  TYPE ZSDT0053-KUNNR,
        CLIENTE_DESTINO TYPE ZSDT0053-KUNNR,
END OF TY_ZSDT0059.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: IT_ZFIT0049         TYPE TABLE OF ZFIT0049,
      IT_ZFIT0050         TYPE TABLE OF ZFIT0050,
      IT_ZFIT0051         TYPE TABLE OF ZFIT0051,
      IT_ZFIT0052         TYPE TABLE OF ZFIT0052,

      IT_ZSDT0051         TYPE TABLE OF TY_ZSDT0051,
      IT_ZSDT0053         TYPE TABLE OF ZSDT0053,
      IT_ZSDT0059         TYPE TABLE OF TY_ZSDT0059.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_ZFIT0049         TYPE ZFIT0049,
      WA_ZFIT0050         TYPE ZFIT0050,
      WA_ZFIT0051         TYPE ZFIT0051,
      WA_ZFIT0052         TYPE ZFIT0052,

      WA_ZSDT0051         TYPE TY_ZSDT0051,
      WA_ZSDT0053         TYPE ZSDT0053,
      WA_ZSDT0059         TYPE TY_ZSDT0059.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  P_BUKRS  FOR ZSDT0051-VKORG OBLIGATORY.
PARAMETER:       P_DATA   TYPE ZFIT0052-DT_FECHAMENTO OBLIGATORY.
PARAMETER:       P_TP     TYPE ZSDT0051-TP_VENDA OBLIGATORY DEFAULT 27.
SELECTION-SCREEN: END OF BLOCK B1.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            F_SELECIONA_DADOS, " Form seleciona dados
            F_PROCESSA_DADOS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.

  DATA TABIX TYPE SY-TABIX.

  SELECT *
    FROM ZSDT0051
    INTO TABLE IT_ZSDT0051
    WHERE TP_VENDA  = P_TP
    AND   VKORG   IN P_BUKRS.

  CHECK SY-SUBRC = 0.

  SELECT *
    FROM ZSDT0053
    INTO TABLE IT_ZSDT0053
    FOR ALL ENTRIES IN IT_ZSDT0051
    WHERE NRO_SOL_OV = IT_ZSDT0051-NRO_SOL_OV
    AND   STATUS     = ''.

  CHECK SY-SUBRC = 0.

  SELECT *
    FROM ZSDT0059
    INTO TABLE IT_ZSDT0059
    FOR ALL ENTRIES IN IT_ZSDT0053
    WHERE NRO_SOL_OV = IT_ZSDT0053-NRO_SOL_OV
    AND   POSNR      = IT_ZSDT0053-POSNR.

  CHECK SY-SUBRC = 0.

  SORT: IT_ZSDT0053 BY NRO_SOL_OV POSNR.

  LOOP AT IT_ZSDT0059 INTO WA_ZSDT0059.
    TABIX = SY-TABIX.
    READ TABLE IT_ZSDT0053 INTO WA_ZSDT0053 WITH KEY NRO_SOL_OV = WA_ZSDT0059-NRO_SOL_OV
                                                     POSNR      = WA_ZSDT0059-POSNR BINARY SEARCH.
    CONCATENATE WA_ZSDT0059-MONAT WA_ZSDT0059-VALDT+4 INTO WA_ZSDT0059-ME_ANO.
    WA_ZSDT0059-MATNR           = WA_ZSDT0053-MATNR.
    WA_ZSDT0059-CLIENTE_ORIGEM  = WA_ZSDT0053-WERKS.
    WA_ZSDT0059-CLIENTE_DESTINO = WA_ZSDT0053-KUNNR.
    MODIFY IT_ZSDT0059 FROM WA_ZSDT0059 INDEX TABIX TRANSPORTING ME_ANO MATNR CLIENTE_ORIGEM CLIENTE_DESTINO.
  ENDLOOP.

  SELECT *
    FROM ZFIT0051
    INTO TABLE IT_ZFIT0051
    FOR ALL ENTRIES IN IT_ZSDT0059
    WHERE COD_FP  = IT_ZSDT0059-COD_FP.

  CHECK SY-SUBRC = 0.

  SELECT *
    FROM ZFIT0049
    INTO TABLE IT_ZFIT0049
    FOR ALL ENTRIES IN IT_ZSDT0059
    WHERE ME_ANO        = IT_ZSDT0059-ME_ANO
    AND   MATNR         = IT_ZSDT0059-MATNR.

  SELECT *
    FROM ZFIT0050
    INTO TABLE IT_ZFIT0050
    FOR ALL ENTRIES IN IT_ZSDT0059
    WHERE ME_ANO          = IT_ZSDT0059-ME_ANO
    AND   MATNR           = IT_ZSDT0059-MATNR
    AND   CLIENTE_ORIGEM  = IT_ZSDT0059-CLIENTE_ORIGEM
    AND   CLIENTE_DESTINO =	IT_ZSDT0059-CLIENTE_DESTINO.



ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PROCESSA_DADOS .
  SORT: IT_ZFIT0051     BY COD_FP,
        IT_ZSDT0051     BY NRO_SOL_OV,
        IT_ZSDT0053     BY NRO_SOL_OV POSNR,
        IT_ZFIT0049     BY ME_ANO MATNR DT_REFERENCIA ,
        IT_ZFIT0050     BY ME_ANO MATNR CLIENTE_ORIGEM CLIENTE_DESTINO DT_REFERENCIA.

  DATA: W_LINHA(3),
        W_LIN TYPE I.

  LOOP AT IT_ZSDT0059 INTO WA_ZSDT0059.
    ADD 1 TO W_LIN.
    MOVE-CORRESPONDING WA_ZSDT0059 TO WA_ZFIT0052.
    WA_ZFIT0052-DT_FECHAMENTO = P_DATA.
    MOVE: SY-MANDT TO WA_ZFIT0052-MANDT,
          SY-UNAME TO WA_ZFIT0052-USNAM,
          SY-DATUM TO WA_ZFIT0052-DATA_ATUAL,
          SY-UZEIT TO WA_ZFIT0052-HORA_ATUAL.

    CLEAR WA_ZFIT0049.
    LOOP AT IT_ZFIT0049 INTO WA_ZFIT0049 WHERE ME_ANO = WA_ZSDT0059-ME_ANO
                                         AND   MATNR  =  WA_ZSDT0059-MATNR.
    ENDLOOP.
    CLEAR WA_ZFIT0050.
    LOOP AT IT_ZFIT0050 INTO WA_ZFIT0050 WHERE ME_ANO          = WA_ZSDT0059-ME_ANO
                                         AND   MATNR           = WA_ZSDT0059-MATNR
                                         AND   CLIENTE_ORIGEM  = WA_ZSDT0059-CLIENTE_ORIGEM
                                         AND   CLIENTE_DESTINO = WA_ZSDT0059-CLIENTE_DESTINO.
    ENDLOOP.

    READ TABLE IT_ZFIT0051 INTO WA_ZFIT0051 WITH KEY COD_FP = WA_ZSDT0059-COD_FP BINARY SEARCH.
    IF WA_ZFIT0051-STATUS = 'P'.
      WA_ZFIT0052-FORMULA  = WA_ZFIT0049-VALOR_COTACAO.
      WA_ZFIT0052-FORMULA2 = WA_ZFIT0049-VALOR_COTACAO.
    ELSE.
      WA_ZFIT0052-FORMULA  = WA_ZFIT0050-VALOR_FRETE.
      WA_ZFIT0052-FORMULA2 = WA_ZFIT0050-VALOR_FRETE .
    ENDIF.
    APPEND WA_ZFIT0052 TO IT_ZFIT0052.
  ENDLOOP.

  IF IT_ZFIT0052[] IS NOT INITIAL.
    MODIFY ZFIT0052 FROM TABLE IT_ZFIT0052.
  ENDIF.

  MOVE W_LIN TO W_LINHA.
  MESSAGE S836(SD) WITH 'Processados'
                         W_LINHA
                         'registros com sucesso!'.
ENDFORM.                    " F_PROCESSA_DADOS
