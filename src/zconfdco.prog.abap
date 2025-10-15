*&------------P R O J E T O  E V O L U I R   -   M A G G I-------------*
* Programa   : ZCONFDCO                                                *
* Descrição  : Subcontratados de Frete                                 *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Paulo Bonetti                          Data: 02/11/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*&---------------------------------------------------------------------*
REPORT  ZCONFDCO.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS.

TABLES : ZDCO_PRODUTOR, ZDCO_NOTA, VBAK, J_1BNFDOC, ZSDT0001, LFA1,
         ZLEST0044.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF TY_DCO,
    NU_DCO          LIKE ZDCO_PRODUTOR-NU_DCO,
    NR_DCO          LIKE ZDCO_PRODUTOR-NR_DCO,
    DT_LANCAMENTO   LIKE ZDCO_PRODUTOR-DT_LANCAMENTO,
    QT_MATERIAL     LIKE ZDCO_PRODUTOR-QT_MATERIAL,
    ID_FORNECEDOR   LIKE ZDCO_PRODUTOR-ID_FORNECEDOR,
    CD_MATERIAL     LIKE ZDCO_PRODUTOR-CD_MATERIAL,
    CD_CENTRO       LIKE ZDCO_PRODUTOR-CD_CENTRO,
    CD_SAFRA        LIKE ZDCO_PRODUTOR-CD_SAFRA,
    CD_TIPO_LEILAO  LIKE ZDCO_PRODUTOR-CD_TIPO_LEILAO,
    DOC_VENDA       LIKE ZDCO_PRODUTOR-VBELN,
    QT_ENTREGUE     LIKE ZDCO_PRODUTOR-QT_ENTREGUE,
    QT_REMESSA      LIKE ZDCO_PRODUTOR-QT_REMESSA,
    NU_AVISO        LIKE ZDCO_PRODUTOR-NU_AVISO,
    NOME_FORNECEDOR TYPE NAME1,
    NOME_DEPOSITO   TYPE LGOBE,
    NOME_CENTRO     TYPE NAME1,
    NOME_MATERIAL   TYPE MAKTX,
    DS_TIPO_LEILAO  TYPE ZDS_TIPO_LEILAO,
  END   OF TY_DCO,

  BEGIN OF TY_VBAK,
    VBELN LIKE VBAK-VBELN,
    KUNNR LIKE VBAK-KUNNR,
  END   OF TY_VBAK,

  BEGIN OF TY_NF,
    VGBEL  TYPE VBRP-VGBEL,
    DOCNUM TYPE J_1BNFDOC-DOCNUM,
    NFENUM TYPE J_1BNFDOC-NFENUM,
    CANCEL TYPE J_1BNFDOC-CANCEL,
    DOCDAT TYPE J_1BNFDOC-DOCDAT,
    MENGE  TYPE J_1BNFLIN-MENGE,
  END OF TY_NF,

  BEGIN OF TY_ZSDT0001,
    NU_DCO       TYPE ZDCO_VINCULO-NU_DCO,
    VBELN        TYPE ZDCO_VINCULO-VBELN,
    PLACA_CAV    TYPE ZSDT0001-PLACA_CAV,
    TP_MOVIMENTO TYPE ZSDT0001-TP_MOVIMENTO,
  END OF   TY_ZSDT0001,

  BEGIN OF TY_VBFA,
    VBELV   TYPE VBFA-VBELV,
    VBTYP_N TYPE VBFA-VBTYP_N,
    VBELN   TYPE J_1BNFLIN-REFKEY,
    VBTYP_V TYPE VBFA-VBTYP_V,
  END OF TY_VBFA,

  BEGIN OF TYPE_LFA1,
    LIFNR TYPE LFA1-LIFNR,
    LAND1 TYPE LFA1-LAND1,
    NAME1 TYPE LFA1-NAME1,
    REGIO TYPE LFA1-REGIO,
  END   OF TYPE_LFA1,

  BEGIN OF TY_SAIDA,
    CD_CENTRO        LIKE ZDCO_PRODUTOR-CD_CENTRO,
    NOME_CENTRO      TYPE NAME1,
    EMISSOR          LIKE KNA1-NAME1,
    TERMINAL         TYPE LFA1-NAME1,
    DOC_VENDA        LIKE ZDCO_PRODUTOR-VBELN,           " Ordem de venda
    NFENUM           TYPE J_1BNFDOC-NFENUM,
    CANCEL           TYPE J_1BNFDOC-CANCEL,
    NR_DCO           LIKE ZDCO_PRODUTOR-NR_DCO,
    DS_TIPO_LEILAO   TYPE ZDS_TIPO_LEILAO,
    PLACA            TYPE ZSDT0001-PLACA_CAV,
    DT_LANCAMENTO    LIKE ZDCO_PRODUTOR-DT_LANCAMENTO,
    PESO_SAIDA       TYPE J_1BNFLIN-MENGE,
    PESO_RETORNO     TYPE J_1BNFLIN-MENGE,
    QTD_VIM_EXP      TYPE J_1BNETQTY,
    ENTREGA          TYPE LFA1-ORT01,
    DCO_PROD         TYPE LFA1-NAME1,
    PRODUTO          TYPE MAKT-MAKTX,                    "Produto
    DOCNUM           TYPE J_1BNFDOC-DOCNUM,
    NFE_EXPORT       TYPE J_1BNFDOC-NFENUM,
    NU_AVISO         TYPE ZDCO_PRODUTOR-NU_AVISO,
    NFENUM_R         TYPE J_1BNFDOC-NFENUM,
    NFENUM_A         TYPE J_1BNFDOC-NFENUM,
    NFENUM_F         TYPE CHAR255,
    DS_NOME_TRANSPOR TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR,
    CHV_NF           TYPE C LENGTH 44,
    CHV_NF_EXP       TYPE C LENGTH 44,
    CHV_DACTE_R      TYPE C LENGTH 44,
    CHV_DACTE_A      TYPE C LENGTH 44,
    CHV_DACTE_F      TYPE C LENGTH 100,
    CHV_DACTE_F2     TYPE C LENGTH 100,
  END   OF TY_SAIDA,

  BEGIN OF TY_FERROV,
    DOCNUM    TYPE ZLEST0045-DOCNUM,
    NR_CTE    TYPE ZLEST0044-NR_CTE,
    CHAVE_CTE TYPE ZLEST0044-CHAVE_CTE,
  END OF TY_FERROV,

  BEGIN OF TY_CALC,
    NFENUM_DOC TYPE J_1BNFNUM9,
    DOCNUM_DOC TYPE J_1BDOCNUM,
    BRGEW_DOC  TYPE BRGEW_15,
    DOCNUM     TYPE J_1BDOCNUM,
    DOCNUM_RET TYPE J_1BDOCNUM,
    QUANT_VINC TYPE J_1BNETQTY,
  END OF TY_CALC.

DATA QTD_VINC TYPE J_1BNETQTY.

TYPES: BEGIN OF TY_ZCARTA.
        INCLUDE TYPE ZCARTA_CORRECAO.
TYPES: END OF TY_ZCARTA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES: BEGIN OF TY_RET.
        INCLUDE STRUCTURE ZSDT_RETLOTE.
TYPES: DOCNUM_RT TYPE VBELN_VF,
       END OF TY_RET,

       BEGIN OF TY_VBFA_.
        INCLUDE STRUCTURE VBFA.
TYPES: REFKEY TYPE J_1BNFLIN-REFKEY,
       END OF TY_VBFA_.



*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
DATA: T_DCO             TYPE TABLE OF TY_DCO,
      T_VBAK            TYPE TABLE OF TY_VBAK,
      T_KNA1            TYPE TABLE OF KNA1,
      T_NF              TYPE TABLE OF TY_NF,
      T_NFE_ACTIVE      TYPE TABLE OF J_1BNFE_ACTIVE WITH HEADER LINE,
      T_ZSDT0001        TYPE TABLE OF TY_ZSDT0001,
      T_RETLOTE         TYPE TABLE OF ZSDT_RETLOTE,
      T_RETLOTE_ALL     TYPE TABLE OF TY_RET,
      T_EXPORT          TYPE TABLE OF ZSDT_EXPORT,
      T_VBFA            TYPE TABLE OF TY_VBFA,
      IT_VBFA           TYPE TABLE OF TY_VBFA_,
      T_VBFA_N          TYPE TABLE OF VBFA WITH HEADER LINE,
      T_LIN             TYPE TABLE OF J_1BNFLIN,
      T_DOC             TYPE TABLE OF J_1BNFDOC,
      T_ZNOM_PROG_REME  TYPE TABLE OF ZNOM_PROG_REME,
      T_ZNOM_TRANSPORTE TYPE TABLE OF ZNOM_TRANSPORTE,
      T_VBFA_R          TYPE TABLE OF VBFA WITH HEADER LINE,
      T_VTTP            TYPE TABLE OF VTTP,
      T_VBAK_R          TYPE TABLE OF VBAK,
      T_ZLEST0060       TYPE TABLE OF ZLEST0060,
      T_J_1BNFLIN       TYPE TABLE OF J_1BNFLIN,
      T_LIN_CORRECAO    TYPE TABLE OF J_1BNFNAD,
      T_J_1BNFDOC       TYPE TABLE OF J_1BNFDOC,
      T_J1BNFDOC1       TYPE TABLE OF J_1BNFDOC,
      T_FERROV          TYPE TABLE OF TY_FERROV,
      T_ZCARTA          TYPE TABLE OF TY_ZCARTA,
      T_LFA1            TYPE TABLE OF TYPE_LFA1,
      IT_CALC           TYPE TABLE OF TY_CALC WITH HEADER LINE,
      T_SAIDA           TYPE TABLE OF TY_SAIDA.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA: WA_DCO             TYPE TY_DCO,
      WA_VBAK            TYPE TY_VBAK,
      WA_NF              TYPE TY_NF,
      WA_KNA1            TYPE KNA1,
      WA_ZSDT0001        TYPE TY_ZSDT0001,
      WA_RETLOTE         TYPE ZSDT_RETLOTE,
      WA_EXPORT          TYPE ZSDT_EXPORT,
      WA_VBFA            TYPE TY_VBFA,
      WA_VBFA_R          TYPE VBFA,
      WA_VBFA_N          TYPE VBFA,
      WA_LIN             TYPE J_1BNFLIN,
      WA_DOC             TYPE J_1BNFDOC,
      WA_ZNOM_PROG_REME  TYPE ZNOM_PROG_REME,
      WA_ZNOM_TRANSPORTE TYPE ZNOM_TRANSPORTE,
      WA_J_1BNFDOC       TYPE J_1BNFDOC,
      WA_VTTP            TYPE VTTP,
      WA_ZLEST0060       TYPE ZLEST0060,
      WA_VBAK_R          TYPE VBAK,
      WA_J1BNFLIN        TYPE J_1BNFLIN,
      WA_LIN_CORRECAO    TYPE J_1BNFNAD,
      WA_J1BNFDOC1       TYPE J_1BNFDOC,
      WA_FERROV          TYPE TY_FERROV,
      WA_ZCARTA          TYPE TY_ZCARTA,
      SL_LFA1            TYPE TYPE_LFA1,
      WA_SAIDA           TYPE TY_SAIDA.
*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*
DATA: X_DATA TYPE D,
      X_HORA TYPE SY-UZEIT.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      T_SORT       TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.

DATA: WG_REFKEY  TYPE C LENGTH 10,
      WG_REMESSA TYPE ZNOM_PROG_REME-ID_REMESSA.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.

SELECT-OPTIONS: P_NR_DCO  FOR ZDCO_PRODUTOR-NR_DCO          NO INTERVALS NO-EXTENSION,
                P_CD_CTR  FOR ZDCO_PRODUTOR-CD_CENTRO       NO INTERVALS NO-EXTENSION,
                P_EMISS   FOR VBAK-KUNNR                    NO INTERVALS NO-EXTENSION,
                P_VBELN   FOR ZDCO_PRODUTOR-VBELN           NO INTERVALS NO-EXTENSION,
                P_NFENUM  FOR J_1BNFDOC-NFENUM              NO INTERVALS NO-EXTENSION,
                P_TP_LEI  FOR ZDCO_PRODUTOR-CD_TIPO_LEILAO  NO INTERVALS NO-EXTENSION,
                P_DT_LCT  FOR ZDCO_PRODUTOR-DT_LANCAMENTO   NO-EXTENSION,
                P_MAT     FOR ZDCO_PRODUTOR-CD_MATERIAL     NO INTERVALS NO-EXTENSION,
                P_DCO_PR  FOR LFA1-LIFNR                    NO INTERVALS NO-EXTENSION.

PARAMETER: P_PLACA   TYPE ZSDT0001-PLACA_CAV,
           P_TERM    TYPE LFA1-LIFNR,
           P_ENTREG  TYPE LFA1-ORT01.

SELECTION-SCREEN END OF BLOCK B1.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
PERFORM F_INICIAR_VARIAVES.
PERFORM F_SELECIONA_DADOS.
PERFORM F_ORGANIZA_DADOS.
PERFORM F_IMPRIME_DADOS.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .
  DATA NR_DCO TYPE ZDCO_PRODUTOR-NR_DCO .

  "CONCATENATE P_NR_DCO+3(2) P_NR_DCO+6(3)  P_NR_DCO+10(4)  P_NR_DCO+15(1) INTO NR_DCO.

  NR_DCO = P_NR_DCO-LOW.

  " DCO
  IF NOT NR_DCO IS INITIAL.
    SELECT ZP~NU_DCO
           ZP~NR_DCO
           ZP~DT_LANCAMENTO
           ZP~QT_MATERIAL
           ZP~ID_FORNECEDOR
           ZP~CD_MATERIAL
           ZP~CD_CENTRO
           ZP~CD_SAFRA
           ZP~CD_TIPO_LEILAO
           ZP~VBELN
           ZP~QT_ENTREGUE
           ZP~QT_REMESSA
           ZP~NU_AVISO
           LF~NAME1
           T1~LGOBE
           T0~NAME1
           MK~MAKTX
           TL~DS_TIPO_LEILAO
      INTO TABLE T_DCO
      FROM ZDCO_PRODUTOR    AS ZP
      LEFT JOIN MAKT AS MK ON MK~SPRAS EQ SY-LANGU AND MK~MATNR EQ ZP~CD_MATERIAL
      LEFT JOIN ZDCO_TIPO_LEILAO AS TL ON TL~CD_TIPO_LEILAO EQ ZP~CD_TIPO_LEILAO
      LEFT JOIN LFA1 AS LF ON LF~LIFNR EQ ZP~ID_FORNECEDOR
      LEFT JOIN T001W AS T0 ON T0~WERKS EQ ZP~CD_CENTRO
      LEFT JOIN T001L AS T1 ON T1~WERKS EQ ZP~CD_CENTRO AND T1~LGORT EQ ZP~CD_SAFRA
     WHERE ZP~NR_DCO         EQ NR_DCO" p_nr_dco
       AND ZP~CD_CENTRO      IN P_CD_CTR
       AND ZP~VBELN          IN P_VBELN
       AND ZP~CD_TIPO_LEILAO IN P_TP_LEI
       AND ZP~CD_MATERIAL    IN P_MAT
       AND ZP~ID_FORNECEDOR  IN P_DCO_PR .
  ELSE.

    SELECT ZP~NU_DCO
           ZP~NR_DCO
           ZP~DT_LANCAMENTO
           ZP~QT_MATERIAL
           ZP~ID_FORNECEDOR
           ZP~CD_MATERIAL
           ZP~CD_CENTRO
           ZP~CD_SAFRA
           ZP~CD_TIPO_LEILAO
           ZP~VBELN
           ZP~QT_ENTREGUE
           ZP~QT_REMESSA
           ZP~NU_AVISO
           LF~NAME1
           T1~LGOBE
           T0~NAME1
           MK~MAKTX
           TL~DS_TIPO_LEILAO
      INTO TABLE T_DCO
      FROM ZDCO_PRODUTOR    AS ZP
      LEFT JOIN MAKT AS MK ON MK~SPRAS EQ SY-LANGU AND MK~MATNR EQ ZP~CD_MATERIAL
      LEFT JOIN ZDCO_TIPO_LEILAO AS TL ON TL~CD_TIPO_LEILAO EQ ZP~CD_TIPO_LEILAO
      LEFT JOIN LFA1 AS LF ON LF~LIFNR EQ ZP~ID_FORNECEDOR
      LEFT JOIN T001W AS T0 ON T0~WERKS EQ ZP~CD_CENTRO
      LEFT JOIN T001L AS T1 ON T1~WERKS EQ ZP~CD_CENTRO AND T1~LGORT EQ ZP~CD_SAFRA
     WHERE ZP~CD_CENTRO      IN P_CD_CTR
       AND ZP~VBELN          IN P_VBELN
       AND ZP~CD_TIPO_LEILAO IN P_TP_LEI
       AND ZP~CD_MATERIAL    IN P_MAT
       AND ZP~ID_FORNECEDOR  IN P_DCO_PR .

  ENDIF.

  " Vinculo do DCO
  SELECT ZV~NU_DCO ZV~VBELN RE~PLACA_CAV RE~TP_MOVIMENTO
    FROM ZDCO_VINCULO AS ZV
    LEFT JOIN ZSDT0001 AS RE ON RE~DOC_REM EQ ZV~VBELN
    INTO TABLE T_ZSDT0001
    FOR ALL ENTRIES IN T_DCO
    WHERE ZV~NU_DCO  EQ  T_DCO-NU_DCO.

  "EXCLUIR DA LISTA ROMANEIOS DE ENTRADA POIS EM ALGUM CASOS DE REMESSAS E TRANSFERENCIA QUANDO O ROMANEIO
  "CHEGA NO DESTINO RECEBE O DOCUMENTO DE REMESSA DA ORIGEM, COM ESSE PROCEDIMENTO EVITA A DUPLICAÇÃO DA INFORMAÇÃO
  DELETE T_ZSDT0001 WHERE TP_MOVIMENTO EQ 'E'.

  " Documento de vendas
  SELECT VBELN KUNNR
    FROM VBAK
    INTO TABLE T_VBAK
    FOR ALL ENTRIES IN T_DCO
    WHERE VBELN EQ T_DCO-DOC_VENDA
      AND KUNNR IN P_EMISS.

  " Cliente
  SELECT *
    FROM KNA1
    INTO TABLE T_KNA1
    FOR ALL ENTRIES IN T_VBAK
    WHERE KUNNR EQ T_VBAK-KUNNR.

  " NOTA FISCAL
  SELECT V~VGBEL NF~DOCNUM NF~NFENUM NF~CANCEL NF~DOCDAT NI~MENGE
    FROM J_1BNFLIN AS NI
    INNER JOIN VBRP AS V ON V~VBELN EQ NI~REFKEY
    INNER JOIN J_1BNFDOC AS NF ON NF~DOCNUM EQ NI~DOCNUM
    INTO TABLE T_NF
    FOR ALL ENTRIES IN T_ZSDT0001
    WHERE V~VGBEL EQ T_ZSDT0001-VBELN
      AND NF~NFENUM IN P_NFENUM
      AND NF~DOCDAT IN P_DT_LCT
      AND NF~CANCEL NE 'X'
* ---> S4 Migration - 17/07/2023 - LA
      AND V~DRAFT EQ SPACE.
* <--- S4 Migration - 17/07/2023 - LA

*  ** Modificação - Eduardo Ruttkowski Tavares - 02.10.2013 >>> INI
*  CH 1094485 - Inclusão do campo Nfe-Exportação
  IF SY-SUBRC IS INITIAL.

    SELECT *
     FROM ZSDT_RETLOTE
       INTO TABLE T_RETLOTE
       FOR ALL ENTRIES IN T_NF
         WHERE DOCNUM EQ T_NF-DOCNUM.

    IF SY-SUBRC IS INITIAL.

      SELECT *
       FROM ZSDT_RETLOTE
         INTO TABLE T_RETLOTE_ALL
         FOR ALL ENTRIES IN T_RETLOTE
           WHERE DOCNUM_RET EQ T_RETLOTE-DOCNUM_RET.

      LOOP AT T_RETLOTE_ALL ASSIGNING FIELD-SYMBOL(<ALL>). <ALL>-DOCNUM_RT = <ALL>-DOCNUM_RET. ENDLOOP.

      SORT T_RETLOTE_ALL BY DOCNUM_RET DOCNUM NFENUM.

      PERFORM SELECAO_NOTAS.

      SELECT *
        FROM ZSDT_EXPORT
          INTO TABLE T_EXPORT
          FOR ALL ENTRIES IN T_RETLOTE
            WHERE DOCNUM EQ T_RETLOTE-DOCNUM_RET.

      IF SY-SUBRC IS INITIAL.
        SELECT VBELV VBTYP_N VBELN
          FROM VBFA
            INTO TABLE T_VBFA
            FOR ALL ENTRIES IN T_EXPORT
              WHERE VBELV   EQ T_EXPORT-ORDEM
                AND VBTYP_N EQ 'M'.

        IF SY-SUBRC IS INITIAL.
          SELECT *
            FROM J_1BNFLIN
              INTO TABLE T_LIN
              FOR ALL ENTRIES IN T_VBFA
                WHERE REFKEY EQ T_VBFA-VBELN.

          IF SY-SUBRC IS INITIAL.
            SELECT *
              FROM J_1BNFDOC
                INTO TABLE T_DOC
                FOR ALL ENTRIES IN T_LIN
                  WHERE DOCNUM EQ T_LIN-DOCNUM.
          ENDIF.
        ENDIF.
***     Campo navio
        SELECT *
          FROM VBFA
          INTO CORRESPONDING FIELDS OF TABLE T_VBFA_N
       FOR ALL ENTRIES IN T_EXPORT
         WHERE VBELV   EQ T_EXPORT-ORDEM
           AND VBTYP_N EQ 'J'
           AND VBTYP_V EQ 'C'.

        IF SY-SUBRC IS INITIAL.
          LOOP AT T_VBFA_N.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = T_VBFA_N-VBELN
              IMPORTING
                OUTPUT = WG_REMESSA.

            SELECT ID_REMESSA ID_NOMEACAO_TRAN
              FROM ZNOM_PROG_REME
              APPENDING CORRESPONDING FIELDS OF TABLE T_ZNOM_PROG_REME
             WHERE ID_REMESSA EQ WG_REMESSA.
          ENDLOOP.

          IF T_ZNOM_PROG_REME IS NOT INITIAL.
            SELECT *
              FROM ZNOM_TRANSPORTE
              INTO CORRESPONDING FIELDS OF TABLE T_ZNOM_TRANSPORTE
           FOR ALL ENTRIES IN T_ZNOM_PROG_REME
             WHERE ID_NOMEACAO_TRAN = T_ZNOM_PROG_REME-ID_NOMEACAO_TRAN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 02.10.2013 <<< END

*** Campo DACTE Rodoviária
    SELECT *
      FROM VTTP AS A
      INTO CORRESPONDING FIELDS OF TABLE T_VTTP
      FOR ALL ENTRIES IN T_NF
      WHERE A~VBELN = T_NF-VGBEL
        AND EXISTS ( SELECT *
                       FROM VTTK AS B
                      WHERE B~TKNUM = A~TKNUM
                        AND B~VSART = '01' ). "Rodoviaria

    IF SY-SUBRC = 0.
      SELECT *
        FROM VBAK
        INTO CORRESPONDING FIELDS OF TABLE T_VBAK_R
        FOR ALL ENTRIES IN T_VTTP
        WHERE TKNUM = T_VTTP-TKNUM.

      IF SY-SUBRC = 0.
        SELECT *
          FROM VBFA
          INTO CORRESPONDING FIELDS OF TABLE T_VBFA_R
          FOR ALL ENTRIES IN T_VBAK_R
          WHERE VBELV = T_VBAK_R-VBELN
          AND VBTYP_N = 'M'
          AND VBTYP_V = 'C'.

        IF SY-SUBRC = 0.
          REFRESH T_J_1BNFLIN.
          CLEAR WG_REFKEY.

          LOOP AT T_VBFA_R.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = T_VBFA_R-VBELN
              IMPORTING
                OUTPUT = WG_REFKEY.

            SELECT *
              FROM J_1BNFLIN
              APPENDING CORRESPONDING FIELDS OF TABLE T_J_1BNFLIN
              WHERE REFKEY = WG_REFKEY.
          ENDLOOP.

*---> 04/07/2023 - Migração S4 - WS
  SORT T_J_1BNFLIN.
*<--- 04/07/2023 - Migração S4 - WS

          DELETE ADJACENT DUPLICATES FROM T_J_1BNFLIN COMPARING ALL FIELDS.

          IF T_J_1BNFLIN IS NOT INITIAL.
            SELECT *
              FROM J_1BNFDOC
              INTO CORRESPONDING FIELDS OF TABLE T_J_1BNFDOC
              FOR ALL ENTRIES IN T_J_1BNFLIN
              WHERE DOCNUM = T_J_1BNFLIN-DOCNUM.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*** Campo DACTE Aquaviário
  SELECT *
    FROM ZLEST0060
    INTO CORRESPONDING FIELDS OF TABLE T_ZLEST0060
    FOR ALL ENTRIES IN T_NF
    WHERE DOC_REM = T_NF-VGBEL.

  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM J_1BNFDOC
      INTO CORRESPONDING FIELDS OF TABLE T_J1BNFDOC1
      FOR ALL ENTRIES IN T_ZLEST0060
      WHERE DOCNUM = T_ZLEST0060-DOCNUM.
  ENDIF.

***  Campo DACTE Ferroviário
  SELECT DOCNUM NR_CTE ZLEST0044~CHAVE_CTE
    FROM ZLEST0044
    JOIN ZLEST0045 ON ZLEST0045~CHAVE_CTE = ZLEST0044~CHAVE_CTE
    INTO CORRESPONDING FIELDS OF TABLE T_FERROV
    FOR ALL ENTRIES IN T_NF
    WHERE ZLEST0045~DOCNUM = T_NF-DOCNUM.

***  Busca carta de correção
  SELECT *
    FROM ZCARTA_CORRECAO
    INTO CORRESPONDING FIELDS OF TABLE T_ZCARTA
    FOR ALL ENTRIES IN T_DOC
    WHERE DOCNUM        = T_DOC-DOCNUM
     AND  NOVO_TERMINAL <> ''.

  IF SY-SUBRC IS INITIAL.
    SORT T_ZCARTA BY DOCNUM  ASCENDING ID_CC DESCENDING.
    SELECT LIFNR LAND1 NAME1
        REGIO
       FROM LFA1
       APPENDING TABLE T_LFA1
       FOR ALL ENTRIES IN T_ZCARTA
     WHERE  LIFNR EQ T_ZCARTA-NOVO_TERMINAL.

    SORT T_LFA1 BY LIFNR ASCENDING.
  ENDIF.

  "Dados Active
  IF T_NF[] IS NOT INITIAL.
    SELECT *
      FROM J_1BNFE_ACTIVE APPENDING TABLE T_NFE_ACTIVE
       FOR ALL ENTRIES IN T_NF
     WHERE DOCNUM = T_NF-DOCNUM.
  ENDIF.

  IF T_DOC[] IS NOT INITIAL.
    SELECT *
      FROM J_1BNFE_ACTIVE APPENDING TABLE T_NFE_ACTIVE
       FOR ALL ENTRIES IN T_DOC
     WHERE DOCNUM = T_DOC-DOCNUM.
  ENDIF.

  IF T_J_1BNFDOC[] IS NOT INITIAL.
    SELECT *
      FROM J_1BNFE_ACTIVE APPENDING TABLE T_NFE_ACTIVE
       FOR ALL ENTRIES IN T_J_1BNFDOC
     WHERE DOCNUM = T_J_1BNFDOC-DOCNUM.
  ENDIF.

  IF T_J1BNFDOC1[] IS NOT INITIAL.
    SELECT *
      FROM J_1BNFE_ACTIVE APPENDING TABLE T_NFE_ACTIVE
       FOR ALL ENTRIES IN T_J1BNFDOC1
     WHERE DOCNUM = T_J1BNFDOC1-DOCNUM.
  ENDIF.

  SORT T_NFE_ACTIVE BY DOCNUM.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ORGANIZA_DADOS .
  DATA: WA_LFA1    TYPE LFA1,
        T_SADRVB   TYPE TABLE OF SADRVB,
        T_VBPAVB   TYPE TABLE OF VBPAVB,
        WA_VBPAVB  TYPE VBPAVB,
        PESO_SAIDA TYPE J_1BNFLIN-MENGE,
        EMISSOR    LIKE KNA1-NAME1,
        TERMINAL   TYPE LFA1-NAME1,
        PLACA      TYPE ZSDT0001-PLACA_CAV,
        ENTREGA    TYPE LFA1-ORT01,
        LV_SEPA    TYPE C LENGTH 1.

  SORT: T_DCO      BY NU_DCO,
        T_NF       BY VGBEL,
        T_VBAK     BY VBELN,
        T_ZSDT0001 BY NU_DCO,
        T_RETLOTE  BY DOCNUM,
        T_EXPORT   BY DOCNUM,
        T_VBFA     BY VBELN DESCENDING,
        T_LIN      BY REFKEY,
        T_DOC      BY DOCNUM.

  LOOP AT T_DCO INTO WA_DCO.

    LOOP AT T_ZSDT0001 INTO WA_ZSDT0001 WHERE NU_DCO  EQ  WA_DCO-NU_DCO.

      PLACA = WA_ZSDT0001-PLACA_CAV.

      IF ( NOT P_PLACA IS INITIAL  ) AND ( PLACA NE P_PLACA ).
        CONTINUE.
      ENDIF.

      IF ( NOT P_TERM IS INITIAL  ) AND  WA_LFA1-LIFNR NE P_TERM .
        CONTINUE.
      ENDIF.

      "Entrega
      CALL FUNCTION 'SD_PARTNER_READ'
        EXPORTING
          F_VBELN  = WA_DCO-DOC_VENDA
          OBJECT   = 'VBPA'
        TABLES
          I_XVBADR = T_SADRVB
          I_XVBPA  = T_VBPAVB.

      DELETE T_VBPAVB WHERE PARVW NE 'LR'.
      ENTREGA  = ''.

      CLEAR WA_LFA1.
      IF NOT T_VBPAVB[] IS INITIAL.

        READ TABLE T_VBPAVB INTO WA_VBPAVB INDEX 1.
        CLEAR WA_KNA1.
        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            P_PARCEIRO   = WA_VBPAVB-KUNNR
            P_PARTYPE    = 'C'
          CHANGING
            WA_INFO_PART = WA_LFA1
            WA_INFO_C    = WA_KNA1.

        IF WA_LFA1-ORT01 IS INITIAL.
          MOVE-CORRESPONDING WA_KNA1 TO WA_LFA1.
        ENDIF.
        ENTREGA  = WA_LFA1-ORT01.

      ENDIF.

      IF ( NOT P_ENTREG IS INITIAL  ) AND  ENTREGA NE P_ENTREG .
        CONTINUE.
      ENDIF.

      READ TABLE T_VBAK INTO WA_VBAK WITH KEY VBELN = WA_DCO-DOC_VENDA BINARY SEARCH.

      IF ( NOT P_EMISS IS INITIAL  ) AND  ( WA_VBAK-KUNNR NE P_EMISS ).
        CONTINUE.
      ENDIF.

      CLEAR WA_LFA1.

      READ TABLE T_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBAK-KUNNR.

      EMISSOR = WA_KNA1-NAME1.

*     NOTA FISCAL
      READ TABLE T_NF INTO WA_NF WITH KEY VGBEL = WA_ZSDT0001-VBELN.

      IF ( ( NOT P_NFENUM IS INITIAL ) AND ( WA_NF-NFENUM NE P_NFENUM ) ) OR
         ( ( NOT P_DT_LCT IS INITIAL ) AND ( WA_NF-DOCDAT NOT IN P_DT_LCT ) ).
        CONTINUE.
      ENDIF.

      IF SY-SUBRC IS INITIAL.
*     TERMINAL
**      Verifica se há carta de correção para o DocNum
        READ TABLE T_ZCARTA INTO WA_ZCARTA WITH KEY DOCNUM = WA_NF-DOCNUM.
        IF SY-SUBRC IS INITIAL.
          READ TABLE T_LFA1 INTO SL_LFA1
             WITH KEY LIFNR = WA_ZCARTA-NOVO_TERMINAL
             BINARY SEARCH.
          TERMINAL = SL_LFA1-NAME1.
        ELSE.
          CALL FUNCTION 'SD_PARTNER_READ'
            EXPORTING
              F_VBELN  = WA_DCO-DOC_VENDA
              OBJECT   = 'VBPA'
            TABLES
              I_XVBADR = T_SADRVB
              I_XVBPA  = T_VBPAVB.

          DELETE T_VBPAVB WHERE PARVW NE 'Z1'.

          CLEAR WA_LFA1.

          IF NOT T_VBPAVB[] IS INITIAL.

            READ TABLE T_VBPAVB INTO WA_VBPAVB INDEX 1.

            CALL FUNCTION 'Z_PARCEIRO_INFO'
              EXPORTING
                P_PARCEIRO   = WA_VBPAVB-LIFNR
                P_PARTYPE    = 'V'
              CHANGING
                WA_INFO_PART = WA_LFA1.

            TERMINAL = WA_LFA1-NAME1.
          ENDIF.
        ENDIF.

***     Campo DACTE Rodoviária
        READ TABLE T_VTTP INTO WA_VTTP WITH KEY VBELN = WA_NF-VGBEL.
        IF SY-SUBRC IS INITIAL.
          READ TABLE T_VBAK_R INTO WA_VBAK_R WITH KEY TKNUM = WA_VTTP-TKNUM.
          IF SY-SUBRC IS INITIAL.
            READ TABLE T_VBFA_R INTO WA_VBFA_R WITH KEY VBELV   = WA_VBAK_R-VBELN
                                                      VBTYP_N = 'M'
                                                      VBTYP_V = 'C'.
            IF SY-SUBRC IS INITIAL.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  INPUT  = WA_VBFA_R-VBELN
                IMPORTING
                  OUTPUT = WG_REFKEY.

              READ TABLE T_J_1BNFLIN INTO WA_J1BNFLIN WITH KEY REFKEY = WG_REFKEY.
              IF SY-SUBRC IS INITIAL.
                READ TABLE T_J_1BNFDOC INTO WA_J_1BNFDOC WITH KEY DOCNUM = WA_J1BNFLIN-DOCNUM.
                IF SY-SUBRC IS INITIAL.
                  WA_SAIDA-NFENUM_R = WA_J_1BNFDOC-NFENUM.

                  "Atribuir Chave Doc. Eletronico.
                  PERFORM F_MONTA_CHAVE USING WA_J_1BNFDOC-DOCNUM
                                     CHANGING WA_SAIDA-CHV_DACTE_R.

                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

***     Campo DACTE Aquaviário
        READ TABLE T_ZLEST0060 INTO WA_ZLEST0060 WITH KEY DOC_REM = WA_NF-VGBEL.
        IF SY-SUBRC IS INITIAL.
          READ TABLE T_J1BNFDOC1 INTO WA_J1BNFDOC1 WITH KEY DOCNUM = WA_ZLEST0060-DOCNUM.
          IF SY-SUBRC IS INITIAL.
            WA_SAIDA-NFENUM_A = WA_J1BNFDOC1-NFENUM.

            "Atribuir Chave Doc. Eletronico.
            PERFORM F_MONTA_CHAVE USING WA_J1BNFDOC1-DOCNUM
                               CHANGING WA_SAIDA-CHV_DACTE_A.

          ENDIF.
        ENDIF.

***     Campo DACTE Ferroviária
        LOOP AT T_FERROV INTO WA_FERROV WHERE DOCNUM = WA_NF-DOCNUM.
          IF WA_SAIDA-NFENUM_F IS INITIAL.
            WA_SAIDA-NFENUM_F = WA_FERROV-NR_CTE.
            WA_SAIDA-CHV_DACTE_F = WA_FERROV-CHAVE_CTE.
          ELSE.
            CONCATENATE WA_SAIDA-NFENUM_F WA_FERROV-NR_CTE INTO WA_SAIDA-NFENUM_F SEPARATED BY ','.

            IF STRLEN( WA_SAIDA-CHV_DACTE_F ) < 50. "Somente um chave

              CONCATENATE WA_SAIDA-CHV_DACTE_F WA_FERROV-CHAVE_CTE
                     INTO WA_SAIDA-CHV_DACTE_F SEPARATED BY ','.
            ELSE.
              IF WA_SAIDA-CHV_DACTE_F2 IS INITIAL.
                WA_SAIDA-CHV_DACTE_F2 = WA_FERROV-CHAVE_CTE.
              ELSE.
                CONCATENATE WA_SAIDA-CHV_DACTE_F2 WA_FERROV-CHAVE_CTE
                       INTO WA_SAIDA-CHV_DACTE_F2 SEPARATED BY ','.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDLOOP.

***     Modificação - Eduardo Ruttkowski Tavares - 02.10.2013 <<< END
        WA_SAIDA-NU_AVISO        = WA_DCO-NU_AVISO.
        WA_SAIDA-CD_CENTRO       = WA_DCO-CD_CENTRO.
        WA_SAIDA-NOME_CENTRO     = WA_DCO-NOME_CENTRO.
        WA_SAIDA-DOC_VENDA       = WA_DCO-DOC_VENDA.
        CONCATENATE WA_DCO-NR_DCO(2) '.' WA_DCO-NR_DCO+2(3) '.' WA_DCO-NR_DCO+5(4) '-' WA_DCO-NR_DCO+9(4) INTO WA_SAIDA-NR_DCO.
        WA_SAIDA-DS_TIPO_LEILAO  = WA_DCO-DS_TIPO_LEILAO.
        WA_SAIDA-DT_LANCAMENTO   = WA_NF-DOCDAT.
        WA_SAIDA-NFENUM          = WA_NF-NFENUM.

        "Atribuir Chave Doc. Eletronico.
        PERFORM F_MONTA_CHAVE USING WA_NF-DOCNUM
                           CHANGING WA_SAIDA-CHV_NF.

        WA_SAIDA-PESO_SAIDA      = WA_NF-MENGE.
        WA_SAIDA-EMISSOR         = EMISSOR.
        WA_SAIDA-TERMINAL        = TERMINAL.
        WA_SAIDA-PLACA           = PLACA.
        WA_SAIDA-DCO_PROD        = WA_DCO-NOME_FORNECEDOR.
        WA_SAIDA-ENTREGA         = ENTREGA.
        WA_SAIDA-PRODUTO         = WA_DCO-NOME_MATERIAL.
        WA_SAIDA-DOCNUM          = WA_NF-DOCNUM.

*** Modificação - Eduardo Ruttkowski Tavares - 02.10.2013 >>> INI
* CH 1094485 - Inclusão do campo Nfe-Exportação

        READ TABLE T_RETLOTE INTO WA_RETLOTE
          WITH KEY DOCNUM = WA_NF-DOCNUM
                   BINARY SEARCH.

        IF SY-SUBRC IS INITIAL. "Se tiver retorno, quebrar por retorno
          LOOP AT IT_CALC INTO DATA(WA_CALC) WHERE DOCNUM EQ WA_NF-DOCNUM.

            LOOP AT T_RETLOTE INTO WA_RETLOTE
                WHERE DOCNUM_RET = WA_CALC-DOCNUM_RET AND
                  DOCNUM =  WA_SAIDA-DOCNUM.

              WA_SAIDA-PESO_RETORNO =  WA_RETLOTE-QUANT_VINC.
              WA_SAIDA-QTD_VIM_EXP = WA_CALC-QUANT_VINC.
              WA_SAIDA-NFE_EXPORT = WA_CALC-NFENUM_DOC.

              READ TABLE T_EXPORT INTO WA_EXPORT
                WITH KEY DOCNUM = WA_RETLOTE-DOCNUM_RET
                         BINARY SEARCH.

              IF SY-SUBRC IS INITIAL.
                READ TABLE T_VBFA INTO WA_VBFA
                  WITH KEY VBELV   = WA_EXPORT-ORDEM
                           VBTYP_N = 'M'.

*                IF SY-SUBRC IS INITIAL.
*                  READ TABLE T_LIN INTO WA_LIN
*                    WITH KEY REFKEY = WA_VBFA-VBELN
*                             BINARY SEARCH.
*
*                  IF SY-SUBRC IS INITIAL.
*                    READ TABLE T_DOC INTO WA_DOC
*                      WITH KEY DOCNUM = WA_LIN-DOCNUM
*                               BINARY SEARCH.

                "Atribuir Chave Doc. Eletronico.
                PERFORM F_MONTA_CHAVE USING WA_CALC-DOCNUM_DOC
                                   CHANGING WA_SAIDA-CHV_NF_EXP.

*                  ENDIF.
*                ENDIF.

*  **         Campo navio
                READ TABLE T_VBFA_N INTO WA_VBFA_N
                  WITH KEY VBELV   = WA_EXPORT-ORDEM
                           VBTYP_N = 'J'
                           VBTYP_V = 'C'.

                IF SY-SUBRC IS INITIAL.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      INPUT  = WA_VBFA_N-VBELN
                    IMPORTING
                      OUTPUT = WG_REMESSA.

                  READ TABLE T_ZNOM_PROG_REME INTO WA_ZNOM_PROG_REME
                    WITH KEY ID_REMESSA = WG_REMESSA.

                  IF SY-SUBRC IS INITIAL.
                    READ TABLE T_ZNOM_TRANSPORTE INTO WA_ZNOM_TRANSPORTE
                      WITH KEY ID_NOMEACAO_TRAN = WA_ZNOM_PROG_REME-ID_NOMEACAO_TRAN.
                    IF SY-SUBRC IS INITIAL.
                      WA_SAIDA-DS_NOME_TRANSPOR = WA_ZNOM_TRANSPORTE-DS_NOME_TRANSPOR.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

              APPEND WA_SAIDA TO T_SAIDA.

            ENDLOOP.
          ENDLOOP.

        ELSE.
          APPEND WA_SAIDA TO T_SAIDA.
        ENDIF.

      ENDIF.

      CLEAR: WA_SAIDA, WA_VBAK, WA_NF, WA_LFA1, WA_KNA1, WA_ZSDT0001,
             WA_RETLOTE, WA_EXPORT, WA_VBFA, WA_LIN, WA_DOC.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .
  IF T_SAIDA[] IS INITIAL.
    MESSAGE I000(Z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.
  PERFORM F_DEFINIR_EVENTOS.
  PERFORM F_ALV_SORT.
  PERFORM F_MONTAR_LAYOUT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      I_CALLBACK_USER_COMMAND = 'F_USER_COMMAND'
      IT_FIELDCAT             = ESTRUTURA[]
      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'A'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
    TABLES
      T_OUTTAB                = T_SAIDA.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MONTAR_LAYOUT.
  PERFORM F_MONTAR_ESTRUTURA USING:

   "TABELA "CAMPO    "TAB INTERNA  "VARIAVEL DA WA "CAPTION

  01 ''   ''      'T_SAIDA' 'CD_CENTRO'            'Cd. Centro'         ' ',
  02 ''   ''      'T_SAIDA' 'NOME_CENTRO'          'Centro'             ' ',
  03 ''   ''      'T_SAIDA' 'EMISSOR'              'Emissor Ordem'      ' ',
  04 ''   ''      'T_SAIDA' 'TERMINAL'             'Z1-Terminal'        ' ',
  05 ''   ''      'T_SAIDA' 'DOC_VENDA'            'Ordem de Venda'     ' ',
  06 ''   ''      'T_SAIDA' 'PRODUTO'              'Material'           ' ',
  07 ''   ''      'T_SAIDA' 'DOCNUM'               'Nr. Documento'      ' ',
  08 ''   ''      'T_SAIDA' 'NFENUM'               'Nr. Nota'           ' ',
  09 ''   ''      'T_SAIDA' 'NR_DCO'               'DCO  '              ' ',
  10 ''   ''      'T_SAIDA' 'DS_TIPO_LEILAO'       'Tipo Leilão'        ' ',
  11 ''   ''      'T_SAIDA' 'PLACA'                'Placa'              ' ',
  12 ''   ''      'T_SAIDA' 'DT_LANCAMENTO'        'Dt. Lançamento'     ' ',
  13 ''   ''      'T_SAIDA' 'PESO_SAIDA'           'Qtde Remessa'       ' ',
  14 ''   ''      'T_SAIDA' 'PESO_RETORNO'         'Qtde Utilizada'     ' ',
  15 ''   ''      'T_SAIDA' 'QTD_VIM_EXP'          'Qtde Vinc. Exp'     ' ',
  16 ''   ''      'T_SAIDA' 'ENTREGA'              'Entrega'            ' ',
  17 ''   ''      'T_SAIDA' 'DCO_PROD'             'DCO Produtor'       ' ',
  18 ''   ''      'T_SAIDA' 'NFE_EXPORT'           'NF-e Exportação'    ' ',
  19 ''   ''      'T_SAIDA' 'NU_AVISO'             'Nr.Av.Leilão'       ' ',
  20 ''   ''      'T_SAIDA' 'NFENUM_R'             'DACTE Rodoviária'   ' ',
  21 ''   ''      'T_SAIDA' 'NFENUM_A'             'DACTE Aquaviária'   ' ',
  22 ''   ''      'T_SAIDA' 'NFENUM_F'             'DACTE Ferroviária'  ' ',
  23 ''   ''      'T_SAIDA' 'DS_NOME_TRANSPOR '    'Navio'              ' ',
  24 ''   ''      'T_SAIDA' 'CHV_NF'               'Chv.NF-e'           ' ',
  25 ''   ''      'T_SAIDA' 'CHV_NF_EXP'           'Chv.NF-e Exp.'      ' ',
  26 ''   ''      'T_SAIDA' 'CHV_DACTE_R'          'Chv.Dacte.Rod.'     ' ',
  27 ''   ''      'T_SAIDA' 'CHV_DACTE_A'          'Chv.Dacte.Aqua.'    ' ',
  28 ''   ''      'T_SAIDA' 'CHV_DACTE_F'          'Chv.Dacte.Ferrov.'  ' ',
  29 ''   ''      'T_SAIDA' 'CHV_DACTE_F2'         'Chv.Dacte.Ferrov.2' ' '.

ENDFORM.                    " F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM F_MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                              VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                              VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                              VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                              VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                              VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                              VALUE(P_OUTPUTLEN).

  DATA: X_CONTADOR TYPE STRING.
  CLEAR: WA_ESTRUTURA, X_CONTADOR.

  X_CONTADOR = STRLEN( P_SCRTEXT_L ).

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
  WA_ESTRUTURA-OUTPUTLEN     = X_CONTADOR.


  IF P_FIELD EQ 'DOCNUM'.
    WA_ESTRUTURA-HOTSPOT = 'X'.
  ELSE.
    CLEAR WA_ESTRUTURA-HOTSPOT.
  ENDIF.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " F_montar_estrutura

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES.

  DATA: W_TEXTO1(40).
  DATA: W_TEXTO2(20).

  V_REPORT = SY-REPID.


*** Nome do Report
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.

*  SELECT SINGLE butxt FROM t001 INTO w_texto2
*    WHERE bukrs EQ p_bukrs.
*
*  CONCATENATE 'Empresa:' p_bukrs '-' w_texto2 INTO w_texto1 SEPARATED BY space.
*** Nome da empresa
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO1.

*  SELECT SINGLE name1 FROM t001w INTO w_texto2
*    WHERE werks = p_branch.
*
*  CONCATENATE 'Filial:' p_branch  '-' w_texto2 INTO  w_texto1 SEPARATED BY space.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.

  WRITE: SY-DATUM TO W_TEXTO2.
  CONCATENATE 'Data:' W_TEXTO2 INTO W_TEXTO1 SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.
  WRITE: SY-UZEIT TO W_TEXTO2.
  CONCATENATE 'Hora:' W_TEXTO2 INTO W_TEXTO1 SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.
  CONCATENATE 'Usuário:' SY-UNAME INTO W_TEXTO1 SEPARATED BY SPACE.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' W_TEXTO1.
ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT.
ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM F_USER_COMMAND USING L_UCOMM
                          L_SELFIELD TYPE SLIS_SELFIELD.

  DATA: VL_NFOBJN TYPE J_1BINTERF-NFOBJN,
        VL_DOCNUM TYPE J_1BNFDOC-DOCNUM.

  IF L_SELFIELD-FIELDNAME = 'DOCNUM'.
    READ TABLE T_SAIDA INDEX L_SELFIELD-TABINDEX INTO WA_SAIDA.

*    SET PARAMETER ID 'JEF' FIELD l_selfield-value.
*
*    CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

    VL_DOCNUM = L_SELFIELD-VALUE.

    CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
      EXPORTING
        DOC_NUMBER         = VL_DOCNUM
      IMPORTING
        OBJ_NUMBER         = VL_NFOBJN
      EXCEPTIONS
        DOCUMENT_NOT_FOUND = 1
        DOCUM_LOCK         = 2
        OTHERS             = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
      EXPORTING
        OBJ_NUMBER         = VL_NFOBJN
      EXCEPTIONS
        OBJECT_NOT_FOUND   = 1
        SCR_CTRL_NOT_FOUND = 2
        OTHERS             = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDFORM.                    "f_user_command
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_CHAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NF_DOCNUM  text
*      <--P_WA_SAIDA_CHV_NF  text
*----------------------------------------------------------------------*
FORM F_MONTA_CHAVE  USING P_DOCNUM TYPE J_1BNFE_ACTIVE-DOCNUM
                 CHANGING P_CHV    TYPE C.

  CLEAR: P_CHV.
  CHECK P_DOCNUM IS NOT INITIAL.

  READ TABLE T_NFE_ACTIVE WITH KEY DOCNUM = P_DOCNUM BINARY SEARCH.

  CHECK SY-SUBRC = 0.

  CONCATENATE T_NFE_ACTIVE-REGIO
              T_NFE_ACTIVE-NFYEAR
              T_NFE_ACTIVE-NFMONTH
              T_NFE_ACTIVE-STCD1
              T_NFE_ACTIVE-MODEL
              T_NFE_ACTIVE-SERIE
              T_NFE_ACTIVE-NFNUM9
              T_NFE_ACTIVE-DOCNUM9
              T_NFE_ACTIVE-CDV INTO DATA(_CHV).

  IF STRLEN( _CHV ) NE 44.
    CLEAR: _CHV.
    EXIT.
  ENDIF.

  IF P_CHV IS INITIAL.
    P_CHV = _CHV.
  ELSE.
    CONCATENATE P_CHV _CHV INTO P_CHV SEPARATED BY ','.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECAO_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECAO_NOTAS .

  SELECT *
    FROM ZNOM_REMETENTE
    INTO TABLE @DATA(IT_REMETENTE)
    FOR ALL ENTRIES IN @T_RETLOTE_ALL
      WHERE DOCNUM_RT EQ @T_RETLOTE_ALL-DOCNUM_RT.

  IF SY-SUBRC IS INITIAL.

    SELECT *
      FROM VBFA
      INTO TABLE IT_VBFA
      FOR  ALL ENTRIES IN IT_REMETENTE
        WHERE VBELV EQ IT_REMETENTE-NR_ORDEM
         AND VBTYP_N = 'M'
         AND VBTYP_V = 'C'.

    LOOP AT IT_VBFA ASSIGNING FIELD-SYMBOL(<VBFA>). <VBFA>-REFKEY = <VBFA>-VBELN. ENDLOOP.

    IF SY-SUBRC IS INITIAL.

      SELECT *
        FROM J_1BNFLIN
          INTO TABLE @DATA(IT_LIN)
          FOR ALL ENTRIES IN @IT_VBFA
            WHERE REFKEY EQ @IT_VBFA-REFKEY.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM J_1BNFDOC
          INTO TABLE @DATA(IT_DOC)
          FOR ALL ENTRIES IN @IT_LIN
            WHERE DOCNUM EQ @IT_LIN-DOCNUM.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT IT_DOC INTO DATA(WA_DOC).
    LOOP AT IT_LIN INTO DATA(WA_LIN) WHERE DOCNUM EQ WA_DOC-DOCNUM.
      LOOP AT IT_VBFA INTO DATA(WA_VBFA_) WHERE REFKEY EQ WA_LIN-REFKEY.
        LOOP AT IT_REMETENTE INTO DATA(WA_REM) WHERE NR_ORDEM EQ WA_VBFA_-VBELV.
          LOOP AT T_RETLOTE_ALL ASSIGNING FIELD-SYMBOL(<ALL>) WHERE DOCNUM_RT EQ WA_REM-DOCNUM_RT AND QUANT_VINC IS NOT INITIAL.

            IF QTD_VINC EQ WA_DOC-BRGEW.
              EXIT.
            ENDIF.

            ADD <ALL>-QUANT_VINC TO QTD_VINC.

            IF QTD_VINC <= WA_DOC-BRGEW.
            ELSE.
              SUBTRACT <ALL>-QUANT_VINC FROM QTD_VINC.
              DATA(COMPLEMENTO) = WA_DOC-BRGEW - QTD_VINC.
              ADD COMPLEMENTO TO QTD_VINC.
              SUBTRACT COMPLEMENTO FROM <ALL>-QUANT_VINC.
            ENDIF.

            APPEND VALUE #(
                            NFENUM_DOC = WA_DOC-NFENUM
                            DOCNUM_DOC = WA_DOC-DOCNUM
                            BRGEW_DOC  = WA_DOC-BRGEW
                            DOCNUM     = <ALL>-DOCNUM
                            DOCNUM_RET = <ALL>-DOCNUM_RET
                            QUANT_VINC = COND #( WHEN COMPLEMENTO IS NOT INITIAL THEN COMPLEMENTO ELSE <ALL>-QUANT_VINC )
                          ) TO IT_CALC.

            IF QTD_VINC <= WA_DOC-BRGEW.
              IF COMPLEMENTO IS INITIAL.
                <ALL>-QUANT_VINC = 0.
              ENDIF.
            ENDIF.

            CLEAR IT_CALC.

          ENDLOOP.
          QTD_VINC = 0.
          COMPLEMENTO = 0.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
