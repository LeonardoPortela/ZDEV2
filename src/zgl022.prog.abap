*&---------------------------------------------------------------------*
*& Report  ZGL022
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZGL022.

TABLES: T001, SKB1, ZGLT049.

*&---------------------------------------------------------------------*
* TYPE DEFINITION
*&---------------------------------------------------------------------*
TYPE-POOLS: ICON.

TYPES: BEGIN OF TY_RESUMO,
         EMPRESA        TYPE BUKRS,
         BALANCO        TYPE ZFIED008,
         NOTA           TYPE ZFIED032,
         CONTA          TYPE SKAT-SAKNR,
         DS_CONTA       TYPE TXT50,
         OBJ_CUSTO      TYPE KOSAR,
         OBJ_CUSTO_TX   TYPE KTEXT,
         OBJ_LUCRO      TYPE PRCTR,
         OBJ_LUCRO_TX   TYPE LTEXT,
         OBJ_GRUPO      TYPE MATKL,
         OBJ_GRUPO_TX   TYPE WGBEZ60,
         VL_FECHAMENTO  TYPE HSLXX12,
         VL_COMPARATIVO TYPE HSLXX12,
         VL_ULTIMO      TYPE HSLXX12,
         TIPO           TYPE MITKZ,
       END OF TY_RESUMO,

       BEGIN OF TY_PARTNER,
         CONTA     TYPE SAKNR,
         CONTA_CTR TYPE SAKNR,
         DS_CONTA  TYPE TXT50,
         PARTNER   TYPE J_1BPARID,
         NOME      TYPE NAME1_GP,
         SALDO     TYPE HSLXX12,
       END OF TY_PARTNER,

       BEGIN OF TY_RESUMO_ALV,
         ICONE          TYPE ICON-ID,
         EMPRESA        TYPE BUKRS,
         BALANCO        TYPE ZFIED008,
         NOTA           TYPE ZFIED032,
         CONTA          TYPE SKAT-SAKNR,
         DS_CONTA       TYPE TXT50,
         OBJ_CUSTO      TYPE KOSAR,
         OBJ_CUSTO_TX   TYPE KTEXT,
         OBJ_LUCRO      TYPE PRCTR,
         OBJ_LUCRO_TX   TYPE LTEXT,
         OBJ_GRUPO      TYPE MATKL,
         OBJ_GRUPO_TX   TYPE WGBEZ60,
         VL_FECHAMENTO  TYPE HSLXX12,
         VL_COMPARATIVO TYPE HSLXX12,
         VL_ULTIMO      TYPE HSLXX12,
         TIPO           TYPE MITKZ,
       END OF TY_RESUMO_ALV.

*&---------------------------------------------------------------------*
* DATA DEFINITION
*&---------------------------------------------------------------------*

DATA: LC_ANO_F          TYPE GJAHR,
      LC_ANO_C          TYPE GJAHR,
      LC_ANO_U          TYPE GJAHR,
      LC_MES_F          TYPE C LENGTH 2,
      LC_MES_C          TYPE C LENGTH 2,
      LC_MES_U          TYPE C LENGTH 2,
      LC_CAMPO          TYPE C LENGTH 512,
      LC_CAMPO_MOEDA(1).

DATA: WA_SKB1        TYPE SKB1,
      IT_SKB1        TYPE TABLE OF SKB1      WITH HEADER LINE,
      IT_SKAT        TYPE TABLE OF SKAT      WITH HEADER LINE,
      IT_ZGLT041     TYPE TABLE OF ZGLT041   WITH HEADER LINE,
      IT_ZGLT039     TYPE TABLE OF ZGLT039   WITH HEADER LINE,
      IT_ZGLT049CN   TYPE TABLE OF ZGLT049CN WITH HEADER LINE,
      IT_ZGLT049LN   TYPE TABLE OF ZGLT049LN WITH HEADER LINE,
      IT_ZGLT049MN   TYPE TABLE OF ZGLT049MN WITH HEADER LINE,
      IT_TKT05       TYPE TABLE OF TKT05 WITH HEADER LINE, "Categorias de centros de custos - textos
      IT_CEPCT       TYPE TABLE OF CEPCT WITH HEADER LINE, "Textos de dados mestre de centro de lucro
      IT_T023T       TYPE TABLE OF T023T WITH HEADER LINE, "Denominações para grupos de mercadoria
      IT_FAGLFLEXT_F TYPE TABLE OF FAGLFLEXT,
      IT_FAGLFLEXT_C TYPE TABLE OF FAGLFLEXT,
      IT_FAGLFLEXT_U TYPE TABLE OF FAGLFLEXT,
      IT_RESUMO      TYPE SORTED TABLE OF TY_RESUMO WITH UNIQUE KEY EMPRESA BALANCO NOTA CONTA DS_CONTA TIPO OBJ_CUSTO OBJ_CUSTO_TX OBJ_LUCRO OBJ_LUCRO_TX OBJ_GRUPO OBJ_GRUPO_TX WITH HEADER LINE,
      IT_RESUMO_ALV  TYPE TABLE OF TY_RESUMO_ALV WITH HEADER LINE,
      WA_RESUMO      LIKE LINE OF IT_RESUMO,
      WA_RESUMO_ALV  LIKE LINE OF IT_RESUMO_ALV,
      WA_FAGLFLEXT   TYPE FAGLFLEXT,
      IT_PARTNER     TYPE TABLE OF TY_PARTNER,
      WA_PARTNER     LIKE LINE OF IT_PARTNER,
      IT_PARTNER_ALV TYPE TABLE OF TY_PARTNER,
      IT_DRE_CC      TYPE TABLE OF ZGL015_DRE_EST04 WITH HEADER LINE,
      IT_DRE_LL      TYPE TABLE OF ZGL015_DRE_EST05 WITH HEADER LINE,
      IT_DRE_MM      TYPE TABLE OF ZGL015_DRE_EST06 WITH HEADER LINE.

RANGES: IT_BUKRS_FILTRO FOR T001-BUKRS.

"1 – Parâmetro
*&---------------------------------------------------------------------*
* SELECTION CRITERIA
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-H01.
SELECT-OPTIONS: S_BUKRS FOR T001-BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION.
PARAMETER : S_MES_F TYPE ZFIED005 OBLIGATORY,
            S_MES_C TYPE ZFIED005,
            S_MES_U TYPE ZFIED005,
            S_MOEDA TYPE WAERS OBLIGATORY DEFAULT 'BRL',
            S_BUKR2 TYPE T001-BUKRS NO-DISPLAY,
            S_ESTRB TYPE ZGLT046-VERSNT NO-DISPLAY DEFAULT '1',
            S_VERSN TYPE ZGLT046-VERSN MATCHCODE OBJECT ZH_ZGLT046,
            S_DRE1  TYPE ZGL015_DRE_EST01-VERSN  NO-DISPLAY,
            S_DRE2  TYPE ZGL015_DRE_EST01-VERSN  NO-DISPLAY,
            S_DRE3  TYPE ZGL015_DRE_EST01-VERSN  NO-DISPLAY,
            S_DRE4  TYPE ZGL015_DRE_EST01-VERSN  NO-DISPLAY.
"1 - Balanço Patrimonial = 2 -  Demons. Result. Exercício = 3 - Demons. Receita Operacional
SELECT-OPTIONS: S_NTCL   FOR ZGLT049-COD_CLAS_BAL MATCHCODE OBJECT ZH_ZGLT039_C,
                S_NOTA   FOR ZGLT049-COD_CLAS_NOT MATCHCODE OBJECT ZH_ZGLT039_N,
                S_CONTA  FOR SKB1-SAKNR    NO-DISPLAY,
                S_NIVEL  FOR ZGLT049-NIVEL NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

*&---------------------------------------------------------------------*
* TYPE INCLUDE
*&---------------------------------------------------------------------*
INCLUDE: ZGL022_0001,
         ZGL022_0002.

*&---------------------------------------------------------------------*
*&   Event START-OF-SELECTION.
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  RANGES: IT_VERSN FOR ZGLT046-VERSN.

  DATA:  LC_ZERO               TYPE HSLXX12,
         IT_SALDOS_FECHAMENTO  TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
         IT_SALDOS_COMPARATIVO TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
         IT_SALDOS_ULTIMO      TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,

         IT_FECHAMENTO         TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
         IT_COMPARATIVO        TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
         IT_ULTIMO             TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,

         IT_CONTAS             TYPE ZCT_EMP_CONTAS,
         WA_CONTAS             TYPE ZLC_EMP_CONTAS.
  "DATA:  CELLCOLOR TYPE LVC_S_SCOL.

  CLEAR: IT_SKB1,
         IT_SKAT.

  IF S_VERSN IS NOT INITIAL.
    SELECT SINGLE VERSNT INTO S_ESTRB FROM ZGLT046 WHERE VERSN EQ S_VERSN.
  ENDIF.

  "2 – Selecionar Contas
  CASE S_MOEDA.
    WHEN 'BRL'.
      LC_CAMPO_MOEDA = 'H'.
    WHEN 'USD'.
      LC_CAMPO_MOEDA = 'K'.
  ENDCASE.

  CLEAR: IT_BUKRS_FILTRO.
  IT_BUKRS_FILTRO-SIGN   = 'I'.
  IT_BUKRS_FILTRO-OPTION = 'EQ'.
  IT_BUKRS_FILTRO-LOW    = S_BUKRS-LOW.
  IT_BUKRS_FILTRO-HIGH   = S_BUKRS-LOW.
  APPEND IT_BUKRS_FILTRO.

  CLEAR: IT_VERSN.
  IF NOT S_VERSN IS INITIAL.
    IT_VERSN-SIGN   = 'I'.
    IT_VERSN-OPTION = 'EQ'.
    IT_VERSN-LOW    = S_VERSN.
    IT_VERSN-HIGH   = S_VERSN.
    APPEND IT_VERSN.
  ENDIF.

  IF S_BUKR2 IS NOT INITIAL.
    CLEAR: IT_BUKRS_FILTRO.
    IT_BUKRS_FILTRO-SIGN   = 'I'.
    IT_BUKRS_FILTRO-OPTION = 'EQ'.
    IT_BUKRS_FILTRO-LOW    = S_BUKR2.
    IT_BUKRS_FILTRO-HIGH   = S_BUKR2.
    APPEND IT_BUKRS_FILTRO.
  ENDIF.

  IF S_NTCL IS INITIAL AND S_NOTA IS INITIAL.
    SELECT * "#EC CI_DB_OPERATION_OK[2431747]
      INTO TABLE IT_SKB1
      FROM SKB1 AS D
     WHERE D~BUKRS IN IT_BUKRS_FILTRO
       AND D~SAKNR IN S_CONTA
       AND EXISTS ( SELECT *
                      FROM ZGLT041 AS C
                     INNER JOIN ZGLT049 AS N ON N~COD_CLAS_BAL EQ C~COD_CLAS_BAL AND N~COD_CLAS_NOT EQ C~COD_CLAS_NOT2
                     INNER JOIN ZGLT046 AS E ON E~VERSN        EQ N~VERSN        AND E~VERSNT       EQ S_ESTRB
                     WHERE C~BUKRS EQ D~BUKRS
                       AND C~SAKNR EQ D~SAKNR
                       AND E~VERSN IN IT_VERSN
                       AND C~COD_CLAS_BAL  IN S_NTCL
                       AND C~COD_CLAS_NOT2 IN S_NOTA ).
*                       AND C~GJAHR EQ S_MES_F+2(4) ). "/Modificação CS2017000372
  ELSEIF S_NTCL IS NOT INITIAL AND S_NOTA IS INITIAL.
    SELECT * "#EC CI_DB_OPERATION_OK[2431747]
      INTO TABLE IT_SKB1
      FROM SKB1 AS S
     WHERE BUKRS IN IT_BUKRS_FILTRO
       AND SAKNR IN S_CONTA
       AND EXISTS ( SELECT * FROM ZGLT041 AS Z
                     WHERE Z~BUKRS EQ S~BUKRS
                       AND Z~SAKNR EQ S~SAKNR
                       AND Z~COD_CLAS_BAL IN S_NTCL )
*                       AND Z~GJAHR EQ S_MES_F+2(4) )   "/Modificação CS2017000372
       AND EXISTS ( SELECT *
                      FROM ZGLT041 AS C
                     INNER JOIN ZGLT049 AS N ON N~COD_CLAS_BAL EQ C~COD_CLAS_BAL AND N~COD_CLAS_NOT EQ C~COD_CLAS_NOT2
                     INNER JOIN ZGLT046 AS E ON E~VERSN        EQ N~VERSN        AND E~VERSNT       EQ S_ESTRB
                     WHERE C~BUKRS EQ S~BUKRS
                       AND C~SAKNR EQ S~SAKNR
                       AND E~VERSN IN IT_VERSN
                       AND C~COD_CLAS_BAL  IN S_NTCL
                       AND C~COD_CLAS_NOT2 IN S_NOTA ).
*                       AND C~GJAHR EQ S_MES_F+2(4) ). "/Modificação CS2017000372
  ELSEIF S_NTCL IS INITIAL AND S_NOTA IS NOT INITIAL.
    SELECT * "#EC CI_DB_OPERATION_OK[2431747]
      INTO TABLE IT_SKB1
      FROM SKB1 AS S
     WHERE BUKRS IN IT_BUKRS_FILTRO
       AND SAKNR IN S_CONTA
       AND EXISTS ( SELECT * FROM ZGLT041 AS Z
                     WHERE Z~BUKRS EQ S~BUKRS
                       AND Z~SAKNR EQ S~SAKNR
                       AND Z~COD_CLAS_NOT2 IN S_NOTA )
*                       AND Z~GJAHR EQ S_MES_F+2(4) )   "/Modificação CS2017000372
       AND EXISTS ( SELECT *
                      FROM ZGLT041 AS C
                     INNER JOIN ZGLT049 AS N ON N~COD_CLAS_BAL EQ C~COD_CLAS_BAL AND N~COD_CLAS_NOT EQ C~COD_CLAS_NOT2
                     INNER JOIN ZGLT046 AS E ON E~VERSN        EQ N~VERSN        AND E~VERSNT       EQ S_ESTRB
                     WHERE C~BUKRS EQ S~BUKRS
                       AND C~SAKNR EQ S~SAKNR
                       AND E~VERSN         IN IT_VERSN
                       AND C~COD_CLAS_BAL  IN S_NTCL
                       AND C~COD_CLAS_NOT2 IN S_NOTA ).
*                       AND C~GJAHR EQ S_MES_F+2(4) ). "/Modificação CS2017000372
  ELSEIF S_NTCL IS NOT INITIAL AND S_NOTA IS NOT INITIAL.
    SELECT * "#EC CI_DB_OPERATION_OK[2431747]
      INTO TABLE IT_SKB1
      FROM SKB1 AS S
     WHERE BUKRS IN IT_BUKRS_FILTRO
       AND SAKNR IN S_CONTA
       AND EXISTS ( SELECT * FROM ZGLT041 AS Z
                     WHERE Z~BUKRS EQ S~BUKRS
                       AND Z~SAKNR EQ S~SAKNR
                       AND Z~COD_CLAS_BAL  IN S_NTCL
                       AND Z~COD_CLAS_NOT2 IN S_NOTA )
*                       AND Z~GJAHR EQ S_MES_F+2(4) )   "/Modificação CS2017000372
       AND EXISTS ( SELECT *
                      FROM ZGLT041 AS C
                     INNER JOIN ZGLT049 AS N ON N~COD_CLAS_BAL EQ C~COD_CLAS_BAL AND N~COD_CLAS_NOT EQ C~COD_CLAS_NOT2
                     INNER JOIN ZGLT046 AS E ON E~VERSN        EQ N~VERSN        AND E~VERSNT       EQ S_ESTRB
                     WHERE C~BUKRS EQ S~BUKRS
                       AND C~SAKNR EQ S~SAKNR
                       AND E~VERSN IN IT_VERSN
                       AND C~COD_CLAS_BAL  IN S_NTCL
                       AND C~COD_CLAS_NOT2 IN S_NOTA ).
*                       AND C~GJAHR EQ S_MES_F+2(4) ). "/Modificação CS2017000372
  ENDIF.

  IF IT_SKB1[] IS INITIAL.
    MESSAGE 'Não encontrato Mestre da Conta do Razão (empresa)' TYPE 'S'.
    EXIT.
  ENDIF.

  SORT IT_SKB1 BY SAKNR.

  SELECT *
    INTO TABLE IT_SKAT
    FROM SKAT
     FOR ALL ENTRIES IN IT_SKB1
   WHERE SPRAS EQ SY-LANGU
     AND KTOPL EQ '0050'
     AND SAKNR EQ IT_SKB1-SAKNR.

  IF IT_SKAT[] IS INITIAL.
    MESSAGE 'Não encontrato Mestre de contas do Razão (plano de contas: denominação)' TYPE 'S'.
    EXIT.
  ENDIF.

  SORT IT_SKAT BY SAKNR.

  "3 – Busca  de Dados Parâmetros Gerais de Conciliação

  SELECT *
    INTO TABLE IT_ZGLT041
    FROM ZGLT041
     FOR ALL ENTRIES IN IT_SKB1
   WHERE BUKRS         IN IT_BUKRS_FILTRO
     AND SAKNR         EQ IT_SKB1-SAKNR
     AND COD_CLAS_BAL  IN S_NTCL
     AND COD_CLAS_NOT2 IN S_NOTA.
*     AND GJAHR         EQ S_MES_F+2(4). "/Modificação CS2017000372

  IF IT_ZGLT041[] IS INITIAL.
    "   MESSAGE 'Não encotrato Parâmetros Gerais de Reconciliação Contábil' TYPE 'S'.
    "   EXIT.
  ENDIF.

  SORT IT_ZGLT041 BY BUKRS SAKNR.

  "4 – Busca  de Dados Descrição Conciliação (Balanço/Nota)

  SELECT *
    INTO TABLE IT_ZGLT039
    FROM ZGLT039
     FOR ALL ENTRIES IN IT_ZGLT041
   WHERE CODIGO   EQ IT_ZGLT041-COD_CLAS_BAL
     AND COD_NOTA EQ IT_ZGLT041-COD_CLAS_NOT2.

  IF IT_ZGLT039[] IS INITIAL.
    "   MESSAGE 'Não encotrato Classificação do balanço para reconciliação' TYPE 'S'.
    "   EXIT.
  ENDIF.

  SORT IT_ZGLT039 BY CODIGO COD_NOTA.

  "5 – Tabela de Saldo

  IF S_ESTRB EQ '1'. " Balanço DF's

    "Dados do Fechamento
    LC_ANO_F = S_MES_F+2(4).
    LC_MES_F = S_MES_F(2).

    CLEAR: IT_SALDOS_FECHAMENTO[],
           IT_SALDOS_COMPARATIVO[],
           IT_SALDOS_ULTIMO[],
           IT_CONTAS[].

    LOOP AT IT_SKB1.
      WA_CONTAS-BUKRS = IT_SKB1-BUKRS.
      WA_CONTAS-SAKNR = IT_SKB1-SAKNR.
      APPEND WA_CONTAS TO IT_CONTAS.
    ENDLOOP.

    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        RYEAR                = LC_ANO_F
        WAERS                = S_MOEDA
        CONTAS               = IT_CONTAS
        P_GERAR_SOC_PARCEIRA = ABAP_TRUE
      TABLES
        IT_SALDOS            = IT_SALDOS_FECHAMENTO
      EXCEPTIONS
        MOEDA_NAO_ADM        = 1
        ERRO_LEDGER          = 2
        OTHERS               = 3.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    "Dados do Comparativo
    IF S_MES_C IS NOT INITIAL.

      LC_ANO_C = S_MES_C+2(4).
      LC_MES_C = S_MES_C(2).

      IF LC_ANO_C EQ LC_ANO_F.
        MOVE IT_SALDOS_FECHAMENTO[] TO IT_SALDOS_COMPARATIVO[].
      ELSE.
        CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
          EXPORTING
            RYEAR                = LC_ANO_C
            WAERS                = S_MOEDA
            CONTAS               = IT_CONTAS
            P_GERAR_SOC_PARCEIRA = ABAP_TRUE
          TABLES
            IT_SALDOS            = IT_SALDOS_COMPARATIVO
          EXCEPTIONS
            MOEDA_NAO_ADM        = 1
            ERRO_LEDGER          = 2
            OTHERS               = 3.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

    ENDIF.

    "Dados do último mês exercício encerrado
    IF S_MES_U IS NOT INITIAL.

      LC_ANO_U = S_MES_U+2(4).
      LC_MES_U = S_MES_U(2).

      IF LC_ANO_U EQ LC_ANO_F.
        MOVE IT_SALDOS_FECHAMENTO[] TO IT_SALDOS_ULTIMO[].
      ELSEIF LC_ANO_U EQ LC_ANO_C.
        MOVE IT_SALDOS_COMPARATIVO[] TO IT_SALDOS_ULTIMO[].
      ELSE.
        CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
          EXPORTING
            RYEAR                = LC_ANO_U
            WAERS                = S_MOEDA
            CONTAS               = IT_CONTAS
            P_GERAR_SOC_PARCEIRA = ABAP_TRUE
          TABLES
            IT_SALDOS            = IT_SALDOS_ULTIMO
          EXCEPTIONS
            MOEDA_NAO_ADM        = 1
            ERRO_LEDGER          = 2
            OTHERS               = 3.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

    ENDIF.

    PERFORM LIMPA_CONS_SPARCEIRA.

    "Gerar Resumo Contas de Balanço Patrimonial
    LOOP AT IT_SKB1.

      CLEAR: WA_RESUMO.

      WA_RESUMO-EMPRESA = IT_SKB1-BUKRS.
      WA_RESUMO-CONTA   = IT_SKB1-SAKNR.
      WA_RESUMO-TIPO    = IT_SKB1-MITKZ.

      "Desc. Conta """"""""""""""""""""""""""""""""""""""""""""""""""""""
      READ TABLE IT_SKAT WITH KEY SAKNR = IT_SKB1-SAKNR BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        WA_RESUMO-DS_CONTA = IT_SKAT-TXT50.
      ENDIF.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "Nota""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      READ TABLE IT_ZGLT041 WITH KEY BUKRS = WA_RESUMO-EMPRESA
                                     SAKNR = WA_RESUMO-CONTA.
      IF SY-SUBRC IS INITIAL.
        READ TABLE IT_ZGLT039 WITH KEY CODIGO   = IT_ZGLT041-COD_CLAS_BAL
                                       COD_NOTA = IT_ZGLT041-COD_CLAS_NOT2.
        IF SY-SUBRC IS INITIAL.
          WA_RESUMO-BALANCO = IT_ZGLT039-DESCR.
          WA_RESUMO-NOTA    = IT_ZGLT039-DESCR_NOTA.
        ENDIF.
      ENDIF.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      PERFORM BUSCA_SALDO USING WA_RESUMO-EMPRESA
                                WA_RESUMO-CONTA
                       CHANGING WA_RESUMO-VL_FECHAMENTO
                                WA_RESUMO-VL_COMPARATIVO
                                WA_RESUMO-VL_ULTIMO.

      COLLECT WA_RESUMO INTO IT_RESUMO.

    ENDLOOP.

    "Ajustar Lucro/Prejuizo
    IF S_VERSN IS NOT INITIAL.
      PERFORM AJUSTA_SALDO_LUCRO_PREJU.
    ENDIF.

  ELSEIF S_ESTRB EQ '2' OR S_ESTRB EQ '3'. "2 -  Demons. Result. Exercício; 3 - Demons. Receita Operacional

    DATA: WA_ZFIT0039   TYPE ZFIT0039,
          WA_ZFIT0039_C TYPE ZFIT0039,
          P_MONAT       TYPE MONAT.

    SELECT *
      INTO TABLE IT_ZGLT049CN
      FROM ZGLT049CN
       FOR ALL ENTRIES IN IT_ZGLT041
     WHERE VERSN        EQ S_VERSN
       AND NIVEL        IN S_NIVEL
       AND COD_CLAS_BAL EQ IT_ZGLT041-COD_CLAS_BAL
       AND COD_CLAS_NOT EQ IT_ZGLT041-COD_CLAS_NOT2.

    SORT IT_ZGLT049CN BY NIVEL COD_CLAS_BAL COD_CLAS_NOT.

    SELECT *
      INTO TABLE IT_ZGLT049LN
      FROM ZGLT049LN
       FOR ALL ENTRIES IN IT_ZGLT041
     WHERE VERSN        EQ S_VERSN
       AND NIVEL        IN S_NIVEL
       AND COD_CLAS_BAL EQ IT_ZGLT041-COD_CLAS_BAL
       AND COD_CLAS_NOT EQ IT_ZGLT041-COD_CLAS_NOT2.

    SORT IT_ZGLT049LN BY NIVEL COD_CLAS_BAL COD_CLAS_NOT.

    SELECT *
      INTO TABLE IT_ZGLT049MN
      FROM ZGLT049MN
       FOR ALL ENTRIES IN IT_ZGLT041
     WHERE VERSN        EQ S_VERSN
       AND NIVEL        IN S_NIVEL
       AND COD_CLAS_BAL EQ IT_ZGLT041-COD_CLAS_BAL
       AND COD_CLAS_NOT EQ IT_ZGLT041-COD_CLAS_NOT2.

    SORT IT_ZGLT049MN BY NIVEL COD_CLAS_BAL COD_CLAS_NOT.

    IF NOT IT_ZGLT049CN[] IS INITIAL.
      SELECT * INTO TABLE IT_TKT05
        FROM TKT05
         FOR ALL ENTRIES IN IT_ZGLT049CN
       WHERE SPRAS EQ SY-LANGU
         AND KOSAR EQ IT_ZGLT049CN-KOSAR.

      SORT IT_TKT05 BY KOSAR.
    ENDIF.

    IF NOT IT_ZGLT049LN[] IS INITIAL.
      SELECT * INTO TABLE IT_CEPCT
        FROM CEPCT
         FOR ALL ENTRIES IN IT_ZGLT049LN
       WHERE SPRAS EQ SY-LANGU
         AND PRCTR EQ IT_ZGLT049LN-PRCTR.

      SORT IT_CEPCT BY PRCTR.
    ENDIF.

    IF NOT IT_ZGLT049MN[] IS INITIAL.
      SELECT * INTO TABLE IT_T023T
        FROM T023T
         FOR ALL ENTRIES IN IT_ZGLT049MN
       WHERE SPRAS EQ SY-LANGU
         AND MATKL EQ IT_ZGLT049MN-MATKL.

      SORT IT_T023T BY MATKL.
    ENDIF.

    "Dados do Fechamento
    LC_ANO_F = S_MES_F+2(4).
    LC_MES_F = S_MES_F(2).
    MOVE LC_MES_F TO P_MONAT.

    SELECT SINGLE * INTO WA_ZFIT0039 FROM ZFIT0039 WHERE BUKRS IN S_BUKRS.

    SELECT * INTO TABLE IT_DRE_CC FROM ZGL015_DRE_EST04 WHERE VERSN EQ WA_ZFIT0039-VERSN.
    SELECT * INTO TABLE IT_DRE_LL FROM ZGL015_DRE_EST05 WHERE VERSN EQ WA_ZFIT0039-VERSN.
    SELECT * INTO TABLE IT_DRE_MM FROM ZGL015_DRE_EST06 WHERE VERSN EQ WA_ZFIT0039-VERSN.

    IF S_BUKR2 IS INITIAL.

      CALL FUNCTION 'Z_PESQUISA_DRE'
        EXPORTING
          I_BUKRS      = S_BUKRS-LOW
          I_GJAHR      = LC_ANO_F
          I_MONATI     = 01
          I_MONATF     = P_MONAT
          I_VERSN      = S_DRE1
          I_WAERS      = S_MOEDA
        TABLES
          T_ZGL030_EST = IT_FECHAMENTO.
    ELSE.
      SELECT SINGLE * INTO WA_ZFIT0039_C FROM ZFIT0039 WHERE BUKRS EQ S_BUKR2.

      SELECT * APPENDING TABLE IT_DRE_CC FROM ZGL015_DRE_EST04 WHERE VERSN EQ WA_ZFIT0039_C-VERSN.
      SELECT * APPENDING TABLE IT_DRE_LL FROM ZGL015_DRE_EST05 WHERE VERSN EQ WA_ZFIT0039_C-VERSN.
      SELECT * APPENDING TABLE IT_DRE_MM FROM ZGL015_DRE_EST06 WHERE VERSN EQ WA_ZFIT0039_C-VERSN.

      CALL FUNCTION 'Z_PESQUISA_DRE'
        EXPORTING
          I_BUKRS      = S_BUKRS-LOW
          I_GJAHR      = LC_ANO_F
          I_MONATI     = 01
          I_MONATF     = P_MONAT
          I_VERSN      = S_DRE1
          I_WAERS      = S_MOEDA
          I_CON        = ABAP_TRUE
          I_BUKRS2     = S_BUKR2
          I_VERSN2     = S_DRE3
        TABLES
          T_ZGL030_EST = IT_FECHAMENTO.
    ENDIF.

    SORT IT_DRE_CC BY SAKNR KOSAR.
    SORT IT_DRE_LL BY SAKNR KOKRS PRCTR.
    SORT IT_DRE_MM BY SAKNR MATKL.

    "Dados do Comparativo
    IF S_MES_C IS NOT INITIAL.

      LC_ANO_C = S_MES_C+2(4).
      LC_MES_C = S_MES_C(2).
      MOVE LC_MES_C TO P_MONAT.

      IF ( LC_ANO_C EQ LC_ANO_F ) AND ( LC_MES_C EQ LC_MES_F ).
        MOVE IT_FECHAMENTO[] TO IT_COMPARATIVO[].
      ELSE.
        IF S_BUKR2 IS INITIAL.
          CALL FUNCTION 'Z_PESQUISA_DRE'
            EXPORTING
              I_BUKRS      = S_BUKRS-LOW
              I_GJAHR      = LC_ANO_C
              I_MONATI     = 01
              I_MONATF     = P_MONAT
              I_VERSN      = S_DRE2
              I_WAERS      = S_MOEDA
            TABLES
              T_ZGL030_EST = IT_COMPARATIVO.
        ELSE.
          CALL FUNCTION 'Z_PESQUISA_DRE'
            EXPORTING
              I_BUKRS      = S_BUKRS-LOW
              I_GJAHR      = LC_ANO_C
              I_MONATI     = 01
              I_MONATF     = P_MONAT
              I_VERSN      = S_DRE2
              I_WAERS      = S_MOEDA
              I_CON        = ABAP_TRUE
              I_BUKRS2     = S_BUKR2
              I_VERSN2     = S_DRE4
            TABLES
              T_ZGL030_EST = IT_COMPARATIVO.
        ENDIF.
      ENDIF.
    ENDIF.

    "Dados do último mês exercício encerrado
    IF S_MES_U IS NOT INITIAL.

      LC_ANO_U = S_MES_U+2(4).
      LC_MES_U = S_MES_U(2).
      MOVE LC_MES_U TO P_MONAT.

      IF ( LC_ANO_U EQ LC_ANO_F ) AND ( LC_MES_U EQ LC_MES_F ).
        MOVE IT_FECHAMENTO[] TO IT_ULTIMO[].
      ELSEIF ( LC_ANO_U EQ LC_ANO_C ) AND ( LC_MES_U EQ LC_MES_C ).
        MOVE IT_COMPARATIVO[] TO IT_ULTIMO[].
      ELSE.
        IF S_BUKR2 IS INITIAL.
          CALL FUNCTION 'Z_PESQUISA_DRE'
            EXPORTING
              I_BUKRS      = S_BUKRS-LOW
              I_GJAHR      = LC_ANO_U
              I_MONATI     = 01
              I_MONATF     = P_MONAT
              I_VERSN      = S_DRE2
              I_WAERS      = S_MOEDA
            TABLES
              T_ZGL030_EST = IT_ULTIMO.
        ELSE.
          CALL FUNCTION 'Z_PESQUISA_DRE'
            EXPORTING
              I_BUKRS      = S_BUKRS-LOW
              I_GJAHR      = LC_ANO_U
              I_MONATI     = 01
              I_MONATF     = P_MONAT
              I_VERSN      = S_DRE2
              I_WAERS      = S_MOEDA
              I_CON        = ABAP_TRUE
              I_BUKRS2     = S_BUKR2
              I_VERSN2     = S_DRE4
            TABLES
              T_ZGL030_EST = IT_ULTIMO.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA: CK_CUSTO(1),
          CK_LUCRO(1),
          CK_MERCA(1),
          P_GRAVAR(1),
          P_SEM_OBJ_DRE(1).

    "Gerar Resumo Contas de Resultado
    LOOP AT IT_SKB1.

      CLEAR: WA_RESUMO.

      WA_RESUMO-EMPRESA = IT_SKB1-BUKRS.
      WA_RESUMO-CONTA   = IT_SKB1-SAKNR.
      WA_RESUMO-TIPO    = IT_SKB1-MITKZ.

      "Desc. Conta """"""""""""""""""""""""""""""""""""""""""""""""""""""
      READ TABLE IT_SKAT WITH KEY SAKNR = IT_SKB1-SAKNR BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        WA_RESUMO-DS_CONTA = IT_SKAT-TXT50.
      ENDIF.
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      "Nota""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      LOOP AT IT_ZGLT041 WHERE BUKRS = WA_RESUMO-EMPRESA
                           AND SAKNR = WA_RESUMO-CONTA.

        READ TABLE IT_ZGLT039 WITH KEY CODIGO   = IT_ZGLT041-COD_CLAS_BAL
                                       COD_NOTA = IT_ZGLT041-COD_CLAS_NOT2.
        IF SY-SUBRC IS INITIAL.
          WA_RESUMO-BALANCO = IT_ZGLT039-DESCR.
          WA_RESUMO-NOTA    = IT_ZGLT039-DESCR_NOTA.
        ENDIF.

        CK_CUSTO = ABAP_FALSE.
        CK_LUCRO = ABAP_FALSE.
        CK_MERCA = ABAP_FALSE.

        LOOP AT S_NIVEL.

          READ TABLE IT_ZGLT049CN WITH KEY NIVEL        = S_NIVEL-LOW
                                           COD_CLAS_BAL = IT_ZGLT041-COD_CLAS_BAL
                                           COD_CLAS_NOT = IT_ZGLT041-COD_CLAS_NOT2
                                           BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            CK_CUSTO = ABAP_TRUE.
            LOOP AT IT_ZGLT049CN WHERE NIVEL        = S_NIVEL-LOW
                                   AND COD_CLAS_BAL = IT_ZGLT041-COD_CLAS_BAL
                                   AND COD_CLAS_NOT = IT_ZGLT041-COD_CLAS_NOT2.
              PERFORM BUSCA_SALDO_CTA_RESULTADO USING WA_RESUMO-EMPRESA
                                                      WA_RESUMO-CONTA
                                                      IT_ZGLT049CN-KOSAR
                                                      ABAP_FALSE
                                                      ABAP_FALSE
                                             CHANGING P_GRAVAR.
              COLLECT WA_RESUMO INTO IT_RESUMO.
            ENDLOOP.
          ENDIF.


          READ TABLE IT_ZGLT049LN WITH KEY NIVEL        = S_NIVEL-LOW
                                           COD_CLAS_BAL = IT_ZGLT041-COD_CLAS_BAL
                                           COD_CLAS_NOT = IT_ZGLT041-COD_CLAS_NOT2
                                           BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            CK_LUCRO = ABAP_TRUE.
            LOOP AT IT_ZGLT049LN WHERE NIVEL        = S_NIVEL-LOW
                                   AND COD_CLAS_BAL = IT_ZGLT041-COD_CLAS_BAL
                                   AND COD_CLAS_NOT = IT_ZGLT041-COD_CLAS_NOT2.
              PERFORM BUSCA_SALDO_CTA_RESULTADO USING WA_RESUMO-EMPRESA
                                                      WA_RESUMO-CONTA
                                                      ABAP_FALSE
                                                      IT_ZGLT049LN-PRCTR
                                                      ABAP_FALSE
                                             CHANGING P_GRAVAR.
              COLLECT WA_RESUMO INTO IT_RESUMO.
            ENDLOOP.
          ENDIF.


          READ TABLE IT_ZGLT049MN WITH KEY NIVEL        = S_NIVEL-LOW
                                           COD_CLAS_BAL = IT_ZGLT041-COD_CLAS_BAL
                                           COD_CLAS_NOT = IT_ZGLT041-COD_CLAS_NOT2
                                           BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            CK_MERCA = ABAP_TRUE.
            LOOP AT IT_ZGLT049MN WHERE NIVEL        = S_NIVEL-LOW
                                   AND COD_CLAS_BAL = IT_ZGLT041-COD_CLAS_BAL
                                   AND COD_CLAS_NOT = IT_ZGLT041-COD_CLAS_NOT2.
              PERFORM BUSCA_SALDO_CTA_RESULTADO USING WA_RESUMO-EMPRESA
                                                      WA_RESUMO-CONTA
                                                      ABAP_FALSE
                                                      ABAP_FALSE
                                                      IT_ZGLT049MN-MATKL
                                             CHANGING P_GRAVAR.
              COLLECT WA_RESUMO INTO IT_RESUMO.
            ENDLOOP.
          ENDIF.

        ENDLOOP.

        P_SEM_OBJ_DRE = ABAP_TRUE.

        LOOP AT IT_FECHAMENTO WHERE SAKNR EQ WA_RESUMO-CONTA.
          IF IT_FECHAMENTO-KOSAR IS NOT INITIAL OR
             IT_FECHAMENTO-PRCTR IS NOT INITIAL OR
             IT_FECHAMENTO-MATKL IS NOT INITIAL.
            P_SEM_OBJ_DRE = ABAP_FALSE.
          ENDIF.
        ENDLOOP.

        LOOP AT IT_COMPARATIVO WHERE SAKNR EQ WA_RESUMO-CONTA.
          IF IT_COMPARATIVO-KOSAR IS NOT INITIAL OR
             IT_COMPARATIVO-PRCTR IS NOT INITIAL OR
             IT_COMPARATIVO-MATKL IS NOT INITIAL.
            P_SEM_OBJ_DRE = ABAP_FALSE.
          ENDIF.
        ENDLOOP.

        LOOP AT IT_ULTIMO WHERE SAKNR EQ WA_RESUMO-CONTA.
          IF IT_ULTIMO-KOSAR IS NOT INITIAL OR
             IT_ULTIMO-PRCTR IS NOT INITIAL OR
             IT_ULTIMO-MATKL IS NOT INITIAL.
            P_SEM_OBJ_DRE = ABAP_FALSE.
          ENDIF.
        ENDLOOP.

        IF P_SEM_OBJ_DRE EQ ABAP_TRUE.
          DELETE IT_RESUMO WHERE CONTA EQ WA_RESUMO-CONTA.
        ENDIF.

        IF CK_CUSTO EQ ABAP_FALSE OR CK_LUCRO EQ ABAP_FALSE OR CK_MERCA EQ ABAP_FALSE OR
           P_SEM_OBJ_DRE EQ ABAP_TRUE.
          PERFORM BUSCA_SALDO_CTA_RESULTADO USING WA_RESUMO-EMPRESA
                                                  WA_RESUMO-CONTA
                                                  ABAP_FALSE
                                                  ABAP_FALSE
                                                  ABAP_FALSE
                                           CHANGING P_GRAVAR.
          IF P_GRAVAR EQ ABAP_TRUE.
            COLLECT WA_RESUMO INTO IT_RESUMO.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDIF.

  LC_ZERO = 0.

  DELETE IT_RESUMO WHERE VL_FECHAMENTO  EQ LC_ZERO
                     AND VL_COMPARATIVO EQ LC_ZERO
                     AND VL_ULTIMO      EQ LC_ZERO.

  IF IT_RESUMO[] IS INITIAL.
    MESSAGE 'Não foram encontrados dados para apresentação do relatório' TYPE 'S'.
    EXIT.
  ENDIF.

  LOOP AT IT_RESUMO INTO WA_RESUMO.
    CLEAR: WA_RESUMO_ALV.
    MOVE-CORRESPONDING WA_RESUMO TO WA_RESUMO_ALV.

    CASE WA_RESUMO-TIPO.
      WHEN 'K' OR 'D'.
        WA_RESUMO_ALV-ICONE = ICON_TABLE_SETTINGS.
    ENDCASE.

    APPEND WA_RESUMO_ALV TO IT_RESUMO_ALV.
  ENDLOOP.

  CALL SCREEN 0001.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO
*&---------------------------------------------------------------------*
*       Busca saldo Fechamento Comparativo Ultimo
*----------------------------------------------------------------------*
FORM BUSCA_SALDO  USING    P_EMPRESA     TYPE BUKRS
                           P_CONTA       TYPE SAKNR
                  CHANGING P_FECHAMENTO  TYPE HSLXX12
                           P_COMPARATIVO TYPE HSLXX12
                           P_VL_ULTIMO   TYPE HSLXX12.

  DATA: VG_MONAT TYPE MONAT,
        REFE1    TYPE HSLXX12,
        LC_KTOKS TYPE SKA1-KTOKS.

  SELECT SINGLE C~KTOKS INTO LC_KTOKS "#EC CI_DB_OPERATION_OK[2389136]
    FROM SKA1 AS C "#EC CI_DB_OPERATION_OK[2431747]
    INNER JOIN T001 AS T ON T~KTOPL EQ C~KTOPL
   WHERE C~SAKNR EQ P_CONTA
     AND T~BUKRS EQ P_EMPRESA.

  P_FECHAMENTO  = 0.
  P_COMPARATIVO = 0.
  P_VL_ULTIMO   = 0.

  LOOP AT IT_SALDOS_FECHAMENTO WHERE RBUKRS = P_EMPRESA
                                 AND RACCT  = P_CONTA.
    P_FECHAMENTO = P_FECHAMENTO + IT_SALDOS_FECHAMENTO-SLVT.

    MOVE LC_MES_F TO VG_MONAT.

    IF VG_MONAT EQ 12.
      IF ( LC_KTOKS EQ 'YB05' ) OR ( LC_KTOKS EQ 'YB06' ) OR ( LC_KTOKS EQ 'YB07' ) OR
         ( LC_KTOKS EQ 'YB08' ) OR ( LC_KTOKS EQ 'ERG.' ).
        VG_MONAT = 15.
      ELSE.
        VG_MONAT = 16.
      ENDIF.
    ENDIF.

    DO VG_MONAT TIMES
      VARYING REFE1 FROM IT_SALDOS_FECHAMENTO-SL01 NEXT IT_SALDOS_FECHAMENTO-SL02.
      P_FECHAMENTO = P_FECHAMENTO + REFE1.
    ENDDO.
  ENDLOOP.

  LOOP AT IT_SALDOS_COMPARATIVO WHERE RBUKRS = P_EMPRESA
                                 AND RACCT  = P_CONTA.
    P_COMPARATIVO = P_COMPARATIVO + IT_SALDOS_COMPARATIVO-SLVT.

    MOVE LC_MES_C TO VG_MONAT.

    IF VG_MONAT EQ 12.
      IF ( LC_KTOKS EQ 'YB05' ) OR ( LC_KTOKS EQ 'YB06' ) OR ( LC_KTOKS EQ 'YB07' ) OR
         ( LC_KTOKS EQ 'YB08' ) OR ( LC_KTOKS EQ 'ERG.' ).
        VG_MONAT = 15.
      ELSE.
        VG_MONAT = 16.
      ENDIF.
    ENDIF.

    DO VG_MONAT TIMES
      VARYING REFE1 FROM IT_SALDOS_COMPARATIVO-SL01 NEXT IT_SALDOS_COMPARATIVO-SL02.
      P_COMPARATIVO = P_COMPARATIVO + REFE1.
    ENDDO.
  ENDLOOP.

  LOOP AT IT_SALDOS_ULTIMO WHERE RBUKRS = P_EMPRESA
                             AND RACCT  = P_CONTA.

    P_VL_ULTIMO = P_VL_ULTIMO + IT_SALDOS_ULTIMO-SLVT.

    MOVE LC_MES_U TO VG_MONAT.

    IF VG_MONAT EQ 12.
      IF ( LC_KTOKS EQ 'YB05' ) OR ( LC_KTOKS EQ 'YB06' ) OR ( LC_KTOKS EQ 'YB07' ) OR
         ( LC_KTOKS EQ 'YB08' ) OR ( LC_KTOKS EQ 'ERG.' ).
        VG_MONAT = 15.
      ELSE.
        VG_MONAT = 16.
      ENDIF.
    ENDIF.

    DO VG_MONAT TIMES
      VARYING REFE1 FROM IT_SALDOS_ULTIMO-SL01 NEXT IT_SALDOS_ULTIMO-SL02.
      P_VL_ULTIMO = P_VL_ULTIMO + REFE1.
    ENDDO.
  ENDLOOP.

ENDFORM.                    " BUSCA_SALDO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO_CTA_RESULTADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUSCA_SALDO_CTA_RESULTADO  USING    P_WA_RESUMO_EMPRESA
                                         P_WA_RESUMO_CONTA
                                         P_ZGLT049C
                                         P_ZGLT049L
                                         P_ZGLT049M
                                CHANGING P_GRAVAR.

  DATA: CK_OBJETO(1).

  CLEAR:
  WA_RESUMO-OBJ_CUSTO   ,
  WA_RESUMO-OBJ_CUSTO_TX,
  WA_RESUMO-OBJ_LUCRO   ,
  WA_RESUMO-OBJ_LUCRO_TX,
  WA_RESUMO-OBJ_GRUPO   ,
  WA_RESUMO-OBJ_GRUPO_TX,
  WA_RESUMO-VL_FECHAMENTO,
  WA_RESUMO-VL_COMPARATIVO,
  WA_RESUMO-VL_ULTIMO.

  P_GRAVAR = ABAP_FALSE.

  IF P_ZGLT049C EQ ABAP_FALSE AND P_ZGLT049L EQ ABAP_FALSE AND P_ZGLT049M EQ ABAP_FALSE.

    CK_OBJETO = ABAP_FALSE.

    READ TABLE IT_DRE_CC WITH KEY SAKNR = WA_RESUMO-CONTA BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      CK_OBJETO = ABAP_TRUE.
    ENDIF.

    READ TABLE IT_DRE_LL WITH KEY SAKNR = WA_RESUMO-CONTA BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      CK_OBJETO = ABAP_TRUE.
    ENDIF.

    READ TABLE IT_DRE_MM WITH KEY SAKNR = WA_RESUMO-CONTA BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      CK_OBJETO = ABAP_TRUE.
    ENDIF.

    IF CK_OBJETO NE ABAP_TRUE.
      LOOP AT IT_FECHAMENTO WHERE SAKNR EQ WA_RESUMO-CONTA.
        WA_RESUMO-VL_FECHAMENTO = WA_RESUMO-VL_FECHAMENTO + IT_FECHAMENTO-VLR_ACM.
      ENDLOOP.

      LOOP AT IT_COMPARATIVO WHERE SAKNR EQ WA_RESUMO-CONTA.
        WA_RESUMO-VL_COMPARATIVO = WA_RESUMO-VL_COMPARATIVO + IT_COMPARATIVO-VLR_ACM.
      ENDLOOP.

      LOOP AT IT_ULTIMO WHERE SAKNR EQ WA_RESUMO-CONTA.
        WA_RESUMO-VL_ULTIMO = WA_RESUMO-VL_ULTIMO + IT_ULTIMO-VLR_ACM.
      ENDLOOP.

      P_GRAVAR = ABAP_TRUE.
    ENDIF.

  ELSE.

    "Tipo de Centro de Custo
    IF P_ZGLT049C NE ABAP_FALSE.

      READ TABLE IT_TKT05 WITH KEY KOSAR = P_ZGLT049C.
      IF SY-SUBRC IS INITIAL.
        WA_RESUMO-OBJ_CUSTO    = IT_TKT05-KOSAR.
        WA_RESUMO-OBJ_CUSTO_TX = IT_TKT05-KTEXT.
      ENDIF.

      LOOP AT IT_FECHAMENTO WHERE SAKNR EQ WA_RESUMO-CONTA
                              AND KOSAR EQ P_ZGLT049C.
        WA_RESUMO-VL_FECHAMENTO = WA_RESUMO-VL_FECHAMENTO + IT_FECHAMENTO-VLR_ACM.
      ENDLOOP.

      LOOP AT IT_COMPARATIVO WHERE SAKNR EQ WA_RESUMO-CONTA
                              AND KOSAR EQ P_ZGLT049C.
        WA_RESUMO-VL_COMPARATIVO = WA_RESUMO-VL_COMPARATIVO + IT_COMPARATIVO-VLR_ACM.
      ENDLOOP.

      LOOP AT IT_ULTIMO WHERE SAKNR EQ WA_RESUMO-CONTA
                              AND KOSAR EQ P_ZGLT049C.
        WA_RESUMO-VL_ULTIMO = WA_RESUMO-VL_ULTIMO + IT_ULTIMO-VLR_ACM.
      ENDLOOP.

    ENDIF.

    "Centro de Lucro
    IF P_ZGLT049L NE ABAP_FALSE.
      READ TABLE IT_CEPCT WITH KEY PRCTR = P_ZGLT049L.
      IF SY-SUBRC IS INITIAL.
        WA_RESUMO-OBJ_LUCRO    = IT_CEPCT-PRCTR.
        WA_RESUMO-OBJ_LUCRO_TX = IT_CEPCT-LTEXT.
      ENDIF.

      LOOP AT IT_FECHAMENTO WHERE SAKNR EQ WA_RESUMO-CONTA
                              AND PRCTR EQ P_ZGLT049L.
        WA_RESUMO-VL_FECHAMENTO = WA_RESUMO-VL_FECHAMENTO + IT_FECHAMENTO-VLR_ACM.
      ENDLOOP.

      LOOP AT IT_COMPARATIVO WHERE SAKNR EQ WA_RESUMO-CONTA
                              AND PRCTR EQ P_ZGLT049L.
        WA_RESUMO-VL_COMPARATIVO = WA_RESUMO-VL_COMPARATIVO + IT_COMPARATIVO-VLR_ACM.
      ENDLOOP.

      LOOP AT IT_ULTIMO WHERE SAKNR EQ WA_RESUMO-CONTA
                              AND PRCTR EQ P_ZGLT049L.
        WA_RESUMO-VL_ULTIMO = WA_RESUMO-VL_ULTIMO + IT_ULTIMO-VLR_ACM.
      ENDLOOP.

    ENDIF.

    "Grupo de Mercadoria
    IF P_ZGLT049M NE ABAP_FALSE.
      READ TABLE IT_T023T WITH KEY MATKL = P_ZGLT049M.
      IF SY-SUBRC IS INITIAL.
        WA_RESUMO-OBJ_GRUPO    = IT_T023T-MATKL.
        WA_RESUMO-OBJ_GRUPO_TX = IT_T023T-WGBEZ60.
      ENDIF.

      LOOP AT IT_FECHAMENTO WHERE SAKNR EQ WA_RESUMO-CONTA
                              AND MATKL EQ P_ZGLT049M.
        WA_RESUMO-VL_FECHAMENTO = WA_RESUMO-VL_FECHAMENTO + IT_FECHAMENTO-VLR_ACM.
      ENDLOOP.

      LOOP AT IT_COMPARATIVO WHERE SAKNR EQ WA_RESUMO-CONTA
                              AND MATKL EQ P_ZGLT049M.
        WA_RESUMO-VL_COMPARATIVO = WA_RESUMO-VL_COMPARATIVO + IT_COMPARATIVO-VLR_ACM.
      ENDLOOP.

      LOOP AT IT_ULTIMO WHERE SAKNR EQ WA_RESUMO-CONTA
                              AND MATKL EQ P_ZGLT049M.
        WA_RESUMO-VL_ULTIMO = WA_RESUMO-VL_ULTIMO + IT_ULTIMO-VLR_ACM.
      ENDLOOP.

    ENDIF.

    P_GRAVAR = ABAP_TRUE.

  ENDIF.

ENDFORM.                    " BUSCA_SALDO_CTA_RESULTADO

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_SALDO_LUCRO_PREJU
*&---------------------------------------------------------------------*
*       Ajusta saldo de lucro e prejuizo da balanço patrimonial
*----------------------------------------------------------------------*
FORM AJUSTA_SALDO_LUCRO_PREJU.

  DATA: LC_046         TYPE ZGLT046,
        LC_ZGLT061     TYPE ZGLT061,
        LC_T001        TYPE T001,
        CK_AJUSTE      TYPE C LENGTH 1,
        CK_LUCRO       TYPE C LENGTH 1,
        CK_PREJU       TYPE C LENGTH 1,
        LC_MONAT       TYPE MONAT,
        IT_DRE_RESULT1 TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        IT_DRE_RESULT2 TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        IT_DRE_RESULT3 TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        "IT_DRE_RESULT1_B TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        "IT_DRE_RESULT2_B TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        "IT_DRE_RESULT3_B TYPE TABLE OF ZGL030_EST_D WITH HEADER LINE,
        VG_VLR_ACM_01  TYPE ZWRBTR_DRE_ACM,
        VG_VLR_ACM_02  TYPE ZWRBTR_DRE_ACM,
        VG_VLR_ACM_03  TYPE ZWRBTR_DRE_ACM,
        "VG_VLR_ACM_01_B  TYPE ZWRBTR_DRE_ACM,
        "VG_VLR_ACM_02_B  TYPE ZWRBTR_DRE_ACM,
        "VG_VLR_ACM_03_B  TYPE ZWRBTR_DRE_ACM,
        VG_VLR_LUCRO   TYPE HSLXX12,
        VG_VLR_PREJU   TYPE HSLXX12,
        REFE1          TYPE HSLXX12,
        LC_VALOR_L1    TYPE HSLXX12,
        LC_VALOR_L2    TYPE HSLXX12,
        LC_VALOR_L3    TYPE HSLXX12,
        IT_SALDOS_1    TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        IT_SALDOS_2    TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        IT_SALDOS_3    TYPE TABLE OF ZDE_FI_GL_SALDO_FAGLFLEXT WITH HEADER LINE,
        LC_CONTAS      TYPE ZCT_EMP_CONTAS.

  CK_AJUSTE = ABAP_FALSE.
  CK_AJUSTE = ABAP_FALSE.
  CK_LUCRO  = ABAP_FALSE.

  SELECT SINGLE * INTO LC_046
    FROM ZGLT046
   WHERE VERSN EQ S_VERSN.

  "Verificar se a conta de lucro ou prejuizo está entre as contas visualizadas
  CHECK LC_046-SAKNR_LUCRO    IS NOT INITIAL.
  CHECK LC_046-SAKNR_PREJUIZO IS NOT INITIAL.

  READ TABLE IT_RESUMO WITH KEY CONTA = LC_046-SAKNR_LUCRO.
  CHECK SY-SUBRC IS INITIAL.

  READ TABLE IT_SKB1 WITH KEY SAKNR = LC_046-SAKNR_LUCRO BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    CK_AJUSTE = ABAP_TRUE.
    CK_LUCRO  = ABAP_TRUE.
  ENDIF.

  READ TABLE IT_SKB1 WITH KEY SAKNR = LC_046-SAKNR_PREJUIZO BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    CK_AJUSTE = ABAP_TRUE.
    CK_PREJU  = ABAP_TRUE.
  ENDIF.

  "Ajustar Lucro/Prejuizo """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  CHECK CK_AJUSTE EQ ABAP_TRUE.

  WA_CONTAS-BUKRS = S_BUKRS-LOW.
  WA_CONTAS-SAKNR = LC_046-SAKNR_LUCRO.
  APPEND WA_CONTAS TO LC_CONTAS.

  "WA_CONTAS-BUKRS = S_BUKRS-LOW.
  "WA_CONTAS-SAKNR = LC_046-SAKNR_PREJUIZO.
  "APPEND WA_CONTAS TO LC_CONTAS.

  IF S_BUKR2 IS NOT INITIAL.
    WA_CONTAS-BUKRS = S_BUKR2.
    WA_CONTAS-SAKNR = LC_046-SAKNR_LUCRO.
    APPEND WA_CONTAS TO LC_CONTAS.
    "WA_CONTAS-BUKRS = S_BUKR2.
    "WA_CONTAS-SAKNR = LC_046-SAKNR_PREJUIZO.
    "APPEND WA_CONTAS TO LC_CONTAS.
  ENDIF.

  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      RYEAR         = LC_ANO_F
      WAERS         = S_MOEDA
      CONTAS        = LC_CONTAS
    TABLES
      IT_SALDOS     = IT_SALDOS_1
    EXCEPTIONS
      MOEDA_NAO_ADM = 1
      ERRO_LEDGER   = 2
      OTHERS        = 3.

  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      RYEAR         = LC_ANO_C
      WAERS         = S_MOEDA
      CONTAS        = LC_CONTAS
    TABLES
      IT_SALDOS     = IT_SALDOS_2
    EXCEPTIONS
      MOEDA_NAO_ADM = 1
      ERRO_LEDGER   = 2
      OTHERS        = 3.

  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      RYEAR         = LC_ANO_U
      WAERS         = S_MOEDA
      CONTAS        = LC_CONTAS
    TABLES
      IT_SALDOS     = IT_SALDOS_3
    EXCEPTIONS
      MOEDA_NAO_ADM = 1
      ERRO_LEDGER   = 2
      OTHERS        = 3.

  PERFORM MOSTRA_TEXTO USING 'Ajustanto Saldos de Lucro/Prejuizo'.

  LC_VALOR_L1 = 0.
  LC_VALOR_L2 = 0.
  LC_VALOR_L3 = 0.

  IF LC_MES_F EQ '12'.
    LOOP AT IT_SALDOS_1 WHERE RACCT EQ LC_046-SAKNR_LUCRO.
      LC_VALOR_L1 = LC_VALOR_L1 +  IT_SALDOS_1-SL16.
    ENDLOOP.
  ENDIF.

  IF LC_MES_C EQ '12'.
    LOOP AT IT_SALDOS_2 WHERE RACCT EQ LC_046-SAKNR_LUCRO.
      LC_VALOR_L2 = LC_VALOR_L2 +  IT_SALDOS_2-SL16.
    ENDLOOP.
  ENDIF.

  IF LC_MES_U EQ '12'.
    LOOP AT IT_SALDOS_3 WHERE RACCT EQ LC_046-SAKNR_LUCRO.
      LC_VALOR_L3 = LC_VALOR_L3 +  IT_SALDOS_3-SL16.
    ENDLOOP.
  ENDIF.

  "" 1º """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF LC_VALOR_L1 EQ 0.

    SELECT SINGLE *
      INTO LC_ZGLT061
      FROM ZGLT061
     WHERE BUKRS EQ S_BUKRS-LOW
       AND GJAHR EQ LC_ANO_F.

    MOVE LC_MES_F TO LC_MONAT.

    CALL FUNCTION 'Z_PESQUISA_DRE'
      EXPORTING
        I_BUKRS      = S_BUKRS-LOW
        I_GJAHR      = LC_ANO_F
        I_MONATI     = 01
        I_MONATF     = LC_MONAT
        I_VERSN      = LC_ZGLT061-VERSN
        I_WAERS      = S_MOEDA
      TABLES
        T_ZGL030_EST = IT_DRE_RESULT1.

    DELETE IT_DRE_RESULT1 WHERE SAKNR IS INITIAL.

    "Consolidado
*    IF S_BUKR2 IS NOT INITIAL.
*
*      SELECT SINGLE *
*        INTO LC_ZGLT061
*        FROM ZGLT061
*       WHERE BUKRS EQ S_BUKR2
*         AND GJAHR EQ LC_ANO_F.
*
*      CALL FUNCTION 'Z_PESQUISA_DRE'
*        EXPORTING
*          I_BUKRS      = S_BUKR2
*          I_GJAHR      = LC_ANO_F
*          I_MONATI     = 01
*          I_MONATF     = LC_MONAT
*          I_VERSN      = LC_ZGLT061-VERSN
*          I_WAERS      = S_MOEDA
*        TABLES
*          T_ZGL030_EST = IT_DRE_RESULT1_B.
*
*      DELETE IT_DRE_RESULT1_B WHERE SAKNR IS INITIAL.
*
*    ENDIF.
  ENDIF.

  "" 2º """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF LC_VALOR_L2 EQ 0.

    SELECT SINGLE *
      INTO LC_ZGLT061
      FROM ZGLT061
     WHERE BUKRS EQ S_BUKRS-LOW
       AND GJAHR EQ LC_ANO_C.

    MOVE LC_MES_C TO LC_MONAT.

    CALL FUNCTION 'Z_PESQUISA_DRE'
      EXPORTING
        I_BUKRS      = S_BUKRS-LOW
        I_GJAHR      = LC_ANO_C
        I_MONATI     = 01
        I_MONATF     = LC_MONAT
        I_VERSN      = LC_ZGLT061-VERSN
        I_WAERS      = S_MOEDA
      TABLES
        T_ZGL030_EST = IT_DRE_RESULT2.

    DELETE IT_DRE_RESULT2 WHERE SAKNR IS INITIAL.

*    "Consolidado
*    IF S_BUKR2 IS NOT INITIAL.
*
*      SELECT SINGLE *
*        INTO LC_ZGLT061
*        FROM ZGLT061
*       WHERE BUKRS EQ S_BUKR2
*         AND GJAHR EQ LC_ANO_C.
*
*      CALL FUNCTION 'Z_PESQUISA_DRE'
*        EXPORTING
*          I_BUKRS      = S_BUKR2
*          I_GJAHR      = LC_ANO_C
*          I_MONATI     = 01
*          I_MONATF     = LC_MONAT
*          I_VERSN      = LC_ZGLT061-VERSN
*          I_WAERS      = S_MOEDA
*        TABLES
*          T_ZGL030_EST = IT_DRE_RESULT2_B.
*
*      DELETE IT_DRE_RESULT2_B WHERE SAKNR IS INITIAL.
*
*    ENDIF.
  ENDIF.

  "" 3º """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF LC_VALOR_L3 EQ 0.
    SELECT SINGLE *
      INTO LC_ZGLT061
      FROM ZGLT061
     WHERE BUKRS EQ S_BUKRS-LOW
       AND GJAHR EQ LC_ANO_U.

    MOVE LC_MES_C TO LC_MONAT.

    CALL FUNCTION 'Z_PESQUISA_DRE'
      EXPORTING
        I_BUKRS      = S_BUKRS-LOW
        I_GJAHR      = LC_ANO_U
        I_MONATI     = 01
        I_MONATF     = 12
        I_VERSN      = LC_ZGLT061-VERSN
        I_WAERS      = S_MOEDA
      TABLES
        T_ZGL030_EST = IT_DRE_RESULT3.

    DELETE IT_DRE_RESULT3 WHERE SAKNR IS INITIAL.

*    "Consolidado
*    IF S_BUKR2 IS NOT INITIAL.
*      SELECT SINGLE *
*        INTO LC_ZGLT061
*        FROM ZGLT061
*       WHERE BUKRS EQ S_BUKR2
*         AND GJAHR EQ LC_ANO_U.
*
*      CALL FUNCTION 'Z_PESQUISA_DRE'
*        EXPORTING
*          I_BUKRS      = S_BUKR2
*          I_GJAHR      = LC_ANO_U
*          I_MONATI     = 01
*          I_MONATF     = 12
*          I_VERSN      = LC_ZGLT061-VERSN
*          I_WAERS      = S_MOEDA
*        TABLES
*          T_ZGL030_EST = IT_DRE_RESULT3_B.
*
*      DELETE IT_DRE_RESULT3_B WHERE SAKNR IS INITIAL.
*    ENDIF.
  ENDIF.

  VG_VLR_ACM_01   = 0.
  "VG_VLR_ACM_01_B = 0.
  VG_VLR_ACM_02   = 0.
  "VG_VLR_ACM_02_B = 0.
  VG_VLR_ACM_03   = 0.
  "VG_VLR_ACM_03_B = 0.

  LOOP AT IT_DRE_RESULT1.
    ADD IT_DRE_RESULT1-VLR_ACM TO VG_VLR_ACM_01.
  ENDLOOP.

*  LOOP AT IT_DRE_RESULT1_B.
*    ADD IT_DRE_RESULT1_B-VLR_ACM TO VG_VLR_ACM_01_B.
*  ENDLOOP.

  LOOP AT IT_DRE_RESULT2.
    ADD IT_DRE_RESULT2-VLR_ACM TO VG_VLR_ACM_02.
  ENDLOOP.

*  LOOP AT IT_DRE_RESULT2_B.
*    ADD IT_DRE_RESULT2_B-VLR_ACM TO VG_VLR_ACM_02_B.
*  ENDLOOP.

  LOOP AT IT_DRE_RESULT3.
    ADD IT_DRE_RESULT3-VLR_ACM TO VG_VLR_ACM_03.
  ENDLOOP.

*  LOOP AT IT_DRE_RESULT3_B.
*    ADD IT_DRE_RESULT3_B-VLR_ACM TO VG_VLR_ACM_03_B.
*  ENDLOOP.

  SELECT SINGLE * INTO LC_T001
    FROM T001
   WHERE BUKRS EQ S_BUKRS-LOW.

  IF ( LC_T001-WAERS EQ 'PYG' ) AND ( S_MOEDA EQ 'PYG' ).
    VG_VLR_ACM_01   = VG_VLR_ACM_01   / 100.
    "VG_VLR_ACM_01_B = VG_VLR_ACM_01_B / 100.
    VG_VLR_ACM_02   = VG_VLR_ACM_02   / 100.
    "VG_VLR_ACM_02_B = VG_VLR_ACM_02_B / 100.
    VG_VLR_ACM_03   = VG_VLR_ACM_03   / 100.
    "VG_VLR_ACM_03_B = VG_VLR_ACM_03_B / 100.
  ENDIF.

*  "Ajustando contas de Lucro e prejuizo com 16º período
*  LOOP AT LC_CONTAS INTO WA_CONTAS WHERE SAKNR EQ LC_046-SAKNR_LUCRO.
*
*    READ TABLE IT_RESUMO INTO WA_RESUMO WITH KEY EMPRESA = WA_CONTAS-BUKRS
*                                                 CONTA   = WA_CONTAS-SAKNR.
*    IF SY-SUBRC IS INITIAL.
*
**      WA_RESUMO-VL_FECHAMENTO  = 0.
**      WA_RESUMO-VL_COMPARATIVO = 0.
**      WA_RESUMO-VL_ULTIMO      = 0.
**
**      IF LC_MES_F EQ '12'.
**        LOOP AT IT_SALDOS_1 WHERE RBUKRS EQ WA_CONTAS-BUKRS
**                              AND RACCT  EQ WA_CONTAS-SAKNR.
**          WA_RESUMO-VL_FECHAMENTO = WA_RESUMO-VL_FECHAMENTO + IT_SALDOS_1-SL16.
**        ENDLOOP.
**      ENDIF.
**
**      IF LC_MES_C EQ '12'.
**        LOOP AT IT_SALDOS_2 WHERE RBUKRS EQ WA_CONTAS-BUKRS
**                              AND RACCT  EQ WA_CONTAS-SAKNR.
**          WA_RESUMO-VL_COMPARATIVO = WA_RESUMO-VL_COMPARATIVO + IT_SALDOS_2-SL16.
**        ENDLOOP.
**      ENDIF.
**
**      IF LC_MES_U EQ '12'.
**        LOOP AT IT_SALDOS_3 WHERE RBUKRS EQ WA_CONTAS-BUKRS
**                              AND RACCT  EQ WA_CONTAS-SAKNR.
**          WA_RESUMO-VL_ULTIMO = WA_RESUMO-VL_ULTIMO + IT_SALDOS_3-SL16.
**        ENDLOOP.
**      ENDIF.
*
**      COLLECT WA_RESUMO INTO IT_RESUMO.
*
*    ELSE.
*      CLEAR WA_RESUMO.
*      WA_RESUMO-CONTA          = WA_CONTAS-SAKNR.
*      WA_RESUMO-EMPRESA        = WA_CONTAS-BUKRS.
*      WA_RESUMO-VL_FECHAMENTO  = 0.
*      WA_RESUMO-VL_COMPARATIVO = 0.
*      WA_RESUMO-VL_ULTIMO      = 0.
*
*      MOVE LC_MES_F TO LC_MONAT.
*
*      LOOP AT IT_SALDOS_1 WHERE RBUKRS EQ WA_CONTAS-BUKRS
*                            AND RACCT  EQ WA_CONTAS-SAKNR.
*        IF LC_MONAT EQ '12'.
*          DO 16 TIMES
*            VARYING REFE1 FROM IT_SALDOS_1-SL01 NEXT IT_SALDOS_1-SL02.
*            WA_RESUMO-VL_FECHAMENTO = WA_RESUMO-VL_FECHAMENTO + REFE1.
*          ENDDO.
*        ELSE.
*          DO LC_MONAT TIMES
*            VARYING REFE1 FROM IT_SALDOS_1-SL01 NEXT IT_SALDOS_1-SL02.
*            WA_RESUMO-VL_FECHAMENTO = WA_RESUMO-VL_FECHAMENTO + REFE1.
*          ENDDO.
*        ENDIF.
*      ENDLOOP.
*
*      MOVE LC_MES_C TO LC_MONAT.
*
*      LOOP AT IT_SALDOS_2 WHERE RBUKRS EQ WA_CONTAS-BUKRS
*                            AND RACCT  EQ WA_CONTAS-SAKNR.
*        IF LC_MONAT EQ '12'.
*          DO 16 TIMES
*            VARYING REFE1 FROM IT_SALDOS_2-SL01 NEXT IT_SALDOS_2-SL02.
*            WA_RESUMO-VL_COMPARATIVO = WA_RESUMO-VL_COMPARATIVO + REFE1.
*          ENDDO.
*        ELSE.
*          DO LC_MONAT TIMES
*            VARYING REFE1 FROM IT_SALDOS_2-SL01 NEXT IT_SALDOS_2-SL02.
*            WA_RESUMO-VL_COMPARATIVO = WA_RESUMO-VL_COMPARATIVO + REFE1.
*          ENDDO.
*        ENDIF.
*      ENDLOOP.
*
*      MOVE LC_MES_U TO LC_MONAT.
*
*      LOOP AT IT_SALDOS_3 WHERE RBUKRS EQ WA_CONTAS-BUKRS
*                            AND RACCT  EQ WA_CONTAS-SAKNR.
*        IF LC_MONAT EQ '12'.
*          DO 16 TIMES
*            VARYING REFE1 FROM IT_SALDOS_3-SL01 NEXT IT_SALDOS_3-SL02.
*            WA_RESUMO-VL_ULTIMO = WA_RESUMO-VL_ULTIMO + REFE1.
*          ENDDO.
*        ELSE.
*          DO LC_MONAT TIMES
*            VARYING REFE1 FROM IT_SALDOS_3-SL01 NEXT IT_SALDOS_3-SL02.
*            WA_RESUMO-VL_ULTIMO = WA_RESUMO-VL_ULTIMO + REFE1.
*          ENDDO.
*        ENDIF.
*      ENDLOOP.
*
*      COLLECT WA_RESUMO INTO IT_RESUMO.
*    ENDIF.
*
*  ENDLOOP.

  "  LOOP AT IT_BUKRS_FILTRO.

  IF ( VG_VLR_ACM_01 NE 0 )." OR ( VG_VLR_ACM_01_B NE 0 ).
    VG_VLR_LUCRO = 0.
    VG_VLR_PREJU = 0.

    READ TABLE IT_RESUMO WITH KEY EMPRESA = S_BUKRS-LOW
                                  CONTA   = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      ADD IT_RESUMO-VL_FECHAMENTO TO VG_VLR_LUCRO.
    ENDIF.

    "READ TABLE IT_RESUMO WITH KEY CONTA = LC_046-SAKNR_PREJUIZO.
    "IF SY-SUBRC IS INITIAL.
    "  ADD IT_RESUMO-VL_FECHAMENTO TO VG_VLR_PREJU.
    "ENDIF.

    "IF VG_VLR_ACM_01 LT 0.
    "  "DRE Negativa é Lucro
    ADD VG_VLR_ACM_01 TO VG_VLR_LUCRO.
    "ELSE.
    "  "DRE Positiva é Prejuizo
    "  ADD VG_VLR_ACM_01 TO VG_VLR_PREJU.
    "ENDIF.

*    IF VG_VLR_ACM_01_B LT 0.
*      "DRE Negativa é Lucro
*      ADD VG_VLR_ACM_01_B TO VG_VLR_LUCRO.
*    ELSE.
*      "DRE Positiva é Prejuizo
*      ADD VG_VLR_ACM_01_B TO VG_VLR_PREJU.
*    ENDIF.

*    "Prejuizo e Lucro maior que 0 (zero)
*    IF ABS( VG_VLR_LUCRO ) GE VG_VLR_PREJU.
*      ADD VG_VLR_PREJU TO VG_VLR_LUCRO.
*      VG_VLR_PREJU = 0.
*    ELSE.
*      ADD VG_VLR_LUCRO TO VG_VLR_PREJU.
*      VG_VLR_LUCRO = 0.
*    ENDIF.

    READ TABLE IT_RESUMO WITH KEY CONTA = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      IT_RESUMO-VL_FECHAMENTO = VG_VLR_LUCRO.
      MODIFY IT_RESUMO INDEX SY-TABIX TRANSPORTING VL_FECHAMENTO.
    ENDIF.

*    READ TABLE IT_RESUMO WITH KEY CONTA = LC_046-SAKNR_PREJUIZO.
*    IF SY-SUBRC IS INITIAL.
*      IT_RESUMO-VL_FECHAMENTO = VG_VLR_PREJU.
*      MODIFY IT_RESUMO INDEX SY-TABIX TRANSPORTING VL_FECHAMENTO.
*    ENDIF.
  ENDIF.

  IF ( VG_VLR_ACM_02 NE 0 )." OR ( VG_VLR_ACM_02_B NE 0 ).

    VG_VLR_LUCRO = 0.
    VG_VLR_PREJU = 0.

    READ TABLE IT_RESUMO WITH KEY EMPRESA = S_BUKRS-LOW
                                  CONTA   = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      ADD IT_RESUMO-VL_COMPARATIVO TO VG_VLR_LUCRO.
    ENDIF.

    "READ TABLE IT_RESUMO WITH KEY CONTA = LC_046-SAKNR_PREJUIZO.
    "IF SY-SUBRC IS INITIAL.
    "  ADD IT_RESUMO-VL_COMPARATIVO TO VG_VLR_PREJU.
    "ENDIF.

    "IF VG_VLR_ACM_02 LT 0.
    "  "DRE Negativa é Lucro
    ADD VG_VLR_ACM_02 TO VG_VLR_LUCRO.
    "ELSE.
    "  "DRE Positiva é Prejuizo
    "  ADD VG_VLR_ACM_02 TO VG_VLR_PREJU.
    "ENDIF.

*    IF VG_VLR_ACM_02_B LT 0.
*      "DRE Negativa é Lucro
*      ADD VG_VLR_ACM_02_B TO VG_VLR_LUCRO.
*    ELSE.
*      "DRE Positiva é Prejuizo
*      ADD VG_VLR_ACM_02_B TO VG_VLR_PREJU.
*    ENDIF.
*
*    "Prejuizo e Lucro maior que 0 (zero)
*    IF ABS( VG_VLR_LUCRO ) GE VG_VLR_PREJU.
*      ADD VG_VLR_PREJU TO VG_VLR_LUCRO.
*      VG_VLR_PREJU = 0.
*    ELSE.
*      ADD VG_VLR_LUCRO TO VG_VLR_PREJU.
*      VG_VLR_LUCRO = 0.
*    ENDIF.

    READ TABLE IT_RESUMO WITH KEY EMPRESA = S_BUKRS-LOW
                                  CONTA = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      IT_RESUMO-VL_COMPARATIVO = VG_VLR_LUCRO.
      MODIFY IT_RESUMO INDEX SY-TABIX TRANSPORTING VL_COMPARATIVO.
    ENDIF.

*    READ TABLE IT_RESUMO WITH KEY CONTA = LC_046-SAKNR_PREJUIZO.
*    IF SY-SUBRC IS INITIAL.
*      IT_RESUMO-VL_COMPARATIVO = VG_VLR_PREJU.
*      MODIFY IT_RESUMO INDEX SY-TABIX TRANSPORTING VL_COMPARATIVO.
*    ENDIF.

  ENDIF.

  IF ( VG_VLR_ACM_03 NE 0 )." OR ( VG_VLR_ACM_03_B NE 0 ).

    VG_VLR_LUCRO = 0.
    VG_VLR_PREJU = 0.

    READ TABLE IT_RESUMO WITH KEY EMPRESA = S_BUKRS-LOW
                                  CONTA = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      ADD IT_RESUMO-VL_ULTIMO TO VG_VLR_LUCRO.
    ENDIF.

    "READ TABLE IT_RESUMO WITH KEY CONTA = LC_046-SAKNR_PREJUIZO.
    "IF SY-SUBRC IS INITIAL.
    "  ADD IT_RESUMO-VL_ULTIMO TO VG_VLR_PREJU.
    "ENDIF.

    "IF VG_VLR_ACM_03 LT 0.
    "  "DRE Negativa é Lucro
    ADD VG_VLR_ACM_03 TO VG_VLR_LUCRO.
    "ELSE.
    "  "DRE Positiva é Prejuizo
    "  ADD VG_VLR_ACM_03 TO VG_VLR_PREJU.
    "ENDIF.

*    IF VG_VLR_ACM_03_B LT 0.
*      "DRE Negativa é Lucro
*      ADD VG_VLR_ACM_03_B TO VG_VLR_LUCRO.
*    ELSE.
*      "DRE Positiva é Prejuizo
*      ADD VG_VLR_ACM_03_B TO VG_VLR_PREJU.
*    ENDIF.
*
*    "Prejuizo e Lucro maior que 0 (zero)
*    IF ABS( VG_VLR_LUCRO ) GE VG_VLR_PREJU.
*      ADD VG_VLR_PREJU TO VG_VLR_LUCRO.
*      VG_VLR_PREJU = 0.
*    ELSE.
*      ADD VG_VLR_LUCRO TO VG_VLR_PREJU.
*      VG_VLR_LUCRO = 0.
*    ENDIF.

    READ TABLE IT_RESUMO WITH KEY EMPRESA = S_BUKRS-LOW
                                  CONTA = LC_046-SAKNR_LUCRO.
    IF SY-SUBRC IS INITIAL.
      IT_RESUMO-VL_ULTIMO = VG_VLR_LUCRO.
      MODIFY IT_RESUMO INDEX SY-TABIX TRANSPORTING VL_ULTIMO.
    ENDIF.

*    READ TABLE IT_RESUMO WITH KEY CONTA = LC_046-SAKNR_PREJUIZO.
*    IF SY-SUBRC IS INITIAL.
*      IT_RESUMO-VL_ULTIMO = VG_VLR_PREJU.
*      MODIFY IT_RESUMO INDEX SY-TABIX TRANSPORTING VL_ULTIMO.
*    ENDIF.

  ENDIF.
  "  ENDLOOP.

*  IF CK_LUCRO EQ ABAP_FALSE.
*    DELETE IT_RESUMO WHERE CONTA EQ LC_046-SAKNR_LUCRO.
*  ENDIF.
*
*  IF CK_PREJU EQ ABAP_FALSE.
*    DELETE IT_RESUMO WHERE CONTA EQ LC_046-SAKNR_PREJUIZO.
*  ENDIF.


ENDFORM.                    " AJUSTA_SALDO_LUCRO_PREJU

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3334   text
*----------------------------------------------------------------------*
FORM MOSTRA_TEXTO  USING  P_TEXTO.

  DATA: VMSG(50).

  MOVE P_TEXTO TO VMSG.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = VMSG.

ENDFORM.                    " MOSTRA_TEXTO

*&---------------------------------------------------------------------*
*&      Form  LIMPA_CONS_SPARCEIRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM LIMPA_CONS_SPARCEIRA .

  DATA: IT_ZGLT060    TYPE TABLE OF ZGLT060 WITH HEADER LINE,
        IT_ZGLT057    TYPE TABLE OF ZGLT057 WITH HEADER LINE,
        WA_ZFIT0039   TYPE ZFIT0039,
        WA_ZFIT0039_B TYPE ZFIT0039.

  CLEAR: IT_ZGLT060[].

  CHECK S_BUKR2 IS NOT INITIAL.

  SELECT SINGLE * INTO WA_ZFIT0039   FROM ZFIT0039 WHERE BUKRS EQ S_BUKRS-LOW.
  SELECT SINGLE * INTO WA_ZFIT0039_B FROM ZFIT0039 WHERE BUKRS EQ S_BUKR2.

  SELECT *
    INTO TABLE IT_ZGLT060
    FROM ZGLT060
   WHERE BUKRS1 EQ S_BUKRS-LOW
     AND WAERS  EQ S_MOEDA.

  SELECT *
    APPENDING TABLE IT_ZGLT060
    FROM ZGLT060
   WHERE BUKRS1 EQ S_BUKR2
     AND WAERS  EQ S_MOEDA.

  SELECT *
    INTO TABLE IT_ZGLT057
    FROM ZGLT057
   WHERE BUKRS_1 EQ S_BUKRS-LOW
     AND BUKRS_2 EQ S_BUKR2
     AND VERSN_1 EQ WA_ZFIT0039-VERSN
     AND VERSN_2 EQ WA_ZFIT0039_B-VERSN.

  READ TABLE IT_ZGLT057 INDEX 1.
  IF SY-SUBRC IS INITIAL.

    LOOP AT IT_ZGLT060 WHERE CK_NSOC_PARCEIRA EQ ABAP_FALSE.
      DELETE IT_SALDOS_FECHAMENTO WHERE RBUKRS EQ IT_ZGLT057-BUKRS_1
                                    AND RASSC  EQ IT_ZGLT057-VBUND_1
                                    AND RACCT  EQ IT_ZGLT060-CTA_LAN.

      DELETE IT_SALDOS_FECHAMENTO WHERE RBUKRS EQ IT_ZGLT057-BUKRS_2
                                    AND RASSC  EQ IT_ZGLT057-VBUND_2
                                    AND RACCT  EQ IT_ZGLT060-CTA_LAN.

      DELETE IT_SALDOS_COMPARATIVO WHERE RBUKRS EQ IT_ZGLT057-BUKRS_1
                                     AND RASSC  EQ IT_ZGLT057-VBUND_1
                                     AND RACCT  EQ IT_ZGLT060-CTA_LAN.

      DELETE IT_SALDOS_COMPARATIVO WHERE RBUKRS EQ IT_ZGLT057-BUKRS_2
                                     AND RASSC  EQ IT_ZGLT057-VBUND_2
                                     AND RACCT  EQ IT_ZGLT060-CTA_LAN.

      DELETE IT_SALDOS_ULTIMO WHERE RBUKRS EQ IT_ZGLT057-BUKRS_1
                                AND RASSC  EQ IT_ZGLT057-VBUND_1
                                AND RACCT  EQ IT_ZGLT060-CTA_LAN.

      DELETE IT_SALDOS_ULTIMO WHERE RBUKRS EQ IT_ZGLT057-BUKRS_2
                                AND RASSC  EQ IT_ZGLT057-VBUND_2
                                AND RACCT  EQ IT_ZGLT060-CTA_LAN.
    ENDLOOP.
  ENDIF.

  LOOP AT IT_ZGLT060 WHERE CK_NSOC_PARCEIRA EQ ABAP_TRUE.

    DELETE IT_SALDOS_FECHAMENTO WHERE RBUKRS EQ IT_ZGLT060-BUKRS1
                                  AND RACCT  EQ IT_ZGLT060-CTA_LAN.

    DELETE IT_SALDOS_COMPARATIVO WHERE RBUKRS EQ IT_ZGLT060-BUKRS1
                                   AND RACCT  EQ IT_ZGLT060-CTA_LAN.

    DELETE IT_SALDOS_ULTIMO WHERE RBUKRS EQ IT_ZGLT060-BUKRS1
                              AND RACCT  EQ IT_ZGLT060-CTA_LAN.
  ENDLOOP.

ENDFORM.                    " LIMPA_CONS_SPARCEIRA
