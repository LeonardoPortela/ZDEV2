************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 02.05.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Relatório para atender contabilização internacional *
*                  (IFRS)                                              *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 12.01.2009    Marcus Barbara       Criação              DEVK905393   *
* 13.01.2009    Marcus Barbara       Alteração            DEVK905399   *
* 13.01.2009    Marcus Barbara       Alteração            DEVK905401   *
* 13.01.2009    Marcus Barbara       Alteração            DEVK905403   *
* 13.01.2009    Marcus Barbara       Alteração            DEVK905407   *
* 13.01.2009    Marcus Barbara       Alteração            DEVK905413   *
* 13.01.2009    Marcus Barbara       Alteração            DEVK905423   *
* 23.01.2009    Marcus Barbara       Alteração            DEVK905441   *
*                                                                      *
************************************************************************

REPORT  ZGL008  NO STANDARD PAGE HEADING  "Não exibe cabeçalho standard
LINE-SIZE 076               "Comprimento da Linha
LINE-COUNT 65.              "Número de Linhas.

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
TABLES: BSIS,
        SKB1,
        SKA1.
*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS,
            KKBLO.

*----------------------------------------------------------------------*
* Tabelas Internas (ALV)                                               *
*----------------------------------------------------------------------*
DATA: IT_FIELDCAT        TYPE SLIS_T_FIELDCAT_ALV,  "Estrutura de saida
      IT_EVENT           TYPE SLIS_T_EVENT  WITH HEADER LINE, "Eventos
      IT_HEADER          TYPE KKBLO_T_LISTHEADER WITH HEADER LINE, "Cabeçalho
      VG_LAYOUT          TYPE SLIS_LAYOUT_ALV.   "Layout do alv

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*

DATA: BEGIN OF WA_SKB1,
        SAKNR LIKE SKB1-SAKNR,
        MITKZ LIKE SKB1-MITKZ,
      END OF WA_SKB1,

      BEGIN OF WA_BSIS,
        BUKRS LIKE BSIS-BUKRS,
        BELNR LIKE BSIS-BELNR,
        BUZEI LIKE BSIS-BELNR,
        GJAHR LIKE BSIS-GJAHR,
        BUDAT LIKE BSIS-BUDAT,
        HKONT LIKE BSIS-HKONT,
        DMBTR LIKE BSIS-DMBTR,
        DMBE2 LIKE BSIS-DMBE2,
      END OF WA_BSIS,

      BEGIN OF WA_TCURR,
        UKURS LIKE TCURR-UKURS,
        GDATU LIKE TCURR-GDATU,
      END OF WA_TCURR,

      BEGIN OF WA_BSIK,
        BUKRS LIKE BSIK-BUKRS,
        BELNR LIKE BSIK-BELNR,
        BUZEI LIKE BSIK-BELNR,
        GJAHR LIKE BSIK-GJAHR,
        HKONT LIKE BSIK-HKONT,
        BUDAT LIKE BSIK-BUDAT,
        DMBTR LIKE BSIK-DMBTR,
        DMBE2 LIKE BSIK-DMBE2,
      END OF WA_BSIK,

      BEGIN OF WA_BSID,
        BUKRS LIKE BSID-BUKRS,
        BELNR LIKE BSID-BELNR,
        BUZEI LIKE BSID-BELNR,
        GJAHR LIKE BSID-GJAHR,
        HKONT LIKE BSID-HKONT,
        BUDAT LIKE BSID-BUDAT,
        DMBTR LIKE BSID-DMBTR,
        DMBE2 LIKE BSID-DMBE2,
      END OF WA_BSID,

      BEGIN OF WA_CONTA,
        SAKNR            LIKE SKAT-SAKNR,
        TXT50            LIKE SKAT-TXT50,
      END   OF WA_CONTA,

      BEGIN OF WA_RELT,
        BUKRS LIKE BSIS-BUKRS,
        GJAHR LIKE BSIS-GJAHR,
        HKONT LIKE BSIS-HKONT,
        TXT50 LIKE SKAT-TXT50,
        MITKZ LIKE SKB1-MITKZ,
        BUDAT LIKE BSIS-BUDAT,
        BELNR LIKE BSIS-BELNR,
        DMBTR LIKE BSIS-DMBTR,
        DMBE2 LIKE BSIS-DMBE2,
        TXHIS TYPE P LENGTH 16 DECIMALS 5,
        GDATU TYPE C LENGTH 10,
        UKURS LIKE TCURR-UKURS,
        DMBE3 LIKE BSIS-DMBTR,
      END OF WA_RELT,

      BEGIN OF WA_T001,
        BUKRS LIKE T001-BUKRS,
        BUTXT LIKE T001-BUTXT,
      END OF WA_T001.

DATA: IT_SKB1    LIKE STANDARD TABLE OF WA_SKB1,
      IT_SKB1_K  LIKE STANDARD TABLE OF WA_SKB1,
      IT_SKB1_D  LIKE STANDARD TABLE OF WA_SKB1,
      IT_CONTA   LIKE STANDARD TABLE OF WA_CONTA,
      IT_CONTA_K LIKE STANDARD TABLE OF WA_CONTA,
      IT_CONTA_D LIKE STANDARD TABLE OF WA_CONTA,
      IT_BSIS    LIKE STANDARD TABLE OF WA_BSIS,
      IT_TCURR   LIKE STANDARD TABLE OF WA_TCURR,
      IT_BSIK    LIKE STANDARD TABLE OF WA_BSIK,
      IT_BSID    LIKE STANDARD TABLE OF WA_BSID,
      IT_RELT    LIKE STANDARD TABLE OF WA_RELT.

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
SELECT-OPTIONS:
            S_BUKRS   FOR BSIS-BUKRS OBLIGATORY, "Empresa
            S_HKONT   FOR BSIS-HKONT OBLIGATORY. "Conta.
PARAMETERS:
            S_TPCTA   LIKE SKB1-MITKZ, "Tipo de conta
            S_GJAHR   LIKE BSIS-GJAHR OBLIGATORY, "Ano do periodo.
            S_MONAT   LIKE BSIS-MONAT OBLIGATORY. "Mês do periodo.
SELECTION-SCREEN END   OF BLOCK B1.

* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Selecionar os dados
  PERFORM F_SELECIONA_DADOS.
* Monta dados
  PERFORM F_MONTA_DADOS.
* Monta cabeçalho
  PERFORM F_MONTA_CAB.
* Monta ALV
  PERFORM F_MONTA_ALV.
* Executa ALV
  PERFORM F_EXECUTA_ALV.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleção de Dados
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.

* AAAAMMDD
  DATA: DATA1 TYPE DATUM,
        DATA2 TYPE DATUM,
        VG_GDATU TYPE C LENGTH 8.

  CLEAR: IT_SKB1,
         IT_SKB1_K,
         IT_SKB1_D,
         IT_BSIS,
         IT_CONTA_K,
         IT_CONTA_D,
         IT_TCURR,
         IT_BSIK,
         IT_BSID.

  CONCATENATE S_GJAHR S_MONAT '01' INTO DATA1.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = DATA1
    IMPORTING
      LAST_DAY_OF_MONTH = DATA2.

* Selecionar Taxa de Cambio Venda
  CONCATENATE DATA2+6(2) DATA2+4(2) DATA2(4) INTO VG_GDATU.
  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      INPUT  = VG_GDATU
    IMPORTING
      OUTPUT = VG_GDATU.

  SELECT SINGLE UKURS GDATU
    FROM TCURR
    INTO WA_TCURR
   WHERE KURST EQ 'B'
     AND FCURR EQ 'USD'
     AND TCURR EQ 'BRL'
     AND GDATU EQ VG_GDATU.
*//////////////////////////////////////////////

* Selecionar as contas que são tratadas como partidas em aberto
  IF S_TPCTA IS INITIAL.
    SELECT SAKNR MITKZ "#EC CI_DB_OPERATION_OK[2431747]
      FROM SKB1
      INTO TABLE IT_SKB1
     WHERE BUKRS IN S_BUKRS
       AND XOPVW EQ 'X'.
  ENDIF.

  SELECT SAKNR MITKZ "#EC CI_DB_OPERATION_OK[2431747]
    FROM SKB1
    INTO TABLE IT_SKB1_K
   WHERE BUKRS IN S_BUKRS
     AND MITKZ EQ 'K'.

  SELECT SAKNR MITKZ "#EC CI_DB_OPERATION_OK[2431747]
    FROM SKB1
    INTO TABLE IT_SKB1_D
   WHERE BUKRS IN S_BUKRS
     AND MITKZ EQ 'D'.
*//////////////////////////////////////////////

* Selecionar contas razões
  IF IT_SKB1 IS NOT INITIAL.
    SELECT SAKNR TXT50
    FROM SKAT
    INTO TABLE IT_CONTA
     FOR ALL ENTRIES IN IT_SKB1
   WHERE SAKNR EQ IT_SKB1-SAKNR
     AND KTOPL EQ '0050'
     AND SPRAS EQ 'PT'.
  ENDIF.

  IF IT_SKB1_K IS NOT INITIAL.
    SELECT SAKNR TXT50
      FROM SKAT
      INTO TABLE IT_CONTA_K
       FOR ALL ENTRIES IN IT_SKB1_K
     WHERE SAKNR EQ IT_SKB1_K-SAKNR
       AND KTOPL EQ '0050'
       AND SPRAS EQ 'PT'.
  ENDIF.

  IF IT_SKB1_D IS NOT INITIAL.
    SELECT SAKNR TXT50
      FROM SKAT
      INTO TABLE IT_CONTA_D
       FOR ALL ENTRIES IN IT_SKB1_D
     WHERE SAKNR EQ IT_SKB1_D-SAKNR
       AND KTOPL EQ '0050'
       AND SPRAS EQ 'PT'.
  ENDIF.
*//////////////////////////////////////////////

* Selecionar partidas em aberto CONTA RAZÃO
  IF IT_SKB1 IS NOT INITIAL.
    SELECT BUKRS BELNR BUZEI GJAHR BUDAT HKONT DMBTR DMBE2
      FROM BSIS
      INTO TABLE IT_BSIS
      FOR ALL ENTRIES IN IT_SKB1
     WHERE BUKRS IN S_BUKRS
       AND HKONT EQ IT_SKB1-SAKNR
       AND HKONT IN S_HKONT
       AND BUDAT LE DATA2
       AND WAERS EQ 'BRL'.
  ENDIF.
*//////////////////////////////////////////////

* Selecionar partidas em aberto FORNECEDOR / CLIENTE
  IF S_TPCTA IS INITIAL.
    IF IT_SKB1_K IS NOT INITIAL.
      SELECT BUKRS BELNR BUZEI GJAHR HKONT BUDAT DMBTR DMBE2
        FROM BSIK
        INTO TABLE IT_BSIK
        FOR ALL ENTRIES IN IT_SKB1_K
       WHERE BUKRS IN S_BUKRS
         AND HKONT EQ IT_SKB1_K-SAKNR
         AND HKONT IN S_HKONT
         AND BUDAT LE DATA2
         AND WAERS EQ 'BRL'.
    ENDIF.

    IF IT_SKB1_D IS NOT INITIAL.
      SELECT BUKRS BELNR BUZEI GJAHR HKONT BUDAT DMBTR DMBE2
        FROM BSID
        INTO TABLE IT_BSID
        FOR ALL ENTRIES IN IT_SKB1_D
       WHERE BUKRS IN S_BUKRS
         AND HKONT EQ IT_SKB1_D-SAKNR
         AND HKONT IN S_HKONT
         AND BUDAT LE DATA2
         AND WAERS EQ 'BRL'.
    ENDIF.
  ELSE.

    IF ( S_TPCTA EQ 'K' ) AND ( IT_SKB1_K IS NOT INITIAL ).

      SELECT BUKRS BELNR BUZEI GJAHR HKONT BUDAT DMBTR DMBE2
        FROM BSIK
        INTO TABLE IT_BSIK
        FOR ALL ENTRIES IN IT_SKB1_K
       WHERE BUKRS IN S_BUKRS
         AND HKONT EQ IT_SKB1_K-SAKNR
         AND HKONT IN S_HKONT
         AND BUDAT LE DATA2
         AND WAERS EQ 'BRL'.

    ELSEIF ( S_TPCTA EQ 'D' ) AND ( IT_SKB1_D IS NOT INITIAL ).

      SELECT BUKRS BELNR BUZEI GJAHR HKONT BUDAT DMBTR DMBE2
        FROM BSID
        INTO TABLE IT_BSID
        FOR ALL ENTRIES IN IT_SKB1_D
       WHERE BUKRS IN S_BUKRS
         AND HKONT EQ IT_SKB1_D-SAKNR
         AND HKONT IN S_HKONT
         AND BUDAT LE DATA2
         AND WAERS EQ 'BRL'.

    ENDIF.
  ENDIF.
*//////////////////////////////////////////////

ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS
*&---------------------------------------------------------------------*
*       Montagem de dados para apresentação do relatório.
*----------------------------------------------------------------------*
FORM F_MONTA_DADOS .

  DATA: VL_DATA_COTACAO TYPE C LENGTH 10.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
    EXPORTING
      INPUT  = WA_TCURR-GDATU
    IMPORTING
      OUTPUT = VL_DATA_COTACAO.

  LOOP AT IT_BSIS INTO WA_BSIS.
    CLEAR: WA_RELT.
    WA_RELT-BUKRS = WA_BSIS-BUKRS.
    WA_RELT-HKONT = WA_BSIS-HKONT+4(6).
    READ TABLE IT_CONTA WITH KEY SAKNR = WA_BSIS-HKONT INTO WA_CONTA.
    IF SY-SUBRC EQ 0.
      WA_RELT-TXT50 = WA_CONTA-TXT50.
    ENDIF.
    WA_RELT-BUDAT = WA_BSIS-BUDAT.
    WA_RELT-BELNR = WA_BSIS-BELNR.
    WA_RELT-DMBTR = WA_BSIS-DMBTR.
    WA_RELT-DMBE2 = WA_BSIS-DMBE2.
    IF ( WA_BSIS-DMBE2 NE 0 ) AND
       ( WA_BSIS-DMBE2 IS NOT INITIAL ).
      WA_RELT-TXHIS = WA_BSIS-DMBTR / WA_BSIS-DMBE2.
    ELSE.
      WA_RELT-TXHIS = 0.
    ENDIF.
    WA_RELT-GDATU = VL_DATA_COTACAO.
    WA_RELT-UKURS = WA_TCURR-UKURS.
    IF ( WA_TCURR-UKURS NE 0 ) AND
       ( WA_TCURR-UKURS IS NOT INITIAL ).
      WA_RELT-DMBE3 = ( WA_BSIS-DMBTR / WA_TCURR-UKURS ) - WA_BSIS-DMBE2.
    ELSE.
      WA_RELT-DMBE3 = 0.
    ENDIF.
    APPEND WA_RELT TO IT_RELT.
  ENDLOOP.

  LOOP AT IT_BSIK INTO WA_BSIK.
    CLEAR: WA_RELT.
    WA_RELT-BUKRS = WA_BSIK-BUKRS.
    WA_RELT-HKONT = WA_BSIK-HKONT+4(6).
    READ TABLE IT_CONTA_K WITH KEY SAKNR = WA_BSIK-HKONT INTO WA_CONTA.
    IF SY-SUBRC EQ 0.
      WA_RELT-TXT50 = WA_CONTA-TXT50.
    ENDIF.
    WA_RELT-MITKZ = 'K'.
    WA_RELT-BUDAT = WA_BSIK-BUDAT.
    WA_RELT-BELNR = WA_BSIK-BELNR.
    WA_RELT-DMBTR = WA_BSIK-DMBTR.
    WA_RELT-DMBE2 = WA_BSIK-DMBE2.
    IF ( WA_BSIK-DMBE2 NE 0 ) AND
       ( WA_BSIK-DMBE2 IS NOT INITIAL ).
      WA_RELT-TXHIS = WA_BSIK-DMBTR / WA_BSIK-DMBE2.
    ELSE.
      WA_RELT-TXHIS = 0.
    ENDIF.
    WA_RELT-GDATU = VL_DATA_COTACAO.
    WA_RELT-UKURS = WA_TCURR-UKURS.
    IF ( WA_TCURR-UKURS NE 0 ) AND
       ( WA_TCURR-UKURS IS NOT INITIAL ).
      WA_RELT-DMBE3 = ( WA_BSIK-DMBTR / WA_TCURR-UKURS ) - WA_BSIK-DMBE2.
    ELSE.
      WA_RELT-DMBE3 = 0.
    ENDIF.
    APPEND WA_RELT TO IT_RELT.
  ENDLOOP.

  LOOP AT IT_BSID INTO WA_BSID.
    CLEAR: WA_RELT.
    WA_RELT-BUKRS = WA_BSID-BUKRS.
    WA_RELT-HKONT = WA_BSID-HKONT+4(6).
    READ TABLE IT_CONTA_D WITH KEY SAKNR = WA_BSID-HKONT INTO WA_CONTA.
    IF SY-SUBRC EQ 0.
      WA_RELT-TXT50 = WA_CONTA-TXT50.
    ENDIF.
    WA_RELT-MITKZ = 'D'.
    WA_RELT-BUDAT = WA_BSID-BUDAT.
    WA_RELT-BELNR = WA_BSID-BELNR.
    WA_RELT-DMBTR = WA_BSID-DMBTR.
    WA_RELT-DMBE2 = WA_BSID-DMBE2.
    IF ( WA_BSID-DMBE2 NE 0 ) AND
       ( WA_BSID-DMBE2 IS NOT INITIAL ).
      WA_RELT-TXHIS = WA_BSID-DMBTR / WA_BSID-DMBE2.
    ELSE.
      WA_RELT-TXHIS = 0.
    ENDIF.
    WA_RELT-GDATU = VL_DATA_COTACAO.
    WA_RELT-UKURS = WA_TCURR-UKURS.
    IF ( WA_TCURR-UKURS NE 0 ) AND
       ( WA_TCURR-UKURS IS NOT INITIAL ).
      WA_RELT-DMBE3 = ( WA_BSID-DMBTR / WA_TCURR-UKURS ) - WA_BSID-DMBE2.
    ELSE.
      WA_RELT-DMBE3 = 0.
    ENDIF.
    APPEND WA_RELT TO IT_RELT.
  ENDLOOP.

ENDFORM.                    " F_MONTA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_CAB
*&---------------------------------------------------------------------*
*       Gera o cabeçalho do relatório
*----------------------------------------------------------------------*
FORM F_MONTA_CAB .

  CLEAR IT_HEADER.

  DATA: VL_DATA_ATUAL    TYPE C LENGTH 14,
        VL_HORA          TYPE C LENGTH 8,
        VL_DATA_COTACAO  TYPE C LENGTH 10,
        HORA_UZEIT       TYPE UZEIT.

* Titulo - Gain or Loss
  IT_HEADER-TYP  = 'H'.
  IT_HEADER-INFO = 'Gain or Loss'.
  APPEND  IT_HEADER.

  SELECT BUKRS BUTXT
    FROM T001
    INTO WA_T001
   WHERE BUKRS IN S_BUKRS.
    IT_HEADER-TYP  = 'S'.
    IT_HEADER-KEY  = 'Empresa'.
    CONCATENATE WA_T001-BUKRS '-' WA_T001-BUTXT INTO IT_HEADER-INFO.
    APPEND IT_HEADER.
  ENDSELECT.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Período'.
  CONCATENATE S_MONAT '/' S_GJAHR INTO IT_HEADER-INFO.
  APPEND IT_HEADER.

  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Cotação'.

  CALL FUNCTION 'CONVERSION_EXIT_EXCRT_OUTPUT'
    EXPORTING
      INPUT  = WA_TCURR-UKURS
    IMPORTING
      OUTPUT = IT_HEADER-INFO.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
    EXPORTING
      INPUT  = WA_TCURR-GDATU
    IMPORTING
      OUTPUT = VL_DATA_COTACAO.

  CONCATENATE VL_DATA_COTACAO '-' IT_HEADER-INFO INTO IT_HEADER-INFO.

  APPEND IT_HEADER.

* Data/Hora
  CONCATENATE SY-DATUM+6(2)
              '.' SY-DATUM+4(2)
              '.' SY-DATUM(4) INTO VL_DATA_ATUAL.
  HORA_UZEIT = SY-UZEIT.
  CONCATENATE HORA_UZEIT(2)
              ':' HORA_UZEIT+2(2)
              ':' HORA_UZEIT+4(2) INTO VL_HORA.
  IT_HEADER-TYP  = 'S'.
  IT_HEADER-KEY  = 'Data'.
  CONCATENATE VL_DATA_ATUAL ' - ' VL_HORA INTO IT_HEADER-INFO.
  APPEND IT_HEADER.

ENDFORM.                    " F_MONTA_CAB

*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_FIELDCAT  USING P_CONT P_KEY  P_TAB  P_FIELD P_DESC
                      P_TAM  P_QTDE P_FIX  P_JUST P_HOT
             CHANGING P_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

* Tabela interna local
  DATA: TL_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

  TL_FIELDCAT-COL_POS    = P_CONT. "Posição
  TL_FIELDCAT-KEY        = P_KEY.  "
  TL_FIELDCAT-TABNAME    = P_TAB.  "Tabela interna
  TL_FIELDCAT-FIELDNAME  = P_FIELD."Campo
  TL_FIELDCAT-SELTEXT_L  = P_DESC. "Descrição longa
  TL_FIELDCAT-SELTEXT_M  = P_DESC. "Descrição media
  TL_FIELDCAT-SELTEXT_S  = P_DESC. "Descrição pequena
  TL_FIELDCAT-OUTPUTLEN  = P_TAM.  "Tamanho
  TL_FIELDCAT-QUANTITY   = P_QTDE. "Campo quantidade
  TL_FIELDCAT-FIX_COLUMN = P_FIX.  "Fixar coluna
  TL_FIELDCAT-JUST       = P_JUST. "Alinhar
  TL_FIELDCAT-HOTSPOT    = P_HOT.  "Clique chama evento
  APPEND TL_FIELDCAT TO P_FIELDCAT.

ENDFORM.                    " F_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ALV
*&---------------------------------------------------------------------*
*       Monta estrutura de apresentação da ALV
*----------------------------------------------------------------------*
FORM F_MONTA_ALV .

  CLEAR: IT_FIELDCAT.

  PERFORM F_FIELDCAT USING:
        '00' '' 'IT_RELT' 'HKONT' 'Conta'
        06  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '01' '' 'IT_RELT' 'TXT50' 'Descrição'
        25  ''  ''             '' '' CHANGING IT_FIELDCAT,
        '02' '' 'IT_RELT' 'MITKZ' 'Tp.Conta'
        08  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '03' '' 'IT_RELT' 'BUDAT' 'Data Lcto'
        10  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '04' '' 'IT_RELT' 'BELNR' 'Nro.Dcto'
        10  ''  ''             '' 'X'  CHANGING IT_FIELDCAT,
        '05' '' 'IT_RELT' 'DMBTR' 'Valor R$'
        10  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '06' '' 'IT_RELT' 'DMBE2' 'Valor US$'
        10  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '07' '' 'IT_RELT' 'TXHIS' 'Tx.Hist.'
        10  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '08' '' 'IT_RELT' 'GDATU' 'Dt.Cotação'
        10  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '09' '' 'IT_RELT' 'UKURS' 'Tx.Fech.'
        08  ''  ''             '' ''  CHANGING IT_FIELDCAT,
        '10' '' 'IT_RELT' 'DMBE3' 'Gain or Loss (USD)'
        18  ''  ''             '' ''  CHANGING IT_FIELDCAT.

ENDFORM.                    " F_MONTA_ALV

*&---------------------------------------------------------------------*
*&      Form  top_of_page                                              *
*&---------------------------------------------------------------------*
*      Chama o cabeçalho da ALV                                        *
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
* Cabeçalho
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER[].
ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  F_EXECUTA_ALV
*&---------------------------------------------------------------------*
*       Executa ALV
*----------------------------------------------------------------------*
FORM F_EXECUTA_ALV .

* Variavel Local
  DATA: VL_REPID LIKE SY-REPID.

  VL_REPID = SY-REPID.

  IT_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  IT_EVENT-FORM = SLIS_EV_TOP_OF_PAGE.
  APPEND IT_EVENT.

* Determinar a tabela de cores
  VG_LAYOUT-ZEBRA = 'X'.

* Função para exibir o ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    I_CALLBACK_PROGRAM       = VL_REPID
*    I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
    I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
    I_CALLBACK_TOP_OF_PAGE   = 'TOP_OF_PAGE'
    IS_LAYOUT                = VG_LAYOUT
*    i_background_id          = c_enjoy
    IT_FIELDCAT              = IT_FIELDCAT[]
    I_DEFAULT                = 'A'
    I_SAVE                   = 'A'
  TABLES
    T_OUTTAB                 = IT_RELT
  EXCEPTIONS
    PROGRAM_ERROR            = 1
    OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_EXECUTA_ALV

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Ao clicar na estrutura vai listar todos os niveis
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING P_UCOMM LIKE SY-UCOMM
      P_FIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_RELT INTO WA_RELT INDEX P_FIELD-TABINDEX.

  IF P_FIELD-FIELDNAME = 'BELNR'.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'FB03'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'FB03'.
    ENDIF.

    SET PARAMETER ID 'BLN' FIELD WA_RELT-BELNR.
    SET PARAMETER ID 'BUK' FIELD WA_RELT-BUKRS.
    SET PARAMETER ID 'GJR' FIELD WA_RELT-GJAHR.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    "user_command
