************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 28.05.2008                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: Relatório de Notas fiscais                          *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 28.05.2008    Michely              Criação              DEVK904103   *
* 20.08.2008    Marcus Barbara       Alteração            DEVK904712   *
* 23.09.2008    Marcus Barbara       Alteração            DEVK904942   *
* 23.09.2008    Marcus Barbara       Alteração            DEVK904944   *
* 23.09.2008    Marcus Barbara       Alteração            DEVK904946   *
* 10.01.2012    Camila Brand         Alteração            DEVK920139   *
************************************************************************

REPORT ZFIS008 NO STANDARD PAGE HEADING    "Não exibe cabeçalho standard
               MESSAGE-ID Z01
               LINE-SIZE 076               "Comprimento da Linha
               LINE-COUNT 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
TABLES: J_1BNFDOC, J_1BNFLIN, J_1BNFSTX, EKKN.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS,
            KKBLO.

*----------------------------------------------------------------------*
* Tabelas Internas (ALV)                                               *
*----------------------------------------------------------------------*
DATA: IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,                   "Estrutura de saida
      IT_EVENT    TYPE SLIS_T_EVENT       WITH HEADER LINE,   "Eventos
      IT_HEADER   TYPE KKBLO_T_LISTHEADER WITH HEADER LINE,   "Cabeçalho
      WA_FIELDCAT LIKE LINE OF IT_FIELDCAT,
      VG_LAYOUT   TYPE SLIS_LAYOUT_ALV.   "Layout do alv

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
DATA: BEGIN OF WA_DOC,
        DOCNUM_DOC LIKE J_1BNFDOC-DOCNUM, "Nº documento
        NFTYPE     LIKE J_1BNFDOC-NFTYPE, "Ctg.de nota fiscal
        DOCTYP     LIKE J_1BNFDOC-DOCTYP, "Tipo de documento
        DOCDAT     LIKE J_1BNFDOC-DOCDAT, "Data do documento
        PSTDAT     LIKE J_1BNFDOC-PSTDAT, "Data de lançamento
        CREDAT     LIKE J_1BNFDOC-CREDAT, "Criar data
        CRETIM     LIKE J_1BNFDOC-CRETIM, "Hora da criação
        CRENAM     LIKE J_1BNFDOC-CRENAM, "Nome do usuário
        CHADAT     LIKE J_1BNFDOC-CHADAT, "Data de modificação
        CHATIM     LIKE J_1BNFDOC-CHATIM, "Hora de modificação
        CHANAM     LIKE J_1BNFDOC-CHANAM, "Nome do usuário
        SERIES     LIKE J_1BNFDOC-SERIES, "Série
        SUBSER     LIKE J_1BNFDOC-SUBSER, "Subséries
        NFNUM	     LIKE J_1BNFDOC-NFNUM,  "Nº nota fiscal
        NFE        LIKE J_1BNFDOC-NFE,    "Nota Fiscal eletrônica "CSB
        NFENUM     LIKE J_1BNFDOC-NFENUM, "Nº NF-e de nove posições "CSB
        ENTRAD     LIKE J_1BNFDOC-ENTRAD, "Nota fiscal entrada
        FATURA     LIKE J_1BNFDOC-FATURA, "Fatura
        PRINTD     LIKE J_1BNFDOC-PRINTD, "Imprimida
        MANUAL     LIKE J_1BNFDOC-MANUAL, "Criad.manualmente
        BELNR      LIKE J_1BNFDOC-BELNR,  "Nº documento contábil
        GJAHR      LIKE J_1BNFDOC-GJAHR,  "Exercício
        BUKRS      LIKE J_1BNFDOC-BUKRS,  "Empresa
        BRANCH     LIKE J_1BNFDOC-BRANCH, "Local de negócios
        PARVW      LIKE J_1BNFDOC-PARVW,  "Nota fiscal função parceiro
        PARID      LIKE J_1BNFDOC-PARID,  "Identificação do parceiro
        PARTYP     LIKE J_1BNFDOC-PARTYP, "Tipo de parceiro nota fiscal
        CANCEL     LIKE J_1BNFDOC-CANCEL, "Estornado
        CANDAT     LIKE J_1BNFDOC-CANDAT, "Data de estorno
        DOCREF_DOC LIKE J_1BNFDOC-DOCREF, "Referência a nota fiscal
        OBSERVAT   LIKE J_1BNFDOC-OBSERVAT, "Observação
        VSTEL      LIKE J_1BNFDOC-VSTEL,  "Local de recebimento de mercadoria
        NFDEC      LIKE J_1BNFDOC-NFDEC,  "Decimais do preço incl. ICMS
        NFTOT      LIKE J_1BNFDOC-NFTOT,  "Valor total incluindo todos os impostos
        TOTIMP     LIKE J_1BNFDOC-NFTOT,  "Valor total com impostos
      END   OF WA_DOC,

      BEGIN OF WA_STX,
        DOCNUM_STX LIKE J_1BNFSTX-DOCNUM,
        ITMNUM_STX LIKE J_1BNFSTX-ITMNUM,
        TAXTYP     LIKE J_1BNFSTX-TAXTYP, "Tipo de imposto
        TAXGRP     LIKE J_1BAJ-TAXGRP, "Grupo de imposto
        BASE       LIKE J_1BNFSTX-BASE,   "Montante básico
        RATE       LIKE J_1BNFSTX-RATE,   "Taxa de imposto
        TAXVAL     LIKE J_1BNFSTX-TAXVAL, "Valor fiscal
        EXCBAS     LIKE J_1BNFSTX-EXCBAS, "Montante básico excluído
        OTHBAS     LIKE J_1BNFSTX-OTHBAS, "Outro montante básico
      END   OF WA_STX,

      BEGIN OF WA_LFA1,
        PARID_P LIKE LFA1-LIFNR,
        NAME1   LIKE LFA1-NAME1,   "NOME
        STCD1	  LIKE LFA1-STCD1, "CNPJ
        STCD2	  LIKE LFA1-STCD2, "CPF
        STCD3	  LIKE LFA1-STCD3, "INSCR.\RG
        STKZN   LIKE LFA1-STKZN,
      END   OF WA_LFA1,

      BEGIN OF WA_KNA1,
        PARID_P LIKE KNA1-KUNNR,
        NAME1   LIKE KNA1-NAME1,   "NOME
        STCD1	  LIKE KNA1-STCD1, "CNPJ
        STCD2	  LIKE KNA1-STCD2, "CPF
        STCD3	  LIKE KNA1-STCD3, "INSCR.\RG
        STKZN   LIKE KNA1-STKZN,
      END   OF WA_KNA1,

      BEGIN OF WA_RSEG,
        BELNR LIKE RSEG-BELNR,
        GJAHR LIKE RSEG-GJAHR,
        EBELN LIKE RSEG-EBELN,
        EBELP LIKE RSEG-EBELP,
      END   OF WA_RSEG,

      BEGIN OF WA_EKKN,
        EBELN LIKE EKKN-EBELN,
        EBELP LIKE EKKN-EBELP,
        AUFNR LIKE EKKN-AUFNR,
        ANLN1 LIKE EKKN-ANLN1,
        ANLN2 LIKE EKKN-ANLN2,
      END   OF WA_EKKN,

      BEGIN OF WA_T001,
        BUKRS LIKE T001-BUKRS,
        BUTXT LIKE T001-BUTXT,
      END   OF WA_T001,

      BEGIN OF WA_BAA OCCURS 0,
        NFTYPE LIKE J_1BAA-NFTYPE,
      END OF WA_BAA.

TYPES: BEGIN OF TY_RBKP,
         ZUONR  TYPE RBKP-ZUONR,
         BELNR  TYPE RBKP-BELNR,
         BUKRS  TYPE RBKP-BUKRS,
         GSBER  TYPE RBKP-GSBER,
         BLDAT  TYPE RBKP-BLDAT,
         BUDAT  TYPE RBKP-BUDAT,
         XBLNR  TYPE RBKP-XBLNR,
         RMWWR  TYPE RBKP-RMWWR,
         LIFNR  TYPE RBKP-LIFNR,
         USNAM  TYPE RBKP-USNAM,
         MWSKZ1 TYPE RBKP-MWSKZ1,
         ZBD1T  TYPE RBKP-ZBD1T,
         ZFBDT  TYPE RBKP-ZFBDT,
         CPUDT  TYPE RBKP-CPUDT,
         CPUTM  TYPE RBKP-CPUTM,
         WAERS  TYPE RBKP-WAERS,
         WMWST1 TYPE RBKP-WMWST1,
         GJAHR  TYPE RBKP-GJAHR,
         BLART  TYPE RBKP-BLART,
         AWKEY  TYPE BKPF-AWKEY,
       END   OF TY_RBKP,

       BEGIN OF TY_BKPF,
         BUKRS TYPE BKPF-BUKRS,
         GJAHR TYPE BKPF-GJAHR,
         BLART TYPE BKPF-BLART,
         BUDAT TYPE BKPF-BUDAT,
         AWKEY TYPE BKPF-AWKEY,
         BELNR TYPE BKPF-BELNR,
       END OF TY_BKPF,

       BEGIN OF TY_BSIS,
         BUKRS TYPE BSIS-BUKRS,
         BELNR TYPE BSIS-BELNR,
         GJAHR TYPE BSIS-GJAHR,
         HKONT TYPE BSIS-HKONT,
         DMBTR TYPE BSIS-DMBTR,
       END OF TY_BSIS,

       BEGIN OF TY_LFA1_AUX,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
       END   OF TY_LFA1_AUX,

       BEGIN OF TY_SAIDA,
         ZUONR  TYPE RBKP-ZUONR,
         BELNR  TYPE RBKP-BELNR,
*        BUKRS      TYPE RBKP-BUKRS,
*        GSBER      TYPE RBKP-GSBER,
         BLDAT  TYPE RBKP-BLDAT,
         BUDAT  TYPE RBKP-BUDAT,
         XBLNR  TYPE RBKP-XBLNR,
         RMWWR  TYPE RBKP-RMWWR,
         LIFNR  TYPE RBKP-LIFNR,
         USNAM  TYPE RBKP-USNAM,
         MWSKZ1 TYPE RBKP-MWSKZ1,
*        ZBD1T      TYPE RBKP-ZBD1T,
*        ZFBDT      TYPE RBKP-ZFBDT,
         CPUDT  TYPE RBKP-CPUDT,
         CPUTM  TYPE RBKP-CPUTM,
*        WAERS      TYPE RBKP-WAERS,
         WMWST1 TYPE RBKP-WMWST1,
*        GJAHR      TYPE RBKP-GJAHR,
*        BLART      TYPE RBKP-BLART,
*        AWKEY      TYPE BKPF-AWKEY,
*        HKONT      TYPE BSIS-HKONT,
         PIS    TYPE BSIS-DMBTR,
         COFINS TYPE BSIS-DMBTR,
         NAME1  TYPE LFA1-NAME1,
       END OF TY_SAIDA.

DATA: BEGIN OF WA_LIN.
        INCLUDE STRUCTURE J_1BNFLIN.
        DATA:   EBELN TYPE EKKN-EBELN,
               EBELP TYPE EKKN-EBELP.
DATA: END   OF WA_LIN.

DATA: BEGIN OF WA_RELATORIO,

*        EBELN LIKE EKKN-EBELN,
*        EBELP LIKE EKKN-EBELP,
        ANLN1 LIKE EKKN-ANLN1,
        ANLN2 LIKE EKKN-ANLN2.

        INCLUDE STRUCTURE WA_DOC.
        INCLUDE STRUCTURE WA_LIN.
        INCLUDE STRUCTURE WA_STX.
        INCLUDE STRUCTURE WA_LFA1.


      DATA: END   OF WA_RELATORIO.

DATA: IT_DOC       LIKE STANDARD TABLE OF WA_DOC,
      IT_LIN       LIKE STANDARD TABLE OF WA_LIN,
      IT_STX       LIKE STANDARD TABLE OF WA_STX,
      IT_LFA1      LIKE STANDARD TABLE OF WA_LFA1,
      IT_KNA1      LIKE STANDARD TABLE OF WA_KNA1,
      IT_RSEG      LIKE STANDARD TABLE OF WA_RSEG,
      IT_EKKN      LIKE STANDARD TABLE OF WA_EKKN,
      IT_RELATORIO LIKE STANDARD TABLE OF WA_RELATORIO,
      IT_BAA       LIKE STANDARD TABLE OF WA_BAA,
      IT_T001      LIKE STANDARD TABLE OF WA_T001.

*&---------------------------------------------------------------------*
*& Field-symbol
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <FS_LIN> LIKE LINE OF IT_LIN.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: T_RBKP     TYPE TABLE OF TY_RBKP,
      T_BKPF     TYPE TABLE OF TY_BKPF,
      T_BSIS     TYPE TABLE OF TY_BSIS,
      T_LFA1_AUX TYPE TABLE OF TY_LFA1_AUX,
      T_SAIDA    TYPE TABLE OF TY_SAIDA.
*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA: WA_RBKP     TYPE     TY_RBKP,
      WA_BKPF     TYPE     TY_BKPF,
      WA_BSIS     TYPE     TY_BSIS,
      WA_LFA1_AUX TYPE TY_LFA1_AUX,
      WA_SAIDA    TYPE    TY_SAIDA.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: C_TABELA     TYPE C LENGTH 12 VALUE 'IT_RELATORIO',
           C_HKONT1(10) TYPE C           VALUE '0000113214',
           C_HKONT2(10) TYPE C           VALUE '0000113216'.

*----------------------------------------------------------------------*
* Variaveis                                                           *
*----------------------------------------------------------------------*
DATA: V_INDEX TYPE SY-TABIX.

*----------------------------------------------------------------------*
* Definição de Macro                                                   *
*----------------------------------------------------------------------*
* Preenche a tabela fieldcat
DEFINE FIELDCAT.
  clear wa_fieldcat.
*  wa_fieldcat-col_pos    = &1. "Posição do campo
  wa_fieldcat-key        = &1. "Campo chave
  wa_fieldcat-tabname    = &2. "Tabela interna
  wa_fieldcat-fieldname  = &3. "Campo da tabela interna
*  wa_fieldcat-outputlen  = &5. "Tamanho do campo de saída
  wa_fieldcat-fix_column = &4. "Congelar a coluna
*  wa_fieldcat-just       = &6. "Alinhamento (R)ight (L)eft (C)ent
  wa_fieldcat-hotspot    = &5. "Cria link no campo (X)
  wa_fieldcat-seltext_s  = &6. "Descrição grande
  wa_fieldcat-seltext_m  = &7. "Descrição media
  wa_fieldcat-seltext_l  = &8. "Descrição pequena

  if wa_fieldcat-fieldname = 'BELNR'.
    wa_fieldcat-hotspot = 'X'.
  else.
    clear wa_fieldcat-hotspot.
  endif.

  append wa_fieldcat to it_fieldcat.

END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-S00.

SELECT-OPTIONS:
           S_BUKRS    FOR J_1BNFDOC-BUKRS OBLIGATORY,   "campo obrigatorio
           S_BRANCH   FOR J_1BNFDOC-BRANCH  OBLIGATORY,  "campo obrigatorio
           S_PSTDAT   FOR J_1BNFDOC-PSTDAT  OBLIGATORY,  "campo obrigatorio
           S_PARID    FOR J_1BNFDOC-PARID,
           S_DOCDAT   FOR J_1BNFDOC-DOCDAT,
           S_CFOP     FOR J_1BNFLIN-CFOP,
           S_MATNR    FOR J_1BNFLIN-MATNR,
           S_NBM      FOR J_1BNFLIN-NBM,
           S_NETPR    FOR J_1BNFLIN-NETPR,
           S_NETWRT   FOR J_1BNFLIN-NETWR,
           S_EXCBAS   FOR J_1BNFSTX-EXCBAS,
           S_MANUAL   FOR J_1BNFDOC-MANUAL,
           S_EBELN    FOR EKKN-EBELN,
           S_ANLN1    FOR EKKN-ANLN1,
           S_AUFNR    FOR EKKN-AUFNR,
           S_GSBER    FOR WA_RBKP-GSBER.
SELECTION-SCREEN END   OF BLOCK B0.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S10.
PARAMETER:
        R_GERAL        RADIOBUTTON GROUP TP DEFAULT 'X',
        R_SERV         RADIOBUTTON GROUP TP,
        R_MATE         RADIOBUTTON GROUP TP.
SELECTION-SCREEN END   OF BLOCK B1.


SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S20.
PARAMETERS: P_TODAS RADIOBUTTON GROUP ES DEFAULT 'X',
            P_SAIDA RADIOBUTTON GROUP ES,
            P_ENTRA RADIOBUTTON GROUP ES.
SELECTION-SCREEN END   OF BLOCK B3.

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR 'INI'.

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_SELECIONA_DADOS.
  PERFORM F_MONTA_DADOS.
  PERFORM F_MONTA_ALV.
  PERFORM F_MONTA_CABECALHO.
  PERFORM F_EXECUTA_ALV.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Monto dados em tabelas internas conforme parâmetros selecionados
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .
  DATA VL_TIPO             TYPE C LENGTH 1.
  IF R_SERV EQ 'X'.
    SELECT ZUONR BELNR BUKRS GSBER BLDAT  BUDAT XBLNR
           RMWWR LIFNR USNAM MWSKZ1 ZBD1T ZFBDT CPUDT
           CPUTM WAERS WMWST1 GJAHR BLART
      FROM RBKP
      INTO TABLE T_RBKP
      WHERE BUKRS IN S_BUKRS
        AND GSBER IN S_GSBER
*        AND GJAHR IN S_GJAHR
        AND BUDAT IN S_PSTDAT
        AND MWSKZ1 EQ 'S1'.

    IF SY-SUBRC <> 0.
      MESSAGE I000 WITH 'Para os parâmetros informados não '
                        'foi encontrado Lançamentos.'.
      STOP.
    ENDIF.

    IF SY-SUBRC IS INITIAL.
      LOOP AT T_RBKP INTO WA_RBKP.
        V_INDEX = SY-TABIX.
        CONCATENATE WA_RBKP-BELNR WA_RBKP-GJAHR INTO WA_RBKP-AWKEY.
        MODIFY T_RBKP INDEX V_INDEX FROM WA_RBKP.
        CLEAR: V_INDEX,
               WA_RBKP.
      ENDLOOP.

      SELECT LIFNR NAME1
      FROM LFA1
      INTO TABLE T_LFA1_AUX
      FOR ALL ENTRIES IN T_RBKP
        WHERE LIFNR EQ T_RBKP-LIFNR.

      SELECT BUKRS GJAHR BLART BUDAT AWKEY BELNR
        FROM BKPF
        INTO TABLE T_BKPF
        FOR ALL ENTRIES IN T_RBKP
        WHERE BUKRS EQ T_RBKP-BUKRS
          AND BUDAT EQ T_RBKP-BUDAT
          AND GJAHR EQ T_RBKP-GJAHR
          AND BLART EQ T_RBKP-BLART
*          AND belnr EQ t_rbkp-belnr
          AND AWKEY EQ T_RBKP-AWKEY.

      IF SY-SUBRC IS INITIAL.
        SELECT BUKRS BELNR GJAHR HKONT DMBTR
          FROM BSIS
          INTO TABLE T_BSIS
          FOR ALL ENTRIES IN T_BKPF
          WHERE BUKRS EQ T_BKPF-BUKRS
          AND   GJAHR EQ T_BKPF-GJAHR
          AND   BELNR EQ T_BKPF-BELNR
          AND   ( HKONT EQ C_HKONT1
          OR      HKONT EQ C_HKONT2 ) .
      ENDIF.
    ENDIF.
  ELSE.
    IF P_TODAS EQ 'X'.
      SELECT DOCNUM NFTYPE DOCTYP DOCDAT
             PSTDAT CREDAT CRETIM CRENAM CHADAT
             CHATIM CHANAM SERIES SUBSER NFNUM NFE NFENUM
             ENTRAD FATURA PRINTD MANUAL BELNR
             GJAHR  BUKRS  BRANCH PARVW  PARID
             PARTYP CANCEL CANDAT DOCREF OBSERVAT
             VSTEL  NFDEC  NFTOT  NFTOT
        FROM J_1BNFDOC
        INTO TABLE IT_DOC
       WHERE PSTDAT IN S_PSTDAT
         AND PARID  IN S_PARID
         AND BUKRS  IN S_BUKRS
         AND BRANCH IN S_BRANCH
         AND DOCDAT IN S_DOCDAT
         AND MANUAL IN S_MANUAL.
    ELSE.
      IF P_ENTRA EQ 'X'.
        VL_TIPO = '1'.
      ENDIF.
      IF P_SAIDA EQ 'X'.
        VL_TIPO = '2'.
      ENDIF.

* Busca tipos de nota (Entrada/Saida)
      SELECT NFTYPE
        FROM J_1BAA
        INTO TABLE IT_BAA
       WHERE DIRECT EQ VL_TIPO.

      SELECT DOCNUM NFTYPE DOCTYP DOCDAT
             PSTDAT CREDAT CRETIM CRENAM CHADAT
             CHATIM CHANAM SERIES SUBSER NFNUM NFE NFENUM
             ENTRAD FATURA PRINTD MANUAL BELNR
             GJAHR  BUKRS  BRANCH PARVW  PARID
             PARTYP CANCEL CANDAT DOCREF OBSERVAT
             VSTEL  NFDEC  NFTOT  NFTOT
        FROM J_1BNFDOC
        INTO TABLE IT_DOC
         FOR ALL ENTRIES IN IT_BAA
       WHERE PSTDAT IN S_PSTDAT
         AND PARID  IN S_PARID
         AND BUKRS  IN S_BUKRS
         AND BRANCH IN S_BRANCH
         AND DOCDAT IN S_DOCDAT
         AND NFTYPE EQ IT_BAA-NFTYPE
         AND MANUAL IN S_MANUAL.
    ENDIF.

    CHECK SY-SUBRC EQ 0.

    SELECT *
      FROM J_1BNFLIN
      INTO TABLE IT_LIN
       FOR ALL ENTRIES IN IT_DOC
     WHERE DOCNUM EQ IT_DOC-DOCNUM_DOC.

    CHECK SY-SUBRC EQ 0.

* Montar dados de imobilizado
    SELECT BELNR GJAHR EBELN EBELP
      FROM RSEG
      INTO TABLE IT_RSEG
       FOR ALL ENTRIES IN IT_DOC
     WHERE BELNR EQ IT_DOC-BELNR
       AND GJAHR EQ IT_DOC-GJAHR.

    SORT IT_RSEG BY BELNR GJAHR.
IF IT_RSEG IS NOT INITIAL.

      LOOP AT IT_LIN ASSIGNING <FS_LIN>.
        <FS_LIN>-EBELN =  <FS_LIN>-XPED.
        <FS_LIN>-EBELP = <FS_LIN>-NITEMPED.
      ENDLOOP.

      SELECT EBELN EBELP AUFNR ANLN1 ANLN2
        FROM EKKN
        INTO TABLE IT_EKKN
         FOR ALL ENTRIES IN IT_LIN
       WHERE EBELN EQ IT_LIN-ebeln
         AND EBELP EQ IT_LIN-ebelp.

*      select ebeln ebelp aufnr anln1 anln2
*        from ekkn
*        into table it_ekkn
*         for all entries in it_rseg
*       where ebeln eq it_rseg-ebeln
*         and ebelp eq it_rseg-ebelp.


    SORT IT_EKKN BY EBELN EBELP.
ENDIF.

    SELECT ST~DOCNUM ST~ITMNUM ST~TAXTYP GR~TAXGRP ST~BASE
           ST~RATE   ST~TAXVAL ST~EXCBAS ST~OTHBAS
      FROM J_1BNFSTX AS ST
     INNER JOIN J_1BAJ AS GR
        ON GR~TAXTYP EQ ST~TAXTYP
      INTO TABLE IT_STX
       FOR ALL ENTRIES IN IT_LIN
     WHERE ST~DOCNUM EQ IT_LIN-DOCNUM
       AND ST~ITMNUM EQ IT_LIN-ITMNUM.
  ENDIF.
ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ALV
*&---------------------------------------------------------------------*
*       Monto estrutura de ALV.
*----------------------------------------------------------------------*
FORM F_MONTA_DADOS.

  IF R_SERV EQ 'X'.
    SORT: T_BKPF BY BUKRS BUDAT GJAHR BLART AWKEY,
          T_BSIS BY BUKRS GJAHR BELNR.

    LOOP AT T_RBKP INTO WA_RBKP.
      READ TABLE T_BKPF INTO WA_BKPF
      WITH KEY BUKRS = WA_RBKP-BUKRS
               BUDAT = WA_RBKP-BUDAT
               GJAHR = WA_RBKP-GJAHR
               BLART = WA_RBKP-BLART
*               belnr = wa_rbkp-belnr
               AWKEY = WA_RBKP-AWKEY
               BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
*        DO.
*        LOOP AT T_BSIS INTO WA_BSIS
*                       WHERE BUKRS = WA_BKPF-BUKRS
*                         AND GJAHR = WA_BKPF-GJAHR
*                         AND BELNR = WA_BKPF-BELNR.

        READ TABLE T_BSIS INTO WA_BSIS
        WITH KEY BUKRS = WA_BKPF-BUKRS
                 GJAHR = WA_BKPF-GJAHR
                 BELNR = WA_BKPF-BELNR
                 BINARY SEARCH.

        IF SY-SUBRC IS INITIAL.


          LOOP AT T_BSIS INTO WA_BSIS FROM SY-TABIX.
            IF WA_BSIS-BUKRS NE WA_BKPF-BUKRS OR
               WA_BSIS-GJAHR NE WA_BKPF-GJAHR OR
               WA_BSIS-BELNR NE WA_BKPF-BELNR.
              EXIT.
            ENDIF.

            IF WA_BSIS-HKONT EQ C_HKONT1.
              MOVE WA_BSIS-DMBTR TO WA_SAIDA-PIS.

            ELSEIF WA_BSIS-HKONT EQ C_HKONT2.
              MOVE WA_BSIS-DMBTR TO WA_SAIDA-COFINS.

            ENDIF.
          ENDLOOP.
        ELSE.
*            EXIT.

        ENDIF.

*        ENDDO.
      ENDIF.
      READ TABLE T_LFA1_AUX INTO WA_LFA1_AUX
        WITH KEY LIFNR = WA_RBKP-LIFNR.

      MOVE: WA_RBKP-ZUONR TO WA_SAIDA-ZUONR,
            WA_RBKP-BELNR TO WA_SAIDA-BELNR,
            WA_RBKP-BLDAT TO WA_SAIDA-BLDAT,
            WA_RBKP-BUDAT TO WA_SAIDA-BUDAT,
            WA_RBKP-XBLNR TO WA_SAIDA-XBLNR,
            WA_RBKP-RMWWR TO WA_SAIDA-RMWWR,
            WA_RBKP-LIFNR TO WA_SAIDA-LIFNR,
            WA_RBKP-USNAM TO WA_SAIDA-USNAM,
            WA_RBKP-MWSKZ1 TO WA_SAIDA-MWSKZ1,
            WA_RBKP-CPUDT TO WA_SAIDA-CPUDT,
            WA_RBKP-CPUTM TO WA_SAIDA-CPUTM,
            WA_RBKP-WMWST1 TO WA_SAIDA-WMWST1,
            WA_LFA1_AUX-NAME1 TO WA_SAIDA-NAME1.

      APPEND WA_SAIDA TO T_SAIDA.

      CLEAR: WA_SAIDA, WA_RBKP, WA_BKPF, WA_LFA1,
             WA_BSIS.

    ENDLOOP.

* ROLLOUT - 13/01/2010 - FIM
  ELSE.
    SORT: IT_DOC BY DOCNUM_DOC PARID,
    IT_LIN BY DOCNUM ITMNUM,
    IT_STX BY DOCNUM_STX ITMNUM_STX.

    LOOP AT IT_DOC INTO WA_DOC.
      CLEAR : WA_RELATORIO.
      WA_RELATORIO-DOCNUM = WA_DOC-DOCNUM_DOC.
      WA_RELATORIO-NFTYPE = WA_DOC-NFTYPE.
      WA_RELATORIO-DOCTYP = WA_DOC-DOCTYP.
      WA_RELATORIO-DOCDAT = WA_DOC-DOCDAT.
      WA_RELATORIO-PSTDAT = WA_DOC-PSTDAT.
      WA_RELATORIO-CREDAT = WA_DOC-CREDAT.
      WA_RELATORIO-CRETIM = WA_DOC-CRETIM.
      WA_RELATORIO-CRENAM = WA_DOC-CRENAM.
      WA_RELATORIO-CHADAT = WA_DOC-CHADAT.
      WA_RELATORIO-CHATIM = WA_DOC-CHATIM.
      WA_RELATORIO-CHANAM = WA_DOC-CHANAM.
      WA_RELATORIO-SERIES = WA_DOC-SERIES.
      WA_RELATORIO-SUBSER = WA_DOC-SUBSER.

      IF WA_DOC-NFE = 'X'.
        WA_RELATORIO-NFENUM = WA_DOC-NFENUM.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_DOC-NFNUM
          IMPORTING
            OUTPUT = WA_RELATORIO-NFENUM.

      ENDIF.

      WA_RELATORIO-ENTRAD = WA_DOC-ENTRAD.
      WA_RELATORIO-FATURA = WA_DOC-FATURA.
      WA_RELATORIO-PRINTD = WA_DOC-PRINTD.
      WA_RELATORIO-MANUAL = WA_DOC-MANUAL.
      WA_RELATORIO-BELNR  = WA_DOC-BELNR.
      WA_RELATORIO-GJAHR  = WA_DOC-GJAHR.
      WA_RELATORIO-BUKRS  = WA_DOC-BUKRS.
      WA_RELATORIO-BRANCH = WA_DOC-BRANCH.
      WA_RELATORIO-PARVW  = WA_DOC-PARVW.
      WA_RELATORIO-PARID  = WA_DOC-PARID.
      WA_RELATORIO-PARTYP = WA_DOC-PARTYP.
      WA_RELATORIO-CANCEL = WA_DOC-CANCEL.
      WA_RELATORIO-CANDAT = WA_DOC-CANDAT.
      WA_RELATORIO-DOCREF = WA_DOC-DOCREF_DOC.
      WA_RELATORIO-OBSERVAT = WA_DOC-OBSERVAT.
      WA_RELATORIO-VSTEL = WA_DOC-VSTEL.
      WA_RELATORIO-NFDEC = WA_DOC-NFDEC.
      WA_RELATORIO-NFTOT = WA_DOC-NFTOT.

      IF ( WA_DOC-PARVW EQ 'LF' ) OR ( WA_DOC-PARVW EQ 'SP' ).
        CLEAR: WA_LFA1.

        SELECT SINGLE LIFNR NAME1 STCD1 STCD2 STCD3 STKZN
          FROM LFA1
          INTO WA_LFA1
         WHERE LIFNR EQ WA_DOC-PARID.

        IF SY-SUBRC EQ 0.
          WA_RELATORIO-NAME1 = WA_LFA1-NAME1. "NOME
          WA_RELATORIO-STCD1 = WA_LFA1-STCD1. "CNPJ
          WA_RELATORIO-STCD2 = WA_LFA1-STCD2. "CPF
          WA_RELATORIO-STCD3 = WA_LFA1-STCD3. "INSCR.\RG
          WA_RELATORIO-STKZN = WA_LFA1-STKZN.
        ENDIF.

      ELSEIF ( WA_DOC-PARVW EQ 'WE' ) OR ( WA_DOC-PARVW EQ 'AG' ).

        CLEAR: WA_KNA1.

        SELECT SINGLE KUNNR NAME1 STCD1 STCD2 STCD3 STKZN
          FROM KNA1
          INTO WA_KNA1
         WHERE KUNNR EQ WA_DOC-PARID.

        IF SY-SUBRC EQ 0.
          WA_RELATORIO-NAME1 = WA_KNA1-NAME1. "NOME
          WA_RELATORIO-STCD1 = WA_KNA1-STCD1. "CNPJ
          WA_RELATORIO-STCD2 = WA_KNA1-STCD2. "CPF
          WA_RELATORIO-STCD3 = WA_KNA1-STCD3. "INSCR.\RG
          WA_RELATORIO-STKZN = WA_KNA1-STKZN.
        ENDIF.

      ENDIF.

      CLEAR: WA_RSEG, WA_EKKN.
      READ TABLE IT_RSEG INTO WA_RSEG WITH KEY BELNR = WA_RELATORIO-BELNR
                                               GJAHR = WA_RELATORIO-GJAHR
                                               BINARY SEARCH.

      READ TABLE IT_EKKN INTO WA_EKKN WITH KEY EBELN = WA_RSEG-EBELN
                                               EBELP = WA_RSEG-EBELP
                                               BINARY SEARCH.
      CHECK WA_EKKN-EBELN IN S_EBELN.
      CHECK WA_EKKN-ANLN1 IN S_ANLN1.
      CHECK WA_EKKN-AUFNR IN S_AUFNR.
      WA_RELATORIO-ANLN1 = WA_EKKN-ANLN1.
      WA_RELATORIO-ANLN2 = WA_EKKN-ANLN2.
      WA_RELATORIO-EBELN = WA_EKKN-EBELN.
      WA_RELATORIO-AUFNR = WA_EKKN-AUFNR.

      READ TABLE IT_LIN INTO WA_LIN WITH KEY DOCNUM = WA_DOC-DOCNUM_DOC
                                             BINARY SEARCH.
      IF SY-SUBRC NE 0.
        APPEND WA_RELATORIO TO IT_RELATORIO.
      ENDIF.

      LOOP AT IT_LIN INTO WA_LIN WHERE DOCNUM EQ WA_DOC-DOCNUM_DOC
                                   AND MATNR  IN S_MATNR
                                   AND CFOP   IN S_CFOP
                                   AND NBM    IN S_NBM
                                   AND NETPR  IN S_NETPR
                                   AND NETWR  IN S_NETWRT.
        IF R_SERV EQ 'X'.
          CHECK WA_LIN-MATNR EQ ''.
        ENDIF.
        IF R_MATE EQ 'X'.
          CHECK WA_LIN-MATNR NE ''.
        ENDIF.
        WA_RELATORIO-DOCNUM   = WA_LIN-DOCNUM.
        WA_RELATORIO-ITMNUM   = WA_LIN-ITMNUM.
        WA_RELATORIO-MATNR    = WA_LIN-MATNR.
        WA_RELATORIO-BWKEY    = WA_LIN-BWKEY.
        WA_RELATORIO-BWTAR    = WA_LIN-BWTAR.
        WA_RELATORIO-CHARG    = WA_LIN-CHARG.
        WA_RELATORIO-MATKL    = WA_LIN-MATKL.
        WA_RELATORIO-MAKTX    = WA_LIN-MAKTX.
        WA_RELATORIO-DOCREF   = WA_LIN-DOCREF.
        WA_RELATORIO-ITMREF   = WA_LIN-ITMREF.
        WA_RELATORIO-CFOP     = WA_LIN-CFOP.
        WA_RELATORIO-NBM      = WA_LIN-NBM.
        WA_RELATORIO-MATORG   = WA_LIN-MATORG.
        WA_RELATORIO-TAXSIT   = WA_LIN-TAXSIT.
        WA_RELATORIO-TAXSI2   = WA_LIN-TAXSI2.
        WA_RELATORIO-OWNPRO   = WA_LIN-OWNPRO.
        WA_RELATORIO-MATUSE   = WA_LIN-MATUSE.
        WA_RELATORIO-REFTYP   = WA_LIN-REFTYP.
        WA_RELATORIO-REFKEY   = WA_LIN-REFKEY.
        WA_RELATORIO-REFITM   = WA_LIN-REFITM.
        WA_RELATORIO-MENGE    = WA_LIN-MENGE.
        WA_RELATORIO-MEINS    = WA_LIN-MEINS.
        WA_RELATORIO-NETPR    = WA_LIN-NETPR.
        WA_RELATORIO-NETWR    = WA_LIN-NETWR.
        WA_RELATORIO-TAXLW1   = WA_LIN-TAXLW1.
        WA_RELATORIO-TAXLW2   = WA_LIN-TAXLW2.
        WA_RELATORIO-TMISS    = WA_LIN-TMISS.
        WA_RELATORIO-NETFRE   = WA_LIN-NETFRE.
        WA_RELATORIO-NETINS   = WA_LIN-NETINS.
        WA_RELATORIO-NETOTH   = WA_LIN-NETOTH.
        WA_RELATORIO-INDUS3   = WA_LIN-INDUS3.
        WA_RELATORIO-ITMTYP   = WA_LIN-ITMTYP.
        WA_RELATORIO-NETDIS   = WA_LIN-NETDIS.
        WA_RELATORIO-SPCSTO   = WA_LIN-SPCSTO.
        WA_RELATORIO-INCLTX   = WA_LIN-INCLTX.
        WA_RELATORIO-STATIT   = WA_LIN-STATIT.
        WA_RELATORIO-WERKS    = WA_LIN-WERKS.
        WA_RELATORIO-DIRECT   = WA_LIN-DIRECT.
        WA_RELATORIO-DSTCAT   = WA_LIN-DSTCAT.
        WA_RELATORIO-INDUS2   = WA_LIN-INDUS2.
        WA_RELATORIO-NFPRI    = WA_LIN-NFPRI.
        WA_RELATORIO-NFNET    = WA_LIN-NFNET.
        WA_RELATORIO-NFDIS    = WA_LIN-NFDIS.
        WA_RELATORIO-NFFRE    = WA_LIN-NFFRE.
        WA_RELATORIO-NFINS    = WA_LIN-NFINS.
        WA_RELATORIO-NFOTH    = WA_LIN-NFOTH.
        WA_RELATORIO-NETWRT   = WA_LIN-NETWRT.
        WA_RELATORIO-NFNETT   = WA_LIN-NFNETT.
        WA_RELATORIO-MWSKZ    = WA_LIN-MWSKZ.
        WA_RELATORIO-KALSM    = WA_LIN-KALSM.
        WA_RELATORIO-ICMSAVR  = WA_LIN-ICMSAVR.
        WA_RELATORIO-SUBTAVR  = WA_LIN-SUBTAVR.
        WA_RELATORIO-LPPNET   = WA_LIN-LPPNET.
        WA_RELATORIO-LPPBRT   = WA_LIN-LPPBRT.
        WA_RELATORIO-ICMSVALP = WA_LIN-ICMSVALP.
        WA_RELATORIO-SUBTVALP = WA_LIN-SUBTVALP.
        WA_RELATORIO-TAXLW3   = WA_LIN-TAXLW3.
        WA_RELATORIO-SRVNR    = WA_LIN-SRVNR.

        READ TABLE IT_STX INTO WA_STX WITH KEY DOCNUM_STX = WA_LIN-DOCNUM
                                               ITMNUM_STX = WA_LIN-ITMNUM
                                               BINARY SEARCH.
        IF SY-SUBRC NE 0.
          APPEND WA_RELATORIO TO IT_RELATORIO.
        ENDIF.
        WA_RELATORIO-TOTIMP = WA_RELATORIO-NETWR.
        LOOP AT IT_STX INTO WA_STX WHERE DOCNUM_STX EQ WA_LIN-DOCNUM
                                     AND ITMNUM_STX EQ WA_LIN-ITMNUM
                                     AND EXCBAS     IN S_EXCBAS.
          WA_RELATORIO-TOTIMP = WA_RELATORIO-TOTIMP + WA_STX-TAXVAL.
        ENDLOOP.
        LOOP AT IT_STX INTO WA_STX WHERE DOCNUM_STX EQ WA_LIN-DOCNUM
                                     AND ITMNUM_STX EQ WA_LIN-ITMNUM
                                     AND EXCBAS     IN S_EXCBAS.
          IF 'ICM0 ICM1 ICM2 ICM3 ICMF IFR1 IC1O ICM4 ICMN ICMO ICMX' CS WA_STX-TAXTYP. "Icms
            WA_RELATORIO-TAXTYP   = WA_STX-TAXTYP. "Tipo de imposto
            WA_RELATORIO-TAXGRP   = WA_STX-TAXGRP. "Grupo de imposto
            WA_RELATORIO-BASE     = WA_STX-BASE.   "Montante básico
            WA_RELATORIO-RATE     = WA_STX-RATE.   "Taxa de imposto
            WA_RELATORIO-TAXVAL   = WA_STX-TAXVAL. "Valor fiscal
            WA_RELATORIO-EXCBAS   = WA_STX-EXCBAS. "Montante básico excluído
            WA_RELATORIO-OTHBAS   = WA_STX-OTHBAS. "Outro montante básico
            APPEND WA_RELATORIO TO IT_RELATORIO.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ENDLOOP.
  ENDIF.
  IF IT_RELATORIO[] IS INITIAL
  AND T_SAIDA[] IS INITIAL.
    MESSAGE I000 WITH 'Não existe informação para esta seleção'.
    STOP.
  ENDIF.

ENDFORM.                    " F_MONTA_ALV

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS
*&---------------------------------------------------------------------*
*       Monto os dados para relatorio
*----------------------------------------------------------------------*
FORM F_MONTA_ALV.
  IF R_SERV EQ 'X'.
    FIELDCAT ''  'T_SAIDA'  'ZUONR'    ''  ''  '' 'Pedido de Compras' ''.
    FIELDCAT ''  'T_SAIDA'  'BELNR'    ''  ''  '' 'Nro.Doc.Fatura'    ''.
    FIELDCAT ''  'T_SAIDA'  'BLDAT'    ''  ''  '' 'Dt.Documento' ''.
    FIELDCAT ''  'T_SAIDA'  'BUDAT'    ''  ''  '' 'Dt.Lançamento' ''.
    FIELDCAT ''  'T_SAIDA'  'MWSKZ1'   ''  ''  '' 'IVA' ''.
    FIELDCAT ''  'T_SAIDA'  'RMWWR'    ''  ''  '' 'Nro.NF.' ''.
*    fieldcat ''  'T_SAIDA'  'CRENAM'   ''  ''  '' 'Nome do usuário' ''.
    FIELDCAT ''  'T_SAIDA'  'WMWST1'   ''  ''  '' 'Vlr.IVA' ''.
    FIELDCAT ''  'T_SAIDA'  'PIS'      ''  ''  '' 'Vlr.PIS' ''.
    FIELDCAT ''  'T_SAIDA'  'COFINS'   ''  ''  '' 'Vlr.Cofins' ''.
    FIELDCAT ''  'T_SAIDA'  'LIFNR'    ''  ''  '' 'Cod.Forn.' ''.
    FIELDCAT ''  'T_SAIDA'  'NAME1 '   ''  ''  '' 'Nome do Fornecedor' ''.
    FIELDCAT ''  'T_SAIDA'  'USNAM'    ''  ''  '' 'Usuário' ''.
    FIELDCAT ''  'T_SAIDA'  'CPUDT'    ''  ''  '' 'Data Entrada' ''.
    FIELDCAT ''  'T_SAIDA'  'CPUTM'    ''  ''  '' 'Hora da Entrada' ''.


  ELSE.
    FIELDCAT ''  C_TABELA 'DOCNUM'    ''  'X' '' 'Nº documento' ''.
    FIELDCAT ''  C_TABELA 'NFTYPE'    ''  ''  '' 'Ctg' ''.
    FIELDCAT ''  C_TABELA 'DOCTYP'    ''  ''  '' 'Tp doc' ''.
    FIELDCAT ''  C_TABELA 'DOCDAT'    ''  ''  '' 'Dt documento' ''.
    FIELDCAT ''  C_TABELA 'PSTDAT'    ''  ''  '' 'Dt lançamento' ''.
    FIELDCAT ''  C_TABELA 'CREDAT'    ''  ''  '' 'Data Criação' ''.
    FIELDCAT ''  C_TABELA 'CRETIM'    ''  ''  '' 'Hora da criação' ''.
    FIELDCAT ''  C_TABELA 'CRENAM'    ''  ''  '' 'Nome do usuário' ''.
    FIELDCAT ''  C_TABELA 'CHADAT'    ''  ''  '' 'Dt modificação' ''.
    FIELDCAT ''  C_TABELA 'CHATIM'    ''  ''  '' 'Hora de modificação' ''.
    FIELDCAT ''  C_TABELA 'SERIES'    ''  ''  '' 'Série' ''.
    FIELDCAT ''  C_TABELA 'SUBSER'    ''  ''  '' 'Subséries' ''.
    FIELDCAT ''  C_TABELA 'NFENUM '    ''  ''  '' 'Nº NF' ''.
    "fieldcat ''  c_tabela 'NFNUM '    ''  ''  '' 'Nº NF' ''.
    FIELDCAT ''  C_TABELA 'ENTRAD'    ''  ''  '' 'NF entrada' ''.
    FIELDCAT ''  C_TABELA 'FATURA'    ''  ''  '' 'Fatura' ''.
    FIELDCAT ''  C_TABELA 'PRINTD'    ''  ''  '' 'Impressa' ''.
    FIELDCAT ''  C_TABELA 'MANUAL'    ''  ''  '' 'Criad.manualmente' ''.
    FIELDCAT ''  C_TABELA 'BELNR'     ''  ''  '' 'Nº doc contábil' ''.
    FIELDCAT ''  C_TABELA 'GJAHR'     ''  ''  '' 'Exercício' ''.
    FIELDCAT ''  C_TABELA 'EBELN'     ''  ''  '' 'Nº Pedido' ''.
    FIELDCAT ''  C_TABELA 'ANLN1'     ''  ''  '' 'Imobilizado' ''.
    FIELDCAT ''  C_TABELA 'ANLN2'     ''  ''  '' 'Sub.Nº' ''.
    FIELDCAT ''  C_TABELA 'AUFNR'     ''  ''  '' 'Nº Ordem' ''.
    FIELDCAT ''  C_TABELA 'BUKRS'     ''  ''  '' 'Empresa' ''.
    FIELDCAT ''  C_TABELA 'BRANCH'    ''  ''  '' 'Local de negócios' ''.
    FIELDCAT ''  C_TABELA 'PARVW'     ''  ''  '' 'Função parceiro' ''.
    FIELDCAT ''  C_TABELA 'PARID'     ''  ''  '' 'Parceiro' ''.
    FIELDCAT ''  C_TABELA 'NAME1'     ''  ''  '' 'Nome Cliente / Fornecedor' ''.
    FIELDCAT ''  C_TABELA 'STCD1'     ''  ''  '' 'CNPJ' ''.
    FIELDCAT ''  C_TABELA 'STCD2'     ''  ''  '' 'CPF' ''.
    FIELDCAT ''  C_TABELA 'STCD3'     ''  ''  '' 'Inscrição \ RG' ''.
    FIELDCAT ''  C_TABELA 'PARTYP'    ''  ''  '' 'Tipo de parceiro' ''.
    FIELDCAT ''  C_TABELA 'CANCEL'    ''  ''  '' 'Estornado' ''.
    FIELDCAT ''  C_TABELA 'CANDAT'    ''  ''  '' 'Dt de estorno' ''.
    FIELDCAT ''  C_TABELA 'DOCREF'    ''  ''  '' 'Referência a nota fiscal' ''.
    FIELDCAT ''  C_TABELA 'OBSERVAT'  ''  ''  '' 'Observação' ''.
    FIELDCAT ''  C_TABELA 'VSTEL'     ''  ''  '' 'Local de expedição/recebimento de mercadoria' ''.
    FIELDCAT ''  C_TABELA 'NFDEC'     ''  ''  '' 'Decimais do preço incl. ICMS' ''.
    FIELDCAT ''  C_TABELA 'NFTOT'     ''  ''  '' 'Valor total com impostos' ''.
    FIELDCAT ''  C_TABELA 'ITMNUM'    ''  ''  '' 'Nº item' ''.
    FIELDCAT ''  C_TABELA 'MATNR'     ''  ''  '' 'Nº material' ''.
    FIELDCAT ''  C_TABELA 'BWKEY'     ''  ''  '' 'Área de avaliação' ''.
    FIELDCAT ''  C_TABELA 'BWTAR'     ''  ''  '' 'Tipo de avaliação' ''.
    FIELDCAT ''  C_TABELA 'CHARG'     ''  ''  '' 'Número do lote' ''.
    FIELDCAT ''  C_TABELA 'MATKL'     ''  ''  '' 'Grupo de mercadorias' ''.
    FIELDCAT ''  C_TABELA 'MAKTX'     ''  ''  '' 'Texto breve de material' ''.
    FIELDCAT ''  C_TABELA 'DOCREF'    ''  ''  '' 'Referência a nota fiscal' ''.
    FIELDCAT ''  C_TABELA 'ITMREF'    ''  ''  '' 'Referência ao nº item (retorno)' ''.
    FIELDCAT ''  C_TABELA 'CFOP'      ''  ''  '' 'Código CFOP e extensão' ''.
    FIELDCAT ''  C_TABELA 'NBM'       ''  ''  '' 'Código de controle p/imposto' ''.
    FIELDCAT ''  C_TABELA 'MATORG'    ''  ''  '' 'Origem de material' ''.
    FIELDCAT ''  C_TABELA 'TAXSIT'    ''  ''  '' 'Situação tributária ICMS' ''.
    FIELDCAT ''  C_TABELA 'TAXSI2'    ''  ''  '' 'Situação fiscal IPI' ''.
    FIELDCAT ''  C_TABELA 'OWNPRO'    ''  ''  '' 'Produção interna' ''.
    FIELDCAT ''  C_TABELA 'MATUSE'    ''  ''  '' 'Utilização de material' ''.
    FIELDCAT ''  C_TABELA 'REFTYP'    ''  ''  '' 'Tipo referência' ''.
    FIELDCAT ''  C_TABELA 'REFKEY'    ''  ''  '' 'Ref. documento de origem' ''.
    FIELDCAT ''  C_TABELA 'REFITM'    ''  ''  '' 'Item ref. documento de origem' ''.
    FIELDCAT ''  C_TABELA 'MENGE'     ''  ''  '' 'Quantidade' ''.
    FIELDCAT ''  C_TABELA 'MEINS'     ''  ''  '' 'Unidade de medida' ''.
    FIELDCAT ''  C_TABELA 'NETPR'     ''  ''  '' 'Preço líquido' ''.
    FIELDCAT ''  C_TABELA 'NETWR'     ''  ''  '' 'Valor líquido' ''.
    FIELDCAT ''  C_TABELA 'TAXLW1'    ''  ''  '' 'Direito fiscal: ICMS' ''.
    FIELDCAT ''  C_TABELA 'TAXLW2'    ''  ''  '' 'Direito fiscal: IPI' ''.
    FIELDCAT ''  C_TABELA 'TMISS'     ''  ''  '' 'Imposto ISS - sem cálculo ICMS/IPI' ''.
    FIELDCAT ''  C_TABELA 'NETFRE'    ''  ''  '' 'Montante líquido de frete' ''.
    FIELDCAT ''  C_TABELA 'NETINS'    ''  ''  '' 'Montante líquido do seguro' ''.
    FIELDCAT ''  C_TABELA 'NETOTH'    ''  ''  '' 'Despesas líquidas' ''.
    FIELDCAT ''  C_TABELA 'INDUS3'    ''  ''  '' 'Material: categoria CFOP' ''.
    FIELDCAT ''  C_TABELA 'ITMTYP'    ''  ''  '' 'Tipo item nota fiscal' ''.
    FIELDCAT ''  C_TABELA 'NETDIS'    ''  ''  '' 'Montante líquido da redução' ''.
    FIELDCAT ''  C_TABELA 'SPCSTO'    ''  ''  '' 'NF especial para determinação CFOP' ''.
    FIELDCAT ''  C_TABELA 'INCLTX'    ''  ''  '' 'Valor e preço incluíndo ICMS/ISS' ''.
    FIELDCAT ''  C_TABELA 'STATIT'    ''  ''  '' 'Código: item estatístico' ''.
    FIELDCAT ''  C_TABELA 'WERKS'     ''  ''  '' 'Centro' ''.
    FIELDCAT ''  C_TABELA 'DIRECT'    ''  ''  '' 'Direção do movimento de mercadorias' ''.
    FIELDCAT ''  C_TABELA 'DSTCAT'    ''  ''  '' 'Categoria de destino' ''.
    FIELDCAT ''  C_TABELA 'INDUS2'    ''  ''  '' 'Local de negócio: categoria CFOP' ''.
    FIELDCAT ''  C_TABELA 'NFPRI'     ''  ''  '' 'Prç líq.+impostos' ''.
    FIELDCAT ''  C_TABELA 'NFNET'     ''  ''  '' 'Vlr líq.+impostos' ''.
    FIELDCAT ''  C_TABELA 'NFDIS'     ''  ''  '' 'Vlr red.+impostos' ''.
    FIELDCAT ''  C_TABELA 'NFFRE'     ''  ''  '' 'Vlr frete+impostos' ''.
    FIELDCAT ''  C_TABELA 'NFINS'     ''  ''  '' 'Vlr seguro+imposto' ''.
    FIELDCAT ''  C_TABELA 'NFOTH'     ''  ''  '' 'Despesas+impostos' ''.
    FIELDCAT ''  C_TABELA 'NETWRT'    ''  ''  '' 'Vlr líq/frt/seg/desp/desc' ''.
    FIELDCAT ''  C_TABELA 'NFNETT'    ''  ''  '' 'Vlr líq/frt/seg/desp/desc' ''.
    FIELDCAT ''  C_TABELA 'TOTIMP'    ''  ''  '' 'Vlr Total + Impostos' ''.
    FIELDCAT ''  C_TABELA 'MWSKZ'     ''  ''  '' 'Código do IVA' ''.
    FIELDCAT ''  C_TABELA 'ICMSAVR'   ''  ''  '' 'ICMS méd. transf. estoque' ''.
    FIELDCAT ''  C_TABELA 'SUBTAVR'   ''  ''  '' 'Subtrib.méd.transf. estoque' ''.
    FIELDCAT ''  C_TABELA 'LPPNET'    ''  ''  '' 'Vlr líq último preço compra' ''.
    FIELDCAT ''  C_TABELA 'LPPBRT'    ''  ''  '' 'Vlr brt último preço compra' ''.
    FIELDCAT ''  C_TABELA 'ICMSVALP'  ''  ''  '' 'Vlr ICMS último preço compra' ''.
    FIELDCAT ''  C_TABELA 'SUBTVALP'  ''  ''  '' 'Vlr subtrib.último preço compra' ''.
    FIELDCAT ''  C_TABELA 'TAXLW3'    ''  ''  '' 'Lei imposto ISS' ''.
    FIELDCAT ''  C_TABELA 'SRVNR'     ''  ''  '' 'Nº de serviço' ''.
    FIELDCAT ''  C_TABELA 'TAXTYP'    ''  ''  '' 'Tipo de imposto' ''.
    FIELDCAT ''  C_TABELA 'TAXGRP'    ''  ''  '' 'Grupo de imposto' ''.
    FIELDCAT ''  C_TABELA 'BASE'      ''  ''  '' 'Montante básico' ''.
    FIELDCAT ''  C_TABELA 'RATE'      ''  ''  '' 'Taxa de imposto' ''.
    FIELDCAT ''  C_TABELA 'TAXVAL'    ''  ''  '' 'Valor fiscal' ''.
    FIELDCAT ''  C_TABELA 'EXCBAS'    ''  ''  '' 'Montante básico excluído' ''.
    FIELDCAT ''  C_TABELA 'OTHBAS'    ''  ''  '' 'Outro montante básico' ''.
  ENDIF.
ENDFORM.                    " F_MONTA_DADOS

*&---------------------------------------------------------------------*
*&      Form  f_executa_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_EXECUTA_ALV .
* Variavel Local
  DATA: VL_REPID LIKE SY-REPID.

  VL_REPID = SY-REPID.

  IT_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  IT_EVENT-FORM = SLIS_EV_TOP_OF_PAGE.
  APPEND IT_EVENT.

  VG_LAYOUT-ZEBRA               = 'X'.
  IF R_SERV EQ 'X'.
* Função para exibir o ALV
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = VL_REPID
*       i_callback_pf_status_set = 'SET_PF_STATUS'
        I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
        IS_LAYOUT               = VG_LAYOUT
*       i_background_id         = c_enjoy
        IT_FIELDCAT             = IT_FIELDCAT[]
        I_DEFAULT               = 'a'
        I_SAVE                  = 'x'
        IT_EVENTS               = IT_EVENT[]
      TABLES
        T_OUTTAB                = T_SAIDA
      EXCEPTIONS
        PROGRAM_ERROR           = 1
        OTHERS                  = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
* Função para exibir o ALV
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = VL_REPID
*       i_callback_pf_status_set = 'SET_PF_STATUS'
        I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
        IS_LAYOUT               = VG_LAYOUT
*       i_background_id         = c_enjoy
        IT_FIELDCAT             = IT_FIELDCAT[]
        I_DEFAULT               = 'a'
        I_SAVE                  = 'x'
        IT_EVENTS               = IT_EVENT[]
      TABLES
        T_OUTTAB                = IT_RELATORIO
      EXCEPTIONS
        PROGRAM_ERROR           = 1
        OTHERS                  = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " f_executa_alv

*&---------------------------------------------------------------------*
*&      Form  f_monta_cabecalho
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MONTA_CABECALHO .
* Colocando os dados para exibição do cabecalho
* Título do relatório
  DATA: VL_BUTXT(300)    TYPE C,               "Nome da empresa
        VL_FILIAL1       LIKE J_1BBRANCH-NAME, "Nome da filial
        VL_FILIAL2       LIKE J_1BBRANCH-NAME, "Nome da filial
        VL_FILIAL(100)   TYPE C,
        VL_DATA1(10)     TYPE C,
        VL_DATA2(10)     TYPE C,
        VL_DATA(25)      TYPE C,
        X_DATA_ATUAL(30) TYPE C,
        X_DATA           TYPE SY-DATUM,
        X_HORA           TYPE SY-UZEIT.

  CLEAR IT_HEADER.

  IT_HEADER-TYP  = 'H'.
  IT_HEADER-INFO = 'Relatório de Notas Fiscais'.
  APPEND  IT_HEADER.

  IF R_SERV EQ 'X'.
    X_DATA = SY-DATUM.
    X_HORA = SY-UZEIT.

    CONCATENATE X_DATA+6(2) '/'
                X_DATA+4(2) '/'
                X_DATA(4)   ' -  '
                X_HORA(2)   ':'
                X_HORA+2(2) ':'
                X_HORA+4(2) INTO X_DATA_ATUAL.
    CLEAR IT_HEADER.

    IT_HEADER-TYP  = 'A'.
    IT_HEADER-INFO =  X_DATA_ATUAL.
    APPEND  IT_HEADER.

    CLEAR IT_HEADER.

    IT_HEADER-TYP  = 'A'.
    IT_HEADER-INFO =  SY-UNAME.
    APPEND  IT_HEADER.
  ELSE.


    SELECT BUKRS BUTXT
      FROM T001
      INTO TABLE IT_T001
     WHERE BUKRS IN S_BUKRS.

    LOOP AT IT_T001 INTO WA_T001.
      CLEAR VL_BUTXT.
      CONCATENATE VL_BUTXT WA_T001-BUKRS ' - ' WA_T001-BUTXT INTO VL_BUTXT
                  SEPARATED BY SPACE.
      IT_HEADER-TYP  = 'S'.
      IT_HEADER-KEY  = 'Empresa'.
      IT_HEADER-INFO = VL_BUTXT.
      APPEND  IT_HEADER.

      CLEAR: VL_FILIAL1, VL_FILIAL2, VL_FILIAL.

      SELECT SINGLE NAME
        FROM J_1BBRANCH
        INTO VL_FILIAL1
       WHERE BUKRS  EQ WA_T001-BUKRS
         AND BRANCH EQ S_BRANCH-LOW.
      CONCATENATE S_BRANCH-LOW
                  VL_FILIAL1 INTO VL_FILIAL1
                  SEPARATED BY SPACE.
      IF NOT S_BRANCH-HIGH IS INITIAL.
        SELECT SINGLE NAME
          FROM J_1BBRANCH
          INTO VL_FILIAL2
         WHERE BUKRS  EQ WA_T001-BUKRS
           AND BRANCH EQ S_BRANCH-HIGH.
        CONCATENATE S_BRANCH-HIGH
                    VL_FILIAL2 INTO VL_FILIAL2
                    SEPARATED BY SPACE.
      ENDIF.
      CONCATENATE VL_FILIAL1
                  VL_FILIAL2 INTO VL_FILIAL
                  SEPARATED BY SPACE.

      IT_HEADER-TYP  = 'S'.
      IT_HEADER-KEY  = 'Filial'.
      IT_HEADER-INFO = VL_FILIAL.
      APPEND  IT_HEADER.

    ENDLOOP.

    CONCATENATE S_PSTDAT-LOW+6(2) '.'
                S_PSTDAT-LOW+4(2) '.'
                S_PSTDAT-LOW(4)
                INTO VL_DATA1.

    CONCATENATE S_PSTDAT-HIGH+6(2) '.'
                S_PSTDAT-HIGH+4(2) '.'
                S_PSTDAT-HIGH(4)
                INTO VL_DATA2.

    CONCATENATE VL_DATA1
                'a'
                VL_DATA2 INTO VL_DATA
                SEPARATED BY SPACE.

    IT_HEADER-TYP  = 'S'.
    IT_HEADER-KEY  = 'Periodo'.
    IT_HEADER-INFO = VL_DATA.
    APPEND  IT_HEADER.
  ENDIF.
ENDFORM.                    " f_monta_cabecalho


*&---------------------------------------------------------------------*
*&      Form  top_of_page                                              *
*&---------------------------------------------------------------------*
*     Form Para Fazer o cabeçalho   no ALV                             *
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
* Cabeçalho
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = c_logo
      IT_LIST_COMMENTARY = IT_HEADER[].
  SET TITLEBAR 'INI'.
ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Quando clica no link
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING UCOMM LIKE SY-UCOMM
      SELFIELD TYPE SLIS_SELFIELD.

  READ TABLE IT_RELATORIO INTO WA_RELATORIO INDEX SELFIELD-TABINDEX.
  IF SELFIELD-FIELDNAME = 'DOCNUM'.
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        TCODE  = 'J1B3N'
      EXCEPTIONS
        OK     = 1
        NOT_OK = 2.
    IF SY-SUBRC = 2.
      MESSAGE E077(S#) WITH 'J1B3N'.
    ENDIF.
    SET PARAMETER ID: 'JEF' FIELD WA_RELATORIO-DOCNUM.
    CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
  ENDIF.

  IF SELFIELD-FIELDNAME = 'BELNR'.
    READ TABLE T_SAIDA INTO WA_SAIDA INDEX SELFIELD-TABINDEX.
    IF NOT WA_SAIDA-BELNR IS INITIAL.
      SET PARAMETER ID: 'RBN' FIELD WA_SAIDA-BELNR,
                        'GJR' FIELD WA_SAIDA-BUDAT(4).
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM F_ORGANIZA_DADOS .
* ROLLOUT - 13/01/2010 - INICIO

ENDFORM.                    " F_ORGANIZA_DADOS
