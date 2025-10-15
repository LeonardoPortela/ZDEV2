*&---------------------------------------------------------------------*
*& Report  Z_SD_AGING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_SD_AGING.

TYPES :  BEGIN OF TY_BSID,
         BUKRS TYPE BSID-BUKRS, " Empresa
         KUNNR TYPE BSID-KUNNR, " Nº cliente 1
         BLART TYPE BSID-BLART, " Tipo de documento
         BUDAT TYPE BSID-BUDAT, " Data de lançamento no documento
         UMSKZ TYPE BSID-UMSKZ, " Código de Razão Especial
         VBEL2 TYPE BSID-VBEL2, " Documento de vendas
         ZUONR TYPE BSID-ZUONR, " Nº atribuição
         BELNR TYPE BSID-BELNR, " Nº documento de um documento contábil
         BLDAT TYPE BSID-BLDAT, " Data no documento
         WAERS TYPE BSID-WAERS, " Código da moeda
         XBLNR TYPE BSID-XBLNR, " Nº documento de referência
         SHKZG TYPE BSID-SHKZG, " Código débito/crédito
         GSBER TYPE BSID-GSBER, " Divisão
         DMBTR TYPE BSID-DMBTR, " Montante em moeda interna
         SGTXT TYPE BSID-SGTXT, " Texto do item
         SAKNR TYPE BSID-SAKNR, " Nº conta do Razão
         ZFBDT TYPE BSID-ZFBDT, " Data base para cálculo do vencimento
         ZBD1T TYPE BSID-ZBD1T, " Dias de desconto 1
         ZTERM TYPE BSID-ZTERM, " Chave de condições de pagamento
         DMBE2 TYPE BSID-DMBE2, " Documento de faturamento
         XREF1 TYPE BSID-XREF1, " Montante na 2ª moeda interna
         XREF2 TYPE BSID-XREF2, " Chave de referência do parceiro de negócios
         XREF3 TYPE BSID-XREF3, " Chave de referência do parceiro de negócios
         KIDNO TYPE BSID-KIDNO, " Chave de referência para item de doc.
         BUPLA TYPE BSID-BUPLA, " Referência de pagamento
         GJAHR TYPE BSID-GJAHR, " Local de negócios
         VBELN TYPE BSID-VBELN, " Documento de faturamento
         AUGBL TYPE BSID-AUGBL, " Nº documento de compensação
         AUGDT TYPE BSID-AUGDT, " Data de compensação
         REBZG TYPE BSID-REBZG, " Nº documento da fatura à qual pertence a operação
         HKONT TYPE BSID-HKONT, " Conta do Razão da contabilidade geral
         UMSKS TYPE BSID-UMSKS, " Classe de operação de Razão Especial
         BSTAT TYPE BSID-BSTAT, " Classe de operação de Razão Especial
       END OF TY_BSID,

       BEGIN OF TY_BSAD,
         BUKRS  TYPE BSAD-BUKRS, " Empresa
         KUNNR  TYPE BSAD-KUNNR, " Nº cliente 1
         BLART  TYPE BSAD-BLART, " Tipo de documento
         AUGDT  TYPE BSAD-AUGDT, " Data de compensação
         UMSKZ  TYPE BSAD-UMSKZ, " Código de Razão Especial
         VBEL2  TYPE BSAD-VBEL2, " Documento de vendas
         ZUONR  TYPE BSAD-ZUONR, " Nº atribuição
         BELNR  TYPE BSAD-BELNR, " Nº documento de um documento contábil
         BUDAT  TYPE BSAD-BUDAT, " Data de lançamento no documento
         BLDAT  TYPE BSAD-BLDAT, " Data no documento
         WAERS  TYPE BSAD-WAERS, " Código da moeda
         XBLNR  TYPE BSAD-XBLNR, " Nº documento de referência
         SHKZG  TYPE BSAD-SHKZG, " Código débito/crédito
         GSBER  TYPE BSAD-GSBER, " Divisão
         DMBTR  TYPE BSAD-DMBTR, " Montante em moeda interna
         SGTXT  TYPE BSAD-SGTXT, " Texto do item
         SAKNR  TYPE BSAD-SAKNR, " Nº conta do Razão
         ZFBDT  TYPE BSAD-ZFBDT, " Data base para cálculo do vencimento
         ZBD1T  TYPE BSAD-ZBD1T, " Dias de desconto 1
         ZTERM  TYPE BSAD-ZTERM, " Chave de condições de pagamento
         VBELN  TYPE BSAD-VBELN, " Documento de faturamento
         DMBE2  TYPE BSAD-DMBE2, " Montante na 2ª moeda interna
         XREF1  TYPE BSAD-XREF1, " Chave de referência do parceiro de negócios
         XREF2  TYPE BSAD-XREF2, " Chave de referência do parceiro de negócios
         XREF3  TYPE BSAD-XREF3, " Chave de referência do parceiro de negócios
         BUPLA  TYPE BSAD-BUPLA, " Local de negócios
         AUGBL  TYPE BSAD-AUGBL, " Nº documento de compensação
         GJAHR  TYPE BSAD-GJAHR, " Exercício
         KIDNO  TYPE BSAD-KIDNO, " Referência de pagamento
         HKONT  TYPE BSAD-HKONT, " Conta do Razão da contabilidade geral
         UMSKS  TYPE BSAD-UMSKS, " Classe de operação de Razão Especial
       END OF TY_BSAD,

       BEGIN OF TY_KNA1,
         KUNNR TYPE KNA1-KUNNR, " Nº Fornecedor 1
         KTOKD TYPE KNA1-KTOKD, " Grupo de contas do Fornecedor
       END OF TY_KNA1,


       BEGIN OF TY_ZSDT0041,
         VBELN         TYPE ZSDT0041-VBELN, " Nº Fornecedor 1
         DOC_SIMULACAO TYPE ZSDT0041-DOC_SIMULACAO, " Nº Fornecedor 1
       END OF TY_ZSDT0041,

       BEGIN OF TY_VBAK,
         VBELN         TYPE VBAK-VBELN, " Nº Fornecedor 1
         SPART         TYPE VBAK-SPART, " Nº FORNECEDOR 1
       END OF TY_VBAK,

       BEGIN OF TY_TSPAT,
         SPART         TYPE TSPAT-SPART, " Nº Fornecedor 1
         VTEXT         TYPE TSPAT-VTEXT, " Nº FORNECEDOR 1
       END OF TY_TSPAT,

       BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR, " Nº cliente 1
         KTOKK TYPE LFA1-KTOKK, " Grupo de contas do cliente
       END OF TY_LFA1,

       BEGIN OF TY_SAIDA,
         VBELN      TYPE VBAK-VBELN,
         AUART      TYPE VBAK-AUART,
         BUKRS      TYPE BSAD-BUKRS,
         KUNNR      TYPE BSAD-KUNNR,
         BLART      TYPE BSAD-BLART,
         AUGDT      TYPE BSAD-AUGDT,
         UMSKZ      TYPE BSAD-UMSKZ,
         VBEL2      TYPE BSAD-VBEL2,
         ZUONR      TYPE BSAD-ZUONR,
         BELNR      TYPE BSAD-BELNR,
         BUDAT      TYPE BSAD-BUDAT,
         BLDAT      TYPE BSAD-BLDAT,
         XBLNR      TYPE BSAD-XBLNR,
         SHKZG      TYPE BSAD-SHKZG,
         GSBER      TYPE BSAD-GSBER,
         DMBTR      TYPE BSAD-DMBTR,
         SGTXT      TYPE BSAD-SGTXT,
         SAKNR      TYPE BSAD-SAKNR,
         ZFBDT      TYPE BSAD-ZFBDT,
         ZFBDT_SUM  TYPE BSAD-ZFBDT,
         ZBD1T      TYPE BSAD-ZBD1T,
         ZTERM      TYPE BSAD-ZTERM,
         DMBE2      TYPE BSAD-DMBE2,
         XREF1      TYPE BSAD-XREF1,
         XREF2      TYPE BSAD-XREF2,
         XREF3      TYPE BSID-XREF3,
         BUPLA      TYPE BSAD-BUPLA,
         AUGBL      TYPE BSAD-AUGBL,
         GJAHR      TYPE BSAD-GJAHR,
         KIDNO      TYPE BSAD-KIDNO,
         HKONT      TYPE BSAD-HKONT,
         USNAM      TYPE BKPF-USNAM,
         TCODE      TYPE BKPF-TCODE,
         BKTXT      TYPE BKPF-BKTXT,
         AWKEY      TYPE BKPF-AWKEY,
         WAERS      TYPE BKPF-WAERS,
         MATNR      TYPE VBRP-MATNR,
         ARKTX      TYPE VBRP-ARKTX,
         FKIMG      TYPE VBRP-FKIMG,
         BSTKD      TYPE VBKD-BSTKD,
         NAME1      TYPE KNA1-NAME1,
         BUTXT      TYPE T001-BUTXT,
         NAME1_TW   TYPE T001W-NAME1,
         BEZEI      TYPE TVAKT-BEZEI,
         DOCNUM     TYPE J_1BNFDOC-DOCNUM,
         XNF        TYPE J_1BNFDOC-NFNUM,
         STATUS(4)  TYPE C,
         ALERTA(4)  TYPE C,
         XVLRREC    TYPE BSID-DMBTR,
         TXT20      TYPE SKAT-TXT20,
       END OF TY_SAIDA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: T_BDC            TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB        TYPE TABLE OF BDCMSGCOLL,
      IT_BSID          TYPE TABLE OF TY_BSID,
      IT_ZSDT0041      TYPE TABLE OF TY_ZSDT0041,
      IT_VBAK          TYPE TABLE OF TY_VBAK,
      IT_TSPAT         TYPE TABLE OF TY_TSPAT,

      IT_BSAD          TYPE TABLE OF TY_BSAD,

      IT_BSIK          TYPE TABLE OF BSIK,
      IT_BSAK          TYPE TABLE OF BSAK,

      IT_KNA1          TYPE TABLE OF TY_KNA1,
      IT_LFA1          TYPE TABLE OF TY_LFA1,
      IT_AGING_SIACORP TYPE TABLE OF ZAGING_SIACORP.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_CONT      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV       TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT    TYPE LVC_S_LAYO,
      WA_BSID      TYPE TY_BSID,
      WA_BSAD      TYPE TY_BSAD,

      WA_BSIK      TYPE BSIK,
      WA_BSAK      TYPE BSAK,

      WA_BSAD_AUX  TYPE TY_BSAD,
      WA_KNA1      TYPE TY_KNA1,
      WA_LFA1      TYPE TY_LFA1,
      WA_AGING_SIACORP TYPE ZAGING_SIACORP.


DATA : P_TODOS TYPE C LENGTH 1.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*


  IF SY-BATCH EQ ABAP_TRUE.
    TRY.
      ZCL_JOB=>GET_CK_PROGRAM_EXECUCAO( EXPORTING I_NOME_PROGRAM = SY-CPROG IMPORTING E_QTD = DATA(E_QTD) ).
    CATCH ZCX_JOB.
    ENDTRY.

    IF E_QTD GT 1.
      LEAVE PROGRAM.
    ENDIF.

  ENDIF.

  REFRESH: IT_AGING_SIACORP.

  PERFORM ZSELECIONA_DADOS.

  CALL FUNCTION 'Z_SD_OUTBOUND_AGING' IN BACKGROUND TASK
    DESTINATION 'XI_AGING_SIACORP'
    TABLES
      T_ZAGING_SIACORP = IT_AGING_SIACORP[].

  COMMIT WORK.

*----------------------------------------------------------------------*
***INCLUDE LZSD1F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZSELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZSELECIONA_DADOS .

  DATA: P_DT_INICIO TYPE BSID-BUDAT,
        P_DT_FIM    TYPE BSID-BUDAT.

  CLEAR : P_TODOS, P_DT_INICIO, P_DT_FIM.

  "P_DT_INICIO = SY-DATUM - 180.
  "CONCATENATE  SY-DATUM(4) '0101' INTO P_DT_INICIO.

  P_DT_INICIO = '20070101'.
  P_DT_FIM    = SY-DATUM.

  "Bsid e Bsad
*----------Clientes

  SELECT KUNNR KTOKD
    FROM KNA1
    INTO TABLE IT_KNA1
   WHERE KTOKD IN ('ZCNF','ZCNJ','ZCPF', 'ZCPJ').
*----------Fim Clientes

*----------Partidas em Aberto
  SELECT BS~BUKRS BS~KUNNR BS~BLART BS~BUDAT BS~UMSKZ BS~VBEL2 BS~ZUONR BS~BELNR
         BS~BLDAT BS~WAERS BS~XBLNR BS~SHKZG BS~GSBER BS~DMBTR BS~SGTXT BS~SAKNR
         BS~ZFBDT BS~ZBD1T BS~ZTERM BS~DMBE2 BS~XREF1 BS~XREF2 BS~XREF3 BS~KIDNO
         BS~BUPLA BS~GJAHR BS~VBELN BS~AUGBL BS~AUGDT BS~REBZG BS~HKONT BS~UMSKS BS~BSTAT
    FROM BSID AS BS
    INTO TABLE IT_BSID
     FOR ALL ENTRIES IN  IT_KNA1
   WHERE BUDAT  >= P_DT_INICIO
     AND BUDAT  <= P_DT_FIM
     AND KUNNR   = IT_KNA1-KUNNR
     AND BLART  NE 'VC'
     AND (
             ( UMSKZ  IN ( '', 'A' )  ) OR
             ( UMSKZ = 'L'  AND HKONT IN ('0000214000', '0000214002') )
          )

     "AND ( UMSKZ  EQ '' OR UMSKZ  EQ 'A' )
     AND ZUONR NE 'PERFORMANCE'.

    IF IT_BSID[] IS NOT INITIAL.

      DATA(LIT_BSID_AUX) = IT_BSID[].

      DELETE LIT_BSID_AUX WHERE VBEL2 IS INITIAL.
      SORT LIT_BSID_AUX BY VBEL2.
      DELETE ADJACENT DUPLICATES FROM LIT_BSID_AUX COMPARING VBEL2.

      IF LIT_BSID_AUX[] IS NOT INITIAL.
        SELECT VBELN DOC_SIMULACAO
           FROM ZSDT0041 INTO TABLE IT_ZSDT0041
           FOR ALL ENTRIES IN LIT_BSID_AUX
          WHERE VBELN = LIT_BSID_AUX-VBEL2.

        SELECT VBELN SPART
           FROM VBAK INTO TABLE IT_VBAK
           FOR ALL ENTRIES IN LIT_BSID_AUX
          WHERE VBELN = LIT_BSID_AUX-VBEL2.
      ENDIF.

      IF IT_VBAK[] IS NOT INITIAL.
        DATA(LIT_VBAK_AUX) = IT_VBAK[].

        SORT LIT_VBAK_AUX BY SPART.
        DELETE ADJACENT DUPLICATES FROM LIT_VBAK_AUX COMPARING SPART.

        SELECT SPART VTEXT
          FROM TSPAT INTO TABLE IT_TSPAT
           FOR ALL ENTRIES IN LIT_VBAK_AUX
          WHERE SPART = LIT_VBAK_AUX-SPART
            AND SPRAS = 'P'.
      ENDIF.


    ENDIF.


  PERFORM: Z_PARTIDAS_ABERTO_SAIDA.
  "PERFORM Z_PARTIDAS_ABERTO_SAIDA_SOMA.

*----------Fim Partidas em Aberto

*----------Partidas Compensadas
*  SELECT BSA~BUKRS BSA~KUNNR BSA~BLART BSA~AUGDT BSA~UMSKZ BSA~VBEL2 BSA~ZUONR BSA~BELNR
*         BSA~BUDAT BSA~BLDAT BSA~WAERS BSA~XBLNR BSA~SHKZG BSA~GSBER BSA~DMBTR BSA~SGTXT
*         BSA~SAKNR BSA~ZFBDT BSA~ZBD1T BSA~ZTERM BSA~VBELN BSA~DMBE2 BSA~XREF1 BSA~XREF2
*         BSA~XREF3 BSA~BUPLA BSA~AUGBL BSA~GJAHR BSA~KIDNO BSA~HKONT BSA~UMSKS
*    FROM BSAD AS BSA
*    INTO TABLE IT_BSAD
*     FOR ALL ENTRIES IN  IT_KNA1
*   WHERE AUGDT >= P_DT_INICIO
*     AND AUGDT <= P_DT_FIM
*     AND KUNNR  = IT_KNA1-KUNNR
*     AND BLART NE 'VC'
*     AND UMSKZ EQ ''
*     AND SHKZG  = 'H' .
*
*
*  PERFORM: Z_PARTIDAS_COMPENSADAS_SAIDA.
*----------Fim Partidas Compensadas

  "bsik e bsak
*----------Fornecedores

  SELECT LIFNR KTOKK
    FROM LFA1
    INTO TABLE IT_LFA1
   WHERE KTOKK IN ('ZFNF','ZFNJ', 'ZPRJ' ).",'ZPRF','ZPRJ'
*----------Fim Clientes

*----------Partidas em Aberto
  SELECT *
    FROM BSIK AS BS
    INTO TABLE IT_BSIK
     FOR ALL ENTRIES IN IT_LFA1
   WHERE BUDAT  >= P_DT_INICIO
     AND BUDAT  <= P_DT_FIM
     AND LIFNR   = IT_LFA1-LIFNR
     AND BLART  NE 'VC'
     "AND ( UMSKZ  EQ '' OR UMSKZ  EQ 'A'  OR UMSKZ  EQ 'K')

     AND (
            ( UMSKZ  IN ( '', 'A', 'K','M' ) ) OR
            ( UMSKZ  EQ 'L' AND HKONT IN ('0000214000', '0000214002') )
         )

     AND ZUONR NE 'PERFORMANCE'
    .

  PERFORM: Z_PARTIDAS_ABERTO_SAIDA_F.
  "PERFORM: Z_PARTIDAS_ABERTO_SAIDA_F_SOMA.

*----------Fim Partidas em Aberto

*----------Partidas Compensadas
*  SELECT *
*    FROM BSAK AS BSA
*    INTO TABLE IT_BSAK
*     FOR ALL ENTRIES IN IT_LFA1
*   WHERE AUGDT >= P_DT_INICIO
*     AND AUGDT <= P_DT_FIM
*     AND LIFNR  = IT_LFA1-LIFNR
*     AND BLART NE 'VC'
*     AND UMSKZ EQ ''
*     AND SHKZG  = 'H' .
*  PERFORM: Z_PARTIDAS_COMPENSADAS_SAIDA_F.
*----------Fim Partidas Compensadas


ENDFORM.                    " ZSELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_ABERTO_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_PARTIDAS_ABERTO_SAIDA .
  SORT: IT_BSID      BY VBEL2,
        IT_KNA1      BY KUNNR,
        IT_ZSDT0041  BY VBELN,
        IT_VBAK      BY VBELN,
        IT_TSPAT     BY SPART.

  LOOP  AT IT_BSID INTO WA_BSID.

    READ TABLE it_vbak INTO DATA(LWA_VBAK) WITH KEY VBELN = WA_BSID-vbel2 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_AGING_SIACORP-DOCVENDA    = LWA_VBAK-VBELN.

      READ TABLE it_tspat INTO DATA(LWA_tspat) WITH KEY spart = LWA_VBAK-spart BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        WA_AGING_SIACORP-setor_atividade = LWA_tspat-vtext.
      ENDIF.

    ENDIF.

    READ TABLE it_zsdt0041 INTO DATA(LWA_zsdt0041) WITH KEY VBELN = WA_BSID-vbel2 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      WA_AGING_SIACORP-DOC_SIMULADOR  = LWA_ZSDT0041-DOC_SIMULACAO.
    ENDIF.

    WA_AGING_SIACORP-BSTAT         = WA_BSID-BSTAT.
    WA_AGING_SIACORP-UMSKZ         = WA_BSID-UMSKZ.
    WA_AGING_SIACORP-CODCLIENTESAP = WA_BSID-KUNNR."CodClienteSAP
    WA_AGING_SIACORP-CODDOCUMENTO  = WA_BSID-BELNR."CodDocumento
    WA_AGING_SIACORP-CODPARCELA    = '001'.
    WA_AGING_SIACORP-DATDOCUMENTO  = WA_BSID-BLDAT."DatDocumento
    WA_AGING_SIACORP-DATVENCIMENTO = WA_BSID-ZFBDT + WA_BSID-ZBD1T."Dt vcto
    WA_AGING_SIACORP-BUKRS         = WA_BSID-BUKRS."Empresa
    WA_AGING_SIACORP-WERKS         = WA_BSID-BUPLA."Centro
    WA_AGING_SIACORP-MOEDA         = WA_BSID-WAERS."Moeda
    WA_AGING_SIACORP-TP_DOC_SAP	   = WA_BSID-BLART."Tipo de Documento
    WA_AGING_SIACORP-CONTA_RAZAO   = WA_BSID-HKONT."Conta Razão


    WA_AGING_SIACORP-VALABERTO = 0.

    IF ( WA_BSID-SHKZG = 'H' ).
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSID-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VALPARCELA          = WA_BSID-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSID-DMBE2 * ( -1 ).
      WA_AGING_SIACORP-VALABERTO           = WA_BSID-DMBTR * ( -1 ).
    ELSE.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSID-DMBTR.
      WA_AGING_SIACORP-VALPARCELA          = WA_BSID-DMBTR.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSID-DMBE2.
      WA_AGING_SIACORP-VALABERTO           = WA_BSID-DMBTR.
    ENDIF.

    WA_AGING_SIACORP-DT_REFERENCIA = SY-DATUM.

    "read table it_bsid into wa_bsid with key bukrs = wa_bsad-bukrs binary search.

    "ValAberto
    APPEND WA_AGING_SIACORP TO IT_AGING_SIACORP.

    CLEAR: WA_BSID,
           WA_AGING_SIACORP.
  ENDLOOP.

ENDFORM.                    " Z_PARTIDAS_ABERTO_SAIDA

*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_COMPENSADAS_SAIDA
*&---------------------------------------------------------------------*
FORM Z_PARTIDAS_COMPENSADAS_SAIDA.

  SORT: IT_BSAD  BY VBEL2,
        IT_KNA1  BY KUNNR.

  LOOP AT IT_BSAD INTO WA_BSAD.

    WA_AGING_SIACORP-CODCLIENTESAP = WA_BSAD-KUNNR."CodClienteSAP
    WA_AGING_SIACORP-CODDOCUMENTO  = WA_BSAD-BELNR."CodDocumento
    WA_AGING_SIACORP-CODPARCELA    = '001'.
    WA_AGING_SIACORP-DATDOCUMENTO  = WA_BSAD-BLDAT."DatDocumento
    WA_AGING_SIACORP-DT_PGTO       = WA_BSAD-AUGDT."DatDocumento
    WA_AGING_SIACORP-DATVENCIMENTO = WA_BSAD-ZFBDT + WA_BSAD-ZBD1T."Dt vcto
    WA_AGING_SIACORP-BUKRS         = WA_BSAD-BUKRS."Empresa
    WA_AGING_SIACORP-WERKS         = WA_BSAD-BUPLA."Centro
    WA_AGING_SIACORP-MOEDA         = WA_BSAD-WAERS."Moeda
    WA_AGING_SIACORP-TP_DOC_SAP	   = WA_BSAD-BLART."Tipo de Documento

    READ TABLE IT_BSID INTO WA_BSID WITH KEY BUKRS = WA_BSAD-BUKRS REBZG = WA_BSAD-BELNR.

    IF ( SY-SUBRC = 0 ) .
      WA_AGING_SIACORP-VALABERTO = WA_BSAD-DMBTR - WA_BSID-DMBTR.
    ELSE.
      WA_AGING_SIACORP-VALABERTO = WA_BSAD-DMBTR.
    ENDIF.

    IF ( WA_BSAD-SHKZG = 'H' ).
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSAD-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VALPARCELA          = WA_BSAD-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSAD-DMBE2 * ( -1 ).
      WA_AGING_SIACORP-VALABERTO = 0.
    ELSE.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   =  WA_BSAD-DMBTR.
      WA_AGING_SIACORP-VALPARCELA          =  WA_BSAD-DMBTR.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE =  WA_BSAD-DMBE2.
    ENDIF.

    WA_AGING_SIACORP-DT_REFERENCIA = SY-DATUM.

    "read table it_bsid into wa_bsid with key bukrs = wa_bsad-bukrs binary search.

    APPEND WA_AGING_SIACORP TO IT_AGING_SIACORP.

    CLEAR: WA_BSID,
           WA_BSAD,
           WA_AGING_SIACORP.
  ENDLOOP.

ENDFORM.                    " Z_PARTIDAS_COMPENSADAS_SAIDA
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_ABERTO_SAIDA_F
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_PARTIDAS_ABERTO_SAIDA_F .

  LOOP  AT IT_BSIK INTO WA_BSIK.

    WA_AGING_SIACORP-CODFORNECSAP  = WA_BSIK-LIFNR."CodClienteSAP
    WA_AGING_SIACORP-CODDOCUMENTO  = WA_BSIK-BELNR."CodDocumento
    WA_AGING_SIACORP-CODPARCELA    = '001'.
    WA_AGING_SIACORP-DATDOCUMENTO  = WA_BSIK-BLDAT."DatDocumento
    WA_AGING_SIACORP-DATVENCIMENTO = WA_BSIK-ZFBDT + WA_BSIK-ZBD1T."Dt vcto
    WA_AGING_SIACORP-BUKRS         = WA_BSIK-BUKRS."Empresa
    WA_AGING_SIACORP-WERKS         = WA_BSIK-BUPLA."Centro
    WA_AGING_SIACORP-MOEDA         = WA_BSIK-WAERS."Moeda
    WA_AGING_SIACORP-TP_DOC_SAP	   = WA_BSIK-BLART."Tipo de Documento
    WA_AGING_SIACORP-CONTA_RAZAO   = WA_BSIK-HKONT."Conta Razão

    WA_AGING_SIACORP-VALABERTO = 0.

    IF ( WA_BSIK-SHKZG = 'H' ).
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSIK-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VALPARCELA          = WA_BSIK-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSIK-DMBE2 * ( -1 ).
      WA_AGING_SIACORP-VALABERTO           = WA_BSIK-DMBTR * ( -1 ).
    ELSE.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSIK-DMBTR.
      WA_AGING_SIACORP-VALPARCELA          = WA_BSIK-DMBTR.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSIK-DMBE2.
      WA_AGING_SIACORP-VALABERTO           = WA_BSIK-DMBTR.
    ENDIF.

    WA_AGING_SIACORP-DT_REFERENCIA = SY-DATUM.

    "read table it_bsid into wa_bsid with key bukrs = wa_bsad-bukrs binary search.

    "ValAberto
    APPEND WA_AGING_SIACORP TO IT_AGING_SIACORP.

    CLEAR: WA_BSIK,
           WA_AGING_SIACORP.
  ENDLOOP.


ENDFORM.                    " Z_PARTIDAS_ABERTO_SAIDA_F
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_COMPENSADAS_SAIDA_F
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_PARTIDAS_COMPENSADAS_SAIDA_F .


  "SORT: IT_BSAK  BY VBEL2.

  LOOP AT IT_BSAK INTO WA_BSAK.

    WA_AGING_SIACORP-CODFORNECSAP  = WA_BSAK-LIFNR."CodClienteSAP
    WA_AGING_SIACORP-CODDOCUMENTO  = WA_BSAK-BELNR."CodDocumento
    WA_AGING_SIACORP-CODPARCELA    = '001'.
    WA_AGING_SIACORP-DATDOCUMENTO  = WA_BSAK-BLDAT."DatDocumento
    WA_AGING_SIACORP-DT_PGTO       = WA_BSAK-AUGDT."DatDocumento
    WA_AGING_SIACORP-DATVENCIMENTO = WA_BSAK-ZFBDT + WA_BSAK-ZBD1T."Dt vcto
    WA_AGING_SIACORP-BUKRS         = WA_BSAK-BUKRS."Empresa
    WA_AGING_SIACORP-WERKS         = WA_BSAK-BUPLA."Centro
    WA_AGING_SIACORP-MOEDA         = WA_BSAK-WAERS."Moeda
    WA_AGING_SIACORP-TP_DOC_SAP	   = WA_BSAK-BLART."Tipo de Documento

    READ TABLE IT_BSIK INTO WA_BSIK WITH KEY BUKRS = WA_BSAK-BUKRS REBZG = WA_BSAK-BELNR.

    IF ( SY-SUBRC = 0 ) .
      WA_AGING_SIACORP-VALABERTO = WA_BSAK-DMBTR - WA_BSIK-DMBTR.
    ELSE.
      WA_AGING_SIACORP-VALABERTO = WA_BSAK-DMBTR.
    ENDIF.

    IF ( WA_BSAD-SHKZG = 'H' ).
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSAK-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VALPARCELA          = WA_BSAK-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSAK-DMBE2 * ( -1 ).
      WA_AGING_SIACORP-VALABERTO = 0.
    ELSE.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   =  WA_BSAK-DMBTR.
      WA_AGING_SIACORP-VALPARCELA          =  WA_BSAK-DMBTR.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE =  WA_BSAK-DMBE2.
    ENDIF.

    WA_AGING_SIACORP-DT_REFERENCIA = SY-DATUM.

    "read table it_bsid into wa_bsid with key bukrs = wa_bsad-bukrs binary search.

    APPEND WA_AGING_SIACORP TO IT_AGING_SIACORP.

    CLEAR: WA_BSIK,
           WA_BSAK,
           WA_AGING_SIACORP.
  ENDLOOP.


ENDFORM.                    " Z_PARTIDAS_COMPENSADAS_SAIDA_F
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_ABERTO_SAIDA_SOMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_PARTIDAS_ABERTO_SAIDA_SOMA .
  SORT: IT_BSID      BY VBEL2,
        IT_KNA1      BY KUNNR.

  LOOP  AT IT_BSID INTO WA_BSID.

    WA_AGING_SIACORP-CODCLIENTESAP = WA_BSID-KUNNR."CodClienteSAP
    "WA_AGING_SIACORP-CODDOCUMENTO  = WA_BSID-BELNR."CodDocumento
    WA_AGING_SIACORP-CODPARCELA    = '001'.
    "WA_AGING_SIACORP-DATDOCUMENTO  = WA_BSID-BLDAT."DatDocumento
    "WA_AGING_SIACORP-DATVENCIMENTO = WA_BSID-ZFBDT + WA_BSID-ZBD1T."Dt vcto
    "WA_AGING_SIACORP-BUKRS         = WA_BSID-BUKRS."Empresa
    "WA_AGING_SIACORP-WERKS         = WA_BSID-BUPLA."Centro
    "WA_AGING_SIACORP-MOEDA         = WA_BSID-WAERS."Moeda
    "WA_AGING_SIACORP-TP_DOC_SAP     = WA_BSID-BLART."Tipo de Documento

    WA_AGING_SIACORP-VALABERTO = 0.

    IF ( WA_BSID-SHKZG = 'H' ).
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSID-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VALPARCELA          = WA_BSID-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSID-DMBE2 * ( -1 ).
      WA_AGING_SIACORP-VALABERTO           = WA_BSID-DMBTR * ( -1 ).
    ELSE.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSID-DMBTR.
      WA_AGING_SIACORP-VALPARCELA          = WA_BSID-DMBTR.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSID-DMBE2.
      WA_AGING_SIACORP-VALABERTO           = WA_BSID-DMBTR.
    ENDIF.

    WA_AGING_SIACORP-DT_REFERENCIA = SY-DATUM.

    "read table it_bsid into wa_bsid with key bukrs = wa_bsad-bukrs binary search.

    "ValAberto
    COLLECT WA_AGING_SIACORP INTO IT_AGING_SIACORP.

    CLEAR: WA_BSID,
           WA_AGING_SIACORP.
  ENDLOOP.
ENDFORM.                    " Z_PARTIDAS_ABERTO_SAIDA_SOMA
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_ABERTO_SAIDA_F_SOMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_PARTIDAS_ABERTO_SAIDA_F_SOMA .

   LOOP  AT IT_BSIK INTO WA_BSIK.

    WA_AGING_SIACORP-CODFORNECSAP  = WA_BSIK-LIFNR."CodClienteSAP
    "WA_AGING_SIACORP-CODDOCUMENTO  = WA_BSIK-BELNR."CodDocumento
    WA_AGING_SIACORP-CODPARCELA    = '001'.
    "WA_AGING_SIACORP-DATDOCUMENTO  = WA_BSIK-BLDAT."DatDocumento
    "WA_AGING_SIACORP-DATVENCIMENTO = WA_BSIK-ZFBDT + WA_BSIK-ZBD1T."Dt vcto
    "WA_AGING_SIACORP-BUKRS         = WA_BSIK-BUKRS."Empresa
    "WA_AGING_SIACORP-WERKS         = WA_BSIK-BUPLA."Centro
    "WA_AGING_SIACORP-MOEDA         = WA_BSIK-WAERS."Moeda
    "WA_AGING_SIACORP-TP_DOC_SAP     = WA_BSIK-BLART."Tipo de Documento

    WA_AGING_SIACORP-VALABERTO = 0.

    IF ( WA_BSID-SHKZG = 'H' ).
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSIK-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VALPARCELA          = WA_BSIK-DMBTR * ( -1 )."Montante em moeda interna
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSIK-DMBE2 * ( -1 ).
      WA_AGING_SIACORP-VALABERTO           = WA_BSIK-DMBTR * ( -1 ).
    ELSE.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_INT   = WA_BSIK-DMBTR.
      WA_AGING_SIACORP-VALPARCELA          = WA_BSIK-DMBTR.
      WA_AGING_SIACORP-VLR_DOC_MOEDA_FORTE = WA_BSIK-DMBE2.
      WA_AGING_SIACORP-VALABERTO           = WA_BSIK-DMBTR.
    ENDIF.

    WA_AGING_SIACORP-DT_REFERENCIA = SY-DATUM.

    "read table it_bsid into wa_bsid with key bukrs = wa_bsad-bukrs binary search.

    "ValAberto
    COLLECT WA_AGING_SIACORP INTO IT_AGING_SIACORP.

    CLEAR: WA_BSIK,
           WA_AGING_SIACORP.
  ENDLOOP.

ENDFORM.                    " Z_PARTIDAS_ABERTO_SAIDA_F_SOMA
