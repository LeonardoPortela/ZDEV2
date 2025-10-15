************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 29.04.2011                                          *
* Objetivo    ...: Relatório Geral de Ordens de Vendas                 *
* Transação   ...: ZFIS21                                              *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 29.04.2011   Victor Hugo     Criação       14:36:22      DEVK915618  *
* 01.06.2011   Victor Hugo     Modificação   14:22:28      DEVK916424  *
* 20.09.2011   Camila Brand    Modificação
* 10.01.2012   Camila Brand    Modificação                 DEVK920143  *
* 11.01.2012   Camila Brand    Modificação                 DEVK920167  *
* 11.01.2012   Camila Brand    Modificação                 DEVK920183  *
* 11.01.2012   Camila Brand    Modificação                 DEVK920185  *
* 11.01.2012   Camila Brand    Modificação                 DEVK920189  *
* 11.01.2012   Camila Brand    Modificação                 DEVK920191  *
* 12.01.2012   Camila Brand    Modificação                 DEVK920197  *
************************************************************************

REPORT  zfis0021.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: vbak,
        vbap,
        bsid,
        vbkd,
        zsdt0051.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

  " Documento de vendas: dados de cabeçalho
  BEGIN OF ty_vbak,
    vbeln    TYPE vbak-vbeln,    " Documento de vendas
    erdat    TYPE vbak-erdat,    " Data de criação do registro
    auart    TYPE vbak-auart,    " Tipo de documento de vendas
    knumv    TYPE vbak-knumv,    " Nº condição do documento
    kunnr    TYPE vbak-kunnr,    " Emissor da ordem (Cliente)
    vkorg    TYPE vbak-vkorg,    " Organização de vendas
    bukrs_vf TYPE vbak-bukrs_vf, " Empresa
    waerk    TYPE vbak-waerk,    " Moeda
    flag(1)  TYPE c,
  END OF ty_vbak,

  BEGIN OF ty_zsdt0051,
    nro_sol_ov TYPE zsdt0051-nro_sol_ov,
    tp_venda   TYPE zsdt0051-tp_venda,
  END OF ty_zsdt0051,

  BEGIN OF ty_zsdt0053,
    vbeln      TYPE zsdt0053-vbeln,
    nro_sol_ov TYPE zsdt0053-nro_sol_ov,
  END OF ty_zsdt0053,

  BEGIN OF ty_zsdt0057,
    tp_venda TYPE zsdt0057-tp_venda,
    bezei    TYPE zsdt0057-bezei,
  END OF ty_zsdt0057,

  " Documento de vendas: dados de cabeçalho (seleção)
  BEGIN OF ty_vbak2,
    vbeln   TYPE vbak-vbeln,    " Documento de vendas
    zfbdt   TYPE bsid-zfbdt,
    vbtyp_n TYPE vbfa-vbtyp_n,
    vbtyp_v TYPE vbfa-vbtyp_v,
    zbd1t   TYPE bsid-zbd1t,
  END OF ty_vbak2,

  " Documento de vendas: dados de item
  BEGIN OF ty_vbap,
    vbeln  TYPE vbap-vbeln, " Documento de vendas
    matnr  TYPE vbap-matnr, " Nº do material
    arktx  TYPE vbap-arktx, " Texto breve do item da ordem do cliente
    werks  TYPE vbap-werks, " Centro
    kwmeng TYPE vbap-kwmeng, " Quantidade da ordem acumulada em unidade de venda
    vrkme  TYPE vbap-vrkme, " Unidade de venda
  END OF ty_vbap,

  " Contabilidade: índice secundário para clientes
  BEGIN OF ty_bsid,
    budat TYPE bsid-budat, " Data de lançamento no documento
    bldat TYPE bsid-bldat, " Data no documento
    xblnr TYPE bsid-xblnr, " Nº documento de referência
    blart TYPE bsid-blart, " Tipo de documento
    gsber TYPE bsid-gsber, " Divisão
    dmbtr TYPE bsid-dmbtr, " Montante em moeda interna
    dmbe2 TYPE bsid-dmbe2, " Montante na 2ª moeda interna
    bukrs TYPE bsid-bukrs, " Empresa
    zfbdt TYPE bsid-zfbdt, " Data base para cálculo do vencimento
    zbd1t TYPE bsid-zbd1t, " Dias de desconto 1
    gjahr TYPE bsid-gjahr, " Exercício
    belnr TYPE bsid-belnr, " Nº documento de um documento contábil
    kunnr TYPE bsid-kunnr, " Nº cliente 1
    vbeln TYPE bsid-vbeln, " Documento de faturamento
    shkzg TYPE bsid-shkzg, " Código débito/crédito
    rebzg TYPE bsid-rebzg, " Nº documento da fatura à qual pertence a operação
    data  TYPE bsid-zfbdt, " Data do Vencimento

  END OF ty_bsid,

  BEGIN OF ty_bsad,
    budat TYPE bsad-budat, " Data de lançamento no documento
    bldat TYPE bsad-bldat, " Data no documento
    xblnr TYPE bsad-xblnr, " Nº documento de referência
    blart TYPE bsad-blart, " Tipo de documento
    gsber TYPE bsad-gsber, " Divisão
    dmbtr TYPE bsad-dmbtr, " Montante em moeda interna
    dmbe2 TYPE bsad-dmbe2, " Montante na 2ª moeda interna
    bukrs TYPE bsad-bukrs, " Empresa
    zfbdt TYPE bsad-zfbdt, " Data base para cálculo do vencimento
    zbd1t TYPE bsad-zbd1t, " Dias de desconto 1
    gjahr TYPE bsad-gjahr, " Exercício
    belnr TYPE bsad-belnr, " Nº documento de um documento contábil
    kunnr TYPE bsad-kunnr, " Nº cliente 1
    vbeln TYPE bsad-vbeln, " Documento de faturamento
    shkzg TYPE bsad-shkzg, " Código débito/crédito
    augbl TYPE bsad-augbl, " doc compensação
    rebzg TYPE bsad-rebzg , " Código débito/crédito
    data  TYPE bsad-zfbdt, " Data do Vencimento
  END OF ty_bsad,

  " Cabeçalho do documento contábil  bukrs belnr gjahr awtyp awkey budat
  BEGIN OF ty_bkpf,
    bukrs     TYPE  bkpf-bukrs, " Empresa
    belnr     TYPE  bkpf-belnr, " Nº documento de um documento contábil
    gjahr     TYPE  bkpf-gjahr, " Exercício
    awtyp     TYPE  bkpf-awtyp, " Operação de referência
    awkey     TYPE  bkpf-awkey, " Chave referência
    budat     TYPE  bkpf-budat, " Data de lançamento no documento
    awkey_aux TYPE  j_1bnflin-refkey, " Referência ao documento de origem
  END OF ty_bkpf,

  " Fluxo de documentos de vendas e distribuição
  BEGIN OF ty_vbfa,
    vbeln        TYPE vbfa-vbeln,   " Documento de vendas e distribuição subseqüente
    vbtyp_n      TYPE vbfa-vbtyp_n, " Categoria de documento SD subseqüente
    vbtyp_v      TYPE vbfa-vbtyp_v, " Ctg.documento de venda e distribuição (SD) precedente
    vbelv        TYPE vbfa-vbelv,   " Documento de vendas e distribuição precedente
    rfmng        TYPE vbfa-rfmng,   " Quantidade referenciada em unidade medida básica
    matnr        TYPE vbfa-matnr,   " Nº do material
    meins        TYPE vbfa-meins,   "Unidade de medida básica
    erdat        TYPE vbfa-erdat,   " Data de criação do registro
    vbeln_awkey  TYPE bkpf-awkey,   " Documento de vendas e distribuição subseqüente
    gjahr        TYPE bkpf-gjahr,   "Ano do documento contabil
    bukrs        TYPE bkpf-bukrs,   "Empresa
    flag_bsid(1) TYPE c      ,   " indica parcela refaturada
  END OF ty_vbfa,

  " Documento de vendas: dados comerciais
  BEGIN OF ty_vbkd,
    vbeln TYPE vbkd-vbeln, " Nº documento de vendas e distribuição
    zterm TYPE vbkd-zterm, " Chave de condições de  pagamento
    valdt TYPE vbkd-valdt, " Data efetiva fixa
    zlsch TYPE vbkd-zlsch, " Forma de Pagamento             "Luis
  END OF ty_vbkd,

  " Condições (dados de operação)
  BEGIN OF ty_konv,
    knumv TYPE konv-knumv, " Nº condição do documento
    kschl TYPE konv-kschl, " Tipo de condição
    kwert TYPE konv-kwert, " Valor condição
  END OF ty_konv,

  " Partidas individuais da nota fiscal
  BEGIN OF ty_j_1bnflin,
    refkey TYPE j_1bnflin-refkey, " Referência ao documento de origem
    docnum TYPE j_1bnflin-docnum, " Nº documento
  END OF ty_j_1bnflin,

  " Cabeçalho da nota fiscal
  BEGIN OF ty_j_1bnfdoc,
    docnum TYPE j_1bnfdoc-docnum, " Nº documento
    nfe    TYPE j_1bnfdoc-nfe,    " Nota Fiscal eletrônica
    nfenum TYPE j_1bnfdoc-nfenum, " Nº NF-e de nove posições
    nfnum  TYPE j_1bnfdoc-nfnum,  " Nº nota fiscal
  END OF ty_j_1bnfdoc,

  " Mestre de clientes (parte geral)
  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr, " Nº cliente 1
    name1 TYPE kna1-name1,                                 " Nome 1
  END OF ty_kna1,

  " Estruturas Auxiliares
  BEGIN OF ty_vbfa_aux,
    vbeln       TYPE vbfa-vbeln,   " Documento de vendas e distribuição subseqüente
    vbtyp_n     TYPE vbfa-vbtyp_n, " Categoria de documento SD subseqüente
    vbtyp_v     TYPE vbfa-vbtyp_v, " Ctg.documento de venda e distribuição (SD) precedente
    vbelv       TYPE vbfa-vbelv,   " Documento de vendas e distribuição precedente
    rfmng       TYPE vbfa-rfmng,   " Quantidade referenciada em unidade medida básica
    matnr       TYPE vbfa-matnr,   " Nº do material
    meins       TYPE vbfa-meins,   " Unidade de medida básica
    erdat       TYPE vbfa-erdat,   " Data de criação do registro
    vbeln_awkey TYPE bkpf-awkey,   " Documento de vendas e distribuição subseqüente
    gjahr       TYPE bkpf-gjahr,   " Ano do documento contabil
    bukrs       TYPE bkpf-bukrs,   " Empresa
  END OF ty_vbfa_aux,

  BEGIN OF ty_saida,
    kunnr      TYPE bsid-kunnr,
    name1      TYPE kna1-name1,
    vbeln      TYPE vbak-vbeln,
    vkorg      TYPE vbak-vkorg,
    belnr      TYPE bsid-belnr,
    awkey      TYPE bkpf-awkey,
    budat      TYPE bsid-budat,
    venc       TYPE bsid-budat,
    bldat      TYPE bsid-bldat,
    zfbdt      TYPE bsid-zfbdt,
    zterm      TYPE vbkd-zterm,
    valdt      TYPE vbkd-valdt,
    rfmng      TYPE vbfa-rfmng,
    dmbtr      TYPE bsid-dmbtr,
    dmbe2      TYPE bsid-dmbe2,
    nfe        TYPE j_1bnfdoc-nfenum,
    kwert      TYPE konv-kwert,
    kwmeng     TYPE vbap-kwmeng,
    erdat      TYPE vbak-erdat,
    qtd        TYPE vbfa-rfmng,
    qtd_aux    TYPE vbfa-rfmng,
    gjahr      TYPE bsid-gjahr,
    waerk      TYPE vbak-waerk,
    valor      TYPE bsid-dmbtr,
    valord     TYPE bsid-dmbe2,
    tipo       TYPE c LENGTH 50,
    tipot      TYPE c LENGTH 4,
    dev        TYPE vbfa-vbtyp_n,
    nro_sol_ov TYPE zsded013,
    tp_venda   TYPE zsdt0051-tp_venda,
    bezei      TYPE zsdt0057-bezei,
    zlsch      TYPE vbkd-zlsch,              "Luis
  END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.
*----------------------------------------------------------------------*
* CONSTANTES
*----------------------------------------------------------------------*

CONSTANTS: msgsd TYPE c LENGTH 50 VALUE 'Faturamento SD',
           msgov TYPE c LENGTH 50 VALUE 'Saldo a Receber Ordem de Venda'.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: t_bdc           TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      t_messtab       TYPE TABLE OF bdcmsgcoll,

      it_vbak         TYPE TABLE OF ty_vbak,
      it_zsdt0051     TYPE TABLE OF ty_zsdt0051,
      it_zsdt0053     TYPE TABLE OF ty_zsdt0053,
      it_zsdt0057     TYPE TABLE OF ty_zsdt0057,
      it_vbak2        TYPE TABLE OF ty_vbak2,
      it_vbak3        TYPE TABLE OF ty_vbak2,
      it_vbap         TYPE TABLE OF ty_vbap,
      it_bsid         TYPE TABLE OF ty_bsid,
      it_bsad         TYPE TABLE OF ty_bsid,
      it_bkpf         TYPE TABLE OF ty_bkpf,
      it_vbfa         TYPE TABLE OF ty_vbfa,
      it_vbfa2        TYPE TABLE OF ty_vbfa,
      it_vbkd         TYPE TABLE OF ty_vbkd,
      it_vbkd2        TYPE TABLE OF ty_vbkd,
      it_konv         TYPE TABLE OF ty_konv,
      it_j_1bnflin    TYPE TABLE OF ty_j_1bnflin,
      it_j_1bnfdoc    TYPE TABLE OF ty_j_1bnfdoc,
      it_kna1         TYPE TABLE OF ty_kna1,
      it_saida        TYPE TABLE OF ty_saida,

      " Tabelas Auxiliares
      it_vbfa_aux     TYPE TABLE OF ty_vbfa_aux,
      it_vbfa_aux_v   TYPE TABLE OF ty_vbfa_aux,
      it_bsid_aux     TYPE TABLE OF ty_bsid,
      it_bsid_aux2    TYPE TABLE OF ty_bsid,
      it_bsad_aux     TYPE TABLE OF ty_bsad,
      it_bkpf_aux     TYPE TABLE OF ty_bkpf,
      it_saida_aux    TYPE TABLE OF ty_saida,
      it_saida2       TYPE TABLE OF ty_saida,

      " Linhas
      it_linha_um     TYPE TABLE OF ty_saida,
      it_linha_dois   TYPE TABLE OF ty_saida,
      it_linha_quatro TYPE TABLE OF ty_saida.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_cont         TYPE REF TO cl_gui_custom_container,
  wa_alv          TYPE REF TO cl_gui_alv_grid,
  wa_layout       TYPE lvc_s_layo,

  wa_vbak         TYPE ty_vbak,
  wa_zsdt0051     TYPE ty_zsdt0051,
  wa_zsdt0053     TYPE ty_zsdt0053,
  wa_zsdt0057     TYPE ty_zsdt0057,
  wa_vbak2        TYPE ty_vbak2,
  wa_vbap         TYPE ty_vbap,
  wa_bsid         TYPE ty_bsid,
  wa_bsad         TYPE ty_bsad,
  wa_bkpf         TYPE ty_bkpf,
  wa_vbfa         TYPE ty_vbfa,
  wa_vbkd         TYPE ty_vbkd,
  wa_konv         TYPE ty_konv,
  wa_j_1bnflin    TYPE ty_j_1bnflin,
  wa_j_1bnfdoc    TYPE ty_j_1bnfdoc,
  wa_kna1         TYPE ty_kna1,
  wa_saida        TYPE ty_saida,
  wa_setleaf      TYPE setleaf,

  " Work Areas Auxiliares
  wa_vbfa_aux     TYPE ty_vbfa_aux,
  wa_vbfa_aux_v   TYPE ty_vbfa_aux,
  wa_bsid_aux     TYPE ty_bsid,
  wa_bsid_aux2    TYPE ty_bsid,
  wa_bsad_aux     TYPE ty_bsad,
  wa_bkpf_aux     TYPE ty_bkpf,
  wa_saida_aux    TYPE ty_saida,
  wa_saida2       TYPE ty_saida,

  " Linhas
  wa_linha_um     TYPE ty_saida,
  wa_linha_dois   TYPE ty_saida,
  wa_linha_quatro TYPE ty_saida.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  it_fcat    TYPE TABLE OF ty_estrutura,
  s_variant  TYPE disvariant           , " Tabela Estrutura co
  t_top      TYPE slis_t_listheader,
  xs_events  TYPE slis_alv_event,
  events     TYPE slis_t_event,
  t_print    TYPE slis_print_alv,
  v_report   LIKE sy-repid,
  t_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE,
  it_setleaf LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
  estrutura  TYPE TABLE OF ty_estrutura.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_vkorg   FOR vbak-vkorg  OBLIGATORY,   " Empresa
                  p_auart   FOR vbak-auart,               " Tipo de Ordem Venda
                  p_vbeln   FOR vbak-vbeln ,              " Ordem de Venda
                  p_werks   FOR vbap-werks ,              " Centro
                  p_kunnr   FOR vbak-kunnr  OBLIGATORY,   " Cliente
                  p_matnr   FOR vbap-matnr ,              " Material
                  p_zfbdt   FOR bsid-zfbdt  OBLIGATORY,   " Data de Vencimento
                  p_tvenda  FOR zsdt0051-tp_venda,        " TIpo de Venda
                  p_zterm   FOR vbkd-zterm,               " Condição de Pagamento
                  p_zlsch   FOR vbkd-zlsch.               " Forma de Pagamento
SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_zterm-low.
  PERFORM busca_cond_pgto.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_zterm-high.
  PERFORM busca_cond_pgto.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: f_iniciar_variaves, " Cabeçalho
           f_seleciona_dados, " Form seleciona dados
           f_saida, " Form de saida
           f_imprime_dados.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM f_seleciona_dados.
  DATA : vg_tabix TYPE  sy-tabix,
         vdata    TYPE  bsid-zfbdt.

  IF ( p_zfbdt-high EQ p_zfbdt-low ).
    MESSAGE i000(z01) WITH 'A data final é igual a data inicial.'.
    STOP.
  ENDIF.
** Filtra Ordens pela data de vencimento
  SELECT  vbak~vbeln bsid~zfbdt vbfa~vbtyp_n vbfa~vbtyp_v zbd1t
    INTO TABLE it_vbak2
    FROM bsid
   INNER JOIN bkpf ON bkpf~bukrs = bsid~bukrs
                  AND bkpf~belnr = bsid~belnr
   INNER JOIN vbfa ON vbfa~vbeln = bkpf~awkey
   INNER JOIN vbak ON vbak~vbeln = vbfa~vbelv
   WHERE vbak~vkorg IN p_vkorg
     AND vbak~kunnr IN p_kunnr
     AND vbak~vbeln IN p_vbeln.

  SELECT  vbak~vbeln bsad~zfbdt vbfa~vbtyp_n vbfa~vbtyp_v zbd1t
    INTO TABLE it_vbak3
    FROM bsad
   INNER JOIN bkpf ON bkpf~bukrs = bsad~bukrs
                  AND bkpf~belnr = bsad~belnr
   INNER JOIN vbfa ON vbfa~vbeln = bkpf~awkey
   INNER JOIN vbak ON vbak~vbeln = vbfa~vbelv
   WHERE vbak~vkorg IN p_vkorg
     AND vbak~kunnr IN p_kunnr
     AND vbak~vbeln IN p_vbeln.

  LOOP AT it_vbak2 INTO wa_vbak2.
    vdata  = wa_vbak2-zfbdt + wa_vbak2-zbd1t.
    wa_vbak2-zfbdt = vdata.
    MODIFY it_vbak2 INDEX sy-tabix FROM wa_vbak2 TRANSPORTING zfbdt.
  ENDLOOP.

  DELETE it_vbak2 WHERE zfbdt  NOT IN p_zfbdt.

  LOOP AT it_vbak3 INTO wa_vbak2.
    vdata  = wa_vbak2-zfbdt + wa_vbak2-zbd1t.
    IF vdata IN p_zfbdt.
      APPEND wa_vbak2 TO it_vbak2.
    ENDIF.
  ENDLOOP.

  DELETE it_vbak2 WHERE ( vbtyp_n NE 'M' AND vbtyp_n NE 'N' AND vbtyp_n NE 'H' AND vbtyp_n NE 'P' )
                  AND ( vbtyp_v NE 'C' AND vbtyp_v  NE 'M' AND vbtyp_v NE 'L' ).


  SELECT vbeln erdat auart knumv kunnr vkorg bukrs_vf waerk
    FROM vbak
    INTO TABLE it_vbak
    WHERE vkorg IN p_vkorg
    AND kunnr IN p_kunnr
    AND vbeln IN p_vbeln.

  IF p_auart[] IS NOT INITIAL.
    DELETE it_vbak WHERE auart NOT IN p_auart.
  ENDIF.

  IF it_vbak IS NOT INITIAL.
    "Tipo de Solicitação
    SELECT vbeln nro_sol_ov
      FROM zsdt0053
      INTO TABLE it_zsdt0053
      FOR ALL ENTRIES IN it_vbak
      WHERE vbeln EQ it_vbak-vbeln.

    IF it_zsdt0053 IS NOT INITIAL.
      SELECT nro_sol_ov tp_venda
        FROM zsdt0051
        INTO TABLE it_zsdt0051
         FOR ALL ENTRIES IN it_zsdt0053
       WHERE nro_sol_ov EQ it_zsdt0053-nro_sol_ov
         AND tp_venda IN p_tvenda.

      "Descrição Tipo Solicitação
      SELECT tp_venda bezei
        FROM zsdt0057
        INTO TABLE it_zsdt0057
         FOR ALL ENTRIES IN it_zsdt0051
       WHERE tp_venda  EQ it_zsdt0051-tp_venda .

    ENDIF.

    " KONV - Condições (dados de operação)
    SELECT FROM v_konv FIELDS knumv , kschl , kwert FOR ALL ENTRIES IN @it_vbak WHERE knumv EQ @it_vbak-knumv AND kschl = 'IBRX' INTO TABLE @it_konv .

    " VBAP - Documento de vendas: dados de item
    SELECT vbeln matnr arktx werks kwmeng vrkme
      FROM vbap
      INTO TABLE it_vbap
      FOR ALL ENTRIES IN it_vbak
    WHERE vbeln EQ it_vbak-vbeln
      AND matnr IN p_matnr
      AND werks IN p_werks.

    " KNA1 - Mestre de clientes (parte geral)
    SELECT kunnr name1
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_vbak
    WHERE kunnr EQ it_vbak-kunnr.

    " VBKD - Documento de vendas: dados comerciais
    SELECT vbeln zterm valdt zlsch            "Luis
        FROM vbkd
        INTO TABLE it_vbkd
        FOR ALL ENTRIES IN it_vbak
      WHERE vbeln EQ it_vbak-vbeln.

*    IF P_ZTERM IS NOT INITIAL.
*      DELETE IT_VBKD WHERE ZTERM  NOT IN P_ZTERM.
*    ENDIF.

    " VBFA -Fluxo de documentos de vendas e distribuição
    SELECT vbeln vbtyp_n vbtyp_v vbelv rfmng matnr meins erdat
      FROM vbfa
      INTO TABLE it_vbfa_aux
      FOR ALL ENTRIES IN it_vbak
    WHERE vbelv EQ it_vbak-vbeln
      AND vbtyp_n IN ('M' , 'N','H' , 'P' )
      AND vbtyp_v IN ('C','M' , 'L' ).

    IF it_vbfa_aux IS NOT INITIAL. "ALRS
      SELECT vbeln vbtyp_n vbtyp_v vbelv rfmng matnr meins erdat
         FROM vbfa
         INTO TABLE it_vbfa_aux_v
         FOR ALL ENTRIES IN it_vbfa_aux
       WHERE vbelv   EQ it_vbfa_aux-vbeln
         AND vbtyp_n EQ 'O'
         AND vbtyp_v EQ 'H'.

      LOOP AT it_vbfa_aux INTO wa_vbfa_aux.
        vg_tabix = sy-tabix.
        READ TABLE it_vbfa_aux_v INTO wa_vbfa_aux_v WITH KEY vbelv = wa_vbfa_aux-vbeln.
        IF sy-subrc IS INITIAL.
          "CLEAR:  WA_VBFA_AUX-VBELN  .
          wa_vbfa_aux-vbeln = wa_vbfa_aux_v-vbeln.

          MODIFY it_vbfa_aux INDEX vg_tabix FROM wa_vbfa_aux TRANSPORTING vbeln.
        ENDIF.
      ENDLOOP.

      CLEAR: wa_vbfa_aux.

      " Conversão VBFA-VBELN PARA BKPF-AWKEY
      LOOP AT it_vbfa_aux INTO wa_vbfa_aux.
        wa_vbfa_aux-vbeln_awkey = wa_vbfa_aux-vbeln.
        wa_vbfa_aux-gjahr       = wa_vbfa_aux-erdat(4).
        APPEND wa_vbfa_aux TO it_vbfa.
      ENDLOOP.

      " BKPF - Cabeçalho do documento contábil
      SELECT bukrs belnr gjahr awtyp awkey budat
        FROM bkpf
        INTO TABLE it_bkpf_aux
        FOR ALL ENTRIES IN it_vbfa
      WHERE awkey EQ it_vbfa-vbeln_awkey
        AND gjahr EQ it_vbfa-gjahr.
    ENDIF.
  ENDIF.

  IF it_bkpf_aux IS NOT INITIAL.

    " BSID - Contabilidade: índice secundário para clientes
    SELECT budat bldat xblnr blart gsber dmbtr dmbe2 bukrs zfbdt zbd1t gjahr belnr kunnr vbeln shkzg
      FROM bsid
      INTO TABLE it_bsid_aux
      FOR ALL ENTRIES IN it_bkpf_aux
    WHERE belnr EQ it_bkpf_aux-belnr
      AND bukrs EQ it_bkpf_aux-bukrs
      AND gjahr EQ it_bkpf_aux-gjahr.

    LOOP AT it_bsid_aux INTO wa_bsid_aux.

      wa_bsid-budat = wa_bsid_aux-budat.
      wa_bsid-bldat = wa_bsid_aux-bldat.
      wa_bsid-xblnr = wa_bsid_aux-xblnr.
      wa_bsid-blart = wa_bsid_aux-blart.
      wa_bsid-gsber = wa_bsid_aux-gsber.
      wa_bsid-dmbtr = wa_bsid_aux-dmbtr.
      wa_bsid-dmbe2 = wa_bsid_aux-dmbe2.
      wa_bsid-bukrs = wa_bsid_aux-bukrs.
      wa_bsid-zfbdt = wa_bsid_aux-zfbdt.
      wa_bsid-zbd1t = wa_bsid_aux-zbd1t.
      wa_bsid-gjahr = wa_bsid_aux-gjahr.
      wa_bsid-belnr = wa_bsid_aux-belnr.
      wa_bsid-kunnr = wa_bsid_aux-kunnr.
      wa_bsid-vbeln = wa_bsid_aux-vbeln.
      wa_bsid-shkzg = wa_bsid_aux-shkzg.

      wa_bsid-data  = wa_bsid_aux-zfbdt + wa_bsid_aux-zbd1t.
      APPEND wa_bsid TO it_bsid.
      CLEAR: wa_bsid_aux, wa_bsid..

    ENDLOOP.

    SELECT budat bldat xblnr blart gsber dmbtr dmbe2 bukrs zfbdt zbd1t gjahr belnr kunnr vbeln shkzg augbl
      FROM bsad
      INTO TABLE it_bsad_aux
      FOR ALL ENTRIES IN it_bkpf_aux
    WHERE belnr EQ it_bkpf_aux-belnr
      AND bukrs EQ it_bkpf_aux-bukrs
      AND gjahr EQ it_bkpf_aux-gjahr.

    LOOP AT it_bsad_aux INTO wa_bsad_aux.

      wa_bsad-budat = wa_bsad_aux-budat.
      wa_bsad-bldat = wa_bsad_aux-bldat.
      wa_bsad-xblnr = wa_bsad_aux-xblnr.
      wa_bsad-blart = wa_bsad_aux-blart.
      wa_bsad-gsber = wa_bsad_aux-gsber.
      wa_bsad-dmbtr = wa_bsad_aux-dmbtr.
      wa_bsad-dmbe2 = wa_bsad_aux-dmbe2.
      wa_bsad-bukrs = wa_bsad_aux-bukrs.
      wa_bsad-zfbdt = wa_bsad_aux-zfbdt.
      wa_bsad-zbd1t = wa_bsad_aux-zbd1t.
      wa_bsad-gjahr = wa_bsad_aux-gjahr.
      wa_bsad-belnr = wa_bsad_aux-belnr.
      wa_bsad-kunnr = wa_bsad_aux-kunnr.
      wa_bsad-vbeln = wa_bsad_aux-vbeln.
      wa_bsad-shkzg = wa_bsad_aux-shkzg.
      wa_bsad-augbl = wa_bsad_aux-augbl.

      wa_bsad-data  = wa_bsad_aux-zfbdt + wa_bsad_aux-zbd1t.
      APPEND wa_bsad TO it_bsad.

      CLEAR: wa_bsad_aux.

    ENDLOOP.

    SORT it_vbfa      BY vbeln.

*---> 04/07/2023 - Migração S4 - WS
    SORT it_bsad_aux BY bukrs augbl kunnr gjahr.
*<--- 04/07/2023 - Migração S4 - WS

    " Verifica se há saldo em outro documento na BSID 28/09/2012 ALRS
    DELETE ADJACENT DUPLICATES FROM it_bsad_aux COMPARING bukrs augbl kunnr gjahr.
    IF it_bsad_aux[] IS NOT INITIAL.
      REFRESH it_bsid_aux.

      SELECT budat bldat xblnr blart gsber dmbtr dmbe2 bukrs zfbdt zbd1t gjahr belnr kunnr vbeln shkzg
        INTO TABLE it_bsid_aux
        FROM bsid
         FOR ALL ENTRIES IN it_bsad_aux
       WHERE bukrs EQ it_bsad_aux-bukrs
         AND belnr EQ it_bsad_aux-augbl
         AND kunnr EQ it_bsad_aux-kunnr
         AND gjahr EQ it_bsad_aux-gjahr.

      LOOP AT it_bsid_aux INTO wa_bsid_aux.
        wa_bsid-budat = wa_bsid_aux-budat.
        wa_bsid-bldat = wa_bsid_aux-bldat.
        wa_bsid-xblnr = wa_bsid_aux-xblnr.
        wa_bsid-blart = wa_bsid_aux-blart.
        wa_bsid-gsber = wa_bsid_aux-gsber.
        wa_bsid-dmbtr = wa_bsid_aux-dmbtr.
        wa_bsid-dmbe2 = wa_bsid_aux-dmbe2.
        wa_bsid-bukrs = wa_bsid_aux-bukrs.
        wa_bsid-zfbdt = wa_bsid_aux-zfbdt.
        wa_bsid-zbd1t = wa_bsid_aux-zbd1t.
        wa_bsid-gjahr = wa_bsid_aux-gjahr.
        wa_bsid-belnr = wa_bsid_aux-belnr.
        wa_bsid-kunnr = wa_bsid_aux-kunnr.
        wa_bsid-vbeln = wa_bsid_aux-vbeln.
        wa_bsid-shkzg = wa_bsid_aux-shkzg.

        wa_bsid-data  = wa_bsid_aux-zfbdt + wa_bsid_aux-zbd1t.
        APPEND wa_bsid TO it_bsid.
        " BKPF - Cabeçalho do documento contábil
        SELECT SINGLE bukrs belnr gjahr awtyp awkey budat
        FROM bkpf
          INTO wa_bkpf
          WHERE bukrs EQ wa_bsid_aux-bukrs
          AND   belnr EQ wa_bsid_aux-belnr
          AND   gjahr EQ wa_bsid_aux-gjahr.

        APPEND wa_bkpf TO it_bkpf_aux.

        " VBFA -Fluxo de documentos de vendas e distribuição
        READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbeln = wa_bsid_aux-vbeln BINARY SEARCH.
        wa_vbfa-vbeln_awkey =  wa_bkpf-awkey.
        wa_vbfa-flag_bsid   = 'S'.
        APPEND wa_vbfa TO it_vbfa.

        CLEAR: wa_bsid_aux, wa_bsid.

      ENDLOOP.
    ENDIF.

    " Conversão do campo AWKEY(20) para tamanho 35 (REFKEY).
    LOOP AT it_bkpf_aux INTO wa_bkpf_aux.
      wa_bkpf_aux-awkey_aux = wa_bkpf_aux-awkey.
      APPEND wa_bkpf_aux TO it_bkpf.
    ENDLOOP.

    " J_1BNFLIN - Partidas individuais da nota fiscal
    SELECT refkey docnum
      FROM j_1bnflin
      INTO TABLE it_j_1bnflin
      FOR ALL ENTRIES IN it_bkpf
    WHERE refkey EQ it_bkpf-awkey_aux.

    IF ( sy-subrc = 0 ).
      " J_1BNFDOC - Cabeçalho da nota fiscal
      SELECT docnum nfe nfenum nfnum
        FROM j_1bnfdoc
        INTO TABLE it_j_1bnfdoc
        FOR ALL ENTRIES IN it_j_1bnflin
      WHERE docnum EQ it_j_1bnflin-docnum
        AND nfe EQ 'X'.

    ENDIF.

  ENDIF.

  it_vbfa2[] = it_vbfa[].
  DELETE it_vbfa2 WHERE flag_bsid   NE 'S'.

  LOOP AT it_vbfa2 INTO wa_vbfa.
    CLEAR wa_vbak2.
    wa_vbak2-vbeln = wa_vbfa-vbelv.
    APPEND wa_vbak2 TO it_vbak2.
  ENDLOOP.

  it_vbkd2[] = it_vbkd[].
  DELETE it_vbkd2 WHERE valdt NOT IN p_zfbdt.
  CHECK it_vbkd2[] IS NOT INITIAL.

  LOOP AT it_vbkd2 INTO wa_vbkd.
    CLEAR wa_vbak2.
    wa_vbak2-vbeln = wa_vbkd-vbeln.
    APPEND wa_vbak2 TO it_vbak2.
  ENDLOOP.

  CHECK it_vbak2[] IS NOT INITIAL.
  REFRESH it_vbak.
*---> 04/07/2023 - Migração S4 - WS
  SORT it_vbak2 BY vbeln.
*<--- 04/07/2023 - Migração S4 - WS
  DELETE ADJACENT DUPLICATES FROM it_vbak2 COMPARING vbeln.
  SELECT vbeln erdat auart knumv kunnr vkorg bukrs_vf waerk
   FROM vbak
   INTO TABLE it_vbak
    FOR ALL ENTRIES IN it_vbak2
    WHERE vbeln EQ it_vbak2-vbeln.

*  IF  NOT P_ZFBDT IS INITIAL.
*    DELETE IT_BSID WHERE DATA NOT IN P_ZFBDT.
*    DELETE IT_BSAD WHERE DATA NOT IN P_ZFBDT.
*  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
FORM f_saida.
  DATA: menge     TYPE ekpo-menge,
        contador  TYPE i,
        sel_valor TYPE i,
        var_vbeln TYPE vbak-vbeln,
        valoraux  TYPE bsid-dmbtr,
        txdolar   TYPE bsid-dmbtr,
        valord    TYPE bsid-dmbe2,
        valordev  TYPE bsid-dmbtr,
        valordevd TYPE bsid-dmbe2,
        quant     TYPE vbfa-rfmng,
        quanttot  TYPE vbap-kwmeng,
        valortot  TYPE konv-kwert,
        moeda     TYPE vbak-waerk.

  SORT: it_vbak      BY vbeln,
        it_vbap      BY vbeln,
        it_kna1      BY kunnr,
        it_vbfa      BY vbelv,
        it_vbkd      BY vbeln,
        it_bkpf      BY awkey belnr,
        it_bsid      BY belnr,
        it_bsad      BY belnr,
        it_konv      BY knumv,
        it_j_1bnflin BY refkey,
        it_j_1bnfdoc BY docnum,
        it_zsdt0051  BY nro_sol_ov,
        it_zsdt0053  BY vbeln,
        it_zsdt0057  BY tp_venda.

  DELETE ADJACENT DUPLICATES FROM it_vbfa COMPARING vbeln vbeln_awkey.

  LOOP AT it_vbak INTO wa_vbak.

    CLEAR: wa_saida_aux-qtd_aux,
           wa_saida_aux-waerk,
           wa_vbfa,
           wa_bkpf,
           wa_bsid,
           wa_bsad,
           wa_vbap,
           wa_kna1,
           wa_vbkd,
           wa_konv,
           wa_j_1bnflin,
           wa_j_1bnfdoc,
           wa_saida_aux,
           wa_zsdt0053,
           wa_zsdt0051,
           wa_zsdt0057,
           contador.

    READ TABLE it_zsdt0053 INTO wa_zsdt0053 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_saida_aux-nro_sol_ov = wa_zsdt0053-nro_sol_ov.

      "Busca Tabela de Solicitação Ordem de Venda - Cabeçalho
      READ TABLE it_zsdt0051 INTO wa_zsdt0051 WITH KEY nro_sol_ov = wa_zsdt0053-nro_sol_ov BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_saida_aux-tp_venda = wa_zsdt0051-tp_venda.
        IF wa_saida_aux-tp_venda IS NOT INITIAL.
          READ TABLE it_zsdt0057 INTO wa_zsdt0057 WITH KEY tp_venda = wa_zsdt0051-tp_venda BINARY SEARCH.
          wa_saida_aux-bezei = wa_zsdt0057-bezei.
        ENDIF.

      ELSEIF p_tvenda IS NOT INITIAL.
        CONTINUE.
      ENDIF.

    ELSEIF p_tvenda IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv = wa_vbak-vbeln.

      ADD 1 TO contador.
      READ TABLE it_bkpf INTO wa_bkpf WITH KEY awkey = wa_vbfa-vbeln_awkey BINARY SEARCH.

      IF ( sy-subrc = 0 ).
        wa_saida_aux-awkey = wa_bkpf-awkey. " Referência ao documento de origem
      ENDIF.

      sel_valor = 0.

      READ TABLE it_bsid INTO wa_bsid WITH KEY belnr = wa_bkpf-belnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF  ( NOT p_zfbdt IS INITIAL ) AND ( wa_bsid-data NOT IN p_zfbdt ).
          CONTINUE.
        ENDIF.
        sel_valor = 1.
        wa_saida_aux-budat = wa_bsid-budat. " Data de lançamento no documento
        wa_saida_aux-bldat = wa_bsid-bldat. " Data no documento
        wa_saida_aux-venc  = wa_bsid-data.  " Data do Vencimento
        wa_saida_aux-dmbtr = wa_bsid-dmbtr. " Montante em moeda interna
        wa_saida_aux-dmbe2 = wa_bsid-dmbe2. " Montante na 2ª moeda interna
        wa_saida_aux-belnr = wa_bsid-belnr. " Nº documento de um documento contábil
        wa_saida_aux-gjahr = wa_bsid-gjahr. " Exercício
        wa_saida_aux-tipot = 'BSID'.        " De onde vem o valor

        IF wa_bsid-shkzg = 'H'.
          wa_saida_aux-dmbtr = wa_saida_aux-dmbtr * -1.
          wa_saida_aux-dmbe2 = wa_saida_aux-dmbe2 * -1.
        ENDIF.

        wa_saida_aux-valor   = wa_saida_aux-valor   + wa_saida_aux-dmbtr. " Valor em Moeda Real
        wa_saida_aux-valord  = wa_saida_aux-valord  + wa_saida_aux-dmbe2. " Valor em Dolar

      ELSE.
        READ TABLE it_bsad INTO wa_bsad WITH KEY belnr = wa_bkpf-belnr BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          IF  ( NOT p_zfbdt IS INITIAL ) AND ( wa_bsad-data NOT IN p_zfbdt ).
            CONTINUE.
          ENDIF.

          sel_valor = 1.
          wa_saida_aux-budat = wa_bsad-budat.
          wa_saida_aux-bldat = wa_bsad-bldat.
          wa_saida_aux-venc  = wa_bsad-data.
          wa_saida_aux-dmbtr = wa_bsad-dmbtr.
          wa_saida_aux-dmbe2 = wa_bsad-dmbe2.
          wa_saida_aux-belnr = wa_bsad-belnr.
          wa_saida_aux-gjahr = wa_bsad-gjahr.
          wa_saida_aux-tipot = 'BSAD'. " De onde vem o valor.

          " Conforme solicitação
          IF wa_bsad-shkzg = 'H'.
            wa_saida_aux-dmbtr = wa_saida_aux-dmbtr * -1.
            wa_saida_aux-dmbe2 = wa_saida_aux-dmbe2 * -1.
          ENDIF.

          wa_saida_aux-valor   = wa_saida_aux-valor   + wa_saida_aux-dmbtr.
          wa_saida_aux-valord  = wa_saida_aux-valord  + wa_saida_aux-dmbe2.

        ENDIF.

      ENDIF.

      IF sel_valor = 0.
        CONTINUE.
      ENDIF.

      IF ( ( NOT p_vbeln[] IS INITIAL ) AND ( wa_vbak-vbeln NOT IN p_vbeln ) ).
        CONTINUE.
      ENDIF.

      READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      IF ( ( NOT p_werks[] IS INITIAL ) AND ( wa_vbap-werks NOT IN p_werks ) ).
        CONTINUE.
      ENDIF.

      IF ( ( NOT p_matnr IS INITIAL ) AND ( wa_vbap-matnr NOT IN p_matnr ) ).
        CONTINUE.
      ENDIF.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      IF ( ( NOT p_zterm[] IS INITIAL ) AND ( wa_vbkd-zterm NOT IN p_zterm ) ).
        CONTINUE.
      ENDIF.

      IF ( wa_vbkd-zlsch NOT IN p_zlsch ).
        CONTINUE.
      ENDIF.

      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv BINARY SEARCH.

      menge = 0.

      IF  ( wa_vbfa-vbtyp_n = 'N') .
        wa_saida_aux-rfmng =  wa_saida_aux-rfmng * -1.
      ENDIF.

      IF  ( wa_vbfa-vbtyp_n = 'H') .
        wa_saida_aux-dev =  wa_vbfa-vbtyp_n.
      ENDIF.

      wa_saida_aux-waerk  = wa_vbak-waerk.          " Moeda
      wa_saida_aux-vbeln  = wa_vbak-vbeln.          " Documento de vendas
      wa_saida_aux-kunnr  = wa_vbak-kunnr.          " Emissor da ordem (Cliente)
      wa_saida_aux-vkorg  = wa_vbak-vkorg.          " Organização de vendas
      wa_saida_aux-rfmng  = wa_vbfa-rfmng.          " Quantidade.

      IF wa_vbfa-flag_bsid = 'S'.
        wa_saida_aux-awkey  = wa_vbfa-vbeln.        " Documento de vendas e distribuição subseqüente
      ELSE.
        wa_saida_aux-awkey  = wa_vbfa-vbeln_awkey.  " Documento de vendas e distribuição subseqüente
      ENDIF.

      wa_saida_aux-zterm  = wa_vbkd-zterm.          " Chave de condições de  pagamento
      wa_saida_aux-valdt  = wa_vbkd-valdt.          " Data efetiva fixa
      wa_saida_aux-zlsch  = wa_vbkd-zlsch.          " Tipo de Pagamento           "Luis
      wa_saida_aux-name1  = wa_kna1-name1.          " Nome 1

      IF wa_vbfa-flag_bsid = 'S'.
*---> 09/06/2023 - Migração S4 - JS
*     wa_saida_aux-kwert  = wa_bsid-dmbtr.        " Valor BSID
        wa_saida_aux-kwert = CONV #( wa_bsid-dmbtr ).
*<--- 09/06/2023 - Migração S4 - JS
      ELSE.
        wa_saida_aux-kwert  = wa_konv-kwert.        " Valor condição
      ENDIF.

      wa_saida_aux-erdat  = wa_vbak-erdat.          " Data de criação do registro
      wa_saida_aux-kwmeng = wa_vbap-kwmeng.         " Quantidade da ordem acumulada em unidade de venda

      READ TABLE it_j_1bnflin INTO wa_j_1bnflin   WITH KEY refkey = wa_bkpf-awkey_aux   BINARY SEARCH.
      IF ( sy-subrc = 0 ).
        READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.
        IF ( wa_j_1bnfdoc-nfe EQ 'X' ).
          wa_saida_aux-nfe = wa_j_1bnfdoc-nfenum.
        ELSE.
          wa_saida_aux-nfe = wa_j_1bnfdoc-nfnum.
        ENDIF.

      ENDIF.

      APPEND wa_saida_aux TO it_saida_aux.

      CLEAR: wa_vbfa,
             wa_bkpf,
             wa_bsid,
             wa_bsad,
             wa_vbap,
             wa_kna1,
             wa_vbkd,
             wa_konv,
             wa_j_1bnflin,
             wa_j_1bnfdoc,
             wa_zsdt0051,
             wa_zsdt0053,
             wa_zsdt0057,
             wa_saida_aux.

    ENDLOOP.

    IF contador = 0. " sem faturamento (mostra apenas o valor da condição da OV ALRS
      READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.

      IF ( ( NOT p_werks[] IS INITIAL ) AND ( wa_vbap-werks NOT IN p_werks ) ).
        CONTINUE.
      ENDIF.

      IF ( ( NOT p_matnr IS INITIAL ) AND ( wa_vbap-matnr NOT IN p_matnr ) ).
        CONTINUE.
      ENDIF.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.

      IF ( ( NOT p_zterm[] IS INITIAL ) AND ( wa_vbkd-zterm NOT IN p_zterm ) ).
        CONTINUE.
      ENDIF.

      IF ( wa_vbkd-zlsch NOT IN p_zlsch ).
        CONTINUE.
      ENDIF.

      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv BINARY SEARCH.

      IF  ( NOT p_zfbdt IS INITIAL ) AND ( wa_vbkd-valdt NOT IN p_zfbdt ).
        CONTINUE.
      ENDIF.

      IF wa_konv-kwert > 0.
        wa_saida_aux-waerk  = wa_vbak-waerk.          " Moeda
        wa_saida_aux-vbeln  = wa_vbak-vbeln.          " Documento de vendas ok
        wa_saida_aux-kunnr  = wa_vbak-kunnr.          " Emissor da ordem (Cliente) ok
        wa_saida_aux-vkorg  = wa_vbak-vkorg.          " Organização de vendas
        wa_saida_aux-rfmng  = 0.                      " Quantidade.
        wa_saida_aux-zterm  = wa_vbkd-zterm.          " Chave de condições de  pagamento
        wa_saida_aux-valdt  = wa_vbkd-valdt.          " Data efetiva fixa
        wa_saida_aux-venc   = wa_vbkd-valdt.          " Data efetiva fixa
        wa_saida_aux-zlsch  = wa_vbkd-zlsch.          " Tipo de Pagamento             "Luis
        wa_saida_aux-name1  = wa_kna1-name1.                  " Nome 1 ok
        wa_saida_aux-kwert  = wa_konv-kwert.          " Valor condição
        wa_saida_aux-erdat  = wa_vbak-erdat.          " Data de criação do registro
        wa_saida_aux-tipot = 'SEMF'.                  " De onde vem o valor.
        APPEND wa_saida_aux TO it_saida_aux.

      ENDIF.

    ENDIF.

  ENDLOOP.

  SORT:  it_saida_aux BY vbeln.

  MOVE it_saida_aux[] TO it_saida2[].

  SORT: it_saida2   BY vbeln.

  DELETE ADJACENT DUPLICATES FROM it_saida2 COMPARING vbeln.

  LOOP AT it_saida2 INTO wa_saida2.

    LOOP AT it_saida_aux INTO wa_saida_aux WHERE vbeln = wa_saida2-vbeln.

      moeda             = wa_saida_aux-waerk.

      "Valores Linhas Finais
      quanttot =  wa_saida_aux-kwmeng.
      valortot =  wa_saida_aux-kwert .

      " Somas os valores de devolução
      IF wa_saida_aux-dev = 'H'.
        valordev  =  valordev  + wa_saida_aux-valor.
        valordevd =  valordevd + wa_saida_aux-valord.
      ENDIF.

      IF wa_saida_aux-tipot = 'BSID'.
        MOVE wa_saida_aux TO wa_saida.
        wa_saida-tipo   = msgsd.

*      "Valores BSID Aplicados por linha.
        valoraux =  valoraux + wa_saida_aux-valor.
        valord   =  valord   + wa_saida_aux-valord.
        "quant    =  quant    +  wa_saida_aux-rfmng.
******
        APPEND wa_saida TO it_saida.
        CLEAR: wa_saida.
      ELSE.

        wa_linha_um-name1 = wa_saida_aux-name1.
        wa_linha_um-vbeln = wa_saida_aux-vbeln.
        "wa_linha_um-belnr = wa_saida_aux-belnr. Conforme solicitado email comentado esses campos
        "wa_linha_um-awkey = wa_saida_aux-awkey. Conforme solicitado email comentado esses campos
        "wa_linha_um-budat = wa_saida_aux-budat. Conforme solicitado email comentado esses campos
        IF wa_saida_aux-venc IS INITIAL.
          wa_linha_um-venc  = wa_saida_aux-valdt.
        ELSE.
          wa_linha_um-venc  = wa_saida_aux-venc.
        ENDIF.
        wa_linha_um-kunnr = wa_saida_aux-kunnr.
        "wa_linha_um-bldat = wa_saida_aux-erdat. Conforme solicitado email comentado esses campos
        wa_linha_um-venc  = wa_saida_aux-valdt.
        wa_linha_um-zterm = wa_saida_aux-zterm.
        wa_linha_um-zlsch = wa_saida_aux-zlsch.         "Luis
        wa_linha_um-waerk = wa_saida_aux-waerk.
        wa_linha_um-tipo  = msgov.
        moeda             = wa_saida_aux-waerk.

        valoraux =  valoraux + wa_saida_aux-valor.
        valord   =  valord   + wa_saida_aux-valord.
        "quant    =  quant    + wa_saida_aux-rfmng.

      ENDIF.

    ENDLOOP.

    " Sai do LOOP mostra resultado final
    "APPEND wa_saida TO it_saida.

    " Calculo para moeda USD e Calculo para moeda BRL.
    wa_linha_um-name1       = wa_saida_aux-name1.
    wa_linha_um-vbeln       = wa_saida_aux-vbeln.
    "wa_linha_um-belnr       = wa_saida_aux-belnr. Conforme solicitado email comentado esses campos
    "wa_linha_um-awkey       = wa_saida_aux-awkey. Conforme solicitado email comentado esses campos
    "wa_linha_um-budat       = wa_saida_aux-budat. Conforme solicitado email comentado esses campos
    wa_linha_um-venc        = wa_saida_aux-venc.
    wa_linha_um-kunnr       = wa_saida_aux-kunnr.
    "wa_linha_um-bldat       = wa_saida_aux-erdat. Conforme solicitado email comentado esses campos
    wa_linha_um-venc        = wa_saida_aux-valdt.
    wa_linha_um-zterm       = wa_saida_aux-zterm.
    wa_linha_um-zlsch       = wa_saida_aux-zlsch.         "Luis
    wa_linha_um-waerk       = wa_saida_aux-waerk.
    wa_linha_um-tipo        = msgov.
    wa_linha_um-nro_sol_ov  = wa_saida_aux-nro_sol_ov.
    wa_linha_um-tp_venda    = wa_saida_aux-tp_venda.
    wa_linha_um-bezei       = wa_saida_aux-bezei.

    IF moeda = 'BRL'.
      wa_linha_um-dmbtr = valortot - valoraux + valordev  .
      "wa_linha_um-rfmng = quanttot - quant.
      txdolar = valoraux / valord.
      IF txdolar IS NOT INITIAL.
        wa_linha_um-dmbe2 = wa_linha_um-dmbtr / txdolar.
      ENDIF.
    ELSE.
      IF valord > 0.
        txdolar = valoraux / valord.
      ELSE.
        txdolar = 0.
      ENDIF.
      wa_linha_um-dmbtr = ( valortot * txdolar ) - valoraux + valordev  . "ALRS
*      wa_linha_um-dmbtr = valortot - valord + valordevd.
      " wa_linha_um-rfmng = quanttot - quant.
      wa_linha_um-dmbe2 = valortot - valord.
    ENDIF.

    "IF NOT wa_linha_um-dmbtr IS INITIAL.
    "Imprimir Totais ultimo valor que passa pelo loop
    APPEND wa_linha_um TO it_saida.
    CLEAR: wa_linha_um,
           var_vbeln  ,
           valoraux   ,
           valord     ,
           quant      ,
           quanttot   ,
           valortot   ,
           txdolar    ,
           moeda      ,
           valordev   ,
           valordevd  .
    "ENDIF.
  ENDLOOP.

***************************************************************************************
*  LOOP AT IT_VBAK INTO WA_VBAK.
*    CLEAR:WA_SAIDA.
*    READ TABLE IT_ZSDT0053 INTO WA_ZSDT0053 WITH KEY VBELN = WA_VBAK-VBELN BINARY SEARCH.
*    IF SY-SUBRC IS INITIAL.
*      WA_SAIDA-NRO_SOL_OV = WA_ZSDT0053-NRO_SOL_OV.
*   endif.
*
*      READ TABLE IT_ZSDT0051 INTO WA_ZSDT0051 WITH KEY NRO_SOL_OV =  WA_ZSDT0053-NRO_SOL_OV BINARY SEARCH.
*      IF SY-SUBRC IS INITIAL.
*        WA_SAIDA-TP_VENDA = WA_ZSDT0051-TP_VENDA.
*        READ TABLE IT_ZSDT0057 INTO WA_ZSDT0057 WITH KEY TP_VENDA = WA_ZSDT0051-TP_VENDA BINARY SEARCH.
*        IF SY-SUBRC IS INITIAL.
*          WA_SAIDA-BEZEI = WA_ZSDT0057-BEZEI.
*        ENDIF.
*      ENDIF.
*    APPEND WA_SAIDA TO IT_SAIDA.
*  ENDLOOP.
********************************************************************************************************
ENDFORM.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
FORM f_iniciar_variaves .

  DATA:
    w_texto1(10),
    w_texto2(10),
    w_texto3(40),

    w_empresa_texto(40),
    w_cliente_texto(40),
    w_ov_texto(40),
    w_centro_texto(40),
    w_material_texto(40),
    w_periodo_texto(40),

    empresa              TYPE c LENGTH 50,
    cliente              TYPE c LENGTH 50,
    material             TYPE c LENGTH 50,
    tipo_ordem           TYPE c LENGTH 200,
    centro               TYPE c LENGTH 50,
    periodo              TYPE c LENGTH 200.


  v_report = sy-repid.

  w_texto3 = 'Relatório Fluxo de Caixa de Recebimentos'.
  PERFORM f_construir_cabecalho USING 'H' w_texto3.

  IF ( p_vkorg NE '' ).
    w_empresa_texto = 'Empresa:'.

    CONCATENATE w_empresa_texto p_vkorg-low INTO empresa SEPARATED BY space.

    PERFORM f_construir_cabecalho USING 'S' empresa.
  ENDIF.

  IF ( p_auart NE '' ).
    w_ov_texto = 'Tipo de O.V:'.
    CONCATENATE w_ov_texto p_auart-low INTO tipo_ordem SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' tipo_ordem.
  ENDIF.

  IF ( p_werks NE '' ).
    w_centro_texto = 'Centro:'.
    CONCATENATE w_centro_texto p_werks-low INTO centro SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' centro.
  ENDIF.

  IF ( p_kunnr NE '' ).
    w_cliente_texto = 'Cliente:'.
    CONCATENATE w_cliente_texto p_kunnr-low INTO cliente SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' cliente.
  ENDIF.

  IF ( p_matnr NE '' ).
    w_material_texto = 'Material:'.
    CONCATENATE w_material_texto p_matnr-low INTO material SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' material.
  ENDIF.

  IF ( NOT  p_zfbdt IS INITIAL ).
    w_periodo_texto = 'Período:  '.
    CONCATENATE p_zfbdt-low+6(2)   '.' p_zfbdt-low+4(2)  '.' p_zfbdt-low(4)  INTO w_texto1.
    CONCATENATE p_zfbdt-high+6(2)  '.' p_zfbdt-high+4(2) '.' p_zfbdt-high(4) INTO w_texto2.
    CONCATENATE w_periodo_texto w_texto1 ' - ' w_texto2 INTO periodo  SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' periodo.
  ENDIF.

ENDFORM.                    " F_INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " Z_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
FORM f_imprime_dados.

  IF it_saida[] IS INITIAL.
    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.

  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = it_fcat[]
*     it_sort                 = t_sort[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
    TABLES
      t_outtab                = it_saida.


ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
FORM f_definir_eventos.

  PERFORM f_carregar_eventos USING: slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
FORM f_carregar_eventos  USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " F_CARREGAR_EVENTOS

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
FORM f_alv_sort .

ENDFORM.                    " F_ALV_SORT

*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
FORM f_user_command USING l_ucomm  l_selfield TYPE slis_selfield.

  READ TABLE it_saida INTO wa_saida INDEX l_selfield-tabindex.

  CASE l_selfield-fieldname.

    WHEN 'VBELN'.
      SET PARAMETER ID 'AUN' FIELD wa_saida-vbeln.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    WHEN 'BELNR'.
      SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
      SET PARAMETER ID 'BUK' FIELD wa_saida-vkorg.
      SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN 'AWKEY'.
      SET PARAMETER ID 'VF' FIELD wa_saida-awkey.
      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

  ENDCASE.
ENDFORM.                    "f_user_command

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM f_alv .
*
  PERFORM alv_preenche_cat USING:
          'KUNNR'       TEXT-011        '10'       ' '     ' '    ' ' , " Cliente
          'NAME1'       TEXT-012        '38'       ' '     ' '    ' ' , " Nome do Cliente
          'VBELN'       TEXT-013        '14'       'X'     ' '    ' ' , " Ordem de Venda
          'BELNR'       TEXT-014        '13'       'X'     ' '    ' ' , " Doc. Contabil
          'AWKEY'       TEXT-015        '16'       'X'     ' '    ' ' , " Doc. Faturamento
          'NFE'         TEXT-016        '11'       ' '     ' '    ' ' , " Nota Fiscal
          'BUDAT'       TEXT-017        '15'       ' '     ' '    ' ' , " Data Lançamento
          'BLDAT'       TEXT-018        '14'       ' '     ' '    ' ' , " Data Documento
          'VENC'        TEXT-019        '11'       ' '     ' '    ' ' , " Vencimento
          'ZLSCH'       TEXT-029        '10'       ' '     ' '    ' ' , " Tipo de Pagamento
          'ZTERM'       TEXT-020        '15'       ' '     ' '    ' ' , " Cond. Pagamento
          'WAERK'       TEXT-026        '6'        ' '     ' '    ' ' , " Moeda
          "'RFMNG'       text-021        '15'       ' '     ' '    ' ' , " Quantidade
          'DMBTR'       TEXT-022        '13'       ' '     ' '    ' ' , " Valor R$
          'DMBE2'       TEXT-023        '13'       ' '     ' '    ' ' , " Valor U$
          'TIPO'        TEXT-025        '29'       ' '     ' '    ' ' , " Tipo
          'TP_VENDA'    TEXT-027        '08'       ' '     ' '    ' ' , " Tipo
          'BEZEI'       TEXT-028        '15'       ' '     ' '    ' ' . " Tipo
ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo  TYPE c
                               p_desc   TYPE c
                               p_tam    TYPE c
                               p_hot    TYPE c
                               p_zero   TYPE c
                               p_soma   TYPE c.


  DATA: wl_fcat TYPE ty_estrutura.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_l = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-do_sum    = p_soma.


  APPEND wl_fcat TO it_fcat.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
FORM f_construir_cabecalho    USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_COND_PGTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_cond_pgto .
  DATA: BEGIN OF tl_temp OCCURS 0,
          zterm TYPE /dsd/hh_zterm_v-zterm,
          vtext TYPE /dsd/hh_zterm_v-vtext,
        END OF tl_temp.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  SELECT DISTINCT zterm vtext
    INTO TABLE tl_temp
    FROM /dsd/hh_zterm_v
   WHERE spras EQ sy-langu
   ORDER BY zterm.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'ZTERM'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'ZTERM'
        value_org       = 'S'
      TABLES
        value_tab       = tl_temp
        return_tab      = tl_return_tab
        dynpfld_mapping = tl_dselc.

  ENDIF.

ENDFORM.                    " BUSCA_COND_PGTO
