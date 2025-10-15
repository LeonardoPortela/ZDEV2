************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 07.02.2011                                          *
* Objetivo    ...: Job Seleção Inicial - Comparativo de Saída e Chegada*
* Autor       ...: Victor Hugo                                         *
************************************************************************

REPORT  zlesj0001.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zlest0039, vbrp, zsdt0001, lfa1, j_1bbranch.

*----------------------------------------------------------------------*
* ESTRUTURA DAS TABELAS
*----------------------------------------------------------------------*
TYPES:

  "TABELA J_1BNFDOC - Cabeçalho da nota fiscal
  BEGIN OF ty_j_1bnfdoc,
    docnum TYPE j_1bnfdoc-docnum, " Nº documento.
    series TYPE j_1bnfdoc-series, " Série.
    bukrs  TYPE j_1bnfdoc-bukrs,  " Empresa.
    branch TYPE j_1bnfdoc-branch, " Local de Negócio.
    parid  TYPE j_1bnfdoc-parid,  " Identificação do Parceiro (cliente, fornecedor, loc. negócio).
    nfenum TYPE j_1bnfdoc-nfenum, " Nº NF-e de nove posições
    docdat TYPE j_1bnfdoc-docdat, " Data do documento
    inco1  TYPE j_1bnfdoc-inco1,  " Incoterms parte 1
    partyp TYPE  j_1bnfdoc-partyp, " Tipo de parceiro nota fiscal
  END OF ty_j_1bnfdoc,

  "TABELA J_1BNFLIN - Partidas individuais da nota fiscal
  BEGIN OF ty_j_1bnflin,
    menge  TYPE j_1bnflin-menge,   " Quantidade
    meins  TYPE j_1bnflin-meins,   " Unidade de medida básica
    matnr  TYPE j_1bnflin-matnr,   " Nº do material
    refkey TYPE j_1bnflin-refkey,  " Referência ao documento de origem
    reftyp TYPE j_1bnflin-reftyp,  " Tipo referência
    docnum TYPE j_1bnflin-docnum,  " Partidas individuais da nota fiscal
    werks  TYPE  j_1bnflin-werks,
  END OF ty_j_1bnflin,

  "TABELA J_1BNFDOC - Cabeçalho da nota fiscal
  BEGIN OF ty_j_1bnf_doc_lin,
    docnum        TYPE j_1bnfdoc-docnum, " Nº documento.
    series        TYPE j_1bnfdoc-series, " Série.
    bukrs         TYPE j_1bnfdoc-bukrs,  " Empresa.
    branch        TYPE j_1bnfdoc-branch, " Local de Negócio.
    parid         TYPE j_1bnfdoc-parid,  " Identificação do Parceiro (cliente, fornecedor, loc. negócio).
    nfenum        TYPE j_1bnfdoc-nfenum, " Nº NF-e de nove posições
    docdat        TYPE j_1bnfdoc-docdat, " Data do documento
    inco1         TYPE j_1bnfdoc-inco1,  " Incoterms parte 1
    partyp        TYPE j_1bnfdoc-partyp, " Tipo de parceiro nota fiscal
    cancel        TYPE j_1bnfdoc-cancel, " Tipo de parceiro nota fiscal

    "TABELA J_1BNFLIN - Partidas individuais da nota fiscal
    menge         TYPE j_1bnflin-menge,   " Quantidade
    meins         TYPE j_1bnflin-meins,   " Unidade de medida básica
    matnr         TYPE j_1bnflin-matnr,   " Nº do material
    refkey        TYPE j_1bnflin-refkey,  " Referência ao documento de origem
    reftyp        TYPE j_1bnflin-reftyp,  " Tipo referência
    werks         TYPE j_1bnflin-werks,
    cfop          TYPE j_1bnflin-cfop,

    " Tabela Electronic Nota Fiscal: Actual Status
    cancel_active TYPE j_1bnfe_active-cancel,
    nfnum9        TYPE j_1bnfe_active-nfnum9,
    docsta        TYPE j_1bnfe_active-docsta,
    scssta        TYPE j_1bnfe_active-scssta,

  END OF ty_j_1bnf_doc_lin,

  "TABELA J_1BNFNAD - Parceiros nota fiscal
  BEGIN OF ty_j_1bnfnad,
    docnum TYPE j_1bnfnad-docnum, "Nº documento
    parid  TYPE j_1bnfnad-parid, " Identificação do parceiro (cliente, fornecedor, loc.negócio)
    parvw  TYPE j_1bnfnad-parvw, " Nota fiscal função parceiro
  END OF ty_j_1bnfnad,

  "TABELA ZLEST0039 - Comparativo de saidas e chegadas
  BEGIN OF ty_zlest0039,
    docnum_les       TYPE zlest0039-docnum, " Nº documento
    serie            TYPE zlest0039-serie, " Código da série
    bukrs            TYPE zlest0039-bukrs, " Empresa
    werks            TYPE zlest0039-werks, " Centro
    matnr            TYPE zlest0039-matnr, " Nº do material
    kunnr            TYPE zlest0039-kunnr, " Nº cliente 1
    nfenum           TYPE zlest0039-nfenum, " Nº NF-e de nove posições
    datasaida        TYPE zlest0039-datasaida, " Data de criação do registro
    pesosaida        TYPE zlest0039-pesosaida, " Peso líquido
    unidadesai       TYPE zlest0039-unidadesaida, " Unidade de medida básica
    status           TYPE zlest0039-status, " Status comparativo saidas e chegadas
    data             TYPE zlest0039-data, " Data de criação do registro
    hora             TYPE zlest0039-hora, " Hora
    formaconfirmacao TYPE zlest0039-formaconfirmacao, " Código de uma posição
    reftyp           TYPE zlest0039-reftyp, " Tipo referência
  END OF ty_zlest0039,

  "TABELA VBRP - Documento de faturamento: dados de item
  BEGIN OF ty_vbrp,
    vgbel TYPE vbrp-vgbel, " Nº documento do documento de referência
    aubel TYPE vbrp-aubel, " Documento de vendas
    vbeln TYPE vbrp-vbeln, " Documento de faturamento
  END OF ty_vbrp,


  "TABELA VBRP - Documento de faturamento: dados de item
  BEGIN OF ty_vbrp_fob,
    vgbel    TYPE vbrp-vgbel, " Nº documento do documento de referência
    aubel    TYPE vbrp-aubel, " Documento de vendas
    vbeln    TYPE vbrp-vbeln, " Documento de faturamento
    mandt    TYPE vbrp-mandt, "
    zpesagem TYPE vbak-zpesagem, "
  END OF ty_vbrp_fob,


  "TABELA ZSDT0001 - Informações para preenchimento dos dados de remessa. (OPUS)
  BEGIN OF ty_zsdt0001,
    placa_cav    TYPE zsdt0001-placa_cav, " Placa do Veículo
    doc_rem      TYPE zsdt0001-doc_rem, " Remessa AMAGGI
    vbeln        TYPE zsdt0001-vbeln, " Nº documento de vendas e distribuição
    tp_movimento TYPE zsdt0001-tp_movimento, " Tipo de Movimento
  END OF ty_zsdt0001,

  "TABELA MKPF - Cabeçalho do documento do material
  BEGIN OF ty_mkpf,
    mblnr    TYPE mkpf-mblnr, " Nº documento de material
    mjahr    TYPE mkpf-mjahr, " Ano do documento do material
    le_vbeln TYPE mkpf-le_vbeln, " Fornecimento
  END OF ty_mkpf,

  "TABELA KNA1 - Mestre de clientes (parte geral)
  BEGIN OF ty_kna1,
    stcd1 TYPE kna1-stcd1, " Nº ID fiscal 1
    kunnr TYPE kna1-kunnr, " Nº cliente 1
  END OF ty_kna1,

  BEGIN OF ty_lfa1,
    stcd1 TYPE lfa1-stcd1,
    lifnr TYPE lfa1-lifnr,
  END OF ty_lfa1,

  BEGIN OF ty_j_1bbranch,
    bukrs  TYPE j_1bbranch-bukrs,
    branch TYPE j_1bbranch-branch,
    stcd1  TYPE j_1bbranch-stcd1,

  END OF ty_j_1bbranch,

  "TABELA LIPS - Documento SD: fornecimento: dados de item
  BEGIN OF ty_lips,
    vbeln TYPE lips-vbeln, " Fornecimento
    vgbel TYPE lips-vgbel, " Nº documento do documento de referência
  END OF ty_lips,

  BEGIN OF ty_vbfa,
    vbelv   TYPE vbfa-vbelv,
    vbeln   TYPE vbfa-vbeln,
    vbtyp_v TYPE vbfa-vbtyp_v,
    vbtyp_n TYPE vbfa-vbtyp_n,
  END OF ty_vbfa,

  BEGIN OF ty_vbpa,
    vbeln TYPE vbpa-vbeln,
    parvw TYPE vbpa-parvw,
    kunnr TYPE vbpa-kunnr,
    lifnr TYPE vbpa-lifnr,
  END OF ty_vbpa,

  BEGIN OF ty_zlest0039_delete,
    docnum TYPE zlest0039-docnum,
  END OF ty_zlest0039_delete.

*  BEGIN OF ty_zlest0039_duplicacao,
*  datasaida    TYPE zlest0039-datasaida,
*  placa_cav    TYPE zlest0039-placa_cav,
*  status_placa TYPE zlest0039-status_placa,
*  END OF ty_zlest0039_duplicacao.


*----------------------------------------------------------------------*
* TABELAS INTERNAS
*----------------------------------------------------------------------*
DATA:
  it_j_1bnfdoc        TYPE TABLE OF ty_j_1bnfdoc,
  it_j_1bnflin        TYPE TABLE OF ty_j_1bnflin,
  it_j_1bnf_doc_lin   TYPE TABLE OF ty_j_1bnf_doc_lin,
  it_lfa1             TYPE TABLE OF ty_lfa1,
  it_j_1bbranch       TYPE TABLE OF ty_j_1bbranch,
  it_j_1bnfnad        TYPE TABLE OF ty_j_1bnfnad,
  it_vbrp             TYPE TABLE OF ty_vbrp,
  it_vbrp_fob         TYPE TABLE OF ty_vbrp_fob,
  it_vbrp_aux         TYPE TABLE OF ty_vbrp,
  it_zsdt0001         TYPE TABLE OF ty_zsdt0001,
  it_mkpf             TYPE TABLE OF ty_mkpf,
  it_lips             TYPE TABLE OF ty_lips,
  it_vbfa             TYPE TABLE OF ty_vbfa,
  it_vbpa             TYPE TABLE OF ty_vbpa,
  it_kna1             TYPE TABLE OF ty_kna1,
  wa_setleaf          TYPE setleaf,
  it_setleaf          LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE,
  cfops               TYPE RANGE OF j_1bnflin-cfop,
  it_zlest0039_delete TYPE TABLE OF ty_zlest0039_delete.



*----------------------------------------------------------------------*
* WORK ÁREA
*----------------------------------------------------------------------*
DATA:
  wa_j_1bnfdoc        TYPE ty_j_1bnf_doc_lin,
  wa_j_1bnflin        TYPE ty_j_1bnf_doc_lin,
  wa_j_1bnf_doc_lin   TYPE ty_j_1bnf_doc_lin,
  wa_zlest0039        TYPE ty_zlest0039,
  wa_kna1             TYPE ty_kna1,
  wa_lfa1             TYPE ty_lfa1,
  wa_j_1bbranch       TYPE ty_j_1bbranch,
  wa_j_1bnfnad        TYPE ty_j_1bnfnad,
  wa_vbrp             TYPE ty_vbrp,
  wa_vbrp_fob         TYPE ty_vbrp_fob,
  wa_zsdt0001         TYPE ty_zsdt0001,
  wa_mkpf             TYPE ty_mkpf,
  wa_lips             TYPE ty_lips,
  wa_vbfa             TYPE ty_vbfa,
  wa_vbpa             TYPE ty_vbpa,
  wa_cfops            TYPE lxhme_range_c10,
  wa_zlest0039_delete TYPE ty_zlest0039_delete.

DATA: data TYPE p.

DATA: zcl_util TYPE REF TO zcl_util.

SELECTION-SCREEN BEGIN OF BLOCK z001 WITH FRAME.
  SELECT-OPTIONS: p_docnum FOR zlest0039-docnum.
SELECTION-SCREEN END OF BLOCK z001.

*----------------------------------------------------------------------*
* PERFORM DE CHAMADAS
*----------------------------------------------------------------------*

IF sy-batch EQ abap_true. "Se JOB MAGGI_ZLES0170 - Transação ZLES0170 estiver rodando, nao rodar esse JOB
  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = 'ZLESR0149' IMPORTING e_qtd  = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.

SELECT SINGLE COUNT(*) INTO @DATA(vg_job)
    FROM tbtco
   WHERE jobname EQ 'ATUALIZA_COMP_J1'
     AND status EQ 'R'.

IF ( vg_job EQ 1 ).
  PERFORM:
         z_insert_tabela.
ENDIF.
*&---------------------------------------------------------------------*
*&      Form  Z_INSERT_TABELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_insert_tabela.
*----------------------------------------------------------------------*
* SELEÇÃO DE NOTAS DE SAÍDA.
*----------------------------------------------------------------------*
  DATA: data         TYPE d,
        wa_zlest0134 TYPE zlest0134.

  RANGES: rg_datas FOR j_1bnfdoc-docdat.

  CLEAR: zcl_util.

  "Opter Área de contabilidade de custos
  SELECT * INTO TABLE it_setleaf
    FROM setleaf
   WHERE setname EQ 'ZCOMPARATIVO_CFOP'.

  LOOP AT it_setleaf INTO wa_setleaf.
    wa_cfops-sign   = 'I'.
    wa_cfops-option = 'EQ'.
    wa_cfops-low    = wa_setleaf-valfrom.
    wa_cfops-high   = wa_setleaf-valfrom.
    APPEND wa_cfops TO cfops.
  ENDLOOP.

  CLEAR: rg_datas.
  IF p_docnum IS INITIAL.

    SELECT SINGLE * INTO wa_zlest0134 FROM zlest0134.
    IF sy-subrc IS INITIAL AND wa_zlest0134-qtd_dias_zlest0039_sai IS NOT INITIAL.
      data = sy-datum - wa_zlest0134-qtd_dias_zlest0039_sai.
    ELSE.
      data = sy-datum - 1.
    ENDIF.
    rg_datas-sign   = 'I'.
    rg_datas-option = 'GE'.
    rg_datas-low    = data.
    rg_datas-high   = data.
    APPEND rg_datas.
  ENDIF.

  SELECT t1~docnum t1~series t1~bukrs t1~branch t1~parid t1~nfenum t1~docdat t1~inco1 t1~partyp t1~cancel
         t2~menge t2~meins t2~matnr t2~refkey t2~reftyp t2~werks t2~cfop
         t4~cancel t4~nfnum9 t4~docsta t4~scssta
    FROM j_1bnfdoc AS t1
    INNER JOIN j_1bnflin AS  t2 ON t1~docnum EQ t2~docnum
    INNER JOIN j_1bnfe_active AS t4 ON t1~docnum EQ t4~docnum
    INTO TABLE it_j_1bnf_doc_lin
  WHERE t1~docnum IN p_docnum
    AND t1~doctyp EQ  '1'
    AND t1~direct EQ  '2'
    AND ( ( t1~inco1  IN ('CIF','CPT','CFR') OR t2~cfop IN cfops ) OR ( t1~inco1 EQ 'FOB') )
    AND t1~docdat IN rg_datas
    AND t1~crenam NE 'INTERFACE'
    AND t2~reftyp IN ('MD','BI')
    AND t1~cancel NE 'X'
    AND t4~docsta EQ '1'
    AND ( t1~nfenum IS NOT NULL AND t1~nfenum NE space )
    AND NOT EXISTS ( SELECT * FROM zlest0039 AS t3 WHERE t1~docnum EQ t3~docnum ).

  CHECK NOT it_j_1bnf_doc_lin[] IS INITIAL.

  SORT it_j_1bnf_doc_lin BY docnum.
  DELETE ADJACENT DUPLICATES FROM it_j_1bnf_doc_lin COMPARING docnum.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR O CNPJ DO CLIENTE DA NOTA FISCAL DE SAÍDA
*----------------------------------------------------------------------*
  SELECT stcd1 kunnr
    FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_j_1bnf_doc_lin
  WHERE kunnr EQ it_j_1bnf_doc_lin-parid.

  SELECT stcd1 lifnr
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_j_1bnf_doc_lin
  WHERE lifnr EQ it_j_1bnf_doc_lin-parid.

  SELECT bukrs branch stcd1
    FROM j_1bbranch
    INTO TABLE it_j_1bbranch
    FOR ALL ENTRIES IN it_j_1bnf_doc_lin
  WHERE branch EQ it_j_1bnf_doc_lin-werks
    AND bukrs  EQ it_j_1bnf_doc_lin-bukrs.
*----------------------------------------------------------------------*
* LOOP DA SELEÇÃO NOTAS DE SAÍDA PARA ATRIBUIÇÃO NA TABELA ZLEST0039.
*----------------------------------------------------------------------*
  SORT:
          it_j_1bnf_doc_lin  BY docnum,
          it_vbrp            BY vbeln aubel,
          it_zsdt0001        BY vbeln,
          it_kna1            BY kunnr,
          it_lfa1            BY lifnr,
          it_j_1bbranch      BY bukrs branch .


  LOOP AT it_j_1bnf_doc_lin INTO wa_j_1bnf_doc_lin.

    CLEAR:   wa_kna1,
             wa_mkpf,
             wa_vbrp,
             wa_lfa1,
             wa_j_1bbranch,
             wa_zsdt0001,
             wa_vbfa,
             wa_lips.

    CLEAR:  zlest0039-docnum,
            zlest0039-serie,
            zlest0039-bukrs,
            zlest0039-werks,
            zlest0039-matnr,
            zlest0039-kunnr,
            zlest0039-nfenum,
            zlest0039-datasaida,
            zlest0039-pesosaida,
            zlest0039-unidadesaida,
            zlest0039-status,
            zlest0039-data,
            zlest0039-hora,
            zlest0039-formaconfirmacao,
            zlest0039-pontocoleta,
            zlest0039-pontotransb,
            zlest0039-pontoentrega,
            zlest0039-cnpj,
            zlest0039-vbeln,
            zlest0039-ebeln,
            zlest0039-placa_cav,
            zlest0039-reftyp,
            zlest0039-inco1.

    zlest0039-docnum       = wa_j_1bnf_doc_lin-docnum.
    zlest0039-serie        = wa_j_1bnf_doc_lin-series.
    zlest0039-bukrs        = wa_j_1bnf_doc_lin-bukrs.
    zlest0039-werks        = wa_j_1bnf_doc_lin-branch.
    zlest0039-matnr        = wa_j_1bnf_doc_lin-matnr.
    zlest0039-kunnr        = wa_j_1bnf_doc_lin-parid.
    zlest0039-nfenum       = wa_j_1bnf_doc_lin-nfenum.
    zlest0039-datasaida    = wa_j_1bnf_doc_lin-docdat.
    zlest0039-pesosaida    = wa_j_1bnf_doc_lin-menge.
    zlest0039-unidadesaida = wa_j_1bnf_doc_lin-meins.
    zlest0039-reftyp       = wa_j_1bnf_doc_lin-reftyp.
    zlest0039-inco1        = wa_j_1bnf_doc_lin-inco1.
    zlest0039-status       = 'ET'.
    zlest0039-data         = sy-datum.
    zlest0039-hora         = sy-uzeit.
    zlest0039-formaconfirmacao = 'A'.
    zlest0039-status_placa = 0.

*"// WBARBOSA 16102024 - US 154985 - VERIFICA SE O DOCNUM ATENDE A EUDR
    zcl_eudr_utils=>check_doc_fiscal_eudr(
      EXPORTING
        i_docnum = wa_j_1bnf_doc_lin-docnum " Nº documento
      RECEIVING
        r_eudr   = zlest0039-eudr " Atende critérios Europeu
    ).
*"// WBARBOSA 16102024 - US 154985 - VERIFICA SE O DOCNUM ATENDE A EUDR

    CREATE OBJECT zcl_util.
    zcl_util->monta_chave_nfe( EXPORTING i_docnum = zlest0039-docnum
                               RECEIVING e_chave  = zlest0039-chave_nfe ).


*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR OS PARCEIROS DAS NOTAS FISCAIS DE SAÍDA
* PC - PONTO DE COLETA
* LR - LOCAL DE ENTREGA
* Z1 - LOCAL DE ENTREGA FINAL (CASO TENHA TRANSBORDO LR)
*----------------------------------------------------------------------*

    CLEAR: it_vbfa[], it_vbpa[], wa_vbfa, wa_vbpa.

    SELECT  vbelv vbeln vbtyp_v vbtyp_n
       FROM vbfa
       INTO TABLE it_vbfa
     WHERE vbeln EQ wa_j_1bnf_doc_lin-refkey(10)
       AND vbtyp_v = 'J'.

    CHECK NOT it_vbfa[] IS INITIAL.

    SELECT vbeln parvw kunnr lifnr
      FROM vbpa
      INTO TABLE it_vbpa
      FOR ALL ENTRIES IN it_vbfa
    WHERE vbeln EQ it_vbfa-vbelv
      AND parvw IN ('PC','LR','Z1').

    SORT: it_vbfa BY vbelv,
          it_vbpa BY vbeln.

    LOOP AT it_vbpa INTO wa_vbpa.

      READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbelv = wa_vbpa-vbeln BINARY SEARCH.

      IF ( sy-subrc = 0 ).
        IF ( wa_vbpa-parvw EQ 'PC' ).
          zlest0039-pontocoleta = wa_vbpa-lifnr.
        ENDIF.

        IF ( wa_vbpa-parvw EQ 'LR' ).
          zlest0039-pontotransb = wa_vbpa-kunnr.
        ENDIF.

        IF ( wa_vbpa-parvw EQ 'Z1' ).
          zlest0039-pontoentrega = wa_vbpa-lifnr.
        ENDIF.
      ENDIF.

      CLEAR: wa_vbpa,
             wa_vbfa.
    ENDLOOP.

    IF ( zlest0039-pontoentrega EQ 0 ).
      zlest0039-pontoentrega = zlest0039-pontotransb.
      CLEAR: zlest0039-pontotransb.
    ENDIF.

    IF ( wa_j_1bnf_doc_lin-partyp EQ 'C').
      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_j_1bnf_doc_lin-parid BINARY SEARCH.
      zlest0039-cnpj = wa_kna1-stcd1.
    ENDIF.

    IF ( wa_j_1bnf_doc_lin-partyp EQ 'V' ).
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_j_1bnf_doc_lin-parid BINARY SEARCH.
      zlest0039-cnpj = wa_lfa1-stcd1.
    ENDIF.

    IF ( wa_j_1bnf_doc_lin-partyp EQ 'B' ).
      READ TABLE it_j_1bbranch INTO wa_j_1bbranch WITH KEY bukrs = wa_j_1bnf_doc_lin-bukrs
                                                           branch = wa_j_1bnf_doc_lin-werks
                                                           BINARY SEARCH.
      zlest0039-cnpj = wa_j_1bbranch-stcd1.
    ENDIF.
*----------------------------------------------------------------------*
* SELEÇÃO DE DADOS DE ORDEM DE VENDAS.
* CASO SEJA DE ORIGEM DE VENDA (T2~REFTYP = 'BI')
* RECUPERAR OS DEMAIS DADOS PELO DOCUMENTO DE FATURA (TABELA: VBRP)
*----------------------------------------------------------------------*

    DATA: refkey_aux TYPE j_1bnflin-refkey.

    IF  ( wa_j_1bnf_doc_lin-reftyp EQ 'BI' ).

      SELECT SINGLE * FROM vbrp AS t6
       WHERE t6~vbeln EQ wa_j_1bnf_doc_lin-refkey AND draft = space .


      IF ( ( sy-subrc EQ 0 ) AND ( wa_j_1bnf_doc_lin-inco1 EQ 'FOB' ) ).

        SELECT t1~vgbel t1~aubel t1~vbeln t1~mandt t2~zpesagem
          FROM vbrp AS t1
          INNER JOIN vbak AS t2 ON t2~vbeln EQ t1~aubel
          INTO TABLE it_vbrp_fob
          WHERE t1~vbeln EQ wa_j_1bnf_doc_lin-refkey
          AND   t2~zpesagem EQ '01'.

        LOOP AT it_vbrp_fob INTO wa_vbrp_fob WHERE vbeln EQ wa_j_1bnf_doc_lin-refkey AND zpesagem EQ '01'.
          zlest0039-vbeln =  wa_vbrp_fob-vgbel.
          zlest0039-ebeln =  wa_vbrp_fob-aubel.
        ENDLOOP.

      ELSEIF ( ( wa_j_1bnf_doc_lin-inco1 EQ 'CIF' ) OR ( wa_j_1bnf_doc_lin-inco1 EQ 'CPT' ) ) AND ( sy-subrc IS INITIAL ).
        zlest0039-vbeln = vbrp-vgbel.
        zlest0039-ebeln = vbrp-aubel.
      ENDIF.

*----------------------------------------------------------------------*
* SELEÇÃO PARA RECUPERAR A PLACA DO VEICULO DA NOTA FISCAL DE SAÍDA
*----------------------------------------------------------------------*
      SELECT SINGLE * INTO @DATA(wa_zsdt0001_01)
        FROM zsdt0001 AS t7
      WHERE t7~doc_rem EQ @vbrp-vgbel
        AND t7~vbeln   EQ @vbrp-aubel.

      IF sy-subrc IS INITIAL.
        zlest0039-placa_cav = wa_zsdt0001_01-placa_cav.
      ELSE.

        SELECT SINGLE k~text1 INTO @DATA(lc_texto)
          FROM vttp AS p
         INNER JOIN vttk AS k ON k~tknum EQ p~tknum AND k~vsart EQ '01'
         WHERE p~vbeln EQ @vbrp-vgbel.

        IF sy-subrc IS INITIAL AND lc_texto IS NOT INITIAL.

          wa_zsdt0001_01-placa_cav = lc_texto(7).

          FIND REGEX '[A-Z]{3}[0-9]{1}[A-Z]{1}[0-9]{2}' IN wa_zsdt0001_01-placa_cav.
          IF sy-subrc IS NOT INITIAL.
            FIND REGEX '[A-Z]{3}[0-9]{4}' IN wa_zsdt0001_01-placa_cav.
            IF sy-subrc IS NOT INITIAL.
              CLEAR: wa_zsdt0001_01-placa_cav.
            ENDIF.
          ENDIF.

          zlest0039-placa_cav = wa_zsdt0001_01-placa_cav.

        ENDIF.

      ENDIF.

    ELSE.
*----------------------------------------------------------------------*
* SELEÇÃO DE DADOS DE PEDIDO DE COMPRA
* PARA OS CASOS DE DOCUMENTOS DE MATERIAL, SERÁ NECESSÁRIO SEPARAR
* O DOCUMENTO DO ANO DE LANÇAMENTO QUE SÃO GRAVADOS JUNTOS
*----------------------------------------------------------------------*

      DATA:
        refkey_sem TYPE j_1bnflin-refkey,
        refkey_ano TYPE j_1bnflin-refkey.

      CLEAR: zlest0039-reftyp.

      zlest0039-reftyp = wa_j_1bnf_doc_lin-reftyp.

      refkey_sem = wa_j_1bnf_doc_lin-refkey(10).
      refkey_ano = wa_j_1bnf_doc_lin-refkey+10(4).


      CLEAR: it_mkpf[], it_lips[].

      SELECT mblnr mjahr le_vbeln
        FROM mkpf
        INTO TABLE it_mkpf
       WHERE mblnr EQ refkey_sem
        AND mjahr EQ refkey_ano.

      SELECT vbeln vgbel
        FROM lips
        INTO TABLE it_lips
        FOR ALL ENTRIES IN it_mkpf
       WHERE vbeln EQ it_mkpf-le_vbeln.

      SORT: it_mkpf BY mblnr mjahr,
            it_lips BY vbeln.

      READ TABLE it_mkpf INTO wa_mkpf WITH KEY mblnr = refkey_sem
                                               mjahr = refkey_ano BINARY SEARCH.
      READ TABLE it_lips INTO wa_lips WITH KEY vbeln = wa_mkpf-le_vbeln BINARY SEARCH.

      zlest0039-vbeln = wa_lips-vbeln.
      zlest0039-ebeln = wa_lips-vgbel.

      CLEAR: refkey_sem,
             refkey_ano.

      CLEAR: it_vbfa[], it_zsdt0001[].

      SELECT  vbelv vbeln vbtyp_v vbtyp_n
         FROM vbfa
         INTO TABLE it_vbfa
       WHERE vbeln EQ wa_j_1bnf_doc_lin-refkey(10)
         AND vbtyp_n = 'R'.

      CHECK NOT it_vbfa[] IS INITIAL.

      SELECT placa_cav doc_rem vbeln tp_movimento
        FROM zsdt0001
        INTO TABLE it_zsdt0001
        FOR ALL ENTRIES IN it_vbfa
      WHERE doc_rem EQ it_vbfa-vbelv
        AND tp_movimento EQ 'S'.

      CHECK NOT it_zsdt0001[] IS INITIAL.

      SORT: it_vbfa     BY vbeln,
            it_zsdt0001 BY doc_rem.

      READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbeln = wa_j_1bnf_doc_lin-refkey(10) BINARY SEARCH.
      READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY doc_rem = wa_vbfa-vbelv
                                                       tp_movimento = 'S' BINARY SEARCH.

      zlest0039-placa_cav  = wa_zsdt0001-placa_cav.

    ENDIF.

    INSERT zlest0039.

    CLEAR: wa_j_1bnf_doc_lin.

  ENDLOOP.

  CLEAR: data.

  data =  sy-datum - 7.

  SELECT t1~docnum
    FROM zlest0039 AS t1
    INTO TABLE it_zlest0039_delete
    WHERE t1~datasaida GE data
    AND EXISTS ( SELECT *
                  FROM j_1bnfdoc AS t2
                  WHERE  t2~docnum = t1~docnum
                    AND  t2~cancel = 'X' ).

  CHECK NOT it_zlest0039_delete[] IS INITIAL.

  LOOP AT it_zlest0039_delete INTO wa_zlest0039_delete.
    CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
      EXPORTING
        docnum         = wa_zlest0039_delete-docnum
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
      DELETE
        FROM zlest0039
        WHERE docnum EQ wa_zlest0039_delete-docnum.
      CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        EXPORTING
          docnum = wa_zlest0039_delete-docnum.

    ENDIF.
  ENDLOOP.

ENDFORM. "
