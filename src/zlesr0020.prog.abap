*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
********************************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                                     *
* Data desenv ...: 13.09.2011                                                              *
* Objetivo    ...: Relatório de Comparativo de Saída e Chegada - MODAL FERROVIÁRIO         *
* Transação   ...: ZLES0051                                                                *
* Autor       ...: Victor Hugo                                                             *
********************************************************************************************
REPORT  zlesr0016 MESSAGE-ID sd.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zlest0019, kna1, makt, lfa1.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES:
  BEGIN OF ty_zlest0019_z1,
    idvagao          TYPE zlest0019-idvagao,
    pesovagao        TYPE zlest0019-pesovagao,
    dtadecarga       TYPE zlest0019-dtadecarga,
    horadescarga     TYPE zlest0019-horadescarga,
    dcl              TYPE zlest0019-dcl,
    dcl_aux          TYPE zlest0019-dcl,
    dcl_aux6         TYPE zlest0019-dcl,
    dcl_aux7         TYPE zlest0019-dcl,
    dcl_aux10        TYPE zlest0019-dcl,
    seriedcl         TYPE zlest0019-seriedcl,
    idinter          TYPE zlest0019-idinter,
    tp_reg           TYPE zlest0019-tp_reg,
    id_refkey        TYPE zlest0019-id_refkey,
    idvagao_var(11)  TYPE c,
    idvagao_regex    TYPE string,
    idvagao_letra(3) TYPE c,
    status_duplica   TYPE zlest0019-status_duplica,
    observacao       TYPE zlest0019-observacao,
    bukrs            TYPE zlest0019-bukrs,
    cnpjferro        TYPE zlest0019-cnpjferro,
    tp_movi          TYPE zlest0019-tp_movi,
    chave            TYPE zlest0019-chave,
  END OF ty_zlest0019_z1,

  BEGIN OF ty_zlest0019_z1_aux,
    idvagao        TYPE zlest0019-idvagao,
    pesovagao      TYPE zlest0019-pesovagao,
    dtadecarga     TYPE zlest0019-dtadecarga,
    horadescarga   TYPE zlest0019-horadescarga,
    dcl            TYPE zlest0019-dcl,
    seriedcl       TYPE zlest0019-seriedcl,
    idinter        TYPE zlest0019-idinter,
    tp_reg         TYPE zlest0019-tp_reg,
    id_refkey      TYPE zlest0019-id_refkey,
    status_duplica TYPE zlest0019-status_duplica,
    observacao     TYPE zlest0019-observacao,
    bukrs          TYPE zlest0019-bukrs,
    cnpjferro      TYPE zlest0019-cnpjferro,
    tp_movi        TYPE zlest0019-tp_movi,
    chave          TYPE zlest0019-chave,
    idvagao2       TYPE zlest0019-idvagao,
  END OF ty_zlest0019_z1_aux,

  BEGIN OF ty_zlest0019_z2,
    bukrs          TYPE zlest0019-bukrs,
    branch         TYPE zlest0019-branch,
    nfenum         TYPE zlest0019-nfenum,
    cnpjcliente    TYPE zlest0019-cnpjcliente,
    idinter        TYPE zlest0019-idinter,
    tp_reg         TYPE zlest0019-tp_reg,
    dcl            TYPE zlest0019-dcl,
    seriedcl       TYPE zlest0019-seriedcl,
    id_refkey      TYPE zlest0019-id_refkey,
    status_duplica TYPE zlest0019-status_duplica,
    pesonf         TYPE zlest0019-pesonf,
  END OF ty_zlest0019_z2,

  BEGIN OF ty_carregamento,
    cnpjferro   TYPE zlest0019-cnpjferro,
    idinter     TYPE zlest0019-idinter,
    tp_reg      TYPE zlest0019-tp_reg,
    bukrs       TYPE zlest0019-bukrs,
    branch      TYPE zlest0019-branch,
    nfenum      TYPE zlest0019-nfenum,
    cnpjcliente TYPE zlest0019-cnpjcliente,
    dcl         TYPE zlest0019-dcl,
    seriedcl    TYPE zlest0019-seriedcl,

  END OF ty_carregamento,

  BEGIN OF ty_zlest0019_z3_aux,
    pesovagao    TYPE zlest0019-pesovagao,
    dtadecarga   TYPE zlest0019-dtadecarga,
    horadescarga TYPE zlest0019-horadescarga,
    dcl          TYPE zlest0019-dcl,
    idvagao      TYPE zlest0019-idvagao,
    idinter      TYPE zlest0019-idinter,
    tp_reg       TYPE zlest0019-tp_reg,
    cnpjferro    TYPE zlest0019-cnpjferro,
    idvagao_aux  TYPE zlest0019-idvagao,
    dcl_aux      TYPE zlest0019-dcl,
    dcl_aux6     TYPE zlest0019-dcl,
    dcl_aux7     TYPE zlest0019-dcl,
    dcl_aux10    TYPE zlest0019-dcl,
    idvagao2     TYPE zlest0019-idvagao,
  END OF ty_zlest0019_z3_aux,

  BEGIN OF ty_zlest0019_z3,

    pesovagao        TYPE zlest0019-pesovagao,
    dtadecarga       TYPE zlest0019-dtadecarga,
    horadescarga     TYPE zlest0019-horadescarga,
    dcl              TYPE zlest0019-dcl,
    idvagao          TYPE zlest0019-idvagao,
    idinter          TYPE zlest0019-idinter,
    tp_reg           TYPE zlest0019-tp_reg,
    cnpjferro        TYPE zlest0019-cnpjferro,
    idvagao_regex    TYPE string,
    idvagao_letra(3) TYPE c,
    dcl_aux          TYPE zlest0019-dcl,
    dcl_aux6         TYPE zlest0019-dcl,
    dcl_aux7         TYPE zlest0019-dcl,
    dcl_aux10        TYPE zlest0019-dcl,

  END OF ty_zlest0019_z3,

  BEGIN OF ty_zlest0019_z4,
    nfenum      TYPE zlest0019-nfenum,
    bukrs       TYPE zlest0019-bukrs,
    branch      TYPE zlest0019-branch,
    cnpjcliente TYPE zlest0019-cnpjcliente,
    idinter     TYPE zlest0019-idinter,
    tp_reg      TYPE zlest0019-tp_reg,
    cnpjferro   TYPE zlest0019-cnpjferro,
  END OF ty_zlest0019_z4,

  BEGIN OF ty_zlest0019_z5,
    dcl         TYPE zlest0019-dcl,
    seriedcl    TYPE zlest0019-seriedcl,
    idinter     TYPE zlest0019-idinter,
    tp_reg      TYPE zlest0019-tp_reg,
    bukrs       TYPE zlest0019-bukrs,
    branch      TYPE zlest0019-branch,
    nfenum      TYPE zlest0019-nfenum,
    cnpjcliente TYPE zlest0019-cnpjcliente,
  END OF ty_zlest0019_z5,

  BEGIN OF ty_zlest0019_z6,
    idvagao      TYPE zlest0019-idvagao,
    pesovagao    TYPE zlest0019-pesovagao,
    dtadecarga   TYPE zlest0019-dtadecarga,
    horadescarga TYPE zlest0019-horadescarga,
    seriedcl     TYPE zlest0019-seriedcl,
    dcl          TYPE zlest0019-dcl,

  END OF ty_zlest0019_z6,

  BEGIN OF ty_zlest0019_z6_aux,
    idvagao         TYPE zlest0019-idvagao,
    pesovagao       TYPE zlest0019-pesovagao,
    dtadecarga      TYPE zlest0019-dtadecarga,
    horadescarga    TYPE zlest0019-horadescarga,
    seriedcl        TYPE zlest0019-seriedcl,
    dcl             TYPE zlest0019-dcl,
    idvagao_var(11) TYPE c,
    dcl_aux         TYPE zlest0019-dcl,
  END OF ty_zlest0019_z6_aux,

  BEGIN OF ty_zlest0019_z7,
    pesovagao    TYPE zlest0019-pesovagao,
    dtadecarga   TYPE zlest0019-dtadecarga,
    horadescarga TYPE zlest0019-horadescarga,
    dcl          TYPE zlest0019-dcl,
    idvagao      TYPE zlest0019-idvagao,
    idinter      TYPE zlest0019-idinter,
    tp_reg       TYPE zlest0019-tp_reg,
    cnpjferro    TYPE zlest0019-cnpjferro,
  END OF ty_zlest0019_z7,

  BEGIN OF ty_kna1,
    name1 TYPE kna1-name1,
    stcd1 TYPE kna1-stcd1,
    kunnr TYPE kna1-kunnr,
  END OF ty_kna1,

  " Estrutura para recuperar descrição do material da nota fiscal.
  BEGIN OF ty_j_1bnfdoc,
    mandt  TYPE j_1bnfdoc-mandt,
    docnum TYPE j_1bnfdoc-docnum,
    nfenum TYPE j_1bnfdoc-nfenum,
    branch TYPE j_1bnfdoc-branch,
    bukrs  TYPE j_1bnfdoc-bukrs,
    cancel TYPE j_1bnfdoc-cancel,
    direct TYPE j_1bnfdoc-direct,
    docdat TYPE j_1bnfdoc-docdat,
  END OF ty_j_1bnfdoc,

  BEGIN OF ty_j_1bnflin,
    mandt  TYPE j_1bnflin-mandt,
    docnum TYPE j_1bnflin-docnum,
    matnr  TYPE j_1bnflin-matnr,
    cfop   TYPE j_1bnflin-cfop,
    itmnum TYPE j_1bnflin-itmnum,
    meins  TYPE j_1bnflin-meins,
    werks  TYPE j_1bnflin-werks,
  END OF ty_j_1bnflin,

  BEGIN OF ty_j_1bnfnad,
    docnum TYPE j_1bnfnad-docnum,
    name1  TYPE j_1bnfnad-name1,
    parid  TYPE j_1bnfnad-parid,
  END OF ty_j_1bnfnad,

  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  BEGIN OF ty_lfa1,
    name1 TYPE lfa1-name1,
    stcd1 TYPE lfa1-stcd1,
    kunnr TYPE lfa1-kunnr,
    ort01 TYPE lfa1-ort01,
    lifnr TYPE lfa1-lifnr,
  END OF ty_lfa1,


  BEGIN OF ty_saida,
    tp_movi_s(20)    TYPE c, "Tipo de movimento Saída
    tp_movi_e(20)    TYPE c, "Tipo de movimento Entrada
    bukrs            TYPE zlest0019-bukrs, " Empresa
    branch           TYPE zlest0019-branch, " Local de negócios

    name1_s          TYPE lfa1-name1, " Local de Carregamento
    name1_e          TYPE lfa1-name1, " Local de Carregamento
    name1_d          TYPE lfa1-name1, " Destinatário

    idvagao_s        TYPE zlest0019-idvagao, " ID do vagão / Saída
    idvagao_e        TYPE zlest0019-idvagao, " ID do vagão / Entrada

    "pesovagao_s          TYPE zlest0019-pesovagao, " Peso do Vagão / Saída
    pesovagao_s      TYPE db20199vp, " Peso do Vagão / Saída
    pesovagao_s_soma TYPE db20199vp, " Peso do Vagão / Saída
    pesovagao_e      TYPE db20199vp, " Peso do Vagão / Entrada
    peso_diferenca   TYPE db20199vp, " Peso do Vagão / Entrada

    dcl_s            TYPE zlest0019-dcl, " DCL / Saída
    dcl_e            TYPE zlest0019-dcl, " DCL / Entrada

    seriedcl_s       TYPE zlest0019-seriedcl, " Serie DCL
    seriedcl_e       TYPE zlest0019-seriedcl, " Serie DCL

    dtadecarga_s(10) TYPE c, " Data / Saida
    dtadecarga_e(10) TYPE c, " Data / Entrada
    dtadecarga_e_aux TYPE zlest0019-dtadecarga,
    dtadecarga_s_aux TYPE zlest0019-dtadecarga,

    horadescarga_s   TYPE zlest0019-horadescarga, " Hora de Descarga / Saída
    horadescarga_e   TYPE zlest0019-horadescarga, " Hora de Descarga / Entrada

    dias_trans       TYPE p,


    matnr            TYPE makt-matnr,
    maktx            TYPE makt-maktx,

    ort01_s          TYPE lfa1-ort01,
    ort01_e          TYPE lfa1-ort01,

    lifnr_s          TYPE lfa1-lifnr,
    lifnr_e          TYPE lfa1-lifnr,
    lifnr_d          TYPE lfa1-lifnr,

    duplic(4)        TYPE c,

    observacao       TYPE zlest0019-observacao,

    idinter          TYPE zlest0019-idinter,
    tp_movi          TYPE zlest0019-tp_movi,
    tp_reg           TYPE zlest0019-tp_reg,
    chave            TYPE zlest0019-chave,
    dcl              TYPE zlest0019-dcl,
    seriedcl         TYPE zlest0019-seriedcl,
    meins            TYPE j_1bnflin-meins,
    werks            TYPE j_1bnflin-werks,

  END OF ty_saida,

  BEGIN OF ty_saida2,
    bukrs          TYPE zlest0019-bukrs,
    branch         TYPE zlest0019-branch,
    matnr          TYPE zlest0019-matnr,
    maktx          TYPE makt-maktx,
    nfenum         TYPE zlest0019-nfenum,
    docnum         TYPE zlest0019-docnum,
    observacao     TYPE zlest0019-observacao,
    idinter        TYPE zlest0019-idinter,
    chave          TYPE zlest0019-chave,
    dcl            TYPE zlest0019-dcl,
    seriedcl       TYPE zlest0019-seriedcl,
    cnpjcliente    TYPE zlest0019-cnpjcliente,
    status_duplica TYPE zlest0019-status_duplica,
    cod_fornecedor TYPE zlest0019-cod_fornecedor,
    cnpjferro_s    TYPE zlest0019-cnpjferro,
    cnpjferro_e    TYPE zlest0019-cnpjferro,
    cnpjferro_fr   TYPE zlest0019-cnpjferro,
    tp_movi_s      TYPE zlest0019-tp_movi,
    tp_movi_E      TYPE zlest0019-tp_movi,
    tp_reg_s       TYPE zlest0019-tp_reg,
    tp_reg_e       TYPE zlest0019-tp_reg,
    idvagao_s      TYPE zlest0019-idvagao,
    idvagao_e      TYPE zlest0019-idvagao,
    pesonf         TYPE zlest0019-pesonf,
    pesovagao_s    TYPE zlest0019-pesodvagao,
    pesovagao_e    TYPE zlest0019-pesodvagao,
    pesovagao_t    TYPE zlest0019-pesodvagao,
    dtadecarga_s   TYPE zlest0019-dtadecarga,
    DTADECARGA_e   TYPE zlest0019-dtadecarga,
    horadescarga_S TYPE zlest0019-horadescarga,
    horadescarga_e TYPE zlest0019-horadescarga,
    name1_s        TYPE j_1bnfnad-name1,
    lifnr_s        TYPE lfa1-lifnr,
    ort01_s        TYPE lfa1-ort01,
    name1_e        TYPE j_1bnfnad-name1,
    lifnr_e        TYPE lfa1-lifnr,
    ort01_e        TYPE lfa1-ort01,
    name1_d        TYPE j_1bnfnad-name1,
    lifnr_d        TYPE lfa1-lifnr,
    meins_d        TYPE j_1bnflin-meins,
    werks_d        TYPE j_1bnflin-werks,
    dias_trans     TYPE i,
    pesovagao_d    TYPE zlest0019-pesodvagao,
    pesovagao_m    TYPE zlest0019-pesodvagao,
    duplic         TYPE char4,
  END OF ty_saida2,

  BEGIN OF ty_1bnf,
    nfenum TYPE j_1bnfdoc-nfenum,
    docnum TYPE j_1bnfnad-docnum,
    bukrs  TYPE j_1bnfdoc-bukrs,
    branch TYPE j_1bnfdoc-branch,
    name1  TYPE j_1bnfnad-name1,
    parid  TYPE j_1bnfnad-parid,
    meins  TYPE j_1bnflin-meins,
    werks  TYPE j_1bnflin-werks,
  END OF ty_1bnf,

  BEGIN OF ty_edicao,
    idvagao    TYPE zlest0019-idvagao,
    dcl        TYPE zlest0019-dcl,
    observacao TYPE zlest0019-observacao,
    dtadecarga TYPE zlest0019-dtadecarga,
  END OF ty_edicao.
*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA:

  it_zlest0019_z1          TYPE TABLE OF ty_zlest0019_z1,
  it_zlest0019_z2          TYPE TABLE OF ty_zlest0019_z2,
  it_carregamento          TYPE TABLE OF ty_carregamento,
  it_zlest0019_z3          TYPE TABLE OF ty_zlest0019_z3,
  it_zlest0019_entrada     TYPE TABLE OF ty_zlest0019_z3,
  it_zlest0019_z4          TYPE TABLE OF ty_zlest0019_z4,
  it_zlest0019_z5          TYPE TABLE OF ty_zlest0019_z5,
  it_zlest0019_z6          TYPE TABLE OF ty_zlest0019_z6,
  it_zlest0019_z7          TYPE TABLE OF ty_zlest0019_z7,
  it_zib_contabil_chv      TYPE TABLE OF zib_contabil_chv   WITH HEADER LINE,

  it_j_1bnfdoc             TYPE TABLE OF ty_j_1bnfdoc,
  it_j_1bnflin             TYPE TABLE OF ty_j_1bnflin,
  it_j_1bnfnad             TYPE TABLE OF ty_j_1bnfnad,
  it_makt                  TYPE TABLE OF ty_makt,

  it_lfa1                  TYPE TABLE OF ty_lfa1,
  it_lfa1_l3               TYPE TABLE OF ty_lfa1,

  it_zlest0019_z1_aux      TYPE TABLE OF ty_zlest0019_z1_aux,
  it_zlest0019_z6_aux      TYPE TABLE OF ty_zlest0019_z6_aux,
  it_zlest0019_z3_aux      TYPE TABLE OF ty_zlest0019_z3_aux,
  it_zlest0019_z3_aux_dois TYPE TABLE OF ty_zlest0019_z3,
  it_zlest0079             TYPE TABLE OF zlest0079, "BPC


  wa_setleaf               TYPE setleaf,
  it_setleaf               LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE,

  it_saida                 TYPE TABLE OF ty_saida,
  it_saida2                TYPE TABLE OF ty_saida2,
  it_1bnf                  TYPE TABLE OF ty_1bnf,
  wa_1bnf                  TYPE ty_1bnf,
  it_saida_079c            TYPE TABLE OF ty_saida,
  it_saida_079d            TYPE TABLE OF ty_saida,
  it_saida_079             TYPE TABLE OF ty_saida,
  it_saida_group           TYPE TABLE OF ty_saida,
  it_saida_soma            TYPE TABLE OF ty_saida,
  it_saida_aux             TYPE TABLE OF ty_saida,
  it_saida_edicao          TYPE TABLE OF ty_saida,

  it_edicao                TYPE TABLE OF ty_edicao,

  it_bdc                   TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  it_messtab               TYPE TABLE OF bdcmsgcoll.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:

  wa_zlest0019_z1          TYPE ty_zlest0019_z1,
  wa_zlest0019_z2          TYPE ty_zlest0019_z2,
  wa_carregamento          TYPE ty_carregamento,

  wa_zlest0019_z3          TYPE ty_zlest0019_z3,
  wa_zlest0019_entrada     TYPE ty_zlest0019_z3,


  wa_zlest0019_z4          TYPE ty_zlest0019_z4,
  wa_zlest0019_z5          TYPE ty_zlest0019_z5,
  wa_zlest0019_z6          TYPE ty_zlest0019_z6,
  wa_zlest0019_z7          TYPE ty_zlest0019_z7,

  wa_kna1                  TYPE ty_kna1,
  wa_j_1bnfdoc             TYPE ty_j_1bnfdoc,
  wa_j_1bnflin             TYPE ty_j_1bnflin,
  wa_j_1bnfnad             TYPE ty_j_1bnfnad,
  wa_makt                  TYPE ty_makt,
  wa_lfa1                  TYPE ty_lfa1,
  wa_lfa1_l3               TYPE ty_lfa1,

  wa_zlest0019_z1_aux      TYPE ty_zlest0019_z1_aux,
  wa_zlest0019_z6_aux      TYPE ty_zlest0019_z6_aux,
  wa_zlest0019_z3_aux      TYPE ty_zlest0019_z3_aux,
  wa_zlest0019_z3_aux_dois TYPE ty_zlest0019_z3,
  wa_zlest0079             TYPE zlest0079, "BPC

  cfops                    TYPE RANGE OF j_1bnflin-cfop,
  wa_cfops                 TYPE lxhme_range_c10,
*     wa_dcl_e(6), "tama nho fixo de 6 para entrada ALRS
  wa_dcl_5(5), "tamanho fixo de 6 para entrada ALRS
  wa_dcl_6(6), "tamanho fixo de 6 para entrada ALRS
  wa_dcl_7(7), "tamanho fixo de 6 para entrada ALRS
  wa_dcl_10(10), "tamanho fixo de 6 para entrada ALRS


  wa_saida                 TYPE ty_saida,
  wa_saida_group           TYPE ty_saida,
  wa_saida_soma            TYPE ty_saida,
  wa_saida_aux             TYPE ty_saida,
  wa_saida_edicao          TYPE ty_saida,

  wa_edicao                TYPE ty_edicao,


  wa_cont                  TYPE REF TO cl_gui_custom_container,
  wa_alv                   TYPE REF TO cl_gui_alv_grid,

  wa_cont_group            TYPE REF TO cl_gui_custom_container,
  wa_alv_group             TYPE REF TO cl_gui_alv_grid,


  wa_layout                TYPE lvc_s_layo,
  wa_stable                TYPE lvc_s_stbl,
  index_aux                TYPE lvc_s_row,
  tabix                    TYPE sy-tabix,

  wa_layout_group          TYPE lvc_s_layo,
  wa_stable_group          TYPE lvc_s_stbl,
  gw_setleaf               TYPE setleaf,
  gw_setlinet              TYPE setlinet.



DATA: dia(2) TYPE c,
      mes(2) TYPE c,
      ano(4) TYPE c.

DATA: d_00 TYPE zlest0019-dtadecarga,
      d_30 TYPE zlest0019-dtadecarga,
      d_60 TYPE zlest0019-dtadecarga.

    DATA: gs_variant  TYPE disvariant.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: it_fcat       TYPE TABLE OF lvc_s_fcat.
DATA: it_fcat_group TYPE TABLE OF lvc_s_fcat.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                 p_bukrs  FOR zlest0019-bukrs  NO-EXTENSION, " Empresa
                 p_werks  FOR zlest0019-branch  NO-EXTENSION, " Filial de Carregamento
                 p_base   FOR zlest0019-erdat NO-EXTENSION NO INTERVALS,
                 p_erdats FOR zlest0019-erdat NO-EXTENSION OBLIGATORY, "  OBLIGATORY , " Período data Saída "158502 CS2024001075 ZLES0057 Correções P1 PSA
                 p_erdatc FOR zlest0019-erdat NO-EXTENSION, " Período data Chegada "158502 CS2024001075 ZLES0057 Correções P1 PSA
                 p_zplaca FOR zlest0019-idvagao NO INTERVALS, " Placa do Vagão
                 p_matnr  FOR makt-matnr NO INTERVALS, " Placa do Vagão
                 p_dcl    FOR zlest0019-dcl NO INTERVALS, " DCL
                 p_kunnr  FOR lfa1-lifnr NO INTERVALS, " Local de Carregamento
                 p_lifnr  FOR kna1-lifnr NO INTERVALS, " Terminal de Descarga
                 p_lifnrf FOR lfa1-lifnr NO INTERVALS. " Destinatário
SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS:
    s_trans TYPE char1 RADIOBUTTON GROUP rb01 , "USER-COMMAND ACT,
    s_todos TYPE char1 RADIOBUTTON GROUP rb01 DEFAULT 'X'.

SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

  PARAMETERS:
                c_bpc      TYPE char1 AS CHECKBOX  DEFAULT ' '.

SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004. "158502 CS2024001075 ZLES0057 Correções P1 PSA

  SELECTION-SCREEN BEGIN OF LINE.

    SELECTION-SCREEN COMMENT 2(5) ctenm.
    PARAMETERS: rel_cte RADIOBUTTON GROUP rel DEFAULT 'X'.

    SELECTION-SCREEN COMMENT 15(5) notanm.
    PARAMETERS: rel_nota RADIOBUTTON GROUP rel.

  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b4.

INITIALIZATION.
  IF sy-batch NE 'X'.
    REFRESH p_base.
    CLEAR  p_base.
    p_base-low       = sy-datum - 1.
    p_base-sign    = 'I'.
    p_base-option  = 'EQ'.
    APPEND p_base.

    REFRESH p_erdats.
    CLEAR  p_erdats.
    p_erdats-low     = sy-datum - 31.
    p_erdats-high    = sy-datum - 1.
    p_erdats-sign    = 'I'.
    p_erdats-option  = 'BT'.
    APPEND p_erdats.

*    REFRESH P_ERDATC.
*    CLEAR  P_ERDATC.
*    P_ERDATC-LOW     = SY-DATUM - 31.
*    P_ERDATC-HIGH    = SY-DATUM - 1.
*    P_ERDATC-SIGN    = 'I'.
*    P_ERDATC-OPTION  = 'BT'.
*    APPEND P_ERDATC.

  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  notanm = 'Notas'.
  ctenm = 'Vagão'.
*  IF S_TRANS IS NOT INITIAL.
*    REFRESH P_ERDATC.
*  ELSE.
*    IF  P_ERDATC[] IS INITIAL.
*      REFRESH P_ERDATC.
*      CLEAR  P_ERDATC.
*      P_ERDATC-LOW     = SY-DATUM - 31.
*      P_ERDATC-HIGH    = SY-DATUM - 1.
*      P_ERDATC-SIGN    = 'I'.
*      P_ERDATC-OPTION  = 'BT'.
*      APPEND P_ERDATC.
*    ENDIF.
*  ENDIF.
*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  gs_variant-report      = sy-repid.

  IF rel_cte = 'X'. "158502 CS2024001075 ZLES0057 Correções P1 PSA
    PERFORM:
              f_seleciona_dados     , " Form seleciona dados
              p_soma_peso_saida,
              f_alv                 . " ALV
    IF sy-batch NE 'X'.
      CALL SCREEN 0100.
    ENDIF.
  ELSEIF rel_nota = 'X'. "158502 CS2024001075 ZLES0057 Correções P1 PSA
    PERFORM:
          f_seleciona_dados2     , " Form seleciona dados2
          f_alv2                 . " ALV2
    IF it_saida2 IS INITIAL.
      MESSAGE i836 WITH TEXT-043.
      EXIT.
    ENDIF.
    IF sy-batch NE 'X'.
      CALL SCREEN 0400.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .
  IF sy-batch EQ 'X'.
    REFRESH: p_base, p_erdats,p_erdatc.
    CLEAR: p_base, p_erdats,p_erdatc.

    p_base-low       = sy-datum - 1.
    p_base-sign    = 'I'.
    p_base-option  = 'EQ'.
    APPEND p_base.
    APPEND p_base.

    p_erdats-low     = sy-datum - 31.
    p_erdats-high    = sy-datum - 1.
    p_erdats-sign    = 'I'.
    p_erdats-option  = 'BT'.
    APPEND p_erdats.

    REFRESH p_erdatc.
*    IF S_TRANS IS NOT INITIAL.
*      REFRESH P_ERDATC.
*    ELSE.
*      REFRESH P_ERDATC.
*      CLEAR  P_ERDATC.
*      P_ERDATC-LOW     = SY-DATUM - 31.
*      P_ERDATC-HIGH    = SY-DATUM - 1.
*      P_ERDATC-SIGN    = 'I'.
*      P_ERDATC-OPTION  = 'BT'.
*      APPEND P_ERDATC.
*    ENDIF.

  ENDIF.
  " Dados Saída
  SELECT
    substring( z1~idvagao,1,10 ) AS idvagao,
    z1~pesovagao,
    z1~dtadecarga,
    z1~horadescarga,
    z1~dcl,
    z1~seriedcl,
    z1~idinter,
    z1~tp_reg,
    z1~id_refkey,
    z1~status_duplica,
    z1~observacao,
    z1~bukrs,
    z1~cnpjferro,
    z1~tp_movi,
    z1~chave
    FROM zlest0019 AS z1
    WHERE z1~dtadecarga BETWEEN @p_erdats-low AND @p_erdats-high "IN p_erdats 158502 CS2024001075 ZLES0057 Correções P1 PSA
    AND substring( z1~idvagao,1,10 )    IN @p_zplaca
    AND z1~dcl        IN @p_dcl
    "AND Z1~BUKRS     IN P_BUKRS  --> Não está sendo gravado empresa no L2/20
    AND z1~idinter    EQ 'L2'
    AND z1~tp_reg     EQ '20'
    INTO TABLE @it_zlest0019_z1_aux.

  CHECK NOT it_zlest0019_z1_aux[] IS INITIAL.

  LOOP AT it_zlest0019_z1_aux INTO wa_zlest0019_z1_aux.

    wa_zlest0019_z1-pesovagao       = wa_zlest0019_z1_aux-pesovagao.
    wa_zlest0019_z1-dtadecarga      = wa_zlest0019_z1_aux-dtadecarga.
    wa_zlest0019_z1-horadescarga    = wa_zlest0019_z1_aux-horadescarga.
    wa_zlest0019_z1-seriedcl        = wa_zlest0019_z1_aux-seriedcl.
    wa_zlest0019_z1-dcl             = wa_zlest0019_z1_aux-dcl.
    wa_zlest0019_z1-idinter         = wa_zlest0019_z1_aux-idinter.
    wa_zlest0019_z1-tp_reg          = wa_zlest0019_z1_aux-tp_reg.
    wa_zlest0019_z1-id_refkey       = wa_zlest0019_z1_aux-id_refkey.
    wa_zlest0019_z1-idvagao         = wa_zlest0019_z1_aux-idvagao.
    wa_zlest0019_z1-status_duplica  = wa_zlest0019_z1_aux-status_duplica.
    wa_zlest0019_z1-observacao      = wa_zlest0019_z1_aux-observacao.
    wa_zlest0019_z1-bukrs           = wa_zlest0019_z1_aux-bukrs.
    wa_zlest0019_z1-cnpjferro       = wa_zlest0019_z1_aux-cnpjferro.
    wa_zlest0019_z1-tp_movi         = wa_zlest0019_z1_aux-tp_movi.
    wa_zlest0019_z1-chave           = wa_zlest0019_z1_aux-chave.

    " Pegar somente as caracteres após as 3 palavras iniciais
    wa_zlest0019_z1-idvagao_regex = wa_zlest0019_z1-idvagao+3.

    " Pegar as 3 primeiras caracteres da variavel.
    wa_zlest0019_z1-idvagao_letra = wa_zlest0019_z1-idvagao(3).

    " Retirar o caracter "T" da variável.
    REPLACE REGEX 'T$' IN wa_zlest0019_z1-idvagao_regex WITH ''.

    " Concatenação para espaçamento do campo idvagao no L2 30.
    CONCATENATE wa_zlest0019_z1_aux-idvagao(10) ' ' INTO wa_zlest0019_z1-idvagao_var RESPECTING BLANKS.

    "ALRS
    "digitos
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zlest0019_z1_aux-dcl
      IMPORTING
        output = wa_zlest0019_z1-dcl_aux.


    wa_dcl_5 = wa_zlest0019_z1-dcl_aux.
    wa_dcl_6 = wa_zlest0019_z1-dcl_aux.
    wa_dcl_7 = wa_zlest0019_z1-dcl_aux.
    wa_dcl_10 = wa_zlest0019_z1-dcl_aux.
    CONDENSE wa_dcl_5 NO-GAPS.
    CONDENSE wa_dcl_6 NO-GAPS.
    CONDENSE wa_dcl_7 NO-GAPS.
    CONDENSE wa_dcl_10 NO-GAPS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dcl_5
      IMPORTING
        output = wa_dcl_5.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dcl_6
      IMPORTING
        output = wa_dcl_6.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dcl_7
      IMPORTING
        output = wa_dcl_7.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dcl_10
      IMPORTING
        output = wa_dcl_10.

    wa_zlest0019_z1-dcl_aux  = wa_dcl_5.
    wa_zlest0019_z1-dcl_aux6 = wa_dcl_6.
    wa_zlest0019_z1-dcl_aux7 = wa_dcl_7.
    wa_zlest0019_z1-dcl_aux10 = wa_dcl_10.

    APPEND wa_zlest0019_z1 TO it_zlest0019_z1.
    CLEAR: wa_zlest0019_z1_aux, wa_zlest0019_z1.

  ENDLOOP.

  CHECK NOT it_zlest0019_z1[] IS INITIAL.

  SELECT z2~bukrs z2~branch z2~nfenum z2~cnpjcliente z2~idinter z2~tp_reg z2~dcl z2~seriedcl z2~id_refkey z2~status_duplica z2~pesonf
    FROM zlest0019 AS z2
    INTO TABLE it_zlest0019_z2
    FOR ALL ENTRIES IN it_zlest0019_z1
  WHERE z2~idinter   EQ 'L2'
    AND z2~tp_reg    EQ '30'
    AND z2~bukrs     IN p_bukrs  "--> Está sendo gravado empresa no L2/30
    AND z2~branch    IN p_werks
    AND z2~dcl       EQ it_zlest0019_z1-dcl
    AND z2~seriedcl  EQ it_zlest0019_z1-seriedcl
    AND z2~id_refkey EQ it_zlest0019_z1-id_refkey.

  CHECK NOT it_zlest0019_z2[] IS INITIAL.

  IF NOT ( p_kunnr IS INITIAL ).

    SELECT  name1 stcd1 kunnr ort01 lifnr
      FROM lfa1
      INTO TABLE it_lfa1
    WHERE lifnr IN p_kunnr.

    READ TABLE it_lfa1 INDEX 1 INTO wa_lfa1.

    SELECT carr~cnpjferro carr~idinter carr~tp_reg carr~bukrs carr~branch carr~nfenum carr~cnpjcliente carr~dcl
      FROM zlest0019 AS carr
      INTO TABLE it_carregamento
      FOR ALL ENTRIES IN it_zlest0019_z2
    WHERE carr~idinter     EQ 'L1'
      AND carr~tp_reg      EQ '30'
      AND carr~bukrs       EQ it_zlest0019_z2-bukrs
      AND carr~branch      EQ it_zlest0019_z2-branch
      AND carr~nfenum      EQ it_zlest0019_z2-nfenum
      AND carr~cnpjcliente EQ it_zlest0019_z2-cnpjcliente
      AND carr~cnpjferro   EQ wa_lfa1-stcd1.

    CHECK NOT it_carregamento[] IS INITIAL.

  ELSE.

    SELECT carr~cnpjferro carr~idinter carr~tp_reg carr~bukrs carr~branch carr~nfenum carr~cnpjcliente carr~dcl
      FROM zlest0019 AS carr
      INTO TABLE it_carregamento
      FOR ALL ENTRIES IN it_zlest0019_z2
    WHERE carr~idinter     EQ 'L1'
      AND carr~tp_reg      EQ '30'
      AND carr~bukrs       EQ it_zlest0019_z2-bukrs
      AND carr~branch      EQ it_zlest0019_z2-branch
      AND carr~nfenum      EQ it_zlest0019_z2-nfenum
      AND carr~cnpjcliente EQ it_zlest0019_z2-cnpjcliente
      AND carr~cnpjferro   NE space.

    SELECT name1 stcd1 kunnr ort01 lifnr
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_carregamento
    WHERE stcd1 EQ it_carregamento-cnpjferro.

  ENDIF.

  " Cabeçalho da Nota Fiscal
  SELECT mandt docnum nfenum branch bukrs cancel direct docdat
    FROM j_1bnfdoc
    INTO TABLE it_j_1bnfdoc
    FOR ALL ENTRIES IN it_zlest0019_z2
  WHERE nfenum EQ it_zlest0019_z2-nfenum
    AND branch EQ it_zlest0019_z2-branch
    AND bukrs  EQ it_zlest0019_z2-bukrs
    AND cancel NE 'X'
    AND direct EQ '2'.


  CHECK NOT it_j_1bnfdoc[] IS INITIAL.

  DATA: vl_tabix TYPE sy-tabix.

  SELECT SINGLE * FROM setleaf INTO gw_setleaf WHERE setname EQ 'MAGGI_ZLESR0020_EXC'.
  IF ( sy-subrc EQ 0 ).

    SELECT SINGLE * FROM setlinet INTO gw_setlinet WHERE setname EQ 'MAGGI_ZLESR0020_EXC'.

    IF ( sy-subrc EQ 0 ).
      CLEAR: wa_j_1bnfdoc.
      LOOP AT it_j_1bnfdoc INTO wa_j_1bnfdoc.

        vl_tabix = sy-tabix.
        IF ( wa_j_1bnfdoc-branch EQ gw_setleaf-valfrom ) AND ( wa_j_1bnfdoc-docdat <= gw_setlinet-descript ).
          DELETE it_j_1bnfdoc INDEX vl_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

  " Item da Nota Fiscal
  IF NOT ( p_matnr IS INITIAL ).

    SELECT mandt docnum matnr cfop itmnum meins werks
      FROM j_1bnflin
      CLIENT SPECIFIED INTO TABLE it_j_1bnflin
      FOR ALL ENTRIES IN it_j_1bnfdoc
    WHERE docnum EQ it_j_1bnfdoc-docnum
      AND mandt  EQ it_j_1bnfdoc-mandt
      AND itmnum EQ '000010'
      AND matnr  IN p_matnr.

  ELSE.

    SELECT mandt docnum matnr cfop itmnum meins werks
      FROM j_1bnflin
      CLIENT SPECIFIED INTO TABLE it_j_1bnflin
      FOR ALL ENTRIES IN it_j_1bnfdoc
    WHERE docnum EQ it_j_1bnfdoc-docnum
      AND mandt  EQ it_j_1bnfdoc-mandt
      AND itmnum EQ '000010'.

  ENDIF.

  CHECK NOT it_j_1bnflin[] IS INITIAL.

  " Parceiros
  SELECT docnum name1 parid
  FROM j_1bnfnad
     INTO TABLE it_j_1bnfnad
    FOR ALL ENTRIES IN  it_j_1bnfdoc
  WHERE docnum EQ it_j_1bnfdoc-docnum
    AND parvw EQ 'Z1'
    AND parid IN p_lifnrf.

  " Descrição do Material
  SELECT matnr maktx
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_j_1bnflin
  WHERE matnr EQ it_j_1bnflin-matnr
    AND spras EQ sy-langu.

  " Entrada 5 digitos
  SELECT z3~pesovagao z3~dtadecarga z3~horadescarga z3~dcl z3~idvagao z3~idinter z3~tp_reg z3~cnpjferro
    FROM zlest0019 AS z3
    INTO TABLE it_zlest0019_z3_aux
    FOR ALL ENTRIES IN it_zlest0019_z1
  WHERE z3~dcl        EQ  it_zlest0019_z1-dcl_aux
    AND z3~dtadecarga IN  p_erdatc
    AND z3~idinter    EQ 'L3'
    AND z3~tp_reg     EQ '20'.

  " Entrada 6 digitos
  SELECT z3~pesovagao z3~dtadecarga z3~horadescarga z3~dcl z3~idvagao z3~idinter z3~tp_reg z3~cnpjferro
    FROM zlest0019 AS z3
    APPENDING TABLE it_zlest0019_z3_aux
    FOR ALL ENTRIES IN it_zlest0019_z1
  WHERE z3~dcl        EQ  it_zlest0019_z1-dcl_aux6
    AND z3~dtadecarga IN  p_erdatc
    AND z3~idinter    EQ 'L3'
    AND z3~tp_reg     EQ '20'.

  " Entrada 7 digitos
  SELECT z3~pesovagao z3~dtadecarga z3~horadescarga z3~dcl z3~idvagao z3~idinter z3~tp_reg z3~cnpjferro
    FROM zlest0019 AS z3
    APPENDING TABLE it_zlest0019_z3_aux
    FOR ALL ENTRIES IN it_zlest0019_z1
  WHERE z3~dcl        EQ  it_zlest0019_z1-dcl_aux7
    AND z3~dtadecarga IN  p_erdatc
    AND z3~idinter    EQ 'L3'
    AND z3~tp_reg     EQ '20'.

  " Entrada 10 digitos
  SELECT z3~pesovagao z3~dtadecarga z3~horadescarga z3~dcl z3~idvagao z3~idinter z3~tp_reg z3~cnpjferro
    FROM zlest0019 AS z3
    APPENDING TABLE it_zlest0019_z3_aux
    FOR ALL ENTRIES IN it_zlest0019_z1
  WHERE z3~dcl        EQ  it_zlest0019_z1-dcl_aux10
    AND z3~dtadecarga IN  p_erdatc
    AND z3~idinter    EQ 'L3'
    AND z3~tp_reg     EQ '20'.

  LOOP AT it_zlest0019_z3_aux INTO wa_zlest0019_z3_aux.

    wa_zlest0019_z3-pesovagao      =  wa_zlest0019_z3_aux-pesovagao.
    wa_zlest0019_z3-dtadecarga     =  wa_zlest0019_z3_aux-dtadecarga.
    wa_zlest0019_z3-horadescarga   =  wa_zlest0019_z3_aux-horadescarga.
    wa_zlest0019_z3-dcl            =  wa_zlest0019_z3_aux-dcl.
    wa_zlest0019_z3-idvagao        =  wa_zlest0019_z3_aux-idvagao.
    wa_zlest0019_z3-idinter        =  wa_zlest0019_z3_aux-idinter.
    wa_zlest0019_z3-tp_reg         =  wa_zlest0019_z3_aux-tp_reg.
    wa_zlest0019_z3-cnpjferro      =  wa_zlest0019_z3_aux-cnpjferro.

    " Pegar as 3 primeiras caracteres da variavel.
    wa_zlest0019_z3-idvagao_letra = wa_zlest0019_z3-idvagao(3).

    " Pegar somente as caracteres após as 3 palavras iniciais
    wa_zlest0019_z3-idvagao_regex = wa_zlest0019_z3-idvagao+3.

    " Retirar o caracter "T" da variável.
    REPLACE REGEX 'T$' IN wa_zlest0019_z3-idvagao_regex WITH ''.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zlest0019_z3_aux-dcl
      IMPORTING
        output = wa_zlest0019_z3-dcl_aux.

    "alrs
    wa_dcl_5  = wa_zlest0019_z3-dcl_aux.
    wa_dcl_6  = wa_zlest0019_z3-dcl_aux.
    wa_dcl_7  = wa_zlest0019_z3-dcl_aux.
    wa_dcl_10 = wa_zlest0019_z3-dcl_aux.
    CONDENSE wa_dcl_5 NO-GAPS.
    CONDENSE wa_dcl_6 NO-GAPS.
    CONDENSE wa_dcl_7 NO-GAPS.
    CONDENSE wa_dcl_10 NO-GAPS.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dcl_5
      IMPORTING
        output = wa_dcl_5.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dcl_6
      IMPORTING
        output = wa_dcl_6.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dcl_7
      IMPORTING
        output = wa_dcl_7.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_dcl_10
      IMPORTING
        output = wa_dcl_10.

    wa_zlest0019_z3-dcl_aux  = wa_dcl_5.
    wa_zlest0019_z3-dcl_aux6 = wa_dcl_6.
    wa_zlest0019_z3-dcl_aux7 = wa_dcl_7.
    wa_zlest0019_z3-dcl_aux10 = wa_dcl_10.

    APPEND wa_zlest0019_z3 TO it_zlest0019_z3.

    CLEAR: wa_zlest0019_z3_aux,
           wa_zlest0019_z3.
  ENDLOOP.


  SORT: it_zlest0019_z3 BY idvagao_regex idvagao_letra dcl_aux6.

  LOOP AT it_zlest0019_z1 INTO wa_zlest0019_z1.
    "158502 CS2024001075 ZLES0057 Correções P1 PSA
    READ TABLE it_zlest0019_z3 INTO wa_zlest0019_z3 WITH KEY idvagao = wa_zlest0019_z1-idvagao dcl      = wa_zlest0019_z1-dcl BINARY SEARCH.

*    READ TABLE it_zlest0019_z3 INTO wa_zlest0019_z3 WITH KEY idvagao_regex = wa_zlest0019_z1-idvagao_regex idvagao_letra = wa_zlest0019_z1-idvagao_letra dcl_aux6      = wa_zlest0019_z1-dcl_aux6 BINARY SEARCH.
*    IF sy-subrc NE 0.
*      READ TABLE it_zlest0019_z3 INTO wa_zlest0019_z3 WITH KEY idvagao_regex = wa_zlest0019_z1-idvagao_regex idvagao_letra = wa_zlest0019_z1-idvagao_letra dcl_aux7      = wa_zlest0019_z1-dcl_aux7.
*      IF sy-subrc NE 0.
*        READ TABLE it_zlest0019_z3 INTO wa_zlest0019_z3 WITH KEY idvagao_regex = wa_zlest0019_z1-idvagao_regex idvagao_letra = wa_zlest0019_z1-idvagao_letra dcl_aux       = wa_zlest0019_z1-dcl_aux.
*        IF sy-subrc NE 0.
*          READ TABLE it_zlest0019_z3 INTO wa_zlest0019_z3 WITH KEY idvagao_regex = wa_zlest0019_z1-idvagao_regex idvagao_letra = wa_zlest0019_z1-idvagao_letra dcl_aux       = wa_zlest0019_z1-dcl_aux10.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    IF  ( NOT p_erdatc IS INITIAL ) AND ( wa_zlest0019_z3-dtadecarga NOT IN p_erdatc ).
*      CONTINUE.
*    ENDIF.


    IF ( sy-subrc EQ 0 ).

      wa_zlest0019_entrada-pesovagao      = wa_zlest0019_z3-pesovagao.
      wa_zlest0019_entrada-dtadecarga     = wa_zlest0019_z3-dtadecarga.
      wa_zlest0019_entrada-horadescarga   = wa_zlest0019_z3-horadescarga.
      wa_zlest0019_entrada-dcl            = wa_zlest0019_z3-dcl.
      wa_zlest0019_entrada-idvagao        = wa_zlest0019_z3-idvagao.
      wa_zlest0019_entrada-idinter        = wa_zlest0019_z3-idinter.
      wa_zlest0019_entrada-tp_reg         = wa_zlest0019_z3-tp_reg.
      wa_zlest0019_entrada-cnpjferro      = wa_zlest0019_z3-cnpjferro.


      wa_zlest0019_entrada-idvagao_regex  = wa_zlest0019_z3-idvagao_regex.
      wa_zlest0019_entrada-idvagao_letra  = wa_zlest0019_z3-idvagao_letra.
      wa_zlest0019_entrada-dcl_aux        = wa_zlest0019_z3-dcl_aux.


      APPEND wa_zlest0019_entrada TO it_zlest0019_entrada.

    ENDIF.

    CLEAR: wa_zlest0019_z1,
           wa_zlest0019_z3,
           wa_zlest0019_entrada.
  ENDLOOP.

  "check not it_zlest0019_entrada[] is initial.

  IF NOT (  it_zlest0019_entrada[] IS INITIAL ) .

    SELECT  name1 stcd1 kunnr ort01 lifnr
      FROM lfa1
      INTO TABLE it_lfa1_l3
      FOR ALL ENTRIES IN it_zlest0019_entrada
    WHERE stcd1 EQ it_zlest0019_entrada-cnpjferro.

    PERFORM: f_saida.

  ELSE.

    PERFORM: f_saida.

  ENDIF.
ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS2
*&---------------------------------------------------------------------*
FORM f_seleciona_dados2 .

  SELECT * FROM zi_zlest0019_dados_01( p_dtini_s = @p_erdats-low , p_dtfim_s = @p_erdats-high ) INTO TABLE @DATA(it_zlest0019).

  IF it_zlest0019 IS NOT INITIAL.
    SORT it_zlest0019 BY idvagao_s dcl.

    IF p_matnr[] IS NOT INITIAL.
      DELETE it_zlest0019 WHERE matnr NOT IN p_matnr[].
    ENDIF.

    IF p_bukrs[] IS NOT INITIAL.
      DELETE it_zlest0019 WHERE bukrs NOT IN p_bukrs[].
    ENDIF.

    IF p_werks[] IS NOT INITIAL.
      DELETE it_zlest0019 WHERE branch NOT IN p_werks[].
    ENDIF.

    IF p_zplaca[] IS NOT INITIAL.
      DELETE it_zlest0019 WHERE idvagao_e NOT IN p_zplaca[].
    ENDIF.

    IF p_dcl[] IS NOT INITIAL.
      DELETE it_zlest0019 WHERE dcl NOT IN p_dcl[].
    ENDIF.

    IF p_erdatc[] IS NOT INITIAL.
      DELETE it_zlest0019 WHERE dtadecarga_e NOT BETWEEN p_erdatc-low AND p_erdatc-high.
    ENDIF.

    FREE: it_saida2.

    IF it_zlest0019 IS NOT INITIAL.
      MOVE-CORRESPONDING it_zlest0019 TO it_saida2.
      FREE: it_1bnf,it_zlest0019.
      SELECT DISTINCT a~nfenum, b~docnum,a~bukrs, a~branch,b~name1, b~parid, c~meins, c~werks FROM j_1bnfdoc AS a
      INNER JOIN j_1bnfnad AS b ON b~parvw = 'Z1'
      INNER JOIN j_1bnflin AS c ON c~itmnum = '000010'
        FOR ALL ENTRIES IN @it_saida2
      WHERE a~nfenum = @it_saida2-nfenum
        AND a~branch = @it_saida2-branch
        AND a~bukrs = @it_saida2-bukrs
        AND b~docnum = @it_saida2-docnum
        AND c~docnum = @it_saida2-docnum
        AND a~cancel <> 'X'
        AND a~direct = '2'
      INTO TABLE @it_1bnf.

      DELETE ADJACENT DUPLICATES FROM it_1bnf.
    ENDIF.

    DATA: peso_int TYPE i.

    LOOP AT it_saida2 ASSIGNING FIELD-SYMBOL(<wa_saida2>).

      IF <wa_saida2>-dtadecarga_e IS NOT INITIAL .
        <wa_saida2>-dias_trans = <wa_saida2>-dtadecarga_e - <wa_saida2>-dtadecarga_s.
      ENDIF.

      IF <wa_saida2>-pesovagao_e IS NOT INITIAL.
        <wa_saida2>-pesovagao_d = <wa_saida2>-pesovagao_e - <wa_saida2>-pesovagao_s.
      ENDIF.

      IF <wa_saida2>-pesovagao_e IS NOT INITIAL AND <wa_saida2>-pesovagao_s IS NOT INITIAL AND <wa_saida2>-pesovagao_t IS NOT INITIAL.
        CLEAR:peso_int.
        peso_int = ( <wa_saida2>-pesovagao_e / <wa_saida2>-pesovagao_t ) *  <wa_saida2>-pesovagao_s. "media ponderada
        <wa_saida2>-pesovagao_m = peso_int.
        <wa_saida2>-pesovagao_d = <wa_saida2>-pesovagao_s - <wa_saida2>-pesovagao_m.
      ENDIF.

      IF <wa_saida2>-cnpjferro_s IS NOT INITIAL.
        SELECT SINGLE * FROM lfa1 WHERE stcd1 = @<wa_saida2>-cnpjferro_s INTO @DATA(wa_lfa1).
        <wa_saida2>-name1_s = wa_lfa1-name1.
        <wa_saida2>-lifnr_s = wa_lfa1-lifnr.
        <wa_saida2>-ort01_s = wa_lfa1-ort01.
      ENDIF.

      IF <wa_saida2>-docnum IS NOT INITIAL.

        CLEAR:wa_1bnf.
        READ TABLE it_1bnf INTO wa_1bnf WITH KEY nfenum = <wa_saida2>-nfenum docnum = <wa_saida2>-docnum branch = <wa_saida2>-branch bukrs = <wa_saida2>-bukrs.

        IF sy-subrc = 0.
          <wa_saida2>-name1_d = wa_1bnf-name1.
          <wa_saida2>-lifnr_d = wa_1bnf-parid.
          <wa_saida2>-meins_d = wa_1bnf-meins.
          <wa_saida2>-werks_d = wa_1bnf-werks.
        ENDIF.
      ENDIF.

      IF <wa_saida2>-cnpjferro_fr IS NOT INITIAL.
        SELECT SINGLE * FROM lfa1 WHERE stcd1 = @<wa_saida2>-cnpjferro_s INTO @DATA(wa_lfa1_fr).
        <wa_saida2>-name1_E = wa_lfa1_fr-name1.
        <wa_saida2>-lifnr_E = wa_lfa1_fr-lifnr.
        <wa_saida2>-ort01_E = wa_lfa1_fr-ort01.
      ENDIF.

      CASE <wa_saida2>-status_duplica.
        WHEN 1.
          <wa_saida2>-duplic = icon_green_light.
        WHEN 2.
          <wa_saida2>-duplic = icon_red_light.
      ENDCASE.



    ENDLOOP.

    IF p_kunnr[] IS NOT INITIAL. " Local de Carregamento
      DELETE it_saida2 WHERE lifnr_s NOT IN p_kunnr[].
    ENDIF.

    IF p_lifnr[] IS NOT INITIAL. " Terminal de Descarga
      DELETE it_saida2 WHERE lifnr_e NOT IN p_lifnr[].
    ENDIF.

    IF p_lifnr[] IS NOT INITIAL. " Destinatário
      DELETE it_saida2 WHERE lifnr_d NOT IN p_lifnr[].
    ENDIF.

  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS2

*&----------------------------------------------------------------------*
*&      Form  F_SAIDA
*&----------------------------------------------------------------------*
FORM f_saida .

  CLEAR: dia, mes, ano.

  SORT: it_zlest0019_z2      BY dcl seriedcl id_refkey,
        it_j_1bnfdoc         BY bukrs branch nfenum,
        it_j_1bnflin         BY docnum mandt,
        it_makt              BY matnr,
        it_carregamento      BY bukrs branch nfenum cnpjcliente,
        it_lfa1              BY stcd1,
        it_zlest0019_entrada BY dcl_aux idvagao_regex idvagao_letra,
        it_lfa1_l3           BY stcd1,
        it_j_1bnfnad         BY docnum.


  LOOP AT it_zlest0019_z1 INTO wa_zlest0019_z1.

    wa_saida-idvagao_s      = wa_zlest0019_z1-idvagao.  " ID Vagão
    wa_saida-pesovagao_s    = wa_zlest0019_z1-pesovagao." Peso Vagão Saída
    wa_saida-duplic         = wa_zlest0019_z1-status_duplica.
    wa_saida-observacao     = wa_zlest0019_z1-observacao.

    "ALRS (BPC)
    wa_saida-idinter     = wa_zlest0019_z1-idinter.
    wa_saida-tp_movi     = wa_zlest0019_z1-tp_movi.
    wa_saida-tp_reg      = wa_zlest0019_z1-tp_reg.
    wa_saida-chave       = wa_zlest0019_z1-chave.
    wa_saida-dcl         = wa_zlest0019_z1-dcl.
    wa_saida-seriedcl    = wa_zlest0019_z1-seriedcl.


    " Tratamento de Data de Saída com as BARRAS!
    wa_saida-dtadecarga_s_aux = wa_zlest0019_z1-dtadecarga.
    dia = wa_zlest0019_z1-dtadecarga+6(2).
    mes = wa_zlest0019_z1-dtadecarga+4(2).
    ano = wa_zlest0019_z1-dtadecarga(4).
    CONCATENATE dia '/' mes '/' ano INTO wa_saida-dtadecarga_s.

    wa_saida-horadescarga_s = wa_zlest0019_z1-horadescarga.
    " Dados de Saída
    READ TABLE it_zlest0019_z2 INTO wa_zlest0019_z2 WITH KEY dcl       = wa_zlest0019_z1-dcl
                                                             seriedcl  = wa_zlest0019_z1-seriedcl
                                                             id_refkey = wa_zlest0019_z1-id_refkey BINARY SEARCH.
    wa_saida-tp_movi_s   = 'Saída'. " Tipo de Movimento
    wa_saida-bukrs       = wa_zlest0019_z2-bukrs.
    wa_saida-branch      = wa_zlest0019_z2-branch.
    wa_saida-dcl_s       = wa_zlest0019_z2-dcl.
    wa_saida-seriedcl_s  = wa_zlest0019_z2-seriedcl.

    READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY bukrs  = wa_zlest0019_z2-bukrs
                                                       branch = wa_zlest0019_z2-branch
                                                       nfenum = wa_zlest0019_z2-nfenum BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).


      READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.

      IF  ( NOT p_lifnrf IS INITIAL ) AND ( wa_j_1bnfnad-parid NOT IN p_lifnrf ).
        CONTINUE.
      ENDIF.

      READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY docnum = wa_j_1bnfdoc-docnum
                                                         mandt  = wa_j_1bnfdoc-mandt BINARY SEARCH.

      wa_saida-meins  = wa_j_1bnflin-meins.
      wa_saida-werks  = wa_j_1bnflin-werks.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_j_1bnflin-matnr BINARY SEARCH.

      IF  ( NOT p_matnr IS INITIAL ) AND ( wa_makt-matnr NOT IN p_matnr ).
        CONTINUE.
      ENDIF.

      wa_saida-name1_d = wa_j_1bnfnad-name1. " destinatário
      wa_saida-matnr   = wa_makt-matnr.
      wa_saida-maktx   = wa_makt-maktx.
      wa_saida-lifnr_d = wa_j_1bnfnad-parid.
    ENDIF.

    READ TABLE it_carregamento INTO wa_carregamento WITH KEY bukrs       = wa_zlest0019_z2-bukrs
                                                             branch      = wa_zlest0019_z2-branch
                                                             nfenum      = wa_zlest0019_z2-nfenum
                                                             cnpjcliente = wa_zlest0019_z2-cnpjcliente BINARY SEARCH.
    IF ( sy-subrc EQ 0 ).

      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY stcd1 = wa_carregamento-cnpjferro BINARY SEARCH.
      wa_saida-name1_s = wa_lfa1-name1. " Local de Carregamento
      wa_saida-ort01_s = wa_lfa1-ort01.
      wa_saida-lifnr_s = wa_lfa1-lifnr.

      " Dados de Entrada
      READ TABLE it_zlest0019_entrada INTO wa_zlest0019_entrada WITH KEY dcl       =  wa_zlest0019_z1-dcl
                                                                         idvagao =  wa_zlest0019_z1-idvagao. "158502 CS2024001075 ZLES0057 Correções P1 PSA
      "idvagao_letra =  wa_zlest0019_z1-idvagao_letra BINARY SEARCH.

      IF ( NOT p_erdatc IS INITIAL ) AND ( wa_zlest0019_entrada-dtadecarga NOT IN p_erdatc ).
        CONTINUE.
      ENDIF.

      " Busca dados da entrada do vagão
      PERFORM: dados_entrada.

    ENDIF.

    CLEAR:
          wa_zlest0019_z2,
          wa_j_1bnfdoc,
          wa_j_1bnflin,
          wa_makt,
          wa_carregamento,
          wa_lfa1,
          wa_zlest0019_entrada,
          wa_lfa1_l3,
          wa_saida,
          wa_saida_aux,
          wa_j_1bnfnad.


  ENDLOOP.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  DADOS_ENTRADA
*&---------------------------------------------------------------------*
FORM dados_entrada.

  wa_saida-tp_movi_e = 'Entrada'.

  IF ( sy-subrc EQ 0 ).
    wa_saida-idvagao_e         =  wa_zlest0019_entrada-idvagao.         " ID Vagão Entrada
    wa_saida-pesovagao_e       =  wa_zlest0019_entrada-pesovagao.       " Peso Vagão Entrada

    CLEAR: dia, mes, ano.

    " Tratamento de Data de Entrada com as BARRAS!
    dia = wa_zlest0019_entrada-dtadecarga+6(2).
    mes = wa_zlest0019_entrada-dtadecarga+4(2).
    ano = wa_zlest0019_entrada-dtadecarga(4).
    CONCATENATE dia '/' mes '/' ano INTO wa_saida-dtadecarga_e.

    wa_saida-dtadecarga_e_aux = wa_zlest0019_entrada-dtadecarga.

    wa_saida-horadescarga_e    =  wa_zlest0019_entrada-horadescarga.    " Hora de Carga Entrada
    wa_saida-dcl_e             =  wa_zlest0019_entrada-dcl.             " Peso Vagão Entrada

    READ TABLE it_lfa1_l3 INTO wa_lfa1_l3 WITH KEY stcd1 = wa_zlest0019_entrada-cnpjferro BINARY SEARCH.

    IF  ( ( sy-subrc EQ 0 ) AND ( NOT wa_zlest0019_entrada-cnpjferro IS INITIAL ) ).

      wa_saida-name1_e           =  wa_lfa1_l3-name1.
      wa_saida-ort01_e           =  wa_lfa1_l3-ort01.
      wa_saida-lifnr_e           =  wa_lfa1_l3-lifnr.

    ENDIF.
  ENDIF.

  wa_saida-dias_trans =  wa_zlest0019_entrada-dtadecarga -  wa_zlest0019_z1-dtadecarga.

  IF ( wa_saida-dtadecarga_e IS INITIAL ).
    wa_saida-dias_trans = 0.
  ENDIF.

  APPEND wa_saida TO it_saida_soma.
  APPEND wa_saida TO it_saida_aux.

ENDFORM.                    " DADOS_ENTRADA

*&---------------------------------------------------------------------*
*&      Form  P_SOMA_PESO_SAIDA
*&---------------------------------------------------------------------*
FORM p_soma_peso_saida .

  DATA: total_s TYPE p,
        total_e TYPE p,
        peso_e  TYPE p.


  " Variaveis para dados de entrada quando existir vagões repetidos na mesma data.

  DATA:
    lifnr_e          TYPE lfa1-lifnr,
    name1_e          TYPE lfa1-name1,
    ort01_e          TYPE lfa1-ort01,
    idvagao_e        TYPE zlest0019-idvagao,
    dtadecarga_e(10) TYPE c, " Data / Entrada
    horadescarga_e   TYPE zlest0019-horadescarga,
    pesovagao_e      TYPE zlest0019-pesovagao,
    dcl_e            TYPE zlest0019-dcl,
    dias_trans_e     TYPE p.

  CLEAR: wa_saida_soma, wa_saida_aux.


  SORT: it_saida_aux BY idvagao_s dtadecarga_s_aux,
        it_saida     BY idvagao_s dtadecarga_s_aux.


  LOOP AT it_saida_soma INTO wa_saida_soma.

    "total = 0. "158502 CS2024001075 ZLES0057 Correções P1 PSA

    "READ TABLE it_saida INTO wa_saida WITH KEY idvagao_s        = wa_saida_soma-idvagao_s dtadecarga_s_aux = wa_saida_soma-dtadecarga_s_aux. 158502 CS2024001075 ZLES0057 Correções P1 PSA
    "IF ( sy-subrc NE 0 ).158502 CS2024001075 ZLES0057 Correções P1 PSA

    LOOP AT it_saida_aux INTO wa_saida_aux WHERE idvagao_s        EQ wa_saida_soma-idvagao_s
AND dtadecarga_s_aux EQ wa_saida_soma-dtadecarga_s_aux."158502 CS2024001075 ZLES0057 Correções P1 PSA

      total_s = ( total_s + wa_saida_aux-pesovagao_s ). "158502 CS2024001075 ZLES0057 Correções P1 PSA
      total_e = ( total_e + wa_saida_aux-pesovagao_e ). "158502 CS2024001075 ZLES0057 Correções P1 PSA

      IF NOT ( wa_saida_aux-pesovagao_e IS INITIAL ).

        peso_e         = wa_saida_aux-pesovagao_e.
        lifnr_e        = wa_saida_aux-lifnr_e.
        name1_e        = wa_saida_aux-name1_e.
        ort01_e        = wa_saida_aux-ort01_e.
        idvagao_e      = wa_saida_aux-idvagao_e.
        dtadecarga_e   = wa_saida_aux-dtadecarga_e.
        horadescarga_e = wa_saida_aux-horadescarga_e.
        pesovagao_e    = wa_saida_aux-pesovagao_e.
        dcl_e          = wa_saida_aux-dcl_e.

        dias_trans_e   = dias_trans_e + wa_saida_aux-dias_trans.

      ELSE.
        wa_saida_aux-dias_trans = sy-datum - wa_saida_soma-dtadecarga_s_aux.
      ENDIF.

    ENDLOOP.

    wa_saida_aux-pesovagao_s_soma = total_s.

    IF NOT ( peso_e IS INITIAL ).

      "wa_saida_aux-peso_diferenca = ( peso_e - total ). "158502 CS2024001075 ZLES0057 Correções P1 PSA
      wa_saida_aux-peso_diferenca = total_s - total_e. "158502 CS2024001075 ZLES0057 Correções P1 PSA

      wa_saida_aux-lifnr_e             = lifnr_e.
      wa_saida_aux-name1_e             = name1_e.
      wa_saida_aux-ort01_e             = ort01_e.
      wa_saida_aux-idvagao_e           = idvagao_e.
      wa_saida_aux-dtadecarga_e        = dtadecarga_e.
      wa_saida_aux-horadescarga_e      = horadescarga_e.
      wa_saida_aux-pesovagao_e         = total_e."pesovagao_e. "158502 CS2024001075 ZLES0057 Correções P1 PSA
      wa_saida_aux-dcl_e               = dcl_e.
      wa_saida_aux-pesovagao_s         = total_s."158502 CS2024001075 ZLES0057 Correções P1 PSA
      IF NOT ( dias_trans_e IS INITIAL ).
        wa_saida_aux-dias_trans          = dias_trans_e.
      ELSE.
        wa_saida_aux-dias_trans = sy-datum - wa_saida_soma-dtadecarga_s_aux.
      ENDIF.

      CLEAR: lifnr_e,
             name1_e,
             ort01_e,
             idvagao_e,
             dtadecarga_e,
             horadescarga_e,
             pesovagao_e,
             dcl_e,
             peso_e,
             dias_trans_e.
    ELSE.

      wa_saida_aux-peso_diferenca = total_s.

      CLEAR:  wa_saida_aux-lifnr_e,
              wa_saida_aux-name1_e,
              wa_saida_aux-ort01_e,
              wa_saida_aux-idvagao_e,
              wa_saida_aux-dtadecarga_e,
              wa_saida_aux-horadescarga_e,
              wa_saida_aux-pesovagao_e,
              wa_saida_aux-dcl_e.

    ENDIF.

    " Verificação de duplicação do vagão
    " 1 = Não duplicado
    " 2 = O mesmo vagão duplicado na mesma data de saída.

    CASE wa_saida_soma-duplic.
      WHEN 1.
        wa_saida_aux-duplic = icon_green_light.
      WHEN 2.
        wa_saida_aux-duplic = icon_red_light.
    ENDCASE.

    APPEND wa_saida_aux TO it_saida.

    CLEAR: wa_saida_soma, wa_saida_aux, total_s,  total_e, peso_e.
    "ENDIF.
  ENDLOOP.

  "ALRS
  d_30 = p_base-low - 30.
  d_60 = p_base-low - 60.
  IF NOT ( s_trans IS INITIAL ).
    DELETE it_saida WHERE dtadecarga_e      NE ''. "processar somente data entrada  = ''.
    DELETE it_saida WHERE dtadecarga_s_aux  GT p_base-low. "processar somente Data saida <= Data Base
    DELETE it_saida WHERE dtadecarga_s_aux  LT d_60. " Eliminar as saidas anteriores a 60 dias
  ELSEIF c_bpc = 'X' OR sy-batch = 'X'.
    it_saida_079c[] = it_saida[].
    "DELETE IT_SAIDA_079C WHERE DTADECARGA_S_AUX NE P_BASE-LOW.
    it_saida_079d[] = it_saida[].
    "DELETE IT_SAIDA_079D WHERE DTADECARGA_E_AUX NE P_BASE-LOW.
    DELETE it_saida_079d WHERE dtadecarga_e EQ ''.
  ENDIF.

  IF c_bpc = 'X' OR sy-batch = 'X'.
    REFRESH it_zlest0079.
    " SOMENTE TRANSITO
    DELETE FROM zlest0079 WHERE data_base LE d_60 OR data_base IS NULL.
    IF  NOT ( s_trans IS INITIAL ).
      LOOP AT it_saida INTO wa_saida.
        wa_zlest0079-data_base       = p_base-low.
        wa_zlest0079-idinter         = wa_saida-idinter.
        wa_zlest0079-tp_movi         = wa_saida-tp_movi.
        wa_zlest0079-tp_reg          = wa_saida-tp_reg.
        wa_zlest0079-chave           = wa_saida-chave.
        wa_zlest0079-dcl             = wa_saida-dcl.
        wa_zlest0079-seriedcl        = wa_saida-seriedcl.

        wa_zlest0079-data_saida      = wa_saida-dtadecarga_s_aux.
        "WA_ZLEST0079-DATA_CHEGADA    = WA_SAIDA-DTADECARGA_E_AUX.
        wa_zlest0079-id              = 2. "TRANSITO

        wa_zlest0079-empresa         = wa_saida-bukrs.
        wa_zlest0079-origem          = wa_saida-lifnr_s.
        wa_zlest0079-tipo_produto    = 'CO'.
        wa_zlest0079-destino         = wa_saida-lifnr_d.
        wa_zlest0079-produto         = wa_saida-matnr.
        wa_zlest0079-quantidade      = wa_saida-pesovagao_s_soma.
        wa_zlest0079-unidade_medida  = wa_saida-meins.
        wa_zlest0079-werks           = wa_saida-werks.

        APPEND wa_zlest0079 TO it_zlest0079.
        CLEAR wa_zlest0079.
      ENDLOOP.
      MODIFY zlest0079 FROM TABLE it_zlest0079.
    ELSE.
      it_saida_079[] = it_saida_079c[].
      SORT it_saida_079 BY chave.
      DELETE ADJACENT DUPLICATES FROM it_saida_079 COMPARING chave.
      LOOP AT it_saida_079 INTO wa_saida.
        DELETE FROM zlest0079 WHERE chave = wa_saida-chave AND id  = 1 .
      ENDLOOP.
      REFRESH it_saida_079.
      "Somente Carga
      LOOP AT it_saida_079c INTO wa_saida.
        wa_zlest0079-data_base       = p_base-low.
        wa_zlest0079-idinter         = wa_saida-idinter.
        wa_zlest0079-tp_movi         = wa_saida-tp_movi.
        wa_zlest0079-tp_reg          = wa_saida-tp_reg.
        wa_zlest0079-chave           = wa_saida-chave.
        wa_zlest0079-dcl             = wa_saida-dcl.
        wa_zlest0079-seriedcl        = wa_saida-seriedcl.
        wa_zlest0079-data_saida      = wa_saida-dtadecarga_s_aux.
*        WA_ZLEST0079-DATA_CHEGADA    = WA_SAIDA-DTADECARGA_E_AUX.
        wa_zlest0079-id              = 1. "CARGA
        wa_zlest0079-empresa         = wa_saida-bukrs.
        wa_zlest0079-origem          = wa_saida-lifnr_s.
        wa_zlest0079-tipo_produto    = 'CO'.
        wa_zlest0079-destino         = wa_saida-lifnr_d.
        wa_zlest0079-produto         = wa_saida-matnr.
        wa_zlest0079-quantidade      = wa_saida-pesovagao_s_soma.
        wa_zlest0079-unidade_medida  = wa_saida-meins.
        wa_zlest0079-werks           = wa_saida-werks.

        APPEND wa_zlest0079 TO it_zlest0079.
        CLEAR wa_zlest0079.
      ENDLOOP.
      it_saida_079[] = it_saida_079d[].
      SORT it_saida_079 BY chave .
      DELETE ADJACENT DUPLICATES FROM it_saida_079 COMPARING chave.
      LOOP AT it_saida_079 INTO wa_saida.
        DELETE FROM zlest0079 WHERE chave = wa_saida-chave AND id  = 3 .
      ENDLOOP.
      REFRESH it_saida_079.
      "Somente Descarga
      LOOP AT it_saida_079d INTO wa_saida.
        wa_zlest0079-data_base       = p_base-low.
        wa_zlest0079-idinter         = wa_saida-idinter.
        wa_zlest0079-tp_movi         = wa_saida-tp_movi.
        wa_zlest0079-tp_reg          = wa_saida-tp_reg.
        wa_zlest0079-chave           = wa_saida-chave.
        wa_zlest0079-dcl             = wa_saida-dcl.
        wa_zlest0079-seriedcl        = wa_saida-seriedcl.
        wa_zlest0079-data_saida      = wa_saida-dtadecarga_s_aux.
        wa_zlest0079-data_chegada    = wa_saida-dtadecarga_e_aux.
        wa_zlest0079-id              = 3. "DESCARGA
        wa_zlest0079-empresa         = wa_saida-bukrs.
        wa_zlest0079-origem          = wa_saida-lifnr_s.
        wa_zlest0079-tipo_produto    = 'CO'.
        wa_zlest0079-destino         = wa_saida-lifnr_d.
        wa_zlest0079-produto         = wa_saida-matnr.
        wa_zlest0079-quantidade      = wa_saida-pesovagao_e.
        wa_zlest0079-unidade_medida  = wa_saida-meins.
        wa_zlest0079-werks           = wa_saida-werks.

        APPEND wa_zlest0079 TO it_zlest0079.
        CLEAR wa_zlest0079.
      ENDLOOP.
      MODIFY zlest0079 FROM TABLE it_zlest0079.
      " Apaga os que não existem mais na 19
      REFRESH it_zlest0079.
      SELECT *
          FROM  zlest0079
          INTO TABLE it_zlest0079
          WHERE idinter IN ('L1', 'L2','L3')
          AND   id = '1'
          AND NOT EXISTS (  SELECT *
                               FROM zlest0019
                               WHERE zlest0019~idinter = zlest0079~idinter
                               AND   zlest0019~tp_movi = zlest0079~tp_movi
                               AND   zlest0019~tp_reg  = zlest0079~tp_reg
                               AND   zlest0019~chave   = zlest0079~chave
                               AND   zlest0019~dcl     = zlest0079~dcl ).

      LOOP AT it_zlest0079 INTO wa_zlest0079.
        DELETE FROM zlest0079
        WHERE idinter = wa_zlest0079-idinter
        AND   tp_movi = wa_zlest0079-tp_movi
        AND   tp_reg  = wa_zlest0079-tp_reg
        AND   chave   = wa_zlest0079-chave
        AND   dcl     = wa_zlest0079-dcl.
      ENDLOOP.
    ENDIF.
    REFRESH: it_saida_079c,it_saida_079d.
  ENDIF.
  " Deletar registros duplicados comparando o IDVAGÃO e DATADECARGA da saída.
  DELETE ADJACENT DUPLICATES FROM it_saida COMPARING idvagao_s
                                                     dtadecarga_s_aux.

ENDFORM.                    " P_SOMA_PESO_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM f_alv .
  PERFORM alv_preenche_cat USING:

          " Saída
          'BUKRS'           TEXT-011   '5'  ' '  ' ', " Empresa
          'BRANCH'          TEXT-040   '10'  ' '  ' ', " Empresa
          'TP_MOVI_S'       TEXT-012   '7'  ' '  ' ', " Tp. Movimento - 'Saída'
          'LIFNR_S'         TEXT-032   '7'  ' '  'X',  " Código do Cliente - 'Saída'
          'NAME1_S'         TEXT-013   '25' ' '  ' ', " Local Carregamento
          'NAME1_D'         TEXT-039   '25' ' '  ' ', " Destinatário
          'ORT01_S'         TEXT-033   '25' ' '  ' ', " Endereço do Cliente - 'Saída'
          'IDVAGAO_S'       TEXT-014   '12' 'X'  ' ', " ID Vagão
          'DUPLIC'          TEXT-031   '5'  ' '  ' ', " Status de Duplicação
          'DTADECARGA_S'    TEXT-018   '10' ' '  ' ', " Data Saída
          'HORADESCARGA_S'  TEXT-019   '6'  ' '  ' ', " Hora Saída
          'PESOVAGAO_S_SOMA' TEXT-015   '7' ' '  ' ', " Peso Saída Vagão
          "'DCL_S'           TEXT-016   '6'  ' '  'X', " DCL
          'SERIEDCL_S'      TEXT-017   '4'  ' '  ' ', " Série DCL
          'MATNR'           TEXT-029   '7'  ' '  'X', " Código Material
          'MAKTX'           TEXT-030   '28' ' '  ' ', " Descrição do Material

          " Entrada
          'TP_MOVI_E'       TEXT-020   '7'  ' '  ' ', " Tp. Movimento - 'Entrada'
          'LIFNR_E'         TEXT-034   '7'  ' '  'X',  " Código do Cliente - 'Saída'
          'NAME1_E'         TEXT-021   '25' ' '  ' ', " Terminal Descarga
          'ORT01_E'         TEXT-033   '25' ' '  ' ', " Endereço do Cliente - 'Saída'
          'IDVAGAO_E'       TEXT-022   '12' ' '  ' ', " ID Vagão
          'DTADECARGA_E'    TEXT-025   '10' ' '  ' ', " Data Chegada
          'HORADESCARGA_E'  TEXT-026   '6'  ' '  ' ', " Hora Chegada
          'PESOVAGAO_E'     TEXT-024   '7'  ' '  ' ', " Peso Chegada Vagão
          "'DCL_E'           TEXT-023   '6'  ' '  ' ', " DCL
          'DIAS_TRANS'      TEXT-027   '3'  ' '  ' ', " Dias Transito
          'PESO_DIFERENCA'  TEXT-035   '7'  ' '  ' ', " Quebra de Peso
          'OBSERVACAO'      TEXT-038   '30' ' '  ' '.
  "''  text-028   ''   ' '  ' '   . " Observações
ENDFORM.         " F_ALV

*&---------------------------------------------------------------------*
*&      Form  F_ALV2
*&---------------------------------------------------------------------*
FORM f_alv2 .
  PERFORM alv_preenche_cat USING:

          " Saída
          'BUKRS'           TEXT-011   '5'  ' '  ' ', " Empresa
          'BRANCH'          TEXT-040   '10'  ' '  ' ', " Empresa
          'DOCNUM'          TEXT-042   '10'  ' '  ' ', " Docnum - 'Saída'
          'NFENUM'          TEXT-041   '10'  ' '  ' ', " Nfenum - 'Saída'
          'TP_MOVI_S'       TEXT-012   '7'  ' '  ' ', " Tp. Movimento - 'Saída'
          'LIFNR_S'         TEXT-032   '7'  ' '  'X',  " Código do Cliente - 'Saída'
          'NAME1_S'         TEXT-013   '25' ' '  ' ', " Local Carregamento
          'NAME1_D'         TEXT-039   '25' ' '  ' ', " Destinatário
          'ORT01_S'         TEXT-033   '25' ' '  ' ', " Endereço do Cliente - 'Saída'
          "'IDVAGAO_S'       TEXT-014   '12' 'X'  ' ', " ID Vagão
          'IDVAGAO_S'       TEXT-014   '12' ' '  ' ', " ID Vagão
          'DUPLIC'          TEXT-031   '5'  ' '  ' ', " Status de Duplicação
          'DTADECARGA_S'    TEXT-018   '10' ' '  ' ', " Data Saída
          'HORADESCARGA_S'  TEXT-019   '6'  ' '  ' ', " HORA SAÍDA
          'PESOVAGAO_S'     TEXT-015   '7' ' '  ' ', " Peso Saída Vagão
          'DCL'             TEXT-016   '6'  ' '  'X', " DCL
          'SERIEDCL'        TEXT-017   '4'  ' '  ' ', " Série DCL
          'MATNR'           TEXT-029   '7'  ' '  'X', " Código Material
          'MAKTX'           TEXT-030   '28' ' '  ' ', " Descrição do Material

          " Entrada
          'TP_MOVI_E'       TEXT-020   '7'  ' '  ' ', " Tp. Movimento - 'Entrada'
          'TP_MOVI_E'       TEXT-020   '7'  ' '  ' ', " Tp. Movimento - 'Entrada'
          'LIFNR_E'         TEXT-034   '7'  ' '  'X',  " Código do Cliente - 'Saída'
          'NAME1_E'         TEXT-021   '25' ' '  ' ', " Terminal Descarga
          'ORT01_E'         TEXT-033   '25' ' '  ' ', " Endereço do Cliente - 'Saída'
          'IDVAGAO_E'       TEXT-022   '12' ' '  ' ', " ID Vagão
          'DTADECARGA_E'    TEXT-025   '10' ' '  ' ', " Data Chegada
          'HORADESCARGA_E'  TEXT-026   '6'  ' '  ' ', " Hora Chegada
          'PESOVAGAO_M'     TEXT-024   '7'  ' '  ' ', " Peso Chegada Vagão
          "'DCL_E'           TEXT-023   '6'  ' '  ' ', " DCL
          'DIAS_TRANS'      TEXT-027   '3'  ' '  ' ', " Dias Transito
          'PESOVAGAO_D'  TEXT-035   '7'  ' '  ' ', " Quebra de Peso
          'OBSERVACAO'      TEXT-038   '30' ' '  ' '.
  "''  text-028   ''   ' '  ' '   . " Observações
ENDFORM.         " F_ALV2

*&---------------------------------------------------------------------*
*&      Form  alv_preenche_cat
*&---------------------------------------------------------------------*
FORM alv_preenche_cat    USING p_campo TYPE c
                               p_desc  TYPE c

                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.



  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " Z_STATUS  OUTPUT

MODULE z_status400 OUTPUT.
  SET PF-STATUS 'FF0400'.
  SET TITLEBAR  'TB0400'.
ENDMODULE.                 " Z_STATUS  OUTPUT

CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA wa_event       TYPE REF TO lcl_event_receiver.
DATA wa_event_group TYPE REF TO lcl_event_receiver.
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no,
      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive,
      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_exibe_alv OUTPUT.

  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF wa_alv IS INITIAL AND NOT
    wa_cont IS INITIAL.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.

  wa_layout-cwidth_opt = 'X'. "Ajusta tamanho na coluna
  wa_layout-col_opt    = 'X'. "Ajusta tamanho na coluna

  wa_layout-sel_mode   = 'A'.
  wa_layout-box_fname  = 'SELECTED'.

  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = gs_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv IS INITIAL.

ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT

MODULE z_exibe_alv400 OUTPUT.

  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF wa_alv IS INITIAL AND NOT
    wa_cont IS INITIAL.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.

  wa_layout-cwidth_opt = 'X'. "Ajusta tamanho na coluna
  wa_layout-col_opt    = 'X'. "Ajusta tamanho na coluna

  wa_layout-sel_mode   = 'A'.
  wa_layout-box_fname  = 'SELECTED'.

  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = gs_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida2
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv IS INITIAL.

ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF ( sy-dynnr EQ '0100' ) OR ( sy-dynnr EQ '0200' ) OR ( sy-dynnr EQ '0400' ).
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
FORM z_handle_hotspot  USING     p_e_row_id    TYPE  lvc_s_row
                                 p_e_column_id TYPE  lvc_s_col
                                 p_es_row_no   TYPE  lvc_s_roid.


  READ TABLE it_saida INTO wa_saida INDEX p_e_row_id.

  CASE p_e_column_id.

    WHEN: 'IDVAGAO_S'.

      CLEAR: it_saida_group,
             wa_saida_group.

      PERFORM: saida_group USING wa_saida-idvagao_s
                                 wa_saida-dtadecarga_s_aux.
      PERFORM: saida_group_alv.
      CALL SCREEN 0200.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.
  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

  CLEAR sl_toolbar.
  MOVE: 'EDIT'                 TO sl_toolbar-function ,
         icon_change           TO sl_toolbar-icon     ,
         TEXT-036              TO sl_toolbar-quickinfo,
         TEXT-036              TO sl_toolbar-text     ,
         space                 TO sl_toolbar-disabled .

  APPEND sl_toolbar TO p_object->mt_toolbar.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.
ENDFORM.                    " Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
FORM z_handle_command  USING    p_e_ucomm TYPE syucomm.

  CASE p_e_ucomm.
    WHEN: 'EDIT'.
      PERFORM: edit_ferroviario.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND


*&---------------------------------------------------------------------*
*&      Form  SAIDA_GROUP
*&---------------------------------------------------------------------*
*      -->P_WA_SAIDA_IDVAGAO_S  text
*      -->P_WA_SAIDA_DTADECARGA_S_AUX  text
*----------------------------------------------------------------------*
FORM saida_group  USING    p_wa_saida_idvagao_s
                           p_wa_saida_dtadecarga_s_aux.

  CLEAR: wa_saida_aux,
         wa_saida_group.

  LOOP AT it_saida_aux INTO wa_saida_aux WHERE idvagao_s        EQ  p_wa_saida_idvagao_s
                                           AND dtadecarga_s_aux EQ  p_wa_saida_dtadecarga_s_aux.

    APPEND wa_saida_aux TO it_saida_group.

    CLEAR: wa_saida,
           wa_saida_group.
  ENDLOOP.

  CLEAR: it_fcat_group.

ENDFORM.                    " SAIDA_GROUP_X
*&---------------------------------------------------------------------*
*&      Form  SAIDA_GROUP_ALV
*&---------------------------------------------------------------------*
FORM saida_group_alv .

  PERFORM alv_preenche_cat_saida_group USING:

        " Saída
        'BUKRS'           TEXT-011   '5'  ' '  ' ' ' ', " Empresa
        'BRANCH'          TEXT-040   '10' ' '  ' ' ' ', " Empresa
        'TP_MOVI_S'       TEXT-012   '7' ' '  ' ' ' ', " Tp. Movimento - 'Saída'
        'LIFNR_S'         TEXT-032   '7' ' '  'X' ' ',  " Código do Cliente - 'Saída'
        'NAME1_S'         TEXT-013   '25' ' '  ' ' ' ', " Local Carregamento
        'NAME1_D'         TEXT-039   '25' ' '  ' ' ' ', " Destinatário
        'ORT01_S'         TEXT-033   '25' ' '  ' ' ' ', " Municipio - 'Saída'
        'IDVAGAO_S'       TEXT-014   '12' ' '  ' ' ' ', " ID Vagão
        'DTADECARGA_S'    TEXT-018   '10' ' '  ' ' ' ', " Data Saída
        'HORADESCARGA_S'  TEXT-019   '6'  ' '  ' ' ' ', " Hora Saída
        'PESOVAGAO_S'     TEXT-015   '7' ' '  ' ' 'X', " Peso Saída Vagão
        'DCL_S'           TEXT-016   '6' ' '  'X' ' ', " DCL
        'SERIEDCL_S'      TEXT-017   '4'  ' '  ' ' ' ', " Série DCL
        'MATNR'           TEXT-029   '7' ' '  'X' ' ', " Código Material
        'MAKTX'           TEXT-030   '28' ' '  ' ' ' ', " Descrição do Material

        " Entrada
        'TP_MOVI_E'       TEXT-020   '7' ' '  ' ' ' ', " Tp. Movimento - 'Entrada'
        'LIFNR_E'         TEXT-034   '7' ' '  'X' ' ',  " Código do Cliente - 'Saída'
        'NAME1_E'         TEXT-021   '25' ' '  ' ' ' ', " Terminal Descarga
        'ORT01_E'         TEXT-033   '25' ' '  ' ' ' ', " Endereço do Cliente - 'Saída'
        'IDVAGAO_E'       TEXT-022   '12' ' '  ' ' ' ', " ID Vagão
        'DTADECARGA_E'    TEXT-025   '10' ' '  ' ' ' ', " Data Chegada
        'HORADESCARGA_E'  TEXT-026   '6'  ' '  ' ' ' ', " Hora Chegada
        'PESOVAGAO_E'     TEXT-024   '7' ' '  ' ' 'X', " Peso Chegada Vagão
        'DCL_E'           TEXT-023   '6' ' '  ' ' ' ', " DCL
        'DIAS_TRANS'      TEXT-027   '3'   ' '  ' ' ' '. " Dias Transito

ENDFORM.                    " SAIDA_GROUP_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT_SAIDA_GROUP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM alv_preenche_cat_saida_group   USING p_campo TYPE c
                                          p_desc  TYPE c
                                          p_tam   TYPE c
                                          p_hot   TYPE c
                                          p_zero  TYPE c
                                          p_soma  TYPE c.



  DATA: wl_fcat_group TYPE lvc_s_fcat.

  wl_fcat_group-tabname   = 'IT_SAIDA_GROUP'.
  wl_fcat_group-fieldname = p_campo.
  wl_fcat_group-scrtext_l = p_desc.
  wl_fcat_group-scrtext_m = p_desc.
  wl_fcat_group-scrtext_s = p_desc.
  wl_fcat_group-outputlen = p_tam.
  wl_fcat_group-hotspot   = p_hot.
  wl_fcat_group-no_zero   = p_zero.
  wl_fcat_group-do_sum    = p_soma.

  APPEND wl_fcat_group TO it_fcat_group.

  CLEAR: wl_fcat_group.

ENDFORM.                    " ALV_PREENCHE_CAT_SAIDA_GROUP
*&---------------------------------------------------------------------*
*&      Module  Z_STATUS_GROUP  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_status_group OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " Z_STATUS_GROUP  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV_GROUP  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_exibe_alv_group OUTPUT.


  IF wa_cont_group IS INITIAL.

    CREATE OBJECT wa_cont_group
      EXPORTING
        container_name              = 'CC_ALV_GROUP'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF wa_alv_group IS INITIAL AND NOT
     wa_cont_group IS INITIAL.

    CREATE OBJECT wa_alv_group
      EXPORTING
        i_parent          = wa_cont_group
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event_group IS INITIAL.

    CREATE OBJECT wa_event_group.
    SET HANDLER: wa_event_group->zm_handle_hotspot FOR wa_alv_group.
    SET HANDLER: wa_event_group->zm_handle_toolbar FOR wa_alv_group.
    SET HANDLER: wa_event_group->zm_handle_user_command FOR wa_alv_group.

  ENDIF.

  wa_layout-cwidth_opt = 'X'. "Ajusta tamanho na coluna
  wa_layout-col_opt    = 'X'. "Ajusta tamanho na coluna

  wa_layout-sel_mode   = 'A'.
  wa_layout-box_fname  = 'SELECTED'.

  CALL METHOD wa_alv_group->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = gs_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv_group IS INITIAL.

ENDMODULE.                 " Z_EXIBE_ALV_GROUP  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  Z_STATUS_EDICAO  OUTPUT
*&---------------------------------------------------------------------*
MODULE z_status_edicao OUTPUT.
  SET PF-STATUS 'FF0300'.
  SET TITLEBAR  'TB0300'.
ENDMODULE.                 " Z_STATUS_EDICAO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND_EDICAO  INPUT
*&---------------------------------------------------------------------*
MODULE z_user_command_edicao INPUT.
  IF sy-dynnr EQ '0300'.
    CASE sy-ucomm.
      WHEN 'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0.
      WHEN 'OK'.
        PERFORM z_atualizar_tabela.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND_EDICAO  INPUT
*&---------------------------------------------------------------------*
*&      Form  EDIT_FERROVIARIO
*&---------------------------------------------------------------------*
FORM edit_ferroviario .

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  " Verifica Seleção de Linhas
  CALL METHOD wa_alv->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-037.
    EXIT.
  ENDIF.

  LOOP AT tl_rows INTO sl_rows.

    READ TABLE it_saida INTO wa_saida INDEX sl_rows-index.

    index_aux            = sl_rows-index.
    wa_edicao-idvagao    = wa_saida-idvagao_s.
    wa_edicao-dcl        = wa_saida-dcl_s.
    wa_edicao-observacao = wa_saida-observacao.
    wa_edicao-dtadecarga = wa_saida-dtadecarga_s.

    CALL SCREEN 0300 STARTING AT 15 10 ENDING AT 165 20.

    CLEAR: sl_rows, wa_saida.
  ENDLOOP.
ENDFORM.                    " EDIT_FERROVIARIO

*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZAR_TABELA
*&---------------------------------------------------------------------*
FORM z_atualizar_tabela .

  wa_saida_edicao-observacao   = wa_edicao-observacao.

  MODIFY it_saida INDEX index_aux FROM wa_saida_edicao
  TRANSPORTING observacao.

  UPDATE zlest0019
    SET observacao =  wa_edicao-observacao
  WHERE idvagao    EQ wa_edicao-idvagao
    AND dcl        EQ wa_edicao-dcl.


  CLEAR: wa_saida_aux, wa_edicao.

  CALL METHOD wa_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDFORM.                    " Z_ATUALIZAR_TABELA
