*----------------------------------------------------------------------*
* Report  ZSDR0007                                                     *
* Descrição  : Relatório Consulta de Resultados - INSUMOS              *
* Módulo     : MM SD                            Transação: ZSDT0028    *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Camila Brand                            Data: 01/04/2011*
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*

REPORT  zsdr0007.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
    kna1,           " Mestre de clientes (parte geral)
    vbak,           " Documento de vendas: dados de cabeçalho
    vbkd,           " Documento de vendas: dados comerciais
    vbap,           " Documento de vendas: dados de item
    vbfa,           " Fluxo de documentos de vendas e distribuição
    ekko,           " Cabeçalho do documento de compra
    ekpo,           " Item do documento de compras
    ekbe,           " Histórico para o documento de compra
    rbkp,           " Cabeçalho doc.da fatura recebida
    j_1bnflin,      " Partidas individuais da nota fiscal
    konv,           " Condições (dados de operação)
    zlest0032,      " Relac. frete proprio/terceiro com a revisão de fatura (Miro)
    vfkp,           " Custos de frete: dados do item
    tcurr.          " Taxas de câmbio

*----------------------------------------------------------------------*
* ESTRUTURA
*----------------------------------------------------------------------*
TYPES:

" Mestre de clientes (parte geral)
BEGIN OF ty_kna1,
  name1  TYPE kna1-name1,                                   " Nome 1
  kunnr  TYPE kna1-kunnr, " Nº cliente 1
END OF ty_kna1,

" Dados Contrato
BEGIN OF ty_vbak,
  vbeln TYPE vbak-vbeln, " Numero Contrato
  vkorg TYPE vbak-vkorg, " Organização de Vendas
  vtweg TYPE vbak-vtweg, " Canal de Distribuição
  spart TYPE vbak-spart, " Setor de Atividade
  auart TYPE vbak-auart, " Tipo de Contrato
  vkbur TYPE vbak-vkbur, " Escritório de Vendas
  kunnr TYPE vbak-kunnr, " Cliente
  waerk TYPE vbak-waerk, " Moeda
  erdat TYPE vbak-erdat, " Data do Contrato
END OF ty_vbak,

" Itens Contrato
BEGIN OF ty_vbap,
   vbeln TYPE vbap-vbeln, " Documento de Vendas
   matnr TYPE vbap-matnr, " Material
   arktx TYPE vbap-arktx, " Descrição do Material
   werks TYPE vbap-werks, " Centro
   zmeng TYPE vbap-zmeng, " Quantidade
   netwr TYPE vbap-netwr, " Valor
   posnr TYPE vbap-posnr, " Item
  ntgew  TYPE vbap-ntgew,
END OF ty_vbap,

" Totalizador Itens
BEGIN OF ty_vbap_tot,
   xtcto TYPE vbap-zmeng, " Quantidade
   posnr TYPE vbap-posnr, " Item
END OF ty_vbap_tot,

" Dados Comerciais
BEGIN OF ty_vbkd,
   vbeln TYPE vbkd-vbeln, " Nº documento de vendas e distribuição
   kurrf TYPE vbkd-kurrf, " Taxa de câmbio para lançamentos FI
END OF ty_vbkd,

" Dados Fatura
BEGIN OF ty_vbfa,
  vbeln   TYPE vbfa-vbeln,   " Doc.Fatura
  vbelv   TYPE vbfa-vbelv,   " Documento de vendas e distribuição precedente
  vbtyp_n TYPE vbfa-vbtyp_n, " Categoria de documento SD subseqüente
  vbtyp_v TYPE vbfa-vbtyp_v, " Ctg.documento de venda e distribuição (SD) precedente
  erdat   TYPE vbfa-erdat,   " Data Criação
  rfmng   TYPE vbfa-rfmng,   " Quantidade
  rfwrt   TYPE vbfa-rfwrt,   " Valor
  posnn   TYPE vbfa-posnn,   " Item
END OF ty_vbfa,

" Totalizador Faturas
BEGIN OF ty_vbfa_fat,
  vbelv     TYPE vbfa-vbelv,   " Documento de vendas e distribuição precedente
  vbtyp_n   TYPE vbfa-vbtyp_n, " Categoria de documento SD subseqüente
  vbtyp_v   TYPE vbfa-vbtyp_v, " Ctg.documento de venda e distribuição (SD) precedente
  erdat     TYPE vbfa-erdat,   " Data Criação
  rfmng     TYPE vbfa-rfmng,   " Quantidade
  rfwrt     TYPE vbfa-rfwrt,   " Valor
END OF ty_vbfa_fat,

" Totalizador Faturas
BEGIN OF ty_vbfa_tot,         " Fluxo de documentos de vendas e distribuição
  vbelnrfmg  TYPE vbfa-vbelv, " Documento de vendas e distribuição precedente
  totalrfmng TYPE vbfa-rfmng, " Documento de vendas e distribuição subseqüente
  posnn      TYPE vbfa-posnn, " item
END OF ty_vbfa_tot,

BEGIN OF ty_vbfa_frt,         " Fluxo de documentos de vendas e distribuição
  vbelnfrt   TYPE vbfa-vbelv, " Documento de vendas e distribuição precedente
  totalfrt   TYPE vbfa-rfmng, " Documento de vendas e distribuição subseqüente
  posnn      TYPE vbfa-posnn, " item
END OF ty_vbfa_frt,

" Frete Previsto
BEGIN OF ty_vbfa_prv,         " Fluxo de documentos de vendas e distribuição
  vbelnprv   TYPE vbfa-vbelv, " Documento de vendas e distribuição precedente
  totalprv   TYPE vbfa-rfmng, " Documento de vendas e distribuição subseqüente

END OF ty_vbfa_prv,

BEGIN OF ty_vbfa_aux,         " Fluxo de documentos de vendas e distribuição
  vbeln     TYPE vbfa-vbeln,   " Documento de vendas e distribuição precedente
  vbtyp_n   TYPE vbfa-vbtyp_n, " Categoria de documento SD subseqüente
  vbtyp_v   TYPE vbfa-vbtyp_v, " Ctg.documento de venda e distribuição (SD) precedente
END OF ty_vbfa_aux,


" Pedido
BEGIN OF ty_ekko,
  ebeln TYPE ekko-ebeln, " Pedido
  bsart TYPE ekko-bsart, " Tipo Pedido
  lifnr TYPE ekko-lifnr, " Fornecedor
  bukrs TYPE ekko-bukrs, " Empresa
  aedat TYPE ekko-aedat, " Data do Pedido
  waers TYPE ekko-waers, " Código da moeda
END OF ty_ekko,

" Item pedidos
BEGIN OF ty_ekpo,
  ebeln TYPE ekpo-ebeln, "Pedido
  ebelp TYPE ekpo-ebelp, "Item do Pedido
  matnr TYPE ekpo-matnr, "Material
  txz01 TYPE ekpo-txz01, "Descrição do Material
  menge TYPE ekpo-menge, "Quantidade
  netwr TYPE ekpo-netwr, "Valor
END OF ty_ekpo,

" Totalizador Itens
BEGIN OF ty_ekpo_aux,
  matnr  TYPE ekpo-matnr, "Material
  valord TYPE ekpo-netwr, "Valor Real
  valorr TYPE ekpo-netwr, "Valor Dolar
END OF ty_ekpo_aux,

" Dados Fatura
BEGIN OF ty_ekbe,
  ebeln TYPE ekbe-ebeln, " Nº do documento de compras
  belnr TYPE ekbe-belnr, " Nº documento de material
  ebelp TYPE ekbe-ebelp, " Nº item do documento de compra
  vgabe TYPE ekbe-vgabe, " Tipo de operação - histórico de pedido
  budat TYPE ekbe-budat, " Data de lançamento no documento
  menge TYPE ekbe-menge, " Quantidade
  dmbtr TYPE ekbe-dmbtr, " Montante em moeda interna
  shkzg TYPE ekbe-shkzg, " Código débito/crédito
  gjahr TYPE ekbe-gjahr, " Ano do documento do material
END OF ty_ekbe,

" Totalizador faturas
BEGIN OF ty_ekbe2,       " Histórico para o documento de compra
  ebeln TYPE ekbe-ebeln, " Nº do documento de compras
  ebelp TYPE ekbe-ebelp, " Nº item do documento de compra
  belnr TYPE ekbe-belnr, " Nº documento de material
  budat TYPE ekbe-budat, " Data de lançamento no documento
  menge TYPE ekbe-menge, " Quantidade
  dmbtr TYPE ekbe-dmbtr, " Montante em moeda interna
  shkzg TYPE ekbe-shkzg, " Código débito/crédito
  gjahr TYPE ekbe-gjahr, " Ano do documento do material
END OF ty_ekbe2,

" Totalizador faturas
BEGIN OF ty_ekbe_aux,
  bel_refkey TYPE j_1bnflin-refkey, " Referência ao documento de origem
END OF ty_ekbe_aux,

" Cabeçalho fatura
BEGIN OF ty_rbkp,
  belnr TYPE rbkp-belnr, " Nº de um documento de faturamento
  gjahr TYPE rbkp-gjahr, " Exercício
  xblnr TYPE rbkp-xblnr, " Nº documento de referência
END OF ty_rbkp,

" NF
BEGIN OF ty_j_1bnflin,
  refkey TYPE j_1bnflin-refkey, " Referência ao documento de origem
  docnum TYPE j_1bnflin-docnum, " Nº documento
END OF ty_j_1bnflin,

" Informações de Frete para SD
BEGIN OF ty_konv,
  knumv TYPE konv-knumv, " Nº condição do documento
  kschl TYPE konv-kschl, " Tipo de condição
  kbetr TYPE konv-kbetr, " Montante ou porcentagem da condição
  kwert TYPE konv-kwert, " Valor condição
END OF ty_konv,

" Informações de Frete para SD
BEGIN OF ty_konv_tot,
  knumv TYPE konv-knumv, " Nº condição do documento
  kwert TYPE konv-kwert, " Valor condição
END OF ty_konv_tot,


" Informações de Frete para SD
BEGIN OF ty_konv_frt,
  knumv TYPE konv-knumv, " Nº condição do documento
  kschl TYPE konv-kschl, " Tipo de condição
  kbetr TYPE konv-kbetr, " Montante ou porcentagem da condição
  kwert TYPE konv-kwert, " Valor condição
END OF ty_konv_frt,

" Relac. frete proprio/terceiro com a revisão de fatura (Miro)
BEGIN OF ty_zlest0032,
 tknum TYPE zlest0032-tknum, " Nº transporte
 fknum TYPE zlest0032-fknum, " Nº custos de frete
END OF ty_zlest0032,

" Custo Frete Itens
BEGIN OF ty_vfkd,
 fknum TYPE vfkp-fknum, " Nº custos de frete
 knumv TYPE vfkp-knumv, " Nº condição do documento
END OF ty_vfkd,


" Taxa de Dolar
BEGIN OF ty_tcurr,
  kurst TYPE  tcurr-kurst, " Categoria da taxa de câmbio
  fcurr TYPE  tcurr-fcurr, " Moeda de procedência
  gdatu TYPE  tcurr-gdatu, " Data a partir da qual o câmbio é válido
  ukurs TYPE  tcurr-ukurs, " Taxa de câmbio
END OF ty_tcurr,

" Informações Saida Rel.
BEGIN OF ty_saida,
  name1   TYPE kna1-name1, " Cliente
  werks   TYPE vbap-werks, " Centro
  vkbur   TYPE vbak-vkbur, " Escr.Vendas
  vbeln   TYPE vbak-vbeln, " Contrato
  ntgew       TYPE vbap-ntgew,         " Qte.Contrato
  qtefaturado TYPE vbap-zmeng,  " Quantidade Faturado
  saldo       TYPE vbap-zmeng, " Saldo
  waerk  TYPE vbak-waerk, " Moeda Cto.
  vlrcont TYPE vbap-netwr, " Valor Contrato

  posnr       TYPE vbap-posnr, " Item.
  zmeng   TYPE vbap-zmeng, " Qte. Contrato
  descmat(60) TYPE c,      " Descricao Material

  valord TYPE ekpo-netwr, "Valor Real
  valorr TYPE ekpo-netwr, "Valor Dolar

END OF ty_saida.


*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA:
  t_bdc              TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  it_saida           TYPE TABLE OF ty_saida                               ,
  it_kna1            TYPE TABLE OF ty_kna1                                ,
  it_vbak            TYPE TABLE OF ty_vbak                                ,
  it_vbkd            TYPE TABLE OF ty_vbkd                                ,
  it_vbap            TYPE TABLE OF ty_vbap                                ,
  it_vbap_tot        TYPE TABLE OF ty_vbap_tot                            ,
  it_vbfa            TYPE TABLE OF ty_vbfa                                ,
  it_vbfa_tot        TYPE TABLE OF ty_vbfa_tot                            ,
  it_vbfa_frt        TYPE TABLE OF ty_vbfa_frt                            ,
  it_vbfa_prv        TYPE TABLE OF ty_vbfa_prv                            ,
  it_vbfa_fat        TYPE TABLE OF ty_vbfa_fat                            ,
  it_vbfa_aux        TYPE TABLE OF ty_vbfa_aux                            ,
  it_ekko            TYPE TABLE OF ty_ekko                                ,
  it_ekpo            TYPE TABLE OF ty_ekpo                                ,
  it_ekpo_aux        TYPE TABLE OF ty_ekpo_aux                            ,
  it_ekbe            TYPE TABLE OF ty_ekbe                                ,
  it_ekbe2           TYPE TABLE OF ty_ekbe2                               ,
  it_ekbe_aux        TYPE TABLE OF ty_ekbe_aux                            ,
  it_rbkp            TYPE TABLE OF ty_rbkp                                ,
  it_j_1bnflin       TYPE TABLE OF ty_j_1bnflin                           ,
  it_konv            TYPE TABLE OF ty_konv                                ,
  it_konv_frt        TYPE TABLE OF ty_konv_frt                            ,
  it_konv_tot        TYPE TABLE OF ty_konv_tot                            ,
  it_zlest0032       TYPE TABLE OF ty_zlest0032                           ,
  it_vfkp            TYPE TABLE OF ty_vfkd                                ,
  it_tcurr           TYPE TABLE OF ty_tcurr                               .


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
    wa_cont            TYPE REF TO cl_gui_custom_container        , " Objeto Container
    wa_alv             TYPE REF TO cl_gui_alv_grid                , " Objeto ALV
    wa_layout          TYPE lvc_s_layo                            , " Layout da Lista / Fim do DATA
    wa_saida           TYPE ty_saida                              ,
    wa_kna1            TYPE ty_kna1                               ,
    wa_vbak            TYPE ty_vbak                               ,
    wa_vbkd            TYPE ty_vbkd                               ,
    wa_vbap            TYPE ty_vbap                               ,
    wa_vbap_tot        TYPE ty_vbap_tot                           ,
    wa_vbfa            TYPE ty_vbfa                               ,
    wa_vbfa_tot        TYPE ty_vbfa_tot                           ,
    wa_vbfa_frt        TYPE ty_vbfa_frt                           ,
    wa_vbfa_prv        TYPE ty_vbfa_prv                           ,
    wa_vbfa_fat        TYPE ty_vbfa_fat                           ,
    wa_vbfa_aux        TYPE ty_vbfa_aux                           ,
    wa_ekko            TYPE ty_ekko                               ,
    wa_ekpo            TYPE ty_ekpo                               ,
    wa_ekpo_aux        TYPE ty_ekpo_aux                           ,
    wa_ekbe            TYPE ty_ekbe                               ,
    wa_ekbe2           TYPE ty_ekbe2                              ,
    wa_ekbe_aux        TYPE ty_ekbe_aux                           ,
    wa_rbkp            TYPE ty_rbkp                               ,
    wa_j_1bnflin       TYPE ty_j_1bnflin                          ,
    wa_konv            TYPE ty_konv                               ,
    wa_konv_frt        TYPE ty_konv_frt                           ,
    wa_konv_tot        TYPE ty_konv_tot                           ,
    wa_zlest0032       TYPE ty_zlest0032                          ,
    wa_vfkp            TYPE ty_vfkd                               ,
    wa_tcurr           TYPE ty_tcurr                              .

*----------------------------------------------------------------------*
* ESTRUTURA ALV  - Tabela Estrutura Colunas do Relatório
*----------------------------------------------------------------------*
DATA:
      it_fcat TYPE TABLE OF lvc_s_fcat,
      s_variant TYPE disvariant.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS:  " Informações SD
                 p_tpcont    FOR vbak-auart NO-EXTENSION NO INTERVALS, " Tipo de contrato
                 p_orgven    FOR vbak-vkorg NO-EXTENSION NO INTERVALS, " Organização de vendas
                 p_cdist     FOR vbak-vtweg NO-EXTENSION NO INTERVALS, " Canal distribuição
                 p_sativ     FOR vbak-spart NO-EXTENSION NO INTERVALS, " Setor de atividade
                 p_escven    FOR vbak-vkbur ,                          " Escritório de vendas
                 p_datco     FOR vbak-erdat ,                          " Data do Contrato
                 p_mater     FOR vbap-matnr ,                          " Material


                 "Informações MM
                 p_centro    FOR ekko-reswk , " Centro
                 p_emp       FOR ekko-bukrs ,  " Nome Empresa
                 p_pedido    FOR ekko-bsart NO INTERVALS, " Tipo Pedido
                 c_dtpe      FOR ekko-aedat . "Data do Pedido


SELECTION-SCREEN: END OF BLOCK b1.


*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: form_seleciona,       " Seleção de Dados - Formulário SD
           form_saida,           " Saída SD
           form_alv.             " Formulário ALV SD
  CALL SCREEN 0100.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FORM_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM form_seleciona.

  DATA: xtcto TYPE vbap-zmeng,
        somarfmng      TYPE vbfa-rfmng,
        diminuirfmng   TYPE vbfa-rfmng,
        totalrfmng     TYPE vbfa-rfmng,
        vbelnrfmg      TYPE vbfa-vbeln,
        ebeln_aux      TYPE ekbe-ebeln,
        ebelp_aux      TYPE ekbe-ebelp,
        belnr_aux      TYPE ekbe-belnr,
        gjahr_aux      TYPE ekbe-gjahr,
        xtot_faturamm  TYPE ekbe-menge,
        budat_aux      TYPE ekbe-budat,
        dmbtr_aux      TYPE ekbe-dmbtr,
        shkzg_aux      TYPE ekbe-shkzg,
        bel_refkey     TYPE j_1bnflin-refkey,
        xtpedr         TYPE ekpo-netwr,
        xtpedu         TYPE ekpo-netwr,
        ebelnaux       TYPE ekpo-ebeln,
        total          TYPE ekpo-netwr,
        mataux         TYPE vbap-matnr,
        xsdo_cto       TYPE vbap-zmeng,
        xnfrprev       TYPE vbfa-rfmng,
        xfpago         TYPE konv-kwert.



*  if  p_tpcont is initial .
*    message i000(z01) with 'É Obrigatório informar o Tipo de Contrato!'.
*    stop.
*  endif.
*
*  if  p_orgven is initial .
*    message i000(z01) with 'É Obrigatório informar a Organização de Vendas!'.
*    stop.
*  endif.
*
*  if  p_cdist is initial .
*    message i000(z01) with 'É Obrigatório informar o Canal de Distribuição!'.
*    stop.
*  endif.
*
*  if  p_sativ is initial .
*    message i000(z01) with 'É Obrigatório informar o Setor de Atividade!'.
*    stop.
*  endif.
*
*  if  p_datco is initial .
*    message i000(z01) with 'É Obrigatório informar a Data do Contrato!'.
*    stop.
*  endif.

  SELECT vbeln vkorg vtweg spart auart vkbur kunnr waerk erdat
  FROM vbak
  INTO TABLE it_vbak
  WHERE auart    IN p_tpcont
  AND   vkorg    IN p_orgven
  AND   vtweg    IN p_cdist
  AND   spart    IN p_sativ
  AND   vkbur    IN p_escven
  AND   erdat    IN p_datco.
  CHECK sy-subrc IS INITIAL.

  SELECT FROM V_KONV FIELDS KNUMV , KSCHL , KBETR , KWERT FOR ALL ENTRIES IN @IT_VBAK WHERE KNUMV EQ @IT_VBAK-KUNNR AND KSCHL = 'KF00' INTO TABLE @IT_KONV .



  SELECT name1 kunnr
  FROM kna1
  INTO TABLE it_kna1
  FOR ALL ENTRIES IN it_vbak
  WHERE kunnr EQ it_vbak-kunnr .
  CHECK sy-subrc IS INITIAL.


  SELECT vbeln matnr arktx werks zmeng netwr posnr ntgew
  FROM vbap
  INTO TABLE it_vbap
  FOR ALL ENTRIES IN it_vbak
  WHERE vbeln EQ it_vbak-vbeln
  AND matnr IN p_mater .
  CHECK sy-subrc IS INITIAL.


  SELECT vbeln kurrf
  FROM vbkd
  INTO TABLE it_vbkd
  FOR ALL ENTRIES IN it_vbap
  WHERE vbeln EQ it_vbap-vbeln.


  SELECT vbelv vbelv vbtyp_n vbtyp_v erdat rfmng rfwrt posnn
  FROM vbfa
  INTO TABLE it_vbfa
  FOR ALL ENTRIES IN it_vbak
  WHERE vbelv EQ it_vbak-vbeln
  AND vbtyp_n IN ('M','N')
  AND vbtyp_v = 'C'.

  SELECT vbeln vbtyp_n vbtyp_v
  FROM vbfa
  INTO TABLE it_vbfa_aux
  FOR ALL ENTRIES IN it_vbak
  WHERE vbelv EQ it_vbak-vbeln
  AND vbtyp_n = 'J' AND vbtyp_v = 'C'
  OR  vbtyp_n = '8'  AND vbtyp_v = 'J'.

  SELECT fknum tknum
  FROM zlest0032
  INTO TABLE it_zlest0032
  FOR ALL ENTRIES IN it_vbfa_aux
  WHERE tknum EQ it_vbfa_aux-vbeln.

  SELECT fknum knumv
  FROM vfkp
  INTO TABLE it_vfkp
  FOR ALL ENTRIES IN it_zlest0032
  WHERE fknum EQ it_zlest0032-fknum.

  SELECT FROM V_KONV FIELDS KNUMV , KSCHL , KWERT FOR ALL ENTRIES IN @IT_VFKP WHERE KSCHL IN ( 'ZFRE' , 'ZPED' , 'ZSEG' , 'ZIOF' , 'ZICM' ) AND KNUMV EQ @IT_VFKP-KNUMV INTO TABLE @IT_KONV_FRT .

*
*Se achar pegar o valor do campo KONV-KWERT totalizar guardar na variável XFPAGO
  LOOP AT it_vfkp INTO wa_vfkp.
    LOOP AT it_konv_frt INTO wa_konv_frt WHERE knumv EQ wa_vfkp-knumv .
      xfpago = wa_konv_frt-kwert.
    ENDLOOP.
    wa_konv_tot-knumv = wa_konv_frt .
    wa_konv_tot-kwert = xfpago.

    APPEND wa_konv_tot TO it_konv_tot.
  ENDLOOP.


  LOOP AT it_vbap INTO wa_vbap.
    CLEAR wa_vbfa_tot-totalrfmng.
    somarfmng = 0.
    diminuirfmng = 0.
    wa_vbfa_tot-vbelnrfmg = 0.
    wa_vbfa-posnn = 0.
    wa_vbfa-vbelv = 0.

    LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv EQ wa_vbap-vbeln AND posnn EQ wa_vbap-posnr  . "wa_vbak-vbeln.
      "Somar/Diminuir
      IF wa_vbfa-vbtyp_n = 'M'.
        somarfmng = somarfmng + wa_vbfa-rfmng.
      ELSEIF wa_vbfa-vbtyp_n = 'N'.
        diminuirfmng = diminuirfmng + wa_vbfa-rfmng.
      ENDIF.

    ENDLOOP.

    wa_vbfa_tot-totalrfmng = somarfmng - diminuirfmng.
    wa_vbfa_tot-posnn = wa_vbfa-posnn.
    wa_vbfa_tot-vbelnrfmg  = wa_vbfa-vbelv.

    IF wa_vbfa_tot-totalrfmng > 0.

      APPEND wa_vbfa_tot TO it_vbfa_tot.

    ENDIF.

  ENDLOOP.

  " Para o Frete Previsto
  LOOP AT it_vbap INTO wa_vbap.
    LOOP AT it_vbfa_tot INTO wa_vbfa_tot WHERE vbelnrfmg EQ wa_vbap-vbeln AND posnn EQ wa_vbap-posnr  . "wa_vbak-vbeln.

      xsdo_cto = 0.

      IF wa_vbfa_tot-totalrfmng > 0.

        xsdo_cto = wa_vbap-zmeng - wa_vbfa_tot-totalrfmng .

        wa_vbfa_frt-vbelnfrt   = wa_vbap-vbeln.
        wa_vbfa_frt-totalfrt  = xsdo_cto.
        wa_vbfa_frt-posnn  = wa_vbap-posnr.

        APPEND wa_vbfa_frt TO it_vbfa_frt.
      ENDIF.

    ENDLOOP.
  ENDLOOP.


  "Calculo Novo valor Previsto = XSDO_CTO * KONV-KBETR guardar variável XNFRPREV

  LOOP AT it_vbfa_frt INTO wa_vbfa_frt.
    LOOP AT it_vbak INTO wa_vbak WHERE vbeln EQ wa_vbfa_frt-vbelnfrt.
      LOOP AT it_konv INTO wa_konv WHERE knumv EQ wa_vbak-kunnr.

        "Calculo Novo valor Previsto
        xnfrprev = wa_vbfa_frt-totalfrt * wa_konv-kbetr.

        wa_vbfa_prv-vbelnprv = wa_vbfa_frt-vbelnfrt.
        wa_vbfa_prv-totalprv   = xnfrprev.

        APPEND wa_vbfa_prv TO it_vbfa_prv.

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.



  " Taxa Dolar.
  SELECT kurst fcurr gdatu ukurs
  FROM tcurr
  INTO TABLE it_tcurr
  WHERE  kurst = 'B'
  AND  fcurr = 'USD'
  AND  gdatu = sy-datum  .


  SELECT ebeln bsart lifnr bukrs aedat waers
  FROM ekko
  INTO TABLE it_ekko
  WHERE aedat IN c_dtpe
  AND reswk IN  p_centro
  AND bukrs IN p_emp
  AND bsart IN  p_pedido  .


  SELECT ebeln ebelp matnr txz01 menge netwr
  FROM ekpo
  INTO TABLE it_ekpo
  FOR ALL ENTRIES IN it_ekko
  WHERE ebeln EQ  it_ekko-ebeln.


  SELECT ebeln belnr ebelp vgabe budat menge dmbtr shkzg gjahr
  FROM ekbe
  INTO TABLE it_ekbe
  FOR ALL ENTRIES IN it_ekpo
  WHERE ebeln EQ  it_ekpo-ebeln
  AND ebelp EQ it_ekpo-ebelp
  AND vgabe = '2'.

  SELECT belnr gjahr xblnr
  FROM rbkp
  INTO TABLE it_rbkp
  FOR ALL ENTRIES IN it_ekbe
  WHERE belnr EQ it_ekbe-belnr
  AND gjahr EQ it_ekbe-gjahr.

  ebeln_aux = ''.
  ebelp_aux = ''.
  belnr_aux = ''.
  gjahr_aux = ''.
  xtot_faturamm  = 0.

  SORT it_ekbe BY ebeln ebelp.

  LOOP AT it_ekbe INTO wa_ekbe .
    IF  ebeln_aux  = '' AND ebelp_aux  = ''.
      ebeln_aux = wa_ekbe-ebeln.
      ebelp_aux = wa_ekbe-ebelp.
      belnr_aux = wa_ekbe-belnr.
      belnr_aux = wa_ekbe-belnr.
      budat_aux = wa_ekbe-budat.
      dmbtr_aux = wa_ekbe-dmbtr.
      shkzg_aux = wa_ekbe-shkzg.
      gjahr_aux = wa_ekbe-gjahr.
    ENDIF.

    IF wa_ekbe-ebeln = ebeln_aux AND ebelp_aux = wa_ekbe-ebelp.
      IF wa_ekbe-shkzg = 'S'.
        xtot_faturamm  = xtot_faturamm  + wa_ekbe-menge.
      ELSEIF wa_ekbe-shkzg = 'H'.
        xtot_faturamm  = xtot_faturamm  - wa_ekbe-menge.
      ENDIF.
    ELSE.

      wa_ekbe2-menge = xtot_faturamm.
      wa_ekbe2-ebeln = ebeln_aux.
      wa_ekbe2-ebelp = ebelp_aux.
      wa_ekbe2-belnr = belnr_aux.
      wa_ekbe2-budat = budat_aux.
      wa_ekbe2-dmbtr = dmbtr_aux.
      wa_ekbe2-shkzg = shkzg_aux.
      wa_ekbe2-gjahr = gjahr_aux.

      APPEND wa_ekbe2 TO it_ekbe2.

      xtot_faturamm = 0.

      IF wa_ekbe-shkzg = 'S'.
        xtot_faturamm  = xtot_faturamm  + wa_ekbe-menge.
      ELSEIF wa_ekbe-shkzg = 'H'.
        xtot_faturamm  = xtot_faturamm  - wa_ekbe-menge.
      ENDIF.

      ebeln_aux     = wa_ekbe-ebeln.
      ebelp_aux     = wa_ekbe-ebelp.
      belnr_aux     = wa_ekbe-belnr.
      budat_aux     = wa_ekbe-budat.
      dmbtr_aux     = wa_ekbe-dmbtr.
      shkzg_aux     = wa_ekbe-shkzg.
      gjahr_aux     = wa_ekbe-gjahr.

    ENDIF.

  ENDLOOP.

  wa_ekbe2-menge = xtot_faturamm.
  wa_ekbe2-ebeln = ebeln_aux.
  wa_ekbe2-ebelp = ebelp_aux.
  wa_ekbe2-belnr = belnr_aux.
  wa_ekbe2-budat = budat_aux.
  wa_ekbe2-dmbtr = dmbtr_aux.
  wa_ekbe2-shkzg = shkzg_aux.
  wa_ekbe2-gjahr = gjahr_aux.


  APPEND wa_ekbe2 TO it_ekbe2.


  LOOP AT it_ekbe2 INTO wa_ekbe2.
    bel_refkey = 0.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_ekbe2-belnr
      IMPORTING
        output = wa_ekbe2-belnr.
    CONCATENATE  wa_ekbe2-belnr wa_ekbe2-gjahr INTO bel_refkey .
    wa_ekbe_aux-bel_refkey = bel_refkey.
    APPEND wa_ekbe_aux TO it_ekbe_aux.

  ENDLOOP.

  SORT it_ekbe_aux BY bel_refkey.

  SELECT refkey docnum
  FROM j_1bnflin
  INTO TABLE it_j_1bnflin
  FOR ALL ENTRIES IN it_ekbe_aux
  WHERE  refkey EQ it_ekbe_aux-bel_refkey .
  "CHECK sy-subrc IS INITIAL.



  SORT it_vbap BY matnr.
  mataux = 0.

  LOOP AT it_vbap INTO wa_vbap.
    CLEAR  wa_ekpo_aux-valord.
    CLEAR  wa_ekpo_aux-valorr.
    xtpedr = 0.
    xtpedu = 0.
    total  = 0.
    ebelnaux = 0.


    SORT it_ekpo BY matnr.

    IF wa_vbap-matnr <> mataux .

      LOOP AT it_ekpo INTO wa_ekpo WHERE matnr EQ wa_vbap-matnr.

        ebelnaux = wa_ekpo-ebeln.
        total =    wa_ekpo-netwr.

        SORT it_ekko BY ebeln.

        LOOP AT it_ekko INTO wa_ekko WHERE  ebeln EQ  ebelnaux.
          " Real / Dolar
          IF wa_ekko-waers = 'BRL' .
            xtpedr = xtpedr + total.
          ELSEIF wa_ekko-waers = 'USD'.
            xtpedu = xtpedu + total.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

      wa_ekpo_aux-matnr = wa_ekpo-matnr.
      wa_ekpo_aux-valorr = xtpedr.
      wa_ekpo_aux-valord = xtpedu.
      APPEND wa_ekpo_aux TO it_ekpo_aux.
      mataux = wa_vbap-matnr.

    ENDIF.

  ENDLOOP.

ENDFORM.                    "form_seleciona_sd

*&---------------------------------------------------------------------*
*&      Form  FORM_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_saida.
  REFRESH it_saida.
  SORT:
    it_vbak BY vbeln,
    it_kna1 BY kunnr,
    it_vbfa_tot BY vbelnrfmg,
    it_vbap BY vbeln,
    it_ekpo_aux BY matnr.


  DATA:
        x_saldo    TYPE vbap-zmeng,
        ntgewaux   TYPE bstmg,
        aux(100) TYPE c.


  LOOP AT it_vbak INTO wa_vbak.

    CLEAR : wa_vbfa,wa_vbap,wa_kna1, wa_saida, wa_vbfa_tot.

    LOOP AT it_vbap INTO wa_vbap WHERE vbeln EQ wa_vbak-vbeln.


      READ TABLE it_vbfa_tot INTO wa_vbfa_tot WITH KEY vbelnrfmg = wa_vbap-vbeln posnn = wa_vbap-posnr   BINARY SEARCH.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr  BINARY SEARCH.

      READ TABLE it_ekpo_aux INTO wa_ekpo_aux WITH KEY matnr = wa_vbap-matnr BINARY SEARCH.


      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      aux = ''.

      wa_saida-name1 = wa_kna1-name1. " Cliente
      wa_saida-werks  = wa_vbap-werks. " Centro
      wa_saida-vkbur = wa_vbak-vkbur. " Escr.Vendas
      wa_saida-posnr = wa_vbap-posnr. " Item
      wa_saida-vbeln = wa_vbak-vbeln. " Contrato
      wa_saida-zmeng = wa_vbap-zmeng. " Qte. Contrato


      CONCATENATE wa_vbap-matnr '-' wa_vbap-arktx INTO aux SEPARATED BY space.
      wa_saida-descmat = aux. " Descrição do Material

      wa_saida-ntgew = wa_vbap-ntgew. " Qte.Liquida

      wa_saida-qtefaturado =  wa_vbfa_tot-totalrfmng.  " Qte. Faturado
      x_saldo = wa_saida-ntgew - wa_saida-qtefaturado. " Saldo
      wa_saida-saldo =  x_saldo.

      wa_saida-waerk = wa_vbak-waerk. " Moeda

      wa_saida-vlrcont = wa_vbap-netwr.

      IF wa_vbak-waerk = 'USD' AND  wa_vbkd-kurrf <> 0 .

        wa_saida-vlrcont = wa_vbap-netwr / wa_vbkd-kurrf.

      ELSEIF wa_vbak-waerk = 'BRL' AND  wa_vbkd-kurrf <> 0.

        wa_saida-vlrcont = wa_vbap-netwr * wa_vbkd-kurrf.

      ENDIF.

      wa_saida-valord = wa_ekpo_aux-valord . "Valor Real
      wa_saida-valorr = wa_ekpo_aux-valorr ."Valor Dolar


      APPEND wa_saida TO it_saida.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    "form_saida


*&---------------------------------------------------------------------*
*&      Form  FORM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_alv .
  PERFORM alv_preenche_cat USING:
     'NAME1'              text-004      '040'      ' ' ' '    'IT_SAIDA' , " Cliente
     'WERKS'              text-005      '006'      ' ' ' '    'IT_SAIDA' , " Centro
     'VKBUR'              text-006      '012'      ' ' ' '    'IT_SAIDA' , " Escritorio de Vendas
     'VBELN'              text-007      '013'      ' ' ' '    'IT_SAIDA' , " Nro. Contrato
     'ZMENG'              text-008      '013'      ' ' ' '    'IT_SAIDA' , " Qte. Contrato
     'QTEFATURADO'        text-009      '013'      ' ' ' '    'IT_SAIDA' , " Qte. Faturado
     'SALDO'              text-010      '013'      ' ' ' '    'IT_SAIDA' , " Saldo
     'WAERK'              text-011      '013'      ' ' ' '    'IT_SAIDA' , " Moeda
     'VLRCONT'            text-012      '013'      ' ' ' '    'IT_SAIDA' , " Valor Contrato
     'VALORR'             text-013      '013'      ' ' ' '    'IT_SAIDA' , " Valor Pedido Real
     'VALORD'             text-014      '013'      ' ' ' '    'IT_SAIDA' . " Valor Pedido Dolar



  "'POSNR'              text-009      '004'      ' ' ' '    'IT_SAIDA' . " Item

ENDFORM.                    " FORM_ALV_SD

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING    value(p_flag)
                           value(p_fnam)
                           value(p_fval).
  CLEAR t_bdc.

  IF NOT p_flag IS INITIAL.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  ELSE.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  ENDIF.

  APPEND t_bdc.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_layout .
  wa_layout-zebra = 'X'.
ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*

FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_zero  TYPE c
                               p_hot   TYPE c
                               p_saida TYPE c  .

  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = p_saida                           .
  wl_fcat-fieldname = p_campo                           .
  wl_fcat-scrtext_l = p_desc                            .
  wl_fcat-scrtext_m = p_desc                            .
  wl_fcat-scrtext_s = p_desc                            .
  wl_fcat-hotspot   = p_hot                             .
  wl_fcat-no_zero   = p_zero                            .
  wl_fcat-outputlen = p_tam                             .

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA:wa_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
            zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING
                e_object e_interactive                   ,

            zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING
                 e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

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
*       text
*----------------------------------------------------------------------*
MODULE z_exibe_alv OUTPUT .

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
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.


  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
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

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF ( sy-dynnr EQ '0100' ).
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
        c_button_normal           TYPE i VALUE 0        ,
        c_menu_and_default_button TYPE i VALUE 1        ,
        c_menu                    TYPE i VALUE 2        ,
        c_separator               TYPE i VALUE 3        ,
        c_radio_button            TYPE i VALUE 4        ,
        c_checkbox                TYPE i VALUE 5        ,
        c_menu_entry              TYPE i VALUE 6        .

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM z_handle_command  USING    p_e_ucomm.

ENDFORM.                    " Z_HANDLE_COMMAND
