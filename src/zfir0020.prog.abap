************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 17.05.2010                                          *
* Objetivo    ...: Relatório de Contas a Receber                       *
* Transação   ...: ZFIS20                                              *
************************************************************************
* Data Modif    Autor       Descriçao      Hora           Request      *
************************************************************************
* 17.05.2011  Victor Hugo   Criação        13:16:26     DEVK915517     *
* 19.05.2011  Victor Hugo   Modificação    09:21:45     DEVK915995     *
* 24.05.2011  Victor Hugo   Modificação    14:14:16     DEVK916141     *
* 26.05.2011  Victor Hugo   Modificação    08:22:55     DEVK916169     *
* 26.05.2011  Victor Hugo   Modificação    16:51:01     DEVK916253     *
* 27.05.2011  Victor Hugo   Modificação    10:28:49     DEVK916269     *
* 27.05.2011  Victor Hugo   Modificação    13:36:25     DEVK916287     *
* 27.05.2011  Victor Hugo   Modificação    14:34:08     DEVK916291     *
* 27.05.2011  Victor Hugo   Modificação    16:14:00     DEVK916295     *
* 27.05.2011  Victor Hugo   Modificação    16:28:04     DEVK916297     *
* 27.05.2011  Victor Hugo   Modificação    16:39:58     DEVK916299     *
* 27.05.2011  Victor Hugo   Modificação    17:11:28     DEVK916301     *
* 30.05.2011  Victor Hugo   Modificação    09:52:48     DEVK916305     *
* 30.05.2011  Victor Hugo   Modificação    13:31:16     DEVK916325     *
* 30.05.2011  Victor Hugo   Modificação    14:44:29     DEVK916341     *
* 31.05.2011  Victor Hugo   Modificação    08:37:06     DEVK916355     *
* 31.05.2011  Victor Hugo   Modificação    09:05:22     DEVK916357     *
* 31.05.2011  Victor Hugo   Modificação    09:38:31     DEVK916359     *
* 31.05.2011  Victor Hugo   Modificação    16:23:19     DEVK916363     *
* 01.06.2011  Victor Hugo   Modificação    14:18:24     DEVK916422     *
* 26.07.2011  Paulo Bonetti Modificação    13:41:00     DEVK917663     *

************************************************************************



REPORT  zfir0020.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

TABLES: bsid,
        vbak,
        bsad,
        vbrp.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:





































  BEGIN OF ty_bsid,
    bukrs TYPE bsid-bukrs, " Empresa
    kunnr TYPE bsid-kunnr, " Nº cliente 1
    blart TYPE bsid-blart, " Tipo de documento
    budat TYPE bsid-budat, " Data de lançamento no documento
    umskz TYPE bsid-umskz, " Código de Razão Especial
    vbel2 TYPE bsid-vbel2, " Documento de vendas
    zuonr TYPE bsid-zuonr, " Nº atribuição
    belnr TYPE bsid-belnr, " Nº documento de um documento contábil

    bldat TYPE bsid-bldat, " Data no documento
    waers TYPE bsid-waers, " Código da moeda
    xblnr TYPE bsid-xblnr, " Nº documento de referência
    shkzg TYPE bsid-shkzg, " Código débito/crédito
    gsber TYPE bsid-gsber, " Divisão
    dmbtr TYPE bsid-dmbtr, " Montante em moeda interna
    sgtxt TYPE bsid-sgtxt, " Texto do item
    saknr TYPE bsid-saknr, " Nº conta do Razão
    zfbdt TYPE bsid-zfbdt, " Data base para cálculo do vencimento
    zbd1t TYPE bsid-zbd1t, " Dias de desconto 1
    zterm TYPE bsid-zterm, " Chave de condições de pagamento
    dmbe2 TYPE bsid-dmbe2, " Documento de faturamento
    xref1 TYPE bsid-xref1, " Montante na 2ª moeda interna
    xref2 TYPE bsid-xref2, " Chave de referência do parceiro de negócios
    xref3 TYPE bsid-xref3, " Chave de referência do parceiro de negócios
    kidno TYPE bsid-kidno, " Chave de referência para item de doc.
    bupla TYPE bsid-bupla, " Referência de pagamento
    gjahr TYPE bsid-gjahr, " Local de negócios
    vbeln TYPE bsid-vbeln, " Documento de faturamento
    augbl TYPE bsid-augbl, " Nº documento de compensação
    augdt TYPE bsid-augdt, " Data de compensação
    rebzg TYPE bsid-rebzg, " Nº documento da fatura à qual pertence a operação
    hkont TYPE bsid-hkont, " Conta do Razão da contabilidade geral
    umsks TYPE bsid-umsks, " Classe de operação de Razão Especial
    posn2 TYPE bsid-posn2, " Item do documento de vendas
  END OF ty_bsid,

  BEGIN OF ty_bsad,
    bukrs TYPE bsad-bukrs, " Empresa
    kunnr TYPE bsad-kunnr, " Nº cliente 1
    blart TYPE bsad-blart, " Tipo de documento
    augdt TYPE bsad-augdt, " Data de compensação
    umskz TYPE bsad-umskz, " Código de Razão Especial
    vbel2 TYPE bsad-vbel2, " Documento de vendas
    zuonr TYPE bsad-zuonr, " Nº atribuição
    belnr TYPE bsad-belnr, " Nº documento de um documento contábil
    budat TYPE bsad-budat, " Data de lançamento no documento
    bldat TYPE bsad-bldat, " Data no documento
    waers TYPE bsad-waers, " Código da moeda
    xblnr TYPE bsad-xblnr, " Nº documento de referência
    shkzg TYPE bsad-shkzg, " Código débito/crédito
    gsber TYPE bsad-gsber, " Divisão
    dmbtr TYPE bsad-dmbtr, " Montante em moeda interna
    sgtxt TYPE bsad-sgtxt, " Texto do item
    saknr TYPE bsad-saknr, " Nº conta do Razão
    zfbdt TYPE bsad-zfbdt, " Data base para cálculo do vencimento
    zbd1t TYPE bsad-zbd1t, " Dias de desconto 1
    zterm TYPE bsad-zterm, " Chave de condições de pagamento
    vbeln TYPE bsad-vbeln, " Documento de faturamento
    dmbe2 TYPE bsad-dmbe2, " Montante na 2ª moeda interna
    xref1 TYPE bsad-xref1, " Chave de referência do parceiro de negócios
    xref2 TYPE bsad-xref2, " Chave de referência do parceiro de negócios
    xref3 TYPE bsad-xref3, " Chave de referência do parceiro de negócios
    bupla TYPE bsad-bupla, " Local de negócios
    augbl TYPE bsad-augbl, " Nº documento de compensação
    gjahr TYPE bsad-gjahr, " Exercício
    kidno TYPE bsad-kidno, " Referência de pagamento
    hkont TYPE bsad-hkont, " Conta do Razão da contabilidade geral
    umsks TYPE bsad-umsks, " Classe de operação de Razão Especial
  END OF ty_bsad,

  BEGIN OF ty_vbsad,
    bukrs TYPE bsad-bukrs,
    gjahr TYPE bsad-gjahr,
    belnr TYPE bsad-belnr,
    augbl TYPE bsad-augbl,
  END OF ty_vbsad,

  BEGIN OF ty_zsdt0054,
    nro_sol_ov TYPE zsdt0054-nro_sol_ov,
    adiant     TYPE zsdt0054-adiant,
  END OF ty_zsdt0054,

  BEGIN OF ty_zsdt0051,
    nro_sol_ov TYPE zsdt0051-nro_sol_ov,
    bstkd      TYPE zsdt0051-bstkd,

  END OF ty_zsdt0051,

  BEGIN OF ty_vbak,
    vbeln TYPE vbak-vbeln, " Documento de vendas
    auart TYPE vbak-auart, " Tipo de documento de vendas
    kunnr TYPE vbak-kunnr,
  END OF ty_vbak,

  BEGIN OF ty_zsdt0053,
    vbeln     TYPE zsdt0053-vbeln, " Documento de vendas
    instrucao TYPE zsdt0053-instrucao,
    contrato  TYPE zsdt0053-contrato,
  END OF ty_zsdt0053,

  BEGIN OF ty_zsdt0066,
    vbeln     TYPE zsdt0066-vbeln, " Documento de vendas
    instrucao TYPE zsdt0066-instrucao,
  END OF ty_zsdt0066,

  BEGIN OF ty_zsdt0045,
    instrucao TYPE zsdt0045-instrucao, " Documento de vendas
    contrato  TYPE zsdt0045-contrato,
  END OF ty_zsdt0045,







  BEGIN OF ty_bkpf,
    bukrs     TYPE bkpf-bukrs, " Empresa
    belnr     TYPE bkpf-belnr, " Nº documento de um documento contábil
    gjahr     TYPE bkpf-gjahr, " Exercício
    usnam     TYPE bkpf-usnam, " Nome do usuário
    tcode     TYPE bkpf-tcode, " Código de transação
    bktxt     TYPE bkpf-bktxt, " Texto de cabeçalho de documento
    awkey     TYPE bkpf-awkey, " Chave referência
    waers     TYPE bkpf-waers,
    awkey_aux TYPE j_1brefkey,






  END OF ty_bkpf,

  BEGIN OF ty_vbrp,
    vbeln TYPE vbrp-vbeln, " Documento de faturamento
    matnr TYPE vbrp-matnr, " Nº do material
    arktx TYPE vbrp-arktx, " Texto breve do item da ordem do cliente
    fkimg TYPE vbrp-fkimg, " Quantidade faturada efetivamente
  END OF ty_vbrp,

  BEGIN OF ty_vbkd,
    vbeln TYPE vbkd-vbeln, " Nº documento de vendas e distribuição
    bstkd TYPE vbkd-bstkd, " Nº pedido do cliente
  END OF ty_vbkd,

  BEGIN OF ty_j_1bnflin,
    refkey TYPE j_1bnflin-refkey, " Referência ao documento de origem
    docnum TYPE j_1bnflin-docnum, " Nº documento


  END OF ty_j_1bnflin,

  BEGIN OF ty_j_1bnfdoc,
    docnum TYPE j_1bnfdoc-docnum, " Nº documento
    nfe    TYPE j_1bnfdoc-nfe,    " Nota Fiscal eletrônica
    nfenum TYPE j_1bnfdoc-nfenum, " Nº NF-e de nove posições
    nfnum  TYPE j_1bnfdoc-nfnum,  " Nº nota fiscal
  END OF ty_j_1bnfdoc,

  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr, " Nº cliente 1
    name1 TYPE kna1-name1,                                 " Nome 1
  END OF ty_kna1,

  BEGIN OF ty_t001,
    bukrs TYPE t001-bukrs, " Empresa
    butxt TYPE t001-butxt, " Denominação da firma ou empresa 25
  END OF ty_t001,

  BEGIN OF ty_t001w,
    werks TYPE t001w-werks, " Centro
    name1 TYPE t001w-name1, " Nome
  END OF ty_t001w,

  BEGIN OF ty_tvakt,
    auart TYPE tvakt-auart, " Tipo de documento de vendas
    bezei TYPE tvakt-bezei, " Denominação


  END OF ty_tvakt,

  BEGIN OF ty_bsis,
    bukrs TYPE  bsis-bukrs,
    belnr TYPE  bsis-belnr,
    gjahr TYPE  bsis-gjahr,
    hkont TYPE  bsis-hkont,
  END OF ty_bsis,

  BEGIN OF ty_skb1,
    bukrs TYPE skb1-bukrs,
    saknr TYPE skb1-saknr,
    fstag TYPE skb1-fstag,
  END OF ty_skb1,






  BEGIN OF ty_ska1,
    ktopl TYPE ska1-ktopl,
    saknr TYPE ska1-saknr,
  END OF ty_ska1,

  BEGIN OF ty_skat,
    saknr TYPE skat-saknr,
    txt20 TYPE skat-txt20,

  END OF ty_skat,

  BEGIN OF ty_vbfa,
    vbeln   TYPE vbfa-vbeln,
    vbtyp_v TYPE vbfa-vbtyp_v,
    vbelv   TYPE vbfa-vbelv,
  END OF ty_vbfa,

  BEGIN OF ty_vbfa_m_j,
    vbeln TYPE vbfa-vbeln,
    vbelv TYPE vbfa-vbelv,
  END OF ty_vbfa_m_j,

  BEGIN OF ty_zdoc_exp,
    vbeln            TYPE zdoc_exp-vbeln,
    id_registro_expo TYPE zdoc_exp-id_registro_expo,
  END OF   ty_zdoc_exp,

  BEGIN OF ty_zreg_exportacao,
    id_registro_expo TYPE zreg_exportacao-id_registro_expo,
    nr_registro_expo TYPE zreg_exportacao-nr_registro_expo,
  END OF   ty_zreg_exportacao,



  BEGIN OF ty_vbap,
    vbeln TYPE vbap-vbeln,
    posnr TYPE vbap-posnr,
    matnr TYPE vbap-matnr,
    arktx TYPE vbap-arktx,
  END OF ty_vbap,



























  BEGIN OF ty_saida,
    vbeln            TYPE vbak-vbeln,
    auart            TYPE vbak-auart,
    bukrs            TYPE bsad-bukrs,
    kunnr            TYPE bsad-kunnr,
    blart            TYPE bsad-blart,
    augdt            TYPE bsad-augdt,
    umskz            TYPE bsad-umskz,
    vbel2            TYPE bsad-vbel2,
    zuonr            TYPE bsad-zuonr,
    belnr            TYPE bsad-belnr,
    budat            TYPE bsad-budat,
    bldat            TYPE bsad-bldat,
    xblnr            TYPE bsad-xblnr,
    shkzg            TYPE bsad-shkzg,
    gsber            TYPE bsad-gsber,
    dmbtr            TYPE bsad-dmbtr,
    sgtxt            TYPE bsad-sgtxt,
    saknr            TYPE bsad-saknr,
    zfbdt            TYPE bsad-zfbdt,
    zfbdt_sum        TYPE bsad-zfbdt,
    zbd1t            TYPE bsad-zbd1t,
    zterm            TYPE bsad-zterm,
    dmbe2            TYPE bsad-dmbe2,
    xref1            TYPE bsad-xref1,
    xref2            TYPE bsad-xref2,
    xref3            TYPE bsid-xref3,
    bupla            TYPE bsad-bupla,
    augbl            TYPE bsad-augbl,
    gjahr            TYPE bsad-gjahr,
    kidno            TYPE bsad-kidno,
    hkont            TYPE bsad-hkont,

    usnam            TYPE bkpf-usnam,
    tcode            TYPE bkpf-tcode,
    bktxt            TYPE bkpf-bktxt,
    awkey            TYPE bkpf-awkey,
    waers            TYPE bkpf-waers,

    matnr            TYPE vbrp-matnr,
    arktx            TYPE vbrp-arktx,
    fkimg            TYPE vbrp-fkimg,

    bstkd            TYPE vbkd-bstkd,

    name1            TYPE kna1-name1,

    butxt            TYPE t001-butxt,

    name1_tw         TYPE t001w-name1,

    bezei            TYPE tvakt-bezei,





    docnum           TYPE j_1bnfdoc-docnum,
    xnf              TYPE j_1bnfdoc-nfnum,
    status(4)        TYPE c,
    alerta(4)        TYPE c,
    xvlrrec          TYPE bsid-dmbtr,

    txt20            TYPE skat-txt20,
    dias_atr         TYPE i,
    line_color(4)    TYPE c, "Used to store row color attributes
    color_cell       TYPE lvc_t_scol,  " Cell color

    nr_registro_expo TYPE zreg_exportacao-nr_registro_expo,
    instrucao        TYPE zsdt0053-instrucao,
    contrato         TYPE zsdt0045-contrato,
    cpf_cnpj         TYPE zz_stcd1,
  END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.
*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA:
  t_bdc              TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  t_messtab          TYPE TABLE OF bdcmsgcoll,

  it_bsid            TYPE TABLE OF ty_bsid,
  it_bsad            TYPE TABLE OF ty_bsad,
  it_vbsad           TYPE TABLE OF ty_vbsad,
  it_zsdt0054        TYPE TABLE OF ty_zsdt0054,
  it_zsdt0051        TYPE TABLE OF ty_zsdt0051,
  it_bsad_aux        TYPE TABLE OF ty_bsad,
  it_bkpf            TYPE TABLE OF ty_bkpf,
  it_bkpf_aux        TYPE TABLE OF ty_bkpf,
  it_vbrp            TYPE TABLE OF ty_vbrp,
  it_vbkd            TYPE TABLE OF ty_vbkd,
  it_j_1bnflin       TYPE TABLE OF ty_j_1bnflin,
  it_j_1bnfdoc       TYPE TABLE OF ty_j_1bnfdoc,
  it_kna1            TYPE TABLE OF ty_kna1,
  it_t001            TYPE TABLE OF ty_t001,
  it_t001w           TYPE TABLE OF ty_t001w,
  it_vbak            TYPE TABLE OF ty_vbak,
  it_zsdt0053        TYPE TABLE OF ty_zsdt0053,
  it_zsdt0066        TYPE TABLE OF ty_zsdt0066,
  it_zsdt0045        TYPE TABLE OF ty_zsdt0045,
  it_vbak_aux        TYPE TABLE OF ty_vbak,
  it_tvakt           TYPE TABLE OF ty_tvakt,
  it_bsis            TYPE TABLE OF ty_bsis,
  it_skb1            TYPE TABLE OF ty_skb1,
  it_ska1            TYPE TABLE OF ty_ska1,
  it_skat            TYPE TABLE OF ty_skat,
  it_vbfa            TYPE TABLE OF ty_vbfa,
  it_vbfa_m_j        TYPE TABLE OF ty_vbfa_m_j,
  it_zdoc_exp        TYPE TABLE OF ty_zdoc_exp,
  it_zreg_exportacao TYPE TABLE OF ty_zreg_exportacao,
  it_vbap            TYPE TABLE OF ty_vbap,
  it_saida           TYPE TABLE OF ty_saida,
  it_color           TYPE TABLE OF lvc_s_scol.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_cont            TYPE REF TO cl_gui_custom_container,
  wa_alv             TYPE REF TO cl_gui_alv_grid,
  wa_layout          TYPE lvc_s_layo,

  wa_bsid            TYPE ty_bsid,
  wa_bsad            TYPE ty_bsad,
  wa_vbsad           TYPE ty_vbsad,
  wa_zsdt0054        TYPE ty_zsdt0054,
  wa_zsdt0051        TYPE ty_zsdt0051,
  wa_bsad_aux        TYPE ty_bsad,
  wa_bkpf            TYPE ty_bkpf,
  wa_bkpf_aux        TYPE ty_bkpf,
  wa_vbrp            TYPE ty_vbrp,
  wa_vbkd            TYPE ty_vbkd,
  wa_j_1bnflin       TYPE ty_j_1bnflin,
  wa_j_1bnfdoc       TYPE ty_j_1bnfdoc,
  wa_kna1            TYPE ty_kna1,
  wa_t001            TYPE ty_t001,
  wa_t001w           TYPE ty_t001w,
  wa_vbak            TYPE ty_vbak,
  wa_vbak_aux        TYPE ty_vbak,
  wa_zsdt0053        TYPE ty_zsdt0053,
  wa_zsdt0066        TYPE ty_zsdt0066,
  wa_zsdt0045        TYPE ty_zsdt0045,
  wa_tvakt           TYPE ty_tvakt,
  wa_bsis            TYPE ty_bsis,
  wa_skb1            TYPE ty_skb1,
  wa_ska1            TYPE ty_ska1,
  wa_skat            TYPE ty_skat,
  wa_vbfa            TYPE ty_vbfa,
  wa_vbfa_m_j        TYPE ty_vbfa_m_j,
  wa_zdoc_exp        TYPE ty_zdoc_exp,
  wa_zreg_exportacao TYPE ty_zreg_exportacao,
  wa_vbap            TYPE ty_vbap,
  wa_saida           TYPE ty_saida,
  wa_color           TYPE lvc_s_scol.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  it_fcat   TYPE TABLE OF ty_estrutura,
  s_variant TYPE disvariant         , " Tabela Estrutura co
  t_top     TYPE slis_t_listheader,
  xs_events TYPE slis_alv_event,
  events    TYPE slis_t_event,
  gd_layout TYPE slis_layout_alv,
  t_print   TYPE slis_print_alv,
  v_report  LIKE sy-repid,
  t_sort    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
  estrutura TYPE TABLE OF ty_estrutura.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:
                 p_bukrs FOR bsid-bukrs , " Empresa
                 p_kunnr FOR bsid-kunnr , " Cliente
                 p_vbel2 FOR bsid-vbel2,  " O.V - Ordem de Venda
                 p_auart FOR vbak-auart NO INTERVALS,  " Tipo de ordem
                 p_blart FOR bsid-blart,  " Tipo de Documento
                 p_matnr FOR vbrp-matnr.  " Material

SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

    " Partidas em Aberto
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: p_aberto  RADIOBUTTON GROUP rb1 USER-COMMAND sel DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 3(20) TEXT-005 FOR FIELD  p_aberto.
    SELECTION-SCREEN END OF LINE.

    PARAMETER:  p_budat TYPE bsid-budat .

    " Partidas Compensadas
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: p_compen  RADIOBUTTON GROUP rb1.
      SELECTION-SCREEN COMMENT 3(20) TEXT-006 FOR FIELD  p_compen.
    SELECTION-SCREEN END OF LINE.

    SELECT-OPTIONS: p_augdt FOR bsad-augdt .

    " Todas as partidas
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: p_part    RADIOBUTTON GROUP rb1.
      SELECTION-SCREEN COMMENT 3(20) TEXT-007 FOR FIELD   p_part.
    SELECTION-SCREEN END OF LINE.

    SELECT-OPTIONS: p_todas FOR bsad-budat .

  SELECTION-SCREEN: END OF BLOCK b3.

  SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
    PARAMETERS: x_norm LIKE bsid-umskz AS CHECKBOX DEFAULT 'X',
                x_ore  LIKE bsid-umskz AS CHECKBOX,
                x_memo LIKE bsid-umskz AS CHECKBOX.

  SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-056.
  PARAMETERS: p_varia LIKE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b5.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
DATA: vg_repid   LIKE sy-repid,
      vg_variant TYPE disvariant.

INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  s_variant-report  = vg_repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = s_variant
      i_save        = 'A'
    IMPORTING
      es_variant    = s_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE s_variant-variant TO p_varia.
  ENDIF.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:
            f_iniciar_variaves,
            f_seleciona_dados, " Form seleciona dados
  "           f_saida, " Form de saida
            f_imprime_dados.

END-OF-SELECTION.
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
    w_tp_texto(40),
    w_tpdoc_texto(40),
    w_partate_texto(40),
    w_partcom_texto(40),
    w_parttds_texto(40),


    empresa             TYPE c LENGTH 50,
    cliente             TYPE c LENGTH 50,
    material            TYPE c LENGTH 50,
    ordem_v             TYPE c LENGTH 200,
    tipo_ordem          TYPE c LENGTH 200,
    partidas_ate        TYPE c LENGTH 200,
    partidas_com        TYPE c LENGTH 200,
    partidas_tds        TYPE c LENGTH 200,
    tipo_doc            TYPE c LENGTH 200.

  v_report = sy-repid.

  IF p_varia IS NOT INITIAL.
    vg_variant-variant = p_varia.
  ENDIF.

  w_texto3 = 'Relatório de Contas a Receber'.
  PERFORM f_construir_cabecalho USING 'H' w_texto3.

  IF ( p_bukrs NE '' ).
    w_empresa_texto = 'Empresa:'.
    CONCATENATE w_empresa_texto p_bukrs-low INTO empresa      SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' empresa.
  ENDIF.

  IF ( p_kunnr NE '' ).
    w_cliente_texto = 'Cliente:'.
    CONCATENATE w_cliente_texto p_kunnr-low INTO cliente      SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' cliente.
  ENDIF.

  IF ( p_vbel2 NE '' ).
    w_ov_texto = 'Ordem de Venda:'.
    CONCATENATE w_ov_texto p_vbel2-low INTO ordem_v           SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' ordem_v.
  ENDIF.

  IF ( p_auart NE '' ).
    w_tp_texto = 'Tipo de Ordem:'.
    CONCATENATE w_tp_texto p_auart-low INTO tipo_ordem         SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' tipo_ordem.
  ENDIF.

  IF ( p_blart NE '' ).
    w_tpdoc_texto = 'Tipo de Documento:'.
    CONCATENATE w_tpdoc_texto p_blart-low INTO tipo_doc        SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' tipo_doc.
  ENDIF.

  IF ( ( p_aberto EQ 'X' ) AND ( NOT  p_budat IS INITIAL ) ).
    w_partate_texto = 'Período:  '.
    CONCATENATE p_budat+6(2)   '.' p_budat+4(2)  '.' p_budat(4)  INTO w_texto1.
    "CONCATENATE p_budat-low+6(2)   '.' p_budat-low+4(2)  '.' p_budat-low(4)  INTO w_texto1.
    "CONCATENATE p_budat-high+6(2)  '.' p_budat-high+4(2) '.' p_budat-high(4) INTO w_texto2.
    CONCATENATE w_partate_texto w_texto1 INTO partidas_ate  SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' partidas_ate.
  ENDIF.

  IF ( ( p_compen EQ 'X' ) AND ( NOT p_augdt IS INITIAL ) ).
    w_partcom_texto = 'Período:  '.
    CONCATENATE p_augdt-low+6(2)   '.' p_augdt-low+4(2)  '.' p_augdt-low(4)  INTO w_texto1.
    CONCATENATE p_augdt-high+6(2)  '.' p_augdt-high+4(2) '.' p_augdt-high(4) INTO w_texto2.
    CONCATENATE w_partcom_texto w_texto1 ' - ' w_texto2 INTO partidas_com  SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' partidas_com.
  ENDIF.

  IF ( ( p_part EQ 'X' ) AND ( NOT p_todas IS INITIAL ) ).
    w_parttds_texto = 'Período:  '.
    CONCATENATE p_todas-low+6(2)   '.' p_todas-low+4(2)  '.' p_todas-low(4)  INTO w_texto1.
    CONCATENATE p_todas-high+6(2)  '.' p_todas-high+4(2) '.' p_todas-high(4) INTO w_texto2.
    CONCATENATE w_parttds_texto w_texto1 ' - ' w_texto2 INTO partidas_tds  SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' partidas_tds.
  ENDIF.
ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_seleciona_dados.

**  SELECT vbeln auart kunnr
**    FROM vbak
**      INTO TABLE it_vbak
**        WHERE auart IN p_auart
**          AND kunnr IN p_kunnr.
**
**  IF NOT it_vbak IS INITIAL.
**
**    SELECT vbeln instrucao contrato
**       FROM zsdt0053
**          INTO TABLE it_zsdt0053
**             FOR ALL ENTRIES IN it_vbak
**                WHERE vbeln EQ it_vbak-vbeln.
**
**    IF NOT it_zsdt0053 IS INITIAL.
**
**      SELECT instrucao contrato
**         FROM zsdt0045
**            INTO TABLE it_zsdt0045
**               FOR ALL ENTRIES IN it_zsdt0053
**                  WHERE instrucao EQ it_zsdt0053-instrucao.
**
**    ENDIF.
**
**    SELECT vbeln instrucao
**       FROM zsdt0066
**          INTO TABLE it_zsdt0066
**             FOR ALL ENTRIES IN it_vbak
**                WHERE vbeln EQ it_vbak-vbeln.
**
**    IF NOT it_zsdt0066 IS INITIAL.
**
**      SELECT instrucao contrato
**         FROM zsdt0045
**            APPENDING TABLE it_zsdt0045
**               FOR ALL ENTRIES IN it_zsdt0066
**                  WHERE instrucao EQ it_zsdt0066-instrucao.
**
**    ENDIF.
**
**
**  ENDIF.

  IF ( p_aberto = 'X' ). " Seleção de Partidas em aberto
    PERFORM z_select_partidas_aberto.
  ELSEIF ( p_compen = 'X' ). " Seleção de Partidas compensadas
    PERFORM z_select_partidas_compensadas.
  ELSE.
    PERFORM z_select_todas_partidas.
  ENDIF.



  "Dados fornecedor.
  IF it_saida IS NOT INITIAL.
    SELECT * FROM kna1 INTO TABLE @DATA(it_kna1)
      FOR ALL ENTRIES IN @it_saida
      WHERE kunnr EQ @it_saida-kunnr.
*
*    SELECT * FROM lfa1 INTO TABLE @DATA(it_lfa1)
*    FOR ALL ENTRIES IN @it_saida
*    WHERE lifnr EQ @it_saida-kunnr.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<ws_saida>).
      READ TABLE it_kna1 INTO DATA(ws_kna1) WITH KEY kunnr = <ws_saida>-kunnr.
      IF sy-subrc EQ 0.
        CASE ws_kna1-stkzn.
          WHEN abap_true.
            <ws_saida>-cpf_cnpj  = ws_kna1-stcd2.
          WHEN abap_false.
            <ws_saida>-cpf_cnpj  = ws_kna1-stcd1.
        ENDCASE.
      ENDIF.

*      READ TABLE it_lfa1 INTO DATA(ws_lfa1) WITH KEY lifnr = <ws_saida>-kunnr.
*      IF sy-subrc EQ 0.
*        CASE ws_lfa1-stkzn.
*          WHEN abap_true.
*            <ws_saida>-cpf_cnpj  = ws_lfa1-stcd2.
*          WHEN abap_false.
*            <ws_saida>-cpf_cnpj  = ws_lfa1-stcd1.
*        ENDCASE.
*      ENDIF.

      CLEAR: ws_kna1.
    ENDLOOP.

  ENDIF.
ENDFORM.                    "f_seleciona_dados
*&----------------------------------------------------------------------*
*&      Form  Z_SELECT_PARTIDAS_ABERTO
*&----------------------------------------------------------------------*
*&       SELEÇÃO DE PARTIDAS EM ABERTO
*&----------------------------------------------------------------------*
FORM z_select_partidas_aberto.

  IF ( x_norm = 'X' ) .

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks bs~posn2
     FROM bsid AS bs
     INTO TABLE it_bsid
    WHERE bukrs IN p_bukrs
     AND kunnr  IN p_kunnr
     AND blart  IN p_blart
     AND budat  <= p_budat
     AND vbel2  IN p_vbel2
     AND umskz  EQ ''.

    PERFORM:  z_partidas_aberto_bsad,
              z_partidas_aberto,
              z_partidas_aberto_saida,
              z_aberto_bsad_saida.

  ENDIF.

  IF ( x_ore = 'X' ) .

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks bs~posn2
     FROM bsid AS bs
     INTO TABLE it_bsid
    WHERE bukrs IN p_bukrs
     AND kunnr  IN p_kunnr
     AND blart  IN p_blart
     AND budat  <= p_budat
     AND vbel2  IN p_vbel2
     AND umsks  NE ''
     AND umskz  NE 'F'.

    PERFORM:  z_partidas_aberto_bsad_e,
              z_partidas_aberto,
              z_partidas_aberto_saida,
              z_aberto_bsad_saida.

  ENDIF.

  IF ( x_memo = 'X' ).

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks bs~posn2
     FROM bsid AS bs
     INTO TABLE it_bsid
    WHERE bukrs IN p_bukrs
     AND kunnr IN p_kunnr
     AND blart IN p_blart
     AND budat <= p_budat
     AND vbel2 IN p_vbel2
     "AND umsks NE ''
     AND umskz EQ 'F'.

    PERFORM:  z_partidas_aberto,
              z_partidas_aberto_saida.

  ENDIF.

  IF ( ( x_norm = '' ) AND ( x_ore = '' ) AND ( x_memo = '' ) ).

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks bs~posn2
     FROM bsid AS bs
     INTO TABLE it_bsid
    WHERE bukrs IN p_bukrs
     AND kunnr IN p_kunnr
     AND blart IN p_blart
     AND budat <= p_budat
     AND vbel2 IN p_vbel2.

    PERFORM:  z_partidas_aberto,
              z_partidas_aberto_saida.

  ENDIF.




*  IF ( ( x_norm = 'X' ) AND ( x_ore = '' ) AND ( x_memo = '' ) ).
*
*    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
*           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
*           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
*           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
*     FROM bsid AS bs
*     INTO TABLE it_bsid
*    WHERE bukrs IN p_bukrs
*     AND kunnr  IN p_kunnr
*     AND blart  IN p_blart
*     AND budat  <= p_budat
*     AND vbel2  IN p_vbel2
*     AND umskz  EQ ''.
*
*  ELSEIF ( ( x_ore = 'X' ) AND ( x_norm = '' ) AND ( x_memo = '' ) ).
*
*    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
*           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
*           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
*           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
*     FROM bsid AS bs
*     INTO TABLE it_bsid
*    WHERE bukrs IN p_bukrs
*     AND kunnr  IN p_kunnr
*     AND blart  IN p_blart
*     AND budat  <= p_budat
*     AND vbel2  IN p_vbel2
*     AND umsks  NE ''
*     AND umskz  NE 'F'.
*
*  ELSEIF ( ( x_memo = 'X' ) AND ( x_norm = '' ) AND ( x_ore = '' ) ).
*
*    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
*           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
*           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
*           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
*     FROM bsid AS bs
*     INTO TABLE it_bsid
*    WHERE bukrs IN p_bukrs
*     AND kunnr IN p_kunnr
*     AND blart IN p_blart
*     AND budat <= p_budat
*     AND vbel2 IN p_vbel2
*     AND umsks NE ''
*     AND umskz EQ 'F'.
*
*  ELSEIF ( ( x_memo = 'X' ) AND ( x_ore = 'X' ) AND ( x_norm = '' ) ).
*
*    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
*           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
*           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
*           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
*      FROM bsid AS bs
*      INTO TABLE it_bsid
*    WHERE bukrs IN p_bukrs
*      AND kunnr  IN p_kunnr
*      AND blart  IN p_blart
*      AND budat  <= p_budat
*      AND vbel2  IN p_vbel2
*      AND umsks  NE ''
*      AND umskz  NE 'F'.
*
*    PERFORM: z_partidas_aberto,
*             z_partidas_aberto_saida.
*
*    CLEAR: it_bsid[], wa_bsid.
*
*    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
*           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
*           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
*           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
*      FROM bsid AS bs
*      INTO TABLE it_bsid
*    WHERE bukrs IN p_bukrs
*      AND kunnr IN p_kunnr
*      AND blart IN p_blart
*      AND budat <= p_budat
*      AND vbel2 IN p_vbel2
*      AND umsks NE ''
*      AND umskz EQ 'F'.
*
*    PERFORM: z_partidas_aberto,
*             z_partidas_aberto_saida.
*
*
*  ELSE.
*
*    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
*           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
*           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
*           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
*     FROM bsid AS bs
*     INTO TABLE it_bsid
*    WHERE bukrs IN p_bukrs
*     AND kunnr IN p_kunnr
*     AND blart IN p_blart
*     AND budat <= p_budat
*     AND vbel2 IN p_vbel2.
*
*  ENDIF.

*  IF it_bsid[] IS INITIAL .
*    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
*                           'informados' .
*    STOP.
*  ENDIF.
*
*  PERFORM:  z_partidas_aberto,
*            z_partidas_aberto_saida.

ENDFORM.                    " Z_SELECT_PARTIDAS_ABERTO

*&---------------------------------------------------------------------*
*&      Form  Z_SELECT_PARTIDAS_COMPENSADAS
*&---------------------------------------------------------------------*
FORM z_select_partidas_compensadas .

  IF p_augdt IS INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Data!'.
    STOP.
  ENDIF.

  IF ( ( x_norm = 'X' ) AND ( x_ore = '' ) AND ( x_memo = '' ) ).

    SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
           bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
           bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
           bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
      FROM bsad AS bsa
      INTO TABLE it_bsad
      WHERE bukrs IN p_bukrs
      AND   vbel2 IN p_vbel2
      AND   kunnr IN p_kunnr
      AND   blart IN p_blart
      AND   augdt IN p_augdt
      AND   umskz EQ ''.

  ELSEIF ( ( x_ore = 'X' ) AND ( x_norm = '' ) AND ( x_memo = '' ) ).

    SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
           bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
           bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
           bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
      FROM bsad AS bsa
      INTO TABLE it_bsad
    WHERE bukrs IN p_bukrs
      AND   vbel2 IN p_vbel2
      AND   kunnr IN p_kunnr
      AND   blart IN p_blart
      AND   augdt IN p_augdt
      AND   umskz NE ''.


  ELSE.

    SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
           bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
           bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
           bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
      FROM bsad AS bsa
      INTO TABLE it_bsad
    WHERE   bukrs IN p_bukrs
      AND   vbel2 IN p_vbel2
      AND   kunnr IN p_kunnr
      AND   blart IN p_blart
      AND   augdt IN p_augdt
      AND   umskz NE 'F'.

  ENDIF.

  IF it_bsad[] IS INITIAL .
    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.

  PERFORM: z_partidas_compensadas,
           z_partidas_compensadas_saida.

ENDFORM.                    " Z_SELECT_PARTIDAS_COMPENSADAS
*&---------------------------------------------------------------------*
*&      Form  Z_SELECT_TODAS_PARTIDAS
*&---------------------------------------------------------------------*
FORM z_select_todas_partidas .

  IF p_todas IS INITIAL.
    MESSAGE i000(z01) WITH 'É Obrigatório informar a Data!'.
    STOP.
  ENDIF.

  IF ( ( x_norm = 'X' ) AND ( x_ore = '' ) AND ( x_memo = '' ) ).

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
     FROM bsid AS bs
     INTO TABLE it_bsid
    WHERE bukrs IN p_bukrs
     AND  kunnr IN p_kunnr
     AND  blart IN p_blart
     AND  budat IN p_todas
     AND  vbel2 IN p_vbel2
     AND  umskz EQ ''.

    PERFORM: z_partidas_aberto,
             z_partidas_aberto_saida.

    SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
           bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
           bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
           bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
      FROM bsad AS bsa
      INTO TABLE it_bsad
      WHERE bukrs IN p_bukrs
      AND   vbel2 IN p_vbel2
      AND   kunnr IN p_kunnr
      AND   blart IN p_blart
      AND   augdt IN p_todas
      AND   umskz EQ ''.


    PERFORM: z_partidas_compensadas,
             z_partidas_compensadas_saida.

  ELSEIF ( ( x_ore = 'X' ) AND ( x_norm = '' ) AND ( x_memo = '' ) ).

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
     FROM bsid AS bs
     INTO TABLE it_bsid
   WHERE bukrs IN p_bukrs
     AND kunnr IN p_kunnr
     AND blart IN p_blart
     AND budat IN p_todas
     AND vbel2 IN p_vbel2
     AND umsks NE ''
     AND umskz NE 'F'.

    PERFORM: z_partidas_aberto,
             z_partidas_aberto_saida.

    SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
           bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
           bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
           bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
      FROM bsad AS bsa
      INTO TABLE it_bsad
    WHERE   bukrs IN p_bukrs
      AND   vbel2 IN p_vbel2
      AND   kunnr IN p_kunnr
      AND   blart IN p_blart
      AND   augdt IN p_todas
      AND   umskz NE ''.

    PERFORM: z_partidas_compensadas,
             z_partidas_compensadas_saida.

  ELSEIF ( ( x_memo = 'X' ) AND ( x_ore = '' ) AND ( x_norm = '' ) ) .

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
     FROM bsid AS bs
     INTO TABLE it_bsid
    WHERE bukrs IN p_bukrs
     AND  kunnr IN p_kunnr
     AND  blart IN p_blart
     AND  budat <= p_budat
     AND  vbel2 IN p_vbel2
     AND  umsks NE ''
     AND  umskz EQ 'F'.

    PERFORM: z_partidas_aberto,
             z_partidas_aberto_saida.

    SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
           bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
           bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
           bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
      FROM bsad AS bsa
      INTO TABLE it_bsad
    WHERE   bukrs IN p_bukrs
      AND   vbel2 IN p_vbel2
      AND   kunnr IN p_kunnr
      AND   blart IN p_blart
      AND   augdt IN p_todas.

    PERFORM: z_partidas_compensadas,
             z_partidas_compensadas_saida.

  ELSEIF ( ( x_memo = 'X' ) AND ( x_ore = 'X' ) AND ( x_norm = '' ) ).

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
      FROM bsid AS bs
      INTO TABLE it_bsid
    WHERE bukrs IN p_bukrs
      AND kunnr  IN p_kunnr
      AND blart  IN p_blart
      AND budat  <= p_budat
      AND vbel2  IN p_vbel2
      AND umsks  NE ''
      AND umskz  NE 'F'.

    PERFORM: z_partidas_aberto,
             z_partidas_aberto_saida.

    CLEAR: it_bsid[], wa_bsid.

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
      FROM bsid AS bs
      INTO TABLE it_bsid
    WHERE bukrs IN p_bukrs
      AND kunnr IN p_kunnr
      AND blart IN p_blart
      AND budat <= p_budat
      AND vbel2 IN p_vbel2
      AND umsks NE ''
      AND umskz EQ 'F'.

    PERFORM: z_partidas_aberto,
             z_partidas_aberto_saida.

    SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
           bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
           bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
           bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
      FROM bsad AS bsa
      INTO TABLE it_bsad
    WHERE   bukrs IN p_bukrs
      AND   vbel2 IN p_vbel2
      AND   kunnr IN p_kunnr
      AND   blart IN p_blart
      AND   augdt IN p_todas.

    PERFORM: z_partidas_compensadas,
             z_partidas_compensadas_saida.

  ELSE.

    SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
           bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
           bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
           bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont bs~umsks
     FROM bsid AS bs
     INTO TABLE it_bsid
    WHERE bukrs IN p_bukrs
     AND kunnr  IN p_kunnr
     AND blart  IN p_blart
     AND budat  IN p_todas
     AND vbel2  IN p_vbel2.

    PERFORM: z_partidas_aberto,
             z_partidas_aberto_saida.

    SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
           bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
           bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
           bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
      FROM bsad AS bsa
      INTO TABLE it_bsad
    WHERE   bukrs IN p_bukrs
      AND   vbel2 IN p_vbel2
      AND   kunnr IN p_kunnr
      AND   blart IN p_blart
      AND   augdt IN p_todas.

    PERFORM: z_partidas_compensadas,
             z_partidas_compensadas_saida.
  ENDIF.

ENDFORM.                    " Z_SELECT_TODAS_PARTIDAS

*&---------------------------------------------------------------------*
*&      Form  z_partidas_aberto
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_partidas_aberto.

  CHECK NOT it_bsid[] IS INITIAL.

  SELECT bukrs gjahr belnr augbl
     FROM bsad
    INTO TABLE it_vbsad
    FOR ALL ENTRIES IN it_bsid
    WHERE bukrs EQ it_bsid-bukrs
      AND kunnr EQ it_bsid-kunnr
      AND augbl EQ it_bsid-belnr
      AND gjahr EQ it_bsid-gjahr.

  IF it_vbsad IS NOT INITIAL.

    SELECT nro_sol_ov adiant FROM zsdt0054
      INTO TABLE it_zsdt0054
        FOR ALL ENTRIES IN it_vbsad
          WHERE adiant EQ it_vbsad-belnr.

    IF it_zsdt0054 IS NOT INITIAL.

      SELECT nro_sol_ov bstkd FROM zsdt0051
        INTO TABLE it_zsdt0051
          FOR ALL ENTRIES IN it_zsdt0054
            WHERE nro_sol_ov EQ it_zsdt0054-nro_sol_ov.

    ENDIF.
  ENDIF.

  SELECT nro_sol_ov adiant FROM zsdt0054
    APPENDING TABLE it_zsdt0054
     FOR ALL ENTRIES IN it_bsid
       WHERE adiant EQ it_bsid-belnr.

  SORT it_zsdt0054 BY nro_sol_ov adiant.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0054 COMPARING nro_sol_ov adiant.

  IF it_zsdt0054 IS NOT INITIAL.

    SELECT nro_sol_ov bstkd FROM zsdt0051
      APPENDING TABLE it_zsdt0051
        FOR ALL ENTRIES IN it_zsdt0054
          WHERE nro_sol_ov EQ it_zsdt0054-nro_sol_ov.

  ENDIF.


  SELECT vbeln matnr arktx fkimg
    FROM vbrp
    INTO TABLE it_vbrp
     FOR ALL ENTRIES IN it_bsid
   WHERE vbeln EQ it_bsid-vbeln.

  SELECT vbeln bstkd
    FROM vbkd
    INTO TABLE it_vbkd
     FOR ALL ENTRIES IN it_bsid
   WHERE vbeln EQ it_bsid-vbel2.

  SELECT kunnr name1
    FROM kna1
    INTO TABLE it_kna1
     FOR ALL ENTRIES IN it_bsid
   WHERE kunnr EQ it_bsid-kunnr.

  SELECT bukrs butxt
    FROM t001
    INTO TABLE it_t001
     FOR ALL ENTRIES IN it_bsid
   WHERE bukrs EQ it_bsid-bukrs.

  SELECT werks name1
    FROM t001w
    INTO TABLE it_t001w
     FOR ALL ENTRIES IN it_bsid
   WHERE werks EQ it_bsid-bupla.


  SELECT  vbeln posnr matnr  arktx
   FROM vbap
   INTO TABLE it_vbap
    FOR ALL ENTRIES IN it_bsid
  WHERE vbeln EQ it_bsid-vbel2
    AND posnr EQ it_bsid-posn2.

  SELECT  vbeln vbtyp_v vbelv
    FROM vbfa
    INTO TABLE it_vbfa
     FOR ALL ENTRIES IN it_bsid
   WHERE vbeln EQ it_bsid-vbeln
     AND vbtyp_v = 'C'.


*  IF sy-subrc IS INITIAL.

**    SELECT vbeln auart kunnr
**      FROM vbak
**      INTO TABLE it_vbak_aux
**       FOR ALL ENTRIES IN it_vbfa
**     WHERE vbeln EQ it_vbfa-vbelv.
*              ENDIF.

  SELECT vbeln auart kunnr
    FROM vbak
   INTO TABLE it_vbak
     FOR ALL ENTRIES IN it_bsid
     WHERE vbeln = it_bsid-vbel2
       AND auart IN p_auart
       AND kunnr IN p_kunnr.

  IF NOT it_vbak IS INITIAL.

    SELECT vbeln instrucao contrato
       FROM zsdt0053
          INTO TABLE it_zsdt0053
             FOR ALL ENTRIES IN it_vbak
                WHERE vbeln EQ it_vbak-vbeln.

    IF NOT it_zsdt0053 IS INITIAL.

      SELECT instrucao contrato
         FROM zsdt0045
            INTO TABLE it_zsdt0045
               FOR ALL ENTRIES IN it_zsdt0053
                  WHERE instrucao EQ it_zsdt0053-instrucao.

    ENDIF.

    SELECT vbeln instrucao
       FROM zsdt0066
          INTO TABLE it_zsdt0066
             FOR ALL ENTRIES IN it_vbak
                WHERE vbeln EQ it_vbak-vbeln.

    IF NOT it_zsdt0066 IS INITIAL.

      SELECT instrucao contrato
         FROM zsdt0045
            APPENDING TABLE it_zsdt0045
               FOR ALL ENTRIES IN it_zsdt0066
                  WHERE instrucao EQ it_zsdt0066-instrucao.

    ENDIF.

    SELECT auart bezei
      FROM tvakt
      INTO TABLE it_tvakt
       FOR ALL ENTRIES IN it_vbak
     WHERE auart EQ it_vbak-auart
      AND spras EQ sy-langu. "CS2023000476 Melhoria - Validar Coluna Tipo OV ZFIS20 - SMC

  ENDIF.

  SELECT bukrs belnr gjahr usnam tcode bktxt awkey waers
    FROM bkpf
    INTO TABLE it_bkpf_aux
     FOR ALL ENTRIES IN it_bsid
   WHERE bukrs EQ it_bsid-bukrs
     AND belnr EQ it_bsid-belnr
     AND gjahr EQ it_bsid-gjahr.

  IF sy-subrc IS INITIAL.

    LOOP AT it_bkpf_aux INTO wa_bkpf_aux.
      wa_bkpf_aux-awkey_aux = wa_bkpf_aux-awkey.
      APPEND wa_bkpf_aux TO it_bkpf.
    ENDLOOP.

*& SE CS2016000384
    DELETE it_bkpf WHERE awkey_aux IS INITIAL.
*& SE CS2016000384

    SELECT refkey docnum
      FROM j_1bnflin
      INTO TABLE it_j_1bnflin
       FOR ALL ENTRIES IN it_bkpf
     WHERE refkey EQ it_bkpf-awkey_aux.

*& SE CS2016000384
    IF NOT it_j_1bnflin IS INITIAL.

      SELECT docnum nfe nfenum nfnum
        FROM j_1bnfdoc
        INTO TABLE it_j_1bnfdoc
         FOR ALL ENTRIES IN it_j_1bnflin
       WHERE docnum EQ it_j_1bnflin-docnum
         AND nfe EQ 'X'.

    ENDIF.
*& SE CS2016000384

  ENDIF.

*& SE CS2016000384
  CHECK NOT it_bsid IS INITIAL.

  SELECT vbeln vbelv
    FROM vbfa
       INTO TABLE it_vbfa_m_j
          FOR ALL ENTRIES IN it_bsid
            WHERE vbeln EQ it_bsid-vbeln
              AND vbtyp_n EQ 'M'
              AND vbtyp_v EQ 'J'.

  IF NOT it_vbfa_m_j IS INITIAL.

    SELECT vbeln id_registro_expo
      FROM zdoc_exp
        INTO TABLE it_zdoc_exp
            FOR ALL ENTRIES IN it_vbfa_m_j
            WHERE vbeln EQ it_vbfa_m_j-vbelv.

    IF NOT it_zdoc_exp IS INITIAL.

      SELECT id_registro_expo nr_registro_expo
          FROM zreg_exportacao
              INTO TABLE it_zreg_exportacao
                FOR ALL ENTRIES IN it_zdoc_exp
                    WHERE id_registro_expo EQ it_zdoc_exp-id_registro_expo.

    ENDIF.

  ENDIF.
*& SE CS2016000384

ENDFORM.                    "z_bkpf_aberto
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_COMPENSADAS
*&---------------------------------------------------------------------*
FORM z_partidas_compensadas.

  CHECK NOT it_bsad IS INITIAL.


  SELECT nro_sol_ov adiant FROM zsdt0054
    INTO TABLE it_zsdt0054
      FOR ALL ENTRIES IN it_bsad
        WHERE adiant EQ it_bsad-belnr.

  IF it_zsdt0054 IS NOT INITIAL.

    SELECT nro_sol_ov bstkd FROM zsdt0051
      INTO TABLE it_zsdt0051
        FOR ALL ENTRIES IN it_zsdt0054
          WHERE nro_sol_ov EQ it_zsdt0054-nro_sol_ov.

  ENDIF.

  SELECT bs~bukrs bs~kunnr bs~blart bs~budat bs~umskz bs~vbel2 bs~zuonr bs~belnr
         bs~bldat bs~waers bs~xblnr bs~shkzg bs~gsber bs~dmbtr bs~sgtxt bs~saknr
         bs~zfbdt bs~zbd1t bs~zterm bs~dmbe2 bs~xref1 bs~xref2 bs~xref3 bs~kidno
         bs~bupla bs~gjahr bs~vbeln bs~augbl bs~augdt bs~rebzg bs~hkont
    FROM bsid AS bs
    INTO TABLE it_bsid
     FOR ALL ENTRIES IN it_bsad
   WHERE bukrs EQ it_bsad-bukrs
     AND belnr EQ it_bsad-augbl
     AND gjahr EQ it_bsad-gjahr.

  SELECT vbeln matnr arktx fkimg
    FROM vbrp
    INTO TABLE it_vbrp
     FOR ALL ENTRIES IN it_bsad
   WHERE vbeln EQ it_bsad-vbeln.

  SELECT vbeln bstkd
    FROM vbkd
    INTO TABLE it_vbkd
     FOR ALL ENTRIES IN it_bsad
   WHERE vbeln EQ it_bsad-vbel2.

  SELECT kunnr name1
    FROM kna1
    INTO TABLE it_kna1
     FOR ALL ENTRIES IN it_bsad
   WHERE kunnr EQ it_bsad-kunnr.

  SELECT bukrs butxt
    FROM t001
    INTO TABLE it_t001
     FOR ALL ENTRIES IN it_bsad
   WHERE bukrs EQ it_bsad-bukrs.

  SELECT werks name1
    FROM t001w
    INTO TABLE it_t001w
     FOR ALL ENTRIES IN it_bsad
   WHERE werks EQ it_bsad-bupla.

  SELECT bukrs belnr gjahr hkont
    FROM bsis
    INTO TABLE it_bsis
     FOR ALL ENTRIES IN it_bsad
   WHERE bukrs EQ it_bsad-bukrs
     AND belnr EQ it_bsad-augbl
     AND gjahr EQ it_bsad-gjahr.

  IF it_bsis IS NOT INITIAL.

    SELECT bukrs saknr fstag
      FROM skb1
      INTO TABLE it_skb1
       FOR ALL ENTRIES IN it_bsis
     WHERE bukrs EQ it_bsis-bukrs
       AND saknr EQ it_bsis-hkont
       AND fstag = 'YB04'.

    IF ( sy-subrc = 0 ).

      SELECT ktopl saknr               "#EC CI_DB_OPERATION_OK[2431747]
        FROM ska1                      "#EC CI_DB_OPERATION_OK[2389136]
        INTO TABLE it_ska1
         FOR ALL ENTRIES IN it_skb1
       WHERE saknr EQ it_skb1-saknr
         AND ktopl = '0050'.

      SELECT saknr txt20
        FROM skat
        INTO TABLE it_skat
         FOR ALL ENTRIES IN it_ska1
       WHERE saknr EQ it_ska1-saknr.

    ENDIF.

  ENDIF.

  SELECT vbeln auart kunnr
  FROM vbak
 INTO TABLE it_vbak
   FOR ALL ENTRIES IN it_bsad
   WHERE vbeln = it_bsad-vbel2
     AND auart IN p_auart
     AND kunnr IN p_kunnr.

  IF NOT it_vbak IS INITIAL.

    SELECT vbeln instrucao contrato
       FROM zsdt0053
          INTO TABLE it_zsdt0053
             FOR ALL ENTRIES IN it_vbak
                WHERE vbeln EQ it_vbak-vbeln.

    IF NOT it_zsdt0053 IS INITIAL.

      SELECT instrucao contrato
         FROM zsdt0045
            INTO TABLE it_zsdt0045
               FOR ALL ENTRIES IN it_zsdt0053
                  WHERE instrucao EQ it_zsdt0053-instrucao.

    ENDIF.

    SELECT vbeln instrucao
       FROM zsdt0066
          INTO TABLE it_zsdt0066
             FOR ALL ENTRIES IN it_vbak
                WHERE vbeln EQ it_vbak-vbeln.

    IF NOT it_zsdt0066 IS INITIAL.

      SELECT instrucao contrato
         FROM zsdt0045
            APPENDING TABLE it_zsdt0045
               FOR ALL ENTRIES IN it_zsdt0066
                  WHERE instrucao EQ it_zsdt0066-instrucao.

    ENDIF.

    SELECT auart bezei
      FROM tvakt
      INTO TABLE it_tvakt
       FOR ALL ENTRIES IN it_vbak
     WHERE auart EQ it_vbak-auart
      AND spras EQ sy-langu. "CS2023000476 Melhoria - Validar Coluna Tipo OV ZFIS20 - SMC

  ENDIF.

  SELECT bukrs belnr gjahr usnam tcode bktxt awkey waers
    FROM bkpf
    INTO TABLE it_bkpf_aux
     FOR ALL ENTRIES IN it_bsad
   WHERE bukrs EQ it_bsad-bukrs
     AND belnr EQ it_bsad-belnr
     AND gjahr EQ it_bsad-gjahr.

  IF sy-subrc IS INITIAL.

    LOOP AT it_bkpf_aux INTO wa_bkpf_aux.
      wa_bkpf_aux-awkey_aux = wa_bkpf_aux-awkey.
      APPEND wa_bkpf_aux TO it_bkpf.
    ENDLOOP.

*& SE CS2016000384
    DELETE it_bkpf WHERE awkey_aux IS INITIAL.
*& SE CS2016000384

    SELECT refkey docnum
      FROM j_1bnflin
      INTO TABLE it_j_1bnflin
       FOR ALL ENTRIES IN it_bkpf
     WHERE refkey EQ it_bkpf-awkey_aux.

*& SE CS2016000384
    IF  NOT it_j_1bnflin IS INITIAL.

      SELECT docnum nfe nfenum nfnum
        FROM j_1bnfdoc
        INTO TABLE it_j_1bnfdoc
         FOR ALL ENTRIES IN it_j_1bnflin
       WHERE docnum EQ it_j_1bnflin-docnum
        AND nfe EQ 'X'.

    ENDIF.
*& SE CS2016000384

  ENDIF.

*& SE CS2016000384

  SELECT vbeln vbelv
    FROM vbfa
       INTO TABLE it_vbfa_m_j
          FOR ALL ENTRIES IN it_bsad
            WHERE vbeln EQ it_bsad-vbeln
              AND vbtyp_n EQ 'M'
              AND vbtyp_v EQ 'J'.

  IF NOT it_vbfa_m_j IS INITIAL.

    SELECT vbeln id_registro_expo
      FROM zdoc_exp
        INTO TABLE it_zdoc_exp
            FOR ALL ENTRIES IN it_vbfa_m_j
            WHERE vbeln EQ it_vbfa_m_j-vbelv.

    IF NOT it_zdoc_exp IS INITIAL.

      SELECT id_registro_expo nr_registro_expo
          FROM zreg_exportacao
              INTO TABLE it_zreg_exportacao
                FOR ALL ENTRIES IN it_zdoc_exp
                    WHERE id_registro_expo EQ it_zdoc_exp-id_registro_expo.

    ENDIF.

  ENDIF.
*& SE CS2016000384

ENDFORM.                    " Z_PARTIDAS_COMPENSADAS


*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_ABERTO_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_partidas_aberto_saida.

  SORT: it_bsid      BY vbel2,
        it_vbak      BY vbeln,
        it_vbak_aux  BY vbeln,
        it_vbfa      BY vbeln,
        it_vbap      BY vbeln posnr,
        it_tvakt     BY auart,
        it_bkpf      BY belnr,
        it_vbrp      BY vbeln,
        it_vbkd      BY vbeln,
        it_kna1      BY kunnr,
        it_t001      BY bukrs,
        it_t001w     BY werks,
        it_j_1bnflin BY refkey,
        it_j_1bnfdoc BY docnum,
        it_zsdt0053  BY vbeln,
        it_zsdt0066  BY vbeln,
        it_zsdt0045 BY instrucao.

  DELETE ADJACENT DUPLICATES FROM it_t001  COMPARING bukrs.
  DELETE ADJACENT DUPLICATES FROM it_t001w COMPARING werks.
  DELETE ADJACENT DUPLICATES FROM it_kna1  COMPARING kunnr.


  LOOP  AT it_bsid INTO wa_bsid.

    wa_saida-status =  icon_led_red.
    wa_saida-bukrs  =  wa_bsid-bukrs.
    wa_saida-kunnr  =  wa_bsid-kunnr.
    wa_saida-saknr  =  wa_bsid-saknr.
    wa_saida-xblnr  =  wa_bsid-xblnr.
    wa_saida-zuonr  =  wa_bsid-zuonr.
    wa_saida-blart  =  wa_bsid-blart.
    wa_saida-belnr  =  wa_bsid-belnr.
    wa_saida-augbl  =  wa_bsid-augbl.
    wa_saida-vbel2  =  wa_bsid-vbel2.
    wa_saida-bldat  =  wa_bsid-bldat.
    wa_saida-budat  =  wa_bsid-budat.
    wa_saida-augdt  =  wa_bsid-augdt.
    wa_saida-zterm  =  wa_bsid-zterm.
    wa_saida-gsber  =  wa_bsid-gsber.
    wa_saida-bupla  =  wa_bsid-bupla.
    wa_saida-umskz  =  wa_bsid-umskz.
    wa_saida-waers  =  wa_bsid-waers.
    wa_saida-gjahr  =  wa_bsid-gjahr.
    wa_saida-vbeln  =  wa_bsid-vbeln.
    wa_saida-hkont  =  wa_bsid-hkont.

    READ TABLE it_vbfa_m_j INTO wa_vbfa_m_j WITH KEY vbeln = wa_bsid-vbeln.
    READ TABLE it_zdoc_exp INTO wa_zdoc_exp WITH KEY vbeln = wa_vbfa_m_j-vbelv.
    READ TABLE it_zreg_exportacao INTO wa_zreg_exportacao WITH KEY id_registro_expo = wa_zdoc_exp-id_registro_expo.
    IF sy-subrc IS INITIAL.
      wa_saida-nr_registro_expo = wa_zreg_exportacao-nr_registro_expo.
    ENDIF.


    wa_saida-sgtxt  =  wa_bsid-sgtxt.
    wa_saida-kidno  =  wa_bsid-kidno.
    wa_saida-xref1  =  wa_bsid-xref1.
    wa_saida-xref2  =  wa_bsid-xref2.
    wa_saida-xref3  =  wa_bsid-xref3.

    " Calculo para data de vencimento.


    wa_saida-zfbdt_sum = wa_bsid-zfbdt + wa_bsid-zbd1t.
    IF ( wa_saida-zfbdt_sum < sy-datum ).
      "wa_saida-line_color = 'C600'.
      MOVE 'ZFBDT_SUM'    TO wa_color-fname.
      MOVE '6'         TO wa_color-color-col.
      MOVE '1'         TO wa_color-color-int.
      MOVE '0'         TO wa_color-color-inv.
      APPEND wa_color TO it_color.
      wa_saida-color_cell[] = it_color[].
      REFRESH it_color.
      wa_saida-alerta = icon_alert.
    ENDIF.
    wa_saida-dias_atr = sy-datum - wa_saida-zfbdt_sum.


    IF ( wa_bsid-shkzg = 'H' ).
      wa_saida-dmbtr   =  wa_bsid-dmbtr * ( -1 ).
*      wa_saida-dmbe2   = 0.
      wa_saida-dmbe2   =  wa_bsid-dmbe2 * ( -1 ).
    ELSE.
      wa_saida-dmbtr  =  wa_bsid-dmbtr.
      wa_saida-dmbe2  =  wa_bsid-dmbe2.
    ENDIF.

    IF NOT wa_bsid-vbel2 IS INITIAL.

      READ TABLE it_vbak INTO wa_vbak   WITH KEY vbeln = wa_bsid-vbel2 BINARY SEARCH.
      wa_saida-auart  =  wa_vbak-auart.

      READ TABLE it_zsdt0053 INTO wa_zsdt0053 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH .
      IF sy-subrc IS INITIAL .
        wa_saida-instrucao = wa_zsdt0053-instrucao.
        wa_saida-contrato = wa_zsdt0053-contrato.
      ELSE.
        READ TABLE it_zsdt0066 INTO wa_zsdt0066 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-instrucao = wa_zsdt0066-instrucao.
        ENDIF.
      ENDIF.

      READ TABLE it_zsdt0045 INTO wa_zsdt0045 WITH KEY instrucao = wa_saida-instrucao BINARY SEARCH.
      IF sy-subrc IS INITIAL .
        wa_saida-contrato = wa_zsdt0045-contrato.
      ENDIF.

    ENDIF.

    READ TABLE it_vbsad INTO wa_vbsad WITH KEY bukrs = wa_bsid-bukrs
                                               augbl = wa_bsid-belnr
                                               gjahr = wa_bsid-gjahr.
    IF sy-subrc IS INITIAL.

      READ TABLE it_zsdt0054 INTO wa_zsdt0054 WITH KEY adiant = wa_vbsad-belnr.
      READ TABLE it_zsdt0051 INTO wa_zsdt0051 WITH KEY nro_sol_ov = wa_zsdt0054-nro_sol_ov.
      IF sy-subrc IS INITIAL .
        wa_saida-contrato = wa_zsdt0051-bstkd.
      ENDIF.
    ELSE.

      READ TABLE it_zsdt0054 INTO wa_zsdt0054 WITH KEY adiant = wa_bsid-belnr.
      READ TABLE it_zsdt0051 INTO wa_zsdt0051 WITH KEY nro_sol_ov = wa_zsdt0054-nro_sol_ov.
      IF sy-subrc IS INITIAL.
        wa_saida-contrato = wa_zsdt0051-bstkd.
      ENDIF.

    ENDIF.

    READ TABLE it_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart   BINARY SEARCH.
    wa_saida-bezei = wa_tvakt-bezei.

    READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbeln = wa_bsid-vbeln BINARY SEARCH.
    "wa_saida-vbel2 = wa_vbfa-vbelv.


    "CS2023000476 Melhoria - Validar Coluna Tipo OV ZFIS20 - SMC - Comentado pois nesse IF está trocando o Auart lido anteriormente correto para auart errado.
    "    IF NOT wa_bsid-vbel2 IS INITIAL.
    "      READ TABLE it_vbak_aux INTO wa_vbak_aux WITH KEY vbeln = wa_vbfa-vbelv BINARY SEARCH.
    "      wa_saida-auart  =  wa_vbak_aux-auart.
    "    ENDIF.
    "CS2023000476 Melhoria - Validar Coluna Tipo OV ZFIS20 - SMC

    READ TABLE it_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak_aux-auart   BINARY SEARCH.
    wa_saida-bezei = wa_tvakt-bezei.




    READ TABLE it_bkpf  INTO wa_bkpf  WITH KEY belnr = wa_bsid-belnr  BINARY SEARCH.
    wa_saida-usnam = wa_bkpf-usnam.
    wa_saida-tcode = wa_bkpf-tcode.
    wa_saida-bktxt = wa_bkpf-bktxt.
    wa_saida-awkey = wa_bkpf-awkey.

    READ TABLE it_vbrp  INTO wa_vbrp  WITH KEY vbeln  = wa_bsid-vbeln   BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-matnr  = wa_vbrp-matnr.
      wa_saida-arktx  = wa_vbrp-arktx.
    ELSE.
      READ TABLE it_vbap  INTO wa_vbap  WITH KEY vbeln  = wa_bsid-vbel2
                                                 posnr  = wa_bsid-posn2 BINARY SEARCH.
      wa_saida-matnr  = wa_vbap-matnr.
      wa_saida-arktx  = wa_vbap-arktx.
    ENDIF.



    IF ( wa_bsid-blart <> 'RV' ).
      wa_saida-fkimg  = 0.
    ELSE.
      wa_saida-fkimg  = wa_vbrp-fkimg.
    ENDIF.

    READ TABLE it_vbkd      INTO wa_vbkd      WITH KEY vbeln  = wa_bsid-vbel2   BINARY SEARCH.
    wa_saida-bstkd = wa_vbkd-bstkd.

    READ TABLE it_kna1      INTO wa_kna1      WITH KEY kunnr  = wa_bsid-kunnr   BINARY SEARCH.
    wa_saida-name1 = wa_kna1-name1.

    READ TABLE it_t001      INTO wa_t001      WITH KEY bukrs  = wa_bsid-bukrs   BINARY SEARCH.
    wa_saida-butxt = wa_t001-butxt.

    READ TABLE it_t001w     INTO wa_t001w     WITH KEY werks  = wa_bsid-bupla   BINARY SEARCH.
    wa_saida-name1_tw = wa_t001w-name1.

    READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = wa_bkpf-awkey_aux   BINARY SEARCH.
    READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.

    IF ( wa_j_1bnfdoc-nfe = 'X' ).
      wa_saida-xnf = wa_j_1bnfdoc-nfenum.
    ELSE.
      wa_saida-xnf = wa_j_1bnfdoc-nfnum.
    ENDIF.

    APPEND wa_saida TO it_saida.

    CLEAR: wa_bsid, wa_vbak, wa_vbak_aux, wa_tvakt, wa_bkpf, wa_vbrp, wa_vbkd,
           wa_kna1, wa_t001, wa_t001w, wa_j_1bnflin, wa_j_1bnfdoc, wa_saida,
           wa_zreg_exportacao, wa_zdoc_exp, wa_vbfa_m_j, wa_zsdt0053, wa_zsdt0066,
           wa_zsdt0045, wa_vbsad, wa_zsdt0054, wa_zsdt0051.

  ENDLOOP.

ENDFORM.                    " Z_PARTIDAS_ABERTO_SAIDA
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_COMPENSADAS_SAIDA
*&---------------------------------------------------------------------*
FORM z_partidas_compensadas_saida.



  SORT: it_bsad  BY vbel2,
        it_vbak  BY vbeln,
        it_bkpf  BY belnr,
        it_vbrp  BY vbeln,
        it_vbkd  BY vbeln,
        it_kna1  BY kunnr,
        it_t001  BY bukrs,
        it_t001w BY werks,
        it_tvakt BY auart,
        it_bsid  BY bukrs,
        it_bsis  BY belnr,
        it_skb1  BY bukrs,
        it_ska1  BY saknr,
        it_skat  BY saknr,
        it_zsdt0053 BY vbeln,
        it_zsdt0066 BY vbeln,
         it_zsdt0045 BY instrucao.

  LOOP AT it_bsad INTO wa_bsad.

    wa_saida-status = icon_led_green.
    wa_saida-bukrs = wa_bsad-bukrs.
    wa_saida-kunnr = wa_bsad-kunnr.
    wa_saida-saknr = wa_bsad-saknr.
    wa_saida-xblnr = wa_bsad-xblnr.
    wa_saida-zuonr = wa_bsad-zuonr.
    wa_saida-blart = wa_bsad-blart.
    wa_saida-belnr = wa_bsad-belnr.
    wa_saida-augbl = wa_bsad-augbl.
    wa_saida-vbeln = wa_bsad-vbeln.
    wa_saida-vbel2 = wa_bsad-vbel2.
    wa_saida-bldat = wa_bsad-bldat.
    wa_saida-budat = wa_bsad-budat.
    wa_saida-augdt = wa_bsad-augdt.
    wa_saida-hkont = wa_bsad-hkont.

    READ TABLE it_vbfa_m_j INTO wa_vbfa_m_j WITH KEY vbeln = wa_bsad-vbeln.
    READ TABLE it_zdoc_exp INTO wa_zdoc_exp WITH KEY vbeln = wa_vbfa_m_j-vbelv.
    READ TABLE it_zreg_exportacao INTO wa_zreg_exportacao WITH KEY id_registro_expo = wa_zdoc_exp-id_registro_expo.
    IF sy-subrc IS INITIAL.
      wa_saida-nr_registro_expo = wa_zreg_exportacao-nr_registro_expo.
    ENDIF.

    " Calculo para data de vencimento.
    wa_saida-zfbdt_sum = wa_bsad-zfbdt + wa_bsad-zbd1t.

    IF ( wa_saida-zfbdt_sum < sy-datum ).
      wa_saida-alerta = icon_alert.
      "WA_SAIDA-LINE_COLOR = 'C610'.
    ENDIF.
    wa_saida-dias_atr = sy-datum - wa_saida-zfbdt_sum.

    wa_saida-zterm = wa_bsad-zterm.
    wa_saida-gsber = wa_bsad-gsber.
    wa_saida-bupla = wa_bsad-bupla.
    wa_saida-umskz = wa_bsad-umskz.
    wa_saida-waers = wa_bsad-waers.

    wa_saida-sgtxt = wa_bsad-sgtxt.
    wa_saida-kidno = wa_bsad-kidno.
    wa_saida-xref1 = wa_bsad-xref1.
    wa_saida-xref2 = wa_bsad-xref2.
    wa_saida-xref3 = wa_bsad-xref3.

    READ TABLE it_bsid INTO wa_bsid WITH KEY bukrs = wa_bsad-bukrs BINARY SEARCH.

    IF ( ( sy-subrc = 0 ) AND ( wa_bsid-rebzg EQ wa_bsad-belnr ) ).
      wa_saida-xvlrrec = wa_bsad-dmbtr - wa_bsid-dmbtr.
    ELSE.
      wa_saida-xvlrrec = wa_bsad-dmbtr.
    ENDIF.

    READ TABLE it_bsis INTO wa_bsis WITH KEY belnr = wa_bsad-augbl BINARY SEARCH.
    READ TABLE it_skb1 INTO wa_skb1 WITH KEY saknr = wa_bsis-hkont BINARY SEARCH.

    IF ( sy-subrc = 0 ).
      READ TABLE it_ska1 INTO wa_ska1 WITH KEY saknr = wa_skb1-saknr BINARY SEARCH.
      READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_ska1-saknr BINARY SEARCH.
      wa_saida-txt20 = wa_skat-txt20.
    ENDIF.

    IF ( wa_bsad-shkzg = 'H' ).
      wa_saida-dmbtr   =  wa_bsad-dmbtr * ( -1 ).
      wa_saida-dmbe2   =  wa_bsad-dmbe2 * ( -1 ).
      wa_saida-xvlrrec = 0.
      CLEAR: wa_saida-txt20.
    ELSE.
      wa_saida-dmbtr  =  wa_bsad-dmbtr.
      wa_saida-dmbe2  =  wa_bsad-dmbe2.
    ENDIF.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_bsad-vbel2 BINARY SEARCH.
    wa_saida-auart = wa_vbak-auart.

    READ TABLE it_zsdt0053 INTO wa_zsdt0053 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH .
    IF sy-subrc IS INITIAL .
      wa_saida-instrucao = wa_zsdt0053-instrucao.
    ELSE.
      READ TABLE it_zsdt0066 INTO wa_zsdt0066 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_saida-instrucao = wa_zsdt0066-instrucao.
      ENDIF.
    ENDIF.

    READ TABLE it_zsdt0045 INTO wa_zsdt0045 WITH KEY instrucao = wa_saida-instrucao BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_saida-contrato = wa_zsdt0045-contrato.
    ENDIF.

    READ TABLE it_zsdt0054 INTO wa_zsdt0054 WITH KEY adiant = wa_bsad-belnr.
    READ TABLE it_zsdt0051 INTO wa_zsdt0051 WITH KEY nro_sol_ov = wa_zsdt0054-nro_sol_ov.
    IF sy-subrc IS INITIAL.
      wa_saida-contrato = wa_zsdt0051-bstkd.
    ENDIF.


    READ TABLE it_bkpf INTO wa_bkpf WITH KEY belnr = wa_bsad-belnr BINARY SEARCH.
    wa_saida-usnam = wa_bkpf-usnam.
    wa_saida-tcode = wa_bkpf-tcode.
    wa_saida-bktxt = wa_bkpf-bktxt.

    READ TABLE it_vbrp INTO wa_vbrp WITH KEY vbeln = wa_bsad-vbeln BINARY SEARCH.
    wa_saida-matnr = wa_vbrp-matnr.
    wa_saida-arktx = wa_vbrp-arktx.

    IF ( wa_bsad-blart <> 'RV' ).
      wa_saida-fkimg = 0.
    ELSE.
      wa_saida-fkimg = wa_vbrp-fkimg.
    ENDIF.




    READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_bsad-vbel2 BINARY SEARCH.
    wa_saida-bstkd = wa_vbkd-bstkd.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsad-kunnr BINARY SEARCH.
    wa_saida-name1 = wa_kna1-name1.

    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_bsad-bukrs BINARY SEARCH.
    wa_saida-butxt = wa_t001-butxt.

    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_bsad-bupla BINARY SEARCH.
    wa_saida-name1_tw = wa_t001w-name1.

    READ TABLE it_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
    wa_saida-bezei = wa_tvakt-bezei.

*---> 05/07/2023 - Migração S4 - DL
    SORT it_j_1bnflin BY docnum refkey.
    SORT it_j_1bnfdoc BY docnum.
*<--- 05/07/2023 - Migração S4 - DL

    READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = wa_bkpf-awkey_aux BINARY SEARCH.

    READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.

    IF ( wa_j_1bnfdoc-nfe = 'X' ).
      wa_saida-xnf = wa_j_1bnfdoc-nfenum.
    ELSE.
      wa_saida-xnf = wa_j_1bnfdoc-nfnum.
    ENDIF.

    APPEND wa_saida TO it_saida.

    CLEAR: wa_saida, wa_zreg_exportacao, wa_zdoc_exp, wa_vbfa_m_j, wa_zsdt0053, wa_zsdt0066,
           wa_zsdt0045, wa_vbsad, wa_zsdt0054, wa_zsdt0051, wa_vbak, wa_bkpf,
           wa_vbrp, wa_vbkd, wa_kna1, wa_t001, wa_t001w, wa_tvakt,
           wa_bsid, wa_bsis, wa_skb1, wa_ska1, wa_skat, wa_bsad.

  ENDLOOP.

  LOOP AT it_saida INTO wa_saida.
    IF wa_saida-auart IN p_auart.
      CONTINUE.
    ELSE.
      DELETE it_saida INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  CLEAR: wa_saida.



ENDFORM.                    " Z_PARTIDAS_COMPENSADAS_SAIDA

**&---------------------------------------------------------------------*
**&      Form  F_SAIDA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM f_saida .
*
*ENDFORM.                    " F_SAIDA
**&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_imprime_dados .


*  IF it_saida[] IS INITIAL.
*    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
*                           'informados' .
*    STOP.
*  ENDIF.

  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_alv.

  IF p_matnr IS NOT INITIAL.
    DELETE it_saida WHERE matnr NOT IN p_matnr. "elimina materiais não selecionados
  ENDIF.

  gd_layout-no_input          = 'X'.
  gd_layout-colwidth_optimize = 'X'.
  gd_layout-info_fieldname    = 'LINE_COLOR'.
  gd_layout-coltab_fieldname  = 'COLOR_CELL'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      is_layout               = gd_layout
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = it_fcat[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
      it_sort                 = t_sort[]
    TABLES
      t_outtab                = it_saida.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho  USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_definir_eventos .

  PERFORM f_carregar_eventos USING: slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_alv_sort .

ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_alv .

  PERFORM alv_preenche_cat USING:

          'STATUS'       TEXT-053        '10'       ' '     ' '    ' ' , " Status
          'BUKRS'        TEXT-011        '10'       ' '     ' '    ' ' , " Empresa
          'BUTXT'        TEXT-012        '25'       ' '     ' '    ' ' , " Nome Empresa
          'KUNNR'        TEXT-013        '10'       ' '     'X'    ' ' , " Cliente
          'NAME1'        TEXT-014        '35'       ' '     ' '    ' ' , " Nome do cliente
          'HKONT'        TEXT-015        '10'       'X'     'X'    ' ' , " Conta
*          'SAKNR'        text-015        '10'       ' '     'X'    ' ' , " Conta
          'XBLNR'        TEXT-016        '16'       ' '     ' '    ' ' , " Referência
          'BKTXT'        TEXT-017        '25'       ' '     ' '    ' ' , " Txto.Cabeçalho
          'ZUONR'        TEXT-018        '18'       ' '     ' '    ' ' , " Atribuição
          'BLART'        TEXT-019        '6'        ' '     ' '    ' ' , " Tp.doc.
          'BELNR'        TEXT-020        '10'       'X'     ' '    ' ' , " Nº.Doc.
          'AUGBL'        TEXT-021        '10'       'X'     ' '    ' ' , " Doc.Comp.
          'VBELN'        TEXT-022        '10'       'X'     ' '    ' ' , " Doc.Faturamento
          'AUART'        TEXT-024        '6'        ' '     ' '    ' ' , " Tp.OV
          'BEZEI'        TEXT-025        '20'       ' '     ' '    ' ' , " Descr.TP.OV.
          'VBEL2'        TEXT-026        '10'       'X'     ' '    ' ' , " Ordem Venda
          'XNF'          TEXT-028        '15'       ' '     ' '    ' ' , " Nro.NF.
          'BLDAT'        TEXT-029        '10'       ' '     ' '    ' ' , " Dt.Docto
          'BUDAT'        TEXT-030        '10'       ' '     ' '    ' ' , " Dt.Lcto
          'AUGDT'        TEXT-031        '10'       ' '     ' '    ' ' , " Dt.comp.
          'ZFBDT_SUM'    TEXT-032        '10'       ' '     ' '    ' ' , " Dt.Vencto
          'ALERTA'       TEXT-033        '15'       ' '     ' '    ' ' , " St.Vc.
          'DIAS_ATR'     TEXT-057        '10'       ' '     ' '    ' ' , " Dias Atraso
          'ZTERM'        TEXT-034        '10'       ' '     ' '    ' ' , " Cond.Pgto
          'GSBER'        TEXT-035        '4'        ' '     ' '    ' ' , " Div.
          'BUPLA'        TEXT-036        '10'       ' '     ' '    ' ' , " Centro
          'NAME1_TW'     TEXT-037        '30'       ' '     ' '    ' ' , " Nome Centro
          'UMSKZ'        TEXT-038        '15'       ' '     ' '    ' ' , " Razão Especial
          'WAERS'        TEXT-039        '5'        ' '     ' '    ' ' , " Moeda
          'DMBTR'        TEXT-040        '13'       ' '     ' '    ' ' , " Valor R$
          'DMBE2'        TEXT-041        '13'       ' '     ' '    ' ' , " Valor US
          'XVLRREC'      TEXT-054        '13'       ' '     ' '    ' ' , " Valor de Credito
          'TXT20'        TEXT-055        '20'       ' '     ' '    ' ' , " Banco Credito
          'SGTXT'        TEXT-042        '50'       ' '     ' '    ' ' , " Texto Item
          'BSTKD'        TEXT-043        '35'       ' '     ' '    ' ' , " Texto Pedido (SD)
          'MATNR'        TEXT-044        '18'       ' '     'X'    ' ' , " Material
          'ARKTX'        TEXT-045        '40'       ' '     ' '    ' ' , " Descrição
          'FKIMG'        TEXT-046        '13'       ' '     ' '    ' ' , " Quantidade
          'KIDNO'        TEXT-047        '30'       ' '     ' '    ' ' , " Ref.Pagto
          'XREF1'        TEXT-048        '12'       ' '     ' '    ' ' , " Chave Ref.1
          'XREF2'        TEXT-049        '12'       ' '     ' '    ' ' , " Chave Ref.2
          'XREF3'        TEXT-050        '30'       ' '     ' '    ' ' , " Chave Ref.3
          'USNAM'        TEXT-051        '12'       ' '     ' '    ' ' , " Usuário
          'TCODE'        TEXT-052        '20'       ' '     ' '    ' ' , " Transação
          'NR_REGISTRO_EXPO' TEXT-058    ' '        ' '     ' '    ' ' , " RE
          'INSTRUCAO'    TEXT-059        ' '        ' '     ' '    ' ' , " Instrução
          'CONTRATO'     TEXT-060        ' '        ' '     ' '    ' ' , " Contrato
          'CPF_CNPJ'     TEXT-061        ' '        ' '     ' '    ' ' . " ID Fiscal Fornecedor / Cliente

ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_carregar_eventos   USING    name
                                   form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_preenche_cat USING    p_campo  TYPE c
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
*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.


ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM f_user_command USING l_ucomm  l_selfield TYPE slis_selfield.

  READ TABLE it_saida INTO wa_saida INDEX l_selfield-tabindex.

  CASE l_selfield-fieldname.

    WHEN 'BELNR'.
      SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
      SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
      SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN 'AUGBL'.
      SET PARAMETER ID 'BLN' FIELD wa_saida-augbl.
      SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN 'VBELN'.
      SET PARAMETER ID 'VF' FIELD wa_saida-vbeln.
      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

    WHEN 'VBEL2'.
      SET PARAMETER ID 'AUN' FIELD wa_saida-vbel2.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

  ENDCASE.
ENDFORM.                    "f_user_command
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_ABERTO_BSAD
*&---------------------------------------------------------------------*
FORM z_partidas_aberto_bsad .

  SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
       bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
       bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
       bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
  FROM bsad AS bsa
  INTO TABLE it_bsad
WHERE   bukrs IN p_bukrs
  AND   kunnr IN p_kunnr
  AND   augdt >=  p_budat
  AND   budat <=  p_budat
  AND   umskz EQ ''.

  CHECK NOT it_bsad[] IS INITIAL.

  SELECT kunnr name1
      FROM kna1
      INTO TABLE it_kna1
       FOR ALL ENTRIES IN it_bsad
     WHERE kunnr EQ it_bsad-kunnr.

  SELECT bukrs butxt
    FROM t001
    INTO TABLE it_t001
     FOR ALL ENTRIES IN it_bsad
   WHERE bukrs EQ it_bsad-bukrs.

  SELECT werks name1
    FROM t001w
    INTO TABLE it_t001w
     FOR ALL ENTRIES IN it_bsad
   WHERE werks EQ it_bsad-bupla.

*  loop at it_bsad_aux into wa_bsad_aux.
*
*    delete it_bsad_aux where augbl eq wa_bsad_aux-belnr
*    and augbl eq wa_bsad_aux-belnr.
*
*
*    if ( sy-subrc ne 0 ).
*      append wa_bsad_aux to it_bsad.
*    endif.
*
*  endloop.

ENDFORM.                    " Z_PARTIDAS_ABERTO_BSAD
*&---------------------------------------------------------------------*
*&      Form  Z_ABERTO_BSAD_SAIDA
*&---------------------------------------------------------------------*
FORM z_aberto_bsad_saida .

  SORT it_zsdt0045 BY instrucao.

  LOOP AT it_bsad INTO wa_bsad   WHERE augdt >= p_budat .

    wa_saida-status = icon_led_green.
    wa_saida-bukrs = wa_bsad-bukrs.
    wa_saida-kunnr = wa_bsad-kunnr.
    wa_saida-saknr = wa_bsad-saknr.
    wa_saida-xblnr = wa_bsad-xblnr.
    wa_saida-zuonr = wa_bsad-zuonr.
    wa_saida-blart = wa_bsad-blart.
    wa_saida-belnr = wa_bsad-belnr.
    wa_saida-augbl = wa_bsad-augbl.
    wa_saida-vbeln = wa_bsad-vbeln.
    wa_saida-vbel2 = wa_bsad-vbel2.
    wa_saida-bldat = wa_bsad-bldat.
    wa_saida-budat = wa_bsad-budat.
    wa_saida-augdt = wa_bsad-augdt.
    wa_saida-hkont = wa_bsad-hkont.

    READ TABLE it_vbfa_m_j INTO wa_vbfa_m_j WITH KEY vbeln = wa_bsad-vbeln.
    READ TABLE it_zdoc_exp INTO wa_zdoc_exp WITH KEY vbeln = wa_vbfa_m_j-vbelv.
    READ TABLE it_zreg_exportacao INTO wa_zreg_exportacao WITH KEY id_registro_expo = wa_zdoc_exp-id_registro_expo.
    IF sy-subrc IS INITIAL.
      wa_saida-nr_registro_expo = wa_zreg_exportacao-nr_registro_expo.
    ENDIF.

    " Calculo para data de vencimento.
    wa_saida-zfbdt_sum = wa_bsad-zfbdt + wa_bsad-zbd1t.

    IF ( wa_saida-zfbdt_sum < sy-datum ).
      wa_saida-alerta = icon_alert.
      "wa_saida-line_color = 'C600'.
      MOVE 'ZFBDT_SUM'    TO wa_color-fname.
      MOVE '6'         TO wa_color-color-col.
      MOVE '1'         TO wa_color-color-int.
      MOVE '0'         TO wa_color-color-inv.
      APPEND wa_color TO it_color.
      wa_saida-color_cell[] = it_color[].
      REFRESH it_color.
    ENDIF.
    wa_saida-dias_atr = sy-datum - wa_saida-zfbdt_sum.

    wa_saida-zterm = wa_bsad-zterm.
    wa_saida-gsber = wa_bsad-gsber.
    wa_saida-bupla = wa_bsad-bupla.
    wa_saida-umskz = wa_bsad-umskz.
    wa_saida-waers = wa_bsad-waers.

    wa_saida-sgtxt = wa_bsad-sgtxt.
    wa_saida-kidno = wa_bsad-kidno.
    wa_saida-xref1 = wa_bsad-xref1.
    wa_saida-xref2 = wa_bsad-xref2.
    wa_saida-xref3 = wa_bsad-xref3.

    READ TABLE it_bsid INTO wa_bsid WITH KEY bukrs = wa_bsad-bukrs BINARY SEARCH.

    IF ( ( sy-subrc = 0 ) AND ( wa_bsid-rebzg EQ wa_bsad-belnr ) ).
      wa_saida-xvlrrec = wa_bsad-dmbtr - wa_bsid-dmbtr.
    ELSE.
      wa_saida-xvlrrec = wa_bsad-dmbtr.
    ENDIF.

    READ TABLE it_bsis INTO wa_bsis WITH KEY belnr = wa_bsad-augbl BINARY SEARCH.
    READ TABLE it_skb1 INTO wa_skb1 WITH KEY saknr = wa_bsis-hkont BINARY SEARCH.

    IF ( sy-subrc = 0 ).
      READ TABLE it_ska1 INTO wa_ska1 WITH KEY saknr = wa_skb1-saknr BINARY SEARCH.
      READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_ska1-saknr BINARY SEARCH.
      wa_saida-txt20 = wa_skat-txt20.
    ENDIF.

    IF ( wa_bsad-shkzg = 'H' ).
      wa_saida-dmbtr   =  wa_bsad-dmbtr * ( -1 ).
      wa_saida-dmbe2   =  wa_bsad-dmbe2 * ( -1 ).
      wa_saida-xvlrrec = 0.
      CLEAR: wa_saida-txt20.
    ELSE.
      wa_saida-dmbtr  =  wa_bsad-dmbtr.
      wa_saida-dmbe2  =  wa_bsad-dmbe2.
    ENDIF.

    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_bsad-vbel2 BINARY SEARCH.
    wa_saida-auart = wa_vbak-auart.

    READ TABLE it_zsdt0053 INTO wa_zsdt0053 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH .
    IF sy-subrc IS INITIAL .
      wa_saida-instrucao = wa_zsdt0053-instrucao.
    ELSE.
      READ TABLE it_zsdt0066 INTO wa_zsdt0066 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      IF sy-subrc IS INITIAL .
        wa_saida-instrucao = wa_zsdt0066-instrucao.
      ENDIF.
    ENDIF.

    READ TABLE it_zsdt0045 INTO wa_zsdt0045 WITH KEY instrucao = wa_saida-instrucao BINARY SEARCH.
    IF sy-subrc IS INITIAL .
      wa_saida-contrato = wa_zsdt0045-contrato.
    ENDIF.

    READ TABLE it_zsdt0054 INTO wa_zsdt0054 WITH KEY adiant = wa_bsad-belnr.
    READ TABLE it_zsdt0051 INTO wa_zsdt0051 WITH KEY nro_sol_ov = wa_zsdt0054-nro_sol_ov.
    IF sy-subrc IS INITIAL .
      wa_saida-contrato = wa_zsdt0051-bstkd.
    ENDIF.

    READ TABLE it_bkpf INTO wa_bkpf WITH KEY belnr = wa_bsad-belnr BINARY SEARCH.
    wa_saida-usnam = wa_bkpf-usnam.
    wa_saida-tcode = wa_bkpf-tcode.
    wa_saida-bktxt = wa_bkpf-bktxt.

    READ TABLE it_vbrp INTO wa_vbrp WITH KEY vbeln = wa_bsad-vbeln BINARY SEARCH.
    wa_saida-matnr = wa_vbrp-matnr.
    wa_saida-arktx = wa_vbrp-arktx.

    IF ( wa_bsad-blart <> 'RV' ).
      wa_saida-fkimg = 0.
    ELSE.
      wa_saida-fkimg = wa_vbrp-fkimg.
    ENDIF.

    READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_bsad-vbel2 BINARY SEARCH.
    wa_saida-bstkd = wa_vbkd-bstkd.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsad-kunnr BINARY SEARCH.
    wa_saida-name1 = wa_kna1-name1.

    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_bsad-bukrs BINARY SEARCH.
    wa_saida-butxt = wa_t001-butxt.

    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_bsad-bupla BINARY SEARCH.
    wa_saida-name1_tw = wa_t001w-name1.

    READ TABLE it_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
    wa_saida-bezei = wa_tvakt-bezei.

    READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = wa_bkpf-awkey_aux BINARY SEARCH.

    READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.

    IF ( wa_j_1bnfdoc-nfe = 'X' ).
      wa_saida-xnf = wa_j_1bnfdoc-nfenum.
    ELSE.
      wa_saida-xnf = wa_j_1bnfdoc-nfnum.
    ENDIF.

    APPEND wa_saida TO it_saida.

    CLEAR: wa_saida, wa_zreg_exportacao, wa_zdoc_exp, wa_vbfa_m_j, wa_zsdt0053, wa_zsdt0066,
           wa_zsdt0045, wa_vbsad, wa_zsdt0054, wa_zsdt0051, wa_vbak,  wa_bkpf, wa_vbrp,
           wa_vbkd, wa_kna1, wa_t001, wa_t001w, wa_tvakt, wa_bsid, wa_bsis,
           wa_skb1, wa_ska1, wa_skat, wa_bsad.

  ENDLOOP.

  LOOP AT it_saida INTO wa_saida.
    IF wa_saida-auart IN p_auart.
      CONTINUE.
    ELSE.
      DELETE it_saida INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  CLEAR: wa_saida.
ENDFORM.                    " Z_ABERTO_BSAD_SAIDA
*&---------------------------------------------------------------------*
*&      Form  Z_PARTIDAS_ABERTO_BSAD_E
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_partidas_aberto_bsad_e .

  REFRESH it_bsad.

  SELECT bsa~bukrs bsa~kunnr bsa~blart bsa~augdt bsa~umskz bsa~vbel2 bsa~zuonr bsa~belnr
    bsa~budat bsa~bldat bsa~waers bsa~xblnr bsa~shkzg bsa~gsber bsa~dmbtr bsa~sgtxt
    bsa~saknr bsa~zfbdt bsa~zbd1t bsa~zterm bsa~vbeln bsa~dmbe2 bsa~xref1 bsa~xref2
    bsa~xref3 bsa~bupla bsa~augbl bsa~gjahr bsa~kidno bsa~hkont bsa~umsks
  FROM bsad AS bsa
  INTO TABLE it_bsad
WHERE   bukrs IN p_bukrs
  AND   kunnr IN p_kunnr
  AND   augdt >=  p_budat
  AND   budat <=  p_budat
*     AND   UMSKS NE ''
  AND   umskz NE ''.

  CHECK NOT it_bsad[] IS INITIAL.

  SELECT kunnr name1
      FROM kna1
      INTO TABLE it_kna1
       FOR ALL ENTRIES IN it_bsad
     WHERE kunnr EQ it_bsad-kunnr.

  SELECT bukrs butxt
    FROM t001
    INTO TABLE it_t001
     FOR ALL ENTRIES IN it_bsad
   WHERE bukrs EQ it_bsad-bukrs.

  SELECT werks name1
    FROM t001w
    INTO TABLE it_t001w
     FOR ALL ENTRIES IN it_bsad
   WHERE werks EQ it_bsad-bupla.
ENDFORM.                    " Z_PARTIDAS_ABERTO_BSAD_E
