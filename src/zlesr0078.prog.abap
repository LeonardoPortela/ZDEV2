*&--------------------------------------------------------------------&*
*&                         Grupo André Maggi
*&--------------------------------------------------------------------&*
*& Projeto..: Aquaviário
*& Data.....: 09.12.2013 07:34:21
*& Descrição: Rerport de Conferencia do Ct-e.
*&--------------------------------------------------------------------&*
REPORT  zlesr0078.
**********************************************************************
* TABELAS
**********************************************************************
TABLES: t001, t001w, j_1bnfdoc, makt, j_1bnflin.


**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_j_1bnflin,
         docnum       TYPE j_1bnflin-docnum, "Documento do SAP
         refkey       TYPE j_1bnflin-refkey, "Referência ao documento de origem
         cfop         TYPE j_1bnflin-cfop, "Código CFOP e extensão
         taxsit       TYPE j_1bnflin-taxsit, "Situação tributária ICMS
         taxsi2       TYPE j_1bnflin-taxsi2, "Situação fiscal IPI
         taxsi5       TYPE j_1bnflin-taxsi5, "Situação de imposto PIS
         taxsi4       TYPE j_1bnflin-taxsi4, "Situação de imposto COFINS
         taxsi3       TYPE j_1bnflin-taxsi3, "Situação de imposto ISS
         nfnet        TYPE j_1bnflin-nfnet, "Valor líquido incluindo impostos
         netwr        TYPE j_1bnflin-netwr, "Valor líquido
         refkey_vbeln TYPE vbfa-vbeln, "Refkey modificado.
       END OF ty_j_1bnflin,

       BEGIN OF ty_aquaviario,
         vbeln          TYPE vbak-vbeln,
         nr_ov          TYPE zlest0061-nr_ov, "Documento de vendas
         bukrs          TYPE zlest0061-bukrs, "Empresa
         werks          TYPE zlest0061-werks, "Centro
         ano_viagem     TYPE zlest0061-ano_viagem, "Ano da Viagem
         nr_viagem      TYPE zlest0061-nr_viagem, "Número da Viagem
         ck_anulado     TYPE zlest0061-ck_anulado, "Documento Anulado
         ort01_l        TYPE lfa1-ort01, "Local
         regio_l        TYPE lfa1-regio, "Região (país, estado, província, condado)
         name1_l        TYPE lfa1-name1,
         ort01_k        TYPE lfa1-ort01, "Local
         regio_k        TYPE lfa1-regio, "Região (país, estado, província, condado)
         name1_k        TYPE lfa1-name1,
         cod_material   TYPE zlest0063-cod_material, "Nº do material
         maktx          TYPE makt-maktx, "Texto breve de material
         kbetr          TYPE konv-kbetr, "Montante ou porcentagem da condição
         auart          TYPE vbak-auart, "Tipo de documento de vendas
         tax_dolar      TYPE konv-kbetr, "Taxa do Dolar do Aquaviario.
         tp_class       TYPE zlest0063-tp_class, "Classificação
         navio          TYPE zsdt0225-navio, "NAvio                    "*-CS2019001753-09.01.2023-#84936-JT-inicio
         local_operacao TYPE zsdt0225-local_operacao, "Local Operacao  "*-CS2019001753-09.01.2023-#84936-JT-inicio
       END OF ty_aquaviario,

** PBI - 66979 -  Inicio
       BEGIN OF ty_aquaviario_aux,
         refkey_vbeln TYPE vbfa-vbeln,
         vbelv        TYPE vbfa-vbelv, "Ordem de Venda
         auart        TYPE vbak-auart, "Tipo de documento de vendas
         maktx        TYPE makt-maktx, "Texto breve de material
         kbetr        TYPE konv-kbetr, "Montante ou porcentagem da condição
         tax_dolar    TYPE kursk, "Taxa do Dolar do Aquaviario.
         brgew        TYPE vbrp-brgew,
       END OF ty_aquaviario_aux,
** PBI - 66979 -  Fim

       BEGIN OF ty_rodoviario,
         tknum     TYPE vbak-tknum, "Nº transporte
         auart     TYPE vbak-auart, "Tipo de documento de vendas
         vbeln     TYPE vttp-vbeln, "Fornecimento
         vbelv     TYPE vbfa-vbelv, "Ordem de Venda
         parvw     TYPE vbpa-parvw, "Função do parceiro
         lifnr     TYPE lfa1-lifnr, "Nº conta do fornecedor
         kunnr     TYPE kna1-kunnr, "Nº cliente 1
         ort01     TYPE lfa1-lifnr, "Nº conta do fornecedor
         regio     TYPE lfa1-regio, "Região (país, estado, província, condado)
         ort01_k   TYPE kna1-ort01, "Local
         regio_k   TYPE kna1-regio, "Região (país, estado, província, condado)
         matnr     TYPE makt-matnr, "Nº do material
         maktx     TYPE makt-maktx, "Texto breve de material
         knumv     TYPE vfkp-knumv, "Nº condição do documento
         kbetr     TYPE konv-kbetr, "Montante ou porcentagem da condição
         j_1btxsdc TYPE vbap-j_1btxsdc, "Código de imposto SD
         waerk     TYPE vbak-waerk, "Moeda do documento SD


       END OF ty_rodoviario,

       BEGIN OF ty_saida,
         icon           TYPE c LENGTH 4,
         direct         TYPE j_1bnfdoc-direct, "Direção do movimento de mercadorias
         model          TYPE j_1bnfdoc-model, "Modelo da nota fiscal
         bukrs          TYPE j_1bnfdoc-bukrs, "Empresa
         pstdat         TYPE j_1bnfdoc-pstdat, "Data de lançamento
         docnum         TYPE j_1bnfdoc-docnum, "Nº documento
         branch         TYPE j_1bnfdoc-branch, "Local de negócios
         nfenum         TYPE j_1bnfdoc-nfenum, "Nº NF-e de nove posições
         series         TYPE j_1bnfdoc-series, "Série
         docstat        TYPE j_1bnfdoc-docstat, "NF-e: status do documento
         docdat         TYPE j_1bnfdoc-docdat, "Data do documento
         parid          TYPE j_1bnfdoc-parid, "Identificação do parceiro (cliente, fornecedor, loc.negócio)
         name1          TYPE j_1bnfdoc-name1,
         cgc_cpf        TYPE c LENGTH 14,
         stains         TYPE j_1bnfdoc-stains, "Nº identificação fiscal regional
         ort01          TYPE j_1bnfdoc-ort01, "Local
         regio          TYPE j_1bnfdoc-regio, "Região (país, estado, província, condado)
         brgew          TYPE j_1bnfdoc-brgew, "Peso bruto
         nfnet_unit     TYPE j_1bnfdoc-brgew, "Peso bruto
         nfnet          TYPE j_1bnflin-nfnet, "Valor líquido incluindo impostos
         crenam         TYPE j_1bnfdoc-crenam, "Nome do usuário
         cancel         TYPE c LENGTH 10,
         cfop           TYPE j_1bnflin-cfop, "Código CFOP e extensão
         taxsit         TYPE c LENGTH 2,
         taxsi2         TYPE j_1bnflin-taxsi2, "Situação fiscal IPI
         taxsi5         TYPE j_1bnflin-taxsi5, "Situação de imposto PIS
         taxsi4         TYPE j_1bnflin-taxsi4 , "Situação de imposto COFINS
         taxsi3         TYPE j_1bnflin-taxsi3 , "Situação de imposto ISS
         refkey         TYPE j_1bnflin-refkey, "Referência ao documento de origem
         base           TYPE j_1bnfstx-base, "Montante básico
         rate           TYPE j_1bnfstx-rate, "Taxa de imposto
         taxval_icms    TYPE j_1bnfstx-taxval, "Valor fiscal
         taxval_pis     TYPE j_1bnfstx-taxval, "Valor fiscal
         taxval_iss     TYPE j_1bnfstx-taxval, "Valor fiscal
         taxval_cofins  TYPE j_1bnfstx-taxval, "Valor fiscal
         othbas         TYPE j_1bnfstx-othbas, "Outro montante básico
         excbas         TYPE j_1bnfstx-excbas, "Montante básico excluído

         vbeln          TYPE vbfa-vbeln, "Documento de vendas e distribuição subseqüente
         vbelv          TYPE vbfa-vbelv, "Documento de vendas e distribuição precedente
         auart          TYPE vbak-auart, "Tipo de documento de vendas
         waerk          TYPE vbak-waerk, "Moeda do documento SD
         knumv          TYPE vbak-knumv, "Nº condição do documento
         tknum          TYPE vbak-tknum, "Nº transporte
         kschl          TYPE konv-kschl, "Tipo de condição
         kbetr          TYPE konv-kbetr, "Montante ou porcentagem da condição
         unit_dolar     TYPE j_1bnfdoc-brgew, "Peso bruto
         tx_dolar       TYPE kursk, "Taxa de câmbio para determinação do preço
         kbetr_ped      TYPE konv-kbetr, "Montante ou porcentagem da condição
         j_1btxsdc      TYPE vbap-j_1btxsdc, "Código de imposto SD
         ano_viagem     TYPE zlest0061-ano_viagem, "Frete Aquaviário - Ordem de Venda
         nr_viagem      TYPE zlest0061-nr_viagem, "Frete Aquaviário - Ordem de Venda
         ck_anulado     TYPE zlest0061-ck_anulado, "Anulado
         po_embarque    TYPE zlest0056-po_embarque, "Frete Aquaviário - Viagem
         po_destino     TYPE zlest0056-po_destino, "Frete Aquaviário - Viagem
         matnr          TYPE makt-matnr, "Textos breves de material
         maktx          TYPE makt-maktx, "Textos breves de material

         ort01_coleta   TYPE lfa1-ort01, "Local
         regio_coleta   TYPE lfa1-regio, "Região (país, estado, província, condado)
         name1_coleta   TYPE lfa1-name1, "Região (país, estado, província, condado)

         ort01_entrega  TYPE kna1-ort01, "Local
         regio_entrega  TYPE kna1-regio, "Região (país, estado, província, condado)
         name1_entrega  TYPE kna1-name1, "Região (país, estado, província, condado)

         tp_class       TYPE zlest0063-tp_class, "Classificação
         estilo_cell    TYPE lvc_t_scol, "Estilo na Celula do ALV

         navio          TYPE zsdt0225-navio, "NAvio                    "*-CS2019001753-09.01.2023-#84936-JT-inicio
         local_operacao TYPE zsdt0225-local_operacao, "Local Operacao  "*-CS2019001753-09.01.2023-#84936-JT-inicio

       END OF ty_saida.

**********************************************************************
* TABELAS INTERNAS
**********************************************************************
DATA: gt_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc, "Cabeçalho da nota fiscal
      gt_j_1bnfe_active TYPE TABLE OF j_1bnfe_active, "Electronic Nota Fiscal: Actual Status
      gt_j_1bnflin      TYPE TABLE OF ty_j_1bnflin, "Estrutura da tabela de Itens.
      gt_j_1bnfstx      TYPE TABLE OF j_1bnfstx, "Nota fiscal: imposto por item
      gt_vbfa           TYPE TABLE OF vbfa, "Fluxo de documentos de vendas e distribuição
      gt_vbak           TYPE TABLE OF vbak, "Documento de vendas: dados de cabeçalho
      gt_konv           TYPE TABLE OF konv, "Condições (dados de operação) AQUAVIARIO
      gt_konv_r         TYPE TABLE OF konv, "Condições (dados de operação) RODOVIARIO
      gt_vbap           TYPE TABLE OF vbap, "Documento de vendas: dados de item
      gt_vbap_r         TYPE TABLE OF vbap, "Documento de vendas: dados de item
      gt_zlest0061      TYPE TABLE OF zlest0061, "Frete Aquaviário - Ordem de Venda
      gt_zlest0056      TYPE TABLE OF zlest0056, "Frete Aquaviário - Viagem
      gt_zlest0063      TYPE TABLE OF zlest0063, "Frete Aquáviário - Comboio
      gt_zsdt0225       TYPE TABLE OF zsdt0225,
      gt_lfa1           TYPE TABLE OF lfa1, "Mestre de fornecedores (parte geral)
      gt_kna1           TYPE TABLE OF kna1, "Mestre de clientes (parte geral)
      gt_vttp           TYPE TABLE OF vttp, "Item de transporte
      gt_vbpa           TYPE TABLE OF vbpa, "Documento SD: parceiro
      gt_lips           TYPE TABLE OF lips, "Documento SD: fornecimento: dados de item
      gt_makt           TYPE TABLE OF makt, "Textos breves de material
      gt_vfkp           TYPE TABLE OF vfkp, "Custos de frete: dados do item
      gt_tcurr          TYPE TABLE OF tcurr, "Taxas de câmbio
      gt_aquaviario     TYPE TABLE OF ty_aquaviario, "Estrutura para o Aquaviario.
      gt_aquaviario_aux TYPE TABLE OF ty_aquaviario_aux, "Estrutura para o Aquaviario.
      gt_rodoviario     TYPE TABLE OF ty_rodoviario, "Estrutura para o Rodoviario.
      gt_saida          TYPE TABLE OF ty_saida. "Estrutura para Saída do ALV
**********************************************************************
* WORK AREA
**********************************************************************
DATA: gw_j_1bnfdoc      TYPE j_1bnfdoc, "Cabeçalho da nota fiscal
      gw_j_1bnfe_active TYPE j_1bnfe_active, "Electronic Nota Fiscal: Actual Status
      gw_j_1bnflin      TYPE ty_j_1bnflin, "Estrutura da tabela de Itens.
      gw_j_1bnfstx      TYPE j_1bnfstx, "Nota fiscal: imposto por item
      gw_vbfa           TYPE vbfa, "Fluxo de documentos de vendas e distribuição
      gw_vbak           TYPE vbak, "Documento de vendas: dados de cabeçalho
      gw_konv           TYPE konv, "Condições (dados de operação) AQUAVIARIO
      gw_konv_r         TYPE konv, "Condições (dados de operação) RODOVIARIO
      gw_vbap           TYPE vbap, "Documento de vendas: dados de item
      gw_vbap_r         TYPE vbap, "Documento de vendas: dados de item
      gw_zlest0061      TYPE zlest0061, "Frete Aquaviário - Ordem de Venda
      gw_zlest0056      TYPE zlest0056, "Frete Aquaviário - Viagem
      gw_zlest0063      TYPE zlest0063, "Frete Aquáviário - Comboio
      gw_zsdt0225       TYPE zsdt0225,
      gw_lfa1           TYPE lfa1, "Mestre de fornecedores (parte geral)
      gw_kna1           TYPE kna1, "Mestre de clientes (parte geral)
      gw_vttp           TYPE vttp, "Item de transporte
      gw_vbpa           TYPE vbpa, "Documento SD: parceiro
      gw_lips           TYPE lips, "Documento SD: fornecimento: dados de item
      gw_makt           TYPE makt, "Textos breves de material
      gw_vfkp           TYPE vfkp, "Custos de frete: dados do item
      gw_tcurr          TYPE tcurr, "Taxas de câmbio
      gw_aquaviario     TYPE ty_aquaviario, "Estrutura para o Aquaviario.
      gw_aquaviario_aux TYPE ty_aquaviario_aux, "Estrutura para o Aquaviario.
      gw_rodoviario     TYPE ty_rodoviario, "Estrutura para o Rodoviario.
      gw_saida          TYPE ty_saida. "Estrutura para saída do ALV.

**********************************************************************
* CLASSES
**********************************************************************
DATA: obj_util TYPE REF TO zcl_util.
**********************************************************************
* Estrutura para o RANGE
**********************************************************************
DATA : BEGIN OF range_status_sap OCCURS 0,
         sign(1),
         option(2),
         low(18),
         high(18),
       END OF range_status_sap .


DATA : BEGIN OF range_status_cte OCCURS 0,
         sign(1),
         option(2),
         low(18),
         high(18),
       END OF range_status_cte .

DATA : BEGIN OF range_status_canc OCCURS 0,
         sign(1),
         option(2),
         low(18),
         high(18),
       END OF range_status_canc .


DATA: p_form TYPE RANGE OF j_1bnfdoc-form,
      w_form LIKE LINE OF p_form.



**********************************************************************
* Estrutura e Classes para o ALV
**********************************************************************
DATA: gt_fcatalog       TYPE lvc_t_fcat, "Catálogo de campos.
      gw_fcatalog       TYPE lvc_s_fcat, "Controle VLA: catálogo de campos
      obj_gui_container TYPE REF TO cl_gui_custom_container, "Classe para chamada do Container.
      obj_alv_grid      TYPE REF TO cl_gui_alv_grid. "Classe referente a grid do ALV.

**********************************************************************
* variaveis
**********************************************************************
DATA: gs_variant TYPE disvariant.


**********************************************************************
* Tela de Seleção
**********************************************************************
*****************
*  Descrição: Bloco Rerefente aos dados do cabeçalho.
*  Data: 09.12.2013 09:12:48
*****************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs  FOR t001-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY, "Empresa
                  p_werks  FOR t001w-werks OBLIGATORY, "Filial
                  p_pstdat FOR j_1bnfdoc-pstdat NO-EXTENSION OBLIGATORY, "Data de Lançamento
                  p_docdat FOR j_1bnfdoc-docdat NO-EXTENSION. "Data do Documento
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS: p_serv RADIOBUTTON GROUP g1,
              p_conh RADIOBUTTON GROUP g1,
              p_tds  RADIOBUTTON GROUP g1.

  SELECTION-SCREEN SKIP 1.

  SELECT-OPTIONS: p_parid   FOR j_1bnfdoc-parid, "Cliente/Fornecedor
                  p_docnum  FOR j_1bnfdoc-docnum NO-EXTENSION, "Número do Documento
                  p_nfenum  FOR j_1bnfdoc-nfenum NO-EXTENSION, "Número do Conhecimento
                  p_cfop    FOR j_1bnflin-cfop, "CFOP
                  p_crenam  FOR j_1bnfdoc-crenam NO INTERVALS NO-EXTENSION, "Usuário
                  p_matnr   FOR  makt-matnr. "Material
SELECTION-SCREEN: END OF BLOCK b2.

*****************
*  Descrição: Bloco Referente aos Status no SAP
*  Data: 09.12.2013 09:13:04
*****************
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: r_ativas RADIOBUTTON GROUP r1 DEFAULT 'X', "Status SAP (ATIVA)
              r_nativa RADIOBUTTON GROUP r1, "Status SAP (Não Ativa).
              r_todas  RADIOBUTTON GROUP r1. "Status SAP (Todos os Status Ativas/Canceladas).
SELECTION-SCREEN: END OF BLOCK b3.

*****************
*  Descrição: Status do Conhecimento Eletronico no SAP.
*  Data: 09.12.2013 09:13:25
*****************
SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS:
    p_inicia RADIOBUTTON GROUP r2, "Inicializada
    p_autor  RADIOBUTTON GROUP r2 DEFAULT 'X', "Autorizada na SEFAZ
    p_rejei  RADIOBUTTON GROUP r2, "Rejeitada na SEFAZ
    p_deneg  RADIOBUTTON GROUP r2, "Denegada/Recusada na SEFAZ
    p_cancel RADIOBUTTON GROUP r2. "Cancelada na SEFAZ
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  PARAMETERS: p_layout LIKE disvariant-variant." layout
SELECTION-SCREEN: END OF BLOCK b5.

**********************************************************************
* Montar F4 do Layout
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.

  CLEAR: gs_variant.

  gs_variant-report = sy-repid.

  IF NOT ( p_layout IS INITIAL ).
    gs_variant-variant = p_layout.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = gs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = gs_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_layout = gs_variant-variant.
  ENDIF.

START-OF-SELECTION.
**********************************************************************
* Seleção dos Dados
**********************************************************************
  PERFORM: range_status_conhecimento,
           selecionar_dados.
*&---------------------------------------------------------------------*
*&      Form  RANGE_STATUS_NOTA
*&---------------------------------------------------------------------*
*       Montar o RANGE do Status do Conhecimento
*----------------------------------------------------------------------*
FORM range_status_conhecimento.

*****************
*  Descrição: Montar o RANGE do STATUS SAP
*  Data: 09.12.2013 13:25:20
* Atribuitr valores de RANGE (intervalo) na estrutura RANGE_STATUS_SAP
* SENDO: P_ATIVAS = Conhecimentos Ativos (Status diferente de Cancelado)
*        P_NATIVA = Conhecimentos não ativos. (Status Cancelado = 'X').
*        P_TODAS  = Todos os Conhecimentos (ATIVOS/Não Ativos).
*****************

  REFRESH: range_status_sap.

  "Caso o status estiver marcado com ATIVAS atribuir
  "o range de espaço, para selecionar somente os conhecimentos com status diferente de cancelado = 'X'.
  IF ( r_ativas EQ 'X' ).

    range_status_sap-sign   = 'I'.
    range_status_sap-option = 'BT'.
    range_status_sap-low    = space.
    range_status_sap-high   = space.
    APPEND range_status_sap.


    "Caso o status Não Ativas estiver marcado atribuir os valores
    "no range de X, tanto para LOW quanto para HIGH e selecionar somente os conhecimentos cancelados = 'X'.
  ELSEIF ( r_nativa EQ 'X' ).

    range_status_sap-sign   = 'I'.
    range_status_sap-option = 'BT'.
    range_status_sap-low    = 'X'.
    range_status_sap-high   = 'X'.
    APPEND range_status_sap.

    "Caso o status seja TODAS estiver marcado atribuir os valores
    "tanto de notas ativas onde o cancelado é diferente de 'X'(ativo).
    "e também onde o status é igual a 'X' (cancelado)
  ELSE.

    range_status_sap-sign   = 'I'.
    range_status_sap-option = 'BT'.
    range_status_sap-low    = space.
    range_status_sap-high   = space.
    APPEND range_status_sap.

    range_status_sap-sign   = 'I'.
    range_status_sap-option = 'BT'.
    range_status_sap-low    = 'X'.
    range_status_sap-high   = 'X'.
    APPEND range_status_sap.

  ENDIF.

  REFRESH: range_status_cte.
  IF ( p_inicia EQ 'X' ).

    range_status_cte-sign   = 'I'.
    range_status_cte-option = 'BT'.
    range_status_cte-low    = space.
    range_status_cte-high   = space.
    APPEND range_status_cte.


  ELSEIF ( p_autor EQ 'X').

    range_status_cte-sign   = 'I'.
    range_status_cte-option = 'BT'.
    range_status_cte-low    = '1'.
    range_status_cte-high   = '1'.
    APPEND range_status_cte.

  ELSEIF ( p_rejei EQ 'X').

    range_status_cte-sign   = 'I'.
    range_status_cte-option = 'BT'.
    range_status_cte-low    = '2'.
    range_status_cte-high   = '2'.
    APPEND range_status_cte.

  ELSEIF ( p_deneg EQ 'X') .

    range_status_cte-sign   = 'I'.
    range_status_cte-option = 'BT'.
    range_status_cte-low    = '3'.
    range_status_cte-high   = '3'.
    APPEND range_status_cte.

  ENDIF.


  REFRESH: range_status_canc.
  IF ( p_cancel EQ 'X').
    range_status_canc-sign   = 'I'.
    range_status_canc-option = 'BT'.
    range_status_canc-low    = 'X'.
    range_status_canc-high   = 'X'.
    APPEND range_status_canc.
  ENDIF.



  IF p_serv EQ 'X'.
    w_form-sign    = 'I'.
    w_form-option  = 'EQ'.
    w_form-low     = 'NF01'.
    APPEND w_form TO p_form.
    CLEAR  w_form.
  ELSEIF p_conh EQ 'X'.
    w_form-sign    = 'I'.
    w_form-option  = 'EQ'.
    w_form-low     = 'NF57'.
    APPEND w_form TO p_form.
    CLEAR  w_form.
  ELSEIF p_tds EQ 'X'.
    w_form-sign    = 'I'.
    w_form-option  = 'EQ'.
    w_form-low     = 'NF01'.
    APPEND w_form TO p_form.
    w_form-low     = 'NF57'.
    APPEND w_form TO p_form.
    CLEAR  w_form.
  ENDIF.

ENDFORM.                    " RANGE_STATUS_NOTA
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM selecionar_dados .

  "Limpar todas as tebelas internas e work areas.
  PERFORM: limpar_tabelas_wa.

*****************
* Descrição: Selecionar Dados do Cabeçalho da nota fiscal
* Data: 09.12.2013 08:54:33
* Parametros: DIRECT   = 2 (Direção - Saída).
*             MODEL    = 57 (Conhecimento de Transporte Eletronico).
*             P_BUKRS  = Empresa Informada pelo usuário
*             P_WERKS  = Filial Emitente
*             p_BRANCH = Filial Informada pelo Usuário
*             P_PSTDAT = Data do Lançamento informada pelo usuário.
*             P_DOCDAT = Data do Documento
*             P_PARID  = Código do Cliente
*             P_DOCNUM =  Número do Documento SAP
*             P_NFENUM =  Número do Conhecimento Eletronico
*             P_CRENAM =  Usuário.
*             CANCEL   = Estrutura de Status
*****************
  SELECT * FROM j_1bnfdoc
    INTO TABLE gt_j_1bnfdoc
  WHERE direct EQ '2'       "Direção Saída
    AND model  IN ('01', '57')      "Modelo do Documento
    AND doctyp IN ('1','4')      "Tipo de Documento
    AND bukrs  IN p_bukrs   "Empresa
    AND branch IN p_werks   "Filial.
    AND pstdat IN p_pstdat  "Data de Lançamento
    AND docdat IN p_docdat  "Data do Documento
    AND parid  IN p_parid   "Código do Cliente/Fornecedor
    AND docnum IN p_docnum  "Número do Documento SAP
    AND nfenum IN p_nfenum  "Número do Conhecimento Eletronico
    AND crenam IN p_crenam "Usuário
    AND form   IN p_form
    AND cancel IN range_status_sap. "Status SAP

  CHECK NOT gt_j_1bnfdoc[] IS INITIAL. "Caso tenha dados na tabela do cabeçolho continuar a busca dos dados.

*****************
*  Descrição: Electronic Nota Fiscal: Actual Status
*  Data: 10.12.2013 10:05:33
* Selecionar o status atual de cada conhecimento encontrado no cabeçalho.
*****************

*  SELECT * FROM j_1bnfe_active
*    INTO TABLE gt_j_1bnfe_active
*    FOR ALL ENTRIES IN gt_j_1bnfdoc
*  WHERE docnum EQ gt_j_1bnfdoc-docnum
*    AND docsta  IN range_status_cte
*    AND cancel  IN range_status_canc.


*****************
*  Descrição: Partidas individuais da nota fiscal
*  Data: 09.12.2013 09:33:31
* Recuperar informações de todos os itens dos documentos que foram selecionados
* na tabela do cabeçalho do documento fiscal.
* Paramaetro: GT_J_1BNFDOC-DOCNUM = Documento SAP do Cabeçalho
*             P_CFOP = CFOP
* Retonar: DOCNUM - Dcocumento SAP
*          REFKEY - Referência ao documento de origem
*****************

  SELECT docnum refkey cfop taxsit taxsi2 taxsi5 taxsi4 taxsi3 nfnet netwr FROM j_1bnflin
    INTO TABLE gt_j_1bnflin
    FOR ALL ENTRIES IN gt_j_1bnfdoc
  WHERE docnum EQ gt_j_1bnfdoc-docnum "Número do Documento SAP Selecionado no cabeçalho.
    AND cfop   IN p_cfop. "CFOP Informadao pelo o usuário.

  "Tratamento para transformar o REFKEY da J_1BNFLIN (ITENS) do mesmo tipo do VBELN (VBFA).
  "Só executar caso tenha encontrado alguma informação na J_1BNFLIN.
  CHECK NOT gt_j_1bnflin[] IS INITIAL.

  FIELD-SYMBOLS: <fs_j_1bnflin> TYPE ty_j_1bnflin.
  LOOP AT gt_j_1bnflin ASSIGNING <fs_j_1bnflin>.
    <fs_j_1bnflin>-docnum   = <fs_j_1bnflin>-docnum.
    <fs_j_1bnflin>-cfop     = <fs_j_1bnflin>-cfop.
    <fs_j_1bnflin>-taxsit   = <fs_j_1bnflin>-taxsit.
    <fs_j_1bnflin>-taxsi2   = <fs_j_1bnflin>-taxsi2.
    <fs_j_1bnflin>-taxsi5   = <fs_j_1bnflin>-taxsi5.
    <fs_j_1bnflin>-taxsi4   = <fs_j_1bnflin>-taxsi4.
    <fs_j_1bnflin>-nfnet    = <fs_j_1bnflin>-nfnet.
    <fs_j_1bnflin>-netwr    = <fs_j_1bnflin>-netwr.
    <fs_j_1bnflin>-refkey_vbeln = <fs_j_1bnflin>-refkey.
  ENDLOOP.
  UNASSIGN <fs_j_1bnflin>.

*****************
*  Descrição: Nota fiscal: imposto por item
*  Data: 09.12.2013 09:36:36
* Recuperar informações de todos os impostos referente a cada item selecionado acima.
* Parametros: GT_J_1BNFLIN-DOCNUM = Docnum SAP dos Itens.
*             TAXTYP = (ICM3-IPSN-ICON).
*                       ICM3 = ICMS DE SD
*                       IPSN = PIS NVV NORMAL TAX
*                       ICON = COFINS NVV NORMAL TAX
*****************

  SELECT * FROM j_1bnfstx
    INTO TABLE gt_j_1bnfstx
    FOR ALL ENTRIES IN gt_j_1bnflin
  WHERE docnum EQ gt_j_1bnflin-docnum "Número do Documento SAP
    AND taxtyp IN ('ICM3','IPSN','ICON','ISSE'). "ICMS-PIS-COFINS

*****************
*  Descrição: Fluxo de documentos de vendas e distribuição
*  Data: 09.12.2013 10:05:06
* Parametros: REFKEY_VBELN - Refkey Modificado da J_1BNFLIN (ITENS).
*****************

  SELECT * FROM vbfa
    INTO TABLE gt_vbfa
    FOR ALL ENTRIES IN gt_j_1bnflin
  WHERE vbeln   EQ gt_j_1bnflin-refkey_vbeln
    AND vbtyp_n EQ 'M'.

*****************
*  Descrição: Documento de vendas: dados de cabeçalho
*  Data: 09.12.2013 10:22:42
* Parametros: GT_VBFA-VBELV = Documento de vendas e distribuição precedente
*****************

  SELECT * FROM vbak
    INTO TABLE gt_vbak
    FOR ALL ENTRIES IN gt_vbfa
  WHERE vbeln EQ gt_vbfa-vbelv.

*****************
*  Descrição: Condições (dados de operação)
*  Data: 09.12.2013 10:31:39
* Parametro: KNUMV  = Nº condição do documento.
* Condição somente para Aquaviário.
*****************

  SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @gt_vbak WHERE knumv EQ @gt_vbak-knumv AND kschl EQ 'ICMI' INTO CORRESPONDING FIELDS OF TABLE @gt_konv .

*****************
*  Descrição: Documento de vendas: dados de item
*  Data: 09.12.2013 10:33:23
*  Parametro: VBELN = Documento de vendas e distribuição subseqüente
*****************

  SELECT * FROM vbap
    INTO TABLE gt_vbap
    FOR ALL ENTRIES IN gt_vbfa
  WHERE vbeln EQ gt_vbfa-vbelv.

*****************
*  Descrição: Recuperar Informações do Aquaviário/Ferroviario referente as seleções realizadas acima.
*****************

  "Se a VBAK (Documento de vendas: dados de cabeçalho)
  "Retornar informações atender a condição abaixo.
  IF NOT ( gt_vbak[] IS INITIAL ).

*****************
*  Descrição: Frete Aquaviário - Ordem de Venda
*  Data: 09.12.2013 10:42:26
* Parametros: NR_OV = GT_VBAK-VBELN (Documento de vendas).
*****************

    SELECT * FROM zlest0061
      INTO TABLE gt_zlest0061
      FOR ALL ENTRIES IN gt_vbak
    WHERE nr_ov EQ gt_vbak-vbeln.

    "Se a tabela interna GT_ZLEST0061 conter dados, continuar o processo.
    IF ( sy-subrc EQ 0 ).

*****************
*  Descrição: Frete Aquaviário - Viagem
*  Data: 09.12.2013 10:47:52
*  Parametros:  BUKRS      = GT_ZLEST0061-BUKRS  (Empresa).
*               WERKS      = GT_ZLEST0061-WERKS  (Centro).
*               NR_VIAGEM  = GT_ZLEST0061-NR_VIAGEM (Número da Viagem).
*               ANO_VIAGEM = GT_ZLEST0061-ANO_VIAGEM (Ano da Viagem).
*
*****************

      SELECT * FROM zlest0056
        INTO TABLE gt_zlest0056
        FOR ALL ENTRIES IN gt_zlest0061
     WHERE bukrs      EQ gt_zlest0061-bukrs
       AND werks      EQ gt_zlest0061-werks
       AND nr_viagem  EQ gt_zlest0061-nr_viagem
       AND ano_viagem EQ gt_zlest0061-ano_viagem.

*****************
*  Descrição: Frete Aquáviário - Comboio
*  Data: 09.12.2013 14:41:04
* Parametros:  BUKRS      =  GT_ZLEST0061-BUKRS (Empresa).
*              WERKS      =  GT_ZLEST0061-WERKS (Centro).
*              ANO_VIAGEM =  GT_ZLEST0061-ANO_VIAGEM (Ano Viagem).
*              NR_VIAGEM  =  GT_ZLEST0061-NR_VIAGEM (Número Viagem).
*              NOME_EMB   =  GT_ZLEST0061-NOME_EMB. (Nome da Embarcação).
*****************

      SELECT * FROM zlest0063
         INTO TABLE gt_zlest0063
        FOR ALL ENTRIES IN gt_zlest0061
      WHERE bukrs       EQ gt_zlest0061-bukrs
        AND werks       EQ gt_zlest0061-werks
        AND ano_viagem  EQ gt_zlest0061-ano_viagem
        AND nr_viagem   EQ gt_zlest0061-nr_viagem
        AND nome_emb    EQ gt_zlest0061-nome_emb.


*****************
*  Descrição: Textos breves de material
*  Data: 09.12.2013 14:54:53
*****************

      SELECT * FROM makt
        INTO TABLE gt_makt
         FOR ALL ENTRIES IN gt_zlest0063
      WHERE matnr EQ gt_zlest0063-cod_material
        AND spras EQ sy-langu.


*****************
*   Descrição: Mestre de fornecedores (parte geral)
*   Data: 09.12.2013 10:51:56
*  Parametros: LIFNR = GT_ZLEST0056-PO_EMBARQUE (Porto de Embarque).
*****************

      SELECT * FROM lfa1
        INTO TABLE gt_lfa1
        FOR ALL ENTRIES IN gt_zlest0056
      WHERE lifnr EQ gt_zlest0056-po_embarque.

*****************
*  Descrição: Mestre de clientes (parte geral)
*  Data: 09.12.2013 10:51:57
* Parametros: KUNNR = GT_ZLEST0056-PO_DESTINO (Porto de Destino).
*****************

      SELECT * FROM kna1
        INTO TABLE gt_kna1
        FOR ALL ENTRIES IN gt_zlest0056
      WHERE kunnr EQ gt_zlest0056-po_destino.

      "*****************
      "*  Descrição: Montar em uma Estrutura todas as Informações somente do Aquaviário.
      "*  Data: 09.12.2013 13:37:23
      "* ZTAG = Transporte Aquaviario em Grãos
      "* ZTAF = Transporte Aquaviario Fertilizante.
      "* ZTAB = Transporte Aquaviario Biomas.
      "* ZTAM = Transporte Aquaviario Minerio.
      "*****************
      LOOP AT gt_vbak INTO gw_vbak WHERE auart EQ 'ZTAG'
                                      OR auart EQ 'ZTAF'
                                      OR auart EQ 'ZTAB'
                                      OR auart EQ 'ZTAM'
                                      OR auart EQ 'ZTAA'. "BUG 168688 - MMSILVA - 03.03.2025

        READ TABLE gt_zlest0061  INTO gw_zlest0061 WITH KEY nr_ov = gw_vbak-vbeln.

        IF ( sy-subrc EQ 0 ).

          gw_aquaviario-auart = gw_vbak-auart.

          READ TABLE gt_konv INTO gw_konv WITH KEY knumv = gw_vbak-knumv.
          gw_aquaviario-kbetr = gw_konv-kbetr.

          gw_aquaviario-nr_ov      = gw_zlest0061-nr_ov.
          gw_aquaviario-vbeln      = gw_vbak-vbeln.
          gw_aquaviario-bukrs      = gw_zlest0061-bukrs.
          gw_aquaviario-werks      = gw_zlest0061-werks.
          gw_aquaviario-ano_viagem = gw_zlest0061-ano_viagem.
          gw_aquaviario-nr_viagem  = gw_zlest0061-nr_viagem.
          gw_aquaviario-ck_anulado = gw_zlest0061-ck_anulado.

          READ TABLE gt_zlest0056 INTO gw_zlest0056 WITH KEY  bukrs      = gw_zlest0061-bukrs
                                                              werks      = gw_zlest0061-werks
                                                              ano_viagem = gw_zlest0061-ano_viagem
                                                              nr_viagem  = gw_zlest0061-nr_viagem.

          READ TABLE gt_lfa1 INTO gw_lfa1 WITH KEY lifnr = gw_zlest0056-po_embarque.

          gw_aquaviario-ort01_l = gw_lfa1-ort01.
          gw_aquaviario-regio_l = gw_lfa1-regio.
          gw_aquaviario-name1_l = gw_lfa1-name1.

          READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_zlest0056-po_destino.

          gw_aquaviario-ort01_k = gw_kna1-ort01.
          gw_aquaviario-regio_k = gw_kna1-regio.
          gw_aquaviario-name1_k = gw_kna1-name1.

          READ TABLE gt_zlest0063 INTO gw_zlest0063 WITH KEY bukrs      = gw_zlest0061-bukrs
                                                             werks      = gw_zlest0061-werks
                                                             ano_viagem = gw_zlest0061-ano_viagem
                                                             nr_viagem  = gw_zlest0061-nr_viagem
                                                             nome_emb   = gw_zlest0061-nome_emb.

          gw_aquaviario-cod_material = gw_zlest0063-cod_material.
          gw_aquaviario-tp_class     = gw_zlest0063-tp_class.

          READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_zlest0063-cod_material.

          gw_aquaviario-maktx = gw_makt-maktx.

          APPEND gw_aquaviario TO gt_aquaviario.

        ENDIF.

        CLEAR: gw_aquaviario, gw_vbak, gw_zlest0061, gw_zlest0056, gw_zlest0063, gw_lfa1, gw_kna1, gw_makt.
      ENDLOOP.
    ENDIF.


    SELECT * FROM zsdt0225 INTO TABLE gt_zsdt0225
      FOR ALL ENTRIES IN gt_vbak
      WHERE nr_ov EQ gt_vbak-vbeln.

    IF gt_zsdt0225[] IS NOT INITIAL. " PBI - 66079 - inicio. ( ADD  if gt_zsdt0225 is not initial. Lia a tabela sem dados)

      SELECT * FROM makt
        INTO TABLE gt_makt
         FOR ALL ENTRIES IN gt_zsdt0225
      WHERE matnr EQ gt_zsdt0225-cod_material
        AND spras EQ sy-langu.

      SELECT * FROM lfa1
        INTO TABLE gt_lfa1
        FOR ALL ENTRIES IN gt_zsdt0225
      WHERE lifnr EQ gt_zsdt0225-po_embarque.

      SELECT * FROM kna1
        INTO TABLE gt_kna1
        FOR ALL ENTRIES IN gt_zsdt0225
      WHERE kunnr EQ gt_zsdt0225-po_destino.

      LOOP AT gt_vbak INTO gw_vbak WHERE auart EQ 'ZTAG'
                                      OR auart EQ 'ZTRF'
                                      OR auart EQ 'ZTRG'
                                      OR auart EQ 'ZTAF'
                                      OR auart EQ 'ZTAB'
                                      OR auart EQ 'ZTAM'
                                      OR auart EQ 'ZTRG'
                                      OR auart EQ 'ZTAA'  "BUG 168688 - MMSILVA - 03.03.2025
                                      OR auart EQ 'ZTRP'  "BUG 168688 - MMSILVA - 03.03.2025
                                      OR auart EQ 'ZELI'  "BUG 191904 - FA - 29.09.2025
                                      OR auart EQ 'ZPOR'. "BUG 191904 - FA - 29.09.2025


        READ TABLE gt_zsdt0225  INTO gw_zsdt0225 WITH KEY nr_ov = gw_vbak-vbeln.

        IF ( sy-subrc EQ 0 ).

          gw_aquaviario-auart = gw_vbak-auart.

          "READ TABLE GT_KONV INTO GW_KONV WITH KEY KNUMV = GW_VBAK-KNUMV.
          "GW_AQUAVIARIO-KBETR = GW_KONV-KBETR.

          gw_aquaviario-kbetr = gw_zsdt0225-vlr_usd.

          gw_aquaviario-nr_ov      = gw_zsdt0225-nr_ov.
          gw_aquaviario-vbeln      = gw_vbak-vbeln.
          gw_aquaviario-bukrs      = gw_zsdt0225-bukrs.
          gw_aquaviario-werks      = gw_zsdt0225-werks.
          gw_aquaviario-ano_viagem = gw_zsdt0225-ano_viagem.

*-CS2019001753-09.01.2023-#84936-JT-inicio
          gw_aquaviario-navio      = gw_zsdt0225-navio.
          gw_aquaviario-local_operacao  = gw_zsdt0225-local_operacao.
*-CS2019001753-09.01.2023-#84936-JT-fim

          READ TABLE gt_lfa1 INTO gw_lfa1 WITH KEY lifnr = gw_zsdt0225-po_embarque.

          gw_aquaviario-ort01_l = gw_lfa1-ort01.
          gw_aquaviario-regio_l = gw_lfa1-regio.
          gw_aquaviario-name1_l = gw_lfa1-name1.

          READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_zsdt0225-po_destino.

          gw_aquaviario-ort01_k = gw_kna1-ort01.
          gw_aquaviario-regio_k = gw_kna1-regio.
          gw_aquaviario-name1_k = gw_kna1-name1.

          gw_aquaviario-cod_material = gw_zsdt0225-cod_material.
          gw_aquaviario-tp_class     = gw_zsdt0225-tp_class.

          READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_zsdt0225-cod_material.

          gw_aquaviario-maktx = gw_makt-maktx.

          APPEND gw_aquaviario TO gt_aquaviario.

        ENDIF.

        CLEAR: gw_aquaviario, gw_vbak, gw_zsdt0225,  gw_lfa1, gw_kna1, gw_makt.
      ENDLOOP.
    ENDIF.
*****************
* PBI - 66979 - Inicio

    LOOP AT  gt_j_1bnflin INTO gw_j_1bnflin.

      gw_aquaviario_aux-refkey_vbeln = gw_j_1bnflin-refkey_vbeln.

      SELECT SINGLE vbelv
       FROM vbfa
      INTO gw_aquaviario_aux-vbelv
       WHERE vbeln = gw_j_1bnflin-refkey_vbeln.

      SELECT SINGLE auart
       FROM vbak
      INTO gw_aquaviario_aux-auart
       WHERE vbeln = gw_aquaviario_aux-vbelv.

      SELECT SINGLE kursk netwr brgew
        FROM vbrp
      INTO (gw_aquaviario_aux-tax_dolar , gw_aquaviario_aux-kbetr , gw_aquaviario_aux-brgew )
        WHERE vbeln  = gw_j_1bnflin-refkey_vbeln.

      IF gw_aquaviario_aux-tax_dolar = 1.
        CLEAR: gw_aquaviario_aux-kbetr.
      ENDIF.

      SELECT SINGLE maktx
        FROM j_1bnflin
      INTO gw_aquaviario_aux-maktx
        WHERE docnum  = gw_j_1bnflin-docnum.

      APPEND gw_aquaviario_aux TO gt_aquaviario_aux.
      CLEAR: gw_aquaviario_aux.
    ENDLOOP.
* PBI - 66979 - Fim
*****************

    "Limpar a Tabela LFA1, KNA1 e MAKT, KONV.
    REFRESH: gt_lfa1[],
             gt_kna1[],
             gt_makt[].

*****************
*  Descrição: Item de transporte
*  Data: 09.12.2013 10:57:34
* Parametros: TKNUM = GT_VBAK-TKNUM (Documento do Transporte).
*****************
    SELECT * FROM vttp
      INTO TABLE gt_vttp
      FOR ALL ENTRIES IN gt_vbak
    WHERE tknum EQ gt_vbak-tknum.

    IF NOT ( gt_vttp[] IS INITIAL ). "Verifica se Existe Item de Tranposrte para documento Rodoviario.

      "*****************
      "* Descrição: Documento SD: parceiro
      "* Data: 09.12.2013 11:00:49
      "* Parametros: VBELN = GT_VTTP-VBELN (Nº documento de vendas e distribuição)
      "*             PARVW = PC-LR (PC = PONTO DE COLETA / LR = LOCAL DE ENTREGA ).
      "* ****************
      SELECT * FROM vbpa
        INTO TABLE gt_vbpa
       FOR ALL ENTRIES IN gt_vttp
     WHERE vbeln EQ gt_vttp-vbeln
        AND parvw IN ('PC','LR' ).

      "*****************
      "*   Descrição: Mestre de fornecedores (parte geral)
      "*   Data: 09.12.2013 10:51:56
      "*  Parametros: LIFNR = GT_VBPA-LIFNR.
      "*****************

      SELECT * FROM lfa1
        INTO TABLE gt_lfa1
        FOR ALL ENTRIES IN gt_vbpa
      WHERE lifnr EQ gt_vbpa-lifnr.

      "*****************
      "*  Descrição: Mestre de clientes (parte geral)
      "*  Data: 09.12.2013 10:51:57
      "* Parametros: KUNNR = GT_VBPA-KUNNR.
      "*****************

      SELECT * FROM kna1
        INTO TABLE gt_kna1
        FOR ALL ENTRIES IN gt_vbpa
      WHERE kunnr EQ gt_vbpa-kunnr.

      "*****************
      "*  Descrição: Documento SD: fornecimento: dados de item
      "*  Data: 09.12.2013 15:27:34
      "* Parametros: VBELN = GT_VTTP-VBELN
      "*****************
      SELECT * FROM lips
        INTO TABLE gt_lips
        FOR ALL ENTRIES IN gt_vttp
     WHERE vbeln EQ gt_vttp-vbeln.

      "*****************
      "*  Descrição: Textos breves de material
      "*  Data: 09.12.2013 15:31:37
      "*  Parametros: MATNR = GT_LIPS-MATNR
      "*              SPRAS = 'PT' (Portugues).
      "*****************

      SELECT * FROM makt
        INTO TABLE gt_makt
        FOR ALL ENTRIES IN gt_lips
      WHERE matnr EQ gt_lips-matnr
        AND spras EQ 'PT'.

      "*****************
      "*  Descrição: Custos de frete: dados do item
      "*  Data: 09.12.2013 15:55:50
      "* Parametros: REBEL = GT_VBAK-TKNUM (Documento de Transporte).
      "*****************

      SELECT * FROM vfkp
        INTO TABLE gt_vfkp
        FOR ALL ENTRIES IN gt_vbak
      WHERE rebel EQ gt_vbak-tknum.

      "*****************
      "* Descrição: Condições (dados de operação)
      "* Data: 09.12.2013 16:07:51
      "* Parametros: KNUMV = GT_VFKP-KNUMV
      "*****************

      SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @gt_vfkp WHERE knumv EQ @gt_vfkp-knumv AND kschl EQ 'ZPED' INTO CORRESPONDING FIELDS OF TABLE @gt_konv_r .

      CLEAR: gw_vbak.

      "*****************
      "* Descrição: Loop para alimentar a estrutura do Rodoviario.
      "* Data: 09.12.2013 16:10:00
      "* Parametros: KNUMV = GT_VFKP-KNUMV
      "* ZTRO = Transporte Rodoviario.
      "* ZTRA = Transporte Rodoviario Avulso
      "* ZTRT = Transporte Rodoviario Prestação Terceiro.
      "*****************

      LOOP AT gt_vbak INTO gw_vbak WHERE auart EQ 'ZTRO'
                                      OR auart EQ 'ZTRA'
                                      OR auart EQ 'ZTRT'.
*                                      OR AUART EQ 'ZTRG'. "Comentado AOENNING / 31/03/2020

        gw_rodoviario-auart = gw_vbak-auart.
        gw_rodoviario-waerk = gw_vbak-waerk.

        READ TABLE gt_vbfa INTO gw_vbfa WITH KEY vbelv = gw_vbak-vbeln.
        gw_rodoviario-vbelv = gw_vbfa-vbelv.

        READ TABLE gt_vbap INTO gw_vbap WITH KEY vbeln = gw_vbfa-vbelv.
        gw_rodoviario-j_1btxsdc = gw_vbap-j_1btxsdc.

        READ TABLE gt_vttp INTO gw_vttp WITH KEY tknum = gw_vbak-tknum.
        gw_rodoviario-vbeln = gw_vttp-vbeln.

        READ TABLE gt_vbpa INTO gw_vbpa  WITH KEY vbeln = gw_vttp-vbeln
                                                  parvw = 'PC'.
        gw_rodoviario-parvw = gw_vbpa-parvw.


        READ TABLE gt_lfa1 INTO gw_lfa1 WITH KEY lifnr = gw_vbpa-lifnr.
        gw_rodoviario-lifnr = gw_lfa1-lifnr.
        gw_rodoviario-ort01 = gw_lfa1-ort01.
        gw_rodoviario-regio = gw_lfa1-regio.

        CLEAR: gw_vbpa.
        READ TABLE gt_vbpa INTO gw_vbpa  WITH KEY vbeln = gw_vttp-vbeln
                                                  parvw = 'LR'.
        gw_rodoviario-parvw = gw_vbpa-parvw.

        READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_vbpa-kunnr.
        gw_rodoviario-kunnr   = gw_kna1-kunnr.
        gw_rodoviario-ort01_k = gw_kna1-ort01.
        gw_rodoviario-regio_k = gw_kna1-regio.

        READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_vttp-vbeln.
        gw_rodoviario-matnr = gw_lips-matnr.

        READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_lips-matnr.
        gw_rodoviario-maktx = gw_makt-maktx.

        READ TABLE gt_vfkp INTO gw_vfkp WITH KEY rebel = gw_vbak-tknum.
        gw_rodoviario-knumv = gw_vfkp-knumv.

        READ TABLE gt_konv_r  INTO gw_konv_r WITH KEY knumv = gw_vfkp-knumv.
        gw_rodoviario-kbetr = gw_konv_r-kbetr.

        APPEND gw_rodoviario TO gt_rodoviario.

        CLEAR: gw_vbak, gw_vttp, gw_vbpa, gw_lfa1, gw_kna1, gw_lips, gw_makt, gw_vfkp, gw_konv_r.
      ENDLOOP.
    ENDIF.

  ENDIF.


  "Somente Agrupar os dados caso tenha encontrado algum registro no cabeçalho do conhecimento.
  IF NOT ( gt_j_1bnfdoc[] IS INITIAL ).
    PERFORM: agrupar_dados.
  ENDIF.



ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  AGRUPAR_DADOS
*&---------------------------------------------------------------------*
*       Perform responsavel por agrupar os dados selecionados na pesquisa.
*----------------------------------------------------------------------*
FORM agrupar_dados .

  DATA: tl_estilo_cell TYPE lvc_t_scol WITH HEADER LINE, "Estilo para celula
        wl_estilo_cell TYPE lvc_s_scol. "Estilo para celula.

  DATA: vl_data_tx TYPE c LENGTH 10, "Data para Buscar Taxa Rodoviario na TCURR - Convertida para BRL
        vl_validto TYPE j_1btxiss-validto. "Data, até a qual o regulamento fiscal é valido


  "Limpar as WORK AREA.
  CLEAR: gw_j_1bnfdoc, gw_j_1bnflin, gw_j_1bnfstx, gw_vbfa, gw_vbak, gw_aquaviario,
         gw_rodoviario, gw_j_1bnfe_active.

  LOOP AT gt_j_1bnfdoc INTO gw_j_1bnfdoc.

    IF gw_j_1bnfdoc-model NE '01'.


      "Buscar o status do conhecimento pelo o documento SAP (docnum).
      "      READ TABLE gt_j_1bnfe_active INTO gw_j_1bnfe_active WITH KEY docnum = gw_j_1bnfdoc-docnum.

      "      IF ( sy-subrc EQ 0 ).

      gw_saida-docnum = gw_j_1bnfdoc-docnum.
      gw_saida-bukrs  = gw_j_1bnfdoc-bukrs.
      gw_saida-branch = gw_j_1bnfdoc-branch.
      gw_saida-nfenum = gw_j_1bnfdoc-nfenum.
      gw_saida-series = gw_j_1bnfdoc-series.
      gw_saida-parid  = gw_j_1bnfdoc-parid.
      gw_saida-name1  = gw_j_1bnfdoc-name1.
      gw_saida-stains = gw_j_1bnfdoc-stains.
      gw_saida-ort01  = gw_j_1bnfdoc-ort01.
      gw_saida-regio  = gw_j_1bnfdoc-regio.
      gw_saida-pstdat = gw_j_1bnfdoc-pstdat.
      gw_saida-docdat = gw_j_1bnfdoc-docdat.
      gw_saida-brgew  = gw_j_1bnfdoc-brgew.
      gw_saida-crenam = gw_j_1bnfdoc-crenam.

      "Se encontrar informaçãos do código CGC atribuir no campo CGC_CPF do ALV
      "Senão atribuir o CPF.
      IF NOT ( gw_j_1bnfdoc-cgc IS INITIAL ).
        gw_saida-cgc_cpf = gw_j_1bnfdoc-cgc.
      ELSE.
        gw_saida-cgc_cpf = gw_j_1bnfdoc-cpf.
      ENDIF.

      "Verificar o Status do conhecimento Eletronico.
      "Caso o Campo CANCEL da J_1BNFDOC esteja marcado X, quer dizer que o conhecimento esta cancelado.
      "Atribuir o valor de CANCELADO quando estiver marcado com X e ATIVO quando não estiver marcado.
      CASE gw_j_1bnfdoc-cancel.
        WHEN: 'X'.

          gw_saida-cancel = 'CANCELADO'.

          wl_estilo_cell-fname = 'STATUS_ICON'.
          wl_estilo_cell-color-col   = '6'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          APPEND wl_estilo_cell TO tl_estilo_cell.
          INSERT LINES OF tl_estilo_cell INTO TABLE gw_saida-estilo_cell.

        WHEN OTHERS.

          gw_saida-cancel = 'ATIVO'.

          wl_estilo_cell-fname = 'STATUS_ICON'.
          wl_estilo_cell-color-col   = '5'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          APPEND wl_estilo_cell TO tl_estilo_cell.
          INSERT LINES OF tl_estilo_cell INTO TABLE gw_saida-estilo_cell.


      ENDCASE.

      "Buscar Itens dos conhecimentos encontrado no cabeçalho (J_1BNFDOC) pelo o número documento SAP (DOCNUM).
      READ TABLE gt_j_1bnflin INTO gw_j_1bnflin WITH KEY docnum = gw_j_1bnfdoc-docnum.
      gw_saida-cfop = gw_j_1bnflin-cfop.

      "Calcular o Valor Unitario do Conhecimento.
      "Formula: Valor líquido incluindo impostos(NFNET - ITENS) / Peso bruto(BRGEW - CABEÇALHO)
      "         GW_J_1BNFLIN-NFNET / GW_J_1BNFDOC-BRGEW.

      IF ( gw_j_1bnfdoc-brgew > 0 ).
        gw_saida-nfnet_unit = gw_j_1bnflin-nfnet / gw_j_1bnfdoc-brgew.
      ENDIF.


      "Atribuir CST (ICMS,IPI, PIS, COFINS).

      "CST ICMS
      "Rotina para converter o número do CST ICMS
      CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
        EXPORTING
          input  = gw_j_1bnflin-taxsit
        IMPORTING
          output = gw_saida-taxsit.

      gw_saida-taxsi2 = gw_j_1bnflin-taxsi2. "CST IPI
      gw_saida-taxsi5 = gw_j_1bnflin-taxsi5. "CST PIS
      gw_saida-taxsi4 = gw_j_1bnflin-taxsi4. "CST COFINS
      gw_saida-taxsi3 = gw_j_1bnflin-taxsi3. "CST ISS

      "Fatura
      gw_saida-refkey = gw_j_1bnflin-refkey.

      "Buscar Imposto por Item do conhecimento (J_1BNFLIN) pelo o número documento SAP (DOCNUM)
      "E os tipos de impostos:
      "  ICM3 = ICMS DE SD
      "  IPSN = PIS NVV NORMAL TAX
      "  ICON = COFINS NVV NORMAL TAX
      READ TABLE gt_j_1bnfstx INTO gw_j_1bnfstx WITH KEY docnum = gw_j_1bnfdoc-docnum
                                                         taxtyp = 'ICM3'.
      "Caso TAXTYP seja ICM3 (ICMS DE SD).
      "Atribuir BASE (MONTANGE BÁSICO).
      "         RATE (TAXA DE IMPOSTO).
      "         TAXVAL (VALOR FISCAL).

      gw_saida-base         = gw_j_1bnfstx-base.   "Montante básico
      gw_saida-rate         = gw_j_1bnfstx-rate.   "Taxa de imposto
      gw_saida-taxval_icms  = gw_j_1bnfstx-taxval. "Valor fiscal

      CLEAR: gw_j_1bnfstx.
      READ TABLE gt_j_1bnfstx INTO gw_j_1bnfstx WITH KEY docnum = gw_j_1bnfdoc-docnum
                                                         taxtyp = 'IPSN'.

      "Caso TAXTYP seja IPSN (PIS NVV NORMAL TAX).
      "Atribuir TAXVAL (valor Fiscal).

      gw_saida-taxval_pis  = gw_j_1bnfstx-taxval. "Valor fiscal

      CLEAR: gw_j_1bnfstx.
      READ TABLE gt_j_1bnfstx INTO gw_j_1bnfstx WITH KEY docnum = gw_j_1bnfdoc-docnum
                                                         taxtyp = 'ICON'.
      "Caso TAXTYP seja ICON (COFINS NVV NORMAL TAX).
      "Atribuir TAXVAL (valor Fiscal).

      gw_saida-taxval_cofins  = gw_j_1bnfstx-taxval. "Valor fiscal


      CLEAR: gw_j_1bnfstx.
      READ TABLE gt_j_1bnfstx INTO gw_j_1bnfstx WITH KEY docnum = gw_j_1bnfdoc-docnum
                                                         taxtyp = 'ISSE'.
      gw_saida-taxval_iss  = gw_j_1bnfstx-taxval. "Valor fiscal


      gw_saida-othbas  = gw_j_1bnfstx-othbas. "Montante básico excluído
      gw_saida-excbas  = gw_j_1bnfstx-excbas. "Outro montante básico

      "Buscar Documetnos de Vendas e Distribuição pelo REFKEY dos ITENS.
      READ TABLE gt_vbfa INTO gw_vbfa WITH KEY vbeln = gw_j_1bnflin-refkey_vbeln.

      "Buscar informaçãos de vendas no cabeçalho pelo  documento de venda e distribuição precedente (VBELV).
      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_vbfa-vbelv.

      "Buscar dados na estrutura do Aquaviario caso existem.
      IF NOT ( gt_aquaviario[] IS INITIAL ).

        READ TABLE gt_aquaviario INTO gw_aquaviario WITH KEY vbeln = gw_vbak-vbeln.

        IF ( sy-subrc EQ 0 ).

          "Valor do Conhecimento Caso seja Aquaviário.
          gw_saida-nfnet      = gw_j_1bnflin-nfnet.


          gw_saida-icon          = icon_ws_ship.

          gw_saida-matnr         = gw_aquaviario-cod_material.
          gw_saida-maktx         = gw_aquaviario-maktx.
          gw_saida-tp_class      = gw_aquaviario-tp_class.

*-CS2019001753-09.01.2023-#84936-JT-inicio
          gw_saida-navio         = gw_aquaviario-navio.
          gw_saida-local_operacao = gw_aquaviario-local_operacao.
*-CS2019001753-09.01.2023-#84936-JT-fim

          READ TABLE gt_zlest0061  INTO gw_zlest0061 WITH KEY nr_ov = gw_aquaviario-vbeln.

          gw_saida-unit_dolar =   ( gw_zlest0061-vlr_usd / gw_zlest0061-peso_vinculado ) * 1000.
          gw_saida-kbetr      =  gw_zlest0061-vlr_usd.
          gw_saida-tx_dolar   =  gw_zlest0061-tax_dolar.

*          "Calcular Valor Unitario do Dolar.
*          "Formula KONV-KBETR / J_1BNFDOC-BRGEW.
*          IF (  GW_J_1BNFDOC-BRGEW > 0 ).
*            GW_SAIDA-UNIT_DOLAR    = GW_AQUAVIARIO-KBETR / GW_J_1BNFDOC-BRGEW. "Valor Unit. Dolar.
*          ENDIF.
*
*          "Atribuir o Valor do Dolar sem calculo.
*          GW_SAIDA-KBETR         = GW_AQUAVIARIO-KBETR. "Valor do Dolar.
*
*          "Calcular a Taxa do Dolar.
*          "Formula / J_1BNFLIN-NFNET / KONV-KBETR.
*          IF ( GW_AQUAVIARIO-KBETR > 0 ).
*            GW_SAIDA-TX_DOLAR      =  GW_J_1BNFLIN-NFNET / GW_AQUAVIARIO-KBETR. "Taxa do Dolar.
*          ENDIF.

          gw_saida-vbelv         = gw_aquaviario-nr_ov.
          gw_saida-auart         = gw_aquaviario-auart.
          gw_saida-ort01_coleta  = gw_aquaviario-ort01_l.
          gw_saida-regio_coleta  = gw_aquaviario-regio_l.
          gw_saida-name1_coleta  = gw_aquaviario-name1_l.

          gw_saida-ort01_entrega = gw_aquaviario-ort01_k.
          gw_saida-regio_entrega = gw_aquaviario-regio_k.
          gw_saida-name1_entrega = gw_aquaviario-name1_k.
          gw_saida-ck_anulado    = gw_aquaviario-ck_anulado.

          "Read especifico para o IVA do Aquaviario.
          READ TABLE gt_vbfa INTO gw_vbfa WITH KEY vbeln = gw_j_1bnflin-refkey_vbeln.
          READ TABLE gt_vbap INTO gw_vbap WITH KEY vbeln = gw_vbfa-vbelv.
          gw_saida-j_1btxsdc = gw_vbap-j_1btxsdc.


        ENDIF.

      ELSE.

*** pbi 66979 - inicio
        "Buscar dados na estrutura do Aquaviario caso existem .
        IF NOT ( gt_aquaviario_aux[] IS INITIAL ).

          READ TABLE gt_aquaviario_aux INTO gw_aquaviario_aux WITH KEY refkey_vbeln = gw_j_1bnflin-refkey_vbeln.

          IF ( sy-subrc EQ 0 ).
            gw_saida-vbelv    = gw_aquaviario_aux-vbelv.
            gw_saida-auart    = gw_aquaviario_aux-auart.
            gw_saida-maktx    = gw_aquaviario_aux-maktx.
            gw_saida-kbetr    = gw_aquaviario_aux-kbetr   .
            gw_saida-tx_dolar = gw_aquaviario_aux-tax_dolar.

            gw_saida-nfnet      = gw_j_1bnflin-nfnet.

            IF gw_aquaviario_aux-tax_dolar > 1.
              IF gw_aquaviario_aux-brgew > 1.
                "VBRP-NETWR (valor dólar) / VBRP-BRGEW (peso bruto) * 1000
                gw_saida-unit_dolar =  gw_aquaviario_aux-kbetr / gw_aquaviario_aux-brgew * 1000.
              ELSE.
                IF gw_aquaviario_aux-brgew = 1.
                  "VBRP-NETWR (valor dólar) / VBRP-BRGEW (peso bruto
                  gw_saida-unit_dolar = gw_aquaviario_aux-kbetr / gw_aquaviario_aux-brgew.
                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR: gw_aquaviario_aux.
          ENDIF.
        ENDIF.
*** PBI 66979 - Fim
      ENDIF.

      "Buscar Informações do Rodoviario caso existem.
      IF NOT ( gt_rodoviario IS INITIAL ).

        READ TABLE gt_rodoviario INTO gw_rodoviario WITH KEY vbelv = gw_vbak-vbeln.

        IF ( sy-subrc EQ 0 ).

          "Valor do Conhecimento caso seja Rodoviario.
          gw_saida-nfnet      = gw_j_1bnflin-netwr.

          gw_saida-icon          = icon_import_all_requests.
          gw_saida-kbetr_ped     = gw_rodoviario-kbetr. "Valor do Pedagio.
          gw_saida-j_1btxsdc     = gw_rodoviario-j_1btxsdc. "IVA

          gw_saida-vbelv         = gw_rodoviario-vbelv. "Ordem de Venda
          gw_saida-auart         = gw_rodoviario-auart.
          gw_saida-ort01_coleta  = gw_rodoviario-ort01.
          gw_saida-regio_coleta  = gw_rodoviario-regio.
          gw_saida-ort01_entrega = gw_rodoviario-ort01_k.
          gw_saida-regio_entrega = gw_rodoviario-regio_k.
          gw_saida-matnr         = gw_rodoviario-matnr.
          gw_saida-maktx         = gw_rodoviario-maktx.

          "Capturar o Valor em Dolar para Rodoviario.
          CASE gw_rodoviario-waerk.

            WHEN: 'BRL'.

              CLEAR: obj_util, vl_data_tx, vl_validto.
              CREATE OBJECT obj_util.

              "Método da Classe Util, responsavel por converter a data no formato Americano para o Brasileiro.
              "Parametros: I_DATA  = J_1BNFDOC-PSTDAT - Data de lançamento
              "            I_OPCAO = '.' Data com ponto. (Formato: D.M.ANO (DD.MM.YYYY) ).
              "Retorno:    E_DATA  = Retorna a Data no formato já convertido. (DD.MM.YYYY).

              obj_util->conv_data_us_br( EXPORTING i_data  = gw_j_1bnfdoc-pstdat
                                                   i_opcao = '.'
                                         RECEIVING e_data  = vl_data_tx ).


              "Função para Converter data para o tipo Data, até a qual o regulamento fiscal é valido
              "Parametro: INPUT = Data do Lançamento já convertida para o formato Brasileiro.
              CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
                EXPORTING
                  input  = vl_data_tx
                IMPORTING
                  output = vl_validto.


              "Seleciona o valor da Taxa na tabela de Taxas de câmbio.
              "Parametros:  KURST = Conversão standard com câmbio de venda
              "             FCURR = Moeda de Precedencia
              "             TCURR = Moeda de Destino
              "             GDATU = Data a partir da qual o câmbio é válido (já convertida para o tipo valido).

              SELECT SINGLE * FROM tcurr INTO gw_tcurr WHERE kurst = 'B'
                                                         AND fcurr = 'USD'
                                                         AND tcurr = 'BRL'
                                                         AND gdatu = vl_validto.
              "Caso encontre a taxa de cambio
              "Dividir o Valor líquido do item da nota com o valor da taxa (UKURS).
              IF ( sy-subrc EQ 0 ).
                gw_saida-kbetr    = gw_j_1bnflin-netwr / gw_tcurr-ukurs.
                gw_saida-tx_dolar = gw_tcurr-ukurs.
              ENDIF.


            WHEN: 'USD'.

          ENDCASE.
        ENDIF.
      ENDIF.

      APPEND gw_saida TO gt_saida.

    ELSE.

      gw_saida-docnum = gw_j_1bnfdoc-docnum.
      gw_saida-bukrs  = gw_j_1bnfdoc-bukrs.
      gw_saida-branch = gw_j_1bnfdoc-branch.

      IF gw_j_1bnfdoc-nfenum IS NOT INITIAL.
        gw_saida-nfenum = gw_j_1bnfdoc-nfenum.
      ELSE.
        gw_saida-nfenum = gw_j_1bnfdoc-nfnum.
      ENDIF.

      gw_saida-series = gw_j_1bnfdoc-series.
      gw_saida-parid  = gw_j_1bnfdoc-parid.
      gw_saida-name1  = gw_j_1bnfdoc-name1.
      gw_saida-stains = gw_j_1bnfdoc-stains.
      gw_saida-ort01  = gw_j_1bnfdoc-ort01.
      gw_saida-regio  = gw_j_1bnfdoc-regio.
      gw_saida-pstdat = gw_j_1bnfdoc-pstdat.
      gw_saida-docdat = gw_j_1bnfdoc-docdat.
      gw_saida-brgew  = gw_j_1bnfdoc-brgew.
      gw_saida-crenam = gw_j_1bnfdoc-crenam.

      IF NOT ( gw_j_1bnfdoc-cgc IS INITIAL ).
        gw_saida-cgc_cpf = gw_j_1bnfdoc-cgc.
      ELSE.
        gw_saida-cgc_cpf = gw_j_1bnfdoc-cpf.
      ENDIF.

      CASE gw_j_1bnfdoc-cancel.
        WHEN: 'X'.

          gw_saida-cancel = 'CANCELADO'.

          wl_estilo_cell-fname = 'STATUS_ICON'.
          wl_estilo_cell-color-col   = '6'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          APPEND wl_estilo_cell TO tl_estilo_cell.
          INSERT LINES OF tl_estilo_cell INTO TABLE gw_saida-estilo_cell.

        WHEN OTHERS.

          gw_saida-cancel = 'ATIVO'.

          wl_estilo_cell-fname = 'STATUS_ICON'.
          wl_estilo_cell-color-col   = '5'.
          wl_estilo_cell-color-int   = '0'.
          wl_estilo_cell-color-inv   = '0'.
          APPEND wl_estilo_cell TO tl_estilo_cell.
          INSERT LINES OF tl_estilo_cell INTO TABLE gw_saida-estilo_cell.
      ENDCASE.

      "Buscar Itens dos conhecimentos encontrado no cabeçalho (J_1BNFDOC) pelo o número documento SAP (DOCNUM).
      READ TABLE gt_j_1bnflin INTO gw_j_1bnflin WITH KEY docnum = gw_j_1bnfdoc-docnum.
      gw_saida-cfop = gw_j_1bnflin-cfop.

      IF ( gw_j_1bnfdoc-brgew > 0 ).
        gw_saida-nfnet_unit = gw_j_1bnflin-nfnet / gw_j_1bnfdoc-brgew.
      ENDIF.

      "Atribuir CST (ICMS,IPI, PIS, COFINS).

      "CST ICMS
      "Rotina para converter o número do CST ICMS
      CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
        EXPORTING
          input  = gw_j_1bnflin-taxsit
        IMPORTING
          output = gw_saida-taxsit.

      gw_saida-taxsi2 = gw_j_1bnflin-taxsi2. "CST IPI
      gw_saida-taxsi5 = gw_j_1bnflin-taxsi5. "CST PIS
      gw_saida-taxsi4 = gw_j_1bnflin-taxsi4. "CST COFINS
      gw_saida-taxsi3 = gw_j_1bnflin-taxsi3. "CST ISS

      "Fatura
      gw_saida-refkey = gw_j_1bnflin-refkey.

      READ TABLE gt_j_1bnfstx INTO gw_j_1bnfstx WITH KEY docnum = gw_j_1bnfdoc-docnum
                                                        taxtyp = 'ICM3'.

      gw_saida-base         = gw_j_1bnfstx-base.   "Montante básico
      gw_saida-rate         = gw_j_1bnfstx-rate.   "Taxa de imposto
      gw_saida-taxval_icms  = gw_j_1bnfstx-taxval. "Valor fiscal

      CLEAR: gw_j_1bnfstx.
      READ TABLE gt_j_1bnfstx INTO gw_j_1bnfstx WITH KEY docnum = gw_j_1bnfdoc-docnum
                                                         taxtyp = 'IPSN'.

      gw_saida-taxval_pis  = gw_j_1bnfstx-taxval. "Valor fiscal

      CLEAR: gw_j_1bnfstx.
      READ TABLE gt_j_1bnfstx INTO gw_j_1bnfstx WITH KEY docnum = gw_j_1bnfdoc-docnum
                                                         taxtyp = 'ISSE'.
      gw_saida-taxval_iss  = gw_j_1bnfstx-taxval. "Valor fiscal


      CLEAR: gw_j_1bnfstx.
      READ TABLE gt_j_1bnfstx INTO gw_j_1bnfstx WITH KEY docnum = gw_j_1bnfdoc-docnum
                                                         taxtyp = 'ICON'.


      gw_saida-taxval_cofins  = gw_j_1bnfstx-taxval. "Valor fiscal
      gw_saida-othbas  = gw_j_1bnfstx-othbas. "Montante básico excluído
      gw_saida-excbas  = gw_j_1bnfstx-excbas. "Outro montante básico

      "Buscar Documetnos de Vendas e Distribuição pelo REFKEY dos ITENS.
      READ TABLE gt_vbfa INTO gw_vbfa WITH KEY vbeln = gw_j_1bnflin-refkey_vbeln.

      "Buscar informaçãos de vendas no cabeçalho pelo  documento de venda e distribuição precedente (VBELV).
      READ TABLE gt_vbak INTO gw_vbak WITH KEY vbeln = gw_vbfa-vbelv.

      "Buscar dados na estrutura do Aquaviario caso existem.
      IF NOT ( gt_aquaviario[] IS INITIAL ).

        READ TABLE gt_aquaviario INTO gw_aquaviario WITH KEY vbeln = gw_vbak-vbeln.

        IF ( sy-subrc EQ 0 ).

          "Valor do Conhecimento Caso seja Aquaviário.
          gw_saida-nfnet      = gw_j_1bnflin-nfnet.
          gw_saida-icon          = icon_ws_ship.
          gw_saida-matnr         = gw_aquaviario-cod_material.
          gw_saida-maktx         = gw_aquaviario-maktx.
          gw_saida-tp_class      = gw_aquaviario-tp_class.

*-CS2019001753-09.01.2023-#84936-JT-inicio
          gw_saida-navio         = gw_aquaviario-navio.
          gw_saida-local_operacao = gw_aquaviario-local_operacao.
*-CS2019001753-09.01.2023-#84936-JT-fim

          READ TABLE gt_zsdt0225  INTO gw_zsdt0225 WITH KEY nr_ov = gw_aquaviario-vbeln.
* Inicio - FA - 29.09.2025
          IF gw_zsdt0225-peso_vinculado = '0.000' OR gw_zsdt0225-peso_vinculado IS INITIAL.

            SELECT SINGLE menge
              FROM zsdt0306_fat
              INTO @DATA(ls_menge)
              WHERE id_seq    = @gw_zsdt0225-id_seq AND
                    dt_recreg = @gw_zsdt0225-dt_recreg.
            IF sy-subrc IS INITIAL.
              gw_saida-unit_dolar =  ( gw_zsdt0225-vlr_usd / ls_menge ) * 1000.

            ENDIF.

          ENDIF.
*          gw_saida-unit_dolar =  ( gw_zsdt0225-vlr_usd / gw_zsdt0225-peso_vinculado ) * 1000.
* Fim - FA - 29.09.2025

          gw_saida-kbetr      =  gw_zsdt0225-vlr_usd.
          gw_saida-tx_dolar   =  gw_zsdt0225-tax_dolar.
*
*          "Calcular Valor Unitario do Dolar.
*          "Formula KONV-KBETR / J_1BNFDOC-BRGEW.
*          IF (  GW_J_1BNFDOC-BRGEW > 0 ).
*            GW_SAIDA-UNIT_DOLAR    = GW_AQUAVIARIO-KBETR / GW_J_1BNFDOC-BRGEW. "Valor Unit. Dolar.
*          ENDIF.
*
*          "Atribuir o Valor do Dolar sem calculo.
*          GW_SAIDA-KBETR         = GW_AQUAVIARIO-KBETR. "Valor do Dolar.
*
*          "Calcular a Taxa do Dolar.
*          "Formula / J_1BNFLIN-NFNET / KONV-KBETR.
*          IF ( GW_AQUAVIARIO-KBETR > 0 ).
*            GW_SAIDA-TX_DOLAR      =  GW_J_1BNFLIN-NFNET / GW_AQUAVIARIO-KBETR. "Taxa do Dolar.
*          ENDIF.

          gw_saida-vbelv         = gw_aquaviario-nr_ov.
          gw_saida-auart         = gw_aquaviario-auart.
          gw_saida-ort01_coleta  = gw_aquaviario-ort01_l.
          gw_saida-regio_coleta  = gw_aquaviario-regio_l.
          gw_saida-name1_coleta  = gw_aquaviario-name1_l.

          gw_saida-ort01_entrega = gw_aquaviario-ort01_k.
          gw_saida-regio_entrega = gw_aquaviario-regio_k.
          gw_saida-name1_entrega = gw_aquaviario-name1_k.

          gw_saida-ck_anulado    = gw_aquaviario-ck_anulado.

          "Read especifico para o IVA do Aquaviario.
          READ TABLE gt_vbfa INTO gw_vbfa WITH KEY vbeln = gw_j_1bnflin-refkey_vbeln.
          READ TABLE gt_vbap INTO gw_vbap WITH KEY vbeln = gw_vbfa-vbelv.
          gw_saida-j_1btxsdc = gw_vbap-j_1btxsdc.

        ENDIF.
      ELSE.
*** pbi 66979 - inicio
        "Buscar dados na estrutura do Aquaviario caso existem .
        IF NOT ( gt_aquaviario_aux[] IS INITIAL ).

          READ TABLE gt_aquaviario_aux INTO gw_aquaviario_aux WITH KEY refkey_vbeln = gw_j_1bnflin-refkey_vbeln.

          IF ( sy-subrc EQ 0 ).
            gw_saida-vbelv    = gw_aquaviario_aux-vbelv.
            gw_saida-auart    = gw_aquaviario_aux-auart.
            gw_saida-maktx    = gw_aquaviario_aux-maktx.
            gw_saida-kbetr    = gw_aquaviario_aux-kbetr   .
            gw_saida-tx_dolar = gw_aquaviario_aux-tax_dolar.

            gw_saida-nfnet      = gw_j_1bnflin-nfnet.

            IF gw_aquaviario_aux-tax_dolar > 1.
              IF gw_aquaviario_aux-brgew > 1.
                "VBRP-NETWR (valor dólar) / VBRP-BRGEW (peso bruto) * 1000
                gw_saida-unit_dolar =  gw_aquaviario_aux-kbetr / gw_aquaviario_aux-brgew * 1000.
              ELSE.
                IF gw_aquaviario_aux-brgew = 1.
                  "VBRP-NETWR (valor dólar) / VBRP-BRGEW (peso bruto
                  gw_saida-unit_dolar = gw_aquaviario_aux-kbetr / gw_aquaviario_aux-brgew.
                ENDIF.
              ENDIF.
            ENDIF.

            CLEAR: gw_aquaviario_aux.
          ENDIF.
        ENDIF.
*** PBI 66979 - Fim
      ENDIF.

      APPEND gw_saida TO gt_saida.
    ENDIF.

    CLEAR: gw_j_1bnfdoc, gw_j_1bnflin, gw_j_1bnfstx, gw_vbfa, gw_vbak, gw_aquaviario,
           gw_rodoviario, gw_j_1bnfe_active, gw_saida, gw_tcurr.
  ENDLOOP.

  CHECK NOT gt_saida[] IS INITIAL.

  PERFORM: catalog_alv,
           call_alv.

  CALL SCREEN 0100.

ENDFORM.                    " AGRUPAR_DADOS
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'.
ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.

  CASE sy-ucomm.
    WHEN: 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " PAI_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM catalog_alv .


  REFRESH: gt_fcatalog[]. "Limpa a tabela de Catalog.

  PERFORM montar_catalog USING:
  'ICON'          'Trans.'             '5'    'X' '' '' 'C' '' ''  '' '' '',
  'BUKRS'         'Empresa'            '8'    'X' '' '' ''  '' ''  '' '' '',
  'BRANCH'        'Filial Trans.'      '8'    'X' '' '' ''  '' ''  '' '' '',
  'CFOP'          'CFOP'               '7'    ''  '' '' ''  '' ''  '' '' '',
  'NFENUM'        'Nr. CT-e/NFPS'      '12'   'X' '' '' ''  '' ''  '' '' '',
  'SERIES'        'Serie'              '5'    '' '' '' ''  '' ''  '' '' '',
  'DOCNUM'        'Docnum'             '8'    'X' '' '' '' '' ''  '' '' '',
  'CANCEL'        'Status SAP'         '9'    '' '' '' '' '' ''  '' '' '',
  'CK_ANULADO'    'Anulado'            '9'    '' '' '' '' '' ''  '' '' '',
  'PARID'         'Cód. Cliente'       '9'    'X' '' '' '' '' ''  '' '' '',
  'NAME1'         'Desc. Cliente'      '15'   '' '' '' '' '' ''  '' '' '',
  'CGC_CPF'       'CPF/CNPJ Cliente'   '15'   '' '' '' '' '' ''  '' '' '',
  'STAINS'        'Insc. Est. Cliente' '11'   '' '' '' '' '' ''  '' '' '',
  'ORT01'         'Munic. Cliente'     '21'   '' '' '' '' '' ''  '' '' '',
  'REGIO'         'UF Cliente'         '8'    '' '' '' '' '' ''  '' '' '',
  'PSTDAT'        'Dt. Lanç.'          '10'   '' '' '' '' '' ''  '' '' '',
  'DOCDAT'        'Dt. Doct.'          '10'   '' '' '' '' '' ''  '' '' '',
  'MAKTX'         'Material'           '20'   '' '' '' '' '' ''  '' '' '',

*-CS2019001753-09.01.2023-#84936-JT-inicio
  'NAVIO'         'Navio'              '40'   '' '' '' '' '' ''  '' '' '',
  'LOCAL_OPERACAO' 'Local de Operação'  '40'   '' '' '' '' '' ''  '' '' '',
*-CS2019001753-09.01.2023-#84936-JT-fim

  'TP_CLASS'      'Classificação'      '12'   '' '' '' '' '' ''  '' '' '',
  'BRGEW'         'Qtd'                '14'   '' '' '' '' 'X' ''  '' '' '',
  'NFNET_UNIT'    'Unit.'              '8'    '' '' '' '' 'X' ''  '' '' '',
  'NFNET'         'Vlr. Ct-e//NFPS'    '12'   '' '' '' '' 'X' ''  '' '' '',
  'BASE'          'Base ICMS'          '10'   '' '' '' '' 'X' ''  '' '' '',
  'RATE'          'Aliquota'           '10'   '' '' '' '' 'X' ''  '' '' '',
  'TAXVAL_ICMS'   'Vlr. ICMS'          '10'   '' '' '' '' 'X' ''  '' '' '',
  'OTHBAS'        'Outros'             '10'   '' '' '' '' 'X' ''  '' '' '',
  'EXCBAS'        'Isento'             '10'   '' '' '' '' 'X' ''  '' '' '',
  'TAXVAL_ISS'    'Vlr ISS'            '10'   '' '' '' '' 'X' ''  '' '' '',
  'TAXVAL_PIS'    'Vlr. Pis'           '10'   '' '' '' '' 'X' ''  '' '' '',
  'TAXVAL_COFINS' 'Vlr. Cofins'        '10'   '' '' '' '' 'X' ''  '' '' '',
  'KBETR_PED'     'Vlr. Pedagio'       '10'   '' '' '' '' 'X' ''  '' '' '',
  'J_1BTXSDC'     'IVA'                '8'    '' '' '' '' '' ''  '' '' '',
  'TAXSIT'        'CST ICMS'           '8'    '' '' '' '' '' ''  '' '' '',
  'TAXSI2'        'CST IPI'            '8'    '' '' '' '' '' ''  '' '' '',
  'TAXSI5'        'CST PIS'            '8'    '' '' '' '' '' ''  '' '' '',
  'TAXSI4'        'CST COFINS'         '8'    '' '' '' '' '' ''  '' '' '',
  'TAXSI3'        'CST ISS'            '8'    '' '' '' '' '' ''  '' '' '',
  'UNIT_DOLAR'    'Unit. Dólar'        '13'   '' '' '' '' 'X' ''  '' '' '2',
  'KBETR'         'Vlr. Dólar'         '13'   '' '' '' '' 'X' ''  '' '' '',
  'TX_DOLAR'      'Tx. Dólar'          '13'   '' '' '' '' '' ''  '' '' '',
  'REFKEY'        'Fatura'             '10'   'X' '' '' '' '' ''  '' '' '',
  'VBELV'         'O.V'                '10'   '' '' '' '' '' ''  '' '' '',
  'AUART'         'Tipo. O.V'          '10'   '' '' '' '' '' ''  '' '' '',
  'ORT01_COLETA'  'Cidade Coleta'      '14'   '' '' '' '' '' ''  '' '' '',
  'REGIO_COLETA'  'UF Coleta'          '8'    '' '' '' '' '' ''  '' '' '',
  'NAME1_COLETA'  'Local Coleta'       '25'   '' '' '' '' '' ''  '' '' '',
  'ORT01_ENTREGA' 'Cidade Entrega'     '14'   '' '' '' '' '' ''  '' '' '',
  'REGIO_ENTREGA' 'UF Entrega'         '8'    '' '' '' '' '' ''  '' '' '',
  'NAME1_ENTREGA' 'Local Entrega'      '25'   '' '' '' '' '' ''  '' '' '',
  'CRENAM'        'Usuário'            '9'    '' '' '' '' '' ''  '' '' ''.

ENDFORM.                    " MONTAR_CATALOG_ALV
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_catalog USING VALUE(p_fieldname)
                          VALUE(p_desc)
                          VALUE(p_tam)
                          VALUE(p_no_zero)
                          VALUE(p_hotspot)
                          VALUE(p_cor)
                          VALUE(p_just)
                          VALUE(p_sum)
                          VALUE(p_ref_tabname)   LIKE dd02d-tabname
                          VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                          VALUE(p_tabname)       LIKE dd02d-tabname
                          VALUE(p_decimal).


  CLEAR: gw_fcatalog.

  gw_fcatalog-fieldname = p_fieldname.
  gw_fcatalog-ref_table = p_ref_tabname..
  gw_fcatalog-ref_field = p_ref_fieldname.
  gw_fcatalog-tabname   = p_tabname.
  gw_fcatalog-scrtext_l = p_desc.
  gw_fcatalog-scrtext_m = p_desc.
  gw_fcatalog-scrtext_s = p_desc.
  gw_fcatalog-outputlen = p_tam.
  gw_fcatalog-no_zero   = p_no_zero.
  gw_fcatalog-hotspot   = p_hotspot.
  gw_fcatalog-emphasize = p_cor.
  gw_fcatalog-just      = p_just.
  gw_fcatalog-do_sum    = p_sum.
  gw_fcatalog-decimals_o = p_decimal.

  APPEND gw_fcatalog TO gt_fcatalog.

ENDFORM.                    " MONTAR_CATALOG

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_alv .

  DATA: wl_layout TYPE lvc_s_layo.

  CREATE OBJECT obj_gui_container
    EXPORTING
      container_name              = 'CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CREATE OBJECT obj_alv_grid
    EXPORTING
      i_parent          = obj_gui_container
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.


  gs_variant-report   = sy-repid.
  gs_variant-username = sy-uname.

  wl_layout-ctab_fname = 'ESTILO_CELL'.


  CALL METHOD obj_alv_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      i_save                        = 'A'
      is_variant                    = gs_variant
    CHANGING
      it_outtab                     = gt_saida[]
      it_fieldcatalog               = gt_fcatalog[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDFORM.                    " CALL_ALV
*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TABELAS_WA
*&---------------------------------------------------------------------*
FORM limpar_tabelas_wa .


  REFRESH: gt_j_1bnfdoc[],
           gt_j_1bnflin[],
           gt_j_1bnfstx[],
           gt_vbfa[],
           gt_vbak[],
           gt_konv[],
           gt_vbap[],
           gt_zlest0061[],
           gt_zlest0056[],
           gt_lfa1[],
           gt_kna1[],
           gt_vttp[],
           gt_vbpa[],
           gt_aquaviario[].

  CLEAR: gw_j_1bnfdoc,
         gw_j_1bnflin,
         gw_j_1bnfstx,
         gw_vbfa,
         gw_vbak,
         gw_konv,
         gw_vbap,
         gw_zlest0061,
         gw_zlest0056,
         gw_lfa1,
         gw_kna1,
         gw_vttp,
         gw_vbpa,
         gw_aquaviario.
ENDFORM.                    " LIMPAR_TABELAS_WA
