************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 13.05.2019                              *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Geração de Arquivo de Tributos                      *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
REPORT zimp61.

TABLES: zimp_tipos_impos.
DATA:v_seq TYPE sy-tabix.
*-----------------------------------------------------------------------
* Tipos
*-----------------------------------------------------------------------

**********************************************************************
* SEGMENTO W GRU
**********************************************************************
TYPES:
  BEGIN OF ty_estseg_w,
    cod_banco(3)                      TYPE c,
    lote_servico(4)                   TYPE c,
    reg_detalhe_lote(1)               TYPE c,
    num_seq_reg_lote(5)               TYPE c,
    cod_seg_reg_detalhe(1)            TYPE c,
    num_seq_reg_complementar(1)       TYPE c,
    uso_informacoes_complementares(1) TYPE c,
**********************************************************************
****Uso Banco do Brasil**********************************************
**********************************************************************
    numero_ref(20)                    TYPE c,
    competencia(6)                    TYPE c,
    cnpj(14)                          TYPE c,
    valor_principal(14)               TYPE c,
    desconto(14)                      TYPE c,
    branco1(12)                       TYPE c,
    outras_deducoes(14)               TYPE c,
    mora_multa(14)                    TYPE c,
    juros_encargos(14)                TYPE c,
    outros_acrescimos(14)             TYPE c,
    branco2(24)                       TYPE c,
    identificador_tributo(2)          TYPE c,
    info_complementar_tributo(50)     TYPE c,
**********************************************************************
****Uso Bradesco******************************************************
**********************************************************************
    inf_compl1(80)                    TYPE c,
    inf_compl2(80)                    TYPE c,
    inf_tributo(2)                    TYPE c,
    inf_compl_tributo(48)             TYPE c,
    codigo_receita_tributo(6)         TYPE c,
    tipo_id_contribuinte(2)           TYPE c,
    identificacao_contribuinte(14)    TYPE c,
    identificador_fgts(16)            TYPE c,
    lacre_conectividade_social(9)     TYPE c,
    dg_lacre_conectividade(2)         TYPE c,
    reservado(1)                      TYPE c,
**********************************************************************
    cnab(2)                           TYPE c,
    cod_ocorrencias_retorno(10)       TYPE c,

  END OF ty_estseg_w,

  BEGIN OF ty_header_arquivo,
    cod_banco(3)                  TYPE c,
    lote_servico(4)               TYPE c,
    tipo_registro(1)              TYPE c,
    cnab(9)                       TYPE c,
    tipo_inscricao_empresa(1)     TYPE c,
    num_inscricao_empresa(14)     TYPE c,
*
*   cod_convenio_banco(20)        TYPE c,
    cod_convenio_banco(9)         TYPE c,
    cod_convenio_banco2(4)        TYPE c,
    cod_convenio_banco3(7)        TYPE c,
*
    ag_mantenedora_conta(5)       TYPE c,
    d_verificador_agencia(1)      TYPE c,
    num_conta_corrente(12)        TYPE c,
    dg_verificador_conta(1)       TYPE c,
    dg_verificador_ag_conta(1)    TYPE c,
    nome_empresa(30)              TYPE c,
    nome_banco(30)                TYPE c,
    cnab1(10)                     TYPE c,
    cod_remessa_retorno(1)        TYPE c,
    data_geracao_arquivo(8)       TYPE c,
    hora_geracao_arquivo(6)       TYPE c,
    num_sequencial_arquivo(6)     TYPE c,
    num_vs_layout_arquivo(3)      TYPE c,
    densidade_gravacao_arquivo(5) TYPE c,
    reservado_banco(20)           TYPE c,
    reservado_empresa(20)         TYPE c,
    cnab2(29)                     TYPE c,
  END OF ty_header_arquivo,

  BEGIN OF ty_header_lote,
    cod_banco_compensacao(3)   TYPE c,
    lote_servico(4)            TYPE c,
    tipo_registro(1)           TYPE c,
    tipo_operacao(1)           TYPE c,
    tipo_servico(2)            TYPE c,
    forma_lancamento(2)        TYPE c,
    num_vs_layout(3)           TYPE c,
    cnab(1)                    TYPE c,
    tipo_inscricao_empresa(1)  TYPE c,
    num_inscricao_empresa(14)  TYPE c,
*
*   cod_convenio_banco(20)     TYPE c,
    cod_convenio_banco(9)      TYPE c,
    cod_convenio_banco2(4)     TYPE c,
    cod_convenio_banco3(7)     TYPE c,
*
    agencia_convenio(5)        TYPE c,
    dig_verif_agencia(1)       TYPE c,
    num_conta_corrrente(12)    TYPE c,
    dig_verif_conta(1)         TYPE c,
    dig_verif_ag_conta(1)      TYPE c,
    nome_empresa(30)           TYPE c,
    mensagem(40)               TYPE c,
    logradouro(30)             TYPE c,
    numero_local(5)            TYPE c,
    complemento(15)            TYPE c,
    cidade(20)                 TYPE c,
    cep(5)                     TYPE c,
    complemento_cep(3)         TYPE c,
    estado(2)                  TYPE c,
    "CNAB_2_1(8)                TYPE C,
    cnab_2(6)                  TYPE c,
    indic_forma_pag(2)         TYPE c,
    cod_ocorrencia_retorno(10) TYPE c,
  END OF ty_header_lote,

*** PBI - 43975 - Inicio
  BEGIN OF ty_header_lote_bbd,
    cod_banco_compensacao(3)   TYPE c,
    lote_servico(4)            TYPE c,
    tipo_registro(1)           TYPE c,
    tipo_operacao(1)           TYPE c,
    tipo_servico(2)            TYPE c,
    forma_lancamento(2)        TYPE c,
    num_vs_layout(3)           TYPE c,
    cnab(1)                    TYPE c,
    tipo_inscricao_empresa(1)  TYPE c,
    num_inscricao_empresa(14)  TYPE c,
*
*   cod_convenio_banco(20)     TYPE c,
    cod_convenio_banco(9)      TYPE c,
    cod_convenio_banco2(4)     TYPE c,
    cod_convenio_banco3(7)     TYPE c,
*
    agencia_convenio(5)        TYPE c,
    dig_verif_agencia(1)       TYPE c,
    num_conta_corrrente(12)    TYPE c,
    dig_verif_conta(1)         TYPE c,
    dig_verif_ag_conta(1)      TYPE c,
    nome_empresa(30)           TYPE c,
    mensagem(40)               TYPE c,
    logradouro(30)             TYPE c,
    numero_local(5)            TYPE c,
    complemento(15)            TYPE c,
    cidade(20)                 TYPE c,
    cep(5)                     TYPE c,
    complemento_cep(3)         TYPE c,
    estado(2)                  TYPE c,
    indic_forma_pag(2)         TYPE c,
    cnab_2(6)                  TYPE c,
    cod_ocorrencia_retorno(10) TYPE c,
  END OF ty_header_lote_bbd,
*** PBI - Fim

  BEGIN OF ty_segmento_n,
    cod_banco(3)                     TYPE c,
    lote_servico(4)                  TYPE c,
    registro_detalhe_lote(1)         TYPE c,
    sequencial_reg_lote(5)           TYPE c,
    cod_seg_reg_detalhe(1)           TYPE c,
    tipo_movimento(1)                TYPE c,
    cod_instrucao_movimento(2)       TYPE c,
    num_docto_atribuido_emprresa(20) TYPE c,
    num_docto_atribuido_banco(20)    TYPE c,
    nome_contribuinte(30)            TYPE c,
    data_pagamento(8)                TYPE c,
    valor_total_pagar(15)            TYPE c,
    informacoes_complementares(120)  TYPE c,
    cod_ocorrencia_retorno(10)       TYPE c,

  END OF ty_segmento_n,

  BEGIN OF ty_segmento_o,
    cod_banco(3)               TYPE c,
    lote_servico(4)            TYPE c,
    registro_detalhe_lote(1)   TYPE c,
    sequencial_reg_lote(5)     TYPE c,
    cod_seg_reg_detalhe(1)     TYPE c,
    tipo_movimento(1)          TYPE c,
    cod_instrucao_movimento(2) TYPE c,
    cod_barras(44)             TYPE c,
    nome_concessionaria(30)    TYPE c,
    data_vencimento(8)         TYPE c,
    data_pagamento(8)          TYPE c,
    valor_pagamento(15)        TYPE c,
    num_doc_empresa(20)        TYPE c,
    num_doc_banco(20)          TYPE c,
    cnab(68)                   TYPE c,
    cod_ocorrencia_retorno(10) TYPE c,
  END OF ty_segmento_o,

*-PBI 71420 - 12.01.2022 - JT - inicio
  BEGIN OF ty_segmento_j,
    cod_banco(3)               TYPE c,
    lote_servico(4)            TYPE c,
    registro_detalhe_lote(1)   TYPE c,
    sequencial_reg_lote(5)     TYPE c,
*
    cod_segmento(1)            TYPE c,
    tipo_movimento(1)          TYPE c,
    cod_instrucao_movimento(2) TYPE c,
    cod_barras(44)             TYPE c,
    nome_concessionaria(30)    TYPE c,
    data_vencimento(8)         TYPE c,
*
    valor_principal(15)        TYPE c,
    valor_desconto(15)         TYPE c,
    valor_juros_encargos(15)   TYPE c,
    data_pagamento(8)          TYPE c,
    valor_pagamento(15)        TYPE c,
    quantidade_moeda(15)       TYPE c,
    num_doc_empresa(20)        TYPE c,
    num_doc_banco(20)          TYPE c,
    cod_moeda(2)               TYPE c,
    cnab(6)                    TYPE c,
    cod_ocorrencia_retorno(10) TYPE c,
  END   OF ty_segmento_j,

  BEGIN OF ty_segmento_j52,
    cod_banco(3)             TYPE c,
    lote_servico(4)          TYPE c,
    registro_detalhe_lote(1) TYPE c,
    sequencial_reg_lote(5)   TYPE c,
    cod_segmento(1)          TYPE c,
    cnab(1)                  TYPE c,
    cod_mov_rem(2)           TYPE c,
    ident_reg_opcio(2)       TYPE c,
    tip_inscricao_pag(1)     TYPE c,
    num_inscricao_pag(15)    TYPE c,
    nome_pag(40)             TYPE c,
    tip_inscricao_ben(1)     TYPE c,
    num_inscricao_ben(15)    TYPE c,
    nome_ben(40)             TYPE c,
    tip_inscricao_sac(1)     TYPE c,
    num_inscricao_sac(15)    TYPE c,
    nome_sac(40)             TYPE c,
    cnab2(53)                TYPE c,
  END   OF ty_segmento_j52,
*-PBI 71420 - 12.01.2022 - JT - fim

  BEGIN OF ty_segmento_j52_pix,
    cod_banco(3)             TYPE c,
    lote_servico(4)          TYPE c,
    registro_detalhe_lote(1) TYPE c,
    sequencial_reg_lote(5)   TYPE c,
    cod_segmento(1)          TYPE c,
    cnab(1)                  TYPE c,
    cod_mov_rem(2)           TYPE c,
    ident_reg_opcio(2)       TYPE c,
    tip_inscricao_pag(1)     TYPE c,
    num_inscricao_pag(15)    TYPE c,
    nome_pag(40)             TYPE c,
    tip_inscricao_ben(1)     TYPE c,
    num_inscricao_ben(15)    TYPE c,
    nome_ben(40)             TYPE c,
    url_chave_pix(79)        TYPE c,
    cod_qrcode(30)           TYPE c,
  END   OF ty_segmento_j52_pix,

  BEGIN OF ty_gps,
    cod_receita(6)             TYPE c,
    tipo_ident_contrib(2)      TYPE c,
    identif_contribuinte(14)   TYPE c,
    cod_identif_tribut(2)      TYPE c,
    mes_ano_comp(6)            TYPE c,
    valor_inss(15)             TYPE c,
    valor_outras_entidades(15) TYPE c,
    atualizacao_menetaria(15)  TYPE c,
    cnab(45)                   TYPE c,

  END OF ty_gps,

  BEGIN OF ty_darf,
    cod_receita(6)           TYPE c,
    tipo_ident_contrib(2)    TYPE c,
    identif_contribuinte(14) TYPE c,
    cod_identif_tribut(2)    TYPE c,
    periodo_apuracao(8)      TYPE c,
    num_referencia(17)       TYPE c,
    valor_principal(15)      TYPE c,
    valor_multa(15)          TYPE c,
    valor_juros_encargos(15) TYPE c,
    data_vencimento(8)       TYPE c,
    cnab(18)                 TYPE c,

  END OF   ty_darf,

  BEGIN OF ty_darf_simples,
    cod_receita(6)                    TYPE c,
    tipo_ident_contrib(2)             TYPE c,
    identif_contribuinte(14)          TYPE c,
    cod_identif_tribut(2)             TYPE c,
    periodo_apuracao(8)               TYPE c,
    valor_receita_bruta_acumulada(15) TYPE c,
    perc_sb_receita_brut_acum(7)      TYPE c,
    valor_principal(15)               TYPE c,
    valor_multa(15)                   TYPE c,
    valor_juros_encargos(15)          TYPE c,
    cnab(21)                          TYPE c,

  END OF ty_darf_simples,

  BEGIN OF ty_gare_sp,
    cod_receita(6)           TYPE c,
    tipo_ident_contrib(2)    TYPE c,
    identif_contribuinte(14) TYPE c,
    cod_identif_tribut(2)    TYPE c,
    data_vencimento(8)       TYPE c,
    inscricao_estadual(12)   TYPE c,
    divida_ativa(13)         TYPE c,
    periodo_referencia(6)    TYPE c,
    num_parcela(13)          TYPE c,
    valor_receita(15)        TYPE c,
    valor_juros_encargos(14) TYPE c,
    valor_multa(14)          TYPE c,
    cnab(1)                  TYPE c,

  END OF ty_gare_sp,

  BEGIN OF ty_ipva,
    cod_receita(6)           TYPE c,
    tipo_ident_contrib(2)    TYPE c,
    identif_contribuinte(14) TYPE c,
    cod_identif_tribut(2)    TYPE c,
    ano_base(4)              TYPE c,
    cod_renavam(9)           TYPE c,
    unid_federacao(2)        TYPE c,
    cod_municipio(5)         TYPE c,
    placa_veiculo(7)         TYPE c,
    opcao_pagamento(1)       TYPE c,
    cnab(68)                 TYPE c,

  END OF ty_ipva,

  BEGIN OF ty_dpvat,
    cod_receita(6)           TYPE c,
    tipo_ident_contrib(2)    TYPE c,
    identif_contribuinte(14) TYPE c,
    cod_identif_tribut(2)    TYPE c,
    ano_base(4)              TYPE c,
    cod_renavam(9)           TYPE c,
    unid_federacao(2)        TYPE c,
    cod_municipio(5)         TYPE c,
    placa_veiculo(7)         TYPE c,
    opcao_pagamento(1)       TYPE c,
    cnab(68)                 TYPE c,

  END OF ty_dpvat,

  BEGIN OF ty_licenciamento,
    cod_receita(6)           TYPE c,
    tipo_ident_contrib(2)    TYPE c,
    identif_contribuinte(14) TYPE c,
    cod_identif_tribut(2)    TYPE c,
    ano_base(4)              TYPE c,
    cod_renavam(9)           TYPE c,
    unid_federacao(2)        TYPE c,
    cod_municipio(5)         TYPE c,
    placa_veiculo(7)         TYPE c,
    opcao_pagamento(1)       TYPE c,
    opcao_retirada_crvl(1)   TYPE c,
    cnab(67)                 TYPE c,
  END OF ty_licenciamento,

  BEGIN OF ty_pix,
    cod_receita(6)           TYPE c,
    tipo_ident_contrib(2)    TYPE c,
    identif_contribuinte(14) TYPE c,
    cod_identif_tribut(2)    TYPE c,
    ano_base(4)              TYPE c,
    cod_renavam(9)           TYPE c,
    unid_federacao(2)        TYPE c,
    cod_municipio(5)         TYPE c,
    placa_veiculo(7)         TYPE c,
    opcao_pagamento(1)       TYPE c,
    opcao_retirada_crvl(1)   TYPE c,
    cnab(67)                 TYPE c,

  END OF ty_pix,



  BEGIN OF ty_darj,
    cod_receita(6)           TYPE c,
    tipo_ident_contrib(2)    TYPE c,
    identif_contribuinte(14) TYPE c,
    inscricao_estadual(8)    TYPE c,
    num_doc_origem(16)       TYPE c,
    valor_principal(15)      TYPE c,
    val_at_monet(15)         TYPE c,
    valor_mora(15)           TYPE c,
    valor_multa(15)          TYPE c,
    data_vencimento(8)       TYPE c,
    periodo_parcela(6)       TYPE c,
  END OF ty_darj,

  BEGIN OF ty_segmento_w,
    cod_banco(3)                      TYPE c,
    lote_servico(4)                   TYPE c,
    reg_detalhe_lote(1)               TYPE c,
    num_seq_reg_lote(5)               TYPE c,
    cod_seg_reg_detalhe(1)            TYPE c,
    num_seq_reg_complementar(1)       TYPE c,
    uso_informacoes_complementares(1) TYPE c,
    informacao_complementar_1(80)     TYPE c,
    informacao_complementar_2(80)     TYPE c,
    identificador_tributo(2)          TYPE c,
    cnpj_branco(06)                   TYPE c,
    cnpj(18)                          TYPE c,
    info_complementar_tributo(46)     TYPE c,
    cnab(2)                           TYPE c,
    cod_ocorrencias_retorno(10)       TYPE c,

  END OF ty_segmento_w,

*** PBI - 43975 - Inicio
  BEGIN OF ty_bbd_segmento_w,
    cod_banco(3)                      TYPE c,
    lote_servico(4)                   TYPE c,
    reg_detalhe_lote(1)               TYPE c,
    num_seq_reg_lote(5)               TYPE c,
    cod_seg_reg_detalhe(1)            TYPE c,
    num_seq_reg_complementar(1)       TYPE c,
    uso_informacoes_complementares(1) TYPE c,
    informacao_complementar_1(80)     TYPE c,
    informacao_complementar_2(80)     TYPE c,
    identificador_tributo(2)          TYPE c,
    cnpj_branco(06)                   TYPE c,
    cnpj(18)                          TYPE c,
    info_complementar_tributo(48)     TYPE c,
    cnab(2)                           TYPE c,
    cod_ocorrencias_retorno(10)       TYPE c,
  END OF ty_bbd_segmento_w,

  BEGIN OF ty_fgts,
    identificador_tributo(2)         TYPE c,
    cod_receita(6)                   TYPE c,
    tipo_ident_contribuinte(2)       TYPE c,
    identificador_contribuinte(14)   TYPE c,
    identificador_fgts(16)           TYPE c,
    lacre_conectividade_social(9)    TYPE c,
    dg_lacre_concetividade_social(2) TYPE c,

  END OF ty_fgts,

  BEGIN OF ty_trailer_arquivo,
    cod_banco(3)             TYPE c,
    lote_servico(4)          TYPE c,
    tipo_registro(1)         TYPE c,
    cnab(9)                  TYPE c,
    qtd_lote(6)              TYPE c,
    qtd_registros_arquivo(6) TYPE c,
    qtde_contas_para_conc(6) TYPE c,
    cnab2(205)               TYPE c,

  END OF ty_trailer_arquivo,

  BEGIN OF ty_trailer_lote,
    cod_banco(3)                     TYPE c,
    lote_servico(4)                  TYPE c,
    tipo_registro(1)                 TYPE c,
    canab(9)                         TYPE c,
    qtd_registro_lote(6)             TYPE c,
    somatoria_valores_pgtos(18)      TYPE c,
    complemento_registro(189)        TYPE c,
    cod_ocorrencias_para_retorno(10) TYPE c,

  END OF ty_trailer_lote,

  BEGIN OF ty_zimp_layout_imp,
    tp_imposto  TYPE zimp_layout_imp-tp_imposto,
    campolayout TYPE zimp_layout_imp-campolayout,
    camposabert TYPE zimp_layout_imp-camposabert,
  END OF ty_zimp_layout_imp,
  ty_arquivo(463) TYPE c,

  BEGIN OF ty_zib_contabil_chv,
    obj_key TYPE zib_contabil_chv-obj_key,
    belnr   TYPE zib_contabil_chv-belnr,
    bukrs   TYPE zib_contabil_chv-bukrs,
    gjahr   TYPE zib_contabil_chv-gjahr,
  END OF ty_zib_contabil_chv,

  BEGIN OF ty_j_1bbranch,
    bukrs      TYPE j_1bbranch-bukrs,
    branch     TYPE j_1bbranch-branch,
    state_insc TYPE j_1bbranch-state_insc,
    name       TYPE j_1bbranch-name,
  END OF ty_j_1bbranch.


*-----------------------------------------------------------------------
* Estruturas
*-----------------------------------------------------------------------
DATA:

  w_arquivo          TYPE ty_arquivo,
  p_unix_old         TYPE char1,
  w_header_arquivo   TYPE ty_header_arquivo,
  w_segmento_n       TYPE ty_segmento_n,
  w_gps              TYPE ty_gps,
  w_darf             TYPE ty_darf,
  w_darf_simples     TYPE ty_darf_simples,
  w_gare_sp          TYPE ty_gare_sp,
  w_ipva             TYPE ty_ipva,
  w_dpvat            TYPE ty_dpvat,
  w_licenciamento    TYPE ty_licenciamento,
  w_pagamento_pix    TYPE ty_pix,
  w_darj             TYPE ty_darj,
  w_segmento_w       TYPE ty_segmento_w,
  gru_seg_w          TYPE ty_estseg_w,
  w_bbd_segmento_w   TYPE ty_bbd_segmento_w,
  w_segmento_o       TYPE ty_segmento_o,
  w_segmento_j       TYPE ty_segmento_j,
  w_segmento_j52     TYPE ty_segmento_j52,
  w_segmento_j52_pix TYPE ty_segmento_j52_pix,
  w_fgts             TYPE ty_fgts,
  w_header_lote      TYPE ty_header_lote,
  w_header_lote_bbd  TYPE ty_header_lote_bbd,
  w_trailer_arquivo  TYPE ty_trailer_arquivo,
  w_trailer_lote     TYPE ty_trailer_lote,
  w_zimp_layout_imp  TYPE ty_zimp_layout_imp,
  w_adrc             TYPE adrc,
  w_t012k            TYPE t012k,
  w_j_1bbranch       TYPE ty_j_1bbranch,
  codigo_banco(5)    TYPE c.

*-----------------------------------------------------------------------
* Tabelas internas
*-----------------------------------------------------------------------
DATA:

  t_arquivo            TYPE TABLE OF ty_arquivo,
  t_arquivo1           TYPE TABLE OF ty_arquivo,
  t_arquivo2           TYPE TABLE OF ty_arquivo,
  t_arquivo3           TYPE TABLE OF ty_arquivo,
  t_arquivo4           TYPE TABLE OF ty_arquivo,
  t_arquivo5           TYPE TABLE OF ty_arquivo,
  t_arquivo6           TYPE TABLE OF ty_arquivo,
  t_arquivo7           TYPE TABLE OF ty_arquivo,
  t_arquivo8           TYPE TABLE OF ty_arquivo,
  t_arquivo9           TYPE TABLE OF ty_arquivo,
  t_arquivo10          TYPE TABLE OF ty_arquivo,
  t_arquivo11          TYPE TABLE OF ty_arquivo,
  t_arquivo12          TYPE TABLE OF ty_arquivo,
  t_arquivo13          TYPE TABLE OF ty_arquivo,
  t_zimp_layout_imp    TYPE TABLE OF ty_zimp_layout_imp,

  t_zimp_cabecalho     TYPE TABLE OF zimp_lanc_impost,
  t_zimp_cabecalho_aux TYPE TABLE OF zimp_lanc_impost,
  t_zimp_cabecalho_2   TYPE TABLE OF zimp_lanc_impost,
  t_zimp_cabecalho_3   TYPE TABLE OF zimp_lanc_impost,
  t_zimp_cabecalho_pix TYPE TABLE OF zimp_lanc_impost,
  aux_zimp_contas_cons TYPE TABLE OF zimp_lanc_impost,
  w_zimp_cabecalho     TYPE          zimp_lanc_impost,
  t_zimp_detalhe       TYPE TABLE OF zimp_lanc_imp_ct,
  aux_zimp_detalhe     TYPE TABLE OF zimp_lanc_imp_ct,
  t_j_1bbranch         TYPE TABLE OF ty_j_1bbranch.


*-----------------------------------------------------------------------
* Símbolos de campo
*-----------------------------------------------------------------------
FIELD-SYMBOLS:

  <f_zimp_cabecalho> TYPE zimp_lanc_impost,
  "<f_zimp_cabecalho_pix> TYPE zimp_lanc_impost,
  <f_zimp_detalhe>   TYPE zimp_lanc_imp_ct,
  <fs_campo>         TYPE any,
  <wa_data>          TYPE any.

*-----------------------------------------------------------------------
* Variáveis
*-----------------------------------------------------------------------
DATA:

  v_butxt             TYPE t001-butxt,
  xobj_key            TYPE zib_contabil-obj_key,
  v_regs_proc         TYPE i,
  v_arqs_proc         TYPE i,
  vlifnr              TYPE zimp_lanc_imp_ct-lifnr,
  vcgc(15),
  conv_banco          TYPE  t045t-dtaid,
  lote_servico        TYPE i,
  sequencial_reg_lote TYPE i,
  filial_lanc         TYPE j_1bbranch-branch,
  v_toal_Reg(6). "Analisar o tipo 10 - ISS em forma de boleto #180566 - BG

TYPES: BEGIN OF ty_log,
         bldat     TYPE bkpf-bldat,
         blart     TYPE bkpf-blart,
         bukrs     TYPE ekko-bukrs,
         budat     TYPE bkpf-budat,
         waers     TYPE ekko-waers,
         konto     TYPE rf05a-konto,
         hbkid     TYPE t012k-hbkid,
         gsber     TYPE ekpo-werks,
         wrbtr(16) TYPE c,
         sel01     TYPE rf05a-sel01,
         agkon     TYPE rf05a-agkon,
         tipo(1)   TYPE c,
         msg(100)  TYPE c,
       END OF ty_log.

DATA: BEGIN OF t_batch OCCURS 0,
        bldat     TYPE bkpf-bldat,
        blart     TYPE bkpf-blart,
        bukrs     TYPE ekko-bukrs,
        budat     TYPE bkpf-budat,
        waers     TYPE ekko-waers,
        konto     TYPE rf05a-konto,
        hbkid     TYPE t012k-hbkid,
        gsber     TYPE ekpo-werks,
        wrbtr(16) TYPE c,
        dmbtr(16) TYPE c,
        sel01     TYPE rf05a-sel01,
        agkon     TYPE rf05a-agkon,
        data(10)  TYPE c,
        belnr     TYPE bkpf-belnr,
        gjahr     TYPE bkpf-gjahr,
      END OF t_batch.

DATA: ti_log TYPE STANDARD TABLE OF ty_log,
      ti_bdc TYPE STANDARD TABLE OF bdcdata,
      ti_msg TYPE STANDARD TABLE OF bdcmsgcoll.

DATA: wa_log LIKE LINE OF ti_log,
      wa_bdc LIKE LINE OF ti_bdc,
      wa_msg LIKE LINE OF ti_msg.

DATA: v_msg           LIKE t100-text,
      v_budat         LIKE bkpf-budat,
      v_msgv1         LIKE balm-msgv1,
      v_msgv2         LIKE balm-msgv2,
      v_msgv3         LIKE balm-msgv3,
      v_msgv4         LIKE balm-msgv4,
      v_mode          TYPE c VALUE 'N',
      v_tit           TYPE zimp_lanc_imp_ct-valor_imp,
      v_tit_f         TYPE zimp_lanc_imp_ct-valor_imp,
      v_tot_tit       TYPE zimp_lanc_imp_ct-valor_imp,
      lv_nome_arquivo TYPE string,
      v_segmento      TYPE char1,
      identificador   TYPE zimp_lanc_impost-identificador,
      proximo(4)      TYPE c.

DATA: wa_zib_contabil_chv TYPE zib_contabil_chv,
      wa_bkpf             TYPE bkpf,
      t_tvarvc            TYPE TABLE OF tvarvc.

DATA: s_tparrc_pix TYPE RANGE OF string.

INCLUDE: zimp61_itau.
"INCLUDE: zimp61_pix.
DATA vobj_key TYPE zib_contabil_err-obj_key.

DATA v_codBanco(4). "Analisar o tipo 10 - ISS em forma de boleto #180566 - BG

*-----------------------------------------------------------------------
* Parâmetros de seleção
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-t01.

  PARAMETERS:
    p_bncemp                      TYPE t012-hbkid DEFAULT 'BBRA'.

  SELECT-OPTIONS:
    s_nr_doc                      FOR <f_zimp_detalhe>-doc_imposto,
    s_lifnr                       FOR <f_zimp_detalhe>-lifnr.

  PARAMETERS:
    p_emp                         TYPE b120-bukrs
                                  OBLIGATORY.

  SELECT-OPTIONS:
    s_dtlanc                      FOR wa_bkpf-budat,

    s_dtvenc                      FOR <f_zimp_cabecalho>-dt_venc OBLIGATORY,
    s_tparrc                      FOR zimp_tipos_impos-tp_arrec
                                  NO INTERVALS OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-t02.
  PARAMETERS: p_pc   TYPE char1 RADIOBUTTON GROUP rgr1 DEFAULT 'X' USER-COMMAND usr,
              p_unix TYPE char1 RADIOBUTTON GROUP rgr1.

  PARAMETERS: p_path(250) DEFAULT 'C:\' LOWER CASE.
SELECTION-SCREEN END OF BLOCK b02.

AT SELECTION-SCREEN.
  IF NOT s_dtvenc IS INITIAL.
    LOOP AT s_dtvenc.
      IF s_dtvenc-low < sy-datum.
        MESSAGE i398(00) WITH 'Data de vencimento dos titulos'
                              'menor que data atual'.
        STOP.
      ENDIF.
    ENDLOOP.
  ENDIF.


**
AT SELECTION-SCREEN OUTPUT.
  IF p_unix_old <> p_unix.
    p_unix_old = p_unix.
    IF p_pc = 'X'.
      p_path = 'C:\'.
    ELSE.
      p_path = '/sap/usr/'.
    ENDIF.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  IF p_pc = 'X'.
    CALL FUNCTION 'WS_FILENAME_GET'
      EXPORTING
        def_filename     = ' '
        def_path         = 'C:\'
        mask             = '*.TXT'
        mode             = 'S'
        title            = 'Busca de Arquivo'
      IMPORTING
        filename         = p_path
      EXCEPTIONS
        inv_winsys       = 1
        no_batch         = 2
        selection_cancel = 3
        selection_error  = 4
        OTHERS           = 5.
  ELSE.
    DATA: wl_path TYPE dxlpath.
    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
      EXPORTING
        i_location_flag = ' '
*       i_server        = lv_servername
        i_path          = '//'
        filemask        = '*.*'
        fileoperation   = 'R'
      IMPORTING
*       O_LOCATION_FLAG =
*       O_SERVER        =
        o_path          = wl_path
*       ABEND_FLAG      =
      EXCEPTIONS
        rfc_error       = 1
        OTHERS          = 2.
    MOVE wl_path TO p_path.

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tparrc-low.
  PERFORM match_code_tparrc.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM zf_carrega_tabelas.

  IF t_zimp_detalhe[] IS INITIAL OR t_zimp_cabecalho[] IS INITIAL.
    MESSAGE  'Não foram encontrados dados para os parâmetros informados.' TYPE 'I'.
    RETURN.
  ENDIF.

*  US - 128395 - CBRAND - Inicio
  SELECT * FROM  zimp_tipos_impos
    INTO TABLE @DATA(t_zimp_tipos_impos)
    FOR ALL ENTRIES IN @t_zimp_cabecalho
  WHERE tp_arrec EQ @t_zimp_cabecalho-tp_imposto
    AND qrcode = 'X'.

  SORT t_zimp_tipos_impos BY tp_arrec.

  DELETE ADJACENT DUPLICATES FROM t_zimp_tipos_impos COMPARING tp_arrec.

  IF t_zimp_tipos_impos IS NOT INITIAL.

    s_tparrc_pix = VALUE #(
      FOR <ls_zimp_tipos_impos> IN t_zimp_tipos_impos
      ( sign = 'I'
        option = 'EQ'
        low = <ls_zimp_tipos_impos>-tp_arrec )
    ).

    t_zimp_cabecalho_pix[] = t_zimp_cabecalho[].

    DELETE t_zimp_cabecalho    WHERE tp_imposto IN s_tparrc_pix.
    DELETE t_zimp_cabecalho_pix WHERE tp_imposto NOT IN s_tparrc_pix.

    PERFORM zf_monta_arquivo_pix USING p_bncemp
                                       p_emp.

  ENDIF.
*  US - 128395 - CBRAND - Fim

*  US - 128395 - CBRAND - Inicio
  IF t_zimp_cabecalho IS NOT INITIAL.
*  US - 128395 - CBRAND - Fim
    IF p_bncemp(2) = 'IT'. "Itaú
      PERFORM zf_monta_arquivo_itau.
    ELSE.
*** BUG - 184742 - CBRAND - Inicio
***PERFORM zf_monta_arquivo.
*** ( Fiz uma cópia porém o form original zf_monta_arquivo ja tinha modificações
*** referente ao CS - #180566 - BG  ) CBRAND
      PERFORM zf_monta_arquivo_new.
*** BUG - 184742 - CBRAND - FIM
    ENDIF.
  ENDIF.

  PERFORM zf_informacao.

  PERFORM zf_batch_input. "BUG - 71976 - CSB

FORM zf_carrega_tabelas .

  SELECT  zimp_lanc_impost~mandt
        zimp_lanc_impost~doc_imposto
        zimp_lanc_impost~bukrs
        zimp_lanc_impost~lote
        zimp_lanc_impost~dt_venc
        zimp_lanc_impost~dt_apuracao
        zimp_lanc_impost~mes_apuracao
        zimp_lanc_impost~ano_apuracao
        zimp_lanc_impost~observacao
        zimp_lanc_impost~cod_imposto
        zimp_lanc_impost~ref_imposto
        zimp_lanc_impost~tp_imposto
        zimp_lanc_impost~cod_pgto
        zimp_lanc_impost~conv_banco
        zimp_lanc_impost~hbkid
        zimp_lanc_impost~gsber
        zimp_lanc_impost~waers
        zimp_lanc_impost~waers_f
        zimp_lanc_impost~identificador
        zimp_lanc_impost~cod_barras
        zimp_lanc_impost~qrcode
        zimp_lanc_impost~data_atual
        zimp_lanc_impost~hora_atual
        zimp_lanc_impost~usuario
        zimp_lanc_impost~loekz
*Inicio Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779
        zimp_lanc_impost~moeda_gp_hist
        zimp_lanc_impost~st_fecha
        zimp_lanc_impost~kostl
        zimp_lanc_impost~zimp_lanc_impost
*Fim Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779
  INTO TABLE t_zimp_cabecalho
  FROM zimp_lanc_impost
  INNER JOIN zimp_cad_lote
  ON zimp_cad_lote~lote = zimp_lanc_impost~lote
  AND zimp_cad_lote~status_lote = 'A'
  WHERE zimp_lanc_impost~bukrs       = p_emp    AND
        zimp_lanc_impost~doc_imposto IN s_nr_doc AND
        zimp_lanc_impost~dt_venc     IN s_dtvenc AND
        zimp_lanc_impost~tp_imposto  IN s_tparrc AND
        zimp_lanc_impost~hbkid  = p_bncemp AND
        zimp_lanc_impost~loekz  NE 'X'.

  IF s_tparrc-low = '08'. "119896 AJUSTE TRANSAÇÃO PARA PAGAMENTOS DE CONSUMO - PSA

    SELECT DISTINCT
     a~mandt
    ,CAST( a~belnr AS NUMC ) AS doc_imposto
    ,a~bukrs
    ,b~lote
    ,a~zfbdt AS dt_venc
    ,a~budat AS dt_apuracao
    ,CAST( '00' AS NUMC ) AS mes_apuracao
    ,a~gjahr AS ano_apuracao
    ,b~sgtxt AS observacao
    ,CAST( '00788' AS NUMC ) AS cod_imposto
    ,'ZGL059' AS ref_imposto
    ,'08' AS tp_imposto
    ,'0000' AS cod_pgto
    ,'X' AS conv_banco
    ,c~hbkid
    ,b~gsber
    ,' ' AS waers
    ,'00000' AS waers_f
    ,'0000000000000000' AS identificador
    ,a~cod_barras AS cod_barras
    ,' ' AS qrcode
    ,c~dt_lcto AS data_atual
    ,c~hr_lcto AS hora_atual
    ,c~usnam AS usuario
    ,' ' AS loekz
    ,'0' AS moeda_gp_hist
    ,'0' AS st_fecha
    ,'0000000000' AS kostl
    FROM zimp_contas_cons AS a
    INNER JOIN zglt081 AS b ON b~belnr  = a~belnr
    INNER JOIN zglt080 AS c ON c~seq_lcto = b~seq_lcto
    "INNER JOIN bsik AS d ON d~belnr = a~belnr
    WHERE a~bukrs = @p_emp
    AND c~doc_lcto IN @s_nr_doc
    AND a~zfbdt IN @s_dtvenc
    AND c~hbkid  = @p_bncemp
    AND b~bschl = 31
    "AND d~augbl = ' '
    INTO TABLE @aux_zimp_contas_cons.


    LOOP AT aux_zimp_contas_cons ASSIGNING FIELD-SYMBOL(<aux_zimp_contas_cons>).
      DATA aux_lifnr TYPE lfa1-lifnr.
      UNPACK <aux_zimp_contas_cons>-gsber TO aux_lifnr.
      SELECT SINGLE stcd1 FROM lfa1 INTO <aux_zimp_contas_cons>-identificador WHERE  lifnr = aux_lifnr.
    ENDLOOP.

    APPEND LINES OF aux_zimp_contas_cons[] TO t_zimp_cabecalho.

  ENDIF.

  "IF sy-subrc IS INITIAL.

**  Begin of CS2022000833 #97834 FF   05.12.2022
  SELECT * FROM tvarvc
    INTO TABLE t_tvarvc
    WHERE name LIKE 'MAGGI_ZIMP62_CONVENIO%'.
  IF sy-subrc <> 0.
    CLEAR t_tvarvc.
  ENDIF.
** End of FF  05.12.2022 21:29:08

  SORT t_zimp_cabecalho BY doc_imposto.

  SELECT *
    INTO TABLE t_zimp_detalhe
    FROM zimp_lanc_imp_ct
    FOR ALL ENTRIES IN t_zimp_cabecalho
    WHERE doc_imposto = t_zimp_cabecalho-doc_imposto
    AND   bukrs       = t_zimp_cabecalho-bukrs.

  IF s_tparrc-low = '08'. "119896 AJUSTE TRANSAÇÃO PARA PAGAMENTOS DE CONSUMO - PSA

    SELECT DISTINCT
    a~mandt,
    CAST( a~belnr AS NUMC ) AS doc_imposto,
    CAST( '00788' AS NUMC ) AS cod_imposto,
     '11' AS cod_abertura,
     a~bukrs,
     CAST( '000001' AS NUMC ) AS seqitem,
     b~bschl,
     ' ' AS umskz,
     ' ' AS hkont,
     a~lifnr,
     ' ' AS kunnr,
     ' ' AS kostl,
     ' ' AS prctr,
     ' ' AS aufnr,
     b~gsber,
     b~matnr,
      CAST( b~netwr AS CURR ) AS valor_imp,
     CAST( b~netwr AS CURR ) AS valor_for,
     c~dt_lcto AS data_atual,
     c~hr_lcto AS hora_atual,
     c~usnam AS usuario,
     CAST( b~netwr AS CURR ) AS vlr_moeda_doc
    FROM zimp_contas_cons AS a
    INNER JOIN zglt081 AS b ON b~belnr  = a~belnr
    INNER JOIN zglt080 AS c ON c~seq_lcto = b~seq_lcto
    "INNER JOIN bsik AS d ON d~belnr = a~belnr
    WHERE a~bukrs = @p_emp
    AND c~doc_lcto IN @s_nr_doc
    AND a~zfbdt IN @s_dtvenc
    AND c~hbkid  = @p_bncemp
    "AND d~augbl = ' '
    AND b~bschl = 31

      UNION ALL

            SELECT DISTINCT
    a~mandt,
    CAST( a~belnr AS NUMC ) AS doc_imposto,
    CAST( '00788' AS NUMC ) AS cod_imposto,
     '01' AS cod_abertura,
     a~bukrs,
     CAST( '000001' AS NUMC ) AS seqitem,
     b~bschl,
     ' ' AS umskz,
     b~hkont,
     ' ' AS lifnr,
     ' ' AS kunnr,
     b~kostl,
     ' ' AS prctr,
     ' ' AS aufnr,
     b~gsber,
     b~matnr,
     SUM( CAST( b~netwr AS CURR ) ) AS valor_imp,
     SUM( CAST( b~netwr AS CURR ) ) AS valor_for,
     c~dt_lcto AS data_atual,
     c~hr_lcto AS hora_atual,
     c~usnam AS usuario,
     SUM( CAST( b~netwr AS CURR ) ) AS vlr_moeda_doc
    FROM zimp_contas_cons AS a
    INNER JOIN zglt081 AS b ON b~belnr  = a~belnr
    INNER JOIN zglt080 AS c ON c~seq_lcto = b~seq_lcto
    "INNER JOIN bsik AS d ON d~belnr = a~belnr
    WHERE a~bukrs = @p_emp
    AND c~doc_lcto IN @s_nr_doc
    AND a~zfbdt IN @s_dtvenc
    AND c~hbkid  = @p_bncemp
    "AND d~augbl = ' '
    AND b~bschl = 40

    GROUP BY a~mandt,
     a~belnr,
     a~bukrs,
     b~bschl,
     b~hkont,
     b~kostl,
     b~gsber,
     b~matnr,
     c~dt_lcto,
     c~hr_lcto,
     c~usnam

    INTO TABLE @aux_zimp_detalhe.


    SORT aux_zimp_detalhe BY doc_imposto cod_abertura DESCENDING.

    "DELETE ADJACENT DUPLICATES FROM aux_zimp_detalhe.

    APPEND LINES OF aux_zimp_detalhe[] TO t_zimp_detalhe.

  ENDIF.


  SELECT
    j_1bbranch~bukrs
    j_1bbranch~branch
    j_1bbranch~state_insc
    j_1bbranch~name
    FROM j_1bbranch
    INTO TABLE t_j_1bbranch
    FOR ALL ENTRIES IN t_zimp_cabecalho
  WHERE bukrs = t_zimp_cabecalho-bukrs
    AND ( branch = t_zimp_cabecalho-gsber OR branch = '0001' ).

  SELECT tp_imposto campolayout camposabert
    FROM zimp_layout_imp
    INTO TABLE t_zimp_layout_imp.

  IF NOT  s_lifnr IS INITIAL.
    LOOP AT t_zimp_detalhe ASSIGNING <f_zimp_detalhe>.
      IF <f_zimp_detalhe>-lifnr IS NOT INITIAL.
        IF <f_zimp_detalhe>-lifnr NOT IN s_lifnr.
          READ TABLE t_zimp_cabecalho INTO w_zimp_cabecalho WITH KEY doc_imposto = <f_zimp_detalhe>-doc_imposto BINARY SEARCH.
          IF sy-subrc = 0.
            w_zimp_cabecalho-loekz = 'X'.
            MODIFY t_zimp_cabecalho FROM w_zimp_cabecalho INDEX  sy-tabix TRANSPORTING loekz.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.

    identificador = <f_zimp_cabecalho>-identificador.

    IF s_tparrc-low = '08' AND <f_zimp_cabecalho>-ref_imposto = 'ZGL059' . "119896 AJUSTE TRANSAÇÃO PARA PAGAMENTOS DE CONSUMO - PSA

      wa_zib_contabil_chv-bukrs = <f_zimp_cabecalho>-bukrs.
      wa_zib_contabil_chv-belnr = <f_zimp_cabecalho>-doc_imposto.
      wa_zib_contabil_chv-gjahr = <f_zimp_cabecalho>-ano_apuracao.
    ELSE.
      CONCATENATE 'ZP' <f_zimp_cabecalho>-bukrs <f_zimp_cabecalho>-doc_imposto '%' INTO vobj_key.
      SELECT SINGLE *
      FROM zib_contabil_chv
      INTO wa_zib_contabil_chv
      WHERE obj_key LIKE vobj_key.

      IF sy-subrc NE 0.
        CONCATENATE 'ZIMP' <f_zimp_cabecalho>-doc_imposto '%' INTO vobj_key.
        SELECT SINGLE *
        FROM zib_contabil_chv
        INTO wa_zib_contabil_chv
        WHERE obj_key LIKE vobj_key
        AND   bukrs   =  <f_zimp_cabecalho>-bukrs .

      ENDIF.
    ENDIF.

    IF sy-subrc = 0.

      SELECT SINGLE *
        FROM bkpf
        INTO wa_bkpf
        WHERE bukrs = wa_zib_contabil_chv-bukrs
        AND   belnr = wa_zib_contabil_chv-belnr
        AND   gjahr = wa_zib_contabil_chv-gjahr.


      IF sy-subrc = 0.
        IF s_dtlanc IS NOT INITIAL AND NOT wa_bkpf-budat IN s_dtlanc.
          MODIFY t_zimp_cabecalho FROM <f_zimp_cabecalho> INDEX  sy-tabix TRANSPORTING loekz.
        ENDIF.
      ENDIF.



    ELSE.
      <f_zimp_cabecalho>-loekz = 'X'.
      MODIFY t_zimp_cabecalho FROM <f_zimp_cabecalho> INDEX  sy-tabix TRANSPORTING loekz.
    ENDIF.




    PERFORM zf_codigo_banco USING codigo_banco.


    SELECT SINGLE dtaid
    INTO conv_banco
    FROM  t045t
    WHERE zlsch = 'E'
    AND bukrs = <f_zimp_cabecalho>-bukrs
    AND hbkid = p_bncemp.

  ENDLOOP.
  DELETE t_zimp_cabecalho WHERE loekz = 'X'.
  "DELETE t_zimp_cabecalho WHERE ref_imposto <> 'ZGL059'. "119896 AJUSTE TRANSAÇÃO PARA PAGAMENTOS DE CONSUMO - PSA


  "ELSE.
*    MESSAGE i208(00) WITH 'Não existe dados para esta seleção'.
*    STOP.
  "ENDIF.

ENDFORM.                    " ZF_CARREGA_TABELAS

FORM zf_monta_arquivo .

  CLEAR: v_tot_tit.
  DATA: v_tp_lanc(2).

  REFRESH t_arquivo.

*------------------------------
*-verifica qual segmento
*------------------------------
  CLEAR v_segmento.

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho> INDEX 1.
  "PERFORM f_tbatch. BUG - 71976 - CSB


*Inicio Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
***  IF <f_zimp_cabecalho>-tp_imposto EQ '07'.
  IF <f_zimp_cabecalho>-tp_imposto EQ '07' OR <f_zimp_cabecalho>-tp_imposto EQ '08'.
*Fim Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
    v_segmento = 'O'.
  ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'. "PSA se for GRU  -> adcionando o imposto 14 = GRU
    v_segmento = 'O'.
  ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- INICIO

*    CASE <f_zimp_cabecalho>-cod_barras+0(4).
*      WHEN '0019' OR '2379' OR '0339' OR '3419' OR '1049'.
*        v_segmento = 'J'.
*      WHEN OTHERS.
*        v_segmento = 'O'.
*    ENDCASE.
    CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
    SELECT SINGLE * FROM bnka INTO @DATA(wa_BNKA)
     WHERE bankl LIKE @v_codbanco.
    IF sy-subrc IS INITIAL.
      v_segmento = 'J'.
    ELSE.
      v_segmento = 'O'.
    ENDIF.

*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- FIM
  ELSE.
    v_segmento = 'N'.
  ENDIF.
*------------------------------
  CLEAR codigo_banco.
  PERFORM zf_codigo_banco      USING codigo_banco.
  SORT t_zimp_detalhe BY doc_imposto cod_abertura bukrs.
  lote_servico = 1.
  sequencial_reg_lote = 1.
  PERFORM prenche_header_arquivo.
  "PERFORM PREENCHE_HEADER_LOTE. "Analisar o tipo 10 - ISS em forma de boleto #180566 - BG

  APPEND w_header_arquivo TO t_arquivo1.

** BUG - 184742 - CBRAND - Inicio
  IF <f_zimp_cabecalho>-tp_imposto EQ '11' AND  p_bncemp = 'BBD'.
    LOOP AT t_zimp_cabecalho INTO DATA(w_zimp).
      CONCATENATE w_zimp-cod_barras+0(3) '%' INTO v_codbanco.
      SELECT SINGLE * FROM bnka INTO @DATA(w_bnka_aux)
            WHERE bankl LIKE @v_codbanco.
      IF sy-subrc IS INITIAL. "SE CODIGO BARRAS = COMECA COM BANCO SEGMENTO 'J' Se não é 'O'.
        APPEND w_zimp TO t_zimp_cabecalho_2.
      ELSE. "Se não é 'O'.
        APPEND w_zimp TO t_zimp_cabecalho_aux.
      ENDIF.
    ENDLOOP.
  ELSE.
** BUG - 184742 - CBRAND - FIM
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- INICIO
    LOOP AT t_zimp_cabecalho INTO w_zimp.
** BUG - 184742 - CBRAND - Inicio
*      IF codigo_banco NE w_zimp-cod_barras(3). "SE CODIGO BARRAS = COMECA COM BANCO SEGMENTO 'J'  Se não é 'O'.
*        APPEND w_zimp TO t_zimp_cabecalho_aux.
*      ELSE.
*        APPEND w_zimp TO t_zimp_cabecalho_2.
*      ENDIF.

      CONCATENATE w_zimp-cod_barras+0(3) '%' INTO v_codbanco.
      SELECT SINGLE * FROM bnka INTO @w_bnka_aux
            WHERE bankl LIKE @v_codbanco.
      IF sy-subrc IS INITIAL. "SE CODIGO BARRAS = COMECA COM BANCO SEGMENTO 'J' Se não é 'O'.
        APPEND w_zimp TO t_zimp_cabecalho_2.
      ELSE. "Se não é 'O'.
        APPEND w_zimp TO t_zimp_cabecalho_aux.
      ENDIF.
** BUG - 184742 - CBRAND - FIM

    ENDLOOP.
  ENDIF.

*** QUAL CABEÇALHO É 'O' E QUAL É 'J' ? - t_zimp_cabecalho_2 'J'
  IF t_zimp_cabecalho_2[] IS NOT INITIAL. "lote 30
    sequencial_reg_lote = 1.
** BUG - 184742 - CBRAND - INICIO
    PERFORM preenche_header_lote USING 'J'.
*    PERFORM preenche_header_lote USING '30'.
** BUG - 184742 - CBRAND - FIM
    IF p_bncemp = 'BBD'.
      sequencial_reg_lote = 0.
      MOVE-CORRESPONDING w_header_lote TO w_header_lote_bbd.
** BUG - 184742 - CBRAND - Inicio
      IF <f_zimp_cabecalho>-tp_imposto EQ '11' .
        w_header_lote_bbd-tipo_servico       =  '20'.
        w_header_lote_bbd-forma_lancamento   =  '31'.
        w_header_lote_bbd-num_vs_layout	     =  '040'.
      ENDIF.
** BUG - 184742 - CBRAND - Fim

      APPEND w_header_lote_bbd    TO t_arquivo1.
    ELSE.
      APPEND w_header_lote    TO t_arquivo1.
    ENDIF.

    LOOP AT t_zimp_cabecalho_2 ASSIGNING <f_zimp_cabecalho>.

      CASE <f_zimp_cabecalho>-tp_imposto.
        WHEN'01'. " darf simples
          PERFORM zf_monta_tp_darf_simples.
        WHEN'02'. " darf normal/preto
          PERFORM zf_monta_tp_darf.
        WHEN '03'.
          PERFORM zf_monta_tp_gps.
        WHEN '04' OR '05' OR '06'.
          PERFORM zf_monta_tp_gare.
        WHEN '07'.
          PERFORM zf_monta_tp_fgts.
          "WHEN'09'. " Validando DARF normal
          " PERFORM ZF_MONTA_TP_DARF.
        WHEN '13'.
          PERFORM zf_monta_tp_ipva.
          "WHEN '14'.
          "PERFORM  zf_monta_tp_dpvat.
        WHEN '15'.
          PERFORM zf_monta_tp_licenciamento.
        WHEN '16'.
          PERFORM zf_monta_tp_darj.
      ENDCASE.

      APPEND w_arquivo TO t_arquivo.

      v_regs_proc = v_regs_proc + 1.

      PERFORM f_tbatch.
      IF <f_zimp_cabecalho>-tp_imposto EQ '07'.
*      ajuste PL - CS2017002462 - Correções Banco do Brasil - CNAB - 21/09/2020 - inicio
        PERFORM preenche_segmento_o.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
*      ajuste PL - CS2017002462 - Correções Banco do Brasil - CNAB - 21/09/2020 - fim
        PERFORM preenche_segmento_w.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'."115457 GRU - CNAB - PSA  -> adcionando o imposto 14 = GRU
        PERFORM preenche_segmento_o_gru.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w_gru.
      ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- INICIO

        CLEAR: v_codbanco.
        CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
        SELECT SINGLE * FROM bnka INTO @DATA(w_BNKA)
         WHERE bankl LIKE @v_codbanco.
        IF sy-subrc IS INITIAL.
          PERFORM preenche_segmento_j.
          PERFORM preenche_segmento_j52.
        ELSE.
          PERFORM preenche_segmento_o.
        ENDIF.
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- FIM

*Inicio Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '08'.
        PERFORM preenche_segmento_o.
*Fim Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
      ELSE.
        PERFORM preenche_segmento_n.
      ENDIF.

      v_tot_tit = v_tot_tit + v_tit.
      "lote_servico = lote_servico + 1.

      sequencial_reg_lote = sequencial_reg_lote + 1.

    ENDLOOP.
*** BUG - 184742 - Inicio - CBRAND
    "PERFORM preenche_trailer_lote.
    PERFORM preenche_trailer_lote USING '0001'.
*** BUG - 184742 - Fim - CBRAND
    APPEND w_trailer_lote    TO t_arquivo1.
  ENDIF.

  IF t_zimp_cabecalho_aux[] IS NOT INITIAL. "lote 31
    sequencial_reg_lote = 1.
    lote_servico = 2.
    CLEAR: w_header_lote, v_tot_tit.
** BUG - 184742 - CBRAND - INICIO
    PERFORM preenche_header_lote USING 'O'.
    "PERFORM preenche_header_lote USING '31'.
** BUG - 184742 - CBRAND - FIM
    IF p_bncemp = 'BBD'.
      sequencial_reg_lote = 0.
      MOVE-CORRESPONDING w_header_lote TO w_header_lote_bbd.

** BUG - 184742 - CBRAND - Inicio
      IF <f_zimp_cabecalho>-tp_imposto EQ '11' .
        w_header_lote_bbd-tipo_servico       =  '22'.
        w_header_lote_bbd-forma_lancamento   =  '11'.
        w_header_lote_bbd-num_vs_layout	     =  '012'.
      ENDIF.
** BUG - 184742 - CBRANND - Fim

      APPEND w_header_lote_bbd    TO t_arquivo1.
    ELSE.
      APPEND w_header_lote    TO t_arquivo1.
    ENDIF.

    LOOP AT t_zimp_cabecalho_aux ASSIGNING <f_zimp_cabecalho>.

      CASE <f_zimp_cabecalho>-tp_imposto.
        WHEN'01'. " darf simples
          PERFORM zf_monta_tp_darf_simples.
        WHEN'02'. " darf normal/preto
          PERFORM zf_monta_tp_darf.
        WHEN '03'.
          PERFORM zf_monta_tp_gps.
        WHEN '04' OR '05' OR '06'.
          PERFORM zf_monta_tp_gare.
        WHEN '07'.
          PERFORM zf_monta_tp_fgts.
        WHEN '13'.
          PERFORM zf_monta_tp_ipva.
        WHEN '15'.
          PERFORM zf_monta_tp_licenciamento.
        WHEN '16'.
          PERFORM zf_monta_tp_darj.
      ENDCASE.

      APPEND w_arquivo TO t_arquivo.

      v_regs_proc = v_regs_proc + 1.

      PERFORM f_tbatch.
      IF <f_zimp_cabecalho>-tp_imposto EQ '07'.
*      ajuste PL - CS2017002462 - Correções Banco do Brasil - CNAB - 21/09/2020 - inicio
        PERFORM preenche_segmento_o.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
*      ajuste PL - CS2017002462 - Correções Banco do Brasil - CNAB - 21/09/2020 - fim
        PERFORM preenche_segmento_w.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'."115457 GRU - CNAB - PSA  -> adcionando o imposto 14 = GRU
        PERFORM preenche_segmento_o_gru.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w_gru.
      ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- INICIO
*      CASE <F_ZIMP_CABECALHO>-COD_BARRAS+0(4).
*        WHEN '0019' OR '2379' OR '0339' OR '3419' OR '1049'.
*          PERFORM PREENCHE_SEGMENTO_J.
*          PERFORM PREENCHE_SEGMENTO_J52.
*        WHEN OTHERS.
*          PERFORM PREENCHE_SEGMENTO_O.
*      ENDCASE.
        CLEAR: v_codbanco.
        CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
        SELECT SINGLE * FROM bnka INTO @DATA(w_BNK)
         WHERE bankl LIKE @v_codbanco.
        IF sy-subrc IS INITIAL.
          PERFORM preenche_segmento_j.
          PERFORM preenche_segmento_j52.
        ELSE.
          PERFORM preenche_segmento_o.
        ENDIF.
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- FIM

*Inicio Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '08'.
        PERFORM preenche_segmento_o.
*Fim Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
      ELSE.
        PERFORM preenche_segmento_n.
      ENDIF.

      v_tot_tit = v_tot_tit + v_tit.
      "lote_servico = lote_servico + 1.

      sequencial_reg_lote = sequencial_reg_lote + 1.

    ENDLOOP.
*** BUG - 184742 - Inicio - CBRAND
    "PERFORM preenche_trailer_lote.
    PERFORM preenche_trailer_lote USING '0002'.
*** BUG - 184742 - Fim - CBRAND
    APPEND w_trailer_lote    TO t_arquivo1.
  ENDIF.
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- FIM

  PERFORM preenche_trailer_arquivo.
  APPEND w_trailer_arquivo TO t_arquivo1.

  "Adicionar mais uma linha em branco quando for banco Bradesco, em branco.
  IF p_bncemp EQ 'BBD'.
    CLEAR: w_trailer_arquivo.
    APPEND w_trailer_arquivo TO t_arquivo1.
  ENDIF.


*-PBI 71420 - 12.01.2022 - JT - inicio
  DESCRIBE TABLE t_arquivo1 LINES DATA(l_lines_arq).
  LOOP AT t_arquivo1  INTO DATA(w_arq).
    IF p_bncemp = 'BBD'.
      CHECK sy-tabix < l_lines_arq.
    ENDIF.
    w_arq+240(2)         = cl_abap_char_utilities=>cr_lf.
    MODIFY t_arquivo1 FROM w_arq INDEX sy-tabix.
  ENDLOOP.
*-PBI 71420 - 12.01.2022 - JT - fim

  PERFORM zf_grava_arquivo.
  lote_servico = lote_servico + 1.
ENDFORM.                    " ZF_MONTA_ARQUIVO

FORM zf_nome_cliente USING p_cgc TYPE char14.

  DATA:
    lv_adrnr      TYPE t001-adrnr,
    lv_cgc_number TYPE j_1bwfield-cgc_number.


  SELECT SINGLE adrnr butxt
    INTO (lv_adrnr, v_butxt)
    FROM t001
    WHERE bukrs = p_emp.


  IF sy-subrc = 0.

    REPLACE '-' WITH space INTO w_adrc-post_code1.
    CONDENSE w_adrc-post_code1 NO-GAPS.
    CONCATENATE '0'
                w_adrc-post_code1
           INTO w_adrc-post_code1.

  ENDIF.


  CALL FUNCTION 'J_1BREAD_CGC_COMPANY'
    EXPORTING
      bukrs      = p_emp
    IMPORTING
      cgc_number = lv_cgc_number.

  vcgc  = lv_cgc_number.
  p_cgc = lv_cgc_number.

  SELECT * FROM  j_1bbranch INTO TABLE @DATA(t_j_1bbranch) WHERE stcd1 EQ @lv_cgc_number.
  DELETE t_j_1bbranch WHERE branch EQ '0001'.
  READ TABLE t_j_1bbranch INTO DATA(w_j_1bbranch) INDEX 1.
  IF sy-subrc EQ 0.
    SELECT SINGLE *
     INTO w_adrc
     FROM adrc
     WHERE addrnumber = w_j_1bbranch-adrnr   AND
           date_from  = '00010101' AND
           nation     = space.
  ENDIF.

ENDFORM.                    " ZF_NOME_CLIENTE

FORM zf_banco_agencia  USING    p_agencia TYPE any
                                p_conta   TYPE any
                                p_nome_banco TYPE any
                                p_dv_agencia TYPE any.

  DATA: wl_agencia    TYPE bankk,
        wl_conta      TYPE bankn,
        wl_nome_banco TYPE bnka-banka,
        dv_agencia    TYPE bkont.

  SELECT bankl UP TO 1 ROWS
    INTO wl_agencia
    FROM t012
    WHERE bukrs = p_emp AND
          hbkid = p_bncemp.
  ENDSELECT.

  SELECT bankn UP TO 1 ROWS
    INTO wl_conta
    FROM t012k
    WHERE bukrs = p_emp AND
          hbkid = p_bncemp.
  ENDSELECT.


  SELECT banka UP TO 1 ROWS
    INTO wl_nome_banco
    FROM bnka
    WHERE bankl = wl_agencia AND banks = 'BR'.
  ENDSELECT.

  SELECT SINGLE dtgko
    INTO dv_agencia
    FROM t012d
    WHERE bukrs = p_emp AND
          hbkid = p_bncemp.

  MOVE: wl_conta TO p_conta,
        wl_agencia TO p_agencia,
        wl_nome_banco TO p_nome_banco,
        dv_agencia TO p_dv_agencia.

ENDFORM.                    " ZF_BANCO_AGENCIA

FORM zf_monta_tp_darf .

  ASSIGN w_darf TO <wa_data>.

  CLEAR w_darf.

  PERFORM add_zero_esquerda USING <f_zimp_cabecalho>-dt_apuracao.

  PERFORM preenche_valor.                                          "160-204--45

  PERFORM add_zero_esquerda USING w_darf-valor_principal.

  IF w_darf-valor_multa IS INITIAL.
    w_darf-valor_multa = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING w_darf-valor_multa.
  ENDIF.

  IF w_darf-valor_juros_encargos IS INITIAL.
    w_darf-valor_juros_encargos = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING w_darf-valor_juros_encargos.
  ENDIF.


  w_darf-cod_receita           = <f_zimp_cabecalho>-cod_pgto. "''."      "111-116--6

*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_darf-cod_receita .
  ELSE.
    PERFORM add_zero_esquerda    USING w_darf-cod_receita.
  ENDIF.
  w_darf-tipo_ident_contrib    = '01'.                             "117-118--2
  w_darf-identif_contribuinte  = identificador."<F_ZIMP_CABECALHO>-IDENTIFICADOR. "119-132
  PERFORM add_zero_esquerda    USING w_darf-identif_contribuinte.
  w_darf-cod_identif_tribut    ='16'." <F_ZIMP_CABECALHO>-TP_IMPOSTO.    "133-

  CONCATENATE <f_zimp_cabecalho>-dt_apuracao+6(2) <f_zimp_cabecalho>-dt_apuracao+4(2) <f_zimp_cabecalho>-dt_apuracao+0(4) INTO  w_darf-periodo_apuracao.

  w_darf-num_referencia        = '00000000000000000'.              "143-159--17


  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_darf-data_vencimento.

  PERFORM z_monta_espaco USING  w_darf-cnab.                      "213-230--18
  w_arquivo = w_darf.
  PERFORM z_format       USING w_arquivo.

  CLEAR w_darf.
ENDFORM.

FORM zf_monta_tp_darf_simples .

  ASSIGN w_darf_simples TO <wa_data>.

  CLEAR w_darf_simples.
  PERFORM preenche_valor.

  PERFORM add_zero_esquerda USING  w_darf_simples-valor_principal.

*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_darf_simples-cod_receita .
  ELSE.
    w_darf_simples-cod_receita = <f_zimp_cabecalho>-cod_pgto.
    PERFORM add_zero_esquerda    USING w_darf_simples-cod_receita.
  ENDIF.

  w_darf_simples-tipo_ident_contrib = '01'.
  w_darf_simples-identif_contribuinte           = identificador."<F_ZIMP_CABECALHO>-IDENTIFICADOR.
  PERFORM add_zero_esquerda USING w_darf_simples-identif_contribuinte.
  w_darf_simples-cod_identif_tribut             = '18'."<F_ZIMP_CABECALHO>-TP_IMPOSTO.

  CONCATENATE <f_zimp_cabecalho>-dt_apuracao+6(2) <f_zimp_cabecalho>-dt_apuracao+4(2) <f_zimp_cabecalho>-dt_apuracao+0(4) INTO  w_darf_simples-periodo_apuracao.
  PERFORM add_zero_esquerda USING  w_darf_simples-periodo_apuracao.


  w_darf_simples-valor_receita_bruta_acumulada  = '000000000000000'.
  w_darf_simples-perc_sb_receita_brut_acum      = '0000000'.

  IF w_darf_simples-valor_multa IS INITIAL.
    w_darf_simples-valor_multa = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING  w_darf_simples-valor_multa.
  ENDIF.

  IF w_darf_simples-valor_juros_encargos IS INITIAL.
    w_darf_simples-valor_juros_encargos = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING  w_darf_simples-valor_juros_encargos.
  ENDIF.
  "W_DARF_SIMPLES-CNAB = '                     '.
  PERFORM  z_monta_espaco USING w_darf_simples-cnab.
  w_arquivo = w_darf_simples.
  PERFORM z_format       USING w_arquivo.

  CLEAR w_darf_simples.
ENDFORM.                     " ZF_MONTA_TP_DARF_SIMPLES

FORM zf_monta_tp_gps .
  ASSIGN w_gps TO <wa_data>.
  CLEAR w_gps.


  PERFORM preenche_valor.

  IF w_gps-valor_inss IS INITIAL.
    w_gps-valor_inss = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING w_gps-valor_inss.
  ENDIF.

  IF w_gps-valor_outras_entidades IS INITIAL.
    w_gps-valor_outras_entidades = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING w_gps-valor_outras_entidades.
  ENDIF.


  w_gps-identif_contribuinte  = identificador."<F_ZIMP_CABECALHO>-IDENTIFICADOR.

  PERFORM add_zero_esquerda USING w_gps-identif_contribuinte.
  w_gps-cod_receita           = <f_zimp_cabecalho>-cod_pgto.

*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING  w_gps-cod_receita.
  ELSE.
    PERFORM add_zero_esquerda USING w_gps-cod_receita.
  ENDIF.
  w_gps-tipo_ident_contrib    = '01'.
  w_gps-cod_identif_tribut    = '17'."W_HEADER_LOTE-FORMA_LANCAMENTO.
  w_gps-mes_ano_comp          = |{ <f_zimp_cabecalho>-mes_apuracao }{ <f_zimp_cabecalho>-ano_apuracao }|.
  w_gps-atualizacao_menetaria  = '000000000000000'.
  PERFORM  z_monta_espaco USING w_gps-cnab.
  w_arquivo = w_gps.

  PERFORM z_format       USING w_arquivo.

  "APPEND W_ARQUIVO TO T_ARQUIVO1.

ENDFORM.                    " ZF_MONTA_TP_GPS

FORM zf_monta_tp_gare .

  ASSIGN w_gare_sp TO <wa_data>.
  CLEAR w_gare_sp.

  PERFORM add_zero_esquerda USING <f_zimp_cabecalho>-identificador.

  PERFORM preenche_valor.
  IF w_gare_sp-valor_multa IS INITIAL.
    w_gare_sp-valor_multa = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING w_gare_sp-valor_multa.
  ENDIF.

  IF w_gare_sp-valor_juros_encargos IS INITIAL.
    w_gare_sp-valor_juros_encargos = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING w_gare_sp-valor_juros_encargos.
  ENDIF.


  w_gare_sp-cod_receita           = <f_zimp_cabecalho>-cod_pgto.


  PERFORM add_zero_esquerda USING w_gare_sp-valor_receita.

  w_gare_sp-tipo_ident_contrib    = '01'.
  w_gare_sp-identif_contribuinte  = identificador."<F_ZIMP_CABECALHO>-IDENTIFICADOR.
  PERFORM add_zero_esquerda USING w_gare_sp-identif_contribuinte.
  w_gare_sp-cod_identif_tribut    = '22'."<F_ZIMP_CABECALHO>-TP_IMPOSTO.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_gare_sp-data_vencimento.

  SORT t_j_1bbranch BY branch.
  READ TABLE t_j_1bbranch INTO w_j_1bbranch WITH KEY branch = <f_zimp_cabecalho>-gsber BINARY SEARCH.

  w_gare_sp-inscricao_estadual    = w_j_1bbranch-state_insc.
  PERFORM add_zero_esquerda USING w_gare_sp-inscricao_estadual.

  w_gare_sp-divida_ativa          = '0000000000000'.

  CONCATENATE <f_zimp_cabecalho>-dt_apuracao+4(2) <f_zimp_cabecalho>-dt_apuracao+0(4) INTO  w_gare_sp-periodo_referencia.

  w_gare_sp-num_parcela           = '0000000000000'.
  PERFORM  z_monta_espaco USING w_gare_sp-cnab.

  w_arquivo = w_gare_sp.

  PERFORM z_format       USING w_arquivo.

ENDFORM.                    " ZF_MONTA_TP_GARE

FORM zf_monta_tp_licenciamento .

  CLEAR w_licenciamento.



  w_licenciamento-cod_receita          = <f_zimp_cabecalho>-cod_pgto.
  w_licenciamento-tipo_ident_contrib   = '01'.
  w_licenciamento-identif_contribuinte = identificador."<F_ZIMP_CABECALHO>-IDENTIFICADOR.
  PERFORM add_zero_esquerda USING w_licenciamento-identif_contribuinte.
  w_licenciamento-cod_identif_tribut   = <f_zimp_cabecalho>-tp_imposto.
  w_licenciamento-ano_base             = <f_zimp_cabecalho>-ano_apuracao.
  w_licenciamento-cod_renavam          = '000000000'.
  w_licenciamento-cod_municipio        = '00000'.
  w_licenciamento-unid_federacao       = '  '.
  w_licenciamento-placa_veiculo        = '       '.
  w_licenciamento-opcao_pagamento      = ' '.
  w_licenciamento-opcao_retirada_crvl  = ' '.
  PERFORM z_monta_espaco USING w_licenciamento-cnab.

  w_arquivo = w_licenciamento.

  PERFORM z_format       USING w_arquivo.

ENDFORM.                    " ZF_MONTA_TP_LICENCIAMENTO

FORM zf_monta_tp_fgts .
  DATA:
    lv_cgc_number TYPE j_1bwfield-cgc_number.

  ASSIGN w_fgts TO <wa_data>.
  CLEAR w_fgts.

  "W_FGTS-IDENTIFICADOR_TRIBUTO = '01'.
  w_fgts-cod_receita = '      '."<F_ZIMP_CABECALHO>-COD_PGTO.
  w_fgts-tipo_ident_contribuinte = '  '. " ' 1'
  "PERFORM ADD_ZERO_ESQUERDA USING <F_ZIMP_CABECALHO>-IDENTIFICADOR.
  w_fgts-identificador_contribuinte ='              '. "<F_ZIMP_CABECALHO>-IDENTIFICADOR.
  w_fgts-identificador_fgts = '                '.
  w_fgts-lacre_conectividade_social = '         '.
  w_fgts-dg_lacre_concetividade_social = ' '.

  "PERFORM PREENCHE_VALOR.

  w_arquivo = w_fgts.

ENDFORM.                    " ZF_MONTA_TP_FGTS

FORM zf_monta_tp_ipva .

  ASSIGN w_ipva TO <wa_data>.
  CLEAR w_ipva.

  PERFORM zf_nome_cliente   USING  w_header_arquivo-num_inscricao_empresa.

  PERFORM preenche_valor.


  w_ipva-cod_receita          = <f_zimp_cabecalho>-cod_pgto.
  w_ipva-tipo_ident_contrib   = '01'.
  w_ipva-identif_contribuinte = identificador."<F_ZIMP_CABECALHO>-IDENTIFICADOR.
  PERFORM add_zero_esquerda USING w_ipva-identif_contribuinte.
  w_ipva-cod_identif_tribut   = <f_zimp_cabecalho>-tp_imposto.
  w_ipva-ano_base             = <f_zimp_cabecalho>-ano_apuracao.
  w_ipva-cod_renavam          = '000000000'.
  w_ipva-unid_federacao       = '  '.
  w_ipva-cod_municipio        = '00000'. "W_ADRC-CITY1.

  PERFORM z_monta_espaco USING w_ipva-placa_veiculo.
  PERFORM z_monta_espaco USING w_ipva-opcao_pagamento .
  PERFORM z_monta_espaco USING w_ipva-cnab .

  w_arquivo = w_ipva.

  PERFORM z_format       USING w_arquivo.

  CLEAR w_ipva.
ENDFORM.                     " ZF_MONTA_IPVA

FORM zf_monta_tp_dpvat .

  ASSIGN w_dpvat TO <wa_data>.

  CLEAR w_dpvat.
  PERFORM zf_nome_cliente   USING  w_header_arquivo-num_inscricao_empresa.
  PERFORM preenche_valor.

  w_dpvat-cod_receita           = <f_zimp_cabecalho>-cod_pgto.
  w_dpvat-tipo_ident_contrib    = '01'.
  w_dpvat-identif_contribuinte  = identificador."<F_ZIMP_CABECALHO>-IDENTIFICADOR.
  PERFORM add_zero_esquerda USING w_dpvat-identif_contribuinte.
  w_dpvat-cod_identif_tribut    = <f_zimp_cabecalho>-tp_imposto.
  w_dpvat-ano_base              =  <f_zimp_cabecalho>-ano_apuracao.
  w_dpvat-cod_renavam           = '000000000'.
  w_dpvat-cod_municipio         = '00000'.

  PERFORM z_monta_espaco USING w_dpvat-unid_federacao.
  PERFORM z_monta_espaco USING w_dpvat-placa_veiculo.
  PERFORM z_monta_espaco USING w_dpvat-opcao_pagamento.
  PERFORM z_monta_espaco USING w_dpvat-cnab.

  w_arquivo = w_dpvat.

  PERFORM z_format       USING w_arquivo.
  CLEAR w_dpvat.
ENDFORM.                     " ZF_MONTA_DPVAT

FORM zf_monta_tp_darj.

  ASSIGN w_darj TO <wa_data>.

  CLEAR w_darj.


  PERFORM preenche_valor.
  PERFORM add_zero_esquerda USING w_darj-valor_principal.


  IF w_darj-val_at_monet IS INITIAL.
    w_darj-val_at_monet = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING w_darj-val_at_monet.
  ENDIF.

  IF w_darj-valor_mora IS INITIAL.
    w_darj-valor_mora = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING w_darj-valor_mora.
  ENDIF.

  IF w_darj-valor_multa IS INITIAL.
    w_darj-valor_multa = '000000000000000'.
  ELSE.
    PERFORM add_zero_esquerda USING w_darj-valor_multa.
  ENDIF.

  w_darj-cod_receita          = <f_zimp_cabecalho>-cod_pgto.
  w_darj-tipo_ident_contrib   = '01'.
  w_darj-identif_contribuinte = identificador."<F_ZIMP_CABECALHO>-IDENTIFICADOR.
  PERFORM add_zero_esquerda USING w_darj-identif_contribuinte.
  w_darj-inscricao_estadual   = '        '.
  w_darj-num_doc_origem       = '0000000000000000'.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_o-data_vencimento.
  "W_DARJ-DATA_VENCIMENTO      = <F_ZIMP_CABECALHO>-DT_VENC.
  w_darj-periodo_parcela      = '000000'.

  w_arquivo = w_darj.

  PERFORM z_format       USING w_arquivo.

  CLEAR w_darj.
ENDFORM.                    "ZF_MONTA_TP_DARJ

FORM zf_grava_arquivo .

  DATA:
    wl_tabname(15),
    wl_index(2).
  FIELD-SYMBOLS <fs_tab> TYPE ANY TABLE.

  DO 13 TIMES.
    MOVE sy-index TO wl_index.
*  CASE SY-INDEX.
    CONCATENATE 't_arquivo' wl_index '[]' INTO wl_tabname.
    ASSIGN (wl_tabname) TO <fs_tab>.
    IF sy-subrc = 0.
      IF NOT <fs_tab> IS INITIAL.
        MOVE p_path TO lv_nome_arquivo.
        MOVE <fs_tab>        TO t_arquivo[].
        v_arqs_proc = v_arqs_proc + 1.

        IF p_pc IS INITIAL.
          OPEN DATASET lv_nome_arquivo FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

          IF sy-subrc <> 0.
            MESSAGE 'Caminho ou nome de arquivo inválido. Impossível continuar!'
            TYPE 'A'.
          ENDIF.

          LOOP AT t_arquivo INTO w_arquivo.
            TRANSFER w_arquivo TO lv_nome_arquivo.
          ENDLOOP.

          CLOSE DATASET lv_nome_arquivo.
        ELSE.
          IF p_bncemp = 'BBD' OR p_bncemp(2) = 'IT' OR p_bncemp(2) = 'BBRA'. "BUG - 71188 - CSB

            DATA: w_filename TYPE string.
            MOVE: lv_nome_arquivo TO w_filename.

            IF w_filename EQ 'C:\' OR w_filename EQ 'C:' OR w_filename EQ 'C'.
              MESSAGE 'Caminho ou nome de arquivo inválido. Impossível continuar!' TYPE 'E'.
              CLEAR: v_regs_proc.
              CLEAR: v_arqs_proc.
              EXIT.
            ENDIF.

            CALL FUNCTION 'GUI_DOWNLOAD'
              EXPORTING
                filename                  = w_filename
                filetype                  = 'ASC'
                write_lf                  = ' '
                write_lf_after_last_line  = ' '
                trunc_trailing_blanks     = 'X'
                trunc_trailing_blanks_eol = 'X'
              TABLES
                data_tab                  = t_arquivo
              EXCEPTIONS
                file_write_error          = 1
                no_batch                  = 2
                gui_refuse_filetransfer   = 3
                invalid_type              = 4
                no_authority              = 5
                unknown_error             = 6
                header_not_allowed        = 7
                separator_not_allowed     = 8
                filesize_not_allowed      = 9
                header_too_long           = 10
                dp_error_create           = 11
                dp_error_send             = 12
                dp_error_write            = 13
                unknown_dp_error          = 14
                access_denied             = 15
                dp_out_of_memory          = 16
                disk_full                 = 17
                dp_timeout                = 18
                file_not_found            = 19
                dataprovider_exception    = 20
                control_flush_error       = 21
                OTHERS                    = 22.

          ELSE.

            DATA: wl_filename TYPE rlgrap-filename.
            DATA: w_filename1 TYPE string.
            MOVE: lv_nome_arquivo TO w_filename1. "wl_filename.

*           IF wl_filename EQ 'C:\' OR wl_filename EQ 'C:' OR wl_filename EQ 'C'.
            IF w_filename1 EQ 'C:\' OR w_filename1 EQ 'C:' OR w_filename1 EQ 'C'.
              MESSAGE 'Caminho ou nome de arquivo inválido. Impossível continuar!' TYPE 'E'.
              CLEAR: v_regs_proc.
              CLEAR: v_arqs_proc.
              EXIT.
            ENDIF.

*-PBI 71420 - 12.01.2022 - JT - inicio
            CALL FUNCTION 'GUI_DOWNLOAD'
              EXPORTING
                filename                  = w_filename1
                filetype                  = 'ASC'
                write_lf_after_last_line  = ' '
                write_lf                  = ' '
                trunc_trailing_blanks     = 'X'
                trunc_trailing_blanks_eol = 'X'
              TABLES
                data_tab                  = t_arquivo
              EXCEPTIONS
                file_write_error          = 1
                no_batch                  = 2
                gui_refuse_filetransfer   = 3
                invalid_type              = 4
                no_authority              = 5
                unknown_error             = 6
                header_not_allowed        = 7
                separator_not_allowed     = 8
                filesize_not_allowed      = 9
                header_too_long           = 10
                dp_error_create           = 11
                dp_error_send             = 12
                dp_error_write            = 13
                unknown_dp_error          = 14
                access_denied             = 15
                dp_out_of_memory          = 16
                disk_full                 = 17
                dp_timeout                = 18
                file_not_found            = 19
                dataprovider_exception    = 20
                control_flush_error       = 21
                OTHERS                    = 22.

*            CALL FUNCTION 'WS_DOWNLOAD'
*              EXPORTING
*                filename                = wl_filename "filename = 'C:\LPT1:'
*                filetype                = 'ASC'
*                no_auth_check           = 'X'
*              TABLES
*                data_tab                = t_arquivo
*              EXCEPTIONS
*                file_open_error         = 1
*                file_write_error        = 2
*                invalid_filesize        = 3
*                invalid_type            = 4
*                no_batch                = 5
*                unknown_error           = 6
*                invalid_table_width     = 7
*                gui_refuse_filetransfer = 8
*                customer_error          = 9
*                no_authority            = 10
*                OTHERS                  = 11.
*-PBI 71420 - 12.01.2022 - JT - fim
            IF sy-subrc <> 0.
              MESSAGE ID '00' TYPE 'E' NUMBER '398' WITH
                 'Erro ao criar o arquivo'
                 <f_zimp_cabecalho>-doc_imposto
                 'na pasta'
                 p_path.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.

  PERFORM gera_relatorio.

ENDFORM.                    " ZF_GRAVA_ARQUIVO

FORM zf_informacao .

  MESSAGE s398(00) WITH v_regs_proc
                        'registros foram processados em'
                        v_arqs_proc 'arquivo(s).'.


ENDFORM.                    " ZF_INFORMACAO

FORM match_code_tparrc .

  DATA: BEGIN OF t_value OCCURS 0,
          line(30),
        END OF t_value,

        BEGIN OF t_zimp_tipos_impos OCCURS 0.
          INCLUDE STRUCTURE zimp_tipos_impos.
  DATA: END OF t_zimp_tipos_impos.

  DATA: t_hfields LIKE help_value OCCURS 0 WITH HEADER LINE.

  CLEAR t_hfields.
  t_hfields-tabname = 'ZIMP_TIPOS_IMPOS'.
  t_hfields-fieldname = 'TP_ARREC'.
  t_hfields-selectflag = 'X'.
  APPEND t_hfields.

  CLEAR t_hfields.
  t_hfields-tabname = 'ZIMP_TIPOS_IMPOS'.
  t_hfields-fieldname = 'ARRECADACAO'.
  APPEND t_hfields.

  SELECT *
    FROM zimp_tipos_impos
    WHERE tp_arrec IS NOT INITIAL
    INTO TABLE @t_zimp_tipos_impos.

  SORT t_zimp_tipos_impos BY tp_arrec.

  LOOP AT t_zimp_tipos_impos.

    t_value-line = t_zimp_tipos_impos-tp_arrec.
    APPEND t_value.

    t_value-line = t_zimp_tipos_impos-arrecadacao.
    APPEND t_value.

  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
    EXPORTING
      titel                     = TEXT-001
    IMPORTING
      select_value              = s_tparrc-low
    TABLES
      fields                    = t_hfields
      valuetab                  = t_value
    EXCEPTIONS
      field_not_in_ddic         = 1
      more_then_one_selectfield = 2
      no_selectfield            = 3
      OTHERS                    = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " MATCH_CODE_TPARRC

FORM gera_relatorio .

  WRITE:/ sy-uline.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.


  READ TABLE s_dtvenc INDEX 1.
  WRITE:/1 sy-vline,
         2 'Empresa Pagadora:',
         20 p_emp," P_bukrs,
         202 sy-vline.

  WRITE:/1 sy-vline,
         2 'Nome do Arquivo:',
         19 lv_nome_arquivo,
         202 sy-vline.

  WRITE:/1 sy-vline,
         2 'Data Vencimento:',
*         19 <F_ZIMP_CABECALHO>-DT_VENC,
         19 s_dtvenc-low,
         202 sy-vline.

  WRITE:/1 sy-vline,
         2 'Valor Total dos Títulos:',
         27 v_tot_tit,
         202 sy-vline.

  WRITE:/1 sy-uline.

ENDFORM.                    " GERA_RELATORIO

FORM f_shdb  USING    p_programa
                      p_tela
                      p_id
                      p_campo
                      p_valor.

  CLEAR wa_bdc.

  wa_bdc-program        = p_programa.
  wa_bdc-dynpro         = p_tela.
  wa_bdc-dynbegin       = p_id.
  wa_bdc-fnam           = p_campo.
  wa_bdc-fval           = p_valor.

  APPEND wa_bdc TO ti_bdc.

ENDFORM.                    " f_shdb

FORM preenche_valor .
  DATA: string(50)    TYPE c,
        vcod_abertura TYPE zimp_lanc_imp_ct-cod_abertura,
        vcalculo      TYPE zimp_lanc_imp_ct-valor_imp,
        vtotal        TYPE zimp_lanc_imp_ct-valor_imp,
        xloop         TYPE i,
        xoperacao(1),
        vfield(50).

  CLEAR v_tit.


  LOOP AT t_zimp_layout_imp INTO w_zimp_layout_imp WHERE tp_imposto = <f_zimp_cabecalho>-tp_imposto.

    string = w_zimp_layout_imp-camposabert.
    vcalculo = 0.
    vtotal  = 0.
    CLEAR: vcod_abertura, xoperacao.
    WHILE string NE space.

      IF string(1) = '<'.
        CLEAR vcod_abertura.
      ELSEIF string(1) = '>'.
        ADD 1 TO xloop .
        vcalculo = 0.

        LOOP AT t_zimp_detalhe ASSIGNING <f_zimp_detalhe>
              WHERE doc_imposto  = <f_zimp_cabecalho>-doc_imposto
              AND   cod_abertura = vcod_abertura
              AND   bukrs        = <f_zimp_cabecalho>-bukrs.

          CASE vcod_abertura.
            WHEN '01'.
              vcalculo = <f_zimp_detalhe>-valor_imp.
            WHEN '07'.
              vcalculo = <f_zimp_detalhe>-valor_imp.
*Inicio Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
            WHEN '08'.
              vcalculo = <f_zimp_detalhe>-valor_imp.
*Fim Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
** CBRAND - Inicio
            WHEN '10'.
              vcalculo = <f_zimp_detalhe>-valor_imp * -1.
** CBRAND - Fim
            WHEN '11'.
              vcalculo = <f_zimp_detalhe>-valor_imp * -1.
            WHEN '17'. "PSA (Aqui´é o tipo de cobertuira e não o tipo de imposto (FGTS = 11)(GRU = 17))
              vcalculo = <f_zimp_detalhe>-valor_imp * -1.
            WHEN OTHERS.
*              VLIFNR = <F_ZIMP_DETALHE>-LIFNR.
          ENDCASE.

          IF <f_zimp_detalhe>-lifnr IS INITIAL.
            vcalculo = <f_zimp_detalhe>-valor_imp.
** CBRAND - Inicio
            IF vcod_abertura = '10'.
              vcalculo = <f_zimp_detalhe>-valor_imp * -1.
            ENDIF.
** CBRAND - Fim
          ELSE.
            vlifnr = <f_zimp_detalhe>-lifnr.
          ENDIF.

        ENDLOOP.

        IF xoperacao  IS INITIAL.
          xoperacao = '+'.
        ENDIF.

        IF xoperacao = '+'.
          ADD vcalculo TO vtotal.
          CLEAR xoperacao.
        ELSEIF xoperacao = '-'.
          SUBTRACT vcalculo FROM vtotal.
          CLEAR xoperacao.
        ELSEIF xoperacao = '*'.
          vtotal = vtotal * vcalculo.
          CLEAR xoperacao.
        ELSEIF xoperacao = '/'.
          vtotal = vtotal / vcalculo.
          CLEAR xoperacao.
        ENDIF.

      ELSEIF '+_-_*_/' CS string(1) AND NOT string(1) IS INITIAL.
        xoperacao = string(1).
      ELSEIF NOT string(1) IS INITIAL.
        CONCATENATE vcod_abertura string(1) INTO vcod_abertura.
      ENDIF.
      SHIFT string.
    ENDWHILE.

    IF s_tparrc-low = '08' AND <f_zimp_detalhe>-vlr_moeda_doc IS NOT INITIAL. "119896 AJUSTE TRANSAÇÃO PARA PAGAMENTOS DE CONSUMO - PSA
      vtotal = <f_zimp_detalhe>-vlr_moeda_doc.
    ENDIF.



    IF vtotal GT 0.
      vfield = w_zimp_layout_imp-campolayout.
      CONDENSE vfield NO-GAPS.

      ASSIGN COMPONENT vfield  OF STRUCTURE <wa_data> TO <fs_campo>.

      IF sy-subrc = 0 AND vfield NE 'VALOR_TOTAL'.
        <fs_campo> = vtotal * 100.
      ENDIF.
*-PBI 71420 - 12.01.2022 - JT - inicio
      IF vcod_abertura = '11' OR vcod_abertura = '17' . "115457 GRU - CNAB - PSA -> adicionado GRU imposto é 14 mas a abertura é 17
        v_tit = v_tit + vtotal.
      ENDIF.
*-PBI 71420 - 12.01.2022 - JT - fim
    ENDIF.
  ENDLOOP.
ENDFORM.                    " PREENCHE_VALOR

FORM f_tbatch .
  CONCATENATE  <f_zimp_cabecalho>-dt_apuracao+6(2)
               <f_zimp_cabecalho>-dt_apuracao+4(2)
               <f_zimp_cabecalho>-dt_apuracao(4)
          INTO t_batch-bldat.
  t_batch-blart = 'ZP'.
  t_batch-bukrs = <f_zimp_cabecalho>-bukrs.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2)
              <f_zimp_cabecalho>-dt_venc+4(2)
              <f_zimp_cabecalho>-dt_venc(4)
         INTO t_batch-budat.
  IF <f_zimp_cabecalho>-waers NE 'BRL'.
    t_batch-waers = <f_zimp_cabecalho>-waers.
  ELSE.
    t_batch-waers = 'BRL'.
  ENDIF.
  WRITE <f_zimp_cabecalho>-dt_venc  TO t_batch-data.

  SELECT *
    INTO w_t012k
    FROM t012k
    UP TO 1 ROWS
    WHERE bukrs = <f_zimp_cabecalho>-bukrs
      AND hbkid = p_bncemp.
  ENDSELECT.

  IF s_tparrc-low = '08' AND <f_zimp_cabecalho>-ref_imposto = 'ZGL059' . "119896 AJUSTE TRANSAÇÃO PARA PAGAMENTOS DE CONSUMO - PSA

    t_batch-bukrs = <f_zimp_cabecalho>-bukrs.
    t_batch-belnr = <f_zimp_cabecalho>-doc_imposto.
    t_batch-gjahr = <f_zimp_cabecalho>-ano_apuracao.
    t_batch-waers = 'BRL'.

*    DATA aux_wrbtr(16) TYPE c.
*    CLEAR: aux_wrbtr.
*
*    LOOP AT aux_zimp_detalhe ASSIGNING FIELD-SYMBOL(<aux_zimp_detalhe>).
*
*      IF <aux_zimp_detalhe>-doc_imposto = <f_zimp_cabecalho>-doc_imposto.
*        aux_wrbtr = <aux_zimp_detalhe>-valor_imp.
*              CONDENSE aux_wrbtr NO-GAPS.
*      t_batch-wrbtr = aux_wrbtr.
*      ENDIF.
*
*      ENDLOOP.

    SELECT SINGLE *
    FROM zib_contabil_chv
    INTO wa_zib_contabil_chv
    WHERE belnr = <f_zimp_cabecalho>-doc_imposto.

  ELSE.

    CONCATENATE 'ZP' <f_zimp_cabecalho>-bukrs <f_zimp_cabecalho>-doc_imposto '%' INTO xobj_key.
    SELECT SINGLE *
    FROM zib_contabil_chv
    INTO wa_zib_contabil_chv
    WHERE obj_key LIKE xobj_key.

    IF sy-subrc NE 0.
      CONCATENATE 'ZIMP' <f_zimp_cabecalho>-doc_imposto '%' INTO xobj_key.
    ENDIF.

    SELECT SINGLE *
      FROM zib_contabil_chv
      INTO wa_zib_contabil_chv
      WHERE obj_key LIKE xobj_key
      AND   bukrs   =  <f_zimp_cabecalho>-bukrs .

  ENDIF.

  CLEAR: v_tit, v_tit_f.

  "DELETE ADJACENT DUPLICATES FROM aux_zimp_detalhe.

  LOOP AT t_zimp_detalhe ASSIGNING <f_zimp_detalhe>
             WHERE doc_imposto = <f_zimp_cabecalho>-doc_imposto.
    IF  <f_zimp_cabecalho>-ref_imposto = 'ZGL059'.
      v_tit  = <f_zimp_detalhe>-valor_imp.
      v_tit_f = <f_zimp_detalhe>-valor_for.
    ELSE.
      IF <f_zimp_detalhe>-lifnr IS INITIAL.
        v_tit  = v_tit + <f_zimp_detalhe>-valor_imp.
        v_tit_f = v_tit_f + <f_zimp_detalhe>-valor_for.
      ELSE.
        vlifnr = <f_zimp_detalhe>-lifnr.
      ENDIF.
    ENDIF.

  ENDLOOP.
  t_batch-konto = w_t012k-hkont.
  t_batch-gsber = <f_zimp_cabecalho>-gsber.
  IF <f_zimp_cabecalho>-waers NE 'BRL'.
    t_batch-wrbtr = v_tit_f.
    t_batch-dmbtr = v_tit.
  ELSE.
    CLEAR v_tit_f.
    t_batch-wrbtr = v_tit.
    t_batch-dmbtr = v_tit_f.
  ENDIF.

  DATA aux_valor(16) TYPE c.
  CLEAR: aux_valor.

  REPLACE '.' WITH ',' INTO t_batch-wrbtr.
  REPLACE '.' WITH ',' INTO t_batch-dmbtr.


  t_batch-sel01 = wa_zib_contabil_chv-belnr.
  "t_batch-agkon =  vlifnr.
  IF  <f_zimp_cabecalho>-ref_imposto = 'ZGL059'. "PSA - TENTATIVA DE AJUSTE NA CONFIÇÃO ZGL059
    t_batch-agkon =  ''.
  ELSE.
    t_batch-agkon =  vlifnr.
  ENDIF.

  APPEND t_batch.
ENDFORM.                    " F_TBATCH

FORM zf_batch_input .

*  PERFORM f_tbatch.

  LOOP AT t_batch.
* executa o mapeamento
    PERFORM f_mapeamento.
* executa a transação
    PERFORM f_transaction.
* trata msg
    PERFORM f_log.
  ENDLOOP.
ENDFORM.                    " ZF_BATCH_INPUT

FORM f_mapeamento .
*eduardo
*if v_budat is INITIAL.
  v_budat = t_batch-budat.
*  endif.

  CLEAR ti_bdc[].

  PERFORM f_shdb USING:

'SAPMF05A'  '0103'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '/00'                 ,
   ''         ''    ''                                  'BKPF-BLDAT'  t_batch-data          ,
   ''         ''    ''                                  'BKPF-BLART'  'ZP'                  ,
   ''         ''    ''                                  'BKPF-BUKRS'  t_batch-bukrs         ,
* Inclusão - Inicio - KZORZAN - ROLLOUT - 30.07.2009 ******************
*   ''         ''    ''                                  'BKPF-BUDAT'  <f_zimp_cabecalho>-zfbdt, " t_batch-budat         ,
   ''         ''    ''                                  'BKPF-BUDAT'   t_batch-data         ,
* Inclusão - Inicio - KZORZAN - ROLLOUT - 30.07.2009 ******************
   ''         ''    ''                                  'BKPF-MONAT'  '7'                   ,
   ''         ''    ''                                  'BKPF-WAERS'  t_batch-waers         ,
   ''         ''    ''                                  'RF05A-KONTO' t_batch-konto         ,
   ''         ''    ''                                  'BSEG-GSBER'  t_batch-gsber         ,
   ''         ''    ''                                  'BSEG-WRBTR'  t_batch-wrbtr         ,
   ''         ''    ''                                  'BSEG-DMBTR'  t_batch-dmbtr         ,
   ''         ''    ''                                  'RF05A-AGKON' t_batch-agkon         ,
   ''         ''    ''                                  'RF05A-AGKOA' 'K'                   ,
   ''         ''    ''                                  'RF05A-XNOPS' 'X'                   ,
   ''         ''    ''                                  'RF05A-XPOS1(01)' ''                ,
   ''         ''    ''                                  'RF05A-XPOS1(03)' 'X'               ,

'SAPMF05A'  '0731'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '=PA'                 ,
   ''         ''    ''                                  'RF05A-SEL01(01)' t_batch-sel01    ,

'SAPDF05X'  '3100'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '=PI'                 ,
   ''         ''    ''                                  'BDC_SUBSCR'  'SAPDF05X                                6102PAGE',
   ''         ''    ''                                  'RF05A-ABPOS' '1'                   ,

'SAPDF05X'  '3100'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '=BS'                 ,
   ''         ''    ''                                  'BDC_SUBSCR'   'SAPDF05X                                6102PAGE',
   ''         ''    ''                                  'RF05A-ABPOS'  '1'                  ,

'SAPMF05A'  '0700' 'X'                                      ''          ''                  ,
   ''         ''    ''                                  'BDC_OKCODE'  '=BU'                 .

ENDFORM.                    " f_mapeamento

FORM f_transaction .

  v_mode = 'E'.

  CLEAR ti_msg[].
  CALL TRANSACTION 'F-53'
        USING ti_bdc
        MODE v_mode
        UPDATE 'S'
        MESSAGES INTO ti_msg.

  IF sy-subrc = 0.
    UPDATE zimp_contas_cons SET doc_comp = 'X'
                          WHERE bukrs = t_batch-bukrs AND
                                belnr = t_batch-belnr AND
                                gjahr = t_batch-gjahr.


  ENDIF.

ENDFORM.                    " f_transaction

FORM f_log .

  CLEAR wa_msg.

  wa_log-bldat = t_batch-bldat.
  wa_log-blart = t_batch-blart.
  wa_log-bukrs = t_batch-bukrs.
  wa_log-budat = t_batch-budat.
  wa_log-waers = t_batch-waers.
  wa_log-konto = t_batch-konto.
  wa_log-hbkid = t_batch-hbkid.
  wa_log-gsber = t_batch-gsber.
  wa_log-wrbtr = t_batch-wrbtr.
  wa_log-sel01 = t_batch-sel01.

  LOOP AT ti_msg INTO wa_msg.

    v_msgv1 = wa_msg-msgv1.
    v_msgv2 = wa_msg-msgv2.
    v_msgv3 = wa_msg-msgv3.
    v_msgv4 = wa_msg-msgv4.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = wa_msg-msgspra
        msg_id                 = wa_msg-msgid
        msg_no                 = wa_msg-msgnr
        msg_var1               = v_msgv1
        msg_var2               = v_msgv2
        msg_var3               = v_msgv3
        msg_var4               = v_msgv4
      IMPORTING
        msg_text               = v_msg
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      CLEAR wa_log-tipo.
      v_msg = 'Não houve mensagem.'.
    ENDIF.

    wa_log-tipo     = wa_msg-msgtyp.
    wa_log-msg      = v_msg.

    APPEND wa_log TO ti_log.

  ENDLOOP.

ENDFORM.                    " f_logc

FORM prenche_header_arquivo .

  DATA: vl_number         TYPE i,
        proximo(6)        TYPE c,
        agencia(8)        TYPE c,
        conta(9)          TYPE c,
        conta_simples(12) TYPE c,
        anterior          TYPE c,
        digito_conta      TYPE c,
        dv_agencia        TYPE c,
        nome_banco        TYPE bnka-banka,
        conv_banco_bbd    TYPE char6.


  PERFORM zf_codigo_banco USING codigo_banco.
  PERFORM zf_banco_agencia  USING agencia
                                  conta
                                  nome_banco
                                  dv_agencia.

  w_header_arquivo-cod_banco              = codigo_banco.
  w_header_arquivo-lote_servico           = '0000'.
  w_header_arquivo-tipo_registro          = '0'.
  w_header_arquivo-cnab                   = '         '.
  w_header_arquivo-tipo_inscricao_empresa = '2'.
  w_header_arquivo-num_inscricao_empresa  = identificador.
  PERFORM add_zero_esquerda    USING w_header_arquivo-num_inscricao_empresa.

*-BUG 71594 - 14.01.2022 - JT - inicio
  IF p_bncemp = 'BBD'.
    conv_banco_bbd = conv_banco.
**  Begin of CS2022000833 #97834 FF   05.12.2022
* BUG - 107478 - CBRAND -  Inicio
    READ TABLE t_tvarvc INTO DATA(ls_tvarvc) WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBD'.
    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
      conv_banco_bbd = ls_tvarvc-low.
      CLEAR: ls_tvarvc.
    ENDIF.
* Verifica se tem stvarv para empresa
    LOOP AT t_tvarvc INTO ls_tvarvc WHERE name CS 'MAGGI_ZIMP62_CONVENIO_BBD'.
      IF ls_tvarvc IS NOT INITIAL.
        DATA(lva_emp_cod) = ls_tvarvc-name+26(4).
        IF lva_emp_cod =  p_emp.
          conv_banco_bbd = ls_tvarvc-low.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR: ls_tvarvc,  lva_emp_cod.
*    READ TABLE t_tvarvc INTO DATA(ls_tvarvc) WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBD'.
*    IF sy-subrc = 0 AND ls_tvarvc-low is NOT INITIAL.
*      conv_banco_bbd = ls_tvarvc-low.
*    ELSE.
*      CLEAR ls_tvarvc.
*    ENDIF.
* BUG - 107478 - CBRAND -  Fim
** End of FF  05.12.2022
    PERFORM add_zero_esquerda    USING conv_banco_bbd.
    w_header_arquivo-cod_convenio_banco     = conv_banco_bbd.
  ELSE.
    w_header_arquivo-cod_convenio_banco     = conv_banco.
    PERFORM add_zero_esquerda    USING w_header_arquivo-cod_convenio_banco.
  ENDIF.

  IF p_bncemp = 'BBRA'.
    w_header_arquivo-cod_convenio_banco2  = '0126'.

**  Begin of CS2022000833 #97834 FF   05.12.2022
* BUG - 107478 - CBRAND -  Inicio
    READ TABLE t_tvarvc INTO ls_tvarvc WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBRA'.
    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
      w_header_arquivo-cod_convenio_banco2 = ls_tvarvc-low.
      CLEAR: ls_tvarvc.
    ENDIF.

    LOOP AT t_tvarvc   INTO ls_tvarvc WHERE name CS  'MAGGI_ZIMP62_CONVENIO_BBRA'.
      IF ls_tvarvc IS NOT INITIAL.
        lva_emp_cod = ls_tvarvc-name+26(4).
        IF lva_emp_cod =  p_emp.
          w_header_arquivo-cod_convenio_banco2 = ls_tvarvc-low.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR:ls_tvarvc, lva_emp_cod.

*    READ TABLE t_tvarvc INTO ls_tvarvc WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBRA'.
*    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
*      w_header_arquivo-cod_convenio_banco2 = ls_tvarvc-low.
*    ELSE.
*      CLEAR ls_tvarvc.
*    ENDIF.
* BUG - 107478 - CBRAND -  Fim
** End of FF  05.12.2022
  ENDIF.
*-BUG 71594 - 14.01.2022 - JT - fim

  w_header_arquivo-ag_mantenedora_conta   = agencia+4(4).
*** pbi - 43975 - inicio
  IF p_bncemp = 'BBD'.
    w_header_arquivo-d_verificador_agencia  = ' '.
  ELSE.
    w_header_arquivo-d_verificador_agencia  = dv_agencia.
  ENDIF.


  PERFORM zf_nome_cliente USING  w_header_arquivo-num_inscricao_empresa.

  PERFORM f_obtem_proximo_arquivo USING proximo.

  PERFORM add_zero_esquerda USING w_header_arquivo-ag_mantenedora_conta.

  WHILE conta(1) NE ' '.
    IF conta(1) NE '-'.
      CONCATENATE  conta_simples conta(1) INTO conta_simples.
    ELSE.
      anterior = conta(1).
    ENDIF.
    IF anterior EQ '-'.
      digito_conta = conta+1(1).
      SHIFT conta.
    ENDIF.

    SHIFT conta.
  ENDWHILE.

  PERFORM add_zero_esquerda USING conta_simples.
  PERFORM add_zero_esquerda USING proximo.

  w_header_arquivo-num_conta_corrente      = conta_simples.
  w_header_arquivo-dg_verificador_conta    = digito_conta.
*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    w_header_arquivo-dg_verificador_ag_conta = ' '.
  ELSE.
    w_header_arquivo-dg_verificador_ag_conta = '0'.
  ENDIF.
  w_header_arquivo-nome_empresa            = v_butxt.
  w_header_arquivo-nome_banco              = nome_banco.
  w_header_arquivo-cnab1                   = '          '.
  w_header_arquivo-cod_remessa_retorno     = '1'.
  w_header_arquivo-hora_geracao_arquivo    = sy-uzeit.
  w_header_arquivo-num_sequencial_arquivo = proximo.
  CONCATENATE  sy-datum+6(2)  sy-datum+4(2)  sy-datum+0(4) INTO w_header_arquivo-data_geracao_arquivo.

  IF p_bncemp = 'BBRA'.
    w_header_arquivo-num_vs_layout_arquivo = '103'.
  ELSEIF p_bncemp = 'BBD'.
    w_header_arquivo-num_vs_layout_arquivo = '089'.
  ELSEIF p_bncemp(2) = 'IT'.
    w_header_arquivo-num_vs_layout_arquivo = '080'.
  ENDIF.
*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    w_header_arquivo-densidade_gravacao_arquivo = '01600'.
*** PBI - 43975 - Fim
  ELSE.
    w_header_arquivo-densidade_gravacao_arquivo = '00000'.
  ENDIF.

  PERFORM z_monta_espaco USING w_header_arquivo-reservado_banco.
  PERFORM z_monta_espaco USING w_header_arquivo-reservado_empresa.
  PERFORM z_monta_espaco USING w_header_arquivo-cnab2.
  PERFORM z_format       USING w_header_arquivo.

ENDFORM.

FORM preenche_header_lote USING tp_lanc TYPE char1."** BUG - 184742 - CBRAND - "tp_lanc TYPE char2. "Analisar o tipo 10 - ISS em forma de boleto #180566 - BG

  DATA: codigo_banco(3)   TYPE c,
        agencia(8)        TYPE c,
        conta(9)          TYPE c,
        conta_simples(12) TYPE c,
        anterior          TYPE c,
        digito_conta      TYPE c,
        dv_agencia        TYPE c,
        nome_banco        TYPE bnka-banka,
        conv_banco_bbd    TYPE char6.

  PERFORM zf_codigo_banco      USING codigo_banco.
  PERFORM f_obtem_proximo_lote USING proximo.
  PERFORM add_zero_esquerda    USING proximo.
  PERFORM zf_banco_agencia     USING agencia
                                     conta
                                     nome_banco
                                     dv_agencia.


  w_header_lote-lote_servico  = lote_servico."PROXIMO.
  PERFORM add_zero_esquerda    USING w_header_lote-lote_servico.
  w_header_lote-tipo_registro = '1'.
  w_header_lote-tipo_operacao = 'C'.
*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    CASE <f_zimp_cabecalho>-cod_barras+0(4).
*     WHEN '0019' OR '2372'.
      WHEN '0019' OR '2379' OR '0339' OR '3419' OR '1049'.
        w_header_lote-tipo_servico  = '98'.
      WHEN OTHERS.
        w_header_lote-tipo_servico  = '22'.
    ENDCASE.
*** PBI - 43975 - fim
  ELSE.
    w_header_lote-tipo_servico  = '98'. "era 22
  ENDIF.
  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho> INDEX 1.

  CASE <f_zimp_cabecalho>-tp_imposto.
    WHEN'01'.
      w_header_lote-forma_lancamento = '18'. "DARF SIMPLES
    WHEN'02'.
      w_header_lote-forma_lancamento = '16'. "DARF NORMAL
    WHEN '03'.
      w_header_lote-forma_lancamento = '17'.
    WHEN '04' . "GARE
      w_header_lote-forma_lancamento = '22'.
    WHEN '05'. "GARE
      w_header_lote-forma_lancamento = '23'.
    WHEN '06'. "GARE
      w_header_lote-forma_lancamento = '24'.
*Inicio Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
***    WHEN '07'. "FGTS
    WHEN '07' OR '08'. "FGTS
*Fim Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
      w_header_lote-forma_lancamento = '11'.
    WHEN '09'. "DARF Via cod Barras
*      ajuste PL - CS2017002462 - Correções Banco do Brasil - CNAB - 21/09/2020 - inicio
*      w_header_lote-forma_lancamento = '16'. "DARF NORMAL
      w_header_lote-forma_lancamento = '11'. "DARF NORMAL
*      ajuste PL - CS2017002462 - Correções Banco do Brasil - CNAB - 21/09/2020 - fim
    WHEN '10'. "IPTU
*      ajuste PL - CS2017002462 - Correções Banco do Brasil - CNAB - 21/09/2020 - inicio
*      w_header_lote-forma_lancamento = '19'.
      IF ( codigo_banco EQ '001' ). "Banco do Brasil
        "w_header_lote-forma_lancamento = '80'.
        w_header_lote-forma_lancamento = '11'. " BUG - 71180 - CSB
      ELSE.
        IF ( codigo_banco EQ '237' ).
          w_header_lote-forma_lancamento = '11'. " PBI 43975 - ADD 237
        ELSE.
          w_header_lote-forma_lancamento = '19'.
        ENDIF.
      ENDIF.
*      ajuste PL - CS2017002462 - Correções Banco do Brasil - CNAB - 21/09/2020 - fim.
    WHEN '11'. " OUTROS
      w_header_lote-forma_lancamento = '11'.
    WHEN '12'. "GNRE
      w_header_lote-forma_lancamento = '11'.
    WHEN '13'.
      PERFORM zf_monta_tp_ipva.
      w_header_lote-forma_lancamento = '25'.
    WHEN '14'.
      w_header_lote-forma_lancamento = '11'.
*      PERFORM  zf_monta_tp_dpvat.
*      w_header_lote-forma_lancamento = '27'.
    WHEN '15'.
      PERFORM zf_monta_tp_licenciamento.
      w_header_lote-forma_lancamento = '26'.
    WHEN '16'.
      w_header_lote-forma_lancamento = '21'.
  ENDCASE.
** BUG - 184742 - CBRAND - INICIO
*  w_header_lote-forma_lancamento = tp_lanc. "Analisar o tipo 10 - ISS em forma de boleto #180566 - BG
** BUG - 184742 - CBRAND - FIM

* "// Inicio seguimento J PBI-71420
*  CASE <F_ZIMP_CABECALHO>-COD_BARRAS+0(3). "Analisar o tipo 10 - ISS em forma de boleto #180566 - BG
*    WHEN '001'."Analisar o tipo 10 - ISS em forma de boleto #180566 - BG
*      IF P_BNCEMP = 'BBRA'.
*        W_HEADER_LOTE-FORMA_LANCAMENTO = '30'.
*      ELSE.
*        W_HEADER_LOTE-FORMA_LANCAMENTO = '31'.
*      ENDIF.
*    WHEN '237'."Analisar o tipo 10 - ISS em forma de boleto #180566 - BG
*      IF P_BNCEMP = 'BBD'.
*        W_HEADER_LOTE-FORMA_LANCAMENTO = '30'.
*      ELSE.
*        W_HEADER_LOTE-FORMA_LANCAMENTO = '31'.
*      ENDIF.
*  ENDCASE.
* "// Inicio seguimento J PBI-71420

  IF p_bncemp = 'BBRA'.
    w_header_lote-num_vs_layout = '012'.
  ELSEIF p_bncemp = 'BBD'.
    IF v_segmento = 'O'.
      w_header_lote-num_vs_layout = '012'.
    ELSEIF v_segmento = 'J'.
      w_header_lote-num_vs_layout = '040'.
    ELSE.
      w_header_lote-num_vs_layout = '012'.
    ENDIF.
  ELSEIF p_bncemp = 'ITAU'.
    w_header_arquivo-num_vs_layout_arquivo = '030'.
  ENDIF.

  w_header_lote-cnab                   = ' '.
  w_header_lote-cod_banco_compensacao  = codigo_banco.
  w_header_lote-tipo_inscricao_empresa = '2'.
  w_header_lote-num_inscricao_empresa  = identificador.
  PERFORM add_zero_esquerda    USING w_header_lote-num_inscricao_empresa.

*-BUG 71594 - 14.01.2022 - JT - inicio
  IF p_bncemp = 'BBD'.
    conv_banco_bbd = conv_banco.
**  Begin of CS2022000833 #97834 FF   05.12.2022
* BUG - 107478 - CBRAND -  Inicio

    READ TABLE t_tvarvc INTO DATA(ls_tvarvc) WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBD'.
    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
      conv_banco_bbd = ls_tvarvc-low.
      CLEAR: ls_tvarvc.
    ENDIF.

    LOOP AT t_tvarvc INTO ls_tvarvc WHERE name CS 'MAGGI_ZIMP62_CONVENIO_BBD'.
      IF ls_tvarvc IS NOT INITIAL.
        DATA(lva_emp_cod) = ls_tvarvc-name+26(4).
        IF lva_emp_cod =  p_emp.
          conv_banco_bbd = ls_tvarvc-low.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR: ls_tvarvc,  lva_emp_cod.
*    READ TABLE t_tvarvc INTO DATA(ls_tvarvc) WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBD'.
*    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
*      conv_banco_bbd = ls_tvarvc-low.
*    ELSE.
*      CLEAR ls_tvarvc.
*    ENDIF.
** End of FF  05.12.2022
* BUG - 107478 - CBRAND -  Fim
    PERFORM add_zero_esquerda    USING conv_banco_bbd.
    w_header_lote-cod_convenio_banco     = conv_banco_bbd.
  ELSE.
    w_header_lote-cod_convenio_banco     = conv_banco.
    PERFORM add_zero_esquerda    USING w_header_lote-cod_convenio_banco.
  ENDIF.

  IF p_bncemp = 'BBRA'.
    w_header_lote-cod_convenio_banco2  = '0126'.

**  Begin of CS2022000833 #97834 FF   05.12.2022
** BUG - 107478 - CBRAND - Inicio
    READ TABLE t_tvarvc INTO ls_tvarvc WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBRA'.
    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
      w_header_lote-cod_convenio_banco2 = ls_tvarvc-low.
      CLEAR: ls_tvarvc.
    ENDIF.

    LOOP AT t_tvarvc   INTO ls_tvarvc WHERE name CS  'MAGGI_ZIMP62_CONVENIO_BBRA'.
      IF ls_tvarvc IS NOT INITIAL.
        lva_emp_cod = ls_tvarvc-name+26(4).
        IF lva_emp_cod =  p_emp.
          w_header_lote-cod_convenio_banco2 = ls_tvarvc-low.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR: ls_tvarvc,  lva_emp_cod.

*    READ TABLE t_tvarvc INTO ls_tvarvc WITH KEY name = 'MAGGI_ZIMP62_CONVENIO_BBRA'.
*    IF sy-subrc = 0 AND ls_tvarvc-low IS NOT INITIAL.
*      w_header_lote-cod_convenio_banco2 = ls_tvarvc-low.
*    ELSE.
*      CLEAR ls_tvarvc.
*    ENDIF.
** End of FF  05.12.2022
** BUG - 107478 - CBRAND - Fim

  ENDIF.
*-BUG 71594 - 14.01.2022 - JT - fim

  w_header_lote-agencia_convenio       = agencia+4(4).
*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    w_header_lote-dig_verif_agencia      = ' '.
  ELSE.
    w_header_lote-dig_verif_agencia      = dv_agencia.
  ENDIF.
  PERFORM add_zero_esquerda USING w_header_lote-agencia_convenio.


  WHILE conta(1) NE ' '.
    IF conta(1) NE '-'.
      CONCATENATE  conta_simples conta(1) INTO conta_simples.
    ELSE.
      anterior = conta(1).
    ENDIF.
    IF anterior EQ '-'.
      digito_conta = conta+1(1).
      SHIFT conta.
    ENDIF.

    SHIFT conta.
  ENDWHILE.

  PERFORM zf_nome_cliente USING  w_header_lote-num_inscricao_empresa.

  PERFORM add_zero_esquerda USING conta_simples.

  w_header_lote-num_conta_corrrente = conta_simples.
  w_header_lote-dig_verif_conta     = digito_conta.
*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    w_header_lote-dig_verif_ag_conta  = ' '.
  ELSE.
    w_header_lote-dig_verif_ag_conta  = '0'.
  ENDIF.
  w_header_lote-nome_empresa        = v_butxt.
  w_header_lote-mensagem            = '                                        '.
  w_header_lote-logradouro          = w_adrc-street.

  w_header_lote-numero_local        = w_adrc-house_num1.

  PERFORM add_zero_esquerda USING w_header_lote-numero_local.

  w_header_lote-complemento         = w_adrc-city2.
  w_header_lote-cidade              = w_adrc-city1.
*  W_ADRC-POST_CODE1                 = |{ W_ADRC-POST_CODE1(5) } + { W_ADRC-POST_CODE1+6(3) }|.
  w_header_lote-cep                 = |{ w_adrc-post_code1(5) }|.
  w_header_lote-complemento_cep     = |{ w_adrc-post_code1+6(3) }|.
  w_header_lote-estado              = w_adrc-region.

  IF p_bncemp = 'BBD'.
    w_header_lote-indic_forma_pag = '01'.
  ELSE .
    PERFORM z_monta_espaco USING w_header_lote-cnab_2.
  ENDIF.

*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_header_lote-cnab_2.
    PERFORM z_monta_espaco USING w_header_lote-cod_ocorrencia_retorno.
  ELSE.
    "PERFORM Z_MONTA_ESPACO USING W_HEADER_LOTE-COD_OCORRENCIA_RETORNO.
    w_header_lote-cod_ocorrencia_retorno = '0000000000'.
  ENDIF.

  PERFORM z_format       USING w_header_lote.

ENDFORM.

FORM preenche_segmento_o .
  ASSIGN w_segmento_o TO <wa_data>.
  DATA: codigo_banco(3)       TYPE c.
  DATA: w_block_1(9)        TYPE c,
        w_block_2(10)       TYPE c,
        w_block_3(10)       TYPE c,
        w_block_4(14)       TYPE c,
        w_block_5(10)       TYPE c,
        w_codigo_barras(44) TYPE c,
        w_dv_1(1)           TYPE c,
        w_dv_2(1)           TYPE c,
        w_dv_3(1)           TYPE c,
        w_dv_4(2)           TYPE c,
        lv_part1(11)        TYPE c,
        lv_part2(11)        TYPE c,
        lv_part3(11)        TYPE c,
        lv_part4(11)        TYPE c.

  DATA: lv_date1          TYPE  d VALUE '19971007',
        lv_date2          TYPE  d,
        lv_datediff       TYPE  p,
        lv_datedifftxt(4) TYPE  c,
        lv_timediff       TYPE  p,
        lv_earliest       TYPE  c.

  DATA: lv_fator_mult  TYPE i.
  DATA: lv_pos  TYPE i.
  DATA: lv_calc  TYPE i.
  DATA: lv_tot  TYPE i.
  DATA: lv_rest  TYPE i.

  v_seq = v_seq  + 1.

*-BUG 71594 - 14.01.2022 - JT - inicio
  IF p_bncemp = 'BBD'.
    sequencial_reg_lote =  v_seq."sequencial_reg_lote + 1.
  ENDIF.
*-BUG 71594 - 14.01.2022 - JT - fim

  PERFORM preenche_valor.
  PERFORM add_zero_esquerda USING w_segmento_o-valor_pagamento.
  PERFORM zf_codigo_banco   USING codigo_banco.
  PERFORM add_zero_esquerda USING codigo_banco.
*  PERFORM add_zero_esquerda USING <f_zimp_cabecalho>-lote.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.


  w_segmento_o-cod_banco                      = codigo_banco.
  w_segmento_o-lote_servico                   = w_header_lote-lote_servico.  "proximo."LOTE_SERVICO. " iNFORMAR O NUMERO DO LOTE AO QUAL PERTENCE O REGISTRO. DEVE SER IGUAL AO NÚMERO INFORMADO NO HEADER DO LOTE
  PERFORM add_zero_esquerda USING w_segmento_o-lote_servico.
  w_segmento_o-registro_detalhe_lote          = '3'.

* Inicio - BUG 93715 - NumenIT - FA - 04.11.2022
  IF  p_bncemp = 'BBD'.
*    w_segmento_o-sequencial_reg_lote            = w_segmento_o-sequencial_reg_lote + 1.
    w_segmento_o-sequencial_reg_lote            = v_seq."SEQUENCIAL_REG_LOTE..
    w_segmento_o-cod_instrucao_movimento        = '09'.
  ELSE.
    w_segmento_o-sequencial_reg_lote            = sequencial_reg_lote.
    w_segmento_o-cod_instrucao_movimento        = '00'.
  ENDIF.
*  w_segmento_o-sequencial_reg_lote            = sequencial_reg_lote.

* Fim - BUG 93715 - NumenIT - FA - 04.11.2022

  PERFORM add_zero_esquerda USING w_segmento_o-sequencial_reg_lote.
  w_segmento_o-cod_seg_reg_detalhe            = 'O'.
  w_segmento_o-tipo_movimento                 = '0'.


** BUG 71188 - Ajuste Código de Barras ( Tinha sumido da request DEVK9A0O4Z ) - Inicio
  lv_date2 = <f_zimp_cabecalho>-dt_venc.

  CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
    EXPORTING
      date1            = lv_date1
      date2            = lv_date2
    IMPORTING
      datediff         = lv_datediff
      timediff         = lv_timediff
      earliest         = lv_earliest
    EXCEPTIONS
      invalid_datetime = 1
      OTHERS           = 2.

  IF lv_datediff >= 10000.
    lv_datediff = lv_datediff - 9000.
  ENDIF.

  lv_datedifftxt = lv_datediff.

* IF <f_zimp_cabecalho>-cod_barras+0(4) = '0019' OR
*    <f_zimp_cabecalho>-cod_barras+0(4) = '2372' OR
*    <f_zimp_cabecalho>-cod_barras+0(4) = '0334'.
  IF <f_zimp_cabecalho>-cod_barras+0(4) = '0019' OR
     <f_zimp_cabecalho>-cod_barras+0(4) = '2379' OR
     <f_zimp_cabecalho>-cod_barras+0(4) = '0339' OR
     <f_zimp_cabecalho>-cod_barras+0(4) = '3419' OR
     <f_zimp_cabecalho>-cod_barras+0(4) = '1049'.

    PERFORM f_codigo_barras USING <f_zimp_cabecalho>-cod_barras
                         CHANGING w_segmento_o-cod_barras.

*    CONCATENATE <f_zimp_cabecalho>-cod_barras+0(4)
*                lv_datedifftxt
*                w_segmento_o-valor_pagamento+5(10)
*                '0000000000000000000000000'
*                INTO w_codigo_barras.
*
**** modulo 11
*    lv_fator_mult = 1.
*    lv_pos = 42.
*    DO.
*      lv_fator_mult = lv_fator_mult + 1.
*      IF lv_fator_mult = 10.
*        lv_fator_mult = 2.
*      ENDIF.
*      IF lv_pos < 0.
*        EXIT.
*      ENDIF.
*      lv_calc = w_codigo_barras+lv_pos(1) * lv_fator_mult.
*      lv_tot = lv_tot + lv_calc.
*      lv_pos = lv_pos - 1.
*    ENDDO.
*
*    lv_rest = lv_tot MOD 11.
*    w_dv_4 = 11 - lv_rest.
*
*    IF w_dv_4 = 11 OR  w_dv_4 = 0 OR  w_dv_4 = 10.
*      w_dv_4 = 1.
*      CONDENSE w_dv_4 NO-GAPS.
*    ENDIF.
*
*    CONCATENATE w_codigo_barras(4) w_dv_4(1) w_codigo_barras+4(39) INTO w_codigo_barras.
*
*    w_segmento_o-cod_barras                     =  w_codigo_barras.
**  w_segmento_o-cod_barras                     = <f_zimp_cabecalho>-cod_barras.

  ELSE. " BUG - 71188 - CSB

    lv_part1 = <f_zimp_cabecalho>-cod_barras+0(11).
    lv_part2 = <f_zimp_cabecalho>-cod_barras+12(11).
    lv_part3 = <f_zimp_cabecalho>-cod_barras+24(11).
    lv_part4 = <f_zimp_cabecalho>-cod_barras+36(11).

    CONCATENATE lv_part1 lv_part2 lv_part3 lv_part4 INTO  w_segmento_o-cod_barras .

*    w_segmento_o-cod_barras                     = <f_zimp_cabecalho>-cod_barras.
  ENDIF.

** BUG  71188- Ajuste Código de Barras ( Tinha sumido da request DEVK9A0O4Z ) - fim

  IF vlifnr IS INITIAL. "119896 AJUSTE TRANSAÇÃO PARA PAGAMENTOS DE CONSUMO - PSA
    vlifnr = <f_zimp_detalhe>-lifnr.
  ENDIF.

  IF vlifnr IS NOT INITIAL .
    SELECT SINGLE name1 INTO w_segmento_o-nome_concessionaria
      FROM lfa1 WHERE lifnr = vlifnr.
  ELSE.
    w_segmento_o-nome_concessionaria            = '                              '.
  ENDIF.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_o-data_vencimento.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_o-data_pagamento.
  "W_SEGMENTO_O-DATA_VENCIMENTO                = <F_ZIMP_CABECALHO>-DT_VENC.
  "W_SEGMENTO_O-DATA_PAGAMENTO                 = <F_ZIMP_CABECALHO>-DT_APURACAO.



  w_segmento_o-num_doc_empresa                = <f_zimp_cabecalho>-doc_imposto.
  w_segmento_o-num_doc_banco                  = '                    '.

  PERFORM z_monta_espaco USING w_segmento_o-cnab.
  "PERFORM Z_MONTA_ESPACO USING W_SEGMENTO_O-COD_OCORRENCIA_RETORNO.
*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_segmento_o-cod_ocorrencia_retorno.
  ELSE.
    w_segmento_o-cod_ocorrencia_retorno = '0000000000'.
  ENDIF.
  "PERFORM Z_FORMAT       USING W_SEGMENTO_O.
  APPEND    w_segmento_o TO t_arquivo1.

ENDFORM.


FORM preenche_segmento_w .
  DATA: codigo_banco(3)       TYPE c.

  v_seq = v_seq + 1.
  PERFORM zf_codigo_banco   USING codigo_banco.
  PERFORM add_zero_esquerda USING codigo_banco.

  w_segmento_w-cod_banco                      = codigo_banco.
  w_segmento_w-lote_servico                   = lote_servico.
  PERFORM add_zero_esquerda USING w_segmento_w-lote_servico.

  " ajuste pl - 24092020 - inicio
*  w_segmento_w-lote_servico = w_segmento_o-lote_servico.
  w_segmento_w-lote_servico = w_header_lote-lote_servico.
  " ajuste pl - 24092020 - fim.
  w_segmento_w-reg_detalhe_lote               = '3'.

*** pbi - 43975 - inicio
*-BUG 71594 - 14.01.2022 - JT - inicio
* IF p_bncemp = 'BBD'.
  w_segmento_w-num_seq_reg_lote               = v_seq."sequencial_reg_lote + 1.
* ELSE.
*   w_segmento_w-num_seq_reg_lote               = sequencial_reg_lote.
* ENDIF.
*-BUG 71594 - 14.01.2022 - JT - fim

  PERFORM add_zero_esquerda USING w_segmento_w-num_seq_reg_lote.
  w_segmento_w-cod_seg_reg_detalhe            = 'W'.
  w_segmento_w-num_seq_reg_complementar       = '0'.
  " ajuste pl - 24092020 - inicio
*  PERFORM z_monta_espaco USING w_segmento_w-uso_informacoes_complementares.
*  w_segmento_w-informacao_complementar_1 = <f_zimp_cabecalho>-cod_barras.

  w_segmento_w-uso_informacoes_complementares       = '1'.
  PERFORM z_monta_espaco USING w_segmento_w-informacao_complementar_1.
  " ajuste pl - 24092020 - fim.

  PERFORM z_monta_espaco USING w_segmento_w-informacao_complementar_2.

  " ajuste pl - 24092020 - inicio
*  PERFORM z_monta_espaco USING w_segmento_w-identificador_tributo.
  w_segmento_w-identificador_tributo = '01'.
  " ajuste pl - 24092020 - fim

  " ajuste GR - 13042024 - inicio
  w_segmento_w-cnpj = <f_zimp_cabecalho>-identificador.
  CONCATENATE '01' w_segmento_w-cnpj INTO w_segmento_w-cnpj.
* w_segmento_w-cnpj_branco = '01'.
  " ajuste GR - 13042024 - fim
*** pbi - 43975 - inicio
  IF p_bncemp = 'BBD'.
    MOVE-CORRESPONDING w_segmento_w TO w_bbd_segmento_w.
    " ajuste GR - 13042024 - inicio
    PERFORM z_monta_espaco USING w_bbd_segmento_w-cnpj_branco.
    " ajuste GR - 13042024 - fim
    PERFORM z_monta_espaco USING w_bbd_segmento_w-info_complementar_tributo.
    PERFORM z_monta_espaco USING w_bbd_segmento_w-cnab.
    PERFORM z_monta_espaco USING w_bbd_segmento_w-cod_ocorrencias_retorno.
    w_bbd_segmento_w-info_complementar_tributo      = w_arquivo.

    APPEND    w_bbd_segmento_w TO t_arquivo1.
  ELSE.
    PERFORM z_monta_espaco USING w_segmento_w-info_complementar_tributo.
    PERFORM z_monta_espaco USING w_segmento_w-cnab.
    PERFORM z_monta_espaco USING w_segmento_w-cod_ocorrencias_retorno.
    w_segmento_w-info_complementar_tributo      = w_arquivo.

    APPEND    w_segmento_w TO t_arquivo1.
  ENDIF.

ENDFORM.

FORM preenche_segmento_n.

  DATA: codigo_banco(3)       TYPE c,
        valor_total_pagamento TYPE c LENGTH 15,
        pagamento_simples(13) TYPE c,
        paga                  TYPE c LENGTH 15,
        pagamento(8)          TYPE c,
        conta(9)              TYPE c,
        anterior              TYPE c.

  PERFORM zf_codigo_banco   USING codigo_banco.
  PERFORM add_zero_esquerda USING codigo_banco.
  PERFORM add_zero_esquerda USING <f_zimp_cabecalho>-lote.

  w_segmento_n-cod_banco                    = codigo_banco.
  w_segmento_n-lote_servico                 = lote_servico.
  PERFORM add_zero_esquerda USING w_segmento_n-lote_servico.
  w_segmento_n-registro_detalhe_lote        = '3'.
*** pbi - 43975 - inicio
  IF p_bncemp = 'BBD'.
    w_segmento_n-sequencial_reg_lote          = sequencial_reg_lote + 1 .
    w_segmento_n-cod_instrucao_movimento      = '09'.
  ELSE.
    w_segmento_n-sequencial_reg_lote          = sequencial_reg_lote .
    w_segmento_n-cod_instrucao_movimento      = '00'.
  ENDIF.
  PERFORM add_zero_esquerda USING w_segmento_n-sequencial_reg_lote.

  w_segmento_n-cod_seg_reg_detalhe          = 'N'.
  w_segmento_n-tipo_movimento               = '0'.
  w_segmento_n-num_docto_atribuido_emprresa = <f_zimp_cabecalho>-doc_imposto.
  w_segmento_n-nome_contribuinte            = v_butxt.
  PERFORM z_monta_espaco USING w_segmento_n-num_docto_atribuido_banco.

*  PERFORM Z_MONTA_ESPACO USING W_SEGMENTO_N-NOME_CONTRIBUINTE.

  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_n-data_pagamento.

  WRITE <f_zimp_detalhe>-valor_imp TO valor_total_pagamento.
  CONDENSE valor_total_pagamento.

  WHILE valor_total_pagamento(1) NE '-'.
    IF valor_total_pagamento(1) NE '.'.
      CONCATENATE  paga valor_total_pagamento(1) INTO paga.
    ELSE.
      anterior = valor_total_pagamento(1).
    ENDIF.

    SHIFT valor_total_pagamento.
  ENDWHILE.

  REPLACE ',' IN paga WITH  ' '.

  PERFORM add_zero_esquerda USING paga.

  w_segmento_n-valor_total_pagar            = paga.
  w_segmento_n-informacoes_complementares   = w_arquivo.
  "PERFORM Z_MONTA_ESPACO USING W_SEGMENTO_N-COD_OCORRENCIA_RETORNO.

*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_segmento_n-cod_ocorrencia_retorno.
  ELSE.
    w_segmento_n-cod_ocorrencia_retorno = '0000000000'.
  ENDIF.
  APPEND    w_segmento_n TO t_arquivo1.

ENDFORM.

FORM preenche_trailer_arquivo .

  DATA: codigo_banco(3) TYPE c,
        qtd_regi(6)     TYPE c.

  CLEAR: qtd_regi.
  "Verificar quantidade registro processados.
  LOOP AT t_arquivo1 ASSIGNING FIELD-SYMBOL(<w_arq>).
    DATA(line) = <w_arq>+7(1).
    IF line EQ '0' OR line EQ '1' OR line EQ '3' OR line EQ '5' OR line EQ '9'.
      ADD 1 TO qtd_regi.
    ENDIF.
  ENDLOOP.

  PERFORM zf_codigo_banco USING codigo_banco.

  w_trailer_arquivo-cod_banco             = codigo_banco.
  w_trailer_arquivo-lote_servico          = '9999'.
  w_trailer_arquivo-tipo_registro         = '9'.
  w_trailer_arquivo-cnab                  = ''.

  "lote_servico = lote_servico -1.
  w_trailer_arquivo-qtd_lote              = lote_servico.
  PERFORM add_zero_esquerda USING w_trailer_arquivo-qtd_lote.

  w_trailer_arquivo-qtd_registros_arquivo = ( qtd_regi + 1 ).
  PERFORM add_zero_esquerda USING w_trailer_arquivo-qtd_registros_arquivo.
  IF p_bncemp(2) EQ 'IT'. "Itaú
    PERFORM z_monta_espaco USING w_trailer_arquivo-qtde_contas_para_conc.
  ELSE.
    w_trailer_arquivo-qtde_contas_para_conc = '000000'. "Padrão é zero, porque não se aplica a conciliação.
  ENDIF.
  PERFORM z_monta_espaco USING w_trailer_arquivo-cnab2.
  PERFORM z_format       USING w_trailer_arquivo.

ENDFORM.
*** BUG - 184742 - Inicio - CBRAND
FORM preenche_trailer_lote USING p_lote TYPE any.
*FORM preenche_trailer_lote.
  "BUG - 184742 - Fim - CBRAND

  DATA: codigo_banco(3)       TYPE c.

  PERFORM zf_codigo_banco USING codigo_banco.
  DATA: v_regs_proc_tt(6)       TYPE c,
        v_tot_tittt(18)         TYPE c,
        v_tot_tittt_simples(18) TYPE c,
        v_lote(4)               TYPE c.



*  V_REGS_PROC_TT = V_REGS_PROC.
  v_tot_tittt = v_tot_tit.

*** BUG - 184742 - Inicio - CBRAND
  IF p_lote = 'X'.
    "Verificar quantidade registro processados.
    LOOP AT t_arquivo1 ASSIGNING FIELD-SYMBOL(<w_arq>).
      DATA(line) = <w_arq>+7(1).
      IF line EQ '1' OR line EQ '2' OR line EQ '3' OR line EQ '4' OR line EQ '5'.
        ADD 1 TO v_regs_proc_tt.
      ENDIF.
    ENDLOOP.

    "quantidade de registros do segundo lote - Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- INICIC
    IF v_toal_Reg IS INITIAL.
      v_toal_Reg = v_regs_proc_tt.
      v_toal_Reg = v_regs_proc_tt.
      v_regs_proc_tt = v_regs_proc_tt + 1.
    ELSE.
      v_regs_proc_tt = v_regs_proc_tt - v_toal_Reg.
    ENDIF.
    "quantidade de registros do segundo lote - Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- FIM
    "PERFORM add_zero_esquerda USING proximo.
    PERFORM add_zero_esquerda USING v_regs_proc_tt.
  ELSE.
    "Verificar quantidade registro processados.
    LOOP AT t_arquivo1 ASSIGNING <w_arq>.
      IF  <w_arq>+3(4) =  p_lote.
        line = <w_arq>+7(1).
        IF line EQ '1' OR line EQ '2' OR line EQ '3' OR line EQ '4' OR line EQ '5'.
          ADD 1 TO v_regs_proc_tt.
        ENDIF.
      ENDIF.
    ENDLOOP.
    ADD 1 TO v_regs_proc_tt.
    PERFORM add_zero_esquerda USING v_regs_proc_tt.
  ENDIF.

*** BUG - 184742 - Fim - CBRAND

  w_trailer_lote-cod_banco          = codigo_banco. "3
  w_trailer_lote-lote_servico       = lote_servico. "4
  PERFORM add_zero_esquerda USING w_trailer_lote-lote_servico .
  w_trailer_lote-tipo_registro      = '5'. "1
  w_trailer_lote-canab              = '         '. "9
  w_trailer_lote-qtd_registro_lote  = v_regs_proc_tt . "6 "Analisar o tipo 10 - ISS em forma de boleto #180566 - BG
  PERFORM add_zero_esquerda USING w_trailer_lote-qtd_registro_lote.

  CONDENSE v_tot_tittt.
  WHILE v_tot_tittt(1) NE ' '.
    IF v_tot_tittt(1) NE '.'.
      CONCATENATE  v_tot_tittt_simples v_tot_tittt(1) INTO v_tot_tittt_simples.
    ENDIF.

    SHIFT v_tot_tittt.
  ENDWHILE.
  PERFORM add_zero_esquerda USING v_tot_tittt_simples.
  w_trailer_lote-somatoria_valores_pgtos = v_tot_tittt_simples.
  "W_TRAILER_LOTE-SOMATORIA_VALORES_PGTOS = '000000000000000000'.
  PERFORM z_monta_espaco USING w_trailer_lote-complemento_registro.
  "PERFORM Z_MONTA_ESPACO USING W_TRAILER_LOTE-COD_OCORRENCIAS_PARA_RETORNO.
*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_trailer_lote-cod_ocorrencias_para_retorno.
  ELSE.
    w_trailer_lote-cod_ocorrencias_para_retorno = '0000000000'.
  ENDIF.
  PERFORM z_format       USING w_trailer_lote.
  w_trailer_lote+41(24) = '000000000000000000000000'.
ENDFORM.

FORM f_obtem_proximo_arquivo USING p_proximo_arquivo TYPE any.
  DATA: vl_number TYPE i.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZSEQ_CNAB'
    IMPORTING
      number                  = vl_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_proximo_arquivo = vl_number.
  ENDIF.
ENDFORM.                    " F_OBTEM_PROXIMO_ARQUIVO

FORM f_obtem_proximo_lote USING p_proximo_lote TYPE any.
  DATA: vl_number TYPE i.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZPROX_LOTE'
    IMPORTING
      number                  = vl_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_proximo_lote = vl_number.
  ENDIF.
ENDFORM.                    " F_OBTEM_PROXIMO_LOTE

FORM zf_codigo_banco  USING    p_codigo TYPE any.
  IF p_bncemp = 'BBRA'.
    p_codigo                    = '001'.
  ELSEIF p_bncemp = 'BBD'.
    p_codigo                    = '237'.
  ELSEIF p_bncemp(2) = 'IT'.
    p_codigo                    = '341'.
  ENDIF.
ENDFORM.

FORM zf_nome_banco USING p_nome_banco TYPE any.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_ZERO_ESQUERDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_HEADER_ARQUIVO_AG_MANTENEDOR  text
*----------------------------------------------------------------------*
FORM add_zero_esquerda  USING    p_numero TYPE any.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_numero
    IMPORTING
      output = p_numero.

ENDFORM.

FORM z_monta_espaco USING p_campo TYPE any.

*  DATA:  l_length TYPE i.
*
*  l_length = strlen( p_campo ).
*  DO l_length TIMES.
*    CONCATENATE ''  p_campo INTO p_campo SEPARATED BY space.
*  ENDDO.

  DATA: gv_campo_espc      TYPE string,            "Espaço convertido
        gv_campo_qtde      TYPE i,                 "Quantidade caracteres
        gv_qtde_vezes      TYPE i,                 "Quantidade espaços a serem inseridos
        gv_campo_size(100) TYPE c,                 "Tamanho real do campo
        gv_char            TYPE char1.

  gv_campo_espc = cl_abap_conv_in_ce=>uccp(  '00a0' ).

  DESCRIBE FIELD p_campo LENGTH gv_campo_size IN CHARACTER MODE.
  gv_campo_qtde = strlen( p_campo ).
  gv_qtde_vezes = gv_campo_size  - gv_campo_qtde.

*-PBI 71420 - 12.01.2022 - JT - inicio
*  DO gv_qtde_vezes TIMES.
*     CONCATENATE p_campo space INTO p_campo RESPECTING BLANKS.
**       SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
**      RESPECTING BLANKS.
**    p_campo+gv_campo_qtde(1) = gv_campo_espc.
**    gv_campo_qtde = gv_campo_qtde + 1.
*  ENDDO.

*  REPLACE ALL OCCURRENCES OF '#' IN p_campo WITH space.

*  DO gv_qtde_vezes TIMES.
*    p_campo+gv_campo_qtde(1) = gv_campo_espc.
*    gv_campo_qtde = gv_campo_qtde + 1.
*  ENDDO.
*-PBI 71420 - 12.01.2022 - JT - fim

ENDFORM.

FORM z_format USING wl_dados TYPE any.

  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN wl_dados WITH 'a' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN wl_dados WITH 'e' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'í'     IN wl_dados WITH 'i' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN wl_dados WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN wl_dados WITH 'u' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN wl_dados WITH 'c' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '&'     IN wl_dados WITH 'e'.
  REPLACE ALL OCCURRENCES OF        'º'     IN wl_dados WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '-'     IN wl_dados WITH | | IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '_'     IN wl_dados WITH | | IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '/'     IN wl_dados WITH | | IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        '\'     IN wl_dados WITH | | IGNORING CASE.

  TRANSLATE wl_dados TO UPPER CASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_SEGMENTO_J
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM preenche_segmento_j.

  ASSIGN w_segmento_j TO <wa_data>.

  DATA: codigo_banco(3)       TYPE c.
  DATA: w_block_1(9)        TYPE c,
        w_block_2(10)       TYPE c,
        w_block_3(10)       TYPE c,
        w_block_4(14)       TYPE c,
        w_block_5(10)       TYPE c,
        w_codigo_barras(44) TYPE c,
        w_dv_1(1)           TYPE c,
        w_dv_2(1)           TYPE c,
        w_dv_3(1)           TYPE c,
        w_dv_4(2)           TYPE c,
        lv_part1(11)        TYPE c,
        lv_part2(11)        TYPE c,
        lv_part3(11)        TYPE c,
        lv_part4(11)        TYPE c.

  DATA: lv_date1          TYPE  d VALUE '19971007',
        lv_date2          TYPE  d,
        lv_datediff       TYPE  p,
        lv_datedifftxt(4) TYPE  c,
        lv_timediff       TYPE  p,
        lv_earliest       TYPE  c.

  DATA: lv_fator_mult  TYPE i.
  DATA: lv_pos  TYPE i.
  DATA: lv_calc  TYPE i.
  DATA: lv_tot  TYPE i.
  DATA: lv_rest  TYPE i.

  IF sequencial_reg_lote IS INITIAL.
    sequencial_reg_lote =  sequencial_reg_lote + 1.
  ENDIF.

  w_segmento_j-cod_segmento         = 'J'.
  w_segmento_j-valor_principal      = '0'.
  w_segmento_j-valor_desconto       = '0'.
  w_segmento_j-valor_juros_encargos = '0'.
  w_segmento_j-valor_pagamento      = '0'.
  w_segmento_j-quantidade_moeda     = '000000000000000'.
  w_segmento_j-cod_moeda            = '09'.
  w_segmento_j-num_doc_empresa      = <f_zimp_cabecalho>-doc_imposto.
  w_segmento_j-num_doc_banco        = '                    '.
*
  PERFORM preenche_valor.
*
  PERFORM add_zero_esquerda USING w_segmento_j-valor_principal.
  PERFORM add_zero_esquerda USING w_segmento_j-valor_desconto.
  PERFORM add_zero_esquerda USING w_segmento_j-valor_juros_encargos.
  PERFORM add_zero_esquerda USING w_segmento_j-valor_pagamento.

*---------------------------------------------------------------------
  PERFORM zf_codigo_banco   USING codigo_banco.
  PERFORM add_zero_esquerda USING codigo_banco.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.

  w_segmento_j-cod_banco                      = codigo_banco.
  w_segmento_j-lote_servico                   = w_header_lote-lote_servico.  "proximo."LOTE_SERVICO. " iNFORMAR O NUMERO DO LOTE AO QUAL PERTENCE O REGISTRO. DEVE SER IGUAL AO NÚMERO INFORMADO NO HEADER DO LOTE
  PERFORM add_zero_esquerda USING w_segmento_j-lote_servico.
  w_segmento_j-registro_detalhe_lote          = '3'.
  w_segmento_j-sequencial_reg_lote            = sequencial_reg_lote.
  PERFORM add_zero_esquerda USING w_segmento_j-sequencial_reg_lote.
  w_segmento_j-tipo_movimento                 = '0'.
  w_segmento_j-cod_instrucao_movimento        = '00'.

  lv_date2 = <f_zimp_cabecalho>-dt_venc.

  CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
    EXPORTING
      date1            = lv_date1
      date2            = lv_date2
    IMPORTING
      datediff         = lv_datediff
      timediff         = lv_timediff
      earliest         = lv_earliest
    EXCEPTIONS
      invalid_datetime = 1
      OTHERS           = 2.

  IF lv_datediff >= 10000.
    lv_datediff = lv_datediff - 9000.
  ENDIF.

  lv_datedifftxt = lv_datediff.


**** BUG - 190592 - CBRAND - Inicio
  CONCATENATE <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO DATA(lva_bankl).
  SELECT SINGLE * FROM bnka INTO @DATA(w_bnka_aux)
        WHERE bankl LIKE @lva_bankl.
  IF sy-subrc IS INITIAL.
**** BUG - 190592 - CBRAND - Fim
* IF <f_zimp_cabecalho>-cod_barras+0(4) = '0019' OR
*    <f_zimp_cabecalho>-cod_barras+0(4) = '2372' OR

**** BUG - 190592 - CBRAND - Inicio
**    <f_zimp_cabecalho>-cod_barras+0(4) = '0334'.
*  IF <f_zimp_cabecalho>-cod_barras+0(4) = '0019' OR
*     <f_zimp_cabecalho>-cod_barras+0(4) = '2379' OR
*     <f_zimp_cabecalho>-cod_barras+0(4) = '0339' OR
*     <f_zimp_cabecalho>-cod_barras+0(4) = '3419' OR
*     <f_zimp_cabecalho>-cod_barras+0(4) = '1049'.
**** BUG - 190592 - CBRAND - Fim
    PERFORM f_codigo_barras USING <f_zimp_cabecalho>-cod_barras
                         CHANGING w_segmento_j-cod_barras.

*    CONCATENATE <f_zimp_cabecalho>-cod_barras+0(4)
*                lv_datedifftxt
*                w_segmento_j-valor_pagamento+5(10)
*                '0000000000000000000000000'
*                INTO w_codigo_barras.
**** modulo 11
*    lv_fator_mult = 1.
*    lv_pos = 42.
*    DO.
*      lv_fator_mult = lv_fator_mult + 1.
*      IF lv_fator_mult = 10.
*        lv_fator_mult = 2.
*      ENDIF.
*      IF lv_pos < 0.
*        EXIT.
*      ENDIF.
*      lv_calc = w_codigo_barras+lv_pos(1) * lv_fator_mult.
*      lv_tot = lv_tot + lv_calc.
*      lv_pos = lv_pos - 1.
*    ENDDO.
*
*    lv_rest = lv_tot MOD 11.
*    w_dv_4 = 11 - lv_rest.
*
*    IF w_dv_4 = 11 OR  w_dv_4 = 0 OR  w_dv_4 = 10.
*      w_dv_4 = 1.
*      CONDENSE w_dv_4 NO-GAPS.
*    ENDIF.
*
*    CONCATENATE w_codigo_barras(4) w_dv_4(1) w_codigo_barras+4(39) INTO w_codigo_barras.
*
*    w_segmento_j-cod_barras                     =  w_codigo_barras.
  ELSE. " BUG - 71188 - CSB
    lv_part1 = <f_zimp_cabecalho>-cod_barras+0(11).
    lv_part2 = <f_zimp_cabecalho>-cod_barras+12(11).
    lv_part3 = <f_zimp_cabecalho>-cod_barras+24(11).
    lv_part4 = <f_zimp_cabecalho>-cod_barras+36(11).

    CONCATENATE lv_part1 lv_part2 lv_part3 lv_part4 INTO  w_segmento_j-cod_barras .
  ENDIF.

  IF vlifnr IS NOT INITIAL.
    SELECT SINGLE name1 INTO w_segmento_j-nome_concessionaria
      FROM lfa1 WHERE lifnr = vlifnr.
  ELSE.
    w_segmento_j-nome_concessionaria            = '                              '.
  ENDIF.

*** BUG - 184742 - Inicio - CBRAND
  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN w_segmento_j-nome_concessionaria  WITH 'a' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN w_segmento_j-nome_concessionaria  WITH 'e' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'í'     IN w_segmento_j-nome_concessionaria  WITH 'i' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN w_segmento_j-nome_concessionaria  WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN w_segmento_j-nome_concessionaria  WITH 'u' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN w_segmento_j-nome_concessionaria  WITH 'c' IGNORING CASE.
  TRANSLATE w_segmento_j-nome_concessionaria  TO UPPER CASE.
*** BUG - 184742 - Fim - CBRAND



  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_j-data_vencimento.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_j-data_pagamento.
*---------------------------------------------------------------------
*
  PERFORM z_monta_espaco USING w_segmento_j-cnab.

*** PBI - 43975 - Inicio
  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_segmento_j-cod_ocorrencia_retorno.
  ELSE.
    w_segmento_j-cod_ocorrencia_retorno = '0000000000'.
  ENDIF.

  APPEND    w_segmento_j TO t_arquivo1.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_SEGMENTO_J52
*&---------------------------------------------------------------------*
FORM preenche_segmento_j52.

  UNASSIGN <wa_data>.
  ASSIGN w_segmento_j52 TO <wa_data>.

  DATA: codigo_banco(3) TYPE c,
        l_name1         TYPE lfa1-name1,
        l_stcd1         TYPE lfa1-stcd1.

  DATA: lv_fator_mult  TYPE i.
  DATA: lv_pos  TYPE i.
  DATA: lv_calc  TYPE i.
  DATA: lv_tot  TYPE i.
  DATA: lv_rest  TYPE i.

  CLEAR: w_segmento_j52,
         l_name1.

  l_stcd1 = '0'.
  PERFORM add_zero_esquerda USING l_stcd1.

  sequencial_reg_lote = sequencial_reg_lote + 1.

  IF vlifnr IS NOT INITIAL.
    SELECT SINGLE     name1    stcd1
             INTO ( l_name1, l_stcd1 )
      FROM lfa1
     WHERE lifnr = vlifnr.
  ENDIF.

  PERFORM zf_codigo_banco   USING codigo_banco.
  PERFORM add_zero_esquerda USING codigo_banco.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.

  w_segmento_j52-cod_banco             = codigo_banco.
  w_segmento_j52-lote_servico          = w_header_lote-lote_servico.
  w_segmento_j52-registro_detalhe_lote = '3'.
  w_segmento_j52-sequencial_reg_lote   = sequencial_reg_lote.
  w_segmento_j52-cod_segmento          = 'J'.
  w_segmento_j52-cnab                  = '  '.
  w_segmento_j52-cod_mov_rem           = '00'.
  w_segmento_j52-ident_reg_opcio       = '52'.
  w_segmento_j52-tip_inscricao_pag     = '2'.
  w_segmento_j52-num_inscricao_pag     = w_header_arquivo-num_inscricao_empresa.
  w_segmento_j52-nome_pag              = w_header_arquivo-nome_empresa.
*
  w_segmento_j52-tip_inscricao_ben     = '2'.
  w_segmento_j52-num_inscricao_ben     = l_stcd1.

*** BUG - 184742 - Inicio - CBRAND
  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN l_name1 WITH 'a' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN l_name1 WITH 'e' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF        'í'     IN l_name1 WITH 'i' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN l_name1 WITH 'o' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN l_name1 WITH 'u' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN l_name1 WITH 'c' IGNORING CASE.
  TRANSLATE l_name1 TO UPPER CASE.
*** BUG - 184742 - Fim - CBRAND

  w_segmento_j52-nome_ben              = l_name1.

*
  w_segmento_j52-tip_inscricao_sac     = '0'.
  w_segmento_j52-num_inscricao_sac     = '0'.
  w_segmento_j52-nome_sac              = ' '.

  PERFORM add_zero_esquerda USING w_segmento_j52-lote_servico.
  PERFORM add_zero_esquerda USING w_segmento_j52-sequencial_reg_lote.
  PERFORM add_zero_esquerda USING w_segmento_j52-num_inscricao_pag.
  PERFORM add_zero_esquerda USING w_segmento_j52-num_inscricao_ben.
  PERFORM add_zero_esquerda USING w_segmento_j52-num_inscricao_sac.

  PERFORM z_monta_espaco    USING w_segmento_j52-cnab2.

  APPEND    w_segmento_j52     TO t_arquivo1.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_SEGMENTO_J52
*&---------------------------------------------------------------------*
FORM f_codigo_barras USING p_cod_original
                  CHANGING p_cod_pagto.

  FREE: p_cod_pagto.

  CHECK p_cod_original IS NOT INITIAL.

  DATA: l_cod_ori(47) TYPE c,
        l_cod_pag(44) TYPE c.

  l_cod_ori = p_cod_original.

  l_cod_pag = l_cod_ori+0(4) && l_cod_ori+32(15) &&
              l_cod_ori+4(5) && l_cod_ori+10(10) &&
              l_cod_ori+21(10).

  p_cod_pagto = l_cod_pag.

ENDFORM.

**********************************************************************
*LAYOUT GRU
**********************************************************************

FORM preenche_segmento_o_gru .
  ASSIGN w_segmento_o TO <wa_data>.
  DATA: codigo_banco(3)       TYPE c.
  DATA: w_block_1(9)        TYPE c,
        w_block_2(10)       TYPE c,
        w_block_3(10)       TYPE c,
        w_block_4(14)       TYPE c,
        w_block_5(10)       TYPE c,
        w_codigo_barras(44) TYPE c,
        w_dv_1(1)           TYPE c,
        w_dv_2(1)           TYPE c,
        w_dv_3(1)           TYPE c,
        w_dv_4(2)           TYPE c,
        lv_part1(11)        TYPE c,
        lv_part2(11)        TYPE c,
        lv_part3(11)        TYPE c,
        lv_part4(11)        TYPE c.

  DATA: lv_date1          TYPE  d VALUE '19971007',
        lv_date2          TYPE  d,
        lv_datediff       TYPE  p,
        lv_datedifftxt(4) TYPE  c,
        lv_timediff       TYPE  p,
        lv_earliest       TYPE  c.

  DATA: lv_fator_mult  TYPE i.
  DATA: lv_pos  TYPE i.
  DATA: lv_calc  TYPE i.
  DATA: lv_tot  TYPE i.
  DATA: lv_rest  TYPE i.

  v_seq = v_seq  + 1.

  IF p_bncemp = 'BBD'.
    sequencial_reg_lote =  v_seq."sequencial_reg_lote + 1.
  ENDIF.

  PERFORM preenche_valor.
  PERFORM add_zero_esquerda USING w_segmento_o-valor_pagamento.
  PERFORM zf_codigo_banco   USING codigo_banco.
  PERFORM add_zero_esquerda USING codigo_banco.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.
  PERFORM add_zero_esquerda USING w_header_lote-lote_servico.


  w_segmento_o-cod_banco                      = codigo_banco.
  w_segmento_o-lote_servico                   = w_header_lote-lote_servico.  "proximo."LOTE_SERVICO. " iNFORMAR O NUMERO DO LOTE AO QUAL PERTENCE O REGISTRO. DEVE SER IGUAL AO NÚMERO INFORMADO NO HEADER DO LOTE
  PERFORM add_zero_esquerda USING w_segmento_o-lote_servico.
  w_segmento_o-registro_detalhe_lote          = '3'.

  IF  p_bncemp = 'BBD'.
    w_segmento_o-sequencial_reg_lote            = v_seq."SEQUENCIAL_REG_LOTE..
    w_segmento_o-cod_instrucao_movimento        = '09'.
  ELSE.
    w_segmento_o-sequencial_reg_lote            = sequencial_reg_lote.
    w_segmento_o-cod_instrucao_movimento        = '00'.
  ENDIF.

  PERFORM add_zero_esquerda USING w_segmento_o-sequencial_reg_lote.
  w_segmento_o-cod_seg_reg_detalhe            = 'O'.
  w_segmento_o-tipo_movimento                 = '0'.

  lv_date2 = <f_zimp_cabecalho>-dt_venc.

  CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
    EXPORTING
      date1            = lv_date1
      date2            = lv_date2
    IMPORTING
      datediff         = lv_datediff
      timediff         = lv_timediff
      earliest         = lv_earliest
    EXCEPTIONS
      invalid_datetime = 1
      OTHERS           = 2.

  IF lv_datediff >= 10000.
    lv_datediff = lv_datediff - 9000.
  ENDIF.

  lv_datedifftxt = lv_datediff.

  IF <f_zimp_cabecalho>-cod_barras+0(4) = '0019' OR
     <f_zimp_cabecalho>-cod_barras+0(4) = '2379' OR
     <f_zimp_cabecalho>-cod_barras+0(4) = '0339' OR
     <f_zimp_cabecalho>-cod_barras+0(4) = '3419' OR
     <f_zimp_cabecalho>-cod_barras+0(4) = '1049'.

    PERFORM f_codigo_barras USING <f_zimp_cabecalho>-cod_barras
                         CHANGING w_segmento_o-cod_barras.

  ELSE. " BUG - 71188 - CSB

    lv_part1 = <f_zimp_cabecalho>-cod_barras+0(11).
    lv_part2 = <f_zimp_cabecalho>-cod_barras+12(11).
    lv_part3 = <f_zimp_cabecalho>-cod_barras+24(11).
    lv_part4 = <f_zimp_cabecalho>-cod_barras+36(11).

    CONCATENATE lv_part1 lv_part2 lv_part3 lv_part4 INTO  w_segmento_o-cod_barras .

  ENDIF.

  IF vlifnr IS NOT INITIAL.
    SELECT SINGLE name1 INTO w_segmento_o-nome_concessionaria
      FROM lfa1 WHERE lifnr = vlifnr.
  ELSE.
    w_segmento_o-nome_concessionaria            = '                              '.
  ENDIF.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_o-data_vencimento.
  CONCATENATE <f_zimp_cabecalho>-dt_venc+6(2) <f_zimp_cabecalho>-dt_venc+4(2) <f_zimp_cabecalho>-dt_venc+0(4) INTO  w_segmento_o-data_pagamento.


  w_segmento_o-num_doc_empresa                = <f_zimp_cabecalho>-doc_imposto.
  w_segmento_o-num_doc_banco                  = '                    '.

  PERFORM z_monta_espaco USING w_segmento_o-cnab.

  IF p_bncemp = 'BBD'.
    PERFORM z_monta_espaco USING w_segmento_o-cod_ocorrencia_retorno.
  ELSE.
    w_segmento_o-cod_ocorrencia_retorno = '0000000000'.
  ENDIF.

  APPEND    w_segmento_o TO t_arquivo1.

ENDFORM.

FORM preenche_segmento_w_gru .

**********************************************************************
* PADRAO PARA TODOS (CONTROLE, SERVIÇO E COMPLEMENTO DE REGISTRO)
**********************************************************************
  CONDENSE codigo_banco NO-GAPS.
  UNPACK codigo_banco TO gru_seg_w-cod_banco.

  DATA: aux_lote_servico(4) TYPE c.
  aux_lote_servico = lote_servico.
  CONDENSE aux_lote_servico NO-GAPS.
  UNPACK aux_lote_servico TO gru_seg_w-lote_servico.

  gru_seg_w-reg_detalhe_lote = '3'.

  v_seq = v_seq + 1.
  DATA: aux_num_seq_reg_lote(5) TYPE c.
  aux_num_seq_reg_lote = v_seq.
  CONDENSE aux_num_seq_reg_lote NO-GAPS.
  UNPACK aux_num_seq_reg_lote TO gru_seg_w-num_seq_reg_lote.

  gru_seg_w-cod_seg_reg_detalhe            = 'W'.
  gru_seg_w-num_seq_reg_complementar       = '0'.
  gru_seg_w-uso_informacoes_complementares = '9'.
**********************************************************************

**********************************************************************
*PADRAO RODAPÉ (RESERVADO E OCORRENCIA )
**********************************************************************
  gru_seg_w-cnab  = ''.
  UNPACK gru_seg_w-cod_ocorrencias_retorno TO gru_seg_w-cod_ocorrencias_retorno.

**********************************************************************

**********************************************************************
* SE FOR BRASIL
**********************************************************************
  IF p_bncemp = 'BBRA'.

*Inicio Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779
    IF <f_zimp_cabecalho>-zimp_lanc_impost IS NOT INITIAL.
      gru_seg_w-numero_ref = <f_zimp_cabecalho>-zimp_lanc_impost.
    ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779

    UNPACK gru_seg_w-numero_ref TO gru_seg_w-numero_ref.
    CONCATENATE <f_zimp_cabecalho>-dt_apuracao+4(2) <f_zimp_cabecalho>-dt_apuracao+0(4) INTO  gru_seg_w-competencia.

    UNPACK <f_zimp_cabecalho>-identificador TO gru_seg_w-cnpj.
    SELECT SINGLE valor_imp AS valor_principal FROM zimp_lanc_imp_ct WHERE doc_imposto = @<f_zimp_cabecalho>-doc_imposto AND   bukrs = @<f_zimp_cabecalho>-bukrs AND cod_abertura = '01' INTO @DATA(vlr_principal).

    DATA: aux_vlr_principal(14) TYPE c.
    aux_vlr_principal = vlr_principal.

    REPLACE '.' IN aux_vlr_principal WITH ''.
    CONDENSE aux_vlr_principal NO-GAPS.
    UNPACK aux_vlr_principal TO gru_seg_w-valor_principal.
    UNPACK gru_seg_w-desconto TO gru_seg_w-desconto.
    gru_seg_w-branco1 = ''.
    UNPACK gru_seg_w-outras_deducoes TO gru_seg_w-outras_deducoes.

    SELECT SINGLE valor_imp AS mora_multa FROM zimp_lanc_imp_ct WHERE doc_imposto = @<f_zimp_cabecalho>-doc_imposto AND   bukrs = @<f_zimp_cabecalho>-bukrs AND cod_abertura = '03' INTO @DATA(vlr_mora_multa).
    DATA: aux_vlr_mora_multa(14) TYPE c.
    aux_vlr_mora_multa = vlr_mora_multa.
    REPLACE '.' IN aux_vlr_mora_multa WITH ''.
    CONDENSE aux_vlr_mora_multa NO-GAPS.
    UNPACK aux_vlr_mora_multa TO gru_seg_w-mora_multa.

    SELECT SINGLE valor_imp AS juros_encargos FROM zimp_lanc_imp_ct WHERE doc_imposto = @<f_zimp_cabecalho>-doc_imposto AND   bukrs = @<f_zimp_cabecalho>-bukrs AND cod_abertura = '05' INTO @DATA(vlr_juros_encargos).
    DATA: aux_vlr_juros_encargos(14) TYPE c.
    aux_vlr_juros_encargos = vlr_juros_encargos.
    REPLACE '.' IN aux_vlr_juros_encargos WITH ''.
    CONDENSE aux_vlr_juros_encargos NO-GAPS.
    UNPACK aux_vlr_juros_encargos TO gru_seg_w-juros_encargos.

    SELECT SINGLE SUM( valor_imp ) AS outros_acrescimos FROM zimp_lanc_imp_ct WHERE doc_imposto = @<f_zimp_cabecalho>-doc_imposto AND   bukrs = @<f_zimp_cabecalho>-bukrs AND cod_abertura IN ('02', '06') INTO @DATA(vlr_outros_acrescimos).

    DATA: aux_vlr_outros_acrescimos(14) TYPE c.
    aux_vlr_outros_acrescimos = vlr_outros_acrescimos.
    REPLACE '.' IN aux_vlr_outros_acrescimos WITH ''.
    CONDENSE aux_vlr_outros_acrescimos NO-GAPS.
    UNPACK aux_vlr_outros_acrescimos TO gru_seg_w-outros_acrescimos.

    gru_seg_w-branco2    = ''.
    gru_seg_w-identificador_tributo = '88'.
    gru_seg_w-info_complementar_tributo = ''.


  ENDIF.

**********************************************************************
**********************************************************************
* SE FOR BRADESCO
**********************************************************************
  IF p_bncemp = 'BBD'.

    UNPACK gru_seg_w-inf_compl1 TO gru_seg_w-inf_compl1.
    UNPACK gru_seg_w-inf_compl2 TO gru_seg_w-inf_compl2.
    UNPACK gru_seg_w-inf_tributo TO gru_seg_w-inf_tributo.
    UNPACK gru_seg_w-codigo_receita_tributo TO gru_seg_w-codigo_receita_tributo.
    SELECT SINGLE CASE WHEN stkzn = '01' THEN '02' ELSE '01' END AS contribuinte FROM lfa1 WHERE lifnr = @vlifnr INTO @data(gru_seg_w-tipo_id_contribuinte). "CNPJ 01 CPF 02
    UNPACK <f_zimp_cabecalho>-identificador TO gru_seg_w-identificacao_contribuinte.
    UNPACK gru_seg_w-identificador_fgts TO gru_seg_w-identificador_fgts.
    UNPACK gru_seg_w-lacre_conectividade_social TO gru_seg_w-lacre_conectividade_social .
    UNPACK gru_seg_w-dg_lacre_conectividade TO gru_seg_w-dg_lacre_conectividade .
    UNPACK gru_seg_w-reservado TO gru_seg_w-reservado .

  ENDIF.

**********************************************************************
* Cabecalho
*    gru_seg_w-cod_banco
*    gru_seg_w-lote_servico
*    gru_seg_w-reg_detalhe_lote
*    gru_seg_w-num_seq_reg_lote
*    gru_seg_w-cod_seg_reg_detalhe
*    gru_seg_w-num_seq_reg_complementar
**********************************************************************
* Rodapé
*        gru_seg_w-cnab
*        gru_seg_w-cod_ocorrencias_retorno
**********************************************************************


  IF p_bncemp = 'BBRA'. "BANCO BRASIL

    CONCATENATE
      gru_seg_w-cod_banco
      gru_seg_w-lote_servico
      gru_seg_w-reg_detalhe_lote
      gru_seg_w-num_seq_reg_lote
      gru_seg_w-cod_seg_reg_detalhe
      gru_seg_w-num_seq_reg_complementar
       gru_seg_w-uso_informacoes_complementares
       gru_seg_w-numero_ref
       gru_seg_w-competencia
       gru_seg_w-cnpj
       gru_seg_w-valor_principal
       gru_seg_w-desconto
       gru_seg_w-branco1
       gru_seg_w-outras_deducoes
       gru_seg_w-mora_multa
       gru_seg_w-juros_encargos
       gru_seg_w-outros_acrescimos
       gru_seg_w-branco2
       gru_seg_w-identificador_tributo
       gru_seg_w-info_complementar_tributo
       gru_seg_w-cnab
       gru_seg_w-cod_ocorrencias_retorno
       INTO DATA(w_seg_rodape_bras) RESPECTING BLANKS.

    APPEND    w_seg_rodape_bras TO t_arquivo1.

  ELSEIF p_bncemp = 'BBD'. "BANCO BRADESCO

    CONCATENATE
      gru_seg_w-cod_banco
      gru_seg_w-lote_servico
      gru_seg_w-reg_detalhe_lote
      gru_seg_w-num_seq_reg_lote
      gru_seg_w-cod_seg_reg_detalhe
      gru_seg_w-num_seq_reg_complementar
      gru_seg_w-uso_informacoes_complementares
      gru_seg_w-inf_compl1
      gru_seg_w-inf_compl2
      gru_seg_w-inf_tributo
      gru_seg_w-codigo_receita_tributo
      gru_seg_w-tipo_id_contribuinte
      gru_seg_w-identificacao_contribuinte
      gru_seg_w-identificador_fgts
      gru_seg_w-lacre_conectividade_social
      gru_seg_w-dg_lacre_conectividade
      gru_seg_w-cnab
      gru_seg_w-cod_ocorrencias_retorno
           INTO DATA(w_seg_rodape_brad) RESPECTING BLANKS.

    APPEND    w_seg_rodape_brad TO t_arquivo1.

  ENDIF.

ENDFORM.
INCLUDE: zimp61_pix.
*&---------------------------------------------------------------------*
*& Form zf_monta_arquivo_new
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_monta_arquivo_new .
*** Fiz uma cópia do  zf_monta_arquivo
*** Grandes alterações e limpei também boa parte dos comentário nesse form

  CLEAR: v_tot_tit.
  DATA: v_tp_lanc(2).
  DATA: lva_lote_c(4).

  REFRESH t_arquivo.

*------------------------------
*-verifica qual segmento
*------------------------------
  CLEAR v_segmento.

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho> INDEX 1.

*Inicio Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
***  IF <f_zimp_cabecalho>-tp_imposto EQ '07'.
  IF <f_zimp_cabecalho>-tp_imposto EQ '07' OR <f_zimp_cabecalho>-tp_imposto EQ '08'.
*Fim Alteração - Leandro Valentim Ferreira - 21.07.23 - #115638
    v_segmento = 'O'.
  ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'. "PSA se for GRU  -> adcionando o imposto 14 = GRU
    v_segmento = 'O'.
  ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- INICIO

    CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
    SELECT SINGLE * FROM bnka INTO @DATA(wa_BNKA)
     WHERE bankl LIKE @v_codbanco.
    IF sy-subrc IS INITIAL.
      v_segmento = 'J'.
    ELSE.
      v_segmento = 'O'.
    ENDIF.
  ELSE.
    v_segmento = 'N'.
  ENDIF.

  CLEAR codigo_banco.
  PERFORM zf_codigo_banco      USING codigo_banco.
  SORT t_zimp_detalhe BY doc_imposto cod_abertura bukrs.
  lote_servico = 1.
  sequencial_reg_lote = 1.
  PERFORM prenche_header_arquivo.

  APPEND w_header_arquivo TO t_arquivo1.


** BUG - 184742 - CBRAND - Inicio
  IF <f_zimp_cabecalho>-tp_imposto EQ '11' AND  p_bncemp = 'BBD'.
    LOOP AT t_zimp_cabecalho INTO DATA(w_zimp).
      CONCATENATE w_zimp-cod_barras+0(3) '%' INTO v_codbanco.
      SELECT SINGLE * FROM bnka INTO @DATA(w_bnka_aux)
            WHERE bankl LIKE @v_codbanco.
      IF sy-subrc IS INITIAL. "SE CODIGO BARRAS = COMECA COM BANCO SEGMENTO 'J' Se não é 'O'.
        IF w_zimp-cod_barras+0(3) = codigo_banco.
          APPEND w_zimp TO t_zimp_cabecalho_2.
        ELSE.
          APPEND w_zimp TO t_zimp_cabecalho_3.
        ENDIF.
      ELSE. "Se não é 'O'.
        APPEND w_zimp TO t_zimp_cabecalho_aux.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT t_zimp_cabecalho INTO w_zimp.
      CONCATENATE w_zimp-cod_barras+0(3) '%' INTO v_codbanco.
      SELECT SINGLE * FROM bnka INTO @w_bnka_aux
            WHERE bankl LIKE @v_codbanco.
      IF sy-subrc IS INITIAL. "SE CODIGO BARRAS = COMECA COM BANCO SEGMENTO 'J' Se não é 'O'.
        IF w_zimp-cod_barras+0(3) = codigo_banco.
          APPEND w_zimp TO t_zimp_cabecalho_2.
        ELSE.
          APPEND w_zimp TO t_zimp_cabecalho_3.
        ENDIF.
      ELSE. "Se não é 'O'.
        APPEND w_zimp TO t_zimp_cabecalho_aux.
      ENDIF.
    ENDLOOP.
  ENDIF.

*** QUAL CABEÇALHO É 'O' E QUAL É 'J' ? - t_zimp_cabecalho_2 'J'
  IF t_zimp_cabecalho_2[] IS NOT INITIAL. "lote 30

    CLEAR: w_header_lote, v_tot_tit.

    DATA(lva_lote) = 1.
    lva_lote_c = lva_lote.

    PERFORM add_zero_esquerda USING lva_lote_c.

    SORT t_zimp_cabecalho_2  BY cod_barras.

    sequencial_reg_lote = 1.
    PERFORM preenche_header_lote USING 'J'.

    IF p_bncemp = 'BBD'.
      sequencial_reg_lote = 0.
      MOVE-CORRESPONDING w_header_lote TO w_header_lote_bbd.

      IF <f_zimp_cabecalho>-tp_imposto EQ '11' .
        w_header_lote_bbd-tipo_servico       =  '20'.
        w_header_lote_bbd-forma_lancamento   =  '31'.
        w_header_lote_bbd-num_vs_layout	     =  '040'.
      ENDIF.

      APPEND w_header_lote_bbd    TO t_arquivo1.
    ELSE.
      w_header_lote-forma_lancamento = '30'.
      APPEND w_header_lote    TO t_arquivo1.
    ENDIF.


    LOOP AT t_zimp_cabecalho_2 ASSIGNING <f_zimp_cabecalho>.

      CASE <f_zimp_cabecalho>-tp_imposto.
        WHEN'01'. " darf simples
          PERFORM zf_monta_tp_darf_simples.
        WHEN'02'. " darf normal/preto
          PERFORM zf_monta_tp_darf.
        WHEN '03'.
          PERFORM zf_monta_tp_gps.
        WHEN '04' OR '05' OR '06'.
          PERFORM zf_monta_tp_gare.
        WHEN '07'.
          PERFORM zf_monta_tp_fgts.
        WHEN '13'.
          PERFORM zf_monta_tp_ipva.
        WHEN '15'.
          PERFORM zf_monta_tp_licenciamento.
        WHEN '16'.
          PERFORM zf_monta_tp_darj.
      ENDCASE.

      APPEND w_arquivo TO t_arquivo.

      v_regs_proc = v_regs_proc + 1.

      PERFORM f_tbatch.
      IF <f_zimp_cabecalho>-tp_imposto EQ '07'.
        PERFORM preenche_segmento_o.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'.
        PERFORM preenche_segmento_o_gru.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w_gru.
      ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU

        CLEAR: v_codbanco.
        CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
        SELECT SINGLE * FROM bnka INTO @DATA(w_BNKA)
         WHERE bankl LIKE @v_codbanco.
        IF sy-subrc IS INITIAL.
          PERFORM preenche_segmento_j.
          PERFORM preenche_segmento_j52.
        ELSE.
          PERFORM preenche_segmento_o.
        ENDIF.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '08'.
        PERFORM preenche_segmento_o.
      ELSE.
        PERFORM preenche_segmento_n.
      ENDIF.
      v_tot_tit = v_tot_tit + v_tit.
      sequencial_reg_lote = sequencial_reg_lote + 1.
    ENDLOOP.
    PERFORM preenche_trailer_lote USING lva_lote_c.
    APPEND w_trailer_lote    TO t_arquivo1.
  ENDIF.
*----------------------------- Inicio Lote 31
  IF t_zimp_cabecalho_3[] IS NOT INITIAL. "lote 31

    CLEAR: w_header_lote, v_tot_tit.

    lva_lote = lva_lote + 1.
    lva_lote_c = lva_lote.

    PERFORM add_zero_esquerda USING lva_lote_c.

    lote_servico = lva_lote.

    SORT t_zimp_cabecalho_3  BY cod_barras.

    sequencial_reg_lote = 1.
    PERFORM preenche_header_lote USING 'J'.

    IF p_bncemp = 'BBD'.
      sequencial_reg_lote = 0.
      MOVE-CORRESPONDING w_header_lote TO w_header_lote_bbd.

      IF <f_zimp_cabecalho>-tp_imposto EQ '11' .
        w_header_lote_bbd-tipo_servico       =  '20'.
        w_header_lote_bbd-forma_lancamento   =  '31'.
        w_header_lote_bbd-num_vs_layout	     =  '040'.
      ENDIF.

      APPEND w_header_lote_bbd    TO t_arquivo1.
    ELSE.
      w_header_lote-forma_lancamento = '31'.
      APPEND w_header_lote    TO t_arquivo1.
    ENDIF.

    LOOP AT t_zimp_cabecalho_3 ASSIGNING <f_zimp_cabecalho>.

      CASE <f_zimp_cabecalho>-tp_imposto.
        WHEN'01'. " darf simples
          PERFORM zf_monta_tp_darf_simples.
        WHEN'02'. " darf normal/preto
          PERFORM zf_monta_tp_darf.
        WHEN '03'.
          PERFORM zf_monta_tp_gps.
        WHEN '04' OR '05' OR '06'.
          PERFORM zf_monta_tp_gare.
        WHEN '07'.
          PERFORM zf_monta_tp_fgts.
        WHEN '13'.
          PERFORM zf_monta_tp_ipva.
        WHEN '15'.
          PERFORM zf_monta_tp_licenciamento.
        WHEN '16'.
          PERFORM zf_monta_tp_darj.
      ENDCASE.

      APPEND w_arquivo TO t_arquivo.

      v_regs_proc = v_regs_proc + 1.

      PERFORM f_tbatch.
      IF <f_zimp_cabecalho>-tp_imposto EQ '07'.
        PERFORM preenche_segmento_o.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'.
        PERFORM preenche_segmento_o_gru.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w_gru.
      ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU
        CLEAR: v_codbanco.
        CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
        SELECT SINGLE * FROM bnka INTO @w_bnka
         WHERE bankl LIKE @v_codbanco.
        IF sy-subrc IS INITIAL.
          PERFORM preenche_segmento_j.
          PERFORM preenche_segmento_j52.
        ELSE.
          PERFORM preenche_segmento_o.
        ENDIF.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '08'.
        PERFORM preenche_segmento_o.
      ELSE.
        PERFORM preenche_segmento_n.
      ENDIF.
      v_tot_tit = v_tot_tit + v_tit.
      sequencial_reg_lote = sequencial_reg_lote + 1.
    ENDLOOP.
    PERFORM preenche_trailer_lote USING lva_lote_c.
    APPEND w_trailer_lote    TO t_arquivo1.
  ENDIF.
*----------------------------- Fim Lote 31
*----------------------------- Inicio - HEADER LOTE TIPO = 'O'.
  IF t_zimp_cabecalho_aux[] IS NOT INITIAL.

    lva_lote = lva_lote + 1.
    lva_lote_c = lva_lote.

    PERFORM add_zero_esquerda USING lva_lote_c.

    sequencial_reg_lote = 1.
    lote_servico = lva_lote.
    CLEAR: w_header_lote, v_tot_tit.
    PERFORM preenche_header_lote USING 'O'.
    IF p_bncemp = 'BBD'.
      sequencial_reg_lote = 0.
      MOVE-CORRESPONDING w_header_lote TO w_header_lote_bbd.
      IF <f_zimp_cabecalho>-tp_imposto EQ '11' .
        w_header_lote_bbd-tipo_servico       =  '22'.
        w_header_lote_bbd-forma_lancamento   =  '11'.
        w_header_lote_bbd-num_vs_layout	     =  '012'.
      ENDIF.

      APPEND w_header_lote_bbd    TO t_arquivo1.
    ELSE.
      APPEND w_header_lote    TO t_arquivo1.
    ENDIF.

    LOOP AT t_zimp_cabecalho_aux ASSIGNING <f_zimp_cabecalho>.

      CASE <f_zimp_cabecalho>-tp_imposto.
        WHEN'01'. " darf simples
          PERFORM zf_monta_tp_darf_simples.
        WHEN'02'. " darf normal/preto
          PERFORM zf_monta_tp_darf.
        WHEN '03'.
          PERFORM zf_monta_tp_gps.
        WHEN '04' OR '05' OR '06'.
          PERFORM zf_monta_tp_gare.
        WHEN '07'.
          PERFORM zf_monta_tp_fgts.
        WHEN '13'.
          PERFORM zf_monta_tp_ipva.
        WHEN '15'.
          PERFORM zf_monta_tp_licenciamento.
        WHEN '16'.
          PERFORM zf_monta_tp_darj.
      ENDCASE.

      APPEND w_arquivo TO t_arquivo.

      v_regs_proc = v_regs_proc + 1.

      PERFORM f_tbatch.
      IF <f_zimp_cabecalho>-tp_imposto EQ '07'.
        PERFORM preenche_segmento_o.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'.
        PERFORM preenche_segmento_o_gru.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w_gru.
      ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU
        CLEAR: v_codbanco.
        CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
        SELECT SINGLE * FROM bnka INTO @DATA(w_BNK)
         WHERE bankl LIKE @v_codbanco.
        IF sy-subrc IS INITIAL.
          PERFORM preenche_segmento_j.
          PERFORM preenche_segmento_j52.
        ELSE.
          PERFORM preenche_segmento_o.
        ENDIF.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '08'.
        PERFORM preenche_segmento_o.
      ELSE.
        PERFORM preenche_segmento_n.
      ENDIF.

      v_tot_tit = v_tot_tit + v_tit.
      sequencial_reg_lote = sequencial_reg_lote + 1.

    ENDLOOP.
    PERFORM preenche_trailer_lote USING lva_lote_c.
    APPEND w_trailer_lote    TO t_arquivo1.
  ENDIF.

  PERFORM preenche_trailer_arquivo.
  APPEND w_trailer_arquivo TO t_arquivo1.

  "Adicionar mais uma linha em branco quando for banco Bradesco, em branco.
  IF p_bncemp EQ 'BBD'.
    CLEAR: w_trailer_arquivo.
    APPEND w_trailer_arquivo TO t_arquivo1.
  ENDIF.

  DESCRIBE TABLE t_arquivo1 LINES DATA(l_lines_arq).
  LOOP AT t_arquivo1  INTO DATA(w_arq).
    IF p_bncemp = 'BBD'.
      CHECK sy-tabix < l_lines_arq.
    ENDIF.
    w_arq+240(2)         = cl_abap_char_utilities=>cr_lf.
    MODIFY t_arquivo1 FROM w_arq INDEX sy-tabix.
  ENDLOOP.

  PERFORM zf_grava_arquivo.
  lote_servico = lote_servico + 1.
ENDFORM.                    " ZF_MONTA_ARQUIVO
