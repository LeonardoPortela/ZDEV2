*----------------------------------------------------------------------*
***INCLUDE LZBPCF05 .
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_j_1bnfdoc,
    belnr     TYPE j_1bnfdoc-belnr,
    docnum    TYPE j_1bnfdoc-docnum,
    pstdat    TYPE j_1bnfdoc-pstdat,
    bukrs     TYPE j_1bnfdoc-bukrs,
    series    TYPE j_1bnfdoc-series,
    nftype    TYPE j_1bnfdoc-nftype,
    docdat    TYPE j_1bnfdoc-docdat,
    crenam    TYPE j_1bnfdoc-crenam,
    model     TYPE j_1bnfdoc-model,
    nfnum     TYPE j_1bnfdoc-nfnum,
    branch    TYPE j_1bnfdoc-branch,
    parid     TYPE j_1bnfdoc-parid,
    nfe       TYPE j_1bnfdoc-nfe,
    nfenum    TYPE j_1bnfdoc-nfenum,
    partyp    TYPE j_1bnfdoc-partyp,
    nftot     TYPE j_1bnfdoc-nftot,
    direct    TYPE j_1bnfdoc-direct,
    crt_bupla TYPE j_1bnfdoc-crt_bupla, " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) - SMC
    cancel    TYPE j_1bnfdoc-cancel,


  END   OF ty_j_1bnfdoc,

  BEGIN OF ty_shtyp,
    vbeln TYPE vbrp-vbeln,
    shtyp TYPE vttk-shtyp,
  END OF ty_shtyp,


  BEGIN OF ty_j_1bnfstx,
    docnum TYPE j_1bnfstx-docnum,
    taxtyp TYPE j_1bnfstx-taxtyp,
    taxval TYPE j_1bnfstx-taxval,
    base   TYPE j_1bnfstx-base,
    othbas TYPE j_1bnfstx-othbas,
    excbas TYPE j_1bnfstx-excbas,
  END   OF ty_j_1bnfstx,

  BEGIN OF ty_j_1baj,
    taxtyp TYPE j_1baj-taxtyp,
    taxgrp TYPE j_1baj-taxgrp,
  END   OF ty_j_1baj,

  BEGIN OF ty_j_1bnflin,
    docnum TYPE j_1bnflin-docnum,
    cfop   TYPE j_1bnflin-cfop,
    menge  TYPE j_1bnflin-menge,
    meins  TYPE j_1bnflin-meins,
    netwrt TYPE j_1bnflin-netwrt,
    matnr  TYPE j_1bnflin-matnr,
    refkey TYPE j_1bnflin-refkey,
  END   OF ty_j_1bnflin,


  BEGIN OF ty_j_1batl1v,
    taxlaw TYPE j_1batl1v-taxlaw,
  END OF ty_j_1batl1v,

  BEGIN OF ty_vbfa_doc,
    docnum TYPE j_1bnflin-docnum,
  END   OF ty_vbfa_doc,

  BEGIN OF ty_remessa,
    refkey        LIKE vbfa-vbeln,
    ch_referencia LIKE zsdt0001-ch_referencia,
    vbeln         LIKE lips-vbeln,
    nr_romaneio   LIKE zsdt0001-nr_romaneio,
    vgbel         LIKE lips-vgbel,
    mjahr         TYPE vbfa-mjahr,
  END OF ty_remessa,

  BEGIN OF ty_remessa_3,
    vbeln  LIKE vbfa-vbeln,
    refkey LIKE bkpf-awkey,
  END   OF ty_remessa_3,

  BEGIN OF ty_remessa2,
    refkey LIKE vbfa-vbeln,
    vgbel  LIKE lips-vgbel,
  END OF ty_remessa2,

  BEGIN OF ty_t001,
    bukrs TYPE t001-bukrs,
    butxt TYPE t001-butxt,
  END   OF ty_t001,

  BEGIN OF ty_t001w,
    werks TYPE t001w-werks,
    name1 TYPE t001w-name1,
  END   OF ty_t001w,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
  END   OF ty_lfa1,

  BEGIN OF ty_j_1bnfe_active,
    docnum  TYPE j_1bnfe_active-docnum,
    docsta  TYPE j_1bnfe_active-docsta,
    cancel  TYPE j_1bnfe_active-cancel,
    scssta  TYPE j_1bnfe_active-scssta,
    regio   TYPE j_1bnfe_active-regio,
    nfyear  TYPE j_1bnfe_active-nfyear,
    nfmonth TYPE j_1bnfe_active-nfmonth,
    stcd1   TYPE j_1bnfe_active-stcd1,
    model   TYPE j_1bnfe_active-model,
    serie   TYPE j_1bnfe_active-serie,
    nfnum9  TYPE j_1bnfe_active-nfnum9,
    docnum9 TYPE j_1bnfe_active-docnum9,
    cdv     TYPE j_1bnfe_active-cdv,
  END   OF ty_j_1bnfe_active,

  BEGIN OF ty_vbrp,
    vbeln TYPE vbrp-vbeln,
    kursk TYPE vbrp-kursk,
  END   OF ty_vbrp,

  BEGIN OF ty_vbrp_aux,
    awkey TYPE bkpf-awkey,
  END   OF ty_vbrp_aux,

  BEGIN OF ty_bkpf,
    bukrs TYPE bkpf-bukrs,
    gjahr TYPE bkpf-gjahr,
    awkey TYPE bkpf-awkey,
    belnr TYPE bkpf-belnr,
    cputm TYPE bkpf-cputm,
    cpudt TYPE bkpf-cpudt,
  END   OF ty_bkpf,

  " Adicionado BSID e BSAD conforme solicitação chamado 44124
  BEGIN OF ty_bsid,
    bukrs TYPE bsid-bukrs,
    belnr TYPE bsid-belnr,
    gjahr TYPE bsid-gjahr,
    dmbtr TYPE bsid-dmbtr,
    dmbe2 TYPE bsid-dmbe2,
    shkzg TYPE bsid-shkzg,
  END   OF ty_bsid,

  BEGIN OF ty_bsad,
    bukrs TYPE bsad-bukrs,
    belnr TYPE bsad-belnr,
    gjahr TYPE bsad-gjahr,
    dmbtr TYPE bsad-dmbtr,
    dmbe2 TYPE bsad-dmbe2,
    shkzg TYPE bsad-shkzg,
  END OF ty_bsad,

  BEGIN OF ty_bsik,
    bukrs TYPE bsik-bukrs,
    belnr TYPE bsik-belnr,
    gjahr TYPE bsik-gjahr,
    dmbtr TYPE bsik-dmbtr,
    dmbe2 TYPE bsik-dmbe2,
    shkzg TYPE bsik-shkzg,
  END   OF ty_bsik,

  BEGIN OF ty_bsak,
    bukrs TYPE bsak-bukrs,
    belnr TYPE bsak-belnr,
    gjahr TYPE bsak-gjahr,
    dmbtr TYPE bsak-dmbtr,
    dmbe2 TYPE bsak-dmbe2,
    shkzg TYPE bsak-shkzg,
  END OF ty_bsak,

  BEGIN OF ty_zdoc_exp,
    vbeln            TYPE zdoc_exp-vbeln,
    id_nomeacao_tran TYPE zdoc_exp-id_nomeacao_tran,
    ds_nome_transpor TYPE znom_transporte-ds_nome_transpor,
    ds_porto         TYPE znom_transporte-ds_porto,
    ds_terminal      TYPE znom_transporte-ds_terminal,
  END   OF ty_zdoc_exp,

  BEGIN OF ty_saida,
    bukrs            TYPE j_1bnfdoc-bukrs,
    branch           TYPE j_1bnfdoc-branch, "FILIAL
    uf_filial        TYPE adrc-region,
    cfop             TYPE j_1bnflin-cfop,   "cfop
    nfenum           TYPE j_1bnfdoc-nfenum, "Nr. Nota
    series           TYPE j_1bnfdoc-series, "sERIE
    direct           TYPE j_1bnfdoc-direct,
    meins            TYPE j_1bnflin-meins,  "UNIDADE
    pstdat           TYPE j_1bnfdoc-pstdat, "DATA LANÇAMENTO
    docdat           TYPE j_1bnfdoc-docdat, "DATA DOCUMENTO
    menge            TYPE j_1bnflin-menge,  "Quantidade
    netwrt           TYPE j_1bnflin-netwrt,  "VALOR TOTAL
    base_icms        TYPE j_1bnfstx-base,   "BASE ICMS
    outros           TYPE j_1bnfstx-othbas, "Outros
    icms             TYPE j_1bnfstx-taxval, "Vlr.ICMS
    pis              TYPE j_1bnfstx-taxval, "Vlr.PIS
    cofins           TYPE j_1bnfstx-taxval, "Vlr.COFINS
    iss              TYPE j_1bnfstx-taxval, "Vlr.COFINS
    inss             TYPE j_1bnfstx-taxval, "Vlr.INSS s/ Fat
    docnum           TYPE j_1bnfdoc-docnum, "Nº documento
    stcd3            TYPE lfa1-stcd3,       "Inscricao Estadual
    excbas           TYPE j_1bnfstx-excbas, "ISENTAS
    produto          TYPE makt-maktx,       "Produto
    matkl            TYPE t023t-matkl,
    wgbez60          TYPE t023t-wgbez60,
    ordem            TYPE lips-vgbel,
    instrucao        TYPE zsdt0053-instrucao,
    remessa          TYPE lips-vbeln,
    romaneio         TYPE zsdt0001-nr_romaneio,
    ref_ro           TYPE zsdt0001-ch_referencia,
    utilizacao       TYPE tvlvt-bezei,
    doc_fatura       TYPE vbfa-vbeln,
    kursk            TYPE vbrp-kursk,
    vlr_dolar        TYPE j_1bnflin-netwrt,
    cpf_prod         TYPE lfa1-stcd1,
    unit_dolar       TYPE p DECIMALS 4,
    status           TYPE c LENGTH 20,      "Status
    nf_status        TYPE c LENGTH 10,      "Estornada / Ativa
    vlr_unit         TYPE p DECIMALS 4,
    "nome_clifor      TYPE c LENGTH 35,
    cod_clifor       TYPE lfa1-lifnr,
    nome_clifor      TYPE lfa1-name1,
    uf_clifor        TYPE lfa1-regio,
    crt_bupla        TYPE string, " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana- SMC
    ort01            TYPE lfa1-ort01,
    parc_coleta      TYPE c LENGTH 35,
    cid_coleta       TYPE lfa1-ort01,
    uf_coleta        TYPE lfa1-regio,
    local_ent        TYPE c LENGTH 35,
    uf_ent           TYPE lfa1-regio,
    terminal         TYPE c LENGTH 35,
    uf_terminal      TYPE lfa1-regio,
    nfe              TYPE j_1bnfdoc-nfe,
    data(25)         TYPE c,
    user             TYPE sy-uname,
    doc_contabil     TYPE zdoc_exp-vbeln,
    ordem_doc        TYPE rbco-aufnr,
    data_registro    TYPE bkpf-cpudt,
    hora_registro    TYPE bkpf-cputm,
    nftype           TYPE j_1bnfdoc-nftype,
    model            TYPE j_1bnfdoc-model,
    refkey           TYPE j_1bnflin-refkey,

    " add
    maktx            TYPE j_1bnflin-maktx,

    taxlw1           TYPE j_1bnflin-taxlw1,
    taxlw2           TYPE j_1batl2-taxsit,
    taxlw4           TYPE j_1bnflin-taxlw4,
    taxlw5           TYPE j_1bnflin-taxlw5,
    taxlw4_xml       TYPE j_1bnflin-taxlw4,
    taxlw5_xml       TYPE j_1bnflin-taxlw5,
    iva              TYPE rseg-mwskz,
    charg            TYPE j_1bnflin-charg,
    ncm              TYPE j_1bnflin-nbm,

    parid            TYPE j_1bnfnad-parid,

    chave_nfe        TYPE char44,

    lei_icms(150),
    lei_cofins(150),

    auart            TYPE vbak-auart,
    tp_movimento     TYPE zsdt0001-tp_movimento,
    shtyp            TYPE zsdt0011-shtyp,
    anln1            TYPE zfiwrt0009-anln1,
    ebeln            TYPE ekkn-ebeln,
    auart2           TYPE aufk-auart,
    autyp            TYPE aufk-autyp,
    knttp            TYPE ekpo-knttp,
    ddtext           TYPE dd07t-ddtext,
    stkzn            TYPE lfa1-stkzn,
    ntgew            TYPE j_1bnfdoc-ntgew,
    kostl            TYPE kostl,
    sakto            TYPE saknr,
    desc_conta_ctb   TYPE skat-txt50,
    desc_centro_cust TYPE cskt-ltext,
    inco1            TYPE inco1,
    inco2            TYPE inco2,
**  Begin of "CS2022000515 - FF  28.11.2022
    cod_icms         TYPE j_1batl1t-taxlaw,
    aufnr            TYPE aufk-aufnr,
*    ktext            TYPE zektext,
    anln2            TYPE ekkn-anln2,
    ebelp2           TYPE ekbe-ebelp,
** End of "CS2022000515 - FF  28.11.2022

**  Begin of "CS2022000515 - FF  16.01.2023
    regio            TYPE t001w-regio,
    vornr            TYPE afvc-vornr,
    ltxa1            TYPE afvc-ltxa1,
    recap            TYPE char3,
** End of "CS2022000515 - FF  16.01.2023
    placa_cav        TYPE zplaca,
    itmnum           TYPE j_1bitmnum,
    taxtyp           TYPE j_1bajt-taxtyp,
    ttypetxt         TYPE j_1bajt-ttypetxt,
  END   OF ty_saida,

  BEGIN OF ty_chave,
    chave_nfe TYPE char44,
  END OF ty_chave,

  BEGIN OF ty_parceiro,
    docnum TYPE j_1bnfnad-docnum,
    parid  TYPE j_1bnfnad-parid,
    partyp TYPE j_1bnfnad-partyp,
    parvw  TYPE j_1bnfnad-parvw,
  END OF ty_parceiro,

  BEGIN OF ty_vbap,
    vbeln TYPE vbap-vbeln,
    bezei TYPE tvlvt-bezei,
  END OF ty_vbap.


*&---------------------------------------------------------------------*
*& CONSTANTES
*&---------------------------------------------------------------------*
CONSTANTS: c_pis(3)    TYPE c VALUE 'PIS',
           c_ipi(3)    TYPE c VALUE 'IPI',
           c_icms(4)   TYPE c VALUE 'ICMS',
           c_icm(3)    TYPE c VALUE 'ICM',
           c_cofins(6) TYPE c VALUE 'COFI',
           c_iss(4)    TYPE c VALUE 'ISSS'.

TYPES: BEGIN OF ty_zcarta.
         INCLUDE TYPE zcarta_correcao.
TYPES: END OF ty_zcarta.


TYPES: BEGIN OF ty_zlest0039,
         docnum    TYPE zlest0039-docnum,
         placa_cav TYPE zlest0039-placa_cav,
       END OF ty_zlest0039.


TYPES: BEGIN OF ty_doc.
         INCLUDE STRUCTURE j_1bnfdoc.
TYPES:   nfnum2   TYPE zsdt0001-nfnum,
         docnum_s TYPE j_1bnfdoc-docnum,
         vbeln    TYPE zsdt0001-vbeln,
       END OF ty_doc,

       BEGIN OF ty_ordem_pm,
         aufnr TYPE caufv-aufnr,
         ltxa1 TYPE afvc-ltxa1,
         vornr TYPE afvc-vornr,
       END OF ty_ordem_pm.

TYPES: BEGIN OF ty_lfa1_1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         regio TYPE lfa1-regio,
       END OF ty_lfa1_1.



*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
DATA: t_j_1bnfdoc        TYPE TABLE OF j_1bnfdoc,
      t_j_1bnfdoc_e      TYPE TABLE OF ty_doc,
      w_j_1bnfdoc_e      TYPE ty_doc,
      t_vbfa_doc         TYPE TABLE OF ty_vbfa_doc,
      t_vbfa_ord         TYPE TABLE OF vbfa,
      t_j_1bnfstx        TYPE TABLE OF j_1bnfstx,
      t_j_1baj           TYPE TABLE OF ty_j_1baj,
      t_j_1bajt          TYPE TABLE OF j_1bajt,
      t_j_1bnflin        TYPE TABLE OF j_1bnflin WITH HEADER LINE,
      t_j_1bnflin_aux    TYPE TABLE OF j_1bnflin,
      t_j_1bnflin2       TYPE TABLE OF j_1bnflin WITH HEADER LINE,
      t_vbpa             TYPE TABLE OF vbpa,
      t_makt             TYPE TABLE OF makt,
      t_t023t            TYPE TABLE OF t023t,
      t_mara             TYPE TABLE OF mara,
      t_t001             TYPE TABLE OF ty_t001,
      t_t001w            TYPE TABLE OF ty_t001w,
      t_shtyp            TYPE TABLE OF ty_shtyp,
      t_lfa1             TYPE TABLE OF ty_lfa1_1,
      t_j_1bnfe_active   TYPE TABLE OF ty_j_1bnfe_active,
      t_bkpf             TYPE TABLE OF ty_bkpf,
*      T_BSAD       TYPE TABLE OF TY_BSAD,
*      T_BSID       TYPE TABLE OF TY_BSID,
*
*      T_BSAK       TYPE TABLE OF TY_BSAK,
*      T_BSIK       TYPE TABLE OF TY_BSIK,

      t_vbap             TYPE TABLE OF ty_vbap,
      t_saida            TYPE TABLE OF ty_saida,
      t_remessa          TYPE TABLE OF ty_remessa,
      t_remessa_mat      TYPE TABLE OF ty_remessa,
      t_remessa2         TYPE TABLE OF ty_remessa2,
      t_remessa_3        TYPE TABLE OF ty_remessa_3,
      t_parceiro         TYPE TABLE OF ty_parceiro,
      t_vbrp             TYPE TABLE OF ty_vbrp,
      t_vbrp_aux         TYPE TABLE OF ty_vbrp_aux,
      it_zcarta          TYPE TABLE OF ty_zcarta,
      it_zlest0039       TYPE TABLE OF ty_zlest0039,
      it_j_1bnfnad       TYPE TABLE OF j_1bnfnad,
      it_zsdt0053        TYPE TABLE OF zsdt0053,
      it_zfiwrt0008      TYPE TABLE OF zfiwrt0008,
      it_zfiwrt0009      TYPE TABLE OF zfiwrt0009,
      it_vbak            TYPE TABLE OF vbak,
      it_zsdt0001        TYPE TABLE OF zsdt0001,
      it_zsdt0011        TYPE TABLE OF zsdt0011,
      t_ekbe             TYPE TABLE OF ekbe,
      t_ekkn             TYPE TABLE OF ekkn,
      t_ekpo             TYPE TABLE OF ekpo,
      t_uf_pedido        TYPE TABLE OF t001w,
      t_ordem_pm         TYPE TABLE OF v_npact,
      t_aufk             TYPE TABLE OF aufk,
      t_dd07v            TYPE TABLE OF dd07v,
      t_skat             TYPE TABLE OF skat,
      t_bkpf_lin         TYPE TABLE OF bkpf,
      t_zfiwrt0008       TYPE TABLE OF zfiwrt0008,
      t_zib_contabil_chv TYPE TABLE OF zib_contabil_chv,
      t_cskt             TYPE TABLE OF cskt,
      t_j_1batl1         TYPE TABLE OF j_1batl1,
      t_j_1batl1t        TYPE TABLE OF j_1batl1t,
      t_j_1batl2         TYPE TABLE OF j_1batl2,
      t_j_1batl4a        TYPE TABLE OF j_1batl4a,
      t_j_1batl4t        TYPE TABLE OF j_1batl4t,
      t_j_1batl5         TYPE TABLE OF j_1batl5,
      t_j_1btaxsitcoft   TYPE TABLE OF j_1btaxsitcoft,
      t_chave            TYPE TABLE OF ty_chave,
      t_zib_nfe_dist_itm TYPE TABLE OF zib_nfe_dist_itm,
      t_vbrp_lin         TYPE TABLE OF vbrp,
      t_rseg_lin         TYPE TABLE OF rseg,
      t_bseg             TYPE TABLE OF bseg,
      t_bkpf_c           TYPE TABLE OF bkpf,
      t_rbco             TYPE TABLE OF rbco,
      t_tka02            TYPE TABLE OF tka02.



*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA: wa_saida_zconf_zconf TYPE ty_saida,
      wa_j_1bnfdoc         TYPE j_1bnfdoc,
      wa_vbfa_ord          TYPE vbfa,
      wa_vbfa_doc          TYPE ty_vbfa_doc,
      wa_j_cabe            TYPE j_1bindoc,
      wa_j_1bnfstx         TYPE j_1bnfstx,
      wa_j_1baj            TYPE ty_j_1baj,
      wa_j_1bnflin         TYPE j_1bnflin,
      wa_j_1bnflin_aux     TYPE j_1bnflin,
      wa_j_1btaxsitcoft    TYPE j_1btaxsitcoft,
      wa_vbpa              TYPE vbpa,
      wa_makt              TYPE makt,
      wa_t023t             TYPE t023t,
      wa_t001_zconf_zconf  TYPE ty_t001,
      wa_shtyp             TYPE ty_shtyp,
      wa_j_1bnfe_active    TYPE ty_j_1bnfe_active,
      wa_bkpf              TYPE ty_bkpf,
      wa_bkpf_c            TYPE bkpf,
*      WA_BSID           TYPE TY_BSID,
*      WA_BSAD           TYPE TY_BSAD,
      wa_bseg              TYPE bseg,

*      WA_BSIK           TYPE TY_BSIK,
*      WA_BSAK           TYPE TY_BSAK,
      wa_remessa           TYPE ty_remessa,
      wa_remessa_mat       TYPE ty_remessa,
      wa_remessa2          TYPE ty_remessa2,
      wa_remessa_3         TYPE ty_remessa_3,
      wa_parceiro          TYPE ty_parceiro,
      "WA_SAIDA          TYPE TY_SAIDA,
      wa_vbap              TYPE ty_vbap,
      wa_vbrp              TYPE ty_vbrp,
      wa_vbrp_aux          TYPE ty_vbrp_aux,
      wa_zcarta            TYPE ty_zcarta,
      wa_j_1bnfnad         TYPE j_1bnfnad,
      wa_j_1bdynad         TYPE j_1bdynad,
      wa_zsdt0053          TYPE zsdt0053,
      wa_zsdt0001          TYPE zsdt0001,
      wa_zsdt0011          TYPE zsdt0011,
      wa_zfiwrt0008        TYPE zfiwrt0008,
      wa_zfiwrt0009        TYPE zfiwrt0009,
      wa_vbak              TYPE vbak,
      wa_ekbe              TYPE ekbe,
      wa_ekpo              TYPE ekpo,
      wa_ekkn              TYPE ekkn,
      wa_aufk              TYPE aufk,
      wa_dd07v             TYPE dd07v,
      wa_skat              TYPE skat,
      ws_cskt              TYPE cskt,
      ws_tka02             TYPE tka02,
      wa_chave             TYPE ty_chave,
      wa_zib_nfe_dist_itm  TYPE zib_nfe_dist_itm,
      v_tmiss(1),
      wa_awkey             TYPE bkpf-awkey,
      wa_adress            TYPE sadr,
      wa_gjahr             LIKE ekbe-gjahr,
      w_ekbe               LIKE ekbe,
      wa_zlest0039         TYPE ty_zlest0039.

DATA: s_doc TYPE j_1bnflin-refkey.
*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*
DATA: x_data  TYPE d,
      x_hora  TYPE sy-uzeit,
      e_ov(1) TYPE c.

*&---------------------------------------------------------------------*
*&      Form  ZSELECIONA_DADOS_ZCONF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ATIVAS  text
*      -->P_NATIVA  text
*      -->P_AUTOR  text
*      -->P_REJEIT  text
*      -->P_RECUS  text
*      -->P_CANCEL  text
*      -->P_AGRES  text
*      -->P_NENV  text
*      -->P_gravr  text
*----------------------------------------------------------------------*
FORM zseleciona_dados_zconf  USING    p_ativas
                                      p_nativa
                                      p_autor
                                      p_rejeit
                                      p_recus
                                      p_cancel
                                      p_agres
                                      p_nenv
                                      p_gravr
                                      p_frepro.


  DATA : t_j_1bnfstx_aux TYPE TABLE OF j_1bnfstx,
         t_j_1bnflin_aux TYPE TABLE OF j_1bnflin,
         t_vbpa_aux      TYPE TABLE OF vbpa,
         t_remessa_aux   TYPE TABLE OF ty_remessa,
         vg_tabix        TYPE          sy-tabix.


  LOOP AT it_movimento_mensal_zconf INTO wa_movimento_mensal_zconf.

    IF wa_movimento_mensal_zconf-campo EQ 'bukrs'.
      p_bukrs-low    = wa_movimento_mensal_zconf-valor_de.
      p_bukrs-high   = wa_movimento_mensal_zconf-valor_ate.
      p_bukrs-sign   = wa_movimento_mensal_zconf-sign.
      p_bukrs-option = wa_movimento_mensal_zconf-option.
      APPEND p_bukrs.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'branch'.
      p_branch-low    = wa_movimento_mensal_zconf-valor_de.
      p_branch-high   = wa_movimento_mensal_zconf-valor_ate.
      p_branch-sign   = wa_movimento_mensal_zconf-sign.
      p_branch-option = wa_movimento_mensal_zconf-option.
      APPEND p_branch.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'direct'.
      p_direct-low    = wa_movimento_mensal_zconf-valor_de.
      p_direct-high   = wa_movimento_mensal_zconf-valor_ate.
      p_direct-sign   = wa_movimento_mensal_zconf-sign.
      p_direct-option = wa_movimento_mensal_zconf-option.
      APPEND p_direct.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'pstdat'.
      p_pstdat-low    = wa_movimento_mensal_zconf-valor_de.
      p_pstdat-high   = wa_movimento_mensal_zconf-valor_ate.
      p_pstdat-sign   = wa_movimento_mensal_zconf-sign.
      p_pstdat-option = wa_movimento_mensal_zconf-option.
      APPEND p_pstdat.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'docdat'.
      p_docdat-low    = wa_movimento_mensal_zconf-valor_de.
      p_docdat-high   = wa_movimento_mensal_zconf-valor_ate.
      p_docdat-sign   = wa_movimento_mensal_zconf-sign.
      p_docdat-option = wa_movimento_mensal_zconf-option.
      APPEND p_docdat.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ 'tmiss'.
      p_tmiss-low    = wa_movimento_mensal_zconf-valor_de.
      p_tmiss-high   = wa_movimento_mensal_zconf-valor_ate.
      p_tmiss-sign   = wa_movimento_mensal_zconf-sign.
      p_tmiss-option = wa_movimento_mensal_zconf-option.
      APPEND p_tmiss.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'tmpro'.
      p_tmiss-low    = wa_movimento_mensal_zconf-valor_de.
      p_tmiss-high   = wa_movimento_mensal_zconf-valor_ate.
      p_tmiss-sign   = wa_movimento_mensal_zconf-sign.
      p_tmiss-option = wa_movimento_mensal_zconf-option.
      APPEND p_tmpro.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'tmptod'.
      p_tmiss-low    = wa_movimento_mensal_zconf-valor_de.
      p_tmiss-high   = wa_movimento_mensal_zconf-valor_ate.
      p_tmiss-sign   = wa_movimento_mensal_zconf-sign.
      p_tmiss-option = wa_movimento_mensal_zconf-option.
      APPEND p_tmptod.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'nordem'.
      p_nordem-low    = wa_movimento_mensal_zconf-valor_de.
      p_nordem-high   = wa_movimento_mensal_zconf-valor_ate.
      p_nordem-sign   = wa_movimento_mensal_zconf-sign.
      p_nordem-option = wa_movimento_mensal_zconf-option.
      APPEND  p_nordem.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'parid'.
      p_parid-low    = wa_movimento_mensal_zconf-valor_de.
      p_parid-high   = wa_movimento_mensal_zconf-valor_ate.
      p_parid-sign   = wa_movimento_mensal_zconf-sign.
      p_parid-option = wa_movimento_mensal_zconf-option.
      APPEND p_parid.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'docnum'.
      p_docnum-low    = wa_movimento_mensal_zconf-valor_de.
      p_docnum-high   = wa_movimento_mensal_zconf-valor_ate.
      p_docnum-sign   = wa_movimento_mensal_zconf-sign.
      p_docnum-option = wa_movimento_mensal_zconf-option.
      APPEND p_docnum.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ  'nfenum'.
      p_nfenum-low    = wa_movimento_mensal_zconf-valor_de.
      p_nfenum-high   = wa_movimento_mensal_zconf-valor_ate.
      p_nfenum-sign   = wa_movimento_mensal_zconf-sign.
      p_nfenum-option = wa_movimento_mensal_zconf-option.
      APPEND  p_nfenum.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ  'cfop'.
      p_cfop-low    = wa_movimento_mensal_zconf-valor_de.
      p_cfop-high   = wa_movimento_mensal_zconf-valor_ate.
      p_cfop-sign   = wa_movimento_mensal_zconf-sign.
      p_cfop-option = wa_movimento_mensal_zconf-option.
      APPEND  p_cfop.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ 'model'.
      p_model-low    = wa_movimento_mensal_zconf-valor_de.
      p_model-high   = wa_movimento_mensal_zconf-valor_ate.
      p_model-sign   = wa_movimento_mensal_zconf-sign.
      p_model-option = wa_movimento_mensal_zconf-option.
      APPEND p_model.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ 'crenam'.
      p_crenam-low    = wa_movimento_mensal_zconf-valor_de.
      p_crenam-high   = wa_movimento_mensal_zconf-valor_ate.
      p_crenam-sign   = wa_movimento_mensal_zconf-sign.
      p_crenam-option = wa_movimento_mensal_zconf-option.
      APPEND p_crenam.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'nfe'.
      p_nfe-low    = wa_movimento_mensal_zconf-valor_de.
      p_nfe-high   = wa_movimento_mensal_zconf-valor_ate.
      p_nfe-sign   = wa_movimento_mensal_zconf-sign.
      p_nfe-option = wa_movimento_mensal_zconf-option.
      APPEND p_nfe.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'chave'.
      "Chave de Acesso / 51 24 03 84590892000622 57000000742035110187282 2
      APPEND  VALUE #(
        regio   = wa_movimento_mensal_zconf-valor_de+0(2)
        nfyear  = wa_movimento_mensal_zconf-valor_de+2(2)
        nfmonth = wa_movimento_mensal_zconf-valor_de+4(2)
        stcd1   = wa_movimento_mensal_zconf-valor_de+6(14)
        model   = wa_movimento_mensal_zconf-valor_de+20(2)
        serie   = wa_movimento_mensal_zconf-valor_de+22(3)
        nfnum9  = wa_movimento_mensal_zconf-valor_de+25(9)
        docnum9 = wa_movimento_mensal_zconf-valor_de+34(9)
        cdv     = wa_movimento_mensal_zconf-valor_de+43(1)
       ) TO it_nfe.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ 'nfnum'.
      p_nfnum-low    = wa_movimento_mensal_zconf-valor_de.
      p_nfnum-high   = wa_movimento_mensal_zconf-valor_ate.
      p_nfnum-sign   = wa_movimento_mensal_zconf-sign.
      p_nfnum-option = wa_movimento_mensal_zconf-option.
      APPEND p_nfnum.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'mat'.
      p_mat-low    = wa_movimento_mensal_zconf-valor_de.
      p_mat-high   = wa_movimento_mensal_zconf-valor_ate.
      p_mat-sign   = wa_movimento_mensal_zconf-sign.
      p_mat-option = wa_movimento_mensal_zconf-option.
      APPEND p_mat.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'matkl'.
      p_matkl-low    = wa_movimento_mensal_zconf-valor_de.
      p_matkl-high   = wa_movimento_mensal_zconf-valor_ate.
      p_matkl-sign   = wa_movimento_mensal_zconf-sign.
      p_matkl-option = wa_movimento_mensal_zconf-option.
      APPEND p_matkl.
    ENDIF.

  ENDLOOP.

  IF NOT p_nordem IS INITIAL AND  1 IN p_direct[].
    MESSAGE i000(z01) WITH 'O n° da Ordem de venda pode se informado '
                           'somente para saída' .
    STOP.
  ENDIF.

  IF p_ativas NE 'X' AND  p_nativa NE 'X' AND p_autor  NE 'X' AND p_rejeit NE 'X' AND p_recus  NE 'X' AND
     p_cancel NE 'X' AND  p_agres  NE 'X' AND p_nenv   NE 'X' .

    MESSAGE i000(z01) WITH 'Selecione pelo menos uma opção de Status da NF !' .
    STOP.
  ENDIF.

  SELECT * INTO TABLE t_j_1batl1
    FROM j_1batl1.

  SORT t_j_1batl1 BY taxlaw.

  SELECT * INTO TABLE t_j_1batl1t
    FROM j_1batl1t
    WHERE langu = sy-langu.

  SORT t_j_1batl1t BY taxlaw.

  IF it_nfe IS INITIAL.
    SELECT *
      FROM j_1bnfdoc
      INTO TABLE t_j_1bnfdoc
      WHERE bukrs  IN p_bukrs
        AND branch IN p_branch
        AND direct IN p_direct
        AND docdat IN p_docdat
        AND model  IN p_model
        AND docnum IN p_docnum
        AND crenam IN p_crenam
        AND nfenum IN p_nfenum
        AND parid  IN p_parid
        AND nfe    IN p_nfe
        AND nfnum  IN p_nfnum
        AND pstdat IN p_pstdat.
*      AND DOCTYP NE '5'. "Estorno
  ELSE.
    "j_1bnfe_active
    FREE: t_j_1bnfe_active.
    SELECT docnum docsta cancel scssta regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
   FROM j_1bnfe_active
   INTO TABLE t_j_1bnfe_active
   FOR ALL ENTRIES IN it_nfe
   WHERE    regio   EQ it_nfe-regio
      AND   nfyear  EQ it_nfe-nfyear
      AND   nfmonth EQ it_nfe-nfmonth
      AND   stcd1   EQ it_nfe-stcd1
      AND   model   EQ it_nfe-model
      AND   serie   EQ it_nfe-serie
      AND   nfnum9  EQ it_nfe-nfnum9.
*      AND   docnum9 EQ it_nfe-docnum9
*      AND   cdv     EQ it_nfe-cdv.
    IF t_j_1bnfe_active IS NOT INITIAL.
      SELECT *
    FROM j_1bnfdoc
    INTO TABLE t_j_1bnfdoc
        FOR ALL ENTRIES IN t_j_1bnfe_active
    WHERE docnum EQ t_j_1bnfe_active-docnum
      AND  bukrs  IN p_bukrs
      AND branch IN p_branch
      AND direct IN p_direct
      AND docdat IN p_docdat
      AND model  IN p_model
      AND docnum IN p_docnum
      AND crenam IN p_crenam
      AND nfenum IN p_nfenum
      AND parid  IN p_parid
      AND nfe    IN p_nfe
      AND nfnum  IN p_nfnum
      AND pstdat IN p_pstdat.
    ENDIF.
    FREE: t_j_1bnfe_active.
  ENDIF.

  SORT t_j_1bnfdoc BY doctyp.
  DELETE t_j_1bnfdoc WHERE doctyp EQ '5' . "Estorno

  IF t_j_1bnfdoc IS NOT INITIAL.
    "------Parceiros da Nota--------
    SELECT docnum parid partyp parvw
      FROM j_1bnfnad
      INTO TABLE t_parceiro
      FOR ALL ENTRIES IN t_j_1bnfdoc
        WHERE docnum EQ t_j_1bnfdoc-docnum.

    "------Status nfe ---------
    SELECT docnum docsta cancel scssta regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
      FROM j_1bnfe_active
      INTO TABLE t_j_1bnfe_active
      FOR ALL ENTRIES IN t_j_1bnfdoc
      WHERE docnum EQ t_j_1bnfdoc-docnum.

    IF t_j_1bnfe_active[] IS NOT INITIAL.
      LOOP AT t_j_1bnfe_active INTO wa_j_1bnfe_active.
        IF wa_j_1bnfe_active-cdv IS NOT INITIAL.
          CONCATENATE
          wa_j_1bnfe_active-regio
          wa_j_1bnfe_active-nfyear
          wa_j_1bnfe_active-nfmonth
          wa_j_1bnfe_active-stcd1
          wa_j_1bnfe_active-model
          wa_j_1bnfe_active-serie
          wa_j_1bnfe_active-nfnum9
          wa_j_1bnfe_active-docnum9
          wa_j_1bnfe_active-cdv    INTO wa_chave-chave_nfe.
          APPEND wa_chave TO t_chave.
        ENDIF.
      ENDLOOP.

      IF t_chave[] IS NOT INITIAL.
        SELECT * INTO TABLE t_zib_nfe_dist_itm
          FROM zib_nfe_dist_itm
          FOR ALL ENTRIES IN t_chave
          WHERE chave_nfe = t_chave-chave_nfe.
      ENDIF.
    ENDIF.

    "--------Itens -------------
    CLEAR v_tmiss.
    IMPORT p1 = e_ov FROM MEMORY ID 'MZCONFX'.

    IF NOT ( p_tmiss[] IS INITIAL ). "Serviço "IR050736  - AOENNING.
      "      V_TMISS = 'X'.
      SELECT *
        FROM j_1bnflin
        INTO TABLE t_j_1bnflin
        FOR ALL ENTRIES IN t_j_1bnfdoc
        WHERE docnum EQ t_j_1bnfdoc-docnum
         AND tmiss  EQ 'X'.

      IF p_cfop[] IS NOT INITIAL.
        SORT t_j_1bnflin BY cfop.
        DELETE t_j_1bnflin WHERE cfop NOT IN p_cfop.
      ENDIF.
      IF  p_mat[] IS NOT INITIAL.
        SORT t_j_1bnflin BY matnr.
        DELETE t_j_1bnflin WHERE matnr NOT IN p_mat.
      ENDIF.

      IF  p_matkl[] IS NOT INITIAL.
        SORT t_j_1bnflin BY matkl.
        DELETE t_j_1bnflin WHERE matkl NOT IN p_matkl.
      ENDIF.

      SORT t_j_1bnflin BY tmiss.
      DELETE t_j_1bnflin WHERE tmiss IS INITIAL.

    ELSEIF NOT ( p_tmpro[] IS INITIAL ). "Produto
      SELECT *
        FROM j_1bnflin
        INTO TABLE t_j_1bnflin
        FOR ALL ENTRIES IN t_j_1bnfdoc
        WHERE docnum EQ t_j_1bnfdoc-docnum
          AND cfop   IN p_cfop
          AND matnr  IN p_mat
          AND tmiss  NE 'X'.

      IF p_cfop[] IS NOT INITIAL.
        SORT t_j_1bnflin BY cfop.
        DELETE t_j_1bnflin WHERE cfop NOT IN p_cfop.
      ENDIF.
      IF  p_mat[] IS NOT INITIAL.
        SORT t_j_1bnflin BY matnr.
        DELETE t_j_1bnflin WHERE matnr NOT IN p_mat.
      ENDIF.

      IF  p_matkl[] IS NOT INITIAL.
        SORT t_j_1bnflin BY matkl.
        DELETE t_j_1bnflin WHERE matkl NOT IN p_matkl.
      ENDIF.

      SORT t_j_1bnflin BY tmiss.
      DELETE t_j_1bnflin WHERE tmiss IS NOT INITIAL.

    ELSE.

      SELECT *
        FROM j_1bnflin
        INTO TABLE t_j_1bnflin
        FOR ALL ENTRIES IN t_j_1bnfdoc
        WHERE docnum EQ t_j_1bnfdoc-docnum.

      IF p_cfop[] IS NOT INITIAL.
        SORT t_j_1bnflin BY cfop.
        DELETE t_j_1bnflin WHERE cfop NOT IN p_cfop.
      ENDIF.
      IF  p_mat[] IS NOT INITIAL.
        SORT t_j_1bnflin BY matnr.
        DELETE t_j_1bnflin WHERE matnr NOT IN p_mat.
      ENDIF.
      IF  p_matkl[] IS NOT INITIAL.
        SORT t_j_1bnflin BY matkl.
        DELETE t_j_1bnflin WHERE matkl NOT IN p_matkl.
      ENDIF.

    ENDIF.

    IF t_j_1bnflin[] IS NOT INITIAL.

      SELECT * INTO TABLE t_vbrp_lin
        FROM vbrp
      FOR ALL ENTRIES IN t_j_1bnflin
       WHERE vbeln = t_j_1bnflin-refkey(10).

      SELECT * INTO TABLE t_rseg_lin
        FROM rseg
      FOR ALL ENTRIES IN t_j_1bnflin
         WHERE belnr = t_j_1bnflin-refkey+0(10)
*           AND gjahr = t_j_1bnflin-refkey+10(4
           AND buzei = t_j_1bnflin-refitm.

      SELECT * INTO TABLE t_j_1batl2
        FROM j_1batl2
        FOR ALL ENTRIES IN t_j_1bnflin
        WHERE taxlaw EQ t_j_1bnflin-taxlw2.

      SELECT * FROM j_1batl4a INTO TABLE t_j_1batl4a
       FOR ALL ENTRIES IN t_j_1bnflin
        WHERE taxlaw    EQ t_j_1bnflin-taxlw4.

      SELECT * INTO TABLE t_j_1batl4t
        FROM j_1batl4t
        WHERE langu = sy-langu.

      SELECT * FROM j_1batl5 INTO TABLE t_j_1batl5
       FOR ALL ENTRIES IN t_j_1bnflin
        WHERE taxlaw    EQ t_j_1bnflin-taxlw5.

      SELECT *
        FROM j_1btaxsitcoft INTO TABLE t_j_1btaxsitcoft
       WHERE langu  = sy-langu.

      SELECT * INTO TABLE t_zfiwrt0008
        FROM zfiwrt0008
        FOR ALL ENTRIES IN t_j_1bnflin
        WHERE seq_lcto = t_j_1bnflin-refkey(10).
      IF t_zfiwrt0008[] IS NOT INITIAL.
        SELECT * INTO TABLE t_zib_contabil_chv
          FROM zib_contabil_chv
          FOR ALL ENTRIES IN t_zfiwrt0008
          WHERE obj_key = t_zfiwrt0008-obj_key.
      ENDIF.

      SORT t_zib_contabil_chv BY obj_key.
      SORT t_zfiwrt0008 BY seq_lcto.

      t_j_1bnflin_aux[] = t_j_1bnflin[].
      SORT t_j_1bnflin_aux BY refkey.
      DELETE t_j_1bnflin_aux WHERE refkey IS INITIAL.

      IF t_j_1bnflin_aux[] IS NOT INITIAL.
        SELECT * INTO TABLE t_bkpf_lin
          FROM bkpf
          FOR ALL ENTRIES IN t_j_1bnflin_aux
          WHERE awkey = t_j_1bnflin_aux-refkey(20).

        SORT t_bkpf_lin BY awkey blart.
      ENDIF.

      IF e_ov = 'X'.
        LOOP AT t_j_1bnfdoc INTO DATA(w_doc).
          IF w_doc-direct = '1'.
            MOVE-CORRESPONDING w_doc TO w_j_1bnfdoc_e.
            APPEND w_j_1bnfdoc_e TO t_j_1bnfdoc_e.
          ENDIF.
        ENDLOOP.
        PERFORM f_busca_vinculo_entrada.
      ENDIF.

      IF p_nordem[] IS NOT INITIAL.

        SELECT *
          FROM vbfa
          INTO TABLE t_vbfa_ord
          FOR ALL ENTRIES IN t_j_1bnflin
          WHERE vbeln EQ  t_j_1bnflin-refkey(10).

        SORT t_vbfa_ord BY vbtyp_n .
        DELETE t_vbfa_ord WHERE vbtyp_n EQ 'N'.

        SORT t_vbfa_ord BY vbtyp_v .
        DELETE t_vbfa_ord WHERE vbtyp_v EQ 'M'.

        DELETE t_vbfa_ord WHERE vbelv NOT IN p_nordem.

        IF  t_vbfa_ord IS NOT INITIAL.

          SORT t_j_1bnflin BY refkey.

          LOOP AT t_vbfa_ord INTO wa_vbfa_ord.

            READ TABLE t_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey  = wa_vbfa_ord-vbeln BINARY SEARCH.
            IF sy-subrc IS  INITIAL.
              wa_vbfa_doc =  wa_j_1bnflin-docnum.
              APPEND wa_vbfa_doc TO t_vbfa_doc.
            ENDIF.

          ENDLOOP.

          IF t_vbfa_doc IS NOT INITIAL.

            SORT t_vbfa_doc BY docnum.

            LOOP AT t_j_1bnfdoc INTO wa_j_1bnfdoc.
              vg_tabix = sy-tabix.
              READ TABLE t_vbfa_doc INTO wa_vbfa_doc WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
              IF sy-subrc IS NOT INITIAL.
                CLEAR: wa_j_1bnfdoc-docnum.
                MODIFY t_j_1bnfdoc INDEX vg_tabix FROM wa_j_1bnfdoc TRANSPORTING docnum.
              ENDIF.
            ENDLOOP.

            DELETE t_j_1bnfdoc WHERE docnum EQ space.

          ENDIF.
          "ELSE.
          "MESSAGE i000(z01) WITH 'O n° da Ordem de venda não encontrado '.
          "EXIT.
        ENDIF.

        CLEAR: wa_j_1bnfdoc,
               wa_vbfa_ord,
               wa_vbfa_doc,
               wa_j_1bnflin.



      ENDIF.


      "------Imposto-------------
      SELECT *
        FROM j_1bnfstx
        INTO TABLE t_j_1bnfstx
        FOR ALL ENTRIES IN t_j_1bnflin
        WHERE docnum EQ t_j_1bnflin-docnum
          AND itmnum EQ t_j_1bnflin-itmnum.

      t_j_1bnfstx_aux[] = t_j_1bnfstx[].

      SORT t_j_1bnfstx_aux BY taxtyp.

      DELETE ADJACENT DUPLICATES FROM t_j_1bnfstx_aux COMPARING taxtyp.

      "-----Tipo de Imposto-------
      IF t_j_1bnfstx_aux IS NOT INITIAL.

        SELECT taxtyp taxgrp
          FROM j_1baj
          INTO TABLE t_j_1baj
          FOR ALL ENTRIES IN t_j_1bnfstx_aux
           WHERE taxtyp EQ t_j_1bnfstx_aux-taxtyp.
        IF sy-subrc EQ 0.
          SELECT *
            FROM j_1bajt
            INTO CORRESPONDING FIELDS OF TABLE t_j_1bajt
            FOR ALL ENTRIES IN t_j_1baj
             WHERE taxtyp EQ t_j_1baj-taxtyp
             AND   spras  EQ sy-langu.
        ENDIF.

      ENDIF.

      t_j_1bnflin_aux[] = t_j_1bnflin[].

      SORT t_j_1bnflin_aux BY matnr.

      DELETE ADJACENT DUPLICATES FROM t_j_1bnflin_aux COMPARING matnr.

      "-------Produto------------
      SELECT *
        FROM makt
        INTO TABLE t_makt
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE matnr EQ t_j_1bnflin_aux-matnr.

      "-------Dados Gerais de material------------
      SELECT *
        FROM mara
        INTO TABLE t_mara
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE matnr EQ t_j_1bnflin_aux-matnr.

      SORT t_mara BY matnr.
      DELETE ADJACENT DUPLICATES FROM t_mara COMPARING matnr.

      IF t_mara[] IS NOT INITIAL.

        SELECT *
         FROM t023t
         INTO TABLE t_t023t
         FOR ALL ENTRIES IN t_mara
         WHERE matkl EQ t_mara-matkl
           AND spras = sy-langu.

      ENDIF.

      t_j_1bnflin_aux[] = t_j_1bnflin[].

      SORT t_j_1bnflin_aux BY refkey.

      DELETE ADJACENT DUPLICATES FROM t_j_1bnflin_aux COMPARING refkey.

      "-----Documento de faturamento-----
      SELECT vbeln kursk
        FROM vbrp
        INTO TABLE t_vbrp
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE vbeln EQ  t_j_1bnflin_aux-refkey(10).

      "------Parceiro da Fatura
      SELECT *
        FROM vbpa
        INTO TABLE t_vbpa
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE vbeln EQ t_j_1bnflin_aux-refkey(10).

      SORT t_vbpa BY parvw.
      DELETE t_vbpa WHERE parvw NE 'Z1'.

      t_vbpa_aux[] = t_vbpa[].

      SORT t_vbpa_aux BY lifnr.

      DELETE ADJACENT DUPLICATES FROM t_vbpa_aux COMPARING lifnr.

      IF t_vbpa_aux IS NOT INITIAL.
        SELECT lifnr, name1, regio
          FROM lfa1
          INTO TABLE @t_lfa1
          FOR ALL ENTRIES IN @t_vbpa_aux
          WHERE lifnr EQ @t_vbpa_aux-lifnr.
      ENDIF.

      "-------Remessa-----------
      "RMI - CS1079057- IR132823 - INICIO
      " adicionado a consulta por centro para não trazer registros errados
      SELECT v~vbeln re~ch_referencia l~vbeln re~nr_romaneio l~vgbel v~mjahr
        FROM vbfa AS v
        INNER JOIN lips AS l ON l~vbeln = v~vbelv
        LEFT JOIN zsdt0001 AS re ON re~doc_rem = l~vbeln AND tp_movimento EQ 'S'
        INTO TABLE t_remessa
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE v~vbeln   EQ t_j_1bnflin_aux-refkey(10)
          AND v~vbtyp_v EQ 'J'
          AND l~werks   EQ t_j_1bnflin_aux-bwkey.
      "RMI - CS1079057- IR132823 - FIM

      SELECT * FROM zsdt0053 INTO TABLE it_zsdt0053
        FOR ALL ENTRIES IN t_remessa
           WHERE vbeln EQ t_remessa-vgbel.

*      SELECT V~VBELN V~VBELV
*         FROM VBFA AS V
*         INTO TABLE T_REMESSA2
*         FOR ALL ENTRIES IN T_J_1BNFLIN_AUX
*         WHERE V~VBELN   EQ T_J_1BNFLIN_AUX-REFKEY(10)
*           AND V~VBTYP_V EQ 'C'
*           AND V~VBTYP_N IN ('M','O').
*
      IF ( t_remessa[] IS NOT INITIAL ).
*        LOOP AT T_REMESSA2 INTO WA_REMESSA2.
*          CLEAR WA_REMESSA.
*
*          WA_REMESSA-REFKEY = WA_REMESSA2-REFKEY.
*          WA_REMESSA-VGBEL  = WA_REMESSA2-VGBEL.
*
*          APPEND WA_REMESSA TO T_REMESSA.
*        ENDLOOP.

        "-------Remessa-----------
        t_remessa_aux[] = t_remessa[].

        SORT t_remessa_aux BY vgbel.

        DELETE ADJACENT DUPLICATES FROM t_remessa_aux COMPARING vgbel.

        "--------Utilização-----
        SELECT vbeln bezei
          FROM vbap AS v
          INNER JOIN tvlvt AS t ON t~abrvw  EQ v~vkaus
          INTO TABLE t_vbap
          FOR ALL ENTRIES IN t_remessa_aux
          WHERE vbeln EQ t_remessa_aux-vgbel.

*        delete t_vbak where: vkorg      ne p_vkorg,
*                       j_1bbranch not in p_werks,
*                       auart      not in p_auart,
*                       vbeln      not in p_vbeln,
*                       kunnr      not in p_parid.

        t_remessa_aux[] = t_remessa[].

        SORT t_remessa_aux BY refkey.
        DELETE t_remessa_aux WHERE: refkey EQ ''.


        LOOP AT t_remessa_aux INTO wa_remessa.

          CONCATENATE wa_remessa-refkey '          ' INTO wa_remessa_3-refkey .
*          call function 'CONVERSION_EXIT_INVDT_INPUT'
*          exporting
*            input  = WA_REMESSA-refkey
*          importing
*            output = WA_REMESSA_3-refkey .

          wa_remessa_3-vbeln  = wa_remessa-refkey.

          READ TABLE t_j_1bnflin INTO DATA(ws_j_lbnflin) WITH KEY bwkey = wa_remessa-refkey.
          IF sy-subrc NE 0.
            s_doc = |{ wa_remessa-refkey }{ wa_remessa-mjahr }|.
            READ TABLE t_j_1bnflin INTO ws_j_lbnflin WITH KEY refkey = s_doc.
            IF ws_j_lbnflin-reftyp EQ 'MD'.
              wa_remessa_3-refkey = CONV #( s_doc ). "AJUSTE NA ZCONF IR077930 - AOENNING.
            ENDIF.
          ENDIF.


          APPEND wa_remessa_3 TO t_remessa_3.

        ENDLOOP.
        CLEAR: s_doc.

        DELETE ADJACENT DUPLICATES FROM t_remessa_aux COMPARING refkey.

        IF t_remessa_3[] IS NOT INITIAL.
          SELECT bukrs gjahr awkey belnr
            FROM bkpf
            INTO TABLE t_bkpf
            FOR ALL ENTRIES IN t_remessa_3
            WHERE awkey = t_remessa_3-refkey .
        ENDIF.


      ELSE.

        LOOP AT t_vbrp INTO wa_vbrp.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_vbrp-vbeln
            IMPORTING
              output = wa_vbrp_aux-awkey.


          APPEND wa_vbrp_aux TO t_vbrp_aux.

        ENDLOOP.

        "CSB - Não achou remessa para frete rodoviário
        IF t_vbrp_aux[] IS NOT INITIAL.
          SELECT bukrs gjahr awkey belnr
              FROM bkpf
              INTO TABLE t_bkpf
              FOR ALL ENTRIES IN t_vbrp_aux
              WHERE awkey = t_vbrp_aux-awkey.
        ENDIF.

      ENDIF.

      "-------Info de Ordem CS201-----------

*--->  IR132814 / CS1078990
      LOOP AT t_j_1bnflin INTO t_j_1bnflin.





        wa_gjahr = t_j_1bnflin-refkey+10(4).

        SELECT *
          FROM ekbe INTO w_ekbe
            WHERE belnr EQ t_j_1bnflin-refkey(10)
            AND buzei EQ t_j_1bnflin-refitm+2(4)
            AND gjahr EQ wa_gjahr.
          APPEND w_ekbe TO t_ekbe.

        ENDSELECT.
      ENDLOOP.
* SELECT *
*      FROM ekbe
*      INTO TABLE t_ekbe
*      FOR ALL ENTRIES IN t_j_1bnflin
*      WHERE belnr EQ t_j_1bnflin-refkey(10)
*      AND buzei EQ t_j_1bnflin-refitm+2(4).
*<---  IR132814 / CS1078990

      IF t_ekbe[] IS NOT INITIAL.

        SELECT *
          FROM ekpo
          INTO TABLE t_ekpo
          FOR ALL ENTRIES IN t_ekbe
          WHERE ebeln EQ t_ekbe-ebeln
            AND ebelp EQ t_ekbe-ebelp.

**  Begin of "CS2022000515 - FF  16.01.2023
        SELECT *
          FROM t001w
          INTO TABLE t_uf_pedido
          FOR ALL ENTRIES IN t_ekpo
          WHERE werks = t_ekpo-werks.
        IF sy-subrc <> 0.
          CLEAR t_uf_pedido.
        ENDIF.
** End of "CS2022000515 - FF  16.01.2023

        SELECT *
          FROM ekkn
          INTO TABLE t_ekkn
          FOR ALL ENTRIES IN t_ekbe
          WHERE ebeln EQ t_ekbe-ebeln
            AND ebelp EQ t_ekbe-ebelp.

        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'AUFTYP'
            text           = 'X'
            langu          = sy-langu
          TABLES
            dd07v_tab      = t_dd07v
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.

        IF t_ekkn[] IS NOT INITIAL.

          SELECT *
            FROM skat
            INTO TABLE t_skat
            FOR ALL ENTRIES IN t_ekkn
            WHERE saknr EQ t_ekkn-sakto
                  AND ktopl EQ '0050'
                  AND spras EQ 'P'.

          SELECT *
            FROM tka02
            INTO TABLE t_tka02
            WHERE bukrs IN p_bukrs.

          SELECT *
            FROM cskt
            INTO TABLE t_cskt
            FOR ALL ENTRIES IN  t_tka02
            WHERE kokrs EQ t_tka02-kokrs.

          SELECT *
            FROM aufk
            INTO TABLE t_aufk
            FOR ALL ENTRIES IN t_ekkn
            WHERE aufnr EQ t_ekkn-aufnr.


**  Begin of CS2022000515 #97985 FF   16.01.2023
          IF t_ekkn[] IS NOT INITIAL.
            SELECT *
              FROM v_npact INTO TABLE t_ordem_pm
              FOR ALL ENTRIES IN t_ekkn
              WHERE aufnr EQ t_ekkn-aufnr.

            IF sy-subrc <> 0.
              CLEAR t_ordem_pm[].
            ENDIF.
          ENDIF.
** End of FF  16.01.2023


        ENDIF.

      ENDIF.

    ENDIF.

    FREE MEMORY ID 'MZCONFX'.


*    IF T_BKPF IS NOT INITIAL.
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 SHKZG
*        FROM BSID
*        INTO TABLE T_BSID
*        FOR ALL ENTRIES IN T_BKPF
*        WHERE BUKRS EQ T_BKPF-BUKRS
*        AND   BELNR EQ T_BKPF-BELNR
*        AND   GJAHR EQ T_BKPF-GJAHR.
*
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 SHKZG
*        FROM BSAD
*        INTO TABLE T_BSAD
*        FOR ALL ENTRIES IN T_BKPF
*        WHERE BUKRS EQ T_BKPF-BUKRS
*        AND   BELNR EQ T_BKPF-BELNR
*        AND   GJAHR EQ T_BKPF-GJAHR.
*
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 SHKZG
*      FROM BSIK
*      INTO TABLE T_BSIK
*      FOR ALL ENTRIES IN T_BKPF
*      WHERE BUKRS EQ T_BKPF-BUKRS
*      AND   BELNR EQ T_BKPF-BELNR
*      AND   GJAHR EQ T_BKPF-GJAHR.
*
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 SHKZG
*        FROM BSAK
*        INTO TABLE T_BSAK
*        FOR ALL ENTRIES IN T_BKPF
*        WHERE BUKRS EQ T_BKPF-BUKRS
*        AND   BELNR EQ T_BKPF-BELNR
*        AND   GJAHR EQ T_BKPF-GJAHR.
*    ENDIF.

    CLEAR: it_zfiwrt0008[].

    DATA(t_j_1bnflin_zw) = t_j_1bnflin[].
    DELETE t_j_1bnflin_zw WHERE reftyp NE 'ZW'.
    SORT t_j_1bnflin_zw BY docnum.
    DELETE ADJACENT DUPLICATES FROM t_j_1bnflin_zw COMPARING docnum.

    IF t_j_1bnflin_zw[] IS NOT INITIAL.
      SELECT *
        FROM zfiwrt0008 INTO TABLE it_zfiwrt0008
         FOR ALL ENTRIES IN t_j_1bnflin_zw
       WHERE docnum =  t_j_1bnflin_zw-docnum.
      SORT it_zfiwrt0008 BY docnum.
    ENDIF.

    IF it_zfiwrt0008[] IS NOT INITIAL.

      SELECT *
        FROM zfiwrt0009 INTO TABLE it_zfiwrt0009
         FOR ALL ENTRIES IN it_zfiwrt0008
       WHERE seq_lcto =  it_zfiwrt0008-seq_lcto.

      SORT it_zfiwrt0009 BY seq_lcto itmnum.

    ENDIF.

    REFRESH it_zcarta.

    SELECT *
      FROM zcarta_correcao
      INTO CORRESPONDING FIELDS OF TABLE it_zcarta
      FOR ALL ENTRIES IN t_j_1bnfdoc
      WHERE docnum        =  t_j_1bnfdoc-docnum.

    SORT it_zcarta BY novo_terminal.
    DELETE it_zcarta WHERE novo_terminal IS INITIAL.

    " Início - US 128292 - Inserção da placa do veículo - RSA
    SELECT docnum placa_cav
           FROM zlest0039
           INTO TABLE it_zlest0039
           FOR ALL ENTRIES IN t_j_1bnfdoc
           WHERE docnum = t_j_1bnfdoc-docnum.

    SORT it_zlest0039 BY docnum.
    " Fim - US 128292 - Inserção da placa do veículo - RSA

    IF it_zcarta[] IS NOT INITIAL.
      SORT it_zcarta BY docnum  ASCENDING id_cc DESCENDING.
      SELECT lifnr, name1, regio
        FROM lfa1
        APPENDING TABLE @t_lfa1
        FOR ALL ENTRIES IN @it_zcarta
        WHERE  lifnr EQ @it_zcarta-novo_terminal.

      SORT t_lfa1 BY lifnr ASCENDING.

    ENDIF.

**------------------------------------- Tipo de transporte-----------******
*    "---remessa do documento de material
    DELETE t_j_1bnflin_aux WHERE reftyp NE 'BI' AND reftyp NE 'MD'.
    LOOP AT t_j_1bnflin_aux INTO wa_j_1bnflin_aux.
      wa_j_1bnflin_aux-cfop = wa_j_1bnflin_aux-refkey(10).
      MODIFY t_j_1bnflin_aux FROM wa_j_1bnflin_aux INDEX sy-tabix TRANSPORTING cfop .
    ENDLOOP.

    IF t_j_1bnflin_aux[] IS NOT INITIAL.
      SELECT vbrp~vbeln vttk~shtyp
         FROM vbrp
         INNER JOIN vbak
         ON vbak~vbeln = vbrp~vgbel
         INNER JOIN vttk
         ON  vttk~tknum = vbak~tknum
         AND vttk~vsart = '01'
         INTO  TABLE t_shtyp
         FOR ALL ENTRIES IN t_j_1bnflin_aux
         WHERE vbrp~vbeln = t_j_1bnflin_aux-cfop.
      "CS2021000612 Ajuste ZCONF
      SELECT vbrp~vbeln vttk~shtyp
         FROM vbrp
         INNER JOIN vbfa
         ON  vbfa~vbelv = vbrp~vgbel
         AND vbfa~vbtyp_n = '8'
         AND vbfa~vbtyp_v = 'J'
         INNER JOIN vttk
         ON  vttk~tknum = vbfa~vbeln
         AND vttk~vsart = '01'
         APPENDING   TABLE t_shtyp
         FOR ALL ENTRIES IN t_j_1bnflin_aux
         WHERE vbrp~vbeln = t_j_1bnflin_aux-cfop.

      SELECT mkpf~mblnr vttk~shtyp
         FROM mkpf
         INNER JOIN vbfa
         ON  vbfa~vbeln = mkpf~mblnr
         AND vbfa~vbtyp_n = 'R'
         AND vbfa~vbtyp_v = 'J'
         INNER JOIN vbfa AS vbfa2
         ON  vbfa2~vbelv = vbfa~vbelv
         AND vbfa2~vbtyp_n = '8'
         AND vbfa2~vbtyp_v = 'J'
         INNER JOIN vttk
         ON  vttk~tknum = vbfa2~vbeln
         AND vttk~vsart = '01'
         APPENDING TABLE t_shtyp
         FOR ALL ENTRIES IN t_j_1bnflin_aux
         WHERE mkpf~mblnr = t_j_1bnflin_aux-cfop.
      "CS2021000612 Ajuste ZCONF
    ENDIF.

    IF t_bkpf_lin[] IS NOT INITIAL.

      DATA etl1433c6r9282 TYPE TABLE OF bseg.
      CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        EXPORTING
          it_for_all_entries = t_bkpf_lin
          i_where_clause     = |BELNR = IT_FOR_ALL_ENTRIES-BELNR|
        IMPORTING
          et_bseg            = etl1433c6r9282
        EXCEPTIONS
          not_found          = 1.
      IF sy-subrc = 0 AND lines( etl1433c6r9282 ) > 0.
        APPEND LINES OF etl1433c6r9282 TO t_bseg.
        sy-dbcnt = lines( etl1433c6r9282 ).
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.

    ENDIF.
    IF t_zib_contabil_chv[] IS NOT INITIAL.

      DATA etl1439c6r454 TYPE TABLE OF bseg.
      CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        EXPORTING
          it_for_all_entries = t_zib_contabil_chv
          i_where_clause     = |BELNR = IT_FOR_ALL_ENTRIES-BELNR|
        IMPORTING
          et_bseg            = etl1439c6r454
        EXCEPTIONS
          not_found          = 1.
      IF sy-subrc = 0 AND lines( etl1439c6r454 ) > 0.
        APPEND LINES OF etl1439c6r454 TO t_bseg.
        sy-dbcnt = lines( etl1439c6r454 ).
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.

    ENDIF.
    IF t_bkpf[] IS NOT INITIAL.

      DATA etl1445c6r3835 TYPE TABLE OF bseg.
      CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        EXPORTING
          it_for_all_entries = t_bkpf
          i_where_clause     = |BELNR = IT_FOR_ALL_ENTRIES-BELNR|
        IMPORTING
          et_bseg            = etl1445c6r3835
        EXCEPTIONS
          not_found          = 1.
      IF sy-subrc = 0 AND lines( etl1445c6r3835 ) > 0.
        APPEND LINES OF etl1445c6r3835 TO t_bseg.
        sy-dbcnt = lines( etl1445c6r3835 ).
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.

    ENDIF.

    IF t_bseg[] IS NOT INITIAL.
      SELECT * INTO TABLE t_bkpf_c
        FROM bkpf
        FOR ALL ENTRIES IN t_bseg
        WHERE bukrs = t_bseg-bukrs
          AND belnr = t_bseg-belnr.

      SELECT * INTO TABLE t_rbco
        FROM rbco
        FOR ALL ENTRIES IN t_bseg
        WHERE bukrs = t_bseg-bukrs
          AND belnr = t_bseg-belnr.

    ENDIF.
*
*    SELECT MKPF~MBLNR MKPF~MJAHR VTTK~SHTYP
*      FROM MKPF
*      INNER JOIN VTTP
*      ON VTTP~VBELN = MKPF~XBLNR
*      INNER JOIN VTTK
*      ON  VTTK~TKNUM = VTTP~TKNUM
*      AND VTTK~VSART = '01'
*      INTO TABLE T_SHTYPM
*      FOR ALL ENTRIES IN T_J_1BNFLIN_AUX
*      WHERE MKPF~MBLNR EQ T_J_1BNFLIN_AUX-CFOP.
**      AND   MKPF~MJAHR EQ T_J_1BNFLIN_AUX-WERKS.
*
*
*    IF T_REMESSA[] IS NOT INITIAL.
*
*      SELECT VTTP~VBELN VTTK~SHTYP
*        FROM VTTP
*        INNER JOIN VTTK
*        ON  VTTK~TKNUM = VTTP~TKNUM
*        AND VTTK~VSART = '01'
*        INTO  TABLE T_SHTYP
*        FOR ALL ENTRIES IN T_REMESSA
*        WHERE VTTP~VBELN = T_REMESSA-VBELN.
*
*    ENDIF.

**-------------------------------------- Tipo de transporte-----------******



  ELSE.

    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                       'informados' .
    STOP.


  ENDIF.



  PERFORM f_organiza_dados USING p_ativas
                                 p_nativa
                                 p_autor
                                 p_rejeit
                                 p_recus
                                 p_cancel
                                 p_agres
                                 p_nenv
                                 p_gravr
                                 p_frepro.

  PERFORM f_gravar_dados USING   p_gravr.




ENDFORM.                    " ZSELECIONA_DADOS_ZCONF
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_organiza_dados USING           p_ativas
                                      p_nativa
                                      p_autor
                                      p_rejeit
                                      p_recus
                                      p_cancel
                                      p_agres
                                      p_nenv
                                      p_gravr
                                      p_frepro.

  DATA: i                   TYPE c LENGTH 1,
        wa_kna1             TYPE kna1,
        wa_lfa1             TYPE lfa1,
        wa_t001w            TYPE t001w,
        wa_vbpavb           TYPE vbpavb,
        wa_bkpf_aux         TYPE bkpf,
        wa_zib_contabil_chv TYPE zib_contabil_chv,
        wa_zfiwrt0008       TYPE zfiwrt0008,
        wa_j_1batl1         TYPE j_1batl1,
        wa_j_1batl1t        TYPE j_1batl1t,
        wa_j_1batl2         TYPE j_1batl2,
        wa_j_1batl4a        TYPE j_1batl4a,
        wa_j_1batl4t        TYPE j_1batl4t,
        wa_j_1batl5         TYPE j_1batl5,
        wa_saida            TYPE ty_saida,

        v_docnum            TYPE j_1bnfdoc-docnum,
        quantidade          TYPE j_1bnflin-menge,
        utilizacao          TYPE tvlvt-bezei,
        cont                TYPE n LENGTH 4,
        v_bukrs             TYPE j_1bnfdoc-bukrs,
        v_parid             TYPE j_1bnfdoc-parid,
        v_len               TYPE i,
        v_len2              TYPE i,
        v_belnr             TYPE rseg-belnr,
        v_gjahr             TYPE rseg-gjahr,
        v_buzei             TYPE rseg-buzei,
        p_nordem_c          TYPE c LENGTH 10,
        v_netwrt            TYPE j_1bnflin-netwrt,
        v_refkey            TYPE bkpf-awkey.

  SORT: t_j_1bnfdoc      BY belnr pstdat bukrs,
        t_j_1bnflin      BY docnum,
        t_lfa1           BY lifnr,
        t_t001           BY bukrs,
        t_remessa        BY refkey,
        t_remessa2       BY refkey,
        t_j_1baj         BY taxtyp,
        t_parceiro       BY docnum,
        t_j_1bnfe_active BY docnum,
        t_makt           BY matnr,
        t_vbpa           BY vbeln,
        t_j_1bnfstx      BY docnum itmnum,
        t_bkpf           BY awkey ,
        t_remessa_3      BY vbeln ,
        it_vbak          BY vbeln,
        t_shtyp          BY vbeln,
        it_zsdt0001      BY ch_referencia,
        it_zsdt0011      BY auart tp_movimento,
        t_j_1bnfdoc_e    BY docnum.

  DATA: wa_vbrp_aux TYPE vbrp,
        wa_rseg_aux TYPE rseg,
        var_vbeln   TYPE zsdt0001-vbeln.

  DATA: tmp_tax   LIKE j_1bnfstx OCCURS 10 WITH HEADER LINE.
  DATA: wa_lin_e TYPE j_1bnflin,
        wa_lin_i TYPE j_1binlin.

  DATA: wl_rbco TYPE rbco.

  DATA: lo_cte_switch  TYPE REF TO cl_j_1bcte_swf,
        lx_cte_related TYPE abap_bool VALUE abap_false.

  DATA: cl_util TYPE REF TO zcl_util.

  SORT t_j_1bnflin BY docnum.
  SORT t_j_1bnfstx BY docnum itmnum.
  SORT t_j_1baj BY taxtyp.
  SORT t_j_1bnfe_active BY docnum.
  SORT t_parceiro BY docnum.
  SORT t_lfa1 BY lifnr.
  SORT t_vbpa BY vbeln.
  SORT t_remessa BY refkey.
  SORT t_bkpf BY awkey.
  SORT t_j_1bnflin2 BY docnum .
  SORT t_vbap BY vbeln.
  SORT t_vbrp BY vbeln.
  SORT it_zsdt0053 BY vbeln.
  SORT t_makt BY matnr.
  SORT t_mara BY matnr.
  SORT t_t023t BY matkl.
  SORT t_ekbe BY belnr buzei.
  SORT t_ekpo BY ebeln ebelp.
  SORT t_ekkn BY ebeln ebelp.
  SORT t_aufk BY aufnr.
  SORT t_dd07v BY domvalue_l.
  SORT t_skat BY saknr.
  SORT t_cskt BY kostl .
  SORT t_j_1batl2 BY taxlaw.
  SORT t_j_1batl4a BY taxlaw taxsitout.
  SORT t_j_1btaxsitcoft BY taxsit.
  SORT t_j_1batl4t BY taxlaw.
  SORT t_j_1batl5 BY taxlaw taxsitout.
  SORT t_zib_nfe_dist_itm BY chave_nfe prod_ncm.
  SORT t_vbrp_lin BY vbeln.
  SORT t_rseg_lin BY belnr gjahr buzei.
  SORT t_bseg BY bukrs belnr gjahr buzei.
  SORT t_bkpf_c BY bukrs
                   belnr
                   gjahr.
  SORT t_rbco BY belnr buzei.

  LOOP AT t_j_1bnfdoc INTO wa_j_1bnfdoc.

    CLEAR: wa_j_1bnflin,  t_j_1bnflin2[], wa_j_1batl1,v_netwrt.

*   Verifica se o documento está relacionado a um CT-e
    lo_cte_switch = cl_j_1bcte_swf=>get_instance( ).
    IF lo_cte_switch->is_cte_ctx_by_model( wa_j_1bnfdoc-model )  = abap_true OR
       lo_cte_switch->is_cte_ctx_by_docnum( iv_docnum = wa_j_1bnfdoc-docref ) = abap_true.
*     Documento é relacionado a CT-e  quando:
*       - Documento processado pertence ao modelo 57
*       - Documento processado não pertence a modelar 57 mas referes a CT-e. (Por exemplo, NF anulando uma CT-e)
      lx_cte_related = 'X'.
    ELSE.
      lx_cte_related = ' '.
    ENDIF.

    READ TABLE t_j_1bnflin TRANSPORTING NO FIELDS WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
    LOOP AT t_j_1bnflin INTO wa_j_1bnflin FROM sy-tabix.
      IF wa_j_1bnflin-docnum NE wa_j_1bnfdoc-docnum.
        EXIT.
      ENDIF.

      cont = 0.
      READ TABLE t_j_1bnflin TRANSPORTING NO FIELDS WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
      LOOP AT t_j_1bnflin INTO wa_j_1bnflin_aux FROM sy-tabix.
        IF wa_j_1bnflin_aux-docnum NE wa_j_1bnfdoc-docnum.
          EXIT.
        ENDIF.
        cont = cont + 1.
      ENDLOOP.

      CLEAR: wa_saida, wa_lfa1,wa_kna1, wa_j_1bnfstx, wa_j_1baj, wa_j_1bnfe_active,wa_bkpf, wa_makt, wa_vbap, wa_remessa, wa_remessa2, wa_remessa_3, wa_vbpa, x_data, x_hora.

      CLEAR :  wa_j_1bnfstx, v_netwrt.

      REFRESH: tmp_tax.

      READ TABLE  t_j_1bnfstx TRANSPORTING NO FIELDS WITH KEY docnum = wa_j_1bnflin-docnum  itmnum = wa_j_1bnflin-itmnum BINARY SEARCH.
      LOOP AT t_j_1bnfstx INTO wa_j_1bnfstx FROM sy-tabix.
        IF wa_j_1bnfstx-docnum NE wa_j_1bnflin-docnum OR  wa_j_1bnfstx-itmnum NE wa_j_1bnflin-itmnum.
          EXIT.
        ENDIF.
        IF wa_j_1bnfstx-taxtyp EQ 'ICM0' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM1' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM2' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM3' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM4' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM5' OR
           wa_j_1bnfstx-taxtyp EQ 'ICOF' OR
           wa_j_1bnfstx-taxtyp EQ 'IPIS'.
          ADD wa_j_1bnfstx-taxval TO v_netwrt.
        ENDIF.

        tmp_tax = wa_j_1bnfstx.
        APPEND tmp_tax.

        READ TABLE t_j_1baj INTO wa_j_1baj WITH KEY taxtyp = wa_j_1bnfstx-taxtyp BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          IF wa_j_1baj-taxgrp EQ c_pis.
*             PIS
            wa_saida-pis = wa_j_1bnfstx-taxval.
          ELSEIF wa_j_1baj-taxgrp EQ c_icms.
*             ICMS

**Inicio / Implementação solicitado no chamado CS2023000817 / AOENNING.
** modificação IR054102 solicitado pelo Adelson WSB
*            IF wa_j_1baj-taxtyp EQ 'ICM2' AND wa_j_1bnfdoc-direct = '1'.
*              CLEAR wa_saida-icms.
*            ELSE.
*              wa_saida-icms      = wa_j_1bnfstx-taxval.
*            ENDIF.

            IF wa_j_1baj-taxtyp+0(3) EQ 'ICM' ."AND wa_j_1bnfdoc-direct = '1'.
              wa_saida-icms      = wa_j_1bnfstx-taxval.
            ENDIF.

* modificação IR054102 solicitado pelo Adelson WSB
            READ TABLE t_j_1bajt INTO DATA(wa_j_1bajt) WITH KEY taxtyp = wa_j_1baj-taxtyp.
            IF sy-subrc EQ 0.
              wa_saida-ttypetxt = |{ wa_j_1bajt-taxtyp }-{ wa_j_1bajt-ttypetxt }|.
            ENDIF.
**Fim / Implementação solicitado no chamado



            wa_saida-outros    = wa_j_1bnfstx-base.
            wa_saida-base_icms = wa_j_1bnfstx-base.
*              wa_saida-outros    = wa_j_1bnfstx-othbas.
            wa_saida-excbas    = wa_j_1bnfstx-excbas.


          ELSEIF wa_j_1baj-taxgrp EQ c_cofins.
*             COFINS
            wa_saida-cofins = wa_j_1bnfstx-taxval.
          ELSEIF wa_j_1baj-taxgrp EQ c_iss .
*             ISS
            wa_saida-iss    = wa_j_1bnfstx-taxval.
          ELSEIF wa_j_1baj-taxtyp EQ 'INSS' .
*             ISS
            wa_saida-inss    = wa_j_1bnfstx-taxval.
          ENDIF.

        ENDIF.

      ENDLOOP.

      IF wa_j_1bnfdoc-nfe EQ 'X'.

        READ TABLE t_j_1bnfe_active INTO wa_j_1bnfe_active WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.

        IF wa_j_1bnfe_active-docsta EQ '  ' AND wa_j_1bnfe_active-scssta EQ ' ' .
          wa_saida-status = 'Não Enviada'.
        ELSEIF wa_j_1bnfe_active-docsta EQ '  '.
          wa_saida-status = 'Aguardando resposta'.
        ELSEIF wa_j_1bnfe_active-docsta EQ 1 AND ( wa_j_1bnfe_active-cancel EQ 'X' OR wa_j_1bnfe_active-scssta EQ '2' ).
          wa_saida-status = 'Cancelada'.
        ELSEIF wa_j_1bnfe_active-docsta EQ 1 AND ( wa_j_1bnfe_active-cancel NE 'X' AND wa_j_1bnfe_active-scssta NE '2' ).
          wa_saida-status = 'autorizada'.
        ELSEIF wa_j_1bnfe_active-docsta EQ 2.
          wa_saida-status = 'rejeitada'.
        ELSEIF wa_j_1bnfe_active-docsta EQ 3.
          wa_saida-status = 'recusada'.
        ELSE.
          wa_saida-status =  wa_j_1bnfe_active-docsta .
        ENDIF.
      ENDIF.

      IF wa_j_1bnfdoc-cancel EQ 'X'.
        wa_saida-nf_status = 'Estornada'.
      ELSE.
        wa_saida-nf_status = 'Ativa'.
      ENDIF.

      "Retira empresa do codigo do parceiro
      CLEAR: v_bukrs, v_parid.
      IF wa_j_1bnfdoc-parvw = 'BR'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_j_1bnfdoc-bukrs
          IMPORTING
            output = v_bukrs.

        CONDENSE v_bukrs NO-GAPS.
        v_len  = strlen( v_bukrs ).
        v_len2 = 10 - v_len.


        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_j_1bnfdoc-parid
          IMPORTING
            output = v_parid.

        CONDENSE v_parid NO-GAPS.
        v_parid = v_parid+v_len(v_len2).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_parid
          IMPORTING
            output = v_parid.

      ELSE.
        v_parid =  wa_j_1bnfdoc-parid.

      ENDIF.

      "Cliente / Fornecedor / Local
      CLEAR: wa_lfa1, wa_kna1.
      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro   = v_parid
          p_partype    = wa_j_1bnfdoc-partyp
        CHANGING
          wa_info_part = wa_lfa1
          wa_info_c    = wa_kna1.


      IF ( wa_lfa1-lifnr IS NOT INITIAL ) OR ( wa_j_1bnfdoc-partyp = 'B' ) .

        IF wa_lfa1-lifnr IS INITIAL.
          wa_saida-cod_clifor    = v_parid.

        ELSE.

          wa_saida-cod_clifor    = wa_lfa1-lifnr.

        ENDIF.

        wa_saida-nome_clifor   = wa_lfa1-name1.
        wa_saida-stcd3         = wa_lfa1-stcd3.
        wa_saida-uf_clifor     = wa_lfa1-regio.
        wa_saida-ort01         = wa_lfa1-ort01.
*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Início de Alteração
*        wa_saida-crt_bupla     = wa_j_1bnfdoc-crt_bupla." (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana- SMC
        wa_saida-crt_bupla     = wa_lfa1-crtn.
*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Fim de Alteração
        wa_saida-stkzn         = wa_lfa1-stkzn.

*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Início de Alteração
*        " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC INICIO>>
*        IF wa_j_1bnfdoc-crt_bupla = 1.
*          wa_saida-crt_bupla =  wa_saida-crt_bupla && ' - Simples Nacional'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 2.
*          wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Simples Nacional - rendimento bruto abaixo limite inferior'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 3.
*          wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Regime Normal (não simples)'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 4.
*          wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Diferimento'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 5.
*          wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Opção pela Tributação'.
*        ENDIF.
*        " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC FIM <<

        CASE wa_lfa1-crtn.
          WHEN 1.
            wa_saida-crt_bupla =  wa_saida-crt_bupla && ' - Simples Nacional'.
          WHEN 2.
            wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Simples Nacional - rendimento bruto abaixo limite inferior'.
          WHEN 3.
            wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Regime Normal (não simples)'.
          WHEN 4.
* - INICIO - IR241104 - 02/07/2025 - RRIBEIRO - STEFANINI
            wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Simples Nacional - Microempreendedor Individual - MEI'.
* - FIM - IR241104 - 02/07/2025 - RRIBEIRO - STEFANINI
          WHEN 5.
            wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Opção pela Tributação'.
          WHEN OTHERS.
            CLEAR wa_saida-crt_bupla.
        ENDCASE.
*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Fim de Alteração

        IF NOT wa_lfa1-stcd1 IS INITIAL.
          wa_saida-cpf_prod     = wa_lfa1-stcd1.
        ELSE.
          wa_saida-cpf_prod     = wa_lfa1-stcd2.
        ENDIF.

      ELSE.
        wa_saida-cod_clifor    =  v_parid.

*        IF WA_KNA1-LIFNR IS INITIAL.
*          WA_SAIDA-COD_CLIFOR    =  V_PARID.
*        ELSE.
*          WA_SAIDA-COD_CLIFOR    = WA_KNA1-LIFNR.
*        ENDIF.

        wa_saida-nome_clifor   = wa_kna1-name1.
        wa_saida-stcd3         = wa_kna1-stcd3.
        wa_saida-uf_clifor     = wa_kna1-regio.
        wa_saida-ort01         = wa_kna1-ort01.
        wa_saida-stkzn         = wa_kna1-stkzn.

*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Início de Alteração
*        wa_saida-crt_bupla     = wa_j_1bnfdoc-crt_bupla." (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana- SMC
        wa_saida-crt_bupla     = wa_kna1-crtn.
*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Fim de Alteração

*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Início de Alteração
*        " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC INICIO>>
*        IF wa_j_1bnfdoc-crt_bupla = 1.
*          wa_saida-crt_bupla =  wa_saida-crt_bupla && ' - Simples Nacional'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 2.
*          wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Simples Nacional - rendimento bruto abaixo limite inferior'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 3.
*          wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Regime Normal (não simples)'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 4.
*          wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Diferimento'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 5.
*          wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Opção pela Tributação'.
*        ENDIF.
*        " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC FIM <<

        CASE wa_kna1-crtn.
          WHEN 1.
            wa_saida-crt_bupla =  wa_saida-crt_bupla && ' - Simples Nacional'.
          WHEN 2.
            wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Simples Nacional - rendimento bruto abaixo limite inferior'.
          WHEN 3.
            wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Regime Normal (não simples)'.
          WHEN 4.
* - INICIO - IR241104 - 02/07/2025 - RRIBEIRO - STEFANINI
            wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Simples Nacional - Microempreendedor Individual - MEI'.
* - FIM - IR241104 - 02/07/2025 - RRIBEIRO - STEFANINI
          WHEN 5.
            wa_saida-crt_bupla  = wa_saida-crt_bupla && ' - Opção pela Tributação'.
          WHEN OTHERS.
            CLEAR wa_saida-crt_bupla.
        ENDCASE.
*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Fim de Alteração

        IF NOT wa_kna1-stcd1 IS INITIAL.
          wa_saida-cpf_prod     = wa_kna1-stcd1.
        ELSE.
          wa_saida-cpf_prod     = wa_kna1-stcd2.
        ENDIF.
      ENDIF.


*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            INPUT  = WA_J_1BNFDOC-PARID
*          IMPORTING
*            OUTPUT = WA_J_1BNFDOC-PARID.
*
*        CONDENSE WA_J_1BNFDOC-PARID NO-GAPS.
*        WA_J_1BNFDOC-PARID = WA_J_1BNFDOC-PARID+V_LEN(V_LEN2).
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT  = WA_J_1BNFDOC-PARID
*          IMPORTING
*            OUTPUT = WA_J_1BNFDOC-PARID.
*      ENDIF.
*
*      "Cliente / Fornecedor / Local
*      CLEAR: WA_LFA1, WA_KNA1.
*      CALL FUNCTION 'Z_PARCEIRO_INFO'
*        EXPORTING
*          P_PARCEIRO   = WA_J_1BNFDOC-PARID
*          P_PARTYPE    = WA_J_1BNFDOC-PARTYP
*        CHANGING
*          WA_INFO_PART = WA_LFA1
*          WA_INFO_C    = WA_KNA1.



      "Chave de Acesso
      CONCATENATE
        wa_j_1bnfe_active-regio
        wa_j_1bnfe_active-nfyear
        wa_j_1bnfe_active-nfmonth
        wa_j_1bnfe_active-stcd1
        wa_j_1bnfe_active-model
        wa_j_1bnfe_active-serie
        wa_j_1bnfe_active-nfnum9
        wa_j_1bnfe_active-docnum9
        wa_j_1bnfe_active-cdv    INTO wa_saida-chave_nfe.

      v_docnum = wa_j_1bnfdoc-docnum.

      READ TABLE t_j_1bnfdoc_e INTO DATA(w_j_1bnfdoc_p) WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
      IF sy-subrc = 0.
        IF w_j_1bnfdoc_p-docnum_s IS NOT INITIAL.
          v_docnum = w_j_1bnfdoc_p-docnum_s.
        ENDIF.
      ENDIF.
      READ TABLE t_parceiro TRANSPORTING NO FIELDS WITH KEY docnum = v_docnum BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT t_parceiro INTO wa_parceiro FROM sy-tabix.
          IF wa_parceiro-docnum NE v_docnum.
            EXIT.
          ENDIF.

          CLEAR: wa_lfa1, wa_kna1.

          CASE wa_parceiro-parvw.
            WHEN 'PC'.
              CALL FUNCTION 'Z_PARCEIRO_INFO'
                EXPORTING
                  p_parceiro   = wa_parceiro-parid
                  p_partype    = wa_parceiro-partyp
                CHANGING
                  wa_info_part = wa_lfa1
                  wa_info_c    = wa_kna1.
              IF wa_lfa1 IS NOT INITIAL.
                wa_saida-parc_coleta = wa_lfa1-name1.
                wa_saida-uf_coleta   = wa_lfa1-regio.
                wa_saida-cid_coleta  = wa_lfa1-ort01.
              ELSE.
                wa_saida-parc_coleta = wa_kna1-name1.
                wa_saida-uf_coleta   = wa_kna1-regio.
                wa_saida-cid_coleta  = wa_kna1-ort01.
              ENDIF.

              "WA_SAIDA-ORT01         = WA_LFA1-ORT01.
            WHEN 'LR'.

              CALL FUNCTION 'Z_PARCEIRO_INFO'
                EXPORTING
                  p_parceiro   = wa_parceiro-parid
                  p_partype    = wa_parceiro-partyp
                CHANGING
                  wa_info_part = wa_lfa1
                  wa_info_c    = wa_kna1.

              IF wa_lfa1 IS NOT INITIAL.
                wa_saida-local_ent   = wa_lfa1-ort01. "WA_LFA1-NAME1.
                wa_saida-uf_ent      = wa_lfa1-regio.
              ELSE.
                wa_saida-local_ent   = wa_kna1-ort01. "WA_LFA1-NAME1.
                wa_saida-uf_ent      = wa_kna1-regio.
              ENDIF.

            WHEN 'Z1'.
              CALL FUNCTION 'Z_PARCEIRO_INFO'
                EXPORTING
                  p_parceiro   = wa_parceiro-parid
                  p_partype    = wa_parceiro-partyp
                CHANGING
                  wa_info_part = wa_lfa1
                  wa_info_c    = wa_kna1.

              IF wa_lfa1 IS NOT INITIAL.
                wa_saida-local_ent   = wa_lfa1-ort01. "WA_LFA1-NAME1.
                wa_saida-uf_ent      = wa_lfa1-regio.
                wa_saida-terminal    = wa_lfa1-name1. "WA_LFA1-NAME1.
              ELSE.
                wa_saida-local_ent   = wa_lfa1-ort01. "WA_LFA1-NAME1.
                wa_saida-uf_ent      = wa_lfa1-regio.
                wa_saida-terminal    = wa_lfa1-name1. "WA_LFA1-NAME1.

              ENDIF.

            WHEN OTHERS.


              "*************************************************************************
*        CLEAR: WA_LFA1,WA_KNA1.
              IF wa_parceiro-parvw EQ 'V'.
                wa_saida-local_ent   = wa_lfa1-ort01. "WA_LFA1-NAME1.
                wa_saida-uf_ent      = wa_lfa1-regio.
                wa_saida-terminal    = wa_lfa1-name1. "WA_LFA1-NAME1.
              ELSE.
                READ TABLE it_zcarta INTO wa_zcarta WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  READ TABLE t_lfa1 INTO wa_lfa1
                      WITH KEY lifnr = wa_zcarta-novo_terminal BINARY SEARCH.

                  wa_saida-terminal = wa_lfa1-name1.
                  wa_saida-uf_terminal = wa_lfa1-regio.
                ELSE.
                  READ TABLE t_vbpa INTO wa_vbpa WITH KEY vbeln = wa_j_1bnflin-refkey(10) BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa-lifnr BINARY SEARCH.
                    wa_saida-terminal    = wa_lfa1-name1.
                    wa_saida-uf_terminal = wa_lfa1-regio.
                  ENDIF.
                ENDIF.
              ENDIF.
          ENDCASE.
******************************************
        ENDLOOP.
      ENDIF.

      READ TABLE t_remessa INTO wa_remessa WITH KEY refkey = wa_j_1bnflin-refkey(10) BINARY SEARCH.

**-------------------------------- Tipo Transporte ---------------------------------------------
      CLEAR  wa_saida-shtyp.
      IF wa_j_1bnflin-reftyp = 'BI' OR wa_j_1bnflin-reftyp = 'MD'. "fatura
        READ TABLE t_shtyp  INTO wa_shtyp WITH KEY vbeln = wa_j_1bnflin-refkey(10)   BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-shtyp = wa_shtyp-shtyp.
        ENDIF.
      ENDIF.

*      CLEAR  WA_SAIDA-SHTYP.
*      IF WA_J_1BNFLIN-REFTYP = 'MD'. "doc material
*        READ TABLE T_SHTYPM  INTO WA_SHTYPM WITH KEY MBLNR = WA_J_1BNFLIN-REFKEY(10)
*                                                     MJAHR = WA_J_1BNFLIN-REFKEY+10(4) BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          WA_SAIDA-SHTYP = WA_SHTYPM-SHTYP.
*        ENDIF.
*      ELSE.
*        IF SY-SUBRC = 0. " se tiver Remessa
*          READ TABLE T_SHTYP  INTO WA_SHTYP WITH KEY VBELN =  WA_REMESSA-VBELN BINARY SEARCH.
*          IF SY-SUBRC = 0.
*            WA_SAIDA-SHTYP = WA_SHTYP-SHTYP.
*          ENDIF.
*        ENDIF.
*      ENDIF.
**-------------------------------- Tipo Transporte ---------------------------------------------

      READ TABLE t_remessa_3 INTO wa_remessa_3 WITH KEY vbeln = wa_remessa-refkey BINARY SEARCH .

      CLEAR: wa_saida-doc_contabil, wa_bkpf_aux.
      CASE  wa_j_1bnflin-reftyp.
        WHEN 'LI'.
          READ TABLE t_bkpf_lin INTO wa_bkpf_aux WITH KEY awkey = wa_j_1bnflin-refkey(20)
                                                      blart = 'RE' BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida-doc_contabil = wa_bkpf_aux-belnr.
          ENDIF.

        WHEN 'ZW'.

          READ TABLE t_zfiwrt0008 INTO wa_zfiwrt0008 WITH KEY seq_lcto = wa_j_1bnflin-refkey(10) BINARY SEARCH.

          IF ( sy-subrc EQ 0 ).
            READ TABLE t_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_zfiwrt0008-obj_key BINARY SEARCH.
            IF sy-subrc = 0.
              wa_saida-doc_contabil = wa_zib_contabil_chv-belnr.
            ENDIF.
            IF wa_zfiwrt0008-belnr_imb IS NOT INITIAL.
              wa_saida-doc_contabil = wa_zfiwrt0008-belnr_imb.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          IF wa_remessa_3-refkey IS NOT INITIAL.
            READ TABLE t_bkpf INTO wa_bkpf WITH KEY awkey = wa_remessa_3-refkey BINARY SEARCH.
            IF sy-subrc = 0.
              wa_saida-doc_contabil = wa_bkpf-belnr.
            ENDIF.
          ELSE.
            IF wa_j_1bnflin-refkey IS NOT INITIAL.
              "Se não tem remessa procura pelo bkpf - CSB
              READ TABLE t_bkpf_lin INTO wa_bkpf_aux WITH KEY awkey = wa_j_1bnflin-refkey(20)
                                                          blart = 'RV' BINARY SEARCH.
              IF sy-subrc = 0.
                wa_saida-doc_contabil = wa_bkpf_aux-belnr.
              ENDIF.
            ENDIF.
          ENDIF.

      ENDCASE.
                                                            "CH.129254
      wa_saida-netwrt   = wa_j_1bnflin-netwr .

      READ TABLE t_j_1bnflin2 WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CLEAR: wa_saida-vlr_dolar.
      ENDIF.


      READ TABLE t_vbap INTO wa_vbap WITH KEY vbeln = wa_remessa-vgbel BINARY SEARCH.
      wa_saida-utilizacao  = wa_vbap-bezei.

      utilizacao = ''.

      READ TABLE t_vbrp INTO wa_vbrp WITH KEY vbeln = wa_j_1bnflin-refkey(10) BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_remessa-vgbel
          IMPORTING
            output = wa_saida-ordem.
***--------------------------------------------
        IF wa_saida-ordem IS NOT INITIAL.
          READ TABLE it_zsdt0053 INTO wa_zsdt0053
              WITH KEY vbeln = wa_saida-ordem BINARY SEARCH.

          IF sy-subrc = 0.
            wa_saida-instrucao = wa_zsdt0053-instrucao.
          ENDIF.

        ENDIF.
***---------------------------------------------


        "WA_SAIDA-ORDEM       = WA_REMESSA-VGBEL.
        wa_saida-remessa     = wa_remessa-vbeln.
        wa_saida-romaneio    = wa_remessa-nr_romaneio.
        wa_saida-ref_ro      = wa_remessa-ch_referencia.
        wa_saida-doc_fatura  = wa_remessa-refkey.

        CLEAR wa_lfa1.

      ENDIF.
      IF wa_saida-ordem IS INITIAL.
        READ TABLE t_j_1bnfdoc_e INTO DATA(w_j_1bnfdoc_e) WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-ordem = w_j_1bnfdoc_e-vbeln.
        ENDIF.
      ENDIF.

      READ TABLE it_zfiwrt0008 INTO wa_zfiwrt0008 WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_zfiwrt0009 INTO wa_zfiwrt0009 WITH KEY seq_lcto = wa_zfiwrt0008-seq_lcto
                                                             itmnum   = wa_j_1bnflin-itmnum
                                                             BINARY SEARCH.
        IF ( sy-subrc = 0 ) AND
           ( wa_zfiwrt0008-imobilizado = 'S' ) AND
           ( wa_zfiwrt0009-anln1 IS NOT INITIAL ).
          wa_saida-anln1 =  wa_zfiwrt0009-anln1.
        ENDIF.
      ENDIF.

      " Início - US 128292 - Inserção da placa do veículo - RSA
      READ TABLE it_zlest0039 INTO wa_zlest0039 WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida-placa_cav = wa_zlest0039-placa_cav.
      ENDIF.
      " Fim - US 128292 - Inserção da placa do veículo - RSA


      READ TABLE t_makt INTO wa_makt WITH KEY  matnr  = wa_j_1bnflin-matnr BINARY SEARCH.
      "WA_SAIDA-PRODUTO             = WA_MAKT-MAKTX.
      wa_saida-produto             = wa_j_1bnflin-matnr.

      READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_j_1bnflin-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-matkl   =  wa_t023t-matkl.
          wa_saida-wgbez60 =  wa_t023t-wgbez60.
        ENDIF.
      ENDIF.

      IF wa_j_1bnfdoc-nfe EQ 'X' .
        wa_saida-nfenum   = wa_j_1bnfdoc-nfenum.
      ELSE .
        wa_saida-nfenum   = wa_j_1bnfdoc-nfnum.
      ENDIF.

      quantidade = wa_j_1bnflin-menge.

      IF quantidade EQ 0.
        quantidade = 1.
      ENDIF.

      CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
        EXPORTING
          branch            = wa_j_1bnfdoc-branch
          bukrs             = wa_j_1bnfdoc-bukrs
        IMPORTING
          address           = wa_adress
        EXCEPTIONS
          branch_not_found  = 1
          address_not_found = 2
          company_not_found = 3
          OTHERS            = 4.

      wa_saida-inco1      = wa_j_1bnfdoc-inco1.
      wa_saida-inco2      = wa_j_1bnfdoc-inco2.
      wa_saida-nfe      = wa_j_1bnfdoc-nfe   .
      wa_saida-bukrs    = wa_j_1bnfdoc-bukrs .
      wa_saida-branch   = wa_j_1bnfdoc-branch.
      wa_saida-uf_filial   = wa_adress-regio.
      wa_saida-docnum   = wa_j_1bnfdoc-docnum.
      wa_saida-pstdat   = wa_j_1bnfdoc-pstdat.
      wa_saida-docdat   = wa_j_1bnfdoc-docdat.
      wa_saida-series   = wa_j_1bnfdoc-series.
      wa_saida-ntgew    = wa_j_1bnfdoc-ntgew.
      wa_saida-direct   = wa_j_1bnfdoc-direct.
      wa_saida-cfop     = wa_j_1bnflin-cfop  .
      wa_saida-menge    = wa_j_1bnflin-menge .
      wa_saida-meins    = wa_j_1bnflin-meins .
      wa_saida-charg    = wa_j_1bnflin-charg.
      wa_saida-ncm      = wa_j_1bnflin-nbm.
*      wa_saida-crt_bupla     = wa_j_1bnfdoc-crt_bupla."125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC

      wa_saida-itmnum   = wa_j_1bnflin-itmnum.

      wa_saida-nftype  = wa_j_1bnfdoc-nftype.
      wa_saida-model   = wa_j_1bnfdoc-model.
      wa_saida-refkey  = wa_j_1bnflin-refkey.

      wa_saida-maktx  = wa_j_1bnflin-maktx.

      READ TABLE t_j_1batl1 INTO wa_j_1batl1 WITH KEY taxlaw = wa_j_1bnflin-taxlw1 BINARY SEARCH.

      IF ( sy-subrc EQ 0 ).
        READ TABLE t_j_1batl1t
          INTO wa_j_1batl1t
         WITH KEY taxlaw = wa_j_1batl1-taxlaw BINARY SEARCH.

        IF sy-subrc = 0.
          CONCATENATE wa_j_1batl1t-line1
                      wa_j_1batl1t-line2
                      wa_j_1batl1t-line3
                      wa_j_1batl1t-line4
          INTO wa_saida-lei_icms
          SEPARATED BY space.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
          EXPORTING
            input  = wa_j_1batl1-taxsit
          IMPORTING
            output = wa_saida-taxlw1.

**  Begin of CS2022000515 #97985 FF   28.11.2022
        wa_saida-cod_icms = wa_j_1batl1t-taxlaw.

        IF wa_saida-lei_icms IS INITIAL.
          wa_saida-lei_icms = wa_j_1batl1t-descrip.
        ENDIF.
** End of FF  28.11.2022

        CLEAR: wa_j_1batl1, wa_j_1batl1t.
      ENDIF.

      READ TABLE t_j_1batl2 INTO wa_j_1batl2 WITH  KEY taxlaw = wa_j_1bnflin-taxlw2 BINARY SEARCH.

      IF ( sy-subrc EQ 0 ).
        wa_saida-taxlw2 = wa_j_1batl2-taxsit.
        CLEAR: wa_j_1batl1.
      ENDIF.

      "Verifica CST Saida
      IF ( wa_j_1bnflin-taxlw4 IS NOT INITIAL ) AND
         ( wa_j_1bnflin-taxsi4 IS NOT INITIAL ) AND
         ( wa_j_1bnfdoc-direct EQ '2'         ).

        READ TABLE t_j_1batl4a INTO wa_j_1batl4a
         WITH KEY taxlaw = wa_j_1bnflin-taxlw4
                        taxsitout = wa_j_1bnflin-taxsi4 BINARY SEARCH.

        IF sy-subrc = 0.
          READ TABLE t_j_1btaxsitcoft INTO wa_j_1btaxsitcoft
           WITH KEY taxsit = wa_j_1bnflin-taxsi4 BINARY SEARCH.

          IF sy-subrc EQ 0.
            wa_saida-lei_cofins = wa_j_1btaxsitcoft-txt.
            wa_saida-taxlw4 = wa_j_1bnflin-taxsi4.
          ENDIF.
        ENDIF.
        CLEAR: wa_j_1batl4a.
      ENDIF.

      IF wa_saida-taxlw4 IS INITIAL.

        READ TABLE t_j_1batl4a INTO wa_j_1batl4a WITH KEY taxlaw = wa_j_1bnflin-taxlw4.

        IF ( sy-subrc EQ 0 ).
          wa_saida-taxlw4 = wa_j_1batl4a-taxsit.
          READ TABLE t_j_1batl4t
            INTO wa_j_1batl4t
           WITH KEY taxlaw = wa_j_1batl4a-taxlaw BINARY SEARCH.

          IF sy-subrc = 0.
            CONCATENATE wa_j_1batl4t-line1
                        wa_j_1batl4t-line2
                        wa_j_1batl4t-line3
                        wa_j_1batl4t-line4
            INTO wa_saida-lei_cofins
            SEPARATED BY space.
          ENDIF.

          CLEAR: wa_j_1batl1, wa_j_1batl4t.
        ENDIF.

      ENDIF.
      DATA(v_nbm) = wa_j_1bnflin-nbm.
      REPLACE ALL OCCURRENCES OF '.' IN v_nbm WITH ' '.
      CONDENSE v_nbm NO-GAPS.

*      READ TABLE t_zib_nfe_dist_itm INTO wa_zib_nfe_dist_itm
*         WITH KEY chave_nfe = wa_saida-chave_nfe
*                  prod_ncm  = v_nbm BINARY SEARCH .
*      IF sy-subrc = 0.
*        wa_saida-taxlw4_xml = wa_zib_nfe_dist_itm-pis_cst.
*        wa_saida-taxlw5_xml = wa_zib_nfe_dist_itm-cof_cst.
*      ENDIF.

      DESCRIBE TABLE t_zib_nfe_dist_itm LINES DATA(vl_lines).

      IF vl_lines > 1.
        READ TABLE t_zib_nfe_dist_itm INTO DATA(wa_zib_nfe_itm) WITH KEY chave_nfe = wa_saida-chave_nfe
                                                                         prod_ncm  = v_nbm.
        IF sy-subrc EQ 0.
          wa_saida-taxlw4_xml = wa_zib_nfe_itm-pis_cst.
          wa_saida-taxlw5_xml = wa_zib_nfe_itm-cof_cst.
        ENDIF.
      ELSE.
        READ TABLE t_zib_nfe_dist_itm INTO wa_zib_nfe_itm INDEX 1.
        IF sy-subrc EQ 0.
          wa_saida-taxlw4_xml = wa_zib_nfe_itm-pis_cst.
          wa_saida-taxlw5_xml = wa_zib_nfe_itm-cof_cst.
        ENDIF.
      ENDIF.


      "Verifica CST Saida
      IF ( wa_j_1bnflin-taxlw5 IS NOT INITIAL ) AND
         ( wa_j_1bnflin-taxsi5 IS NOT INITIAL ) AND
         ( wa_j_1bnfdoc-direct EQ '2'         ).

        READ TABLE t_j_1batl5 INTO wa_j_1batl5
         WITH KEY taxlaw = wa_j_1bnflin-taxlw5
                  taxsitout = wa_j_1bnflin-taxsi5 BINARY SEARCH.

        IF sy-subrc = 0.
          wa_saida-taxlw5 = wa_j_1bnflin-taxsi5.
        ENDIF.
        CLEAR: wa_j_1batl5.

      ENDIF.

      IF wa_saida-taxlw5 IS INITIAL.

        READ TABLE t_j_1batl5 INTO wa_j_1batl5 WITH KEY taxlaw = wa_j_1bnflin-taxlw5.

        IF ( sy-subrc EQ 0 ).
          wa_saida-taxlw5 = wa_j_1batl5-taxsit.
        ENDIF.

      ENDIF.

      IF ( wa_j_1bnflin-reftyp EQ 'BI' ).

        CLEAR: wa_vbrp_aux.
        READ TABLE  t_vbrp_lin INTO wa_vbrp_aux WITH KEY vbeln = wa_j_1bnflin-refkey(10) BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-iva = wa_vbrp_aux-j_1btxsdc.
        ENDIF.

      ELSEIF ( wa_j_1bnflin-reftyp EQ 'LI').

        CLEAR: wa_rseg_aux.
        v_belnr = wa_j_1bnflin-refkey+0(10).
        v_gjahr = wa_j_1bnflin-refkey+10(4).
        READ TABLE t_rseg_lin INTO wa_rseg_aux WITH KEY belnr = v_belnr
                                                        gjahr = v_gjahr
                                                        buzei =	wa_j_1bnflin-refitm BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-iva = wa_rseg_aux-mwskz.
        ENDIF.

      ENDIF.


      "Valor Total da NF

*      call function 'J_1B_NF_VALUE_DETERMINATION'
*        exporting
*          nf_header   = wa_j_1bnfdoc
*        importing
*          ext_header  = wa_j_cabe
*        tables
*          nf_item     = t_j_1bnflin
*          nf_item_tax = t_j_1bnfstx.

      " Se não achar BSID E BSAD continuar com valores
*******************-------------------------------------------
      " Se for nota de serviço (exceção a regra)
      IF wa_j_1bnfdoc-nfesrv EQ 'X' .
        wa_saida-netwrt   = wa_j_1bnflin-nfnet.
      ENDIF.

      IF  wa_saida-netwrt EQ 0 .
*        IF WA_J_1BNFLIN-NFNET > 0 .
*          WA_SAIDA-NETWRT   = WA_J_1BNFLIN-NFNET.
*        ELSE.
*         comentado em 04.12.12 solicitação Marcos Santos
*          IF P_DIRECT = 1 AND WA_J_1BNFLIN-REFTYP NE 'ZW'.
*            WA_SAIDA-NETWRT   = WA_J_1BNFLIN-NETWR  + WA_SAIDA-ICMS + WA_SAIDA-PIS + WA_SAIDA-COFINS + WA_SAIDA-ISS.
*          ELSE.
*            WA_SAIDA-NETWRT   = WA_J_1BNFLIN-NETWR .
*          ENDIF.
        wa_saida-netwrt   = wa_j_1bnflin-netwr .
*        ENDIF.
      ENDIF.
      " Valor Item
      IF cont > 1.
        wa_saida-netwrt = wa_j_1bnflin-netwr.
      ENDIF.


      "Comentado porque estava somando o imposto.
*      "ALRS
      "IF V_NETWRT GT 0 AND  1 IN P_DIRECT[].
      "  ADD V_NETWRT TO WA_SAIDA-NETWRT.
      "ENDIF.

      CLEAR: wa_lin_e, wa_lin_i.

      MOVE-CORRESPONDING wa_j_1bnflin TO wa_lin_e.

      CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION_I'
        EXPORTING
          nf_item                 = wa_lin_e
          iv_nf_direction         = wa_j_1bnfdoc-direct
          ix_posted_with_xml_data = wa_j_1bnfdoc-autom_incoming
          ix_cte_related          = lx_cte_related
        IMPORTING
          ext_item                = wa_lin_i
        TABLES
          nf_item_tax             = tmp_tax.

      IF wa_lin_i-nftot > 0 .
        wa_saida-netwrt = wa_lin_i-nftot.
      ELSEIF wa_lin_i-nfnett > 0 .
        wa_saida-netwrt = wa_lin_i-nfnett.
      ELSEIF wa_lin_i-netwrt > 0.
        wa_saida-netwrt = wa_lin_i-netwrt.
      ENDIF.

      wa_saida-vlr_unit = wa_saida-netwrt / quantidade.
      wa_saida-user     = sy-uname.
      wa_saida-kursk      = 0.
      wa_saida-vlr_dolar  = 0.
      wa_saida-unit_dolar = 0.
      "ALRS 11/01/2016
      IF wa_saida-doc_contabil  IS NOT INITIAL.
        CLEAR wa_vbrp-kursk.

        READ TABLE t_bseg
          INTO wa_bseg
          WITH KEY bukrs = wa_saida-bukrs
                   belnr = wa_saida-doc_contabil BINARY SEARCH.
        IF sy-subrc = 0.
          IF wa_bseg-dmbe2 NE 0.
            wa_vbrp-kursk      = wa_bseg-dmbtr / wa_bseg-dmbe2.
          ENDIF.
        ENDIF.

        "Busca Data e Hora Registro
        CLEAR wa_bkpf_c.

        READ TABLE t_bkpf_c
          INTO wa_bkpf_c
          WITH KEY bukrs = wa_saida-bukrs
                   belnr = wa_saida-doc_contabil
                   gjahr = wa_saida-pstdat(4) BINARY SEARCH.

        IF sy-subrc = 0.

          IF wa_bkpf_c-cpudt NE 0.
            wa_saida-data_registro   = wa_bkpf_c-cpudt.
          ENDIF.

          IF wa_bkpf_c-cputm NE 0.
            wa_saida-hora_registro   = wa_bkpf_c-cputm.
          ENDIF.

        ENDIF.

        READ TABLE t_rbco
          INTO wl_rbco
          WITH KEY  belnr = wa_saida-refkey+0(10)
                    buzei = wa_rseg_aux-buzei BINARY SEARCH.

        IF ( sy-subrc EQ 0 ).

          wa_saida-ordem_doc = wl_rbco-aufnr.
          SHIFT wa_saida-ordem_doc LEFT DELETING LEADING '0'.
          CLEAR: wl_rbco.

        ENDIF.

      ENDIF.

*      READ TABLE T_BSID INTO WA_BSID WITH KEY BELNR = WA_BKPF-BELNR.
*      IF SY-SUBRC IS INITIAL.
*        IF WA_BSID-DMBE2 NE 0.
*          WA_VBRP-KURSK      = WA_BSID-DMBTR / WA_BSID-DMBE2.
*        ENDIF.
*      ELSE.
*        READ TABLE T_BSAD INTO WA_BSAD WITH KEY BELNR = WA_BKPF-BELNR.
*        IF SY-SUBRC IS INITIAL.
*          IF WA_BSAD-DMBE2 NE 0.
*            WA_VBRP-KURSK      = WA_BSAD-DMBTR / WA_BSAD-DMBE2.
*          ENDIF.
*        ELSE.
*          READ TABLE T_BSIK INTO WA_BSIK WITH KEY BELNR = WA_BKPF-BELNR.
*          IF SY-SUBRC IS INITIAL.
*            IF WA_BSIK-DMBE2 NE 0.
*              WA_VBRP-KURSK      = WA_BSIK-DMBTR / WA_BSIK-DMBE2.
*            ENDIF.
*          ELSE.
*            READ TABLE T_BSAK INTO WA_BSAK WITH KEY BELNR = WA_BKPF-BELNR.
*            IF SY-SUBRC IS INITIAL.
*              WA_VBRP-KURSK      = WA_BSAK-DMBTR / WA_BSAK-DMBE2.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.

      IF wa_vbrp-kursk GT 0.
        wa_saida-kursk      = wa_vbrp-kursk."TAXA
        wa_saida-vlr_dolar  = wa_saida-netwrt / wa_vbrp-kursk."VLR DOLAR
        wa_saida-unit_dolar = ( wa_saida-netwrt / quantidade ) /  wa_vbrp-kursk.
      ENDIF.

*      IF  WA_SAIDA-VLR_DOLAR EQ 0 . " Se não achar BSID E BSAD continuar com valores
*        IF WA_VBRP-KURSK > 1 .
*          WA_SAIDA-KURSK      = WA_VBRP-KURSK."TAXA
*          WA_SAIDA-VLR_DOLAR  = WA_SAIDA-NETWRT / WA_VBRP-KURSK."VLR DOLAR
*          WA_SAIDA-UNIT_DOLAR = ( WA_SAIDA-NETWRT / QUANTIDADE ) /  WA_VBRP-KURSK.
*        ELSE.
*          WA_SAIDA-KURSK      = 0.
*          WA_SAIDA-VLR_DOLAR  = 0.
*          WA_SAIDA-UNIT_DOLAR = 0.
*        ENDIF.
*      ENDIF.

*----------------- Info de Ordem CS2017001206 INÍCIO

      IF ( wa_j_1bnflin-reftyp EQ 'LI' )
*** Stefanini - IR221568 - 17/02/2025 - LAZAROSR - Início de Alteração
      OR ( wa_j_1bnflin-reftyp EQ 'MD' ).
*** Stefanini - IR221568 - 17/02//2025 - LAZAROSR - Fim de Alteração

        CLEAR: wa_ekbe, wa_aufk.

        READ TABLE t_ekbe INTO wa_ekbe WITH KEY belnr = wa_j_1bnflin-refkey(10)
                                                buzei = wa_j_1bnflin-refitm+2(4) BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          wa_saida-ebeln = wa_ekbe-ebeln.
          wa_saida-ebelp2 = wa_ekbe-ebelp. "CS2022000515 - FF  28.11.2022

*---
          READ TABLE t_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekbe-ebeln
                                                  ebelp = wa_ekbe-ebelp BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            wa_saida-knttp = wa_ekpo-knttp.

**  Begin of "CS2022000515 - FF  16.01.2023
            CASE wa_ekpo-recap.
              WHEN 'S'.
                wa_saida-recap = 'SIM'.
              WHEN 'N'.
                wa_saida-recap = 'NÃO'.
              WHEN OTHERS.
            ENDCASE.

            READ TABLE t_uf_pedido INTO DATA(wa_uf) WITH KEY werks = wa_ekpo-werks.
            IF sy-subrc = 0.
              wa_saida-regio = wa_uf-regio.
            ELSE.
              CLEAR wa_saida-regio.
            ENDIF.
** End of "CS2022000515 - FF  16.01.2023


          ENDIF.



*---
          READ TABLE t_ekkn INTO wa_ekkn WITH KEY ebeln = wa_ekbe-ebeln
                                                  ebelp = wa_ekbe-ebelp BINARY SEARCH.
          IF sy-subrc IS INITIAL.
**  Begin of "CS2022000515 - FF  28.11.2022
            wa_saida-anln1 = wa_ekkn-anln1.
            wa_saida-anln2 = wa_ekkn-anln2.
**  End of "CS2022000515 - FF  28.11.2022

**  Begin of "CS2022000515 - FF  16.01.2023
            READ TABLE t_ordem_pm INTO DATA(wa_ordem_pm) WITH KEY aufnr = wa_ekkn-aufnr.
            IF sy-subrc = 0.
              wa_saida-vornr = wa_ordem_pm-vornr.
              wa_saida-ltxa1 = wa_ordem_pm-ltxa1.
            ENDIF.
** End of "CS2022000515 - FF  16.01.2023

            READ TABLE t_aufk INTO wa_aufk WITH KEY aufnr = wa_ekkn-aufnr BINARY SEARCH.

            IF sy-subrc IS INITIAL.

              wa_saida-auart2 = wa_aufk-auart.
              wa_saida-autyp = wa_aufk-autyp.
**  Begin of "CS2022000515 - FF  28.11.2022
              wa_saida-aufnr = wa_aufk-aufnr.
*              wa_saida-ktext = wa_aufk-ktext.
** End of "CS2022000515 - FF  28.11.2022
              READ TABLE t_dd07v INTO wa_dd07v WITH KEY domvalue_l = wa_saida-autyp BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wa_saida-ddtext = wa_dd07v-ddtext.
              ENDIF.

            ENDIF.

            wa_saida-kostl = COND #( WHEN wa_saida-knttp NE ' ' THEN wa_ekkn-kostl ELSE '' ).
            wa_saida-sakto = COND #( WHEN wa_saida-knttp NE ' ' THEN wa_ekkn-sakto ELSE '' ).

            READ TABLE t_skat INTO wa_skat WITH KEY saknr = wa_saida-sakto BINARY SEARCH.
            wa_saida-desc_conta_ctb = wa_skat-txt50.

            READ TABLE t_cskt INTO ws_cskt WITH KEY kostl =  wa_saida-kostl BINARY SEARCH.
            wa_saida-desc_centro_cust = ws_cskt-ltext.

          ENDIF.


*---
        ENDIF.

      ENDIF.

*----------------- Info de Ordem CS2017001206 FIM


      IF wa_saida-nf_status EQ 'Cancelada'.
        wa_saida-menge      = 0.
        wa_saida-netwrt     = 0.
        wa_saida-vlr_unit   = 0.
        wa_saida-pis        = 0.
        wa_saida-icms       = 0.
        wa_saida-base_icms  = 0.
        wa_saida-outros     = 0.
        wa_saida-excbas     = 0.
        wa_saida-cofins     = 0.
        wa_saida-kursk      = 0.
        wa_saida-vlr_dolar  = 0.
        wa_saida-unit_dolar = 0.
      ENDIF.

      x_data = sy-datum.
      x_hora = sy-uzeit.

      CONCATENATE x_data+6(2) '/'
                  x_data+4(2) '/'
                  x_data(4)   ' -  '
                  x_hora(2)   ':'
                  x_hora+2(2) ':'
                  x_hora+4(2) INTO wa_saida-data.
      i = '1'.

      IF ( p_ativas EQ 'X' AND wa_j_1bnfdoc-cancel NE 'X' AND wa_j_1bnfdoc-nfe NE 'X'     ) OR
         ( p_nativa EQ 'X' AND wa_j_1bnfdoc-cancel EQ 'X' AND wa_j_1bnfdoc-nfe NE 'X'     ) OR
         ( p_autor  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfdoc-nfe EQ 'X' AND ( wa_j_1bnfe_active-cancel NE 'X' AND wa_j_1bnfe_active-scssta NE '2' )  ) OR
         ( p_rejeit EQ 'X' AND wa_j_1bnfe_active-docsta EQ 2 AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
         ( p_recus  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 3 AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
         ( p_cancel EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND ( wa_j_1bnfe_active-cancel EQ 'X' OR wa_j_1bnfe_active-scssta EQ '2' ) ) OR
     "    ( p_autor  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfe_active-cancel EQ 'X' AND  wa_j_1bnfdoc-nfe EQ 'X'  ) OR
         ( p_agres  EQ 'X' AND wa_j_1bnfe_active-docsta EQ ' ' AND wa_j_1bnfe_active-scssta NE ' ' AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
         ( p_nenv   EQ 'X' AND wa_j_1bnfe_active-docsta EQ ' ' AND wa_j_1bnfe_active-scssta EQ   ' '  AND wa_j_1bnfdoc-nfe EQ 'X'  ).
        i = '0'.
      ENDIF.

      IF ( p_ativas EQ 'X' AND p_nativa NE 'X' AND  wa_j_1bnfdoc-cancel EQ 'X' ) OR
         ( p_nativa EQ 'X' AND p_ativas NE 'X' AND  wa_j_1bnfdoc-cancel NE 'X' ) .
        i = '1'.
      ENDIF.



      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-ordem
        IMPORTING
          output = p_nordem_c.


      IF  ( p_nordem  IS INITIAL AND i = '0' ).
        i = '0'.
      ELSEIF ( (  p_nordem IS NOT INITIAL AND p_nordem_c IN p_nordem ) AND i = '0' ).
        i = '0'.
      ELSEIF ( ( p_nordem IS NOT INITIAL  AND p_nordem_c NOT IN p_nordem ) AND i = '0') .
        i = '1'.
      ENDIF.
*--> CS1000080/IR099136--->
      IF p_frepro EQ 'X' AND wa_saida-doc_contabil EQ ''.
        SELECT SINGLE belnr INTO wa_saida-doc_contabil
           FROM bkpf
            WHERE awkey EQ wa_saida-refkey.
      ENDIF.
*<-- CS1000080/IR099136 <---
      IF i EQ '1' .
        CONTINUE.
      ELSE.
        APPEND wa_saida TO t_saida.
      ENDIF.

      APPEND wa_j_1bnflin TO t_j_1bnflin2.

    ENDLOOP.

    CLEAR: wa_j_1bnfdoc, wa_saida.

  ENDLOOP.



  LOOP AT t_saida INTO wa_saida.
    wa_ret_movimento_mensal_zconf-inco1          = wa_saida-inco1.
    wa_ret_movimento_mensal_zconf-inco2          = wa_saida-inco2.

    wa_ret_movimento_mensal_zconf-bukrs         = wa_saida-bukrs.
    wa_ret_movimento_mensal_zconf-branch        = wa_saida-branch.
    wa_ret_movimento_mensal_zconf-cfop          = wa_saida-cfop.
    wa_ret_movimento_mensal_zconf-nfenum        = wa_saida-nfenum.
    wa_ret_movimento_mensal_zconf-series        = wa_saida-series.
    wa_ret_movimento_mensal_zconf-meins         = wa_saida-meins.
    wa_ret_movimento_mensal_zconf-pstdat        = wa_saida-pstdat.
    wa_ret_movimento_mensal_zconf-docdat        = wa_saida-docdat.
    wa_ret_movimento_mensal_zconf-menge         = wa_saida-menge.
    wa_ret_movimento_mensal_zconf-ntgew         = wa_saida-ntgew.
    wa_ret_movimento_mensal_zconf-netwrt        = wa_saida-netwrt.
    wa_ret_movimento_mensal_zconf-base_icms     = wa_saida-base_icms.
    wa_ret_movimento_mensal_zconf-outros        = wa_saida-outros.
    wa_ret_movimento_mensal_zconf-icms          = wa_saida-icms.
    wa_ret_movimento_mensal_zconf-pis           = wa_saida-pis.
    wa_ret_movimento_mensal_zconf-cofins        = wa_saida-cofins.
    wa_ret_movimento_mensal_zconf-iss           = wa_saida-iss.
    wa_ret_movimento_mensal_zconf-inss          = wa_saida-inss.
    wa_ret_movimento_mensal_zconf-docnum        = wa_saida-docnum.
    wa_ret_movimento_mensal_zconf-stcd3         = wa_saida-stcd3.
    wa_ret_movimento_mensal_zconf-excbas        = wa_saida-excbas.
    wa_ret_movimento_mensal_zconf-produto       = wa_saida-produto.
    wa_ret_movimento_mensal_zconf-matkl         = wa_saida-matkl.
    wa_ret_movimento_mensal_zconf-wgbez60       = wa_saida-wgbez60.
    wa_ret_movimento_mensal_zconf-ordem         = wa_saida-ordem.
    wa_ret_movimento_mensal_zconf-remessa       = wa_saida-remessa.
    wa_ret_movimento_mensal_zconf-romaneio      = wa_saida-romaneio.
    wa_ret_movimento_mensal_zconf-ref_ro        = wa_saida-ref_ro.
    wa_ret_movimento_mensal_zconf-utilizacao    = wa_saida-utilizacao.
    wa_ret_movimento_mensal_zconf-doc_fatura    = wa_saida-doc_fatura.
    wa_ret_movimento_mensal_zconf-kursk         = wa_saida-kursk.
    wa_ret_movimento_mensal_zconf-vlr_dolar     = wa_saida-vlr_dolar.
    wa_ret_movimento_mensal_zconf-cpf_prod      = wa_saida-cpf_prod.
    wa_ret_movimento_mensal_zconf-unit_dolar    = wa_saida-unit_dolar.
    wa_ret_movimento_mensal_zconf-status        = wa_saida-status.
    wa_ret_movimento_mensal_zconf-nf_status     = wa_saida-nf_status.
    wa_ret_movimento_mensal_zconf-vlr_unit      = wa_saida-vlr_unit.
    wa_ret_movimento_mensal_zconf-cod_clifor    = wa_saida-cod_clifor.
    wa_ret_movimento_mensal_zconf-nome_clifor   = wa_saida-nome_clifor.
    wa_ret_movimento_mensal_zconf-uf_clifor     = wa_saida-uf_clifor.
    wa_ret_movimento_mensal_zconf-ort01         = wa_saida-ort01.
    wa_ret_movimento_mensal_zconf-crt_bupla     = wa_saida-crt_bupla."125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC
    wa_ret_movimento_mensal_zconf-stkzn         = wa_saida-stkzn.
    wa_ret_movimento_mensal_zconf-parc_coleta   = wa_saida-parc_coleta.
    wa_ret_movimento_mensal_zconf-cid_coleta    = wa_saida-cid_coleta.
    wa_ret_movimento_mensal_zconf-uf_coleta     = wa_saida-uf_coleta.
    wa_ret_movimento_mensal_zconf-local_ent     = wa_saida-local_ent.
    wa_ret_movimento_mensal_zconf-uf_ent        = wa_saida-uf_ent.
    wa_ret_movimento_mensal_zconf-terminal      = wa_saida-terminal.
    wa_ret_movimento_mensal_zconf-uf_terminal   = wa_saida-uf_terminal.
    wa_ret_movimento_mensal_zconf-nfe           = wa_saida-nfe.
    wa_ret_movimento_mensal_zconf-data          = wa_saida-data.
    wa_ret_movimento_mensal_zconf-user1         = wa_saida-user.
    wa_ret_movimento_mensal_zconf-doc_contabil  = wa_saida-doc_contabil.
    wa_ret_movimento_mensal_zconf-ordem_doc     = wa_saida-ordem_doc.
    wa_ret_movimento_mensal_zconf-data_registro = wa_saida-data_registro.
    wa_ret_movimento_mensal_zconf-hora_registro = wa_saida-hora_registro.
    wa_ret_movimento_mensal_zconf-nftype        = wa_saida-nftype.
    wa_ret_movimento_mensal_zconf-model         = wa_saida-model.
    wa_ret_movimento_mensal_zconf-refkey        = wa_saida-refkey.
    wa_ret_movimento_mensal_zconf-maktx         = wa_saida-maktx.
    wa_ret_movimento_mensal_zconf-taxlw1        = wa_saida-taxlw1.
    wa_ret_movimento_mensal_zconf-taxlw2        = wa_saida-taxlw2.
    wa_ret_movimento_mensal_zconf-taxlw4        = wa_saida-taxlw4.
    wa_ret_movimento_mensal_zconf-taxlw5        = wa_saida-taxlw5.
    "
    wa_ret_movimento_mensal_zconf-taxlw4_xml    = wa_saida-taxlw4_xml.
    wa_ret_movimento_mensal_zconf-taxlw5_xml    = wa_saida-taxlw5_xml.
    "
    wa_ret_movimento_mensal_zconf-iva           = wa_saida-iva.
    wa_ret_movimento_mensal_zconf-charg         = wa_saida-charg.
    wa_ret_movimento_mensal_zconf-ncm           = wa_saida-ncm.
    wa_ret_movimento_mensal_zconf-chave_nfe     = wa_saida-chave_nfe.
    wa_ret_movimento_mensal_zconf-lei_icms      = wa_saida-lei_icms.
    wa_ret_movimento_mensal_zconf-lei_cofins    = wa_saida-lei_cofins.
    wa_ret_movimento_mensal_zconf-shtyp         = wa_saida-shtyp.
    wa_ret_movimento_mensal_zconf-anln1         = wa_saida-anln1.
    wa_ret_movimento_mensal_zconf-instrucao     = wa_saida-instrucao.
    wa_ret_movimento_mensal_zconf-ebeln         = wa_saida-ebeln.
    wa_ret_movimento_mensal_zconf-auart         = wa_saida-auart2.
    wa_ret_movimento_mensal_zconf-autyp         = wa_saida-autyp.
    wa_ret_movimento_mensal_zconf-knttp         = wa_saida-knttp.
    wa_ret_movimento_mensal_zconf-ddtext        = wa_saida-ddtext.
    wa_ret_movimento_mensal_zconf-kostl         = wa_saida-kostl.
    wa_ret_movimento_mensal_zconf-desc_conta_ctb         = wa_saida-desc_conta_ctb.
    wa_ret_movimento_mensal_zconf-sakto         = wa_saida-sakto.
    wa_ret_movimento_mensal_zconf-desc_centro_cust        = wa_saida-desc_centro_cust.

**  Begin of "CS2022000515 - FF  28.11.2022
    wa_ret_movimento_mensal_zconf-cod_icms = wa_saida-cod_icms.
    wa_ret_movimento_mensal_zconf-aufnr    = wa_saida-aufnr.
*    wa_ret_movimento_mensal_zconf-ktext    = wa_saida-ktext.
    wa_ret_movimento_mensal_zconf-anln2    = wa_saida-anln2.
    wa_ret_movimento_mensal_zconf-ebelp2   = wa_saida-ebelp2.
** End of "CS2022000515 - FF  28.11.2022

**  Begin of "CS2022000515 - FF  16.01.2023
    wa_ret_movimento_mensal_zconf-regio = wa_saida-regio.
    wa_ret_movimento_mensal_zconf-vornr = wa_saida-vornr.
    wa_ret_movimento_mensal_zconf-ltxa1 = wa_saida-ltxa1.
    wa_ret_movimento_mensal_zconf-recap = wa_saida-recap.
** End of "CS2022000515 - FF  16.01.2023

    wa_ret_movimento_mensal_zconf-placa_cav = wa_saida-placa_cav.
    wa_ret_movimento_mensal_zconf-itmnum    = wa_saida-itmnum.
    wa_ret_movimento_mensal_zconf-direct    = wa_saida-direct.
    wa_ret_movimento_mensal_zconf-ttypetxt    = wa_saida-ttypetxt.

    APPEND wa_ret_movimento_mensal_zconf TO it_ret_movimento_mensal_zconf.

    CLEAR : wa_ret_movimento_mensal_zconf , wa_saida.
  ENDLOOP.



ENDFORM.                    " F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gravar_dados USING  p_gravr  .
  DATA: v_anterior LIKE zdados_saida-docnum,
        wa_saida   TYPE ty_saida.

  TABLES : zdados_saida.

  IF p_gravr EQ 'X'.
    LOOP AT t_saida INTO wa_saida.

      IF wa_saida-docnum NE v_anterior.
        zdados_saida-sequencia = 1.
        v_anterior = wa_saida-docnum.
      ELSE.
        zdados_saida-sequencia = zdados_saida-sequencia + 1.
      ENDIF.

      zdados_saida-bukrs         = wa_saida-bukrs.
      zdados_saida-branch        = wa_saida-branch.
      zdados_saida-cfop          = wa_saida-cfop.
      zdados_saida-nfenum        = wa_saida-nfenum.
      zdados_saida-series        = wa_saida-series.
      zdados_saida-meins         = wa_saida-meins.
      zdados_saida-pstdat        = wa_saida-pstdat.
      zdados_saida-docdat        = wa_saida-docdat.
      zdados_saida-menge         = wa_saida-menge.
      zdados_saida-ntgew         = wa_saida-ntgew.
      zdados_saida-netwrt        = wa_saida-netwrt.
      zdados_saida-base_icms     = wa_saida-base_icms.
      zdados_saida-outros        = wa_saida-outros.
      zdados_saida-icms          = wa_saida-icms.
      zdados_saida-pis           = wa_saida-pis.
      zdados_saida-cofins        = wa_saida-cofins.
      zdados_saida-iss           = wa_saida-iss.
      zdados_saida-inss          = wa_saida-inss.
      zdados_saida-docnum        = wa_saida-docnum.
      zdados_saida-stcd3         = wa_saida-stcd3.
      zdados_saida-excbas        = wa_saida-excbas.
      zdados_saida-produto       = wa_saida-produto.
      zdados_saida-matkl         = wa_saida-matkl.
      zdados_saida-wgbez60       = wa_saida-wgbez60.
      zdados_saida-ordem         = wa_saida-ordem.
      zdados_saida-remessa       = wa_saida-remessa.
      zdados_saida-romaneio      = wa_saida-romaneio.
      zdados_saida-ref_ro        = wa_saida-ref_ro.
      zdados_saida-utilizacao    = wa_saida-utilizacao.
      zdados_saida-doc_fatura    = wa_saida-doc_fatura.
      zdados_saida-kursk         = wa_saida-kursk.
      zdados_saida-vlr_dolar     = wa_saida-vlr_dolar.
      zdados_saida-cpf_prod      = wa_saida-cpf_prod.
      zdados_saida-unit_dolar    = wa_saida-unit_dolar.
      zdados_saida-status        = wa_saida-status.
      zdados_saida-nf_status     = wa_saida-nf_status.
      zdados_saida-vlr_unit      = wa_saida-vlr_unit.
      zdados_saida-cod_clifor    = wa_saida-cod_clifor.
      zdados_saida-crt_bupla     = wa_saida-crt_bupla." (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC
      zdados_saida-nome_clifor   = wa_saida-nome_clifor.
      zdados_saida-uf_clifor     = wa_saida-uf_clifor.
      zdados_saida-ort01         = wa_saida-ort01.
      zdados_saida-stkzn         = wa_saida-stkzn.
      zdados_saida-parc_coleta   = wa_saida-parc_coleta.
      zdados_saida-cid_coleta    = wa_saida-cid_coleta.
      zdados_saida-uf_coleta     = wa_saida-uf_coleta.
      zdados_saida-local_ent     = wa_saida-local_ent.
      zdados_saida-uf_ent        = wa_saida-uf_ent.
      zdados_saida-terminal      = wa_saida-terminal.
      zdados_saida-uf_terminal   = wa_saida-uf_terminal.
      zdados_saida-nfe           = wa_saida-nfe.
      zdados_saida-data          = wa_saida-data.
      zdados_saida-user1         = wa_saida-user.
      zdados_saida-doc_contabil  = wa_saida-doc_contabil.
      zdados_saida-data_registro = wa_saida-data_registro.
      zdados_saida-hora_registro = wa_saida-hora_registro.
      zdados_saida-nftype        = wa_saida-nftype.
      zdados_saida-model         = wa_saida-model.
      zdados_saida-refkey        = wa_saida-refkey.
      zdados_saida-maktx         = wa_saida-maktx.
      zdados_saida-taxlw1        = wa_saida-taxlw1.
      zdados_saida-taxlw2        = wa_saida-taxlw2.
      zdados_saida-taxlw4        = wa_saida-taxlw4.
      zdados_saida-taxlw5        = wa_saida-taxlw5.
      "
      zdados_saida-taxlw4_xml    = wa_saida-taxlw4_xml.
      zdados_saida-taxlw5_xml    = wa_saida-taxlw5_xml.
      "
      zdados_saida-iva           = wa_saida-iva.
      zdados_saida-charg         = wa_saida-charg.
      zdados_saida-ncm           = wa_saida-ncm.
      zdados_saida-chave_nfe     = wa_saida-chave_nfe.
      zdados_saida-lei_icms      = wa_saida-lei_icms.
      zdados_saida-lei_cofins    = wa_saida-lei_cofins.
*      ZDADOS_SAIDA-SHTYP        = WA_SAIDA-SHTYP.
      zdados_saida-anln1         = wa_saida-anln1.

      zdados_saida-inco1         = wa_saida-inco1.
      zdados_saida-inco2         = wa_saida-inco2.

      zdados_saida-placa_cav     = wa_saida-placa_cav.
      zdados_saida-itmnum        = wa_saida-itmnum.
      zdados_saida-ttypetxt      = wa_saida-ttypetxt.

      INSERT zdados_saida.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " F_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_VINCULO_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_vinculo_entrada.

  DATA: t_zsdt0001_e_aux TYPE TABLE OF zsdt0001,
        t_zsdt0001_s_aux TYPE TABLE OF zsdt0001,
        vnr_romaneio     TYPE zsdt0001-nr_romaneio,
        tabix            TYPE sy-tabix.

  CHECK t_j_1bnfdoc_e[] IS NOT INITIAL.

  SELECT  *
    FROM zmmt_ee_zgr_docs
    INTO TABLE @DATA(t_ee_zgr_docs)
    FOR ALL ENTRIES IN @t_j_1bnfdoc_e
   WHERE docnum EQ @t_j_1bnfdoc_e-docnum.

  IF t_ee_zgr_docs[] IS NOT INITIAL.
    SELECT *
      FROM zmmt_ee_zgr INTO TABLE @DATA(t_ee_zgr)
      FOR ALL ENTRIES IN @t_ee_zgr_docs
     WHERE obj_key EQ @t_ee_zgr_docs-obj_key.

    IF t_ee_zgr[] IS NOT INITIAL.
      SELECT  *
        FROM zsdt0001 INTO TABLE @DATA(t_zsdt0001_e)
        FOR ALL ENTRIES IN @t_ee_zgr
       WHERE ch_referencia EQ @t_ee_zgr-ch_referencia.
    ENDIF.
  ENDIF.

  LOOP AT t_j_1bnfdoc_e INTO DATA(w_j_1bnfdoc_e).

    IF w_j_1bnfdoc_e-nfenum IS NOT INITIAL.
      w_j_1bnfdoc_e-nfnum2  = w_j_1bnfdoc_e-nfenum.
    ELSEIF w_j_1bnfdoc_e-nfnum IS NOT INITIAL.
      w_j_1bnfdoc_e-nfnum2  = w_j_1bnfdoc_e-nfnum.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_j_1bnfdoc_e-nfnum2
      IMPORTING
        output = w_j_1bnfdoc_e-nfnum2.
    "
    MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX sy-tabix TRANSPORTING nfnum2.
  ENDLOOP.

  SELECT  *
    FROM zsdt0001 APPENDING TABLE t_zsdt0001_e
    FOR ALL ENTRIES IN t_j_1bnfdoc_e
   WHERE bukrs        EQ t_j_1bnfdoc_e-bukrs
     AND branch       EQ t_j_1bnfdoc_e-branch
     AND parid        EQ t_j_1bnfdoc_e-parid
     AND docdat       EQ t_j_1bnfdoc_e-docdat
     AND nfnum        EQ t_j_1bnfdoc_e-nfnum2
     AND tp_movimento EQ 'E'.

  CHECK t_zsdt0001_e[] IS NOT INITIAL.

  "Pesquisa romaneio SEM zeros a esquerda
  LOOP AT t_zsdt0001_e INTO DATA(wl_zsdt0001_e).
    wl_zsdt0001_e-id_referencia = wl_zsdt0001_e-nr_romaneio.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wl_zsdt0001_e-id_referencia
      IMPORTING
        output = wl_zsdt0001_e-id_referencia.
    MODIFY t_zsdt0001_e FROM wl_zsdt0001_e INDEX sy-tabix TRANSPORTING id_referencia.
  ENDLOOP.

  SELECT *
    FROM zsdt0001 INTO TABLE @DATA(t_zsdt0001_s)
    FOR ALL ENTRIES IN @t_zsdt0001_e
   WHERE bukrs          EQ @t_zsdt0001_e-bukrs
     AND branch         EQ @t_zsdt0001_e-branch
     AND id_referencia  EQ @t_zsdt0001_e-id_referencia
     AND tp_movimento   EQ 'S'
     AND nr_safra       EQ @t_zsdt0001_e-nr_safra.
  "
  "Pesquisa romaneio COM zeros a esquerda
  LOOP AT t_zsdt0001_e INTO wl_zsdt0001_e.
    wl_zsdt0001_e-id_referencia = wl_zsdt0001_e-nr_romaneio.

    MODIFY t_zsdt0001_e FROM wl_zsdt0001_e INDEX sy-tabix TRANSPORTING id_referencia.
  ENDLOOP.

  SELECT *
    FROM zsdt0001 APPENDING TABLE t_zsdt0001_s
    FOR ALL ENTRIES IN t_zsdt0001_e
   WHERE bukrs          EQ t_zsdt0001_e-bukrs
     AND branch         EQ t_zsdt0001_e-branch
     AND id_referencia  EQ t_zsdt0001_e-id_referencia
     AND tp_movimento   EQ 'S'
     AND nr_safra       EQ t_zsdt0001_e-nr_safra.


  "pesquisa pelo ID_CARGA
  SELECT *
    FROM zsdt0001 APPENDING TABLE t_zsdt0001_s
    FOR ALL ENTRIES IN t_zsdt0001_e
   WHERE bukrs          EQ t_zsdt0001_e-bukrs
     AND branch         EQ t_zsdt0001_e-branch
     AND id_carga       EQ t_zsdt0001_e-id_carga
     AND id_carga       NE ' '
     AND tp_movimento   EQ 'S'
     AND nr_safra       EQ t_zsdt0001_e-nr_safra.

  "Volta sem Zeros a esquerda
  LOOP AT t_zsdt0001_e INTO wl_zsdt0001_e.
    wl_zsdt0001_e-id_referencia = wl_zsdt0001_e-nr_romaneio.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wl_zsdt0001_e-id_referencia
      IMPORTING
        output = wl_zsdt0001_e-id_referencia.
    MODIFY t_zsdt0001_e FROM wl_zsdt0001_e INDEX sy-tabix TRANSPORTING id_referencia.
  ENDLOOP.

  t_zsdt0001_e_aux[] = t_zsdt0001_e[].
  t_zsdt0001_s_aux[] = t_zsdt0001_s[].

  SORT:  t_ee_zgr_docs     BY docnum,
         t_ee_zgr          BY obj_key,
         t_zsdt0001_e      BY ch_referencia, "INTERFACE GRAOS
         t_zsdt0001_e_aux  BY bukrs branch parid docdat nfnum, "Romaneio
         t_zsdt0001_s      BY bukrs branch id_referencia nr_safra,
         t_zsdt0001_s_aux  BY bukrs branch id_carga nr_safra.

  LOOP AT t_j_1bnfdoc_e INTO w_j_1bnfdoc_e.
    tabix = sy-tabix.
    "busca 1
    READ TABLE t_ee_zgr_docs INTO DATA(w_ee_zgr_docs) WITH KEY docnum = w_j_1bnfdoc_e-docnum BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE t_ee_zgr INTO DATA(w_t_ee_zgr) WITH KEY obj_key = w_ee_zgr_docs-obj_key BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE t_zsdt0001_e INTO wl_zsdt0001_e WITH KEY ch_referencia = w_t_ee_zgr-ch_referencia BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE t_zsdt0001_s INTO DATA(w_zsdt0001_s) WITH KEY bukrs          = wl_zsdt0001_e-bukrs
                                                                   branch         = wl_zsdt0001_e-branch
                                                                   id_referencia  = wl_zsdt0001_e-id_referencia
                                                                   nr_safra       = wl_zsdt0001_e-nr_safra BINARY SEARCH.
          IF sy-subrc = 0.
            w_j_1bnfdoc_e-vbeln    = w_zsdt0001_s-vbeln.
            w_j_1bnfdoc_e-docnum_s = w_zsdt0001_s-nro_nf_prod.
            MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX tabix TRANSPORTING vbeln docnum_s .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "Busca 2
    IF w_j_1bnfdoc_e-vbeln IS INITIAL.
      READ TABLE t_zsdt0001_e_aux INTO DATA(w_zsdt0001_e_aux) WITH KEY bukrs  = w_j_1bnfdoc_e-bukrs
                                                                       branch = w_j_1bnfdoc_e-branch
                                                                       parid  = w_j_1bnfdoc_e-parid
                                                                       docdat = w_j_1bnfdoc_e-docdat
                                                                       nfnum  = w_j_1bnfdoc_e-nfnum2 BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE t_zsdt0001_s INTO w_zsdt0001_s WITH KEY bukrs          = w_zsdt0001_e_aux-bukrs
                                                           branch         = w_zsdt0001_e_aux-branch
                                                           id_referencia  = w_zsdt0001_e_aux-id_referencia
                                                           nr_safra       = w_zsdt0001_e_aux-nr_safra BINARY SEARCH.
        IF sy-subrc = 0.
          w_j_1bnfdoc_e-vbeln = w_zsdt0001_s-vbeln.
          w_j_1bnfdoc_e-docnum_s = w_zsdt0001_s-nro_nf_prod.
          MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX tabix TRANSPORTING vbeln docnum_s.
        ELSE.
          w_zsdt0001_e_aux-id_referencia = w_zsdt0001_e_aux-nr_romaneio.
          READ TABLE t_zsdt0001_s INTO w_zsdt0001_s WITH KEY bukrs          = w_zsdt0001_e_aux-bukrs
                                                             branch         = w_zsdt0001_e_aux-branch
                                                             id_referencia  = w_zsdt0001_e_aux-id_referencia
                                                             nr_safra       = w_zsdt0001_e_aux-nr_safra BINARY SEARCH.
          IF sy-subrc = 0.
            w_j_1bnfdoc_e-vbeln    = w_zsdt0001_s-vbeln.
            w_j_1bnfdoc_e-docnum_s = w_zsdt0001_s-nro_nf_prod.
            MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX tabix TRANSPORTING vbeln docnum_s.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "Busca 3
    IF w_j_1bnfdoc_e-vbeln IS INITIAL.
      READ TABLE t_zsdt0001_e_aux INTO w_zsdt0001_e_aux WITH KEY  bukrs  = w_j_1bnfdoc_e-bukrs
                                                                  branch = w_j_1bnfdoc_e-branch
                                                                  parid  = w_j_1bnfdoc_e-parid
                                                                  docdat = w_j_1bnfdoc_e-docdat
                                                                  nfnum  = w_j_1bnfdoc_e-nfnum2 BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE t_zsdt0001_s_aux INTO w_zsdt0001_s WITH KEY bukrs      = w_zsdt0001_e_aux-bukrs
                                                               branch     = w_zsdt0001_e_aux-branch
                                                               id_carga   = w_zsdt0001_e_aux-id_carga
                                                               nr_safra   = w_zsdt0001_e_aux-nr_safra BINARY SEARCH.
        IF sy-subrc = 0.
          w_j_1bnfdoc_e-vbeln = w_zsdt0001_s-vbeln.
          w_j_1bnfdoc_e-docnum_s = w_zsdt0001_s-nro_nf_prod.
          MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX tabix TRANSPORTING vbeln docnum_s.
        ENDIF.
      ENDIF.
    ENDIF.



  ENDLOOP.

  DELETE t_j_1bnfdoc_e  WHERE vbeln IS INITIAL.

  IF t_j_1bnfdoc_e[] IS NOT INITIAL.
    SELECT docnum parid partyp parvw
        FROM j_1bnfnad
        APPENDING TABLE t_parceiro
        FOR ALL ENTRIES IN t_j_1bnfdoc_e
          WHERE docnum EQ t_j_1bnfdoc_e-docnum_s
          AND   parvw  EQ 'PC'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZSELECIONA_DADOS_ZCONF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ATIVAS  text
*      -->P_NATIVA  text
*      -->P_AUTOR  text
*      -->P_REJEIT  text
*      -->P_RECUS  text
*      -->P_CANCEL  text
*      -->P_AGRES  text
*      -->P_NENV  text
*      -->P_gravr  text
*----------------------------------------------------------------------*
FORM zseleciona_dados_zconf_prd  USING    p_ativas
                                      p_nativa
                                      p_autor
                                      p_rejeit
                                      p_recus
                                      p_cancel
                                      p_agres
                                      p_nenv
                                      p_gravr.


  DATA : t_j_1bnfstx_aux TYPE TABLE OF j_1bnfstx,
         t_j_1bnflin_aux TYPE TABLE OF j_1bnflin,
         t_vbpa_aux      TYPE TABLE OF vbpa,
         t_remessa_aux   TYPE TABLE OF ty_remessa,
         vg_tabix        TYPE          sy-tabix.


  LOOP AT it_movimento_mensal_zconf INTO wa_movimento_mensal_zconf.

    IF wa_movimento_mensal_zconf-campo EQ 'bukrs'.
      p_bukrs-low    = wa_movimento_mensal_zconf-valor_de.
      p_bukrs-high   = wa_movimento_mensal_zconf-valor_ate.
      p_bukrs-sign   = wa_movimento_mensal_zconf-sign.
      p_bukrs-option = wa_movimento_mensal_zconf-option.
      APPEND p_bukrs.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'branch'.
      p_branch-low    = wa_movimento_mensal_zconf-valor_de.
      p_branch-high   = wa_movimento_mensal_zconf-valor_ate.
      p_branch-sign   = wa_movimento_mensal_zconf-sign.
      p_branch-option = wa_movimento_mensal_zconf-option.
      APPEND p_branch.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'direct'.
      p_direct-low    = wa_movimento_mensal_zconf-valor_de.
      p_direct-high   = wa_movimento_mensal_zconf-valor_ate.
      p_direct-sign   = wa_movimento_mensal_zconf-sign.
      p_direct-option = wa_movimento_mensal_zconf-option.
      APPEND p_direct.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'pstdat'.
      p_pstdat-low    = wa_movimento_mensal_zconf-valor_de.
      p_pstdat-high   = wa_movimento_mensal_zconf-valor_ate.
      p_pstdat-sign   = wa_movimento_mensal_zconf-sign.
      p_pstdat-option = wa_movimento_mensal_zconf-option.
      APPEND p_pstdat.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'docdat'.
      p_docdat-low    = wa_movimento_mensal_zconf-valor_de.
      p_docdat-high   = wa_movimento_mensal_zconf-valor_ate.
      p_docdat-sign   = wa_movimento_mensal_zconf-sign.
      p_docdat-option = wa_movimento_mensal_zconf-option.
      APPEND p_docdat.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ 'tmiss'.
      p_tmiss-low    = wa_movimento_mensal_zconf-valor_de.
      p_tmiss-high   = wa_movimento_mensal_zconf-valor_ate.
      p_tmiss-sign   = wa_movimento_mensal_zconf-sign.
      p_tmiss-option = wa_movimento_mensal_zconf-option.
      APPEND p_tmiss.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'tmpro'.
      p_tmiss-low    = wa_movimento_mensal_zconf-valor_de.
      p_tmiss-high   = wa_movimento_mensal_zconf-valor_ate.
      p_tmiss-sign   = wa_movimento_mensal_zconf-sign.
      p_tmiss-option = wa_movimento_mensal_zconf-option.
      APPEND p_tmpro.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'tmptod'.
      p_tmiss-low    = wa_movimento_mensal_zconf-valor_de.
      p_tmiss-high   = wa_movimento_mensal_zconf-valor_ate.
      p_tmiss-sign   = wa_movimento_mensal_zconf-sign.
      p_tmiss-option = wa_movimento_mensal_zconf-option.
      APPEND p_tmptod.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'nordem'.
      p_nordem-low    = wa_movimento_mensal_zconf-valor_de.
      p_nordem-high   = wa_movimento_mensal_zconf-valor_ate.
      p_nordem-sign   = wa_movimento_mensal_zconf-sign.
      p_nordem-option = wa_movimento_mensal_zconf-option.
      APPEND  p_nordem.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'parid'.
      p_parid-low    = wa_movimento_mensal_zconf-valor_de.
      p_parid-high   = wa_movimento_mensal_zconf-valor_ate.
      p_parid-sign   = wa_movimento_mensal_zconf-sign.
      p_parid-option = wa_movimento_mensal_zconf-option.
      APPEND p_parid.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'docnum'.
      p_docnum-low    = wa_movimento_mensal_zconf-valor_de.
      p_docnum-high   = wa_movimento_mensal_zconf-valor_ate.
      p_docnum-sign   = wa_movimento_mensal_zconf-sign.
      p_docnum-option = wa_movimento_mensal_zconf-option.
      APPEND p_docnum.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ  'nfenum'.
      p_nfenum-low    = wa_movimento_mensal_zconf-valor_de.
      p_nfenum-high   = wa_movimento_mensal_zconf-valor_ate.
      p_nfenum-sign   = wa_movimento_mensal_zconf-sign.
      p_nfenum-option = wa_movimento_mensal_zconf-option.
      APPEND  p_nfenum.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ  'cfop'.
      p_cfop-low    = wa_movimento_mensal_zconf-valor_de.
      p_cfop-high   = wa_movimento_mensal_zconf-valor_ate.
      p_cfop-sign   = wa_movimento_mensal_zconf-sign.
      p_cfop-option = wa_movimento_mensal_zconf-option.
      APPEND  p_cfop.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ 'model'.
      p_model-low    = wa_movimento_mensal_zconf-valor_de.
      p_model-high   = wa_movimento_mensal_zconf-valor_ate.
      p_model-sign   = wa_movimento_mensal_zconf-sign.
      p_model-option = wa_movimento_mensal_zconf-option.
      APPEND p_model.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ 'crenam'.
      p_crenam-low    = wa_movimento_mensal_zconf-valor_de.
      p_crenam-high   = wa_movimento_mensal_zconf-valor_ate.
      p_crenam-sign   = wa_movimento_mensal_zconf-sign.
      p_crenam-option = wa_movimento_mensal_zconf-option.
      APPEND p_crenam.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'nfe'.
      p_nfe-low    = wa_movimento_mensal_zconf-valor_de.
      p_nfe-high   = wa_movimento_mensal_zconf-valor_ate.
      p_nfe-sign   = wa_movimento_mensal_zconf-sign.
      p_nfe-option = wa_movimento_mensal_zconf-option.
      APPEND p_nfe.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'chave'.
      "Chave de Acesso / 51 24 03 84590892000622 57000000742035110187282 2
      APPEND  VALUE #(
        regio   = wa_movimento_mensal_zconf-valor_de+0(2)
        nfyear  = wa_movimento_mensal_zconf-valor_de+2(2)
        nfmonth = wa_movimento_mensal_zconf-valor_de+4(2)
        stcd1   = wa_movimento_mensal_zconf-valor_de+6(14)
        model   = wa_movimento_mensal_zconf-valor_de+20(2)
        serie   = wa_movimento_mensal_zconf-valor_de+22(3)
        nfnum9  = wa_movimento_mensal_zconf-valor_de+25(9)
        docnum9 = wa_movimento_mensal_zconf-valor_de+34(9)
        cdv     = wa_movimento_mensal_zconf-valor_de+43(1)
       ) TO it_nfe.
    ENDIF.


    IF wa_movimento_mensal_zconf-campo EQ 'nfnum'.
      p_nfnum-low    = wa_movimento_mensal_zconf-valor_de.
      p_nfnum-high   = wa_movimento_mensal_zconf-valor_ate.
      p_nfnum-sign   = wa_movimento_mensal_zconf-sign.
      p_nfnum-option = wa_movimento_mensal_zconf-option.
      APPEND p_nfnum.
    ENDIF.

    IF wa_movimento_mensal_zconf-campo EQ 'mat'.
      p_mat-low    = wa_movimento_mensal_zconf-valor_de.
      p_mat-high   = wa_movimento_mensal_zconf-valor_ate.
      p_mat-sign   = wa_movimento_mensal_zconf-sign.
      p_mat-option = wa_movimento_mensal_zconf-option.
      APPEND p_mat.
    ENDIF.


  ENDLOOP.

  IF NOT p_nordem IS INITIAL AND  1 IN p_direct[].
    MESSAGE i000(z01) WITH 'O n° da Ordem de venda pode se informado '
                           'somente para saída' .
    STOP.
  ENDIF.

  IF p_ativas NE 'X' AND  p_nativa NE 'X' AND p_autor  NE 'X' AND p_rejeit NE 'X' AND p_recus  NE 'X' AND
     p_cancel NE 'X' AND  p_agres  NE 'X' AND p_nenv   NE 'X' .

    MESSAGE i000(z01) WITH 'Selecione pelo menos uma opção de Status da NF !' .
    STOP.
  ENDIF.

*SELECT REGIO || NFYEAR || NFMONTH || STCD1 || MODEL || SERIE || NFNUM9 || DOCNUM9 || CDV  AS CHAVE
*   FROM SAPSR3.J_1BNFE_ACTIVE@SAPECC a
*  WHERE MANDT = '300'
*    AND DOCNUM = '0012352066';
  IF it_nfe IS INITIAL.
    SELECT *
      FROM j_1bnfdoc
      INTO TABLE t_j_1bnfdoc
      WHERE bukrs  IN p_bukrs
        AND branch IN p_branch
        AND direct IN p_direct
        AND model  IN p_model
        AND docnum IN p_docnum
        AND crenam IN p_crenam
        AND nfenum IN p_nfenum
        AND parid  IN p_parid
        AND nfe    IN p_nfe
        AND nfnum  IN p_nfnum
        AND pstdat IN p_pstdat
        AND docdat IN p_docdat
        AND doctyp NE '5'. "Estorno
  ELSE.
    "j_1bnfe_active
    FREE: t_j_1bnfe_active.
    SELECT docnum docsta cancel scssta regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
   FROM j_1bnfe_active
   INTO TABLE t_j_1bnfe_active
   FOR ALL ENTRIES IN it_nfe
   WHERE    regio   EQ it_nfe-regio
      AND   nfyear  EQ it_nfe-nfyear
      AND   nfmonth EQ it_nfe-nfmonth
      AND   stcd1   EQ it_nfe-stcd1
      AND   model   EQ it_nfe-model
      AND   serie   EQ it_nfe-serie
      AND   nfnum9  EQ it_nfe-nfnum9.
*      AND   docnum9 EQ it_nfe-docnum9
*      AND   cdv     EQ it_nfe-cdv.
    IF t_j_1bnfe_active IS NOT INITIAL.

      SELECT *
    FROM j_1bnfdoc
    INTO TABLE t_j_1bnfdoc
        FOR ALL ENTRIES IN t_j_1bnfe_active
    WHERE docnum EQ t_j_1bnfe_active-docnum
      AND bukrs  IN p_bukrs
      AND branch IN p_branch
      AND direct IN p_direct
      AND model  IN p_model
      AND docnum IN p_docnum
      AND crenam IN p_crenam
      AND nfenum IN p_nfenum
      AND parid  IN p_parid
      AND nfe    IN p_nfe
      AND nfnum  IN p_nfnum
      AND pstdat IN p_pstdat
      AND docdat IN p_docdat
      AND doctyp NE '5'. "Estorno
    ENDIF.

    FREE: t_j_1bnfe_active.
  ENDIF.


  IF t_j_1bnfdoc IS NOT INITIAL.
    "------Parceiros da Nota--------
    SELECT docnum parid partyp parvw
      FROM j_1bnfnad
      INTO TABLE t_parceiro
      FOR ALL ENTRIES IN t_j_1bnfdoc
        WHERE docnum EQ t_j_1bnfdoc-docnum.

    "------Status nfe ---------
    SELECT docnum docsta cancel scssta regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
      FROM j_1bnfe_active
      INTO TABLE t_j_1bnfe_active
      FOR ALL ENTRIES IN t_j_1bnfdoc
      WHERE docnum EQ t_j_1bnfdoc-docnum.


    "--------Itens -------------
    CLEAR v_tmiss.
    IMPORT p1 = e_ov FROM MEMORY ID 'MZCONFX'.

    IF p_tmiss[] IS NOT INITIAL .
      "      V_TMISS = 'X'.
      SELECT *
        FROM j_1bnflin
        INTO TABLE t_j_1bnflin
        FOR ALL ENTRIES IN t_j_1bnfdoc
        WHERE docnum EQ t_j_1bnfdoc-docnum
          AND cfop   IN p_cfop
          AND matnr  IN p_mat
          AND tmiss  EQ 'X'.

    ELSEIF NOT ( p_tmpro[] IS INITIAL ).

      SELECT *
        FROM j_1bnflin
        INTO TABLE t_j_1bnflin
        FOR ALL ENTRIES IN t_j_1bnfdoc
        WHERE docnum EQ t_j_1bnfdoc-docnum
          AND cfop   IN p_cfop
          AND matnr  IN p_mat
          AND tmiss  NE 'X'.
    ELSE.

      SELECT *
        FROM j_1bnflin
        INTO TABLE t_j_1bnflin
        FOR ALL ENTRIES IN t_j_1bnfdoc
        WHERE docnum EQ t_j_1bnfdoc-docnum
          AND cfop   IN p_cfop
          AND matnr  IN p_mat.
    ENDIF.



    IF sy-subrc IS INITIAL.
      IF e_ov = 'X'.
        LOOP AT t_j_1bnfdoc INTO DATA(w_doc).
          IF w_doc-direct = '1'.
            MOVE-CORRESPONDING w_doc TO w_j_1bnfdoc_e.
            APPEND w_j_1bnfdoc_e TO t_j_1bnfdoc_e.
          ENDIF.
        ENDLOOP.
        PERFORM f_busca_vinculo_entrada_prd.
      ENDIF.

      IF p_nordem[] IS NOT INITIAL.

        SELECT *
          FROM vbfa
          INTO TABLE t_vbfa_ord
          FOR ALL ENTRIES IN t_j_1bnflin
          WHERE vbeln EQ  t_j_1bnflin-refkey(10)
          AND vbtyp_n NE 'N'
          AND vbtyp_v NE 'M'.

        DELETE t_vbfa_ord WHERE vbelv NOT IN p_nordem.

        IF  t_vbfa_ord IS NOT INITIAL.

          SORT: t_j_1bnflin BY refkey.

          LOOP AT t_vbfa_ord INTO wa_vbfa_ord.

            READ TABLE t_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey  = wa_vbfa_ord-vbeln BINARY SEARCH.
            IF sy-subrc IS  INITIAL.
              APPEND VALUE #( docnum = wa_j_1bnflin-docnum ) TO t_vbfa_doc.
**              wa_vbfa_doc =  wa_j_1bnflin-docnum.
**              APPEND wa_vbfa_doc TO t_vbfa_doc.
            ENDIF.

          ENDLOOP.

          IF t_vbfa_doc IS NOT INITIAL.

            SORT: t_vbfa_doc BY docnum.

            LOOP AT t_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_j_1bnfdoc>).
              vg_tabix = sy-tabix.
              READ TABLE t_vbfa_doc INTO wa_vbfa_doc WITH KEY docnum = <fs_j_1bnfdoc>-docnum BINARY SEARCH.
              IF sy-subrc IS NOT INITIAL.
                CLEAR: <fs_j_1bnfdoc>-docnum.
**                MODIFY t_j_1bnfdoc INDEX vg_tabix FROM wa_j_1bnfdoc TRANSPORTING docnum.
              ENDIF.
            ENDLOOP.

            DELETE t_j_1bnfdoc WHERE docnum EQ space.

          ENDIF.
          "ELSE.
          "MESSAGE i000(z01) WITH 'O n° da Ordem de venda não encontrado '.
          "EXIT.
        ENDIF.

        CLEAR: wa_j_1bnfdoc,
               wa_vbfa_ord,
               wa_vbfa_doc,
               wa_j_1bnflin.



      ENDIF.


      "------Imposto-------------
      SELECT *
        FROM j_1bnfstx
        INTO TABLE t_j_1bnfstx
        FOR ALL ENTRIES IN t_j_1bnflin
        WHERE docnum EQ t_j_1bnflin-docnum
          AND itmnum EQ t_j_1bnflin-itmnum.

      t_j_1bnfstx_aux[] = t_j_1bnfstx[].

      SORT t_j_1bnfstx_aux BY taxtyp.

      DELETE ADJACENT DUPLICATES FROM t_j_1bnfstx_aux COMPARING taxtyp.

      "-----Tipo de Imposto-------
      IF t_j_1bnfstx_aux IS NOT INITIAL.

        SELECT taxtyp taxgrp
          FROM j_1baj
          INTO TABLE t_j_1baj
          FOR ALL ENTRIES IN t_j_1bnfstx_aux
           WHERE taxtyp EQ t_j_1bnfstx_aux-taxtyp.
        IF sy-subrc EQ 0.
          SELECT *
            FROM j_1bajt
            INTO CORRESPONDING FIELDS OF TABLE t_j_1bajt
            FOR ALL ENTRIES IN t_j_1baj
             WHERE taxtyp EQ t_j_1baj-taxtyp
            AND   spras  EQ sy-langu.
        ENDIF.

      ENDIF.

      t_j_1bnflin_aux[] = t_j_1bnflin[].

      SORT t_j_1bnflin_aux BY matnr.

      DELETE ADJACENT DUPLICATES FROM t_j_1bnflin_aux COMPARING matnr.

      "-------Produto------------
      SELECT *
        FROM makt
        INTO TABLE t_makt
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE matnr EQ t_j_1bnflin_aux-matnr.

      "-------Dados Gerais de material------------
      SELECT *
        FROM mara
        INTO TABLE t_mara
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE matnr EQ t_j_1bnflin_aux-matnr.

      SORT t_mara BY matnr.
      DELETE ADJACENT DUPLICATES FROM t_mara COMPARING matnr.

      IF t_mara[] IS NOT INITIAL.

        SELECT *
         FROM t023t
         INTO TABLE t_t023t
         FOR ALL ENTRIES IN t_mara
         WHERE matkl EQ t_mara-matkl
           AND spras = sy-langu.

      ENDIF.

      t_j_1bnflin_aux[] = t_j_1bnflin[].

      SORT t_j_1bnflin_aux BY refkey.

      DELETE ADJACENT DUPLICATES FROM t_j_1bnflin_aux COMPARING refkey.

      "-----Documento de faturamento-----
      SELECT vbeln kursk
        FROM vbrp
        INTO TABLE t_vbrp
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE vbeln EQ  t_j_1bnflin_aux-refkey(10).

      "------Parceiro da Fatura
      SELECT *
        FROM vbpa
        INTO TABLE t_vbpa
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE vbeln EQ t_j_1bnflin_aux-refkey(10)
          AND parvw EQ 'Z1'.

      t_vbpa_aux[] = t_vbpa[].

      SORT t_vbpa_aux BY lifnr.

      DELETE ADJACENT DUPLICATES FROM t_vbpa_aux COMPARING lifnr.

      IF t_vbpa_aux IS NOT INITIAL.
        SELECT lifnr, name1, regio
          FROM lfa1
          INTO TABLE @t_lfa1
          FOR ALL ENTRIES IN @t_vbpa_aux
          WHERE lifnr EQ @t_vbpa_aux-lifnr.
      ENDIF.

      "-------Remessa-----------

      SELECT v~vbeln re~ch_referencia l~vbeln re~nr_romaneio l~vgbel
        FROM vbfa AS v
        INNER JOIN lips AS l ON l~vbeln = v~vbelv
        LEFT JOIN zsdt0001 AS re ON re~doc_rem = l~vbeln AND tp_movimento EQ 'S'
        INTO TABLE t_remessa
        FOR ALL ENTRIES IN t_j_1bnflin_aux
        WHERE v~vbeln   EQ t_j_1bnflin_aux-refkey(10)
          AND v~vbtyp_v EQ 'J'.



      SELECT * FROM zsdt0053 INTO TABLE it_zsdt0053
        FOR ALL ENTRIES IN t_remessa
           WHERE vbeln EQ t_remessa-vgbel.

*      SELECT V~VBELN V~VBELV
*         FROM VBFA AS V
*         INTO TABLE T_REMESSA2
*         FOR ALL ENTRIES IN T_J_1BNFLIN_AUX
*         WHERE V~VBELN   EQ T_J_1BNFLIN_AUX-REFKEY(10)
*           AND V~VBTYP_V EQ 'C'
*           AND V~VBTYP_N IN ('M','O').
*
      IF ( t_remessa[] IS NOT INITIAL ).
*        LOOP AT T_REMESSA2 INTO WA_REMESSA2.
*          CLEAR WA_REMESSA.
*
*          WA_REMESSA-REFKEY = WA_REMESSA2-REFKEY.
*          WA_REMESSA-VGBEL  = WA_REMESSA2-VGBEL.
*
*          APPEND WA_REMESSA TO T_REMESSA.
*        ENDLOOP.

        "-------Remessa-----------
        t_remessa_aux[] = t_remessa[].

        SORT t_remessa_aux BY vgbel.

        DELETE ADJACENT DUPLICATES FROM t_remessa_aux COMPARING vgbel.

        "--------Utilização-----
        SELECT vbeln bezei
          FROM vbap AS v
          INNER JOIN tvlvt AS t ON t~abrvw  EQ v~vkaus
          INTO TABLE t_vbap
          FOR ALL ENTRIES IN t_remessa_aux
          WHERE vbeln EQ t_remessa_aux-vgbel.

*        delete t_vbak where: vkorg      ne p_vkorg,
*                       j_1bbranch not in p_werks,
*                       auart      not in p_auart,
*                       vbeln      not in p_vbeln,
*                       kunnr      not in p_parid.

        t_remessa_aux[] = t_remessa[].

        DELETE t_remessa_aux WHERE: refkey EQ ''.

        SORT t_remessa_aux BY refkey.


        LOOP AT t_remessa_aux INTO wa_remessa.

          CONCATENATE wa_remessa-refkey '          ' INTO wa_remessa_3-refkey .
*          call function 'CONVERSION_EXIT_INVDT_INPUT'
*          exporting
*            input  = WA_REMESSA-refkey
*          importing
*            output = WA_REMESSA_3-refkey .

          wa_remessa_3-vbeln  = wa_remessa-refkey.

          APPEND wa_remessa_3 TO t_remessa_3.

        ENDLOOP.

        DELETE ADJACENT DUPLICATES FROM t_remessa_aux COMPARING refkey.

        IF t_remessa_3[] IS NOT INITIAL.
          SELECT bukrs gjahr awkey belnr
            FROM bkpf
            INTO TABLE t_bkpf
            FOR ALL ENTRIES IN t_remessa_3
            WHERE awkey = t_remessa_3-refkey .
        ENDIF.


      ELSE.

        LOOP AT t_vbrp INTO wa_vbrp.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_vbrp-vbeln
            IMPORTING
              output = wa_vbrp_aux-awkey.


          APPEND wa_vbrp_aux TO t_vbrp_aux.

        ENDLOOP.

        "CSB - Não achou remessa para frete rodoviário
        IF t_vbrp_aux[] IS NOT INITIAL.
          SELECT bukrs gjahr awkey belnr
              FROM bkpf
              INTO TABLE t_bkpf
              FOR ALL ENTRIES IN t_vbrp_aux
              WHERE awkey = t_vbrp_aux-awkey.
        ENDIF.

      ENDIF.

      "-------Info de Ordem CS201-----------

      SELECT *
        FROM ekbe
        INTO TABLE t_ekbe
        FOR ALL ENTRIES IN t_j_1bnflin
        WHERE belnr EQ t_j_1bnflin-refkey(10)
          AND buzei EQ t_j_1bnflin-refitm+2(4).

      IF t_ekbe[] IS NOT INITIAL.

        SELECT *
          FROM ekpo
          INTO TABLE t_ekpo
          FOR ALL ENTRIES IN t_ekbe
          WHERE ebeln EQ t_ekbe-ebeln
            AND ebelp EQ t_ekbe-ebelp.

        SELECT *
          FROM ekkn
          INTO TABLE t_ekkn
          FOR ALL ENTRIES IN t_ekbe
          WHERE ebeln EQ t_ekbe-ebeln
            AND ebelp EQ t_ekbe-ebelp.

        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'AUFTYP'
            text           = 'X'
            langu          = sy-langu
          TABLES
            dd07v_tab      = t_dd07v
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.

        IF t_ekkn[] IS NOT INITIAL.

          SELECT *
            FROM skat
            INTO TABLE t_skat
            FOR ALL ENTRIES IN t_ekkn
            WHERE saknr EQ t_ekkn-sakto
                  AND ktopl EQ '0050'
                  AND spras EQ 'P'.

          SELECT *
            FROM tka02
            INTO TABLE t_tka02
            WHERE bukrs IN p_bukrs.

          SELECT *
            FROM cskt
            INTO TABLE t_cskt
            FOR ALL ENTRIES IN  t_tka02
            WHERE kokrs EQ t_tka02-kokrs.

          SELECT *
            FROM aufk
            INTO TABLE t_aufk
            FOR ALL ENTRIES IN t_ekkn
            WHERE aufnr EQ t_ekkn-aufnr.

        ENDIF.

      ENDIF.

    ENDIF.

    FREE MEMORY ID 'MZCONFX'.


*    IF T_BKPF IS NOT INITIAL.
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 SHKZG
*        FROM BSID
*        INTO TABLE T_BSID
*        FOR ALL ENTRIES IN T_BKPF
*        WHERE BUKRS EQ T_BKPF-BUKRS
*        AND   BELNR EQ T_BKPF-BELNR
*        AND   GJAHR EQ T_BKPF-GJAHR.
*
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 SHKZG
*        FROM BSAD
*        INTO TABLE T_BSAD
*        FOR ALL ENTRIES IN T_BKPF
*        WHERE BUKRS EQ T_BKPF-BUKRS
*        AND   BELNR EQ T_BKPF-BELNR
*        AND   GJAHR EQ T_BKPF-GJAHR.
*
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 SHKZG
*      FROM BSIK
*      INTO TABLE T_BSIK
*      FOR ALL ENTRIES IN T_BKPF
*      WHERE BUKRS EQ T_BKPF-BUKRS
*      AND   BELNR EQ T_BKPF-BELNR
*      AND   GJAHR EQ T_BKPF-GJAHR.
*
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 SHKZG
*        FROM BSAK
*        INTO TABLE T_BSAK
*        FOR ALL ENTRIES IN T_BKPF
*        WHERE BUKRS EQ T_BKPF-BUKRS
*        AND   BELNR EQ T_BKPF-BELNR
*        AND   GJAHR EQ T_BKPF-GJAHR.
*    ENDIF.

    CLEAR: it_zfiwrt0008[].

    DATA(t_j_1bnflin_zw) = t_j_1bnflin[].
    DELETE t_j_1bnflin_zw WHERE reftyp NE 'ZW'.
    SORT t_j_1bnflin_zw BY docnum.
    DELETE ADJACENT DUPLICATES FROM t_j_1bnflin_zw COMPARING docnum.

    IF t_j_1bnflin_zw[] IS NOT INITIAL.
      SELECT *
        FROM zfiwrt0008 INTO TABLE it_zfiwrt0008
         FOR ALL ENTRIES IN t_j_1bnflin_zw
       WHERE docnum =  t_j_1bnflin_zw-docnum.
      SORT it_zfiwrt0008 BY docnum.
    ENDIF.

    IF it_zfiwrt0008[] IS NOT INITIAL.

      SELECT *
        FROM zfiwrt0009 INTO TABLE it_zfiwrt0009
         FOR ALL ENTRIES IN it_zfiwrt0008
       WHERE seq_lcto =  it_zfiwrt0008-seq_lcto.

      SORT it_zfiwrt0009 BY seq_lcto itmnum.

    ENDIF.

    REFRESH it_zcarta.

    SELECT *
      FROM zcarta_correcao
      INTO CORRESPONDING FIELDS OF TABLE it_zcarta
      FOR ALL ENTRIES IN t_j_1bnfdoc
      WHERE docnum        =  t_j_1bnfdoc-docnum
       AND  novo_terminal <> ''.

    IF sy-subrc IS INITIAL.
      SORT it_zcarta BY docnum  ASCENDING id_cc DESCENDING.
      SELECT lifnr, name1, regio
        FROM lfa1
        APPENDING TABLE @t_lfa1
        FOR ALL ENTRIES IN @it_zcarta
        WHERE  lifnr EQ @it_zcarta-novo_terminal.

      SORT t_lfa1 BY lifnr ASCENDING.

    ENDIF.

    " Início - US 128292 - Inserção da placa do veículo - RSA
    SELECT docnum placa_cav
           FROM zlest0039
           INTO TABLE it_zlest0039
           FOR ALL ENTRIES IN t_j_1bnfdoc
           WHERE docnum = t_j_1bnfdoc-docnum.

    SORT it_zlest0039 BY docnum.
    " Fim - US 128292 - Inserção da placa do veículo - RSA

**------------------------------------- Tipo de transporte-----------******
*    "---remessa do documento de material
    DELETE t_j_1bnflin_aux WHERE reftyp NE 'BI'.
    LOOP AT t_j_1bnflin_aux INTO wa_j_1bnflin_aux.
      wa_j_1bnflin_aux-cfop = wa_j_1bnflin_aux-refkey(10).
      MODIFY t_j_1bnflin_aux FROM wa_j_1bnflin_aux INDEX sy-tabix TRANSPORTING cfop .
    ENDLOOP.

    IF t_j_1bnflin_aux[] IS NOT INITIAL.
      SELECT vbrp~vbeln vttk~shtyp
         FROM vbrp
         INNER JOIN vbak
         ON vbak~vbeln = vbrp~vgbel
         INNER JOIN vttk
         ON  vttk~tknum = vbak~tknum
         AND vttk~vsart = '01'
         INTO  TABLE t_shtyp
         FOR ALL ENTRIES IN t_j_1bnflin_aux
         WHERE vbrp~vbeln = t_j_1bnflin_aux-cfop.
    ENDIF.

*
*    SELECT MKPF~MBLNR MKPF~MJAHR VTTK~SHTYP
*      FROM MKPF
*      INNER JOIN VTTP
*      ON VTTP~VBELN = MKPF~XBLNR
*      INNER JOIN VTTK
*      ON  VTTK~TKNUM = VTTP~TKNUM
*      AND VTTK~VSART = '01'
*      INTO TABLE T_SHTYPM
*      FOR ALL ENTRIES IN T_J_1BNFLIN_AUX
*      WHERE MKPF~MBLNR EQ T_J_1BNFLIN_AUX-CFOP.
**      AND   MKPF~MJAHR EQ T_J_1BNFLIN_AUX-WERKS.
*
*
*    IF T_REMESSA[] IS NOT INITIAL.
*
*      SELECT VTTP~VBELN VTTK~SHTYP
*        FROM VTTP
*        INNER JOIN VTTK
*        ON  VTTK~TKNUM = VTTP~TKNUM
*        AND VTTK~VSART = '01'
*        INTO  TABLE T_SHTYP
*        FOR ALL ENTRIES IN T_REMESSA
*        WHERE VTTP~VBELN = T_REMESSA-VBELN.
*
*    ENDIF.

**-------------------------------------- Tipo de transporte-----------******



  ELSE.

    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                       'informados' .
    STOP.


  ENDIF.



  PERFORM f_organiza_dados_prd USING p_ativas
                                 p_nativa
                                 p_autor
                                 p_rejeit
                                 p_recus
                                 p_cancel
                                 p_agres
                                 p_nenv
                                 p_gravr.

  PERFORM f_gravar_dados_prd USING   p_gravr.




ENDFORM.                    " ZSELECIONA_DADOS_ZCONF
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_organiza_dados_prd USING           p_ativas
                                      p_nativa
                                      p_autor
                                      p_rejeit
                                      p_recus
                                      p_cancel
                                      p_agres
                                      p_nenv
                                      p_gravr.

  DATA: i                   TYPE c LENGTH 1,
        wa_kna1             TYPE kna1,
        wa_lfa1             TYPE lfa1,
        wa_t001w            TYPE t001w,
        wa_vbpavb           TYPE vbpavb,
        wa_bkpf_aux         TYPE bkpf,
        wa_zib_contabil_chv TYPE zib_contabil_chv,
        wa_zfiwrt0008       TYPE zfiwrt0008,
        wa_j_1batl1         TYPE j_1batl1,
        wa_j_1batl1t        TYPE j_1batl1t,
        wa_j_1batl2         TYPE j_1batl2,
        wa_j_1batl4a        TYPE j_1batl4a,
        wa_j_1batl4t        TYPE j_1batl4t,
        wa_j_1batl5         TYPE j_1batl5,
        wa_saida            TYPE ty_saida,

        v_docnum            TYPE j_1bnfdoc-docnum,
        quantidade          TYPE j_1bnflin-menge,
        utilizacao          TYPE tvlvt-bezei,
        cont                TYPE n LENGTH 4,
        v_bukrs             TYPE j_1bnfdoc-bukrs,
        v_parid             TYPE j_1bnfdoc-parid,
        v_len               TYPE i,
        v_len2              TYPE i,
        v_belnr             TYPE rseg-belnr,
        v_gjahr             TYPE rseg-gjahr,
        v_buzei             TYPE rseg-buzei,
        p_nordem_c          TYPE c LENGTH 10,
        v_netwrt            TYPE j_1bnflin-netwrt.

  SORT: t_j_1bnfdoc      BY belnr pstdat bukrs,
        t_j_1bnflin      BY docnum,
        t_lfa1           BY lifnr,
        t_t001           BY bukrs,
        t_remessa        BY refkey,
        t_remessa2       BY refkey,
        t_j_1baj         BY taxtyp,
        t_parceiro       BY docnum,
        t_j_1bnfe_active BY docnum,
        t_makt           BY matnr,
        t_vbpa           BY vbeln,
        t_j_1bnfstx      BY docnum itmnum,
        t_bkpf           BY awkey ,
        t_remessa_3      BY vbeln ,
        it_vbak          BY vbeln,
        t_shtyp          BY vbeln,
        it_zsdt0001      BY ch_referencia,
        it_zsdt0011      BY auart tp_movimento,
        t_j_1bnfdoc_e    BY docnum.

  DATA: wa_vbrp_aux TYPE vbrp,
        wa_rseg_aux TYPE rseg,
        var_vbeln   TYPE zsdt0001-vbeln.

  DATA: tmp_tax   LIKE j_1bnfstx OCCURS 10 WITH HEADER LINE.
  DATA: wa_lin_e TYPE j_1bnflin,
        wa_lin_i TYPE j_1binlin.

  DATA: lo_cte_switch  TYPE REF TO cl_j_1bcte_swf,
        lx_cte_related TYPE abap_bool VALUE abap_false.

  DATA: cl_util TYPE REF TO zcl_util.

  LOOP AT t_j_1bnfdoc INTO wa_j_1bnfdoc.

    CLEAR: wa_j_1bnflin,  t_j_1bnflin2[], wa_j_1batl1,v_netwrt.

*   Verifica se o documento está relacionado a um CT-e
    lo_cte_switch = cl_j_1bcte_swf=>get_instance( ).
    IF lo_cte_switch->is_cte_ctx_by_model( wa_j_1bnfdoc-model )  = abap_true OR
       lo_cte_switch->is_cte_ctx_by_docnum( iv_docnum = wa_j_1bnfdoc-docref ) = abap_true.
*     Documento é relacionado a CT-e  quando:
*       - Documento processado pertence ao modelo 57
*       - Documento processado não pertence a modelar 57 mas referes a CT-e. (Por exemplo, NF anulando uma CT-e)
      lx_cte_related = 'X'.
    ELSE.
      lx_cte_related = ' '.
    ENDIF.

    LOOP AT t_j_1bnflin INTO wa_j_1bnflin WHERE docnum = wa_j_1bnfdoc-docnum.

      cont = 0.
      LOOP AT t_j_1bnflin INTO wa_j_1bnflin_aux WHERE docnum = wa_j_1bnfdoc-docnum.
        cont = cont + 1.
      ENDLOOP.

      CLEAR: wa_saida, wa_lfa1,wa_kna1, wa_j_1bnfstx, wa_j_1baj, wa_j_1bnfe_active,wa_bkpf, wa_makt, wa_vbap, wa_remessa, wa_remessa2, wa_remessa_3, wa_vbpa, x_data, x_hora.

      CLEAR :  wa_j_1bnfstx, v_netwrt.

      REFRESH: tmp_tax.

      LOOP AT t_j_1bnfstx INTO wa_j_1bnfstx WHERE docnum EQ wa_j_1bnflin-docnum AND itmnum EQ wa_j_1bnflin-itmnum.
        IF wa_j_1bnfstx-taxtyp EQ 'ICM0' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM1' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM2' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM3' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM4' OR
           wa_j_1bnfstx-taxtyp EQ 'ICM5' OR
           wa_j_1bnfstx-taxtyp EQ 'ICOF' OR
           wa_j_1bnfstx-taxtyp EQ 'IPIS'.
          ADD wa_j_1bnfstx-taxval TO v_netwrt.
        ENDIF.

        tmp_tax = wa_j_1bnfstx.
        APPEND tmp_tax.

        READ TABLE t_j_1baj INTO wa_j_1baj WITH KEY taxtyp = wa_j_1bnfstx-taxtyp.

        IF sy-subrc IS INITIAL.
          IF wa_j_1baj-taxgrp EQ c_pis.
*             PIS
            wa_saida-pis = wa_j_1bnfstx-taxval.
          ELSEIF wa_j_1baj-taxgrp EQ c_icms.
*             ICMS

*            IF wa_j_1baj-taxtyp EQ 'ICM2' .
*              wa_saida-outros    = wa_j_1bnfstx-base.
*
*            ELSE.
*
*              wa_saida-icms      = wa_j_1bnfstx-taxval.
*              wa_saida-base_icms = wa_j_1bnfstx-base.
*              wa_saida-outros    = wa_j_1bnfstx-othbas.
*              wa_saida-excbas    = wa_j_1bnfstx-excbas.
*            ENDIF.

**Inicio / Implementação solicitado no chamado CS2023000817 / AOENNING.
** modificação IR054102 solicitado pelo Adelson WSB
*            IF wa_j_1baj-taxtyp EQ 'ICM2' AND wa_j_1bnfdoc-direct = '1'.
*              CLEAR wa_saida-icms.
*            ELSE.
*              wa_saida-icms      = wa_j_1bnfstx-taxval.
*            ENDIF.

            IF wa_j_1baj-taxtyp+0(3) EQ 'ICM' ."AND wa_j_1bnfdoc-direct = '1'.
              wa_saida-icms      = wa_j_1bnfstx-taxval.
            ENDIF.

* modificação IR054102 solicitado pelo Adelson WSB
            READ TABLE t_j_1bajt INTO DATA(wa_j_1bajt) WITH KEY taxtyp = wa_j_1baj-taxtyp.
            IF sy-subrc EQ 0.
              wa_saida-ttypetxt = |{ wa_j_1bajt-taxtyp }-{ wa_j_1bajt-ttypetxt }|.
            ENDIF.
**Fim / Implementação solicitado no chamado CS2023000817 / AOENNING.


            wa_saida-outros    = wa_j_1bnfstx-base.
            wa_saida-base_icms = wa_j_1bnfstx-base.
*              wa_saida-outros    = wa_j_1bnfstx-othbas.
            wa_saida-excbas    = wa_j_1bnfstx-excbas.


          ELSEIF wa_j_1baj-taxgrp EQ c_cofins.
*             COFINS
            wa_saida-cofins = wa_j_1bnfstx-taxval.
          ELSEIF wa_j_1baj-taxgrp EQ c_iss .
*             ISS
            wa_saida-iss    = wa_j_1bnfstx-taxval.
          ELSEIF wa_j_1baj-taxtyp EQ 'INSS' .
*             ISS
            wa_saida-inss    = wa_j_1bnfstx-taxval.
          ENDIF.

        ENDIF.

      ENDLOOP.

      IF wa_j_1bnfdoc-nfe EQ 'X'.

        READ TABLE t_j_1bnfe_active INTO wa_j_1bnfe_active WITH KEY docnum = wa_j_1bnfdoc-docnum.

        IF wa_j_1bnfe_active-docsta EQ '  ' AND wa_j_1bnfe_active-scssta EQ ' ' .
          wa_saida-status = 'Não Enviada'.
        ELSEIF wa_j_1bnfe_active-docsta EQ '  '.
          wa_saida-status = 'Aguardando resposta'.
        ELSEIF wa_j_1bnfe_active-docsta EQ 1 AND ( wa_j_1bnfe_active-cancel EQ 'X' OR wa_j_1bnfe_active-scssta EQ '2' ).
          wa_saida-status = 'Cancelada'.
        ELSEIF wa_j_1bnfe_active-docsta EQ 1 AND ( wa_j_1bnfe_active-cancel NE 'X' AND wa_j_1bnfe_active-scssta NE '2' ).
          wa_saida-status = 'autorizada'.
        ELSEIF wa_j_1bnfe_active-docsta EQ 2.
          wa_saida-status = 'rejeitada'.
        ELSEIF wa_j_1bnfe_active-docsta EQ 3.
          wa_saida-status = 'recusada'.
        ELSE.
          wa_saida-status =  wa_j_1bnfe_active-docsta .
        ENDIF.
      ENDIF.

      IF wa_j_1bnfdoc-cancel EQ 'X'.
        wa_saida-nf_status = 'Estornada'.
      ELSE.
        wa_saida-nf_status = 'Ativa'.
      ENDIF.

      "Retira empresa do codigo do parceiro
      CLEAR: v_bukrs, v_parid.
      IF wa_j_1bnfdoc-parvw = 'BR'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_j_1bnfdoc-bukrs
          IMPORTING
            output = v_bukrs.

        CONDENSE v_bukrs NO-GAPS.
        v_len  = strlen( v_bukrs ).
        v_len2 = 10 - v_len.


        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_j_1bnfdoc-parid
          IMPORTING
            output = v_parid.

        CONDENSE v_parid NO-GAPS.
        v_parid = v_parid+v_len(v_len2).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_parid
          IMPORTING
            output = v_parid.

      ELSE.
        v_parid =  wa_j_1bnfdoc-parid.

      ENDIF.

      "Cliente / Fornecedor / Local
      CLEAR: wa_lfa1, wa_kna1.
      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro   = v_parid
          p_partype    = wa_j_1bnfdoc-partyp
        CHANGING
          wa_info_part = wa_lfa1
          wa_info_c    = wa_kna1.


      IF ( wa_lfa1-lifnr IS NOT INITIAL ) OR ( wa_j_1bnfdoc-partyp = 'B' ) .

        IF wa_lfa1-lifnr IS INITIAL.
          wa_saida-cod_clifor    = v_parid.

        ELSE.

          wa_saida-cod_clifor    = wa_lfa1-lifnr.

        ENDIF.

        wa_saida-nome_clifor   = wa_lfa1-name1.
        wa_saida-stcd3         = wa_lfa1-stcd3.
        wa_saida-uf_clifor     = wa_lfa1-regio.
        wa_saida-ort01         = wa_lfa1-ort01.
*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Início de Alteração
*        wa_saida-crt_bupla     = wa_j_1bnfdoc-crt_bupla."125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC
        wa_saida-crt_bupla     = wa_lfa1-crtn.
*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Fim de Alteração
        wa_saida-stkzn         = wa_lfa1-stkzn.

*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Início de Alteração
*        " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC INICIO>>
*        IF wa_j_1bnfdoc-crt_bupla = 1.
*          wa_saida-crt_bupla = '1 - Simples Nacional'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 2.
*          wa_saida-crt_bupla = '2 - Simples Nacional - rendimento bruto abaixo limite inferior'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 3.
*          wa_saida-crt_bupla = '3 - Regime Normal (não simples)'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 4.
*          wa_saida-crt_bupla = '4 - Diferimento'.
*        ELSEIF wa_j_1bnfdoc-crt_bupla = 5.
*          wa_saida-crt_bupla = '5 - Opção pela Tributação'.
*        ENDIF.
*        " (COD_REG_TRIB) 125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC FIM <<

        CASE wa_lfa1-crtn.
          WHEN 1.
            wa_saida-crt_bupla =  '1 - Simples Nacional'.
          WHEN 2.
            wa_saida-crt_bupla  = '2 - Simples Nacional - rendimento bruto abaixo limite inferior'.
          WHEN 3.
            wa_saida-crt_bupla  = '3 - Regime Normal (não simples)'.
          WHEN 4.
* - INICIO - IR241104 - 02/07/2025 - RRIBEIRO - STEFANINI
            wa_saida-crt_bupla  = '4 - Simples Nacional - Microempreendedor Individual - MEI'.
* - FIM - IR241104 - 02/07/2025 - RRIBEIRO - STEFANINI
          WHEN 5.
            wa_saida-crt_bupla  = '5 - Opção pela Tributação'.
          WHEN OTHERS.
            CLEAR wa_saida-crt_bupla.
        ENDCASE.
*** Stefanini - IR241104 - 26/06/2025 - LAZAROSR - Fim de Alteração

        IF NOT wa_lfa1-stcd1 IS INITIAL.
          wa_saida-cpf_prod     = wa_lfa1-stcd1.
        ELSE.
          wa_saida-cpf_prod     = wa_lfa1-stcd2.
        ENDIF.

      ELSE.
        wa_saida-cod_clifor    =  v_parid.

*        IF WA_KNA1-LIFNR IS INITIAL.
*          WA_SAIDA-COD_CLIFOR    =  V_PARID.
*        ELSE.
*          WA_SAIDA-COD_CLIFOR    = WA_KNA1-LIFNR.
*        ENDIF.

        wa_saida-nome_clifor   = wa_kna1-name1.
        wa_saida-stcd3         = wa_kna1-stcd3.
        wa_saida-uf_clifor     = wa_kna1-regio.
        wa_saida-ort01         = wa_kna1-ort01.
        wa_saida-stkzn         = wa_kna1-stkzn.

        IF NOT wa_kna1-stcd1 IS INITIAL.
          wa_saida-cpf_prod     = wa_kna1-stcd1.
        ELSE.
          wa_saida-cpf_prod     = wa_kna1-stcd2.
        ENDIF.
      ENDIF.


*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            INPUT  = WA_J_1BNFDOC-PARID
*          IMPORTING
*            OUTPUT = WA_J_1BNFDOC-PARID.
*
*        CONDENSE WA_J_1BNFDOC-PARID NO-GAPS.
*        WA_J_1BNFDOC-PARID = WA_J_1BNFDOC-PARID+V_LEN(V_LEN2).
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT  = WA_J_1BNFDOC-PARID
*          IMPORTING
*            OUTPUT = WA_J_1BNFDOC-PARID.
*      ENDIF.
*
*      "Cliente / Fornecedor / Local
*      CLEAR: WA_LFA1, WA_KNA1.
*      CALL FUNCTION 'Z_PARCEIRO_INFO'
*        EXPORTING
*          P_PARCEIRO   = WA_J_1BNFDOC-PARID
*          P_PARTYPE    = WA_J_1BNFDOC-PARTYP
*        CHANGING
*          WA_INFO_PART = WA_LFA1
*          WA_INFO_C    = WA_KNA1.



      "Chave de Acesso
      CONCATENATE
        wa_j_1bnfe_active-regio
        wa_j_1bnfe_active-nfyear
        wa_j_1bnfe_active-nfmonth
        wa_j_1bnfe_active-stcd1
        wa_j_1bnfe_active-model
        wa_j_1bnfe_active-serie
        wa_j_1bnfe_active-nfnum9
        wa_j_1bnfe_active-docnum9
        wa_j_1bnfe_active-cdv    INTO wa_saida-chave_nfe.

      v_docnum = wa_j_1bnfdoc-docnum.

      READ TABLE t_j_1bnfdoc_e INTO DATA(w_j_1bnfdoc_p) WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
      IF sy-subrc = 0.
        IF w_j_1bnfdoc_p-docnum_s IS NOT INITIAL.
          v_docnum = w_j_1bnfdoc_p-docnum_s.
        ENDIF.
      ENDIF.

      LOOP AT t_parceiro INTO wa_parceiro WHERE docnum EQ v_docnum.

        CLEAR: wa_lfa1, wa_kna1.

        CASE wa_parceiro-parvw.
          WHEN 'PC'.
            CALL FUNCTION 'Z_PARCEIRO_INFO'
              EXPORTING
                p_parceiro   = wa_parceiro-parid
                p_partype    = wa_parceiro-partyp
              CHANGING
                wa_info_part = wa_lfa1
                wa_info_c    = wa_kna1.
            IF wa_lfa1 IS NOT INITIAL.
              wa_saida-parc_coleta = wa_lfa1-name1.
              wa_saida-uf_coleta   = wa_lfa1-regio.
              wa_saida-cid_coleta  = wa_lfa1-ort01.
            ELSE.
              wa_saida-parc_coleta = wa_kna1-name1.
              wa_saida-uf_coleta   = wa_kna1-regio.
              wa_saida-cid_coleta  = wa_kna1-ort01.
            ENDIF.

            "WA_SAIDA-ORT01         = WA_LFA1-ORT01.
          WHEN 'LR'.

            CALL FUNCTION 'Z_PARCEIRO_INFO'
              EXPORTING
                p_parceiro   = wa_parceiro-parid
                p_partype    = wa_parceiro-partyp
              CHANGING
                wa_info_part = wa_lfa1
                wa_info_c    = wa_kna1.

            IF wa_lfa1 IS NOT INITIAL.
              wa_saida-local_ent   = wa_lfa1-ort01. "WA_LFA1-NAME1.
              wa_saida-uf_ent      = wa_lfa1-regio.
            ELSE.
              wa_saida-local_ent   = wa_kna1-ort01. "WA_LFA1-NAME1.
              wa_saida-uf_ent      = wa_kna1-regio.
            ENDIF.

          WHEN 'Z1'.
            CALL FUNCTION 'Z_PARCEIRO_INFO'
              EXPORTING
                p_parceiro   = wa_parceiro-parid
                p_partype    = wa_parceiro-partyp
              CHANGING
                wa_info_part = wa_lfa1
                wa_info_c    = wa_kna1.

            IF wa_lfa1 IS NOT INITIAL.
              wa_saida-local_ent   = wa_lfa1-ort01. "WA_LFA1-NAME1.
              wa_saida-uf_ent      = wa_lfa1-regio.
              wa_saida-terminal    = wa_lfa1-name1. "WA_LFA1-NAME1.
            ELSE.
              wa_saida-local_ent   = wa_lfa1-ort01. "WA_LFA1-NAME1.
              wa_saida-uf_ent      = wa_lfa1-regio.
              wa_saida-terminal    = wa_lfa1-name1. "WA_LFA1-NAME1.

            ENDIF.

          WHEN OTHERS.


            "*************************************************************************
*        CLEAR: WA_LFA1,WA_KNA1.
            IF wa_parceiro-parvw EQ 'V'.
              wa_saida-local_ent   = wa_lfa1-ort01. "WA_LFA1-NAME1.
              wa_saida-uf_ent      = wa_lfa1-regio.
              wa_saida-terminal    = wa_lfa1-name1. "WA_LFA1-NAME1.
            ELSE.
              READ TABLE it_zcarta INTO wa_zcarta WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                READ TABLE t_lfa1 INTO wa_lfa1
                    WITH KEY lifnr = wa_zcarta-novo_terminal.

                wa_saida-terminal = wa_lfa1-name1.
                wa_saida-uf_terminal = wa_lfa1-regio.
              ELSE.
                READ TABLE t_vbpa INTO wa_vbpa WITH KEY vbeln = wa_j_1bnflin-refkey(10).
                IF sy-subrc IS INITIAL.
                  READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_vbpa-lifnr.
                  wa_saida-terminal    = wa_lfa1-name1.
                  wa_saida-uf_terminal = wa_lfa1-regio.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.
******************************************
      ENDLOOP.


      READ TABLE t_remessa INTO wa_remessa WITH KEY refkey = wa_j_1bnflin-refkey(10).

**-------------------------------- Tipo Transporte ---------------------------------------------
      CLEAR  wa_saida-shtyp.
      IF wa_j_1bnflin-reftyp = 'BI'. "fatura
        READ TABLE t_shtyp  INTO wa_shtyp WITH KEY vbeln = wa_j_1bnflin-refkey(10)   BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-shtyp = wa_shtyp-shtyp.
        ENDIF.
      ENDIF.

*      CLEAR  WA_SAIDA-SHTYP.
*      IF WA_J_1BNFLIN-REFTYP = 'MD'. "doc material
*        READ TABLE T_SHTYPM  INTO WA_SHTYPM WITH KEY MBLNR = WA_J_1BNFLIN-REFKEY(10)
*                                                     MJAHR = WA_J_1BNFLIN-REFKEY+10(4) BINARY SEARCH.
*        IF SY-SUBRC = 0.
*          WA_SAIDA-SHTYP = WA_SHTYPM-SHTYP.
*        ENDIF.
*      ELSE.
*        IF SY-SUBRC = 0. " se tiver Remessa
*          READ TABLE T_SHTYP  INTO WA_SHTYP WITH KEY VBELN =  WA_REMESSA-VBELN BINARY SEARCH.
*          IF SY-SUBRC = 0.
*            WA_SAIDA-SHTYP = WA_SHTYP-SHTYP.
*          ENDIF.
*        ENDIF.
*      ENDIF.
**-------------------------------- Tipo Transporte ---------------------------------------------

      READ TABLE t_remessa_3 INTO wa_remessa_3 WITH KEY vbeln = wa_remessa-refkey BINARY SEARCH .

      CLEAR: wa_saida-doc_contabil, wa_bkpf_aux.
      CASE  wa_j_1bnflin-reftyp.
        WHEN 'LI'.
          SELECT SINGLE * FROM bkpf INTO wa_bkpf_aux WHERE awkey EQ wa_j_1bnflin-refkey
                                                       AND blart EQ 'RE'.
          IF sy-subrc = 0.
            wa_saida-doc_contabil = wa_bkpf_aux-belnr.
          ENDIF.

        WHEN 'ZW'.

          SELECT SINGLE * FROM zfiwrt0008 INTO wa_zfiwrt0008 WHERE seq_lcto EQ wa_j_1bnflin-refkey.

          IF ( sy-subrc EQ 0 ).
            SELECT SINGLE * FROM zib_contabil_chv INTO wa_zib_contabil_chv WHERE obj_key EQ wa_zfiwrt0008-obj_key.
            IF sy-subrc = 0.
              wa_saida-doc_contabil = wa_zib_contabil_chv-belnr.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          IF wa_remessa_3-refkey IS NOT INITIAL.
            READ TABLE t_bkpf INTO wa_bkpf WITH KEY awkey = wa_remessa_3-refkey.
            IF sy-subrc = 0.
              wa_saida-doc_contabil = wa_bkpf-belnr.
            ENDIF.
          ELSE.
            IF wa_j_1bnflin-refkey IS NOT INITIAL.
              "Se não tem remessa procura pelo bkpf - CSB
              SELECT SINGLE * FROM bkpf INTO wa_bkpf_aux WHERE awkey EQ wa_j_1bnflin-refkey
                                               AND blart EQ 'RV'.
              IF sy-subrc = 0.
                wa_saida-doc_contabil = wa_bkpf_aux-belnr.
              ENDIF.
            ENDIF.
          ENDIF.

      ENDCASE.
                                                            "CH.129254
      wa_saida-netwrt   = wa_j_1bnflin-netwr .

      READ TABLE t_j_1bnflin2 WITH KEY docnum = wa_j_1bnflin-docnum.
      IF sy-subrc IS INITIAL.
        CLEAR: wa_saida-vlr_dolar.
      ENDIF.


      READ TABLE t_vbap INTO wa_vbap WITH KEY vbeln = wa_remessa-vgbel.
      wa_saida-utilizacao  = wa_vbap-bezei.

      utilizacao = ''.

      READ TABLE t_vbrp INTO wa_vbrp WITH KEY vbeln = wa_j_1bnflin-refkey(10).

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_remessa-vgbel
          IMPORTING
            output = wa_saida-ordem.
***--------------------------------------------
        IF wa_saida-ordem IS NOT INITIAL.
          READ TABLE it_zsdt0053 INTO wa_zsdt0053
              WITH KEY vbeln = wa_saida-ordem.

          IF sy-subrc = 0.
            wa_saida-instrucao = wa_zsdt0053-instrucao.
          ENDIF.

        ENDIF.
***---------------------------------------------


        "WA_SAIDA-ORDEM       = WA_REMESSA-VGBEL.
        wa_saida-remessa     = wa_remessa-vbeln.
        wa_saida-romaneio    = wa_remessa-nr_romaneio.
        wa_saida-ref_ro      = wa_remessa-ch_referencia.
        wa_saida-doc_fatura  = wa_remessa-refkey.

        CLEAR wa_lfa1.

      ENDIF.
      IF wa_saida-ordem IS INITIAL.
        READ TABLE t_j_1bnfdoc_e INTO DATA(w_j_1bnfdoc_e) WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-ordem = w_j_1bnfdoc_e-vbeln.
        ENDIF.
      ENDIF.

      READ TABLE it_zfiwrt0008 INTO wa_zfiwrt0008 WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_zfiwrt0009 INTO wa_zfiwrt0009 WITH KEY seq_lcto = wa_zfiwrt0008-seq_lcto
                                                             itmnum   = wa_j_1bnflin-itmnum
                                                             BINARY SEARCH.
        IF ( sy-subrc = 0 ) AND
           ( wa_zfiwrt0008-imobilizado = 'S' ) AND
           ( wa_zfiwrt0009-anln1 IS NOT INITIAL ).
          wa_saida-anln1 =  wa_zfiwrt0009-anln1.
        ENDIF.
      ENDIF.


      " Início - US 128292 - Inserção da placa do veículo - RSA
      READ TABLE it_zlest0039 INTO wa_zlest0039 WITH KEY docnum = wa_j_1bnfdoc-docnum BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_saida-placa_cav = wa_zlest0039-placa_cav.
      ENDIF.
      " Fim - US 128292 - Inserção da placa do veículo - RSA


      READ TABLE t_makt INTO wa_makt WITH KEY  matnr  = wa_j_1bnflin-matnr.
      "WA_SAIDA-PRODUTO             = WA_MAKT-MAKTX.
      wa_saida-produto             = wa_j_1bnflin-matnr.

      READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_j_1bnflin-matnr.
      IF sy-subrc = 0.
        READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl.
        IF sy-subrc = 0.
          wa_saida-matkl   =  wa_t023t-matkl.
          wa_saida-wgbez60 =  wa_t023t-wgbez60.
        ENDIF.
      ENDIF.

      IF wa_j_1bnfdoc-nfe EQ 'X' .
        wa_saida-nfenum   = wa_j_1bnfdoc-nfenum.
      ELSE .
        wa_saida-nfenum   = wa_j_1bnfdoc-nfnum.
      ENDIF.

      quantidade = wa_j_1bnflin-menge.

      IF quantidade EQ 0.
        quantidade = 1.
      ENDIF.

      CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
        EXPORTING
          branch  = wa_j_1bnfdoc-bukrs
          bukrs   = wa_j_1bnfdoc-branch
        IMPORTING
          address = wa_adress.


      wa_saida-inco1    = wa_j_1bnfdoc-inco1.
      wa_saida-inco2    = wa_j_1bnfdoc-inco2.
      wa_saida-nfe      = wa_j_1bnfdoc-nfe   .
      wa_saida-bukrs    = wa_j_1bnfdoc-bukrs .
      wa_saida-branch   = wa_j_1bnfdoc-branch.
      wa_saida-uf_filial   = wa_adress-regio.
      wa_saida-docnum   = wa_j_1bnfdoc-docnum.
      wa_saida-pstdat   = wa_j_1bnfdoc-pstdat.
      wa_saida-docdat   = wa_j_1bnfdoc-docdat.
      wa_saida-direct   = wa_j_1bnfdoc-direct.  "Í
      wa_saida-series   = wa_j_1bnfdoc-series.
      wa_saida-ntgew    = wa_j_1bnfdoc-ntgew.
      wa_saida-cfop     = wa_j_1bnflin-cfop  .
      wa_saida-menge    = wa_j_1bnflin-menge .
      wa_saida-meins    = wa_j_1bnflin-meins .
      wa_saida-charg    = wa_j_1bnflin-charg.
      wa_saida-ncm      = wa_j_1bnflin-nbm.

      wa_saida-itmnum   = wa_j_1bnflin-itmnum.

      wa_saida-nftype  = wa_j_1bnfdoc-nftype.
      wa_saida-model   = wa_j_1bnfdoc-model.
      wa_saida-refkey  = wa_j_1bnflin-refkey.

      wa_saida-maktx  = wa_j_1bnflin-maktx.

      SELECT SINGLE * FROM j_1batl1 INTO wa_j_1batl1 WHERE taxlaw EQ wa_j_1bnflin-taxlw1.

      IF ( sy-subrc EQ 0 ).
        SELECT SINGLE *
          FROM j_1batl1t
          INTO wa_j_1batl1t
         WHERE taxlaw = wa_j_1batl1-taxlaw
           AND langu = sy-langu.

        IF sy-subrc = 0.
          CONCATENATE wa_j_1batl1t-line1
                      wa_j_1batl1t-line2
                      wa_j_1batl1t-line3
                      wa_j_1batl1t-line4
          INTO wa_saida-lei_icms
          SEPARATED BY space.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
          EXPORTING
            input  = wa_j_1batl1-taxsit
          IMPORTING
            output = wa_saida-taxlw1.

**  Begin of CS2022000515 #97985 FF   28.11.2022
        wa_saida-cod_icms = wa_j_1batl1t-taxlaw.

        IF wa_saida-lei_icms IS INITIAL.
          wa_saida-lei_icms = wa_j_1batl1t-descrip.
        ENDIF.
** End of FF  28.11.2022


        CLEAR: wa_j_1batl1, wa_j_1batl1t.
      ENDIF.

      SELECT SINGLE * FROM j_1batl2 INTO wa_j_1batl2 WHERE taxlaw EQ wa_j_1bnflin-taxlw2.

      IF ( sy-subrc EQ 0 ).
        wa_saida-taxlw2 = wa_j_1batl2-taxsit.
        CLEAR: wa_j_1batl1.
      ENDIF.

      "Verifica CST Saida
      IF ( wa_j_1bnflin-taxlw4 IS NOT INITIAL ) AND
         ( wa_j_1bnflin-taxsi4 IS NOT INITIAL ) AND
         ( wa_j_1bnfdoc-direct EQ '2'         ).

        SELECT SINGLE * FROM j_1batl4a INTO wa_j_1batl4a
         WHERE taxlaw    EQ wa_j_1bnflin-taxlw4
           AND taxsitout EQ wa_j_1bnflin-taxsi4.

        IF sy-subrc = 0.
          SELECT SINGLE txt
            FROM j_1btaxsitcoft INTO wa_saida-lei_cofins
           WHERE langu  = sy-langu
             AND taxsit = wa_j_1bnflin-taxsi4.

          IF sy-subrc EQ 0.
            wa_saida-taxlw4 = wa_j_1bnflin-taxsi4.
          ENDIF.
        ENDIF.
        CLEAR: wa_j_1batl4a.
      ENDIF.

      IF wa_saida-taxlw4 IS INITIAL.

        SELECT SINGLE * FROM j_1batl4a INTO wa_j_1batl4a WHERE taxlaw EQ wa_j_1bnflin-taxlw4.

        IF ( sy-subrc EQ 0 ).
          wa_saida-taxlw4 = wa_j_1batl4a-taxsit.
          SELECT SINGLE *
            FROM j_1batl4t
            INTO wa_j_1batl4t
           WHERE taxlaw = wa_j_1batl4a-taxlaw
             AND langu = sy-langu.

          IF sy-subrc = 0.
            CONCATENATE wa_j_1batl4t-line1
                        wa_j_1batl4t-line2
                        wa_j_1batl4t-line3
                        wa_j_1batl4t-line4
            INTO wa_saida-lei_cofins
            SEPARATED BY space.
          ENDIF.

          CLEAR: wa_j_1batl1, wa_j_1batl4t.
        ENDIF.

      ENDIF.
      DATA(v_nbm) = wa_j_1bnflin-nbm.
      REPLACE ALL OCCURRENCES OF '.' IN v_nbm WITH ' '.
      CONDENSE v_nbm NO-GAPS.



      DESCRIBE TABLE t_zib_nfe_dist_itm LINES DATA(vl_lines).

      IF vl_lines > 1.
        READ TABLE t_zib_nfe_dist_itm INTO DATA(wa_zib_nfe_itm) WITH KEY chave_nfe = wa_saida-chave_nfe
                                                                         prod_ncm  = v_nbm.
        IF sy-subrc EQ 0.
          wa_saida-taxlw4_xml = wa_zib_nfe_itm-pis_cst.
          wa_saida-taxlw5_xml = wa_zib_nfe_itm-cof_cst.
        ENDIF.
      ELSE.
        READ TABLE t_zib_nfe_dist_itm INTO wa_zib_nfe_itm INDEX 1.
        IF sy-subrc EQ 0.
          wa_saida-taxlw4_xml = wa_zib_nfe_itm-pis_cst.
          wa_saida-taxlw5_xml = wa_zib_nfe_itm-cof_cst.
        ENDIF.
      ENDIF.



*        SELECT SINGLE cof_cst
*          FROM zib_nfe_dist_itm
*          INTO wa_saida-taxlw5_xml
*          WHERE chave_nfe = wa_saida-chave_nfe
*          AND   prod_ncm  = v_nbm.


*      SELECT SINGLE pis_cst
*          FROM zib_nfe_dist_itm
*          INTO wa_saida-taxlw4_xml
*          WHERE chave_nfe = wa_saida-chave_nfe
*          AND   prod_ncm  = v_nbm.

      "Verifica CST Saida
      IF ( wa_j_1bnflin-taxlw5 IS NOT INITIAL ) AND
         ( wa_j_1bnflin-taxsi5 IS NOT INITIAL ) AND
         ( wa_j_1bnfdoc-direct EQ '2'         ).

        SELECT SINGLE * FROM j_1batl5 INTO wa_j_1batl5
         WHERE taxlaw    EQ wa_j_1bnflin-taxlw5
           AND taxsitout EQ wa_j_1bnflin-taxsi5.

        IF sy-subrc = 0.
          wa_saida-taxlw5 = wa_j_1bnflin-taxsi5.
        ENDIF.
        CLEAR: wa_j_1batl5.

      ENDIF.

      IF wa_saida-taxlw5 IS INITIAL.

        SELECT SINGLE * FROM j_1batl5 INTO wa_j_1batl5 WHERE taxlaw EQ wa_j_1bnflin-taxlw5.

        IF ( sy-subrc EQ 0 ).
          wa_saida-taxlw5 = wa_j_1batl5-taxsit.
        ENDIF.

      ENDIF.



      IF ( wa_j_1bnflin-reftyp EQ 'BI' ).

        CLEAR: wa_vbrp_aux.
        SELECT SINGLE * FROM vbrp INTO wa_vbrp_aux WHERE vbeln EQ wa_j_1bnflin-refkey.

        wa_saida-iva = wa_vbrp_aux-j_1btxsdc.

      ELSEIF ( wa_j_1bnflin-reftyp EQ 'LI').

        CLEAR: wa_rseg_aux.
        v_belnr = wa_j_1bnflin-refkey+0(10).
        v_gjahr = wa_j_1bnflin-refkey+10(4).
        SELECT SINGLE * FROM rseg INTO wa_rseg_aux WHERE belnr EQ v_belnr
                                                   AND   gjahr EQ v_gjahr
                                                   AND   buzei EQ	wa_j_1bnflin-refitm.

        wa_saida-iva = wa_rseg_aux-mwskz.

      ENDIF.


      "Valor Total da NF

*      call function 'J_1B_NF_VALUE_DETERMINATION'
*        exporting
*          nf_header   = wa_j_1bnfdoc
*        importing
*          ext_header  = wa_j_cabe
*        tables
*          nf_item     = t_j_1bnflin
*          nf_item_tax = t_j_1bnfstx.

      " Se não achar BSID E BSAD continuar com valores
*******************-------------------------------------------
      " Se for nota de serviço (exceção a regra)
      IF wa_j_1bnfdoc-nfesrv EQ 'X' .
        wa_saida-netwrt   = wa_j_1bnflin-nfnet.
      ENDIF.

      IF  wa_saida-netwrt EQ 0 .
*        IF WA_J_1BNFLIN-NFNET > 0 .
*          WA_SAIDA-NETWRT   = WA_J_1BNFLIN-NFNET.
*        ELSE.
*         comentado em 04.12.12 solicitação Marcos Santos
*          IF P_DIRECT = 1 AND WA_J_1BNFLIN-REFTYP NE 'ZW'.
*            WA_SAIDA-NETWRT   = WA_J_1BNFLIN-NETWR  + WA_SAIDA-ICMS + WA_SAIDA-PIS + WA_SAIDA-COFINS + WA_SAIDA-ISS.
*          ELSE.
*            WA_SAIDA-NETWRT   = WA_J_1BNFLIN-NETWR .
*          ENDIF.
        wa_saida-netwrt   = wa_j_1bnflin-netwr .
*        ENDIF.
      ENDIF.
      " Valor Item
      IF cont > 1.
        wa_saida-netwrt = wa_j_1bnflin-netwr.
      ENDIF.


      "Comentado porque estava somando o imposto.
*      "ALRS
      "IF V_NETWRT GT 0 AND  1 IN P_DIRECT[].
      "  ADD V_NETWRT TO WA_SAIDA-NETWRT.
      "ENDIF.

      CLEAR: wa_lin_e, wa_lin_i.

      MOVE-CORRESPONDING wa_j_1bnflin TO wa_lin_e.

      CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION_I'
        EXPORTING
          nf_item                 = wa_lin_e
          iv_nf_direction         = wa_j_1bnfdoc-direct
          ix_posted_with_xml_data = wa_j_1bnfdoc-autom_incoming
          ix_cte_related          = lx_cte_related
        IMPORTING
          ext_item                = wa_lin_i
        TABLES
          nf_item_tax             = tmp_tax.

      IF wa_lin_i-nftot > 0 .
        wa_saida-netwrt = wa_lin_i-nftot.
      ELSEIF wa_lin_i-nfnett > 0 .
        wa_saida-netwrt = wa_lin_i-nfnett.
      ELSEIF wa_lin_i-netwrt > 0.
        wa_saida-netwrt = wa_lin_i-netwrt.
      ENDIF.

      wa_saida-vlr_unit = wa_saida-netwrt / quantidade.
      wa_saida-user     = sy-uname.
      wa_saida-kursk      = 0.
      wa_saida-vlr_dolar  = 0.
      wa_saida-unit_dolar = 0.
      "ALRS 11/01/2016
      IF wa_saida-doc_contabil  IS NOT INITIAL.
        CLEAR wa_vbrp-kursk.

*<--- S4 Migration - 16/06/2023 - MA
        SELECT SINGLE *
          FROM bseg
          INTO wa_bseg
          WHERE bukrs = wa_saida-bukrs
          AND belnr   = wa_saida-doc_contabil. "#EC CI_DB_OPERATION_OK[2431747]
*<--- S4 Migration - 16/06/2023 - MA

        IF sy-subrc = 0.
          IF wa_bseg-dmbe2 NE 0.
            wa_vbrp-kursk      = wa_bseg-dmbtr / wa_bseg-dmbe2.
          ENDIF.
        ENDIF.

        "Busca Data e Hora Registro
        CLEAR wa_bkpf_c.

        SELECT SINGLE *
          FROM bkpf
          INTO wa_bkpf_c
          WHERE bukrs = wa_saida-bukrs
            AND belnr = wa_saida-doc_contabil
            AND gjahr = wa_saida-pstdat(4).

        IF sy-subrc = 0.

          IF wa_bkpf_c-cpudt NE 0.
            wa_saida-data_registro   = wa_bkpf_c-cpudt.
          ENDIF.

          IF wa_bkpf_c-cputm NE 0.
            wa_saida-hora_registro   = wa_bkpf_c-cputm.
          ENDIF.

        ENDIF.

        DATA(vl_doc_contabil) = wa_saida-refkey+0(10).
        DATA(vl_doc_buzei)    = wa_rseg_aux-buzei.

        SELECT SINGLE *
          FROM rbco
          INTO @DATA(wl_rbco)
          WHERE belnr EQ @vl_doc_contabil
            AND buzei EQ @vl_doc_buzei.

        IF ( sy-subrc EQ 0 ).

          wa_saida-ordem_doc = wl_rbco-aufnr.
          SHIFT wa_saida-ordem_doc LEFT DELETING LEADING '0'.
          CLEAR: wl_rbco, vl_doc_contabil, vl_doc_buzei.

        ENDIF.

      ENDIF.

*      READ TABLE T_BSID INTO WA_BSID WITH KEY BELNR = WA_BKPF-BELNR.
*      IF SY-SUBRC IS INITIAL.
*        IF WA_BSID-DMBE2 NE 0.
*          WA_VBRP-KURSK      = WA_BSID-DMBTR / WA_BSID-DMBE2.
*        ENDIF.
*      ELSE.
*        READ TABLE T_BSAD INTO WA_BSAD WITH KEY BELNR = WA_BKPF-BELNR.
*        IF SY-SUBRC IS INITIAL.
*          IF WA_BSAD-DMBE2 NE 0.
*            WA_VBRP-KURSK      = WA_BSAD-DMBTR / WA_BSAD-DMBE2.
*          ENDIF.
*        ELSE.
*          READ TABLE T_BSIK INTO WA_BSIK WITH KEY BELNR = WA_BKPF-BELNR.
*          IF SY-SUBRC IS INITIAL.
*            IF WA_BSIK-DMBE2 NE 0.
*              WA_VBRP-KURSK      = WA_BSIK-DMBTR / WA_BSIK-DMBE2.
*            ENDIF.
*          ELSE.
*            READ TABLE T_BSAK INTO WA_BSAK WITH KEY BELNR = WA_BKPF-BELNR.
*            IF SY-SUBRC IS INITIAL.
*              WA_VBRP-KURSK      = WA_BSAK-DMBTR / WA_BSAK-DMBE2.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.

      IF wa_vbrp-kursk GT 0.
        wa_saida-kursk      = wa_vbrp-kursk."TAXA
        wa_saida-vlr_dolar  = wa_saida-netwrt / wa_vbrp-kursk."VLR DOLAR
        wa_saida-unit_dolar = ( wa_saida-netwrt / quantidade ) /  wa_vbrp-kursk.
      ENDIF.

*      IF  WA_SAIDA-VLR_DOLAR EQ 0 . " Se não achar BSID E BSAD continuar com valores
*        IF WA_VBRP-KURSK > 1 .
*          WA_SAIDA-KURSK      = WA_VBRP-KURSK."TAXA
*          WA_SAIDA-VLR_DOLAR  = WA_SAIDA-NETWRT / WA_VBRP-KURSK."VLR DOLAR
*          WA_SAIDA-UNIT_DOLAR = ( WA_SAIDA-NETWRT / QUANTIDADE ) /  WA_VBRP-KURSK.
*        ELSE.
*          WA_SAIDA-KURSK      = 0.
*          WA_SAIDA-VLR_DOLAR  = 0.
*          WA_SAIDA-UNIT_DOLAR = 0.
*        ENDIF.
*      ENDIF.

*----------------- Info de Ordem CS2017001206 INÍCIO

      IF ( wa_j_1bnflin-reftyp EQ 'LI' ).

        CLEAR: wa_ekbe, wa_aufk.

        READ TABLE t_ekbe INTO wa_ekbe WITH KEY belnr = wa_j_1bnflin-refkey(10)
                                                buzei = wa_j_1bnflin-refitm+2(4).
        IF sy-subrc IS INITIAL.

          wa_saida-ebeln = wa_ekbe-ebeln.
          wa_saida-ebelp2 = wa_ekbe-ebelp. "CS2022000515 - FF  28.11.2022
*---
          READ TABLE t_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekbe-ebeln
                                                  ebelp = wa_ekbe-ebelp.
          IF sy-subrc IS INITIAL.
            wa_saida-knttp = wa_ekpo-knttp.
          ENDIF.
*---
          READ TABLE t_ekkn INTO wa_ekkn WITH KEY ebeln = wa_ekbe-ebeln
                                                  ebelp = wa_ekbe-ebelp.
          IF sy-subrc IS INITIAL.
            READ TABLE t_aufk INTO wa_aufk WITH KEY aufnr = wa_ekkn-aufnr.

            IF sy-subrc IS INITIAL.

              wa_saida-auart2 = wa_aufk-auart.
              wa_saida-autyp = wa_aufk-autyp.

**  Begin of "CS2022000515 - FF  28.11.2022
              wa_saida-aufnr = wa_aufk-aufnr.
*              wa_saida-ktext = wa_aufk-ktext.
** End of "CS2022000515 - FF  28.11.2022


**  Begin of "CS2022000515 - FF  16.01.2023
              READ TABLE t_ordem_pm INTO DATA(wa_ordem_pm) WITH KEY aufnr = wa_ekkn-aufnr.
              IF sy-subrc = 0.
                wa_saida-vornr = wa_ordem_pm-vornr.
                wa_saida-ltxa1 = wa_ordem_pm-ltxa1.
              ENDIF.
** End of "CS2022000515 - FF  16.01.2023


              READ TABLE t_dd07v INTO wa_dd07v WITH KEY domvalue_l = wa_saida-autyp.
              IF sy-subrc IS INITIAL.
                wa_saida-ddtext = wa_dd07v-ddtext.
              ENDIF.

            ENDIF.

            wa_saida-kostl = COND #( WHEN wa_saida-knttp NE ' ' THEN wa_ekkn-kostl ELSE '' ).
            wa_saida-sakto = COND #( WHEN wa_saida-knttp NE ' ' THEN wa_ekkn-sakto ELSE '' ).

            READ TABLE t_skat INTO wa_skat WITH KEY saknr = wa_saida-sakto.
            wa_saida-desc_conta_ctb = wa_skat-txt50.

            READ TABLE t_cskt INTO ws_cskt WITH KEY kostl =  wa_saida-kostl.
            wa_saida-desc_centro_cust = ws_cskt-ltext.

          ENDIF.


*---
        ENDIF.

      ENDIF.

*----------------- Info de Ordem CS2017001206 FIM


      IF wa_saida-nf_status EQ 'Cancelada'.
        wa_saida-menge      = 0.
        wa_saida-netwrt     = 0.
        wa_saida-vlr_unit   = 0.
        wa_saida-pis        = 0.
        wa_saida-icms       = 0.
        wa_saida-base_icms  = 0.
        wa_saida-outros     = 0.
        wa_saida-excbas     = 0.
        wa_saida-cofins     = 0.
        wa_saida-kursk      = 0.
        wa_saida-vlr_dolar  = 0.
        wa_saida-unit_dolar = 0.
      ENDIF.

      x_data = sy-datum.
      x_hora = sy-uzeit.

      CONCATENATE x_data+6(2) '/'
                  x_data+4(2) '/'
                  x_data(4)   ' -  '
                  x_hora(2)   ':'
                  x_hora+2(2) ':'
                  x_hora+4(2) INTO wa_saida-data.
      i = '1'.

      IF ( p_ativas EQ 'X' AND wa_j_1bnfdoc-cancel NE 'X' AND wa_j_1bnfdoc-nfe NE 'X'     ) OR
         ( p_nativa EQ 'X' AND wa_j_1bnfdoc-cancel EQ 'X' AND wa_j_1bnfdoc-nfe NE 'X'     ) OR
         ( p_autor  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfdoc-nfe EQ 'X' AND ( wa_j_1bnfe_active-cancel NE 'X' AND wa_j_1bnfe_active-scssta NE '2' )  ) OR
         ( p_rejeit EQ 'X' AND wa_j_1bnfe_active-docsta EQ 2 AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
         ( p_recus  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 3 AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
         ( p_cancel EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND ( wa_j_1bnfe_active-cancel EQ 'X' OR wa_j_1bnfe_active-scssta EQ '2' ) ) OR
     "    ( p_autor  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfe_active-cancel EQ 'X' AND  wa_j_1bnfdoc-nfe EQ 'X'  ) OR
         ( p_agres  EQ 'X' AND wa_j_1bnfe_active-docsta EQ ' ' AND wa_j_1bnfe_active-scssta NE ' ' AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
         ( p_nenv   EQ 'X' AND wa_j_1bnfe_active-docsta EQ ' ' AND wa_j_1bnfe_active-scssta EQ   ' '  AND wa_j_1bnfdoc-nfe EQ 'X'  ).
        i = '0'.
      ENDIF.

      IF ( p_ativas EQ 'X' AND p_nativa NE 'X' AND  wa_j_1bnfdoc-cancel EQ 'X' ) OR
         ( p_nativa EQ 'X' AND p_ativas NE 'X' AND  wa_j_1bnfdoc-cancel NE 'X' ) .
        i = '1'.
      ENDIF.



      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-ordem
        IMPORTING
          output = p_nordem_c.


      IF  ( p_nordem  IS INITIAL AND i = '0' ).
        i = '0'.
      ELSEIF ( (  p_nordem IS NOT INITIAL AND p_nordem_c IN p_nordem ) AND i = '0' ).
        i = '0'.
      ELSEIF ( ( p_nordem IS NOT INITIAL  AND p_nordem_c NOT IN p_nordem ) AND i = '0') .
        i = '1'.
      ENDIF.

      IF i EQ '1' .
        CONTINUE.
      ELSE.
        APPEND wa_saida TO t_saida.
      ENDIF.

      APPEND wa_j_1bnflin TO t_j_1bnflin2.

    ENDLOOP.

    CLEAR: wa_j_1bnfdoc, wa_saida.

  ENDLOOP.



  LOOP AT t_saida INTO wa_saida.
    wa_ret_movimento_mensal_zconf-inco1        = wa_saida-inco1.
    wa_ret_movimento_mensal_zconf-inco2        = wa_saida-inco2.

    wa_ret_movimento_mensal_zconf-bukrs         = wa_saida-bukrs.
    wa_ret_movimento_mensal_zconf-branch        = wa_saida-branch.
    wa_ret_movimento_mensal_zconf-cfop          = wa_saida-cfop.
    wa_ret_movimento_mensal_zconf-nfenum        = wa_saida-nfenum.
    wa_ret_movimento_mensal_zconf-series        = wa_saida-series.
    wa_ret_movimento_mensal_zconf-meins         = wa_saida-meins.
    wa_ret_movimento_mensal_zconf-pstdat        = wa_saida-pstdat.
    wa_ret_movimento_mensal_zconf-docdat        = wa_saida-docdat.
    wa_ret_movimento_mensal_zconf-menge         = wa_saida-menge.
    wa_ret_movimento_mensal_zconf-ntgew         = wa_saida-ntgew.
    wa_ret_movimento_mensal_zconf-netwrt        = wa_saida-netwrt.
    wa_ret_movimento_mensal_zconf-base_icms     = wa_saida-base_icms.
    wa_ret_movimento_mensal_zconf-outros        = wa_saida-outros.
    wa_ret_movimento_mensal_zconf-icms          = wa_saida-icms.
    wa_ret_movimento_mensal_zconf-pis           = wa_saida-pis.
    wa_ret_movimento_mensal_zconf-cofins        = wa_saida-cofins.
    wa_ret_movimento_mensal_zconf-iss           = wa_saida-iss.
    wa_ret_movimento_mensal_zconf-inss          = wa_saida-inss.
    wa_ret_movimento_mensal_zconf-docnum        = wa_saida-docnum.
    wa_ret_movimento_mensal_zconf-stcd3         = wa_saida-stcd3.
    wa_ret_movimento_mensal_zconf-excbas        = wa_saida-excbas.
    wa_ret_movimento_mensal_zconf-produto       = wa_saida-produto.
    wa_ret_movimento_mensal_zconf-matkl         = wa_saida-matkl.
    wa_ret_movimento_mensal_zconf-wgbez60       = wa_saida-wgbez60.
    wa_ret_movimento_mensal_zconf-ordem         = wa_saida-ordem.
    wa_ret_movimento_mensal_zconf-remessa       = wa_saida-remessa.
    wa_ret_movimento_mensal_zconf-romaneio      = wa_saida-romaneio.
    wa_ret_movimento_mensal_zconf-ref_ro        = wa_saida-ref_ro.
    wa_ret_movimento_mensal_zconf-utilizacao    = wa_saida-utilizacao.
    wa_ret_movimento_mensal_zconf-doc_fatura    = wa_saida-doc_fatura.
    wa_ret_movimento_mensal_zconf-kursk         = wa_saida-kursk.
    wa_ret_movimento_mensal_zconf-vlr_dolar     = wa_saida-vlr_dolar.
    wa_ret_movimento_mensal_zconf-cpf_prod      = wa_saida-cpf_prod.
    wa_ret_movimento_mensal_zconf-unit_dolar    = wa_saida-unit_dolar.
    wa_ret_movimento_mensal_zconf-status        = wa_saida-status.
    wa_ret_movimento_mensal_zconf-nf_status     = wa_saida-nf_status.
    wa_ret_movimento_mensal_zconf-vlr_unit      = wa_saida-vlr_unit.
    wa_ret_movimento_mensal_zconf-cod_clifor    = wa_saida-cod_clifor.
    wa_ret_movimento_mensal_zconf-nome_clifor   = wa_saida-nome_clifor.
    wa_ret_movimento_mensal_zconf-uf_clifor     = wa_saida-uf_clifor.
    wa_ret_movimento_mensal_zconf-ort01         = wa_saida-ort01.
    wa_ret_movimento_mensal_zconf-crt_bupla     = wa_saida-crt_bupla."125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC
    wa_ret_movimento_mensal_zconf-stkzn         = wa_saida-stkzn.
    wa_ret_movimento_mensal_zconf-parc_coleta   = wa_saida-parc_coleta.
    wa_ret_movimento_mensal_zconf-cid_coleta    = wa_saida-cid_coleta.
    wa_ret_movimento_mensal_zconf-uf_coleta     = wa_saida-uf_coleta.
    wa_ret_movimento_mensal_zconf-local_ent     = wa_saida-local_ent.
    wa_ret_movimento_mensal_zconf-uf_ent        = wa_saida-uf_ent.
    wa_ret_movimento_mensal_zconf-terminal      = wa_saida-terminal.
    wa_ret_movimento_mensal_zconf-uf_terminal   = wa_saida-uf_terminal.
    wa_ret_movimento_mensal_zconf-nfe           = wa_saida-nfe.
    wa_ret_movimento_mensal_zconf-data          = wa_saida-data.
    wa_ret_movimento_mensal_zconf-user1         = wa_saida-user.
    wa_ret_movimento_mensal_zconf-doc_contabil  = wa_saida-doc_contabil.
    wa_ret_movimento_mensal_zconf-ordem_doc     = wa_saida-ordem_doc.
    wa_ret_movimento_mensal_zconf-data_registro = wa_saida-data_registro.
    wa_ret_movimento_mensal_zconf-hora_registro = wa_saida-hora_registro.
    wa_ret_movimento_mensal_zconf-nftype        = wa_saida-nftype.
    wa_ret_movimento_mensal_zconf-model         = wa_saida-model.
    wa_ret_movimento_mensal_zconf-refkey        = wa_saida-refkey.
    wa_ret_movimento_mensal_zconf-maktx         = wa_saida-maktx.
    wa_ret_movimento_mensal_zconf-taxlw1        = wa_saida-taxlw1.
    wa_ret_movimento_mensal_zconf-taxlw2        = wa_saida-taxlw2.
    wa_ret_movimento_mensal_zconf-taxlw4        = wa_saida-taxlw4.
    wa_ret_movimento_mensal_zconf-taxlw5        = wa_saida-taxlw5.
    "
    wa_ret_movimento_mensal_zconf-taxlw4_xml    = wa_saida-taxlw4_xml.
    wa_ret_movimento_mensal_zconf-taxlw5_xml    = wa_saida-taxlw5_xml.
    "
    wa_ret_movimento_mensal_zconf-iva           = wa_saida-iva.
    wa_ret_movimento_mensal_zconf-charg         = wa_saida-charg.
    wa_ret_movimento_mensal_zconf-ncm           = wa_saida-ncm.
    wa_ret_movimento_mensal_zconf-chave_nfe     = wa_saida-chave_nfe.
    wa_ret_movimento_mensal_zconf-lei_icms      = wa_saida-lei_icms.
    wa_ret_movimento_mensal_zconf-lei_cofins    = wa_saida-lei_cofins.
    wa_ret_movimento_mensal_zconf-shtyp         = wa_saida-shtyp.
    wa_ret_movimento_mensal_zconf-anln1         = wa_saida-anln1.
    wa_ret_movimento_mensal_zconf-instrucao     = wa_saida-instrucao.
    wa_ret_movimento_mensal_zconf-ebeln         = wa_saida-ebeln.
    wa_ret_movimento_mensal_zconf-auart         = wa_saida-auart2.
    wa_ret_movimento_mensal_zconf-autyp         = wa_saida-autyp.
    wa_ret_movimento_mensal_zconf-knttp         = wa_saida-knttp.
    wa_ret_movimento_mensal_zconf-ddtext        = wa_saida-ddtext.
    wa_ret_movimento_mensal_zconf-kostl         = wa_saida-kostl.
    wa_ret_movimento_mensal_zconf-desc_conta_ctb         = wa_saida-desc_conta_ctb.
    wa_ret_movimento_mensal_zconf-sakto         = wa_saida-sakto.
    wa_ret_movimento_mensal_zconf-desc_centro_cust        = wa_saida-desc_centro_cust.

**  Begin of "CS2022000515 - FF  28.11.2022
    wa_ret_movimento_mensal_zconf-cod_icms = wa_saida-cod_icms.
    wa_ret_movimento_mensal_zconf-aufnr    = wa_saida-aufnr.
*    wa_ret_movimento_mensal_zconf-ktext    = wa_saida-ktext.
    wa_ret_movimento_mensal_zconf-anln2    = wa_saida-anln2.
    wa_ret_movimento_mensal_zconf-ebelp2   = wa_saida-ebelp2.
** End of "CS2022000515 - FF  28.11.2022

**  Begin of "CS2022000515 - FF  16.01.2023
    wa_ret_movimento_mensal_zconf-regio = wa_saida-regio.
    wa_ret_movimento_mensal_zconf-vornr = wa_saida-vornr.
    wa_ret_movimento_mensal_zconf-ltxa1 = wa_saida-ltxa1.
    wa_ret_movimento_mensal_zconf-recap = wa_saida-recap.
** End of "CS2022000515 - FF  16.01.2023

    wa_ret_movimento_mensal_zconf-placa_cav = wa_saida-placa_cav.
    wa_ret_movimento_mensal_zconf-itmnum    = wa_saida-itmnum.
    wa_ret_movimento_mensal_zconf-direct    = wa_saida-direct.

    APPEND wa_ret_movimento_mensal_zconf TO it_ret_movimento_mensal_zconf.

  ENDLOOP.

  CLEAR : wa_ret_movimento_mensal_zconf , wa_saida.


ENDFORM.                    " F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gravar_dados_prd USING  p_gravr  .
  DATA: v_anterior LIKE zdados_saida-docnum,
        wa_saida   TYPE ty_saida.

*  TABLES : ZDADOS_SAIDA.

  IF p_gravr EQ 'X'.
    LOOP AT t_saida INTO wa_saida.

      IF wa_saida-docnum NE v_anterior.
        zdados_saida-sequencia = 1.
        v_anterior = wa_saida-docnum.
      ELSE.
        zdados_saida-sequencia = zdados_saida-sequencia + 1.
      ENDIF.

      zdados_saida-bukrs         = wa_saida-bukrs.
      zdados_saida-branch        = wa_saida-branch.
      zdados_saida-cfop          = wa_saida-cfop.
      zdados_saida-nfenum        = wa_saida-nfenum.
      zdados_saida-series        = wa_saida-series.
      zdados_saida-meins         = wa_saida-meins.
      zdados_saida-pstdat        = wa_saida-pstdat.
      zdados_saida-docdat        = wa_saida-docdat.
      zdados_saida-menge         = wa_saida-menge.
      zdados_saida-ntgew         = wa_saida-ntgew.
      zdados_saida-netwrt        = wa_saida-netwrt.
      zdados_saida-base_icms     = wa_saida-base_icms.
      zdados_saida-outros        = wa_saida-outros.
      zdados_saida-icms          = wa_saida-icms.
      zdados_saida-pis           = wa_saida-pis.
      zdados_saida-cofins        = wa_saida-cofins.
      zdados_saida-iss           = wa_saida-iss.
      zdados_saida-inss          = wa_saida-inss.
      zdados_saida-docnum        = wa_saida-docnum.
      zdados_saida-stcd3         = wa_saida-stcd3.
      zdados_saida-excbas        = wa_saida-excbas.
      zdados_saida-produto       = wa_saida-produto.
      zdados_saida-matkl         = wa_saida-matkl.
      zdados_saida-wgbez60       = wa_saida-wgbez60.
      zdados_saida-ordem         = wa_saida-ordem.
      zdados_saida-remessa       = wa_saida-remessa.
      zdados_saida-romaneio      = wa_saida-romaneio.
      zdados_saida-ref_ro        = wa_saida-ref_ro.
      zdados_saida-utilizacao    = wa_saida-utilizacao.
      zdados_saida-doc_fatura    = wa_saida-doc_fatura.
      zdados_saida-kursk         = wa_saida-kursk.
      zdados_saida-vlr_dolar     = wa_saida-vlr_dolar.
      zdados_saida-cpf_prod      = wa_saida-cpf_prod.
      zdados_saida-unit_dolar    = wa_saida-unit_dolar.
      zdados_saida-status        = wa_saida-status.
      zdados_saida-nf_status     = wa_saida-nf_status.
      zdados_saida-vlr_unit      = wa_saida-vlr_unit.
      zdados_saida-cod_clifor    = wa_saida-cod_clifor.
      zdados_saida-nome_clifor   = wa_saida-nome_clifor.
      zdados_saida-uf_clifor     = wa_saida-uf_clifor.
      zdados_saida-ort01         = wa_saida-ort01.
      zdados_saida-crt_bupla     = wa_saida-crt_bupla."125494-Inclusão coluna - ZCONF (Simples Nacional) Equalização ECC x Hana - SMC
      zdados_saida-stkzn         = wa_saida-stkzn.
      zdados_saida-parc_coleta   = wa_saida-parc_coleta.
      zdados_saida-cid_coleta    = wa_saida-cid_coleta.
      zdados_saida-uf_coleta     = wa_saida-uf_coleta.
      zdados_saida-local_ent     = wa_saida-local_ent.
      zdados_saida-uf_ent        = wa_saida-uf_ent.
      zdados_saida-terminal      = wa_saida-terminal.
      zdados_saida-uf_terminal   = wa_saida-uf_terminal.
      zdados_saida-nfe           = wa_saida-nfe.
      zdados_saida-data          = wa_saida-data.
      zdados_saida-user1         = wa_saida-user.
      zdados_saida-doc_contabil  = wa_saida-doc_contabil.
      zdados_saida-data_registro = wa_saida-data_registro.
      zdados_saida-hora_registro = wa_saida-hora_registro.
      zdados_saida-nftype        = wa_saida-nftype.
      zdados_saida-model         = wa_saida-model.
      zdados_saida-refkey        = wa_saida-refkey.
      zdados_saida-maktx         = wa_saida-maktx.
      zdados_saida-taxlw1        = wa_saida-taxlw1.
      zdados_saida-taxlw2        = wa_saida-taxlw2.
      zdados_saida-taxlw4        = wa_saida-taxlw4.
      zdados_saida-taxlw5        = wa_saida-taxlw5.
      "
      zdados_saida-taxlw4_xml    = wa_saida-taxlw4_xml.
      zdados_saida-taxlw5_xml    = wa_saida-taxlw5_xml.
      "
      zdados_saida-iva           = wa_saida-iva.
      zdados_saida-charg         = wa_saida-charg.
      zdados_saida-ncm           = wa_saida-ncm.
      zdados_saida-chave_nfe     = wa_saida-chave_nfe.
      zdados_saida-lei_icms      = wa_saida-lei_icms.
      zdados_saida-lei_cofins    = wa_saida-lei_cofins.
*      ZDADOS_SAIDA-SHTYP        = WA_SAIDA-SHTYP.
      zdados_saida-anln1         = wa_saida-anln1.
      zdados_saida-inco1         = wa_saida-inco1.
      zdados_saida-inco2         = wa_saida-inco2.

      zdados_saida-placa_cav     = wa_saida-placa_cav.
      zdados_saida-itmnum        = wa_saida-itmnum.

      INSERT zdados_saida.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " F_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_VINCULO_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_vinculo_entrada_prd.

  DATA: t_zsdt0001_e_aux TYPE TABLE OF zsdt0001,
        t_zsdt0001_s_aux TYPE TABLE OF zsdt0001,
        vnr_romaneio     TYPE zsdt0001-nr_romaneio,
        tabix            TYPE sy-tabix.

  CHECK t_j_1bnfdoc_e[] IS NOT INITIAL.

  SELECT  *
    FROM zmmt_ee_zgr_docs
    INTO TABLE @DATA(t_ee_zgr_docs)
    FOR ALL ENTRIES IN @t_j_1bnfdoc_e
   WHERE docnum EQ @t_j_1bnfdoc_e-docnum.

  IF t_ee_zgr_docs[] IS NOT INITIAL.
    SELECT *
      FROM zmmt_ee_zgr INTO TABLE @DATA(t_ee_zgr)
      FOR ALL ENTRIES IN @t_ee_zgr_docs
     WHERE obj_key EQ @t_ee_zgr_docs-obj_key.

    IF t_ee_zgr[] IS NOT INITIAL.
      SELECT  *
        FROM zsdt0001 INTO TABLE @DATA(t_zsdt0001_e)
        FOR ALL ENTRIES IN @t_ee_zgr
       WHERE ch_referencia EQ @t_ee_zgr-ch_referencia.
    ENDIF.
  ENDIF.

  LOOP AT t_j_1bnfdoc_e INTO DATA(w_j_1bnfdoc_e).

    IF w_j_1bnfdoc_e-nfenum IS NOT INITIAL.
      w_j_1bnfdoc_e-nfnum2  = w_j_1bnfdoc_e-nfenum.
    ELSEIF w_j_1bnfdoc_e-nfnum IS NOT INITIAL.
      w_j_1bnfdoc_e-nfnum2  = w_j_1bnfdoc_e-nfnum.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_j_1bnfdoc_e-nfnum2
      IMPORTING
        output = w_j_1bnfdoc_e-nfnum2.
    "
    MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX sy-tabix TRANSPORTING nfnum2.
  ENDLOOP.

  SELECT  *
    FROM zsdt0001 APPENDING TABLE t_zsdt0001_e
    FOR ALL ENTRIES IN t_j_1bnfdoc_e
   WHERE bukrs        EQ t_j_1bnfdoc_e-bukrs
     AND branch       EQ t_j_1bnfdoc_e-branch
     AND parid        EQ t_j_1bnfdoc_e-parid
     AND docdat       EQ t_j_1bnfdoc_e-docdat
     AND nfnum        EQ t_j_1bnfdoc_e-nfnum2
     AND tp_movimento EQ 'E'.

  CHECK t_zsdt0001_e[] IS NOT INITIAL.

  "Pesquisa romaneio SEM zeros a esquerda
  LOOP AT t_zsdt0001_e INTO DATA(wl_zsdt0001_e).
    wl_zsdt0001_e-id_referencia = wl_zsdt0001_e-nr_romaneio.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wl_zsdt0001_e-id_referencia
      IMPORTING
        output = wl_zsdt0001_e-id_referencia.
    MODIFY t_zsdt0001_e FROM wl_zsdt0001_e INDEX sy-tabix TRANSPORTING id_referencia.
  ENDLOOP.

  SELECT *
    FROM zsdt0001 INTO TABLE @DATA(t_zsdt0001_s)
    FOR ALL ENTRIES IN @t_zsdt0001_e
   WHERE bukrs          EQ @t_zsdt0001_e-bukrs
     AND branch         EQ @t_zsdt0001_e-branch
     AND id_referencia  EQ @t_zsdt0001_e-id_referencia
     AND tp_movimento   EQ 'S'
     AND nr_safra       EQ @t_zsdt0001_e-nr_safra.
  "
  "Pesquisa romaneio COM zeros a esquerda
*  LOOP AT t_zsdt0001_e INTO wl_zsdt0001_e.
  LOOP AT t_zsdt0001_e ASSIGNING FIELD-SYMBOL(<fs_zsdt0001_e>).
    <fs_zsdt0001_e>-id_referencia = <fs_zsdt0001_e>-nr_romaneio.
*    MODIFY t_zsdt0001_e FROM wl_zsdt0001_e INDEX sy-tabix TRANSPORTING id_referencia.
  ENDLOOP.

  SELECT *
    FROM zsdt0001 APPENDING TABLE t_zsdt0001_s
    FOR ALL ENTRIES IN t_zsdt0001_e
   WHERE bukrs          EQ t_zsdt0001_e-bukrs
     AND branch         EQ t_zsdt0001_e-branch
     AND id_referencia  EQ t_zsdt0001_e-id_referencia
     AND tp_movimento   EQ 'S'
     AND nr_safra       EQ t_zsdt0001_e-nr_safra.


  "pesquisa pelo ID_CARGA
  SELECT *
    FROM zsdt0001 APPENDING TABLE t_zsdt0001_s
    FOR ALL ENTRIES IN t_zsdt0001_e
   WHERE bukrs          EQ t_zsdt0001_e-bukrs
     AND branch         EQ t_zsdt0001_e-branch
     AND id_carga       EQ t_zsdt0001_e-id_carga
     AND id_carga       NE ' '
     AND tp_movimento   EQ 'S'
     AND nr_safra       EQ t_zsdt0001_e-nr_safra.

  "Volta sem Zeros a esquerda
*  LOOP AT t_zsdt0001_e INTO wl_zsdt0001_e.
  LOOP AT t_zsdt0001_e ASSIGNING <fs_zsdt0001_e>.
    <fs_zsdt0001_e>-id_referencia = <fs_zsdt0001_e>-nr_romaneio.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <fs_zsdt0001_e>-id_referencia
      IMPORTING
        output = <fs_zsdt0001_e>-id_referencia.
*    MODIFY t_zsdt0001_e FROM wl_zsdt0001_e INDEX sy-tabix TRANSPORTING id_referencia.
  ENDLOOP.

  t_zsdt0001_e_aux[] = t_zsdt0001_e[].
  t_zsdt0001_s_aux[] = t_zsdt0001_s[].

  SORT:  t_ee_zgr_docs     BY docnum,
         t_ee_zgr          BY obj_key,
         t_zsdt0001_e      BY ch_referencia, "INTERFACE GRAOS
         t_zsdt0001_e_aux  BY bukrs branch parid docdat nfnum, "Romaneio
         t_zsdt0001_s      BY bukrs branch id_referencia nr_safra,
         t_zsdt0001_s_aux  BY bukrs branch id_carga nr_safra.

  LOOP AT t_j_1bnfdoc_e INTO w_j_1bnfdoc_e.
    tabix = sy-tabix.
    "busca 1
    READ TABLE t_ee_zgr_docs INTO DATA(w_ee_zgr_docs) WITH KEY docnum = w_j_1bnfdoc_e-docnum BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE t_ee_zgr INTO DATA(w_t_ee_zgr) WITH KEY obj_key = w_ee_zgr_docs-obj_key BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE t_zsdt0001_e INTO wl_zsdt0001_e WITH KEY ch_referencia = w_t_ee_zgr-ch_referencia BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE t_zsdt0001_s INTO DATA(w_zsdt0001_s) WITH KEY bukrs          = wl_zsdt0001_e-bukrs
                                                                   branch         = wl_zsdt0001_e-branch
                                                                   id_referencia  = wl_zsdt0001_e-id_referencia
                                                                   nr_safra       = wl_zsdt0001_e-nr_safra BINARY SEARCH.
          IF sy-subrc = 0.
            w_j_1bnfdoc_e-vbeln    = w_zsdt0001_s-vbeln.
            w_j_1bnfdoc_e-docnum_s = w_zsdt0001_s-nro_nf_prod.
            MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX tabix TRANSPORTING vbeln docnum_s .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "Busca 2
    IF w_j_1bnfdoc_e-vbeln IS INITIAL.
      READ TABLE t_zsdt0001_e_aux INTO DATA(w_zsdt0001_e_aux) WITH KEY bukrs  = w_j_1bnfdoc_e-bukrs
                                                                       branch = w_j_1bnfdoc_e-branch
                                                                       parid  = w_j_1bnfdoc_e-parid
                                                                       docdat = w_j_1bnfdoc_e-docdat
                                                                       nfnum  = w_j_1bnfdoc_e-nfnum2 BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE t_zsdt0001_s INTO w_zsdt0001_s WITH KEY bukrs          = w_zsdt0001_e_aux-bukrs
                                                           branch         = w_zsdt0001_e_aux-branch
                                                           id_referencia  = w_zsdt0001_e_aux-id_referencia
                                                           nr_safra       = w_zsdt0001_e_aux-nr_safra BINARY SEARCH.
        IF sy-subrc = 0.
          w_j_1bnfdoc_e-vbeln = w_zsdt0001_s-vbeln.
          w_j_1bnfdoc_e-docnum_s = w_zsdt0001_s-nro_nf_prod.
          MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX tabix TRANSPORTING vbeln docnum_s.
        ELSE.
          w_zsdt0001_e_aux-id_referencia = w_zsdt0001_e_aux-nr_romaneio.
          READ TABLE t_zsdt0001_s INTO w_zsdt0001_s WITH KEY bukrs          = w_zsdt0001_e_aux-bukrs
                                                             branch         = w_zsdt0001_e_aux-branch
                                                             id_referencia  = w_zsdt0001_e_aux-id_referencia
                                                             nr_safra       = w_zsdt0001_e_aux-nr_safra BINARY SEARCH.
          IF sy-subrc = 0.
            w_j_1bnfdoc_e-vbeln    = w_zsdt0001_s-vbeln.
            w_j_1bnfdoc_e-docnum_s = w_zsdt0001_s-nro_nf_prod.
            MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX tabix TRANSPORTING vbeln docnum_s.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "Busca 3
    IF w_j_1bnfdoc_e-vbeln IS INITIAL.
      READ TABLE t_zsdt0001_e_aux INTO w_zsdt0001_e_aux WITH KEY  bukrs  = w_j_1bnfdoc_e-bukrs
                                                                  branch = w_j_1bnfdoc_e-branch
                                                                  parid  = w_j_1bnfdoc_e-parid
                                                                  docdat = w_j_1bnfdoc_e-docdat
                                                                  nfnum  = w_j_1bnfdoc_e-nfnum2 BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE t_zsdt0001_s_aux INTO w_zsdt0001_s WITH KEY bukrs      = w_zsdt0001_e_aux-bukrs
                                                               branch     = w_zsdt0001_e_aux-branch
                                                               id_carga   = w_zsdt0001_e_aux-id_carga
                                                               nr_safra   = w_zsdt0001_e_aux-nr_safra BINARY SEARCH.
        IF sy-subrc = 0.
          w_j_1bnfdoc_e-vbeln = w_zsdt0001_s-vbeln.
          w_j_1bnfdoc_e-docnum_s = w_zsdt0001_s-nro_nf_prod.
          MODIFY t_j_1bnfdoc_e FROM w_j_1bnfdoc_e INDEX tabix TRANSPORTING vbeln docnum_s.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  DELETE t_j_1bnfdoc_e  WHERE vbeln IS INITIAL.

  IF t_j_1bnfdoc_e[] IS NOT INITIAL.
    SELECT docnum parid partyp parvw
        FROM j_1bnfnad
        APPENDING TABLE t_parceiro
        FOR ALL ENTRIES IN t_j_1bnfdoc_e
          WHERE docnum EQ t_j_1bnfdoc_e-docnum_s
          AND   parvw  EQ 'PC'.
  ENDIF.

ENDFORM.
