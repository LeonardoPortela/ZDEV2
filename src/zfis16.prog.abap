*&------------P R O J E T O  E V O L U I R   -   M A G G I-------------*
* Programa   : ZFIS16                                                  *
* Descrição  : Subcontratados de Frete                                 *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Paulo Bonetti                          Data: 02/11/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*&---------------------------------------------------------------------*


REPORT  zfis16.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
* TIPOS PARA ALV
*----------------------------------------------------------------------*

TYPE-POOLS: slis.

TABLES : vfkp, vttk, rbkp, zib_cte_dist_n55, lfa1.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES:
  BEGIN OF ty_vttk,
    tdlnr           TYPE vttk-tdlnr,
    tplst           TYPE vttk-tplst,
    exti1           TYPE vttk-exti1,
    shtyp           TYPE vttk-shtyp,
    tknum           TYPE vttk-tknum,
    text1           TYPE vttk-text1,
    add02           TYPE vttk-add02,
    add03           TYPE vttk-add02,
    fknum           TYPE zlest0032-fknum,
    belnr           TYPE zlest0032-belnr,
    obj_key_sub     TYPE zlest0032-obj_key_sub,
    bukrs           TYPE vfkp-bukrs,
    werks           TYPE vfkp-werks,
    budat           TYPE vfkp-budat,
    vbeln           TYPE vttp-vbeln,
    gjahr           TYPE zlest0032-gjahr,
    data            TYPE zlest0032-data,
    branch          TYPE j_1bbranch-branch,
    obj_key_sub_aux TYPE bseg-belnr,
  END   OF ty_vttk,

  BEGIN OF ty_vtpa,
    vbeln TYPE vtpa-vbeln,
    lifnr TYPE vtpa-lifnr,
    parvw TYPE vtpa-parvw,
    kunnr TYPE vtpa-kunnr,
  END   OF ty_vtpa,

  BEGIN OF ty_vttk_aux,
    lifnr TYPE lfa1-lifnr,
  END   OF ty_vttk_aux,

  BEGIN OF ty_konv,
    knumv LIKE konv-knumv,
    kwert LIKE konv-kwert,
    kschl LIKE konv-kschl,
  END   OF ty_konv,

  BEGIN OF ty_vfkp,
    rebel TYPE vfkp-rebel,
    bukrs TYPE vfkp-bukrs,
    werks TYPE vfkp-werks,
    knumv TYPE vfkp-knumv,
    ebeln TYPE vfkp-ebeln,
    lblni TYPE vfkp-lblni,
    fkpty TYPE vfkp-fkpty,
    fknum TYPE vfkp-fknum, "US #167060 - MMSILVA - 25.02.2025
  END   OF ty_vfkp,

  BEGIN OF ty_vbak       ,
    vbeln TYPE vbak-vbeln,
    tknum TYPE vbak-tknum,
  END   OF ty_vbak       ,

  BEGIN OF ty_vbpa       ,
    lifnr TYPE vbpa-lifnr,
    vbeln TYPE vbpa-vbeln,
    parvw TYPE vbpa-parvw,
  END   OF ty_vbpa       ,

  BEGIN OF ty_vbrp       ,
    vbeln     TYPE vbrp-vbeln,
    posnr     TYPE vbrp-posnr,
    vgbel     TYPE vbrp-vgbel,
    netwr     TYPE vbrp-netwr,
    vbeln_ref TYPE j_1bnflin-refkey,
  END   OF ty_vbrp       ,

  BEGIN OF ty_vbfa       ,
    vbeln   TYPE vbfa-vbeln,
    vbtyp_n TYPE vbfa-vbtyp_n,
    vbtyp_v TYPE vbfa-vbtyp_v,
    vbelv   TYPE vbfa-vbelv,
  END   OF ty_vbfa       ,

  BEGIN OF ty_vbfa_aux       ,
    vbeln   TYPE vbfa-vbeln,
    vbtyp_n TYPE vbfa-vbtyp_n,
    vbtyp_v TYPE vbfa-vbtyp_v,
    vbelv   TYPE vbfa-vbelv,
    mjahr   TYPE vbfa-mjahr,
    refkey  TYPE j_1bnflin-refkey,
  END   OF ty_vbfa_aux       ,

  BEGIN OF ty_zgr_docs,
    av_vbeln TYPE zmmt_ee_zgr_docs-av_vbeln,
    ft_belnr TYPE zmmt_ee_zgr_docs-ft_belnr,
    ft_gjahr TYPE zmmt_ee_zgr_docs-ft_gjahr,
    refkey   TYPE j_1bnflin-refkey,
  END OF  ty_zgr_docs,

  BEGIN OF ty_coleta     ,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
    ort01 TYPE lfa1-ort01,
    regio TYPE lfa1-regio,
  END   OF ty_coleta     ,

  BEGIN OF ty_remetente  ,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
  END   OF ty_remetente  ,

  BEGIN OF ty_parceiro  ,
    lifnr  TYPE lfa1-lifnr,
    name1  TYPE lfa1-name1,
    stcd1  TYPE lfa1-stcd1,
    stcd2  TYPE lfa1-stcd2,
    ort01  TYPE lfa1-ort01,
    regio  TYPE lfa1-regio,
    stkzn  TYPE lfa1-stkzn,
    crtn   TYPE lfa1-crtn,
    indtyp TYPE lfa1-indtyp,
  END   OF ty_parceiro  ,

  BEGIN OF ty_t001w      ,
    werks TYPE t001w-werks,
    lifnr TYPE t001w-lifnr,
    kunnr TYPE t001w-kunnr,
  END   OF ty_t001w      ,

  BEGIN OF ty_bnflin,
    docnum TYPE j_1bnflin-docnum,
    refkey TYPE j_1bnflin-refkey,
    netwr  TYPE j_1bnflin-netwr,
  END OF ty_bnflin,

  BEGIN OF ty_bnfdoc,
    docnum TYPE j_1bnfdoc-docnum,
    nfenum TYPE j_1bnfdoc-nfenum,
    series TYPE j_1bnfdoc-series,
    pstdat TYPE j_1bnfdoc-pstdat,
    nfe    TYPE j_1bnfdoc-nfe,
    nfnum  TYPE j_1bnfdoc-nfnum,
  END OF ty_bnfdoc,

  "--------------------- AQUI

  BEGIN OF ty_j_1bnfe_active_cte,
    docnum  TYPE j_1bnfe_active-docnum,
    regio   TYPE j_1bnfe_active-regio,
    nfyear  TYPE j_1bnfe_active-nfyear,
    nfmonth TYPE j_1bnfe_active-nfmonth,
    stcd1   TYPE j_1bnfe_active-stcd1,
    model   TYPE j_1bnfe_active-model,
    serie   TYPE j_1bnfe_active-serie,
    nfnum9  TYPE j_1bnfe_active-nfnum9,
    docnum9 TYPE j_1bnfe_active-docnum9,
    cdv     TYPE j_1bnfe_active-cdv,
  END OF ty_j_1bnfe_active_cte,

  BEGIN OF ty_j_1bnfe_active_nfe,
    docnum  TYPE j_1bnfe_active-docnum,
    regio   TYPE j_1bnfe_active-regio,
    nfyear  TYPE j_1bnfe_active-nfyear,
    nfmonth TYPE j_1bnfe_active-nfmonth,
    stcd1   TYPE j_1bnfe_active-stcd1,
    model   TYPE j_1bnfe_active-model,
    serie   TYPE j_1bnfe_active-serie,
    nfnum9  TYPE j_1bnfe_active-nfnum9,
    docnum9 TYPE j_1bnfe_active-docnum9,
    cdv     TYPE j_1bnfe_active-cdv,
  END OF ty_j_1bnfe_active_nfe,


  BEGIN OF ty_ekbe,
    ebeln TYPE ekbe-ebeln,
    vgabe TYPE ekbe-vgabe,
    lfbnr TYPE ekbe-lfbnr,
    belnr TYPE ekbe-belnr,
    gjahr TYPE ekbe-gjahr,
  END OF ty_ekbe,

  BEGIN OF ty_rbkp,
    bukrs TYPE rbkp-bukrs,
    belnr TYPE rbkp-belnr,
    gjahr TYPE rbkp-gjahr,
    rmwwr TYPE rbkp-rmwwr,
    budat TYPE rbkp-budat,
    xblnr TYPE vttk-tknum,
    awkey TYPE bkpf-awkey,
    stblg TYPE rbkp-stblg,
  END OF ty_rbkp,

  BEGIN OF ty_bkpf,
    bukrs TYPE bkpf-bukrs,
    gjahr TYPE bkpf-gjahr,
    awkey TYPE bkpf-awkey,
    belnr TYPE bkpf-belnr,
  END OF ty_bkpf,

  BEGIN OF ty_rbco,
    belnr	  TYPE rbco-belnr,
    gjahr	  TYPE rbco-gjahr,
    buzei	  TYPE rbco-buzei,
    cobl_nr	TYPE rbco-cobl_nr,
    wrbtr   TYPE rbco-wrbtr,
    saknr   TYPE rbco-saknr,
  END OF ty_rbco,

  BEGIN OF ty_bsis,
    bukrs	TYPE bsis-bukrs,
    hkont	TYPE bsis-hkont,
    augdt	TYPE bsis-augdt,
    augbl	TYPE bsis-augbl,
    zuonr	TYPE bsis-zuonr,
    gjahr	TYPE bsis-gjahr,
    belnr	TYPE bsis-belnr,
    buzei	TYPE bsis-buzei,
    dmbtr TYPE bsis-dmbtr,
  END OF ty_bsis,

  BEGIN OF ty_zlest0002,
    pc_veiculo TYPE zlest0002-pc_veiculo,
    cd_cidade  TYPE zlest0002-cd_cidade,
    cd_uf      TYPE zlest0002-cd_uf,
  END OF ty_zlest0002,


  BEGIN OF ty_saida,
    werks          TYPE vfkp-werks,
    bukrs          TYPE vfkp-bukrs,
    erdat          TYPE vttk-erdat,
    dacte          TYPE vttk-exti1,
    tknum          TYPE vttk-tknum,
    text1          TYPE vttk-text1,
    fknum          TYPE vfkp-fknum,
    shtyp          TYPE c LENGTH 27,
    remetente      TYPE lfa1-name1,
    filial         TYPE vttk-tdlnr,
    material       TYPE makt-matnr,
    desc_mat       TYPE makt-maktx,
    quantidade     TYPE vtrlp-lfimg,
    netwr          TYPE vtrlp-netwr,
    proprietario   TYPE lfa1-name1,
    cod_prop       TYPE lfa1-lifnr,
    cpf_prod       TYPE lfa1-stcd1,
    destinatario   TYPE lfa1-name1,
    parc_coleta    TYPE c LENGTH 35,
    cid_coleta     TYPE lfa1-ort01,
    uf_coleta      TYPE lfa1-regio,
    pont_entrega   TYPE c LENGTH 35,
    cid_entrega    TYPE lfa1-ort01,
    uf_entrega     TYPE lfa1-regio,
    cod_dest       TYPE lfa1-lifnr,
    crtn           TYPE lfa1-crtn,
    crtn_desc      TYPE val_text,
    indtyp         TYPE lfa1-indtyp,
    indtyp_desc    TYPE j_1btindtypt-j_1bindtypx,
    tp_pessoa      TYPE c LENGTH 10,
    ordem          TYPE vbak-vbeln,
    fatura         TYPE vbrp-vbeln,
    data(25)       TYPE c,
    user           TYPE sy-uname,
    belnr          TYPE rbkp-belnr,
    gjahr          TYPE rbkp-gjahr,
    vlr_fatura     TYPE vbrp-netwr,
    belnrb         TYPE bkpf-belnr,
    budat          TYPE rbkp-budat,
    pc_veiculo     TYPE  zlest0002-pc_veiculo	,
    cd_cidade      TYPE zlest0002-cd_cidade,
    cd_uf          TYPE zlest0002-cd_uf,
    nfenum         TYPE j_1bnfdoc-nfenum,
    series         TYPE j_1bnfdoc-series,
    docnum_nfe     TYPE j_1bnflin-docnum,
    pstdat         TYPE j_1bnfdoc-pstdat,
    nfnett         TYPE j_1bnflin-nfnett,
    docnum         TYPE j_1bnflin-docnum,
    docnum_ped     TYPE j_1bnflin-docnum,
    branch         TYPE j_1bbranch-branch,
    bukrs_sub      TYPE j_1bbranch-bukrs,
    doc_sub        TYPE zlest0032-obj_key_sub,
    wrbtr          TYPE bseg-wrbtr,
    chave_cte(250) TYPE c,
    chave_nfe(250) TYPE c,
    pis_sub        TYPE rbco-wrbtr,
    cofins_sub     TYPE rbco-wrbtr,
    filial_2       TYPE j_1bbranch-branch,  "*-CS2022001119-07.02.2023-#99561-JT
    pis_tom        TYPE rbco-wrbtr, " User Story 144142 // MMSILVA - 01.10.2024
    cofins_tom     TYPE rbco-wrbtr, " User Story 144142 // MMSILVA - 01.10.2024
    kwert          TYPE konv-kwert, "US #167060 - MMSILVA - 25.02.2025
    pis_ped        TYPE acdoca-hsl, "US #167060 - MMSILVA - 25.02.2025
    cofins_ped     TYPE acdoca-hsl, "US #167060 - MMSILVA - 25.02.2025
  END  OF ty_saida.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.


"US #167060 - MMSILVA - 25.02.2025 - Inicio
TYPES: BEGIN OF ty_vfkp_ped,
         rebel TYPE vfkp-rebel,
         knumv TYPE vfkp-knumv,
       END OF ty_vfkp_ped,

       BEGIN OF ty_prcd_elements,
         knumv TYPE prcd_elements-knumv,
         kposn TYPE prcd_elements-kposn,
         kschl TYPE prcd_elements-kschl,
         kwert TYPE prcd_elements-kwert,
       END OF ty_prcd_elements,

       BEGIN OF ty_zlest0032,
         tknum       TYPE zlest0032-tknum,
         obj_key_ped TYPE zlest0032-obj_key_ped,
       END OF ty_zlest0032,

       BEGIN OF ty_zib_contabil_chv,
         belnr   TYPE zib_contabil_chv-belnr,
         obj_key TYPE zib_contabil_chv-obj_key,
         bukrs   TYPE zib_contabil_chv-bukrs,
         gjahr   TYPE zib_contabil_chv-gjahr,
       END OF ty_zib_contabil_chv,

       BEGIN OF ty_bkpf_ped,
         belnr TYPE bkpf-belnr,
         bukrs TYPE bkpf-bukrs,
         gjahr TYPE bkpf-gjahr,
         stblg TYPE bkpf-stblg,
       END OF ty_bkpf_ped,

       BEGIN OF ty_acdoca,
         belnr  TYPE acdoca-belnr,
         rldnr  TYPE acdoca-rldnr,
         rbukrs TYPE acdoca-rbukrs,
         gjahr  TYPE acdoca-gjahr,
         racct  TYPE acdoca-racct,
         hsl    TYPE acdoca-hsl,
       END OF ty_acdoca.
"US #167060 - MMSILVA - 25.02.2025 - Fim

*&---------------------------------------------------------------------*
*& Constantes
*&---------------------------------------------------------------------*

*constants:
*c_pc(2)            type c value 'PC',
*c_lr(2)            type c value 'LR'.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: t_vttk               TYPE TABLE OF ty_vttk WITH HEADER LINE,
      t_vttk_aux           TYPE TABLE OF ty_vttk WITH HEADER LINE,
      t_lfa1               TYPE TABLE OF lfa1,
      t_vtpa               TYPE TABLE OF ty_vtpa,
      t_coleta             TYPE TABLE OF ty_coleta,
      t_rem_lif            TYPE TABLE OF ty_remetente,
      t_parc_lif           TYPE TABLE OF ty_parceiro,
      t_parc_kun           TYPE TABLE OF ty_parceiro,
      t_rem_kun            TYPE TABLE OF ty_remetente,
      t_rem_lst            TYPE TABLE OF ty_remetente,
      t_t001w              TYPE TABLE OF ty_t001w,
      t_saida              TYPE TABLE OF ty_saida,
      t_konv               TYPE TABLE OF ty_konv,
      t_vfsi               TYPE TABLE OF vfsi,
      t_vfkp               TYPE TABLE OF ty_vfkp,
      t_vfkp_aux           TYPE TABLE OF ty_vfkp,
      t_tvtkt              TYPE TABLE OF tvtkt,
      t_vbak               TYPE TABLE OF ty_vbak,
      t_vbpa               TYPE TABLE OF ty_vbpa,
      t_vbfa               TYPE TABLE OF ty_vbfa WITH HEADER LINE,
      t_vbfa_aux           TYPE TABLE OF ty_vbfa_aux WITH HEADER LINE,
      t_vbfa_aux_m         TYPE TABLE OF ty_vbfa_aux WITH HEADER LINE,
      t_vbfa_aux_r         TYPE TABLE OF ty_vbfa_aux WITH HEADER LINE,
      t_zgr_docs           TYPE TABLE OF ty_zgr_docs WITH HEADER LINE,
      t_zlest0110          TYPE TABLE OF zlest0110 WITH HEADER LINE,
      t_bnfdoc             TYPE TABLE OF ty_bnfdoc WITH HEADER LINE,
      t_bnflin             TYPE TABLE OF ty_bnflin WITH HEADER LINE,
      t_vbrp               TYPE TABLE OF ty_vbrp,
      t_ekbe               TYPE TABLE OF ty_ekbe,
      t_rbkp               TYPE TABLE OF ty_rbkp,
      t_rbkp_aux           TYPE TABLE OF ty_rbkp,
      t_bkpf               TYPE TABLE OF ty_bkpf,
      t_rbco               TYPE TABLE OF ty_rbco,
      t_bsis               TYPE TABLE OF ty_bsis,
      t_zlest0002          TYPE TABLE OF ty_zlest0002,
      t_zlest0032          TYPE TABLE OF zlest0032,
      t_j_1bnflin          TYPE TABLE OF j_1bnflin,
      t_lips               TYPE TABLE OF lips,
      t_j_1bbranch         TYPE TABLE OF j_1bbranch,
      t_bseg               TYPE TABLE OF bseg,
      t_j_1bnfe_active_cte TYPE TABLE OF ty_j_1bnfe_active_cte,
      t_j_1bnfe_active_nfe TYPE TABLE OF ty_j_1bnfe_active_nfe,
      t_j_1btindtypt       TYPE TABLE OF j_1btindtypt,
      t_dd07t              TYPE TABLE OF dd07t,
      t_vfkp_ped           TYPE TABLE OF ty_vfkp_ped, "US #167060 - MMSILVA - 25.02.2025
      t_prcd_elements      TYPE TABLE OF ty_prcd_elements, "US #167060 - MMSILVA - 25.02.2025
      t_zlest0032_ped      TYPE TABLE OF ty_zlest0032, "US #167060 - MMSILVA - 25.02.2025
      t_zib_contabil_chv   TYPE TABLE OF ty_zib_contabil_chv, "US #167060 - MMSILVA - 25.02.2025
      t_bkpf_ped           TYPE TABLE OF ty_bkpf_ped, "US #167060 - MMSILVA - 25.02.2025
      t_acdoca             TYPE TABLE OF ty_acdoca. "US #167060 - MMSILVA - 25.02.2025





*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA: wa_vttk               TYPE ty_vttk,
      wa_coleta             TYPE ty_coleta,
      wa_vtpa               TYPE ty_vtpa,
      wa_rem_lif            TYPE ty_remetente,
      wa_parc_lif           TYPE ty_parceiro,
      wa_parc_kun           TYPE ty_parceiro,
      wa_rem_kun            TYPE ty_remetente,
      wa_rem_lst            TYPE ty_remetente,
      wa_t001w              TYPE ty_t001w,
      wa_j_1bnfstx          TYPE j_1bnfstx,
      wa_konv               TYPE ty_konv,
      wa_vfsi               TYPE vfsi,
      wa_vfkp               TYPE ty_vfkp,
      wa_tvtkt              TYPE tvtkt,
      wa_vbak               TYPE ty_vbak,
      wa_vbpa               TYPE ty_vbpa,
      wa_vbrp               TYPE ty_vbrp,
      wa_ekbe               TYPE ty_ekbe,
      wa_saida              TYPE ty_saida,
      wa_rbkp               TYPE ty_rbkp,
      wa_bkpf               TYPE ty_bkpf,
      wa_zlest0002          TYPE ty_zlest0002,
      wa_zlest0032          TYPE zlest0032,
      wa_j_1bnflin          TYPE j_1bnflin,
      wa_lips               TYPE lips,
      wa_j_1bbranch         TYPE  j_1bbranch,
      wa_j_1bbranch_aux     TYPE  j_1bbranch,
      wa_bseg               TYPE bseg,
      wa_j_1bnfe_active_cte TYPE ty_j_1bnfe_active_cte,
      wa_j_1bnfe_active_nfe TYPE ty_j_1bnfe_active_nfe,
      wa_vfkp_ped           TYPE ty_vfkp, "US #167060 - MMSILVA - 25.02.2025
      wa_prcd_elements      TYPE ty_prcd_elements, "US #167060 - MMSILVA - 25.02.2025
      wa_zlest0032_ped      TYPE ty_zlest0032, "US #167060 - MMSILVA - 25.02.2025
      wa_zib_contabil_chv   TYPE ty_zib_contabil_chv, "US #167060 - MMSILVA - 25.02.2025
      wa_bkpf_ped           TYPE ty_bkpf_ped, "US #167060 - MMSILVA - 25.02.2025
      wa_acdoca             TYPE ty_acdoca. "US #167060 - MMSILVA - 25.02.2025

*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*

DATA: x_data   TYPE d,
      x_hora   TYPE sy-uzeit,
      vg_tabix TYPE sy-tabix.


RANGES: rcontas FOR rbco-saknr. " User Story 144142 // MMSILVA - 01.10.2024

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*

DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.

DATA: w_variant TYPE disvariant,
      w_layout  TYPE slis_layout_alv.
*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*PARAMETER: p_bukrs  TYPE vfkp-bukrs OBLIGATORY.
*           P_TDLNR  TYPE VTTK-TDLNR OBLIGATORY.
  SELECT-OPTIONS: "P_BUDAT  FOR  RBKP-BUDAT NO-EXTENSION OBLIGATORY,
                  p_bukrs  FOR  vfkp-bukrs OBLIGATORY, "RJF
                  p_werks  FOR  vfkp-werks, " NO INTERVALS NO-EXTENSION,
                  p_erdat  FOR  vttk-erdat NO-EXTENSION,
                  s_budat  FOR  rbkp-budat,
                  p_tdlnr  FOR  vttk-tdlnr,
                  p_shtyp  FOR  vttk-shtyp NO INTERVALS NO-EXTENSION,
                  p_tknum  FOR  vttk-tknum NO INTERVALS NO-EXTENSION,
                  p_chav_c FOR zib_cte_dist_n55-cd_chave_cte, "Inicio ajuste USER STORY 158497 / AOENNING.
                  p_chav_n FOR zib_cte_dist_n55-n55_chave_acesso, "Inicio ajuste USER STORY 158497 / AOENNING.
                  p_cod_su FOR lfa1-lifnr. "Inicio ajuste USER STORY 158497 / AOENNING.


  " US #167060 - MMSILVA - 25.02.2025 - Inicio
  SELECTION-SCREEN BEGIN OF BLOCK b3.
    PARAMETERS: p_ped AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK b3.
  " US #167060 - MMSILVA - 25.02.2025 - Fim


  "p_add02  for  vttk-add02 no intervals no-extension.
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-009.
    PARAMETERS: p_layout      TYPE disvariant-variant MODIF ID t1 DEFAULT '/STD'. "Layout
  SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN: END OF BLOCK b1.

"&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
PERFORM f_iniciar_variaves.

IF p_erdat[] IS NOT INITIAL OR s_budat[] IS NOT INITIAL.

  PERFORM f_seleciona_dados.
  PERFORM f_organiza_dados.
  PERFORM f_imprime_dados.

ELSE.

  MESSAGE s000(z01) WITH 'Campo data inválido.' 'Informar Data Registro ou Data Miro'
        DISPLAY LIKE 'E'.
  RETURN.
ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*

FORM f_seleciona_dados .
  DATA: t_vttk_aux   TYPE TABLE OF ty_vttk WITH HEADER LINE,
        t_vttk_aux2  TYPE TABLE OF ty_vttk_aux,
        t_vbrp_aux   TYPE TABLE OF ty_vbrp,
        t_vbpa_aux   TYPE TABLE OF ty_vbpa,
        t_t001w_aux  TYPE TABLE OF ty_t001w,
        t_vtpa_aux   TYPE TABLE OF ty_vtpa,
        wa_vttk_aux2 TYPE ty_vttk_aux.

  DATA r_bukrs TYPE RANGE OF bukrs.


  TYPES: BEGIN OF ty_vt,
           tdlnr TYPE vttk-tdlnr,
           tplst TYPE vttk-tplst,
           exti1 TYPE vttk-exti1,
           shtyp TYPE vttk-shtyp,
           tknum TYPE vttk-tknum,
           text1 TYPE vttk-text1,
           add02 TYPE vttk-add02,
           add03 TYPE vttk-add03,
         END OF ty_vt.

  TYPES: BEGIN OF ty_vf,
           fknum TYPE vfkp-fknum,
           fkpos TYPE vfkp-fkpos,
           bukrs TYPE vfkp-bukrs,
           tdlnr TYPE vfkp-tdlnr,
           werks TYPE vfkp-werks,
           budat TYPE vfkp-budat,
           rebel TYPE vfkp-rebel,
         END OF ty_vf.

  TYPES: BEGIN OF ty_vp,
           tknum TYPE vttp-tknum,
           vbeln TYPE vttp-vbeln,
         END OF ty_vp.

  TYPES: BEGIN OF ty_z,
           tknum       TYPE zlest0032-tknum,
           fknum       TYPE zlest0032-fknum,
           belnr       TYPE zlest0032-belnr,
           obj_key_sub TYPE zlest0032-obj_key_sub,
           gjahr       TYPE zlest0032-gjahr,
           data        TYPE zlest0032-data,
         END OF ty_z.

  DATA: lt_vt TYPE TABLE OF ty_vt.
  DATA: lt_vf TYPE TABLE OF ty_vf.
  DATA: lt_vp TYPE TABLE OF ty_vp.
  DATA: lt_z  TYPE TABLE OF ty_z.

  DATA: ls_vt TYPE  ty_vt.
  DATA: ls_vf TYPE  ty_vf.
  DATA: ls_vp TYPE  ty_vp.
  DATA: ls_z  TYPE  ty_z.

  FIELD-SYMBOLS: <fs_vbrp> TYPE ty_vbrp,
                 <fs_vttk> TYPE ty_vttk.

  CLEAR: t_bsis[], t_rbco[].

*  " Cabeçalho doc.da fatura recebida - Victor

  IF s_budat IS NOT INITIAL.

    SELECT  rb~bukrs rb~belnr rb~gjahr rb~rmwwr rb~budat
      FROM rbkp AS rb
      INNER JOIN rseg AS rs ON rs~belnr EQ rb~belnr
                         AND   rs~gjahr EQ rb~gjahr
      INTO TABLE t_rbkp
*    WHERE rb~bukrs EQ p_bukrs
    WHERE rb~bukrs IN p_bukrs
    AND   rb~budat IN s_budat
    AND   rs~xblnr NE ''
    AND   rb~stblg EQ space.

    CHECK t_rbkp[] IS NOT INITIAL.

    SORT t_rbkp BY belnr gjahr.
    DELETE ADJACENT DUPLICATES FROM t_rbkp COMPARING  belnr gjahr.

*    SELECT vt~tdlnr,
*           vt~tplst,
*       "    VT~ERDAT
*           vt~exti1,
*           vt~shtyp,
*           vt~tknum,
*           vt~text1,
*           vt~add02,"pf = 0000000003
*           vt~add03, "pf = 0000000003
*           z~fknum,  "doc custo
*           z~belnr,
*           z~obj_key_sub,
*           vf~bukrs,
*           vf~werks,
*           vf~budat,
*           vp~vbeln,
*           z~gjahr,
*           z~data
*    FROM vttk AS vt
*  INNER JOIN vttp AS vp ON vp~tknum EQ vt~tknum
*  INNER JOIN vfkp AS vf ON vf~rebel EQ vt~tknum
*  LEFT JOIN zlest0032 AS z ON z~tknum EQ vt~tknum
*    INTO TABLE @t_vttk
*  FOR ALL ENTRIES IN @t_rbkp_aux
*    WHERE vf~bukrs EQ @p_bukrs
*      AND vf~tdlnr IN @p_tdlnr
*      AND vf~werks IN @p_werks
*      AND vf~budat IN @p_erdat
*      AND belnr EQ @t_rbkp_aux-belnr
*      AND vt~shtyp IN @p_shtyp
*      AND vt~tknum IN @p_tknum.

*    GROUP BY vt~tdlnr
*           vt~tplst
*        "   VT~ERDAT
*           vt~exti1
*           vt~shtyp
*           vt~tknum
*           vt~text1
*           vt~add02 "pf = 0000000003
*           vt~add03 "pf = 0000000003
*           z~fknum  "doc custo
*           z~belnr
*           z~obj_key_sub
*           vf~bukrs
*           vf~werks
*           vf~budat
*           vp~vbeln
*           z~gjahr
*           z~data.

    SELECT z~tknum
           z~fknum
           z~belnr
           z~obj_key_sub
           z~gjahr
           z~data
    FROM zlest0032 AS z
      INTO TABLE lt_z
      FOR ALL ENTRIES IN t_rbkp
      WHERE z~belnr = t_rbkp-belnr.

    SORT  lt_z BY tknum.

    CHECK lt_z[] IS NOT INITIAL.

    SELECT vf~fknum
           vf~fkpos
           vf~bukrs
           vf~tdlnr
           vf~werks
           vf~budat
           vf~rebel
    FROM vfkp AS vf
      INTO TABLE lt_vf
       FOR ALL ENTRIES IN lt_z
        WHERE vf~rebel = lt_z-tknum
*        AND vf~bukrs EQ p_bukrs
        AND vf~bukrs IN p_bukrs "RJF
        AND vf~tdlnr IN p_tdlnr
        AND vf~werks IN p_werks
        AND vf~budat IN p_erdat
        AND vf~rebel IN p_tknum.

    SORT  lt_vf BY rebel.

    IF  lt_vf[] IS NOT INITIAL.

      SELECT vt~tdlnr
             vt~tplst
             vt~exti1
             vt~shtyp
             vt~tknum
             vt~text1
             vt~add02 "pf = 0000000003
             vt~add03 "pf = 0000000003
      FROM vttk AS vt
        INTO TABLE lt_vt
      FOR ALL ENTRIES IN lt_vf
        WHERE vt~tknum = lt_vf-rebel
          AND vt~shtyp IN p_shtyp.

      IF lt_vt[] IS NOT INITIAL.

        SORT lt_vt BY tknum.

        SELECT vp~tknum
               vp~vbeln
        FROM vttp AS vp
          INTO TABLE lt_vp
          FOR ALL ENTRIES IN lt_vt
          WHERE vp~tknum = lt_vt-tknum.

        SORT  lt_vp BY tknum vbeln.
        DELETE ADJACENT DUPLICATES FROM lt_vp COMPARING ALL FIELDS.
        SORT lt_vp BY tknum.

      ENDIF.

      LOOP AT lt_vf INTO ls_vf.
        READ TABLE  lt_vt INTO ls_vt WITH  KEY tknum = ls_vf-rebel BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_vt TO wa_vttk.
          wa_vttk-bukrs = ls_vf-bukrs.
          wa_vttk-werks = ls_vf-werks.
          wa_vttk-budat = ls_vf-budat.
          READ TABLE lt_vp TRANSPORTING NO FIELDS WITH KEY tknum = ls_vt-tknum BINARY SEARCH.
          IF sy-subrc = 0.
            LOOP AT lt_vp INTO ls_vp FROM sy-tabix.
              IF ls_vp-tknum NE ls_vt-tknum.
                EXIT.
              ENDIF.
              wa_vttk-vbeln = ls_vp-vbeln.
              READ TABLE lt_z TRANSPORTING NO FIELDS WITH KEY tknum = ls_vp-tknum BINARY SEARCH.
              IF sy-subrc = 0.
                LOOP AT  lt_z INTO ls_z FROM sy-tabix.
                  IF ls_z-tknum NE ls_vp-tknum.
                    EXIT.
                  ENDIF.
                  wa_vttk-fknum       = ls_z-fknum      .
                  wa_vttk-belnr       = ls_z-belnr      .
                  wa_vttk-obj_key_sub = ls_z-obj_key_sub.
                  wa_vttk-gjahr       = ls_z-gjahr      .
                  wa_vttk-data        = ls_z-data       .
                  APPEND  wa_vttk TO t_vttk.

                ENDLOOP.
              ELSE.
                APPEND  wa_vttk TO t_vttk.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
        CLEAR wa_vttk.
      ENDLOOP.


      SORT t_vttk                                   BY tdlnr tplst exti1 shtyp tknum text1 add02 add03 fknum belnr obj_key_sub bukrs werks budat.
      DELETE ADJACENT DUPLICATES FROM t_vttk COMPARING tdlnr tplst exti1 shtyp tknum text1 add02 add03 fknum belnr obj_key_sub bukrs werks budat.
    ENDIF.
  ELSE.

    SELECT vf~fknum
           vf~fkpos
           vf~bukrs
           vf~tdlnr
           vf~werks
           vf~budat
           vf~rebel
    FROM vfkp AS vf
      INTO TABLE lt_vf
*        WHERE vf~bukrs EQ p_bukrs
       WHERE vf~bukrs IN p_bukrs "RJF
        AND vf~tdlnr IN p_tdlnr
        AND vf~werks IN p_werks
        AND vf~budat IN p_erdat
        AND vf~rebel IN p_tknum.

    SORT  lt_vf BY rebel.


    IF  lt_vf[] IS NOT INITIAL.

      SELECT vt~tdlnr
             vt~tplst
             vt~exti1
             vt~shtyp
             vt~tknum
             vt~text1
             vt~add02 "pf = 0000000003
             vt~add03 "pf = 0000000003
      FROM vttk AS vt
        INTO TABLE lt_vt
      FOR ALL ENTRIES IN lt_vf
        WHERE vt~tknum = lt_vf-rebel
          AND vt~shtyp IN p_shtyp.

      IF lt_vt[] IS NOT INITIAL.

        SORT lt_vt BY tknum.

        SELECT vp~tknum
               vp~vbeln
        FROM vttp AS vp
          INTO TABLE lt_vp
          FOR ALL ENTRIES IN lt_vt
          WHERE vp~tknum = lt_vt-tknum.

        SORT  lt_vp BY tknum vbeln.
        DELETE ADJACENT DUPLICATES FROM lt_vp COMPARING ALL FIELDS.
        SORT lt_vp BY tknum.

        SELECT z~tknum
               z~fknum
               z~belnr
               z~obj_key_sub
               z~gjahr
               z~data
        FROM zlest0032 AS z
          INTO TABLE lt_z
          FOR ALL ENTRIES IN lt_vt
          WHERE z~tknum = lt_vt-tknum.

        SORT  lt_z BY tknum.
      ENDIF.

      LOOP AT lt_vf INTO ls_vf.
        READ TABLE  lt_vt INTO ls_vt WITH  KEY tknum = ls_vf-rebel BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_vt TO wa_vttk.
          wa_vttk-bukrs = ls_vf-bukrs.
          wa_vttk-werks = ls_vf-werks.
          wa_vttk-budat = ls_vf-budat.
          READ TABLE lt_vp TRANSPORTING NO FIELDS WITH KEY tknum = ls_vt-tknum BINARY SEARCH.
          IF sy-subrc = 0.
            LOOP AT lt_vp INTO ls_vp FROM sy-tabix.
              IF ls_vp-tknum NE ls_vt-tknum.
                EXIT.
              ENDIF.
              wa_vttk-vbeln = ls_vp-vbeln.
              READ TABLE lt_z TRANSPORTING NO FIELDS WITH KEY tknum = ls_vp-tknum BINARY SEARCH.
              IF sy-subrc = 0.
                LOOP AT  lt_z INTO ls_z FROM sy-tabix.
                  IF ls_z-tknum NE ls_vp-tknum.
                    EXIT.
                  ENDIF.
                  wa_vttk-fknum       = ls_z-fknum      .
                  wa_vttk-belnr       = ls_z-belnr      .
                  wa_vttk-obj_key_sub = ls_z-obj_key_sub.
                  wa_vttk-gjahr       = ls_z-gjahr      .
                  wa_vttk-data        = ls_z-data       .
                  APPEND  wa_vttk TO t_vttk.

                ENDLOOP.
              ELSE.
                APPEND  wa_vttk TO t_vttk.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
        CLEAR wa_vttk.
      ENDLOOP.

    ENDIF.
**  " Cabeçalho doc.da fatura recebida - Victor
*    SELECT vt~tdlnr
*           vt~tplst
*       "    VT~ERDAT
*           vt~exti1
*           vt~shtyp
*           vt~tknum
*           vt~text1
*           vt~add02 "pf = 0000000003
*           vt~add03 "pf = 0000000003
*           z~fknum  "doc custo
*           z~belnr
*           z~obj_key_sub
*           vf~bukrs
*           vf~werks
*           vf~budat
*           vp~vbeln
*           z~gjahr
*           z~data
*    FROM vttk AS vt
*    INNER JOIN vttp AS vp ON vp~tknum EQ vt~tknum
*    INNER JOIN vfkp AS vf ON vf~rebel EQ vt~tknum
*    LEFT JOIN zlest0032 AS z ON z~tknum EQ vt~tknum
*      INTO TABLE t_vttk
*    WHERE vf~bukrs EQ p_bukrs
*      AND vf~tdlnr IN p_tdlnr
*      AND vf~werks IN p_werks
*      AND vf~budat IN p_erdat
*      "AND VT~ERDAT IN P_ERDAT
*      AND vt~shtyp IN p_shtyp
*      AND vt~tknum IN p_tknum
*      "and vt~add02 in p_add02
*    GROUP BY vt~tdlnr
*           vt~tplst
*        "   VT~ERDAT
*           vt~exti1
*           vt~shtyp
*           vt~tknum
*           vt~text1
*           vt~add02 "pf = 0000000003
*           vt~add03 "pf = 0000000003
*           z~fknum  "doc custo
*           z~belnr
*           z~obj_key_sub
*           vf~bukrs
*           vf~werks
*           vf~budat
*           vp~vbeln
*           z~gjahr
*           z~data.

    SORT t_vttk                                   BY tdlnr tplst exti1 shtyp tknum text1 add02 add03 fknum belnr obj_key_sub bukrs werks budat.
    DELETE ADJACENT DUPLICATES FROM t_vttk COMPARING tdlnr tplst exti1 shtyp tknum text1 add02 add03 fknum belnr obj_key_sub bukrs werks budat.

    IF t_vttk[] IS NOT INITIAL.

      SELECT bukrs belnr gjahr rmwwr budat
        FROM rbkp
        INTO TABLE t_rbkp
        FOR ALL ENTRIES IN t_vttk
      WHERE belnr EQ t_vttk-belnr
        AND budat IN s_budat.

    ENDIF.
  ENDIF.

  CHECK t_vttk[] IS NOT INITIAL.

  LOOP AT t_vttk ASSIGNING <fs_vttk>.
    <fs_vttk>-branch          = <fs_vttk>-tdlnr+6(4).
    <fs_vttk>-obj_key_sub_aux = <fs_vttk>-obj_key_sub.
  ENDLOOP.
  UNASSIGN <fs_vttk>.

  "Custos de frete (Item)
  SELECT rebel
         bukrs
         werks
         knumv
         ebeln
         lblni
         fkpty
  FROM vfkp
    INTO TABLE t_vfkp
    FOR ALL ENTRIES IN t_vttk
  WHERE rebel EQ  t_vttk-tknum.

  SORT t_vfkp  BY fkpty.
  DELETE t_vfkp WHERE fkpty = 'Z003'.

  t_vttk_aux[] = t_vttk[].
  SORT t_vttk_aux BY vbeln.
  DELETE ADJACENT DUPLICATES FROM t_vttk_aux COMPARING vbeln.
  DELETE t_vttk_aux WHERE vbeln IS INITIAL.
  IF t_vttk_aux[] IS NOT INITIAL.
    SELECT * FROM lips
      INTO TABLE t_lips
      FOR ALL ENTRIES IN t_vttk
    WHERE vbeln EQ t_vttk-vbeln.
  ENDIF.
  IF t_lips[] IS NOT INITIAL.
    SELECT *
      FROM j_1bbranch
      INTO TABLE t_j_1bbranch
       FOR ALL ENTRIES IN t_lips
   WHERE branch EQ t_lips-werks.
  ENDIF.

  SELECT *
    FROM j_1bbranch
    APPENDING TABLE t_j_1bbranch
     FOR ALL ENTRIES IN t_vttk
 WHERE branch EQ t_vttk-branch.

*  SELECT * FROM BSEG
*    INTO TABLE T_BSEG
*    FOR ALL ENTRIES IN T_VTTK
* WHERE BELNR EQ T_VTTK-OBJ_KEY_SUB_AUX
*   AND BUZEI EQ '001'
*   AND BSCHL EQ '31'.

*=============== inicio ==================
*
*  " Histórico para o documento de compra - Victor
*  SELECT EBELN VGABE LFBNR BELNR GJAHR
*    FROM EKBE
*    INTO TABLE T_EKBE
*    FOR ALL ENTRIES IN T_VFKP
*  WHERE EBELN EQ T_VFKP-EBELN
*    AND LFBNR EQ T_VFKP-LBLNI
*    AND VGABE EQ '2'.
*

*  " Cabeçalho doc.da fatura recebida - Victor
  IF p_erdat IS NOT INITIAL.

    SELECT bukrs belnr gjahr rmwwr budat
      FROM rbkp
      INTO TABLE t_rbkp
      FOR ALL ENTRIES IN t_vttk
    WHERE belnr EQ t_vttk-belnr
      AND budat IN s_budat.

  ENDIF.

  IF t_rbkp[] IS NOT INITIAL.

    LOOP AT t_rbkp INTO wa_rbkp.
      vg_tabix = sy-tabix.
      CONCATENATE wa_rbkp-belnr wa_rbkp-gjahr INTO wa_rbkp-awkey.
      MODIFY t_rbkp INDEX vg_tabix FROM wa_rbkp TRANSPORTING awkey.
    ENDLOOP.

    SELECT bukrs gjahr awkey belnr
      FROM bkpf
      INTO TABLE t_bkpf
      FOR ALL ENTRIES IN  t_rbkp
      WHERE bukrs = t_rbkp-bukrs
      AND   gjahr = t_rbkp-gjahr
      AND   awkey = t_rbkp-awkey.

  ENDIF.


  CLEAR: rcontas[].
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '0000113214' ) TO rcontas.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '0000113216' ) TO rcontas.

  SELECT belnr, gjahr, buzei, cobl_nr, wrbtr, saknr
    INTO TABLE @t_rbco
    FROM rbco
     FOR ALL ENTRIES IN @t_vttk
   WHERE belnr EQ @t_vttk-belnr
     AND gjahr EQ @t_vttk-gjahr
     AND saknr IN @rcontas.

  DATA(t_vttk_sub) = t_vttk[].
  DELETE t_vttk_sub WHERE obj_key_sub EQ space.



  IF t_vttk_sub[] IS NOT INITIAL.

    LOOP AT t_vttk_sub ASSIGNING FIELD-SYMBOL(<fs_aux>).
      <fs_aux>-belnr = <fs_aux>-obj_key_sub.
      <fs_aux>-gjahr = <fs_aux>-data(4).
    ENDLOOP.

    "Alteração referente ajsute ZFIS016 - IR046327
    IF t_vttk_sub IS NOT INITIAL.
      SELECT * FROM j_1bbranch
      INTO TABLE @DATA(t_j_1bbranch)
      FOR ALL ENTRIES IN @t_vttk_sub
      WHERE branch EQ @t_vttk_sub-branch.
      r_bukrs = VALUE #( FOR ls IN t_j_1bbranch ( sign = 'I' option = 'EQ' low = ls-bukrs ) ).
    ENDIF.
    "====================================================================================================

    SELECT bukrs, hkont, augdt, augbl, zuonr, gjahr, belnr, buzei, dmbtr
      INTO TABLE @t_bsis
      FROM bsis
       FOR ALL ENTRIES IN @t_vttk_sub
     WHERE belnr EQ @t_vttk_sub-belnr
       AND gjahr EQ @t_vttk_sub-gjahr
       AND hkont IN @rcontas
       AND bukrs IN @r_bukrs.


*    SELECT BUKRS, HKONT, AUGDT, AUGBL, ZUONR, GJAHR, BELNR, BUZEI, DMBTR APPENDING TABLE @T_BSIS
*      FROM BSAS
*       FOR ALL ENTRIES IN @T_VTTK_SUB
*     WHERE BELNR EQ @T_VTTK_SUB-BELNR
*       AND GJAHR EQ @T_VTTK_SUB-GJAHR
*       AND HKONT IN @RCONTAS.

  ENDIF.

*  " Cabeçalho doc.da fatura recebida - Victor
*  SELECT rb~bukrs rb~belnr rb~gjahr rb~rmwwr rs~xblnr rb~budat rb~stblg
*    FROM rbkp AS rb
*    INNER JOIN rseg AS rs ON rs~belnr EQ rb~belnr
*                       AND   rs~gjahr EQ rb~gjahr
*    INTO TABLE t_rbkp
*  WHERE rb~bukrs EQ p_bukrs
*  AND   rb~budat IN p_budat
*  AND   rs~xblnr NE ''
*  AND   rb~stblg EQ space.
*
*  CHECK t_rbkp[] IS NOT INITIAL.
*  SORT t_rbkp BY belnr gjahr.
*  DELETE ADJACENT DUPLICATES FROM t_rbkp COMPARING  belnr gjahr.
*
*  " Histórico para o documento de compra - Victor
*  SELECT ebeln vgabe lfbnr belnr gjahr
*    FROM ekbe
*    INTO TABLE t_ekbe
*    FOR ALL ENTRIES IN t_rbkp
*  WHERE  belnr EQ t_rbkp-belnr
*  AND gjahr    EQ t_rbkp-gjahr
*  AND vgabe    EQ '2'.
*
*  CHECK t_ekbe[] IS NOT INITIAL.
*
*  t_rbkp_aux[] = t_rbkp[].
*  SORT  t_rbkp_aux BY xblnr.
*  DELETE ADJACENT DUPLICATES FROM t_rbkp_aux COMPARING xblnr.

*  SELECT vt~tdlnr
*       vt~tplst
*       vt~erdat
*       vt~exti1
*       vt~shtyp
*       vt~tknum
*       vt~text1
*       z~fknum  "doc custo
*       vf~bukrs
*       vf~werks
*FROM vttk AS vt
*INNER JOIN vttp AS vp ON vp~tknum EQ vt~tknum
*INNER JOIN vfkp AS vf ON vf~rebel EQ vt~tknum
*LEFT JOIN zlest0032 AS z ON z~tknum EQ vt~tknum
*  INTO TABLE t_vttk
*FOR ALL ENTRIES IN t_rbkp_aux
*WHERE vf~bukrs EQ p_bukrs
*  AND vf~tdlnr EQ p_tdlnr
*  AND vt~tknum EQ t_rbkp_aux-xblnr
*  AND vf~werks IN p_werks
*  AND vt~shtyp IN p_shtyp
*  AND vt~tknum IN p_tknum
*
*GROUP BY vt~tdlnr
*       vt~tplst
*       vt~erdat
*       vt~exti1
*       vt~shtyp
*       vt~tknum
*       vt~add02 "pf = 0000000003
*       vt~text1
*       z~fknum  "doc custo
*       vf~bukrs
*       vf~werks.
*=============== FIM ==================

  SELECT pc_veiculo cd_cidade cd_uf
    FROM zlest0002
    INTO TABLE t_zlest0002
    FOR ALL ENTRIES IN t_vttk
  WHERE pc_veiculo EQ t_vttk-text1(7).

  "Custos de frete (Item)
  SELECT rebel
         bukrs
         werks
         knumv
         ebeln
         lblni
         fkpty
  FROM vfkp
    INTO TABLE t_vfkp
    FOR ALL ENTRIES IN t_vttk
  WHERE rebel EQ  t_vttk-tknum.

  SORT t_vfkp  BY fkpty.
  DELETE t_vfkp WHERE fkpty = 'Z003'.

  "Custos de frete (Sub Item)
  IF t_vfkp[] IS NOT INITIAL.
    SELECT *
      FROM vfsi
      INTO TABLE t_vfsi
      FOR ALL ENTRIES IN t_vfkp
      WHERE knumv EQ t_vfkp-knumv.

    SORT t_vfsi BY activ.
    DELETE t_vfsi WHERE activ NE 'X'.
  ENDIF.

  "Tipos de transporte
  SELECT *
    FROM tvtkt
    INTO TABLE t_tvtkt
    FOR ALL ENTRIES IN t_vttk
    WHERE shtyp EQ t_vttk-shtyp.

  t_vttk_aux[] = t_vttk[].
  SORT t_vttk_aux BY tknum.

  DELETE ADJACENT DUPLICATES FROM t_vttk_aux COMPARING tknum.

  IF t_vttk_aux[] IS NOT INITIAL.
    "Documento de vendas
    SELECT vbeln tknum
      FROM  vbak
      INTO TABLE t_vbak
      FOR ALL ENTRIES IN t_vttk_aux
      WHERE tknum EQ t_vttk_aux-tknum.
  ENDIF.

  IF t_vbak[] IS NOT INITIAL.
    "Fatura
    SELECT vbeln posnr vgbel netwr
      FROM vbrp
      INTO TABLE t_vbrp
      FOR ALL ENTRIES IN t_vbak
      WHERE vgbel EQ t_vbak-vbeln AND draft = space .

    LOOP AT t_vbrp ASSIGNING <fs_vbrp>.
      <fs_vbrp>-vbeln_ref = <fs_vbrp>-vbeln.
    ENDLOOP.

  ENDIF.

  IF t_vbrp[] IS NOT INITIAL.
    SELECT * FROM j_1bnflin
      INTO TABLE t_j_1bnflin
      FOR ALL ENTRIES IN t_vbrp
    WHERE refkey EQ t_vbrp-vbeln_ref.
  ENDIF.

  IF t_j_1bnflin[] IS NOT INITIAL.
    SELECT
      docnum
      regio
      nfyear
      nfmonth
      stcd1
      model
      serie
      nfnum9
      docnum9
      cdv
    FROM j_1bnfe_active
    INTO TABLE t_j_1bnfe_active_cte
    FOR ALL ENTRIES IN t_j_1bnflin
    WHERE docnum EQ t_j_1bnflin-docnum.
  ENDIF.

  "Fluxo
  IF t_vttk[] IS NOT INITIAL.
*    SELECT VBELN VBTYP_N VBTYP_V VBELV
*      FROM VBFA
*      INTO TABLE T_VBFA
*      FOR ALL ENTRIES IN T_VTTK
*      WHERE VBELN EQ T_VTTK-TKNUM
*       AND ( VBTYP_N EQ '8'
*         AND VBTYP_V EQ 'J' )
*        OR ( VBTYP_N EQ '8'
*         AND VBTYP_V EQ '7' ).

    SELECT vbeln vbtyp_n vbtyp_v vbelv
    FROM vbfa
    INTO TABLE t_vbfa
    FOR ALL ENTRIES IN t_vttk
    WHERE vbeln EQ t_vttk-tknum.

    DELETE  t_vbfa WHERE  ( vbtyp_n NE '8' AND vbtyp_v NE 'J' )
                   AND    ( vbtyp_n NE '8' AND vbtyp_v NE '7' ).


    "IF sy-subrc IS INITIAL.
    IF t_vbfa[] IS NOT INITIAL.
      SELECT vbeln vbtyp_n vbtyp_v vbelv
        FROM vbfa
        INTO TABLE t_vbfa_aux_m
        FOR ALL ENTRIES IN t_vbfa
        WHERE vbelv EQ t_vbfa-vbelv.

      SORT t_vbfa_aux_m BY vbtyp_n.
      DELETE  t_vbfa_aux_m WHERE vbtyp_n NE 'M'.


      LOOP AT t_vbfa_aux_m.
        MOVE-CORRESPONDING: t_vbfa_aux_m TO t_vbfa_aux.
        MOVE: t_vbfa_aux_m-vbeln TO t_vbfa_aux-refkey.

        APPEND t_vbfa_aux.
        CLEAR: t_vbfa_aux.
      ENDLOOP.

      SELECT vbeln vbtyp_n vbtyp_v vbelv mjahr
        FROM vbfa
        APPENDING TABLE t_vbfa_aux_r
        FOR ALL ENTRIES IN t_vbfa
        WHERE vbelv EQ t_vbfa-vbelv.


      SORT t_vbfa_aux_r BY vbtyp_n.
      DELETE  t_vbfa_aux_r WHERE vbtyp_n NE 'R'.

      LOOP AT t_vbfa_aux_r.
        MOVE-CORRESPONDING: t_vbfa_aux_r TO t_vbfa_aux.
        CONCATENATE t_vbfa_aux_r-vbeln t_vbfa_aux_r-mjahr INTO t_vbfa_aux-refkey.

        APPEND t_vbfa_aux.
        CLEAR: t_vbfa_aux.
      ENDLOOP.

      IF t_vbfa_aux[] IS NOT INITIAL.
        SELECT docnum refkey netwr
          FROM j_1bnflin
          INTO TABLE t_bnflin
           FOR ALL ENTRIES IN t_vbfa_aux
            WHERE refkey EQ t_vbfa_aux-refkey.
      ENDIF.

      SELECT *
        FROM zlest0110 INTO TABLE t_zlest0110
         FOR ALL ENTRIES IN t_vbfa
         WHERE vbeln EQ t_vbfa-vbelv.

*-CS2021001045 - 15.02.2022 - JT - inicio
      IF sy-subrc <> 0.
        SELECT *
          FROM zlest0213 INTO TABLE @DATA(t_zlest0213)
           FOR ALL ENTRIES IN @t_vbfa
         WHERE vbeln EQ @t_vbfa-vbelv.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING t_zlest0213[] TO t_zlest0110[].
        ENDIF.
      ENDIF.
*-CS2021001045 - 15.02.2022 - JT - fim

      SELECT av_vbeln ft_belnr ft_gjahr
        FROM zmmt_ee_zgr_docs
        INTO TABLE t_zgr_docs
         FOR ALL ENTRIES IN t_vbfa
         WHERE av_vbeln EQ t_vbfa-vbelv.

      LOOP AT t_zgr_docs.
        CONCATENATE t_zgr_docs-ft_belnr t_zgr_docs-ft_gjahr INTO t_zgr_docs-refkey.
        MODIFY t_zgr_docs.
      ENDLOOP.

      IF t_zgr_docs[] IS NOT INITIAL.
        SELECT docnum refkey netwr
          FROM j_1bnflin
          APPENDING TABLE t_bnflin
           FOR ALL ENTRIES IN t_zgr_docs
            WHERE refkey EQ t_zgr_docs-refkey.
      ENDIF.

      IF t_bnflin[] IS NOT INITIAL.
        SELECT docnum nfenum series pstdat nfe nfnum
          FROM j_1bnfdoc
          INTO TABLE t_bnfdoc
           FOR ALL ENTRIES IN t_bnflin
           WHERE docnum EQ t_bnflin-docnum.

        SELECT
          docnum
          regio
          nfyear
          nfmonth
          stcd1
          model
          serie
          nfnum9
          docnum9
          cdv
        FROM j_1bnfe_active
        INTO TABLE t_j_1bnfe_active_nfe
        FOR ALL ENTRIES IN t_bnflin
        WHERE docnum EQ t_bnflin-docnum.


      ENDIF.
    ENDIF.

  ENDIF.


  t_vbrp_aux[] = t_vbrp.
  SORT t_vbrp_aux BY vbeln.
  DELETE ADJACENT DUPLICATES FROM t_vbrp_aux COMPARING vbeln.

  IF t_vbrp_aux[] IS NOT INITIAL.
    "------Parceiro da Fatura (Ponto de Coleta)
    SELECT lifnr vbeln parvw
      FROM vbpa
      INTO TABLE t_vbpa
      FOR ALL ENTRIES IN t_vbrp_aux
      WHERE vbeln EQ  t_vbrp_aux-vbeln.

    SORT  t_vbpa BY parvw.
    DELETE t_vbpa WHERE parvw NE 'PC'.
  ENDIF.


  t_vbpa_aux[] = t_vbpa.
  SORT t_vbpa_aux BY lifnr.
  DELETE ADJACENT DUPLICATES FROM t_vbpa_aux COMPARING lifnr.

  IF t_vbpa_aux[] IS NOT INITIAL.
    SELECT lifnr name1 ort01 regio
      FROM lfa1
      INTO TABLE t_coleta
      FOR ALL ENTRIES IN t_vbpa_aux
      WHERE lifnr EQ t_vbpa_aux-lifnr.
  ENDIF.


  t_vttk_aux[] = t_vttk[].
  SORT t_vttk_aux BY tplst.
  DELETE ADJACENT DUPLICATES FROM t_vttk_aux COMPARING tplst.

  IF t_vttk_aux[] IS NOT INITIAL.
    SELECT werks lifnr kunnr
      FROM t001w
      INTO TABLE t_t001w
      FOR ALL ENTRIES IN t_vttk_aux
      WHERE werks EQ t_vttk_aux-tplst.
  ENDIF.

  LOOP AT t_vttk_aux INTO wa_vttk.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_vttk-tplst
      IMPORTING
        output = wa_vttk_aux2-lifnr.

    APPEND wa_vttk_aux2 TO t_vttk_aux2.
  ENDLOOP.

  IF t_vttk_aux2[] IS NOT INITIAL.
    SELECT lifnr name1
      FROM lfa1
      INTO TABLE t_rem_lst
      FOR ALL ENTRIES IN t_vttk_aux2
      WHERE lifnr EQ t_vttk_aux2-lifnr.
  ENDIF.


  t_t001w_aux[] = t_t001w.
  SORT t_t001w_aux BY lifnr.
  DELETE ADJACENT DUPLICATES FROM t_t001w_aux COMPARING lifnr.

  IF t_t001w_aux[] IS NOT INITIAL.
    SELECT  lifnr name1
      FROM lfa1
      INTO TABLE t_rem_lif
      FOR ALL ENTRIES IN t_t001w_aux
      WHERE lifnr EQ t_t001w_aux-lifnr.
  ENDIF.


  t_t001w_aux[] = t_t001w.
  SORT t_t001w_aux BY kunnr.
  DELETE ADJACENT DUPLICATES FROM t_t001w_aux COMPARING kunnr.

  IF t_t001w_aux[] IS NOT INITIAL.
    SELECT kunnr name1
      FROM kna1
      INTO TABLE t_rem_kun
      FOR ALL ENTRIES IN t_t001w_aux
      WHERE kunnr EQ t_t001w_aux-kunnr.
  ENDIF.

  "Parceiro de transporte
  SELECT vbeln lifnr parvw kunnr
    FROM vtpa
    INTO TABLE t_vtpa
    FOR ALL ENTRIES IN t_vttk
    WHERE vbeln EQ t_vttk-tknum.


  t_vtpa_aux[] = t_vtpa[].
  SORT t_vtpa_aux BY lifnr.
  DELETE ADJACENT DUPLICATES FROM t_vtpa_aux COMPARING lifnr.

  IF t_vtpa_aux[] IS NOT INITIAL.
    SELECT  lifnr name1 stcd1 stcd2 ort01 regio stkzn crtn indtyp
      FROM lfa1
      INTO TABLE t_parc_lif
      FOR ALL ENTRIES IN t_vtpa_aux
      WHERE lifnr EQ t_vtpa_aux-lifnr.
  ENDIF.

* Busca Valores dos domínios
  IF t_parc_lif[] IS NOT INITIAL.

* Descrição de domínio
    SELECT * FROM dd07t
      INTO TABLE t_dd07t
      WHERE domname = 'J_1BCRTN'."Código de Regime Tributário - número

*Descrição de Tipos de Industrialização
    SELECT * FROM j_1btindtypt
      INTO TABLE t_j_1btindtypt
      WHERE spras = sy-langu.

  ENDIF.


  t_vtpa_aux[] = t_vtpa[].
  SORT t_vtpa_aux BY kunnr.
  DELETE ADJACENT DUPLICATES FROM t_vtpa_aux COMPARING kunnr.

  IF t_vtpa_aux[] IS NOT INITIAL.
    SELECT  kunnr name1 stcd1 stcd2 ort01 regio
      FROM kna1
      INTO TABLE t_parc_kun
      FOR ALL ENTRIES IN t_vtpa_aux
      WHERE kunnr EQ t_vtpa_aux-kunnr.
  ENDIF.

  "US #167060 - MMSILVA - 25.02.2025 - Inicio
  IF t_vttk[] IS NOT INITIAL.
    "Select Valor Pedágio
    SELECT rebel knumv
      FROM vfkp
      INTO TABLE t_vfkp_ped
      FOR ALL ENTRIES IN t_vttk
      WHERE rebel EQ t_vttk-tknum
      AND   fkpty NE 'Z003'.

    SORT t_vfkp_ped BY rebel. "US #171474 - MMSILVA - 07.05.2025

    IF t_vfkp_ped[] IS NOT INITIAL.
      SELECT knumv kposn kschl kwert
      FROM prcd_elements
      INTO TABLE t_prcd_elements
      FOR ALL ENTRIES IN t_vfkp_ped
      WHERE knumv EQ t_vfkp_ped-knumv
      AND   kposn EQ '000001'
      AND   kschl EQ 'ZPED'.

      SORT t_prcd_elements BY knumv kposn kschl. "US #171474 - MMSILVA - 07.05.2025
    ENDIF.

    "Select valor PIS/COFINS do Pedágio
    SELECT tknum obj_key_ped
      FROM zlest0032
      INTO TABLE t_zlest0032_ped
      FOR ALL ENTRIES IN t_vttk
      WHERE tknum EQ t_vttk-tknum.

    SORT t_zlest0032_ped BY tknum. "US #171474 - MMSILVA - 07.05.2025

    IF t_zlest0032_ped[] IS NOT INITIAL.
      SELECT belnr obj_key bukrs gjahr
        FROM zib_contabil_chv
        INTO TABLE t_zib_contabil_chv
        FOR ALL ENTRIES IN t_zlest0032_ped
        WHERE obj_key EQ t_zlest0032_ped-obj_key_ped.

      SORT t_zib_contabil_chv BY obj_key. "US #171474 - MMSILVA - 07.05.2025

      IF t_zib_contabil_chv[] IS NOT INITIAL.
        SELECT belnr bukrs gjahr stblg
          FROM bkpf
          INTO TABLE t_bkpf_ped
          FOR ALL ENTRIES IN t_zib_contabil_chv
          WHERE bukrs EQ t_zib_contabil_chv-bukrs
          AND   belnr EQ t_zib_contabil_chv-belnr
          AND   gjahr EQ t_zib_contabil_chv-gjahr
          AND   stblg EQ ''.

        SORT t_bkpf_ped BY belnr bukrs gjahr. "US #171474 - MMSILVA - 07.05.2025

        IF t_bkpf_ped[] IS NOT INITIAL.
          SELECT belnr rldnr rbukrs gjahr racct hsl
            FROM acdoca
            INTO TABLE t_acdoca
            FOR ALL ENTRIES IN t_zib_contabil_chv
            WHERE rldnr  EQ '0L'
            AND   rbukrs EQ t_zib_contabil_chv-bukrs
            AND   gjahr  EQ t_zib_contabil_chv-gjahr
            AND   belnr  EQ t_zib_contabil_chv-belnr
            AND   racct  IN ('0000113214', '0000113216').

          SORT t_acdoca BY belnr rldnr rbukrs gjahr. "US #171474 - MMSILVA - 07.05.2025

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  "US #167060 - MMSILVA - 25.02.2025 - Fim

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM f_organiza_dados .

  DATA: BEGIN OF it_vtrlp.
          INCLUDE STRUCTURE vtrlp.
  DATA: END OF it_vtrlp.

  DATA: lt_bseg TYPE TABLE OF bseg.

  DATA: wa_lfa1      TYPE lfa1,
        wa_kna1      TYPE kna1,
        wa_vtrlp     TYPE vtrlp,
        val_frete    TYPE vfsi-netwr,
        t_vtrlp      LIKE STANDARD TABLE OF it_vtrlp,
        vg_parceiro  TYPE j_1bparid,
        wa_t001w     TYPE ty_t001w,
        wa_info_part TYPE lfa1,
        wl_refkey    TYPE j_1bnflin-refkey.

  SORT: t_vttk      BY tknum,
        t_vfkp      BY rebel,
*        t_vfkp      BY ebeln lblni,
        t_t001w     BY werks,
        t_vfsi      BY knumv,
        t_tvtkt     BY shtyp,
        t_vbak      BY tknum,
        t_vbrp      BY vgbel,
        t_vbpa      BY vbeln,
        t_lfa1      BY lifnr,
        t_coleta    BY lifnr,
        t_rem_lif   BY lifnr,
        t_rem_kun   BY lifnr,
        t_rem_lst   BY lifnr,
        t_parc_lif  BY lifnr,
        t_parc_kun  BY lifnr,
        t_ekbe      BY belnr gjahr,
        t_rbkp      BY belnr gjahr,
        t_bkpf      BY bukrs gjahr awkey,
        t_dd07t     BY valpos,
        t_j_1btindtypt BY j_1bindtyp,
        t_j_1bnflin BY refkey,
        t_j_1bnfe_active_cte BY docnum,
        t_j_1bnfe_active_nfe BY docnum,
        t_zlest0032 BY tknum,
        t_vbfa      BY vbeln,
        t_vbfa_aux  BY vbelv vbtyp_n,
        t_bnflin    BY refkey,
        t_bnfdoc    BY docnum,
        t_zgr_docs  BY av_vbeln,
        t_zlest0110 BY vbeln,
        t_zlest0002 BY pc_veiculo,
        t_lips      BY vbeln,
        t_j_1bbranch  BY branch,
        t_rbco       BY belnr gjahr,
        t_vtpa       BY vbeln,
*        T_EKBE      BY EBELN LFBNR,
        t_bsis      BY belnr gjahr.


  IF t_vttk[] IS NOT INITIAL.
    t_vttk_aux[] =  t_vttk[].
    SORT t_vttk_aux  BY obj_key_sub_aux.
    DELETE t_vttk_aux WHERE obj_key_sub_aux IS INITIAL.
    IF t_vttk_aux[] IS NOT INITIAL.
      CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
        EXPORTING
          it_for_all_entries = t_vttk_aux[]
          i_where_clause     = |BELNR = IT_FOR_ALL_ENTRIES-OBJ_KEY_SUB_AUX AND BUZEI EQ '001' AND BSCHL EQ '31'|
        IMPORTING
          et_bseg            = lt_bseg
        EXCEPTIONS
          not_found          = 1.
      IF sy-subrc <> 0 OR lines( lt_bseg ) = 0.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ELSE.
        sy-dbcnt = lines( lt_bseg ).
      ENDIF.

      SORT lt_bseg BY bukrs belnr.
    ENDIF.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM t_rbkp COMPARING belnr.
  SORT t_rbkp BY belnr.

*    User Story 144142 // MMSILVA - 01.10.2024
  IF lt_bseg IS NOT INITIAL.
    SELECT belnr, gjahr, buzei, cobl_nr, wrbtr, saknr
    APPENDING CORRESPONDING FIELDS OF TABLE @t_rbco
    FROM rbco
     FOR ALL ENTRIES IN @lt_bseg
   WHERE belnr EQ @lt_bseg-belnr
     AND gjahr EQ @lt_bseg-gjahr
     AND saknr IN @rcontas.

*  DATA(lt_bseg_sub) = lt_bseg[].
*  DELETE lt_bseg_sub WHERE obj_key_sub EQ space.
  ENDIF.
*    User Story 144142 // MMSILVA - 01.10.2024

  LOOP AT t_vttk INTO wa_vttk.

    READ TABLE t_vttk TRANSPORTING NO FIELDS WITH KEY tknum = wa_vttk-tknum BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT t_vttk FROM  sy-tabix.
        IF t_vttk-tknum NE wa_vttk-tknum.
          EXIT.
        ENDIF.
        CALL FUNCTION 'RV_SHIPMENT_VIEW'
          EXPORTING
            shipment_number = wa_vttk-tknum
            language        = sy-langu
            option_packages = 'X'
          TABLES
            f_trlp          = t_vtrlp.

        READ TABLE t_vtrlp INTO wa_vtrlp INDEX 1.
        wa_saida-material   = wa_vtrlp-matnr.
        wa_saida-desc_mat   = wa_vtrlp-arktx.
        wa_saida-quantidade = wa_vtrlp-lfimg.

        READ TABLE t_vtpa TRANSPORTING NO FIELDS WITH KEY vbeln = wa_vttk-tknum BINARY SEARCH .
        IF sy-subrc = 0.
          LOOP AT t_vtpa INTO wa_vtpa FROM sy-tabix.
            IF wa_vtpa-vbeln NE wa_vttk-tknum .
              EXIT.
            ENDIF.
            CLEAR wa_lfa1.

            IF wa_vtpa-parvw EQ 'PV'.

              READ TABLE t_parc_lif INTO wa_parc_lif WITH KEY lifnr = wa_vtpa-lifnr BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                wa_saida-proprietario = wa_parc_lif-name1.
                wa_saida-cod_prop     = wa_parc_lif-lifnr.
                wa_saida-crtn         = wa_parc_lif-crtn.
                wa_saida-indtyp       = wa_parc_lif-indtyp.

                IF NOT wa_parc_lif-stcd1 IS INITIAL.
                  wa_saida-cpf_prod     = wa_parc_lif-stcd1.
                ELSE.
                  wa_saida-cpf_prod     = wa_parc_lif-stcd2.
                ENDIF.

                READ TABLE t_dd07t INTO DATA(w_dd07t) WITH KEY valpos = wa_saida-crtn BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  wa_saida-crtn_desc = w_dd07t-ddtext.
                ENDIF.

                READ TABLE t_j_1btindtypt INTO DATA(w_j_1btindtypt) WITH KEY j_1bindtyp = wa_saida-indtyp BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  wa_saida-indtyp_desc = w_j_1btindtypt-j_1bindtypx.
                ENDIF.

                IF wa_parc_lif-stkzn NE ''.
                  wa_saida-tp_pessoa = 'Física'.
                ELSE.
                  wa_saida-tp_pessoa = 'Jurídica'.
                ENDIF.

              ENDIF.
            ENDIF.

            IF wa_vtpa-parvw EQ 'LR'.

              IF wa_vtpa-kunnr NE ''.
                READ TABLE t_parc_kun INTO wa_parc_kun WITH KEY lifnr = wa_vtpa-kunnr BINARY SEARCH.
                wa_saida-cod_dest     = wa_parc_kun-lifnr.
                wa_saida-destinatario = wa_parc_kun-name1.
              ELSE.
                READ TABLE t_parc_lif INTO wa_parc_lif WITH KEY lifnr = wa_vtpa-lifnr BINARY SEARCH.
                wa_saida-cod_dest     = wa_parc_lif-lifnr.
                wa_saida-destinatario = wa_parc_lif-name1.
              ENDIF.

            ENDIF.

            IF wa_vtpa-parvw EQ 'PC' OR wa_vtpa-parvw EQ 'LR'.

              IF wa_vtpa-lifnr IS NOT INITIAL.

                CLEAR wa_lfa1.

                READ TABLE t_parc_lif INTO wa_parc_lif WITH KEY lifnr = wa_vtpa-lifnr BINARY SEARCH.

                IF wa_vtpa-parvw EQ 'PC'.
                  wa_saida-parc_coleta = wa_parc_lif-name1.
                  wa_saida-cid_coleta  = wa_parc_lif-ort01.
                  wa_saida-uf_coleta   = wa_parc_lif-regio.
                ELSE.
                  wa_saida-pont_entrega = wa_parc_lif-name1.
                  wa_saida-cid_entrega  = wa_parc_lif-ort01.
                  wa_saida-uf_entrega   = wa_parc_lif-regio.
                ENDIF.

              ELSE.
                READ TABLE t_parc_kun INTO wa_parc_kun WITH KEY lifnr = wa_vtpa-kunnr BINARY SEARCH.

                IF wa_vtpa-parvw EQ 'PC'.
                  wa_saida-parc_coleta = wa_parc_kun-name1.
                  wa_saida-cid_coleta  = wa_parc_kun-ort01.
                  wa_saida-uf_coleta   = wa_parc_kun-regio.
                ELSE.
                  wa_saida-pont_entrega = wa_parc_kun-name1.
                  wa_saida-cid_entrega  = wa_parc_kun-ort01.
                  wa_saida-uf_entrega   = wa_parc_kun-regio.
                ENDIF.
                " wa_saida-cod_dest     = WA_VTPA-lifnr.
              ENDIF.

            ENDIF.
            CLEAR : wa_vtpa, wa_parc_kun, wa_parc_lif.
          ENDLOOP.
        ENDIF.
        CLEAR wa_lfa1.

        READ TABLE t_t001w INTO wa_t001w  WITH KEY werks = wa_vttk-tplst BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          IF wa_t001w-lifnr IS NOT INITIAL.

            READ TABLE t_rem_lif INTO wa_rem_lif WITH KEY lifnr = wa_t001w-lifnr BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_rem_lif TO wa_info_part.
            ENDIF.

          ELSEIF wa_t001w-kunnr IS NOT INITIAL.

            READ TABLE t_rem_kun INTO wa_rem_kun WITH KEY lifnr = wa_t001w-kunnr BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_rem_kun TO wa_info_part.
            ENDIF.

          ELSE.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_vttk-tplst
              IMPORTING
                output = vg_parceiro.

            READ TABLE t_rem_lst INTO wa_rem_lst WITH KEY lifnr = vg_parceiro BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_rem_lst TO wa_info_part.
            ENDIF.

          ENDIF.
        ENDIF.

        wa_saida-remetente = wa_info_part-name1.

        READ TABLE t_vfkp INTO wa_vfkp WITH KEY rebel = wa_vttk-tknum BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-bukrs  = wa_vfkp-bukrs.
          wa_saida-werks  = wa_vfkp-werks.
        ENDIF.

*       US #167060 - MMSILVA - 25.02.2025 - Inicio
        READ TABLE t_vfkp_ped INTO DATA(wa_vfkp_ped) WITH KEY rebel = wa_vttk-tknum BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE t_prcd_elements INTO DATA(wa_prcd_elements) WITH KEY knumv = wa_vfkp_ped-knumv
                                                                          kposn = '000001'
                                                                          kschl = 'ZPED' BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            wa_saida-kwert = wa_prcd_elements-kwert.
          ENDIF.
        ENDIF.

        READ TABLE t_zlest0032_ped INTO wa_zlest0032_ped WITH KEY tknum = wa_vttk-tknum BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE t_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_zlest0032_ped-obj_key_ped BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            READ TABLE t_bkpf_ped INTO wa_bkpf_ped WITH KEY belnr = wa_zib_contabil_chv-belnr
                                                            bukrs = wa_zib_contabil_chv-bukrs
                                                            gjahr = wa_zib_contabil_chv-gjahr BINARY SEARCH.

            IF sy-subrc IS INITIAL.

              wa_saida-docnum_ped = wa_zib_contabil_chv-belnr.

              LOOP AT t_acdoca INTO wa_acdoca WHERE belnr  = wa_bkpf_ped-belnr
                                              AND   rldnr  = '0L'
                                              AND   rbukrs = wa_bkpf_ped-bukrs
                                              AND   gjahr  = wa_bkpf_ped-gjahr.

                IF sy-subrc IS INITIAL.
                  IF wa_acdoca-racct EQ '0000113214'.
                    wa_saida-pis_ped = wa_acdoca-hsl.
                  ENDIF.
                  IF wa_acdoca-racct EQ '0000113216'.
                    wa_saida-cofins_ped = wa_acdoca-hsl.
                  ENDIF.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
*       US #167060 - MMSILVA - 25.02.2025 - Fim

        READ TABLE t_rbkp INTO wa_rbkp WITH KEY belnr = wa_vttk-belnr BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-netwr  = wa_rbkp-rmwwr.
          wa_saida-belnr  = wa_rbkp-belnr.
          wa_saida-budat  = wa_rbkp-budat.
        ENDIF.

        READ TABLE t_bkpf INTO wa_bkpf WITH KEY   bukrs = wa_rbkp-bukrs
                                                  gjahr = wa_rbkp-gjahr
                                                  awkey = wa_rbkp-awkey BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-belnrb  = wa_bkpf-belnr.
        ENDIF.

        READ TABLE t_tvtkt INTO wa_tvtkt WITH KEY shtyp = wa_vttk-shtyp BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONCATENATE wa_vttk-shtyp  ' - ' wa_tvtkt-bezei(20) INTO wa_saida-shtyp.
        ENDIF.

        READ TABLE t_vbak INTO wa_vbak WITH KEY tknum = wa_vttk-tknum BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-ordem  = wa_vbak-vbeln.
        ENDIF.

        READ TABLE t_vbrp INTO wa_vbrp WITH KEY vgbel = wa_vbak-vbeln BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-fatura     = wa_vbrp-vbeln.
          wa_saida-vlr_fatura = wa_vbrp-netwr.
        ENDIF.


        READ TABLE t_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = wa_vbrp-vbeln_ref BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-docnum = wa_j_1bnflin-docnum.
        ENDIF.

        READ TABLE t_j_1bnfe_active_cte INTO wa_j_1bnfe_active_cte WITH KEY  docnum = wa_saida-docnum BINARY SEARCH.
        IF  sy-subrc IS INITIAL.
          wa_saida-chave_cte = |{ wa_j_1bnfe_active_cte-regio }{ wa_j_1bnfe_active_cte-nfyear }{ wa_j_1bnfe_active_cte-nfmonth }{ wa_j_1bnfe_active_cte-stcd1 }{ wa_j_1bnfe_active_cte-model }{ wa_j_1bnfe_active_cte-serie }{ wa_j_1bnfe_active_cte-nfnum9 }{
          wa_j_1bnfe_active_cte-docnum9 }{ wa_j_1bnfe_active_cte-cdv }|.
        ENDIF.

        IF wa_vbrp-vbeln IS NOT INITIAL AND wa_saida-parc_coleta IS INITIAL.
          CLEAR wa_lfa1.

          READ TABLE t_vbpa   INTO wa_vbpa   WITH KEY vbeln = wa_vbrp-vbeln BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            READ TABLE t_coleta INTO wa_coleta WITH KEY lifnr = wa_vbpa-lifnr BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wa_saida-parc_coleta = wa_coleta-name1.
              wa_saida-cid_coleta  = wa_coleta-ort01.
              wa_saida-uf_coleta   = wa_coleta-regio.
            ENDIF.
          ENDIF.
        ENDIF.

        wa_saida-filial = wa_vttk-tdlnr.
        wa_saida-filial_2 = wa_vttk-tdlnr+6(4).  "*-CS2022001119-07.02.2023-#99561-JT

        wa_saida-erdat  = wa_vttk-budat.
        wa_saida-dacte  = wa_vttk-exti1.
        wa_saida-tknum  = wa_vttk-tknum.

        READ TABLE t_zlest0032 INTO wa_zlest0032 WITH KEY tknum = wa_vttk-tknum BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-fknum  = wa_zlest0032-fknum.
        ENDIF.

        READ TABLE t_vbfa WITH KEY vbeln = wa_vttk-tknum BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          IF t_vbfa-vbtyp_n EQ '8' AND t_vbfa-vbtyp_v EQ 'J'.

            READ TABLE t_vbfa_aux WITH KEY vbelv = t_vbfa-vbelv vbtyp_n = 'M' BINARY SEARCH.

            IF sy-subrc NE 0.
              READ TABLE t_vbfa_aux WITH KEY vbelv = t_vbfa-vbelv vbtyp_n = 'R' BINARY SEARCH.
            ENDIF.
            IF sy-subrc IS INITIAL AND t_vbfa_aux-vbtyp_n EQ 'M'.

              READ TABLE t_bnflin WITH KEY refkey = t_vbfa_aux-refkey BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wa_saida-nfnett = t_bnflin-netwr.

                READ TABLE t_bnfdoc WITH KEY docnum = t_bnflin-docnum BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  "Modificação Chamado CS2017000210
                  IF t_bnfdoc-nfe IS NOT INITIAL.
                    wa_saida-nfenum     = t_bnfdoc-nfenum.
                  ELSE.
                    wa_saida-nfenum     = t_bnfdoc-nfnum.
                  ENDIF.
*              WA_SAIDA-NFENUM     = T_BNFDOC-NFENUM.
                  wa_saida-series     = t_bnfdoc-series.
                  wa_saida-pstdat     = t_bnfdoc-pstdat.
                  wa_saida-docnum_nfe = t_bnfdoc-docnum.

                  READ TABLE t_j_1bnfe_active_nfe INTO wa_j_1bnfe_active_nfe WITH KEY  docnum = wa_saida-docnum_nfe BINARY SEARCH.
                  IF  sy-subrc IS INITIAL.
                    wa_saida-chave_nfe = |{ wa_j_1bnfe_active_nfe-regio }{ wa_j_1bnfe_active_nfe-nfyear }{ wa_j_1bnfe_active_nfe-nfmonth }{ wa_j_1bnfe_active_nfe-stcd1 }{ wa_j_1bnfe_active_nfe-model }{ wa_j_1bnfe_active_nfe-serie }{
                                            wa_j_1bnfe_active_nfe-nfnum9 }{ wa_j_1bnfe_active_nfe-docnum9 }{ wa_j_1bnfe_active_nfe-cdv }|.
                  ENDIF.
                ENDIF.
              ENDIF.

            ELSEIF sy-subrc IS INITIAL AND t_vbfa_aux-vbtyp_n EQ 'R'.

              READ TABLE t_bnflin WITH KEY refkey = t_vbfa_aux-refkey BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wa_saida-nfnett = t_bnflin-netwr.

                READ TABLE t_bnfdoc WITH KEY docnum = t_bnflin-docnum BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  "Modificação Chamado CS2017000210
                  IF t_bnfdoc-nfe IS NOT INITIAL.
                    wa_saida-nfenum     = t_bnfdoc-nfenum.
                  ELSE.
                    wa_saida-nfenum     = t_bnfdoc-nfnum.
                  ENDIF.
*              WA_SAIDA-NFENUM = T_BNFDOC-NFENUM.
                  wa_saida-series = t_bnfdoc-series.
                  wa_saida-pstdat = t_bnfdoc-pstdat.
                  wa_saida-docnum_nfe = t_bnfdoc-docnum.

                  READ TABLE t_j_1bnfe_active_nfe INTO wa_j_1bnfe_active_nfe WITH KEY  docnum = wa_saida-docnum_nfe BINARY SEARCH.
                  IF  sy-subrc IS INITIAL.
                    wa_saida-chave_nfe = |{ wa_j_1bnfe_active_nfe-regio }{ wa_j_1bnfe_active_nfe-nfyear }{ wa_j_1bnfe_active_nfe-nfmonth }{ wa_j_1bnfe_active_nfe-stcd1 }{ wa_j_1bnfe_active_nfe-model }{ wa_j_1bnfe_active_nfe-serie }{
                    wa_j_1bnfe_active_nfe-nfnum9 }{ wa_j_1bnfe_active_nfe-docnum9 }{ wa_j_1bnfe_active_nfe-cdv }|.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

          ELSEIF t_vbfa-vbtyp_n EQ '8' AND t_vbfa-vbtyp_v EQ '7'.

            READ TABLE t_zgr_docs WITH KEY av_vbeln = t_vbfa-vbelv BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              READ TABLE t_bnflin WITH KEY refkey = t_zgr_docs-refkey BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                wa_saida-nfnett = t_bnflin-netwr.

                READ TABLE t_bnfdoc WITH KEY docnum = t_bnflin-docnum BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  "Modificação Chamado CS2017000210
                  IF t_bnfdoc-nfe IS NOT INITIAL.
                    wa_saida-nfenum     = t_bnfdoc-nfenum.
                  ELSE.
                    wa_saida-nfenum     = t_bnfdoc-nfnum.
                  ENDIF.
*              WA_SAIDA-NFENUM = T_BNFDOC-NFENUM.
                  wa_saida-series = t_bnfdoc-series.
                  wa_saida-pstdat = t_bnfdoc-pstdat.
                  wa_saida-docnum_nfe = t_bnfdoc-docnum.

                  READ TABLE t_j_1bnfe_active_nfe INTO wa_j_1bnfe_active_nfe WITH KEY  docnum = wa_saida-docnum_nfe BINARY SEARCH.
                  IF  sy-subrc IS INITIAL.
                    wa_saida-chave_nfe = |{ wa_j_1bnfe_active_nfe-regio }{ wa_j_1bnfe_active_nfe-nfyear }{ wa_j_1bnfe_active_nfe-nfmonth }{ wa_j_1bnfe_active_nfe-stcd1 }{ wa_j_1bnfe_active_nfe-model }{ wa_j_1bnfe_active_nfe-serie }{
                    wa_j_1bnfe_active_nfe-nfnum9 }{ wa_j_1bnfe_active_nfe-docnum9 }{ wa_j_1bnfe_active_nfe-cdv }|.
                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
              READ TABLE t_zlest0110 WITH KEY vbeln = t_vbfa-vbelv BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wa_saida-nfnett     = t_zlest0110-vl_nota_fiscal.
                wa_saida-nfenum     = t_zlest0110-numero.
                wa_saida-series     = t_zlest0110-serie.
                wa_saida-pstdat     = t_zlest0110-dtemissao.
                CLEAR: wa_saida-docnum_nfe.

                ""INI- KCM -  DEVK9A2RV9 - STEFANINI - MM - 3000023163 - IR252300 - Ajustes ZFIS16 - 02.09.2025
                "Buscamos o DOCNUM na J_1BNFDOC
                DATA(lv_series) = t_zlest0110-serie.

                " 1ª tentativa: série original
                SELECT SINGLE *
                  FROM j_1bnfdoc
                  INTO @DATA(ls_j1bnfdoc)
                 WHERE nfenum = @t_zlest0110-numero
                   AND series = @lv_series
                   ""AND parid  = t_zlest0110-parid
                   AND branch = @t_zlest0110-werks
                   AND cancel = ''.

                " 2ª tentativa: série sem zeros à esquerda
                IF sy-subrc <> 0.
                  DATA(lv_series_aux) = lv_series.
                  SHIFT lv_series_aux LEFT DELETING LEADING '0'.

                  SELECT SINGLE *
                    FROM j_1bnfdoc
                    INTO @DATA(ls_j1bnfdoc_aux)
                   WHERE nfenum = @t_zlest0110-numero
                     AND series = @lv_series_aux
                     ""AND parid  = t_zlest0110-parid
                     AND branch = @t_zlest0110-werks
                     AND cancel = ''.
                ENDIF.

                " Se encontrou, preenche a saída
                IF sy-subrc = 0.
                  wa_saida-docnum_nfe = t_zlest0110-chave.  "chave da ZLEST0110
                  IF ls_j1bnfdoc-docnum IS NOT INITIAL.
                    wa_saida-chave_nfe  = ls_j1bnfdoc-docnum. "docnum da J_1BNFDOC
                  ELSE.
                    wa_saida-chave_nfe  = ls_j1bnfdoc_aux-docnum.
                  ENDIF.
                ELSE.
                  CLEAR wa_saida-docnum_nfe.
                ENDIF.
                ""FIM- KCM -  DEVK9A2RV9 - STEFANINI - MM - 3000023163 - IR252300 - Ajustes ZFIS16 - 02.09.2025

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        wa_saida-pc_veiculo = wa_vttk-text1(7).

        READ TABLE t_zlest0002 INTO wa_zlest0002 WITH KEY pc_veiculo = wa_saida-pc_veiculo BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-cd_cidade  = wa_zlest0002-cd_cidade.
          wa_saida-cd_uf      = wa_zlest0002-cd_uf.
        ENDIF.

        CASE wa_vttk-add03.
          WHEN: '0000000001'.

            "READ TABLE T_ZLEST0032 INTO WA_ZLEST0032 WITH KEY TKNUM = WA_VTTK-TKNUM.
            READ TABLE t_lips INTO wa_lips WITH KEY vbeln = wa_vttk-vbeln BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              ""INI- KCM -  DEVK9A2RV9 - STEFANINI - MM - 3000023163 - IR252300 - Ajustes ZFIS16 - 03.09.2025
              "--- Verifica se o centro atual (virtual) tem mapeamento para centro real
              SELECT SINGLE centro_real
                INTO @DATA(lv_werks_real)
                FROM zsdt_depara_cen
               WHERE centrov_1 = @wa_lips-werks.

              IF sy-subrc = 0 AND lv_werks_real IS NOT INITIAL.
                "--- Encontrou mapeamento: substitui pelo centro real
                wa_lips-werks = lv_werks_real.
              ENDIF.
              ""FIM- KCM -  DEVK9A2RV9 - STEFANINI - MM - 3000023163 - IR252300 - Ajustes ZFIS16 - 03.09.2025

              READ TABLE t_j_1bbranch INTO wa_j_1bbranch WITH KEY branch = wa_lips-werks BINARY SEARCH.

              IF sy-subrc IS INITIAL.

                READ TABLE t_j_1bbranch INTO wa_j_1bbranch_aux WITH KEY branch = wa_vttk-branch BINARY SEARCH.
                IF sy-subrc IS INITIAL.

                  IF ( wa_j_1bbranch-bukrs NE wa_j_1bbranch_aux-bukrs ).

*                      SELECT SINGLE *
*                       FROM bseg
*                        INTO wa_bseg
*                       WHERE belnr EQ wa_vttk-obj_key_sub_aux
*                         AND buzei EQ '001'
*                         AND bschl EQ '31'
*                         AND bukrs EQ wa_j_1bbranch_aux-bukrs.
                    READ TABLE lt_bseg INTO wa_bseg WITH KEY bukrs = wa_j_1bbranch_aux-bukrs
                                                             belnr = wa_vttk-obj_key_sub_aux BINARY SEARCH.

                    IF sy-subrc IS INITIAL.
                      wa_saida-bukrs_sub = wa_j_1bbranch_aux-bukrs.
                      wa_saida-doc_sub   = wa_bseg-belnr.
                      wa_saida-wrbtr     = wa_bseg-wrbtr.
                    ENDIF.

                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

        ENDCASE.


        x_data = sy-datum.
        x_hora = sy-uzeit.

        CONCATENATE x_data+6(2) '/'
                    x_data+4(2) '/'
                    x_data(4)   ' -  '
                    x_hora(2)   ':'
                    x_hora+2(2) ':'
                    x_hora+4(2) INTO wa_saida-data.

        wa_saida-user        = sy-uname.

        "PIS COFINS """""""""""""""""""""""""""""""""""""""""
        DATA(ck_achou_pis) = abap_false.
        DATA(ck_achou_cofins) = abap_false.

        READ TABLE t_rbco TRANSPORTING NO FIELDS WITH KEY belnr = wa_vttk-belnr
                                                          gjahr = wa_vttk-gjahr BINARY SEARCH.
        IF sy-subrc = 0.
          LOOP AT t_rbco INTO DATA(wa_rbco) FROM sy-tabix.
            IF wa_rbco-belnr NE wa_vttk-belnr OR
               wa_rbco-gjahr NE wa_vttk-gjahr.
              EXIT.
            ENDIF.
            CASE wa_rbco-saknr.
              WHEN '0000113214'. "pis
                ck_achou_pis = abap_true.
                ADD wa_rbco-wrbtr TO wa_saida-pis_sub.
              WHEN '0000113216'. "cofins
                ck_achou_cofins = abap_true.
                ADD wa_rbco-wrbtr TO wa_saida-cofins_sub.
            ENDCASE.
          ENDLOOP.
        ENDIF.
        IF wa_vttk-obj_key_sub IS NOT INITIAL AND ( ck_achou_cofins EQ abap_false OR ck_achou_pis EQ abap_false ).

          READ TABLE t_bsis TRANSPORTING NO FIELDS WITH KEY belnr = wa_vttk-obj_key_sub
                                                            gjahr = wa_vttk-data(4) BINARY SEARCH.

          LOOP AT t_bsis INTO DATA(wa_bsis) FROM sy-tabix.
            IF wa_bsis-belnr NE wa_vttk-obj_key_sub OR
               wa_bsis-gjahr NE wa_vttk-data(4).
              EXIT.
            ENDIF.
            CASE wa_bsis-hkont.
              WHEN '0000113214'. "pis
                IF ck_achou_pis EQ abap_false.
                  ADD wa_bsis-dmbtr TO wa_saida-pis_sub.
                ENDIF.
              WHEN '0000113216'. "cofins
                IF ck_achou_cofins EQ abap_false.
                  ADD wa_bsis-dmbtr TO wa_saida-cofins_sub.
                ENDIF.
            ENDCASE.
          ENDLOOP.
        ENDIF.
        """""""""""""""""""""""""""""""""""""""""""""""""""""

*       User Story 144142 // MMSILVA - 01.10.2024
        DATA(ck_achou_pis_tom) = abap_false.
        DATA(ck_achou_cofins_tom) = abap_false.

        READ TABLE t_rbco TRANSPORTING NO FIELDS WITH KEY belnr = wa_bseg-belnr
                                                          gjahr = wa_bseg-gjahr BINARY SEARCH.
        IF sy-subrc = 0.
          LOOP AT t_rbco INTO DATA(wa_rbco_tom) FROM sy-tabix.
            IF wa_rbco_tom-belnr NE wa_bseg-belnr OR
               wa_rbco_tom-gjahr NE wa_bseg-gjahr.
              EXIT.
            ENDIF.
            CASE wa_rbco_tom-saknr.
              WHEN '0000113214'. "pis
                ck_achou_pis_tom = abap_true.
                ADD wa_rbco-wrbtr TO wa_saida-pis_tom.
              WHEN '0000113216'. "cofins
                ck_achou_cofins_tom = abap_true.
                ADD wa_rbco_tom-wrbtr TO wa_saida-cofins_tom.
            ENDCASE.
          ENDLOOP.
        ENDIF.
        IF wa_vttk-obj_key_sub IS NOT INITIAL AND ( ck_achou_cofins_tom EQ abap_false OR ck_achou_pis_tom EQ abap_false ).

          READ TABLE t_bsis TRANSPORTING NO FIELDS WITH KEY belnr = wa_vttk-obj_key_sub
                                                            gjahr = wa_vttk-data(4) BINARY SEARCH.

          LOOP AT t_bsis INTO DATA(wa_bsis_tom) FROM sy-tabix.
            IF wa_bsis_tom-belnr NE wa_vttk-obj_key_sub OR
               wa_bsis_tom-gjahr NE wa_vttk-data(4).
              EXIT.
            ENDIF.
            CASE wa_bsis_tom-hkont.
              WHEN '0000113214'. "pis
                IF ck_achou_pis_tom EQ abap_false.
                  ADD wa_bsis_tom-dmbtr TO wa_saida-pis_tom.
                ENDIF.
              WHEN '0000113216'. "cofins
                IF ck_achou_cofins_tom EQ abap_false.
                  ADD wa_bsis_tom-dmbtr TO wa_saida-cofins_tom.
                ENDIF.
            ENDCASE.
          ENDLOOP.
        ENDIF.
*       User Story 144142 // MMSILVA - 01.10.2024


        APPEND wa_saida TO t_saida.

        CLEAR: wa_j_1bbranch, wa_j_1bbranch_aux, wa_lips, wa_bseg, wa_saida.

      ENDLOOP.
    ENDIF.

    CLEAR: wa_saida, wa_lfa1, wa_vttk, wa_konv, t_vtrlp, wa_vtrlp, x_data, x_hora, wa_vfkp,
             wa_vfsi,  wa_tvtkt, wa_vbak, wa_vbrp, wa_kna1, wa_vbpa, wa_ekbe, wa_rbkp.

    CLEAR: wa_vfkp , wa_rbkp, wa_zlest0032, wa_bkpf, wa_vfkp_ped.


  ENDLOOP.

*-CS2022001119-07.02.2023-#99561-JT-inicio
  CHECK t_saida[] IS NOT INITIAL.

  SELECT bukrs, branch
    INTO TABLE @DATA(t_j1bb1)
    FROM j_1bbranch
     FOR ALL ENTRIES IN @t_saida
   WHERE branch = @t_saida-filial_2.

  SELECT bukrs, branch
    INTO TABLE @DATA(t_j1bb2)
    FROM j_1bbranch
     FOR ALL ENTRIES IN @t_saida
   WHERE branch = @t_saida-werks.

  SELECT fknum, fkpos, rebel, fkpty, netwr
    INTO TABLE @DATA(t_vfkp2)
    FROM vfkp
     FOR ALL ENTRIES IN @t_saida
   WHERE rebel = @t_saida-tknum
     AND fkpty = 'Z001'.

  LOOP AT t_saida INTO wa_saida.
    vg_tabix = sy-tabix.

    CHECK wa_saida-fknum IS INITIAL OR
          wa_saida-wrbtr IS INITIAL.

    READ TABLE t_j1bb1 INTO DATA(w_j1bb1) WITH KEY branch = wa_saida-filial_2.
    CHECK sy-subrc = 0.

    READ TABLE t_j1bb2 INTO DATA(w_j1bb2) WITH KEY branch = wa_saida-werks.
    CHECK sy-subrc = 0.

    IF w_j1bb1-bukrs = w_j1bb2-bukrs.
      READ TABLE t_vfkp2 INTO DATA(w_vfkp2) WITH KEY rebel = wa_saida-tknum.
      CHECK sy-subrc = 0.

      IF wa_saida-fknum IS INITIAL.
        wa_saida-fknum = w_vfkp2-fknum.
      ENDIF.
      IF wa_saida-wrbtr IS INITIAL.
        wa_saida-wrbtr = w_vfkp2-netwr.
      ENDIF.

      MODIFY t_saida FROM wa_saida INDEX vg_tabix TRANSPORTING fknum wrbtr.
    ENDIF.
  ENDLOOP.
*-CS2022001119-07.02.2023-#99561-JT-fim

*>>>>Inicio ajuste USER STORY 158497 / AOENNING.
  IF p_chav_c IS NOT INITIAL.
    SORT t_saida BY chave_cte.
    DELETE t_saida WHERE chave_cte NOT IN p_chav_c.
  ENDIF.

  IF p_chav_n IS NOT INITIAL.
    SORT t_saida BY chave_nfe.
    DELETE t_saida WHERE chave_nfe NOT IN p_chav_n.
  ENDIF.

  IF p_cod_su IS NOT INITIAL.
    SORT t_saida BY cod_prop.
    DELETE t_saida WHERE cod_prop NOT IN p_cod_su.
  ENDIF.
*>>>>Fim ajuste USER STORY 158497 / AOENNING.

* US #167060 - MMSILVA - 25.02.2025 - Inicio
  IF p_ped IS NOT INITIAL.
    DELETE t_saida WHERE kwert IS INITIAL.
  ENDIF.
* US #167060 - MMSILVA - 25.02.2025 - Fim


*
*  LOOP AT T_RBKP INTO WA_RBKP.
*
*    READ TABLE T_EKBE INTO WA_EKBE WITH KEY BELNR = WA_RBKP-BELNR
*                                            GJAHR = WA_RBKP-GJAHR BINARY SEARCH.
*
*    IF ( SY-SUBRC NE 0 ).
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE T_VFKP INTO WA_VFKP WITH KEY EBELN = WA_EKBE-EBELN
*                                            LBLNI = WA_EKBE-LFBNR  BINARY SEARCH.
*
*    IF ( SY-SUBRC NE 0 ).
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE T_VTTK INTO WA_VTTK WITH KEY TKNUM = WA_VFKP-REBEL BINARY SEARCH.
*
*    IF ( SY-SUBRC NE 0 ).
*      CONTINUE.
*    ENDIF.
*
*    LOOP AT T_VTTK WHERE TKNUM = WA_VTTK-TKNUM.
*
*      CALL FUNCTION 'RV_SHIPMENT_VIEW'
*        EXPORTING
*          SHIPMENT_NUMBER = WA_VTTK-TKNUM
*          LANGUAGE        = SY-LANGU
*          OPTION_PACKAGES = 'X'
*        TABLES
*          F_TRLP          = T_VTRLP.
*
*      READ TABLE T_VTRLP INTO WA_VTRLP INDEX 1.
*      WA_SAIDA-MATERIAL   = WA_VTRLP-MATNR.
*      WA_SAIDA-DESC_MAT   = WA_VTRLP-ARKTX.
*      WA_SAIDA-QUANTIDADE = WA_VTRLP-LFIMG.
*
*
*
*
*      LOOP AT T_VTPA INTO WA_VTPA WHERE VBELN = WA_VTTK-TKNUM .
*
*        CLEAR WA_LFA1.
*
*        IF WA_VTPA-PARVW EQ 'PV'.
*
*          READ TABLE T_PARC_LIF INTO WA_PARC_LIF WITH KEY LIFNR = WA_VTPA-LIFNR BINARY SEARCH.
*
*          WA_SAIDA-PROPRIETARIO = WA_PARC_LIF-NAME1.
*          WA_SAIDA-COD_PROP     = WA_PARC_LIF-LIFNR.
*
*          IF NOT WA_PARC_LIF-STCD1 IS INITIAL.
*            WA_SAIDA-CPF_PROD     = WA_PARC_LIF-STCD1.
*          ELSE.
*            WA_SAIDA-CPF_PROD     = WA_PARC_LIF-STCD2.
*          ENDIF.
*
**            if wa_vttk-add02 eq '0000000003'.
**              wa_saida-tp_pessoa = 'Física'.
**            else.
**              wa_saida-tp_pessoa = 'Jurídica'.
**            endif.
*
*          IF WA_PARC_LIF-STKZN NE ''.
*            WA_SAIDA-TP_PESSOA = 'Física'.
*          ELSE.
*            WA_SAIDA-TP_PESSOA = 'Jurídica'.
*          ENDIF.
*
*        ENDIF.
*
*        IF WA_VTPA-PARVW EQ 'LR'.
*
*          IF WA_VTPA-KUNNR NE ''.
*            READ TABLE T_PARC_KUN INTO WA_PARC_KUN WITH KEY LIFNR = WA_VTPA-KUNNR BINARY SEARCH.
*            WA_SAIDA-COD_DEST     = WA_PARC_KUN-LIFNR.
*            WA_SAIDA-DESTINATARIO = WA_PARC_KUN-NAME1.
*          ELSE.
*            READ TABLE T_PARC_LIF INTO WA_PARC_LIF WITH KEY LIFNR = WA_VTPA-LIFNR BINARY SEARCH.
*            WA_SAIDA-COD_DEST     = WA_PARC_LIF-LIFNR.
*            WA_SAIDA-DESTINATARIO = WA_PARC_LIF-NAME1.
*          ENDIF.
*
*        ENDIF.
*
*        IF WA_VTPA-PARVW EQ 'PC' OR WA_VTPA-PARVW EQ 'LR'.
*
*          IF WA_VTPA-LIFNR IS NOT INITIAL.
*
*            CLEAR WA_LFA1.
*
*            READ TABLE T_PARC_LIF INTO WA_PARC_LIF WITH KEY LIFNR = WA_VTPA-LIFNR BINARY SEARCH.
*
*            IF WA_VTPA-PARVW EQ 'PC'.
*              WA_SAIDA-PARC_COLETA = WA_PARC_LIF-NAME1.
*              WA_SAIDA-CID_COLETA  = WA_PARC_LIF-ORT01.
*              WA_SAIDA-UF_COLETA   = WA_PARC_LIF-REGIO.
*            ELSE.
*              WA_SAIDA-PONT_ENTREGA = WA_PARC_LIF-NAME1.
*              WA_SAIDA-CID_ENTREGA  = WA_PARC_LIF-ORT01.
*              WA_SAIDA-UF_ENTREGA   = WA_PARC_LIF-REGIO.
*            ENDIF.
*
*          ELSE.
*            READ TABLE T_PARC_KUN INTO WA_PARC_KUN WITH KEY LIFNR = WA_VTPA-KUNNR BINARY SEARCH.
*
*            IF WA_VTPA-PARVW EQ 'PC'.
*              WA_SAIDA-PARC_COLETA = WA_PARC_KUN-NAME1.
*              WA_SAIDA-CID_COLETA  = WA_PARC_KUN-ORT01.
*              WA_SAIDA-UF_COLETA   = WA_PARC_KUN-REGIO.
*            ELSE.
*              WA_SAIDA-PONT_ENTREGA = WA_PARC_KUN-NAME1.
*              WA_SAIDA-CID_ENTREGA  = WA_PARC_KUN-ORT01.
*              WA_SAIDA-UF_ENTREGA   = WA_PARC_KUN-REGIO.
*            ENDIF.
*            " wa_saida-cod_dest     = WA_VTPA-lifnr.
*          ENDIF.
*
*        ENDIF.
*        CLEAR : WA_VTPA, WA_PARC_KUN, WA_PARC_LIF.
*      ENDLOOP.
*
*      CLEAR WA_LFA1.
*
*      READ TABLE T_T001W INTO WA_T001W  WITH KEY WERKS = WA_VTTK-TPLST BINARY SEARCH.
*
*      IF WA_T001W-LIFNR IS NOT INITIAL.
*        READ TABLE T_REM_LIF INTO WA_REM_LIF WITH KEY LIFNR = WA_T001W-LIFNR BINARY SEARCH.
*        MOVE-CORRESPONDING WA_REM_LIF TO WA_INFO_PART.
*
*      ELSEIF WA_T001W-KUNNR IS NOT INITIAL.
*
*        READ TABLE T_REM_KUN INTO WA_REM_KUN WITH KEY LIFNR = WA_T001W-KUNNR BINARY SEARCH.
*        MOVE-CORRESPONDING WA_REM_KUN TO WA_INFO_PART.
*
*      ELSE.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT  = WA_VTTK-TPLST
*          IMPORTING
*            OUTPUT = VG_PARCEIRO.
*
*        READ TABLE T_REM_LST INTO WA_REM_LST WITH KEY LIFNR = VG_PARCEIRO BINARY SEARCH.
*
*        MOVE-CORRESPONDING WA_REM_LST TO WA_INFO_PART.
*
*      ENDIF.
*
*      WA_SAIDA-REMETENTE = WA_INFO_PART-NAME1 .
*
**      READ TABLE T_VFKP INTO WA_VFKP WITH KEY REBEL = WA_VTTK-TKNUM BINARY SEARCH.
**
**      READ TABLE T_EKBE INTO WA_EKBE WITH KEY EBELN = WA_VFKP-EBELN
**                                              LFBNR = WA_VFKP-LBLNI BINARY SEARCH.
**
**      READ TABLE T_RBKP INTO WA_RBKP WITH KEY BELNR = WA_EKBE-BELNR
**                                              GJAHR = WA_EKBE-GJAHR BINARY SEARCH.
*
*
*      "wa_saida-netwr  = val_frete.
*      WA_SAIDA-NETWR  = WA_RBKP-RMWWR.
*      WA_SAIDA-BELNR  = WA_RBKP-BELNR.
*      WA_SAIDA-GJAHR  = WA_RBKP-GJAHR.
*      WA_SAIDA-BUDAT  = WA_RBKP-BUDAT.
*
*      READ TABLE T_BKPF INTO WA_BKPF WITH KEY   BUKRS = WA_RBKP-BUKRS
*                                                GJAHR = WA_RBKP-GJAHR
*                                                AWKEY = WA_RBKP-AWKEY BINARY SEARCH.
*      " docto contábil
*      WA_SAIDA-BELNRB  = WA_BKPF-BELNR.
*
*      READ TABLE T_TVTKT INTO WA_TVTKT WITH KEY SHTYP = WA_VTTK-SHTYP BINARY SEARCH.
*      CONCATENATE WA_VTTK-SHTYP  ' - ' WA_TVTKT-BEZEI(20) INTO WA_SAIDA-SHTYP.
*
*      READ TABLE T_VBAK INTO WA_VBAK WITH KEY TKNUM = WA_VTTK-TKNUM BINARY SEARCH.
*      WA_SAIDA-ORDEM  = WA_VBAK-VBELN.
*
*      READ TABLE T_VBRP INTO WA_VBRP WITH KEY VGBEL = WA_VBAK-VBELN BINARY SEARCH.
*      WA_SAIDA-FATURA     = WA_VBRP-VBELN.
*      WA_SAIDA-VLR_FATURA = WA_VBRP-NETWR.
*
*      IF WA_VBRP-VBELN IS NOT INITIAL AND WA_SAIDA-PARC_COLETA IS INITIAL.
*        CLEAR WA_LFA1.
*
*        READ TABLE T_VBPA   INTO WA_VBPA   WITH KEY VBELN = WA_VBRP-VBELN BINARY SEARCH.
*        READ TABLE T_COLETA INTO WA_COLETA WITH KEY LIFNR = WA_VBPA-LIFNR BINARY SEARCH.
*
*        WA_SAIDA-PARC_COLETA = WA_COLETA-NAME1.
*        WA_SAIDA-CID_COLETA  = WA_COLETA-ORT01.
*        WA_SAIDA-UF_COLETA   = WA_COLETA-REGIO.
*
*      ENDIF.
*
*      WA_SAIDA-BUKRS  = WA_VTTK-BUKRS.
*      WA_SAIDA-WERKS  = WA_VTTK-WERKS.
*      WA_SAIDA-FILIAL = WA_VTTK-TDLNR.
*      WA_SAIDA-ERDAT  = WA_VTTK-ERDAT.
*      WA_SAIDA-DACTE  = WA_VTTK-EXTI1.
*      WA_SAIDA-TKNUM  = WA_VTTK-TKNUM.
*      WA_SAIDA-FKNUM  = WA_VTTK-FKNUM.
*
*      READ TABLE T_VBFA
*        WITH KEY VBELN = WA_VTTK-TKNUM.
*
*      IF T_VBFA-VBTYP_N EQ '8'
*      AND T_VBFA-VBTYP_V EQ 'J'.
*        READ TABLE T_VBFA_AUX
*          WITH KEY VBELV = T_VBFA-VBELV.
*
*        IF SY-SUBRC IS INITIAL
*        AND T_VBFA_AUX-VBTYP_N EQ 'M'.
*          READ TABLE T_BNFLIN
*           WITH KEY REFKEY = T_VBFA_AUX-REFKEY.
*          IF SY-SUBRC IS INITIAL.
*            WA_SAIDA-NFNETT = T_BNFLIN-NETWR.
*
*            READ TABLE T_BNFDOC
*              WITH KEY DOCNUM = T_BNFLIN-DOCNUM.
*            IF SY-SUBRC IS INITIAL.
*              WA_SAIDA-NFENUM = T_BNFDOC-NFENUM.
*              WA_SAIDA-SERIES = T_BNFDOC-SERIES.
*              WA_SAIDA-PSTDAT = T_BNFDOC-PSTDAT.
*
*            ENDIF.
*          ENDIF.
*        ELSEIF SY-SUBRC IS INITIAL
*          AND T_VBFA_AUX-VBTYP_N EQ 'R'.
*          READ TABLE T_BNFLIN
*          WITH KEY REFKEY = T_VBFA_AUX-REFKEY.
*          IF SY-SUBRC IS INITIAL.
*            WA_SAIDA-NFNETT = T_BNFLIN-NETWR.
*
*            READ TABLE T_BNFDOC
*              WITH KEY DOCNUM = T_BNFLIN-DOCNUM.
*            IF SY-SUBRC IS INITIAL.
*              WA_SAIDA-NFENUM = T_BNFDOC-NFENUM.
*              WA_SAIDA-SERIES = T_BNFDOC-SERIES.
*              WA_SAIDA-PSTDAT = T_BNFDOC-PSTDAT.
*
*            ENDIF.
*          ENDIF.
*
*        ENDIF.
*
*      ELSEIF T_VBFA-VBTYP_N EQ '8'
*         AND T_VBFA-VBTYP_V EQ '7'.
*        READ TABLE T_ZGR_DOCS
*          WITH KEY AV_VBELN = T_VBFA-VBELV.
*
*        IF SY-SUBRC IS INITIAL.
*          READ TABLE T_BNFLIN
*            WITH KEY REFKEY = T_ZGR_DOCS-REFKEY.
*
*          IF SY-SUBRC IS INITIAL.
*            WA_SAIDA-NFNETT = T_BNFLIN-NETWR.
*
*            READ TABLE T_BNFDOC
*              WITH KEY DOCNUM = T_BNFLIN-DOCNUM.
*            IF SY-SUBRC IS INITIAL.
*              WA_SAIDA-NFENUM = T_BNFDOC-NFENUM.
*              WA_SAIDA-SERIES = T_BNFDOC-SERIES.
*              WA_SAIDA-PSTDAT = T_BNFDOC-PSTDAT.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*
*      ENDIF.
*
*      WA_SAIDA-PC_VEICULO = WA_VTTK-TEXT1(7).
*
*      READ TABLE T_ZLEST0002 INTO WA_ZLEST0002 WITH KEY PC_VEICULO = WA_SAIDA-PC_VEICULO.
*      IF ( SY-SUBRC EQ 0 ).
*        WA_SAIDA-CD_CIDADE  = WA_ZLEST0002-CD_CIDADE.
*        WA_SAIDA-CD_UF      = WA_ZLEST0002-CD_UF.
*      ENDIF.
*
*
*
*      X_DATA = SY-DATUM.
*      X_HORA = SY-UZEIT.
*
*      CONCATENATE X_DATA+6(2) '/'
*                  X_DATA+4(2) '/'
*                  X_DATA(4)   ' -  '
*                  X_HORA(2)   ':'
*                  X_HORA+2(2) ':'
*                  X_HORA+4(2) INTO WA_SAIDA-DATA.
*
*      WA_SAIDA-USER        = SY-UNAME.
*
*      APPEND WA_SAIDA TO T_SAIDA.
*
*    ENDLOOP.
*
*    CLEAR: WA_SAIDA, WA_LFA1, WA_VTTK, WA_KONV, T_VTRLP, WA_VTRLP, X_DATA, X_HORA, WA_VFKP,
*             WA_VFSI,  WA_TVTKT, WA_VBAK, WA_VBRP, WA_KNA1, WA_VBPA, WA_EKBE, WA_RBKP.
*  ENDLOOP.

ENDFORM.                    " F_ORGANIZA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*

FORM f_imprime_dados .
  IF t_saida[] IS INITIAL.
    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.

  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_montar_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = estrutura[]
      it_sort                 = t_sort[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
      is_layout               = w_layout
      is_variant              = w_variant
    TABLES
      t_outtab                = t_saida.

ENDFORM.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*

FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*

FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*

FORM f_montar_layout.
  PERFORM f_montar_estrutura USING:

  1 ''   ''      'T_SAIDA' 'BUKRS'        'Empresa'                 ' ' ' ',
  2 ''   ''      'T_SAIDA' 'FILIAL'       'Agente de Frete'         ' ' 'X',
  3 ''   ''      'T_SAIDA' 'WERKS'        'Centro'                  ' ' ' ',
  4 ''   ''      'T_SAIDA' 'REMETENTE'    'Remetente'               ' ' ' ',
  5 ''   ''      'T_SAIDA' 'COD_DEST'     'Cód. Dest.'              ' ' 'X',
  6 ''   ''      'T_SAIDA' 'DESTINATARIO' 'Nome Destinatário'       ' ' ' ',
  7 ''   ''      'T_SAIDA' 'COD_PROP'     'Cód. Subcont.'           ' ' ' ',
  8 ''   ''      'T_SAIDA' 'PROPRIETARIO' 'Nome Subcontratado'      ' ' ' ',
  9 ''   ''      'T_SAIDA' 'CPF_PROD'     'CNPJ-CPF Subcont.'       ' ' ' ',
 10 ''   ''      'T_SAIDA' 'TP_PESSOA'    'Descrição'               ' ' ' ',
 11 ''   ''      'T_SAIDA' 'PARC_COLETA'  'Ponto de Coleta'         ' ' ' ',
 12 ''   ''      'T_SAIDA' 'CID_COLETA'   'Cidade Coleta'           ' ' ' ',
 13 ''   ''      'T_SAIDA' 'UF_COLETA'    'UF Coleta'               ' ' ' ',
 14 ''   ''      'T_SAIDA' 'ERDAT'        'Dt. Criação'             ' ' ' ',
 15 ''   ''      'T_SAIDA' 'DACTE'        'DACTE'                   ' ' ' ',
 15 ''   ''      'T_SAIDA' 'DOCNUM'       'Docnum Cte'              ' ' ' ',
 16 ''   ''      'T_SAIDA' 'BUDAT'        'Dt.Mv. MIRO'             ' ' ' ',
 17 ''   ''      'T_SAIDA' 'BELNR'        'Nro. MIRO'               ' ' ' ',
 18 ''   ''      'T_SAIDA' 'NETWR'        'Vlr. MIRO'               ' ' ' ',
 19 ''   ''      'T_SAIDA' 'SHTYP'        'Tipo Transporte'         ' ' ' ',
 19 ''   ''      'T_SAIDA' 'MATERIAL'     'Material'                ' ' 'X',
 20 ''   ''      'T_SAIDA' 'DESC_MAT'     'Denominação'             ' ' ' ',
 21 ''   ''      'T_SAIDA' 'QUANTIDADE'   'Quantidade'              ' ' ' ',
 22 ''   ''      'T_SAIDA' 'TKNUM'        'Doc. Transporte'         ' ' 'X',
 23 ''   ''      'T_SAIDA' 'FKNUM'        'Doc Custo'               ' ' 'X',
 24 ''   ''      'T_SAIDA' 'ORDEM'        'Ordem de Venda'          ' ' 'X',
 25 ''   ''      'T_SAIDA' 'FATURA'       'Doc Fatura'              ' ' ' ',
 26 ''   ''      'T_SAIDA' 'VLR_FATURA'   'Vlr. Fatura'             ' ' ' ',
 27 ''   ''      'T_SAIDA' 'PONT_ENTREGA' 'Ponto de Entrega'        ' ' ' ',
 28 ''   ''      'T_SAIDA' 'CID_ENTREGA'  'Cidade Entrega'          ' ' ' ',
 29 ''   ''      'T_SAIDA' 'UF_ENTREGA'   'UF Entrega'              ' ' ' ',
 30 ''   ''      'T_SAIDA' 'BELNRB'       'Doc. Contábil'           ' ' ' ',
 31 ''   ''      'T_SAIDA' 'PC_VEICULO'   'Placa do Cavalo'         ' ' ' ',
 32 ''   ''      'T_SAIDA' 'CD_CIDADE'    'Cidade da Placa'         ' ' ' ',
 33 ''   ''      'T_SAIDA' 'CD_UF'        'UF da Placa'             ' ' ' ',
 34 ''   ''      'T_SAIDA' 'NFENUM'       'Nr. NF'                  ' ' ' ',
 35 ''   ''      'T_SAIDA' 'SERIES'       'Série NF'                ' ' ' ',
 36 ''   ''      'T_SAIDA' 'DOCNUM_NFE'   'Docnum NF'               ' ' ' ',
 37 ''   ''      'T_SAIDA' 'BUKRS_SUB'    'Emp.Doc.Sub'             ' ' ' ',
 38 ''   ''      'T_SAIDA' 'DOC_SUB'      'Doc. Subcontra.'         ' ' ' ',
 39 ''   ''      'T_SAIDA' 'WRBTR'        'Vlr. Subcontra.'         ' ' ' ',
 40 ''   ''      'T_SAIDA' 'PSTDAT'       'Data Movimento'          ' ' ' ',
 41 ''   ''      'T_SAIDA' 'NFNETT'       'Valor NF'                ' ' ' ',
 42 ''   ''      'T_SAIDA' 'CHAVE_NFE'    'Chave Acesso NF-e '      ' ' ' ',
 43 ''   ''      'T_SAIDA' 'CHAVE_CTE'    'Chave Acesso CT-e  '     ' ' ' ',
 44 ''   ''      'T_SAIDA' 'CRTN'         'CódRegTribut.- nº'       ' ' ' ',
 45 ''   ''      'T_SAIDA' 'CRTN_DESC'    'Desc. CódRegTribut.'     ' ' ' ',
 46 ''   ''      'T_SAIDA' 'INDTYP'       'Tp.princ.set.ind.'       ' ' ' ',
 47 ''   ''      'T_SAIDA' 'INDTYP_DESC'  'Desc. Tp.princ.set.ind.' ' ' ' ',
 48 ''   ''      'T_SAIDA' 'PIS_SUB'      'PIS Doc. Subco.'         ' ' ' ',
 49 ''   ''      'T_SAIDA' 'COFINS_SUB'   'COFINS Doc. Subco.'      ' ' ' ',
* 50 ''   ''      'T_SAIDA' 'PIS_TOM'      'PIS Doc. Cont. Tom.'     ' ' ' ', " User Story 144142 // MMSILVA - 01.10.2024
* 51 ''   ''      'T_SAIDA' 'COFINS_TOM'   'COFINS Doc. Cont. Tom.'  ' ' ' ', " User Story 144142 // MMSILVA - 01.10.2024
 52 ''   ''      'T_SAIDA' 'DOCNUM_PED'   'Docnum Pedágio'              ' ' ' ', " User Story 167060 // AOENNING - 28.02.2025
 53 ''   ''      'T_SAIDA' 'KWERT'        'Valor Pedágio'           ' ' ' ', "US #167060 - MMSILVA - 25.02.2025
 54 ''   ''      'T_SAIDA' 'PIS_PED'      'PIS Pedágio'             ' ' ' ', "US #167060 - MMSILVA - 25.02.2025
 55 ''   ''      'T_SAIDA' 'COFINS_PED'   'COFINS Pedágio'          ' ' ' '. "US #167060 - MMSILVA - 25.02.2025


  CLEAR: w_variant, w_layout.
  w_layout-colwidth_optimize = 'X'.
  w_layout-zebra      = 'X'.
  w_variant-report    = sy-repid.
  w_variant-variant   = p_layout.

ENDFORM.                    " F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_no_zero)        TYPE c  .

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.
  wa_estrutura-no_zero       = p_no_zero.

  IF ( p_field EQ 'TKNUM' ) OR ( p_field EQ 'BELNR' ) OR ( p_field EQ 'DOCNUM_PED' ).
    wa_estrutura-hotspot = 'X'.
  ELSE.
    CLEAR wa_estrutura-hotspot.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " F_montar_estrutura

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves.

  DATA: w_texto1(40).
  DATA: w_texto2(20).

  v_report = sy-repid.

*** Nome do Report
  PERFORM f_construir_cabecalho USING 'H' TEXT-002.

  SELECT SINGLE butxt FROM t001 INTO w_texto2
    WHERE bukrs EQ p_bukrs-low.

  CONCATENATE 'Empresa:' p_bukrs-low '-' w_texto2 INTO w_texto1 SEPARATED BY space.
*** Nome da empresa
  PERFORM f_construir_cabecalho USING 'H' w_texto1.

  IF NOT p_werks IS INITIAL.

    SELECT SINGLE name1 FROM t001w INTO w_texto2
      WHERE werks = p_werks-low.

    CONCATENATE 'Filial:' p_werks-low  '-' w_texto2 INTO  w_texto1 SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' w_texto1.
  ENDIF.

  WRITE: sy-datum TO w_texto2.
  CONCATENATE 'Data:' w_texto2 INTO w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.
  WRITE: sy-uzeit TO w_texto2.
  CONCATENATE 'Hora:' w_texto2 INTO w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.
  CONCATENATE 'Usuário:' sy-uname INTO w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.
ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.


*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'A'.
*  LS_LINE-KEY = 'QUEBRA'.
*  LS_LINE-INFO = ' '.
*  APPEND LS_LINE TO T_TOP.



ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort.

*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUKRS'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 1.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUTXT'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 2.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'GSBER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 3.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'NAME'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 4.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'DATA'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 5.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'USER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 6.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM f_user_command USING l_ucomm
                          l_selfield TYPE slis_selfield.

  IF l_selfield-fieldname = 'TKNUM'.

    READ TABLE t_saida INDEX l_selfield-tabindex INTO wa_saida.

    SET PARAMETER ID 'TNR' FIELD l_selfield-value.

    CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.

  ENDIF.


  IF l_selfield-fieldname = 'BELNR'.

    READ TABLE t_saida INDEX l_selfield-tabindex INTO wa_saida.

    SET PARAMETER ID 'RBN' FIELD wa_saida-belnr.
    SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr.
    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.


  ENDIF.

  IF l_selfield-fieldname = 'DOCNUM_PED'.

    READ TABLE t_saida INDEX l_selfield-tabindex INTO wa_saida.

    SET PARAMETER ID 'BLN' FIELD wa_saida-docnum_ped.
    SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.


  ENDIF.

ENDFORM.                    "f_user_command
