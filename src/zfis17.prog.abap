*&------------P R O J E T O  E V O L U I R   -   M A G G I-------------*
* Programa   : ZFIS17                                                  *
* Descrição  : Resumo Geral de Vendas                                  *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Paulo Bonetti                          Data: 10.11.2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*&---------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      : Camila Brand                          Data: 06.06.2011  *
* Observações: Alteraçães conforme chamado 41573                       *
*----------------------------------------------------------------------*
* Autor      : Welgem Barbosa                        Data: 20.07.2015  *
* Observações: Alteraçães conforme IR109704                            *
*----------------------------------------------------------------------*

REPORT  zfis17.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
* TIPOS PARA ALV
*----------------------------------------------------------------------*

TABLES : vbak, vbap, j_1bnfdoc, zdde.

TYPE-POOLS: slis.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES:

*      BEGIN OF ty_vbak,
*        vkorg      TYPE vbak-vkorg,
*        vtweg      TYPE vbak-vtweg,
*        vbeln      TYPE vbak-vbeln,
*        auart      TYPE vbak-auart,
*        audat      TYPE vbak-audat,
*        knumv      TYPE vbak-knumv,
*        kunnr      TYPE vbak-kunnr,
*        tknum      TYPE vbak-tknum,
*        werks      TYPE vbap-werks,
*        charg      TYPE vbap-charg,
*        j_1bbranch TYPE t001w-j_1bbranch,
*      END   OF ty_vbak,

  BEGIN OF ty_zdoc_memo,
    docnum          TYPE zdoc_memo_nf_exp-docnum,
    quantidade_memo TYPE zdoc_memorando-quantidade_memo,
    nr_memorando    TYPE zdoc_memorando-nr_memorando,
  END OF ty_zdoc_memo,

  BEGIN OF ty_zdoc_memo_aux,
    docnum          TYPE zdoc_memo_nf_exp-docnum,
    quantidade_memo TYPE zdoc_memorando-quantidade_memo,
  END OF ty_zdoc_memo_aux,

  BEGIN OF ty_vbfa,
    vbelv      TYPE vbfa-vbelv,
    vbtyp_n    TYPE vbfa-vbtyp_n,
    vbtyp_v    TYPE vbfa-vbtyp_v,
    vbeln      TYPE vbfa-vbeln,
    rfmng      TYPE vbfa-rfmng,
    rfwrt      TYPE vbfa-rfwrt,
    erdat      TYPE vbfa-erdat,
    vbelnvk    TYPE vbak-vbeln,
    vkorg      TYPE vbak-vkorg,
    vtweg      TYPE vbak-vtweg,
    auart      TYPE vbak-auart,
    audat      TYPE vbak-audat,
    knumv      TYPE vbak-knumv,
    kunnr      TYPE vbak-kunnr,
    tknum      TYPE vbak-tknum,
    werks      TYPE vbap-werks,
    charg      TYPE vbap-charg,
    lgort      TYPE vbap-lgort,
    j_1bbranch TYPE t001w-j_1bbranch,
    vkbur      TYPE vbak-vkbur,
  END   OF ty_vbfa,

  BEGIN OF ty_vbrp,
    vbeln TYPE vbrp-vbelv,
    netwr TYPE vbrp-netwr,
    kursk TYPE vbrp-kursk,
    arktx TYPE vbrp-arktx,
    matnr TYPE vbrp-matnr,
  END   OF ty_vbrp,

  BEGIN OF ty_j_1bnflin,
    refkey TYPE j_1bnflin-refkey,
    docnum TYPE j_1bnflin-docnum,
    menge  TYPE j_1bnflin-menge,
    cfop   TYPE j_1bnflin-cfop,
  END   OF ty_j_1bnflin,

  BEGIN OF ty_vbfa_aux,
    vg_refkey TYPE j_1bnflin-refkey,
  END OF ty_vbfa_aux,

  BEGIN OF ty_vbfa_fat,
    vbelv TYPE vbfa-vbelv,
    vbeln TYPE vbfa-vbeln,
  END OF ty_vbfa_fat,

  BEGIN OF ty_vbfa_bkpf,
    ano   TYPE bkpf-gjahr,
    vbeln TYPE bkpf-awkey,
  END OF ty_vbfa_bkpf,

*  removido as variaveis do Zdoc
  BEGIN OF ty_zdoc_exp,
    vbeln            TYPE zdoc_exp-vbeln,
    id_nomeacao_tran TYPE zdoc_exp-id_nomeacao_tran,
*        DS_NOME_TRANSPOR TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR,
*        DS_PORTO         TYPE ZNOM_TRANSPORTE-DS_PORTO ,
*        DS_TERMINAL      TYPE ZNOM_TRANSPORTE-DS_TERMINAL,
    id_dde           TYPE zdde-id_dde,
    id_registro_expo TYPE zreg_exportacao-id_registro_expo,
    id_due           TYPE zsdt0170-id_due,
  END   OF ty_zdoc_exp,

  BEGIN OF ty_znom_transporte,
    id_nomeacao_tran TYPE znom_transporte-id_nomeacao_tran,
    ds_nome_transpor TYPE znom_transporte-ds_nome_transpor,
    ds_porto         TYPE znom_transporte-ds_porto,
    ds_terminal      TYPE znom_transporte-ds_terminal,

  END   OF ty_znom_transporte,

  BEGIN OF ty_j_1bnfdoc_aux,
    docnum TYPE j_1bnfdoc-docnum,
    nfenum TYPE j_1bnfdoc-nfenum,
    cancel TYPE j_1bnfdoc-cancel,
    nfe    TYPE j_1bnfdoc-nfe,
    pstdat TYPE j_1bnfdoc-pstdat,
  END   OF ty_j_1bnfdoc_aux,

  BEGIN OF ty_j_1bnfdoc,
    belnr  TYPE j_1bnfdoc-belnr,
    docnum TYPE j_1bnfdoc-docnum,
    pstdat TYPE j_1bnfdoc-pstdat,
    bukrs  TYPE j_1bnfdoc-bukrs,
    series TYPE j_1bnfdoc-series,
    nftype TYPE j_1bnfdoc-nftype,
    docdat TYPE j_1bnfdoc-docdat,
    crenam TYPE j_1bnfdoc-crenam,
    model  TYPE j_1bnfdoc-model,
    nfnum  TYPE j_1bnfdoc-nfnum,
    branch TYPE j_1bnfdoc-branch,
    parid  TYPE j_1bnfdoc-parid,
    nfe    TYPE j_1bnfdoc-nfe,
    nfenum TYPE j_1bnfdoc-nfenum,
    partyp TYPE j_1bnfdoc-partyp,
    nftot  TYPE j_1bnfdoc-nftot,
    direct TYPE j_1bnfdoc-direct,
    cancel TYPE j_1bnfdoc-cancel,
  END   OF ty_j_1bnfdoc,

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

  BEGIN OF ty_zdde         ,
    id_dde       TYPE zdde-id_dde,
    dt_dde       TYPE zdde-dt_dde,
    nr_dde       TYPE zdde-nr_dde,
    dt_averbacao TYPE zdde-dt_averbacao,
  END OF ty_zdde           ,

  BEGIN OF ty_reg_exportacao    ,
    id_registro_expo    TYPE zreg_exportacao-id_registro_expo,
    nr_registro_expo    TYPE zreg_exportacao-nr_registro_expo,
    dt_registro_expo    TYPE zreg_exportacao-dt_registro_expo,
    id_importador       TYPE zreg_exportacao-id_importador,

    id_nomeacao_tran    TYPE zreg_exportacao-id_nomeacao_tran,
    id_nomeacao_tran_fk TYPE znom_transporte-id_nomeacao_tran,

*        DS_NOME_TRANSPOR TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR,
*        DS_PORTO         TYPE ZNOM_TRANSPORTE-DS_PORTO ,
*        DS_TERMINAL      TYPE ZNOM_TRANSPORTE-DS_TERMINAL,
  END   OF ty_reg_exportacao    ,

  BEGIN OF ty_kna1,        " Mestre de clientes (parte geral)
    name1 TYPE kna1-name1,                             " Nome 1
    kunnr TYPE kna1-kunnr, " Nº cliente 1
  END OF ty_kna1,

  BEGIN OF ty_znom_conhec,
    id_nomeacao_tran TYPE znom_conhec-id_nomeacao_tran,
    nr_conhec        TYPE znom_conhec-nr_conhec,
    dt_data          TYPE znom_conhec-dt_data,
    sg_pais_destino  TYPE znom_conhec-sg_pais_destino,
    land1            TYPE t005t-land1,
  END   OF ty_znom_conhec,

  " Novo valor de fatura
  BEGIN OF ty_bkpf,
    bukrs TYPE  bkpf-bukrs,
    gjahr TYPE  bkpf-gjahr,
    awkey TYPE  bkpf-awkey,
    belnr TYPE  bkpf-belnr,
  END   OF ty_bkpf,

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

  BEGIN OF ty_t001l,
    werks TYPE t001l-werks,
    lgort TYPE t001l-lgort,
    lgobe TYPE t001l-lgobe,
  END OF ty_t001l,


  BEGIN OF ty_vbrk,
    vbeln TYPE vbrk-vbeln,
    fksto TYPE vbrk-fksto,
  END OF ty_vbrk,

  " Informações de Saida
  BEGIN OF ty_saida,
    charg            TYPE vbap-charg,
    vkorg            TYPE vbak-vkorg,
    vkbur            TYPE vbak-vkbur,
    werks            TYPE vbap-werks,
    vbeln            TYPE vbak-vbeln,
    name1            TYPE kna1-name1,
    kunnr            TYPE kna1-kunnr,
    rfmng            TYPE vbfa-rfmng,
    rfwrt            TYPE vbfa-rfwrt,
    kursk            TYPE vbrp-kursk,
    erdat            TYPE vbfa-erdat,
    docfat           TYPE vbfa-vbeln,
    unit_fat_us      TYPE vbrp-netwr,
    vlr_fat_us       TYPE bsid-dmbe2,
    unit_fatura      TYPE vbfa-rfwrt,
    vlr_fatura       TYPE bsid-dmbtr,
    base_icms        TYPE konv-kwert,
    aliq_icms        TYPE konv-kbetr,
    vlr_icms         TYPE konv-kwert,
    auart            TYPE vbak-auart,
    audat            TYPE vbak-audat,
    nr_dco           TYPE zdco_produtor-nr_dco,
    id_nomeacao_tran TYPE zdoc_exp-id_nomeacao_tran,
    ds_nome_transpor TYPE znom_transporte-ds_nome_transpor,
    ds_porto         TYPE znom_transporte-ds_porto,
    ds_terminal      TYPE znom_transporte-ds_terminal,
    belnr            TYPE j_1bnfdoc-belnr,
    docnum           TYPE j_1bnfdoc-docnum,
    pstdat           TYPE j_1bnfdoc-pstdat,
    bukrs            TYPE j_1bnfdoc-bukrs,
    series           TYPE j_1bnfdoc-series,
    nftype           TYPE j_1bnfdoc-nftype,
    docdat           TYPE j_1bnfdoc-docdat,
    crenam           TYPE j_1bnfdoc-crenam,
    model            TYPE j_1bnfdoc-model,
    nfnum            TYPE j_1bnfdoc-nfnum,
    branch           TYPE j_1bnfdoc-branch,
    parid            TYPE j_1bnfdoc-parid,
    nfe              TYPE j_1bnfdoc-nfe,
    nfenum           TYPE j_1bnfdoc-nfenum,
    chave_nfe        TYPE zib_nfe_dist_itm-chave_nfe,
    partyp           TYPE j_1bnfdoc-partyp,
    nftot            TYPE j_1bnfdoc-nftot,
    direct           TYPE j_1bnfdoc-direct,
    cancel           TYPE j_1bnfdoc-cancel,
    vtweg            TYPE c LENGTH 15,
    data             TYPE c LENGTH 25,
    user             TYPE sy-uname,
    dt_dde           TYPE zdde-dt_dde,
    nr_dde           TYPE zdde-nr_dde,
    dt_averbacao     TYPE zdde-dt_averbacao,
    nr_registro_expo TYPE zreg_exportacao-nr_registro_expo,
    dt_registro_expo TYPE zreg_exportacao-dt_registro_expo,
    numero_due       TYPE zsdt0170-numero_due,
    dt_due           TYPE zsdt0170-dt_registro,
    dt_averb_due     TYPE zsdt0170-dt_situacao,
    nr_conhec        TYPE string,
    dt_data          TYPE string,
    entrga           TYPE vbfa-vbelv,
    arktx            TYPE vbrp-arktx,
    sg_pais          TYPE c LENGTH 250,
    ds_pais          TYPE c LENGTH 250,
    lgort            TYPE t001l-lgort,
    lgobe            TYPE t001l-lgobe,
    contrato         TYPE zsdt0051-bstkd,
    instrucao        TYPE zsdt0053-instrucao,
    cfop             TYPE j_1bnflin-cfop,
    quantidade_memo  TYPE zdoc_memorando-quantidade_memo,
    nro_sol_ov       TYPE zsdt0053-nro_sol_ov,
    ort01            TYPE kna1-ort01,
    regio            TYPE kna1-regio,
    cgc              TYPE char18,
    container        TYPE zsdt0053-container,
    tp_ato           TYPE zsdt0053-tp_ato,
    nr_drawback      TYPE zsdt0053-nr_drawback,
    qtd_drawback     TYPE zsdt0053-qtd_drawback,
    matnr            TYPE vbrp-matnr,

  END OF ty_saida.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: t_vbfa            TYPE TABLE OF ty_vbfa,
      t_j_1bnflin       TYPE TABLE OF ty_j_1bnflin,
      t_vbfa_aux        TYPE TABLE OF ty_vbfa_aux,
      t_vbfa_fat        TYPE TABLE OF ty_vbfa_fat,
      t_zsdt0053        TYPE TABLE OF zsdt0053,
      t_zsdt0051        TYPE TABLE OF zsdt0051,
      t_vbfa_bkpf       TYPE TABLE OF ty_vbfa_bkpf,
      t_konv            TYPE TABLE OF konv,
      t_lips            TYPE TABLE OF lips,
      t_kna1            TYPE TABLE OF kna1,
      t_vbrp            TYPE TABLE OF ty_vbrp,
      t_dco_produtor    TYPE TABLE OF zdco_produtor,
      t_zdoc_exp        TYPE TABLE OF ty_zdoc_exp,
      t_j_1bnfdoc       TYPE TABLE OF ty_j_1bnfdoc,
      t_j_1bnfdoc_aux   TYPE TABLE OF ty_j_1bnfdoc_aux,
      t_zdde            TYPE TABLE OF ty_zdde,
      t_reg_exportacao  TYPE TABLE OF ty_reg_exportacao,
      t_zsdt0170        TYPE TABLE OF zsdt0170,
      t_zsdt0174        TYPE TABLE OF zsdt0174,
      t_znom_transporte TYPE TABLE OF ty_znom_transporte,
      t_znom_conhec     TYPE TABLE OF ty_znom_conhec,
      t_bkpf            TYPE TABLE OF ty_bkpf,
      t_bsid            TYPE TABLE OF ty_bsid,
      t_bsad            TYPE TABLE OF ty_bsad,
      t_saida           TYPE TABLE OF ty_saida,
      t_j_1bnfe_active  TYPE TABLE OF ty_j_1bnfe_active,
      t_t001l           TYPE TABLE OF ty_t001l,
      gt_zdoc_memo      TYPE TABLE OF ty_zdoc_memo,
      gt_zdoc_memo_aux  TYPE TABLE OF ty_zdoc_memo_aux,
      it_vbrk           TYPE TABLE OF ty_vbrk,
      gt_t005t          TYPE TABLE OF t005t,
      tg_bdc            TYPE TABLE OF bdcdata.


*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA: wa_vbfa            TYPE ty_vbfa,
      wa_j_1bnflin       TYPE ty_j_1bnflin,
      wa_vbfa_aux        TYPE ty_vbfa_aux,
      wa_vbfa_fat        TYPE ty_vbfa_fat,
      wa_zsdt0053        TYPE zsdt0053,
      wa_zsdt0051        TYPE zsdt0051,
      wa_vbfa_bkpf       TYPE ty_vbfa_bkpf,
      wa_konv            TYPE konv,
      wa_lips            TYPE lips,
      wa_kna1            TYPE kna1,
      wa_vbrp            TYPE ty_vbrp,
      wa_dco_produtor    TYPE zdco_produtor,
      wa_zdoc_exp        TYPE ty_zdoc_exp,
      wa_j_1bnfdoc       TYPE ty_j_1bnfdoc,
      wa_j_1bnfdoc_aux   TYPE ty_j_1bnfdoc_aux,
      wa_zdde            TYPE ty_zdde,
      wa_reg_exportacao  TYPE ty_reg_exportacao,
      wa_zsdt0170        TYPE zsdt0170,
      wa_znom_transporte TYPE ty_znom_transporte,
      wa_znom_conhec     TYPE ty_znom_conhec,
      wa_bkpf            TYPE ty_bkpf,
      wa_bsid            TYPE ty_bsid,
      wa_bsad            TYPE ty_bsad,
      wa_saida           TYPE ty_saida,
      wa_j_1bnfe_active  TYPE ty_j_1bnfe_active,
      wa_t001l           TYPE ty_t001l,
      gw_zdoc_memo       TYPE ty_zdoc_memo,
      gw_zdoc_memo_aux   TYPE ty_zdoc_memo_aux,
      wa_vbrk            TYPE ty_vbrk,
      gw_t005t           TYPE t005t,
      wg_bdc             TYPE bdcdata.


*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*

DATA: x_data TYPE d,
      x_hora TYPE sy-uzeit.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*

DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     TYPE sy-repid,
      t_top        TYPE slis_t_listheader,
      t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*PARAMETER: P_VKORG TYPE VBAK-VKORG OBLIGATORY.

  SELECT-OPTIONS : p_vkorg FOR vbak-vkorg OBLIGATORY,
                   p_werks FOR vbap-werks OBLIGATORY,
                   p_audat FOR vbak-audat ,
                   p_dt_av FOR zdde-dt_averbacao,
                   p_auart FOR vbak-auart,
                   p_vbeln FOR vbak-vbeln , " NO INTERVALS NO-EXTENSION,
                   p_parid FOR j_1bnfdoc-parid , "NO INTERVALS NO-EXTENSION,
                   p_vkbur FOR vbak-vkbur  NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETER: p_ativas TYPE char1 AS CHECKBOX DEFAULT 'X',
             p_nativa TYPE char1 AS CHECKBOX .
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETER: p_autor  TYPE char1 AS CHECKBOX  DEFAULT 'X',
             p_rejeit TYPE char1 AS CHECKBOX ,
             p_recus  TYPE char1 AS CHECKBOX ,
             p_cancel TYPE char1 AS CHECKBOX ,
             p_agres  TYPE char1 AS CHECKBOX ,
             p_nenv   TYPE char1 AS CHECKBOX .
SELECTION-SCREEN: END OF BLOCK b3 .

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
PERFORM: f_iniciar_variaves.
IF p_dt_av IS NOT INITIAL.
  PERFORM: f_seleciona_dados_dde.
ELSE.
  PERFORM: f_seleciona_dados.
ENDIF.

PERFORM: f_organiza_dados,
         f_imprime_dados.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*


FORM f_seleciona_dados .
  DATA : t_lips_aux TYPE TABLE OF lips,
         refkey     TYPE vbrp-vbeln,
         ano        TYPE bkpf-gjahr.

  FIELD-SYMBOLS: <fs_reg_exportacao> TYPE ty_reg_exportacao.
  FIELD-SYMBOLS: <fs_znom_conhec> TYPE ty_znom_conhec.

  SELECT vf~vbelv
         vf~vbtyp_n
         vf~vbtyp_v
         vf~vbeln
         vf~rfmng
         vf~rfwrt
         vf~erdat
         vk~vbeln
         vk~vkorg
         vk~vtweg
         vk~auart
         vk~audat
         vk~knumv
         vk~kunnr
         vk~tknum
         vp~werks
         vp~charg
         vp~lgort
         t0~j_1bbranch
         vk~vkbur
  FROM vbfa AS vf
  INNER JOIN vbak  AS vk ON vk~vbeln EQ vf~vbelv
  INNER JOIN vbap  AS vp ON vp~vbeln EQ vk~vbeln
  INNER JOIN t001w AS t0 ON t0~werks EQ vp~werks
  INTO TABLE t_vbfa
    WHERE vf~erdat IN p_audat
       AND vf~vbtyp_n  IN ('M', 'O','P')
       AND vf~vbtyp_v  IN ('C','L')
       AND vk~vkbur IN p_vkbur.


  SELECT vbeln fksto
    FROM vbrk
    INTO TABLE it_vbrk
    FOR ALL ENTRIES IN t_vbfa
  WHERE vbeln EQ t_vbfa-vbelv.

  LOOP AT t_vbfa INTO wa_vbfa.

    READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbelv.

    IF ( wa_vbrk-fksto EQ 'X' ).
      DELETE t_vbfa WHERE vbelv EQ wa_vbrk-vbeln.
    ENDIF.

    CLEAR: wa_vbfa, wa_vbrk.
  ENDLOOP.


  DELETE t_vbfa WHERE: vkorg      NOT IN p_vkorg,
                       j_1bbranch NOT IN p_werks,
                       auart      NOT IN p_auart,
                       vbelv      NOT IN p_vbeln,
                       kunnr      NOT IN p_parid.


  IF t_vbfa[] IS NOT INITIAL.

    SELECT werks  lgort lgobe
      FROM t001l
      INTO TABLE t_t001l
      FOR ALL ENTRIES IN t_vbfa
    WHERE werks EQ t_vbfa-werks
      AND lgort EQ t_vbfa-lgort.

    SELECT vbelv vbeln
      FROM vbfa
      INTO TABLE t_vbfa_fat
      FOR ALL ENTRIES IN t_vbfa
      WHERE vbeln EQ t_vbfa-vbeln
      AND   vbtyp_n IN ('M','O')
      AND   vbtyp_v = 'J'.


    SELECT  vbeln netwr kursk arktx matnr
      FROM vbrp
      INTO TABLE t_vbrp
      FOR ALL ENTRIES IN t_vbfa
      WHERE vbeln EQ t_vbfa-vbeln.

    SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @t_vbfa WHERE knumv EQ @t_vbfa-knumv INTO CORRESPONDING FIELDS OF TABLE @t_konv .

    SELECT *
      FROM kna1
      INTO TABLE t_kna1
      FOR ALL ENTRIES IN t_vbfa
       WHERE kunnr EQ t_vbfa-kunnr.

    SELECT *
      FROM zdco_produtor
      INTO TABLE t_dco_produtor
      FOR ALL ENTRIES IN t_vbfa
       WHERE vbeln EQ t_vbfa-vbelnvk.

    SELECT *
      FROM zsdt0053
      INTO TABLE t_zsdt0053
      FOR ALL ENTRIES IN t_vbfa
     WHERE vbeln EQ t_vbfa-vbelv.

    IF ( t_zsdt0053[] IS NOT INITIAL ).
      SELECT *
        FROM zsdt0051
        INTO TABLE t_zsdt0051
        FOR ALL ENTRIES IN t_zsdt0053
       WHERE nro_sol_ov EQ t_zsdt0053-nro_sol_ov.
    ENDIF.

    SORT t_vbfa BY vbeln.

    LOOP AT t_vbfa INTO wa_vbfa.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_vbfa-vbeln
        IMPORTING
          output = wa_vbfa-vbeln.
      wa_vbfa_aux-vg_refkey = wa_vbfa-vbeln.
      APPEND wa_vbfa_aux TO t_vbfa_aux.
    ENDLOOP.

    IF t_vbfa_aux[] IS NOT INITIAL.
      "--------Numero NF ----------
      SELECT refkey docnum menge cfop
        FROM j_1bnflin
        INTO TABLE t_j_1bnflin
        FOR ALL ENTRIES IN t_vbfa_aux
        WHERE refkey  EQ t_vbfa_aux-vg_refkey.

      IF t_j_1bnflin[] IS NOT INITIAL.
*  "-----Status nfe ---------
        SELECT docnum docsta cancel scssta regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
          FROM j_1bnfe_active
          INTO TABLE t_j_1bnfe_active
          FOR ALL ENTRIES IN t_j_1bnflin
          WHERE docnum EQ t_j_1bnflin-docnum.

        SELECT docnum nfenum cancel nfe pstdat
          FROM j_1bnfdoc
          INTO TABLE t_j_1bnfdoc_aux
          FOR ALL ENTRIES IN t_j_1bnflin
          WHERE docnum EQ t_j_1bnflin-docnum.

** Dados de notas vinculadas a memorando
        SELECT a~docnum b~quantidade_memo nr_memorando
          INTO TABLE gt_zdoc_memo
          FROM zdoc_memo_nf_exp AS a
         INNER JOIN zdoc_memorando AS b ON a~nr_nota_exp EQ b~nr_nota_exp
           FOR ALL ENTRIES IN t_j_1bnflin
         WHERE a~docnum EQ t_j_1bnflin-docnum
           AND b~cancelado EQ ''.

        IF gt_zdoc_memo IS NOT INITIAL.
          SORT gt_zdoc_memo BY docnum.

          LOOP AT gt_zdoc_memo INTO gw_zdoc_memo.
            MOVE-CORRESPONDING gw_zdoc_memo TO gw_zdoc_memo_aux.
            COLLECT gw_zdoc_memo_aux INTO gt_zdoc_memo_aux.
          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

    "-----Documento Exportação------------
    SELECT e~vbeln e~id_nomeacao_tran
*      T~DS_NOME_TRANSPOR  T~DS_PORTO T~DS_TERMINAL
      e~id_dde e~id_registro_expo e~id_due
      FROM zdoc_exp AS e
*      INNER JOIN ZNOM_TRANSPORTE AS T ON T~ID_NOMEACAO_TRAN EQ E~ID_NOMEACAO_TRAN
      INTO TABLE t_zdoc_exp
      FOR ALL ENTRIES IN t_vbfa_fat
      WHERE vbeln EQ t_vbfa_fat-vbelv
        AND NOT EXISTS ( SELECT *
                          FROM zdoc_exp_recusa AS b
                         WHERE b~id_doc_exp = e~id_doc_exp ).

    SELECT id_dde dt_dde nr_dde dt_averbacao
      FROM zdde
      INTO TABLE t_zdde
      FOR ALL ENTRIES IN t_zdoc_exp
      WHERE id_dde EQ t_zdoc_exp-id_dde.

    IF t_zdoc_exp[] IS NOT INITIAL.
      "RE
      SELECT id_registro_expo nr_registro_expo dt_registro_expo id_importador id_nomeacao_tran
        FROM zreg_exportacao
        INTO TABLE t_reg_exportacao
        FOR ALL ENTRIES IN t_zdoc_exp
        WHERE id_registro_expo EQ t_zdoc_exp-id_registro_expo.

      "DU-e
      SELECT *
        FROM zsdt0170
        INTO TABLE t_zsdt0170
        FOR ALL ENTRIES IN t_zdoc_exp
        WHERE id_due EQ t_zdoc_exp-id_due.
    ENDIF.

    LOOP AT t_reg_exportacao ASSIGNING <fs_reg_exportacao>.
      SHIFT <fs_reg_exportacao>-id_nomeacao_tran LEFT DELETING LEADING '0'.
      <fs_reg_exportacao>-id_nomeacao_tran_fk = <fs_reg_exportacao>-id_nomeacao_tran.
    ENDLOOP.

    "RE
    IF t_reg_exportacao[] IS NOT INITIAL.
      SELECT id_nomeacao_tran ds_nome_transpor ds_porto ds_terminal
        FROM znom_transporte
        INTO  TABLE t_znom_transporte
        FOR ALL ENTRIES IN t_reg_exportacao
        WHERE id_nomeacao_tran EQ t_reg_exportacao-id_nomeacao_tran_fk.

      SELECT id_nomeacao_tran nr_conhec dt_data sg_pais_destino
        FROM znom_conhec
        INTO TABLE t_znom_conhec
        FOR ALL ENTRIES IN t_reg_exportacao
        WHERE id_nomeacao_tran EQ t_reg_exportacao-id_nomeacao_tran_fk.
    ENDIF.

    "DU-e
    IF t_zsdt0170[] IS NOT INITIAL.
      SELECT id_nomeacao_tran ds_nome_transpor ds_porto ds_terminal
        FROM znom_transporte
        APPENDING  TABLE t_znom_transporte
        FOR ALL ENTRIES IN t_zsdt0170
        WHERE id_nomeacao_tran EQ t_zsdt0170-id_nomeacao_tran.

      SELECT id_nomeacao_tran nr_conhec dt_data sg_pais_destino
        FROM znom_conhec
        APPENDING TABLE t_znom_conhec
        FOR ALL ENTRIES IN t_zsdt0170
        WHERE id_nomeacao_tran EQ t_zsdt0170-id_nomeacao_tran.

      SELECT *
        FROM zsdt0174
          INTO TABLE t_zsdt0174
          FOR ALL ENTRIES IN t_zsdt0170
          WHERE id_due EQ t_zsdt0170-id_due.

    ENDIF.

*      FOR ALL ENTRIES IN T_ZDOC_EXP
*      WHERE ID_NOMEACAO_TRAN EQ T_ZDOC_EXP-ID_NOMEACAO_TRAN.

    LOOP AT t_znom_conhec ASSIGNING <fs_znom_conhec>.
      MOVE <fs_znom_conhec>-sg_pais_destino TO <fs_znom_conhec>-land1.
    ENDLOOP.

    UNASSIGN <fs_znom_conhec>.

    IF t_znom_conhec[] IS NOT INITIAL.
      SELECT *
        FROM t005t
        INTO TABLE gt_t005t
         FOR ALL ENTRIES IN t_znom_conhec
       WHERE land1 EQ t_znom_conhec-land1
         AND spras EQ sy-langu.
    ENDIF.

    IF t_zsdt0174 IS NOT INITIAL.
      SELECT *
        FROM t005t
        APPENDING TABLE gt_t005t
         FOR ALL ENTRIES IN t_zsdt0174
       WHERE land1 EQ t_zsdt0174-destino_country
         AND spras EQ sy-langu.
    ENDIF.


    " Novo valor fatura
    LOOP AT t_vbfa INTO wa_vbfa.
      ano = ''.
      ano = wa_vbfa-erdat(4).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_vbfa-vbeln
        IMPORTING
          output = wa_vbfa-vbeln.

      wa_vbfa_bkpf-vbeln = wa_vbfa-vbeln.
      wa_vbfa_bkpf-ano = ano.

      APPEND wa_vbfa_bkpf TO t_vbfa_bkpf.

    ENDLOOP.

    IF t_vbfa_bkpf[] IS NOT INITIAL.
      SELECT bukrs gjahr awkey belnr
        FROM bkpf
        INTO TABLE t_bkpf
        FOR ALL ENTRIES IN t_vbfa_bkpf
        WHERE bukrs IN p_vkorg
        AND   gjahr EQ t_vbfa_bkpf-ano
        AND   awkey EQ t_vbfa_bkpf-vbeln.

      IF t_bkpf[] IS NOT INITIAL.
        SELECT bukrs belnr gjahr dmbtr dmbe2 shkzg
          FROM bsid
          INTO TABLE t_bsid
          FOR ALL ENTRIES IN t_bkpf
          WHERE bukrs EQ t_bkpf-bukrs
          AND   belnr EQ t_bkpf-belnr
          AND   gjahr EQ t_bkpf-gjahr.

        SELECT bukrs belnr gjahr dmbtr dmbe2 shkzg
          FROM bsad
          INTO TABLE t_bsad
          FOR ALL ENTRIES IN t_bkpf
          WHERE bukrs EQ t_bkpf-bukrs
          AND   belnr EQ t_bkpf-belnr
          AND   gjahr EQ t_bkpf-gjahr.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM f_organiza_dados .
  DATA: i                  TYPE c LENGTH 1,
        vg_refkey          TYPE j_1bnflin-refkey,
        v_awkey            TYPE bkpf-awkey,
        v_id_nomeacao_tran TYPE znom_conhec-id_nomeacao_tran,
        v_cgc              TYPE bapibranch-cgc_number.

  SORT: t_konv           BY knumv kschl,
        t_kna1           BY kunnr,
        t_dco_produtor   BY vbeln,
        t_vbrp           BY vbeln,
        t_zdoc_exp       BY vbeln,
        t_j_1bnflin      BY refkey,
        t_j_1bnfe_active BY docnum,
        t_zdde           BY id_dde,
        t_reg_exportacao BY id_registro_expo,
        t_znom_conhec    BY id_nomeacao_tran,
        t_vbfa_fat       BY  vbeln,
        t_zsdt0174 BY id_due destino_country.

  DELETE ADJACENT DUPLICATES FROM t_zsdt0174 COMPARING id_due destino_country.

  LOOP AT t_vbfa INTO wa_vbfa.
    CLEAR: wa_saida, wa_konv, wa_kna1, wa_dco_produtor, wa_lips, wa_zdoc_exp, wa_vbrp,
           wa_j_1bnfe_active, wa_zdde, wa_reg_exportacao, wa_znom_conhec, wa_j_1bnflin,
           wa_j_1bnfdoc_aux, vg_refkey, wa_bsid, wa_bsad, wa_bkpf, gw_zdoc_memo, wa_znom_transporte.

    " Dados Fatura
    wa_saida-docfat = wa_vbfa-vbeln.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_vbfa-vbeln
      IMPORTING
        output = wa_vbfa-vbeln.

    v_awkey = wa_vbfa-vbeln.

    READ TABLE t_t001l INTO wa_t001l WITH KEY werks = wa_vbfa-werks
                                              lgort = wa_vbfa-lgort.
    wa_saida-lgort  = wa_t001l-lgort.
    wa_saida-lgobe  = wa_t001l-lgobe.

    READ TABLE t_zsdt0053 INTO wa_zsdt0053 WITH KEY vbeln = wa_vbfa-vbelv.
    IF sy-subrc = 0.

      READ TABLE t_zsdt0051 INTO wa_zsdt0051 WITH KEY nro_sol_ov = wa_zsdt0053-nro_sol_ov.

      IF sy-subrc = 0.
        wa_saida-contrato         = wa_zsdt0051-bstkd.
        wa_saida-instrucao        = wa_zsdt0053-instrucao.
        wa_saida-ds_nome_transpor = wa_zsdt0053-navio.
        wa_saida-ds_porto         = wa_zsdt0053-porto.
      ENDIF.
    ENDIF.

    READ TABLE t_bkpf INTO wa_bkpf WITH KEY awkey = v_awkey.
    READ TABLE t_bsid INTO wa_bsid WITH KEY belnr = wa_bkpf-belnr.

*** Stefanini - IR201148 - 07/10/2024 - LAZAROSR - Início de Alteração
    " Ajuste para quando a nota vir com valor zerado
    IF wa_vbfa-rfmng IS INITIAL.
      wa_vbfa-rfmng = 1.
    ENDIF.
*** Stefanini - IR201148 - 07/10/2024 - LAZAROSR - Fim de Alteração

    IF wa_bsid-shkzg = 'H'.

      wa_saida-rfmng = ( wa_vbfa-rfmng * ( -1 ) ). " Quantidade Faturada

      "valor R$
      wa_saida-vlr_fatura = ( wa_bsid-dmbtr * ( -1 ) ). "R$
      wa_saida-unit_fatura = wa_bsid-dmbtr / wa_vbfa-rfmng.

      "valor USS
      wa_saida-vlr_fat_us  = ( wa_bsid-dmbe2 * ( -1 ) ).
      wa_saida-unit_fat_us = wa_bsid-dmbe2 / wa_vbfa-rfmng.

    ELSE.
      wa_saida-rfmng = wa_vbfa-rfmng. " Quantidade Faturada

      "valor R$
      wa_saida-vlr_fatura = wa_bsid-dmbtr. "R$
      wa_saida-unit_fatura = wa_bsid-dmbtr / wa_vbfa-rfmng.

      " valor USS
      wa_saida-vlr_fat_us  = wa_bsid-dmbe2.
      wa_saida-unit_fat_us = wa_bsid-dmbe2 / wa_vbfa-rfmng.

    ENDIF.

    IF  ( ( wa_saida-vlr_fatura EQ 0 ) OR ( wa_saida-vlr_fat_us EQ 0 ) ).
      READ TABLE t_bsad INTO wa_bsad WITH KEY belnr = wa_bkpf-belnr.

      IF wa_bsad-shkzg = 'H'.

        wa_saida-rfmng = ( wa_vbfa-rfmng * ( -1 ) ). " Quantidade Faturada

        " valor R$
        wa_saida-vlr_fatura = ( wa_bsad-dmbtr * ( -1 ) ). "R$
        wa_saida-unit_fatura = wa_bsad-dmbtr / wa_vbfa-rfmng.

        "  valor USS
        wa_saida-vlr_fat_us  = ( wa_bsad-dmbe2 * ( -1 ) ).
        wa_saida-unit_fat_us = wa_bsad-dmbe2 / wa_vbfa-rfmng.

      ELSE.

        wa_saida-rfmng = wa_vbfa-rfmng. " Quantidade Faturada

        " valor R$
        wa_saida-vlr_fatura = wa_bsad-dmbtr. "R$
        wa_saida-unit_fatura = wa_bsad-dmbtr / wa_vbfa-rfmng.

        "  valor USS
        wa_saida-vlr_fat_us  = wa_bsad-dmbe2.
        wa_saida-unit_fat_us = wa_bsad-dmbe2 / wa_vbfa-rfmng.
      ENDIF.
    ENDIF.

    " Safra
    "wa_saida-charg = wa_vbak-charg.
    wa_saida-charg = wa_vbfa-charg.

    " Empresa
    wa_saida-vkorg = wa_vbfa-vkorg.

    " Canal de Distribuição

    IF wa_vbfa-vtweg EQ 10.
      wa_saida-vtweg = 'Mercado Interno'.
    ELSEIF wa_vbfa-vtweg EQ 20.
      wa_saida-vtweg = 'Mercado Externo'.
    ENDIF.
    " Escritorio de vendas
    wa_saida-vkbur = wa_vbfa-vkbur.


    " Centro
    wa_saida-werks = wa_vbfa-werks.

    "	Numero da Ordem de Venda
    wa_saida-vbeln = wa_vbfa-vbelnvk.

    " Tipo de Doc
    wa_saida-auart = wa_vbfa-auart.

    " Data de emissão da Ordem
    wa_saida-audat = wa_vbfa-audat.

    CALL FUNCTION 'HR_BR_LER_FILIAL_GERAL'
      EXPORTING
        company_code           = wa_saida-vkorg
        branch                 = wa_saida-vkbur
      IMPORTING
        cgc                    = v_cgc
      EXCEPTIONS
        branch_not_found       = 1
        address_not_found      = 2
        company_not_found      = 3
        general_data_not_found = 4
        OTHERS                 = 5.

    WRITE v_cgc USING EDIT MASK '__.___.___/____-__' TO wa_saida-cgc.

    READ TABLE t_zsdt0053 INTO DATA(w_zsdt0053) WITH KEY vbeln = wa_vbfa-vbelnvk.
    IF sy-subrc = 0.
      wa_saida-nro_sol_ov   = w_zsdt0053-nro_sol_ov  .
      wa_saida-container    = w_zsdt0053-container   .
      wa_saida-nr_drawback  = w_zsdt0053-nr_drawback .
      wa_saida-qtd_drawback = w_zsdt0053-qtd_drawback.
      wa_saida-tp_ato       = w_zsdt0053-tp_ato      .
    ENDIF.


    " EMISSOR
    READ TABLE t_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbfa-kunnr BINARY SEARCH.
    wa_saida-name1 = wa_kna1-name1.
    wa_saida-kunnr = wa_kna1-kunnr. "USER STORY 78880
    wa_saida-ort01 = wa_kna1-ort01.
    wa_saida-regio = wa_kna1-regio.

    " Cambio
    READ TABLE t_vbrp INTO wa_vbrp WITH KEY vbeln = wa_vbfa-vbeln BINARY SEARCH.

    wa_saida-kursk = wa_vbrp-kursk.
    wa_saida-arktx = wa_vbrp-arktx.
    wa_saida-matnr = wa_vbrp-matnr.

    " Nr DCO
    READ TABLE t_dco_produtor INTO wa_dco_produtor WITH KEY  vbeln = wa_vbfa-vbelnvk BINARY SEARCH.

    wa_saida-nr_dco = wa_dco_produtor-nr_dco.

    " BASE ICMS
    READ TABLE t_konv INTO wa_konv WITH KEY knumv = wa_vbfa-knumv kschl = 'BX10' BINARY SEARCH.

    wa_saida-base_icms   = wa_konv-kwert.

    " ALIQUOTA ICMS
    READ TABLE t_konv INTO wa_konv WITH KEY knumv = wa_vbfa-knumv kschl = 'BX13' BINARY SEARCH.
    wa_saida-vlr_icms    = wa_konv-kwert.
    IF wa_saida-vlr_icms > 0 .
      wa_saida-aliq_icms   = wa_konv-kbetr.
    ELSE.
      wa_saida-aliq_icms   = 0.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_vbfa-vbeln
      IMPORTING
        output = wa_vbfa-vbeln.

    vg_refkey = wa_vbfa-vbeln.

    READ TABLE t_j_1bnflin INTO wa_j_1bnflin
      WITH KEY refkey = vg_refkey  BINARY SEARCH.
    wa_saida-docnum = wa_j_1bnflin-docnum.
    wa_saida-cfop   = wa_j_1bnflin-cfop.

    READ TABLE gt_zdoc_memo_aux INTO gw_zdoc_memo
      WITH KEY docnum = wa_saida-docnum.
    IF sy-subrc IS INITIAL.
      wa_saida-quantidade_memo = gw_zdoc_memo-quantidade_memo.
    ENDIF.

    READ TABLE t_j_1bnfdoc_aux INTO wa_j_1bnfdoc_aux
      WITH KEY docnum = wa_j_1bnflin-docnum.
    wa_saida-pstdat = wa_j_1bnfdoc_aux-pstdat.
    wa_saida-nfenum = wa_j_1bnfdoc_aux-nfenum .
    wa_saida-cancel = wa_j_1bnfdoc_aux-cancel.


    wa_saida-entrga  = wa_vbfa-vbelv.
    wa_saida-erdat   = wa_vbfa-erdat.

    READ TABLE t_j_1bnfe_active INTO wa_j_1bnfe_active
      WITH KEY docnum = wa_j_1bnflin-docnum BINARY SEARCH.

    IF sy-subrc EQ 0.
      CONCATENATE wa_j_1bnfe_active-regio   "Região do emissor NF-e
                  wa_j_1bnfe_active-nfyear  "Ano da data do documento da NF-e
                  wa_j_1bnfe_active-nfmonth "Mês da data do documento da NF-e
                  wa_j_1bnfe_active-stcd1   "Nº CNPJ do emissor da NF-e
                  wa_j_1bnfe_active-model   "Modelo da nota fiscal
                  wa_j_1bnfe_active-serie   "SERIE
                  wa_j_1bnfe_active-nfnum9  "Nº NF-e de nove posições
                  wa_j_1bnfe_active-docnum9 "NF-e: nº aleatório
                  wa_j_1bnfe_active-cdv     "Dígito controle p/chave de acesso NF-e
             INTO wa_saida-chave_nfe.
    ENDIF.


    READ TABLE t_vbfa_fat INTO wa_vbfa_fat
      WITH KEY vbeln = wa_vbfa-vbeln BINARY SEARCH.

    READ TABLE t_zdoc_exp INTO wa_zdoc_exp
      WITH KEY vbeln = wa_vbfa_fat-vbelv BINARY SEARCH.
    wa_saida-id_nomeacao_tran = wa_zdoc_exp-id_nomeacao_tran.

*    Comentando as variaveis antigas
*    WA_SAIDA-DS_NOME_TRANSPOR = WA_ZDOC_EXP-DS_NOME_TRANSPOR.
*    WA_SAIDA-DS_PORTO         = WA_ZDOC_EXP-DS_PORTO        .
*    WA_SAIDA-DS_TERMINAL      = WA_ZDOC_EXP-DS_TERMINAL     .


    READ TABLE t_zdde INTO wa_zdde WITH KEY id_dde = wa_zdoc_exp-id_dde.
    IF ( sy-subrc EQ 0 ) AND ( wa_zdoc_exp-id_dde IS NOT INITIAL ).
      wa_saida-dt_dde       = wa_zdde-dt_dde.
      wa_saida-nr_dde       = wa_zdde-nr_dde.
      wa_saida-dt_averbacao = wa_zdde-dt_averbacao.
    ELSE.
      READ TABLE t_zsdt0170 INTO wa_zsdt0170 WITH KEY id_due       = wa_zdoc_exp-id_due
                                                      situacao_due = '70'. "Averbada

      IF ( sy-subrc EQ 0 ) AND ( wa_zdoc_exp-id_due IS NOT INITIAL ).
        wa_saida-dt_averb_due = wa_zsdt0170-dt_situacao.
      ENDIF.
    ENDIF.

    CLEAR: v_id_nomeacao_tran.

    READ TABLE t_reg_exportacao INTO wa_reg_exportacao
      WITH KEY id_registro_expo = wa_zdoc_exp-id_registro_expo.
    IF ( sy-subrc EQ 0 ) AND ( wa_zdoc_exp-id_registro_expo IS NOT INITIAL ).
      wa_saida-nr_registro_expo = wa_reg_exportacao-nr_registro_expo.
      wa_saida-dt_registro_expo = wa_reg_exportacao-dt_registro_expo.

*     Incluindo as variaveis novas para o Wa_saida
      READ TABLE t_znom_transporte INTO wa_znom_transporte WITH KEY id_nomeacao_tran = wa_reg_exportacao-id_nomeacao_tran_fk.
      IF sy-subrc EQ 0.
        wa_saida-ds_nome_transpor = wa_znom_transporte-ds_nome_transpor.
        wa_saida-ds_porto         = wa_znom_transporte-ds_porto        .
        wa_saida-ds_terminal      = wa_znom_transporte-ds_terminal     .
      ENDIF.

      v_id_nomeacao_tran = wa_reg_exportacao-id_nomeacao_tran_fk.

    ELSE.
      READ TABLE t_zsdt0170 INTO wa_zsdt0170 WITH KEY id_due = wa_zdoc_exp-id_due.
      IF ( sy-subrc EQ 0 ).
        wa_saida-numero_due = wa_zsdt0170-numero_due.
        wa_saida-dt_due     = wa_zsdt0170-dt_registro_portal.

*       Incluindo as variaveis novas para o Wa_saida
        READ TABLE t_znom_transporte INTO wa_znom_transporte WITH KEY id_nomeacao_tran = wa_zsdt0170-id_nomeacao_tran.
        IF sy-subrc EQ 0.
          wa_saida-ds_nome_transpor = wa_znom_transporte-ds_nome_transpor.
          wa_saida-ds_porto         = wa_znom_transporte-ds_porto        .
          wa_saida-ds_terminal      = wa_znom_transporte-ds_terminal     .
        ENDIF.

        v_id_nomeacao_tran = wa_zsdt0170-id_nomeacao_tran.
      ENDIF.
    ENDIF.

    wa_saida-nr_conhec = ''.
    wa_saida-dt_data   = ''.

    i = '1'.

    LOOP AT t_zsdt0174 INTO DATA(w_zsdt0174) WHERE id_due EQ wa_zdoc_exp-id_due.
      READ TABLE gt_t005t INTO gw_t005t WITH KEY land1 = w_zsdt0174-destino_country.

      wa_saida-sg_pais = COND #( WHEN wa_saida-sg_pais IS INITIAL THEN w_zsdt0174-destino_country ELSE |{ wa_saida-sg_pais }, { w_zsdt0174-destino_country }| ).
      wa_saida-ds_pais = COND #( WHEN wa_saida-ds_pais IS INITIAL THEN gw_t005t-landx ELSE |{ wa_saida-ds_pais }, { gw_t005t-landx }| ).

    ENDLOOP.

    DATA(subrc) = sy-subrc.

    LOOP AT t_znom_conhec INTO wa_znom_conhec WHERE id_nomeacao_tran = v_id_nomeacao_tran.
      READ TABLE gt_t005t INTO gw_t005t WITH KEY land1 = wa_znom_conhec-land1.

      IF i EQ '1' .
        wa_saida-nr_conhec = wa_znom_conhec-nr_conhec.

        CONCATENATE wa_znom_conhec-dt_data+6(2) '.' wa_znom_conhec-dt_data+4(2) '.' wa_znom_conhec-dt_data(4) INTO wa_saida-dt_data.
        IF subrc IS NOT INITIAL.
          CONCATENATE wa_saida-sg_pais wa_znom_conhec-sg_pais_destino INTO wa_saida-sg_pais.
          CONCATENATE wa_saida-ds_pais gw_t005t-landx INTO wa_saida-ds_pais.
        ENDIF.

        i = '2'.
      ELSE.
        CONCATENATE wa_saida-nr_conhec ', ' wa_znom_conhec-nr_conhec INTO wa_saida-nr_conhec.
        CONCATENATE wa_saida-dt_data   ', ' wa_znom_conhec-dt_data+6(2) '.' wa_znom_conhec-dt_data+4(2) '.' wa_znom_conhec-dt_data(4) INTO wa_saida-dt_data.
        IF subrc IS NOT INITIAL.
          CONCATENATE wa_saida-sg_pais ',' wa_znom_conhec-sg_pais_destino INTO wa_saida-sg_pais.
          CONCATENATE wa_saida-ds_pais ',' gw_t005t-landx INTO wa_saida-ds_pais.
        ENDIF.
      ENDIF.

      CLEAR: gw_t005t.

    ENDLOOP.

    x_data = sy-datum.
    x_hora = sy-uzeit.

    CONCATENATE x_data+6(2) '/'
                x_data+4(2) '/'
                x_data(4)   ' -  '
                x_hora(2)   ':'
                x_hora+2(2) ':'
                x_hora+4(2) INTO wa_saida-data.

    wa_saida-user        = sy-uname.

    i = '1'.

    IF ( p_ativas EQ 'X' AND wa_j_1bnfdoc-cancel NE 'X' AND wa_j_1bnfdoc-nfe NE 'X'     ) OR
       ( p_nativa EQ 'X' AND wa_j_1bnfdoc-cancel EQ 'X' AND wa_j_1bnfdoc-nfe NE 'X'     ) OR
       ( p_autor  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
       ( p_rejeit EQ 'X' AND wa_j_1bnfe_active-docsta EQ 2 AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
       ( p_recus  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 3 AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
       ( p_autor  EQ 'X' AND wa_j_1bnfe_active-docsta EQ 1 AND wa_j_1bnfe_active-cancel EQ 'X' AND  wa_j_1bnfdoc-nfe EQ 'X'  ) OR
       ( p_agres  EQ 'X' AND wa_j_1bnfe_active-docsta EQ ' ' AND wa_j_1bnfe_active-scssta NE ' ' AND wa_j_1bnfdoc-nfe EQ 'X'  ) OR
       ( p_nenv   EQ 'X' AND wa_j_1bnfe_active-docsta EQ ' ' AND wa_j_1bnfe_active-scssta EQ   ' '  AND wa_j_1bnfdoc-nfe EQ 'X'  ).
      i = '0'.
    ENDIF.

    IF ( p_ativas EQ 'X' AND p_nativa NE 'X' AND  wa_j_1bnfdoc-cancel EQ 'X' ) OR
       ( p_nativa EQ 'X' AND p_ativas NE 'X' AND  wa_j_1bnfdoc-cancel NE 'X' ) .
      i = '1'.
    ENDIF.

    IF p_dt_av IS NOT INITIAL.
      IF ( wa_saida-dt_averbacao NOT IN p_dt_av ) AND "DDE
         ( wa_saida-dt_averb_due NOT IN p_dt_av ).    "DU-e
        i = '1'.
      ENDIF.
    ENDIF.

    CLEAR wa_j_1bnfdoc.

    IF i EQ '1' .
      CONTINUE.
    ELSE.
      IF wa_saida-cancel NE 'X'.
        APPEND wa_saida TO t_saida.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_ORGANIZA_DADOS


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_DDE
*&---------------------------------------------------------------------*


FORM f_seleciona_dados_dde .
  DATA : t_lips_aux TYPE TABLE OF lips,
         refkey     TYPE vbrp-vbeln,
         ano        TYPE bkpf-gjahr.

  FIELD-SYMBOLS: <fs_reg_exportacao> TYPE ty_reg_exportacao.
  FIELD-SYMBOLS: <fs_znom_conhec> TYPE ty_znom_conhec.

  SELECT id_dde dt_dde nr_dde dt_averbacao
    FROM zdde
    INTO TABLE t_zdde
    "FOR ALL ENTRIES IN t_zdoc_exp
   WHERE dt_averbacao IN p_dt_av.

  SELECT *
    FROM zsdt0170 AS a
    INTO TABLE t_zsdt0170
   WHERE situacao_due  EQ '70' "Averbada
     AND dt_situacao   IN p_dt_av
     AND EXISTS ( SELECT *
                    FROM zdoc_exp AS b
                   WHERE b~id_due EQ a~id_due ).

*  SORT T_ZSDT0170 BY NUMERO_DUE ID_DUE DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM T_ZSDT0170 COMPARING NUMERO_DUE.

  IF t_zdde[] IS NOT INITIAL.
*    Removido o Join do Zdoc
    SELECT e~vbeln e~id_nomeacao_tran
*      T~DS_NOME_TRANSPOR  T~DS_PORTO T~DS_TERMINAL
      e~id_dde e~id_registro_expo e~id_due
      FROM zdoc_exp AS e
*     INNER JOIN ZNOM_TRANSPORTE AS T ON T~ID_NOMEACAO_TRAN EQ E~ID_NOMEACAO_TRAN
      INTO TABLE t_zdoc_exp
       FOR ALL ENTRIES IN t_zdde
     WHERE id_dde EQ t_zdde-id_dde
       AND NOT EXISTS ( SELECT *
                         FROM zdoc_exp_recusa AS b
                        WHERE b~id_doc_exp = e~id_doc_exp ).
  ENDIF.

  IF t_zsdt0170[] IS NOT INITIAL.
    SELECT e~vbeln e~id_nomeacao_tran e~id_dde e~id_registro_expo e~id_due
      FROM zdoc_exp AS e APPENDING TABLE t_zdoc_exp
       FOR ALL ENTRIES IN t_zsdt0170
     WHERE id_due EQ t_zsdt0170-id_due
       AND NOT EXISTS ( SELECT *
                          FROM zdoc_exp_recusa AS b
                         WHERE b~id_doc_exp = e~id_doc_exp ).
  ENDIF.

  IF t_zdoc_exp[] IS NOT INITIAL.
    SELECT vbelv vbeln
      FROM vbfa
      INTO TABLE t_vbfa_fat
       FOR ALL ENTRIES IN t_zdoc_exp
     WHERE vbelv   = t_zdoc_exp-vbeln
       AND vbtyp_n IN ('M','O')
       AND vbtyp_v = 'J'.

    IF t_vbfa_fat[] IS NOT INITIAL.

      SELECT vf~vbelv
             vf~vbtyp_n
             vf~vbtyp_v
             vf~vbeln
             vf~rfmng
             vf~rfwrt
             vf~erdat
             vk~vbeln
             vk~vkorg
             vk~vtweg
             vk~auart
             vk~audat
             vk~knumv
             vk~kunnr
             vk~tknum
             vp~werks
             vp~charg
             vp~lgort
             t0~j_1bbranch
             vk~vkbur
        FROM vbfa AS vf
       INNER JOIN vbak  AS vk ON vk~vbeln EQ vf~vbelv
       INNER JOIN vbap  AS vp ON vp~vbeln EQ vk~vbeln
       INNER JOIN t001w AS t0 ON t0~werks EQ vp~werks
        INTO TABLE t_vbfa
         FOR ALL ENTRIES IN t_vbfa_fat
       WHERE vf~erdat   IN p_audat
         AND vf~vbeln   EQ t_vbfa_fat-vbeln
         AND vf~vbtyp_n IN ('M', 'O','P')
         AND vf~vbtyp_v IN ('C','L')
         AND vk~vkbur   IN p_vkbur.

      SELECT vbeln fksto
        FROM vbrk
        INTO TABLE it_vbrk
         FOR ALL ENTRIES IN t_vbfa
       WHERE vbeln EQ t_vbfa-vbelv.

      LOOP AT t_vbfa INTO wa_vbfa.

        READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbelv.

        IF ( wa_vbrk-fksto EQ 'X' ).
          DELETE t_vbfa WHERE vbelv EQ wa_vbrk-vbeln.
        ENDIF.

        CLEAR: wa_vbfa, wa_vbrk.
      ENDLOOP.

      DELETE t_vbfa WHERE: vkorg      NOT IN p_vkorg,
                           j_1bbranch NOT IN p_werks,
                           auart      NOT IN p_auart,
                           vbelv      NOT IN p_vbeln,
                           kunnr      NOT IN p_parid.

      IF t_vbfa[] IS NOT INITIAL.

        SELECT werks  lgort lgobe
          FROM t001l
          INTO TABLE t_t001l
           FOR ALL ENTRIES IN t_vbfa
         WHERE werks EQ t_vbfa-werks
           AND lgort EQ t_vbfa-lgort.

        "ENDIF.

        SELECT  vbeln netwr kursk arktx matnr
          FROM vbrp
          INTO TABLE t_vbrp
          FOR ALL ENTRIES IN t_vbfa
          WHERE vbeln EQ t_vbfa-vbeln.

        SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @t_vbfa WHERE knumv EQ @t_vbfa-knumv INTO CORRESPONDING FIELDS OF TABLE @t_konv .

        SELECT *
          FROM kna1
          INTO TABLE t_kna1
          FOR ALL ENTRIES IN t_vbfa
           WHERE kunnr EQ t_vbfa-kunnr.

        SELECT *
          FROM zdco_produtor
          INTO TABLE t_dco_produtor
          FOR ALL ENTRIES IN t_vbfa
           WHERE vbeln EQ t_vbfa-vbelnvk.

        SELECT *
          FROM zsdt0053
          INTO TABLE t_zsdt0053
          FOR ALL ENTRIES IN t_vbfa
         WHERE vbeln EQ t_vbfa-vbelv.

        IF ( t_zsdt0053[] IS NOT INITIAL ).
          SELECT *
            FROM zsdt0051
            INTO TABLE t_zsdt0051
            FOR ALL ENTRIES IN t_zsdt0053
           WHERE nro_sol_ov EQ t_zsdt0053-nro_sol_ov.
        ENDIF.

        SORT t_vbfa BY vbeln.

        LOOP AT t_vbfa INTO wa_vbfa.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_vbfa-vbeln
            IMPORTING
              output = wa_vbfa-vbeln.
          wa_vbfa_aux-vg_refkey = wa_vbfa-vbeln.
          APPEND wa_vbfa_aux TO t_vbfa_aux.
        ENDLOOP.

        IF t_vbfa_aux[] IS NOT INITIAL.
          "--------Numero NF ----------
          SELECT refkey docnum menge cfop
            FROM j_1bnflin
            INTO TABLE t_j_1bnflin
            FOR ALL ENTRIES IN t_vbfa_aux
            WHERE refkey  EQ t_vbfa_aux-vg_refkey.

          IF t_j_1bnflin[] IS NOT INITIAL.
*    "-----Status nfe ---------
            SELECT docnum docsta cancel scssta regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
              FROM j_1bnfe_active
              INTO TABLE t_j_1bnfe_active
               FOR ALL ENTRIES IN t_j_1bnflin
             WHERE docnum EQ t_j_1bnflin-docnum.

            SELECT docnum nfenum cancel nfe pstdat
              FROM j_1bnfdoc
              INTO TABLE t_j_1bnfdoc_aux
               FOR ALL ENTRIES IN t_j_1bnflin
             WHERE docnum EQ t_j_1bnflin-docnum.

** Dados de notas vinculadas a memorando
            SELECT a~docnum b~quantidade_memo nr_memorando
              INTO TABLE gt_zdoc_memo
              FROM zdoc_memo_nf_exp AS a
             INNER JOIN zdoc_memorando AS b ON a~nr_nota_exp EQ b~nr_nota_exp
               FOR ALL ENTRIES IN t_j_1bnflin
             WHERE a~docnum EQ t_j_1bnflin-docnum.

            IF gt_zdoc_memo IS NOT INITIAL.
              SORT gt_zdoc_memo BY docnum.

              LOOP AT gt_zdoc_memo INTO gw_zdoc_memo.
                MOVE-CORRESPONDING gw_zdoc_memo TO gw_zdoc_memo_aux.
                COLLECT gw_zdoc_memo_aux INTO gt_zdoc_memo_aux.
              ENDLOOP.

            ENDIF.

          ENDIF.
        ENDIF.
**               incluido Ds transport porto terminal na consulta
*        SELECT EX~ID_REGISTRO_EXPO EX~NR_REGISTRO_EXPO EX~DT_REGISTRO_EXPO EX~ID_IMPORTADOR T~DS_NOME_TRANSPOR  T~DS_PORTO T~DS_TERMINAL
*          FROM ZREG_EXPORTACAO AS EX " Inserido apelido para a tabela
*          INNER JOIN ZNOM_TRANSPORTE AS T ON T~ID_NOMEACAO_TRAN EQ EX~ID_NOMEACAO_TRAN " incluido o Join no Reg_exportação
*          INTO TABLE T_REG_EXPORTACAO
*          FOR ALL ENTRIES IN T_ZDOC_EXP
*          WHERE ID_REGISTRO_EXPO EQ T_ZDOC_EXP-ID_REGISTRO_EXPO.

        "RE
        SELECT id_registro_expo nr_registro_expo dt_registro_expo id_importador id_nomeacao_tran
          FROM zreg_exportacao
          INTO TABLE t_reg_exportacao
          FOR ALL ENTRIES IN t_zdoc_exp
          WHERE id_registro_expo EQ t_zdoc_exp-id_registro_expo.


        LOOP AT t_reg_exportacao ASSIGNING <fs_reg_exportacao>.
          SHIFT <fs_reg_exportacao>-id_nomeacao_tran LEFT DELETING LEADING '0'.
          <fs_reg_exportacao>-id_nomeacao_tran_fk = <fs_reg_exportacao>-id_nomeacao_tran.
        ENDLOOP.

        "RE
        IF t_reg_exportacao[] IS NOT INITIAL.
          SELECT id_nomeacao_tran ds_nome_transpor ds_porto ds_terminal
            FROM znom_transporte
            INTO  TABLE t_znom_transporte
            FOR ALL ENTRIES IN t_reg_exportacao
            WHERE id_nomeacao_tran EQ t_reg_exportacao-id_nomeacao_tran_fk.

          SELECT id_nomeacao_tran nr_conhec dt_data sg_pais_destino
            FROM znom_conhec
            INTO TABLE t_znom_conhec
*            FOR ALL ENTRIES IN T_ZDOC_EXP
            FOR ALL ENTRIES IN t_reg_exportacao
            WHERE id_nomeacao_tran EQ t_reg_exportacao-id_nomeacao_tran_fk.
        ENDIF.

        "DU-e
        IF t_zsdt0170[] IS NOT INITIAL.
          SELECT id_nomeacao_tran ds_nome_transpor ds_porto ds_terminal
            FROM znom_transporte
            APPENDING  TABLE t_znom_transporte
            FOR ALL ENTRIES IN t_zsdt0170
            WHERE id_nomeacao_tran EQ t_zsdt0170-id_nomeacao_tran.

          SELECT id_nomeacao_tran nr_conhec dt_data sg_pais_destino
            FROM znom_conhec
            APPENDING TABLE t_znom_conhec
            FOR ALL ENTRIES IN t_zsdt0170
            WHERE id_nomeacao_tran EQ t_zsdt0170-id_nomeacao_tran.

          SELECT *
            FROM zsdt0174
              INTO TABLE t_zsdt0174
              FOR ALL ENTRIES IN t_zsdt0170
              WHERE id_due EQ t_zsdt0170-id_due.

        ENDIF.

        LOOP AT t_znom_conhec ASSIGNING <fs_znom_conhec>.
          MOVE <fs_znom_conhec>-sg_pais_destino TO <fs_znom_conhec>-land1.
        ENDLOOP.

        UNASSIGN <fs_znom_conhec>.

        IF t_znom_conhec[] IS NOT INITIAL.
          SELECT *
            FROM t005t
            INTO TABLE gt_t005t
             FOR ALL ENTRIES IN t_znom_conhec
           WHERE land1 EQ t_znom_conhec-land1
             AND spras EQ sy-langu.
        ENDIF.

        IF t_zsdt0174 IS NOT INITIAL.
          SELECT *
            FROM t005t
            APPENDING TABLE gt_t005t
             FOR ALL ENTRIES IN t_zsdt0174
           WHERE land1 EQ t_zsdt0174-destino_country
             AND spras EQ sy-langu.
        ENDIF.

        " Novo valor fatura
        LOOP AT t_vbfa INTO wa_vbfa.
          ano = ''.
          ano = wa_vbfa-erdat(4).

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_vbfa-vbeln
            IMPORTING
              output = wa_vbfa-vbeln.

          wa_vbfa_bkpf-vbeln = wa_vbfa-vbeln.
          wa_vbfa_bkpf-ano = ano.
          APPEND wa_vbfa_bkpf TO t_vbfa_bkpf.
        ENDLOOP.

        IF t_vbfa_bkpf[] IS NOT INITIAL.
          SELECT bukrs gjahr awkey belnr
            FROM bkpf
            INTO TABLE t_bkpf
            FOR ALL ENTRIES IN t_vbfa_bkpf
            WHERE bukrs IN p_vkorg
            AND   gjahr EQ t_vbfa_bkpf-ano
            AND   awkey EQ t_vbfa_bkpf-vbeln.

          IF t_bkpf[] IS NOT INITIAL.
            SELECT bukrs belnr gjahr dmbtr dmbe2 shkzg
              FROM bsid
              INTO TABLE t_bsid
              FOR ALL ENTRIES IN t_bkpf
              WHERE bukrs EQ t_bkpf-bukrs
              AND   belnr EQ t_bkpf-belnr
              AND   gjahr EQ t_bkpf-gjahr.

            SELECT bukrs belnr gjahr dmbtr dmbe2 shkzg
              FROM bsad
              INTO TABLE t_bsad
              FOR ALL ENTRIES IN t_bkpf
              WHERE bukrs EQ t_bkpf-bukrs
              AND   belnr EQ t_bkpf-belnr
              AND   gjahr EQ t_bkpf-gjahr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_SELECIONA_DADOS_DDE

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
      i_callback_program       = v_report
      i_callback_pf_status_set = 'SET_PF_STATUS' "US - 182076 - CBRAND
      i_callback_user_command  = 'F_USER_COMMAND'
      it_fieldcat              = estrutura[]
      it_sort                  = t_sort[]
      i_save                   = 'A'
      it_events                = events
      is_print                 = t_print
    TABLES
      t_outtab                 = t_saida.


ENDFORM.

FORM set_pf_status USING rt_extab TYPE slis_t_extab.        "#EC CALLED
*** US - 182076 - Inicio - CBRAND
  DATA(hidden_buttons) = VALUE syucomm_t(
     ( '&REFRESH' ) ).
  SET PF-STATUS 'ZSTANDARD' EXCLUDING hidden_buttons.
*** US - 182076 - Fim - CBRAND
ENDFORM. "Set_pf_status

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
  CLEAR: xs_events, events. "US - 182076 - CBRAND
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*

FORM f_montar_layout.
  PERFORM f_montar_estrutura USING:

  1 'VBAP'             'CHARG'                 'T_SAIDA' 'CHARG'              'Safra'                 ' ',
  2 'VBAK'             'VKORG'                 'T_SAIDA' 'VKORG'              'Empresa'               ' ',
  3 'VBAK'             'VKBUR'                 'T_SAIDA' 'VKBUR'              'Escritorio de Vendas'  ' ',
  3 'VBAP'             'WERKS'                 'T_SAIDA' 'WERKS'              'Centro'                ' ',
  4 'VBAK'             'VTWEG'                 'T_SAIDA' 'VTWEG'              'Canal de Distribuição' ' ',
  5 'VBAK'             'VBELN'                 'T_SAIDA' 'VBELN'              'Ordem de Venda'        ' ',
  5 'ZSDT0053'         'NRO_SOL_OV'            'T_SAIDA' 'NRO_SOL_OV'         'Nr.Sol.OV'             ' ',
  6 'KNA1'             'KUNNR'                 'T_SAIDA' 'KUNNR'              'Cod .Cliente'          ' ',
  6 'KNA1'             'NAME1'                 'T_SAIDA' 'NAME1'              'Emissor da Ordem'      ' ',
  6 'KNA1'             'ORT01'                 'T_SAIDA' 'ORT01'              'Município'             ' ',
  6 'KNA1'             'REGIO'                 'T_SAIDA' 'REGIO'              'UF'                    ' ',
  7 'VBAK'             'AUART'                 'T_SAIDA' 'AUART'              'Tipo de Documento'     ' ',
  8 'VBAK'             'AUDAT'                 'T_SAIDA' 'AUDAT'              'Emissão da Ordem'      ' ',
  9 'VBFA'             'ERDAT'                 'T_SAIDA' 'ERDAT'              'Dt. Fatura'            ' ',
 10 'VBFA'             'VBELN'                 'T_SAIDA' 'DOCFAT'             'Doc. Fatura'           ' ',
 11 'VBFA'             'RFMNG'                 'T_SAIDA' 'RFMNG'              'Qtde Faturada'         ' ',
 11 ' '                ' '                     'T_SAIDA' 'QUANTIDADE_MEMO'    'Qtde de Memorando'     ' ',
 12 'ZDCO_PRODUTOR'    'NR_DCO'                'T_SAIDA' 'NR_DCO'             'DCO'                   ' ',
 13 'VBFA'             'RFWRT'                 'T_SAIDA' 'UNIT_FATURA'        'Vlr Unit Fatura R$'    ' ',
 14 'BSID'             'DMBTR'                 'T_SAIDA' 'VLR_FATURA'         'Vlr Fatura R$'         ' ',
 15 'VBRP'             'NETWR'                 'T_SAIDA' 'UNIT_FAT_US'        'Vlr Unit Fatura U$'    ' ',
 16 'BSID'             'DMBE2'                 'T_SAIDA' 'VLR_FAT_US'         'Vlr Fatura U$'         ' ',
 17 'VBRP'             'KURSK'                 'T_SAIDA' 'KURSK'              'Câmbio'                ' ',
 18 'KONV'             'KWERT'                 'T_SAIDA' 'BASE_ICMS'          'Base do ICMS'          ' ',
 19 'KONV'             'KBETR'                 'T_SAIDA' 'ALIQ_ICMS'          'Alíq do ICMS'          ' ',
 20 'KONV'             'KWERT'                 'T_SAIDA' 'VLR_ICMS '          'Valor do ICMS'         ' ',
 21 'J_1BNFDOC'        'DOCNUM'                'T_SAIDA' 'DOCNUM'             'Doc. Num'              ' ',
 22 'J_1BNFDOC'        'PSTDAT'                'T_SAIDA' 'PSTDAT'             'Dt. de Emissão'        ' ',
 23 'j_1bnflin'        'CFOP'                  'T_SAIDA' 'CFOP'               'CFOP'                  ' ',
 24 'J_1BNFDOC'        'NFENUM'                'T_SAIDA' 'NFENUM'             'Nr. NFE'               ' ',
 25 'ZIB_NFE_DIST_ITM' 'CHAVE_NFE'             'T_SAIDA' 'CHAVE_NFE'          'Chv. NFe'              ' ',
 26 'ZNOM_TRANSPORTE'  'ID_NOMEACAO_TRAN'      'T_SAIDA' 'ID_NOMEACAO_TRAN'   'ID. Nomeação'          ' ',
 26 'ZNOM_TRANSPORTE'  'DS_NOME_TRANSPOR'      'T_SAIDA' 'DS_NOME_TRANSPOR'   'Nome do Navio'         ' ',
 27 'ZNOM_TRANSPORTE'  'DS_PORTO'              'T_SAIDA' 'DS_PORTO'           'Porto'                 ' ',
 28 'ZDDE'             'NR_DDE'                'T_SAIDA' 'NR_DDE'             'Nr. DDE'               ' ',
 29 'ZDDE'             'DT_DDE'                'T_SAIDA' 'DT_DDE'             'Dt. DDE'               ' ',
 30 'ZREG_EXPORTACAO'  'NR_REGISTRO_EXPO'      'T_SAIDA' 'NR_REGISTRO_EXPO'   'Nr. RE'                ' ',
 31 'ZREG_EXPORTACAO'  'DT_REGISTRO_EXPO'      'T_SAIDA' 'DT_REGISTRO_EXPO'   'Dt. RE'                ' ',
 32 'ZSDT0170'         'NUMERO_DUE'            'T_SAIDA' 'NUMERO_DUE'         'Nr. DU-e'              ' ',
 33 'ZSDT0170'         'DT_REGISTRO'           'T_SAIDA' 'DT_DUE'             'Dt. DU-e'              ' ',
 34 ''                 ''                      'T_SAIDA' 'NR_CONHEC'          'Nr. Conhec'            ' ',
 35 ''                 ''                      'T_SAIDA' 'DT_DATA'            'Dt. Conhec'            ' ',
 36 'VBFA'             'VBELV'                 'T_SAIDA' 'ENTRGA'             'Nr. Entrga'            ' ',
 37 'VBRP'             'MATNR'                 'T_SAIDA' 'MATNR'              'Nr. Material'          ' ',
 38 'VBRP'             'ARKTX'                 'T_SAIDA' 'ARKTX'              'Desc. Material'        ' ',
 39 ''                 ''                      'T_SAIDA' 'SG_PAIS'            'Pais Destino'          ' ',
 40 ''                 ''                      'T_SAIDA' 'DS_PAIS'            'Descrição do Pais'     ' ',
 41 'T001L'            'LGORT'                 'T_SAIDA' 'LGORT'              'Depósito'              ' ',
 42 'T001L'            'LGOBE'                 'T_SAIDA' 'LGOBE'              'Terminal'              ' ',
 43 'ZSDT0051'         'BSTKD'                 'T_SAIDA' 'CONTRATO'           'Contrato'              ' ',
 44 'ZSDT0053'         'INSTRUCAO'             'T_SAIDA' 'INSTRUCAO'          'Instrução'             ' ',
 45 'ZDDE'             'DT_AVERBACAO'          'T_SAIDA' 'DT_AVERBACAO'       'Dt. Averb.DDE'         ' ',
 46 'ZSDT0170'         'DT_SITUACAO'           'T_SAIDA' 'DT_AVERB_DUE'       'Dt. Averb.DU-e'        ' ',

 47 ''                 'CGC'                   'T_SAIDA' 'CGC'                'CGC'                            18,
 48 'ZSDT0053'         'CONTAINER'             'T_SAIDA' 'CONTAINER'          'Container'                      ' ',
 49 'ZSDT0053'         'TP_ATO'                'T_SAIDA' 'TP_ATO'             'Tipo Drawback'                  ' ',
 50 'ZSDT0053'         'NR_DRAWBACK'           'T_SAIDA' 'NR_DRAWBACK'        'Numero do Ato'                  ' ',
 51 'ZSDT0053'         'QTD_DRAWBACK'          'T_SAIDA' 'QTD_DRAWBACK'       'Quantidade Drawback(kg)'        ' '.


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
                              VALUE(p_outputlen).

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
  wa_estrutura-outputlen     = COND #( WHEN p_outputlen IS INITIAL THEN x_contador
                                       ELSE p_outputlen ).

  CASE p_field.
    WHEN 'DT_DUE' OR 'DT_AVERB_DUE'.
      wa_estrutura-reptext_ddic = p_scrtext_l.
  ENDCASE.

  IF p_field EQ 'DOCNUM' OR  p_field EQ 'VBELN' OR  p_field EQ 'DOCFAT' OR p_field EQ 'NRO_SOL_OV'.
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
  DATA: w_texto3(20).
* ---> S4 Migration - 19/06/2023 - JS
*  DATA: vg_lines TYPE char02.
  DATA: vg_lines(02) TYPE c.
* <--- S4 Migration - 19/06/2023 - JS

  IF p_audat IS INITIAL AND  p_dt_av IS INITIAL.
    MESSAGE 'Informe um filto : "Data de Emissão" ou "Data de Averbação" !' TYPE 'I'.
    STOP.
  ENDIF.

  v_report = sy-repid.

*** Nome do Report
  PERFORM f_construir_cabecalho USING 'H' TEXT-002.

  SELECT SINGLE butxt FROM t001 INTO w_texto2
    WHERE bukrs IN p_vkorg.

  DESCRIBE TABLE p_vkorg LINES vg_lines.
  IF vg_lines > 1.
    SORT p_vkorg BY low.
    READ TABLE p_vkorg INTO DATA(w_vkorg) INDEX 1.
    DATA(emp_ini) = w_vkorg-low.

    SELECT SINGLE butxt FROM t001 INTO w_texto2
    WHERE bukrs EQ emp_ini.

    SORT p_vkorg DESCENDING BY low.
    READ TABLE p_vkorg INTO w_vkorg INDEX 1.
    DATA(emp_fim) = w_vkorg-low.

    SELECT SINGLE butxt FROM t001 INTO w_texto3
    WHERE bukrs EQ emp_fim.

    CONCATENATE 'Empresa:' emp_ini '-' w_texto2 'Até' emp_fim '-' w_texto3 INTO w_texto1 SEPARATED BY space.
*** Nome da empresa
    PERFORM f_construir_cabecalho USING 'H' w_texto1.

  ELSE.
    CONCATENATE 'Empresa:' p_vkorg '-' w_texto2 INTO w_texto1 SEPARATED BY space.
*** Nome da empresa
    PERFORM f_construir_cabecalho USING 'H' w_texto1.
  ENDIF.
  IF NOT p_werks IS INITIAL.

    SELECT SINGLE name1 FROM t001w INTO w_texto2
      WHERE werks = p_werks.

    CONCATENATE 'Filial:' p_werks  '-' w_texto2 INTO  w_texto1 SEPARATED BY space.
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

  DATA: vl_nfobjn TYPE j_1binterf-nfobjn,
        vl_docnum TYPE j_1bnfdoc-docnum.
  DATA opt TYPE ctu_params.

  READ TABLE t_saida INDEX l_selfield-tabindex INTO wa_saida.

  IF l_selfield-fieldname = 'DOCNUM'.

    vl_docnum = l_selfield-value.

    CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
      EXPORTING
        doc_number         = vl_docnum
      IMPORTING
        obj_number         = vl_nfobjn
      EXCEPTIONS
        document_not_found = 1
        docum_lock         = 2
        OTHERS             = 3.

    CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
      EXPORTING
        obj_number         = vl_nfobjn
      EXCEPTIONS
        object_not_found   = 1
        scr_ctrl_not_found = 2
        OTHERS             = 3.


  ELSEIF   l_selfield-fieldname = 'NRO_SOL_OV'.

    PERFORM f_preencher_dynpro USING:
       'X' 'ZSDR0022'                      '0050',
       ' ' 'WG_HEADER-NRO_SOL_OV'          wa_saida-nro_sol_ov,
       ' ' 'BDC_OKCODE'                    'ATUAL'.

    opt-dismode = 'E'.
    opt-defsize = ' '.

    CALL TRANSACTION 'ZSDT0062' USING tg_bdc OPTIONS FROM opt.
  ELSE.
    CASE l_selfield-fieldname.
      WHEN 'VBELN'.
        SET PARAMETER ID 'AUN' FIELD wa_saida-vbeln.
        CALL TRANSACTION  'VA03' AND SKIP FIRST SCREEN.

      WHEN 'DOCFAT'.
        SET PARAMETER ID 'VF' FIELD wa_saida-docfat.
        CALL TRANSACTION  'VF03' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDIF.

*** US - 182076 - CBRAND - Inicio
  CASE l_ucomm.
    WHEN '&REFR'.
      CLEAR: t_saida[].

      REFRESH: t_vbfa, t_j_1bnflin, t_vbfa_aux, t_vbfa_fat, t_zsdt0053,
         t_zsdt0051, t_vbfa_bkpf, t_konv, t_lips, t_kna1, t_vbrp,
         t_dco_produtor, t_zdoc_exp, t_j_1bnfdoc, t_j_1bnfdoc_aux,
         t_zdde, t_reg_exportacao, t_zsdt0170, t_zsdt0174,
         t_znom_transporte, t_znom_conhec, t_bkpf, t_bsid, t_bsad,
         t_saida, t_j_1bnfe_active, t_t001l, gt_zdoc_memo,
         gt_zdoc_memo_aux, it_vbrk, gt_t005t.


      IF p_dt_av IS NOT INITIAL.
        PERFORM: f_seleciona_dados_dde.
      ELSE.
        PERFORM: f_seleciona_dados.
      ENDIF.

      PERFORM: f_organiza_dados.
               "f_imprime_dados.

      l_selfield-refresh     = 'X'.
      l_selfield-col_stable  = 'X'.
      l_selfield-row_stable  = 'X'.
  ENDCASE.
*** US - 182076 - CBRAND - Fim

ENDFORM.                    "f_user_command

FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wg_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wg_bdc-program,
  l_value TO wg_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wg_bdc-fnam,
      l_value TO wg_bdc-fval.
  ENDIF.
  APPEND wg_bdc TO tg_bdc.
  CLEAR: wg_bdc.

ENDFORM.
