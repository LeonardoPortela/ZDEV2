*&---------------------------------------------------------------------*
*&  Include           ZFIS34TOP
*&---------------------------------------------------------------------*
REPORT zfis34.

*=======================================================================
* TABLES
*=======================================================================
TABLES: zfit0136, zfit0137, zfit0138, zfit0195, t001k, t001w, j_1bnfdoc, j_1bnflin, zib_contabil.

*=======================================================================
* Types
*=======================================================================
TYPES:
  BEGIN OF ty_j_1bagt,
    cfop   TYPE j_1bagt-cfop,
    cfotxt TYPE j_1bagt-cfotxt,
  END OF ty_j_1bagt,

  BEGIN OF ty_t001w,
    werks      TYPE t001w-werks,
    name1      TYPE t001w-name1,
    j_1bbranch TYPE t001w-j_1bbranch,
  END OF ty_t001w,

  BEGIN OF ty_csks,
    kostl TYPE csks-kostl,
    name1 TYPE csks-name1,
  END OF ty_csks,

  BEGIN OF ty_j_1bnfdoc,
    docdat TYPE j_1bnfdoc-docdat,
    pstdat TYPE j_1bnfdoc-pstdat,
    docnum TYPE j_1bnfdoc-docnum,
    nftype TYPE j_1bnfdoc-nftype,
    credat TYPE j_1bnfdoc-credat,
    nftot  TYPE j_1bnfdoc-nftot,
    nfenum TYPE j_1bnfdoc-nfenum,
    series TYPE j_1bnfdoc-series,
    parid  TYPE j_1bnfdoc-parid,
    cretim TYPE j_1bnfdoc-cretim,
    crenam TYPE j_1bnfdoc-crenam,
    direct TYPE j_1bnfdoc-direct,
  END OF ty_j_1bnfdoc,

  BEGIN OF ty_j_1bnflin,
    docnum TYPE j_1bnflin-docnum,
    cfop   TYPE j_1bnflin-cfop,
    werks  TYPE j_1bnflin-werks,
    itmnum TYPE j_1bnflin-itmnum,
    matnr  TYPE j_1bnflin-matnr,
    maktx  TYPE j_1bnflin-maktx,
    refkey TYPE j_1bnflin-refkey,
    menge  TYPE j_1bnflin-menge,
    nfpri  TYPE j_1bnflin-nfpri,
    nfnet  TYPE j_1bnflin-nfnet,
    netwr  TYPE j_1bnflin-netwr,
  END OF ty_j_1bnflin,

  BEGIN OF ty_zib_log,
    obj_key TYPE zib_contabil_chv-obj_key,
    belnr   TYPE zib_contabil_chv-belnr,
  END OF ty_zib_log,

  BEGIN OF ty_zib_err,
    obj_key    TYPE zib_contabil_err-obj_key,
    message    TYPE zib_contabil_err-message,
    message_v1 TYPE zib_contabil_err-message_v1,
  END OF ty_zib_err,

  BEGIN OF ty_saida_principal,
    status(4)           TYPE c,
    filial              TYPE j_1bnflin-werks,
    nome_filial         TYPE t001w-name1,
    centro_custo        TYPE zfit0137-kostl,
    total_me            TYPE j_1bnflin-nfnet,
    total_mi            TYPE j_1bnflin-nfnet,
    total_nf            TYPE j_1bnflin-nfnet,
    p_estorno           TYPE j_1bnflin-nfnet,
    cred_icms           TYPE j_1bnflin-nfnet,
    v_estorno           TYPE j_1bnflin-nfnet,
    nr_documento(255)   TYPE c, " ZIB_CONTABIL_CHV-BELNR,
    nr_doc_estorno(255) TYPE c,
  END OF ty_saida_principal,

  BEGIN OF ty_saida_item,
    docnum          TYPE j_1bnfdoc-docnum,
    nota_fiscal(20) TYPE c,
    dt_lcto         TYPE j_1bnfdoc-pstdat,
    dt_dcto         TYPE j_1bnfdoc-docdat,
    filial          TYPE j_1bnflin-werks,
    filial_name     TYPE t001w-name1,
    matnr           TYPE j_1bnflin-matnr,
    maktx           TYPE j_1bnflin-maktx,
    quantidade      TYPE j_1bnflin-menge,
    preco           TYPE j_1bnflin-nfpri,
    valor_nota      TYPE j_1bnflin-nfnet,
    cfop            TYPE j_1bnflin-cfop,
  END OF ty_saida_item.

*=======================================================================
* STRUCTURES E INTERNAL TABLES
*=======================================================================
DATA: it_zfit0136      TYPE STANDARD TABLE OF zfit0136 WITH DEFAULT KEY,
      it_zfit0137      TYPE STANDARD TABLE OF zfit0137 WITH KEY werks,
      it_zfit0138      TYPE STANDARD TABLE OF zfit0138 WITH KEY bukrs,
      it_aux_136       TYPE STANDARD TABLE OF zfit0136 WITH DEFAULT KEY,
      it_aux_137       TYPE STANDARD TABLE OF zfit0137 WITH KEY werks,
      it_aux_138       TYPE STANDARD TABLE OF zfit0138 WITH KEY bukrs,
      it_aux_195       TYPE STANDARD TABLE OF zfit0195 WITH DEFAULT KEY, "Equalização ECC X HANA #112601  - SMC
      "IT_J_1BNFDOC     TYPE STANDARD TABLE OF TY_J_1BNFDOC WITH KEY DOCNUM,
      it_j_1bnfdoc     TYPE SORTED TABLE OF ty_j_1bnfdoc WITH UNIQUE KEY docnum,
      it_j_1bnfdoc_aux TYPE STANDARD TABLE OF ty_j_1bnfdoc WITH KEY docnum,
      it_j_1bnflin     TYPE STANDARD TABLE OF ty_j_1bnflin WITH KEY docnum,
      it_j_1bnfstx     TYPE STANDARD TABLE OF j_1bnfstx,
      it_j_1bnflin_aux TYPE STANDARD TABLE OF ty_j_1bnflin WITH KEY docnum,
      it_j_1bnflin_tmp TYPE STANDARD TABLE OF ty_j_1bnflin WITH KEY docnum,
      it_t001w         TYPE STANDARD TABLE OF ty_t001w   WITH KEY werks,
      it_zib_log       TYPE STANDARD TABLE OF ty_zib_log WITH KEY obj_key,
      it_zib_err       TYPE STANDARD TABLE OF ty_zib_err WITH KEY obj_key,
      it_saida_prin    TYPE STANDARD TABLE OF ty_saida_principal,
      it_saida_gera    TYPE STANDARD TABLE OF ty_saida_principal,
      it_saida_item    TYPE STANDARD TABLE OF ty_saida_item,
      it_zib_cont      TYPE STANDARD TABLE OF zib_contabil,
      it_zib_cont_aux  TYPE STANDARD TABLE OF zib_contabil,


      wa_zfit0136      LIKE LINE OF it_zfit0136,
      wa_zfit0137      LIKE LINE OF it_zfit0137,
      wa_zfit0138      LIKE LINE OF it_zfit0138,
      wa_aux_136       LIKE LINE OF it_aux_136,
      wa_aux_137       LIKE LINE OF it_aux_137,
      wa_aux_138       LIKE LINE OF it_aux_138,
      wa_aux_195       LIKE LINE OF it_aux_195, "Equalização ECC X HANA #112601  - SMC
      wa_j_1bnfdoc     LIKE LINE OF it_j_1bnfdoc,
      wa_j_1bnfdoc_aux LIKE LINE OF it_j_1bnfdoc_aux,
      wa_j_1bnflin     LIKE LINE OF it_j_1bnflin,
      wa_j_1bnflin_aux LIKE LINE OF it_j_1bnflin_aux,
      wa_j_1bnflin_tmp LIKE LINE OF it_j_1bnflin_tmp,
      wa_j_1bnfstx     LIKE LINE OF it_j_1bnfstx,
      wa_t001w         LIKE LINE OF it_t001w,
      wa_zib_log       LIKE LINE OF it_zib_log,
      wa_zib_err       LIKE LINE OF it_zib_err,
      wa_saida_prin    LIKE LINE OF it_saida_prin,
      wa_saida_gera    LIKE LINE OF it_saida_gera,
      wa_saida_item    LIKE LINE OF it_saida_item,
      wa_zib_cont      LIKE LINE OF it_zib_cont,
      wa_zib_cont_aux  LIKE LINE OF it_zib_cont_aux.


*=======================================================================
* VARIABLES
*=======================================================================
DATA: dia_aux(10) TYPE c,
      dia_inicio  TYPE sy-datum,
      dia_final   TYPE dats,
      mes(05)     TYPE c,
      ano(07)     TYPE c,
      total_me    TYPE j_1bnflin-nfnet,
      total_mi    TYPE j_1bnflin-nfnet,
      total_en    TYPE j_1bnflin-nfnet,
      v_estorno   TYPE j_1bnflin-nfnet,
      xzib(255)   TYPE c,
      l_erro(255) TYPE c,
      xicone(4)   TYPE c,
      v_tpmerc(2) TYPE c,
      v_filial(4) TYPE c,
      count_item  TYPE i.

DATA: r_cfop       TYPE RANGE OF j_1bnflin-cfop WITH HEADER LINE,
      r_werks      TYPE RANGE OF zfit0137-werks WITH HEADER LINE,
      r_centro     TYPE RANGE OF t001w-j_1bbranch WITH HEADER LINE,
      r_centro_aux TYPE RANGE OF t001w-j_1bbranch WITH HEADER LINE,
      r_matkl      TYPE RANGE OF matkl WITH HEADER LINE. "Equalização ECC X HANA #112601  - SMC
DATA v_snum(10) TYPE c.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: it_fcat       TYPE TABLE OF lvc_s_fcat,
      it_fcat2      TYPE TABLE OF lvc_s_fcat,
      gs_variant_c  TYPE disvariant,
      gs_variant_c2 TYPE disvariant.



CLASS cl_gui_cfw DEFINITION LOAD.

DATA: url(255)                TYPE c,
      p_text                  TYPE sdydo_text_element,
      sdydo_text_element(255),
      p_text_table            TYPE sdydo_text_table,
      vl_cont                 TYPE i,
      vl_gtext                TYPE tgsbt-gtext,
      vl_landx                TYPE t005t-landx,

      wa_alv_0001             TYPE REF TO cl_gui_alv_grid,
      wa_container_0001       TYPE REF TO cl_gui_custom_container,
      wa_alv_0002             TYPE REF TO cl_gui_alv_grid,
      wa_container_0002       TYPE REF TO cl_gui_custom_container,

      wa_layout               TYPE lvc_s_layo.

DATA: it_selected_rows TYPE lvc_t_row,                                  "Tabela de linhas selecionadas na alv de saída
      wa_selected_rows TYPE lvc_s_row.

*=======================================================================
* PARAMETERS
*=======================================================================
"DATA: .

*=======================================================================
* SELECTION-SCREEN
*=======================================================================
SELECTION-SCREEN BEGIN OF SCREEN 0101 AS SUBSCREEN.
  SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: p_bukrs  FOR t001k-bukrs OBLIGATORY NO INTERVALS NO-EXTENSION,
                    p_werks  FOR t001w-werks OBLIGATORY,
                    p_mes    FOR zfit0138-monat OBLIGATORY NO INTERVALS NO-EXTENSION,
                    p_ano    FOR zfit0138-gjahr OBLIGATORY NO INTERVALS NO-EXTENSION,
                    p_dtlanc FOR zfit0138-data_atual.
  SELECTION-SCREEN: END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 0101.


*=======================================================================
* START
*=======================================================================
START-OF-SELECTION.
  CALL SCREEN 0100.
