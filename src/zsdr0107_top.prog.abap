*======================================================================*
* PROJETO            : SAP Ninjas                                      *
* PROGRAMA           : ZSDR0107                                        *
* TRANSACAO          : ZSDT0199                                        *
* DESCRICAO          : Relatório Impostos Retidos                      *
*======================================================================*
* AUTOR              : Ronaldo Freitas                                 *
* Solicitante        : Rogerval Dias                                   *
* DATA               : 15.08.2024                                      *
*======================================================================*
*                      HISTORICO DE MUDANÇAS                           *
*======================================================================*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*======================================================================*
*&---------------------------------------------------------------------*
*&  Include           ZSDR0107_TOP
*&---------------------------------------------------------------------*
REPORT zsdr0107.
TABLES: j_1bnfdoc, j_1bnflin, vbak, konv,
        j_1bnfe_active, vbfa, t685, zsdt0264.

TYPES:
  BEGIN OF ty_saida,
    branch    TYPE j_1bnfdoc-branch,  " Filial
    cfop      TYPE j_1bnflin-cfop,    " Cfop
    nfenum    TYPE j_1bnfdoc-nfenum,  " NF nº
    series    TYPE j_1bnfdoc-series,  " Serie
    docnum    TYPE j_1bnfdoc-docnum,  " Docnum Fiscal
    refitm    TYPE j_1bnflin-refitm,  " Item
    belnr     TYPE j_1bnfdoc-belnr,   " Doc Contábil
    insc_est  TYPE j_1bnfdoc-munins,  " Insc. Estadual
    parid     TYPE j_1bnfdoc-parid,   " Cliente
    cpf_cnpj  TYPE char20,            " Cpf / Cnpj
    uf_c_for  TYPE j_1bnfdoc-regio,   " Uf
    matnr     TYPE j_1bnflin-matnr,   " Produto
    maktx     TYPE j_1bnflin-maktx,   " Descr. Material
    pstdat    TYPE char10,            " Data
    menge     TYPE j_1bnflin-menge,   " Quantidade
    meins     TYPE j_1bnflin-meins,   " Unidade Medida
    fator_f   TYPE zsdt0343_t2-fator, " Valor FETHAB  ( kschl = ZSFE / ZGFE / ZAFE / ZMFE / ZFCF / ZFOF )
    fator_i   TYPE zsdt0343_t2-fator, " Valor IAGRO ( kschl = ZSFA )
    fator_fa  TYPE zsdt0343_t2-fator, " Valor FABOV (kschl = ZGFA )
    fator_im  TYPE zsdt0343_t2-fator, " Valor IMAmt   ( kschl = ZIMM )
    fator_ima TYPE zsdt0343_t2-fator,      " Valor IMAFIR  ( kschl = ZFCI / ZFOI )
    valor_nf  TYPE j_1bnfdoc-nftot,        " Valor NF
    base_icms TYPE j_1bnflin-vicmsstret,   " Base icms
    vlr_icms  TYPE j_1bnflin-vbcstret,     " Vlr_icms
    outros    TYPE char50,                 " Outros
    isentos   TYPE char20,                 " Isentos
    vlr_unit  TYPE j_1bnflin-netwr,        " Vlr Unit
    terminal  TYPE char20,                 " Terminal
    scssta    TYPE j_1bnfe_active-scssta,  " Status SAP
    docsta    TYPE j_1bnfe_active-docsta,  " Status NFE
    chavea    TYPE zde_chave_doc_e,        " J_1BNFE_ACTIVE "CHAVE Nf-e
    act       TYPE char1,
  END OF ty_saida.

DATA: BEGIN OF it_tab,
        mes(2) TYPE n,
        ano(4) TYPE n,
      END OF it_tab.

DATA:
  it_saida TYPE TABLE OF ty_saida,
  wa_saida TYPE ty_saida.

DATA: gs_variant       TYPE disvariant,
      wa_layout        TYPE lvc_s_layo,
      lv_erro,
      lva_data(22)     TYPE          c,
      gob_gui_alv_grid TYPE REF TO   cl_gui_alv_grid,
      git_fcat_pend    TYPE          lvc_t_fcat,
      message          TYPE itex132.

RANGES: r_docnum  FOR j_1bnfdoc-docnum.

*======================================================================*
*** Screen
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK bloco1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
    p_bukrs   FOR j_1bnfdoc-bukrs,   " Empresa
    p_branch  FOR j_1bnfdoc-branch,  " Local de Neg
    p_docnum  FOR j_1bnfdoc-docnum,  " Docnum
    p_chave   FOR zsdt0264-chave NO INTERVALS, " ChaveAcesso
    p_nfenum  FOR j_1bnfdoc-nfenum,  " NF-e
    p_series  FOR j_1bnfdoc-series,  " Série
    p_pstdat  FOR j_1bnfdoc-pstdat,  " Data
    p_parid   FOR j_1bnfdoc-parid,   " Cliente
    p_matkl   FOR j_1bnflin-matkl,   " Grp.Merc.
    p_vbeln   FOR vbak-vbeln,        " OV
    p_auart   FOR vbak-auart,        " Tipo Ov
    p_kschl   FOR t685-kschl.        " Imposto
SELECTION-SCREEN END  OF BLOCK bloco1.

*======================================================================*
*** Lógica Principal
**======================================================================*
START-OF-SELECTION.
* Validações
  IF p_docnum[] IS INITIAL AND p_chave[] IS INITIAL.
    IF p_bukrs IS INITIAL.
      MESSAGE s836(sd) WITH 'Campo empresa obrigatório!' DISPLAY LIKE 'E'.
      lv_erro = abap_true.
      EXIT.
    ENDIF.
    IF p_branch IS INITIAL.
      MESSAGE s836(sd) WITH 'Campo Local de Negócios obrigatório!' DISPLAY LIKE 'E'.
      lv_erro = abap_true.
      EXIT.
    ENDIF.
     IF p_pstdat IS INITIAL.
      MESSAGE s836(sd) WITH 'Campo Data de Lançamento obrigatório!' DISPLAY LIKE 'E'.
      lv_erro = abap_true.
      EXIT.
    ENDIF.
  ENDIF.




  IF lv_erro IS INITIAL.
    PERFORM:
               fm_selecao,
               fm_exibirdados.
  ENDIF.
*======================================================================*
*** Fim Lógica Principal
*======================================================================*
