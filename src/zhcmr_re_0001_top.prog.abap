*&---------------------------------------------------------------------*
*&  Include           ZHCMR_RE_0001_TOP
*&---------------------------------------------------------------------*
REPORT zhcmr_re_0001.
TABLES: zhcmt_f_uniorg.


TYPES:
  BEGIN OF ty_zhcmrre,
    orgeh             TYPE    zhcmt_f_uniorg-orgeh,                     "Cod. Uniorg
    area_folha        TYPE    zhcmt_f_uniorg-area_folha,                "Área Proc. Folha
    cod_ccusto        TYPE    zhcmt_f_uniorg-cod_ccusto,                "Cod. Centro Custo
    cod_filial        TYPE    zhcmt_f_uniorg-cod_filial,                "Cod. Filial
    cod_empresa       TYPE    zhcmt_f_uniorg-cod_empresa,               "Cod. Empresa
    stext             TYPE    zhcmt_f_uniorg-stext,                     "Nome Uniorg
    pernr_gestor      TYPE    zhcmt_f_uniorg-pernr_gestor,              "Pernr Gestor
    cpf_gestor        TYPE    zhcmt_f_uniorg-cpf_gestor,                "CPF Gestor
    nome_gestor       TYPE    zhcmt_f_uniorg-nome_gestor,               "Nome Gestor
    email_gestor      TYPE    zhcmt_f_uniorg-email_gestor,              "E-mail Gestor
    nome_ccusto       TYPE    zhcmt_f_uniorg-nome_ccusto,               "Nome Centro Custo
    nome_filial       TYPE    zhcmt_f_uniorg-nome_filial,               "Nome Filial
    nome_empresa      TYPE    zhcmt_f_uniorg-nome_empresa,              "Nome Empresa
    persk_gestor      TYPE    zhcmt_f_uniorg-persk_gestor,              "Sub. Grup. Empreg.
    desc_persk_gestor	TYPE    zhcmt_f_uniorg-desc_persk_gestor ,        "Nome Sub. Grup. Empreg.
  END OF ty_zhcmrre,

  BEGIN OF ty_saida,
    orgeh             TYPE    zhcmt_f_uniorg-orgeh,                     "Cod. Uniorg
    area_folha        TYPE    zhcmt_f_uniorg-area_folha,                "Área Proc. Folha
    cod_ccusto        TYPE    zhcmt_f_uniorg-cod_ccusto,                "Cod. Centro Custo
    cod_filial        TYPE    zhcmt_f_uniorg-cod_filial,                "Cod. Filial
    cod_empresa       TYPE    zhcmt_f_uniorg-cod_empresa,               "Cod. Empresa
    stext             TYPE    zhcmt_f_uniorg-stext,                     "Nome Uniorg
    pernr_gestor      TYPE    zhcmt_f_uniorg-pernr_gestor,              "Pernr Gestor
    cpf_gestor        TYPE    zhcmt_f_uniorg-cpf_gestor,                "CPF Gestor
    nome_gestor       TYPE    zhcmt_f_uniorg-nome_gestor,               "Nome Gestor
    email_gestor      TYPE    zhcmt_f_uniorg-email_gestor,              "E-mail Gestor
    nome_ccusto       TYPE    zhcmt_f_uniorg-nome_ccusto,               "Nome Centro Custo
    nome_filial       TYPE    zhcmt_f_uniorg-nome_filial,               "Nome Filial
    nome_empresa      TYPE    zhcmt_f_uniorg-nome_empresa,              "Nome Empresa
    persk_gestor      TYPE    zhcmt_f_uniorg-persk_gestor,              "Sub. Grup. Empreg.
    desc_persk_gestor	TYPE    zhcmt_f_uniorg-desc_persk_gestor ,        "Nome Sub. Grup. Empreg.
  END OF ty_saida.

DATA: it_zhcmr TYPE STANDARD TABLE OF ty_zhcmrre,
      it_saida TYPE TABLE OF ty_zhcmrre,
      wa_saida TYPE ty_zhcmrre.

DATA: gs_variant       TYPE disvariant,
      wa_layout        TYPE lvc_s_layo,
      lva_data(22)     TYPE          c,
      gob_gui_alv_grid TYPE REF TO   cl_gui_alv_grid,
      git_fcat_pend    TYPE          lvc_t_fcat,
      message            TYPE itex132.

SELECTION-SCREEN BEGIN OF BLOCK bloco1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  p_uorg FOR zhcmt_f_uniorg-orgeh,
  p_cust FOR zhcmt_f_uniorg-cod_ccusto,
  p_gest FOR zhcmt_f_uniorg-pernr_gestor.
SELECTION-SCREEN END  OF BLOCK bloco1.


START-OF-SELECTION.

  PERFORM:
  fm_selecao,
  fm_exibirdados.
