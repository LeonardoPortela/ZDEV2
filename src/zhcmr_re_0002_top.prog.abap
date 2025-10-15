*======================================================================*
* PROJETO            : HCM                                             *
* PROGRAMA           : ZHCMR_RE_0002                                   *
* TRANSACAO          : ZHCM_RE0004                                     *
* DESCRICAO          : Relatório de Conferência Prêmio/Gratificação    *
*======================================================================*
* AUTOR              : Ronaldo Freitas - RFREITAS                      *
* Solicitante        : Bruna Macedo                                    *
* DATA               : 24.06.2024                                      *
*======================================================================*
*                      HISTORICO DE MUDANÇAS                           *
*======================================================================*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*======================================================================*
*&---------------------------------------------------------------------*
*&  Include           ZHCMR_RE_0001_TOP
*&---------------------------------------------------------------------*
REPORT zhcmr_re_0001.
TABLES: zhcmt_f_uniorg, pernr.

TYPES:
  BEGIN OF ty_saida1,
    bukrs          TYPE    pa0001-bukrs,                             "COD. EMPRESA
    bukrs_text     TYPE    butxt, "hrp1000-stext,                            "NOME EMPRESA
    cod_filial     TYPE    pa0001-werks,                             "COD. FILIAL
    nome_filial    TYPE    t500p-name1, "hrp1000-stext,                            "NOME_FILIAL
    matricula      TYPE    pa0001-pernr,                             "Matricula
    cname          TYPE    pa0002-cname,                             "Nome
    edates         TYPE    char10,                                    "DATA ADMISSÃO
    status         TYPE    hrp1000-stext,                            "stat2 + descrição
    cttyp          TYPE    hrp1000-stext,                            "TIPO DE CONTRATOconcatenar pa0016-cttyp + descrição
    ctedt          TYPE    char10,                             "TÉRMINO CONTRATO DET.  pa0016-ctedt
    bet01          TYPE    pa0008-bet01,                             "SALÁRIO BASE  pa0008-bet01
    persk          TYPE    hrp1000-stext,                             "SUBGRUPO EMPREGADOS   pa0001-persk + descrição
    cargo          TYPE    pa0001-stell,                             "CARGO  pa0001-stell
    desc_cargo     TYPE    hrp1000-stext,                            "DESC. CARGO  hrp1000-stext
    pos_ant        TYPE    pa0001-plans,                             "POSIÇÃO ANTERIOR   pa0001-plans
    plans          TYPE    pa0001-plans,                             "POSIÇÃO ANTERIOR   pa0001-plans
    des_pos_ant    TYPE    hrp1000-stext,                            "DESC. POSIÇÃO ANTERIOR   hrp1000-stext
    pos_atual      TYPE    hrp9666-objid,                            "POSIÇÃO ATUAL   hrp9666-objid
    desc_pos_atual TYPE    hrp1000-stext,                            "DESC. POSIÇÃO ATUAL  hrp1000-stext
    tp_p_ant       TYPE    hrp1000-stext,                         "TP PRÊMIO ANTERIOR   hrp9666-ztpbenef  (quero o código e a descrição na frente, assim : PR Prêmio Rescisão, GR Gratificação, PM Prêmio Mensal)
    vlr_p_ant      TYPE    hrp9666-zvalor,                           "VLR PRÊMIO ANTERIOR   hrp9666-zvalor
    tp_p_atual     TYPE    hrp1000-stext,                         "*TP PRÊMIO ATUAL  hrp9666-ztpbenef  (quero o código e a descrição na frente, assim : PR Prêmio Rescisão, GR Gratificação, PM Prêmio Mensal)
    vlr_p_atual    TYPE    hrp9666-zvalor,                           "*VLR PRÊMIO ATUAL  hrp9666-zvalor
    gratif         TYPE    hrp9666-zperc,                            "*% GRATIFICAÇÃO   hrp9666-zperc
    advert         TYPE    i,                                        "*ADVERTENCIA/SUSPENSAO  Contador da pa0030 quantidade de linhas
    falta          TYPE    char01,                                        "*FALTA  Regra de seleção na espec
    vlr_p_z        TYPE    char01,                                   "*VALOR PREMIO ZERADO ?   Dependendo do resultado da pa0015 recebe X ou vazio
    movimenta      TYPE    char01,                                   "*MOVIMENTAÇÃO ?  Depende da regra se encontrou movimentação recebe X
    avos_p_m       TYPE    char02,                                   "*CALC - AVOS PRÊMIO MENSAL    Regra de seleção na espec
    vlr_p_m        TYPE    hrp9666-zvalor,                           "*CALC - VLR PRÊMIO MENSAL    Regra de seleção na espec
    acumul_avos    TYPE    hrp9666-zvalor,                                   "*ACUMULADO - AVOS PRÊMIO RESC  Regra de seleção na espec
    acumul_vlr     TYPE    hrp9666-zvalor,                           "*ACUMULADO - VLR PRÊMIO RESC  Regra de seleção na espec
    avos_p_folha   TYPE    hrp9666-zvalor,                                   "*AVOS PREMIO FOLHA   Regra de seleção na espec
    vlr_p_folha    TYPE    hrp9666-zvalor,                           "*VLR PREMIO FOLHA   Regra de seleção na espec
  END OF ty_saida1,

  BEGIN OF ty_saida2,
    bukrs       TYPE    pa0001-bukrs,                             "COD. EMPRESA
    bukrs_text  TYPE    butxt, "hrp1000-stext,                            "NOME EMPRESA
    cod_filial  TYPE    pa0001-werks,                             "COD. FILIAL
    nome_filial TYPE    t500p-name1, "hrp1000-stext,                            "NOME_FILIAL
    matricula   TYPE    pa0001-pernr,                             "Matricula
    cname       TYPE    pa0002-cname,                             "Nome
    edates      TYPE    char10,                                    "DATA ADMISSÃO
    status      TYPE    hrp1000-stext,                            "stat2 + descrição
    cttyp       TYPE    hrp1000-stext,                            "TIPO DE CONTRATOconcatenar pa0016-cttyp + descrição
    ctedt       TYPE    char10,                             "TÉRMINO CONTRATO DET.  pa0016-ctedt
    bet01       TYPE    pa0008-bet01,                             "SALÁRIO BASE	pa0008-bet01
    persk       TYPE    hrp1000-stext,                             "SUBGRUPO EMPREGADOS   pa0001-persk + descrição
    cargo       TYPE    pa0001-stell,                             "CARGO  pa0001-stell
    desc_cargo  TYPE    hrp1000-stext,                            "DESC. CARGO  hrp1000-stext
    plans       TYPE    pa0001-plans,                             "POSIÇÃO   pa0001-plans
    des_pos     TYPE    hrp1000-stext,                            "DESC. POSIÇÃO  hrp1000-stext
    tp_premio   TYPE    hrp1000-stext,                         "*TP PRÊMIO   hrp9666-ztpbenef  (quero o código e a descrição na frente, assim : PR Prêmio Rescisão, GR Gratificação, PM Prêmio Mensal)
    vlr_p_atual TYPE    hrp9666-zvalor,                           "*VLR PRÊMIO 	hrp9666-zvalor
    competencia TYPE    char10,
    compi       TYPE    sy-datum,
    compf       TYPE    sy-datum,                                "*COMPETENCIA   conforme regra da pc_payresult
    advert      TYPE    i,                                        "*ADVERTENCIA/SUSPENSAO  Contador da pa0030 quantidade de linhas
    falta       TYPE    char01,                                  "*FALTA  Regra de seleção na espec
    vlr_p_z     TYPE    char01,                                   "*VALOR PREMIO ZERADO ?   Dependendo do resultado da pa0015 recebe X ou vazio
    acumul_avos TYPE    hrp9666-zvalor,                           "*ACUMULADO - AVOS PRÊMIO RESC  Regra de seleção na espec
    acumul_vlr  TYPE    hrp9666-zvalor,                           "*ACUMULADO - VLR PRÊMIO RESC  Regra de seleção na espec
  END OF ty_saida2.

DATA: BEGIN OF it_tab,
        mes(2) TYPE n,
        ano(4) TYPE n,
      END OF it_tab.

DATA:
*      it_zhcmr TYPE STANDARD TABLE OF ty_zhcmrre,
*      it_saida TYPE TABLE OF ty_zhcmrre,
  it_saida1 TYPE TABLE OF ty_saida1,
  it_saida2 TYPE TABLE OF ty_saida2,
  wa_saida1 TYPE ty_saida1,
  wa_saida2 TYPE ty_saida2.
*      wa_saida TYPE ty_zhcmrre.


data: rg_ano TYPE RANGE OF zhcmt0004-anopr,
      rg_mes TYPE RANGE OF zhcmt0004-mespr.

*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Início de Alteração
DATA:
      vg_n_encontrou_dados TYPE c.
*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Fim de Alteração

DATA: gs_variant       TYPE disvariant,
      wa_layout        TYPE lvc_s_layo,
      lva_data(22)     TYPE          c,
      gob_gui_alv_grid TYPE REF TO   cl_gui_alv_grid,
      git_fcat_pend    TYPE          lvc_t_fcat,
      git_fcat_pend1   TYPE          lvc_t_fcat,
      git_fcat_pend2   TYPE          lvc_t_fcat,
      message          TYPE itex132.

*======================================================================*
*** Infotipos
*======================================================================*
INFOTYPES: 0000 NAME p0000.
INFOTYPES: 0001 NAME p0001.
INFOTYPES: 0002 NAME p0002.
INFOTYPES: 0004 NAME p0004.
INFOTYPES: 0006 NAME p0006.
INFOTYPES: 0007 NAME p0007.
INFOTYPES: 0008 NAME p0008.
INFOTYPES: 0009 NAME p0009.
INFOTYPES: 0021 NAME p0021.
INFOTYPES: 0030 NAME p0030.
INFOTYPES: 0040 NAME p0040.
INFOTYPES: 0041 NAME p0041.
INFOTYPES: 0057 NAME p0057.
INFOTYPES: 0105 NAME p0105.
INFOTYPES: 0167 NAME p0167.
INFOTYPES: 0169 NAME p0169.
INFOTYPES: 0398 NAME p0398.
INFOTYPES: 0465 NAME p0465.
INFOTYPES: 0598 NAME p0598.
INFOTYPES: 0625 NAME p0625.
INFOTYPES: 0661 NAME p0661.
INFOTYPES: 9003 NAME p9003.
INFOTYPES: 9004 NAME p9004.
INFOTYPES: 1005 NAME p1005.

*======================================================================*
*** Screen
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK bloco1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
    p_mes FOR it_tab-mes NO INTERVALS NO-EXTENSION OBLIGATORY,
    p_ano FOR it_tab-ano NO INTERVALS NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN END  OF BLOCK bloco1.

SELECTION-SCREEN BEGIN OF BLOCK bloco2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS:
    p_pernr  FOR p0000-pernr NO INTERVALS,           "Matricula
    p_plans  FOR pernr-plans NO INTERVALS,           "Posição
    p_bukrs  FOR pernr-bukrs NO INTERVALS,           "Empresa
    p_werks  FOR pernr-werks NO INTERVALS,           "Area RH
    p_kostl  FOR pernr-kostl NO INTERVALS.           "Centro Custo
SELECTION-SCREEN END  OF BLOCK bloco2.

SELECTION-SCREEN BEGIN OF BLOCK bloco3 WITH FRAME TITLE TEXT-003.
  PARAMETERS p_check AS CHECKBOX DEFAULT abap_off.
  SELECT-OPTIONS:
     p_datad FOR pernr-dayps NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END  OF BLOCK bloco3.

*======================================================================*
*** Lógica Principal
**======================================================================*
START-OF-SELECTION.

* Vamos ver a opção 1 - SEM MARCAR O X NO CAMPO SIMULAR RESCISÃO
  IF p_mes IS NOT INITIAL.
    READ TABLE p_mes INTO DATA(lv_mes) INDEX 1.
    IF lv_mes-low LT 1 OR lv_mes-low GT 12 OR lv_mes-low CN '0123456789'.
      MESSAGE s836(sd) WITH TEXT-005 lv_mes-low '.' DISPLAY LIKE 'E'.
      DATA(lv_erro) = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_ano IS NOT INITIAL.
    READ TABLE p_ano INTO DATA(lv_ano) INDEX 1.
    IF lv_ano-low LT 1500 OR lv_ano-low CN '0123456789'.
      MESSAGE s836(sd) WITH TEXT-006 lv_ano-low '.' DISPLAY LIKE 'E'.
      lv_erro = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_check IS NOT INITIAL AND p_datad IS INITIAL.
    MESSAGE s836(sd) WITH TEXT-007 '.' DISPLAY LIKE 'E'.
    lv_erro = abap_true.
    EXIT.
  ENDIF.

  IF lv_erro IS INITIAL.
    PERFORM:
               fm_selecao,
               fm_exibirdados.
  ENDIF.
*======================================================================*
*** Fim Lógica Principal
*======================================================================*
