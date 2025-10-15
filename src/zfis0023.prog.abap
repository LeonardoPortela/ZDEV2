************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 23.09.2011                                          *
* Objetivo    ...: Geração Arquivo ANEEL - Contabilidade               *
* Transação   ...: ZFIS23                                              *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 20.09.2011   Camila Brand   Criação       10:46           DEVK918943 *
************************************************************************


REPORT  zfis0023.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
TABLES:
  bkpf , " Cabeçalho do documento contábil
  bseg , " Segmento do documento contabilidade financeira
  aufk , " Dados mestre da ordem
  skb1 , " Mestre da Conta do Razão (empresa)
  skat , " Mestre de contas do Razão (plano de contas: denominação)
  t077s, " Grupos de contas do Razão Especial
  t001 . " Empresas
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

  ty_arquivo(100) TYPE c,

  BEGIN OF ty_bkpf,
    bukrs TYPE bkpf-bukrs,
    belnr TYPE bkpf-belnr,
    gjahr TYPE bkpf-gjahr,
    monat TYPE bkpf-monat,
    budat TYPE bkpf-budat,
    bldat TYPE bkpf-bldat,
    blart TYPE bkpf-blart,
  END OF ty_bkpf,

  BEGIN OF ty_bseg,
    bukrs TYPE bseg-bukrs,
    belnr TYPE bseg-belnr,
    gjahr TYPE bseg-gjahr,
    shkzg TYPE bseg-shkzg,
    dmbtr TYPE bseg-dmbtr,
    hkont TYPE bseg-hkont,
    sgtxt TYPE bseg-sgtxt,
    gsber TYPE bseg-gsber,
    bschl TYPE bseg-bschl,
    altkt TYPE bseg-altkt,
    kostl TYPE bseg-kostl,
    buzei TYPE bseg-buzei,
    umskz TYPE bseg-umskz,
    aufnr TYPE bseg-aufnr,
  END OF ty_bseg,

  BEGIN OF ty_bseg_hkont,
    belnr TYPE bseg-belnr,
  END OF ty_bseg_hkont,

  BEGIN OF ty_aufk,
    aufnr TYPE aufk-aufnr,
    auart TYPE aufk-auart,
    objnr TYPE aufk-objnr,
    bukrs TYPE aufk-bukrs,
  END OF ty_aufk,

  BEGIN OF ty_cobrb_aux,
    objnr TYPE cobrb-objnr,
    gbisj TYPE cobrb-gbisj,
    gbisp TYPE cobrb-gbisp,
    anln1 TYPE cobrb-anln1,
    anln2 TYPE cobrb-anln2,
    hkont TYPE cobrb-hkont,
  END OF ty_cobrb_aux,


  BEGIN OF ty_cobrb,
    objnr TYPE cobrb-objnr,
    gbisj TYPE cobrb-gbisj,
    gbisp TYPE cobrb-gbisp,
    anln1 TYPE cobrb-anln1,
    anln2 TYPE cobrb-anln2,
    hkont TYPE cobrb-hkont,
    bukrs TYPE coas-bukrs,
  END OF ty_cobrb,


  BEGIN OF ty_anla,
    bukrs TYPE anla-bukrs,
    anln1 TYPE anla-anln1,
    anln2 TYPE anla-anln2,
    anlkl TYPE anla-anlkl,
  END OF ty_anla,

  BEGIN OF ty_anka,
    anlkl TYPE anka-anlkl,
    ktogr TYPE anka-ktogr,
  END OF ty_anka,

  BEGIN OF ty_t095,
    ktopl  TYPE t095-ktopl,
    ktogr  TYPE t095-ktogr,
    afabe  TYPE t095-afabe,
    ktansw TYPE t095-ktansw,
  END OF ty_t095,

  BEGIN OF ty_skb1,
    bukrs TYPE skb1-bukrs,
    saknr TYPE skb1-saknr,
    altkt TYPE skb1-altkt,
    xspeb TYPE skb1-xspeb,
  END OF ty_skb1,

  BEGIN OF ty_skb1_aux,
    bukrs         TYPE skb1-bukrs,
    saknr         TYPE skb1-saknr,
    altkt         TYPE skb1-altkt,
    xcredito      TYPE bseg-dmbtr,
    xdebito       TYPE bseg-dmbtr,
    dscanell(132) TYPE c,
  END OF ty_skb1_aux,

  BEGIN OF ty_skat,
    spras TYPE skat-spras,
    ktopl TYPE skat-ktopl,
    saknr TYPE skat-saknr,
    txt50 TYPE skat-txt50,
  END OF ty_skat,

  BEGIN OF ty_ska1,
    ktopl TYPE ska1-ktopl,
    saknr TYPE ska1-saknr,
    ktoks TYPE ska1-ktoks,
    xloev TYPE ska1-xloev,
    xspea TYPE ska1-xspea,
    xspeb TYPE ska1-xspeb,
    xspep TYPE ska1-xspep,
  END OF ty_ska1,

  BEGIN OF ty_t077z,
    ktopl TYPE t077z-ktopl,
    ktoks TYPE t077z-ktoks,
    txt30 TYPE t077z-txt30,
  END OF ty_t077z,

  BEGIN OF ty_t001,
    bukrs TYPE t001-bukrs,
    butxt TYPE t001-butxt,
  END OF ty_t001,


  BEGIN OF ty_csks,
    kostl TYPE csks-kostl,
    kosar TYPE csks-kosar,
  END OF ty_csks,

  BEGIN OF ty_saida,
    bukrs         TYPE  bkpf-bukrs,   " Empresa
    butxt         TYPE  t001-butxt,   " Empresa
    monat         TYPE  bkpf-monat,   " MEs
    gjahr         TYPE  bkpf-gjahr,   " Ano
    gsber         TYPE  bseg-gsber,   " Divisão
    hkont         TYPE  bseg-hkont,   " Conta
    txt50         TYPE  skat-txt50,   " Descrição Conta
    altkt         TYPE  skb1-altkt,   " Conta ANEEL
    dscanell(132) TYPE c     ,   " Descricao conta aneel
    belnr         TYPE  bkpf-belnr,   " Documento
    budat         TYPE  bkpf-budat,   " Dt.Lcto
    bldat         TYPE  bkpf-bldat,   " Dt.Docto
    blart         TYPE  bkpf-blart,   " Tp.Docto
    bschl         TYPE  bseg-bschl,   " Ch.lcto
    kostl         TYPE  bseg-kostl,   " Centro de Custo
    sgtxt         TYPE  bseg-sgtxt,   " Texto Item
    shkzg         TYPE  bseg-shkzg,   " D/C
    xdebito       TYPE  bseg-dmbtr,   " Vlr.Debito    =  Se   BSEG-SHKZG=”S”
    xcredito      TYPE  bseg-dmbtr,   " Vlr.Debito     =   Se  BSEG-SHKZG=”H”
    xsaldo        TYPE  bseg-dmbtr,   " Saldo  R$    = (Vlr.Debito – Vlr.Credito)
    saknr         TYPE  skb1-saknr ,   "Nº conta do Razão
    aufnr         TYPE  bseg-aufnr,   " Nº ordem
  END OF ty_saida,

  BEGIN OF ty_saida_arq,
    bukrs(4)     TYPE  c, "Empresa
    mes(2)       TYPE  c, "Mes
    ano(4)       TYPE  c, "Ano
    altkt(9)     TYPE  c, "Conta ANEEL
    xdebito(20)  TYPE  c, "Vlr.Debito    =  Se   BSEG-SHKZG=”S”
    xcredito(20) TYPE  c, "Vlr.Debito     =   Se  BSEG-SHKZG=”H”
    xsaldo(20)   TYPE  c, "Saldo  R$    = (Vlr.Debito – Vlr.Credito)
  END OF ty_saida_arq,

  BEGIN OF ty_saida_pn,
    saknr         TYPE skb1-saknr , "Nº conta do Razão
    txt50         TYPE skat-txt50 , "Descrição Conta
    altkt         TYPE skb1-altkt , "Conta ANEEL
    dscanell(132) TYPE c          , "Descricao conta aneel
    ktoks         TYPE ska1-ktoks , "Grupo de Contas
    txt30         TYPE t077z-txt30, "Descricao Grupo
    xspeb2        TYPE skb1-xspeb , "Código: conta bloqueada para lançamento?
    xloev         TYPE ska1-xloev , "Código: conta está marcaca para eliminação?
    xspea         TYPE ska1-xspea , "Código: conta está bloqueada para criar ?
    xspeb         TYPE ska1-xspeb , "Código: conta bloqueada para lançamento?
    xspep         TYPE ska1-xspep , "Código: a conta está bloqueada para planejamento ?
  END OF ty_saida_pn,

  BEGIN OF ty_saida_sal,
    bukrs         TYPE zfit0024-bukrs,
    gjahr         TYPE zfit0024-gjahr,
    monat         TYPE zfit0024-monat,
    altkt         TYPE zfit0024-altkt,
    dscanell(132) TYPE c             , "Descricao conta aneel
    vlr_sldant    TYPE zfit0024-vlr_saldo,
    vlr_debito    TYPE zfit0024-vlr_debito,
    vlr_credito   TYPE zfit0024-vlr_credito,
    vlr_saldo     TYPE zfit0024-vlr_saldo,
  END OF ty_saida_sal.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA:
  it_bkpf           TYPE TABLE OF ty_bkpf,
  it_bseg           TYPE TABLE OF ty_bseg,
  it_acdoca         TYPE TABLE OF acdoca,
  it_bseg_hkont     TYPE TABLE OF ty_bseg_hkont,
  it_aufk           TYPE TABLE OF ty_aufk,
  it_cobrb_aux      TYPE TABLE OF ty_cobrb_aux,
  it_cobrb          TYPE TABLE OF ty_cobrb,
  it_anla           TYPE TABLE OF ty_anla,
  it_anka           TYPE TABLE OF ty_anka,
  it_t095           TYPE TABLE OF ty_t095,
  it_skb1_cont      TYPE TABLE OF ty_skb1,
  it_skb1_alt       TYPE TABLE OF ty_skb1,
  it_skb1           TYPE TABLE OF ty_skb1,
  it_skb1_s         TYPE TABLE OF ty_skb1,
  it_zfit0023       TYPE TABLE OF zfit0023,
  it_skb1_aux       TYPE TABLE OF ty_skb1_aux,
  it_skat           TYPE TABLE OF ty_skat,
  it_ska1           TYPE TABLE OF ty_ska1,
  it_csks           TYPE TABLE OF ty_csks,
  it_t077z          TYPE TABLE OF ty_t077z,
  it_t001           TYPE TABLE OF ty_t001,
  it_saida          TYPE TABLE OF ty_saida,
  it_saida_aux      TYPE TABLE OF ty_saida,
  it_saida2         TYPE TABLE OF ty_saida,
  it_saida_ini      TYPE TABLE OF ty_saida,
  it_saida_ant      TYPE TABLE OF zfit0024,
  it_saida_arq      TYPE TABLE OF ty_saida_arq,
  it_saida_pn       TYPE TABLE OF ty_saida_pn,
  it_saida_sal      TYPE TABLE OF ty_saida_sal,
  it_zfit0024       TYPE TABLE OF zfit0024,
  tl_zfit0024       TYPE TABLE OF zfit0024,
  it_zfit0024_s     TYPE TABLE OF zfit0024,
  it_zfit0024_sa    TYPE TABLE OF zfit0024,
  it_setleaf        LIKE TABLE OF setleaf  INITIAL SIZE 0 WITH HEADER LINE,
  it_setlinet       LIKE TABLE OF setlinet INITIAL SIZE 0 WITH HEADER LINE,
  t_bdc             TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  it_saldo_contas   TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
  it_saldo_contas_s TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
  it_saldo_contas_2 TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
  it_saldo_contas_3 TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
  t_conta_saldo     TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
  t_messtab         TYPE TABLE OF bdcmsgcoll,
  t_arquivo         TYPE TABLE OF ty_arquivo,
  it_contas         TYPE zct_emp_contas.



DATA: v_bukrs       TYPE bkpf-bukrs,
      v_bukrs2      TYPE bkpf-bukrs,
      v1_monat      TYPE zfit0024-monat,
      v_saldo1      TYPE zfit0024-vlr_saldo,
      v_saldo2      TYPE zfit0024-vlr_saldo,
      rg_mig_source TYPE RANGE OF fins_acdoc_mig_source.
*&---------------------------------------------------------------------*
*& Ranges
*&---------------------------------------------------------------------*
RANGES: rg_monat FOR bkpf-monat.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_bkpf        TYPE ty_bkpf,
  wa_bseg        TYPE ty_bseg,
  wa_bseg_aux    TYPE ty_bseg,
  wa_bseg_hkont  TYPE ty_bseg_hkont,
  wa_acdoca      TYPE acdoca,
  wa_aufk        TYPE ty_aufk,
  wa_cobrb_aux   TYPE ty_cobrb_aux,
  wa_cobrb       TYPE ty_cobrb,
  wa_anla        TYPE ty_anla,
  wa_anka        TYPE ty_anka,
  wa_t095        TYPE ty_t095,
  wa_skb1_cont   TYPE  ty_skb1,
  wa_skb1_alt    TYPE  ty_skb1,
  wa_skb1        TYPE ty_skb1,
  wa_skb1_aux    TYPE ty_skb1_aux,
  wa_zfit0023    TYPE zfit0023,
  wa_zfit0024    TYPE zfit0024,
  wa_zfit0024_s  TYPE zfit0024,
  wa_zfit0024_sa TYPE zfit0024,
  wa_skat        TYPE ty_skat,
  wa_ska1        TYPE ty_ska1,
  wa_csks        TYPE ty_csks,
  wa_t077z       TYPE ty_t077z,
  wa_t001        TYPE ty_t001,
  wa_saida       TYPE ty_saida,
  wa_saida2      TYPE ty_saida,
  wa_saida_ant   TYPE zfit0024,
  wa_saida_arq   TYPE ty_saida_arq,
  wa_saida_pn    TYPE ty_saida_pn,
  wa_saida_sal   TYPE ty_saida_sal,
  wa_setleaf     TYPE setleaf,
  wa_setlinet    TYPE setlinet,
  w_arquivo      TYPE ty_arquivo,
  wa_contas      TYPE zlc_emp_contas,
  wa_cont        TYPE REF TO cl_gui_custom_container , " Objeto Container
  wa_alv         TYPE REF TO cl_gui_alv_grid         , " Objeto ALV
  wa_layout      TYPE lvc_s_layo                     . " Layout da Lista / Fim do DATA


*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*

DATA: it_fcat   TYPE TABLE OF ty_estrutura, " lvc_s_fcat,
      s_variant TYPE disvariant           , " Tabela Estrutura colunas relatorio
      t_top     TYPE slis_t_listheader,
      xs_events TYPE slis_alv_event,
      events    TYPE slis_t_event,
      t_print   TYPE slis_print_alv,
      v_report  LIKE sy-repid,
      t_sort    TYPE slis_t_sortinfo_alv WITH HEADER LINE.



*&---------------------------------------------------------------------*
*& Contantes para arquivo XML
*&---------------------------------------------------------------------*
CONSTANTS:

  xml_bmp    TYPE c LENGTH 3  VALUE 'BMP',
  xml_age    TYPE c LENGTH 6  VALUE 'agente',
  xml_cd_emp TYPE c LENGTH 14 VALUE 'codigo_empresa',
  xml_ano    TYPE c LENGTH 3  VALUE 'ano',
  xml_mes    TYPE c LENGTH 3  VALUE 'mes',
  xml_contas TYPE c LENGTH 6  VALUE 'contas',
  xml_conta  TYPE c LENGTH 5  VALUE 'conta',
  xml_nro    TYPE c LENGTH 6  VALUE 'numero',
  xml_deb    TYPE c LENGTH 6  VALUE 'debito',
  xml_cred   TYPE c LENGTH 7  VALUE 'credito',
  xml_saldo  TYPE c LENGTH 5  VALUE 'saldo'.




*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                p_bukrs FOR bkpf-bukrs  NO INTERVALS NO-EXTENSION , " Empresa
                p_auart FOR bkpf-gjahr  NO INTERVALS NO-EXTENSION , " Ano
                p_monat FOR bkpf-monat  NO INTERVALS NO-EXTENSION . " Mes

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    lcont  TYPE char1  RADIOBUTTON GROUP rb02 USER-COMMAND aaaa,
    paneel TYPE char1  RADIOBUTTON GROUP rb02,
    consal TYPE char1  RADIOBUTTON GROUP rb02,
    gerarq TYPE char1  RADIOBUTTON GROUP rb02 DEFAULT 'X',
    arqxml TYPE char1  RADIOBUTTON GROUP rb02.
SELECTION-SCREEN: END OF BLOCK b2.


SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

  PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY DEFAULT 'C:\ANEEL.txt'.

SELECTION-SCREEN : END OF BLOCK b3.

IF lcont = 'X' OR consal = 'X' .
  IF p_bukrs-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Código da Empresa'.
    STOP.
  ENDIF.

  IF p_auart-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Ano'.
    STOP.
  ENDIF.

  IF p_monat-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Mês'.
    STOP.
  ENDIF.

ELSEIF paneel = 'X'.

  IF p_bukrs-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Código da Empresa'.
    STOP.
  ENDIF.

ELSEIF gerarq  = 'X'.

  IF p_bukrs-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Código da Empresa'.
    STOP.
  ENDIF.

  IF p_auart-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Ano'.
    STOP.
  ENDIF.

  IF p_monat-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Mês'.
    STOP.
  ENDIF.


ELSEIF arqxml  = 'X'.

  IF p_bukrs-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Código da Empresa'.
    STOP.
  ENDIF.

  IF p_auart-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Ano'.
    STOP.
  ENDIF.

  IF p_monat-low IS INITIAL.
    MESSAGE i000(z01) WITH 'Informe o Mês'.
    STOP.
  ENDIF.


ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_abre_arquivo.

AT SELECTION-SCREEN OUTPUT.
  IF arqxml = 'X'.
    SELECT *
     FROM setleaf
     INTO TABLE it_setleaf
     WHERE setname EQ 'MAGGI_EMPRESA_ANEEL'.

    SELECT *
    FROM setlinet
    INTO TABLE it_setlinet
    FOR ALL ENTRIES IN it_setleaf
    WHERE lineid  EQ it_setleaf-lineid
    AND setname   EQ it_setleaf-setname.

    READ TABLE it_setleaf INTO wa_setleaf    WITH KEY valfrom = p_bukrs-low.

    READ TABLE it_setlinet INTO wa_setlinet  WITH KEY lineid = wa_setleaf-lineid.

    IF wa_setlinet-descript IS NOT INITIAL.
      v_bukrs = wa_setlinet-descript(4).
    ELSE.
      v_bukrs = p_bukrs-low.
    ENDIF.
    SELECT SINGLE bukrs butxt
      FROM t001
      INTO wa_t001
      WHERE bukrs = p_bukrs-low.
    CONDENSE wa_t001-butxt NO-GAPS.
    CONCATENATE 'C:\APLBMP' v_bukrs '_' wa_t001-butxt+0(10) '_' p_auart-low p_monat-low '_A.XML' INTO p_file.
  ELSE.
    p_file = 'C:\ANEEL.txt'.
  ENDIF.
  IF lcont IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_FILE'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF .
    ENDLOOP.
  ELSEIF paneel IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_FILE'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF .
    ENDLOOP.

  ELSEIF consal IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_FILE'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF .
    ENDLOOP.
  ELSEIF gerarq IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_FILE'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF .
    ENDLOOP.

  ELSEIF arqxml IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_FILE'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF .
    ENDLOOP.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_ABRE_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_abre_arquivo.
  " Funcao para abri "Disco Local"
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      field_name    = 'C:\'
    CHANGING
      file_name     = p_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE e000(zb) WITH 'Nome do arquivo é muito longo.'.
    WHEN 2.
      MESSAGE e000(zb) WITH 'Ocorreu um erro.'.
  ENDCASE.
*  ENDIF.
ENDFORM.                    " F_ABRE_ARQUIVO

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF lcont = 'X'.
    PERFORM:  f_iniciar_variaves             , " Monta cabeçalho do Relatorio
              f_seleciona_dados              , " Form seleciona dados
              f_saida                        , " Form de saida
              f_imprime_dados                . " Form ALV ALV Lançamentos Contábeis

  ELSEIF paneel = 'X'.

    PERFORM:  f_iniciar_variaves             , " Monta cabeçalho do Relatorio
              f_seleciona_dados              , " Form seleciona dados
              f_saida                        , " Form de saida
              f_imprime_dados                . " Form ALV Plano de contas ANEEL


  ELSEIF consal = 'X'.

    PERFORM:  f_iniciar_variaves             , " Monta cabeçalho do Relatorio
              f_seleciona_dados              , " Form seleciona dados
              f_saida                        , " Form de saida
              f_imprime_dados                . " Form ALV Plano de contas ANEEL


  ELSEIF gerarq   = 'X' OR arqxml = 'X'.

    "Buscar dado no SET.
    SELECT *
      FROM setleaf
    INTO TABLE it_setleaf
    WHERE setname EQ 'MAGGI_EMPRESA_ANEEL'.

    SELECT *
     FROM setlinet
     INTO TABLE it_setlinet
      FOR ALL ENTRIES IN it_setleaf
    WHERE lineid  EQ it_setleaf-lineid
      AND setname EQ it_setleaf-setname.

    READ TABLE it_setleaf INTO wa_setleaf    WITH KEY valfrom = p_bukrs-low.

    READ TABLE it_setlinet INTO wa_setlinet  WITH KEY lineid = wa_setleaf-lineid.

    IF wa_setlinet-descript IS NOT INITIAL.
      v_bukrs = wa_setlinet-descript(4).
    ELSE.
      v_bukrs = p_bukrs-low.
    ENDIF.

    CLEAR: wa_setleaf, wa_setlinet.

    SELECT *
    FROM zfit0024
    INTO TABLE tl_zfit0024
    WHERE bukrs EQ v_bukrs
    AND   gjahr EQ p_auart-low
    AND   monat EQ p_monat-low.

    DATA: vl_answer   TYPE char1,
          v1_msg(200) TYPE c.

    IF NOT tl_zfit0024[] IS INITIAL.

      CONCATENATE 'O Mês  ' p_monat-low '/' p_auart-low 'da Empresa   ' p_bukrs-low  ',já foi processado deseja gerar novamente ?'
           INTO v1_msg SEPARATED BY space.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question  = v1_msg
        IMPORTING
          answer         = vl_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.
      IF vl_answer EQ '1'.
        CLEAR: v1_msg,
               vl_answer,
               v1_monat.


*        SELECT SINGLE monat
*          FROM  zfit0024
*          INTO  v1_monat
*          WHERE bukrs EQ v_bukrs
*          AND   gjahr EQ p_auart-low
*          AND   monat LT p_monat-low.


*        IF v1_monat IS NOT INITIAL.

        v1_msg = 'Os meses anteriores não precisam ser gerados novamente ?'.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question  = v1_msg
          IMPORTING
            answer         = vl_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        IF vl_answer EQ '1'.
          EXIT.
        ELSEIF  vl_answer EQ '2'.
          DELETE zfit0024 FROM TABLE tl_zfit0024.
          PERFORM:  f_seleciona_dados              , " Form seleciona dados
                    f_saida                        . " Form de saida gera TXT
        ENDIF.
*        ELSE.
*          PERFORM:  f_seleciona_dados              , " Form seleciona dados
*                    f_saida                        . " Form de saida gera TXT
*        ENDIF.
      ELSEIF vl_answer EQ '2'. .
        EXIT.
      ENDIF.
    ELSE.
      PERFORM:  f_seleciona_dados              , " Form seleciona dados
                f_saida                        . " Form de saida gera TXT
    ENDIF.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Form para selecionar os dados e relacionar as tabelas.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados.
  DATA vgjahr TYPE bkpf-gjahr.
  IF lcont = 'X' OR gerarq  = 'X' OR arqxml = 'X'.

    IF p_monat-low = 12.

      SELECT bukrs
             belnr
             gjahr
             monat
             budat
             bldat
             blart
        FROM bkpf
        INTO TABLE it_bkpf
       WHERE bukrs IN p_bukrs
         AND gjahr IN p_auart
         AND monat BETWEEN 12 AND 16
         AND bstat NE 'D'
      AND blart NE 'XS'.

    ELSE.
      SELECT bukrs
             belnr
             gjahr
             monat
             budat
             bldat
             blart
        FROM bkpf
        INTO TABLE it_bkpf
       WHERE bukrs IN p_bukrs
         AND gjahr IN p_auart
         AND monat IN p_monat
         AND bstat NE 'D'
         AND blart NE 'XS'.
    ENDIF.

    IF it_bkpf IS NOT INITIAL.

*      DATA etl750c6r9897 TYPE TABLE OF bseg.
*      DATA lt_fields_l750c6r2645 TYPE fagl_t_field.
*      lt_fields_l750c6r2645 = VALUE #( ( line = 'BUKRS' )
*       ( line = 'BELNR' )
*       ( line = 'GJAHR' )
*       ( line = 'SHKZG' )
*       ( line = 'DMBTR' )
*       ( line = 'HKONT' )
*       ( line = 'SGTXT' )
*       ( line = 'GSBER' )
*       ( line = 'BSCHL' )
*       ( line = 'ALTKT' )
*       ( line = 'KOSTL' )
*       ( line = 'BUZEI' )
*       ( line = 'UMSKZ' )
*       ( line = 'AUFNR' )
*       ).
*
*      CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
*        EXPORTING
*          it_for_all_entries = it_bkpf
*          i_where_clause     = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR|
*          it_fieldlist       = lt_fields_l750c6r2645
*        IMPORTING
*          et_bseg            = etl750c6r9897
*        EXCEPTIONS
*          not_found          = 1.
*      IF sy-subrc = 0 AND lines( etl750c6r9897 ) > 0.
*        MOVE-CORRESPONDING etl750c6r9897 TO it_bseg.
*        sy-dbcnt = lines( etl750c6r9897 ).
*      ELSE.
*        sy-subrc = 4.
*        sy-dbcnt = 0.
*      ENDIF.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'G' ) TO rg_mig_source.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = space ) TO rg_mig_source.

      "Ajuste alteração seleção de dados BSEG para acdoca / USER STORY 128053 / AOENNING.
      SELECT * FROM acdoca INTO TABLE it_acdoca
      FOR ALL ENTRIES IN it_bkpf
        WHERE rldnr    EQ '0L'
        AND  rbukrs    EQ it_bkpf-bukrs
        AND  gjahr     EQ it_bkpf-gjahr
        AND   belnr    EQ it_bkpf-belnr
        AND mig_source IN rg_mig_source.



*      IF it_bseg IS NOT INITIAL.
      IF it_acdoca IS NOT INITIAL.

        RANGES: rg_hkont FOR bseg-hkont.
        REFRESH: rg_hkont.

        SELECT *
          FROM setleaf
          INTO TABLE it_setleaf
          WHERE setname EQ 'MAGI_ZFIS23_CTAS_DEL'.

        LOOP AT it_setleaf INTO wa_setleaf.
          rg_hkont-sign   = 'I'.
          rg_hkont-option = 'EQ'.
          rg_hkont-low    = wa_setleaf-valfrom(10).
          APPEND rg_hkont.
          CLEAR: rg_hkont.
        ENDLOOP.

*        DELETE it_bseg WHERE umskz EQ 'F'.
        DELETE it_acdoca WHERE umskz EQ 'F'.


        "DELETE it_bseg WHERE hkont IN rg_hkont.

        IF rg_hkont[] IS NOT INITIAL.

          "Ajuste alteração seleção de dados BSEG para acdoca / USER STORY 128053 / AOENNING.
*          LOOP AT it_bseg INTO wa_bseg_aux.
*            IF wa_bseg_aux-hkont IN rg_hkont.
*              wa_bseg_hkont-belnr = wa_bseg_aux-belnr.
*              APPEND wa_bseg_hkont TO it_bseg_hkont.
*              CLEAR: wa_bseg_hkont.
*            ENDIF.
*          ENDLOOP.

          LOOP AT it_acdoca INTO DATA(wa_acdoca_aux).
            IF wa_acdoca_aux-racct IN rg_hkont.
              wa_bseg_hkont-belnr = wa_acdoca_aux-belnr.
              APPEND wa_bseg_hkont TO it_bseg_hkont.
              CLEAR: wa_acdoca_aux.
            ENDIF.
          ENDLOOP.

*          LOOP AT it_bseg_hkont INTO wa_bseg_hkont.
*            DELETE it_bseg WHERE belnr EQ wa_bseg_hkont-belnr.
*            CLEAR: wa_bseg_hkont-belnr.
*          ENDLOOP.

          LOOP AT it_bseg_hkont INTO wa_bseg_hkont.
            DELETE it_acdoca WHERE belnr EQ wa_bseg_hkont-belnr.
            CLEAR: wa_bseg_hkont-belnr.
          ENDLOOP.


          CLEAR: wa_setleaf.
          "Ajuste alteração seleção de dados BSEG para acdoca / USER STORY 128053 / AOENNING.
        ENDIF.


        "Saldo das contas mês de janeiro/2015 (mudança plano contas Aneel)
        REFRESH: it_contas,it_saldo_contas,it_saldo_contas_2,it_saldo_contas_3.
        IF p_monat-low = 1 AND p_auart-low = 2015.
*          LOOP AT it_bseg INTO wa_bseg.
*            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*              EXPORTING
*                input  = wa_bseg-hkont
*              IMPORTING
*                output = wa_bseg-hkont.
*            IF '1_2' CS wa_bseg-hkont+0(1).
*              wa_contas-bukrs = p_bukrs-low.
*              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                EXPORTING
*                  input  = wa_bseg-hkont
*                IMPORTING
*                  output = wa_bseg-hkont.
*              wa_contas-saknr = wa_bseg-hkont.
*              APPEND wa_contas TO it_contas.
*            ENDIF.
*          ENDLOOP.

          LOOP AT it_acdoca INTO DATA(wa_acdoca).
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = wa_acdoca-racct
              IMPORTING
                output = wa_acdoca-racct.
            IF '1_2' CS wa_acdoca-racct+0(1).
              wa_contas-bukrs = p_bukrs-low.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_acdoca-racct
                IMPORTING
                  output = wa_acdoca-racct.
              wa_contas-saknr = wa_acdoca-racct.
              APPEND wa_contas TO it_contas.
            ENDIF.
          ENDLOOP.

          SORT it_contas BY saknr.
          DELETE ADJACENT DUPLICATES FROM it_contas COMPARING ALL FIELDS.

          CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
            EXPORTING
              ryear         = p_auart-low
              waers         = 'BRL'
              contas        = it_contas
              p_gerar_todas = abap_true
            TABLES
              it_saldos     = it_saldo_contas
              it_saldos_2   = it_saldo_contas_2
              it_saldos_3   = it_saldo_contas_3
            EXCEPTIONS
              moeda_nao_adm = 1
              erro_ledger   = 2
              OTHERS        = 3.

          LOOP AT it_saldo_contas.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = it_saldo_contas-racct
              IMPORTING
                output = it_saldo_contas-racct.
            MODIFY it_saldo_contas INDEX sy-tabix TRANSPORTING racct.
          ENDLOOP.
          " Contas por empresa qe tem saldo em dezembro e sem movimentação em janeiro
          CALL FUNCTION 'G_SET_GET_ALL_VALUES'
            EXPORTING
              class         = '0000'
              setnr         = 'MAGGI_ZFIS23_ANEEL'
            TABLES
              set_values    = t_conta_saldo
            EXCEPTIONS
              set_not_found = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
          ENDIF.
          SORT t_conta_saldo BY from.
          REFRESH it_contas.
          LOOP AT t_conta_saldo.
            IF t_conta_saldo-from+0(4) = p_bukrs-low.
              wa_contas-bukrs = p_bukrs-low.
              wa_contas-saknr = t_conta_saldo-from+4(10).
              APPEND wa_contas TO it_contas.
            ENDIF.
          ENDLOOP.
          "
          CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
            EXPORTING
              ryear         = p_auart-low
              waers         = 'BRL'
              contas        = it_contas
              p_gerar_todas = abap_true
            TABLES
              it_saldos     = it_saldo_contas_s
              it_saldos_2   = it_saldo_contas_2
              it_saldos_3   = it_saldo_contas_3
            EXCEPTIONS
              moeda_nao_adm = 1
              erro_ledger   = 2
              OTHERS        = 3.

          " Conta Alternativa
          SELECT bukrs                 "#EC CI_DB_OPERATION_OK[2431747]
                 saknr
                 altkt
            FROM skb1
            INTO TABLE it_skb1_s
             FOR ALL ENTRIES IN it_contas
           WHERE bukrs EQ it_contas-bukrs
            AND  saknr EQ it_contas-saknr.

        ENDIF.

        " Dados mestre da ordem
        SELECT aufnr
               auart
               objnr
               bukrs
          FROM aufk
          INTO TABLE it_aufk
           FOR ALL ENTRIES IN it_acdoca
         WHERE aufnr EQ it_acdoca-aufnr
          AND  auart IN  ('ZOAN','ZMAN').


        IF it_aufk IS NOT INITIAL.

          " Regras de repartição de normas de apropriação (apropr.ordem)
          SELECT objnr
                 gbisj
                 gbisp
                 anln1
                 anln2
                 hkont
            FROM cobrb
            INTO TABLE it_cobrb_aux
             FOR ALL ENTRIES IN it_aufk
           WHERE objnr EQ it_aufk-objnr
            AND  gbisj EQ space
            AND  gbisp EQ space .


          LOOP AT it_cobrb_aux INTO wa_cobrb_aux .
            READ TABLE it_aufk INTO wa_aufk  WITH KEY objnr =  wa_cobrb_aux-objnr.
            wa_cobrb-objnr = wa_cobrb_aux-objnr.
            wa_cobrb-gbisj = wa_cobrb_aux-gbisj.
            wa_cobrb-gbisp = wa_cobrb_aux-gbisp.
            wa_cobrb-anln1 = wa_cobrb_aux-anln1.
            wa_cobrb-anln2 = wa_cobrb_aux-anln2.
            wa_cobrb-hkont = wa_cobrb_aux-hkont.
            wa_cobrb-bukrs = wa_aufk-bukrs.

            APPEND wa_cobrb TO it_cobrb.

          ENDLOOP.
          CLEAR: wa_cobrb_aux,
                 wa_cobrb,
                 wa_aufk.

        ENDIF.

        IF it_cobrb IS NOT INITIAL.


          SELECT bukrs                 "#EC CI_DB_OPERATION_OK[2431747]
                 saknr
                 altkt
            FROM skb1
            INTO TABLE it_skb1_alt
             FOR ALL ENTRIES IN it_cobrb
           WHERE bukrs EQ it_cobrb-bukrs
             AND saknr EQ it_cobrb-hkont.


          SELECT bukrs
                 anln1
                 anln2
                 anlkl
            FROM anla
            INTO TABLE it_anla
             FOR ALL ENTRIES IN it_cobrb
            WHERE bukrs EQ it_cobrb-bukrs
            AND  anln1  EQ it_cobrb-anln1
            AND  anln2  EQ it_cobrb-anln2 .
        ENDIF.

        IF it_anla IS NOT INITIAL.
          SELECT anlkl
                 ktogr
            FROM anka
            INTO TABLE it_anka
             FOR ALL ENTRIES IN it_anla
            WHERE anlkl EQ it_anla-anlkl.
        ENDIF.

        IF it_anka IS NOT INITIAL.
          SELECT ktopl
                 ktogr
                 afabe
                 ktansw
            FROM t095
            INTO TABLE it_t095
             FOR ALL ENTRIES IN it_anka
            WHERE ktopl EQ '0050'
            AND   ktogr EQ it_anka-ktogr
            AND   afabe EQ 01 .

        ENDIF.

        IF it_t095 IS NOT INITIAL.
          SELECT bukrs                 "#EC CI_DB_OPERATION_OK[2431747]
                 saknr
                 altkt
            FROM skb1
            INTO TABLE it_skb1_cont
             FOR ALL ENTRIES IN it_t095
           WHERE bukrs EQ p_bukrs-low
             AND saknr EQ it_t095-ktansw.

        ENDIF.


        " Conta Alternativa
*        SELECT *
*          FROM zfit0023
*          INTO TABLE it_zfit0023
*           FOR ALL ENTRIES IN it_bseg
*         WHERE bukrs EQ it_bseg-bukrs
*          AND  hkont EQ it_bseg-hkont
*          AND  kostl EQ it_bseg-kostl .

        SELECT *
         FROM zfit0023
         INTO TABLE it_zfit0023
          FOR ALL ENTRIES IN it_acdoca
        WHERE bukrs EQ it_acdoca-rbukrs
         AND  hkont EQ it_acdoca-racct
         AND  kostl EQ it_acdoca-rcntr.

        " Conta Alternativa
*        SELECT bukrs                   "#EC CI_DB_OPERATION_OK[2431747]
*               saknr
*               altkt
*          FROM skb1
*          INTO TABLE it_skb1
*           FOR ALL ENTRIES IN it_bseg
*         WHERE bukrs EQ it_bseg-bukrs
*          AND  saknr EQ it_bseg-hkont.

        SELECT bukrs                   "#EC CI_DB_OPERATION_OK[2431747]
        saknr
        altkt
        FROM skb1
        INTO TABLE it_skb1
         FOR ALL ENTRIES IN it_acdoca
       WHERE bukrs EQ it_acdoca-rbukrs
        AND  saknr EQ it_acdoca-racct.

        " Texto Conta
*        SELECT spras
*               ktopl
*               saknr
*               txt50
*          FROM skat
*        INTO TABLE it_skat
*          FOR ALL ENTRIES IN it_bseg
*        WHERE spras EQ 'PT'
*          AND  ktopl EQ '0050'
*          AND  saknr EQ it_bseg-hkont.

        SELECT spras
               ktopl
               saknr
               txt50
          FROM skat
        INTO TABLE it_skat
          FOR ALL ENTRIES IN it_acdoca
        WHERE spras EQ 'PT'
          AND  ktopl EQ '0050'
          AND  saknr EQ it_acdoca-racct.

        " Grupo de Contas
*        SELECT ktopl                   "#EC CI_DB_OPERATION_OK[2431747]
*               saknr                   "#EC CI_DB_OPERATION_OK[2389136]
*               ktoks
*          FROM ska1
*          INTO TABLE it_ska1
*           FOR ALL ENTRIES IN it_bseg
*         WHERE ktopl EQ '0050'
*          AND  saknr EQ it_bseg-hkont.

        SELECT ktopl                   "#EC CI_DB_OPERATION_OK[2431747]
                saknr                  "#EC CI_DB_OPERATION_OK[2389136]
                ktoks
           FROM ska1
           INTO TABLE it_ska1
            FOR ALL ENTRIES IN it_acdoca
          WHERE ktopl EQ '0050'
           AND  saknr EQ it_acdoca-racct.

      ENDIF.


    ENDIF.

  ELSEIF paneel = 'X'.

    SELECT bukrs                       "#EC CI_DB_OPERATION_OK[2431747]
           saknr
           altkt
           xspeb
      FROM skb1
      INTO TABLE it_skb1
     WHERE bukrs IN p_bukrs.

    IF it_skb1 IS NOT INITIAL.

      SELECT spras
             ktopl
             saknr
             txt50
        FROM skat
      INTO TABLE it_skat
       FOR ALL ENTRIES IN it_skb1
      WHERE saknr EQ it_skb1-saknr
      AND   spras EQ 'PT'
      AND   ktopl EQ '0050'.

      SELECT ktopl                     "#EC CI_DB_OPERATION_OK[2431747]
             saknr                     "#EC CI_DB_OPERATION_OK[2389136]
             ktoks
             xloev
             xspea
             xspeb
             xspep
        FROM ska1
        INTO TABLE it_ska1
         FOR ALL ENTRIES IN it_skb1
       WHERE saknr EQ it_skb1-saknr.

    ENDIF.

    IF it_ska1 IS NOT INITIAL.
      SELECT ktopl
             ktoks
             txt30
        FROM t077z
        INTO TABLE it_t077z
         FOR ALL ENTRIES IN it_ska1
       WHERE ktopl EQ '0050'
        AND  spras EQ 'PT'
        AND  ktoks EQ it_ska1-ktoks.

    ENDIF.


  ELSEIF consal = 'X'.

    DATA: v_gjahr TYPE zfit0024-gjahr,
          v_monat TYPE zfit0024-monat,
          v_bukrs TYPE bkpf-bukrs.

    "Buscar dado no SET.
    SELECT *
      FROM setleaf
    INTO TABLE it_setleaf
    WHERE setname EQ 'MAGGI_EMPRESA_ANEEL'.

    SELECT *
     FROM setlinet
     INTO TABLE it_setlinet
      FOR ALL ENTRIES IN it_setleaf
    WHERE lineid  EQ it_setleaf-lineid
      AND setname EQ it_setleaf-setname.

    READ TABLE it_setleaf INTO wa_setleaf    WITH KEY valfrom = p_bukrs-low.

    READ TABLE it_setlinet INTO wa_setlinet  WITH KEY lineid = wa_setleaf-lineid.

    IF wa_setlinet-descript IS NOT INITIAL.
      v_bukrs = wa_setlinet-descript(4).
    ELSE.
      v_bukrs = p_bukrs-low.
    ENDIF.

    CLEAR: wa_setleaf, wa_setlinet.

    SELECT *
      FROM zfit0024
      INTO TABLE it_zfit0024_s
     WHERE bukrs EQ v_bukrs
       AND gjahr IN p_auart
       AND monat IN p_monat.

    IF  p_monat-low = 1.
      v_gjahr = p_auart-low - 1.
      v_monat = 12.
    ELSE.
      v_gjahr = p_auart-low.
      v_monat = p_monat-low - 1.
    ENDIF.

    "ALRS1
    v_bukrs2 = v_bukrs.
    IF v_monat = '11' AND v_gjahr = '2014'. "neste mês houve troca dos codigos da empresa
      IF v_bukrs = '2980'.
        v_bukrs2 = '518'.
      ELSEIF v_bukrs = '7719'.
        v_bukrs2 = '564'.
      ELSEIF v_bukrs = '7720'.
        v_bukrs2 = '565'.
      ELSEIF v_bukrs = '7721'.
        v_bukrs2 = '566'.
      ELSEIF v_bukrs = '7729'.
        v_bukrs2 = '563'.
      ENDIF.
    ENDIF.

    SELECT *
      FROM zfit0024
      INTO TABLE it_zfit0024_sa
      WHERE bukrs EQ v_bukrs2
        AND gjahr EQ v_gjahr
        AND monat EQ v_monat.

    IF  p_monat-low = 1.
      CLEAR: v_saldo1,v_saldo2.
      SELECT SUM( vlr_saldo )
       FROM   zfit0024
       INTO   v_saldo1
       WHERE  bukrs EQ p_bukrs
       AND  gjahr EQ v_gjahr
       AND  monat EQ 12
       AND  altkt EQ '0000024061'.

      SELECT SUM( vlr_saldo )
       FROM   zfit0024
       INTO   v_saldo2
       WHERE  bukrs EQ p_bukrs
       AND  gjahr EQ v_gjahr
       AND  monat EQ 12
       AND  altkt BETWEEN '600000000' AND '799999999'.

      ADD v_saldo1 TO v_saldo2.

      READ TABLE it_zfit0024_sa INTO wa_zfit0024_sa WITH KEY altkt  = '0000024061'.
      IF sy-subrc = 0.
        wa_zfit0024_sa-vlr_saldo = v_saldo2.
        MODIFY it_zfit0024_sa FROM wa_zfit0024_sa INDEX sy-tabix TRANSPORTING vlr_saldo.
      ENDIF.
    ENDIF.

*    SELECT kostl
*      FROM csks
*      INTO TABLE it_csks
*       FOR ALL ENTRIES IN it_bseg
*     WHERE kokrs EQ 'MAGI'
*      AND  kostl EQ it_bseg-kostl
*      AND  kosar EQ 'F'.

    SELECT kostl
    FROM csks
    INTO TABLE it_csks
     FOR ALL ENTRIES IN it_acdoca
   WHERE kokrs EQ 'MAGI'
    AND  kostl EQ it_acdoca-rcntr
    AND  kosar EQ 'F'.

  ENDIF.
ENDFORM.                    "f_seleciona_dados

*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .

  DATA: empcont(18) TYPE c,
        thead       TYPE thead,
        tlinetab    TYPE TABLE OF tline,
        wa_tlinetab TYPE  tline,
        tabix       TYPE sy-tabix,
        valtkt      TYPE  skb1-altkt.

  CLEAR:  empcont,
          thead,
          tlinetab ,
          wa_tlinetab.


  "Relatório ALV – Lançamentos Contábeis - Dados para o TXT
  IF lcont = 'X' OR  gerarq = 'X' OR arqxml = 'X'.


    SORT: it_acdoca BY belnr,
          it_bkpf BY belnr.


    LOOP AT it_acdoca INTO wa_acdoca.


      wa_saida-gsber  = wa_acdoca-rbusa. " Divisão
      wa_saida-hkont  = wa_acdoca-racct. " Conta
      wa_saida-aufnr  = wa_acdoca-aufnr. " Nº Ordem



      wa_saida-bschl  = wa_acdoca-bschl. " Ch.lcto
      wa_saida-sgtxt  = wa_acdoca-sgtxt. " Texto Item
      wa_saida-kostl  = wa_acdoca-rcntr. " Centro de Custo

      SHIFT wa_saida-hkont LEFT DELETING LEADING '0'.

      READ TABLE it_bkpf INTO wa_bkpf WITH KEY belnr = wa_acdoca-belnr.

      wa_saida-bukrs = wa_bkpf-bukrs. " Empresa
      wa_saida-belnr = wa_bkpf-belnr. " Documento
      wa_saida-budat = wa_bkpf-budat. " Dt.Lcto
      wa_saida-bldat = wa_bkpf-bldat. " Dt.Docto
      wa_saida-blart = wa_bkpf-blart. " Tp.Docto

      READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_acdoca-racct.

      wa_saida-txt50  = wa_skat-txt50. " Descricao Conta


      READ TABLE it_ska1 INTO wa_ska1 WITH KEY saknr  = wa_acdoca-racct.

      " Regra nova
      IF wa_acdoca-aufnr IS NOT INITIAL AND wa_ska1-ktoks = 'YB06' .

        READ TABLE it_aufk      INTO wa_aufk      WITH KEY aufnr = wa_acdoca-aufnr .

        IF wa_aufk-auart = 'ZOAN'.

          READ TABLE it_cobrb     INTO wa_cobrb     WITH KEY objnr = wa_aufk-objnr.
          READ TABLE it_anla      INTO wa_anla      WITH KEY bukrs = wa_cobrb-bukrs anln1 = wa_cobrb-anln1  anln2 = wa_cobrb-anln2.
          READ TABLE it_anka      INTO wa_anka      WITH KEY anlkl = wa_anla-anlkl.
          READ TABLE it_t095      INTO wa_t095      WITH KEY ktogr = wa_anka-ktogr.
          READ TABLE it_skb1_cont INTO wa_skb1_cont WITH KEY saknr = wa_t095-ktansw .

          wa_saida-altkt =  wa_skb1_cont-altkt.   " Conta ANEEL II

          CONCATENATE  wa_t095-ktansw wa_acdoca-rbukrs INTO empcont .

        ELSEIF ( wa_aufk-auart = 'ZMAN' ) OR ( wa_aufk-auart EQ 'ZENE' ) .

          READ TABLE it_cobrb     INTO wa_cobrb     WITH KEY objnr = wa_aufk-objnr.
          READ TABLE it_skb1_alt  INTO wa_skb1_alt  WITH KEY saknr = wa_cobrb-hkont .

          wa_saida-altkt =  wa_skb1_alt-altkt.   " Conta ANEEL II

          CONCATENATE  wa_cobrb-hkont wa_acdoca-rbukrs INTO empcont .

        ENDIF.

      ELSEIF wa_acdoca-rcntr IS NOT INITIAL.

        READ TABLE it_zfit0023 INTO wa_zfit0023 WITH KEY kostl = wa_acdoca-rcntr hkont = wa_acdoca-racct.
        wa_saida-altkt    = wa_zfit0023-altkt.   " Conta ANEEL
        CONCATENATE  wa_acdoca-racct wa_acdoca-rbukrs INTO empcont .
      ENDIF.

      IF wa_saida-altkt IS INITIAL. " wa_bseg-kostl IS NOT INITIAL
        READ TABLE it_skb1 INTO wa_skb1 WITH KEY saknr = wa_acdoca-racct.
        wa_saida-altkt    = wa_skb1-altkt.   " Conta ANEEL
        CONCATENATE  wa_acdoca-racct wa_acdoca-rbukrs INTO empcont .

      ENDIF.

      READ TABLE it_csks INTO wa_csks WITH KEY kostl  = wa_acdoca-rcntr.

      " Regra nova
      IF wa_acdoca-aufnr IS NOT INITIAL AND wa_csks-kosar = 'F' .
        CLEAR: empcont, wa_zfit0023.

        READ TABLE it_zfit0023 INTO wa_zfit0023 WITH KEY kostl = wa_acdoca-rcntr hkont = wa_acdoca-racct.
        wa_saida-altkt    = wa_zfit0023-altkt.   " Conta ANEEL

        CONCATENATE  wa_acdoca-racct wa_acdoca-rbukrs INTO empcont .

      ENDIF.



      thead-tdobject  = 'SKB1'.
      thead-tdname    = empcont.
      thead-tdid      = '0001'.
      thead-tdspras   = 'PT'.


      "Pegar o campo da estrutura EENO_DYNP-ZEILE
      CALL FUNCTION 'TEXT_READ'
        EXPORTING
          i_header   = thead
          i_readonly = 'X'
        IMPORTING
          e_header   = thead
        TABLES
          t_lines    = tlinetab
        EXCEPTIONS
          notedited  = 1
          notfound   = 2
          id         = 3
          object     = 4
          name       = 5
          language   = 6
          OTHERS     = 7.

      IF tlinetab IS NOT INITIAL.

        READ TABLE tlinetab INTO wa_tlinetab INDEX 1.

        wa_saida-dscanell =  wa_tlinetab-tdline. " Descrição Conta Aneel

      ENDIF.


      IF wa_acdoca-drcrk = 'S'.
        wa_saida-xdebito = wa_acdoca-hsl.    " Vlr.Debito
        wa_saida-xsaldo  =  wa_saida-xdebito .
        wa_saida-shkzg   =  'D'.
      ELSEIF wa_acdoca-drcrk = 'H'.
        wa_saida-xcredito = wa_acdoca-hsl.  " Vlr.Credito
        wa_saida-xcredito =  wa_saida-xcredito * -1.
        wa_saida-xsaldo  =  wa_saida-xcredito.
        wa_saida-shkzg    =  'C'.
      ENDIF.

      APPEND wa_saida TO it_saida.

      CLEAR: wa_saida,
             wa_bseg ,
             wa_bkpf ,
             wa_skat ,
             wa_zfit0023,
             wa_tlinetab,
             wa_aufk,
             wa_cobrb,
             wa_anla,
             wa_anka,
             wa_t095,
             wa_skb1_cont,
             thead,
             empcont,
             wa_ska1,
             wa_skb1_alt,
             wa_acdoca,
             wa_csks.


    ENDLOOP.

    " Gerar o Arquivo TXT.
    IF gerarq = 'X' OR arqxml = 'X'.


      CLEAR: wa_saida   ,
             wa_setleaf ,
             wa_setlinet,
             it_setleaf ,
             it_setlinet.




      DATA: xdebito(20)  TYPE c,
            xcredito(20) TYPE c,
            xsaldo(19)   TYPE c,
            xsaldo2(20)  TYPE c,
            debito       TYPE bseg-dmbtr,
            credito      TYPE bseg-dmbtr,
            saldo        TYPE bseg-dmbtr,
            int          TYPE i,
            altkt(10)    TYPE c,
            mesant       TYPE bkpf-monat,
            v1_vlr_saldo TYPE zfit0024-vlr_saldo,
            vg_tabix     TYPE sy-tabix,
            v1_altkt     TYPE skb1-altkt,
            anoant       TYPE bkpf-gjahr.

      "Exemplos de Formatos válidos:
      "10504199811101 000000000002937146330000000000029371463300000000000000000000
      "10504199811103 0000000878536907874900000008843049010229+0000000208084388859
      "0504199811105 0000008888001693906000000000000000000000-0000228744049851220

      MOVE it_saida[] TO it_saida2[].
      SORT: it_saida2   BY altkt,
            it_saida    BY altkt,
            it_zfit0024 BY altkt bukrs.
      DELETE ADJACENT DUPLICATES FROM it_saida2 COMPARING altkt.
      DELETE it_saida2 WHERE altkt EQ space.

      mesant = p_monat-low - 1.

      REFRESH it_saida_aux.
      IF p_monat-low = 1.
        MOVE it_saida[] TO it_saida_aux[].
        SORT it_saida_aux BY hkont altkt.
        DELETE ADJACENT DUPLICATES FROM it_saida_aux COMPARING hkont altkt.
        LOOP AT it_saida_aux INTO wa_saida.
          tabix = sy-tabix.
          READ TABLE it_saldo_contas WITH KEY racct = wa_saida-hkont.
          IF sy-subrc = 0.
            wa_saida-xsaldo = it_saldo_contas-slvt + it_saldo_contas-sl01.
          ELSE.
            wa_saida-xsaldo = 0.
          ENDIF.
          MODIFY it_saida_aux FROM wa_saida INDEX tabix TRANSPORTING xsaldo.
        ENDLOOP.
        SORT it_saida_aux BY altkt hkont .
      ENDIF.

      LOOP AT it_saida2 INTO wa_saida2.
        tabix = sy-tabix.
        wa_saida2-xsaldo = 0.
        LOOP AT it_saida_aux INTO wa_saida WHERE altkt = wa_saida2-altkt.
          ADD wa_saida-xsaldo TO wa_saida2-xsaldo.
        ENDLOOP.
        MODIFY it_saida2 FROM wa_saida2 INDEX tabix TRANSPORTING xsaldo.
      ENDLOOP.

      SELECT *
      FROM setleaf
      INTO TABLE it_setleaf
      WHERE setname EQ 'MAGGI_EMPRESA_ANEEL'.

      SELECT *
      FROM setlinet
      INTO TABLE it_setlinet
      FOR ALL ENTRIES IN it_setleaf
      WHERE lineid  EQ it_setleaf-lineid
      AND setname   EQ it_setleaf-setname.


      LOOP AT it_saida2 INTO wa_saida2.


        LOOP AT it_saida INTO wa_saida WHERE altkt = wa_saida2-altkt.
          xdebito  = 0.
          xcredito = 0.
          xsaldo   = 0.
          xsaldo2  = 0.
          READ TABLE it_setleaf INTO wa_setleaf    WITH KEY valfrom = wa_saida-bukrs.
          READ TABLE it_setlinet INTO wa_setlinet  WITH KEY lineid = wa_setleaf-lineid.
          IF wa_setlinet-descript IS NOT INITIAL.
            wa_saida_arq-bukrs = wa_setlinet-descript(4).
            wa_zfit0024-bukrs  = wa_setlinet-descript.
          ELSE.
            wa_saida_arq-bukrs = wa_saida-bukrs.
            wa_zfit0024-bukrs  = wa_saida-bukrs.
          ENDIF.

          CLEAR v1_altkt.


          wa_saida_arq-mes = p_monat-low.
          wa_saida_arq-ano = p_auart-low.

          wa_zfit0024-monat  = p_monat-low.
          wa_zfit0024-gjahr  = p_auart-low.

          wa_zfit0024-altkt  = wa_saida-altkt.

          v1_altkt =  wa_saida-altkt.


          SHIFT v1_altkt LEFT DELETING LEADING '0'.

          wa_saida_arq-altkt = v1_altkt.


          debito = debito   + wa_saida-xdebito.
          credito = credito + wa_saida-xcredito .

          " Dados para Log.
          wa_zfit0024-mandt = sy-mandt .
          "wa_zfit0024-saknr  = wa_saida-saknr.



        ENDLOOP.

        " Dados para Log.

*----------------------------------------/ Comentado AEONNING.
*        credito = credito * -1.
*----------------------------------------/ Comentado AEONNING.

        saldo =  debito - credito .

        wa_zfit0024-vlr_debito  = debito.
        wa_zfit0024-vlr_credito = credito.

        MOVE debito TO xdebito.

        IF arqxml = 'X'.
          TRANSLATE xdebito USING '.,'.
        ELSE.
          TRANSLATE xdebito USING '. '.
        ENDIF.

        CONDENSE xdebito NO-GAPS.


        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = xdebito
          IMPORTING
            output = xdebito.


        wa_saida_arq-xdebito = xdebito.


        MOVE credito TO xcredito.

        IF arqxml = 'X'.
          TRANSLATE xcredito USING '.,'.
        ELSE.
          TRANSLATE xcredito USING '. '.
        ENDIF.

        CONDENSE xcredito NO-GAPS.


        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = xcredito
          IMPORTING
            output = xcredito.

        wa_saida_arq-xcredito = xcredito.

        IF p_monat-low = 01 .

          CLEAR: anoant.

          anoant = p_auart-low - 1.

          valtkt = wa_saida-altkt.
          SHIFT valtkt LEFT DELETING LEADING '0'.
          "ALRS1
          IF '6_7' CS valtkt+0(1). "ALRS
            v1_vlr_saldo = 0.
          ELSE.
            IF wa_saida-altkt NE '0000024061'.
              SELECT SINGLE vlr_saldo
              FROM   zfit0024
              INTO   v1_vlr_saldo
              WHERE  bukrs EQ wa_saida_arq-bukrs
              AND  gjahr EQ anoant
              AND  monat EQ 12
              AND  altkt EQ wa_saida-altkt.
            ELSE.
              CLEAR: v_saldo1,v_saldo2.
              SELECT SUM( vlr_saldo )
               FROM   zfit0024
               INTO   v_saldo1
               WHERE  bukrs EQ p_bukrs
               AND  gjahr EQ anoant
               AND  monat EQ 12
               AND  altkt EQ '0000024061'.

              SELECT SUM( vlr_saldo )
               FROM   zfit0024
               INTO   v_saldo2
               WHERE  bukrs EQ p_bukrs
               AND  gjahr EQ anoant
               AND  monat EQ 12
               AND  altkt BETWEEN '0600000000' AND '0799999999'.

              v1_vlr_saldo = v_saldo1 + v_saldo2.
            ENDIF.

          ENDIF.

          IF  v1_vlr_saldo IS NOT INITIAL.
            saldo = saldo + v1_vlr_saldo.
          ENDIF.

          IF p_auart-low = 2015 AND ( NOT  '6_7' CS valtkt+0(1) ). "ALRS.
            wa_zfit0024-vlr_saldo   = wa_saida2-xsaldo.
            saldo                   = wa_saida2-xsaldo.
          ELSE.
            wa_zfit0024-vlr_saldo   = saldo.
          ENDIF.

          IF saldo > 0.

            MOVE saldo TO xsaldo.
            IF arqxml = 'X'.
              TRANSLATE xsaldo USING '.,'.
            ELSE.
              TRANSLATE xsaldo USING '. '.
            ENDIF.

            CONDENSE xsaldo NO-GAPS.


            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = xsaldo
              IMPORTING
                output = xsaldo.


            CONCATENATE  '+' xsaldo INTO xsaldo2.

            wa_saida_arq-xsaldo = xsaldo2.

          ELSEIF saldo < 0.

            saldo = saldo * -1.

            MOVE saldo TO xsaldo.
            IF arqxml = 'X'.
              TRANSLATE xsaldo USING '.,'.
            ELSE.
              TRANSLATE xsaldo USING '. '.
            ENDIF.

            CONDENSE xsaldo NO-GAPS.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = xsaldo
              IMPORTING
                output = xsaldo.

            CONCATENATE  '-' xsaldo INTO xsaldo2 .

            wa_saida_arq-xsaldo = xsaldo2.

          ELSEIF saldo = 0.

            MOVE saldo TO xsaldo2.
            IF arqxml = 'X'.
              TRANSLATE xsaldo2 USING '.,'.
            ELSE.
              TRANSLATE xsaldo2 USING '. '.
            ENDIF.

            CONDENSE xsaldo2 NO-GAPS.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = xsaldo2
              IMPORTING
                output = xsaldo2.

            wa_saida_arq-xsaldo = xsaldo2.

          ENDIF.

        ELSE.
          "ALRS1
          v_bukrs2 = wa_saida_arq-bukrs.
          IF mesant = '11' AND p_auart-low = '2014'. "neste mês houve troca dos codigos da empresa
            IF wa_saida_arq-bukrs = '2980'.
              v_bukrs2 = '518'.
            ELSEIF wa_saida_arq-bukrs = '7719'.
              v_bukrs2 = '564'.
            ELSEIF wa_saida_arq-bukrs = '7720'.
              v_bukrs2 = '565'.
            ELSEIF wa_saida_arq-bukrs = '7721'.
              v_bukrs2 = '566'.
            ELSEIF wa_saida_arq-bukrs = '7729'.
              v_bukrs2 = '563'.
            ENDIF.
          ENDIF.
          SELECT SINGLE vlr_saldo
          FROM   zfit0024
          INTO   v1_vlr_saldo
          WHERE  bukrs EQ v_bukrs2
          AND  gjahr EQ p_auart-low
          AND  monat EQ mesant
          AND  altkt EQ wa_saida-altkt.


          IF  v1_vlr_saldo IS NOT INITIAL.
            saldo = saldo + v1_vlr_saldo.
          ENDIF.

          wa_zfit0024-vlr_saldo   = saldo.


          IF saldo > 0.

            MOVE saldo TO xsaldo.
            IF arqxml = 'X'.
              TRANSLATE xsaldo USING '.,'.
            ELSE.
              TRANSLATE xsaldo USING '. '.
            ENDIF.

            CONDENSE xsaldo NO-GAPS.


            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = xsaldo
              IMPORTING
                output = xsaldo.


            CONCATENATE  '+' xsaldo INTO xsaldo2.

            wa_saida_arq-xsaldo = xsaldo2.

          ELSEIF saldo < 0.

            saldo = saldo * -1.

            MOVE saldo TO xsaldo.
            IF arqxml = 'X'.
              TRANSLATE xsaldo USING '.,'.
            ELSE.
              TRANSLATE xsaldo USING '. '.
            ENDIF.

            CONDENSE xsaldo NO-GAPS.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = xsaldo
              IMPORTING
                output = xsaldo.

            CONCATENATE  '-' xsaldo INTO xsaldo2 .

            wa_saida_arq-xsaldo = xsaldo2.

          ELSEIF saldo = 0.

            MOVE saldo TO xsaldo2.

            IF arqxml = 'X'.
              TRANSLATE xsaldo2 USING '.,'.
            ELSE.
              TRANSLATE xsaldo2 USING '. '.
            ENDIF.

            CONDENSE xsaldo2 NO-GAPS.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = xsaldo2
              IMPORTING
                output = xsaldo2.

            wa_saida_arq-xsaldo = xsaldo2.

          ENDIF.
        ENDIF.

        APPEND wa_saida_arq TO it_saida_arq.
        APPEND wa_zfit0024  TO it_zfit0024.

        CLEAR:  wa_saida,
                wa_saida_arq,
                wa_zfit0024,
                debito,
                credito ,
                saldo,
                xsaldo,
                xsaldo2,
                int,
                v1_vlr_saldo,
                it_saida_ant,
                wa_setleaf,
                wa_setlinet .

      ENDLOOP.

      " Verifica se tem registros no mes anterior que não estão no mes atual e insere no TXT.
      IF it_zfit0024 IS NOT INITIAL.
        DATA: v_bukrs  TYPE bkpf-bukrs.
        CLEAR: v_bukrs,
               wa_setleaf,
               wa_setlinet.

        SELECT *
        FROM setleaf
        INTO TABLE it_setleaf
        WHERE setname EQ 'MAGGI_EMPRESA_ANEEL'.

        SELECT *
        FROM setlinet
        INTO TABLE it_setlinet
        FOR ALL ENTRIES IN it_setleaf
        WHERE lineid  EQ it_setleaf-lineid
        AND setname   EQ it_setleaf-setname.

        READ TABLE it_setleaf INTO wa_setleaf    WITH KEY valfrom = p_bukrs-low.

        READ TABLE it_setlinet INTO wa_setlinet  WITH KEY lineid = wa_setleaf-lineid.

        IF wa_setlinet-descript IS NOT INITIAL.
          v_bukrs = wa_setlinet-descript(4).
        ELSE.
          v_bukrs = p_bukrs-low.
        ENDIF.

        CLEAR: wa_setleaf, wa_setlinet.



        IF p_monat-low = 1.  " Para Janeiro ele busca ano anterior e mes de dezembro.

          SELECT  *
            FROM  zfit0024
            INTO TABLE it_saida_ant
           WHERE bukrs EQ v_bukrs
            AND  gjahr EQ anoant
            AND  monat EQ 12.

        ELSE.
          v_bukrs2 = v_bukrs.
          IF mesant = '11' AND p_auart-low = '2014'. "neste mês houve troca dos codigos da empresa
            IF v_bukrs = '2980'.
              v_bukrs2 = '518'.
            ELSEIF v_bukrs = '7719'.
              v_bukrs2 = '564'.
            ELSEIF v_bukrs = '7720'.
              v_bukrs2 = '565'.
            ELSEIF v_bukrs = '7721'.
              v_bukrs2 = '566'.
            ELSEIF v_bukrs = '7729'.
              v_bukrs2 = '563'.
            ENDIF.
          ENDIF.

          SELECT  *
            FROM  zfit0024
            INTO TABLE it_saida_ant
           WHERE bukrs EQ v_bukrs2
            AND  gjahr EQ p_auart-low
            AND  monat EQ mesant.

          IF mesant = '11' AND p_auart-low = '2014'. "neste mês houve troca dos codigos da empresa
            LOOP AT  it_saida_ant INTO wa_saida_ant.
              vg_tabix = sy-tabix.
              wa_saida_ant-bukrs = v_bukrs.
              MODIFY it_saida_ant INDEX vg_tabix FROM wa_saida_ant TRANSPORTING bukrs.
            ENDLOOP.
          ENDIF.
        ENDIF.

        CLEAR: v_bukrs,
               wa_setleaf,
               wa_setlinet.

        IF it_saida_ant IS NOT INITIAL.

          LOOP AT  it_saida_ant INTO wa_saida_ant.
            vg_tabix = sy-tabix.
            READ TABLE it_zfit0024 INTO  wa_zfit0024  WITH KEY altkt = wa_saida_ant-altkt bukrs = wa_saida_ant-bukrs BINARY SEARCH .
            IF sy-subrc IS INITIAL.
              CLEAR: wa_saida_ant-altkt.
              MODIFY it_saida_ant INDEX vg_tabix FROM wa_saida_ant TRANSPORTING altkt.
            ENDIF.
          ENDLOOP.

          CLEAR: wa_zfit0024,
                 wa_saida_ant.

          DELETE it_saida_ant WHERE altkt EQ space.

          IF p_monat-low EQ '01' AND p_auart-low EQ '2015'. "Exceção
            REFRESH: it_saida_ini, it_saida_ant.
            LOOP AT it_saldo_contas_s.
              wa_saida-bukrs       = it_saldo_contas_s-rbukrs.
              wa_saida-hkont       = it_saldo_contas_s-racct.
              READ TABLE it_skb1_s INTO wa_skb1 WITH KEY saknr = it_saldo_contas_s-racct .
              wa_saida-altkt  = wa_skb1-altkt.   " Conta ANEEL
              wa_saida-xsaldo = it_saldo_contas_s-slvt + it_saldo_contas_s-sl01.
              APPEND wa_saida TO it_saida_ini.
            ENDLOOP.

            it_saida2[] = it_saida_ini[].
            SORT it_saida2 BY altkt.
            DELETE ADJACENT DUPLICATES FROM it_saida2 COMPARING altkt.
            LOOP AT it_saida2 INTO wa_saida2.
              tabix = sy-tabix.
              wa_saida2-xsaldo = 0.
              LOOP AT it_saida_ini INTO wa_saida WHERE altkt = wa_saida2-altkt.
                ADD wa_saida-xsaldo TO wa_saida2-xsaldo.
              ENDLOOP.
              MODIFY it_saida2 FROM wa_saida2 INDEX tabix TRANSPORTING xsaldo.
            ENDLOOP.

            LOOP AT it_saida2 INTO wa_saida2.
              READ TABLE it_setleaf INTO wa_setleaf    WITH KEY valfrom = wa_saida2-bukrs.
              READ TABLE it_setlinet INTO wa_setlinet  WITH KEY lineid = wa_setleaf-lineid.
              IF wa_setlinet-descript IS NOT INITIAL.
                wa_saida_ant-bukrs = wa_setlinet-descript(4).
              ENDIF.
              wa_saida_ant-altkt     = wa_saida2-altkt.
              wa_saida_ant-vlr_saldo = wa_saida2-xsaldo.
              APPEND wa_saida_ant TO it_saida_ant.
            ENDLOOP.
          ENDIF.

          IF it_saida_ant IS NOT INITIAL.

            LOOP AT it_saida_ant INTO wa_saida_ant.

              tabix = sy-tabix.

              wa_saida_arq-bukrs        =  wa_saida_ant-bukrs.
              wa_saida_arq-mes          =  p_monat-low.
              wa_saida_arq-ano          =  p_auart-low.

              " Dado para Log
              wa_zfit0024-altkt       =  wa_saida_ant-altkt.

              SHIFT wa_saida_ant-altkt LEFT DELETING LEADING '0'.

              wa_saida_arq-altkt    = wa_saida_ant-altkt.
              wa_saida_arq-xdebito  = '0,00'.
              wa_saida_arq-xcredito = '0,00'.

              IF '6_7' CS wa_saida_ant-altkt+0(1)  AND p_monat-low = 1. "ALRS Zerar contas de resultado em janeiro
                wa_saida_ant-vlr_saldo = 0.
                MODIFY it_saida_ant FROM wa_saida_ant INDEX tabix TRANSPORTING vlr_saldo.
              ENDIF.

              CLEAR: xsaldo,
                     saldo.

              IF wa_saida_ant-vlr_saldo < 0.

                saldo = wa_saida_ant-vlr_saldo.

                saldo = saldo * -1.

                MOVE saldo TO xsaldo.

                IF arqxml = 'X'.
                  TRANSLATE xsaldo USING '.,'.
                ELSE.
                  TRANSLATE xsaldo USING '. '.
                ENDIF.

                CONDENSE xsaldo NO-GAPS.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = xsaldo
                  IMPORTING
                    output = xsaldo.

                CONCATENATE  '-' xsaldo INTO xsaldo2 .

                wa_saida_arq-xsaldo  =  xsaldo2.


              ELSEIF wa_saida_ant-vlr_saldo  > 0.

                saldo = wa_saida_ant-vlr_saldo.

                MOVE saldo TO xsaldo.
                IF arqxml = 'X'.
                  TRANSLATE xsaldo USING '.,'.
                ELSE.
                  TRANSLATE xsaldo USING '. '.
                ENDIF.

                CONDENSE xsaldo NO-GAPS.


                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = xsaldo
                  IMPORTING
                    output = xsaldo.


                CONCATENATE  '+' xsaldo INTO xsaldo2.

                wa_saida_arq-xsaldo  =  xsaldo2.

              ELSEIF wa_saida_ant-vlr_saldo = 0.

                saldo = wa_saida_ant-vlr_saldo.

                MOVE saldo TO xsaldo2.
                IF arqxml = 'X'.
                  TRANSLATE xsaldo2 USING '.,'.
                ELSE.
                  TRANSLATE xsaldo2 USING '. '.
                ENDIF.

                CONDENSE xsaldo2 NO-GAPS.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = xsaldo2
                  IMPORTING
                    output = xsaldo2.

                wa_saida_arq-xsaldo  =  xsaldo2.

              ENDIF.


              wa_zfit0024-mandt       =  sy-mandt .
              wa_zfit0024-bukrs       =  wa_saida_ant-bukrs.
              wa_zfit0024-gjahr       =  p_auart-low.
              wa_zfit0024-monat       =  p_monat-low.

              wa_zfit0024-vlr_debito  =  0. "WA_SAIDA_ARQ-XDEBITO.
              wa_zfit0024-vlr_credito =  0. "WA_SAIDA_ARQ-XCREDITO.

*------------------------------------------------------------------------------------/ USER STORY 128053 / AOENNING.
              IF wa_saida_arq-mes EQ 01 AND wa_saida_arq-altkt EQ '24061'.
                wa_zfit0024-vlr_saldo = 0.
                wa_saida_arq-xsaldo = '0,00'.
              ELSE.
                wa_zfit0024-vlr_saldo   =  wa_saida_ant-vlr_saldo.
              ENDIF.
*------------------------------------------------------------------------------------/ USER STORY 128053 / AOENNING.

              APPEND wa_saida_arq TO it_saida_arq.
              APPEND wa_zfit0024 TO it_zfit0024.

            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

      DATA: vl_cami  TYPE string.

      vl_cami  =  p_file.

      IF it_saida_arq IS NOT INITIAL.

        DELETE it_saida_arq WHERE altkt IS INITIAL.

        IF gerarq = 'X' .
          "GERA O TXT

          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              filename = vl_cami
            TABLES
              data_tab = it_saida_arq "t_file
            EXCEPTIONS
              OTHERS   = 4.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          PERFORM z_gravar_dados_zfit0024.
          MESSAGE i000(z01) WITH 'Arquivo e Log gerados com sucesso!!!'.

        ELSEIF arqxml = 'X'.
          "GERA O XML
          IF it_saida_arq IS NOT INITIAL.
            REFRESH t_arquivo.
            DELETE it_saida_arq WHERE altkt IS INITIAL.
            PERFORM ctnab USING xml_bmp.
            "Agente
            PERFORM ctnab USING xml_age.
            READ TABLE it_saida_arq  INTO wa_saida_arq INDEX 1.
            PERFORM ctnav USING xml_cd_emp wa_saida_arq-bukrs.
            PERFORM ctnav USING xml_ano    wa_saida_arq-ano.
            PERFORM ctnav USING xml_mes    wa_saida_arq-mes.
            PERFORM ctnfe USING xml_age.
            "Contas
            PERFORM ctnab USING xml_contas.
            LOOP AT it_saida_arq INTO wa_saida_arq.
              PERFORM ctnab USING xml_conta.
              PERFORM ctnav USING xml_nro wa_saida_arq-altkt.
              PERFORM ctnav USING xml_deb wa_saida_arq-xdebito.
              PERFORM ctnav USING xml_cred wa_saida_arq-xcredito.
              PERFORM ctnav USING xml_saldo wa_saida_arq-xsaldo.
              PERFORM ctnfe USING xml_conta.
            ENDLOOP.
            PERFORM ctnfe USING xml_contas.
            PERFORM ctnfe USING xml_bmp.
            "
            CALL FUNCTION 'GUI_DOWNLOAD'
              EXPORTING
                filename = vl_cami
              TABLES
                data_tab = t_arquivo
              EXCEPTIONS
                OTHERS   = 4.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
            PERFORM z_gravar_dados_zfit0024.
            MESSAGE i000(z01) WITH 'Arquivo e Log gerados com sucesso!!!'.
          ENDIF.

        ENDIF.


      ELSE.

        MESSAGE i000(z01) WITH 'Não existem dados para ser gerados.'.


      ENDIF.

    ENDIF.

    "Relatório ALV – Plano de Conta ANEEL
  ELSEIF paneel = 'X'.

    CLEAR: wa_bkpf,
           wa_bseg,
           wa_skat,
           wa_skb1_aux,
           wa_ska1,
           wa_t077z,
           wa_saida_pn.

    SORT: it_acdoca BY belnr,
          it_bkpf BY belnr.


    LOOP AT it_skb1 INTO wa_skb1.

      wa_saida_pn-saknr  = wa_skb1-saknr.
      wa_saida_pn-altkt  = wa_skb1-altkt.
      wa_saida_pn-xspeb2 = wa_skb1-xspeb.


      READ TABLE it_skat INTO wa_skat WITH KEY saknr  = wa_skb1-saknr.
      wa_saida_pn-txt50 = wa_skat-txt50.

      READ TABLE it_ska1 INTO wa_ska1 WITH KEY saknr  = wa_skb1-saknr.

      wa_saida_pn-ktoks = wa_ska1-ktoks.
      wa_saida_pn-xloev	=	wa_ska1-xloev.
      wa_saida_pn-xspea = wa_ska1-xspea.
      wa_saida_pn-xspeb = wa_ska1-xspeb.
      wa_saida_pn-xspep = wa_ska1-xspep.


      READ TABLE it_t077z INTO wa_t077z WITH KEY ktopl = wa_ska1-ktopl  ktoks =  wa_ska1-ktoks  .

      wa_saida_pn-txt30 = wa_t077z-txt30.


      CONCATENATE  wa_skb1-saknr p_bukrs-low INTO empcont .

      thead-tdobject  = 'SKB1'.
      thead-tdname    = empcont.
      thead-tdid      = '0001'.
      thead-tdspras   = 'PT'.


      "Pegar o campo da estrutura EENO_DYNP-ZEILE
      CALL FUNCTION 'TEXT_READ'
        EXPORTING
          i_header   = thead
          i_readonly = 'X'
        IMPORTING
          e_header   = thead
        TABLES
          t_lines    = tlinetab
        EXCEPTIONS
          notedited  = 1
          notfound   = 2
          id         = 3
          object     = 4
          name       = 5
          language   = 6
          OTHERS     = 7.

      IF tlinetab IS NOT INITIAL.

        READ TABLE tlinetab INTO wa_tlinetab INDEX 1.

        wa_saida_pn-dscanell =  wa_tlinetab-tdline. " Descrição Conta Aneel

      ENDIF.


      APPEND wa_saida_pn TO it_saida_pn.

      CLEAR: wa_saida_pn,
             wa_bseg ,
             wa_bkpf ,
             wa_skat ,
             wa_zfit0023,
             wa_tlinetab,
             wa_t077z.

    ENDLOOP.

    "Relatório ALV – Consulta de Saldos.
  ELSEIF consal = 'X'.

    LOOP AT it_zfit0024_s INTO wa_zfit0024_s.

      wa_saida_sal-bukrs       = wa_zfit0024_s-bukrs.
      wa_saida_sal-gjahr       = wa_zfit0024_s-gjahr.
      wa_saida_sal-monat       = wa_zfit0024_s-monat.
      wa_saida_sal-altkt       = wa_zfit0024_s-altkt.
      wa_saida_sal-vlr_debito  = wa_zfit0024_s-vlr_debito.
      IF wa_zfit0024_s-vlr_credito < 0.
        wa_saida_sal-vlr_credito = abs( wa_zfit0024_s-vlr_credito ).
      ELSE.
        wa_saida_sal-vlr_credito = wa_zfit0024_s-vlr_credito.
      ENDIF.
      wa_saida_sal-vlr_saldo   = wa_zfit0024_s-vlr_saldo.

      READ TABLE it_zfit0024_sa INTO wa_zfit0024_sa WITH KEY altkt  = wa_zfit0024_s-altkt.
      valtkt = wa_zfit0024_s-altkt.
      SHIFT valtkt LEFT DELETING LEADING '0'.


      IF p_monat-low = 1 AND '6_7' CS valtkt+0(1) . "ALRS zera contas de resultado
        wa_saida_sal-vlr_sldant = 0.
      ELSE.
        wa_saida_sal-vlr_sldant =  wa_zfit0024_sa-vlr_saldo.
      ENDIF.

*      " Descricao da Conta
*
*      read table it_skb1 into wa_skb1 WITH KEY altkt  = wa_zfit0024_s-altkt.
*
*      CONCATENATE  wa_skb1-saknr p_bukrs-low INTO empcont .
*
*      thead-tdobject  = 'SKB1'.
*      thead-tdname    = empcont.
*      thead-tdid      = '0001'.
*      thead-tdspras   = 'PT'.
*
*
*      "Pegar o campo da estrutura EENO_DYNP-ZEILE
*      CALL FUNCTION 'TEXT_READ'
*        EXPORTING
*          i_header   = thead
*          i_readonly = 'X'
*        IMPORTING
*          e_header   = thead
*        TABLES
*          t_lines    = tlinetab
*        EXCEPTIONS
*          notedited  = 1
*          notfound   = 2
*          id         = 3
*          object     = 4
*          name       = 5
*          language   = 6
*          OTHERS     = 7.
*
*      IF tlinetab IS NOT INITIAL.
*
*        READ TABLE tlinetab INTO wa_tlinetab INDEX 1.
*
*        wa_saida_sal-dscanell =  wa_tlinetab-tdline. " Descrição Conta Aneel
*
*      ENDIF.

      APPEND wa_saida_sal TO it_saida_sal.

      CLEAR: wa_zfit0024_s,
             wa_zfit0024_sa ,
             wa_saida_sal,
             wa_skb1.

    ENDLOOP.
  ENDIF.


ENDFORM.                    "f_saida


*&---------------------------------------------------------------------*
*&      Form  FORM_ALV
*&---------------------------------------------------------------------*
FORM f_alv .
  IF lcont = 'X'.
    PERFORM alv_preenche_cat USING:
          'BUKRS'         TEXT-004   '07'   ' '  'X',
          'GSBER'         TEXT-005   '07'   ' '  'X',
          'HKONT'         TEXT-006   '12'   'X'  ' ',       "FS00
          'TXT50'         TEXT-007   '50'   ' '  ' ',
          'ALTKT'         TEXT-008   '15'   ' '  'X',
          'DSCANELL'      TEXT-009   '50'   ' '  'X',
          'BELNR'         TEXT-010   '20'   'X'  'X',       " FB03
          'AUFNR'         TEXT-038   '13'   'X'  'X',
          'BUDAT'         TEXT-011   '12'   ' '  ' ',
          'BLDAT'         TEXT-012   '10'   ' '  ' ',
          'BLART'         TEXT-013   '08'   ' '  'X',
          'BSCHL'         TEXT-014   '07'   ' '  'X',
          'KOSTL'         TEXT-022   '15'   ' '  'X',
          'SGTXT'         TEXT-015   '50'   ' '  'X',
          'SHKZG'         TEXT-016   '03'   ' '  'X',
          'XDEBITO'       TEXT-017   '12'   ' '  ' ',
          'XCREDITO'      TEXT-018   '12'   ' '  ' ',
          'XSALDO'        TEXT-019   '12'   ' '  ' '.

  ELSEIF paneel = 'X'.
    PERFORM alv_preenche_cat USING:
          'SAKNR'         TEXT-028   '07'   'X'  'X',       " FS00
          'TXT50'         TEXT-007   '50'   ' '  ' ',
          'ALTKT'         TEXT-008   '15'   ' '  'X',
          'DSCANELL'      TEXT-009   '50'   ' '  'X',
          'KTOKS'         TEXT-020   '12'   ' '  ' ',
          'TXT30'         TEXT-021   '50'   ' '  'X',
          'XLOEV'         TEXT-023   '18'   ' '  'X',
          'XSPEA'         TEXT-024   '18'   ' '  'X',
          'XSPEB'         TEXT-025   '18'   ' '  ' ',
          'XSPEP'         TEXT-026   '18'   ' '  'X',
          'XSPEB2'        TEXT-027   '18'   ' '  'X'.

  ELSEIF consal = 'X'.
    PERFORM alv_preenche_cat USING:
        'BUKRS'           TEXT-029   '13'   ' '  'X',
        'GJAHR'           TEXT-030   '04'   ' '  'X',
        'MONAT'           TEXT-031   '16'   ' '  'X',
        'ALTKT'           TEXT-032   '15'   ' '  'X',
        "'DSCANELL'        text-037   '50'   ' '  'X',
        'VLR_SLDANT'      TEXT-033   '15'   ' '  ' ',
        'VLR_DEBITO'      TEXT-034   '15'   ' '  ' ',
        'VLR_CREDITO'     TEXT-035   '15'   ' '  ' ',
        'VLR_SALDO'       TEXT-036   '15'   ' '  ' '.

  ENDIF.
ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c.
  DATA: wl_fcat TYPE ty_estrutura.

  IF lcont = 'X'.
    wl_fcat-tabname   = 'IT_SAIDA'  .
    wl_fcat-fieldname = p_campo   .
    wl_fcat-seltext_s = p_desc    .
    wl_fcat-seltext_m = p_desc    .
    wl_fcat-seltext_l = p_desc    .
    wl_fcat-hotspot   = p_hot     .
    wl_fcat-no_zero   = p_zero    .
    wl_fcat-outputlen = p_tam     .

    APPEND wl_fcat TO it_fcat.

  ELSEIF paneel = 'X'.

    wl_fcat-tabname   = 'IT_SAIDA_PN'  .
    wl_fcat-fieldname = p_campo   .
    wl_fcat-seltext_s = p_desc    .
    wl_fcat-seltext_m = p_desc    .
    wl_fcat-seltext_l = p_desc    .
    wl_fcat-hotspot   = p_hot     .
    wl_fcat-no_zero   = p_zero    .
    wl_fcat-outputlen = p_tam     .

    APPEND wl_fcat TO it_fcat.

  ELSEIF consal = 'X'.

    wl_fcat-tabname   = 'IT_SAIDA_SAL'  .
    wl_fcat-fieldname = p_campo   .
    wl_fcat-seltext_s = p_desc    .
    wl_fcat-seltext_m = p_desc    .
    wl_fcat-seltext_l = p_desc    .
    wl_fcat-hotspot   = p_hot     .
    wl_fcat-no_zero   = p_zero    .
    wl_fcat-outputlen = p_tam     .

    APPEND wl_fcat TO it_fcat.
  ENDIF.




ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .

  IF lcont = 'X'.
    IF it_saida[] IS INITIAL.
      MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                             'informados' .
      STOP.
    ENDIF.

    PERFORM f_definir_eventos.
    PERFORM f_alv.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = v_report
        i_callback_user_command = 'F_USER_COMMAND'
        it_fieldcat             = it_fcat[]
        it_sort                 = t_sort[]
        i_save                  = 'X'
        it_events               = events
        is_print                = t_print
      TABLES
        t_outtab                = it_saida.
  ELSEIF paneel = 'X'.

    IF it_saida_pn[] IS INITIAL.
      MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                             'informados' .
      STOP.
    ENDIF.
    PERFORM f_definir_eventos.
    PERFORM f_alv.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = v_report
        i_callback_user_command = 'F_USER_COMMAND'
        it_fieldcat             = it_fcat[]
        it_sort                 = t_sort[]
        i_save                  = 'X'
        it_events               = events
        is_print                = t_print
      TABLES
        t_outtab                = it_saida_pn.


  ELSEIF consal = 'X'.

    IF it_saida_sal[] IS INITIAL.
      MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                             'informados' .
      STOP.
    ENDIF.
    PERFORM f_definir_eventos.
    PERFORM f_alv.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = v_report
        i_callback_user_command = 'F_USER_COMMAND'
        it_fieldcat             = it_fcat[]
        it_sort                 = t_sort[]
        i_save                  = 'X'
        it_events               = events
        is_print                = t_print
      TABLES
        t_outtab                = it_saida_sal.

  ENDIF.

ENDFORM.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos

*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves.

  DATA: w_texto1(40),
        w_texto2(40),
        w_texto3(40),
        v1_butxt TYPE t001-butxt.

  v_report = sy-repid.
  CLEAR:  v1_butxt.

  IF lcont = 'X'.

    w_texto1 = 'ZFIS23 - Lançamentos Contábeis  '.
    PERFORM f_construir_cabecalho USING 'H' w_texto1.

    CONCATENATE  'Periodo: ' p_monat-low '/' p_auart-low  INTO w_texto2 .
    PERFORM f_construir_cabecalho USING 'S' w_texto2.

    " Dados da Empresa
    SELECT SINGLE butxt
      FROM t001
      INTO  v1_butxt
     WHERE bukrs EQ p_bukrs-low.

    CONCATENATE  'Empresa: ' p_bukrs-low '  -  ' v1_butxt INTO w_texto3 .
    PERFORM f_construir_cabecalho USING 'S' w_texto3.

  ELSEIF paneel = 'X' OR consal = 'X'.

    w_texto1 = 'ZFIS23 – Consulta Saldos ANEEL   '.
    PERFORM f_construir_cabecalho USING 'H' w_texto1.

    " Dados da Empresa
    SELECT SINGLE butxt
      FROM t001
      INTO  v1_butxt
     WHERE bukrs EQ p_bukrs-low.

    CONCATENATE  'Empresa: ' '   '  p_bukrs-low '   ' '  -  ' '   ' v1_butxt INTO w_texto3 .
    PERFORM f_construir_cabecalho USING 'S' w_texto3.

  ENDIF.
ENDFORM.                    "f_iniciar_variaves

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

ENDFORM.                    " F_CONSTRUIR_CABECALHO

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
ENDFORM. "X_TOP_PAGE´

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

  CASE l_selfield-fieldname.
    WHEN 'HKONT'.
      READ TABLE it_saida INTO wa_saida INDEX l_selfield-tabindex.
      SET PARAMETER ID 'SAK' FIELD wa_saida-hkont. "preenche o campo da tela de pesquisa
      CALL TRANSACTION 'FS00' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado
    WHEN  'BELNR'.
      READ TABLE it_saida INTO wa_saida INDEX l_selfield-tabindex.
      SET PARAMETER ID 'BLN' FIELD wa_saida-belnr. "preenche o campo da tela de pesquisa
      SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
      SET PARAMETER ID 'GJR' FIELD p_auart-low.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado
    WHEN  'SAKNR'.
      READ TABLE it_saida_pn INTO wa_saida_pn INDEX l_selfield-tabindex.
      SET PARAMETER ID 'SAK' FIELD wa_saida_pn-saknr. "preenche o campo da tela de pesquisa
      CALL TRANSACTION 'FS00' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado
  ENDCASE.

ENDFORM.                    "f_user_command

*&---------------------------------------------------------------------*
*&      Form  Z_GRAVAR_DADOS_ZFIT0024
*&---------------------------------------------------------------------*
*       Gravar Dados na tabela ZFIT0024
*----------------------------------------------------------------------*
FORM z_gravar_dados_zfit0024.

  LOOP AT it_zfit0024 INTO wa_zfit0024.

    MODIFY zfit0024 FROM wa_zfit0024.

  ENDLOOP.

  CLEAR: wa_zfit0024.

ENDFORM.                    "z_gravar_dados_zlest0014

**&---------------------------------------------------------------------*
**&      Form  ABRE_XML
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_ANEEL  text
**----------------------------------------------------------------------*
*FORM ABRE_XML  USING    P_ANEEL.
*  PERFORM CAB USING XML_BMP P_ANEEL .
*ENDFORM.                    " ABRE_XML
*
**&---------------------------------------------------------------------*
**&      Form  CTNAB
**&---------------------------------------------------------------------*
**       function de abertura de tag
**----------------------------------------------------------------------*
*FORM CAB USING VALUE(TAG) XML.
*  CONCATENATE XML '<' TAG '>' SPACE INTO XML.
*ENDFORM. " CTNAB

*&---------------------------------------------------------------------*
*       function de abertura de tag
*----------------------------------------------------------------------*
FORM ctnab  USING VALUE(tag).
  CONCATENATE '<' tag '>' space INTO w_arquivo.
  APPEND w_arquivo TO t_arquivo.
ENDFORM. " CTNAB

*&---------------------------------------------------------------------*
*&      Form  CTNAV
*&---------------------------------------------------------------------*
*       function de fechamento de campo obrigatorio
*----------------------------------------------------------------------*
FORM ctnav  USING  VALUE(tag) VALUE(conteudo).
  CONCATENATE '<' tag '>' conteudo '</' tag '>' INTO  w_arquivo.
  APPEND w_arquivo TO t_arquivo.
ENDFORM.                    " CTNAV

*&---------------------------------------------------------------------*
*&      Form  CTNFE
*&---------------------------------------------------------------------*
*       function de fechamento de tag
*----------------------------------------------------------------------*
FORM ctnfe  USING VALUE(tag) .
  CONCATENATE '</' tag '>' INTO w_arquivo.
  APPEND w_arquivo TO t_arquivo.
ENDFORM. " CTNFE
