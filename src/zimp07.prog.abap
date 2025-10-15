************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 09.05.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Efetivação de Lançamento de Tributos a Pagar        *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 09.05.2009    Desenvolvedor ABAP   Criação              DEVK905828   *
************************************************************************

REPORT  zimp07.

*-----------------------------------------------------------------------
* Declaração para SELECT-OPTIONS
*-----------------------------------------------------------------------
TABLES:
  b120,
* Início Alteração Ricardo Furst.
  zimp_detalhe.
* Fim Alteração Ricardo Furst.

*-----------------------------------------------------------------------
* Pool de tipos
*-----------------------------------------------------------------------
TYPE-POOLS:
  slis.

*-----------------------------------------------------------------------
* Tipos
*-----------------------------------------------------------------------
TYPES:

  BEGIN OF ty_relatorio,
    nro_doc_tr       TYPE zimp_detalhe-nro_doc_tr,
    buzei            TYPE zimp_detalhe-buzei,
* Início Alteração Ricardo Furst.
    gjahr            TYPE zimp_detalhe-gjahr,
* Fim Alteração Ricardo Furst.
    aprovador        TYPE c,
    tp_arrec         TYPE zimp_cabecalho-tp_arrec,
    zfbdt            TYPE zimp_cabecalho-zfbdt,
    vlr_principal    TYPE zimp_detalhe-vlr_principal,
    vlr_multa        TYPE zimp_detalhe-vlr_multa,
    vlr_juros        TYPE zimp_detalhe-vlr_juros,
    vlr_total        TYPE zimp_detalhe-vlr_principal,
    gsber            TYPE zimp_detalhe-gsber,
    lifnr            TYPE zimp_cabecalho-lifnr,
    sgtxt            TYPE zimp_detalhe-sgtxt,
    mes_ano_comp(7)  TYPE c,
    cod_pgto         TYPE zimp_cabecalho-cod_pgto,
    forn_retencao    TYPE zimp_detalhe-lifnr,
    cod_ident        TYPE zimp_detalhe-cod_ident,
    cta_multa        TYPE zimp_cabecalho-cta_multa,
    cta_juros        TYPE zimp_cabecalho-cta_juros,
    cod_barras       TYPE zimp_detalhe-cod_barras,
  END OF ty_relatorio.

*-----------------------------------------------------------------------
* Tabelas internas
*-----------------------------------------------------------------------
DATA:

  t_zimp_cabecalho   TYPE TABLE OF zimp_cabecalho WITH HEADER LINE,
  t_zimp_detalhe     TYPE TABLE OF zimp_detalhe,
  t_relatorio        TYPE TABLE OF ty_relatorio,
  t_fieldcat         TYPE slis_t_fieldcat_alv,
  t_listheader       TYPE slis_t_listheader,
***  t_bdc              TYPE TABLE OF bdcdata INITIAL SIZE 30,
  t_mess             TYPE TABLE OF bdcmsgcoll,
* Início Alteração Ricardo Furst.
  t_zimp_det_aux     TYPE STANDARD TABLE OF zimp_detalhe WITH HEADER LINE.
* Fim Alteração Ricardo Furst.


****************************************
DATA: t_bdc LIKE bdcdata OCCURS 0 WITH HEADER LINE,
      t_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: mensg LIKE message VALUE IS INITIAL,
      msgno LIKE sy-msgno.
*****************************************

*-----------------------------------------------------------------------
* Estruturas
*-----------------------------------------------------------------------
DATA:

  w_relatorio        TYPE ty_relatorio,
  w_fieldcat         TYPE slis_fieldcat_alv,
  w_layout           TYPE slis_layout_alv,
  w_listheader       TYPE slis_listheader,
  w_bdc              TYPE bdcdata.

DATA:
  lv_wrbtr(15)  TYPE c,
  lv_juros(15)  TYPE c,
  lv_multa(15)  TYPE c,
  lv_nro_doc_tr TYPE zimp_cabecalho-nro_doc_tr,
  lv_budat(10)  TYPE c,
  lv_bldat(10)  TYPE c,
  lv_zfbdt(10)  TYPE c,
  lv_dt_apur(7) TYPE c.

DATA:
  lw_bdc_opt    TYPE ctu_params.

*-----------------------------------------------------------------------
* Símbolos de campo
*-----------------------------------------------------------------------
FIELD-SYMBOLS:

  <f_zimp_cabecalho> TYPE zimp_cabecalho,
  <f_zimp_detalhe>   TYPE zimp_detalhe,
  <f_relatorio>      TYPE ty_relatorio,
  <f_mess>           TYPE bdcmsgcoll.

*-----------------------------------------------------------------------
* Constantes
*-----------------------------------------------------------------------
CONSTANTS:

  c_s                TYPE c              VALUE 'S',
  c_e                TYPE c              VALUE 'E',
  c_x                TYPE c              VALUE 'X',
  c_0000             TYPE bdcdata-dynpro VALUE '0000'.

*-----------------------------------------------------------------------
* Parâmetros de seleção
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-t01.

SELECT-OPTIONS:
  s_nr_doc           FOR <f_zimp_detalhe>-nro_doc_tr
                     NO-EXTENSION NO INTERVALS.

PARAMETERS:
  p_emp              TYPE b120-bukrs
                     OBLIGATORY.

SELECT-OPTIONS:
  s_filial           FOR b120-j_1bbranch
                     NO-EXTENSION NO INTERVALS,

  s_dtlanc           FOR <f_zimp_cabecalho>-budat
                     OBLIGATORY,

  s_dtvenc           FOR <f_zimp_cabecalho>-zfbdt,
  s_codpag           FOR <f_zimp_cabecalho>-cod_pgto,
  s_tparrc           FOR <f_zimp_cabecalho>-tp_arrec
                     NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b01.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Busca dados nas tabelas e verifica a consistência
  PERFORM zf_busca_dados.

* Monta relatório
  PERFORM zf_monta_relatorio.

* Configura e exibe relatório
  PERFORM zf_exibe_relatorio. "<<<

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       Busca dados para processamento
*----------------------------------------------------------------------*
FORM zf_busca_dados .
* Início Alteração Ricardo Furst 08.07.2009
  IF  s_filial-low IS INITIAL.

    CLEAR: s_filial.
    DELETE s_filial INDEX 1.

  ENDIF.
* Fim Alteração Ricardo Furst 08.07.2009
* Seleciona cabeçalho dos documentos
  SELECT *
    INTO TABLE t_zimp_cabecalho
    FROM zimp_cabecalho
    WHERE bukrs       = p_emp    AND
          nro_doc_tr IN s_nr_doc AND
          budat      IN s_dtlanc AND
          zfbdt      IN s_dtvenc AND
          cod_pgto   IN s_codpag AND
          tp_arrec   IN s_tparrc AND
          belnr       = space.

  IF NOT sy-batch IS INITIAL.
    WRITE:/ 'selecionou:', sy-dbcnt, 'registros' .
  ENDIF.

*** Busca os documentos estornados
  IF sy-subrc <> 0.
    SELECT *
    INTO TABLE t_zimp_cabecalho
    FROM zimp_cabecalho
    WHERE bukrs       = p_emp    AND
          nro_doc_tr IN s_nr_doc AND
          budat      IN s_dtlanc AND
          zfbdt      IN s_dtvenc AND
          cod_pgto   IN s_codpag AND
          tp_arrec   IN s_tparrc AND
          belnr      NE space    AND
          estorno    EQ 'X'.
  ENDIF.

  IF sy-subrc = 0.

    SORT t_zimp_cabecalho BY nro_doc_tr gjahr.

*   Seleciona itens dos documentos
    SELECT *
      INTO TABLE t_zimp_detalhe
      FROM zimp_detalhe
      WHERE bukrs       = p_emp
        AND nro_doc_tr IN s_nr_doc
* Início Alteração Ricardo Furst.
        AND job EQ 'X'.
* Fim Alteração Ricardo Furst.


    IF sy-subrc = 0.

*     Efetua o filtro por filial
      DELETE t_zimp_detalhe WHERE NOT gsber IN s_filial.

    ENDIF.

  ENDIF.

* Verifica se existem registros válidos
  IF t_zimp_detalhe[] IS INITIAL.
    MESSAGE text-e01 TYPE c_s DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " ZF_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_RELATORIO
*&---------------------------------------------------------------------*
*       Monta estrutura do relatório
*----------------------------------------------------------------------*
FORM zf_monta_relatorio .

  LOOP AT t_zimp_detalhe ASSIGNING <f_zimp_detalhe>.

    READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>
      WITH KEY nro_doc_tr = <f_zimp_detalhe>-nro_doc_tr
               gjahr      = <f_zimp_detalhe>-gjahr
      BINARY SEARCH.

    IF sy-subrc = 0.

      w_relatorio-tp_arrec      = <f_zimp_cabecalho>-tp_arrec.
      w_relatorio-nro_doc_tr    = <f_zimp_detalhe>-nro_doc_tr.
      w_relatorio-zfbdt         = <f_zimp_cabecalho>-zfbdt.
      w_relatorio-vlr_principal = <f_zimp_detalhe>-vlr_principal.
      w_relatorio-vlr_multa     = <f_zimp_detalhe>-vlr_multa.
      w_relatorio-vlr_juros     = <f_zimp_detalhe>-vlr_juros.
      w_relatorio-vlr_total     = w_relatorio-vlr_principal +
                                  w_relatorio-vlr_multa     +
                                  w_relatorio-vlr_juros.
      w_relatorio-sgtxt         = <f_zimp_detalhe>-sgtxt.
      w_relatorio-gsber         = <f_zimp_detalhe>-gsber.
      w_relatorio-lifnr         = <f_zimp_cabecalho>-lifnr.

      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
        EXPORTING
          input  = <f_zimp_cabecalho>-mes_ano_comp
        IMPORTING
          output = w_relatorio-mes_ano_comp.

      w_relatorio-cod_pgto      = <f_zimp_cabecalho>-cod_pgto.
      w_relatorio-forn_retencao = <f_zimp_detalhe>-lifnr.
      w_relatorio-lifnr         = <f_zimp_cabecalho>-lifnr.
      w_relatorio-cod_ident     = <f_zimp_detalhe>-cod_ident.
      w_relatorio-cta_multa     = <f_zimp_cabecalho>-cta_multa.
      w_relatorio-cta_juros     = <f_zimp_cabecalho>-cta_juros.
      w_relatorio-cod_barras    = <f_zimp_detalhe>-cod_barras.
      w_relatorio-buzei         = <f_zimp_detalhe>-buzei.
* Início Alteração Ricardo Furst.
      w_relatorio-gjahr         = <f_zimp_detalhe>-gjahr.
* Fim Alteração Ricardo Furst.

      APPEND w_relatorio TO t_relatorio.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " ZF_MONTA_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*       Configura e exibe relatório
*----------------------------------------------------------------------*
FORM zf_exibe_relatorio .

  w_layout-colwidth_optimize = c_x.
  w_layout-zebra             = c_x.
  w_layout-box_fieldname     = 'APROVADOR'.

  PERFORM zf_fieldcat USING:
    'NRO_DOC_TR'    'Nro. Doc.'(002),
    'ZFBDT'         'Dt. Vcto.'(004),
    'GSBER'         'Filial'(011).

  w_fieldcat-inttype = 'N'.

  PERFORM zf_fieldcat USING:
    'LIFNR'         'Conta Fornecedor'(012),
    'CTA_MULTA'     'Conta Multa'(013),
    'CTA_JUROS'     'Conta Juros'(014).

  CLEAR w_fieldcat-inttype.

  PERFORM zf_fieldcat USING:
    'TP_ARREC'      'Tipo'(001),
    'MES_ANO_COMP'  'Mês/Ano Comp.'(015),
    'COD_PGTO'      'Código Pgto.'(016).

  w_fieldcat-inttype = 'N'.

  PERFORM zf_fieldcat USING
    'FORN_RETENCAO' 'Forn. retenção'(017).

  CLEAR w_fieldcat-inttype.

  PERFORM zf_fieldcat USING:
    'COD_IDENT'     'Identificação'(018),
    'VLR_PRINCIPAL' 'Vlr. Principal'(005),
    'VLR_MULTA'     'Vlr. Multa'(006),
    'VLR_JUROS'     'Vlr. Juros'(007),
    'VLR_TOTAL'     'Vlr. Total'(010),
    'SGTXT'         'Texto'(008),
    'COD_BARRAS'    'Cód. barras'(019).

* Início Alteração Ricardo Furst 13.07.2009
  PERFORM zf_alv_user_command.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program       = sy-repid
*      i_callback_pf_status_set = 'ZF_ALV_STATUS_SET'
*      i_callback_user_command  = 'ZF_ALV_USER_COMMAND'
*      i_callback_top_of_page   = 'ZF_ALV_TOP_OF_PAGE'
*      is_layout                = w_layout
*      it_fieldcat              = t_fieldcat
*    TABLES
*      t_outtab                 = t_relatorio
*    EXCEPTIONS
*      program_error            = 1
*      OTHERS                   = 2.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
* Fim Alteração Ricardo Furst 13.07.2009
ENDFORM.                    " ZF_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  ZF_FIELDCAT
*&---------------------------------------------------------------------*
*       Monta ALV Fieldcat
*----------------------------------------------------------------------*
FORM zf_fieldcat  USING    p_field TYPE slis_fieldcat_alv-fieldname
                           p_text  TYPE slis_fieldcat_alv-seltext_l.

  w_fieldcat-fieldname = p_field.
  w_fieldcat-seltext_l = p_text.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ZF_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  zf_alv_status_set
*&---------------------------------------------------------------------*
*       PF-STATUS ALV
*----------------------------------------------------------------------*
FORM zf_alv_status_set USING lt_extab TYPE slis_t_extab.    "#EC CALLED

  SET PF-STATUS 'ZIMP04' EXCLUDING lt_extab.

ENDFORM.                    "zf_alv_status_set

*&---------------------------------------------------------------------*
*&      Form  zf_alv_top_of_page
*&---------------------------------------------------------------------*
*       TOP-OF-PAGE ALV
*----------------------------------------------------------------------*
FORM zf_alv_top_of_page.                                    "#EC CALLED

* Variáveis locais
  DATA:
    lv_dtlanc(10) TYPE c.


  IF t_listheader[] IS INITIAL.

    w_listheader-typ  = 'S'.
    w_listheader-key  = 'Empresa'(009).
    w_listheader-info = p_emp.

    APPEND w_listheader TO t_listheader.

    WRITE s_dtlanc-low TO lv_dtlanc.

    CONCATENATE lv_dtlanc space 'até' space INTO w_listheader-info
      SEPARATED BY space.

    WRITE s_dtlanc-high TO lv_dtlanc.

    CONCATENATE w_listheader-info lv_dtlanc INTO w_listheader-info
      SEPARATED BY space.

    w_listheader-key = 'Data Lançamento'.

    APPEND w_listheader TO t_listheader.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_listheader.

ENDFORM.                    "zf_alv_top_of_page

*&---------------------------------------------------------------------*
*&      Form  zf_alv_user_command
*&---------------------------------------------------------------------*
*       USER-COMMAND ALV
*----------------------------------------------------------------------*
FORM zf_alv_user_command.

  DATA: v_msg            LIKE sy-lisel,
        v_message_id     LIKE  sy-msgid,
        v_message_number LIKE  sy-msgno,
        v_message_var1   LIKE  sy-msgv1,
        v_message_var2   LIKE  sy-msgv2,
        v_message_var3   LIKE  sy-msgv3,
        v_message_var4   LIKE  sy-msgv4.

* Início Alteração Ricardo Furst 13.07.2009
*FORM zf_alv_user_command USING r_ucomm     TYPE sy-ucomm
*                               ls_selfield TYPE slis_selfield. "#EC CALLED
* Fim Alteração Ricardo Furst 13.07.2009

* Variáveis locais
*  DATA:
*    lv_wrbtr(15)  TYPE c,
*    lv_juros(15)  TYPE c,
*    lv_multa(15)  TYPE c,
*    lv_nro_doc_tr TYPE zimp_cabecalho-nro_doc_tr,
*    lv_budat(10)  TYPE c,
*    lv_bldat(10)  TYPE c,
*    lv_zfbdt(10)  TYPE c,
*    lv_dt_apur(7) TYPE c.
*
** Estruturas locais
*  DATA:
*    lw_bdc_opt    TYPE ctu_params.


* Processa a aprovação
*  CHECK r_ucomm = 'DATA_SAVE'.

  lw_bdc_opt-defsize  = c_x.
  lw_bdc_opt-racommit = c_x.
  lw_bdc_opt-nobinpt  = c_x.
  lw_bdc_opt-dismode  = 'N'.

  LOOP AT t_relatorio ASSIGNING <f_relatorio>.
* Início Alteração Ricardo Furst 13.07.2009
*    WHERE aprovador = c_x.
* Fim Alteração Ricardo Furst 13.07.2009

    REFRESH t_bdc.

    READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>
      WITH KEY nro_doc_tr = <f_relatorio>-nro_doc_tr.

    READ TABLE t_zimp_detalhe ASSIGNING <f_zimp_detalhe>
      WITH KEY nro_doc_tr = <f_relatorio>-nro_doc_tr
               buzei      = <f_relatorio>-buzei.

* Formatando datas * BUDAT | BLDAT
    WRITE: <f_zimp_cabecalho>-budat TO lv_budat,
           <f_zimp_cabecalho>-bldat TO lv_bldat,
           <f_zimp_cabecalho>-zfbdt TO lv_zfbdt.

    PERFORM zf_insere_bdc USING:
      'SAPMF05A' '0100' c_x   space space,
      space      c_0000 space 'BKPF-BLDAT'  lv_bldat,
      space      c_0000 space 'BKPF-BLART'  <f_zimp_cabecalho>-blart,
      space      c_0000 space 'BKPF-BUKRS'  <f_zimp_cabecalho>-bukrs,
      space      c_0000 space 'BKPF-BUDAT'  lv_budat,
      space      c_0000 space 'BKPF-WAERS'  'BRL',
      space      c_0000 space 'BDC_OKCODE'  '/00',
      space      c_0000 space 'RF05A-NEWBS' '31',
      space      c_0000 space 'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr.

    IF <f_zimp_cabecalho>-dt_per_apur IS INITIAL.

      PERFORM zf_insere_bdc USING:
        space      c_0000 space 'BKPF-XBLNR' <f_relatorio>-mes_ano_comp.

    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
        EXPORTING
          input  = <f_zimp_cabecalho>-dt_per_apur
        IMPORTING
          output = lv_dt_apur.

      PERFORM zf_insere_bdc USING:
        space      c_0000 space 'BKPF-XBLNR' lv_dt_apur.

    ENDIF.

    WRITE <f_relatorio>-vlr_total TO lv_wrbtr.

    PERFORM zf_insere_bdc USING:
      space      c_0000 space 'BDC_OKCODE' '/00',

      'SAPMF05A' '0302' c_x   space space,
      space      c_0000 space 'BSEG-WRBTR' lv_wrbtr,
      space      c_0000 space 'BSEG-BUPLA' <f_zimp_detalhe>-gsber,
      space      c_0000 space 'BSEG-GSBER' <f_zimp_detalhe>-gsber,
      space      c_0000 space 'BSEG-ZFBDT' lv_zfbdt,
      space      c_0000 space 'BSEG-ZLSCH' 'P',
      space      c_0000 space 'BSEG-ZUONR' <f_zimp_detalhe>-cod_ident,
      space      c_0000 space 'BSEG-SGTXT' <f_zimp_detalhe>-sgtxt,
      space      c_0000 space 'RF05A-NEWBS' '40',
      space      c_0000 space 'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto,
      space      c_0000 space 'BDC_OKCODE' '=ZK'.

* Início Alteração Ricardo Furst 15.07.2009
    PERFORM zf_insere_bdc USING:

      'SAPMF05A' '0332' c_x   space space,
      space      c_0000 space 'BDC_OKCODE'  '/00',
      space      c_0000 space 'BSEG-DMBE2'  ' ',
      space      c_0000 space 'BSEG-DMBE3'  ' ',
      space      c_0000 space 'BSEG-HBKID'  'BBD',
      space      c_0000 space 'RF05A-NEWBS' '40',
      space      c_0000 space 'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto.
* Fim Alteração Ricardo Furst 15.07.2009

    WRITE <f_relatorio>-vlr_principal TO lv_wrbtr.

    PERFORM zf_insere_bdc USING:

      'SAPMF05A' '0300' c_x   space space,
*      space      c_0000 space 'BDC_OKCODE'  '=BU',
      space      c_0000 space 'BDC_OKCODE'  '/00',
      space      c_0000 space 'BSEG-WRBTR'  lv_wrbtr,
      space      c_0000 space 'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,
      space      c_0000 space 'BSEG-ZUONR'  <f_zimp_detalhe>-cod_ident,
      space      c_0000 space 'COBL-GSBER'  <f_zimp_detalhe>-gsber,
      space      c_0000 space 'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt.
    IF NOT <f_zimp_cabecalho>-cta_multa IS INITIAL.
      PERFORM verifica_multa.
    ENDIF.

    IF NOT <f_zimp_cabecalho>-cta_juros IS INITIAL.
      PERFORM verifica_juros.
    ENDIF.

* Início Alteração Ricardo Furst 15.07.2009
    PERFORM zf_insere_bdc USING:

      'SAPMF05A' '0300' c_x   space space,
      space      c_0000 space 'BDC_OKCODE'  '=BS',
      space      c_0000 space 'BSEG-WRBTR'  lv_wrbtr,
      space      c_0000 space 'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,
      space      c_0000 space 'BSEG-ZUONR'  <f_zimp_detalhe>-cod_ident,
      space      c_0000 space 'COBL-GSBER'  <f_zimp_detalhe>-gsber,
      space      c_0000 space 'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt.
*    IF NOT <f_zimp_cabecalho>-cta_multa IS INITIAL.
*      PERFORM verifica_multa.
*    ENDIF.
*
*    IF NOT <f_zimp_cabecalho>-cta_juros IS INITIAL.
*      PERFORM verifica_juros.
*    ENDIF.
* Fim Alteração Ricardo Furst 15.07.2009



    PERFORM zf_insere_bdc USING:
      space     c_0000 space 'BDC_OKCODE' '=BU'.
* Início Alteração Ricardo Furst 15.07.2009
    IF <f_zimp_cabecalho>-dt_per_apur IS INITIAL.

      PERFORM zf_insere_bdc USING:
        space      c_0000 space 'BKPF-XBLNR' <f_relatorio>-mes_ano_comp.

    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
        EXPORTING
          input  = <f_zimp_cabecalho>-dt_per_apur
        IMPORTING
          output = lv_dt_apur.

      PERFORM zf_insere_bdc USING:
        space      c_0000 space 'BKPF-XBLNR' lv_dt_apur.

    ENDIF.
* Fim Alteração Ricardo Furst 15.07.2009

***********************************************************

    DATA: v_tabix TYPE sy-tabix,
          v_lifnr TYPE zimp_cabecalho-lifnr.

*    CLEAR: t_zimp_cabecalho.
*    SELECT *
****       lifnr
****           cta_imposto
****           cta_multa
****           cta_juros
****           cta_at_mon
****           cta_tse
*       INTO TABLE t_zimp_cabecalho
*       FROM zimp_cabecalho
*       WHERE bukrs  = p_emp    AND
*       lifnr <> '' AND cta_imposto <> ''
*      AND cta_multa <> '' AND cta_juros <> ''
*      AND cta_at_mon <> '' AND cta_tse <> '' .
*    IF sy-subrc = 0.
*      SORT t_zimp_cabecalho BY lifnr.
*      DELETE adjacent duplicates FROM t_zimp_cabecalho.
*    ENDIF.
*
*    CLEAR v_tabix.
*
*    v_tabix = sy-tabix.
*
*    LOOP AT t_zimp_cabecalho.
*      at FIRST.
*      v_lifnr = t_zimp_cabecalho-lifnr.
*      ENDAT.
*      at END OF lifnr.
*      v_tabix = sy-tabix.
*      ENDAT.
*    ENDLOOP.
*
** tem q selecionar por cada lifnr.<<<<<<<<<<<<<<
*    CASE v_tabix.
*      WHEN '2'.
*        PERFORM f_shdb02.
*      WHEN '3'.
*        PERFORM f_shdb03.
*      WHEN '4'.
*        PERFORM f_shdb04.
*      WHEN '5'.
*        PERFORM f_shdb05.
*      WHEN '6'.
*        PERFORM f_shdb06.
*     eNDCASE.

*    CLEAR v_lifnr.

    READ TABLE t_zimp_cabecalho INDEX 1.

*CTA_IMPOSTO
*CTA_MULTA
*CTA_JUROS
*CTA_AT_MON
*CTA_TSE

    IF     NOT t_zimp_cabecalho-cta_tse     IS INITIAL AND
           NOT t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
           NOT t_zimp_cabecalho-cta_juros   IS INITIAL AND
           NOT t_zimp_cabecalho-cta_multa   IS INITIAL AND
           NOT t_zimp_cabecalho-cta_imposto IS INITIAL.

      PERFORM f_shdb06.

    ELSEIF     t_zimp_cabecalho-cta_tse    IS INITIAL AND
           NOT t_zimp_cabecalho-cta_at_mon IS INITIAL AND
           NOT t_zimp_cabecalho-cta_juros  IS INITIAL AND
           NOT t_zimp_cabecalho-cta_multa  IS INITIAL.

      PERFORM f_shdb05.

    ELSEIF     t_zimp_cabecalho-cta_tse    IS INITIAL AND
               t_zimp_cabecalho-cta_at_mon IS INITIAL AND
           NOT t_zimp_cabecalho-cta_juros  IS INITIAL AND
           NOT t_zimp_cabecalho-cta_multa  IS INITIAL.

      PERFORM f_shdb04.

    ELSEIF      t_zimp_cabecalho-cta_tse    IS INITIAL AND
                t_zimp_cabecalho-cta_at_mon IS INITIAL AND
                t_zimp_cabecalho-cta_juros  IS INITIAL AND
            NOT t_zimp_cabecalho-cta_multa  IS INITIAL.

      PERFORM f_shdb03.

    ELSEIF     t_zimp_cabecalho-cta_tse     IS INITIAL AND
               t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
               t_zimp_cabecalho-cta_juros   IS INITIAL AND
               t_zimp_cabecalho-cta_multa   IS INITIAL AND
           NOT t_zimp_cabecalho-cta_imposto IS INITIAL.

      PERFORM f_shdb02.

    ELSEIF     NOT t_zimp_cabecalho-cta_tse     IS INITIAL AND
                   t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
                   t_zimp_cabecalho-cta_juros   IS INITIAL AND
                   t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb03_novo.

    ELSEIF         t_zimp_cabecalho-cta_tse     IS INITIAL AND
                   t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
               NOT t_zimp_cabecalho-cta_juros   IS INITIAL AND
                   t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb03_novo2.

    ELSEIF         t_zimp_cabecalho-cta_tse     IS INITIAL AND
               NOT t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
                   t_zimp_cabecalho-cta_juros   IS INITIAL AND
                   t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb03_novo3.

    ELSEIF     NOT t_zimp_cabecalho-cta_tse     IS INITIAL AND
               NOT t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
                   t_zimp_cabecalho-cta_juros   IS INITIAL AND
                   t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb04_novo.

    ELSEIF         t_zimp_cabecalho-cta_tse     IS INITIAL AND
               NOT t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
                   t_zimp_cabecalho-cta_juros   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb04_novo01.

    ELSEIF     NOT t_zimp_cabecalho-cta_tse     IS INITIAL AND
                   t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
                   t_zimp_cabecalho-cta_juros   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb04_novo02.

    ELSEIF         t_zimp_cabecalho-cta_tse     IS INITIAL AND
               NOT t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
               NOT t_zimp_cabecalho-cta_juros   IS INITIAL AND
                   t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb04_novo03.

    ELSEIF     NOT t_zimp_cabecalho-cta_tse     IS INITIAL AND
                   t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
               NOT  t_zimp_cabecalho-cta_juros   IS INITIAL AND
                   t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb04_novo04.

    ELSEIF     NOT t_zimp_cabecalho-cta_tse     IS INITIAL AND
               NOT t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
                   t_zimp_cabecalho-cta_juros   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb04_novo05.

    ELSEIF     NOT t_zimp_cabecalho-cta_tse     IS INITIAL AND
               NOT t_zimp_cabecalho-cta_at_mon  IS INITIAL AND
               NOT t_zimp_cabecalho-cta_juros   IS INITIAL AND
                   t_zimp_cabecalho-cta_multa   IS INITIAL AND
               NOT t_zimp_cabecalho-cta_imposto IS INITIAL.
      PERFORM f_shdb04_novo06.
    ENDIF.
**********************************************************
*    BREAK-POINT.
    CALL TRANSACTION 'F-43' USING t_bdc
       MESSAGES INTO t_mess OPTIONS FROM lw_bdc_opt.

    IF sy-subrc = 0.

      LOOP AT t_relatorio INTO w_relatorio.

        MOVE p_emp TO t_zimp_det_aux-bukrs.
        MOVE w_relatorio-nro_doc_tr TO t_zimp_det_aux-nro_doc_tr.
        MOVE w_relatorio-gjahr TO t_zimp_det_aux-gjahr.
        MOVE w_relatorio-buzei TO t_zimp_det_aux-buzei.
        MOVE space TO t_zimp_det_aux-job.

        UPDATE zimp_detalhe SET job = t_zimp_det_aux-job
          WHERE bukrs      = p_emp                    AND
                nro_doc_tr = t_zimp_det_aux-nro_doc_tr AND
                gjahr      = t_zimp_det_aux-gjahr AND
                buzei      = t_zimp_det_aux-buzei.
        APPEND t_zimp_det_aux.

      ENDLOOP.

      COMMIT WORK AND WAIT.

      READ TABLE t_mess ASSIGNING <f_mess>
        WITH KEY msgid = 'F5'
                 msgnr = '312'.

      IF sy-subrc = 0.

        UPDATE zimp_cabecalho SET belnr   = <f_mess>-msgv1
                                  estorno = ''
          WHERE bukrs      = p_emp AND
                nro_doc_tr = <f_relatorio>-nro_doc_tr.

        COMMIT WORK AND WAIT.

        UPDATE zimp_detalhe SET aprovador = 'X' "<f_relatorio>-aprovador
          WHERE bukrs      = t_zimp_cabecalho-bukrs      AND
                nro_doc_tr = t_zimp_cabecalho-nro_doc_tr AND
                gjahr      = t_zimp_cabecalho-gjahr.

        IF sy-subrc <> 0.
          ROLLBACK WORK.
          MESSAGE text-e03 TYPE 'A'.
        ENDIF.

        FORMAT COLOR COL_POSITIVE.
        WRITE:/ t_zimp_cabecalho-bukrs,
                t_zimp_cabecalho-nro_doc_tr, '-->',
                <f_mess>-msgv1(15).

        REFRESH t_mess.

*   Limpa linha do ALV
        DELETE t_relatorio.

      ENDIF.

    ELSE.

      LOOP AT t_relatorio INTO w_relatorio.

        MOVE p_emp TO t_zimp_det_aux-bukrs.
        MOVE w_relatorio-nro_doc_tr TO t_zimp_det_aux-nro_doc_tr.
        MOVE w_relatorio-gjahr TO t_zimp_det_aux-gjahr.
        MOVE w_relatorio-buzei TO t_zimp_det_aux-buzei.
        MOVE space TO t_zimp_det_aux-job.

        UPDATE zimp_detalhe SET job = t_zimp_det_aux-job
          WHERE bukrs      = p_emp                    AND
                nro_doc_tr = t_zimp_det_aux-nro_doc_tr AND
                gjahr      = t_zimp_det_aux-gjahr AND
                buzei      = t_zimp_det_aux-buzei.

        COMMIT WORK AND WAIT.

        APPEND t_zimp_det_aux.

      ENDLOOP.

      LOOP AT t_mess ASSIGNING <f_mess>.
        IF <f_mess>-msgtyp EQ 'E'.
*       it_msg-msgtyp eq 'S'.

          MOVE: <f_mess>-msgid TO v_message_id,
                <f_mess>-msgnr TO v_message_number,
                <f_mess>-msgv1 TO v_message_var1,
                <f_mess>-msgv2 TO v_message_var2,
                <f_mess>-msgv3 TO v_message_var3,
                <f_mess>-msgv4 TO v_message_var4.

          CALL FUNCTION 'RPY_MESSAGE_COMPOSE'
            EXPORTING
              message_id        = v_message_id
              message_number    = v_message_number
              message_var1      = v_message_var1
              message_var2      = v_message_var2
              message_var3      = v_message_var3
              message_var4      = v_message_var4
            IMPORTING
              message_text      = v_msg
            EXCEPTIONS
              message_not_found = 1
              OTHERS            = 2.

          FORMAT COLOR COL_NEGATIVE.
          WRITE:/ t_zimp_cabecalho-bukrs,
                  t_zimp_cabecalho-nro_doc_tr, '-->',
                  v_msg.
          SKIP 1.
*          WRITE:/ .
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDLOOP.
* Início Alteração Ricardo Furst 13.07.2009
*  ls_selfield-refresh = c_x.
* Fim Alteração Ricardo Furst 13.07.2009

ENDFORM.                    "zf_alv_user_command
****&---------------------------------------------------------------------*
****&      Form  ZF_INSERE_BDC
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
***FORM zf_insere_bdc USING p_program  TYPE bdcdata-program
***                         p_dynpro   TYPE bdcdata-dynpro
***                         p_dynbegin TYPE bdcdata-dynbegin
***                         p_fnam     TYPE bdcdata-fnam
***                         p_fval     TYPE any.
***
***  w_bdc-program  = p_program.
***  w_bdc-dynpro   = p_dynpro.
***  w_bdc-dynbegin = p_dynbegin.
***  w_bdc-fnam     = p_fnam.
***  w_bdc-fval     = p_fval.
***
***  APPEND w_bdc TO t_bdc.
***
***ENDFORM.                    " ZF_INSERE_BDC
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_MULTA
*&---------------------------------------------------------------------*
FORM verifica_multa .

  WRITE <f_relatorio>-vlr_multa TO lv_wrbtr.

  PERFORM zf_insere_bdc USING:
    space      c_0000 space 'RF05A-NEWBS' '40',
    space      c_0000 space 'RF05A-NEWKO' <f_zimp_cabecalho>-cta_multa,
    space      c_0000 space 'BDC_OKCODE' '/00'.
*
*   'SAPMF05A' '0300' c_x   space space,
*    space      c_0000 space 'COBL-GSBER'  <f_zimp_detalhe>-gsber,
*    space      c_0000 space 'BSEG-WRBTR'  lv_wrbtr,
*    space      c_0000 space 'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,
**        space      c_0000 space 'BSEG-GSBER'  <f_zimp_detalhe>-gsber,
*    space      c_0000 space 'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt.

ENDFORM.                    " VERIFICA_MULTA
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_JUROS
*&---------------------------------------------------------------------*
FORM verifica_juros .

  WRITE <f_relatorio>-vlr_juros TO lv_wrbtr.

  PERFORM zf_insere_bdc USING:
    space      c_0000 space 'RF05A-NEWBS' '40',
    space      c_0000 space 'RF05A-NEWKO' <f_zimp_cabecalho>-cta_juros,
    space      c_0000 space 'BDC_OKCODE' '/00'.
*
*  'SAPMF05A' '0300' c_x   space space,
*    space      c_0000 space 'BDC_OKCODE'  '=BU',
*    space      c_0000 space 'BSEG-WRBTR'  lv_wrbtr,
*    space      c_0000 space 'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,
*    space      c_0000 space 'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt.

ENDFORM.                    " VERIFICA_JUROS
*&---------------------------------------------------------------------*
*&      Form  F_SHDB02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_shdb02 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.

  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '62,50',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '82,16',                                "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.


*  REFRESH t_msg.
*  CALL TRANSACTION 'F-43'
*    USING t_bdc MODE 'E'
*    MESSAGES INTO t_msg
*    UPDATE 'S'.
*
*  LOOP AT t_msg.
*    msgno = t_msg-msgnr.
*
*    CALL FUNCTION 'WRITE_MESSAGE'
*      EXPORTING
*        msgid  = t_msg-msgid     "Id. da mensagem
*        msgno  = msgno           "Número da mensagem
*        msgty  = t_msg-msgtyp    "Tipo de erro
*        msgv1  = t_msg-msgv1     "1º parâmetro
*        msgv2  = t_msg-msgv2     "2º parâmetro
*        msgv3  = t_msg-msgv3     "3º parâmetro
*        msgv4  = t_msg-msgv4     "4º parâmetro
*        msgv5  = ' '             "tabmess-fldname
*      IMPORTING
**              ERROR         =
*        messg         = mensg.
**              MSGLN         =
*    WRITE / mensg-msgtx.
*  ENDLOOP.


ENDFORM.                                                    " F_SHDB02
*&---------------------------------------------------------------------*
*&      Form  F_SHDB03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_shdb03 .

**  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
**
**  REFRESH t_bdc.
**  PERFORM insert_line USING:
**'X'  'SAPMF05A'  '100',
**' '  'BDC_CURSOR'	'RF05A-NEWKO',
**' '  'BDC_OKCODE'	 '/00',
**' '	 'BKPF-BLDAT' lv_bldat, "'21072009',
**' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
**' '  'BKPF-BUKRS'	 <f_zimp_cabecalho>-bukrs,                "'0001',
**' '  'BKPF-BUDAT'	lv_budat, "'21072009',
**' '  'BKPF-MONAT'	'7',
**' '  'BKPF-WAERS'	'BRL',
**' '  'BKPF-XBLNR'	<f_relatorio>-mes_ano_comp,               "'04/2009',
**' '  'RF05A-NEWBS'  '31',
**' '  'RF05A-NEWKO'  <f_zimp_cabecalho>-lifnr,               "'100210',
**' '  'BDC_SUBSCR'   'SAPMF05A',     "  1300APPL_SUB_T
**' '  'BDC_SUBSCR'	'SAPLSEXM'.
**
**  PERFORM insert_line USING:
**  'X'  'SAPMF05A'	 '302',
**  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
**  ' '  'BDC_OKCODE'	'=ZK',
**  ' '  'BSEG-WRBTR'	'120,00',
**  ' '  'BSEG-GSBER'	'0101',
**  ' '  'BSEG-ZTERM'	'Z001',
**  ' '	 'BSEG-ZBD1T'	'2',
**  ' '  'BSEG-ZFBDT'	'30072009',
**  ' '  'BSEG-ZLSCH'	'P',
**  ' '  'BSEG-ZUONR'	'REG_03',
**  ' '  'BSEG-SGTXT'	'PRINCIPAL/MULTA',
**  ' '  'RF05A-NEWBS'  '40',
**  ' '  'RF05A-NEWKO'  '431100',
**  ' '  'BDC_SUBSCR'	'SAPLKACB',                             "1004BLOCK
**  ' '  'COBL-GSBER'	'0101'.
**
**  PERFORM insert_line USING:
**  'X'  'SAPMF05A'  '300',
**  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
**  ' '  'BDC_OKCODE' '=BS',
**  ' '  'BSEG-WRBTR'	 '20,00',
**  ' '  'BSEG-BUPLA'	 '0101',
**  ' '  'BSEG-ZUONR'   'REG_03',
**  ' '  'BSEG-SGTXT'	 'PRINCIPAL/MULTA',
**  ' '  'BDC_SUBSCR'   'SAPLKACB',
**  ' '  'COBL-GSBER'   '0101'.
**
**  PERFORM insert_line USING:
**  'X'  'SAPMF05A'  '700',
**  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
**  ' '  'BDC_OKCODE'	'=BU',
**  ' '  'BKPF-XBLNR'	'04/2009'.


  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '75,00',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '98,59',                                "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_multa. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_multa TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.

ENDFORM.                                                    " F_SHDB03
*&---------------------------------------------------------------------*
*&      Form  F_SHDB04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_shdb04 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_multa. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_multa TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_juros. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_juros TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.



ENDFORM.                                                    " F_SHDB04
*&---------------------------------------------------------------------*
*&      Form  F_SHDB05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_shdb05 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_multa. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_multa TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_juros. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_juros TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_at_mon. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_atual_mone TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.



ENDFORM.                                                    " F_SHDB05
*&---------------------------------------------------------------------*
*&      Form  F_SHDB06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_shdb06 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .


  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.


  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_multa. "lifnr. "'213600'.


  WRITE <f_zimp_detalhe>-vlr_multa TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_juros. "lifnr. "'213600'.


  WRITE <f_zimp_detalhe>-vlr_juros TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_at_mon. "lifnr. "'213600'.


  WRITE <f_zimp_detalhe>-vlr_atual_mone TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_tse. "lifnr. "'213600'.



  WRITE <f_zimp_detalhe>-tse TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.


ENDFORM.                                                    " F_SHDB06





*&---------------------------------------------------------------------*
*&      Form  ZF_INSERE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_insere_bdc USING p_program  TYPE bdcdata-program
                         p_dynpro   TYPE bdcdata-dynpro
                         p_dynbegin TYPE bdcdata-dynbegin
                         p_fnam     TYPE bdcdata-fnam
                         p_fval     TYPE any.

  w_bdc-program  = p_program.
  w_bdc-dynpro   = p_dynpro.
  w_bdc-dynbegin = p_dynbegin.
  w_bdc-fnam     = p_fnam.
  w_bdc-fval     = p_fval.

  APPEND w_bdc TO t_bdc.

ENDFORM.                    " ZF_INSERE_BDC
*&---------------------------------------------------------------------*
*&      Form  INSERT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1447   text
*      -->P_1448   text
*      -->P_1449   text
*----------------------------------------------------------------------*
**form INSERT_LINE  using    value(p_1447)
**                           value(p_1448)
**                           value(p_1449).


FORM insert_line USING u_start TYPE c u_name TYPE c u_value.

  CLEAR t_bdc.

  MOVE u_start TO t_bdc-dynbegin.

  IF u_start = 'X'.
    MOVE:
    u_name  TO t_bdc-program,
    u_value TO t_bdc-dynpro.
  ELSE.
    MOVE:
    u_name  TO t_bdc-fnam,
    u_value TO t_bdc-fval.
  ENDIF.

  APPEND t_bdc.



ENDFORM.                    " INSERT_LINE
*&---------------------------------------------------------------------*
*&      Form  F_SHDB03_NOVO
*&---------------------------------------------------------------------*
FORM f_shdb03_novo .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '75,00',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '98,59',                                "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_tse. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-tse TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.



ENDFORM.                    " F_SHDB03_NOVO
*&---------------------------------------------------------------------*
*&      Form  f_shdb03_novo2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_shdb03_novo2 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '75,00',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '98,59',                                "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_juros. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_juros TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.



ENDFORM.                    " F_SHDB03_NOVO
*&---------------------------------------------------------------------*
*&      Form  f_shdb04_novo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_shdb04_novo .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_tse. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-tse TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_at_mon. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_atual_mone TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.



ENDFORM.                                                    " F_SHDB04
*&---------------------------------------------------------------------*
*&      Form  f_shdb03_novo3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_shdb03_novo3 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.

  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '75,00',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '98,59',                                "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_at_mon. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_atual_mone TO lv_wrbtr.



*  WRITE <f_zimp_detalhe>-vlr_juros TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.



ENDFORM.                    " F_SHDB03_NOVO
*&---------------------------------------------------------------------*
*&      Form  F_SHDB05_NOVO
*&---------------------------------------------------------------------*
FORM f_shdb04_novo01 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_multa. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_multa TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_at_mon. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_atual_mone TO lv_wrbtr.

  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.


ENDFORM.                    " F_SHDB05_NOVO
*&---------------------------------------------------------------------*
*&      Form  F_SHDB05_NOVO
*&---------------------------------------------------------------------*
FORM f_shdb04_novo02 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.
  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_multa. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_multa TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_tse. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-tse TO lv_wrbtr.

  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.


ENDFORM.                    " F_SHDB05_NOVO
*&---------------------------------------------------------------------*
*&      Form  F_SHDB04_NOVO03
*&---------------------------------------------------------------------*
FORM f_shdb04_novo03 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.


PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.

  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'    '300',
  ' '  'BDC_CURSOR'  'BSEG-SGTXT',
  ' '  'BDC_OKCODE'  '=BS',
  ' '  'BSEG-WRBTR'  lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'  <f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'  'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'  <f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_at_mon. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_atual_mone TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_juros. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_juros TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.


ENDFORM.                    " F_SHDB04_NOVO03
*&---------------------------------------------------------------------*
*&      Form  F_SHDB04_NOVO04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_shdb04_novo04 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.



PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.


  WRITE v_soma TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'    '300',
  ' '  'BDC_CURSOR'  'BSEG-SGTXT',
  ' '  'BDC_OKCODE'  '=BS',
  ' '  'BSEG-WRBTR'  lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'  <f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'  'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'  <f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_tse. "lifnr. "'213600'.
WRITE <f_zimp_detalhe>-tse TO lv_wrbtr.


*  WRITE <f_zimp_detalhe>-vlr_juros TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_juros. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_juros TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.


ENDFORM.                    " F_SHDB04_NOVO03
*&---------------------------------------------------------------------*
*&      Form  F_SHDB04_NOVO05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_shdb04_novo05 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.


PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.


  WRITE v_soma TO lv_wrbtr.



  PERFORM insert_line USING:
  'X'  'SAPMF05A'    '300',
  ' '  'BDC_CURSOR'  'BSEG-SGTXT',
  ' '  'BDC_OKCODE'  '=BS',
  ' '  'BSEG-WRBTR'  lv_wrbtr,                              "'100,00',
  ' '  'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,                "'0101',
  ' '  'BSEG-ZUONR'  <f_zimp_detalhe>-cod_ident,            "'REG_02',
  ' '  'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'  'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'  <f_zimp_detalhe>-gsber,                "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_tse. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-tse TO lv_wrbtr.









  PERFORM insert_line USING:
  'X'  'SAPMF05A'    '300',
  ' '  'BDC_CURSOR'  'BSEG-SGTXT',
  ' '  'BDC_OKCODE'  '=BS',
  ' '  'BSEG-WRBTR'  lv_wrbtr,                              "'100,00',
  ' '  'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,                "'0101',
  ' '  'BSEG-ZUONR'  <f_zimp_detalhe>-cod_ident,            "'REG_02',
  ' '  'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'  'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'  <f_zimp_detalhe>-gsber,                "'0101',
  ' '  'RF05A-NEWBS' '40',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_at_mon. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_atual_mone TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_multa. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_multa TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.


ENDFORM.                    " F_SHDB04_NOVO03
*&---------------------------------------------------------------------*
*&      Form  f_shdb04_novo06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_shdb04_novo06 .

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>.
*      WITH KEY nro_doc_tr .

  DATA: v_soma TYPE zimp_detalhe-vlr_multa.
  v_soma = <f_zimp_detalhe>-vlr_principal  + <f_zimp_detalhe>-vlr_juros +
           <f_zimp_detalhe>-vlr_multa      + <f_zimp_detalhe>-tse       +
           <f_zimp_detalhe>-vlr_outras_ent + <f_zimp_detalhe>-vlr_atual_mone.
  WRITE v_soma TO lv_wrbtr.

  REFRESH t_bdc.
  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0100',
  ' '  'BDC_CURSOR'	'RF05A-NEWKO',
  ' '  'BDC_OKCODE' '/00',
  ' '  'BKPF-BLDAT' lv_bldat,  "'21072009',
  ' '  'BKPF-BLART' <f_zimp_cabecalho>-blart, "'TB',
  ' '  'BKPF-BUKRS' <f_zimp_cabecalho>-bukrs,               "'0001',
  ' '  'BKPF-BUDAT' lv_budat, "'21072009',
*  ' '  'BKPF-MONAT' '7',
  ' '  'BKPF-WAERS' 'BRL',
  ' '  'BKPF-XBLNR' lv_dt_apur,                             "'04/2009',
  ' '  'RF05A-NEWBS' '31',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-lifnr,              "'100210',
  ' '  'BDC_SUBSCR' 'SAPMF05A', "'1300APPL_SUB_T',
  ' '  'BDC_SUBSCR' 'SAPLSEXM'.


  PERFORM insert_line USING:
  'X'  'SAPMF05A' '0302',
  ' '  'BDC_CURSOR' 'BSEG-SGTXT',
  ' '  'BDC_OKCODE' '=ZK',
  ' '  'BSEG-WRBTR' lv_wrbtr,                               "'100,00',
  ' '  'BSEG-GSBER' <f_zimp_detalhe>-gsber,                 "'0101',
*  ' '  'BSEG-ZTERM' 'Z001',
  ' '  'BSEG-ZTERM' '0004',
*  ' '  'BSEG-ZBD1T' '2',
  ' '  'BSEG-ZBD1T' '0',
  ' '  'BSEG-ZFBDT' lv_zfbdt, "'30072009',
  ' '  'BSEG-ZLSCH'	'P',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt. "'PRINCIPAL'.



PERFORM insert_line USING:
  'X'  'SAPMF05A' '0332',
  ' '  'BDC_CURSOR' 'RF05A-NEWKO',
  ' '	 'BDC_OKCODE'	'/00',
*   ' '  'BSEG-DMBE2' '81,25',                                "'62,50',
*  ' '  'BSEG-DMBE3'  '106,81',                               "'82,16',
  ' '  'BSEG-HBKID'	'BBD',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_imposto. "lifnr. "'213600'.

  v_soma = <f_zimp_detalhe>-vlr_principal + <f_zimp_detalhe>-vlr_outras_ent.


  WRITE v_soma TO lv_wrbtr.





  PERFORM insert_line USING:
  'X'  'SAPMF05A'    '300',
  ' '  'BDC_CURSOR'  'BSEG-SGTXT',
  ' '  'BDC_OKCODE'  '=BS',
  ' '  'BSEG-WRBTR'  lv_wrbtr,                              "'100,00',
  ' '  'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,                "'0101',
  ' '  'BSEG-ZUONR'  <f_zimp_detalhe>-cod_ident,            "'REG_02',
  ' '  'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'  'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'  <f_zimp_detalhe>-gsber,                "'0101',
  ' '  'RF05A-NEWBS' '40',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_tse. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-tse TO lv_wrbtr.




  PERFORM insert_line USING:
  'X'  'SAPMF05A'    '300',
  ' '  'BDC_CURSOR'  'BSEG-SGTXT',
  ' '  'BDC_OKCODE'  '=BS',
  ' '  'BSEG-WRBTR'  lv_wrbtr,                              "'100,00',
  ' '  'BSEG-BUPLA'  <f_zimp_detalhe>-gsber,                "'0101',
  ' '  'BSEG-ZUONR'  <f_zimp_detalhe>-cod_ident,            "'REG_02',
  ' '  'BSEG-SGTXT'  <f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'  'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'  <f_zimp_detalhe>-gsber,                "'0101',
  ' '  'RF05A-NEWBS' '40',
  ' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_at_mon. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_atual_mone TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'RF05A-NEWBS' '40',
 	' '  'RF05A-NEWKO' <f_zimp_cabecalho>-cta_juros. "lifnr. "'213600'.

  WRITE <f_zimp_detalhe>-vlr_juros TO lv_wrbtr.
  PERFORM insert_line USING:
  'X'  'SAPMF05A'  	'300',
  ' '  'BDC_CURSOR'	'BSEG-SGTXT',
  ' '  'BDC_OKCODE'	'=BS',
  ' '  'BSEG-WRBTR'	lv_wrbtr,                               "'100,00',
  ' '  'BSEG-BUPLA'	<f_zimp_detalhe>-gsber,                 "'0101',
  ' '  'BSEG-ZUONR'	<f_zimp_detalhe>-cod_ident,             "'REG_02',
  ' '  'BSEG-SGTXT'	<f_zimp_detalhe>-sgtxt, "'PRINCIPAL',
  ' '  'BDC_SUBSCR'	'SAPLKACB', "'1004BLOCK',
  ' '  'COBL-GSBER'	<f_zimp_detalhe>-gsber.                 "'0101',

  PERFORM insert_line USING:
  'X'  'SAPMF05A'   '700',
  ' '  'BDC_CURSOR'	'RF05A-NEWBS',
  ' '  'BDC_OKCODE'	'=BU',
  ' '  'BKPF-XBLNR'	lv_dt_apur.                             "'04/2009'.


ENDFORM.                    " F_SHDB04_NOVO03
