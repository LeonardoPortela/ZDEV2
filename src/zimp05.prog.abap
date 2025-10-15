************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 09.05.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Relatório ALV de Lançamento de Tributos a Pagar     *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 09.05.2009    Desenvolvedor ABAP   Criação              DEVK905828   *
* 20.08.2009    Rodrigo - Rollout    Modif.Layout         DEVK906334   *
************************************************************************

REPORT  zimp05.

*-----------------------------------------------------------------------
* Declaração para SELECT-OPTIONS
*-----------------------------------------------------------------------
TABLES:
  b120, d020s.

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
    tp_arrec         TYPE zimp_cabecalho-tp_arrec,
    desc_tp_arrec    TYPE zimp_tipos_impos-arrecadacao,
    nro_doc_tr       TYPE zimp_detalhe-nro_doc_tr,
    zfbdt            TYPE zimp_cabecalho-zfbdt,
    vlr_principal    TYPE zimp_detalhe-vlr_principal,
    vlr_multa        TYPE zimp_detalhe-vlr_multa,
    vlr_juros        TYPE zimp_detalhe-vlr_juros,
* Início Alteração Ricardo Furst 20.07.2009
    vlr_mon          TYPE zimp_detalhe-vlr_atual_mone,
    tse              TYPE zimp_detalhe-tse,
    usuario          TYPE zimp_detalhe-usuario_apr,
* Fim Alteração Ricardo Furst 20.07.2009
    vlr_total        TYPE zimp_detalhe-vlr_principal,
    gsber            TYPE zimp_detalhe-gsber,
    lifnr            TYPE zimp_cabecalho-lifnr,
    sgtxt            TYPE zimp_detalhe-sgtxt,
    mes_ano_comp     TYPE zimp_cabecalho-mes_ano_comp,
    cod_pgto         TYPE zimp_cabecalho-cod_pgto,
    forn_retencao    TYPE zimp_detalhe-lifnr,
    cod_ident        TYPE zimp_detalhe-cod_ident,
    cta_multa        TYPE zimp_cabecalho-cta_multa,
    cta_juros        TYPE zimp_cabecalho-cta_juros,
    cod_barras       TYPE zimp_detalhe-cod_barras,
    belnr            TYPE zimp_cabecalho-belnr,
    estorno          TYPE zimp_cabecalho-estorno,
vlr_outras_ent TYPE zimp_detalhe-vlr_outras_ent,
  END OF ty_relatorio.

*-----------------------------------------------------------------------
* Tabelas internas
*-----------------------------------------------------------------------
DATA:

  t_zimp_cabecalho   TYPE TABLE OF zimp_cabecalho,
  t_zimp_detalhe     TYPE TABLE OF zimp_detalhe,
  t_relatorio        TYPE TABLE OF ty_relatorio,
  t_fieldcat         TYPE slis_t_fieldcat_alv,
  t_listheader       TYPE slis_t_listheader.

*-----------------------------------------------------------------------
* Estruturas
*-----------------------------------------------------------------------
DATA:

  w_relatorio        TYPE ty_relatorio,
  w_fieldcat         TYPE slis_fieldcat_alv,
  w_layout           TYPE slis_layout_alv,
  w_listheader       TYPE slis_listheader.
DATA: gx_variant         LIKE disvariant.
DATA: g_variant          LIKE disvariant.
DATA: g_variant_save.
DATA: g_repid            LIKE sy-repid.

*-----------------------------------------------------------------------
* Símbolos de campo
*-----------------------------------------------------------------------
FIELD-SYMBOLS:

  <f_zimp_cabecalho> TYPE zimp_cabecalho,
  <f_zimp_detalhe>   TYPE zimp_detalhe,
  <f_relatorio>      TYPE ty_relatorio.

*-----------------------------------------------------------------------
* Constantes
*-----------------------------------------------------------------------
CONSTANTS:

  c_s                TYPE c VALUE 'S',
  c_e                TYPE c VALUE 'E',
  c_x                TYPE c VALUE 'X'.

*-----------------------------------------------------------------------
* Parâmetros de seleção
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-t01.

SELECT-OPTIONS:
  s_nr_doc           FOR <f_zimp_detalhe>-nro_doc_tr,

  s_emp              FOR b120-bukrs        OBLIGATORY,

  s_filial           FOR b120-j_1bbranch,

  s_dtlanc           FOR <f_zimp_cabecalho>-budat
                     OBLIGATORY,

  s_dtvenc           FOR <f_zimp_cabecalho>-zfbdt,
  s_codpag           FOR <f_zimp_cabecalho>-cod_pgto,
  s_tparrc           FOR <f_zimp_cabecalho>-tp_arrec
                     NO INTERVALS.

PARAMETERS:
  p_aprov            TYPE zimp_detalhe-aprovador AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-t02.

PARAMETERS:
p_var              LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b02.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  PERFORM variant_f4 USING p_var.
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  PERFORM variant_fill.

*---------------------------------------------------------------------*
*       MAIN                                                          *
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init_variant.
  PERFORM variant_default USING p_var.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Busca dados nas tabelas e verifica a consistência
  PERFORM zf_busca_dados.

* Monta relatório
  PERFORM zf_monta_relatorio.

* Configura e exibe relatório
  PERFORM zf_exibe_relatorio.

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
*IF p_aprov IS INITIAL.
* Seleciona cabeçalho dos documentos
  SELECT *
    INTO TABLE t_zimp_cabecalho
    FROM zimp_cabecalho
    WHERE bukrs      IN s_emp    AND
          nro_doc_tr IN s_nr_doc AND
          budat      IN s_dtlanc AND
          zfbdt      IN s_dtvenc AND
          cod_pgto   IN s_codpag AND
          tp_arrec   IN s_tparrc.
*ELSE.
*  SELECT *
*    INTO TABLE t_zimp_cabecalho
*    FROM zimp_cabecalho
*    WHERE bukrs       = p_emp    AND
*          nro_doc_tr IN s_nr_doc AND
*          budat      IN s_dtlanc AND
*          zfbdt      IN s_dtvenc AND
*          cod_pgto   IN s_codpag AND
*          tp_arrec   IN s_tparrc AND
*          impostos   =  p_aprov.
*
*ENDIF.
  IF sy-subrc = 0.

*   Se foi apenas selecionado um documento, verifica se já não foi
*   criado seu respectivo documento FI-AP
    IF sy-dbcnt = 1.

      READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho>
        INDEX 1.

      IF NOT <f_zimp_cabecalho>-belnr IS INITIAL.
* Início Alteração Ricardo Furst 13.07.2009
*        MESSAGE text-e02 TYPE c_s DISPLAY LIKE c_e.
*        LEAVE LIST-PROCESSING.
* Fim Alteração Ricardo Furst 13.07.2009
      ENDIF.

    ENDIF.

    SORT t_zimp_cabecalho BY nro_doc_tr gjahr.
** Modificação - Gustavo M. Rollout 01.06.09
*   Seleciona itens dos documentos
    IF NOT p_aprov IS INITIAL.
      SELECT *
        INTO TABLE t_zimp_detalhe
        FROM zimp_detalhe
        WHERE bukrs      IN s_emp    AND
              nro_doc_tr IN s_nr_doc

         AND aprovador = p_aprov.

    ELSE.
      SELECT *
      INTO TABLE t_zimp_detalhe
      FROM zimp_detalhe
      WHERE bukrs      IN s_emp    AND
            nro_doc_tr IN s_nr_doc.
    ENDIF.
** Modificação - Gustavo M. Rollout 01.06.09
    IF sy-subrc = 0.

*     Efetua o filtro por filial e elimina já aprovados
      IF NOT p_aprov IS INITIAL.
        DELETE t_zimp_detalhe WHERE NOT gsber     IN s_filial OR
                                    NOT aprovador  = p_aprov.
      ELSE.
        DELETE t_zimp_detalhe WHERE NOT gsber     IN s_filial." OR
*                                  NOT aprovador  = p_aprov.
      ENDIF.
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
* Início Alteração Ricardo Furst 20.07.2009
      w_relatorio-vlr_mon       = <f_zimp_detalhe>-vlr_atual_mone.
      w_relatorio-tse           = <f_zimp_detalhe>-tse.
      w_relatorio-vlr_outras_ent = <f_zimp_detalhe>-vlr_outras_ent.
* Fim Alteração Ricardo Furst 20.07.2009
      w_relatorio-vlr_total     = w_relatorio-vlr_principal  +
                                  w_relatorio-vlr_multa      +
                                  w_relatorio-vlr_juros      +
                                  w_relatorio-vlr_outras_ent +
* Início Alteração Ricardo Furst 20.07.2009
                                  w_relatorio-vlr_mon        +
                                  w_relatorio-tse.
      w_relatorio-usuario       = <f_zimp_detalhe>-usuario_apr.
* Fim Alteração Ricardo Furst 20.07.2009
      w_relatorio-sgtxt         = <f_zimp_detalhe>-sgtxt.
      w_relatorio-gsber         = <f_zimp_detalhe>-gsber.
      w_relatorio-lifnr         = <f_zimp_cabecalho>-lifnr.
      w_relatorio-mes_ano_comp  = <f_zimp_cabecalho>-mes_ano_comp.
      w_relatorio-cod_pgto      = <f_zimp_cabecalho>-cod_pgto.
      w_relatorio-forn_retencao = <f_zimp_detalhe>-lifnr.
      w_relatorio-lifnr         = <f_zimp_cabecalho>-lifnr.
      w_relatorio-cod_ident     = <f_zimp_detalhe>-cod_ident.
      w_relatorio-cta_multa     = <f_zimp_cabecalho>-cta_multa.
      w_relatorio-cta_juros     = <f_zimp_cabecalho>-cta_juros.
      w_relatorio-cod_barras    = <f_zimp_detalhe>-cod_barras.
      w_relatorio-estorno       = <f_zimp_cabecalho>-estorno.
      w_relatorio-belnr         = <f_zimp_cabecalho>-belnr.

      SELECT SINGLE arrecadacao
        INTO w_relatorio-desc_tp_arrec
        FROM zimp_tipos_impos
        WHERE tp_arrec = w_relatorio-tp_arrec.

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
    'TP_ARREC'      'Tipo de Imposto'(001),
    'DESC_TP_ARREC' 'Impostos'(020),
    'MES_ANO_COMP'  'Mês/Ano Comp.'(015),
    'COD_PGTO'      'Código Pgto.'(016),
    'FORN_RETENCAO' 'Forn. retenção'(017),
    'COD_IDENT'     'Identificação'(018),
    'VLR_PRINCIPAL' 'Vlr. Principal'(005),
    'VLR_MULTA'     'Vlr. Multa'(006),
    'VLR_JUROS'     'Vlr. Juros'(007),
* Início Alteração Ricardo Furst 20.07.2009
    'VLR_MON'       'Vlr. At. Monet'(023),
    'TSE'           'Vlr. TSE'(024),
* Eduardo
   'VLR_OUTRAS_ENT'  'Outras Entidades'(099),
* Eduardo
* Fim Alteração Ricardo Furst 20.07.2009
    'VLR_TOTAL'     'Vlr. Total'(010),
* Início Alteração Ricardo Furst 20.07.2009
    'USUARIO'       'Usuario'(025),
* Fim Alteração Ricardo Furst 20.07.2009
    'SGTXT'         'Texto'(008),
    'COD_BARRAS'    'Cód. barras'(019),
    'BELNR'         'Doc.Contábil'(021),
    'ESTORNO'       'Doc.Estornado'(022).

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'ZF_ALV_TOP_OF_PAGE'
      is_layout              = w_layout
      i_save                 = 'A'
      is_variant             = g_variant
      it_fieldcat            = t_fieldcat
    TABLES
      t_outtab               = t_relatorio
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

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
  IF p_field = 'VLR_PRINCIPAL'  OR
     p_field = 'VLR_MULTA'      OR
     p_field = 'VLR_JUROS'      OR
     p_field = 'VLR_MON'        OR
     p_field = 'TSE'            OR
     p_field = 'VLR_OUTRAS_ENT' OR
     p_field = 'VLR_TOTAL'.
    w_fieldcat-do_sum = 'X'.
  ELSE.
    CLEAR w_fieldcat-do_sum.
  ENDIF.
  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ZF_FIELDCAT

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
    READ TABLE s_emp INDEX 1.
    w_listheader-typ  = 'S'.
    w_listheader-key  = 'Empresa'(009).
    w_listheader-info = s_emp-low.

    APPEND w_listheader TO t_listheader.

    WRITE s_dtlanc-low TO lv_dtlanc.

    CONCATENATE lv_dtlanc 'até' INTO w_listheader-info
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
*&      Form  VARIANT_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VARIANT  text
*----------------------------------------------------------------------*
FORM variant_f4 USING    p_variant.

  DATA h_exit.

  CLEAR gx_variant.

*  CLEAR T_DYNPREAD[].
*  CLEAR T_DYNPREAD.
*  T_DYNPREAD-FIELDNAME = 'SAVE_X'.
*  APPEND T_DYNPREAD.
*  CLEAR T_DYNPREAD.
*  T_DYNPREAD-FIELDNAME = 'SAVE_A'.
*  APPEND T_DYNPREAD.
*  CLEAR T_DYNPREAD.
*  T_DYNPREAD-FIELDNAME = 'SAVE_N'.
*  APPEND T_DYNPREAD.
*  CLEAR T_DYNPREAD.
*  T_DYNPREAD-FIELDNAME = 'SAVE_U'.
*  APPEND T_DYNPREAD.
*  CALL FUNCTION 'DYNP_VALUES_READ'
*       EXPORTING
*            DYNAME               = D020S-PROG
*            DYNUMB               = D020S-DNUM
*            TRANSLATE_TO_UPPER   = 'X'
**           REQUEST              = ' '
*       TABLES
*            DYNPFIELDS           = T_DYNPREAD.
**
*  LOOP AT T_DYNPREAD.
*    CASE T_DYNPREAD-FIELDNAME.
*      WHEN 'SAVE_A'.
*        MOVE T_DYNPREAD-FIELDVALUE TO SAVE_A.
*      WHEN 'SAVE_X'.
*        MOVE T_DYNPREAD-FIELDVALUE TO SAVE_X.
*      WHEN 'SAVE_N'.
*        MOVE T_DYNPREAD-FIELDVALUE TO SAVE_N.
*      WHEN 'SAVE_U'.
*        MOVE T_DYNPREAD-FIELDVALUE TO SAVE_U.
*      WHEN OTHERS.
*        MESSAGE X000(0K) WITH TEXT-102.
*    ENDCASE.
*  ENDLOOP.
*
*  PERFORM READ_BUTTONS.

  CALL FUNCTION 'LVC_VARIANT_F4'
       EXPORTING
            is_variant          = g_variant
*           IT_DEFAULT_FIELDCAT =
            i_save              = g_variant_save
       IMPORTING
            e_exit              = h_exit
            es_variant          = gx_variant
       EXCEPTIONS
            not_found           = 1
            program_error       = 2
            OTHERS              = 3
            .
  IF sy-subrc <> 0.
    MESSAGE i000(0k) WITH text-101.
  ENDIF.

  IF h_exit IS INITIAL.
    g_variant-variant = gx_variant-variant.
    p_variant         = gx_variant-variant.
  ENDIF.
ENDFORM.                               " VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  variant_default
*&---------------------------------------------------------------------*
FORM variant_default USING p_variant LIKE g_variant-variant.

  gx_variant = g_variant.

  IF NOT p_variant IS INITIAL.
    gx_variant-variant = p_variant.
  ENDIF.

  CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = g_variant_save
    CHANGING
      cs_variant    = gx_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  CASE sy-subrc.
    WHEN 0.
      p_variant = gx_variant-variant.
    WHEN 2.
      CLEAR p_variant.
  ENDCASE.

ENDFORM.                               " variant_default
*&---------------------------------------------------------------------*
*&      Form  init_variant
*&---------------------------------------------------------------------*
FORM init_variant.

  CLEAR g_variant.
  g_repid              = sy-repid.
  d020s-prog           = g_repid.
  d020s-dnum           = sy-dynnr.
  g_variant-report     = g_repid.
  g_variant-username   = sy-uname.

ENDFORM.                               " init_variant

*&---------------------------------------------------------------------*
*&      Form  variant_fill
*&---------------------------------------------------------------------*
FORM variant_fill.

  PERFORM read_buttons.

  CHECK NOT p_var IS INITIAL.
  CLEAR g_variant.
  MOVE p_var TO g_variant-variant.
  MOVE g_repid TO g_variant-report.
  CALL FUNCTION 'LVC_VARIANT_EXISTENCE_CHECK'
    EXPORTING
      i_save     = g_variant_save
    CHANGING
      cs_variant = g_variant
    EXCEPTIONS
      OTHERS     = 01.
  IF sy-subrc NE 0.
    MESSAGE i000(0k) WITH text-104.
  ENDIF.

ENDFORM.                               " variant_fill

*&---------------------------------------------------------------------*
*&      Form  read_buttons
*&---------------------------------------------------------------------*
FORM read_buttons.

*  IF SAVE_A = 'X'.
*    G_VARIANT_SAVE = 'A'.
*  ELSEIF SAVE_X = 'X'.
*    G_VARIANT_SAVE = 'X'.
*  ELSEIF SAVE_U = 'X'.
*    G_VARIANT_SAVE = 'U'.
*  ELSEIF SAVE_N = 'X'.
*    G_VARIANT_SAVE = SPACE.
*  ENDIF.

ENDFORM.                               " read_buttons
