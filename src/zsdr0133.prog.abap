*&---------------------------------------------------------------------*
*& Report  ZSDR0133                                                    *&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Realização de Baixa de Volume de NF de compra           *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT zsdr0133.

*******************************************************************************************
* Tabelas
*******************************************************************************************
TABLES: j_1bnfdoc,
        j_1bnflin,
        zsdt0276.

*******************************************************************************************
* Types
*******************************************************************************************
TYPES: BEGIN OF ty_j_1bnfdoc,
         docnum	   TYPE j_1bnflin-docnum,
         itmnum	   TYPE j_1bnflin-itmnum,
         credat    TYPE j_1bnfdoc-credat,
         docdat    TYPE j_1bnfdoc-docdat,
         model     TYPE j_1bnfdoc-model,
         series	   TYPE j_1bnfdoc-series,
         nfnum     TYPE j_1bnfdoc-nfnum,
         bukrs 	   TYPE j_1bnfdoc-bukrs,
         branch	   TYPE j_1bnfdoc-branch,
         parvw     TYPE j_1bnfdoc-parvw,
         parid     TYPE j_1bnfdoc-parid,
         partyp	   TYPE j_1bnfdoc-partyp,
         nfenum	   TYPE j_1bnfdoc-nfenum,
         matnr     TYPE j_1bnflin-matnr,
         matkl     TYPE j_1bnflin-matkl,
         charg     TYPE j_1bnflin-charg,
         cfop	     TYPE j_1bnflin-cfop,
         nbm       TYPE j_1bnflin-nbm,
         menge     TYPE j_1bnflin-menge,
         meins     TYPE j_1bnflin-meins,
         nfe       TYPE j_1bnfdoc-nfe,
         entrad    TYPE j_1bnfdoc-entrad,
         parid_b   TYPE j_1bnfdoc-parid,
         chave_nfe TYPE zib_nfe_dist_itm-chave_nfe.
TYPES: END OF ty_j_1bnfdoc.

TYPES: BEGIN OF ty_saida_0100,
         icone                  TYPE c LENGTH 4,
         docnum	                TYPE j_1bnflin-docnum,
         itmnum	                TYPE j_1bnflin-itmnum,
         credat                 TYPE j_1bnfdoc-credat,
         docdat                 TYPE j_1bnfdoc-docdat,
         model                  TYPE j_1bnfdoc-model,
         series	                TYPE j_1bnfdoc-series,
         nfnum                  TYPE j_1bnfdoc-nfnum,
         bukrs 	                TYPE j_1bnfdoc-bukrs,
         branch	                TYPE j_1bnfdoc-branch,
         parid                  TYPE j_1bnfdoc-parid,
         nfenum	                TYPE j_1bnfdoc-nfenum,
         matnr                  TYPE j_1bnflin-matnr,
         cfop	                  TYPE j_1bnflin-cfop,
         nbm                    TYPE j_1bnflin-nbm,
         menge                  TYPE j_1bnflin-menge,
         meins                  TYPE j_1bnflin-meins,
         entrad                 TYPE j_1bnfdoc-entrad,
         name1                  TYPE lfa1-name1,
         regio                  TYPE lfa1-regio,
         maktx                  TYPE makt-maktx,
         dt_recepcao_cct        TYPE zlest0146-dt_recepcao,
         peso_aferido_recepcao  TYPE zlest0146-peso_aferido_recepcao,
         peso_cct               TYPE zlest0146-peso_aferido_recepcao,
         peso_fiscal_cct        TYPE zlest0146-peso_aferido_recepcao,
         term_cct               TYPE zsdt0168-lifnr,
         ds_term_cct            TYPE lfa1-name1,
         dif_peso_cct_nf        TYPE zlest0146-peso_aferido_recepcao,
         conf_cct_portal        TYPE c LENGTH 4,
         dt_recepcao_portal     TYPE zlest0186-dt_recepcao,
         term_cct_portal        TYPE zsdt0168-lifnr,
         ds_term_cct_portal     TYPE lfa1-name1,
         rom_completo           TYPE c LENGTH 4,
         tp_nf_rem              TYPE znom_reme_notas-tp_nf_rem,
         docnum_rfl             TYPE j_1bnfdoc-docnum,
         nfenum_rfl             TYPE j_1bnfdoc-nfenum,
         cct_rfl                TYPE c LENGTH 4,
         peso_cct_rfl           TYPE zlest0146-peso_aferido_recepcao,
         dt_cct_rfl             TYPE zlest0146-dt_recepcao,
         term_cct_rfl           TYPE zsdt0168-lifnr,
         ds_term_cct_rfl        TYPE lfa1-name1,
         terminal_rfl           TYPE lfa1-lifnr,
         ds_terminal_rfl        TYPE lfa1-name1,
         chave_nfe_rfl          TYPE zib_nfe_dist_itm-chave_nfe,
         conf_cct_portal_rfl    TYPE c LENGTH 4,
         dt_recepcao_portal_rfl TYPE zlest0186-dt_recepcao,
         term_cct_portal_rfl    TYPE zsdt0168-lifnr,
         ds_term_cct_portal_rfl TYPE lfa1-name1,
         ent_prop               TYPE c LENGTH 4,
         id_due                 TYPE zsdt0170-id_due,
         numero_due             TYPE zsdt0170-numero_due,
         chave_acesso           TYPE zsdt0170-chave_acesso,
         dt_due                 TYPE zsdt0170-dt_registro,
         qtde_vinc_due          TYPE zsdt0173-peso_liq_total,
         valor_pago             TYPE zsdt0276-valor,
         qtde_baixar            TYPE zsdt0276-menge,
         id_due_ret             TYPE zsdt0170-id_due,
         docnum_exp             TYPE j_1bnfdoc-docnum,
         nfenum_exp             TYPE j_1bnfdoc-nfenum,
         fatura_id              TYPE zsdt0172-fatura_id,
         navio                  TYPE znom_transporte-ds_nome_transpor,
         saldo_exportar         TYPE zsdt0173-peso_liq_total,
         chave_nfe              TYPE zib_nfe_dist_itm-chave_nfe,
         und_trib_xml           TYPE zib_nfe_dist_itm-prod_und_trib,
         ncm_xml                TYPE zib_nfe_dist_itm-prod_ncm,
         cfop_xml               TYPE zib_nfe_dist_itm-prod_cfop,
         restricao              TYPE c LENGTH 150,
         color                  TYPE kkblo_specialcol OCCURS 0.
TYPES: END OF ty_saida_0100.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

*******************************************************************************************
*  Inicio Definition Classes
*******************************************************************************************
CLASS lcl_alv_toolbar_0100 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_event_handler_0100 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed4 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

ENDCLASS.

*******************************************************************************************
* variaveis
*******************************************************************************************
DATA: t_j_1bnfdoc           TYPE TABLE OF ty_j_1bnfdoc,
      t_j_1bnfdoc_aux       TYPE TABLE OF ty_j_1bnfdoc,
      t_j_1bnfe_active      TYPE TABLE OF j_1bnfe_active,
      t_lfa1                TYPE TABLE OF lfa1,
      t_kna1                TYPE TABLE OF kna1,
      t_makt                TYPE TABLE OF makt,
      t_zsdt0276            TYPE TABLE OF zsdt0276,
      t_documentos          TYPE TABLE OF zsdt0276,
      w_documentos          TYPE zsdt0276,
      t_znom_reme_notas     TYPE TABLE OF znom_reme_notas,
      t_nf_produtor         TYPE TABLE OF zdoc_nf_produtor,
      t_nf_produtor_algodao TYPE TABLE OF zdoc_nf_produtor,
      t_saida_0100          TYPE TABLE OF ty_saida_0100,
      w_saida_0100          TYPE ty_saida_0100,
      l_erro                TYPE c,
      r_cfops               TYPE TABLE OF lxhme_range_c10.

RANGES:
      r_model                FOR j_1bnfdoc-model.

DATA: obj_alv_0100         TYPE REF TO cl_gui_alv_grid,
      obj_container_0100   TYPE REF TO cl_gui_custom_container,
      gt_catalog           TYPE lvc_t_fcat,
      gw_catalog           TYPE lvc_s_fcat,
      obj_toolbar_0100     TYPE REF TO lcl_alv_toolbar_0100,
*---------------------------
*---- ALV field catalogs
*---------------------------
      it_fcat              TYPE lvc_t_fcat,
      wa_fcat              TYPE lvc_s_fcat,
*---------------------------
* ALV excluded functions
*---------------------------
      it_exclude_fcode     TYPE ui_functions,
      wa_exclude_fcode     LIKE LINE OF it_exclude_fcode,
*---------------------------
* Alv Styles
*---------------------------
      ls_edit              TYPE lvc_s_styl,
      lt_edit              TYPE lvc_t_styl,
*---------------------------
* ALV layout variant
*---------------------------
      gs_variant           TYPE disvariant,
*---------------------------
* ALV layout
*---------------------------
      gs_layout            TYPE lvc_s_layo,
*---------------------------
* ALV Stable
*---------------------------
      wa_stable            TYPE lvc_s_stbl,

      it_selectedcell      TYPE lvc_t_cell,
      wa_selectedcell      TYPE lvc_s_cell,

      it_sel_rows          TYPE lvc_t_row,
      wa_sel_rows          TYPE lvc_s_row,

      gt_estilo            TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo            TYPE lvc_s_styl,

      gt_f4                TYPE lvc_t_f4 WITH HEADER LINE,
*---------------------------
* Objetos
*---------------------------
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button,
      wa_estrutura         TYPE ty_estrutura,
      estrutura            TYPE TABLE OF ty_estrutura.

*******************************************************************************************
* Tela selecao
*******************************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs      FOR j_1bnfdoc-bukrs   NO INTERVALS NO-EXTENSION,
                s_branch     FOR j_1bnfdoc-branch,
                s_docnum     FOR j_1bnfdoc-docnum  NO INTERVALS,
                s_docdat     FOR j_1bnfdoc-docdat,
                s_lifnr      FOR j_1bnfdoc-parid   NO INTERVALS,
                s_matnr      FOR j_1bnflin-matnr   NO INTERVALS.
SELECTION-SCREEN: END   OF BLOCK b2.

*******************************************************************************************
* inicio
*******************************************************************************************
START-OF-SELECTION.

*-Selecao dados
  PERFORM f_selecionar_dados.

  IF t_j_1bnfdoc[] IS INITIAL.
    MESSAGE s024(sd) WITH text-010.
    STOP.
  ENDIF.

*-Selecao dados
  PERFORM f_processa_dados.

  IF t_saida_0100[] IS INITIAL.
    MESSAGE s024(sd) WITH text-010.
    STOP.
  ENDIF.

*-ALV
  CALL SCREEN 0100.

*******************************************************************************************
* seleciona dados
*******************************************************************************************
FORM f_selecionar_dados.

*---------------------------------------
*-monta range
*---------------------------------------
  r_model-sign   = 'I'.
  r_model-option = 'EQ'.
  r_model-low    = '55'.
  APPEND r_model.
  r_model-low    = '01'.
  APPEND r_model.
  r_model-low    = '04'.
  APPEND r_model.

  CALL FUNCTION 'Z_MEMO_CFOP_ENTRADAS'
    TABLES
      cfops = r_cfops.

*---------------------------------------
*-selecao j_1bnfdoc
*---------------------------------------
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE t_j_1bnfdoc
    FROM j_1bnfdoc AS dc
   INNER JOIN j_1bnflin AS li ON li~docnum EQ dc~docnum
   WHERE dc~bukrs IN s_bukrs
     AND dc~branch IN s_branch
     AND dc~docdat IN s_docdat
     AND dc~direct EQ '1'
     AND dc~model  IN r_model
     AND dc~cancel EQ abap_false
     AND dc~doctyp NE '5'
     AND dc~docnum IN s_docnum
     AND dc~parid  IN s_lifnr
     AND li~cfop   IN r_cfops
     AND li~matnr  IN s_matnr.

  CHECK t_j_1bnfdoc[] IS NOT INITIAL.

  PERFORM: f_get_j_1bnfe_active,
           f_set_chave_doc,
           f_get_lfa1,
           f_get_kna1,
           f_get_j_1bbranch,
           f_get_makt,
           f_get_due,
           f_get_nfe_exportacao,
           f_get_nfe_exportacao_algodao,
           f_get_baixa.

ENDFORM.

*******************************************************************************************
* processa dados
*******************************************************************************************
FORM f_processa_dados.

  DATA: v_menge            TYPE ekpo-menge,
        v_rom_completo     TYPE char01,
        v_cct_cp           TYPE char01,
        wl_zlest0146_cp    TYPE zlest0146,
        v_count_dues       TYPE i,
        v_count_nf_prod    TYPE i,
        v_count_saida_nf   TYPE i,
        v_count_saida_doc  TYPE i,
        t_zsdt0001_ro_vinc TYPE zsdt0001_ro_vinc_t,
        w_zlest0146        TYPE zlest0146,
        t_zlest0147        TYPE zlest0147_t,
        v_doc_rateio       TYPE char01,
        ls_acckey          TYPE j_1b_nfe_access_key,
        w_nf_produtor      LIKE LINE OF t_nf_produtor.

  SORT: t_j_1bnfdoc BY docnum,
        t_lfa1      BY lifnr,
        t_kna1      BY kunnr.

  FREE: t_saida_0100.

  LOOP AT t_j_1bnfdoc INTO DATA(w_j_1bnfdoc).

    CLEAR: w_saida_0100.

    w_saida_0100-bukrs      = w_j_1bnfdoc-bukrs.
    w_saida_0100-branch     = w_j_1bnfdoc-branch.
    w_saida_0100-docnum     = w_j_1bnfdoc-docnum.
    w_saida_0100-itmnum     = w_j_1bnfdoc-itmnum.
    w_saida_0100-docdat     = w_j_1bnfdoc-docdat.
    w_saida_0100-credat     = w_j_1bnfdoc-credat.
    w_saida_0100-entrad     = w_j_1bnfdoc-entrad.
    w_saida_0100-chave_nfe  = w_j_1bnfdoc-chave_nfe.

    IF w_j_1bnfdoc-nfe IS NOT INITIAL.
      w_saida_0100-nfenum  = w_j_1bnfdoc-nfenum.
    ELSE.
      w_saida_0100-nfenum  = w_j_1bnfdoc-nfnum.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_saida_0100-nfenum
        IMPORTING
          output = w_saida_0100-nfenum.
    ENDIF.

    w_saida_0100-model   = w_j_1bnfdoc-model.
    w_saida_0100-series  = w_j_1bnfdoc-series.

    "Processa Cliente/Fonecedor/Local de Negócio
    CASE w_j_1bnfdoc-partyp.
      WHEN 'V'.
        READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY lifnr = w_j_1bnfdoc-parid BINARY SEARCH.
        IF sy-subrc EQ 0.
          w_saida_0100-parid = w_lfa1-lifnr.
          w_saida_0100-name1 = w_lfa1-name1.
          w_saida_0100-regio = w_lfa1-regio.
        ENDIF.
      WHEN 'C'.
        READ TABLE t_kna1 INTO DATA(w_kna1) WITH KEY kunnr = w_j_1bnfdoc-parid BINARY SEARCH.
        IF sy-subrc EQ 0.
          w_saida_0100-parid = w_kna1-kunnr.
          w_saida_0100-name1 = w_kna1-name1.
          w_saida_0100-regio = w_kna1-regio.
        ENDIF.
      WHEN 'B'.
        READ TABLE t_lfa1 INTO w_lfa1 WITH KEY lifnr = w_j_1bnfdoc-parid_b BINARY SEARCH.
        IF sy-subrc EQ 0.
          w_saida_0100-parid = w_lfa1-lifnr.
          w_saida_0100-name1 = w_lfa1-name1.
          w_saida_0100-regio = w_lfa1-regio.
        ENDIF.
    ENDCASE.

    w_saida_0100-matnr   = w_j_1bnfdoc-matnr.
    READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_j_1bnfdoc-matnr.
    IF sy-subrc EQ 0.
      w_saida_0100-maktx = w_makt-maktx.
    ENDIF.

    w_saida_0100-cfop    = w_j_1bnfdoc-cfop.
    w_saida_0100-nbm     = w_j_1bnfdoc-nbm.
    w_saida_0100-meins   = w_j_1bnfdoc-meins.

    IF w_j_1bnfdoc-meins NE 'KG'.
      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = w_j_1bnfdoc-matnr
          i_mein1             = w_j_1bnfdoc-meins
          i_meins             = 'KG'
          i_menge             = w_j_1bnfdoc-menge
        IMPORTING
          menge               = v_menge
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        w_saida_0100-menge = v_menge.
      ENDIF.
    ELSE.
      w_saida_0100-menge = w_j_1bnfdoc-menge.
    ENDIF.

*-------------------------------------------------------------------------------*
*   Dados CCT
*-------------------------------------------------------------------------------*
    CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
      EXPORTING
        i_docnum     = w_saida_0100-docnum
      IMPORTING
        e_zlest0146  = w_zlest0146
        e_zlest0147  = t_zlest0147
        e_doc_rateio = v_doc_rateio.

    IF ( w_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
      w_saida_0100-peso_aferido_recepcao = w_zlest0146-peso_aferido_recepcao.

      READ TABLE t_zlest0147 INTO DATA(w_0147) INDEX 1.
      IF sy-subrc EQ 0.
        w_saida_0100-peso_fiscal_cct = w_0147-peso_fiscal.

        IF w_0147-complemento EQ abap_true.
          w_saida_0100-peso_cct = w_0147-peso_fiscal.
        ELSE.
          w_saida_0100-peso_cct = w_zlest0146-peso_aferido_recepcao.
        ENDIF.
      ENDIF.
    ENDIF.

    w_saida_0100-dif_peso_cct_nf  = w_saida_0100-menge - w_saida_0100-peso_cct.
    w_saida_0100-saldo_exportar   = w_saida_0100-menge.

    IF w_j_1bnfdoc-matkl EQ '700140'."algodao
      LOOP AT t_nf_produtor_algodao INTO w_nf_produtor WHERE docnum_prod EQ w_saida_0100-docnum
                                                         AND itmnum_prod EQ w_saida_0100-itmnum.
        ADD w_nf_produtor-menge TO w_saida_0100-qtde_vinc_due.
        SUBTRACT w_nf_produtor-menge FROM w_saida_0100-saldo_exportar.
      ENDLOOP.
    ELSE.
      LOOP AT t_znom_reme_notas INTO DATA(w_znom_reme_notas) WHERE docnum EQ w_saida_0100-docnum
                                                               AND itmnum EQ w_saida_0100-itmnum.
        ADD w_znom_reme_notas-nr_quantidade TO w_saida_0100-qtde_vinc_due.
        SUBTRACT w_znom_reme_notas-nr_quantidade FROM w_saida_0100-saldo_exportar.
      ENDLOOP.
    ENDIF.

    LOOP AT t_zsdt0276 INTO DATA(w_zsdt0276) WHERE docnum  EQ w_saida_0100-docnum
                                               AND itmnum  EQ w_saida_0100-itmnum.
      SUBTRACT  w_zsdt0276-menge FROM w_saida_0100-saldo_exportar.
    ENDLOOP.

    CHECK w_saida_0100-saldo_exportar IS NOT INITIAL.

    w_saida_0100-qtde_baixar  = w_saida_0100-saldo_exportar.

    APPEND w_saida_0100 TO t_saida_0100.

  ENDLOOP.

  SORT t_saida_0100 BY bukrs branch credat docnum id_due.

ENDFORM.

*******************************************************************************************
* j_abnfe_active
*******************************************************************************************
FORM f_get_j_1bnfe_active .

  SELECT *
    FROM j_1bnfe_active
    INTO TABLE t_j_1bnfe_active
     FOR ALL ENTRIES IN t_j_1bnfdoc
   WHERE docnum = t_j_1bnfdoc-docnum.

  SORT t_j_1bnfe_active BY docnum.

ENDFORM.

*******************************************************************************************
* color saida
*******************************************************************************************
FORM f_fill_color_saida  CHANGING p_saida_0100 TYPE ty_saida_0100.

  DATA: wl_color  TYPE kkblo_specialcol.

  CLEAR: wl_color.
  wl_color-fieldname = 'QTDE_BAIXAR'.
  wl_color-color-col = 6.
  wl_color-color-int = 1.
  wl_color-color-inv = 1.
  APPEND wl_color TO p_saida_0100-color.

* wl_color-fieldname = 'SALDO_EXPORTAR'.
* APPEND wl_color TO p_saida_0100-color.

ENDFORM.

*******************************************************************************************
* chave nfe
*******************************************************************************************
FORM f_set_chave_doc.

  LOOP AT t_j_1bnfdoc INTO DATA(w_j_1bnfdoc).

    READ TABLE t_j_1bnfe_active INTO DATA(w_j_1bnfe_active)
                                WITH KEY docnum = w_j_1bnfdoc-docnum
                                BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    CONCATENATE w_j_1bnfe_active-regio   "Região do emissor NF-e
                w_j_1bnfe_active-nfyear  "Ano da data do documento da NF-e
                w_j_1bnfe_active-nfmonth "Mês da data do documento da NF-e
                w_j_1bnfe_active-stcd1   "Nº CNPJ do emissor da NF-e
                w_j_1bnfe_active-model   "Modelo da nota fiscal
                w_j_1bnfe_active-serie   "SERIE
                w_j_1bnfe_active-nfnum9  "Nº NF-e de nove posições
                w_j_1bnfe_active-docnum9 "NF-e: nº aleatório
                w_j_1bnfe_active-cdv     "Dígito controle p/chave de acesso NF-e
           INTO w_j_1bnfdoc-chave_nfe.

    IF strlen( w_j_1bnfdoc-chave_nfe ) NE 44.
      CLEAR: w_j_1bnfdoc-chave_nfe.
    ENDIF.

    MODIFY t_j_1bnfdoc FROM w_j_1bnfdoc.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* lfa1
*******************************************************************************************
FORM f_get_lfa1.

  t_j_1bnfdoc_aux[] = t_j_1bnfdoc[].
  DELETE t_j_1bnfdoc_aux WHERE partyp NE 'V'.
  SORT t_j_1bnfdoc_aux BY parid.
  DELETE ADJACENT DUPLICATES FROM t_j_1bnfdoc_aux COMPARING parid.

  CHECK t_j_1bnfdoc_aux[] IS NOT INITIAL.

  SELECT *
    FROM lfa1
    INTO TABLE t_lfa1
     FOR ALL ENTRIES IN t_j_1bnfdoc_aux
   WHERE lifnr EQ t_j_1bnfdoc_aux-parid.

ENDFORM.

*******************************************************************************************
* j_1bbranch
*******************************************************************************************
FORM f_get_j_1bbranch.

  LOOP AT t_j_1bnfdoc INTO DATA(w_j_1bnfdoc) WHERE partyp EQ 'B'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_j_1bnfdoc-parid+4(4)
      IMPORTING
        output = w_j_1bnfdoc-parid_b.

    MODIFY t_j_1bnfdoc FROM w_j_1bnfdoc.
  ENDLOOP.

  t_j_1bnfdoc_aux[] = t_j_1bnfdoc[].
  DELETE t_j_1bnfdoc_aux WHERE partyp NE 'B'.
  SORT t_j_1bnfdoc_aux BY parid_b.
  DELETE ADJACENT DUPLICATES FROM t_j_1bnfdoc_aux COMPARING parid_b.

  CHECK t_j_1bnfdoc_aux[] IS NOT INITIAL.

  SELECT *
    FROM lfa1
    APPENDING TABLE t_lfa1
     FOR ALL ENTRIES IN t_j_1bnfdoc_aux
   WHERE lifnr EQ t_j_1bnfdoc_aux-parid_b.

ENDFORM.

*******************************************************************************************
* kna1
*******************************************************************************************
FORM f_get_kna1.

  t_j_1bnfdoc_aux[] = t_j_1bnfdoc[].
  DELETE t_j_1bnfdoc_aux WHERE partyp NE 'C'.
  SORT t_j_1bnfdoc_aux BY parid.
  DELETE ADJACENT DUPLICATES FROM t_j_1bnfdoc_aux COMPARING parid.

  CHECK t_j_1bnfdoc_aux[] IS NOT INITIAL.

  SELECT *
    FROM kna1
    INTO TABLE t_kna1
     FOR ALL ENTRIES IN t_j_1bnfdoc_aux
   WHERE kunnr EQ t_j_1bnfdoc_aux-parid.

ENDFORM.

*******************************************************************************************
* makt
*******************************************************************************************
FORM f_get_makt.

  t_j_1bnfdoc_aux[] = t_j_1bnfdoc[].
  SORT t_j_1bnfdoc_aux BY matnr.
  DELETE ADJACENT DUPLICATES FROM t_j_1bnfdoc_aux COMPARING matnr.

  CHECK t_j_1bnfdoc_aux[] IS NOT INITIAL.

  SELECT *
    FROM makt
    INTO TABLE t_makt
     FOR ALL ENTRIES IN t_j_1bnfdoc_aux
   WHERE matnr EQ t_j_1bnfdoc_aux-matnr
     AND spras EQ sy-langu.

ENDFORM.

*******************************************************************************************
* get due
*******************************************************************************************
FORM f_get_due.

  SELECT *
    FROM znom_reme_notas
    INTO TABLE t_znom_reme_notas
     FOR ALL ENTRIES IN t_j_1bnfdoc
   WHERE docnum  EQ t_j_1bnfdoc-docnum.

ENDFORM.

*******************************************************************************************
* get nfe exportacao
*******************************************************************************************
FORM f_get_nfe_exportacao .

  CHECK t_znom_reme_notas[] IS NOT INITIAL.

* Nota Fiscal do Produtor Vinculadas na Nota de Exportação
  SELECT *
    FROM zdoc_nf_produtor
    INTO TABLE t_nf_produtor
     FOR ALL ENTRIES IN t_znom_reme_notas
   WHERE docnum_prod      = t_znom_reme_notas-docnum
     AND itmnum_prod      = t_znom_reme_notas-itmnum
     AND id_nomeacao_tran = t_znom_reme_notas-id_nomeacao_tran
     AND grp_retorno      = t_znom_reme_notas-grp_retorno.

ENDFORM.

*******************************************************************************************
* get nfe exportacao algodao
*******************************************************************************************
FORM f_get_nfe_exportacao_algodao.

  CHECK t_znom_reme_notas[] IS NOT INITIAL.

* Nota Fiscal do Produtor Vinculadas na Nota de Exportação
  SELECT *
    FROM zdoc_nf_produtor
    INTO TABLE t_nf_produtor_algodao
     FOR ALL ENTRIES IN t_znom_reme_notas
   WHERE docnum_prod  = t_znom_reme_notas-docnum
     AND itmnum_prod  = t_znom_reme_notas-itmnum.

ENDFORM.

*******************************************************************************************
* get baica
*******************************************************************************************
FORM f_get_baixa.

  SELECT *
    FROM zsdt0276
    INTO TABLE t_zsdt0276
     FOR ALL ENTRIES IN t_j_1bnfdoc
   WHERE   docnum  = t_j_1bnfdoc-docnum
     AND ( status  = ' '
      OR   status  = 'A' ).

ENDFORM.

*******************************************************************************************
* REFRESH
*******************************************************************************************
FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

*******************************************************************************************
* catalogo alv
*******************************************************************************************
FORM f_criar_catalog USING p_screen.

  FREE: wa_fcat, it_fcat.

  PERFORM f_estrutura_alv USING:
   01  'J_1BNFDOC'        'BUKRS'     'T_SAIDA_0100'  'BUKRS'             'Empresa'                   '07'   ' '    ''  ' ' ' ' ' ' ' ' 'X' ,
   02  'J_1BNFDOC'        'BRANCH'    'T_SAIDA_0100'  'BRANCH'            'Centro'                    '06'   ' '    ''  ' ' ' ' ' ' ' ' 'X' ,
   03  'J_1BNFDOC'        'DOCNUM'    'T_SAIDA_0100'  'DOCNUM'            'Docnum'                    '10'   ' '    ''  ' ' ' ' 'X' ' ' 'X' ,
   04  'J_1BNFLIN'        'ITMNUM'    'T_SAIDA_0100'  'ITMNUM'            'Item'                      '06'   ' '    ''  ' ' ' ' ' ' ' ' 'X' ,
   05  'J_1BNFDOC'        'NFENUM'    'T_SAIDA_0100'  'NFENUM'            'Nro.NF'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   06  'J_1BNFDOC'        'SERIES'    'T_SAIDA_0100'  'SERIES'            'Série'                     '05'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   07  'J_1BNFDOC'        'MODEL'     'T_SAIDA_0100'  'MODEL'             'Modelo'                    '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
   08  'J_1BNFDOC'        'CREDAT'    'T_SAIDA_0100'  'CREDAT'            'Dt.Criação'                '10'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
   09  'J_1BNFDOC'        'DOCDAT'    'T_SAIDA_0100'  'DOCDAT'            'Dt.Emissao'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   10  'J_1BNFDOC'        'PARID'     'T_SAIDA_0100'  'PARID'             'Parceiro'                  '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   11  'LFA1'             'NAME1'     'T_SAIDA_0100'  'NAME1'             'Nome'                      '35'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   12  'LFA1'             'REGIO'     'T_SAIDA_0100'  'REGIO'             'Estado'                    '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   13  'J_1BNFLIN'        'MATNR'     'T_SAIDA_0100'  'MATNR'             'Material'                  '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   14  'MAKT'             'MAKTX'     'T_SAIDA_0100'  'MAKTX'             'Ds.Material'               '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   15  'J_1BNFLIN'        'CFOP'      'T_SAIDA_0100'  'CFOP'              'CFOP'                      '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   16  'J_1BNFLIN'        'NBM'       'T_SAIDA_0100'  'NBM'               'NCM'                       '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   17  'ZIB_NFE_DIST_ITM' 'CHAVE_NFE' 'T_SAIDA_0100'  'CHAVE_NFE'         'Chv.NFe'                   '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   18  'J_1BNFLIN'        'MEINS'     'T_SAIDA_0100'  'MEINS'             'Unid.'                     '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
   19  'J_1BNFLIN'        'MENGE'     'T_SAIDA_0100'  'MENGE'             'Qtde.NF (KG)'              '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   20  'J_1BNFLIN'        'MENGE'     'T_SAIDA_0100'  'DIF_PESO_CCT_NF'   'Dif.Peso.NFxCCT'           '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   21  'J_1BNFLIN'        'MENGE'     'T_SAIDA_0100'  'QTDE_VINC_DUE'     'Qtde Vinc.'                '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   22  'J_1BNFLIN'        'MENGE'     'T_SAIDA_0100'  'SALDO_EXPORTAR'    'Saldo Exportar'            '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
   23  'ZSDT0276'         'MENGE'     'T_SAIDA_0100'  'QTDE_BAIXAR'       'Qtde a Baixar'             '13'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
   24  'ZSDT0276'         'VALOR'     'T_SAIDA_0100'  'VALOR_PAGO'        'Valor Pago'                '13'   'X'    ''  ' ' ' ' ' ' ' ' '' .

ENDFORM.

*******************************************************************************************
* exclude fcode
*******************************************************************************************
FORM f_exclude_fcode USING p_screen.

  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO it_exclude_fcode.

ENDFORM.

*******************************************************************************************
* estrutura alv
*******************************************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                           VALUE(p_tabname)       LIKE dd02d-tabname
                           VALUE(p_field)         LIKE dd03d-fieldname
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                           VALUE(p_outputlen)
                           VALUE(p_edit)
                           VALUE(p_sum)
                           VALUE(p_emphasize)
                           VALUE(p_just)
                           VALUE(p_hotspot)
                           VALUE(p_f4)
                           VALUE(p_key).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = p_key.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
* wa_fcat-checkbox    = p_check.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

*******************************************************************************************
* refresh alv
*******************************************************************************************
FORM f_refresh_alv USING p_alv.

  CHECK obj_alv_0100 IS NOT INITIAL.

  wa_stable-row = abap_true.
  wa_stable-col = abap_true.

  CALL METHOD obj_alv_0100->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

*******************************************************************************************
* efetua baixa
*******************************************************************************************
FORM f_efetua_baixa CHANGING p_erro.

  DATA: t_index_rows TYPE lvc_t_row,
        t_row_no     TYPE lvc_t_roid,
        w_et_row_no  LIKE LINE OF t_row_no,
        l_user_lock  TYPE sy-uname,
        l_erro_lock  TYPE c.

  FREE: t_index_rows,
        t_row_no,
        t_documentos,
        l_erro_lock,
        l_user_lock,
        p_erro.

  LOOP AT t_saida_0100  INTO w_saida_0100.
    FREE: w_saida_0100-color.
    MODIFY t_saida_0100 FROM w_saida_0100 INDEX sy-tabix.
  ENDLOOP.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  IF t_row_no[] IS INITIAL.
    MESSAGE s024(sd) WITH text-031.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  LOOP AT t_row_no INTO w_et_row_no.
    READ TABLE t_saida_0100 INTO w_saida_0100 INDEX w_et_row_no-row_id.
    CHECK sy-subrc = 0.

    FREE: w_saida_0100-color.

*---lock registros
    CALL FUNCTION 'ENQUEUE_EZSDT0276'
      EXPORTING
        mode_zsdt0276  = 'E'
        mandt          = sy-mandt
        docnum         = w_saida_0100-docnum
        itmnum         = w_saida_0100-itmnum
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      l_erro_lock = abap_true.
      l_user_lock = sy-msgv1.
      p_erro = abap_true.
      PERFORM f_fill_color_saida CHANGING w_saida_0100.
      MODIFY t_saida_0100   FROM w_saida_0100 INDEX w_et_row_no-row_id.
      CONTINUE.
    ENDIF.

*---valida quantidade a baixar
    IF w_saida_0100-qtde_baixar > w_saida_0100-saldo_exportar.
      p_erro = abap_true.
      PERFORM f_fill_color_saida CHANGING w_saida_0100.
      MODIFY t_saida_0100   FROM w_saida_0100 INDEX w_et_row_no-row_id.
      CONTINUE.
    ENDIF.

    w_documentos-docnum   = w_saida_0100-docnum.
    w_documentos-itmnum   = w_saida_0100-itmnum.
    w_documentos-menge    = w_saida_0100-qtde_baixar.
    w_documentos-valor    = w_saida_0100-valor_pago.
    APPEND w_documentos  TO t_documentos.
  ENDLOOP.

  IF l_erro_lock = abap_true.
    MESSAGE s024(sd) WITH text-050 l_user_lock DISPLAY LIKE 'E'.
    PERFORM f_refresh_alv USING '0100'.
    EXIT.
  ENDIF.

  IF p_erro = abap_true.
    MESSAGE s024(sd) WITH text-040 DISPLAY LIKE 'E'.
    PERFORM f_refresh_alv USING '0100'.
    EXIT.
  ENDIF.
*------------------------------------------------
* efetua baixas
*------------------------------------------------
  CALL FUNCTION 'ZSD_LANCAR_BAIXAS'
    TABLES
      t_documentos       = t_documentos
    EXCEPTIONS
      id_baixa_not_found = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE e024(sd) WITH text-030.
  ENDIF.

  IF sy-ucomm = '&CANCEL'.
    p_erro = abap_true.
  ENDIF.

*--------------------------------------------
*-unlock registros
*--------------------------------------------
  LOOP AT t_row_no INTO w_et_row_no.
    READ TABLE t_saida_0100 INTO w_saida_0100 INDEX w_et_row_no-row_id.
    CHECK sy-subrc = 0.

    CALL FUNCTION 'DEQUEUE_EZSDT0276'
      EXPORTING
        mode_zsdt0276  = 'E'
        mandt          = sy-mandt
        docnum         = w_saida_0100-docnum
        itmnum         = w_saida_0100-itmnum
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
  ENDLOOP.

ENDFORM.

*******************************************************************************************
* implementacao Classe
*******************************************************************************************
CLASS lcl_alv_toolbar_0100 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: tl_parametros TYPE ustyp_t_parameters.

*      TY_TOOLBAR-ICON      = ICON_SYSTEM_UNDO.
*      TY_TOOLBAR-FUNCTION  = 'DISP_NFE_AJUSTE'.
*      TY_TOOLBAR-TEXT      = 'Lib. NF-e p/ Ajuste'.
*      TY_TOOLBAR-BUTN_TYPE = 0.
*      APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*      CLEAR TY_TOOLBAR.


  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: var_answer TYPE c.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler_0100 IMPLEMENTATION.

  METHOD catch_hotspot.

    CASE e_column_id.
      WHEN 'DOCNUM'.
        READ TABLE t_saida_0100 INTO w_saida_0100 INDEX e_row_id.
        CHECK ( sy-subrc EQ 0 ) AND ( w_saida_0100-docnum IS NOT INITIAL ).

        SET PARAMETER ID 'JEF'  FIELD w_saida_0100-docnum.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_data_changed4.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          wl_name1 TYPE makt-maktx,
          l_stext  TYPE sbttx,
          l_name1  TYPE pbtxt.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good
                             WHERE fieldname = 'QTDE_BAIXAR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      CHECK lv_value IS NOT INITIAL.

      CLEAR: l_stext, l_name1,
             w_saida_0100.

      READ TABLE t_saida_0100 INTO w_saida_0100 INDEX ls_good-row_id.

      CHECK sy-subrc = 0.

      IF lv_value > w_saida_0100-saldo_exportar.
        MESSAGE s024(sd) WITH text-040 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

INCLUDE zsdr0133_status_0100o01.

*******************************************************************************************
*******************************************************************************************
