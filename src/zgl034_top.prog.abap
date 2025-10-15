*&---------------------------------------------------------------------*
*&  Include           ZGL034_TOP
*&---------------------------------------------------------------------*

REPORT zgl034.

TABLES: zglt080.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END OF ty_t001.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF ty_lfa1.

TYPES: BEGIN OF ty_saida_0100,
         status_ctb TYPE c LENGTH 4,
         status_nf  TYPE c LENGTH 4,
         butxt      TYPE t001-butxt,
         name1      TYPE lfa1-name1,
         anexos     TYPE i.
         INCLUDE STRUCTURE zglt080.
TYPES: END OF ty_saida_0100.

TYPES: BEGIN OF ty_saida_0110,
         status_nf  TYPE c LENGTH 4,
         status_ctb TYPE c LENGTH 4,
         shipfrom   TYPE lfa1-regio,
         shipto     TYPE lfa1-regio,
         nftype     TYPE j_1baa-nftype,
         estilo     TYPE lvc_t_styl.
         INCLUDE STRUCTURE zglt081.
TYPES: END OF ty_saida_0110.

TYPES: BEGIN OF ty_saida_0110_treina,
         seq_lcto TYPE zseq_lcto,
         seqitem  TYPE num06,
         cpf      TYPE pbr_cpfnr,
         id_lms	  TYPE zid_lms,
       END OF ty_saida_0110_treina.

TYPES: BEGIN OF ty_irf,
         text40 TYPE t059u-text40,
         gsber  TYPE zglt081-gsber,
         nfenum TYPE zglt081-nfenum,
         series TYPE zglt081-series.
         INCLUDE STRUCTURE zglt088.
TYPES: END OF ty_irf.

TYPES: BEGIN OF ty_j_1bnfdoc,
         docnum TYPE j_1bnfdoc-docnum,
         nftype TYPE j_1bnfdoc-nftype,
         doctyp TYPE j_1bnfdoc-doctyp,
         direct TYPE j_1bnfdoc-direct,
         docdat TYPE j_1bnfdoc-docdat,
         cancel TYPE j_1bnfdoc-cancel,
         nfenum TYPE j_1bnfdoc-nfenum,
         series TYPE j_1bnfdoc-series,
         parid  TYPE j_1bnfdoc-parid,
       END OF ty_j_1bnfdoc.

DATA: BEGIN OF tg_080 OCCURS 0,
        instid_a TYPE srgbtbrel-instid_a.
        INCLUDE STRUCTURE zglt080.
DATA: END OF tg_080.

DATA: BEGIN OF tg_081 OCCURS 0.
        INCLUDE STRUCTURE zglt081.
DATA: END OF tg_081.

DATA: BEGIN OF tg_080_aux OCCURS 0.
        INCLUDE STRUCTURE zglt080.
DATA: END OF tg_080_aux.

DATA: BEGIN OF tg_081_aux OCCURS 0.
        INCLUDE STRUCTURE zglt081.
DATA: END OF tg_081_aux.

DATA: BEGIN OF tg_impo OCCURS 0,
        taxtyp   TYPE zglt085-taxtyp,
        ttypetxt TYPE j_1bttytxt,
        taxgrp   TYPE j_1btaxgrp,
        base     TYPE zglt085-base,
        rate     TYPE zglt085-rate,
        taxval   TYPE zglt085-taxval,
        excbas   TYPE zglt085-excbas,
        othbas   TYPE zglt085-othbas,
      END OF tg_impo.

DATA: BEGIN OF tg_desc_cab OCCURS 0,
        name1    TYPE lfa1-name1,
        ds_hbkid TYPE t012t-text1,
      END OF tg_desc_cab.

DATA: BEGIN OF tg_anexos OCCURS 0.
        INCLUDE STRUCTURE srgbtbrel.
DATA: END OF tg_anexos.


DATA:   msg_erro   TYPE string. "115706 CS2022000452 Bloquear Geração de Doc. Fiscal em Período Fechado - ZGL059 - PSA
DATA:   lv_zlspr   TYPE t008t-textl. " RJF - CS2024000206 - Lançamentos Bloqueados - Contratos Corporativos (ZGL059)
*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

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

CLASS lcl_alv_toolbar_0110 DEFINITION.
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

CLASS lcl_event_handler_0110 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_event_handler_0100 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container,
      obj_alv_0110       TYPE REF TO cl_gui_alv_grid,
      obj_container_0110 TYPE REF TO cl_gui_custom_container,
      obj_alv_0112       TYPE REF TO cl_gui_alv_grid,
      obj_container_0112 TYPE REF TO cl_gui_custom_container,
      obj_alv_0113       TYPE REF TO cl_gui_alv_grid,
      obj_container_0113 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0100 TYPE REF TO lcl_alv_toolbar_0100,
      obj_toolbar_0110 TYPE REF TO lcl_alv_toolbar_0110.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo TYPE lvc_s_styl.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura.

* Gerenciador Arquivos
DATA: manager TYPE REF TO cl_gos_manager,
      obj     TYPE borident,
      ip_mode TYPE sgs_rwmod,
      objtype TYPE borident-objtype VALUE 'ZGL059'.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100  TYPE TABLE OF ty_saida_0100,
      wa_saida_0100  TYPE ty_saida_0100,
      it_saida_0110  TYPE TABLE OF ty_saida_0110,
      wa_saida_0110  TYPE ty_saida_0110,
      it_0110_treina TYPE TABLE OF ty_saida_0110_treina,
      it_saida_0112  TYPE TABLE OF ty_saida_0110,
      wa_saida_0112  TYPE ty_saida_0110,
      it_saida_0113  TYPE TABLE OF ty_irf,
      wa_saida_0113  TYPE ty_irf,
      tg_irf         TYPE TABLE OF ty_irf WITH HEADER LINE,
      tg_irf_basman  TYPE TABLE OF ty_irf WITH HEADER LINE,
      tg_j_1bnfdoc   TYPE TABLE OF ty_j_1bnfdoc WITH HEADER LINE,
      wa_zib_chave   TYPE zib_contabil_chv,
      wa_zib_erro    TYPE zib_contabil_err,
      wa_zmmt0024    TYPE zmmt0024,
      wa_zglt081     TYPE zglt081,
      it_outreturn   TYPE TABLE OF zfie_ret_document,
      wa_outreturn   TYPE zfie_ret_document,
      tg_t001        TYPE TABLE OF ty_t001 WITH HEADER LINE,
      tg_lfa1        TYPE TABLE OF ty_lfa1 WITH HEADER LINE,
      tg_sai_0110    TYPE TABLE OF ty_saida_0110 WITH HEADER LINE.

*-------------------------------------------------------------------
* Ranges
*-------------------------------------------------------------------
RANGES:  p_bukrs     FOR zglt080-bukrs,    "Empresa
         p_lifnr     FOR zglt080-lifnr,    "Fornecedor
         p_budat     FOR zglt080-budat,    "Data Lcto
         p_zfbdt     FOR zglt080-zfbdt,    "Data Vcto
         p_seq_lcto  FOR zglt080-seq_lcto, "Seq. Lcto
         p_bldat     FOR zglt080-bldat,    "Data do Documento
         p_usnam     FOR zglt080-usnam.    "Usuario

RANGES:  r_bukrs     FOR zglt080-bukrs,    "Empresa
         r_lifnr     FOR zglt080-lifnr,    "Fornecedor
         r_budat     FOR zglt080-budat,    "Data Lcto
         r_zfbdt     FOR zglt080-zfbdt,    "Data Vcto
         r_seq_lcto  FOR zglt080-seq_lcto, "Seq. Lcto
         r_bldat     FOR zglt080-bldat,    "Data do Documento
         r_usnam     FOR zglt080-usnam.    "Usuario

*-------------------------------------------------------------------*
* Variaveis
*-------------------------------------------------------------------*

DATA: vg_not_found   TYPE c,
      vg_opr_lcto    TYPE c LENGTH 10,
      var_answer     TYPE c,
      vg_sel_lote    TYPE c,
      p_file         TYPE rlgrap-filename,
      vg_objkey_proc TYPE zglt081-objkey,
      vg_ger_mov     TYPE c,
      vg_call_ext    TYPE c,
      vg_id_log      TYPE i,
      vg_message     TYPE zglt087-message,
      wl_0100_sel    TYPE ty_saida_0100,
      vg_cbar_unico  TYPE zglt080-cod_barras,
      vg_nf_unica    TYPE c,
      vg_modify_irf  TYPE c,
      vg_modify_cbar TYPE c,
      owner          TYPE soud-usrnam,
      sofd_dat       LIKE sofdd,
      v_resp_treina  TYPE c,
      vl_matnr18     TYPE matnr18.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_display TYPE c VALUE 'DISPLAY' LENGTH 7,
           c_edit    TYPE c VALUE 'EDIT'    LENGTH 7,
           c_new     TYPE c VALUE 'NEW'     LENGTH 7,
           c_0       TYPE c VALUE '0',
           c_1       TYPE c VALUE '1',
           c_2       TYPE c VALUE '2',
           c_b       TYPE c VALUE 'B',
           c_s       TYPE c VALUE 'S',
           c_l       TYPE c VALUE 'L',
           c_x       TYPE c VALUE 'X',
           c_d       TYPE c VALUE 'D',
           c_k       TYPE c VALUE 'K',
           c_w       TYPE c VALUE 'W',
           c_f       TYPE c VALUE 'F',
           c_t       TYPE c VALUE 'T',
           c_i       TYPE c VALUE 'I',
           c_n       TYPE c VALUE 'N',
           c_h       TYPE c VALUE 'H',
           c_ag(2)   TYPE c VALUE 'AG',
           c_ne(2)   TYPE c VALUE 'NE',
           c_01(2)   TYPE c VALUE '01',
           c_30(2)   TYPE c VALUE '30',
           c_40(2)   TYPE c VALUE '40',
           c_50(4)   TYPE c VALUE '0050',
           c_60(2)   TYPE c VALUE '60',
           c_76(2)   TYPE c VALUE '76',
           c_71(2)   TYPE c VALUE '71',
           c_72(2)   TYPE c VALUE '72',
           c_br(2)   TYPE c VALUE 'BR',
           c_lf(2)   TYPE c VALUE 'LF',
           c_lr(2)   TYPE c VALUE 'LR',
           c_icm3(4) TYPE c VALUE 'ICM3', "
           c_ipis(4) TYPE c VALUE 'IPIS',
           c_icof(4) TYPE c VALUE 'ICOF',
           c_ics1(4) TYPE c VALUE 'ICS1'.

INITIALIZATION. "\\ Start program;

  GET PARAMETER ID 'OBJKEY' FIELD vg_objkey_proc.
  GET PARAMETER ID 'GERMOV' FIELD vg_ger_mov.

  SET PARAMETER ID 'OBJKEY' FIELD ''.
  SET PARAMETER ID 'GERMOV' FIELD ''.

  IF vg_objkey_proc IS NOT INITIAL.
    "Verificar se lançamento já foi criado.
    CLEAR: wa_zmmt0024, wa_zglt081.
    SELECT SINGLE *
      FROM zmmt0024 INTO wa_zmmt0024
     WHERE objkey = vg_objkey_proc.

    IF sy-subrc = 0.
      SELECT SINGLE *
        FROM zglt081 AS a INTO wa_zglt081
       WHERE a~objkey EQ wa_zmmt0024-objkey
         AND EXISTS ( SELECT *
                        FROM zglt080 AS b
                       WHERE b~seq_lcto = a~seq_lcto
                         AND loekz = '' ).
      IF sy-subrc NE 0.
        vg_call_ext = 'X'.
        PERFORM f_gravar_lcto USING ''.
      ENDIF.
    ENDIF.
  ENDIF.

  IF vg_ger_mov IS NOT INITIAL.
    PERFORM f_selecionar_dados.
  ENDIF.

  IF ( vg_call_ext IS NOT INITIAL ) OR
     ( vg_ger_mov  IS NOT INITIAL ).
    RETURN.
  ENDIF.

  p_budat-low = sy-datum.
  p_usnam-low = sy-uname.

  CALL SCREEN 0100.
