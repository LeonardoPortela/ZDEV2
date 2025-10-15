*&---------------------------------------------------------------------*
*& Include          ZFIR0022_LM_TOP
*&---------------------------------------------------------------------*
**********************************************************************
*TYPES
**********************************************************************
CLASS lcl_handle_events DEFINITION DEFERRED.
DATA: gr_events TYPE REF TO lcl_handle_events.
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
   CLASS-METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function,
      on_before_user_command FOR EVENT before_salv_function OF cl_salv_events IMPORTING e_salv_function,
      on_after_user_command FOR EVENT after_salv_function OF cl_salv_events IMPORTING e_salv_function,
      "on_hotspot_click FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING e_row_id e_column_id es_row_no,
      on_hotspot_click FOR EVENT LINK_CLICK OF cl_salv_events_table IMPORTING column row sender,
      make_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_interactive e_object,
*      on_handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
*        IMPORTING
*          er_data_changed
*          e_onf4
*          e_onf4_before
*          e_onf4_after
*          e_ucomm
*          sender,
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

ENDCLASS.
  TYPES:
    BEGIN OF ty_lm,
      nfenum          TYPE j_1bnfdoc-nfenum,
      doc_fatura      TYPE zfit0026-doc_fatura,
      belnr           TYPE bkpf-belnr,
      valdt           TYPE vbrk-valdt,
      valor           TYPE vbrk-netwr,
      total_recebido  TYPE vbrk-netwr,
      saldo_finan     TYPE vbrk-netwr,
      refkey          TYPE j_1bnflin-refkey,
      vbeln           TYPE zfit0026-vbeln,
      seq             TYPE zfit0026-seq,
      data_venc       TYPE zfit0026-data_venc,
      moeda           TYPE zfit0026-moeda,
      mont_moeda      TYPE zfit0026-mont_moeda,
      taxa            TYPE zfit0026-taxa,
      mont_mi         TYPE zfit0026-mont_mi,
      forma_pag       TYPE zfit0026-forma_pag,
      status          TYPE zfit0026-status,
      bukrs           TYPE zfit0026-bukrs,
      razao_especial  TYPE zfit0026-razao_especial,
      zterm           TYPE zfit0026-zterm,
      data_pgto       TYPE zfit0026-data_pgto,
      mont_rbdo       TYPE zfit0026-mont_rbdo,
      vlr_multa_calc  TYPE p DECIMALS 2,
      vlr_juros_calc  TYPE p DECIMALS 2,
      vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
      vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
      observacao(255) TYPE c,
      ajuste          TYPE c,
      rec_vlr_total   TYPE zfit0026-rec_vlr_total,
      vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
      vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
      num_comp_adiant TYPE zfit0026-num_comp_adiant, "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
    END OF ty_lm,

    BEGIN OF wa_lanc,
      vbeln           TYPE zfit0026-vbeln,
      seq             TYPE zfit0026-seq,
      data_venc       TYPE zfit0026-data_venc,
      moeda           TYPE zfit0026-moeda,
      mont_moeda      TYPE zfit0026-mont_moeda,
      taxa            TYPE zfit0026-taxa,
      forma_pag       TYPE zfit0026-forma_pag,
      status          TYPE zfit0026-status,
      uname           TYPE zfit0026-uname,
      data_registro   TYPE zfit0026-data_registro,
      obj_key         TYPE zfit0026-obj_key,
      docnum          TYPE zfit0026-docnum,
      razao_especial  TYPE zfit0026-razao_especial,
      mont_moeda_fix  TYPE zfit0026-mont_moeda,
      mont_moeda_parc TYPE zfit0026-mont_moeda,
      zterm           TYPE zfit0026-zterm,
      razao           TYPE c LENGTH 8,
      nfenum          TYPE j_1bnfdoc-nfenum,
      data_pgto       TYPE zfit0026-data_pgto,
      vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
      vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
      mont_mi         TYPE zfit0026-mont_mi,
      mont_rbdo       TYPE zfit0026-mont_rbdo,
      vlr_multa_calc  TYPE zfit0026-vlr_multa_calc,
      vlr_juros_calc  TYPE zfit0026-vlr_juros_calc,
      doc_fatura      TYPE zfit0026-doc_fatura,
      bukrs_vf        TYPE vbak-bukrs_vf,
      edit(4)         TYPE c,
      gera(4)         TYPE c,
      estor(4)        TYPE c,
      status_doc(4)   TYPE c,
      excluir(4)      TYPE c,
      observacao(255) TYPE c,
      ajuste(1)       TYPE c,
      rec_vlr_total   TYPE zfit0026-rec_vlr_total,
      vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
      vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
      pgto_ant(15)    TYPE c,
      num_comp_adiant TYPE zfit0026-num_comp_adiant, "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
    END OF wa_lanc,

    BEGIN OF wa_edit,
      vbeln           TYPE zfit0026-vbeln,
      seq             TYPE zfit0026-seq,
      data_venc       TYPE zfit0026-data_venc,
      moeda           TYPE zfit0026-moeda,
      mont_moeda      TYPE zfit0026-mont_moeda,
      taxa            TYPE zfit0026-taxa,
      mont_mi         TYPE zfit0026-mont_mi,
      forma_pag       TYPE zfit0026-forma_pag,
      status          TYPE zfit0026-status,
      bukrs           TYPE zfit0026-bukrs,
      razao_especial  TYPE zfit0026-razao_especial,
      zterm           TYPE zfit0026-zterm,
      nfenum          TYPE j_1bnfdoc-nfenum,
      data_pgto       TYPE zfit0026-data_pgto,
      mont_rbdo       TYPE zfit0026-mont_rbdo,
      vlr_multa_calc  TYPE p DECIMALS 2,
      vlr_juros_calc  TYPE p DECIMALS 2,
*    VLR_MULTA_CALC  TYPE ZFIT0026-VLR_MULTA_CALC,
*    VLR_JUROS_CALC  TYPE ZFIT0026-VLR_JUROS_CALC,
      vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
      vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
      doc_fatura      TYPE zfit0026-doc_fatura,
      observacao(255) TYPE c,
      ajuste          TYPE c,
      rec_vlr_total   TYPE zfit0026-rec_vlr_total,
      vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
      vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
      valor           TYPE vbrk-netwr,
      total_recebido  TYPE vbrk-netwr,
      num_comp_adiant TYPE zfit0026-num_comp_adiant, "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
    END OF wa_edit,

    BEGIN OF ty_editor,
      line(255),
    END   OF ty_editor,

    BEGIN OF wa_lanc_ver,
      vbeln           TYPE zfit0026-vbeln,
      seq             TYPE zfit0026-seq,
      data_venc       TYPE zfit0026-data_venc,
      moeda           TYPE zfit0026-moeda,
      mont_moeda      TYPE zfit0026-mont_moeda,
      taxa            TYPE zfit0026-taxa,
      mont_mi         TYPE zfit0026-mont_mi,
      forma_pag       TYPE zfit0026-forma_pag,
      status          TYPE zfit0026-status,
      uname           TYPE zfit0026-uname,
      data_registro   TYPE zfit0026-data_registro,
      bukrs           TYPE zfit0026-bukrs,
      obj_key         TYPE zfit0026-obj_key,
      docnum          TYPE zfit0026-docnum,
      zterm           TYPE zfit0026-zterm,
      doc_fatura      TYPE zfit0026-doc_fatura,
      data_pgto       TYPE zfit0026-data_pgto,
      vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
      vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
      mont_rbdo       TYPE zfit0026-mont_rbdo,
      vlr_multa_calc  TYPE zfit0026-vlr_multa_calc,
      vlr_juros_calc  TYPE zfit0026-vlr_juros_calc,
      razao_especial  TYPE zfit0026-razao_especial,
      observacao(255) TYPE c,
      ajuste          TYPE zfit0026-ajuste,
      mont_moeda_fix  TYPE zfit0026-mont_moeda,
      nfenum          TYPE j_1bnfdoc-nfenum,
      edit(4)         TYPE c,
      gera(4)         TYPE c,
      status_doc(4)   TYPE c,
      excluir(4)      TYPE c,
      rec_vlr_total   TYPE zfit0026-rec_vlr_total,
      vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
      vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
      num_comp_adiant TYPE zfit0026-num_comp_adiant,
    END OF wa_lanc_ver,

    BEGIN OF wa_cont_seq,
      vbeln TYPE zfit0026-vbeln,
      seq   TYPE zfit0026-seq,
    END OF wa_cont_seq,

    BEGIN OF wa_saida,
      bukrs_vf        TYPE  vbak-bukrs_vf,
      auart           TYPE  vbak-auart,
      kunnr           TYPE  vbak-kunnr,
      vkbur           TYPE  vbak-vkbur,
      vbeln           TYPE  vbak-vbeln,
      erdat           TYPE  vbak-erdat,
      waerk           TYPE  vbak-waerk,
      knumv           TYPE  vbak-knumv,
      netwr           TYPE  vbak-netwr,
      netwr_l         TYPE  vbap-netwr,
      mwsbp           TYPE  vbap-mwsbp,
      zlsch           TYPE  vbkd-zlsch,
      kurrf           TYPE  vbkd-kurrf,
      valdt           TYPE  vbkd-valdt,
      name1           TYPE  kna1-name1,
      werks           TYPE  vbap-werks,
      zterm           TYPE  vbkd-zterm,
      text1           TYPE t052u-text1,
      meio_pgmto      TYPE dd07v-ddtext,
      edit(4)         TYPE  c,
      visual(4)       TYPE  c,
      observacao(255) TYPE c,
      rec_vlr_total   TYPE zfit0026-rec_vlr_total,
      pgto_ant(15)    TYPE c,
      check_calc(1)    TYPE c,
    END OF wa_saida,

    BEGIN OF ty_alter,
      row_id         TYPE i,
      vlr_multa_rbdo TYPE zfit0026-vlr_multa_rbdo,
      vlr_juros_rbdo TYPE zfit0026-vlr_juros_rbdo,
      mont_rbdo      TYPE zfit0026-mont_rbdo,
      data_pgto      TYPE zfit0026-data_pgto,
      rec_vlr_total  TYPE zfit0026-rec_vlr_total,
      forma_pag      TYPE zfit0026-forma_pag,
      zterm          TYPE zfit0026-zterm,
    END OF ty_alter.

**********************************************************************
*ALV
**********************************************************************

DATA: gr_table TYPE REF TO CL_SALV_TABLE.
  DATA container_lm         TYPE REF TO cl_gui_custom_container.
  DATA container_main TYPE REF TO cl_gui_custom_container.
  DATA painel1              TYPE REF TO cl_gui_container.
  DATA painel2              TYPE REF TO cl_gui_container.
  DATA painel_control       TYPE REF TO cl_gui_splitter_container.
  DATA lt_rows              TYPE salv_t_row.
  DATA ls_row               TYPE int4.
  DATA ls_api               TYPE REF TO if_salv_gui_om_extend_grid_api.
  DATA ls_edit              TYPE REF TO if_salv_gui_om_edit_restricted.
  DATA lr_column            TYPE REF TO cl_salv_column.
  DATA lr_columns_TB        TYPE REF TO cl_salv_columns_table.
  DATA lr_column_TB         TYPE REF TO cl_salv_column_table.
  DATA lr_columns           TYPE REF TO cl_salv_columns.
  DATA lr_functions         TYPE REF TO cl_salv_functions.
  DATA lr_selections        TYPE REF TO cl_salv_selections.
  DATA lr_sorts_tb          TYPE REF TO cl_salv_sorts.
  DATA lr_aggregations_tb   TYPE REF TO cl_salv_aggregations.
  DATA lr_sort_tb           TYPE REF TO cl_salv_sort.
  DATA lr_aggregation_tb    TYPE REF TO cl_salv_aggregation.
  DATA lv_key               TYPE salv_s_layout_key.
  DATA lex_not_found        TYPE REF TO cx_salv_not_found.
  DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
  DATA l_title              TYPE lvc_title.
  DATA lr_layout            TYPE REF TO cl_salv_layout.
  DATA icon_status          TYPE string.
  DATA it_lanc_mass         TYPE STANDARD TABLE OF zfie0026 INITIAL SIZE 0.
  DATA it_lanc_mass_err     TYPE STANDARD TABLE OF zfie0026 INITIAL SIZE 0.
  DATA wa_edit              TYPE wa_edit.
  DATA it_edit              LIKE STANDARD TABLE OF wa_edit  WITH HEADER LINE.
  DATA wa_lanc_mass         TYPE zfie0026.
  DATA wa_ERROR             TYPE zfie0026.
  DATA ordemVenda           TYPE zfie0026-vbeln.
  DATA wa_lanc              TYPE wa_lanc.
  DATA wa_lanc_ver          TYPE wa_lanc_ver.
  DATA wa_editor            TYPE ty_editor.
  DATA it_lanc              LIKE STANDARD TABLE OF wa_lanc             WITH HEADER LINE.
  DATA it_editor            TYPE TABLE OF ty_editor.
  DATA wa_saida             TYPE wa_saida.
  DATA r_devo_recu          TYPE RANGE OF auart.
  DATA wa_cont_seq          TYPE wa_cont_seq.
  DATA it_cont_seq          LIKE STANDARD TABLE OF wa_cont_seq         WITH HEADER LINE.
  DATA it_saida             LIKE STANDARD TABLE OF wa_saida WITH HEADER LINE.
  DATA it_lanc_ver          LIKE STANDARD TABLE OF wa_lanc_ver      WITH HEADER LINE.
  DATA lr_cols_aggreg       TYPE REF TO cl_salv_aggregations.
  DATA lr_col_aggreg        TYPE REF TO cl_salv_aggregation.
  DATA lr_cols_Sorts        TYPE REF TO cl_salv_sorts.
  DATA lr_col_sort          TYPE REF TO cl_salv_sort.
  DATA l_s_cell             TYPE salv_s_cell.
  DATA l_t_cell             TYPE salv_t_cell.
  DATA l_t_colum            TYPE salv_t_column.
  DATA l_t_row              TYPE salv_t_row.
  DATA l_s_mode             TYPE i.
  FIELD-SYMBOLS <_valida>   TYPE zfie0026.
**********************************************************************
*VALIDA
**********************************************************************
  DATA: t_day_attributes    TYPE TABLE OF  casdayattr.
  DATA: w_day_attributes    TYPE casdayattr.
  DATA: dt_ini              TYPE sy-datum.
  DATA: dt_fim              TYPE sy-datum.
  DATA: v_data_aux          TYPE datum.
  DATA: v_mont_aux          TYPE zfit0026-vlr_multa_rbdo.
  DATA: vlr_jros            TYPE p DECIMALS 2.
  DATA: vlr_mult            TYPE p DECIMALS 2.
  DATA: fat_prop            TYPE p DECIMALS 9.
  DATA: prop_juros          TYPE p DECIMALS 9.
  DATA: prop_multa          TYPE p DECIMALS 9.
  DATA: vlr_multa           TYPE p DECIMALS 2.
  DATA: vlr_juros           TYPE p DECIMALS 2.
  DATA: mont_rbdo_anter     TYPE vbrk-netwr.
  DATA: dt_pgto_anter       TYPE datum.
  DATA: p_calc            TYPE c,
        p_ins(1)        TYPE c,
        p_mi(1)         TYPE c,
        total_ov        TYPE p DECIMALS 2,
        tot_ov          TYPE p DECIMALS 2,
        _new_multa      TYPE p DECIMALS 2,
        _new_juros      TYPE p DECIMALS 2,
        _new_vlr        TYPE p DECIMALS 2,
        it_alter_values TYPE STANDARD TABLE OF ty_alter INITIAL SIZE 0,
        it_get_values   TYPE STANDARD TABLE OF ty_alter INITIAL SIZE 0,
        wa_alter_values TYPE ty_alter,
        p_erro(1) type c.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
data E01(150) TYPE c.
E01 = |Montante da OV deve ser igual ao montante parcial da referencia!|.
**********************************************************************
*START-OF-SELECTION.
**********************************************************************
  START-OF-SELECTION.

    CLEAR: it_lanc_mass,it_lanc_mass[].
    IMPORT it_lanc_mass FROM MEMORY ID 'ZFIR0022_LM'.

    READ TABLE it_lanc_mass INTO wa_lanc_mass INDEX 1.

    ordemVenda =  wa_lanc_mass-vbeln.




    PERFORM make_container_lm.
    CALL SCREEN 0100.
