*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_TOP
*&---------------------------------------------------------------------*

REPORT zsdr0095.

TYPE-POOLS: icon.

TABLES: j_1bnfdoc, j_1bnflin, lfa1, znom_reme_notas, zib_nfe_dist_itm, zsdt0053.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0100,
         seq_doc                TYPE c LENGTH 40,
         eudr                   TYPE c LENGTH 4,
         icone                  TYPE c LENGTH 4,
         docnum_ligacao	        TYPE j_1bnflin-docnum,
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

         "Dados Form. Lote Ini
         docnum_rfl             TYPE j_1bnfdoc-docnum,
         id_vinc                TYPE zid_vinc,
         nfenum_rfl             TYPE j_1bnfdoc-nfenum,
         qtd_nf_rfl             TYPE j_1bnflin-menge, "//wbarbosa 22112024 US-158263
         cct_rfl                TYPE c LENGTH 4,
         peso_cct_rfl           TYPE zlest0146-peso_aferido_recepcao,
         dt_cct_rfl             TYPE zlest0146-dt_recepcao,
         term_cct_rfl           TYPE zsdt0168-lifnr,
         ds_term_cct_rfl        TYPE lfa1-name1,
         terminal_rfl           TYPE lfa1-lifnr,
         ds_terminal_rfl        TYPE lfa1-name1,
         chave_nfe_rfl          TYPE zib_nfe_dist_itm-chave_nfe,
         vinculada_xml_rfl      TYPE c LENGTH 4,
         cancel_rfl             TYPE c LENGTH 4,

         conf_cct_portal_rfl    TYPE c LENGTH 4,
         dt_recepcao_portal_rfl TYPE zlest0186-dt_recepcao,
         term_cct_portal_rfl    TYPE zsdt0168-lifnr,
         ds_term_cct_portal_rfl TYPE lfa1-name1,
         saldo_exportar_rfl     TYPE j_1bnflin-menge,
         qtd_nf_export          TYPE j_1bnflin-menge, "//wbarbosa 22112024 US-158263

*"//WBARBOSA 18112024 US-158263
         docnum_retorno         TYPE j_1bnfdoc-docnum,
         data_retorno           TYPE j_1bdocdat,
         nf_retorno             TYPE zsdt_retlote-nf_retorno,
         chave_retorno          TYPE zib_nfe_forn-nu_chave,
         qtd_nf_retorno         TYPE j_1bnflin-menge,
         finalidade             TYPE val_text,
*"//WBARBOSA 18112024 US-158263

         "Dados Form. Lote Fim

         ent_prop               TYPE c LENGTH 4,
         id_due                 TYPE zsdt0170-id_due,
         numero_due             TYPE zsdt0170-numero_due,
         chave_acesso           TYPE zsdt0170-chave_acesso,
         dt_due                 TYPE zsdt0170-dt_registro,
         qtde_vinc_due          TYPE zsdt0173-peso_liq_total,
         id_due_ret             TYPE zsdt0170-id_due,
         docnum_exp             TYPE j_1bnfdoc-docnum,
         nfenum_exp             TYPE j_1bnfdoc-nfenum,
         fatura_id              TYPE zsdt0172-fatura_id,
         navio                  TYPE znom_transporte-ds_nome_transpor,
         saldo_exportar         TYPE zsdt0173-peso_liq_total,
         qtde_baixada           TYPE zsdt0276-menge,
         chave_nfe              TYPE zib_nfe_dist_itm-chave_nfe,
         chave_nfe_ref          TYPE zib_nfe_dist_itm-chave_nfe,
         und_trib_xml           TYPE zib_nfe_dist_itm-prod_und_trib,
         ncm_xml                TYPE zib_nfe_dist_itm-prod_ncm,
         qcom_xml               TYPE zib_nfe_dist_itm-prod_qtd_comerci,
         cfop_xml               TYPE zib_nfe_dist_itm-prod_cfop,
         restricao              TYPE c LENGTH 150,
         color                  TYPE   kkblo_specialcol OCCURS 0,
         cancel                 TYPE j_1bnfdoc-cancel,
         instrucao              TYPE zsdt0053-instrucao,
         saldo_devolucao        TYPE zsdt0173-peso_liq_total. "US #131067 - MMSILVA - 10.07.2025
TYPES:                 END OF ty_saida_0100.


TYPES: BEGIN OF ty_saida_0200,
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
         cancel                 TYPE j_1bnfdoc-cancel.
TYPES: END OF ty_saida_0200.



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
         chave_nfe TYPE zib_nfe_dist_itm-chave_nfe,
         cancel    TYPE j_1bnfdoc-cancel.
TYPES: END OF ty_j_1bnfdoc.

TYPES: BEGIN OF y_produtor,
         vbeln            TYPE zdoc_nf_produtor-vbeln,
         docnum_prod      TYPE zdoc_nf_produtor-docnum_prod,
         itmnum_prod      TYPE zdoc_nf_produtor-itmnum_prod,
         grp_retorno      TYPE zdoc_nf_produtor-grp_retorno,
         id_nomeacao_tran TYPE zdoc_nf_produtor-id_nomeacao_tran,
       END OF y_produtor,

       BEGIN OF y_vbfa,
         vbelv TYPE vbfa-vbelv,
         vbeln TYPE j_1bnflin-refkey, "VBFA-VBELN,
         posnn TYPE vbfa-posnn,
       END OF y_vbfa,

       BEGIN OF y_lin,
         docnum TYPE j_1bnflin-docnum,
         refkey TYPE j_1bnflin-refkey,
         refitm TYPE zdoc_nf_produtor-itmnum_prod,
       END OF y_lin,

       BEGIN OF y_nfdoc,
         docnum TYPE j_1bnfdoc-docnum,
         cancel TYPE j_1bnfdoc-cancel,
       END OF y_nfdoc,

       BEGIN OF y_doc,
         docnum  TYPE j_1bnfdoc-docnum,
         regio   TYPE j_1bregio,
         nfyear  TYPE j_1byear,
         nfmonth TYPE j_1bmonth,
         stcd1   TYPE j_1bstcd1,
         model   TYPE j_1bmodel,
         serie   TYPE j_1bseries,
         nfnum9  TYPE j_1bnfnum9,
         docnum9 TYPE j_1bdocnum9,
         cdv     TYPE j_1bcheckdigit,
       END OF y_doc,

       BEGIN OF y_active,
         docnum  TYPE j_1bnfdoc-docnum,
         regio   TYPE  j_1bregio,
         nfyear  TYPE j_1byear,
         nfmonth TYPE j_1bmonth,
         stcd1   TYPE j_1bstcd1,
         model   TYPE j_1bmodel,
         serie   TYPE j_1bseries,
         nfnum9  TYPE j_1bnfnum9,
         docnum9 TYPE j_1bdocnum9,
         cdv     TYPE j_1bcheckdigit,
       END OF y_active,

       BEGIN OF ty_field,
         fieldname TYPE char30,
       END OF ty_field,

       BEGIN OF ty_zsdt0276,
         docnum TYPE zsdt0276-docnum,
         itmnum TYPE zsdt0276-itmnum,
         menge  TYPE zsdt0276-menge,
       END OF ty_zsdt0276,

       BEGIN OF type_retlote,
         docnum       TYPE zsdt_retlote-docnum,
         nfenum       TYPE zsdt_retlote-nfenum,
         werks        TYPE zsdt_retlote-werks,
         nf_retorno   TYPE zsdt_retlote-nf_retorno,
         docnum_ret   TYPE zsdt_retlote-docnum_ret,
         quant_vinc   TYPE zsdt_retlote-quant_vinc,
         data_criacao TYPE zsdt_retlote-data_criacao,
         id_export    TYPE zsdt_retlote-id_export, "81360
       END   OF type_retlote.

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

CLASS lcl_event_handler_0100 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0100 TYPE REF TO lcl_alv_toolbar_0100.

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

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100           TYPE TABLE OF ty_saida_0100,
      wa_saida_0100           TYPE ty_saida_0100,
      it_saida_0200           TYPE TABLE OF ty_saida_0200,

      gt_docnum_log           TYPE TABLE OF zsdt_docnum_log,
      gt_retlote              TYPE TABLE OF type_retlote,
      gt_export               TYPE TABLE OF zsdt_export,
      gt_sumret               TYPE TABLE OF type_retlote,
      gs_sumret               TYPE type_retlote,
      gt_nom_remetente        TYPE TABLE OF znom_remetente,

      gt_doc_ret              TYPE TABLE OF ty_j_1bnfdoc,
      gt_active_ret           TYPE TABLE OF j_1bnfe_active,
      gt_dd07t                TYPE TABLE OF dd07t,

      it_saida_0100_aux       TYPE TABLE OF ty_saida_0100,
      it_saida_0100_ret       TYPE TABLE OF ty_saida_0100,
      tg_j_1bnfdoc            TYPE TABLE OF ty_j_1bnfdoc WITH HEADER LINE,
      tg_j_1bnfdoc_2          TYPE TABLE OF ty_j_1bnfdoc WITH HEADER LINE,
      tg_j_1bnfe_active       TYPE TABLE OF j_1bnfe_active WITH HEADER LINE,
      tg_zib_nfe_dist_itm     TYPE TABLE OF zib_nfe_dist_itm WITH HEADER LINE,
      tg_j_1bnfdoc_aux        TYPE TABLE OF ty_j_1bnfdoc WITH HEADER LINE,
      tg_lfa1                 TYPE TABLE OF lfa1       WITH HEADER LINE,
      tg_kna1                 TYPE TABLE OF kna1       WITH HEADER LINE,
      tg_makt                 TYPE TABLE OF makt       WITH HEADER LINE,
      tg_zsdt0170             TYPE TABLE OF zsdt0170   WITH HEADER LINE,
      tg_zsdt0170_ret         TYPE TABLE OF zsdt0170   WITH HEADER LINE,
      tg_zsdt0173             TYPE TABLE OF zsdt0173   WITH HEADER LINE,
      tg_zsdt0172             TYPE TABLE OF zsdt0172   WITH HEADER LINE,
      tg_zsdt0168             TYPE TABLE OF zsdt0168   WITH HEADER LINE,
      tg_znom_reme_notas_vinc TYPE TABLE OF znom_reme_notas WITH HEADER LINE,
      t_field                 TYPE TABLE OF ty_field,
      t_value                 TYPE TABLE OF rgsb4,
      tg_zlest0146            TYPE TABLE OF zlest0146  WITH HEADER LINE,
      tg_zlest0147            TYPE TABLE OF zlest0147  WITH HEADER LINE,
      tg_zlest0186            TYPE TABLE OF zlest0186  WITH HEADER LINE,
      tg_zsdt0276             TYPE TABLE OF zsdt0276   WITH HEADER LINE,
      tg_zsdt0276_aux         TYPE TABLE OF zsdt0276   WITH HEADER LINE,
      tg_zsdt0276_tot         TYPE TABLE OF ty_zsdt0276 WITH HEADER LINE,
      tg_znom_transporte      TYPE TABLE OF znom_transporte WITH HEADER LINE,
      tg_znom_reme_notas      TYPE TABLE OF znom_reme_notas WITH HEADER LINE,
      tg_j_1bbranch           TYPE TABLE OF j_1bbranch WITH HEADER LINE,
      tg_nf_produtor          TYPE TABLE OF zdoc_nf_produtor,
      tg_nf_produtor_algodao  TYPE TABLE OF zdoc_nf_produtor,
      tg_vbfa                 TYPE TABLE OF y_vbfa,
      tg_nfoc                 TYPE TABLE OF y_nfdoc,
      tg_lin                  TYPE TABLE OF y_lin,
      tg_nfdoc                TYPE TABLE OF y_nfdoc,
      tg_active               TYPE TABLE OF y_active,
      lv_pos_col              TYPE lvc_colpos,
      user_full               TYPE c LENGTH 1.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao TYPE c LENGTH 20,
      qtde_total  TYPE zsdt0173-peso_liq_total,
      var_answer  TYPE c.


*-------------------------------------------------------------------
* Ranges
*-------------------------------------------------------------------

RANGES: r_model FOR j_1bnfdoc-model.

DATA: r_cfops  TYPE TABLE OF lxhme_range_c10.
DATA: r_docnum TYPE rsis_t_range.
DATA: lt_docnum_eudr TYPE zsds092_t.
DATA: r_docnum_aux TYPE rsis_t_range.
DATA: r_eudr TYPE RANGE OF zeudr.

DATA: gt_flote_vinc TYPE zdesd_formacao_lote.
DATA: gt_flote_vinc_log TYPE zdesd_formacao_lote.



*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_novo           TYPE c VALUE 'NOVO'         LENGTH 4.

*&--------------------------------------------------------------------&*
*& SHDB                                                               &*
*&--------------------------------------------------------------------&*
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata LIKE LINE OF ti_bdcdata,
      tl_bdc     TYPE TABLE OF bdcdata,
      wl_bdc     TYPE bdcdata,
      opt        TYPE ctu_params.

* US #179396 - MMSILVA - 20.05.2025 - Inicio
*&--------------------------------------------------------------------&*
*& Layout                                                             &*
*&--------------------------------------------------------------------&*
DATA: rs_variant       LIKE disvariant.
* US #179396 - MMSILVA - 20.05.2025 - Fim


*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*

*"// wbarbosa 18112024 US-158263
SELECTION-SCREEN: BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-003.

  PARAMETERS: p_ccfe AS CHECKBOX DEFAULT abap_true,  "// Compra com Fim Especifico
              p_come AS CHECKBOX,                    "// Comercialização
              p_tran AS CHECKBOX,                    "// Transferencia
              p_eudr AS CHECKBOX.                    "// EUDR

SELECTION-SCREEN: END OF BLOCK b0.
*"// wbarbosa 18112024 US-158263

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001. "Identificação

  SELECT-OPTIONS: p_bukrs    FOR j_1bnfdoc-bukrs NO INTERVALS," OBLIGATORY, "// wbarbosa 18112024 US-158263
                  p_branch   FOR j_1bnfdoc-branch," OBLIGATORY, "// wbarbosa 18112024 US-158263
                  p_matnr    FOR j_1bnflin-matnr,
                  p_matkl    FOR j_1bnflin-matkl,
                  p_docdat   FOR j_1bnfdoc-docdat," OBLIGATORY, "// wbarbosa 18112024 US-158263
                  p_docnum   FOR j_1bnflin-docnum,
                  p_nf       FOR j_1bnfdoc-nfenum,
                  p_knf      FOR zib_nfe_dist_itm-chave_nfe NO INTERVALS, "" AHSS - Chamado 140376 - 18/06/2024 - Ajuste para filtrar pela chave
                  p_tp_nf    FOR znom_reme_notas-tp_nf_rem NO INTERVALS,
                  p_parid    FOR j_1bnfdoc-parid,
                  p_cdterm   FOR lfa1-lifnr,
                  p_safra    FOR j_1bnflin-charg,
                  p_instru   FOR zsdt0053-instrucao. "// wbarbosa 18112024 US-158263

*  PARAMETERS: p_cmpc AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-004.

  PARAMETERS: p_ccct RADIOBUTTON GROUP rad1,
              p_scct RADIOBUTTON GROUP rad1,
              p_ambo RADIOBUTTON GROUP rad1 DEFAULT 'X'.

SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002. "Processamento
  PARAMETERS: p_srvcct TYPE check DEFAULT ' '.
SELECTION-SCREEN: END OF BLOCK b3.

PARAMETERS p_report(10) TYPE c NO-DISPLAY.

* US #179396 - MMSILVA - 20.05.2025 - Inicio
SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-005.
  PARAMETERS: p_layout TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f_alv_variant_f4 CHANGING p_layout.
* US #179396 - MMSILVA - 20.05.2025 - Fim

*"// wbarbosa 18112024 US-158263
AT SELECTION-SCREEN.
  IF p_bukrs[] IS INITIAL OR
    p_branch[] IS INITIAL OR
    p_docdat[] IS INITIAL OR
    ( p_matnr[] IS INITIAL AND p_matkl[] IS INITIAL ).
    MESSAGE s000(z01) WITH 'Preencher todos os campos obrigatórios!' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'P_BUKRS-LOW'  OR
           'P_BRANCH-LOW' OR
           'P_DOCDAT-LOW' OR
           'P_MATNR-LOW'  OR
           'P_MATKL-LOW'.
        screen-required = 2.
      WHEN OTHERS.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.
*"// wbarbosa 18112024 US-158263

START-OF-SELECTION.

  PERFORM: f_check_user,
           f_selecionar_dados,
           f_processa_dados.

  CALL SCREEN 0100.
