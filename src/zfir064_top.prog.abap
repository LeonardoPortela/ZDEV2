*&---------------------------------------------------------------------*
*&  Include           ZFIR064_TOP
*&---------------------------------------------------------------------*


report zfir064.

tables: t001, t001l, mara, mchb, kna1, zsdt_export, zlest0146, zsdt0002.

types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

types: begin of ty_saida_0100,
         matnr    type mara-matnr,
         werks    type t001l-werks,
         lgort    type t001l-lgort,
         charg    type mchb-charg,
         sld_e    type mard-labst,
         saldo_nf TYPE zde_nota_retorno_rfl-saldo_nf, "US 142076 - MMSILVA - 27.11.2024
         sld_d    type mard-labst.
types end of ty_saida_0100.

data: begin of tg_estoque_mb52 occurs 0,
        matnr like mara-matnr,
        werks like t001w-werks,
        lgort like mard-lgort,
        charg like mchb-charg,
        labst like mard-labst.
data:  end of tg_estoque_mb52.

data: begin of tg_estoque_zsdt0034 occurs 0,
        branch    type j_1bnfdoc-branch,
        docnum    type j_1bnfdoc-docnum,
        nfenum    type j_1bnfdoc-nfenum,
        doc_ret   type j_1bnfdoc-docnum,
        doc_exp   type j_1bnfdoc-docnum,
        matnr     type j_1bnflin-matnr,
        charg     type j_1bnflin-charg,
        saldo     type ccm_quant,
        parid_nad type j_1bnfnad-parid,
        werks_v   like zsdt_depara_cen-centrov_1,
        lgort     like mard-lgort,
        lgort_t   like mard-lgort.
data:  end of tg_estoque_zsdt0034.

* US 142076 // MMSILVA - 31.10.2024 - Inicio
data: begin of tg_estoque_zsdt0163 occurs 0,
        branch   type zde_nota_retorno_rfl-branch,
        docnum   type zde_nota_retorno_rfl-docnum,
        nfenum   type zde_nota_retorno_rfl-nfenum,
*         DOC_RET    TYPE J_1BNFDOC-DOCNUM,
*         DOC_EXP    TYPE J_1BNFDOC-DOCNUM,
        matnr    type zde_nota_retorno_rfl-matnr,
        charg    type zde_nota_retorno_rfl-charg,
        saldo_nf type zde_nota_retorno_rfl-saldo_nf,
        parid    type zde_nota_retorno_rfl-parid,
        werks_d  like zsdt0023-werks_v,
        lgort    like zde_nota_retorno_rfl-lgort,
        lgort_d  like zsdt0023-lgort_v,
      end of tg_estoque_zsdt0163.
* US 142076 // MMSILVA - 31.10.2024 - Fim

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

class lcl_alv_toolbar_0100 definition.
  public section.
*Constructor
    methods: constructor
      importing io_alv_grid type ref to cl_gui_alv_grid,
*Event for toolbar
      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm.

endclass.                    "LCL_ALV_TOOLBAR DEFINITION

class lcl_event_handler_0100 definition.
  public section.

    class-methods:
      catch_hotspot for event hotspot_click of cl_gui_alv_grid
        importing e_row_id e_column_id es_row_no.
endclass.

data: obj_alv_0100       type ref to cl_gui_alv_grid,
      obj_container_0100 type ref to cl_gui_custom_container.

data: gt_catalog type lvc_t_fcat,
      gw_catalog type lvc_s_fcat.

data: obj_toolbar_0100 type ref to lcl_alv_toolbar_0100.

* ALV field catalogs
data: it_fcat type lvc_t_fcat,
      wa_fcat type lvc_s_fcat.

* ALV excluded functions
data: it_exclude_fcode type ui_functions,
      wa_exclude_fcode like line of it_exclude_fcode.

* Alv Styles
data: ls_edit type lvc_s_styl,
      lt_edit type lvc_t_styl.

* ALV layout variant
data: gs_variant       type disvariant.

* ALV layout
data: gs_layout        type lvc_s_layo.

* ALV Stable
data: wa_stable        type lvc_s_stbl.

data: it_selectedcell type lvc_t_cell,
      wa_selectedcell type lvc_s_cell.

data: it_sel_rows type lvc_t_row,
      wa_sel_rows type lvc_s_row.

data: gt_estilo type lvc_t_styl with header line,
      wl_estilo type lvc_s_styl.

data: gt_f4  type lvc_t_f4 with header line.

* Objetos
data: c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      ty_toolbar           type stb_button.

data: wa_estrutura type ty_estrutura,
      estrutura    type table of ty_estrutura.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
data: it_saida_0100       type table of ty_saida_0100,
      wa_saida_0100       type ty_saida_0100,
      tg_zsdt_depara_depo type table of zsdt_depara_depo with header line,
      tg_zsdt_depara_cen  type table of zsdt_depara_cen  with header line.


field-symbols: <lt_data>      type any table,
               <lt_data_line> type any table,
               <ls_data>      type any,
               <ls_data_line> type any.

data: lr_data            type ref to data,
      lr_data_line       type ref to data,
      lr_data_descr      type ref to cl_abap_datadescr,
      lr_data_line_descr type ref to cl_abap_datadescr.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
data: vg_operacao type c length 20,
      var_answer  type c.


* US 142076 // MMSILVA - 27.11.2024 - Inicio "INSERIDO DADOS DA ZSDR0112 PARA ENCONTRAR RESULTADOS

DATA: t_dd07v TYPE TABLE OF dd07v,
      s_dd07v TYPE dd07v.

TYPES: BEGIN OF ty_saida_0100_zsdt0163,
         docnum              TYPE zde_nota_retorno_rfl-docnum,
         reftyp              TYPE zde_nota_retorno_rfl-reftyp,
         vbeln_vl            TYPE zde_nota_retorno_rfl-vbeln_vl,
         mblnr_s             TYPE mkpf-mblnr,
         mjahr_s             TYPE mkpf-mjahr,
         werks_d             TYPE zsdt0023-werks_v,
         lgort_d             TYPE zsdt0023-lgort_v,
         credat              TYPE zde_nota_retorno_rfl-credat,
         pstdat              TYPE zde_nota_retorno_rfl-pstdat,
         docdat              TYPE zde_nota_retorno_rfl-docdat,
         nfenum              TYPE zde_nota_retorno_rfl-nfenum,
         bukrs               TYPE zde_nota_retorno_rfl-bukrs,
         branch              TYPE zde_nota_retorno_rfl-branch,
         werks               TYPE zde_nota_retorno_rfl-werks,
         matnr               TYPE zde_nota_retorno_rfl-matnr,
         menge               TYPE zde_nota_retorno_rfl-menge,
         meins               TYPE zde_nota_retorno_rfl-meins,
         netpr               TYPE zde_nota_retorno_rfl-netpr,
         netwrt              TYPE zde_nota_retorno_rfl-netwrt,
         charg               TYPE zde_nota_retorno_rfl-charg,
         lgort               TYPE zde_nota_retorno_rfl-lgort,
         lifnr_z1            TYPE zde_nota_retorno_rfl-lifnr_z1,
         lifnr_z1_cct        TYPE zde_nota_retorno_rfl-lifnr_z1,
         chave_nfe           TYPE zde_nota_retorno_rfl-chave_nfe,
         parid               TYPE zde_nota_retorno_rfl-parid,
         partyp              TYPE zde_nota_retorno_rfl-partyp,
         qtde_vinc           TYPE zde_nota_retorno_rfl-qtde_vinc,
         qtde_cct            TYPE zde_nota_retorno_rfl-qtde_cct,
         dt_recepcao_cct     TYPE zde_nota_retorno_rfl-dt_recepcao_cct,
         qtde_quebra         TYPE zde_nota_retorno_rfl-qtde_quebra,
         qtde_sobra          TYPE zde_nota_retorno_rfl-qtde_sobra,
         saldo_nf            TYPE zde_nota_retorno_rfl-saldo_nf,
         saldo_cct           TYPE zde_nota_retorno_rfl-saldo_cct,

         docnum_retorno      TYPE zfiwrt0008-docnum_retorno,
         docnum_ret_flag(20),
         nferet_quebra       TYPE j_1bnfdoc-nfenum, "nfenum_retorno
         nferet_flag(20),
         docdat_quebra       TYPE j_1bnfdoc-docdat, "docdat_retorno

         mblnr               TYPE mkpf-mblnr,
         mjahr               TYPE mkpf-mjahr,
         budat               TYPE mkpf-budat,
         mblnr_flag(20),

         seq_lcto_znfw       TYPE zfiwrt0008-seq_lcto,
         seq_lcto_flag(20),
         docnum_znfw         TYPE j_1bnfdoc-docnum,
         docnum_flag(20),
         nfenum_znfw         TYPE j_1bnfdoc-nfenum,
         nfenum_flag(20),
         docdat_znfw         TYPE j_1bnfdoc-docdat,
         mblnr_znfw          TYPE zfiwrt0008-mblnr,
         mblnr_znfw_flag(20),

         lcto_conc           TYPE char30,
         nferet_quebra_in    TYPE j_1bnfdoc-nfenum,
         nfenum_znfw_in      TYPE j_1bnfdoc-nfenum,
         del_registro        TYPE c,
         qtde_atribuida      TYPE zde_nota_retorno_rfl-saldo_nf,
         finalidade(30)      TYPE c,

         mblnr_cce           TYPE zde_nota_retorno_rfl-mblnr_cce,
         mjahr_cce           TYPE zde_nota_retorno_rfl-mjahr_cce,
         authcode            TYPE zcarta_correcao-authcode,
*         marc             TYPE char1,
         gerar_lcto          TYPE char1,
         dias                TYPE i,
         matkl               TYPE matkl,
         datatransb          TYPE zlest0039-datatransb,
         pesotransb          TYPE zlest0039-pesotransb.
TYPES:   color             TYPE   kkblo_specialcol OCCURS 0.
TYPES END OF ty_saida_0100_zsdt0163.

DATA: BEGIN OF tg_mkpf OCCURS 0,
        mblnr TYPE mkpf-mblnr,
        mjahr TYPE mkpf-mjahr,
        bktxt TYPE mkpf-bktxt,
      END OF tg_mkpf,

      BEGIN OF tg_mkpf_2 OCCURS 0,
        mblnr TYPE mkpf-mblnr,
        mjahr TYPE mkpf-mjahr,
        xblnr TYPE mkpf-xblnr,
        budat TYPE mkpf-budat,
      END OF tg_mkpf_2,

      BEGIN OF tg_mseg OCCURS 0,
        mblnr TYPE mseg-mblnr,
        mjahr TYPE mseg-mjahr,
        werks TYPE mseg-werks,
        lgort TYPE mseg-lgort,
      END OF tg_mseg,

      BEGIN OF tg_mseg_2 OCCURS 0,
        mblnr TYPE mseg-mblnr,
        mjahr TYPE mseg-mjahr,
        smbln TYPE mseg-smbln,
      END OF tg_mseg_2.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
       END OF ty_mara.

TYPES: BEGIN OF ty_notas.
         INCLUDE STRUCTURE zde_nota_retorno_rfl.
TYPES: END OF ty_notas.


*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100_zsdt0163 TYPE TABLE OF ty_saida_0100_zsdt0163,
      it_saida_copy          TYPE TABLE OF ty_saida_0100_zsdt0163,
      wa_saida_0100_zsdt0163 TYPE ty_saida_0100_zsdt0163,
      wa_saida_lc            TYPE ty_saida_0100_zsdt0163,
      wa_saida_copy          TYPE ty_saida_0100_zsdt0163,
      tg_zsdt0023            TYPE TABLE OF zsdt0023 WITH HEADER LINE,
      tg_zsdt0023_aux        TYPE TABLE OF zsdt0023 WITH HEADER LINE,
      tg_notas_rfl           TYPE zde_nota_retorno_rfl_t,
      tg_mara                TYPE TABLE OF ty_mara,
      tg_notas               TYPE TABLE OF ty_notas WITH HEADER LINE,
      tg_zlest0039           TYPE TABLE OF zlest0039,
      tg_notas_aux           TYPE TABLE OF ty_notas WITH HEADER LINE,
      tg_doc_ret             TYPE TABLE OF j_1bnfdoc       WITH HEADER LINE,
      tg_zsdt_retlote        TYPE TABLE OF zsdt_retlote    WITH HEADER LINE,
      tg_zsdt0168            TYPE TABLE OF zsdt0168        WITH HEADER LINE,
      tg_zsdt_export         TYPE TABLE OF zsdt_export     WITH HEADER LINE,
      tg_active              TYPE TABLE OF j_1bnfe_active  WITH HEADER LINE,
      tg_zfiwrt0008          TYPE TABLE OF zfiwrt0008      WITH HEADER LINE,
      t_zfiwrt0008           TYPE TABLE OF zfiwrt0008      WITH HEADER LINE,
      t_zfiwrt0009           TYPE TABLE OF zfiwrt0009      WITH HEADER LINE,
      tg_zsdt0283            TYPE TABLE OF zsdt0283,
      wg_zsdt0283            TYPE zsdt0283.

TYPES: BEGIN OF ty_result,
         lgort_d  TYPE zsdt0023-lgort_v,
         saldo_nf TYPE zde_nota_retorno_rfl-saldo_nf,
         werks_d  TYPE zsdt0023-werks_v.
TYPES:    END OF ty_result.

DATA: it_result TYPE TABLE OF ty_result.

* US 142076 // MMSILVA - 27.11.2024 - Fim


*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-001.
  parameters: p_bukrs type t001-bukrs obligatory,
              p_fkart type zsdt0002-fkart default 'ZRFL' matchcode object zsdt0002,
              p_werks type t001l-werks obligatory,
              p_lgort type t001l-lgort obligatory,
              p_matnr type mara-matnr  obligatory,
              p_charg type mchb-charg  obligatory,
              p_kunnr TYPE kna1-kunnr  obligatory.
  select-options:   p_dtem  for zlest0146-dt_recepcao no-extension obligatory.

selection-screen: end of block b1.

start-of-selection.

  perform: f_selecionar_dados,
           f_processa_dados,
           f_selecionar_dados_zsdt0163 USING '',
           f_processa_dados_zsdt0163.


  call screen 0100.
