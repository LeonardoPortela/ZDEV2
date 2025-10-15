*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MAD |09/06/2025 |Inclusão do campo de Check-List&*
*&                                    |de Documentação Jurídica.      &*
*&                                    |Chamado: 174343.               &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações p/ geração Contrato &*
*&                                    |Compra.                        &*
*&                                    |Chamado: 168919 2ª Parte.      &*
*&--------------------------------------------------------------------&*
**********************************************************************
* TABELAS
**********************************************************************
TABLES: vbak, zsdt0040, rsvar.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_sinte,
         vkbur         TYPE zsdt0040-vkbur,
         bezei         TYPE tvkbt-bezei,
         name1         TYPE kna1-name1,
         regional      TYPE zsdt0270-regional,
         doc_simulacao TYPE zsdt0040-doc_simulacao,
         contrato      TYPE icon_d,
         ped_venda     TYPE icon_d,
         distrato      TYPE icon_d,
         aditivos      TYPE icon_d,
         declar_rec    TYPE icon_d,
         chklstjd      TYPE char30,                  "<<<------"174343 - NMS ------>>>
         tipo_venda    TYPE char50,
         waerk         TYPE zsdt0040-waerk,
         erdat         TYPE zsdt0040-erdat,
         safra         TYPE zsdt0040-safra,
         cultura       TYPE zsdt0040-cultura,
         status_ctr    TYPE char1,
         status_ovd    TYPE char1,
         status_dtr    TYPE char1,
         status_adt    TYPE char1,
         status_dcr    TYPE char1.
TYPES: END   OF ty_sinte.

TYPES: BEGIN OF ty_anali,
         doc_simulacao TYPE zsdt0040-doc_simulacao,
         tpdoc         TYPE zsdt0310-tipo_doc,
         tipo_doc      TYPE char30, "zsdt0310-TIPO_DOC.
         ordem_saida   TYPE i,
         id_documento  TYPE zsdt0310-id_documento,
         documento     TYPE icon_d,
         assinatura    TYPE icon_d,
         tipo_assina   TYPE char20, "zsdt0310-TIPO_DOC_DIGITAL,
         status_cod    TYPE zsdt0310-status,
         status        TYPE char100,
         tipo_venda    TYPE char50,
         vkbur         TYPE zsdt0040-vkbur,
         bezei         TYPE tvkbt-bezei,
         regional      TYPE zsdt0270-regional,
         name1         TYPE kna1-name1,
         safra         TYPE zsdt0040-safra,
         cultura       TYPE zsdt0040-cultura,
         categ_adit    TYPE char20,
         seq_adit      TYPE char20,
         nr_doc_gerado TYPE zsdt0310-nr_doc_gerado,
         vbeln         TYPE zsdt0312-vbeln,
         posnr         TYPE zsdt0312-posnr,
         data          TYPE zsdt0310-data,
         hora          TYPE zsdt0310-hora,
         usname        TYPE zsdt0310-usname,
         log           TYPE icon_d,
         contrato      TYPE icon_d,
         ped_venda     TYPE icon_d,
         distrato      TYPE icon_d,
         aditivos      TYPE icon_d,
         declar_rec    TYPE icon_d,
         status_e      TYPE char1,
         status_e_ctr  TYPE char1,
         status_e_ovd  TYPE char1,
         status_e_dtr  TYPE char1,
         status_e_adt  TYPE char1,
         status_e_dcr  TYPE char1,
         safr2         TYPE char10,                  "<<<------"168919 - NMS ------>>>
         line_color    TYPE char4.
TYPES: END   OF ty_anali.

TYPES: BEGIN OF ty_regional,
         filial       TYPE zsdt0271-filial,
         cod_regional TYPE zsdt0271-cod_regional,
         regional     TYPE zsdt0270-regional.
TYPES: END OF ty_regional.

**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: gv_erro, " 29.11.2024 - RAMON - US #158242.
      g_grid                 TYPE REF TO cl_gui_alv_grid,
      g_custom_container     TYPE REF TO cl_gui_custom_container,
      g_grid_pop             TYPE REF TO cl_gui_alv_grid,
      g_custom_container_pop TYPE REF TO cl_gui_custom_container,

      c_alv_toolbarmanager   TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1            TYPE REF TO cl_gui_container,
      cl_container_95        TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id          TYPE REF TO cl_dd_document,
      picture                TYPE REF TO cl_gui_picture,
      l_graphic_conv         TYPE i,
      l_graphic_offs         TYPE i,
      graphic_size           TYPE i,
      l_graphic_xstr         TYPE xstring,
      url(255)               TYPE c,
      graphic_url(255),
      t_function             TYPE ui_functions,
      w_function             TYPE ui_func,
*
      t_zsdt0090             TYPE TABLE OF zsdt0090,
      w_zsdt0090             TYPE zsdt0090,
      t_zsdt0040             TYPE TABLE OF zsdt0040,
      w_zsdt0040             TYPE zsdt0040,
      tg_zmmt0035            TYPE TABLE OF zmmt0035,      "<<<------"168919 - NMS ------>>>
      t_zsdt0041             TYPE TABLE OF zsdt0041,
      w_zsdt0041             TYPE zsdt0041,
      t_sel_0041             TYPE TABLE OF zsdt0041,
      w_sel_0041             TYPE zsdt0041,
      t_zsdt0310             TYPE TABLE OF zsdt0310,
      w_zsdt0310             TYPE zsdt0310,
      t_zsdt0310_aux         TYPE TABLE OF zsdt0310,
      w_zsdt0310_aux         TYPE zsdt0310,
*
      t_0310_ctr             TYPE TABLE OF zsdt0310,
      w_0310_ctr             TYPE zsdt0310,
      t_0310_ovd             TYPE TABLE OF zsdt0310,
      w_0310_ovd             TYPE zsdt0310,
      t_0310_ass_ctr         TYPE TABLE OF zsdt0310,
      w_0310_ass_ctr         TYPE zsdt0310,
      t_0310_ass_ovd         TYPE TABLE OF zsdt0310,
      w_0310_ass_ovd         TYPE zsdt0310,
      t_0310_ass_pdf         TYPE TABLE OF zsdt0310,
      w_0310_ass_pdf         TYPE zsdt0310,
*
      t_zsdt0311             TYPE TABLE OF zsdt0311,
      w_zsdt0311             TYPE zsdt0311,
      t_zsdt0312             TYPE TABLE OF zsdt0312,
      w_zsdt0312             TYPE zsdt0312,
      t_zsdt0313             TYPE TABLE OF zsdt0313,
      w_zsdt0313             TYPE zsdt0313,
      t_zsdt0314             TYPE TABLE OF zsdt0314,
      w_zsdt0314             TYPE zsdt0314,
      t_doc_simulacao        TYPE TABLE OF zsd_range_docsi,
      w_doc_simulacao        TYPE zsd_range_docsi,
      t_tvkbt                TYPE TABLE OF tvkbt,
      t_kna1                 TYPE TABLE OF kna1,
      tg_lfa1                TYPE TABLE OF lfa1,           "<<<------"168919 - NMS ------>>>
      w_kna1                 TYPE kna1,
      w_tvkbt                TYPE tvkbt,
      t_regional             TYPE TABLE OF ty_regional,
      w_regional             TYPE ty_regional,
      t_tpvenda              TYPE TABLE OF rgsb4,
      w_tpvenda              TYPE rgsb4,
      t_sinte                TYPE TABLE OF ty_sinte,
      w_sinte                TYPE ty_sinte,
      t_anali                TYPE TABLE OF ty_anali,
      w_anali                TYPE ty_anali,
      t_tipodoc              TYPE TABLE OF dd07v,
      w_tipodoc              TYPE dd07v,
      t_status               TYPE TABLE OF dd07v,
      w_status               TYPE dd07v,
      tg_chklstjd            TYPE TABLE OF dd07v,
*
      t_pdf_files            TYPE zsdt_pdf_files,
      w_pdf_files            TYPE zsde_pdf_files,
      t_fieldcat             TYPE lvc_t_fcat,
      w_fieldcat             TYPE lvc_s_fcat,
      t_sort                 TYPE lvc_t_sort,
      w_sort                 TYPE lvc_s_sort,
      t_color                TYPE lvc_t_scol,
      w_color                TYPE lvc_s_scol,
      t_ucomm                TYPE TABLE OF syst_ucomm,
      w_ucomm                TYPE syst_ucomm,
      t_exctab               TYPE slis_t_extab,
      w_exctab               TYPE slis_extab,
      w_layout               TYPE lvc_s_layo,
      w_stable               TYPE lvc_s_stbl    VALUE 'XX',
      t_style                TYPE lvc_t_styl,
      w_style                TYPE lvc_s_styl,
      t_rows                 TYPE lvc_t_row,
      w_rows                 TYPE lvc_s_row,
      t_bdc                  TYPE TABLE OF bdcdata,
      w_bdc                  TYPE bdcdata,
      tg_zsdt0381            TYPE TABLE OF zsdt0381,   ""<<<------"174343 - NMS ------>>>
*
*     p_insumo             TYPE char1,
*     p_merint             TYPE char1,
*     p_sintet             TYPE char1,
*     p_analit             TYPE char1,
*
      p_vlr                  TYPE char1,
      p_manual               TYPE char1,
      p_eletronica           TYPE char1,
*
      l_pdf_xtring           TYPE xstring,
      l_qtd                  TYPE i,
      l_tabix                TYPE sy-tabix,
      l_cockpit              TYPE char02,
      l_erro                 TYPE c,
      l_pend                 TYPE c,
      l_tem_0041             TYPE c,
      l_tem_0310             TYPE c,
      l_status               TYPE icon_d,
      l_editar               TYPE c,
      l_werks                TYPE char10,
      l_lote_editado         TYPE numc10,
      l_id_documento         TYPE zsdt0310-id_documento,
      l_id_seq               TYPE zsdt0225-id_seq,
      l_resp                 TYPE c,
      ok_code                TYPE sy-ucomm,
      ok_code2               TYPE sy-ucomm,
      r_matkl_sel            TYPE RANGE OF matkl WITH HEADER LINE,
*
      zcl_util               TYPE REF TO zcl_util,
      zcl_insumos            TYPE REF TO zcl_integracao_insumos,
*
      l_sel_button           TYPE smp_dyntxt,

      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
