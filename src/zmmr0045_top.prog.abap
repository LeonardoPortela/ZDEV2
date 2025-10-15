*&---------------------------------------------------------------------*
*& Include          ZMMR0045_TOP
*&---------------------------------------------------------------------*
REPORT zmmr0045 MESSAGE-ID zsd.

************************************************************************
* tabelas
************************************************************************
TABLES: zmmt0191.

************************************************************************
* Types
************************************************************************
TYPES: BEGIN OF ty_boleta.
         INCLUDE TYPE zi_mm_boleta_compra_info.
TYPES: END OF ty_boleta.

TYPES: BEGIN OF ty_boleta_parc.
         INCLUDE TYPE zi_mm_boleta_comp_parc_info.
TYPES: END OF ty_boleta_parc.

TYPES: BEGIN OF ty_boleta_item.
         INCLUDE TYPE zi_mm_boleta_comp_it_info.
TYPES: END OF ty_boleta_item.

TYPES: BEGIN OF ty_boleta_wf.
         INCLUDE TYPE zi_mm_boleta_comp_wf_info.
TYPES: END OF ty_boleta_wf.

TYPES: BEGIN OF ty_saida,
         guid              TYPE zi_mm_boleta_compra_info-guid,
         id_boleta_compra  TYPE char10, "zi_mm_boleta_compra_info-idboletacompra,
         data_adiantamento TYPE zi_mm_boleta_compra_info-dataadiantamento,
         fornecedor        TYPE zi_mm_boleta_compra_info-fornecedor,
         royalties_valor   TYPE zi_mm_boleta_compra_info-royaltiesvalor,
         tipo_insumo       TYPE zi_mm_boleta_compra_info-desctpinsumo,
         safra             TYPE zi_mm_boleta_compra_info-safra,
         waers             TYPE zi_mm_boleta_compra_info-waers,
         condicao_negocio  TYPE zi_mm_boleta_compra_info-desccondnegocio,
         inco1             TYPE zi_mm_boleta_compra_info-inco1,
         anexos            TYPE icon_d, "zi_mm_boleta_compra_info-stanexo,
         st_workflow       TYPE char40, "icon_d, "zi_mm_boleta_compra_info-statusworkflow,
         st_solic          TYPE char40, "icon_d, "zi_mm_boleta_compra_info-stsolic,
         st_ped            TYPE char40, "icon_d, "zi_mm_boleta_compra_info-stped,
         user_solic        TYPE zi_mm_boleta_compra_info-createdby,
         user_retorno      TYPE zi_mm_boleta_compra_info-usuarioretorno,
         user_pedido       TYPE zi_mm_boleta_compra_info-usuariocriaped,
         qtd_item          TYPE zi_mm_boleta_compra_info-qtditens.
TYPES: END   OF ty_saida.

TYPES: BEGIN OF ty_saida_parc,
         guid_hd          TYPE zi_mm_boleta_comp_parc_info-guidhd,
         guid             TYPE zi_mm_boleta_comp_parc_info-guid,
         id_boleta_compra TYPE char10, "zi_mm_boleta_compra_info-idboletacompra,
         data_vencimento  TYPE zi_mm_boleta_comp_parc_info-datavencimento,
         waers            TYPE zi_mm_boleta_comp_parc_info-waers,
         valor            TYPE zi_mm_boleta_comp_parc_info-valor.
TYPES: END OF ty_saida_parc.

TYPES: BEGIN OF ty_saida_wf,
         guid_hd          TYPE zi_mm_boleta_comp_wf_info-guidhd,
         guid             TYPE zi_mm_boleta_comp_wf_info-guid,
         id_workflow      TYPE zi_mm_boleta_comp_wf_info-idworkflow,
         id_boleta_compra TYPE char10, "zi_mm_boleta_compra_info-idboletacompra,
         created_by       TYPE zi_mm_boleta_comp_wf_info-createdby,
         mensagem         TYPE zi_mm_boleta_comp_wf_info-mensagem,
         tp_usuario       TYPE char20, "zi_mm_boleta_comp_wf_info-tpusuario,
         data             TYPE sy-datum,
         hora             TYPE sy-uzeit.
TYPES: END OF ty_saida_wf.

TYPES: BEGIN OF ty_saida_item,
         guidhd             TYPE zi_mm_boleta_comp_it_info-guidhd,
         guid               TYPE zi_mm_boleta_comp_it_info-guid,
         idboletacompra     TYPE char10, "zi_mm_boleta_compra_info-idboletacompra,
         iditem             TYPE zi_mm_boleta_comp_it_info-iditem,
         matnr              TYPE zi_mm_boleta_comp_it_info-matnr,
         maktx              TYPE zi_mm_boleta_comp_it_info-maktx,
         werks              TYPE zi_mm_boleta_comp_it_info-werks,
         menge              TYPE zi_mm_boleta_comp_it_info-menge,
         meins              TYPE zi_mm_boleta_comp_it_info-meins,
         waers              TYPE zi_mm_boleta_comp_it_info-waers,
         netpr              TYPE zi_mm_boleta_comp_it_info-netwr,
         netwr              TYPE zi_mm_boleta_comp_it_info-netwr,
         localembarque      TYPE zi_mm_boleta_comp_it_info-localembarque,
         nrosolcp           TYPE zi_mm_boleta_comp_it_info-nrosolcp,
         inscockpitapro     TYPE zi_mm_boleta_comp_it_info-inscockpitapro,
         createdby          TYPE zi_mm_boleta_comp_it_info-createdby,
         createdat          TYPE zi_mm_boleta_comp_it_info-createdat,
         lastchangedby      TYPE zi_mm_boleta_comp_it_info-lastchangedby,
         lastchangedat      TYPE zi_mm_boleta_comp_it_info-lastchangedat,
         locallastchangedat TYPE zi_mm_boleta_comp_it_info-locallastchangedat,
         novo               TYPE char01,
         editado            TYPE char01,
         style              TYPE lvc_t_styl.
TYPES: END   OF ty_saida_item.

TYPES: BEGIN OF ty_tl001,
         id_boleta_compra   TYPE char10, " zi_mm_boleta_compra_info-idboletacompra,
         usuario_sol        TYPE zi_mm_boleta_compra_info-createdby,
         bukrs              TYPE zi_mm_boleta_compra_info-bukrs,
         moeda              TYPE zi_mm_boleta_compra_info-waers,
         dt_adiantamento    TYPE zi_mm_boleta_compra_info-dataadiantamento,
         dt_solicitacao     TYPE zi_mm_boleta_compra_info-dataadiantamento,
         kursf              TYPE zi_mm_boleta_compra_info-kursf,
         perc_adiantamento  TYPE zi_mm_boleta_compra_info-percadiantamento,
         tipo_insumo        TYPE zi_mm_boleta_compra_info-desctpinsumo,
         frete              TYPE zi_mm_boleta_compra_info-inco1,
         safra              TYPE zi_mm_boleta_compra_info-safra,
         cond_negocio       TYPE zi_mm_boleta_compra_info-desccondnegocio,
         fornecedor         TYPE zi_mm_boleta_compra_info-fornecedor,
         dt_pagto           TYPE zi_mm_boleta_compra_info-datavencimento,
         royalties_data     TYPE zi_mm_boleta_compra_info-royaltiesdata,
         pagto_pos_embarque TYPE zi_mm_boleta_compra_info-pagamentoposembarque,
         royalties_valor    TYPE zi_mm_boleta_compra_info-royaltiesvalor,
         observacao1        TYPE zi_mm_boleta_compra_info-observacoes,
         observacao2        TYPE zi_mm_boleta_compra_info-observacoes.
TYPES: END   OF ty_tl001.

TYPES: BEGIN OF ty_tl300,
         motivo1 TYPE zi_mm_boleta_comp_wf_info-mensagem,
         motivo2 TYPE zi_mm_boleta_comp_wf_info-mensagem.
TYPES: END   OF ty_tl300.

************************************************************************
* variaveis
************************************************************************
DATA: lv_editar               TYPE char01,
      lv_tabix                TYPE sy-tabix,
      lv_erro                 TYPE char01,
      lv_obj_key              TYPE sibflporb-instid,
      lv_icon                 TYPE icon_d,
      lc_call                 TYPE zmme0250,
      t_anexos                TYPE TABLE OF bdn_con,
*
      t_boleta                TYPE TABLE OF ty_boleta,
      t_boleta_item           TYPE TABLE OF ty_boleta_item,
      t_boleta_parc           TYPE TABLE OF ty_boleta_parc,
      t_boleta_wf             TYPE TABLE OF ty_boleta_wf,
      t_saida                 TYPE TABLE OF ty_saida,
      t_saida_parc            TYPE TABLE OF ty_saida_parc,
      t_saida_wf              TYPE TABLE OF ty_saida_wf,
      t_saida_item            TYPE TABLE OF ty_saida_item,
      t_saida_item_del        TYPE TABLE OF ty_saida_item,
      t_saida_item_cop        TYPE TABLE OF ty_saida_item,
      w_boleta_aux            TYPE ty_boleta,
      w_boleta                TYPE ty_boleta,
      w_boleta_item           TYPE ty_boleta_item,
      w_boleta_parc           TYPE ty_boleta_parc,
      w_boleta_wf             TYPE ty_boleta_wf,
      w_saida                 TYPE ty_saida,
      w_saida_parc            TYPE ty_saida_parc,
      w_saida_wf              TYPE ty_saida_wf,
      w_saida_item            TYPE ty_saida_item,
      w_saida_item_cop        TYPE ty_saida_item,
      w_zmmt0193              TYPE zmmt0193,
      w_zmmt0194              TYPE zmmt0194,
      tl001                   TYPE ty_tl001,
      tl300                   TYPE ty_tl300,
      t_idd07v                TYPE TABLE OF dd07v,
      w_idd07v                TYPE dd07v,
      t_bdc                   TYPE TABLE OF bdcdata,

*
      g_manager               TYPE REF TO cl_gos_manager,
      g_grid                  TYPE REF TO cl_gui_alv_grid,
      g_custom_container      TYPE REF TO cl_gui_custom_container,
      g_grid_parc             TYPE REF TO cl_gui_alv_grid,
      g_custom_container_parc TYPE REF TO cl_gui_custom_container,
      g_grid_item             TYPE REF TO cl_gui_alv_grid,
      g_custom_container_item TYPE REF TO cl_gui_custom_container,
      g_grid_wf               TYPE REF TO cl_gui_alv_grid,
      g_custom_container_wf   TYPE REF TO cl_gui_custom_container,
*
      c_alv_toolbarmanager    TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1             TYPE REF TO cl_gui_container,
      cl_container_95         TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id           TYPE REF TO cl_dd_document,
      picture                 TYPE REF TO cl_gui_picture,
      l_graphic_conv          TYPE i,
      l_graphic_offs          TYPE i,
      graphic_size            TYPE i,
      l_graphic_xstr          TYPE xstring,
      url(255)                TYPE c,
      graphic_url(255),
      t_function              TYPE ui_functions,
      w_function              TYPE ui_func,
      ok_code                 TYPE sy-ucomm,
      ok_code2                TYPE sy-ucomm,
      ok_code3                TYPE sy-ucomm,
      lc_object               TYPE borident,
      lc_ip_mode              TYPE sgs_rwmod,
*
      t_fieldcat              TYPE lvc_t_fcat,
      w_fieldcat              TYPE lvc_s_fcat,
      t_sort                  TYPE lvc_t_sort,
      w_sort                  TYPE lvc_s_sort,
      t_color                 TYPE lvc_t_scol,
      w_color                 TYPE lvc_s_scol,
      t_ucomm                 TYPE TABLE OF syst_ucomm,
      w_ucomm                 TYPE syst_ucomm,
      t_exctab                TYPE slis_t_extab,
      w_exctab                TYPE slis_extab,
      w_layout                TYPE lvc_s_layo,
      w_layout_parc           TYPE lvc_s_layo,
      w_layout_wf             TYPE lvc_s_layo,
      w_layout_item           TYPE lvc_s_layo,
      w_stable                TYPE lvc_s_stbl    VALUE 'XX',
      t_style                 TYPE lvc_t_styl,
      w_style                 TYPE lvc_s_styl,
      t_rows                  TYPE lvc_t_row,
      t_rows_head             TYPE lvc_t_row,
      w_rows                  TYPE lvc_s_row,
*
      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

************************************************************************
* ranges
************************************************************************
RANGES: r_dtsol      FOR zmmt0191-created_at.

************************************************************************
* parametros entrada
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_idbol   FOR zmmt0191-id_boleta_compra,
                  s_bukrs   FOR zmmt0191-bukrs             NO-EXTENSION NO INTERVALS OBLIGATORY,
                  s_dtsol   FOR zmmt0191-data_adiantamento NO-EXTENSION              OBLIGATORY,
                  s_tpins   FOR zmmt0191-tipo_insumo       NO-EXTENSION NO INTERVALS OBLIGATORY,
                  s_safra   FOR zmmt0191-safra,
                  s_moeda   FOR zmmt0191-waers,
                  s_status  FOR zmmt0191-status_workflow,
                  s_fornec  FOR zmmt0191-fornecedor.
  PARAMETERS    : p_submit TYPE char01                     NO-DISPLAY.
SELECTION-SCREEN END   OF BLOCK b1.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
