*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*

**********************************************************************
* TABELAS
**********************************************************************
TABLES: zlest0210, vbak, vbap, ekko, j_1bnfdoc.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_alv.
         INCLUDE TYPE zlest0209_out.
         INCLUDE TYPE zlest0210_out.
TYPES: END   OF ty_alv.

*TYPES: BEGIN OF ty_alv_transf.
*         INCLUDE TYPE zlest0209_out.
*         INCLUDE TYPE zlest0210_out.
*       TYPES: END   OF ty_alv_transf.

TYPES: BEGIN OF ty_alv_transf,
         ped_ebeln 	  TYPE  ekko-ebeln,      "( Nr. do pedido)
         ped_data  	  TYPE  ekko-aedat,      "(data pedido )
         ped_bsart 	  TYPE  ekko-bsart,      "(tipo pedido)
         ped_reswr 	  TYPE  ekko-reswk,      "(filial Fornecedora)
         ped_entrega  TYPE  lfa1-lifnr,      "(filial Fornecedora)
         rem_vbeln    TYPE  lips-vbeln,      "( remessa )
         rem_lfdat    TYPE  likp-lfdat,      "( data remessa )  "*-CS2024000522-12.09.2024-JT-#151251-inicio
         rem_posnr    TYPE  lips-posnr,      "(item remessa)
         rem_matnr    TYPE  lips-matnr,      "(material)
         rem_gp_matnr TYPE  lips-matkl,      "(grupo do material)
         rem_lgort    TYPE  lips-lgort,      "(deposito)
         rem_charg    TYPE  lips-charg,      "(Nr do lote)
         rem_vgbel    TYPE  lips-vgbel,      "(nr. pedido)
         rem_vgpos    TYPE  lips-vgpos,      "(item do pedido)
         rem_vgtyp    TYPE  lips-vgtyp,      "(Categoria de documento)
         rem_inco1    TYPE  likp-inco1,      "(incoterms)
         rem_route    TYPE  likp-route,      "(código Itinerário)
         rem_kunnr    TYPE  likp-kunnr,     "(cliente Pedido)
         rem_brgew    TYPE  likp-btgew,     "(quantidade)
         rem_gewei    TYPE  likp-gewei,     "(unidade de medida)
         rem_tcode    TYPE  likp-tcode,     "(Tcode do registro)
         rem_data     TYPE  likp-erdat,     "(data de remessa)
         rem_filial   TYPE  likp-vstel,     "(Filial de Saída )
         rem_fat      TYPE  ekbe-belnr,     "(nr do faturamento)
         rem_fat_ano  TYPE  ekbe-gjahr,     "(ano do documento de faturamento)
         ped_coleta   TYPE  ekpa-lifn2,     "(parceiro local de coleta )
         ped_ebelp    TYPE  ekpo-ebelp,     "(item do pedido)
         ped_matnr    TYPE  ekpo-matnr,     "(material)
         ped_bukrs    TYPE  ekpo-bukrs,
         ped_menge    TYPE  ekpo-menge,     "(Quantidade do pedido)
         ped_meins    TYPE  ekpo-meins,     "(unidade de medida)
         ped_icon1    TYPE  ekpo-inco1,     "(incoterms)
         ped_wekrs    TYPE  ekpo-werks,     "(centro recebedor da transferência)
         ped_lgort    TYPE  ekpo-lgort,     "(deposito)
         ped_gp_matnr TYPE  ekpo-matkl,     "(grupo do material)
         ped_route    TYPE  ekpv-route,     "(código Itinerário)
         ped_charg    TYPE  eket-charg,     "(lote)

         rem_doc_nf   TYPE  j_1bnflin-docnum,
         rem_nr_nf    TYPE  j_1bnfdoc-nfenum,

         base         TYPE  j_1bnfstx-base,
         taxval       TYPE  j_1bnfstx-taxval,
         chave_nf(44) TYPE c,

         frete_tknum  TYPE vttp-tknum,
         frete_shtyp  TYPE vttk-shtyp,   "(tipo de transporte)
         frete_placa  TYPE vttk-text1,   "(pegar somente os 7 primeiros caracteres)
         frete_agente TYPE vttk-tdlnr,   "(agente de frete)
         frete_custo  TYPE vfkp-fknum ,  "(documento de custo)

         frete_ov     TYPE vbak-vbeln,   "(Ov de serviço)
         frete_fatura TYPE vbfa-vbeln,   "( fatura de serviço)
         rem_doc_cte  TYPE j_1bnflin-docnum,

         ag_frete     TYPE lfa1-lifnr,
         dados_transp TYPE char5,
         placa        TYPE zplaca,
         quantidade   TYPE lips-lfimg,
         itinerario   TYPE likp-route,
         vlr_frete    TYPE dzwert,
         unid_cond    TYPE konwa,
         tp_frete     TYPE zde_tp_frete,

         ebeln_ic     TYPE char10,
         transp       TYPE char10,
         doc_custo    TYPE char10,
         ov_serv      TYPE char10,
         fat_serv     TYPE char10,
         dacte        TYPE char10,
         cellstyles   TYPE lvc_t_styl.
TYPES:       END OF ty_alv_transf.

TYPES: BEGIN OF ty_vbfa.
         INCLUDE STRUCTURE vbfa.
TYPES:   refkey TYPE j_1bnflin-refkey.
TYPES: END   OF ty_vbfa.

TYPES: BEGIN OF ty_jlin,
         docnum    TYPE j_1bnfdoc-docnum,
         nfenum    TYPE j_1bnfdoc-nfenum,
         parid     TYPE j_1bnfdoc-parid,
         stains    TYPE j_1bnfdoc-stains,
         refkey    TYPE j_1bnflin-refkey,
         brgew     TYPE j_1bnfdoc-brgew,  "*-CS2024000522-18.07.2024-JT-#143588
         cfop      TYPE j_1bnflin-cfop,   "*-CS2024000522-18.07.2024-JT-#143588
         docdat    TYPE j_1bnfdoc-docdat, "*-CS2024000522-10.09.2024-JT-#151259
         chave_nfe TYPE zde_chave_nfe.
TYPES: END   OF ty_jlin.

TYPES: BEGIN OF ty_chave_nfe,
         chave_nfe TYPE zde_chave_nfe.
TYPES: END   OF ty_chave_nfe.

TYPES: BEGIN OF ty_lips.
         INCLUDE STRUCTURE lips.
TYPES:   xblnr TYPE ekbe-xblnr.
TYPES: END   OF ty_lips.

TYPES: BEGIN OF ty_ekbe,
         ebeln  TYPE ekbe-ebeln,
         ebelp  TYPE ekbe-ebelp,
         belnr  TYPE ekbe-belnr,
         gjahr  TYPE ekbe-gjahr,
         xblnr  TYPE ekbe-xblnr,
         refkey TYPE j_1bnflin-refkey.
TYPES: END   OF ty_ekbe.

DATA: BEGIN OF w_log_erros,
        type          TYPE bapi_mtype,
        number        TYPE symsgno,
        ch_referencia LIKE zsdt0001-ch_referencia,
        mensagem      TYPE bapi_msg,
      END OF w_log_erros.
**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      picture              TYPE REF TO cl_gui_picture,
      l_graphic_conv       TYPE i,
      l_graphic_offs       TYPE i,
      graphic_size         TYPE i,
      l_graphic_xstr       TYPE xstring,
      url(255)             TYPE c,
      graphic_url(255),
      t_function           TYPE ui_functions,
      w_function           TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_alv                TYPE TABLE OF ty_alv,
      w_alv                TYPE ty_alv,
      t_alv_transf         TYPE TABLE OF ty_alv_transf,
      w_alv_transf         TYPE ty_alv_transf,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
*
      t_vbak               TYPE TABLE OF vbak,
      w_vbak               TYPE vbak,
      t_vbfa               TYPE TABLE OF ty_vbfa,
      w_vbfa               TYPE ty_vbfa,
      t_jlin               TYPE TABLE OF ty_jlin,
      w_jlin               TYPE ty_jlin,
      t_set                TYPE TABLE OF rgsb4,
      w_set                TYPE rgsb4,
      t_tvarvc             TYPE TABLE OF tvarvc,
      w_tvarvc             TYPE tvarvc,
      t_vbeln              TYPE zlest0212_tab,
      w_vbeln              TYPE zlest0212_out,
      t_vbeln_transf       TYPE zlest0214_tab,
      w_vbeln_transf       TYPE zlest0214_out,
      t_chave_nfe          TYPE TABLE OF ty_chave_nfe,
      w_chave_nfe          TYPE ty_chave_nfe,
      w_campos_nfe         TYPE zde_campos_nfe,
      t_zlest0210          TYPE TABLE OF zlest0210,
      w_zlest0210          TYPE zlest0210,
*
      l_tabix              TYPE sy-tabix,
*---> 20.06.2023 - Migração S4 - DG
      "      l_cockpit            TYPE char02,
      l_cockpit            TYPE zchar02,
*<--- 20.06.2023 - Migração S4 - DG
      l_dacte              TYPE j_1bdocnum,
      l_form_saida         TYPE char30,
      l_chave_nf_venda     TYPE zde_chave_nfe,
      l_nf_remessa         TYPE j_1bnfnum9,
      l_lock_ag_frete      TYPE char1,
      ok_code              TYPE sy-ucomm,

      "t_saida_03           TYPE TABLE OF ty_saida_03,
      "w_saida_03           TYPE  ty_saida_03,
      t_ekko               TYPE TABLE OF ekko,
      w_ekko               TYPE ekko,
      t_lips               TYPE TABLE OF ty_lips,
      t_lips_aux           TYPE TABLE OF ty_lips,
      w_lips               TYPE ty_lips,
      t_likp               TYPE TABLE OF likp,
      w_likp               TYPE likp,
      t_ekbe_aux           TYPE TABLE OF ekbe,
      w_ekbe_aux           TYPE ekbe,
      t_ekbe               TYPE TABLE OF ty_ekbe,
      w_ekbe               TYPE ty_ekbe,
      t_ekpa               TYPE TABLE OF ekpa,
      w_ekpa               TYPE ekpa,
      t_ekpo               TYPE TABLE OF ekpo,
      w_ekpo               TYPE ekpo,
      t_ekpv               TYPE TABLE OF ekpv,
      w_ekpv               TYPE ekpv,
      t_eket               TYPE TABLE OF eket,
      w_eket               TYPE  eket,
      t_zsdt0011           TYPE TABLE OF zsdt0011,
      w_zsdt0011           TYPE zsdt0011,
      t_j_1bnflin          TYPE TABLE OF j_1bnflin,
      w_j_1bnflin          TYPE j_1bnflin,
      t_j_1bnfdoc          TYPE TABLE OF j_1bnfdoc,
      w_j_1bnfdoc          TYPE j_1bnfdoc,
      t_j_1bnfstx          TYPE TABLE OF j_1bnfstx,
      w_j_1bnfstx          TYPE j_1bnfstx,
      t_j_1bnfe_active     TYPE TABLE OF j_1bnfe_active,
      w_j_1bnfe_active     TYPE  j_1bnfe_active,
      t_vttp               TYPE TABLE OF vttp,
      w_vttp               TYPE  vttp,
      t_vttk               TYPE TABLE OF vttk,
      w_vttk               TYPE  vttk,
      t_vfkp               TYPE TABLE OF vfkp,
      w_vfkp               TYPE vfkp,

      t_itens              TYPE bapidlvreftosto OCCURS 0 WITH HEADER LINE,
      w_itens              TYPE bapidlvreftosto,
      t_retorno            TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
      t_retorno2           TYPE TABLE OF bapiret2 INITIAL SIZE 0 WITH HEADER LINE,
      nlinhas              TYPE i,
      t_items              TYPE bapishpdelivnumb OCCURS 0 WITH HEADER LINE,
      zcl_util             TYPE REF TO zcl_util,
      t_dados_remessa      TYPE zzsdt_dados_remessa.


DATA: vg_ebeln TYPE ebeln,
      vg_ebelp TYPE ebelp,
      vg_bsart TYPE bsart.

DATA: w_header_data    TYPE bapiobdlvhdrchg,
      w_header_control TYPE bapiobdlvhdrctrlchg,
      header_partner   TYPE TABLE OF bapidlvpartnerchg INITIAL SIZE 0 WITH HEADER LINE,
      item_data        TYPE TABLE OF bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE,
      item_control     TYPE TABLE OF bapiobdlvitemctrlchg INITIAL SIZE 0 WITH HEADER LINE,
      item_data_spl    TYPE TABLE OF /spe/bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE.

DATA: sl_vbkok_wa TYPE vbkok,
      sl_vbpok    TYPE vbpok,
      tl_vbpok    TYPE TABLE OF vbpok,
      tl_prot     TYPE TABLE OF prott,
      sl_prot     TYPE prott.

DATA:  t_log_warni  LIKE STANDARD TABLE OF w_log_erros.

DATA: t_return TYPE TABLE OF bapiret2,
      w_return TYPE bapiret2.

DATA: sl_hdata    TYPE bapiobdlvhdrchg,
      sl_hcont    TYPE bapiobdlvhdrctrlchg,
      vl_delivery TYPE bapiobdlvhdrchg-deliv_numb,
      tl_bapiret2 TYPE bapiret2_t.

DATA: results_received(1)          TYPE c,
      error_reverse_goods_issue(1) TYPE c,
      it_mesg                      TYPE STANDARD TABLE OF mesg.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

*-CS2024000522-26.08.2024-JT-#147087-inicio
TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END   OF ty_estrutura.

DATA: lt_sort      TYPE lvc_t_sort,
      wa_sort      LIKE LINE OF lt_sort,
      tl_bdc       TYPE TABLE OF bdcdata,
      wl_bdc       TYPE bdcdata,
      it_fieldcat  TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      wa_fieldcat  TYPE ty_estrutura,
      ls_variant   TYPE disvariant,
      l_grid_title TYPE lvc_title.
*-CS2024000522-26.08.2024-JT-#147087-fim

RANGES:
      r_cfop            FOR j_1bnflin-cfop,
      r_matkl           FOR j_1bnflin-matkl.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
