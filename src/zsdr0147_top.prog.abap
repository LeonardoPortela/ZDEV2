*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*
REPORT zsdr0147.

**********************************************************************
* TABELAS
**********************************************************************
TABLES: t001, zsdt0225, ekko, ekpo, sscrfields, rsvar.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_saida,
         status         TYPE icon_d,
         id_seq         TYPE zsdt0225-id_seq,
         bukrs          TYPE zsdt0225-bukrs,
         bukrs_fat      TYPE zsdt0306-bukrs_fat,
         emp_fat_serv   TYPE zsdt0225-bukrs,
         werks          TYPE zsdt0225-werks,
         ebeln          TYPE ekko-ebeln,
         ebelp          TYPE ekpo-ebelp,
         matnr          TYPE zsdt0225-cod_material,
         maktx          TYPE makt-maktx,
         matnr_ov       TYPE zsdt0225-cod_material,
         matkl          TYPE mara-matkl,
         menge          TYPE ekpo-menge,
         cl_codigo      TYPE zsdt0225-cl_codigo,
         kunnr          TYPE zsdt0225-cl_codigo,
         kunnr_ori      TYPE zsdt0225-cl_codigo,
         dt_fatura      TYPE zsdt0225-dt_fatura,
         auart          TYPE zsdt0225-auart,
         waerk          TYPE zsdt0225-waerk,
         navio          TYPE zsdt0225-navio,
         local_operacao TYPE zsdt0225-local_operacao,
         netpr          TYPE zlest0061-netpr,
         tax_dolar      TYPE zsdt0225-tax_dolar,
         vlr_usd        TYPE zsdt0225-vlr_usd,
         vlr_brl        TYPE zsdt0225-vlr_brl,
         waerk_fatura   TYPE zsdt0225-waerk_fatura,
         nr_ov          TYPE zsdt0225-nr_ov,
         fatura         TYPE zsdt0225-fatura,
         docnum         TYPE zsdt0225-docnum,
         vkorg          TYPE zlest0061-vkorg,
         vtweg          TYPE zlest0061-vtweg,
         spart          TYPE zlest0061-spart,
         zterm          TYPE zlest0061-zterm,
         celltab        TYPE lvc_t_styl.
TYPES: END   OF ty_saida.

TYPES: BEGIN OF ty_saida_ov,
         id_seq   TYPE zsdt0225-id_seq,
         matnr_ov TYPE zsdt0225-cod_material,
         menge    TYPE ekpo-menge,
         vlr_usd  TYPE zsdt0225-vlr_usd,
         vlr_brl  TYPE zsdt0225-vlr_brl.
TYPES: END   OF ty_saida_ov.

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
      t_zsdt0306           TYPE TABLE OF zsdt0306,
      w_zsdt0306           TYPE zsdt0306,
      t_zsdt0225           TYPE TABLE OF zsdt0225,
      w_zsdt0225           TYPE zsdt0225,
      w_zlest0055          TYPE zlest0055,
      w_zsdt0307           TYPE zsdt0307,
      t_makt               TYPE TABLE OF makt,
      w_makt               TYPE makt,
      t_mara               TYPE TABLE OF mara,
      w_mara               TYPE mara,
      t_saida              TYPE TABLE OF ty_saida,
      w_saida              TYPE ty_saida,
      t_saida_ov           TYPE TABLE OF ty_saida_ov,
      w_saida_ov           TYPE ty_saida_ov,
      t_lista              TYPE TABLE OF spopli,
      t_saida_aux          TYPE TABLE OF ty_saida,
      w_saida_aux          TYPE ty_saida,
      t_saida_ori          TYPE TABLE OF ty_saida,
      w_saida_ori          TYPE ty_saida,
*
      it_value             LIKE rgsb4 OCCURS 0 WITH HEADER LINE,
      r_dt_fatura          TYPE RANGE OF zlest0061-dt_fatura WITH HEADER LINE,
      r_matkl_sel          TYPE RANGE OF matkl               WITH HEADER LINE,
      it_value_01          LIKE rgsb4 OCCURS 0 WITH HEADER LINE,
      r_vlr_brl            TYPE RANGE OF zsdt0225-vlr_brl WITH HEADER LINE,
      v_vlr_brl(25)        TYPE c,
      wl_header_in         TYPE bapisdhd1,
      wl_header_inx2       TYPE bapisdh1x,
      wl_header_inx        TYPE bapisdhd1x,
      wl_return            TYPE bapiret2,
      w_header_in          TYPE bapisdh1,
      w_header_inx         TYPE bapisdh1x,
*
      obj_zcl_util_sd      TYPE REF TO zcl_util_sd, "Classe de Utilitário SD
      obj_zcl_util         TYPE REF TO zcl_util, "Classe para Utilidade Geral.

      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_sort               TYPE lvc_t_sort,
      w_sort               TYPE lvc_s_sort,
      t_color              TYPE lvc_t_scol,
      w_color              TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl    VALUE 'XX',
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      t_exclude            TYPE ui_functions,
*
      l_qtd                TYPE i,
      l_tabix              TYPE sy-tabix,
      l_cockpit            TYPE char02,
      l_erro               TYPE c,
      l_editar             TYPE c,
      l_pedidos            TYPE string,
      l_werks              TYPE char10,
      l_lote_editado       TYPE numc10,
      l_id_seq             TYPE zsdt0225-id_seq,
      l_resp               TYPE c,
      ok_code              TYPE sy-ucomm,
      ok_code2             TYPE sy-ucomm,
      lt_f401              TYPE lvc_t_f4,
      wl_f401              TYPE lvc_s_f4,
*
      zcl_util             TYPE REF TO zcl_util,
*
      l_sel_button         TYPE smp_dyntxt,

      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

**********************************************************************
* BAPI sales order
**********************************************************************
DATA: BEGIN OF it_partner OCCURS 0.
        INCLUDE STRUCTURE bapiparnr.
      DATA: END OF it_partner.

DATA: BEGIN OF it_return OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
      DATA: END OF it_return.

DATA: BEGIN OF it_itemdata OCCURS 0.
        INCLUDE STRUCTURE bapisditm.
      DATA: END OF it_itemdata.

DATA: BEGIN OF it_condition OCCURS 0.
        INCLUDE STRUCTURE bapicond .
      DATA: END OF it_condition.

DATA: it_items_inx    TYPE TABLE OF bapisditmx WITH HEADER LINE,
      wl_items_inx    TYPE bapisditmx,
      tl_schedules_in TYPE TABLE OF bapischdl  WITH HEADER LINE,
      wl_schedules_in TYPE bapischdl,
      tl_vbuv         TYPE TABLE OF vbuv WITH HEADER LINE,
      wl_vbuv         TYPE vbuv,
      wl_fieldname    TYPE rmdi_name,
      wl_text         TYPE rmdi_ddtxt,
      tl_bapiparex    TYPE TABLE OF bapiparex WITH HEADER LINE,
      wl_bape_vbak    TYPE bape_vbak,
      wl_bape_vbakx   TYPE bape_vbakx,
      tg_msg_ret      TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      wl_msg_ret      TYPE zfiwrs0002,
*
      tl_bdc          TYPE TABLE OF bdcdata,
      wl_bdc          TYPE bdcdata.

**********************************************************************
* ranges
**********************************************************************
RANGES: r_dtfat             FOR zsdt0225-dt_fatura.

DATA: r_matkl TYPE RANGE OF matkl,
      t_matkl LIKE LINE OF r_matkl.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
