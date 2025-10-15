*&---------------------------------------------------------------------*
*& Include ZFIR0100_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_saida1,
         status    TYPE icon_d,
         id_seq    TYPE zsdt0306-id_seq,
         bukrs     TYPE zsdt0306-bukrs,
         werks     TYPE zsdt0306-werks,
         bukrs_fat TYPE zsdt0306-bukrs_fat,
         werks_fat TYPE zsdt0306-werks_fat,
         ebeln     TYPE zsdt0306-ebeln,
         ebelp     TYPE zsdt0306-ebelp,
         matnr     TYPE zsdt0306-matnr,
         maktx     TYPE makt-maktx,
         matkl     TYPE mara-matkl,
         operacao  TYPE zsdt0306-operacao,
         qtdpedid  TYPE zsdt0306_fat-qtdpedid,
         safra     TYPE zsdt0306-unsez,        "*-IR194132-29.10.2024-#156085-JT
       END OF ty_saida1,

       BEGIN OF ty_tvarvc,
         tp_doc TYPE tvarvc-low,
       END OF ty_tvarvc,
       BEGIN OF ty_err,
         status  TYPE zfie0026-valid,
         message TYPE string,
       END OF ty_err.
DATA it_err TYPE STANDARD TABLE OF ty_err INITIAL SIZE 0.
DATA wa_err_saida TYPE ty_err.
DATA container_main       TYPE REF TO cl_gui_custom_container.
DATA painel_control       TYPE REF TO cl_gui_splitter_container.
DATA painel1              TYPE REF TO cl_gui_container.
DATA painel2              TYPE REF TO cl_gui_container.
DATA lr_column1            TYPE REF TO cl_salv_column.
DATA lr_columns_tb1        TYPE REF TO cl_salv_columns_table.
DATA lr_column_tb1         TYPE REF TO cl_salv_column_table.
DATA lr_columns1           TYPE REF TO cl_salv_columns.
DATA lr_functions1         TYPE REF TO cl_salv_functions.
DATA lr_selections1        TYPE REF TO cl_salv_selections.
DATA lr_sorts_tb1 TYPE REF TO cl_salv_sorts.
DATA lr_aggregations_tb1 TYPE REF TO cl_salv_aggregations.
DATA lr_sort_tb1 TYPE REF TO cl_salv_sort.
DATA lr_aggregation_tb1 TYPE REF TO cl_salv_aggregation.
DATA lv_key1               TYPE salv_s_layout_key.
DATA lex_not_found1        TYPE REF TO cx_salv_not_found.
DATA gr_table1             TYPE REF TO cl_salv_table.
DATA lr_display_settings1  TYPE REF TO cl_salv_display_settings.
DATA l_title1              TYPE lvc_title.
DATA ls_api1               TYPE REF TO if_salv_gui_om_extend_grid_api.
DATA ls_edit1              TYPE REF TO if_salv_gui_om_edit_restricted.
DATA lr_layout1            TYPE REF TO cl_salv_layout.
DATA icon_status1          TYPE string.
DATA lt_rows1              TYPE TABLE OF lvc_s_row.  "TYPE TABLE OF lvc_s_row "TYPE salv_t_row.
DATA qtd_rows1             TYPE i.
DATA ls_selected_row1      TYPE lvc_s_row.
DATA lr_column2            TYPE REF TO cl_salv_column.
DATA lr_columns_tb2        TYPE REF TO cl_salv_columns_table.
DATA lr_column_tb2         TYPE REF TO cl_salv_column_table.
DATA lr_columns2           TYPE REF TO cl_salv_columns.
DATA lr_functions2         TYPE REF TO cl_salv_functions_list."cl_salv_functions.
DATA lr_descr2               TYPE REF TO cl_abap_structdescr.
DATA lr_selections2        TYPE REF TO cl_salv_selections.
DATA lv_key2               TYPE salv_s_layout_key.
DATA lex_not_found2        TYPE REF TO cx_salv_not_found.
DATA gr_table2             TYPE REF TO cl_salv_table.
DATA lr_display_settings2  TYPE REF TO cl_salv_display_settings.
DATA l_title2              TYPE lvc_title.
DATA ls_api2               TYPE REF TO if_salv_gui_om_extend_grid_api.
DATA ls_edit2              TYPE REF TO if_salv_gui_om_edit_restricted.
DATA lr_extend2              TYPE REF TO if_salv_gui_om_edit_restricted.
DATA lr_layout2            TYPE REF TO cl_salv_layout.
DATA icon_status2         TYPE string.
DATA lt_rows2              TYPE salv_t_row."TYPE TABLE OF lvc_s_row.  "TYPE TABLE OF lvc_s_row "TYPE salv_t_row.
DATA ls_row2 TYPE int4.
DATA qtd_rows2             TYPE i.
DATA ls_selected_row2      TYPE lvc_s_row.
DATA: dtini TYPE sy-datum.
DATA: dtfim TYPE sy-datum.
DATA: lv_message TYPE string.
DATA: it_message TYPE TABLE OF string.
DATA: t_saida1 TYPE TABLE OF ty_saida1.
DATA: w_saida1 TYPE ty_saida1.
DATA: t_saida2 TYPE TABLE OF zsdt0306_fat."zsdt0306_fat.
DATA: t_saida2_mem TYPE TABLE OF zsdt0306_fat."zsdt0306_fat.
DATA: w_saida2 TYPE zsdt0306_fat.
DATA: lv_guid TYPE sysuuid_c.
DATA: w_0225 TYPE  zsdt0225.
DATA: w_saida2_fat TYPE zsdt0306_fat."zsdt0306_fat.
DATA: it_tvarvc TYPE TABLE OF ty_tvarvc.
DATA: lr_scell2    TYPE salv_s_cell.
DATA: lr_scells2   TYPE salv_t_cell.
DATA: lr_scolumns2 TYPE salv_t_column.
DATA: lr_srows2    TYPE salv_t_row.
DATA: lr_smode2    TYPE i.
DATA: w_rows TYPE int4.
DATA: t_rows TYPE salv_t_row.
DATA: qtd_rows TYPE int4.
DATA: l_ukurs     TYPE ukurs_curr.
DATA: l_gdatu     TYPE gdatu_inv.
DATA: l_vlr_usd   TYPE zlest0061-vlr_usd.
DATA: l_vlr_brl   TYPE zlest0061-vlr_brl.
DATA: l_tax_dolar TYPE zlest0061-tax_dolar.
DATA: l_stcd_str  TYPE c LENGTH 9.
DATA: l_stcd_conc TYPE c LENGTH 4.
DATA: l_tabix2    TYPE sy-tabix.
DATA: l_value     TYPE lvc_value.
DATA: l_popup     TYPE char1.
DATA: l_resp      TYPE char1.
DATA: l_texto1    TYPE char35.
DATA: l_texto2    TYPE char35.

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

DATA: wl_header_in    TYPE bapisdhd1,
      wl_header_inx2  TYPE bapisdh1x,
      wl_header_inx   TYPE bapisdhd1x,
      wl_return       TYPE bapiret2,
      vpurch_no_s     TYPE char35,
      wa_itemdata     TYPE bapisditm,
      wl_items_inx    TYPE bapisditmx,
      wa_condition    TYPE bapicond,
      wa_partner      TYPE bapiparnr,
      wa_return       TYPE bapiret2,
      it_items_inx    TYPE TABLE OF bapisditmx WITH HEADER LINE,
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
      wl_msg_ret      TYPE zfiwrs0002.

DATA: vlr_icms        TYPE zlest0061-vlr_usd,
      vlr_pis         TYPE zlest0061-vlr_usd,
      vlr_cofins      TYPE zlest0061-vlr_usd,
      vlr_iss         TYPE zlest0061-vlr_usd,
      vlr_liquido     TYPE zlest0061-vlr_usd,
      vl_validto      TYPE j_1btxiss-validto,
      vl_validfrom    TYPE j_1btxiss-validfrom,
      vl_data         TYPE c LENGTH 10,
      vbeln_ov        TYPE bapivbeln-vbeln,
      wg_mensagem(30),
      wl_erro(1),
      l_id_seq        TYPE zsdt0225-id_seq.

DATA: t_zsdt0306 TYPE STANDARD TABLE OF zsdt0306.

DATA: vbeln_fatura   TYPE c LENGTH 10,
      wa_j_1bnflin   TYPE j_1bnflin,
      wl_lifnr       TYPE lfa1-lifnr,
      l_itm_number   TYPE posnr_va,
      msg(150),
      _dia           TYPE i,
      r_auart_ztrg   TYPE RANGE OF auart,
      w_zsdt0225     TYPE zsdt0225,
      w_zsdt0307     TYPE zsdt0307,
      r_matkl        TYPE RANGE OF matkl,
      t_matkl        LIKE LINE OF r_matkl,
      wa_zlest0055   TYPE zlest0055,
      l_pedidos(150) TYPE c,
      l_erro         TYPE c,
      l_tabix        TYPE sy-tabix,
      w_saida2_aux   TYPE zsdt0306_fat, "zsdt0306_fat,
      w_saida2_ori   TYPE zsdt0306_fat, "zsdt0306_fat,
      w_saida2_ov    TYPE zsdt0306_fat, "zsdt0306_fat,
      w_stable       TYPE lvc_s_stbl    VALUE 'XX',
      v_vlr_brl(25)  TYPE c,
      t_success      TYPE TABLE OF bapivbrksuccess,
      t_billing      TYPE TABLE OF bapivbrk,
      t_return       TYPE TABLE OF bapireturn1,
      it_zlest0055   TYPE STANDARD TABLE OF zlest0055,
      t_saida2_aux   TYPE STANDARD TABLE OF zsdt0306_fat, "zsdt0306_fat,
      t_saida2_ori   TYPE TABLE OF zsdt0306_fat, "zsdt0306_fat,
      t_saida2_ov    TYPE TABLE OF zsdt0306_fat, "zsdt0306_fat,
      r_vlr_brl      TYPE RANGE OF zsdt0225-vlr_brl WITH HEADER LINE,
      r_matkl_sel    TYPE RANGE OF matkl               WITH HEADER LINE,
      it_value_01    LIKE rgsb4 OCCURS 0 WITH HEADER LINE,
      it_value       LIKE rgsb4 OCCURS 0 WITH HEADER LINE,
      r_dt_fatura    TYPE RANGE OF zlest0061-dt_fatura WITH HEADER LINE.

TYPE-POOLS: icon.

DATA: isval TYPE TABLE OF sval WITH HEADER LINE.
DATA: repid TYPE sy-repid.
DATA: icon_name     TYPE  icon-name,
      lr_returncode TYPE returncode,
      _erro(1)      TYPE c.
DATA: _value TYPE string.
DATA: new_value TYPE decfloat34,
      linhas    TYPE i,
      t_lista   TYPE TABLE OF spopli.
