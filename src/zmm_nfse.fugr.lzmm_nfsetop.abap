FUNCTION-POOL zmm_nfse MESSAGE-ID znfse_distri.

* INCLUDE LZMM_NFSED...                      " Local class definition

TABLES: zibt_nfse_001,
        zibt_nfse_002,
        zibs_nfse_001,
        zsheader_data_nfse_inbound,
        zspayment_data_nfse_inbound, t042z,
        zibs_nfse_po_serv,
        zibs_nfps_001,
        /tcsr/t_cancrt.

DATA: cursor_field LIKE d021s-fnam,
      cursor_line  TYPE i,
      cursor_value LIKE d021s-fnam.

DATA go_cc_pedidos TYPE REF TO cl_gui_custom_container.
DATA go_cc_associados TYPE REF TO cl_gui_custom_container.

DATA go_alv_pedidos TYPE REF TO cl_gui_alv_grid.
DATA go_alv_associados TYPE REF TO cl_gui_alv_grid.

"Table controls
CONTROLS: tc_condition_list TYPE TABLEVIEW USING SCREEN 2000,
          tc_popup_list     TYPE TABLEVIEW USING SCREEN 3000,
          tc_popup_search   TYPE TABLEVIEW USING SCREEN 3000,
          tc_service        TYPE TABLEVIEW USING SCREEN 3010.

"Tabelas internas
DATA: gt_scr_condition_list TYPE /tcsr/y_cond_list,
      gt_scr_popup_list     TYPE zibc_nfse_pedidos,
      "gt_scr_popup_search   TYPE /tcsr/y_po_list.
      gt_scr_popup_search   TYPE zibc_nfse_pedidos,
      gt_service            TYPE TABLE OF zibs_nfse_po_serv.


DATA: gw_scr_condition_list TYPE /tcsr/s_cond_list,
      gw_scr_popup_list     TYPE zibs_nfse_pedidos,
      gw_scr_popup_search   TYPE zibs_nfse_pedidos.

DATA gv_popup_xml_value TYPE /tcsr/e_nfse_value.
DATA gv_popup_sap_value TYPE /tcsr/e_nfse_value.
DATA gv_total_tolerance TYPE /tcsr/e_tolerance_tax.

*** inicio - ALX
DATA gv_iss_2000 TYPE flag.
*** Fim  - ALX

data gv_erro TYPE flag.

** X - Edita tudo
** C - Somente Cabeçalho
** P - Edita somente pagamento
DATA gv_edit_2000 TYPE flag.

DATA go_cc_2000_text TYPE REF TO cl_gui_custom_container.
DATA gt_2000_text TYPE TABLE OF smum_xmltb.

DATA gv_header_old TYPE zsheader_data_nfse_inbound.
DATA gv_paymen_old TYPE zspayment_data_nfse_inbound.

DATA go_text_2000 TYPE REF TO cl_gui_textedit.

DATA ok_code_9050   TYPE sy-ucomm.

DATA gv_subscr_905x TYPE sy-dynnr VALUE '3050'.
DATA gv_save_3010 TYPE flag.
DATA gv_tc_popup_list_lines LIKE sy-loopc.

" TELA ------------------------------- 4000
DATA ok_code_4000   TYPE sy-ucomm.
DATA gv_subscr_4050x TYPE sy-dynnr VALUE '4050'.
DATA gv_subscr_4051x TYPE sy-dynnr VALUE '4051'.

DATA go_cc_4000_01 TYPE REF TO cl_gui_custom_container.
DATA go_alv_4000_01 TYPE REF TO cl_gui_alv_grid.

DATA go_cc_4000_02 TYPE REF TO cl_gui_custom_container.
DATA go_alv_4000_02 TYPE REF TO cl_gui_alv_grid.

DATA gt_4000_alv_01 TYPE TABLE OF zibs_nfps_001.
DATA gt_4000_alv_02 TYPE TABLE OF zibs_nfps_001.

DATA gt_4000_global_01 TYPE TABLE OF zibs_nfps_001.
DATA gt_4000_global_02 TYPE TABLE OF zibs_nfps_001.

DATA gt_xnota TYPE TABLE OF zibs_nfps_xnota.

DATA gs_nfse_4000 TYPE zibs_nfse_001.

DATA gv_desa_assign TYPE c.
DATA gv_desa_btn_02 TYPE c.
" -------------------------------------------

" TELA 9070 -------------
DATA gt_cancr_text TYPE TABLE OF tline-tdline.
DATA go_editor_popup TYPE REF TO cl_gui_textedit.
DATA go_container_popup TYPE REF TO cl_gui_custom_container.
DATA gv_9070_canc.
DATA gv_9070_readonly.

DATA gv_9070_motivo TYPE  zmotivo_cancelar.
DATA gv_9070_descr TYPE  zmotivo_descricao.

"Tabelas internas
DATA: gt_bapiret2       TYPE TABLE OF bapiret2,
      gt_searched_po    TYPE zibc_nfse_pedidos,
      gt_associated_po  TYPE zibc_nfse_pedidos,
      gt_nfse_002       TYPE TABLE OF zibt_nfse_002,
      gt_po_header_data TYPE STANDARD TABLE OF zmm_nfse_po_header_data,
      gt_po_item_data   TYPE STANDARD TABLE OF zmm_nfse_po_item_data.

DATA: gs_nfse_data     TYPE zmm_nfse_inbound_export_data,
      gs_searched_po   TYPE zibs_nfse_pedidos,
      gs_associated_po TYPE zibs_nfse_pedidos.

DATA go_znfse TYPE REF TO zcl_nfse_inbound.

DATA: w_valor        TYPE komp-netwr,
      w_wmwst        TYPE komp-netwr.

DATA: wa_bukrs_calendary TYPE bukrs. "USER STORY 158527 - MMSILVA - 17.01.2025

"Telas
SELECTION-SCREEN BEGIN OF SCREEN 3050 AS SUBSCREEN .
SELECT-OPTIONS :  s_ebeln3 FOR zibt_nfse_002-ebeln NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF SCREEN 3050.


SELECTION-SCREEN BEGIN OF SCREEN 4050 AS SUBSCREEN .

*SELECTION-SCREEN BEGIN OF LINE.
*
*SELECTION-SCREEN COMMENT 1(10) text-S01. " Chave NF-e
*SELECT-OPTIONS so_chnfe FOR zibs_nfps_001-chave_nfe NO INTERVALS.
*
*" /// localizacao ( tamanho )
*SELECTION-SCREEN COMMENT 40(11) text-S02. " Nota Fiscal
*SELECT-OPTIONS so_nfe FOR zibs_nfps_001-nfenum NO INTERVALS.
*
*" /// localizacao ( tamanho )
*SELECTION-SCREEN COMMENT 80(11) text-S03. " Fornecedor NF
*SELECT-OPTIONS so_lifnr FOR zibs_nfps_001-lifnr NO INTERVALS.
*
*SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS so_chnfe FOR zibs_nfps_001-chave_nfe NO INTERVALS.
SELECT-OPTIONS so_nfe FOR zibs_nfps_001-nfenum NO INTERVALS.
SELECT-OPTIONS so_lifnr FOR zibs_nfps_001-lifnr NO INTERVALS.
SELECT-OPTIONS so_tknum FOR zibs_nfps_001-tknum NO INTERVALS.
SELECTION-SCREEN END OF SCREEN 4050.

SELECTION-SCREEN BEGIN OF SCREEN 4051 AS SUBSCREEN .
SELECTION-SCREEN BEGIN OF BLOCK b01.

"SELECTION-SCREEN COMMENT 1(20) text-004. " Data emissão NF-e
SELECT-OPTIONS so_daem FOR zibs_nfps_001-docdat .
SELECT-OPTIONS so_datr FOR zibs_nfps_001-datbg.
SELECT-OPTIONS so_ebeln FOR zibs_nfps_001-ebeln NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN END OF SCREEN 4051.

CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,

      "handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD  handle_double_click.
    PERFORM f_double_click USING e_row e_column es_row_no.
  ENDMETHOD.                    "on_user_command

*  METHOD handle_top_of_page.
*    PERFORM f_top_of_page.
*  ENDMETHOD.

  METHOD handle_data_changed.
    PERFORM f_on_data_changed USING er_data_changed.
  ENDMETHOD.

ENDCLASS.                    "lcl_'handle_events DEFINITION
