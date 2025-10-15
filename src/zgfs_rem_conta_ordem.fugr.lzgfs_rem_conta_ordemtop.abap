FUNCTION-POOL zgfs_rem_conta_ordem.         "MESSAGE-ID ..

TYPE-POOLS : slis, icon, kkblo.

*******************************************************************************************
* types
*******************************************************************************************
TYPES: BEGIN OF ty_saida,
         dt_emissao       TYPE zib_nfe_dist_ter-dt_emissao,
         numero           TYPE zib_nfe_dist_ter-numero,
         serie            TYPE zib_nfe_dist_ter-serie,
         vl_total         TYPE zib_nfe_dist_ter-vl_total,
         chave_nfe        TYPE zib_nfe_dist_ter-chave_nfe,
         prod_descricao   TYPE zib_nfe_dist_itm-prod_descricao,
         prod_qtd_comerci TYPE zib_nfe_dist_itm-prod_qtd_comerci,
         forne_cnpj       TYPE char18, "zib_nfe_dist_ter-forne_cnpj,   "*-CS2024000522-21.06.2024-JT-#143588
         forne_ie         TYPE zib_nfe_dist_ter-forne_ie,              "*-CS2024000522-21.06.2024-JT-#143588
         agente_frete     TYPE lifnr,                                  "*-CS2024000522-21.06.2024-JT-#143588
         placa            TYPE zde_tx_campo,                           "*-CS2024000522-21.06.2024-JT-#143588
         vlr_unit_frete   TYPE zde_vlr15_02,                           "*-CS2024000522-21.06.2024-JT-#143588
         chave_cte        TYPE zde_chave_doc_e.                        "*-CS2024000522-21.06.2024-JT-#143588
TYPES: END   OF ty_saida.

TYPES: BEGIN OF ty_transp.
         INCLUDE STRUCTURE zlest0211_out.
TYPES: END   OF ty_transp.

TYPES: BEGIN OF ty_logproc,
         status     TYPE icon-id,
         vbeln      TYPE zlest0214-vbeln,
         etapa_proc TYPE zlest0214-etapa_proc,
         etapa_desc TYPE char100,
         doc_gerado TYPE zlest0214-doc_gerado,
         msgv1      TYPE char200, "zlest0214-msgv1, CSB - 19.12.2022 - BUG - 98493
         data       TYPE zlest0214-data,
         hora       TYPE zlest0214-hora,
         usuario    TYPE zlest0214-usuario.
TYPES: END   OF ty_logproc.

TYPES: BEGIN OF ty_lfa1.
         INCLUDE STRUCTURE lfa1.
TYPES:   forne_cnpj TYPE zib_nfe_dist_ter-forne_cnpj.
TYPES: END OF ty_lfa1.

TYPES: BEGIN OF ty_icon,
         id   TYPE icon-id,
         name TYPE icon-name.
TYPES: END   OF ty_icon.

*******************************************************************************************
* classes / btree
*******************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

DATA: g_nfnum9            TYPE j_1bnfnum9,
      g_vbeln             TYPE vbeln,
      g_chave_nf_venda    TYPE zde_chave_nfe,
*
      g_vbeln_venda       TYPE vbeln_va,
      g_ov_dummy          TYPE vbeln_va,
      g_remessa_dummy     TYPE vbeln_vl,
      g_refkey            TYPE j_1brefkey,
      g_ag_frete          TYPE lifnr,
      g_cod_motorista     TYPE lifnr,
      g_nf_remessa        TYPE j_1bnfnum9,
      g_nf_venda          TYPE j_1bnfnum9,
*
      g_safra             TYPE string,
      g_nr_ordem          TYPE string,
      g_tipo_proc         TYPE char1,
      g_rem_vbeln         TYPE vbeln_vl,
      g_rem_posnr         TYPE posnr_vl,
      g_bsart             TYPE esart,
      g_p_bruto           TYPE ntgew,
      g_p_liquido         TYPE ntgew,
      r_gerar,
      r_atrib,
      vg_view_transp      TYPE c,
      vg_tipo_frete(3),

*
      w_xvbap             TYPE vbapvb,
      w_vbak              TYPE vbak,
      w_vbkd              TYPE vbkd,
      w_kna1              TYPE kna1,
      t_vbap              TYPE TABLE OF vbap,
      w_vbap              TYPE vbap,
      t_vbep              TYPE TABLE OF vbep,
      w_vbep              TYPE vbep,
      t_vbpa              TYPE TABLE OF vbpa,
      w_vbpa              TYPE vbpa,
      t_lips              TYPE TABLE OF lips,
      w_lips              TYPE lips,
      t_icon              TYPE TABLE OF ty_icon,
      w_icon              TYPE ty_icon,
      t_idd07v            TYPE TABLE OF  dd07v,
      w_idd07v            TYPE dd07v,
      w_j_1bnfdoc         TYPE j_1bnfdoc,  "*-CS2024000522-18.07.2024-JT-#143588
      w_zlest0210         TYPE zlest0210,
      w_zlest0211         TYPE zlest0211,
      w_zsdt0001          TYPE zsdt0001,
      w_zlest0002         TYPE zlest0002,
      t_zlest0214         TYPE TABLE OF zlest0214,
      w_zlest0214         TYPE zlest0214,
      "      t_zlest0228         TYPE TABLE OF zlest0228,"
      "      w_zlest0228         TYPE zlest0228,


      w_campos_nfe        TYPE zde_campos_nfe,
      zcl_util            TYPE REF TO zcl_util,
*
      t_saida             TYPE TABLE OF ty_saida,
      w_saida             TYPE ty_saida,
      t_transp            TYPE TABLE OF ty_transp,
      t_transp_aux1       TYPE TABLE OF ty_transp,
      t_transp_aux2       TYPE TABLE OF ty_transp,
      w_transp            TYPE ty_transp,
      t_logproc           TYPE TABLE OF ty_logproc,
      w_logproc           TYPE ty_logproc,
      t_lfa1              TYPE TABLE OF ty_lfa1,
      w_lfa1              TYPE ty_lfa1,
      w_info_c            TYPE kna1,
      w_info_k            TYPE lfa1,
      t_set               TYPE TABLE OF rgsb4,
      w_set               TYPE rgsb4,
*
      tree1               TYPE REF TO cl_hrpayna_gui_alv_tree, "cl_gui_alv_tree.
      mr_toolbar          TYPE REF TO cl_gui_toolbar,
      g_container         TYPE scrfname VALUE 'CONTAINER',
      g_container2        TYPE scrfname VALUE 'CONTAINER2',
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      g_custom_container2 TYPE REF TO cl_gui_custom_container,
      g_grid              TYPE REF TO cl_gui_alv_grid,
      g_grid2             TYPE REF TO cl_gui_alv_grid,
      w_tool              TYPE stb_button,
      ok_code             TYPE sy-ucomm,
      l_cod_motorista     LIKE lfa1-lifnr,
      l_nome_motorista    TYPE lfa1-name1,
      l_cpf_motorista     TYPE lfa1-stcd1,
      l_tem_romaneio      TYPE c,
      l_tem_lancamento    TYPE c,
      l_ok                TYPE c,
      l_clear             TYPE c,
      l_seleciona         TYPE c,
      l_elimina           TYPE c,
      l_vgbel             TYPE vbrp-vgbel,
      l_tabix             TYPE sy-tabix,
      l_icon_name         TYPE icon-name,
      l_ov_dummy          TYPE vbeln_vl,

      l_safra_ordem_car   TYPE zlest0108-safra_ordem_car,
      l_nro_ordem_car     TYPE zlest0108-nro_ordem_car,
      l_peso_tara         TYPE zlest0108-peso_liq,
      l_peso_bruto        TYPE zlest0108-peso_liq,
      l_id_ordem          TYPE zde_id_ordem,

      t_sort              TYPE lvc_t_sort,
      w_sort              TYPE lvc_s_sort,
      t_fieldcatalog      TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab            TYPE slis_t_extab,
      w_exctab            TYPE slis_extab,
      w_item_layout       TYPE lvc_s_laci,
      w_layout            TYPE lvc_s_layo,
      ls_fieldcatalog     TYPE lvc_s_fcat,
      ls_exclude          TYPE ui_func,
      pt_exclude          TYPE ui_functions,
      pt_exclude2         TYPE ui_functions,
      t_del_rows          TYPE lvc_t_row,
      w_del_rows          TYPE lvc_s_row,
      t_sel_cols          TYPE lvc_t_col,
      w_sel_cols          TYPE lvc_s_col,
      l_row_id            TYPE lvc_s_row,
      l_column_id         TYPE lvc_s_col,
      l_stable            TYPE lvc_s_stbl,
      t_fcat_lvc          TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE,
      t_fcat_kkb          TYPE kkblo_t_fieldcat,
*
      w_header_in         TYPE bapisdh1,  "bapisdhd1,
      w_header_inx        LIKE bapisdh1x, "bapisdhd1x,
      t_items_in          TYPE STANDARD TABLE OF bapisditm,
      w_items_in          TYPE bapisditm,
      t_items_inx         TYPE STANDARD TABLE OF bapisditmx,
      w_items_inx         TYPE bapisditmx,
      t_schedules_in      TYPE STANDARD TABLE OF bapischdl,
      w_schedules_in      TYPE bapischdl,
      t_schedules_inx     TYPE STANDARD TABLE OF bapischdlx,
      w_schedules_inx     TYPE bapischdlx,
      t_conditions_in     TYPE STANDARD TABLE OF bapicond,
      w_conditions_in     TYPE bapicond,
      t_partners          TYPE STANDARD TABLE OF bapiparnr,
      w_partners          TYPE bapiparnr,
      t_bapiparex         TYPE TABLE OF bapiparex,
      w_bapiparex         TYPE bapiparex,
      w_bape_vbak         TYPE bape_vbak,
      w_bape_vbakx        TYPE bape_vbakx,
      t_return            TYPE STANDARD TABLE OF bapiret2.

RANGES:
      r_cfop               FOR zib_nfe_dist_itm-prod_cfop,
      r_name               FOR icon-name.

*******************************************************************************************
* classes / implementacao
*******************************************************************************************
CLASS lcl_event DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event2 DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.
INCLUDE zsdctao_toolbar_event_receiver.
INCLUDE zsdctao_tree_event_receiver.

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver,
      m_event_handler        TYPE REF TO lcl_event,
      m_event_handler2       TYPE REF TO lcl_event2.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event IMPLEMENTATION.

  METHOD toolbar.
*    FREE e_object->mt_toolbar.
*
*    CLEAR w_tool.
*    w_tool-function = 'INSERT'. "cl_gui_alv_grid=>mc_fc_loc_insert_row.
*    w_tool-text     = ''.
*    w_tool-icon     = '@17@'.
*    APPEND w_tool TO e_object->mt_toolbar.
*
*    CLEAR w_tool.
*    w_tool-function = 'DELETE'. "cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    w_tool-text     = ''.
*    w_tool-icon     = '@18@'.
*    APPEND w_tool TO e_object->mt_toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.
    CASE e_ucomm.

      WHEN 'INSERT'.

      WHEN 'DELETE'.

    ENDCASE.

    CALL METHOD g_grid->refresh_table_display.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.

*******************************************************************************************
* botoes alv
*******************************************************************************************
CLASS lcl_event2 IMPLEMENTATION.

  METHOD toolbar.

  ENDMETHOD.             "DISPLAY

*******************************************************************************************
* user command alv
*******************************************************************************************
  METHOD user_command.

    l_stable-row = 'X'.
    l_stable-col = 'X'.

    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = l_stable.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.
