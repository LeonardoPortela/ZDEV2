FUNCTION-POOL zgfs_dados_pedido  MESSAGE-ID znfe_distri.  "*-CS2024000522-18.07.2024-JT-#147087

* INCLUDE LZGFS_DADOS_PEDIDOD...             " Local class definition


TYPES: BEGIN OF ty_dados_pedidos,
         ebeln       TYPE ekpo-ebeln,
         ebelp       TYPE ekpo-ebelp,
         menge       TYPE ekpo-menge,
         meins       TYPE ekpo-meins,
         matnr       TYPE ekpo-matnr,
         ped_werks   TYPE ekpo-werks,
         ped_entrega TYPE lfa1-lifnr,
         lgort       TYPE ekpo-lgort,
         charg       TYPE eket-charg,
         ped_coleta  TYPE ekpa-lifn2,
       END   OF ty_dados_pedidos,

       tty_dados_pedidos TYPE TABLE OF ty_dados_pedidos,

       BEGIN OF ty_dados_processa_po,
         ebeln       TYPE ekpo-ebeln,
         ebelp       TYPE ekpo-ebelp,
         menge       TYPE ekpo-menge,
         meins       TYPE ekpo-meins,
         matnr       TYPE ekpo-matnr,
         ped_werks   TYPE ekpo-werks,
         ped_entrega TYPE lfa1-lifnr,
         lgort       TYPE ekpo-lgort,
         charg       TYPE eket-charg,
         ped_coleta  TYPE ekpa-lifn2,
       END   OF ty_dados_processa_po.

*-CS2024000522-26.08.2024-JT-#147087-inicio
TYPES:  BEGIN OF ty_log_remessa.
          INCLUDE STRUCTURE zlest0247.
*         ebeln    TYPE ekpo-ebeln,
*         tipo_msg TYPE char01,
*         number   TYPE symsgno,
*         message  TYPE bapi_msg,
TYPES: END   OF ty_log_remessa.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END   OF ty_estrutura.
*-CS2024000522-26.08.2024-JT-#147087-fim

DATA: gt_dados_pedidos     TYPE TABLE OF ty_dados_pedidos,
      wa_dados_pedidos     TYPE ty_dados_pedidos,
      wa_dados_po          TYPE ty_dados_pedidos,
      gt_log_remessa       TYPE TABLE OF ty_log_remessa,
      wa_log_remessa       TYPE ty_log_remessa,
      gt_dados_remessa     TYPE zzsdt_dados_remessa,
      wa_dados_remessa     TYPE zzsd_dados_remessa,
      gt_dados_processa_po TYPE TABLE OF ty_dados_processa_po,
      wa_dados_processa_po TYPE ty_dados_processa_po,
      t_items              TYPE bapishpdelivnumb OCCURS 0 WITH HEADER LINE,
      t_itens              TYPE bapidlvreftosto OCCURS 0 WITH HEADER LINE,
      w_itens              TYPE bapidlvreftosto,
      t_retorno            TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
      t_retorno2           TYPE TABLE OF bapiret2 INITIAL SIZE 0 WITH HEADER LINE,
      item_data            TYPE TABLE OF bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE,
      item_control         TYPE TABLE OF bapiobdlvitemctrlchg INITIAL SIZE 0 WITH HEADER LINE,
      item_data_spl        TYPE TABLE OF /spe/bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE,
      header_partner       TYPE TABLE OF bapidlvpartnerchg INITIAL SIZE 0 WITH HEADER LINE,
      t_return             TYPE TABLE OF bapiret2,
      w_return             TYPE bapiret2,
      w_header_data        TYPE bapiobdlvhdrchg,
      w_header_control     TYPE bapiobdlvhdrctrlchg,
*
      it_fieldcat          TYPE TABLE OF ty_estrutura,
      wa_estrutura         TYPE ty_estrutura,
      wa_fieldcat          TYPE ty_estrutura,
      ls_variant           TYPE disvariant,
      l_grid_title         TYPE lvc_title.


DATA: sl_vbkok_wa TYPE vbkok,
      sl_vbpok    TYPE vbpok,
      tl_vbpok    TYPE TABLE OF vbpok,
      tl_prot     TYPE TABLE OF prott,
      sl_prot     TYPE prott.

DATA: sl_hdata    TYPE bapiobdlvhdrchg,
      sl_hcont    TYPE bapiobdlvhdrctrlchg,
      vl_delivery TYPE bapiobdlvhdrchg-deliv_numb,
      tl_bapiret2 TYPE bapiret2_t,
      t_exctab_po TYPE slis_t_extab,
      w_exctab_po TYPE slis_extab.


DATA: g_container        TYPE scrfname VALUE 'CONT_PO',
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_grid             TYPE REF TO cl_gui_alv_grid,
      ok_code            TYPE sy-ucomm,
      l_stable           TYPE lvc_s_stbl,
      w_layout           TYPE lvc_s_layo,
      t_function         TYPE ui_functions,
      w_function         TYPE ui_func,
      ls_fieldcatalog    TYPE lvc_s_fcat,
      t_fieldcatalog     TYPE lvc_t_fcat.

DATA: vg_display TYPE c.


*******************************************************************************************
* classes / implementacao
*******************************************************************************************
CLASS lcl_event DEFINITION .

  PUBLIC SECTION.
    METHODS: toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
      IMPORTING e_object.
    METHODS: user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

ENDCLASS.


DATA: m_event_handler        TYPE REF TO lcl_event.

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

  METHOD on_data_changed.

    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          l_lifnr      TYPE lifnr,
          vl_tot_menge TYPE ekpo-menge,
          vl_tabix     TYPE sy-tabix.

    FIELD-SYMBOLS: <fs_dados_pedidos> TYPE tty_dados_pedidos.

    ASSIGN er_data_changed->mp_mod_rows->* TO <fs_dados_pedidos>.

    CLEAR ls_good.
    READ TABLE er_data_changed->mt_good_cells INTO ls_good WITH KEY fieldname = 'EBELN'.
    IF ls_good IS INITIAL.
      READ TABLE er_data_changed->mt_good_cells INTO ls_good WITH KEY fieldname = 'EBELP'.
    ENDIF.

    READ TABLE <fs_dados_pedidos> INTO DATA(wa_change_pedidos) INDEX 1.
    IF NOT wa_change_pedidos-ebeln IS INITIAL AND NOT wa_change_pedidos-ebelp IS INITIAL.

      SELECT SINGLE menge
             FROM ekpo
             INTO @DATA(vl_ekpo_menge)
             WHERE ebeln EQ @wa_change_pedidos-ebeln
             AND   ebelp EQ @wa_change_pedidos-ebelp.

      SELECT SUM( menge )
             FROM ekbe
             INTO @DATA(vl_ekbe_menge)
             WHERE ebeln EQ @wa_change_pedidos-ebeln
             AND   ebelp EQ @wa_change_pedidos-ebelp
             AND   vgabe EQ '8'.

      READ TABLE gt_dados_pedidos INTO wa_dados_pedidos INDEX ls_good-row_id.
      IF sy-subrc EQ 0.
        vl_tabix = sy-tabix.
        vl_tot_menge = vl_ekpo_menge - vl_ekbe_menge.
        IF wa_dados_pedidos-menge LT vl_tot_menge AND wa_dados_pedidos-menge GT 0.
          wa_dados_pedidos-menge = wa_dados_pedidos-menge.
        ELSE.
          wa_dados_pedidos-menge = vl_tot_menge.
        ENDIF.
        MODIFY gt_dados_pedidos FROM wa_dados_pedidos INDEX vl_tabix.
      ENDIF.
    ENDIF.



    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = l_stable.

  ENDMETHOD.


ENDCLASS.

INCLUDE zles0200_nova_tela.
