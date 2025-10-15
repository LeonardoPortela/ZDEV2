*&---------------------------------------------------------------------*
*&  Include           ZPMR0016_TOPO

TABLES: eqkt, t001w, efhm, equi, equz, iloa, eqbs, fleet.

*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
TABLES: zpmt0079.
*GGARAUJO1 - 12/09/2024 - IR190024 - Fim

DATA: p_eq_sup      TYPE c.
TYPES: BEGIN OF ty_saida,
         equnr                TYPE equnr,
         eqktx                TYPE eqkt-eqktx,
         iwerk                TYPE equz-iwerk,
         cbx_emprestar        TYPE c LENGTH 1,
         cbx_ord_abast        TYPE c LENGTH 1,
         cbx_ord_remon        TYPE c LENGTH 1,
         devolucao_automatica TYPE c LENGTH 1,"Rubenilson - 23.12.24 - US138088
         icon_eq_inf          TYPE char8,
         eq_inf               TYPE char8,
         eq_sup               TYPE char8,
         sequencia            TYPE p DECIMALS 1,
         celltab              TYPE lvc_t_styl,"Rubenilson - 23.12.24 - US138088
       END OF ty_saida.


TYPES: BEGIN OF ty_mpla.
         INCLUDE STRUCTURE mpla.
         INCLUDE STRUCTURE mpla_addition.
TYPES: END OF ty_mpla.

TYPES: BEGIN OF ty_saida_equi_responsavel.
         INCLUDE STRUCTURE zequi_emprestimo.
TYPES:
         p_sel        TYPE char08,
         cbx_devolver TYPE c LENGTH 1,
         dt_devolucao TYPE zequi_emprestimo-erdat,
         hr_devolucao TYPE zlest0056-hora_registro,
         sequencia    TYPE p DECIMALS 1,
         cellcolor    TYPE lvc_t_scol,"Rubenilson - 23.12.24 - US138088
       END OF ty_saida_equi_responsavel.

TYPES: BEGIN OF ty_status_equnr,
         equnr   TYPE equnr,
         eqktx   TYPE eqkt-eqktx,
         hequi   TYPE hequi,
         eqktx_  TYPE eqkt-eqktx,
         eqp_inf TYPE char8,
         eqp_sup TYPE char8,
         status  TYPE iconname, "RJF - US 158036-26-11-2024-#158036-RJF
         det     TYPE char70, "RJF - US 158036-26-11-2024-#158036-RJF
       END OF ty_status_equnr.

TYPES: BEGIN OF ty_mhis.
         INCLUDE STRUCTURE mhis.
         INCLUDE STRUCTURE mhis_addition.
TYPES: END   OF ty_mhis.

TYPES: BEGIN OF ty_mpos.
         INCLUDE STRUCTURE mpos.
         INCLUDE STRUCTURE mpos_addition.
TYPES: END OF ty_mpos.


TYPES: BEGIN OF ty_mmpt.
         INCLUDE STRUCTURE mmpt.
         INCLUDE STRUCTURE mmpt_addition.
TYPES: END OF ty_mmpt.

TYPES: BEGIN OF ty_equi,
*         SWERK TYPE EQUI-SWERK,
         iwerk TYPE equz-iwerk,
         equnr TYPE equi-equnr,
         objnr TYPE equi-objnr,
         eqtyp TYPE equi-eqtyp,
         datbi TYPE equz-datbi,
         hequi TYPE equz-hequi,
         marc  TYPE c.
TYPES: END OF ty_equi.

TYPES: BEGIN OF ty_zequi_emprestimo.
         INCLUDE STRUCTURE zequi_emprestimo.
TYPES:   p_sel TYPE c.
TYPES: END OF ty_zequi_emprestimo.

TYPES: BEGIN OF ty_ucomm,
         ucomm TYPE  sy-ucomm,
       END OF ty_ucomm.

TYPES:BEGIN OF ty_fields,
        group     TYPE char3,
        value     TYPE num1,
        invisible TYPE num1,
      END OF ty_fields.

TYPES:BEGIN OF ty_equip,
        equnr   TYPE equz-equnr,
        eqktx   TYPE eqkt-eqktx,
        origem  TYPE swerk,
        destino TYPE equz-iwerk,
        status  TYPE char30,
      END OF ty_equip.

DATA:
  BEGIN OF radio_button,
    defi TYPE abap_bool,
    temp TYPE abap_bool,
  END OF radio_button.

DATA: w_cursor_field TYPE c LENGTH 50.

DATA: it_eqkt                    TYPE TABLE OF eqkt,
      lt_equi                    TYPE TABLE OF ty_equip,
      fs_equip                   TYPE ty_equip,
      it_fcat                    TYPE TABLE OF lvc_s_fcat,
      lt_fcat                    TYPE TABLE OF lvc_s_fcat,
      it_itob                    TYPE TABLE OF itob,
      it_equi                    TYPE TABLE OF ty_equi,
      it_vequi                   TYPE TABLE OF v_equi,
      it_zequi_emprestimo        TYPE TABLE OF zequi_emprestimo,
      it_saida_equi_disponiveis  TYPE TABLE OF ty_saida,
*GGARAUJO1 - 12/09/2024 - IR190024 - Inicio
      it_zpmt0079                TYPE TABLE OF zpmt0079,
      wa_zpmt0079                TYPE zpmt0079,
*GGARAUJO1 - 12/09/2024 - IR190024 - Fim
      it_exib_equip              TYPE TABLE OF ty_saida,
      it_saida_equi_emprestados  TYPE TABLE OF ty_zequi_emprestimo,
      gt_saida_equi_emprestados  TYPE TABLE OF ty_zequi_emprestimo,
      it_saida_equi_responsavel  TYPE TABLE OF ty_saida_equi_responsavel,
      gt_saida_equi_responsavel  TYPE TABLE OF ty_saida_equi_responsavel,
      it_hierarchy               TYPE TABLE OF rihequi,
      it_saida_emprestimo_equi   TYPE TABLE OF ty_saida,
      it_saida_dev_equi          TYPE TABLE OF ty_saida_equi_responsavel,
      wa_saida_dev_equi          TYPE ty_saida_equi_responsavel,
      it_selected_rows           TYPE lvc_t_row,
      it_system_status           TYPE STANDARD TABLE OF bapi_itob_status,
      it_user_status             TYPE STANDARD TABLE OF bapi_itob_status,
      it_msg_return              TYPE TABLE OF zfiwrs0002,
      wa_msg_return              TYPE zfiwrs0002,
      it_status_equnr            TYPE TABLE OF ty_status_equnr,
      it_status_hequi            TYPE TABLE OF ty_status_equnr,
      it_status                  TYPE TABLE OF ty_status_equnr,
      wa_equz                    TYPE equz,
      wa_eqkt                    TYPE eqkt,
      st_equz                    TYPE TABLE OF rihequi,
      w_equz                     TYPE rihequi,
      wa_itob                    TYPE itob,
      wa_mpos                    TYPE mpos,
      wa_iloa                    TYPE iloa,
      wa_selected_rows           TYPE lvc_s_row,
      wa_system_status           TYPE bapi_itob_status,
      wa_status_equnr            TYPE ty_status_equnr,
      wa_status_hequi            TYPE ty_status_equnr,
      w_lg_equz                  TYPE equz,
      wa_return                  TYPE bapireturn,
      wa_zequi_emprestimo        TYPE zequi_emprestimo,
      wa_saida_equi_responsavel  TYPE ty_saida_equi_responsavel,
      wa_saida_equi_emprestados  TYPE zequi_emprestimo,
      wa_vequi                   TYPE v_equi,
      wa_hierarchy               TYPE rihequi,
      wa_equi                    TYPE ty_equi,
      t_equz                     TYPE ty_equi,
      gt_equz                    TYPE ty_equi,
      t_equi                     TYPE equi OCCURS 0,
      wa_mensagem                TYPE char30,
      wa_layout                  TYPE lvc_s_layo,
      wa_layout_500              TYPE lvc_s_layo,
      ls_layout                  TYPE lvc_s_layo,
      ls_good                    TYPE lvc_s_modi,
      wa_stable                  TYPE lvc_s_stbl,
      wa_stable_500              TYPE lvc_s_stbl,
      wa_saida_equi_disponiveis  TYPE ty_saida,
      wa_impos                   TYPE ty_mpos,
      wa_saida_emprestimo_equi   TYPE ty_saida,
      it_imhis                   TYPE ty_mhis OCCURS 0 WITH HEADER LINE,
      it_impos                   TYPE ty_mpos OCCURS 0 WITH HEADER LINE,
      it_impla                   TYPE ty_mpla OCCURS 0 WITH HEADER LINE,
      it_immpt                   TYPE ty_mmpt OCCURS 0 WITH HEADER LINE,
      obj_custom_0110            TYPE REF TO cl_gui_custom_container,
      obj_alv_0110               TYPE REF TO cl_gui_alv_grid,
      obj_custom_0120            TYPE REF TO cl_gui_custom_container,
      obj_alv_0120               TYPE REF TO cl_gui_alv_grid,
      obj_custom_0130            TYPE REF TO cl_gui_custom_container,
      obj_alv_0130               TYPE REF TO cl_gui_alv_grid,
      obj_alv_0200               TYPE REF TO cl_gui_alv_grid,
      obj_alv_0400               TYPE REF TO cl_gui_alv_grid,
      obj_alv_0500               TYPE REF TO cl_gui_alv_grid,
      obj_custom_0200            TYPE REF TO cl_gui_custom_container,
      obj_custom_0400            TYPE REF TO cl_gui_custom_container,
      obj_custom_0500            TYPE REF TO cl_gui_custom_container,
      obj_custom_0300            TYPE REF TO cl_gui_custom_container,
      obj_alv_status             TYPE REF TO cl_gui_alv_grid,
      tbx_centro                 TYPE equz-iwerk,
      wa_retorn                  TYPE c,
      l_eqkt                     TYPE eqkt,
      txt_centrdestino           TYPE t001w-name1,
      tbx_equipamento            TYPE eqkt-equnr,
      tbx_centro_destino         LIKE equz-iwerk,
      txt_centro_destino         TYPE t001w-name1,
      tbx_qt_dias                TYPE numc3,
      tbx_status_bapis           TYPE char50,
      loc_instalacao             TYPE char30,
      imobilizado                TYPE anln1,
      subimobilizado             TYPE anln2,
      p_devolucao                TYPE char8,
      indice                     TYPE num1,
      return_status              LIKE abap_false,
      status_bapis               LIKE abap_true,
      at_costcenter_destino      TYPE kostl,
      at_costcenter_origem       TYPE kostl,
      id_cent_trabalho           TYPE cr_objid,
      at_costcenter_standorder   TYPE aufnr,
      at_costcenter_settlorder   TYPE aufnr,
      at_data_general-standorder TYPE aufnr,
      at_data_general-settlorder TYPE aufnr,
      lines                      TYPE sy-tabix,
      ok_code                    LIKE sy-ucomm,
      gt_exc_button              TYPE ui_functions,
      it_exclude_fcode           TYPE ui_functions,
      wa_toolbar                 TYPE stb_button,
      wa_data_general            TYPE bapi_itob,
      wa_data_specific_exp       TYPE bapi_itob_eq_only,
      l_ucomm                    TYPE sy-ucomm,
      l_rc                       TYPE i,
      fields                     TYPE TABLE OF ty_fields,
      p_def                      TYPE c,
      p_defi                     TYPE c,
      p_temp                     TYPE c VALUE 'X',
      it_ucomm                   TYPE TABLE OF ty_ucomm,
      w_ucomm                    TYPE sy-ucomm,
      c_destino                  TYPE iwerk,
      c_origem                   TYPE iwerk,
      gt_erro                    TYPE TABLE OF bapiret2,
      gw_erro                    TYPE bapiret2,

      BEGIN OF g_tabstrip,
        tab1 TYPE char40,
        tab2 TYPE char40,
        tab3 TYPE char40,
        qtd1 TYPE char3 VALUE '0',
        qtd2 TYPE char3 VALUE '0',
        qtd3 TYPE char3 VALUE '0',
      END OF g_tabstrip,

      BEGIN OF g_ts_0100,
        subscreen   LIKE sy-dynnr,
        program     LIKE sy-repid VALUE 'ZPMR0016',
        pressed_tab LIKE sy-ucomm VALUE 'TAB_DISPONIVEIS',
      END OF g_ts_0100.

DATA: p_dev_por  TYPE sy-uname,
      p_data_dev TYPE sy-datum,
      p_hora_dev TYPE sy-uzeit,
      quadro_002 TYPE char40.

DATA: clicks TYPE sy-tabix.
DATA: yx(1)            TYPE c VALUE 'X'.
DATA: x_xaktyp  LIKE  t370-aktyp.
DATA: mem_id_editequi(8)         TYPE c VALUE 'EDITEQUI'.

DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata,
      p_resp, check, p_erro(1),
      wa_bdcdata LIKE LINE OF ti_bdcdata.

DATA: vg_erro  TYPE char01.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

CONTROLS itabstrip TYPE TABSTRIP.
