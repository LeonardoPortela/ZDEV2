*&---------------------------------------------------------------------*
*&  Include           ZFIS33TOP
*&---------------------------------------------------------------------*

program zfis33. "Equalização ECC X HANA #108307   - SMC

include <cl_alv_control>.
*=======================================================================
* TABLES
*=======================================================================
tables: zfit0136, zfit0137, zfit0151 .

*=======================================================================
* Types
*=======================================================================
types t_j_1baonv type sorted table of j_1baonv with unique key cfop.
types:
  begin of ty_j_1bagt,
    cfop   type j_1bcfop, "Equalização ECC X HANA #108307   - SMC
    cfotxt type j_1bagt-cfotxt,
  end of ty_j_1bagt,

  begin of ty_t001w,
    werks type t001w-werks,
    name1 type t001w-name1,
  end of ty_t001w,

  begin of ty_csks,
    kostl type csks-kostl,
    name1 type csks-name1,
  end of ty_csks.

*=======================================================================
* STRUCTURES E INTERNAL TABLES
*=======================================================================
data: it_zfit0136     type standard table of zfit0136 with key cfop,
      it_zfit0137     type standard table of zfit0137 with key werks,
      it_zfit0151     type standard table of zfit0151 with key werks,
      it_zfit0195     type standard table of zfit0195 with key matkl, "Equalização ECC X HANA #108307   - SMC
      it_saida_136    type standard table of zfit0136 with key cfop,
      it_saida_137    type standard table of zfit0137 with key werks,
      it_saida_151    type standard table of zfit0151 with key werks,
      it_saida_195    type standard table of zfit0195, "Equalização ECC X HANA #108307   - SMC
      it_aux_136      type standard table of zfit0136 with key cfop,
      it_aux_137      type standard table of zfit0137 with key werks,
      it_aux_151      type standard table of zfit0151 with key werks,
      it_aux_195      type standard table of zfit0195, "Equalização ECC X HANA #108307   - SMC

      wa_zfit0136     like line of it_zfit0136,
      wa_zfit0137     like line of it_zfit0137,
      wa_zfit0151     like line of it_zfit0151,
      wa_zfit0195     like line of it_zfit0195, "Equalização ECC X HANA #108307   - SMC
      wa_saida_136    like line of it_saida_136,
      wa_saida_137    like line of it_saida_137,
      wa_saida_151    like line of it_saida_151,
      wa_saida_195    like line of it_saida_195, "Equalização ECC X HANA #108307   - SMC

      it_zfit0136_alv type standard table of ty_j_1bagt.


*=======================================================================
* VARIABLES
*=======================================================================
data: screen_cadastro type sy-dynnr value 0101,
      screen_a        type sy-repid,
      rd_cfop         type c value 'X',
      rd_filial_cc    type c,
      rd_filial_ctb   type c,
      rd_grupo_merc   type c, "Equalização ECC X HANA #108307   - SMC
      bt_empresa      type c, "Equalização ECC X HANA #108307   - SMC
      n_filial        type t001w-name1,
      n_centro        type cskt-ktext,
      n_cfop          type j_1bagt-cfotxt,
      cfop_aux        type j_1bcfop, "Equalização ECC X HANA #108307   - SMC
      werks_aux       type t001w-werks,
      werks_aux_2     type t001w-werks,
      kostl_aux       type cskt-kostl,
      saknr_aux       type saknr,
      matkl_aux       type t023t-matkl. "Equalização ECC X HANA #108307   - SMC
"wgbez_aux       TYPE t023t-wgbez."Equalização ECC X HANA #108307   - SMC

*=======================================================================
* ALV
*=======================================================================
class: lcl_alv_toolbar definition deferred.

data: g_container          type scrfname,
      g_container2         type scrfname,
      g_container3         type scrfname,
      g_container4         type scrfname, "Equalização ECC X HANA #108307   - SMC
      g_grid               type ref to cl_gui_alv_grid,
      g_grid2              type ref to cl_gui_alv_grid,
      g_grid3              type ref to cl_gui_alv_grid,
      g_grid4              type ref to cl_gui_alv_grid, "Equalização ECC X HANA #108307   - SMC
      g_custom_container   type ref to cl_gui_custom_container,
      g_custom_container2  type ref to cl_gui_custom_container,
      g_custom_container3  type ref to cl_gui_custom_container,
      g_custom_container4  type ref to cl_gui_custom_container, "Equalização ECC X HANA #108307   - SMC

      gt_fieldcat          type lvc_t_fcat,
      gt_fieldcat2         type lvc_t_fcat,
      gt_fieldcat3         type lvc_t_fcat,
      gt_fieldcat4         type lvc_t_fcat, "Equalização ECC X HANA #108307   - SMC
      gs_layout            type lvc_s_layo,
      i_selected_rows      type lvc_t_row,
      w_selected_rows      type lvc_s_row,

      wg_display,
      wg_mensagem(30),
      tg_selectedcell      type lvc_t_cell,
      wg_selectedcell      type lvc_s_cell,
      tg_selectedrow       type lvc_t_row,
      wg_selectedrow       type lvc_s_row,
      ty_toolbar           type stb_button,
      wa_stable            type lvc_s_stbl,
      obg_toolbar          type ref to lcl_alv_toolbar,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      tg_msg_ret           type table of zfiwrs0002 with header line.

data: wl_repid    type sy-repid,
      tl_function type ui_functions,
      wl_function like tl_function with header line.

*=======================================================================
* CONSTANTES
*=======================================================================
constants: c_x              type c value 'X',
           c_add(3)         type c value 'ADD',
           c_del(3)         type c value 'DEL',
           c_exit(4)        type c value 'EXIT',
           c_back(4)        type c value 'BACK',
           c_save(4)        type c value 'SAVE',
*          C_RED(4)         TYPE C VALUE '@0A@',
*          C_YELLOW(4)      TYPE C VALUE '@09@',
*          C_GREEN(4)       TYPE C VALUE '@08@',
*          C_AGUARD(4)      TYPE C VALUE '@9R@',
           c_proces(6)      type c value 'PROCES',
           c_cancel(6)      type c value 'CANCEL',
           c_change(6)      type c value 'CHANGE',
           c_search(6)      type c value 'SEARCH',
           c_show_msgre(10) type c value 'SHOW_MSGRE'.


*=======================================================================
* PARAMETERS
*=======================================================================
data: p_werks      type t001w-werks,
      p_kostl      type cskt-kostl,
      p_saknr      type saknr,
      p_cfop       type j_1bcfop, "Equalização ECC X HANA #108307 INICIO  - SMC
      p_tp_mercado type zfit0136-tp_mercado,
      p_butxt      type t001-butxt.
*=======================================================================
*SUBTELA - FILTRO EMPRESA
*=======================================================================
*SELECTION-SCREEN BEGIN OF SCREEN 106.
*PARAMETERS: p_bukrs TYPE t001-bukrs OBLIGATORY.
*SELECTION-SCREEN END OF SCREEN 106.
*=======================================================================

data: p_bukrs     type t001-bukrs,
      p_txt_bukrs type t001-butxt.

*=======================================================================

initialization.
  "Equalização ECC X HANA #108307 FIM - SMC

*=======================================================================
* START
*=======================================================================
start-of-selection.

  call screen 0100. "comentado Equalização ECC X HANA #108307 FIM - SMC
