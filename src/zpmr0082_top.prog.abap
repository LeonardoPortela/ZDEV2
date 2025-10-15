*&---------------------------------------------------------------------*
*&  Include           ZPMR0028_TOP
*&---------------------------------------------------------------------*
*REPORT  zpmr0028 MESSAGE-ID zpmmsg.


tables: "aufk,
  pa0001, trugt, afru, kapp, hrp1001, ztpm_d_usuario, zepm_aponta_cat_notas.
"viqmfe, viqmel, viqmur, viqmsm, viqmma, riwo00, riwo02.

"DATA: h_vflag.

data: code_ok type ok.

data: vg_erro, vg_enter.

data: lv_notif     type qmnum,
      v_nota       type qmnum,
      v_tipo_ordem type auart,
      v_nota_descr type auftext,
      v_de         type c,
      v_quant_oper type c,
      v_ktext      type aufk-ktext,
      v_maktx      type makt-maktx,
      v_ltxa1      type ltxa1,
      v_op_descrip type ltxa1,
      v_conf_enc   type char20,
      v_conf_nota  type char20,
      v_aufnr      type aufk-aufnr,
      v_vornr      type afvc-vornr,
      v_istru      type afvc-istru,
      v_ordem      type aufk-aufnr, "VALUE '000011000975'.
      v_ucomm      type sy-ucomm.

data: lv_initialized type abap_bool.

data: gt_return_save type table of bapiret2.

data: ti_bdcdata type standard table of bdcdata,
      p_resp, check, p_erro(1),
      wa_bdcdata like line of ti_bdcdata.

data: begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data: end of it_msg.

types: begin of ty_editor,
         line(72),
       end   of ty_editor.

data:
  tl_dynpselect   type table of dselc,
  tl_dynpvaluetab type table of dval,
  wl_help_info    type help_info,
  wa_viqmel       type viqmel,
  wl_selected     type help_info-fldvalue.

data: it_methods      type table of bapi_alm_order_method,
      it_header       type table of bapi_alm_order_headers_i,
      it_operation    type table of bapi_alm_order_operation,
      it_notification type standard table of bapi2080_1,
      it_zpmt0012     type table of zpmt0012, "#96115  FF  07.03.2023
      it_dados_tela   type table of zepm_aponta_cat_notas with key notifno. "#96115  FF  23.05.2023

data: it_aponta_notas type table of zepm_aponta_cat_notas.

data: wa_methods   type bapi_alm_order_method,
      wa_header    type bapi_alm_order_headers_i,
      wa_operation type bapi_alm_order_operation.

**  Begin of   "FF #96115
constants: line_length type i value 132.
constants  c_true      type  i              value  1.
constants  c_false     type  i              value  0.
data:
*   reference to wrapper class of control
  g_editor               type ref to cl_gui_textedit,
*   reference to custom container: necessary to bind TextEdit Control
  g_editor_container     type ref to cl_gui_custom_container,
  g_repid                like sy-repid,
  g_ok_code              like sy-ucomm,       " return code from screen
  g_relink               type c,               " to manage relinking
  g_mytable(line_length) type c occurs 0,
  g_mycontainer(30)      type c,      " string for the containers
  g_container_linked     type i.                            "#EC NEEDED


data: it_editor      type table of ty_editor.

data gt_texto type table of zepm_texto_longo.
data wa_texto type zepm_texto_longo.
data gt_lines type table of tline.
** End of FF

types:  begin of ty_zaponta.
          include type zoperations.
types:    aufnr      type aufk-aufnr,
          ktext      type crtx-ktext,
          werks      type aufk-werks,
          budat      type afru-budat,
*          ltxa1      type afvc-ltxa1, "US #174095 - MMSILVA - 06.05.2025 - Comentado devido DUMP por conta de duplicidade de declaração (já existe na zoperations)
          ktext1     type aufk-ktext,
          begzt      type sy-uzeit,
          endzt      type sy-uzeit,
          pause      type sy-uzeit,
          ngrad      type kako-ngrad,
          ueberlast  type kako-ueberlast,
          einzh      type p decimals 2,
          einzt      type p decimals 2,
          v_sobrcarg type p decimals 2,
          istru      type afvc-istru,
          marc       type c,
        end of ty_zaponta.

types: begin of ty_hrp1001,
         werks type crhd-werks,
         sobid type hrp1001-sobid,
         pernr type pa0001-pernr,
         sname type pa0001-sname,
         arbpl type crhd-arbpl,
         ktext type crtx-ktext,
         objid type crhd-objid,
         kapid type crca-kapid,
       end of ty_hrp1001.

types: begin of ty_aufk,
         aufnr type aufk-aufnr,
         vaplz type aufk-vaplz,
         aufpl type afko-aufpl,
         vornr type afvc-vornr,
         ltxa1 type afvc-ltxa1,
         werks type werks-werks,
         ktext type crtx-ktext,
         istru type afvc-istru, " #96115  FF
       end of ty_aufk.


types: begin of ty_epregado,
         pernr type pa0001-pernr,
         sname type pa0001-sname,
       end of ty_epregado.

types: begin of ty_selec_parada,
         grund type trugt-grund,
         grdtx type trugt-grdtx,
       end of ty_selec_parada.


types: begin of ty_afru,
         pernr type afru-pernr,
         werks type afru-werks,
         budat type afru-budat,
         isdd  type afru-isdd,
         isdz  type afru-isdz,
         iedd  type afru-iedd,
         iedz  type afru-iedz,
         aufnr type afru-aufnr,
         vornr type afru-vornr,
         ismnw type afru-ismnw,
         ismne type afru-ismne,
       end of ty_afru.

types: begin of ty_kako,
         kapid     type kako-kapid, "ID da capacidade
         werks     type kako-werks, " Centro
         begzt     type p decimals 2, " Hora de início em segundos (interno)
         endzt     type p decimals 2, " Hora do fim em segundos (interno)
         pause     type p decimals 2,  "Horário de intervalo acumulado em segundos (interno)
         ueberlast type kako-ueberlast, "Sobrecarga
         einzt     type p decimals 2,    "Tempo de utilização em segundos (interno)
         einzh     type p decimals 2,
         ngrad     type kako-ngrad,
         v_begzt   type p decimals 2,
         v_endzt   type p decimals 2,
         v_pause   type p decimals 2,
         v_einzt   type p decimals 2,
         v_einzh   type p decimals 2,
       end of ty_kako.

types:
  begin of ty_req_pend,
    banfn type eban-banfn,  " Numero da requisição
  end of ty_req_pend,

  begin of ty_ped_pend,
    ebeln type eban-ebeln,  " Nº pedido
  end of ty_ped_pend.

data: it_selec_parada type table of ty_selec_parada with header line.

data: it_aponta        type table of ty_zaponta,
      it_aponta_export type table of ty_zaponta,
      zaponta          type ty_zaponta.
data: it_aufk type table of ty_aufk with header line.
data: it_afvc type table of ty_aufk with header line.
data: it_empregado type table of ty_hrp1001 with header line.
data: it_cent_trab type table of ty_hrp1001 with header line.
data: it_crtx type table of crtx with header line.
data: it_crhd type table of crhd with header line.
data: t_afru type table of ty_afru with header line.
data: tl_afru type table of ty_afru with header line.
data: it_kako type table of ty_kako with header line.
data: it_select_rows type lvc_t_row.
data: wa_select_rows type lvc_s_row.
data: t_req_pend type table of ty_req_pend with header line.
data: t_ped_pend type table of ty_ped_pend with header line.

data: w_cursor_field type c length 50,
      ws_viqmel      type viqmel.

data: vg_dt_hr_init_parada TYPE char12,
      vg_dt_hr_init_apont  TYPE char12.

data: it_hrp1001 type table of ty_hrp1001 with header line.

data: tl_return_tab type table of ddshretval with header line,
      tl_dselc      type table of dselc      with header line.

data: soma_data  type p decimals 2,
      soma_hora  type p decimals 2,
      soma_total type p decimals 2.

data:ld_no_day     type i,
     ld_no_month   type i,
     ld_no_year    type i,
     ld_no_cal_day type i.

data:
  g_custom_container   type ref to cl_gui_custom_container,
  g_custom_container_1 type ref to cl_gui_custom_container,
  g_custom_container_2 type ref to cl_gui_custom_container,
  dg_parent_alv        type ref to cl_gui_container,
  ctl_alv              type ref to cl_gui_alv_grid,
  dg_parent_alv_1      type ref to cl_gui_container,
  dg_parent_alv_2      type ref to cl_gui_container,
  ctl_alv_1            type ref to cl_gui_alv_grid,
  ctl_alv_2            type ref to cl_gui_alv_grid,
  it_exclude_fcode     type ui_functions,
  wa_exclude_fcode     like line of it_exclude_fcode,
  it_exclude_fcode_1   type ui_functions,
  wa_exclude_fcode_1   like line of it_exclude_fcode,
  it_exclude_fcode_2   type ui_functions,
  wa_exclude_fcode_2   like line of it_exclude_fcode,
  gs_layout            type lvc_s_layo,
  gs_layout_1          type lvc_s_layo,
  gs_layout_2          type lvc_s_layo,
  gs_variant           type disvariant,
  gs_variant_1         type disvariant,
  gs_variant_2         type disvariant,
  it_fieldcatalog      type lvc_t_fcat,
  it_fieldcatalog_1    type lvc_t_fcat,
  it_fieldcatalog_2    type lvc_t_fcat,
  wa_fieldcatalog      type lvc_s_fcat,
  it_sort              type lvc_t_sort,
  ls_stable            type lvc_s_stbl,
  it_sort_1            type lvc_t_sort,
  ls_stable_1          type lvc_s_stbl,
  it_sort_2            type lvc_t_sort,
  ls_stable_2          type lvc_s_stbl,
  obj_grid_empregado   type ref to cl_gui_alv_grid.


class zcl_events definition.
  public section.

    data: ls_good         type lvc_s_modi.

    methods:
      on_dt_ch for event data_changed  of cl_gui_alv_grid
        importing er_data_changed  e_onf4 e_onf4_before e_onf4_after e_ucomm sender,
      on_onf4 for event onf4 of cl_gui_alv_grid
        importing e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display,
      on_dt_ch_fs for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells sender,
      on_click for event hotspot_click of cl_gui_alv_grid
        importing e_row_id e_column_id es_row_no.

endclass.

class zcl_main definition.

  public section.
    data:
      header          type bapi_alm_order_header_e,



      it_nothdre      type bapi2080_nothdre,
      nothdre         type bapi2080_nothdre,
      tl_header       type table of bapi_alm_order_headers_i,
      tw_header       type bapi_alm_order_headers_i,
      tl_header_up    type table of bapi_alm_order_headers_up,
      tl_partner      type table of bapi_alm_order_partn_mul,
      tl_partner_up   type table of bapi_alm_order_partn_mul_up,
      tl_operation    type table of bapi_alm_order_operation,
      tl_operation_up type table of bapi_alm_order_operation_up,
      tl_numbers      type table of bapi_alm_numbers,
      _data           type ref to lvc_t_data,
      acao            type sy-ucomm.

    methods:
      screen, get_header importing input type aufnr optional
        returning value(return) type sy-subrc, set_operations,

      get_item importing input type qmnum optional returning
      value(return) type sy-subrc,

      get_pernr importing input type sy-tabix optional
        returning value(return) type sname,

      get_grund importing input type sy-tabix optional
        returning value(return) type co_grdtx,

      get_activity importing input type any optional
      input1 type sy-tabix optional,

      get_desc importing input  type sy-tabix optional
      input1 type any optional
      field  type char30 optional
      table  type char11 optional,

      f4_activity importing table type standard table optional
      field type char30 optional
      index type sy-tabix optional,

      refresh importing input type ref to cl_gui_alv_grid optional,
      help_start importing input type sy-tabix optional
      field type char30 optional
      table type char11 optional,

      code_f4 importing input type sy-tabix optional
      field type char30 optional
      table type char11 optional,

      set_f4 importing table type standard table optional
      field type char30 optional
      index type sy-tabix optional,

      get_cat_code importing field type char30 optional
      index type sy-tabix optional,

      set_code importing field type char30 optional
      index type sy-tabix optional,

      set_txt importing field type char30 optional
      index type sy-tabix optional,

      set_code_group importing field type char30 optional
      index type sy-tabix optional,

      set_item, set_estrutura importing input type aufnr optional, pbo,

      set_layout importing input type char5 optional, set_fcat
      importing input type char30 optional,

      set_block,
      set_alv1,
      set_alv2,
      set_alv3,
      set_alv4,
      set_alv5,
      set_alv6,
      set_apontar,
      set_encerra_ordem,
      set_encerra_ordem_nota,
      set_edit_nota,
      set_save_nota,
      set_avaria importing i_grava   type char1 optional
                           i_ini     type char1 optional
                           i_fim     type char1 optional
                           iw_viqmel type viqmel optional,
      set_get_return,
      add_line,
      set_notificacao
       importing input  type char1 optional
                 input1 type char26 optional, save_commit.


*  PRIVATE SECTION.

    types: begin of ty_pa001,
             pernr type persno,
             sname type sname,
           end of ty_pa001,

           begin of ty_ucomm,
             ucomm type  sy-ucomm,
           end of ty_ucomm,

           begin of ty_code_group,
             code_group type qcodegrp,
             shorttxtgr type qktextgr,
           end of ty_code_group,

           begin of ty_code,
             code_group type qcodegrp,
             code       type qcode,
             shorttxtcd type qktextcd,
           end of ty_code,

           begin of ty_f4activity,
             activity    type activity,
             work_cntr   type arbpl,
             description type ltxa1,
           end of ty_f4activity,

           begin of ty_aufk,
             aufnr type aufk-aufnr,
             aufpl type afko-aufpl,
             objnr type afvc-objnr,
             vornr type afvc-vornr,
             aueru type afru-aueru,
             ktext type aufk-ktext,
             ltxa1 type afvc-ltxa1,
             werks type aufk-werks,
             vaplz type aufk-vaplz,
             istru type afvc-istru, " #96115  FF
           end of ty_aufk,

           begin of ty_afru,
             aufnr type afru-aufnr,
             vornr type afru-vornr,
             aueru type afru-aueru,
             aufpl type afru-aufpl, "#96115  FF  07.03.2023
           end of ty_afru,

           begin of ty_afvc,
             aufnr type afko-aufnr,
             aufpl type afvc-aufpl,
             ltxa1 type afvc-ltxa1,
           end of ty_afvc.


    data:
      it_opera     type table of ty_zaponta, "ZOPERATIONS,
      it_opera_aux type table of ty_zaponta, "ZOPERATIONS,
      zaponta      type ty_zaponta, "ZOPERATIONS,
      it_niobj     type table of znotitemobj,
      it_nidef     type table of znotitemdef,
      it_nicau     type table of znotifcaus,
      it_nitas     type table of znotiftask,
      it_nifac     type table of znotifactv,
      it_pa001     type table of ty_pa001.

    data:
      it_operat             type table of bapi_alm_order_operation_e,
      it_olist              type table of bapi_alm_order_objectlist,
      it_timetickets        type table of bapi_alm_timeconfirmation,
      codes1                type table of bapi10011t,
      it_code_group         type table of ty_code_group,
      it_code               type table of ty_code,
      it_return             type table of bapiret2,
      it_return_add         type table of bapiret2,
      it_return_change      type table of bapiret2,
      it_return_del         type table of bapiret2,
      i_return              type table of bapiret2,
      t_return              type table of bapiret2,
      wa_return             type bapiret2,
      tl_return             type table of ddshretval,
      tg_return             type table of bapi_alm_return,
      it_notif              type  bapi2080_nothdre,          "notifheader_export
      it_notif_h            type  bapi2080_nothdtxte,
      it_notiteme           type table of bapi2080_notiteme, "notitem it_niobj e it_nidef
      it_notcause           type table of bapi2080_notcause, "notifcaus IT_NICAU
      it_notactve           type table of bapi2080_notactve, "notifactv IT_NIFAC
      it_nottaske           type table of bapi2080_nottaske, "notiftask IT_NITAS
      tnotifheader_export   type  bapi2080_nothdre,
      tnotifheader          type  bapi2080_nothdri,
      tnotifheader_x        type  bapi2080_nothdri_x,
      gs_notifhdtext        type  bapi2080_nothdtxte,
      gs_notifheader_export	type 	bapi2080_nothdre,
      gs_notifheader        type  bapi2080_nothdri,
      tnotfulltxt           type table of bapi2080_notfulltxti,
      tnotifitem            type table of  bapi2080_notitemi,
      tnotifitem_x          type table of  bapi2080_notitemi_x,
      tnotifcaus            type table of  bapi2080_notcausi,
      tnotifcaus_x          type table of  bapi2080_notcausi_x,
      tnotifactv            type table of  bapi2080_notactvi,
      tnotifactv_x          type table of  bapi2080_notactvi_x,
      tnotiftask            type table of  bapi2080_nottaski,
      tnotiftask_x          type table of  bapi2080_nottaski_x,
      it_f4activity         type table of ty_f4activity.

    data: it_methods      type table of bapi_alm_order_method,
          it_header       type table of bapi_alm_order_headers_i,
          it_operation    type table of bapi_alm_order_operation,
          it_notification type standard table of bapi2080_1.

    data: wa_methods   type bapi_alm_order_method,
          wa_header    type bapi_alm_order_headers_i,
          wa_operation type bapi_alm_order_operation.

    data:
      ld_systemstatus	type bapi2080_notadt,
      ld_number	      type bapi2080_nothdre, "-NOTIF_NO,
      ld_userstatus	  type bapi2080_notadt-usrstatus,
      ld_syststat	    type bapi2080_notsti,
      ld_testrun      type bapi20783t-status_ind.


    data:
      at_code       type char8,
      at_code_group type char8,
      at_cat_typ    type otkat.

    data:
      alv1     type ref to cl_gui_alv_grid,
      alv2     type ref to cl_gui_alv_grid,
      alv3     type ref to cl_gui_alv_grid,
      alv4     type ref to cl_gui_alv_grid,
      alv5     type ref to cl_gui_alv_grid,
      alv6     type ref to cl_gui_alv_grid,

      cont1    type ref to cl_gui_custom_container,
      cont2    type ref to cl_gui_custom_container,
      cont3    type ref to cl_gui_custom_container,
      cont4    type ref to cl_gui_custom_container,
      cont5    type ref to cl_gui_custom_container,
      cont6    type ref to cl_gui_custom_container,

      lt_f4    type lvc_t_f4,

      fcat     type lvc_t_fcat,
      fcat_aux type lvc_t_fcat,
      str      type ref to data,
      it_exc   type ui_functions,
      variant  type disvariant,
      layout   type lvc_s_layo,
      estilo   type lvc_t_styl,
      stable   type lvc_s_stbl.

    data: status  type lvc_style,
          ativo   type lvc_style value cl_gui_alv_grid=>mc_style_enabled,
          inativo type lvc_style value cl_gui_alv_grid=>mc_style_disabled.

    data: it_ucomm type table of ty_ucomm.
    data: t_ucomm type table of ty_ucomm.


    data: it_afvc type table of ty_afvc,
          wa_afvc type ty_afvc,
          t_aufk  type table of ty_aufk,
          t_afru  type table of ty_afru,
          t_afko  type table of ty_aufk,
*          T_AFVC  TYPE TABLE OF TY_AFRU,
          wa_aufk type ty_aufk.

*    DATA: LV_NOTIF     TYPE QMNUM,
*          V_NOTA       TYPE QMNUM,
*          V_NOTA_DESCR TYPE AUFTEXT,
*          V_ORDEM      TYPE AUFNR. "VALUE '000011000975'.

endclass.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_APONTA' ITSELF
controls: tc_aponta type tableview using screen 0201.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_APONTA'
data:     g_tc_aponta_lines  like sy-loopc.

data:     enter like sy-ucomm.



**  Begin of    #96115  FF
*DATA  g_auth_check VALUE 'X'.                               "note191299
*
*
*
*DATA  x_display.                                            "n672526
*ENHANCEMENT-POINT LIQS0TOP_01 SPOTS ES_SAPLIQS0 STATIC.
*
*TYPE-POOLS: iqs0.
*TYPE-POOLS: itob.
*TYPE-POOLS: ito0c.
*TYPE-POOLS: ito2c.
*TYPE-POOLS: shlp.
*TYPE-POOLS: sbdst .
*INCLUDE liqs0tpt.            "Tabellen definition
*INCLUDE liqs0tps.            "Steuerungsvariablen
*INCLUDE liqs0tpc.            "Konstanten
*INCLUDE liqs0tpq.            "Datendeklaration für QMeldungen
*INCLUDE liqs0tpf.            "Funktionsbezogene Variablen
*INCLUDE liqs0tab.            "Tabstripdaten
*INCLUDE liqs0tox.            "Textcontrol OCX
*INCLUDE mixadt00.            "Schnittstelle Adressverwaltung
*INCLUDE sapcamfl.            "Adressverwaltung SD
*INCLUDE liqs1tts.            "Time Segment
*INCLUDE claim_010.
*INCLUDE qmel_uc_classes.     "local classes for calculation
*INCLUDE qmel_uc_data.        "special data for calculation
*INCLUDE eaml_iqs0top.        "EHP605
*
*
*DATA: last-syst-ucomm LIKE syst-ucomm.
*
** BADI NOTIF_CREATE_OBJ
*DATA: gv_notif_create_obj TYPE REF TO if_ex_notif_create_obj.
** BADI NOTIF_AUTHORITY_01                                  "note638734
*DATA: gv_notif_authority_01 TYPE REF TO if_ex_notif_authority_01.
** BADI WOC_FL_Determine                                    "note535366
*DATA: gv_woc_fl_determine  TYPE REF TO if_ex_woc_fl_determine.
** BADI  IWO1_WKCTR_CHANGE                                  "note
*DATA : gv_iwo1_wkctr_change TYPE REF TO if_ex_iwo1_wkctr_change.
**---------------------------------------------------------------------*
**Definition BAdI WPS_CONNECTION       P6BK075291
**---------------------------------------------------------------------*
*DATA: gv_instance_revision TYPE REF TO if_ex_wps_connection.
*DATA: gv_instance_revision_ini TYPE flag.
*
*DATA: gv_sfbcall_reset         TYPE flag.                   "967195
*DATA: gv_old_sfbcall_xfbcall   TYPE flag.                   "967195
*DATA: gv_old_sfbcall_aufnr     TYPE viqmel-aufnr.           "967195
*DATA: gv_old_sfbcall_coer_back TYPE flag.                   "967195
*ENHANCEMENT-POINT LIQS0TOP_02 SPOTS ES_SAPLIQS0 STATIC .
*
**{   INSERT         DFPS  -  Verteilung  -
*
*DATA:
*  gv_flg_distribution  TYPE  flag.     "Auftragsverteilung->Komponenten
*
**}   INSERT
*
*
** Start of Definition DFPS Badi (technical Status)
*INCLUDE dfps_tst_badi_top.
**&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_RIWO00' ITSELF
**CONTROLS: TC_RIWO00 TYPE TABLEVIEW USING SCREEN 8050.
*
**--------------------------------------------------------------------*
**--- QNAO start of definition part for assigned objects
*INCLUDE liqs0pao.
**--------------------------------------------------------------------*
**--- implementation part for assigned objects of items
*INCLUDE liqs0oao.
*
** Data needed for BAdI BADI_SAPLIQS0_EXTPOP
*DATA: BEGIN OF gs_extpop,
*        badi       TYPE REF TO badi_sapliqs0_extpop,
*        size       TYPE char1,
*        applid     TYPE char13,
*        repid      TYPE syrepid,
*        dynnr      TYPE dynnr,
*        title      TYPE sytitle,
*        title_par1 TYPE cua_tit_tx,
*        title_par2 TYPE cua_tit_tx,
*        status     TYPE sypfkey,
*        ok_code    TYPE fcode,
*      END OF gs_extpop.
**--- declaration for time zone support
*TYPE-POOLS: tzs1.
*
*TYPES: BEGIN OF gtype_s_tzone_table_conv, " for structures in table controls or steploops
*         stepl        TYPE systepl,
*         converted(1) TYPE c,
*       END OF gtype_s_tzone_table_conv,
*
*       gtype_t_tzone_table_conv TYPE SORTED TABLE OF gtype_s_tzone_table_conv WITH UNIQUE KEY stepl.
*
*DATA: gb_badi_time_zone    TYPE REF TO badi_eam_tz_iwoc_core,
*      gb_badi_time_zone_gen    TYPE REF TO badi_eam_tz_generic_core,
*      gv_time_zone_active  TYPE tz_d_active,
*      gv_tzone_conv_flag   TYPE flag,
*      gv_tzone_mzeit_flag   TYPE flag,
*      gv_tzone_strur_flag   TYPE flag,
*      gv_time_zone_init     TYPE flag,
*      gv_tzone_auztv_flag   TYPE flag,
*      gv_tzone_strur_init TYPE flag,
*      gt_tzone_viqmma_flag  TYPE gtype_t_tzone_table_conv,
*      gt_tzone_viqmsm_flag  TYPE gtype_t_tzone_table_conv.
*
*DATA: s_html_container TYPE REF TO cl_gui_custom_container. "N1161708
*DATA: s_html_control TYPE REF TO cl_gui_html_viewer.        "N1161708
*DATA: gv_notif_change  TYPE flag .                          "N1319354
*DATA: gv_qmnum         TYPE viqmel-qmnum.                   "N1319354
*DATA: gv_active        TYPE flag.                           "N1319354
*
**--------------------------------------------------------------------------------
** side panal addition - note 1816377
*
* DATA: gv_tag_value TYPE string.
*
**--------------------------------------------------------------------------------
*
** End of FF
