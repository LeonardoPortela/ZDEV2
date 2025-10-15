*&---------------------------------------------------------------------*
*&  Include           ZPMR0028_TOP
*&---------------------------------------------------------------------*

TABLES: aufk, pa0001, trugt, afru, kapp, hrp1001.

DATA: code_ok TYPE ok.



DATA: lv_notif     TYPE qmnum,
      v_nota       TYPE qmnum,
      v_nota_descr TYPE auftext,
      v_ktext      TYPE aufk-ktext,
      v_ltxa1      TYPE ltxa1,
      v_conf_enc   TYPE char20,
      v_conf_nota  TYPE char20,
      v_aufnr      TYPE aufk-aufnr,
      v_vornr      TYPE afvc-vornr,
      v_ordem      TYPE aufk-aufnr. "VALUE '000011000975'.

DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata,
      p_resp, check, p_erro(1),
      wa_bdcdata LIKE LINE OF ti_bdcdata.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

DATA:
  tl_dynpselect   TYPE TABLE OF dselc,
  tl_dynpvaluetab TYPE TABLE OF dval,
  wl_help_info    TYPE help_info,
  wl_selected     TYPE help_info-fldvalue.

DATA: it_methods      TYPE TABLE OF bapi_alm_order_method,
      it_header       TYPE TABLE OF bapi_alm_order_headers_i,
      it_operation    TYPE TABLE OF bapi_alm_order_operation,
      it_notification TYPE STANDARD TABLE OF bapi2080_1.

DATA: wa_methods   TYPE bapi_alm_order_method,
      wa_header    TYPE bapi_alm_order_headers_i,
      wa_operation TYPE bapi_alm_order_operation.


TYPES:  BEGIN OF ty_zaponta.
          INCLUDE TYPE zoperations.
TYPES:    aufnr      TYPE aufk-aufnr,
          ktext      TYPE crtx-ktext,
          werks      TYPE aufk-werks,
          budat      TYPE afru-budat,
*          ltxa1      TYPE afvc-ltxa1,
          ktext1     TYPE aufk-ktext,
          begzt      TYPE sy-uzeit,
          endzt      TYPE sy-uzeit,
          pause      TYPE sy-uzeit,
          ngrad      TYPE kako-ngrad,
          ueberlast  TYPE kako-ueberlast,
          einzh      TYPE p DECIMALS 2,
          einzt      TYPE p DECIMALS 2,
          v_sobrcarg TYPE p DECIMALS 2,
          marc       TYPE c,
        END OF ty_zaponta.

TYPES: BEGIN OF ty_hrp1001,
         werks TYPE crhd-werks,
         sobid TYPE hrp1001-sobid,
         pernr TYPE pa0001-pernr,
         sname TYPE pa0001-sname,
         arbpl TYPE crhd-arbpl,
         ktext TYPE crtx-ktext,
         objid TYPE crhd-objid,
         kapid TYPE crca-kapid,
       END OF ty_hrp1001.

TYPES: BEGIN OF ty_aufk,
         aufnr TYPE aufk-aufnr,
         vaplz TYPE aufk-vaplz,
         aufpl TYPE afko-aufpl,
         vornr TYPE afvc-vornr,
         ltxa1 TYPE afvc-ltxa1,
         werks TYPE werks-werks,
         ktext TYPE crtx-ktext,
       END OF ty_aufk.


TYPES: BEGIN OF ty_epregado,
         pernr TYPE pa0001-pernr,
         sname TYPE pa0001-sname,
       END OF ty_epregado.

TYPES: BEGIN OF ty_selec_parada,
         grund TYPE trugt-grund,
         grdtx TYPE trugt-grdtx,
       END OF ty_selec_parada.


TYPES: BEGIN OF ty_afru,
         pernr TYPE afru-pernr,
         werks TYPE afru-werks,
         budat TYPE afru-budat,
         isdd  TYPE afru-isdd,
         isdz  TYPE afru-isdz,
         iedd  TYPE afru-iedd,
         iedz  TYPE afru-iedz,
         aufnr TYPE afru-aufnr,
         vornr TYPE afru-vornr,
         ismnw TYPE afru-ismnw,
         ismne TYPE afru-ismne,
       END OF ty_afru.

TYPES: BEGIN OF ty_kako,
         kapid     TYPE kako-kapid, "ID da capacidade
         werks     TYPE kako-werks, " Centro
         begzt     TYPE p DECIMALS 2, " Hora de início em segundos (interno)
         endzt     TYPE p DECIMALS 2, " Hora do fim em segundos (interno)
         pause     TYPE p DECIMALS 2,  "Horário de intervalo acumulado em segundos (interno)
         ueberlast TYPE kako-ueberlast, "Sobrecarga
         einzt     TYPE p DECIMALS 2,    "Tempo de utilização em segundos (interno)
         einzh     TYPE p DECIMALS 2,
         ngrad     TYPE kako-ngrad,
         v_begzt   TYPE p DECIMALS 2,
         v_endzt   TYPE p DECIMALS 2,
         v_pause   TYPE p DECIMALS 2,
         v_einzt   TYPE p DECIMALS 2,
         v_einzh   TYPE p DECIMALS 2,
       END OF ty_kako.

TYPES:
  BEGIN OF ty_req_pend,
    banfn TYPE eban-banfn,  " Numero da requisição
  END OF ty_req_pend,

  BEGIN OF ty_ped_pend,
    ebeln TYPE eban-ebeln,  " Nº pedido
  END OF ty_ped_pend.

DATA: it_selec_parada TYPE TABLE OF ty_selec_parada WITH HEADER LINE.

DATA: it_aponta TYPE TABLE OF ty_zaponta,
      zaponta   TYPE ty_zaponta.
DATA: it_aufk TYPE TABLE OF ty_aufk WITH HEADER LINE.
DATA: it_afvc TYPE TABLE OF ty_aufk WITH HEADER LINE.
DATA: it_empregado TYPE TABLE OF ty_hrp1001 WITH HEADER LINE.
DATA: it_cent_trab TYPE TABLE OF ty_hrp1001 WITH HEADER LINE.
DATA: it_crtx TYPE TABLE OF crtx WITH HEADER LINE.
DATA: it_crhd TYPE TABLE OF crhd WITH HEADER LINE.
DATA: t_afru TYPE TABLE OF ty_afru WITH HEADER LINE.
DATA: tl_afru TYPE TABLE OF ty_afru WITH HEADER LINE.
DATA: it_kako TYPE TABLE OF ty_kako WITH HEADER LINE.
DATA: it_select_rows TYPE lvc_t_row.
DATA: wa_select_rows TYPE lvc_s_row.
DATA: t_req_pend TYPE TABLE OF ty_req_pend WITH HEADER LINE.
DATA: t_ped_pend TYPE TABLE OF ty_ped_pend WITH HEADER LINE.

DATA: w_cursor_field TYPE c LENGTH 50.

DATA: it_hrp1001 TYPE TABLE OF ty_hrp1001 WITH HEADER LINE.

DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
      tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

DATA: soma_data  TYPE p DECIMALS 2,
      soma_hora  TYPE p DECIMALS 2,
      soma_total TYPE p DECIMALS 2.

DATA:ld_no_day     TYPE i,
     ld_no_month   TYPE i,
     ld_no_year    TYPE i,
     ld_no_cal_day TYPE i.

DATA:
  g_custom_container   TYPE REF TO cl_gui_custom_container,
  g_custom_container_1 TYPE REF TO cl_gui_custom_container,
  g_custom_container_2 TYPE REF TO cl_gui_custom_container,
  dg_parent_alv        TYPE REF TO cl_gui_container,
  ctl_alv              TYPE REF TO cl_gui_alv_grid,
  dg_parent_alv_1      TYPE REF TO cl_gui_container,
  dg_parent_alv_2      TYPE REF TO cl_gui_container,
  ctl_alv_1            TYPE REF TO cl_gui_alv_grid,
  ctl_alv_2            TYPE REF TO cl_gui_alv_grid,
  it_exclude_fcode     TYPE ui_functions,
  wa_exclude_fcode     LIKE LINE OF it_exclude_fcode,
  it_exclude_fcode_1   TYPE ui_functions,
  wa_exclude_fcode_1   LIKE LINE OF it_exclude_fcode,
  it_exclude_fcode_2   TYPE ui_functions,
  wa_exclude_fcode_2   LIKE LINE OF it_exclude_fcode,
  gs_layout            TYPE lvc_s_layo,
  gs_layout_1          TYPE lvc_s_layo,
  gs_layout_2          TYPE lvc_s_layo,
  gs_variant           TYPE disvariant,
  gs_variant_1         TYPE disvariant,
  gs_variant_2         TYPE disvariant,
  it_fieldcatalog      TYPE lvc_t_fcat,
  it_fieldcatalog_1    TYPE lvc_t_fcat,
  it_fieldcatalog_2    TYPE lvc_t_fcat,
  wa_fieldcatalog      TYPE lvc_s_fcat,
  it_sort              TYPE lvc_t_sort,
  ls_stable            TYPE lvc_s_stbl,
  it_sort_1            TYPE lvc_t_sort,
  ls_stable_1          TYPE lvc_s_stbl,
  it_sort_2            TYPE lvc_t_sort,
  ls_stable_2          TYPE lvc_s_stbl,
  obj_grid_empregado   TYPE REF TO cl_gui_alv_grid.

DATA: wa_selected_rows            TYPE lvc_s_row,
      it_selected_rows            TYPE lvc_t_row,
      lines                       TYPE sy-tabix.


CLASS zcl_events DEFINITION.
  PUBLIC SECTION.

    DATA: ls_good         TYPE lvc_s_modi.

    METHODS:
      on_dt_ch FOR EVENT data_changed  OF cl_gui_alv_grid
        IMPORTING er_data_changed  e_onf4 e_onf4_before e_onf4_after e_ucomm sender,
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display,
      on_dt_ch_fs FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender,
      on_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.

CLASS zcl_main DEFINITION.

  PUBLIC SECTION.
    DATA:
      header          TYPE bapi_alm_order_header_e,



      it_nothdre      TYPE bapi2080_nothdre,
      nothdre         TYPE bapi2080_nothdre,
      tl_header       TYPE TABLE OF bapi_alm_order_headers_i,
      tw_header       TYPE bapi_alm_order_headers_i,
      tl_header_up    TYPE TABLE OF bapi_alm_order_headers_up,
      tl_partner      TYPE TABLE OF bapi_alm_order_partn_mul,
      tl_partner_up   TYPE TABLE OF bapi_alm_order_partn_mul_up,
      tl_operation    TYPE TABLE OF bapi_alm_order_operation,
      tl_operation_up TYPE TABLE OF bapi_alm_order_operation_up,
      tl_numbers      TYPE TABLE OF bapi_alm_numbers,
      _data           TYPE REF TO lvc_t_data,
      acao            TYPE sy-ucomm.

    METHODS:
      screen, get_header IMPORTING input TYPE aufnr OPTIONAL
        RETURNING VALUE(return) TYPE sy-subrc, set_operations,

      get_item IMPORTING input TYPE qmnum OPTIONAL RETURNING
      VALUE(return) TYPE sy-subrc,

      get_pernr IMPORTING input TYPE sy-tabix OPTIONAL
        RETURNING VALUE(return) TYPE sname,

      get_grund IMPORTING input TYPE sy-tabix OPTIONAL
        RETURNING VALUE(return) TYPE co_grdtx,

      get_activity IMPORTING input TYPE any OPTIONAL
      input1 TYPE sy-tabix OPTIONAL,

      get_desc IMPORTING input  TYPE sy-tabix OPTIONAL
      input1 TYPE any OPTIONAL
      field  TYPE char30 OPTIONAL
      table  TYPE char11 OPTIONAL,

      f4_activity IMPORTING table TYPE STANDARD TABLE OPTIONAL
      field TYPE char30 OPTIONAL
      index TYPE sy-tabix OPTIONAL,

      refresh IMPORTING input TYPE REF TO cl_gui_alv_grid OPTIONAL,
      help_start IMPORTING input TYPE sy-tabix OPTIONAL
      field TYPE char30 OPTIONAL
      table TYPE char11 OPTIONAL,

      code_f4 IMPORTING input TYPE sy-tabix OPTIONAL
      field TYPE char30 OPTIONAL
      table TYPE char11 OPTIONAL,

      set_f4 IMPORTING table TYPE STANDARD TABLE OPTIONAL
      field TYPE char30 OPTIONAL
      index TYPE sy-tabix OPTIONAL,

      get_cat_code IMPORTING field TYPE char30 OPTIONAL
      index TYPE sy-tabix OPTIONAL,

      set_code IMPORTING field TYPE char30 OPTIONAL
      index TYPE sy-tabix OPTIONAL,

      set_txt IMPORTING field TYPE char30 OPTIONAL
      index TYPE sy-tabix OPTIONAL,

      set_code_group IMPORTING field TYPE char30 OPTIONAL
      index TYPE sy-tabix OPTIONAL,

      set_item, set_estrutura IMPORTING input TYPE aufnr OPTIONAL, pbo,

      set_layout IMPORTING input TYPE char5 OPTIONAL, set_fcat
      IMPORTING input TYPE char30 OPTIONAL,

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
      set_get_return,
      add_line,
      set_notificacao
       IMPORTING input  TYPE char1 OPTIONAL
                 input1 TYPE char26 OPTIONAL, save_commit.


*  PRIVATE SECTION.

    TYPES: BEGIN OF ty_pa001,
             pernr TYPE persno,
             sname TYPE sname,
           END OF ty_pa001,

           BEGIN OF ty_ucomm,
             ucomm TYPE  sy-ucomm,
           END OF ty_ucomm,

           BEGIN OF ty_code_group,
             code_group TYPE qcodegrp,
             shorttxtgr TYPE qktextgr,
           END OF ty_code_group,

           BEGIN OF ty_code,
             code_group TYPE qcodegrp,
             code       TYPE qcode,
             shorttxtcd TYPE qktextcd,
           END OF ty_code,

           BEGIN OF ty_f4activity,
             activity    TYPE activity,
             work_cntr   TYPE arbpl,
             description TYPE ltxa1,
           END OF ty_f4activity,

           BEGIN OF ty_aufk,
             aufnr TYPE aufk-aufnr,
             aufpl TYPE afko-aufpl,
             objnr TYPE afvc-objnr,
             vornr TYPE afvc-vornr,
             aueru TYPE afru-aueru,
             ktext TYPE aufk-ktext,
             ltxa1 TYPE afvc-ltxa1,
             werks TYPE aufk-werks,
             vaplz TYPE aufk-vaplz,
           END OF ty_aufk,

           BEGIN OF ty_afru,
             aufnr TYPE afru-aufnr,
             vornr TYPE afru-vornr,
             aueru TYPE afru-aueru,
           END OF ty_afru,

           BEGIN OF ty_afvc,
             aufnr TYPE afko-aufnr,
             aufpl TYPE afvc-aufpl,
             ltxa1 TYPE afvc-ltxa1,
           END OF ty_afvc.


    DATA:
      it_opera TYPE TABLE OF ty_zaponta, "ZOPERATIONS,
      zaponta  TYPE ty_zaponta, "ZOPERATIONS,
      it_niobj TYPE TABLE OF znotitemobj,
      it_nidef TYPE TABLE OF znotitemdef,
      it_nicau TYPE TABLE OF znotifcaus,
      it_nitas TYPE TABLE OF znotiftask,
      it_nifac TYPE TABLE OF znotifactv,
      it_pa001 TYPE TABLE OF ty_pa001.

    DATA:
      it_operat      TYPE TABLE OF bapi_alm_order_operation_e,
      it_olist       TYPE TABLE OF bapi_alm_order_objectlist,
      it_timetickets TYPE TABLE OF bapi_alm_timeconfirmation,
      codes1         TYPE TABLE OF bapi10011t,
      it_code_group  TYPE TABLE OF ty_code_group,
      it_code        TYPE TABLE OF ty_code,
      it_return      TYPE TABLE OF bapiret2,
      i_return       TYPE TABLE OF bapiret2,
      t_return       TYPE TABLE OF bapiret2,
      wa_return      TYPE bapiret2,
      tl_return      TYPE TABLE OF ddshretval,
      tg_return      TYPE TABLE OF bapi_alm_return,
      it_notif       TYPE  bapi2080_nothdre,          "notifheader_export
      it_notif_h     TYPE  bapi2080_nothdtxte,
      it_notiteme    TYPE TABLE OF bapi2080_notiteme, "notitem it_niobj e it_nidef
      it_notcause    TYPE TABLE OF bapi2080_notcause, "notifcaus IT_NICAU
      it_notactve    TYPE TABLE OF bapi2080_notactve, "notifactv IT_NIFAC
      it_nottaske    TYPE TABLE OF bapi2080_nottaske, "notiftask IT_NITAS

      tnotifitem     TYPE TABLE OF  bapi2080_notitemi,
      tnotifitem_x   TYPE TABLE OF  bapi2080_notitemi_x,
      tnotifcaus     TYPE TABLE OF  bapi2080_notcausi,
      tnotifcaus_x   TYPE TABLE OF  bapi2080_notcausi_x,
      tnotifactv     TYPE TABLE OF  bapi2080_notactvi,
      tnotifactv_x   TYPE TABLE OF  bapi2080_notactvi_x,
      tnotiftask     TYPE TABLE OF  bapi2080_nottaski,
      tnotiftask_x   TYPE TABLE OF  bapi2080_nottaski_x,
      it_f4activity  TYPE TABLE OF ty_f4activity.

    DATA: it_methods      TYPE TABLE OF bapi_alm_order_method,
          it_header       TYPE TABLE OF bapi_alm_order_headers_i,
          it_operation    TYPE TABLE OF bapi_alm_order_operation,
          it_notification TYPE STANDARD TABLE OF bapi2080_1.

    DATA: wa_methods   TYPE bapi_alm_order_method,
          wa_header    TYPE bapi_alm_order_headers_i,
          wa_operation TYPE bapi_alm_order_operation.

    DATA:
      ld_systemstatus	TYPE bapi2080_notadt,
      ld_number	      TYPE bapi2080_nothdre, "-NOTIF_NO,
      ld_userstatus	  TYPE bapi2080_notadt-usrstatus,
      ld_syststat	    TYPE bapi2080_notsti,
      ld_testrun      TYPE bapi20783t-status_ind.


    DATA:
      at_code       TYPE char8,
      at_code_group TYPE char8,
      at_cat_typ    TYPE otkat.

    DATA:
      alv1     TYPE REF TO cl_gui_alv_grid,
      alv2     TYPE REF TO cl_gui_alv_grid,
      alv3     TYPE REF TO cl_gui_alv_grid,
      alv4     TYPE REF TO cl_gui_alv_grid,
      alv5     TYPE REF TO cl_gui_alv_grid,
      alv6     TYPE REF TO cl_gui_alv_grid,

      cont1    TYPE REF TO cl_gui_custom_container,
      cont2    TYPE REF TO cl_gui_custom_container,
      cont3    TYPE REF TO cl_gui_custom_container,
      cont4    TYPE REF TO cl_gui_custom_container,
      cont5    TYPE REF TO cl_gui_custom_container,
      cont6    TYPE REF TO cl_gui_custom_container,

      lt_f4    TYPE lvc_t_f4,

      fcat     TYPE lvc_t_fcat,
      fcat_aux TYPE lvc_t_fcat,
      str      TYPE REF TO data,
      it_exc   TYPE ui_functions,
      variant  TYPE disvariant,
      layout   TYPE lvc_s_layo,
      estilo   TYPE lvc_t_styl,
      stable   TYPE lvc_s_stbl.

    DATA: status  TYPE lvc_style,
          ativo   TYPE lvc_style VALUE cl_gui_alv_grid=>mc_style_enabled,
          inativo TYPE lvc_style VALUE cl_gui_alv_grid=>mc_style_disabled.

    DATA: it_ucomm TYPE TABLE OF ty_ucomm.
    DATA: t_ucomm TYPE TABLE OF ty_ucomm.


    DATA: it_afvc TYPE TABLE OF ty_afvc,
          wa_afvc TYPE ty_afvc,
          t_aufk  TYPE TABLE OF ty_aufk,
          t_afru  TYPE TABLE OF ty_afru,
          t_afko  TYPE TABLE OF ty_aufk,
*          T_AFVC  TYPE TABLE OF TY_AFRU,
          wa_aufk TYPE ty_aufk.

*    DATA: LV_NOTIF     TYPE QMNUM,
*          V_NOTA       TYPE QMNUM,
*          V_NOTA_DESCR TYPE AUFTEXT,
*          V_ORDEM      TYPE AUFNR. "VALUE '000011000975'.

ENDCLASS.
