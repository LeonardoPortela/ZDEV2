*&---------------------------------------------------------------------*
*& Include ZGL015_TOP                                        PoolMóds.        ZGL015
*&
*&---------------------------------------------------------------------*
program  zgl015.

data  titulo(15).
data  wgzglt034-lote(25).
data  wgzglt000-tp_lcto(20).
data  wgzglt000-bukrs(10).
data  wgzglt000-moeda_doc(20).
data  txt_ap_fiscal(15).
data  wgzglt000-xblnr(20).
data  txt_prov(90).
data  txtper(15).
data  txt_dt_doc(15).
data  txt_dt_lcto(15).
data  wgzglt000-bktxt(25).
data  wgzglt035-doc_lcto(25).
data  txt_doc_cont(20).
data  txt_doc_cont_e(20).
data  txt_doc_reversao(20). "130130 - CS2023000969 Gisele Follmann PSA
data  wgzglt000-blart(20).
data  txt-taxa(20).
data  txt-taxa_i(20).
data  wgzglt000-moeda_interna(20).
data  wgzglt000-moeda_forte(20).
data  wgzglt000-moeda_grupo(20).
data  wgzglt035-dt_doc(20).
data  wgzglt035-st_lc_moeda(30).
data  wgzglt035-prov_est(5).
data  wgzglt035-dt_lcto(20).
data  wgzglt035-dt_doc_ult_mes(30).
data  wgzglt035-dt_lcto_ult_mes(30).
data  wgzglt035-st_fecha(20).
data  wgzglt035-dt_lcto_ult_mes1(30).
data  wgzglt035-moeda_gp_hist(50).
data  wgzglt035-st_agrupa(50).
data  wgzglt035_usnam(50).
data  wgzglt035_dt_entrada(50).
data  wgzglt035_hr_entrada(50).
data  ts_100_tab1(15).
data  ts_100_tab2(25).
data  ts_100_tab3(12).
data  vg_doc_copia  type zglt035-doc_lcto.
data  vg_belnr_copia  type zglt035-belnr.
data  vg_file type string.

include: <icon>.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TS_100'
constants: begin of c_ts_100,
             tab1 like sy-ucomm value 'TS_100_FC1',
             tab2 like sy-ucomm value 'TS_100_FC2',
             tab3 like sy-ucomm value 'TS_100_FC3',
           end of c_ts_100.
*&SPWIZARD: DATA FOR TABSTRIP 'TS_100'
controls:  ts_100 type tabstrip.
data: begin of g_ts_100,
        subscreen   like sy-dynnr,
        prog        like sy-repid value 'ZGL015',
        pressed_tab like sy-ucomm value c_ts_100-tab1,
      end of g_ts_100.


************************************************************************
*** Definição de Macros                                              ***
************************************************************************
define __format_date.

  concatenate &1+6(4) &1+3(2) &1(2) into &2.

end-of-definition.

define __format_value.
  try.

    translate &1 using '. '.
    condense &1 no-gaps.
    translate &1 using ',.'.

    move &1 to &2.

  catch cx_sy_conversion_no_number.

  endtry.
end-of-definition.



*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
data : begin of t_mwdat occurs 0.
         include structure rtax1u15.
data : end of t_mwdat.
data    w_mwdat type rtax1u15.

types: begin of ty_glt035,
         doc_lcto        type zglt035-doc_lcto,
         bukrs           type zglt034-bukrs,
         butxt           type t001-butxt,
         tp_lcto         type zglt035-tp_lcto,
         descricao       type zglt031-descricao,
         dpto_resp       type zglt035-dpto_resp,
         taxa            type zglt035-taxa,
         taxa_i          type zglt035-taxa_i,
         moeda_doc       type zglt035-moeda_doc,
         st_lc_moeda     type zglt035-st_lc_moeda,
         moeda_interna   type zglt035-moeda_interna,
         moeda_int_hist  type zglt035-moeda_int_hist,
         moeda_forte     type zglt035-moeda_forte,
         moeda_ft_hist   type zglt035-moeda_ft_hist,
         moeda_grupo     type zglt035-moeda_grupo,
         moeda_gp_hist   type zglt035-moeda_gp_hist,
         blart           type zglt035-blart,
         xblnr           type zglt035-xblnr,
         bktxt           type zglt035-bktxt,
         budat           type zglt035-budat,
         bldat           type zglt035-bldat,
         prov_est        type zglt035-prov_est,
         usnam           type zglt035-usnam,
         dt_entrada      type zglt035-dt_entrada,
         hr_entrada      type zglt035-hr_entrada,
         dt_doc          type zglt031-dt_doc,
         dt_doc_ult_mes  type zglt031-dt_doc_ult_mes,
         dt_lcto         type zglt031-dt_lcto,
         dt_lcto_ult_mes type zglt031-dt_lcto_ult_mes,
         monat           type zglt035-monat,
         gjahr           type zglt035-gjahr,
         belnr           type zglt035-belnr,
         gjahr_e         type zglt035-gjahr,
         belnr_e         type zglt035-belnr,
         st_ap_fiscal    type zglt035-st_ap_fiscal,
         st_fecha        type zglt035-st_fecha,
         st_agrupa       type zglt035-st_agrupa,
         reversao_doc    type zglt035-belnr,
         reversao_status type iconname,
       end of ty_glt035,

       begin of ty_glt036,
         mark(1),
         check(1),
         doc_lcto        type zglt036-doc_lcto,
         seqitem         type zglt036-seqitem,
         tp_lcto         type zglt036-tp_lcto,
         bschl           type zglt036-bschl,
         hkont           type zglt036-hkont,
         descr           type char30,
         umskz           type zglt036-umskz,
         anbwa           type zglt036-anbwa,
         bewar           type zglt036-bewar,
         vbund           type zglt036-vbund,
         d_c             type char1,
         icon            type icon-id,
         kostl           type zglt036-kostl,
         prctr           type zglt036-prctr,
         aufnr           type zglt036-aufnr,
         vornr           type zglt036-vornr,
         vbeln           type zglt036-vbeln,
         matnr           type zglt036-matnr,
         matnr_fi        type zglt036-matnr_fi,
         werks           type zglt036-werks,
         divisao         type j_1bbranch-branch,
         zuonr           type zglt036-zuonr,
         tax_code        type zglt036-tax_code,
         sgtxt           type zglt036-sgtxt,
         gsber           type zglt036-gsber,
         vlr_moeda_doc   type zglt036-vlr_moeda_doc,
         vlr_moeda_int   type zglt036-vlr_moeda_int,
         vlr_moeda_forte type zglt036-vlr_moeda_forte,
         vlr_moeda_grupo type zglt036-vlr_moeda_grupo,
         dt_vct          type zglt036-dt_vct,
         hbkid           type zglt036-hbkid,
         bvtyp           type zglt036-bvtyp,
         zlsch           type zglt036-zlsch,
         quantity        type zglt036-quantity,
         base_uom        type zglt036-base_uom,
         zlspr           type bseg-zlspr,
         akont           type lfb1-akont,
         menge           type mseg-menge,
         descr_a         type char30,
         kostl_c(1)      type c,
         xclasse(1)      type c,
         style           type lvc_t_styl,
       end of ty_glt036,

       begin of ty_zglt031,
         tp_lcto     type zglt031-tp_lcto,
         st_lc_moeda type zglt031-st_lc_moeda,
       end of ty_zglt031,

       begin of ty_obj,
         mark(1),
         seqitem         type zglt036-seqitem,
         hkont           type zglt036-hkont,
         txt50           type skat-txt50,
         vbund           type zglt036-vbund,
         iva(40)         type c,
         kostl           type zglt036-kostl,
         prctr           type zglt036-prctr,
         aufnr           type zglt036-aufnr,
         vornr           type zglt036-vornr,
         matnr           type zglt036-matnr,
         vlr_moeda_doc   type zglt036-vlr_moeda_doc,
         vlr_moeda_int   type zglt036-vlr_moeda_int,
         vlr_moeda_forte type zglt036-vlr_moeda_forte,
         vlr_moeda_grupo type zglt036-vlr_moeda_grupo,
         quantity        type zglt036-quantity,
         base_uom        type zglt036-base_uom,
       end of ty_obj,

       begin of ty_vat,
         seqitem type zglt036-seqitem,
         mwskz   type t030k-mwskz,    "Vat
         msatz   type rtax1u15-msatz, "Taxa
         base    type rtax1u15-kbetr, "Base imposto
         kbetr   type rtax1u15-kbetr, "valor imposto
         hkont   type rtax1u15-hkont, "conta imposto
         txt50   type skat-txt50,
         kostl   type zglt036-kostl,
         prctr   type zglt036-prctr,
         aufnr   type zglt036-aufnr,
         matnr   type zglt036-matnr,
       end of ty_vat,

       begin of ty_fields,
         campo(30) type c,
         group1(5) type c,
         value     type sy-tabix,
         invisible type sy-tabix,
       end of ty_fields,

       begin of ty_editor,
         line(72),
       end of ty_editor,

       begin of ty_glt034,
         lote        type zglt034-lote,
         descr_lote  type zglt034-descr_lote,
         bukrs       type zglt034-bukrs,
         usnam       type zglt034-usnam,
         dep_resp    type zglt034-dep_resp,
         status_lote type zglt034-status_lote,
         data_atual  type zglt034-data_atual,
         hora_atual  type zglt034-hora_atual,
         usuario     type zglt034-usuario,
       end of ty_glt034,

       begin of ty_zib_contabil_chv,
         obj_key type zib_contabil_chv-obj_key,
         belnr   type zib_contabil_chv-belnr,
         bukrs   type zib_contabil_chv-bukrs,
         gjahr   type zib_contabil_chv-gjahr,
       end of ty_zib_contabil_chv,

       begin of ty_zib_contabil_err,
         obj_key        type zib_contabil_err-obj_key,
         nr_item        type zib_contabil_err-nr_item,
         interface      type zib_contabil_err-interface,
         dt_atualizacao type zib_contabil_err-dt_atualizacao,
         hr_atualizacao type zib_contabil_err-hr_atualizacao,
         type           type zib_contabil_err-type,
         id             type zib_contabil_err-id,
         num            type zib_contabil_err-num,
         message        type zib_contabil_err-message,
         message_v1     type zib_contabil_err-message_v1,
         message_v2     type zib_contabil_err-message_v2,
         message_v3     type zib_contabil_err-message_v3,
         message_v4     type zib_contabil_err-message_v4,
       end of ty_zib_contabil_err.


*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
data: ok-code         type sy-ucomm,
      tg_selectedcell type lvc_t_cell,
      wg_selectedcell type lvc_s_cell,
      x_field(30),
      wg_colaps(4)    value '@K2@',
      wg_sub01        type sy-dynnr,
*      BTN_VISAO(25)         VALUE '@KU@ Visão de Razão',
      btn_visao(25),
      x_visao(1),
      init,
      ref1            type ref to cl_gui_alv_grid.

data: tg_excel   type table of zalsmex_tabline.

** Criação de tabela dinamica
data: tg_fieldcatalog     type lvc_t_fcat,
      wg_fieldcatalog     type lvc_s_fcat,
      wa_layout           type lvc_s_layo,
      wa_stable           type lvc_s_stbl,

      tg_fields           type table of ty_fields   with header line,
      tg_editor           type table of ty_editor,
      wg_editor           type ty_editor,
      tg_msg_ret          type table of zfiwrs0002  with header line,

      wg_zglt031          type zglt031,
      tg_zglt031          type table of zglt031,
      wg_zglt034          type zglt034,
      tg_zglt035          type table of ty_glt035,
      wg_zglt035          type ty_glt035,
      tg_zglt036          type table of ty_glt036,
      tg_zglt036_aux      type table of ty_glt036,
      tg_zglt036_excel    type table of ty_glt036,
      tg_zglt036_ori      type table of ty_glt036,
      wg_zglt036          type ty_glt036,
      wg_zglt036_tot      type ty_glt036,
      tg_obj              type table of ty_obj,
      wg_obj              type ty_obj,
      tg_vat              type table of ty_vat,
      wg_vat              type ty_vat,
      wa_zib_contabil_chv type ty_zib_contabil_chv,
      it_zib_contabil_err type table of ty_zib_contabil_err  with header line.

data: ok_code         like sy-ucomm,
      wg_mensagem(30),
      wg_menslib(30),
      wg_menslot(30),
      wg_acao(30),
      vg_chamada(1),
      vg_carrega(1),
      v_lote15        type zglt034-lote.

data: xs_events  type          slis_alv_event,
      pop_events type          slis_t_event.

*Class definition for ALV toolbar
class:  lcl_alv_toolbar     definition deferred.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
data: g_container          type scrfname value 'CC_CONTABIL',
      g_custom_container   type ref to cl_gui_custom_container,
      obg_conteiner_err    type ref to cl_gui_custom_container,
      obg_conteiner_vat    type ref to cl_gui_custom_container,
      obg_conteiner_obj    type ref to cl_gui_custom_container,
      container_1          type ref to cl_gui_container,
      container_2          type ref to cl_gui_container,
      splitter             type ref to cl_gui_splitter_container,
      grid1                type ref to cl_gui_alv_grid,
      grid3                type ref to cl_gui_alv_grid,
      grid4                type ref to cl_gui_alv_grid,
      grid5                type ref to cl_gui_alv_grid,
      gs_variant_c         type disvariant,
      obg_toolbar          type ref to lcl_alv_toolbar,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      g_descbox            type scrfname value 'CC_OBS',
      g_custom_cont_desc   type ref to cl_gui_custom_container,
      obg_descbox          type ref to cl_gui_textedit,
      obg_docking          type ref to cl_gui_docking_container,
      wa_style             type lvc_s_styl,
      style                type lvc_t_styl   with header line,
      style2               type lvc_t_styl   with header line.

data: tl_fieldcat type slis_t_fieldcat_alv,
      wl_fieldcat type slis_t_fieldcat_alv with header line,
      wl_layout   type slis_layout_alv,
      wl_print    type slis_print_alv,
      tl_obj_aux  type table of ty_obj.

* alrs
*Declaration for toolbar buttons
data: ty_toolbar type stb_button.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
constants:
  c_0              type c value '0',
  c_1              type c value '1',
  c_x              type c value 'X',
  c_i              type c value 'I',
  c_n              type c value 'N',
  c_ne(2)          type c value 'NE',
  c_add(3)         type c value 'ADD',
  c_del(3)         type c value 'DEL',
  c_dg1(3)         type c value 'DG1',
  c_dg2(3)         type c value 'DG2',
  c_exit(4)        type c value 'EXIT',
  c_back(4)        type c value 'BACK',
  c_save(4)        type c value 'SAVE',
  c_modif(5)       type c value 'MODIF',
  c_cancel(6)      type c value 'CANCEL',
  c_deldoc(6)      type c value 'DELDOC',
  c_search(6)      type c value 'SEARCH',
  c_displa(6)      type c value 'DISPLA',
  c_atuali(6)      type c value 'ATUALI',
  c_clos_msg(8)    type c value 'CLOS_MSG',
  c_show_msgre(10) type c value 'SHOW_MSGRE',
  c_col_exp(7)     type c value 'COL_EXP',
  c_0051(4)        type c value '0051',
  c_0052(4)        type c value '0052',
  c_excel(5)       type c value 'EXCEL'.


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
class lcl_event_handler definition.
  public section.
    class-methods:
      on_hotspot_click for event hotspot_click of cl_gui_alv_grid
        importing e_row_id e_column_id.

    class-methods:
      on_double_click for event double_click of cl_gui_alv_grid
        importing e_row e_column.

    class-methods:
      on_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_pop for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_finished for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_onf4 for event onf4 of cl_gui_alv_grid
        importing e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

endclass.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class lcl_alv_toolbar definition.
  public section.
*Constructor
    methods:
      constructor
        importing io_alv_grid type ref to cl_gui_alv_grid,
*Event for toolbar
      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class lcl_alv_toolbar implementation.
  method constructor.
*   Create ALV toolbar manager instance
    create object c_alv_toolbarmanager
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.                    "constructor

  method on_toolbar.
    data: wl_desactive.

    wl_desactive = space.

*    SELECT *
*      FROM zglt037
*      INTO @DATA(ls_user)
*      UP TO 1 ROWS
*      WHERE aprovador = @sy-uname.
*    ENDSELECT.
*    IF sy-subrc IS NOT INITIAL.
    if sy-tcode ne 'ZGL016A'.
      ty_toolbar-icon      = icon_copy_object.
      ty_toolbar-function  = 'CHECK'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-text      = 'Check'.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      ty_toolbar-icon      = icon_copy_object.
      ty_toolbar-function  = 'DIVISAO'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-text      = text-010.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      ty_toolbar-icon      = icon_copy_object.
      ty_toolbar-function  = 'TEXTO'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-text      = text-011.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      ty_toolbar-icon      = icon_calculation.
      ty_toolbar-function  = 'VAT'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      if wg_zglt035-tp_lcto gt 0.
        ty_toolbar-icon      = icon_expand_all.
        ty_toolbar-function  = 'EXP'.
        ty_toolbar-disabled  = wl_desactive.
        ty_toolbar-butn_type = 0.
        append ty_toolbar to e_object->mt_toolbar.
        clear ty_toolbar.

        ty_toolbar-icon      = icon_collapse_all.
        ty_toolbar-function  = 'RET'.
        ty_toolbar-disabled  = wl_desactive.
        ty_toolbar-butn_type = 0.
        append ty_toolbar to e_object->mt_toolbar.
        clear ty_toolbar.
      endif.

      ty_toolbar-icon      = icon_insert_row.
      ty_toolbar-function  = c_add.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      ty_toolbar-icon      = icon_delete_row.
      ty_toolbar-function  = c_del.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

      ty_toolbar-butn_type = 5.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

*   variable for Toolbar Button
      ty_toolbar-icon      = icon_view_close.
      ty_toolbar-function  = c_clos_msg.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.

**  Begin of CS2022000638   #80266 FF   06.01.2023
      ty_toolbar-icon      = icon_import.
      ty_toolbar-quickinfo = text-012.
      ty_toolbar-function  = c_excel.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-text      = text-012.
      ty_toolbar-butn_type = 0.
      append ty_toolbar to e_object->mt_toolbar.
      clear ty_toolbar.
** End of FF  06.01.2023

*    CALL REORGANIZE METHOD OF TOOLBAR MANAGER TO
*    DISPLAY THE TOOLBAR
      call method c_alv_toolbarmanager->reorganize
        exporting
          io_alv_toolbar = e_object.
    else.

      free  e_object->mt_toolbar.

*    CALL REORGANIZE METHOD OF TOOLBAR MANAGER TO
*    DISPLAY THE TOOLBAR
      call method c_alv_toolbarmanager->reorganize
        exporting
          io_alv_toolbar = e_object.

    endif.
  endmethod.                    "on_toolbar

  method handle_user_command.
    data: vdivisao1     type zglt036-gsber,
          vsgtxt        type zglt036-sgtxt,
          vdatas(10),
          wcont         type i,
          vflag_cus(1),
          wa_tbsl       type tbsl,
          wa_lfa1       type lfa1,
          wa_t030k      type t030k,
          wl_skat       type skat,
          vg_lifnrvat   type lfa1-lifnr,
          vlr_moeda_doc	type wrbtr.

    data: l_dynpfields type table of dynpread,
          w_dynpfields type dynpread.
    refresh l_dynpfields.
    clear   w_dynpfields.

    if wg_zglt034-lote is  not initial.
      w_dynpfields-fieldname  = 'WG_ZGLT035-BUDAT'.
      append  w_dynpfields to l_dynpfields.

      call function 'DYNP_VALUES_READ'
        exporting
          dyname     = sy-repid
          dynumb     = sy-dynnr
        tables
          dynpfields = l_dynpfields.
      read table l_dynpfields into  w_dynpfields index 1.
      vdatas = w_dynpfields-fieldvalue.
      concatenate vdatas+6(4) vdatas+3(2) vdatas+0(2) into wg_zglt035-budat.
      "
      refresh l_dynpfields.
      clear   w_dynpfields.
      w_dynpfields-fieldname  = 'WG_ZGLT035-MOEDA_DOC'.
      append  w_dynpfields to l_dynpfields.

      call function 'DYNP_VALUES_READ'
        exporting
          dyname     = sy-repid
          dynumb     = sy-dynnr
        tables
          dynpfields = l_dynpfields.
      read table l_dynpfields into w_dynpfields index 1.
      move w_dynpfields-fieldvalue to wg_zglt035-moeda_doc.
      "
      refresh l_dynpfields.
      clear   w_dynpfields.
      w_dynpfields-fieldname  = 'WG_ZGLT035-MONAT'.
      append  w_dynpfields to l_dynpfields.

      call function 'DYNP_VALUES_READ'
        exporting
          dyname     = sy-repid
          dynumb     = sy-dynnr
        tables
          dynpfields = l_dynpfields.
      read table l_dynpfields into w_dynpfields index 1.
      move w_dynpfields-fieldvalue to wg_zglt035-monat.
    endif.

    if wg_zglt035-budat is  initial or
       wg_zglt035-monat is  initial or
       wg_zglt035-moeda_doc is initial.
      data(lv_erro) = 'X'.

      if e_ucomm = c_excel.
        message 'Preencha os Dados Gerais antes de importar a planilha' type 'I'.
      else.
        message text-i04 type 'I'.
      endif.
    endif.

    if wg_zglt035-tp_lcto gt 0.
      check tg_zglt036[] is not initial.
    endif.

    data: tl_zglt036_aux type table of ty_glt036,
          wl_zglt036     type ty_glt036,
          wl_zglt036_ori type ty_glt036,
          wl_lines       type sy-tabix.

    loop at tg_zglt036       into wg_zglt036.
      if wg_zglt036-check = 'X' and wg_zglt036-divisao is not initial .
        vdivisao1 = wg_zglt036-divisao.
        exit.
      endif.
    endloop.
    clear: vsgtxt, vg_lifnrvat.
    loop at tg_zglt036       into wg_zglt036.
      if wg_zglt036-check = 'X'.
        if wg_zglt036-sgtxt is not initial and vsgtxt is initial.
          vsgtxt = wg_zglt036-sgtxt.
        endif.
        select single *
          from tbsl
          into wa_tbsl
          where bschl = wg_zglt036-bschl.
        if wa_tbsl-koart = 'K'.
          vg_lifnrvat = wg_zglt036-hkont.
        endif.
      endif.
    endloop.


    case e_ucomm.
      when 'VAT'.
        refresh tg_vat.
        loop at tg_zglt036       into wg_zglt036.
          if wg_zglt036-check = 'X' and wg_zglt036-tax_code is not initial .

            vflag_cus = 'N'.
            refresh t_mwdat.

            clear vlr_moeda_doc.
            vlr_moeda_doc = wg_zglt036-vlr_moeda_doc.

            call function 'CALCULATE_TAX_FROM_NET_AMOUNT'
              exporting
                i_bukrs           = wg_zglt035-bukrs
                i_mwskz           = wg_zglt036-tax_code
                i_waers           = wg_zglt035-moeda_doc
                i_wrbtr           = vlr_moeda_doc
              tables
                t_mwdat           = t_mwdat
              exceptions
                bukrs_not_found   = 1
                country_not_found = 2
                mwskz_not_defined = 3
                mwskz_not_valid   = 4
                ktosl_not_found   = 5
                kalsm_not_found   = 6
                parameter_error   = 7
                knumh_not_found   = 8
                kschl_not_found   = 9
                unknown_error     = 10
                account_not_found = 11
                txjcd_not_valid   = 12
                others            = 13.

            if sy-subrc <> 0.
              message id sy-msgid type sy-msgty number sy-msgno
                      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            endif.
            loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
              if wg_obj-vlr_moeda_int eq 0 and
                 wg_obj-vlr_moeda_forte eq 0 and
                 wg_obj-vlr_moeda_grupo eq 0 and
                 wg_obj-vlr_moeda_doc eq 0.
                continue.
              endif.
              vflag_cus = 'S'.
              if t_mwdat[] is not initial.
                clear t_mwdat.
                read table t_mwdat into w_mwdat index 1. "INTO s_mwdat
                "Exceção para belgica (BE) e alemanha (DE)
                if vg_lifnrvat is not initial.
                  select single *
                    from lfa1
                    into wa_lfa1
                    where lifnr = vg_lifnrvat.
                  if wa_lfa1-land1 = 'BE'.
                    select single *
                      from t030k
                      into wa_t030k
                      where ktopl = '0050'
                      and   ktosl = 'ZBE'
                      and   mwskz = wg_zglt036-tax_code.
                    if sy-subrc = 0.
                      w_mwdat-hkont = wa_t030k-konts.
                      w_mwdat-ktosl = 'ZBE'.
                      w_mwdat-kschl = 'ZBE'.
                    endif.
                  elseif wa_lfa1-land1 = 'DE'.
                    select single *
                      from t030k
                      into wa_t030k
                      where ktopl = '0050'
                      and   ktosl = 'ZDE'
                      and   mwskz = wg_zglt036-tax_code.
                    if sy-subrc = 0.
                      w_mwdat-hkont = wa_t030k-konts.
                      w_mwdat-ktosl = 'ZDE'.
                      w_mwdat-kschl = 'ZDE'.
                    endif.
                  endif.

                endif.
                if  w_mwdat-msatz ne 0.
                  "grava Base
                  wg_vat-seqitem = wg_zglt036-seqitem.
                  wg_vat-mwskz   = wg_zglt036-tax_code.
                  wg_vat-msatz   = w_mwdat-msatz. "Taxa
                  wg_vat-base    = wg_obj-vlr_moeda_doc. "base
                  wg_vat-kbetr   = wg_obj-vlr_moeda_doc  / ( 1 + ( w_mwdat-msatz / 100 ) ). "valor
                  wg_vat-hkont   = wg_zglt036-hkont. "conta
                  wg_vat-kostl   = wg_zglt036-kostl.
                  wg_vat-prctr   = wg_zglt036-prctr.
                  wg_vat-aufnr   = wg_zglt036-aufnr.
                  wg_vat-matnr   = wg_zglt036-matnr.

                  select single *
                   from skat
                   into wl_skat
                   where spras eq sy-langu
                   and ktopl eq '0050'
                   and saknr eq  wg_zglt036-hkont.
                  if sy-subrc = 0.
                    wg_vat-txt50 = wl_skat-txt50. "text conta
                  endif.
                  append wg_vat to tg_vat.
                  "grava VAT
                  wg_vat-seqitem = wg_zglt036-seqitem.
                  wg_vat-mwskz   = wg_zglt036-tax_code.
                  wg_vat-msatz   = w_mwdat-msatz. "Taxa
                  wg_vat-base    = wg_obj-vlr_moeda_doc. "base
                  wg_vat-kbetr   = wg_obj-vlr_moeda_doc - wg_vat-kbetr. "valor imposto
                  wg_vat-hkont   = w_mwdat-hkont. "conta imposto
                  wg_vat-kostl   = wg_zglt036-kostl.
                  wg_vat-prctr   = wg_zglt036-prctr.
                  wg_vat-aufnr   = wg_zglt036-aufnr.
                  wg_vat-matnr   = wg_zglt036-matnr.
                  select single *
                   from skat
                   into wl_skat
                   where spras eq sy-langu
                   and ktopl eq '0050'
                   and saknr eq  wg_vat-hkont.
                  if sy-subrc = 0.
                    wg_vat-txt50 = wl_skat-txt50. "text conta
                  endif.
                  append wg_vat to tg_vat.
                endif.

              endif.
            endloop.

            if vflag_cus = 'N'.
              if t_mwdat[] is not initial.
                clear t_mwdat.
                read table t_mwdat into w_mwdat index 1. "INTO s_mwdat
                "Exceção para belgica (BE) e alemanha (DE)
                if vg_lifnrvat is not initial.
                  select single *
                    from lfa1
                    into wa_lfa1
                    where lifnr = vg_lifnrvat.
                  if wa_lfa1-land1 = 'BE'.
                    select single *
                      from t030k
                      into wa_t030k
                      where ktopl = '0050'
                      and   ktosl = 'ZBE'
                      and   mwskz = wg_zglt036-tax_code.
                    if sy-subrc = 0.
                      w_mwdat-hkont = wa_t030k-konts.
                      w_mwdat-ktosl = 'ZBE'.
                      w_mwdat-kschl = 'ZBE'.
                    endif.
                  elseif wa_lfa1-land1 = 'DE'.
                    select single *
                      from t030k
                      into wa_t030k
                      where ktopl = '0050'
                      and   ktosl = 'ZDE'
                      and   mwskz = wg_zglt036-tax_code.
                    if sy-subrc = 0.
                      w_mwdat-hkont = wa_t030k-konts.
                      w_mwdat-ktosl = 'ZDE'.
                      w_mwdat-kschl = 'ZDE'.
                    endif.
                  endif.

                endif.
                if  w_mwdat-msatz ne 0.
                  "grava Base
                  wg_vat-seqitem = wg_zglt036-seqitem.
                  wg_vat-mwskz   = wg_zglt036-tax_code.
                  wg_vat-msatz   = w_mwdat-msatz. "Taxa
                  wg_vat-base    = wg_zglt036-vlr_moeda_doc. "base
                  wg_vat-kbetr   = wg_zglt036-vlr_moeda_doc  / ( 1 + ( w_mwdat-msatz / 100 ) ). "valor
                  wg_vat-hkont   = wg_zglt036-hkont. "conta
                  wg_vat-kostl   = wg_zglt036-kostl.
                  wg_vat-prctr   = wg_zglt036-prctr.
                  wg_vat-aufnr   = wg_zglt036-aufnr.
                  wg_vat-matnr   = wg_zglt036-matnr.
                  select single *
                   from skat
                   into wl_skat
                   where spras eq sy-langu
                   and ktopl eq '0050'
                   and saknr eq  wg_zglt036-hkont.
                  if sy-subrc = 0.
                    wg_vat-txt50 = wl_skat-txt50. "text conta
                  endif.
                  append wg_vat to tg_vat.
                  "grava VAT
                  wg_vat-seqitem = wg_zglt036-seqitem.
                  wg_vat-mwskz   = wg_zglt036-tax_code.
                  wg_vat-msatz   = w_mwdat-msatz. "Taxa
                  wg_vat-base    = wg_zglt036-vlr_moeda_doc. "base
                  wg_vat-kbetr   = wg_zglt036-vlr_moeda_doc - wg_vat-kbetr. "valor imposto
                  wg_vat-hkont   = w_mwdat-hkont. "conta imposto
                  wg_vat-kostl   = wg_zglt036-kostl.
                  wg_vat-prctr   = wg_zglt036-prctr.
                  wg_vat-aufnr   = wg_zglt036-aufnr.
                  wg_vat-matnr   = wg_zglt036-matnr.
                  select single *
                   from skat
                   into wl_skat
                   where spras eq sy-langu
                   and ktopl eq '0050'
                   and saknr eq  wg_vat-hkont.
                  if sy-subrc = 0.
                    wg_vat-txt50 = wl_skat-txt50. "text conta
                  endif.
                  append wg_vat to tg_vat.
                endif.

              endif.

            endif.
          endif.

        endloop.
        if tg_vat[] is not initial.
          call screen 200 starting at 10  1
                          ending   at 190 16.
        endif.

      when 'EXP'.
        loop at tg_zglt036_ori into wl_zglt036_ori.
          read table tg_zglt036 into wl_zglt036 with key seqitem = wl_zglt036_ori-seqitem.
          if sy-subrc ne 0.
            append wl_zglt036_ori to tg_zglt036.
          endif.
        endloop.
        sort tg_zglt036 by seqitem.

      when 'RET'.
        loop at tg_zglt036 into wl_zglt036.
          if wl_zglt036-check ne 'X'.
            wl_zglt036-xclasse = 'E'.
            modify tg_zglt036 from wl_zglt036 index sy-tabix transporting xclasse.
          endif.
        endloop.
        delete tg_zglt036 where xclasse = 'E'.
        sort tg_zglt036 by seqitem.
      when c_add.
        if wg_zglt035-tp_lcto eq 0.
          tl_zglt036_aux[] = tg_zglt036[].
          refresh: tg_zglt036.
          loop at tl_zglt036_aux into wl_zglt036.
            append wl_zglt036 to tg_zglt036.
          endloop.

          clear: wl_zglt036.
          describe table tl_zglt036_aux lines wcont.
          add 1 to wcont.
          wl_zglt036-check = 'X'.
          wl_zglt036-seqitem  = wcont.

          if wg_zglt035-bukrs = '0200'.

            select single * from zglt034
              into @data(wzglt034)
            where lote eq @wg_zglt034-lote.

            select single gsber  from zimp_cad_depto
              into wl_zglt036-divisao
            where dep_resp eq wzglt034-dep_resp.

          else.

            select single gsber
            from zimp_cad_depto
            into  wl_zglt036-divisao
            where bukrs eq wg_zglt035-bukrs
            and   gsber ne ''.

          endif.

          append wl_zglt036 to tg_zglt036.
        else.
          message text-e60 type 'I'.
        endif.

      when c_del.
        if wg_zglt035-tp_lcto eq 0.
          call method grid1->get_selected_cells
            importing
              et_cell = tg_selectedcell.

          loop at tg_selectedcell into wg_selectedcell.
            delete tg_zglt036 index wg_selectedcell-row_id-index.
          endloop.
        else.
          message text-e60 type 'I'.
        endif.

      when 'CHECK'.
        if wg_zglt035-budat is not initial and
           wg_zglt035-monat is not initial and
           wg_zglt035-moeda_doc is not initial.
          loop at tg_zglt036       into wg_zglt036.
            if  wg_zglt036-check = 'X'.
              clear  wg_zglt036-check.
            else.
              wg_zglt036-check = 'X'.
            endif.
            modify tg_zglt036 from wg_zglt036 index sy-tabix transporting check.
          endloop.
*        ELSE.
*          MESSAGE TEXT-I04 TYPE 'I'.
        endif.

      when 'DIVISAO'.
        loop at tg_zglt036       into wg_zglt036.
          if wg_zglt036-check = 'X'.
            wg_zglt036-divisao = vdivisao1.
            modify tg_zglt036 from wg_zglt036 index sy-tabix transporting divisao.
          endif.
        endloop.

      when 'TEXTO'.
        loop at tg_zglt036       into wg_zglt036.
          if wg_zglt036-check = 'X'.
            wg_zglt036-sgtxt   = vsgtxt.
            modify tg_zglt036 from wg_zglt036 index sy-tabix transporting sgtxt.
          endif.
        endloop.

**  Begin of CS2022000638   #80266 FF   06.01.2023
      when c_excel.

        if lv_erro is initial.

          clear: vg_file, tg_excel[], tg_zglt036_excel[], tg_zglt036, tg_zglt036[].

          call method grid1->set_table_for_first_display "Limpar o ALV ao carregar a planilha
            exporting
*             IT_TOOLBAR_EXCLUDING = TL_FUNCTION
              is_layout       = wa_layout
            changing
              it_fieldcatalog = tg_fieldcatalog[]
              it_outtab       = tg_zglt036[].

          perform f_popup_info_layout.
          perform f_busca_arquivo using vg_file.
          perform f_le_arquivo.

*Inicio Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266
          grid1->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
          grid1->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
*Fim Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266

          perform f_monta_arq_saida.
        else.
          clear lv_erro.
        endif.
        clear e_ucomm.
** End of FF  06.01.2023

    endcase.

*** Método de atualização de dados na Tela
    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.

    call method cl_gui_cfw=>flush.

    set cursor field wg_zglt035-moeda_doc.

    call function 'SAPGUI_SET_FUNCTIONCODE'
      exporting
        functioncode           = '=ENT'
      exceptions
        function_not_supported = 1
        others                 = 2.

  endmethod.                    "zm_handle_user_command

endclass.                    "lcl_alv_toolbar IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_event_handler implementation.
  method on_hotspot_click.
    data: wl_obj_aux     type ty_obj,
          tl_zglt032_aux type table of zglt032,
          wl_zglt032_aux type zglt032,
          tl_zglt036_aux type table of ty_glt036,
          wl_zglt036_aux type ty_glt036,
          wl_skat        type skat,
          w_flag1(1).

**  Begin of CS2022000638   #80266 FF   02.02.2023
    if tg_excel[] is not initial.
      clear tg_selectedcell[].
      call method grid1->get_selected_cells
        importing
          et_cell = tg_selectedcell.
    endif.
**  End of CS2022000638   #80266 FF   02.02.2023

    read table tg_selectedcell  into wg_selectedcell  index 1.
    "READ TABLE TG_ZGLT036       INTO WG_ZGLT036       WITH KEY SEQITEM = WG_SELECTEDCELL-ROW_ID-INDEX.
    read table tg_zglt036       into wg_zglt036      index wg_selectedcell-row_id-index.

    if wg_zglt036-check ne 'X'.
      exit.
    endif.
    if wg_zglt036-xclasse ne 'X'.
      exit.
    endif.

    refresh:  wl_fieldcat, tl_obj_aux.

    sort tg_obj by seqitem.
    loop at tg_zglt036 into wg_zglt036.
      if wg_zglt036-check = 'X' and  wg_zglt036-xclasse eq 'X'.
        if wg_zglt035-tp_lcto eq 0.
          move-corresponding wg_zglt036 to wl_obj_aux. "Adicionado as informações na workearea.BUG IMPEDITIVO 103379*/AOENNING.
          wl_obj_aux-seqitem =  wg_zglt036-seqitem.
          wl_obj_aux-hkont   =  wg_zglt036-hkont.

          select single *
            from skat
            into wl_skat
            where spras eq sy-langu
            and ktopl eq '0050'
            and saknr eq  wg_zglt036-hkont.

          wl_obj_aux-txt50    = wl_skat-txt50.
          wl_obj_aux-quantity = wg_zglt036-quantity.
          wl_obj_aux-base_uom = wg_zglt036-base_uom.
          wl_obj_aux-vbund    = wg_zglt036-vbund.

*         Busca da taxa (IVA)

          select single t001~bukrs, t001~land1, t005~kalsm
            from t001
            inner join t005 on t001~land1 eq t005~land1
            into @data(wl_t001)
            where bukrs eq @wg_zglt035-bukrs.

          if ( sy-subrc = 0 ).

            select single *
              from t007s
              into @data(wl_iva)
              where spras eq @sy-langu
                and kalsm eq @wl_t001-kalsm
                and mwskz eq @wg_zglt036-tax_code.

            if ( wl_iva is not initial ).
              concatenate wl_iva-mwskz '-' wl_iva-text1 into wl_obj_aux-iva.
            endif.

          endif.

          wl_obj_aux-kostl    = wg_zglt036-kostl.
          wl_obj_aux-prctr    = wg_zglt036-prctr.
          wl_obj_aux-aufnr    = wg_zglt036-aufnr.
          wl_obj_aux-vornr    = wg_zglt036-vornr.
          wl_obj_aux-matnr    = wg_zglt036-matnr.

          read table tg_obj into wg_obj with key  seqitem = wg_zglt036-seqitem.
          if sy-subrc = 0.
            move-corresponding wg_obj  to wl_obj_aux.
            wl_obj_aux-vbund = wg_zglt036-vbund.
            concatenate wl_iva-mwskz '-' wl_iva-text1 into wl_obj_aux-iva.
          endif.
          append wl_obj_aux to tl_obj_aux.
          clear: wl_obj_aux, wl_t001, wl_iva.
        else.
          loop at  tg_obj into wg_obj where  seqitem = wg_zglt036-seqitem.
            move-corresponding wg_zglt036 to wl_obj_aux. "Adicionado as informações na workearea.BUG IMPEDITIVO 103379*/AOENNING.

            wl_obj_aux-seqitem =  wg_zglt036-seqitem.
            wl_obj_aux-hkont   =  wg_zglt036-hkont.
            select single *
              from skat
              into wl_skat
              where spras eq sy-langu
              and ktopl eq '0050'
              and saknr eq  wg_zglt036-hkont.

            move-corresponding wg_obj  to wl_obj_aux.
            wl_obj_aux-txt50 = wl_skat-txt50.
            wl_obj_aux-vbund = wg_zglt036-vbund.

            append wl_obj_aux to tl_obj_aux.
            clear wl_obj_aux.
          endloop.
        endif.
      endif.
    endloop.

*    ENDIF.

*  PERFORM F_MONTAR_ESTRUTURA_OBJ USING:
*        1   ' '       ' '                 'TG_OBJ'  'HKONT'                TEXT-A01          '12'  ' ' ' ' ' ' ' ' ' ',
*        1   ' '       ' '                 'TG_OBJ'  'TXT50'                TEXT-A02          '20'  ' ' ' ' ' ' ' ' ' ',
*        1   ' '       ' '                 'TG_OBJ'  'VBUND'                TEXT-A03          '12'  ' ' ' ' ' ' ' ' ' ',
*        1   'CSKS'    'KOSTL'             'TG_OBJ'  'KOSTL'                TEXT-A04          '12'  'X' ' ' ' ' ' ' ' ',
*        2   'CEPC'    'PRCTR'             'TG_OBJ'  'PRCTR'                TEXT-A05          '12'  'X' ' ' ' ' ' ' ' ',
*        3   'AFIH'    'AUFNR'             'TG_OBJ'  'AUFNR'                TEXT-A06          '12'  'X' ' ' ' ' ' ' ' ',
*        3   'AFVC'    'VORNR'             'TG_OBJ'  'VORNR'                TEXT-A49          '05'  'X' ' ' ' ' 'X' ' ',
*        4   'ZGLT036' 'MATNR'             'TG_OBJ'  'MATNR'                TEXT-A07          '18'  'X' ' ' ' ' ' ' ' ',
*        5   'ZGLT036' 'VLR_MOEDA_DOC'     'TG_OBJ'  'VLR_MOEDA_DOC'        TEXT-A08          '20'  'X' ' ' ' ' ' ' ' ',
*        5   'ZGLT036' 'VLR_MOEDA_INT'     'TG_OBJ'  'VLR_MOEDA_INT'        TEXT-A09          '20'  'X' ' ' ' ' ' ' ' ',
*        6   'ZGLT036' 'VLR_MOEDA_FORTE'   'TG_OBJ'  'VLR_MOEDA_FORTE'      TEXT-A10          '20'  'X' ' ' ' ' ' ' ' ',
*        7   'ZGLT036' 'VLR_MOEDA_GRUPO'   'TG_OBJ'  'VLR_MOEDA_GRUPO'      TEXT-A11          '20'  'X' ' ' ' ' ' ' ' ',
*        7   'ZGLT036' 'QUANTITY'          'TG_OBJ'  'QUANTITY'             TEXT-A46          '10'  'X' ' ' ' ' ' ' ' ',
*        7   'ZGLT036' 'BASE_UOM'          'TG_OBJ'  'BASE_UOM'             TEXT-A47          '07'  'X' ' ' ' ' ' ' ' '.
*
*
*    WL_LAYOUT-ZEBRA             = 'X'.
*    WL_PRINT-NO_PRINT_LISTINFOS = 'X'.
*
*    PERFORM F_CARREGAR_EVENTOS USING: SLIS_EV_USER_COMMAND  'F_USER_COMANDO_POPUP',
*                                      SLIS_EV_PF_STATUS_SET 'F_STATUS_POPUP'.
*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*      EXPORTING
*        I_CALLBACK_PROGRAM    = SY-REPID
*        IT_FIELDCAT           = WL_FIELDCAT[]
*        IS_LAYOUT             = WL_LAYOUT
*        IT_EVENTS             = POP_EVENTS
*        I_DEFAULT             = ' '
*        I_SAVE                = ' '
*        IS_PRINT              = WL_PRINT
*        I_SCREEN_START_COLUMN = 10
*        I_SCREEN_START_LINE   = 1
*        I_SCREEN_END_COLUMN   = 200
*        I_SCREEN_END_LINE     = 20
*      TABLES
*        T_OUTTAB              = TL_OBJ_AUX
*      EXCEPTIONS
*        PROGRAM_ERROR         = 1
*        OTHERS                = 2.

*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ELSE.

    call screen 0300 starting at 010 1
                     ending   at 230 20.

    if tl_obj_aux[] is initial.
      message text-i02 type 'I'.
      exit.
    endif.

*      IF WG_ZGLT035-TP_LCTO EQ 0.
*        READ TABLE TG_ZGLT036       INTO WG_ZGLT036      INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
*        DELETE TG_OBJ WHERE SEQITEM = WG_ZGLT036-SEQITEM.
*      ELSE.
    refresh tg_obj.
*      ENDIF.
    loop at tl_obj_aux into wg_obj.
      append wg_obj to tg_obj.
    endloop.

    loop at tg_zglt036 into wg_zglt036.
      if  wg_zglt036-xclasse = 'X'.
        loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
          move-corresponding  wg_obj to wg_zglt036.
        endloop.
        clear: wg_zglt036-vlr_moeda_int, wg_zglt036-vlr_moeda_forte, wg_zglt036-vlr_moeda_doc,wg_zglt036-vlr_moeda_grupo.

        loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
          add:  wg_obj-vlr_moeda_int    to wg_zglt036-vlr_moeda_int,
                wg_obj-vlr_moeda_forte  to wg_zglt036-vlr_moeda_forte,
                wg_obj-vlr_moeda_grupo  to wg_zglt036-vlr_moeda_grupo,
                wg_obj-vlr_moeda_doc    to wg_zglt036-vlr_moeda_doc.
        endloop.

        if wg_zglt036-d_c = 'C'.
          if wg_zglt036-vlr_moeda_int   >= 0.
            wg_zglt036-vlr_moeda_int    = wg_zglt036-vlr_moeda_int  * -1.
          endif.

          if wg_zglt036-vlr_moeda_forte >= 0.
            wg_zglt036-vlr_moeda_forte  = wg_zglt036-vlr_moeda_forte * -1.
          endif.

          if wg_zglt036-vlr_moeda_grupo >= 0.
            wg_zglt036-vlr_moeda_grupo  = wg_zglt036-vlr_moeda_grupo * -1.
          endif.

          if wg_zglt036-vlr_moeda_doc >= 0.
            wg_zglt036-vlr_moeda_doc  = wg_zglt036-vlr_moeda_doc * -1.
          endif.
        endif.
        if wg_zglt036-vlr_moeda_int ne 0 or wg_zglt036-vlr_moeda_forte ne 0 or wg_zglt036-vlr_moeda_grupo ne 0.
          wg_zglt036-kostl_c = 'X'.
        else.
          wg_zglt036-kostl_c = ' '.
        endif.
        modify tg_zglt036 from wg_zglt036.
      endif.
    endloop.
*    ENDIF.

*** Método de atualização de dados na Tela
    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.
  endmethod.                    "on_HOTSPOT_CLICK

  method on_double_click.

  endmethod.                    "ON_DOUBLE_CLICK

  method on_data_changed.
    data: ls_good     type lvc_s_modi,
          v_round(15) type p decimals 0,
          lv_value    type lvc_value,
          lv_valueg   type lvc_value,
          lv_valuef   type lvc_value,
          vdata       type tcurr-gdatu,
          vdata_f     type tcurr-gdatu,
          vdatax      type sy-datum,
          wl_zglt036  like line of tg_zglt036,
          tl_tcurr    type table of tcurr,
          wl_tcurr    type tcurr,
          wl_tcurr_f  type tcurr,
          wl_tcurr_g  type tcurr,
          vg_sinal(1),
          wl_t001     type t001,
          wl_t005     type t005,
          flag_r(1),
          v_anln1     type anla-anln1,
          v_anln2     type anla-anln2.

    data: wl_tbsl   type tbsl,
          wl_lfa1   type lfa1,
          wl_kna1   type kna1,
          wl_skat   type skat,
          wl_skb1   type skb1,
          wl_anla   type anla,
          wa_t095   type t095,
          wl_akont  type lfb1-akont,
          wl_hkont  type t074-hkont,
*          WL_HKONT2 TYPE ZGLT036-HKONT,
          wl_hkont2 type zib_contabil-hkont,
          wl_hkont3 type zglt036-hkont,
          wl_umskz  type zglt036-umskz,
          wl_cskb   type cskb,
          wl_csks   type csks,
          wl_tka02  type tka02.

    data: chdat(8)   type c,
          houtput(8) type n.

    clear: flag_r, vdata.


    loop at er_data_changed->mt_good_cells
                         into ls_good
                         where fieldname = 'BSCHL'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.

      select single * from tbsl
      into wl_tbsl
      where bschl eq lv_value.

      clear lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'D_C'
          i_value     = lv_value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'AKONT'
          i_value     = lv_value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR_A'
          i_value     = lv_value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'HKONT'
          i_value     = lv_value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR'
          i_value     = lv_value.

      check sy-subrc = 0.

      read table tg_zglt036 into wl_zglt036 index ls_good-row_id.
      if wl_tbsl-shkzg eq 'S'.
        wl_zglt036-d_c = 'D'.
      else.
        wl_zglt036-d_c = 'C'.
      endif.

      lv_value = wl_zglt036-d_c.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'D_C'
          i_value     = lv_value.

    endloop.

    loop at er_data_changed->mt_good_cells
                         into ls_good
                         where fieldname = 'HKONT' or fieldname = 'UMSKZ'.

      lv_value = ls_good-value.
      condense lv_value no-gaps.

      read table tg_zglt036 into wl_zglt036 index ls_good-row_id.

      data(tabix) = sy-tabix. "ALRS

      if  ls_good-fieldname = 'HKONT'.
        select single * from tbsl
           into wl_tbsl
           where bschl eq wl_zglt036-bschl.

        check sy-subrc = 0.
        if  wl_tbsl-koart ne 'A'.
          wl_hkont2 = lv_value.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wl_hkont2
            importing
              output = wl_hkont2.
          lv_value = wl_hkont2.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'HKONT'
              i_value     = lv_value.
        endif.

        wl_hkont2 = lv_value.
        wl_hkont3 = lv_value.
        wl_umskz  = wl_zglt036-umskz.
      else.
        wl_umskz  = lv_value.
        wl_hkont2 =  wl_zglt036-hkont.
        wl_hkont3 =  wl_zglt036-hkont.
      endif.

      read table tg_obj into wg_obj with key  seqitem = wl_zglt036-seqitem.
      if sy-subrc = 0.
        delete tg_obj where seqitem = wl_zglt036-seqitem.
      endif.

      clear lv_value.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'AKONT'
          i_value     = lv_value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR'
          i_value     = lv_value.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR_A'
          i_value     = lv_value.

      move-corresponding wl_zglt036 to wg_zglt036.
      wg_zglt036-hkont = wl_hkont2.
      wg_zglt036-akont = wl_hkont2.
      wg_zglt036-umskz = wl_umskz.

      select single * from tbsl
      into wl_tbsl
      where bschl eq wl_zglt036-bschl.
      check sy-subrc = 0.

      case wl_tbsl-koart.
        when 'K'.
          select single * from lfa1 into wl_lfa1 where lifnr eq wl_hkont2.
          check sy-subrc = 0.
          move: wl_lfa1-name1 to wg_zglt036-descr.
          if wl_lfa1-vbund is not initial.
            lv_value = wl_lfa1-vbund.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'VBUND'
                i_value     = lv_value.
          endif.
          "
          select single akont from lfb1 into wl_akont         where lifnr = wl_hkont2  and bukrs = wg_zglt035-bukrs.
          check sy-subrc = 0.
          wg_zglt036-akont = wl_akont.
          if wl_umskz ne ''.
            select single skont from t074 into wl_hkont
              where ktopl = '0050'
              and   koart = 'K'
              and   umskz = wl_umskz
              and   hkont = wl_akont.
            wg_zglt036-akont = wl_hkont.
          endif.
          select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                               and spras eq sy-langu
                                                               and ktopl eq '0050'.

        when 'D'.
          select single * from kna1 into wl_kna1 where kunnr eq wl_hkont2.
          check sy-subrc = 0.
          if wl_kna1-vbund is not initial.
            lv_value = wl_kna1-vbund.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'VBUND'
                i_value     = lv_value.
          endif.

          select single akont from knb1 into wl_akont         where kunnr = wl_hkont2  and bukrs = wg_zglt035-bukrs.
          wg_zglt036-akont = wl_akont.
          if wl_umskz ne ''.
            select single skont from t074 into wl_hkont
              where ktopl = '0050'
              and   koart = 'D'
              and   umskz = wl_umskz
              and   hkont = wl_akont.
            wg_zglt036-akont = wl_hkont.
          endif.
          select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                               and spras eq sy-langu
                                                               and ktopl eq '0050'.

          move: wl_kna1-name1 to wg_zglt036-descr.
        when 'S'.
          select single * from skat into wl_skat where saknr eq wl_hkont2
                                                   and spras eq sy-langu
                                                   and ktopl eq '0050'.
          move: wl_skat-txt50 to wg_zglt036-descr.
          wg_zglt036-descr_a = wg_zglt036-descr.
        when 'I'.
          split wl_hkont3  at '-' into v_anln1 v_anln2.
          if v_anln2 is initial.
            v_anln2 = '0'.
            concatenate v_anln1 '-' v_anln2 into wl_hkont3.
            lv_value = wl_hkont3.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'HKONT'
                i_value     = lv_value.
          endif.
          clear: wa_t095, wl_anla.
          select single * from anla into wl_anla where bukrs eq wg_zglt035-bukrs
                                                 and   anln1 eq v_anln1
                                                 and   anln2 eq v_anln2.

          move: wl_anla-mcoa1 to wg_zglt036-descr.
          wg_zglt036-descr_a = wg_zglt036-descr.

          select single * from t095 into wa_t095
            where ktopl = '0050'
            and   ktogr = wl_anla-ktogr
            and   afabe = 1.

          wg_zglt036-akont = wa_t095-ktansw.
        when 'A'.
          split wl_hkont3  at '-' into v_anln1 v_anln2.
          if v_anln2 is initial.
            v_anln2 = '0'.
            concatenate v_anln1 '-' v_anln2 into wl_hkont3.
            lv_value = wl_hkont3.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'HKONT'
                i_value     = lv_value.
          endif.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = v_anln1
            importing
              output = v_anln1.


          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = v_anln2
            importing
              output = v_anln2.

          clear: wa_t095, wl_anla.
          select single * from anla into wl_anla where bukrs eq wg_zglt035-bukrs
                                                 and   anln1 eq v_anln1
                                                 and   anln2 eq v_anln2.
*          MOVE: WL_ANLA-MCOA1 TO WG_ZGLT036-DESCR.
          concatenate wl_anla-txt50 wl_anla-txa50 into wg_zglt036-descr separated by space.

          select single * from t095 into wa_t095
            where ktopl = '0050'
            and   ktogr = wl_anla-ktogr
            and   afabe = 1.

          wg_zglt036-akont = wa_t095-ktansw.
          select single * from skat into wl_skat where saknr eq wg_zglt036-akont
                                                  and spras eq sy-langu
                                                  and ktopl eq '0050'.
          move: wl_skat-txt50 to wg_zglt036-descr_a.
      endcase.


      lv_value = wg_zglt036-akont.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'AKONT'
          i_value     = lv_value.

      lv_value = wg_zglt036-descr_a.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR_A'
          i_value     = lv_value.

      lv_value = wg_zglt036-descr.
      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR'
          i_value     = lv_value.

      select single *
        from tka02
        into wl_tka02
        where bukrs  = wg_zglt035-bukrs.

      clear wl_cskb.
      select single *                  "#EC CI_DB_OPERATION_OK[2389136]
          from cskb
          into wl_cskb
          where  kokrs  = wl_tka02-kokrs
          and    kstar  = wl_hkont2
          and    datab  le sy-datum
          and    datbi  ge sy-datum.

      "ALRS 27.11.2015
      clear wl_skb1.
      if '0200_0201_0202' cs wg_zglt035-bukrs. "EUROPA
        select single *                "#EC CI_DB_OPERATION_OK[2431747]
          from skb1
          into wl_skb1
          where bukrs = wg_zglt035-bukrs
          and   saknr = wl_hkont2
          and fstag   = 'YB09'.

      endif.


      "Limpar objetos de custo
      lv_value     = ''.

**  Begin of CS2022000638   #80266 FF   12.01.2023
      if tg_excel[] is initial. "Não limpar o VLR_MOEDA_DOC se estiver importando dados via excel.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_DOC'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_INT'
            i_value     = lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_FORTE'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_GRUPO'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VBUND'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'KOSTL'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'PRCTR'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'AUFNR'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'MATNR'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'KOSTL_C'
            i_value     = lv_value.

      else.
        modify tg_zglt036 from wg_zglt036 index tabix.
      endif.
** End of FF  12.01.2023

      if ( wl_cskb-kstar is not initial or wl_skb1-fstag   = 'YB09' ) and wl_tbsl-koart  = 'S'.
        move 'X' to wg_zglt036-xclasse.
        lv_value = wg_zglt036-xclasse.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'XCLASSE'
            i_value     = lv_value.

        lv_value     = '@1F@'.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'ICON'
            i_value     = lv_value.
      else.
        lv_value     = ''.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'XCLASSE'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'ICON'
            i_value     = lv_value.
      endif.
    endloop.

    loop at er_data_changed->mt_good_cells
                         into ls_good
                         where fieldname = 'CHECK'.
      call function 'SAPGUI_SET_FUNCTIONCODE'
        exporting
          functioncode           = '=ENT'
        exceptions
          function_not_supported = 1
          others                 = 2.
    endloop.

    if wg_zglt035-budat is not initial.
      if wg_zglt035-moeda_gp_hist = 'X'.
        vdatax = wg_zglt035-bldat.
      else.
        vdatax = wg_zglt035-budat.
        if wg_zglt035-st_fecha is not initial.
          add 1 to vdatax.
        endif.
      endif.

      move vdatax to chdat.
      houtput = '99999999' - chdat.
      vdata  = houtput.

      vdatax = wg_zglt035-budat - 180.
      move vdatax to chdat.
      houtput = '99999999' - chdat.
      vdata_f  = houtput.
    endif.


    select single *
      from t001
      into wl_t001
      where bukrs = wg_zglt035-bukrs.

    select single *
      from t005
      into wl_t005
      where land1 = wl_t001-land1.

    if wg_zglt035-moeda_interna is not initial.
      wl_t005-waers = wg_zglt035-moeda_interna.
    endif.
    if wl_t005-land1 = 'PA'.
      wl_t005-waers = 'USD'.
    endif.

    if wg_zglt035-moeda_forte is not initial.
      wl_t005-curha = wg_zglt035-moeda_forte.
    endif.
    loop at er_data_changed->mt_good_cells
                           into ls_good
                           where fieldname = 'MATNR_FI'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      read table tg_zglt036 into wl_zglt036 index ls_good-row_id.
      if wl_zglt036-check eq 'X'   and wg_zglt035-st_ap_fiscal = 'X' and lv_value is not initial.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = lv_value
          importing
            output = lv_value.
        concatenate 'T.F2 / M.' lv_value into lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'ZUONR'
            i_value     = lv_value.
      else.
*        CLEAR LV_VALUE.
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'MATNR_FI'
*            I_VALUE     = LV_VALUE.
      endif.
    endloop.

*Inicio Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266
    clear tg_zglt036_aux[].
    tg_zglt036_aux[] = tg_zglt036[].
*Fim Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266

    "moeda documento
    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_MOEDA_DOC'.

**  Begin of " CS2022000638   #80266 FF   02.02.2023
      read table tg_zglt036 assigning field-symbol(<fs_036>) index ls_good-row_id.
      if sy-subrc = 0.
        if <fs_036>-vlr_moeda_int is not initial and
           tg_excel[] is not initial.
          continue. "Não executar o processo abaixo, pois o campo vlr_moeda_int e outros valores,  já vieram preenchidos da planilha importada.
        endif.
      endif.
** End of FF  02.02.2023

      clear: wl_tcurr_g,wl_tcurr_f, wl_tcurr.
      lv_value  = ls_good-value.
      lv_valueg = ls_good-value.
      lv_valuef = ls_good-value.
      condense lv_value  no-gaps.
      condense lv_valueg no-gaps.
      condense lv_valuef no-gaps.
      if wg_zglt035-st_lc_moeda is initial and
*         WG_ZGLT035-MOEDA_FT_HIST IS INITIAL AND
         vdata is not initial.
        "Conversão da moeda doc para moeda forte
        select single *
           from tcurr
           into wl_tcurr_f
           where kurst = 'B'
           and   fcurr = wl_t005-curha
           and   tcurr = wg_zglt035-moeda_doc
           and   gdatu =  vdata.
        if sy-subrc ne 0.
          select  *
          from tcurr
          into table tl_tcurr
          where kurst = 'B'
          and   fcurr = wl_t005-curha
          and   tcurr = wg_zglt035-moeda_doc
          and   gdatu le  vdata_f
          order by gdatu ascending.
          clear wl_tcurr.
          if tl_tcurr[] is not initial.
            read table tl_tcurr into wl_tcurr_f index 1.
            refresh tl_tcurr.
          endif.
        endif.

        if wg_zglt035-taxa gt 0.
          wl_tcurr_f-ukurs = wg_zglt035-taxa.
          if '0201_0202' cs wg_zglt035-bukrs and wg_zglt035-moeda_doc = 'EUR'.
            multiply wl_tcurr_f-ukurs by -1.
          endif.
        endif.
        "Conversão da moeda doc para grupo
        select single *
          from tcurr
          into wl_tcurr_g
          where kurst = 'B'
          and   fcurr = wl_t005-curin
          and   tcurr = wg_zglt035-moeda_doc
          and   gdatu =  vdata.

        if sy-subrc ne 0.
          if wl_t005-curin = wg_zglt035-moeda_doc.
            wl_tcurr_g-ukurs = 1.
          else.
            select  *
              from tcurr
              into table tl_tcurr
              where kurst = 'B'
              and   fcurr = wl_t005-curin
              and   tcurr = wg_zglt035-moeda_doc
              and   gdatu le  vdata_f
              order by gdatu ascending.
            clear wl_tcurr_g.
            if tl_tcurr[] is not initial.
              read table tl_tcurr into wl_tcurr_g index 1.
              refresh tl_tcurr.
            endif.
          endif.
        endif.

        "Conversão da moeda doc para interna
        select single *
          from tcurr
          into wl_tcurr
          where kurst = 'B'
          and   fcurr =  wg_zglt035-moeda_doc
          and   tcurr = wl_t005-waers
          and   gdatu =  vdata.
        if sy-subrc ne 0.
          select  *
          from tcurr
          into table tl_tcurr
          where kurst = 'B'
          and   fcurr = wg_zglt035-moeda_doc
          and   tcurr = wl_t005-waers
          and   gdatu le  vdata_f
          order by gdatu ascending.
          clear wl_tcurr.
          if tl_tcurr[] is not initial.
            read table tl_tcurr into wl_tcurr index 1.
            refresh tl_tcurr.
          endif.
        endif.

        if wg_zglt035-taxa_i gt 0.
          wl_tcurr-ukurs = wg_zglt035-taxa_i.
          if '0201_0202' cs wg_zglt035-bukrs and wg_zglt035-moeda_doc = 'EUR'.
            multiply wl_tcurr-ukurs by -1.
          endif.
        endif.
      endif.

      read table tg_zglt036 into wl_zglt036 index ls_good-row_id.
      if wl_zglt036-check ne 'X'   .
        lv_value = 0.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_DOC'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_INT'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_FORTE'
            i_value     = lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_GRUPO'
            i_value     = lv_value.
      endif.

      if wl_zglt036-kostl_c = 'X' or wl_zglt036-xclasse = 'X' .
        lv_value = wl_zglt036-vlr_moeda_doc. "mantem o valor
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_DOC'
            i_value     = lv_value.
*        MESSAGE TEXT-I03 TYPE 'I'.
      elseif wl_tcurr-ukurs ne 0 and wg_zglt035-moeda_doc ne wl_t005-waers and wg_zglt035-st_lc_moeda is initial .
        if wl_tcurr-ukurs lt 0.
          multiply wl_tcurr-ukurs by -1.
          lv_value = lv_value / wl_tcurr-ukurs .
        else.
          lv_value = lv_value * wl_tcurr-ukurs .
        endif.
        if wg_zglt035-bukrs = '0101'.
          v_round  = lv_value.
          lv_value = v_round.
        endif.
        condense lv_value no-gaps.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_INT'
            i_value     = lv_value.

        "moeda forte
        if wl_tcurr_f-ukurs ne 0 and wg_zglt035-moeda_doc ne wl_t005-curha.
          if wl_tcurr_f-ukurs lt 0.
            multiply wl_tcurr_f-ukurs by -1.
            lv_valuef = lv_valuef * wl_tcurr_f-ukurs .
          else.
            lv_valuef = lv_valuef / wl_tcurr_f-ukurs .
          endif.
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
          flag_r = 'X'.
        else.
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
        endif.

        vg_sinal = '+'.
        if wl_tcurr_g-ukurs lt 0.
          vg_sinal = '-'.
          multiply wl_tcurr_g-ukurs by -1.
        endif.
        "moeda do grupo
        if wl_tcurr_g-ukurs ne 0.
          if vg_sinal = '+'.
            lv_valueg = lv_valueg / wl_tcurr_g-ukurs .
          else.
            lv_valueg = lv_valueg * wl_tcurr_g-ukurs .
          endif.
        endif.
        if wl_t005-curin ne 'UFIR'.
          condense lv_valueg no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_GRUPO'
              i_value     = lv_valueg.
        endif.
        flag_r = 'X'.
      elseif  wg_zglt035-moeda_doc = wl_t005-waers and wg_zglt035-st_lc_moeda is initial .
        condense lv_value no-gaps.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_INT'
            i_value     = lv_value.

        "moeda forte
        if wl_tcurr_f-ukurs ne 0 and wg_zglt035-moeda_doc ne wl_t005-curha.
          if wl_tcurr_f-ukurs lt 0.
            multiply  wl_tcurr_f-ukurs by -1.
            lv_valuef = lv_valuef * wl_tcurr_f-ukurs .
          else.
            lv_valuef = lv_valuef / wl_tcurr_f-ukurs .
          endif.
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
          flag_r = 'X'.
        else.
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
        endif.

        "moeda do grupo
        vg_sinal = '+'.
        if wl_tcurr_g-ukurs lt 0.
          vg_sinal = '-'.
          multiply wl_tcurr_g-ukurs by -1.
        endif.
        if wl_tcurr_g-ukurs ne 0.
          if vg_sinal = '+'.
            lv_valueg = lv_valueg / wl_tcurr_g-ukurs .
          else.
            lv_valueg = lv_valueg * wl_tcurr_g-ukurs .
          endif.
        endif.
        if wl_t005-curin ne 'UFIR'.
          condense lv_valueg no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_GRUPO'
              i_value     = lv_valueg.
        endif.
        flag_r = 'X'.
      endif.

      if wg_zglt035-bukrs = '0200'.
        "Conversão da moeda doc para interna
        select single *
          from tcurr
          into wl_tcurr
          where kurst = 'B'
          and   fcurr = 'USD'
          and   tcurr = 'CHF'
          and   gdatu = vdata.

        if sy-subrc ne 0.
          select  *
          from tcurr
          into table tl_tcurr
          where kurst = 'B'
          and   fcurr = 'USD'
          and   tcurr = 'CHF'
          and   gdatu le  vdata_f
          order by gdatu ascending.
          clear wl_tcurr.
          if tl_tcurr[] is not initial.
            read table tl_tcurr into wl_tcurr index 1.
            refresh tl_tcurr.
          endif.
        endif.

        "moeda forte
        if wl_tcurr-ukurs ne 0 .
          lv_valuef = lv_value / wl_tcurr-ukurs .
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
          flag_r = 'X'.
        endif.
      endif.

*Inicio Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266
      read table tg_zglt036_aux assigning <fs_036> index ls_good-row_id.
      if sy-subrc = 0.
        if <fs_036>-vlr_moeda_int is not initial and
           tg_excel[] is not initial.
*         "já vieram preenchidos da planilha importada.
          lv_value = <fs_036>-vlr_moeda_doc. "mantem o valor
          condense lv_value no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_DOC'
              i_value     = lv_value.
        endif.

        if <fs_036>-vlr_moeda_forte is not initial and
           tg_excel[] is not initial.
*         já vieram preenchidos da planilha importada.
          lv_value = <fs_036>-vlr_moeda_forte. "mantem o valor
          condense lv_value no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_value.
        endif.

        if <fs_036>-vlr_moeda_int is not initial and
           tg_excel[] is not initial.
*         já vieram preenchidos da planilha importada.
          lv_value = <fs_036>-vlr_moeda_int. "mantem o valor
          condense lv_value no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_INT'
              i_value     = lv_value.
        endif.
      endif.
*Fim Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266
    endloop.

    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_MOEDA_INT' or
                                   fieldname = 'VLR_MOEDA_FORTE' or
                                   fieldname = 'VLR_MOEDA_GRUPO'.
      read table tg_zglt036 into wl_zglt036 index ls_good-row_id.
      if wl_zglt036-check ne 'X'   .

        loop at tg_obj into wg_obj where seqitem = wg_zglt036-seqitem.
          wg_obj-vlr_moeda_int    = 0.
          wg_obj-vlr_moeda_forte  = 0.
          wg_obj-vlr_moeda_grupo  = 0.
          modify tg_obj from wg_obj index sy-tabix transporting vlr_moeda_int vlr_moeda_forte vlr_moeda_grupo.
        endloop.

        lv_value = 0.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_INT'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_FORTE'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_GRUPO'
            i_value     = lv_value.
      endif.
      if wl_zglt036-kostl_c = 'X' or wl_zglt036-xclasse = 'X' .
        if ls_good-fieldname = 'VLR_MOEDA_INT'.
          lv_value = wl_zglt036-vlr_moeda_int. "mantem o valor
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_INT'
              i_value     = lv_value.
        endif.

        if ls_good-fieldname = 'VLR_MOEDA_FORTE'.
          lv_value = wl_zglt036-vlr_moeda_forte. "mantem o valor
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_value.
        endif.

        if ls_good-fieldname = 'VLR_MOEDA_GRUPO'.
          lv_value = wl_zglt036-vlr_moeda_grupo. "mantem o valor
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_GRUPO'
              i_value     = lv_value.
        endif.

      endif.

    endloop.


    "SOMENTE CALCULAR MOEDA DOC
*    IF  FLAG_R NE 'X'.
*      LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                               INTO LS_GOOD
*                               WHERE FIELDNAME = 'VLR_MOEDA_INT'.
*        CLEAR WL_TCURR.
*        LV_VALUE = LS_GOOD-VALUE.
*        LV_VALUEG = LS_GOOD-VALUE.
*        LV_VALUEF = LS_GOOD-VALUE.
*        CONDENSE LV_VALUE NO-GAPS.
*        CONDENSE LV_VALUEG NO-GAPS.
*        CONDENSE LV_VALUEF NO-GAPS.
**        IF WG_ZGLT035-MOEDA_FT_HIST IS INITIAL AND VDATA IS NOT INITIAL. "Moeda forte
*
*        "Conversão da moeda doc para grupo
*        SELECT SINGLE *
*          FROM TCURR
*          INTO WL_TCURR_G
*          WHERE KURST = 'B'
*          AND   FCURR = WL_T005-CURIN
*          AND   TCURR = WL_T005-WAERS
*          AND   GDATU =  VDATA.
*
*        IF SY-SUBRC NE 0.
*          IF WL_T005-CURIN = WL_T005-WAERS.
*            WL_TCURR_G-UKURS = 1.
*          ELSE.
*            SELECT  *
*              FROM TCURR
*              INTO TABLE TL_TCURR
*              WHERE KURST = 'B'
*              AND   FCURR = WL_T005-CURIN
*              AND   TCURR = WL_T005-WAERS
*              AND   GDATU LE  VDATA_F
*              ORDER BY GDATU ASCENDING.
*            CLEAR WL_TCURR_G.
*            IF TL_TCURR[] IS NOT INITIAL.
*              READ TABLE TL_TCURR INTO WL_TCURR_G INDEX 1.
*              REFRESH TL_TCURR.
*            ENDIF.
*          ENDIF.
*        ENDIF.
**        IF WL_TCURR_G-UKURS LT 0.
**          WL_TCURR_G-UKURS = WL_TCURR_G-UKURS * -1.
**        ENDIF.
*
*        SELECT SINGLE *
*          FROM TCURR
*          INTO WL_TCURR
*          WHERE KURST = 'B'
*          AND   FCURR = WL_T005-CURHA
*          AND   TCURR = WL_T005-WAERS
*          AND   GDATU =  VDATA.
*        IF SY-SUBRC NE 0.
*          SELECT  *
*          FROM TCURR
*          INTO TABLE TL_TCURR
*          WHERE KURST = 'B'
*          AND   FCURR = WL_T005-CURHA
*          AND   TCURR = WL_T005-WAERS
*          AND   GDATU LE  VDATA_F
*          ORDER BY GDATU ASCENDING.
*          CLEAR WL_TCURR.
*          IF TL_TCURR[] IS NOT INITIAL.
*            READ TABLE TL_TCURR INTO WL_TCURR INDEX 1.
*            REFRESH TL_TCURR.
*          ENDIF.
*        ENDIF.
*
**        IF WL_TCURR-UKURS LT 0.
**          WL_TCURR-UKURS = WL_TCURR-UKURS * -1.
**        ENDIF.
*
*        IF WG_ZGLT035-TAXA_I GT 0.
*          WL_TCURR-UKURS = WG_ZGLT035-TAXA_I.
*        ENDIF.
**        ENDIF.
*        READ TABLE TG_ZGLT036 INTO WL_ZGLT036 INDEX LS_GOOD-ROW_ID.
*        IF WL_ZGLT036-CHECK NE 'X'   .
*          LV_VALUE = 0.
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'VLR_MOEDA_INT'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'VLR_MOEDA_FORTE'
*              I_VALUE     = LV_VALUE.
*
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'VLR_MOEDA_GRUPO'
*              I_VALUE     = LV_VALUE.
*        ENDIF.
*        IF WL_ZGLT036-KOSTL_C = 'X' OR WL_ZGLT036-XCLASSE = 'X' .
*          LV_VALUE = WL_ZGLT036-VLR_MOEDA_INT. "mantem o valor
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'VLR_MOEDA_INT'
*              I_VALUE     = LV_VALUE.
**          MESSAGE TEXT-I03 TYPE 'I'.
*        ELSEIF WL_TCURR-UKURS NE 0 AND WG_ZGLT035-MOEDA_DOC = WL_T005-WAERS
*               AND WG_ZGLT035-ST_LC_MOEDA IS INITIAL.
*
*          IF WL_TCURR-UKURS LT 0.
*            MULTIPLY WL_TCURR-UKURS BY -1.
*            LV_VALUE = LV_VALUE * WL_TCURR-UKURS .
*          ELSE.
*            LV_VALUE = LV_VALUE / WL_TCURR-UKURS .
*          ENDIF.
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'VLR_MOEDA_FORTE'
*              I_VALUE     = LV_VALUE.
*
*          "moeda do grupo
*          VG_SINAL = '+'.
*          IF WL_TCURR_G-UKURS LT 0.
*            VG_SINAL = '-'.
*            MULTIPLY WL_TCURR_G-UKURS BY -1.
*          ENDIF.
*          IF WL_TCURR_G-UKURS NE 0.
*            IF VG_SINAL = '+'.
*              LV_VALUEG = LV_VALUEG / WL_TCURR_G-UKURS .
*            ELSE.
*              LV_VALUEG = LV_VALUEG * WL_TCURR_G-UKURS .
*            ENDIF.
*          ENDIF.
*          IF WL_T005-CURIN NE 'UFIR'.
*            CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*              EXPORTING
*                I_ROW_ID    = LS_GOOD-ROW_ID
*                I_FIELDNAME = 'VLR_MOEDA_GRUPO'
*                I_VALUE     = LV_VALUEG.
*          ENDIF.
*
*          FLAG_R = 'X'.
*        ENDIF.
*
*      ENDLOOP.
*    ENDIF.
*
*    IF FLAG_R NE 'X'.
*      LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                               INTO LS_GOOD
*                               WHERE FIELDNAME = 'VLR_MOEDA_FORTE'.
*        CLEAR WL_TCURR.
*        LV_VALUE = LS_GOOD-VALUE.
*        CONDENSE LV_VALUE NO-GAPS.
*        IF WG_ZGLT035-MOEDA_INT_HIST IS INITIAL AND VDATA IS NOT INITIAL. "Moeda interna
*          SELECT SINGLE *
*            FROM TCURR
*            INTO WL_TCURR
*            WHERE KURST = 'B'
*            AND   FCURR = WL_T005-CURHA
*            AND   TCURR = WL_T005-WAERS
*            AND   GDATU =  VDATA.
*          IF SY-SUBRC NE 0.
*            SELECT  *
*            FROM TCURR
*            INTO TABLE TL_TCURR
*            WHERE KURST = 'B'
*            AND   FCURR = WL_T005-CURHA
*            AND   TCURR = WL_T005-WAERS
*            AND   GDATU LE  VDATA_F
*            ORDER BY GDATU ASCENDING.
*            CLEAR WL_TCURR.
*            IF TL_TCURR[] IS NOT INITIAL.
*              READ TABLE TL_TCURR INTO WL_TCURR INDEX 1.
*              REFRESH TL_TCURR.
*            ENDIF.
*          ENDIF.
*
**          IF WL_TCURR-UKURS LT 0.
**            WL_TCURR-UKURS = WL_TCURR-UKURS * -1.
**          ENDIF.
*
*          IF WG_ZGLT035-TAXA GT 0.
*            WL_TCURR-UKURS = WG_ZGLT035-TAXA.
*          ENDIF.
*        ENDIF.
*        READ TABLE TG_ZGLT036 INTO WL_ZGLT036 INDEX LS_GOOD-ROW_ID.
*        IF WL_ZGLT036-CHECK NE 'X'   .
*          LV_VALUE = 0.
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'VLR_MOEDA_FORTE'
*              I_VALUE     = LV_VALUE.
*        ENDIF.
*        IF WL_ZGLT036-KOSTL_C = 'X' OR WL_ZGLT036-XCLASSE = 'X' .
*          LV_VALUE = WL_ZGLT036-VLR_MOEDA_FORTE. "mantem o valor
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'VLR_MOEDA_FORTE'
*              I_VALUE     = LV_VALUE.
**          MESSAGE TEXT-I03 TYPE 'I'.
*        ELSEIF WL_TCURR-UKURS NE 0 AND WG_ZGLT035-MOEDA_DOC = WL_T005-CURHA
*               AND WG_ZGLT035-ST_LC_MOEDA IS INITIAL.
*          IF WL_TCURR-UKURS LT 0.
*            MULTIPLY WL_TCURR-UKURS BY -1.
*            LV_VALUE = LV_VALUE / WL_TCURR-UKURS . "mantem o valor
*          ELSE.
*            LV_VALUE = LV_VALUE * WL_TCURR-UKURS . "mantem o valor
*          ENDIF.
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'VLR_MOEDA_INT'
*              I_VALUE     = LV_VALUE.
*        ENDIF.
*
*      ENDLOOP.
*    ENDIF.
*
*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                         INTO LS_GOOD
*                         WHERE FIELDNAME = 'VLR_MOEDA_GRUPO'.
*
*      READ TABLE TG_ZGLT036 INTO WL_ZGLT036 INDEX LS_GOOD-ROW_ID.
*      IF WL_ZGLT036-CHECK NE 'X'   .
*        LV_VALUE = 0.
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'VLR_MOEDA_GRUPO'
*            I_VALUE     = LV_VALUE.
*      ENDIF.
*      IF WL_ZGLT036-KOSTL_C = 'X' OR WL_ZGLT036-XCLASSE = 'X' .
*        LV_VALUE = WL_ZGLT036-VLR_MOEDA_GRUPO. "mantem o valor
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'VLR_MOEDA_GRUPO'
*            I_VALUE     = LV_VALUE.
**        MESSAGE TEXT-I03 TYPE 'I'.
*      ENDIF.
*
*    ENDLOOP.
    "SOMENTE CALCULAR MOEDA DOC

    loop at er_data_changed->mt_good_cells
                        into ls_good
                        where fieldname = 'CHECK'.
      read table tg_zglt036 into wl_zglt036 index ls_good-row_id.

**  Begin of " CS2022000638   #80266 FF   02.02.2023
      if sy-subrc = 0.
        if wl_zglt036-vlr_moeda_int is not initial and
           tg_excel[] is not initial.
          continue. "Não executar o processo abaixo, pois o campo vlr_moeda_int e outros valores,  já vieram preenchidos da planilha importada.
        endif.
      endif.
** End of FF  02.02.2023

      if wl_zglt036-check eq 'X'   .
        lv_value = 0.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_INT'
            i_value     = lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_FORTE'
            i_value     = lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_DOC'
            i_value     = lv_value.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_GRUPO'
            i_value     = lv_value.
      endif.

    endloop.

**  Begin of CS2022000638   #80266 FF   18.01.2023
    loop at er_data_changed->mt_good_cells
                         into ls_good
                         where fieldname = 'GSBER'.

      lv_value = ls_good-value.
      condense lv_value no-gaps.

      call method er_data_changed->modify_cell
        exporting
          i_row_id    = ls_good-row_id
          i_fieldname = 'GSBER'
          i_value     = lv_value.
    endloop.
** End of FF  18.01.2023

  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_pop.
    data: ls_good     type lvc_s_modi,
          v_round(15) type p decimals 0,
          lv_value    type lvc_value,
          lv_valueg   type lvc_value,
          lv_valuef   type lvc_value,
          vg_sinal(1),
          vdata       type tcurr-gdatu,
          vdata_f     type tcurr-gdatu,
          vdatax      type sy-datum,
          wl_zglt036  like line of tg_zglt036,
          tl_tcurr    type table of tcurr,
          wl_tcurr    type tcurr,
          wl_tcurrg   type tcurr,
          wl_tcurr_f  type tcurr,
          wl_aufk     type aufk,
          wl_t001     type t001,
          wl_t005     type t005,
          wl_obj_aux  type ty_obj,
          flag_r(1).


    select single *
      from t001
      into wl_t001
      where bukrs = wg_zglt035-bukrs.

    select single *
      from t005
      into wl_t005
      where land1 = wl_t001-land1.

    if wg_zglt035-moeda_interna is not initial.
      wl_t005-waers = wg_zglt035-moeda_interna.
    endif.

    if wg_zglt035-moeda_forte is not initial.
      wl_t005-curha = wg_zglt035-moeda_forte.
    endif.

    data: chdat(8)   type c,
          houtput(8) type n.
    clear:       flag_r, vdata,vdata_f.
    if wg_zglt035-budat is not initial.
      if wg_zglt035-moeda_gp_hist = 'X'.
        vdatax = wg_zglt035-bldat.
      else.
        vdatax = wg_zglt035-budat.
        if wg_zglt035-st_fecha is not initial.
          add 1 to vdatax.
        endif.
      endif.

      move vdatax to chdat.
      houtput = '99999999' - chdat.
      vdata  = houtput.

      vdatax = wg_zglt035-budat - 180.
      move vdatax to chdat.
      houtput = '99999999' - chdat.
      vdata_f  = houtput.

    endif.

    "moeda documento
    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'AUFNR'.
      lv_value = ls_good-value.
      condense lv_value no-gaps.
      wg_zglt036-aufnr = lv_value.
      select single *
                  from aufk
                  into wl_aufk
                  where bukrs  = wg_zglt035-bukrs
                  and   aufnr  = wg_zglt036-aufnr.
      if sy-subrc eq 0.
        lv_value = wl_aufk-kostl. "KOSTV.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'KOSTL'
            i_value     = lv_value.
      endif.
    endloop.

*Inicio Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266
    clear tg_zglt036_aux[].
    tg_zglt036_aux[] = tg_zglt036[].
*Fim Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266

    "moeda documento
    loop at er_data_changed->mt_good_cells
                             into ls_good
                             where fieldname = 'VLR_MOEDA_DOC'.

      clear:  wl_tcurrg, wl_tcurr.
      lv_value = ls_good-value.
      lv_valueg = ls_good-value.
      lv_valuef = ls_good-value.
      condense lv_value no-gaps.
      condense lv_valueg no-gaps.
      condense lv_valuef no-gaps.
      if wg_zglt035-st_lc_moeda is initial and
*         WG_ZGLT035-MOEDA_FT_HIST IS INITIAL AND
         vdata is not initial.
        "Conversão da moeda doc para moeda forte
        select single *
           from tcurr
           into wl_tcurr_f
           where kurst = 'B'
           and   fcurr = wl_t005-curha
           and   tcurr = wg_zglt035-moeda_doc
           and   gdatu =  vdata.
        if sy-subrc ne 0.
          select  *
          from tcurr
          into table tl_tcurr
          where kurst = 'B'
          and   fcurr = wl_t005-curha
          and   tcurr = wg_zglt035-moeda_doc
          and   gdatu le  vdata_f
          order by gdatu ascending.
          clear wl_tcurr.
          if tl_tcurr[] is not initial.
            read table tl_tcurr into wl_tcurr_f index 1.
            refresh tl_tcurr.
          endif.
        endif.


        if wg_zglt035-taxa gt 0.
          wl_tcurr_f-ukurs = wg_zglt035-taxa.
          if '0201_0202' cs wg_zglt035-bukrs and wg_zglt035-moeda_doc = 'EUR'.
            multiply wl_tcurr_f-ukurs by -1.
          endif.
        endif.

        "Conversão da moeda doc para Grupo
        select single *
          from tcurr
          into wl_tcurrg
          where kurst = 'B'
          and   fcurr = wl_t005-curin
          and   tcurr = wg_zglt035-moeda_doc
          and   gdatu =  vdata.

        if sy-subrc ne 0.
          if wl_t005-curin = wg_zglt035-moeda_doc.
            wl_tcurrg-ukurs = 1.
          else.
            select  *
              from tcurr
              into table tl_tcurr
              where kurst = 'B'
              and   fcurr = wl_t005-curin
              and   tcurr = wg_zglt035-moeda_doc
              and   gdatu le  vdata_f
              order by gdatu ascending.
            clear wl_tcurrg.
            if tl_tcurr[] is not initial.
              read table tl_tcurr into wl_tcurrg index 1.
              refresh tl_tcurr.
            endif.
          endif.
        endif.

        "Conversão da moeda doc para interna
        select single *
          from tcurr
          into wl_tcurr
          where kurst = 'B'
          and   fcurr = wg_zglt035-moeda_doc
          and   tcurr = wl_t005-waers
          and   gdatu = vdata.

        if sy-subrc ne 0.
          select  *
          from tcurr
          into table tl_tcurr
          where kurst = 'B'
          and   fcurr = wg_zglt035-moeda_doc
          and   tcurr = wl_t005-waers
          and   gdatu le  vdata_f
          order by gdatu ascending.
          clear wl_tcurr.
          if tl_tcurr[] is not initial.
            read table tl_tcurr into wl_tcurr index 1.
            refresh tl_tcurr.
          endif.
        endif.

        if wg_zglt035-taxa_i gt 0.
          wl_tcurr-ukurs = wg_zglt035-taxa_i.
          if '0201_0202' cs wg_zglt035-bukrs and wg_zglt035-moeda_doc = 'EUR'.
            multiply wl_tcurr-ukurs by -1.
          endif.
        endif.
      endif.
      read table tl_obj_aux into wl_obj_aux index ls_good-row_id.
      read table tg_zglt036 into wl_zglt036 with key seqitem = wl_obj_aux-seqitem.
      if wl_zglt036-check ne 'X'   .
        lv_value = 0.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_DOC'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_INT'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_FORTE'
            i_value     = lv_value.

        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_GRUPO'
            i_value     = lv_value.
      endif.

      if wl_tcurr-ukurs ne 0 and wg_zglt035-moeda_doc ne wl_t005-waers and wg_zglt035-st_lc_moeda is initial .
        "alrs
        if wl_tcurr-ukurs lt 0.
          multiply wl_tcurr-ukurs by -1.
          lv_value = lv_value / wl_tcurr-ukurs .
        else.
          lv_value = lv_value * wl_tcurr-ukurs .
        endif.
        if wg_zglt035-bukrs = '0101'.
          v_round  = lv_value.
          lv_value = v_round.
        endif.
        condense lv_value no-gaps.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_INT'
            i_value     = lv_value.

        "moeda forte
        if wl_tcurr_f-ukurs ne 0 and wg_zglt035-moeda_doc ne wl_t005-curha.
          if wl_tcurr_f-ukurs lt 0.
            multiply wl_tcurr_f-ukurs by -1.
            lv_valuef = lv_valuef * wl_tcurr_f-ukurs .
          else.
            lv_valuef = lv_valuef / wl_tcurr_f-ukurs .
          endif.
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
          flag_r = 'X'.
        else.
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
        endif.

        vg_sinal = '+'.
        if wl_tcurrg-ukurs lt 0.
          vg_sinal = '-'.
          multiply wl_tcurrg-ukurs by -1.
        endif.
        if wl_tcurrg-ukurs ne 0.
          if vg_sinal = '+'.
            lv_valueg = lv_valueg / wl_tcurrg-ukurs . "mantem o valor
          else.
            lv_valueg = lv_valueg * wl_tcurrg-ukurs . "mantem o valor
          endif.
        endif.
        if wl_t005-curin ne 'UFIR'.
          condense lv_valueg no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_GRUPO'
              i_value     = lv_valueg.
        endif.
        flag_r = 'X'.
      elseif  wg_zglt035-moeda_doc = wl_t005-waers and wg_zglt035-st_lc_moeda is initial .
        condense lv_value no-gaps.
        call method er_data_changed->modify_cell
          exporting
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_MOEDA_INT'
            i_value     = lv_value.

        "moeda forte
        if wl_tcurr_f-ukurs ne 0 and wg_zglt035-moeda_doc ne wl_t005-curha.
          if  wl_tcurr_f-ukurs lt 0.
            multiply  wl_tcurr_f-ukurs by -1.
            lv_valuef = lv_valuef * wl_tcurr_f-ukurs .
          else.
            lv_valuef = lv_valuef / wl_tcurr_f-ukurs .
          endif.
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
          flag_r = 'X'.
        else.
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
        endif.

        vg_sinal = '+'.
        if wl_tcurrg-ukurs lt 0.
          vg_sinal = '-'.
          multiply wl_tcurrg-ukurs by -1.
        endif.
        if wl_tcurrg-ukurs ne 0.
          if vg_sinal = '+'.
            lv_valueg = lv_valueg / wl_tcurrg-ukurs . "mantem o valor
          else.
            lv_valueg = lv_valueg * wl_tcurrg-ukurs . "mantem o valor
          endif.
        endif.
        if wl_t005-curin ne 'UFIR'.
          condense lv_valueg no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_GRUPO'
              i_value     = lv_valueg.
        endif.
        flag_r = 'X'.
      endif.
      if wg_zglt035-bukrs = '0200'.
        "Conversão da moeda doc para interna
        select single *
          from tcurr
          into wl_tcurr
          where kurst = 'B'
          and   fcurr = 'USD'
          and   tcurr = 'CHF'
          and   gdatu = vdata.

        if sy-subrc ne 0.
          select  *
          from tcurr
          into table tl_tcurr
          where kurst = 'B'
          and   fcurr = 'USD'
          and   tcurr = 'CHF'
          and   gdatu le  vdata_f
          order by gdatu ascending.
          clear wl_tcurr.
          if tl_tcurr[] is not initial.
            read table tl_tcurr into wl_tcurr index 1.
            refresh tl_tcurr.
          endif.
        endif.

        "moeda forte
        if wl_tcurr-ukurs ne 0 .
          lv_valuef = lv_value / wl_tcurr-ukurs .
          condense lv_valuef no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_valuef.
          flag_r = 'X'.
        endif.
      endif.

*Inicio Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266
      read table tg_zglt036_aux assigning field-symbol(<fs_036>) index ls_good-row_id.
      if sy-subrc = 0.
        if <fs_036>-vlr_moeda_int is not initial and
           tg_excel[] is not initial.
*         "já vieram preenchidos da planilha importada.
          lv_value = <fs_036>-vlr_moeda_doc. "mantem o valor
          condense lv_value no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_DOC'
              i_value     = lv_value.
        endif.

        if <fs_036>-vlr_moeda_forte is not initial and
           tg_excel[] is not initial.
*         já vieram preenchidos da planilha importada.
          lv_value = <fs_036>-vlr_moeda_forte. "mantem o valor
          condense lv_value no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_FORTE'
              i_value     = lv_value.
        endif.

        if <fs_036>-vlr_moeda_int is not initial and
           tg_excel[] is not initial.
*         já vieram preenchidos da planilha importada.
          lv_value = <fs_036>-vlr_moeda_int. "mantem o valor
          condense lv_value no-gaps.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_INT'
              i_value     = lv_value.
        endif.

        if tg_excel[] is not initial.
          loop at tg_obj assigning field-symbol(<fs_obj>).
            read table tg_zglt036 into wl_zglt036 with key seqitem = <fs_obj>-seqitem.
            if sy-subrc eq 0.
              <fs_obj>-vlr_moeda_doc = wl_zglt036-vlr_moeda_doc.
              <fs_obj>-vlr_moeda_int = wl_zglt036-vlr_moeda_int.
              <fs_obj>-vlr_moeda_forte = wl_zglt036-vlr_moeda_forte.
              <fs_obj>-vlr_moeda_grupo = wl_zglt036-vlr_moeda_grupo.
            endif.
          endloop.
        endif.
      endif.
*Fim Alteração - Leandro Valentim Ferreira - 11.05.23 - 80266

    endloop.

***  Begin of CS2022000638   #80266 FF   02.02.2023
*    LOOP AT er_data_changed->mt_good_cells
*                         INTO ls_good
*                         WHERE fieldname = 'KOSTL'.
*
*      lv_value = ls_good-value.
*      CONDENSE lv_value NO-GAPS.
*
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'KOSTL'
*          i_value     = lv_value.
*    ENDLOOP.
*** End of FF  02.02.2023


  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_finished.

    perform: f_atualiza_alv.

*** Método de atualização de dados na Tela
    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.
    refresh tg_msg_ret.
    "ALRS 30.11.2015 para deixar rapido
*    PERFORM F_VERIFICA_ERROS.
    call function 'Z_DOC_CHECK_NEW'
      exporting
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'TS_100-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      importing
        e_messagem    = wg_mensagem
      tables
        it_msgs       = tg_msg_ret.

  endmethod.                    "on_data_changed_finished

  method on_onf4.

    field-symbols: <stylet> type lvc_t_styl.
    field-symbols: <stylel> type lvc_s_styl.

    types: begin of ty_field,
             tabname   type dd03l-tabname,     "Nome da tabela
             fieldname type dd03l-fieldname,   "Nome de campo
             s(1)      type c,
           end of ty_field,

           begin of ty_value,
             tabname    type dd03l-tabname,     "Nome da tabela
             fieldname  type dd03l-fieldname,   "Nome de campo
             char79(79) type c,
           end of ty_value,

           begin of ty_t012,
             hbkid type t012-hbkid,
             text1 type t012t-text1,
           end of ty_t012.

    data: begin of wl_valuetab,
            field(50),
          end of wl_valuetab.

    data: wl_kunnr  type kna1-kunnr,
          wl_lifnr  type lfa1-lifnr,
          wl_akont  type lfb1-akont,
          wl_hkont  type t074-hkont,
          wl_anln1  type anla-anln1,
          wl_bvtyp  type bseg-bvtyp,
          w_div1(1),
          w_div2(1).

    data: tl_valuetab      like table of wl_valuetab,
          tl_field         type table of ty_field,
          wl_field         type ty_field,
          tl_value         type table of ty_value,
          wl_value         type ty_value,
          wl_j_1bbranch    type j_1bbranch,
          tl_j_1bbranch    type table of j_1bbranch,
          wl_tgsbt         type tgsbt,
          tl_tgsbt         type table of tgsbt,
          tl_tgsb          type table of tgsb,
          wl_tgsb          type tgsb,
          wl_tbsl          type tbsl,
          tl_lfa1          type table of lfa1,
          wl_lfa1          type lfa1,
          tl_kna1          type table of kna1,
          wl_kna1          type kna1,
          tl_skat          type table of skat,
          wl_skat          type skat,
          tl_anla          type table of anla,
          wl_anla          type anla,
          wl_cskb          type cskb,
          wl_tka02         type tka02,
          wl_t001          type t001,
          wl_t005          type t005,
          tl_t012          type table of ty_t012,
          wl_t012          type          ty_t012,
          tl_t042z         type table of t042z,
          wl_t042z         type          t042z,
          tl_t074u         type table of t074u,
          wl_t074u         type t074u,
          tl_t074t         type table of t074t,
          wl_t074t         type t074t,
          tl_t007s         type table of t007s,
          tl_t007a         type table of t007a,
          wl_t007s         type t007s,
          wl_t007a         type t007a,
          tl_t030k         type table of t030k,
          wl_t030k         type t030k,
          wa_caufv         type caufv,
          wl_afvc          type afvc,
          tl_afvc          type table of afvc,
          t_set            type standard table of setleaf,
          w_set            type setleaf,
          t_linha          type standard table of setlinet,
          w_linha          type setlinet,
          vl_forma         type t042-bukrs,
          wl_index         type sy-tabix,
          wl_char(20),
          wl_fieldname(30),
          wl_tabname(30),
          t_divisao        type standard table of  rgsb4,
          w_divisao        type rgsb4.

    clear: wg_zglt036, wg_obj .
    clear: w_div1, w_div2.
    if e_fieldname = 'VORNR'.
      read table tl_obj_aux into wg_obj index es_row_no-row_id.
    else.
      read table tg_zglt036 into wg_zglt036 index es_row_no-row_id.
    endif.
    case e_fieldname.
      when 'VORNR'.
        select single *
            from caufv
            into wa_caufv
            where aufnr  = wg_obj-aufnr.
        select *
          from afvc
          into table tl_afvc
          where aufpl = wa_caufv-aufpl.

        check tl_afvc is not initial.

        wl_fieldname  = 'VORNR'.
        wl_tabname    = 'AFVC'.

        loop at tl_afvc into wl_afvc.
          wl_index = sy-tabix.
          move: wl_afvc-vornr  to wl_valuetab-field.
          append wl_valuetab   to tl_valuetab.
          clear:  wl_valuetab.

          move: wl_afvc-ltxa1  to wl_valuetab-field.
          append wl_valuetab   to tl_valuetab.
          clear:  wl_valuetab.

        endloop.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'VORNR'.
        wl_field-s = 'X'.
        append wl_field to tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'LTXA1'.
        wl_field-s = ' '.
        append wl_field to tl_field.

      when 'TAX_CODE'.
        select single *
          from t001
          into wl_t001
          where bukrs = wg_zglt035-bukrs.

        select single *
          from t005
          into wl_t005
          where land1 = wl_t001-land1.

        select *
          from t007s
          into corresponding fields of table tl_t007s
             where spras eq sy-langu
               and kalsm eq wl_t005-kalsm.

        check tl_t007s is not initial.

        select *
          from t007a
          into table tl_t007a
          for all entries in tl_t007s
          where  kalsm  = tl_t007s-kalsm
          and    mwskz  = tl_t007s-mwskz
          and    xinact =  ''.

        select *
        from t030k
          into table tl_t030k
        for all entries in tl_t007s
        where ktopl = '0050'
        and   mwskz = tl_t007s-mwskz.

        wl_fieldname  = 'MWSKZ'.
        wl_tabname    = 'T007S'.

        loop at tl_t007s into wl_t007s.
          wl_index = sy-tabix.
          read table tl_t030k into wl_t030k with key mwskz = wl_t007s-mwskz.
          if sy-subrc ne 0.
            delete tl_t007s index wl_index.
            continue.
          endif.

          read table tl_t007a into wl_t007a with key mwskz = wl_t007s-mwskz.
          if sy-subrc ne 0.
            delete tl_t007s index wl_index.
            continue.
          endif.

          move: wl_t007s-mwskz to wl_valuetab-field.
          append wl_valuetab   to tl_valuetab.

          move: wl_t007s-text1  to wl_valuetab-field.
          append wl_valuetab    to tl_valuetab.

          clear:  wl_valuetab.
        endloop.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'MWSKZ'.
        wl_field-s = 'X'.
        append wl_field to tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'TEXT1'.
        wl_field-s = ' '.
        append wl_field to tl_field.
      when 'UMSKZ'.
        select single * from tbsl
          into wl_tbsl
        where bschl eq wg_zglt036-bschl.

        check sy-subrc = 0.

        select * from t074u into table tl_t074u
          where koart = wl_tbsl-koart.

        check tl_t074u is not initial.

        select *
          from t074t
          into corresponding fields of table tl_t074t
          for all entries in tl_t074u
             where t074t~spras eq sy-langu
               and t074t~koart eq tl_t074u-koart
               and t074t~shbkz eq tl_t074u-umskz.

        check tl_t074t is not initial.

        wl_fieldname  = 'SHBKZ'.
        wl_tabname    = 'T074T'.

        loop at tl_t074t into wl_t074t.
          move: wl_t074t-shbkz to wl_valuetab-field.
          append wl_valuetab to tl_valuetab.

          move: wl_t074t-ltext to wl_valuetab-field.
          append wl_valuetab to tl_valuetab.

          clear: wl_kna1, wl_valuetab.
        endloop.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'SHBKZ'.
        wl_field-s = 'X'.
        append wl_field to tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'LTEXT'.
        wl_field-s = ' '.
        append wl_field to tl_field.
      when 'ZLSCH'.
        refresh: t_set, t_linha.
        select *
          from  setleaf
          into table t_set
          where setclass      = '0000'
          and   setname        = 'MAGGI_PGT_EUROPA'.

        if t_set[] is not initial.
          select *
            from setlinet
            into table t_linha
            for all entries in t_set
            where setclass   = t_set-setclass
            and subclass     = t_set-subclass
            and setname      = t_set-setname
            "AND LANGU        = 'P'
            and lineid       = t_set-lineid.
        endif.


        sort t_set by valfrom.

        wl_fieldname  = 'ZLSCH'.
        wl_tabname    = 'T042Z'.

        select single *
            from t001
            into wl_t001
            where bukrs = wg_zglt035-bukrs.

        select  * from t042z
          into table tl_t042z
           where land1 = wl_t001-land1.

        check tl_t042z is not initial.
        sort tl_t042z by zlsch.

        loop at tl_t042z into wl_t042z.


          concatenate wl_t001-land1 wl_t042z-zlsch into vl_forma separated by space.
          "busca tradução para ingles
          read table t_set into w_set with key valfrom = vl_forma.
          if sy-subrc = 0.
            read table t_linha into w_linha with key  setclass   = w_set-setclass
                                                      subclass   = w_set-subclass
                                                      setname    = w_set-setname
                                                      lineid     = w_set-lineid.
            if sy-subrc = 0.
              move: w_linha-descript to wl_valuetab-field.
            else.
              continue.
              "MOVE: WL_T042Z-TEXT1 TO WL_VALUETAB-FIELD.
            endif.
          else.
            move: wl_t042z-text1 to wl_valuetab-field.
          endif.
          append wl_valuetab to tl_valuetab.

          move: wl_t042z-zlsch to wl_valuetab-field.
          append wl_valuetab   to tl_valuetab.

          clear: wl_t042z, wl_valuetab.
        endloop.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'ZLSCH'.
        wl_field-s = 'X'.
        append wl_field to tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'TEXT1'.
        wl_field-s = ' '.
        append wl_field to tl_field.
      when 'BVTYP'.
        check wg_zglt036-bschl is not initial.

        clear: wl_kunnr,wl_lifnr,wl_bvtyp.

        select single * from tbsl
          into wl_tbsl
        where bschl eq wg_zglt036-bschl.

        check wl_tbsl is not initial.
        case wl_tbsl-koart.
          when 'K'.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = wg_zglt036-hkont
              importing
                output = wl_lifnr.

          when 'D'.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = wg_zglt036-hkont
              importing
                output = wl_kunnr.
        endcase.

        call function 'FI_F4_BVTYP'
          exporting
            i_kunnr = wl_kunnr
            i_lifnr = wl_lifnr
            i_xshow = space
          importing
            e_bvtyp = wl_bvtyp.

        if not wl_bvtyp is initial.
          wg_zglt036-bvtyp = wl_bvtyp.
          modify tg_zglt036 from wg_zglt036 index es_row_no-row_id.
        endif.

*** Método de atualização de dados na Tela
        call method grid1->refresh_table_display
          exporting
            is_stable = wa_stable.
        exit.
      when 'HKONT'.
        check wg_zglt036-bschl is not initial.

        select single * from tbsl
          into wl_tbsl
        where bschl eq wg_zglt036-bschl.

        check wl_tbsl is not initial.
        case wl_tbsl-koart.
          when 'K'.
            wl_fieldname  = 'LIFNR'.
            wl_tabname    = 'LFA1'.

            select *
              from lfa1
              inner join lfb1
              on lfb1~bukrs  = wg_zglt035-bukrs
              and lfb1~lifnr = lfa1~lifnr
              into corresponding fields of table tl_lfa1.

            check tl_lfa1 is not initial.
            sort tl_lfa1 by lifnr.

            loop at tl_lfa1 into wl_lfa1.
              move: wl_lfa1-lifnr to wl_valuetab-field.
              append wl_valuetab to tl_valuetab.

              move: wl_lfa1-name1 to wl_valuetab-field.
              append wl_valuetab to tl_valuetab.

              clear: wl_lfa1, wl_valuetab.
            endloop.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'LIFNR'.
            wl_field-s = 'X'.
            append wl_field to tl_field.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'NAME1'.
            wl_field-s = ' '.
            append wl_field to tl_field.
          when 'D'.
            wl_fieldname  = 'KUNNR'.
            wl_tabname    = 'KNA1'.

            select *
              from kna1
              inner join knb1
              on knb1~bukrs  = wg_zglt035-bukrs
              and knb1~kunnr = kna1~kunnr
              into corresponding fields of table tl_kna1.

            check tl_kna1 is not initial.
            sort tl_kna1 by kunnr.

            loop at tl_kna1 into wl_kna1.
              move: wl_kna1-kunnr to wl_valuetab-field.
              append wl_valuetab to tl_valuetab.

              move: wl_kna1-name1 to wl_valuetab-field.
              append wl_valuetab to tl_valuetab.

              clear: wl_kna1, wl_valuetab.
            endloop.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'KUNNR'.
            wl_field-s = 'X'.
            append wl_field to tl_field.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'NAME1'.
            wl_field-s = ' '.
            append wl_field to tl_field.
          when 'S'.
            wl_fieldname  = 'SAKNR'.
            wl_tabname    = 'SKAT'.

            select * from skat into table tl_skat where spras eq sy-langu and ktopl eq '0050'.

            check tl_skat is not initial.
            sort tl_skat by saknr.

            loop at tl_skat into wl_skat.
              move: wl_skat-saknr to wl_valuetab-field.
              append wl_valuetab to tl_valuetab.

              move: wl_skat-txt50 to wl_valuetab-field.
              append wl_valuetab to tl_valuetab.

              clear: wl_skat, wl_valuetab.
            endloop.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'SAKNR'.
            wl_field-s = 'X'.
            append wl_field to tl_field.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'TXT50'.
            wl_field-s = ' '.
            append wl_field to tl_field.
          when 'A'.
            wl_fieldname  = 'ANLN1'.
            wl_tabname    = 'ANLA'.

            select * from anla into table tl_anla.

            check tl_anla is not initial.
            sort tl_anla by anln1.

            loop at tl_anla into wl_anla.
              move: wl_anla-anln1 to wl_valuetab-field.
              append wl_valuetab to tl_valuetab.

              move: wl_anla-mcoa1 to wl_valuetab-field.
              append wl_valuetab to tl_valuetab.

              clear: wl_anla, wl_valuetab.
            endloop.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'ANLN1'.
            wl_field-s = 'X'.
            append wl_field to tl_field.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'MCOA1'.
            wl_field-s = ' '.
            append wl_field to tl_field.
        endcase.
      when 'DIVISAO'.
        check wg_zglt035-bukrs is not initial.
        " Divisões de empresas
        call function 'G_SET_GET_ALL_VALUES'
          exporting
            class         = '0000'
            setnr         = 'MAGGI_ZGL0016_DIV'
          tables
            set_values    = t_divisao
          exceptions
            set_not_found = 1
            others        = 2.
        if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        endif.
        sort t_divisao by from.

        clear: w_div1, w_div2,wl_tgsb.
        refresh tl_tgsb.
        loop at t_divisao into w_divisao.
          if w_divisao-from+0(4) = wg_zglt035-bukrs.
            w_div1 = 'X'.
            wl_tgsb-gsber = w_divisao-from+5(4).
            append wl_tgsb to tl_tgsb.
          endif.
        endloop.

        select single *
            from t001
            into wl_t001
            where bukrs = wg_zglt035-bukrs.

        if wl_t001-land1 = 'BR' and  w_div1 ne 'X'.
          wl_fieldname  = 'BRANCH'.
          wl_tabname    = 'J_1BBRANCH'.
          select  * from j_1bbranch
            into table tl_j_1bbranch
          where bukrs eq wg_zglt035-bukrs.

          check tl_j_1bbranch is not initial.
          sort tl_j_1bbranch by branch.

          loop at tl_j_1bbranch into wl_j_1bbranch.
            move: wl_j_1bbranch-branch to wl_valuetab-field.
            append wl_valuetab to tl_valuetab.

            move: wl_j_1bbranch-name to wl_valuetab-field.
            append wl_valuetab to tl_valuetab.

            clear: wl_j_1bbranch, wl_valuetab.
          endloop.

          wl_field-tabname = wl_tabname.
          wl_field-fieldname = 'BRANCH'.
          wl_field-s = 'X'.
          append wl_field to tl_field.

          wl_field-tabname = wl_tabname.
          wl_field-fieldname = 'NAME'.
          wl_field-s = ' '.
          append wl_field to tl_field.
        else.
          wl_fieldname  = 'GSBER'.
          wl_tabname    = 'TGSBT'.
          if w_div1 ne 'X'.
            select  * from tgsbt
              into table tl_tgsbt
            where spras = sy-langu.
          else.
            select  * from tgsbt
              into table tl_tgsbt
             for all entries in tl_tgsb
            where spras = sy-langu
            and   gsber = tl_tgsb-gsber.
          endif.

          check tl_tgsbt is not initial.
          sort tl_tgsbt by gsber.

          loop at tl_tgsbt into wl_tgsbt.
            move: wl_tgsbt-gsber to wl_valuetab-field.
            append wl_valuetab to tl_valuetab.

            move: wl_tgsbt-gtext to wl_valuetab-field.
            append wl_valuetab to tl_valuetab.

            clear: wl_tgsbt, wl_valuetab.
          endloop.

          wl_field-tabname = wl_tabname.
          wl_field-fieldname = 'GSBER'.
          wl_field-s = 'X'.
          append wl_field to tl_field.

          wl_field-tabname = wl_tabname.
          wl_field-fieldname = 'GTEXT'.
          wl_field-s = ' '.
          append wl_field to tl_field.
        endif.
      when 'HBKID'.
        check wg_zglt035-bukrs is not initial.
        wl_fieldname  = 'HBKID'.
        wl_tabname    = 'T012T'.

        select  hbkid text1
          from t012t
          into table tl_t012
        where bukrs eq wg_zglt035-bukrs
        and   spras eq sy-langu.

        check tl_t012 is not initial.
        sort tl_t012 by hbkid.

        loop at tl_t012 into wl_t012.
          move: wl_t012-hbkid to wl_valuetab-field.
          append wl_valuetab to tl_valuetab.

          move: wl_t012-text1 to wl_valuetab-field.
          append wl_valuetab to tl_valuetab.

          clear: wl_valuetab.
        endloop.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'HBKID'.
        wl_field-s = 'X'.
        append wl_field to tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'TEXT1'.
        wl_field-s = ' '.
        append wl_field to tl_field.

    endcase.

    if    wl_fieldname  is not initial
      and wl_tabname    is not initial
      and tl_field[]    is not initial
      and tl_valuetab[] is not initial.
      clear  wl_index.
      call function 'HELP_VALUES_GET_WITH_TABLE_EXT'
        exporting
*         cucol                     = '3'
          fieldname                 = wl_fieldname
          tabname                   = wl_tabname
        importing
          index                     = wl_index
          select_value              = wl_char
        tables
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_valuetab
        exceptions
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      if sy-subrc is initial and wl_index gt 0.
        case e_fieldname.
          when 'VORNR'.
            read table tl_afvc into wl_afvc index wl_index.
          when 'TAX_CODE'.
            read table tl_t007s into wl_t007s index wl_index.
          when 'UMSKZ'.
            read table tl_t074t into wl_t074t index wl_index.
          when 'HKONT'.
            case wl_tbsl-koart.
              when 'K'.
                read table tl_lfa1 into wl_lfa1 index wl_index.
              when 'D'.
                read table tl_kna1 into wl_kna1 index wl_index.
              when 'S'.
                read table tl_skat into wl_skat index wl_index.
              when 'A'.
                read table tl_anla into wl_anla index wl_index.
            endcase.
          when 'DIVISAO'.
            if wl_t001-land1 = 'BR' and w_div1 is initial.
              read table tl_j_1bbranch into wl_j_1bbranch index wl_index.
            else.
              read table tl_tgsbt into wl_tgsbt index wl_index.
            endif.
          when 'ZLSCH'.
            read table tl_t042z into wl_t042z index wl_index.
          when 'HBKID'.
            read table tl_t012   into wl_t012 index wl_index.
        endcase.

        if es_row_no-row_id gt 0.
          if  e_fieldname = 'VORNR'.
            read table tl_obj_aux into wg_obj  index es_row_no-row_id.
          else.
            read table tg_zglt036 into wg_zglt036 index es_row_no-row_id.
          endif.
          if sy-subrc is initial.
            if  e_fieldname = 'VORNR'.

            else.
              assign wg_zglt036-style  to <stylet>.
              if sy-subrc eq 0.
                read table <stylet> with key fieldname = e_fieldname assigning <stylel>.
                if sy-subrc = 0.
                  exit.
                endif.
              endif.
            endif.

            case e_fieldname.
              when 'VORNR'.
                move: wl_afvc-vornr to wg_obj-vornr.
              when 'TAX_CODE'.
                move: wl_t007s-mwskz to wg_zglt036-tax_code.
              when 'UMSKZ'.
                move: wl_t074t-shbkz to wg_zglt036-umskz.
              when 'HKONT'.
                case wl_tbsl-koart.
                  when 'K'.
                    move: wl_lfa1-lifnr to wg_zglt036-hkont,
                          wl_lfa1-name1 to wg_zglt036-descr.
                    select single akont from lfb1 into wl_akont         where lifnr = wg_zglt036-hkont  and bukrs = wg_zglt035-bukrs.
                    check sy-subrc = 0.
                    wg_zglt036-akont = wl_akont.

                    select single * from lfa1 into wl_lfa1 where lifnr eq wg_zglt036-hkont.
                    check sy-subrc = 0.
                    move wl_lfa1-vbund to wg_zglt036-vbund.

                    if wg_zglt036-umskz ne ''.
                      select single skont from t074 into wl_hkont
                        where ktopl = '0050'
                        and   koart = 'K'
                        and   umskz = wg_zglt036-umskz
                        and   hkont = wg_zglt036-akont.
                      wg_zglt036-akont = wl_hkont.
                    endif.
                    select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                                         and spras eq sy-langu
                                                                         and ktopl eq '0050'.
                  when 'D'.
                    move: wl_kna1-kunnr to wg_zglt036-hkont,
                          wl_kna1-name1 to wg_zglt036-descr.

                    select single * from kna1 into wl_kna1 where kunnr eq wg_zglt036-hkont.
                    check sy-subrc = 0.

                    select single akont from knb1 into wl_akont         where kunnr = wg_zglt036-hkont  and bukrs = wg_zglt035-bukrs.
                    wg_zglt036-akont = wl_akont.

                    select single * from kna1 into wl_kna1 where lifnr eq wg_zglt036-hkont.
                    check sy-subrc = 0.
                    move wl_kna1-vbund to wg_zglt036-vbund.

                    if wg_zglt036-umskz ne ''.
                      select single skont from t074 into wl_hkont
                        where ktopl = '0050'
                        and   koart = 'D'
                        and   umskz = wg_zglt036-umskz
                        and   hkont = wl_akont.
                      wg_zglt036-akont = wl_hkont.
                    endif.
                    select single txt50 from skat into wg_zglt036-descr_a where saknr eq wg_zglt036-akont
                                                                         and spras eq sy-langu
                                                                         and ktopl eq '0050'.

                    move: wl_kna1-name1 to wg_zglt036-descr.
                  when 'S'.
                    move: wl_skat-saknr to wg_zglt036-hkont,
                          wl_skat-txt50 to wg_zglt036-descr.

                    move: wl_skat-saknr to wg_zglt036-akont,
                          wl_skat-txt50 to wg_zglt036-descr_a.
                  when 'A'.
                    move: wl_anla-mcoa1 to wg_zglt036-descr.

                    concatenate wl_anla-anln1 '-' wl_anla-anln2 into wg_zglt036-hkont.

                    wg_zglt036-akont = wg_zglt036-hkont.
                    select single * from anla into wl_anla where bukrs eq wg_zglt035-bukrs
                                                           and   anln1 eq wl_anla-anln1
                                                           and   anln2 eq wl_anla-anln2.
                    move: wl_anla-mcoa1 to wg_zglt036-descr_a.
                endcase.
                select single *
                      from tka02
                      into wl_tka02
                      where bukrs  = wg_zglt035-bukrs.

                select single *        "#EC CI_DB_OPERATION_OK[2389136]
                    from cskb
                    into wl_cskb
                    where  kokrs  = wl_tka02-kokrs
                    and    kstar  = wg_zglt036-hkont
                    and    datab  le sy-datum
                    and    datbi  ge sy-datum.

                if sy-subrc = 0 and wl_tbsl-koart  = 'S'.
                  move 'X' to wg_zglt036-xclasse.
                  wg_zglt036-icon =  '@1F@'.
                else.
                  clear: wg_zglt036-icon, wg_zglt036-xclasse.
                endif.
              when 'DIVISAO'.
                if wl_t001-land1 = 'BR' and w_div1 is initial.
                  move: wl_j_1bbranch-branch to wg_zglt036-divisao.
                else.
                  move: wl_tgsbt-gsber to wg_zglt036-divisao.
                endif.
              when 'HBKID'.
                move: wl_t012-hbkid to wg_zglt036-hbkid.
              when 'ZLSCH'.
                move: wl_t042z-zlsch to wg_zglt036-zlsch.
            endcase.
            if  e_fieldname = 'VORNR'.
              modify tl_obj_aux from wg_obj index es_row_no-row_id.
            else.
              modify tg_zglt036 from wg_zglt036 index es_row_no-row_id.
            endif.
          endif.
        endif.
      endif.
    endif.

    clear: wg_zglt036.

**** Método de atualização de dados na Tela
    if  e_fieldname = 'VORNR'.
      call method grid5->refresh_table_display
        exporting
          is_stable = wa_stable.
    else.
      call method grid1->refresh_table_display
        exporting
          is_stable = wa_stable.
    endif.




  endmethod.                    "ON_ONF4

endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION
