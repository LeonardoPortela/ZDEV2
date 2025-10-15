*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 04/06/2019                                              &*
*& Descrição: Solicitação de Pedidos de Compra                        &*
*& Transação: ZMM0149                                                 &*
*---------------------------------------------------------------------&*

REPORT  zmmr149n.

INCLUDE <cl_alv_control>."

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

TYPES: BEGIN OF ty_cadlan,
         nro_sol_cp   TYPE zmmt0035-nro_sol_cp,
         safra        TYPE zmmt0035-safra,
         bstyp        TYPE zmmt0035-bstyp,
         bsart        TYPE zmmt0035-bsart,
         batxt        TYPE t161t-batxt,
         ekgrp        TYPE zmmt0035-ekgrp,
         eknam        TYPE t024-eknam,
         werks        TYPE t001w-werks,
         name1_w      TYPE t001w-name1,
         lifnr        TYPE lfa1-lifnr,
         name1_l      TYPE lfa1-name1,
         ped_forn     TYPE zmmt0035-ped_forn,
         waers        TYPE zmmt0035-waers,
         wkurs        TYPE zmmt0035-wkurs,
         zterm        TYPE t052-zterm,
         text1        TYPE t052u-text1,
         ebeln        TYPE zmmt0035-ebeln,
         ebeln_son    TYPE zmmt0035-ebeln,
         banfn        TYPE zmmt0035-banfn,
         ihran        TYPE zmmt0035-ihran,
         texto_neg    TYPE zmmt0035-texto_neg,
         lifnr_n      TYPE lfa1-lifnr,
         name1_n      TYPE lfa1-name1,
         inco1        TYPE zmmt0035-inco1,
         inco2        TYPE zmmt0035-inco2,
         data_criacao TYPE zmmt0035-data_criacao,
       END OF ty_cadlan,

       BEGIN OF ty_coplan,
         nro_sol_cp TYPE zmmt0035-nro_sol_cp,
         safra      TYPE zmmt0035-safra,
         ekgrp      TYPE zmmt0035-ekgrp,
         werks      TYPE zmmt0035-werks,
         lifnr      TYPE zmmt0035-lifnr,
         waers      TYPE zmmt0035-waers,
         wkurs      TYPE zmmt0035-wkurs,
         zterm      TYPE zmmt0035-zterm,
         ped_forn   TYPE zmmt0035-ped_forn,
         bsart      TYPE zmmt0035-bsart,
         texto_neg  TYPE zmmt0035-texto_neg,
         ihran      TYPE zmmt0035-ihran,
         inco1      TYPE zmmt0035-inco1,
         inco2      TYPE zmmt0035-inco2,
       END OF ty_coplan,

       BEGIN OF ty_ekbe,
         ebeln TYPE ekbe-ebeln,
         ebelp TYPE ekbe-ebelp,
         menge TYPE ekbe-menge,
         dmbtr TYPE ekbe-dmbtr,
         shkzg TYPE ekbe-shkzg,
       END OF ty_ekbe,

       BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END   OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor.

TYPES BEGIN OF ty_produto.
TYPES: mark(1),
       loekz(4),
       ebeln       TYPE zmmt0037-ebeln,
       ebelp       TYPE zmmt0037-ebelp,
       werks       TYPE zmmt0037-werks,
       matnr       TYPE makt-matnr,
       maktx       TYPE makt-maktx,
       lgort       TYPE zmmt0037-lgort,
       charg       TYPE zmmt0037-charg,
       menge       TYPE zmmt0037-menge,
       menge_tro   TYPE zmmt0037-menge,
       menge_alt   TYPE zmmt0037-menge,
       menge_migo  TYPE zmmt0037-menge,
       menge_ori   TYPE zmmt0037-menge,
       meins       TYPE zmmt0037-meins,
       valor       TYPE zmmt0037-netpr,
       brtwr       TYPE zmmt0037-brtwr,
       bicms       TYPE zmmt0037-bicms,
       picms       TYPE zmmt0037-picms,
       netpr       TYPE zmmt0037-netpr,
       netpr_roya  TYPE zmmt0037-netpr_roya,
       netpr_germ  TYPE zmmt0037-netpr_germ,
       netpr_desc  TYPE zmmt0037-netpr_desc,
       netpr_supl  TYPE zmmt0037-netpr_supl,
       netpr_frete TYPE zmmt0037-netpr_frete,
       netpr_final TYPE zmmt0037-netpr,
       netpr_orig  TYPE zmmt0037-netpr,
       wmwst       TYPE komp-netwr,
       peinh       TYPE zmmt0037-peinh,
       bprme       TYPE zmmt0037-bprme,
       mwskz       TYPE zmmt0037-mwskz,
       qtde_troca  TYPE zmmt0037-netpr,
       vlr_troca   TYPE zmmt0037-netpr,
       bsart       TYPE ekko-bsart,
       style       TYPE lvc_t_styl,
       change(1)   TYPE c.                                  "BUG 52329
TYPES END OF ty_produto.

TYPES BEGIN OF ty_cop_item.
TYPES: mark(1),
       nro_sol_cp TYPE zmmt0037-nro_sol_cp,
       matnr      TYPE zmmt0037-matnr,
       maktx      TYPE makt-maktx,
       lgort      TYPE zmmt0037-lgort,
       charg      TYPE zmmt0037-charg,
       menge      TYPE zmmt0037-menge,
       meins      TYPE zmmt0037-meins,
       brtwr      TYPE zmmt0037-brtwr,
       netpr_roya TYPE zmmt0037-netpr_roya,
       netpr_germ TYPE zmmt0037-netpr_germ.
TYPES      END OF ty_cop_item.

TYPES BEGIN OF ty_zmmt0037_aux.
TYPES: nro_sol_cp TYPE zmmt0037-nro_sol_cp,
       ebeln      TYPE zmmt0037-ebeln,
       ebelp      TYPE zmmt0037-ebelp,
       "matnr      TYPE zmmt0037-matnr,
       menge      TYPE zmmt0037-menge,
       netpr      TYPE zmmt0037-netpr,
       xvlrpedido TYPE zmmt0037-menge.
TYPES      END OF ty_zmmt0037_aux.

TYPES BEGIN OF ty_zmmt0035_aux.
TYPES: nro_sol_cp TYPE zmmt0035-nro_sol_cp,
       ebeln      TYPE zmmt0035-ebeln.
"ebelp      TYPE zmmt0035-ebelp.
TYPES      END OF ty_zmmt0035_aux.

TYPES BEGIN OF ty_zmmt0147.
TYPES: nro_sol_cp TYPE zmmt0035-nro_sol_cp,
       ebeln      TYPE zmmt0035-ebeln,
       ebelp      TYPE zmmt0037-ebelp,
       xsdoadto   TYPE zmmt0147-sdo_adto,
       vlr_adto   TYPE zmmt0147-vlr_adto,
       dt_vcto    TYPE zmmt0147-dt_vcto,
       perc_adto  TYPE zmmt0147-perc_adto,
       dt_atual   TYPE zmmt0147-dt_atual,
       hr_atual   TYPE zmmt0147-hr_atual,
       usnam      TYPE zmmt0147-usnam,
       dep_resp   TYPE zmmt0147-dep_resp,
       resp_neg   TYPE zmmt0147-resp_neg.
TYPES      END OF ty_zmmt0147.

TYPES BEGIN OF ty_zmmt0147_aux.
TYPES: nro_sol_cp TYPE zmmt0035-nro_sol_cp,
       ebeln      TYPE zmmt0035-ebeln,
       ebelp      TYPE zmmt0037-ebelp,
       xtotadto   TYPE zmmt0147-vlr_adto.
TYPES  END OF ty_zmmt0147_aux.

TYPES BEGIN OF ty_ekbe_pf.
TYPES: belnr TYPE ekbe-belnr,
       gjahr TYPE ekbe-gjahr,
       awkey TYPE bkpf-awkey,
       bukrs TYPE ekko-bukrs,
       ebeln TYPE ekko-ebeln,
       END OF ty_ekbe_pf.


TYPES BEGIN OF ty_pos_finan.
TYPES:ebeln	     TYPE bsak-ebeln,
      xblnr      TYPE bsak-xblnr,
      belnr_m    TYPE ekbe-belnr,
      belnr_c    TYPE bkpf-belnr,
      blart      TYPE bsak-blart,
      budat      TYPE bsak-budat,
      augbl      TYPE bsak-augbl,
      augdt      TYPE bsak-augdt,
      dmbtr      TYPE bsak-dmbtr,
      dmbe2      TYPE bsak-dmbe2,
      status(40),
      END OF ty_pos_finan.

TYPES BEGIN OF ty_status_adiant.
TYPES:
  mark(1),
  icon(4),
  ebeln     TYPE zmmt0037-ebeln,
  ebelp     TYPE zmmt0037-ebelp,
  saldo     TYPE zmmt0037-menge,
  perc_util TYPE zmmt0147-perc_adto,
  perc_adto TYPE zmmt0147-perc_adto,
  vlr_adto  TYPE zmmt0147-vlr_adto,
  dt_vcto   TYPE zmmt0147-dt_vcto,
  nro_sol   TYPE zfit0046-nro_sol,
  status    TYPE zfit0045-status,
  belnr     TYPE zfit0045-belnr,
  augbl     TYPE bsak-augbl,
  dep_resp  TYPE zfit0045-dep_resp,
  resp_neg  TYPE zfit0045-resp_neg,
  valor     TYPE netpr,
  style     TYPE lvc_t_styl,
  END OF ty_status_adiant.


*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: ok-code        TYPE sy-ucomm,
      wg_cadlan      TYPE ty_cadlan,
      wg_cadlan_mem  TYPE ty_cadlan,
      wg_cadlan_old  TYPE ty_cadlan,
      wg_coplan      TYPE ty_coplan,
      tl_index_rows  TYPE lvc_t_row,
      wl_index_rows  TYPE lvc_s_row,
      w_valor        TYPE komp-netwr,
      w_valor2       TYPE komp-netwr,
      w_wmwst        TYPE komp-netwr,
      w_wmwst2       TYPE komp-netwr,
      p_tipo_oper(1),
      p_erro_oper(1),
      x_field(30),
      vtexto(50),


      BEGIN OF tg_fatura OCCURS 0,
        mark(1),
        dt_vcto    TYPE zmmt0036-dt_vcto,
        percentual TYPE zmmt0036-percentual,
        valor      TYPE zmmt0036-valor,
        menge      TYPE zmmt0036-menge,
        nro_sol_cp TYPE zmmt0147-nro_sol_cp,
      END OF tg_fatura,

      tg_produto      TYPE TABLE OF ty_produto  WITH HEADER LINE,
      tg_produto_aux  TYPE TABLE OF ty_produto  WITH HEADER LINE,
      tg_produto2     TYPE TABLE OF ty_produto  WITH HEADER LINE,
      tg_produto3     TYPE TABLE OF ty_produto  WITH HEADER LINE,
      tg_produto_old  TYPE TABLE OF ty_produto  WITH HEADER LINE,
      tg_cop_item     TYPE TABLE OF ty_cop_item WITH HEADER LINE,
      tg_zmmt0037_aux TYPE TABLE OF ty_zmmt0037_aux WITH HEADER LINE,
      tg_zmmt0035_aux TYPE TABLE OF ty_zmmt0035_aux WITH HEADER LINE,
      tg_zmmt0147     TYPE TABLE OF ty_zmmt0147 WITH HEADER LINE,
      t_zmmt0147      TYPE TABLE OF zmmt0147 WITH HEADER LINE,
      tg_zmmt0147_aux TYPE TABLE OF ty_zmmt0147_aux WITH HEADER LINE,
      tg_pos_finan    TYPE TABLE OF ty_pos_finan WITH HEADER LINE,
      w_zmmt0147      TYPE ty_zmmt0147,


      BEGIN OF tg_produto_sel OCCURS 0,
        mark(1),
        loekz         TYPE ekpo-loekz,
        ebeln         TYPE zmmt0037-ebeln,
        ebelp         TYPE zmmt0037-ebelp,
        ebeln_ori     TYPE zmmt0037-ebeln,
        ebelp_ori     TYPE zmmt0037-ebelp,
        matnr         TYPE makt-matnr,
        maktx         TYPE makt-maktx,
        lgort         TYPE zmmt0037-lgort,
        charg         TYPE zmmt0037-charg,
        menge         TYPE zmmt0037-menge,
        meins         TYPE zmmt0037-meins,
        valor         TYPE zmmt0037-netpr,
        netpr         TYPE zmmt0037-netpr,
        netpr_roya    TYPE zmmt0037-netpr_roya,
        netpr_germ    TYPE zmmt0037-netpr_germ,
        netpr_desc    TYPE zmmt0037-netpr_desc,
        netpr_supl    TYPE zmmt0037-netpr_supl,
        netpr_desc1   TYPE zmmt0037-netpr_desc,
        netpr_frete   TYPE zmmt0037-netpr_frete,
        netpr_final   TYPE zmmt0037-netpr,
        netpr_orig    TYPE zmmt0037-netpr,
        peinh         TYPE zmmt0037-peinh,
        bprme         TYPE zmmt0037-bprme,
        mwskz         TYPE zmmt0037-mwskz,
        werks         TYPE zmmt0037-werks,
        wmwst         TYPE komp-netwr,
        id_lote_frete TYPE zde_id_lote_frete,
        parvw1        TYPE tpar-parvw,
        lifnr1        TYPE lfa1-lifnr,
        parvw2        TYPE tpar-parvw,
        lifnr2        TYPE lfa1-lifnr,
        netpr_tot     TYPE zmmt0037-netpr_supl,             "BUG 63565
        situ(1),
        style         TYPE lvc_t_styl,
      END OF tg_produto_sel,

      BEGIN OF tg_programa OCCURS 0,
        mark(1),
        ebelp      TYPE zmmt0038-ebelp,
        matnr      TYPE makt-matnr,
        maktx      TYPE makt-maktx,
        data_progr TYPE zmmt0038-data_progr,
        menge      TYPE zmmt0038-menge,
      END OF tg_programa,

      BEGIN OF tg_parceiro OCCURS 0,
        parvw TYPE tpart-parvw,
        vtext TYPE tpart-vtext,
        lifnr TYPE lfa1-lifnr,
        name1 TYPE lfa1-name1,
      END OF tg_parceiro,

      BEGIN OF tg_adto OCCURS 0,
        mark(1),
        icon(4),
        ebeln     TYPE zmmt0037-ebeln,
        ebelp     TYPE zmmt0037-ebelp,
        saldo     TYPE zmmt0147-vlr_adto,
        perc_util TYPE zmmt0147-perc_adto,
        perc_adto TYPE zmmt0147-perc_adto,
        vlr_adto  TYPE zmmt0147-vlr_adto,
        dt_vcto   TYPE zmmt0147-dt_vcto,
        nro_sol   TYPE zfit0046-nro_sol,
        status    TYPE zfit0045-status,
        belnr     TYPE zfit0045-belnr,
        augbl     TYPE bsak-augbl,
        dep_resp  TYPE zfit0045-dep_resp,
        resp_neg  TYPE zfit0045-resp_neg,
        valor     TYPE netpr,
        style     TYPE lvc_t_styl,
      END OF tg_adto.

DATA: tg_produto_par     LIKE TABLE OF tg_produto_sel,
      tg_produto_sel_old LIKE TABLE OF tg_produto_sel,
      t_zmmt0035_log     TYPE TABLE OF zmmt0035_log,
      t_zmmt0037_log     TYPE TABLE OF zmmt0037_log,
      wa_produto_par     LIKE tg_produto_sel,
      wa_produto_sel     LIKE tg_produto_sel,
      zebeln             TYPE ebeln,
      zebelp             TYPE ebelp.

** Criação de tabela dinamica
DATA: t_fieldcatalog       TYPE lvc_t_fcat,
      w_fieldcatalog       TYPE lvc_s_fcat,
      wa_layout            TYPE lvc_s_layo,
      gt_exc_button        TYPE ui_functions,
      gt_f4                TYPE lvc_t_f4 WITH HEADER LINE,
      gt_f4_adto           TYPE lvc_t_f4,
      gw_f4_adto           TYPE lvc_s_f4,
      wa_stable            TYPE lvc_s_stbl,
      wg_editor            TYPE ty_editor,
      wg_adto              LIKE LINE OF tg_adto,

      tg_fields            TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_editor            TYPE TABLE OF ty_editor,
      tg_msg_ret           TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      w_msg_ret            TYPE zfiwrs0002,
      tg_status_adiant     TYPE TABLE OF ty_status_adiant WITH HEADER LINE,
      tg_status_adiant_aux TYPE TABLE OF ty_status_adiant WITH HEADER LINE.

*** Criação de BDC
DATA:
  t_msg      TYPE TABLE OF bdcmsgcoll,   " Collecting Error messages
  w_msg      TYPE bdcmsgcoll,
  w_msg1(51),
  w_mode     TYPE c,
  t_bdcdata  LIKE TABLE OF bdcdata,
  fs_bdcdata LIKE LINE OF t_bdcdata.

DATA: v_orig_pgm    TYPE sy-repid,
      vg_chamada(1).

DATA: lt_exclude TYPE slis_t_extab,
      ls_exclude TYPE slis_extab.

DATA: g_exit_caused_by_caller,
      gs_exit_caused_by_user  TYPE slis_exit_by_user,
      g_repid                 LIKE sy-repid.


DATA: lt_lines  TYPE TABLE OF tline,
      lst_lines LIKE LINE OF  lt_lines,
      wl_header TYPE thead.

DATA: it_fcat         TYPE TABLE OF lvc_s_fcat,
      obj_custom_0300 TYPE REF TO cl_gui_custom_container,
      obj_alv_0300    TYPE REF TO cl_gui_alv_grid,
      vl_resposta     TYPE c,
      tl_function     TYPE ui_functions.


* types
TYPES:
  t_fieldcat TYPE slis_fieldcat_alv,
  t_events   TYPE slis_alv_event,
  t_layout   TYPE slis_layout_alv.
* Workareas
DATA:
  w_fieldcat TYPE t_fieldcat,
  w_events   TYPE t_events,
  w_layout   TYPE t_layout.



* Internal Tables
DATA:
  i_fieldcat TYPE STANDARD TABLE OF t_fieldcat,
  i_events   TYPE STANDARD TABLE OF t_events.

DATA: it_texto TYPE STANDARD TABLE OF tline,
      wa_texto TYPE tline,
      tl_texto TYPE catsxt_longtext_itab,
      wl_texto TYPE LINE OF catsxt_longtext_itab,
      wl_name  TYPE thead-tdname,
      wa_style TYPE lvc_s_styl,
      style    TYPE lvc_t_styl WITH HEADER LINE,
      vg_netpr TYPE zmmt0037-netpr,
      l_bsart  TYPE ekko-bsart.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF c_tab_strip_imp,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC2',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC1',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC3',
             tab4 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC4',
             tab5 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC5',
           END OF c_tab_strip_imp.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
CONTROLS:  tab_strip_imp TYPE TABSTRIP.
DATA: BEGIN OF g_tab_strip_imp,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZMMR149',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

DATA: ok_code         LIKE sy-ucomm,
      wg_mensagem(30),
      e_db_click      TYPE  zfiwrs0002,
      gs_variant_c    TYPE disvariant,
      wg_acao(30),
      xmodif(1),
      owner           TYPE soud-usrnam,
      sofd_dat        LIKE sofdd.

*** Anexos:
DATA: t_anexos     TYPE TABLE OF bdn_con,
      l_obj_key    TYPE sibflporb-instid,
      l_lines      TYPE i,
      anexo_obj    TYPE REF TO cl_gos_manager,
      l_ip_mode    TYPE sgs_rwmod,
      l_ip_service TYPE sgs_srvnam,
      w_bor        TYPE borident.

DATA: vg_datatime TYPE sdok_crtst.

*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.
*            lcl_alv_toolbar2  definition deferred.
*            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container_fatura        TYPE scrfname VALUE 'CC_FAT_IMP',
      g_custom_cont_fat         TYPE REF TO cl_gui_custom_container,

      g_container_produto       TYPE scrfname VALUE 'CC_PROD_IMP',
      g_container_produto2      TYPE scrfname VALUE 'CC_PROD_ORI',
      g_custom_cont_prod        TYPE REF TO cl_gui_custom_container,
      g_custom_cont_prod2       TYPE REF TO cl_gui_custom_container,

      g_container_cop_item      TYPE scrfname VALUE 'CC_COP_ITEM',
      g_custom_cop_item         TYPE REF TO cl_gui_custom_container,

      g_container_produtos      TYPE scrfname VALUE 'CC_PROD_SEL',
      g_custom_cont_prods       TYPE REF TO cl_gui_custom_container,

      g_container_status_adiant TYPE scrfname VALUE 'CONTAINER_STATUS_ADIANT',
      g_custom_status_adiant    TYPE REF TO cl_gui_custom_container,

      g_container_programa      TYPE scrfname VALUE 'CC_PROG_IMP',
      g_custom_cont_prog        TYPE REF TO cl_gui_custom_container,

      g_container_parceiro      TYPE scrfname VALUE 'CC_PARC',
      g_custom_cont_parc        TYPE REF TO cl_gui_custom_container,

      g_container_adt           TYPE scrfname VALUE 'CC_ADT',
      g_custom_cont_adt         TYPE REF TO cl_gui_custom_container,

      g_container_pos_finan     TYPE scrfname VALUE 'CC_POS_FIN',
      g_custom_cont_pos_finan   TYPE REF TO cl_gui_custom_container,

      obg_conteiner_err         TYPE REF TO cl_gui_custom_container,
      container_1               TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2               TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter                  TYPE REF TO cl_gui_splitter_container,
      grid1                     TYPE REF TO cl_gui_alv_grid,
      grid2                     TYPE REF TO cl_gui_alv_grid,
      grid3                     TYPE REF TO cl_gui_alv_grid,
      grid4                     TYPE REF TO cl_gui_alv_grid,
      grid5                     TYPE REF TO cl_gui_alv_grid,
      grid6                     TYPE REF TO cl_gui_alv_grid,
      grid7                     TYPE REF TO cl_gui_alv_grid,
      grid8                     TYPE REF TO cl_gui_alv_grid,
      grid9                     TYPE REF TO cl_gui_alv_grid,
      grid10                    TYPE REF TO cl_gui_alv_grid,
      obg_toolbar               TYPE REF TO lcl_alv_toolbar,
      obg_toolbar2              TYPE REF TO lcl_alv_toolbar,
      obg_toolbar3              TYPE REF TO lcl_alv_toolbar,
      obg_toolbar4              TYPE REF TO lcl_alv_toolbar,
      obg_toolbar8              TYPE REF TO lcl_alv_toolbar,
      obg_toolbar10             TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager      TYPE REF TO cl_alv_grid_toolbar_manager,
      g_descbox                 TYPE scrfname VALUE 'CC_DESC',
      g_cc_err                  TYPE scrfname VALUE 'CC_ERR',
      obg_descbox               TYPE REF TO cl_gui_textedit,
      g_custom_cont_desc        TYPE REF TO cl_gui_custom_container,
      obg_docking               TYPE REF TO cl_gui_docking_container,

      style2                    TYPE lvc_t_styl WITH HEADER LINE.

* alrs
*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.
*** TREE DE MENSAGENS.
DATA node_itab LIKE node_str OCCURS 0.
DATA node LIKE node_str.

DATA container TYPE REF TO cl_gui_custom_container.
DATA splitter_msg TYPE REF TO cl_gui_easy_splitter_container.
DATA right TYPE REF TO cl_gui_container.
DATA left  TYPE REF TO cl_gui_container.

DATA editor TYPE REF TO cl_gui_textedit.
DATA tree TYPE REF TO cl_gui_simple_tree.

DATA behaviour_left TYPE REF TO cl_dragdrop.
DATA behaviour_right TYPE REF TO cl_dragdrop.

DATA handle_tree TYPE i.
DATA num_row TYPE i VALUE 0.

DATA znetpr_pai TYPE netpr.
DATA znetpr_filho TYPE netpr.
DATA zvalor_produto TYPE netpr.
DATA zvalor_adto TYPE netpr.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_0               TYPE c VALUE '0',
           c_1               TYPE c VALUE '1',
           c_2               TYPE c VALUE '2',
           c_b               TYPE c VALUE 'B',
           c_s               TYPE c VALUE 'S',
           c_l               TYPE c VALUE 'L',
           c_x               TYPE c VALUE 'X',
           c_d               TYPE c VALUE 'D',
           c_k               TYPE c VALUE 'K',
           c_w               TYPE c VALUE 'W',
           c_f               TYPE c VALUE 'F',
           c_t               TYPE c VALUE 'T',
           c_i               TYPE c VALUE 'I',
           c_n               TYPE c VALUE 'N',
           c_h               TYPE c VALUE 'H',
           c_ag(2)           TYPE c VALUE 'AG',
           c_ne(2)           TYPE c VALUE 'NE',
           c_01(2)           TYPE c VALUE '01',
           c_30(2)           TYPE c VALUE '30',
           c_40(2)           TYPE c VALUE '40',
           c_50(4)           TYPE c VALUE '0050',
           c_76(2)           TYPE c VALUE '76',
           c_71(2)           TYPE c VALUE '71',
           c_72(2)           TYPE c VALUE '72',
           c_br(2)           TYPE c VALUE 'BR',
           c_lf(2)           TYPE c VALUE 'LF',
           c_lr(2)           TYPE c VALUE 'LR',
           c_z1(2)           TYPE c VALUE 'Z1',
           c_add(3)          TYPE c VALUE 'ADD',
           c_del(3)          TYPE c VALUE 'DEL',
           c_dg1(3)          TYPE c VALUE 'DG1',
           c_dg2(3)          TYPE c VALUE 'DG2',
           c_dummy_header(3) TYPE c VALUE '099',
           c_dummy_itens(3)  TYPE c VALUE '098',
           c_exit(4)         TYPE c VALUE 'EXIT',
           c_root(4)         TYPE c VALUE 'ROOT',
           c_minimizar(4)    TYPE c VALUE '@K2@',
           c_maximizar(4)    TYPE c VALUE '@K1@',
           c_back(4)         TYPE c VALUE 'BACK',
           c_save(4)         TYPE c VALUE 'SAVE',
           c_desat(5)        TYPE c VALUE 'DESAT',
           c_dmbtr(5)        TYPE c VALUE 'DMBTR',
           c_modif(5)        TYPE c VALUE 'MODIF',
           c_cancel(6)       TYPE c VALUE 'CANCEL',
           c_reinicia(8)     TYPE c VALUE 'REINICIA',
           c_deldoc(6)       TYPE c VALUE 'DELDOC',
           c_displa(6)       TYPE c VALUE 'DISPLA',
           c_dclick(6)       TYPE c VALUE 'DCLICK',
           c_search(6)       TYPE c VALUE 'SEARCH',
           c_atuali(6)       TYPE c VALUE 'ATUALI',
           c_add_msg(7)      TYPE c VALUE 'ADD_MSG',
           c_del_msg(7)      TYPE c VALUE 'DEL_MSG',
           c_clos_msg(8)     TYPE c VALUE 'CLOS_MSG',
           c_save_msg(8)     TYPE c VALUE 'SAVE_MSG',
           c_show_msgre(10)  TYPE c VALUE 'SHOW_MSGRE',
           c_gera_req(8)     TYPE c VALUE 'GERA_REQ',
           c_gera_ped(8)     TYPE c VALUE 'GERA_PED'.


*ALRS
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

    CLASS-METHODS:
      on_hotspot_click_status_adiant FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.



    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_8 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed2 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed5 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed6 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished2 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed3 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .
    CLASS-METHODS:
      on_data_changed4 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .
    CLASS-METHODS:
      on_data_changed_finished3 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_finished4 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_f4                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_treeobject DEFINITION
*---------------------------------------------------------------------*
*       Definition of Data Container                                  *
*---------------------------------------------------------------------*
CLASS lcl_drag_object DEFINITION.
  PUBLIC SECTION.
    DATA text TYPE mtreesnode-text.
ENDCLASS.                    "lcl_drag_object DEFINITION
*---------------------------------------------------------------------*
*       CLASS dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      node_double_click FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.                    "lcl_dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command2 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command3 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command4 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command8 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command9 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_double_click.
    CHECK e_row-rowtype IS NOT INITIAL.
    PERFORM fm_dados_solicitacao USING e_row e_column-fieldname.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_hotspot_click.
    CHECK e_row_id-index IS NOT INITIAL.
    PERFORM fm_dados_solicitacao USING e_row_id-index 'NRO_SOL'.
  ENDMETHOD.

  METHOD on_toolbar.
    DATA: wl_desactive.
    DATA: wl_desactive2.
    CLEAR wl_desactive.
    IF ( wg_cadlan-nro_sol_cp IS INITIAL  ) .
      wl_desactive = 1.
    ENDIF.

    IF wg_acao NE c_modif.
      wl_desactive = 1.
    ENDIF.
    IF g_tab_strip_imp-subscreen NE '0700'.
      ty_toolbar-icon      =  icon_insert_row.
      ty_toolbar-function  =  c_add.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.


      ty_toolbar-icon      =  icon_delete_row.
      ty_toolbar-function  =  c_del.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    IF g_tab_strip_imp-subscreen = '0300'.

      ty_toolbar-icon      =  icon_replace.
      ty_toolbar-function  =  'TROCA'.
      ty_toolbar-text      =  'Troca'.
      ty_toolbar-quickinfo =  'Troca'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.


      ty_toolbar-icon      =  icon_disconnect.
      ty_toolbar-function  =  'PED'.
      ty_toolbar-text      =  'Pedido'.
      ty_toolbar-quickinfo =  'Desmembrar Pedido'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
      "
      ty_toolbar-icon      =  icon_system_undo.
      ty_toolbar-function  =  'DES'.
      ty_toolbar-text      =  'Desfazer'.
      ty_toolbar-quickinfo =  'Desfazer alteração'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_delivery_proposal.
      ty_toolbar-function  =  'REM'.
      ty_toolbar-text      =  'Remessa'.
      ty_toolbar-quickinfo =  'Remessa Final'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

*-----CS2020001451 - 12.01.2021 - inicio
      IF wg_cadlan-bsart = 'ZFTE'.
        wl_desactive2 = wl_desactive.
      ELSE.
        wl_desactive2 = 1.
      ENDIF.

      ty_toolbar-icon      =  icon_disconnect.
      ty_toolbar-function  =  'ZIMP'.
      ty_toolbar-text      =  'Pedido ZIMP'.
      ty_toolbar-quickinfo =  'Pedido ZIMP'.
      ty_toolbar-disabled  = wl_desactive2.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
*-----CS2020001451 - 12.01.2021 - fim

    ENDIF.

    IF g_tab_strip_imp-subscreen = '0700'.
      ty_toolbar-icon      =  icon_generate.
      ty_toolbar-function  =  'GER_ADTO'.
      ty_toolbar-text      =  'Abrir Solicitação de Adiantamento'.
      ty_toolbar-quickinfo =  'Abrir Solicitação de Adiantamento'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    DATA: tl_fatura_aux LIKE TABLE OF tg_fatura,
          wl_fatura     LIKE LINE OF tg_fatura,
          wl_lines      TYPE sy-tabix.
    REFRESH:  tl_fatura_aux .

    IF wg_cadlan-nro_sol_cp IS  NOT INITIAL.
      CASE e_ucomm.
        WHEN c_add.
          tl_fatura_aux[] =  tg_fatura[].
          REFRESH: tg_fatura.
          LOOP AT tl_fatura_aux INTO wl_fatura.
            APPEND wl_fatura TO tg_fatura.
          ENDLOOP.
          CLEAR: wl_fatura.
          APPEND wl_fatura TO tg_fatura.

          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        WHEN c_del.
          CALL METHOD grid1->get_selected_rows
            IMPORTING
              et_index_rows = tl_index_rows.

          LOOP AT tl_index_rows INTO wl_index_rows.
            DELETE tg_fatura INDEX wl_index_rows-index.
          ENDLOOP.

          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

  METHOD handle_user_command9.
    DATA: tg_adto_aux LIKE TABLE OF tg_adto,
          wl_adto     LIKE LINE OF tg_adto,
          wl_lines    TYPE sy-tabix.
    REFRESH:  tg_adto_aux .

    IF wg_cadlan-nro_sol_cp IS  NOT INITIAL.
      CASE e_ucomm.
        WHEN c_add.
*          tg_adto_aux[] =  tg_adto[].
*          REFRESH: tg_adto.
*          LOOP AT tg_adto_aux INTO wl_adto.
*            APPEND wl_adto TO tg_adto.
*          ENDLOOP.
*          CLEAR: wl_adto.
*          READ TABLE tg_fatura INTO DATA(w_fatura) INDEX 1.
*          wl_adto-dt_vcto   = w_fatura-dt_vcto.
*          wl_adto-vlr_adto  = w_fatura-valor.
*          wl_adto-perc_adto = w_fatura-percentual.
*          wl_adto-ebeln     = wg_cadlan-ebeln.
*          tg_adto-icon      = '@39@'.
*          APPEND wl_adto TO tg_adto.
*
*          CALL METHOD grid8->refresh_table_display
*            EXPORTING
*              is_stable = wa_stable.
        WHEN c_del.
          CALL METHOD grid8->get_selected_rows
            IMPORTING
              et_index_rows = tl_index_rows.


          LOOP AT tl_index_rows INTO wl_index_rows.
            DELETE tg_adto INDEX wl_index_rows-index.
          ENDLOOP.

          CALL METHOD grid8->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

  METHOD handle_user_command2.
    DATA: tl_produto_aux LIKE TABLE OF tg_produto,
          wl_produto     LIKE LINE OF tg_produto,
          wl_lines       TYPE sy-tabix,
          vebelp         TYPE zmmt0037-ebelp,
          w_answer(1).
    REFRESH:  tl_produto_aux .

    IF wg_cadlan-nro_sol_cp IS  NOT INITIAL.
      CASE e_ucomm.
        WHEN 'TROCA'.
          PERFORM f_troca USING 'T'.
        WHEN 'PED'.
          PERFORM f_troca USING 'P'.
        WHEN 'DES'.
          PERFORM f_troca USING 'U'.
        WHEN 'REM'.
          PERFORM f_troca USING 'R'.
*-----CS2020001451 - 12.01.2021 - inicio
        WHEN 'ZIMP'.
          PERFORM f_troca USING 'Z'.
*-----CS2020001451 - 12.01.2021 - fim

        WHEN c_add .
          w_answer = '3'.
          IF wg_cadlan-ebeln IS NOT INITIAL.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                text_question         = 'Pedido já criado, deseja aumentar o valor? ?'
                text_button_1         = 'Sim'(100)
                icon_button_1         = 'ICON_OKAY '
                text_button_2         = 'Não'(101)
                icon_button_2         = 'ICON_CANCEL'
                default_button        = '1'
                display_cancel_button = ' '
                start_column          = 25
                start_row             = 6
              IMPORTING
                answer                = w_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.

            IF w_answer = '2'. "não
              EXIT.
            ENDIF.
          ENDIF.
          tl_produto_aux[] =  tg_produto[].
          REFRESH: tg_produto.
          CLEAR vebelp.
          LOOP AT tl_produto_aux INTO wl_produto.
            APPEND wl_produto TO tg_produto.
            IF wl_produto-ebeln IS INITIAL.
              vebelp = wl_produto-ebelp.
            ENDIF.
          ENDLOOP.
          CLEAR: wl_produto.
          ADD 10 TO vebelp.
          wl_produto-ebelp = vebelp.
          IF w_answer = '1'.
            wl_produto-loekz =   icon_create.
          ENDIF.
          APPEND wl_produto TO tg_produto.

          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        WHEN c_del.
          CALL METHOD grid2->get_selected_rows
            IMPORTING
              et_index_rows = tl_index_rows.

          IF wg_cadlan-ebeln IS INITIAL.
            LOOP AT tl_index_rows INTO wl_index_rows.
              DELETE tg_produto INDEX wl_index_rows-index.
            ENDLOOP.
          ELSE.
            LOOP AT tl_index_rows INTO wl_index_rows.
              READ TABLE tg_produto INTO wl_produto INDEX wl_index_rows-index.
              IF wl_produto-loekz = icon_create.
                DELETE tg_produto INDEX wl_index_rows-index.
              ENDIF.
            ENDLOOP.
          ENDIF.

          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

  METHOD handle_user_command3.
    DATA: tl_programa_aux LIKE TABLE OF tg_programa,
          wl_programa     LIKE LINE OF tg_programa,
          wl_produto      LIKE LINE OF tg_produto,
          wl_lines        TYPE sy-tabix,
          vebelp          TYPE zmmt0038-ebelp.
    REFRESH:  tl_programa_aux .

    IF wg_cadlan-nro_sol_cp IS  NOT INITIAL.
      CASE e_ucomm.
        WHEN c_add.
          IF tg_programa[] IS INITIAL.
            CLEAR wl_programa.
            LOOP AT tg_produto INTO wl_produto.
              wl_programa-ebelp   = wl_produto-ebelp.
              wl_programa-matnr   = wl_produto-matnr.
              wl_programa-maktx   = wl_produto-maktx.
              wl_programa-menge   = wl_produto-menge.
              APPEND wl_programa TO tg_programa.
              CLEAR wl_programa.
            ENDLOOP.
          ELSE.
            tl_programa_aux[] =  tg_programa[].
            REFRESH: tg_programa.
            CLEAR vebelp.
            LOOP AT tl_programa_aux INTO wl_programa.
              APPEND wl_programa TO tg_programa.
              vebelp = wl_programa-ebelp.
            ENDLOOP.
            CLEAR: wl_programa.
            ADD 10 TO vebelp.
            wl_programa-ebelp = vebelp.
            APPEND wl_programa TO tg_programa.
          ENDIF.
          CALL METHOD grid3->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        WHEN c_del.
          CALL METHOD grid3->get_selected_rows
            IMPORTING
              et_index_rows = tl_index_rows.

          LOOP AT tl_index_rows INTO wl_index_rows.
            DELETE tg_programa INDEX wl_index_rows-index.
          ENDLOOP.

          CALL METHOD grid3->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

  METHOD handle_user_command4.
    DATA: tl_parceiro_aux LIKE TABLE OF tg_parceiro,
          wl_parceiro     LIKE LINE OF tg_parceiro.

    REFRESH:  tl_parceiro_aux.
    IF wg_cadlan-nro_sol_cp IS  NOT INITIAL.
      CASE e_ucomm.
        WHEN c_add.
          tl_parceiro_aux[] =  tg_parceiro[].
          REFRESH: tg_parceiro.
          LOOP AT tl_parceiro_aux INTO wl_parceiro.
            APPEND wl_parceiro TO tg_parceiro.
          ENDLOOP.
          CLEAR: wl_parceiro.
          APPEND wl_parceiro TO tg_parceiro.

          CALL METHOD grid4->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        WHEN c_del.
          CALL METHOD grid4->get_selected_rows
            IMPORTING
              et_index_rows = tl_index_rows.

          LOOP AT tl_index_rows INTO wl_index_rows.
            DELETE tg_parceiro INDEX wl_index_rows-index.
          ENDLOOP.

          CALL METHOD grid4->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD handle_user_command8.
    IF wg_cadlan-nro_sol_cp IS  NOT INITIAL.
      CASE e_ucomm.
        WHEN 'GER_ADTO'.

          IF tg_msg_ret IS NOT INITIAL.
            EXIT.
          ENDIF.

          CALL METHOD grid8->get_selected_rows
            IMPORTING
              et_index_rows = tl_index_rows.

          IF lines( tl_index_rows[] ) NE 1.
            MESSAGE s897(sd) WITH 'Selecione  uma linha!'  DISPLAY LIKE 'E'.
            EXIT.
          ELSE.
            LOOP AT tl_index_rows INTO wl_index_rows.
              CLEAR: wg_adto.

              READ TABLE tg_adto INTO wg_adto INDEX wl_index_rows-index.

              IF wg_adto-ebeln IS INITIAL
                OR wg_adto-resp_neg IS INITIAL
                OR wg_adto-dep_resp IS INITIAL
                OR wg_adto-dt_vcto IS INITIAL.

                MESSAGE s897(sd) WITH 'Preenchas todas as informações!'  DISPLAY LIKE 'E'.
                EXIT.
              ENDIF.
              "
              SELECT SINGLE *
                FROM ekko
                INTO @DATA(_ekko)
                WHERE ebeln = @wg_adto-ebeln.

              IF _ekko-frgke NE '2' AND _ekko-bsart NE 'NB' AND _ekko-bsart+0(1) NE 'Y' AND _ekko-bsart NE 'ZSON'.
                MESSAGE s897(sd) WITH  'Pedido não está Liberado'  DISPLAY LIKE 'E'.
                EXIT.
              ENDIF.
              IF wg_adto-nro_sol IS INITIAL.
                DATA: zcheck TYPE char1.
                "Check saldo do pedido.
                PERFORM f_check_saldo_pedido USING wg_adto CHANGING zcheck.

                IF zcheck IS INITIAL.
                  PERFORM f_dados_adiantamento USING wg_adto.


                  CLEAR: t_msg,
                         w_msg,
                         w_msg1,
                         w_mode,
                         t_bdcdata,
                         fs_bdcdata.

                  PERFORM populate_bdcdata USING wg_adto-ebeln
                                                 wg_adto-resp_neg
                                                 wg_adto-dep_resp
                                                 wg_adto-dt_vcto
                                                 wg_cadlan-nro_sol_cp.


                  EXPORT v_orig_pgm FROM sy-repid           TO MEMORY ID 'ORIG_PROG'.
                  EXPORT v_vlr_adto FROM wg_adto-vlr_adto   TO MEMORY ID 'VLR_ADTO'.

                  w_mode = 'E'.
                  CALL TRANSACTION 'ZFI0025' USING t_bdcdata  MODE w_mode
                   UPDATE 'S' MESSAGES INTO t_msg.
                  PERFORM busca_dados.
                  CALL METHOD grid8->refresh_table_display
                    EXPORTING
                      is_stable = wa_stable.
                ELSE.
                  MESSAGE s897(sd) WITH 'Valor ultrapassa 100% valor pedido!'  DISPLAY LIKE 'E'.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command
ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_hotspot_click.
    CLEAR wg_cadlan-ebeln_son.
    READ TABLE tg_produto INTO DATA(wg_produto) INDEX e_row_id-index.
    IF wg_produto-ebeln IS NOT INITIAL.
      wg_cadlan-ebeln_son = wg_produto-ebeln.
      SET PARAMETER ID 'BES' FIELD wg_produto-ebeln.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDMETHOD.

  METHOD on_hotspot_click_status_adiant.

***=============================INICIO BUG 71354 / Anderson Oenning - 12/01/2022
    DATA: wa_status_adiant TYPE ty_status_adiant,
          v_ebeln          TYPE ekko-ebeln.
    CLEAR wg_cadlan-ebeln_son.
    FREE: tg_status_adiant[].

    "Verificando valor total do produto.
    READ TABLE tg_adto INTO DATA(wg_adto) INDEX e_row_id-index.
    CLEAR: zvalor_produto.
    IF tg_produto[] IS NOT INITIAL.
      IF wg_adto-ebeln =  wg_cadlan-ebeln.
        CLEAR v_ebeln.
      ELSE.
        v_ebeln =  wg_adto-ebeln.
      ENDIF.
      LOOP AT tg_produto INTO DATA(w_produto) WHERE ebeln = v_ebeln.
        ADD w_produto-valor TO zvalor_produto.
      ENDLOOP.
    ENDIF.

    IF wg_adto-ebeln IS NOT INITIAL.
      SELECT * FROM zfit0046
      INTO TABLE @DATA(tl_zfit0046)
        WHERE ebeln =  @wg_adto-ebeln
        AND EXISTS ( SELECT * FROM zfit0045
                     WHERE zfit0045~nro_sol = zfit0046~nro_sol
                     AND   zfit0045~loekz EQ '' ).

      IF tl_zfit0046[] IS NOT INITIAL.
        SELECT *
          FROM zfit0045
          INTO TABLE @DATA(tl_zfit0045)
          FOR ALL ENTRIES IN @tl_zfit0046
          WHERE nro_sol = @tl_zfit0046-nro_sol.

        SELECT *
        FROM zmmt0147
        INTO TABLE @DATA(tl_zmmt0147)
        FOR ALL ENTRIES IN @tl_zfit0046
        WHERE nro_sol_cp = @tl_zfit0046-nro_sol_cp.

        IF tl_zfit0045[] IS NOT INITIAL.
          SELECT *
          FROM bsak
          INTO TABLE @DATA(tl_bsak)
          FOR ALL ENTRIES IN @tl_zfit0045
          WHERE bukrs  = @tl_zfit0045-bukrs
          AND  belnr  = @tl_zfit0045-belnr.
        ENDIF.
      ENDIF.

      IF tl_zfit0046[] IS NOT INITIAL.                                 "DT_PGTO
        LOOP AT tl_zfit0046 INTO DATA(w_zfit0046).
          wa_status_adiant-vlr_adto  = w_zfit0046-vlr_adiantamento.

          wa_status_adiant-perc_adto = ( w_zfit0046-vlr_adiantamento / zvalor_produto ) * 100.

          READ TABLE tl_zfit0045 INTO DATA(w_zfit0045) WITH KEY nro_sol = w_zfit0046-nro_sol.
          IF sy-subrc EQ 0.
            wa_status_adiant-status = w_zfit0045-status.
            wa_status_adiant-belnr = w_zfit0045-belnr.
            wa_status_adiant-dt_vcto   = w_zfit0045-dt_pgto.
          ENDIF.

          READ TABLE tl_bsak INTO DATA(w_bsak) WITH KEY bukrs = w_zfit0045-bukrs
                                                        belnr = w_zfit0045-belnr.
          IF sy-subrc EQ 0.
            wa_status_adiant-augbl     = w_bsak-augbl.
          ENDIF.

          READ TABLE tl_zmmt0147 INTO DATA(w_zmmt0147) WITH KEY nro_sol_cp = w_zfit0045-nro_sol_cp
                                                                dt_vcto    = w_zfit0045-dt_pgto.

*
          IF sy-subrc EQ 0.
            wa_status_adiant-ebeln     = w_zfit0046-ebeln.
            wa_status_adiant-dep_resp  = w_zmmt0147-dep_resp.
            wa_status_adiant-resp_neg  = w_zmmt0147-resp_neg.
          ENDIF.
          IF wa_status_adiant-ebeln IS INITIAL.
            wa_status_adiant-perc_adto = 0.
          ENDIF.

          wa_status_adiant-icon      = icon_complete.
          IF sy-subrc EQ 0.
            wa_status_adiant-nro_sol    = w_zfit0046-nro_sol.
          ENDIF.

          APPEND wa_status_adiant TO tg_status_adiant.
          CLEAR: w_zmmt0147, w_zfit0045, w_bsak, wa_status_adiant, w_zfit0046.
        ENDLOOP.
        FREE: tl_zfit0046.
        IF tg_status_adiant[] IS NOT INITIAL.
*          SORT tg_status_adiant[] BY ebeln nro_sol dt_vcto.
*          DELETE ADJACENT DUPLICATES FROM tg_status_adiant[] COMPARING ebeln nro_sol dt_vcto.

          FREE: tg_status_adiant_aux.
          tg_status_adiant_aux[] = tg_status_adiant[].
          SORT tg_status_adiant[] BY nro_sol.
          DELETE ADJACENT DUPLICATES FROM tg_status_adiant[] COMPARING nro_sol.

          LOOP AT tg_status_adiant ASSIGNING FIELD-SYMBOL(<ls_status_adiant>).

            <ls_status_adiant>-perc_adto = ''.
            <ls_status_adiant>-vlr_adto = ''.

            LOOP AT tg_status_adiant_aux ASSIGNING FIELD-SYMBOL(<ls_tg_status_adiant_aux>) WHERE nro_sol EQ <ls_status_adiant>-nro_sol.
              ADD <ls_tg_status_adiant_aux>-perc_adto TO  <ls_status_adiant>-perc_adto.
              ADD <ls_tg_status_adiant_aux>-vlr_adto TO  <ls_status_adiant>-vlr_adto.
            ENDLOOP.

          ENDLOOP.

        ENDIF.
        CALL SCREEN 0201 STARTING AT 5 5.

      ENDIF.
    ENDIF.
**=============================INICIO BUG 71354 / Anderson Oenning - 12/01/2022
  ENDMETHOD.
* Método de  execução para Duplo-click
  METHOD on_double_click.
    CLEAR wg_cadlan-ebeln_son.
    IF e_row-index GT 0.
      READ TABLE tg_produto INTO DATA(wg_produto) INDEX e_row-index.
      IF wg_produto-ebeln IS NOT INITIAL.
        wg_cadlan-ebeln_son = wg_produto-ebeln.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_f4.
    TYPES: BEGIN OF t_f4_structure,
             fieldtext TYPE dfies-fieldtext,
             fieldname TYPE dfies-fieldname,
           END OF t_f4_structure.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA: ls_modi TYPE lvc_s_modi.


    CASE e_fieldname.

      WHEN 'LGORT'.
        TYPES : BEGIN OF ty_lgort,
                  lgort TYPE t001l-lgort,
                  lgobe TYPE t001l-lgobe,
                END OF ty_lgort.

        DATA: wl_return_lg TYPE  ddshretval,
              wl_dselclg   TYPE  dselc,
              tl_lgort     TYPE TABLE OF ty_lgort,
              tl_return_lg TYPE TABLE OF ddshretval,
              tl_dselclg   TYPE TABLE OF dselc.

        SELECT  lgort lgobe
           FROM t001l
           INTO TABLE tl_lgort
            WHERE  werks = wg_cadlan-werks
          ORDER BY lgort ASCENDING.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'LGORT'
            value_org       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          TABLES
            value_tab       = tl_lgort
            return_tab      = tl_return_lg
            dynpfld_mapping = tl_dselclg.

        READ TABLE tl_return_lg INTO wl_return_lg INDEX 1.
        IF sy-subrc = 0 AND wl_return_lg-fieldval <> ''.
          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'LGORT'.
          ls_modi-value     = wl_return_lg-fieldval.
          APPEND ls_modi TO <itab>.
          er_event_data->m_event_handled = 'X'.
        ENDIF.

      WHEN 'DEP_RESP'.
        TYPES: BEGIN OF ty_dep ,
                 dep_resp TYPE zfit0045-dep_resp,
                 text1    TYPE t012t-text1,
               END OF ty_dep.

        DATA: tl_return_tab TYPE TABLE OF ddshretval,
              wl_return_tab TYPE  ddshretval,
              tl_dselc      TYPE TABLE OF dselc,
              tl_dep        TYPE TABLE OF ty_dep.

        CLEAR tl_dep.

        SELECT dep_resp dep_resp_desc
          FROM zimp_cad_depto
          INTO TABLE tl_dep.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'DEP_RESP'
            value_org       = 'S'
          TABLES
            value_tab       = tl_dep
            return_tab      = tl_return_tab
            dynpfld_mapping = tl_dselc.

        READ TABLE tl_return_tab INTO wl_return_tab INDEX 1.
        IF sy-subrc = 0 AND wl_return_tab-fieldval <> ''.
          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'DEP_RESP'.
          ls_modi-value     = wl_return_tab-fieldval.
          APPEND ls_modi TO <itab>.
          er_event_data->m_event_handled = 'X'.
        ENDIF.



      WHEN 'RESP_NEG'.

        TYPES: BEGIN OF ty_usr,
                 bname     TYPE v_usr_name-bname,
                 name_text TYPE v_usr_name-name_text,
               END OF ty_usr.


        DATA: tl_return_tab2 TYPE TABLE OF ddshretval,
              wl_return_tab2 TYPE  ddshretval,
              tl_dselc2      TYPE TABLE OF dselc,
              tl_usr         TYPE TABLE OF ty_usr.

        SELECT bname  name_text
           FROM  v_usr_name INTO TABLE tl_usr.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'BNAME'
            value_org       = 'S'
          TABLES
            value_tab       = tl_usr
            return_tab      = tl_return_tab2
            dynpfld_mapping = tl_dselc2.

        READ TABLE tl_return_tab2 INTO wl_return_tab2 INDEX 1.
        IF sy-subrc = 0 AND wl_return_tab2-fieldval <> ''.
          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'RESP_NEG'.
          ls_modi-value     = wl_return_tab2-fieldval.
          APPEND ls_modi TO <itab>.
          er_event_data->m_event_handled = 'X'.
        ENDIF.

    ENDCASE.
  ENDMETHOD. "on_f4

  METHOD on_data_changed2.
    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          lv_value2  TYPE lvc_value,
          vl_value   TYPE lvc_value,
          wl_makt    TYPE makt,
          wl_mara    TYPE mara,
          v_lifnr    TYPE lfa1-lifnr,
          vbrtwr     TYPE zmmt0037-brtwr,
          vbicms     TYPE zmmt0037-bicms,
          vpicms     TYPE zmmt0037-picms,
          vmenge     TYPE zmmt0037-menge,
          vmenge_d   TYPE zmmt0037-menge,
          vnetpr     TYPE zmmt0037-netpr,
          vbprme     TYPE zmmt0037-bprme,
          vpeinh     TYPE zmmt0037-peinh,
          vmwskz     TYPE zmmt0037-mwskz,
          v_ebeln    TYPE ekpo-ebeln,
          wl_produto LIKE LINE OF tg_produto,
          w_fator    TYPE i.

    "
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_cadlan-lifnr
      IMPORTING
        output = v_lifnr.

    w_fator = 1.
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'MATNR'.
      lv_value = ls_good-value.
      lv_value2 = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE *
        FROM makt
        INTO wl_makt
          WHERE spras EQ 'P'
          AND matnr EQ lv_value.

      IF sy-subrc IS INITIAL.
        MOVE wl_makt-maktx TO lv_value.

      ELSE.
        CLEAR lv_value.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MAKTX'
          i_value     = lv_value.

      SELECT SINGLE *
        FROM mara
        INTO wl_mara
          WHERE  matnr EQ lv_value2.

      IF sy-subrc IS INITIAL.
*        MOVE WL_MARA-MEINS TO LV_VALUE.
        IF wl_mara-bstme IS NOT INITIAL.
          MOVE wl_mara-bstme TO lv_value.
        ELSE.
          MOVE wl_mara-meins TO lv_value.
        ENDIF.
      ELSE.
        CLEAR lv_value.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MEINS'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'BPRME'
          i_value     = lv_value.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'NETPR_ROYA' OR fieldname = 'NETPR_GERM'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.

      IF ls_good-fieldname = 'NETPR_ROYA'.
        ADD wl_produto-netpr_germ TO vnetpr.
      ELSE.
        ADD wl_produto-netpr_roya TO vnetpr.
      ENDIF.

      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR'
          i_value     = lv_value.


      PERFORM r_imposto_item USING v_lifnr
                                      wl_produto-werks
                                      wl_produto-ebelp
                                      v_ebeln
                                      wl_produto-matnr
                                      wl_produto-menge
                                      vnetpr
                                      wl_produto-mwskz
                                      wl_produto-peinh
                                      wl_produto-bprme
                                      wl_produto-netpr_desc
                                      wl_produto-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.
      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.


      vnetpr = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.


    ENDLOOP.

    "Ajuste de Preço Bruto
    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname EQ 'BRTWR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vbrtwr = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      vnetpr = vbrtwr - ( ( vbrtwr * ( wl_produto-bicms / 100 ) ) * ( wl_produto-picms / 100 ) ).

      PERFORM r_imposto_item USING v_lifnr
                                      wl_produto-werks
                                      wl_produto-ebelp
                                      v_ebeln
                                      wl_produto-matnr
                                      wl_produto-menge
                                      vnetpr
                                      wl_produto-mwskz
                                      wl_produto-peinh
                                      wl_produto-bprme
                                      wl_produto-netpr_desc
                                      wl_produto-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.

      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR'
          i_value     = lv_value.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      vnetpr = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.
    ENDLOOP.

    "Ajuste de Percentual base de calculo para ICMS
    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname EQ 'BICMS'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vbicms = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      vnetpr = wl_produto-brtwr - ( ( wl_produto-brtwr * ( vbicms / 100 ) ) * ( wl_produto-picms / 100 ) ).

      PERFORM r_imposto_item USING v_lifnr
                                      wl_produto-werks
                                      wl_produto-ebelp
                                      v_ebeln
                                      wl_produto-matnr
                                      wl_produto-menge
                                      vnetpr
                                      wl_produto-mwskz
                                      wl_produto-peinh
                                      wl_produto-bprme
                                      wl_produto-netpr_desc
                                      wl_produto-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.

      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR'
          i_value     = lv_value.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      vnetpr = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.
    ENDLOOP.

    "Ajuste de Percentual de ICMS
    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname EQ 'PICMS'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vpicms = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      vnetpr = wl_produto-brtwr - ( ( wl_produto-brtwr * ( wl_produto-bicms / 100 ) ) * ( vpicms / 100 ) ).

      PERFORM r_imposto_item USING v_lifnr
                                      wl_produto-werks
                                      wl_produto-ebelp
                                      v_ebeln
                                      wl_produto-matnr
                                      wl_produto-menge
                                      vnetpr
                                      wl_produto-mwskz
                                      wl_produto-peinh
                                      wl_produto-bprme
                                      wl_produto-netpr_desc
                                      wl_produto-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.

      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR'
          i_value     = lv_value.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      vnetpr = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'NETPR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.

      PERFORM r_imposto_item USING v_lifnr
                                      wl_produto-werks
                                      wl_produto-ebelp
                                      v_ebeln
                                      wl_produto-matnr
                                      wl_produto-menge
                                      vnetpr
                                      wl_produto-mwskz
                                      wl_produto-peinh
                                      wl_produto-bprme
                                      wl_produto-netpr_desc
                                      wl_produto-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      vnetpr = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.


    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE fieldname = 'BPRME'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vbprme = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      PERFORM r_imposto_item USING v_lifnr
                                      wl_produto-werks
                                      wl_produto-ebelp
                                      v_ebeln
                                      wl_produto-matnr
                                      wl_produto-menge
                                      wl_produto-netpr
                                      wl_produto-mwskz
                                      wl_produto-peinh
                                      vbprme
                                      wl_produto-netpr_desc
                                      wl_produto-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      vnetpr = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.


    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE  fieldname = 'NETPR_DESC'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      "
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.

      IF wg_cadlan-ebeln IS NOT INITIAL.
        SELECT SINGLE knumv
              FROM ekko
             INTO @DATA(v_knumv)
             WHERE ebeln =  @wg_cadlan-ebeln.

        SELECT SINGLE *
          FROM konv
          INTO @DATA(wa_konv)
          WHERE knumv = @v_knumv
          AND   kposn = @wl_produto-ebelp
          AND   kschl = 'RB00'.
      ELSE.
        CLEAR wa_konv.
      ENDIF.
      "05.09.2020
      PERFORM r_imposto_item USING v_lifnr
                                     wl_produto-werks
                                     wl_produto-ebelp
                                     v_ebeln
                                     wl_produto-matnr
                                     wl_produto-menge
                                     wl_produto-netpr
                                     wl_produto-mwskz
                                     wl_produto-peinh
                                     wl_produto-bprme
                                     vnetpr
                                     wl_produto-netpr_supl
                        CHANGING   w_valor
                                   w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      vnetpr = w_valor.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.
      "05.09.2020 fim

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE  fieldname = 'NETPR_SUPL'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      "
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      IF wg_cadlan-ebeln IS NOT INITIAL.
        SELECT SINGLE knumv
              FROM ekko
             INTO @DATA(v_knumv2)
             WHERE ebeln =  @wg_cadlan-ebeln.

        SELECT SINGLE *
          FROM konv
          INTO @DATA(wa_konv2)
          WHERE knumv = @v_knumv2
          AND   kposn = @wl_produto-ebelp
          AND   kschl = 'ZB00'.
      ELSE.
        CLEAR wa_konv2.
      ENDIF.

      "05.09.2020
      PERFORM r_imposto_item USING v_lifnr
                                     wl_produto-werks
                                     wl_produto-ebelp
                                     v_ebeln
                                     wl_produto-matnr
                                     wl_produto-menge
                                     wl_produto-netpr
                                     wl_produto-mwskz
                                     wl_produto-peinh
                                     wl_produto-bprme
                                     wl_produto-netpr_desc
                                     vnetpr
                        CHANGING   w_valor
                                   w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      vnetpr = w_valor.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.
      "05.09.2020 fim

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE  fieldname = 'NETPR_FRETE'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      vnetpr = wl_produto-netpr + lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FRETE'
          i_value     = lv_value.


      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR'
          i_value     = lv_value.

    ENDLOOP.


    LOOP AT er_data_changed->mt_good_cells
                            INTO ls_good
                            WHERE fieldname = 'MENGE'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vmenge = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.

      vmenge_d = wl_produto-menge - wl_produto-menge_migo.
*      IF vmenge_d GT vmenge AND wl_produto-menge_migo GT 0.
*        lv_value = wl_produto-menge.
*        MESSAGE 'Quantidade menor que a disponivel' TYPE 'I'.
*        CALL METHOD er_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'MENGE'
*            i_value     = lv_value.
*      ELSE.
      PERFORM r_imposto_item USING v_lifnr
                                      wl_produto-werks
                                      wl_produto-ebelp
                                      v_ebeln
                                      wl_produto-matnr
                                      vmenge
                                      wl_produto-netpr
                                      wl_produto-mwskz
                                      wl_produto-peinh
                                      wl_produto-bprme
                                      wl_produto-netpr_desc
                                      wl_produto-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.


      vnetpr = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.
*      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                            INTO ls_good
                            WHERE   fieldname = 'PEINH'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vpeinh = lv_value.

      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      PERFORM r_imposto_item USING v_lifnr
                                      wl_produto-werks
                                      wl_produto-ebelp
                                      v_ebeln
                                      wl_produto-matnr
                                      wl_produto-menge
                                      wl_produto-netpr
                                      wl_produto-mwskz
                                      vpeinh
                                      wl_produto-bprme
                                      wl_produto-netpr_desc
                                      wl_produto-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE fieldname = 'QTDE_TROCA'.


      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      wl_produto-qtde_troca = vnetpr.

      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'QTDE_TROCA'
          i_value     = lv_value.


      PERFORM r_imposto_item USING v_lifnr
                                            wl_produto-werks
                                            wl_produto-ebelp
                                            v_ebeln
                                            wl_produto-matnr
                                            wl_produto-qtde_troca
                                            wl_produto-netpr
                                            wl_produto-mwskz
                                            wl_produto-peinh
                                            wl_produto-bprme
                                            wl_produto-netpr_desc
                                            wl_produto-netpr_supl
                               CHANGING   w_valor
                                          w_wmwst.
      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_TROCA'
          i_value     = lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE   fieldname = 'MWSKZ'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vmwskz = lv_value.

      READ TABLE tg_produto INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      PERFORM r_imposto_item USING v_lifnr
                                      wl_produto-werks
                                      wl_produto-ebelp
                                      v_ebeln
                                      wl_produto-matnr
                                      wl_produto-menge
                                      wl_produto-netpr
                                      vmwskz
                                      wl_produto-peinh
                                      wl_produto-bprme
                                      wl_produto-netpr_desc
                                      wl_produto-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.

    ENDLOOP.

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_DATA_CHANGED2

  METHOD on_data_changed5.
    DATA: ls_good     TYPE lvc_s_modi,
          lv_value    TYPE lvc_value,
          lv_value2   TYPE lvc_value,
          vl_value    TYPE lvc_value,
          wl_makt     TYPE makt,
          wl_matnr    TYPE mara-matnr,
          wl_mara     TYPE mara,
          vmenge      TYPE zmmt0037-menge,
          vnetpr      TYPE zmmt0037-netpr,
          vbprme      TYPE zmmt0037-bprme,
          vmwskz      TYPE zmmt0037-mwskz,
          v_ebeln     TYPE ekpo-ebeln,
          v_ebeln2    TYPE ekpo-ebeln,
          wl_produto  LIKE LINE OF tg_produto_sel,
          wl_produto2 LIKE LINE OF tg_produto2,
          w_fator     TYPE i.

    "
    CLEAR v_ebeln2.
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'MATNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      lv_value2 = lv_value.

      READ TABLE tg_produto_sel INTO wl_produto INDEX ls_good-row_id.

      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.


      SELECT SINGLE *
        FROM makt
        INTO wl_makt
          WHERE spras EQ 'P'
          AND matnr EQ lv_value.

      IF sy-subrc IS INITIAL.
        MOVE wl_makt-maktx TO lv_value.

      ELSE.
        CLEAR lv_value.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MAKTX'
          i_value     = lv_value.

      SELECT SINGLE *
        FROM mara
        INTO wl_mara
          WHERE  matnr EQ lv_value2.

      IF sy-subrc IS INITIAL.
        IF wl_mara-bstme IS NOT INITIAL.
          MOVE wl_mara-bstme TO lv_value.
          MOVE wl_mara-bstme TO wl_mara-meins.
        ELSE.
          MOVE wl_mara-meins TO lv_value.
        ENDIF.
      ELSE.
        CLEAR lv_value.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MEINS'
          i_value     = lv_value.


      IF wl_mara-bstme IS NOT INITIAL.
        MOVE wl_mara-bstme TO lv_value.
        wl_produto-bprme = wl_mara-bstme.
      ELSE.
        MOVE wl_mara-meins TO lv_value.
        wl_produto-bprme = wl_mara-meins.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'BPRME'
          i_value     = lv_value.


      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                    wl_produto-werks
                                    wl_produto-ebelp
                                    v_ebeln2
                                    wl_mara-matnr
                                    wl_produto-menge
                                    wl_produto-netpr
                                    wl_produto-mwskz
                                    wl_produto-peinh
                                    wl_produto-bprme
                                    wl_produto-netpr_desc
                                    wl_produto-netpr_supl
                             CHANGING   w_valor
                                       w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.

    ENDLOOP.


    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'BPRME'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vbprme = lv_value.
      CHECK vbprme IS NOT INITIAL.

      READ TABLE tg_produto_sel INTO wl_produto INDEX ls_good-row_id.
      wl_produto-bprme = vbprme.

      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                            wl_produto-werks
                                            wl_produto-ebelp
                                            v_ebeln2
                                            wl_produto-matnr
                                            wl_produto-menge
                                            wl_produto-netpr
                                            wl_produto-mwskz
                                            wl_produto-peinh
                                            wl_produto-bprme
                                            wl_produto-netpr_desc
                                            wl_produto-netpr_supl
                               CHANGING   w_valor
                                          w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.


    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'NETPR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      CHECK vnetpr GT 0.

      READ TABLE tg_produto_sel INTO wl_produto INDEX ls_good-row_id.
      wl_produto-netpr = vnetpr.

      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                            wl_produto-werks
                                            wl_produto-ebelp
                                            v_ebeln2
                                            wl_produto-matnr
                                            wl_produto-menge
                                            wl_produto-netpr
                                            wl_produto-mwskz
                                            wl_produto-peinh
                                            wl_produto-bprme
                                            wl_produto-netpr_desc
                                            wl_produto-netpr_supl
                               CHANGING   w_valor
                                          w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.


    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE  fieldname = 'NETPR_DESC'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto_sel INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                            wl_produto-werks
                                            wl_produto-ebelp
                                            v_ebeln2
                                            wl_produto-matnr
                                            wl_produto-menge
                                            wl_produto-netpr
                                            wl_produto-mwskz
                                            wl_produto-peinh
                                            wl_produto-bprme
                                            vnetpr
                                            wl_produto-netpr_supl
                               CHANGING   w_valor
                                          w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.


    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                            INTO ls_good
                            WHERE  fieldname = 'NETPR_SUPL'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto_sel INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                            wl_produto-werks
                                            wl_produto-ebelp
                                            v_ebeln2
                                            wl_produto-matnr
                                            wl_produto-menge
                                            wl_produto-netpr
                                            wl_produto-mwskz
                                            wl_produto-peinh
                                            wl_produto-bprme
                                            wl_produto-netpr_desc
                                            vnetpr
                               CHANGING   w_valor
                                          w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.



    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                            INTO ls_good
                            WHERE fieldname = 'MENGE'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE tg_produto2 INTO wl_produto2 INDEX 1.
      READ TABLE tg_produto_sel INTO wl_produto INDEX ls_good-row_id.
      wl_produto-menge = lv_value.

      IF wl_produto-matnr NE wl_produto2-matnr AND wl_produto-bprme NE wl_produto2-bprme AND wl_produto-menge GT 0.
        IF wl_produto2-vlr_troca GT 0.
          wl_produto-netpr = wl_produto2-vlr_troca / wl_produto-menge.
          lv_value = wl_produto-netpr.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'NETPR'
              i_value     = lv_value.
        ENDIF.

      ENDIF.

      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.

      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                            wl_produto-werks
                                            wl_produto-ebelp
                                            v_ebeln2
                                            wl_produto-matnr
                                            wl_produto-menge
                                            wl_produto-netpr
                                            wl_produto-mwskz
                                            wl_produto-peinh
                                            wl_produto-bprme
                                            wl_produto-netpr_desc
                                            wl_produto-netpr_supl
                               CHANGING   w_valor
                                          w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.

*** BUG - 63565 - CSB - Inicio
      lv_value = w_wmwst + w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_TOT'
          i_value     = lv_value.
*** BUG - 63565 - CSB - Fim

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                            INTO ls_good
                            WHERE   fieldname = 'PEINH'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      wl_produto-peinh = vnetpr.

      READ TABLE tg_produto_sel INTO wl_produto INDEX ls_good-row_id.

      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.

      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                            wl_produto-werks
                                            wl_produto-ebelp
                                            v_ebeln2
                                            wl_produto-matnr
                                            wl_produto-menge
                                            wl_produto-netpr
                                            wl_produto-mwskz
                                            wl_produto-peinh
                                            wl_produto-bprme
                                            wl_produto-netpr_desc
                                            wl_produto-netpr_supl
                               CHANGING   w_valor
                                          w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.

    ENDLOOP.

    "MWSKZ
    LOOP AT er_data_changed->mt_good_cells
                           INTO ls_good
                           WHERE   fieldname = 'MWSKZ'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vmwskz = lv_value.

      READ TABLE tg_produto_sel INTO wl_produto INDEX ls_good-row_id.
      CLEAR: w_valor,w_wmwst,v_ebeln2.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      wl_produto-netpr = wl_produto-netpr.
      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                               wl_produto-werks
                               wl_produto-ebelp
                               v_ebeln2
                               wl_produto-matnr
                               wl_produto-menge
                               wl_produto-netpr
                               wl_produto-mwskz "Iva original
                               wl_produto-peinh
                               wl_produto-bprme
                               wl_produto-netpr_desc
                               wl_produto-netpr_supl
                  CHANGING   w_valor2  "valor original
                             w_wmwst2. "imposto original

      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                   wl_produto-werks
                                   wl_produto-ebelp
                                   v_ebeln2
                                   wl_produto-matnr
                                   wl_produto-menge
                                   wl_produto-netpr
                                   vmwskz "Novo Iva
                                   wl_produto-peinh
                                   wl_produto-bprme
                                   wl_produto-netpr_desc
                                   wl_produto-netpr_supl
                      CHANGING   w_valor
                                 w_wmwst.
      IF wl_produto-bprme = 'TO'.
        w_fator = 1000.
      ELSE.
        w_fator = 1.
      ENDIF.
      IF wl_produto-menge GT 0.
        vnetpr =   ( w_valor2 / ( 1 +  ( w_wmwst / w_valor2 ) ) ) / ( wl_produto-menge / wl_produto-peinh ) * w_fator.
      ELSE.
        vnetpr = 0.
      ENDIF.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR'
          i_value     = lv_value.

      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                        wl_produto-werks
                                        wl_produto-ebelp
                                        v_ebeln2
                                        wl_produto-matnr
                                        wl_produto-menge
                                        vnetpr
                                        vmwskz
                                         wl_produto-peinh
                                         wl_produto-bprme
                                         wl_produto-netpr_desc
                                         wl_produto-netpr_supl
                           CHANGING   w_valor
                                      w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      lv_value = w_valor. " - WL_PRODUTO-NETPR_DESC + WL_PRODUTO-NETPR_SUPL.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.

*** BUG - 63565 - CSB - Inicio
      lv_value = w_valor + w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_TOT'
          i_value     = lv_value.
*** BUG - 63565 - CSB - Fim
    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed6.
    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          lv_value2  TYPE lvc_value,
          vl_value   TYPE lvc_value,
          wl_makt    TYPE makt,
          wl_mara    TYPE mara,
          vmenge     TYPE zmmt0037-menge,
          vnetpr     TYPE zmmt0037-netpr,
          vbprme     TYPE zmmt0037-bprme,
          vpeinh     TYPE zmmt0037-peinh,
          vmwskz     TYPE zmmt0037-mwskz,
          v_ebeln    TYPE ekpo-ebeln,
          wl_produto LIKE LINE OF tg_produto.


    LOOP AT er_data_changed->mt_good_cells
                                   INTO ls_good
                                   WHERE fieldname = 'QTDE_TROCA'.


      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto2 INTO wl_produto INDEX ls_good-row_id.
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      wl_produto-qtde_troca = vnetpr.
      IF wl_produto-qtde_troca > wl_produto-menge_tro.
        MESSAGE 'Quantidade troca maior que disponivel' TYPE 'I'.
        EXIT.
      ENDIF.

      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'QTDE_TROCA'
          i_value     = lv_value.


      PERFORM r_imposto_item USING wg_cadlan-lifnr
                                            wl_produto-werks
                                            wl_produto-ebelp
                                            v_ebeln
                                            wl_produto-matnr
                                            wl_produto-qtde_troca
                                            wl_produto-netpr
                                            wl_produto-mwskz
                                            wl_produto-peinh
                                            wl_produto-bprme
                                            wl_produto-netpr_desc
                                            wl_produto-netpr_supl
                               CHANGING   w_valor
                                          w_wmwst.
      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_TROCA'
          i_value     = lv_value.

    ENDLOOP.


*** PBI - 60952 - Inicio - CSB

    DATA: v_lifnr    TYPE lfa1-lifnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_cadlan-lifnr
      IMPORTING
        output = v_lifnr.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE  fieldname = 'NETPR_DESC'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto2 INTO wl_produto INDEX ls_good-row_id.
      "
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.

      IF wg_cadlan-ebeln IS NOT INITIAL.
        SELECT SINGLE knumv
              FROM ekko
             INTO @DATA(v_knumv)
             WHERE ebeln =  @wg_cadlan-ebeln.

        SELECT SINGLE *
          FROM konv
          INTO @DATA(wa_konv)
          WHERE knumv = @v_knumv
          AND   kposn = @wl_produto-ebelp
          AND   kschl = 'RB00'.
      ELSE.
        CLEAR wa_konv.
      ENDIF.
      "05.09.2020
      PERFORM r_imposto_item USING v_lifnr
                                     wl_produto-werks
                                     wl_produto-ebelp
                                     v_ebeln
                                     wl_produto-matnr
                                     wl_produto-menge
                                     wl_produto-netpr
                                     wl_produto-mwskz
                                     wl_produto-peinh
                                     wl_produto-bprme
                                     vnetpr
                                     wl_produto-netpr_supl
                        CHANGING   w_valor
                                   w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      vnetpr = w_valor.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.
      "05.09.2020 fim

    ENDLOOP.


    LOOP AT er_data_changed->mt_good_cells
                          INTO ls_good
                          WHERE  fieldname = 'NETPR_SUPL'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vnetpr = lv_value.
      READ TABLE tg_produto2 INTO wl_produto INDEX ls_good-row_id.
      "
      v_ebeln = wg_cadlan-ebeln. "pedido original
      IF wl_produto-ebeln IS NOT INITIAL.
        v_ebeln = wl_produto-ebeln. "pedido filho
      ENDIF.
      IF wg_cadlan-ebeln IS NOT INITIAL.
        SELECT SINGLE knumv
              FROM ekko
             INTO @DATA(v_knumv2)
             WHERE ebeln =  @wg_cadlan-ebeln.

        SELECT SINGLE *
          FROM konv
          INTO @DATA(wa_konv2)
          WHERE knumv = @v_knumv2
          AND   kposn = @wl_produto-ebelp
          AND   kschl = 'ZB00'.
      ELSE.
        CLEAR wa_konv2.
      ENDIF.

      "05.09.2020
      PERFORM r_imposto_item USING v_lifnr
                                     wl_produto-werks
                                     wl_produto-ebelp
                                     v_ebeln
                                     wl_produto-matnr
                                     wl_produto-menge
                                     wl_produto-netpr
                                     wl_produto-mwskz
                                     wl_produto-peinh
                                     wl_produto-bprme
                                     wl_produto-netpr_desc
                                     vnetpr
                        CHANGING   w_valor
                                   w_wmwst.

      lv_value = w_wmwst.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WMWST'
          i_value     = lv_value.

      lv_value = w_valor.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR'
          i_value     = lv_value.

      vnetpr = w_valor.
      lv_value = vnetpr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NETPR_FINAL'
          i_value     = lv_value.
      "05.09.2020 fim

    ENDLOOP.
*** PBI - 60952 - Fim - CSB


  ENDMETHOD.

  METHOD on_data_changed_8.
    DATA: ls_good   TYPE lvc_s_modi,
          lv_value  TYPE lvc_value,
          vl_value  TYPE lvc_value,
          wl_fatura LIKE LINE OF tg_fatura,
          vmenge    TYPE zmmt0037-menge,
          vnetpr    TYPE zmmt0037-netpr,
          vperc     TYPE zmmt0036-percentual,
          vvalor    TYPE zmmt0036-valor.
*    READ TABLE tg_adto INTO DATA(wg_adto) INDEX ls_good-row_id.
*    CLEAR: vnetpr, vmenge, zvalor_adto, zvalor_produto, vvalor, vperc.
*    LOOP AT tg_produto INTO DATA(wl_produto) WHERE ebeln = wg_adto-ebeln.
*      IF wl_produto-netpr_final > 0.
*        ADD wl_produto-netpr_final TO vnetpr.
*        ADD wl_produto-menge       TO vmenge.
*      ENDIF.
*    ENDLOOP.
*
*    "Verificando valor total do produto.
*    CLEAR: zvalor_produto, tg_zmmt0147.
*    FREE: tg_zmmt0147[].
*    IF tg_produto[] IS NOT INITIAL.
*      LOOP AT tg_produto INTO DATA(w_produto) WHERE ebeln = wg_adto-ebeln.
*        ADD w_produto-valor TO zvalor_produto.
*      ENDLOOP.
*    ENDIF.
*
***=============================INICIO BUG 71354 / Anderson Oenning - 12/01/2022
*    "Verificando saldo adiantamento.
*    IF wg_cadlan-nro_sol_cp IS NOT INITIAL.
*      SELECT * FROM zfit0046
*      INTO TABLE @DATA(tl_zfit0046)
*        WHERE nro_sol_cp EQ  @wg_cadlan-nro_sol_cp
*        AND EXISTS ( SELECT * FROM zfit0045
*                     WHERE zfit0045~nro_sol = zfit0046~nro_sol
*                     AND   zfit0045~loekz EQ '' ).
*
*      IF tl_zfit0046[] IS NOT INITIAL.
*        IF wg_adto-ebeln IS INITIAL.
*          wg_adto-ebeln    = wg_cadlan-ebeln.
*        ENDIF.
*        LOOP AT tl_zfit0046 INTO DATA(w_zfit0046) WHERE ebeln = wg_adto-ebeln.
*          ADD w_zfit0046-vlr_adiantamento TO zvalor_adto.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
**=============================FIM BUG 71354 / Anderson Oenning - 12/01/2022
    CLEAR: vnetpr, vmenge, zvalor_adto, zvalor_produto, vvalor, vperc.
    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE fieldname = 'PERC_ADTO'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vperc = lv_value.
      READ TABLE tg_adto INTO DATA(w_adto) INDEX ls_good-row_id.

      IF w_adto-ebeln = wg_cadlan-ebeln.
        CLEAR w_adto-ebeln.
      ENDIF.
      LOOP AT tg_produto INTO DATA(wl_produto) WHERE ebeln = w_adto-ebeln.
        IF wl_produto-netpr_final > 0.
          ADD wl_produto-netpr_final TO vnetpr.
          ADD wl_produto-menge       TO vmenge.
        ENDIF.
      ENDLOOP.
      IF vperc > 0 AND vnetpr > 0.
        "Verificando valor total do produto.
        CLEAR: zvalor_produto, tg_zmmt0147.
        FREE: tg_zmmt0147[].
        IF tg_produto[] IS NOT INITIAL.
          LOOP AT tg_produto INTO DATA(w_produto) WHERE ebeln = w_adto-ebeln.
            ADD w_produto-valor TO zvalor_produto.
          ENDLOOP.
        ENDIF.

**=============================INICIO BUG 71354 / Anderson Oenning - 12/01/2022
        "Verificando saldo adiantamento.
        IF wg_cadlan-nro_sol_cp IS NOT INITIAL.
          SELECT * FROM zfit0046
          INTO TABLE @DATA(tl_zfit0046)
            WHERE nro_sol_cp EQ  @wg_cadlan-nro_sol_cp
            AND EXISTS ( SELECT * FROM zfit0045
                         WHERE zfit0045~nro_sol = zfit0046~nro_sol
                         AND   zfit0045~loekz EQ '' ).

          IF tl_zfit0046[] IS NOT INITIAL.
            IF w_adto-ebeln IS INITIAL.
              w_adto-ebeln    = wg_cadlan-ebeln.
            ENDIF.
            LOOP AT tl_zfit0046 INTO DATA(w_zfit0046) WHERE ebeln = w_adto-ebeln.
              ADD w_zfit0046-vlr_adiantamento TO zvalor_adto.
            ENDLOOP.
          ENDIF.
        ENDIF.
        "
        w_adto-vlr_adto = ( vperc / 100 ) * vnetpr .
        lv_value = w_adto-vlr_adto.
        CONDENSE lv_value NO-GAPS.

        zvalor_adto = zvalor_adto + lv_value.
        IF zvalor_adto LE zvalor_produto.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_ADTO'
              i_value     = lv_value.
        ELSE.
          CLEAR: lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_ADTO'
              i_value     = lv_value.

          MESSAGE s897(sd) WITH 'Valor ultrapassa 100% valor pedido!'  DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

      ENDIF.
      "
*      IF vperc > 0 AND vmenge > 0.
*        READ TABLE tg_adto INTO w_adto INDEX ls_good-row_id.
*        w_adto-menge = ( vperc / 100 ) * vmenge.
*        lv_value = wl_fatura-menge.
*        CALL METHOD er_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'MENGE'
*            i_value     = lv_value.
*      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE fieldname = 'VLR_ADTO'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vvalor = lv_value.

      IF vvalor > 0  AND vnetpr > 0.
        READ TABLE tg_adto INTO w_adto INDEX ls_good-row_id.

        w_adto-perc_adto = ( vvalor / vnetpr ) * 100.
        lv_value = w_adto-perc_adto.
        CONDENSE lv_value NO-GAPS.

        zvalor_adto = zvalor_adto + vvalor.
        IF zvalor_adto LE zvalor_produto.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'PERC_ADTO'
              i_value     = lv_value.
        ELSE.

          CLEAR: lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'PERC_ADTO'
              i_value     = lv_value.

          MESSAGE s897(sd) WITH 'Valor ultrapassa 100% valor pedido!'  DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                                INTO LS_GOOD
*                                WHERE FIELDNAME = 'MENGE'.
*      LV_VALUE = LS_GOOD-VALUE.
*      CONDENSE LV_VALUE NO-GAPS.
*      IF LV_VALUE > 0.
*        READ TABLE TG_FATURA INTO WL_FATURA INDEX LS_GOOD-ROW_ID.
*        IF WL_FATURA-PERCENTUAL NE 0 OR WL_FATURA-VALOR NE 0.
*          LV_VALUE = 0.
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'MENGE'
*              I_VALUE     = LV_VALUE.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed.
    DATA: ls_good   TYPE lvc_s_modi,
          lv_value  TYPE lvc_value,
          vl_value  TYPE lvc_value,
          wl_fatura LIKE LINE OF tg_fatura,
          vmenge    TYPE zmmt0037-menge,
          vnetpr    TYPE zmmt0037-netpr,
          vperc     TYPE zmmt0036-percentual,
          vvalor    TYPE zmmt0036-valor.

    CLEAR: vnetpr, vmenge.
    LOOP AT tg_produto INTO DATA(wl_produto).
      IF wl_produto-netpr_final > 0.
        ADD wl_produto-netpr_final TO vnetpr.
        ADD wl_produto-menge       TO vmenge.
      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE fieldname = 'PERCENTUAL'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vperc = lv_value.

      IF vperc > 0 AND vnetpr > 0.
        READ TABLE tg_fatura INTO wl_fatura INDEX ls_good-row_id.
        wl_fatura-valor = ( vperc / 100 ) * vnetpr .
        lv_value = wl_fatura-valor.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR'
            i_value     = lv_value.
      ENDIF.
      "
      IF vperc > 0 AND vmenge > 0.
        READ TABLE tg_fatura INTO wl_fatura INDEX ls_good-row_id.
        wl_fatura-menge = ( vperc / 100 ) * vmenge.
        lv_value = wl_fatura-menge.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'MENGE'
            i_value     = lv_value.
      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE fieldname = 'VALOR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      vvalor = lv_value.

      IF vvalor > 0  AND vnetpr > 0.
        READ TABLE tg_fatura INTO wl_fatura INDEX ls_good-row_id.
        wl_fatura-percentual = ( vvalor / vnetpr ) * 100.
        lv_value = wl_fatura-percentual.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'PERCENTUAL'
            i_value     = lv_value.
      ENDIF.

      wl_fatura-nro_sol_cp = wg_cadlan-nro_sol_cp.
    ENDLOOP.

*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                                INTO LS_GOOD
*                                WHERE FIELDNAME = 'MENGE'.
*      LV_VALUE = LS_GOOD-VALUE.
*      CONDENSE LV_VALUE NO-GAPS.
*      IF LV_VALUE > 0.
*        READ TABLE TG_FATURA INTO WL_FATURA INDEX LS_GOOD-ROW_ID.
*        IF WL_FATURA-PERCENTUAL NE 0 OR WL_FATURA-VALOR NE 0.
*          LV_VALUE = 0.
*          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*            EXPORTING
*              I_ROW_ID    = LS_GOOD-ROW_ID
*              I_FIELDNAME = 'MENGE'
*              I_VALUE     = LV_VALUE.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

*** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    PERFORM verifica_erros.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.

  ENDMETHOD.                    "on_data_changed_finisheD

  METHOD on_data_changed_finished2.
    DATA: tl_dynpfields TYPE TABLE OF dynpread,
          wl_dynpfields TYPE dynpread,
          vnetpr_final  TYPE zmmt0037-netpr,
          vmenge        TYPE zmmt0036-menge.

    CLEAR: vmenge,vnetpr_final.
    LOOP AT tg_produto INTO DATA(wg_produto).
      IF wg_produto-netpr_final > 0.
        ADD wg_produto-netpr_final TO  vnetpr_final.
        ADD wg_produto-menge       TO  vmenge.
      ENDIF.
    ENDLOOP.

    LOOP AT tg_fatura INTO DATA(wg_fatura).
      IF wg_fatura-percentual > 0 AND vnetpr_final > 0.
        wg_fatura-valor = ( wg_fatura-percentual / 100 ) * vnetpr_final .
        MODIFY tg_fatura FROM wg_fatura INDEX sy-tabix TRANSPORTING valor.
      ENDIF.
      IF wg_fatura-percentual > 0 AND vmenge > 0.
        wg_fatura-menge = ( wg_fatura-percentual / 100 ) * vmenge.
        MODIFY tg_fatura FROM wg_fatura INDEX sy-tabix TRANSPORTING menge.
      ENDIF.
    ENDLOOP.

    MOVE: 'WG_CADLAN-EBELN_SON'        TO wl_dynpfields-fieldname,
          wg_cadlan-ebeln_son          TO wl_dynpfields-fieldvalue.
    APPEND wl_dynpfields TO tl_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = tl_dynpfields.


    "ALRS
*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*      EXPORTING
*        functioncode           = '/00'
*      EXCEPTIONS
*        function_not_supported = 1.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED2

  METHOD on_data_changed3.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          wl_makt  TYPE makt.


    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'MATNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE *
        FROM makt
        INTO wl_makt
          WHERE spras EQ 'P'
          AND matnr EQ lv_value.

      IF sy-subrc IS INITIAL.
        MOVE wl_makt-maktx TO lv_value.

      ELSE.
        CLEAR lv_value.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MAKTX'
          i_value     = lv_value.
    ENDLOOP.


  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed4.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
          wl_name1 TYPE lfa1-name1,
          wl_vtext TYPE tpart-vtext.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'LIFNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE name1
        FROM lfa1
        INTO wl_name1
          WHERE lifnr EQ lv_value.

      IF sy-subrc IS INITIAL.
        MOVE wl_name1 TO lv_value.

      ELSE.
        CLEAR lv_value.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NAME1'
          i_value     = lv_value.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'PARVW'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE vtext
        FROM tpart
        INTO wl_vtext
          WHERE spras EQ sy-langu
            AND parvw EQ lv_value.

      IF sy-subrc IS INITIAL.
        MOVE wl_vtext TO lv_value.

      ELSE.
        CLEAR lv_value.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VTEXT'
          i_value     = lv_value.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed_finished3.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    PERFORM verifica_erros.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.
  ENDMETHOD.                    "on_data_changed_finished3

  METHOD on_data_changed_finished4.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD node_double_click.

  ENDMETHOD.                    "drop_complete
ENDCLASS.                    "lcl_dragdrop_receiver IMPLEMENTATION

*ALRS fim
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_IMP_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_strip_imp_active_tab_set OUTPUT.
  PERFORM verifica_erros.
  tab_strip_imp-activetab = g_tab_strip_imp-pressed_tab.
  CASE g_tab_strip_imp-pressed_tab.
    WHEN c_tab_strip_imp-tab1.
      g_tab_strip_imp-subscreen = '0300'.
    WHEN c_tab_strip_imp-tab2.
      g_tab_strip_imp-subscreen = '0200'.
    WHEN c_tab_strip_imp-tab3.
      g_tab_strip_imp-subscreen = '0400'.
    WHEN c_tab_strip_imp-tab4.
      g_tab_strip_imp-subscreen = '0500'.
    WHEN c_tab_strip_imp-tab5.
      g_tab_strip_imp-subscreen = '0700'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                 " TAB_STRIP_IMP_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .
  DATA: wl_linha(6) ,
        wl_zmmt0035     TYPE zmmt0035,
        wl_fatura       LIKE LINE OF tg_fatura,
        wl_fatura_aux   LIKE LINE OF tg_fatura,
        tg_fatura_aux   LIKE TABLE OF tg_fatura,

        wl_produto      LIKE LINE OF tg_produto,
        wl_produto_old  LIKE LINE OF tg_produto,
        wl_programa     LIKE LINE OF tg_programa,
        wl_programa_aux LIKE LINE OF tg_programa,
        tg_produto_aux  LIKE TABLE OF tg_produto,
        tg_programa_aux LIKE TABLE OF tg_programa,

        tl_makt         TYPE TABLE OF makt    WITH HEADER LINE,
        tl_t007a        TYPE TABLE OF t007a   WITH HEADER LINE,
        tl_t006         TYPE TABLE OF t006   WITH HEADER LINE,
        tl_t001l        TYPE TABLE OF t001l   WITH HEADER LINE,

        wl_t024         TYPE t024,
        wl_zsdt0044     TYPE zsdt0044,
        wl_t001w        TYPE t001w,
        wl_lfa1         TYPE lfa1,
        wl_t052u        TYPE t052u,
        wl_tcurc        TYPE tcurc,

        v_menge1        TYPE zmmt0037-menge,
        v_menge2        TYPE zmmt0037-menge,
        v_perc          TYPE zmmt0036-percentual,
        v_safra1        TYPE i,
        v_safra2        TYPE i,
        v_qtde          TYPE i VALUE 0,
        v_qtde_fat      TYPE zmmt0036-menge,
        v_qtde_prod     TYPE zmmt0037-menge,
        v_valor_fat     TYPE zmmt0036-valor,
        v_valor_prod    TYPE zmmt0036-valor,
        v_netpr_final   TYPE zmmt0037-netpr,
        v_netpr_old     TYPE zmmt0037-netpr,
        v_lifnr         TYPE lfa1-lifnr,
        it_datas        TYPE TABLE OF iscal_day,
        wa_datas        TYPE iscal_day.

  DATA: zzstatus TYPE char01.


  REFRESH: tg_msg_ret.
  CLEAR: tg_msg_ret.

  IF wg_cadlan-ihran IS INITIAL.
    MOVE: text-e01                  TO tg_msg_ret-msg,
        'WG_CADLAN-IHRAN'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Dt. Cotação' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar = 'MG'
        factory_calendar = 'ZT'
        date_from        = wg_cadlan-ihran
        date_to          = wg_cadlan-ihran
      TABLES
        holidays         = it_datas
      EXCEPTIONS
        OTHERS           = 1.

    READ TABLE it_datas INTO wa_datas WITH KEY date = wg_cadlan-ihran.
    IF sy-subrc = 0.
      MOVE: text-e01                  TO tg_msg_ret-msg,
      'WG_CADLAN-IHRAN'         TO tg_msg_ret-field.
      CONCATENATE  'Dt. Cotação' 'não é dia útil' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
  IF wg_cadlan-ekgrp IS INITIAL .
    MOVE: text-e01                  TO tg_msg_ret-msg,
          'WG_CADLAN-EKGRP'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Grupo de compradores' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
       FROM t024
       INTO wl_t024
        WHERE  ekgrp EQ wg_cadlan-ekgrp.
    IF sy-subrc NE 0.
      CONCATENATE text-e04 'Grupo de compradores' INTO  tg_msg_ret-msg SEPARATED BY space.
      MOVE 'WG_CADLAN-EKGRP'         TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_cadlan-zterm IS INITIAL .
    MOVE: text-e01                  TO tg_msg_ret-msg,
          'WG_CADLAN-ZTERM'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Condição de Pagamento' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
       FROM t052u
       INTO wl_t052u
        WHERE  zterm EQ wg_cadlan-zterm.
    IF sy-subrc NE 0.
      CONCATENATE text-e04 'Condição de Pagamento' INTO  tg_msg_ret-msg SEPARATED BY space.
      MOVE 'WG_CADLAN-ZTERM'         TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  SELECT SINGLE ekorg
        FROM t024w
        INTO @DATA(v_ekorg)
        WHERE werks EQ @wg_cadlan-werks.

  IF wg_cadlan-safra IS INITIAL .
    MOVE: text-e01                  TO tg_msg_ret-msg,
          'WG_CADLAN-SAFRA'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'SAFRA' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
       FROM zsdt0044
       INTO wl_zsdt0044
        WHERE  safra EQ wg_cadlan-safra+0(4).
    IF sy-subrc NE 0.
      CONCATENATE text-e04 ' SAFRA' INTO  tg_msg_ret-msg  SEPARATED BY space..
      MOVE 'WG_CADLAN-SAFRA'         TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF v_ekorg EQ '0015'.
      v_safra1 = wg_cadlan-safra+0(4).
      v_safra2 = wg_cadlan-safra+5(4).
      v_safra1 = v_safra2 - v_safra1.
      IF v_safra1 NE 1 OR wg_cadlan-safra+4(1) NE '/'.
        CONCATENATE text-e08 '' INTO  tg_msg_ret-msg  SEPARATED BY space..
        MOVE 'WG_CADLAN-SAFRA'         TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.
  ENDIF.

  IF wg_cadlan-werks IS INITIAL .
    MOVE: text-e01                  TO tg_msg_ret-msg,
          'WG_CADLAN-FILIAL'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Filial' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
       FROM t001w
       INTO wl_t001w
        WHERE  werks EQ wg_cadlan-werks.
    IF sy-subrc NE 0.
      CONCATENATE text-e04 ' Filial' INTO  tg_msg_ret-msg SEPARATED BY space..
      MOVE 'WG_CADLAN-FILIAL'         TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
    SELECT *
      FROM t001l
      INTO TABLE tl_t001l
      WHERE werks = wg_cadlan-werks.
  ENDIF.

  IF wg_cadlan-lifnr IS INITIAL .
    MOVE: text-e01                  TO tg_msg_ret-msg,
          'WG_CADLAN-LIFNR'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Fornecedor' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_cadlan-lifnr
      IMPORTING
        output = v_lifnr.
    SELECT SINGLE *
       FROM lfa1
       INTO wl_lfa1
        WHERE  lifnr EQ v_lifnr.
    IF sy-subrc NE 0.
      CONCATENATE text-e04 'Fornecedor' INTO  tg_msg_ret-msg SEPARATED BY space..
      MOVE 'WG_CADLAN-LIFNR'           TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_cadlan-waers IS INITIAL .
    MOVE: text-e01                  TO tg_msg_ret-msg,
          'WG_CADLAN-WAERS'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Moeda' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
       FROM tcurc
       INTO wl_tcurc
        WHERE  waers EQ wg_cadlan-waers.
    IF sy-subrc NE 0.
      CONCATENATE text-e04 ' Moeda' INTO  tg_msg_ret-msg SEPARATED BY space.
      MOVE 'WG_CADLAN-WAERS'         TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  tg_fatura_aux[] = tg_fatura[].
  SORT: tg_fatura_aux BY dt_vcto  .
  v_perc = 0.
  v_qtde_fat = 0.
  v_valor_fat = 0.
  LOOP AT tg_fatura INTO wl_fatura.
    wl_linha = sy-tabix.
    v_qtde = 0.
    ADD wl_fatura-percentual TO v_perc.
    ADD wl_fatura-menge TO v_qtde_fat.
    ADD wl_fatura-valor TO v_valor_fat.
    LOOP AT tg_fatura_aux INTO wl_fatura_aux WHERE dt_vcto = wl_fatura-dt_vcto .
      ADD 1 TO v_qtde.
    ENDLOOP.

    IF v_qtde > 1.
      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
      CONCATENATE text-e03 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDLOOP.

  IF v_perc NE 100.
    MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
    CONCATENATE text-e07 '' INTO  tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  LOOP AT tg_fatura INTO wl_fatura.
    wl_linha = sy-tabix.
    IF wl_fatura-percentual IS INITIAL  AND
       wl_fatura-valor     IS INITIAL AND
       wl_fatura-menge IS INITIAL.
      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
      CONCATENATE text-e11 'LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.


*    IF wl_fatura-percentual IS INITIAL.
*      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
*      CONCATENATE text-e01 'Percentual LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.
*
*    IF wl_fatura-valor IS INITIAL.
*      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
*      CONCATENATE text-e01 'Valor LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.
*
*    IF wl_fatura-menge IS INITIAL.
*      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
*      CONCATENATE text-e01 'Quantidade LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.

  ENDLOOP.


  IF tg_produto[] IS NOT INITIAL.
    SELECT *
      FROM makt
      INTO TABLE tl_makt
      FOR ALL ENTRIES IN tg_produto
      WHERE matnr = tg_produto-matnr.

    SELECT *
      FROM t007a
      INTO TABLE tl_t007a
      FOR ALL ENTRIES IN tg_produto
      WHERE mwskz  = tg_produto-mwskz.

    SELECT *
      FROM t006
      INTO TABLE tl_t006
      FOR ALL ENTRIES IN tg_produto
      WHERE msehi = tg_produto-meins.

  ENDIF.

  SORT: tl_makt   BY matnr,
        tl_t007a  BY mwskz,
        tl_t006   BY msehi.

  v_qtde_prod = 0.
  v_valor_prod = 0.
  LOOP AT tg_produto INTO  wl_produto .
    wl_linha = sy-tabix.
    IF  wl_produto-valor GT 0.
      ADD wl_produto-menge TO v_qtde_prod .
      ADD wl_produto-valor TO v_valor_prod .
    ENDIF.
    IF wl_produto-matnr  IS INITIAL.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE text-e01 'Material LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_makt
        WITH KEY matnr = wl_produto-matnr
                 BINARY SEARCH.
      IF sy-subrc NE 0.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE text-e04 'Material' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wl_produto-netpr_final LT 0.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE text-e12 'LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wl_produto-netpr_final EQ 0.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE text-e25 'LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wl_produto-lgort  IS INITIAL.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE text-e01 'Depósito LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_t001l
        WITH KEY werks = wg_cadlan-werks
                 lgort = wl_produto-lgort
                 BINARY SEARCH.
      IF sy-subrc NE 0.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE text-e04 'Depósito' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wl_produto-mwskz  IS INITIAL.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE text-e01 'IVA LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_t007a
        WITH KEY mwskz = wl_produto-mwskz
                 BINARY SEARCH.
      IF sy-subrc NE 0.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE text-e04 'IVA' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wl_produto-meins  IS INITIAL.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE text-e01 'Unid. LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_t006
        WITH KEY msehi = wl_produto-meins
                 BINARY SEARCH.
      IF sy-subrc NE 0.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE text-e04 'Unid.' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wl_produto-menge IS INITIAL AND wl_produto-loekz IS INITIAL .
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE text-e01 'Quantidade LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wl_produto-peinh IS INITIAL.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE text-e01 'Por LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDLOOP.

  IF v_qtde_fat GT 0 AND v_qtde_prod GT v_qtde_fat.
    MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
    CONCATENATE text-e09 '' INTO  tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

*  Modificação CS2016001138
*  IF V_VALOR_FAT GT 0 AND V_VALOR_PROD GT V_VALOR_FAT.
*    MOVE:  C_TAB_STRIP_IMP-TAB1 TO TG_MSG_RET-ABA.
*    CONCATENATE TEXT-E10 '' INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
*  ENDIF.

  REFRESH  tl_makt.
  IF tg_programa[] IS NOT INITIAL.
    SELECT *
      FROM makt
      INTO TABLE tl_makt
      FOR ALL ENTRIES IN tg_programa
      WHERE matnr = tg_programa-matnr.
  ENDIF.

  tg_produto_aux[] = tg_produto[].
  tg_programa_aux[] = tg_programa[].
  SORT: tg_produto_aux  BY matnr,
        tg_programa_aux BY matnr,
        tl_makt         BY matnr.

  LOOP AT tg_programa INTO  wl_programa .
    wl_linha = sy-tabix.
    IF wl_programa-matnr  IS INITIAL.
      MOVE:  c_tab_strip_imp-tab3 TO tg_msg_ret-aba.
      CONCATENATE text-e01 'Material LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      READ TABLE tl_makt
        WITH KEY matnr = wl_programa-matnr
                 BINARY SEARCH.
      IF sy-subrc NE 0.
        MOVE:  c_tab_strip_imp-tab3 TO tg_msg_ret-aba.
        CONCATENATE text-e04 'Material' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
      READ TABLE tg_produto_aux INTO wl_produto
      WITH KEY matnr = wl_programa-matnr
                 BINARY SEARCH.
      IF sy-subrc NE 0.
        MOVE:  c_tab_strip_imp-tab3 TO tg_msg_ret-aba.
        CONCATENATE text-e05 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wl_programa-data_progr IS INITIAL.
      MOVE:  c_tab_strip_imp-tab3 TO tg_msg_ret-aba.
      CONCATENATE text-e01 'Data Programação LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wl_programa-menge IS INITIAL.
      MOVE:  c_tab_strip_imp-tab3 TO tg_msg_ret-aba.
      CONCATENATE text-e01 'Quantidade LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      CLEAR: v_menge1,v_menge2.
      LOOP AT  tg_produto_aux INTO wl_produto WHERE matnr = wl_programa-matnr.
        ADD wl_produto-menge TO v_menge1.
      ENDLOOP.

      LOOP AT  tg_programa_aux INTO wl_programa_aux WHERE matnr = wl_programa-matnr.
        ADD wl_programa_aux-menge TO v_menge2.
      ENDLOOP.
      IF v_menge2 GT v_menge1 .
        MOVE:  c_tab_strip_imp-tab3 TO tg_msg_ret-aba.
        CONCATENATE text-e06  wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.

  ENDLOOP.

  LOOP AT tg_parceiro.

    CALL FUNCTION 'CONVERSION_EXIT_PARVW_OUTPUT'
      EXPORTING
        input  = tg_parceiro-parvw
      IMPORTING
        output = tg_parceiro-parvw.

    wl_linha = sy-tabix.
    IF tg_parceiro-lifnr  IS INITIAL.
      MOVE:  c_tab_strip_imp-tab4 TO tg_msg_ret-aba.
      CONCATENATE text-e17 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
        FROM lfa1
        INTO @DATA(wlfa1)
        WHERE lifnr = @tg_parceiro-lifnr.
      IF sy-subrc NE 0.
        MOVE:  c_tab_strip_imp-tab4 TO tg_msg_ret-aba.
        CONCATENATE text-e18 ' ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wg_cadlan-bsart = 'ZFTE'.
      IF tg_parceiro-parvw <> 'PR' AND
         tg_parceiro-parvw <> 'EF'.
        MOVE:  c_tab_strip_imp-tab4 TO tg_msg_ret-aba.
        CONCATENATE text-e20 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ELSE.
      IF tg_parceiro-parvw <> 'EF'.
        MOVE:  c_tab_strip_imp-tab4 TO tg_msg_ret-aba.
        CONCATENATE text-e21 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF wg_cadlan-bsart = 'ZFTE' AND tg_parceiro[] IS INITIAL.
    MOVE:  c_tab_strip_imp-tab4 TO tg_msg_ret-aba.
    CONCATENATE text-e22 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

* Obrigatoriedade de anexar arquivo. - PBI - 60951 - Inicio
  DATA: lva_data     TYPE sy-datum,
        lva_hora     TYPE sy-uzeit,
        lva_anexo(1) TYPE c.


  "Check pedido.
  "Verifica se o pedido esta aprovado.
  CLEAR: zzstatus.
  CALL FUNCTION 'ZF_CHECK_APRO_PEDIDO_ZMM0149'
    EXPORTING
      i_ebeln    = wg_cadlan-ebeln
    IMPORTING
      e_aprovado = zzstatus.


  FREE: t_anexos.
  lva_anexo = 'N'.
  l_obj_key = wg_cadlan-nro_sol_cp.



  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      classname          = 'ZMMR149_ANEXO'
      objkey             = l_obj_key
      client             = sy-mandt
    TABLES
      gos_connections    = t_anexos
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3
      OTHERS             = 4.

  DESCRIBE TABLE t_anexos LINES l_lines.
*  clear: vg_datatime.
  IF l_lines IS INITIAL.
    lva_anexo = 'S'.
  ELSE.
    SELECT SINGLE *
      FROM zmmt0035
      INTO wl_zmmt0035
      WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.
    SORT t_anexos DESCENDING BY crea_time.
    READ TABLE t_anexos INTO DATA(w_anexo)  INDEX 1.
    IF sy-subrc EQ 0.
      MOVE w_anexo-crea_time(8) TO lva_data.
      IF lva_data < wl_zmmt0035-data_atual.
        lva_anexo = 'S'.
      ELSE.
        IF lva_data = wl_zmmt0035-data_atual.
          MOVE w_anexo-crea_time+8(6) TO lva_hora.
          SUBTRACT 14400 FROM lva_hora. "4 horas
          IF lva_hora LT wl_zmmt0035-hora_atual.
            lva_anexo = 'S'.
          ENDIF.
        ENDIF.
      ENDIF.


    ENDIF.

  ENDIF.

  CLEAR: v_netpr_final,v_netpr_old.
  LOOP AT tg_produto INTO wl_produto.
    ADD wl_produto-netpr_final TO  v_netpr_final.
  ENDLOOP.
  LOOP AT tg_produto_old INTO wl_produto_old.
    ADD wl_produto_old-netpr_final TO  v_netpr_old.
  ENDLOOP.

  vg_netpr = abs( v_netpr_final - v_netpr_old ) .
  IF vg_netpr GT 1 AND lva_anexo = 'S'.
    CLEAR: vg_netpr.
    IF zzstatus IS NOT INITIAL AND wg_cadlan-ebeln IS NOT INITIAL.
      CONCATENATE text-e23 'Valor Final Alterado  ' INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE trata_fields OUTPUT.
  LOOP AT tg_fields.
    LOOP AT SCREEN.
      IF screen-name EQ tg_fields-campo
      OR screen-group1 EQ tg_fields-group1.
        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        MODIFY SCREEN.
*        EXIT.
      ENDIF.
      IF    screen-name = 'WG_CADLAN-EBELN'.
        IF wg_cadlan-ebeln IS NOT INITIAL.
          screen-input     = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field."
  ENDIF.
  CLEAR: tg_fields.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.

  IF wg_acao IS INITIAL OR wg_acao = c_displa.
    APPEND c_save TO fcode.
    APPEND c_deldoc TO fcode.
    IF xmodif = 'X' OR wg_acao IS INITIAL OR wg_cadlan-nro_sol_cp IS INITIAL.
      APPEND c_modif TO fcode.
    ENDIF.
  ELSEIF xmodif = 'X' .
    APPEND c_modif TO fcode.
  ENDIF.

  APPEND 'CRIA_ADTO' TO fcode.

  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        wl_function LIKE tl_function WITH HEADER LINE.

  wa_layout-zebra      = c_x.
  wa_layout-no_rowmark = space.
  wa_layout-col_opt    = c_x.
  wa_stable-row        = c_x.
  wa_layout-sel_mode   = 'B'.
  wa_layout-cwidth_opt   = 'X'.
  wa_layout-box_fname    = 'MARK'.
  wa_layout-no_toolbar = space.
  CLEAR  wa_layout-stylefname.

  wa_stable-row         = c_x.
  wa_stable-col         = c_x.

  gs_variant_c-report      = sy-repid.


  IF g_custom_cont_fat IS INITIAL.
    CREATE OBJECT g_custom_cont_fat
      EXPORTING
        container_name = g_container_fatura.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_cont_fat
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.


    PERFORM montar_layout.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_fatura[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_data_changed FOR grid1.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
    CALL METHOD grid1->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog[].

    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      w_fieldcatalog-outputlen = '15'.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_fatura[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "GRID2
  IF g_custom_cont_prod IS INITIAL.
    CREATE OBJECT g_custom_cont_prod
      EXPORTING
        container_name = g_container_produto.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = g_custom_cont_prod.

    PERFORM montar_layout_prod.

    CREATE OBJECT obg_toolbar2
      EXPORTING
        io_alv_grid = grid2.


    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.


*      * Register event handler
    SET HANDLER obg_toolbar2->on_toolbar FOR grid2.
    SET HANDLER obg_toolbar2->handle_user_command2 FOR grid2.

    wa_layout-stylefname = 'STYLE'.
    wa_layout-zebra = ''.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_c
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_default            = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_produto[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    PERFORM build_f4_cat.


    SET HANDLER:
              lcl_event_handler=>on_hotspot_click          FOR grid2,
              lcl_event_handler=>on_double_click           FOR grid2,
              lcl_event_handler=>on_data_changed_finished2 FOR grid2,
              lcl_event_handler=>on_data_changed2 FOR grid2,
              lcl_event_handler=>on_f4 FOR grid2.



  ELSE.
    PERFORM montar_layout_prod.
    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      w_fieldcatalog-outputlen = '12'.
      IF w_fieldcatalog-fieldname EQ 'EBELP'.
        w_fieldcatalog-outputlen = '08'.
      ELSEIF w_fieldcatalog-fieldname EQ 'MAKTX'.
        w_fieldcatalog-outputlen = '30'.
      ELSEIF w_fieldcatalog-fieldname EQ 'MEINS'.
        w_fieldcatalog-outputlen = '05'.
      ELSEIF w_fieldcatalog-fieldname EQ 'PEINH'.
        w_fieldcatalog-outputlen = '10'.
      ELSEIF w_fieldcatalog-fieldname EQ 'MWSKZ'.
        w_fieldcatalog-outputlen = '10'.
      ENDIF.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.

    CALL METHOD grid2->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_c
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_default            = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_produto[].

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "GRID3
  IF g_custom_cont_prog IS INITIAL.
    CREATE OBJECT g_custom_cont_prog
      EXPORTING
        container_name = g_container_programa.

    CREATE OBJECT grid3
      EXPORTING
        i_parent = g_custom_cont_prog.

    PERFORM montar_layout_prog.

    CREATE OBJECT obg_toolbar3
      EXPORTING
        io_alv_grid = grid3.

*      * Register event handler
    SET HANDLER obg_toolbar3->on_toolbar FOR grid3.
    SET HANDLER obg_toolbar3->handle_user_command3 FOR grid3.


    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.

    CLEAR wa_layout-stylefname.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_programa[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
*              lcl_event_handler=>on_double_click FOR grid1,
              lcl_event_handler=>on_data_changed_finished3 FOR grid3,
              lcl_event_handler=>on_data_changed3 FOR grid3.

  ELSE.

    CALL METHOD grid3->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog[].

    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      w_fieldcatalog-outputlen = '12'.
      IF w_fieldcatalog-fieldname EQ 'EBELP'.
        w_fieldcatalog-outputlen = '08'.
      ELSEIF w_fieldcatalog-fieldname EQ 'MAKTX'.
        w_fieldcatalog-outputlen = '30'.
      ENDIF.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.

    CALL METHOD grid3->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_programa[].

    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "GRID4
  IF g_custom_cont_parc IS INITIAL.
    CREATE OBJECT g_custom_cont_parc
      EXPORTING
        container_name = g_container_parceiro.

    CREATE OBJECT grid4
      EXPORTING
        i_parent = g_custom_cont_parc.

    PERFORM montar_layout_parc.

    CREATE OBJECT obg_toolbar4
      EXPORTING
        io_alv_grid = grid4.

*      * Register event handler
    SET HANDLER obg_toolbar4->on_toolbar FOR grid4.
    SET HANDLER obg_toolbar4->handle_user_command4 FOR grid4.


    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.

*    wa_layout-cwidth_opt   = ' '.

    CLEAR wa_layout-stylefname.
    CALL METHOD grid4->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_parceiro[].

    CALL METHOD grid4->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid4->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
*              lcl_event_handler=>on_double_click FOR grid1,
              lcl_event_handler=>on_data_changed_finished4 FOR grid4,
              lcl_event_handler=>on_data_changed4 FOR grid4.

  ELSE.
    PERFORM montar_layout_parc.

    CALL METHOD grid4->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CLEAR wa_layout-stylefname.
    CALL METHOD grid4->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_parceiro[].

    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF g_custom_cont_desc IS INITIAL.

    CREATE OBJECT g_custom_cont_desc
      EXPORTING
        container_name = g_descbox.

    IF g_custom_cont_desc IS NOT INITIAL.
      CREATE OBJECT obg_descbox
        EXPORTING
          parent                     = g_custom_cont_desc
          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position          = 72
          max_number_chars           = 350
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

      CALL METHOD obg_descbox->set_toolbar_mode
        EXPORTING
          toolbar_mode = '0'.

      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
    ENDIF.
  ENDIF.


  "======================GRID08 - ADIANTAMENTO=============================
*** pbi - 60949 - inicio
  "GRID8
  IF g_custom_cont_adt IS INITIAL.

    CREATE OBJECT g_custom_cont_adt
      EXPORTING
        container_name = g_container_adt.

    CREATE OBJECT grid8
      EXPORTING
        i_parent = g_custom_cont_adt.

    PERFORM montar_layout_adto.

    CREATE OBJECT obg_toolbar8
      EXPORTING
        io_alv_grid = grid8.
* Register event handler

    SET HANDLER obg_toolbar8->handle_user_command9 FOR grid8.
    SET HANDLER obg_toolbar->handle_user_command8 FOR grid8.
    SET HANDLER obg_toolbar8->on_toolbar FOR grid8.


    SET HANDLER:
        lcl_event_handler=>on_f4 FOR grid8.

    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.

    wa_layout-stylefname = 'STYLE'.

    SET HANDLER: lcl_event_handler=>on_hotspot_click_status_adiant FOR grid8.

    CALL METHOD grid8->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_default            = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_adto[].

    CALL METHOD grid8->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid8->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    SET HANDLER:
              lcl_event_handler=>on_data_changed_8 FOR grid8.


    REFRESH gt_f4_adto[].


    gw_f4_adto-fieldname = 'DEP_RESP'.
    gw_f4_adto-register  = 'X'.
    gw_f4_adto-getbefore = 'X'.
    INSERT gw_f4_adto INTO TABLE gt_f4_adto.
    gw_f4_adto-fieldname = 'RESP_NEG'.
    INSERT gw_f4_adto INTO TABLE gt_f4_adto.


    CALL METHOD grid8->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4_adto.


  ELSE.
    PERFORM montar_layout_adto.

    wa_layout-cwidth_opt = ''.

    CALL METHOD grid8->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid8->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_default            = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_adto[].

    CALL METHOD grid8->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
*** PBI - 60949 - Fim


ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZMMT0036'         'DT_VCTO'      'TG_FATURA' 'DT_VCTO'       'Data Vencto'    '20' 'X' ' ' ' ' ' ',
        2 'ZMMT0036'         'PERCENTUAL'   'TG_FATURA' 'PERCENTUAL'    'Percentual'     '20' 'X' 'X' ' ' ' ',
        3 'ZMMT0036'         'VALOR'        'TG_FATURA' 'VALOR'         'Valor'          '20' 'X' 'X' ' ' ' ',
        4 'ZMMT0036'         'MENGE'        'TG_FATURA' 'MENGE'         'Quantidade'     '20' 'X' 'X' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize)
                            VALUE(p_col_opt).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  IF p_field = 'LOEKZ'.
    w_fieldcatalog-icon         = c_x.
  ENDIF.
  IF p_field = 'ICON'.
    w_fieldcatalog-icon         = c_x.
  ENDIF.
  "
  IF p_field = 'EBELN'.
    w_fieldcatalog-hotspot    = c_x.
  ENDIF.

  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  IF p_field EQ 'CHECKBOX'.
    w_fieldcatalog-checkbox = c_x.
  ENDIF.

  IF p_field EQ 'LGORT'.
    w_fieldcatalog-f4availabl = c_x.
  ENDIF.

  IF p_field EQ 'RESP_NEG'.
    w_fieldcatalog-f4availabl = c_x.
  ENDIF.
  IF p_field EQ'DEP_RESP'.
    w_fieldcatalog-f4availabl = c_x.
  ENDIF.

*  IF p_field EQ 'PARVW'.
*    w_fieldcatalog-f4availabl = c_x.
*  ENDIF.

  w_fieldcatalog-col_opt     = abap_false.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_prod.
  REFRESH t_fieldcatalog.
  DATA: altera(1).
  CLEAR altera.
  IF wg_acao = 'MODIF'.
    altera = 'X'.
  ENDIF.

  PERFORM montar_estrutura USING:
        1 'EKPO'             'LOEKZ'      'TG_PRODUTO' 'LOEKZ'       'Status'      '03' ' ' ' ' ' ' '',
        1 'ZMMT0037'         'EBELN'      'TG_PRODUTO' 'EBELN'       'Ped.Filho'   '10' ' ' ' ' ' ' '',
        1 'ZMMT0037'         'EBELP'      'TG_PRODUTO' 'EBELP'       'Item'        '10' ' ' ' ' ' ' '',
        1 'EKKO'             'BSART'      'TG_PRODUTO' 'BSART'       'Tipo'        '08' ' ' ' ' ' ' '',
        1 'ZMMT0037'         'WERKS'      'TG_PRODUTO' 'WERKS'       'Centro'      '06' ' ' ' ' ' ' '',
        1 'MARA'             'MATNR'      'TG_PRODUTO' 'MATNR'       'Material'    '15' altera ' ' ' ' '',
        1 'MAKT'             'MAKTX'      'TG_PRODUTO' 'MAKTX'       'Texto Breve' '30' ' ' ' ' ' ' '',
        1 ' '                ' '          'TG_PRODUTO' 'LGORT'       'Depósito'    '10' altera ' ' ' ' '',
        1 'ZMMT0037'         'CHARG'      'TG_PRODUTO' 'CHARG'       'Lote'        '10' altera ' ' ' ' '',
        1 'ZMMT0037'         'MENGE'      'TG_PRODUTO' 'MENGE_ORI'   'Qtde.Dispon' '15' ' ' 'X' ' ' '',
        1 'ZMMT0037'         'MENGE'      'TG_PRODUTO' 'MENGE'       'Qtde.Pedido' '15' altera 'X' ' ' '',
        1 'ZMMT0037'         'MEINS'      'TG_PRODUTO' 'MEINS'       'Unid.Ped'    '05' ' ' ' ' ' ' ''.
  IF wg_cadlan-bsart = 'ZSEM'. "sementes
    PERFORM montar_estrutura USING:
       1 'ZMMT0037'         'BRTWR'      'TG_PRODUTO' 'BRTWR'       'Preço Bruto'  '15' ' ' ' ' ' ' '',
       1 'ZMMT0037'         'BICMS'      'TG_PRODUTO' 'BICMS'       'Base ICMS'    '08' ' ' ' ' ' ' '',
       1 'ZMMT0037'         'PICMS'      'TG_PRODUTO' 'PICMS'       '%ICMS'        '05' ' ' ' ' ' ' '',
       1 'ZMMT0037'         'NETPR'      'TG_PRODUTO' 'NETPR'       'Preço Liq.'   '15' ' ' ' ' ' ' '',
       1 'ZMMT0037'         'NETPR_ROYA' 'TG_PRODUTO' 'NETPR_ROYA'  'Royalties'    '15' altera ' ' ' ' '',
       1 'ZMMT0037'         'NETPR_GERM' 'TG_PRODUTO' 'NETPR_GERM'  'Germoplasma'  '15' altera ' ' ' ' ''.
  ELSE.
    PERFORM montar_estrutura USING:
       1 'ZMMT0037'         'BRTWR'      'TG_PRODUTO' 'BRTWR'       'Preço Bruto'  '15' altera ' ' ' ' '',
       1 'ZMMT0037'         'BICMS'      'TG_PRODUTO' 'BICMS'       'Base ICMS'    '08' altera ' ' ' ' '',
       1 'ZMMT0037'         'PICMS'      'TG_PRODUTO' 'PICMS'       '%ICMS'        '05' altera ' ' ' ' '',
       1 'ZMMT0037'         'NETPR'      'TG_PRODUTO' 'NETPR'       'Preço Liq.'   '15' ' ' ' ' ' ' ''.
  ENDIF.


  IF wg_acao = 'MODIF'.
    PERFORM montar_estrutura USING:
       1 'ZMMT0037'         'BPRME'       'TG_PRODUTO' 'BPRME'       'UPP'         '05' 'X' ' ' ' ' ' ',
       1 'ZMMT0037'         'PEINH'       'TG_PRODUTO' 'PEINH'       'Por'         '10' 'X' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR'       'TG_PRODUTO' 'VALOR'       'Valor'       '15' ' ' 'X' ' ' ' ',
       1 'ZMMT0037'         'NETPR_DESC'  'TG_PRODUTO' 'NETPR_DESC'  'Desconto'    '15' 'X' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR_SUPL'  'TG_PRODUTO' 'NETPR_SUPL'  'Suplemento'  '15' 'X' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR_FRETE' 'TG_PRODUTO' 'NETPR_FRETE' 'Valor Frete' '15' 'X' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR'       'TG_PRODUTO' 'NETPR_FINAL' 'Valor Final' '15' ' ' 'X' ' ' ' ',
       1 'ZMMT0037'         'MWSKZ'       'TG_PRODUTO' 'MWSKZ'       'IVA'         '10' 'X' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR'       'TG_PRODUTO' 'WMWST'       'Val.IVA'     '15' ' ' ' ' ' ' ' '.
  ELSE.
    PERFORM montar_estrutura USING:
       1 'ZMMT0037'         'BPRME'       'TG_PRODUTO' 'BPRME'       'UPP'         '05' ' ' ' ' ' ' ' ',
       1 'ZMMT0037'         'PEINH'       'TG_PRODUTO' 'PEINH'       'Por'         '10' ' ' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR'       'TG_PRODUTO' 'VALOR'       'Valor'       '15' ' ' 'X' ' ' ' ',
       1 'ZMMT0037'         'NETPR_DESC'  'TG_PRODUTO' 'NETPR_DESC'  'Desconto'    '15' ' ' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR_SUPL'  'TG_PRODUTO' 'NETPR_SUPL'  'Suplemento'  '15' ' ' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR_FRETE' 'TG_PRODUTO' 'NETPR_FRETE' 'Valor Frete' '15' ' ' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR'       'TG_PRODUTO' 'NETPR_FINAL' 'Valor Final' '15' ' ' 'X' ' ' ' ',
       1 'ZMMT0037'         'MWSKZ'       'TG_PRODUTO' 'MWSKZ'       'IVA'         '10' ' ' ' ' ' ' ' ',
       1 'ZMMT0037'         'NETPR'       'TG_PRODUTO' 'WMWST'       'Val.IVA'     '15' ' ' ' ' ' ' ' '.

  ENDIF.
  PERFORM montar_estrutura USING:
      1 'ZMMT0037'         'NETPR_ORIG'   'TG_PRODUTO' 'NETPR_ORIG'       'Preço Orig.'  '15' ' ' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_PROD

FORM montar_layout_prod2 USING p_tipo.
  REFRESH t_fieldcatalog.

  PERFORM montar_estrutura USING:
        1 'ZMMT0037'         'EBELP'      'TG_PRODUTO2' 'EBELP'       'Item'        '10' ' ' ' ' ' ' ' ',
        1 'MARA'             'MATNR'      'TG_PRODUTO2' 'MATNR'       'Material'    '15' ' ' ' ' ' ' ' ',
        1 'MAKT'             'MAKTX'      'TG_PRODUTO2' 'MAKTX'       'Texto Breve' '30' ' ' ' ' ' ' ' ',
        1 ' '                ' '          'TG_PRODUTO2' 'LGORT'       'Depósito'    '10' ' ' ' ' ' ' ' ',
        1 'ZMMT0037'         'NETPR'      'TG_PRODUTO2' 'NETPR'       'Preço Liq.'  '15' ' ' ' ' ' ' ' ',
        1 'ZMMT0037'         'PEINH'      'TG_PRODUTO2' 'PEINH'       'Por'         '10' ' ' ' ' ' ' ' ',
        1 'ZMMT0037'         'BPRME'      'TG_PRODUTO2' 'BPRME'       'UPP'         '05' ' ' ' ' ' ' ' ',
        1 'ZMMT0037'         'MENGE'      'TG_PRODUTO2' 'MENGE'       'Qtde.Pedido' '15' ' ' 'X' ' ' ' ',
        1 'ZMMT0037'         'MENGE'      'TG_PRODUTO2' 'MENGE_TRO'   'Qtde.dispon' '15' ' ' 'X' ' ' ' ',
        1 'ZMMT0037'         'NETPR'      'TG_PRODUTO2' 'QTDE_TROCA'  'Qntde Troca' '15' 'X' 'X' ' ' ' ',
        1 'ZMMT0037'         'NETPR'      'TG_PRODUTO2' 'VLR_TROCA'   'Valor Troca' '15' ' ' ' ' ' ' ' ',
        1 'ZMMT0037'         'MWSKZ'      'TG_PRODUTO2' 'MWSKZ'       'IVA'         '10' ' ' ' ' ' ' ' ',
        1 'ZMMT0037'         'NETPR'      'TG_PRODUTO2' 'WMWST'       'Val.IVA'     '15' ' ' ' ' ' ' ' ',
        1 'ZMMT0037'         'NETPR_DESC' 'TG_PRODUTO2' 'NETPR_DESC'  'Desconto'    '15' 'X' ' ' ' ' ' ', "PBI - 60968
        1 'ZMMT0037'         'NETPR_SUPL' 'TG_PRODUTO2' 'NETPR_SUPL'  'Suplemento'  '15' 'X' ' ' ' ' ' '. "PBI - 60968


  IF p_tipo = 'Z'.
    LOOP AT t_fieldcatalog INTO  w_fieldcatalog.
      w_fieldcatalog-edit = ' '.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT_PROD

FORM montar_layout_prods USING p_tipo.

  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZMMT0037'         'EBELP'      'TG_PRODUTO_SEL' 'EBELP'       'Item'        '10' ' ' ' ' ' ' ' '.
  IF p_tipo = 'T'.
    PERFORM montar_estrutura USING:
            1 'ZMMT0037'         'WERKS'      'TG_PRODUTO_SEL' 'WERKS'       'Centro'      '12' 'X' ' ' ' ' ' '.
  ELSE.
    PERFORM montar_estrutura USING:
            1 'ZMMT0037'         'WERKS'      'TG_PRODUTO_SEL' 'WERKS'       'Centro'      '12' ' ' ' ' ' ' ' '.
  ENDIF.
  IF p_tipo = 'T'.
    PERFORM montar_estrutura USING:
          1 'MARA'             'MATNR'      'TG_PRODUTO_SEL' 'MATNR'       'Material'    '15' 'X' ' ' ' ' ' '.
  ELSE.
    PERFORM montar_estrutura USING:
          1 'MARA'             'MATNR'      'TG_PRODUTO_SEL' 'MATNR'       'Material'    '15' ' ' ' ' ' ' ' '.
  ENDIF.

  PERFORM montar_estrutura USING:
        1 'MAKT'             'MAKTX'      'TG_PRODUTO_SEL' 'MAKTX'       'Texto Breve' '20' ' ' ' ' ' ' ' '.
*        1 ' '                ' '          'TG_PRODUTO_SEL' 'LGORT'       'Depósito'    '07' ' ' ' ' ' ' ' '.

  IF p_tipo = 'T' OR p_tipo = 'P'.
    PERFORM montar_estrutura USING:
          1 'ZMMT0037'         'MENGE'      'TG_PRODUTO_SEL' 'MENGE'       'Qtde.Pedido' '15' 'X' 'X' ' ' ' '.
  ELSE.
    PERFORM montar_estrutura USING:
          1 'ZMMT0037'         'MENGE'      'TG_PRODUTO_SEL' 'MENGE'       'Qtde.Pedido' '15' ' ' 'X' ' ' ' '.
  ENDIF.

  PERFORM montar_estrutura USING:
      1 'ZMMT0037'         'MEINS'      'TG_PRODUTO_SEL' 'MEINS'       'Unid.'       '10' ' ' ' ' ' ' ' ',
      1 'ZMMT0037'         'BPRME'      'TG_PRODUTO_SEL' 'BPRME'       'UPP'         '05' 'X' ' ' ' ' ' '.


  IF p_tipo = 'T' OR p_tipo = 'P'.
    IF wg_cadlan-bsart = 'ZSEM'. "sementes
      PERFORM montar_estrutura USING:
         1 'ZMMT0037'         'NETPR'      'TG_PRODUTO_SEL' 'NETPR'       'Preço Liq.'  '15' ' ' ' ' ' ' ' ',
         1 'ZMMT0037'         'NETPR_ROYA' 'TG_PRODUTO_SEL' 'NETPR_ROYA'  'Royalties'   '15' 'X' ' ' ' ' ' ',
         1 'ZMMT0037'         'NETPR_GERM' 'TG_PRODUTO_SEL' 'NETPR_GERM'  'Germoplasma' '15' 'X' ' ' ' ' ' '.
    ELSE.
      PERFORM montar_estrutura USING:
        1 'ZMMT0037'         'NETPR'      'TG_PRODUTO_SEL' 'NETPR'       'Preço Liq.'  '15' 'X' ' ' ' ' ' '.
    ENDIF.
  ELSE.
    IF wg_cadlan-bsart = 'ZSEM'. "sementes
      PERFORM montar_estrutura USING:
         1 'ZMMT0037'         'NETPR'      'TG_PRODUTO_SEL' 'NETPR'       'Preço Liq.'  '15' ' ' ' ' ' ' ' ',
         1 'ZMMT0037'         'NETPR_ROYA' 'TG_PRODUTO_SEL' 'NETPR_ROYA'  'Royalties'   '15' ' ' ' ' ' ' ' ',
         1 'ZMMT0037'         'NETPR_GERM' 'TG_PRODUTO_SEL' 'NETPR_GERM'  'Germoplasma' '15' ' ' ' ' ' ' ' '.
    ELSE.
      PERFORM montar_estrutura USING:
        1 'ZMMT0037'         'NETPR'      'TG_PRODUTO_SEL' 'NETPR'       'Preço Liq.'  '15' ' ' ' ' ' ' ' '.
    ENDIF.
  ENDIF.

  PERFORM montar_estrutura USING:
     1 'ZMMT0037'         'NETPR'       'TG_PRODUTO_SEL' 'NETPR_FINAL' 'Valor Final' '15' ' ' 'X' ' ' ' '.
  IF p_tipo = 'T'.
    PERFORM montar_estrutura USING:
     1 'ZMMT0037'         'MWSKZ'      'TG_PRODUTO_SEL' 'MWSKZ'       'IVA'         '10' 'X' ' ' ' ' ' ',
     1 'ZMMT0037'         'NETPR'      'TG_PRODUTO_SEL' 'WMWST'       'Val.IVA'     '15' ' ' ' ' ' ' ' ',
     1 'ZMMT0037'         'NETPR_DESC' 'TG_PRODUTO_SEL' 'NETPR_DESC'  'Desconto'    '15' 'X' ' ' ' ' ' ',
     1 'ZMMT0037'         'NETPR_SUPL' 'TG_PRODUTO_SEL' 'NETPR_SUPL'  'Suplemento'  '15' 'X' ' ' ' ' ' ',
     1 'ZMMT0037'         'NETPR'      'TG_PRODUTO_SEL' 'NETPR_TOT'   'Total'       '15' ' ' ' ' ' ' ' '. "Bug - 63565
  ELSE.
    PERFORM montar_estrutura USING:
     1 'ZMMT0037'         'MWSKZ'      'TG_PRODUTO_SEL' 'MWSKZ'       'IVA'         '10' ' ' ' ' ' ' ' ',
     1 'ZMMT0037'         'NETPR'      'TG_PRODUTO_SEL' 'WMWST'       'Val.IVA'     '15' ' ' ' ' ' ' ' ',
     1 'ZMMT0037'         'NETPR_DESC' 'TG_PRODUTO_SEL' 'NETPR_DESC'  'Desconto'    '15' 'X' ' ' ' ' ' ',
     1 'ZMMT0037'         'NETPR_SUPL' 'TG_PRODUTO_SEL' 'NETPR_SUPL'  'Suplemento'  '15' 'X' ' ' ' ' ' '.
  ENDIF.

  IF p_tipo = 'Z'.
    PERFORM montar_estrutura USING:
     1 'ZMMT0037'         'LGORT'           'TG_PRODUTO_SEL' 'LGORT'         'Depósito'     '10' 'X' ' ' ' ' ' ',
     1 'EKPO'             'ID_LOTE_FRETE'   'TG_PRODUTO_SEL' 'ID_LOTE_FRETE' 'Lote'         '15' 'X' ' ' ' ' ' ',
     1 'TPART'            'PARVW'           'TG_PRODUTO_SEL' 'PARVW1'        'Função'       '10' ' ' ' ' ' ' ' ',
     1 'LFA1'             'LIFNR'           'TG_PRODUTO_SEL' 'LIFNR1'        'Parceiro'     '15' 'X' ' ' ' ' ' '.
*    1 'TPART'            'PARVW'           'TG_PRODUTO_SEL' 'PARVW2'        'Função'       '10' ' ' ' ' ' ' ' ',
*    1 'LFA1'             'LIFNR'           'TG_PRODUTO_SEL' 'LIFNR2'        'Parceiro'     '15' 'X' ' ' ' ' ' '.
  ENDIF.

  IF p_tipo = 'Z'.
    READ TABLE tg_produto2 INTO DATA(wg_produto2) INDEX 1.
*** BUG 52329 - Inicio
    IF wg_produto2-change = 'X'.
      LOOP AT t_fieldcatalog INTO  w_fieldcatalog.
        IF  w_fieldcatalog-fieldname = 'MENGE'.
          w_fieldcatalog-edit = 'X'.
        ELSE.
          w_fieldcatalog-edit = ' '.
        ENDIF.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
*** BUG 52329 - Fim
    ELSE.
      LOOP AT t_fieldcatalog INTO  w_fieldcatalog.
        IF w_fieldcatalog-fieldname = 'LGORT'         OR
           w_fieldcatalog-fieldname = 'ID_LOTE_FRETE' OR
           w_fieldcatalog-fieldname = 'LIFNR1'        OR
           w_fieldcatalog-fieldname = 'LIFNR2'        OR
           w_fieldcatalog-fieldname = 'MENGE'.
          w_fieldcatalog-edit = 'X'.
        ELSE.
          w_fieldcatalog-edit = ' '.
        ENDIF.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT_PROD
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_PROG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_prog .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZMMT0038'         'EBELP'      'TG_PROGRAMA' 'EBELP'       'Item'         '10' ' ' ' ' ' ' ' ',
        1 'MAKT'             'MATNR'      'TG_PROGRAMA' 'MATNR'       'Material'     '15' 'X' ' ' ' ' ' ',
        1 'MAKT'             'MAKTX'      'TG_PROGRAMA' 'MAKTX'       'Texto Breve'  '30' ' ' ' ' ' ' ' ',
        1 'ZMMT0038'         'DATA_PROGR' 'TG_PROGRAMA' 'DATA_PROGR'  'Data Entrega' '15' 'X' ' ' ' ' ' ',
        1 'ZMMT0038'         'MENGE'      'TG_PROGRAMA' 'MENGE'       'Quantidade'   '15' 'X' 'X' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_PROG

FORM montar_layout_parc .

  DATA: altera(1).
  CLEAR altera.
  IF wg_acao = 'MODIF'.
    altera = 'X'.
  ENDIF.

  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'TPART'        'PARVW'      'TG_PARCEIRO' 'PARVW'       'Função'           '07' altera ' ' ' ' ' ',
        1 'TPART'        'VTEXT'      'TG_PARCEIRO' 'VTEXT'       'Denominação'      '15' ' '    ' ' ' ' ' ',
        1 'LFA1'         'LIFNR'      'TG_PARCEIRO' 'LIFNR'       'Parceiro'         '10' altera ' ' ' ' ' ',
        1 'LFA1'         'NAME1'      'TG_PARCEIRO' 'NAME1'       'Nome'             '40' ' '    ' ' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_PROG

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_COP_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_cop_item .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        "1 'ZMMT0037'         'EBELP'      'TG_COP_ITEM' 'EBELP'       'Item'        '10' ' ' ' ' ' ' ' ',
        1 'MARA'             'MATNR'      'TG_COP_ITEM' 'MATNR'       'Material'    '15' ' ' ' ' ' ' ' ',
        1 'MAKT'             'MAKTX'      'TG_COP_ITEM' 'MAKTX'       'Desc. Material' '30' ' ' ' ' ' ' ' ',
        1 ' '                ' '          'TG_COP_ITEM' 'LGORT'       'Depósito'    '10' ' ' ' ' ' ' ' ',
        1 'ZMMT0037'         'MENGE'      'TG_COP_ITEM' 'MENGE'       'Qtde Pedido'  '15' ' ' ' ' ' ' ' ',
        1 'ZMMT0037'         'MENGE'      'TG_COP_ITEM' 'MEINS'       'Unid.Ped'    '15' ' ' '' ' ' ' '.
  IF wg_coplan-bsart = 'ZSEM'. "sementes
    PERFORM montar_estrutura USING:
    1 'ZMMT0037'         'NETPR'      'TG_COP_ITEM' 'BRTWR'       'Preço Bruto'  '15' ' ' ' ' ' ' ' ',
    1 'ZMMT0037'         'NETPR_ROYA' 'TG_COP_ITEM' 'NETPR_ROYA'  'Royalties'    '15' 'X' ' ' ' ' ' ',
    1 'ZMMT0037'         'NETPR_GERM' 'TG_COP_ITEM' 'NETPR_GERM'  'Germoplasma'  '15' 'X' ' ' ' ' ' '.
  ELSE.
    PERFORM montar_estrutura USING:
    1 'ZMMT0037'         'NETPR'      'TG_COP_ITEM' 'BRTWR'       'Preço Bruto' '15' 'X' ' ' ' ' ' '.
  ENDIF.
ENDFORM.

FORM montar_layout_adto .
  REFRESH t_fieldcatalog.
  DATA: altera(1).
  CLEAR altera.
  IF wg_acao = 'MODIF'.
    altera = 'X'.
  ENDIF.
  PERFORM montar_estrutura USING:
*       1 '     '            'ICON'           'TG_ADTO'  'ICON'         'Status'           '05' ' '    ' ' ' ' ' ',
       1 'ZMMT0037'         'EBELN'          'TG_ADTO'  'EBELN'        'Pedido'           '09' ' '    ' ' ' ' ' ',
       1 'ZMMT0147'         'VLR_ADTO'       'TG_ADTO'  'SALDO'        'Saldo Adto'       '09' ' '    'X' ' ' ' ',
       1 'ZMMT0147'         'PERC_ADTO'      'TG_ADTO'  'PERC_UTIL'    '% Adto Util'      '09' ' '    ' ' ' ' ' ',
       1 'ZMMT0147'         'PERC_ADTO'      'TG_ADTO'  'PERC_ADTO'    '% Solic.Adto'     '09' altera    ' ' ' ' ' ',
       1 'ZMMT0147'         'VLR_ADTO'       'TG_ADTO'  'VLR_ADTO'     'Vlr.Adiantamento' '12' altera    ' ' ' ' ' ',
       1 'ZMMT0147'         'DT_VCTO'        'TG_ADTO'  'DT_VCTO'      'Dt.Vencimento'    '11' altera    ' ' ' ' ' ',
       1 'ZFIT0045'         'RESP_NEG'       'TG_ADTO'  'RESP_NEG'     'Responsável pela Negociação'  '18' altera    ' ' ' ' ' ',
       1 'ZFIT0045'         'DEP_RESP'       'TG_ADTO'  'DEP_RESP'     'Departamento'     '11' altera    ' ' ' ' ' '.

ENDFORM.

FORM montar_layout_pos_finan .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
       1 'BSAK'   'EBELN'     'TG_POS_FINAN'  'EBELN'     'Pedido'       '10' ' '    ' ' ' ' ' ',
       1 'BSAK'   'XBLNR'     'TG_POS_FINAN'  'XBLNR'     'Referência'   '10' ' '    ' ' ' ' ' ',
       1 'EBKE'   'BELNR'     'TG_POS_FINAN'  'BELNR_M'   'Nro.MIRO	'    '12' ' '    ' ' ' ' ' ',
       1 'BKPF'   'BELNR'     'TG_POS_FINAN'  'BELNR_C'   'Doc.Contabil' '20' ' '    ' ' ' ' ' ',
       1 'BSAK'   'BLART'     'TG_POS_FINAN'  'BLART'     'Tp.Doc.'      '20' ' '    ' ' ' ' ' ',
       1 'BSAK'   'BUDAT'     'TG_POS_FINAN'  'BUDAT'     'Dt.Lcto'      '20' ' '    ' ' ' ' ' ',
       1 'BSAK'   'AUGBL'     'TG_POS_FINAN'  'AUGBL'     'Doc.Comp.'    '20' ' '    ' ' ' ' ' ',
       1 'BSAK'   'AUGDT'     'TG_POS_FINAN'  'AUGDT'     'Dt.Comp.'     '20' ' '    ' ' ' ' ' ',
       1 'BSAK'   'DMBTR'     'TG_POS_FINAN'  'DMBTR'     'Valor BRL'    '20' ' '    'X' ' ' ' ',
       1 'BSAK'   'DMBE2'     'TG_POS_FINAN'  'DMBE2'     'Valor USD'    '20' ' '    'X' ' ' ' ',
       1 '  '     '  '        'TG_POS_FINAN'  'STATUS'    'Status'       '20' ' '    ' ' ' ' ' '.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_IMP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_strip_imp_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab_strip_imp-tab1.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab1.
    WHEN c_tab_strip_imp-tab2.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab2.
    WHEN c_tab_strip_imp-tab3.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab3.
    WHEN c_tab_strip_imp-tab4.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab4.
    WHEN c_tab_strip_imp-tab5.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab5.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                 " TAB_STRIP_IMP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit INPUT.
  CASE ok-code.
    WHEN c_back.
      CLEAR: vg_chamada, wg_cadlan.
      SET SCREEN 0.

    WHEN c_exit.
      CLEAR: vg_chamada, wg_cadlan.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA w_answer.
  DATA v_nro_sol TYPE zmmt0035-nro_sol_cp.
  DATA: cursorfield(30) TYPE c,
        cursorline(30)  TYPE c,
        cursorvalue(30) TYPE c,
        e_data          TYPE dats,
        e_hora          TYPE cdhdr-utime,
        zzstatus(01)    TYPE c.


  CASE ok-code.
    WHEN 'LOG'.
* PBI - 60951 - Inicio
*        CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
*          TABLES
*            table    = t_zmmt0035_log
*          EXCEPTIONS
*            fb_error = 1
*            OTHERS   = 2.


      DATA: gt_opcoes_log TYPE spopli OCCURS 5 WITH HEADER LINE.
      DATA: l_program TYPE sy-repid.


      REFRESH gt_opcoes_log.

      l_program = sy-repid.

      gt_opcoes_log-varoption =  'CABEÇALHO'.
      APPEND gt_opcoes_log.
      CLEAR  gt_opcoes_log.

      gt_opcoes_log-varoption =  'ITENS'.
      APPEND gt_opcoes_log.
      CLEAR  gt_opcoes_log.

      CLEAR: vl_resposta.

      " "Verifica se o pedido esta aprovado.
      CLEAR: zzstatus.
      CALL FUNCTION 'ZF_CHECK_APRO_PEDIDO_ZMM0149'
        EXPORTING
          i_ebeln    = wg_cadlan-ebeln
        IMPORTING
          e_data     = e_data
          e_hora     = e_hora
          e_aprovado = zzstatus.

      IF zzstatus IS NOT INITIAL AND wg_cadlan-ebeln IS NOT INITIAL.

        CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
          EXPORTING
            mark_max           = 0
            start_col          = 15
            start_row          = 3
            textline1          = 'Selecione o Log Desejado:'
            titel              = 'Log de Modificações'
          IMPORTING
            answer             = vl_resposta
          TABLES
            t_spopli           = gt_opcoes_log
          EXCEPTIONS
            not_enough_answers = 1
            too_much_answers   = 2
            too_much_marks     = 3
            OTHERS             = 4.

        IF vl_resposta = '1'.

          SELECT  *
           FROM zmmt0035_log
           INTO TABLE t_zmmt0035_log
           WHERE nro_sol_cp = wg_cadlan-nro_sol_cp AND data_atual >= e_data.

          DELETE t_zmmt0035_log WHERE data_atual = e_data AND hora_atual < e_hora.
          SORT t_zmmt0035_log DESCENDING BY data_atual hora_atual.


        ELSE.


          SELECT  *
             FROM zmmt0037_log
             INTO TABLE t_zmmt0037_log
             WHERE nro_sol_cp = wg_cadlan-nro_sol_cp AND data_atual >= e_data.
          DELETE t_zmmt0037_log WHERE data_atual = e_data AND hora_atual < e_hora.
          SORT t_zmmt0037_log DESCENDING BY data_atual hora_atual.

        ENDIF.

        CALL SCREEN 0901  STARTING AT 10 10
                        ENDING   AT 220 50.
* PBI - 60951 - Fim

      ELSE.
*    MESSAGE i024(sd) WITH 'Pedido não esta aprovado'
      ENDIF.

    WHEN 'PICK'.
      GET CURSOR FIELD cursorfield LINE cursorline VALUE cursorvalue.
      IF wg_cadlan-ebeln IS NOT INITIAL
         AND NOT cursorvalue IS INITIAL
         AND cursorfield = 'WG_CADLAN-EBELN'.
        SET PARAMETER ID 'BES' FIELD cursorvalue.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN c_deldoc.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
*         DIAGNOSE_OBJECT       = ' '
          text_question         = 'Confirma a exclusão da Solicitação?'
          text_button_1         = 'Sim'(001)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Não'(002)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
*         USERDEFINED_F1_HELP   = ' '
          start_column          = 25
          start_row             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          answer                = w_answer
*       TABLES
*         PARAMETER             =
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      .
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF w_answer = '1'.
        PERFORM eliminar_solicitacao.
      ENDIF.
    WHEN c_search.
      PERFORM busca_dados.
    WHEN c_gera_req.

      DELETE tg_fatura WHERE  dt_vcto IS INITIAL.
      DELETE tg_produto WHERE  matnr IS INITIAL.
      DELETE tg_programa WHERE  matnr IS INITIAL.

      CALL METHOD grid1->check_changed_data.
      PERFORM verifica_erros.

      IF tg_msg_ret[] IS NOT INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ELSE.
        PERFORM cria_req.
      ENDIF.

    WHEN 'CAB_SON'.
      PERFORM  altera_cab.
    WHEN c_gera_ped.

      DELETE tg_fatura WHERE  dt_vcto IS INITIAL.
      DELETE tg_produto WHERE  matnr IS INITIAL.
      DELETE tg_programa WHERE  matnr IS INITIAL.

      CALL METHOD grid1->check_changed_data.
      PERFORM verifica_erros.

      IF tg_msg_ret[] IS NOT INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ELSE.
        "IF WG_CADLAN-BANFN IS INITIAL.
        "  MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E13.
        IF wg_cadlan-bsart IS INITIAL.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH text-e15.
        ELSE.
          PERFORM cria_ped.
        ENDIF.
      ENDIF.

    WHEN c_save.
      DELETE tg_fatura   WHERE  dt_vcto IS INITIAL.
      DELETE tg_produto  WHERE  matnr   IS INITIAL.
      DELETE tg_programa WHERE  matnr   IS INITIAL.

      CALL METHOD grid1->check_changed_data.

      PERFORM verifica_erros.

      IF tg_msg_ret[] IS INITIAL.
        CLEAR wg_acao.
        PERFORM grava_dados.
        REFRESH: tg_fields.
        PERFORM trata_campos USING space
                                 'GR2'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM trata_campos USING space
                                  'GR1'
                                   c_1       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 1.

      ELSE.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.

    WHEN c_back.
      CLEAR wg_acao.
    WHEN c_add.
      wg_acao = c_modif.
      PERFORM limpa_campos.
      PERFORM obtem_proximo.
      REFRESH: tg_fields.
      PERFORM trata_campos USING space
                                 'GR2'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM trata_campos USING space
                                'GR1'
                                 c_0       "INPUT 1     NO INPUT 0
                                 c_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.
      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 0.

    WHEN c_displa.
      wg_acao = c_displa.
      v_nro_sol = wg_cadlan-nro_sol_cp.
      PERFORM limpa_campos.
      wg_cadlan-nro_sol_cp = v_nro_sol.
      PERFORM busca_dados.
      REFRESH: tg_fields.
      PERFORM trata_campos USING space
                           'GR2'
                              c_0       "INPUT 1     NO INPUT 0
                              c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM trata_campos USING space
                                'GR1'
                                 c_1       "INPUT 1     NO INPUT 0
                                 c_0.      "INVISIBLE 1 VISIBLE 0
    WHEN c_cancel.
      CLEAR wg_acao.
    WHEN c_atuali.

    WHEN c_modif.
      IF wg_acao = c_modif.
        CLEAR wg_acao.
        REFRESH: tg_fields.
        PERFORM trata_campos USING space
                                 'GR2'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM trata_campos USING space
                                  'GR1'
                                   c_1       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0

        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 1.
      ELSE.
        wg_acao = c_modif.

        tg_produto_old[] = tg_produto[].
        wg_cadlan_old    = wg_cadlan.

        PERFORM trata_campos USING space
                                   'GR2'
                                      c_1       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM trata_campos USING space
                                  'GR1'
                                   c_0       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0
        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 0.

      ENDIF.
    WHEN c_show_msgre.
      "CLEAR wg_acao.
      PERFORM verifica_erros.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = c_x
          i_repid       = sy-repid
          i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
          i_set_field   = 'X_FIELD'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.

    WHEN c_exit.

      LEAVE PROGRAM.
*** PBI 609049 - Inicio CSB
    WHEN 'ANEXO'.
      PERFORM f_importar_anexos.
    WHEN 'COPY'.
      CALL SCREEN 0800  STARTING AT 010 3 "
                  ENDING   AT 140 018.
    WHEN 'CRIA_ADTO'.

      CLEAR: t_msg,
             w_msg,
             w_msg1,
             w_mode,
             t_bdcdata,
             fs_bdcdata.

      PERFORM populate_bdcdata USING ''
                                     ''
                                     ''
                                     ''
                                     ''.

      w_mode = 'E'.
      CALL TRANSACTION 'ZFI0025' USING t_bdcdata  MODE w_mode
       UPDATE 'S' MESSAGES INTO t_msg.

*      REFRESH tg_adto.
*      PERFORM busca_dados.
*
*      CALL METHOD grid8->refresh_table_display
*        EXPORTING
*          is_stable = wa_stable.

    WHEN 'POS_FINAN'.
      PERFORM f_listar_posicao_finan.
*** PBI 609049 - Fim CSB
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  POPULATE_BDCDATA
*&---------------------------------------------------------------------*
FORM populate_bdcdata USING p_adto-ebeln
                            p_adto-resp_neg
                            p_adto-dep_resp
                            p_adto-dt_vcto
                            p_adto-nro_sol_cp.

  DATA: data TYPE char10.

  "Data de vencimento.
  data = |{ p_adto-dt_vcto+6(02) }.{ p_adto-dt_vcto+4(02) }.{ p_adto-dt_vcto(04) }|.

  PERFORM :
    fill_bdc_data USING 'ZFIR0031' '0100' 'X'  ' '  ' ',
    fill_bdc_data USING  ''  ''  ''   'BDC_CURSOR' 'WG_CADLAN-NRO_SOL',
    fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE'   '=ADD',

    fill_bdc_data USING 'ZFIR0031' '0100' 'X'  ' '  ' ',
    fill_bdc_data USING  ''  ''  ''   'BDC_CURSOR'  'WG_CADLAN-EBELN',
    fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE'   '=SEARCH',
    fill_bdc_data USING  ''  ''  ''   'WG_CADLAN-EBELN'  p_adto-ebeln,
    fill_bdc_data USING  ''  ''  ''   'WG_CADLAN-HBKID'  'BBRA',

    fill_bdc_data USING 'ZFIR0031' '0100' 'X'  ' '  ' ',
    fill_bdc_data USING  ''  ''  ''   'BDC_CURSOR'  'WG_CADLAN-DEP_RESP',
    fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE'   '=SEARCH',
    fill_bdc_data USING  ''  ''  ''   'WG_CADLAN-DT_PGTO'  data,
    fill_bdc_data USING  ''  ''  ''   'WG_CADLAN-RESP_NEG' p_adto-resp_neg,
    fill_bdc_data USING  ''  ''  ''   'WG_CADLAN-DEP_RESP' p_adto-dep_resp,
    fill_bdc_data USING  ''  ''  ''   'WG_CADLAN-NRO_SOL_CP' p_adto-nro_sol_cp.

ENDFORM.                               " Form populate_bdc.
*&---------------------------------------------------------------------*
*&      Form  FILL BDC DATA
*&---------------------------------------------------------------------*
FORM fill_bdc_data USING VALUE(p_program)
                      VALUE(p_dynpro)
                      VALUE(p_dynbegin)
                      VALUE(p_fnam)
                      VALUE(p_fval).
  CLEAR fs_bdcdata .
  IF p_dynbegin = 'X' .
    fs_bdcdata-program = p_program .
    fs_bdcdata-dynpro  = p_dynpro .
    fs_bdcdata-dynbegin = p_dynbegin .
    APPEND fs_bdcdata TO t_bdcdata.
  ELSE.
    fs_bdcdata-fnam = p_fnam.
    fs_bdcdata-fval = p_fval.
    CONDENSE fs_bdcdata-fval.
    APPEND fs_bdcdata TO t_bdcdata.
  ENDIF.                               " IF p_dynbeg..

ENDFORM .                              " Fill_entry


*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
FORM trata_campos  USING    p_field
                            p_group1
                            p_value
                            p_invisible.

  tg_fields-campo     = p_field.
  tg_fields-group1    = p_group1.
  tg_fields-value     = p_value.
  tg_fields-invisible = p_invisible.
  APPEND tg_fields.

ENDFORM.                    " TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .

  DATA: wl_zmmt0035  TYPE zmmt0035,
        tl_zmmt0147  TYPE TABLE OF zmmt0147 WITH HEADER LINE,
        tl_zfit0046  TYPE TABLE OF zfit0046 WITH HEADER LINE,
        tl_zfit0045  TYPE TABLE OF zfit0045 WITH HEADER LINE,
        tl_bsak      TYPE TABLE OF bsak     WITH HEADER LINE,
        tl_zmmt0036  TYPE TABLE OF zmmt0036 WITH HEADER LINE,
        tl_zmmt0037  TYPE TABLE OF zmmt0037 WITH HEADER LINE,
        tl_zmmt0038  TYPE TABLE OF zmmt0038 WITH HEADER LINE,
        tl_zmmt0106  TYPE TABLE OF zmmt0106 WITH HEADER LINE,
        wl_makt      TYPE makt,
        wl_lfa1      TYPE lfa1,
        wl_t001w     TYPE t001w,
        wl_t052u     TYPE t052u,
        wl_t024      TYPE t024,
        wl_t161t     TYPE t161t,
        wl_cont      TYPE sy-tabix,
        wl_cont_aux  TYPE sy-tabix,
        wl_cont_aux2 TYPE sy-tabix,
        v_lifnr      TYPE lfa1-lifnr,
        v_ebeln      TYPE ekko-ebeln,
        v_menge      TYPE ekpo-menge.

  DATA it_ekbe_soma  TYPE TABLE OF ty_ekbe.
  DATA it_ekbe       TYPE TABLE OF ty_ekbe.
  DATA v_diff        TYPE zmmt0037-netpr.

  CLEAR: wg_cadlan_mem.


  IF wg_cadlan-nro_sol_cp IS NOT INITIAL  AND ( wg_acao EQ c_displa OR wg_acao = 'RES' ).
    IF  wg_acao = 'RES'.
      wg_acao = c_modif.
    ENDIF.
    SELECT SINGLE *
     FROM zmmt0035
     INTO  wl_zmmt0035
      WHERE  nro_sol_cp EQ wg_cadlan-nro_sol_cp.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº de Solicitação não encontrada!'.
      LEAVE TO SCREEN 100.
    ELSEIF wl_zmmt0035-loekz IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº de Solicitação foi eliminada!'.
      LEAVE TO SCREEN 100.
    ELSE.
      MOVE-CORRESPONDING wl_zmmt0035 TO wg_cadlan.
      "

      CLEAR: wl_lfa1, wl_t001w, wl_t052u,wl_t024.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wg_cadlan-lifnr
        IMPORTING
          output = v_lifnr.
      SELECT SINGLE *
        FROM lfa1
        INTO wl_lfa1
        WHERE lifnr = v_lifnr.

      IF sy-subrc = 0.
        wg_cadlan-name1_l  = wl_lfa1-name1.
      ENDIF.

      SELECT SINGLE *
        FROM t001w
        INTO wl_t001w
        WHERE werks = wg_cadlan-werks.
      IF sy-subrc = 0.
        wg_cadlan-name1_w  = wl_t001w-name1.
      ENDIF.

      SELECT SINGLE *
        FROM t052u
        INTO wl_t052u
        WHERE spras = 'P'
        AND zterm = wg_cadlan-zterm.
      IF sy-subrc = 0.
        wg_cadlan-text1    = wl_t052u-text1.
      ENDIF.

      SELECT SINGLE *
        FROM t024
        INTO wl_t024
        WHERE ekgrp = wg_cadlan-ekgrp.
      IF sy-subrc = 0.
        wg_cadlan-eknam   = wl_t024-eknam.
      ENDIF.

      SELECT SINGLE *
        FROM t161t
        INTO wl_t161t
        WHERE spras = 'P'
        AND   bsart = wg_cadlan-bsart
        AND   bstyp = 'F'.

      IF sy-subrc = 0.
        wg_cadlan-batxt   = wl_t161t-batxt.
      ENDIF.

      wl_name =  wl_zmmt0035-nro_sol_cp.

      REFRESH: tg_editor, lt_lines.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id        = 'ZMM1'
          language  = sy-langu
          name      = wl_name
          object    = 'ZMM0149'
        TABLES
          lines     = lt_lines
        EXCEPTIONS
          id        = 1
          language  = 2
          name      = 3
          not_found = 4
          OTHERS    = 5.


      IF lt_lines[] IS NOT INITIAL.

        LOOP AT lt_lines INTO lst_lines.
          MOVE lst_lines-tdline TO wg_editor.
          APPEND wg_editor TO tg_editor.
        ENDLOOP.

      ELSE.

        REFRESH: tg_editor.
        CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
        wl_cont = strlen( wl_zmmt0035-texto_neg ).
        wl_cont_aux = wl_cont / 72.
        DO.
          MOVE: wl_zmmt0035-texto_neg+wl_cont_aux2 TO wg_editor-line.
          "ADD 72 TO WL_CONT_AUX2.
          ADD 72 TO wl_cont_aux2.
          APPEND wg_editor TO tg_editor.

          IF wl_cont_aux2 GT wl_cont.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.


      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.
      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 1.

      " Condições pagamento
      REFRESH: tl_zmmt0036.
      SELECT *
         FROM zmmt0036
         INTO TABLE tl_zmmt0036
         WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.

      REFRESH: tg_fatura.
      CLEAR tg_fatura.
      LOOP AT tl_zmmt0036.
        MOVE-CORRESPONDING tl_zmmt0036 TO tg_fatura.
        APPEND tg_fatura.
      ENDLOOP.

      " produtos
      REFRESH: tl_zmmt0037.
      SELECT *
      FROM zmmt0037
      INTO TABLE tl_zmmt0037
      WHERE nro_sol_cp = wg_cadlan-nro_sol_cp
      ORDER BY ebeln ebelp.

      REFRESH: tg_produto.
      CLEAR tg_produto.
      LOOP AT tl_zmmt0037.
        MOVE-CORRESPONDING tl_zmmt0037 TO tg_produto.
        MOVE-CORRESPONDING tg_produto TO tg_produto3. "compara se pedido e tabela estão iguais
        tg_produto-menge_ori = tg_produto-menge.
        tg_produto-loekz = icon_create.
        v_ebeln = wg_cadlan-ebeln.
        IF tg_produto-ebeln IS NOT INITIAL.
          v_ebeln =  tg_produto-ebeln.
        ENDIF.
        REFRESH style.
        SELECT SINGLE *
          FROM ekpo
          INTO @DATA(wa_ekpo)
          WHERE ebeln = @v_ebeln
          AND   ebelp = @tg_produto-ebelp.
        IF sy-subrc NE 0.
          CLEAR wa_ekpo.
        ELSEIF wa_ekpo-elikz = 'X'.
          CLEAR tg_produto-menge_ori.
        ENDIF.

        IF sy-subrc = 0.
          tg_produto-ebeln = tl_zmmt0037-ebeln.
          SELECT SINGLE *
              FROM ekbe
              INTO @DATA(wa_ekbe_)
              WHERE ebeln = @v_ebeln
              AND   ebelp = @tg_produto-ebelp.
*          IF sy-subrc EQ 0 OR v_ebeln NE wg_cadlan-ebeln OR wa_ekpo-loekz = 'L'.
          IF v_ebeln NE wg_cadlan-ebeln OR wa_ekpo-loekz = 'L'.
            CLEAR tg_produto-loekz.
            v_menge = tg_produto-menge.

            MOVE-CORRESPONDING wa_ekpo TO tg_produto.

            IF v_menge NE tg_produto-menge.
              tg_produto-loekz = icon_create.
              tg_produto-menge = v_menge.
            ENDIF.
            IF v_ebeln EQ wg_cadlan-ebeln.
              CLEAR tg_produto-ebeln.
            ENDIF.

            wa_style-fieldname = 'EBELP'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'WERKS'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'MATNR'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'LGORT'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            IF v_ebeln NE wg_cadlan-ebeln.
              wa_style-fieldname = 'MENGE'.
              wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
              INSERT  wa_style INTO TABLE style .
            ENDIF.

            wa_style-fieldname = 'NETPR'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'NETPR_ROYA'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'NETPR_GERM'.

            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'PEINH'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'BPRME'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'MWSKZ'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
            "
            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'NETPR_DESC'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .

            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'NETPR_SUPL'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .

            INSERT  wa_style INTO TABLE style .
            wa_style-fieldname = 'NETPR_FRETE'.
            wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
            INSERT  wa_style INTO TABLE style .
          ENDIF.
          "
        ELSEIF wg_cadlan-ebeln IS NOT INITIAL.
          tg_produto-loekz = icon_create.
        ENDIF.

        REFRESH: it_ekbe, it_ekbe_soma.
        SELECT *
           FROM ekbe
             INTO CORRESPONDING FIELDS OF TABLE it_ekbe "MIGO
            WHERE ebeln EQ wa_ekpo-ebeln
            AND   ebelp EQ wa_ekpo-ebelp
            AND   bewtp EQ 'E'.

        "MIGO
        LOOP AT it_ekbe INTO DATA(wa_ekbe).
          IF wa_ekbe-shkzg = 'H'.
            MULTIPLY  wa_ekbe-menge BY -1.
            MULTIPLY  wa_ekbe-dmbtr BY -1.
          ENDIF.
          CLEAR wa_ekbe-shkzg.
          COLLECT wa_ekbe INTO it_ekbe_soma.
        ENDLOOP.

        SORT it_ekbe_soma  BY ebeln ebelp.

        READ TABLE it_ekbe_soma INTO wa_ekbe WITH KEY ebeln = wa_ekpo-ebeln
                                                 ebelp = wa_ekpo-ebelp BINARY SEARCH.
        IF sy-subrc = 0.
          tg_produto-menge_migo =  wa_ekbe-menge.
          SUBTRACT wa_ekbe-menge FROM wa_ekpo-menge.
          tg_produto-menge_ori = wa_ekpo-menge. "Disponivel agora nesse campo
        ENDIF.

        SELECT SINGLE *
          FROM zmmt0037_log
          INTO @DATA(wzmmt0037_log1)
          WHERE nro_sol_cp = @wg_cadlan-nro_sol_cp
          AND   ebeln      = @tg_produto-ebeln
          AND   ebelp      = @tg_produto-ebelp
          AND   tipo       = 'A'.
        IF sy-subrc = 0.
          tg_produto-loekz = icon_warning.
        ENDIF.

        SELECT SINGLE *
         FROM zmmt0037_log
         INTO @DATA(wzmmt0037_log2)
         WHERE nro_sol_cp = @wg_cadlan-nro_sol_cp
         AND   ebeln      = @tg_produto-ebeln
         AND   ebelp      = @tg_produto-ebelp
         AND   tipo       = 'E'.
        IF sy-subrc = 0.
          tg_produto-loekz = icon_system_undo.
        ELSE.
          IF wa_ekpo-loekz = 'L'.
            tg_produto-menge = 0.
            tg_produto-loekz = icon_delete.
          ELSEIF wa_ekpo-loekz = 'S'.
            tg_produto-menge = 0.
            tg_produto-loekz = icon_locked.
          ENDIF.
        ENDIF.

*        " "Verifica se o pedido esta aprovado.
*        CLEAR: zzstatus.
*        CALL FUNCTION 'ZF_CHECK_APRO_PEDIDO_ZMM0149'
*          EXPORTING
*            i_ebeln    = wg_cadlan-ebeln
*          IMPORTING
*            e_data     = e_data
*            e_aprovado = zzstatus.
*
**** PBI - 60951 - Inicio
*        SELECT SINGLE *
*           FROM zmmt0035_log
*           INTO @DATA(wzmmt0035_log)
*           WHERE nro_sol_cp = @wg_cadlan-nro_sol_cp AND data_atual >= @e_data.
**** PBI - 60951 - Fim

        SELECT SINGLE *
          FROM makt
          INTO wl_makt
          WHERE matnr = tg_produto-matnr
          AND spras   = 'P'.

        tg_produto-maktx = wl_makt-maktx.
        IF tg_produto-peinh GT 0.
          DATA(fator) = 1000.
          IF tg_produto-bprme NE 'TO'.
            fator = 1.
          ENDIF.
          IF wa_ekpo-loekz EQ 'L'.
            tg_produto-valor = 0.
          ELSE.
            tg_produto-valor = ( tg_produto-menge * ( ( tg_produto-netpr ) / tg_produto-peinh ) ) / fator.
          ENDIF.
        ELSE.
          tg_produto-valor = 0.
        ENDIF.

        SELECT SINGLE *
          FROM ekko
          INTO @DATA(wa_ekko)
          WHERE ebeln = @v_ebeln.
        " Imposto item

        IF sy-subrc NE 0.
          PERFORM r_imposto_item USING wa_ekko-lifnr
                                        wa_ekpo-werks
                                        tg_produto3-ebelp
                                        v_ebeln
                                        tg_produto3-matnr
                                        tg_produto3-menge
                                        tg_produto3-netpr
                                        tg_produto3-mwskz
                                        tg_produto3-peinh
                                        tg_produto3-bprme
                                        tg_produto3-netpr_desc
                                        tg_produto3-netpr_supl

                          CHANGING   w_valor
                                     w_wmwst.
          tg_produto-netpr_final  = w_valor.
          tg_produto-wmwst        = w_wmwst.
        ELSEIF wa_ekpo-loekz NE 'L'.
          CLEAR: w_valor, w_wmwst.
          "calcula valor final tabela 'Z'
          IF v_ebeln    EQ wg_cadlan-ebeln.
            PERFORM r_imposto_item USING wa_ekko-lifnr
                                         wa_ekpo-werks
                                         tg_produto3-ebelp
                                         v_ebeln
                                         tg_produto-matnr
                                         tg_produto-menge
                                         tg_produto-netpr
                                         tg_produto-mwskz
                                         tg_produto-peinh
                                         tg_produto-bprme
                                         tg_produto-netpr_desc
                                         tg_produto-netpr_supl

                           CHANGING   w_valor
                                      w_wmwst.
            tg_produto3-valor        = w_valor.
          ENDIF.

          CLEAR: w_valor, w_wmwst.
          PERFORM r_imposto_item USING wa_ekko-lifnr
                                       wa_ekpo-werks
                                       wa_ekpo-ebelp
                                       wa_ekpo-ebeln
                                       wa_ekpo-matnr
                                       wa_ekpo-menge
                                       0
                                       wa_ekpo-mwskz
                                       wa_ekpo-peinh
                                       wa_ekpo-bprme
                                       0
                                       0
                          CHANGING   w_valor
                                     w_wmwst.
          tg_produto-valor        = w_valor.
          tg_produto-wmwst        = w_wmwst.
          IF tg_produto-valor = 0 AND tg_produto-peinh GT 0.
            tg_produto-valor = ( tg_produto-menge * ( ( tg_produto-netpr ) / tg_produto-peinh ) ) / fator.
          ENDIF.
          "
          tg_produto3-netpr_final =  tg_produto3-valor. " - TG_PRODUTO3-NETPR_DESC + TG_PRODUTO3-NETPR_SUPL.
          tg_produto-netpr_final  =  tg_produto-valor. " - TG_PRODUTO-NETPR_DESC - WA_KONV-KBETR + TG_PRODUTO-NETPR_SUPL - WA_KONV2-KBETR.
          v_diff = tg_produto3-netpr_final - tg_produto-netpr_final.
          IF abs( v_diff ) LT '0.02'.
            tg_produto3-netpr_final = tg_produto-netpr_final.
          ENDIF.
          IF v_ebeln    EQ wg_cadlan-ebeln AND wa_ekpo-loekz IS INITIAL AND
            tg_produto-netpr_final NE tg_produto3-netpr_final.
            tg_produto-netpr_final = tg_produto3-netpr_final.
            tg_produto-loekz = icon_modification_original. "modificou o pedido mãe / gerar novamente.
          ENDIF.
*          CLEAR: WA_KONV, WA_KONV2.
        ENDIF.

        tg_produto-bsart   = wa_ekko-bsart.
        tg_produto-style[] = style[].
        IF tl_zmmt0037-brtwr = 0 AND tg_produto-menge > 0 AND tg_produto-valor > 0.
          tg_produto-brtwr   = tg_produto-valor / tg_produto-menge.
        ELSE.
          tg_produto-brtwr   = tl_zmmt0037-brtwr.
        ENDIF.
        APPEND tg_produto.
        CLEAR tg_produto.
      ENDLOOP.
      SORT tg_produto BY ebeln ebelp.
      LOOP AT tg_produto.
        IF tg_produto-loekz = icon_modification_original. "modificou o pedido mãe / gerar novamente.
          MESSAGE 'Valor diferente do pedido <Gerar> pedido novamente' TYPE 'I'.
          EXIT.
        ENDIF.
      ENDLOOP.
      " programação entrega
      SELECT *
      FROM zmmt0038
      INTO TABLE tl_zmmt0038
      WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.

      REFRESH: tg_programa.
      CLEAR tg_programa.
      LOOP AT tl_zmmt0038.
        MOVE-CORRESPONDING tl_zmmt0038 TO tg_programa.
        SELECT SINGLE *
          FROM makt
          INTO wl_makt
          WHERE matnr = tg_programa-matnr
          AND spras   = 'P'.

        tg_programa-maktx = wl_makt-maktx.
        APPEND tg_programa.
      ENDLOOP.

      " parceiros
      SELECT *
      FROM zmmt0106
      INTO TABLE tl_zmmt0106
      WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.

      REFRESH: tg_parceiro.
      CLEAR tg_parceiro.
      LOOP AT tl_zmmt0106.
        MOVE-CORRESPONDING tl_zmmt0106 TO tg_parceiro.
        SELECT SINGLE name1
          FROM lfa1
          INTO @DATA(wname1)
          WHERE lifnr = @tg_parceiro-lifnr.

        SELECT SINGLE vtext
          FROM tpart
          INTO @DATA(wvtext)
          WHERE spras = @sy-langu
            AND parvw = @tg_parceiro-parvw.

        tg_parceiro-name1 = wname1.
        tg_parceiro-vtext = wvtext.
        APPEND tg_parceiro.
      ENDLOOP.


    ENDIF.
  ELSEIF wg_cadlan-nro_sol_cp IS NOT INITIAL.
    CLEAR: wl_lfa1, wl_t001w, wl_t052u,wl_t024.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_cadlan-lifnr
      IMPORTING
        output = v_lifnr.
    SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
      WHERE lifnr = v_lifnr.

    IF sy-subrc = 0.
      wg_cadlan-name1_l  = wl_lfa1-name1.
    ENDIF.

    SELECT SINGLE *
      FROM t001w
      INTO wl_t001w
      WHERE werks = wg_cadlan-werks.
    IF sy-subrc = 0.
      wg_cadlan-name1_w  = wl_t001w-name1.
    ENDIF.

    SELECT SINGLE *
      FROM t052u
      INTO wl_t052u
      WHERE spras = 'P'
      AND zterm = wg_cadlan-zterm.
    IF sy-subrc = 0.
      wg_cadlan-text1    = wl_t052u-text1.
    ENDIF.

    SELECT SINGLE *
      FROM t024
      INTO wl_t024
      WHERE ekgrp = wg_cadlan-ekgrp.
    IF sy-subrc = 0.
      wg_cadlan-eknam   = wl_t024-eknam.
    ENDIF.

    SELECT SINGLE *
      FROM t161t
      INTO wl_t161t
      WHERE spras = 'P'
      AND   bsart = wg_cadlan-bsart
      AND   bstyp = 'F'.

    IF sy-subrc = 0.
      wg_cadlan-batxt   = wl_t161t-batxt.
    ENDIF.
  ENDIF.


*** pbi 60949 - inicio - csb

  CLEAR:  tg_zmmt0037_aux,
          tg_zmmt0035_aux,
          tl_zmmt0147,
          tg_zmmt0147,
          tg_zmmt0147_aux,
          tl_zfit0046,
          tl_zfit0045,
          tl_bsak.

  REFRESH: tg_zmmt0037_aux[].

  IF tl_zmmt0037 IS NOT INITIAL.

    tg_zmmt0037_aux-nro_sol_cp = tl_zmmt0037-nro_sol_cp.
    tg_zmmt0037_aux-ebeln      = tl_zmmt0037-ebeln.
    tg_zmmt0037_aux-ebelp      = tl_zmmt0037-ebelp.
    "tg_zmmt0037_aux-matnr      = tl_zmmt0037-matnr.
    tg_zmmt0037_aux-menge      = tl_zmmt0037-menge.
    tg_zmmt0037_aux-netpr      = tl_zmmt0037-netpr.
    tg_zmmt0037_aux-xvlrpedido = ( tl_zmmt0037-menge * tl_zmmt0037-netpr ).

    APPEND tg_zmmt0037_aux.

    IF wl_zmmt0035 IS NOT INITIAL.
      tg_zmmt0035_aux-nro_sol_cp =  wl_zmmt0035-nro_sol_cp.
      tg_zmmt0035_aux-ebeln      =  wl_zmmt0035-ebeln.
      "tg_zmmt0035_aux-ebelp      =  wl_zmmt0035-ebelp.
      APPEND tg_zmmt0035_aux.
    ENDIF.
  ENDIF.

*** Busca de Dados de Solicitação de Adiantamento

  CLEAR: tg_adto.
  REFRESH tg_adto[].
  FREE: tl_zfit0046,  tl_zfit0045, tl_zfit0045, tg_zmmt0147.
*
  IF wg_cadlan-nro_sol_cp IS NOT INITIAL.
    SELECT * FROM zfit0046
      INTO TABLE tl_zfit0046
        WHERE nro_sol_cp =  wg_cadlan-nro_sol_cp
        AND EXISTS ( SELECT * FROM zfit0045
                     WHERE zfit0045~nro_sol = zfit0046~nro_sol
                     AND   zfit0045~loekz EQ '' ).


    IF tl_zfit0046[] IS NOT INITIAL.
      SELECT *
      FROM zfit0045
      INTO TABLE tl_zfit0045
      FOR ALL ENTRIES IN tl_zfit0046
      WHERE nro_sol = tl_zfit0046-nro_sol.

      SELECT *
      FROM bsak
      INTO TABLE tl_bsak
      FOR ALL ENTRIES IN tl_zfit0045
      WHERE bukrs	=	tl_zfit0045-bukrs
      AND  belnr  = tl_zfit0045-belnr.

      SELECT * FROM zmmt0147
      INTO CORRESPONDING FIELDS OF TABLE tg_zmmt0147
      FOR ALL ENTRIES IN tl_zfit0045
      WHERE nro_sol_cp =  tl_zfit0045-nro_sol_cp.
    ENDIF.
  ENDIF.


  "Verificando saldo adiantamento.
  CLEAR: zvalor_adto.
  IF tl_zfit0046[] IS NOT INITIAL.
    LOOP AT tl_zfit0046 INTO DATA(w_zfit0046).
      ADD w_zfit0046-vlr_adiantamento TO zvalor_adto.
    ENDLOOP.
  ENDIF.


*** Monta saida de dados.

  CLEAR: znetpr_pai, znetpr_pai, zebeln, zvalor_produto.
  tg_produto_aux[] = tg_produto[].
  SORT tg_produto_aux[] BY ebeln.
  SORT tg_produto[] BY ebeln.
  DELETE ADJACENT DUPLICATES FROM tg_produto_aux[] COMPARING ebeln.

  IF tg_produto_aux[] IS NOT INITIAL.
    LOOP AT tg_produto_aux.
      IF tg_produto_aux-ebeln IS NOT INITIAL. "ZSON não gera adto
        CONTINUE.
      ENDIF.
      CLEAR: zvalor_produto.
      LOOP AT tg_produto.
        IF  tg_produto-ebeln EQ tg_produto_aux-ebeln.
          ADD tg_produto-valor TO zvalor_produto.
        ENDIF.
      ENDLOOP.
      tg_adto-saldo    = zvalor_produto.
      "
      CLEAR zvalor_adto.
      IF tg_produto_aux-ebeln IS INITIAL.
        tg_adto-ebeln    = wg_cadlan-ebeln.
      ELSE.
        tg_adto-ebeln    = tg_produto_aux-ebeln.
      ENDIF.
      LOOP AT tl_zfit0046 INTO w_zfit0046 WHERE ebeln = tg_adto-ebeln.
        ADD w_zfit0046-vlr_adiantamento TO zvalor_adto.
        READ TABLE tl_zfit0045 INTO DATA(wa_zfit0045) WITH KEY ebeln = tg_adto-ebeln.
        IF sy-subrc = 0.
          tg_adto-resp_neg = wa_zfit0045-resp_neg.
          tg_adto-dep_resp = wa_zfit0045-dep_resp.
        ELSE.
          CLEAR: tg_adto-resp_neg,tg_adto-dep_resp.
        ENDIF.
      ENDLOOP.
      tg_adto-saldo = tg_adto-saldo - zvalor_adto.
      tg_adto-perc_util = ( zvalor_adto / zvalor_produto ) * 100.


      tg_adto-icon     = icon_generate.

      APPEND tg_adto.
    ENDLOOP.
  ENDIF.

  IF tg_adto[] IS NOT INITIAL.
    LOOP AT tg_adto ASSIGNING FIELD-SYMBOL(<w_adto>).
*      CLEAR zvalor_adto.
*      LOOP AT tl_zfit0046 INTO w_zfit0046 WHERE ebeln = <w_adto>-ebeln.
*        ADD w_zfit0046-vlr_adiantamento TO zvalor_adto.
*      ENDLOOP.
*      <w_adto>-saldo = <w_adto>-saldo - zvalor_adto.
*      <w_adto>-perc_util = ( zvalor_adto / zvalor_produtot ) * 100.
      wa_style-fieldname = 'PERC_UTIL'.
      wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
      INSERT  wa_style INTO TABLE style .

      IF <w_adto>-saldo LE 0.
        wa_style-fieldname = 'PERC_ADTO'.
        wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'VLR_ADTO'.
        wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'DT_VCTO'.
        wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'DEP_RESP'.
        wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'RESP_NEG'.
        wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
        INSERT  wa_style INTO TABLE style.
        <w_adto>-style = style[].
      ENDIF.

    ENDLOOP.
  ENDIF.


  "Salva os dados na memoria, para depois analiar as alterações na tela.
  wg_cadlan_mem = wg_cadlan.


ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .
  DATA: wl_input_cadlan   TYPE zmmt0035,
        tl_input_zmmt0036 TYPE TABLE OF zmmt0036 WITH HEADER LINE,
        tl_input_zmmt0037 TYPE TABLE OF zmmt0037 WITH HEADER LINE,
        tl_input_zmmt0038 TYPE TABLE OF zmmt0038 WITH HEADER LINE,
        tl_input_zmmt0106 TYPE TABLE OF zmmt0106 WITH HEADER LINE.


  MOVE-CORRESPONDING wg_cadlan TO wl_input_cadlan.
  MOVE: sy-mandt TO wl_input_cadlan-mandt,
        sy-uname TO wl_input_cadlan-usnam,
        sy-datum TO wl_input_cadlan-data_atual,
        sy-uzeit TO wl_input_cadlan-hora_atual,
*        sy-datum TO wl_input_cadlan-data_criacao,
        'F'      TO wl_input_cadlan-bstyp .

  IF wl_input_cadlan-data_criacao IS INITIAL AND wl_input_cadlan-nro_sol_cp IS NOT INITIAL.
    wl_input_cadlan-data_criacao = sy-datum.
  ENDIF.

  REFRESH: tg_editor.

  IF obg_descbox IS NOT INITIAL.
    CALL METHOD obg_descbox->get_text_as_r3table
      IMPORTING
        table = tg_editor.

    LOOP AT tg_editor INTO wg_editor.
      IF sy-tabix EQ 1.
        wl_input_cadlan-texto_neg = wg_editor-line.

      ELSEIF sy-tabix GE 2.
        CONCATENATE wl_input_cadlan-texto_neg  wg_editor-line INTO wl_input_cadlan-texto_neg SEPARATED BY space.

      ENDIF.
    ENDLOOP.

    IF tg_editor[] IS NOT INITIAL.

      wl_name = wg_cadlan-nro_sol_cp.
      REFRESH lt_lines.
      LOOP AT tg_editor INTO wg_editor.
        MOVE wg_editor-line TO lst_lines-tdline.
        APPEND lst_lines TO lt_lines.
      ENDLOOP.

      wl_header-tdobject = 'ZMM0149'.
      wl_header-tdid     = 'ZMM1'.
      wl_header-tdspras  = sy-langu.
      wl_header-tdname  = wl_name.


      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          client          = sy-mandt
          header          = wl_header
          savemode_direct = 'X'
        TABLES
          lines           = lt_lines[]
        EXCEPTIONS
          id              = 1
          language        = 2
          name            = 3
          object          = 4
          OTHERS          = 5.

      CLEAR lt_lines.

      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          object          = 'ZMM0149'
          name            = wl_header-tdname
          id              = 'ZMM1'
          language        = sy-langu
          savemode_direct = ' '.

    ENDIF.
  ENDIF.

*  LOOP AT tg_adto.
*    MOVE : wg_cadlan-nro_sol_cp      TO t_zmmt0147-nro_sol_cp,
*           tg_adto-ebeln             TO t_zmmt0147-ebeln,
*           tg_adto-ebelp             TO t_zmmt0147-ebelp,
*           tg_adto-saldo             TO t_zmmt0147-sdo_adto,
*           tg_adto-perc_adto         TO t_zmmt0147-perc_adto,
*           tg_adto-vlr_adto          TO t_zmmt0147-vlr_adto,
*           tg_adto-dt_vcto           TO t_zmmt0147-dt_vcto,
*           sy-datum                  TO t_zmmt0147-dt_atual,
*           sy-uzeit                  TO t_zmmt0147-hr_atual,
**           sy-mandt                  TO tg_zmmt0147-mandt,
*           sy-uname                  TO t_zmmt0147-usnam.
*
*    APPEND t_zmmt0147.
*  ENDLOOP.

  LOOP AT tg_fatura.
    MOVE : wg_cadlan-nro_sol_cp     TO tl_input_zmmt0036-nro_sol_cp,
           tg_fatura-dt_vcto        TO tl_input_zmmt0036-dt_vcto,
           tg_fatura-percentual     TO tl_input_zmmt0036-percentual,
           tg_fatura-valor          TO tl_input_zmmt0036-valor,
           tg_fatura-menge          TO tl_input_zmmt0036-menge,
           sy-mandt                 TO tl_input_zmmt0036-mandt,
           sy-uname                 TO tl_input_zmmt0036-usnam,
           sy-datum                 TO tl_input_zmmt0036-data_atual,
           sy-uzeit                 TO tl_input_zmmt0036-hora_atual.

    APPEND tl_input_zmmt0036.
  ENDLOOP.

  LOOP AT tg_produto.
    MOVE :  sy-mandt                 TO tl_input_zmmt0037-mandt,
            wg_cadlan-nro_sol_cp     TO tl_input_zmmt0037-nro_sol_cp,
            tg_produto-ebeln         TO tl_input_zmmt0037-ebeln,
            tg_produto-ebelp         TO tl_input_zmmt0037-ebelp,
            tg_produto-matnr         TO tl_input_zmmt0037-matnr,
            tg_produto-werks         TO tl_input_zmmt0037-werks,
            tg_produto-lgort         TO tl_input_zmmt0037-lgort,
            tg_produto-charg         TO tl_input_zmmt0037-charg,
            tg_produto-menge         TO tl_input_zmmt0037-menge,
            tg_produto-meins         TO tl_input_zmmt0037-meins,
            tg_produto-brtwr         TO tl_input_zmmt0037-brtwr,
            tg_produto-bicms         TO tl_input_zmmt0037-bicms,
            tg_produto-picms         TO tl_input_zmmt0037-picms,
            tg_produto-netpr         TO tl_input_zmmt0037-netpr,
            tg_produto-netpr_desc    TO tl_input_zmmt0037-netpr_desc,
            tg_produto-netpr_supl    TO tl_input_zmmt0037-netpr_supl,
            tg_produto-netpr_germ    TO tl_input_zmmt0037-netpr_germ,
            tg_produto-netpr_roya    TO tl_input_zmmt0037-netpr_roya,
            tg_produto-netpr_frete   TO tl_input_zmmt0037-netpr_frete,
            tg_produto-peinh         TO tl_input_zmmt0037-peinh,
            tg_produto-bprme         TO tl_input_zmmt0037-bprme,
            tg_produto-mwskz         TO tl_input_zmmt0037-mwskz,
            sy-uname                 TO tl_input_zmmt0037-usnam,
            sy-datum                 TO tl_input_zmmt0037-data_atual,
            sy-uzeit                 TO tl_input_zmmt0037-hora_atual.

    APPEND tl_input_zmmt0037.
  ENDLOOP.

  LOOP AT tg_programa.
    MOVE: sy-mandt                 TO tl_input_zmmt0038-mandt,
          wg_cadlan-nro_sol_cp     TO tl_input_zmmt0038-nro_sol_cp,
          tg_programa-ebelp        TO tl_input_zmmt0038-ebelp,
          tg_programa-matnr        TO tl_input_zmmt0038-matnr,
          tg_programa-data_progr   TO tl_input_zmmt0038-data_progr,
          tg_programa-menge        TO tl_input_zmmt0038-menge,
          sy-uname                 TO tl_input_zmmt0038-usnam,
          sy-datum                 TO tl_input_zmmt0038-data_atual,
          sy-uzeit                 TO tl_input_zmmt0038-hora_atual.

    APPEND tl_input_zmmt0038.
  ENDLOOP.

  LOOP AT tg_parceiro.
    MOVE: sy-mandt                 TO tl_input_zmmt0106-mandt,
          wg_cadlan-nro_sol_cp     TO tl_input_zmmt0106-nro_sol_cp,
          tg_parceiro-lifnr        TO tl_input_zmmt0106-lifnr,
          tg_parceiro-parvw        TO tl_input_zmmt0106-parvw.

    APPEND tl_input_zmmt0106.
  ENDLOOP.

  DELETE FROM zmmt0036 WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.
  DELETE FROM zmmt0037 WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.
  DELETE FROM zmmt0038 WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.
  DELETE FROM zmmt0106 WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.

  PERFORM pf_log_sistema.


  MODIFY zmmt0035 FROM       wl_input_cadlan.
  MODIFY zmmt0036 FROM TABLE tl_input_zmmt0036.
  MODIFY zmmt0037 FROM TABLE tl_input_zmmt0037.
  MODIFY zmmt0038 FROM TABLE tl_input_zmmt0038.
  MODIFY zmmt0106 FROM TABLE tl_input_zmmt0106.
  MODIFY zmmt0147 FROM TABLE t_zmmt0147.
  COMMIT WORK.

  MESSAGE s836(sd) WITH 'Lançamento'
                         wg_cadlan-nro_sol_cp
                         ', criado/modificado com sucesso!'.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_campos .
  CLEAR: wg_cadlan , tg_editor,lt_lines,tg_parceiro,wg_mensagem,x_field.
  wg_cadlan-waers = 'BRL'.
*  CLEAR: tg_produto, tg_fatura,tg_produto,tg_programa,tg_parceiro, tg_editor.
  REFRESH: tg_fatura,tg_produto,tg_programa,tg_parceiro, tg_editor.

ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM obtem_proximo .
  DATA: vnum(10) TYPE c,
        vseq(10) TYPE p.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZID_SOV'
    IMPORTING
      number      = vseq.

  vnum = vseq .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vnum
    IMPORTING
      output = vnum.

  wg_cadlan-nro_sol_cp = vnum.

ENDFORM.                    " OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE inicializa_tela OUTPUT.
  IF sy-calld = 'X' AND  wg_cadlan-nro_sol_cp IS INITIAL.
    IF vg_chamada NE 'X'.
      GET PARAMETER ID 'SOLI_51' FIELD  wg_cadlan-nro_sol_cp.
      IF wg_cadlan-nro_sol_cp IS INITIAL.

      ELSE.
        wg_acao = c_displa.
        REFRESH: tg_fields.
        PERFORM trata_campos USING space
                                   'GR2'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM trata_campos USING space
                                  'GR1'
                                   c_1       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0
        vg_chamada = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF wg_acao IS INITIAL.
    REFRESH: tg_fields.
    PERFORM trata_campos USING space
                               'GR2'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                              'GR1'
                               c_1       "INPUT 1     NO INPUT 0
                               c_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.

ENDMODULE.                 " INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_SAFRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_safra INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE,
        vsafra        TYPE zsdt0044-safra.

  DATA: BEGIN OF tl_safra OCCURS 0,
          safra4 TYPE zsdt0044-safra,
          safra  TYPE zmmt0035-safra,
        END OF tl_safra.

  SELECT safra
    FROM zsdt0044
    INTO TABLE tl_safra
    ORDER BY safra ASCENDING.

  LOOP AT tl_safra.
    vsafra = tl_safra-safra4.
    ADD 1 TO vsafra .
    CONCATENATE tl_safra-safra4 '/' vsafra INTO tl_safra-safra.
    MODIFY tl_safra INDEX sy-tabix TRANSPORTING safra.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SAFRA'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_CADLAN-SAFRA'
      value_org       = 'S'
    TABLES
      value_tab       = tl_safra
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_SAFRA  INPUT
*&---------------------------------------------------------------------*
*&      Form  ELIMINAR_SOLICITACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eliminar_solicitacao .
  DATA: wl_zmmt0035 TYPE zmmt0035.

  SELECT  SINGLE *
    FROM zmmt0035
    INTO wl_zmmt0035
     WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.

  IF sy-subrc IS INITIAL.
    IF wl_zmmt0035-loekz IS INITIAL.
      MOVE: c_x TO wl_zmmt0035-loekz.
      MODIFY zmmt0035 FROM wl_zmmt0035.
      MESSAGE s836(sd) WITH 'O documento foi eliminado!'.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o documento'
                            'já foi marcado para eliminação!'.
    ENDIF.
  ENDIF.
ENDFORM.                    " ELIMINAR_SOLICITACAO
*&---------------------------------------------------------------------*
*&      Module  SEARCH_ZTERM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_zterm INPUT.
  DATA: tl_return_term TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselct      TYPE TABLE OF dselc      WITH HEADER LINE.


  DATA: BEGIN OF tl_term OCCURS 0,
          zterm TYPE t052-zterm,
          text1 TYPE t052u-text1,
        END OF tl_term.


  SELECT t052~zterm t052u~text1
     FROM t052
     INNER JOIN t052u ON t052u~zterm = t052~zterm
     INTO TABLE tl_term
      WHERE  t052~koart = ''
      AND    t052u~spras = 'P'
      AND    t052u~ztagg = t052~ztagg
    ORDER BY t052~zterm ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZTERM'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_CADLAN-ZTERM'
      value_org       = 'S'
    TABLES
      value_tab       = tl_term
      return_tab      = tl_return_term
      dynpfld_mapping = tl_dselct.
ENDMODULE.                 " SEARCH_ZTERM  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_F4_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_f4_cat.
  gt_f4-fieldname = 'LGORT'.
  gt_f4-register = 'X'.
  gt_f4-getbefore = 'X'.
  gt_f4-chngeafter ='X'.
  APPEND gt_f4.
  CALL METHOD grid2->register_f4_for_fields
    EXPORTING
      it_f4 = gt_f4[].

ENDFORM.                    "BUILD_F4_CAT
*&---------------------------------------------------------------------*
*&      Form  CRIA_REQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_req .

  DATA: it_requisition_items TYPE STANDARD TABLE OF bapiebanc,
        wa_requisition_items TYPE bapiebanc,
        it_return            TYPE STANDARD TABLE OF bapireturn,
        wa_return            TYPE bapireturn.

  DATA: wa_makt TYPE makt,
        wa_mara TYPE mara.

  DATA: it_editor_req TYPE STANDARD TABLE OF ty_editor,  "Tabela para extração do texto da solicitação de compra
        wa_editor_req TYPE ty_editor,
        it_lines      TYPE STANDARD TABLE OF tline,          "Tabela para gravar texto do cabeçalho
        wa_lines      TYPE tline,
        x_header      TYPE thead,                            "Tabela-parâmetro para gravar texto do cabeçalho
        wa_editor     TYPE ty_editor,                        "WA texto do cabeçalho na solicitação de compra Agro
        w_number      LIKE bapiebanc-preq_no,                "Número da Requisição
        c_e           TYPE c VALUE 'I',
        c_x           TYPE c VALUE 'X'.

  REFRESH:it_requisition_items, it_return.
  CLEAR: wa_requisition_items, wa_return.

  LOOP AT tg_produto.

    wa_requisition_items-doc_type   = 'NB'.                   "Tipo de requisição de compra (P/ Agro sempre NB)
    wa_requisition_items-preq_item  = tg_produto-ebelp.       "N Item
    wa_requisition_items-material   = tg_produto-matnr.       "N Material

    SELECT SINGLE * INTO wa_makt FROM makt WHERE matnr EQ wa_requisition_items-material AND spras EQ sy-langu.
    wa_requisition_items-short_text = wa_makt-maktx.          "Texto Breve Material

    wa_requisition_items-store_loc  = tg_produto-lgort.       "Depósito
    wa_requisition_items-quantity   = tg_produto-menge.       "Quantidade
    wa_requisition_items-pur_group  = wg_cadlan-ekgrp.        "Grupo de Comprador
    wa_requisition_items-plant = wg_cadlan-werks.             "Centro

    SELECT SINGLE * INTO wa_mara FROM mara WHERE matnr EQ wa_requisition_items-material.
    wa_requisition_items-mat_grp    = wa_mara-matkl.          "Grupo de Mercadorias
    wa_requisition_items-unit       = wa_mara-meins.          "Unidade do Material

    wa_requisition_items-deliv_date = sy-datum.               "Data da remessa
    wa_requisition_items-del_datcat = 1.                      "Tipo de data da remessa
    APPEND wa_requisition_items TO it_requisition_items.

  ENDLOOP.

  CALL FUNCTION 'BAPI_REQUISITION_CREATE'
    IMPORTING
      number            = w_number
    TABLES
      requisition_items = it_requisition_items
      "REQUISITION_ITEM_TEXT = REQUISITION_ITEM_TEXT
      return            = it_return.

  READ TABLE it_return INTO wa_return WITH KEY type = c_e.
  IF sy-subrc EQ 0.

    "Commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = c_x.

    "Grava o cabeçalho
    x_header-tdobject = 'EBANH'.
    x_header-tdname   = w_number.
    x_header-tdid     = 'B01'.
    x_header-tdspras  = sy-langu.


    IF obg_descbox IS NOT INITIAL.
      CALL METHOD obg_descbox->get_text_as_r3table
        IMPORTING
          table = it_editor_req.
    ENDIF.

    REFRESH: it_lines.
    CLEAR: wa_lines.

    LOOP AT it_editor_req INTO wa_editor_req.
      wa_lines-tdformat = '*'.
      wa_lines-tdline = wa_editor_req-line.
      APPEND wa_lines TO it_lines.
    ENDLOOP.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = x_header
        savemode_direct = 'X'
      TABLES
        lines           = it_lines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.

    MESSAGE s000(zwrm001) DISPLAY LIKE 'S' WITH 'Requisição de Compra nº' w_number 'criada.'.
    wg_cadlan-banfn = w_number.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    READ TABLE it_return INTO wa_return INDEX 1.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH wa_return-message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CRIA_PED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_ped .

  DATA:
    it_editor_ped             TYPE STANDARD TABLE OF ty_editor,  "Tabela para extração do texto da solicitação de compra
    wa_editor_ped             TYPE ty_editor,
    it_return_ped             TYPE STANDARD TABLE OF bapiret2, "TABLE OF BAPIRET2 WITH HEADER LINE,
    wa_return_ped             TYPE bapiret2,
    it_poitem_ped             TYPE STANDARD TABLE OF bapimepoitem, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    wa_poitem_ped             TYPE bapimepoitem,
    it_poitemx_ped            TYPE STANDARD TABLE OF bapimepoitemx,
    wa_poitemx_ped            TYPE bapimepoitemx,
    wa_poheader_ped           TYPE bapimepoheader,
    wa_poheaderx_ped          TYPE bapimepoheaderx,
    it_popartner_ped          TYPE STANDARD TABLE OF bapiekkop, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    wa_popartner_ped          TYPE bapiekkop, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    it_bapimepotextheader_ped TYPE STANDARD TABLE OF bapimepotextheader,
    wa_bapimepotextheader_ped TYPE bapimepotextheader,
    it_pocond                 TYPE STANDARD TABLE OF bapimepocond,
    wa_pocond                 TYPE bapimepocond,
    it_pocondx                TYPE STANDARD TABLE OF bapimepocondx,
    wa_pocondx                TYPE bapimepocondx,
    purchaseorder             LIKE bapimepoheader-po_number,
    v_ekorg                   TYPE t024w-ekorg,
    v_branch                  TYPE t001w-j_1bbranch,
    v_bukrs                   TYPE j_1bbranch-bukrs,
    v_lifnr                   TYPE lfa1-lifnr,
    v_ebeln                   TYPE ekko-ebeln,
    vnetpr_final              TYPE zmmt0037-netpr,
    w_zmmt0035                TYPE zmmt0035,
    w_answer(1),
    v_altera_pgt(1),
    v_altera(1).

  SELECT SINGLE *
    FROM zmmt0035
    INTO w_zmmt0035
    WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.

  IF sy-subrc NE 0.
    MESSAGE 'Grave a solicitação antes, para gerar o pedido!' TYPE 'I'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = 'Deseja criar/alterar pedido ?'
      text_button_1         = 'Sim'(100)
      icon_button_1         = 'ICON_OKAY '
      text_button_2         = 'Não'(101)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = w_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF w_answer = '2'. "não
    EXIT.
  ENDIF.

  CLEAR: v_altera, v_altera_pgt.
  IF wg_cadlan-ebeln IS NOT INITIAL.
    LOOP AT tg_produto.
      IF tg_produto-loekz = icon_create OR
         tg_produto-loekz = icon_modification_original.
        v_ebeln = wg_cadlan-ebeln.
        IF tg_produto-ebeln IS NOT INITIAL.
          v_ebeln =  tg_produto-ebeln.
        ELSE.
          CLEAR v_ebeln.
        ENDIF.
        SELECT SINGLE *
          FROM zmmt0037
          INTO @DATA(wzmmt0037)
          WHERE nro_sol_cp = @wg_cadlan-nro_sol_cp
          AND   ebelp      = @tg_produto-ebelp
          AND   ebeln      = @v_ebeln.
        IF sy-subrc = 0.
          v_altera = 'X'.
        ELSE.
          CLEAR v_altera.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF v_altera IS INITIAL.
      MESSAGE 'Pedido já gerado para esta solicitação ou não gravado!' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.

  SELECT SINGLE zterm FROM ekko INTO @DATA(_zterm) WHERE ebeln = @wg_cadlan-ebeln.
  IF wg_cadlan-zterm NE _zterm.
    v_altera_pgt = 'X'.
  ENDIF.

* --- Gera Pedido sem Requisição
  IF tg_produto[] IS NOT INITIAL.

    REFRESH: it_return_ped, it_poitem_ped, it_poitemx_ped,
             it_bapimepotextheader_ped, it_pocond,it_pocondx.
    CLEAR: wa_return_ped, wa_poitem_ped, wa_poitemx_ped, wa_pocond,wa_pocondx,
           wa_poheader_ped, wa_poheaderx_ped, wa_bapimepotextheader_ped.

    "Itens--------------
    LOOP AT tg_produto.
      IF tg_produto-valor = 0.
        CONTINUE.
      ENDIF.
      IF v_altera = 'X'.
        IF ( tg_produto-loekz NE icon_create                   AND
             tg_produto-loekz NE icon_modification_original  ) OR
            tg_produto-ebeln IS NOT INITIAL. "Pedido filho não modifica
          CONTINUE.
        ENDIF.
      ENDIF.
      CLEAR: wa_poitem_ped, wa_poitemx_ped, wa_pocond,wa_pocondx.

      IF 'ZFTE_ZSEM' CS wg_cadlan-bsart.
        wa_poitem_ped-conf_ctrl = '0003'.
        wa_poitemx_ped-conf_ctrl = 'X'.
      ENDIF.
      wa_poitem_ped-po_item       = tg_produto-ebelp.                          "Item
      wa_poitem_ped-material      = tg_produto-matnr.                          "Material
      wa_poitem_ped-quantity      = tg_produto-menge.                          "Quantidade
      wa_poitem_ped-po_price      = 1.                                         "Transferência do preço: 1 = bruto, 2 = líquido
      wa_poitem_ped-price_unit    = tg_produto-peinh.                          "Unidade de preço
      wa_poitem_ped-orderpr_un    = tg_produto-bprme.                          "Unidade do preço do pedido
      wa_poitem_ped-net_price     = tg_produto-netpr.                          "Preço
      wa_poitem_ped-tax_code      = tg_produto-mwskz.                          "Código do Imposto
      IF tg_produto-werks IS NOT INITIAL.
        wa_poitem_ped-plant         = tg_produto-werks.                           "Centro
      ELSE.
        wa_poitem_ped-plant         = wg_cadlan-werks.                           "Centro
      ENDIF.
      wa_poitem_ped-stge_loc      = tg_produto-lgort.                          "Depósito
      wa_poitem_ped-batch         = tg_produto-charg.                          "lote
      APPEND wa_poitem_ped TO it_poitem_ped.
      wa_poitemx_ped-po_item      = tg_produto-ebelp.                          "Item
      wa_poitemx_ped-po_itemx     = 'X'.                                       "Item
      wa_poitemx_ped-material     = 'X'.                                       "Material
      wa_poitemx_ped-quantity     = 'X'.                                       "Quantidade
      wa_poitemx_ped-po_price     = 'X'.                                       "Transferência do preço: 1 = bruto, 2 = líquido
      wa_poitemx_ped-price_unit   = 'X'.                                       "Unidade de preço
      wa_poitemx_ped-orderpr_un   = 'X'.                                       "Unidade do preço do pedido
      wa_poitemx_ped-net_price    = 'X'.                                       "Preço
      wa_poitemx_ped-tax_code     = 'X'.                                       "Código do Imposto
      wa_poitemx_ped-plant        = 'X'.                                       "Centro
      wa_poitemx_ped-stge_loc     = 'X'.                                       "Depósito
      wa_poitemx_ped-batch        = 'X'.                                       "lote
      APPEND wa_poitemx_ped TO it_poitemx_ped.
      "
      sy-subrc = 4.
      IF wg_cadlan-ebeln IS NOT INITIAL.
        SELECT SINGLE knumv
            FROM ekko
           INTO @DATA(v_knumv)
           WHERE ebeln =  @wg_cadlan-ebeln.
        "desconto
        SELECT SINGLE *
          FROM konv
          INTO @DATA(wa_konv)
          WHERE knumv = @v_knumv
          AND   kposn = @tg_produto-ebelp
          AND   kschl = 'RB00'.
      ENDIF.
      IF tg_produto-netpr_desc GT 0 OR sy-subrc = 0.
        wa_pocond-itm_number = tg_produto-ebelp.
        wa_pocond-cond_type = 'RB00'.
        wa_pocond-cond_value = tg_produto-netpr_desc.
        wa_pocond-cond_unit = tg_produto-bprme.
        wa_pocond-currency  = wg_cadlan-waers.
        IF sy-subrc NE 0.
          wa_pocond-change_id = 'I'.
        ELSE.
          wa_pocond-cond_value = tg_produto-netpr_desc * -1.
          wa_pocond-change_id = 'U'.
        ENDIF.
        APPEND wa_pocond TO it_pocond.
        "
        wa_pocondx-itm_number = tg_produto-ebelp.
        wa_pocondx-cond_type  = 'X'.
        wa_pocondx-cond_value = 'X'.
        wa_pocondx-cond_unit  = 'X'.
        wa_pocondx-currency   = 'X'.
        wa_pocondx-change_id  = 'X'.
        APPEND wa_pocondx TO it_pocondx.
      ENDIF.
      "
      "suplemento
      sy-subrc = 4.
      IF wg_cadlan-ebeln IS NOT INITIAL.
        SELECT SINGLE *
          FROM konv
          INTO wa_konv
          WHERE knumv = v_knumv
          AND   kposn = tg_produto-ebelp
          AND   kschl = 'ZB00 '.
      ENDIF.
      IF tg_produto-netpr_supl GT 0 OR sy-subrc = 0.
        wa_pocond-itm_number = tg_produto-ebelp.
        wa_pocond-cond_type = 'ZB00 '.
        wa_pocond-cond_value = tg_produto-netpr_supl.
        wa_pocond-cond_unit = tg_produto-bprme.
        wa_pocond-currency  = wg_cadlan-waers.
        IF sy-subrc NE 0.
          wa_pocond-change_id = 'I'.
        ELSE.
          wa_pocond-change_id = 'U'.
        ENDIF.
        APPEND wa_pocond TO it_pocond.
        "
        wa_pocondx-itm_number = tg_produto-ebelp.
        wa_pocondx-cond_type  = 'X'.
        wa_pocondx-cond_value = 'X'.
        wa_pocondx-cond_unit  = 'X'.
        wa_pocondx-currency   = 'X'.
        wa_pocondx-change_id  = 'X'.
        APPEND wa_pocondx TO it_pocondx.
      ENDIF.
    ENDLOOP.

    "Cabeçalho----------
    SELECT SINGLE j_1bbranch
      FROM t001w
      INTO v_branch
      WHERE werks EQ wg_cadlan-werks.

    SELECT SINGLE bukrs
      FROM j_1bbranch
      INTO v_bukrs
      WHERE branch EQ v_branch.

    wa_poheader_ped-comp_code = v_bukrs.                  "Empresa
    wa_poheader_ped-doc_type = wg_cadlan-bsart.           "Tipo de Pedido

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_cadlan-lifnr
      IMPORTING
        output = v_lifnr.

    wa_poheader_ped-vendor   = v_lifnr.                   "Fornecedor pela ZMM0045

    SELECT SINGLE ekorg
      FROM t024w
      INTO v_ekorg
      WHERE werks EQ wg_cadlan-werks.

    wa_poheader_ped-purch_org = v_ekorg.                  "Organização de Compras
    wa_poheader_ped-doc_date  = sy-datum.                 "Data do Pedido
    wa_poheader_ped-langu     = sy-langu.                 "Idioma
    wa_poheader_ped-pur_group = wg_cadlan-ekgrp.          "Grupo de Compradores
    wa_poheader_ped-currency  = wg_cadlan-waers.          "Moeda pela ZMM0045
    IF wg_cadlan-wkurs IS NOT INITIAL.
      wa_poheader_ped-exch_rate = wg_cadlan-wkurs.        "Taxa de Câmbio pela ZMM0045
    ELSE.
      wa_poheader_ped-exch_rate = 1.                      "Taxa de Câmbio pela ZMM0045
    ENDIF.
    wa_poheader_ped-our_ref   = wg_cadlan-safra.          "Safra
    wa_poheader_ped-quot_date = wg_cadlan-ihran.          "data cotação
    wa_poheader_ped-ref_1     = wg_cadlan-ped_forn.

    wa_poheader_ped-pmnttrms    = wg_cadlan-zterm.
    wa_poheader_ped-incoterms1  = wg_cadlan-inco1.
    wa_poheader_ped-incoterms2  = wg_cadlan-inco2.
    wa_poheader_ped-collect_no  = wg_cadlan-nro_sol_cp.   "Nº Solicitação


    wa_poheaderx_ped-comp_code  = 'X'.                     "Empresa
    wa_poheaderx_ped-doc_type   = 'X'.                     "Tipo de Pedido
    wa_poheaderx_ped-vendor     = 'X'.                     "Fornecedor pela ZMM0045
    wa_poheaderx_ped-purch_org  = 'X'.                     "Organização de Compras
    wa_poheaderx_ped-doc_date   = 'X'.                     "Data do Pedido
    wa_poheaderx_ped-langu      = 'X'.                     "Idioma
    wa_poheaderx_ped-pur_group  = 'X'.                     "Grupo de Compradores
    wa_poheaderx_ped-currency   = 'X'.                     "Moeda pela ZMM0045
    wa_poheaderx_ped-exch_rate  = 'X'.                     "Taxa pela ZMM0045
    wa_poheaderx_ped-our_ref    = 'X'.                     "Safra
    wa_poheaderx_ped-quot_date  = 'X'.                     "Data Cotação
    wa_poheaderx_ped-ref_1      = 'X'.
    wa_poheaderx_ped-pmnttrms   = 'X'.
    wa_poheaderx_ped-incoterms1 = 'X'.
    wa_poheaderx_ped-incoterms2 = 'X'.
    wa_poheaderx_ped-collect_no = 'X'.                      "Nº Solicitação

    "Texto Cabeçalho----
    IF obg_descbox IS NOT INITIAL.
      CALL METHOD obg_descbox->get_text_as_r3table
        IMPORTING
          table = it_editor_ped.
    ENDIF.

    LOOP AT it_editor_ped INTO wa_editor_ped.
      wa_bapimepotextheader_ped-text_id = 'F01'.
      "WA_BAPIMEPOTEXTHEADER_PED-TEXT_FORM = '*'.
      wa_bapimepotextheader_ped-text_line = wa_editor_ped-line.
      APPEND wa_bapimepotextheader_ped TO it_bapimepotextheader_ped.
    ENDLOOP.

*---parceiros
    LOOP AT tg_parceiro.
      wa_popartner_ped-partnerdesc = tg_parceiro-parvw.
      wa_popartner_ped-langu       = sy-langu.
      wa_popartner_ped-buspartno   = tg_parceiro-lifnr.
      APPEND wa_popartner_ped     TO it_popartner_ped.
    ENDLOOP.

    IF v_altera IS INITIAL.
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader         = wa_poheader_ped
          poheaderx        = wa_poheaderx_ped
        IMPORTING
          exppurchaseorder = purchaseorder
        TABLES
          return           = it_return_ped
          poitem           = it_poitem_ped
          poitemx          = it_poitemx_ped
          pocond           = it_pocond
          pocondx          = it_pocondx
          popartner        = it_popartner_ped
          potextheader     = it_bapimepotextheader_ped.

      READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'S' id = '06' number = '017'.
    ELSE.
      CLEAR vnetpr_final.
      LOOP AT tg_produto INTO DATA(wg_produto).
        ADD wg_produto-netpr_final TO  vnetpr_final.
      ENDLOOP.
      SELECT SINGLE * "PROCSTAT, RLWRT
        INTO @DATA(v_ekko)
        FROM ekko
      WHERE ebeln = @wg_cadlan-ebeln.
      IF  v_ekko-procstat = '05' AND v_ekko-rlwrt LT vnetpr_final. "valor todas pedido mãe + filhos
        UPDATE ekko SET rlwrt = 0
          WHERE ebeln = wg_cadlan-ebeln.
        "
        COMMIT WORK.
      ENDIF.
      "
      CALL FUNCTION 'BAPI_PO_CHANGE'
        EXPORTING
          purchaseorder = wg_cadlan-ebeln
          poheader      = wa_poheader_ped
          poheaderx     = wa_poheaderx_ped
        TABLES
          return        = it_return_ped
          poitem        = it_poitem_ped
          poitemx       = it_poitemx_ped
          pocond        = it_pocond
          pocondx       = it_pocondx
          popartner     = it_popartner_ped
          potextheader  = it_bapimepotextheader_ped.
      READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'E' .
      IF sy-subrc = 0.
        sy-subrc = 4.
      ELSE.
        sy-subrc = 0.
      ENDIF.
    ENDIF.
    IF sy-subrc EQ 0.
      "Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      MESSAGE s000(zwrm001) DISPLAY LIKE 'S' WITH wa_return_ped-message.
      IF v_altera IS INITIAL.
        wg_cadlan-ebeln = purchaseorder.
        UPDATE zmmt0035 SET ebeln = wg_cadlan-ebeln
           WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.
      ELSE.
        PERFORM grava_dados.
      ENDIF.
      "Altera Filho Condição Pagamento
      IF  v_altera_pgt = 'X'.
        "Pega pedidos filhos ZSON
        SELECT DISTINCT zmmt0037~ebeln
          FROM zmmt0037
          INNER JOIN ekko ON ekko~ebeln = zmmt0037~ebeln
          AND ekko~bsart = 'ZSON' " pedido filho
          INTO TABLE @DATA(it_zmmt0037)
          WHERE zmmt0037~nro_sol_cp EQ @wg_cadlan-nro_sol_cp
          AND   zmmt0037~ebeln      NE ' '.

*      "Somente Condição de pagamento
        CLEAR wa_poheaderx_ped.
        wa_poheaderx_ped-pmnttrms   = 'X'.
        LOOP AT it_zmmt0037 INTO DATA(w_0037).
          CALL FUNCTION 'BAPI_PO_CHANGE'
            EXPORTING
              purchaseorder = w_0037-ebeln
              poheader      = wa_poheader_ped
              poheaderx     = wa_poheaderx_ped
            TABLES
              return        = it_return_ped.

          READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'E' .
          IF sy-subrc = 0.
            sy-subrc = 4.
          ELSE.
            sy-subrc = 0.
          ENDIF.

          IF sy-subrc EQ 0.
            "Commit
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      "
      CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
        TABLES
          table    = it_return_ped
        EXCEPTIONS
          fb_error = 1
          OTHERS   = 2.
    ENDIF.

  ELSE.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH text-e14.
  ENDIF.
ENDFORM.

FORM altera_cab.
  DATA:
    it_editor_ped             TYPE STANDARD TABLE OF ty_editor,  "Tabela para extração do texto da solicitação de compra
    wa_editor_ped             TYPE ty_editor,
    it_return_ped             TYPE STANDARD TABLE OF bapiret2, "TABLE OF BAPIRET2 WITH HEADER LINE,
    wa_return_ped             TYPE bapiret2,

    it_bapimepotextheader_ped TYPE STANDARD TABLE OF bapimepotextheader,
    wa_bapimepotextheader_ped TYPE bapimepotextheader.

  IF wg_cadlan-ebeln_son IS INITIAL.
    EXIT.
  ENDIF.
  "Texto Cabeçalho----
  IF obg_descbox IS NOT INITIAL.
    CALL METHOD obg_descbox->get_text_as_r3table
      IMPORTING
        table = it_editor_ped.
  ENDIF.

  LOOP AT it_editor_ped INTO wa_editor_ped.
    wa_bapimepotextheader_ped-text_id = 'F01'.
    "WA_BAPIMEPOTEXTHEADER_PED-TEXT_FORM = '*'.
    wa_bapimepotextheader_ped-text_line = wa_editor_ped-line.
    APPEND wa_bapimepotextheader_ped TO it_bapimepotextheader_ped.
  ENDLOOP.

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = wg_cadlan-ebeln_son
    TABLES
      return        = it_return_ped
      potextheader  = it_bapimepotextheader_ped.

  READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'E' .
  IF sy-subrc = 0.
    sy-subrc = 4.
  ELSE.
    sy-subrc = 0.
  ENDIF.

  IF sy-subrc EQ 0.
    "Commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    MESSAGE s836(sd) WITH 'Pedido'
                          wg_cadlan-ebeln_son
                          ', alterado texto com sucesso!'.
    CLEAR wg_cadlan-ebeln_son.
  ENDIF.


ENDFORM.

FORM altera_ped USING p_tipo CHANGING p_erro.

  DATA:
    tabix                     TYPE sy-tabix,
    it_editor_ped             TYPE STANDARD TABLE OF ty_editor,  "Tabela para extração do texto da solicitação de compra
    wa_editor_ped             TYPE ty_editor,
    it_return_ped             TYPE STANDARD TABLE OF bapiret2, "TABLE OF BAPIRET2 WITH HEADER LINE,
    it_return_ped_bloq        TYPE STANDARD TABLE OF bapiret2, "TABLE OF BAPIRET2 WITH HEADER LINE,
    wa_return_ped             TYPE bapiret2,
    it_poitem_ped             TYPE STANDARD TABLE OF bapimepoitem, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    it_poitem_ped_bloq        TYPE STANDARD TABLE OF bapimepoitem, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    wa_poitem_ped             TYPE bapimepoitem,
    it_poitemx_ped            TYPE STANDARD TABLE OF bapimepoitemx,
    it_poitemx_ped_bloq       TYPE STANDARD TABLE OF bapimepoitemx,
    wa_poitemx_ped            TYPE bapimepoitemx,
    it_popartner_ped          TYPE STANDARD TABLE OF bapiekkop, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    wa_popartner_ped          TYPE bapiekkop, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    it_pocond                 TYPE STANDARD TABLE OF bapimepocond,
    it_pocond_bloq            TYPE STANDARD TABLE OF bapimepocond,
    wa_pocond                 TYPE bapimepocond,
    it_pocondx                TYPE STANDARD TABLE OF bapimepocondx,
    it_pocondx_bloq           TYPE STANDARD TABLE OF bapimepocondx,
    wa_pocondx                TYPE bapimepocondx,

    wa_poheader_ped           TYPE bapimepoheader,
    wa_poheaderx_ped          TYPE bapimepoheaderx,
    it_bapimepotextheader_ped TYPE STANDARD TABLE OF bapimepotextheader,
    wa_bapimepotextheader_ped TYPE bapimepotextheader,
    purchaseorder             LIKE bapimepoheader-po_number,
    purchaseorder_son         LIKE bapimepoheader-po_number,
    v_ekorg                   TYPE t024w-ekorg,
    v_branch                  TYPE t001w-j_1bbranch,
    v_bukrs                   TYPE j_1bbranch-bukrs,
    v_lifnr                   TYPE lfa1-lifnr,
    w_zmmt0035                TYPE zmmt0035,
    t_0037                    TYPE TABLE OF zmmt0037,
    t_ekko                    TYPE TABLE OF ekko,
    t_ekpo                    TYPE TABLE OF ekpo,
    t_ekpo_zfte               TYPE TABLE OF ekpo,
    w_ekko                    TYPE ekko,
    w_ekpo                    TYPE ekpo,
    w_ekpo_zfte               TYPE ekpo,
    w_0037                    TYPE zmmt0037,
    l_total_zimp              TYPE ekpo-netwr,
    l_total_zfte              TYPE ekpo-netwr,
    l_total_sel               TYPE ekpo-netwr,
    tl_input_zmmt0037         TYPE TABLE OF zmmt0037     WITH HEADER LINE,
    tl_input_zmmt0037_log     TYPE TABLE OF zmmt0037_log WITH HEADER LINE,
    v_ebeln                   TYPE ekpo-ebeln,
    v_ebeln_ori               TYPE ekpo-ebeln,
    v_ebeln_eli               TYPE ekpo-ebeln,
    v_ebelp                   TYPE ekpo-ebelp,
    v_menge                   TYPE ekpo-menge,
    v_diff                    TYPE zmmt0037-netpr,
    v_netpr                   TYPE zmmt0037-netpr,
    v_netpr_desc              TYPE zmmt0037-netpr,
    v_netpr_supl              TYPE zmmt0037-netpr,
    vl_toler                  TYPE zmmt0113-vl_toler,
    v_rlwrt                   TYPE ekko-rlwrt,
    v_trans_total(1),
    v_erro(1),
    wa_style                  TYPE lvc_s_styl,
    style                     TYPE lvc_t_styl WITH HEADER LINE.

  CLEAR: purchaseorder.
  CLEAR: v_trans_total, v_rlwrt. "valor aprovado
  "
  CLEAR vl_toler.
  SELECT SINGLE vl_toler
    FROM zmmt0113
    INTO vl_toler
    WHERE bsart = wg_cadlan-bsart.

*-------------------------------------------------------
* pedido ZIMP
*-------------------------------------------------------
  IF p_tipo = 'Z'.

    SELECT * FROM zmmt0037
             INTO TABLE t_0037
            WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.

    IF t_0037[] IS NOT INITIAL.
      SELECT * FROM ekko
               INTO TABLE t_ekko
                FOR ALL ENTRIES IN t_0037
              WHERE ebeln = t_0037-ebeln.

      SELECT * FROM ekpo
               INTO TABLE t_ekpo
                FOR ALL ENTRIES IN t_0037
              WHERE ebeln = t_0037-ebeln
                AND ebelp = t_0037-ebelp.
    ENDIF.

    SELECT * FROM ekpo
             INTO TABLE t_ekpo_zfte
            WHERE ebeln = wg_cadlan-ebeln.

    SORT t_ekko BY ebeln.
    SORT t_ekpo BY ebeln ebelp.

    CLEAR: l_total_zimp,
           l_total_zfte.

    LOOP AT t_ekpo_zfte INTO w_ekpo_zfte.
      l_total_zfte = l_total_zfte + w_ekpo_zfte-netwr.
    ENDLOOP.

    LOOP AT t_0037 INTO w_0037.
      CLEAR: w_ekko, w_ekpo.

      READ TABLE t_ekko INTO w_ekko WITH KEY ebeln = w_0037-ebeln
                                    BINARY SEARCH.
      CHECK w_ekko-bsart = 'ZIMP'.

      READ TABLE t_ekpo INTO w_ekpo WITH KEY ebeln = w_0037-ebeln
                                             ebelp = w_0037-ebelp
                                    BINARY SEARCH.
      CHECK w_ekpo-loekz <> 'L'.

      l_total_zimp = l_total_zimp + w_ekpo-netwr.
    ENDLOOP.


*** bug 52329 - inicio
    READ TABLE tg_produto2 INTO DATA(wg_produto2) INDEX 1.
    IF wg_produto2-change = 'X'.
      LOOP AT tg_produto_sel.
        l_total_sel = l_total_sel + tg_produto_sel-valor.
      ENDLOOP.
      l_total_zimp = l_total_zimp + l_total_sel - tg_produto2-valor.
*** bug 52329 - fim
    ELSE.
      LOOP AT tg_produto_sel.
        l_total_sel = l_total_sel + tg_produto_sel-valor.
      ENDLOOP.
      l_total_zimp = l_total_zimp + l_total_sel.
    ENDIF.

    IF l_total_zimp > l_total_zfte.
      MESSAGE 'Valor Total Pedidos ZIMP ultrapassa Total Pedido ZFTE !' TYPE 'I'.
      p_erro = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

*-------------------------------------------------------

  CLEAR v_erro.

  IF p_tipo <> 'Z'.
    LOOP AT tg_produto2.
      IF tg_produto2-qtde_troca > tg_produto2-menge.
        v_erro = 'T'.
        EXIT.
      ENDIF.

      IF p_tipo NE 'R' AND p_tipo NE 'U'.
        IF  tg_produto2-qtde_troca <= 0.
          v_erro = 'Q'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF v_erro = 'T'.
    MESSAGE 'Quantidade de Troca deve ser menor ou igual a Quantidade do Pedido !' TYPE 'I'.
    p_erro = 'X'.
    EXIT.
  ENDIF.

  IF v_erro = 'Q'..
    MESSAGE 'Favor informar o Quantidade de Troca !' TYPE 'I'.
    p_erro = 'X'.
    EXIT.
  ENDIF.

* pbi - 60951 - Inicio
  IF p_tipo = 'R'.

    TYPES:
      t_thead TYPE TABLE OF thead WITH DEFAULT KEY.

    DATA: t_lines  TYPE TABLE OF tline WITH HEADER LINE,
          tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab.

    READ TABLE tg_produto_sel INDEX 1.
    READ TABLE tg_produto_sel_old INTO DATA(wg_produto_sel_old) INDEX 1.

    IF tg_produto_sel-netpr_desc <> wg_produto_sel_old-netpr_desc
      OR  tg_produto_sel-netpr_supl <> wg_produto_sel_old-netpr_supl.

      DATA(t_header) = VALUE t_thead(
      ( tdobject = 'ZMMR149' tdid = 'ZMM' tdspras = sy-langu tdname = tg_produto_sel-ebeln  ) ).

      LOOP AT t_header INTO DATA(w_header).

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client                  = sy-mandt
            id                      = w_header-tdid
            language                = w_header-tdspras
            name                    = w_header-tdname
            object                  = w_header-tdobject
          TABLES
            lines                   = t_lines[]
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF  t_lines[] IS NOT INITIAL.
          LOOP AT t_lines[] INTO DATA(w_lines).
            MOVE: w_lines-tdline TO wl_texto.
            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.
        ENDIF.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Motivo Alterações'
            im_display_mode = abap_false
          CHANGING
            ch_text         = tl_texto.

        IF tl_texto[] IS INITIAL.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH text-e24.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.
* pbi - 60951 - Fim

  READ TABLE tg_produto_sel INDEX 1.
  v_ebeln = wg_cadlan-ebeln. "pedido original
  IF tg_produto_sel-ebeln IS NOT INITIAL.
    v_ebeln = tg_produto_sel-ebeln. "pedido filho
    purchaseorder = tg_produto_sel-ebeln. "pedido filho
  ENDIF.
  SORT tg_produto BY ebeln ebelp.
  LOOP AT tg_produto WHERE  ebeln = tg_produto_sel-ebeln.

  ENDLOOP.
  v_ebelp = tg_produto-ebelp.
  SELECT SINGLE lifnr
    FROM ekko
    INTO v_lifnr "fornecedor do pedido
   WHERE ebeln = v_ebeln.

  IF ( p_tipo = 'T' AND v_lifnr NE wg_cadlan-lifnr_n ) OR p_tipo = 'P'.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = 'Troca de fornecedor, será gerado um novo pedido, confirma dados do cabeçalho atual ?'
        text_button_1         = 'Sim'(100)
        icon_button_1         = 'ICON_OKAY '
        text_button_2         = 'Não'(101)
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ' '
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = w_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF w_answer = '2'. "não
      EXIT.
    ENDIF.
  ENDIF.

  REFRESH: style.
  CLEAR: wa_style.

  IF p_tipo = 'T'.
    wa_style-fieldname = 'MWSKZ'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'WERKS'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'MATNR'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'MENGE'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'NETPR'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'NETPR_ROYA'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'NETPR_GERM'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
  ELSEIF p_tipo = 'P'  OR p_tipo = 'U'.
    wa_style-fieldname = 'MATNR'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'MENGE'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'NETPR'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'NETPR_ROYA'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
    wa_style-fieldname = 'NETPR_GERM'.
    wa_style-style     = cl_gui_alv_grid=>mc_style_disabled +  alv_style_font_bold.
    INSERT  wa_style INTO TABLE style .
  ENDIF.

  SELECT SINGLE *
    FROM zmmt0035
    INTO w_zmmt0035
    WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.

  IF sy-subrc NE 0.
    p_erro = 'X'.
    MESSAGE 'Grave a solicitação antes, para gerar o pedido!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wg_cadlan-ebeln IS INITIAL.
    p_erro = 'X'.
    MESSAGE 'Pedido mãe não criado!' TYPE 'I'.
    EXIT.
  ENDIF.
  CLEAR:  v_diff, v_erro.

  IF p_tipo = 'T' OR p_tipo = 'P' OR p_tipo = 'Z'.

    LOOP AT tg_produto_sel.
      tabix = sy-tabix.
      SELECT SINGLE *
        FROM mbew
        INTO @DATA(wl_mbew)
          WHERE  matnr EQ @tg_produto_sel-matnr
          AND    bwkey EQ @tg_produto_sel-werks.

      IF sy-subrc NE 0.
        MESSAGE text-e19 TYPE 'I'.
        v_erro = 'X'.
        EXIT.
      ENDIF.


      IF tg_produto_sel-situ    = 'X'.
        MESSAGE 'Já processado com sucesso!' TYPE 'I'.
        v_erro = 'X'.
        EXIT.
      ENDIF.
      IF  v_ebeln = wg_cadlan-ebeln.
        READ TABLE tg_produto2 WITH KEY ebelp = tg_produto_sel-ebelp.
      ELSE.
        READ TABLE tg_produto2 WITH KEY ebeln = v_ebeln
                                        ebelp = tg_produto_sel-ebelp.
      ENDIF.

      IF p_tipo <> 'Z'.
        v_diff = tg_produto_sel-netpr_final - tg_produto2-vlr_troca.
        IF  abs( v_diff ) GT vl_toler.
          MESSAGE 'Valor final do produto diferente do original!' TYPE 'I'.
          v_erro = 'X'.
          EXIT.
        ENDIF.
        IF v_diff LT 0 AND tg_produto_sel-netpr_desc GT 0.
          MESSAGE 'Valor desconto inválido, valor total menor que total troca!' TYPE 'I'.
          v_erro = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

      IF wg_cadlan-bsart = 'ZSEM'.
        v_netpr = tg_produto_sel-netpr_roya + tg_produto_sel-netpr_germ.
        IF v_netpr NE tg_produto_sel-netpr.
          MESSAGE 'Germoplasma+Royalties diferente do liquido!' TYPE 'I'.
          v_erro = 'X'.
          EXIT.
        ENDIF.
      ENDIF.

      IF tg_produto_sel-werks IS INITIAL.
        MESSAGE 'Deve ser Informado o centro para transferencia!' TYPE 'I'.
        v_erro = 'X'.
        EXIT.
      ENDIF.

      IF tg_produto_sel-situ    = 'X'.
        MESSAGE 'Já processado com sucesso!' TYPE 'I'.
        v_erro = 'X'.
        EXIT.
      ENDIF.
      "
      SELECT SINGLE bukrs
        INTO @DATA(vbukrs)
        FROM j_1bbranch
        WHERE branch = @tg_produto_sel-werks.

      SELECT SINGLE bukrs
        INTO @DATA(vbukrs2)
        FROM j_1bbranch
        WHERE branch = @wg_cadlan-werks.

      IF vbukrs NE vbukrs2.
        MESSAGE 'Centro para transferencia, deve ser da mesma empresa!' TYPE 'I'.
        v_erro = 'X'.
        EXIT.
      ENDIF.

    ENDLOOP.

  ELSE.

    LOOP AT tg_produto_sel.
      IF tg_produto_sel-situ    = 'X'.
        MESSAGE 'Já processado com sucesso!' TYPE 'I'.
        v_erro = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDIF.
  "
  IF v_erro = 'X'.
    p_erro = 'X'.
    EXIT.
  ENDIF.
  "
  IF ( p_tipo = 'T' AND v_lifnr NE wg_cadlan-lifnr_n ) OR p_tipo = 'P' OR
       p_tipo = 'Z'.
    v_ebelp = 0.
  ENDIF.
* --- Gera Pedido sem Requisição
  IF tg_produto_sel[] IS NOT INITIAL.

    REFRESH: it_return_ped, it_return_ped_bloq, it_poitem_ped_bloq,it_poitem_ped, it_poitemx_ped, it_pocond, it_pocondx, it_pocond_bloq, it_pocondx_bloq,
             it_bapimepotextheader_ped.

    "Itens--------------
    IF p_tipo = 'U'. "Desfazer
      LOOP AT tg_produto_sel.
        tg_produto_sel-situ    = 'X'.

        MODIFY tg_produto_sel INDEX sy-tabix TRANSPORTING style situ.
        CLEAR: wa_return_ped, wa_poitem_ped, wa_poitemx_ped,
               wa_poheader_ped, wa_poheaderx_ped, wa_bapimepotextheader_ped,
               l_bsart.

        SELECT *
         FROM zmmt0037_log
         INTO TABLE @DATA(it_zmmt0037_log)
         WHERE nro_sol_cp = @wg_cadlan-nro_sol_cp
         AND   ebelp      = @tg_produto_sel-ebelp
         AND   ebeln      = @tg_produto_sel-ebeln.

        SELECT SINGLE bsart
                 INTO l_bsart
                 FROM ekko
                WHERE ebeln = tg_produto_sel-ebeln.

        LOOP AT it_zmmt0037_log INTO DATA(wa_zmmt0037_log).
          MOVE-CORRESPONDING   wa_zmmt0037_log TO tl_input_zmmt0037_log.
          MOVE :  'E'                          TO tl_input_zmmt0037_log-tipo,
                  sy-uname                     TO tl_input_zmmt0037_log-usnam,
                  sy-datum                     TO tl_input_zmmt0037_log-data_atual,
                  sy-uzeit                     TO tl_input_zmmt0037_log-hora_atual.
          APPEND tl_input_zmmt0037_log.
          IF wa_zmmt0037_log-campo_ori = 'ELIKZ'.
            CLEAR: wa_poitem_ped,wa_poitemx_ped.
            v_ebeln_ori = wa_zmmt0037_log-ebeln_ori.
            CLEAR v_ebeln_eli.
            wa_poitem_ped-po_item    = wa_zmmt0037_log-ebelp_ori.
            wa_poitem_ped-no_more_gr  = ' '.
            APPEND wa_poitem_ped TO it_poitem_ped.

            wa_poitemx_ped-po_item     = wa_zmmt0037_log-ebelp_ori.
            wa_poitemx_ped-po_itemx    = 'X'.                                       "Item
            wa_poitemx_ped-no_more_gr  = 'X'.
            APPEND wa_poitemx_ped TO it_poitemx_ped.

          ELSEIF wa_zmmt0037_log-campo_ori = 'MENGE'.
            "Elimina linha a desfazer EBELN/EBELP
            CLEAR: wa_poitem_ped,wa_poitemx_ped.
            v_ebeln_eli = wa_zmmt0037_log-ebeln.
            IF v_ebeln_eli IS INITIAL.
              v_ebeln_eli = wg_cadlan-ebeln.
            ENDIF.
            wa_poitem_ped-po_item    = wa_zmmt0037_log-ebelp.
            wa_poitem_ped-delete_ind = 'L'.
            APPEND wa_poitem_ped TO it_poitem_ped_bloq.

            wa_poitemx_ped-po_item     = wa_zmmt0037_log-ebelp.
            wa_poitemx_ped-po_itemx    = 'X'.                                       "Item
            wa_poitemx_ped-delete_ind  = 'X'.
            APPEND wa_poitemx_ped TO it_poitemx_ped_bloq.
            "
            "Volta original quantidade EBELN_ORI/EBELP_ORI
            v_ebeln_ori = wa_zmmt0037_log-ebeln_ori.
            CONDENSE wa_zmmt0037_log-valor_ori NO-GAPS.
            v_menge = wa_zmmt0037_log-valor_ori.

            READ TABLE it_zmmt0037_log INTO DATA(wa_zmmt0037_log2) WITH KEY campo_ori = 'NETPR'.
            CONDENSE wa_zmmt0037_log2-valor_ori NO-GAPS.
            v_netpr = wa_zmmt0037_log2-valor_ori.


            SELECT SINGLE *
              FROM ekpo
              INTO @DATA(wekpo)
              WHERE ebeln = @wa_zmmt0037_log-ebeln_ori
              AND   ebelp = @wa_zmmt0037_log-ebelp_ori.

            READ TABLE it_zmmt0037_log INTO wa_zmmt0037_log2 WITH KEY campo_ori = 'QTDE_TROCA'.
            IF sy-subrc = 0.
              CONDENSE wa_zmmt0037_log2-valor_ori NO-GAPS.
              v_menge = wa_zmmt0037_log2-valor_ori.
              IF wekpo-menge GT 1 .
                v_menge = wekpo-menge + v_menge. " 06.09.2020 ( ao invés de pegar a quantidade gravada no log, retorna a quantidade trocada
              ENDIF.
            ENDIF.

            READ TABLE it_zmmt0037_log INTO wa_zmmt0037_log2 WITH KEY campo_ori = 'BPRME'.
            IF sy-subrc = 0.
              CONDENSE wa_zmmt0037_log2-valor_ori NO-GAPS.
              wekpo-bprme = wa_zmmt0037_log2-valor_ori.
            ENDIF.

            CLEAR: wa_poitem_ped,wa_poitemx_ped.
            "Volta quantidade  linha a desfazer EBELN_ORI/EBELP_ORI
            IF wekpo-loekz EQ 'L'.
              wa_poitem_ped-delete_ind = ' '.
              wa_poitemx_ped-delete_ind  = 'X'.
            ENDIF.
            wa_poitem_ped-po_item    = wa_zmmt0037_log-ebelp_ori.
            wa_poitem_ped-quantity   = v_menge.
            "01.09.2020
            IF wekpo-menge = 1.
              wa_poitem_ped-net_price   = v_netpr.
              wa_poitem_ped-orderpr_un  = wekpo-bprme. "teste
            ENDIF.
            "
            APPEND wa_poitem_ped TO it_poitem_ped.

            wa_poitemx_ped-po_item     = wa_zmmt0037_log-ebelp_ori.
            wa_poitemx_ped-po_itemx    = 'X'.                                       "Item
            wa_poitemx_ped-quantity    = 'X'.
            IF wekpo-menge = 1.
              wa_poitemx_ped-net_price   = 'X'.
              wa_poitemx_ped-orderpr_un  = 'X'.
            ENDIF.
            APPEND wa_poitemx_ped TO it_poitemx_ped.
            " Descontos
            DATA(_pbxx2) = 'N'.
            CLEAR: v_netpr_desc,v_netpr_supl.
            READ TABLE it_zmmt0037_log INTO wa_zmmt0037_log2 WITH KEY campo_ori = 'NETPR_DESC'.
            IF sy-subrc = 0.
              CONDENSE wa_zmmt0037_log2-valor_ori NO-GAPS.
              v_netpr_desc = wa_zmmt0037_log2-valor_ori.
              IF sy-subrc = 0 AND v_netpr_desc GT 0.
                wa_pocond-itm_number =  wa_zmmt0037_log-ebelp_ori.
                wa_pocond-cond_type = 'RB00'.
                wa_pocond-cond_value = v_netpr_desc * -1.
                wa_pocond-cond_unit = tg_produto2-bprme.
                wa_pocond-currency  = wg_cadlan-waers.
                wa_pocond-change_id = 'U'.
                "
                wa_pocondx-itm_number =  wa_zmmt0037_log-ebelp_ori.
                wa_pocondx-cond_type  = 'X'.
                wa_pocondx-cond_value = 'X'.
                wa_pocondx-cond_unit  = 'X'.
                wa_pocondx-currency   = 'X'.
                wa_pocondx-change_id  = 'X'.
                APPEND wa_pocondx TO it_pocondx.
                APPEND wa_pocond TO it_pocond.
                _pbxx2 = 'S'.
              ENDIF.
            ENDIF.
            "
            READ TABLE it_zmmt0037_log INTO wa_zmmt0037_log2 WITH KEY campo_ori = 'NETPR_SUPL'.
            IF sy-subrc = 0 .
              CONDENSE wa_zmmt0037_log2-valor_ori NO-GAPS.
              v_netpr_supl = wa_zmmt0037_log2-valor_ori.
              IF sy-subrc = 0 AND v_netpr_supl GT 0.
                wa_pocond-itm_number =  wa_zmmt0037_log-ebelp_ori.
                wa_pocond-cond_type = 'ZB00'.
                wa_pocond-cond_value = v_netpr_supl.
                wa_pocond-cond_unit = tg_produto2-bprme.
                wa_pocond-currency  = wg_cadlan-waers.
                wa_pocond-change_id = 'U'.
                "
                wa_pocondx-itm_number =  wa_zmmt0037_log-ebelp_ori.
                wa_pocondx-cond_type  = 'X'.
                wa_pocondx-cond_value = 'X'.
                wa_pocondx-cond_unit  = 'X'.
                wa_pocondx-currency   = 'X'.
                wa_pocondx-change_id  = 'X'.
                APPEND wa_pocondx TO it_pocondx.
                APPEND wa_pocond TO it_pocond.
                _pbxx2 = 'S'.
              ENDIF.
            ENDIF.

            "0,01 Preço bruto
            IF  _pbxx2 = 'S'.
              wa_pocond-itm_number =  wa_zmmt0037_log-ebelp_ori.
              wa_pocond-cond_type = 'PBXX'.
              wa_pocond-cond_value = v_netpr.
              wa_pocond-cond_unit = tg_produto2-bprme.
              wa_pocond-currency  = wg_cadlan-waers.
              wa_pocond-change_id = 'U'.
              "
              wa_pocondx-itm_number =  wa_zmmt0037_log-ebelp_ori.
              wa_pocondx-cond_type  = 'X'.
              wa_pocondx-cond_value = 'X'.
              wa_pocondx-cond_unit  = 'X'.
              wa_pocondx-currency   = 'X'.
              wa_pocondx-change_id  = 'X'.
              APPEND wa_pocondx TO it_pocondx.
              APPEND wa_pocond TO it_pocond.
            ENDIF.
            "
            IF wg_cadlan-ebeln = wa_zmmt0037_log-ebeln_ori.
              CLEAR wa_zmmt0037_log-ebeln_ori.
            ENDIF.

            IF l_bsart <> 'ZIMP'.
              UPDATE zmmt0037 SET menge = v_menge
                                  netpr = v_netpr
                                  bprme = wekpo-bprme
                                  netpr_desc = v_netpr_desc
                                  netpr_supl = v_netpr_supl
                WHERE nro_sol_cp = wg_cadlan-nro_sol_cp
                AND   ebelp      = wa_zmmt0037_log-ebelp_ori
                AND   ebeln      = wa_zmmt0037_log-ebeln_ori.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      "Antes de Alterar o pedido grava 999999999 para identificar que o HEDGE não vai ser alterado
      REFRESH tl_input_zmmt0037.
      LOOP AT tg_produto_sel.
        MOVE :  sy-mandt                     TO tl_input_zmmt0037-mandt,
                wg_cadlan-nro_sol_cp         TO tl_input_zmmt0037-nro_sol_cp,
                '9999999999'                 TO tl_input_zmmt0037-ebeln,
                tg_produto_sel-ebelp         TO tl_input_zmmt0037-ebelp,
                tg_produto_sel-matnr         TO tl_input_zmmt0037-matnr,
                tg_produto_sel-lgort         TO tl_input_zmmt0037-lgort,
                tg_produto_sel-charg         TO tl_input_zmmt0037-charg,
                tg_produto_sel-menge         TO tl_input_zmmt0037-menge,
                tg_produto_sel-meins         TO tl_input_zmmt0037-meins,
                tg_produto_sel-netpr         TO tl_input_zmmt0037-netpr,
                tg_produto_sel-netpr_desc    TO tl_input_zmmt0037-netpr_desc,
                tg_produto_sel-netpr_supl    TO tl_input_zmmt0037-netpr_supl,
                tg_produto_sel-netpr_germ    TO tl_input_zmmt0037-netpr_germ,
                tg_produto_sel-netpr_roya    TO tl_input_zmmt0037-netpr_roya,
                tg_produto_sel-peinh         TO tl_input_zmmt0037-peinh,
                tg_produto_sel-bprme         TO tl_input_zmmt0037-bprme,
                tg_produto_sel-mwskz         TO tl_input_zmmt0037-mwskz,
                tg_produto_sel-werks         TO tl_input_zmmt0037-werks,
                tg_produto_sel-netpr_orig    TO tl_input_zmmt0037-netpr_orig,
                sy-uname                     TO tl_input_zmmt0037-usnam,
                sy-datum                     TO tl_input_zmmt0037-data_atual,
                sy-uzeit                     TO tl_input_zmmt0037-hora_atual.

        APPEND tl_input_zmmt0037.
      ENDLOOP.
      MODIFY zmmt0037     FROM TABLE tl_input_zmmt0037.
      COMMIT WORK.

      IF v_ebeln_eli IS NOT INITIAL.
        CALL FUNCTION 'BAPI_PO_CHANGE'
          EXPORTING
            purchaseorder = v_ebeln_eli
          TABLES
            return        = it_return_ped_bloq
            poitem        = it_poitem_ped_bloq
            poitemx       = it_poitemx_ped_bloq
            potextheader  = it_bapimepotextheader_ped.

        READ TABLE it_return_ped_bloq INTO wa_return_ped WITH KEY type = 'E' .
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          "
          " Apaga pedido 999999999
          DELETE FROM zmmt0037 WHERE nro_sol_cp  = wg_cadlan-nro_sol_cp
                               AND   ebeln       = '9999999999' .
          COMMIT WORK.
          CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
            TABLES
              table    = it_return_ped
            EXCEPTIONS
              fb_error = 1
              OTHERS   = 2.
          "
          EXIT.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.
      ENDIF.

      IF l_bsart = 'ZIMP'.
        CLEAR wa_return_ped.
        wa_return_ped-type    = 'S'.
        wa_return_ped-id      = '06'.
        wa_return_ped-number  = '017'.
        APPEND wa_return_ped TO it_return_ped.
      ELSE.
        CALL FUNCTION 'BAPI_PO_CHANGE'
          EXPORTING
            purchaseorder = v_ebeln_ori
          TABLES
            return        = it_return_ped
            poitem        = it_poitem_ped
            poitemx       = it_poitemx_ped
            pocond        = it_pocond
            pocondx       = it_pocondx
            potextheader  = it_bapimepotextheader_ped.
      ENDIF.

      READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'E' .
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        "
        " Apaga pedido 999999999
        DELETE FROM zmmt0037 WHERE nro_sol_cp  = wg_cadlan-nro_sol_cp
                             AND   ebeln       = '9999999999' .
        COMMIT WORK.

        CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
          TABLES
            table    = it_return_ped
          EXCEPTIONS
            fb_error = 1
            OTHERS   = 2.
        "
        EXIT.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
      MODIFY zmmt0037_log FROM TABLE tl_input_zmmt0037_log.
      "
      " Apaga pedido 999999999
      DELETE FROM zmmt0037 WHERE nro_sol_cp  = wg_cadlan-nro_sol_cp
                           AND   ebeln       = '9999999999' .

      COMMIT WORK.

      EXIT.

    ELSE.
      LOOP AT tg_produto_sel.
        CLEAR: wa_return_ped, wa_poitem_ped, wa_poitemx_ped,
               wa_poheader_ped, wa_poheaderx_ped, wa_bapimepotextheader_ped.
        IF  v_ebeln = wg_cadlan-ebeln.
          READ TABLE tg_produto2 WITH KEY ebelp = tg_produto_sel-ebelp.
        ELSE.
          READ TABLE tg_produto2 WITH KEY ebeln = v_ebeln
                                          ebelp = tg_produto_sel-ebelp.
        ENDIF.
        tabix = sy-tabix.
        tg_produto2-menge_alt       = tg_produto2-menge - tg_produto2-qtde_troca.
        IF p_tipo = 'R'.    "Remessa Final
          wa_poitem_ped-po_item     = tg_produto2-ebelp.                          "Item
          wa_poitem_ped-no_more_gr  = 'X'.
          APPEND wa_poitem_ped TO it_poitem_ped.


          wa_poitemx_ped-po_item    = tg_produto2-ebelp.                          "Item
          wa_poitemx_ped-po_itemx   = 'X'.                                       "Item
          wa_poitemx_ped-no_more_gr = 'X'.
          APPEND wa_poitemx_ped TO it_poitemx_ped.

        ELSE.

          wa_poitem_ped-po_item    = tg_produto2-ebelp.                          "Item
          IF tg_produto2-menge_alt = 0.
            v_trans_total = 'X'.
            wa_poitem_ped-quantity   = 1.                                        "Quantidade 1 para não dar erro compensação
            wa_poitem_ped-net_price  =  1 / 100.              "01.09.2020
            wa_poitem_ped-orderpr_un = tg_produto2-bprme.
            IF tg_produto2-bprme = 'TO'.
              wa_poitem_ped-orderpr_un = 'KG'.
            ENDIF.
            "
            SELECT SINGLE knumv
               FROM ekko
              INTO @DATA(v_knumv)
              WHERE ebeln =  @v_ebeln.

            DATA(_pbxx) = 'N'.
            "desconto aqui
            SELECT SINGLE *
              FROM konv
              INTO @DATA(wa_konv)
              WHERE knumv = @v_knumv
              AND   kposn = @tg_produto2-ebelp
              AND   kschl = 'RB00'.

            IF sy-subrc = 0.
              wa_pocond-itm_number = tg_produto2-ebelp.
              wa_pocond-cond_type = 'RB00'.
              wa_pocond-cond_value = 0.
              wa_pocond-cond_unit = tg_produto2-bprme.
              wa_pocond-currency  = wg_cadlan-waers.
              wa_pocond-cond_value = 0.
              wa_pocond-change_id = 'U'.
              "
              wa_pocondx-itm_number = tg_produto2-ebelp.
              wa_pocondx-cond_type  = 'X'.
              wa_pocondx-cond_value = 'X'.
              wa_pocondx-cond_unit  = 'X'.
              wa_pocondx-currency   = 'X'.
              wa_pocondx-change_id  = 'X'.

              IF v_lifnr EQ wg_cadlan-lifnr_n AND p_tipo NE 'P' AND p_tipo NE 'Z'.
                APPEND wa_pocondx TO it_pocondx.
                APPEND wa_pocond TO it_pocond.
              ELSE.
                APPEND wa_pocondx TO it_pocondx_bloq.
                APPEND wa_pocond TO it_pocond_bloq.
              ENDIF.
              _pbxx = 'S'.
            ENDIF.
            "
            "suplemento aqui
            SELECT SINGLE *
              FROM konv
              INTO wa_konv
              WHERE knumv = v_knumv
              AND   kposn = tg_produto2-ebelp
              AND   kschl = 'ZB00'.

            IF sy-subrc = 0.
              wa_pocond-itm_number = tg_produto2-ebelp.
              wa_pocond-cond_type = 'ZB00'.
              wa_pocond-cond_value = 0.
              wa_pocond-cond_unit = tg_produto2-bprme.
              wa_pocond-currency  = wg_cadlan-waers.
              wa_pocond-cond_value = 0.
              wa_pocond-change_id = 'U'.
              "
              wa_pocondx-itm_number = tg_produto2-ebelp.
              wa_pocondx-cond_type  = 'X'.
              wa_pocondx-cond_value = 'X'.
              wa_pocondx-cond_unit  = 'X'.
              wa_pocondx-currency   = 'X'.
              wa_pocondx-change_id  = 'X'.
              IF v_lifnr EQ wg_cadlan-lifnr_n AND p_tipo NE 'P' AND p_tipo NE 'Z'.
                APPEND wa_pocondx TO it_pocondx.
                APPEND wa_pocond TO it_pocond.
              ELSE.
                APPEND wa_pocondx TO it_pocondx_bloq.
                APPEND wa_pocond TO it_pocond_bloq.
              ENDIF.
              _pbxx = 'S'.
            ENDIF.
            "0,01 Preço bruto
            IF  _pbxx = 'S'.
              wa_pocond-itm_number = tg_produto2-ebelp.
              wa_pocond-cond_type = 'PBXX'.
              wa_pocond-cond_value = 1 / 100.
              wa_pocond-cond_unit = wa_poitem_ped-orderpr_un.
              wa_pocond-currency  = wg_cadlan-waers.
              wa_pocond-change_id = 'U'.
              "
              wa_pocondx-itm_number = tg_produto2-ebelp.
              wa_pocondx-cond_type  = 'X'.
              wa_pocondx-cond_value = 'X'.
              wa_pocondx-cond_unit  = 'X'.
              wa_pocondx-currency   = 'X'.
              wa_pocondx-change_id  = 'X'.
              IF v_lifnr EQ wg_cadlan-lifnr_n AND p_tipo NE 'P'.
                APPEND wa_pocondx TO it_pocondx.
                APPEND wa_pocond TO it_pocond.
              ELSE.
                APPEND wa_pocondx TO it_pocondx_bloq.
                APPEND wa_pocond TO it_pocond_bloq.
              ENDIF.
              _pbxx = 'S'.
            ENDIF.
            MODIFY tg_produto2 INDEX tabix TRANSPORTING menge_alt. " BPRME. " NETPR .
          ELSE.
            wa_poitem_ped-quantity   = tg_produto2-menge_alt.                          "Quantidade
            MODIFY tg_produto2 INDEX tabix TRANSPORTING menge_alt.
          ENDIF.

          IF 'ZFTE_ZSEM' CS wg_cadlan-bsart.
            wa_poitem_ped-conf_ctrl = '0003'.
            wa_poitemx_ped-conf_ctrl = 'X'.
          ENDIF.

          IF v_lifnr EQ wg_cadlan-lifnr_n AND p_tipo NE 'P'  AND p_tipo NE 'Z'.
            APPEND wa_poitem_ped TO it_poitem_ped.
          ELSE.
            APPEND wa_poitem_ped TO it_poitem_ped_bloq.
          ENDIF.

          wa_poitemx_ped-po_item     = tg_produto2-ebelp.                          "Item
          wa_poitemx_ped-po_itemx    = 'X'.                                       "Item
          IF tg_produto2-menge_alt = 0.
*            WA_POITEMX_PED-DELETE_IND = 'X'.             01.09.2020

            wa_poitemx_ped-quantity    = 'X'.
            wa_poitemx_ped-net_price   = 'X'.
            wa_poitemx_ped-orderpr_un  = 'X'.                                      "Quantidade
          ELSE.
            wa_poitemx_ped-quantity    = 'X'.                                       "Quantidade
          ENDIF.
          IF v_lifnr EQ wg_cadlan-lifnr_n AND p_tipo NE 'P' AND p_tipo NE 'Z'.
            APPEND wa_poitemx_ped TO it_poitemx_ped.
          ELSE.
            APPEND wa_poitemx_ped TO it_poitemx_ped_bloq.
          ENDIF.

          ADD 10 TO v_ebelp.
          tg_produto_sel-ebelp = v_ebelp.
          MODIFY tg_produto_sel INDEX tabix TRANSPORTING ebelp.


          CLEAR: wa_return_ped, wa_poitem_ped, wa_poitemx_ped,
                 wa_poheader_ped, wa_poheaderx_ped, wa_bapimepotextheader_ped.
          "

          wa_poitem_ped-po_item    = v_ebelp.                                       "Item
          wa_poitem_ped-material   = tg_produto_sel-matnr.                          "Material
          wa_poitem_ped-quantity   = tg_produto_sel-menge.                          "Quantidade
          wa_poitem_ped-po_price   = 1.                                             "Transferência do preço: 1 = bruto, 2 = líquido
          wa_poitem_ped-net_price  = tg_produto_sel-netpr.                          "Preço
          wa_poitem_ped-price_unit = tg_produto_sel-peinh.                          "Unidade de preço
          wa_poitem_ped-orderpr_un = tg_produto_sel-bprme.                          "Unidade do preço do pedido
          wa_poitem_ped-tax_code   = tg_produto_sel-mwskz.                          "Código do Imposto
          wa_poitem_ped-plant      = tg_produto_sel-werks.                          "Centro
          wa_poitem_ped-batch      = tg_produto_sel-id_lote_frete.
          wa_poitem_ped-stge_loc   = tg_produto_sel-lgort.                          "Depósito

          IF 'ZFTE_ZSEM' CS wg_cadlan-bsart.
            wa_poitem_ped-conf_ctrl = '0003'.
            wa_poitemx_ped-conf_ctrl = 'X'.
          ENDIF.
          APPEND wa_poitem_ped TO it_poitem_ped.

          wa_poitemx_ped-po_item    = v_ebelp.                                   "Item
          wa_poitemx_ped-po_itemx   = 'X'.                                       "Item
          wa_poitemx_ped-material   = 'X'.                                       "Material
          wa_poitemx_ped-quantity   = 'X'.                                       "Quantidade
          wa_poitemx_ped-po_price   = 'X'.                                       "Transferência do preço: 1 = bruto, 2 = líquido
          wa_poitemx_ped-net_price  = 'X'.                                       "Preço
          wa_poitemx_ped-price_unit = 'X'.                                       "Unidade de preço
          wa_poitemx_ped-orderpr_un = 'X'.                                       "Unidade do preço do pedido
          wa_poitemx_ped-tax_code   = 'X'.                                       "Código do Imposto
          wa_poitemx_ped-plant      = 'X'.                                       "Centro
          wa_poitemx_ped-batch      = 'X'.
          wa_poitemx_ped-stge_loc   = 'X'.                                       "Depósito

          APPEND wa_poitemx_ped TO it_poitemx_ped.
*          IF TG_PRODUTO2-MENGE_ALT = 0 OR V_DIFF GT 0.
*            IF TG_PRODUTO2-MENGE_ALT = 0.
*              ADD V_DIFF TO TG_PRODUTO_SEL-NETPR_DESC1.
*            ELSE.
*              TG_PRODUTO_SEL-NETPR_DESC1 = V_DIFF.
*            ENDIF.
          IF tg_produto_sel-netpr_desc GT 0.
            wa_pocond-itm_number = v_ebelp.
            wa_pocond-cond_type = 'RB00'.
            wa_pocond-cond_value = tg_produto_sel-netpr_desc.
            wa_pocond-cond_unit = tg_produto_sel-bprme.
            wa_pocond-currency  = wg_cadlan-waers.
            wa_pocond-change_id = 'I'.
            APPEND wa_pocond TO it_pocond.
            "
            wa_pocondx-itm_number = v_ebelp.
            wa_pocondx-cond_type  = 'X'.
            wa_pocondx-cond_value = 'X'.
            wa_pocondx-cond_unit  = 'X'.
            wa_pocondx-currency   = 'X'.
            wa_pocondx-change_id  = 'X'.
            APPEND wa_pocondx TO it_pocondx.
          ENDIF.

          IF tg_produto_sel-netpr_supl GT 0.
            wa_pocond-itm_number = v_ebelp.
            wa_pocond-cond_type = 'ZB00'.
            wa_pocond-cond_value = tg_produto_sel-netpr_supl.
            wa_pocond-cond_unit = tg_produto_sel-bprme.
            wa_pocond-currency  = wg_cadlan-waers.
            wa_pocond-change_id = 'I'.
            APPEND wa_pocond TO it_pocond.
            "
            wa_pocondx-itm_number = v_ebelp.
            wa_pocondx-cond_type  = 'X'.
            wa_pocondx-cond_value = 'X'.
            wa_pocondx-cond_unit  = 'X'.
            wa_pocondx-currency   = 'X'.
            wa_pocondx-change_id  = 'X'.
            APPEND wa_pocondx TO it_pocondx.
          ENDIF.
        ENDIF.

      ENDLOOP.

      "Cabeçalho----------
      SELECT SINGLE j_1bbranch
        FROM t001w
        INTO v_branch
        WHERE werks EQ wg_cadlan-werks.

      SELECT SINGLE bukrs
        FROM j_1bbranch
        INTO v_bukrs
        WHERE branch EQ v_branch.

      wa_poheader_ped-comp_code = v_bukrs.                  "Empresa
      IF ( p_tipo = 'T' AND v_lifnr NE wg_cadlan-lifnr_n ) OR p_tipo = 'P' .
        wa_poheader_ped-doc_type  = 'ZSON'. " Pedido filho sempre
      ELSEIF p_tipo = 'Z'.
        wa_poheader_ped-doc_type  = 'ZIMP'. " Pedido filho sempre
      ELSE.
        SELECT SINGLE bsart
          INTO @DATA(v_bsart)
          FROM ekko WHERE ebeln = @v_ebeln.
        wa_poheader_ped-doc_type  = v_bsart. "WG_CADLAN-BSART.           "Tipo de Pedido original
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wg_cadlan-lifnr_n
        IMPORTING
          output = wg_cadlan-lifnr_n.

      wa_poheader_ped-vendor   = wg_cadlan-lifnr_n.                   "Fornecedor pela ZMM0045

      SELECT SINGLE ekorg
        FROM t024w
        INTO v_ekorg
        WHERE werks EQ wg_cadlan-werks.

      wa_poheader_ped-purch_org = v_ekorg.                  "Organização de Compras
      wa_poheader_ped-doc_date  = sy-datum.                 "Data do Pedido
      wa_poheader_ped-langu     = sy-langu.                 "Idioma
      wa_poheader_ped-pur_group = wg_cadlan-ekgrp.          "Grupo de Compradores
      wa_poheader_ped-currency  = wg_cadlan-waers.          "Moeda pela ZMM0045
      IF wg_cadlan-wkurs IS NOT INITIAL.
        wa_poheader_ped-exch_rate = wg_cadlan-wkurs.        "Taxa de Câmbio pela ZMM0045
      ELSE.
        wa_poheader_ped-exch_rate = 1.                      "Taxa de Câmbio pela ZMM0045
      ENDIF.
      wa_poheader_ped-our_ref   = wg_cadlan-safra.          "Safra
      wa_poheader_ped-quot_date = wg_cadlan-ihran.          "data cotação
      wa_poheader_ped-ref_1     = wg_cadlan-ped_forn.

      wa_poheader_ped-pmnttrms    = wg_cadlan-zterm.
      wa_poheader_ped-incoterms1  = wg_cadlan-inco1.
      wa_poheader_ped-incoterms2  = wg_cadlan-inco2.
      wa_poheader_ped-collect_no  = wg_cadlan-nro_sol_cp.   "Nº Solicitação


      wa_poheaderx_ped-comp_code  = 'X'.                     "Empresa
      wa_poheaderx_ped-doc_type   = 'X'.                     "Tipo de Pedido
      wa_poheaderx_ped-vendor     = 'X'.                     "Fornecedor pela ZMM0045
      wa_poheaderx_ped-purch_org  = 'X'.                     "Organização de Compras
      wa_poheaderx_ped-doc_date   = 'X'.                     "Data do Pedido
      wa_poheaderx_ped-langu      = 'X'.                     "Idioma
      wa_poheaderx_ped-pur_group  = 'X'.                     "Grupo de Compradores
      wa_poheaderx_ped-currency   = 'X'.                     "Moeda pela ZMM0045
      wa_poheaderx_ped-exch_rate  = 'X'.                     "Taxa pela ZMM0045
      wa_poheaderx_ped-our_ref    = 'X'.                     "Safra
      wa_poheaderx_ped-quot_date  = 'X'.                     "Data Cotação
      wa_poheaderx_ped-ref_1      = 'X'.
      wa_poheaderx_ped-pmnttrms   = 'X'.
      wa_poheaderx_ped-incoterms1 = 'X'.
      wa_poheaderx_ped-incoterms2 = 'X'.
      wa_poheaderx_ped-collect_no = 'X'.                      "Nº Solicitação

      "Texto Cabeçalho----
      IF obg_descbox IS NOT INITIAL.
        CALL METHOD obg_descbox->get_text_as_r3table
          IMPORTING
            table = it_editor_ped.
      ENDIF.

      LOOP AT it_editor_ped INTO wa_editor_ped.
        wa_bapimepotextheader_ped-text_id = 'F01'.
        wa_bapimepotextheader_ped-text_form = '*'.
        wa_bapimepotextheader_ped-text_line = wa_editor_ped-line.
        APPEND wa_bapimepotextheader_ped TO it_bapimepotextheader_ped.
      ENDLOOP.
    ENDIF.
*--------------------------------
*---parceiro
*--------------------------------
    FREE: it_popartner_ped.

    IF p_tipo = 'Z'.
      tg_produto_par[] = tg_produto_sel[].

      LOOP AT tg_produto_par        INTO wa_produto_par.
        IF wa_produto_par-lifnr1 IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_PARVW_OUTPUT'
            EXPORTING
              input  = wa_produto_par-parvw1
            IMPORTING
              output = wa_produto_par-parvw1.
          wa_popartner_ped-partnerdesc = wa_produto_par-parvw1.
          wa_popartner_ped-langu       = sy-langu.
          wa_popartner_ped-buspartno   = wa_produto_par-lifnr1.
          APPEND wa_popartner_ped     TO it_popartner_ped.
        ENDIF.
        IF wa_produto_par-lifnr2 IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_PARVW_OUTPUT'
            EXPORTING
              input  = wa_produto_par-parvw2
            IMPORTING
              output = wa_produto_par-parvw2.
          wa_popartner_ped-partnerdesc = wa_produto_par-parvw2.
          wa_popartner_ped-langu       = sy-langu.
          wa_popartner_ped-buspartno   = wa_produto_par-lifnr2.
          APPEND wa_popartner_ped     TO it_popartner_ped.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ( p_tipo = 'T' AND v_lifnr NE wg_cadlan-lifnr_n ) OR p_tipo = 'P' OR
        ( p_tipo = 'Z' AND tg_produto2-change <> 'X').
      CLEAR purchaseorder. "
      " COTAÇÃO IGUAL AO MÃE
      SELECT SINGLE angnr
        INTO @DATA(vg_angnr)
        FROM ekko
        WHERE ebeln = @wg_cadlan-ebeln.
      wa_poheader_ped-quotation = vg_angnr.
      wa_poheaderx_ped-quotation = 'X'.
      "Inclui novo pedido
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader         = wa_poheader_ped
          poheaderx        = wa_poheaderx_ped
        IMPORTING
          exppurchaseorder = purchaseorder
        TABLES
          return           = it_return_ped
          poitem           = it_poitem_ped
          poitemx          = it_poitemx_ped
          pocond           = it_pocond
          pocondx          = it_pocondx
          popartner        = it_popartner_ped
          potextheader     = it_bapimepotextheader_ped.
      "
      READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'E' .
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        "
        CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
          TABLES
            table    = it_return_ped
          EXCEPTIONS
            fb_error = 1
            OTHERS   = 2.
        "
        EXIT.
      ELSE.
        sy-subrc = 0.
      ENDIF.
      "Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      purchaseorder_son = purchaseorder.

      "
      "Antes de Alterar o pedido grava 999999999 para identificar que o HEDGE não vai ser alterado
      REFRESH tl_input_zmmt0037.
      LOOP AT tg_produto_sel.
        purchaseorder = '9999999999'.
        MOVE :  sy-mandt                     TO tl_input_zmmt0037-mandt,
                wg_cadlan-nro_sol_cp         TO tl_input_zmmt0037-nro_sol_cp,
                purchaseorder                TO tl_input_zmmt0037-ebeln,
                tg_produto_sel-ebelp         TO tl_input_zmmt0037-ebelp,
                tg_produto_sel-matnr         TO tl_input_zmmt0037-matnr,
                tg_produto_sel-lgort         TO tl_input_zmmt0037-lgort,
                tg_produto_sel-menge         TO tl_input_zmmt0037-menge,
                tg_produto_sel-meins         TO tl_input_zmmt0037-meins,
                tg_produto_sel-netpr         TO tl_input_zmmt0037-netpr,
                tg_produto_sel-netpr_desc    TO tl_input_zmmt0037-netpr_desc,
                tg_produto_sel-netpr_supl    TO tl_input_zmmt0037-netpr_supl,
                tg_produto_sel-netpr_germ    TO tl_input_zmmt0037-netpr_germ,
                tg_produto_sel-netpr_roya    TO tl_input_zmmt0037-netpr_roya,
                tg_produto_sel-peinh         TO tl_input_zmmt0037-peinh,
                tg_produto_sel-bprme         TO tl_input_zmmt0037-bprme,
                tg_produto_sel-mwskz         TO tl_input_zmmt0037-mwskz,
                tg_produto_sel-werks         TO tl_input_zmmt0037-werks,
                tg_produto_sel-netpr_orig    TO tl_input_zmmt0037-netpr_orig,
                sy-uname                     TO tl_input_zmmt0037-usnam,
                sy-datum                     TO tl_input_zmmt0037-data_atual,
                sy-uzeit                     TO tl_input_zmmt0037-hora_atual.

        APPEND tl_input_zmmt0037.
      ENDLOOP.
      MODIFY zmmt0037     FROM TABLE tl_input_zmmt0037.
      COMMIT WORK.

      IF p_tipo = 'Z'.
        wa_return_ped-type    = 'S'.
        wa_return_ped-id      = '06'.
        wa_return_ped-number  = '017'.
        APPEND wa_return_ped TO it_return_ped_bloq.
      ELSE.
        CALL FUNCTION 'BAPI_PO_CHANGE'
          EXPORTING
            purchaseorder = v_ebeln
          TABLES
            return        = it_return_ped_bloq
            poitem        = it_poitem_ped_bloq
            poitemx       = it_poitemx_ped_bloq
            pocond        = it_pocond_bloq
            pocondx       = it_pocondx_bloq
            potextheader  = it_bapimepotextheader_ped.
      ENDIF.

      READ TABLE it_return_ped_bloq INTO wa_return_ped WITH KEY type = 'E' .
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        "
        " Apaga pedido 999999999
        DELETE FROM zmmt0037 WHERE nro_sol_cp  = wg_cadlan-nro_sol_cp
                             AND   ebeln       = purchaseorder.
        COMMIT WORK.
        CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
          TABLES
            table    = it_return_ped_bloq
          EXCEPTIONS
            fb_error = 1
            OTHERS   = 2.
        "
        EXIT.
      ENDIF.
      "Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      " Apaga pedido 999999999
      DELETE FROM zmmt0037 WHERE nro_sol_cp  = wg_cadlan-nro_sol_cp
                           AND   ebeln       = purchaseorder.
      COMMIT WORK.

      purchaseorder = purchaseorder_son.

    ELSE.
      IF  v_trans_total = 'X'.
        SELECT SINGLE  rlwrt
          INTO v_rlwrt
          FROM ekko
          WHERE ebeln = v_ebeln.
        IF v_rlwrt GT 0.
          IF v_diff > 0.
            v_rlwrt = v_rlwrt + v_diff + + ( 1 / 100 ).
          ELSE.
            v_rlwrt = v_rlwrt + ( 1 / 100 ).
          ENDIF.
          UPDATE ekko SET rlwrt = v_rlwrt
              WHERE ebeln = v_ebeln.
          COMMIT WORK.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'BAPI_PO_CHANGE'
        EXPORTING
          purchaseorder = v_ebeln
          poheader      = wa_poheader_ped
          poheaderx     = wa_poheaderx_ped
        TABLES
          return        = it_return_ped
          poitem        = it_poitem_ped
          poitemx       = it_poitemx_ped
          pocond        = it_pocond
          pocondx       = it_pocondx
          potextheader  = it_bapimepotextheader_ped.
    ENDIF.


    IF 'T_R' CS p_tipo AND v_lifnr EQ wg_cadlan-lifnr_n.
      READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'E' .
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        "
        CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
          TABLES
            table    = it_return_ped
          EXCEPTIONS
            fb_error = 1
            OTHERS   = 2.
        "
        EXIT.
      ELSE.
        sy-subrc = 0.
      ENDIF.
    ELSE.
      IF p_tipo = 'Z' AND wg_produto2-change = 'X'. "BUG 52329 - Inicio
        READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'S' id = '06' number = '023'.
      ELSE. "BUG 52329 - Fim
        READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'S' id = '06' number = '017'.
      ENDIF.
    ENDIF.
    IF sy-subrc EQ 0.
      "Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      REFRESH tl_input_zmmt0037.
      LOOP AT tg_produto_sel.
        tg_produto_sel-style[] = style[].
        tg_produto_sel-situ    = 'X'.
        MODIFY tg_produto_sel INDEX sy-tabix TRANSPORTING style situ.
        MOVE :  sy-mandt                     TO tl_input_zmmt0037-mandt,
                wg_cadlan-nro_sol_cp         TO tl_input_zmmt0037-nro_sol_cp,
                purchaseorder                TO tl_input_zmmt0037-ebeln,
                tg_produto_sel-ebelp         TO tl_input_zmmt0037-ebelp,
                tg_produto_sel-matnr         TO tl_input_zmmt0037-matnr,
                tg_produto_sel-lgort         TO tl_input_zmmt0037-lgort,
                tg_produto_sel-charg         TO tl_input_zmmt0037-charg,
                tg_produto_sel-menge         TO tl_input_zmmt0037-menge,
                tg_produto_sel-meins         TO tl_input_zmmt0037-meins,
                tg_produto_sel-netpr         TO tl_input_zmmt0037-netpr,
                tg_produto_sel-netpr_desc    TO tl_input_zmmt0037-netpr_desc,
                tg_produto_sel-netpr_supl    TO tl_input_zmmt0037-netpr_supl,
                tg_produto_sel-netpr_germ    TO tl_input_zmmt0037-netpr_germ,
                tg_produto_sel-netpr_roya    TO tl_input_zmmt0037-netpr_roya,
                tg_produto_sel-peinh         TO tl_input_zmmt0037-peinh,
                tg_produto_sel-bprme         TO tl_input_zmmt0037-bprme,
                tg_produto_sel-mwskz         TO tl_input_zmmt0037-mwskz,
                tg_produto_sel-werks         TO tl_input_zmmt0037-werks,
                tg_produto_sel-netpr_orig    TO tl_input_zmmt0037-netpr_orig,
                sy-uname                     TO tl_input_zmmt0037-usnam,
                sy-datum                     TO tl_input_zmmt0037-data_atual,
                sy-uzeit                     TO tl_input_zmmt0037-hora_atual.

        APPEND tl_input_zmmt0037.
        "Valores Originais
        IF  v_ebeln = wg_cadlan-ebeln.
          READ TABLE tg_produto2 WITH KEY ebelp = tg_produto_sel-ebelp_ori.
        ELSE.
          READ TABLE tg_produto2 WITH KEY ebeln = v_ebeln
                                         ebelp = tg_produto_sel-ebelp_ori.
        ENDIF.

        MOVE :  sy-mandt                     TO tl_input_zmmt0037_log-mandt,
                wg_cadlan-nro_sol_cp         TO tl_input_zmmt0037_log-nro_sol_cp,
                purchaseorder                TO tl_input_zmmt0037_log-ebeln,
                tg_produto_sel-ebelp         TO tl_input_zmmt0037_log-ebelp,
                v_ebeln                      TO tl_input_zmmt0037_log-ebeln_ori,
                tg_produto_sel-ebelp_ori     TO tl_input_zmmt0037_log-ebelp_ori,
                'A'                          TO tl_input_zmmt0037_log-tipo,
                sy-uname                     TO tl_input_zmmt0037_log-usnam,
                sy-datum                     TO tl_input_zmmt0037_log-data_atual,
                sy-uzeit                     TO tl_input_zmmt0037_log-hora_atual.


        IF p_tipo = 'R' .
          tl_input_zmmt0037_log-campo_ori = 'ELIKZ'.
          tl_input_zmmt0037_log-campo_mod = 'Valor'.
          tl_input_zmmt0037_log-valor_ori = ' '.
          APPEND tl_input_zmmt0037_log.
        ELSE.
          tl_input_zmmt0037_log-campo_ori = 'MATNR'.
          tl_input_zmmt0037_log-campo_mod = 'Cód Material'.
          tl_input_zmmt0037_log-valor_ori = tg_produto2-matnr.
          tl_input_zmmt0037_log-valor_novo = tg_produto_sel-matnr.
          APPEND tl_input_zmmt0037_log.
          "
          tl_input_zmmt0037_log-campo_ori = 'MENGE'.
          tl_input_zmmt0037_log-campo_mod = 'Quantidade'.
          tl_input_zmmt0037_log-valor_ori = tg_produto2-menge.
          tl_input_zmmt0037_log-valor_novo = tg_produto_sel-menge.
          APPEND tl_input_zmmt0037_log.
          "
          tl_input_zmmt0037_log-campo_ori = 'QTDE_TROCA'.
          tl_input_zmmt0037_log-campo_mod = 'Quantidade'.
          tl_input_zmmt0037_log-valor_ori = tg_produto2-qtde_troca.
          "tl_input_zmmt0037_log-valor_novo = tg_produto_sel-qtde_troca. verificar
          APPEND tl_input_zmmt0037_log.

          tl_input_zmmt0037_log-campo_ori = 'NETPR'.
          tl_input_zmmt0037_log-campo_mod = 'Valor'.
          tl_input_zmmt0037_log-valor_ori = tg_produto2-netpr.
          tl_input_zmmt0037_log-valor_novo = tg_produto_sel-netpr.
          APPEND tl_input_zmmt0037_log.
          "
          tl_input_zmmt0037_log-campo_ori = 'NETPR_DESC'.
          tl_input_zmmt0037_log-campo_mod = 'Valor Desconto'.
          tl_input_zmmt0037_log-valor_ori = tg_produto2-netpr_desc.
          tl_input_zmmt0037_log-valor_novo = tg_produto_sel-netpr_desc.
          APPEND tl_input_zmmt0037_log.

          tl_input_zmmt0037_log-campo_ori = 'NETPR_SUPL'.
          tl_input_zmmt0037_log-campo_mod = 'Valor Supl'.
          tl_input_zmmt0037_log-valor_ori = tg_produto2-netpr_supl.
          tl_input_zmmt0037_log-valor_novo = tg_produto_sel-netpr_supl.
          APPEND tl_input_zmmt0037_log.
          "
          tl_input_zmmt0037_log-campo_ori = 'MWSKZ'.
          tl_input_zmmt0037_log-campo_mod = 'Código do IVA'.
          tl_input_zmmt0037_log-valor_ori = tg_produto2-mwskz.
          tl_input_zmmt0037_log-valor_novo = tg_produto_sel-mwskz.
          APPEND tl_input_zmmt0037_log.
          "
          tl_input_zmmt0037_log-campo_ori = 'BPRME'.
          tl_input_zmmt0037_log-campo_mod = 'Unid preço pedido'.

          tl_input_zmmt0037_log-valor_ori = tg_produto2-bprme.
          tl_input_zmmt0037_log-valor_novo = tg_produto_sel-bprme.
          APPEND tl_input_zmmt0037_log.
          "
          tl_input_zmmt0037_log-campo_ori = 'LIFNR'.
          tl_input_zmmt0037_log-campo_mod = 'Nº.conta fornecedor'.
          tl_input_zmmt0037_log-valor_ori = v_lifnr.
          tl_input_zmmt0037_log-valor_novo = wg_cadlan-lifnr_n .

          APPEND tl_input_zmmt0037_log.
        ENDIF.

        IF p_tipo <> 'Z'.
          IF tg_produto2-menge_alt NE 0.
            UPDATE zmmt0037 SET menge = tg_produto2-menge_alt
            WHERE nro_sol_cp = wg_cadlan-nro_sol_cp
            AND   ebelp      = tg_produto2-ebelp
            AND   ebeln      = tg_produto2-ebeln.
          ELSE.
            tg_produto2-netpr = 1 / 100.
            DATA(v_bprme) = tg_produto_sel-bprme.
            IF v_bprme = 'TO'.
              v_bprme = 'KG'.
            ENDIF.
            UPDATE zmmt0037 SET menge = 1
                                netpr = tg_produto2-netpr
                                bprme = v_bprme
                                netpr_desc = 0
                                netpr_supl = 0
            WHERE nro_sol_cp = wg_cadlan-nro_sol_cp
            AND   ebelp      = tg_produto2-ebelp
            AND   ebeln      = tg_produto2-ebeln.
          ENDIF.
        ENDIF.
      ENDLOOP.

      MODIFY zmmt0037     FROM TABLE tl_input_zmmt0037.
      MODIFY zmmt0037_log FROM TABLE tl_input_zmmt0037_log.
      " Apaga pedido 999999999 - confirma
      purchaseorder = '9999999999'.
      DELETE FROM zmmt0037 WHERE nro_sol_cp  = wg_cadlan-nro_sol_cp
                           AND   ebeln       = purchaseorder.
      COMMIT WORK.

      IF grid5 IS NOT INITIAL.
        CALL METHOD grid5->free.

        IF g_custom_cont_prods IS NOT INITIAL.
          CALL METHOD g_custom_cont_prods->free.
        ENDIF.

        FREE: grid5.
        FREE: g_custom_cont_prods.
      ENDIF.
      IF grid6 IS NOT INITIAL.
        CALL METHOD grid6->free.

        IF g_custom_cont_prod2 IS NOT INITIAL.
          CALL METHOD g_custom_cont_prod2->free.
        ENDIF.

        FREE: grid6.
        FREE: g_custom_cont_prod2.
      ENDIF.
*      WG_ACAO = C_DISPLA.
      wg_acao = 'RES'.
      REFRESH tg_produto_sel.
      PERFORM busca_dados.
      CALL METHOD grid2->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
      SET SCREEN 0.

    ELSE.
*---> CS1048756 / IR121398
      PERFORM verifica_zson USING '9124' purchaseorder wg_cadlan-nro_sol_cp.
*<--- CS1048756 / IR121398

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
        TABLES
          table    = it_return_ped
        EXCEPTIONS
          fb_error = 1
          OTHERS   = 2.


    ENDIF.

  ELSE.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH text-e14.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  SEARCH_BSART  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_bsart INPUT.

  TYPE-POOLS: vrm.

  DATA: options_bsart TYPE vrm_values,
        value_bsart   LIKE LINE OF options_bsart,
        linha_03      TYPE i.

  "Criando opções na listbox WA_TRANS-TRANSF_APROV
  value_bsart-key = 'ZDEF'.
  value_bsart-text = 'ZDEF'.
  APPEND value_bsart TO options_bsart.

  value_bsart-key = 'ZFTE'.
  value_bsart-text = 'ZFTE'.
  APPEND value_bsart TO options_bsart.

  value_bsart-key = 'ZSEM'.
  value_bsart-text = 'ZSEM'.
  APPEND value_bsart TO options_bsart.

  value_bsart-key = 'ZNB'.
  value_bsart-text = 'ZNB'.
  APPEND value_bsart TO options_bsart.

  value_bsart-key = 'ZEFI'.
  value_bsart-text = 'ZEFI'.
  APPEND value_bsart TO options_bsart.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'WG_CADLAN-BSART'
      values = options_bsart.

  CLEAR: options_bsart, value_bsart.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_TROCA_CENTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_troca USING p_tipo .
  DATA it_ekbe_soma  TYPE TABLE OF ty_ekbe.
  DATA it_ekbe      TYPE TABLE OF ty_ekbe.
  DATA: so_item         TYPE RANGE OF ekpo-ebelp,
        wa_item         LIKE LINE OF so_item,
        v_ebeln         TYPE ekko-ebeln,
        v_ebelnf        TYPE ekko-ebeln,
        w_erro(1),
        it_zmmt0037_log TYPE TABLE OF zmmt0037_log,
        w_zmmt0037_log  TYPE zmmt0037_log,
        w_zsdt0062      TYPE zsdt0062.

  p_tipo_oper = p_tipo.
  REFRESH: tg_produto_sel, tg_produto2.
  CLEAR tg_produto_sel.

  IF wg_cadlan-ebeln IS INITIAL.
    MESSAGE 'Não existe Pedido mãe criado' TYPE 'I'.
    EXIT.
  ENDIF.

  CALL METHOD grid2->get_selected_rows
    IMPORTING
      et_index_rows = tl_index_rows.

  IF tl_index_rows[] IS INITIAL.
*   MESSAGE 'Nenhuma linha selecionada!' TYPE 'I' DISPLAY LIKE 'I'.
    MESSAGE s897(sd) WITH 'Nenhuma linha selecionada!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT tl_index_rows INTO wl_index_rows.
    READ TABLE tg_produto INTO DATA(wg_produto) INDEX wl_index_rows-index.
    "
    wa_item-sign = 'I'.
    wa_item-option = 'EQ'.
    wa_item-low = wg_produto-ebelp.
    APPEND wa_item  TO so_item.

  ENDLOOP.

  CLEAR v_ebelnf.
  v_ebeln = wg_cadlan-ebeln. "pedido original
  IF wg_produto-ebeln IS NOT INITIAL.
    v_ebeln  = wg_produto-ebeln. "pedido filho
    v_ebelnf = wg_produto-ebeln. "pedido filho
  ENDIF.

  IF lines( tl_index_rows ) NE 1 AND  p_tipo = 'U'. "desfazer
    MESSAGE 'Selecione apenas uma linha para desfazer' TYPE 'I'.
    EXIT.
  ENDIF.

  CLEAR w_erro.

  LOOP AT tl_index_rows INTO wl_index_rows.
    READ TABLE tg_produto INTO DATA(wg_produto3) INDEX wl_index_rows-index.
    IF wg_produto3-ebeln IS NOT INITIAL AND
       wg_produto3-ebeln NE v_ebeln.
      w_erro = 'X'.
      EXIT.
    ENDIF.

* PBI - 60951 - Inicio
    IF p_tipo = 'T'. "troca
      READ TABLE tg_produto INTO DATA(wg_produto_troca) INDEX wl_index_rows-index.
      SELECT SINGLE *
        FROM zsdt0062
        INTO  w_zsdt0062
        WHERE ebeln = v_ebeln
        AND   ebelp = wg_produto_troca-ebelp
        AND   status = 'L'.
      IF sy-subrc = 0.
        w_erro = 'M'.
        EXIT.
      ENDIF.
    ENDIF.
* PBI - 60951 - Fim

    IF p_tipo = 'U'. "desfazer
      IF wg_produto3-loekz = icon_system_undo.
        w_erro = 'U'.
        EXIT.
      ELSEIF wg_produto3-loekz NE icon_warning.
        w_erro = 'W'.
        EXIT.
      ENDIF.
    ELSEIF p_tipo NE 'R'.
      IF  wg_produto3-qtde_troca <= 0.
        w_erro = 'Q'.
      ENDIF.
    ENDIF.
    "
    IF p_tipo = 'U'. " desfazer
      SELECT SINGLE *
           FROM zmmt0037_log
           INTO  w_zmmt0037_log
           WHERE nro_sol_cp = wg_cadlan-nro_sol_cp
           AND   ebelp_ori  = wg_produto3-ebelp
           AND   ebeln_ori  = v_ebeln.
      IF sy-subrc = 0.
        READ TABLE  tg_produto INTO DATA(wg_produto4) WITH KEY ebelp = w_zmmt0037_log-ebelp
                                                               ebeln = w_zmmt0037_log-ebeln.
        IF sy-subrc = 0.
          IF wg_produto4-loekz NE icon_system_undo.
            w_erro = 'S'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.


  IF w_erro = 'S'.
    MESSAGE |Desfazer antes a linha {  w_zmmt0037_log-ebelp } pedido { w_zmmt0037_log-ebeln }  | TYPE 'I'.
    MESSAGE |Desfazer antes a linha {  w_zmmt0037_log-ebelp } pedido { w_zmmt0037_log-ebeln }  | TYPE 'I'.
    EXIT.
  ENDIF.

  IF w_erro = 'W'.
    MESSAGE 'Não há modificações a serem desfeitas!' TYPE 'I'.
    MESSAGE 'Não há modificações a serem desfeitas!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF w_erro = 'U'.
    MESSAGE 'Linha já desfeita modificação!' TYPE 'I'.
    MESSAGE 'Linha já desfeita modificação!' TYPE 'I'.
    EXIT.
  ENDIF.

  IF w_erro = 'X'.
    MESSAGE 'Pedidos filhos não podem ser diferentes' TYPE 'I'.
    MESSAGE 'Pedidos filhos não podem ser diferentes' TYPE 'I'.
    EXIT.
  ENDIF.

  IF w_erro = 'M'.
    MESSAGE 'Pedido que está ocorrendo modificação tem vínculo na transação ZSDT0112' TYPE 'I'.
    MESSAGE 'Pedido que está ocorrendo modificação tem vínculo na transação ZSDT0112' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
   FROM ekko
   INTO @DATA(wa_ekko)
   WHERE ebeln EQ @v_ebeln.

  wg_cadlan-lifnr_n = wa_ekko-lifnr.
  SELECT SINGLE name1
    FROM lfa1
    INTO wg_cadlan-name1_n
   WHERE lifnr = wg_cadlan-lifnr_n.

  DATA(v_elikz) = ' '.
  IF p_tipo = 'U'.
    v_elikz = 'X'.
  ENDIF.
  SELECT *
    FROM ekpo
    INTO TABLE @DATA(it_ekpo)
    WHERE ebeln EQ @v_ebeln
    AND   ebelp IN @so_item
    AND   elikz IN ( ' ', @v_elikz )
    AND   loekz EQ ' '.

  "
  IF it_ekpo[] IS NOT INITIAL.
    SELECT *
       FROM ekbe
         INTO CORRESPONDING FIELDS OF TABLE it_ekbe "MIGO
        FOR ALL ENTRIES IN it_ekpo
        WHERE ebeln EQ it_ekpo-ebeln
        AND   ebelp EQ it_ekpo-ebelp
        AND   bewtp EQ 'E'.
  ENDIF.

  "MIGO
  LOOP AT it_ekbe INTO DATA(wa_ekbe).
    IF wa_ekbe-shkzg = 'H'.
      MULTIPLY  wa_ekbe-menge BY -1.
      MULTIPLY  wa_ekbe-dmbtr BY -1.
    ENDIF.
    CLEAR wa_ekbe-shkzg.
    COLLECT wa_ekbe INTO it_ekbe_soma.
  ENDLOOP.

  SORT it_ekbe_soma  BY ebeln ebelp.
  REFRESH style.

  LOOP AT it_ekpo INTO DATA(wa_ekpo).
    "MIGO
    CLEAR  wa_ekbe.
    READ TABLE it_ekbe_soma INTO wa_ekbe WITH KEY ebeln = wa_ekpo-ebeln
                                                  ebelp = wa_ekpo-ebelp BINARY SEARCH.
    IF p_tipo <> 'Z'.
      SUBTRACT wa_ekbe-menge FROM wa_ekpo-menge.
    ENDIF.

    IF wa_ekpo-menge GT 0.
      READ TABLE tg_produto INTO DATA(wg_produto2) WITH KEY ebeln = v_ebelnf "pedido filho ou branco para original na linha do item
                                                            ebelp = wa_ekpo-ebelp.


      MOVE-CORRESPONDING wg_produto2 TO tg_produto_sel.
      tg_produto_sel-style = style[].

      IF p_tipo EQ 'P' OR p_tipo EQ 'T' OR p_tipo EQ 'Z'.
        tg_produto_sel-valor        = wg_produto2-vlr_troca.
        tg_produto_sel-netpr_final  = wg_produto2-vlr_troca.
        tg_produto_sel-menge        = wg_produto2-qtde_troca.
      ENDIF.

      tg_produto_sel-ebeln_ori     = tg_produto_sel-ebeln.
      tg_produto_sel-ebelp_ori     = tg_produto_sel-ebelp.
      tg_produto_sel-werks         = wa_ekpo-werks.
      tg_produto_sel-netpr_orig    = wg_produto2-netpr.
      tg_produto_sel-id_lote_frete = wa_ekpo-id_lote_frete.

      "calculo o valor de quantidade 1
      PERFORM r_imposto_item USING wg_cadlan-lifnr_n
                                      wg_produto2-werks
                                      wg_produto2-ebelp
                                      v_ebeln
                                      wg_produto2-matnr
                                      1
                                      wg_produto2-netpr
                                      wg_produto2-mwskz
                                      wg_produto2-peinh
                                      wg_produto2-bprme
                                      wg_produto2-netpr_desc
                                      wg_produto2-netpr_supl
                         CHANGING   w_valor
                                    w_wmwst.
      tg_produto_sel-netpr_desc1 = w_valor.
      tg_produto_sel-netpr_desc = 0.
      tg_produto_sel-netpr_supl = 0.

*-------------------
*--   tratamento parceiros
*-------------------
      READ TABLE tg_parceiro INDEX 1.
      IF sy-subrc = 0 AND p_tipo = 'Z'.
        LOOP AT tg_parceiro.
          IF tg_parceiro-parvw = 'PR'.
            tg_produto_sel-parvw1 = tg_parceiro-parvw.
            tg_produto_sel-lifnr1 = tg_parceiro-lifnr.
          ELSE.
            tg_produto_sel-parvw2 = tg_parceiro-parvw.
            tg_produto_sel-lifnr2 = tg_parceiro-lifnr.
          ENDIF.
        ENDLOOP.
        APPEND tg_produto_sel.
      ELSE.
        APPEND tg_produto_sel.
      ENDIF.

** BUG 52329 - Inicio
      IF p_tipo EQ 'Z' AND wg_produto2-bsart = 'ZIMP'.
        wg_produto2-change = 'X'.
        wg_produto2-vlr_troca = wg_produto2-valor.
      ENDIF.
*** BUG 52329 - Fim

      MOVE-CORRESPONDING wg_produto2 TO tg_produto2.
      tg_produto2-menge_tro = tg_produto2-menge - tg_produto2-menge_migo.
      APPEND tg_produto2.
    ENDIF.
  ENDLOOP.

  IF tg_produto_sel[] IS NOT INITIAL.
    tg_produto_sel_old[] = tg_produto_sel[]. "PBI - 60952

    DATA(start) = 10.
    DATA(start_limit) = 1.
    DATA(end) = 240.
    DATA(end_limit) = 20.


    CALL SCREEN 0600  STARTING AT start start_limit
                      ENDING   AT end end_limit.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0600 OUTPUT.

  IF p_tipo_oper = 'T' OR p_tipo_oper = 'P'.
    LOOP AT SCREEN.
      IF    screen-name = 'WG_CADLAN-LIFNR_N'.
        screen-input     = 1.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_tipo_oper = 'Z'.
    READ TABLE tg_produto_sel INDEX 1.
    wa_produto_sel = tg_produto_sel.
    LOOP AT tg_produto_sel.
      tg_produto_sel-menge = wa_produto_sel-menge.
      MODIFY tg_produto_sel INDEX sy-tabix.
    ENDLOOP.
  ENDIF.

  SET PF-STATUS 'Z002'.
  SET TITLEBAR '0600'.
  "
  IF g_custom_cont_prods IS INITIAL.

    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_layout-col_opt    = space. "C_X.
    wa_stable-row        = c_x.
    wa_layout-sel_mode   = 'A'.
    wa_layout-cwidth_opt = c_x.
    wa_layout-box_fname  = 'MARK'.
    wa_layout-stylefname = 'STYLE'.
    wa_layout-no_toolbar = c_x.

    CASE p_tipo_oper.
      WHEN 'T'.   wa_layout-grid_title   = 'Troca'.
      WHEN 'P'.   wa_layout-grid_title   = 'Desmembra Pedido'.
      WHEN 'R'.   wa_layout-grid_title   = 'Remessa Final'.
      WHEN 'U'.   wa_layout-grid_title   = 'Desfazer'.
      WHEN 'Z'.   wa_layout-grid_title   = 'Pedido ZIMP'.
    ENDCASE.

    CREATE OBJECT g_custom_cont_prods
      EXPORTING
        container_name = g_container_produtos.

    CREATE OBJECT grid5
      EXPORTING
        i_parent = g_custom_cont_prods.

    PERFORM montar_layout_prods USING p_tipo_oper.

    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.


    CALL METHOD grid5->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_produto_sel[].

    CALL METHOD grid5->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid5->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_data_changed_finished2 FOR grid5,
              lcl_event_handler=>on_data_changed5 FOR grid5,
              lcl_event_handler=>on_f4 FOR grid5.
  ELSE.
    CALL METHOD grid5->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
  "ORIGINAL
  "GRID6
  IF g_custom_cont_prod2 IS INITIAL.
    CREATE OBJECT g_custom_cont_prod2
      EXPORTING
        container_name = g_container_produto2.

    CREATE OBJECT grid6
      EXPORTING
        i_parent = g_custom_cont_prod2.

    PERFORM montar_layout_prod2  USING p_tipo_oper.

    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.

    wa_layout-stylefname = 'STYLE'.
    wa_layout-grid_title   = 'Original'.
    wa_layout-cwidth_opt = abap_true.
    wa_layout-zebra      = abap_true.
    wa_layout-sel_mode   = 'A'.
*    wa_layout-col_opt    = abap_true.



    CALL METHOD grid6->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_c
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_default            = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_produto2[].

    CALL METHOD grid6->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid6->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_data_changed6 FOR grid6.


  ELSE.
    CALL METHOD grid6->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0600 INPUT.
  CASE ok-code.
    WHEN c_search.
      CLEAR  wg_cadlan-name1_n.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wg_cadlan-lifnr_n
        IMPORTING
          output = wg_cadlan-lifnr_n.
      SELECT SINGLE name1
        FROM lfa1
        INTO wg_cadlan-name1_n
        WHERE lifnr = wg_cadlan-lifnr_n.
    WHEN 'TRANS'.
      CLEAR p_erro_oper.
      PERFORM  altera_ped USING p_tipo_oper CHANGING p_erro_oper.
      IF p_erro_oper NE 'X'.
        IF grid5 IS NOT INITIAL.
          CALL METHOD grid5->free.

          IF g_custom_cont_prods IS NOT INITIAL.
            CALL METHOD g_custom_cont_prods->free.
          ENDIF.

          FREE: grid5.
          FREE: g_custom_cont_prods.
        ENDIF.
        IF grid6 IS NOT INITIAL.
          CALL METHOD grid6->free.

          IF g_custom_cont_prod2 IS NOT INITIAL.
            CALL METHOD g_custom_cont_prod2->free.
          ENDIF.

          FREE: grid6.
          FREE: g_custom_cont_prod2.
        ENDIF.

        wg_acao = 'RES'.
        REFRESH tg_produto_sel.
        PERFORM busca_dados.
        CALL METHOD grid2->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
        SET SCREEN 0.
      ENDIF.
    WHEN 'SAIR'.
      IF grid5 IS NOT INITIAL.
        CALL METHOD grid5->free.

        IF g_custom_cont_prods IS NOT INITIAL.
          CALL METHOD g_custom_cont_prods->free.
        ENDIF.

        FREE: grid5.
        FREE: g_custom_cont_prods.
      ENDIF.
      IF grid6 IS NOT INITIAL.
        CALL METHOD grid6->free.

        IF g_custom_cont_prod2 IS NOT INITIAL.
          CALL METHOD g_custom_cont_prod2->free.
        ENDIF.

        FREE: grid6.
        FREE: g_custom_cont_prod2.
      ENDIF.
*      WG_ACAO = C_DISPLA.
      wg_acao = 'RES'.
      REFRESH tg_produto_sel.
      PERFORM busca_dados.
      CALL METHOD grid2->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.

FORM r_imposto_item USING     w_lifnr
                              w_werks
                              w_ebelp
                              w_ebeln
                              w_matnr
                              w_menge
                              w_netpr
                              w_mwskz
                              w_peinh
                              w_bprme
                              w_netpr_desc
                              w_netpr_supl
                    CHANGING  w_valor
                              w_wmwst.
  DATA i_taxcom LIKE  taxcom.
  DATA: wa_ite  LIKE mepoitem.
  DATA: w_lifnr_centro TYPE lfa1-lifnr.

  CLEAR wa_ite.
  CALL FUNCTION 'MEPO_DOC_ITEM_GET'
    EXPORTING
      im_ebelp = w_ebelp                                    "'00010'
    IMPORTING
      ex_item  = wa_ite
    EXCEPTIONS
      failure  = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DATA: BEGIN OF t_konv OCCURS 0.
          INCLUDE STRUCTURE konv.
        DATA: END OF t_konv.

  TYPES: ty_konv TYPE TABLE OF komv.

  FIELD-SYMBOLS: <wmwst> TYPE any,
                 <lfa1>  TYPE lfa1,
                 <ekpo>  TYPE ekpo,
                 <ek2>   TYPE ekpo,
                 <ekko>  TYPE ekko,
                 <vorga> TYPE any,
                 <konv>  TYPE ty_konv,
                 <cva>   TYPE any.

  ASSIGN ('(SAPLMEPO)ekpo') TO <ekpo>.
  ASSIGN ('(SAPLMEPO)ekko') TO <ekko>.
  ASSIGN ('(SAPLMEPO)lfa1') TO <lfa1>.

  CLEAR <ekpo>.
  SELECT SINGLE * FROM ekpo INTO <ekpo>
    WHERE ebeln = w_ebeln AND
          ebelp = w_ebelp.

  IF sy-subrc NE 0 OR <ekpo>-matnr NE w_matnr.
    IF w_werks IS INITIAL.
      w_werks = wg_cadlan-werks.
    ENDIF.
    <ekpo>-werks = w_werks.

    SELECT SINGLE bukrs
        INTO  <ekpo>-bukrs
        FROM j_1bbranch
        WHERE branch = <ekpo>-werks.
    SELECT SINGLE *
      FROM mara
      INTO @DATA(w_mara)
      WHERE  matnr = @w_matnr.
    MOVE-CORRESPONDING w_mara TO <ekpo>.

    SELECT SINGLE *
        FROM mard
        INTO @DATA(w_mard)
        WHERE  matnr = @w_matnr
        AND    werks = @w_werks.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING w_mard TO <ekpo>.
    ENDIF.

    SELECT SINGLE *
      FROM mbew
      INTO @DATA(w_mbew)
      WHERE bwkey = @w_werks
      AND   matnr = @w_matnr.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING w_mbew TO <ekpo>.
    ENDIF.
    "
    SELECT SINGLE *
      FROM marc
      INTO @DATA(w_marc)
      WHERE werks = @w_werks
      AND   matnr = @w_matnr.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING w_marc TO <ekpo>.
    ENDIF.
  ELSE.
    CLEAR <ekko>.
    SELECT SINGLE * FROM ekko INTO <ekko>
       WHERE ebeln = w_ebeln.

    SELECT SINGLE * FROM lfa1 INTO <lfa1>
       WHERE lifnr = <ekko>-lifnr.

  ENDIF.

  IF w_netpr GT 0 AND w_peinh GT 0.
    DATA(fator) = 1000.
    IF w_bprme NE 'TO'.
      fator  = 1.
    ENDIF.
    "
    IF <ekpo>-matnr NE w_matnr AND w_matnr  IS NOT INITIAL.
      <ekpo>-matnr = w_matnr.
      SELECT SINGLE meins
        INTO <ekpo>-meins
        FROM mara
      WHERE matnr =  w_matnr.
    ENDIF.

    IF w_werks IS INITIAL.
      <ekpo>-werks = wg_cadlan-werks.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <ekpo>-werks
      IMPORTING
        output = w_lifnr_centro.

    SELECT SINGLE * FROM lfa1 INTO @DATA(w_lifnr_cen)
       WHERE lifnr = @w_lifnr_centro.

    IF w_lifnr IS NOT INITIAL.
      <ekko>-lifnr = w_lifnr.
    ENDIF.

    SELECT SINGLE * FROM lfa1 INTO <lfa1>
      WHERE lifnr = <ekko>-lifnr.


    <ekpo>-netpr = w_netpr.
    <ekpo>-menge = w_menge.
    <ekpo>-netwr = ( ( w_menge * w_netpr ) / w_peinh / fator ) - w_netpr_desc + w_netpr_supl.
    <ekpo>-brtwr = <ekpo>-netwr.
    <ekpo>-effwr = <ekpo>-netwr.
    <ekpo>-bonba = <ekpo>-netwr.
    <ekpo>-mwskz = w_mwskz.
    <ekpo>-bprme = w_bprme.
    <ekpo>-txjcd = w_lifnr_cen-txjcd.
    <ekpo>-loekz = ' '.
  ENDIF.


  CLEAR <ekko>.
  SELECT SINGLE * FROM ekko INTO <ekko>
    WHERE ebeln = w_ebeln.

  IF <ekko>-bukrs IS INITIAL .
    SELECT SINGLE bukrs
      INTO  <ekko>-bukrs
      FROM j_1bbranch
      WHERE branch = <ekpo>-werks.
    <ekko>-bstyp = 'F'.
    <ekko>-lifnr = w_lifnr.
    <ekko>-waers = wg_cadlan-waers.
    <ekko>-ekorg = 'OC01'.
    <ekko>-kalsm = 'RM0000'.
    <ekko>-aedat = sy-datum.
    <ekko>-bedat = sy-datum.
  ENDIF.

  IF <ekko>-knumv IS NOT INITIAL.
    SELECT * FROM konv INTO TABLE t_konv
     WHERE knumv = <ekko>-knumv.

    ASSIGN ('(SAPLMEPO)tkomv[]') TO <konv>.
    <konv>[] = t_konv[].
  ENDIF.


  IF <ekpo>-ebelp IS INITIAL AND  w_ebelp IS NOT INITIAL.
    <ekpo>-ebelp = w_ebelp.
  ENDIF.



  ASSIGN ('(SAPLMEPO)fc_vorga') TO <vorga>.
  ASSIGN ('(SAPLMEPO)cva_en') TO <cva>.

  <vorga> = <cva>.

  PERFORM kond_taxes(saplmepo) USING 'D' 'X'.

  CHECK <ekpo>-loekz = space.
  ASSIGN ('(SAPLMEPO)taxcom-WMWST') TO <wmwst>.


  DATA: w_netwr  TYPE komp-netwr.

  w_netwr = <ekpo>-netwr.
  w_wmwst  =  <wmwst>.
  w_valor  = ( w_netwr + <wmwst> ).

ENDFORM.                    " R_IMPOSTO_ITEM
*&---------------------------------------------------------------------*
*&      Module  LIMPA_TELA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE limpa_tela INPUT.
  DATA wg_cadlan_cop      TYPE ty_cadlan.
  MOVE-CORRESPONDING wg_cadlan TO wg_cadlan_cop.
  CLEAR: wg_cadlan_cop-nro_sol_cp,  wg_cadlan_cop-waers.
  IF wg_cadlan_cop IS NOT INITIAL.
    PERFORM limpa_campos.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_IMPORTAR_ANEXOS
*&---------------------------------------------------------------------*
FORM f_importar_anexos .
  FREE: t_anexos.

  IF wg_cadlan-nro_sol_cp IS INITIAL.
    MESSAGE 'Por favor Informar número da ordem!' TYPE 'I'.
    EXIT.
  ELSE.

    l_obj_key = wg_cadlan-nro_sol_cp.

    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        classname          = 'ZMMR149_ANEXO'
        objkey             = l_obj_key
        client             = sy-mandt
      TABLES
        gos_connections    = t_anexos
      EXCEPTIONS
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3
        OTHERS             = 4.

    DESCRIBE TABLE t_anexos LINES l_lines.

    CREATE OBJECT anexo_obj TYPE cl_gos_manager.

    l_ip_mode     = 'E'.
    l_ip_service  = COND #( WHEN l_lines = 0 THEN 'PCATTA_CREA'
                                             ELSE 'VIEW_ATTA' ).
    w_bor-objkey  = l_obj_key. "l_chave.
    w_bor-objtype = 'ZMMR149_ANEXO'.

    anexo_obj->set_rw_mode( ip_mode = l_ip_mode ).

    anexo_obj->start_service_direct(
      EXPORTING
        ip_service         = l_ip_service
        is_object          = w_bor
      EXCEPTIONS
        no_object          = 1
        object_invalid     = 2
        execution_failed   = 3
        OTHERS             = 4 ).

    WAIT UP TO 2 SECONDS.

    COMMIT WORK.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0800 OUTPUT.
  SET PF-STATUS 'Z003'.
  SET TITLEBAR '0800'.

  DATA: gs_variant  TYPE disvariant.
  gs_variant-report      = sy-repid.


  IF g_custom_cop_item IS INITIAL.

    CREATE OBJECT g_custom_cop_item
      EXPORTING
        container_name = g_container_cop_item.

    CREATE OBJECT grid7
      EXPORTING
        i_parent = g_custom_cop_item.

    PERFORM montar_layout_cop_item.
*
*    REFRESH: tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    APPEND wl_function TO tl_function.

    " wa_layout-stylefname = 'STYLE'.
    wa_layout-grid_title   = 'Cópia'.

    CLEAR wa_layout-stylefname.
    wa_layout-cwidth_opt = abap_true.
    wa_layout-zebra      = abap_true.
    wa_layout-sel_mode   = 'A'.
    wa_layout-col_opt    = abap_true.

    CALL METHOD grid7->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_save               = 'A'
        is_variant           = gs_variant
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_cop_item[].

    CALL METHOD grid7->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid7->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    FREE: gt_f4.
    gt_f4-fieldname = 'WG_COPLAN-NRO_SOL_CP'.
    gt_f4-register = 'X'.
    gt_f4-getbefore = 'X'.
    gt_f4-chngeafter ='X'.
    APPEND gt_f4.

    CALL METHOD grid7->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    SET HANDLER:
          lcl_event_handler=>on_f4 FOR grid7.

  ELSE.
    PERFORM montar_layout_cop_item.
    "
    CALL METHOD grid7->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid7->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        i_save               = 'A'
        is_variant           = gs_variant
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_cop_item[].
    "
    CALL METHOD grid7->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN  'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      SET SCREEN 0.
    WHEN 'PROC_CP'.
      PERFORM executa_copia.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_NRO_SOL_CP  INPUT
*&---------------------------------------------------------------------*
MODULE search_nro_sol_cp INPUT.


  DATA: tl_return_tab_op TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc_op      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF t_fieldtab OCCURS 3.
          INCLUDE STRUCTURE dynpread.
        DATA: END OF t_fieldtab.

  DATA: tl_field_tab LIKE dfies OCCURS 0 WITH HEADER LINE.
  DATA  tl_field_wa LIKE dfies.


  DATA: BEGIN OF tl_operacao OCCURS 0,
          nro_sol_cp TYPE zmmt0035-nro_sol_cp,
          safra      TYPE zmmt0035-safra,
          bsart      TYPE zmmt0035-bsart,
          lifnr      TYPE zmmt0035-lifnr,
          name1      TYPE lfa1-name1,
        END OF tl_operacao.

  DATA: BEGIN OF tl_data OCCURS 1,
          DATA(60),
        END OF tl_data.

  REFRESH: tl_data, tl_operacao, t_fieldtab, tl_dselc_op,  tl_field_tab.
  CLEAR:   tl_data, tl_operacao, t_fieldtab, tl_dselc_op.

  IF wg_coplan-nro_sol_cp IS NOT INITIAL.
    PERFORM preenche_copia.
  ELSE.

    SELECT nro_sol_cp safra bsart lifnr
      FROM zmmt0035
      INTO TABLE tl_operacao.

    LOOP AT  tl_operacao INTO DATA(wl_operacao).

*    tl_data = wl_operacao-nro_sol_cp.
*    APPEND tl_data.
*
*    tl_data = wl_operacao-safra.
*    APPEND tl_data.
*
*    tl_data = wl_operacao-bsart.
*    APPEND tl_data.

      SELECT SINGLE name1
        FROM lfa1
        INTO wl_operacao-name1
          WHERE  lifnr EQ wl_operacao-lifnr.

*    tl_data =  wl_operacao-name1.
*    APPEND tl_data.

      MODIFY tl_operacao FROM  wl_operacao INDEX sy-tabix.
    ENDLOOP.


    PERFORM f_fieldinfo_get USING 'ZMMT0035' 'NRO_SOL_CP'  CHANGING tl_field_wa.
    tl_field_wa-tabname   = 'ZMMT0035'.
    tl_field_wa-fieldname = 'NRO_SOL_CP'.
    tl_field_wa-fieldtext = 'NRO_SOL_CP'.
    tl_field_wa-reptext   = 'Nro.Sol.Ordem'.
    tl_field_wa-scrtext_l = 'Nro.Sol.Ordem'.
    APPEND tl_field_wa TO tl_field_tab.

    PERFORM f_fieldinfo_get USING 'ZMMT0035' 'SAFRA'  CHANGING tl_field_wa.
    tl_field_wa-tabname   = 'ZMMT0035'.
    tl_field_wa-fieldname = 'SAFRA'.
    tl_field_wa-fieldtext = 'SAFRA'.
    tl_field_wa-reptext   = 'Safra'.
    tl_field_wa-scrtext_l = 'Safra'.
    APPEND tl_field_wa TO tl_field_tab.

    PERFORM f_fieldinfo_get USING 'ZMMT0035' 'BSART'  CHANGING tl_field_wa.
    tl_field_wa-tabname   = 'ZMMT0035'.
    tl_field_wa-fieldname = 'BSART'.
    tl_field_wa-fieldtext = 'BSART'.
    tl_field_wa-reptext   = 'Tp.Pedido'.
    tl_field_wa-scrtext_l = 'Tp.Pedido'.
    APPEND tl_field_wa TO tl_field_tab.

    PERFORM f_fieldinfo_get USING 'LFA1' 'NAME1'  CHANGING tl_field_wa.
    tl_field_wa-tabname   = 'LFA1'.
    tl_field_wa-fieldname = 'NAME1'.
    tl_field_wa-fieldtext = 'NAME1'.
    tl_field_wa-reptext   = 'Nome Fornecedor'.
    tl_field_wa-scrtext_l = 'Nome Fornecedor'.
    APPEND tl_field_wa TO tl_field_tab.


    tl_dselc_op-fldname    = 'F0001'.
    tl_dselc_op-dyfldname  = 'WG_COPLAN-NRO_SOL_CP'.
    APPEND tl_dselc_op.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'NRO_SOL_CP'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'WG_COPLAN-NRO_SOL_CP'
        value_org       = 'S'
      TABLES
        value_tab       = tl_operacao "tl_data "first table
        "field_tab       = tl_field_tab[] "second table
        return_tab      = tl_return_tab_op
        dynpfld_mapping = tl_dselc_op.

    wg_coplan-nro_sol_cp = tl_return_tab_op-fieldval.

    PERFORM preenche_copia.

  ENDIF.

ENDMODULE.
FORM f_fieldinfo_get USING fu_tabname

fu_fieldname

CHANGING fwa_field_tab.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = fu_tabname
      fieldname      = fu_fieldname
      lfieldname     = fu_fieldname
    IMPORTING
      dfies_wa       = fwa_field_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno

    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM. " f_fieldinfo_get

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_COPIA
*&---------------------------------------------------------------------*
FORM preenche_copia .

  DATA: wl_zmmt0035_cop TYPE zmmt0035,
        tl_zmmt0037_cop TYPE TABLE OF zmmt0037 WITH HEADER LINE,
        wl_makt         TYPE makt.

  SELECT SINGLE *
   FROM zmmt0035
   INTO  wl_zmmt0035_cop
    WHERE  nro_sol_cp EQ wg_coplan-nro_sol_cp.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº de Solicitação não encontrada!'.
    LEAVE TO SCREEN 800.
  ELSE.
    MOVE-CORRESPONDING wl_zmmt0035_cop TO wg_coplan.

    SELECT *
      FROM zmmt0037
      INTO TABLE tl_zmmt0037_cop
      WHERE nro_sol_cp EQ wg_coplan-nro_sol_cp
      ORDER BY ebeln ebelp.



    LOOP AT tl_zmmt0037_cop .

      SELECT SINGLE *
        FROM makt
        INTO wl_makt
        WHERE matnr = tl_zmmt0037_cop-matnr
        AND spras   = sy-langu.

      tg_cop_item-matnr = tl_zmmt0037_cop-matnr.
      tg_cop_item-maktx = wl_makt-maktx.
      tg_cop_item-lgort = tl_zmmt0037_cop-lgort.
      tg_cop_item-charg = tl_zmmt0037_cop-charg.
      tg_cop_item-menge = tl_zmmt0037_cop-menge.
      tg_cop_item-meins = tl_zmmt0037_cop-meins.
      tg_cop_item-brtwr = tl_zmmt0037_cop-brtwr.

      APPEND tg_cop_item.
      CLEAR: tg_cop_item, wl_makt.
    ENDLOOP.

    CALL METHOD grid7->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_COPIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM executa_copia .
  DATA: v_lifnr     TYPE lfa1-lifnr,
        ws_zmmt0035 TYPE zmmt0035,
        ws_ekpo     TYPE ekpo,
        it_zmmt0037 TYPE TABLE OF zmmt0037,
        ws_zmmt0037 TYPE zmmt0037,
        it_zmmt0036 TYPE TABLE OF zmmt0036,
        ws_zmmt0036 TYPE zmmt0036,
        it_zmmt0038 TYPE TABLE OF zmmt0038,
        ws_zmmt0038 TYPE zmmt0038,
        it_zmmt0106 TYPE TABLE OF zmmt0106,
        ws_zmmt0106 TYPE zmmt0106,
        zebelp      TYPE ebelp.

  PERFORM limpa_campos.

  "Check dados cabecalho.
  CLEAR: ws_zmmt0035.
  SELECT SINGLE * FROM zmmt0035 INTO ws_zmmt0035 WHERE nro_sol_cp EQ wg_coplan-nro_sol_cp.


  "Itens produtos.
  FREE: it_zmmt0037.
  SELECT * FROM zmmt0037 INTO TABLE it_zmmt0037 WHERE nro_sol_cp EQ wg_coplan-nro_sol_cp.

  "Fatura.
  FREE: tg_fatura[].
  SELECT * FROM zmmt0036 INTO CORRESPONDING FIELDS OF TABLE tg_fatura WHERE nro_sol_cp EQ wg_coplan-nro_sol_cp.

  "Programação.
  FREE: tg_programa[].
  SELECT * FROM zmmt0038 INTO CORRESPONDING FIELDS OF TABLE tg_programa WHERE nro_sol_cp EQ wg_coplan-nro_sol_cp.

  "Parceiro.
  FREE: tg_parceiro[].
  SELECT * FROM zmmt0106 INTO CORRESPONDING FIELDS OF TABLE tg_parceiro WHERE nro_sol_cp EQ wg_coplan-nro_sol_cp.

  CLEAR: wg_coplan-nro_sol_cp.
  MOVE-CORRESPONDING wg_coplan TO wg_cadlan.

  PERFORM obtem_proximo.
  REFRESH: tg_fields.
  PERFORM trata_campos USING space
                             'GR2'
                              c_1
                              c_0.

  PERFORM trata_campos USING space
                            'GR1'
                             c_0
                             c_0.

  CALL METHOD obg_descbox->set_text_as_r3table
    EXPORTING
      table = tg_editor.

  CALL METHOD obg_descbox->set_readonly_mode
    EXPORTING
      readonly_mode = 0.

  "==========================================================
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wg_cadlan-lifnr
    IMPORTING
      output = v_lifnr.
  SELECT SINGLE *
    FROM lfa1
    INTO @DATA(wl_lfa1)
    WHERE lifnr = @v_lifnr.

  IF sy-subrc = 0.
    wg_cadlan-name1_l  = wl_lfa1-name1.
  ENDIF.

  SELECT SINGLE *
    FROM t001w
    INTO @DATA(wl_t001w)
    WHERE werks = @wg_cadlan-werks.
  IF sy-subrc = 0.
    wg_cadlan-name1_w  = wl_t001w-name1.
  ENDIF.

  SELECT SINGLE *
    FROM t052u
    INTO @DATA(wl_t052u)
    WHERE spras = 'P'
    AND zterm = @wg_cadlan-zterm.
  IF sy-subrc = 0.
    wg_cadlan-text1    = wl_t052u-text1.
  ENDIF.

  SELECT SINGLE *
    FROM t024
    INTO @DATA(wl_t024)
    WHERE ekgrp = @wg_cadlan-ekgrp.
  IF sy-subrc = 0.
    wg_cadlan-eknam   = wl_t024-eknam.
  ENDIF.

  SELECT SINGLE *
    FROM t161t
    INTO @DATA(wl_t161t)
    WHERE spras = 'P'
    AND   bsart = @wg_cadlan-bsart
    AND   bstyp = 'F'.

  IF sy-subrc = 0.
    wg_cadlan-batxt   = wl_t161t-batxt.
  ENDIF.
  "==========================================================

  CLEAR: zebelp.
  IF tg_cop_item[] IS NOT INITIAL.

    CALL METHOD grid7->get_selected_rows
      IMPORTING
        et_index_rows = tl_index_rows.


    IF tl_index_rows IS INITIAL.
      MESSAGE 'Selecione a linha.' TYPE 'I'.
      EXIT.
    ENDIF.

    LOOP AT tl_index_rows INTO wl_index_rows.

      "ABAP Produto.
      IF zebelp IS INITIAL.
        ADD 10 TO zebelp.
      ELSE.
        zebelp = zebelp + 10.
      ENDIF.


      READ TABLE  tg_cop_item INDEX  wl_index_rows.

      IF ws_zmmt0035 IS NOT INITIAL.
        CLEAR: ws_ekpo.
        READ TABLE it_zmmt0037 INTO ws_zmmt0037 WITH KEY matnr = tg_cop_item-matnr.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING ws_zmmt0037 TO tg_produto.
        ENDIF.
      ENDIF.

      "tg_produto-nro_sol_cp = wg_cadlan-nro_sol_cp.
      tg_produto-ebeln = ''.
      tg_produto-ebelp = zebelp."tg_produto-ebelp.
      tg_produto-matnr = tg_cop_item-matnr.
      tg_produto-maktx = tg_cop_item-maktx.
      tg_produto-lgort = tg_cop_item-lgort.

      IF tg_cop_item-charg IS NOT INITIAL.
        tg_produto-charg = tg_cop_item-charg.
      ENDIF.

      IF tg_cop_item-menge IS NOT INITIAL.
        tg_produto-menge = tg_cop_item-menge.
      ENDIF.

      IF tg_cop_item-meins IS NOT INITIAL.
        tg_produto-meins = tg_cop_item-meins.
      ENDIF.

      IF tg_cop_item-brtwr IS NOT INITIAL.
        tg_produto-brtwr = tg_cop_item-brtwr.
      ENDIF.

      IF tg_cop_item-netpr_roya IS NOT INITIAL.
        tg_produto-netpr_roya  = tg_cop_item-netpr_roya.
      ENDIF.

      IF tg_cop_item-netpr_germ IS NOT INITIAL.
        tg_produto-netpr_germ  = tg_cop_item-netpr_germ.
      ENDIF.

      IF wg_cadlan-bsart = 'ZSEM'.
        IF tg_cop_item-netpr_roya IS NOT INITIAL AND tg_cop_item-netpr_germ IS NOT INITIAL.
          tg_produto-brtwr = tg_cop_item-netpr_roya + tg_cop_item-netpr_germ.
        ENDIF.
      ENDIF.
      APPEND tg_produto.
    ENDLOOP.
    CLEAR: zebelp.
    wg_acao = c_modif.
    LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LISTAR_POSICAO_FINAN
*&---------------------------------------------------------------------*
FORM f_listar_posicao_finan .

  DATA: tl_zmmt0037_pf TYPE TABLE OF zmmt0037,
        tl_zmmt0035_pf TYPE TABLE OF zmmt0035,
        tl_ekbe_aux    TYPE TABLE OF ekbe,
        tl_ekbe        TYPE TABLE OF ekbe,
        tl_ekbe_23     TYPE TABLE OF ty_ekbe_pf,
        wl_ekbe        LIKE LINE OF  tl_ekbe_23,
        tl_bkpf        TYPE TABLE OF bkpf,
        tl_bsik        TYPE TABLE OF bsik,
        tl_bsak        TYPE TABLE OF bsak,
        tl_rbkp        TYPE TABLE OF rbkp,
        zebeln         TYPE ebeln,
        r_belnr        TYPE RANGE OF mblnr,
        t_ekko         TYPE TABLE OF ekko,
        t_bsak         TYPE TABLE OF bsak.

  FREE: tg_pos_finan, tl_zmmt0037_pf, tl_zmmt0035_pf, tl_bkpf.

  CHECK wg_cadlan-nro_sol_cp IS NOT INITIAL.

  SELECT  *
    FROM zmmt0037
    INTO TABLE tl_zmmt0037_pf
    WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.

  CHECK tl_zmmt0037_pf IS NOT INITIAL.

  SELECT  *
    FROM zmmt0035
    INTO TABLE tl_zmmt0035_pf
    WHERE nro_sol_cp = wg_cadlan-nro_sol_cp.


  SELECT *
    FROM ekbe
    INTO TABLE tl_ekbe_aux
    FOR ALL ENTRIES IN tl_zmmt0037_pf
    WHERE ebeln = tl_zmmt0037_pf-ebeln
        AND ebelp  = tl_zmmt0037_pf-ebelp
        AND vgabe IN ('2','3','4','c').


  SELECT *
  FROM ekbe
  APPENDING CORRESPONDING FIELDS OF TABLE tl_ekbe_aux
  FOR ALL ENTRIES IN tl_zmmt0035_pf
  WHERE ebeln = tl_zmmt0035_pf-ebeln
      AND vgabe IN ('2','3','4','c').

  LOOP AT  tl_ekbe_aux INTO DATA(wl_ekbe_aux).
    IF wl_ekbe_aux-vgabe = '4' OR wl_ekbe_aux-vgabe = 'C'.
      IF wl_ekbe_aux-belnr(2) = 15.
        APPEND wl_ekbe_aux  TO tl_ekbe.
        CLEAR wl_ekbe_aux .
      ENDIF.
    ELSE.
      "Busca de Dados Pedido de Compras
      SELECT SINGLE bukrs INTO wl_ekbe-bukrs
      FROM ekko
      WHERE ebeln =  wl_ekbe_aux-ebeln.

      wl_ekbe-belnr = wl_ekbe_aux-belnr.
      wl_ekbe-gjahr = wl_ekbe_aux-gjahr.
      wl_ekbe-ebeln = wl_ekbe_aux-ebeln.
      CONCATENATE wl_ekbe_aux-belnr wl_ekbe_aux-gjahr INTO wl_ekbe-awkey .

      APPEND wl_ekbe TO tl_ekbe_23.
      CLEAR wl_ekbe.
    ENDIF.
  ENDLOOP.

  IF tl_ekbe_23 IS NOT INITIAL.

    SELECT * FROM rbkp INTO TABLE tl_rbkp
    FOR ALL ENTRIES IN tl_ekbe_23
    WHERE belnr EQ tl_ekbe_23-belnr
      AND stblg NE space.
    .
    IF tl_rbkp IS NOT INITIAL.
      SORT tl_rbkp BY belnr.
      SORT tl_ekbe_23 BY belnr.
      r_belnr = VALUE #( FOR l IN tl_rbkp ( low = l-belnr sign = 'I' option = 'EQ' ) ).
      DELETE tl_ekbe_23 WHERE belnr IN r_belnr.
    ENDIF.

    CHECK tl_ekbe_23 IS NOT INITIAL.

    " Busca de Dados Documentos Contábil
    SELECT *
      FROM bkpf
      INTO TABLE tl_bkpf
      FOR ALL ENTRIES IN tl_ekbe_23
      WHERE bukrs  = tl_ekbe_23-bukrs
        AND gjahr	 = tl_ekbe_23-gjahr
        AND awkey	 = tl_ekbe_23-awkey "(ekbe-belnr+ ekbe-gjahr)
        AND blart NE  'ML'.

    " Busca de Dados Documentos Contábil em Aberto
    CLEAR: tl_bsik.
    SELECT *
      FROM bsik
      INTO TABLE tl_bsik
      FOR ALL ENTRIES IN tl_bkpf
      WHERE  bukrs = tl_bkpf-bukrs
        AND  gjahr = tl_bkpf-gjahr
        AND  belnr = tl_bkpf-belnr.

    " Busca de Dados Documentos Contábil Compensadas
    CLEAR: tl_bsak.
    SELECT *
      FROM bsak AS a
      INTO CORRESPONDING FIELDS OF TABLE tl_bsak
      FOR ALL ENTRIES IN tl_bkpf
      WHERE  a~bukrs = tl_bkpf-bukrs
        AND  a~gjahr = tl_bkpf-gjahr
        AND  a~belnr = tl_bkpf-belnr.


  ENDIF.

  LOOP AT tl_ekbe_23 INTO wl_ekbe.

    READ TABLE tl_bkpf INTO DATA(wl_bkpf) WITH KEY  bukrs = wl_ekbe-bukrs
                                                    gjahr = wl_ekbe-gjahr
                                                    awkey = wl_ekbe-awkey.

    tg_pos_finan-belnr_m =  wl_ekbe-belnr.
    tg_pos_finan-belnr_c  = wl_bkpf-belnr.


    LOOP AT  tl_bsik INTO DATA(wl_bsik) WHERE bukrs = wl_bkpf-bukrs
                                        AND   gjahr = wl_bkpf-gjahr
                                        AND   belnr = wl_bkpf-belnr.


      IF zebeln EQ wl_bsik-ebeln.
        CONTINUE.
      ENDIF.
      tg_pos_finan-ebeln = wl_bsik-ebeln.
      tg_pos_finan-xblnr = wl_bsik-xblnr.
      tg_pos_finan-blart = wl_bsik-blart.
      tg_pos_finan-budat = wl_bsik-budat.
      tg_pos_finan-augbl = wl_bsik-augbl.
      tg_pos_finan-augdt = wl_bsik-augdt.
      tg_pos_finan-dmbtr = wl_bsik-dmbtr.
      tg_pos_finan-dmbe2 = wl_bsik-dmbe2.
      IF wl_bsik-shkzg = 'H'.
        MULTIPLY tg_pos_finan-dmbtr  BY -1.
        MULTIPLY tg_pos_finan-dmbe2  BY -1.
      ENDIF.

      IF wl_bsik-augbl IS INITIAL.
        tg_pos_finan-status = 'Em Aberto'.
      ELSE.
        tg_pos_finan-status = 'Liquidado'.
      ENDIF.

      APPEND tg_pos_finan.
    ENDLOOP.

    LOOP AT  tl_bsak INTO DATA(wl_bsak) WHERE bukrs = wl_bkpf-bukrs
                                        AND   gjahr = wl_bkpf-gjahr
                                        AND   belnr = wl_bkpf-belnr.

      IF zebeln EQ wl_bsak-ebeln.
        CONTINUE.
      ENDIF.
      tg_pos_finan-ebeln = wl_bsak-ebeln.
      tg_pos_finan-xblnr = wl_bsak-xblnr.
      tg_pos_finan-blart = wl_bsak-blart.
      tg_pos_finan-budat = wl_bsak-budat.
      tg_pos_finan-augbl = wl_bsak-augbl.
      tg_pos_finan-augdt = wl_bsak-augdt.
      tg_pos_finan-dmbtr = wl_bsak-dmbtr.
      tg_pos_finan-dmbe2 = wl_bsak-dmbe2.
      IF wl_bsak-shkzg = 'H'.
        MULTIPLY tg_pos_finan-dmbtr  BY -1.
        MULTIPLY tg_pos_finan-dmbe2  BY -1.
      ENDIF.

      IF wl_bsak-augbl IS INITIAL.
        tg_pos_finan-status = 'Em Aberto'.
      ELSE.
        tg_pos_finan-status = 'Liquidado'.
      ENDIF.

      zebeln = wl_ekbe-ebeln.

      APPEND tg_pos_finan.
    ENDLOOP.

  ENDLOOP.

  "Selecionando pedido.
  IF tl_ekbe IS NOT INITIAL.
    READ TABLE tl_ekbe INTO DATA(wa_ekbe) INDEX 1.
    SELECT SINGLE bukrs INTO @DATA(vbukrs)
      FROM ekko
      WHERE ebeln =  @wa_ekbe-ebeln.

    SELECT *
      FROM bsak
      INTO TABLE t_bsak
        FOR ALL ENTRIES IN  tl_ekbe
     WHERE bukrs = vbukrs
       AND augbl = tl_ekbe-belnr
       AND gjahr = tl_ekbe-gjahr.
  ENDIF.

  CLEAR: wl_ekbe, tg_pos_finan.
  SORT tl_ekbe BY belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM tl_ekbe COMPARING belnr gjahr.
  LOOP AT tl_ekbe INTO DATA(w_ekbe).
    LOOP AT t_bsak INTO DATA(w_bsak) WHERE augbl = w_ekbe-belnr
                                     AND   gjahr = w_ekbe-gjahr.
      tg_pos_finan-belnr_m  = w_ekbe-belnr.
      tg_pos_finan-belnr_c  = w_bsak-belnr.
      tg_pos_finan-ebeln    = w_ekbe-ebeln.
      tg_pos_finan-xblnr    = w_bsak-xblnr.
      tg_pos_finan-budat    = w_bsak-budat.
      tg_pos_finan-augbl    = w_ekbe-belnr.
      tg_pos_finan-augdt    = w_bsak-budat.
      tg_pos_finan-dmbtr    = w_bsak-dmbtr.
      tg_pos_finan-dmbe2    = w_bsak-dmbe2.
      tg_pos_finan-blart    = w_bsak-blart.
      tg_pos_finan-status   = 'Liquidado'.
      APPEND tg_pos_finan.
    ENDLOOP.
    CLEAR: tg_pos_finan, w_bsak, w_ekbe.
  ENDLOOP.


  IF tg_pos_finan[] IS NOT INITIAL.

    CALL SCREEN 0900  STARTING AT 010 3
                   ENDING   AT 130 018.

  ELSE.
    MESSAGE 'Dados de Posição Financeira não econtrados' TYPE 'I'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0900 OUTPUT.

  SET PF-STATUS 'Z003'.
  SET TITLEBAR '0900'.
  "
  IF g_custom_cont_pos_finan IS INITIAL.
    CLEAR wa_layout.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_layout-col_opt    = c_x.
    wa_stable-row        = c_x.
    wa_layout-sel_mode   = 'B'.
    wa_layout-cwidth_opt = c_x.
*    wa_layout-box_fname  = 'MARK'.
*    wa_layout-stylefname = 'STYLE'.
    wa_layout-no_toolbar = c_x.


    CREATE OBJECT g_custom_cont_pos_finan
      EXPORTING
        container_name = g_container_pos_finan.

    CREATE OBJECT grid9
      EXPORTING
        i_parent = g_custom_cont_pos_finan.

    PERFORM montar_layout_pos_finan.

*    REFRESH: tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    APPEND wl_function TO tl_function.


    CALL METHOD grid9->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_pos_finan[].

    CALL METHOD grid9->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid9->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD grid9->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0900 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN  'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      SET SCREEN 0.
  ENDCASE.
*  CLEAR: ok_code.
**  ok_code = sy-ucomm.
*  CASE sy-ucomm.
*    WHEN 'SAIR'.
*
*      IF grid9 IS NOT INITIAL.
*        CALL METHOD grid9->free.
*
*        IF g_custom_cont_pos_finan IS NOT INITIAL.
*          CALL METHOD g_custom_cont_pos_finan->free.
*        ENDIF.
*
*        FREE: grid9.
*        FREE: g_custom_cont_pos_finan.
*
*      ENDIF.
*
*      LEAVE TO SCREEN 0.
*    WHEN 'EXIT'.
*
*      IF grid9 IS NOT INITIAL.
*        CALL METHOD grid9->free.
*
*        IF g_custom_cont_pos_finan IS NOT INITIAL.
*          CALL METHOD g_custom_cont_pos_finan->free.
*        ENDIF.
*
*        FREE: grid9.
*        FREE: g_custom_cont_pos_finan.
*
*      ENDIF.
*
*      LEAVE TO SCREEN 0.
*  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATLOG_LOG
*&---------------------------------------------------------------------*
FORM build_fieldcatlog_log .
  CLEAR:w_fieldcat,i_fieldcat[].

  PERFORM build_fcatalog_log USING:
           'NRO_SOL_CP'  'ZMMT0037_LOG' 'Nro.Sol.Compras',
           'EBELP'       'ZMMT0037_LOG' 'Item Nv.',
           'EBELN'       'ZMMT0037_LOG' 'Pedido Novo',
           'EBELN_ORI'   'ZMMT0037_LOG' 'Pedido Anterior',
           'EBELP_ORI'   'ZMMT0037_LOG' 'Item Ant.',
           'CAMPO_ORI'   'ZMMT0037_LOG' 'Campo Técnico Modificado',
           'CAMPO_MOD'   'ZMMT0037_LOG' 'Campo Modificado',
           'TIPO'        'ZMMT0037_LOG' 'Tipo',
           'VALOR_ORI'   'ZMMT0037_LOG' 'Valor Anterior',
           'VALOR_NOVO'  'ZMMT0037_LOG' 'Valor Novo',
           'USNAM'       'ZMMT0037_LOG' 'Usuário',
           'DATA_ATUAL'  'ZMMT0037_LOG' 'Dt.Modif.',
           'HORA_ATUAL'  'ZMMT0037_LOG' 'Hr.Modif.'.


ENDFORM.                    "BUILD_FIELDCATLOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCATALOG
*&---------------------------------------------------------------------*
FORM build_fcatalog_log USING l_field l_tab l_text.

  w_fieldcat-fieldname      = l_field.
  w_fieldcat-tabname        = l_tab.
  w_fieldcat-seltext_m      = l_text.

  APPEND w_fieldcat TO i_fieldcat.
  CLEAR w_fieldcat.

ENDFORM.                    " build_fieldcatlog
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
FORM build_layout_log .

  w_layout-colwidth_optimize = 'X'.
  w_layout-zebra             = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_LOG_SISTEMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pf_log_sistema .

  DATA:   tl_input_zmmt0035_log     TYPE TABLE OF zmmt0035_log WITH HEADER LINE.
  DATA:   tl_input_zmmt0037_log     TYPE TABLE OF zmmt0037_log WITH HEADER LINE.

  FREE: tl_input_zmmt0035_log, tl_input_zmmt0037_log.

*** PBI - 60951 - Inicio
  IF wg_cadlan <> wg_cadlan_old.

    IF wg_cadlan-safra NE wg_cadlan_old-safra.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                  wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                  wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                  wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                  'A'                    TO tl_input_zmmt0035_log-tipo,
                  sy-uname               TO tl_input_zmmt0035_log-usnam,
                  sy-datum               TO tl_input_zmmt0035_log-data_atual,
                  sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.


      tl_input_zmmt0035_log-campo_ori = 'SAFRA'.
      tl_input_zmmt0035_log-campo_mod = 'Safra'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-safra.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-safra.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-bstyp NE wg_cadlan_old-bstyp.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'BSTYP'.
      tl_input_zmmt0035_log-campo_mod = 'Cat.doc compras'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-bstyp.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-bstyp.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-bsart NE wg_cadlan_old-bsart.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'BSART'.
      tl_input_zmmt0035_log-campo_mod = 'Tipo doc. compras'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-bsart.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-bsart.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.


    IF wg_cadlan-batxt NE wg_cadlan_old-batxt.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'BATXT'.
      tl_input_zmmt0035_log-campo_mod = 'Desc doc compra'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-batxt.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-batxt.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-ekgrp NE wg_cadlan_old-ekgrp.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'EKGRP'.
      tl_input_zmmt0035_log-campo_mod = 'Grupo compradores'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-ekgrp.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-ekgrp.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.

    IF wg_cadlan-eknam NE wg_cadlan_old-eknam.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'EKNAM'.
      tl_input_zmmt0035_log-campo_mod = 'Desc.grp compradores'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-eknam.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-eknam.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-werks NE wg_cadlan_old-werks.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'WERKS'.
      tl_input_zmmt0035_log-campo_mod = 'Centro'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-werks.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-werks.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-name1_w NE wg_cadlan_old-name1_w.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'NAME1_W'.
      tl_input_zmmt0035_log-campo_mod = 'Desc centro'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-name1_w.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-name1_w.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-lifnr NE wg_cadlan_old-lifnr.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'LIFNR'.
      tl_input_zmmt0035_log-campo_mod = 'Nº.conta fornecedor'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-lifnr.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-lifnr.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "
    IF wg_cadlan-name1_l NE wg_cadlan_old-name1_l.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'NAME1_L'.
      tl_input_zmmt0035_log-campo_mod = 'Desc fornecedor'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-name1_l.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-name1_l.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-ped_forn NE wg_cadlan_old-ped_forn.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'PED_FORN'.
      tl_input_zmmt0035_log-campo_mod = 'Pedido'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-ped_forn.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-ped_forn.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "
    IF wg_cadlan-waers NE wg_cadlan_old-waers.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'WAERS'.
      tl_input_zmmt0035_log-campo_mod = 'Código da moeda'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-waers.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-waers.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-wkurs NE wg_cadlan_old-wkurs.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'WKURS'.
      tl_input_zmmt0035_log-campo_mod = 'Taxa de câmbio'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-wkurs.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-wkurs.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-zterm NE wg_cadlan_old-zterm.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori =  'ZTERM'.
      tl_input_zmmt0035_log-campo_mod =  'Chave cond.pagamento'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-zterm.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-zterm.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.


    IF wg_cadlan-ebeln NE wg_cadlan_old-ebeln.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'EBELN'.
      tl_input_zmmt0035_log-campo_mod = 'Nº doc. compras'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-ebeln.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-ebeln.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-ebeln_son NE wg_cadlan_old-ebeln_son.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori =  'EBELN_SON'.
      tl_input_zmmt0035_log-campo_mod =  'Nº doc. compras'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-ebeln_son.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-ebeln_son.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-banfn NE wg_cadlan_old-banfn.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'BANFN'.
      tl_input_zmmt0035_log-campo_mod = 'Nº requis.compra'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-banfn.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-banfn.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-ihran NE wg_cadlan_old-ihran.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'IHRAN'.
      tl_input_zmmt0035_log-campo_mod = 'Dt p/entrega cotação'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-ihran.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-ihran.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-texto_neg NE wg_cadlan_old-texto_neg.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'TEXTO_NEG'.
      tl_input_zmmt0035_log-campo_mod = 'texto de negociação'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-texto_neg.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-texto_neg.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-lifnr_n NE wg_cadlan_old-lifnr_n.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori =  'LIFNR_N'.
      tl_input_zmmt0035_log-campo_mod =  'Nº.conta fornecedor'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-lifnr_n.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-lifnr_n.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-name1_n NE wg_cadlan_old-name1_n.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori =  'NAME1_N'.
      tl_input_zmmt0035_log-campo_mod =  'Desc fornecedor'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-name1_n.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-name1_n.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-inco1 NE wg_cadlan_old-inco1.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori =  'INCO1'.
      tl_input_zmmt0035_log-campo_mod =  'Incoterms parte 1'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-inco1.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-inco1.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.
    "

    IF wg_cadlan-inco2 NE wg_cadlan_old-inco2.
      MOVE :      sy-mandt               TO tl_input_zmmt0035_log-mandt,
                wg_cadlan-nro_sol_cp   TO tl_input_zmmt0035_log-nro_sol_cp,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln,
                wg_cadlan-ebeln        TO tl_input_zmmt0035_log-ebeln_ori,
                'A'                    TO tl_input_zmmt0035_log-tipo,
                sy-uname               TO tl_input_zmmt0035_log-usnam,
                sy-datum               TO tl_input_zmmt0035_log-data_atual,
                sy-uzeit               TO tl_input_zmmt0035_log-hora_atual.

      tl_input_zmmt0035_log-campo_ori = 'INCO2'.
      tl_input_zmmt0035_log-campo_mod = 'Incoterms parte 2'.
      tl_input_zmmt0035_log-valor_ori = wg_cadlan_old-inco2.
      tl_input_zmmt0035_log-valor_novo = wg_cadlan-inco2.
      APPEND tl_input_zmmt0035_log.
      CLEAR: tl_input_zmmt0035_log.
    ENDIF.

    MODIFY zmmt0035_log FROM TABLE tl_input_zmmt0035_log.

  ENDIF.

*** PBI - 60951 - Fim

* "// BUG 63039 PBI 60952 - CS2020001160
  FREE tl_input_zmmt0037_log.

  LOOP AT tg_produto.

    READ TABLE tg_produto_old WITH KEY ebeln = tg_produto-ebeln
                                       ebelp = tg_produto-ebelp.

    IF tg_produto NE tg_produto_old.

      IF tg_produto-matnr NE tg_produto_old-matnr.

        MOVE:
        sy-mandt              TO tl_input_zmmt0037_log-mandt,
        wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
        wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
        tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
        'A'                   TO tl_input_zmmt0037_log-tipo,
        sy-uname              TO tl_input_zmmt0037_log-usnam,
        sy-datum              TO tl_input_zmmt0037_log-data_atual,
        sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.



        tl_input_zmmt0037_log-campo_ori = 'MATNR'.
        tl_input_zmmt0037_log-campo_mod = 'Cód Material'.
        tl_input_zmmt0037_log-valor_ori = tg_produto_old-matnr.
        tl_input_zmmt0037_log-valor_novo = tg_produto-matnr.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-lgort NE tg_produto_old-lgort.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori = 'lgort'.
        tl_input_zmmt0037_log-campo_mod = 'Depósito'.
        tl_input_zmmt0037_log-valor_ori = tg_produto_old-lgort.
        tl_input_zmmt0037_log-valor_novo = tg_produto-lgort.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-charg NE tg_produto_old-charg.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori = 'CHARG'.
        tl_input_zmmt0037_log-campo_mod = 'Lote'.
        tl_input_zmmt0037_log-valor_ori = tg_produto_old-charg.
        tl_input_zmmt0037_log-valor_novo = tg_produto-charg.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-menge NE tg_produto_old-menge.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'MENGE'.
        tl_input_zmmt0037_log-campo_mod  = 'Quantidade'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-menge.
        tl_input_zmmt0037_log-valor_novo = tg_produto-menge.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-netpr_roya NE tg_produto_old-netpr_roya.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'NETPR_ROYA'.
        tl_input_zmmt0037_log-campo_mod  = 'Royalties'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-netpr_roya.
        tl_input_zmmt0037_log-valor_novo = tg_produto-netpr_roya.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-netpr_germ NE tg_produto_old-netpr_germ.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'NETPR_GERM'.
        tl_input_zmmt0037_log-campo_mod  = 'Germoplasma'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-netpr_germ.
        tl_input_zmmt0037_log-valor_novo = tg_produto-netpr_germ.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.


      IF tg_produto-brtwr NE tg_produto_old-brtwr.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'BRTWR'.
        tl_input_zmmt0037_log-campo_mod  = 'Preço Bruto'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-brtwr.
        tl_input_zmmt0037_log-valor_novo = tg_produto-brtwr.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.


      IF tg_produto-bicms NE tg_produto_old-bicms.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'BICMS'.
        tl_input_zmmt0037_log-campo_mod  = 'Base ICMS'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-bicms.
        tl_input_zmmt0037_log-valor_novo = tg_produto-bicms.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.


      IF tg_produto-picms NE tg_produto_old-picms.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'PICMS'.
        tl_input_zmmt0037_log-campo_mod  = '%ICMS'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-picms.
        tl_input_zmmt0037_log-valor_novo = tg_produto-picms.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.


      IF tg_produto-peinh NE tg_produto_old-peinh.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'PEINH'.
        tl_input_zmmt0037_log-campo_mod  = 'Por'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-peinh.
        tl_input_zmmt0037_log-valor_novo = tg_produto-peinh.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-netpr_frete NE tg_produto_old-netpr_frete.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'NETPR_FRETE'.
        tl_input_zmmt0037_log-campo_mod  = 'Valor Frete'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-netpr_frete.
        tl_input_zmmt0037_log-valor_novo = tg_produto-netpr_frete.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-qtde_troca NE tg_produto_old-qtde_troca.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'QTDE_TROCA'.
        tl_input_zmmt0037_log-campo_mod  = 'Quantidade'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-qtde_troca.
        tl_input_zmmt0037_log-valor_novo = tg_produto-qtde_troca.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-netpr NE tg_produto_old-netpr.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'NETPR'.
        tl_input_zmmt0037_log-campo_mod  = 'Valor'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-netpr.
        tl_input_zmmt0037_log-valor_novo = tg_produto-netpr.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-netpr_desc NE tg_produto_old-netpr_desc.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'NETPR_DESC'.
        tl_input_zmmt0037_log-campo_mod  = 'Valor Desconto'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-netpr_desc.
        tl_input_zmmt0037_log-valor_novo = tg_produto-netpr_desc.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-netpr_supl NE tg_produto_old-netpr_supl.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'NETPR_SUPL'.
        tl_input_zmmt0037_log-campo_mod  = 'Valor Supl'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-netpr_supl.
        tl_input_zmmt0037_log-valor_novo = tg_produto-netpr_supl.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-mwskz NE tg_produto_old-mwskz.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'MWSKZ'.
        tl_input_zmmt0037_log-campo_mod  = 'Código do IVA'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-mwskz.
        tl_input_zmmt0037_log-valor_novo = tg_produto-mwskz.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.

      IF tg_produto-bprme NE tg_produto_old-bprme.
        MOVE:
     sy-mandt              TO tl_input_zmmt0037_log-mandt,
     wg_cadlan-nro_sol_cp  TO tl_input_zmmt0037_log-nro_sol_cp,
*      wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln,
*      tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp,
     wg_cadlan-ebeln       TO tl_input_zmmt0037_log-ebeln_ori,
     tg_produto_old-ebelp  TO tl_input_zmmt0037_log-ebelp_ori,
     'A'                   TO tl_input_zmmt0037_log-tipo,
     sy-uname              TO tl_input_zmmt0037_log-usnam,
     sy-datum              TO tl_input_zmmt0037_log-data_atual,
     sy-uzeit              TO tl_input_zmmt0037_log-hora_atual.

        tl_input_zmmt0037_log-campo_ori  = 'BPRME'.
        tl_input_zmmt0037_log-campo_mod  = 'Unid preço pedido'.
        tl_input_zmmt0037_log-valor_ori  = tg_produto_old-bprme.
        tl_input_zmmt0037_log-valor_novo = tg_produto-bprme.
        APPEND tl_input_zmmt0037_log.
        CLEAR: tl_input_zmmt0037_log.
      ENDIF.
    ENDIF.

    MODIFY zmmt0037_log FROM TABLE tl_input_zmmt0037_log.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_ADIANTAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_dados_adiantamento USING wg_adto LIKE LINE OF tg_adto.

  DATA: w_zmmt0147 TYPE zmmt0147.


  "Registras informações adiantamento.
  w_zmmt0147 = VALUE #(
                      nro_sol_cp = wg_cadlan-nro_sol_cp
                      ebeln      = wg_adto-ebeln
                      ebelp      = wg_adto-ebelp
                      perc_adto  = wg_adto-perc_adto
                      vlr_adto   = wg_adto-vlr_adto
                      dt_vcto    = wg_adto-dt_vcto
                      dep_resp   = wg_adto-dep_resp
                      resp_neg   = wg_adto-resp_neg
                      dt_atual   = sy-datum
                      hr_atual   = sy-uzeit
                      usnam      = sy-uname  ).
  IF w_zmmt0147 IS NOT INITIAL.
    MODIFY zmmt0147 FROM w_zmmt0147.
    COMMIT WORK.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0201 OUTPUT.
  SET PF-STATUS 'STATUS_0201'.
  SET TITLEBAR 'TITLE_0201'.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0201  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0201 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_ADIANT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_adiant OUTPUT.
  DATA:
    t_function TYPE ui_functions,
    w_function LIKE tl_function WITH HEADER LINE.

  wa_layout-zebra       = 'X'.
  wa_stable-row         = 'X'.
  wa_layout-cwidth_opt  = 'X'.
  wa_layout-info_fname  = space .
  CLEAR  wa_layout-stylefname.

  wa_stable-row         = c_x.
  wa_stable-col         = c_x.

  IF  g_custom_status_adiant IS INITIAL.
    CREATE OBJECT g_custom_status_adiant
      EXPORTING
        container_name = g_container_status_adiant.

    CREATE OBJECT grid10
      EXPORTING
        i_parent = g_custom_status_adiant.

    PERFORM montar_layout_status_adiant.

    APPEND cl_gui_alv_grid=>mc_fc_excl_all  TO gt_exc_button.


    CALL METHOD grid10->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = gt_exc_button
*       it_toolbar_excluding = t_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_status_adiant[].


    SET HANDLER obg_toolbar->on_hotspot_click  FOR grid10.

    CALL METHOD grid10->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid10->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid10->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ELSE.
    CALL METHOD grid10->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_STATUS_ADIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_status_adiant .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura_status USING:
*       1 '     '            'ICON'           'TG_ADTO'  'ICON'         'Status'           '15' ' '    ' ' ' ' '',
       1 'ZMMT0037'         'EBELN'          'TG_ADTO'  'EBELN'        'Pedido'           '10' ' '    ' ' ' ' '',
*       1 'ZMMT0037'         'SALDO'          'TG_ADTO'  'SALDO'        'Saldo Adto'       '12' ''     ' ' ' ' '',
       1 'ZMMT0147'         'PERC_ADTO'      'TG_ADTO'  'PERC_ADTO'    '% Solic.Adto'     '20' ' '    'X' ' ' '',
       1 'ZMMT0147'         'VLR_ADTO'       'TG_ADTO'  'VLR_ADTO'     'Vlr.Adiantamento' '20' ' '    'X' ' ' '',
       1 'ZMMT0147'         'DT_VCTO'        'TG_ADTO'  'DT_VCTO'      'Dt.Vencimento'    '20' ' '    ' ' ' ' '',
       1 'ZFIT0045'         'RESP_NEG'       'TG_ADTO'  'RESP_NEG'     'Responsável pela Negociação'  '20' ' '    ' ' ' ' '',
       1 'ZFIT0045'         'DEP_RESP'       'TG_ADTO'  'DEP_RESP'     'Departamento'     '20' ' '    ' ' ' ' '',
       1 'ZFIT0046'         'NRO_SOL'        'TG_ADTO'  'NRO_SOL'      'Nro.Sol.Adto'     '20' ' '    ' ' ' ' 'X',
       1 'ZFIT0045'         'STATUS'         'TG_ADTO'  'STATUS'       'Status Aprov.'    '20' ' '    ' ' ' ' '',
       1 'ZFIT0045'         'BELNR'          'TG_ADTO'  'BELNR'        'Doc.Contabil'     '20' ' '    ' ' ' ' '',
       1 'BSAK'             'AUGBL'          'TG_ADTO'  'AUGBL'        'Doc.Pgto'         '20' ' '    ' ' ' ' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_8111   text
*      -->P_8112   text
*      -->P_8113   text
*      -->P_8114   text
*      -->P_8115   text
*      -->P_8116   text
*      -->P_8117   text
*      -->P_8118   text
*      -->P_8119   text
*----------------------------------------------------------------------*
FORM montar_estrutura_status  USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize)
                            VALUE(p_hotspot).


  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.
  w_fieldcatalog-hotspot       = p_hotspot.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SALDO_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ZCHECK  text
*----------------------------------------------------------------------*
FORM f_check_saldo_pedido USING wg_adto  LIKE LINE OF tg_adto CHANGING p_zcheck.

  "Verificando valor total do produto.
  CLEAR: zvalor_produto, p_zcheck, zvalor_adto.
  FREE: tg_zmmt0147[].
  IF tg_produto[] IS NOT INITIAL.
    LOOP AT tg_produto INTO DATA(w_produto).
      ADD w_produto-valor TO zvalor_produto.
    ENDLOOP.
  ENDIF.


**=============================INICIO BUG 71354 / Anderson Oenning - 12/01/2022
  "Verificando saldo adiantamento.
  IF wg_cadlan-nro_sol_cp IS NOT INITIAL.
    SELECT * FROM zfit0046
    INTO TABLE @DATA(tg_zfit0046) WHERE nro_sol_cp EQ @wg_cadlan-nro_sol_cp
      AND EXISTS ( SELECT * FROM zfit0045
                     WHERE zfit0045~nro_sol = zfit0046~nro_sol
                     AND   zfit0045~loekz EQ '' ).
    IF tg_zfit0046[] IS NOT INITIAL.
      LOOP AT tg_zfit0046[] INTO DATA(w_zfit0046).
        ADD w_zfit0046-vlr_adiantamento TO zvalor_adto.
      ENDLOOP.
    ENDIF.
  ENDIF.

  zvalor_adto = zvalor_adto + wg_adto-vlr_adto.
  IF zvalor_adto LE zvalor_produto.
    p_zcheck = abap_false.
  ELSE.
    p_zcheck = abap_true.
  ENDIF.

  FREE: tg_zfit0046[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_exclude .

* Exclude function codes from standard GUI status
*  MOVE '&ALL' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&SAL' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&REFRESH' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&OUP' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&ILT' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '%SL' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&OL0' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&OAD' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&AVE' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&EB9' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&ODN' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&SUM' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&VEXCEL' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&AQW' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '%PC' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&RNT_PREV' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&GRAPH' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&CRDESIG' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&ABC' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&CRBATCH' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&XXL' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&RNT' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&CRTEMPL' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&URL' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&VLOTUS' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&VCRYSTAL' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&OLX' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&CFI' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.
*  MOVE '&CDF' TO ls_exclude-fcode.
*  INSERT ls_exclude INTO TABLE lt_exclude.

ENDFORM.

*&--------------------------------------------------------------------*
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
FORM set_pf_status                                          "#EC CALLED
  USING
    rt_extab TYPE slis_t_extab.

  DATA: et TYPE slis_extab.
*  et-fcode = '&NFO'. APPEND et TO rt_extab.
*  et-fcode = '&CRB'. APPEND et TO rt_extab.
*  et-fcode = '&CRE'. APPEND et TO rt_extab.
*  et-fcode = 'HTML'. APPEND et TO rt_extab.
*  et-fcode = '&ETA'. APPEND et TO rt_extab.
*  et-fcode = '&ABC'. APPEND et TO rt_extab.
*  et-fcode = '&CRL'. APPEND et TO rt_extab.
*  et-fcode = '&CRR'. APPEND et TO rt_extab.
*  et-fcode = '&UMC'. APPEND et TO rt_extab.
*  et-fcode = '&AQW'. APPEND et TO rt_extab.
*  et-fcode = '&EB9'. APPEND et TO rt_extab.
*  et-fcode = '&LFO'. APPEND et TO rt_extab.
*  et-fcode = '&SUM'. APPEND et TO rt_extab.
*  et-fcode = '&XXL'. APPEND et TO rt_extab.
*  et-fcode = '&OMP'. APPEND et TO rt_extab.
*  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
ENDFORM.                    "SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0901  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0901 OUTPUT.
  SET PF-STATUS 'ST0901'.
  SET TITLEBAR 'TIT0901'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0901  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0901 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0901  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_0901 OUTPUT.

  REFRESH it_fcat.
  PERFORM fm_mont_layout_0300.
  wa_layout-cwidth_opt = abap_true.

  IF ( obj_custom_0300 IS INITIAL ).
    CREATE OBJECT obj_custom_0300
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0300
      EXPORTING
        i_parent          = obj_custom_0300
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF vl_resposta = 1.

    CALL METHOD obj_alv_0300->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        it_toolbar_excluding          = gt_exc_button
        i_save                        = 'A'
      CHANGING
        it_fieldcatalog               = it_fcat
        it_outtab                     = t_zmmt0035_log
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.

    CALL METHOD obj_alv_0300->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        it_toolbar_excluding          = gt_exc_button
        i_save                        = 'A'
      CHANGING
        it_fieldcatalog               = it_fcat
        it_outtab                     = t_zmmt0037_log
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDIF.

  CALL METHOD obj_alv_0300->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0300->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_MONT_LAYOUT_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_mont_layout_0300 .

  PERFORM alv_preenche_cat USING:
       'NRO_SOL_CP ' 'Nro.Sol.Compras         ' '10'  ''  ''  ''  '' '' '' '' '' '',
       'EBELP      ' 'Item Nv                 ' '10'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'EBELN      ' 'Pedido Novo             ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'EBELN_ORI  ' 'Pedido Anterior         ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'EBELP_ORI  ' 'Item Ant                ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'CAMPO_ORI  ' 'Campo Técnico Modificado' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'CAMPO_MOD  ' 'Campo Modificado        ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'TIPO       ' 'Tipo                    ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'VALOR_ORI  ' 'Valor Anterior          ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'VALOR_NOVO ' 'Valor Novo              ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'USNAM      ' 'Usuário                 ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'DATA_ATUAL ' 'Dt.Modif                ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' ',
       'HORA_ATUAL ' 'Hr.Modif                ' '15'  ''  ''  ''  ' ' ' '  '' '' '' ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
FORM alv_preenche_cat    USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.
  CLEAR: wa_layout, wl_fcat.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-edit      = p_edit.
  wl_fcat-checkbox  = p_check.
  wl_fcat-ref_table = p_ref_tabname.
  wl_fcat-ref_field = p_ref_fieldname.
  wl_fcat-tabname   = p_ref_tabname.
  wl_fcat-no_out    = p_no_out.
  APPEND wl_fcat TO it_fcat.
ENDFORM.                    "ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ZSON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4042   text
*----------------------------------------------------------------------*
FORM verifica_zson  USING v_linha v_pedido v_solic.

  DATA: v_ekko     TYPE ekko,
        v_ekpo     TYPE ekpo,
        v_ref      TYPE ekko-ihrez,
        v_zmmt0037 TYPE zmmt0037.

  IF v_pedido IS INITIAL.
    EXIT.
  ENDIF.

  SELECT SINGLE * INTO v_zmmt0037 FROM zmmt0037
    WHERE nro_sol_cp EQ v_solic
    AND ebeln EQ v_pedido.

  IF sy-subrc NE 0.

    SELECT SINGLE * INTO v_ekko FROM ekko WHERE ebeln EQ v_pedido.

    IF sy-subrc EQ 0 .
      IF v_ekko-bsart EQ 'ZSON' AND v_ekko-ernam EQ sy-uname.

        CONCATENATE 'ABAP' v_linha INTO  v_ref.

        UPDATE ekko
            SET ihrez = v_ref
            WHERE ebeln EQ v_pedido.

        UPDATE ekpo
           SET eglkz = 'X'
               elikz = 'X'
               erekz = 'X'
               loekz = 'X'
           WHERE ebeln EQ v_pedido.

        COMMIT WORK.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SOLICITACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM fm_dados_solicitacao  USING    p_row
                                    p_column.

  DATA: tg_adiant TYPE TABLE OF ty_status_adiant.

  FREE: tg_adiant.
  tg_adiant = tg_status_adiant[].

  TRY .
      DATA(wa_status_adiant) = tg_adiant[ p_row ].
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

*  FREE tg_adiant.
*  tg_adiant[] = tg_status_adiant[].

  CASE  p_column.
    WHEN 'NRO_SOL'.
      READ TABLE tg_adiant ASSIGNING FIELD-SYMBOL(<w_status_adiant>) WITH KEY nro_sol = wa_status_adiant-nro_sol.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'Z_FUC_EXIBIR_ZFI0025'
          EXPORTING
            i_nro_sol = <w_status_adiant>-nro_sol.
      ENDIF.

  ENDCASE.

ENDFORM.
