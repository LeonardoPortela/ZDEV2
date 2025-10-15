*&---------------------------------------------------------------------*
*&  Include           ZIM12_TOP
*&---------------------------------------------------------------------*
 report zim13.

 tables: bsis,  zsys_inves, bkpf, tka01, aufk.
 include <cl_alv_control>.

**&--------------------------------------------------------------------&*
**& Estruturas                                                         &*
**&--------------------------------------------------------------------&*
 data: begin of it_msg occurs 0.
         include structure bdcmsgcoll.
 data: end of it_msg.
 data: wl_mode(1).

 data: begin of t_ap_inv occurs 0.
         include structure zim01_sol_ap_inv.
 data: end of t_ap_inv.

 data: begin of t_08 occurs 0.
         include structure zim08_rel_inv2.
 data: end of t_08.

 data: begin of t_08_usd occurs 0.
         include structure zim08_rel_inv_us.
 data: end of t_08_usd.

 data: begin of t_inv occurs 0.
         include structure zim01_sol_ap_inv.
 data: end of t_inv.

 data: begin of t_inv_aux occurs 0.
         include structure zim01_sol_ap_inv.
 data: end of t_inv_aux.

**********************************************************************
* Filtro versão - PSA
 data: wa_field like sval,
       li_field type standard table of sval,
       p_versio type rkcsp-versn.
 data retorno_filtro(1) type c.

**************************************************************************************************
 "EXPORTAÇÃO
**************************************************************************************************
 " CS2019000495
 "Precisa criar uma interface do SAP que envia os dados para o Sysphera.
 "Os campos necessários são os da tabela abaixo, este dados devem ser enviados para uma Stage,
 "conforme o segundo print. Este envio deve ser parametrizado no SAP de uma forma no qual o usuario chave
 "possa definir a periodicidade do envio dos dados, informando alguns parametros, como:
 " ano, mes e periodicidade, sendo possivel gerar o JOB para fazer isso de forma automatica a cada periodo.

 "Os dados devem ter origem do SAP na transação KSB1.

 " CS2019000506
 "Precisa criar uma interface do SAP que envia os dados para o Sysphera.
 "Os campos necessários são os da tabela abaixo, este dados devem ser enviados para uma Stage,
 "conforme o segundo print. Este envio deve ser parametrizado no SAP de uma forma no qual o usuario chave
 "possa definir a periodicidade do envio dos dados, informando alguns parametros, como:
 "ANO, MES, CENTRO_CUSTO e periodicidade, sendo possivel gerar o JOB para fazer isso de forma automatica a cada periodo.

 "Campos que o SAP precisa enviar para o Stage do Sysphera. A Origem dos dados no SAP pode ser a ZIM01, modelo competência.

 types:  begin of ty_conf.
           include structure zfit0012.
 types:   maktx type makt-maktx,
         end of ty_conf.

 types:


   begin of ty_zim01,
     ano              type gjahr,
     mes              type monat,
     centro_custo     type kostl,
     nome_centro(200),
     cod_conta        type saknr,
     nome_conta(200),
     cod_item         type numc10,
     nome_item(200),
     cod_compra       type numc10,
     id_sysphera      type numc10,
     vlr_local        type p length 16 decimals 2,
     vlr_dolar        type p length 16 decimals 2,
   end of ty_zim01,

   "CS2019000509
   "Precisa criar uma interface do SAP que envia os dados para o Sysphera.
   "Os campos necessários são os da tabela abaixo, este dados devem ser enviados para uma Stage,
   "conforme o segundo print. Este envio deve ser parametrizado no SAP de uma forma no qual o usuario chave
   "possa definir a periodicidade do envio dos dados, informando alguns parametros, como:
   " ano, mes e periodicidade, sendo possivel gerar o JOB para fazer isso de forma automatica a cada periodo.

   " fonte DRE

   begin of ty_dre,
     ano                type gjahr,
     mes                type monat,
     centro_custo       type kostl,
     nome_centro(200),
     cod_conta          type saknr,
     nome_conta(200),
     id_concatenado(30),
     cod_produto        type numc10,
     nome_produto(200),
     vlr_local          type p length 16 decimals 2,
     vlr_dolar          type p length 16 decimals 2,
   end of ty_dre,

   "CS2019000510
   "Precisa criar uma interface do SAP que envia os dados para o Sysphera.
   "Os campos necessários são os da tabela abaixo, este dados devem ser enviados para uma Stage,
   "conforme o segundo print. Este envio deve ser parametrizado no SAP de uma forma no qual o usuario chave
   "possa definir a periodicidade do envio dos dados, informando alguns parametros, como ano, mes, centro de custo.

   "Modelo de dados que o SAP precisa enviar para a Stage. Origem dos dados HCM.

   begin of ty_hcm,
     ano              type gjahr,
     mes              type monat,
     centro_custo     type kostl,
     nome_centro(200),
     cod_conta        type saknr,
     nome_conta(200),
     cod_cargo        type numc10,
     nome_cargo(200),
     vlr_local        type p length 16 decimals 2,
   end of ty_hcm,

   "CS2019000511
   "Precisa criar uma interface do SAP que envia os dados para o Sysphera.
   "Os campos necessários são os da tabela abaixo, este dados devem ser enviados para uma Stage,
   "conforme o segundo print. Este envio deve ser parametrizado no SAP de uma forma no qual o usuario chave
   "possa definir a periodicidade do envio dos dados, informando alguns parametros, como:
   "ano, mes e periodicidade, sendo possivel gerar o JOB para fazer isso de forma automatica a cada periodo.

   "Campos que o SAP precisa gerar e popular na Stage. Origem HCM


   begin of ty_hcm2,
     ano                   type gjahr,
     mes                   type monat,
     centro_custo          type kostl,
     nome_centro(200),
     cod_cargo             type numc10,
     nome_cargo(200),
     cod_chapa             type numc10,
     nome_funcionario(200),
     str_situacao(20),
     qtd_dependentes       type numc10,
     qtd_funcionario       type numc10, "fixo 1
     dt_nascimento(10)     type c,
     sexo(10)              type c,
     saldoatu              type char7, "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
     saldoper              type char7, "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
     anzhl_50              type pranz, "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
     betrg_50              type maxbt, "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
     anzhl_100             type pranz, "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
     betrg_100             type maxbt, "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
     anzhl_dif             type pranz, " BUG - 110727 - CBRAND
     betrg_dif             type maxbt, " BUG - 110727 - CBRAND]
     sld_acum              type pranz, " US - 124701 - CBRAND
     sld_per               type pranz, " US - 124701 - CBRAND
   end of ty_hcm2,

   "CS2019000517
   " O GEO precisa enviar dados para a Stage para que o Sysphera busque as informações.
   begin of ty_geo,
     ano              type gjahr,
     mes              type monat,
     cod_cenario(200),
     centro_custo     type kostl,
     nome_centro(200),
     cod_conta        type saknr,
     nome_conta(200),
     cod_compra       type numc10,
     vlr_local        type p length 16 decimals 2,
     vlr_dolar        type p length 16 decimals 2,
   end of ty_geo,

**************************************************************************************************
   "IMPORTACAO
**************************************************************************************************
   ""CS2019000501
   "No SAP já existe a transação KP06, o que precisa ser feito é a integração dos valores
   "do orçado do Sysphera para esta transação. O Sysphera irá colocar os valores na Stage,
   "e o SAP precisa buscar os dados alimentando a KP06. Este processo deve ser parametrizado de
   "forma que o usuário chave possa fazer esta importação atraves de alguns parametros
   "(ANO, MES, VERSÃO=CENARIO, COD_CENTRO_CUSTO, COD_CONTA), de forma manual.
   begin of ty_kp06,
     ano                 type gjahr,
     mes                 type monat,
     codigo_cenario(200),
     nome_cenario(200),
     centro_custo        type kostl,
     nome_centro(200),
     cod_conta           type saknr,
     nome_conta(200),
     vlr_local           type p length 16 decimals 2,
     vlr_dolar           type p length 16 decimals 2,
     ds_area(4),
   end of ty_kp06,


   "CS2019000508
   "Precisa criar no SAP a rotina para ler os dados da Stage relativos
   "aos investimentos criados no Sysphera. Estes dados devem preencher os campos da transação ZIM02.

   "Os campos que o Sysphera ira popular na Stage são os abaixo.
   begin of ty_zim02,
     numero_linha        type buzei,
     ano                 type gjahr,
     mes                 type monat,
     codigo_cenario(200),
     centro_custo        type kostl,
     nome_centro(200),
     cod_conta           type saknr,
     nome_conta(200),
     cod_item            type numc10,
     nome_item(200),
     cod_grupo           type numc10,
     nome_grupo(200),
     str_objetivo        type string,
     str_descricao       type string,
     str_finalidade      type zfin,
     qtd                 type p length 16 decimals 2,
     vlr_local           type p length 16 decimals 2,
     vlr_dolar           type p length 16 decimals 2,
     id_investimento     type numc10,
     fase                type  zfase,
     cod_natureza        type izwek,
     nome_natureza       type txt50,
     posnr               type zim01_sol_ap_inv-posnr,
   end of ty_zim02,

   begin of ty_ksb1_result,
     bukrs  type bukrs,
     gjahr  type gjahr,
     budat  type budat,
     kostl  type kostl,
     gkont  type hkont,
     aufnr  type aufnr,
     wtgbtr type dmbtr,
     wkgbtr type dmbtr,
     belnr  type bkpf-belnr,
     matnr  type makt-matnr,
     maktx  type makt-maktx,
     lifnr  type lfa1-lifnr,
     name1  type lfa1-name1,
   end of ty_ksb1_result.

 types: begin of ty_fields,
          campo(30) type c,
          group1(5) type c,
          value     type sy-tabix,
          invisible type sy-tabix,
        end of ty_fields,

        begin of ty_usrefus,
          bname     type usrefus-bname,
          useralias type usrefus-useralias,
        end of ty_usrefus.

 types: t_kp06 type standard table of ty_kp06 with empty key.

 types: begin of ty_inves.
          include structure zsys_inves.
 types:   cellcolor type lvc_t_scol.
 types: end   of ty_inves.

* TYPES: BEGIN OF T_KP06 OCCURS 0,
*     ANO                 TYPE GJAHR,
*     MES                 TYPE MONAT,
*     CODIGO_CENARIO TYPE CHAR200 ,
*     NOME_CENARIO TYPE CHAR200,
*     CENTRO_CUSTO        TYPE KOSTL,
*     NOME_CENTRO  TYPE  CHAR200,
*     COD_CONTA           TYPE SAKNR,
*     NOME_CONTA TYPE CHAR200,
*     VLR_LOCAL           TYPE P LENGTH 16 DECIMALS 2,
*     VLR_DOLAR           TYPE P LENGTH 16 DECIMALS 2,
*     END OF T_KP06.
*

*DATA:  T_KP06 TYPE TABLE OF TY_KP06 OCCURS 0.
*     ANO                 TYPE GJAHR,
*     MES                 TYPE MONAT,
*     CODIGO_CENARIO(200),
*     NOME_CENARIO(200),
*     CENTRO_CUSTO        TYPE KOSTL,
*     NOME_CENTRO(200),
*     COD_CONTA           TYPE SAKNR,
*     NOME_CONTA(200),
*     VLR_LOCAL           TYPE P LENGTH 16 DECIMALS 2,
*     VLR_DOLAR           TYPE P LENGTH 16 DECIMALS 2,
*   END OF T_KP06 .

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*

 data: ok-code         like sy-ucomm,
       ok_code         like sy-ucomm,
       vdatai          type sy-datum,
       vdataf          type sy-datum,
       wg_mensagem(30),
       x_field(30),
       wg_acao(30).

** Criação de tabela dinamica
 data: t_fieldcatalog type lvc_t_fcat,
       w_fieldcatalog type lvc_s_fcat,
       wa_layout      type lvc_s_layo,
       wa_stable      type lvc_s_stbl,

       tg_fields      type table of ty_fields   with header line,
       tg_msg_ret     type table of zfiwrs0002  with header line.

 data:
   it_ksb1         type table of zsys_ksb1,
   it_inves        type table of ty_inves, "zsys_inves,
   it_inves_ret    type table of zsys_inves,
   it_zim01        type table of ty_zim01,
   it_dre          type table of ty_dre,
   it_hcm          type table of ty_hcm,
   it_hcm2         type table of ty_hcm2,
   it_geo          type table of ty_geo,
   it_kp06_aux     type table of ty_kp06,
   it_kp06_area    type table of ty_kp06,
   it_kp06_aux_v   type table of ty_kp06,
   it_kp06_area_v  type table of ty_kp06,
   it_kp06         type table of ty_kp06,
   it_zim02        type table of ty_zim02,
*-CS2022000122 - 07.03.2022 - JT - inicio
   it_zgl056       type table of zsys_zgl056,
*-CS2022000122 - 07.03.2022 - JT - fim
   "
   it_zglt_dre_02  type table of zglt_dre_02,
   it_retorno_rest type table of zmm_sysphera_kp06,

   wa_ksb1         type zsys_ksb1,
   wa_inves        type ty_inves, "zsys_inves,
   wa_inves_ret    type zsys_inves,
   wa_ksb1_result  type ty_ksb1_result,
   wa_zim01        type ty_zim01,
   wa_dre          type ty_dre,
   wa_hcm          type ty_hcm,
   wa_hcm2         type ty_hcm2,
   wa_geo          type ty_geo,
   wa_kp06         type ty_kp06,
   wa_zim02        type ty_zim02,
   wa_color        type lvc_s_scol,
   "
   wa_zglt_dre_02  type zglt_dre_02,
*
   it_struct       type ref to cl_abap_structdescr,
   it_fields       type abap_component_tab,
   wa_fields       type abap_componentdescr,
*
   tl_index_rows   type lvc_t_row,
   wl_index_rows   type lvc_s_row.

 data:
   it_ordens  type zttsys_aufnrl,
   wa_ordens  type zsys_aufnr,
   it_conf    type table of zfit0012,
   it_conf2   type table of ty_conf,
   lv_message type string.


*Class definition for ALV toolbar
 class:  lcl_alv_toolbar   definition deferred.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
 data: g_container          type scrfname value 'CC_RESU',
       g_custom_container   type ref to cl_gui_custom_container,
       container_1          type ref to cl_gui_container,       "splitter conteiner 1
       container_2          type ref to cl_gui_container,       "splitter conteiner 2
       splitter             type ref to cl_gui_splitter_container,
       grid1                type ref to cl_gui_alv_grid,
       "
       obg_toolbar          type ref to lcl_alv_toolbar,
       c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,

       obg_docking          type ref to cl_gui_docking_container,

       wa_style             type lvc_s_styl,
       style                type lvc_t_styl   with header line,

       gs_variant_c         type disvariant.

*Declaration for toolbar buttons
 data: ty_toolbar type stb_button.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
 constants:
   c_0               type c value '0',
   c_1               type c value '1',
   c_2               type c value '2',
   c_b               type c value 'B',
   c_s               type c value 'S',
   c_l               type c value 'L',
   c_x               type c value 'X',
   c_d               type c value 'D',
   c_k               type c value 'K',
   c_w               type c value 'W',
   c_f               type c value 'F',
   c_t               type c value 'T',
   c_i               type c value 'I',
   c_n               type c value 'N',
   c_h               type c value 'H',
   c_ag(2)           type c value 'AG',
   c_ne(2)           type c value 'NE',
   c_01(2)           type c value '01',
   c_30(2)           type c value '30',
   c_40(2)           type c value '40',
   c_50(4)           type c value '0050',
   c_76(2)           type c value '76',
   c_71(2)           type c value '71',
   c_72(2)           type c value '72',
   c_br(2)           type c value 'BR',
   c_lf(2)           type c value 'LF',
   c_lr(2)           type c value 'LR',
   c_z1(2)           type c value 'Z1',
   c_add(3)          type c value 'ADD',
   c_del(3)          type c value 'DEL',
   c_dg1(3)          type c value 'DG1',
   c_dg2(3)          type c value 'DG2',
   c_dummy_header(3) type c value '099',
   c_dummy_itens(3)  type c value '098',
   c_exit(4)         type c value 'EXIT',
   c_root(4)         type c value 'ROOT',
   c_minimizar(4)    type c value '@K2@',
   c_maximizar(4)    type c value '@K1@',
   c_back(4)         type c value 'BACK',
   c_save(4)         type c value 'SAVE',
   c_desat(5)        type c value 'DESAT',
   c_dmbtr(5)        type c value 'DMBTR',
   c_modif(5)        type c value 'MODIF',
   c_cancel(6)       type c value 'CANCEL',
   c_deldoc(6)       type c value 'DELDOC',
   c_dclick(6)       type c value 'DCLICK',
   c_search(6)       type c value 'SEARCH',
   c_atuali(6)       type c value 'ATUALI',
   c_add_msg(7)      type c value 'ADD_MSG',
   c_del_msg(7)      type c value 'DEL_MSG',
   c_clos_msg(8)     type c value 'CLOS_MSG',
   c_save_msg(8)     type c value 'SAVE_MSG',
   c_displa(6)       type c value 'DISPLA',
   c_show_msgre(10)  type c value 'SHOW_MSGRE'.


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
 class lcl_event_handler definition.
   public section.
     class-methods:
       on_double_click for event double_click of cl_gui_alv_grid
         importing e_row e_column.

     class-methods:
       on_data_changed for event data_changed of cl_gui_alv_grid
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
*     DATA: WL_DESACTIVE.
*
*     WG_ACAO = C_ADD.  "c_modif.
*
*     TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
*     TY_TOOLBAR-FUNCTION  = C_ADD.
*     TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
*     TY_TOOLBAR-BUTN_TYPE = 0.
*     APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*     CLEAR TY_TOOLBAR.
*
*     TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
*     TY_TOOLBAR-FUNCTION  = C_DEL.
*     TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
*     TY_TOOLBAR-BUTN_TYPE = 0.
*     APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*     CLEAR TY_TOOLBAR.
*
*     TY_TOOLBAR-BUTN_TYPE = 3.
*     APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*     CLEAR TY_TOOLBAR.
*
**   variable for Toolbar Button
*     TY_TOOLBAR-ICON      = ICON_VIEW_CLOSE.
*     TY_TOOLBAR-FUNCTION  = C_CLOS_MSG.
*     TY_TOOLBAR-DISABLED  = SPACE.
*     TY_TOOLBAR-BUTN_TYPE = 0.
*     APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*     CLEAR TY_TOOLBAR.
***   Call reorganize method of toolbar manager to
***   display the toolbar
*     CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*       EXPORTING
*         IO_ALV_TOOLBAR = E_OBJECT.
   endmethod.                    "on_toolbar

   method handle_user_command.

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
* Método de  execução para Duplo-click
   method on_double_click.

   endmethod.                    "ON_DOUBLE_CLICK

   method on_data_changed.

   endmethod.                    "ON_DATA_CHANGED

   method on_data_changed_finished.

   endmethod.                    "on_data_changed_finished

   method on_onf4.

   endmethod.                    "ON_ONF4

 endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

 selection-screen: begin of block a1 with frame title text-003.
   parameters: p_bukrs type bukrs                  modif id t1,
               p_versn type zgl020_dre_dados-versn modif id t1,
               p_ano   type bkpf-gjahr             modif id t1, "OBLIGATOR
               p_mes   type bkpf-monat             modif id t1, " OBLIGATO
               p_kokrs type tka01-kokrs            modif id t1. " OBLIGATO

   select-options:
               s_custo  for bsis-kostl             modif id t1,
               s_posnr  for zsys_inves-im_posnr    modif id t2, " OBLIGATO
               s_perio  for zsys_inves-k1_budat  no-extension  modif id t1,
               s_aufnr   for  aufk-aufnr no intervals.

* SELECT-OPTIONS:
*          p_bukrs FOR  bsis-bukrs             MODIF ID t1.
* PARAMETERS:
*          p_versn TYPE zgl020_dre_dados-versn MODIF ID t1.
* SELECT-OPTIONS:
*           p_ano   FOR bkpf-gjahr             MODIF ID t1, "OBLIGATORY,
*           p_mes  FOR bkpf-monat             MODIF ID t1, " OBLIGATORY,
*           p_kokrs FOR tka01-kokrs            MODIF ID t1, " OBLIGATORY.
*           s_custo  FOR bsis-kostl             MODIF ID t1,
*           s_posnr  FOR zsys_inves-im_posnr    MODIF ID t1, " OBLIGATORY.
*           s_perio  FOR zsys_inves-k1_budat  NO-EXTENSION  MODIF ID t1. " OBLIGATORY.
 selection-screen:end of block a1.

 selection-screen: begin of block b1 with frame title text-001.
   parameters: r_ksb1  radiobutton group rad1 user-command usr1 default 'X',
               r_zim01 radiobutton group rad1,
               r_dre   radiobutton group rad1,
               r_hcm   radiobutton group rad1,
               r_hcm2  radiobutton group rad1,
               r_inves radiobutton group rad1,
*             R_GEO   RADIOBUTTON GROUP RAD1.
               r_kp06  radiobutton group rad1,
               r_zim02 radiobutton group rad1,
*-CS2022000122 - 07.03.2022 - JT - inicio
               r_gl056 radiobutton group rad1,
               r_mapa  radiobutton group rad1.
*-CS2022000122 - 07.03.2022 - JT - fim

 selection-screen:end of block b1.

 at selection-screen output.

   loop at screen.
     if r_inves = abap_false.
       if screen-group1 = 'T2'.
         screen-active    = 0.
         screen-input     = 0.
         screen-invisible = 1.
       endif.
     endif.
     modify screen.
   endloop.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
 start-of-selection.
   data is_valid type c.

   if r_zim01 = 'X'.
     perform f_validar_obrigatorio changing is_valid.

     if is_valid is  initial.
       perform f_zim01.
     else.
       exit.
     endif.

   elseif r_zim02 = 'X'.

     perform f_zim02 tables t_inv.

   elseif r_dre = 'X'.
     perform f_validar_obrigatorio changing is_valid.

     if is_valid is  initial.
       perform f_dre.
     else.
       exit.
     endif.

   elseif r_ksb1 = 'X'.
     perform f_validar_obrigatorio changing is_valid.

     if is_valid is  initial.
       perform f_ksb1.
     else.
       exit.
     endif.

   elseif r_inves = 'X'.
     perform f_validar_obrigatorio changing is_valid.

     if is_valid is  initial.
       perform f_inves.
     else.
       exit.
     endif.

   elseif r_hcm = 'X'.
     perform f_validar_obrigatorio changing is_valid.

     if is_valid is  initial.
       perform f_hcm.
     else.
       exit.
     endif.

   elseif r_hcm2 = 'X'.
     perform f_validar_obrigatorio changing is_valid.

     if is_valid is  initial.
       perform f_hcm2.

     else.
       exit.
     endif.


   elseif r_kp06 = 'X'.
     perform f_kp06.

*-CS2022000122 - 07.03.2022 - JT - inicio
   elseif r_gl056 = abap_true.
     perform f_validar_obrigatorio changing is_valid.

     if is_valid is  initial.
       perform f_zgl056.
     else.
       exit.
     endif.
*-CS2022000122 - 07.03.2022 - JT - fim
   elseif  r_mapa  = abap_true.
     perform f_validar_obrigatorio changing is_valid.

     if is_valid is  initial.
       perform f_mapa.
     else.
       exit.
     endif.
   endif.

   call screen 0100.

 end-of-selection.


*&---------------------------------------------------------------------*
*&  DECLARAÇÕES BAPI KP06
*&---------------------------------------------------------------------*

   data : begin of gt_project occurs 0,
            project_def(24) type c,
          end of gt_project.

   data : lt_messtab type table of bdcmsgcoll,
          ls_messtab type bdcmsgcoll.

   data: ls_headerinfo  like bapiplnhdr,
         lt_indexstruc  type table of bapiacpstru with header line,
         lt_coobject    type table of bapipcpobj with header line,
         lt_totvalue    type table of bapipcptot with header line,
         lt_return      type table of bapiret2 with header line,
         lt_return_auxi type table of bapiret2 with header line,
         ls_return      like line of lt_return,
         ls_pervalue    type table of  bapipcpval with header line.

   data : lv_object type obj_indx,
          lv_value  type val_indx.
   data:
     lt_bapi_return type standard table of bapiret2,
     wa_bapi_return like line of lt_bapi_return.
