*&---------------------------------------------------------------------*
*& Include MZDRE0001TOP                                      PoolMóds.        SAPMZDRE0001
*&
*&---------------------------------------------------------------------*

program  sapmzdre0001 message-id zdre.

type-pools: icon.

tables: zgl020_dre_dados,
        zgl015_dre_est01,
        zgl015_dre_est02,
        zgl015_dre_est03,
        zgl015_dre_est04,
        zgl015_dre_est05,
        zgl015_dre_est06,
        zgl015_dre_est07.

types: begin of tp_conta_razao.
types:  id type tv_nodekey,
        tx type txt50_skat.
        include structure zgl015_dre_est03.
types: end of tp_conta_razao.

types: begin of tp_centro_custo.
types:  id type tv_nodekey,
        tx type ktext.
        include structure zgl015_dre_est04.
types: end of tp_centro_custo.

types: begin of tp_centro_lucro.
types:  id type tv_nodekey,
        tx type ltext.
        include structure zgl015_dre_est05.
types: end of tp_centro_lucro.

types: begin of tp_grupo_material.
types:  id type tv_nodekey,
        tx type wgbez60.
        include structure zgl015_dre_est06.
types: end of tp_grupo_material.

types: begin of tp_nivel_agrupado.
types:  id type tv_nodekey,
        tx type char50.
        include structure zgl015_dre_est07.
types: end of tp_nivel_agrupado.

types: begin of tp_dre_est02.
types:   nv01(02)  type c,
         nv02(02)  type c,
         nv03(02)  type c,
         nv04(02)  type c,
         nv05(02)  type c,
         na01(02)  type c,
         na02(02)  type c,
         na03(02)  type c,
         na04(02)  type c,
         nitxt_ant type char50.
         include structure zgl015_dre_est02.
types: end of tp_dre_est02.

types: begin of tp_dre_dados.
types: icost       type char04,
       icoli       type char04,
       irepo       type char04,
       butxt       type butxt,
       vstxt       type vstxt_011t,
       status_prot type c length 8,
       rowcolor    type char04.
       include structure zgl020_dre_dados.
types: end of tp_dre_dados.

types: begin of ty_zgl015_dre_est01_alv.
types:   butxt  type butxt,
         editar type char04,
         estrut type char04.
         include structure zgl015_dre_est01.
types: end of ty_zgl015_dre_est01_alv.

types: begin of ty_motra_erro.
types: nivel type zgl021_dre_dados-nivel,
       saknr type zgl021_dre_dados-saknr,
       kokrs type zgl015_dre_est05-kokrs,
       prctr type zgl015_dre_est05-prctr,
       kosar type zgl015_dre_est04-kosar,
       matkl type zgl015_dre_est06-matkl.
types: end of ty_motra_erro.

types: begin of ty_excel,
         bukrs     type zgl021_dre_dados-bukrs,
         versn     type zgl021_dre_dados-versn,
         monat     type zgl021_dre_dados-monat,
         gjahr     type zgl021_dre_dados-gjahr,
         nivel     type zgl021_dre_dados-nivel,
         saknr     type zgl021_dre_dados-saknr,
         desc(70),
         qtd_ton   type zgl024_dre_dados-qtd_ton,
         vlr_rea   type zgl021_dre_dados-vlr_rea,
         vlr_dolar type zgl021_dre_dados-vlr_dolar,
         vlr_grupo type zgl021_dre_dados-vlr_grupo,
       end of ty_excel,

*USER STORY 75412  / AOENNING
       begin of ty_fbl3n_zgl060,"Saida  alv 9014
         bukrs           type zgl021_dre_dados-bukrs,
         gjahr           type gjahr,
         monat           type monat,
         saknr           type saknr,
         moedaint_fbl3n  type vlcur12,
         moedafor_fbl3n  type vlcur12,
         moedaind_fbl3n  type vlcur12,
         moedaint_zgl060 type vlcur12,
         moedafor_zgl060 type vlcur12,
         moedaind_zgl060 type vlcur12,
         dif_moedaint    type vlcur12,
         dif_moedafor    type vlcur12,
         dif_moedaind    type vlcur12,
         cell_color      type lvc_t_scol,    " Cor da Célula
       end of ty_fbl3n_zgl060,


       begin of ty_param,
         bukrs type zgl021_dre_dados-bukrs,
         gjahr type gjahr,
         monat type monat,
         saknr type saknr,
       end of  ty_param,



       begin of ty_param_,
         bukrs type zgl021_dre_dados-bukrs,
         gjahr type gjahr,
         monat type monat,
         saknr type saknr,
         kostl type zglt_dre_02-kostl,
         kosar type zglt_dre_02-kosar,
         prctr type  zglt_dre_02-prctr,
         matnr type zglt_dre_02-matnr,
         matkl type zglt_dre_02-matkl,
         aufnr type zglt_dre_02-aufnr,
         vbund type zglt_dre_02-vbund,
       end of  ty_param_,

       begin of ty_param_dre,
         bukrs type zgl021_dre_dados-bukrs,
         gjahr type gjahr,
         monat type monat,
         saknr type saknr,
         kostl type zglt_dre_02-kostl,
         prctr type zglt_dre_02-prctr,
         matkl type zglt_dre_02-matkl,
       end of  ty_param_dre,

       begin of ty_param_cc,
         bukrs type zgl021_dre_dados-bukrs,
         gjahr type gjahr,
         monat type monat,
         saknr type saknr,
         kostl type zglt_dre_02-kostl,
         vlhsl type vlcur12,
         vlksl type vgcur12,
         vlosl type vocur12,
       end of  ty_param_cc,

*USER STORY 75413  / AOENNING
       begin of ty_zgl060_tabderivada,"Saida alv 9013
         bukrs        type zgl021_dre_dados-bukrs,
         gjahr        type gjahr,
         monat        type monat,
         saknr        type saknr,
         kostl        type zglt_dre_02-kostl,
         kosar        type zglt_dre_02-kosar,
         prctr        type  zglt_dre_02-prctr,
         matnr        type zglt_dre_02-matnr,
         matkl        type zglt_dre_02-matkl,
         aufnr        type zglt_dre_02-aufnr,
         vbund        type zglt_dre_02-vbund,
         moedaint_zgl type vlcur12,
         moedafor_zgl type vlcur12,
         moedaind_zgl type vlcur12,
         moedaint_der type vlcur12,
         moedafor_der type vlcur12,
         moedaind_der type vlcur12,
         dif_moedaint type vlcur12,
         dif_moedafor type vlcur12,
         dif_moedaind type vlcur12,
         vlhsl        type vlcur12,
         vlksl        type vlcur12,
         vlosl        type vlcur12,
         cell_color   type lvc_t_scol,    " Cor da Célula
       end of ty_zgl060_tabderivada,



*USER STORY 102744  / AOENNING
       begin of ty_tabderivada_dre,"Saida alv 9015
         bukrs         type zgl021_dre_dados-bukrs,
         gjahr         type gjahr,
         monat         type monat,
         saknr         type saknr,
         kostl         type zglt_dre_02-kostl,
         prctr         type  zglt_dre_02-prctr,
         matkl         type zglt_dre_02-matkl,
         moedaint_tbdr type vlcur12,
         moedafor_tbdr type vlcur12,
         moedaind_tbdr type vlcur12,
         qtde_tbdr     type quan1_12,
         moedaint_cust type vlcur12,
         moedafor_cust type vlcur12,
         moedaind_cust type vlcur12,
         moedaint_res  type vlcur12,
         moedafor_res  type vlcur12,
         moedaind_res  type vlcur12,
         qtde_res      type quan1_12,
         moedaint_dre  type vlcur12,
         moedafor_dre  type vlcur12,
         moedaind_dre  type vlcur12,
         qtde_dre      type quan1_12,
         nivel_dre     type znivel_dre,
         dif_moedaint  type vlcur12,
         dif_moedafor  type vlcur12,
         dif_moedaind  type vlcur12,
         cell_color    type lvc_t_scol,    " Cor da Célula
       end of ty_tabderivada_dre,

       begin of ty_dre,
         bukrs type   bukrs,
         gjahr type   gjahr,
         poper type   poper,
         saknr type   saknr,
         kostl type   kostl,
         prctr type   prctr,
         matkl type   matkl,
         vlhsl type   vlcur12,
         vlksl type   vgcur12,
         vlosl type   vocur12,
         nivel type   znivel_dre,
         qtmsl type   quan1_12,
       end of ty_dre,

       begin of ty_file,
         field(1000),
       end of ty_file.


data: it_zglt_dre_04            type table of zglt_dre_04 with header line,
      it_zglt_dre_02            type table of zglt_dre_02 with header line,
      it_zglt_dre_02_aux        type table of zglt_dre_02 with header line,
      it_fbl3n_zgl060           type table of ty_fbl3n_zgl060,
      wa_fbl3n_zgl060           type ty_fbl3n_zgl060,
      it_zglt061                type table of zglt061,
      it_zgl015_dre_est08       type table of zgl015_dre_est08,
      wa_zgl015_dre_est08       type zgl015_dre_est08,
      it_zgl015_dre_est04       type table of zgl015_dre_est04 with header line,
      it_param                  type table of ty_param,
      it_param_                 type table of ty_param_,
      it_param_dre              type table of ty_param_dre,
      it_zgl060_tabderivada     type table of ty_zgl060_tabderivada,
      it_zgl060_tabderivada_aux type table of ty_zgl060_tabderivada,
      wa_zgl060_tabderivada     type ty_zgl060_tabderivada,
      it_tabderivada_dre        type table of ty_tabderivada_dre,
      wa_tabderivada_dre        type  ty_tabderivada_dre,
      ws_x001                   type x001,
      ws_moedaint               type t001-waers,
      ws_moedafor               type t001-waers,
      ws_moedaind               type t001-waers,
      it_saldo                  type table of zde_fi_gl_saldo_faglflext with header line,
      it_saldo2                 type table of zde_fi_gl_saldo_faglflext with header line,
      it_saldo3                 type table of zde_fi_gl_saldo_faglflext with header line,
      it_contas                 type zct_emp_contas,
      it_0300                   type table of zde_dre_dif_alv with header line,
      it_coas                   type table of coas with header line,
      it_dre_02_cc              type table of zglt_dre_02 with header line,
      it_dre_02_cl              type table of zglt_dre_02 with header line,
      it_dre_02_mat             type table of zglt_dre_02 with header line,
      it_zgl015_dre_cvend       type table of zgl015_dre_cvend with header line,
      it_dre_02_mat_aux         type table of zglt_dre_02 with header line,
      it_dre_02_raz             type table of zglt_dre_02 with header line,
      it_dre_02_avu             type table of zglt_dre_02 with header line,
      it_dre_02_tbdr            type table of zglt_dre_02 with header line,
      it_dre_02_tbdr_aux        type table of zglt_dre_02 with header line,
      it_dre_02_cust            type table of zglt_dre_02 with header line,
      it_dre_02_cust_aux        type table of zglt_dre_02 with header line,

      it_dre                    type table of ty_dre with header line,
      it_total_cust_cc          type table of zglt_dre_02 with header line,
      it_total_cust_cc_aux      type table of zglt_dre_02 with header line,
      it_dre_02_cust_cc_1       type table of zglt_dre_02 with header line,
      it_dre_02_cust_cc_2       type table of zglt_dre_02 with header line,
      it_dre_02_cust_cc_3       type table of zglt_dre_02 with header line,
      it_dre_02_cust_avu        type table of zglt_dre_02 with header line,


      it_zgl021_dre_dados       type table of zgl021_dre_dados with header line,
      it_zgl022_dre_dados       type table of zgl022_dre_dados with header line,
      it_zgl023_dre_dados       type table of zgl023_dre_dados with header line,
      it_zgl024_dre_dados       type table of zgl024_dre_dados with header line.

data: gob_gui_alv_9014 type ref to cl_gui_alv_grid,
      gob_gui_alv_9013 type ref to cl_gui_alv_grid,
      ls_stable_9013   type lvc_s_stbl,
      ls_stable_9014   type lvc_s_stbl,
      ls_stable_9015   type lvc_s_stbl,
      gob_gui_alv_9015 type ref to cl_gui_alv_grid,
      wa_color         type          lvc_s_scol,  " Cor para célula
      it_color         type table of lvc_s_scol,  " Cor para célula
      git_fcat_        type lvc_t_fcat,
      git_fcat_9015    type lvc_t_fcat,
      git_fcat         type lvc_t_fcat.

data: rg_monat type range of monat.


" Classe
class lcl_event_receiver definition deferred.
data:  event_receiver   type ref to lcl_event_receiver.


class lcl_event_receiver definition.
  public section.
    methods:

      hotspot_click
        for event hotspot_click of cl_gui_alv_grid
        importing e_row_id e_column_id es_row_no .


    class-methods:
      get_ucomm for event user_command of cl_gui_alv_grid
        importing e_ucomm.

endclass.

class lcl_event_receiver implementation.
  method hotspot_click.

  endmethod.

  method get_ucomm.

  endmethod.
endclass.


*----------------------------------------------------------------------*
* Selections Screen
*----------------------------------------------------------------------*
selection-screen begin of screen 1101 as subscreen nesting level 4.
  select-options: t_empres for zgl020_dre_dados-bukrs   no-extension no intervals,
                  t_estrut for zgl020_dre_dados-versn   no-extension no intervals,
                  t_mes    for zgl020_dre_dados-monat no-extension no intervals,
                  t_ano    for zgl020_dre_dados-gjahr no-extension no intervals,
                  t_moeda  for zgl020_dre_dados-waers no-extension no intervals.


selection-screen end of screen 1101.



constants: ok_csta      type sy-ucomm   value 'CSTA',
           ok_cpro      type sy-ucomm   value 'CPRO',
           ok_ccancpro  type sy-ucomm   value 'CCANCPRO',
           ok_cexcel    type sy-ucomm   value 'CEXCEL',
***     Inicio - ALX
           c_fbxgl      type sy-ucomm   value 'CFBXZG',
***     Fim - ALX
           c_zgl_tbder  type sy-ucomm   value 'ZGL_TABDER',
           c_tbdr_dre   type sy-ucomm   value 'TBDR_DRE',
           ok_filtro    type sy-ucomm   value 'TAB_FILTRO',
           ok_result    type sy-ucomm   value 'TAB_RESULT',
           ok_save      type sy-ucomm   value 'SAVE',
           ok_liberar   type sy-ucomm   value 'LIBERAR',
           ok_back      type sy-ucomm   value 'BACK',
           ok_exit      type sy-ucomm   value 'EXIT',
           ok_cancel    type sy-ucomm   value 'CANCEL',
           ok_atualiza  type sy-ucomm   value 'ATUALIZA',
           ok_nova_est  type sy-ucomm   value 'NOVA_EST',
           ok_copi_est  type sy-ucomm   value 'COPI_EST',
           ok_dele_est  type sy-ucomm   value 'DELE_EST',
           ok_cta_razao type sy-ucomm   value 'CTA_RAZAO',
           ok_cta_custo type sy-ucomm   value 'CTA_CUSTO',
           ok_cta_lucro type sy-ucomm   value 'CTA_LUCRO',
           ok_grp_merca type sy-ucomm   value 'GRP_MERCA',
           ok_agp_nivel type sy-ucomm   value 'AGP_NIVEL',
           ok_erro_cl   type sy-ucomm   value 'ERRO_CL',
           ok_erro_cc   type sy-ucomm   value 'ERRO_CC',
           ok_erro_gm   type sy-ucomm   value 'ERRO_GM',
           ok_erro_all  type sy-ucomm   value 'ERRO_ALL',
           tl_0002      type sydynnr    value '0002',
           tl_1001      type sydynnr    value '1001',
           tl_1002      type sydynnr    value '1002',
           tl_1101      type sydynnr    value '1101',
           tl_1102      type sydynnr    value '1102',
           tl_9001      type sydynnr    value '9001',
           tl_9003      type sydynnr    value '9003',
           c_x          type c length 1 value 'X',
           c_j          type c length 1 value 'J',
           c_icost      type c length 5 value 'ICOST',
           c_icoli      type c length 5 value 'ICOLI',
           c_irepo      type c length 5 value 'IREPO',
           c_editar     type c length 6 value 'EDITAR',
           c_insert     type c length 6 value 'INSERT',
           c_estrut     type c length 6 value 'ESTRUT',
           c_excluir    type c length 7 value 'EXCLUIR',
           c_conf       type c length 4 value 'CONF'.

controls: tab_strip  type tabstrip,
          tab_filtro type tableview using screen 0001,
          tab_result type tableview using screen 0001.

data: ok_code_0001      type sy-ucomm,
      ok_code_1103      type sy-ucomm,
      ok_code_1105      type sy-ucomm,
      ok_code_9000      type sy-ucomm,
      ok_code_9002      type sy-ucomm,
      ok_code_9004      type sy-ucomm,
      ok_code_9005      type sy-ucomm,
      ok_code_9006      type sy-ucomm,
      ok_code_9007      type sy-ucomm,
      ok_code_9008      type sy-ucomm,
      ok_code_9009      type sy-ucomm,
      ok_code_9010      type sy-ucomm,
      tl_0001           type sydynnr,
      tl_9000           type sydynnr,
      wa_fcode          type sy-ucomm,
      it_fcode          like table of wa_fcode,
      qt_char_nivel     type i,
      vg_empresa_dest   type butxt,
      vg_estrutura_dest type vstxt_011t,
      vg_plano_conta    type ktplt,
      vg_cc_grupo       type ktext,
      vg_centro_lucro   type ltext,
      vg_conta_razao    type txt50_skat,
      vg_grupo_material type wgbez60,
      vg_gerar_rel      type char01.

data: ls_saknr_2       type zgl015_dre_est03-saknr,
      ls_saknr_3       type zgl015_dre_est03-saknr,
      ls_saknr_4       type zgl015_dre_est03-saknr,
      ls_saknr_5       type zgl015_dre_est03-saknr,
      vg_conta_razao_2 type txt50_skat,
      vg_conta_razao_3 type txt50_skat,
      vg_conta_razao_4 type txt50_skat,
      vg_conta_razao_5 type txt50_skat,
      vg_levar_qtde_2  type zgl015_dre_est03-levar_qtde,
      vg_levar_qtde_3  type zgl015_dre_est03-levar_qtde,
      vg_levar_qtde_4  type zgl015_dre_est03-levar_qtde,
      vg_levar_qtde_5  type zgl015_dre_est03-levar_qtde.

data: ls_kosar_2    type zgl015_dre_est04-kosar,
      ls_kosar_3    type zgl015_dre_est04-kosar,
      ls_kosar_4    type zgl015_dre_est04-kosar,
      ls_kosar_5    type zgl015_dre_est04-kosar,
      vg_cc_grupo_2 type ktext,
      vg_cc_grupo_3 type ktext,
      vg_cc_grupo_4 type ktext,
      vg_cc_grupo_5 type ktext.

data: ls_prctr_2        type zgl015_dre_est05-prctr,
      ls_prctr_3        type zgl015_dre_est05-prctr,
      ls_prctr_4        type zgl015_dre_est05-prctr,
      ls_prctr_5        type zgl015_dre_est05-prctr,
      ls_kokrs_1        type zgl015_dre_est05-kokrs,
      ls_kokrs_2        type zgl015_dre_est05-kokrs,
      ls_kokrs_3        type zgl015_dre_est05-kokrs,
      ls_kokrs_4        type zgl015_dre_est05-kokrs,
      ls_kokrs_5        type zgl015_dre_est05-kokrs,
      vg_centro_lucro_2 type ltext,
      vg_centro_lucro_3 type ltext,
      vg_centro_lucro_4 type ltext,
      vg_centro_lucro_5 type ltext.

data: ls_matkl_2          type zgl015_dre_est06-matkl,
      ls_matkl_3          type zgl015_dre_est06-matkl,
      ls_matkl_4          type zgl015_dre_est06-matkl,
      ls_matkl_5          type zgl015_dre_est06-matkl,
      vg_grupo_material_2 type wgbez60,
      vg_grupo_material_3 type wgbez60,
      vg_grupo_material_4 type wgbez60,
      vg_grupo_material_5 type wgbez60,
      wg_file_local       type rlgrap-filename,
      it_excel            type table of ty_excel with header line,
      it_file             type table of ty_file with header line.


*---------- Definition -----------------------------------------------*
class lcl_dre_event_dre definition.
  public section.
    methods handle_hotspot_dre
      for event hotspot_click of cl_gui_alv_grid
      importing e_column_id
                es_row_no.


endclass.                    "lcl_event_handler DEFINITION

*---------- Definition -----------------------------------------------*
class lcl_dre_event_dre_est definition.
  public section.
    methods handle_hotspot_dre_est
      for event hotspot_click of cl_gui_alv_grid
      importing e_column_id
                es_row_no.

    class-methods:
      get_ucomm for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.                    "lcl_event_est_handler DEFINITION

data: it_dre_dados            type table of zgl020_dre_dados with header line,
      it_dre_dados_alv        type table of tp_dre_dados with header line,
      wa_dre_dados_alv        type tp_dre_dados,
      wa_scroll_col           type lvc_s_col,
      wa_scroll_row           type lvc_s_roid,
      prim_dre                type char01,
      prim_dre_est            type char01,
      prim_dre_nivel          type char01,
      prim_dre_nivel_re       type char01,
      dre_container           type ref to cl_gui_custom_container,
      dre_alv                 type ref to cl_gui_alv_grid,
      dre_container_est       type ref to cl_gui_custom_container,
      dre_alv_est             type ref to cl_gui_alv_grid,
      dre_gs_layout           type lvc_s_layo,
      dre_catalogo            type lvc_t_fcat,
      dre_sort                type lvc_t_sort,
      dre_except_qinfo        type lvc_t_qinf,
      dre_catalogo_est        type lvc_t_fcat,
      dre_event_dre           type ref to lcl_dre_event_dre,
      dre_event_dre_est       type ref to lcl_dre_event_dre_est,
      it_zgl015_dre_est01     type table of zgl015_dre_est01 with header line,
      wa_zgl015_dre_est01     type zgl015_dre_est01,
      wa_zgl015_dre_est02     type zgl015_dre_est02,
      it_zgl015_dre_est02     type table of zgl015_dre_est02 with header line,
      it_zgl015_dre_est02_aux type table of zgl015_dre_est02 with header line,
      it_zgl015_dre_est03     type table of zgl015_dre_est03 with header line,
      it_zgl015_dre_est03_aux type table of zgl015_dre_est03 with header line,
      it_zgl015_dre_est03_id  type table of tp_conta_razao   with header line,
      wa_zgl015_dre_est03_id  type tp_conta_razao,
      it_zgl015_dre_est04_id  type table of tp_centro_custo  with header line,
      wa_zgl015_dre_est04_id  type tp_centro_custo,
      it_zgl015_dre_est05     type table of zgl015_dre_est05 with header line,
      it_zgl015_dre_est05_id  type table of tp_centro_lucro  with header line,
      ws_zgl015_dre_est08     type zgl015_dre_est08,
      wa_zgl015_dre_est05_id  type tp_centro_lucro,
      it_motra_erro           type table of ty_motra_erro with header line,

      it_zgl015_dre_est06     type table of zgl015_dre_est06 with header line,
      it_zgl015_dre_est06_id  type table of tp_grupo_material  with header line,
      wa_zgl015_dre_est06_id  type tp_grupo_material,

      it_zgl015_dre_est07     type table of zgl015_dre_est07 with header line,
      it_zgl015_dre_est07_id  type table of tp_nivel_agrupado with header line,
      wa_zgl015_dre_est07_id  type tp_nivel_agrupado,

      it_zgl015_dre_est01_alv type table of ty_zgl015_dre_est01_alv with header line,
      wa_zgl015_dre_est01_alv type ty_zgl015_dre_est01_alv,
      vg_editar               type c length 1,
      qt_niveis               type i,
      vg_bukrs_txt            type butxt,
      wa_dre_est02            type tp_dre_est02.


*DATA: it_dre_02_cc TYPE TABLE OF ZGLT_DRE_02 WITH HEADER LINE.

*&** Níveis de Estrutura **********************************************
*&*********************************************************************

class        lcl_application definition.
  public section.
    methods:
      handle_node_double_click
        for event node_double_click
        of cl_gui_list_tree
        importing node_key,
      handle_expand_no_children
        for event expand_no_children
        of cl_gui_list_tree
        importing node_key,
      handle_item_double_click
        for event item_double_click
        of cl_gui_list_tree
        importing node_key item_name,
      handle_button_click
        for event button_click
        of cl_gui_list_tree
        importing node_key item_name,
      handle_link_click
        for event link_click
        of cl_gui_list_tree
        importing node_key item_name,
      handle_checkbox_change
        for event checkbox_change
        of cl_gui_list_tree
        importing node_key item_name checked.
endclass.                    "LCL_APPLICATION DEFINITION

types: item_table_type like standard table of mtreeitm with default key.

data: g_application      type ref to lcl_application,
      g_custom_container type ref to cl_gui_custom_container,
      g_tree             type ref to cl_gui_list_tree,
      g_event(30),
      g_node_key         type tv_nodekey,
      g_item_name        type tv_itmname.

*&*********************************************************************



*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
form z_estrutura_fieldcat tables it_catalogo type lvc_t_fcat
                           using p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit
                                 p_do_sum
                                 p_icon
                                 p_just
                                 p_emphasize
                                 p_edit.

  data: wa_catalog type lvc_s_fcat.
  wa_catalog-tabname     = p_tab_name.
  wa_catalog-fieldname   = p_fieldname.
  wa_catalog-scrtext_l   = p_texto_grande.
  wa_catalog-scrtext_m   = p_texto_grande.
  wa_catalog-scrtext_s   = p_texto_grande.
  wa_catalog-hotspot     = p_hot.
  wa_catalog-col_pos     = p_posicao.
  wa_catalog-outputlen   = p_outputlen.
  wa_catalog-fix_column  = p_fix_column.
  wa_catalog-convexit    = p_convexit.
  wa_catalog-do_sum      = p_do_sum.
  wa_catalog-icon        = p_icon.
  wa_catalog-just        = p_just.
  wa_catalog-emphasize   = p_emphasize.
  wa_catalog-edit        = p_edit.
  append wa_catalog to it_catalogo.

endform.                    " Z_ESTRUTURA_FIELDCAT

*---------- Implementation -------------------------------------------*
class lcl_dre_event_dre implementation.
  method handle_hotspot_dre.
    read table it_dre_dados_alv into wa_dre_dados_alv index es_row_no-row_id.
    case e_column_id-fieldname.
      when c_icost.
        perform seleciona_dre using wa_dre_dados_alv.
        leave to screen 1.
      when c_icoli.
        if wa_dre_dados_alv-liberado eq 'X'.

          if wa_dre_dados_alv-status gt 0.
            call function 'ZENQUEUE_PROC_DRE'
              exceptions
                foreign_lock   = 1
                system_failure = 2
                others         = 3.
            if sy-subrc is  initial.
              call function 'ZDEQUEUE_PROC_DRE'.
              perform liberar_dre using wa_dre_dados_alv.
              leave to screen 1.

            else.
              message s005.

            endif.
          else.
            message s005.

          endif.
        else.
          perform liberar_dre using wa_dre_dados_alv.
          leave to screen 1.
        endif.
      when c_irepo.
        if ( wa_dre_dados_alv-status eq '3' ) or ( wa_dre_dados_alv-status eq '2' ).
          perform liberar_dre using wa_dre_dados_alv.
          leave to screen 1.
        endif.
    endcase.
  endmethod.                    "handle_hotspot_click
endclass.                    "lcl_event_handler IMPLEMENTATION

*---------- Implementation -------------------------------------------*
class lcl_dre_event_dre_est implementation.
  method handle_hotspot_dre_est.
    read table it_zgl015_dre_est01_alv into wa_zgl015_dre_est01_alv index es_row_no-row_id.
    case e_column_id-fieldname.
      when c_editar.
        vg_editar = 'X'.
        clear: zgl015_dre_est01.
        move-corresponding wa_zgl015_dre_est01_alv to zgl015_dre_est01.
        call screen 9002 starting at 10 10.
      when c_estrut.
        tl_9000 = tl_9003.
        clear: wa_zgl015_dre_est01.
        move-corresponding wa_zgl015_dre_est01_alv to wa_zgl015_dre_est01.
        if not prim_dre_nivel is initial.
          call method g_tree->delete_all_nodes.
          prim_dre_nivel_re = c_x.
        endif.
        leave to screen 9000.
    endcase.
  endmethod.                    "handle_hotspot_click


  method get_ucomm.
    data: it_selected_rows type lvc_t_row,
          wa_selected_rows type lvc_s_row,
          lines            type sy-tabix.

    call method dre_alv->get_selected_rows
      importing
        et_index_rows = it_selected_rows.

    describe table it_selected_rows lines lines.

    if ( lines is initial ).
      message text-e01 type 'I' display like 'E'.

    else.

      read table it_selected_rows into wa_selected_rows index 1.
      read table it_dre_dados_alv into data(wa_dre_dados_alv) index wa_selected_rows-index.

      "Seleção valores transação ZGL060.
      select * from zglt_dre_04  into table it_zglt_dre_04 where bukrs eq t_empres-low and gjahr eq t_ano-low and poper eq t_mes-low.
      if sy-subrc eq 0.

      endif.

      "Seleção valores Tabela Derivada.
      select * from zglt_dre_02  into table it_zglt_dre_02 where bukrs eq t_empres-low and gjahr eq t_ano-low and poper eq t_mes-low.
      if sy-subrc eq 0.

      endif.
    endif.
  endmethod.                    "GET_UCOMM
endclass.                    "lcl_event_handler IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_application implementation.

  method  handle_node_double_click.
    " this method handles the node double click event of the tree
    " control instance

    " show the key of the double clicked node in a dynpro field
    g_event = 'NODE_DOUBLE_CLICK'.
    g_node_key = node_key.
  endmethod.                    "HANDLE_NODE_DOUBLE_CLICK

  method  handle_item_double_click.
    " this method handles the item double click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the double clicked item in a dynpro field
    g_event = 'ITEM_DOUBLE_CLICK'.
    g_node_key = node_key.
    g_item_name = item_name.
  endmethod.                    "HANDLE_ITEM_DOUBLE_CLICK

  method  handle_link_click.
    " this method handles the link click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked link in a dynpro field
    g_event = 'LINK_CLICK'.
    g_node_key = node_key.
    g_item_name = item_name.
  endmethod.                    "HANDLE_LINK_CLICK

  method  handle_button_click.
    " this method handles the button click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked button in a dynpro field
    g_event = 'BUTTON_CLICK'.
    g_node_key = node_key.
    g_item_name = item_name.
  endmethod.                    "HANDLE_BUTTON_CLICK

  method  handle_checkbox_change.
    " this method handles the checkbox_change event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked checkbox in a dynpro field
    g_event = 'CHECKBOX_CHANGE'.
    g_node_key = node_key.
    g_item_name = item_name.
  endmethod.                    "HANDLE_CHECKBOX_CHANGE


  method handle_expand_no_children.
    data: node_table type treev_ntab,
          node       type treev_node,
          item_table type item_table_type,
          item       type mtreeitm.

* show the key of the expanded node in a dynpro field
    g_event = 'EXPAND_NO_CHILDREN'.
    g_node_key = node_key.

*    if node_key = c_nodekey-child2.
** add the children for node with key 'Child2'
** Node with key 'New3'
*      clear node.
*      node-node_key = c_nodekey-new3.
*      node-relatkey = c_nodekey-child2.
*      node-relatship = cl_gui_list_tree=>relat_last_child.
*      append node to node_table.
*
** Node with key 'New4'
*      clear node.
*      node-node_key = c_nodekey-new4.
*      node-relatkey = c_nodekey-child2.
*      node-relatship = cl_gui_list_tree=>relat_last_child.
*      append node to node_table.
*
** Items of node with key 'New3'
*      clear item.
*      item-node_key = c_nodekey-new3.
*      item-item_name = '1'.
*      item-class = cl_gui_list_tree=>item_class_text.
*      item-length = 11.
*      item-usebgcolor = 'X'. "
*      item-text = 'SAPTROX1'.
*      append item to item_table.
*
*      clear item.
*      item-node_key = c_nodekey-new3.
*      item-item_name = '2'.
*      item-class = cl_gui_list_tree=>item_class_text.
*      item-alignment = cl_gui_list_tree=>align_auto.
*      item-font = cl_gui_list_tree=>item_font_prop.
*      item-text = 'Kommentar zu SAPTROX1'(001).
*      append item to item_table.
*
** Items of node with key 'New4'
*      clear item.
*      item-node_key = c_nodekey-new4.
*      item-item_name = '1'.
*      item-class = cl_gui_list_tree=>item_class_text.
*      item-length = 11.
*      item-usebgcolor = 'X'. "
*      item-text = 'SAPTRIXTROX'.
*      append item to item_table.
*
*      clear item.
*      item-node_key = c_nodekey-new4.
*      item-item_name = '2'.
*      item-class = cl_gui_list_tree=>item_class_text.
*      item-alignment = cl_gui_list_tree=>align_auto.
*      item-font = cl_gui_list_tree=>item_font_prop.
*      item-text = 'Kommentar zu SAPTRIXTROX'(002).
*      append item to item_table.
*    endif.

    call method g_tree->add_nodes_and_items
      exporting
        node_table                     = node_table
        item_table                     = item_table
        item_table_structure_name      = 'MTREEITM'
      exceptions
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6.
    if sy-subrc <> 0.
      message a000.
    endif.
  endmethod.                    "HANDLE_EXPAND_NO_CHILDREN

endclass.                    "LCL_APPLICATION IMPLEMENTATION
