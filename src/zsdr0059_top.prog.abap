*&---------------------------------------------------------------------*
*&  Include           ZSDR0059_TOP
*&---------------------------------------------------------------------*

report zsdr0059.

tables: zib_nfe_forn, zib_nfe_dist_ter, zsdt0127, lfa1, zfit0230.

types: begin of ty_saida_0100,
         bukrs           type zib_nfe_dist_ter-bukrs,
         branch          type zib_nfe_dist_ter-branch,
         numero          type zib_nfe_dist_ter-numero,
         serie           type zib_nfe_dist_ter-serie,
         dt_emissao      type zib_nfe_dist_ter-dt_emissao,
         lifnr           type lfa1-lifnr,
         forne_cnpj      type stcd1,
         forne_ie        type stcd3,
         stcd2           type stcd2, "USER STORY 163035 - MMSILVA - 08.01.2025
         dest_cnpj       type stcd1,
         dest_ie         type stcd3,
         name_forn       type lfa1-name1,
         model           type zib_nfe_dist_ter-model,
         cancel          type zib_nfe_dist_ter-cancel,
         vl_total        type zib_nfe_dist_ter-vl_total,
         dt_vencimento   type zib_nfe_dist_ter-dt_vencimento,
         zterm           type zib_nfe_dist_ter-ctr_zterm,
         text1           type t052u-text1,
         vl_total_fatura type zib_nfe_dist_ter-vl_total_fatura,
         taxval          type j_1bnfstx-taxval, "PBI - 72546 - CBRAND
         taxval_ipi      type j_1bnfstx-taxval, "PBI - 72546 - CBRAND
         migo            type ekbe-ebeln,
         ebeln           type ekbe-ebeln,
         dt_migo         type ekbe-budat,
         gj_migo         type ekbe-gjahr,
         miro            type ekbe-ebeln,
         dt_miro         type ekbe-budat,
         gj_miro         type ekbe-gjahr,
         docnum          type j_1bnfdoc-docnum,
         pstdat          type j_1bnfdoc-pstdat,
         cd_operacao     type zsdt0127-cd_operacao,
         ds_operacao     type char80,
         chave           type zib_nfe_dist_ter-chave_nfe,
         line_color(4)   type c, "Used to store row color attributes
         cfop            type c length 200,
         docnum_ref      type zib_nfe_forn-docnum_ref,
         rate            type j_1bnfstx-rate,
         rate_ipi        type j_1bnfstx-rate,
         stblg           type rbkp-stblg,         "*-CS2022000243-#76365-26.04.2022-JT-inicio
         candat          type j_1bnfdoc-candat,   "*-CS2022000243-#76365-26.04.2022-JT-inicio
         tpnf            type char20,
         finnfe          type char20,
         tpcte           type char20,
         tpserv          type char20,
         modal           type char20,
         pedido          type ekbe-ebeln,
         chave_nfe_ref   type string,
       end of ty_saida_0100,

       begin of ty_saida_0105,
         ds_operacao type char80.
         include structure zsdt0127.
types  end of ty_saida_0105.

types: begin of ty_zib_nfe_dist_ter,
         chave_nfe       type zib_nfe_dist_ter-chave_nfe,
         bukrs           type zib_nfe_dist_ter-bukrs,
         branch          type zib_nfe_dist_ter-branch,
         numero          type zib_nfe_dist_ter-numero,
         serie           type zib_nfe_dist_ter-serie,
         dt_emissao      type zib_nfe_dist_ter-dt_emissao,
         forne_cnpj      type zib_nfe_dist_ter-forne_cnpj,
         forne_ie        type zib_nfe_dist_ter-forne_ie,
         destino_cnpj    type zib_nfe_dist_ter-destino_cnpj,
         destino_ie      type zib_nfe_dist_ter-destino_ie,
         model           type zib_nfe_dist_ter-model,
         cancel          type zib_nfe_dist_ter-cancel,
         vl_total        type zib_nfe_dist_ter-vl_total,
         vl_icms_total   type zib_nfe_dist_ter-vl_icms_total,
         vl_ipi_total    type zib_nfe_dist_ter-vl_ipi_total,
         stcd1           type lfa1-stcd1,
         numero_aux      type zib_nfe_dist_ter-numero,
         serie_aux       type zib_nfe_dist_ter-serie,
         ref_doc_no      type ekbe-xblnr,
         lifnr           type lfa1-lifnr,
         dt_vencimento   type zib_nfe_dist_ter-dt_vencimento,
         ctr_zterm       type zib_nfe_dist_ter-ctr_zterm,
         vl_total_fatura type zib_nfe_dist_ter-vl_total_fatura,
         cd_tipo_doc     type zib_nfe_dist_ter-cd_tipo_doc,
         cd_fina_emissao type zib_nfe_dist_ter-cd_fina_emissao,
       end of ty_zib_nfe_dist_ter,

       begin of ty_zib_nfe_dist_itm,
         chave_nfe        type zib_nfe_dist_ter-chave_nfe,
         prod_cfop        type zib_nfe_dist_itm-prod_cfop,
         icms_aqt         type zib_nfe_dist_itm-icms_aqt,
         prod_pedido_comp type zde_ped_compra_xml,
         ipi_aqt          type zib_nfe_dist_itm-ipi_aqt,
       end of ty_zib_nfe_dist_itm,

       begin of ty_zib_cte_dist_ter,
         cd_chave_cte    type zib_cte_dist_ter-cd_chave_cte,
         "E_EMISSOR         TYPE ZIB_CTE_DIST_TER-E_EMISSOR,
         "F_EMISSOR         TYPE ZIB_CTE_DIST_TER-F_EMISSOR,
         f_tomadora      type zib_cte_dist_ter-f_tomadora,
         e_tomadora      type zib_cte_dist_ter-e_tomadora,
         numr_cte        type zib_cte_dist_ter-numr_cte,
         numr_serie      type zib_cte_dist_ter-numr_serie,
         dt_emissao      type zib_cte_dist_ter-dt_emissao,
         emit_cnpj       type zib_cte_dist_ter-emit_cnpj,
         emit_ie         type zib_cte_dist_ter-emit_ie,
         modelo          type zib_cte_dist_ter-modelo,
         cancel          type zib_cte_dist_ter-cancel,
         valor_prestacao type zib_cte_dist_ter-valor_prestacao,
         valor_icms      type zib_cte_dist_ter-valor_icms,
         p_emissor       type zib_cte_dist_ter-p_emissor,
         stcd1           type lfa1-stcd1,
         numr_cte_aux    type zib_cte_dist_ter-numr_cte,
         numr_serie_aux  type zib_cte_dist_ter-numr_serie,
         ref_doc_no      type ekbe-xblnr,
         lifnr           type lfa1-lifnr,
         codg_cfop       type zib_cte_dist_ter-codg_cfop,
         cd_tipo_cte     type zib_cte_dist_ter-cd_tipo_cte,
         cd_modal        type zib_cte_dist_ter-cd_modal,
         cd_tipo_servico type zib_cte_dist_ter-cd_tipo_servico,
       end of ty_zib_cte_dist_ter,

       begin of ty_lfa1,
         name1 type lfa1-name1,
         stcd1 type lfa1-stcd1,
         lifnr type lfa1-lifnr,
         stcd3 type lfa1-stcd3,
         stcd2 type lfa1-stcd2, "USER STORY 163035 - MMSILVA - 08.01.2024
       end of ty_lfa1,

       begin of ty_j_1bnfdoc,
         docnum type j_1bnfdoc-docnum,
         nftype type j_1bnfdoc-nftype,
         doctyp type j_1bnfdoc-doctyp,
         direct type j_1bnfdoc-direct,
         docdat type j_1bnfdoc-docdat,
         cancel type j_1bnfdoc-cancel,
         nfenum type j_1bnfdoc-nfenum,
         series type j_1bnfdoc-series,
         parid  type j_1bnfdoc-parid,
         pstdat type j_1bnfdoc-pstdat,
         refkey type j_1bnflin-refkey,
         model  type j_1bnfdoc-model,
         branch type j_1bnfdoc-branch,
         candat type j_1bnfdoc-candat,
       end of ty_j_1bnfdoc,

       begin of ty_j_1bnfe_active,
         docnum type j_1bnfe_active-docnum,
         model  type j_1bnfe_active-model,
         nfnum9 type j_1bnfe_active-nfnum9,
         serie  type j_1bnfe_active-serie,
         stcd1  type j_1bnfe_active-stcd1,
         direct type j_1bnfe_active-direct,
         partyp type j_1bnfe_active-partyp,
         bukrs  type j_1bnfe_active-partyp,
         branch type j_1bnfe_active-partyp,
       end of ty_j_1bnfe_active,


       begin of ty_zib_nfe_forn,
         docnum     type zib_nfe_forn-docnum,
         nu_chave   type zib_nfe_forn-nu_chave,
         docnum_ref type zib_nfe_forn-docnum_ref, "PBI - 72503 - CSB
       end of ty_zib_nfe_forn,

       begin of ty_ekko,
         ebeln type ekko-ebeln,
         lifnr type ekko-lifnr,
         zterm type ekko-zterm,
       end of ty_ekko,

       begin of ty_ekbe,
         ebeln type ekbe-ebeln,
         belnr type ekbe-belnr,
         gjahr type ekbe-gjahr,
         vgabe type ekbe-vgabe,
         xblnr type ekbe-xblnr,
         lifnr type ekko-lifnr,
         shkzg type ekbe-shkzg,
         zterm type ekko-zterm,
       end of ty_ekbe,

       begin of ty_mkpf,
         mblnr type mkpf-mblnr,
         mjahr type mkpf-mjahr,
         budat type mkpf-budat,
       end of ty_mkpf,

       begin of ty_rbkp,
         bukrs  type rbkp-bukrs,
         belnr  type rbkp-belnr,
         gjahr  type rbkp-gjahr,
         budat  type rbkp-budat,
         lifnr  type rbkp-lifnr,
         refkey type j_1bnflin-refkey,
         xblnr  type rbkp-xblnr,
         zterm  type rbkp-zterm,
         stblg  type rbkp-stblg, "*-CS2022000243-#76365-26.04.2022-JT-inicio
       end of ty_rbkp,

       begin of ty_dados_filial,
         bukrs      type j_1bbranch-bukrs,
         branch     type j_1bbranch-branch,
         stcd1      type j_1bbranch-stcd1,
         state_insc type j_1bbranch-state_insc,
       end of ty_dados_filial,

       "US #164021 - MMSILVA - 27.02.2025 - Inicio
       begin of ty_miro_cte,
         docnum type j_1bnflin-docnum,
         refkey type j_1bnflin-refkey,
       end of ty_miro_cte,

       begin of ty_migo_cte,
         belnr type ekbe-belnr,
         lfbnr type ekbe-lfbnr,
         vgabe type ekbe-vgabe,
         budat type ekbe-budat,
       end of ty_migo_cte.
"US #164021 - MMSILVA - 27.02.2025 - Fim

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

class lcl_alv_toolbar_0100 definition.
  public section.
*Constructor
    methods: constructor
      importing io_alv_grid type ref to cl_gui_alv_grid,
*Event for toolbar
      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm.

endclass.                    "LCL_ALV_TOOLBAR DEFINITION

class lcl_event_handler_0100 definition.
  public section.
    class-methods: handle_hotspot_click for event hotspot_click of cl_gui_alv_grid importing e_row_id e_column_id es_row_no.
endclass.

class lcl_alv_toolbar_0105 definition.
  public section.
*Constructor
    methods: constructor
      importing io_alv_grid type ref to cl_gui_alv_grid,
*Event for toolbar
      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm.

endclass.                    "LCL_ALV_TOOLBAR DEFINITION

class lcl_event_handler_0105 definition.
  public section.
    class-methods: handle_hotspot_click for event hotspot_click of cl_gui_alv_grid importing e_row_id e_column_id es_row_no.
endclass.

class lcl_timer definition.
  public section.
    methods:
      handle_finished for event finished of cl_gui_timer.
endclass.                    "lcl_receiver DEFINITION

*----------------------------------------------------------------------*
* Estruturas ALV
*----------------------------------------------------------------------*

data: obj_alv_0100       type ref to cl_gui_alv_grid,
      obj_container_0100 type ref to cl_gui_custom_container,
      obj_alv_0105       type ref to cl_gui_alv_grid,
      obj_container_0105 type ref to cl_gui_custom_container.

data: lcl_event_handler_0100 type ref to lcl_event_handler_0100,
      lcl_event_handler_0105 type ref to lcl_event_handler_0105.

* ALV field catalogs
data: it_fcat type lvc_t_fcat,
      wa_fcat type lvc_s_fcat.

* ALV excluded functions
data: it_exclude_fcode type ui_functions,
      wa_exclude_fcode like line of it_exclude_fcode.

* Alv Styles
data: ls_edit type lvc_s_styl,
      lt_edit type lvc_t_styl.

* ALV layout variant
data: gs_variant   type disvariant,
      variante     like disvariant,
      gs_variant_c type disvariant.

* ALV layout
data: gs_layout        type lvc_s_layo.

* ALV Stable
data: wa_stable        type lvc_s_stbl.

data: it_sel_rows type lvc_t_row,
      wa_sel_rows type lvc_s_row.

data: obj_toolbar_0100 type ref to lcl_alv_toolbar_0100,
      obj_toolbar_0105 type ref to lcl_alv_toolbar_0105.

data: gt_estilo type lvc_t_styl with header line,
      wl_estilo type lvc_s_styl.

data: gt_f4  type lvc_t_f4 with header line.

* Objetos para Timer
data: ob_timer       type ref to cl_gui_timer,
      ob_recev       type ref to lcl_timer,
      vg_timer_ativo type c.

* Objetos
data: c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      ty_toolbar           type stb_button.

data: vg_row type lvc_s_row,
      vg_col type lvc_s_col.

* Variaveis
data: wl_centro(50)   type c,
      wl_material(50) type c,
      wl_dt_mov(50)   type c,
      vg_layout_0101  type c.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
data: it_saida_0100           type table of ty_saida_0100,
      wa_saida_0100           type ty_saida_0100,
      it_saida_0105           type table of ty_saida_0105,
      wa_saida_0105           type ty_saida_0105,
      tg_zib_nfe_dist_ter     type table of ty_zib_nfe_dist_ter with header line,
      tg_zib_nfe_f            type table of ty_zib_nfe_forn with header line,
      tg_xml                  type znfe_xml_sefaz_auth,
      tg_xml_cte              type zcte_xml_sefaz_auth,
      w_icmstot               type zintg_icms00,
      tg_zib_nfe_dist_itm     type table of ty_zib_nfe_dist_itm with header line,
      tg_zib_nfe_forn         type table of zib_nfe_forn        with header line,
      tg_zib_nfe_forn_aux     type table of zib_nfe_forn        with header line,
      tg_zib_nfe_dist_ter_aux type table of ty_zib_nfe_dist_ter with header line,

      tg_zib_cte_dist_ter     type table of ty_zib_cte_dist_ter with header line,
      tg_zib_cte_dist_ter_aux type table of ty_zib_cte_dist_ter with header line,
      tg_dados_filial         type table of ty_dados_filial     with header line,
      tg_lfa1                 type table of ty_lfa1             with header line,
      tg_lfa1_aux             type table of ty_lfa1             with header line,
      tg_zsdt0127             type table of zsdt0127            with header line,
      tg_zsdt0127_aut         type table of zsdt0127            with header line,
      tg_j_1bnfstx            type table of j_1bnfstx           with header line,
      tg_j_1bnfdoc            type table of ty_j_1bnfdoc        with header line,
      tg_j_1bnfe_active       type table of ty_j_1bnfe_active   with header line,
      it_zib_nfe_forn         type table of ty_zib_nfe_forn     with header line,
      tg_ekko                 type table of ty_ekko             with header line,
      tg_ekbe                 type table of ty_ekbe             with header line,
      tg_ekbe_aux             type table of ty_ekbe             with header line,
      tg_mkpf                 type table of ty_mkpf             with header line,
      tg_rbkp                 type table of ty_rbkp             with header line,
      tg_zib_nfe_dist_ref     type table of zib_nfe_dist_ref    with header line,

      wa_miro_cte             type ty_miro_cte, "US #164021 - MMSILVA - 27.02.2025
      wa_migo_cte             type ty_migo_cte, "US #164021 - MMSILVA - 27.02.2025
      wa_migo_cte2            type ty_migo_cte. "US #164021 - MMSILVA - 27.02.2025

*-------------------------------------------------------------------
* Ranges
*-------------------------------------------------------------------

ranges: r_modelo  for zib_nfe_forn-nu_chave_modelo,
        r_chave   for zib_nfe_forn-nu_chave,
        r_st_xml  for zib_nfe_forn-st_nota,
        r_st_for  for zib_nfe_dist_ter-forne_ie, "RJF
        r_st_mnf  for zsdt0127-cd_operacao,
        r_st_lct  for zib_nfe_forn-st_nota.

*-------------------------------------------------------------------
* Variáveis de Status
*-------------------------------------------------------------------

data: it_value type vrm_values,
      wa_value type vrm_value.

*----------------------------------------------------------------------*
* tela de Seleção
*----------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-001.
  select-options: p_bukrs    for  zib_nfe_dist_ter-bukrs,      " OBLIGATORY,
                  p_branch   for  zib_nfe_dist_ter-branch,     " OBLIGATORY,
                  p_lifnr    for  lfa1-lifnr, "USER STORY 163035 - MMSILVA - 08.01.2024
                  p_stcd2    for  lfa1-stcd2, "USER STORY 163035 - MMSILVA - 08.01.2024
                  p_cnpj     for  zib_nfe_dist_ter-forne_cnpj,
                  p_for_ie   for  zib_nfe_dist_ter-forne_ie,
                  p_data     for  zib_nfe_dist_ter-dt_emissao, "NO-EXTENSION, " OBLIGATORY,
                  p_nota     for  zib_nfe_dist_ter-numero,
                  p_chave    for zfit0230-chave_acesso. "US #172277 - MMSILVA - 28.03.2025
*                  p_chave    for zib_nfe_forn-nu_chave. "US #172277 - MMSILVA - 28.03.2025 - Comentado devido nova formatação
  parameters:     p_modelo   type zib_nfe_dist_ter-model as listbox visible length 10 obligatory default '55'
                                  user-command us1.
  parameters:     p_distri    type c as checkbox default 'X'.
selection-screen: end of block b1.

selection-screen begin of block b2 with frame title text-002.
  selection-screen comment /1(50) descr1.
  parameters: p_xml1 as checkbox, "Autorizado
              p_xml2 as checkbox. "cancelado

  selection-screen skip 1.
  selection-screen comment /1(50) descr2.
  parameters: p_mnf1 as checkbox,  "Sem manifesto
              p_mnf2 as checkbox,  "Ciencia da operacao
              p_mnf3 as checkbox,  "Confirmação da operacao
              p_mnf4 as checkbox,  "Operação nao realizada
              p_mnf5 as checkbox,  "Desconhecimento da Operacao
              p_mnf6 as checkbox.  "Desacordo de Entrega de Serviços (CT-e)

  selection-screen skip 1.
  selection-screen comment /1(50) descr3.
  parameters: p_lct1 as checkbox,  "Lançados
              p_lct2 as checkbox.  "Pendentes

  selection-screen skip 1.

*PARAMETERS:  p_st_xml(02) TYPE c AS LISTBOX VISIBLE LENGTH 15.
*PARAMETERS:  p_st_mnf(06) TYPE c AS LISTBOX VISIBLE LENGTH 35.
*PARAMETERS:  p_st_lct(02) TYPE c AS LISTBOX VISIBLE LENGTH 15.
selection-screen end of block b2.

*-CS2020001253 - 16.07.2021 - JT - inicio
at selection-screen.
  if p_chave[] is initial.
    if p_bukrs[] is initial.
      message s000(z01) with 'Campo Empresa obrigatório' display like 'E'.
      stop.
    endif.
    if p_branch[] is initial.
      message s000(z01) with 'Campo Filial obrigatório' display like 'E'.
      stop.
    endif.
    if p_data[] is initial.
      message s000(z01) with 'Campo Data Emissão obrigatório' display like 'E'.
      stop.
    endif.
  endif.
*-CS2020001253 - 16.07.2021 - JT - fim

* US #172277 - MMSILVA - 28.03.2025 - Inicio
  loop at p_chave.

    REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN p_chave-low WITH ''.
    CONDENSE p_chave-low NO-GAPS.

    MODIFY p_chave.

  endloop.
* US #172277 - MMSILVA - 28.03.2025 - Fim

at selection-screen output.
  descr1 = 'XML'.
  descr2 = 'Manifesto'.
  descr3 = 'Lançamento'.

*-CS2020001253 - 16.07.2021 - JT - inicio
  loop at screen.
    if screen-name = 'P_BUKRS-LOW'  or
       screen-name = 'P_BRANCH-LOW' or
       screen-name = 'P_DATA-LOW'.
      if p_chave[] is initial.
        screen-required = 2.
      else.
        screen-required = 0.
      endif.
    endif.
*-CS2022000243-#76365-26.04.2022-JT-inicio
    if p_modelo = '55'.
      if screen-name = 'P_MNF6'.
        p_mnf6       = abap_false.
        screen-input = 0.
      endif.
    endif.
*-CS2022000243-#76365-26.04.2022-JT-fim

    modify screen.
  endloop.
*-CS2020001253 - 16.07.2021 - JT - fim

initialization.

  perform f_config_list.

start-of-selection.

  perform f_config_ranges.
  perform f_iniciar_variaveis.
  perform f_atualiza_fiscal.
  perform f_selecionar_dados.
  perform f_processar_dados.
  perform f_imprimir_dados.
