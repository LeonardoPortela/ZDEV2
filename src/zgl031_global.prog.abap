*&---------------------------------------------------------------------*
*&  Include           ZGL031_GLOBAL
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2JVI |12/05/2025 |Ajuste Campo Competencia para  &*
*&                                    |range para os tipos 17, 29, 21 &*
*&                                    |e '22'                         &*
*&                                    |Chamado: 164255.               &*
*&--------------------------------------------------------------------&*
REPORT  zgl031.

TYPE-POOLS: vrm, icon.
TABLES: icon, j_1bbranch, zglt050, zglt073.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_zglt050,
         status     TYPE icon-id,
         inf_tp_opr TYPE c.
         INCLUDE STRUCTURE zglt050.
TYPES END OF ty_zglt050.

TYPES: BEGIN OF ty_zglt073,
         obj_key TYPE zib_contabil-obj_key.
         INCLUDE STRUCTURE zglt073.
TYPES END OF ty_zglt073.

TYPES: BEGIN OF ty_saida_0120,
         seq_lcto       TYPE zglt067-seq_lcto,
         cbx_ctas_pgar  TYPE c LENGTH 1,
         status         TYPE icon-id,
         nro_parc       TYPE num03,
         bukrs          TYPE t001-bukrs,
         filial         TYPE j_1bbranch-branch,
         taxa_cambio    TYPE zglt067-wkurs,
         vlr_premio_usd TYPE zglt067-vlr_premio_usd,
         vlr_premio_brl TYPE zglt067-vlr_premio_brl,
         dt_venc        TYPE zglt067-dt_venc,
         pais_pgto      TYPE zglt067-banks,
         forma_pgto     TYPE zglt067-zlsch,
         bco_empresa    TYPE zglt067-hbkid,
         bco_parceiro   TYPE zglt067-bvtyp,
         bloq_pgto      TYPE zglt067-zlspr,
         lote           TYPE zglt067-lote,
         nro_documento  TYPE zglt067-doc_lcto,
         doc_contabil   TYPE zib_contabil_chv-belnr,
         dt_lcto_ctb    TYPE zglt067-dt_lcto_ctb,
         estilo         TYPE lvc_t_styl,
         cod_barras     TYPE zglt067-cod_barras,
       END OF ty_saida_0120,

       BEGIN OF ty_saida_0130,
         nr_item         TYPE zglt068-nr_item,
         st_baixa        TYPE char04,
         filial          TYPE j_1bbranch-branch,
         chassi          TYPE zglt068-invnr,
         nr_serie        TYPE am_sernr,
         imobilizado     TYPE zglt068-anln1,
         subnumero       TYPE zglt068-anln2,
         mercadoria      TYPE zglt068-matnr,
         descr_bens      TYPE zglt068-descr_bens,
         centro_custo    TYPE zglt068-kostl,
         aufnr           TYPE zglt036-aufnr,
         vornr           TYPE zglt036-vornr,
         uf              TYPE char2,
         cidade          TYPE char15,
         taxa_cambio     TYPE zglt067-wkurs,
         vlr_premio_usd  TYPE zglt068-vlr_premio_usd,
         vlr_premio_brl  TYPE zglt068-vlr_premio_brl,
         vlr_aj_prem_usd TYPE zglt068-vlr_aj_prem_usd,
         vlr_aj_prem_brl TYPE zglt068-vlr_aj_prem_brl,
         vlr_risco_usd   TYPE zglt068-vlr_risco_usd,
         vlr_risco_brl   TYPE zglt068-vlr_risco_brl,
         dt_inic_vigenc  TYPE zglt068-dt_in_vig,
         dt_baixa        TYPE zglt068-dt_baixa,
         clau_benef      TYPE char10,
         banco           TYPE zglt068-banco,
         estilo          TYPE lvc_t_styl,
*** Stefanini - IR206194 - 28/11/2024 - LAZAROSR - Início de Alteração
*         PROP_BAIXA      TYPE P DECIMALS 3, " DECIMALS 2 - RMNI Ajuste bug encontrado de arredondamento incorreto IR082587
         prop_baixa      TYPE p DECIMALS 10, " DECIMALS 2 - RMNI Ajuste bug encontrado de arredondamento incorreto IR082587
*** Stefanini - IR206194 - 28/11/2024 - LAZAROSR - Fim de Alteração
       END OF ty_saida_0130,

       BEGIN OF ty_saida_0132,
         check           TYPE c,
         nr_item         TYPE zglt068-nr_item,
         filial          TYPE j_1bbranch-branch,
         chassi          TYPE zglt068-invnr,
         nr_serie        TYPE am_sernr,
         imobilizado     TYPE zglt068-anln1,
         subnumero       TYPE zglt068-anln2,
         mercadoria      TYPE zglt068-matnr,
         descr_bens      TYPE zglt068-descr_bens,
         centro_custo    TYPE zglt068-kostl,
         uf              TYPE char2,
         cidade          TYPE char15,
         taxa_cambio     TYPE zglt067-wkurs,
         vlr_premio_usd  TYPE zglt068-vlr_premio_usd,
         vlr_premio_brl  TYPE zglt068-vlr_premio_brl,
         vlr_aj_prem_usd TYPE zglt068-vlr_aj_prem_usd,
         vlr_aj_prem_brl TYPE zglt068-vlr_aj_prem_brl,
         vlr_risco_usd   TYPE zglt068-vlr_risco_usd,
         vlr_risco_brl   TYPE zglt068-vlr_risco_brl,
         dt_inic_vigenc  TYPE zglt068-dt_in_vig,
         dt_baixa        TYPE zglt068-dt_baixa,
         clau_benef      TYPE char10,
         banco           TYPE zglt068-banco,
         estilo          TYPE lvc_t_styl,
       END OF ty_saida_0132,

       BEGIN OF ty_saida_0150,
         mark           TYPE c,
         status         TYPE icon-id,
         seq_lcto       TYPE zseq_lcto,
         nro_apolice    TYPE znr_apolice,
         doc_contabil   TYPE zib_contabil_chv-belnr,
         dt_apropr      TYPE zglt073-dt_apropr,
         nro_parc       TYPE num03,
         bukrs          TYPE bukrs,
         waers          TYPE waers,
         nr_item        TYPE zglt073-nr_item,
         anln1          TYPE anln1,
         anln2          TYPE anln2,
         invnr          TYPE zglt073-invnr,
         sernr          TYPE zglt073-sernr,
         matnr          TYPE matnr,
         descr_bens     TYPE char40,
         werks          TYPE char04,
         kostl          TYPE kostl,
         wkurs          TYPE wkurs,
         vlr_premio_usd TYPE zglt073-vlr_premio_usd,
         vlr_premio_brl TYPE zglt073-vlr_premio_brl,
         dt_in_vig      TYPE dats,
         dt_baixa       TYPE dats,
         lote           TYPE zlote_num,
         doc_lcto       TYPE num10,
         month_baixa    TYPE zglt073-month_baixa,
         xblnr          TYPE xblnr,   "RJF - 83767
       END OF ty_saida_0150,

       BEGIN OF ty_saida_0160,
         seq_lcto       TYPE zglt067-seq_lcto,
         cbx_ctas_pgar  TYPE c LENGTH 1,
         status         TYPE icon-id,
         nro_parc       TYPE num03,
         bukrs          TYPE zglt050-bukrs,
         filial         TYPE j_1bbranch-branch,
         taxa_cambio    TYPE zglt067-wkurs,
         vlr_premio_usd TYPE zglt067-vlr_premio_usd,
         vlr_premio_brl TYPE zglt067-vlr_premio_brl,
         dt_venc        TYPE zglt067-dt_venc,
         pais_pgto      TYPE zglt067-banks,
         forma_pgto     TYPE zglt067-zlsch,
         bco_empresa    TYPE zglt067-hbkid,
         bco_parceiro   TYPE zglt067-bvtyp,
         bloq_pgto      TYPE zglt067-zlspr,
         lote           TYPE zglt067-lote,
         nro_documento  TYPE zglt067-doc_lcto,
         doc_contabil   TYPE zib_contabil_chv-belnr,
         dt_lcto_ctb    TYPE zglt067-dt_lcto_ctb,
         estilo         TYPE lvc_t_styl,
         cod_barras     TYPE zglt067-cod_barras,
       END OF ty_saida_0160,

       BEGIN OF ty_saida_0170,
         mark             TYPE c,
         status           TYPE icon-id,
         seq_lcto         TYPE zseq_lcto,
         nro_apolice      TYPE znr_apolice,
         doc_contabil     TYPE zib_contabil_chv-belnr,
         bukrs            TYPE bukrs,
         waers            TYPE waers,
         nr_item          TYPE zglt073-nr_item,
         anln1            TYPE anln1,
         anln2            TYPE anln2,
         invnr            TYPE zglt073-invnr,
         sernr            TYPE zglt073-sernr,
         matnr            TYPE matnr,
         descr_bens       TYPE char40,
         werks            TYPE char04,
         kostl            TYPE kostl,
         wkurs            TYPE wkurs,
         vlr_premio_usd   TYPE zglt068-vlr_premio_usd,
         vlr_premio_brl   TYPE zglt068-vlr_premio_brl,
         vlr_bx_usd       TYPE zglt068-vlr_premio_usd, "Novo
         vlr_bx_brl       TYPE zglt068-vlr_premio_brl, "Novo
         aprop_usd        TYPE zglt068-vlr_premio_usd, "Novo
         aprop_brl        TYPE zglt068-vlr_premio_brl, "Novo
         saldo_usd        TYPE zglt068-vlr_premio_usd, "Novo
         saldo_brl        TYPE zglt068-vlr_premio_brl, "Novo
         vlr_ajuste_usd   TYPE zglt068-vlr_premio_usd, "Novo
         vlr_ajuste_brl   TYPE zglt068-vlr_premio_brl, "Novo
         dt_baixa         TYPE dats,
         lote_ajus        TYPE zlote_num,
         doc_lcto_ajus    TYPE num10,
         nro_aprop        TYPE i,                       "Novo
         perc_aprop       TYPE p DECIMALS 2,            "Novo
         dt_lcto_ctb_ajus TYPE zglt067-dt_lcto_ctb,
       END OF ty_saida_0170,

       BEGIN OF ty_saida_0200,
         status       TYPE icon-id,
         msg_erro     TYPE char200,
         doc_contabil TYPE zib_contabil_chv-belnr,
         index_erro   TYPE sy-tabix,
       END OF ty_saida_0200,

       BEGIN OF ty_cabecalho_0110,
         butxt      TYPE text30,
         tipo       TYPE text30,
         modalidade TYPE text30,
         corretora  TYPE text30,
         seguradora TYPE text30,
       END OF ty_cabecalho_0110,

       BEGIN OF ty_cabecalho_0150,
         bukrs        TYPE bukrs,
         descr_bukrs  TYPE text50,
         descr_tipo   TYPE text50,
**<<<------"164255 - NMS - INI------>>>
*         seq_tipo     TYPE num2,
         seq_tipo     TYPE n LENGTH 3,
**<<<------"164255 - NMS - FIM------>>>
         seq_lcto     TYPE zglt050-seq_lcto,
         dt_aprop_de  TYPE zglt050-vig_de,
         dt_aprop_ate TYPE zglt050-vig_ate,
**<<<------"164255 - NMS - INI------>>>
*         COMPETENCIA  TYPE C LENGTH 6,
         competencia  TYPE spmon,
         competenci2  TYPE spmon,
         dtapr_ult_de TYPE dats,
**<<<------"164255 - NMS - FIM------>>>
       END OF ty_cabecalho_0150,

       BEGIN OF ty_cabecalho_0160,
         bukrs        TYPE bukrs,
         descr_bukrs  TYPE text50,
         descr_tipo   TYPE text50,
         seq_tipo     TYPE num03,
         seq_lcto     TYPE zglt050-seq_lcto,
         dt_aprop_de  TYPE zglt050-vig_de,
         dt_aprop_ate TYPE zglt050-vig_ate,
**<<<------"164255 - NMS - INI------>>>
*         COMPETENCIA  TYPE C LENGTH 6,
         competencia  TYPE spmon,
**<<<------"164255 - NMS - FIM------>>>
       END OF ty_cabecalho_0160,

       BEGIN OF ty_cabecalho_0170,
         bukrs        TYPE bukrs,
         descr_bukrs  TYPE text50,
         descr_tipo   TYPE text50,
         seq_tipo     TYPE num2,
         seq_lcto     TYPE zglt050-seq_lcto,
         dt_aprop_de  TYPE zglt050-vig_de,
         dt_aprop_ate TYPE zglt050-vig_ate,
*         COMPETENCIA  TYPE C LENGTH 6,      "<<<------"164255 - NMS------>>>
       END OF ty_cabecalho_0170,

       BEGIN OF ty_werks,
         werks TYPE zglt068-werks,
         kostl TYPE zglt068-kostl,
       END OF ty_werks,

       BEGIN OF ty_fields,
         campo     TYPE screen-name,
         group1    TYPE char3,
         group2    TYPE char3,
         value     TYPE char1,
         invisible TYPE char1,
       END OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END OF ty_editor,

       BEGIN OF ty_menu_tree,
         node_pai   TYPE char50,
         node_filho TYPE char50,
       END OF ty_menu_tree.

DATA: obj_docking           TYPE REF TO cl_gui_docking_container,
      obj_custom_tree       TYPE REF TO cl_gui_custom_container,
      obj_alv_tree          TYPE REF TO cl_gui_alv_tree,
      obj_custom_txt        TYPE REF TO cl_gui_custom_container,
      obj_custom_editor     TYPE REF TO cl_gui_textedit,
      obj_custom_0120       TYPE REF TO cl_gui_custom_container,
      obj_alv_0120          TYPE REF TO cl_gui_alv_grid,
      obj_custom_0130       TYPE REF TO cl_gui_custom_container,
      obj_custom_0132       TYPE REF TO cl_gui_custom_container,
      obj_alv_0130          TYPE REF TO cl_gui_alv_grid,
      obj_alv_0132          TYPE REF TO cl_gui_alv_grid,
      obj_custom_0150       TYPE REF TO cl_gui_custom_container,
      obj_custom_0160       TYPE REF TO cl_gui_custom_container,
      obj_custom_0170       TYPE REF TO cl_gui_custom_container,
      obj_alv_0150          TYPE REF TO cl_gui_alv_grid,
      obj_alv_0160          TYPE REF TO cl_gui_alv_grid,
      obj_alv_0170          TYPE REF TO cl_gui_alv_grid,
      obj_alv_0200          TYPE REF TO cl_gui_alv_grid,
      obj_custom_0200       TYPE REF TO cl_gui_custom_container,
      obj_splitter          TYPE REF TO cl_gui_splitter_container,
      obj_custom_splitter   TYPE REF TO cl_gui_container,
      obj_toolbar_manager   TYPE REF TO cl_alv_grid_toolbar_manager,
      obj_dyndoc_id         TYPE REF TO cl_dd_document,
      gt_fcat_0100          TYPE TABLE OF lvc_s_fcat,
      gt_menu_tree          TYPE TABLE OF ty_menu_tree,
      gt_saida_0120         TYPE TABLE OF ty_saida_0120,
      gt_saida_0130         TYPE TABLE OF ty_saida_0130,
      gt_saida_0132         TYPE TABLE OF ty_saida_0132,
      gt_saida_0150         TYPE TABLE OF ty_saida_0150,
      gt_saida_0160         TYPE TABLE OF ty_saida_0160,
      gt_saida_0170         TYPE TABLE OF ty_saida_0170,
      gt_saida_0200         TYPE TABLE OF ty_saida_0200,
      gt_editor             TYPE TABLE OF ty_editor,
      gt_fcat_0120          TYPE TABLE OF lvc_s_fcat,
      gt_fcat_0130          TYPE TABLE OF lvc_s_fcat,
      gt_fcat_0132          TYPE TABLE OF lvc_s_fcat,
      gt_fcat_0150          TYPE TABLE OF lvc_s_fcat,
      gt_fcat_0160          TYPE TABLE OF lvc_s_fcat,
      gt_fcat_0170          TYPE TABLE OF lvc_s_fcat,
      gt_fcat_0200          TYPE TABLE OF lvc_s_fcat,
      gt_msg_return         TYPE TABLE OF zfiwrs0002,
      gt_zglt064            TYPE TABLE OF zglt064,
      gt_zglt067            TYPE TABLE OF zglt067,
      gt_zglt068            TYPE TABLE OF zglt068,
      gt_zglt079            TYPE TABLE OF zglt079,
      gt_fields             TYPE TABLE OF ty_fields WITH HEADER LINE,
      gt_zglt073            TYPE TABLE OF zglt073,
      gt_zglt073_ctb        TYPE TABLE OF ty_zglt073,
      gt_zib_chv            TYPE TABLE OF zib_contabil_chv,
      gt_lfbk               TYPE TABLE OF lfbk,
      gt_mara               TYPE TABLE OF mara,
      gt_makt               TYPE TABLE OF makt,
      gt_anla               TYPE TABLE OF anla,
      gt_bdc                TYPE TABLE OF bdcdata,
      gt_selected_rows      TYPE lvc_t_row,
      gt_zglt050            TYPE TABLE OF zglt050,
      gt_zglt031            TYPE TABLE OF zglt031,
      gt_zglt032            TYPE TABLE OF zglt032,
      wl_variant_0120       TYPE disvariant,
      wl_variant_0130       TYPE disvariant,
      wl_variant_0132       TYPE disvariant,
      wl_variant_0150       TYPE disvariant,
      wl_variant_0160       TYPE disvariant,
      wl_variant_0170       TYPE disvariant,
      gt_estilo             TYPE lvc_t_styl WITH HEADER LINE,
      gt_color              TYPE lvc_t_scol WITH HEADER LINE,
      gt_selectedcell       TYPE lvc_t_cell,
      wl_selected_rows      TYPE lvc_s_row,
      lt_nodes_select       TYPE lvc_t_nkey,
      l_active_node         TYPE lvc_nkey,
      wl_fcat               TYPE lvc_s_fcat,
      wl_bdc                TYPE bdcdata,
      wl_menu_tree          TYPE ty_menu_tree,
      wl_zib_chave          TYPE zib_contabil_chv,
      wl_zglt034            TYPE zglt034,
      wl_zib_erro           TYPE zib_contabil_err,
      wl_selectedcell       TYPE lvc_s_cell,
      wl_toolbar            TYPE stb_button,
      wl_fields             TYPE ty_fields,
      wl_cabecalho_0110     TYPE ty_zglt050,
      wl_cabecalho_0150     TYPE ty_cabecalho_0150,
      wl_cabecalho_0160     TYPE ty_cabecalho_0160,
      wl_cabecalho_0170     TYPE ty_cabecalho_0170,
      wl_saida_0120         TYPE ty_saida_0120,
      wl_saida_0130         TYPE ty_saida_0130,
      wl_saida_0132         TYPE ty_saida_0132,
      wl_saida_0150         TYPE ty_saida_0150,
      wl_saida_0160         TYPE ty_saida_0160,
      wl_saida_0170         TYPE ty_saida_0170,
      wl_saida_0200         TYPE ty_saida_0200,
      wl_zglt067            TYPE zglt067,
      wl_zglt068            TYPE zglt068,
      wl_zglt050            TYPE zglt050,
      wl_zglt079            TYPE zglt079,
      gt_exc_button         TYPE ui_functions,
      wl_layout             TYPE lvc_s_layo,
      wl_stable             TYPE lvc_s_stbl,
      wl_exclude            TYPE ui_functions,
      ls_good               TYPE lvc_s_modi,
      wl_msg_return         TYPE zfiwrs0002,
      wl_editor             TYPE ty_editor,
      wl_cell               TYPE lvc_s_cell,
      wl_zglt073            TYPE zglt073,
      wl_lfbk               TYPE lfbk,
      wl_anla               TYPE anla,
      wl_anlz               TYPE anlz,
      wl_mara               TYPE mara,
      wl_makt               TYPE makt,
      wl_zglt064            TYPE zglt064,
      wl_j_1bbranch         TYPE j_1bbranch,
      wl_color              TYPE lvc_s_scol,
      wl_obj(30),
      wl_estilo             TYPE lvc_s_styl,
      wl_cabecalho_0110_aux TYPE ty_cabecalho_0110,
      wl_zglt032            TYPE zglt032,
      wl_change_werks       TYPE ty_werks,
      wl_t001               TYPE t001,
      gt_t001               TYPE TABLE OF t001,
      ok_code               LIKE sy-ucomm,
      wg_mensagem           TYPE char30,
      header_status         TYPE c VALUE abap_false,
      op_modo               TYPE char10,
      vg_tp_lcto            TYPE zglt064-tp_lcto,
      opt                   TYPE ctu_params,
      lines                 TYPE sy-tabix,
      return_status         TYPE c,
      at_index              TYPE sy-tabix,
      cod_fornecedor        TYPE lifnr,
      custom_mode           TYPE i, " 1 = disable | 0 = enable "
      screen_principal      TYPE char4 VALUE '0140',
      screen_item           TYPE char4,
      gv_col                TYPE lvc_s_col,
      gv_row                TYPE lvc_s_row,
      r_bukrs_150           TYPE RANGE OF zglt050-bukrs,
      r_tipo_150            TYPE RANGE OF zglt050-seq_tipo,
      r_seq_150             TYPE RANGE OF zglt050-seq_lcto,
      wa_seq_150            LIKE LINE OF r_seq_150,
      p_file                TYPE rlgrap-filename,
      wa_estrutura          TYPE ty_estrutura,
      it_estrutura          TYPE TABLE OF ty_estrutura,
      r_bukrs_160           TYPE RANGE OF zglt050-bukrs,
      r_tipo_160            TYPE RANGE OF zglt050-seq_tipo,
      r_seq_160             TYPE RANGE OF zglt050-seq_lcto,
      e_status(1),
      autorizado(1),
      edit(1),
      e_messa(64),
      edit_apolice(1), "USER STORY 163032 - MMSILVA - 09.01.2025
      _param                TYPE ustyp_t_parameters, "USER STORY 163032 - MMSILVA - 09.01.2025
      r_parid               TYPE RANGE OF memoryid, "USER STORY 163032 - MMSILVA - 09.01.2025
      c_exib_log            TYPE char01, "USER STORY 163032 - MMSILVA - 09.01.2025
      c_sel_table           TYPE objs-objectname. "USER STORY 163032 - MMSILVA - 09.01.2025

DATA: v_log_erro TYPE c LENGTH 20,
      v_ctr      TYPE c,
      gv_loopscr TYPE c,           "<<<------"164255 - NMS------>>>
      v_menu     TYPE c VALUE 'X'.

CONSTANTS: c_dt_corte_aprov TYPE zglt050-dt_criacao VALUE '20170614'.

CONSTANTS:
  c_screen_0110   TYPE char4  VALUE '0110', "Screen de Cadastro/Ctas Pagar/Bens Assegurados
  c_screen_0120   TYPE char4  VALUE '0120', "Screen Contas Pagar
  c_screen_0130   TYPE char4  VALUE '0130', "Screen Bens Assegurados
  c_screen_0132   TYPE char4  VALUE '0132', "Screen Lista Bens para Baixa
  c_screen_0140   TYPE char4  VALUE '0140', "Screen Vazia
  c_screen_0150   TYPE char4  VALUE '0150', "Screen Relatório Aprop.
  c_screen_0160   TYPE char4  VALUE '0160', "Screen Relatório C.Pagar/Receber
  c_screen_0170   TYPE char4  VALUE '0170', "Screen Ajuste Aprop.
  c_back          TYPE char15 VALUE 'BACK',
  c_canc          TYPE char15 VALUE 'CANC',
  c_exit          TYPE char15 VALUE 'EXIT',
  c_enter         TYPE char15 VALUE 'ENTER',
  c_anexo_apolice TYPE char15 VALUE 'ANEXO_APOLICE',
  c_dele          TYPE char15 VALUE 'DELE',
  c_tp_seguro     TYPE char15 VALUE 'TP_SEGURO',
  c_show_msgre    TYPE char15 VALUE 'SHOW_MSGRE',
  c_btn_buscar    TYPE char15 VALUE 'BTN_PROCURAR',
  c_tipo_moeda    TYPE char15 VALUE 'TIPO_MOEDA',
  c_tp_opr        TYPE char10 VALUE 'TP_OPR',
  c_novo          TYPE char15 VALUE 'NOVO',
  c_save          TYPE char15 VALUE 'SAVE',
  c_search        TYPE char15 VALUE 'SEARCH',
  c_change        TYPE char15 VALUE 'CHANGE',
  c_insert_row    TYPE char15 VALUE 'INSERT_ROW',
  c_ger_ctb_apol  TYPE char15 VALUE 'GER_CTB_APOL',
  c_est_ctb_apol  TYPE char15 VALUE 'EST_CTB_APOL',
  c_disp_ctb_apol TYPE char15 VALUE 'DISP_CTB_APOL',
  c_screen_0100   TYPE char4  VALUE '0100', "USER STORY 163032 - MMSILVA - 09.01.2025
  c_edit_tipo     TYPE char10 VALUE 'EDIT_TIPO'. "USER STORY 163032 - MMSILVA - 09.01.2025

RANGES r_date FOR sy-datum.

RANGES: r_bukrs    FOR zglt050-bukrs,
        r_seq_lcto FOR zglt050-seq_lcto,
        r_seq_tipo FOR zglt050-seq_tipo,
        r_vig_de   FOR zglt050-vig_de,
        r_vig_ate  FOR zglt050-vig_ate.


PARAMETER: p_slcto TYPE zglt050-seq_lcto NO-DISPLAY.

INITIALIZATION. "\\ Start program;

  wl_cabecalho_0170-dt_aprop_de  = sy-datum - 365.
  wl_cabecalho_0170-dt_aprop_ate = sy-datum.

  PERFORM autorizacao.

  CALL SCREEN 0100.
