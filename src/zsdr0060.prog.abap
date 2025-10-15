*&---------------------------------------------------------------------*
*& Report  ZSDR0060
*&
*&---------------------------------------------------------------------*
*&
*& Cockpit Montagem de Cargas - Insumos
*&
*&---------------------------------------------------------------------*
REPORT zsdr0060.
*=============================================================================*
*INCLUDES                                                                     *
*=============================================================================*
*INCLUDE ZSDR0060_TOP.
*=============================================================================*
*TABLES                                                                       *
*=============================================================================*
TABLES: tvko, tvbur, tspa, tinc, kna1, zsdt0129, zsdt0133, zsdt0082,
        zsdt0282, zmmt0196, zmmt0201,zsde_alv_lista_carga.
*=============================================================================*
*DATA                                                                         *
*=============================================================================*

CONTROLS tabstrip2 TYPE TABSTRIP. "Rubenilson Pereira - 22.04.25 #169642

CONTROLS tabstrip_7200 TYPE TABSTRIP. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>

TYPES: BEGIN OF ty_f4_nlote,
         charg     TYPE mseg-charg,
         ebeln     TYPE ekbe-ebeln,
         lgort     TYPE mseg-lgort,
         werks     TYPE mseg-werks,
         matnr     TYPE mseg-matnr,
         vfdat     TYPE mch1-vfdat,
         clabs     TYPE mchb-clabs,
         categoria TYPE zsdt0134-categoria,
*"CS2022000698 - ZSDT0112 - exibir o código do fornecedor na tela de vinculação de lotes / Anderson Oenning
         lifnr     TYPE mslb-lifnr.
*"CS2022000698 - ZSDT0112 - exibir o código do fornecedor na tela de vinculação de lotes / Anderson Oenning
TYPES: END OF ty_f4_nlote.


*** Inicio - Rubenilson Pereira - 22.04.25 #169642

TYPES:
  BEGIN OF ty_tela_monta_carga,
    nro_cg         TYPE zmmt0201-nro_cg,
    nro_sol        TYPE zmmt0196-nro_sol,
    seq            TYPE zmmt0196-seq,
    ponto_coleta   TYPE zmmt0196-parceiro_pc,
    descricao_pc   TYPE lfa1-name1,
    local_entrega  TYPE zmmt0196-parceiro_le,
    descricao_le   TYPE lfa1-name1,
    qtd_tot_prev   TYPE zmmt0196-solicitacao_qte,
    tipo_frete     TYPE inco1,
    data_embarque  TYPE sy-datum,
    frete_por_t    TYPE char1,
    frete_por_v    TYPE char1,
    valor_frete    TYPE dmbtr,
    transportadora TYPE lifnr,
    desc_transp    TYPE name1,
    motorista      TYPE lifnr,
    desc_motorista TYPE name1,
    placa_cavalo   TYPE zplaca,
    placa_carreta1 TYPE zplaca,
    placa_carreta2 TYPE zplaca,
    placa_dolly    TYPE zplaca,
    qtd_prevista   TYPE p LENGTH 7,
    status         TYPE numc2,
    viagem_id      TYPE zmmt0201-viagem_id,
  END OF ty_tela_monta_carga,

  BEGIN OF ty_0203,
    nro_cg    TYPE zmmt0203-nro_cg,
    dt_nfe    TYPE zmmt0203-data_nfe,
    chave_nfe TYPE zmmt0203-chave_nfe,
    chave_cte TYPE zmmt0203-chave_cte,
  END OF ty_0203,

  BEGIN OF ty_dist_ter,
    chave_nfe      TYPE zib_nfe_dist_ter-chave_nfe,
    dt_emissao     TYPE zib_nfe_dist_ter-dt_emissao,
    st_fiscal_data TYPE zib_nfe_dist_ter-st_fiscal_data,
  END OF ty_dist_ter,

  BEGIN OF ty_nfe_itm,
    chave_nfe  TYPE zib_nfe_dist_itm-chave_nfe,
    docnum_nfe TYPE zib_nfe_dist_itm-docnum_nfe,
  END OF ty_nfe_itm,

  BEGIN OF ty_doc,
    docnum TYPE j_1bnfdoc-docnum,
    pstdat TYPE j_1bnfdoc-pstdat,
  END OF ty_doc.
*** Fim - Rubenilson Pereira - 22.04.25 #169642

TYPES: BEGIN OF ty_f4_nfase,
         nr_fase   TYPE zmmt0102-nr_fase,
         categoria TYPE zmmt0102-categoria,
         matnr     TYPE mara-matnr,
         menge     TYPE char18, "zmmt0102-menge,
         lfimg     TYPE char18, "zsdt0134-lfimg,
         sdo_fase  TYPE char18, "zmmt0102-menge,
       END OF ty_f4_nfase.
TYPES:
  BEGIN OF ty_popup_nr_fornecedor,
    nr_forn TYPE zsdt0062-nr_forn,
  END OF ty_popup_nr_fornecedor,

  BEGIN OF ty_popup_vinculacao,
    material TYPE mara-matnr,
    pedido   TYPE ekpo-ebeln,
    "IS_INVALID TYPE ABAP_BOOL,
  END OF ty_popup_vinculacao.

DATA: popup             TYPE ty_popup_nr_fornecedor,
      popup_vinculacao  TYPE ty_popup_vinculacao,
      index_click       TYPE i,
      index_click_frete TYPE i.



TYPES: BEGIN OF ty_f4_nlote_def,
         charg    TYPE mseg-charg,
         ebeln    TYPE ekbe-ebeln,
         lgort    TYPE mseg-lgort,
         werks    TYPE mseg-werks,
         matnr    TYPE mseg-matnr,
         vfdat    TYPE mch1-vfdat,
         clabs    TYPE mchb-clabs,
         qt_carga TYPE zpped006. "MCHB-CLABS.
TYPES: END OF ty_f4_nlote_def.

TYPES: BEGIN OF ty_f4_filial_frete,
         filial_resp TYPE tvkbt-vkbur,
         bezei       TYPE tvkbt-bezei.
TYPES: END OF ty_f4_filial_frete.

TYPES: BEGIN OF ty_f4_pedido,
         ebeln    TYPE zsdt0062-ebeln,
         ebelp    TYPE zsdt0062-ebelp,
         matnr    TYPE zsdt0062-matnr,
         charg    TYPE zsdt0138-charg,
         qtd_vinc TYPE zsdt0062-qtd_vinc.
TYPES: END OF ty_f4_pedido.

TYPES: BEGIN OF ty_f4_ov,
         vbeln TYPE zsdt0144-vbeln,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         lifnr TYPE lfa1-lifnr,
         name2 TYPE lfa1-name1.
TYPES: END OF ty_f4_ov.

TYPES: BEGIN OF ty_f4_wrkst_sem,
         wrkst TYPE mara-wrkst.
TYPES: END OF ty_f4_wrkst_sem.

TYPES: BEGIN OF ty_header_lote,
         marca        TYPE zsdt0129-marca,
         placa_cav    TYPE zsdt0129-placa_cav,
         placa_car1   TYPE zsdt0129-placa_car1,
         placa_car2   TYPE zsdt0129-placa_car1,
         placa_car3   TYPE zsdt0129-placa_car3,
         motorista    TYPE zsdt0129-motorista,
         mot_desc     TYPE lfa1-name1,
         stcd2        TYPE lfa1-stcd2,
         telf1        TYPE lfa1-telf1,
         qtd_total_kg TYPE zsdt0129-qtd_total_kg,
         ctg_transp   TYPE zsdt0129-ctg_transp,
         ctg_transp_d TYPE char10,
         inco1        TYPE zsdt0129-inco1,
         wrkst        TYPE mara-wrkst,
         dt_entrega   TYPE zsdt0129-dt_entrega,
         armazem_org  TYPE zsdt0129-armazem_org. " Rubenilson - 17.09.24 - #144012
TYPES: END OF ty_header_lote.

TYPES: BEGIN OF ty_header_cargad,
         cod_ce      TYPE lfa1-lifnr,
         name1       TYPE lfa1-name1,
         cod_tr      TYPE lfa1-lifnr,
         name2       TYPE lfa1-name1,
         vbeln       TYPE zsdt0082-vbeln,
         cod_mt      TYPE lfa1-lifnr,
         name3       TYPE lfa1-name1,
         placa_cav   TYPE zlest0002-pc_veiculo,
         placa_car1  TYPE zlest0002-pc_veiculo,
         placa_car2  TYPE zlest0002-pc_veiculo,
         placa_car3  TYPE zlest0002-pc_veiculo,
         tipo_rtc    TYPE ztipo_rtc, "*-CS2021000218-30.08.2022-#893743-JT
         cpf_rtc(11) TYPE c, "zsdt0139-cpf_rtc,
         nome_rtc    TYPE zsdt0259-nome,
         cod_ar      TYPE zsdt0139-cod_ar,
         name4       TYPE lfa1-name1.
TYPES: END OF ty_header_cargad.

TYPES: BEGIN OF ty_header_ovs,
         pto_col      TYPE lfa1-lifnr,
         pto_col_desc TYPE lfa1-name1,
         pto_ent      TYPE kna1-kunnr,
         pto_ent_desc TYPE kna1-name1,
         prc_frt      TYPE zsdt0133-preco_frete,
         qte_ov       TYPE vbap-kwmeng,
         meins        TYPE mara-meins.
TYPES:  END OF ty_header_ovs.

TYPES: BEGIN OF ty_f4_loc_emb,
         lifnr          TYPE zsdt0132-lifnr,
         cod_loc_emb    TYPE zsdt0132-nr_rot,
         local_embarq   TYPE zsdt0132-rot_desc,
         armazem        TYPE zsdt0132-armazem,
         transportadora TYPE zsdt0132-transportadora,
         transp_resp    TYPE zsdt0132-transp_resp.
TYPES: END OF ty_f4_loc_emb.

TYPES: BEGIN OF ty_5522,
         adiantamento TYPE zfiwed007,
         observacao   TYPE c LENGTH 255,
         nro_sol      TYPE zsdt0138-nro_sol,
         seq_cam      TYPE zsdt0138-seq_cam,
         seq          TYPE zsdt0138-seq,
         filial_resp  TYPE vkbur,
         frete        TYPE c,
       END OF ty_5522.

TYPES: BEGIN OF ty_0163,
         check     TYPE c,
         lifnr     TYPE lifnr,
         desc      TYPE c LENGTH 50,
         adrnr     TYPE adrnr,
         smtp_addr TYPE ad_smtpadr,
       END OF ty_0163.

TYPES: BEGIN OF ty_enviar,
         lifnr     TYPE lifnr,
         adrnr     TYPE adrnr,
         smtp_addr TYPE ad_smtpadr,
       END OF ty_enviar.


TYPES: BEGIN OF ty_zsdt0131_alv,
         antig      TYPE char1,
         texto      TYPE char5.
         INCLUDE STRUCTURE zsdt0131.
TYPES:   cellstyles TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0131_alv.

TYPES: BEGIN OF ty_vbak_ck,
         vbeln TYPE vbak-vbeln,
         vkbur TYPE vbak-vkbur,
       END OF ty_vbak_ck.

"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
TYPES: BEGIN OF ty_set_pc_sol_7200,
         name_pc TYPE lfa1-name1.
         INCLUDE STRUCTURE zsdt0132.
TYPES: END OF ty_set_pc_sol_7200.
"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

*TYPES:
*  BEGIN OF ty_solic_frete.
*    INCLUDE TYPE zsd_alv_solic_frete.
*TYPES cellstyles TYPE lvc_t_styl.
*TYPES END OF ty_solic_frete.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TABSTRIP'
CONSTANTS: BEGIN OF c_tabstrip,
             tab1 LIKE sy-ucomm VALUE 'TABSTRIP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TABSTRIP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TABSTRIP_FC3',
             tab4 LIKE sy-ucomm VALUE 'TABSTRIP_FC4',
             tab5 LIKE sy-ucomm VALUE 'TABSTRIP_FC5',
             tab6 LIKE sy-ucomm VALUE 'TABSTRIP_FC6',
             tab7 LIKE sy-ucomm VALUE 'TABSTRIP_FC7',
             tab8 LIKE sy-ucomm VALUE 'TABSTRIP_FC8',
           END OF c_tabstrip.

*&SPWIZARD: DATA FOR TABSTRIP 'TABSTRIP'
CONTROLS:  tabstrip TYPE TABSTRIP.
DATA: BEGIN OF g_tabstrip,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZSDR0060',
        pressed_tab LIKE sy-ucomm, "VALUE C_TABSTRIP-TAB1,
      END OF g_tabstrip.
DATA:      ok_code  LIKE sy-ucomm.
DATA:      ok_code2 LIKE sy-ucomm.

DATA: wa_header_lote   TYPE ty_header_lote,
      wa_header_cargad TYPE ty_header_cargad,
      wa_header_ovs    TYPE ty_header_ovs.

DATA: wa_5522 TYPE ty_5522.

DATA: vg_subt_lote   TYPE char4,
      vg_lote_editar TYPE zsdt0129-nro_lote.

DATA: zcl_romaneio TYPE REF TO zcl_romaneio,
      zcx_cadastro TYPE REF TO zcx_cadastro.

DATA: it_0163    TYPE TABLE OF ty_0163,
      it_0164    TYPE TABLE OF zsdt0164,
      it_enviar  TYPE TABLE OF ty_enviar,
      wa_enviar  TYPE ty_enviar,
      _str       TYPE REF TO data,
      it_fcat    TYPE lvc_t_fcat,
      _layout    TYPE lvc_s_layo,
      _function  TYPE ui_functions,
      _stable    TYPE lvc_s_stbl VALUE 'XX',
      _conteiner TYPE REF TO cl_gui_custom_container,
      _grid      TYPE REF TO cl_gui_alv_grid.

DATA: p_ative           TYPE char1.
*      v_troca_nota      TYPE c,
*      v_habi_troca_nota TYPE c,
*      gt_zsdt0346       TYPE TABLE OF zsdt0346.

*-CS2019001891 - 28.06.2021 - JT - inicio
DATA: t_list               TYPE vrm_values,
      w_list               TYPE vrm_value,
      t_values             TYPE TABLE OF dynpread,
      w_values             TYPE dynpread,
      l_selected_value(40) TYPE c,
      t_zsdt0282           TYPE TABLE OF zsdt0282,
      w_zsdt0282           TYPE zsdt0282,
      t_zsdt0282_tot       TYPE TABLE OF zsdt0282,
      w_zsdt0282_tot       TYPE zsdt0282,
      g_filial_resp        TYPE zsdt0282-transp,
      g_tp_tela            TYPE zsdt0282-tp_tela,
      g_nome_transp        TYPE zsdt0282-nome,
      tabstrip_tab4        TYPE char40,
      tabstrip_tab5        TYPE char40,
      tabstrip_tab6        TYPE char40.
*-CS2019001891 - 28.06.2021 - JT - fim

*** Inicio - Rubenilson Pereira - 22.04.25 #169642
DATA: t_0196                    TYPE TABLE OF zmmt0196,
      t_0202                    TYPE TABLE OF zmmt0202,
      t_ekko                    TYPE TABLE OF ekko,
      t_ekpo                    TYPE TABLE OF ekpo,
      t_0200                    TYPE TABLE OF zmmt0200,
      t_lfa1                    TYPE TABLE OF lfa1,
      t_0132                    TYPE TABLE OF zsdt0132,
      t_monta_carga             TYPE TABLE OF zsde_alv_mont_carga,
      ref1                      TYPE REF TO cl_gui_alv_grid,
      ref2                      TYPE REF TO cl_gui_alv_grid,
      init                      TYPE c,
      init2                     TYPE c,
      wa_tela_monta_carga       TYPE ty_tela_monta_carga,
      wa_tela_monta_carga_aux   TYPE ty_tela_monta_carga,
      t_lista_cargas            TYPE TABLE OF zcds_lista_cargas,
      t_alv_lista_cargas        TYPE TABLE OF zsde_alv_lista_carga,
      t_0203                    TYPE TABLE OF zmmt0203,
      t_dist_ter                TYPE TABLE OF ty_dist_ter,
      t_nfe_itm                 TYPE TABLE OF ty_nfe_itm,
      t_doc                     TYPE TABLE OF ty_doc,
      t_sub_itens               TYPE TABLE OF zsde_alv_itens_carga,
      t_sub_itens_del           TYPE TABLE OF zsde_alv_itens_carga,
      g_grid                    TYPE REF TO cl_gui_alv_grid,
      g_grid_chaves             TYPE REF TO cl_gui_alv_grid,
      g_grid_notas              TYPE REF TO cl_gui_alv_grid,
      g_custom_container        TYPE REF TO cl_gui_custom_container,
      g_custom_container_chaves TYPE REF TO cl_gui_custom_container,
      g_custom_container_notas  TYPE REF TO cl_gui_custom_container,
      t_sub_notas               TYPE TABLE OF zsde_alv_sub_notas,
      t_chaves_nf               TYPE TABLE OF zsde_alv_sub_chaves_nf,
      manager                   TYPE REF TO cl_gos_manager,
      obj                       TYPE borident,
      ip_mode                   TYPE sgs_rwmod,
      objtype                   TYPE borident-objtype VALUE 'ANEXO_CARGA',
      wl_stable                 TYPE lvc_s_stbl,
      t_bdcdata                 TYPE TABLE OF bdcdata,
      vg_transf_fornec          TYPE c,
      gv_spart                  TYPE spart,
      gv_edit_log               TYPE c,
      t_status_carga            TYPE TABLE OF tvarvc,
      gv_ucomm                  TYPE sy-ucomm,
      t_chaves_del              TYPE zmmt_chaves_nota,
      t_chaves_transf_del       TYPE zmmt_chaves_nota.

*** Fim - Rubenilson Pereira - 22.04.25 #169642

"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
DATA: gva_vbeln_lote_sel  TYPE zsdt0131-vbeln,
      gva_posnr_lote_sel  TYPE zsdt0131-posnr,
      gva_nr_rot_lote_sel TYPE zsdt0082-nr_rot.

CONSTANTS: c_save_carga_7200         TYPE c LENGTH 100 VALUE 'SAVE',
           c_cancel_opr_7200         TYPE c LENGTH 100 VALUE 'CANCEL',
           c_reenv_transp_safra_7200 TYPE c LENGTH 100 VALUE 'SND_TRA_SF',

           c_refresh_lista_7200      TYPE c LENGTH 100 VALUE 'ATUALIZ_CG',
           c_view_carga_7200         TYPE c LENGTH 100 VALUE 'VIEW_CARGA',
           c_cria_carga_7200         TYPE c LENGTH 100 VALUE 'CRIA_CARGA',
           c_duplo_click_7200        TYPE c LENGTH 100 VALUE '&IC1',         "<<<------"169508 - NMS ------->>>
           c_exibe_legenda_7200      TYPE c LENGTH 100 VALUE 'LEG2',         "<<<------"169508 - NMS ------->>>
           c_determinar_pc_7200      TYPE c LENGTH 100 VALUE 'SET_PC',
           c_edit_solic_7200         TYPE c LENGTH 100 VALUE 'EDIT_SOLIC',
           c_fatura_auto_7200        TYPE c LENGTH 100 VALUE 'FATUR_AUTO',
           c_gera_cotacao_7200       TYPE c LENGTH 100 VALUE 'GERA_COTACAO', "<<<------"169508 - NMS ------->>>
           c_autor_emb_7200          TYPE c LENGTH 100 VALUE 'AUTOR_EMB',
           c_troca_nota_7200         TYPE c LENGTH 100 VALUE 'TROCA_NOTA',
           c_edit_log_7200           TYPE c LENGTH 100 VALUE 'EDIT_LOG',
           c_edit_frete_7200         TYPE c LENGTH 100 VALUE 'EDIT_FRETE',  "*-US192364-14.10.2025-#192364-JT
           c_inf_chave_7200          TYPE c LENGTH 100 VALUE 'INF_CHAVE',
           c_env_doc_carguero_7200   TYPE c LENGTH 100 VALUE 'DOC_CARGUE',
           c_inf_lotes_7200          TYPE c LENGTH 100 VALUE 'INF_LOTES',
           c_conf_carga_7200         TYPE c LENGTH 100 VALUE 'CONF_CARGA',
           c_canc_carga_7200         TYPE c LENGTH 100 VALUE 'CANC_CARGA',
           c_send_carguero_7200      TYPE c LENGTH 100 VALUE 'SEND_CARGU',
           c_gerar_romaneios_7200    TYPE c LENGTH 100 VALUE 'GERAR_ROMA',
           c_vinc_pedidos_7200       TYPE c LENGTH 100 VALUE 'VINC_PEDI'.


"Objetos de Tela ALV Principal Tela 7200

DATA: git_sel_rows_7200            TYPE lvc_t_row.

DATA: ref_7200                     TYPE REF TO cl_gui_alv_grid,
      init_7200                    TYPE c,

      ref_7200_02                  TYPE REF TO cl_gui_alv_grid,
      init_7200_02                 TYPE c,

      "ref2                               TYPE REF TO cl_gui_alv_grid,

      "Tabelas de Saida ALV Principal 7200 - Montagem e Listar Carga

      gwa_set_ponto_coleta_7200    TYPE ty_set_pc_sol_7200,

      git_monta_carga_saida_sem    TYPE zsds381_t,
      git_lista_carga_saida_sem    TYPE zsds382_t,

      "Objetos de Tela 7200
      gob_cc_7200_sol_carga        TYPE REF TO cl_gui_custom_container,
      gob_cc_7200_nf_venda         TYPE REF TO cl_gui_custom_container,
      gob_cc_7200_nf_transf_forn   TYPE REF TO cl_gui_custom_container,
      gob_cc_7200_lotes            TYPE REF TO cl_gui_custom_container,
      gob_cc_7200_bordero          TYPE REF TO cl_gui_custom_container,
      gob_cc_7200_romaneios        TYPE REF TO cl_gui_custom_container,

      gob_splitter_lotes_7200      TYPE REF TO cl_gui_splitter_container,
      gob_parent_ov_lotes_7200     TYPE REF TO cl_gui_container,
      gob_parent_inf_lotes_7200    TYPE REF TO cl_gui_container,


      gob_grid_7200_solicitacoes   TYPE REF TO cl_gui_alv_grid,
      gob_grid_7200_nf_venda       TYPE REF TO cl_gui_alv_grid,
      gob_grid_7200_nf_transf_forn TYPE REF TO cl_gui_alv_grid,
      gob_grid_7200_ov_lotes       TYPE REF TO cl_gui_alv_grid,
      gob_grid_7200_lotes          TYPE REF TO cl_gui_alv_grid,
      gob_grid_7200_bordero        TYPE REF TO cl_gui_alv_grid,
      gob_grid_7200_romaneios      TYPE REF TO cl_gui_alv_grid.


"Work Area e Tabelas de Saida da Tela 7200
DATA: gwa_cab_carga_saida   TYPE zsds382,
      git_7200_solicitacoes TYPE zsds381_t,
      git_7200_notas_venda  TYPE zsdt0410_t,
      git_7200_notas_transf TYPE zsdt0410_t,
      git_7200_bordero      TYPE zsds387_t,
      git_7200_ov_lotes     TYPE zsds381_t,
      git_7200_lotes        TYPE zsdt0134_t,
      git_7200_romaneios    TYPE zless0006_t.
"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

*-US192364-14.10.2025-#192364-JT-inicio
DATA: lwa_preco_frete          TYPE dmbtr,
      lwa_motivo_ajuste_frete1 TYPE char80,
      lwa_motivo_ajuste_frete2 TYPE char80,
      lwa_motivo_ajuste_frete3 TYPE char80,
      ok_code_7211             TYPE syst_ucomm.
*-US192364-14.10.2025-#192364-JT-fim

*-CS2021000218-30.08.2022-#893743-JT-inicio
DATA: w_rtc_proprio        TYPE c,
      w_rtc_terceiro       TYPE c,
      l_cpf_rtc            TYPE zsdt0139-cpf_rtc,
      l_carga_5820_nro_cgd TYPE zsdt0139-nro_cgd.
"gt_solic_frete          TYPE TABLE OF ty_solic_frete,
"gt_solic_frete_bkp      TYPE TABLE OF ty_solic_frete,
"gt_solic_frete_aux      TYPE TABLE OF ty_solic_frete,
"l_deleted.

*      g_custom_container_7103 TYPE REF TO cl_gui_custom_container,
*      ctl_alv1_7103           TYPE REF TO cl_gui_alv_grid.

RANGES: r_werks             FOR zsdt0266-werks.
*-CS2021000218-30.08.2022-#893743-JT-fim


CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      handle_on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id
                  es_row_no.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_on_button_click.

    DATA: tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab,
          wl_field TYPE lvc_s_col,
          v_cont   TYPE i,
          c_x      TYPE c,
          wl_name  TYPE thead-tdname,
          lt_lines TYPE TABLE OF tline.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          is_table    TYPE lvc_s_stbl.

    REFRESH tl_texto.
    CLEAR:wl_texto.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        es_sel_hide = ls_sel_hide
        e_grid      = ref1.

    CALL METHOD ref1->refresh_table_display
      EXPORTING
        is_stable = is_table.

    CALL METHOD cl_gui_cfw=>dispatch.
    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD.

ENDCLASS.
*--------------------------------------------------------------------
*-CS2019001891 - JT - 27.01.2021 - inicio
CLASS lcl_simple_text_editor DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:   co_x  TYPE xfeld VALUE 'X'.

    DATA: container    TYPE REF TO cl_gui_custom_container
                                                    READ-ONLY,
          display_mode TYPE xfeld                   READ-ONLY,
          editor       TYPE REF TO cl_gui_textedit  READ-ONLY,
          longtext_tab TYPE catsxt_longtext_itab    READ-ONLY,
          title        TYPE sytitle                 READ-ONLY.

    METHODS: constructor
      IMPORTING
        im_title        TYPE sytitle
        im_longtext_tab TYPE catsxt_longtext_itab
        im_display_mode TYPE xfeld,

      free,

      get_text
        RETURNING
          VALUE(re_longtext) TYPE catsxt_longtext_itab,

      start.

ENDCLASS.                    "lcl_simple_text_editor DEFINITION

* Begin YEKAL0K026011
CLASS lcl_simple_text_editor IMPLEMENTATION.
  METHOD constructor.

    title        = im_title.
    longtext_tab = im_longtext_tab.
    display_mode = im_display_mode.

  ENDMETHOD.                    "constructor

  METHOD free.
    CALL METHOD: editor->free,
                 container->free.
  ENDMETHOD.                    "free

  METHOD get_text.
    DATA: lf_count TYPE sytabix.

    CALL METHOD editor->get_text_as_r3table
      IMPORTING
        table           = longtext_tab
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        OTHERS          = 3.

    CALL METHOD cl_gui_cfw=>flush.

    lf_count = lines( longtext_tab ).

    DO.
      DELETE longtext_tab FROM  lf_count
                     WHERE table_line IS INITIAL.
      IF sy-subrc IS INITIAL AND lf_count > 1.
        SUBTRACT 1 FROM lf_count.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    re_longtext = longtext_tab.

  ENDMETHOD.                    "get_text

  METHOD start.
    CREATE OBJECT container
      EXPORTING
        container_name              = 'LONGTEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc IS INITIAL.

    CREATE OBJECT editor
      EXPORTING
        parent                 = container
        wordwrap_mode          = '2'
        wordwrap_position      = '72'
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.

    CHECK sy-subrc IS INITIAL.

    IF NOT display_mode IS INITIAL.
*     Set control to display only
      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = editor->true.
    ENDIF.

    CALL METHOD editor->set_text_as_r3table
      EXPORTING
        table           = longtext_tab
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        OTHERS          = 3.

  ENDMETHOD.                    "start

ENDCLASS.                    "lcl_simple_text_editor IMPLEMENTATION

"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
CLASS lcl_carga_7200_sol DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

CLASS lcl_carga_7200_lotes DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

      toolbar FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.



ENDCLASS.


CLASS lcl_carga_7200_romaneios DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

      toolbar FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.


CLASS lcl_carga_7200_ov_lotes DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click_ordem FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,


      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,


      toolbar_ordem FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

*** Inicio - Rubenilson Pereira - 22.04.25 #169642
*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_carga DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_carga IMPLEMENTATION.

  METHOD set_toolbar.

    DATA: wl_toolbar         TYPE stb_button.

    IF g_grid->is_ready_for_input( ) EQ 1.

      READ TABLE e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>)
      WITH KEY function = '&LOCAL&INSERT_ROW'.
      IF sy-subrc IS INITIAL.
        <fs_toolbar>-function = 'INSERT_ROW'.
      ENDIF.

      READ TABLE e_object->mt_toolbar ASSIGNING <fs_toolbar>
      WITH KEY function = '&LOCAL&DELETE_ROW'.
      IF sy-subrc IS INITIAL.
        <fs_toolbar>-function = 'DELETE_ROW'.
      ENDIF.

      wl_toolbar-butn_type    = 3.
      APPEND wl_toolbar TO e_object->mt_toolbar.
      CLEAR wl_toolbar.

      wl_toolbar-function     = 'DEF_AUTO'.
      wl_toolbar-icon         = icon_generate.
      wl_toolbar-butn_type    = 0.
      wl_toolbar-text         = 'Definição Automática'.

      APPEND wl_toolbar TO e_object->mt_toolbar.
      CLEAR wl_toolbar.
    ENDIF.
  ENDMETHOD.                    "SET_TOOLBAR

  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'DEF_AUTO'.

        PERFORM f_define_itens_auto.

      WHEN 'INSERT_ROW'.

        PERFORM f_insere_novos_itens.

      WHEN 'DELETE_ROW'.

        PERFORM f_deleta_linhas.

    ENDCASE.
  ENDMETHOD.                    "GET_UCOMM

  METHOD on_data_changed.

    DATA: ls_good     TYPE lvc_s_modi,
          lv_value    TYPE lvc_value,
          lv_sol      TYPE vbap-kwmeng,
          lv_roteiro  TYPE zsdt0132-nr_rot,
          lv_total    TYPE zmmt0201-qtd_total_kg,
          lv_valor_un TYPE zmmt0201-qtd_total_kg,
          lv_tot_kg   TYPE zmmt0202-qtd_vinc_carga,
          lv_menge    TYPE bstmg,
          lv_modif    TYPE c,
          ls_coluna   TYPE lvc_s_col,
          ls_linha    TYPE lvc_s_row,
          ls_row_no   TYPE lvc_s_roid,
          ls_stable   TYPE lvc_s_stbl.

    DATA: e_route  TYPE trolz-route,
          e_return TYPE bapiret2.

    FIELD-SYMBOLS: <fs_field> TYPE any.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_good>).
      CLEAR lv_modif.

      IF <ls_good>-value IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE t_sub_itens ASSIGNING FIELD-SYMBOL(<fs_sub_itens>) INDEX <ls_good>-row_id.
      ASSIGN COMPONENT <ls_good>-fieldname OF STRUCTURE <fs_sub_itens> TO <fs_field>.

      CASE <ls_good>-fieldname.

        WHEN 'QTD_CARGA'.

          IF <fs_sub_itens>-matnr IS NOT INITIAL.

            lv_modif = abap_true.
            lv_menge = <ls_good>-value.

            CALL METHOD zcl_solicitacao_entrada_insumo=>get_conversao_um
              EXPORTING
                i_matnr     = <fs_sub_itens>-matnr
                i_meins_inp = <fs_sub_itens>-unidade
                i_meins_out = 'KG'
                i_menge     = lv_menge
              RECEIVING
                r_menge     = lv_tot_kg.

            <fs_sub_itens>-qtd_carga_kg = lv_tot_kg.

            LOOP AT t_sub_itens ASSIGNING <fs_sub_itens>.
              lv_total = lv_total + <fs_sub_itens>-qtd_carga_kg.
            ENDLOOP.

            IF lv_total > wa_tela_monta_carga-qtd_prevista AND t_0203 IS INITIAL.
              MESSAGE 'Peso total dos itens ultrapassa total previsto da carga' TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.

          ENDIF.

        WHEN 'NRO_SOL'.

          zcl_solicitacao_entrada_insumo=>saldo_solicitacao(
             EXPORTING
               i_nro_cg          = wa_tela_monta_carga-nro_cg
               i_nro_solicitacao = wa_tela_monta_carga-nro_sol
               i_itens           = abap_true
               i_tp_saldo        = 'G'
             IMPORTING
               e_saldo = DATA(lt_saldo) ).

          SORT lt_saldo BY nro_solic seq tp_saldo.

          lv_modif = abap_true.

          SELECT SINGLE *
            FROM zmmt0196
            INTO @DATA(ls_0196)
            WHERE nro_sol = @<ls_good>-value
              AND seq     = @<fs_sub_itens>-item_solic.
          IF sy-subrc IS INITIAL.

*            SELECT nro_sol, seq, SUM( qtd_vinc_carga ) AS total
*              FROM zmmt0202
*              INTO @DATA(ls_0202)
*              WHERE nro_sol = @ls_0196-nro_sol
*               AND  seq     = @ls_0196-seq
*              GROUP BY nro_sol,seq.
*            ENDSELECT.
*
*            READ TABLE lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>)
*            WITH KEY nro_solic = <ls_good>-value
*                     seq       = <fs_sub_itens>-item_solic
*            BINARY SEARCH.
*            IF sy-subrc IS INITIAL.
*              IF ls_0202-total > 0.
*                <fs_sub_itens>-saldo_a_formar = <fs_saldo>-saldo - ls_0202-total.
*              ELSE.
*                <fs_sub_itens>-saldo_a_formar = <fs_saldo>-saldo.
*              ENDIF.
*            ENDIF.

            READ TABLE lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>)
              WITH KEY nro_solic = <ls_good>-value
                       seq       = <fs_sub_itens>-item_solic
                       tp_saldo  = <fs_sub_itens>-tp_saldo_vinc BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_sub_itens>-saldo_a_formar = <fs_saldo>-saldo.
            ENDIF.

            SELECT SINGLE matnr,meins
              FROM ekpo
              INTO @DATA(ls_ekpo)
              WHERE ebeln = @ls_0196-ebeln
                AND ebelp = @ls_0196-ebelp.
            IF sy-subrc IS INITIAL.
              <fs_sub_itens>-matnr = ls_ekpo-matnr.
              <fs_sub_itens>-unidade = ls_ekpo-meins.

              SELECT SINGLE maktx
                FROM makt
                INTO @DATA(lv_desc)
                WHERE matnr = @ls_ekpo-matnr
                  AND spras = @sy-langu.
              IF sy-subrc IS INITIAL.
                <fs_sub_itens>-maktx = lv_desc.
              ENDIF.
            ENDIF.

            SELECT lifnr,name1
              FROM lfa1
              INTO TABLE @DATA(lt_lfa1)
              WHERE lifnr = @ls_0196-parceiro_pc
                OR  lifnr = @ls_0196-parceiro_le.
            IF sy-subrc IS INITIAL.
              SORT lt_lfa1 BY lifnr.
            ENDIF.

            SELECT SINGLE bukrs,bsart
              FROM ekko
              INTO @DATA(ls_ekko)
              WHERE ebeln = @ls_0196-ebeln.
            IF sy-subrc IS INITIAL.
              <fs_sub_itens>-bukrs = ls_ekko-bukrs.
              <fs_sub_itens>-esart = ls_ekko-bsart.
            ENDIF.

            <fs_sub_itens>-ebeln = ls_0196-ebeln.
            <fs_sub_itens>-ebelp = ls_0196-ebelp.
*            <fs_sub_itens>-qtd_carga = ls_0196-solicitacao_qte.

*            lv_menge = ls_0196-solicitacao_qte.
*
*            CALL METHOD zcl_solicitacao_entrada_insumo=>get_conversao_um
*              EXPORTING
*                i_matnr     = <fs_sub_itens>-matnr
*                i_meins_inp = <fs_sub_itens>-unidade
*                i_meins_out = 'KG'
*                i_menge     = lv_menge
*              RECEIVING
*                r_menge     = lv_tot_kg.
*
*            <fs_sub_itens>-qtd_carga_kg = lv_tot_kg.

            <fs_sub_itens>-ponto_coleta = ls_0196-parceiro_pc.
            READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
            WITH KEY lifnr = ls_0196-parceiro_pc
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_sub_itens>-desc_pc = <fs_lfa1>-name1.
            ENDIF.

            <fs_sub_itens>-local_entrega = ls_0196-parceiro_le.
            READ TABLE lt_lfa1 ASSIGNING <fs_lfa1>
            WITH KEY lifnr = ls_0196-parceiro_le
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_sub_itens>-desc_le = <fs_lfa1>-name1.
            ENDIF.

          ENDIF.

*          ls_coluna-fieldname = 'QTD_CARGA'.
*          ls_linha-index =  <ls_good>-row_id .
*          ls_row_no-row_id = <ls_good>-row_id .
*
*          g_grid->set_current_cell_via_id(
*          EXPORTING
*            is_column_id = ls_coluna
*            is_row_id    = ls_linha
*            is_row_no    = ls_row_no ).

      ENDCASE.


    ENDLOOP.

    IF lv_modif = abap_true.

      ls_stable-row = 'X'.
      ls_stable-col = 'X'.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = ls_stable.
    ENDIF.


  ENDMETHOD.

  METHOD on_f4.

    DATA: lt_saldo   TYPE zsdt_saldo_solic,
          lt_rettab  TYPE TABLE OF ddshretval,
          lw_modi    TYPE lvc_s_modi,
          t_mapping  TYPE TABLE OF  dselc,
          s_mapping  TYPE dselc,
          lt_saldo_2 TYPE TABLE OF zsde_saldo_solic_sh.

    FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

    ASSIGN er_event_data->m_data->* TO <lfst_modi>.
    CHECK sy-subrc = 0.

    zcl_solicitacao_entrada_insumo=>saldo_solicitacao(
    EXPORTING
      i_nro_cg            = wa_tela_monta_carga-nro_cg
      i_nro_solicitacao   = wa_tela_monta_carga-nro_sol
      i_itens  = abap_true
      i_tp_saldo = 'G'
    IMPORTING
      e_saldo = lt_saldo ).

    MOVE-CORRESPONDING lt_saldo TO lt_saldo_2.

    s_mapping-fldname     = 'NRO_SOL'.
    s_mapping-dyfldname   = 'NRO_SOL'.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'SEQ'.
    s_mapping-dyfldname   = 'SEQ'.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'TP_SALDO'.
    s_mapping-dyfldname   = 'TP_SALDO'.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        ddic_structure  = 'ZSDE_SALDO_SOLIC_SH'
        retfield        = 'NRO_SOLIC'
        value_org       = 'S'
      TABLES
        value_tab       = lt_saldo_2
        return_tab      = lt_rettab
        dynpfld_mapping = t_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF lt_rettab IS NOT INITIAL.

      READ TABLE t_sub_itens ASSIGNING FIELD-SYMBOL(<fs_sub_itens>) INDEX es_row_no-row_id.
      IF sy-subrc IS INITIAL.

        READ TABLE lt_rettab ASSIGNING FIELD-SYMBOL(<fs_rettab>) WITH KEY fieldname = 'NRO_SOLIC'.
        IF sy-subrc IS INITIAL.

          CLEAR lw_modi.
          lw_modi-row_id    = es_row_no-row_id.
          lw_modi-fieldname = e_fieldname.
          lw_modi-value     = <fs_rettab>-fieldval.
          APPEND lw_modi TO <lfst_modi>.

        ENDIF.

        READ TABLE lt_rettab ASSIGNING <fs_rettab> WITH KEY fieldname = 'SEQ'.
        IF sy-subrc IS INITIAL.
          <fs_sub_itens>-item_solic    = <fs_rettab>-fieldval.
        ENDIF.

        READ TABLE lt_rettab ASSIGNING <fs_rettab> WITH KEY fieldname = 'TP_SALDO'.
        IF sy-subrc IS INITIAL.
          <fs_sub_itens>-tp_saldo_vinc = <fs_rettab>-fieldval.
        ENDIF.

        er_event_data->m_event_handled = 'X'.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
CLASS lcl_notas_venda_7200 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
*
    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

CLASS lcl_notas_transf_7200 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
*
    CLASS-METHODS on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----


CLASS lcl_bordero_7200 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
*
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_carga_notas DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
*
    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_carga_notas IMPLEMENTATION.

  METHOD set_toolbar.

    DATA: wl_toolbar         TYPE stb_button.

    IF g_grid_chaves IS BOUND.

      IF g_grid_chaves->is_ready_for_input( ) EQ 1.

        READ TABLE e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>)
        WITH KEY function = '&LOCAL&DELETE_ROW'.
        IF sy-subrc IS INITIAL.
          <fs_toolbar>-function = 'DELETE_ROW'.
        ENDIF.

      ELSE.

        wl_toolbar-function = 'PDF_NFE'.
        wl_toolbar-icon     = icon_base_planning_object.
        wl_toolbar-text      = 'PDF NF-e'.
        wl_toolbar-quickinfo = 'PDF NF-e'.
        APPEND wl_toolbar TO e_object->mt_toolbar.

      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'DELETE_ROW'.
        PERFORM f_elimina_linhas_nota.
      WHEN 'PDF_NFE'.
        PERFORM f_pdf_nfe_venda_5900.
    ENDCASE.
  ENDMETHOD.

  METHOD on_data_changed.

  ENDMETHOD.
ENDCLASS.
*** Fim - Rubenilson Pereira - 22.04.25 #169642

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_carga_notas2 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
*
    CLASS-METHODS on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_carga_notas2 IMPLEMENTATION.

  METHOD set_toolbar.

    DATA: wl_toolbar         TYPE stb_button.

    IF g_grid_notas IS BOUND.

      IF g_grid_notas->is_ready_for_input( ) EQ 1.

        READ TABLE e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>)
        WITH KEY function = '&LOCAL&DELETE_ROW'.
        IF sy-subrc IS INITIAL.
          <fs_toolbar>-function = 'DELETE_ROW2'.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'DELETE_ROW2'.
        PERFORM f_elimina_linhas_nota2.
    ENDCASE.
  ENDMETHOD.

  METHOD on_data_changed.

  ENDMETHOD.

ENDCLASS.

DATA: zeditor     TYPE REF TO lcl_simple_text_editor,
      t_text      TYPE catsxt_longtext_itab,
      t_observ    TYPE TABLE OF tline,
      w_text      TYPE textline,
      w_observ    TYPE tline,
      w_header    TYPE thead,
      l_id        TYPE thead-tdid,
      l_object    TYPE thead-tdobject,
      l_name_text TYPE thead-tdname.
*-CS2019001891 - JT - 27.01.2021 - inicio
*--------------------------------------------------------------------

CONSTANTS:
  BEGIN OF c_abas,
    tab1 LIKE sy-ucomm VALUE 'ABAS_F1',
    tab2 LIKE sy-ucomm VALUE 'ABAS_F2',
  END OF c_abas.

CONTROLS: abas TYPE TABSTRIP.

DATA:
  BEGIN OF g_abas,
    subscreen   LIKE sy-dynnr,
    prog        LIKE sy-repid VALUE 'ZSDR0060',
    pressed_tab LIKE sy-ucomm VALUE c_abas-tab1,
  END OF g_abas.

DATA: gv_erro TYPE flag.

*=============================================================================*
*SELECTION-SCREEN                                                             *
*=============================================================================*

*** Inicio - Rubenilson Pereira - 11.04.25 #169642
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-205.

  PARAMETERS: rb_entr TYPE c RADIOBUTTON GROUP g4 DEFAULT 'X' USER-COMMAND entr,
              rb_said TYPE c RADIOBUTTON GROUP g4.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_spart2 RADIOBUTTON GROUP a1 DEFAULT 'X' USER-COMMAND abc,
              p_spart3 RADIOBUTTON GROUP a1,
              p_spart4 RADIOBUTTON GROUP a1.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-206.

  PARAMETERS: rb_mont TYPE c RADIOBUTTON GROUP g5 DEFAULT 'X' USER-COMMAND mont MODIF ID mt,
              rb_list TYPE c RADIOBUTTON GROUP g5 MODIF ID mt.

SELECTION-SCREEN END OF BLOCK b2.
*** Fim - Rubenilson Pereira - 11.04.25 #169642

SELECTION-SCREEN BEGIN OF BLOCK 02 WITH FRAME TITLE TEXT-062.

*-CS2019001891 - 28.06.2021 - JT - inicio
  SELECT-OPTIONS: x_vkorg    FOR zsdt0282-vkorg      NO INTERVALS NO-EXTENSION    MODIF ID x2.  "Org. de Vendas VKORG
  PARAMETERS    : x_transp  TYPE zsdt0282-transp AS LISTBOX VISIBLE LENGTH 35 MODIF ID x2.
  SELECT-OPTIONS: x_vkbur    FOR tvbur-vkbur         MODIF ID x3,                            "Esc. de Vendas VKBUR
                  x_inco1    FOR tinc-inco1          NO INTERVALS MODIF ID x2,               "Incoterms INCO1
                  x_kunnr    FOR kna1-kunnr          NO INTERVALS MODIF ID x2,               "Cliente KUNNR
                  x_nrsol    FOR zsdt0129-nro_lote   NO INTERVALS MODIF ID x2,               "Nr. Solicitação NRO_LOTE
                  x_datas    FOR zsdt0129-data_atual MODIF ID x2,                            "Data Solicitação DATA_ATUAL
                  x_ovcor    FOR zsdt0082-vbeln      MODIF ID x2.                            "Documento de vendas
  PARAMETERS:     x_spart   TYPE tspa-spart          MODIF ID x2 NO-DISPLAY.                "Setor de Atividade TSPA

*PARAMETERS: p_fcorp RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND def MODIF ID 02,
*            p_ftpng RADIOBUTTON GROUP g1 MODIF ID 02,
*            p_ftroo RADIOBUTTON GROUP g1 MODIF ID 02.
SELECTION-SCREEN END OF BLOCK 02.
*-CS2019001891 - 28.06.2021 - JT - fim

SELECTION-SCREEN BEGIN OF BLOCK 04 WITH FRAME TITLE TEXT-063.
  PARAMETERS: p_filial RADIOBUTTON GROUP g3 DEFAULT 'X' USER-COMMAND jkl MODIF ID 04,
              p_corplt RADIOBUTTON GROUP g3 MODIF ID 04,
              p_corpcg RADIOBUTTON GROUP g3 MODIF ID 04,
              p_corppt RADIOBUTTON GROUP g3 MODIF ID 04,
** Inicio - Rubenilson - 13.08.24 - 147916
              p_logcor RADIOBUTTON GROUP g3 MODIF ID 04,
              p_transp RADIOBUTTON GROUP g3 MODIF ID 04.
** Fim - Rubenilson - 13.08.24 - 147916
SELECTION-SCREEN END OF BLOCK 04.
"Filtros
*-CS2019001891 - 28.06.2021 - JT - inicio
"Corporativo SPART 02
*SELECTION-SCREEN BEGIN OF BLOCK 21 WITH FRAME TITLE text-003.
*SELECT-OPTIONS: r_vkorg FOR tvko-vkorg          NO INTERVALS NO-EXTENSION MODIF ID c2,  "Org. de Vendas VKORG
*                r_vkbur FOR tvbur-vkbur         MODIF ID c2,                            "Esc. de Vendas VKBUR
*                r_inco1 FOR tinc-inco1          NO INTERVALS MODIF ID c2,               "Incoterms INCO1
*                r_kunnr FOR kna1-kunnr          NO INTERVALS MODIF ID c2,               "Cliente KUNNR
*                r_nrsol FOR zsdt0129-nro_lote   NO INTERVALS MODIF ID c2,               "Nr. Solicitação NRO_LOTE
*                r_datas FOR zsdt0129-data_atual MODIF ID c2,                            "Data Solicitação DATA_ATUAL
*                r_ovcor FOR zsdt0082-vbeln      MODIF ID c2.                            "Documento de vendas
*PARAMETERS:     r_spart TYPE tspa-spart          MODIF ID c2 NO-DISPLAY.                "Setor de Atividade TSPA
*SELECTION-SCREEN END OF BLOCK 21.
*"Transp. PNG SPART 02
*SELECTION-SCREEN BEGIN OF BLOCK 22 WITH FRAME TITLE text-072.
*SELECT-OPTIONS: g_vkorg FOR tvko-vkorg          NO INTERVALS NO-EXTENSION MODIF ID t2,  "Org. de Vendas VKORG
*                g_inco1 FOR tinc-inco1          NO INTERVALS MODIF ID t2,               "Incoterms INCO1
*                g_kunnr FOR kna1-kunnr          NO INTERVALS MODIF ID t2,               "Cliente KUNNR
*                g_nrsol FOR zsdt0129-nro_lote   NO INTERVALS MODIF ID t2,               "Nr. Solicitação NRO_LOTE
*                g_datas FOR zsdt0129-data_atual MODIF ID t2,                            "Data Solicitação DATA_ATUAL
*                g_ovcor FOR zsdt0082-vbeln      MODIF ID t2.                            "Documento de vendas
*PARAMETERS:     g_spart TYPE tspa-spart          MODIF ID t2 NO-DISPLAY.                "Setor de Atividade TSPA
*SELECTION-SCREEN END OF BLOCK 22.
*"Transp. ROO SPART 02
*SELECTION-SCREEN BEGIN OF BLOCK 23 WITH FRAME TITLE text-073.
*SELECT-OPTIONS: o_vkorg FOR tvko-vkorg          NO INTERVALS NO-EXTENSION MODIF ID t3,  "Org. de Vendas VKORG
*                o_inco1 FOR tinc-inco1          NO INTERVALS MODIF ID t3,               "Incoterms INCO1
*                o_kunnr FOR kna1-kunnr          NO INTERVALS MODIF ID t3,               "Cliente KUNNR
*                o_nrsol FOR zsdt0129-nro_lote   NO INTERVALS MODIF ID t3,               "Nr. Solicitação NRO_LOTE
*                o_datas FOR zsdt0129-data_atual MODIF ID t3,                            "Data Solicitação DATA_ATUAL
*                o_ovcor FOR zsdt0082-vbeln      MODIF ID t3.                            "Documento de vendas
*PARAMETERS:     o_spart TYPE tspa-spart         MODIF ID t3 NO-DISPLAY.                "Setor de Atividade TSPA
*SELECTION-SCREEN END OF BLOCK 23.
*-CS2019001891 - 28.06.2021 - JT - fim

"Corporativo SPART 03
SELECTION-SCREEN BEGIN OF BLOCK 31 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: d_vkorg FOR tvko-vkorg          NO INTERVALS NO-EXTENSION MODIF ID d3,  "Org. de Vendas VKORG
                  d_vkbur FOR tvbur-vkbur         MODIF ID d3,                            "Esc. de Vendas VKBUR
                  d_inco1 FOR tinc-inco1          NO INTERVALS MODIF ID d3,               "Incoterms INCO1
                  d_kunnr FOR kna1-kunnr          NO INTERVALS MODIF ID d3,               "Cliente KUNNR
                  d_numcg FOR zsdt0129-nro_lote   NO INTERVALS MODIF ID d3,               "Nr. Carga NRO_LOTE
                  d_datab FOR zsdt0129-data_atual MODIF ID d3,                            "Data Carga DATA_ATUAL
                  d_ovcor FOR zsdt0082-vbeln      MODIF ID d3.                            "Documento de vendas
  PARAMETERS:     d_spart TYPE tspa-spart         NO-DISPLAY MODIF ID d3.                 "Setor de Atividade TSPA
SELECTION-SCREEN END OF BLOCK 31.

"Filial SPART 04
SELECTION-SCREEN BEGIN OF BLOCK 41 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: p_vkorg FOR tvko-vkorg          NO INTERVALS NO-EXTENSION MODIF ID f4,  "Org. de Vendas VKORG
                  p_vkbur FOR tvbur-vkbur         MODIF ID f4,                            "Esc. de Vendas VKBUR
                  p_inco1 FOR tinc-inco1          NO INTERVALS NO-EXTENSION MODIF ID f4,  "Incoterms INCO1
                  p_kunnr FOR kna1-kunnr          NO INTERVALS MODIF ID f4,               "Cliente KUNNR
                  p_nlote FOR zsdt0129-nro_lote   NO INTERVALS MODIF ID f4,               "Nr. Lote NRO_LOTE
                  p_dataa FOR zsdt0129-data_atual MODIF ID f4,                            "Data Lote DATA_ATUAL
                  p_ovcor FOR zsdt0082-vbeln      MODIF ID f4.                            "Documento de vendas
  PARAMETERS:     p_spart TYPE tspa-spart          NO-DISPLAY MODIF ID f4.                "Setor de Atividade TSPA
SELECTION-SCREEN END OF BLOCK 41.
"Corporativo SPART 04
SELECTION-SCREEN BEGIN OF BLOCK 42 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: c_vkorg FOR tvko-vkorg          NO INTERVALS NO-EXTENSION MODIF ID c4,  "Org. de Vendas VKORG
                  c_inco1 FOR tinc-inco1          NO INTERVALS NO-EXTENSION MODIF ID c4,  "Incoterms INCO1
                  c_kunnr FOR kna1-kunnr          NO INTERVALS NO-EXTENSION MODIF ID c4,  "Cliente KUNNR
                  c_vkbur FOR tvbur-vkbur         MODIF ID c_l,                           "Esc. de Vendas VKBUR
                  c_nlote FOR zsdt0129-nro_lote   NO INTERVALS MODIF ID c_l,              "Nr. Lote NRO_LOTE
                  c_dataa FOR zsdt0129-data_atual MODIF ID c_l,                           "Data Lote DATA_ATUAL
                  c_numcg FOR zsdt0133-nro_cg     NO INTERVALS MODIF ID c_c,              "Nr. Carga NRO_CG
                  c_datab FOR zsdt0133-data_atual MODIF ID c_c,                           "Data Carga DATA_ATUAL
                  c_ovcor FOR zsdt0082-vbeln      MODIF ID c4.                           "Documento de vendas
  PARAMETERS:     c_spart TYPE tspa-spart         NO-DISPLAY MODIF ID c4.                 "Setor de Atividade TSPA
SELECTION-SCREEN END OF BLOCK 42.
"Logística Corp SPART 04
SELECTION-SCREEN BEGIN OF BLOCK 43 WITH FRAME TITLE TEXT-038.
  SELECT-OPTIONS: l_vkorg FOR tvko-vkorg          NO INTERVALS NO-EXTENSION MODIF ID l4,  "Org. de Vendas VKORG
                  l_inco1 FOR tinc-inco1          NO INTERVALS MODIF ID l4,               "Incoterms INCO1
                  l_numcg FOR zsdt0133-nro_cg     NO INTERVALS MODIF ID l4,               "Nr. Carga NRO_CG
                  l_datab FOR zsdt0133-data_atual MODIF ID l4,                            "Data Carga DATA_ATUAL
                  l_ovcor FOR zsdt0082-vbeln      MODIF ID l4.                            "Documento de vendas
  PARAMETERS:     l_spart TYPE tspa-spart          NO-DISPLAY MODIF ID l4.                "Setor de Atividade TSPA
SELECTION-SCREEN END OF BLOCK 43.
"Transportadora SPART 04
SELECTION-SCREEN BEGIN OF BLOCK 44 WITH FRAME TITLE TEXT-039.
  SELECT-OPTIONS: t_vkorg FOR tvko-vkorg          NO INTERVALS NO-EXTENSION MODIF ID t4,  "Org. de Vendas VKORG
                  t_inco1 FOR tinc-inco1          NO INTERVALS MODIF ID t4,               "Incoterms INCO1
                  t_kunnr FOR kna1-kunnr          NO INTERVALS NO-EXTENSION MODIF ID t4,  "Cliente
                  t_numcg FOR zsdt0133-nro_cg     NO INTERVALS MODIF ID t4,               "Nr. Carga NRO_CG
                  t_datab FOR zsdt0133-data_atual MODIF ID t4,                            "Data Carga DATA_ATUAL
                  t_ovcor FOR zsdt0082-vbeln      MODIF ID t4.                            "Documento de vendas
  PARAMETERS:     t_spart TYPE tspa-spart         NO-DISPLAY MODIF ID t4.                 "Setor de Atividade TSPA
SELECTION-SCREEN END OF BLOCK 44.

*** Inicio - Rubenilson Pereira - 11.04.25 #169642
" Entrada Carga
SELECTION-SCREEN BEGIN OF BLOCK 45 WITH FRAME TITLE TEXT-207.

  SELECT-OPTIONS: e_bukrs FOR zmmt0201-bukrs NO INTERVALS MODIF ID crg,
                  e_dtsol FOR zmmt0196-date_create MODIF ID crg,
                  e_nrsol FOR zmmt0196-nro_sol MODIF ID crg,
                  e_pedid FOR zmmt0196-ebeln NO INTERVALS MODIF ID crg,
                  e_dtcar FOR zmmt0201-date_create MODIF ID crg,
                  e_carga FOR zmmt0201-nro_cg MODIF ID crg,
                  e_stat  FOR zsde_alv_lista_carga-id_status_carga MODIF ID crg,
                  e_idvgm FOR zmmt0201-viagem_id MODIF ID crg.

SELECTION-SCREEN END OF BLOCK 45.
*** Fim - Rubenilson Pereira - 11.04.25 #169642

"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
SELECTION-SCREEN BEGIN OF BLOCK 46 WITH FRAME TITLE TEXT-207.
  SELECT-OPTIONS: s_vkorg FOR tvko-vkorg           NO INTERVALS NO-EXTENSION MODIF ID j4,  "Org. de Vendas VKORG
                  s_vkbur FOR tvbur-vkbur          MODIF ID j4,                            "Esc. de Vendas VKBUR
                  s_inco1 FOR tinc-inco1           NO INTERVALS NO-EXTENSION MODIF ID j4,  "Incoterms INCO1
                  s_kunnr FOR kna1-kunnr           NO INTERVALS MODIF ID j4,               "Cliente KUNNR
                  s_dtlsol FOR zsdt0082-dt_sol      MODIF ID j4,                            "Data Liberação Solicitação
                  s_nrsol FOR zsdt0082-nro_sol     MODIF ID j4,                            "Nro.Solicitação
                  s_dtcar FOR zmmt0201-date_create MODIF ID j4,                            "Data Carga
                  s_carga FOR zmmt0201-nro_cg      MODIF ID j4,                            "Numero Carga
                  s_ordem FOR zsdt0082-vbeln       MODIF ID j4,                            "Documento de vendas
                  s_idvgm FOR zmmt0201-viagem_id   MODIF ID j4,                            "Viagem Carguero
                  s_id_sf FOR zsdt0133-id_carga_safra_control   MODIF ID j4.                            "Viagem Carguero
SELECTION-SCREEN END OF BLOCK 46.
"SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

*=============================================================================*
*AT SELECTION-SCREEN                                                          *
*=============================================================================*
AT SELECTION-SCREEN OUTPUT.

  PERFORM f_select_screen_output.

*-CS2019001891 - 28.06.2021 - JT - inicio
****************************************************************
*At Selection Screen
****************************************************************
AT SELECTION-SCREEN." ON x_vkorg.
  PERFORM trata_transportadora.

****************************************************************
*At Selection Screen X_TRANSP
****************************************************************
AT SELECTION-SCREEN ON x_transp.
  FREE: g_filial_resp.

* IF p_spart2 = abap_true AND x_transp IS INITIAL.
*   MESSAGE text-170 TYPE 'S' DISPLAY LIKE 'E'.
*   STOP.
* ENDIF.

  w_values-fieldname = 'X_TRANSP'.
  APPEND w_values   TO t_values.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-cprog
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_values.

  READ TABLE t_values INTO w_values INDEX 1.
  IF sy-subrc = 0 AND w_values-fieldvalue IS NOT INITIAL.
    READ TABLE t_list INTO w_list WITH KEY key = w_values-fieldvalue.
    IF sy-subrc = 0.
      g_filial_resp = w_list-key.
    ENDIF.
  ENDIF.

****************************************************************
* initialization
****************************************************************
INITIALIZATION.

  SELECT *
    FROM zsdt0282
    INTO TABLE t_zsdt0282_tot.
*-CS2019001891 - 28.06.2021 - JT - fim

*=============================================================================*
*SELEÇÃO                                                                      *
*=============================================================================*
START-OF-SELECTION.

  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
  DATA(_sementes_new) = abap_false.
  SELECT SINGLE low
    FROM tvarvc INTO @_sementes_new
    WHERE name EQ 'ZSDT0112_SEMENTES_NEW'.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----


  CASE abap_true.
    WHEN rb_entr.

      PERFORM f_limpa_variaveis.

      IF e_bukrs IS INITIAL.
        MESSAGE 'Campo empresa é obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CASE abap_true.
        WHEN p_spart2.
          gv_spart = '02'.
        WHEN p_spart3.
          gv_spart = '03'.
        WHEN p_spart4.
          gv_spart = '04'.
        WHEN OTHERS.
      ENDCASE.

      IF rb_mont IS NOT INITIAL.

        LOOP AT e_bukrs ASSIGNING FIELD-SYMBOL(<fs_empresa>).

          zcl_carga_entrada_insumos=>check_permissao_carga_core(
          EXPORTING
            i_atividade = '01'
            i_bukrs = <fs_empresa>-low
            i_spart = gv_spart
            RECEIVING
            r_msg_error = DATA(lv_erro) ).
          IF lv_erro IS NOT INITIAL.
            MESSAGE lv_erro TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

        ENDLOOP.

        zcl_carga_entrada_insumos=>busca_dados_montar_carga(
        EXPORTING
          i_dt_sol = e_dtsol[]
          i_empresa = e_bukrs[]
          i_nro_sol = e_nrsol[]
          i_pedido  = e_pedid[]
          i_segmento = gv_spart
        IMPORTING
          e_tabela_monta_carga = t_monta_carga
          e_msg_erro = DATA(lv_msg_erro)
          ).

        IF lv_msg_erro IS NOT INITIAL.
          MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        DELETE t_monta_carga WHERE saldo_a_solic IS INITIAL.

        IF t_monta_carga[] IS INITIAL.
          MESSAGE 'Nenhuma solicitação com Saldo!' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        PERFORM f_exibe_alv.

      ELSEIF rb_list IS NOT INITIAL.
        tabstrip2-activetab = 'PUSH1'.

        LOOP AT e_bukrs ASSIGNING <fs_empresa>.

          zcl_carga_entrada_insumos=>check_permissao_carga_core(
          EXPORTING
            i_atividade = '02'
            i_bukrs = <fs_empresa>-low
            i_spart = gv_spart
            RECEIVING
            r_msg_error = lv_erro ).
          IF lv_erro IS NOT INITIAL.
            MESSAGE lv_erro TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

        ENDLOOP.

        zcl_carga_entrada_insumos=>busca_dados_carga(
        EXPORTING
          i_carga = e_carga[]
          i_empresa = e_bukrs[]
          i_segmento = gv_spart
          i_id_viagem = e_idvgm[]
          i_dt_carga  = e_dtcar[]
          i_status    = e_stat[]
         IMPORTING
           e_cds_carga = t_lista_cargas
           e_cargas = t_alv_lista_cargas
           e_notas  = t_0203
           e_msg_erro = lv_msg_erro ).

        IF lv_msg_erro IS NOT INITIAL.
          MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        PERFORM f_exibe_alv_lista.

      ENDIF.

    WHEN rb_said.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      IF p_spart4 EQ abap_true AND _sementes_new EQ abap_true.
        PERFORM f_start_select_saida_spart_04.
      ELSE.
        w_rtc_proprio = abap_true.

        PERFORM check_parametros CHANGING gv_erro.

        IF gv_erro IS INITIAL.
          CALL SCREEN 5000.
        ENDIF.
      ENDIF.

*      w_rtc_proprio = abap_true.
*
*      PERFORM check_parametros CHANGING gv_erro.
*
*      IF gv_erro IS INITIAL.
*        CALL SCREEN 5000.
*      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    WHEN OTHERS.
  ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  CHECK_PARAMETROS
*&---------------------------------------------------------------------*
FORM trata_transportadora.

  FREE: w_values, t_values.
  FREE: t_list.

  CHECK x_vkorg[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0282
    INTO TABLE t_zsdt0282
   WHERE vkorg IN x_vkorg.

  LOOP AT t_zsdt0282 INTO w_zsdt0282.
    w_list-key     = w_zsdt0282-transp.
    w_list-text    = w_zsdt0282-nome.
    APPEND w_list TO t_list.
  ENDLOOP.

  READ TABLE t_list INTO w_list INDEX 1.
  IF sy-subrc = 0.
    IF x_transp IS INITIAL.
*     x_transp = w_list-key.
    ENDIF.
  ELSE.
    FREE x_transp.
  ENDIF.

  READ TABLE t_list INTO w_list WITH KEY key = x_transp.
  IF sy-subrc <> 0.
    FREE x_transp.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'X_TRANSP'
      values          = t_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_PARAMETROS
*&---------------------------------------------------------------------*
FORM check_parametros CHANGING c_erro.

  DATA: vl_visao    TYPE char1,
        it_zsdt0060 TYPE STANDARD TABLE OF zsdt0060,
        it_kna1     TYPE STANDARD TABLE OF kna1.

  FREE: w_zsdt0282_tot,
        g_filial_resp,
        g_nome_transp,
        g_tp_tela.

  IF p_spart2 IS NOT INITIAL.

*-CS2019001891 - 28.06.2021 - JT - inicio
    READ TABLE x_vkorg INDEX 1.
    READ TABLE t_zsdt0282_tot INTO w_zsdt0282_tot WITH KEY vkorg  = x_vkorg-low
                                                           transp = x_transp.

    vl_visao      = w_zsdt0282_tot-tp_visao.
    vg_subt_lote  = w_zsdt0282_tot-tp_tela.
    g_tp_tela     = w_zsdt0282_tot-tp_tela.
    g_filial_resp = w_zsdt0282_tot-transp.
    g_nome_transp = w_zsdt0282_tot-nome.
    x_spart       = '02'.

    CASE vg_subt_lote.
      WHEN '5420'.
        g_tabstrip-pressed_tab = c_tabstrip-tab4.
      WHEN '5520'.
        g_tabstrip-pressed_tab = c_tabstrip-tab5.
      WHEN '5620'.
        g_tabstrip-pressed_tab = c_tabstrip-tab6.
    ENDCASE.

    "Check para os parâmetros Obrigatórios
    IF x_vkorg IS INITIAL AND x_ovcor IS INITIAL.
      MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ELSEIF x_vkbur IS INITIAL AND x_ovcor IS INITIAL AND g_tp_tela = '5420'.
      MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    IF x_transp IS INITIAL.
      MESSAGE TEXT-170 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

*   IF p_fcorp IS NOT INITIAL.
*     vl_visao = 'C'.
*     vg_subt_lote = '5420'.
*     g_tabstrip-pressed_tab = c_tabstrip-tab4.
*     r_spart = '02'.
*     "Check para os parâmetros Obrigatórios
*     IF r_vkorg IS INITIAL AND r_ovcor IS INITIAL.
*       MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
*       STOP.
*     ELSEIF r_vkbur IS INITIAL AND r_ovcor IS INITIAL.
*       MESSAGE text-005 TYPE 'S' DISPLAY LIKE 'E'.
*       STOP.
*     ENDIF.
*   ELSEIF p_ftpng IS NOT INITIAL.
*     vl_visao = 'P'.
*     vg_subt_lote = '5520'.
*     g_tabstrip-pressed_tab = c_tabstrip-tab5.
*     g_spart = '02'.
*     IF g_vkorg IS INITIAL AND g_ovcor IS INITIAL.
*       MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
*       STOP.
*     ENDIF.
*   ELSEIF p_ftroo IS NOT INITIAL.
*     vl_visao = 'R'.
*     vg_subt_lote = '5620'.
*     g_tabstrip-pressed_tab = c_tabstrip-tab6.
*     o_spart = '02'.
*     IF o_vkorg IS INITIAL AND o_ovcor IS INITIAL.
*       MESSAGE text-004 TYPE 'S' DISPLAY LIKE 'E'.
*       STOP.
*     ENDIF.
*   ENDIF.
*-CS2019001891 - 28.06.2021 - JT - fim

  ELSEIF p_spart3 IS NOT INITIAL.
    vl_visao = 'C'.
    vg_subt_lote = '5720'.
    g_tabstrip-pressed_tab = c_tabstrip-tab7.
    d_spart = '03'.
    "Check para os parâmetros Obrigatórios
    IF d_vkorg IS INITIAL AND d_ovcor IS INITIAL..
      MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ELSEIF d_vkbur IS INITIAL AND d_ovcor IS INITIAL..
      MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ELSEIF p_spart4 IS NOT INITIAL.
    IF p_filial IS NOT INITIAL.
      vl_visao = 'F'.
      vg_subt_lote = '5120'.
      g_tabstrip-pressed_tab = c_tabstrip-tab1.
      p_spart = '04'.
      "Check para os parâmetros Obrigatórios
      IF p_vkorg IS INITIAL AND p_ovcor IS INITIAL..
        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ELSEIF p_vkbur IS INITIAL AND p_ovcor IS INITIAL.
        MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ELSEIF p_inco1 IS INITIAL.
        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ELSEIF p_corplt IS NOT INITIAL.
      vl_visao = 'C'.
      vg_subt_lote = '5120'.
      g_tabstrip-pressed_tab = c_tabstrip-tab1.
      c_spart = '04'.
      "Check para os parâmetros Obrigátórios
      IF c_vkorg IS INITIAL AND c_ovcor IS INITIAL.
        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ELSEIF c_inco1 IS INITIAL.
        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ELSEIF p_corpcg IS NOT INITIAL.
      vl_visao = 'C'.
      vg_subt_lote = '5230'.
      g_tabstrip-pressed_tab = c_tabstrip-tab2.
      c_spart = '04'.
      "Check para os parâmetros Obrigátórios
      IF c_vkorg IS INITIAL AND c_ovcor IS INITIAL.
        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ELSEIF c_inco1 IS INITIAL.
        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ELSEIF p_corppt IS NOT INITIAL.
      vl_visao = 'C'.
      vg_subt_lote = '5320'.
      g_tabstrip-pressed_tab = c_tabstrip-tab3.
      c_spart = '04'.
      "Check para os parâmetros Obrigátórios
      IF c_vkorg IS INITIAL AND c_ovcor IS INITIAL.
        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ELSEIF c_inco1 IS INITIAL.
        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ELSEIF p_logcor IS NOT INITIAL.
      vl_visao = 'L'.
      vg_subt_lote = '5220'.
      g_tabstrip-pressed_tab = c_tabstrip-tab2.
      l_spart = '04'.
      "Check para os parâmetros Obrigátórios
      IF l_vkorg IS INITIAL AND l_ovcor IS INITIAL.
        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ELSEIF l_datab IS INITIAL.
        MESSAGE TEXT-040 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ELSEIF p_transp IS NOT INITIAL.
      vl_visao = 'T'.
      vg_subt_lote = '5230'.
      g_tabstrip-pressed_tab = c_tabstrip-tab2.
      t_spart = '04'.
      "Check para os parâmetros Obrigátórios
      IF t_vkorg IS INITIAL AND t_ovcor IS INITIAL.
        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ELSEIF t_inco1 IS INITIAL.
        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

  "Check de permissão de visão
  AUTHORITY-CHECK OBJECT 'ZSDT0112'
  ID 'Z_TP_VISAO' FIELD vl_visao.

  IF sy-subrc <> 0.
    MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ELSE.
    IF p_spart2 IS NOT INITIAL.
*-CS2019001891 - 28.06.2021 - JT - inicio
*     IF p_fcorp IS NOT INITIAL.
*       IF vl_visao NE 'C'.
*         MESSAGE text-008 TYPE 'S' DISPLAY LIKE 'E'.
*         STOP.
*       ENDIF.
*     ELSEIF p_ftpng IS NOT INITIAL.
*       IF vl_visao NE 'P'.
*         MESSAGE text-008 TYPE 'S' DISPLAY LIKE 'E'.
*         STOP.
*       ENDIF.
*     ELSEIF p_ftroo IS NOT INITIAL.
*       IF vl_visao NE 'R'.
*         MESSAGE text-008 TYPE 'S' DISPLAY LIKE 'E'.
*         STOP.
*       ENDIF.
*     ENDIF.
*-CS2019001891 - 28.06.2021 - JT - fim
    ELSEIF p_spart3 IS NOT INITIAL.
      IF vl_visao NE 'C'.
        MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ELSEIF p_spart4 IS NOT INITIAL.
      IF p_filial IS NOT INITIAL.
        IF vl_visao NE 'F'.
          MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
          STOP.
        ENDIF.
      ELSEIF p_corplt IS NOT INITIAL OR
             p_corpcg IS NOT INITIAL OR
             p_corppt IS NOT INITIAL.
        IF vl_visao NE 'C'.
          MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
          STOP.
        ENDIF.
      ELSEIF p_logcor IS NOT INITIAL.
        IF vl_visao NE 'L'.
          MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
          STOP.
        ENDIF.
      ELSEIF p_transp IS NOT INITIAL.
        IF vl_visao NE 'T'.
          MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
          STOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  "Check de Permissão de Esc. de Vendas
  IF p_filial IS NOT INITIAL.

    SELECT *
      FROM zsdt0060
      INTO TABLE it_zsdt0060
      WHERE usnam    EQ sy-uname
        AND programa EQ 'ZSDR016'
        AND vkbur    IN p_vkbur.

    IF it_zsdt0060 IS INITIAL.
      MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    IF p_inco1-low EQ '*'.
      MESSAGE TEXT-079 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ELSEIF p_corplt IS NOT INITIAL.

    IF c_inco1-low EQ '*'.
      MESSAGE TEXT-079 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    IF c_vkbur IS NOT INITIAL.

      SELECT *
            FROM zsdt0060
            INTO TABLE it_zsdt0060
            WHERE usnam    EQ sy-uname
              AND programa EQ 'ZSDR016'
              AND vkbur    IN c_vkbur.

      IF it_zsdt0060 IS INITIAL.
        MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.


    ENDIF.

  ELSEIF ( p_corpcg IS NOT INITIAL OR p_corppt IS NOT INITIAL ) AND
         ( c_vkbur IS NOT INITIAL ).

    SELECT *
      FROM zsdt0060
      INTO TABLE it_zsdt0060
      WHERE usnam    EQ sy-uname
        AND programa EQ 'ZSDR016'
        AND vkbur    IN c_vkbur.

    IF it_zsdt0060 IS INITIAL.
      MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ENDIF.

  "Check do Cliente
  IF p_filial IS NOT INITIAL AND p_kunnr IS NOT INITIAL.

    SELECT *
      FROM kna1
      INTO TABLE it_kna1
      WHERE kunnr IN p_kunnr.

    IF it_kna1 IS INITIAL.
      MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ELSEIF ( p_corplt IS NOT INITIAL OR p_corpcg IS NOT INITIAL OR p_corppt IS NOT INITIAL ) AND
         ( c_kunnr IS NOT INITIAL ).

    SELECT *
      FROM kna1
      INTO TABLE it_kna1
      WHERE kunnr IN c_kunnr.

    IF it_kna1 IS INITIAL.
      MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ELSEIF p_transp IS NOT INITIAL AND t_kunnr IS NOT INITIAL.

    SELECT *
      FROM kna1
      INTO TABLE it_kna1
      WHERE kunnr IN t_kunnr.

    IF it_kna1 IS INITIAL.
      MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ENDIF.

  DATA: lt_vbak TYPE TABLE OF ty_vbak_ck.
  DATA: ls_vbak TYPE ty_vbak_ck.

  DATA: lt_zsdt0060 TYPE TABLE OF zsdt0060.
  DATA: ls_zsdt0060 TYPE zsdt0060.

  DATA: lv_tabix TYPE sy-tabix.

  DATA: lt_tab TYPE esp1_message_tab_type.
  DATA: ls_tab TYPE esp1_message_wa_type.

*-CS2019001891 - 28.06.2021 - JT - inicio
  IF x_ovcor[] IS NOT INITIAL OR
*    r_ovcor[] IS NOT INITIAL OR
*    g_ovcor[] IS NOT INITIAL OR
*    o_ovcor[] IS NOT INITIAL OR
     d_ovcor[] IS NOT INITIAL OR
     p_ovcor[] IS NOT INITIAL OR
     c_ovcor[] IS NOT INITIAL OR
     l_ovcor[] IS NOT INITIAL OR
     t_ovcor[] IS NOT INITIAL.
    IF x_ovcor[] IS NOT INITIAL.
      SELECT vbeln vkbur INTO TABLE lt_vbak
        FROM vbak
        WHERE vbeln IN x_ovcor.
    ENDIF.
*   IF r_ovcor[] IS NOT INITIAL.
*     SELECT vbeln vkbur INTO TABLE lt_vbak
*       FROM vbak
*       WHERE vbeln IN r_ovcor.
*   ENDIF.
*   IF g_ovcor[] IS NOT INITIAL.
*     SELECT vbeln vkbur INTO TABLE lt_vbak
*       FROM vbak
*       WHERE  vbeln IN g_ovcor.
*   ENDIF.
*   IF o_ovcor[] IS NOT INITIAL.
*     SELECT vbeln vkbur INTO TABLE lt_vbak
*       FROM vbak
*       WHERE vbeln IN o_ovcor.
*   ENDIF.
    IF d_ovcor[] IS NOT INITIAL.
      SELECT vbeln vkbur INTO TABLE lt_vbak
        FROM vbak
        WHERE vbeln IN d_ovcor.
    ENDIF.
    IF p_ovcor[] IS NOT INITIAL.
      SELECT vbeln vkbur INTO TABLE lt_vbak
        FROM vbak
        WHERE vbeln IN p_ovcor.
    ENDIF.
    IF c_ovcor[] IS NOT INITIAL.
      SELECT vbeln vkbur INTO TABLE lt_vbak
        FROM vbak
        WHERE vbeln IN c_ovcor.
    ENDIF.
    IF l_ovcor[] IS NOT INITIAL.
      SELECT vbeln vkbur INTO TABLE lt_vbak
        FROM vbak
        WHERE vbeln IN l_ovcor.
    ENDIF.
    IF t_ovcor[] IS NOT INITIAL.
      SELECT vbeln vkbur INTO TABLE lt_vbak
        FROM vbak
        WHERE vbeln IN t_ovcor.
    ENDIF.
*-CS2019001891 - 28.06.2021 - JT - fim

    IF lt_vbak[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0060
        INTO TABLE it_zsdt0060
        FOR ALL ENTRIES IN lt_vbak
        WHERE usnam    EQ sy-uname
          AND programa EQ 'ZSDR016'
          AND vkbur    = lt_vbak-vkbur.

      SORT it_zsdt0060 BY vkbur.
      LOOP AT lt_vbak INTO ls_vbak.
        lv_tabix = sy-tabix.
        READ TABLE it_zsdt0060 TRANSPORTING NO FIELDS WITH KEY vkbur = ls_vbak-vkbur.
        IF sy-subrc EQ 0.
*-CS2019001891 - 28.06.2021 - JT - inicio
*         IF ( ( r_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN r_vkbur[] ) OR
          IF ( ( x_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN x_vkbur[] ) OR
*-CS2019001891 - 28.06.2021 - JT - inicio
             ( d_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN d_vkbur[] ) OR
             ( p_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN p_vkbur[] ) OR
             ( c_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN c_vkbur[] ) ) OR
*-CS2019001891 - 28.06.2021 - JT - inicio
            ( x_vkbur[] IS INITIAL AND
*           ( r_vkbur[] IS INITIAL AND
*-CS2019001891 - 28.06.2021 - JT - fim
            d_vkbur[] IS INITIAL AND
            p_vkbur[] IS INITIAL AND
            c_vkbur[] IS INITIAL ).
            ls_vbak-vbeln = '9999999999'.
            MODIFY lt_vbak FROM ls_vbak INDEX lv_tabix.
          ENDIF.
        ELSE.
*-CS2019001891 - 28.06.2021 - JT - inicio
*         IF ( r_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN r_vkbur[] ) OR
          IF ( x_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN x_vkbur[] ) OR
*-CS2019001891 - 28.06.2021 - JT - fim
             ( d_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN d_vkbur[] ) OR
             ( p_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN p_vkbur[] ) OR
             ( c_vkbur[] IS NOT INITIAL AND ls_vbak-vkbur IN c_vkbur[] ).
            ls_vbak-vbeln = '9999999999'.
            MODIFY lt_vbak FROM ls_vbak INDEX lv_tabix.
          ENDIF.

        ENDIF.
      ENDLOOP.
      DELETE lt_vbak WHERE vbeln = '9999999999'.
      IF lt_vbak[] IS NOT INITIAL.

        ls_tab-msgid  = 'Z_FI'.
        ls_tab-msgno  = '000'.
        ls_tab-msgty  = 'E'.

        ls_tab-msgv1  = 'Pedido de venda não pertence ao escritório selecionado!'.
        ls_tab-lineno = 1.
        APPEND ls_tab TO lt_tab.

        LOOP AT lt_vbak INTO ls_vbak.
          CONCATENATE ls_vbak-vbeln '-' ls_vbak-vkbur INTO ls_tab-msgv1 .
          ls_tab-lineno = ls_tab-lineno + 1.
          APPEND ls_tab TO lt_tab.
        ENDLOOP.

        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_tab.

        c_erro = 'X'.
      ENDIF.


    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_configure_select_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_select_screen_output.

  FREE: w_zsdt0282_tot.

  LOOP AT SCREEN.

    CASE abap_true.
      WHEN rb_entr."Entrada |=================================================================================================================

        CASE screen-group1.
          WHEN 'X2'  OR
               'X3'  OR
               'T4'  OR
               'l4'  OR
               'C4'  OR
               'L4'  OR
               'J4'  OR
               'T4'  OR
               'C_L' OR
               'C_C' OR
               'F4'  OR
               'D3'  OR
               '04'.

            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.

        ENDCASE.

        CASE abap_true.
          WHEN rb_mont. "Montar Carga

            IF screen-name CS 'E_DTCAR' OR
               screen-name CS 'E_CARGA' OR
               screen-name CS 'E_STAT'  OR
               screen-name CS 'E_IDVGM'.

              screen-invisible = '1'.
              screen-input = '0'.
              MODIFY SCREEN.
            ENDIF.

          WHEN rb_list. "Listar Carga

            IF screen-name CS 'E_DTSOL' OR
               screen-name CS 'E_PEDID'.

              screen-invisible = '1'.
              screen-input = '0'.
              MODIFY SCREEN.
            ENDIF.

        ENDCASE.

      WHEN rb_said."Saida   |=================================================================================================================

        IF screen-name(7) = 'X_VKORG'.
          PERFORM trata_transportadora.
        ENDIF.

        CASE abap_true.
          WHEN p_spart2. "Fertilizantes

            READ TABLE x_vkorg INDEX 1.
            READ TABLE t_zsdt0282_tot INTO w_zsdt0282_tot WITH KEY vkorg  = x_vkorg-low
                                                                   transp = x_transp.

            IF w_zsdt0282_tot-tp_tela = '5420' OR w_zsdt0282_tot-tp_tela IS INITIAL.
              IF screen-group1 = 'C4'  OR
                 screen-group1 = 'C_L' OR
                 screen-group1 = 'C_C' OR
                 screen-group1 = 'T4'  OR
                 screen-group1 = 'L4'  OR
                 screen-group1 = 'F4'  OR
                 screen-group1 = '04'  OR
                 screen-group1 = 'D3'  OR
                 screen-group1 = 'T2'  OR
                 screen-group1 = 'T3'.
                screen-active = '0'.
                MODIFY SCREEN.
                CLEAR: d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                       p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                       c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_L, C_C
                       l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "L4
                       t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4

                CONTINUE.
              ENDIF.
            ELSEIF w_zsdt0282_tot-tp_tela = '5520'.
              IF screen-group1 = 'C4'  OR
                 screen-group1 = 'C_L' OR
                 screen-group1 = 'C_C' OR
                 screen-group1 = 'T4'  OR
                 screen-group1 = 'L4'  OR
                 screen-group1 = 'F4'  OR
                 screen-group1 = '04'  OR
                 screen-group1 = 'X3'  OR
                 screen-group1 = 'D3'  OR
                 screen-group1 = 'C2'  OR
                 screen-group1 = 'T3'.
                screen-active = '0'.
                MODIFY SCREEN.
                CLEAR: d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                       p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                       c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
                       l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
                       t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
                CONTINUE.
              ENDIF.

            ELSEIF w_zsdt0282_tot-tp_tela = '5620'.
              IF screen-group1 = 'C4'  OR
                 screen-group1 = 'C_L' OR
                 screen-group1 = 'C_C' OR
                 screen-group1 = 'T4'  OR
                 screen-group1 = 'L4'  OR
                 screen-group1 = 'F4'  OR
                 screen-group1 = '04'  OR
                 screen-group1 = 'X3'  OR
                 screen-group1 = 'D3'  OR
                 screen-group1 = 'X3'  OR
                 screen-group1 = 'T2'  OR
                 screen-group1 = 'C2'.
                screen-active = '0'.
                MODIFY SCREEN.
                CLEAR: d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                       p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                       c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
                       l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
                       t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4

                CONTINUE.
              ENDIF.
            ENDIF.
          WHEN p_spart3. "Defensivos

            IF screen-group1 = 'C4'  OR
               screen-group1 = 'C_L' OR
               screen-group1 = 'C_C' OR
               screen-group1 = 'F4'  OR
               screen-group1 = 'T4'  OR
               screen-group1 = 'L4'  OR
               screen-group1 = '02'  OR
               screen-group1 = 'X2'  OR
               screen-group1 = '04'  OR
               screen-group1 = 'C2'  OR
               screen-group1 = 'X3'  OR
               screen-group1 = 'T2'  OR
               screen-group1 = 'T3'.
              screen-active = '0'.
              MODIFY SCREEN.
              CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
                     p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                     c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
                     l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
                     t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4

              CONTINUE.
            ENDIF.
          WHEN p_spart4. "Sementes

            IF p_filial EQ abap_true.
              IF screen-group1 = 'C4'  OR
                 screen-group1 = 'C_L' OR
                 screen-group1 = 'C_C' OR
                 screen-group1 = 'T4'  OR
                 screen-group1 = 'L4'  OR
                 screen-group1 = '02'  OR
                 screen-group1 = 'X2'  OR
                 screen-group1 = 'C2'  OR
                 screen-group1 = 'D3'  OR
                 screen-group1 = 'X3'  OR
                 screen-group1 = 'T2'  OR
                 screen-group1 = 'T3'.
                screen-active = '0'.
                MODIFY SCREEN.
                CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
                       d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                       c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
                       l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
                       t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
                CONTINUE.
              ENDIF.
            ELSEIF p_corplt EQ abap_true.
              IF screen-group1 = 'F4'  OR
                 screen-group1 = 'C_C' OR
                 screen-group1 = 'T4'  OR
                 screen-group1 = 'L4'  OR
                 screen-group1 = '02'  OR
                 screen-group1 = 'X2'  OR
                 screen-group1 = 'C2'  OR
                 screen-group1 = 'D3'  OR
                 screen-group1 = 'X3'  OR
                 screen-group1 = 'T2'  OR
                 screen-group1 = 'T3'.
                screen-active = '0'.
                MODIFY SCREEN.
                CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
                       d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                       p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                       c_numcg[], c_datab[], "C_C
                       l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
                       t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
                CONTINUE.
              ENDIF.
            ELSEIF p_corpcg EQ abap_true.
              IF screen-group1 = 'F4'  OR
                 screen-group1 = 'C_L' OR
                 screen-group1 = 'T4'  OR
                 screen-group1 = 'L4'  OR
                 screen-group1 = '02'  OR
                 screen-group1 = 'X2'  OR
                 screen-group1 = 'C2'  OR
                 screen-group1 = 'D3'  OR
                 screen-group1 = 'X3'  OR
                 screen-group1 = 'T2'  OR
                 screen-group1 = 'T3'.
                screen-active = '0'.
                MODIFY SCREEN.
                CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
                       d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                       p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                       c_vkbur[], c_nlote[], c_dataa[], " C_L
                       l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
                       t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
                CONTINUE.
              ENDIF.
            ELSEIF p_corppt EQ abap_true.
              IF screen-group1 = 'F4'  OR
                 screen-group1 = 'C_L' OR
                 screen-group1 = 'T4'  OR
                 screen-group1 = 'L4'  OR
                 screen-group1 = '02'  OR
                 screen-group1 = 'X2'  OR
                 screen-group1 = 'C2'  OR
                 screen-group1 = 'D3'  OR
                 screen-group1 = 'X3'  OR
                 screen-group1 = 'T2'  OR
                 screen-group1 = 'T3'.
                screen-active = '0'.
                MODIFY SCREEN.
                CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
                       d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                       p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                       c_vkbur[], c_nlote[], c_dataa[], " C_L
                       l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
                       t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
                CONTINUE.
              ENDIF.
            ELSEIF p_logcor EQ abap_true.
              IF screen-group1 = 'F4'  OR
                 screen-group1 = 'C_C' OR
                 screen-group1 = 'C_L' OR
                 screen-group1 = 'T4'  OR
                 screen-group1 = 'C4'  OR
                 screen-group1 = '02'  OR
                 screen-group1 = 'X2'  OR
                 screen-group1 = 'C2'  OR
                 screen-group1 = 'D3'  OR
                 screen-group1 = 'X3'  OR
                 screen-group1 = 'T2'  OR
                 screen-group1 = 'T3'.
                screen-active = '0'.
                MODIFY SCREEN.
                CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
                       d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                       p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                       c_vkorg[], c_inco1[], c_kunnr[], c_ovcor[], c_spart,  "C4
                       c_vkbur[], c_nlote[], c_dataa[], " C_L
                       c_numcg[], c_datab[], "C_C
                       t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
                CONTINUE.
              ENDIF.
            ELSEIF p_transp EQ abap_true.
              IF screen-group1 = 'F4'  OR
                 screen-group1 = 'C_C' OR
                 screen-group1 = 'C_L' OR
                 screen-group1 = 'L4'  OR
                 screen-group1 = 'C4'  OR
                 screen-group1 = '02'  OR
                 screen-group1 = 'X2'  OR
                 screen-group1 = 'C2'  OR
                 screen-group1 = 'D3'  OR
                 screen-group1 = 'X3'  OR
                 screen-group1 = 'T2'  OR
                 screen-group1 = 'T3'.
                screen-active = '0'.
                MODIFY SCREEN.
                CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
                       d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                       p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                       c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
                       l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart. "l4
                CONTINUE.
              ENDIF.
            ENDIF.
        ENDCASE.

        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        DATA(_sementes_new) = abap_false.
        SELECT SINGLE low
          FROM tvarvc INTO @_sementes_new
          WHERE name EQ 'ZSDT0112_SEMENTES_NEW'.

        IF _sementes_new EQ abap_false. "Temp Output

          IF screen-group1 EQ 'CRG' OR
             screen-group1 EQ 'J4' OR
             screen-group1 EQ 'MT'.
            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ENDIF.

          CONTINUE.
        ENDIF.
        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

        CASE screen-group1.
          WHEN 'MT'. "Montar e Listar Cargas

            CASE abap_true.
              WHEN p_spart2. "Fertilizantes

                screen-invisible = '1'.
                screen-input = '0'.
                MODIFY SCREEN.

              WHEN p_spart3. "Defensivos

                screen-invisible = '1'.
                screen-input = '0'.
                MODIFY SCREEN.

              WHEN p_spart4. "Sementes

            ENDCASE.

          WHEN 'J4'. "Filtros para Montar e Listar Carga Saida

            CASE abap_true.
              WHEN p_spart2. "Fertilizantes

                screen-invisible = '1'.
                screen-input = '0'.
                MODIFY SCREEN.

              WHEN p_spart3. "Defensivos

                screen-invisible = '1'.
                screen-input = '0'.
                MODIFY SCREEN.

              WHEN p_spart4. "Sementes

                CASE abap_true.
                  WHEN rb_mont. "Montar Carga

                    IF screen-name CS 'S_DTCAR' OR
                       screen-name CS 'S_CARGA' OR
                       screen-name CS 'S_IDVGM'.

                      screen-invisible = '1'.
                      screen-input = '0'.
                      MODIFY SCREEN.

                    ENDIF.

                  WHEN rb_list. "Listar Carga

                    IF screen-name CS 'S_DTSOL' OR
                       screen-name CS 's_ordem'.

                      screen-invisible = '1'.
                      screen-input = '0'.
                      MODIFY SCREEN.

                    ENDIF.

                ENDCASE.

            ENDCASE.

          WHEN 'F4'.

            CASE abap_true.
              WHEN p_spart2. "Fertilizantes


              WHEN p_spart3. "Defensivos


              WHEN p_spart4. "Sementes

                screen-invisible = '1'.
                screen-input = '0'.
                MODIFY SCREEN.

            ENDCASE.

          WHEN '04'.

            CASE abap_true.
              WHEN p_spart2. "Fertilizantes


              WHEN p_spart3. "Defensivos


              WHEN p_spart4. "Sementes

                screen-invisible = '1'.
                screen-input = '0'.
                MODIFY SCREEN.

            ENDCASE.

          WHEN 'CRG'.

            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.

        ENDCASE.

    ENDCASE.

  ENDLOOP.

ENDFORM.


INCLUDE ole2incl.

INCLUDE zsdr0060_forms.

INCLUDE zsdr0060_5000.

INCLUDE zsdr0060_5120.

INCLUDE zsdr0060_5130.

INCLUDE zsdr0060_5131.

INCLUDE zsdr0060_5140.

INCLUDE zsdr0060_5141.

INCLUDE zsdr0060_5121.

INCLUDE zsdr0060_5220.

INCLUDE zsdr0060_5230.

INCLUDE zsdr0060_5320.

INCLUDE zsdr0060_5221.

INCLUDE zsdr0060_5321.

INCLUDE zsdr0060_5420.

INCLUDE zsdr0060_5520.

INCLUDE zsdr0060_5620.

INCLUDE zsdr0060_5720.

INCLUDE zsdr0060_5730.

INCLUDE zsdr0060_5731.

INCLUDE zsdr0060_5740.

INCLUDE zsdr0060_5741.

INCLUDE zsdr0060_5820.

INCLUDE zsdr0060_5521.

INCLUDE zsdr0060_5821.

INCLUDE zsdr0060_5822 IF FOUND.  "*-CS2021000218-31.08.2022-#89492-JT-inicio

INCLUDE zsdr0060_5322.

INCLUDE zsdr0060_5522.

INCLUDE zsdr0060_6001.

INCLUDE zsdr0060_7100.

INCLUDE zsdr0060_5231.

INCLUDE zsdr0060_8000.

INCLUDE zsdr0060_5900.

INCLUDE zsdr0060_7200.

INCLUDE zsdr0060_7211.  "*-US192364-14.10.2025-#192364-JT
