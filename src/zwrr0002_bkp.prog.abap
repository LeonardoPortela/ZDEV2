*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 03/08/2010                                              &*
*& Descrição: Automatização Nota Fiscal Writer                        &*
*& Transação: ZNFW0001                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK917576   03.08.2010                            &*
*& Marcos Faneli   DEVK937099   29.04.2014                            &*
*& Sara Oikawa     DEVK9A0J9H   23.04.2020 [CS2019001879]             &*
*&--------------------------------------------------------------------&*

REPORT  zwrr0002_031121 MESSAGE-ID znfw.
TYPE-POOLS: vrm.
INCLUDE <icon>.
TABLES: zfiwrt0008,
        zsdt0075.
*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_fiscal,
         nftype         TYPE zfiwrt0001-nftype,
         itmtyp         TYPE zfiwrt0001-itmtyp,
         dias           TYPE zfiwrt0001-dias,
         retorno        TYPE zfiwrt0001-retorno,
         zpesagem       TYPE zfiwrt0001-zpesagem,
         imobilizado    TYPE zfiwrt0001-imobilizado,
         tp_mv_imob     TYPE zfiwrt0001-tp_mv_imob,
         docref         TYPE zfiwrt0008-docref,
         nr_romaneio    TYPE zfiwrt0008-nr_romaneio,
         move_plant     TYPE zfiwrt0008-move_plant,
         move_stloc     TYPE zfiwrt0008-move_stloc,
         ctrl_zrfl      TYPE zfiwrt0001-ctrl_zrfl,
         energia        TYPE zfiwrt0008-energia,
         disp_nf_cct    TYPE zfiwrt0008-disp_nf_cct,
         servico        TYPE zfiwrt0008-servico,
         transf_icms    TYPE zfiwrt0001-transf_icms,
         complemento    TYPE zfiwrt0008-complemento,
         inco1          TYPE j_1bdydoc-inco1,
         inco2          TYPE j_1bdydoc-inco2,
         aviso_rec      TYPE zfiwrt0001-aviso_rec,
         lm_estoque     TYPE zfiwrt0001-lm_estoque,
         ebeln          TYPE zfiwrt0008-ebeln,
         vbeln          TYPE vbap-vbeln,
         kostl          TYPE zfiwrt0008-kostl,
         referencia     TYPE zfiwrt0008-referencia,
         access_key(54), "  TYPE ZFIWRT0008-ACCESS_KEY,
         konto          TYPE zfiwrt0008-konto,
         move_mat       TYPE zfiwrt0008-move_mat,
         move_batch     TYPE zfiwrt0008-move_batch,
         bktxt          TYPE zfiwrt0008-bktxt,
         mtsnr          TYPE zfiwrt0008-mtsnr,
       END OF ty_fiscal,

       BEGIN OF ty_seq_lcto,
         seq_lcto TYPE zfiwrt0008-seq_lcto,
       END OF ty_seq_lcto  ,

       BEGIN OF ty_direitos,
         cfop     TYPE zfiwrt0006-cfop,
         taxlw1   TYPE zfiwrt0006-taxlw1,
         taxlw2   TYPE zfiwrt0006-taxlw2,
         taxlw4   TYPE zfiwrt0006-taxlw4,
         taxlw5   TYPE zfiwrt0006-taxlw5,
         indcoper TYPE zfiwrt0006-indcoper,
         opertyp  TYPE zfiwrt0006-opertyp,
         taxcode  TYPE zfiwrt0006-taxcode,
       END OF ty_direitos,

       BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END   OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor,

       BEGIN OF ty_aprov,
         nivel_aprov  TYPE zfiwrt0007-nivel_aprov,
         usnam        TYPE zfiwrt0007-usnam,
         nome(80),
         departamento TYPE zfiwrt0007-departamento,
       END OF ty_aprov,

       BEGIN OF ty_parc,
         parvw    TYPE zfiwrt0015-parvw,
         parid    TYPE zfiwrt0015-parid,
         nome(80),
         style    TYPE lvc_t_styl,
       END OF ty_parc,

       BEGIN OF ty_docs,
         docnum        TYPE zfiwrt0008-docnum,
         belnr         TYPE zfiwrt0008-belnr,
         mblnr         TYPE zfiwrt0008-mblnr,
         budat         TYPE zfiwrt0008-budat,
         bldat         TYPE zfiwrt0008-bldat,
         branch        TYPE zfiwrt0008-branch,
         nfenum        TYPE j_1bnfdoc-nfenum,
         series        TYPE j_1bnfdoc-series,
         tcode_org     TYPE zfiwrt0008-tcode_org,
         not_check_xml TYPE zfiwrt0008-not_check_xml,
         loc_carrega   TYPE zfiwrt0008-loc_carrega, "CS2020001418 - CSB
       END OF ty_docs,

       BEGIN OF ty_matnr_ov_pd,
         vbeln TYPE vbak-vbeln,
         posnr TYPE vbap-posnr,
         ebeln TYPE ekko-ebeln,
         matnr TYPE vbap-matnr,
         meins TYPE ekpo-meins,
         maktx TYPE makt-maktx,
         steuc TYPE marc-steuc,
         werks TYPE ekpo-werks,
         lgort TYPE ekpo-lgort,
         charg TYPE eket-charg,
       END OF ty_matnr_ov_pd,

       BEGIN OF ty_trans,
         lifnr      TYPE zfiwrt0019-lifnr,
         placa      TYPE zfiwrt0019-placa,
         anzpk      TYPE zfiwrt0019-anzpk,
         shpunt     TYPE zfiwrt0019-shpunt,
         ntgew      TYPE zfiwrt0019-ntgew,
         brgew      TYPE zfiwrt0019-brgew,
         ufplaca    TYPE zfiwrt0019-ufplaca,
         placa_car1 TYPE zfiwrt0019-placa_car1,
         placa_car2 TYPE zfiwrt0019-placa_car2,
         placa_car3 TYPE zfiwrt0019-placa_car3,
         motorista  TYPE zfiwrt0019-motorista,
       END OF ty_trans.

TYPES: BEGIN OF ty_doc_refs.
         INCLUDE STRUCTURE zfiwrt0020_alv.
TYPES:  END OF ty_doc_refs.

*-CS2021001266 - 15.12.2021 - JT- inicio
TYPES: BEGIN OF ty_regula,
         operacao TYPE zfiwrt0008-operacao,
         rate     TYPE zfiwrt0010-rate,
         shipto   TYPE lfa1-regio.
TYPES: END   OF ty_regula.

DATA: t_regula TYPE TABLE OF ty_regula,
      w_regula TYPE ty_regula.
*-CS2021001266 - 15.12.2021 - JT- fim

DATA: w_zfiwrt0001   TYPE zfiwrt0001.  "*-CS2023000043-09.02.2023-#102019-JT

DATA: manager        TYPE REF TO cl_gos_manager.
DATA: t_zfiwrt0032 TYPE STANDARD TABLE OF zfiwrt0032 INITIAL SIZE 0.

*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED,
            lcl_alv_toolbar2  DEFINITION DEFERRED.
*            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container           TYPE scrfname VALUE 'CC_ITENS_NOTA',
      g_custom_container    TYPE REF TO cl_gui_custom_container,
      container_1           TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2           TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter              TYPE REF TO cl_gui_splitter_container,
      grid1                 TYPE REF TO cl_gui_alv_grid,
      grid2                 TYPE REF TO cl_gui_alv_grid,
      grid4                 TYPE REF TO cl_gui_alv_grid,
      grid5                 TYPE REF TO cl_gui_alv_grid,
      obg_toolbar           TYPE REF TO lcl_alv_toolbar,
      obg_toolbar2          TYPE REF TO lcl_alv_toolbar2,
*      OBG_TOOLBAR3          TYPE REF TO LCL_ALV_TOOLBAR3,
      c_alv_toolbarmanager  TYPE REF TO cl_alv_grid_toolbar_manager,
      c_alv_toolbarmanager2 TYPE REF TO cl_alv_grid_toolbar_manager,
      c_alv_toolbarmanager3 TYPE REF TO cl_alv_grid_toolbar_manager,
      g_descbox             TYPE scrfname VALUE 'CC_DESC',
      g_custom_cont_desc    TYPE REF TO cl_gui_custom_container,
      obg_descbox           TYPE REF TO cl_gui_textedit,
      obg_docking           TYPE REF TO cl_gui_docking_container,
      grid3                 TYPE REF TO cl_gui_alv_grid,
      obg_conteiner_contab  TYPE REF TO cl_gui_custom_container,
      obg_conteiner_aprov   TYPE REF TO cl_gui_custom_container,
      obg_conteiner_parc    TYPE REF TO cl_gui_custom_container,
      g_cc_contab           TYPE scrfname VALUE 'CC_CONTAB',
      g_cc_aprov            TYPE scrfname VALUE 'CC_APROV',
      g_cc_parc             TYPE scrfname VALUE 'CC_PARC',
      wa_style              TYPE lvc_s_styl,
      style                 TYPE lvc_t_styl  WITH HEADER LINE,
      style2                TYPE lvc_t_styl WITH HEADER LINE.

*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.
*** TREE DE MENSAGENS.
DATA node_itab LIKE node_str OCCURS 0.
DATA node      LIKE node_str.

DATA container    TYPE REF TO cl_gui_custom_container.
DATA splitter_msg TYPE REF TO cl_gui_easy_splitter_container.
DATA right        TYPE REF TO cl_gui_container.
DATA left         TYPE REF TO cl_gui_container.

DATA editor TYPE REF TO cl_gui_textedit.
DATA tree   TYPE REF TO cl_gui_simple_tree.

DATA behaviour_left  TYPE REF TO cl_dragdrop.
DATA behaviour_right TYPE REF TO cl_dragdrop.

DATA handle_tree TYPE i.
DATA num_row     TYPE i VALUE 0.
DATA viniciou_lcto_znfw0009.
DATA: vlr_total_nota LIKE zib_nfe_dist_itm-prod_vlr_total_b,
      vlr_total_item LIKE zib_nfe_dist_itm-prod_vlr_total_b.
DATA vsugere_itens_znfw0009.
DATA vlancamento_znfw0009(3) TYPE c.

DATA: vl_prazo TYPE ze_prazo.

DATA: lit_zib_nfe_dist_itm TYPE TABLE OF zib_nfe_dist_itm.

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
           c_sp(2)           TYPE c VALUE 'SP',
           c_lr(2)           TYPE c VALUE 'LR',
           c_pc(2)           TYPE c VALUE 'PC',
           c_ne(2)           TYPE c VALUE 'NE',
           c_01(2)           TYPE c VALUE '01',
           c_30(2)           TYPE c VALUE '30',
           c_40(2)           TYPE c VALUE '40',
           c_43(2)           TYPE c VALUE '43', "RJF - Antonio solicitou voltar versão
           c_31(2)           TYPE c VALUE '31', "RJF - Antonio solicitou voltar versão
           c_50(4)           TYPE c VALUE '0050',
           c_76(2)           TYPE c VALUE '76',
           c_71(2)           TYPE c VALUE '71',
           c_72(2)           TYPE c VALUE '72',
           c_br(2)           TYPE c VALUE 'BR',
           c_lf(2)           TYPE c VALUE 'LF',
           "C_LR(2)           TYPE C VALUE 'LR',
           c_z1(2)           TYPE c VALUE 'Z1',
           c_add(3)          TYPE c VALUE 'ADD',
           c_del(3)          TYPE c VALUE 'DEL',
           c_dg1(3)          TYPE c VALUE 'DG1',
           c_dg2(3)          TYPE c VALUE 'DG2',
           c_dummy_header(3) TYPE c VALUE '099',
           c_dummy_itens(3)  TYPE c VALUE '098',
           c_icm3(4)         TYPE c VALUE 'ICM3',
           c_ipis(4)         TYPE c VALUE 'IPIS',
           c_icof(4)         TYPE c VALUE 'ICOF',
           c_ics1(4)         TYPE c VALUE 'ICS1',
           c_icop(4)         TYPE c VALUE 'ICOP',
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
           c_deldoc(6)       TYPE c VALUE 'DELDOC',
           c_dclick(6)       TYPE c VALUE 'DCLICK',
           c_search(6)       TYPE c VALUE 'SEARCH',
           c_atuali(6)       TYPE c VALUE 'ATUALI',
           c_add_msg(7)      TYPE c VALUE 'ADD_MSG',
           c_del_msg(7)      TYPE c VALUE 'DEL_MSG',
           c_gerar_nf(8)     TYPE c VALUE 'GERAR_NF',
           c_clos_msg(8)     TYPE c VALUE 'CLOS_MSG',
           c_save_msg(8)     TYPE c VALUE 'SAVE_MSG',
           c_show_msgre(10)  TYPE c VALUE 'SHOW_MSGRE',
           c_display(10)     TYPE c VALUE 'ZNFW0002_D',
           c_lim(3)          TYPE c VALUE 'LIM'.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: ok-code              TYPE sy-ucomm,
      p_seq_lcto           TYPE zfiwrt0008-seq_lcto,
      p_operacao           TYPE zfiwrt0001-operacao,
      p_operacao_old       TYPE zfiwrt0001-operacao,  "*-CS2023000043-09.02.2023-#102019-JT
      p_bukrs              TYPE zfiwrt0008-bukrs,
      p_branch             TYPE zfiwrt0008-branch,
      p_parvw              TYPE zfiwrt0008-parvw,
      p_parid              TYPE zfiwrt0008-parid,
      p_loekz(6)           TYPE c,
      wg_desc_operacao(30),
      wg_desc_branch(30),
      wg_desc_parvw(30),
      wg_desc_parid(30),
      wg_desc_bukrs(30),
      wg_op_fiscal(50),
      wg_desc_nftype       TYPE j_1baat-nfttxt,
      wg_desc_itmtyp       TYPE j_1bitemtypest-text,
      wg_desc_cfop         TYPE j_1bagnt-cfotxt,
      wg_desc_taxlw1       TYPE j_1batl1t-descrip,
      wg_desc_taxlw2       TYPE j_1batl2t-descrip,
      wg_desc_taxlw4       TYPE j_1batl4t-descrip,
      wg_desc_taxlw5       TYPE j_1batl5t-descrip,
      wg_desc_taxcode      TYPE j_1btxsdct-txt,
      wg_fiscal            TYPE ty_fiscal,
      wg_direitos          TYPE ty_direitos,
      wg_docs              TYPE ty_docs,
      wg_transporte        TYPE ty_trans,
      wg_titulo(50)        VALUE 'Dados de Cabeçalho',
      wg_titulo2(50)       VALUE 'Dados de Itens',
      vg_subscreen1        TYPE sy-dynnr VALUE '213',
      vg_subscreen2        TYPE sy-dynnr VALUE '214',
      wg_dg1(4)            VALUE c_minimizar,
      wg_dg2(4)            VALUE c_minimizar,
      wg_shipfrom          TYPE lfa1-regio,
      wg_shipto            TYPE lfa1-regio,
      wg_editor            TYPE ty_editor,
      tg_editor            TYPE TABLE OF ty_editor,
      tg_tbsl              TYPE TABLE OF tbsl WITH HEADER LINE,
      tg_fields            TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_selectedcell      TYPE lvc_t_cell,
      wg_selectedcell      TYPE lvc_s_cell,
      wg_dclknodekey       TYPE tv_nodekey,
      wg_sugere_transp     TYPE c,
      v_automatico_memo    TYPE char1,

      wg_acao(30),
      wg_flag,
      x_field(30),
      wg_mensagem(30),
      tg_aprov             TYPE TABLE OF ty_aprov WITH HEADER LINE,
      tg_parc              TYPE TABLE OF ty_parc WITH HEADER LINE,
      g_init_once,
      xbol(2),
      v_zlsch              TYPE zfiwrt0011-zlsch,
      it_seq_lcto          TYPE TABLE OF ty_seq_lcto WITH HEADER LINE,
      vg_mod_fieldcat      TYPE boolean.

DATA: t_fieldcatalog     TYPE lvc_t_fcat,
      t_fieldcatalog_get TYPE lvc_t_fcat,
      w_fieldcatalog     TYPE lvc_s_fcat,
      wa_layout          TYPE lvc_s_layo,
      wa_stable_itens    TYPE lvc_s_stbl,
      wa_stable          TYPE lvc_s_stbl.

FIELD-SYMBOLS: <fs_get> TYPE lvc_s_fcat.

**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

DATA: BEGIN OF tg_impo2 OCCURS 0,

        line     TYPE i,
        taxtyp   TYPE zfiwrt0010-taxtyp,
        ttypetxt TYPE j_1bttytxt,
        taxgrp   TYPE j_1btaxgrp,
        base     TYPE zfiwrt0010-base,
        rate     TYPE zfiwrt0010-rate,
        taxval   TYPE zfiwrt0010-taxval,
        excbas   TYPE zfiwrt0010-excbas,
        othbas   TYPE zfiwrt0010-othbas,
        color    TYPE c LENGTH 4,
      END OF tg_impo2.

** fim alteração Alexandre  ref; CS1016278 - IR107256 - 27.02.2003


DATA: BEGIN OF tg_impo OCCURS 0,
*      mark(1),
*      itmnum   TYPE zfiwrt0010-itmnum,
        taxtyp   TYPE zfiwrt0010-taxtyp,
        ttypetxt TYPE j_1bttytxt,
        taxgrp   TYPE j_1btaxgrp,
        base     TYPE zfiwrt0010-base,
        rate     TYPE zfiwrt0010-rate,
        taxval   TYPE zfiwrt0010-taxval,
        excbas   TYPE zfiwrt0010-excbas,
        othbas   TYPE zfiwrt0010-othbas,
        color    TYPE c LENGTH 4,
*      incluido por alexandre Rimini 30.03.2023
        line     TYPE i,
      END OF tg_impo,

      BEGIN OF tg_itens OCCURS 0,
        mark(1),
        itmnum         TYPE zfiwrt0009-itmnum,
        matnr          TYPE zfiwrt0009-matnr,
        maktx          TYPE makt-maktx,
        cfop(10),    "        TYPE ZFIWRT0006-CFOP,
        charg          TYPE zfiwrt0009-charg,
        werks          TYPE t001w-werks,
        lgort          TYPE zfiwrt0009-lgort,
        menge          TYPE zfiwrt0009-menge,
        meins          TYPE zfiwrt0009-meins,
        netpr          TYPE zfiwrt0009-netpr,
        netwr          TYPE zfiwrt0009-netwr,
        anln1          TYPE zfiwrt0009-anln1,
        anln2          TYPE zfiwrt0009-anln2,
        steuc          TYPE marc-steuc,
        vbeln          TYPE vbap-vbeln,
        posnr          TYPE vbap-posnr,
*-CS2020001331 - 06.10.2021 - JT - inicio
        possui_icms_st TYPE zpossui_icms_st,
*-CS2020001331 - 06.10.2021 - JT - fim
        vlr_iten_xml   TYPE zfiwrt0009-netwr,
        ncm_xml        TYPE zib_nfe_dist_itm-prod_ncm,
        vlr_icms_xml   TYPE zib_nfe_dist_itm-icms_valor,
        renas          TYPE atwrt,
        fase(4),
        vbeln_r        TYPE zfiwrt0009-vbeln_r,
        style2         TYPE lvc_t_styl,
        netdis         TYPE j_1bnetdis,
        netfre         TYPE j_1bnetfre,
        netins         TYPE j_1bnetins,
        netoth         TYPE j_1bnetoth,
        add_text       TYPE char01,
      END OF tg_itens,

      tg_docrefs TYPE TABLE OF ty_doc_refs WITH HEADER LINE,

      BEGIN OF tg_contab OCCURS 0,
        bschl      TYPE zfiwrt0011-bschl,
        hkont      TYPE zfiwrt0011-hkont,
        txt50      TYPE skat-txt50,
        waers_i    TYPE zfiwrt0011-waers_i,
        dmbtr      TYPE zfiwrt0011-dmbtr,
        curha	     TYPE zfiwrt0011-curha,
        dmbe2      TYPE zfiwrt0011-dmbe2,
        curin	     TYPE zfiwrt0011-curin,
        dmbe3      TYPE zfiwrt0011-dmbe3,
        waers      TYPE zfiwrt0011-waers,
        wrbtr      TYPE zfiwrt0011-wrbtr,
        artnr      TYPE zfiwrt0011-artnr,
        taxtyp     TYPE zfiwrt0011-taxtyp,
        estorno    TYPE zfiwrt0011-estorno,
        newbw      TYPE zfiwrt0011-newbw,
        zlsch      TYPE zfiwrt0011-zlsch,
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        taxa_juros TYPE zfiwrt0011-taxa_juros,
        taxa_multa TYPE zfiwrt0011-taxa_multa,
        hbkid      TYPE zfiwrt0011-hbkid,
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        zfbdt      TYPE zfiwrt0011-zfbdt,
        kostl      TYPE zfiwrt0011-kostl,
        umskz      TYPE zfiwrt0011-umskz,
        vbund      TYPE zfiwrt0011-vbund,
        style2     TYPE lvc_t_styl,
      END OF tg_contab,

      BEGIN OF tg_mensagems OCCURS 0,
        seqnum  TYPE zfiwrt0005-seqnum,
        linnum  TYPE zfiwrt0005-linnum,
        message TYPE zfiwrt0005-message,
      END OF tg_mensagems,

      BEGIN OF tg_movest OCCURS 0,
        bwart   TYPE zfiwrt0004-bwart,
        tcode   TYPE zfiwrt0004-tcode,
        mwskz1  TYPE zfiwrt0004-mwskz1,
        estorno TYPE zfiwrt0004-estorno,
      END OF tg_movest,

      BEGIN OF git_matnr_ov_pd OCCURS 0,
        vbeln TYPE vbak-vbeln,
        posnr TYPE vbap-posnr,
        ebeln TYPE ekko-ebeln,
        matnr TYPE vbap-matnr,
        meins TYPE ekpo-meins,
        maktx TYPE makt-maktx,
        steuc TYPE marc-steuc,
        werks TYPE ekpo-werks,
        lgort TYPE ekpo-lgort,
        charg TYPE eket-charg,
      END OF git_matnr_ov_pd,

      BEGIN OF tg_impo_comp OCCURS 0,
        itmnum TYPE zfiwrt0010-itmnum,
        edit   TYPE char1.
        INCLUDE STRUCTURE tg_impo.
DATA: END OF tg_impo_comp,

*-CS2023000043-09.02.2023-#102019-JT-inicio
BEGIN OF tg_impo_gera OCCURS 0.
  INCLUDE STRUCTURE tg_impo.
DATA: END OF tg_impo_gera,
*-CS2023000043-09.02.2023-#102019-JT-fim

BEGIN OF tg_mara_itens OCCURS 0,
  matnr TYPE mara-matnr,
  matkl TYPE mara-matkl,
END OF tg_mara_itens.

DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.
DATA: tg_impo_aux LIKE TABLE OF tg_impo WITH HEADER LINE.
DATA: tg_mensagems_aux LIKE TABLE OF tg_mensagems WITH HEADER LINE.
DATA: tg_mensagems_lim LIKE TABLE OF tg_mensagems WITH HEADER LINE.
DATA: tg_mensagems_imob LIKE TABLE OF tg_mensagems WITH HEADER LINE.

*-CS2021000595 - 22.06.2021 - JT - inicio
TYPES: BEGIN OF ty_ncm_mat,
         steuc TYPE marc-steuc,
         matnr TYPE marc-matnr.
TYPES: END   OF ty_ncm_mat.

TYPES: BEGIN OF ty_dtlcto,
         uname TYPE uname,
         budat TYPE zfiwrt0008-budat.
TYPES: END   OF ty_dtlcto.

DATA: t_tvarv   TYPE TABLE OF tvarvc,
      w_tvarv   TYPE tvarvc,
      t_ncm_mat TYPE TABLE OF ty_ncm_mat,
      w_ncm_mat TYPE ty_ncm_mat,
      t_set     TYPE TABLE OF rgsb4,
      w_set     TYPE rgsb4,
      t_dtlcto  TYPE TABLE OF ty_dtlcto,
      l_data    TYPE char10,
      w_dtlcto  TYPE ty_dtlcto.
*-CS2021000595 - 22.06.2021 - JT - fim

DATA: it_itens_nf LIKE TABLE OF tg_itens,
      wl_itens_nf LIKE LINE OF tg_itens.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF c_tab_strip_nf,
             tab1  LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC1',
             tab2  LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC2',
             tab3  LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC3',
             tab4  LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC4',
             tab5  LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC5',
             tab6  LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC6',
             tab7  LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC7',
             tab8  LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC8',
             tab9  LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC9',
             tab10 LIKE sy-ucomm VALUE 'TAB_STRIP_NF_FC10',
           END OF c_tab_strip_nf.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
CONTROLS:  tab_strip_nf TYPE TABSTRIP.
DATA: BEGIN OF g_tab_strip_nf,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZWRR0002',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_nf-tab1,
      END OF g_tab_strip_nf.
DATA:      ok_code   LIKE sy-ucomm.
DATA:      var_ucomm TYPE sy-ucomm.

DATA:
  gf_authorization_ft_09 TYPE c. "Workflow de Documentos

**********************************************************************
*DATA API LIM
**********************************************************************
DATA: ivals           TYPE TABLE OF sval,
      xvals           TYPE sval,
      wa_param        TYPE zstruct_get_data_lim,
      v_identificador TYPE string,
      lr_content      TYPE znfwe0002, "Estrutura Content
      lr_results      TYPE znfwe0003. "Estrutura com os campos do Content

DATA: vg_parid TYPE werks_d.


*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
INITIALIZATION.
  gf_authorization_ft_09 = abap_true.

*-CS2021000595 - 22.06.2021 - JT - inicio
  FREE v_automatico_memo.
  IMPORT v_automatico_memo FROM MEMORY ID 'AUTOMATICO'.
  DELETE FROM MEMORY ID 'AUTOMATICO'.

  IF v_automatico_memo = abap_true.
    PERFORM iniciar_lancamento_notas.
    PERFORM busca_dados.
    PERFORM busca_descricoes.
    PERFORM salvar_processo.
  ELSE.
    CALL SCREEN 100.
  ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .
    CLASS-METHODS:
      on_data_changed4 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_finished2 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
    CLASS-METHODS:
      on_data_changed_finished3 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_finished4 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
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
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar2 DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler2
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar2 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid2 TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
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

  METHOD on_toolbar.
    DATA: wl_desactive.
*    break-point.
*   Add customized toolbar buttons.
    IF wg_docs-docnum IS INITIAL.
      READ TABLE tg_fields TRANSPORTING NO FIELDS
        WITH KEY group1 = 'GR1'.
      IF sy-subrc IS INITIAL.
        IF wg_fiscal-retorno EQ 'S'.
          wl_desactive = 1.
        ELSE.
          wl_desactive = space.
        ENDIF.
      ELSE.
        wl_desactive = 1.
      ENDIF.
    ELSE.
      wl_desactive = 1.
    ENDIF.
    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    IF wg_docs-docnum IS INITIAL.
      READ TABLE tg_fields TRANSPORTING NO FIELDS
        WITH KEY group1 = 'GR1'.
      IF sy-subrc IS INITIAL.
        wl_desactive = space.
      ELSE.
        wl_desactive = 1.
      ENDIF.
    ELSE.
      wl_desactive = 1.
    ENDIF.
    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

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
    DATA: tl_itens_aux LIKE TABLE OF tg_itens,
          wl_itens     LIKE LINE OF tg_itens,
          wl_lines     TYPE sy-tabix.
    REFRESH: tl_itens_aux.

    IF p_operacao IS   NOT INITIAL
       AND p_bukrs IS  NOT INITIAL
       AND p_branch IS NOT INITIAL.

*      IF WG_FISCAL-LM_ESTOQUE EQ 'S'
*        AND P_PARVW IS  NOT INITIAL
*        AND P_PARID IS NOT INITIAL.
*        EXIT.
*      ENDIF.

      CASE e_ucomm.
        WHEN c_clos_msg.
          IF grid2 IS NOT INITIAL.
            CALL METHOD grid2->free.
            FREE: container_2, grid2.
          ENDIF.
*    posiciona spliter na altura x
          IF splitter IS NOT INITIAL.
            CALL METHOD splitter->set_row_height
              EXPORTING
                id     = 1
                height = 100.
          ENDIF.
          LEAVE TO SCREEN 100.
        WHEN c_add.
          tl_itens_aux[] = tg_itens[].
          REFRESH: tg_itens.
          LOOP AT tl_itens_aux INTO wl_itens.
            wl_itens-itmnum = sy-tabix * 10.
            wl_itens-fase = icon_display_more.
            APPEND wl_itens TO tg_itens.
          ENDLOOP.
          DESCRIBE TABLE tg_itens LINES wl_lines.
          CLEAR: wl_itens.
          wl_itens-fase = icon_display_more.
          wl_itens-itmnum = ( wl_lines + 1 ) * 10 .
          APPEND wl_itens TO tg_itens.

          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        WHEN c_del.
          CALL METHOD grid1->get_selected_cells
            IMPORTING
              et_cell = tg_selectedcell.

          LOOP AT tg_selectedcell INTO wg_selectedcell.
            DELETE tg_itens INDEX wg_selectedcell-row_id-index.

          ENDLOOP.
          IF wg_fiscal-retorno EQ 'N'.
            tl_itens_aux[] = tg_itens[].
            REFRESH: tg_itens.
            LOOP AT tl_itens_aux INTO wl_itens.
              wl_itens-itmnum = sy-tabix * 10.
              APPEND wl_itens TO tg_itens.
            ENDLOOP.
          ENDIF.
          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar2 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager2
      EXPORTING
        io_alv_grid = io_alv_grid2.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.
*   Add customized toolbar buttons.
    IF wg_docs-docnum IS INITIAL.
      READ TABLE tg_fields TRANSPORTING NO FIELDS
        WITH KEY group1 = 'GR1'.
      IF sy-subrc IS INITIAL.
        wl_desactive = space.

      ELSE.
        wl_desactive = 1.
      ENDIF.
    ELSE.
      wl_desactive = 1.
    ENDIF.
    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    IF wg_docs-docnum IS INITIAL.
      READ TABLE tg_fields TRANSPORTING NO FIELDS
            WITH KEY group1 = 'GR1'.
      IF sy-subrc IS INITIAL.
        wl_desactive = space.
      ELSE.
        wl_desactive = 1.
      ENDIF.
    ELSE.
      wl_desactive = 1.
    ENDIF.
    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*    ty_toolbar-butn_type = 3.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**   variable for Toolbar Button
*    ty_toolbar-icon      =  icon_view_close.
*    ty_toolbar-function  =  c_clos_msg.
*    ty_toolbar-disabled  = space.
*    ty_toolbar-butn_type = 0.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD c_alv_toolbarmanager2->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN c_add.
        APPEND INITIAL LINE TO tg_parc.
      WHEN c_del.
        CALL METHOD grid5->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_parc INTO tg_parc INDEX wg_selectedcell-row_id-index.

          IF tg_parc-parvw EQ p_parvw
          AND tg_parc-parid EQ p_parid.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel excluir o'
                                                   'registro!'.
          ELSE.
            DELETE tg_parc INDEX wg_selectedcell-row_id-index.

          ENDIF.

        ENDLOOP.
    ENDCASE.

    CALL METHOD grid5->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.
    DATA: wl_itens LIKE LINE OF tg_itens.

    num_row = e_row.  "*-CS2023000043-09.02.2023-#102019-JT
    SELECT SINGLE *
         FROM zfiwrt0001
         INTO @DATA(wl_0001_1)
          WHERE operacao EQ @p_operacao.

    IF e_row GT 0.
      READ TABLE tg_itens INTO wl_itens INDEX e_row.
      IF ( wg_fiscal-complemento EQ 'N' AND wl_0001_1-complement_icms = 'N' )
      OR wg_fiscal-complemento IS INITIAL.
        IF wl_itens-matnr IS NOT INITIAL
        AND wl_itens-werks IS NOT INITIAL
        AND wl_itens-menge IS NOT INITIAL
        AND wl_itens-netpr IS NOT INITIAL.
*    posiciona spliter na altura x
          CALL METHOD splitter->set_row_height
            EXPORTING
              id     = 1
              height = 50.

          vg_subscreen1 = c_dummy_header.

          IF grid2 IS NOT INITIAL.
            CALL METHOD grid2->free.

          ENDIF.

          FREE: container_2, grid2, tg_impo_aux.

          CALL METHOD splitter->get_container
            EXPORTING
              row       = 2
              column    = 1
            RECEIVING
              container = container_2.
          IF grid2 IS INITIAL.
            wa_layout-no_toolbar = c_x.
            CREATE OBJECT grid2
              EXPORTING
                i_parent = container_2.

            wa_layout-cwidth_opt = c_x.
            wa_layout-edit = space.
            wa_layout-info_fname = 'COLOR'.
*          wa_layout-grid_title = 'Impostos'.
            CONDENSE e_row NO-GAPS.
            CONCATENATE 'Impostos do Item' '-' wl_itens-itmnum INTO wa_layout-grid_title SEPARATED BY space.
            PERFORM montar_layout_impostos.
            PERFORM monta_impostos TABLES tg_impo_aux
                                   USING e_row.
            PERFORM f_monta_cor_imposto.
            CALL METHOD grid2->set_table_for_first_display
              EXPORTING
                is_layout       = wa_layout
              CHANGING
                it_fieldcatalog = t_fieldcatalog[]
                it_outtab       = tg_impo_aux[].

*-CS2023000043-09.02.2023-#102019-JT-inicio
            CALL METHOD grid2->register_edit_event
              EXPORTING
                i_event_id = cl_gui_alv_grid=>mc_evt_modified.

            CALL METHOD grid2->register_edit_event
              EXPORTING
                i_event_id = cl_gui_alv_grid=>mc_evt_enter.

            SET HANDLER: lcl_event_handler=>on_data_changed_finished4 FOR grid2,
                         lcl_event_handler=>on_data_changed4 FOR grid2.
*-CS2023000043-09.02.2023-#102019-JT-fim

*      *** Método de atualização de dados na Tela
            CALL METHOD grid2->refresh_table_display
              EXPORTING
                is_stable = wa_stable.
          ELSE.
*      *** Método de atualização de dados na Tela
            CALL METHOD grid2->refresh_table_display
              EXPORTING
                is_stable = wa_stable.

          ENDIF.
          wg_dg1 = c_maximizar.
          LEAVE TO SCREEN 100.
        ELSE.
*    posiciona spliter na altura x
          CALL METHOD splitter->set_row_height
            EXPORTING
              id     = 1
              height = 100.
        ENDIF.
      ELSEIF ( wg_fiscal-complemento EQ 'S' AND wl_itens-matnr IS NOT INITIAL ) OR wl_0001_1-complement_icms = 'S'.

        num_row = e_row.
*    posiciona spliter na altura x
        CALL METHOD splitter->set_row_height
          EXPORTING
            id     = 1
            height = 50.

        vg_subscreen1 = c_dummy_header.

        IF grid2 IS NOT INITIAL.
          CALL METHOD grid2->free.

        ENDIF.

        FREE: container_2, grid2, tg_impo_aux.

        CALL METHOD splitter->get_container
          EXPORTING
            row       = 2
            column    = 1
          RECEIVING
            container = container_2.
        IF grid2 IS INITIAL.
          wa_layout-no_toolbar = space.
          CREATE OBJECT grid2
            EXPORTING
              i_parent = container_2.

          wa_layout-cwidth_opt = c_x.
          wa_layout-no_toolbar = c_x.
          wa_layout-info_fname = 'COLOR'.
*          wa_layout-grid_title = 'Impostos'.
          CONDENSE e_row NO-GAPS.
          CONCATENATE 'Impostos do Item' '-' wl_itens-itmnum INTO wa_layout-grid_title SEPARATED BY space.
          PERFORM montar_layout_impostos.
          PERFORM monta_impostos TABLES tg_impo_aux
                                 USING e_row.
          PERFORM f_monta_cor_imposto.
          CALL METHOD grid2->set_table_for_first_display
            EXPORTING
              is_layout       = wa_layout
            CHANGING
              it_fieldcatalog = t_fieldcatalog[]
              it_outtab       = tg_impo_aux[].

          CALL METHOD grid2->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_modified.

          CALL METHOD grid2->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_enter.

          SET HANDLER:
*              LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR GRID1,
                    lcl_event_handler=>on_data_changed_finished4 FOR grid2,
                    lcl_event_handler=>on_data_changed4 FOR grid2.

*      *** Método de atualização de dados na Tela
          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        ELSE.
*      *** Método de atualização de dados na Tela
          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.
        wg_dg1 = c_maximizar.
        LEAVE TO SCREEN 100.

      ELSE.
*    POSICIONA SPLITER NA ALTURA X
        CALL METHOD splitter->set_row_height
          EXPORTING
            id     = 1
            height = 100.
      ENDIF.
    ENDIF.

** Método de atualização de dados na Tela
*    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY.

  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD on_hotspot_click.
    DATA i_ebelp TYPE ekpo-ebelp.

    READ TABLE tg_itens INTO wl_itens_nf  INDEX e_row_id-index.
    i_ebelp = wl_itens_nf-itmnum.
    IF wl_itens_nf-charg IS NOT INITIAL AND
       wl_itens_nf-werks IS NOT INITIAL AND
       wl_itens_nf-lgort IS NOT INITIAL AND
       wl_itens_nf-menge GT 0.
      CALL FUNCTION 'Z_MM_INDEA_LOTE'
        EXPORTING
          i_ebeln = p_seq_lcto
          i_ebelp = i_ebelp
          i_matnr = wl_itens_nf-matnr
          i_charg = wl_itens_nf-charg
          i_werks = wl_itens_nf-werks
          i_lgort = wl_itens_nf-lgort
          i_menge = wl_itens_nf-menge
          i_renas = wl_itens_nf-renas
          i_btn   = 'X'
          i_tcode = sy-tcode.
    ENDIF.
    "
  ENDMETHOD.

  METHOD on_data_changed.
    DATA: ls_good          TYPE lvc_s_modi,
          lv_value         TYPE lvc_value,
          lv_anln1         TYPE anla-anln1,
          vl_tabix         TYPE sy-tabix,
          vl_value         TYPE lvc_value,
          wl_mara          TYPE mara,
          wl_vbap          TYPE vbap,
          wl_makt          TYPE makt,
          wl_t001w         TYPE t001w,
          wl_mch1          TYPE mch1,
          wl_itens         LIKE LINE OF tg_itens,
          wl_where(30),
          wl_marc          TYPE marc,
          wl_mbew          TYPE mbew,
          wl_1bbranch      TYPE j_1bbranch,
          wl_1baa          TYPE j_1baa,
          wl_1bapn         TYPE  j_1bapn,
          wl_direct        TYPE j_1bapnv-direct,
          wl_dstcat        TYPE j_1bapnv-dstcat,
          tl_anep          TYPE TABLE OF anep,
          tl_anla          TYPE TABLE OF anla,
          tl_zfiwrt0009    TYPE TABLE OF zfiwrt0009,
          wl_zfiwrt0009    TYPE zfiwrt0009,
          wl_zfiwrt0008    TYPE zfiwrt0008,
          wl_j_1bnfdoc     TYPE j_1bnfdoc,
          wl_j_1bnfdoc_aux TYPE j_1bnfdoc,
          wl_tot_anbtr     TYPE anep-anbtr,
          wl_menge         TYPE mseg-menge,
          wl_dmbtr         TYPE mseg-dmbtr,
          wl_anln1         TYPE anla-anln1,
          wl_anln2         TYPE anla-anln2,
          wl_anla          TYPE anla,
          wl_msg_ret       TYPE zfiwrs0002,
          tl_anlc          TYPE TABLE OF anlc.


    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'MATNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE *
        FROM mara
        INTO wl_mara
          WHERE matnr EQ lv_value.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE *
          FROM makt
          INTO wl_makt
           WHERE spras EQ sy-langu
             AND matnr EQ wl_mara-matnr.

        IF sy-subrc IS INITIAL.
          MOVE: wl_makt-maktx TO lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'MAKTX'
              i_value     = lv_value.

        ENDIF.
        MOVE: wl_mara-meins TO lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'MEINS'
            i_value     = lv_value.
      ELSE.
        CLEAR: lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'MATNR'
            i_value     = lv_value.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Material selecionado não foi encontrado!'.
      ENDIF.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'WERKS'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE *
        FROM t001w
        INTO wl_t001w
         WHERE werks EQ lv_value.

      IF sy-subrc IS INITIAL.
*      select *
      ELSE.
        CLEAR: lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'WERKS'
            i_value     = lv_value.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Centro/Filial não encontrado.'.
      ENDIF.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'CHARG'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-tabix.
      IF wl_itens IS NOT INITIAL.
        wl_where = 'MATNR EQ WL_ITENS-MATNR'.
      ENDIF.
      IF lv_value IS NOT INITIAL.
        .

        IF sy-subrc IS INITIAL.
*      select *
        ELSE.
          CLEAR: lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'CHARG'
              i_value     = lv_value.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não existe lote para este material.'.
        ENDIF.
      ENDIF.
    ENDLOOP.


    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'VBELN' .

      READ TABLE tg_itens INTO wl_itens INDEX ls_good-tabix.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      IF wl_itens-posnr IS INITIAL.
        wl_itens-posnr = 10.
        MODIFY tg_itens FROM wl_itens INDEX ls_good-tabix TRANSPORTING posnr.
      ENDIF.
      SELECT SINGLE *
        FROM vbap
        INTO wl_vbap
        WHERE vbeln = lv_value
        AND   posnr = wl_itens-posnr.
      IF sy-subrc NE 0.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ordem selecionada não foi encontrada!'.
      ENDIF.
      "
      MOVE: wl_vbap-matnr TO lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MATNR'
          i_value     = lv_value.

      MOVE: wl_vbap-werks TO lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'WERKS'
          i_value     = lv_value.

      MOVE: wl_vbap-lgort TO lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'LGORT'
          i_value     = lv_value.

      MOVE: wl_vbap-charg TO lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'CHARG'
          i_value     = lv_value.

      IF viniciou_lcto_znfw0009 IS INITIAL.
        MOVE: wl_vbap-netpr TO lv_value.
        CONDENSE lv_value NO-GAPS.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NETPR'
            i_value     = lv_value.
      ENDIF.

      SELECT SINGLE *
        FROM mara
        INTO wl_mara
          WHERE matnr EQ wl_vbap-matnr.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE *
          FROM makt
          INTO wl_makt
           WHERE spras EQ sy-langu
             AND matnr EQ wl_mara-matnr.

        IF sy-subrc IS INITIAL.
          MOVE: wl_makt-maktx TO lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'MAKTX'
              i_value     = lv_value.

        ENDIF.
        MOVE: wl_mara-meins TO lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'MEINS'
            i_value     = lv_value.
      ELSE.
        CLEAR: lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'MATNR'
            i_value     = lv_value.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Material selecionado não foi encontrado!'.
      ENDIF.

    ENDLOOP.

*** PBI - 73759 - Inicio - CBRAND
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'ANLN1' OR fieldname = 'ANLN2'  .

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'ANLN1'
        IMPORTING
          e_value     = wl_anln1.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'ANLN2'
        IMPORTING
          e_value     = wl_anln2.

      CLEAR: tl_anep, tl_zfiwrt0009, wl_tot_anbtr.
      SELECT *
        FROM anep
        INTO TABLE tl_anep
         WHERE bukrs = p_bukrs
          AND  anln1 = wl_anln1
          AND  anln2 = wl_anln2
          AND  afabe    = 01.

      IF tl_anep IS NOT INITIAL.
        LOOP AT tl_anep INTO DATA(wl_anep).
          ADD wl_anep-anbtr TO  wl_tot_anbtr.
        ENDLOOP.

        IF wg_fiscal-imobilizado = 'S' AND wg_fiscal-tp_mv_imob <> 'V' AND wl_tot_anbtr > 0.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'NETPR'
              i_value     = wl_tot_anbtr.

*          SELECT *
*            FROM zfiwrt0009
*          INTO TABLE tl_zfiwrt0009
*            WHERE anln1  = wl_anln1
*               AND anln2 = wl_anln2.
*
*          IF tl_zfiwrt0009 IS NOT INITIAL.
*
*            LOOP AT tl_zfiwrt0009 INTO wl_zfiwrt0009.
*
*              SELECT SINGLE *
*                FROM zfiwrt0008
*                INTO wl_zfiwrt0008
*                  WHERE seq_lcto = wl_zfiwrt0009-seq_lcto.
*
*              SELECT SINGLE *
*                FROM j_1bnfdoc
*                INTO wl_j_1bnfdoc
*                  WHERE docnum = wl_zfiwrt0008-docnum.
*
*              IF wl_j_1bnfdoc-direct = 2 AND  wl_j_1bnfdoc-candat IS INITIAL.
*
*                SELECT SINGLE *
*                  FROM j_1bnfdoc
*                  INTO wl_j_1bnfdoc_aux
*                  WHERE  direct  = '1'
*                    AND  partyp  = 'B'
*                    AND  bukrs   = wl_j_1bnfdoc-bukrs
*                    AND  branch  = wl_j_1bnfdoc-parid+4(4)   "Empresa+Centro
*                    AND  docdat  = wl_j_1bnfdoc-docdat
*                    AND  nfenum  = wl_j_1bnfdoc-nfenum.
*
*                IF wl_j_1bnfdoc_aux IS INITIAL.
*                  MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Existe saida para esse Imobilizado sem Entrada na Filial Destino, Nota;' wl_j_1bnfdoc-nfenum.
*                ENDIF.
*              ENDIF.
*              CLEAR: wl_zfiwrt0009, wl_zfiwrt0008.
*            ENDLOOP.
*          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*** PBI - 73759 - Fim - CBRAND

    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'NETDIS'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-tabix.
      IF sy-subrc IS INITIAL.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NETDIS'
            i_value     = lv_value.

        SELECT SINGLE *
          FROM zfiwrt0009
          INTO @DATA(ls_zfiwrt0009)
          WHERE seq_lcto = @p_seq_lcto
            AND itmnum = @wl_itens-itmnum.
        IF sy-subrc IS INITIAL.
          ls_zfiwrt0009-netdis = lv_value.
          MODIFY zfiwrt0009 FROM ls_zfiwrt0009.
          COMMIT WORK.
        ENDIF.

      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'NETFRE'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-tabix.
      IF sy-subrc IS INITIAL.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NETFRE'
            i_value     = lv_value.

        SELECT SINGLE *
          FROM zfiwrt0009
          INTO @ls_zfiwrt0009
          WHERE seq_lcto = @p_seq_lcto
            AND itmnum = @wl_itens-itmnum.
        IF sy-subrc IS INITIAL.
          ls_zfiwrt0009-netfre = lv_value.
          MODIFY zfiwrt0009 FROM ls_zfiwrt0009.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'NETINS'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-tabix.
      IF sy-subrc IS INITIAL.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NETINS'
            i_value     = lv_value.

        SELECT SINGLE *
          FROM zfiwrt0009
          INTO @ls_zfiwrt0009
          WHERE seq_lcto = @p_seq_lcto
            AND itmnum = @wl_itens-itmnum.
        IF sy-subrc IS INITIAL.
          ls_zfiwrt0009-netins = lv_value.
          MODIFY zfiwrt0009 FROM ls_zfiwrt0009.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'NETOTH'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-tabix.
      IF sy-subrc IS INITIAL.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NETOTH'
            i_value     = lv_value.

        SELECT SINGLE *
          FROM zfiwrt0009
          INTO @ls_zfiwrt0009
          WHERE seq_lcto = @p_seq_lcto
            AND itmnum = @wl_itens-itmnum.
        IF sy-subrc IS INITIAL.
          ls_zfiwrt0009-netoth = lv_value.
          MODIFY zfiwrt0009 FROM ls_zfiwrt0009.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.

*Inicio Alteração - Leandro Valentim Ferreira - 06.06.23 - 91631
    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'ANLN1'.
      lv_anln1 = lv_value = ls_good-value.

      CONDENSE lv_value NO-GAPS.
      READ TABLE tg_itens ASSIGNING FIELD-SYMBOL(<wl_itens>) INDEX ls_good-tabix.
      IF sy-subrc IS INITIAL.

        IF wg_fiscal-imobilizado EQ 'S' AND wg_fiscal-tp_mv_imob NE 'V'.
          SELECT *
                 INTO TABLE tl_anlc
                 FROM anlc
                 WHERE bukrs EQ p_bukrs
                   AND anln1 EQ lv_anln1
                   AND afabe EQ '1'.

          IF sy-subrc EQ 0.
            SORT tl_anlc BY gjahr.
            READ TABLE tl_anlc INTO DATA(wl_anlc) INDEX 1.
            IF wl_anlc-kansw > 0.
              lv_value = wl_anlc-kansw.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'NETPR'
                  i_value     = lv_value.

              <wl_itens>-netwr = ( ( <wl_itens>-menge * lv_value ) + <wl_itens>-netfre + <wl_itens>-netins + <wl_itens>-netoth ) - <wl_itens>-netdis.

              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'NETWR'
                  i_value     = <wl_itens>-netwr.
            ENDIF.
          ENDIF.
        ENDIF.


        CLEAR tl_anla[].
        SELECT *
               INTO TABLE tl_anla
               FROM anla
               WHERE bukrs EQ p_bukrs
                 AND anln1 EQ lv_anln1.

        SORT tl_anla BY anln1 anln2 DESCENDING.
        READ TABLE tl_anla INTO wl_anla INDEX 1.
        IF sy-subrc EQ 0.
          CONDENSE lv_anln1 NO-GAPS.
*          IF tg_mensagems_aux[] IS NOT INITIAL.
*            SORT tg_mensagems_aux BY seqnum DESCENDING linnum DESCENDING.
*            READ TABLE tg_mensagems_aux ASSIGNING FIELD-SYMBOL(<fs_message>) INDEX 1.
*            IF sy-subrc EQ 0.
*              ADD 1 TO <fs_message>-linnum.
*              <fs_message>-message = 'Imobilizado: ' && lv_anln1.
*
*              IF wl_anla-anln2 IS NOT INITIAL.
*                APPEND INITIAL LINE TO tg_mensagems_aux ASSIGNING FIELD-SYMBOL(<fs_message1>).
*                MOVE-CORRESPONDING <fs_message> TO <fs_message1>.
*                ADD 1 TO <fs_message1>-linnum.
*                <fs_message1>-message = 'Subnº do imobilizado: ' && wl_anla-anln2.
*              ENDIF.
*            ENDIF.
*          ENDIF.
          IF wl_anla-anln2 IS NOT INITIAL.
            lv_value = wl_anla-anln2.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'ANLN2'
                i_value     = lv_value.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'NETPR'.
      lv_anln1 = lv_value = ls_good-value.

      CONDENSE lv_value NO-GAPS.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-tabix.
      IF sy-subrc IS INITIAL.

        IF wg_fiscal-imobilizado EQ 'S' AND wg_fiscal-tp_mv_imob NE 'V'.
          SELECT *
                 INTO TABLE tl_anlc
                 FROM anlc
                 WHERE bukrs EQ p_bukrs
                   AND anln1 EQ wl_itens-anln1
                   AND afabe EQ '1'.

          IF sy-subrc EQ 0.
            SORT tl_anlc BY gjahr.
            READ TABLE tl_anlc INTO wl_anlc INDEX 1.
            IF wl_itens-netpr IS NOT INITIAL.
              IF wl_anlc-kansw > 0 AND wl_anlc-kansw < wl_itens-netpr.
                lv_value = wl_anlc-kansw.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'NETPR'
                    i_value     = lv_value.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*Fim Alteração - Leandro Valentim Ferreira - 06.06.23 - 91631


    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    PERFORM verifica_erros.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.
*    LOOP AT er_data_changed->mt_good_cells
*                             INTO ls_good.
*
*      READ TABLE tg_itens INTO wl_itens INDEX ls_good-ROW_ID.
****> Determina o CFOP
*      IF wg_direitos-cfop IS INITIAL.
*        SELECT SINGLE *
*          FROM marc
*          INTO wl_marc
*           WHERE matnr EQ wl_itens-matnr.
*
*        IF sy-subrc IS INITIAL.
*          SELECT SINGLE *
*            FROM mbew
*            INTO wl_mbew
*             WHERE matnr EQ wl_itens-matnr
*               AND bwkey EQ wl_itens-werks.
*
*          IF sy-subrc IS INITIAL.
*            SELECT SINGLE *
*              FROM j_1bbranch
*               INTO wl_1bbranch
*               WHERE bukrs  EQ p_bukrs
*                 AND branch EQ p_branch.
*
*            IF sy-subrc IS INITIAL.
*              SELECT SINGLE *
*                FROM j_1baa
*                INTO wl_1baa
*                 WHERE nftype EQ wg_fiscal-nftype.
*
*              IF wl_1baa-entrad EQ c_x.
*                wl_direct = c_1.
*              ELSE.
*                wl_direct = c_2.
*              ENDIF.
*
*              IF wg_direitos-indcoper EQ c_d.
*                wl_dstcat = c_0.
*
*              ELSE.
*                wl_dstcat = c_1.
*
*              ENDIF.
*
*              SELECT SINGLE *
*                FROM j_1bapn
*                INTO wl_1bapn
*                 WHERE direct EQ wl_direct
*                   AND dstcat EQ wl_dstcat
*                   AND indus3 EQ wl_marc-indus
*                   AND itmtyp EQ wg_fiscal-itmtyp
*                   AND ownpro EQ wl_mbew-ownpr
*                   AND matuse EQ wl_mbew-mtuse
*                   AND indus1 EQ wl_1bbranch-industry.
*
*              IF sy-subrc IS INITIAL.
*                lv_value = wl_1bapn-cfop.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        lv_value = wg_direitos-cfop.
*
*      ENDIF.
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'CFOP'
*          i_value     = lv_value.
*
*      WL_ITENS-NETWR = wl_itens-menge * wl_itens-netpr.
*      lv_value = WL_ITENS-NETWR.
*      CONDENSE LV_VALUE.
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'NETWR'
*          i_value     = lv_value.
*    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD on_data_changed4.
    DATA: ls_good   TYPE lvc_s_modi,
          lv_value  TYPE lvc_value,
          wl_rate   TYPE zfiwrt0010-rate,
          wl_base   TYPE zfiwrt0010-base,
          wl_taxval TYPE zfiwrt0010-taxval.

    LOOP AT er_data_changed->mt_good_cells
                                 INTO ls_good
                                 WHERE fieldname = 'BASE'
                                   OR  fieldname = 'RATE'
                                   OR  fieldname = 'EXCBAS'
                                   OR  fieldname = 'OTHBAS'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      IF ls_good-fieldname EQ 'BASE'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'RATE'
          IMPORTING
            e_value     = wl_rate.

***  Realiza o calculo do campo "Valor da Venda"
        TRY.
            wl_taxval = lv_value * ( wl_rate / 100 ).
          CATCH cx_sy_zerodivide.
        ENDTRY.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TAXVAL'
            i_value     = wl_taxval.
      ELSEIF  ls_good-fieldname EQ 'RATE'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'BASE'
          IMPORTING
            e_value     = wl_base.

***  Realiza o calculo do campo "TAXVAL"
        TRY.
            wl_taxval = wl_base * ( lv_value / 100 ).
          CATCH cx_sy_zerodivide.
        ENDTRY.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TAXVAL'
            i_value     = wl_taxval.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED4
  METHOD on_data_changed_finished.
    DATA: wl_itens     LIKE LINE OF tg_itens,
          ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          wl_marc      TYPE marc,
          wl_mbew      TYPE mbew,
          wl_1bbranch  TYPE j_1bbranch,
          wl_1baa      TYPE j_1baa,
          wl_1bapn     TYPE  j_1bapn,
          wl_direct    TYPE j_1bapnv-direct,
          wl_dstcat    TYPE j_1bapnv-dstcat,
          tl_anep      TYPE TABLE OF anep,
          wl_tot_anbtr TYPE anep-anbtr.
*    BREAK ABAP.

    LOOP AT et_good_cells INTO ls_good
       WHERE tabix GT 0.

      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE *
          FROM zfiwrt0001
          INTO @DATA(wl_0001_1)
           WHERE operacao EQ @p_operacao.

        IF wg_direitos-cfop IS INITIAL AND wl_0001_1-lm_indea NE 'S' AND
           wl_0001_1-complement_icms NE 'S'. "*-CS2023000043-09.02.2023-#102019-JT
          SELECT SINGLE *
            FROM marc
            INTO wl_marc
             WHERE matnr EQ wl_itens-matnr
               AND werks EQ p_branch.

          IF sy-subrc IS INITIAL.
            wl_itens-steuc = wl_marc-steuc.
            SELECT SINGLE *
              FROM mbew
              INTO wl_mbew
               WHERE matnr EQ wl_itens-matnr
                 AND bwkey EQ wl_itens-werks.

            IF sy-subrc IS INITIAL.
              SELECT SINGLE *
                FROM j_1bbranch
                 INTO wl_1bbranch
                 WHERE bukrs  EQ p_bukrs
                   AND branch EQ p_branch.

              IF sy-subrc IS INITIAL.
                SELECT SINGLE *
                  FROM j_1baa
                  INTO wl_1baa
                   WHERE nftype EQ wg_fiscal-nftype.

                IF wl_1baa-entrad EQ c_x.
                  wl_direct = c_1.
                ELSE.
                  wl_direct = c_2.
                ENDIF.

                IF wg_direitos-indcoper EQ c_d.
                  wl_dstcat = c_0.

                ELSE.
                  wl_dstcat = c_1.

                ENDIF.

                SELECT SINGLE *
                  FROM j_1bapn
                  INTO wl_1bapn
                   WHERE direct EQ wl_direct
                     AND dstcat EQ wl_dstcat
                     AND indus3 EQ wl_marc-indus
                     AND itmtyp EQ wg_fiscal-itmtyp
                     AND ownpro EQ wl_mbew-ownpr
                     AND matuse EQ wl_mbew-mtuse
                     AND indus1 EQ wl_1bbranch-industry.

                IF sy-subrc IS INITIAL.
                  wl_itens-cfop = wl_1bapn-cfop.
                ELSE.
                  CLEAR: wl_itens-cfop.
                ENDIF.
              ELSE.
                CLEAR: wl_itens-cfop.
              ENDIF.
            ELSE.
              CLEAR: wl_itens-cfop.
            ENDIF.
          ELSE.
            CLEAR: wl_itens-cfop, wl_itens-steuc.
          ENDIF.
        ELSEIF wl_0001_1-lm_indea NE 'S'.
          SELECT SINGLE *
            FROM marc
            INTO wl_marc
             WHERE matnr EQ wl_itens-matnr
               AND werks EQ p_branch.

          wl_itens-steuc = wl_marc-steuc.

*-CS2023000043-09.02.2023-#102019-JT-inicio
          IF wl_0001_1-complement_icms NE 'S'.
            wl_itens-cfop = wg_direitos-cfop.
          ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim
        ENDIF.

        IF ( viniciou_lcto_znfw0009 IS NOT INITIAL ) AND
           ( wg_fiscal-ebeln IS NOT INITIAL OR wg_fiscal-vbeln IS NOT INITIAL ) AND
           ( vlancamento_znfw0009 = 'MIC' OR vlancamento_znfw0009 = 'ONF'  ).

          IF wl_itens-menge IS NOT INITIAL AND wl_itens-netpr IS NOT INITIAL.

*            wl_itens-netwr = wl_itens-menge * wl_itens-netpr.
            wl_itens-netwr = ( ( wl_itens-menge * wl_itens-netpr ) + wl_itens-netfre + wl_itens-netins + wl_itens-netoth ) - wl_itens-netdis.

            IF wl_itens-netwr <> wl_itens-vlr_iten_xml.

              DATA(lva_libera_diferenca) = abap_false.

              IF wg_fiscal-access_key IS NOT INITIAL.
                DATA: lva_diferenca      TYPE zfiwrt0009-netwr,
                      lva_tolerancia_min TYPE zfit0141-tolerancia,
                      lva_tolerancia_max TYPE zfit0141-tolerancia.

                lva_diferenca = wl_itens-netwr - wl_itens-vlr_iten_xml.

                IF abs( lva_diferenca ) > 1.

                  SELECT SINGLE tolerancia
                    FROM zfit0141
                    INTO @DATA(tolerancia)
                    WHERE chave EQ @wg_fiscal-access_key.

                  IF sy-subrc IS INITIAL.
                    lva_tolerancia_min = tolerancia * -1.
                    lva_tolerancia_max = tolerancia.

                    IF lva_diferenca BETWEEN lva_tolerancia_min AND lva_tolerancia_max.
                      lva_libera_diferenca = abap_true.
                    ENDIF.
                  ENDIF.

                  IF lva_libera_diferenca EQ abap_false.
                    MESSAGE 'Valor total do item não corresponde ao valor total da nota' TYPE 'I'.
                    CLEAR: wl_itens-menge, wl_itens-netpr.
                    wl_itens-netwr = wl_itens-vlr_iten_xml.
                    MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id.
                    EXIT.
                  ENDIF.

                ENDIF.

              ENDIF.

            ELSE.
              MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id.
            ENDIF.
          ENDIF.
        ELSE.
*          wl_itens-netwr = wl_itens-menge * wl_itens-netpr.
          wl_itens-netwr = ( ( wl_itens-menge * wl_itens-netpr ) + wl_itens-netfre + wl_itens-netins + wl_itens-netoth ) - wl_itens-netdis.
          MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id.
        ENDIF.


*** pbi - 73759 - inicio - cbrand
        CLEAR: tl_anep, wl_tot_anbtr.
        SELECT *
          FROM anep
          INTO TABLE tl_anep
           WHERE bukrs = p_bukrs
            AND  anln1 = wl_itens-anln1
            AND  anln2 = wl_itens-anln2
            AND  afabe    = 01.

        IF tl_anep IS NOT INITIAL.

          LOOP AT tl_anep INTO DATA(wl_anep).
            ADD wl_anep-anbtr TO  wl_tot_anbtr.
          ENDLOOP.

          IF wg_fiscal-imobilizado = 'S' AND wg_fiscal-tp_mv_imob <> 'V' AND wl_tot_anbtr > 0.

            REFRESH: style2, tg_itens-style2.

            wa_style-fieldname = 'NETPR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.

            INSERT wa_style INTO TABLE style2.
            INSERT LINES OF style2 INTO TABLE tg_itens-style2.
            MOVE-CORRESPONDING tg_itens TO wl_itens.
            MODIFY tg_itens[] FROM wl_itens INDEX ls_good-row_id TRANSPORTING style2.
          ENDIF.
        ENDIF.
*** PBI - 73759 - inicio - CBRAND

      ENDIF.
    ENDLOOP.

    PERFORM monta_contabil.
    PERFORM verifica_erros.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.
*** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.


    PERFORM verifica_erros.

  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD on_data_changed_finished2.
    DATA: ls_good TYPE lvc_s_modi,
          wl_parc LIKE LINE OF tg_parc.
*    break abap.
    LOOP AT et_good_cells INTO ls_good
    WHERE tabix GT 0.

      READ TABLE tg_parc INTO wl_parc INDEX ls_good-row_id.
      IF sy-subrc IS INITIAL.
        IF wl_parc-parvw EQ c_ag
        OR wl_parc-parvw EQ c_lr.
          SELECT SINGLE name1
            FROM kna1
             INTO wl_parc-nome
             WHERE kunnr EQ wl_parc-parid.

        ELSEIF wl_parc-parvw EQ c_br
           OR wl_parc-parvw  EQ c_z1
           OR  wl_parc-parvw EQ c_lf
           OR  wl_parc-parvw EQ c_pc
           OR  wl_parc-parvw EQ c_sp.
          SELECT SINGLE name1
            FROM lfa1
             INTO wl_parc-nome
              WHERE lifnr EQ wl_parc-parid.
        ENDIF.
        MODIFY tg_parc FROM wl_parc INDEX ls_good-row_id.
        CLEAR: wl_parc.
      ENDIF.
    ENDLOOP.
    PERFORM verifica_erros.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.
*** Método de atualização de dados na Tela
    CALL METHOD grid5->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "on_data_changed2

  METHOD on_data_changed_finished3.
    DATA: ls_good   TYPE lvc_s_modi,
          lv_value  TYPE lvc_value,
          wl_contab LIKE LINE OF tg_contab,
          v_cli.

    DATA: wa_zsdt0075 TYPE zsdt0075.
    CLEAR: wl_contab.

*    break abap.
    LOOP AT et_good_cells INTO ls_good
    WHERE tabix GT 0.

      IF ls_good-fieldname EQ 'ZLSCH'.
        IF ls_good-value EQ 'D'.
          READ TABLE tg_contab ASSIGNING FIELD-SYMBOL(<fs_contab>) INDEX ls_good-row_id.
          IF sy-subrc IS INITIAL.
            SELECT SINGLE *
                   INTO @DATA(wl_zfit0196)
                   FROM zfit0196.
            IF <fs_contab>-taxa_juros IS INITIAL.
              <fs_contab>-taxa_juros = wl_zfit0196-juros.
            ENDIF.
            IF <fs_contab>-taxa_multa IS INITIAL.
              <fs_contab>-taxa_multa = wl_zfit0196-multa.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE tg_contab ASSIGNING <fs_contab> INDEX ls_good-row_id.
          IF sy-subrc IS INITIAL.
            CLEAR: <fs_contab>-taxa_juros, <fs_contab>-taxa_multa.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE tg_contab INTO tg_contab INDEX ls_good-row_id.
      IF sy-subrc IS INITIAL.
        READ TABLE tg_contab INTO wl_contab
          WITH KEY hkont = tg_contab-hkont
                   estorno = 'X'.
        IF sy-subrc IS INITIAL.
          MOVE: tg_contab-zlsch TO wl_contab-zlsch,
                tg_contab-zfbdt TO wl_contab-zfbdt,
                tg_contab-kostl TO wl_contab-kostl.

          MODIFY tg_contab FROM wl_contab INDEX sy-tabix."ls_good-row_id.
          CLEAR: wl_contab, tg_contab.
        ENDIF.

      ENDIF.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE tg_contab INTO wl_contab INDEX ls_good-row_id.

      CLEAR: wa_zsdt0075.
      SELECT SINGLE * FROM zsdt0075 INTO wa_zsdt0075
       WHERE kunnr = wl_contab-hkont
         AND bdatu >= sy-datum.

      IF ( sy-subrc NE 0 ) AND ( wl_contab-zlsch NE c_d ).
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Para este cliente é obrigatório ser emitido Boleto de Cobrança ajustar a forma de pagamento'.
      ENDIF.



*      IF XBOL = 'N'.                                                  "Validação pela função "AG"
*        IF LV_VALUE <> 'D'.                                           "A formapgto na aba contabilização é tipo "D"
*          CLEAR: LV_VALUE.                                            "Limpa a variável
*          READ TABLE TG_CONTAB INTO WL_CONTAB INDEX LS_GOOD-ROW_ID.
*          MOVE: LV_VALUE TO WL_CONTAB-ZLSCH.
*          MODIFY TG_CONTAB FROM WL_CONTAB INDEX SY-TABIX."ls_good-row_id.
*          CLEAR: WL_CONTAB, TG_CONTAB.
*          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Para este cliente é obrigatório ser emitido Boleto de Cobrança ajustar a forma de pagamento'.
*        ENDIF.
*      ELSE.


      "ENDIF.




    ENDLOOP.

    PERFORM verifica_erros.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.

*** Método de atualização de dados na Tela
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED3
  METHOD on_data_changed_finished4.

    DATA: ls_good      TYPE lvc_s_modi,
          wl_impo_aux  LIKE LINE OF tg_impo_aux,
          wl_impo_comp LIKE LINE OF tg_impo_comp,
          wl_itens2    LIKE LINE OF tg_itens.
    CLEAR: wl_impo_aux.
*    break abap.
    READ TABLE tg_itens INTO wl_itens2 INDEX num_row.
    LOOP AT et_good_cells INTO ls_good
    WHERE tabix GT 0.

      READ TABLE tg_impo_aux INTO tg_impo_aux INDEX ls_good-row_id.
      IF sy-subrc IS INITIAL.
        READ TABLE tg_impo_aux INTO wl_impo_aux
          WITH KEY taxtyp = tg_impo_aux-taxtyp.

        IF sy-subrc IS INITIAL.
          MOVE: tg_impo_aux-base   TO wl_impo_aux-base,
                tg_impo_aux-rate   TO wl_impo_aux-rate,
                tg_impo_aux-taxval TO wl_impo_aux-taxval,
                tg_impo_aux-excbas TO wl_impo_aux-excbas,
                tg_impo_aux-othbas TO wl_impo_aux-othbas.
          MODIFY tg_impo_aux FROM wl_impo_aux INDEX sy-tabix."ls_good-row_id.
          "CLEAR: wl_impo_aux, tg_impo_aux.
        ENDIF.
        READ TABLE tg_impo_comp INTO wl_impo_comp WITH KEY itmnum = wl_itens2-itmnum
                                                           taxtyp = tg_impo_aux-taxtyp BINARY SEARCH.
        IF sy-subrc NE 0.
          MOVE-CORRESPONDING: wl_impo_aux TO wl_impo_comp.
          MOVE: wl_itens2-itmnum TO wl_impo_comp-itmnum,
                abap_true        TO wl_impo_comp-edit.
          APPEND wl_impo_comp TO tg_impo_comp.
        ELSE.
          MOVE-CORRESPONDING: wl_impo_aux TO wl_impo_comp.
          MOVE: wl_itens2-itmnum TO wl_impo_comp-itmnum,
                abap_true        TO wl_impo_comp-edit.
          MODIFY tg_impo_comp INDEX sy-tabix FROM wl_impo_comp.
        ENDIF.
        CLEAR: tg_impo_comp.
        CLEAR: wl_impo_aux, tg_impo_aux.
      ENDIF.
    ENDLOOP.

*    PERFORM verifica_erros.
*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*      EXPORTING
*        i_screen      = '100'
*        i_show        = space
*        i_repid       = sy-repid
*        i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
*        i_set_field   = 'X_FIELD'
*      IMPORTING
*        e_messagem    = wg_mensagem
*      TABLES
*        it_msgs       = tg_msg_ret.

*** Método de atualização de dados na Tela
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED3
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD node_double_click.
    DATA: wl_mensagems LIKE LINE OF tg_mensagems,
          tl_editor    TYPE TABLE OF ty_editor,
          wl_editor    TYPE ty_editor.

    REFRESH: tl_editor.
    IF node_key EQ c_root.
      LOOP AT tg_mensagems_aux INTO wl_mensagems.

        wl_editor-line = wl_mensagems-message.
        APPEND wl_editor TO tl_editor.
      ENDLOOP.

      CALL METHOD editor->set_text_as_r3table
        EXPORTING
          table = tl_editor.

      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
    ELSE.
      LOOP AT tg_mensagems_aux INTO wl_mensagems
         WHERE seqnum EQ node_key.

        wl_editor-line = wl_mensagems-message.
        APPEND wl_editor TO tl_editor.
      ENDLOOP.

      CALL METHOD editor->set_text_as_r3table
        EXPORTING
          table = tl_editor.

      READ TABLE tg_mensagems INTO wl_mensagems
        WITH KEY seqnum = node_key.

      IF sy-subrc IS NOT INITIAL.
        READ TABLE tg_fields TRANSPORTING NO FIELDS
          WITH KEY group1 = 'GR1'.
        IF sy-subrc IS INITIAL.
          CALL METHOD editor->set_readonly_mode
            EXPORTING
              readonly_mode = 0.
          wg_dclknodekey  = node_key.
        ELSE.
          CALL METHOD editor->set_readonly_mode
            EXPORTING
              readonly_mode = 1.
        ENDIF.
      ELSE.
        CALL METHOD editor->set_readonly_mode
          EXPORTING
            readonly_mode = 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "drop_complete
ENDCLASS.                    "lcl_dragdrop_receiver IMPLEMENTATION

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
MODULE tab_strip_nf_active_tab_set OUTPUT.
  IF sy-calld = 'X' AND  p_seq_lcto IS INITIAL.
    GET PARAMETER ID 'SEQ' FIELD  p_seq_lcto.
    ok-code = c_search.
  ENDIF.
  PERFORM verifica_erros.
  tab_strip_nf-activetab = g_tab_strip_nf-pressed_tab.
  CASE g_tab_strip_nf-pressed_tab.
    WHEN c_tab_strip_nf-tab1.
      g_tab_strip_nf-subscreen = '0213'.
    WHEN c_tab_strip_nf-tab2.
      g_tab_strip_nf-subscreen = '0220'.
    WHEN c_tab_strip_nf-tab3.
      g_tab_strip_nf-subscreen = '0230'.
    WHEN c_tab_strip_nf-tab4.
      g_tab_strip_nf-subscreen = '0241'.
    WHEN c_tab_strip_nf-tab5.
      g_tab_strip_nf-subscreen = '0250'.
    WHEN c_tab_strip_nf-tab6.
      g_tab_strip_nf-subscreen = '0215'.                    "'0211'.
    WHEN c_tab_strip_nf-tab7.
      g_tab_strip_nf-subscreen = '0212'.
    WHEN c_tab_strip_nf-tab8.
      g_tab_strip_nf-subscreen = '0260'.
    WHEN c_tab_strip_nf-tab9.
      g_tab_strip_nf-subscreen = '0270'.
    WHEN   c_tab_strip_nf-tab10.
      g_tab_strip_nf-subscreen = '0280'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tab_strip_nf_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'LIM'.
      PERFORM f_get_data_lim.
    WHEN c_tab_strip_nf-tab1.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab1.
    WHEN c_tab_strip_nf-tab2.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab2.
    WHEN c_tab_strip_nf-tab3.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab3.
    WHEN c_tab_strip_nf-tab4.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab4.
    WHEN c_tab_strip_nf-tab5.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab5.
    WHEN c_tab_strip_nf-tab6.

      PERFORM f_check_regras_znfw0009.

      IF ( wg_fiscal-ebeln IS INITIAL AND wg_fiscal-vbeln IS INITIAL ) AND
         ( viniciou_lcto_znfw0009 = 'X'   ) AND
         ( vlancamento_znfw0009   = 'MIC' ) AND sy-subrc EQ 0.
        MESSAGE 'Não informado o Nr. Pedido ou lançamento sem Ordem de Venda!' TYPE  'S'.
        EXIT.
      ELSE.
        PERFORM z_sugere_itens.
        g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab6.
      ENDIF.
    WHEN c_tab_strip_nf-tab7.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab7.
    WHEN c_tab_strip_nf-tab8.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab8.
    WHEN c_tab_strip_nf-tab9.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab9.
    WHEN c_tab_strip_nf-tab10.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab10.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.



ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.
  READ TABLE tg_fields
   WITH KEY group1 = 'GR1'.
  IF sy-subrc IS NOT INITIAL.
    APPEND c_save TO fcode.
    APPEND c_atuali TO fcode.
    APPEND c_deldoc TO fcode.
  ENDIF.

  IF sy-ucomm <> c_add. "LIM PSA Inclui no FCODE para ele exluir do menu
    APPEND  c_lim TO fcode.
  ENDIF.

  IF sy-tcode EQ c_display.
    SET PF-STATUS 'Z002' EXCLUDING fcode.
  ELSE.
    SET PF-STATUS 'Z001' EXCLUDING fcode.
  ENDIF.
  IF tg_itens[] IS NOT INITIAL.
*    SET CURSOR FIELD 'WG_DESC_OPERACAO'.
  ENDIF.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*& salvar processo
*&---------------------------------------------------------------------*
FORM salvar_processo.

  DELETE tg_itens WHERE matnr IS INITIAL
                    AND werks IS INITIAL
                    AND netwr IS INITIAL.

  IF v_automatico_memo = abap_false.
    CALL METHOD grid1->check_changed_data.
  ENDIF.

  PERFORM verifica_erros.

  IF tg_msg_ret[] IS INITIAL.
    PERFORM grava_dados.
    REFRESH: tg_fields.
*-CS2023000043-09.02.2023-#102019-JT-inicio
    CLEAR: wg_acao.
    IF grid2 IS NOT INITIAL.
      CALL METHOD grid2->free.
    ENDIF.
    FREE: container_2, grid2, tg_impo_aux.
*-CS2023000043-09.02.2023-#102019-JT-fim

  ELSE.
    IF v_automatico_memo = abap_false.
      MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = c_x
          i_repid       = sy-repid
          i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
          i_set_field   = 'X_FIELD'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: wl_0008       TYPE zfiwrt0008,
        wl_zib_chv    TYPE zib_contabil_chv,
        wl_zib_objkey TYPE zib_contabil-obj_key,
        msg(255),
        resposta,
        opt           TYPE ctu_params,
        tl_bdc        TYPE TABLE OF bdcdata,
        wl_bdc        TYPE bdcdata,
        wl_doc_ref    TYPE zfiwrt0008-seq_lcto.

  REFRESH: tl_bdc.

  READ TABLE tg_fields TRANSPORTING NO FIELDS
    WITH KEY group1 = 'GR1'.
  IF sy-subrc IS INITIAL.
    IF ok-code = c_search.
      ok-code = c_atuali.
    ENDIF.

  ENDIF.

  CASE ok-code.
    WHEN 'ANEXAR'.
      PERFORM habilitar_workflow_documentos.
    WHEN c_deldoc.
      PERFORM elemina_doc.
      PERFORM limpa_campos.
      REFRESH: tg_fields.
      CLEAR: p_operacao, p_seq_lcto, p_parvw, p_parid, p_branch, p_bukrs, wg_desc_parid,
             wg_desc_operacao, wg_desc_parvw, wg_desc_branch, wg_desc_bukrs, p_loekz.
    WHEN c_search.
      PERFORM limpa_campos.
      PERFORM busca_dados_doc.
      PERFORM busca_descricoes.
    WHEN c_save.
*-CS2021000595 - 22.06.2021 - JT - inicio
      PERFORM salvar_processo.
*-CS2021000595 - 22.06.2021 - JT - fim

    WHEN c_show_msgre.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.

    WHEN c_back.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          seq_lcto = p_seq_lcto.
      wl_doc_ref = p_seq_lcto.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          seq_lcto = wl_doc_ref.
      SET SCREEN 0.

    WHEN c_add.

      PERFORM z_novo_lan.

      wg_acao = c_add.  "*-CS2023000043-09.02.2023-#102019-JT

*      IF WG_FLAG IS INITIAL.
*
*        WG_SUGERE_TRANSP = ABAP_TRUE.
*
*        PERFORM LIMPA_CAMPOS.
*        CLEAR: P_OPERACAO, P_SEQ_LCTO, P_PARVW, P_PARID, P_BRANCH, P_BUKRS, WG_DESC_PARID,
*               WG_DESC_OPERACAO, WG_DESC_PARVW, WG_DESC_BRANCH, WG_DESC_BUKRS, P_LOEKZ.
*        PERFORM GET_NEXT_NUMBER IN PROGRAM ZWRR0001 USING  'ZSEQ_LCTO'
*                                                        '1'
*                                               CHANGING P_SEQ_LCTO.
*
*      ENDIF.
*      PERFORM TRATA_CAMPOS USING SPACE
*                                  'GR1'
*                                     C_1       "INPUT 1     NO INPUT 0
*                                     C_0.      "INVISIBLE 1 VISIBLE 0
*      PERFORM TRATA_CAMPOS USING SPACE
*                                'GR2'
*                                 C_0       "INPUT 1     NO INPUT 0
*                                 C_0.      "INVISIBLE 1 VISIBLE 0
**      wg_docs-bldat = sy-datum.
*      WG_DOCS-BUDAT = SY-DATUM.
    WHEN c_cancel.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          seq_lcto = p_seq_lcto.

      wl_doc_ref = p_seq_lcto.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          seq_lcto = wl_doc_ref.

      LEAVE TO TRANSACTION 'ZNFW0002'.
      REFRESH: tg_fields.
      CLEAR: p_operacao, p_seq_lcto, p_parvw, p_parid, p_branch, p_bukrs, wg_desc_parid,
             wg_desc_operacao, wg_desc_parvw, wg_desc_branch, wg_desc_bukrs, p_loekz.
      PERFORM limpa_campos.
*      PERFORM busca_dados_doc.

    WHEN c_atuali.
*      refresh: tg_parc.
      PERFORM busca_dados.
*      PERFORM busca_dados_doc.
      PERFORM busca_descricoes.
    WHEN c_modif.

      IF p_seq_lcto IS NOT INITIAL.
        TRY.
            zcl_boletim_producao=>zif_boletim_producao~get_instance(
            )->check_permissao_modificacao( i_seq_lcto_znfw = p_seq_lcto ).
          CATCH zcx_boletim_producao INTO DATA(zcx_bol_prod).
            zcx_bol_prod->published_erro( EXPORTING i_msgty = 'I' i_msgty_display = 'W' ) .
            RETURN.
        ENDTRY.
      ENDIF.

      REFRESH: tg_fields.
      SELECT SINGLE *
        FROM zfiwrt0008
         WHERE seq_lcto EQ p_seq_lcto
           AND loekz EQ space.
      IF sy-subrc IS INITIAL
      AND zfiwrt0008-operacao EQ p_operacao
      AND zfiwrt0008-branch   EQ p_branch
      AND zfiwrt0008-parid    EQ p_parid.

        CLEAR: wl_zib_objkey, wl_zib_chv.
        CONCATENATE 'ZGF' zfiwrt0008-seq_lcto zfiwrt0008-budat(4) INTO wl_zib_objkey.

        SELECT SINGLE *
          FROM zib_contabil_chv
           INTO wl_zib_chv
            WHERE obj_key EQ wl_zib_objkey.

        IF  zfiwrt0008-docnum IS INITIAL AND zfiwrt0008-mblnr  IS INITIAL    AND wl_zib_chv-belnr IS INITIAL.
          CALL FUNCTION 'ENQUEUE_EZFIWRT0008'
            EXPORTING
*             MODE_ZFIWRT0008       = 'E'
*             MANDT          = SY-MANDT
              seq_lcto       = p_seq_lcto
*             X_SEQ_LCTO     = ' '
*             _SCOPE         = '2'
*             _WAIT          = ' '
*             _COLLECT       = ' '
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc NE 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.

            PERFORM trata_campos USING space
                                        'GR1'
                                           c_1       "INPUT 1     NO INPUT 0
                                           c_0.      "INVISIBLE 1 VISIBLE 0
            PERFORM trata_campos USING space
                                      'GR2'
                                       c_0       "INPUT 1     NO INPUT 0
                                       c_0.      "INVISIBLE 1 VISIBLE 0

            IF tg_parc[] IS INITIAL.

            ENDIF.
          ENDIF.
          wg_acao = c_modif.
        ELSE.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'O documento, não pode ser'
                                                      'modificado.'.
        ENDIF.
      ENDIF.
    WHEN c_gerar_nf.
      CLEAR: wl_0008.
      SELECT SINGLE *
        FROM zfiwrt0008
         INTO wl_0008
         WHERE seq_lcto EQ p_seq_lcto.
      IF sy-subrc IS INITIAL.

*        PERFORM F_PREENCHER_DYNPRO USING:
*        'X' 'ZWRR0004'             '0100',
*        ' ' 'P_SEQ_LCTO'           P_SEQ_LCTO,
*        ' ' 'BDC_OKCODE'           'SEARCH'.
*
*        OPT-DISMODE = 'E'.
*        OPT-DEFSIZE = ' '.
*
*        CALL TRANSACTION 'ZNFW0005' USING TL_BDC OPTIONS FROM OPT.
        SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD p_seq_lcto.
        SUBMIT zwrr0004  AND RETURN.
      ENDIF.
    WHEN c_exit.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          seq_lcto = p_seq_lcto.
      wl_doc_ref = p_seq_lcto.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          seq_lcto = wl_doc_ref.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_DG'
CONSTANTS: BEGIN OF c_tab_strip_dg,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_DG_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_DG_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_DG_FC3',
           END OF c_tab_strip_dg.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_DG'
CONTROLS:  tab_strip_dg TYPE TABSTRIP.
DATA: BEGIN OF g_tab_strip_dg,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZWRR0002',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_dg-tab1,
      END OF g_tab_strip_dg.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_STRIP_DG'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
MODULE tab_strip_dg_active_tab_set OUTPUT.
  tab_strip_dg-activetab = g_tab_strip_dg-pressed_tab.
  CASE g_tab_strip_dg-pressed_tab.
    WHEN c_tab_strip_dg-tab1.
      g_tab_strip_dg-subscreen = '0215'.                    "'0211'.
    WHEN c_tab_strip_dg-tab2.
      g_tab_strip_dg-subscreen = '0212'.
    WHEN c_tab_strip_dg-tab3.
      g_tab_strip_dg-subscreen = '0220'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TAB_STRIP_DG_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TAB_STRIP_DG'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tab_strip_dg_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab_strip_dg-tab1.
      g_tab_strip_dg-pressed_tab = c_tab_strip_dg-tab1.
    WHEN c_tab_strip_dg-tab2.
      g_tab_strip_dg-pressed_tab = c_tab_strip_dg-tab2.
    WHEN c_tab_strip_dg-tab3.
      g_tab_strip_dg-pressed_tab = c_tab_strip_dg-tab3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                    "TAB_STRIP_DG_ACTIVE_TAB_GET INPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_IMPOSTOS' ITSELF
CONTROLS: tc_impostos TYPE TABLEVIEW USING SCREEN 0220.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_IMPOSTOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_impostos_change_tc_attr OUTPUT.
  DESCRIBE TABLE tg_impo LINES tc_impostos-lines.
ENDMODULE.                    "TC_IMPOSTOS_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_ITENS' ITSELF
CONTROLS: tc_itens TYPE TABLEVIEW USING SCREEN 0211.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_ITENS'
DATA:     g_tc_itens_lines  LIKE sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_itens_change_tc_attr OUTPUT.
  DESCRIBE TABLE tg_itens LINES tc_itens-lines.
ENDMODULE.                    "TC_ITENS_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_itens_get_lines OUTPUT.
  g_tc_itens_lines = sy-loopc.
ENDMODULE.                    "TC_ITENS_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc_itens_modify INPUT.
  DATA: wl_line      TYPE sy-tabix,
        tl_itens_aux LIKE tg_itens OCCURS 0 WITH HEADER LINE,
        wl_marc      TYPE marc,
        wl_mbew      TYPE mbew,
        wl_1bbranch  TYPE j_1bbranch,
        wl_1baa      TYPE j_1baa,
        wl_1bapn     TYPE  j_1bapn,
        wl_direct    TYPE j_1bapnv-direct,
        wl_dstcat    TYPE j_1bapnv-dstcat.

***> Determina o CFOP
  IF wg_direitos-cfop IS INITIAL.
    SELECT SINGLE *
      FROM marc
      INTO wl_marc
       WHERE matnr EQ tg_itens-matnr.

    IF sy-subrc IS INITIAL.
      tg_itens-steuc  = wl_marc-steuc.

      SELECT SINGLE *
        FROM mbew
        INTO wl_mbew
         WHERE matnr EQ tg_itens-matnr
           AND bwkey EQ tg_itens-werks.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE *
          FROM j_1bbranch
           INTO wl_1bbranch
           WHERE bukrs  EQ p_bukrs
             AND branch EQ p_branch.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE *
            FROM j_1baa
            INTO wl_1baa
             WHERE nftype EQ wg_fiscal-nftype.

          IF wl_1baa-entrad EQ c_x.
            wl_direct = c_1.
          ELSE.
            wl_direct = c_2.
          ENDIF.

          IF wg_direitos-indcoper EQ c_d.
            wl_dstcat = c_0.

          ELSE.
            wl_dstcat = c_1.

          ENDIF.

          SELECT SINGLE *
            FROM j_1bapn
            INTO wl_1bapn
             WHERE direct EQ wl_direct
               AND dstcat EQ wl_dstcat
               AND indus3 EQ wl_marc-indus
               AND itmtyp EQ wg_fiscal-itmtyp
               AND ownpro EQ wl_mbew-ownpr
               AND matuse EQ wl_mbew-mtuse
               AND indus1 EQ wl_1bbranch-industry.

          IF sy-subrc IS INITIAL.
            tg_itens-cfop = wl_1bapn-cfop.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT SINGLE *
      FROM marc
      INTO wl_marc
       WHERE matnr EQ tg_itens-matnr.

    tg_itens-steuc  = wl_marc-steuc.
    tg_itens-cfop = wg_direitos-cfop.
  ENDIF.
***<
***> Total
*  tg_itens-netwr = tg_itens-menge * tg_itens-netpr.
  tg_itens-netwr = ( ( tg_itens-menge * tg_itens-netpr ) + tg_itens-netfre + tg_itens-netins + tg_itens-netoth ) - tg_itens-netdis.
***<

  tl_itens_aux[] = tg_itens[].
  DELETE tl_itens_aux WHERE itmnum IS INITIAL.
  SORT: tl_itens_aux BY itmnum.
  DESCRIBE TABLE tl_itens_aux LINES wl_line.
  READ TABLE tl_itens_aux INDEX wl_line TRANSPORTING itmnum.
***Verifica entrada na tabela de itens
  ADD 10 TO tl_itens_aux-itmnum.
  MOVE: tl_itens_aux-itmnum TO tg_itens-itmnum.

  MODIFY tg_itens
    INDEX tc_itens-current_line.

  IF sy-subrc IS NOT INITIAL.
    tg_itens-fase = icon_display_more.
    APPEND tg_itens.

  ENDIF.
ENDMODULE.                    "TC_ITENS_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_itens_mark INPUT.
  DATA: g_tc_itens_wa2 LIKE LINE OF tg_itens.
  IF tc_itens-line_sel_mode = 1
  AND tg_itens-mark = 'X'.
    LOOP AT tg_itens INTO g_tc_itens_wa2
      WHERE mark = 'X'.
      g_tc_itens_wa2-mark = ''.
      MODIFY tg_itens
        FROM g_tc_itens_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY tg_itens
    INDEX tc_itens-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TC_ITENS_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_ITENS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_itens_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC_ITENS'
                              'TG_ITENS'
                              'MARK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TC_ITENS_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  SET_SUBSCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_subscreen OUTPUT.

ENDMODULE.                 " SET_SUBSCREEN  OUTPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_CONTAB' ITSELF
CONTROLS: tc_contab TYPE TABLEVIEW USING SCREEN 0240.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_CONTAB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_contab_change_tc_attr OUTPUT.
  DESCRIBE TABLE tg_contab LINES tc_contab-lines.
ENDMODULE.                    "TC_CONTAB_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_MOVEST' ITSELF
CONTROLS: tc_movest TYPE TABLEVIEW USING SCREEN 0250.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_MOVEST'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_movest_change_tc_attr OUTPUT.
  DESCRIBE TABLE tg_movest LINES tc_movest-lines.
ENDMODULE.                    "TC_MOVEST_CHANGE_TC_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .
  DATA: wl_0001     TYPE zfiwrt0001,
        tl_0002     TYPE TABLE OF zfiwrt0002 WITH HEADER LINE,
        tl_0003     TYPE TABLE OF zfiwrt0003 WITH HEADER LINE,
        tl_0004     TYPE TABLE OF zfiwrt0004 WITH HEADER LINE,
        tl_0005     TYPE TABLE OF zfiwrt0005 WITH HEADER LINE,
        tl_0006     TYPE TABLE OF zfiwrt0006 WITH HEADER LINE,
        tl_0007     TYPE TABLE OF zfiwrt0007 WITH HEADER LINE,
        wl_0019     TYPE  zfiwrt0019,
        tl_1baj     TYPE TABLE OF j_1baj     WITH HEADER LINE,
        tl_1bajt    TYPE TABLE OF j_1bajt   WITH HEADER LINE,
        tl_tbsl     TYPE TABLE OF tbsl       WITH HEADER LINE,
        tl_skat     TYPE TABLE OF skat       WITH HEADER LINE,
        tl_cskb     TYPE TABLE OF cskb       WITH HEADER LINE,
        tl_user     TYPE TABLE OF user_addr  WITH HEADER LINE,
        wl_kna1     TYPE kna1,
        wl_lfa1     TYPE lfa1,
        wl_t001w    TYPE t001w,
        wl_t001     TYPE t001,
        wl_1bbranch TYPE j_1bbranch,
        wl_1bad     TYPE j_1bad,
        wl_1badt    TYPE j_1badt,
        wl_1baa     TYPE j_1baa.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: lt_returns         TYPE TABLE OF bapiret2,
        ls_coeldes         TYPE bapi1030_ceoutputlist,
        ls_cskb            LIKE LINE OF tl_cskb,
        lv_controllingarea TYPE  bapi1030_gen-co_area,
        lv_costelement     TYPE  bapi1030_gen-cost_elem,
        lv_keydate         TYPE  bapi1030_gen-some_date.
* <--- S4 Migration - 18/07/2023 - CA

  REFRESH: tl_0002, tl_0003, tl_0004, tl_0005, tl_0006, tl_1baj, tl_1bajt, tl_tbsl,
           tl_skat, tl_cskb.
  CLEAR: wl_0001, tl_0002, tl_0003, tl_0004, tl_0005, tl_0006, tl_1baj, tl_1bajt,
         wl_kna1, wl_lfa1, wl_t001w, tl_tbsl, tl_skat, tl_cskb, wl_1baa.

  p_parid = |{ p_parid ALPHA = IN }|.

  SELECT SINGLE *
  FROM zfiwrt0008
  INTO wl_0008
   WHERE seq_lcto EQ p_seq_lcto.
  IF sy-subrc IS INITIAL.
    CONCATENATE 'Ao atualizar os campos, você pode perder as informacões originais'
                ' do documento, têm certeza que deseja atualizar?' INTO msg.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-006
        text_question         = msg
        text_button_1         = 'Sim'
        icon_button_1         = '@0V@'
        text_button_2         = 'Não'
        icon_button_2         = '@0W@'
        default_button        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = resposta.
    IF resposta EQ c_1.

    ELSE.
      IF v_automatico_memo = abap_false.
        LEAVE TO SCREEN 100.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.
  ENDIF.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF p_operacao <> p_operacao_old.
    FREE: wg_direitos-taxlw1,
          wg_direitos-taxlw2,
          wg_direitos-taxlw4,
          wg_direitos-taxlw5,
          tg_impo_comp.
  ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

  IF p_bukrs IS NOT INITIAL.
    SELECT COUNT(*)
      FROM t001
       WHERE bukrs EQ p_bukrs.
    IF sy-subrc IS NOT INITIAL.
      IF v_automatico_memo = abap_false.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Cód. de Empresa não existe!'.
        LEAVE TO SCREEN 100.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM t001w
    INTO wl_t001w
     WHERE werks EQ p_branch.

  IF p_operacao IS NOT INITIAL.
    SELECT SINGLE *
      FROM zfiwrt0001
      INTO wl_0001
       WHERE operacao EQ p_operacao.

    IF sy-subrc IS INITIAL.

      w_zfiwrt0001 = wl_0001.  "*-CS2023000043-09.02.2023-#102019-JT

      IF ( wl_0001-disp_nf_cct = 'S' ) AND ( p_seq_lcto IS NOT INITIAL ).
        SELECT SINGLE *
          FROM zfiwrt0008 INTO @DATA(_wl_0008)
         WHERE seq_lcto EQ @p_seq_lcto.

        IF ( sy-subrc NE 0 ) AND ( wg_sugere_transp EQ abap_true ) AND ( p_operacao = '0019' ). "Form. Lote Produto Acabado Itacoatiara
          CLEAR: wg_sugere_transp.
          wg_transporte-lifnr = '0000001002'.
        ENDIF.
      ENDIF.

      SELECT SINGLE *
        FROM j_1baa
        INTO wl_1baa
         WHERE nftype EQ wl_0001-nftype.

      IF wl_0001-parvw EQ c_ag.
        SELECT SINGLE *
          FROM kna1
          INTO wl_kna1
           WHERE kunnr EQ p_parid.

      ELSEIF wl_0001-parvw EQ c_br
       OR   wl_0001-parvw EQ c_lf.
        SELECT SINGLE *
          FROM lfa1
          INTO wl_lfa1
           WHERE lifnr EQ p_parid.

      ENDIF.
      IF wl_0001-lm_estoque = 'S'.
        MOVE: wl_0001-descricao TO wg_desc_operacao.
      ELSE.
        SELECT *
          FROM zfiwrt0002
          INTO TABLE tl_0002
           WHERE operacao EQ p_operacao.

        IF sy-subrc IS INITIAL.
          MOVE: wl_0001-descricao TO wg_desc_operacao.

          SELECT *
            FROM j_1baj
            INTO TABLE tl_1baj
             FOR ALL ENTRIES IN tl_0002
             WHERE taxtyp EQ tl_0002-taxtyp.

          SELECT *
            FROM j_1bajt
            INTO TABLE tl_1bajt
             FOR ALL ENTRIES IN tl_0002
            WHERE  spras  EQ sy-langu
              AND  taxtyp EQ tl_0002-taxtyp.
        ELSE.
          CLEAR: wg_desc_operacao.
          IF v_automatico_memo = abap_false.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Cód. de Operação não existe!'.
            LEAVE TO SCREEN 100.
          ELSE.
            LEAVE TO SCREEN 0.
          ENDIF.
        ENDIF.
      ENDIF.

      SELECT *
        FROM zfiwrt0003
        INTO TABLE tl_0003
         WHERE operacao EQ p_operacao.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM tbsl
          INTO TABLE tl_tbsl
           FOR ALL ENTRIES IN tl_0003
           WHERE bschl EQ tl_0003-bschl.

        SELECT *
          FROM skat
          INTO TABLE tl_skat
           FOR ALL ENTRIES IN tl_0003
            WHERE spras EQ sy-langu
              AND ktopl EQ c_50
              AND saknr EQ tl_0003-hkont.

* ---> S4 Migration - 18/07/2023 - CA
*        SELECT *
*        FROM cskb
*        INTO TABLE tl_cskb
*         FOR ALL ENTRIES IN tl_0003
*          WHERE kstar EQ tl_0003-hkont
*            AND  ( datbi GE sy-datum
*              AND datab LE sy-datum )
*            AND katyp EQ '01'.

        "Seleção não possuía filtro por controlling; selecionar todos
        SELECT kokrs
          FROM tka01
          INTO TABLE @DATA(tl_tka01)
          WHERE kokrs <> @space.
        IF sy-subrc IS INITIAL.

          LOOP AT tl_tka01 INTO DATA(ls_tka01).

            lv_controllingarea  = ls_tka01-kokrs.

            LOOP AT tl_0003 INTO DATA(ls_0003).

              lv_costelement      = ls_0003-hkont.
              lv_keydate          = sy-datum.

              CLEAR: lt_returns[], ls_coeldes.

              CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
                EXPORTING
                  controllingarea   = lv_controllingarea
                  costelement       = lv_costelement
                  keydate           = lv_keydate
                IMPORTING
                  costelementdetail = ls_coeldes
                TABLES
                  return            = lt_returns.

              READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
              IF sy-subrc <> 0 AND
                 ls_coeldes-celem_category = '01'.

                ls_cskb-kokrs = ls_tka01-kokrs.
                ls_cskb-kstar = ls_0003-hkont.
                ls_cskb-katyp = ls_coeldes-celem_category.

                APPEND ls_cskb TO tl_cskb.
                CLEAR ls_cskb.
              ENDIF.

              CLEAR: ls_0003.
            ENDLOOP.

            CLEAR: ls_tka01.
          ENDLOOP.

        ENDIF.
* <--- S4 Migration - 18/07/2023 - CA
      ENDIF.
      SELECT *
      FROM zfiwrt0004
      INTO TABLE tl_0004
       WHERE operacao EQ p_operacao.

      SELECT *
        FROM zfiwrt0005
        INTO TABLE tl_0005
         WHERE operacao EQ p_operacao.

      SELECT *
        FROM zfiwrt0006
        INTO TABLE tl_0006
         WHERE operacao EQ p_operacao.

      SELECT *
        FROM zfiwrt0007
        INTO TABLE tl_0007
         WHERE operacao EQ p_operacao
           AND branch   EQ p_branch
           AND tipo     EQ c_w.
**
      IF tl_0007[] IS NOT INITIAL.
        SELECT *
          FROM user_addr
          INTO TABLE tl_user
           FOR ALL ENTRIES IN tl_0007
            WHERE bname EQ tl_0007-usnam.

      ENDIF.

      SELECT SINGLE *
        FROM zfiwrt0019
        INTO wl_0019
     WHERE seq_lcto EQ p_seq_lcto.

      PERFORM preenche_campos TABLES tl_0002
                                     tl_0003
                                     tl_0004
                                     tl_0005
                                     tl_0006
                                     tl_0007
                                     tl_1baj
                                     tl_1bajt
                                     tl_tbsl
                                     tl_skat
                                     tl_cskb
                                     tl_user
                              USING  wl_0001
                                     wl_kna1
                                     wl_lfa1
                                     wl_t001w
                                     wl_1baa
                                     wl_0019.

      PERFORM z_sugere_itens.
    ELSE.
      IF v_automatico_memo = abap_false.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Cód. de operação nao existe!'.
        LEAVE TO SCREEN 100.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.

  ELSE.
    IF v_automatico_memo = abap_false.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Cód. de operação nao existe!'.
      LEAVE TO SCREEN 100.
    ELSE.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

  p_operacao_old = p_operacao.  "*-CS2023000043-09.02.2023-#102019-JT

*  ELSE.
*    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'É preciso preencher todos os campos'
*                                           'obrigatorios!'.
*  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_0001  text
*      -->P_TL_0002  text
*      -->P_TL_0003  text
*      -->P_TL_0004  text
*      -->P_TL_0005  text
*      -->P_TL_0006  text
*      -->P_TL_1BAJ  text
*      -->P_TL_1BAJT  text
*----------------------------------------------------------------------*
FORM preenche_campos  TABLES   tl_0002  STRUCTURE zfiwrt0002
                               tl_0003  STRUCTURE zfiwrt0003
                               tl_0004  STRUCTURE zfiwrt0004
                               tl_0005  STRUCTURE zfiwrt0005
                               tl_0006  STRUCTURE zfiwrt0006
                               tl_0007  STRUCTURE zfiwrt0007
                               tl_1baj  STRUCTURE j_1baj
                               tl_1bajt STRUCTURE j_1bajt
                               tl_tbsl  STRUCTURE tbsl
                               tl_skat  STRUCTURE skat
                               tl_cskb  STRUCTURE cskb
                               tl_user  STRUCTURE user_addr
                      USING    wl_0001 TYPE zfiwrt0001
                               wl_kna1 TYPE kna1
                               wl_lfa1 TYPE lfa1
                               wl_t001w TYPE t001w
                               wl_1baa TYPE j_1baa
                               wl_0019 TYPE zfiwrt0019.


  DATA: wl_indcoper         TYPE zfiwrt0006-indcoper,
        wl_texto_fiscal(30),
        tl_values           TYPE vrm_values,
        wl_values           TYPE LINE OF vrm_values,
        wl_cont             TYPE sy-tabix,
        wl_cont_aux         TYPE sy-tabix,
        wl_cont_aux2        TYPE sy-tabix,
        wl_tab_lin          TYPE sy-tabix.

  DATA: lv_parid_key    TYPE lfa1-lifnr.

** Preenche valores da tela.
  IF wl_0001-opr_blq NE c_l.
    IF v_automatico_memo = abap_false.

*=#134309 - BUG DUMP - 09.04.2024 - JAIME - inicio ====================
*      IF v_automatico_memo = abap_false.
      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.
*      ENDIF.
      DESCRIBE TABLE node_itab LINES wl_tab_lin.
      IF wl_tab_lin LT 2.
        REFRESH: node_itab.
        IF tree IS NOT INITIAL.
*          IF v_automatico_memo = abap_false. "BUG SOLTO 134309 / AOENNING / DUMP
          CALL METHOD tree->delete_all_nodes.
*           PERFORM preenche_tree USING 'Root'
*                                  space
*                                  c_x
*                                  space
*                                  'Mensagens da Nota'
*                                  space.

          PERFORM preenche_tree USING c_root
                                   space
                                   c_x
                                   space
                                   'Mensagens da Nota'
                                   space.
*          ENDIF.
        ENDIF.
      ENDIF.
*=#134309 - BUG DUMP - 09.04.2024 - JAIME - fim    ====================

      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Operação bloqueada para uso!'.
    ELSE.
      LEAVE TO SCREEN 0.
    ENDIF.
  ELSE.
***Dados Gerais
****Header
    MOVE: wl_0001-nftype        TO wg_fiscal-nftype,
          wl_0001-itmtyp        TO wg_fiscal-itmtyp,
          wl_0001-parvw         TO p_parvw,
          wl_0001-dias          TO wg_fiscal-dias,
          wl_0001-retorno       TO wg_fiscal-retorno,
          wl_0001-zpesagem      TO wg_fiscal-zpesagem,
          wl_0001-imobilizado   TO wg_fiscal-imobilizado,
          wl_0001-tp_mv_imob    TO wg_fiscal-tp_mv_imob,
          wl_0001-ctrl_zrfl     TO wg_fiscal-ctrl_zrfl,
          wl_0001-energia       TO wg_fiscal-energia,
          wl_0001-servico       TO wg_fiscal-servico,
          wl_0001-disp_nf_cct   TO wg_fiscal-disp_nf_cct,
          wl_0001-transf_icms   TO wg_fiscal-transf_icms,
          wl_0001-complemento   TO wg_fiscal-complemento,
          wl_0001-aviso_rec     TO wg_fiscal-aviso_rec,
          wl_0001-lm_estoque    TO wg_fiscal-lm_estoque.


    PERFORM f_define_origem_destino USING wl_kna1
                                          wl_lfa1
                                          wl_t001w
                                          wl_1baa
                                 CHANGING wl_indcoper
                                          wl_texto_fiscal.

**  Busca texto de tipo de operação
    CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
      EXPORTING
        i_table_name = 'ZFIWRT0006'
        i_field_name = 'OPERTYP'
      IMPORTING
        e_t_list     = tl_values.

    READ TABLE tl_0006
      WITH KEY indcoper = wl_indcoper.

    MOVE: tl_0006-cfop     TO wg_direitos-cfop,
*         tl_0006-taxlw1   TO wg_direitos-taxlw1,  "*-CS2023000043-09.02.2023-#102019-JT
*         tl_0006-taxlw2   TO wg_direitos-taxlw2,  "*-CS2023000043-09.02.2023-#102019-JT
*         tl_0006-taxlw4   TO wg_direitos-taxlw4,  "*-CS2023000043-09.02.2023-#102019-JT
*         tl_0006-taxlw5   TO wg_direitos-taxlw5,  "*-CS2023000043-09.02.2023-#102019-JT
          tl_0006-taxcode  TO wg_direitos-taxcode,
          tl_0006-indcoper TO wg_direitos-indcoper,
          tl_0006-opertyp  TO wg_direitos-opertyp.

*-CS2023000043-09.02.2023-#102019-JT-inicio
    IF wg_direitos-taxlw1 IS INITIAL.
      MOVE tl_0006-taxlw1  TO wg_direitos-taxlw1.
    ENDIF.
    IF wg_direitos-taxlw2 IS INITIAL.
      MOVE tl_0006-taxlw2  TO wg_direitos-taxlw2.
    ENDIF.
    IF wg_direitos-taxlw4 IS INITIAL.
      MOVE tl_0006-taxlw4  TO wg_direitos-taxlw4.
    ENDIF.
    IF wg_direitos-taxlw5 IS INITIAL.
      MOVE tl_0006-taxlw5  TO wg_direitos-taxlw5.
    ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

    READ TABLE tl_values INTO wl_values
      WITH KEY key = tl_0006-opertyp.

    CONCATENATE wl_texto_fiscal '-' wl_values-text INTO wg_op_fiscal SEPARATED BY space.
***      descricao da operacao.
    REFRESH: tg_editor.
    CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
    wl_cont = strlen( wl_0001-txt_compl ).
    wl_cont_aux = wl_cont / 72.

    DO.
      MOVE: wl_0001-txt_compl+wl_cont_aux2 TO wg_editor-line.
      ADD 72 TO wl_cont_aux2.
      APPEND wg_editor TO tg_editor.

      IF wl_cont_aux2 GT wl_cont.
        EXIT.

      ENDIF.
    ENDDO.

    IF v_automatico_memo = abap_false.
      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.
    ENDIF.
***Impostos
    IF wg_docs-docnum IS INITIAL.
      REFRESH: tg_impo.
      LOOP AT tl_0002.
        READ TABLE tl_1baj
          WITH KEY taxtyp = tl_0002-taxtyp.

        READ TABLE tl_1bajt
          WITH KEY taxtyp = tl_0002-taxtyp.

        MOVE: tl_0002-taxtyp   TO tg_impo-taxtyp,
              tl_1bajt-ttypetxt TO tg_impo-ttypetxt,
              tl_1baj-taxgrp  TO tg_impo-taxgrp.

        IF tl_0002-taxtyp EQ c_icm3.
          IF wg_direitos-opertyp EQ c_t.

          ELSEIF wg_direitos-opertyp EQ c_i.

          ELSEIF wg_direitos-opertyp EQ c_n.

          ENDIF.
        ELSE.

        ENDIF.

        APPEND tg_impo.
        CLEAR: tg_impo.
      ENDLOOP.
    ENDIF.

***Contabilidade
    IF wg_docs-belnr IS INITIAL.
      REFRESH: tg_contab, tg_contab-style2, style2.
      LOOP AT tl_0003.
*        where estorno is initial.
        READ TABLE tl_skat
          WITH KEY saknr = tl_0003-hkont.

        READ TABLE tl_tbsl
          WITH KEY bschl = tl_0003-bschl.

        READ TABLE tl_cskb
           WITH KEY kstar = tl_0003-hkont.

        IF sy-subrc IS NOT INITIAL.
*        and tl_0003-estorno is not initial.
          wa_style-fieldname = 'KOSTL'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style2.
        ELSE.
          IF tl_0003-estorno IS NOT INITIAL.
            wa_style-fieldname = 'KOSTL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style2.
          ENDIF.
        ENDIF.

        IF tl_tbsl-koart EQ c_k
        OR tl_tbsl-koart EQ c_d.

          SELECT SINGLE *
            FROM j_1bad INTO @DATA(lwa_j_1bad)
           WHERE parvw EQ @p_parvw.

          IF sy-subrc EQ 0.

            lv_parid_key = p_parid.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lv_parid_key
              IMPORTING
                output = lv_parid_key.

            CASE lwa_j_1bad-partyp.
              WHEN 'C'.

                SELECT SINGLE *
                  FROM kna1 INTO @DATA(_wl_kna1)
                 WHERE kunnr = @lv_parid_key.

                IF sy-subrc EQ 0.
                  IF tl_tbsl-koart = c_d.
                    MOVE p_parid        TO tg_contab-hkont.
                  ELSEIF tl_tbsl-koart = c_k.
                    MOVE _wl_kna1-lifnr TO tg_contab-hkont.
                  ENDIF.
                ENDIF.

              WHEN 'B' OR 'V'.

                SELECT SINGLE *
                  FROM lfa1 INTO @DATA(_wl_lfa1)
                 WHERE lifnr = @lv_parid_key.

                IF sy-subrc EQ 0.
                  IF tl_tbsl-koart = c_k.
                    MOVE p_parid        TO tg_contab-hkont.
                  ELSEIF tl_tbsl-koart = c_d.
                    MOVE _wl_lfa1-kunnr TO tg_contab-hkont.
                  ENDIF.
                ENDIF.

            ENDCASE.


            IF tl_0003-estorno IS INITIAL.
*              wa_style-fieldname = 'ZFBDT'.
*              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*              insert  wa_style into table style2.
*
*              wa_style-fieldname = 'ZLSCH'.
*              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*              insert  wa_style into table style2.
*              insert lines of style2 into table tg_contab-style2.
            ELSE.
              wa_style-fieldname = 'ZFBDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style2.

              wa_style-fieldname = 'ZLSCH'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style2.
            ENDIF.

          ENDIF.



        ELSE.
          MOVE: tl_0003-hkont   TO tg_contab-hkont.

          wa_style-fieldname = 'ZFBDT'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style2.

          wa_style-fieldname = 'ZLSCH'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style2.
        ENDIF.
        MOVE: tl_0003-bschl   TO tg_contab-bschl,
*            tl_0003-hkont   TO tg_contab-hkont,
              tl_skat-txt50   TO tg_contab-txt50,
              tl_0003-taxtyp  TO tg_contab-taxtyp,
              tl_0003-estorno TO tg_contab-estorno,
              tl_0003-newbw   TO tg_contab-newbw,
              tl_0003-umskz   TO tg_contab-umskz.

        INSERT LINES OF style2 INTO TABLE tg_contab-style2.
        APPEND tg_contab.
        CLEAR: tg_contab.
        REFRESH: style2.
      ENDLOOP.

      tg_tbsl[] = tl_tbsl[].
    ENDIF.

***Movimentacao de estoque
    IF wg_docs-mblnr IS INITIAL.
      REFRESH: tg_movest.
      LOOP AT tl_0004.
        MOVE: tl_0004-bwart  TO tg_movest-bwart,
              tl_0004-tcode  TO tg_movest-tcode,
              tl_0004-mwskz1 TO tg_movest-mwskz1,
              tl_0004-estorno TO tg_movest-estorno.

*            tl_0004-lgort  TO tg_movest-lgort.

        APPEND tg_movest.
        CLEAR: tg_movest.
      ENDLOOP.
    ENDIF.

*=#134309 - BUG DUMP - 09.04.2024 - JAIME - inicio ====================
*    IF wg_docs-docnum IS INITIAL
*    AND wg_docs-belnr IS INITIAL
*    AND wg_docs-mblnr IS INITIAL
*    AND wg_docs-nfenum IS INITIAL.
*=#134309 - BUG DUMP - 09.04.2024 - JAIME - fim    ====================
    DESCRIBE TABLE node_itab LINES wl_tab_lin.
    IF wl_tab_lin LT 2.
      REFRESH: node_itab.
      IF tree IS NOT INITIAL.
        IF v_automatico_memo = abap_false.
          CALL METHOD tree->delete_all_nodes.
*           PERFORM preenche_tree USING 'Root'
*                                  space
*                                  c_x
*                                  space
*                                  'Mensagens da Nota'
*                                  space.

          PERFORM preenche_tree USING c_root
                                   space
                                   c_x
                                   space
                                   'Mensagens da Nota'
                                   space.
        ENDIF.
        SORT tl_0005 BY seqnum linnum.
        LOOP AT tl_0005.
          ON CHANGE OF tl_0005-seqnum.
            IF v_automatico_memo = abap_false.
              PERFORM preenche_tree USING tl_0005-seqnum
                                          c_root
                                          space
                                          cl_gui_simple_tree=>relat_last_child
                                          tl_0005-message
                                          handle_tree.
            ENDIF.
          ENDON.
          tg_mensagems-seqnum  = tl_0005-seqnum.
          tg_mensagems-linnum  = tl_0005-linnum.
          tg_mensagems-message = tl_0005-message.
          APPEND tg_mensagems.         " msgs q foram parametrizadas na operacao
          APPEND tg_mensagems TO tg_mensagems_aux.  " tabela q iram conter todas as msgs
          " inclusive as adcionadas na criacao da NF
        ENDLOOP.

        IF tg_mensagems_lim IS NOT INITIAL.
          APPEND LINES OF tg_mensagems_lim[] TO tg_mensagems_aux[].
        ENDIF.
      ENDIF.
*    CALL METHOD tree->add_nodes
*      EXPORTING
*        node_table           = node_itab
*        table_structure_name = 'NODE_STR'.



*
*      PERFORM preenche_tree USING wl_lines
*                                  'Root'
*                                  space
*                                  cl_gui_simple_tree=>relat_last_child
*                                  space
*                                  handle_tree.
*
*
*      CALL METHOD tree->add_nodes
*        EXPORTING
*          node_table           = node_itab
*          table_structure_name = 'NODE_STR'.
    ENDIF.
    REFRESH: tg_aprov.
    LOOP AT tl_0007.
      READ TABLE tl_user
       WITH KEY bname = tl_0007-usnam.

      MOVE: tl_0007-nivel_aprov  TO tg_aprov-nivel_aprov,
            tl_0007-usnam        TO tg_aprov-usnam,
            tl_0007-departamento TO tg_aprov-departamento.

      CONCATENATE tl_user-name_first tl_user-name_last INTO tg_aprov-nome SEPARATED BY space.

      APPEND tg_aprov.
      CLEAR: tg_aprov.
    ENDLOOP.

** Parceiros da Nota Fiscal
    REFRESH: tg_parc, tg_parc-style, style.
    IF p_parvw IS NOT INITIAL
    AND p_parid IS NOT INITIAL.
      tg_parc-parvw = p_parvw.
      tg_parc-parid = p_parid.
      IF tg_parc-parvw EQ c_ag.
        tg_parc-nome  = wl_kna1-name1.    "wg_desc_parid.

      ELSEIF tg_parc-parvw EQ c_lf
          OR tg_parc-parvw EQ c_br.
        tg_parc-nome  = wl_lfa1-name1.  "wg_desc_parid.

      ENDIF.
      wa_style-fieldname = 'PARVW'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  wa_style INTO TABLE style .
      wa_style-fieldname = 'PARID'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*          APPEND WA_STYLE TO STYLE.
      INSERT  wa_style INTO TABLE style .
      INSERT LINES OF style INTO TABLE tg_parc-style.
    ENDIF.
    APPEND tg_parc.
    REFRESH: style.
*    ENDIF.  "*=#134309 - BUG DUMP - 09.04.2024 - JAIME - ====================

  ENDIF.

  "Transporte
  IF NOT ( wl_0019 IS INITIAL ).
    MOVE: wl_0019-lifnr  TO wg_transporte-lifnr,
          wl_0019-placa  TO wg_transporte-placa,
          wl_0019-anzpk  TO wg_transporte-anzpk,
          wl_0019-shpunt TO wg_transporte-shpunt,
          wl_0019-ntgew  TO wg_transporte-ntgew,
          wl_0019-brgew  TO wg_transporte-brgew,
          wl_0019-ufplaca TO wg_transporte-ufplaca,

          wl_0019-placa_car1 TO wg_transporte-placa_car1,
          wl_0019-placa_car2 TO wg_transporte-placa_car2,
          wl_0019-placa_car3 TO wg_transporte-placa_car3,
          wl_0019-motorista  TO wg_transporte-motorista.
  ENDIF.


ENDFORM.                    " PREENCHE_CAMPOS
*&---------------------------------------------------------------------*
*&      Module  VALIDA_PARAMETROS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_parametros INPUT.
  REFRESH: tg_itens.
  MOVE: c_search TO ok-code.

ENDMODULE.                 " VALIDA_PARAMETROS  OUTPUT
MODULE valida_parametros2 INPUT.
  REFRESH: tg_itens.

ENDMODULE.                 " VALIDA_PARAMETROS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit INPUT.
  CASE ok-code.
    WHEN c_back.
      SET SCREEN 0.

    WHEN c_exit.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_oper INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.
  DATA: BEGIN OF t_fieldtab OCCURS 3.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF t_fieldtab.

  DATA: BEGIN OF tl_operacao OCCURS 0,
          operacao  TYPE zfiwrt0001-operacao,
          descricao TYPE zfiwrt0001-descricao,
        END OF tl_operacao.

  REFRESH: tl_operacao, t_fieldtab.
  CLEAR:   tl_operacao, t_fieldtab.

  SELECT operacao descricao
    FROM zfiwrt0001
    INTO TABLE tl_operacao
     WHERE opr_blq EQ c_l.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OPERACAO'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'P_OPERACAO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_operacao
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.                 " SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PARID  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_parid INPUT.
  DATA: BEGIN OF dynpfields OCCURS 1.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF dynpfields.

* Parameter für F4IF_FIELD_VALUE_REQUEST
  DATA: mc_obj LIKE help_info-mcobj.
  DATA: return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.
  DATA: da_display TYPE c.

  REFRESH:dynpfields.

  MOVE 'P_PARVW'  TO dynpfields-fieldname.
  APPEND dynpfields.
  MOVE 'P_BUKRS'  TO dynpfields-fieldname.
  APPEND dynpfields.
  MOVE 'P_PARID'  TO dynpfields-fieldname.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname                   = sy-repid
      dynumb                   = sy-dynnr
      perform_input_conversion = 'X'
    TABLES
      dynpfields               = dynpfields
    EXCEPTIONS
      invalid_abapworkarea     = 1
      invalid_dynprofield      = 2
      invalid_dynproname       = 3
      invalid_dynpronummer     = 4
      invalid_request          = 5
      no_fielddescription      = 6
      invalid_parameter        = 7
      undefind_error           = 8
      OTHERS                   = 9.

  READ TABLE dynpfields
      WITH KEY fieldname = 'P_PARVW'.

  IF p_parvw EQ c_ag
  OR dynpfields-fieldvalue EQ c_ag.

    READ TABLE dynpfields
       WITH KEY fieldname = 'P_PARID'.
*   Matchcodeobjekt aufrufen
    mc_obj = 'C_KUNNR'.
    IF dynpfields-fieldinp EQ space.
      da_display = c_x. "CHARX.
    ENDIF.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = space
        fieldname         = space
        searchhelp        = mc_obj
        dynprofield       = 'X'
        value             = dynpfields-fieldvalue
        display           = da_display
      TABLES
        return_tab        = return_values
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND
       dynpfields-fieldinp NE space.
      p_parid = return_values-fieldval.
    ENDIF.

  ELSEIF p_parvw EQ c_br
    OR   p_parvw EQ c_lf
    OR dynpfields-fieldvalue EQ c_br
    OR dynpfields-fieldvalue EQ c_lf.

    READ TABLE dynpfields
       WITH KEY fieldname = 'P_PARID'.
*   Matchcodeobjekt aufrufen
    mc_obj = 'KRED_C'.
    IF dynpfields-fieldinp EQ space.
      da_display = c_x. "CHARX.
    ENDIF.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = space
        fieldname         = space
        searchhelp        = mc_obj
        dynprofield       = 'X'
        value             = dynpfields-fieldvalue
        display           = da_display
      TABLES
        return_tab        = return_values
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND
       dynpfields-fieldinp NE space.
      p_parid = return_values-fieldval.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SEARCH_PARID  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_descricoes .

*-CS2023000043-09.02.2023-#102019-JT-inicio
  CLEAR: wg_desc_taxlw1,
         wg_desc_taxlw2,
         wg_desc_taxlw4,
         wg_desc_taxlw5.
*-CS2023000043-09.02.2023-#102019-JT-fim

  IF wg_fiscal-nftype IS NOT INITIAL.
    SELECT SINGLE nfttxt
      FROM j_1baat
      INTO wg_desc_nftype
       WHERE spras  EQ sy-langu
         AND nftype EQ wg_fiscal-nftype.

  ENDIF.

  IF wg_fiscal-itmtyp IS NOT INITIAL.
    SELECT SINGLE text
      FROM j_1bitemtypest
      INTO wg_desc_itmtyp
       WHERE spras  EQ sy-langu
         AND itmtyp EQ wg_fiscal-itmtyp.

  ENDIF.

  IF wg_direitos-cfop IS NOT INITIAL.
    SELECT SINGLE cfotxt
     FROM j_1bagnt
     INTO wg_desc_cfop
      WHERE spras  EQ sy-langu
        AND cfop EQ wg_direitos-cfop.
  ENDIF.

  IF wg_direitos-taxlw1 IS NOT INITIAL.
    SELECT SINGLE descrip
     FROM j_1batl1t
     INTO wg_desc_taxlw1
      WHERE langu  EQ sy-langu
        AND taxlaw EQ wg_direitos-taxlw1.
  ENDIF.

  IF wg_direitos-taxlw2 IS NOT INITIAL.
    SELECT SINGLE descrip
     FROM j_1batl2t
     INTO wg_desc_taxlw2
      WHERE langu  EQ sy-langu
        AND taxlaw EQ wg_direitos-taxlw2.
  ENDIF.

  IF wg_direitos-taxlw4 IS NOT INITIAL.
    SELECT SINGLE descrip
     FROM j_1batl4t
     INTO wg_desc_taxlw4
      WHERE langu  EQ sy-langu
        AND taxlaw EQ wg_direitos-taxlw4.
  ENDIF.

  IF wg_direitos-taxlw5 IS NOT INITIAL.
    SELECT SINGLE descrip
     FROM j_1batl5t
     INTO wg_desc_taxlw5
      WHERE langu  EQ sy-langu
        AND taxlaw EQ wg_direitos-taxlw5.
  ENDIF.

  IF wg_direitos-taxcode IS NOT INITIAL.
    SELECT SINGLE txt
     FROM j_1btxsdct
     INTO wg_desc_taxcode
      WHERE langu   EQ sy-langu
        AND taxcode EQ  wg_direitos-taxcode.
  ENDIF.
ENDFORM.                    " BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*&      Module  VALIDA_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_material INPUT.
  DATA: wl_mara TYPE mara,
        wl_makt TYPE makt.
  IF tg_itens-matnr IS NOT INITIAL.
    SELECT SINGLE *
      FROM mara
      INTO wl_mara
       WHERE matnr EQ tg_itens-matnr.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE maktx
        FROM makt
        INTO tg_itens-maktx
         WHERE spras EQ sy-langu
           AND matnr EQ tg_itens-matnr.

    ELSE.
      MESSAGE e836(sd) WITH 'Número do material não existe!'.
    ENDIF.
  ENDIF.
ENDMODULE.                 " VALIDA_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDA_CENTRO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_centro INPUT.
  DATA: wl_t001w TYPE t001w.

  IF tg_itens-werks IS NOT INITIAL.
    SELECT SINGLE *
      FROM t001w
      INTO wl_t001w
       WHERE werks EQ tg_itens-werks.

    IF sy-subrc IS INITIAL.
*      select *
    ELSE.
      MESSAGE e836(sd) WITH 'Centro/Filial não encontrado.'.
    ENDIF.
  ELSE.
  ENDIF.
ENDMODULE.                 " VALIDA_CENTRO  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALL_SUBCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE call_subcreen INPUT.
  CASE sy-ucomm.
    WHEN c_dg1.
      IF vg_subscreen1 EQ c_dummy_header.
        vg_subscreen1 = '213'.
        wg_dg1 = c_minimizar.
      ELSE.
        vg_subscreen1 = c_dummy_header.
        wg_dg1 = c_maximizar.
      ENDIF.
    WHEN c_dg2.
      IF vg_subscreen2 EQ c_dummy_itens.
        vg_subscreen2 = '214'.
        wg_dg2 = c_minimizar.
      ELSE.
        vg_subscreen2 = c_dummy_itens.
        wg_dg2 = c_maximizar.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " CALL_SUBCREEN  INPUT
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
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.
  DATA: waref      TYPE REF TO data.

  DATA: wa_obj  TYPE borident,
        ip_mode TYPE sgs_rwmod.

  "workflow documentos
  IF manager IS NOT INITIAL.
    CALL METHOD manager->unpublish.
    CLEAR: manager.
  ENDIF.


  IF it_seq_lcto[] IS NOT INITIAL.
    READ TABLE it_seq_lcto INDEX 1.
    wa_obj-objtype = 'ZWRR0002'.
    CONCATENATE sy-mandt it_seq_lcto-seq_lcto INTO wa_obj-objkey.

    IF gf_authorization_ft_09 EQ abap_true.
      ip_mode = 'E'.
    ELSE.
      ip_mode = 'D'.
    ENDIF.

    CREATE OBJECT manager
      EXPORTING
        is_object        = wa_obj
        ip_no_commit     = 'R'
        ip_mode          = ip_mode
      EXCEPTIONS
        object_invalid   = 1
        callback_invalid = 2
        OTHERS           = 3.

    SET TITLEBAR 'TLWORK' WITH p_seq_lcto.

  ELSE.
*    IF SY-TCODE EQ C_DISPLAY.
*      SET PF-STATUS 'Z002' EXCLUDING FCODE.
*    ELSE.
*      SET PF-STATUS 'Z001' EXCLUDING FCODE.
*    ENDIF.
  ENDIF.

  IF g_custom_container IS INITIAL.
*    WA_LAYOUT-CWIDTH_OPT = C_X.
    wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
    wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-COL_OPT    = C_X.
    wa_stable-row        = c_x.

    wa_layout-stylefname = 'STYLE2'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
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
*-CS2020001331 - 06.10.2021 - JT - inicio
    PERFORM f_dropdown_table.
*-CS2020001331 - 06.10.2021 - JT - fim

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.
    SET HANDLER lcl_event_handler=>on_data_changed FOR grid1.

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

*BUG 75507 - CBRAND - Inicio
    grid1->activate_display_protocol( space ).
*BUG 75507 - CBRAND - Fim

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_itens[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_hotspot_click FOR grid1,
              lcl_event_handler=>on_double_click FOR grid1,
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_data_changed FOR grid1.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.



  ELSE.
    wa_layout-stylefname = 'STYLE2'.
*    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*      IMPORTING
*        ET_FIELDCATALOG = T_FIELDCATALOG[].
    PERFORM montar_layout.
    IF wg_docs-docnum IS INITIAL.
      READ TABLE tg_fields TRANSPORTING NO FIELDS
        WITH KEY group1 = 'GR1'.
      IF sy-subrc IS INITIAL.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
          WHERE fieldname EQ 'MATNR'
             OR fieldname EQ 'CHARG'
             OR fieldname EQ 'WERKS'
             OR fieldname EQ 'LGORT'
             OR fieldname EQ 'NETPR'
             OR fieldname EQ 'MENGE'
             OR fieldname EQ 'MEINS'
             OR fieldname EQ 'ANLN1'
             OR fieldname EQ 'ANLN2'
             OR fieldname EQ 'VBELN'
             OR fieldname EQ 'POSNR'
             OR fieldname EQ 'MEINS'
             OR fieldname EQ 'RENAS'
             OR fieldname EQ 'POSSUI_ICMS_ST'.

          IF wg_fiscal-retorno EQ 'S'.
            IF p_parvw EQ c_ag.
              IF  w_fieldcatalog-fieldname EQ 'MENGE'.
                w_fieldcatalog-edit = c_x.
              ELSE.
                w_fieldcatalog-edit = space.
              ENDIF.
            ELSEIF p_parvw EQ c_br.
*            IF  w_fieldcatalog-fieldname EQ 'MENGE'.
*              w_fieldcatalog-edit = c_x.
*            ELSE.
              w_fieldcatalog-edit = space.
*            ENDIF.
            ENDIF.
          ELSE .
            w_fieldcatalog-edit = c_x.
          ENDIF.


          IF w_fieldcatalog-fieldname EQ 'ANLN1'
           OR w_fieldcatalog-fieldname EQ 'ANLN2'.
            IF wg_fiscal-imobilizado EQ c_s.
              w_fieldcatalog-emphasize = c_x.
            ELSE.
              w_fieldcatalog-emphasize = space.
            ENDIF.
          ENDIF.
          SELECT SINGLE *
              FROM zfiwrt0001
              INTO @DATA(wl_0001_)
               WHERE operacao EQ @p_operacao.

          IF wl_0001_-ge_remessa = 'S'.
            IF w_fieldcatalog-fieldname EQ 'MATNR'
               OR w_fieldcatalog-fieldname EQ 'CHARG'
               OR w_fieldcatalog-fieldname EQ 'WERKS'
               OR w_fieldcatalog-fieldname EQ 'LGORT'.
              w_fieldcatalog-edit = space.
            ENDIF.
          ENDIF.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.
      ELSE.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE fieldname EQ 'MATNR'
            OR fieldname EQ 'CHARG'
            OR fieldname EQ 'WERKS'
            OR fieldname EQ 'LGORT'
            OR fieldname EQ 'NETPR'
            OR fieldname EQ 'MENGE'
            OR fieldname EQ 'MEINS'
            OR fieldname EQ 'ANLN1'
            OR fieldname EQ 'ANLN2'
            OR fieldname EQ 'VBELN'
            OR fieldname EQ 'POSNR'
            OR fieldname EQ 'RENAS'
            OR fieldname EQ 'POSSUI_ICMS_ST'.
          w_fieldcatalog-edit = space.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.
      ENDIF.
    ENDIF.
    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    " ALRS
    IF grid2 IS NOT INITIAL AND  wg_fiscal-complemento = 'S'.
      CALL METHOD grid2->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog[].

      IF wg_docs-docnum IS INITIAL.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
            WHERE fieldname EQ 'TAXTYP'
               OR fieldname EQ 'TTYPETXT'
               OR fieldname EQ 'TAXGRP'.
          w_fieldcatalog-edit = space.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.
        READ TABLE tg_fields TRANSPORTING NO FIELDS
          WITH KEY group1 = 'GR1'.
        IF sy-subrc IS INITIAL.
          LOOP AT t_fieldcatalog INTO w_fieldcatalog
            WHERE fieldname EQ 'BASE'
               OR fieldname EQ 'RATE'
               OR fieldname EQ 'EXCBAS'
               OR fieldname EQ 'TAXVAL'
               OR fieldname EQ 'OTHBAS'.
            w_fieldcatalog-edit = c_x.
            MODIFY t_fieldcatalog FROM w_fieldcatalog.
          ENDLOOP.
        ELSE.
          LOOP AT t_fieldcatalog INTO w_fieldcatalog
            WHERE fieldname EQ 'BASE'
               OR fieldname EQ 'RATE'
               OR fieldname EQ 'EXCBAS'
               OR fieldname EQ 'TAXVAL'
               OR fieldname EQ 'OTHBAS'.

            w_fieldcatalog-edit = space.
            MODIFY t_fieldcatalog FROM w_fieldcatalog.
          ENDLOOP.
        ENDIF.
      ENDIF.
      CALL METHOD grid2->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog[].

      CALL METHOD grid2->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.
  ENDIF.
  CLEAR wa_layout-stylefname.

*  ELSEIF sy-dynnr EQ 212.
  IF g_custom_cont_desc IS INITIAL.
    CREATE OBJECT g_custom_cont_desc
      EXPORTING
        container_name = g_descbox.

    IF g_custom_cont_desc IS NOT INITIAL.
      CREATE OBJECT obg_descbox
        EXPORTING
          parent            = g_custom_cont_desc
          wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = 72
          max_number_chars  = 255.

      CALL METHOD obg_descbox->set_toolbar_mode
        EXPORTING
          toolbar_mode = '0'.

      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
    ENDIF.
  ENDIF.
*  ELSEIF sy-dynnr EQ 241.
  IF obg_conteiner_contab IS INITIAL.
    CREATE OBJECT obg_conteiner_contab
      EXPORTING
        container_name = g_cc_contab.

    CREATE OBJECT grid3
      EXPORTING
        i_parent = obg_conteiner_contab.

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
*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Contabilização'.
    PERFORM montar_layout_contab.

    wl_filter-fieldname = 'DMBTR'."c_dmbtr.
    wl_filter-sign      = 'I'. "c_i.
    wl_filter-option    = 'NE'. "c_ne.
    wl_filter-low       = '0'.

    APPEND wl_filter TO tl_filter.

    wl_filter-fieldname = 'ESTORNO'."c_dmbtr.
    wl_filter-sign      = 'I'. "c_i.
    wl_filter-option    = 'EQ'. "c_ne.
    wl_filter-low       = space.

    APPEND wl_filter TO tl_filter.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_contab[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_data_changed_finished3 FOR grid3.

  ELSE.
    CALL METHOD grid3->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog.

    READ TABLE tg_fields
      WITH KEY group1 = 'GR1'.
    IF sy-subrc IS INITIAL.
*      MOVE: c_x TO wa_layout-edit.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE fieldname EQ 'ZFBDT'
            OR fieldname EQ 'ZLSCH'
            OR fieldname EQ 'KOSTL'
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
            OR fieldname EQ 'HBKID'.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610

        w_fieldcatalog-edit = c_x.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
*
    ELSE.
*      MOVE : space TO wa_layout-edit.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE fieldname EQ 'ZFBDT'
            OR fieldname EQ 'ZLSCH'
            OR fieldname EQ 'KOSTL'
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
            OR fieldname EQ 'HBKID'.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610


        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
    ENDIF.

    CALL METHOD grid3->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.

    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
*  ENDIF.

** MENSAGENS DA NF
  IF container IS INITIAL.
    CREATE OBJECT container
      EXPORTING
        container_name = 'CC_MSG_NF'.
    CREATE OBJECT splitter_msg
      EXPORTING
        parent      = container
        orientation = 1.
    left = splitter_msg->top_left_container.
    right = splitter_msg->bottom_right_container.
    CREATE OBJECT editor
      EXPORTING
        parent            = right
        wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position = 72
        max_number_chars  = 255.
    CALL METHOD editor->set_readonly_mode
      EXPORTING
        readonly_mode = 1.

    CREATE OBJECT tree
      EXPORTING
        parent              = left
        node_selection_mode = tree->node_sel_mode_single.

** Definition of drag drop behaviour
*    CREATE OBJECT behaviour_left.
*    CALL METHOD behaviour_left->add
*        EXPORTING
*              flavor = 'Tree_move_to_Edit'
*              dragsrc = 'X'
*              droptarget = ' '
*              effect = cl_dragdrop=>copy.
*   CALL METHOD behaviour_left->add
*        EXPORTING
*              flavor = 'Tree_copy_to_Edit'
*              dragsrc = 'X'
*              droptarget = ' '
*              effect = cl_dragdrop=>copy.
*
*    CALL METHOD behaviour_left->get_handle
*         IMPORTING handle = handle_tree.
*
** Drag Drop behaviour of tree control nodes are defined in the node
** structure
*    PERFORM fill_tree.
    PERFORM preenche_tree USING c_root
                                 space
                                 c_x
                                 space
                                 'Mensagens da Nota'
                                 space.
    CALL METHOD tree->add_nodes
      EXPORTING
        node_table           = node_itab
        table_structure_name = 'NODE_STR'.

    event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
    event-appl_event = 'X'.
    APPEND event TO events.

    CALL METHOD tree->set_registered_events
      EXPORTING
        events = events.

* registration of drag and drop events
    DATA dragdrop TYPE REF TO lcl_dragdrop_receiver.
    CREATE OBJECT dragdrop.
    SET HANDLER dragdrop->node_double_click FOR tree.


    CALL METHOD tree->expand_node
      EXPORTING
        node_key = 'ROOT'. "c_Root.

  ELSE.
    IF node_itab[] IS NOT INITIAL.
*      IF v_automatico_memo = abap_false. "BUG SOLTO 134309 / AOENNING / DUMP
      CALL METHOD tree->delete_all_nodes.
      CALL METHOD tree->add_nodes
        EXPORTING
          node_table           = node_itab
          table_structure_name = 'NODE_STR'.

      CALL METHOD tree->expand_node
        EXPORTING
          node_key = 'ROOT'. "c_Root.
*      ENDIF.
    ENDIF.
  ENDIF.

  IF obg_conteiner_aprov IS INITIAL.
    CREATE OBJECT obg_conteiner_aprov
      EXPORTING
        container_name = g_cc_aprov.

    CREATE OBJECT grid4
      EXPORTING
        i_parent = obg_conteiner_aprov.

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

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-grid_title = 'Aprovadores'.
    PERFORM montar_layout_aprov.

*    WL_FILTER-FIELDNAME = 'DMBTR'."c_dmbtr.
*    WL_FILTER-SIGN      = 'I'. "c_i.
*    WL_FILTER-OPTION    = 'NE'. "c_ne.
*    WL_FILTER-LOW       = '0'.
*
*    APPEND WL_FILTER TO TL_FILTER.

    CALL METHOD grid4->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
*       IT_FILTER            = TL_FILTER
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_aprov[].

  ELSE.
    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF obg_conteiner_parc IS INITIAL.
    CREATE OBJECT obg_conteiner_parc
      EXPORTING
        container_name = g_cc_parc.

    CREATE OBJECT grid5
      EXPORTING
        i_parent = obg_conteiner_parc.

    REFRESH: tl_function.
    CREATE OBJECT obg_toolbar2
      EXPORTING
        io_alv_grid2 = grid5.

*      * Register event handler
    SET HANDLER obg_toolbar2->on_toolbar FOR grid5.
    SET HANDLER obg_toolbar2->handle_user_command FOR grid5.

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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
*    wa_layout-edit = c_x.
    wa_layout-stylefname = 'STYLE'.
    wa_layout-grid_title = 'Parceiros da Nota'.
    PERFORM montar_layout_parc.
*    PERFORM montar_style.
*    WL_FILTER-FIELDNAME = 'DMBTR'."c_dmbtr.
*    WL_FILTER-SIGN      = 'I'. "c_i.
*    WL_FILTER-OPTION    = 'NE'. "c_ne.
*    WL_FILTER-LOW       = '0'.
*
*    APPEND WL_FILTER TO TL_FILTER.
    CALL METHOD grid5->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
*       IT_FILTER            = TL_FILTER
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_parc[].

    CALL METHOD grid5->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_data_changed_finished2 FOR grid5.
*              lcl_event_handler=>on_data_changed2 FOR grid5.

  ELSE.
*    PERFORM montar_style.
    CALL METHOD grid5->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog.

    READ TABLE tg_fields
      WITH KEY group1 = 'GR1'.
    IF sy-subrc IS INITIAL.
*      MOVE: c_x TO wa_layout-edit.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE fieldname EQ 'PARVW'
            OR fieldname EQ 'PARID'.

        w_fieldcatalog-edit = c_x.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
*
    ELSE.
*      MOVE : space TO wa_layout-edit.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE fieldname EQ 'PARVW'
            OR fieldname EQ 'PARID'.

        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
    ENDIF.

    CALL METHOD grid5->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.

    CALL METHOD grid5->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
FORM f_dropdown_table.

  DATA: lt_dropdown TYPE lvc_t_dral,
        ls_dropdown TYPE lvc_s_dral.

  FREE: lt_dropdown.

  ls_dropdown-handle  = '1'.
  ls_dropdown-int_value = 'S'.
  ls_dropdown-value   = 'S Sim'.
  APPEND ls_dropdown TO lt_dropdown.

  ls_dropdown-handle  = '1'.
  ls_dropdown-int_value = 'N'.
  ls_dropdown-value   = 'N Não'.
  APPEND ls_dropdown TO lt_dropdown.

  ls_dropdown-handle  = '1'.
  ls_dropdown-int_value = ' '.
  ls_dropdown-value   = '   '.
  APPEND ls_dropdown TO lt_dropdown.

  CALL METHOD grid1->set_drop_down_table
    EXPORTING
      it_drop_down_alias = lt_dropdown.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  REFRESH t_fieldcatalog.
  SELECT SINGLE *
      FROM zfiwrt0001
      INTO @DATA(wl_0001)
       WHERE operacao EQ @p_operacao.
  IF sy-subrc = 0.
    IF wl_0001-ge_remessa = 'S'.
      PERFORM montar_estrutura USING:
        1 'VBAP'        'VBELN'   'TG_ITENS' 'VBELN'  'Ord.Venda'   '10' ' ' 'X' 'X'.
    ENDIF.

  ENDIF.
  PERFORM montar_estrutura USING:
        1 'ZFIWRT0009'  'ITMNUM'  'TG_ITENS' 'ITMNUM' ' '           '04' ' ' ' ' ' ',
        1 'MARA'        'MATNR'   'TG_ITENS' 'MATNR'  ' '           '15' ' ' ' ' 'X',
        2 'MAKT'        'MAKTX'   'TG_ITENS' 'MAKTX'  ' '           '25' ' ' ' ' ' ',
        2 'MARC'        'STEUC'   'TG_ITENS' 'STEUC'  'NCM'         '07' ' ' ' ' ' '.
  IF wl_0001-lm_indea        = 'S' OR
     wl_0001-complement_icms = 'S' AND ( wg_acao = c_modif OR wg_acao = c_add ).  "*-CS2023000043-09.02.2023-#102019-JT
    PERFORM montar_estrutura USING:
          3 ' '    ' '    'TG_ITENS' 'CFOP'   'CFOP'           '07' 'X' ' ' ' '.
  ELSE.
    PERFORM montar_estrutura USING:
          3 ' '    ' '    'TG_ITENS' 'CFOP'   'CFOP'           '07' ' ' ' ' ' '.
  ENDIF.
  PERFORM montar_estrutura USING:
      4 ' '           ' '       'TG_ITENS' 'CHARG'  'Lote'        '10' ' ' ' ' ' ',
      5 'T001W'       'WERKS'   'TG_ITENS' 'WERKS'  ' '           '05' ' ' ' ' 'X',
      5 'ZFIWRT0009'  'LGORT'   'TG_ITENS' 'LGORT'  ' '           '05' ' ' ' ' ' ',
      6 'ZFIWRT0009'  'ANLN1'   'TG_ITENS' 'ANLN1'  ' '           '10' 'X' ' ' ' ',
      7 'ZFIWRT0009'  'ANLN2'   'TG_ITENS' 'ANLN2'  ' '           '10' ' ' 'X' ' ',
*-CS2020001331 - 06.10.2021 - JT - inicio
      8 'ZFIWRT0009'  'POSSUI_ICMS_ST' 'TG_ITENS' 'POSSUI_ICMS_ST'  'ICMS ST?' '08' 'X' ' ' ' ',
*-CS2020001331 - 06.10.2021 - JT - fim
      9 'MSEG'        'MENGE'   'TG_ITENS' 'MENGE'  'Quantidade'          '10' ' ' 'X' 'X',
     10 'MARA'        'MEINS'   'TG_ITENS' 'MEINS'  ' '                   '05' ' ' ' ' 'X'.

  IF  wg_fiscal-imobilizado = 'S' AND
    ( wg_fiscal-tp_mv_imob =  'T' OR wg_fiscal-tp_mv_imob = 'C' ).
    PERFORM montar_estrutura USING:
          11 'ZFIWRT0009'  'NETPR'   'TG_ITENS' 'NETPR'  'Preço'               '10' ' ' ' ' 'X'.
  ELSE.
    PERFORM montar_estrutura USING:
          11 'ZFIWRT0009'  'NETPR'   'TG_ITENS' 'NETPR'  'Preço'               '10' 'X' ' ' 'X'.
  ENDIF.

  PERFORM montar_estrutura USING:
 12 'ZFIWRT0009'  'NETDIS'  'TG_ITENS' 'NETDIS' 'Desconto'            '10' 'X' ' ' 'X',
 13 'ZFIWRT0009'  'NETFRE'  'TG_ITENS' 'NETFRE' 'Frete'               '10' 'X' ' ' 'X',
 14 'ZFIWRT0009'  'NETINS'  'TG_ITENS' 'NETINS' 'Seguro'              '10' 'X' ' ' 'X',
 15 'ZFIWRT0009'  'NETOTH'  'TG_ITENS' 'NETOTH' 'Despesas acessórias' '20' 'X' ' ' 'X',
 16 'MSEG'        'DMBTR'   'TG_ITENS' 'NETWR'  'Total'               '10' ' ' 'X' 'X'.


  IF wl_0001-lm_indea = 'S'.
    PERFORM montar_estrutura USING:
       13 'ZMMT0102'    'RENAS'   'TG_ITENS' 'RENAS'  'Renasem'     '15' 'X' ' ' ' ',
       14 ' '           ' '       'TG_ITENS' 'FASE'   'Fase'        '05' ' ' ' ' ' '.
  ENDIF.

  PERFORM f_texto_sd USING wl_0001-texto_sd.
  PERFORM f_texto_imbolizado.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  IF wg_fiscal-complemento = 'S'.
    p_edit = 'X'.
  ENDIF.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  IF p_field = 'FASE'.
    w_fieldcatalog-icon = c_x.
    w_fieldcatalog-hotspot  = c_x.
  ENDIF.
  IF p_field EQ 'ESTORNO'.
    w_fieldcatalog-checkbox = c_x.
  ENDIF.

  IF p_field EQ 'PARID'.
    w_fieldcatalog-key           = 'X'.
    w_fieldcatalog-key_sel       = 'X'.
  ENDIF.

  IF p_field EQ 'ANLN1'.
*  OR P_FIELD EQ 'ANLN2'.
    w_fieldcatalog-f4availabl = c_x.
*W_FIELDCATALOG-AUTO_VALUE
* BUG - 75507 - Inicio - CBRAND
    w_fieldcatalog-checktable = '!'.
* BUG - 75507 - Fim - CBRAND
  ENDIF.

  IF p_field EQ 'ZLSCH'.
    w_fieldcatalog-f4availabl = c_x.
  ENDIF.

  IF p_field EQ 'POSSUI_ICMS_ST'.
    w_fieldcatalog-drdn_hndl = '1'.
    w_fieldcatalog-drdn_alias = abap_true.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_IMPOSTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_impostos .

  DATA(l_edit) = COND #( WHEN w_zfiwrt0001-complement_icms = 'S' AND ( wg_acao = c_modif OR wg_acao = c_add ) THEN abap_true
                                                                                                              ELSE abap_off ).

  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'J_1BAJ'       'TAXTYP'   'TL_IMPO_AUX' 'TAXTYP'  ' '  ' ' ' ' ' ' ' ',
        2 'J_1BAJT'      'TTYPETXT' 'TL_IMPO_AUX' 'TTYPETXT'  ' '  ' ' ' ' ' ' ' ',
        3 'J_1BAJ'       'TAXGRP'   'TL_IMPO_AUX' 'TAXGRP'  ' '  ' ' ' ' ' ' ' ',
        4 'ZFIWRT0010'   'BASE'     'TL_IMPO_AUX' 'BASE'    ' '  ' ' l_edit ' ' ' ',
        5 'ZFIWRT0010'   'RATE'     'TL_IMPO_AUX' 'RATE'    ' '  ' ' l_edit ' ' ' ',
        6 'ZFIWRT0010'   'TAXVAL'   'TL_IMPO_AUX' 'TAXVAL'  ' '  ' ' l_edit ' ' ' ',
        7 'ZFIWRT0010'   'EXCBAS'   'TL_IMPO_AUX' 'EXCBAS'  ' '  ' ' l_edit ' ' ' ',
        8 'ZFIWRT0010'   'OTHBAS'   'TL_IMPO_AUX' 'OTHBAS'  ' '  ' ' l_edit ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_IMPOSTOS

*&---------------------------------------------------------------------*
*&      Form  MONTA_IMPOSTOS
*&---------------------------------------------------------------------*
FORM f_calculo_difal USING  p_itens   STRUCTURE tg_itens
                            p_taxtyp       TYPE zfiwrt0010-taxtyp
                   CHANGING p_impo    STRUCTURE tg_impo.

  DATA: l_lifnr_lcneg TYPE lfa1-lifnr,
        l_lifnr_parce TYPE lfa1-lifnr,
        l_regio_lcneg TYPE lfa1-regio,
        l_regio_parce TYPE lfa1-regio,
        l_rate_lcneg  TYPE j_1btxic1-rate,
        l_rate_orige  TYPE j_1btxic1-rate,
        l_subtot_aliq TYPE j_1btxic1-rate,
        l_total_item  TYPE zfiwrt0010-taxval.

  CLEAR:  l_lifnr_lcneg,
          l_lifnr_parce,
          l_regio_lcneg,
          l_regio_parce,
          l_rate_lcneg,
          l_rate_orige.

  CHECK p_itens-possui_icms_st = 'N'
    AND p_taxtyp               = c_icop.

*----------------------------------
* operacao tem ICOP?
*----------------------------------
  SELECT taxtyp
    INTO @DATA(l_taxtyp)
    FROM zfiwrt0002
      UP TO 1 ROWS
   WHERE operacao = @p_operacao
     AND taxtyp   = @c_icop.
  ENDSELECT.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_branch
    IMPORTING
      output = l_lifnr_lcneg.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_parid
    IMPORTING
      output = l_lifnr_parce.

  SELECT regio
    INTO l_regio_lcneg
    FROM lfa1
      UP TO 1 ROWS
   WHERE lifnr = l_lifnr_lcneg.
  ENDSELECT.

  SELECT regio
    INTO l_regio_parce
    FROM lfa1
      UP TO 1 ROWS
   WHERE lifnr = l_lifnr_parce.
  ENDSELECT.

  CHECK l_regio_lcneg <> l_regio_parce.

  SELECT rate
    INTO l_rate_lcneg
    FROM j_1btxic1
      UP TO 1 ROWS
   WHERE land1    = c_br
     AND shipfrom	=	l_regio_lcneg
     AND shipto	  =	l_regio_lcneg.
  ENDSELECT.

  SELECT rate
    INTO l_rate_orige
    FROM j_1btxic1
      UP TO 1 ROWS
   WHERE land1    = c_br
     AND shipfrom	=	l_regio_parce
     AND shipto	  =	l_regio_lcneg.
  ENDSELECT.

*----------------------------
* calculo
*----------------------------
  p_impo-base   = p_itens-netwr.
  p_impo-rate   = l_rate_lcneg  - l_rate_orige.
  p_impo-taxval = p_impo-base   * ( p_impo-rate / 100 ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTA_IMPOSTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM monta_impostos  TABLES tl_impo STRUCTURE tg_impo
                     USING   e_row.
  DATA: BEGIN OF wl_1btxic,
          rate TYPE j_1btxic3-rate,
          base TYPE j_1btxic3-base,
        END OF wl_1btxic.

**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

*data:   tg_impo_aux2  like table of tg_impo2 with header line,
*        wa_impo_aux2  like tg_impo2,
*        wa_impo_aux3  like tg_impo,
  DATA: v_line TYPE i,
        v_ics1 TYPE zfiwrt0010-taxval.

  v_ics1 = 0.
  v_line = 1.

** Fim Alteração feita por Alexandre

  DATA: wl_itens     LIKE LINE OF tg_itens,
        wl_1baa      TYPE j_1baa,
        wl_base_aux  TYPE j_1btxic3-base,
        wl_a924      TYPE a924,
        wl_konp      TYPE konp,
        wl_t001w     TYPE t001w,
        wl_1btxsdc   TYPE j_1btxsdc,
        wl_1btxpis   TYPE j_1btxpis,
        wl_1btxcof   TYPE j_1btxcof,
        wl_impo_comp LIKE LINE OF tg_impo_comp,
        v_dmbtr      TYPE zfiwrt0011-dmbtr,
        l_row        TYPE sy-tabix.

  DATA: vl_cst_icms TYPE c LENGTH 2.

  l_row = e_row.


**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

  READ TABLE tg_impo WITH KEY taxtyp = 'ICS1'.
*
  IF sy-subrc = 0.

    MOVE 99 TO tg_impo-line.
    MODIFY tg_impo INDEX sy-tabix TRANSPORTING line.
    SORT tg_impo BY line DESCENDING.

  ENDIF.

** Fim Alteração feita por Alexandre

  "PERFORM impostos_complemento USING e_row.
**  Alteracao feita por Lucas ref; CS1129419 - IR147555 - 21.08.2023
*  READ TABLE tg_itens INTO wl_itens INDEX e_row.
  READ TABLE tg_itens INTO wl_itens INDEX l_row.
** Fim Alteração feita por Lucas

  DATA(_visual_lcto) = abap_false.
  SELECT SINGLE *
    FROM zfiwrt0008
   WHERE seq_lcto EQ p_seq_lcto.

  IF ( sy-subrc EQ 0 ) AND ( p_seq_lcto IS NOT INITIAL ).
    READ TABLE tg_fields TRANSPORTING NO FIELDS
      WITH KEY group1 = 'GR1'.

    IF sy-subrc IS NOT INITIAL.
      _visual_lcto = abap_true.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM j_1baa
    INTO wl_1baa
     WHERE nftype EQ wg_fiscal-nftype.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF w_zfiwrt0001-complement_icms = 'S'.
    READ TABLE tg_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                     edit   = abap_true.
    IF sy-subrc = 0.
      LOOP AT tg_impo_comp WHERE itmnum = wl_itens-itmnum.
        MOVE-CORRESPONDING tg_impo_comp TO tl_impo.
        APPEND tl_impo.
      ENDLOOP.
      EXIT.
    ENDIF.
  ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

  DATA(_calcula_valor_nf) = abap_true.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(i_data)
   WHERE setname EQ 'MAGI_ZNFW0002_PAUTA'
     AND valfrom EQ @p_bukrs.

  IF ( sy-subrc EQ 0 ) AND ( wl_1baa-form IS NOT INITIAL ).
    _calcula_valor_nf = abap_false.
  ENDIF.

  IF ( wl_1baa-direct EQ '1' ).

    CLEAR: wl_a924, wl_konp, wl_t001w, wl_1btxsdc.
*  WL_1BTXIC3 TYPE J_1BTXIC3.
    SELECT SINGLE *
      FROM j_1btxsdc
      INTO wl_1btxsdc
       WHERE taxcode EQ wg_direitos-taxcode.

    LOOP AT tg_impo.
      CLEAR wl_1btxic.
      READ TABLE tg_impo_comp INTO wl_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                                         taxtyp = tg_impo-taxtyp BINARY SEARCH.
      IF ( sy-subrc EQ 0 AND  wg_fiscal-complemento EQ 'S' ) OR
         ( sy-subrc EQ 0 AND  _visual_lcto EQ abap_true    ).

        MOVE-CORRESPONDING: wl_impo_comp TO tl_impo.
        MOVE :  tg_impo-ttypetxt  TO tl_impo-ttypetxt,
                tg_impo-taxgrp    TO tl_impo-taxgrp.

*-CS2020001331 - 06.10.2021 - JT - inicio
        PERFORM f_calculo_difal  USING wl_itens
                                       tg_impo-taxtyp
                              CHANGING tl_impo.
*-CS2020001331 - 06.10.2021 - JT - fim

        APPEND tl_impo.
      ELSEIF tg_impo[] IS NOT INITIAL.
        IF tg_impo-taxtyp EQ c_icm3.
          IF wg_direitos-opertyp EQ c_t.
*          SELECT SINGLE *
*            FROM J_1BAA
*            INTO WL_1BAA
*             WHERE ITMTYP EQ WG_FISCAL-ITMTYP.

            DATA(_utiliza_base_nf) = abap_false.
            IF wl_1baa-entrad EQ c_x.
* Ini - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              PERFORM f_j1btax_43_31  USING wl_itens
                                      CHANGING wl_1btxic.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              IF  wl_1btxic IS INITIAL.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto
                     AND gruop    = c_30
                     AND value    = p_parid
                     AND value2	  =	wl_itens-matnr.

                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate base
                    FROM j_1btxic3
                    INTO wl_1btxic
                     WHERE land1    = c_br
                       AND shipfrom	=	wg_shipfrom
                       AND shipto	  =	wg_shipto
                       AND gruop    = c_40
                       AND value    = p_parid.

                  IF sy-subrc IS NOT INITIAL.
                    IF p_parvw NE c_br
                    AND p_parvw NE c_ag.
                      SELECT SINGLE rate base
                        FROM j_1btxic2
                        INTO wl_1btxic
                         WHERE land1    = c_br
                           AND shipfrom	=	wg_shipfrom
                           AND shipto	  =	wg_shipto
                           AND matnr    = wl_itens-matnr.
                    ENDIF.
                    IF sy-subrc IS NOT INITIAL.
                      SELECT SINGLE rate
                        FROM j_1btxic1
                        INTO wl_1btxic
                         WHERE land1    = c_br
                           AND shipfrom	=	wg_shipfrom
                           AND shipto	  =	wg_shipto.

                      IF sy-subrc EQ 0.
                        _utiliza_base_nf = abap_true.
                      ENDIF.

                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
* Ini - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              PERFORM f_j1btax_43_31  USING wl_itens
                                      CHANGING wl_1btxic.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              IF wl_1btxic IS INITIAL.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto
                     AND gruop    = c_76
                     AND value    = p_parid
                     AND value2	  =	wl_itens-matnr.

                IF sy-subrc IS NOT INITIAL.
                  IF p_parvw NE c_br
                  AND p_parvw NE c_ag.
                    SELECT SINGLE rate base
                      FROM j_1btxic2
                      INTO wl_1btxic
                       WHERE land1    = c_br
                         AND shipfrom	=	wg_shipfrom
                         AND shipto	  =	wg_shipto
                         AND matnr    = wl_itens-matnr.
                  ENDIF.
                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE rate
                      FROM j_1btxic1
                      INTO wl_1btxic
                       WHERE land1    = c_br
                         AND shipfrom = wg_shipfrom
                         AND shipto   = wg_shipto.

                    IF sy-subrc EQ 0.
                      _utiliza_base_nf = abap_true.
                    ENDIF.
                  ENDIF.
                ENDIF.

              ENDIF.
            ENDIF.

            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            SELECT SINGLE *
              FROM t001w
              INTO wl_t001w
               WHERE werks EQ wl_itens-werks.
            IF sy-subrc IS INITIAL.
              IF ( wl_1baa-direct NE '1' ).

                SELECT SINGLE *
                  FROM a924
                  INTO wl_a924
                   WHERE kschl    EQ 'ZIVP'
                     AND aland    EQ 'BR'
                     AND txreg_sf EQ wl_t001w-regio
                     AND matnr    EQ wl_itens-matnr
                     AND datab    LE sy-datum
                     AND datbi    GE sy-datum.

                IF sy-subrc IS INITIAL.


                  SELECT SINGLE *
                    FROM konp
                    INTO wl_konp
                     WHERE knumh EQ wl_a924-knumh.

                ENDIF.
              ENDIF.
            ENDIF.
            IF wl_1btxic-base IS INITIAL.

**Inicio CS2022000696 - Anderson Oenning - 22/06/2022
              IF _calcula_valor_nf EQ abap_true.
                IF wl_konp-kbetr GT wl_itens-netpr.
*                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
                  wl_itens-netwr = ( ( wl_itens-menge * wl_konp-kbetr ) + wl_itens-netfre + wl_itens-netins + wl_itens-netoth ) - wl_itens-netdis.
                ENDIF.
              ENDIF.
**Fim CS2022000696 - Anderson Oenning - 22/06/2022
*              tl_impo-base   = wl_itens-netwr.

              IF _utiliza_base_nf EQ abap_true.
                tl_impo-base   = wl_itens-netwr.
              ELSE.
                tl_impo-base   = wl_1btxic-base.  "ALRS Se a J1NTAX for base ZERO 23/'0/2023
              ENDIF.

              tl_impo-taxval = ( tl_impo-base * ( wl_1btxic-rate / 100 ) ).
              tl_impo-othbas = 0.

            ELSE.
**Inicio CS2022000696 - Anderson Oenning - 22/06/2022
              IF _calcula_valor_nf EQ abap_true.
                IF wl_konp-kbetr GT wl_itens-netpr.
*                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
                  wl_itens-netwr = ( ( wl_itens-menge * wl_konp-kbetr ) + wl_itens-netfre + wl_itens-netins + wl_itens-netoth ) - wl_itens-netdis.
                ENDIF.
              ENDIF.
**Fim CS2022000696 - Anderson Oenning - 22/06/2022
              tl_impo-base   = wl_itens-netwr * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( wl_1btxic-rate / 100 ).

              CLEAR: vl_cst_icms.

              SELECT SINGLE *
                FROM j_1batl1 INTO @DATA(wl_j_1batl1)
               WHERE taxlaw = @wg_direitos-taxlw1.

              IF sy-subrc EQ 0.
                CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
                  EXPORTING
                    input  = wl_j_1batl1-taxsit
                  IMPORTING
                    output = vl_cst_icms.
              ENDIF.

              IF vl_cst_icms EQ '20'.
                tl_impo-excbas = wl_itens-netwr - tl_impo-base.
              ELSE.
                tl_impo-othbas = wl_itens-netwr - tl_impo-base.
              ENDIF.

            ENDIF.
            tl_impo-rate = wl_1btxic-rate.
            IF wg_fiscal-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wg_direitos-opertyp EQ c_i.
**  aqui outros tipos de operacoes
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-excbas.
            IF wg_fiscal-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wg_direitos-opertyp EQ c_n.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
            IF wg_fiscal-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ENDIF.
        ELSEIF wl_1btxsdc-pis EQ c_x
           AND tg_impo-taxtyp EQ c_ipis.

          SELECT SINGLE *
            FROM j_1btxpis
            INTO wl_1btxpis
             WHERE country EQ c_br
               AND gruop   EQ c_72
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr + v_ics1.  " Alexandre Rimini 09.05.2023 chamado 1016278 - Antes era: wl_itens-netwr.
            tl_impo-rate   = wl_1btxpis-rate.
            tl_impo-taxval = tl_impo-base * ( wl_1btxpis-rate / 100 ).
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.
          IF wg_fiscal-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxpis.

        ELSEIF wl_1btxsdc-cofins EQ c_x
           AND tg_impo-taxtyp EQ c_icof.
          SELECT SINGLE *
            FROM j_1btxcof
            INTO wl_1btxcof
             WHERE country EQ c_br
               AND gruop   EQ c_71
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr  + v_ics1.  " Alexandre Rimini 09.05.2023 chamado 1016278 - Antes era: wl_itens-netwr.
            tl_impo-rate   = wl_1btxcof-rate.
            IF  tl_impo-base > 0 AND wl_1btxcof-rate  > 0.
              tl_impo-taxval = tl_impo-base * ( wl_1btxcof-rate / 100 ).
            ENDIF.
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.

          IF wg_fiscal-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxcof.

        ELSEIF  tg_impo-taxtyp EQ c_ics1.
          SELECT SINGLE *
           FROM j_1baa
           INTO wl_1baa
            WHERE itmtyp EQ wg_fiscal-itmtyp.

          IF wl_1baa-entrad EQ c_x.
            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = c_br
                 AND shipfrom	=	wg_shipfrom
                 AND shipto	  =	wg_shipto
                 AND gruop    = c_30
                 AND value    = p_parid
                 AND value2	  =	wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = c_br
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = c_40
                   AND value    = p_parid.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate
                  FROM j_1btxic1
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto.

              ENDIF.

            ENDIF.

          ELSE.

            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = c_br
                 AND shipfrom	=	wg_shipfrom
                 AND shipto	  =	wg_shipto
                 AND gruop    = c_76
                 AND value    = p_parid
                 AND value2	  =	wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate
                FROM j_1btxic1
                INTO wl_1btxic
                 WHERE land1    = c_br
                   AND shipfrom = wg_shipfrom
                   AND shipto   = wg_shipto.
            ENDIF.


          ENDIF.
          MOVE-CORRESPONDING: tg_impo TO tl_impo.

*      IF WL_1BTXIC-BASE IS INITIAL.
*        TL_IMPO-BASE   = WL_ITENS-NETWR.
*        TL_IMPO-TAXVAL = ( TL_IMPO-BASE * ( WL_1BTXIC-RATE / 100 ) ).
*        TL_IMPO-OTHBAS = 0.
*
*      ELSE.
*        TL_IMPO-BASE   = WL_ITENS-NETWR * ( WL_1BTXIC-BASE / 100 ).
          tl_impo-rate =  wl_1btxic-rate .
          IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0.
            tl_impo-base = wl_itens-netwr / ( 1 - ( ( wl_1btxic-rate * ( wl_1btxic-base / 100 ) ) / 100 ) ). " DEVK9A0ZAE - Ajuste 06.01.2021 - Anderosn Oenning
*            tl_impo-base = wl_itens-netwr / ( ( wl_1btxic-base - wl_1btxic-rate ) / 100 ).
          ENDIF.

*          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
*            tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
*          ENDIF.

          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
            IF  wl_1btxic-base > 0.
**Inicio USER STORY #81382 - Anderson Oenning
              tl_impo-base   = tl_impo-base * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ELSE.
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ENDIF.
*            v_ics1 = tl_impo-taxval. " Alexandre - Rimini - CS1016278 - INC107256 - 09.05.2023
**Fim USER STORY #81382 - Anderson Oenning
          ENDIF.



*      ENDIF.
*      TL_IMPO-RATE = WL_1BTXIC-RATE.

          IF wg_fiscal-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxic.

*-CS2020001331 - 06.10.2021 - JT - inicio
        ELSEIF  tg_impo-taxtyp EQ c_icop.

          MOVE-CORRESPONDING: tg_impo  TO tl_impo.

          PERFORM f_calculo_difal   USING wl_itens
                                          tg_impo-taxtyp
                                 CHANGING tl_impo.

          IF wg_fiscal-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxic.
*-CS2020001331 - 06.10.2021 - JT - fim

        ELSE.

**        Aqui outros impostos
          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          MOVE: wl_itens-netwr TO tl_impo-othbas.

          IF wg_fiscal-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSEIF ( wl_1baa-direct EQ '2' ).

    CLEAR: wl_a924, wl_konp, wl_t001w, wl_1btxsdc.
*  WL_1BTXIC3 TYPE J_1BTXIC3.
    SELECT SINGLE *
      FROM j_1btxsdc
      INTO wl_1btxsdc
       WHERE taxcode EQ wg_direitos-taxcode.

    LOOP AT tg_impo.
      CLEAR wl_1btxic.
      READ TABLE tg_impo_comp INTO wl_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                                         taxtyp = tg_impo-taxtyp BINARY SEARCH.
      IF ( sy-subrc EQ 0 AND  wg_fiscal-complemento EQ 'S' ) OR
         ( sy-subrc EQ 0 AND _visual_lcto EQ abap_true     ).
        MOVE-CORRESPONDING: wl_impo_comp TO tl_impo.
        MOVE :  tg_impo-ttypetxt  TO tl_impo-ttypetxt,
                tg_impo-taxgrp    TO tl_impo-taxgrp.
        APPEND tl_impo.
      ELSEIF tg_impo[] IS NOT INITIAL.
        IF tg_impo-taxtyp EQ c_icm3.
          IF wg_direitos-opertyp EQ c_t.
            SELECT SINGLE *
              FROM j_1baa
              INTO wl_1baa
               WHERE itmtyp EQ wg_fiscal-itmtyp.

            IF wl_1baa-entrad EQ c_x.
* ini - ir122480 - znfw0002 - j1btax #103385 rjf - 2023.12.05
              PERFORM f_j1btax_43_31  USING wl_itens
                                      CHANGING wl_1btxic.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              IF wl_1btxic IS INITIAL.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto
                     AND gruop    = c_30
                     AND value    = p_parid
                     AND value2	  =	wl_itens-matnr.

                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate base
                    FROM j_1btxic3
                    INTO wl_1btxic
                     WHERE land1    = c_br
                       AND shipfrom	=	wg_shipfrom
                       AND shipto	  =	wg_shipto
                       AND gruop    = c_40
                       AND value    = p_parid.

                  IF sy-subrc IS NOT INITIAL.
                    IF p_parvw NE c_br
                    AND p_parvw NE c_ag.
                      SELECT SINGLE rate base
                        FROM j_1btxic2
                        INTO wl_1btxic
                         WHERE land1    = c_br
                           AND shipfrom	=	wg_shipfrom
                           AND shipto	  =	wg_shipto
                           AND matnr    = wl_itens-matnr.
                    ENDIF.
                    IF sy-subrc IS NOT INITIAL.
                      SELECT SINGLE rate
                        FROM j_1btxic1
                        INTO wl_1btxic
                         WHERE land1    = c_br
                           AND shipfrom	=	wg_shipfrom
                           AND shipto	  =	wg_shipto.

                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
* ini - ir122480 - znfw0002 - j1btax #103385 rjf - 2023.12.05
              PERFORM f_j1btax_43_31  USING wl_itens
                                      CHANGING wl_1btxic.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
              IF wl_1btxic IS INITIAL.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto
                     AND gruop    = c_76
                     AND value    = p_parid
                     AND value2	  =	wl_itens-matnr.

                IF sy-subrc IS NOT INITIAL.
                  IF p_parvw NE c_br
                  AND p_parvw NE c_ag.
                    SELECT SINGLE rate base
                      FROM j_1btxic2
                      INTO wl_1btxic
                       WHERE land1    = c_br
                         AND shipfrom	=	wg_shipfrom
                         AND shipto	  =	wg_shipto
                         AND matnr    = wl_itens-matnr.
                  ENDIF.
                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE rate
                      FROM j_1btxic1
                      INTO wl_1btxic
                       WHERE land1    = c_br
                         AND shipfrom = wg_shipfrom
                         AND shipto   = wg_shipto.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            SELECT SINGLE *
              FROM t001w
              INTO wl_t001w
               WHERE werks EQ wl_itens-werks.
            IF sy-subrc IS INITIAL.


              SELECT SINGLE *
                FROM a924
                INTO wl_a924
                 WHERE kschl    EQ 'ZIVP'
                   AND aland    EQ 'BR'
                   AND txreg_sf EQ wl_t001w-regio
                   AND matnr    EQ wl_itens-matnr
                   AND datab    LE sy-datum
                   AND datbi    GE sy-datum.

              IF sy-subrc IS INITIAL.


                SELECT SINGLE *
                  FROM konp
                  INTO wl_konp
                   WHERE knumh EQ wl_a924-knumh.

              ENDIF.

            ENDIF.

*-CS2021001266 - 15.12.2021 - JT- inicio
            READ TABLE t_regula INTO w_regula WITH KEY operacao = p_operacao
                                              BINARY SEARCH.
            IF sy-subrc = 0.
              IF wg_shipfrom = wg_shipto. " AND wg_shipto = w_regula-shipto.
                wl_1btxic-rate = w_regula-rate.
              ENDIF.
            ENDIF.
*-CS2021001266 - 15.12.2021 - JT- fim

            IF wl_1btxic-base IS INITIAL.
**Inicio CS2022000696 - Anderson Oenning - 22/06/2022
              IF _calcula_valor_nf EQ abap_true.
                IF wl_konp-kbetr GT wl_itens-netpr.
*                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
                  wl_itens-netwr = ( ( wl_itens-menge * wl_konp-kbetr ) + wl_itens-netfre + wl_itens-netins + wl_itens-netoth ) - wl_itens-netdis.
                ENDIF.
              ENDIF.
**Fim CS2022000696 - Anderson Oenning - 22/06/2022
              tl_impo-base   = wl_itens-netwr.
              tl_impo-taxval = ( tl_impo-base * ( wl_1btxic-rate / 100 ) ).
              tl_impo-othbas = 0.

            ELSE.
**Inicio CS2022000696 - Anderson Oenning - 22/06/2022
              IF _calcula_valor_nf EQ abap_true.
                IF wl_konp-kbetr GT wl_itens-netpr.
*                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
                  wl_itens-netwr = ( ( wl_itens-menge * wl_konp-kbetr ) + wl_itens-netfre + wl_itens-netins + wl_itens-netoth ) - wl_itens-netdis..
                ENDIF.
              ENDIF.
**Fim CS2022000696 - Anderson Oenning - 22/06/2022
              tl_impo-base   = wl_itens-netwr * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( wl_1btxic-rate / 100 ).

              CLEAR: vl_cst_icms.

              SELECT SINGLE *
                FROM j_1batl1 INTO wl_j_1batl1
               WHERE taxlaw = wg_direitos-taxlw1.

              IF sy-subrc EQ 0.
                CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
                  EXPORTING
                    input  = wl_j_1batl1-taxsit
                  IMPORTING
                    output = vl_cst_icms.
              ENDIF.

              IF vl_cst_icms EQ '20'.
                tl_impo-excbas = wl_itens-netwr - tl_impo-base.
              ELSE.
                tl_impo-othbas = wl_itens-netwr - tl_impo-base.
              ENDIF.

            ENDIF.
            tl_impo-rate = wl_1btxic-rate.
            IF wg_fiscal-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wg_direitos-opertyp EQ c_i.
**  aqui outros tipos de operacoes
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-excbas.
            IF wg_fiscal-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wg_direitos-opertyp EQ c_n.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
            IF wg_fiscal-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ENDIF.
        ELSEIF wl_1btxsdc-pis EQ c_x
           AND tg_impo-taxtyp EQ c_ipis.

          SELECT SINGLE *
            FROM j_1btxpis
            INTO wl_1btxpis
             WHERE country EQ c_br
               AND gruop   EQ c_72
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.

*            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  _itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
            tl_impo-base   =  wl_itens-netwr.
            tl_impo-rate   = wl_1btxpis-rate.
            tl_impo-taxval = tl_impo-base * ( wl_1btxpis-rate / 100 ).
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.
          IF wg_fiscal-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxpis.

        ELSEIF wl_1btxsdc-cofins EQ c_x
           AND tg_impo-taxtyp EQ c_icof.
          SELECT SINGLE *
            FROM j_1btxcof
            INTO wl_1btxcof
             WHERE country EQ c_br
               AND gruop   EQ c_71
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
*            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  w-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
            tl_impo-base   = wl_itens-netwr.
            tl_impo-rate   = wl_1btxcof-rate.
            IF  tl_impo-base > 0 AND wl_1btxcof-rate  > 0.
              tl_impo-taxval = tl_impo-base * ( wl_1btxcof-rate / 100 ).
            ENDIF.
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.

          IF wg_fiscal-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxcof.

        ELSEIF  tg_impo-taxtyp EQ c_ics1.
          SELECT SINGLE *
           FROM j_1baa
           INTO wl_1baa
            WHERE itmtyp EQ wg_fiscal-itmtyp.

          IF wl_1baa-entrad EQ c_x.
            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = c_br
                 AND shipfrom	=	wg_shipfrom
                 AND shipto	  =	wg_shipto
                 AND gruop    = c_30
                 AND value    = p_parid
                 AND value2	  =	wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = c_br
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = c_40
                   AND value    = p_parid.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate
                  FROM j_1btxic1
                  INTO wl_1btxic
                   WHERE land1    = c_br
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto.

              ENDIF.

            ENDIF.

          ELSE.

            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = c_br
                 AND shipfrom	=	wg_shipfrom
                 AND shipto	  =	wg_shipto
                 AND gruop    = c_76
                 AND value    = p_parid
                 AND value2	  =	wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate
                FROM j_1btxic1
                INTO wl_1btxic
                 WHERE land1    = c_br
                   AND shipfrom = wg_shipfrom
                   AND shipto   = wg_shipto.
            ENDIF.

          ENDIF.
          MOVE-CORRESPONDING: tg_impo TO tl_impo.

*      IF WL_1BTXIC-BASE IS INITIAL.
*        TL_IMPO-BASE   = WL_ITENS-NETWR.
*        TL_IMPO-TAXVAL = ( TL_IMPO-BASE * ( WL_1BTXIC-RATE / 100 ) ).
*        TL_IMPO-OTHBAS = 0.
*
*      ELSE.
*        TL_IMPO-BASE   = WL_ITENS-NETWR * ( WL_1BTXIC-BASE / 100 ).
          tl_impo-rate =  wl_1btxic-rate .
          IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0. "=A1/(1 - ( (B1 * (B2/100)) / 100))
            tl_impo-base = wl_itens-netwr / ( 1 - ( ( wl_1btxic-rate * ( wl_1btxic-base / 100 ) ) / 100 ) ). " DEVK9A0ZAE - Ajuste 06.01.2021 - Anderosn Oenning
*            tl_impo-base = wl_itens-netwr / ( ( wl_1btxic-base - wl_1btxic-rate ) / 100 ).
          ENDIF.

*          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
*            tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
*          ENDIF.

          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
            IF  wl_1btxic-base > 0.

**Inicio USER STORY #81382 - Anderson Oenning
              tl_impo-base   = tl_impo-base * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ELSE.
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ENDIF.
**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
*            v_ics1 = tl_impo-taxval.

**Fim USER STORY #81382 - Anderson Oenning
          ENDIF.




*      ENDIF.
*      TL_IMPO-RATE = WL_1BTXIC-RATE.

          IF wg_fiscal-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxic.
        ELSE.

**        Aqui outros impostos
          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          MOVE: wl_itens-netwr TO tl_impo-othbas.

          IF wg_fiscal-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  tg_impo_gera[] = tl_impo[].  "*-CS2023000043-09.02.2023-#102019-JT

  PERFORM impostos_complemento USING l_row.

ENDFORM.                    " MONTA_IMPOSTOS
*&---------------------------------------------------------------------*
*&      Module  BUSCA_VALOR_CONT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_valor_cont OUTPUT.
  PERFORM monta_contabil.

*  ENDLOOP.
ENDMODULE.                 " BUSCA_VALOR_CONT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_CONTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_contab.
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZFIWRT0011'  'BSCHL'   'TG_CONTAB' 'BSCHL'    ' '  '4' ' ' ' ' ' ',
        2 'ZFIWRT0011'  'HKONT'   'TG_CONTAB' 'HKONT'    ' '  '8' ' ' ' ' ' ',
        3 'SKAT'        'TXT50'   'TG_CONTAB' 'TXT50'    ' '  '25'  ' ' ' ' ' ',
        3 ''            ' '       'TG_CONTAB' 'UMSKZ'    'Rz.Esp'  '8'  ' ' ' ' ' ',
        4 'ZFIWRT0011'  'DMBTR'   'TG_CONTAB' 'DMBTR'    'Valor'  '13'  ' ' ' ' ' ',
        5 'ZFIWRT0011'  'TAXTYP'  'TG_CONTAB' 'TAXTYP'   ' '  '8'  ' ' ' ' ' ',
        9 'ZFIWRT0011'  'ESTORNO' 'TG_CONTAB' 'ESTORNO'  'Estorno'  '5' ' ' ' ' ' ',
        6 'ZFIWRT0011'  'NEWBW'   'TG_CONTAB' 'NEWBW'    ' '  '8' ' ' ' ' ' ',
        7 'ZFIWRT0011'  'ZLSCH'   'TG_CONTAB' 'ZLSCH'    ' '  '8' ' ' ' ' ' ',
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        8 'ZFIWRT0011'  'TAXA_JUROS'   'TG_CONTAB' 'TAXA_JUROS'    ' '  '8' ' ' ' ' ' ',
        8 'ZFIWRT0011'  'TAXA_MULTA'   'TG_CONTAB' 'TAXA_MULTA'    ' '  '8' ' ' ' ' ' ',
        8 'ZFIWRT0011'  'HBKID'        'TG_CONTAB' 'HBKID'         ' '  '8' ' ' ' ' ' ',
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        8 'ZFIWRT0011'  'ZFBDT'   'TG_CONTAB' 'ZFBDT'    'Data Vencimento'  '8' ' ' ' ' ' ',
        8 'ZFIWRT0011'  'KOSTL'   'TG_CONTAB' 'KOSTL'    'Centro de Custos'  '8' ' ' ' ' ' ',
        8 'ZFIWRT0011'  'VBUND'   'TG_CONTAB' 'VBUND'    'Soc.Parceira'  '8' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_CONTAB
*&---------------------------------------------------------------------*
*&      Form  FILL_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_tree .
  DATA: node LIKE mtreesnode.

* node table of the left tree
  CLEAR node.
  node-node_key = c_root.
  node-isfolder = 'X'.
  node-text = 'Mensagens da Nota'.
  node-dragdropid = ' '.
  APPEND node TO node_itab.

*  CLEAR node.
*  node-node_key = 'Child1'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 1'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child2'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 2'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child3'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 3'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child4'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 4'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child5'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 5'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child6'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 6'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
*
*  CLEAR node.
*  node-node_key = 'Child7'.
*  node-relatkey = 'Root'.
*  node-relatship = cl_gui_simple_tree=>relat_last_child.
*  node-text = 'DragDrop Text 7'.
*  node-dragdropid = handle_tree.       " handle of behaviour
*  APPEND node TO node_itab.
ENDFORM.                    " FILL_TREE
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_230  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_230 INPUT.
  DATA: wl_lines         TYPE sy-tabix,
        wl_lines_aux(6),
        wl_key           TYPE tv_nodekey,
        tl_nodes         TYPE REF TO cl_gui_object,
        wl_selected_node TYPE tv_nodekey,
        tl_editor        TYPE TABLE OF ty_editor.
  .
*        node LIKE mtreesnode.

  DESCRIBE TABLE node_itab LINES wl_lines.

  CASE sy-ucomm.
    WHEN c_add_msg.
      PERFORM preenche_tree USING wl_lines
                                  c_root
                                  space
                                  cl_gui_simple_tree=>relat_last_child
                                  space
                                  handle_tree.

    WHEN c_del_msg.
      CALL METHOD tree->get_selected_node
        IMPORTING
          node_key = wl_selected_node.

      IF wl_selected_node NE c_root
      AND wl_selected_node IS NOT INITIAL.
        READ TABLE tg_mensagems_aux
          WITH KEY seqnum = wl_selected_node.

        READ TABLE tg_mensagems
         WITH KEY message = tg_mensagems_aux-message.
        IF sy-subrc IS NOT INITIAL.
          DELETE node_itab WHERE node_key EQ wl_selected_node.
          DELETE tg_mensagems_aux WHERE seqnum = wl_selected_node.
          REFRESH: tg_editor.
          CALL METHOD editor->set_text_as_r3table
            EXPORTING
              table = tl_editor.

          CALL METHOD tree->delete_node
            EXPORTING
              node_key = wl_selected_node.
        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não é possivel eliminar essa'
                                                 'mensagem!'.
        ENDIF.
      ENDIF.
    WHEN c_save_msg.
*       tg_mensagems_aux

      CALL METHOD editor->get_text_as_r3table
        IMPORTING
          table = tg_editor.

      CLEAR: tg_mensagems_aux, tg_mensagems.
      LOOP AT tg_editor INTO wg_editor.
        IF sy-tabix EQ 1.
          PERFORM preenche_tree USING wg_dclknodekey "tl_0005-seqnum
                                    c_root
                                    space
                                    cl_gui_simple_tree=>relat_last_child
                                    wg_editor-line
                                    handle_tree.
*          tg_mensagems-seqnum     = wl_line.
*          tg_mensagems-message    = wg_editor-line.
*          MODIFY tg_mensagems INDEX wl_line.
*
*          IF sy-subrc IS NOT INITIAL.
*            APPEND tg_mensagems.
*
*          ENDIF.

          DELETE tg_mensagems_aux WHERE seqnum EQ wg_dclknodekey.
        ENDIF.


        tg_mensagems_aux-seqnum = wg_dclknodekey.
        ADD 1 TO tg_mensagems_aux-linnum.
        tg_mensagems_aux-message = wg_editor-line.

*        READ TABLE tg_mensagems_aux TRANSPORTING NO FIELDS
*          WITH KEY seqnum = wl_line
*                   linnum = tg_mensagems_aux-linnum.
*        IF sy-subrc IS INITIAL.
*          MODIFY tg_mensagems_aux INDEX sy-tabix.
*
*        ELSE.
        APPEND tg_mensagems_aux.

*        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_230  INPUT
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_LINES  text
*      -->P_6516   text
*      -->P_CL_GUI_SIMPLE_TREE=>RELAT_LAST  text
*      -->P_SPACE  text
*      -->P_HANDLE_TREE  text
*----------------------------------------------------------------------*
FORM preenche_tree  USING    p_key
                             VALUE(p_6516)
                             VALUE(p_isfolder)
                             VALUE(p_relat_last)
                             VALUE(p_text)
                             p_handle_tree.

  CLEAR node.
  node-node_key = p_key.
  node-isfolder = p_isfolder.
  node-relatkey = p_6516.
  node-relatship = p_relat_last.
  node-text = p_text.
  node-dragdropid = p_handle_tree.       " handle of behaviour
  READ TABLE node_itab TRANSPORTING NO FIELDS
 WITH KEY node_key = p_key.
  IF sy-subrc IS INITIAL.
    MODIFY  node_itab INDEX sy-tabix FROM node .
  ELSE.
    APPEND node TO node_itab.

  ENDIF.

ENDFORM.                    " PREENCHE_TREE
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_campos .

  CLEAR: tg_mensagems_aux, tg_mensagems, tg_itens, tg_impo, tg_contab, tg_movest,
         wg_op_fiscal, wg_desc_nftype, wg_desc_itmtyp, wg_desc_cfop, wg_desc_taxlw1,
         wg_desc_taxlw2, wg_desc_taxlw4, wg_desc_taxlw5, wg_fiscal, wg_direitos, wg_shipfrom,
         wg_shipto, wg_editor, tg_editor, tg_tbsl, tg_selectedcell, wg_flag, x_field, wg_docs,
         wg_desc_taxcode, g_init_once,
         wg_acao. "*-CS2023000043-09.02.2023-#102019-JT

  REFRESH: tg_mensagems_aux, tg_mensagems, tg_impo, tg_impo_comp,
           tg_contab, tg_movest, tg_editor,
           tg_tbsl, tg_selectedcell, tg_itens, tg_aprov. "NODE_ITAB.

*  CALL METHOD TREE->DELETE_ALL_NODES.
*  PERFORM PREENCHE_TREE USING C_ROOT
*                         SPACE
*                         C_X
*                         SPACE
*                         'Mensagens da Nota'
*                         SPACE.
ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .
  DATA: wl_input_0008 TYPE zfiwrt0008,
        tl_input_0009 TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
        tl_input_0010 TYPE TABLE OF zfiwrt0010 WITH HEADER LINE,
        tl_input_0011 TYPE TABLE OF zfiwrt0011 WITH HEADER LINE,
        tl_input_0012 TYPE TABLE OF zfiwrt0012 WITH HEADER LINE,
        tl_input_0013 TYPE TABLE OF zfiwrt0013 WITH HEADER LINE,
        tl_input_0015 TYPE TABLE OF zfiwrt0015 WITH HEADER LINE,
        tl_input_0019 TYPE TABLE OF zfiwrt0019 WITH HEADER LINE,
        tl_input_0020 TYPE TABLE OF zfiwrt0020 WITH HEADER LINE,
        tl_impo_aux   LIKE TABLE OF tg_impo WITH HEADER LINE,
        wl_0008_aux   TYPE zfiwrt0008,
        wl_0020       TYPE zfiwrt0020.

  DATA: lt_0020       TYPE TABLE OF zfiwrt0020.
  DATA: lt_lin        TYPE TABLE OF j_1bnflin.
  DATA: lt_0094       TYPE TABLE OF zsdt0094.
  DATA: ls_0094       TYPE zsdt0094.
  DATA(obj_tx_curva_db) = NEW zcl_taxa_curva_db( ).

  wl_input_0008-seq_lcto = p_seq_lcto.


  SELECT SINGLE *
    FROM zfiwrt0008
    INTO wl_0008_aux
     WHERE seq_lcto EQ wl_input_0008-seq_lcto.

  IF sy-subrc IS NOT INITIAL.
    MOVE:  sy-uname TO wl_input_0008-usnam,
           sy-datum TO wl_input_0008-dt_criacao,
           sy-uzeit TO wl_input_0008-hr_criacao.
    CLEAR wl_input_0008-ch_referencia.
  ELSE.
    MOVE: wl_0008_aux-usnam      TO wl_input_0008-usnam,
          wl_0008_aux-dt_criacao TO wl_input_0008-dt_criacao,
          wl_0008_aux-hr_criacao TO wl_input_0008-hr_criacao,
          wl_0008_aux-ch_referencia TO wl_input_0008-ch_referencia.
  ENDIF.

*  DELETE FROM zfiwrt0008 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
** ZFIWRT0008
  MOVE : sy-mandt             TO wl_input_0008-mandt,
         p_operacao           TO wl_input_0008-operacao,
         p_bukrs              TO wl_input_0008-bukrs,
         p_branch             TO wl_input_0008-branch,
         p_parvw              TO wl_input_0008-parvw,
         p_parid              TO wl_input_0008-parid,
         wg_fiscal-nftype     TO wl_input_0008-nftype,
         wg_fiscal-move_plant TO wl_input_0008-move_plant,
         wg_fiscal-move_stloc TO wl_input_0008-move_stloc,
         wg_fiscal-ctrl_zrfl  TO wl_input_0008-ctrl_zrfl,
         wg_fiscal-access_key   TO wl_input_0008-access_key,
         wg_fiscal-nr_romaneio  TO wl_input_0008-nr_romaneio,
         wg_fiscal-docref       TO wl_input_0008-docref,
         wg_fiscal-imobilizado  TO wl_input_0008-imobilizado,
         wg_fiscal-tp_mv_imob   TO wl_input_0008-tp_mv_imob,
         wg_fiscal-kostl        TO wl_input_0008-kostl,
         wg_fiscal-zpesagem     TO wl_input_0008-zpesagem,
         wg_fiscal-dias         TO wl_input_0008-dias,
         wg_fiscal-retorno      TO wl_input_0008-retorno,
         wg_fiscal-energia      TO wl_input_0008-energia,
         wg_fiscal-servico      TO wl_input_0008-servico,
         wg_fiscal-complemento  TO wl_input_0008-complemento,
         wg_fiscal-inco1        TO wl_input_0008-inco1,
         wg_fiscal-inco2        TO wl_input_0008-inco2,
         wg_fiscal-ebeln        TO wl_input_0008-ebeln,
         wg_fiscal-referencia   TO wl_input_0008-referencia,
         wg_fiscal-lm_estoque   TO wl_input_0008-lm_estoque,
         wg_fiscal-konto        TO wl_input_0008-konto,
         wg_fiscal-move_mat     TO wl_input_0008-move_mat,
         wg_fiscal-move_batch   TO wl_input_0008-move_batch,
         wg_fiscal-bktxt        TO wl_input_0008-bktxt,
         wg_fiscal-mtsnr        TO wl_input_0008-mtsnr,

         wg_direitos-cfop       TO wl_input_0008-cfop,
         wg_direitos-taxlw1     TO wl_input_0008-taxlw1,
         wg_direitos-taxlw2     TO wl_input_0008-taxlw2,
         wg_direitos-taxlw4     TO wl_input_0008-taxlw4,
         wg_direitos-taxlw5     TO wl_input_0008-taxlw5,
         wg_direitos-opertyp    TO wl_input_0008-opertyp,
         wg_direitos-taxcode    TO wl_input_0008-taxcode,
         sy-uname               TO wl_input_0008-usuario_ult_mod,
         sy-datum               TO wl_input_0008-dt_ult_mod,
         sy-uzeit               TO wl_input_0008-hr_ult_mod,
         wg_docs-nfenum         TO wl_input_0008-nfenum,
         wg_docs-budat          TO wl_input_0008-budat,
         wg_docs-bldat          TO wl_input_0008-bldat,
         wg_docs-series         TO wl_input_0008-series,
         wg_docs-belnr          TO wl_input_0008-belnr,
         wg_docs-mblnr          TO wl_input_0008-mblnr,
         wg_docs-docnum         TO wl_input_0008-docnum,
         wg_docs-loc_carrega    TO wl_input_0008-loc_carrega, "CS2020001418 - CSB
         wl_0008_aux-mjahr      TO wl_input_0008-mjahr,
         wl_0008_aux-loekz      TO wl_input_0008-loekz,
         wl_0008_aux-status     TO wl_input_0008-status,
         wl_0008_aux-obj_key    TO wl_input_0008-obj_key,
         wl_input_0008-seq_lcto TO tl_input_0019-seq_lcto,
         wg_transporte-lifnr    TO tl_input_0019-lifnr,
         wg_transporte-placa    TO tl_input_0019-placa,
         wg_transporte-anzpk    TO tl_input_0019-anzpk,
         wg_transporte-shpunt   TO tl_input_0019-shpunt,
         wg_transporte-ntgew    TO tl_input_0019-ntgew,
         wg_transporte-brgew    TO tl_input_0019-brgew,
         wg_transporte-ufplaca  TO tl_input_0019-ufplaca,

         wg_transporte-placa_car1 TO tl_input_0019-placa_car1,
         wg_transporte-placa_car2 TO tl_input_0019-placa_car2,
         wg_transporte-placa_car3 TO tl_input_0019-placa_car2,
         wg_transporte-motorista  TO tl_input_0019-motorista.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = p_parid
*    IMPORTING
*      output = wl_input_0008-parid.

  REFRESH: tg_editor.
  IF obg_descbox IS NOT INITIAL.
    CALL METHOD obg_descbox->get_text_as_r3table
      IMPORTING
        table = tg_editor.

    LOOP AT tg_editor INTO wg_editor.
      IF sy-tabix EQ 1.
        wl_input_0008-txt_compl = wg_editor-line.

      ELSEIF sy-tabix GE 2.
        CONCATENATE wl_input_0008-txt_compl  wg_editor-line INTO wl_input_0008-txt_compl SEPARATED BY space.

      ENDIF.
    ENDLOOP.
  ENDIF.
***<
*DOCNUM
*BELNR
** ZFIWRT0009
  DELETE FROM zfiwrt0009 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  DELETE FROM zfiwrt0010 WHERE seq_lcto EQ wl_input_0008-seq_lcto.

  "Documentos Referenciados
  LOOP AT tg_docrefs.
    MOVE:sy-mandt               TO tl_input_0020-mandt,
         wl_input_0008-seq_lcto TO tl_input_0020-seq_lcto,
         tg_docrefs-docnum      TO tl_input_0020-docnum.
    APPEND tl_input_0020.
  ENDLOOP.

  "Verificação de Documento Anulado Referênciado
  "CFOP 1206 - Anulação de valor relativo à prestação de serviço de transporte
  "CFOP 2206 - Anulação de valor relativo à prestação de serviço de transporte
  READ TABLE tl_input_0009 INDEX 1.

  IF tl_input_0009-cfop(4) EQ '1206' OR tl_input_0009-cfop(4) EQ '2206'.
    SELECT * INTO wl_0020
      FROM zfiwrt0020
     WHERE seq_lcto EQ wl_input_0008-seq_lcto.
      "Anular Documento.
      CALL METHOD zcl_cte=>set_anular_cte_saida
        EXPORTING
          i_docnum = wl_0020-docnum
          i_anular = abap_false.
    ENDSELECT.
  ENDIF.

  DELETE FROM zfiwrt0020 WHERE seq_lcto EQ wl_input_0008-seq_lcto.

  LOOP AT tg_itens.
    MOVE:sy-mandt               TO tl_input_0009-mandt,
         wl_input_0008-seq_lcto TO tl_input_0009-seq_lcto,
*         sy-tabix               TO tl_input_0009-itmnum,
         tg_itens-itmnum        TO tl_input_0009-itmnum,
         tg_itens-matnr         TO tl_input_0009-matnr,
         tg_itens-cfop          TO tl_input_0009-cfop,
         tg_itens-charg         TO tl_input_0009-charg,
         tg_itens-menge         TO tl_input_0009-menge,
         tg_itens-meins         TO tl_input_0009-meins,
         tg_itens-netpr         TO tl_input_0009-netpr,
         tg_itens-netwr         TO tl_input_0009-netwr,
         wg_fiscal-itmtyp       TO tl_input_0009-itmtyp,
         tg_itens-werks         TO tl_input_0009-bwkey,
         tg_itens-lgort         TO tl_input_0009-lgort,
         tg_itens-anln1         TO tl_input_0009-anln1,
         tg_itens-anln2         TO tl_input_0009-anln2,
         tg_itens-vbeln         TO tl_input_0009-vbeln,
         tg_itens-posnr         TO tl_input_0009-posnr,
*-CS2020001331 - 06.10.2021 - JT - inicio
         tg_itens-possui_icms_st TO tl_input_0009-possui_icms_st,
*-CS2020001331 - 06.10.2021 - JT - fim
         tg_itens-netdis        TO tl_input_0009-netdis,
         tg_itens-netfre        TO tl_input_0009-netfre,
         tg_itens-netins        TO tl_input_0009-netins,
         tg_itens-netoth        TO tl_input_0009-netoth.

*    MULTIPLY tl_input_0009-itmnum BY 10.

*    CONDENSE tl_input_0009-itmnum NO-GAPS.
    APPEND tl_input_0009.
***    ZFIWRT0010

*-CS2023000043-09.02.2023-#102019-JT-inicio
    IF w_zfiwrt0001-complement_icms = 'S'.
      LOOP AT tg_impo_comp WHERE itmnum = tg_itens-itmnum.
        MOVE: sy-mandt               TO tl_input_0010-mandt,
              wl_input_0008-seq_lcto TO tl_input_0010-seq_lcto,
              tl_input_0009-itmnum   TO tl_input_0010-itmnum,
              tg_impo_comp-taxtyp    TO tl_input_0010-taxtyp,
              tg_impo_comp-base      TO tl_input_0010-base,
              tg_impo_comp-rate      TO tl_input_0010-rate,
              tg_impo_comp-taxval    TO tl_input_0010-taxval,
              tg_impo_comp-excbas    TO tl_input_0010-excbas,
              tg_impo_comp-othbas    TO tl_input_0010-othbas.

        APPEND tl_input_0010.
      ENDLOOP.
    ELSE.
      PERFORM monta_impostos TABLES tl_impo_aux
                             USING sy-tabix."tl_input_0009-itmnum.
      LOOP AT tl_impo_aux.
        MOVE: sy-mandt               TO tl_input_0010-mandt,
              wl_input_0008-seq_lcto TO tl_input_0010-seq_lcto,
              tl_input_0009-itmnum   TO tl_input_0010-itmnum,
              tl_impo_aux-taxtyp     TO tl_input_0010-taxtyp,
              tl_impo_aux-base       TO tl_input_0010-base,
              tl_impo_aux-rate       TO tl_input_0010-rate,
              tl_impo_aux-taxval     TO tl_input_0010-taxval,
              tl_impo_aux-excbas     TO tl_input_0010-excbas,
              tl_impo_aux-othbas     TO tl_input_0010-othbas.

        APPEND tl_input_0010.
      ENDLOOP.
    ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

    CLEAR: tl_input_0009.
  ENDLOOP.
** ZFIWRT0011
  DELETE FROM zfiwrt0011 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  PERFORM monta_contabil.
  LOOP AT tg_contab.
    MOVE: sy-mandt                TO tl_input_0011-mandt,
          wl_input_0008-seq_lcto  TO tl_input_0011-seq_lcto,
          tg_contab-bschl         TO tl_input_0011-bschl,
          tg_contab-hkont         TO tl_input_0011-hkont,
          tg_contab-taxtyp        TO tl_input_0011-taxtyp,
          tg_contab-waers_i       TO tl_input_0011-waers_i,
          tg_contab-dmbtr         TO tl_input_0011-dmbtr,
          tg_contab-curha	        TO tl_input_0011-curha,
          tg_contab-dmbe2         TO tl_input_0011-dmbe2,
          tg_contab-curin         TO tl_input_0011-curin,
          tg_contab-dmbe3         TO tl_input_0011-dmbe3,
          tg_contab-waers         TO tl_input_0011-waers,
          tg_contab-wrbtr         TO tl_input_0011-wrbtr,
          tg_contab-artnr         TO tl_input_0011-artnr,
          tg_contab-estorno       TO tl_input_0011-estorno,
          tg_contab-zlsch         TO tl_input_0011-zlsch,
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
          tg_contab-taxa_juros    TO tl_input_0011-taxa_juros,
          tg_contab-taxa_multa    TO tl_input_0011-taxa_multa,
          tg_contab-hbkid         TO tl_input_0011-hbkid,
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
          tg_contab-zfbdt         TO tl_input_0011-zfbdt,
          tg_contab-kostl         TO tl_input_0011-kostl,
          tg_contab-umskz         TO tl_input_0011-umskz,
          tg_contab-vbund         TO tl_input_0011-vbund.

    tl_input_0011-buzei  = sy-tabix.

    APPEND tl_input_0011.
    CLEAR: tl_input_0011.
  ENDLOOP.

** ZFIWRT0012
  DELETE FROM zfiwrt0012 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  LOOP AT tg_movest.
    MOVE: sy-mandt                TO  tl_input_0012-mandt,
          wl_input_0008-seq_lcto  TO  tl_input_0012-seq_lcto,
          tg_movest-bwart         TO  tl_input_0012-bwart,
          tg_movest-tcode         TO  tl_input_0012-tcode,
          tg_movest-mwskz1        TO  tl_input_0012-mwskz1,
          tg_movest-estorno       TO  tl_input_0012-estorno.

    APPEND tl_input_0012.
    CLEAR: tl_input_0012.
  ENDLOOP.

** ZFIWRT0013
  DELETE FROM zfiwrt0013 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  LOOP AT tg_mensagems_aux.
    MOVE: sy-mandt                 TO tl_input_0013-mandt,
          wl_input_0008-seq_lcto   TO tl_input_0013-seq_lcto,
          tg_mensagems_aux-seqnum  TO tl_input_0013-seqnum,
          tg_mensagems_aux-linnum  TO tl_input_0013-linnum,
          tg_mensagems_aux-message TO tl_input_0013-message.

    APPEND tl_input_0013.
    CLEAR: tl_input_0013.
  ENDLOOP.

** ZFIWRT0015
  DELETE FROM zfiwrt0015 WHERE seq_lcto EQ wl_input_0008-seq_lcto.
  LOOP AT tg_parc.
    MOVE: sy-mandt                 TO tl_input_0015-mandt,
          wl_input_0008-seq_lcto   TO tl_input_0015-seq_lcto,
          tg_parc-parvw            TO tl_input_0015-parvw,
          tg_parc-parid            TO tl_input_0015-parid.

    APPEND tl_input_0015.
    CLEAR: tl_input_0015.
  ENDLOOP.

  LOOP AT tl_input_0011 INTO DATA(wa_11) WHERE kostl IS NOT INITIAL.
    wl_input_0008-kostl = wa_11-kostl.
  ENDLOOP.


  SELECT SINGLE *
    FROM j_1baa INTO @DATA(_wl_j_1baa)
   WHERE nftype = @wl_input_0008-nftype.

  IF sy-subrc EQ 0.
    wl_input_0008-form = _wl_j_1baa-form.
  ENDIF.

  IF ( viniciou_lcto_znfw0009 IS NOT INITIAL ) AND
     ( vlancamento_znfw0009   IS NOT INITIAL ).
    wl_input_0008-tcode_org = 'ZNFW0009'.
  ENDIF.

  MODIFY zfiwrt0008 FROM wl_input_0008.
  MODIFY zfiwrt0009 FROM TABLE tl_input_0009.
  MODIFY zfiwrt0010 FROM TABLE tl_input_0010.
  MODIFY zfiwrt0011 FROM TABLE tl_input_0011.
  MODIFY zfiwrt0012 FROM TABLE tl_input_0012.
  MODIFY zfiwrt0013 FROM TABLE tl_input_0013.
  MODIFY zfiwrt0015 FROM TABLE tl_input_0015.
  MODIFY zfiwrt0020 FROM TABLE tl_input_0020.
  MODIFY zfiwrt0019 FROM tl_input_0019.

  COMMIT WORK AND WAIT.

  IF p_operacao = 561.
    SELECT * INTO TABLE lt_0020
      FROM zfiwrt0020
     WHERE seq_lcto EQ wl_input_0008-seq_lcto.
    IF lt_0020[] IS INITIAL.
      lt_0020[] = tl_input_0020[].
    ENDIF.
    IF lt_0020[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_lin
        FROM j_1bnflin
        FOR ALL ENTRIES IN lt_0020
        WHERE docnum = lt_0020-docnum.
      IF sy-subrc = 0.
        DELETE lt_lin WHERE refkey IS INITIAL.

*---> 04/07/2023 - Migração S4 - WS
        SORT lt_lin BY refkey.
*<--- 04/07/2023 - Migração S4 - WS
        DELETE ADJACENT DUPLICATES FROM lt_lin COMPARING refkey.
        IF lt_lin[] IS NOT INITIAL.
          SELECT * INTO TABLE lt_0094
            FROM zsdt0094
            FOR ALL ENTRIES IN lt_lin
            WHERE nro_sol_ov = lt_lin-refkey(10)
              AND estorno = 0.
          IF lt_0094[] IS NOT INITIAL.
            SORT lt_0094 BY nro_sol_ov.
            DELETE lt_0094 WHERE nro_sol_ov IS INITIAL.

*---> 04/07/2023 - Migração S4 - WS
            SORT lt_0094 BY nro_sol_ov.
*<--- 04/07/2023 - Migração S4 - WS
            DELETE ADJACENT DUPLICATES FROM lt_0094 COMPARING nro_sol_ov.
            LOOP AT lt_0094 INTO ls_0094.
              obj_tx_curva_db->estorno_aqv( ls_0094-nro_sol_ov ).
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  MESSAGE s836(sd) WITH 'Lançamento' wl_input_0008-seq_lcto ', criado/modificado com sucesso!'.


ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_contabil .
  DATA: tl_impo_aux LIKE TABLE OF tg_impo WITH HEADER LINE,
        tl_impo     LIKE TABLE OF tg_impo WITH HEADER LINE,
        wl_tabix    TYPE sy-tabix,
        wl_contab   LIKE LINE OF tg_contab,
        wa_tka02    TYPE tka02,
        v_count     TYPE i,
        vg_bseg(1),
        t_hkont     TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
        v_parid     TYPE zfiwrt0008-parid,
        v_koart     TYPE tbsl-koart,
        v_dmbtr     TYPE zfiwrt0011-dmbtr,
        v_vbund     TYPE bseg-vbund.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'CONTAS_EC-CS'
    TABLES
      set_values    = t_hkont
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  SELECT SINGLE *
    FROM tka02
    INTO wa_tka02
    WHERE bukrs = p_bukrs.

  SELECT SINGLE *
     FROM zfiwrt0001
     INTO w_zfiwrt0001
      WHERE operacao EQ p_operacao.

  CONCATENATE 'CE4' wa_tka02-kokrs '_ACCT' INTO DATA(tabco1).
  CONCATENATE 'CE4' wa_tka02-kokrs         INTO DATA(tabco2).

  REFRESH: tl_impo, tl_impo_aux.
  CLEAR: tl_impo, tl_impo_aux, v_koart.

  LOOP AT tg_contab.
    READ TABLE tg_tbsl
    WITH KEY bschl = tg_contab-bschl.
    IF tg_tbsl-koart = 'D' OR tg_tbsl-koart = 'K'.
      v_koart = tg_tbsl-koart.
    ENDIF.
    MOVE: 0 TO tg_contab-dmbtr.
    MODIFY tg_contab.
  ENDLOOP.


  LOOP AT tg_itens.
    PERFORM monta_impostos TABLES tl_impo_aux
                           USING sy-tabix.

    LOOP AT tl_impo_aux.
      MOVE-CORRESPONDING tl_impo_aux TO tl_impo.
      COLLECT tl_impo.
    ENDLOOP.
    REFRESH: tl_impo_aux.
  ENDLOOP.

*-CS2020001331 - 06.10.2021 - JT - inicio
  READ TABLE tl_impo WITH KEY taxtyp = c_icop.
  IF sy-subrc = 0.
    LOOP AT tg_contab.
      tg_contab-taxtyp = c_icop.
      MODIFY tg_contab INDEX sy-tabix.
    ENDLOOP.
  ENDIF.
*-CS2020001331 - 06.10.2021 - JT - fim

  LOOP AT tg_contab.
    wl_tabix = sy-tabix.
    READ TABLE tg_tbsl
    WITH KEY bschl = tg_contab-bschl.

    "Sociedade Parceira
    CLEAR: tg_contab-vbund, v_vbund.
    READ TABLE t_hkont WITH KEY from = tg_contab-hkont.
    IF sy-subrc = 0.
      IF v_koart = 'D'.
        SELECT SINGLE vbund INTO v_vbund FROM kna1
          WHERE kunnr = p_parid "soc parceira do emissor
          AND   ktokd IN ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ').
      ELSEIF v_koart = 'K'.
        SELECT SINGLE vbund INTO v_vbund FROM lfa1
          WHERE lifnr = p_parid "soc parceira do emissor
          AND   ktokk IN ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ').
      ENDIF.
      tg_contab-vbund = v_vbund.
      MODIFY tg_contab INDEX wl_tabix TRANSPORTING vbund.
    ENDIF.
    "

    IF tg_contab-taxtyp IS INITIAL.
      IF wg_fiscal-complemento = 'S' OR w_zfiwrt0001-complement_icms = 'S'.
        LOOP AT tl_impo
          WHERE taxtyp EQ c_icm3.
          IF tg_tbsl-shkzg EQ c_h.
            SUBTRACT  tl_impo-taxval FROM tg_contab-dmbtr.
          ELSE.
            ADD tl_impo-taxval TO tg_contab-dmbtr.
          ENDIF.
        ENDLOOP.
      ENDIF.
      MODIFY tg_contab INDEX wl_tabix.
      IF wg_fiscal-energia EQ c_n .
        LOOP AT tg_itens.
          IF tg_tbsl-shkzg EQ c_h.
            SUBTRACT  tg_itens-netwr FROM tg_contab-dmbtr.
*          AT LAST.
*            MULTIPLY tg_contab-dmbtr BY -1.
*          ENDAT.
          ELSE.
            ADD tg_itens-netwr TO tg_contab-dmbtr.
          ENDIF.
        ENDLOOP.
      ELSEIF wg_fiscal-energia EQ c_s.
        LOOP AT tl_impo
          WHERE taxtyp EQ c_ics1.
          IF tg_tbsl-shkzg EQ c_h.

*====== "Inicio USER STORY 81382  "Anderson Oenning
*            SUBTRACT  tl_impo-base FROM tg_contab-dmbtr.
            CLEAR: v_dmbtr.
            v_dmbtr = ( tg_itens-netwr + tl_impo-taxval ). "Valor total do item + imposto
            SUBTRACT  v_dmbtr FROM tg_contab-dmbtr.

          ELSE.
*            ADD tl_impo-base TO tg_contab-dmbtr.
            CLEAR: v_dmbtr.
            v_dmbtr = ( tg_itens-netwr + tl_impo-taxval ). "Valor total do item + imposto
            ADD v_dmbtr TO tg_contab-dmbtr.
*            ADD tl_impo-base TO tg_contab-dmbtr.
*====== "Inicio USER STORY 81382  "Anderson Oenning
          ENDIF.
        ENDLOOP.
      ENDIF.
      MODIFY tg_contab INDEX wl_tabix.
    ELSE.
      READ TABLE tl_impo
        WITH KEY taxtyp = tg_contab-taxtyp.
      IF sy-subrc IS INITIAL.
        IF tg_tbsl-shkzg EQ c_h.
          MOVE: tl_impo-taxval TO tg_contab-dmbtr.
          MULTIPLY tg_contab-dmbtr BY -1.
        ELSE.
          MOVE: tl_impo-taxval TO tg_contab-dmbtr.
        ENDIF.
        MODIFY tg_contab INDEX wl_tabix.
      ENDIF.

    ENDIF.

    CLEAR: wl_tabix, tl_impo, tg_tbsl.
  ENDLOOP.

  CLEAR vg_bseg.
  LOOP AT tg_docrefs INTO DATA(wa_docrefs).

    SELECT * INTO TABLE @DATA(itens_documento)
      FROM j_1bnflin
     WHERE docnum EQ @wa_docrefs-docnum.

    SELECT SINGLE parid
      FROM j_1bnfdoc
      INTO v_parid
      WHERE docnum EQ wa_docrefs-docnum.

    LOOP AT itens_documento INTO DATA(wa_itens_documento).

*-CS2020001331 - 06.10.2021 - JT - inicio
      IF wa_itens_documento-refkey IS NOT INITIAL.
        CONCATENATE wa_itens_documento-refkey '%' INTO wa_itens_documento-refkey.

        SELECT * INTO TABLE @DATA(it_bkpf)
          FROM bkpf
         WHERE awkey LIKE @wa_itens_documento-refkey.
      ELSE.
        sy-subrc = 4.
      ENDIF.
*-CS2020001331 - 06.10.2021 - JT - fim

      IF sy-subrc IS INITIAL.

* ---> S4 Migration - 19/06/2023 - JS
*        SELECT * INTO TABLE @DATA(it_bseg)
*          FROM bseg
*           FOR ALL ENTRIES IN @it_bkpf
*         WHERE bukrs EQ @it_bkpf-bukrs
*           AND belnr EQ @it_bkpf-belnr
*           AND gjahr EQ @it_bkpf-gjahr.
        DATA it_bseg TYPE TABLE OF bseg.

        CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
          EXPORTING
            it_for_all_entries = it_bkpf
            i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
          IMPORTING
            et_bseg            = it_bseg
          EXCEPTIONS
            not_found          = 1.

        IF sy-subrc = 0 AND lines( it_bseg ) > 0.
          sy-dbcnt = lines( it_bseg ).
        ELSE.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ENDIF.
* <--- S4 Migration - 19/06/2023 - JS

        SELECT * INTO TABLE @DATA(it_tbsl)
          FROM tbsl
           FOR ALL ENTRIES IN @it_bseg
         WHERE bschl EQ @it_bseg-bschl.

        SORT it_tbsl BY bschl.
        CLEAR v_koart.
        LOOP AT it_bseg INTO DATA(wa_bseg2).
          READ TABLE it_tbsl INTO DATA(wa_tbsl2) WITH KEY bschl = wa_bseg2-bschl BINARY SEARCH.
          IF wa_tbsl2-koart = 'D' OR wa_tbsl2-koart = 'K'.
            v_koart = wa_tbsl2-koart.
          ENDIF.
        ENDLOOP.


        LOOP AT it_bseg INTO DATA(wa_bseg).
          vg_bseg = 'X'.
          CLEAR: wl_contab.
          READ TABLE it_bkpf INTO DATA(wa_bkpf) WITH KEY bukrs = wa_bseg-bukrs
                                                         belnr = wa_bseg-belnr
                                                         gjahr = wa_bseg-gjahr.
          wl_contab-waers   = wa_bkpf-waers.
          wl_contab-waers_i = wa_bkpf-hwaer.
          wl_contab-curha	  = wa_bkpf-hwae2.
          wl_contab-curin	  = wa_bkpf-hwae3.
          READ TABLE it_tbsl INTO DATA(wa_tbsl) WITH KEY bschl = wa_bseg-bschl BINARY SEARCH.

          CASE wa_tbsl-koart.
            WHEN 'D'.
              wl_contab-hkont  = wa_bseg-kunnr.
            WHEN 'K'.
              wl_contab-hkont  = wa_bseg-lifnr.
            WHEN OTHERS.
              wl_contab-hkont  = wa_bseg-hkont.
          ENDCASE.

          wl_contab-bschl  = wa_tbsl-stbsl.
          "WL_CONTAB-ZLSCH  = 'P'.
*
          IF wa_tbsl-shkzg EQ c_h.
            ADD wa_bseg-dmbtr TO wl_contab-dmbtr.
            wl_contab-dmbtr = wl_contab-dmbtr * -1.
            "
            ADD wa_bseg-dmbe2 TO wl_contab-dmbe2.
            MULTIPLY wl_contab-dmbe2 BY -1.
            "
            ADD wa_bseg-dmbe3 TO wl_contab-dmbe3.
            MULTIPLY wl_contab-dmbe3 BY -1.

            ADD wa_bseg-wrbtr TO wl_contab-wrbtr.
            MULTIPLY wl_contab-wrbtr BY -1.
          ELSE.
            ADD wa_bseg-dmbtr TO wl_contab-dmbtr.
            ADD wa_bseg-dmbe2 TO wl_contab-dmbe2.
            ADD wa_bseg-dmbe3 TO wl_contab-dmbe3.
            ADD wa_bseg-wrbtr TO wl_contab-wrbtr.
          ENDIF.

          CLEAR wl_contab-artnr.
          IF wa_bseg-paobjnr IS NOT INITIAL AND wa_bseg-paobjnr GT 0.
            CLEAR v_count.
            SELECT COUNT(*) FROM  dd02l
                INTO    v_count
                WHERE   tabname = tabco1
                AND     as4local  = 'A'.
            IF v_count GT 0.
              SELECT SINGLE artnr
                FROM (tabco1) "CE4MAGI_ACCT
                INTO wl_contab-artnr
              WHERE paobjnr = wa_bseg-paobjnr.
            ENDIF.

            IF sy-subrc NE 0 OR v_count = 0.
              CLEAR  v_count.
              SELECT COUNT(*) FROM  dd02l
                INTO    v_count
                WHERE   tabname = tabco2
                AND     as4local  = 'A'.
              IF v_count GT 0.
                SELECT SINGLE artnr
                  FROM (tabco2)
                  INTO wl_contab-artnr
                WHERE paobjnr = wa_bseg-paobjnr.
              ENDIF.
            ENDIF.
          ENDIF.
          "
          wl_contab-taxtyp  = wa_bseg-taxps.
          "WL_CONTAB-ESTORNO = ABAP_TRUE.
          "WL_CONTAB-NEWBW  = .
          "WL_CONTAB-ZLSCH  = .
          wl_contab-zfbdt   = wa_bseg-zfbdt.
          wl_contab-kostl   = wa_bseg-kostl.
          wl_contab-umskz   = wa_bseg-umsks.
          "Sociedade Parceira
          CLEAR: tg_contab-vbund, v_vbund.
          READ TABLE t_hkont WITH KEY from = wl_contab-hkont.
          IF sy-subrc = 0.
            IF v_koart = 'D'.
              SELECT SINGLE vbund INTO v_vbund FROM kna1
                WHERE kunnr = v_parid "soc parceira do emissor
                AND   ktokd IN ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ').
              wl_contab-vbund = v_vbund.
            ELSEIF v_koart = 'K'.
              SELECT SINGLE vbund INTO v_vbund FROM lfa1
                WHERE lifnr = v_parid "soc parceira do emissor
                AND   ktokk IN ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ').
              wl_contab-vbund = v_vbund.
            ENDIF.
          ENDIF.
          "
          APPEND wl_contab TO tg_contab.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF vg_bseg = 'X'.
    DELETE tg_contab WHERE dmbtr = 0.
  ENDIF.

  SORT tg_contab BY taxtyp bschl.
ENDFORM.                    " MONTA_CONTABIL
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE trata_fields OUTPUT.

  DATA: wa_setleaf         TYPE setleaf,
        wa_zfiwrt0026      TYPE zfiwrt0026,
        vg_contingencia(1).
  CLEAR vg_contingencia.
  SELECT COUNT( * )
             FROM tvarvc
             WHERE name = 'FAT_CONTINGENCIA_GOLIVE_US'
             AND low  = sy-uname.
  IF sy-subrc = 0.
    vg_contingencia = 'X'.
  ENDIF.



  CLEAR: wl_1baa.
  SELECT SINGLE *
    FROM j_1baa
    INTO wl_1baa
     WHERE nftype EQ wg_fiscal-nftype.

  LOOP AT tg_fields.
    LOOP AT SCREEN.
      IF ( screen-name EQ tg_fields-campo
      OR screen-group1 EQ tg_fields-group1 ).
        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

*  CLEAR: wa_setleaf.
*  SELECT SINGLE *
*    FROM setleaf
*    INTO wa_setleaf
*   WHERE setname = 'VF01_USUARIO'
*     AND valfrom = sy-uname.
  "AUTHORITY-CHECK OBJECT 'ZNFW_BUDAT' ID 'ACTVT' FIELD '03'.

  CLEAR: wa_zfiwrt0026.
  SELECT SINGLE *
    FROM zfiwrt0026
    INTO wa_zfiwrt0026
  WHERE usname EQ  sy-uname.

  IF ( sy-subrc IS INITIAL ).
    LOOP AT SCREEN.
      IF ( screen-name EQ 'WG_DOCS-BUDAT' ) AND ( wg_docs-budat IS NOT INITIAL ).
        screen-input     = c_1.
        screen-invisible = c_0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wg_docs-docnum IS INITIAL.
    READ TABLE tg_fields
      WITH KEY group1 = 'GR1'.
    IF sy-subrc IS INITIAL.
      IF wg_fiscal-zpesagem NE '01'.
        LOOP AT SCREEN.
          IF screen-name EQ 'WG_FISCAL-NR_ROMANEIO'.
            screen-input     =  c_0.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0

            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
      "ALRS
      LOOP AT SCREEN.
        IF  wg_fiscal-lm_estoque = 'S'.
          IF screen-name EQ 'WG_FISCAL-INCO1'      OR
             screen-name EQ 'WG_FISCAL-INCO2'      OR
             screen-name EQ 'WG_FISCAL-DOCREF'     OR
             screen-name EQ 'WG_FISCAL-REFERENCIA'.
            screen-input     =  c_0.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_1.      "INVISIBLE 1 VISIBLE 0
            MODIFY SCREEN.
          ENDIF.
        ENDIF.

        IF screen-name EQ 'WG_FISCAL-TP_MV_IMOB' OR
           screen-name EQ 'WG_FISCAL-KOSTL' OR
           screen-name EQ 'TXTCCUSTO'.
          IF wg_fiscal-imobilizado EQ 'S' .
            "SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0
          ELSE.
            "SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_1.      "INVISIBLE 1 VISIBLE 0
          ENDIF.
          IF screen-name EQ 'WG_FISCAL-KOSTL' OR
             screen-name EQ 'TXTCCUSTO'.
            IF wg_fiscal-tp_mv_imob = 'T'.  " transferência
              screen-input     =  c_1.      "INPUT 1     NO INPUT 0
              screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0
            ELSE.
              screen-input     =  c_0.      "INPUT 1     NO INPUT 0
              screen-invisible =  c_1.      "INVISIBLE 1 VISIBLE 0
            ENDIF.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.

        "04.10.2018
        IF screen-name EQ 'WG_FISCAL-ACCESS_KEY' OR
           screen-name EQ 'TXT_CHAVE_ACESSO'.

          "18.02.2019 CS2018003078
          IF vlancamento_znfw0009 IS NOT INITIAL.
            screen-input     =  c_0.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0
          ELSE.
            screen-input     =  c_0.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_1.      "INVISIBLE 1 VISIBLE 0
          ENDIF.

*          IF ( WL_1BAA-NFE    EQ 'X'     ) AND
*             ( WL_1BAA-FORM   IS INITIAL ) AND
*             ( WL_1BAA-DIRECT EQ '2' OR WL_1BAA-NFTYPE EQ 'YI' ).
*            SCREEN-INPUT     =  C_1.      "INPUT 1     NO INPUT 0
*            SCREEN-INVISIBLE =  C_0.      "INVISIBLE 1 VISIBLE 0
*          ELSE.
*            SCREEN-INPUT     =  C_0.      "INPUT 1     NO INPUT 0
*            SCREEN-INVISIBLE =  C_1.      "INVISIBLE 1 VISIBLE 0
*          ENDIF.
          MODIFY SCREEN.
        ENDIF.


        "11.10.2018
        IF screen-name EQ 'WG_FISCAL-KONTO'.
          IF wg_fiscal-lm_estoque = 'S'. "alrs
            screen-input     =  c_1.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0
          ELSE.
            screen-input     =  c_0.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_1.      "INVISIBLE 1 VISIBLE 0
          ENDIF.
        ENDIF.

      ENDLOOP.

      IF wg_fiscal-retorno EQ 'N'.
        LOOP AT SCREEN.
          IF screen-name EQ 'WG_FISCAL-DOCREF'.
            screen-input     =  c_0.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0

            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.


      IF ( wg_fiscal-aviso_rec EQ 'S' ) OR
         ( wg_fiscal-vbeln IS INITIAL ) AND "Não é lançamento sobre Ordem Venda
         ( viniciou_lcto_znfw0009 IS NOT INITIAL ).

        LOOP AT SCREEN.
          IF screen-name EQ 'WG_FISCAL-EBELN'.
            screen-input     =  c_1.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0

            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF wl_1baa-form IS NOT INITIAL.
        LOOP AT SCREEN.
          IF screen-name EQ 'WG_DOCS-NFENUM'
          OR screen-name EQ 'WG_DOCS-SERIES'
          OR screen-name EQ 'WG_DOCS-BLDAT'
          OR screen-name EQ 'WG_DOCS-BUDAT'.
            IF screen-name NE 'WG_DOCS-BUDAT' AND vg_contingencia NE 'X'.
              screen-input     =  c_0.      "INPUT 1     NO INPUT 0
              screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0
            ENDIF.
            IF wg_fiscal-complemento = 'S' AND ( screen-name EQ 'WG_DOCS-BLDAT' OR screen-name EQ 'WG_DOCS-BUDAT' ).
              screen-input     =  c_1.      "INPUT 1     NO INPUT 0
              screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0
            ENDIF.
            MODIFY SCREEN.
          ENDIF.

        ENDLOOP.

        IF vlancamento_znfw0009 EQ abap_false.
          CLEAR: wg_docs-nfenum, wg_docs-series.
          IF wg_fiscal-complemento NE 'S' AND vg_contingencia NE 'X'..
            wg_docs-budat = sy-datum.
            wg_docs-bldat = sy-datum.
          ENDIF.
        ENDIF.
      ELSE.

        LOOP AT SCREEN.
          IF wg_fiscal-complemento = 'S' AND ( screen-name EQ 'WG_DOCS-BLDAT' OR screen-name EQ 'WG_DOCS-BUDAT' ).
            screen-input     =  c_1.      "INPUT 1     NO INPUT 0
            screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0

            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ELSE.

    ENDIF.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name EQ 'WG_DOCS-NFENUM'
      OR screen-name EQ 'WG_DOCS-SERIES'
      OR screen-name EQ 'WG_DOCS-BLDAT'
      OR screen-name EQ 'WG_DOCS-BUDAT'
      OR screen-name EQ 'WG_FISCAL-NR_ROMANEIO'
      OR screen-name EQ 'WG_FISCAL-DOCREF'
      OR screen-name EQ 'P_OPERACAO'
      OR screen-name EQ 'P_BUKRS'
      OR screen-name EQ 'P_BRANCH'
      OR screen-name EQ 'P_PARID'.
        IF screen-name NE 'WG_DOCS-BUDAT'.
          screen-input     =  c_0.      "INPUT 1     NO INPUT 0
          screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0
        ENDIF.
        IF wg_fiscal-complemento = 'S' AND ( screen-name EQ 'WG_DOCS-BLDAT' OR screen-name EQ 'WG_DOCS-BUDAT' ).
          screen-input     =  c_1.      "INPUT 1     NO INPUT 0
          screen-invisible =  c_0.      "INVISIBLE 1 VISIBLE 0
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF  screen-name EQ 'PBADD'
      OR screen-name EQ 'PBDEL'
      OR screen-name EQ 'PBSAVE'.
        screen-input     =  c_0.      "INPUT 1     NO INPUT 0
        screen-invisible =  c_1.      "INVISIBLE 1 VISIBLE 0
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF wg_docs-docnum IS INITIAL.
    IF w_zfiwrt0001-complement_icms = 'S' AND ( wg_acao = c_modif OR wg_acao = c_add ).
      LOOP AT SCREEN.
        IF screen-name EQ 'WG_DIREITOS-TAXLW1' OR
           screen-name EQ 'WG_DIREITOS-TAXLW2' OR
           screen-name EQ 'WG_DIREITOS-TAXLW4' OR
           screen-name EQ 'WG_DIREITOS-TAXLW5'.
          screen-input     =  c_1.      "INPUT 1     NO INPUT 0
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field."'WG_DESC_OPERACAO'.
  ENDIF.
** Depois de tratados os valores da tabela tem que ser eliminados para um proximo tratamento.
*  REFRESH tg_fields.
  CLEAR: tg_fields.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0112   text
*      -->P_C_0  text
*----------------------------------------------------------------------*
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
*&      Module  SEARCH_SEQ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_seq INPUT.
  DATA: BEGIN OF tl_seq_lanc OCCURS 0,
          seq_lcto  TYPE zfiwrt0008-seq_lcto,
          operacao  TYPE zfiwrt0008-operacao,
          bukrs     TYPE zfiwrt0008-bukrs,
          branch    TYPE zfiwrt0008-branch,
          parvw     TYPE zfiwrt0008-parvw,
          parid     TYPE zfiwrt0008-parid,
          docnum    TYPE zfiwrt0008-docnum,
          nronf     TYPE zfiwrt0008-nfenum,
          descricao TYPE zfiwrt0001-descricao,
          name      TYPE lfa1-name1,
        END OF tl_seq_lanc.

  DATA: tl_lfa1   TYPE TABLE OF lfa1 WITH HEADER LINE,
        tl_kna1   TYPE TABLE OF kna1 WITH HEADER LINE,
        tl_0001   TYPE TABLE OF zfiwrt0001 WITH HEADER LINE,
        tl_bnfdoc TYPE TABLE OF j_1bnfdoc WITH HEADER LINE.

  REFRESH: tl_seq_lanc, t_fieldtab, tl_bnfdoc, tl_lfa1, tl_kna1, tl_0001.
  CLEAR:   tl_seq_lanc, t_fieldtab, tl_bnfdoc, tl_lfa1, tl_kna1, tl_0001.

  SELECT seq_lcto operacao bukrs branch parvw parid
         docnum nfenum
    FROM zfiwrt0008
    INTO TABLE tl_seq_lanc.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM lfa1
       INTO TABLE tl_lfa1
       FOR ALL ENTRIES IN tl_seq_lanc
        WHERE lifnr EQ tl_seq_lanc-parid.

    SELECT *
      FROM kna1
      INTO TABLE tl_kna1
       FOR ALL ENTRIES IN tl_seq_lanc
        WHERE kunnr EQ tl_seq_lanc-parid.

    SELECT *
      FROM zfiwrt0001
      INTO TABLE tl_0001
       FOR ALL ENTRIES IN tl_seq_lanc
        WHERE operacao EQ tl_seq_lanc-operacao.

    SELECT *
      FROM j_1bnfdoc
      INTO TABLE tl_bnfdoc
       FOR ALL ENTRIES IN tl_seq_lanc
       WHERE docnum EQ tl_seq_lanc-docnum.

  ENDIF.

  SORT: tl_kna1 BY kunnr,
        tl_lfa1 BY lifnr,
        tl_0001 BY operacao,
        tl_bnfdoc BY docnum.

  LOOP AT tl_seq_lanc.
    IF tl_seq_lanc-parvw EQ c_ag.
      READ TABLE tl_kna1
        WITH KEY kunnr = tl_seq_lanc-parid
                 BINARY SEARCH.
      MOVE: tl_kna1-name1 TO tl_seq_lanc-name.
    ELSEIF tl_seq_lanc-parvw EQ c_lf
        OR tl_seq_lanc-parvw EQ c_br.
      READ TABLE tl_lfa1
        WITH KEY lifnr = tl_seq_lanc-parid
                 BINARY SEARCH.
      MOVE: tl_lfa1-name1 TO tl_seq_lanc-name.
    ENDIF.
    READ TABLE tl_0001
      WITH KEY operacao = tl_seq_lanc-operacao
                   BINARY SEARCH.

    READ TABLE tl_bnfdoc
      WITH KEY docnum = tl_seq_lanc
               BINARY SEARCH.

    MOVE: tl_0001-descricao TO tl_seq_lanc-descricao.

    IF tl_bnfdoc-nfenum IS INITIAL.
      MOVE: tl_bnfdoc-nfnum TO tl_seq_lanc-nronf.
    ELSE.
      MOVE: tl_bnfdoc-nfenum TO tl_seq_lanc-nronf.
    ENDIF.
    MODIFY tl_seq_lanc.
    CLEAR: tl_bnfdoc, tl_lfa1, tl_kna1, tl_0001.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SEQ_LCTO'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'P_SEQ_LCTO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_seq_lanc
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_SEQ  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados_doc .
  DATA: wl_0001       TYPE zfiwrt0001,
        wl_0008       TYPE zfiwrt0008,
        tl_0009       TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
        tl_0010       TYPE TABLE OF zfiwrt0010 WITH HEADER LINE,
        tl_0011       TYPE TABLE OF zfiwrt0011 WITH HEADER LINE,
        tl_0012       TYPE TABLE OF zfiwrt0012 WITH HEADER LINE,
        tl_0013       TYPE TABLE OF zfiwrt0013 WITH HEADER LINE,
        tl_0015       TYPE TABLE OF zfiwrt0015 WITH HEADER LINE,
        tl_0020       TYPE TABLE OF zfiwrt0020 WITH HEADER LINE,
        tl_1baj       TYPE TABLE OF j_1baj     WITH HEADER LINE,
        tl_1bajt      TYPE TABLE OF j_1bajt    WITH HEADER LINE,
        tl_tbsl       TYPE TABLE OF tbsl       WITH HEADER LINE,
        tl_skat       TYPE TABLE OF skat       WITH HEADER LINE,
        tl_cskb       TYPE TABLE OF cskb       WITH HEADER LINE,
        tl_makt       TYPE TABLE OF makt       WITH HEADER LINE,
        tl_0005       TYPE TABLE OF zfiwrt0005 WITH HEADER LINE,
        tl_0007       TYPE TABLE OF zfiwrt0007 WITH HEADER LINE,
        tl_user       TYPE TABLE OF user_addr  WITH HEADER LINE,
        wl_0019       TYPE zfiwrt0019,
        tl_knpar      TYPE TABLE OF kna1       WITH HEADER LINE,
        tl_lfpar      TYPE TABLE OF lfa1       WITH HEADER LINE,
        tl_marc       TYPE TABLE OF marc       WITH HEADER LINE,
        wl_active     TYPE j_1bnfe_active,
        wl_zib_chv    TYPE zib_contabil_chv,
        wl_kna1       TYPE kna1,
        wl_lfa1       TYPE lfa1,
        wl_t001w      TYPE t001w,
        wl_t001       TYPE t001,
        wl_1bad       TYPE j_1bad,
        wl_1badt      TYPE j_1badt,
        wl_1baa       TYPE j_1baa,
        wl_zib_objkey TYPE zib_contabil_chv-obj_key.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: lt_returns         TYPE TABLE OF bapiret2,
        ls_coeldes         TYPE bapi1030_ceoutputlist,
        lv_controllingarea TYPE bapi1030_gen-co_area,
        lv_costelement     TYPE bapi1030_gen-cost_elem,
        lv_keydate         TYPE bapi1030_gen-some_date.
* <--- S4 Migration - 18/07/2023 - CA
  REFRESH: tl_0009, tl_0010, tl_0011, tl_0012, tl_0013, tl_0015, tl_1baj, tl_1bajt, tl_tbsl,
        tl_tbsl, tl_skat, tl_makt, tl_0005, tl_0007, tl_user, tl_knpar, tl_lfpar,
        tl_marc, tl_cskb.

  IF p_seq_lcto IS NOT INITIAL.
    SELECT SINGLE *
      FROM zfiwrt0008
      INTO wl_0008
       WHERE seq_lcto EQ p_seq_lcto.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº de Documento não encontrado!'.
      LEAVE TO SCREEN 100.
    ELSE.

      SELECT SINGLE prazo FROM zsdt_retlote INTO vl_prazo WHERE docnum_ret = wl_0008-docnum_retorno.

      SELECT SINGLE * FROM zfiwrt0019 INTO wl_0019 WHERE seq_lcto EQ wl_0008-seq_lcto.

      SELECT * INTO TABLE tl_0020 FROM zfiwrt0020 WHERE seq_lcto EQ wl_0008-seq_lcto.

      SELECT SINGLE * FROM j_1baa INTO wl_1baa WHERE nftype EQ wl_0008-nftype.

      SELECT SINGLE * FROM t001w INTO wl_t001w WHERE werks EQ wl_0008-branch.

      SELECT SINGLE * FROM j_1bnfe_active INTO wl_active WHERE docnum EQ wl_0008-docnum.

      CONCATENATE 'ZGF' wl_0008-seq_lcto wl_0008-budat(4) INTO wl_zib_objkey.

      SELECT SINGLE * FROM zib_contabil_chv INTO wl_zib_chv WHERE obj_key EQ wl_zib_objkey.

      SELECT SINGLE * FROM zfiwrt0001 INTO wl_0001 WHERE operacao EQ wl_0008-operacao.

      SELECT * FROM zfiwrt0005 INTO TABLE tl_0005 WHERE operacao EQ wl_0008-operacao.

      IF wl_0008-parvw EQ c_ag.
        SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wl_0008-parid.

      ELSEIF wl_0008-parvw EQ c_br
        OR   wl_0008-parvw EQ c_lf.
        SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ wl_0008-parid.

      ENDIF.
      SELECT * FROM zfiwrt0015 INTO TABLE tl_0015 WHERE seq_lcto EQ wl_0008-seq_lcto.

      IF sy-subrc IS INITIAL.
        SELECT * FROM lfa1 INTO TABLE tl_lfpar FOR ALL ENTRIES IN tl_0015 WHERE lifnr EQ tl_0015-parid.
        SELECT * FROM kna1 INTO TABLE tl_knpar FOR ALL ENTRIES IN tl_0015 WHERE kunnr EQ tl_0015-parid.
      ENDIF.

      SELECT *
        FROM zfiwrt0009 INTO TABLE tl_0009
       WHERE seq_lcto EQ wl_0008-seq_lcto.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM makt
          INTO TABLE tl_makt
          FOR ALL ENTRIES IN tl_0009
           WHERE spras EQ sy-langu
             AND matnr EQ tl_0009-matnr.

        SELECT *
          FROM marc
          INTO TABLE tl_marc
           FOR ALL ENTRIES IN tl_0009
           WHERE matnr EQ tl_0009-matnr
             AND werks EQ tl_0009-bwkey.
      ENDIF.

      SELECT *
        FROM zfiwrt0010
        INTO TABLE tl_0010
         WHERE seq_lcto EQ wl_0008-seq_lcto.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM j_1baj
          INTO TABLE tl_1baj
           FOR ALL ENTRIES IN tl_0010
           WHERE taxtyp EQ tl_0010-taxtyp.

        SELECT *
          FROM j_1bajt
          INTO TABLE tl_1bajt
           FOR ALL ENTRIES IN tl_0010
          WHERE  spras  EQ sy-langu
            AND  taxtyp EQ tl_0010-taxtyp.

      ENDIF.

      SELECT *
        FROM zfiwrt0011
        INTO TABLE tl_0011
         WHERE seq_lcto EQ wl_0008-seq_lcto.
      IF sy-subrc IS INITIAL.
        SELECT *
          FROM tbsl
          INTO TABLE tl_tbsl
           FOR ALL ENTRIES IN tl_0011
           WHERE bschl EQ tl_0011-bschl.

        SELECT *
          FROM skat
          INTO TABLE tl_skat
           FOR ALL ENTRIES IN tl_0011
            WHERE spras EQ sy-langu
              AND ktopl EQ c_50
              AND saknr EQ tl_0011-hkont.

* ---> S4 Migration - 18/07/2023 - CA
*        SELECT *
*          FROM cskb
*          INTO TABLE tl_cskb
*          FOR ALL ENTRIES IN tl_0011
*            WHERE kstar EQ tl_0011-hkont
*              AND ( datbi GE wl_0008-dt_criacao
*                AND datab LE wl_0008-dt_criacao )
*              AND katyp EQ '01'.

        SELECT kokrs UP TO 1 ROWS
          FROM tka02
          INTO @DATA(lv_kokrs)
          WHERE bukrs = @wl_0008-bukrs.
        ENDSELECT.
        IF sy-subrc = 0.

          LOOP AT tl_0011 INTO DATA(ls_0011).

            lv_controllingarea  = lv_kokrs.
            lv_costelement      = ls_0011-hkont.
            lv_keydate          = wl_0008-dt_criacao.

            CLEAR: lt_returns[], ls_coeldes.

            CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
              EXPORTING
                controllingarea   = lv_controllingarea
                costelement       = lv_costelement
                keydate           = lv_keydate
              IMPORTING
                costelementdetail = ls_coeldes
              TABLES
                return            = lt_returns.

            READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
            IF sy-subrc <> 0.
              tl_cskb-kokrs = lv_kokrs.
              tl_cskb-kstar = ls_0011-hkont.
              tl_cskb-katyp = ls_coeldes-celem_category.



              APPEND tl_cskb.
              CLEAR tl_cskb.
            ENDIF.

            CLEAR ls_0011.
          ENDLOOP.
        ENDIF.
* <--- S4 Migration - 18/07/2023 - CA
      ENDIF.
      SELECT *
        FROM zfiwrt0012
        INTO TABLE tl_0012
         WHERE seq_lcto EQ wl_0008-seq_lcto.

      SELECT *
        FROM zfiwrt0013
        INTO TABLE tl_0013
         WHERE seq_lcto EQ wl_0008-seq_lcto.

      SELECT *
       FROM zfiwrt0007
       INTO TABLE tl_0007
        WHERE operacao EQ wl_0008-operacao
          AND tipo     EQ c_w.

      IF tl_0007[] IS NOT INITIAL.
        SELECT *
          FROM user_addr
          INTO TABLE tl_user
           FOR ALL ENTRIES IN tl_0007
            WHERE bname EQ tl_0007-usnam.

      ENDIF.

      w_zfiwrt0001 = wl_0001.  "*-CS2023000043-09.02.2023-#102019-JT

      PERFORM preenche_campos_doc TABLES tl_0009
                                         tl_0010
                                         tl_0011
                                         tl_0012
                                         tl_0013
                                         tl_0015
                                         tl_0020
                                         tl_tbsl
                                         tl_skat
                                         tl_cskb
                                         tl_1baj
                                         tl_1bajt
                                         tl_makt
                                         tl_0005
                                         tl_0007
                                         tl_user
                                         tl_knpar
                                         tl_lfpar
                                         tl_marc
                                  USING  wl_0001
                                         wl_0008
                                         wl_kna1
                                         wl_lfa1
                                         wl_t001w
                                         wl_active
                                         wl_1baa
                                         wl_zib_chv
                                         wl_0019.

    ENDIF.
  ENDIF.
ENDFORM.                    " BUSCA_DADOS_DOC
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMPOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0009  text
*      -->P_TL_0010  text
*      -->P_TL_0011  text
*      -->P_TL_0012  text
*      -->P_TL_0013  text
*      -->P_TL_TBSL  text
*      -->P_TL_SKAT  text
*      -->P_TL_1BAJ  text
*      -->P_TL_1BAJT  text
*      -->P_WL_0001  text
*      -->P_WL_0008  text
*      -->P_WL_KNA1  text
*      -->P_WL_LFA1  text
*      -->P_WL_T001W  text
*----------------------------------------------------------------------*
FORM preenche_campos_doc  TABLES   tl_0009 STRUCTURE zfiwrt0009
                                   tl_0010 STRUCTURE zfiwrt0010
                                   tl_0011 STRUCTURE zfiwrt0011
                                   tl_0012 STRUCTURE zfiwrt0012
                                   tl_0013 STRUCTURE zfiwrt0013
                                   tl_0015 STRUCTURE zfiwrt0015
                                   tl_0020 STRUCTURE zfiwrt0020
                                   tl_tbsl STRUCTURE tbsl
                                   tl_skat STRUCTURE skat
                                   tl_cskb STRUCTURE cskb
                                   tl_1baj STRUCTURE j_1baj
                                   tl_1bajt STRUCTURE j_1bajt
                                   tl_makt  STRUCTURE makt
                                   tl_0005  STRUCTURE zfiwrt0005
                                   tl_0007  STRUCTURE zfiwrt0007
                                   tl_user  STRUCTURE user_addr
                                   tl_knpar STRUCTURE kna1
                                   tl_lfpar STRUCTURE lfa1
                                   tl_marc  STRUCTURE marc
                          USING    wl_0001 TYPE zfiwrt0001
                                   wl_0008 TYPE zfiwrt0008
                                   wl_kna1 TYPE kna1
                                   wl_lfa1 TYPE lfa1
                                   wl_t001w TYPE t001w
                                   wl_active TYPE j_1bnfe_active
                                   wl_1baa   TYPE j_1baa
                                   wl_zib_chv TYPE zib_contabil_chv
                                   wl_0019    TYPE zfiwrt0019.

  DATA: wl_indcoper         TYPE zfiwrt0006-indcoper,
        wl_texto_fiscal(30),
        tl_values           TYPE vrm_values,
        wl_values           TYPE LINE OF vrm_values,
        wl_cont             TYPE sy-tabix,
        wl_cont_aux         TYPE sy-tabix,
        wl_cont_aux2        TYPE sy-tabix,
        wl_impo_comp        LIKE LINE OF tg_impo_comp,
        wl_0011             TYPE zfiwrt0011,
        "TL_ZSDT0075 TYPE TABLE OF TY_ZSDT0075,
        wl_0075             TYPE zsdt0075,
        i_ebelp             TYPE ekpo-ebelp,
        it_j_1bnfdoc        TYPE TABLE OF j_1bnfdoc WITH HEADER LINE.

  p_operacao = wl_0008-operacao.
  p_bukrs    = wl_0008-bukrs.
  p_branch   = wl_0008-branch.
  p_parvw    = wl_0008-parvw.
  p_parid    = wl_0008-parid.

  IF wl_0008-parvw = 'AG'.
    READ TABLE tl_0011 INTO wl_0011
    WITH KEY bschl = '01'
             seq_lcto = wl_0008-seq_lcto.

    v_zlsch = wl_0011-zlsch.

    SELECT SINGLE *
      FROM zsdt0075
      INTO wl_0075
      WHERE kunnr = wl_0008-parid
      AND bdatu >= sy-datum.

    IF sy-subrc IS INITIAL.
      xbol = 'X'.
    ELSE.
      xbol = 'N'.
    ENDIF.
  ENDIF.

  IF wl_0008-loekz IS NOT INITIAL.
    p_loekz = '@11@'.
  ELSE.
    p_loekz = space.
  ENDIF.

** Preenche valores da tela.
***Dados Gerais
****Header
  MOVE: wl_0008-nftype       TO wg_fiscal-nftype,
        wl_0008-dias         TO wg_fiscal-dias,
        wl_0008-retorno      TO wg_fiscal-retorno,
        wl_0008-zpesagem     TO wg_fiscal-zpesagem,
        wl_0008-imobilizado  TO wg_fiscal-imobilizado,
        wl_0008-tp_mv_imob   TO wg_fiscal-tp_mv_imob,
        wl_0008-kostl        TO wg_fiscal-kostl,
        wl_0008-docref       TO wg_fiscal-docref,
        wl_0008-nr_romaneio  TO wg_fiscal-nr_romaneio,
        wl_0008-move_plant   TO wg_fiscal-move_plant,
        wl_0008-move_stloc   TO wg_fiscal-move_stloc,
        wl_0008-ctrl_zrfl    TO wg_fiscal-ctrl_zrfl,
        wl_0008-energia      TO wg_fiscal-energia,
        wl_0008-servico      TO wg_fiscal-servico,
        wl_0008-complemento  TO wg_fiscal-complemento,
        wl_0008-inco1        TO wg_fiscal-inco1,
        wl_0008-inco2        TO wg_fiscal-inco2,
        wl_0008-referencia   TO wg_fiscal-referencia,
        wl_0008-access_key   TO wg_fiscal-access_key,
        wl_0008-ebeln        TO wg_fiscal-ebeln,
        wl_0008-docnum       TO wg_docs-docnum,
        wl_0008-belnr        TO wg_docs-belnr,
        wl_0008-mblnr        TO wg_docs-mblnr,
        wl_0008-budat        TO wg_docs-budat,
        wl_0008-branch       TO wg_docs-branch,
        wl_0008-bldat        TO wg_docs-bldat,
        wl_0008-series       TO wg_docs-series,
        wl_0001-transf_icms  TO wg_fiscal-transf_icms,
        wl_0008-lm_estoque   TO wg_fiscal-lm_estoque,
        wl_0008-konto        TO wg_fiscal-konto,
        wl_0008-move_mat     TO wg_fiscal-move_mat,
        wl_0008-move_batch   TO wg_fiscal-move_batch,
        wl_0008-bktxt        TO wg_fiscal-bktxt,
        wl_0008-mtsnr        TO wg_fiscal-mtsnr,
        wl_0008-tcode_org    TO wg_docs-tcode_org,
        wl_0008-not_check_xml TO wg_docs-not_check_xml,
        wl_zib_chv-belnr     TO wg_docs-belnr.


  IF ( wl_1baa-form IS INITIAL ) OR ( wl_0008-tcode_org EQ 'ZNFW0009' ).
    MOVE: wl_0008-nfenum   TO wg_docs-nfenum.
  ELSE.
    MOVE: wl_active-nfnum9 TO wg_docs-nfenum.
  ENDIF.

  PERFORM f_define_origem_destino USING wl_kna1
                                        wl_lfa1
                                        wl_t001w
                                        wl_1baa
                               CHANGING wl_indcoper
                                        wl_texto_fiscal.

**  Busca texto de tipo de operação
  CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
    EXPORTING
      i_table_name = 'ZFIWRT0006'
      i_field_name = 'OPERTYP'
    IMPORTING
      e_t_list     = tl_values.

  MOVE: wl_0008-cfop     TO wg_direitos-cfop,
        wl_0008-taxlw1   TO wg_direitos-taxlw1,
        wl_0008-taxlw2   TO wg_direitos-taxlw2,
        wl_0008-taxlw4   TO wg_direitos-taxlw4,
        wl_0008-taxlw5   TO wg_direitos-taxlw5,
*        Wl_0008-indcoper TO wg_direitos-indcoper,
        wl_0008-opertyp  TO wg_direitos-opertyp,
        wl_0008-taxcode  TO wg_direitos-taxcode.

  READ TABLE tl_values INTO wl_values
      WITH KEY key = wg_direitos-opertyp.

  CONCATENATE wl_texto_fiscal '-' wl_values-text INTO wg_op_fiscal SEPARATED BY space.
***      descricao da operacao.
*  REFRESH: tg_editor.
*  MOVE: wl_0001-txt_compl TO wg_editor.
*  APPEND wg_editor TO tg_editor.
  REFRESH: tg_editor.
  CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
  wl_cont = strlen( wl_0008-txt_compl ).
  wl_cont_aux = wl_cont / 72.

  DO.
    MOVE: wl_0008-txt_compl+wl_cont_aux2 TO wg_editor-line.
    ADD 72 TO wl_cont_aux2.
    APPEND wg_editor TO tg_editor.

    IF wl_cont_aux2 GT wl_cont.
      EXIT.

    ENDIF.
  ENDDO.
  CALL METHOD obg_descbox->set_text_as_r3table
    EXPORTING
      table = tg_editor.

***Impostos
  REFRESH: tg_impo, tg_impo_comp.
  LOOP AT tl_0010.
    READ TABLE tl_1baj
      WITH KEY taxtyp = tl_0010-taxtyp.

    READ TABLE tl_1bajt
      WITH KEY taxtyp = tl_0010-taxtyp.

    MOVE: tl_0010-taxtyp    TO tg_impo-taxtyp,
          tl_1bajt-ttypetxt TO tg_impo-ttypetxt,
          tl_1baj-taxgrp    TO tg_impo-taxgrp.

    APPEND tg_impo.
    MOVE-CORRESPONDING: tl_0010 TO wl_impo_comp.
    APPEND wl_impo_comp TO tg_impo_comp.

    CLEAR: tg_impo.
  ENDLOOP.

  SORT: tg_impo      BY taxtyp ttypetxt taxgrp,
        tg_impo_comp BY itmnum taxtyp .

  DELETE ADJACENT DUPLICATES FROM tg_impo COMPARING ALL FIELDS.
***Contabilidade
  REFRESH: tg_contab, tg_contab-style2, style2.

*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
  SELECT SINGLE *
         INTO @DATA(wl_zfit0196)
         FROM zfit0196.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610

  LOOP AT tl_0011.
*    where estorno is initial.
    READ TABLE tl_skat
      WITH KEY saknr = tl_0011-hkont.

    READ TABLE tl_tbsl
      WITH KEY bschl = tl_0011-bschl.

    READ TABLE tl_cskb
      WITH KEY kstar = tl_0011-hkont.
    IF sy-subrc IS NOT INITIAL.
      wa_style-fieldname = 'KOSTL'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  wa_style INTO TABLE style2.
    ELSE.
      IF tl_0011-estorno IS NOT INITIAL.
        wa_style-fieldname = 'KOSTL'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style2.
      ENDIF.
    ENDIF.

    IF tl_tbsl-koart EQ c_k
    OR tl_tbsl-koart EQ c_d
    AND tl_0011-estorno IS INITIAL.
*      wa_style-fieldname = 'ZFBDT'.
*      wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*      insert  wa_style into table style2.
*
*      wa_style-fieldname = 'ZLSCH'.
*      wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*      insert  wa_style into table style2.
*      insert lines of style2 into table tg_contab-style2.
*      MOVE p_parid      TO tg_contab-hkont.
    ELSE.
      wa_style-fieldname = 'ZFBDT'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  wa_style INTO TABLE style2.

      wa_style-fieldname = 'ZLSCH'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  wa_style INTO TABLE style2.
*      MOVE: tl_0011-hkont   TO tg_contab-hkont.
    ENDIF.

    MOVE: tl_0011-bschl   TO tg_contab-bschl,
          tl_0011-hkont   TO tg_contab-hkont,
          tl_skat-txt50   TO tg_contab-txt50,
          tl_0011-taxtyp  TO tg_contab-taxtyp,
          tl_0011-estorno TO tg_contab-estorno,
          tl_0011-newbw   TO tg_contab-newbw,
          tl_0011-zfbdt   TO tg_contab-zfbdt,
          tl_0011-zlsch   TO tg_contab-zlsch,
          tl_0011-kostl   TO tg_contab-kostl,

*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
          tl_0011-hbkid   TO tg_contab-hbkid.

    IF tg_contab-zlsch EQ 'D'.
      IF tl_0011-taxa_juros IS NOT INITIAL.
        MOVE: tl_0011-taxa_juros   TO tg_contab-taxa_juros.
      ELSE.
        MOVE: wl_zfit0196-juros   TO tg_contab-taxa_juros.
      ENDIF.

      IF tl_0011-taxa_multa IS NOT INITIAL.
        MOVE: tl_0011-taxa_multa   TO tg_contab-taxa_multa.
      ELSE.
        MOVE: wl_zfit0196-multa   TO tg_contab-taxa_multa.
      ENDIF.
    ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610

    INSERT LINES OF style2 INTO TABLE tg_contab-style2.
    APPEND tg_contab.
    CLEAR: tg_contab.
    REFRESH: style2.
  ENDLOOP.

  tg_tbsl[] = tl_tbsl[].
  SORT: tl_makt BY matnr,
        tl_marc BY matnr werks.
*** Itens da Nota
  REFRESH: tg_itens.
  LOOP AT tl_0009.
    READ TABLE tl_makt
      WITH KEY matnr = tl_0009-matnr
               BINARY SEARCH.

    READ TABLE tl_marc
      WITH KEY matnr = tl_0009-matnr
               werks = tl_0009-bwkey
               BINARY SEARCH.

    MOVE: tl_0009-itmnum  TO tg_itens-itmnum,
          tl_0009-matnr   TO tg_itens-matnr,
          tl_makt-maktx   TO tg_itens-maktx,
          tl_0009-cfop    TO tg_itens-cfop,
          tl_0009-charg   TO tg_itens-charg,
          tl_0009-bwkey   TO tg_itens-werks,
          tl_0009-lgort   TO tg_itens-lgort,
          tl_0009-menge   TO tg_itens-menge,
          tl_0009-meins   TO tg_itens-meins,
          tl_0009-netpr   TO tg_itens-netpr,
          tl_0009-netwr   TO tg_itens-netwr,
          tl_0009-itmtyp  TO wg_fiscal-itmtyp,
          tl_0009-anln1   TO tg_itens-anln1,
          tl_0009-anln2   TO tg_itens-anln2,
          tl_0009-vbeln   TO tg_itens-vbeln,
          tl_0009-posnr   TO tg_itens-posnr,
          tl_marc-steuc   TO tg_itens-steuc,
*-CS2020001331 - 06.10.2021 - JT - inicio
          tl_0009-possui_icms_st TO tg_itens-possui_icms_st.
*-CS2020001331 - 06.10.2021 - JT - fim
    tg_itens-netdis = tl_0009-netdis.
    tg_itens-netfre = tl_0009-netfre.
    tg_itens-netins = tl_0009-netins.
    tg_itens-netoth = tl_0009-netoth.
    tg_itens-netwr = ( ( tg_itens-menge * tg_itens-netpr ) + tg_itens-netfre + tg_itens-netins + tg_itens-netoth ) - tg_itens-netdis.

    tg_itens-fase = icon_display_more.
    i_ebelp = tg_itens-itmnum.
    "
    SELECT SINGLE renas FROM zmmt0102 INTO tg_itens-renas
      WHERE ebeln = p_seq_lcto
      AND   ebelp = i_ebelp.
    "
    APPEND tg_itens.
    CLEAR: tg_itens.
  ENDLOOP.

***Movimentacao de estoque
  REFRESH: tg_movest.
  LOOP AT tl_0012.
    MOVE: tl_0012-bwart   TO tg_movest-bwart,
          tl_0012-tcode   TO tg_movest-tcode,
          tl_0012-mwskz1  TO tg_movest-mwskz1,
          tl_0012-estorno TO tg_movest-estorno.

    APPEND tg_movest.
    CLEAR: tg_movest.
  ENDLOOP.

  REFRESH: node_itab.
  IF tree IS NOT INITIAL.
*    IF v_automatico_memo = abap_false. "BUG SOLTO 134309 / AOENNING / DUMP
    CALL METHOD tree->delete_all_nodes.
*      PERFORM preenche_tree USING 'Root'
*                                  space
*                                  c_x
*                                  space
*                                  'Mensagens da Nota'
*                                  space.

    PERFORM preenche_tree USING c_root
                             space
                             c_x
                             space
                             'Mensagens da Nota'
                             space.
*    ENDIF.

    CLEAR: tl_0013.
*---> 06/07/2023 - Migração S4 - WS
    SORT tl_0013 BY seqnum.
*<--- 06/07/2023 - Migração S4 - WS
    LOOP AT tl_0013.
      ON CHANGE OF tl_0013-seqnum.
*       AT NEW SEQNUM.
        PERFORM preenche_tree USING tl_0013-seqnum
                                    c_root
                                    space
                                    cl_gui_simple_tree=>relat_last_child
                                    tl_0013-message
                                    handle_tree.
      ENDON.
*      ENDAT.
      tg_mensagems-seqnum  = tl_0013-seqnum.
      tg_mensagems-linnum  = tl_0013-linnum.
      tg_mensagems-message = tl_0013-message.

      READ TABLE tl_0005
        WITH KEY message = tl_0013-message.
      IF sy-subrc IS INITIAL.
        APPEND tg_mensagems.         " msgs q foram parametrizadas na operacao
      ENDIF.
      APPEND tg_mensagems TO tg_mensagems_aux.  " tabela q iram conter todas as msgs
      " inclusive as adcionadas na criacao da NF
    ENDLOOP.
  ENDIF.

  REFRESH: tg_aprov.
  LOOP AT tl_0007.
    READ TABLE tl_user
     WITH KEY bname = tl_0007-usnam.

    MOVE: tl_0007-nivel_aprov  TO tg_aprov-nivel_aprov,
          tl_0007-usnam        TO tg_aprov-usnam,
          tl_0007-departamento TO tg_aprov-departamento.

    CONCATENATE tl_user-name_first tl_user-name_last INTO tg_aprov-nome SEPARATED BY space.

    APPEND tg_aprov.
    CLEAR: tg_aprov.
  ENDLOOP.

** Parceiros da Nota fiscal
  REFRESH: tg_parc, style.

  CLEAR: tg_parc.

  tg_parc-parvw = p_parvw.
  tg_parc-parid = p_parid.

  IF p_parvw EQ c_ag.
    tg_parc-nome = wl_kna1-name1.

  ELSEIF p_parvw EQ c_br
      OR p_parvw EQ c_lf
      OR p_parvw EQ c_z1.
    tg_parc-nome  = wl_lfa1-name1.

  ENDIF.
  wa_style-fieldname = 'PARVW'.
  wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT  wa_style INTO TABLE style .
  wa_style-fieldname = 'PARID'.
  wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT  wa_style INTO TABLE style .
  INSERT LINES OF style INTO TABLE tg_parc-style.

  APPEND tg_parc.
  REFRESH: style.
  CLEAR: tg_parc.

  LOOP AT tl_0015.
    READ TABLE tg_parc TRANSPORTING NO FIELDS
      WITH KEY parvw = tl_0015-parvw
               parid = tl_0015-parid.

    IF sy-subrc IS NOT INITIAL.
      IF tl_0015-parvw EQ c_ag OR
         tl_0015-parvw EQ c_lr.

        READ TABLE tl_knpar
          WITH KEY kunnr = tl_0015-parid.

        tg_parc-nome  = tl_knpar-name1.

      ELSEIF tl_0015-parvw EQ c_br
        OR   tl_0015-parvw EQ c_lf
        OR   tl_0015-parvw EQ c_z1
        OR   tl_0015-parvw EQ c_pc
        OR   tl_0015-parvw EQ c_sp.
        READ TABLE tl_lfpar
          WITH KEY lifnr = tl_0015-parid.

        tg_parc-nome  = tl_lfpar-name1.

      ENDIF.
      tg_parc-parvw = tl_0015-parvw.
      tg_parc-parid = tl_0015-parid.

      APPEND tg_parc.
      CLEAR: tg_parc, tl_lfpar, tl_knpar.
    ENDIF.
  ENDLOOP.

  IF NOT ( wl_0019 IS INITIAL ).
    MOVE: wl_0019-lifnr  TO wg_transporte-lifnr,
          wl_0019-placa  TO wg_transporte-placa,
          wl_0019-anzpk  TO wg_transporte-anzpk,
          wl_0019-shpunt TO wg_transporte-shpunt,
          wl_0019-ntgew  TO wg_transporte-ntgew,
          wl_0019-brgew  TO wg_transporte-brgew,
          wl_0019-ufplaca TO wg_transporte-ufplaca,

          wl_0019-placa_car1 TO wg_transporte-placa_car1,
          wl_0019-placa_car2 TO wg_transporte-placa_car2,
          wl_0019-placa_car3 TO wg_transporte-placa_car3,
          wl_0019-motorista  TO wg_transporte-motorista.
  ENDIF.

  CLEAR: tg_docrefs[].

  IF tl_0020[] IS NOT INITIAL.
    SELECT * INTO TABLE it_j_1bnfdoc
      FROM j_1bnfdoc
       FOR ALL ENTRIES IN tl_0020
     WHERE docnum EQ tl_0020-docnum.

    SORT it_j_1bnfdoc BY docnum.
  ENDIF.

  LOOP AT tl_0020 .
    READ TABLE it_j_1bnfdoc WITH KEY docnum = tl_0020-docnum BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.
    tg_docrefs-docnum = it_j_1bnfdoc-docnum.
    tg_docrefs-docdat = it_j_1bnfdoc-docdat.
    tg_docrefs-model  = it_j_1bnfdoc-model .
    tg_docrefs-series = it_j_1bnfdoc-series.
    tg_docrefs-nfenum = it_j_1bnfdoc-nfenum.
    tg_docrefs-nftot  = it_j_1bnfdoc-nftot.
    APPEND tg_docrefs.
  ENDLOOP.

ENDFORM.                    " PREENCHE_CAMPOS_DOC
*&---------------------------------------------------------------------*
*&      Form  VALIDA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_dados .
  DATA: wl_0001  TYPE zfiwrt0001,
        wl_t001  TYPE t001,
*        wl_1bbranch TYPE j_1bbranch,
        wl_1bad  TYPE j_1bad,
        wl_1badt TYPE j_1badt,
        wl_kna1  TYPE kna1,
        wl_lfa1  TYPE lfa1,
        wl_0008  TYPE zfiwrt0008.


  CLEAR: wl_0001, wl_t001, wl_1bbranch, wl_1bad, wl_1badt, wl_kna1, wl_lfa1, wl_0008.


  IF p_operacao IS  NOT INITIAL
  AND p_bukrs IS    NOT INITIAL
   AND p_branch IS  NOT INITIAL
    AND p_parvw IS  NOT INITIAL
     AND p_parid IS NOT INITIAL.

    SELECT SINGLE *
      FROM zfiwrt0001
      INTO wl_0001
       WHERE operacao EQ p_operacao.

    IF  sy-subrc IS INITIAL.
*          MOVE: wl_0001-descricao TO wg_desc_operacao.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Cód. de Operação não existe!'.
      LEAVE TO SCREEN 100.
    ENDIF.

    SELECT SINGLE *
      FROM t001
      INTO wl_t001
       WHERE bukrs EQ p_bukrs.

    IF sy-subrc IS INITIAL.
*          MOVE: wl_t001-butxt TO wg_desc_bukrs.

      SELECT SINGLE *
        FROM j_1bbranch
        INTO wl_1bbranch
         WHERE bukrs  EQ p_bukrs
           AND branch EQ p_branch.

      IF sy-subrc IS INITIAL.
*            MOVE: wl_1bbranch-name TO wg_desc_branch.

      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'O Local de negocio nao foi encontrado para'
                              'essa empresa!'.
        LEAVE TO SCREEN 100.
      ENDIF.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'A empresa não existe!'.
      LEAVE TO SCREEN 100.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bad
      INTO wl_1bad
       WHERE parvw EQ p_parvw.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
        FROM j_1badt
        INTO wl_1badt
         WHERE spras EQ sy-langu
           AND parvw EQ p_parvw.

*          MOVE: wl_1badt-partxt TO wg_desc_parvw.

      IF p_parvw EQ c_ag.
        SELECT SINGLE *
          FROM kna1
          INTO wl_kna1
           WHERE kunnr EQ p_parid.

        IF sy-subrc IS INITIAL.
*              MOVE: wl_kna1-name1 TO wg_desc_parid.

        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Cliente selecionado, não foi encontrado!'.
          LEAVE TO SCREEN 100.
        ENDIF.

      ELSEIF p_parvw EQ c_lf
        OR   p_parvw EQ c_br.
        SELECT SINGLE *
          FROM lfa1
          INTO wl_lfa1
           WHERE lifnr EQ p_parid.

        IF sy-subrc IS INITIAL.
*              MOVE: wl_lfa1-name1 TO wg_desc_parid.

        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Fornecedor/Filial selecionado, não foi encontrado!'.
          LEAVE TO SCREEN 100.
        ENDIF.

      ENDIF.

    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'A função de Parceiro selecionada, não foi'
                            'encontrada!'.
      LEAVE TO SCREEN 100.
    ENDIF.
*        IF tg_itens[] IS INITIAL.
*          tg_itens-itmnum = 10.
*          APPEND tg_itens.
*        ENDIF.
*   ok-code = c_atuali.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Preencher os campos obrigatorios!'.
    LEAVE TO SCREEN 100.
  ENDIF.
*ENDIF.

  IF wg_fiscal-retorno EQ c_s
  AND wg_fiscal-docref IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do '
                                           'campo "Doc. Original".'.
    g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab1.
    x_field = 'WG_FISCAL-DOCREF'.
    LEAVE TO SCREEN 100.
  ENDIF.

  IF wg_fiscal-zpesagem EQ c_01
  AND wg_fiscal-nr_romaneio IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do '
                                           'campo "Romaneio".'.
    x_field = 'WG_FISCAL-NR_ROMANEIO'.
    g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab1.
    LEAVE TO SCREEN 100.
  ENDIF.

  LOOP AT tg_itens.
    IF tg_itens-matnr IS INITIAL
    OR tg_itens-werks IS INITIAL
    OR tg_itens-netwr IS INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Preencher informações obrigatorias,'
                                             'nos itens da nota.'.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab6.
      LEAVE TO SCREEN 100.
    ENDIF.
    IF wg_fiscal-imobilizado EQ c_s
    AND tg_itens-anln1 IS INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Preencher informações obrigatorias,'
                                              'nos itens da nota.'.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab6.
      LEAVE TO SCREEN 100.
    ENDIF.
  ENDLOOP.

  IF zfiwrt0008-parvw = 'AG'.

  ENDIF.

**********************************************************************
* 102147 CS2023000072 Validação centro de custo transferências IMOBILIZADO - PSA
**********************************************************************

*  IF tg_parc-parvw = 'BR' AND wg_fiscal-imobilizado = 'S' AND wg_fiscal-tp_mv_imob = 'T' AND tg_parc-parid <> p_parid.
*
*    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Escrever aqui menssagem de erro!,'
*                                                  'Continua!'.
*    g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab6.
*    LEAVE TO SCREEN 100.
*
*  ENDIF.


ENDFORM.                    " VALIDA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ELEMINA_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM elemina_doc .
  DATA: wl_0008 TYPE zfiwrt0008.

  SELECT  SINGLE *
    FROM zfiwrt0008
    INTO wl_0008
     WHERE seq_lcto EQ p_seq_lcto.

  IF sy-subrc IS INITIAL.
    IF wl_0008-status IS INITIAL OR wl_0008-docnum IS INITIAL.
      MOVE: c_x TO wl_0008-loekz.
      MODIFY zfiwrt0008 FROM wl_0008.
      MESSAGE s836(sd) WITH 'O documento foi eliminado!'.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o documento'
                            'foi marcado para processamento!'.
    ENDIF.
  ENDIF.
ENDFORM.                    " ELEMINA_DOC
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .

  DATA: wl_0001          TYPE zfiwrt0001,
        wl_0011          TYPE zfiwrt0011,
        wl_t001          TYPE t001,
        wl_t001w         TYPE t001w,
        wa_zmmt0102      TYPE zmmt0102,
        wl_1bbranch      TYPE j_1bbranch,
        wl_1bad          TYPE j_1bad,
        wl_kna1          TYPE kna1,
        wl_knb1          TYPE knb1,
        wl_anla          TYPE anla,
        wl_lfa1          TYPE lfa1,
        wl_1badt         TYPE j_1badt,
        wl_zlest0002     TYPE zlest0002,
        tl_anlc          TYPE TABLE OF anlc,
        tl_2000          TYPE TABLE OF zfiwrt2000 WITH HEADER LINE,
        tl_2001          TYPE TABLE OF zfiwrt2001 WITH HEADER LINE,
        tl_2002          TYPE TABLE OF zfiwrt2002 WITH HEADER LINE,
        tl_t042z         TYPE TABLE OF t042z WITH HEADER LINE,
        tl_tbsl          TYPE TABLE OF tbsl       WITH HEADER LINE,
        tl_cskb          TYPE TABLE OF cskb       WITH HEADER LINE,
        tl_csks          TYPE TABLE OF csks       WITH HEADER LINE,
        wl_csks          TYPE csks,
        wl_linha(6),
        v_data_nota(10),
        lc_qtd_linhas    TYPE i,
        tl_mcha          TYPE TABLE OF mcha WITH HEADER LINE,
        tl_t001l         TYPE TABLE OF t001l WITH HEADER LINE,
        tl_knb1          TYPE TABLE OF knb1 WITH HEADER LINE,
        tl_lfb1          TYPE TABLE OF lfb1 WITH HEADER LINE,
        tl_mara          TYPE TABLE OF mara WITH HEADER LINE,
        wl_1baa          TYPE j_1baa,
        wl_marc          TYPE marc,
        wl_mbew          TYPE mbew,
        tl_0009          TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
        tl_t001k         TYPE TABLE OF t001k WITH HEADER LINE,
        wl_seq_lcto      TYPE zfiwrt0008-seq_lcto,
        wl_zib_nfe_forn  TYPE zib_nfe_forn,
        wl_cnpj          TYPE lfa1-stcd1,
        wl_parid         TYPE zfiwrt0008-parid,
        wl_parid2        TYPE zfiwrt0008-parid,
        wl_tot_contab    TYPE dmbtr,
        wl_vbap          TYPE vbap,
        p_xblnr          TYPE  xblnr1,
        wa_setleaf       TYPE setleaf,
        wl_tka02         TYPE tka02,
        vkokrs           TYPE tka02-kokrs,
        wa_zsdt0075      TYPE zsdt0075,
        ls_acckey_str    TYPE j_1b_nfe_access_key,
        lv_stcd1_ck2     TYPE j_1b_nfe_access_key-stcd1,
        lv_series        TYPE j_1bseries,
        lv_nfnum9        TYPE j_1bnfnum9,
        lv_parid_key     TYPE lfa1-lifnr,

        lv_totdiff       TYPE zfiwrt0009-netwr,
        lv_totref        TYPE zfiwrt0009-netwr,
        lv_totitens      TYPE zfiwrt0009-netwr,
        lv_vlr_itens     TYPE wrbtr,
        lv_stcd1_ck      TYPE lfa1-stcd1,
        v_ebelp          TYPE ekpo-ebelp,
        lva_anln1        TYPE anla-anln1,
        wl_doc           TYPE j_1bnfdoc,
        wa_j_1bnfdoc     TYPE j_1bnfdoc,
        wa_zfiwrt0020    TYPE zfiwrt0020,
        wa_zfiwrt0008    TYPE zfiwrt0008,
        vg_candat        TYPE j_1bnfdoc-candat,
        lva_msg_tmp      TYPE string,
        lv_available     TYPE c,
        lv_iv_icms       TYPE c,   "*-CS2023000043-14.02.2023-#102019-JT-COMENTADO
        tl_zfiwrt0009    TYPE TABLE OF zfiwrt0009,
        wl_zfiwrt0009    TYPE zfiwrt0009,
        wl_zfiwrt0008    TYPE zfiwrt0008,
        wl_j_1bnfdoc     TYPE j_1bnfdoc,
        wl_j_1bnfdoc_aux TYPE j_1bnfdoc.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: lt_returns         TYPE TABLE OF bapiret2,
        ls_coeldes         TYPE bapi1030_ceoutputlist,
        ls_cskb            LIKE LINE OF tl_cskb,
        lv_controllingarea TYPE  bapi1030_gen-co_area,
        lv_costelement     TYPE  bapi1030_gen-cost_elem,
        lv_keydate         TYPE  bapi1030_gen-some_date.
* <--- S4 Migration - 18/07/2023 - CA

  REFRESH: tg_msg_ret, tl_mcha, tl_t001l, tl_0009, tl_t001k, tl_t042z, tl_tbsl,
           tl_knb1, tl_lfb1, tl_cskb, tl_csks.
  CLEAR: tg_msg_ret, wl_1baa, wl_seq_lcto, wl_parid, tl_t001k,
         wl_knb1. ", lv_iv_icms. *-CS2023000043-14.02.2023-#102019-JT-COMENTADO
  DATA: object  TYPE sibflporb,
        lt_stat TYPE sgs_t_acnt.

  CONDENSE wg_fiscal-access_key NO-GAPS.
  IF p_seq_lcto GT 0.
    CASE p_operacao .
      WHEN '131' OR '186' OR '241' OR '216' OR '221' OR '621' OR '176' OR '20'.
        MOVE 'BO' TO object-catid.
        object-typeid = 'ZWRR0002'.
        CONCATENATE sy-mandt p_seq_lcto INTO object-instid.
        REFRESH  lt_stat.
        CALL METHOD cl_gos_attachment_query=>count_for_object
          EXPORTING
            is_object = object
            ip_arl    = space
          RECEIVING
            rt_stat   = lt_stat.
        IF lt_stat[] IS INITIAL.
          MOVE: TEXT-e57                   TO tg_msg_ret-msg,
            c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
            'WG_FISCAL-INCO1'          TO tg_msg_ret-field.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
    ENDCASE.
  ENDIF.

*Inicio Alteração - Leandro Valentim Ferreira - 14.06.23 - #108893
  IF w_zfiwrt0001-valida_cfop EQ 'S'.
    IF wg_docs-nfenum IS NOT INITIAL AND
       wg_docs-bldat IS NOT INITIAL AND
       p_parid IS NOT INITIAL.

      SELECT SINGLE *
             INTO @DATA(wl_zib_nfe_dist_ter)
             FROM zib_nfe_dist_ter
             WHERE numero     EQ @wg_docs-nfenum
               AND dt_emissao EQ @wg_docs-bldat.

      IF wl_zib_nfe_dist_ter-chave_nfe IS NOT INITIAL.

        SELECT SINGLE *
               INTO @DATA(wl_zib_nfe_dist_itm)
               FROM zib_nfe_dist_itm
               WHERE chave_nfe EQ @wl_zib_nfe_dist_ter-chave_nfe.
      ENDIF.

      SELECT SINGLE *
             INTO @DATA(wl_lfa1_new)
             FROM lfa1
             WHERE lifnr EQ @p_parid.

      IF wl_zib_nfe_dist_itm-prod_cfop IS NOT INITIAL AND wl_lfa1_new-regio IS NOT INITIAL.
        SELECT *
               INTO TABLE @DATA(tl_zfiwrt0028)
               FROM zfiwrt0028
               WHERE operacao EQ @p_operacao
                 AND uf       EQ @wl_lfa1_new-regio
                 AND cfop     EQ @wl_zib_nfe_dist_itm-prod_cfop.

        IF sy-subrc NE 0.
          MOVE: TEXT-e95                   TO tg_msg_ret-msg, "CFOP não permitido nesta operação!
                c_tab_strip_nf-tab1        TO tg_msg_ret-aba.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 14.06.23 - #108893

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF w_zfiwrt0001-complement_icms = 'S'.
    SELECT SINGLE *
      INTO @DATA(w_batl1)
      FROM j_1batl1
     WHERE taxlaw = @wg_direitos-taxlw1.

    IF sy-subrc <> 0.
      MOVE: TEXT-e91                   TO tg_msg_ret-msg,
            c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_DIREITOS-TAXLW1'        TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(w_batl2)
      FROM j_1batl2
     WHERE taxlaw = @wg_direitos-taxlw2.

    IF sy-subrc <> 0.
      MOVE: TEXT-e92                   TO tg_msg_ret-msg,
            c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_DIREITOS-TAXLW2'        TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(w_batl4a)
      FROM j_1batl4a
     WHERE taxlaw = @wg_direitos-taxlw4.

    IF sy-subrc <> 0.
      MOVE: TEXT-e93                   TO tg_msg_ret-msg,
            c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_DIREITOS-TAXLW4'        TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(w_batl5)
      FROM j_1batl5
     WHERE taxlaw = @wg_direitos-taxlw5.

    IF sy-subrc <> 0.
      MOVE: TEXT-e94                   TO tg_msg_ret-msg,
            c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_DIREITOS-TAXLW5'        TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
*-CS2023000043-09.02.2023-#102019-JT-fim

*** PBI - 73759 - Inicio - CBRAND
  IF tg_itens[] IS NOT INITIAL.

    LOOP AT tg_itens.
**      Ajuste chamado - #103385 - RJF
      SELECT SINGLE *
       FROM anla
       INTO wl_anla
       WHERE  bukrs = p_bukrs "RJF S4Hana
          AND anln1 = tg_itens-anln1
          AND anln2 = tg_itens-anln2.

      IF sy-subrc = 0.

*** BUG - 79101 - Inicio - CBRAND
        CLEAR: tl_zfiwrt0009,
               wl_zfiwrt0009,
               wl_zfiwrt0008,
               lva_anln1,
               wl_j_1bnfdoc,
               wl_j_1bnfdoc_aux.
        SELECT SINGLE * FROM j_1baa INTO wl_1baa WHERE nftype EQ wg_fiscal-nftype.
        IF wg_fiscal-imobilizado = 'S' AND wg_fiscal-tp_mv_imob <> 'V'
           AND wl_1baa-direct = '2'.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = tg_itens-anln1
            IMPORTING
              output = lva_anln1.

          SELECT *
            FROM zfiwrt0009
          INTO TABLE tl_zfiwrt0009
            WHERE anln1  = lva_anln1
               AND anln2 = tg_itens-anln2.

          IF tl_zfiwrt0009 IS NOT INITIAL.

            LOOP AT tl_zfiwrt0009 INTO wl_zfiwrt0009.

              SELECT SINGLE *
                FROM zfiwrt0008
                INTO wl_zfiwrt0008
                  WHERE seq_lcto = wl_zfiwrt0009-seq_lcto.

              SELECT SINGLE *
                FROM j_1bnfdoc
                INTO wl_j_1bnfdoc
                  WHERE docnum = wl_zfiwrt0008-docnum.

              IF wl_j_1bnfdoc-direct = 2 AND  wl_j_1bnfdoc-candat IS INITIAL.
                SELECT SINGLE *
                  FROM j_1bnfdoc
                  INTO wl_j_1bnfdoc_aux
                  WHERE  direct  = '1'
                    AND  partyp  = 'B'
                    AND  bukrs   = wl_j_1bnfdoc-bukrs
                    AND  branch  = wl_j_1bnfdoc-parid+4(4)   "Empresa+Centro
                    AND  docdat  = wl_j_1bnfdoc-docdat
                    AND  nfenum  = wl_j_1bnfdoc-nfenum.

*                IF sy-subrc NE 0.
*                  CONCATENATE wl_j_1bnfdoc-docdat+6(2) '.' wl_j_1bnfdoc-docdat+4(2) '.' wl_j_1bnfdoc-docdat+0(4) INTO v_data_nota.
*                  CONCATENATE 'Existe saida para o Imobilizado sem Entrada na Filial Destino, SeqLct:' wl_zfiwrt0009-seq_lcto  'NOTA: '  wl_j_1bnfdoc-nfenum 'Item' tg_itens-itmnum '-' v_data_nota INTO tg_msg_ret-msg SEPARATED BY space.
*                  MOVE:   c_tab_strip_nf-tab3  TO tg_msg_ret-aba,
*                     'TG_ITENS-ANLN1'      TO tg_msg_ret-field.
*                  APPEND tg_msg_ret.
*                  CLEAR: tg_msg_ret.
*                ENDIF.
              ENDIF.
              CLEAR: wl_zfiwrt0009, wl_zfiwrt0008.
            ENDLOOP.
          ENDIF.
        ENDIF.
*** BUG - 79101 - Fim - CBRAND


        IF wg_fiscal-imobilizado = 'S' AND wg_fiscal-tp_mv_imob <> 'C'.
          IF wl_anla-bukrs <> p_bukrs.
            CONCATENATE 'Este Imobilizado pertence a empresa!' wl_anla-bukrs 'Item' tg_itens-itmnum  INTO tg_msg_ret-msg SEPARATED BY space.

            MOVE: c_tab_strip_nf-tab3  TO tg_msg_ret-aba,
                 'P_BUKRS'  TO tg_msg_ret-field.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.

        IF wl_anla-deakt IS NOT INITIAL.

          CONCATENATE 'Este Imobilizado esta Desativado!!' 'Item' tg_itens-itmnum  INTO tg_msg_ret-msg SEPARATED BY space.
          MOVE:   c_tab_strip_nf-tab3  TO tg_msg_ret-aba,
             'TG_ITENS-ANLN1'      TO tg_msg_ret-field.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

        IF wl_anla-leart = '03' OR  wl_anla-leart = '04'.
          CONCATENATE 'Ativo imobilizado com restrições de movimentação. Por favor entrar em contato com imobilizado@amaggi.com.br;'
           'Item' tg_itens-itmnum  INTO tg_msg_ret-msg SEPARATED BY space.
          MOVE:  c_tab_strip_nf-tab3  TO tg_msg_ret-aba,
                  'TG_ITENS-ANLN1'    TO tg_msg_ret-field.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ELSE.
        IF  wg_fiscal-imobilizado EQ 'S' AND wg_fiscal-tp_mv_imob <> 'C'.
          CONCATENATE 'Este Imobilizado não existe na tabela ANLA / ou não foi informado!!' 'Item' tg_itens-itmnum  INTO tg_msg_ret-msg SEPARATED BY space.
          MOVE:   c_tab_strip_nf-tab3  TO tg_msg_ret-aba,
                  'TG_ITENS-ANLN1'     TO tg_msg_ret-field.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
**      Ajuste chamado - #103385 - RJF
    ENDLOOP.
  ENDIF.

*** PBI - 73759 - Fim - CBRAND
  SELECT SINGLE *
     FROM tka02
     INTO wl_tka02
     WHERE bukrs  = p_bukrs.

  MOVE wl_tka02-kokrs TO vkokrs.

  IF wg_fiscal-complemento EQ 'N'
  OR wg_fiscal-complemento EQ 'S'
  OR wg_fiscal-complemento IS INITIAL.
    IF tg_itens[] IS NOT INITIAL.
      SELECT *
        FROM mcha
        INTO TABLE tl_mcha
         FOR ALL ENTRIES IN tg_itens
         WHERE matnr EQ tg_itens-matnr
           AND werks EQ tg_itens-werks
           AND charg EQ tg_itens-charg.

      SELECT *
        FROM t001l
        INTO TABLE tl_t001l
         FOR ALL ENTRIES IN tg_itens
         WHERE werks EQ tg_itens-werks
           AND lgort EQ tg_itens-lgort.


      SELECT *
        FROM mara
        INTO TABLE tl_mara
         FOR ALL ENTRIES IN tg_itens
         WHERE matnr EQ tg_itens-matnr.

      SELECT *
        FROM t001k
        INTO TABLE tl_t001k
         FOR ALL ENTRIES IN tg_itens
         WHERE bwkey EQ tg_itens-werks.


    ENDIF.
    IF tg_movest[] IS NOT INITIAL.
      SELECT  *
        FROM zfiwrt2000
        INTO TABLE tl_2000
         FOR ALL ENTRIES IN tg_movest
         WHERE bwart EQ tg_movest-bwart.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM zfiwrt2001
          INTO TABLE tl_2001
          FOR ALL ENTRIES IN tl_2000
           WHERE bwart       EQ tl_2000-bwart
             AND function_name EQ tl_2000-function_name.

        IF sy-subrc IS INITIAL.
          SELECT *
            FROM zfiwrt2002
            INTO TABLE tl_2002
            FOR ALL ENTRIES IN tl_2001
             WHERE bwart EQ tl_2001-bwart
               AND function_name EQ tl_2001-function_name.

        ENDIF.

      ENDIF.

    ELSEIF wg_fiscal-lm_estoque EQ 'S'.
      MOVE: 'Não existe parametro para movimentação do estoque!'                  TO tg_msg_ret-msg,
         c_tab_strip_nf-tab5        TO tg_msg_ret-aba,
         'P_PARID'                  TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    SELECT SINGLE *
      FROM zfiwrt0001
      INTO @DATA(wl_0001_1)
       WHERE operacao EQ @p_operacao.

    IF wg_fiscal-lm_estoque EQ 'S' AND wl_0001_1-lm_indea NE 'S'.
      IF tg_aprov[] IS INITIAL.
        MOVE: 'Não existe aprovadores para mov. estoque!'                  TO tg_msg_ret-msg,
         c_tab_strip_nf-tab5        TO tg_msg_ret-aba,
         'P_PARID'                  TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.


    IF tg_contab[] IS NOT INITIAL.
      SELECT *
        FROM t042z
        INTO TABLE tl_t042z
         FOR ALL ENTRIES IN tg_contab
         WHERE zlsch EQ tg_contab-zlsch
           AND land1 EQ 'BR'.

      SELECT *
        FROM tbsl
        INTO TABLE tl_tbsl
        FOR ALL ENTRIES IN tg_contab
         WHERE bschl EQ tg_contab-bschl.

* ---> S4 Migration - 18/07/2023 - CA
*      SELECT *
*        FROM cskb
*        INTO TABLE tl_cskb
*        FOR ALL ENTRIES IN tg_contab
*         WHERE kstar EQ tg_contab-hkont
*           AND  ( datbi GE sy-datum
*              AND datab LE sy-datum )
*           AND katyp EQ '01'.

      LOOP AT tg_contab INTO DATA(ls_contab).

        lv_controllingarea  = vkokrs.
        lv_costelement      = ls_contab-hkont.
        lv_keydate          = sy-datum.

        CLEAR: lt_returns[], ls_coeldes.

        CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
          EXPORTING
            controllingarea   = lv_controllingarea
            costelement       = lv_costelement
            keydate           = lv_keydate
          IMPORTING
            costelementdetail = ls_coeldes
          TABLES
            return            = lt_returns.

        READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
        IF sy-subrc <> 0 AND
           ls_coeldes-celem_category = '01'.

          ls_cskb-kokrs = vkokrs.
          ls_cskb-kstar = ls_contab-hkont.
          ls_cskb-katyp = ls_coeldes-celem_category.

          APPEND ls_cskb TO tl_cskb.
          CLEAR ls_cskb.
        ENDIF.

        CLEAR: ls_contab.
      ENDLOOP.
* <--- S4 Migration - 18/07/2023 - CA

      SELECT *
        FROM csks
        INTO TABLE tl_csks
         FOR ALL ENTRIES IN tg_contab
         WHERE  kokrs = vkokrs
           AND  kostl EQ tg_contab-kostl
           AND  ( datbi GE sy-datum
              AND datab LE sy-datum ).
    ENDIF.
    IF tg_parc[] IS NOT INITIAL.
      SELECT *
        FROM knb1
        INTO TABLE tl_knb1
         FOR ALL ENTRIES IN tg_parc
         WHERE kunnr EQ tg_parc-parid
           AND bukrs EQ p_bukrs.

      SELECT *
        FROM lfb1
        INTO TABLE tl_lfb1
         FOR ALL ENTRIES IN tg_parc
         WHERE lifnr EQ tg_parc-parid
           AND bukrs EQ p_bukrs.
    ENDIF.

    IF wl_0001_1-aviso_rec EQ abap_true.

      "Verificar Parceiro SP
      READ TABLE tg_parc WITH KEY parvw = 'SP' TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        MOVE: c_tab_strip_nf-tab9     TO tg_msg_ret-aba.
        tg_msg_ret-msg = TEXT-e82.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      "Verificar Parceiro LC
      READ TABLE tg_parc WITH KEY parvw = 'LC' TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        MOVE: c_tab_strip_nf-tab9     TO tg_msg_ret-aba.
        tg_msg_ret-msg = TEXT-e83.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      "Verificar Parceiro LR
      READ TABLE tg_parc WITH KEY parvw = 'LR' TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        MOVE: c_tab_strip_nf-tab9     TO tg_msg_ret-aba.
        tg_msg_ret-msg = TEXT-e84.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.

    READ TABLE tl_2002
      WITH KEY par_value2 = 'PARID'.
    IF sy-subrc IS INITIAL.
      IF p_parid IS INITIAL.
        MOVE: TEXT-e79                TO tg_msg_ret-msg,
           c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'P_PARID'     TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
    ENDIF.

    READ TABLE tl_2002
      WITH KEY par_value2 = 'MOVE_STLOC'.
    IF sy-subrc IS INITIAL.
      IF wg_fiscal-move_stloc IS INITIAL.
        MOVE: TEXT-e25                TO tg_msg_ret-msg,
           c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_FISCAL-MOVE_STLOC'     TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
    ENDIF.

    IF p_parvw = c_br.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = p_parid
        IMPORTING
          output = wl_parid2.
      CONDENSE wl_parid2 NO-GAPS.
      IF strlen( wl_parid2 ) GT 4.
        MOVE: 'Código de parceiro Filial Inválido!'                  TO tg_msg_ret-msg,
        c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
        'P_PARID'                  TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
    ENDIF.

    IF wg_fiscal-lm_estoque NE 'S'.
      IF wg_fiscal-inco1 IS INITIAL.
        MOVE: TEXT-e28                   TO tg_msg_ret-msg,
              c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
              'WG_FISCAL-INCO1'          TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

      IF wg_fiscal-inco2 IS INITIAL.
        MOVE: TEXT-e28                   TO tg_msg_ret-msg,
              c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
              'WG_FISCAL-INCO2'          TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
    ENDIF.

    READ TABLE tl_2002
      WITH KEY par_value2 = 'MOVE_PLANT'.
    IF sy-subrc IS INITIAL.
      IF wg_fiscal-move_plant IS INITIAL.
        MOVE: TEXT-e27                TO tg_msg_ret-msg,
           c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_FISCAL-MOVE_PLANT'     TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    READ TABLE tl_2002
      WITH KEY par_value2 = 'KONTO'.
    IF sy-subrc IS INITIAL.
      IF wg_fiscal-konto IS INITIAL.
        MOVE: TEXT-e65                TO tg_msg_ret-msg,
           c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_FISCAL-KONTO'     TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wg_fiscal-konto
          IMPORTING
            output = wg_fiscal-konto.
      ENDIF.

    ENDIF.


    READ TABLE tl_2002
      WITH KEY par_value2 = 'MOVE_MAT'.
    IF sy-subrc IS INITIAL.
      IF wg_fiscal-move_mat IS INITIAL.
        MOVE: TEXT-e66                TO tg_msg_ret-msg,
           c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_FISCAL-MOVE_MAT'     TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wg_fiscal-move_mat
          IMPORTING
            output = wg_fiscal-move_mat.
      ENDIF.
    ENDIF.

    READ TABLE tl_2002
      WITH KEY par_value2 = 'MOVE_BATCH'.
    IF sy-subrc IS INITIAL.
      IF wg_fiscal-move_batch IS INITIAL.
        MOVE: TEXT-e67                TO tg_msg_ret-msg,
           c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_FISCAL-MOVE_BATCH'     TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wg_fiscal-tp_mv_imob = 'T'. " transferência
      IF wg_fiscal-move_plant IS INITIAL.
        MOVE: TEXT-e27                TO tg_msg_ret-msg,
           c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
           'WG_FISCAL-MOVE_PLANT'     TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        SELECT SINGLE *
          FROM t001w
          INTO wl_t001w
          WHERE werks = wg_fiscal-move_plant.
        IF sy-subrc NE 0.
          MOVE: TEXT-e48             TO tg_msg_ret-msg,
          c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
          'WG_FISCAL-MOVE_PLANT'     TO tg_msg_ret-field.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      IF wg_fiscal-kostl IS INITIAL.
        MOVE: TEXT-e44                TO tg_msg_ret-msg,
         c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
         'WG_FISCAL-KOSTL'     TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wg_fiscal-kostl
          IMPORTING
            output = wg_fiscal-kostl.
        SELECT SINGLE *
             FROM csks
             INTO wl_csks
             WHERE  kokrs = vkokrs
                AND   kostl EQ wg_fiscal-kostl
                AND  ( datbi GE sy-datum
                   AND datab LE sy-datum ).
        IF sy-subrc NE 0.
          MOVE: TEXT-e45                TO tg_msg_ret-msg,
          c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
          'WG_FISCAL-KOSTL'     TO tg_msg_ret-field.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
    ENDIF.

    "Valida Request
    IF xbol = 'N'.
      LOOP AT tg_contab.
        IF tg_contab-bschl EQ '01'
          AND tg_contab-zlsch NE 'D'.
          MOVE: TEXT-e43       TO tg_msg_ret-msg,
                'ZLSCH'        TO tg_msg_ret-field.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret, wg_desc_operacao.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT SINGLE * FROM j_1baa INTO wl_1baa WHERE nftype EQ wg_fiscal-nftype.

    SELECT SINGLE * FROM zfiwrt0001 INTO wl_0001 WHERE operacao EQ p_operacao.

    IF wl_0001 IS INITIAL.
      MOVE: TEXT-e02            TO tg_msg_ret-msg,
*            C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
            'P_OPERACAO'        TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret, wg_desc_operacao.
    ELSE.
      IF ( wl_0001-referencia EQ 'S' ) AND ( wg_fiscal-referencia IS INITIAL ).
        MOVE: TEXT-e46               TO tg_msg_ret-msg,
              'WG_FISCAL-REFERENCIA' TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret, wg_desc_operacao.
      ELSE.
        MOVE: wl_0001-descricao TO wg_desc_operacao.
      ENDIF.
    ENDIF.

    IF NOT ( wg_fiscal-referencia IS INITIAL ).

      SELECT SINGLE * FROM j_1bnfdoc
        INTO wl_doc
      WHERE docnum EQ wg_fiscal-referencia.

      IF sy-subrc NE 0.
        MOVE: TEXT-e47                 TO tg_msg_ret-msg,
              'WG_FISCAL-REFERENCIA'   TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret, wg_desc_operacao.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM t001 INTO wl_t001 WHERE bukrs EQ p_bukrs.

    IF sy-subrc IS INITIAL.
      MOVE: wl_t001-butxt TO wg_desc_bukrs.

      SELECT SINGLE *
        FROM j_1bbranch
        INTO wl_1bbranch
         WHERE bukrs  EQ p_bukrs
           AND branch EQ p_branch.

      IF sy-subrc IS NOT  INITIAL.
        MOVE: TEXT-e05            TO tg_msg_ret-msg,
             'P_BRANCH'           TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret, wg_desc_branch.
      ELSE.
        MOVE: wl_1bbranch-name TO wg_desc_branch.
      ENDIF.
    ELSE.
      MOVE: TEXT-e11            TO tg_msg_ret-msg,
            'P_BUKRS'           TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret, wg_desc_bukrs.
    ENDIF.

    IF wg_fiscal-lm_estoque NE 'S'.
      SELECT SINGLE *
        FROM j_1bad
        INTO wl_1bad
         WHERE parvw EQ p_parvw.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE *
          FROM j_1badt
          INTO wl_1badt
           WHERE spras EQ sy-langu
             AND parvw EQ p_parvw.

        MOVE: wl_1badt-partxt TO wg_desc_parvw.

        IF p_parvw EQ c_ag.
          SELECT SINGLE *
            FROM kna1
            INTO wl_kna1
             WHERE kunnr EQ p_parid.

          IF sy-subrc IS NOT INITIAL.
            MOVE: TEXT-e06            TO tg_msg_ret-msg,
                 'P_PARID'            TO tg_msg_ret-field.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret, wg_desc_parid.
          ELSE.
            IF wl_kna1-sperr = 'X'.
              CONCATENATE TEXT-e56 ''  wl_parid TEXT-e55 '' wl_linha INTO  tg_msg_ret-msg.
              MOVE: 'P_PARID'            TO tg_msg_ret-field.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret, wg_desc_parid.
            ELSE.
              SELECT SINGLE *
                FROM knb1
                INTO wl_knb1
                 WHERE kunnr EQ p_parid
                   AND bukrs EQ p_bukrs.
              IF sy-subrc IS NOT INITIAL.
                MOVE: TEXT-e31             TO tg_msg_ret-msg,
                      'P_PARID'            TO tg_msg_ret-field.
                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret, wg_desc_parid.
              ELSE.
                MOVE: wl_kna1-name1 TO wg_desc_parid,
                      wl_kna1-stcd1 TO wl_cnpj.
              ENDIF.
            ENDIF.
          ENDIF.

        ELSEIF p_parvw EQ c_lf
          OR   p_parvw EQ c_br.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = p_parid
            IMPORTING
              output = wl_parid.

          SELECT SINGLE *
            FROM lfa1
            INTO wl_lfa1
             WHERE lifnr EQ wl_parid.

          IF sy-subrc IS NOT  INITIAL.
            MOVE: TEXT-e07            TO tg_msg_ret-msg,
                 'P_PARID'            TO tg_msg_ret-field.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret, wg_desc_parid.
          ELSE.
            IF wl_lfa1-sperr = 'X'.
              CONCATENATE TEXT-e54 ''  wl_parid TEXT-e55 '' wl_linha INTO tg_msg_ret-msg.
              MOVE: 'P_PARID'            TO tg_msg_ret-field.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret, wg_desc_parid.
            ELSE.
              MOVE: wl_lfa1-name1 TO wg_desc_parid,
                    wl_lfa1-stcd1 TO wl_cnpj.
            ENDIF.
          ENDIF.

        ENDIF.

      ELSE.
        MOVE: TEXT-e09            TO tg_msg_ret-msg,
              'P_PARVW'           TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret, wg_desc_parvw.
      ENDIF.
    ENDIF.
    SORT tl_lfb1 BY lifnr.
    SORT tl_knb1 BY kunnr.

    IF wg_fiscal-lm_estoque NE 'S'.
      LOOP AT tg_parc.
        wl_linha = sy-tabix.
        IF tg_parc-parid NE p_parid.
          IF tg_parc-parvw EQ 'LF'
          OR tg_parc-parvw EQ 'BR'
          OR tg_parc-parvw EQ 'Z1'
          OR tg_parc-parvw EQ 'SP'
          OR tg_parc-parvw EQ 'PC'.
            READ TABLE tl_lfb1 WITH KEY lifnr = tg_parc-parid
                                        BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.
              MOVE: c_tab_strip_nf-tab9     TO tg_msg_ret-aba.

              CONCATENATE TEXT-e37 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ELSEIF tl_lfb1-sperr = 'X'.
              CONCATENATE TEXT-e54 ''  wl_parid TEXT-e55 '' wl_linha INTO  tg_msg_ret-msg.
              MOVE: 'P_PARID'            TO tg_msg_ret-field.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret, wg_desc_parid.
            ENDIF.
          ELSEIF tg_parc-parvw IS INITIAL.
            MOVE: c_tab_strip_nf-tab1     TO tg_msg_ret-aba.

            CONCATENATE TEXT-e35 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ELSE.
            READ TABLE tl_knb1 WITH KEY kunnr = tg_parc-parid
                                         BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.
              MOVE: c_tab_strip_nf-tab9     TO tg_msg_ret-aba.

              CONCATENATE TEXT-e36 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ELSEIF tl_kna1-sperr = 'X'.
              CONCATENATE TEXT-e56 ''  wl_parid TEXT-e55 '' wl_linha INTO  tg_msg_ret-msg.
              MOVE: 'P_PARID'            TO tg_msg_ret-field.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret, wg_desc_parid.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF wg_fiscal-ctrl_zrfl EQ c_s.
      READ TABLE tg_parc
        WITH KEY parvw = 'Z1'.
      IF sy-subrc IS NOT INITIAL.
        MOVE: TEXT-e40            TO tg_msg_ret-msg,
              c_tab_strip_nf-tab9 TO tg_msg_ret-aba.
*          'WG_FISCAL-DOCREF'  to tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.


    IF ( wg_fiscal-retorno EQ c_s  ) AND ( wg_fiscal-docref IS INITIAL ) AND ( NOT ( tg_docrefs[] IS NOT INITIAL AND wg_docs-tcode_org IS NOT INITIAL ) ).
      MOVE: TEXT-e03            TO tg_msg_ret-msg,
            c_tab_strip_nf-tab1 TO tg_msg_ret-aba,
            'WG_FISCAL-DOCREF'  TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.


    IF wg_fiscal-aviso_rec EQ c_s
    AND wg_fiscal-ebeln IS INITIAL.
      MOVE: TEXT-e42            TO tg_msg_ret-msg,
            c_tab_strip_nf-tab1 TO tg_msg_ret-aba,
            'WG_FISCAL-EBELN'  TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wg_fiscal-zpesagem EQ c_01
    AND wg_fiscal-nr_romaneio IS INITIAL.
      MOVE: TEXT-e04                TO tg_msg_ret-msg,
            c_tab_strip_nf-tab1     TO tg_msg_ret-aba,
            'WG_FISCAL-NR_ROMANEIO' TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.


    IF wg_docs-budat  IS INITIAL.
      MOVE: TEXT-e17                TO tg_msg_ret-msg,
            space                   TO tg_msg_ret-aba,
            'WG_DOCS-BUDAT' TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wg_docs-bldat  IS INITIAL.
      MOVE: TEXT-e18                TO tg_msg_ret-msg,
            space                   TO tg_msg_ret-aba,
            'WG_DOCS-BLDAT' TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    "Validação Parametros e Montagem da Contabilização...
    SELECT *
      FROM zfiwrt0003 INTO TABLE @DATA(lit_0003)
     WHERE operacao EQ @p_operacao.

    LOOP AT lit_0003 INTO DATA(lwa_0003).

      READ TABLE tl_tbsl WITH KEY bschl = lwa_0003-bschl.

      CHECK sy-subrc EQ 0.

      IF ( tl_tbsl-koart EQ c_k ) OR ( tl_tbsl-koart EQ c_d ).

        SELECT SINGLE *
          FROM j_1bad INTO @DATA(lwa_j_1bad)
         WHERE parvw EQ @p_parvw.

        IF sy-subrc EQ 0.

          lv_parid_key = p_parid.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_parid_key
            IMPORTING
              output = lv_parid_key.

          CASE lwa_j_1bad-partyp.
            WHEN 'C'.

              SELECT SINGLE *
                FROM kna1 INTO @DATA(_wl_kna1)
               WHERE kunnr = @lv_parid_key.

              IF sy-subrc EQ 0.
                IF tl_tbsl-koart = c_d.
                  MOVE p_parid        TO tg_contab-hkont.
                ELSEIF tl_tbsl-koart = c_k.

                  IF _wl_kna1-lifnr IS INITIAL.
                    CONCATENATE 'Cliente:' _wl_kna1-kunnr 'sem fornecedor vinculado na XD03/Aba Controle!' INTO lva_msg_tmp SEPARATED BY space.
                    APPEND VALUE #( aba = c_tab_strip_nf-tab1
                                    msg = lva_msg_tmp ) TO tg_msg_ret.
                  ELSE.
                    MOVE _wl_kna1-lifnr TO tg_contab-hkont.
                  ENDIF.

                ENDIF.
              ENDIF.

            WHEN 'B' OR 'V'.

              SELECT SINGLE *
                FROM lfa1 INTO @DATA(_wl_lfa1)
               WHERE lifnr = @lv_parid_key.

              IF sy-subrc EQ 0.
                IF tl_tbsl-koart = c_k.
                  MOVE p_parid        TO tg_contab-hkont.
                ELSEIF tl_tbsl-koart = c_d.
                  IF _wl_lfa1-kunnr IS INITIAL.
                    CONCATENATE 'Fornecedor:' _wl_lfa1-lifnr 'sem cliente vinculado na XK03/Aba Controle!' INTO lva_msg_tmp SEPARATED BY space.
                    APPEND VALUE #( aba = c_tab_strip_nf-tab1
                                    msg = lva_msg_tmp ) TO tg_msg_ret.
                  ELSE.
                    MOVE _wl_lfa1-kunnr TO tg_contab-hkont.
                  ENDIF.
                ENDIF.
              ENDIF.

          ENDCASE.

        ENDIF.

      ENDIF.

    ENDLOOP.

    LOOP AT tg_contab.

      ADD tg_contab-dmbtr TO  wl_tot_contab.

    ENDLOOP.
    IF wl_tot_contab NE 0.
      MOVE: TEXT-e24                TO tg_msg_ret-msg,
            c_tab_strip_nf-tab4     TO tg_msg_ret-aba,
            space                   TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    SORT: tl_t042z BY zlsch,
          tl_tbsl  BY bschl,
          tl_cskb  BY kstar,
          tl_csks  BY kostl.

    CLEAR: wl_linha.

    LOOP AT tg_contab.

      IF tg_contab-dmbtr IS NOT INITIAL
      AND tg_contab-estorno IS INITIAL.
        ADD 1 TO wl_linha.
      ENDIF.
      READ TABLE tl_tbsl
        WITH KEY bschl = tg_contab-bschl
                 BINARY SEARCH.

      "CFOP 1206 - Anulação de valor relativo à prestação de serviço de transporte
      "CFOP 2206 - Anulação de valor relativo à prestação de serviço de transporte
      IF tg_itens-cfop(4) NE '1206' AND tg_itens-cfop(4) NE '2206'.
        IF tl_tbsl-koart EQ c_k OR tl_tbsl-koart EQ c_d.

          IF tg_contab-zlsch IS INITIAL
          AND tg_contab-estorno IS INITIAL.
            MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                  c_tab_strip_nf-tab4 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e32 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ELSE.
            READ TABLE tl_t042z
              WITH KEY zlsch = tg_contab-zlsch
                       BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL
            AND tg_contab-estorno IS INITIAL.
              MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                    c_tab_strip_nf-tab4 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e33 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.

          IF tg_contab-zfbdt IS INITIAL
          AND tg_contab-estorno IS INITIAL.
            MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                   c_tab_strip_nf-tab4 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e34 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE tl_cskb
        WITH KEY kstar = tg_contab-hkont
      BINARY SEARCH.

      IF sy-subrc IS INITIAL AND tg_contab-dmbtr IS NOT INITIAL AND tg_contab-estorno EQ abap_false.
        IF tg_contab-kostl IS INITIAL AND tl_cskb-katyp EQ '01'.
          MOVE: c_tab_strip_nf-tab4 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e38 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ELSEIF tg_contab-kostl IS NOT INITIAL AND tl_cskb-katyp NE '01'.
          MOVE: c_tab_strip_nf-tab4 TO tg_msg_ret-aba.
          MESSAGE s001 WITH tg_contab-hkont wl_linha INTO tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ELSEIF tg_contab-kostl IS NOT INITIAL.
          READ TABLE tl_csks WITH KEY kostl = tg_contab-kostl BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            MOVE: c_tab_strip_nf-tab4 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e39 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.
      ENDIF.


      SELECT SINGLE * FROM zfiwrt0008 INTO @DATA(wl_0008)
        WHERE seq_lcto EQ  @p_seq_lcto.

      IF  wl_0008-tcode_org NE 'ZSDT0165'.

        IF tg_itens-cfop(4) NE '1206' AND tg_itens-cfop(4) NE '2206'.
          IF NOT ( tg_contab-zlsch IS INITIAL ).
            CLEAR: wa_zsdt0075.
            SELECT SINGLE * FROM zsdt0075 INTO wa_zsdt0075
             WHERE kunnr = tg_contab-hkont
               AND bdatu >= sy-datum.

            IF ( sy-subrc NE 0 ) AND ( tg_contab-zlsch NE c_d ).
              MOVE: c_tab_strip_nf-tab4 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e43 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

    DESCRIBE TABLE tg_itens LINES wl_linha.
    IF wl_linha EQ 0 .
      MOVE:    TEXT-e10            TO tg_msg_ret-msg,
               c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.


    IF wg_fiscal-lm_estoque NE 'S'.
      IF wl_1baa-form IS INITIAL.
        IF wg_docs-nfenum  IS INITIAL.
          MOVE: TEXT-e16                TO tg_msg_ret-msg,
                space                   TO tg_msg_ret-aba,
                'WG_DOCS-NFENUM' TO tg_msg_ret-field.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
        "
        IF wg_docs-series EQ space.
          MOVE: TEXT-e22                TO tg_msg_ret-msg,
                space                   TO tg_msg_ret-aba,
                'WG_DOCS-SERIES' TO tg_msg_ret-field.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      IF ( NOT wg_docs-nfenum IS INITIAL ) AND ( NOT wg_docs-series IS INITIAL ).

        CONCATENATE wg_docs-nfenum '-' wg_docs-series INTO p_xblnr.

        DATA: v_ck_somente_dup TYPE char01.
        DATA: p_werks TYPE j_1bnflin-werks.
        p_werks = p_branch.

        IF wg_docs-not_check_xml EQ abap_true.
          v_ck_somente_dup = abap_true.
        ELSE.
          v_ck_somente_dup = abap_false.
        ENDIF.

        CLEAR: lv_totitens.

        lv_totitens  = REDUCE j_1bnetval( INIT x TYPE j_1bnetval FOR b IN tg_itens NEXT x = x + b-netwr ).
        lv_vlr_itens = lv_totitens.

        CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
          EXPORTING
            p_lifnr          = p_parid
            p_parvw          = p_parvw
            p_nftype         = wg_fiscal-nftype
            p_xblnr          = p_xblnr
            p_data           = wg_docs-bldat
            p_werks          = p_werks
            p_valor_nf       = lv_vlr_itens
            p_ck_somente_dup = v_ck_somente_dup
          EXCEPTIONS
            error            = 1
            OTHERS           = 2.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO tg_msg_ret-msg.
          tg_msg_ret-field = 'P_PARID'.
          APPEND tg_msg_ret.
          tg_msg_ret-field = 'WG_DOCS-NFENUM'.
          APPEND tg_msg_ret.
          tg_msg_ret-field = 'WG_DOCS-SERIES'.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.


        SELECT SINGLE * FROM j_1baa INTO wl_1baa WHERE nftype EQ wg_fiscal-nftype.
        "CS2017000751/CS2017002639
        IF wl_1baa-direct = '1'. "ENTRADA
          SELECT SINGLE *
              FROM zfiwrt0008
              INTO wa_zfiwrt0008
            WHERE parid   = p_parid "fornecedor
            AND   nfenum  = wg_docs-nfenum
            AND   series  = wg_docs-series
            AND   bldat   = wg_docs-bldat.
        ELSE.
          SELECT SINGLE *
              FROM zfiwrt0008
              INTO wa_zfiwrt0008
            WHERE nfenum  = wg_docs-nfenum
            AND   series  = wg_docs-series
            AND   bldat   = wg_docs-bldat
            AND   branch  = p_branch. "filial
        ENDIF.


        IF sy-subrc = 0.
          IF wa_zfiwrt0008-seq_lcto NE p_seq_lcto AND wa_zfiwrt0008-loekz IS INITIAL.
            CLEAR vg_candat.
            IF wa_zfiwrt0008-docnum IS NOT INITIAL.
              SELECT SINGLE candat
                   INTO vg_candat
                   FROM j_1bnfdoc
                   WHERE docnum = wa_zfiwrt0008-docnum.

              IF vg_candat IS INITIAL.
                CONCATENATE TEXT-e53  wa_zfiwrt0008-seq_lcto INTO tg_msg_ret-msg SEPARATED BY space.
                MOVE:   space           TO tg_msg_ret-aba,
                       'WG_DOCS-NFENUM' TO tg_msg_ret-field.

                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      "18.02.2019 CS2018003078
      "IF ( WL_1BAA-FORM IS INITIAL ) AND ( WL_1BAA-NFE IS NOT INITIAL ) AND ( WL_1BAA-DIRECT EQ '2' OR WL_1BAA-NFTYPE EQ 'YI'  ).
      IF vlancamento_znfw0009 IS NOT INITIAL.

        IF wg_fiscal-access_key IS INITIAL.
          MOVE: TEXT-e63                TO tg_msg_ret-msg,
             c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
             'WG_FISCAL-ACCESS_KEY'     TO tg_msg_ret-field.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ELSE.
          CLEAR: ls_acckey_str, lv_nfnum9, lv_series, lv_parid_key.
          "Check Fields Acesso
          MOVE wg_fiscal-access_key TO ls_acckey_str.

          DATA(_invalid) = abap_false.

          IF strlen( wg_fiscal-access_key ) NE 44.
            _invalid = abap_true.
          ENDIF.

          IF NOT  wg_docs-bldat+2(2) = ls_acckey_str-nfyear OR
             NOT  wg_docs-bldat+4(2) = ls_acckey_str-nfmonth.
            _invalid = abap_true.
          ENDIF.

          IF NOT wl_1baa-model IS INITIAL AND
             NOT wl_1baa-model = ls_acckey_str-model.
            _invalid = abap_true.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wg_docs-nfenum
            IMPORTING
              output = lv_nfnum9.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wg_docs-series
            IMPORTING
              output = lv_series.

          IF NOT lv_nfnum9 IS INITIAL AND
             NOT lv_nfnum9 = ls_acckey_str-nfnum9.
            _invalid = abap_true.
          ENDIF.

          IF lv_series <> ls_acckey_str-serie.
            _invalid = abap_true.
          ENDIF.

          "Validar CNPJ Emissor da Chave de Acesso
          CLEAR: lv_stcd1_ck.

          IF vlancamento_znfw0009 = 'MIC'.

            lv_parid_key = p_branch.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lv_parid_key
              IMPORTING
                output = lv_parid_key.

            SELECT SINGLE *
              FROM lfa1 INTO _wl_lfa1
             WHERE lifnr EQ lv_parid_key.

            IF sy-subrc EQ 0.
              lv_stcd1_ck = _wl_lfa1-stcd1.
            ENDIF.

          ELSE.
            SELECT SINGLE *
              FROM j_1bad INTO @DATA(wl_j_1bad)
             WHERE parvw EQ @p_parvw.

            IF sy-subrc EQ 0.

              lv_parid_key = p_parid.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = lv_parid_key
                IMPORTING
                  output = lv_parid_key.

              CASE wl_j_1bad-partyp.
                WHEN 'C'.

                  SELECT SINGLE *
                    FROM kna1 INTO _wl_kna1
                   WHERE kunnr = lv_parid_key.

                  IF sy-subrc EQ 0.
                    lv_stcd1_ck = _wl_kna1-stcd1.
                  ENDIF.

                WHEN 'B' OR 'V'.

                  SELECT SINGLE *
                    FROM lfa1 INTO _wl_lfa1
                   WHERE lifnr = lv_parid_key.

                  IF sy-subrc EQ 0.
                    IF _wl_lfa1-stcd1 IS NOT INITIAL.
                      lv_stcd1_ck = _wl_lfa1-stcd1.
                    ELSE. "CPF
                      lv_stcd1_ck2 = wl_lfa1-stcd2.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = lv_stcd1_ck2
                        IMPORTING
                          output = lv_stcd1_ck2.
                      lv_stcd1_ck = lv_stcd1_ck2.
                    ENDIF.
                  ENDIF.
              ENDCASE.
            ENDIF.
          ENDIF.

          " 23.04.2024 - 133290 - RAMON -->
          DATA lv_stcd1(14).
          "lv_stcd1_ck         = |{ lv_stcd1_ck ALPHA = IN }|.
          lv_stcd1 = |{ lv_stcd1_ck ALPHA = IN }|.
          lv_stcd1_ck = lv_stcd1.
          " 23.04.2024 - 133290 - RAMON --<

          ls_acckey_str-stcd1 = |{ ls_acckey_str-stcd1 ALPHA = IN }|.

          IF lv_stcd1_ck <> ls_acckey_str-stcd1.
            _invalid = abap_true.
          ENDIF.

*-CS2021000595 - 22.06.2021 - JT - inicio
*         IF _invalid EQ abap_true.
*         IF _invalid EQ abap_true AND p_operacao <> '0093'.
          IF _invalid EQ abap_true AND NOT ( lv_series GE '890' AND lv_series LE '899' ).
*-CS2021000595 - 22.06.2021 - JT - fim
            MOVE: TEXT-e64             TO tg_msg_ret-msg,
            c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
            'WG_FISCAL-ACCESS_KEY'     TO tg_msg_ret-field.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    SORT: tl_mcha BY matnr werks charg,
          tl_t001l BY werks lgort,
          tl_mara BY matnr,
          tl_t001k BY bwkey.

    LOOP AT tg_itens.
      wl_linha = sy-tabix.
      "CS2021000825
      IF p_branch NE tg_itens-werks.
        SELECT SINGLE j_1bbranch
          FROM t001w
          INTO @DATA(_local)
        WHERE werks =  @tg_itens-werks.
        IF _local NE p_branch.
          MOVE:
          c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e85 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
      "CS2021000825
      "
      SELECT SINGLE *
      FROM zfiwrt0001
      INTO @DATA(wl_0001_)
       WHERE operacao EQ @p_operacao.
      IF sy-subrc = 0.
        IF wl_0001_-lm_indea = 'S'.
          v_ebelp = tg_itens-itmnum.
          SELECT SINGLE * FROM zmmt0102 INTO wa_zmmt0102 WHERE ebeln = p_seq_lcto AND ebelp = v_ebelp.
          IF sy-subrc NE 0.
            MOVE:
            c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e78 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
*          IF TG_ITENS-RENAS IS INITIAL.
*            MOVE C_TAB_STRIP_NF-TAB6 TO TG_MSG_RET-ABA.
*            CONCATENATE TEXT-E80 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*            APPEND TG_MSG_RET.
*            CLEAR: TG_MSG_RET.
*          ENDIF.
        ENDIF.
        IF wl_0001_-ge_remessa = 'S'.
          IF  tg_itens-vbeln IS INITIAL.
            MOVE:
            c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e68 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
          IF  tg_itens-posnr IS INITIAL.
            MOVE:
            c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e69 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
          IF  tg_itens-vbeln IS NOT INITIAL AND
              tg_itens-posnr IS NOT INITIAL.
            SELECT SINGLE *
              FROM vbap
              INTO wl_vbap
              WHERE vbeln = tg_itens-vbeln
              AND   posnr = tg_itens-posnr.
            IF  sy-subrc NE 0.
              MOVE:
              c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e70 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ELSEIF wl_vbap-matnr NE tg_itens-matnr.
              MOVE:
              c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e71 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ELSE.
              IF p_branch NE wl_vbap-werks.
                MOVE:
                c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
                CONCATENATE TEXT-e74 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ENDIF.
              SELECT SINGLE *
                FROM vbak
                INTO @DATA(wl_vbak)
                WHERE vbeln = @tg_itens-vbeln.

              SELECT SINGLE *
                FROM kna1
                INTO @DATA(wkna1)
                WHERE kunnr = @wl_vbak-kunnr.

              SELECT SINGLE *
                FROM j_1bad INTO lwa_j_1bad
               WHERE parvw EQ p_parvw.

              IF sy-subrc NE 0.
                tg_msg_ret-aba = c_tab_strip_nf-tab6.
                tg_msg_ret-msg = 'Cadastro tipo parceiro não encontrado:' && p_parvw.
                APPEND tg_msg_ret.
              ELSE.

                CASE lwa_j_1bad-partyp.
                  WHEN 'C' OR 'B'.
                    IF wkna1-kunnr NE p_parid.
                      tg_msg_ret-aba = c_tab_strip_nf-tab6.
                      CONCATENATE 'Cliente OV: ' wkna1-kunnr 'não corresponde ao parceiro do lançamento:' p_parid INTO tg_msg_ret-msg SEPARATED BY space.
                      APPEND tg_msg_ret.
                    ENDIF.
                  WHEN 'V'.

                    IF wkna1-lifnr IS INITIAL.

                      CONCATENATE 'Cliente:' wkna1-kunnr 'sem fornecedor vinculado na XD03/Aba Controle!' INTO lva_msg_tmp SEPARATED BY space.
                      APPEND VALUE #( aba = c_tab_strip_nf-tab1
                                      msg = lva_msg_tmp ) TO tg_msg_ret.
                    ELSE.

                      IF wkna1-lifnr NE p_parid.
                        MOVE:
                        c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
                        CONCATENATE TEXT-e72 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
                        APPEND tg_msg_ret.
                        CLEAR: tg_msg_ret.
                      ELSE.
                        SELECT SINGLE *
                           FROM lfa1
                           INTO @DATA(wlfa1)
                           WHERE lifnr = @p_parid.
                        IF wkna1-stcd1 NE wlfa1-stcd1.
                          MOVE:
                          c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
                          CONCATENATE TEXT-e73 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
                          APPEND tg_msg_ret.
                          CLEAR: tg_msg_ret.
                        ENDIF.
                      ENDIF.

                    ENDIF.

                ENDCASE.

              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.


        IF wl_0001_-aviso_rec = 'S'.

          SELECT SINGLE * FROM ekpo INTO @DATA(wekpo)
              WHERE ebeln EQ @wg_fiscal-ebeln
               AND  ebelp EQ '00010'.

          IF wekpo-bstae IS INITIAL.
            MOVE: c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e81 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'J_1B_MATERIAL_READ'
        EXPORTING
          matnr               = tg_itens-matnr
          val_area            = tg_itens-werks
          val_type            = space
          language            = sy-langu
          i_werks             = p_branch
*      IMPORTING
*         nbm                 = sl_item-nbm
*         matuse              = sl_item-matuse
*         matorg              = sl_item-matorg
*         material_text_record = wa_material_text_record
*         e_matkl             = sl_item-matkl
        EXCEPTIONS
          material_not_found  = 1
          valuation_not_found = 2
          OTHERS              = 3.
      IF sy-subrc IS NOT INITIAL.
*      wl_linha = sy-tabix.
        MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
              c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e15 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF tg_itens-matnr IS INITIAL
      OR tg_itens-werks IS INITIAL
      OR tg_itens-netwr IS INITIAL
      OR ( wg_fiscal-imobilizado EQ c_s
      AND tg_itens-anln1 IS INITIAL ).
        IF w_zfiwrt0001-complement_icms = 'S' AND tg_itens-netwr IS INITIAL.
        ELSE.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e01 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      ENDIF.

      IF tg_itens-matnr IS NOT INITIAL
      AND tg_itens-werks IS NOT INITIAL
      AND tg_itens-cfop IS INITIAL.
        IF  wg_fiscal-lm_estoque NE 'S'.
          MOVE:   c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e12 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      ENDIF.

      IF tg_itens-charg IS NOT INITIAL.
*      wl_linha = sy-tabix.
        READ TABLE tl_mcha
          WITH KEY matnr = tg_itens-matnr
                   werks = tg_itens-werks
                   charg = tg_itens-charg
                   BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e13 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

*Inicio Alteração - Leandro Valentim Ferreira - 06.06.23 - 91631
      IF tg_itens-anln1 IS NOT INITIAL.
        IF wg_fiscal-imobilizado NE 'S' AND wg_fiscal-tp_mv_imob NE 'C'.
          SELECT SINGLE *
           FROM anla
           INTO wl_anla
           WHERE  anln1 EQ tg_itens-anln1
            AND bukrs EQ p_bukrs.
          IF sy-subrc NE 0.
            CONCATENATE 'Este Imobilizado não pertence a empresa!' p_bukrs INTO tg_msg_ret-msg SEPARATED BY space.

            MOVE: c_tab_strip_nf-tab3 TO tg_msg_ret-aba,
                 'TG_ITENS-ANLN1'  TO tg_msg_ret-field.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.

        IF wg_fiscal-imobilizado EQ 'S' AND wg_fiscal-tp_mv_imob EQ 'V'.
          CLEAR wl_anla.
          SELECT SINGLE *
                 FROM anla
                 INTO wl_anla
                 WHERE anln1 EQ tg_itens-anln1
                   AND bukrs EQ p_bukrs
                   AND leart IN ('03','04').

          IF sy-subrc EQ 0.
            tg_msg_ret-msg = 'Ativo imobilizado com restrições de movimentação. Por favor entrar em contato com imobilizado@amaggi.com.br'.
            MOVE: c_tab_strip_nf-tab3 TO tg_msg_ret-aba,
                 'TG_ITENS-ANLN1'  TO tg_msg_ret-field.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.

        IF wg_fiscal-imobilizado EQ 'S' AND wg_fiscal-tp_mv_imob EQ 'T'.
          CLEAR tl_anlc.
          SELECT *
                 INTO TABLE tl_anlc
                 FROM anlc
                 WHERE bukrs EQ p_bukrs
                   AND anln1 EQ tg_itens-anln1
                   AND afabe EQ '5'.

          IF sy-subrc EQ 0.
            SORT tl_anlc BY gjahr DESCENDING.
            READ TABLE tl_anlc INTO DATA(wl_anlc) INDEX 1.
            IF wl_anlc-ansaz > 0.
              tg_msg_ret-msg = ' EXISTE ADIANTAMENTO EM ABERTO PARA O IMOBILIZADO INFORMADO'.
              MOVE: c_tab_strip_nf-tab3 TO tg_msg_ret-aba,
                   'TG_ITENS-ANLN1'  TO tg_msg_ret-field.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.

***  =======================================================================================cs2023000072 - validação centro de custo transferências imobilizado / aoenning

          "Se Centro de destino for diferente do centro informado no ID Parceiro: Bloquear lançamento.
          vg_parid = |{ p_parid ALPHA = IN }|.
          IF wg_fiscal-move_plant <> vg_parid.
            tg_msg_ret-msg = 'Centro de destino é diferente do ID Parceiro'.
            MOVE: c_tab_strip_nf-tab3 TO tg_msg_ret-aba,
                  'TG_ITENS-ANLN1'  TO tg_msg_ret-field.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.

          IF wg_fiscal-kostl IS NOT INITIAL.

            "Consultar centro de custo.
            SELECT SINGLE gsber FROM csks INTO @DATA(vg_gsber)
              WHERE kostl EQ @wg_fiscal-kostl.
            IF vg_gsber <> vg_parid. "Se Centro de custo for diferente do centro informado no ID Parceiro: Bloquear lançamento.
              tg_msg_ret-msg = 'Centro de custo não pertence ao ID Parceiro informado'.
              MOVE: c_tab_strip_nf-tab3 TO tg_msg_ret-aba,
                  'TG_ITENS-ANLN1'  TO tg_msg_ret-field.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: vg_gsber, vg_parid.

***=============================================================================================CS2023000072 - Validação centro de custo transferências IMOBILIZADO / AOENNING

      ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 06.06.23 - 91631

      IF wg_fiscal-servico EQ 'N'.
        IF tg_itens-steuc IS INITIAL
        OR tg_itens-steuc EQ 'NCM00'.
*        wl_linha = sy-tabix.

          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e19 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.

        ENDIF.
      ENDIF.
      IF tg_itens-lgort IS NOT INITIAL.
*      wl_linha = sy-tabix.
        READ TABLE tl_t001l
          WITH KEY werks = tg_itens-werks
                   lgort = tg_itens-lgort
                   BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e14 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.


      READ TABLE tl_mara
          WITH KEY matnr = tg_itens-matnr
                   BINARY SEARCH.

      IF tg_itens-charg IS INITIAL.
        IF tl_mara-xchpf IS NOT INITIAL.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e29 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      IF tl_mara-mstae IS NOT INITIAL.
        MOVE:  c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e58 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF tg_itens-werks IS NOT INITIAL.

        READ TABLE tl_t001k
          WITH KEY bwkey = tg_itens-werks
                   BINARY SEARCH.
        IF tl_t001k-bukrs NE p_bukrs.
          MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e30 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

        CLEAR wl_marc.
        SELECT SINGLE *
          FROM marc
          INTO wl_marc
          WHERE matnr = tg_itens-matnr
          AND   werks = tg_itens-werks.
        IF wl_marc-mmsta  IS NOT INITIAL.
          MOVE:  c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e58 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

        IF wl_marc-steuc  IS INITIAL.
          MOVE:  c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e61 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

        IF tl_mara-mtart NE 'ABF'.
          CLEAR wl_mbew.
          SELECT SINGLE *
            FROM mbew
            INTO wl_mbew
            WHERE matnr = tg_itens-matnr
            AND   bwkey = tg_itens-werks.
          IF wl_mbew-mtuse  IS INITIAL.
            MOVE:  c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e59 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
          IF wl_mbew-mtorg   IS INITIAL.
            MOVE:  c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e60 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.

      ENDIF.

      READ TABLE tl_2002
        WITH KEY par_value  = 'ZFIWRT0009'
                 par_value2 = 'LGORT'.
      IF sy-subrc IS INITIAL.
        IF tg_itens-lgort IS INITIAL.
          MOVE: TEXT-e25                TO tg_msg_ret-msg,
                c_tab_strip_nf-tab6     TO tg_msg_ret-aba.
*          'WG_FISCAL-MOVE_STLOC'     TO tg_msg_ret-field.
          CONCATENATE TEXT-e26 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.

        ENDIF.
      ENDIF.

      "Verificação de Documento Anulado Referênciado
      "CFOP 1206 - Anulação de valor relativo à prestação de serviço de transporte
      "CFOP 2206 - Anulação de valor relativo à prestação de serviço de transporte
      IF tg_itens-cfop(4) EQ '1206' OR tg_itens-cfop(4) EQ '2206'.
        DESCRIBE TABLE tg_docrefs LINES lc_qtd_linhas.
        IF lc_qtd_linhas IS INITIAL.
          tg_msg_ret-aba = c_tab_strip_nf-tab1.
          CONCATENATE TEXT-e49 'LINHA:' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ELSE.
          LOOP AT tg_docrefs.

            "Verificar se Documento Existe
            SELECT SINGLE * INTO wa_j_1bnfdoc
              FROM j_1bnfdoc
             WHERE docnum EQ tg_docrefs-docnum.

            IF sy-subrc IS NOT INITIAL.
              tg_msg_ret-aba = c_tab_strip_nf-tab1.
              CONCATENATE TEXT-e51 'LINHA:' wl_linha 'DOC.:'  tg_docrefs-docnum INTO tg_msg_ret-msg SEPARATED BY space.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ELSE.
              "Verificar se Está cancelado
              IF wa_j_1bnfdoc-cancel EQ abap_true.
                tg_msg_ret-aba = c_tab_strip_nf-tab1.
                CONCATENATE TEXT-e50 'LINHA:' wl_linha 'DOC.:'  tg_docrefs-docnum INTO tg_msg_ret-msg SEPARATED BY space.
                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ELSE.

                "Procura se o Documento está referênciado em outro documento
                SELECT SINGLE * INTO wa_zfiwrt0020
                  FROM zfiwrt0020
                 WHERE docnum   EQ tg_docrefs-docnum
                   AND seq_lcto NE p_seq_lcto.

                IF sy-subrc IS INITIAL.
                  SELECT SINGLE * INTO wa_zfiwrt0008
                    FROM zfiwrt0008
                   WHERE seq_lcto EQ wa_zfiwrt0020-seq_lcto.

                  IF sy-subrc IS INITIAL AND wa_zfiwrt0008-docnum IS NOT INITIAL.

                    SELECT SINGLE * INTO wa_j_1bnfdoc
                      FROM j_1bnfdoc
                     WHERE docnum EQ wa_zfiwrt0008-docnum
                       AND cancel EQ abap_false.

                    IF sy-subrc IS INITIAL.
                      tg_msg_ret-aba = c_tab_strip_nf-tab1.
                      CONCATENATE TEXT-e52 'LINHA:' wl_linha 'DOC.:'  tg_docrefs-docnum 'DOC.:' wa_zfiwrt0008-docnum INTO tg_msg_ret-msg SEPARATED BY space.
                      APPEND tg_msg_ret.
                      CLEAR: tg_msg_ret.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF wl_linha EQ 1.

          CLEAR: lv_totref, lv_totitens.

          lv_totref   = REDUCE j_1bnftot( INIT x TYPE j_1bnftot FOR a IN tg_docrefs NEXT x = x + a-nftot ).
          lv_totitens = REDUCE j_1bnetval( INIT x TYPE j_1bnetval FOR b IN tg_itens NEXT x = x + b-netwr ).
          lv_totdiff = abs( lv_totref - lv_totitens ).
          IF lv_totdiff GT '0.01'.
            APPEND VALUE #(
                            aba = c_tab_strip_nf-tab1
                         msg = 'Valor Total dos Documentos Referênciados Difere do Valor Total dos Itens Nota!'
                       ) TO tg_msg_ret.
          ENDIF.
        ENDIF.
      ENDIF.

*-CS2020001331 - 06.10.2021 - JT - inicio
      READ TABLE tg_impo_comp WITH KEY itmnum = tg_itens-itmnum
                                       taxtyp = c_icop.
      IF sy-subrc = 0.
        IF tg_itens-possui_icms_st IS INITIAL.
          CONCATENATE 'Informar campo ICMS ST. Linha:' wl_linha INTO tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
*-CS2020001331 - 06.10.2021 - JT - fim

    ENDLOOP.

    DATA: soma_vlr_total  LIKE tg_itens-netwr,
          diferenca       TYPE zfit0141-tolerancia,
          diferenca_icms  TYPE zfit0141-tolerancia,
          tolerancia_min  TYPE zfit0141-tolerancia,
          tolerancia_max  TYPE zfit0141-tolerancia,
          lit_matnr_ov_pd LIKE TABLE OF git_matnr_ov_pd,
          lva_pedido_ov   TYPE c LENGTH 20,
          lva_vlr_imposto TYPE zib_nfe_dist_itm-icms_valor,
          lva_valor_icms  TYPE zib_nfe_dist_itm-icms_valor,
          lva_vlr_str_1   TYPE c LENGTH 50,
          lva_vlr_str_2   TYPE c LENGTH 50,
          lit_impo_aux    LIKE TABLE OF tg_impo WITH HEADER LINE,
          valida.

    IF wg_fiscal-access_key IS NOT INITIAL.
      SELECT SINGLE tolerancia
        FROM zfit0141
        INTO @DATA(tolerancia)
        WHERE chave EQ @wg_fiscal-access_key.
    ENDIF.

    tolerancia_min = tolerancia * -1.
    tolerancia_max = tolerancia.
    soma_vlr_total = 0.

    IF ( viniciou_lcto_znfw0009 IS NOT INITIAL ) AND ( tg_itens[] IS NOT INITIAL ) AND ( vlancamento_znfw0009 = 'MIC' OR vlancamento_znfw0009 = 'ONF' ).

      PERFORM f_get_matnr_ov_pd TABLES lit_matnr_ov_pd.

      DATA(lva_valida_icms) = abap_true.

      SELECT *
        FROM setleaf INTO TABLE @DATA(lit_set_ncm_excecao)
       WHERE setname EQ 'ZNFW0002_VALIDA_NCM_EXC'.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(lwa_setleaf)
       WHERE setname EQ 'ZNFW0002_VALIDA_ICMS_EXC'
         AND valfrom EQ @p_operacao.

      IF sy-subrc EQ 0.
        lva_valida_icms = abap_false.
      ENDIF.

      CLEAR: tg_mara_itens[].
      IF tg_itens[] IS NOT INITIAL.
        SELECT matnr matkl
          FROM mara INTO TABLE tg_mara_itens
           FOR ALL ENTRIES IN tg_itens
         WHERE matnr EQ tg_itens-matnr.
      ENDIF.

      LOOP AT tg_itens INTO DATA(wl_itens).

        DATA(lva_tabix) = sy-tabix.

        DATA(lva_valida_ncm) = abap_true.

        soma_vlr_total =  soma_vlr_total + wl_itens-netwr.

        IF ( wg_fiscal-ebeln IS NOT INITIAL OR wg_fiscal-vbeln IS NOT INITIAL ).

          IF wg_fiscal-ebeln IS NOT INITIAL.
            lva_pedido_ov = wg_fiscal-ebeln.
          ELSEIF wg_fiscal-vbeln IS NOT INITIAL..
            lva_pedido_ov = wg_fiscal-vbeln.
          ENDIF.

          READ TABLE  lit_matnr_ov_pd TRANSPORTING NO FIELDS WITH KEY matnr = wl_itens-matnr.
          IF sy-subrc <> 0.
            CONCATENATE 'O material informado:' wl_itens-matnr ' não existe no pedido/ordem de venda:' lva_pedido_ov '!'
                   INTO DATA(lva_msg_erro) SEPARATED BY space.

            APPEND VALUE #(
                     aba =  c_tab_strip_nf-tab6
                     msg = lva_msg_erro
                        ) TO tg_msg_ret.
          ENDIF.

        ENDIF.

        READ TABLE tg_mara_itens WITH KEY matnr = wl_itens-matnr.
        IF ( sy-subrc EQ 0 ) AND ( tg_mara_itens-matkl IS NOT INITIAL ).
          READ TABLE lit_set_ncm_excecao WITH KEY valfrom = tg_mara_itens-matkl TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            lva_valida_ncm = abap_false.
          ENDIF.
        ENDIF.

        IF lva_valida_ncm EQ abap_true.
          DATA(lva_ncm_item_xml) = wl_itens-ncm_xml.
          DATA(lva_ncm_item_lct) = wl_itens-steuc.

          REPLACE ALL OCCURRENCES OF: '.' IN lva_ncm_item_xml  WITH ' ',
                                      '.' IN lva_ncm_item_lct  WITH ' '.

          CONDENSE: lva_ncm_item_lct, lva_ncm_item_xml NO-GAPS.

          IF lva_ncm_item_xml <> lva_ncm_item_lct.
            CONCATENATE 'NCM' lva_ncm_item_lct 'do material:' wl_itens-matnr 'não corresponde ao NCM do item do XML:' lva_ncm_item_xml '!'
                     INTO lva_msg_erro SEPARATED BY space.

            APPEND VALUE #(
                       aba =  c_tab_strip_nf-tab6
                       msg = lva_msg_erro
                          ) TO tg_msg_ret.
          ENDIF.
        ENDIF.

        "Validação Valor ICMS
        IF lva_valida_icms EQ abap_true.

          CLEAR: lit_impo_aux[], lva_vlr_imposto.
          PERFORM monta_impostos TABLES lit_impo_aux USING lva_tabix.

          LOOP AT lit_impo_aux INTO DATA(lwa_impo_aux) WHERE taxtyp EQ c_icm3.
            ADD lwa_impo_aux-taxval TO lva_vlr_imposto.
          ENDLOOP.

          IF ( lva_vlr_imposto <> wl_itens-vlr_icms_xml ).

            diferenca_icms = lva_vlr_imposto - wl_itens-vlr_icms_xml.

            IF ( NOT ( diferenca_icms BETWEEN tolerancia_min AND tolerancia_max ) ) AND abs( diferenca_icms ) > 1.

              WRITE: lva_vlr_imposto       TO lva_vlr_str_1,
                     wl_itens-vlr_icms_xml TO lva_vlr_str_2.

              CONDENSE: lva_vlr_str_1, lva_vlr_str_2 NO-GAPS.

              CONCATENATE 'Valor ICMS:' lva_vlr_str_1 'do item:' wl_itens-itmnum ' diferente do valor do ICMS do XML:' lva_vlr_str_2 '!'
                       INTO lva_msg_erro SEPARATED BY space.

              APPEND VALUE #(
                         aba =  c_tab_strip_nf-tab6
                         msg = lva_msg_erro
                            ) TO tg_msg_ret.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

      "==================================================Ajuste no valor total BUG SOLTO #137346 / AOENNING
      "Verifica se o vlr total da nota esta preenchido.
      IF vlr_total_nota IS INITIAL.
        IF lit_zib_nfe_dist_itm IS NOT INITIAL.
          LOOP AT lit_zib_nfe_dist_itm INTO DATA(lwa_zib_nfe_dist_itm).
            CLEAR: vlr_total_item.
            vlr_total_item = lwa_zib_nfe_dist_itm-prod_vlr_total_b - lwa_zib_nfe_dist_itm-prod_vl_desconto + lwa_zib_nfe_dist_itm-prod_vl_frete + lwa_zib_nfe_dist_itm-prod_vl_seguro + lwa_zib_nfe_dist_itm-prod_vl_outro.
            ADD vlr_total_item TO vlr_total_nota.
            CLEAR: lwa_zib_nfe_dist_itm.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "==================================================Ajuste no valor total BUG SOLTO #137346 / AOENNING

      diferenca = soma_vlr_total - vlr_total_nota.

      IF abs( diferenca ) > 1.

        IF ( soma_vlr_total <> vlr_total_nota ) AND NOT ( diferenca BETWEEN tolerancia_min AND tolerancia_max ).
          APPEND VALUE #(
                     aba =  c_tab_strip_nf-tab6
                     msg = 'O Valor total dos itens  não corresponde ao Valor da Nota Fiscal!'
                         ) TO tg_msg_ret.
        ENDIF.
      ENDIF.

*** PBI - 68354 - Inicio - CBRAND
    ELSE.
      IF ( tg_itens[] IS NOT INITIAL ).
        "Validação Valor ICMS
        CLEAR: wl_itens.
        LOOP AT tg_itens INTO wl_itens.
          lva_tabix = sy-tabix.

          CLEAR: lit_impo_aux, lva_vlr_imposto. "Limpeza variavel IR175651.
          PERFORM monta_impostos TABLES lit_impo_aux USING lva_tabix.
        ENDLOOP.

        LOOP AT lit_impo_aux INTO lwa_impo_aux WHERE taxtyp EQ c_icm3.
          ADD lwa_impo_aux-taxval TO lva_valor_icms.
        ENDLOOP.

*-CS2023000043-14.02.2023-#102019-JT-COMENTADO
        lv_iv_icms = 'S'.
        EXPORT lv_iv_icms TO MEMORY ID 'ZIV_ICMS'.
*-CS2023000043-14.02.2023-#102019-JT-COMENTADO

        CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
          EXPORTING
            p_lifnr      = p_parid
            p_parvw      = p_parvw
            p_nftype     = wg_fiscal-nftype
            p_xblnr      = p_xblnr
            p_data       = wg_docs-bldat
            p_werks      = p_werks
            p_valor_icms = lva_valor_icms
            p_iva        = wg_direitos-taxcode
          EXCEPTIONS
            error        = 1
            OTHERS       = 2.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO tg_msg_ret-msg.
          tg_msg_ret-field = 'P_PARID'.
          APPEND tg_msg_ret.
          tg_msg_ret-field = 'WG_DOCS-NFENUM'.
          APPEND tg_msg_ret.
          tg_msg_ret-field = 'WG_DOCS-SERIES'.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      ENDIF.
*** PBI - 68354 - Fim - CBRAND
    ENDIF.


    IF ( wg_fiscal-retorno EQ 'S' ) AND ( NOT ( tg_docrefs[] IS NOT INITIAL AND wg_docs-tcode_org IS NOT INITIAL ) ).
      wl_seq_lcto = wg_fiscal-docref.

      CALL FUNCTION 'ZNFW_BUSCA_SALDO_RETORNO_NEW'
        EXPORTING
          i_bukrs       = p_bukrs
          i_branch      = p_branch
          i_parvw       = p_parvw
          i_parid       = p_parid
          i_seq_lcto    = wl_seq_lcto
          i_seq_modify  = p_seq_lcto
          i_imobilizado = wg_fiscal-imobilizado
        TABLES
          et_itens      = tl_0009
        EXCEPTIONS
          sem_saldo     = 1
          OTHERS        = 2.

      IF sy-subrc <> 0.
        MOVE: TEXT-e21            TO tg_msg_ret-msg,
              c_tab_strip_nf-tab1 TO tg_msg_ret-aba,
              'WG_FISCAL-DOCREF'  TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
      LOOP AT tl_0009.
        READ TABLE tg_itens
          WITH KEY itmnum = tl_0009-itmnum.
        IF sy-subrc IS INITIAL.
          IF tg_itens-menge GT tl_0009-menge.
            wl_linha = sy-tabix.
            MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
                c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e20 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.



*---------------------------------------------------------------------
  "Valida formulario
*---------------------------------------------------------------------
  IF wl_0001_1-nftype IS NOT INITIAL
    AND p_bukrs IS NOT INITIAL
     AND p_branch IS NOT INITIAL.

    "Verifica-se existe formulario cadastrado
    SELECT SINGLE nftype, form FROM j_1baa
      INTO @DATA(wa_1baa)
      WHERE nftype = @wl_0001_1-nftype.

    IF ( sy-subrc EQ 0 ) AND ( wa_1baa-form IS NOT INITIAL ).

      "Verifica se existe formulário cadastrado
      SELECT SINGLE bukrs, branch, form FROM j_1bb2
        INTO @DATA(wl_1bb2)
        WHERE bukrs  = @p_bukrs
          AND branch = @p_branch
          AND form   = @wa_1baa-form.

      IF ( sy-subrc NE 0 ) OR ( wl_1bb2-form IS INITIAL ).

        MESSAGE s000(z_les) WITH
'Formulário de impressão não configurado para a ' 'filial informada. Por favor criar ' 'FI para o departamento fiscal.'  INTO tg_msg_ret-msg.
        MOVE: c_tab_strip_nf-tab1        TO tg_msg_ret-aba,
              'WG_FISCAL-NFTYPE'         TO tg_msg_ret-field.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

    ENDIF.

  ENDIF.
*---------------------------------------------------------------------

  IF wg_fiscal-complemento EQ 'S'.
    REFRESH tg_msg_ret.
    IF wg_docs-budat  IS INITIAL.
      MOVE: TEXT-e17                TO tg_msg_ret-msg,
            space                   TO tg_msg_ret-aba,
            'WG_DOCS-BUDAT' TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wg_docs-bldat  IS INITIAL.
      MOVE: TEXT-e18                TO tg_msg_ret-msg,
            space                   TO tg_msg_ret-aba,
            'WG_DOCS-BLDAT' TO tg_msg_ret-field.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
    LOOP AT tg_itens.
      wl_linha = sy-tabix.
      IF tg_itens-werks IS INITIAL.
        MOVE: "TEXT-E01            TO TG_MSG_RET-MSG,
              c_tab_strip_nf-tab6 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e41 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wg_fiscal-disp_nf_cct EQ 'S'.
    IF wg_transporte-lifnr IS INITIAL.
      MOVE: TEXT-e62                   TO tg_msg_ret-msg,
            c_tab_strip_nf-tab10       TO tg_msg_ret-aba,
            'WG_TRANSPORTE-LIFNR'      TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
    IF wg_transporte-placa_car1 IS NOT INITIAL.
      SELECT SINGLE * FROM zlest0002 INTO wl_zlest0002 WHERE pc_veiculo =  wg_transporte-placa_car1.
      MOVE: TEXT-e75                   TO tg_msg_ret-msg,
            c_tab_strip_nf-tab10       TO tg_msg_ret-aba,
            'WG_TRANSPORTE-PLACA_CAR1'      TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
    IF wg_transporte-placa_car2 IS NOT INITIAL.
      SELECT SINGLE * FROM zlest0002 INTO wl_zlest0002 WHERE pc_veiculo =  wg_transporte-placa_car2.
      MOVE: TEXT-e76                   TO tg_msg_ret-msg,
            c_tab_strip_nf-tab10       TO tg_msg_ret-aba,
            'WG_TRANSPORTE-PLACA_CAR2'      TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
    IF wg_transporte-placa_car3 IS NOT INITIAL.
      SELECT SINGLE * FROM zlest0002 INTO wl_zlest0002 WHERE pc_veiculo =  wg_transporte-placa_car3.
      MOVE: TEXT-e76                   TO tg_msg_ret-msg,
            c_tab_strip_nf-tab10       TO tg_msg_ret-aba,
            'WG_TRANSPORTE-PLACA_CAR3'      TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen      = '100'
*     I_SHOW        = C_X
      i_repid       = sy-repid
      i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
      i_set_field   = 'X_FIELD'
    IMPORTING
      e_messagem    = wg_mensagem
    TABLES
      it_msgs       = tg_msg_ret.



ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_APROV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_aprov .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZFIWRT0007' 'NIVEL_APROV'    'TG_APROV' 'NIVEL_APROV'     'Nivel Aprov.'  '10' ' ' ' ' ' ',
        2 'ZFIWRT0007' 'USNAM'          'TG_APROV' 'USNAM'           ' '  '8' ' ' ' ' ' ',
        3 'USER_ADDR'  ' '              'TG_APROV' 'NOME'            'Nome Completo'  '25'  ' ' ' ' ' ',
        4 'ZFIWRT0007' 'DEPARTAMENTO'   'TG_APROV' 'DEPARTAMENTO'    'Departamento'  '25'  ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_APROV
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_PARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_parc .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
          1 'ZFIWRT0015' 'PARVW'        'TG_PARC' 'PARVW'     'Função Parceiro'  '10' ' ' ' ' ' ',
          2 'ZFIWRT0015' 'PARID'        'TG_PARC' 'PARID'     'Parceiro'  '8' ' ' ' ' ' ',
          3 ' '          ' '            'TG_PARC' 'NOME'      'Nome do Parceiro'  '25'  ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_PARC
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0260  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0260 INPUT.

ENDMODULE.                 " USER_COMMAND_0260  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_style .
  DATA : style TYPE lvc_t_styl WITH HEADER LINE.

  LOOP AT tg_parc.
    REFRESH: style.
    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      style-fieldname = w_fieldcatalog-fieldname.
      APPEND style.
    ENDLOOP.
    INSERT LINES OF style INTO TABLE tg_parc-style.
    MODIFY tg_parc.
  ENDLOOP.
ENDFORM.                    " MONTAR_STYLE
*&---------------------------------------------------------------------*
*&      Form  f_preencher_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*----------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.                    " f_preencher_dynpro
*&---------------------------------------------------------------------*
*&      Module  MATCHCODE_DOCREF  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE matchcode_docref INPUT.
  DATA: BEGIN OF tl_docref OCCURS 0,
          seq_lcto TYPE zfiwrt0008-seq_lcto,
*         OPERACAO TYPE ZFIWRT0008-OPERACAO,
          bukrs    TYPE zfiwrt0008-bukrs,
          branch   TYPE zfiwrt0008-branch,
          parvw    TYPE zfiwrt0008-parvw,
          parid    TYPE zfiwrt0008-parid,
          nfenum   TYPE zfiwrt0008-nfenum,
          series   TYPE zfiwrt0008-series,
        END OF tl_docref.

  DATA: tl_itens TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
*        tl_bnfdoc type table of j_1bnfdoc with header line,
        tl_0008  TYPE TABLE OF zfiwrt0008 WITH HEADER LINE.

  REFRESH: tl_docref, t_fieldtab, tl_itens, tl_0008, tl_bnfdoc.
  CLEAR:   tl_docref, t_fieldtab, tl_itens, tl_0008, tl_bnfdoc.

  CALL FUNCTION 'ZNFW_BUSCA_SALDO_RETORNO_NEW'
    EXPORTING
      i_bukrs       = p_bukrs
      i_branch      = p_branch
      i_parvw       = p_parvw
      i_parid       = p_parid
      i_imobilizado = wg_fiscal-imobilizado
*     I_SEQ_LCTO    =
    TABLES
      et_itens      = tl_itens
    EXCEPTIONS
      sem_saldo     = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não ha documentos com saldo'
                                           'existente para nota fiscal'
                                           'de retorno.'.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    SORT tl_itens BY seq_lcto.
    DELETE ADJACENT DUPLICATES FROM tl_itens COMPARING seq_lcto.
    IF tl_itens[] IS NOT INITIAL.
      SELECT *
        FROM zfiwrt0008
        INTO TABLE tl_0008
         FOR ALL ENTRIES IN tl_itens
         WHERE seq_lcto EQ tl_itens-seq_lcto.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM j_1bnfdoc
          INTO TABLE tl_bnfdoc
           FOR ALL ENTRIES IN tl_0008
           WHERE docnum EQ tl_0008-docnum.

      ENDIF.
    ENDIF.

    SORT: tl_0008 BY seq_lcto,
          tl_bnfdoc BY docnum.

    LOOP AT tl_itens.
      READ TABLE tl_0008
        WITH KEY seq_lcto = tl_itens-seq_lcto
                    BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        READ TABLE tl_bnfdoc
          WITH KEY docnum = tl_0008-docnum
                    BINARY SEARCH.

      ENDIF.

      IF tl_bnfdoc-nfenum IS NOT INITIAL.
        MOVE: tl_bnfdoc-nfenum TO tl_docref-nfenum,
              tl_bnfdoc-series TO tl_docref-series.
      ELSE.
        MOVE: tl_bnfdoc-nfnum  TO tl_docref-nfenum,
              tl_bnfdoc-series TO tl_docref-series.
      ENDIF.

      MOVE: tl_itens-seq_lcto TO tl_docref-seq_lcto,
            p_bukrs           TO tl_docref-bukrs,
            p_branch          TO tl_docref-branch,
            p_parvw           TO tl_docref-parvw,
            p_parid           TO tl_docref-parid.

      APPEND tl_docref.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'SEQ_LCTO'
        dynpprog        = sy-repid                          "'ZFINR018'
        dynpnr          = sy-dynnr
        dynprofield     = 'WG_FISCAL-DOCREF'
        value_org       = 'S'
      TABLES
        value_tab       = tl_docref
        return_tab      = tl_return_tab
        dynpfld_mapping = tl_dselc.
  ENDIF.
ENDMODULE.                 " MATCHCODE_DOCREF  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ITENS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_itens INPUT.
  DATA: wl_docref TYPE zfiwrt0008-seq_lcto,
        tl_marc   TYPE TABLE OF marc WITH HEADER LINE,
        tl_makt   TYPE TABLE OF makt WITH HEADER LINE,
        wl_header TYPE zfiwrt0008.

  wl_docref = wg_fiscal-docref.

  IF wg_fiscal-retorno EQ 'S'.
    REFRESH: tl_itens.

    CALL FUNCTION 'ZNFW_BUSCA_SALDO_RETORNO_NEW'
      EXPORTING
        i_bukrs    = p_bukrs
        i_branch   = p_branch
        i_parvw    = p_parvw
        i_parid    = p_parid
        i_seq_lcto = wl_docref
        i_operacao = p_operacao
        i_shipfrom = wg_shipfrom
        i_shipto   = wg_shipto
      IMPORTING
        e_header   = wl_header
      TABLES
        et_itens   = tl_itens
      EXCEPTIONS
        sem_saldo  = 1
        OTHERS     = 2.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'ENQUEUE_EZFIWRT0008'
        EXPORTING
*         MODE_ZFIWRT0008       = 'E'
*         MANDT          = SY-MANDT
          seq_lcto       = wl_docref
*         X_SEQ_LCTO     = ' '
*         _SCOPE         = '2'
*         _WAIT          = ' '
*         _COLLECT       = ' '
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

        IF p_parvw EQ c_br.
          MOVE: wl_header-nfenum TO wg_docs-nfenum,
                wl_header-series TO wg_docs-series,
                wl_header-bldat  TO wg_docs-bldat.
        ENDIF.

        IF tl_itens[] IS NOT INITIAL.
          SELECT *
            FROM marc
            INTO TABLE tl_marc
             FOR ALL ENTRIES IN tl_itens
             WHERE matnr EQ tl_itens-matnr
               AND werks EQ tl_itens-bwkey.

          SELECT *
            FROM makt
            INTO TABLE tl_makt
            FOR ALL ENTRIES IN tl_itens
             WHERE spras EQ sy-langu
               AND matnr EQ tl_itens-matnr.

        ENDIF.

        SORT: tl_marc BY matnr werks,
              tl_makt BY matnr.
        REFRESH: tg_itens.
        LOOP AT tl_itens.
          READ TABLE tl_marc
            WITH KEY matnr = tl_itens-matnr
                     werks = tl_itens-bwkey
                     BINARY SEARCH.

          READ TABLE tl_makt
            WITH KEY matnr = tl_itens-matnr
                     BINARY SEARCH.

          MOVE-CORRESPONDING: tl_itens TO tg_itens.
          MOVE: tl_itens-bwkey TO tg_itens-werks,
                tl_marc-steuc  TO tg_itens-steuc,
                tl_makt-maktx  TO tg_itens-maktx.

          tg_itens-fase = icon_display_more.
          APPEND tg_itens.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " GET_ITENS  INPUT
*&---------------------------------------------------------------------*
*&      Form  IMPOSTOS_COMPLEMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM impostos_complemento  USING p_row.
  DATA: wl_itens LIKE LINE OF tg_itens.

  SORT tg_impo_comp BY itmnum taxtyp.

  READ TABLE tg_itens INTO wl_itens INDEX p_row.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  IF w_zfiwrt0001-complement_icms = 'S'.
    LOOP AT tg_impo_gera.
      CLEAR tg_impo_comp.
      MOVE-CORRESPONDING: tg_impo_gera TO tg_impo_comp.
      READ TABLE tg_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                       taxtyp = tg_impo_gera-taxtyp. " BINARY SEARCH.
      IF sy-subrc NE 0.
        MOVE: wl_itens-itmnum  TO tg_impo_comp-itmnum.
        APPEND tg_impo_comp.
      ELSE.
        MODIFY tg_impo_comp INDEX sy-tabix.
      ENDIF.
      CLEAR: tg_impo_comp.
    ENDLOOP.
  ELSE.
*-CS2023000043-09.02.2023-#102019-JT-fim
    LOOP AT tg_impo.
      CLEAR tg_impo_comp.
      MOVE-CORRESPONDING: tg_impo TO tg_impo_comp.
      READ TABLE tg_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                       taxtyp = tg_impo-taxtyp. " BINARY SEARCH.
      IF sy-subrc NE 0.
        MOVE: wl_itens-itmnum  TO tg_impo_comp-itmnum.
        APPEND tg_impo_comp.
      ELSE.
        MODIFY tg_impo_comp INDEX sy-tabix.
      ENDIF.
      CLEAR: tg_impo_comp.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " IMPOSTOS_COMPLEMENTO
*&---------------------------------------------------------------------*
*&      Module  VALIDA_MENSAGEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_mensagem INPUT.
  DATA:  wl_tab TYPE sy-tabix.
  DATA: wl_0001 TYPE zfiwrt0001.
  DESCRIBE TABLE node_itab LINES wl_tab.
  " IF WL_TAB LT 2.
  REFRESH: node_itab.
  IF tree IS NOT INITIAL.
*    IF v_automatico_memo = abap_false. "BUG SOLTO 134309 / AOENNING / DUMP
    CALL METHOD tree->delete_all_nodes.
*    ENDIF.
  ENDIF.
  REFRESH: tg_mensagems_aux, tg_mensagems.
  IF p_operacao IS NOT INITIAL.
    SELECT SINGLE *
      FROM zfiwrt0001
      INTO wl_0001
       WHERE operacao EQ p_operacao.
    IF wl_0001-complemento = 'S'.
      CLEAR wg_docs-budat.
    ENDIF.
  ENDIF.
  "ENDIF.
ENDMODULE.                 " VALIDA_MENSAGEM  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_FORMA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_forma INPUT.
  DATA: tl_return_tab4 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc4      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_forma OCCURS 0,
          zlsch TYPE t042z-zlsch,
          text1 TYPE t042z-text1,
        END OF tl_forma.

  SELECT  zlsch text1
     FROM  t042z INTO TABLE tl_forma
    WHERE land1 = 'BR'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZLSCH'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'BSEG-ZLSCH'
      value_org       = 'S'
    TABLES
      value_tab       = tl_forma
      return_tab      = tl_return_tab4
      dynpfld_mapping = tl_dselc4.
ENDMODULE.                 " SEARCH_FORMA  INPUT

INCLUDE zwrr0002_0213.
*&---------------------------------------------------------------------*
*&      Form  HABILITAR_WORKFLOW_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM habilitar_workflow_documentos .
  REFRESH: it_seq_lcto.
  CLEAR: it_seq_lcto.

  "Somente validar acesso para modificar
  IF p_seq_lcto IS NOT INITIAL.
    it_seq_lcto-seq_lcto = p_seq_lcto .
    APPEND it_seq_lcto.
  ENDIF.



ENDFORM.

FORM f_define_origem_destino USING p_kna1  TYPE kna1
                                   p_lfa1  TYPE lfa1
                                   p_t001w TYPE t001w
                                   p_1baa  TYPE j_1baa
                          CHANGING p_indcoper
                                   p_texto_fiscal.

  IF p_parvw EQ c_ag.
    IF p_kna1-regio EQ p_t001w-regio.
      p_indcoper = c_d.
      p_texto_fiscal = 'Dentro do Estado'.
    ELSE.
      p_indcoper = c_f.
      p_texto_fiscal = 'Fora do Estado'.
    ENDIF.
    IF p_1baa-direct EQ 1.
      MOVE: p_kna1-regio TO wg_shipfrom.
    ELSE.
      MOVE: p_kna1-regio TO wg_shipto.
    ENDIF.
  ELSEIF p_parvw EQ c_br
     OR  p_parvw EQ c_lf.
    IF p_lfa1-regio EQ p_t001w-regio.
      p_indcoper = c_d.
      p_texto_fiscal = 'Dentro do Estado'.
    ELSE.
      p_indcoper = c_f.
      p_texto_fiscal = 'Fora do Estado'.
    ENDIF.

    IF p_1baa-direct EQ 1.
      MOVE: p_lfa1-regio TO wg_shipfrom.
    ELSE.
      MOVE: p_lfa1-regio TO wg_shipto.
    ENDIF.
  ENDIF.

  IF p_1baa-direct EQ 1.
    MOVE: p_t001w-regio TO wg_shipto.
  ELSE.
    MOVE: p_t001w-regio TO wg_shipfrom.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  INICIA_LANCTO  OUTPUT
*&---------------------------------------------------------------------*
FORM iniciar_lancamento_notas.

  DATA: v_bukrs_memo       TYPE t001-bukrs,
        v_branch_memo      TYPE zib_nfe_dist_ter-branch,
        v_numero_memo      TYPE zib_nfe_dist_ter-numero,
        v_serie_memo       TYPE zib_nfe_dist_ter-serie,
        v_dt_emissao_memo  TYPE zib_nfe_dist_ter-dt_emissao,
        v_lifnr_memo       TYPE lfa1-lifnr,
        v_operacao_memo    TYPE zfiwrt0001-operacao,
        v_chave_nfe_memo   TYPE zib_nfe_dist_ter-chave_nfe,
        v_lcto_memo        TYPE c LENGTH 10,
        v_agente           TYPE zib_nfe_dist_ter-f_transporte,
        v_coleta           TYPE zib_nfe_dist_ter-pc_partiner,
        v_entrega          TYPE zib_nfe_dist_ter-lr_partiner,
        v_loc_carrega      TYPE zfiwrt0008-loc_carrega, "CS2020001418
        lva_pedido_ov      TYPE c LENGTH 10,
        t_zib_nfe_dist_itm TYPE TABLE OF zib_nfe_dist_itm WITH HEADER LINE.

*-CS2021000595 - 22.06.2021 - JT - inicio
*----------------------------------------------------------
* busca TVARV NCM x MAterial
*----------------------------------------------------------
  FREE: t_ncm_mat, t_set, t_dtlcto.
  "133290 CS2024000037 Lib. tab. vinc. de NCM vs prod. SAP - PSA

*  IF sy-uname IS NOT INITIAL .
*    "Modelo anterior
**    BREAK-POINT.
*    SELECT *
*    FROM tvarvc
*    INTO TABLE t_tvarv
*   WHERE name = 'ZWRR0002_NCM_MAT'.
*
*    LOOP AT t_tvarv INTO w_tvarv.
*      CLEAR w_ncm_mat.
*
*      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*        EXPORTING
*          input        = w_tvarv-high
*        IMPORTING
*          output       = w_tvarv-high
*        EXCEPTIONS
*          length_error = 1
*          OTHERS       = 2.
*
*      w_ncm_mat-steuc    = w_tvarv-low.
*      w_ncm_mat-matnr    = w_tvarv-high.
*      APPEND w_ncm_mat  TO t_ncm_mat.
*    ENDLOOP.
*
*  ELSE.
  "Modelo Novo


  CLEAR: t_ZFIWRT0032.
  SELECT DISTINCT * FROM zfiwrt0032
    WHERE ltrim( operacao,'0' ) = ltrim( @p_operacao,'0' )
    INTO TABLE @t_ZFIWRT0032.

  LOOP AT t_ZFIWRT0032 ASSIGNING FIELD-SYMBOL(<fs_ZFIWRT0032>).
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = <fs_ZFIWRT0032>-material
      IMPORTING
        output       = <fs_ZFIWRT0032>-material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    w_ncm_mat-steuc    = <fs_ZFIWRT0032>-ncm.
    w_ncm_mat-matnr    = <fs_ZFIWRT0032>-material.
    APPEND w_ncm_mat  TO t_ncm_mat.
    CLEAR:w_ncm_mat.
  ENDLOOP.
*  ENDIF.


  "************************************************************
  "133290 CS2024000037 Lib. tab. vinc. de NCM vs prod. SAP - PSA
*  SELECT *
*    FROM tvarvc
*    INTO TABLE t_tvarv
*   WHERE name = 'ZWRR0002_NCM_MAT'.
*
*  LOOP AT t_tvarv INTO w_tvarv.
*    CLEAR w_ncm_mat.
*
*    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*      EXPORTING
*        input        = w_tvarv-high
*      IMPORTING
*        output       = w_tvarv-high
*      EXCEPTIONS
*        length_error = 1
*        OTHERS       = 2.
*
*    w_ncm_mat-steuc    = w_tvarv-low.
*    w_ncm_mat-matnr    = w_tvarv-high.
*    APPEND w_ncm_mat  TO t_ncm_mat.
*  ENDLOOP.
  "************************************************************
*  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*    EXPORTING
*      class           = '0000'
*      setnr           = 'ZNFW0009_DTLCTO_AUT'
*      no_descriptions = abap_false
*    TABLES
*      set_values      = t_set
*    EXCEPTIONS
*      set_not_found   = 1
*      OTHERS          = 2.

*  LOOP AT t_set   INTO w_set.
*    l_data = w_set-title+6(4) &&  w_set-title+3(2) &&  w_set-title(2).
*    w_dtlcto-uname   = w_set-from.
*    w_dtlcto-budat   = l_data.
*    APPEND w_dtlcto TO t_dtlcto.
*  ENDLOOP.

  SELECT SINGLE * FROM zfiwrt2005 INTO @DATA(ws_zfiwrt2005)
  WHERE usuario EQ @sy-uname
  AND operacao EQ @v_operacao_memo.
  IF sy-subrc EQ 0.
    w_dtlcto-uname   = ws_zfiwrt2005-usuario.
    w_dtlcto-budat   = ws_zfiwrt2005-dt_lanc.
    APPEND w_dtlcto TO t_dtlcto.
  ENDIF.
  CLEAR: ws_zfiwrt2005.

  SORT t_dtlcto BY uname.
*-CS2021000595 - 22.06.2021 - JT - inicio

*-CS2021001266 - 15.12.2021 - JT- inicio
  FREE: t_set,
        t_regula.

  "CS2022000878 / AOENNING.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class           = '0000'
      setnr           = 'SET_MAGGI_ZNFW_ICMS'
      no_descriptions = abap_false
    TABLES
      set_values      = t_set
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT t_set    INTO w_set.
    w_regula-operacao = w_set-from.
    w_regula-rate     = w_set-title.
    w_regula-shipto   = 'MT'. "w_set-title.
    APPEND w_regula  TO t_regula.
  ENDLOOP.

  SORT t_regula BY operacao.
  DELETE ADJACENT DUPLICATES FROM t_regula
                        COMPARING operacao.
*-CS2021001266 - 15.12.2021 - JT- fim

  IF sy-calld EQ 'X' AND viniciou_lcto_znfw0009 NE 'X'.

    viniciou_lcto_znfw0009 = 'X'.

    IMPORT v_bukrs_memo      FROM MEMORY ID 'EMPRESA'.
    IMPORT v_branch_memo     FROM MEMORY ID 'FILIAL'.
    IMPORT v_numero_memo     FROM MEMORY ID 'NFE'.
    IMPORT v_serie_memo      FROM MEMORY ID 'SERIE'.
    IMPORT v_dt_emissao_memo FROM MEMORY ID 'DT_EMISSAO'.
    IMPORT v_lifnr_memo      FROM MEMORY ID 'IDPARCEIRO'.
    IMPORT v_operacao_memo   FROM MEMORY ID 'OPERACAO'.
    IMPORT v_chave_nfe_memo  FROM MEMORY ID 'CHAVE'.
    IMPORT v_lcto_memo       FROM MEMORY ID 'LANCAMENTO'.
    IMPORT v_agente          FROM MEMORY ID 'AGENTE'.
    IMPORT v_coleta          FROM MEMORY ID 'COLETA'.
    IMPORT v_entrega         FROM MEMORY ID 'ENTREGA'.
    IMPORT v_loc_carrega     FROM MEMORY ID 'LOC_CARREGA'. "CS2020001418 - Inicio - CSB

    DELETE FROM MEMORY ID 'EMPRESA'.
    DELETE FROM MEMORY ID 'FILIAL'.
    DELETE FROM MEMORY ID 'NFE'.
    DELETE FROM MEMORY ID 'SERIE'.
    DELETE FROM MEMORY ID 'DT_EMISSAO'.
    DELETE FROM MEMORY ID 'IDPARCEIRO'.
    DELETE FROM MEMORY ID 'OPERACAO'.
    DELETE FROM MEMORY ID 'CHAVE'.
    DELETE FROM MEMORY ID 'LANCAMENTO'.
    DELETE FROM MEMORY ID 'AGENTE'.
    DELETE FROM MEMORY ID 'COLETA'.
    DELETE FROM MEMORY ID 'ENTREGA'.
    DELETE FROM MEMORY ID 'LOC_CARREGA'.


    vlancamento_znfw0009 = v_lcto_memo.

    IF  v_bukrs_memo      IS NOT INITIAL   AND  v_branch_memo        IS NOT INITIAL  AND
        v_numero_memo     IS NOT INITIAL   AND  v_serie_memo         IS NOT INITIAL  AND
        v_dt_emissao_memo IS NOT INITIAL   AND  v_operacao_memo      IS NOT INITIAL  AND
        v_chave_nfe_memo  IS NOT INITIAL   AND  vlancamento_znfw0009 IS NOT INITIAL.

      PERFORM z_novo_lan.

      p_bukrs              = v_bukrs_memo.
      p_branch             = v_branch_memo.
      p_parid              = v_lifnr_memo.
      p_operacao           = v_operacao_memo.
      wg_docs-nfenum       = v_numero_memo.
      wg_docs-series       = v_serie_memo.
      wg_docs-budat        = sy-datum.
      wg_docs-loc_carrega  = v_loc_carrega.

      "CS2022000878 / AOENNING.
      CLEAR: ws_zfiwrt2005.
      SELECT SINGLE * FROM zfiwrt2005 INTO ws_zfiwrt2005
      WHERE usuario EQ sy-uname
      AND operacao EQ v_operacao_memo.
      IF sy-subrc EQ 0.
        wg_docs-budat = ws_zfiwrt2005-dt_lanc .
      ELSE.
        wg_docs-budat = sy-datum.
      ENDIF.
      CLEAR: ws_zfiwrt2005.

      IF vlancamento_znfw0009 = 'MIC'.
        wg_docs-budat     = v_dt_emissao_memo.
      ELSE.
*-CS2021000595 - 22.06.2021 - JT - inicio
        READ TABLE t_dtlcto INTO w_dtlcto WITH KEY uname = sy-uname.
        IF sy-subrc = 0.
          wg_docs-budat = w_dtlcto-budat.
        ENDIF.
      ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim

      wg_fiscal-inco1      = 'CIF'.
      wg_fiscal-inco2      = 'CIF'.
      wg_docs-bldat        = v_dt_emissao_memo.
      wg_fiscal-access_key = v_chave_nfe_memo.

      CLEAR: t_zib_nfe_dist_itm[].
      SELECT *
        FROM zib_nfe_dist_itm INTO TABLE t_zib_nfe_dist_itm
       WHERE chave_nfe        EQ v_chave_nfe_memo
         AND prod_pedido_comp NE space.

      SORT t_zib_nfe_dist_itm BY prod_pedido_comp.
      DELETE ADJACENT DUPLICATES FROM t_zib_nfe_dist_itm COMPARING prod_pedido_comp.

      LOOP AT t_zib_nfe_dist_itm.
        lva_pedido_ov = t_zib_nfe_dist_itm-prod_pedido_comp.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lva_pedido_ov
          IMPORTING
            output = lva_pedido_ov.

        SELECT SINGLE *
          FROM ekko INTO @DATA(lwa_ekko)
         WHERE ebeln EQ @lva_pedido_ov.

        IF sy-subrc EQ 0.
          wg_fiscal-ebeln = lva_pedido_ov.
        ELSE.
          SELECT SINGLE *
            FROM vbak INTO @DATA(lwa_vbak)
           WHERE vbeln EQ @lva_pedido_ov.

          IF sy-subrc EQ 0.
            wg_fiscal-vbeln = lva_pedido_ov.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF v_agente IS NOT INITIAL.
        tg_parc-parvw = 'SP'.
        tg_parc-parid = v_agente.
        tg_parc-nome = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = v_agente ) )->at_lfa1-name1.
        wa_style-fieldname = 'PARVW'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT wa_style INTO TABLE style .
        wa_style-fieldname = 'PARID'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT wa_style INTO TABLE style .
        INSERT LINES OF style INTO TABLE tg_parc-style.
        APPEND tg_parc.
        REFRESH: style, tg_parc-style.
      ENDIF.

      IF v_coleta  IS NOT INITIAL.
        tg_parc-parvw = 'PC'.
        tg_parc-parid = v_coleta.
        tg_parc-nome = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = v_coleta ) )->at_lfa1-name1.
        wa_style-fieldname = 'PARVW'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT wa_style INTO TABLE style .
        wa_style-fieldname = 'PARID'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT wa_style INTO TABLE style .
        INSERT LINES OF style INTO TABLE tg_parc-style.
        APPEND tg_parc.
        REFRESH: style, tg_parc-style.
      ENDIF.

      IF v_entrega IS NOT INITIAL.
        tg_parc-parvw = 'LR'.
        tg_parc-parid = v_entrega.
        tg_parc-nome  = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = v_entrega ) )->at_kna1-name1.
        wa_style-fieldname = 'PARVW'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT wa_style INTO TABLE style .
        wa_style-fieldname = 'PARID'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT wa_style INTO TABLE style .
        INSERT LINES OF style INTO TABLE tg_parc-style.
        APPEND tg_parc.
        REFRESH: style, tg_parc-style.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  INICIA_LANCTO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE inicia_lancto OUTPUT.

*-CS2021000595 - 22.06.2021 - JT - inicio
  PERFORM iniciar_lancamento_notas.
*-CS2021000595 - 22.06.2021 - JT - fim

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  Z_NOVO_LAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_novo_lan .

  IF wg_flag IS INITIAL.

    wg_sugere_transp = abap_true.

    PERFORM limpa_campos.
    CLEAR: p_operacao, p_seq_lcto, p_parvw, p_parid, p_branch, p_bukrs, wg_desc_parid,
           wg_desc_operacao, wg_desc_parvw, wg_desc_branch, wg_desc_bukrs, p_loekz.
    PERFORM get_next_number IN PROGRAM zwrr0001 USING  'ZSEQ_LCTO'
                                                    '1'
                                           CHANGING p_seq_lcto.

  ENDIF.
  PERFORM trata_campos USING space
                              'GR1'
                                 c_1       "INPUT 1     NO INPUT 0
                                 c_0.      "INVISIBLE 1 VISIBLE 0
  PERFORM trata_campos USING space
                            'GR2'
                             c_0       "INPUT 1     NO INPUT 0
                             c_0.      "INVISIBLE 1 VISIBLE 0
*      wg_docs-bldat = sy-datum.
  wg_docs-budat = sy-datum.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_sugere_itens.

  DATA: lwa_item        LIKE LINE OF tg_itens,
        lva_ncm         TYPE marc-steuc,
        lva_fator_conv  TYPE i,
        lva_count_itens TYPE i,
        lva_matnr_xml   TYPE mara-matnr,
        lva_matnr_18    TYPE matnr18,
        lit_matnr_ov_pd TYPE TABLE OF ty_matnr_ov_pd,
        vg_sem(1).

  IF tg_itens[] IS INITIAL.
    CLEAR: vsugere_itens_znfw0009.
  ENDIF.
  CLEAR vg_sem.

  CHECK ( viniciou_lcto_znfw0009 = 'X'  AND vsugere_itens_znfw0009 IS INITIAL ) AND ( vlancamento_znfw0009 = 'MIC' OR vlancamento_znfw0009 = 'ONF' ).

  vsugere_itens_znfw0009 = 'X'.

  CLEAR: vlr_total_nota, lva_count_itens, lit_matnr_ov_pd[], lit_zib_nfe_dist_itm[].

  PERFORM f_get_matnr_ov_pd TABLES lit_matnr_ov_pd.

  SELECT  *
    FROM zib_nfe_dist_itm INTO TABLE lit_zib_nfe_dist_itm
   WHERE chave_nfe EQ wg_fiscal-access_key.

  LOOP AT lit_zib_nfe_dist_itm INTO DATA(lwa_zib_nfe_dist_itm).

    CLEAR: lwa_item.

    lva_fator_conv = 1.

    lva_matnr_xml = lwa_zib_nfe_dist_itm-prod_codigo.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lva_matnr_xml
      IMPORTING
        output = lva_matnr_18.
    lva_matnr_xml = lva_matnr_18.

    PERFORM f_format_ncm USING lwa_zib_nfe_dist_itm-prod_ncm CHANGING lva_ncm.

    lwa_item-itmnum = ( lva_count_itens + 1 ) * 10.

    "Identificação Material Pedido/Ordem Venda
    CASE vlancamento_znfw0009.
      WHEN 'MIC'.
        READ TABLE lit_matnr_ov_pd INTO DATA(lwa_matnr_ov_pd) WITH KEY matnr = lva_matnr_xml "Notas da MIC, os codigos do materiais do pedido/ordem venda são iguais ao do XML
                                                                       steuc = lva_ncm.

        IF sy-subrc NE 0. "Só adicionar o item se encontrar a correspondencia exata(ncm e matnr) no pedido/ov.
          PERFORM f_check_regras_znfw0009. "Ajustes feito para bypass
          IF sy-subrc EQ 0.
            CONTINUE.
          ENDIF.
        ENDIF.
      WHEN 'ONF'.

        DATA(lva_count_ncm_pd_ov) = 0.
        DATA(lva_count_ncm_pd) = 0.
        LOOP AT lit_matnr_ov_pd INTO lwa_matnr_ov_pd WHERE steuc = lva_ncm.
          ADD 1 TO lva_count_ncm_pd_ov.
        ENDLOOP.
        LOOP AT lit_matnr_ov_pd INTO lwa_matnr_ov_pd.
          ADD 1 TO lva_count_ncm_pd.
        ENDLOOP.

        IF lva_count_ncm_pd_ov EQ 1. "Se encontrou somente um item no pedido/ov com o ncm selecionado, utilizar o item em questão..
          sy-subrc = 0.
        ELSE.

          READ TABLE t_ncm_mat INTO w_ncm_mat WITH KEY steuc = lva_ncm.

          IF sy-subrc = 0.
*---------- Descricao material
            SELECT maktx
              INTO @DATA(l_maktx)
              FROM makt
                UP TO 1 ROWS
             WHERE matnr = @w_ncm_mat-matnr
               AND spras = @sy-langu.
            ENDSELECT.

            "133290 CS2024000037 Lib. tab. vinc. de NCM vs prod. SAP - PSA
            LOOP AT t_zfiwrt0032 ASSIGNING FIELD-SYMBOL(<get_zfiwrt0032>) WHERE material = w_ncm_mat-matnr AND ncm = w_ncm_mat-steuc AND operacao = p_operacao.

              IF <get_zfiwrt0032>-lote_aut IS NOT INITIAL.
                lwa_item-charg = sy-datum+0(4).
              ELSE.
                lwa_item-charg = <get_zfiwrt0032>-lote.
              ENDIF.

              lwa_item-matnr = <get_zfiwrt0032>-material.
              lwa_item-meins = 'KG'.
              lwa_item-steuc = <get_zfiwrt0032>-ncm.
              lwa_item-maktx = l_maktx.
              lwa_item-werks = p_branch.
              lwa_item-lgort = <get_zfiwrt0032>-deposito.
            ENDLOOP.



            "Bloquear Alterações Campos
*            APPEND VALUE #( fieldname = 'MATNR' style = cl_gui_alv_grid=>mc_style_disabled ) TO lwa_item-style2.
*            APPEND VALUE #( fieldname = 'WERKS' style = cl_gui_alv_grid=>mc_style_disabled ) TO lwa_item-style2.

            sy-subrc = 2.
            IF lva_count_ncm_pd = 0 AND wg_fiscal-ebeln IS NOT INITIAL.
              vg_sem = 'X'.
            ENDIF.
          ELSE.
            sy-subrc = 4.
          ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim
        ENDIF.

      WHEN OTHERS.
        sy-subrc = 4.
    ENDCASE.

    IF sy-subrc = 0.
      lwa_item-matnr = lwa_matnr_ov_pd-matnr.
      lwa_item-meins = lwa_matnr_ov_pd-meins.
      lwa_item-steuc = lwa_matnr_ov_pd-steuc.
      lwa_item-maktx = lwa_matnr_ov_pd-maktx.
      lwa_item-werks = lwa_matnr_ov_pd-werks.
      lwa_item-lgort = lwa_matnr_ov_pd-lgort.
      lwa_item-charg = lwa_matnr_ov_pd-charg.

      lwa_item-vbeln = lwa_matnr_ov_pd-vbeln.
      lwa_item-posnr = lwa_matnr_ov_pd-posnr.

*      "Bloquear Alterações Campos
*      APPEND VALUE #( fieldname = 'MATNR' style = cl_gui_alv_grid=>mc_style_disabled ) TO lwa_item-style2.
*      APPEND VALUE #( fieldname = 'WERKS' style = cl_gui_alv_grid=>mc_style_disabled ) TO lwa_item-style2.
    ENDIF.

    IF ( lwa_zib_nfe_dist_itm-prod_und_comerci = 'TO'  ) OR
       ( lwa_zib_nfe_dist_itm-prod_und_comerci = 'TON' ) OR
       ( lwa_zib_nfe_dist_itm-prod_und_comerci = 'TL'  ) OR
       ( lwa_zib_nfe_dist_itm-prod_und_comerci = 'TN'  ).
*
*      SELECT SINGLE *
*        FROM marc INTO @DATA(lwa_marc)
*       WHERE steuc EQ @lva_ncm
*         AND werks EQ @p_branch.


      SELECT SINGLE *
        FROM marc INTO @DATA(lwa_marc)
       WHERE matnr EQ @lwa_item-matnr
         AND werks EQ @p_branch.

      IF sy-subrc EQ 0.
        SELECT SINGLE *
          FROM mara INTO @DATA(lwa_mara)
         WHERE matnr EQ @lwa_marc-matnr.

        IF sy-subrc EQ 0 AND ( lwa_mara-meins EQ 'KG' ).
          lva_fator_conv = 1000.
        ENDIF.
      ENDIF.
    ENDIF.

    lwa_item-cfop           = wg_direitos-cfop.
    lwa_item-menge          = lwa_zib_nfe_dist_itm-prod_qtd_comerci * lva_fator_conv.
    lwa_item-netpr          = lwa_zib_nfe_dist_itm-prod_vlr_und_com / lva_fator_conv.
*    lwa_item-netwr          = lwa_zib_nfe_dist_itm-prod_vlr_total_b.

*&------------------------------------------------------------------ajuste bug / aoenning
    lwa_item-netdis         = lwa_zib_nfe_dist_itm-prod_vl_desconto.
    lwa_item-netoth         = lwa_zib_nfe_dist_itm-prod_vl_outro.
    lwa_item-netfre         = lwa_zib_nfe_dist_itm-prod_vl_frete.
    lwa_item-netins         = lwa_zib_nfe_dist_itm-prod_vl_seguro.
*&------------------------------------------------------------------Ajuste BUG / AOENNING

    lwa_item-netwr          = lwa_zib_nfe_dist_itm-prod_vlr_total_b - lwa_item-netdis + lwa_item-netfre + lwa_item-netins + lwa_item-netoth.
    lwa_item-vlr_iten_xml   = lwa_zib_nfe_dist_itm-prod_vlr_total_b.
    lwa_item-ncm_xml        = lwa_zib_nfe_dist_itm-prod_ncm.
    lwa_item-vlr_icms_xml   = lwa_zib_nfe_dist_itm-icms_valor.

    IF lwa_item-matnr IS NOT INITIAL AND vg_sem IS INITIAL. "ALRS
      APPEND lwa_item TO tg_itens.

      ADD 1 TO lva_count_itens.

*      ADD lwa_zib_nfe_dist_itm-prod_vlr_total_b TO vlr_total_nota.
*&------------------------------------------------------------------ajuste bug / aoenning
      ADD lwa_item-netwr TO vlr_total_nota.
*&------------------------------------------------------------------Ajuste BUG / AOENNING
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM f_format_ncm USING p_ncm TYPE zib_nfe_dist_itm-prod_ncm
               CHANGING c_ncm TYPE marc-steuc.

  CLEAR: c_ncm.

  CHECK p_ncm IS NOT INITIAL.

  c_ncm = p_ncm.
  REPLACE ALL OCCURRENCES OF '.' IN c_ncm WITH ' '.
  CONDENSE c_ncm NO-GAPS.
  CONCATENATE c_ncm+0(4) '.' c_ncm+4(2) '.' c_ncm+6(2) INTO c_ncm.


ENDFORM.

FORM f_get_matnr_ov_pd TABLES t_matnr_ov_pd STRUCTURE git_matnr_ov_pd.

  DATA: lit_marc  TYPE TABLE OF marc,
        lva_charg TYPE eket-charg.

  CLEAR: t_matnr_ov_pd[].

  IF wg_fiscal-ebeln IS NOT INITIAL.

    SELECT *
      FROM ekpo INTO TABLE @DATA(it_ekpo_tmp)
     WHERE ebeln EQ @wg_fiscal-ebeln
       AND loekz EQ @abap_false.

    LOOP AT it_ekpo_tmp INTO DATA(lwa_ekpo_tmp).

      CLEAR: lva_charg.

      SELECT SINGLE  *
        FROM eket INTO @DATA(lwa_eket)
       WHERE ebeln EQ @lwa_ekpo_tmp-ebeln.

      IF sy-subrc EQ 0.
        lva_charg = lwa_eket-charg.
      ENDIF.

      APPEND VALUE #( ebeln = lwa_ekpo_tmp-ebeln
                      matnr = lwa_ekpo_tmp-matnr
                      meins = lwa_ekpo_tmp-meins
                      werks = lwa_ekpo_tmp-werks
                      lgort = lwa_ekpo_tmp-lgort
                      steuc = lwa_ekpo_tmp-j_1bnbm
                      charg = lva_charg ) TO t_matnr_ov_pd[].
    ENDLOOP.

  ELSEIF wg_fiscal-vbeln IS NOT INITIAL.

    SELECT *
      FROM vbap INTO TABLE @DATA(it_vbap_tmp)
     WHERE vbeln EQ @wg_fiscal-vbeln.

    LOOP AT it_vbap_tmp INTO DATA(lwa_vbap_tmp).
      APPEND VALUE #( vbeln = lwa_vbap_tmp-vbeln
                      posnr = lwa_vbap_tmp-posnr
                      matnr = lwa_vbap_tmp-matnr
                      meins = lwa_vbap_tmp-meins
                      werks = lwa_vbap_tmp-werks
                      lgort = lwa_vbap_tmp-lgort
                      charg = lwa_vbap_tmp-charg ) TO t_matnr_ov_pd[].
    ENDLOOP.

  ENDIF.

  CHECK t_matnr_ov_pd[] IS NOT INITIAL.

  LOOP AT t_matnr_ov_pd ASSIGNING FIELD-SYMBOL(<fs_matnr_ov_pd>).

    SELECT SINGLE *
      FROM makt INTO @DATA(wl_makt)
     WHERE matnr EQ @<fs_matnr_ov_pd>-matnr
       AND spras EQ @sy-langu.

    IF sy-subrc EQ 0.
      <fs_matnr_ov_pd>-maktx = wl_makt-maktx.
    ENDIF.

    SELECT SINGLE *
      FROM marc INTO @DATA(lwa_marc)
     WHERE matnr EQ @<fs_matnr_ov_pd>-matnr
       AND werks EQ @p_branch.

    IF sy-subrc EQ 0 .
      IF <fs_matnr_ov_pd>-steuc IS INITIAL.
        <fs_matnr_ov_pd>-steuc = lwa_marc-steuc.
      ENDIF.
    ENDIF.

  ENDLOOP.

  DELETE t_matnr_ov_pd WHERE NOT ( maktx IS NOT INITIAL AND steuc IS NOT INITIAL ).

ENDFORM.

*FORM z_sugere_itens.
*
*  DATA: tl_itens_aux LIKE TABLE OF tg_itens,
*        wl_itens     LIKE LINE OF tg_itens,
*        wl_lines     TYPE sy-tabix,
*        v_ncm        TYPE marc-steuc,
*        l_lines.
*
*  IF tg_itens[] IS INITIAL.
*    CLEAR: vsugere_itens_znfw0009.
*  ENDIF.
*
*  IF ( viniciou_lcto_znfw0009 = 'X' AND vsugere_itens_znfw0009 IS INITIAL ) AND
*     ( vlancamento_znfw0009 = 'MIC'  AND wg_fiscal-ebeln IS NOT INITIAL OR
*       vlancamento_znfw0009 = 'ONF'  AND wg_fiscal-ebeln IS NOT INITIAL ).
*
*    CLEAR: vlr_total_nota.
*
*    vsugere_itens_znfw0009 = 'X'.
*
*    SELECT *
*       FROM ekpo INTO TABLE @DATA(it_ekpo)
*      WHERE ebeln EQ @wg_fiscal-ebeln.
*
*    SELECT  *
*      FROM zib_nfe_dist_itm INTO TABLE @DATA(it_zib_nfe_dist_itm)
*     WHERE chave_nfe EQ @wg_fiscal-access_key.
*
*    REFRESH: tl_itens_aux.
*
*    DESCRIBE TABLE it_ekpo LINES l_lines.
*
*    IF l_lines > 1.
*
*      LOOP AT  it_zib_nfe_dist_itm INTO DATA(wa_zib_nfe_dist_itm).
*        REFRESH: style2, wl_itens-style2.
*        tl_itens_aux[] = tg_itens[].
*        REFRESH: tg_itens.
*        LOOP AT tl_itens_aux INTO wl_itens.
*          wl_itens-itmnum = sy-tabix * 10.
*          APPEND wl_itens TO tg_itens.
*        ENDLOOP.
*        DESCRIBE TABLE tg_itens LINES wl_lines.
*        CLEAR: wl_itens.
*        wl_itens-itmnum = ( wl_lines + 1 ) * 10 .
*        "
*        v_ncm = wa_zib_nfe_dist_itm-prod_ncm.
*        REPLACE ALL OCCURRENCES OF '.' IN v_ncm WITH ' '.
*        CONDENSE v_ncm NO-GAPS.
*        CONCATENATE v_ncm+0(4) '.' v_ncm+4(2) '.' v_ncm+6(2) INTO v_ncm.
*        SELECT SINGLE matnr
*          FROM marc
*          INTO @DATA(_matnr)
*          WHERE steuc = @v_ncm.
*        "
*        IF sy-subrc = 0.
*          READ TABLE  it_ekpo INTO DATA(wa_ekpo_) WITH KEY matnr = _matnr.
*          IF sy-subrc = 0.
*            wl_itens-matnr = wa_ekpo_-matnr.
*            wl_itens-meins = wa_ekpo_-meins.
*            SELECT SINGLE steuc
*            FROM marc
*            INTO wl_itens-steuc
*             WHERE matnr EQ wl_itens-matnr
*               AND werks EQ p_branch.
*
*            SELECT SINGLE *
*              FROM makt INTO @DATA(wl_makt)
*             WHERE spras EQ @sy-langu
*               AND matnr EQ @wa_ekpo_-matnr.
*
*            IF sy-subrc = 0.
*              wl_itens-maktx = wl_makt-maktx.
*            ENDIF.
*
*            wl_itens-werks = wa_ekpo_-werks.
*            wl_itens-lgort = wa_ekpo_-lgort.
*
*            SELECT SINGLE  *
*              FROM eket INTO @DATA(wa_eket_)
*             WHERE ebeln EQ @wa_ekpo_-ebeln.
*
*            IF sy-subrc EQ 0.
*              wl_itens-charg = wa_eket_-charg.
*            ENDIF.
*            wa_style-fieldname = 'WERKS'.
*            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*            INSERT  wa_style INTO TABLE style2.
*            "
*            wa_style-fieldname = 'MATNR'.
*            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*            INSERT  wa_style INTO TABLE style2.
*          ENDIF.
*        ENDIF.
*        "
*
*        wl_itens-cfop = wg_direitos-cfop.
*        "
*        DATA(_fator) = 1000.
*        _fator = 1.
**        IF V_LCTO_MEMO = 'MIC'.
*        IF wa_zib_nfe_dist_itm-prod_und_comerci = 'TO' AND wa_ekpo_-meins = 'KG'.
*          _fator = 1000.
**          ENDIF.
*        ENDIF.
*        vlr_total_nota          = vlr_total_nota +  wa_zib_nfe_dist_itm-prod_vlr_total_b.
*        wl_itens-menge          = wa_zib_nfe_dist_itm-prod_qtd_comerci * _fator.
*        wl_itens-netpr          = wa_zib_nfe_dist_itm-prod_vlr_und_com / _fator.
*        wl_itens-netwr          = wa_zib_nfe_dist_itm-prod_vlr_total_b.
*        wl_itens-vlr_iten_xml   = wa_zib_nfe_dist_itm-prod_vlr_total_b.
*
*        INSERT LINES OF style2 INTO TABLE wl_itens-style2.
*        APPEND wl_itens TO tg_itens[].
*        CLEAR wl_itens.
*      ENDLOOP.
*
*    ELSEIF l_lines = 1.
*
*      LOOP AT  it_zib_nfe_dist_itm INTO wa_zib_nfe_dist_itm.
*        REFRESH: style2, wl_itens-style2.
*        READ TABLE  it_ekpo INTO DATA(wa_ekpo) INDEX 1.
*
*        tl_itens_aux[] = tg_itens[].
*        REFRESH: tg_itens.
*        LOOP AT tl_itens_aux INTO wl_itens.
*          wl_itens-itmnum = sy-tabix * 10.
*          APPEND wl_itens TO tg_itens.
*        ENDLOOP.
*        DESCRIBE TABLE tg_itens LINES wl_lines.
*        CLEAR: wl_itens.
*        wl_itens-itmnum = ( wl_lines + 1 ) * 10 .
*
*        wl_itens-matnr = wa_ekpo-matnr.
*        wl_itens-meins = wa_ekpo-meins.
*        SELECT SINGLE steuc
*          FROM marc
*          INTO wl_itens-steuc
*           WHERE matnr EQ wl_itens-matnr
*             AND werks EQ p_branch.
*
*        SELECT SINGLE *
*          FROM makt INTO @DATA(wl_makt_)
*         WHERE spras EQ @sy-langu
*           AND matnr EQ @wa_ekpo-matnr.
*
*        IF sy-subrc = 0.
*          wl_itens-maktx = wl_makt_-maktx.
*        ENDIF.
*
*        wl_itens-werks = wa_ekpo-werks.
*        wl_itens-lgort = wa_ekpo-lgort.
*
*        SELECT SINGLE  *
*          FROM eket INTO @DATA(wa_eket)
*         WHERE ebeln EQ @wa_ekpo-ebeln.
*
*        IF sy-subrc EQ 0.
*          wl_itens-charg = wa_eket-charg.
*        ENDIF.
*
*        wa_style-fieldname = 'WERKS'.
*        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*        INSERT  wa_style INTO TABLE style2.
*        "
*        wa_style-fieldname = 'MATNR'.
*        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*        INSERT  wa_style INTO TABLE style2.
*        "
*        wl_itens-cfop = wg_direitos-cfop.
*        "
*        DATA(_fator2) = 1000.
*        _fator2 = 1.
**        IF V_LCTO_MEMO = 'MIC'.
*        IF wa_zib_nfe_dist_itm-prod_und_comerci = 'TO' AND wa_ekpo-meins = 'KG'.
*          _fator2 = 1000.
*        ENDIF.
**        ENDIF.
*        wl_itens-menge          = wa_zib_nfe_dist_itm-prod_qtd_comerci * _fator2.
*        wl_itens-netpr          = wa_zib_nfe_dist_itm-prod_vlr_und_com / _fator2.
*        wl_itens-netwr          = wa_zib_nfe_dist_itm-prod_vlr_total_b.
*        wl_itens-vlr_iten_xml   = wa_zib_nfe_dist_itm-prod_vlr_total_b.
*        vlr_total_nota          = vlr_total_nota +  wa_zib_nfe_dist_itm-prod_vlr_total_b.
*
*        INSERT LINES OF style2 INTO TABLE wl_itens-style2.
*        APPEND wl_itens TO tg_itens[].
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ENTER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE ENTER INPUT.
**      refresh: tg_parc.
*  PERFORM BUSCA_DADOS.
**      PERFORM busca_dados_doc.
*  PERFORM BUSCA_DESCRICOES.
*ENDMODULE.

FORM f_check_regras_znfw0009.

  CONCATENATE p_bukrs p_operacao INTO DATA(lva_bukrs_opr_not_check).

  SELECT SINGLE *
    FROM setleaf INTO @DATA(lwa_setleaf)
   WHERE setname EQ 'ZNFW0002_REGRAS_ZNFW0009'
     AND valfrom EQ @lva_bukrs_opr_not_check.

  IF sy-subrc EQ 0. "Se parametrizado
    sy-subrc = 4. "Não deve checar regras da ZNFW0009
  ELSE.
    sy-subrc = 0. "Deve checar regras da ZNFW0009
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TEXTO_SD
*&---------------------------------------------------------------------*
FORM f_texto_sd  USING  p_texto_sd.

  DATA: lva_name TYPE thead-tdname.

  DATA: lt_lines_aux TYPE TABLE OF tline,
        lt_lines     TYPE TABLE OF tline.

  DATA: lwa_lines TYPE tline.

  IF p_texto_sd IS NOT INITIAL.
    IF p_bukrs IS NOT INITIAL.
      SELECT SINGLE vkorg
        FROM tvko
        INTO @DATA(lva_vkorg)
        WHERE bukrs = @p_bukrs.

      LOOP AT tg_itens INTO DATA(lwa_itens).
        PERFORM f_input_matnr CHANGING lwa_itens-matnr.

        CONCATENATE lwa_itens-matnr
                    lva_vkorg
                    '10'
                    INTO lva_name.

        REFRESH lt_lines_aux.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            client                  = sy-mandt
            id                      = '0001'
            language                = sy-langu
            name                    = lva_name
            object                  = 'MVKE'
          TABLES
            lines                   = lt_lines_aux
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF lt_lines_aux[] IS NOT INITIAL.

          DESCRIBE TABLE tg_mensagems LINES DATA(lva_lines).
          DATA(lit_mensagens) = tg_mensagems[].
          SORT: lit_mensagens BY seqnum DESCENDING.
          READ TABLE lit_mensagens INTO DATA(lwa_msg) INDEX 1.
          DATA(lva_seqnum) = lwa_msg-seqnum.

          LOOP AT lt_lines_aux INTO lwa_lines.
            lva_lines = lva_lines + 1.
            lva_seqnum = lva_seqnum + 1.
            tg_mensagems-linnum = lva_lines.
            tg_mensagems-seqnum = lva_seqnum.
            tg_mensagems-message = lwa_lines-tdline.
            APPEND tg_mensagems.
          ENDLOOP.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INPUT_OUTPUT_MATNR
*&---------------------------------------------------------------------*
FORM f_input_matnr  CHANGING p_matnr TYPE matnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = p_matnr
    IMPORTING
      output       = p_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_COR_IMPOSTO
*&---------------------------------------------------------------------*
FORM f_monta_cor_imposto .
  IF vl_prazo = 'D'.
    LOOP AT tg_impo_aux ASSIGNING FIELD-SYMBOL(<lfs_impo>) WHERE taxtyp = 'ICM3'.
      <lfs_impo>-color = 'C310'.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM f_get_data_lim. "PSA

  xvals-tabname   = 'ZSTRUCT_GET_DATA_LIM'.
  xvals-fieldname = 'ID'.
  APPEND xvals TO ivals.



  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'LIM'
    TABLES
      fields          = ivals
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  CONDENSE xvals-value NO-GAPS.

  READ TABLE ivals INTO xvals WITH KEY fieldname = 'ID'.
  IF sy-subrc  = 0.
    wa_param  =  VALUE #(  id = xvals-value endpoint = '' method = 'POST'  ).

    TRY.
        zcl_int_ob_get_data_lim=>zif_integracao_outbound~get_instance(
        )->execute_request(
          EXPORTING
            i_info_request           = wa_param
          IMPORTING
            e_id_integracao          = DATA(resul_id)    " Id. de Integração
            e_integracao             = DATA(result_json)    " Tabela de Integração
        ).

        IF result_json IS NOT INITIAL.

          CALL METHOD /ui2/cl_json=>deserialize
            EXPORTING
              json = result_json-ds_data_retorno
            CHANGING
              data = lr_content.

          IF lr_content-content  IS NOT INITIAL.
**********************************************************************
*POPULA CAMPOS ZNFW0002
**********************************************************************

            PERFORM popula_itens_api.

*            REFRESH: tg_parc.
            PERFORM busca_dados.
*      PERFORM busca_dados_doc.
            PERFORM busca_descricoes.

**********************************************************************
*POPULA MENSSAGEM
**********************************************************************
            APPEND VALUE #( seqnum = 1 linnum = 1 message = wa_param-id ) TO  tg_mensagems_lim[].


          ENDIF.
        ENDIF.

      CATCH zcx_integracao INTO DATA(zcx_integracao).
        MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
         NUMBER zcx_integracao->zif_error~msgno
           WITH zcx_integracao->zif_error~msgv1
                zcx_integracao->zif_error~msgv2
                zcx_integracao->zif_error~msgv3
                zcx_integracao->zif_error~msgv4.


      CATCH zcx_error INTO DATA(zcx_error).
*        MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
*         NUMBER zcx_error->zif_error~msgno
*           WITH zcx_error->zif_error~msgv1
*                zcx_error->zif_error~msgv2
*                zcx_error->zif_error~msgv3
*                zcx_error->zif_error~msgv4.



        MESSAGE e024(sd) WITH 'A LIM não foi encontrada!'.

    ENDTRY.


  ENDIF.


ENDFORM.


FORM popula_itens_api.

  CLEAR: p_operacao,p_bukrs,p_branch,p_parid.

  p_operacao = lr_content-content-operacao.
  p_bukrs = lr_content-content-id_empresa_sap.
  p_branch = lr_content-content-id_filial_sap.
  p_parid = lr_content-content-coddocliente.


  DATA: tl_itens_aux LIKE TABLE OF tg_itens,
        wl_itens     LIKE LINE OF tg_itens,
        wl_lines     TYPE sy-tabix.
  REFRESH: tl_itens_aux.

  tl_itens_aux[] = tg_itens[].

  REFRESH: tg_itens.
  CLEAR: wl_itens.

  LOOP AT tl_itens_aux INTO wl_itens.

    wl_itens-itmnum = sy-tabix * 10.
    wl_itens-fase = icon_display_more.
    APPEND wl_itens TO tg_itens.

  ENDLOOP.

  DESCRIBE TABLE tg_itens LINES wl_lines.


  wl_itens-fase = icon_display_more.
  wl_itens-itmnum = ( wl_lines + 1 ) * 10 .
  wl_itens-matnr = lr_content-content-codmaterial.
  wl_itens-werks = lr_content-content-centrocusto.
  wl_itens-anln1 = lr_content-content-codimobilizado.
  wl_itens-anln2 = lr_content-content-codimobsub.
  wl_itens-menge = lr_content-content-quantidade2.
  wl_itens-meins = lr_content-content-unidade.
  wl_itens-netpr = lr_content-content-vendadireta.


  DATA: api_material TYPE mara-matnr.
  api_material = |{ lr_content-content-codmaterial ALPHA = IN }| .

  SELECT SINGLE a~maktg,b~steuc FROM m_mat1m AS a
    INNER JOIN marc AS b ON b~matnr = a~matnr
    INNER JOIN mbew AS c ON c~matnr = a~matnr AND c~bwkey = b~werks
    WHERE b~werks = @lr_content-content-id_filial_sap
    AND a~matnr = @api_material
    INTO (@wl_itens-maktx,@wl_itens-steuc).

  wg_fiscal-move_plant = lr_content-content-coddocliente.
  APPEND wl_itens TO tg_itens.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  INICIA_DOC_LIM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE inicia_doc_lim OUTPUT.

  IF sy-ucomm EQ c_lim OR sy-ucomm EQ 'FURT'.
    IF tg_mensagems_lim[] IS NOT INITIAL.
      LOOP AT tg_mensagems_lim.
        IF v_automatico_memo = abap_false.
          PERFORM preenche_tree USING tg_mensagems_lim-seqnum
                                      c_root
                                      space
                                      cl_gui_simple_tree=>relat_last_child
                                      tg_mensagems_lim-message
                                      handle_tree.
        ENDIF.

      ENDLOOP.

      APPEND LINES OF tg_mensagems_lim[] TO tg_mensagems_aux[].

      PERFORM verifica_erros.

      PERFORM busca_dados.

    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INICIA_CHECK_IMOBILIZADO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE inicia_check_imobilizado OUTPUT.
  IF tg_itens[] IS NOT INITIAL.
    LOOP AT tg_itens ASSIGNING FIELD-SYMBOL(<ws_itens>).

      IF <ws_itens>-add_text EQ abap_true.
        CONTINUE.
      ENDIF.

      IF tg_mensagems_aux[] IS NOT INITIAL.
        SORT tg_mensagems_aux BY seqnum DESCENDING linnum DESCENDING.
        READ TABLE tg_mensagems_aux ASSIGNING FIELD-SYMBOL(<fs_message>) INDEX 1.
        IF sy-subrc EQ 0.
          ADD 1 TO <fs_message>-linnum.
          ADD 1 TO <fs_message>-seqnum.

          <fs_message>-message = 'Imobilizado: ' && <ws_itens>-anln1 && <ws_itens>-anln2.

          <ws_itens>-add_text = abap_true.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO tg_mensagems_aux ASSIGNING <fs_message>.
        ADD 1 TO <fs_message>-linnum.
        ADD 1 TO <fs_message>-seqnum.
        <fs_message>-message = 'Imobilizado: ' && <ws_itens>-anln1 && <ws_itens>-anln2.
        <ws_itens>-add_text = abap_true.
      ENDIF.
    ENDLOOP.

    LOOP AT tg_mensagems_aux.

      PERFORM preenche_tree USING tg_mensagems_aux-seqnum
                                      c_root
                                      space
                                      cl_gui_simple_tree=>relat_last_child
                                      tg_mensagems_aux-message
                                      handle_tree.
    ENDLOOP.

*    PERFORM verifica_erros.

*    PERFORM busca_dados.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_TEXTO_IMBOLIZADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_texto_imbolizado .

ENDFORM.
* Ini - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
*&---------------------------------------------------------------------*
*& Form f_j1btax_43_31
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- WL_1BTXIC
*&---------------------------------------------------------------------*
FORM f_j1btax_43_31 USING wl_itens     LIKE LINE OF tg_itens
                    CHANGING wl_1btxic TYPE any.
* Ini - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
  IF wl_itens-werks IS NOT INITIAL AND wl_itens-matnr IS NOT INITIAL AND wl_1btxic IS INITIAL.

    SELECT SINGLE extwg
      FROM mara
      INTO @DATA(wl_extwg)
        WHERE matnr EQ @wl_itens-matnr.

    IF sy-subrc IS INITIAL.
      IF wl_extwg IS NOT INITIAL.
        " Grupo 43
*        IF wl_1btxic IS INITIAL.
        SELECT SINGLE rate base
                       FROM j_1btxic3
                       INTO wl_1btxic
                        WHERE land1     = c_br
                          AND shipfrom  = wg_shipfrom
                          AND shipto    = wg_shipto
                          AND gruop     = c_43
                          AND value     = wl_extwg
                          AND value2    = wl_itens-werks.
*        ELSE.
*          SELECT SINGLE rate base
*                         FROM j_1btxic3
*                         INTO wl_1btxic
*                          WHERE land1     = c_br
*                            AND shipfrom  = wg_shipfrom
*                            AND shipto    = wg_shipto
*                            AND gruop     = c_43
*                            AND value     = wl_extwg
*                            AND value2    = wl_itens-werks.
*        ENDIF.
      ENDIF.

      IF ( wl_extwg IS INITIAL OR wl_1btxic IS INITIAL ).
        " Grupo 31
        SELECT SINGLE rate base
                       FROM j_1btxic3
                       INTO wl_1btxic
                        WHERE land1     = c_br
                          AND shipfrom  = wg_shipfrom
                          AND shipto    = wg_shipto
                          AND gruop     = c_31
                          AND value     = wl_itens-werks
                          AND value2    = wl_itens-matnr
                          AND value3    = p_parid.
      ENDIF.

      IF wl_1btxic IS INITIAL.
        SELECT SINGLE rate base
                       FROM j_1btxic3
                       INTO wl_1btxic
                        WHERE land1     = c_br
                          AND shipfrom  = wg_shipfrom
                          AND shipto    = wg_shipto
                          AND gruop     = c_31
                          AND value     = wl_itens-werks
                          AND value2    = wl_itens-matnr
                          AND value3    = ''.
      ENDIF.
    ENDIF. "Mara
  ENDIF. "Itens matnr werks
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
ENDFORM.
* Fim - IR122480 - ZNFW0002 - J1BTAX #103385 RJF - 2023.12.05
