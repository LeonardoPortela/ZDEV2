*&---------------------------------------------------------------------*
*& Report  ZSDR0060
*&
*&---------------------------------------------------------------------*
*&
*& Cockpit Montagem de Cargas - Insumos
*&
*&---------------------------------------------------------------------*
REPORT zsdr0180.
*=============================================================================*
*INCLUDES                                                                     *
*=============================================================================*
*INCLUDE ZSDR0060_TOP.
*=============================================================================*
*TABLES                                                                       *
*=============================================================================*
TABLES: tvko, tvbur, tspa, tinc, kna1, zsdt0129, zsdt0133, zsdt0082,
        zsdt0282.
*=============================================================================*
*DATA                                                                         *
*=============================================================================*


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
         dt_entrega   TYPE zsdt0129-dt_entrega.
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
        prog        LIKE sy-repid VALUE 'ZSDR0180',
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

DATA p_ative TYPE char1.

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

*-CS2021000218-30.08.2022-#893743-JT-inicio
DATA: w_rtc_proprio        TYPE c,
      w_rtc_terceiro       TYPE c,
      l_cpf_rtc            TYPE zsdt0139-cpf_rtc,
      l_carga_5820_nro_cgd TYPE zsdt0139-nro_cgd.

RANGES: r_werks             FOR zsdt0266-werks.
*-CS2021000218-30.08.2022-#893743-JT-fim

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
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_spart2 RADIOBUTTON GROUP a1 DEFAULT 'X' USER-COMMAND abc,
              p_spart3 RADIOBUTTON GROUP a1,
              p_spart4 RADIOBUTTON GROUP a1.
SELECTION-SCREEN END OF BLOCK a1.

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
  PARAMETERS:
*              p_filial RADIOBUTTON GROUP g3 DEFAULT 'X' USER-COMMAND jkl MODIF ID 04,
*              p_corplt RADIOBUTTON GROUP g3 MODIF ID 04,
*              p_corpcg RADIOBUTTON GROUP g3 MODIF ID 04,
*              p_corppt RADIOBUTTON GROUP g3 MODIF ID 04,
    p_logcor RADIOBUTTON GROUP g3 DEFAULT 'X' USER-COMMAND jkl MODIF ID 04,
    p_transp RADIOBUTTON GROUP g3 MODIF ID 04.
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
*=============================================================================*
*AT SELECTION-SCREEN                                                          *
*=============================================================================*
AT SELECTION-SCREEN OUTPUT.

  FREE: w_zsdt0282_tot.

  LOOP AT SCREEN.

    IF screen-name(7) = 'X_VKORG'.
      PERFORM trata_transportadora.
    ENDIF.

    IF p_spart2 EQ abap_true.
      READ TABLE x_vkorg INDEX 1.
      READ TABLE t_zsdt0282_tot INTO w_zsdt0282_tot WITH KEY vkorg  = x_vkorg-low
                                                             transp = x_transp.
*     IF p_fcorp EQ abap_true.
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
          CLEAR: "x_vkorg[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, "T2
                 "g_vkorg[], g_inco1[], g_kunnr[], g_nrsol[], g_datas[], g_ovcor[], g_spart, "T2
                 "o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
                 d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                 p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                 c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_L, C_C
                 l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "L4
                 t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4

          CONTINUE.
        ENDIF.
*     ELSEIF p_ftpng EQ abap_true.
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
          CLEAR: "x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, "C2
                 "r_vkorg[], r_vkbur[], r_inco1[], r_kunnr[], r_nrsol[], r_datas[], r_ovcor[], r_spart, "C2
                 "o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
                 d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                 p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                 c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
                 l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
                 t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
          CONTINUE.
        ENDIF.
*     ELSEIF p_ftroo EQ abap_true.
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
          CLEAR: "x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, "C2
                 "r_vkorg[], r_vkbur[], r_inco1[], r_kunnr[], r_nrsol[], r_datas[], r_ovcor[], r_spart, "C2
                 "g_vkorg[], g_inco1[], g_kunnr[], g_nrsol[], g_datas[], g_ovcor[], g_spart, "T2
*                 o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
                 d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                 p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                 c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
                 l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
                 t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4

          CONTINUE.
        ENDIF.
      ENDIF.
    ELSEIF p_spart3 EQ abap_true.
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
               "r_vkorg[], r_vkbur[], r_inco1[], r_kunnr[], r_nrsol[], r_datas[], r_ovcor[], r_spart, "C2
               "g_vkorg[], g_inco1[], g_kunnr[], g_nrsol[], g_datas[], g_ovcor[], g_spart, "T2
               "o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
*       d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
               p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
               c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
               l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
               t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4

        CONTINUE.
      ENDIF.
    ELSEIF p_spart4 EQ abap_true.
*      IF p_filial EQ abap_true.
*        IF screen-group1 = 'C4'  OR
*           screen-group1 = 'C_L' OR
*           screen-group1 = 'C_C' OR
*           screen-group1 = 'T4'  OR
*           screen-group1 = 'L4'  OR
*           screen-group1 = '02'  OR
*           screen-group1 = 'X2'  OR
*           screen-group1 = 'C2'  OR
*           screen-group1 = 'D3'  OR
*           screen-group1 = 'X3'  OR
*           screen-group1 = 'T2'  OR
*           screen-group1 = 'T3'.
*          screen-active = '0'.
*          MODIFY SCREEN.
*          CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
*                 "r_vkorg[], r_vkbur[], r_inco1[], r_kunnr[], r_nrsol[], r_datas[], r_ovcor[], r_spart, "C2
*                 "g_vkorg[], g_inco1[], g_kunnr[], g_nrsol[], g_datas[], g_ovcor[], g_spart, "T2
*                 "o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
*                 d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
**       p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
*                 c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
*                 l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
*                 t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
*          CONTINUE.
*        ENDIF.
*      ELSEIF p_corplt EQ abap_true.
*        IF screen-group1 = 'F4'  OR
*           screen-group1 = 'C_C' OR
*           screen-group1 = 'T4'  OR
*           screen-group1 = 'L4'  OR
*           screen-group1 = '02'  OR
*           screen-group1 = 'X2'  OR
*           screen-group1 = 'C2'  OR
*           screen-group1 = 'D3'  OR
*           screen-group1 = 'X3'  OR
*           screen-group1 = 'T2'  OR
*           screen-group1 = 'T3'.
*          screen-active = '0'.
*          MODIFY SCREEN.
*          CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
*                 "r_vkorg[], r_vkbur[], r_inco1[], r_kunnr[], r_nrsol[], r_datas[], r_ovcor[], r_spart, "C2
*                 "g_vkorg[], g_inco1[], g_kunnr[], g_nrsol[], g_datas[], g_ovcor[], g_spart, "T2
*                 "o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
*                 d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
*                 p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
**               c_vkorg[], c_inco1[], c_kunnr[], c_ovcor[], c_spart,  "C4
**               c_vkbur[], c_nlote[], c_dataa[], " C_L
*                  c_numcg[], c_datab[], "C_C
*                 l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
*                 t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
*          CONTINUE.
*        ENDIF.
*      ELSEIF p_corpcg EQ abap_true.
*        IF screen-group1 = 'F4'  OR
*           screen-group1 = 'C_L' OR
*           screen-group1 = 'T4'  OR
*           screen-group1 = 'L4'  OR
*           screen-group1 = '02'  OR
*           screen-group1 = 'X2'  OR
*           screen-group1 = 'C2'  OR
*           screen-group1 = 'D3'  OR
*           screen-group1 = 'X3'  OR
*           screen-group1 = 'T2'  OR
*           screen-group1 = 'T3'.
*          screen-active = '0'.
*          MODIFY SCREEN.
*          CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
*                 "r_vkorg[], r_vkbur[], r_inco1[], r_kunnr[], r_nrsol[], r_datas[], r_ovcor[], r_spart, "C2
*                 "g_vkorg[], g_inco1[], g_kunnr[], g_nrsol[], g_datas[], g_ovcor[], g_spart, "T2
*                 "o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
*                 d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
*                 p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
**                 c_vkorg[], c_inco1[], c_kunnr[], c_ovcor[], c_spart,  "C4
*                 c_vkbur[], c_nlote[], c_dataa[], " C_L
**                 c_numcg[], c_datab[], "C_C
*                 l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
*                 t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
*          CONTINUE.
*        ENDIF.
*      ELSEIF p_corppt EQ abap_true.
*        IF screen-group1 = 'F4'  OR
*           screen-group1 = 'C_L' OR
*           screen-group1 = 'T4'  OR
*           screen-group1 = 'L4'  OR
*           screen-group1 = '02'  OR
*           screen-group1 = 'X2'  OR
*           screen-group1 = 'C2'  OR
*           screen-group1 = 'D3'  OR
*           screen-group1 = 'X3'  OR
*           screen-group1 = 'T2'  OR
*           screen-group1 = 'T3'.
*          screen-active = '0'.
*          MODIFY SCREEN.
*          CLEAR: x_vkorg[], x_vkbur[], x_inco1[], x_kunnr[], x_nrsol[], x_datas[], x_ovcor[], x_spart, x_transp, "C2
*                 "r_vkorg[], r_vkbur[], r_inco1[], r_kunnr[], r_nrsol[], r_datas[], r_ovcor[], r_spart, "C2
*                 "g_vkorg[], g_inco1[], g_kunnr[], g_nrsol[], g_datas[], g_ovcor[], g_spart, "T2
*                 "o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
*                 d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
*                 p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
**                 c_vkorg[], c_inco1[], c_kunnr[], c_ovcor[], c_spart,  "C4
*                 c_vkbur[], c_nlote[], c_dataa[], " C_L
**                 c_numcg[], c_datab[], "C_C
*                 l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "l4
*                 t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
*          CONTINUE.
*        ENDIF.
*      ELSEIF p_logcor EQ abap_true.
      IF p_logcor EQ abap_true.
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
                 "r_vkorg[], r_vkbur[], r_inco1[], r_kunnr[], r_nrsol[], r_datas[], r_ovcor[], r_spart, "C2
                 "g_vkorg[], g_inco1[], g_kunnr[], g_nrsol[], g_datas[], g_ovcor[], g_spart, "T2
                 "o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
                 d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                 p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                 c_vkorg[], c_inco1[], c_kunnr[], c_ovcor[], c_spart,  "C4
                 c_vkbur[], c_nlote[], c_dataa[], " C_L
                 c_numcg[], c_datab[], "C_C
*       l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart, "L4
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
                 "r_vkorg[], r_vkbur[], r_inco1[], r_kunnr[], r_nrsol[], r_datas[], r_ovcor[], r_spart, "C2
                 "g_vkorg[], g_inco1[], g_kunnr[], g_nrsol[], g_datas[], g_ovcor[], g_spart, "T2
                 "o_vkorg[], o_inco1[], o_kunnr[], o_nrsol[], o_datas[], o_ovcor[], o_spart, "T3
                 d_vkorg[], d_vkbur[], d_inco1[], d_kunnr[], d_numcg[], d_datab[], d_ovcor[], d_spart, "D3
                 p_vkorg[], p_vkbur[], p_inco1[], p_kunnr[], p_nlote[], p_dataa[], p_ovcor[], p_spart, "F4
                 c_vkorg[], c_inco1[], c_kunnr[], c_vkbur[], c_nlote[], c_dataa[], c_numcg[], c_datab[], c_ovcor[], c_spart, "C4, C_1, C_C
                 l_vkorg[], l_inco1[], l_numcg[], l_datab[], l_ovcor[], l_spart. "l4
*       t_vkorg[], t_inco1[], t_kunnr[], t_numcg[], t_datab[], t_ovcor[], t_spart. "T4
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

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

  w_rtc_proprio = abap_true.

  PERFORM check_parametros CHANGING gv_erro.

  IF gv_erro IS INITIAL.
    CALL SCREEN 5000.
  ENDIF.

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
*    IF p_filial IS NOT INITIAL.
*      vl_visao = 'F'.
*      vg_subt_lote = '5120'.
*      g_tabstrip-pressed_tab = c_tabstrip-tab1.
*      p_spart = '04'.
*      "Check para os parâmetros Obrigatórios
*      IF p_vkorg IS INITIAL AND p_ovcor IS INITIAL..
*        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ELSEIF p_vkbur IS INITIAL AND p_ovcor IS INITIAL.
*        MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ELSEIF p_inco1 IS INITIAL.
*        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*    ELSEIF p_corplt IS NOT INITIAL.
*      vl_visao = 'C'.
*      vg_subt_lote = '5120'.
*      g_tabstrip-pressed_tab = c_tabstrip-tab1.
*      c_spart = '04'.
*      "Check para os parâmetros Obrigátórios
*      IF c_vkorg IS INITIAL AND c_ovcor IS INITIAL.
*        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ELSEIF c_inco1 IS INITIAL.
*        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*    ELSEIF p_corpcg IS NOT INITIAL.
*      vl_visao = 'C'.
*      vg_subt_lote = '5230'.
*      g_tabstrip-pressed_tab = c_tabstrip-tab2.
*      c_spart = '04'.
*      "Check para os parâmetros Obrigátórios
*      IF c_vkorg IS INITIAL AND c_ovcor IS INITIAL.
*        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ELSEIF c_inco1 IS INITIAL.
*        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*    ELSEIF p_corppt IS NOT INITIAL.
*      vl_visao = 'C'.
*      vg_subt_lote = '5320'.
*      g_tabstrip-pressed_tab = c_tabstrip-tab3.
*      c_spart = '04'.
*      "Check para os parâmetros Obrigátórios
*      IF c_vkorg IS INITIAL AND c_ovcor IS INITIAL.
*        MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ELSEIF c_inco1 IS INITIAL.
*        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
    IF p_logcor IS NOT INITIAL.
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
*      IF p_filial IS NOT INITIAL.
*        IF vl_visao NE 'F'.
*          MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
*          STOP.
*        ENDIF.
*      ELSEIF p_corplt IS NOT INITIAL OR
*             p_corpcg IS NOT INITIAL OR
*             p_corppt IS NOT INITIAL.
*        IF vl_visao NE 'C'.
*          MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
*          STOP.
*        ENDIF.
      IF p_logcor IS NOT INITIAL.
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

*  "Check de Permissão de Esc. de Vendas
*  IF p_filial IS NOT INITIAL.
*
*    SELECT *
*      FROM zsdt0060
*      INTO TABLE it_zsdt0060
*      WHERE usnam    EQ sy-uname
*        AND programa EQ 'ZSDR016'
*        AND vkbur    IN p_vkbur.
*
*    IF it_zsdt0060 IS INITIAL.
*      MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
*
*    IF p_inco1-low EQ '*'.
*      MESSAGE TEXT-079 TYPE 'S' DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
*
*  ELSEIF p_corplt IS NOT INITIAL.
*
*    IF c_inco1-low EQ '*'.
*      MESSAGE TEXT-079 TYPE 'S' DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
*
*    IF c_vkbur IS NOT INITIAL.
*
*      SELECT *
*            FROM zsdt0060
*            INTO TABLE it_zsdt0060
*            WHERE usnam    EQ sy-uname
*              AND programa EQ 'ZSDR016'
*              AND vkbur    IN c_vkbur.
*
*      IF it_zsdt0060 IS INITIAL.
*        MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*
*
*    ENDIF.
*
*  ELSEIF ( p_corpcg IS NOT INITIAL OR p_corppt IS NOT INITIAL ) AND
*         ( c_vkbur IS NOT INITIAL ).
*
*    SELECT *
*      FROM zsdt0060
*      INTO TABLE it_zsdt0060
*      WHERE usnam    EQ sy-uname
*        AND programa EQ 'ZSDR016'
*        AND vkbur    IN c_vkbur.
*
*    IF it_zsdt0060 IS INITIAL.
*      MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
*
*  ENDIF.

*  "Check do Cliente
*  IF p_filial IS NOT INITIAL AND p_kunnr IS NOT INITIAL.
*
*    SELECT *
*      FROM kna1
*      INTO TABLE it_kna1
*      WHERE kunnr IN p_kunnr.
*
*    IF it_kna1 IS INITIAL.
*      MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.
*
*  ELSEIF ( p_corplt IS NOT INITIAL OR p_corpcg IS NOT INITIAL OR p_corppt IS NOT INITIAL ) AND
*         ( c_kunnr IS NOT INITIAL ).
*
*    SELECT *
*      FROM kna1
*      INTO TABLE it_kna1
*      WHERE kunnr IN c_kunnr.
*
*    IF it_kna1 IS INITIAL.
*      MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
*      STOP.
*    ENDIF.

  IF p_transp IS NOT INITIAL AND t_kunnr IS NOT INITIAL.

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

INCLUDE ole2incl.

INCLUDE zsdr0060_forms.

INCLUDE zsdr0180_5000.
*INCLUDE zsdr0060_5000.

INCLUDE zsdr0180_5120.
*INCLUDE zsdr0060_5120.

INCLUDE zsdr0180_5130.
*INCLUDE zsdr0060_5130.

INCLUDE zsdr0180_5131.
*INCLUDE zsdr0060_5131.

INCLUDE zsdr0180_5140.
*INCLUDE zsdr0060_5140.

INCLUDE zsdr0180_5141.
*INCLUDE zsdr0060_5141.

INCLUDE zsdr0180_5121.
*INCLUDE zsdr0060_5121.

INCLUDE zsdr0180_5220.
*INCLUDE zsdr0060_5220.

INCLUDE zsdr0180_5230.
*INCLUDE zsdr0060_5230.

INCLUDE zsdr0180_5320.
*INCLUDE zsdr0060_5320.

INCLUDE zsdr0180_5221.
*INCLUDE zsdr0060_5221.

INCLUDE zsdr0180_5321.
*INCLUDE zsdr0060_5321.

INCLUDE zsdr0180_5420.

INCLUDE zsdr0180_5520.

INCLUDE zsdr0180_5620.

INCLUDE zsdr0180_5720.

INCLUDE zsdr0180_5730.

INCLUDE zsdr0180_5731.

INCLUDE zsdr0180_5740.

INCLUDE zsdr0180_5741.

INCLUDE zsdr0180_5820.

INCLUDE zsdr0180_5521.

INCLUDE zsdr0180_5821.

INCLUDE zsdr0180_5822 IF FOUND.  "*-CS2021000218-31.08.2022-#89492-JT-inicio

INCLUDE zsdr0180_5322.

INCLUDE zsdr0180_5522.

INCLUDE zsdr0180_6001.

INCLUDE zsdr0180_7100.

INCLUDE zsdr0180_5231.

INCLUDE zsdr0180_8000.
