*&---------------------------------------------------------------------*
*&  Include           ZLESR0098_TOP
*&---------------------------------------------------------------------*

REPORT zlesr0098.

TABLES: zsdt0001,
        j_1bnfdoc.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0100,
         sel,
         ic_xml        TYPE char04,
         ic_mov        TYPE char04,
         name1         TYPE lfa1-name1,
         stcd1         TYPE lfa1-stcd1,
         maktx         TYPE makt-maktx,
         desc_operacao TYPE zlest0129-desc_operacao,
         q_s           TYPE brgew, "Quebra/Sobra
         icon_reenvio  TYPE c LENGTH 4,
*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984
         icon_eudr     TYPE c LENGTH 4.
*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984
         INCLUDE STRUCTURE zsdt0001.
TYPES: END OF ty_saida_0100.

TYPES: BEGIN OF ty_saida_0200,
         sel,
         icon_reenvio       TYPE c LENGTH 4,
         nr_perc_umidade_s  TYPE zsdt0001-nr_perc_umidade,
         nr_qtd_umidade_s   TYPE zsdt0001-nr_qtd_umidade,
         nr_perc_impureza_s TYPE zsdt0001-nr_perc_impureza,
         nr_qtd_impureza_s  TYPE zsdt0001-nr_qtd_impureza,
         nr_perc_avaria_s   TYPE zsdt0001-nr_perc_avaria,
         nr_qtd_avaria_s    TYPE zsdt0001-nr_qtd_avaria,
         nr_perc_ardido_s   TYPE zsdt0001-nr_perc_ardido,
         nr_qtd_ardido_s    TYPE zsdt0001-nr_qtd_ardido,
         nr_perc_quebra_s   TYPE zsdt0001-nr_perc_quebra,
         nr_qtd_quebra_s    TYPE zsdt0001-nr_qtd_quebra,
         nr_perc_esverd_s   TYPE zsdt0001-nr_perc_esverd,
         nr_qtd_esverd_s    TYPE zsdt0001-nr_qtd_esverd,
         nr_perc_umidade_e  TYPE zsdt0001-nr_perc_umidade,
         nr_qtd_umidade_e   TYPE zsdt0001-nr_qtd_umidade,
         nr_perc_impureza_e TYPE zsdt0001-nr_perc_impureza,
         nr_qtd_impureza_e  TYPE zsdt0001-nr_qtd_impureza,
         nr_perc_avaria_e   TYPE zsdt0001-nr_perc_avaria,
         nr_qtd_avaria_e    TYPE zsdt0001-nr_qtd_avaria,
         nr_perc_ardido_e   TYPE zsdt0001-nr_perc_ardido,
         nr_qtd_ardido_e    TYPE zsdt0001-nr_qtd_ardido,
         nr_perc_quebra_e   TYPE zsdt0001-nr_perc_quebra,
         nr_qtd_quebra_e    TYPE zsdt0001-nr_qtd_quebra,
         nr_perc_esverd_e   TYPE zsdt0001-nr_perc_esverd,
         nr_qtd_esverd_e    TYPE zsdt0001-nr_qtd_esverd,
         datachegada        TYPE zlest0039-datachegada,
         pesochegada        TYPE zlest0039-pesochegada,
*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984
         icon_eudr          TYPE c LENGTH 4.
*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984
         INCLUDE STRUCTURE zsdt0001.
TYPES: END OF ty_saida_0200.

DATA: BEGIN OF tg_zsdt0001 OCCURS 0,
        forne_cnpj TYPE zib_nfe_dist_ter-forne_cnpj,
        numero     TYPE zib_nfe_dist_ter-numero,
        serie      TYPE zib_nfe_dist_ter-serie,
        msg_v1     TYPE zob_ret_msg-msg_v1.
        INCLUDE STRUCTURE zsdt0001.
DATA: END OF tg_zsdt0001.

DATA: BEGIN OF tg_t001 OCCURS 0,
        bukrs TYPE t001-bukrs,
        butxt TYPE t001-butxt,
      END OF tg_t001,

      BEGIN OF tg_j_1bbranch OCCURS 0,
        bukrs  TYPE j_1bbranch-bukrs,
        branch TYPE j_1bbranch-branch,
      END OF tg_j_1bbranch,

      BEGIN OF tg_kna1 OCCURS 0,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
        stcd1 TYPE kna1-stcd1,
      END OF tg_kna1,

      BEGIN OF tg_lfa1 OCCURS 0,
        lifnr TYPE lfa1-lifnr,
        name1 TYPE lfa1-name1,
        stcd1 TYPE lfa1-stcd1,
      END OF tg_lfa1,

      BEGIN OF tg_makt OCCURS 0,
        matnr TYPE makt-matnr,
        maktx TYPE makt-maktx,
      END OF tg_makt,

      BEGIN OF tg_vbfa OCCURS 0,
        vbelv        TYPE vbfa-vbelv,
        vbtyp_n      TYPE vbfa-vbtyp_n,
        vbeln        TYPE vbfa-vbeln,
        vbeln_refkey TYPE j_1bnflin-refkey,
      END OF tg_vbfa.

DATA: BEGIN OF tg_j_1bnflin OCCURS 0,
        docnum TYPE j_1bnflin-docnum,
        refkey TYPE j_1bnflin-refkey.
DATA: END OF tg_j_1bnflin.

DATA: BEGIN OF tg_j_1bnfe_active OCCURS 0,
        docnum  TYPE j_1bnfe_active-docnum,
        regio   TYPE j_1bnfe_active-regio,
        nfyear  TYPE j_1bnfe_active-nfyear,
        nfmonth TYPE j_1bnfe_active-nfmonth,
        stcd1   TYPE j_1bnfe_active-stcd1,
        model   TYPE j_1bnfe_active-model,
        serie   TYPE j_1bnfe_active-serie,
        nfnum9  TYPE j_1bnfe_active-nfnum9,
        docnum9 TYPE j_1bnfe_active-docnum9,
        cdv     TYPE j_1bnfe_active-cdv.
DATA: END OF tg_j_1bnfe_active.

DATA: BEGIN OF tg_zib_nfe_dist_ter OCCURS 0,
        forne_cnpj TYPE zib_nfe_dist_ter-forne_cnpj,
        numero     TYPE zib_nfe_dist_ter-numero,
        serie      TYPE zib_nfe_dist_ter-serie,
        chave_nfe  TYPE zib_nfe_dist_ter-chave_nfe.
DATA: END OF tg_zib_nfe_dist_ter.

DATA: BEGIN OF tg_zlest0129 OCCURS 0.
        INCLUDE STRUCTURE zlest0129.
DATA: END OF tg_zlest0129.

DATA: tg_zsdt0001_rem_s TYPE TABLE OF zsdt0001,
      tg_zsdt0001_rem_e TYPE TABLE OF zsdt0001,
      tg_zlest0039      TYPE TABLE OF zlest0039,
      ws_zsdt0001_rem_s TYPE zsdt0001,
      ws_zsdt0001_rem_e TYPE zsdt0001,
      ws_zlest0039      TYPE zlest0039.

*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984
CONSTANTS: gc_atende_eudr TYPE c LENGTH 01 VALUE 'S'.
*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0100 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
* Estruturas ALV
*----------------------------------------------------------------------*

DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant   TYPE disvariant,
      variante     LIKE disvariant,
      gs_variant_c TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE slis_layout_alv.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo TYPE lvc_s_styl.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

DATA: it_estrutura TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura.

DATA: xs_events TYPE slis_alv_event,
      events    TYPE slis_t_event,
      t_print   TYPE slis_print_alv,
      v_report  LIKE sy-repid,
      t_top     TYPE slis_t_listheader.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100   TYPE TABLE OF ty_saida_0100,
      wa_saida_0100   TYPE ty_saida_0100,
      it_saida_0200   TYPE TABLE OF ty_saida_0200,
      wa_saida_0200   TYPE ty_saida_0200,
      tg_zob_ret_msg  TYPE TABLE OF zob_ret_msg WITH HEADER LINE,
      tg_zsdt0001_aux LIKE TABLE OF tg_zsdt0001 WITH HEADER LINE.


*----------------------------------------------------------------------*
* tela de Seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-003.
  PARAMETERS    : p_romane RADIOBUTTON                 GROUP g1 USER-COMMAND usr1 DEFAULT 'X',
                  p_classi RADIOBUTTON                 GROUP g1.
SELECTION-SCREEN: END   OF BLOCK b0.


*** Inicio - Rubenilson - 17.09.24 - #151332
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-005.

  PARAMETERS: p_entr TYPE c RADIOBUTTON GROUP g2 MODIF ID t1,
              p_said TYPE c RADIOBUTTON GROUP g2 MODIF ID t1,
              p_tods TYPE c RADIOBUTTON GROUP g2 MODIF ID t1.

SELECTION-SCREEN END OF BLOCK b2.
*** Fim - Rubenilson - 17.09.24 - #151332

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: p_bukrs  FOR zsdt0001-bukrs          MODIF ID t2 NO INTERVALS NO-EXTENSION, " OBLIGATORY,
                  p_branch FOR j_1bnfdoc-branch        MODIF ID t2 NO INTERVALS NO-EXTENSION, " OBLIGATORY,
*                  p_tp_mov FOR zsdt0001-tp_movimento   MODIF ID t1, " Rubenilson - 17.09.24 - #151332
                  p_num_ro FOR zsdt0001-nr_romaneio    MODIF ID t1,
                  p_safra  FOR zsdt0001-nr_safra       MODIF ID t2,
                  p_dt_mov FOR zsdt0001-dt_movimento   MODIF ID t2, " OBLIGATORY,
                  p_parid  FOR zsdt0001-parid          MODIF ID t1,
                  p_tp_fre FOR zsdt0001-tp_frete       MODIF ID t1,
                  p_matnr  FOR zsdt0001-matnr          MODIF ID t2,
                  p_id_cli FOR zsdt0001-id_cli_dest    MODIF ID t3,
                  p_placa  FOR zsdt0001-placa_cav      MODIF ID t1,
                  p_moto   FOR zsdt0001-motorista      MODIF ID t1,
                  p_dt_fec FOR zsdt0001-dt_fechamento  MODIF ID t1,
                  p_dt_abe FOR zsdt0001-dt_abertura    MODIF ID t1,
                  p_dc_rem FOR zsdt0001-doc_rem        MODIF ID t1,
                  p_transg FOR zsdt0001-tp_transgenia  MODIF ID t1,
                  p_lc_des FOR zsdt0001-local_descarga MODIF ID t1,
                  p_tp_ent FOR zsdt0001-tipo_entrada   MODIF ID t1,
                  p_ch_ref FOR zsdt0001-ch_referencia  MODIF ID t1,
                  p_knfe   FOR zsdt0001-chave_nfe      MODIF ID t1. "AHSS #145574 Inserido o campo Chave Nfe no filtro
  PARAMETER:      p_chv_nf AS CHECKBOX                 MODIF ID t1.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-002.
  PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b5.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*

DATA: vg_repid   LIKE sy-repid,
      vg_variant TYPE disvariant.

*-CS2020001167 - 24.03.2021 - JT - inicio
*---------------------------------------------------------------------*
* evento saida tela
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p_romane = abap_true.
      IF screen-group1 = 'T3'.
        screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
      ENDIF.
    ELSE.
      IF screen-group1 = 'T1'.
        screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON p_bukrs.

  CHECK sy-ucomm <> 'USR1'.

  IF p_bukrs IS INITIAL.
    MESSAGE s000(z01) WITH 'Empresa campo obrigatório.' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

AT SELECTION-SCREEN ON p_branch.

  CHECK sy-ucomm <> 'USR1'.
  CHECK p_romane  = abap_true.

  IF p_branch IS INITIAL.
    MESSAGE s000(z01) WITH 'Filial: campo obrigatório.' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

AT SELECTION-SCREEN ON p_dt_mov.

  CHECK sy-ucomm <> 'USR1'.

  IF p_dt_mov IS INITIAL.
    MESSAGE s000(z01) WITH 'Período: campo obrigatório.' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
*-CS2020001167 - 24.03.2021 - JT - fim

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid        = sy-repid.
  variante-report = vg_repid.

  IF ( p_varia IS NOT INITIAL ).
    vg_variant-variant = p_varia.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.



START-OF-SELECTION.

*-CS2020001167 - 24.03.2021 - JT - inicio
  IF p_romane = abap_true.
    PERFORM f_selecionar_dados.
    PERFORM f_processar_dados.
    PERFORM f_imprimir_dados.
  ELSE.
    PERFORM f_selecionar_dados_classif.
    PERFORM f_processar_dados_classif.
    PERFORM f_imprimir_dados_classif.
  ENDIF.
*-CS2020001167 - 24.03.2021 - JT - fim
