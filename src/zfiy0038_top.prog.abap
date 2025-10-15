*&---------------------------------------------------------------------*
*&  Include           ZFIY0038_TOP
*&---------------------------------------------------------------------*
REPORT zfiy0038.


*-------------------------------------------------------------------
* Tables
*-------------------------------------------------------------------
TABLES: vbak, zfit0083, zfiyt0032, zfiyt0033.

*-------------------------------------------------------------------
* Types
*-------------------------------------------------------------------

TYPES: BEGIN OF ty_saida_0100,
         status(5)   TYPE  c,
         auart       TYPE vbak-auart,
         kunnr       TYPE vbak-kunnr,
         name1       TYPE kna1-name1,
         vgbel       TYPE vbak-vgbel,
         vbeln       TYPE vbak-vbeln,
         remessa     TYPE vbfa-vbeln,
         fatura      TYPE zfiyt0032-fatura,
         erdat       TYPE vbfa-erdat,
         bstnk       TYPE vbak-bstnk,
         seq         TYPE zfiyt0032-seq,
         xblnr       TYPE vbrk-xblnr,
         bstkd_e     TYPE vbkd-bstkd_e,
         matnr       TYPE vbap-matnr,
         arktx       TYPE vbap-arktx,
         ntgew       TYPE vbap-ntgew,
         netpr       TYPE vbap-netpr,
         netwr       TYPE vbap-netwr,
         waerk       TYPE vbap-waerk,
         erdat_p     TYPE vbfa-erdat,
         sdo_a_apl   TYPE zfiyt0032-sdo_a_apl,
         vlr_aduana  TYPE zfiyt0032-vlr_aduana,
         valor_aplic TYPE zfiyt0032-valor_aplic,
         source_ref  TYPE zfiyt0032-source_ref,
         nro_liq_cb  TYPE zfiyt0032-nro_liq_cb,
         dt_aplic    TYPE zfiyt0032-dt_aplic,
         celltab     TYPE lvc_t_styl.
TYPES END OF ty_saida_0100.

TYPES: BEGIN OF ty_saida_0101_cambio,
         cont_part_name TYPE  zfiyt0033-cont_part_name,
         source_ref     TYPE  zfiyt0033-source_ref,
         nro_liq_cb     TYPE  zfiyt0033-nro_liq_cb,
         date_of_deal   TYPE  zfiyt0033-date_of_deal,
         amount_dealt   TYPE  zfiyt0033-amount_dealt,
         sdo_a_apl      TYPE  zfiyt0033-sdo_a_apl,
         valor_aplic    TYPE  zfiyt0033-valor_aplic,
         dt_aplic       TYPE  zfiyt0033-dt_aplic,
         tp_aplic(1)    TYPE  c.
TYPES  END OF ty_saida_0101_cambio.


TYPES: BEGIN OF ty_saida_0101_permisso,
         status(5)   TYPE  c,
         bstnk       TYPE  vbak-bstnk,
         seq         TYPE  zfiyt0032-seq,
         vbeln       TYPE  vbak-vbeln,
         erdat       TYPE  vbfa-erdat,
         sdo_a_apl   TYPE  zfiyt0032-sdo_a_apl,
         valor_aplic TYPE  zfiyt0032-valor_aplic,
         source_ref  TYPE  zfiyt0032-source_ref,
         nro_liq_cb  TYPE zfiyt0032-nro_liq_cb,
         kunnr       TYPE  vbak-kunnr,
         name1       TYPE  kna1-name1,
         dt_aplic    TYPE  zfiyt0032-dt_aplic,
         tp_aplic(1) TYPE  c.
TYPES  END OF ty_saida_0101_permisso.

TYPES: BEGIN OF ty_saida_0200,
         status(5)        TYPE   c,
         tp_lcto          TYPE   zfiyt0033-tp_lcto,
         lote             TYPE   zfiyt0033-lote,
         doc_lcto         TYPE   zglt035-doc_lcto,
         source_ref       TYPE   zfiyt0033-source_ref,
         currency_1       TYPE   zfiyt0033-currency_1,
         date_of_deal     TYPE   zfiyt0033-date_of_deal,
         time_of_deal     TYPE   zfiyt0033-time_of_deal,
         amount_dealt     TYPE   zfiyt0033-amount_dealt,
         counter_amount   TYPE   zfiyt0033-counter_amount,
         spot_basis_rate  TYPE   zfiyt0033-spot_basis_rate,
         nro_liq_cb       TYPE   zfiyt0033-nro_liq_cb,
         cod_fechto       TYPE   zfiyt0033-cod_fechto,
         trader_name      TYPE   zfiyt0033-trader_name,
         counterparty_nam TYPE   zfiyt0033-counterparty_nam,
         cont_part_name   TYPE   zfiyt0033-cont_part_name,
         sdo_a_apl        TYPE   zfiyt0033-sdo_a_apl,
         valor_aplic      TYPE   zfiyt0033-valor_aplic,
         dt_aplic         TYPE   zfiyt0033-dt_aplic,
         celltab          TYPE   lvc_t_styl.

TYPES END OF ty_saida_0200.


TYPES: BEGIN OF ty_saida_0300,
         kunnr            TYPE   zfiyt0032-kunnr,
         name1            TYPE   kna1-name1,
         vgbel            TYPE   zfiyt0032-vgbel,
         vbeln            TYPE   zfiyt0032-vbeln,
         remessa          TYPE   zfiyt0032-remessa,
         erdat            TYPE   zfiyt0032-erdat,
         bstnk            TYPE   zfiyt0032-bstnk,
         seq              TYPE   zfiyt0032-seq,
         xblnr            TYPE   zfiyt0032-xblnr,
         bstkd_e          TYPE   zfiyt0032-bstkd_e,
         matnr            TYPE   zfiyt0032-matnr,
         arktx            TYPE   zfiyt0032-arktx,
         ntgew            TYPE   zfiyt0032-ntgew,
         netpr            TYPE   zfiyt0032-netpr,
         netwr            TYPE   zfiyt0032-netwr,
         waerk            TYPE   zfiyt0032-waerk,
         erdat_p          TYPE   vbfa-erdat,
         sdo_a_apl        TYPE   zfiyt0032-sdo_a_apl,
         vlr_aduana       TYPE   zfiyt0032-vlr_aduana,
         valor_aplic      TYPE   zfiyt0032-valor_aplic,
         lote             TYPE   zfiyt0033-lote,
         doc_lcto         TYPE   zfiyt0033-doc_lcto,
         source_ref       TYPE   zfiyt0032-source_ref,
         nro_liq_cb       TYPE   zfiyt0032-nro_liq_cb,
         date_of_deal     TYPE   zfiyt0033-date_of_deal,
         time_of_deal     TYPE   zfiyt0033-time_of_deal,
         amount_dealt     TYPE   zfiyt0033-amount_dealt,
         counter_amount   TYPE   zfiyt0033-counter_amount,
         spot_basis_rate  TYPE   zfiyt0033-spot_basis_rate,
         cod_fechto       TYPE   zfiyt0033-cod_fechto,
         trader_name      TYPE   zfiyt0033-trader_name,
         counterparty_nam TYPE   zfiyt0033-counterparty_nam,
         cont_part_name   TYPE   zfiyt0033-cont_part_name.
TYPES END OF ty_saida_0300.

*-------------------------------------------------------------------
* Load Image screen 0101
*-------------------------------------------------------------------
  DATA: w_lines TYPE i.
  TYPES pict_line(256) TYPE c.

  DATA : container TYPE REF TO cl_gui_custom_container,
         editor    TYPE REF TO cl_gui_textedit,
         picture   TYPE REF TO cl_gui_picture,
         pict_tab  TYPE TABLE OF pict_line,
         url(255)  TYPE c,
         graphic_url(255).

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.

  DATA: l_graphic_conv TYPE i,
        l_graphic_offs TYPE i,
        graphic_size   TYPE i,
        l_graphic_xstr TYPE xstring.



*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_sol_inter  TYPE c VALUE  'SOL_INTER'         LENGTH 9,
           c_anul_aplic TYPE c VALUE  'ANUL_APLIC'        LENGTH 10,
           c_ger_contab TYPE c VALUE  'GER_CONTABIL'      LENGTH 12,
           c_save_fchto TYPE c VALUE  'SAVE_FCHTO'        LENGTH 10.

" Classe
CLASS: lcl_event_receiver_0100 DEFINITION DEFERRED,
       lcl_event_receiver_0101 DEFINITION DEFERRED,
       lcl_event_receiver_0200 DEFINITION DEFERRED.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_0100 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_0101 DEFINITION.
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

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_0200 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,
      semantic_checks  IMPORTING pr_data_changed TYPE REF TO cl_alv_changed_data_protocol,
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

*-------------------------------------------------------------------
* Ranges
*-------------------------------------------------------------------
RANGES: rg_auart FOR vbak-auart.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: gva_ucomm       TYPE sy-ucomm,
      gva_idx         TYPE sy-tabix,
      gva_type(5)     TYPE c,
      gva_error_in_data         TYPE c,
      lva_kunnr_i     TYPE vbak-kunnr,
      lva_vbeln_i     TYPE vbak-vbeln,
      lva_bstnk_i     TYPE vbak-bstnk,
      lva_bloomb_i    TYPE zfit0083-source_ref,
      lva_nrolc_i     TYPE zfiyt0032-nro_liq_cb,
      lva_lote_i      TYPE zfiyt0033-lote,
      lva_erdat_i(10) TYPE c,
      lva_erdat_f(10) TYPE c.


*-------------------------------------------------------------------
* Tabelas Internas
*-------------------------------------------------------------------
DATA: git_vbak                TYPE TABLE OF vbak WITH HEADER LINE,
      git_vbfa                TYPE TABLE OF vbfa WITH HEADER LINE,
      git_zfiyt0032           TYPE TABLE OF zfiyt0032 WITH HEADER LINE,
      git_zfiyt0032_aux       TYPE TABLE OF zfiyt0032 WITH HEADER LINE,
      git_zfiyt0033           TYPE TABLE OF zfiyt0033 WITH HEADER LINE,
      git_zfiyt0033_aux       TYPE TABLE OF zfiyt0033 WITH HEADER LINE,
      git_zfit0083            TYPE TABLE OF zfit0083  WITH HEADER LINE,
      git_vbap                TYPE TABLE OF vbap WITH HEADER LINE,
      git_vbkd                TYPE TABLE OF vbkd WITH HEADER LINE,
      git_vbfa_fat            TYPE TABLE OF vbfa WITH HEADER LINE,
      git_vbrk                TYPE TABLE OF vbrk WITH HEADER LINE,
      git_zfiyt0032_sld       TYPE TABLE OF zfiyt0032 WITH HEADER LINE,
      git_kna1                TYPE TABLE OF kna1 WITH HEADER LINE,
      git_zglt031             TYPE TABLE OF zglt031 WITH HEADER LINE,
      git_zglt032             TYPE TABLE OF zglt032 WITH HEADER LINE,
      git_zglt035             TYPE TABLE OF zglt035 WITH HEADER LINE,
      git_zglt036             TYPE TABLE OF zglt036 WITH HEADER LINE,
      git_saida_0100          TYPE TABLE OF ty_saida_0100,
      git_saida_0101_cambio   TYPE TABLE OF ty_saida_0101_cambio,
      git_saida_0101_permisso TYPE TABLE OF ty_saida_0101_permisso,
      git_saida_0200          TYPE TABLE OF ty_saida_0200,
      git_saida_0300          TYPE TABLE OF ty_saida_0300.

*---------------------------------------------------------------------*
* WORKAREA                                                            *
*---------------------------------------------------------------------*
DATA: gwa_vbak                LIKE LINE OF git_vbak,
      gwa_vbfa                LIKE LINE OF git_vbfa,
      gwa_kna1                LIKE LINE OF git_kna1,
      gwa_zfiyt0032           LIKE LINE OF git_zfiyt0032,
      gwa_zfiyt0032_aux       LIKE LINE OF git_zfiyt0032,
      gwa_zfiyt0033           LIKE LINE OF git_zfiyt0033,
      gwa_zfiyt0033_aux       LIKE LINE OF git_zfiyt0033,
      gwa_zfit0083            LIKE LINE OF git_zfit0083,
      gwa_zfiyt0032_sld       LIKE LINE OF git_zfiyt0032_sld,
      gwa_vbfa_fat            LIKE LINE OF git_vbfa_fat,
      gwa_vbrk                LIKE LINE OF git_vbrk,
      gwa_vbkd                LIKE LINE OF git_vbkd,
      gwa_vbap                LIKE LINE OF git_vbap,
      gwa_zglt031             LIKE LINE OF git_zglt031,
      gwa_zglt032             LIKE LINE OF git_zglt032,
      gwa_zglt035             LIKE LINE OF git_zglt035,
      gwa_zglt036             LIKE LINE OF git_zglt036,
      gwa_saida_0100          TYPE ty_saida_0100,
      gwa_saida_0101_cambio   TYPE ty_saida_0101_cambio,
      gwa_saida_0101_permisso TYPE ty_saida_0101_permisso,
      gwa_saida_0101_per_aux  TYPE ty_saida_0101_permisso,
      gwa_saida_0200          TYPE ty_saida_0200,
      gwa_saida_0300          TYPE ty_saida_0300.

*-----------------------------------------------------------------------------------------------------*
* OBJ / CONTAINER
*-----------------------------------------------------------------------------------------------------*
DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container,
      obj_toolbar_0100   TYPE REF TO lcl_event_receiver_0100.

DATA: obj_alv_0101_cambio       TYPE REF TO cl_gui_alv_grid,
      obj_container_0101_cambio TYPE REF TO cl_gui_custom_container,
      obj_toolbar_0101_cambio   TYPE REF TO lcl_event_receiver_0101.

DATA: obj_alv_0101_permisso       TYPE REF TO cl_gui_alv_grid,
      obj_container_0101_permisso TYPE REF TO cl_gui_custom_container,
      obj_toolbar_0101_permisso   TYPE REF TO lcl_event_receiver_0101.

DATA: obj_alv_0200       TYPE REF TO cl_gui_alv_grid,
      obj_container_0200 TYPE REF TO cl_gui_custom_container,
      obj_toolbar_0200   TYPE REF TO lcl_event_receiver_0200.


DATA: obj_alv_0300       TYPE REF TO cl_gui_alv_grid,
      obj_container_0300 TYPE REF TO cl_gui_custom_container.

* ALV Stable
DATA: gwa_stable        TYPE lvc_s_stbl.


* ALV field catalogs
DATA: git_fcat TYPE lvc_t_fcat,
      gwa_fcat TYPE lvc_s_fcat.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV excluded functions
DATA: git_exclude_fcode TYPE ui_functions,
      gwa_exclude_fcode LIKE LINE OF git_exclude_fcode.

DATA: git_exclude_fcode_0101_cambio TYPE ui_functions,
      gwa_exclude_fcode_0101_cambio LIKE LINE OF git_exclude_fcode_0101_cambio.

DATA: git_exclude_fcode_0101_perm TYPE ui_functions,
      gwa_exclude_fcode_0101_perm LIKE LINE OF git_exclude_fcode_0101_perm.

DATA: git_exclude_fcode_0200 TYPE ui_functions,
      gwa_exclude_fcode_0200 LIKE LINE OF git_exclude_fcode.

DATA: git_exclude_fcode_0300 TYPE ui_functions,
      gwa_exclude_fcode_0300 LIKE LINE OF git_exclude_fcode.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: git_sel_rows TYPE lvc_t_row,
      gwa_sel_rows TYPE lvc_s_row.

DATA: git_sel_rows_cambio TYPE lvc_t_row,
      gwa_sel_rows_cambio TYPE lvc_s_row,
      git_sel_rows_permis TYPE lvc_t_row,
      gwa_sel_rows_permis TYPE lvc_s_row,
      git_sel_rows_fechto TYPE lvc_t_row,
      gwa_sel_rows_fechto TYPE lvc_s_row.


DATA: git_layout TYPE lvc_s_layo.

*-----------------------------------------------------------------------------------------------------*
* OPCIONES
*-----------------------------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_solper RADIOBUTTON GROUP rb1 DEFAULT 'X'  USER-COMMAND mod. "Solicitud de Permisos de Embarque X Cierre de cambio
SELECTION-SCREEN COMMENT 03(79) text-002 FOR FIELD  p_solper.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_cambio RADIOBUTTON GROUP rb1.                               "Cierre de cambio – Realizado
SELECTION-SCREEN COMMENT 03(79) text-003 FOR FIELD p_cambio.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_infapl RADIOBUTTON GROUP rb1.                               "Informe de aplicaciones realizadas
SELECTION-SCREEN COMMENT 03(79) text-004 FOR FIELD p_infapl.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-005.

SELECT-OPTIONS: p_kunnr  FOR vbak-kunnr,
                p_vbeln  FOR vbak-vbeln NO INTERVALS,
                p_bstnk  FOR vbak-bstnk NO INTERVALS,
                p_bloomb FOR zfit0083-source_ref NO INTERVALS,
                p_nrolc  FOR zfiyt0032-nro_liq_cb,
                p_lote   FOR zfiyt0033-lote,
                p_erdat  FOR vbak-erdat. "OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

*-----------------------------------------------------------------------------------------------------*
*  Layout
*-----------------------------------------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-006.
PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b3.


AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE abap_true.
      WHEN p_solper.
        IF ( screen-name =  'P_KUNNR-LOW' OR  screen-name = 'P_KUNNR-HIGH'  OR
             screen-name =  'P_VBELN-LOW' OR  screen-name = 'P_VBELN-HIGH'  OR
             screen-name =  'P_BSTNK-LOW' OR   screen-name = 'P_BSTNK-HIGH').

          screen-input = '1'.

          REFRESH: p_bloomb,
                   p_nrolc,
                   p_lote.

          MODIFY SCREEN.
        ENDIF.
        IF ( screen-name  = 'P_BLOOMB-LOW' OR screen-name = 'P_BLOOMB-HIGH' OR
            screen-name   = 'P_NROLC-LOW'  OR  screen-name = 'P_NROLC-HIGH' OR
              screen-name = 'P_LOTE-LOW'   OR  screen-name = 'P_LOTE-HIGH' ).

          screen-input = '0'.

          MODIFY SCREEN.
        ENDIF.

      WHEN p_cambio.
        IF ( screen-name =  'P_KUNNR-LOW' OR  screen-name = 'P_KUNNR-HIGH'  OR
             screen-name =  'P_VBELN-LOW' OR  screen-name = 'P_VBELN-HIGH'  OR
             screen-name =  'P_BSTNK-LOW' OR   screen-name = 'P_BSTNK-HIGH').

          screen-input = '0'.
          MODIFY SCREEN.
        ENDIF.
        IF ( screen-name = 'P_BLOOMB-LOW' OR screen-name = 'P_BLOOMB-HIGH' OR
             screen-name = 'P_NROLC-LOW'  OR  screen-name = 'P_NROLC-HIGH' OR
             screen-name = 'P_LOTE-LOW'   OR  screen-name = 'P_LOTE-HIGH').

          screen-input = '1'.

          REFRESH: p_kunnr,
                   p_vbeln,
                   p_bstnk.


          MODIFY SCREEN.
        ENDIF.
      WHEN p_infapl.
        IF ( screen-name =  'P_KUNNR-LOW' OR  screen-name = 'P_KUNNR-HIGH'  OR
             screen-name =  'P_VBELN-LOW' OR  screen-name = 'P_VBELN-HIGH'  OR
             screen-name =  'P_BSTNK-LOW' OR   screen-name = 'P_BSTNK-HIGH').

          screen-input = '1'.
          MODIFY SCREEN.
        ENDIF.
        IF ( screen-name = 'P_BLOOMB-LOW'   OR screen-name = 'P_BLOOMB-HIGH' OR
             screen-name = 'P_NROLC-LOW'    OR  screen-name = 'P_NROLC-HIGH' OR
             screen-name = 'P_LOTE-LOW'     OR  screen-name = 'P_LOTE-HIGH' ).

          screen-input = '1'.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.

  ENDLOOP.

AT SELECTION-SCREEN.

  gva_ucomm = sy-ucomm. "CAPTURE USER COMMAND

  IF p_varia IS INITIAL.
    CLEAR: gs_variant.
  ENDIF.

  CASE gva_ucomm.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'ONLI'.

      IF p_erdat-low IS INITIAL.
        MESSAGE 'Preencher Data Operação (Obrigatório)'(009) TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        PERFORM f_executar.

        CONCATENATE p_erdat-low+6(2)  '.' p_erdat-low+4(2)  '.'  p_erdat-low+0(4)  INTO lva_erdat_i.

        IF p_erdat-high IS NOT INITIAL.
          CONCATENATE p_erdat-high+6(2) '.' p_erdat-high+4(2) '.'  p_erdat-high+0(4) INTO lva_erdat_f.
        ENDIF.

        CASE abap_true.
          WHEN p_solper. "Solicitud de Permisos de Embarque X Cierre de cambio
            lva_kunnr_i  =  p_kunnr-low.
            lva_vbeln_i  =  p_vbeln-low.
            lva_bstnk_i  =  p_bstnk-low.

            CALL SCREEN '0100'.
          WHEN p_cambio. "Cierre de cambio – Realizado
            lva_bloomb_i = p_bloomb-low.
            lva_nrolc_i  = p_nrolc-low.
            lva_lote_i   = p_lote-low.


            CALL SCREEN '0200'.
          WHEN p_infapl.  "Informe de aplicaciones realizadas
            lva_kunnr_i  =  p_kunnr-low.
            lva_vbeln_i  =  p_vbeln-low.
            lva_bstnk_i  =  p_bstnk-low.
            lva_bloomb_i = p_bloomb-low.
            lva_nrolc_i  = p_nrolc-low.
            lva_lote_i   = p_lote-low.

            CALL SCREEN '0300'.
        ENDCASE.
      ENDIF.
  ENDCASE.
