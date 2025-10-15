*&---------------------------------------------------------------------*
*&  Include           ZFIR0074TOP
*&---------------------------------------------------------------------*

REPORT zfir0074.

*=======================================================================
* Tabelas
*=======================================================================

TABLES: zimp_aprovador, zinv_aprovador, zglt037,zsdt0336, zsdt0141, zsdt0152, zlest0156, zsdt0161, zmmt0150, zimp_cad_lote.

*=======================================================================
* Types
*=======================================================================

TYPES: BEGIN OF t_zimp_aprovador,
         dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc,
         persnumber    TYPE usr21-persnumber,
         name_first    TYPE adrp-name_first,
         name_last     TYPE adrp-name_last,
         name          TYPE char80,
         celltab       TYPE lvc_t_styl,
         ck_ant        TYPE char1.
         INCLUDE STRUCTURE zimp_aprovador.
TYPES: END OF t_zimp_aprovador.

TYPES: BEGIN OF t_zglt037,
         dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc,
         persnumber    TYPE usr21-persnumber,
         name_first    TYPE adrp-name_first,
         name_last     TYPE adrp-name_last,
         name          TYPE char80,
         celltab       TYPE lvc_t_styl,
         ck_ant        TYPE char1.
         INCLUDE       STRUCTURE zglt037.
TYPES: END OF t_zglt037.

TYPES: BEGIN OF t_zadto_aprovador,
         dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc,
         persnumber    TYPE usr21-persnumber,
         name_first    TYPE adrp-name_first,
         name_last     TYPE adrp-name_last,
         name          TYPE char80,
         celltab       TYPE lvc_t_styl,
         ck_ant        TYPE char1.
         INCLUDE STRUCTURE zadto_aprovador.
TYPES: END OF t_zadto_aprovador.

TYPES: BEGIN OF t_zinv_aprovador,
         ds_operacao   LIKE zfit0043-ds_operacao,
         tipo_desc(50) TYPE c,
         persnumber    TYPE usr21-persnumber,
         name_first    TYPE adrp-name_first,
         name_last     TYPE adrp-name_last,
         name          TYPE char80,
         maktx         TYPE makt-maktx,
         celltab       TYPE lvc_t_styl,
         ck_ant        TYPE char1.
         INCLUDE STRUCTURE zinv_aprovador.
TYPES: END OF t_zinv_aprovador.

TYPES: BEGIN OF t_trans,
         aprovador_de   TYPE usnam,
         aprovador_para TYPE usnam,
         transf_aprov   TYPE char1,
         dt_val_de      TYPE dats,
         dt_val_para    TYPE dats,
         motivo         TYPE char30,
         estrategia     TYPE char40.
TYPES: END OF t_trans.

TYPES: BEGIN OF t_zsdt0141,
         persnumber TYPE usr21-persnumber,
         name_first TYPE adrp-name_first,
         name_last  TYPE adrp-name_last,
         name       TYPE char80,
         celltab    TYPE lvc_t_styl,
         ck_ant     TYPE char1.
         INCLUDE       STRUCTURE zsdt0141.
TYPES: END OF t_zsdt0141.


TYPES: BEGIN OF t_zsdt0152,
         persnumber TYPE usr21-persnumber,
         name_first TYPE adrp-name_first,
         name_last  TYPE adrp-name_last,
         name       TYPE char80,
         celltab    TYPE lvc_t_styl,
         ck_ant     TYPE char1.
         INCLUDE       STRUCTURE zsdt0152.
TYPES: END OF t_zsdt0152.


TYPES: BEGIN OF t_zlest0156,
         persnumber TYPE usr21-persnumber,
         name_first TYPE adrp-name_first,
         name_last  TYPE adrp-name_last,
         name       TYPE char80,
         celltab    TYPE lvc_t_styl,
         ck_ant     TYPE char1.
         INCLUDE       STRUCTURE zlest0156.
TYPES: END OF t_zlest0156.

TYPES: BEGIN OF t_zsdt0161,
         persnumber TYPE usr21-persnumber,
         name_first TYPE adrp-name_first,
         name_last  TYPE adrp-name_last,
         name       TYPE char80,
         celltab    TYPE lvc_t_styl,
         ck_ant     TYPE char1.
         INCLUDE       STRUCTURE zsdt0161.
TYPES: END OF t_zsdt0161.

TYPES: BEGIN OF t_zsdt0336,
         persnumber TYPE usr21-persnumber,
         name_first TYPE adrp-name_first,
         name_last  TYPE adrp-name_last,
         name       TYPE char80,
         celltab    TYPE lvc_t_styl,
         ck_ant     TYPE char1.
         INCLUDE       STRUCTURE zsdt0336.
TYPES: END OF t_zsdt0336.

TYPES: BEGIN OF t_zfiwrt0033,
         persnumber TYPE usr21-persnumber,
         name_first TYPE adrp-name_first,
         name_last  TYPE adrp-name_last,
         name       TYPE char80,
         celltab    TYPE lvc_t_styl,
         ck_ant     TYPE char1.
         INCLUDE       STRUCTURE zfiwrt0033.
TYPES: END OF t_zfiwrt0033.

TYPES: BEGIN OF t_zmmt0150,
         persnumber TYPE usr21-persnumber,
         name_first TYPE adrp-name_first,
         name_last  TYPE adrp-name_last,
         name       TYPE char80,
         celltab    TYPE lvc_t_styl,
         ck_ant     TYPE char1.
         INCLUDE       STRUCTURE zmmt0150.
TYPES: END OF t_zmmt0150.

" 06.05.2025 - 174338 - RAMON -->
TYPES: BEGIN OF t_zsdt0385,
         persnumber TYPE usr21-persnumber,
         name_first TYPE adrp-name_first,
         name_last  TYPE adrp-name_last,
         name       TYPE char80,
         celltab    TYPE lvc_t_styl,
         ck_ant     TYPE char1.
         INCLUDE       STRUCTURE zsdt0385.
TYPES: END OF t_zsdt0385.
" 06.05.2025 - 174338 - RAMON --<


*=======================================================================
* Constantes
*=======================================================================

CONSTANTS: BEGIN OF c_main_tab,
             tab1  LIKE sy-ucomm VALUE 'MAIN_TAB_TAB1',
             tab2  LIKE sy-ucomm VALUE 'MAIN_TAB_TAB2',
             tab3  LIKE sy-ucomm VALUE 'MAIN_TAB_TAB4',
             tab4  LIKE sy-ucomm VALUE 'MAIN_TAB_TAB3',
             tab5  LIKE sy-ucomm VALUE 'MAIN_TAB_TAB5',
             tab6  LIKE sy-ucomm VALUE 'MAIN_TAB_TAB6',
             tab7  LIKE sy-ucomm VALUE 'MAIN_TAB_TAB7',
             tab8  LIKE sy-ucomm VALUE 'MAIN_TAB_TAB8',
             tab9  LIKE sy-ucomm VALUE 'MAIN_TAB_TAB9',
             tab10 LIKE sy-ucomm VALUE 'MAIN_TAB_TAB10',
             tab11 LIKE sy-ucomm VALUE 'MAIN_TAB_TAB11',
             tab12 LIKE sy-ucomm VALUE 'MAIN_TAB_TAB12', "150184 CS2024000781 Aprovações ZNFW - PSA
             tab13 LIKE sy-ucomm VALUE 'MAIN_TAB_TAB13', " 06.05.2025 - 174338 - RAMON
           END OF c_main_tab.

DATA: ck_disp TYPE char1.                             "Check Primeira apresentação da ALV
*=======================================================================
* Variáveis
*=======================================================================

DATA: it_saida_zimp            TYPE STANDARD TABLE OF t_zimp_aprovador,           "Tabela para seleção de estratégia
      it_saida_zimp_c          TYPE STANDARD TABLE OF t_zimp_aprovador,           "Tebela para o complementar da seleção de estratégia
      wa_saida_zimp            TYPE t_zimp_aprovador,
      wa_saida_zimp_c          TYPE t_zimp_aprovador,

      it_saida_vrcamb          TYPE STANDARD TABLE OF t_zimp_aprovador,           "Tabela para seleção de estratégia
      it_saida_vrcamb_c        TYPE STANDARD TABLE OF t_zimp_aprovador,           "Tebela para o complementar da seleção de estratégia
      wa_saida_vrcamb          TYPE t_zimp_aprovador,
      wa_saida_vrcamb_c        TYPE t_zimp_aprovador,

      it_saida_zglt            TYPE STANDARD TABLE OF t_zglt037,                  "Tabela para seleção de estratégia
      it_saida_zglt_c          TYPE STANDARD TABLE OF t_zglt037,                  "Tebela para o complementar da seleção de estratégia
      wa_saida_zglt            TYPE t_zglt037,
      wa_saida_zglt_c          TYPE t_zglt037,

      it_saida_zadto           TYPE STANDARD TABLE OF t_zadto_aprovador,          "Tabela para seleção de estratégia
      it_saida_zadto_c         TYPE STANDARD TABLE OF t_zadto_aprovador,          "Tebela para o complementar da seleção de estratégia
      wa_saida_zadto           TYPE t_zadto_aprovador,
      wa_saida_zadto_c         TYPE t_zadto_aprovador,

      it_saida_zinv            TYPE STANDARD TABLE OF t_zinv_aprovador,           "Tabela para seleção de estratégia
      it_saida_zinv_c          TYPE STANDARD TABLE OF t_zinv_aprovador,           "Tebela para o complementar da seleção de estratégia
      wa_saida_zinv            TYPE t_zinv_aprovador,
      wa_saida_zinv_c          TYPE t_zinv_aprovador,

      it_saida_zov             TYPE STANDARD TABLE OF t_zsdt0141,                  "Tabela para seleção de estratégia
      it_saida_zov_c           TYPE STANDARD TABLE OF t_zsdt0141,                  "Tebela para o complementar da seleção de estratégia
      wa_saida_zov             TYPE t_zsdt0141,
      wa_saida_zov_c           TYPE t_zsdt0141,

      it_saida_lim             TYPE STANDARD TABLE OF t_zsdt0152,                  "Tabela para seleção de estratégia
      it_saida_lim_c           TYPE STANDARD TABLE OF t_zsdt0152,                  "Tebela para o complementar da seleção de estratégia
      wa_saida_lim             TYPE t_zsdt0152,
      wa_saida_lim_c           TYPE t_zsdt0152,

      it_saida_zfre            TYPE STANDARD TABLE OF t_zlest0156,                  "Tabela para seleção de estratégia
      it_saida_zfre_c          TYPE STANDARD TABLE OF t_zlest0156,                  "Tebela para o complementar da seleção de estratégia
      wa_saida_zfre            TYPE t_zlest0156,
      wa_saida_zfre_c          TYPE t_zlest0156,

      it_saida_zsolov          TYPE STANDARD TABLE OF t_zsdt0161,                  "Tabela para seleção de estratégia
      it_saida_zsolov_c        TYPE STANDARD TABLE OF t_zsdt0161,                  "Tebela para o complementar da seleção de estratégia
      wa_saida_zsolov          TYPE t_zsdt0161,
      wa_saida_zsolov_c        TYPE t_zsdt0161,

      it_saida_var_camb        TYPE STANDARD TABLE OF t_zmmt0150,                  "Tabela para seleção de estratégia
      it_saida_var_camb_c      TYPE STANDARD TABLE OF t_zmmt0150,                  "Tebela para o complementar da seleção de estratégia
      wa_saida_var_camb        TYPE t_zmmt0150,
      wa_saida_var_camb_c      TYPE t_zmmt0150,

      it_saida_isencao         TYPE STANDARD TABLE OF t_zsdt0336,                  "Tabela para seleção de estratégia
      it_saida_isencao_c       TYPE STANDARD TABLE OF t_zsdt0336,                  "Tebela para o complementar da seleção de estratégia
      wa_saida_isencao         TYPE t_zsdt0336,
      wa_saida_isencao_c       TYPE t_zsdt0336,

      "150184 CS2024000781 Aprovações ZNFW - PSA
      it_saida_operznfw_final  TYPE STANDARD TABLE OF t_zfiwrt0033,
      it_saida_operznfw        TYPE STANDARD TABLE OF t_zfiwrt0033,                  "Tabela para seleção de estratégia
      it_saida_operznfw_c      TYPE STANDARD TABLE OF t_zfiwrt0033,                  "Tebela para o complementar da seleção de estratégia
      wa_saida_operznfw        TYPE t_zfiwrt0033,
      wa_saida_operznfw_c      TYPE t_zfiwrt0033,

      " 06.05.2025 - 174338 - RAMON -->
      it_saida_checklist_final TYPE STANDARD TABLE OF t_zsdt0385,
      it_saida_checklist       TYPE STANDARD TABLE OF t_zsdt0385,                  "Tabela para seleção de estratégia
      it_saida_checklist_c     TYPE STANDARD TABLE OF t_zsdt0385,                  "Tebela para o complementar da seleção de estratégia
      wa_saida_checklist       TYPE t_zsdt0385,
      wa_saida_checklist_c     TYPE t_zsdt0385.
" 06.05.2025 - 174338 - RAMON --<


FIELD-SYMBOLS: <wa_saida_zimp>      TYPE t_zimp_aprovador,
               <wa_saida_zglt>      TYPE t_zglt037,
               <wa_saida_zov>       TYPE t_zsdt0141,
               <wa_saida_zadto>     TYPE t_zadto_aprovador,
               <wa_saida_zinv>      TYPE t_zinv_aprovador,
               <wa_saida_zfre>      TYPE t_zlest0156,
               <wa_saida_lim>       TYPE t_zsdt0152,
               <wa_saida_zsolov>    TYPE t_zsdt0161,
*               <wa_saida_varcabio> TYPE t_zmmt0150,
               <wa_saida_varcabio>  TYPE t_zmmt0150,
               <wa_saida_isencao>   TYPE t_zsdt0336,
               <wa_saida_operznfw>  TYPE t_zfiwrt0033,
               <wa_saida_checklist> TYPE t_zsdt0385. " 06.05.2025 - 174338 - RAMON


DATA: wa_trans TYPE t_trans.                                            "Tabela de transferência de aprovador

DATA: i_fieldcat TYPE STANDARD TABLE OF lvc_s_fcat.                     "Fieldacat
DATA: wa_stable   TYPE lvc_s_stbl.                                      "Tabela para setar estabilidade do cursor no momento do refresh da alv

DATA: o_custom TYPE REF TO cl_gui_custom_container,                     "Docking Container
      o_grid   TYPE REF TO cl_gui_alv_grid.                             "Grid

DATA: gs_layout TYPE lvc_s_layo.                                        "Layout

DATA: BEGIN OF i_main_tab,                                              "Tabela para controle das tabs selecionadas na screen 9000.
        subscreen   LIKE sy-dynnr,                                      "Subscreen
        prog        LIKE sy-repid VALUE 'ZFIR0074',                     "Program
        pressed_tab LIKE sy-ucomm VALUE c_main_tab-tab1,                "Tab
      END OF i_main_tab.

DATA: ok_code  TYPE sy-ucomm,                                           "OK Button
      vl_uzeit TYPE sy-uzeit.                                           "Hora da Transferência


*=======================================================================
* Classes
*=======================================================================

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS: "DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID IMPORTING ER_DATA_CHANGED,
      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING
                                                                                 e_modified et_good_cells.

ENDCLASS.                    "lcl_event_receiver DEFINITION

DATA: event_handler TYPE REF TO lcl_event_receiver.                     "Handler dos eventos
DATA: it_selected_rows TYPE lvc_t_row,                                  "Tabela de linhas selecionadas na alv de saída
      wa_selected_rows TYPE lvc_s_row.                                  "

*=======================================================================
* Tela de Seleção
*=======================================================================

SELECTION-SCREEN BEGIN OF SCREEN 1010 AS SUBSCREEN .
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001 .
    SELECT-OPTIONS: p_bukrs FOR zimp_aprovador-bukrs MATCHCODE OBJECT c_t001,
                    p_werks FOR zsdt0152-werks,
                    p_depto FOR zimp_aprovador-dep_resp MATCHCODE OBJECT zsh_dep_resp,
                    p_aprov FOR zimp_aprovador-aprovador MATCHCODE OBJECT /plmb/sea_collective_username,
                    p_moeda FOR zimp_aprovador-waers MATCHCODE OBJECT /ba1/f4_fx_waers,
                    p_matnr FOR zinv_aprovador-matnr,
                    p_opera FOR zinv_aprovador-tp_operacao,
                    p_tppgt FOR zinv_aprovador-tipo,
                    p_nivel FOR zinv_aprovador-nivel NO INTERVALS,
                    p_forn  FOR zglt037-pgt_forn NO INTERVALS.
  SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 1010.

CALL SCREEN 9000.
