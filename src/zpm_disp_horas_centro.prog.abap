*-----------------------------------------------------------------------------*
*&                 AMAGGI - Projeto Abaco
*-----------------------------------------------------------------------------*
*& Criado por:  José Godoy ( JAP ) - Ábaco Consultores
*& Data      : 16/06/2017
*& Pedido por: Cleudo Ferreira
*& Chamado/Descrição :xxxx Porto Itacoatiara
*& Request: DEVK971787 PM - 08.06.2017 - ALV Rel.Diagn.p/Planta [  ] JAP
*-----------------------------------------------------------------------------*
*& Histórico de Alterações:                                                   *
*-----------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                       *
*&----------------------------------------------------------------------------*
*& 30.01.18    |            |AOENNING       |"Calculo horas apontadas         *
*-----------------------------------------------------------------------------*
REPORT zpm_disp_horas_centro.

*----------------------------------------------------------*
*          Tables Types
*----------------------------------------------------------*
*
TABLES: crhd, aufk, afru, crca, kako, viaufkst, crco.
*
*-----------------------------------------------------------------------------*
*-> Tipos internos
*-----------------------------------------------------------------------------*
*
TYPES: BEGIN OF y_aufk,
         aufnr TYPE aufk-aufnr,
         werks TYPE aufk-werks,
         kostl TYPE aufk-kostl,
         vaplz TYPE aufk-vaplz,
         autyp TYPE aufk-autyp,
         auart TYPE aufk-auart,
       END OF y_aufk.

TYPES: BEGIN OF y_viaufkst,
         aufnr TYPE viaufkst-aufnr,
         equnr TYPE viaufkst-equnr,
         werks TYPE viaufkst-werks,
         tplnr TYPE viaufkst-tplnr,
         kostl TYPE viaufkst-kostl,
       END OF y_viaufkst.

TYPES: BEGIN OF y_viaufkst_,
         aufnr TYPE viaufkst-aufnr,
         equnr TYPE viaufkst-equnr,
         werks TYPE viaufkst-werks,
         tplnr TYPE viaufkst-tplnr,
         kostl TYPE viaufkst-kostl,
       END OF y_viaufkst_.

TYPES: BEGIN OF y_iflo,
         tplnr TYPE iflo-tplnr,
         tplma TYPE iflo-tplma,
         pltxt TYPE iflo-pltxt,
       END OF y_iflo.


*
TYPES: BEGIN OF y_afru,
         rueck TYPE afru-rueck,
         rmzhl TYPE afru-rmzhl,
         ltxa1 TYPE afru-ltxa1,
         ersda TYPE afru-ersda,
         budat TYPE afru-budat,
         aenam TYPE afru-aenam,
         aufnr TYPE afru-aufnr,
         vornr TYPE afru-vornr,
         werks TYPE afru-werks,
         arbid TYPE afru-arbid,
         ismnw TYPE p DECIMALS 2,
         ofmnw TYPE afru-ofmnw,
         ismne TYPE afru-ismne,
         idaur TYPE afru-idaur,
         anzma TYPE afru-anzma,
         grund TYPE afru-grund,
         isdd  TYPE afru-isdd,
         isdz  TYPE afru-isdz,
         iedd  TYPE afru-iedd,
         iedz  TYPE afru-iedz,
         pernr TYPE afru-pernr,
         ismnu TYPE afru-ismnu,
         tplnr TYPE viaufkst-tplnr,
       END OF y_afru.
*
TYPES: BEGIN OF y_pa0001,
         pernr TYPE pa0001-pernr,
         sname TYPE pa0001-sname,
       END OF y_pa0001.

TYPES: BEGIN OF y_eqkt,
         equnr TYPE eqkt-equnr,
         eqktx TYPE eqkt-eqktx,
       END OF y_eqkt.
*
TYPES: BEGIN OF y_crca,
         objid TYPE crca-objid,
         kapid TYPE crca-kapid,
         objty TYPE crca-objty,
       END OF y_crca.
*
TYPES: BEGIN OF y_crco,
         objty TYPE crco-objty,
         objid TYPE crco-objid,
         kostl TYPE crco-kostl,
       END OF y_crco.

TYPES: BEGIN OF y_crhd,
         werks TYPE crhd-werks,
         objid TYPE crhd-objid,
         arbpl TYPE crhd-arbpl,
         kapid TYPE crhd-kapid,
         verwe TYPE crhd-verwe,
       END OF y_crhd.
*
TYPES: BEGIN OF y_cskt,
         kostl TYPE cskt-kostl,
         ktext TYPE cskt-ktext,
       END OF y_cskt.
*
TYPES: BEGIN OF y_arbpl,
         arbpl TYPE crhd-arbpl,
       END OF y_arbpl.
*
TYPES: BEGIN OF y_kako,
         werks     TYPE kako-werks,
         kapid     TYPE kako-kapid,
         aznor     TYPE kako-aznor,
         begzt     TYPE kako-begzt,
         endzt     TYPE kako-endzt,
         ngrad     TYPE kako-ngrad,
         pause     TYPE kako-pause,
         ueberlast TYPE kako-ueberlast,
       END OF y_kako.
*
TYPES: BEGIN OF y_reg,
         arbpl  TYPE crhd-arbpl,
         werks  TYPE afru-werks,
         kostl  TYPE aufk-kostl,
         ktext  TYPE cskt-ktext,
         arbid  TYPE afru-arbid,
         hrsper TYPE p DECIMALS 2,
         hrsapo TYPE p DECIMALS 2,
         hrsdis TYPE p DECIMALS 2,
         tplnr  TYPE viaufkst-tplnr,
         icon   TYPE char15,
       END OF y_reg.

TYPES: BEGIN OF y_reg_2,
         arbpl  TYPE crhd-arbpl,
         werks  TYPE afru-werks,
         kostl  TYPE aufk-kostl,
         ktext  TYPE cskt-ktext,
         arbid  TYPE afru-arbid,
         hrsper TYPE p DECIMALS 2,
         hrsapo TYPE p DECIMALS 2,
         hrsdis TYPE p DECIMALS 2,
         tplnr  TYPE viaufkst-tplnr,
       END OF y_reg_2.

TYPES: BEGIN OF y_reg_3,
         arbpl  TYPE crhd-arbpl,
         werks  TYPE afru-werks,
         kostl  TYPE aufk-kostl,
         ktext  TYPE cskt-ktext,
         arbid  TYPE afru-arbid,
         hrsper TYPE p DECIMALS 2,
         hrsapo TYPE p DECIMALS 2,
         hrsdis TYPE p DECIMALS 2,
         tplnr  TYPE viaufkst-tplnr,
       END OF y_reg_3.

TYPES: BEGIN OF ty_saida_2,
         rueck     TYPE afru-rueck,
         rmzhl     TYPE afru-rmzhl,
         ltxa1     TYPE afru-ltxa1,
         ersda(10) TYPE c,
         budat(10) TYPE c,
         aenam     TYPE afru-aenam,
         aufnr     TYPE afru-aufnr,
         vornr     TYPE afru-vornr,
         werks     TYPE afru-werks,
         arbid     TYPE afru-arbid,
         ismnw     TYPE p DECIMALS 2,
         ismnu     TYPE afru-ismnu,
         ofmnw     TYPE afru-ofmnw,
         arbpl     TYPE crhd-arbpl,
         ismne     TYPE afru-ismne,
         idaur     TYPE afru-idaur,
         anzma     TYPE afru-anzma,
         grund     TYPE afru-grund,
         pernr     TYPE afru-pernr,
         sname     TYPE pa0001-sname,
         kostl     TYPE aufk-kostl,
         tplnr     TYPE viaufkst-tplnr,
         ktext     TYPE cskt-ktext,
         pltxt     TYPE iflo-pltxt,
         auart     TYPE aufk-auart,
         isdd(10)  TYPE c,
         isdz(10)  TYPE c,
         iedd(10)  TYPE c,
         iedz(10)  TYPE c,
       END OF ty_saida_2.

TYPES: BEGIN OF ty_saida_3,
         werks TYPE afru-werks,
         kostl TYPE aufk-kostl,
         ktext TYPE cskt-ktext,
         tplma TYPE iflo-tplma,
         tplnr TYPE viaufkst-tplnr,
         pltxt TYPE iflo-pltxt,
         equnr TYPE viaufkst-equnr,
         eqktx TYPE eqkt-eqktx,
         aufnr TYPE afru-aufnr,
         arbid TYPE afru-arbid,
         arbpl TYPE crhd-arbpl,
         ismnw TYPE p DECIMALS 2,
         ismnu TYPE afru-ismnu,
         vaplz TYPE aufk-vaplz,
       END OF ty_saida_3.

TYPES: BEGIN OF ty_saida_4,
         rueck     TYPE afru-rueck,
         rmzhl     TYPE afru-rmzhl,
         ltxa1     TYPE afru-ltxa1,
         ersda(10) TYPE c,
         budat(10) TYPE c,
         aenam     TYPE afru-aenam,
         aufnr     TYPE afru-aufnr,
         vornr     TYPE afru-vornr,
         werks     TYPE afru-werks,
         arbid     TYPE afru-arbid,
         ismnw     TYPE p DECIMALS 2,
         ofmnw     TYPE afru-ofmnw,
         arbpl     TYPE crhd-arbpl,
         ismne     TYPE afru-ismne,
         idaur     TYPE afru-idaur,
         anzma     TYPE afru-anzma,
         grund     TYPE afru-grund,
         pernr     TYPE afru-pernr,
         sname     TYPE pa0001-sname,
         kostl     TYPE aufk-kostl,
         tplnr     TYPE viaufkst-tplnr,
         ktext     TYPE cskt-ktext,
         pltxt     TYPE iflo-pltxt,
         auart     TYPE aufk-auart,
         ismnu     TYPE afru-ismnu,
         isdd(10)  TYPE c,
         isdz(10)  TYPE c,
         iedd(10)  TYPE c,
         iedz(10)  TYPE c,
       END OF ty_saida_4.
*
*-----------------------------------------------------------------------------*
** DECLARAÇÃO DE TABELAS alv                                                  **
*-----------------------------------------------------------------------------*
*DATA:
*  T_LAYOUT     TYPE SLIS_LAYOUT_ALV     OCCURS 0 WITH HEADER LINE,
*  T_LISTHEADER TYPE SLIS_T_LISTHEADER,
*  T_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.
*-----------------------------------------------------------------------------*
*-> Tabelas internas
*-----------------------------------------------------------------------------*
DATA: it_aufk      TYPE TABLE OF y_aufk,
      wa_aufk      TYPE y_aufk,
      it_afru      TYPE TABLE OF y_afru,
      it_pa0001    TYPE TABLE OF y_pa0001,
      wa_pa0001    TYPE y_pa0001,
      it_crca      TYPE TABLE OF y_crca,
      wa_crca      TYPE y_crca,
      it_crco      TYPE TABLE OF y_crco,
      wa_crco      TYPE y_crco,
      it_viaufkst  TYPE TABLE OF y_viaufkst WITH HEADER LINE,
      wa_viaufkst  TYPE y_viaufkst,
      it_viaufkst_ TYPE TABLE OF y_viaufkst_ WITH HEADER LINE,
      wa_viaufkst_ TYPE y_viaufkst_,
      it_iflo      TYPE TABLE OF y_iflo,
      wa_iflo      TYPE y_iflo,
      it_crhd      TYPE TABLE OF y_crhd,
      wa_crhd      TYPE y_crhd,
      it_kako      TYPE TABLE OF y_kako,
      wa_kako      TYPE y_kako,
      it_cskt      TYPE TABLE OF y_cskt,
      wa_cskt      TYPE y_cskt,
      it_arbpl     TYPE TABLE OF y_arbpl WITH HEADER LINE,
      it_return    TYPE TABLE OF ddshretval,
      wa_return    LIKE LINE OF  it_return,
      it_reg       TYPE TABLE OF y_reg,
      it_saida_2   TYPE TABLE OF ty_saida_2,
      wa_saida_2   TYPE ty_saida_2,
      it_saida_3   TYPE TABLE OF ty_saida_3,
      wa_saida_3   TYPE ty_saida_3,
      it_saida_4   TYPE TABLE OF ty_saida_4,
      wa_saida_4   TYPE ty_saida_4,
      wa_afru      TYPE y_afru,
      wa_reg       TYPE y_reg,
      it_eqkt      TYPE TABLE OF y_eqkt,
      wa_eqkt      TYPE y_eqkt,
      it_reg_2     TYPE TABLE OF y_reg_2,
      wa_reg_2     TYPE y_reg_2,
      it_reg_3     TYPE TABLE OF y_reg_3,
      wa_reg_3     TYPE y_reg_3.


*-----------------------------------------------------------------------------*
** DECLARAÇÃO DE VARIÁVEIS                                                    **
*-----------------------------------------------------------------------------*
DATA:
  cap_total   TYPE p DECIMALS 2,
  temp_total  TYPE p DECIMALS 2,
  temp_trab   TYPE p DECIMALS 2,
  gra_ut      TYPE p DECIMALS 2,
  temp_utiz   TYPE p DECIMALS 2,
  lv_segundos TYPE p DECIMALS 2,
  lv_date1    TYPE p,
  lv_date2    TYPE p,
  lv_tabix    TYPE sy-tabix.


CONSTANTS: lc_u(1)         VALUE 'U',
           lc_a(1)         VALUE 'A',
           lc_1(1)         VALUE '1',
           lc_nops(4)      VALUE 'NOPS',
           lc_no_predit(9) VALUE 'NO_PREDIT'.


DATA: p_selecao TYPE i.

*----------------------------------------------------*
*                ALV GRID                            *
*----------------------------------------------------*

DATA:
  "IT_SELECT           TYPE STANDARD TABLE OF TY_DADOS_IMOB,
  g_custom_container TYPE REF TO cl_gui_custom_container,
  g_container        TYPE REF TO cl_gui_custom_container,
  g_container_3      TYPE REF TO cl_gui_custom_container,
  dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
  dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
  dg_parent_1        TYPE REF TO cl_gui_container,
  dg_parent_2        TYPE REF TO cl_gui_container,
  dg_parent_3        TYPE REF TO cl_gui_container,
  dg_parent_4        TYPE REF TO cl_gui_container,
  dg_splitter_3      TYPE REF TO cl_gui_splitter_container,
  dg_splitter_4      TYPE REF TO cl_gui_splitter_container,
  dg_parent_2a       TYPE REF TO cl_gui_container,
  dg_parent_2a_2     TYPE REF TO cl_gui_container,
  dg_parent_alv      TYPE REF TO cl_gui_container,
  dg_parent_alv_2    TYPE REF TO cl_gui_container,
  picture            TYPE REF TO cl_gui_picture,
  picture_2          TYPE REF TO cl_gui_picture,
  ctl_alv            TYPE REF TO cl_gui_alv_grid,
  ctl_alv_2          TYPE REF TO cl_gui_alv_grid,
  ctl_alv_3          TYPE REF TO cl_gui_alv_grid,
  ctl_alv_4          TYPE REF TO cl_gui_alv_grid,
  dg_dyndoc_id       TYPE REF TO cl_dd_document,
  dg_dyndoc_id_2     TYPE REF TO cl_dd_document,
  table_element      TYPE REF TO cl_dd_table_element,
  table_element_2    TYPE REF TO cl_dd_table_element,
  column             TYPE REF TO cl_dd_area,
  column_2           TYPE REF TO cl_dd_area,
  table_element2     TYPE REF TO cl_dd_table_element,
  table_element3     TYPE REF TO cl_dd_table_element,
  column_1           TYPE REF TO cl_dd_area,
  column_3           TYPE REF TO cl_dd_area,
  dg_html_cntrl      TYPE REF TO cl_gui_html_viewer,
  dg_html_cntrl_2    TYPE REF TO cl_gui_html_viewer,
  it_exclude_fcode   TYPE ui_functions,
  wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
  it_exclude_fcode_2 TYPE ui_functions,
  it_exclude_fcode_3 TYPE ui_functions,
  wa_exclude_fcode_2 LIKE LINE OF it_exclude_fcode,
  wa_exclude_fcode_3 LIKE LINE OF it_exclude_fcode,
  gs_layout          TYPE lvc_s_layo,
  gs_layout_2        TYPE lvc_s_layo,
  gs_layout_3        TYPE lvc_s_layo,
  gs_variant         TYPE disvariant,
  gs_variant_2       TYPE disvariant,
  gs_variant_3       TYPE disvariant,
  it_fieldcatalog    TYPE lvc_t_fcat,
  it_fieldcatalog_2  TYPE lvc_t_fcat,
  it_fieldcatalog_3  TYPE lvc_t_fcat,
  wa_fieldcatalog    TYPE lvc_s_fcat,
  wa_fieldcatalog_2  TYPE lvc_s_fcat,
  wa_fieldcatalog_3  TYPE lvc_s_fcat,
  it_sort            TYPE lvc_t_sort,
  it_sort_           TYPE lvc_t_sort,
  it_sort_3          TYPE lvc_t_sort,
  "GS_SCROLL_COL       TYPE LVC_S_COL,
  "GS_SCROLL_ROW       TYPE LVC_S_ROID,
  "GS_STABLE           TYPE LVC_S_STBL,
  "IT_SELECTED_ROWS    TYPE LVC_T_ROW,
  "WA_SELECTED_ROWS    TYPE LVC_S_ROW,
  ls_stable          TYPE lvc_s_stbl,
  ls_stable_2        TYPE lvc_s_stbl,
  ls_stable_3        TYPE lvc_s_stbl,
  t_sort             TYPE lvc_t_sort,
  w_sort             TYPE lvc_t_sort WITH HEADER LINE,
  t_out_1            TYPE TABLE OF y_reg,
  t_fcat_1           TYPE TABLE OF lvc_s_fcat,
  w_fcat_1           TYPE lvc_s_fcat.

DATA: setor(30) TYPE c.


CLASS lcl_dados DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      generate_it_saida_3 IMPORTING i_werks TYPE werks_d i_arbid TYPE afru-arbid,
      generate_it_saida_4 IMPORTING i_werks TYPE werks_d i_arbid TYPE afru-arbid.
ENDCLASS.

CLASS lcl_dados IMPLEMENTATION.
  METHOD generate_it_saida_3.
    FREE: it_saida_3.
    CLEAR  wa_saida_3.
    LOOP AT it_afru INTO wa_afru WHERE werks = i_werks AND arbid = i_arbid.

      wa_saida_3-werks = wa_afru-werks.
*          WA_SAIDA_3-AUFNR = |{ WA_AFRU-AUFNR ALPHA = OUT }|.
      wa_saida_3-arbid = wa_afru-arbid.

      IF wa_afru-ismne = 'MIN'.
        wa_afru-ismnw = ( wa_afru-ismnw / 60 ).
      ENDIF.

      wa_saida_3-ismnw = wa_afru-ismnw.
      wa_saida_3-ismnu = 'H'. "WA_AFRU-ISMNU.

      READ TABLE it_crhd INTO wa_crhd WITH KEY objid = wa_afru-arbid.
      IF sy-subrc = 0.
        wa_saida_3-arbpl = wa_crhd-arbpl.
      ENDIF.

      READ TABLE it_aufk INTO wa_aufk WITH KEY aufnr = wa_afru-aufnr.
      IF sy-subrc = 0.
        wa_saida_3-kostl = wa_aufk-kostl.
      ENDIF.

      READ TABLE it_viaufkst INTO wa_viaufkst WITH KEY aufnr = wa_aufk-aufnr.
      IF sy-subrc = 0.
*            WA_SAIDA_3-EQUNR = |{ WA_VIAUFKST-EQUNR ALPHA = OUT }|.
        wa_saida_3-tplnr = wa_viaufkst-tplnr.
      ENDIF.

      READ TABLE it_iflo INTO wa_iflo WITH KEY tplnr = wa_viaufkst-tplnr.
      IF sy-subrc = 0.
        wa_saida_3-pltxt = wa_iflo-pltxt.
*            WA_SAIDA_3-TPLMA = WA_IFLO-TPLMA.
      ENDIF.

      READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_aufk-kostl.
      IF sy-subrc = 0.
        wa_saida_3-ktext = wa_cskt-ktext.
      ENDIF.

      READ TABLE it_eqkt INTO wa_eqkt WITH KEY equnr = wa_viaufkst-equnr.
      IF sy-subrc = 0.
*            WA_SAIDA_3-EQKTX = WA_EQKT-EQKTX.
      ENDIF.

      COLLECT wa_saida_3 INTO it_saida_3.
*          APPEND WA_SAIDA_3 TO IT_SAIDA_3.
      CLEAR  wa_reg.
      CLEAR  wa_saida_3.
      CLEAR  wa_aufk.
      CLEAR  wa_cskt.
      CLEAR  wa_iflo.
      CLEAR  wa_viaufkst.
      CLEAR  wa_crhd.
      CLEAR  wa_eqkt.
    ENDLOOP.
  ENDMETHOD.
  METHOD generate_it_saida_4.
    FREE: it_saida_4.
    CLEAR: it_saida_4.
    LOOP AT it_afru INTO wa_afru WHERE werks = i_werks AND arbid = i_arbid.
      wa_saida_4-pernr  =   wa_afru-pernr.
      wa_saida_4-rueck  =   |{ wa_afru-rueck ALPHA = OUT }|.
      wa_saida_4-rmzhl  =   |{ wa_afru-rmzhl ALPHA = OUT }|.
      wa_saida_4-ltxa1  =   wa_afru-ltxa1.
      wa_saida_4-ersda  =   wa_afru-ersda.
      wa_saida_4-budat  =   wa_afru-budat.
      wa_saida_4-aenam  =   wa_afru-aenam.
      wa_saida_4-aufnr  =   |{ wa_afru-aufnr ALPHA = OUT }|.
      wa_saida_4-vornr  =   wa_afru-vornr.
      wa_saida_4-werks  =   wa_afru-werks.
      wa_saida_4-arbid  =   wa_afru-arbid.

      IF wa_afru-ismne = 'MIN'.
        wa_afru-ismnw = ( wa_afru-ismnw / 60 ).
      ENDIF.

      wa_saida_4-ismnw   =  wa_afru-ismnw.

      wa_saida_4-ismnu   =  'H'. "WA_AFRU-ISMNU'.
      wa_saida_4-ofmnw    = wa_afru-ofmnw.
      wa_saida_4-ismne  =   wa_afru-ismne.
      wa_saida_4-idaur  =   wa_afru-idaur.
      wa_saida_4-anzma  =   wa_afru-anzma.
      wa_saida_4-grund  =   wa_afru-grund.
      wa_saida_4-isdd   =   wa_afru-isdd.
      wa_saida_4-isdz   =   wa_afru-isdz.
      wa_saida_4-iedd   =   wa_afru-iedd.
      wa_saida_4-iedz   =   wa_afru-iedz.

      READ TABLE it_crhd INTO wa_crhd WITH KEY objid = wa_afru-arbid.
      IF sy-subrc = 0.
        wa_saida_4-arbpl  =   wa_crhd-arbpl.
      ENDIF.

      READ TABLE it_crca INTO wa_crca WITH KEY objid = wa_afru-arbid.
      IF sy-subrc = 0.
      ENDIF.

      READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_afru-pernr.
      IF sy-subrc = 0.
        wa_saida_4-sname = wa_pa0001-sname.
      ENDIF.


      READ TABLE it_aufk INTO wa_aufk WITH KEY aufnr = wa_afru-aufnr.
      IF sy-subrc = 0.
        wa_saida_4-kostl  = wa_aufk-kostl.
        wa_saida_4-auart  = wa_aufk-auart.
      ENDIF.

      READ TABLE it_viaufkst INTO wa_viaufkst WITH KEY aufnr = wa_aufk-aufnr.
      IF sy-subrc = 0.
        wa_saida_4-tplnr  =   wa_viaufkst-tplnr.
      ENDIF.


      READ TABLE it_iflo INTO wa_iflo WITH KEY tplnr = wa_viaufkst-tplnr.
      IF sy-subrc = 0.
        wa_saida_4-pltxt = wa_iflo-pltxt.
*            WA_SAIDA_3-TPLMA = WA_IFLO-TPLMA.
      ENDIF.

      READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_aufk-kostl.
      IF sy-subrc = 0.
        wa_saida_4-ktext = wa_cskt-ktext.
      ENDIF.

      APPEND wa_saida_4 TO it_saida_4.
      CLEAR  wa_reg.
      CLEAR  wa_saida_4.
      CLEAR  wa_aufk.
      CLEAR  wa_crca.
      CLEAR  wa_crhd.
      CLEAR  wa_afru.
      CLEAR  wa_pa0001.
      CLEAR  wa_saida_3.
      CLEAR  wa_viaufkst.
      CLEAR  wa_cskt.
      CLEAR  wa_iflo.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------*
*               EVENTOS              *
*----------------------------------------------------*


DATA: "R_EVENT_HANDLER TYPE REF TO LCL_EVENTS_HANDLER,
  i_selected_rows TYPE lvc_t_row,                "Linhas selecionadas
  w_selected_rows TYPE lvc_s_row.                "Colunas Selecionadas

CLASS lcl_eventos DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.
ENDCLASS.

CLASS lcl_eventos_2 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

ENDCLASS.

CLASS lcl_eventos_3 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

ENDCLASS.

*-----------------------------------------------------------------------------*
*  Radio Button 146081 CS2023000568 Visão análitica ZPM0036 PSA
*-----------------------------------------------------------------------------*

CONSTANTS : rbSelected TYPE c LENGTH 1 VALUE 'X'.

DATA : p_txt TYPE c LENGTH 100.

INCLUDE zpm_disp_horas_centro_anaclass.

*-----------------------------------------------------------------------------*
*  Tela de seleção
*-----------------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
         s_werks FOR kako-werks OBLIGATORY,   "Centro Planejamento
         s_arbpl FOR viaufkst-vaplz MATCHCODE OBJECT cram,  " Centro de trabalho
         s_kostl FOR crco-kostl,   "CENTRO DE CUSTO
         s_verwe FOR crhd-verwe DEFAULT '0005',
         s_date  FOR afru-isdd NO-EXTENSION OBLIGATORY DEFAULT sy-datum TO sy-datum.

  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-200. "146081 CS2023000568 Visão análitica ZPM0036 PSA
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 15.
      PARAMETERS: rb1 RADIOBUTTON GROUP rb.
      SELECTION-SCREEN COMMENT 20(30) TEXT-201.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 15.
      PARAMETERS: rb2 RADIOBUTTON GROUP rb.
      SELECTION-SCREEN COMMENT 20(30) TEXT-202.
    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN:END OF BLOCK b2.
SELECTION-SCREEN:END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT. "146081 CS2023000568 Visão análitica ZPM0036 PSA


*-----------------------------------------------------------------------------*
*   Processamento
*-----------------------------------------------------------------------------*
START-OF-SELECTION.

"146081 CS2023000568 Visão análitica ZPM0036 PSA
dtstart = |{ s_date-low+6(2) }/{ s_date-low+4(2) }/{ s_date-low+0(4) }|.
dtend = |{ s_date-high+6(2) }/{ s_date-high+4(2) }/{ s_date-high+0(4) }|.


  IF rb1 = rbSelected.
    p_selecao = 1.
  ELSEIF rb2 = rbSelected.
    p_selecao = 2.
  ENDIF.

  PERFORM zf_seleciona.
  PERFORM zf_saida.

  IF it_reg IS NOT INITIAL.
    CASE p_selecao. "146081 CS2023000568 Visão análitica ZPM0036 PSA
      WHEN 1.
        PERFORM zf_execut_alv.
      WHEN 2.
        CALL SELECTION-SCREEN '0400'.
      WHEN OTHERS.
    ENDCASE.
  ELSE.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.


* Seleção


*-----------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_seleciona .

  IF s_kostl IS NOT INITIAL.
    " Selecionando planejamento de horas trabalhada / Capacidades.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_kako
       FROM kako
       WHERE werks IN s_werks.
    SORT it_kako ASCENDING BY kapid.

    SELECT *
   INTO CORRESPONDING FIELDS OF TABLE it_crca
   FROM crca
   FOR ALL ENTRIES IN it_kako
   WHERE kapid EQ it_kako-kapid
     AND objty EQ lc_a.
    SORT it_crca ASCENDING BY objid.

    SELECT *
   INTO CORRESPONDING FIELDS OF TABLE it_crco
   FROM crco
   FOR ALL ENTRIES IN it_crca
   WHERE objid EQ it_crca-objid
     AND objty EQ lc_a
     AND kostl IN s_kostl.
    SORT it_crco ASCENDING BY objid.

    IF it_crco IS NOT INITIAL.
      SELECT *
       INTO CORRESPONDING FIELDS OF TABLE it_crhd
       FROM crhd FOR ALL ENTRIES IN it_crco
        WHERE objid EQ it_crco-objid
       AND   werks IN s_werks
       AND   arbpl IN s_arbpl
       AND   verwe IN s_verwe
       AND   objty EQ lc_a.
      SORT it_crhd ASCENDING BY objid.
    ENDIF.

  ELSE.

    "Selecionando informações tabelas centro de trabalho.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_crhd
      FROM crhd
      WHERE werks IN s_werks
      AND verwe IN s_verwe
      AND arbpl IN s_arbpl
      AND objty EQ lc_a.
    SORT it_crhd ASCENDING BY objid.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_crco
      FROM crco
      FOR ALL ENTRIES IN it_crhd
      WHERE objid EQ it_crhd-objid
        AND objty EQ lc_a
        AND kostl IN s_kostl.
    SORT it_crco ASCENDING BY objid.

    SELECT *
   INTO CORRESPONDING FIELDS OF TABLE it_crca
   FROM crca
   FOR ALL ENTRIES IN it_crco
   WHERE objid EQ it_crco-objid
     AND objty EQ lc_a.
    SORT it_crca ASCENDING BY objid.

    " Selecionando planejamento de horas trabalhada / Capacidades.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_kako
       FROM kako FOR ALL ENTRIES IN it_crca
      WHERE kapid EQ it_crca-kapid
        AND werks IN s_werks.
    SORT it_kako ASCENDING BY kapid.

  ENDIF.

  IF it_crco IS NOT INITIAL.
    "Selecionando apontamento de horas / Capacidades.
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_afru
    FROM afru
    FOR ALL ENTRIES IN it_crco
    WHERE arbid EQ it_crco-objid
    AND werks IN s_werks
    AND isdd  IN s_date
     AND stokz EQ ' '
     AND stzhl EQ 0.
    SORT it_afru ASCENDING BY werks aufnr.

    "Deletar apontamento zerados.
    DELETE it_afru WHERE ismnw EQ 0.

    "Selecioando informações mão de obra.
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_pa0001
    FROM pa0001
    FOR ALL ENTRIES IN it_afru
    WHERE pernr EQ it_afru-pernr.

    "Seleção centor de custo da ordem de manutenção utilizada para apontamento / Capacidade.
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_aufk
    FROM aufk
    FOR ALL ENTRIES IN it_afru
    WHERE aufnr EQ it_afru-aufnr
      AND werks EQ it_afru-werks
*    AND KOSTL IN S_KOSTL
      AND autyp EQ '30'.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_cskt
       FROM cskt FOR ALL ENTRIES IN it_aufk
      WHERE kostl EQ it_aufk-kostl.
    SORT it_cskt ASCENDING BY kostl.

    " Seleção local de instalação / equipamento
    FREE: it_viaufkst[].
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_viaufkst_
    FROM viaufkst
    FOR ALL ENTRIES IN it_aufk
    WHERE aufnr EQ it_aufk-aufnr.
*    AND KOSTL IN S_KOSTL.

    IF it_viaufkst_[] IS NOT INITIAL.
      LOOP AT it_viaufkst_.
        setor = |{ it_viaufkst_-tplnr(18) }|.
        it_viaufkst_-tplnr = setor.
        it_viaufkst-werks = it_viaufkst_-werks.
        it_viaufkst-tplnr = it_viaufkst_-tplnr.
        it_viaufkst-equnr = it_viaufkst_-equnr.
        it_viaufkst-aufnr = it_viaufkst_-aufnr.
        APPEND it_viaufkst.
      ENDLOOP.
    ENDIF.

    "Seleção da descrição do local instalação.
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_iflo
    FROM iflo
    FOR ALL ENTRIES IN it_viaufkst
    WHERE tplnr EQ it_viaufkst-tplnr.

    "Seleção da descrição do equipamento.
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_eqkt
    FROM eqkt
    FOR ALL ENTRIES IN it_viaufkst
    WHERE equnr EQ it_viaufkst-equnr.
  ENDIF.

ENDFORM.

FORM zf_saida.
  DATA: var TYPE p DECIMALS 2.
*  SORT IT_AFRU BY WERKS.
*  SORT IT_CRHD BY ARBPL.
*  SORT IT_AUFK BY KOSTL.

*  FREE: IT_REG.
  lv_date1 = s_date-low.

  IF s_date-high IS INITIAL OR s_date-high = '00000000'. "146081 CS2023000568 Visão análitica ZPM0036 PSA
    lv_date2 = s_date-low.
  ELSE.
    lv_date2 = s_date-high.
  ENDIF.

  lv_date1 = lv_date2 - lv_date1 + 1.


  LOOP AT it_crhd INTO wa_crhd.
    wa_reg_2-werks = wa_crhd-werks.
    wa_reg_2-arbpl = wa_crhd-arbpl.

    READ TABLE it_crco INTO wa_crco WITH KEY objid = wa_crhd-objid.
    IF sy-subrc = 0.
    ENDIF.

    READ TABLE it_crca INTO wa_crca WITH KEY objid = wa_crco-objid.
    IF sy-subrc = 0.
    ENDIF.

    READ TABLE it_kako INTO wa_kako WITH KEY kapid = wa_crca-kapid.
    IF sy-subrc = 0.
*      CALCULO DE HORAS
      temp_trab     = wa_kako-endzt - wa_kako-begzt. "Horas Periodo
      temp_utiz     = ( temp_trab - wa_kako-pause ). " Menos pausa.
      temp_utiz     = ( temp_utiz / 60 ) / 60. " Convertendo em hora.
      gra_ut        = ( temp_utiz * wa_kako-ngrad ) / 100. "Percentual
      cap_total     = gra_ut * wa_kako-aznor.
      temp_total    = cap_total *  lv_date1.
      wa_reg_2-hrsper = temp_total.
    ENDIF.

    APPEND wa_reg_2 TO it_reg_2.
    CLEAR wa_afru.
    CLEAR wa_reg_2.
    CLEAR wa_kako.
    CLEAR wa_crca.
    CLEAR wa_crco.
    CLEAR wa_crhd.
    CLEAR temp_trab.
    CLEAR temp_utiz.
    CLEAR gra_ut   .
    CLEAR cap_total.
    CLEAR temp_total.
    CLEAR var.
  ENDLOOP.

  LOOP AT it_afru INTO wa_afru.

    IF wa_afru-ismne = 'MIN'.
      wa_afru-ismnw = ( wa_afru-ismnw / 60 ).
    ENDIF.

    wa_reg_3-hrsapo = wa_afru-ismnw.        "Horas Apontadas
    wa_reg_3-arbid  = wa_afru-arbid.

    READ TABLE it_crhd INTO wa_crhd WITH KEY objid = wa_afru-arbid.
    IF sy-subrc = 0.
      wa_reg_3-werks = wa_crhd-werks.
      wa_reg_3-arbpl = wa_crhd-arbpl.
    ENDIF.
    COLLECT wa_reg_3 INTO it_reg_3.
    CLEAR wa_reg_3.
    CLEAR wa_afru.
  ENDLOOP.


  LOOP AT it_reg_2 INTO wa_reg_2.

    wa_reg-werks  = wa_reg_2-werks.
    wa_reg-arbpl  = wa_reg_2-arbpl.
    wa_reg-hrsper = wa_reg_2-hrsper.

    READ TABLE it_reg_3 INTO wa_reg_3 WITH KEY werks = wa_reg_2-werks
                                               arbpl = wa_reg_2-arbpl.
    IF sy-subrc = 0.
      wa_reg-arbid  = wa_reg_3-arbid.
      wa_reg-hrsapo = wa_reg_3-hrsapo.
    ENDIF.

    wa_reg-hrsdis = ( wa_reg-hrsper - wa_reg-hrsapo ).
    var = wa_reg-hrsdis.

    IF wa_reg-hrsdis > 0.
      wa_reg-icon   = icon_green_light.
    ELSE.
      wa_reg-icon   = icon_red_light.
    ENDIF.

    APPEND wa_reg TO it_reg.
    CLEAR wa_reg.
    CLEAR wa_reg_2.
    CLEAR wa_reg_3.
  ENDLOOP.
*  ENDLOOP.

  SORT it_reg  ASCENDING BY  werks arbpl.

ENDFORM.

CLASS lcl_eventos IMPLEMENTATION.
  METHOD on_hotspot_click.

    "Evento 01

    DATA: setor(30) TYPE c.

    CLEAR wa_reg.

    FREE it_saida_3.
    READ TABLE it_reg INTO wa_reg INDEX e_row_id-index.
    CASE e_column_id-fieldname.
      WHEN:'HRSAPO'.
        IF wa_reg-hrsapo IS INITIAL.
          EXIT.
        ENDIF.

        "146081 CS2023000568 Visão análitica ZPM0036 PSA
        CALL METHOD lcl_dados=>generate_it_saida_3
          EXPORTING
            i_werks = wa_reg-werks
            i_arbid = wa_reg-arbid.

        "146081 CS2023000568 Visão análitica ZPM0036 PSA -> foi colocado esse script dentro da classe generate_it_saida_3
*        LOOP AT it_afru INTO wa_afru WHERE werks = wa_reg-werks AND arbid = wa_reg-arbid.
*
*          wa_saida_3-werks = wa_afru-werks.
**          WA_SAIDA_3-AUFNR = |{ WA_AFRU-AUFNR ALPHA = OUT }|.
*          wa_saida_3-arbid = wa_afru-arbid.
*
*          IF wa_afru-ismne = 'MIN'.
*            wa_afru-ismnw = ( wa_afru-ismnw / 60 ).
*          ENDIF.
*
*          wa_saida_3-ismnw = wa_afru-ismnw.
*          wa_saida_3-ismnu = 'H'. "WA_AFRU-ISMNU.
*
*          READ TABLE it_crhd INTO wa_crhd WITH KEY objid = wa_afru-arbid.
*          IF sy-subrc = 0.
*            wa_saida_3-arbpl = wa_crhd-arbpl.
*          ENDIF.
*
*          READ TABLE it_aufk INTO wa_aufk WITH KEY aufnr = wa_afru-aufnr.
*          IF sy-subrc = 0.
*            wa_saida_3-kostl = wa_aufk-kostl.
*          ENDIF.
*
*          READ TABLE it_viaufkst INTO wa_viaufkst WITH KEY aufnr = wa_aufk-aufnr.
*          IF sy-subrc = 0.
**            WA_SAIDA_3-EQUNR = |{ WA_VIAUFKST-EQUNR ALPHA = OUT }|.
*            wa_saida_3-tplnr = wa_viaufkst-tplnr.
*          ENDIF.
*
*          READ TABLE it_iflo INTO wa_iflo WITH KEY tplnr = wa_viaufkst-tplnr.
*          IF sy-subrc = 0.
*            wa_saida_3-pltxt = wa_iflo-pltxt.
**            WA_SAIDA_3-TPLMA = WA_IFLO-TPLMA.
*          ENDIF.
*
*          READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_aufk-kostl.
*          IF sy-subrc = 0.
*            wa_saida_3-ktext = wa_cskt-ktext.
*          ENDIF.
*
*          READ TABLE it_eqkt INTO wa_eqkt WITH KEY equnr = wa_viaufkst-equnr.
*          IF sy-subrc = 0.
**            WA_SAIDA_3-EQKTX = WA_EQKT-EQKTX.
*          ENDIF.
*
*          COLLECT wa_saida_3 INTO it_saida_3.
**          APPEND WA_SAIDA_3 TO IT_SAIDA_3.
*          CLEAR  wa_reg.
*          CLEAR  wa_saida_3.
*          CLEAR  wa_aufk.
*          CLEAR  wa_cskt.
*          CLEAR  wa_iflo.
*          CLEAR  wa_viaufkst.
*          CLEAR  wa_crhd.
*          CLEAR  wa_eqkt.
*        ENDLOOP.

        SORT it_saida_3 ASCENDING BY  werks aufnr.

        CALL SCREEN 300.

      WHEN:'ARBPL'."Ordem Manuteção
        IF wa_reg-arbpl IS NOT INITIAL AND wa_reg-werks IS NOT INITIAL.

          SET PARAMETER ID 'WRK' FIELD wa_reg-werks.
          SET PARAMETER ID 'AGR' FIELD wa_reg-arbpl.
          CALL TRANSACTION 'IR03' AND SKIP FIRST SCREEN .
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_eventos_2 IMPLEMENTATION.
  METHOD on_hotspot_click.
    "Evento 02

    CLEAR wa_saida_2. "Limpando work area
    CLEAR wa_saida_4. "Limpando work area
    FREE  it_saida_2. "Limpando tabela
    FREE  it_saida_4. "Limpando tabela

    READ TABLE it_saida_3 INTO wa_saida_3 INDEX e_row_id-index.
    CASE e_column_id-fieldname.
*
      WHEN:'ISMNW'.
        IF wa_saida_3-ismnw IS INITIAL.
          EXIT.
        ENDIF.

        "146081 CS2023000568 Visão análitica ZPM0036 PSA
        CALL METHOD lcl_dados=>generate_it_saida_4
          EXPORTING
            i_werks = wa_saida_3-werks
            i_arbid = wa_saida_3-arbid.

*        LOOP AT it_afru INTO wa_afru WHERE werks = wa_saida_3-werks AND arbid = wa_saida_3-arbid.
*          wa_saida_4-pernr  =   wa_afru-pernr.
*          wa_saida_4-rueck  =   |{ wa_afru-rueck ALPHA = OUT }|.
*          wa_saida_4-rmzhl  =   |{ wa_afru-rmzhl ALPHA = OUT }|.
*          wa_saida_4-ltxa1  =   wa_afru-ltxa1.
*          wa_saida_4-ersda  =   wa_afru-ersda.
*          wa_saida_4-budat  =   wa_afru-budat.
*          wa_saida_4-aenam  =   wa_afru-aenam.
*          wa_saida_4-aufnr  =   |{ wa_afru-aufnr ALPHA = OUT }|.
*          wa_saida_4-vornr  =   wa_afru-vornr.
*          wa_saida_4-werks  =   wa_afru-werks.
*          wa_saida_4-arbid  =   wa_afru-arbid.
*
*          IF wa_afru-ismne = 'MIN'.
*            wa_afru-ismnw = ( wa_afru-ismnw / 60 ).
*          ENDIF.
*
*          wa_saida_4-ismnw   =  wa_afru-ismnw.
*
*          wa_saida_4-ismnu   =  'H'. "WA_AFRU-ISMNU'.
*          wa_saida_4-ofmnw    = wa_afru-ofmnw.
*          wa_saida_4-ismne  =   wa_afru-ismne.
*          wa_saida_4-idaur  =   wa_afru-idaur.
*          wa_saida_4-anzma  =   wa_afru-anzma.
*          wa_saida_4-grund  =   wa_afru-grund.
*          wa_saida_4-isdd   =   wa_afru-isdd.
*          wa_saida_4-isdz   =   wa_afru-isdz.
*          wa_saida_4-iedd   =   wa_afru-iedd.
*          wa_saida_4-iedz   =   wa_afru-iedz.
*
*          READ TABLE it_crhd INTO wa_crhd WITH KEY objid = wa_afru-arbid.
*          IF sy-subrc = 0.
*            wa_saida_4-arbpl  =   wa_crhd-arbpl.
*          ENDIF.
*
*          READ TABLE it_crca INTO wa_crca WITH KEY objid = wa_afru-arbid.
*          IF sy-subrc = 0.
*          ENDIF.
*
*          READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_afru-pernr.
*          IF sy-subrc = 0.
*            wa_saida_4-sname = wa_pa0001-sname.
*          ENDIF.
*
*
*          READ TABLE it_aufk INTO wa_aufk WITH KEY aufnr = wa_afru-aufnr.
*          IF sy-subrc = 0.
*            wa_saida_4-kostl  = wa_aufk-kostl.
*            wa_saida_4-auart  = wa_aufk-auart.
*          ENDIF.
*
*          READ TABLE it_viaufkst INTO wa_viaufkst WITH KEY aufnr = wa_aufk-aufnr.
*          IF sy-subrc = 0.
*            wa_saida_4-tplnr  =   wa_viaufkst-tplnr.
*          ENDIF.
*
*
*          READ TABLE it_iflo INTO wa_iflo WITH KEY tplnr = wa_viaufkst-tplnr.
*          IF sy-subrc = 0.
*            wa_saida_4-pltxt = wa_iflo-pltxt.
**            WA_SAIDA_3-TPLMA = WA_IFLO-TPLMA.
*          ENDIF.
*
*          READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_aufk-kostl.
*          IF sy-subrc = 0.
*            wa_saida_4-ktext = wa_cskt-ktext.
*          ENDIF.
*
*          APPEND wa_saida_4 TO it_saida_4.
*          CLEAR  wa_reg.
*          CLEAR  wa_saida_4.
*          CLEAR  wa_aufk.
*          CLEAR  wa_crca.
*          CLEAR  wa_crhd.
*          CLEAR  wa_afru.
*          CLEAR  wa_pa0001.
*          CLEAR  wa_saida_3.
*          CLEAR  wa_viaufkst.
*          CLEAR  wa_cskt.
*          CLEAR  wa_iflo.
*        ENDLOOP.
    ENDCASE.


    READ TABLE it_saida_3 INTO wa_saida_3 INDEX e_row_id-index.
    CASE e_column_id-fieldname.
*
      WHEN:'ISMNW'.
        IF wa_saida_3-ismnw IS INITIAL.
          EXIT.
        ENDIF.

        LOOP AT it_saida_4 INTO wa_saida_4 WHERE werks = wa_saida_3-werks
                                             AND arbid = wa_saida_3-arbid
                                             AND tplnr = wa_saida_3-tplnr
                                             AND kostl = wa_saida_3-kostl.

          wa_saida_2-pernr  = wa_saida_4-pernr.
          wa_saida_2-rueck  = |{ wa_saida_4-rueck ALPHA = OUT }|.
          wa_saida_2-rmzhl  = |{ wa_saida_4-rmzhl ALPHA = OUT }|.
          wa_saida_2-ltxa1  = wa_saida_4-ltxa1.
          wa_saida_2-ersda  = |{ wa_saida_4-ersda+6(2) }.{ wa_saida_4-ersda+4(2) }.{ wa_saida_4-ersda(4) }|.
          wa_saida_2-budat  = |{ wa_saida_4-budat+6(2) }.{ wa_saida_4-budat+4(2) }.{ wa_saida_4-budat(4) }|.
          wa_saida_2-aenam  = wa_saida_4-aenam.
          wa_saida_2-aufnr  = |{ wa_saida_4-aufnr ALPHA = OUT }|.
          wa_saida_2-vornr  = wa_saida_4-vornr.
          wa_saida_2-werks  = wa_saida_4-werks.
          wa_saida_2-arbid  = wa_saida_4-arbid.

*          IF WA_SAIDA_4-ISMNU = 'MIN'.
*          WA_SAIDA_4-ISMNW = ( WA_SAIDA_4-ISMNW / 60 ).
*          ENDIF.


          wa_saida_2-ismnw  = wa_saida_4-ismnw.
          wa_saida_2-ismnu  = 'H'."WA_SAIDA_4-ISMNU.

          wa_saida_2-ofmnw  = wa_saida_4-ofmnw.
          wa_saida_2-ismne  = wa_saida_4-ismne.
          wa_saida_2-idaur  = wa_saida_4-idaur.
          wa_saida_2-anzma  = wa_saida_4-anzma.
          wa_saida_2-grund  = wa_saida_4-grund.
          wa_saida_2-isdd   = |{ wa_saida_4-isdd+6(2) }.{ wa_saida_4-isdd+4(2) }.{ wa_saida_4-isdd(4) }|.
          wa_saida_2-isdz   = |{ wa_saida_4-isdz(2) }:{ wa_saida_4-isdz+2(2) }|.
          wa_saida_2-iedd   = |{ wa_saida_4-iedd+6(2) }.{ wa_saida_4-iedd+4(2) }.{ wa_saida_4-iedd(4) }|.
          wa_saida_2-iedz   = |{ wa_saida_4-iedz(2) }:{ wa_saida_4-iedz+2(2) }|.
          wa_saida_2-arbpl  = wa_saida_4-arbpl.
          wa_saida_2-sname  = wa_saida_4-sname.
          wa_saida_2-kostl  = wa_saida_4-kostl.
          wa_saida_2-tplnr  = wa_saida_4-tplnr.
          wa_saida_2-ktext  = wa_saida_4-ktext.
          wa_saida_2-pltxt  = wa_saida_4-pltxt.
          wa_saida_2-auart  = wa_saida_4-auart.
          APPEND wa_saida_2 TO it_saida_2.

          CLEAR  wa_saida_2.
          CLEAR  wa_saida_4.
        ENDLOOP.
    ENDCASE.

    SORT it_saida_2 ASCENDING BY  werks aufnr isdd isdz iedd iedz.

    CALL SCREEN 200.

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA:
 lst_layout TYPE lvc_s_layo.

  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        p_text_2                TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        p_text_table_2          TYPE sdydo_text_table,
        vl_cont                 TYPE i,
        vl_butxt                TYPE t001-butxt,
        vl_dates1               TYPE char10,
        vl_dates2               TYPE char10.

  SET PF-STATUS 'TB01'.
  SET TITLEBAR 'TB02'.


* Adicionando Logo Marca no Cabeçalho
  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.


    IF  it_reg IS NOT INITIAL.
*      P_TEXT = TEXT-004.

      PERFORM fill_it_fieldcatalog USING:

        9  'ICON  '  'T_SAIDA '  '10'   ' '  ' '  ' '  'Status            '  ''  ''  'ICON     '  ' ',
        2  'WERKS '  'T_SAIDA '  '04'   ' '  ' '  ' '  'Centro            '  ''  ''  'AFRU     '  ' ',
        3  'ARBPL '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Centro Trab       '  ''  'X'  'CRHD     '  ' ',
*       4  'KOSTL '  'T_SAIDA '  '20'   ' '  ' '  ' '  'C.custo           '  ''  ''  'IFLO     '  ' ',
*       5  'KTEXT '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Desc c.custo      '  ''  ''  'VIAUFKST '  ' ',
        6  'HRSPER'  'T_SAIDA '  '20'   ' '  ' '  ' '  'Qtd Hr Planejada  '  ''  ''  'VIAUFKST '  ' ',
        7  'HRSAPO'  'T_SAIDA '  '20'   ' '  ' '  ' '  'Qtd Hr Apontada   '  ''  'X' 'AFRU     '  ' ',
        8  'HRSDIS'  'T_SAIDA '  '20'   ' '  ' '  ' '  'Qtd Hr Disponivel '  'C110'  ''  'VIAUFKST '  '  '.

* Adicionando informação do parametro de entrada no cabeçalho.
      PERFORM fill_gs_variant.

      gs_layout-sel_mode   = 'A'.
      gs_layout-cwidth_opt = 'X'.
      CLEAR: it_exclude_fcode, it_exclude_fcode[].

      CREATE OBJECT ctl_alv
        EXPORTING
          i_parent = dg_parent_alv.

      CALL METHOD ctl_alv->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout
          is_variant           = gs_variant
          it_toolbar_excluding = it_exclude_fcode
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = it_fieldcatalog
          it_outtab            = it_reg
          it_sort              = it_sort.

      SET HANDLER: lcl_eventos=>on_hotspot_click FOR ctl_alv.


      CREATE OBJECT dg_dyndoc_id
        EXPORTING
          style = 'ALV_GRID'.

      CALL METHOD dg_dyndoc_id->initialize_document.

      CALL METHOD dg_dyndoc_id->add_table
        EXPORTING
          no_of_columns = 1
          border        = '0'
          width         = '100%'
        IMPORTING
          table         = table_element.

      CALL METHOD table_element->add_column
        IMPORTING
          column = column.

      CALL METHOD table_element->set_column_style
        EXPORTING
          col_no    = 1
          "SAP_ALIGN = 'CENTER'
          sap_style = cl_dd_document=>heading.

      CALL METHOD column->add_text
        EXPORTING
          text      = p_text
          sap_style = 'HEADING'.

      CALL METHOD dg_dyndoc_id->add_table
        EXPORTING
          no_of_columns = 2
          border        = '0'
          width         = '100%'
        IMPORTING
          table         = table_element2.

      CALL METHOD table_element2->add_column
        EXPORTING
          sap_style   = 'SAP_BOLD'
          style_class = 'SAP_BOLD'
        IMPORTING
          column      = column_1.

      PERFORM cabecario.

      "------------------
      "------------------
      CALL METHOD column_1->add_text
        EXPORTING
          text_table = p_text_table
          fix_lines  = 'X'.

      CALL METHOD dg_dyndoc_id->merge_document.

      CREATE OBJECT dg_html_cntrl
        EXPORTING
          parent = dg_parent_2.

      dg_dyndoc_id->html_control = dg_html_cntrl.

      CALL METHOD dg_dyndoc_id->display_document
        EXPORTING
          reuse_control      = 'X'
          parent             = dg_parent_2
        EXCEPTIONS
          html_display_error = 1.
    ELSE.

      ls_stable-row = 'X'.
      ls_stable-col = 'X'.

      CALL METHOD ctl_alv->refresh_table_display
        EXPORTING
          is_stable = ls_stable
        EXCEPTIONS
          finished  = 1
          OTHERS    = 2.

      IF sy-subrc <> 0.
      ENDIF.

    ENDIF.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXECUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_execut_alv .
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.



FORM fill_gs_variant.

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.
ENDFORM.

FORM fill_gs_variant_2.

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.
ENDFORM.

FORM fill_gs_variant_3.

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.
ENDFORM.


*Parametros da ALV.
FORM fill_it_fieldcatalog USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field).

  DATA:  wa_fieldcatalog  TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos     = p_colnum.
  wa_fieldcatalog-fieldname   = p_fieldname.
  wa_fieldcatalog-tabname     = p_tabname.
  wa_fieldcatalog-outputlen   = p_len.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-icon        = p_icon.
  wa_fieldcatalog-do_sum      = p_do_sum.
  wa_fieldcatalog-coltext     = p_header.
  wa_fieldcatalog-emphasize   = p_emphasize.
  wa_fieldcatalog-hotspot     = p_hotspot.
  wa_fieldcatalog-ref_table   = p_ref_table.
  wa_fieldcatalog-ref_table   = p_ref_field.
*  WA_FIELDCATALOG-CHECKTABLE  = P_CHECKTABLE.

  gs_layout-excp_conds    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-sel_mode      = 'A'.
  gs_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef    = ' '.

  APPEND wa_fieldcatalog TO it_fieldcatalog.


ENDFORM.                    " F_DEFINE_CONTAINER_HEADER


*Parametros da ALV.
FORM fill_it_fieldcatalog_2 USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field).

  DATA:  wa_fieldcatalog_2  TYPE lvc_s_fcat.

  wa_fieldcatalog_2-col_pos     = p_colnum.
  wa_fieldcatalog_2-fieldname   = p_fieldname.
  wa_fieldcatalog_2-tabname     = p_tabname.
  wa_fieldcatalog_2-outputlen   = p_len.
  wa_fieldcatalog_2-edit        = p_edit.
  wa_fieldcatalog_2-icon        = p_icon.
  wa_fieldcatalog_2-do_sum      = p_do_sum.
  wa_fieldcatalog_2-coltext     = p_header.
  wa_fieldcatalog_2-emphasize   = p_emphasize.
  wa_fieldcatalog_2-hotspot     = p_hotspot.
  wa_fieldcatalog_2-ref_table   = p_ref_table.
  wa_fieldcatalog_2-ref_table   = p_ref_field.
*  WA_FIELDCATALOG-CHECKTABLE  = P_CHECKTABLE.

  gs_layout_2-excp_conds    = 'X'.
  gs_layout_2-zebra         = 'X'.
  gs_layout_2-sel_mode      = 'A'.
  gs_layout_2-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout_2-totals_bef    = ''.

  APPEND wa_fieldcatalog_2 TO it_fieldcatalog_2.


ENDFORM.                    " F_DEFINE_CONTAINER_HEADER

FORM fill_it_fieldcatalog_3 USING VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header)
                                VALUE(p_emphasize)
                                VALUE(p_hotspot)
                                VALUE(p_ref_table)
                                VALUE(p_ref_field).

  DATA:  wa_fieldcatalog_3  TYPE lvc_s_fcat.

  wa_fieldcatalog_3-col_pos     = p_colnum.
  wa_fieldcatalog_3-fieldname   = p_fieldname.
  wa_fieldcatalog_3-tabname     = p_tabname.
  wa_fieldcatalog_3-outputlen   = p_len.
  wa_fieldcatalog_3-edit        = p_edit.
  wa_fieldcatalog_3-icon        = p_icon.
  wa_fieldcatalog_3-do_sum      = p_do_sum.
  wa_fieldcatalog_3-coltext     = p_header.
  wa_fieldcatalog_3-emphasize   = p_emphasize.
  wa_fieldcatalog_3-hotspot     = p_hotspot.
  wa_fieldcatalog_3-ref_table   = p_ref_table.
  wa_fieldcatalog_3-ref_table   = p_ref_field.
*  WA_FIELDCATALOG-CHECKTABLE  = P_CHECKTABLE.

  gs_layout_3-excp_conds    = 'X'.
  gs_layout_3-zebra         = 'X'.
  gs_layout_3-sel_mode      = 'A'.
  gs_layout_3-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout_3-totals_bef    = ''.

  APPEND wa_fieldcatalog_3 TO it_fieldcatalog_3.


ENDFORM.                    " F_DEFINE_CONTAINER_HEADER



FORM fill_it_sort .

  DATA: wa_sort TYPE lvc_s_sort.

  wa_sort-spos = '1'.
  wa_sort-fieldname = 'WERKS'.
  "WA_SORT-DOWN = 'X'.
  wa_sort-group = '*'.
  wa_sort-subtot = 'X '.
  APPEND wa_sort TO it_sort.

*  IF P_BEBER IS NOT INITIAL.
*
*    WA_SORT-SPOS = '2'.
*    WA_SORT-FIELDNAME = 'BEBER'.
*    "WA_SORT-DOWN = 'X'.
*    WA_SORT-GROUP = '*'.
*    WA_SORT-SUBTOT = 'X'.
*    APPEND WA_SORT TO IT_SORT.
*
*  ENDIF.
ENDFORM.

*  Busca a logo Marca e adiciona no cabeçario.
FORM f_pega_imagem  USING    nome_logo
                  CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.

  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.

  WHILE l_graphic_conv > 255.

    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.

  ENDWHILE.

  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.

ENDFORM.                    " F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*&      Form  CABECARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cabecario .

  IF s_werks IS NOT INITIAL.
    LOOP AT s_werks.
      IF s_werks-option NE 'EQ' AND s_werks-option NE 'BT'.
        sdydo_text_element = 'Centro: Multiplas Seleções'.
        EXIT.
      ELSEIF s_werks-option EQ 'BT'.
        CONCATENATE 'Centro:' s_werks-low '-' s_werks-high INTO sdydo_text_element SEPARATED BY space.
        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Centro: Multiplas Seleções'.
        ELSE.
          CONCATENATE 'Centro:' s_werks-low INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, sdydo_text_element.
  ELSE.
    IF s_werks IS NOT INITIAL.
      sdydo_text_element = 'Centro:'.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.
  ENDIF.
  CLEAR: vl_cont, sdydo_text_element.


* Mosta o cabeçario com o periodo pesquisado.
  LOOP AT s_date.
    IF s_date-option EQ 'BT'.
      CONCATENATE s_date-low+6(2) '.' s_date-low+4(2) '.' s_date-low(4) INTO vl_dates1.
      CONCATENATE s_date-high+6(2) '.' s_date-high+4(2) '.' s_date-high(4) INTO vl_dates2.
      CONCATENATE 'Período:' vl_dates1 '-' vl_dates2 INTO sdydo_text_element SEPARATED BY space.
      EXIT.
    ELSE.
      CONCATENATE s_date-low+6(2) '.' s_date-low+4(2) '.' s_date-low(4) INTO vl_dates1.
      CONCATENATE 'Período:' vl_dates1 INTO sdydo_text_element SEPARATED BY space.
    ENDIF.
  ENDLOOP.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element, vl_dates1, vl_dates2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'TB03'.
  SET TITLEBAR 'TB04'.

  IF it_saida_2 IS NOT INITIAL.
*      P_TEXT = TEXT-007.
    PERFORM fill_it_fieldcatalog_2 USING:

     1  'WERKS '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Centro              '  ' '  ' '  'AFRU'    ' ',
     2  'KOSTL '  'IT_SAIDA_2 '    '15'  ' '     ' '    ' '   'Centro custo        '  ' '  ' '  'AUFK'    ' ',
     3  'KTEXT '  'IT_SAIDA_2 '    '30'  ' '     ' '    ' '   'Desc c.custo        '  ' '  ' '  'CSKT'    ' ',
     4  'TPLNR '  'IT_SAIDA_2 '    '30'  ' '     ' '    ' '   'Local inst          '  ' '  ' '  'VIAUFKST' ' ',
     5  'PLTXT '  'IT_SAIDA_2 '    '30'  ' '     ' '    ' '   'Desc Local          '  ' '  ' '  'IFLO'    ' ',
     6  'AUART '  'IT_SAIDA_2 '    '15'  ' '     ' '    ' '   'Tipo ordem          '  ' '  ' '  'AFRU'    ' ',
     7  'AUFNR '  'IT_SAIDA_2 '    '15'  ' '     ' '    ' '   'Ordem               '  ' '  'X'  'AFRU'    ' ',
     8  'VORNR '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Nº operação         '  ' '  ' '  'AFRU'    ' ',
     9  'ARBPL '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Centro trab         '  ' '  ' '  'CRHD'    ' ',
    10  'ERSDA '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Data entr conf      '  ' '  ' '  'AFRU'    ' ',
    11  'BUDAT '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Data lanc           '  ' '  ' '  'AFRU'    ' ',
    12  'PERNR '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Nº pessoal          '  ' '  ' '  'AFRU'    ' ',
    13  'SNAME '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Nome pessoal        '  ' '  ' '  'PA0001'  ' ',
    14  'ISDD  '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Data inicio         '  ' '  ' '  'AFRU'    ' ',
    15  'ISDZ  '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Hora inicio         '  ' '  ' '  'AFRU'    ' ',
    16  'IEDD  '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Data fim            '  ' '  ' '  'AFRU'    ' ',
    17  'IEDZ  '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Hora fim            '  ' '  ' '  'AFRU'    ' ',
    18  'ISMNW '  'IT_SAIDA_2 '    '10'  ' '     ' '    'X'   'Trab real           '  'C110 '  ' '  'AFRU'    ' ',
    19  'ISMNU '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Und trab real       '  ' '  ' '  'AFRU'    ' '.




  ENDIF.
  IF g_container IS INITIAL.

    CREATE OBJECT g_container
      EXPORTING
        container_name              = 'CONTAINER_2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT ctl_alv_2
      EXPORTING
        i_parent = g_container.

*    WA_EXCLUDE_FCODE_2 = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*    APPEND WA_EXCLUDE_FCODE_2 TO IT_EXCLUDE_FCODE_2.


    CALL METHOD ctl_alv_2->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_2
        is_variant           = gs_variant_2
        it_toolbar_excluding = it_exclude_fcode_2
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_2
        it_outtab            = it_saida_2
        it_sort              = it_sort.

    SET HANDLER: lcl_eventos_3=>on_hotspot_click FOR ctl_alv_2.

  ELSE.
    CALL METHOD ctl_alv_2->refresh_table_display.
  ENDIF.

* _______________________________________


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'TP03'.
  SET TITLEBAR 'TP04'.

  IF it_saida_3 IS NOT INITIAL.
*      P_TEXT = TEXT-007.
    PERFORM fill_it_fieldcatalog_3 USING:

     1  'WERKS '  'IT_SAIDA_3 '    '10'  ' '     ' '    ' '   'Centro              '  ' '  ' '  'AFRU    '   ' ',
     4  'KOSTL '  'IT_SAIDA_3 '    '10'  ' '     ' '    ' '   'Centro cust         '  ' '  ' '  'AUFK    '   ' ',
     5  'KTEXT '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Desc cent.custo     '  ' '  ' '  'CSKT    '   ' ',
     2  'TPLNR '  'IT_SAIDA_3 '    '10'  ' '     ' '    ' '   'Local inst          '  ' '  ' '  'VIAUFKST'   ' ',
     3  'PLTXT '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Desc local          '  ' '  ' '  'IFLO    '   ' ',
*     6  'EQUNR '  'IT_SAIDA_3 '    '10'  ' '     ' '    ' '   'Equpamento          '  ' '  ' '  'VIAUFKST'   ' ',
*     6  'EQKTX '  'IT_SAIDA_3 '    '10'  ' '     ' '    ' '   'Txt.Eqpto           '  ' '  ' '  'EQKT    '   ' ',
*     7  'AUFNR '  'IT_SAIDA_3 '    '10'  ' '     ' '    ' '   'Ordem               '  ' '  ' '  'AFRU    '   ' ',
     8  'ARBPL '  'IT_SAIDA_3 '    '10'  ' '     ' '    ' '   'Cent.trab           '  ' '  ' '  'AFRU    '   ' ',
     9  'ISMNW '  'IT_SAIDA_3 '    '10'  ' '     ' '    'X'   'Quant hrs apont     '  'C110 '  'X'  'AFRU    '   ' ',
     10 'ISMNU '  'IT_SAIDA_3 '    '10'  ' '     ' '    ' '   'Und trab real       '  ' '  ' '  'AFRU    '   ' '.
*   11  'ARBPL '  'IT_SAIDA_2 '    '10'  ' '     ' '    ' '   'Centro trab         '  ' '  ' '  'CRHD'   ' ',
*   10  'TPLMA '  'IT_SAIDA_3 '    '10'  ' '     ' '    ' '   'Local superior      '  ' '  ' '  'IFLO    '   ' ',



  ENDIF.
  IF g_container_3 IS INITIAL.

    CREATE OBJECT g_container_3
      EXPORTING
        container_name              = 'CONTAINER_3'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT ctl_alv_3
      EXPORTING
        i_parent = g_container_3.

*    WA_EXCLUDE_FCODE_2 = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*    APPEND WA_EXCLUDE_FCODE_2 TO IT_EXCLUDE_FCODE_2.


    CALL METHOD ctl_alv_3->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_3
        is_variant           = gs_variant_3
        it_toolbar_excluding = it_exclude_fcode_3
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_3
        it_outtab            = it_saida_3
        it_sort              = it_sort_3.

    SET HANDLER: lcl_eventos_2=>on_hotspot_click FOR ctl_alv_3.

  ELSE.
    CALL METHOD ctl_alv_3->refresh_table_display.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

"146081 CS2023000568 Visão análitica ZPM0036 PSA
MODULE user_command_0400 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE status_0400 OUTPUT.
  SET PF-STATUS 'STATUS_0400'.
  SET TITLEBAR 'T0400'.
  CREATE OBJECT lo_report.
  lo_report->get_data_c1( ).
  lo_report->generate_output1( ).
  lo_report->generate_output2( ).
  lo_report->generate_output3( ).
ENDMODULE.

FORM action_process.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDFORM.


*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_EVENTOS_3
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_eventos_3 IMPLEMENTATION.

  METHOD on_hotspot_click.


    READ TABLE it_saida_2 INTO wa_saida_2 INDEX e_row_id-index.

    CASE e_column_id-fieldname.
      WHEN:'AUFNR'."Ordem Manuteção
        IF wa_saida_2-aufnr IS NOT INITIAL.
          SET PARAMETER ID 'ANR' FIELD wa_saida_2-aufnr.
          CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN .
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.               "LCL_EVENTOS_3
