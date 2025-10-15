**********************************************************************
*                         Consultoria                                *
**********************************************************************
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 27.04.2022                                              *
* Descrição: Melhoria Cockpit de Pagto de Transbordo                 *
* Report   : ZLESR0154                                               *
**********************************************************************
* Projeto  : CS2022000141                                            *
**********************************************************************
REPORT zlesr0154.

**********************************************************************
* tabelas
**********************************************************************
TABLES: t001, icon, zlest0220, zlest0039, sscrfields.

**********************************************************************
* includes
**********************************************************************
INCLUDE <icon>.

**********************************************************************
* field symbols
**********************************************************************
FIELD-SYMBOLS: <fs_fld> TYPE any.

**********************************************************************
* typesabelas
**********************************************************************
TYPES: BEGIN OF ty_alv_rod.
         INCLUDE TYPE zlest0220.
         TYPES: status TYPE icon-id.
TYPES: erro TYPE char1.
TYPES: msg_erro TYPE char255.
TYPES: END   OF ty_alv_rod.

TYPES: BEGIN OF ty_alv_fer.
         INCLUDE TYPE zlest0219.
         TYPES: status TYPE icon-id.
TYPES: erro TYPE char1.
TYPES: msg_erro TYPE char255.
TYPES: END   OF ty_alv_fer.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.


TYPES: BEGIN OF ty_active,
         stcd1       TYPE lfa1-stcd1,
         serie       TYPE j_1bnfe_active-serie,
         nfnum9      TYPE j_1bnfe_active-nfnum9,
         cnpj_filial TYPE j_1bbranch-stcd1.
TYPES: END OF ty_active.




TYPES: BEGIN OF ty_alv_nfps_rod,
         bukrs          TYPE bukrs,
         branch         TYPE j_1bnfdoc-branch,
         t001w_name1    TYPE t001w-name1,
         cnpj_forn      TYPE zlest0220-cnpj_forn,
         desc_cnpj_forn TYPE lfa1-name1,
         nfps           TYPE zlest0220-nfps,
         data_nfps      TYPE zlest0220-data_nfps,
         chave_nf_prod  TYPE zlest0220-chave_nf_prod,
         valor_servico  TYPE zlest0220-valor_servico,
         matnr          TYPE zlest0039-matnr,
         maktx          TYPE makt-maktx,
         transb_efetivo TYPE zlest0039-transb_efetivo,
         lfa1_name1     TYPE lfa1-name1,
         lfa1_ort01     TYPE lfa1-ort01,
         nfenum         TYPE zlest0039-nfenum,
         serie          TYPE zlest0039-serie,
         nfenum_terc    TYPE zlest0039-nfenum,
         serie_terc     TYPE zlest0039-serie,
         datasaida      TYPE zlest0039-datasaida,
         pesosaida      TYPE zlest0039-pesosaida,
         data_descarga  TYPE zlest0220-data_descarga,
         datatransb     TYPE zlest0039-datatransb,
         pesotransb     TYPE zlest0039-pesotransb,
         peso_descarga  TYPE zlest0220-peso_descarga,
         saldo          TYPE zlest0220-peso_descarga,
         pontoentrega   TYPE zlest0039-pontoentrega,
         kna1_name1     TYPE kna1-name1,
         kna1_ort01     TYPE kna1-ort01.
TYPES: END   OF ty_alv_nfps_rod.

TYPES: BEGIN OF ty_alv_nfps_fer,
         bukrs           TYPE bukrs,
         t001w_name1     TYPE t001w-name1,
         nfps            TYPE zlest0219-nfps,
         data_nfps       TYPE zlest0219-data_nfps,
         valor_servico   TYPE zlest0219-valor_servico,
         tarifa          TYPE zlest0219-tarifa,
         matnr           TYPE zlest0219-produto,
         maktx           TYPE makt-maktx,
         dcl             TYPE zlest0219-dcl,
         idvagao         TYPE zlest0219-idvagao,
         peso_vagao      TYPE zlest0219-peso_vagao,
         data_carga      TYPE zlest0219-data_carga,
         cnpj_transbordo TYPE zlest0219-cnpj_transbordo,
         kna1_kunnr      TYPE kna1-kunnr,
         kna1_name1      TYPE kna1-name1.
TYPES: END   OF ty_alv_nfps_fer.

TYPES: BEGIN OF ty_alv_nfprod,
         status         TYPE icon-id,
         bukrs          TYPE bukrs,
         t001w_name1    TYPE t001w-name1,
         matnr          TYPE zlest0039-matnr,
         maktx          TYPE makt-maktx,
         transb_efetivo TYPE zlest0039-transb_efetivo,
         lfa1_name1     TYPE lfa1-name1,
         lfa1_ort01     TYPE lfa1-ort01,
         nfenum         TYPE zlest0039-nfenum,
         serie          TYPE zlest0039-serie,
         nfenum_terc    TYPE zlest0039-nfenum,
         serie_terc     TYPE zlest0039-serie,
         datasaida      TYPE zlest0039-datasaida,
         pesosaida      TYPE zlest0039-pesosaida,
         data_descarga  TYPE zlest0220-data_descarga,
         datatransb     TYPE zlest0039-datatransb,
         pesotransb     TYPE zlest0039-pesotransb,
         pontoentrega   TYPE zlest0039-pontoentrega,
         kna1_name1     TYPE kna1-name1,
         kna1_ort01     TYPE kna1-ort01,
         nfps           TYPE zlest0220-nfps,
         valor_servico  TYPE zlest0220-valor_servico.
TYPES: END   OF ty_alv_nfprod.

TYPES: BEGIN OF ty_zlest0039.
         INCLUDE STRUCTURE zlest0039.
         TYPES: cnpj_14 TYPE j_1bcgc.
TYPES: nfenum_9  TYPE znfnum.
TYPES: serie_3   TYPE j_1bseries.
TYPES: END OF ty_zlest0039.

TYPES: BEGIN OF ty_icon,
         id   TYPE icon-id,
         name TYPE icon-name.
TYPES: END   OF ty_icon.


**********************************************************************
* variaveis
**********************************************************************
DATA: l_sel_button         TYPE smp_dyntxt,
      l_opcao              TYPE char1,
      l_nfps               TYPE znfnum,
      l_data_char          TYPE char10,
      l_tabix              TYPE sy-tabix,
      l_icon_name          TYPE icon-name,
      l_leave              TYPE syst_ucomm,
*
      t_zlest0039          TYPE TABLE OF ty_zlest0039,

      t_active             TYPE TABLE OF ty_active,
      ws_active            TYPE ty_active,
      t_j_1bbranch         TYPE TABLE OF j_1bbranch,
      ws_j_1bbranch        TYPE j_1bbranch,
      t_lfa1               TYPE TABLE OF lfa1,
      t_kna1               TYPE TABLE OF kna1,
      ws_lfa1              TYPE lfa1,
      t_zlest0041          TYPE TABLE OF zlest0041,
      it_zlest0041         TYPE TABLE OF zlest0041,
      ws_zlest0041         TYPE zlest0041,
      t_zlest0219          TYPE TABLE OF zlest0219,
      t_zlest0219_aux      TYPE TABLE OF zlest0219,
      t_zlest0220          TYPE TABLE OF zlest0220,
      t_zlest0220_aux      TYPE TABLE OF zlest0220,
      t_tab                TYPE TABLE OF alsmex_tabline,
      t_raw                TYPE truxs_t_text_data,
      t_file_rod           TYPE TABLE OF zlese0035,
      t_file_rod_aux       TYPE TABLE OF zlese0035,
      t_file_fer           TYPE TABLE OF zlese0036,
      t_file_fer_aux       TYPE TABLE OF zlese0036,
      t_alv_rod            TYPE TABLE OF ty_alv_rod,
      t_alv_fer            TYPE TABLE OF ty_alv_fer,
      t_alv_nfps_rod       TYPE TABLE OF ty_alv_nfps_rod,
      t_alv_nfps_fer       TYPE TABLE OF ty_alv_nfps_fer,
      t_alv_nfprod         TYPE TABLE OF ty_alv_nfprod,
      t_icon               TYPE TABLE OF ty_icon,
*
      w_zlest0039          TYPE ty_zlest0039,
      ws_zlest0039         TYPE ty_zlest0039,
      w_zlest0041          TYPE zlest0041,
      w_zlest0219          TYPE zlest0219,
      w_zlest0220          TYPE zlest0220,
      w_tab                TYPE alsmex_tabline,
      w_file_rod           TYPE zlese0035,
      w_file_rod_aux       TYPE zlese0035,
      w_file_fer           TYPE zlese0036,
      w_file_fer_aux       TYPE zlese0036,
      w_icon               TYPE ty_icon,
      w_alv_rod            TYPE ty_alv_rod,
      w_alv_fer            TYPE ty_alv_fer,
      w_alv_nfps_rod       TYPE ty_alv_nfps_rod,
      w_alv_nfps_fer       TYPE ty_alv_nfps_fer,
      w_alv_nfprod         TYPE ty_alv_nfprod,

*------------------------------------
*---- ALV
*------------------------------------
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      picture              TYPE REF TO cl_gui_picture,
      l_graphic_conv       TYPE i,
      l_graphic_offs       TYPE i,
      graphic_size         TYPE i,
      l_graphic_xstr       TYPE xstring,
      url(255)             TYPE c,
      graphic_url(255),
      t_function           TYPE ui_functions,
      w_function           TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm,
*
      zcl_util             TYPE REF TO zcl_util.

DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader.

DATA: variante         LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.



*------------------------------------
*---- figuras
*------------------------------------
DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

**********************************************************************
* ranges
**********************************************************************
RANGES: r_name      FOR icon-name.

**********************************************************************
* classes / implementacoes
**********************************************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.
  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
* SELECTION-SCREEN
**********************************************************************
PARAMETERS:     p_submit TYPE char20 NO-DISPLAY.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:     p_import RADIOBUTTON GROUP g1 USER-COMMAND rad1 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: POSITION 4.
PARAMETERS:     p_rodo  RADIOBUTTON GROUP g2 DEFAULT 'X'.
SELECTION-SCREEN: COMMENT 6(20) text-004.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: POSITION 4.
PARAMETERS:     p_ferro RADIOBUTTON GROUP g2.
SELECTION-SCREEN: COMMENT 6(20) text-005.
SELECTION-SCREEN END OF LINE.
*----------------
PARAMETERS:     p_nfps   RADIOBUTTON GROUP g1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: POSITION 4.
PARAMETERS:     p_ro_nfp RADIOBUTTON GROUP g3 DEFAULT 'X'.
SELECTION-SCREEN: COMMENT 6(20) text-004.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: POSITION 4.
PARAMETERS:     p_fe_nfp RADIOBUTTON GROUP g3.
SELECTION-SCREEN: COMMENT 6(20) text-005.
SELECTION-SCREEN END OF LINE.
*----------------
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: POSITION 1.
PARAMETERS:     p_nfprod RADIOBUTTON GROUP g1.
SELECTION-SCREEN: COMMENT 3(40) text-006.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK b1.
*----------------
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:     p_file         LIKE rlgrap-filename MODIF ID t1.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-010.
SELECT-OPTIONS: s_empre         FOR t001-bukrs                MODIF ID t2a NO INTERVALS,
                s_cnpj          FOR zlest0220-cnpj_transbordo MODIF ID t2b NO INTERVALS,
                s_ntserv        FOR zlest0220-nfps            MODIF ID t2b NO INTERVALS,
                s_period        FOR zlest0220-data_nfps       MODIF ID t2a.
SELECTION-SCREEN END   OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-011.
SELECT-OPTIONS: s_empre2        FOR t001-bukrs                MODIF ID t3a NO INTERVALS,
                s_werks         FOR zlest0039-werks           MODIF ID t3b NO INTERVALS,
                s_matnr         FOR zlest0039-matnr           MODIF ID t3b NO INTERVALS,
                s_nfenum        FOR zlest0039-nfenum          MODIF ID t3b,
                s_ptrans        FOR zlest0039-pontotransb     MODIF ID t3b,
                s_pentre        FOR zlest0039-pontoentrega    MODIF ID t3b,
                s_perio2        FOR zlest0039-datatransb      MODIF ID t3a.
SELECTION-SCREEN END   OF BLOCK b4.

SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'
SELECTION-SCREEN FUNCTION KEY 2.  "Will have a function code of 'FC02'
SELECTION-SCREEN FUNCTION KEY 3.  "Will have a function code of 'FC02'

**********************************************************************
*SELECTION-SCREEN p_file
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_select_file USING p_file.

**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN.

  FREE MEMORY ID 'ZLESR0154'.
  FREE: l_leave.

  CASE sy-ucomm.
    WHEN 'FC01'.
      l_opcao = '1'.

    WHEN 'FC02'.
      l_opcao = '1'.
      SUBMIT zlesr0156 VIA SELECTION-SCREEN WITH p_submit = sy-cprog
                       EXPORTING LIST TO MEMORY
                       AND RETURN.
      IMPORT l_leave TO l_leave FROM MEMORY ID 'ZLESR0154'.
      IF l_leave = 'LEAVE'.
        LEAVE PROGRAM.
      ENDIF.


    WHEN 'FC03'.
      l_opcao = '1'.
      SUBMIT zlesr0155 VIA SELECTION-SCREEN WITH p_submit = sy-cprog
                       AND RETURN.
      IMPORT l_leave TO l_leave FROM MEMORY ID 'ZLESR0154'.
      IF l_leave = 'LEAVE'.
        LEAVE PROGRAM.
      ENDIF.

  ENDCASE.



**********************************************************************
*SELECTION-SCREEN output
**********************************************************************
AT SELECTION-SCREEN OUTPUT.
  IF l_opcao = 2 OR l_opcao = 3.
    LOOP AT SCREEN.
      screen-active    = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF p_import = abap_true.
        IF screen-name = 'P_RO_NFP' OR
           screen-name = 'P_FE_NFP'.
          screen-input     = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1(2) = 'T2' OR
           screen-group1(2) = 'T3'.
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSEIF p_nfps = abap_true.
        IF screen-name = 'P_RODO' OR
           screen-name = 'P_FERRO'.
          screen-input     = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1(2) = 'T1' OR
           screen-group1(2) = 'T3'.
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSEIF p_nfprod = abap_true.
        IF screen-name = 'P_RODO' OR
           screen-name = 'P_FERRO'.
          screen-input     = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1(2) = 'T1' OR
           screen-group1(2) = 'T2'.
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF screen-group1 = 'T2A' OR
         screen-group1 = 'T3A'.
        screen-required = 2.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.


**********************************************************************
* inicio
**********************************************************************
*INITIALIZATION.

    FREE MEMORY ID 'ZLESR0154'.
    l_leave = 'LEAVE'.
    EXPORT l_leave FROM l_leave TO MEMORY ID 'ZLESR0154'.

    l_opcao                = '1'.

  ENDIF.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

  FREE MEMORY ID 'ZLESR0154'.
  l_leave = 'LEAVE'.
  EXPORT l_leave FROM l_leave TO MEMORY ID 'ZLESR0154'.

  l_opcao                = '1'.

  l_sel_button-icon_id   = icon_dangerous_goods.
  l_sel_button-icon_text = 'Transbordo'.
  sscrfields-functxt_01  = l_sel_button.

  l_sel_button-icon_id   = icon_warehouse.
  l_sel_button-icon_text = 'Armazenagem'.
  sscrfields-functxt_02  = l_sel_button.

  l_sel_button-icon_id   = icon_work_center.
  l_sel_button-icon_text = 'Prestação Serviços'.
  sscrfields-functxt_03  = l_sel_button.

**********************************************************************
* START
**********************************************************************
START-OF-SELECTION.

  IF l_opcao = '1'.
    IF p_import = abap_true.
      IF p_file   IS INITIAL.
        MESSAGE s024(sd) WITH 'Informar campos obrigatórios.' DISPLAY LIKE 'E'.
*        STOP.
      ENDIF.
    ENDIF.

    IF p_nfps = abap_true.
      IF s_empre  IS INITIAL OR
         s_period IS INITIAL.
        MESSAGE s024(sd) WITH 'Informar campos obrigatórios.' DISPLAY LIKE 'E'.
*        STOP.
      ENDIF.
    ENDIF.

    IF p_nfprod = abap_true.
      IF s_empre2  IS INITIAL OR
         s_perio2 IS INITIAL.
        MESSAGE s024(sd) WITH 'Informar campos obrigatórios.' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK l_opcao = '1'.

  PERFORM f_selecao_dados.

  CASE abap_true.
    WHEN p_import.
      PERFORM f_carrega_arquivo.

      IF t_file_rod[] IS INITIAL AND
         t_file_fer[] IS INITIAL.
        MESSAGE s024(sd) WITH text-101 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      CASE abap_true.
        WHEN p_rodo.
          PERFORM f_processa_rodoviario.
          PERFORM f_salva_rodoviario.
        WHEN p_ferro.
          PERFORM f_processa_ferroviario.
          PERFORM f_salva_ferroviario.
      ENDCASE.

    WHEN p_nfps.
      CASE abap_true.
        WHEN p_ro_nfp.
          PERFORM f_selecao_dados_nfps_rodo.

          IF t_alv_nfps_rod[] IS INITIAL.
            MESSAGE s024(sd) WITH text-131 DISPLAY LIKE 'E'.
            STOP.
          ENDIF.

          PERFORM f_exibir_dados_nfps.

        WHEN p_fe_nfp.
          PERFORM f_selecao_dados_nfps_ferr.

          IF t_alv_nfps_fer[] IS INITIAL.
            MESSAGE s024(sd) WITH text-131 DISPLAY LIKE 'E'.
            STOP.
          ENDIF.

          PERFORM f_exibir_dados_nfps.
      ENDCASE.

    WHEN p_nfprod.
      PERFORM f_selecao_dados_nfprod.

      IF t_alv_nfprod[] IS INITIAL.
        MESSAGE s024(sd) WITH text-131 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      PERFORM f_exibir_dados_nfprod.

  ENDCASE.

**********************************************************************
* selecao dados
**********************************************************************
FORM f_selecao_dados.

*----------------------------------------------
*-Montar Range icons
*----------------------------------------------
  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_GREEN_LIGHT'.
  APPEND r_name.

  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_YELLOW_LIGHT'.
  APPEND r_name.

  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_RED_LIGHT'.
  APPEND r_name.

  SELECT id
         name
    FROM icon
    INTO TABLE t_icon
   WHERE name IN r_name.

  SORT t_icon  BY name.

ENDFORM.

**********************************************************************
* selecao dados nfps rodoviario
**********************************************************************
FORM f_selecao_dados_nfps_rodo.

  DATA: w_campos_nfe TYPE zde_campos_nfe,
        r_lifnr      TYPE RANGE OF lifnr,
        r_branch     TYPE RANGE OF j_1bbranc_.

  CREATE OBJECT zcl_util.

*------------------------------------------------
* selecao
*------------------------------------------------
  SELECT *
    FROM zlest0220
    INTO TABLE t_zlest0220
   WHERE bukrs           IN s_empre
     AND cnpj_transbordo IN s_cnpj
     AND nfps            IN s_ntserv
     AND data_nfps       IN s_period.

  IF p_ro_nfp = abap_true.
    DELETE t_zlest0220 WHERE tipo_carga = 'F'.
  ELSE.
    DELETE t_zlest0220 WHERE tipo_carga = 'R'.
  ENDIF.

  CHECK t_zlest0220[] IS NOT INITIAL.

  t_zlest0220_aux[] = t_zlest0220[].
  DELETE t_zlest0220_aux WHERE chave_nf_prod IS INITIAL.

  IF t_zlest0220_aux[] IS NOT INITIAL.
    SELECT *
      FROM zlest0039
      INTO TABLE t_zlest0039
       FOR ALL ENTRIES IN t_zlest0220_aux
     WHERE chave_nfe = t_zlest0220_aux-chave_nf_prod.
  ENDIF.


  ""=====Inicio #74126 / Anderson Oenning
  FREE: t_active.
  "Seleção tabela.
  t_active = VALUE #( FOR l IN t_zlest0220 ( stcd1  = l-chave_nf_prod+6(14)
                                             serie  = l-chave_nf_prod+22(3)
                                             nfnum9 = l-chave_nf_prod+25(9)
                                        cnpj_filial = l-cnpj_filial
   ) ).

  FREE: t_lfa1, t_j_1bbranch.
  IF t_active IS NOT INITIAL.
    SELECT * FROM lfa1 INTO TABLE t_lfa1
      FOR ALL ENTRIES IN t_active
      WHERE stcd1  EQ  t_active-stcd1
        AND land1  EQ 'BR' ORDER BY PRIMARY KEY .

    SELECT * FROM j_1bbranch INTO TABLE t_j_1bbranch
     FOR ALL ENTRIES IN t_active
     WHERE stcd1  EQ  t_active-cnpj_filial.
  ENDIF.

  r_lifnr  = VALUE #( FOR i IN t_lfa1 ( sign = 'I' option = 'EQ' low = i-lifnr ) ).
  r_branch = VALUE #( FOR t IN t_j_1bbranch ( sign = 'I' option = 'EQ' low = t-branch ) ).

  IF r_lifnr IS NOT INITIAL OR r_branch IS NOT INITIAL.
    SORT: r_branch BY low,
         r_lifnr  BY low.
  ENDIF.
  SELECT * FROM zlest0041 INTO TABLE it_zlest0041
  FOR ALL ENTRIES IN  t_active
  WHERE nr_nf             = t_active-nfnum9
  AND serie               = t_active-serie
  AND centro_comprador    IN r_branch
  AND cod_cliente         IN r_lifnr.

  IF it_zlest0041 IS NOT INITIAL.
    SELECT *
    FROM zlest0039
    APPENDING TABLE t_zlest0039
    FOR ALL ENTRIES IN it_zlest0041
    WHERE docnum = it_zlest0041-docnum.
  ENDIF.

  ""=====Fim #74126 / Anderson Oenning

  IF t_zlest0039[] IS NOT INITIAL.
    SORT t_zlest0039 BY docnum.
    DELETE ADJACENT DUPLICATES FROM t_zlest0039 COMPARING docnum.

    SELECT *
    FROM zlest0041
    INTO TABLE t_zlest0041
     FOR ALL ENTRIES IN t_zlest0039
     WHERE docnum = t_zlest0039-docnum.

    SELECT werks, name1
      FROM t001w
      INTO TABLE @DATA(t_t001w)
       FOR ALL ENTRIES IN @t_zlest0039
     WHERE werks = @t_zlest0039-werks.

    SELECT matnr, maktx
      FROM makt
      INTO TABLE @DATA(t_makt)
       FOR ALL ENTRIES IN @t_zlest0039
     WHERE matnr = @t_zlest0039-matnr
       AND spras = @sy-langu.


    SELECT *
    FROM lfa1
    APPENDING TABLE t_lfa1
    FOR ALL ENTRIES IN t_zlest0039
    WHERE lifnr = t_zlest0039-pontoentrega ORDER BY PRIMARY KEY .

    DELETE ADJACENT DUPLICATES FROM t_lfa1 COMPARING lifnr.

    SELECT *
    FROM kna1
    INTO TABLE t_kna1
    FOR ALL ENTRIES IN t_zlest0039
    WHERE kunnr EQ t_zlest0039-transb_efetivo.

*  ENDIF.
  ENDIF.

*------------------------------------------------
* gerar alv
*------------------------------------------------
  FREE: t_alv_nfps_rod.
  CLEAR: w_zlest0220.

  LOOP AT t_zlest0220 INTO w_zlest0220.

    CLEAR: w_alv_nfps_rod, w_zlest0039, w_zlest0041, ws_active, ws_j_1bbranch, ws_lfa1, ws_zlest0041, ws_zlest0039.


""=====Inicio #74126 / Anderson Oenning
    READ TABLE t_zlest0039  INTO w_zlest0039 WITH KEY chave_nfe = w_zlest0220-chave_nf_prod.
    IF sy-subrc NE 0.
      "Procurar informação com segunda seleção.
      READ TABLE t_active INTO ws_active WITH KEY stcd1  =  w_zlest0220-chave_nf_prod+6(14)
                                                        serie  =  w_zlest0220-chave_nf_prod+22(3)
                                                        nfnum9 = w_zlest0220-chave_nf_prod+25(9).

      IF sy-subrc EQ 0.
        READ TABLE t_lfa1 INTO ws_lfa1 WITH KEY stcd1 = ws_active-stcd1.
        IF sy-subrc EQ 0.
          READ TABLE t_j_1bbranch INTO ws_j_1bbranch WITH KEY stcd1 = ws_active-cnpj_filial.
          IF sy-subrc EQ 0.
            SELECT SINGLE * FROM zlest0041 INTO w_zlest0041
              WHERE centro_comprador  = ws_j_1bbranch-branch
                AND  nr_nf            = ws_active-nfnum9
                AND cod_cliente       = ws_lfa1-lifnr
                AND serie             = ws_active-serie.

            IF sy-subrc EQ 0.
              SELECT SINGLE * FROM zlest0039
                INTO ws_zlest0039
                WHERE docnum EQ w_zlest0041-docnum.
              IF sy-subrc EQ 0.
                w_zlest0039 = ws_zlest0039.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
""=====Fim #74126 / Anderson Oenning
    ELSE.
      READ TABLE t_zlest0041  INTO w_zlest0041 WITH KEY docnum    = w_zlest0039-docnum.
    ENDIF.

    READ TABLE t_t001w INTO DATA(w_t001w) WITH KEY werks = w_zlest0039-werks.
    IF sy-subrc = 0.
      w_alv_nfps_rod-t001w_name1 = w_t001w-name1.
    ENDIF.

    READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_zlest0039-matnr.
    IF sy-subrc = 0.
      w_alv_nfps_rod-maktx         = w_makt-maktx.
    ENDIF.

    READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY lifnr = w_zlest0039-pontoentrega.
    IF sy-subrc = 0.
      w_alv_nfps_rod-lfa1_name1    = w_lfa1-name1.
      w_alv_nfps_rod-lfa1_ort01    = w_lfa1-ort01.
    ENDIF.

    READ TABLE t_kna1 INTO DATA(w_kna1) WITH KEY kunnr = w_zlest0039-transb_efetivo.
    IF sy-subrc = 0.
      w_alv_nfps_rod-kna1_name1    = w_kna1-name1.
      w_alv_nfps_rod-kna1_ort01    = w_kna1-ort01.
    ENDIF.

    w_campos_nfe               = zcl_util->get_atributos_nfe( w_zlest0220-chave_nf_prod ).

    IF w_zlest0220-chave_nf_prod IS NOT INITIAL.
      SELECT docnum
        INTO @DATA(l_docnum)
        FROM j_1bnfe_active
          UP TO 1 ROWS
       WHERE regio   = @w_campos_nfe-regio
         AND nfyear  = @w_campos_nfe-nfyear
         AND nfmonth = @w_campos_nfe-nfmonth
         AND stcd1   = @w_campos_nfe-stcd1
         AND model   = @w_campos_nfe-model
         AND serie   = @w_campos_nfe-serie
         AND nfnum9  = @w_campos_nfe-nfnum9
         AND docnum9 = @w_campos_nfe-docnum9
         AND cdv     = @w_campos_nfe-cdv.
      ENDSELECT.

      IF sy-subrc = 0.
        SELECT branch
          INTO @DATA(l_branch)
            UP TO 1 ROWS
          FROM j_1bnfdoc
         WHERE docnum = @l_docnum.
        ENDSELECT.
        IF sy-subrc = 0.
          SELECT name1
            FROM t001w
            INTO @DATA(l_name1)
              UP TO 1 ROWS
           WHERE werks = @l_branch.
          ENDSELECT.
          w_alv_nfps_rod-branch      = l_branch.
          w_alv_nfps_rod-t001w_name1 = l_name1.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT name1
      INTO @DATA(l_desc_cnpj_forn)
      FROM lfa1
        UP TO 1 ROWS
     WHERE stcd1 = @w_zlest0220-cnpj_forn.
    ENDSELECT.
    IF sy-subrc = 0.
      w_alv_nfps_rod-desc_cnpj_forn  = l_desc_cnpj_forn.
    ENDIF.

    w_alv_nfps_rod-bukrs             = w_zlest0039-bukrs.
    w_alv_nfps_rod-cnpj_forn         = w_zlest0220-cnpj_forn.
    w_alv_nfps_rod-nfps              = w_zlest0220-nfps.
    w_alv_nfps_rod-data_nfps         = w_zlest0220-data_nfps.
    w_alv_nfps_rod-chave_nf_prod     = w_zlest0220-chave_nf_prod.
    w_alv_nfps_rod-valor_servico     = w_zlest0220-valor_servico.
    w_alv_nfps_rod-matnr             = w_zlest0039-matnr.
    w_alv_nfps_rod-transb_efetivo    = w_zlest0039-transb_efetivo.
    w_alv_nfps_rod-nfenum            = w_zlest0039-nfenum.
    w_alv_nfps_rod-serie             = w_zlest0039-serie.
    w_alv_nfps_rod-nfenum_terc       = w_zlest0041-nr_nf. "w_campos_nfe-nfnum9.
    w_alv_nfps_rod-serie_terc        = w_zlest0041-serie. "w_campos_nfe-serie.
    w_alv_nfps_rod-datasaida         = w_zlest0039-datasaida.
    w_alv_nfps_rod-pesosaida         = w_zlest0039-pesosaida.
    w_alv_nfps_rod-datatransb        = w_zlest0039-datatransb.
    w_alv_nfps_rod-data_descarga     = w_zlest0220-data_descarga.
    w_alv_nfps_rod-pesotransb        = w_zlest0039-pesotransb.
    w_alv_nfps_rod-peso_descarga     = w_zlest0220-peso_descarga.
    w_alv_nfps_rod-saldo             = w_zlest0039-pesotransb
                                     - w_zlest0220-peso_descarga.
    w_alv_nfps_rod-pontoentrega      = w_zlest0039-pontoentrega.

    APPEND w_alv_nfps_rod           TO t_alv_nfps_rod.
  ENDLOOP.

ENDFORM.

**********************************************************************
* selecao dados nfps ferroviario
**********************************************************************
FORM f_selecao_dados_nfps_ferr.

*------------------------------------------------
* selecao
*------------------------------------------------
  SELECT *
    FROM zlest0219
    INTO TABLE t_zlest0219
   WHERE bukrs           IN s_empre
     AND cnpj_forn_nfps  IN s_cnpj
     AND nfps            IN s_ntserv
     AND data_nfps       IN s_period.

  CHECK t_zlest0219[] IS NOT INITIAL.

  SELECT bukrs, stcd1, branch
    FROM j_1bbranch
    INTO TABLE @DATA(t_j1bbranch)
     FOR ALL ENTRIES IN @t_zlest0219
   WHERE bukrs = @t_zlest0219-bukrs
     AND stcd1 = @t_zlest0219-cnpj_filial.

  IF t_j1bbranch[] IS NOT INITIAL.
    SELECT werks, name1
      FROM t001w
      INTO TABLE @DATA(t_t001w)
       FOR ALL ENTRIES IN @t_j1bbranch
     WHERE werks = @t_j1bbranch-branch.
  ENDIF.

  SELECT matnr, maktx
    FROM makt
    INTO TABLE @DATA(t_makt)
     FOR ALL ENTRIES IN @t_zlest0219
   WHERE matnr = @t_zlest0219-produto
     AND spras = @sy-langu.

  SELECT kunnr, name1, stcd1
    FROM kna1
    INTO TABLE @DATA(t_kna1)
     FOR ALL ENTRIES IN @t_zlest0219
   WHERE stcd1 = @t_zlest0219-cnpj_transbordo.

*------------------------------------------------
* gerar alv
*------------------------------------------------
  FREE: t_alv_nfps_fer.

  LOOP AT t_zlest0219 INTO w_zlest0219.

    CLEAR: w_alv_nfps_fer.

    READ TABLE t_j1bbranch INTO DATA(w_j1bbranch) WITH KEY bukrs = w_zlest0219-bukrs
                                                           stcd1 = w_zlest0219-cnpj_filial.

    READ TABLE t_t001w INTO DATA(w_t001w) WITH KEY werks = w_j1bbranch-branch.
    IF sy-subrc = 0.
      w_alv_nfps_fer-t001w_name1   = w_t001w-name1.
    ENDIF.

    READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_zlest0219-produto.
    IF sy-subrc = 0.
      w_alv_nfps_fer-maktx         = w_makt-maktx.
    ENDIF.

    READ TABLE t_kna1 INTO DATA(w_kna1) WITH KEY stcd1 = w_zlest0219-cnpj_transbordo.
    IF sy-subrc = 0.
      w_alv_nfps_fer-kna1_name1    = w_kna1-name1.
      w_alv_nfps_fer-kna1_kunnr    = w_kna1-kunnr.
    ENDIF.

    w_alv_nfps_fer-bukrs           = w_zlest0219-bukrs.
    w_alv_nfps_fer-nfps            = w_zlest0219-nfps.
    w_alv_nfps_fer-data_nfps       = w_zlest0219-data_nfps.
    w_alv_nfps_fer-valor_servico   = w_zlest0219-valor_servico.
    w_alv_nfps_fer-tarifa          = w_zlest0219-tarifa.
    w_alv_nfps_fer-matnr           = w_zlest0219-produto.
    w_alv_nfps_fer-dcl             = w_zlest0219-dcl.
    w_alv_nfps_fer-idvagao         = w_zlest0219-idvagao && w_zlest0219-serie_vagao.
    w_alv_nfps_fer-peso_vagao      = w_zlest0219-peso_vagao.
    w_alv_nfps_fer-data_carga      = w_zlest0219-data_carga.
    w_alv_nfps_fer-cnpj_transbordo = w_zlest0219-cnpj_transbordo.

    APPEND w_alv_nfps_fer         TO t_alv_nfps_fer.
  ENDLOOP.

ENDFORM.

**********************************************************************
* exibir NFPS
**********************************************************************
FORM f_exibir_dados_nfps.

  CALL SCREEN 100.

ENDFORM.

**********************************************************************
* selecao dados nfpod
**********************************************************************
FORM f_selecao_dados_nfprod.

  DATA: w_campos_nfe         TYPE zde_campos_nfe.

  CREATE OBJECT zcl_util.

*------------------------------------------------
* selecao
*------------------------------------------------
  SELECT *
    FROM zlest0039
    INTO TABLE t_zlest0039
   WHERE bukrs           IN s_empre2
     AND werks           IN s_werks
     AND matnr           IN s_matnr
     AND nfenum          IN s_nfenum
     AND pontotransb     IN s_ptrans
     AND pontoentrega    IN s_pentre
     AND datatransb      IN s_perio2.

  CHECK t_zlest0039[] IS NOT INITIAL.

* LOOP AT t_zlest0039  INTO w_zlest0039.
*   w_zlest0039-cnpj_14   = w_zlest0039-cnpj.
*   w_zlest0039-nfenum_9  = w_zlest0039-nfenum.
*   w_zlest0039-serie_3   = w_zlest0039-serie.
*   MODIFY t_zlest0039 FROM w_zlest0039 INDEX sy-tabix.
* ENDLOOP.

  SELECT *
       FROM zlest0041
       INTO TABLE t_zlest0041
        FOR ALL ENTRIES IN t_zlest0039
      WHERE docnum = t_zlest0039-docnum.

  SELECT *
    FROM zlest0220
    INTO TABLE t_zlest0220
     FOR ALL ENTRIES IN t_zlest0039
   WHERE chave_nf_prod    = t_zlest0039-chave_nfe.

* SELECT *
*   FROM zlest0220
*   INTO TABLE t_zlest0220
*    FOR ALL ENTRIES IN t_zlest0039
*  WHERE cnpj_transbordo  = t_zlest0039-cnpj
*    AND nfps             = t_zlest0039-nfenum_9
*    AND serie            = t_zlest0039-serie_3.

  SELECT werks, name1
    FROM t001w
    INTO TABLE @DATA(t_t001w)
     FOR ALL ENTRIES IN @t_zlest0039
   WHERE werks = @t_zlest0039-werks.

  SELECT matnr, maktx
    FROM makt
    INTO TABLE @DATA(t_makt)
     FOR ALL ENTRIES IN @t_zlest0039
   WHERE matnr = @t_zlest0039-matnr
     AND spras = @sy-langu.

  SELECT lifnr, name1, ort01
    FROM lfa1
    INTO TABLE @DATA(t_lfa1)
     FOR ALL ENTRIES IN @t_zlest0039
   WHERE lifnr = @t_zlest0039-pontoentrega.

  SELECT kunnr, name1, ort01
    FROM kna1
    INTO TABLE @DATA(t_kna1)
     FOR ALL ENTRIES IN @t_zlest0039
   WHERE kunnr = @t_zlest0039-transb_efetivo.

*------------------------------------------------
* gerar alv
*------------------------------------------------
  FREE: t_alv_nfprod.

  LOOP AT t_zlest0039 INTO w_zlest0039.

    CLEAR: w_alv_nfprod, w_zlest0220, w_icon.

    READ TABLE t_zlest0220  INTO w_zlest0220 WITH KEY chave_nf_prod  = w_zlest0039-chave_nfe.

    IF sy-subrc = 0.
      l_icon_name = 'ICON_GREEN_LIGHT'.
    ELSE.
      l_icon_name = 'ICON_YELLOW_LIGHT'.
    ENDIF.

    READ TABLE t_icon  INTO w_icon  WITH KEY name = l_icon_name
                                    BINARY SEARCH.
    w_alv_nfprod-status        = w_icon-id.

    READ TABLE t_t001w INTO DATA(w_t001w) WITH KEY werks = w_zlest0039-werks.
    IF sy-subrc = 0.
      w_alv_nfprod-t001w_name1 = w_t001w-name1.
    ENDIF.

    READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_zlest0039-matnr.
    IF sy-subrc = 0.
      w_alv_nfprod-maktx         = w_makt-maktx.
    ENDIF.

    READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY lifnr = w_zlest0039-pontoentrega.
    IF sy-subrc = 0.
      w_alv_nfprod-lfa1_name1    = w_lfa1-name1.
      w_alv_nfprod-lfa1_ort01    = w_lfa1-ort01.
    ENDIF.

    READ TABLE t_kna1 INTO DATA(w_kna1) WITH KEY kunnr = w_zlest0039-transb_efetivo.
    IF sy-subrc = 0.
      w_alv_nfprod-kna1_name1    = w_kna1-name1.
      w_alv_nfprod-kna1_ort01    = w_kna1-ort01.
    ENDIF.

    w_campos_nfe               = zcl_util->get_atributos_nfe( w_zlest0220-chave_nf_prod ).

    w_alv_nfprod-bukrs           = w_zlest0039-bukrs.
    w_alv_nfprod-nfps            = w_zlest0220-nfps.
    w_alv_nfprod-valor_servico   = w_zlest0220-valor_servico.
    w_alv_nfprod-matnr           = w_zlest0039-matnr.
    w_alv_nfprod-transb_efetivo  = w_zlest0039-transb_efetivo.
    w_alv_nfprod-nfenum          = w_zlest0039-nfenum.
    w_alv_nfprod-serie           = w_zlest0039-serie.
*    w_alv_nfprod-nfenum_terc     = w_campos_nfe-nfnum9.
*    w_alv_nfprod-serie_terc      = w_campos_nfe-serie.
    w_alv_nfprod-datasaida       = w_zlest0039-datasaida.
    w_alv_nfprod-pesosaida       = w_zlest0039-pesosaida.
    w_alv_nfprod-datatransb      = w_zlest0039-datatransb.
    w_alv_nfprod-pesotransb      = w_zlest0039-pesotransb.
    w_alv_nfprod-pontoentrega    = w_zlest0039-pontoentrega.

    READ TABLE t_zlest0041  INTO w_zlest0041 WITH KEY docnum    = w_zlest0039-docnum.
    IF sy-subrc = 0.
      w_alv_nfprod-nfenum_terc     = w_zlest0041-nr_nf.
      w_alv_nfprod-serie_terc      = w_zlest0041-serie.
    ENDIF.

    APPEND w_alv_nfprod         TO t_alv_nfprod.
  ENDLOOP.

ENDFORM.

**********************************************************************
* exibir NFPROD
**********************************************************************
FORM f_exibir_dados_nfprod.

  CALL SCREEN 100.

ENDFORM.

**********************************************************************
* carregar arquivo
**********************************************************************
FORM f_carrega_arquivo.

  DATA: l_erro TYPE char1,
        l_cols TYPE i.

  FREE: t_file_rod,
        t_file_fer.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = text-130.

*----------------------------------------
* upload excel
*----------------------------------------
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 100
      i_end_row               = 30000
*     i_end_col               = 256
*     i_end_row               = 65536
    TABLES
      intern                  = t_tab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH text-100 p_file DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*----------------------------------------
* carrega tabela interna
*----------------------------------------
  FREE: l_erro, l_cols.

  IF p_rodo = abap_true.
    LOOP AT t_tab INTO w_tab.
      l_cols = l_cols + 1.
      ASSIGN COMPONENT w_tab-col OF STRUCTURE w_file_rod TO <fs_fld>.
      <fs_fld> = w_tab-value.
      AT END OF row.
        IF l_cols <> 13.
          l_erro = abap_true.
        ENDIF.
        APPEND w_file_rod TO t_file_rod.
        FREE l_cols.
      ENDAT.
    ENDLOOP.
  ELSE.
    LOOP AT t_tab INTO w_tab.
      l_cols = l_cols + 1.
      ASSIGN COMPONENT w_tab-col OF STRUCTURE w_file_fer TO <fs_fld>.
      <fs_fld> = w_tab-value.
      AT END OF row.
        IF l_cols <> 12.
          l_erro = abap_true.
        ENDIF.
        APPEND w_file_fer TO t_file_fer.
        FREE l_cols.
      ENDAT.
    ENDLOOP.
  ENDIF.

  IF l_erro = abap_true.
    MESSAGE s024(sd) WITH text-115 text-116 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

**********************************************************************
* processamento rodoviario
**********************************************************************
FORM f_processa_rodoviario.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = text-130.

  DELETE t_file_rod INDEX 1.

  LOOP AT t_file_rod INTO w_file_rod.

    l_tabix = sy-tabix.

    CONDENSE w_file_rod-cnpj_filial      NO-GAPS.
    CONDENSE w_file_rod-cnpj_forn        NO-GAPS.
    CONDENSE w_file_rod-produto          NO-GAPS.
    CONDENSE w_file_rod-nfps             NO-GAPS.
    CONDENSE w_file_rod-data_nfps        NO-GAPS.
    CONDENSE w_file_rod-chave_nf_prod    NO-GAPS.
    CONDENSE w_file_rod-placa_cav        NO-GAPS.
    CONDENSE w_file_rod-peso_nf          NO-GAPS.
    CONDENSE w_file_rod-data_descarga    NO-GAPS.
    CONDENSE w_file_rod-peso_descarga    NO-GAPS.
    CONDENSE w_file_rod-tarifa           NO-GAPS.
    CONDENSE w_file_rod-valor_servico    NO-GAPS.
    CONDENSE w_file_rod-cnpj_transbordo  NO-GAPS.

*------------------------------
*---empresa
*------------------------------
    SELECT bukrs
      INTO @DATA(l_bukrs)
      FROM t001z
        UP TO 1 ROWS
     WHERE paval = @w_file_rod-cnpj_filial(8).
    ENDSELECT.

    IF sy-subrc = 0.
      w_file_rod-bukrs = l_bukrs.
    ENDIF.

*------------------------------
*---material
*------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = w_file_rod-produto
      IMPORTING
        output       = w_file_rod-produto
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

*------------------------------
*---NFPS
*------------------------------
    l_nfps           = w_file_rod-nfps.
    w_file_rod-nfps  = l_nfps.

*------------------------------
*---DATA NFPS
*------------------------------
    l_data_char          = w_file_rod-data_nfps.
    w_file_rod-data_nfps = l_data_char+6(4) && l_data_char+3(2) && l_data_char(2).

*------------------------------
*---DATA descarga
*------------------------------
    l_data_char              = w_file_rod-data_descarga.
    w_file_rod-data_descarga = l_data_char+6(4) && l_data_char+3(2) && l_data_char(2).

*------------------------------
*---pesos
*------------------------------
    w_file_rod-peso_descarga = w_file_rod-peso_descarga * 1000.
    w_file_rod-peso_nf       = w_file_rod-peso_nf       * 1000.

*------------------------------
*---tarifa
*------------------------------
    PERFORM f_change_text CHANGING w_file_rod-tarifa.

*------------------------------
*---valor servico
*------------------------------
    PERFORM f_change_text CHANGING w_file_rod-valor_servico.

    MODIFY t_file_rod         FROM w_file_rod INDEX l_tabix.
  ENDLOOP.

ENDFORM.

**********************************************************************
* salvar rodoviario
**********************************************************************
FORM f_salva_rodoviario.

  DATA: l_cont       TYPE i,
        w_campos_nfe TYPE zde_campos_nfe.

  CREATE OBJECT zcl_util.

  FREE: t_zlest0220,
        t_alv_rod.

  CHECK t_file_rod[] IS NOT INITIAL.

  t_file_rod_aux[] = t_file_rod[].
  DELETE t_file_rod_aux WHERE chave_nf_prod IS INITIAL.

  IF t_file_rod_aux[] IS NOT INITIAL.
    SELECT *
      FROM zlest0039
      INTO TABLE t_zlest0039
       FOR ALL ENTRIES IN t_file_rod_aux
     WHERE chave_nfe = t_file_rod_aux-chave_nf_prod.


    "Seleção tabela.
    t_active = VALUE #( FOR l IN t_file_rod_aux ( stcd1  = l-chave_nf_prod+6(14)
                                                  serie  = l-chave_nf_prod+22(3)
                                                  nfnum9 = l-chave_nf_prod+25(9)
                                                  cnpj_filial = l-cnpj_filial
     ) ).

    IF t_active IS NOT INITIAL.
      SELECT * FROM lfa1 INTO TABLE t_lfa1
        FOR ALL ENTRIES IN t_active
        WHERE stcd1  EQ  t_active-stcd1
          AND land1  EQ 'BR'.

      SELECT * FROM j_1bbranch INTO TABLE t_j_1bbranch
       FOR ALL ENTRIES IN t_active
       WHERE stcd1  EQ  t_active-cnpj_filial.
    ENDIF.
  ENDIF.

  SELECT *
    FROM zlest0220
    INTO TABLE t_zlest0220
     FOR ALL ENTRIES IN t_file_rod
   WHERE bukrs           = t_file_rod-bukrs
     AND cnpj_filial     = t_file_rod-cnpj_filial
     AND cnpj_transbordo = t_file_rod-cnpj_transbordo.
*    AND nfps            = t_file_rod-nfps.

  SELECT kunnr, stcd1
    FROM kna1
    INTO TABLE @DATA(t_kna1)
     FOR ALL ENTRIES IN @t_file_rod
   WHERE stcd1  = @t_file_rod-cnpj_transbordo.

  t_file_rod_aux[] = t_file_rod[].

  LOOP AT t_file_rod INTO w_file_rod.

    CLEAR: w_alv_rod,
           l_cont,
           w_zlest0220,
           w_zlest0039.

    MOVE 'ICON_RED_LIGHT'          TO l_icon_name.
    MOVE-CORRESPONDING w_file_rod  TO w_alv_rod.

    READ TABLE t_icon      INTO w_icon   WITH KEY name = l_icon_name
                                         BINARY SEARCH.

    w_campos_nfe     = zcl_util->get_atributos_nfe( w_file_rod-chave_nf_prod ).

    IF w_file_rod-bukrs IS INITIAL.
      w_alv_rod-erro     = abap_true.
      w_alv_rod-msg_erro = text-110.
    ENDIF.

    IF w_file_rod-chave_nf_prod IS INITIAL.
      w_alv_rod-erro     = abap_true.
      w_alv_rod-msg_erro = text-125.
    ENDIF.

    READ TABLE t_zlest0220 INTO w_zlest0220 WITH KEY nfps = w_file_rod-nfps.
    IF sy-subrc = 0.
      w_alv_rod-erro     = abap_true.
      w_alv_rod-msg_erro = text-111.
    ENDIF.

    READ TABLE t_zlest0039 INTO w_zlest0039 WITH KEY chave_nfe = w_file_rod-chave_nf_prod.
    IF sy-subrc <> 0.

      "Procurar informação com segunda seleção.
      READ TABLE t_active INTO DATA(ws_active) WITH KEY stcd1 =  w_file_rod-chave_nf_prod+6(14)
                                                        serie =  w_file_rod-chave_nf_prod+22(3)
                                                        nfnum9 =  w_file_rod-chave_nf_prod+25(9).

      IF sy-subrc EQ 0.
        READ TABLE t_lfa1 INTO DATA(ws_lfa1) WITH KEY stcd1 = ws_active-stcd1.
        IF sy-subrc EQ 0.
          READ TABLE t_j_1bbranch INTO DATA(ws_j_1bbranch) WITH KEY stcd1 = ws_active-cnpj_filial.
          IF sy-subrc EQ 0.
            SELECT SINGLE * FROM zlest0041 INTO ws_zlest0041
              WHERE centro_comprador  = ws_j_1bbranch-branch
                AND  nr_nf            = ws_active-nfnum9
                AND cod_cliente       = ws_lfa1-lifnr
                AND serie             = ws_active-serie.

            IF sy-subrc EQ 0.
              SELECT SINGLE * FROM zlest0039
                INTO ws_zlest0039
                WHERE docnum EQ ws_zlest0041-docnum.

              IF sy-subrc NE 0.
                w_alv_rod-erro     = abap_true.
                w_alv_rod-msg_erro = text-117.
              ELSE.
                w_zlest0039 = ws_zlest0039.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
    ENDIF.

    LOOP AT t_file_rod_aux INTO w_file_rod_aux WHERE bukrs           = w_file_rod-bukrs
                                                 AND cnpj_filial     = w_file_rod-cnpj_filial
                                                 AND cnpj_transbordo = w_file_rod-cnpj_transbordo
                                                 AND nfps            = w_file_rod-nfps
                                                 AND chave_nf_prod   = w_file_rod-chave_nf_prod.
      l_cont = l_cont + 1.
    ENDLOOP.

    IF l_cont > 1.
      w_alv_rod-erro     = abap_true.
      w_alv_rod-msg_erro = text-113.
    ENDIF.

    READ TABLE t_kna1 INTO DATA(w_kna1) WITH KEY stcd1 = w_file_rod-cnpj_transbordo.
    IF sy-subrc <> 0.
      w_alv_rod-erro     = abap_true.
      w_alv_rod-msg_erro = text-140.
    ELSE.
      IF w_kna1-kunnr <> w_zlest0039-transb_efetivo.
        w_alv_rod-erro     = abap_true.
        w_alv_rod-msg_erro = text-141.
      ENDIF.
    ENDIF.

    SELECT docnum
      INTO @DATA(l_docnum)
      FROM j_1bnfe_active
        UP TO 1 ROWS
     WHERE regio   = @w_campos_nfe-regio
       AND nfyear  = @w_campos_nfe-nfyear
       AND nfmonth = @w_campos_nfe-nfmonth
       AND stcd1   = @w_campos_nfe-stcd1
       AND model   = @w_campos_nfe-model
       AND serie   = @w_campos_nfe-serie
       AND nfnum9  = @w_campos_nfe-nfnum9
       AND docnum9 = @w_campos_nfe-docnum9
       AND cdv     = @w_campos_nfe-cdv.
    ENDSELECT.

    IF sy-subrc <> 0.
      w_alv_rod-erro     = abap_true.
      w_alv_rod-msg_erro = text-118.
    ENDIF.

    SELECT matnr
      INTO @DATA(l_matnr)
      FROM mara
        UP TO 1 ROWS
     WHERE matnr = @w_file_rod-produto.
    ENDSELECT.

    IF sy-subrc <> 0.
      w_alv_rod-erro     = abap_true.
      w_alv_rod-msg_erro = text-119.
    ENDIF.

    MOVE w_icon-id           TO w_alv_rod-status.

*-------------------------------------
* gerar log erro ou gravar tabela
*-------------------------------------
    IF w_alv_rod-erro = abap_true.
      APPEND w_alv_rod              TO t_alv_rod.
    ELSE.
      CLEAR w_zlest0220.
      MOVE-CORRESPONDING w_file_rod TO w_zlest0220.
      MOVE '0'                      TO w_zlest0220-serie.
      MOVE 'R'                      TO w_zlest0220-tipo_carga.
      MOVE sy-datum                 TO w_zlest0220-zdt_atual.
      MOVE sy-uzeit                 TO w_zlest0220-zhr_atual.
      MOVE sy-uname                 TO w_zlest0220-zsd_user.
      MODIFY zlest0220            FROM w_zlest0220.
      COMMIT WORK AND WAIT.
    ENDIF.

    CLEAR: ws_lfa1, ws_j_1bbranch, ws_zlest0039, ws_zlest0041, ws_active.
  ENDLOOP.

  IF t_alv_rod[] IS INITIAL.
    MESSAGE s024(sd) WITH text-112.
    EXIT.
  ENDIF.

*-------------------------------------
* exibe log de erros
*-------------------------------------
  CALL SCREEN 100.

ENDFORM.

**********************************************************************
* tratamento ferroviario
**********************************************************************
FORM f_processa_ferroviario.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = text-130.

  DELETE t_file_fer INDEX 1.

  LOOP AT t_file_fer INTO w_file_fer.

    l_tabix = sy-tabix.

    CONDENSE w_file_fer-cnpj_filial      NO-GAPS.
    CONDENSE w_file_fer-cnpj_forn_nfps   NO-GAPS.
    CONDENSE w_file_fer-produto          NO-GAPS.
    CONDENSE w_file_fer-nfps             NO-GAPS.
    CONDENSE w_file_fer-data_nfps        NO-GAPS.
    CONDENSE w_file_fer-dcl              NO-GAPS.
    CONDENSE w_file_fer-data_carga       NO-GAPS.
    CONDENSE w_file_fer-idvagao          NO-GAPS.
    CONDENSE w_file_fer-peso_vagao       NO-GAPS.
    CONDENSE w_file_fer-tarifa           NO-GAPS.
    CONDENSE w_file_fer-valor_servico    NO-GAPS.
    CONDENSE w_file_fer-cnpj_transbordo  NO-GAPS.

*------------------------------
*---empresa
*------------------------------
    SELECT bukrs
      INTO @DATA(l_bukrs)
      FROM t001z
        UP TO 1 ROWS
     WHERE paval = @w_file_fer-cnpj_filial(8).
    ENDSELECT.

    IF sy-subrc = 0.
      w_file_fer-bukrs = l_bukrs.
    ENDIF.

*------------------------------
*---material
*------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = w_file_fer-produto
      IMPORTING
        output       = w_file_fer-produto
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

*------------------------------
*---NFPS
*------------------------------
    l_nfps          = w_file_fer-nfps.
    w_file_fer-nfps = l_nfps.

*------------------------------
*---DATA NFPS
*------------------------------
    l_data_char          = w_file_fer-data_nfps.
    w_file_fer-data_nfps = l_data_char+6(4) && l_data_char+3(2) && l_data_char(2).

*------------------------------
*---DATA descarga
*------------------------------
    l_data_char              = w_file_fer-data_carga.
    w_file_fer-data_carga    = l_data_char+6(4) && l_data_char+3(2) && l_data_char(2).

*------------------------------
*---pesos
*------------------------------
    w_file_fer-peso_vagao    = w_file_fer-peso_vagao * 1000.

*------------------------------
*---tarifa
*------------------------------
    PERFORM f_change_text CHANGING w_file_fer-tarifa.

*------------------------------
*---valor servico
*------------------------------
    PERFORM f_change_text CHANGING w_file_fer-valor_servico.

    MODIFY t_file_fer         FROM w_file_fer INDEX l_tabix.
  ENDLOOP.

ENDFORM.

**********************************************************************
* salvar ferroviario
**********************************************************************
FORM f_salva_ferroviario.

  DATA: l_cont    TYPE i.

  FREE: t_zlest0219,
        t_alv_fer.

  CHECK t_file_fer[] IS NOT INITIAL.

  SELECT *
    FROM zlest0219
    INTO TABLE t_zlest0219
     FOR ALL ENTRIES IN t_file_fer
   WHERE bukrs           = t_file_fer-bukrs
     AND cnpj_filial     = t_file_fer-cnpj_filial
     AND cnpj_forn_nfps  = t_file_fer-cnpj_transbordo
     AND nfps            = t_file_fer-nfps
     AND dcl             = t_file_fer-dcl.

  t_file_fer_aux[] = t_file_fer[].

  LOOP AT t_file_fer INTO w_file_fer.

    CLEAR: w_alv_fer, l_cont.

    MOVE 'ICON_RED_LIGHT'          TO l_icon_name.
    MOVE-CORRESPONDING w_file_fer  TO w_alv_fer.

    READ TABLE t_icon      INTO w_icon   WITH KEY name = l_icon_name
                                         BINARY SEARCH.

    IF w_file_fer-bukrs IS INITIAL.
      w_alv_fer-erro     = abap_true.
      w_alv_fer-msg_erro = text-110.
    ENDIF.

    READ TABLE t_zlest0219 INTO w_zlest0219 WITH KEY bukrs          = w_file_fer-bukrs
                                                     cnpj_filial    = w_file_fer-cnpj_filial
                                                     cnpj_forn_nfps = w_file_fer-cnpj_transbordo
                                                     nfps           = w_file_fer-nfps
                                                     dcl            = w_file_fer-dcl.
    IF sy-subrc = 0.
      w_alv_fer-erro     = abap_true.
      w_alv_fer-msg_erro = text-111.
    ENDIF.

    READ TABLE t_zlest0219 INTO w_zlest0219 WITH KEY dcl  = w_file_fer-dcl.
    IF sy-subrc = 0.
      w_alv_fer-erro     = abap_true.
      w_alv_fer-msg_erro = text-114.
    ENDIF.

    LOOP AT t_file_fer_aux INTO w_file_fer_aux WHERE bukrs           = w_file_fer-bukrs
                                                 AND cnpj_filial     = w_file_fer-cnpj_filial
                                                 AND cnpj_transbordo = w_file_fer-cnpj_transbordo
                                                 AND nfps            = w_file_fer-nfps
                                                 AND dcl             = w_file_fer-dcl.
      l_cont = l_cont + 1.
    ENDLOOP.

    IF l_cont > 1.
      w_alv_fer-erro     = abap_true.
      w_alv_fer-msg_erro = text-113.
    ENDIF.

*-------------------------------------
* valida material
*-------------------------------------
    SELECT matnr
      INTO @DATA(l_matnr)
      FROM mara
        UP TO 1 ROWS
     WHERE matnr = @w_file_fer-produto.
    ENDSELECT.

    IF sy-subrc <> 0.
      w_alv_fer-erro     = abap_true.
      w_alv_fer-msg_erro = text-119.
    ENDIF.

*-------------------------------------
* valida cnpj filial
*-------------------------------------
    SELECT stcd1
      INTO @DATA(l_stcd1)
      FROM j_1bbranch
        UP TO 1 ROWS
     WHERE bukrs = @w_file_fer-bukrs
       AND stcd1 = @w_file_fer-cnpj_filial.
    ENDSELECT.

    IF sy-subrc <> 0.
      w_alv_fer-erro     = abap_true.
      w_alv_fer-msg_erro = text-132.
    ENDIF.

*-------------------------------------
* valida cnpj fornecedor
*-------------------------------------
    SELECT stcd1
      INTO @DATA(l_stcd_forn)
      FROM lfa1
        UP TO 1 ROWS
     WHERE stcd1 = @w_file_fer-cnpj_forn_nfps.
    ENDSELECT.

    IF sy-subrc <> 0.
      w_alv_fer-erro     = abap_true.
      w_alv_fer-msg_erro = text-133.
    ENDIF.

    MOVE w_icon-id           TO w_alv_fer-status.

*-------------------------------------
* gerar log erro ou gravar tabela
*-------------------------------------
    IF w_alv_fer-erro = abap_true.
      APPEND w_alv_fer               TO t_alv_fer.
    ELSE.
      CLEAR w_zlest0219.
      MOVE-CORRESPONDING w_file_fer TO w_zlest0219.
      MOVE w_alv_fer-idvagao(3)     TO w_zlest0219-idvagao.
      MOVE w_alv_fer-idvagao+3(08)  TO w_zlest0219-serie_vagao.
      MOVE sy-datum                 TO w_zlest0219-zdt_atual.
      MOVE sy-uzeit                 TO w_zlest0219-zhr_atual.
      MOVE sy-uname                 TO w_zlest0219-zsd_user.
      MODIFY zlest0219            FROM w_zlest0219.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDLOOP.

  IF t_alv_fer[] IS INITIAL.
    MESSAGE s024(sd) WITH text-112.
    EXIT.
  ENDIF.

*-------------------------------------
* exibe log de erros
*-------------------------------------
  CALL SCREEN 100.

ENDFORM.

**********************************************************************
* tratamento valores
**********************************************************************
FORM f_change_text CHANGING p_out_text.

  DATA: l_index     TYPE i,
        l_length    TYPE i,
        l_character TYPE c,
        l_allowed   TYPE string.

  TRANSLATE p_out_text TO LOWER CASE.

  l_allowed = '1234567890,'.
  l_length  = strlen( p_out_text ).
  l_index   = 0.

  WHILE l_length GT l_index.
    l_character = p_out_text+l_index(1).
    SEARCH l_allowed FOR l_character.
    IF sy-subrc NE 0.
      REPLACE l_character WITH space INTO p_out_text.
    ENDIF.
    l_index = l_index + 1.
  ENDWHILE.

  REPLACE ALL OCCURRENCES OF REGEX ',' IN p_out_text WITH '.'.
  CONDENSE p_out_text NO-GAPS.

ENDFORM.

**********************************************************************
* seleciona arquivo
**********************************************************************
FORM f_select_file USING p_filename TYPE localfile.

  DATA: l_subrc     LIKE sy-subrc,
        t_filetable TYPE filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Selecione o arquivo .xls'
      default_filename = '*.xls'
      multiselection   = ' '
    CHANGING
      file_table       = t_filetable
      rc               = l_subrc.

  READ TABLE t_filetable INTO p_filename INDEX 1.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

  DATA: wl_layout TYPE slis_layout_alv.

  PERFORM f_fieldcatalog.
  variante = VALUE #(
report = sy-repid
).

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-zebra        = abap_false.
* w_layout-edit         = abap_true. " Makes all Grid editable
*  w_layout-no_totarr    = abap_true.
*  w_layout-no_totexp    = abap_true.
*  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.
  w_layout-stylefname   = 'CELLSTYLES'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF g_grid IS INITIAL.
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'HEADER'.

    PERFORM f_alv_header .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    "Grafico 1
    CALL METHOD cl_gui_cfw=>flush.

    CREATE OBJECT: g_custom_container
       EXPORTING
         container_name = 'CC_IMG',
         picture
       EXPORTING
         parent = g_custom_container.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = cl_container_95.
*    ENDIF.

    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.

    CASE abap_true.
      WHEN p_import.
        CASE abap_true.
          WHEN p_rodo.
            CALL METHOD g_grid->set_table_for_first_display
              EXPORTING
                is_layout                     = w_layout
                i_save                        = 'A'
                it_toolbar_excluding          = t_function
                is_variant                    = variante
              CHANGING
                it_outtab                     = t_alv_rod[]
                it_fieldcatalog               = t_fieldcat
              EXCEPTIONS
                invalid_parameter_combination = 1
                program_error                 = 2
                too_many_lines                = 3
                OTHERS                        = 4.

          WHEN p_ferro.
            CALL METHOD g_grid->set_table_for_first_display
              EXPORTING
                is_layout                     = w_layout
                i_save                        = 'A'
                it_toolbar_excluding          = t_function
                is_variant                    = variante
              CHANGING
                it_outtab                     = t_alv_fer[]
                it_fieldcatalog               = t_fieldcat
              EXCEPTIONS
                invalid_parameter_combination = 1
                program_error                 = 2
                too_many_lines                = 3
                OTHERS                        = 4.
        ENDCASE.

      WHEN p_nfps.
        CASE abap_true.

          WHEN p_ro_nfp.
            CALL METHOD g_grid->set_table_for_first_display
              EXPORTING
                is_layout                     = w_layout
                i_save                        = 'A'
                it_toolbar_excluding          = t_function
                is_variant                    = variante
              CHANGING
                it_outtab                     = t_alv_nfps_rod[]
                it_fieldcatalog               = t_fieldcat
              EXCEPTIONS
                invalid_parameter_combination = 1
                program_error                 = 2
                too_many_lines                = 3
                OTHERS                        = 4.

          WHEN p_fe_nfp.
            CALL METHOD g_grid->set_table_for_first_display
              EXPORTING
                is_layout                     = w_layout
                i_save                        = 'A'
                it_toolbar_excluding          = t_function
                is_variant                    = variante
              CHANGING
                it_outtab                     = t_alv_nfps_fer[]
                it_fieldcatalog               = t_fieldcat
              EXCEPTIONS
                invalid_parameter_combination = 1
                program_error                 = 2
                too_many_lines                = 3
                OTHERS                        = 4.
        ENDCASE.

      WHEN p_nfprod.

        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
            it_toolbar_excluding          = t_function
            is_variant                    = variante
          CHANGING
            it_outtab                     = t_alv_nfprod[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

    ENDCASE.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

  wl_layout-colwidth_optimize = 'X'.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program       = v_report
*      is_variant               = gs_variant_c
*      i_callback_user_command  = 'USER_COMMAND'
*      i_callback_pf_status_set = 'PF_STATUS_SET'
*      it_fieldcat              = estrutura[]
*      is_layout                = wl_layout
*      i_save                   = 'X'
*      it_events                = events
*      is_print                 = t_print
*    TABLES
*      t_outtab                 = t_alv_nfps_rod.

*
*CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      it_fieldcat           = estrutura[]
*      i_save                = 'A'
*      i_screen_start_column = 3
*      i_screen_start_line   = 3
*      i_screen_end_column   = 140
*      i_screen_end_line     = 25
*    TABLES
*      t_outtab              = t_alv_nfps_rod.


ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  CASE abap_true.
    WHEN p_import.

      CASE abap_true.
        WHEN p_rodo.
          PERFORM f_estrutura_alv USING:
             01  ''      ''       'T_ALV_ROD'   'STATUS'              'Status'                      '05'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             02  'LFA1'  'STCD1'  'T_ALV_ROD'   'CNPJ_FILIAL'         'CNPJ Amaggi'                 '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             03  'LFA1'  'STCD1'  'T_ALV_ROD'   'CNPJ_FORN'           'CNPJ Fornecedor'             '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             04  'MARA'  'MATNR'  'T_ALV_ROD'   'PRODUTO'             'Produto'                     '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             05  ''      ''       'T_ALV_ROD'   'NFPS'                'NF Serviço'                  '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             06  ''      ''       'T_ALV_ROD'   'DATA_NFPS'           'Dt.Emissão NF Serviço'       '22'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             07  ''      ''       'T_ALV_ROD'   'CHAVE_NF_PROD'       'Chave NF Produto'            '46'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             08  ''      ''       'T_ALV_ROD'   'PLACA_CAV'           'Placa CMS'                   '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             09  ''      ''       'T_ALV_ROD'   'PESO_NF'             'Peso NF'                     '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             10  ''      ''       'T_ALV_ROD'   'DATA_DESCARGA'       'Data Descarga'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             11  ''      ''       'T_ALV_ROD'   'PESO_DESCARGA'       'Peso Descarga'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             12  ''      ''       'T_ALV_ROD'   'TARIFA'              'Tarifa'                      '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             13  ''      ''       'T_ALV_ROD'   'VALOR_SERVICO'       'Valor NFPS'                  '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             14  'LFA1'  'STCD1'  'T_ALV_ROD'   'CNPJ_TRANSBORDO'     'CNPJ Transbordo'             '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             15  ''      ''       'T_ALV_ROD'   'MSG_ERRO'            'Mensagem'                    '50'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

        WHEN p_ferro.
          PERFORM f_estrutura_alv USING:
             01  ''      ''       'T_ALV_FER'   'STATUS'              'Status'                      '05'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             02  'LFA1'  'STCD1'  'T_ALV_FER'   'CNPJ_FILIAL'         'CNPJ Amaggi'                 '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             03  'LFA1'  'STCD1'  'T_ALV_FER'   'CNPJ_FORN_NFPS'      'CNPJ Fornecedor'             '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             04  'MARA'  'MATNR'  'T_ALV_FER'   'PRODUTO'             'Produto'                     '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             05  ''      ''       'T_ALV_FER'   'NFPS'                'NF Serviço'                  '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             06  ''      ''       'T_ALV_FER'   'DATA_NFPS'           'Dt.Emissão NF Serviço'       '22'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             07  ''      ''       'T_ALV_FER'   'DCL'                 'DCL'                         '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             08  ''      ''       'T_ALV_FER'   'DATA_CARGA'          'Data Carga'                  '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             09  ''      ''       'T_ALV_FER'   'IDVAGAO'             'Placa Vagão'                 '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             10  ''      ''       'T_ALV_FER'   'PESO_VAGAO'          'Peso Descarga'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             11  ''      ''       'T_ALV_FER'   'TARIFA'              'Tarifa'                      '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             12  ''      ''       'T_ALV_FER'   'VALOR_SERVICO'       'Valor NFPS'                  '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             13  'LFA1'  'STCD1'  'T_ALV_FER'   'CNPJ_TRANSBORDO'     'CNPJ Transbordo'             '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             14  ''      ''       'T_ALV_FER'   'MSG_ERRO'            'Mensagem'                    '50'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.
      ENDCASE.

    WHEN p_nfps.
      CASE abap_true.

        WHEN p_ro_nfp.
          PERFORM f_estrutura_alv USING:
             01  ''      ''       'T_ALV_NFPS_ROD'  'BUKRS'               'Empresa'                     '08'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             02  ''      ''       'T_ALV_NFPS_ROD'  'T001W_NAME1'         'Centro Remetente'            '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             03  ''      ''       'T_ALV_NFPS_ROD'  'CNPJ_FORN'           'CNPJ Forn.NFPS Serv'         '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             04  ''      ''       'T_ALV_NFPS_ROD'  'DESC_CNPJ_FORN'      'Descr.CNPJ Forn'             '40'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             05  ''      ''       'T_ALV_NFPS_ROD'  'NFPS'                'NF.Prest.Serv.'              '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             06  ''      ''       'T_ALV_NFPS_ROD'  'DATA_NFPS'           'Data NFPS'                   '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             07  ''      ''       'T_ALV_NFPS_ROD'  'CHAVE_NF_PROD'       'Chave NF Prod'               '44'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             09  ''      ''       'T_ALV_NFPS_ROD'  'VALOR_SERVICO'       'Valor Serv.'                 '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             10  'MARA'  'MATNR'  'T_ALV_NFPS_ROD'  'MATNR'               'Cód.Produto'                 '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             11  ''      ''       'T_ALV_NFPS_ROD'  'MAKTX'               'Descrição Produto'           '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             12  'KNA1'  'KUNNR'  'T_ALV_NFPS_ROD'  'TRANSB_EFETIVO'      'Cód Transb. Efetivo'         '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             13  ''      ''       'T_ALV_NFPS_ROD'  'KNA1_NAME1'          'Desc.Transb.Efetivo'         '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             14  ''      ''       'T_ALV_NFPS_ROD'  'KNA1_ORT01'          'Cidade Transb.Efetivo'       '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             15  ''      ''       'T_ALV_NFPS_ROD'  'NFENUM'              'Nota Fiscal'                 '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             16  ''      ''       'T_ALV_NFPS_ROD'  'SERIE'               'Série NF'                    '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             17  ''      ''       'T_ALV_NFPS_ROD'  'NFENUM_TERC'         'NF Terceiro'                 '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             18  ''      ''       'T_ALV_NFPS_ROD'  'SERIE_TERC'          'Série NF.Terc.'              '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             19  ''      ''       'T_ALV_NFPS_ROD'  'DATASAIDA'           'Data Emissão'                '14'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             20  ''      ''       'T_ALV_NFPS_ROD'  'PESOSAIDA'           'Peso NF'                     '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             21  ''      ''       'T_ALV_NFPS_ROD'  'DATATRANSB'          'Data Descarga'               '14'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             22  ''      ''       'T_ALV_NFPS_ROD'  'PESOTRANSB'          'Peso Descarga'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             23  ''      ''       'T_ALV_NFPS_ROD'  'PESO_DESCARGA'       'Peso Validado'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             24  ''      ''       'T_ALV_NFPS_ROD'  'SALDO'               'Saldo'                       '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             25  'LFA1'  'LIFNR'  'T_ALV_NFPS_ROD'  'PONTOENTREGA'        'Cód.Destino'                 '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             26  ''      ''       'T_ALV_NFPS_ROD'  'LFA1_NAME1'          'Descrição Destino'           '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             27  ''      ''       'T_ALV_NFPS_ROD'  'LFA1_ORT01'          'Cidade Destino'              '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

        WHEN p_fe_nfp.
          PERFORM f_estrutura_alv USING:
             01  ''      ''       'T_ALV_NFPS_FER'  'BUKRS'               'Empresa'                     '08'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             02  ''      ''       'T_ALV_NFPS_FER'  'T001W_NAME1'         'Centro Remetente'            '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             03  ''      ''       'T_ALV_NFPS_FER'  'NFPS'                'NF.Prest.Serv.'              '14'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             04  ''      ''       'T_ALV_NFPS_FER'  'DATA_NFPS'           'Data NFPS'                   '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             05  ''      ''       'T_ALV_NFPS_FER'  'VALOR_SERVICO'       'Valor Serviço'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             06  ''      ''       'T_ALV_NFPS_FER'  'TARIFA'              'Valor Tarifa'                '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             07  'MARA'  'MATNR'  'T_ALV_NFPS_FER'  'MATNR'               'Cód.Produto'                 '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             08  ''      ''       'T_ALV_NFPS_FER'  'MAKTX'               'Descrição Produto'           '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             09  ''      ''       'T_ALV_NFPS_FER'  'DCL'                 'Nr.DCL'                      '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             10  ''      ''       'T_ALV_NFPS_FER'  'IDVAGAO'             'PLaca Vagão'                 '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             11  ''      ''       'T_ALV_NFPS_FER'  'PESO_VAGAO'          'Peso Vagão'                  '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             12  ''      ''       'T_ALV_NFPS_FER'  'DATA_CARGA'          'Data Carga'                  '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             13  'KNA1'  'STCD1'  'T_ALV_NFPS_FER'  'CNPJ_TRANSBORDO'     'CNPJ Transbordo'             '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             14  'KNA1'  'KUNNR'  'T_ALV_NFPS_FER'  'KNA1_KUNNR'          'Cód.Transbordo'              '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
             15  ''      ''       'T_ALV_NFPS_FER'  'KNA1_NAME1'          'Descr.Transbordo'            '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.
      ENDCASE.

    WHEN p_nfprod.
      PERFORM f_estrutura_alv USING:
         01  ''      ''       'T_ALV_NFPROD'    'STATUS'              'Status'                      '05'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         02  ''      ''       'T_ALV_NFPROD'    'BUKRS'               'Centro'                      '08'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         03  ''      ''       'T_ALV_NFPROD'    'T001W_NAME1'         'Centro Remetente'            '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         05  'MARA'  'MATNR'  'T_ALV_NFPROD'    'MATNR'               'Cód.Produto'                 '18'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         06  ''      ''       'T_ALV_NFPROD'    'MAKTX'               'Descrição Produto'           '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         07  'KNA1'  'KUNNR'  'T_ALV_NFPROD'    'TRANSB_EFETIVO'      'Cód Transb. Efetivo'         '20'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         08  ''      ''       'T_ALV_NFPROD'    'KNA1_NAME1'          'Desc.Transb.Efetivo'         '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         09  ''      ''       'T_ALV_NFPROD'    'KNA1_ORT01'          'Cidade Transb.Efetivo'       '25'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         10  ''      ''       'T_ALV_NFPROD'    'NFENUM'              'Nota Fiscal'                 '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         11  ''      ''       'T_ALV_NFPROD'    'SERIE'               'Série NF'                    '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         12  ''      ''       'T_ALV_NFPROD'    'NFENUM_TERC'         'NF Terceiro'                 '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         13  ''      ''       'T_ALV_NFPROD'    'SERIE_TERC'          'Série NF.Terc.'              '10'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         14  ''      ''       'T_ALV_NFPROD'    'DATASAIDA'           'Data Emissão'                '14'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         15  ''      ''       'T_ALV_NFPROD'    'PESOSAIDA'           'Peso NF'                     '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         16  ''      ''       'T_ALV_NFPROD'    'DATATRANSB'          'Data Descarga'               '14'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         17  ''      ''       'T_ALV_NFPROD'    'PESOTRANSB'          'Peso Descarga'               '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         18  'LFA1'  'LIFNR'  'T_ALV_NFPROD'    'PONTOENTREGA'        'Cód.Destino'                 '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         19  ''      ''       'T_ALV_NFPROD'    'LFA1_NAME1'          'Descrição Destino'           '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         20  ''      ''       'T_ALV_NFPROD'    'LFA1_ORT01'          'Cidade Destino'              '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         21  ''      ''       'T_ALV_NFPROD'    'NFPS'                'NF.Prest.Serv.'              '12'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
         22  ''      ''       'T_ALV_NFPROD'    'VALOR_SERVICO'       'Valor Serv.'                 '15'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.


  ENDCASE.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "16

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
*  cabecalho
**********************************************************************
FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch.

  CASE abap_true.
    WHEN p_import.
      CASE abap_true.
        WHEN p_rodo.
          wl_linha = text-120.
        WHEN p_ferro.
          wl_linha = text-121.
      ENDCASE.

    WHEN p_nfps.
      CASE abap_true.
        WHEN p_ro_nfp.
          wl_linha = text-122.
        WHEN p_fe_nfp.
          wl_linha = text-126.
      ENDCASE.

    WHEN p_nfprod.
      wl_linha     = text-123.
  ENDCASE.

  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

  CALL METHOD obj_dyndoc_id->new_line.

  CASE abap_true.
    WHEN p_import.
      CONCATENATE  'Arquivo:' p_file INTO wl_linha SEPARATED BY space.
      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.

    WHEN p_nfps.
      CONCATENATE  'Empresa....:' s_empre-low
             INTO wl_linha SEPARATED BY space.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.

      READ TABLE s_period INDEX 1.

      wl_data1 = s_period-low+6(2)  && '.' && s_period-low+4(2)  && '.' && s_period-low(4).
      wl_data2 = s_period-high+6(2) && '.' && s_period-high+4(2) && '.' && s_period-high(4).

      IF s_period-high IS NOT INITIAL.
        CONCATENATE  'Período......:' wl_data1 'a' wl_data2
               INTO wl_linha SEPARATED BY space.
      ELSE.
        CONCATENATE  'Período......:' wl_data1
               INTO wl_linha SEPARATED BY space.
      ENDIF.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.

    WHEN p_nfprod.
      CONCATENATE  'Empresa....:' s_empre2-low
             INTO wl_linha SEPARATED BY space.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.

      READ TABLE s_perio2 INDEX 1.

      wl_data1 = s_perio2-low+6(2)  && '.' && s_perio2-low+4(2)  && '.' && s_perio2-low(4).
      wl_data2 = s_perio2-high+6(2) && '.' && s_perio2-high+4(2) && '.' && s_perio2-high(4).

      IF s_perio2-high IS NOT INITIAL.
        CONCATENATE  'Período......:' wl_data1 'a' wl_data2
               INTO wl_linha SEPARATED BY space.
      ELSE.
        CONCATENATE  'Período......:' wl_data1
               INTO wl_linha SEPARATED BY space.
      ENDIF.

      wl_text = wl_linha.
      CALL METHOD obj_dyndoc_id->new_line.

      CALL METHOD obj_dyndoc_id->add_text
        EXPORTING
          text         = wl_text
          sap_fontsize = cl_dd_area=>list_normal.

  ENDCASE.

ENDFORM.                    " ZF_ALV_HEADER

**********************************************************************
*  imagen
**********************************************************************
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

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

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZLESR0154'.
  SET TITLEBAR  'ZLESR0154'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  FREE: t_rows[].

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

**********************************************************************
**********************************************************************
