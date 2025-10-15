*&---------------------------------------------------------------------*
*& Report  ZMMR122
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr122.

TABLES: j_1bbranch,ckmlhd, ckmlpp, mlkey.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

TYPES:
  BEGIN OF ty_matnr,
    matnr TYPE mseg-matnr,
    werks TYPE mseg-werks,
  END OF ty_matnr,

  BEGIN OF ty_mara,
    matnr TYPE mara-matnr,
    meins TYPE mara-meins,
  END OF ty_mara,

  BEGIN OF ty_docs,
    cd_agru TYPE zmmt0083-cd_agru,
    matnr   TYPE mseg-matnr,
    werks   TYPE mseg-werks,
    gsber   TYPE bsis-gsber,
    bukrs   TYPE mseg-bukrs,
    mblnr   TYPE mseg-mblnr,
    bwart   TYPE mseg-bwart,
    gjahr   TYPE mseg-gjahr,
    belnr   TYPE bsis-belnr,
    dmbtr   TYPE bsis-dmbtr,
    menge   TYPE mseg-menge,
    sgtxt   TYPE bsis-sgtxt,
  END OF ty_docs,

  BEGIN OF ty_bsis.
    INCLUDE TYPE bsis.
TYPES: mblnr    LIKE mseg-mblnr,
    zeile    TYPE  mblpo,
    matnr    TYPE  mseg-matnr,
    doc_lcto TYPE zglt036-doc_lcto.
TYPES: END OF ty_bsis,

BEGIN OF ty_mseg.
  INCLUDE TYPE mseg.
TYPES: awkey TYPE bkpf-awkey.
TYPES: END OF ty_mseg,

*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Início de Alteração
BEGIN OF ty_bsis_est,
  bukrs TYPE bsis-bukrs,
  hkont TYPE bsis-hkont,
  gjahr TYPE bsis-gjahr,
  belnr TYPE bsis-belnr,
  ghkon TYPE bsis-ghkon,
  bschl TYPE bsis-bschl,
END OF ty_bsis_est.
*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Fim de Alteração



DATA:
  vg_bdatj TYPE ckmlpp-bdatj,
  vg_poper TYPE ckmlpp-poper.
*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:

  it_ckmlhd        TYPE TABLE OF ckmlhd,
  t_ckmlhd         TYPE TABLE OF ckmlhd,
  it_ckmlpp        TYPE TABLE OF ckmlpp,
  it_ckmlpp_aux    TYPE TABLE OF ckmlpp,
  t_ckmlpp         TYPE TABLE OF ckmlpp,
  it_ckmlcr        TYPE TABLE OF ckmlcr,
  it_ckmlcr_aux    TYPE TABLE OF ckmlcr,
  t_ckmlcr         TYPE TABLE OF ckmlcr,
  it_mlcd          TYPE TABLE OF mlcd,
  t_mlcd           TYPE TABLE OF mlcd,
  it_makt          TYPE TABLE OF makt,
  t_makt           TYPE TABLE OF makt,
  it_mara          TYPE TABLE OF ty_mara,

  it_zmmt0083      TYPE TABLE OF zmmt0083,
  it_zmmt0084      TYPE TABLE OF zmmt0084,
  it_zmmt0148      TYPE TABLE OF zmmt0148,

  it_mbew          TYPE TABLE OF mbew,
  it_t030          TYPE TABLE OF t030,
  it_t030_aux      TYPE TABLE OF t030,
  it_t156          TYPE TABLE OF t156,
  it_bsis          TYPE TABLE OF ty_bsis,
  it_bsis2         TYPE TABLE OF ty_bsis,
  t_bsis           TYPE TABLE OF ty_bsis,
  it_bseg          TYPE TABLE OF bseg,
  it_bkpf          TYPE TABLE OF bkpf,
  it_zglt036       TYPE TABLE OF zglt036,
  it_mseg          TYPE TABLE OF ty_mseg,
  it_matnr         TYPE TABLE OF ty_matnr,
  it_docs          TYPE TABLE OF ty_docs,
  it_docs_alv      TYPE TABLE OF ty_docs,
  it_color         TYPE TABLE OF lvc_s_scol,
*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Início de Alteração
  it_bsis_est_lanc TYPE TABLE OF ty_bsis_est.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

DATA:
      r_bschl_est TYPE RANGE OF bsis-bschl.
*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Fim de Alteração


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: ok-code       TYPE sy-ucomm,
      wa_ckmlhd     TYPE ckmlhd,
      wa_ckmlpp     TYPE ckmlpp,
      wa_ckmlpp_aux TYPE ckmlpp,
      wa_ckmlcr     TYPE ckmlcr,
      wa_ckmlcr_aux TYPE ckmlcr,
      wa_mlcd       TYPE mlcd,
      wa_makt       TYPE makt,
      wa_mbew       TYPE mbew, "pbi - 67583 - csb
      wa_mara       TYPE ty_mara,
      wa_bsis       TYPE ty_bsis,
      wa_bseg       TYPE bseg,
      wa_bkpf       TYPE bkpf,
      wa_zglt036    TYPE zglt036,
      wa_mseg       TYPE ty_mseg,
      wa_mseg2      TYPE ty_mseg,
      wa_matnr      TYPE ty_matnr,
      wa_docs       TYPE ty_docs,
      wa_j_1bbranch TYPE j_1bbranch,
      wa_zmmt0083   TYPE zmmt0083,
      wa_zmmt0084   TYPE zmmt0084,
      wa_zmmt0148   TYPE zmmt0148,
      wa_t030       TYPE t030,
      wa_t030_aux   TYPE t030,
      wa_t156       TYPE t156,
      wa_color      TYPE lvc_s_scol.


CONSTANTS c_x               TYPE c VALUE 'X'.

DATA: event       TYPE cntl_simple_event,
      events      TYPE cntl_simple_events,
      tl_filter   TYPE lvc_t_filt,
      wl_filter   TYPE lvc_s_filt,
      tl_function TYPE ui_functions,
      wl_function LIKE tl_function  WITH HEADER LINE.

*----------------------------------------------------------------------*
* Field-symbols
*----------------------------------------------------------------------*
* <fs_data> p/ser a tabela dinâmica onde constaram os dados de exibição
FIELD-SYMBOLS: <fs_data>      TYPE ANY TABLE,
               <fs_data2>     TYPE ANY TABLE,
               <fs_data3>     TYPE ANY TABLE,
               <t_cellcolors> TYPE lvc_t_scol,
               <it_color>     TYPE lvc_t_scol,

*work-área p/ trabalhar os dados antes de inclui <fs_data>
               <wa_data>      TYPE any,
               <wa_data2>     TYPE any,
               <wa_data3>     TYPE any,

*campo que recebera dados e apontara p/ os campos dinâmicos da wa.
               <fs_campo>     TYPE any,
               <fs_campo2>    TYPE any,
               <fs_campo3>    TYPE any.


DATA: wa_fcat_lvc   TYPE lvc_s_fcat,
      lt_fcat_lvc   TYPE lvc_t_fcat,
      lt_fcat_lvc2  TYPE lvc_t_fcat,
      t_data        TYPE REF TO data,
      t_data2       TYPE REF TO data,
      t_data3       TYPE REF TO data,
      wa_cellcolors TYPE LINE OF lvc_t_scol.


DATA: wa_layout TYPE lvc_s_layo,
      wa_stable TYPE lvc_s_stbl.

DATA: g_container        TYPE scrfname VALUE 'CC_REL',
      g_container2       TYPE scrfname VALUE 'CC_DOCS',
      "
      cl_container_95    TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id      TYPE REF TO cl_dd_document,
      editcontainer      TYPE REF TO cl_gui_custom_container,
      cl_container       TYPE REF TO cl_gui_custom_container,

      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_custom_conta0200 TYPE REF TO cl_gui_custom_container,
      container_1        TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2        TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter           TYPE REF TO cl_gui_splitter_container,
      grid1              TYPE REF TO cl_gui_alv_grid,
      grid2              TYPE REF TO cl_gui_alv_grid,
      gs_variant_c       TYPE disvariant,
      gs_variant_2       TYPE disvariant.

DATA: v_camp(10),       " variável p/ montar campo dinâmico
      v_camp2(10),      " variável p/ montar campo dinâmico
      v_camp3(10),      " variável p/ montar campo dinâmico
      v_text(100).      " variável p/ montar texto dinâmico

DATA: vg_cagrup(1) TYPE c.
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
* PBI - 67583 - Inicio - CSB
*    CLASS-METHODS:
*      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
*        IMPORTING e_row e_column.
* PBI - 67583 - Fim - CSB

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,

      catch_hotspot_grid01 FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* PBI - 67583 - Inicio - CSB
*  METHOD on_double_click.
*    DATA: vwerks   TYPE mseg-werks,
*          vgsber   TYPE bsis-gsber,
*          vmatnr   TYPE mseg-matnr,
*          vcd_agru TYPE zmmt0083-cd_agru,
*          vval_ctb TYPE bsis-dmbtr.
*
*    IF e_row-index GT 0.
*      LOOP AT <fs_data> INTO <wa_data>.
*        IF sy-tabix =   e_row-index.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*      "
*      ASSIGN COMPONENT 'WERKS'  OF STRUCTURE <wa_data> TO <fs_campo>.
*      MOVE <fs_campo> TO vwerks.
*      ASSIGN COMPONENT 'GSBER'  OF STRUCTURE <wa_data> TO <fs_campo>.
*      MOVE <fs_campo> TO vgsber.
*      "
*      ASSIGN COMPONENT 'MATNR'  OF STRUCTURE <wa_data> TO <fs_campo>.
*      MOVE <fs_campo> TO vmatnr.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = vmatnr
*        IMPORTING
*          output = vmatnr.
*      "
*      IF  e_column+1(3) = 'VAL' OR e_column = 'VAL_CTB' OR e_column = 'VAL_MAT' OR e_column = 'VLR_LEDGER'.
*        "Colunas valor zero não faz nada
*        ASSIGN COMPONENT  e_column  OF STRUCTURE <wa_data> TO <fs_campo>.
*        MOVE <fs_campo> TO vval_ctb.
*        IF vval_ctb = 0.
*          EXIT.
*        ENDIF.
*        REFRESH it_docs_alv .
*        IF e_column = 'VAL_CTB'.
*          vcd_agru = 9999.
*        ELSEIF e_column = 'VLR_LEDGER'.
*          vcd_agru = 8888.
*        ELSEIF e_column = 'VAL_MAT'.
*          vcd_agru = 0.
*        ELSE.
*          vcd_agru = e_column+4(4).
*        ENDIF.
*        LOOP AT it_docs INTO wa_docs WHERE cd_agru = vcd_agru
*                                     AND   werks   = vwerks
*                                     AND   gsber   = vgsber
*                                     AND   matnr   = vmatnr.
*          APPEND wa_docs TO it_docs_alv.
*        ENDLOOP.
*        CALL SCREEN 0200 STARTING AT 050 3
*                         ENDING   AT 172 38.
*      ENDIF.
*    ENDIF.
*  ENDMETHOD.                    "ON_DOUBLE_CLICK
* PBI - 67583 - Fim - CSB

  METHOD catch_hotspot.

    IF e_row_id GT 0.
      READ TABLE it_docs_alv INTO wa_docs INDEX e_row_id-index.
      IF  e_column_id = 'BELNR' AND wa_docs-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD wa_docs-belnr.
        SET PARAMETER ID 'BUK' FIELD wa_docs-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_docs-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF e_column_id = 'MBLNR' AND wa_docs-mblnr IS NOT INITIAL.
* ---> S4 Migration - 19/07/2023 - LO
*        SET PARAMETER ID 'MBN' FIELD wa_docs-mblnr.
*        SET PARAMETER ID 'MJA' FIELD wa_docs-gjahr.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ''
            i_skip_first_screen = 'X'
            i_deadend           = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_docs-mblnr
            i_mjahr             = wa_docs-gjahr
          EXCEPTIONS
            illegal_combination = 1
            OTHERS              = 2.
* <--- S4 Migration - 19/07/2023 - LO
      ENDIF.
    ENDIF.
  ENDMETHOD.
*** PBI - 67583 Inicio - CSB
  METHOD catch_hotspot_grid01.
    DATA: vwerks   TYPE mseg-werks,
          vgsber   TYPE bsis-gsber,
*          vmatnr   TYPE mseg-matnr,
          vmatnr   TYPE matnr18,
          vcd_agru TYPE zmmt0083-cd_agru,
          vval_ctb TYPE bsis-dmbtr.

    IF e_row_id GT 0.
      LOOP AT <fs_data> INTO <wa_data>.
        IF sy-tabix =   e_row_id-index.
          EXIT.
        ENDIF.
      ENDLOOP.
      "
      ASSIGN COMPONENT 'WERKS'  OF STRUCTURE <wa_data> TO <fs_campo>.
      MOVE <fs_campo> TO vwerks.
      ASSIGN COMPONENT 'GSBER'  OF STRUCTURE <wa_data> TO <fs_campo>.
      MOVE <fs_campo> TO vgsber.
      "
      ASSIGN COMPONENT 'MATNR'  OF STRUCTURE <wa_data> TO <fs_campo>.
      MOVE <fs_campo> TO vmatnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vmatnr
        IMPORTING
          output = vmatnr.
      "
      IF  e_column_id+1(3) = 'VAL' OR e_column_id = 'VAL_CTB' OR e_column_id = 'VAL_MAT' OR e_column_id = 'VLR_LEDGER'.
        "Colunas valor zero não faz nada
        ASSIGN COMPONENT  e_column_id  OF STRUCTURE <wa_data> TO <fs_campo>.
        MOVE <fs_campo> TO vval_ctb.
        IF vval_ctb = 0.
          EXIT.
        ENDIF.
        REFRESH it_docs_alv .
        IF e_column_id = 'VAL_CTB'.
          vcd_agru = 9999.
        ELSEIF e_column_id = 'VLR_LEDGER'.
          vcd_agru = 8888.
        ELSEIF e_column_id = 'VAL_MAT'.
          vcd_agru = 0.
        ELSE.
          vcd_agru = e_column_id+4(4).
        ENDIF.
        LOOP AT it_docs INTO wa_docs WHERE cd_agru = vcd_agru
                                     AND   werks   = vwerks
                                     AND   gsber   = vgsber
                                     AND   matnr   = vmatnr.
          APPEND wa_docs TO it_docs_alv.
        ENDLOOP.
        CALL SCREEN 0200 STARTING AT 050 3
                         ENDING   AT 172 38.
      ENDIF.
    ENDIF.

  ENDMETHOD.
* PBI - 67583 - Fim - CSB
ENDCLASS.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
"DATA   DYFIELDS LIKE DYNPREAD OCCURS 1 WITH HEADER LINE.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR j_1bbranch-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY  ,
                  p_werks FOR mlkey-werks_ml_productive OBLIGATORY.

  PARAMETERS:     p_bdatj TYPE ckmlpp-bdatj OBLIGATORY.
  SELECT-OPTIONS: p_budat FOR wa_bsis-budat.


  SELECT-OPTIONS: p_poper FOR ckmlpp-poper OBLIGATORY,
                  p_curtp FOR mlkey-curtp  NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT 10,
                  s_matnr FOR ckmlhd-matnr OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: r_val LIKE bsid-umskz AS CHECKBOX  DEFAULT 'X',
              r_qtd LIKE bsid-umskz AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b2.
*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  "
  PERFORM: f_seleciona_dados,
           f_saida,
           f_alv.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .
  DATA: vg_last_day_aux(8),
        vg_last_day         TYPE sy-datum,
        vg_first_day_aux(8),
        vg_first_day        TYPE sy-datum,
        wl_tka02            TYPE tka02,
        tabix               TYPE sy-tabix.

  IF p_poper-low = '001'.
    vg_bdatj = p_bdatj - 1.
    vg_poper = '012'.
  ELSE.
    vg_bdatj = p_bdatj .
    vg_poper = p_poper-low - 1.
  ENDIF.

  CONCATENATE p_bdatj p_poper-low+1(2) '01' INTO vg_first_day_aux.
  vg_first_day = vg_first_day_aux.

  IF  p_poper-high IS NOT INITIAL.
    CONCATENATE p_bdatj p_poper-high+1(2) '01' INTO vg_last_day_aux.
  ELSE.
    CONCATENATE p_bdatj p_poper-low+1(2) '01' INTO vg_last_day_aux.
  ENDIF.
  vg_last_day = vg_last_day_aux.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = vg_last_day
    IMPORTING
      e_date = vg_last_day.

  SELECT *
    FROM zmmt0084
    INTO TABLE it_zmmt0084
    WHERE  code    = 'ZMM0124'. "SY-TCODE.

  CHECK it_zmmt0084 IS NOT INITIAL.

  SELECT *
  FROM zmmt0083
  INTO TABLE it_zmmt0083
  FOR ALL ENTRIES IN it_zmmt0084
  WHERE cd_agru = it_zmmt0084-cd_agru
  AND   code    = it_zmmt0084-code.

  CHECK it_zmmt0083 IS NOT INITIAL.

* PBI - 67583 - Inicio - CSB
  CLEAR: vg_cagrup.
  SELECT *
    FROM zmmt0148
    INTO TABLE it_zmmt0148
   WHERE bukrs IN s_bukrs
     AND centro_virt IN p_werks.

  IF it_zmmt0148 IS NOT INITIAL.
    vg_cagrup = 'X'.
  ENDIF.

* PBI - 67583 - Fim - CSB

  "contas associadas a classe de avaliação para validar partida
  SELECT *
      FROM mbew
      INTO TABLE it_mbew
     WHERE matnr IN s_matnr
       AND bwkey IN p_werks.

  SELECT matnr meins
      FROM mara
      INTO TABLE it_mara
     WHERE matnr IN s_matnr.

  SELECT *
    FROM t030
    INTO TABLE it_t030
     FOR ALL ENTRIES IN it_mbew
   WHERE ktopl EQ '0050'
     AND bklas EQ it_mbew-bklas
     AND ktosl EQ 'BSX'.
*
  SELECT *
    FROM t156
    INTO TABLE it_t156
    FOR ALL ENTRIES IN it_zmmt0083
    WHERE  bwart EQ it_zmmt0083-bwart.

*  "seleciona os documentos contábeis pela OBYC BSX
  IF  p_budat IS NOT INITIAL.
    SELECT *
        FROM bsis
        INTO CORRESPONDING FIELDS OF TABLE it_bsis
         FOR ALL ENTRIES IN it_t030
        WHERE bukrs IN s_bukrs
          AND hkont EQ it_t030-konts
          AND gjahr EQ p_bdatj
          AND monat IN p_poper
          AND werks IN p_werks
          AND budat IN p_budat.
    SELECT *
       FROM bsis
       APPENDING CORRESPONDING FIELDS OF TABLE it_bsis
       WHERE bukrs IN s_bukrs
         AND hkont IN ( '0000341000','0000341018', '0000341032','0000341071', '0000341068', '0000341069', '0000341091', '0000341119'  )
         AND blart IN ('LM', 'ML', 'AB' )
         AND gjahr EQ p_bdatj
         AND monat IN p_poper
         AND werks IN p_werks
         AND budat IN p_budat.
  ELSE.
    SELECT *
     FROM bsis
     INTO CORRESPONDING FIELDS OF TABLE it_bsis
      FOR ALL ENTRIES IN it_t030
     WHERE bukrs IN s_bukrs
       AND hkont EQ it_t030-konts
       AND gjahr EQ p_bdatj
       AND monat IN p_poper
       AND werks IN p_werks.
    SELECT *
    FROM bsis
    APPENDING CORRESPONDING FIELDS OF TABLE it_bsis
    WHERE bukrs IN s_bukrs
      AND hkont IN ( '0000341000','0000341018', '0000341032','0000341071', '0000341068', '0000341069', '0000341091', '0000341119'  )
      AND blart IN ('LM', 'ML', 'AB' )
      AND gjahr EQ p_bdatj
      AND monat IN p_poper
      AND werks IN p_werks.
  ENDIF.

  DELETE it_bsis WHERE  hkont = '0000114019' AND blart = 'LM'.
  DELETE it_bsis WHERE  hkont = '0000114026' AND blart = 'LM'.

*CHECK IT_BSIS[] IS NOT INITIAL.
  IF it_bsis[] IS NOT INITIAL.
    SORT it_bsis BY bukrs belnr gjahr.

    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = it_bsis
        i_where_clause     = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND BUZEI EQ IT_FOR_ALL_ENTRIES-BUZEI|
      IMPORTING
        et_bseg            = it_bseg
      EXCEPTIONS
        not_found          = 1.
    IF sy-subrc <> 0 OR lines( it_bseg ) = 0.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ELSE.
      sy-dbcnt = lines( it_bseg ).
    ENDIF.

    SORT   it_bseg     BY bukrs belnr gjahr buzei.

    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf
       FOR ALL ENTRIES IN it_bsis
      WHERE bukrs EQ it_bsis-bukrs
        AND belnr EQ it_bsis-belnr
        AND gjahr EQ it_bsis-gjahr.

    SORT it_bkpf BY bukrs belnr gjahr.
    SORT it_bsis BY bukrs matnr werks. "CSB
    LOOP AT it_bsis INTO wa_bsis.
      tabix = sy-tabix.
      READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_bsis-bukrs
                                               belnr = wa_bsis-belnr
                                               gjahr = wa_bsis-gjahr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF wa_bkpf-awkey(5) NE 'ZGL17'.
          wa_bsis-mblnr = wa_bkpf-awkey(10).
          MOVE wa_bsis-buzei TO wa_bsis-zeile.
        ELSE.
          CLEAR wa_bsis-mblnr.
          wa_bsis-doc_lcto = wa_bkpf-awkey+5(10).
          wa_bsis-zeile = 0.
        ENDIF.

        MODIFY it_bsis FROM wa_bsis INDEX tabix TRANSPORTING mblnr matnr zeile doc_lcto.

      ENDIF.
      CLEAR: wa_bsis.
    ENDLOOP.

    it_bsis2[] = it_bsis[].
    DELETE it_bsis2[] WHERE doc_lcto IS INITIAL.

    SELECT *
      FROM zglt036
      INTO TABLE it_zglt036
      FOR ALL ENTRIES IN it_bsis2
      WHERE doc_lcto = it_bsis2-doc_lcto
      AND hkont IN ( '0000341000','0000341018', '0000341032','0000341071', '0000341068', '0000341069', '0000341091', '0000341119'  )
      AND matnr IN s_matnr
      AND werks IN p_werks.

    SORT it_zglt036 BY doc_lcto werks.

    SELECT *
      FROM mseg
      INTO TABLE it_mseg
       FOR ALL ENTRIES IN it_bsis
     WHERE mblnr EQ it_bsis-mblnr
       AND mjahr EQ it_bsis-gjahr
       AND matnr IN s_matnr
       AND werks IN p_werks.

    SORT  it_mseg BY mblnr gjahr werks xauto matnr.
    DATA vxauto TYPE mseg-xauto.

    LOOP AT it_bsis INTO wa_bsis.
      tabix = sy-tabix.
      READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_bsis-bukrs
                                               belnr = wa_bsis-belnr
                                               gjahr = wa_bsis-gjahr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = wa_bsis-bukrs
                                                 belnr = wa_bsis-belnr
                                                 gjahr = wa_bsis-gjahr
                                                 buzei = wa_bsis-buzei BINARY SEARCH.
        IF sy-subrc = 0 AND wa_bseg-matnr IS NOT INITIAL.
          wa_bsis-matnr = wa_bseg-matnr.
        ELSEIF wa_bsis-doc_lcto IS NOT INITIAL.
          READ TABLE it_zglt036 INTO wa_zglt036 WITH KEY doc_lcto = wa_bsis-doc_lcto
                                                         werks    = wa_bsis-werks.
          IF sy-subrc = 0.
            wa_bsis-matnr = wa_zglt036-matnr.
          ENDIF.
        ENDIF.
        IF wa_bsis-shkzg = 'S'.
          vxauto = 'X'.
          READ TABLE it_mseg INTO wa_mseg WITH KEY mblnr = wa_bsis-mblnr
                                                   mjahr = wa_bsis-gjahr
                                                   werks = wa_bsis-werks
                                                   xauto = vxauto BINARY SEARCH.
          IF sy-subrc = 0.
            IF wa_mseg-bwart = '309'. "troca material
              READ TABLE it_mseg INTO wa_mseg2 WITH KEY mblnr = wa_bsis-mblnr
                                                  mjahr = wa_bsis-gjahr
                                                  werks = wa_bsis-werks
                                                  xauto = vxauto
                                                  matnr = wa_bsis-matnr BINARY SEARCH.
              wa_bsis-matnr = wa_mseg2-matnr.
            ENDIF.
          ENDIF.
        ENDIF.

        MODIFY it_bsis FROM wa_bsis INDEX tabix TRANSPORTING matnr.
      ENDIF.
      CLEAR: wa_bsis.
    ENDLOOP.

    DELETE it_bsis WHERE matnr NOT IN s_matnr.

    SELECT *
      FROM ckmlhd
      INTO TABLE it_ckmlhd
      WHERE matnr IN s_matnr
      AND   bwkey IN p_werks.

    IF it_ckmlhd[] IS NOT INITIAL.

      DATA: lv_jahrper2 TYPE mldoc-jahrper,
            lw_mlcd     TYPE mlcd.

      CONCATENATE p_bdatj p_poper INTO lv_jahrper2.

      SELECT * FROM  mldoc
               INTO TABLE @DATA(gt_mldoc)
               FOR ALL ENTRIES IN @it_ckmlhd
               WHERE kalnr  EQ @it_ckmlhd-kalnr  AND
               jahrper      EQ @lv_jahrper2      AND
               curtp        EQ @p_curtp-low      AND
               categ        IN ( 'VN' )          AND
               ptyp         EQ 'V+'.


      LOOP AT gt_mldoc INTO DATA(w_mldoc).
        MOVE-CORRESPONDING w_mldoc TO lw_mlcd.
        lw_mlcd-poper = w_mldoc-jahrper+4(3).
        lw_mlcd-bdatj = w_mldoc-jahrper(4).
        lw_mlcd-lbkum = w_mldoc-quant.
*        lw_mlcd-docref = w_mldoc-docref.
        APPEND lw_mlcd TO it_mlcd.
        CLEAR lw_mlcd.
      ENDLOOP.

      SORT gt_mldoc BY bvalt.

      " Manter select em tabela obsoleta com objetivo de buscar registros antigos que não estão na nova tabela MLDOC.
      SELECT *                         "#EC CI_DB_OPERATION_OK[2354768]
         FROM mlcd
         INTO TABLE @DATA(gt_mlcd_2)
          FOR ALL ENTRIES IN @it_ckmlhd
         WHERE kalnr EQ @it_ckmlhd-kalnr
         AND   bdatj  EQ @p_bdatj
         AND   poper IN @p_poper
         AND   curtp EQ @p_curtp-low
         AND   categ IN ( 'VN' )
         AND   ptyp  EQ 'V+'.

      LOOP AT gt_mlcd_2 ASSIGNING FIELD-SYMBOL(<fs_mlcd>).
        READ TABLE gt_mldoc WITH KEY bvalt = <fs_mlcd>-bvalt BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          APPEND <fs_mlcd> TO it_mlcd.
        ENDIF.
      ENDLOOP.

      SORT it_mlcd BY kalnr bdatj poper untper categ ptyp bvalt curtp .
      DELETE ADJACENT DUPLICATES FROM it_mlcd COMPARING kalnr bdatj poper untper categ ptyp bvalt curtp.
      "*<--- 19/07/2023 - Migração S4 - LO

      SELECT *
        FROM makt
        INTO TABLE it_makt
        FOR ALL ENTRIES IN it_ckmlhd
        WHERE matnr EQ it_ckmlhd-matnr
        AND   spras EQ sy-langu.

      DATA: wa_kalnr  TYPE ckmv0_matobj_str,
            lt_kalnr  TYPE ckmv0_matobj_tbl,
            lt_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE,
            lt_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE.


      DATA: lv_bdatj_1 TYPE  ckmlpp-bdatj,
            lv_poper_1 TYPE  ckmlpp-poper,
            lv_jahrper TYPE mldoc-jahrper.

      lv_bdatj_1 = vg_bdatj.
      lv_poper_1 = vg_poper.

      LOOP AT it_ckmlhd INTO DATA(wa_ckmlhd).
        wa_kalnr-kalnr = wa_ckmlhd-kalnr.
        APPEND wa_kalnr TO lt_kalnr.
      ENDLOOP.

      CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
        EXPORTING
          i_bdatj_1               = lv_bdatj_1
          i_poper_1               = lv_poper_1
        TABLES
          t_kalnr                 = lt_kalnr
          t_ckmlpp                = lt_ckmlpp
          t_ckmlcr                = lt_ckmlcr "ALRS
        EXCEPTIONS
          no_data_found           = 1
          input_data_inconsistent = 2
          buffer_inconsistent     = 3
          OTHERS                  = 4.


      IF  lines( lt_ckmlpp[] ) > 0.

        MOVE-CORRESPONDING lt_ckmlpp[] TO it_ckmlpp[].
        sy-dbcnt = lines( lt_ckmlpp[] ).
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.

      DELETE lt_ckmlcr WHERE curtp NE p_curtp-low.

      IF lines( lt_ckmlcr[] ) > 0.

        MOVE-CORRESPONDING lt_ckmlcr[] TO it_ckmlcr[].
        sy-dbcnt = lines( lt_ckmlcr[] ).
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.

* <--- S4 Migration - 09/07/2023 - MA

    ELSE.
      MESSAGE 'Material sem ledger' TYPE 'E'.
    ENDIF.

  ENDIF.

*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Início de Alteração
  PERFORM: f_buscar_bschl_est,
           f_buscar_estorno_lanc.
*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Fim de Alteração

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
FORM f_saida.
  "
  DATA: vmatnr          TYPE mara-matnr,
        vsalk3          TYPE ckmlcr-salk3,
        vgsber          TYPE bsis-gsber,
        vvprsv          TYPE mbew-vprsv,
        vgtext          TYPE tgsbt-gtext,
        vtipo(10),
        xachou(1),
*** PBI - 67583 - CSB - Inicio
        vgkonts         TYPE  t030-konts,
        vgcentro_virt   TYPE  zmmt0148-centro_virt,
        vgcentro_ref    TYPE  zmmt0148-centro_ref,
        vgorigem        TYPE  zmmt0148-origem,
        vgagrup01       TYPE  zmmt0148-agrup01,
        vgagrup02       TYPE  zmmt0148-agrup02,
        vgagrup03       TYPE  zmmt0148-agrup03,
        vgagrup04       TYPE  zmmt0148-agrup04,
        vguf            TYPE  zmmt0148-uf,
        vgcap_instalada TYPE  zmmt0148-cap_instalada.
*** PBI - 67583 - CSB - Fim


  SORT: it_ckmlhd   BY matnr bwkey,
        it_ckmlpp   BY kalnr bdatj poper,
        it_ckmlcr   BY kalnr bdatj poper,
        it_mlcd     BY kalnr bdatj poper,
        it_makt     BY matnr,
        it_mara     BY matnr,
        it_mseg     BY mblnr gjahr matnr werks zeile,
        it_bkpf     BY bukrs belnr gjahr,
        it_bsis     BY bukrs belnr gjahr,
        it_bseg     BY bukrs belnr gjahr buzei,
        it_zmmt0083 BY bwart,
        it_t030     BY konts,
        it_t156     BY bwart.

** Bug - 70537 - Inicio - CSB
  it_t030_aux[] = it_t030[].
  SORT: it_t030_aux BY bklas.
** Bug - 70537 - Fim - CSB

  "Cria layout e tabela dinamica
  PERFORM f_monta_layout.
  "
  "colocar cor
  LOOP AT it_zmmt0084 INTO wa_zmmt0084.
    CLEAR: v_camp.
    CONCATENATE  wa_zmmt0084-entsai 'VAL'
                 wa_zmmt0084-cd_agru
                 INTO v_camp.
    MOVE  v_camp     TO wa_color-fname.
    IF wa_zmmt0084-entsai = 'E'.
      MOVE '5'         TO wa_color-color-col.
    ELSE.
      MOVE '6'         TO wa_color-color-col.
    ENDIF.
    MOVE '1'         TO wa_color-color-int.
    MOVE '1'         TO wa_color-color-inv.
    APPEND wa_color TO it_color.


    CLEAR: v_camp.
    CONCATENATE  wa_zmmt0084-entsai 'QTD'
               wa_zmmt0084-cd_agru
               INTO v_camp.
    MOVE  v_camp     TO wa_color-fname.
    IF wa_zmmt0084-entsai = 'E'.
      MOVE '5'         TO wa_color-color-col.
    ELSE.
      MOVE '6'         TO wa_color-color-col.
    ENDIF.
    MOVE '1'         TO wa_color-color-int.
    MOVE '1'         TO wa_color-color-inv.
    APPEND wa_color TO it_color.
  ENDLOOP.

  SORT it_zmmt0084 BY cd_agru.
  " Popula tabela dinamica
  SORT it_bsis  BY bukrs matnr werks. "CSB
  SORT t_ckmlpp BY kalnr.
*---> 05/07/2023 - Migração S4 - DL
  SORT it_mbew BY matnr bwkey.

  SORT t_ckmlhd BY matnr bwkey.

  it_bsis2[] = it_bsis[].
  SORT it_bsis2 BY mblnr matnr werks.

  LOOP AT it_bsis INTO wa_bsis.

*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Início de Alteração
    READ TABLE it_bsis_est_lanc TRANSPORTING NO FIELDS
                                WITH KEY bukrs = wa_bsis-bukrs
                                         hkont = wa_bsis-ghkon
                                         gjahr = wa_bsis-gjahr
                                         belnr = wa_bsis-belnr
                                         ghkon = wa_bsis-hkont
                                         BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      " Se o registro foi estornando, então desconsiderar
      CONTINUE.
    ENDIF.
*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Fim de Alteração

    "ALRS
    IF wa_bsis-matnr IS NOT INITIAL.
      READ TABLE it_mbew INTO wa_mbew WITH KEY matnr = wa_bsis-matnr  bwkey = wa_bsis-werks BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    CLEAR xachou.

    "Partida de estoque
    READ TABLE it_t030 INTO wa_t030 WITH KEY konts = wa_bsis-hkont BINARY SEARCH.
    IF sy-subrc NE 0 AND NOT ( '0000341000_0000341018_0000341032_0000341071_0000341068_0000341069_0000341091_0000341119' CS wa_bsis-hkont ).
      CONTINUE.
    ENDIF.

    READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_bsis-bukrs
                                             belnr = wa_bsis-belnr
                                             gjahr = wa_bsis-gjahr BINARY SEARCH.
    "
    CLEAR: wa_mseg,wa_mseg2.
    LOOP AT it_mseg INTO wa_mseg WHERE mblnr = wa_bsis-mblnr
                                 AND   mjahr = wa_bsis-gjahr
                                 AND   matnr = wa_bsis-matnr
                                 AND   werks = wa_bsis-werks.

      ADD wa_mseg-menge TO wa_mseg2-menge.
    ENDLOOP.
    wa_mseg-menge = wa_mseg2-menge.

    IF sy-subrc IS INITIAL. "SE existe documento de material
      READ TABLE it_ckmlhd INTO wa_ckmlhd WITH KEY matnr = wa_mseg-matnr
                                                   bwkey = wa_mseg-werks BINARY SEARCH.
      IF sy-subrc NE 0. "so processa com ledger aqui
        CONTINUE.
      ENDIF.
      "
      CLEAR vgsber.
      SELECT SINGLE gsber
        FROM t134g
        INTO vgsber
        WHERE werks = wa_mseg-werks.
      "
      READ TABLE it_makt   INTO wa_makt   WITH KEY matnr = wa_mseg-matnr BINARY SEARCH.
      READ TABLE it_ckmlpp INTO wa_ckmlpp WITH KEY kalnr = wa_ckmlhd-kalnr BINARY SEARCH.
      CLEAR vsalk3.
      IF sy-subrc = 0. "PBI - 67583 - csb
        LOOP AT it_ckmlcr INTO wa_ckmlcr WHERE kalnr = wa_ckmlhd-kalnr.
          ADD wa_ckmlcr-salk3     TO vsalk3.
        ENDLOOP.
*** PBI - 67583 - Inicio - CSB - comentei
      ELSE.

      ENDIF.
*** PBI - 67583 - Fim - CSB
      "
      "se ja gravou o saldo inicial para MATNR/WERKS não grava mais  ( COLLECT )
      READ TABLE it_matnr INTO wa_matnr WITH KEY matnr = wa_mseg-matnr
                                                 werks = wa_mseg-werks.
      IF sy-subrc = 0. " OR VGSBER NE WA_MSEG-WERKS.
        CLEAR:  vsalk3  , wa_ckmlpp.
      ELSE.
        wa_matnr-matnr = wa_mseg-matnr.
        wa_matnr-werks = wa_mseg-werks.
        APPEND wa_matnr TO it_matnr.
        "
        v_camp2 = 'SLD_FIMQ'.
        PERFORM f_carrega_dados USING wa_ckmlpp-lbkum     v_camp2.
        v_camp2 = 'SLD_FIMV'.
        PERFORM f_carrega_dados USING vsalk3              v_camp2.
      ENDIF.
      "Campos fixos
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_mseg-matnr
        IMPORTING
          output = vmatnr.

      CLEAR: vgtext, vvprsv.
      READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mseg-matnr BINARY SEARCH.
      SELECT SINGLE vprsv INTO vvprsv FROM mbew WHERE matnr = wa_mseg-matnr
                                                AND   bwkey = wa_mseg-werks.
      SELECT SINGLE gtext INTO vgtext FROM tgsbt WHERE spras = sy-langu
                                                 AND   gsber = vgsber.

*** PBI - 67583 - Inicio - CSB
      CLEAR: vgkonts, wa_mbew ,wa_t030_aux .
      READ TABLE it_mbew INTO wa_mbew WITH KEY matnr = wa_bsis-matnr  bwkey = wa_bsis-werks BINARY SEARCH.

      IF sy-subrc = 0.
        READ TABLE it_t030_aux INTO wa_t030_aux WITH KEY  bklas = wa_mbew-bklas BINARY SEARCH.
        vgkonts =  wa_t030_aux-konts.
      ENDIF.


      READ TABLE it_zmmt0148 INTO wa_zmmt0148 WITH KEY bukrs = wa_bsis-bukrs centro_virt = wa_bsis-werks.
      IF sy-subrc = 0.

        vgcentro_virt   = wa_zmmt0148-centro_virt.
        vgcentro_ref    = wa_zmmt0148-centro_ref.
        vgorigem        = wa_zmmt0148-origem.
        vgagrup01       = wa_zmmt0148-agrup01.
        vgagrup02       = wa_zmmt0148-agrup02.
        vgagrup03       = wa_zmmt0148-agrup03.
        vgagrup04       = wa_zmmt0148-agrup04.
        vguf            = wa_zmmt0148-uf.
        vgcap_instalada = wa_zmmt0148-cap_instalada.

        PERFORM f_carrega_dados USING:
                vgcentro_virt      'CENTRO_VIRT',
                vgcentro_ref       'CENTRO_REF',
                vgorigem           'ORIGEM',
                vgagrup01          'AGRUP01',
                vgagrup02          'AGRUP02',
                vgagrup03          'AGRUP03',
                vgagrup04          'AGRUP04',
                vguf               'UF',
                vgcap_instalada    'CAP_INSTALADA'.

      ENDIF.
*** PBI - 67583 - Fim - CSB

      PERFORM f_carrega_dados USING:
            vgsber             'GSBER',
            vgtext             'GTEXT',
            wa_mseg-werks      'WERKS',
            vgkonts            'KONTS',
            vmatnr             'MATNR',
            vvprsv             'VPRSV',
            wa_mara-meins      'MEINS',
            wa_makt-maktx      'MAKTX',
            wa_ckmlpp-lbkum    'SLD_INIQ',
            vsalk3             'SLD_INIV'.

      "Campos dinamicos
      READ TABLE it_zmmt0083 INTO wa_zmmt0083  WITH KEY bwart = wa_mseg-bwart BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_zmmt0084 INTO wa_zmmt0084  WITH KEY cd_agru = wa_zmmt0083-cd_agru BINARY SEARCH.

        READ TABLE it_t156 INTO wa_t156 WITH KEY bwart = wa_mseg-bwart BINARY SEARCH.

        "Pega a quantidade da BSEG
        READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = wa_bsis-bukrs
                                                 belnr = wa_bsis-belnr
                                                 gjahr = wa_bsis-gjahr
                                                 buzei = wa_bsis-buzei BINARY SEARCH.
        IF sy-subrc = 0.
          IF wa_bkpf-blart = 'ML'.
            LOOP AT it_bsis2 INTO DATA(wa_bsis2)
                           WHERE mblnr = wa_mseg-mblnr
                           AND   matnr = wa_mseg-matnr
                           AND   werks = wa_mseg-werks.
              IF wa_bsis2-blart NE 'ML'.
                xachou = 'X'.
                EXIT.
              ENDIF.
            ENDLOOP.
            IF  xachou IS NOT INITIAL.
              wa_mseg-menge = wa_bseg-menge.
            ENDIF.
            CLEAR xachou.

          ELSEIF wa_bseg-menge GT 0.
            wa_mseg-menge = wa_bseg-menge.
            IF wa_mseg-bwart = '309'.
              LOOP AT it_bsis2 INTO wa_bsis2
                             WHERE mblnr = wa_mseg-mblnr
                             AND   matnr = wa_mseg-matnr.
                IF wa_bsis2-blart EQ 'ML'.
                  xachou = 'X'.
                ENDIF.
              ENDLOOP.
*              IF wa_mseg-shkzg = 'S'. "Entrada
              IF wa_bsis-shkzg = 'S' AND xachou = 'X'. "Entrada
                MULTIPLY wa_mseg-menge BY -1.
                MULTIPLY wa_bsis-dmbtr BY -1.
                MULTIPLY wa_bsis-dmbe2 BY -1.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        "
        IF wa_bsis-shkzg = 'H' OR xachou = 'X'.
          MULTIPLY wa_bsis-dmbtr BY -1.
          MULTIPLY wa_bsis-dmbe2 BY -1.
          MULTIPLY wa_mseg-menge BY -1.
        ENDIF.
        "Valor
        IF r_val = 'X'.
          CLEAR: v_camp.
          CONCATENATE  wa_zmmt0084-entsai 'VAL'
                       wa_zmmt0084-cd_agru
                       INTO v_camp.

          IF wa_zmmt0084-entsai = 'E'.
            v_camp3 = 'SLD_FIMVE'.
          ELSE.
            v_camp3 = 'SLD_FIMVS'.
          ENDIF.

          v_camp2 = 'SLD_FIMV'.
          IF p_curtp-low = 10.
            PERFORM f_carrega_dados USING wa_bsis-dmbtr     v_camp.
            PERFORM f_carrega_dados USING wa_bsis-dmbtr     v_camp2.
            PERFORM f_carrega_dados USING wa_bsis-dmbtr     v_camp3.
          ELSE.
            PERFORM f_carrega_dados USING wa_bsis-dmbe2     v_camp.
            PERFORM f_carrega_dados USING wa_bsis-dmbe2     v_camp2.
            PERFORM f_carrega_dados USING wa_bsis-dmbe2     v_camp3.
          ENDIF.
        ENDIF.
        "
        "Quantidade
        IF r_qtd = 'X' AND  wa_bsis-belnr+0(2) NE '48'.
          IF wa_zmmt0084-entsai = 'E'.
            v_camp3 = 'SLD_FIMQE'.
          ELSE.
            v_camp3 = 'SLD_FIMQS'.
          ENDIF.
          v_camp2 = 'SLD_FIMQ'.

          CLEAR: v_camp.
          CONCATENATE  wa_zmmt0084-entsai 'QTD'
                    wa_zmmt0084-cd_agru
                    INTO v_camp.

          PERFORM f_carrega_dados USING wa_mseg-menge     v_camp.
          PERFORM f_carrega_dados USING wa_mseg-menge     v_camp2.
          PERFORM f_carrega_dados USING wa_mseg-menge     v_camp3.
        ENDIF.
        "Grava analitico
        CLEAR wa_docs.
        wa_docs-cd_agru = wa_zmmt0083-cd_agru.
        wa_docs-bukrs   = wa_mseg-bukrs.
        wa_docs-mblnr   = wa_mseg-mblnr.
        wa_docs-matnr   = wa_mseg-matnr.
        wa_docs-werks   = wa_mseg-werks.
        wa_docs-gsber   = vgsber.
        wa_docs-bwart   = wa_mseg-bwart.
        wa_docs-gjahr   = wa_mseg-mjahr.
        wa_docs-belnr   = wa_bsis-belnr.
        wa_docs-sgtxt   = wa_bsis-sgtxt.
        wa_docs-menge   = wa_mseg-menge.
        IF p_curtp-low = 10.
          wa_docs-dmbtr   = wa_bsis-dmbtr.
        ELSE.
          wa_docs-dmbtr   = wa_bsis-dmbe2.
        ENDIF.
        APPEND wa_docs TO it_docs.
      ELSE.
        IF wa_bsis-shkzg = 'H'.
          MULTIPLY wa_bsis-dmbtr BY -1.
          MULTIPLY wa_bsis-dmbe2 BY -1.
        ENDIF.
        v_camp = 'VAL_MAT'.
        IF p_curtp-low = 10.
          PERFORM f_carrega_dados USING wa_bsis-dmbtr     v_camp.
        ELSE.
          PERFORM f_carrega_dados USING wa_bsis-dmbe2     v_camp.
        ENDIF.
        "Grava analitico
        CLEAR wa_docs.
        wa_docs-cd_agru = 0. "doc de material sem agrupamento
        wa_docs-bukrs   = wa_mseg-bukrs.
        wa_docs-mblnr   = wa_mseg-mblnr.
        wa_docs-matnr   = wa_mseg-matnr.
        wa_docs-werks   = wa_mseg-werks.
        wa_docs-gsber   = vgsber.
        wa_docs-bwart   = wa_mseg-bwart.
        wa_docs-gjahr   = wa_mseg-mjahr.
        wa_docs-belnr   = wa_bsis-belnr.
        wa_docs-sgtxt   = wa_bsis-sgtxt.
        wa_docs-menge   = wa_mseg-menge.
        IF p_curtp-low = 10.
          wa_docs-dmbtr   = wa_bsis-dmbtr.
        ELSE.
          wa_docs-dmbtr   = wa_bsis-dmbe2.
        ENDIF.
        APPEND wa_docs TO it_docs.
      ENDIF.

    ELSE.
      IF wa_bsis-belnr IS NOT INITIAL.
        DATA:   lt_bseg TYPE fagl_t_bseg.

        CALL FUNCTION 'FAGL_GET_BSEG'
          EXPORTING
            i_bukrs   = wa_bsis-bukrs
            i_belnr   = wa_bsis-belnr
            i_gjahr   = wa_bsis-gjahr
          IMPORTING
            et_bseg   = lt_bseg
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.


        DELETE lt_bseg WHERE matnr NE wa_bsis-matnr AND buzei NE wa_bsis-buzei.

        READ TABLE lt_bseg INTO DATA(ls_bseg) INDEX 1.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_bseg TO wa_bseg.
        ENDIF.

        IF wa_bkpf-awkey(5) EQ 'ZGL17'.
          wa_bseg-matnr = wa_bsis-matnr.
        ENDIF.


        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        SELECT SINGLE *
            FROM makt
            INTO wa_makt
            WHERE matnr EQ wa_bseg-matnr
            AND   spras EQ sy-langu.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_bseg-matnr
          IMPORTING
            output = vmatnr.

        CLEAR: vgtext, vgsber.
        SELECT SINGLE gsber
             FROM t134g
             INTO vgsber
             WHERE werks = wa_bsis-werks.

        READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_bseg-matnr BINARY SEARCH.

        SELECT SINGLE vprsv INTO vvprsv FROM mbew WHERE matnr = wa_bseg-matnr
                                                  AND   bwkey = wa_bsis-werks.
        SELECT SINGLE gtext INTO vgtext FROM tgsbt WHERE spras = sy-langu
                                                   AND   gsber = vgsber.
        "inicio
        READ TABLE it_ckmlhd INTO wa_ckmlhd WITH KEY matnr = wa_bseg-matnr
                                                     bwkey = wa_bsis-werks BINARY SEARCH.
        IF sy-subrc NE 0. "so processa com ledger aqui
          CONTINUE.
        ENDIF.
        "
        READ TABLE it_ckmlpp INTO wa_ckmlpp WITH KEY kalnr = wa_ckmlhd-kalnr BINARY SEARCH.
        CLEAR vsalk3.
        IF sy-subrc = 0.
          LOOP AT it_ckmlcr INTO wa_ckmlcr WHERE kalnr = wa_ckmlhd-kalnr.
            ADD wa_ckmlcr-salk3     TO vsalk3.
          ENDLOOP.
*** pbi - 67583 - inicio - csb - comentei
        ELSE.

        ENDIF.
*** PBI - 67583 - Fim - CSB
        "
        "se ja gravou o saldo inicial para MATNR/WERKS não grava mais  ( COLLECT )
        READ TABLE it_matnr INTO wa_matnr WITH KEY matnr = wa_bseg-matnr
                                                   werks = wa_bsis-werks.
        IF sy-subrc = 0. " OR VGSBER NE WA_MSEG-WERKS.
          CLEAR:  vsalk3  , wa_ckmlpp.
        ELSE.
          wa_matnr-matnr = wa_bseg-matnr.
          wa_matnr-werks = wa_bsis-werks.
          APPEND wa_matnr TO it_matnr.
          "
          v_camp2 = 'SLD_FIMQ'.
          PERFORM f_carrega_dados USING wa_ckmlpp-lbkum     v_camp2.
          v_camp2 = 'SLD_FIMV'.
          PERFORM f_carrega_dados USING vsalk3              v_camp2.
        ENDIF.
        "fim

*** PBI - 67583 - Inicio - CSB
        CLEAR: vgkonts, wa_mbew ,wa_t030_aux,  wa_zmmt0148 .
        READ TABLE it_mbew INTO wa_mbew WITH KEY matnr = wa_bsis-matnr  bwkey = wa_bsis-werks BINARY SEARCH.

        IF sy-subrc = 0.
          READ TABLE it_t030_aux INTO wa_t030_aux WITH KEY  bklas = wa_mbew-bklas BINARY SEARCH.
          vgkonts =  wa_t030_aux-konts.
        ENDIF.

        READ TABLE it_zmmt0148 INTO wa_zmmt0148 WITH KEY bukrs = wa_bsis-bukrs centro_virt = wa_bsis-werks.
        IF sy-subrc = 0.

          vgcentro_virt   = wa_zmmt0148-centro_virt.
          vgcentro_ref    = wa_zmmt0148-centro_ref.
          vgorigem        = wa_zmmt0148-origem.
          vgagrup01       = wa_zmmt0148-agrup01.
          vgagrup02       = wa_zmmt0148-agrup02.
          vgagrup03       = wa_zmmt0148-agrup03.
          vgagrup04       = wa_zmmt0148-agrup04.
          vguf            = wa_zmmt0148-uf.
          vgcap_instalada = wa_zmmt0148-cap_instalada.

          PERFORM f_carrega_dados USING:
                  vgcentro_virt      'CENTRO_VIRT',
                  vgcentro_ref       'CENTRO_REF',
                  vgorigem           'ORIGEM',
                  vgagrup01          'AGRUP01',
                  vgagrup02          'AGRUP02',
                  vgagrup03          'AGRUP03',
                  vgagrup04          'AGRUP04',
                  vguf               'UF',
                  vgcap_instalada    'CAP_INSTALADA'.

        ENDIF.
**** PBI - 67583 - Fim - CSB

        PERFORM f_carrega_dados USING:
                vgsber             'GSBER',
                vgtext             'GTEXT',
                wa_bsis-werks      'WERKS',
                vgkonts            'KONTS',
                vmatnr             'MATNR',
                vvprsv             'VPRSV',
                wa_mara-meins      'MEINS',
                wa_makt-maktx      'MAKTX',
                wa_ckmlpp-lbkum    'SLD_INIQ',
                vsalk3             'SLD_INIV'.

        IF wa_bsis-shkzg = 'H'.
          MULTIPLY wa_bsis-dmbtr BY -1.
          MULTIPLY wa_bsis-dmbe2 BY -1.
        ENDIF.
        IF  '0000341000_0000341018_0000341032_0000341071_0000341068_0000341069_0000341091_0000341119' CS wa_bsis-hkont.
          v_camp = 'VLR_LEDGER'.
          CLEAR v_camp2.
          CLEAR: wa_docs.
          wa_docs-cd_agru = 8888. "doc ledger
        ELSE.
          v_camp = 'VAL_CTB'.
          v_camp2 = 'SLD_FIMV'.
          CLEAR: wa_docs.
          wa_docs-cd_agru = 9999. "doc de material sem agrupamento e sem doc contabil
        ENDIF.

        IF r_val = 'X'.
          IF p_curtp-low = 10.
            PERFORM f_carrega_dados USING wa_bsis-dmbtr     v_camp.
            PERFORM f_carrega_dados USING wa_bsis-dmbtr     v_camp2.
          ELSE.
            PERFORM f_carrega_dados USING wa_bsis-dmbe2     v_camp.
            PERFORM f_carrega_dados USING wa_bsis-dmbe2     v_camp2.
          ENDIF.
        ENDIF.
        "Grava analitico

        wa_docs-bukrs   = wa_bsis-bukrs.
        wa_docs-belnr   = wa_bsis-belnr.
        wa_docs-gjahr   = wa_bsis-gjahr.
        wa_docs-werks   = wa_bsis-werks.
        wa_docs-gsber   = vgsber.
        wa_docs-matnr   = wa_bseg-matnr.
        IF p_curtp-low = 10.
          wa_docs-dmbtr   = wa_bsis-dmbtr.
        ELSE.
          wa_docs-dmbtr   = wa_bsis-dmbe2.
        ENDIF.
        wa_docs-sgtxt   = wa_bsis-sgtxt.
        wa_docs-menge   = 0.
        APPEND wa_docs TO it_docs.

      ELSE. "========================= BUG 64199 - AOENNING 12/08/2021

        CLEAR: wa_makt.
        SELECT SINGLE *
            FROM makt
            INTO wa_makt
            WHERE matnr EQ wa_bsis-matnr
            AND   spras EQ sy-langu.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_bsis-matnr
          IMPORTING
            output = vmatnr.

        CLEAR: vgtext, vgsber.
        SELECT SINGLE gsber
             FROM t134g
             INTO vgsber
             WHERE werks = wa_bsis-werks.

        READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_bsis-matnr BINARY SEARCH.

        SELECT SINGLE vprsv INTO vvprsv FROM mbew WHERE matnr = wa_bsis-matnr
                                                  AND   bwkey = wa_bsis-werks.
        SELECT SINGLE gtext INTO vgtext FROM tgsbt WHERE spras = sy-langu
                                                   AND   gsber = vgsber.
        "inicio
        READ TABLE t_ckmlhd INTO wa_ckmlhd WITH KEY matnr = wa_bsis-matnr
                                                    bwkey = wa_bsis-werks BINARY SEARCH.
        IF sy-subrc NE 0. "so processa com ledger aqui
          CONTINUE.
        ENDIF.

        CLEAR: wa_ckmlpp.
        READ TABLE t_ckmlpp INTO wa_ckmlpp WITH KEY kalnr = wa_ckmlhd-kalnr BINARY SEARCH.
        CLEAR vsalk3.
        IF sy-subrc = 0.
          CLEAR:wa_ckmlcr.
          LOOP AT t_ckmlcr INTO wa_ckmlcr WHERE kalnr = wa_ckmlhd-kalnr.
            ADD wa_ckmlcr-salk3     TO vsalk3.
          ENDLOOP.
*** PBI - 67583 - Inicio - CSB * Comentei
        ELSE.

        ENDIF.
*** PBI - 67583 - Fim - CSB
        "
        "se ja gravou o saldo inicial para MATNR/WERKS não grava mais  ( COLLECT )
        READ TABLE it_matnr INTO wa_matnr WITH KEY matnr = wa_bsis-matnr
                                                   werks = wa_bsis-werks.
        IF sy-subrc = 0. " OR VGSBER NE WA_MSEG-WERKS.
          CLEAR:  vsalk3  , wa_ckmlpp.
        ELSE.
          wa_matnr-matnr = wa_bsis-matnr.
          wa_matnr-werks = wa_bsis-werks.
          APPEND wa_matnr TO it_matnr.
          "
          v_camp2 = 'SLD_FIMQ'.
          PERFORM f_carrega_dados USING wa_ckmlpp-lbkum     v_camp2.
          v_camp2 = 'SLD_FIMV'.
          PERFORM f_carrega_dados USING vsalk3              v_camp2.
        ENDIF.
        "fim


*** pbi - 67583 - inicio - csb
        CLEAR: vgkonts, wa_mbew ,wa_t030_aux .
        READ TABLE it_mbew INTO wa_mbew WITH KEY matnr = wa_bsis-matnr  bwkey = wa_bsis-werks BINARY SEARCH.

        IF sy-subrc = 0.
          READ TABLE it_t030_aux INTO wa_t030_aux WITH KEY  bklas = wa_mbew-bklas BINARY SEARCH.
          vgkonts =  wa_t030_aux-konts.
        ENDIF.

        CLEAR: wa_zmmt0148.
        READ TABLE it_zmmt0148 INTO wa_zmmt0148 WITH KEY bukrs = wa_bsis-bukrs centro_virt = wa_bsis-werks.
        IF sy-subrc = 0.

          vgcentro_virt   = wa_zmmt0148-centro_virt.
          vgcentro_ref    = wa_zmmt0148-centro_ref.
          vgorigem        = wa_zmmt0148-origem.
          vgagrup01       = wa_zmmt0148-agrup01.
          vgagrup02       = wa_zmmt0148-agrup02.
          vgagrup03       = wa_zmmt0148-agrup03.
          vgagrup04       = wa_zmmt0148-agrup04.
          vguf            = wa_zmmt0148-uf.
          vgcap_instalada = wa_zmmt0148-cap_instalada.

          PERFORM f_carrega_dados USING:
                  vgcentro_virt      'CENTRO_VIRT',
                  vgcentro_ref       'CENTRO_REF',
                  vgorigem           'ORIGEM',
                  vgagrup01          'AGRUP01',
                  vgagrup02          'AGRUP02',
                  vgagrup03          'AGRUP03',
                  vgagrup04          'AGRUP04',
                  vguf               'UF',
                  vgcap_instalada    'CAP_INSTALADA'.

        ENDIF.
*** PBI - 67583 - Fim - CSB

        PERFORM f_carrega_dados USING:
                vgsber             'GSBER',
                vgtext             'GTEXT',
                wa_bsis-werks      'WERKS',
                vgkonts            'KONTS',
                vmatnr             'MATNR',
                vvprsv             'VPRSV',
                wa_mara-meins      'MEINS',
                wa_makt-maktx      'MAKTX',
                wa_ckmlpp-lbkum    'SLD_INIQ',
                vsalk3             'SLD_INIV'.

        IF wa_bsis-shkzg = 'H'.
          MULTIPLY wa_bsis-dmbtr BY -1.
          MULTIPLY wa_bsis-dmbe2 BY -1.
        ENDIF.
        IF  '0000341000_0000341018_0000341032_0000341071_0000341068_0000341069_0000341091_0000341119' CS wa_bsis-hkont.
          v_camp = 'VLR_LEDGER'.
          CLEAR v_camp2.
          CLEAR: wa_docs.
          wa_docs-cd_agru = 8888. "doc ledger
        ELSE.
          v_camp = 'VAL_CTB'.
          v_camp2 = 'SLD_FIMV'.
          CLEAR: wa_docs.
          wa_docs-cd_agru = 9999. "doc de material sem agrupamento e sem doc contabil
        ENDIF.

        IF r_val = 'X'.
          IF p_curtp-low = 10.
            PERFORM f_carrega_dados USING wa_bsis-dmbtr     v_camp.
            PERFORM f_carrega_dados USING wa_bsis-dmbtr     v_camp2.
          ELSE.
            PERFORM f_carrega_dados USING wa_bsis-dmbe2     v_camp.
            PERFORM f_carrega_dados USING wa_bsis-dmbe2     v_camp2.
          ENDIF.
        ENDIF.
        "Grava analitico

        wa_docs-bukrs   = wa_bsis-bukrs.
        wa_docs-belnr   = wa_bsis-belnr.
        wa_docs-gjahr   = wa_bsis-gjahr.
        wa_docs-werks   = wa_bsis-werks.
        wa_docs-gsber   = vgsber.
        wa_docs-matnr   = wa_bsis-matnr.
        IF p_curtp-low = 10.
          wa_docs-dmbtr   = wa_bsis-dmbtr.
        ELSE.
          wa_docs-dmbtr   = wa_bsis-dmbe2.
        ENDIF.
        wa_docs-sgtxt   = wa_bsis-sgtxt.
        wa_docs-menge   = 0.
        APPEND wa_docs TO it_docs.

      ENDIF.
    ENDIF.

    PERFORM f_carrega_alv USING <fs_data2>
                                <wa_data2>.

    CLEAR           <wa_data2>.
    CLEAR           <wa_data3>.
    PERFORM f_carrega_alv USING <fs_data3>
                                <wa_data3>.

    CLEAR: wa_bsis, "CSB
           wa_docs. "CSB

***** CSB
    CLEAR: vgcentro_virt,
           vgcentro_ref,
           vgorigem,
           vgagrup01,
           vgagrup02,
           vgagrup03,
           vgagrup04,
           vguf,
           vgcap_instalada,
           v_camp,
           v_camp2.



  ENDLOOP.

  "Saldo inicial somente
  LOOP AT it_ckmlhd INTO wa_ckmlhd.
    READ TABLE it_matnr INTO wa_matnr WITH KEY matnr = wa_ckmlhd-matnr
                                               werks = wa_ckmlhd-bwkey.
    IF sy-subrc = 0. "
      CONTINUE.
    ENDIF.
    "
    CLEAR vgsber.
    SELECT SINGLE gsber
      FROM t134g
      INTO vgsber
      WHERE werks = wa_ckmlhd-bwkey.

    CLEAR: vgtext, vvprsv.
    READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_ckmlhd-matnr BINARY SEARCH.
    "
    SELECT SINGLE vprsv INTO vvprsv FROM mbew WHERE matnr = wa_ckmlhd-matnr
                                              AND   bwkey = wa_ckmlhd-bwkey.
    "
    SELECT SINGLE gtext INTO vgtext FROM tgsbt WHERE spras = sy-langu
                                               AND   gsber = vgsber.

    CLEAR: vgkonts, wa_mbew ,wa_t030_aux .
    READ TABLE it_mbew INTO wa_mbew WITH KEY matnr = wa_ckmlhd-matnr  bwkey = wa_ckmlhd-bwkey BINARY SEARCH.

    IF sy-subrc = 0.
      READ TABLE it_t030_aux INTO wa_t030_aux WITH KEY  bklas = wa_mbew-bklas BINARY SEARCH.
      vgkonts =  wa_t030_aux-konts.
    ENDIF.
    READ TABLE it_ckmlpp INTO wa_ckmlpp WITH KEY kalnr = wa_ckmlhd-kalnr BINARY SEARCH.
    CLEAR vsalk3.
    IF sy-subrc = 0.
      LOOP AT it_ckmlcr INTO wa_ckmlcr WHERE kalnr = wa_ckmlhd-kalnr.
        ADD wa_ckmlcr-salk3     TO vsalk3.
      ENDLOOP.
    ENDIF.

    IF vsalk3 = 0 AND wa_ckmlpp-lbkum  = 0.
      CONTINUE.
    ENDIF.

    "Campos fixos
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_mara-matnr
      IMPORTING
        output = vmatnr.

    PERFORM f_carrega_dados USING:
     vgsber             'GSBER',
     vgtext             'GTEXT',
     wa_ckmlhd-bwkey    'WERKS',
     vgkonts            'KONTS',
     vmatnr             'MATNR',
     vvprsv             'VPRSV',
     wa_mara-meins      'MEINS',
     wa_makt-maktx      'MAKTX',
     wa_ckmlpp-lbkum    'SLD_INIQ',
     vsalk3             'SLD_INIV',
     wa_ckmlpp-lbkum    'SLD_FIMQ',
     vsalk3             'SLD_FIMV'.
    PERFORM f_carrega_alv USING <fs_data2>
                               <wa_data2>.

    CLEAR           <wa_data2>.
    CLEAR           <wa_data3>.
    PERFORM f_carrega_alv USING <fs_data3>
                                <wa_data3>.



  ENDLOOP.

  "copia dados para a tabela que tem a cor
  LOOP AT <fs_data2> INTO <wa_data2>.
    MOVE-CORRESPONDING <wa_data2> TO <wa_data>.
    ASSIGN COMPONENT 'T_CELLCOLORS'   OF STRUCTURE <wa_data> TO <fs_campo>.
    MOVE it_color TO <fs_campo>.

    IF r_val = 'X'.
      ASSIGN COMPONENT 'VLR_LEDGER'   OF STRUCTURE <wa_data> TO <fs_campo>.
      <fs_campo> = <fs_campo> * -1. "Inverto sinal
    ENDIF.

    PERFORM f_carrega_alv2 USING <fs_data>
                                 <wa_data>.
    CLEAR <wa_data>.

    " totaliza colunas dinamicas
    MOVE-CORRESPONDING <wa_data2> TO <wa_data3>.
    ASSIGN COMPONENT 'WERKS'   OF STRUCTURE <wa_data3> TO <fs_campo>.
    CLEAR <fs_campo>.
    ASSIGN COMPONENT 'GSBER'   OF STRUCTURE <wa_data3> TO <fs_campo>.
    CLEAR <fs_campo>.
    ASSIGN COMPONENT 'GTEXT'   OF STRUCTURE <wa_data3> TO <fs_campo>.
    CLEAR <fs_campo>.
    ASSIGN COMPONENT 'MATNR'   OF STRUCTURE <wa_data3> TO <fs_campo>.
    CLEAR <fs_campo>.
    ASSIGN COMPONENT 'VPRSV'   OF STRUCTURE <wa_data3> TO <fs_campo>.
    CLEAR <fs_campo>.
    ASSIGN COMPONENT 'MEINS'   OF STRUCTURE <wa_data3> TO <fs_campo>.
    CLEAR <fs_campo>.
    ASSIGN COMPONENT 'MAKTX'   OF STRUCTURE <wa_data3> TO <fs_campo>.
    CLEAR <fs_campo>.
    PERFORM f_carrega_alv USING <fs_data3>
                                <wa_data3>.
    CLEAR: <wa_data2>, "CSB
           <wa_data3>.

  ENDLOOP.



  SORT it_docs BY cd_agru werks gsber matnr.
ENDFORM.
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  SET PF-STATUS '0100' EXCLUDING fcode.
  SET TITLEBAR  '0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_layout .
  REFRESH lt_fcat_lvc.
  PERFORM monta_fieldcat USING:
         'T_CELLCOLORS'      'ZCDF_CELL_COLOR' ''               ''   'T_CELL_COLOR',
         'GSBER'      'BSIS'      'Divisão'                     '10'       'GSBER',
         'GTEXT'      'TGSBT'     'Nome Divisão'                '20'       'GTEXT',
         'KONTS'      'T030'      'Conta Contábil'              '10'       'KONTS', " PBI - 67583 - CSB
         'WERKS'      'MSEG'      'Centro'                      '10'       'WERKS',
         'MATNR'      ' '         'Material'                    '12'       ' ',
         'MEINS'      'MARA'      'Un.Med'                      '07'       'MEINS',
         'VPRSV'      'MBEW'      'Ctr. Preço'                  '07'       'VPRSV',
         'MAKTX'      'MAKT'      'Descrição'                   '30'       'MAKTX',
         'VAL_MAT'    'BSIS'      'Vlr Classificar'             '15'       'DMBTR'.
*** PBI - 67583 - Inicio - CSB
  IF vg_cagrup = 'X'.
    PERFORM monta_fieldcat USING:
            'CENTRO_VIRT'    'ZMMT0148'     'Centro'                       '4'   'CENTRO_VIRT',
            'CENTRO_REF'     'ZMMT0148'     'Referência'                   '4'   'CENTRO_REF',
            'ORIGEM'         'ZMMT0148'     'Origem'                       '25'  'ORIGEM',
            'AGRUP01'        'ZMMT0148'     'Agrupamento 1'	               '25'  'AGRUP01',
            'AGRUP02'        'ZMMT0148'     'Agrupamento 2'	               '25'  'AGRUP02',
            'AGRUP03'        'ZMMT0148'     'Agrupamento 3'                '25'  'AGRUP03',
            'AGRUP04'        'ZMMT0148'     'Agrupamento 4'	               '25'  'AGRUP04',
            'UF'             'ZMMT0148'     'UF'                           '3'   'UF',
            'CAP_INSTALADA'  'ZMMT0148'     'Cap.Instalada'                '25'  'CAP_INSTALADA'.
  ENDIF.
*** PBI - 67583 - Fim - CSB

  IF r_val = 'X'.
    PERFORM monta_fieldcat USING:
    'SLD_INIV'   'CKMLCR'    'Valor Inicial'               '15'       'SALK3'.
  ENDIF.

  IF  r_qtd  = 'X'.
    PERFORM monta_fieldcat USING:
    'SLD_INIQ'   'CKMLPP'    'Estoque Inicial'             '15'       'LBKUM'.
  ENDIF.

  "Organiza ENTRADA/SAIDA
  SORT it_zmmt0084 BY seq entsai cd_agru.

  "Entradas
  LOOP AT it_zmmt0084 INTO wa_zmmt0084 WHERE entsai = 'E'.
    CLEAR: v_camp,
           v_text.
    CONCATENATE  wa_zmmt0084-entsai 'VAL'
                 wa_zmmt0084-cd_agru
                 INTO v_camp.

    IF r_val = 'X'.
      IF p_curtp-low = 10.
        CONCATENATE  wa_zmmt0084-ds_gru_val 'R$' INTO v_text SEPARATED BY space.
      ELSE.
        CONCATENATE  wa_zmmt0084-ds_gru_val 'USD' INTO v_text SEPARATED BY space.
      ENDIF.
      PERFORM monta_fieldcat USING v_camp   'BSIS' v_text            '15' 'DMBTR'.
    ENDIF.

    CLEAR: v_camp,
           v_text.
    CONCATENATE  wa_zmmt0084-entsai 'QTD'
                 wa_zmmt0084-cd_agru
                 INTO v_camp.

    IF r_qtd = 'X'.
      CONCATENATE  wa_zmmt0084-ds_gru_val 'Qt' INTO v_text SEPARATED BY space.
      PERFORM monta_fieldcat USING
             v_camp   'MSEG' v_text            '15' 'MENGE'.
    ENDIF.

  ENDLOOP.


  "Saldo final Entrada
  IF r_val = 'X'.
    PERFORM monta_fieldcat USING:
      'VAL_CTB'    'BSIS'      'Cpl. Ledger'                '15'       'DMBTR'.

    PERFORM monta_fieldcat USING:
          'SLD_FIMVE'   'CKMLCR'    'Valor das Entradas'  '15'       'SALK3'.
  ENDIF.

  IF  r_qtd  = 'X'.
    PERFORM monta_fieldcat USING:
          'SLD_FIMQE'   'CKMLPP'    'Total das Entradas'   '15'       'LBKUM'.
  ENDIF.

  "Saidas
  LOOP AT it_zmmt0084 INTO wa_zmmt0084 WHERE entsai = 'S'.
    CLEAR: v_camp,
           v_text.
    CONCATENATE  wa_zmmt0084-entsai 'VAL'
                 wa_zmmt0084-cd_agru
                 INTO v_camp.

    IF r_val = 'X'.
      IF p_curtp-low = 10.
        CONCATENATE  wa_zmmt0084-ds_gru_val 'R$' INTO v_text SEPARATED BY space.
      ELSE.
        CONCATENATE  wa_zmmt0084-ds_gru_val 'USD' INTO v_text SEPARATED BY space.
      ENDIF.
      PERFORM monta_fieldcat USING v_camp   'BSIS' v_text            '15' 'DMBTR'.
    ENDIF.

    CLEAR: v_camp,
           v_text.
    CONCATENATE  wa_zmmt0084-entsai 'QTD'
                 wa_zmmt0084-cd_agru
                 INTO v_camp.

    IF r_qtd = 'X'.
      CONCATENATE  wa_zmmt0084-ds_gru_val 'Qt' INTO v_text SEPARATED BY space.
      PERFORM monta_fieldcat USING
             v_camp   'MSEG' v_text            '15' 'MENGE'.
    ENDIF.

  ENDLOOP.

  "Saldo final Saida

  IF r_val = 'X'.
    PERFORM monta_fieldcat USING:
          'VLR_LEDGER'  'CKMLCR'    'Cpl. Ledger CPV'  '15'       'SALK3'.
    PERFORM monta_fieldcat USING:
          'SLD_FIMVS'   'CKMLCR'    'Valor das Saidas'  '15'       'SALK3'.
  ENDIF.

  IF r_qtd = 'X'.
    PERFORM monta_fieldcat USING:
          'SLD_FIMQS'   'CKMLPP'    'Total das Saidas'   '15'       'LBKUM'.
  ENDIF.

  "Saldo final
  IF r_val = 'X'.
    PERFORM monta_fieldcat USING:
         'SLD_FIMV'   'CKMLCR'    'Valor Final'  '15'       'SALK3'.
  ENDIF.

  IF r_qtd = 'X'.
    PERFORM monta_fieldcat USING:
          'SLD_FIMQ'   'CKMLPP'    'Estoque Final'   '15'       'LBKUM'.
  ENDIF.


*  TABELA DINAMICA
  DATA: t_alvdata TYPE REF TO data.

* Monta tabela dinâmica
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      i_style_table   = ' '
*                     tab com as informações de campo
      it_fieldcatalog = lt_fcat_lvc
    IMPORTING
*                     retorna tab dinâmica com campos informados
      ep_table        = t_data.

*  free lt_fcat_lvc.

  IF <fs_data> IS ASSIGNED.
    UNASSIGN <fs_data>.
    UNASSIGN <wa_data>.
    UNASSIGN <fs_campo> .
  ENDIF.

* Carrega <fs_data> com a estrutura dos campos passados para o metodo
  ASSIGN t_data->* TO <fs_data>.
  CREATE DATA t_alvdata LIKE LINE OF <fs_data>.
  ASSIGN t_alvdata->* TO <wa_data>.

  REFRESH <fs_data>.

  "apaga para não exibir coluna
  DELETE lt_fcat_lvc WHERE fieldname   =  'T_CELLCOLORS'.

*  TABELA DINAMICA
  DATA: t_alvdata2 TYPE REF TO data.

* Monta tabela dinâmica 2 sem a cor, (não funciona o COLLECT)
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      i_style_table   = ' '
*                     tab com as informações de campo
      it_fieldcatalog = lt_fcat_lvc
    IMPORTING
*                     retorna tab dinâmica com campos informados
      ep_table        = t_data2.

*  free lt_fcat_lvc.

  IF <fs_data2> IS ASSIGNED.
    UNASSIGN <fs_data2>.
    UNASSIGN <wa_data2>.
    UNASSIGN <fs_campo2> .
  ENDIF.

* Carrega <fs_data> com a estrutura dos campos passados para o metodo
  ASSIGN t_data2->* TO <fs_data2>.
  CREATE DATA t_alvdata2 LIKE LINE OF <fs_data2>.
  ASSIGN t_alvdata2->* TO <wa_data2>.

  REFRESH <fs_data2>.

*  TABELA DINAMICA
  DATA: t_alvdata3 TYPE REF TO data.

* Monta tabela dinâmica 2 sem a cor, (não funciona o COLLECT)
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      i_style_table   = ' '
*                     tab com as informações de campo
      it_fieldcatalog = lt_fcat_lvc
    IMPORTING
*                     retorna tab dinâmica com campos informados
      ep_table        = t_data3.

*  free lt_fcat_lvc.

  IF <fs_data3> IS ASSIGNED.
    UNASSIGN <fs_data3>.
    UNASSIGN <wa_data3>.
    UNASSIGN <fs_campo3> .
  ENDIF.

* Carrega <fs_data> com a estrutura dos campos passados para o metodo
  ASSIGN t_data3->* TO <fs_data3>.
  CREATE DATA t_alvdata3 LIKE LINE OF <fs_data3>.
  ASSIGN t_alvdata3->* TO <wa_data3>.

  REFRESH <fs_data3>.
ENDFORM.                    " MONTAR_LAYOUT


FORM monta_fieldcat USING p_field
                          p_tabref
                          p_text
                          p_out
                          p_ref_field.
  CLEAR:  wa_fcat_lvc.
  wa_fcat_lvc-fieldname   = p_field.
  wa_fcat_lvc-tabname     = '<FS_DATA>'.
  wa_fcat_lvc-seltext     = p_text.

  wa_fcat_lvc-scrtext_m  = p_text.
  wa_fcat_lvc-scrtext_l   = p_text.
  wa_fcat_lvc-scrtext_s   = p_text.

  wa_fcat_lvc-outputlen   = p_out.

  IF p_ref_field = 'DMBTR'.
    wa_fcat_lvc-datatype = 'CURR'.
    wa_fcat_lvc-intlen   = 16.
  ELSEIF p_ref_field = 'SALK3'.
    wa_fcat_lvc-datatype = 'CURR'.
    wa_fcat_lvc-intlen   = 21.
  ELSEIF p_ref_field = 'MENGE'.
    wa_fcat_lvc-datatype = 'QUAN'.
    wa_fcat_lvc-intlen   = 17.
  ELSEIF p_ref_field = 'LBKUM'.
    wa_fcat_lvc-datatype = 'QUAN'.
    wa_fcat_lvc-intlen   = 20.
  ELSE.
    wa_fcat_lvc-ref_table   = p_tabref.
    wa_fcat_lvc-ref_field   = p_ref_field.
  ENDIF.

  IF  p_field <> 'UF'.
    IF p_field+1(3) = 'VAL' OR  p_field = 'VAL_CTB' OR  p_field = 'VAL_MAT'.
      wa_fcat_lvc-hotspot = c_x.
    ENDIF.
  ENDIF.

*inclui dados da work-área p/ tabela sem cab.
  APPEND wa_fcat_lvc TO lt_fcat_lvc.

ENDFORM.                    " monta_fieldcat

FORM monta_fieldcat2 USING p_field
                          p_tabref
                          p_text
                          p_out
                          p_ref_field.
  CLEAR:  wa_fcat_lvc.
  wa_fcat_lvc-fieldname   = p_field.
  wa_fcat_lvc-tabname     = '<FS_DATA>'.
  wa_fcat_lvc-ref_table   = p_tabref.
  wa_fcat_lvc-seltext     = p_text.

  wa_fcat_lvc-scrtext_m  = p_text.
  wa_fcat_lvc-scrtext_l   = p_text.
  wa_fcat_lvc-scrtext_s   = p_text.

  wa_fcat_lvc-outputlen   = p_out.
  wa_fcat_lvc-ref_field   = p_ref_field.

  IF p_field = 'MBLNR' OR p_field = 'BELNR'.
    wa_fcat_lvc-hotspot = c_x.
  ENDIF.

  IF p_field = 'DMBTR' OR p_field = 'MENGE'.
    wa_fcat_lvc-do_sum = c_x.
  ENDIF.

*inclui dados da work-área p/ tabela sem cab.
  APPEND wa_fcat_lvc TO lt_fcat_lvc2.

ENDFORM.                    " monta_fieldcat


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv .

  CALL SCREEN 0100.
ENDFORM.

FORM f_carrega_dados USING    p_valor
                              p_campo.
  IF p_campo IS INITIAL.
    EXIT.
  ENDIF.
*Aponta <fs_campo> para <wa_data>-campo montado
  ASSIGN COMPONENT p_campo  OF STRUCTURE <wa_data2> TO <fs_campo2>.
  IF <fs_campo2> IS NOT ASSIGNED OR sy-subrc NE 0.
    EXIT.
  ENDIF.
  ASSIGN COMPONENT p_campo  OF STRUCTURE <wa_data3> TO <fs_campo3>.
  IF p_campo = 'SLD_FIMV' OR p_campo = 'SLD_FIMQ'.
    ADD p_valor TO <fs_campo2>.
  ELSE.
    MOVE p_valor TO <fs_campo2>.
  ENDIF.
ENDFORM.                    " f_carrega_dados

FORM f_carrega_alv USING    p_tab TYPE table
                            p_wa.

  COLLECT p_wa INTO p_tab.

ENDFORM.
FORM f_carrega_alv2 USING    p_tab TYPE table
                             p_wa.
  APPEND p_wa TO p_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  REFRESH : events,
            tl_filter,
            tl_function.

  CLEAR: wl_function,
         wl_filter  ,
         event    .

  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

  IF cl_container_95 IS INITIAL AND sy-batch NE 'X'.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF grid1 IS INITIAL.
    wa_layout-cwidth_opt = c_x.
    wa_layout-zebra       = c_x.
*    WA_LAYOUT-NO_TOOLBAR  = C_X.
    wa_layout-no_rowmark  = space.
    wa_layout-col_opt     = c_x.

    wa_layout-sel_mode    = 'B'.
    wa_layout-box_fname   = ''.
    wa_layout-ctab_fname  = 'T_CELLCOLORS'.

    IF sy-batch NE 'X'.
      CREATE OBJECT obj_dyndoc_id
        EXPORTING
*         STYLE      =
*         BACKGROUND_COLOR =
*         BDS_STYLESHEET =
          no_margins = 'X'.

      PERFORM zf_alv_header .


      IF editcontainer IS INITIAL .
        CREATE OBJECT editcontainer
          EXPORTING
            container_name = 'HEADER'.
      ENDIF .

      CALL METHOD obj_dyndoc_id->merge_document.

      CALL METHOD obj_dyndoc_id->display_document
        EXPORTING
          reuse_control      = 'X'
          parent             = editcontainer
        EXCEPTIONS
          html_display_error = 1.
    ENDIF.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = cl_container_95.

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

    CLEAR wa_layout-cwidth_opt.
    gs_variant_c-report = sy-repid.

** BUG - 70537 - Inicio - CSB
    FIELD-SYMBOLS: <ls_record>   TYPE any,
                   <ld_fld_iniv> TYPE any,
                   <ld_fld_iniq> TYPE any,
                   <ld_fld_fimv> TYPE any,
                   <ld_fld_fimq> TYPE any.

    LOOP AT  <fs_data> ASSIGNING <ls_record>.

      UNASSIGN  <ld_fld_iniv>.
      UNASSIGN  <ld_fld_iniq>.
      UNASSIGN  <ld_fld_fimv>.
      UNASSIGN  <ld_fld_fimq>.


      ASSIGN COMPONENT 'SLD_INIV' OF STRUCTURE <ls_record> TO <ld_fld_iniv>.
      ASSIGN COMPONENT 'SLD_INIQ' OF STRUCTURE <ls_record> TO <ld_fld_iniq>.
      ASSIGN COMPONENT 'SLD_FIMV' OF STRUCTURE <ls_record> TO <ld_fld_fimv>.
      ASSIGN COMPONENT 'SLD_FIMQ' OF STRUCTURE <ls_record> TO <ld_fld_fimq>.

                                                            "IR086019
*      IF ( <ld_fld_iniv>  IS BOUND ) AND
*         ( <ld_fld_iniq>  IS BOUND ) AND
*         ( <ld_fld_fimv>  IS BOUND ) AND
*         ( <ld_fld_fimq>  IS BOUND ).
      "
**      IF ( <ld_fld_iniv>  = 0 ) AND
**         ( <ld_fld_iniq>  = 0 ) AND
**         ( <ld_fld_fimv>  = 0 ) AND
**         ( <ld_fld_fimq>  = 0 ).
**
**        DELETE TABLE <fs_data> FROM <ls_record>.
**
***        ENDIF.
**      ENDIF.

    ENDLOOP.
** BUG - 70537 - Fim - CSB


    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_c
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
        i_default            = 'X'
        i_save               = 'X'
      CHANGING
        it_fieldcatalog      = lt_fcat_lvc[]
        it_outtab            = <fs_data>.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* PBI - 67583 - Inicio - CSB
*    SET HANDLER: lcl_event_handler=>on_double_click FOR grid1.
    SET HANDLER: lcl_event_handler=>catch_hotspot_grid01 FOR grid1.
* PBI - 67583 - Fim- CSB


    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = lt_fcat_lvc[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.


  ELSE.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = lt_fcat_lvc[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR  '0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_0200 OUTPUT.

  REFRESH : events,
            tl_filter,
            tl_function.

  CLEAR: wl_function,
         wl_filter  ,
         event    .


  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

  IF g_custom_conta0200 IS INITIAL.
    wa_layout-cwidth_opt = c_x.
    wa_layout-zebra       = c_x.
    wa_layout-no_rowmark  = space.
    wa_layout-sel_mode    = 'B'.
    wa_layout-box_fname   = ''.

    CREATE OBJECT g_custom_conta0200
      EXPORTING
        container_name = g_container2.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_conta0200
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_2.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = container_2.

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

    CLEAR wa_layout-cwidth_opt.
    gs_variant_2-report = sy-repid.

    PERFORM f_monta_layout2.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant_2
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
        i_default            = 'X'
      CHANGING
        it_fieldcatalog      = lt_fcat_lvc2[]
        it_outtab            = it_docs_alv[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    SET HANDLER:
              lcl_event_handler=>catch_hotspot FOR grid2.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.

    CALL METHOD grid2->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = lt_fcat_lvc2[].

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ELSE.

    CALL METHOD grid2->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = lt_fcat_lvc2[].

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_layout2 .
  REFRESH lt_fcat_lvc2.
  PERFORM monta_fieldcat2 USING:
         'MBLNR'      'MSEG'      'Doc.Material'   '10'       'MBLNR',
         'BWART'      'MSEG'      'Tipo'           '06'       'BWART',
         'BELNR'      'BSIS'      'Doc.Contabil'   '10'       'BELNR',
         'DMBTR'      'BSIS'      'Vlr Contabil'   '15'       'DMBTR',
         'MENGE'      'MSEG'      'Quantidade'     '15'       'MENGE',
         'SGTXT'      'BSIS'      'texto contabil' '30'       'SGTXT'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv_header .
  DATA:   wl_data(10),
           wl_hora(8),
           wl_linha(60),
           wl_text TYPE sdydo_text_element.

  CLEAR wl_text .

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.


  CONCATENATE  'Empresa:' s_bukrs-low
          INTO wl_linha SEPARATED BY space.
  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


  IF p_werks-high IS INITIAL.
    CONCATENATE 'Centro  :' p_werks-low
     INTO wl_linha SEPARATED BY space.
  ELSE.
    CONCATENATE 'Centro  :' p_werks-low 'à' p_werks-high
    INTO wl_linha SEPARATED BY space.
  ENDIF.
  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.





  CONCATENATE 'Ano  :'  p_bdatj
     INTO wl_linha SEPARATED BY space.

  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


  IF p_poper-high IS INITIAL.
    CONCATENATE 'Mês  :' p_poper-low
    INTO wl_linha SEPARATED BY space.
  ELSE.
    CONCATENATE 'Mês :' p_poper-low  INTO wl_linha SEPARATED BY space.
    CONCATENATE wl_linha 'à' p_poper-high  INTO wl_linha SEPARATED BY space.
  ENDIF.
  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.



  CONCATENATE 'Moeda  :' p_curtp-low
  INTO wl_linha SEPARATED BY space.

  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


ENDFORM.

*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Início de Alteração
FORM f_buscar_bschl_est.

  DATA:
        wl_bschl LIKE LINE OF r_bschl_est.

  wl_bschl-sign   = 'I'.
  wl_bschl-option = 'EQ'.
  wl_bschl-low    = '40'.
  APPEND wl_bschl TO r_bschl_est.

  wl_bschl-sign   = 'I'.
  wl_bschl-option = 'EQ'.
  wl_bschl-low    = '50'.
  APPEND wl_bschl TO r_bschl_est.

ENDFORM.

FORM f_buscar_estorno_lanc.

  DATA:
        lt_bsis TYPE TABLE OF ty_bsis.

  lt_bsis = it_bsis[].

  DELETE lt_bsis WHERE bschl NOT IN r_bschl_est.

  SORT lt_bsis BY bukrs ghkon gjahr belnr hkont bschl.
  DELETE ADJACENT DUPLICATES FROM lt_bsis COMPARING bukrs ghkon gjahr belnr hkont bschl.

  IF it_bsis IS NOT INITIAL.

    SELECT bukrs hkont gjahr belnr ghkon bschl
      FROM bsis
      INTO TABLE it_bsis_est_lanc
      FOR ALL ENTRIES IN it_bsis
      WHERE bukrs = it_bsis-bukrs
        AND hkont = it_bsis-ghkon
        AND gjahr = it_bsis-gjahr
        AND belnr = it_bsis-belnr
        AND ghkon = it_bsis-hkont
        AND bschl IN r_bschl_est.

    IF sy-subrc IS INITIAL.
      SORT it_bsis_est_lanc BY bukrs hkont gjahr belnr ghkon bschl.
    ENDIF.

  ENDIF.

ENDFORM.
*** Stefanini - IR215576 - 21/01/2025 - LAZAROSR - Fim de Alteração
