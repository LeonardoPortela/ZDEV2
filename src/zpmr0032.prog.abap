*--------------------------------------------------------------------------------------------------------*
*&                          AMAGGI
*--------------------------------------------------------------------------------------------------------*
*& REPORT ZPMR0032.                                                                                      *
*& Data           : 19/01/2018                                                                           *
*& Especificado   : Anderson Oenning                                                                     *
*& Desenvolvimento: Anderson Oenning                                                                     *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*--------------------------------------------------------------------------------------------------------*
REPORT zpmr0032.

*----------------------------------------------------*
*               Tables                               *
*----------------------------------------------------*

TABLES: jest, aufk, eqkt, equi, viaufkst, csku, afko, afvc, coep, ekbe, makt, t247, setleaf, t357, itob.


*----------------------------------------------------*
*               Types                                *
*----------------------------------------------------*

TYPES:

  BEGIN OF ty_jest ,         " Status da Ordem
    objnr TYPE jest-objnr,   " Objeto
    stat  TYPE jest-stat,    " Nº do objeto
    txt04 TYPE tj02t-txt04,  " Descrição dos status
  END OF ty_jest,

  BEGIN OF ty_tj02t,
    istat TYPE tj02t-istat,
    txt04 TYPE tj02t-txt04,
  END OF ty_tj02t,

  BEGIN OF ty_equi,
    iwerk TYPE v_equi-iwerk,
    equnr TYPE v_equi-equnr,
    eqtyp TYPE v_equi-eqtyp,
    iloan TYPE v_equi-iloan,
    datbi TYPE v_equi-datbi,
    beber TYPE v_equi-beber,
  END OF ty_equi,

  BEGIN OF ty_aufk,            "Seleção de ordens.
    aufnr    TYPE aufk-aufnr,
    werks    TYPE aufk-werks,
    user4    TYPE aufk-user4,
    auart    TYPE aufk-auart,
    autyp    TYPE aufk-autyp,
    ktext    TYPE aufk-ktext,
    objnr    TYPE aufk-objnr,
    bukrs    TYPE aufk-bukrs,
    erdat    TYPE aufk-erdat,
    vornr    TYPE afvc-vornr,
    ltxa1    TYPE afvc-ltxa1,
    vaplz    TYPE aufk-vaplz,
    idat2    TYPE aufk-idat2,
    status   TYPE char30,
    d_status TYPE char30,
  END OF ty_aufk,

  BEGIN OF ty_afko,             "Seleção de informação do objeto
    aufnr  TYPE afko-aufnr,
    rsnum  TYPE afko-rsnum,
    aufpl  TYPE afko-aufpl,
    plnbez TYPE afko-plnbez,
  END OF ty_afko,

  BEGIN OF ty_afvc,             "Selecão de informação operação da ordem
    aufpl TYPE afvc-aufpl,
    objnr TYPE afvc-objnr,
    vornr TYPE afvc-vornr,
    werks TYPE afvc-werks,
    tplnr TYPE afvc-tplnr,
    equnr TYPE afvc-equnr,
    ltxa1 TYPE afvc-ltxa1,
  END OF ty_afvc,

  BEGIN OF ty_iloa,
    tplnr TYPE iloa-tplnr,
    beber TYPE iloa-beber,
    swerk TYPE iloa-swerk,
  END OF ty_iloa,

  BEGIN OF ty_viaufkst,         "Seleção de informação equipamento e local instalação
    objnr      TYPE aufk-objnr,
    aufnr      TYPE aufk-aufnr,
    equnr      TYPE viaufkst-equnr,
    tplnr      TYPE viaufkst-tplnr,
    erdat      TYPE aufk-erdat,
    werks      TYPE aufk-werks,
    bukrs      TYPE aufk-bukrs,
    kostl      TYPE iloa-kostl,
    vaplz      TYPE viaufkst-vaplz,
    vornr      TYPE afvc-vornr,
    ltxa1      TYPE afvc-ltxa1,
    auart      TYPE viaufkst-auart,
    idat2      TYPE viaufkst-idat2,
    status     TYPE char10,
    des_status TYPE char10,
  END OF ty_viaufkst,

  BEGIN OF ty_ekkn,             "Seleção de informação classe de custo
    ebeln TYPE ekkn-ebeln,
    ebelp TYPE ekkn-ebelp,
    aedat TYPE ekkn-aedat,
    menge TYPE ekkn-menge,
    aufnr TYPE ekkn-aufnr,
  END OF ty_ekkn,

  BEGIN OF ty_csku,             "Seleção de informação classe de custo
    spras TYPE csku-spras,
    kstar TYPE csku-kstar,
    ktext TYPE csku-ktext,
  END OF ty_csku,

  BEGIN OF ty_ekpo,              "Seleção de informação do pedido compra
    ebeln TYPE ekpo-ebeln,
    ebelp TYPE ekpo-ebelp,
    txz01 TYPE ekpo-txz01,
    matnr TYPE ekpo-matnr,
    menge TYPE ekpo-menge,
  END OF ty_ekpo,

  BEGIN OF ty_itob,
    equnr TYPE itob-equnr,
    beber TYPE itob-beber,
    swerk TYPE itob-swerk,
    iloan TYPE iloa-iloan,
  END OF ty_itob,

  BEGIN OF ty_t357,
    werks TYPE t357-werks,
    beber TYPE t357-beber,
    fing  TYPE t357-fing,
  END OF ty_t357,

  BEGIN OF ty_makt,              "Seleção informação do material
    matnr TYPE makt-matnr,
    spras TYPE makt-spras,
    maktx TYPE makt-maktx,
    maktg TYPE makt-maktg,
  END OF ty_makt,

  BEGIN OF ty_cskt,
    kostl TYPE cskt-kostl,
    ktext TYPE cskt-ktext,
  END OF ty_cskt,

  BEGIN OF ty_setleaf,
    setname TYPE setleaf-setname, "Nome do grupo classe de custo
    valfrom TYPE setleaf-valfrom, "classe de custo
    kstar   TYPE coep-kstar,
  END OF ty_setleaf,

  BEGIN OF ty_coep,              "Seleção informação custo
    werks   TYPE coep-werks,
    bukrs   TYPE coep-bukrs,
    objnr   TYPE coep-objnr,
    perio   TYPE coep-perio,
    gjahr   TYPE coep-gjahr,
    kstar   TYPE coep-kstar,
    wtgbtr  TYPE mseg-menge,
    meinb   TYPE coep-meinb,
    zlenr   TYPE coep-zlenr,
    matnr   TYPE coep-matnr,
    ebeln   TYPE coep-ebeln,
    ebelp   TYPE coep-ebelp,
    vrgng   TYPE coep-vrgng,
    sgtxt   TYPE coep-sgtxt,
    belnr   TYPE coep-belnr,
    megbtr  TYPE coep-megbtr,
    mbgbtr  TYPE coep-mbgbtr,
    buzei   TYPE coep-buzei,
    refbz   TYPE coep-refbz,
    valfrom TYPE setleaf-valfrom, "classe de custo
    gsber   TYPE coep-gsber,
  END OF ty_coep,

  BEGIN OF ty_cobk,
    belnr TYPE cobk-belnr,
    refbn TYPE cobk-refbn,
    bldat TYPE cobk-bldat,
    budat TYPE cobk-budat,
  END OF ty_cobk,

  BEGIN OF ty_eqkt,
    equnr TYPE eqkt-equnr,
    eqktx TYPE eqkt-eqktx,
  END OF ty_eqkt,

  BEGIN OF ty_iflo,
    tplnr TYPE iflo-tplnr,
    pltxt TYPE iflo-pltxt,
  END OF ty_iflo,

  BEGIN OF y_arbpl,
    werks TYPE crhd-werks,
    arbpl TYPE crhd-arbpl,
  END OF y_arbpl,

  BEGIN OF ty_mseg,
    mblnr TYPE mseg-mblnr,
    bwart TYPE mseg-bwart,
    matnr TYPE mseg-matnr,
    ebelp TYPE mseg-ebelp,
    lgort TYPE mseg-lgort,
    menge TYPE mseg-menge,
    aufnr TYPE mseg-aufnr,
    rsnum TYPE mseg-rsnum,
  END OF ty_mseg,

  BEGIN OF ty_regmseg,
    mblnr  TYPE mseg-mblnr,
    bwart  TYPE mseg-bwart,
    matnr  TYPE mseg-matnr,
    ebelp  TYPE mseg-ebelp,
    lgort  TYPE mseg-lgort,
    menge  TYPE mseg-menge,
    aufnr  TYPE mseg-aufnr,
    rsnum  TYPE mseg-rsnum,
    belnr  TYPE cobk-belnr,
    refbn  TYPE cobk-refbn,
    bldat  TYPE cobk-bldat,
    budat  TYPE cobk-budat,
    objnr  TYPE coep-objnr,
    wtgbtr TYPE coep-wtgbtr,
    vrgng  TYPE coep-vrgng,
    pargb  TYPE coep-pargb,
    werks  TYPE coep-werks,
    ebeln  TYPE coep-ebeln,
    megbtr TYPE coep-megbtr,
  END OF ty_regmseg,

  BEGIN OF ty_saida,
    werks   TYPE coep-werks,
    bukrs   TYPE coep-bukrs,
    aufnr   TYPE aufk-aufnr,
    equnr   TYPE afih-equnr,
    eqktx   TYPE eqkt-eqktx,
    objnr   TYPE coep-objnr,
    perio   TYPE coep-perio,
    gjahr   TYPE coep-gjahr,
    kstar   TYPE coep-kstar,
    ktext   TYPE csku-ktext,
    wtgbtr  TYPE aufk-user4,
    meinb   TYPE coep-meinb,
    zlenr   TYPE coep-zlenr,
    matnr   TYPE coep-matnr,
    maktx   TYPE makt-maktx,
    ebeln   TYPE coep-ebeln,
    ebelp   TYPE coep-ebelp,
    txz01   TYPE ekpo-txz01,
    p_menge TYPE ekpo-menge,
    vrgng   TYPE coep-vrgng,
    sgtxt   TYPE coep-sgtxt,
    vornr   TYPE afvc-vornr,
    lgort   TYPE mseg-lgort,
    menge   TYPE aufk-user4,
    rsnum   TYPE mseg-rsnum,
    refbn   TYPE cobk-refbn,
    bwart   TYPE mseg-bwart,
    vlr_unt TYPE aufk-user4,
    tplnr   TYPE iloa-tplnr,
    pltxt   TYPE iflo-pltxt,
    bldat   TYPE cobk-bldat,
    budat   TYPE cobk-budat,
    kostl   TYPE viaufkst-kostl,
    t_ktext TYPE cskt-ktext,
    eqtyp   TYPE equi-eqtyp,
    setname TYPE setleaf-setname,
    valfrom TYPE setleaf-valfrom,
    ltxa1   TYPE afvc-ltxa1,
    vaplz   TYPE viaufkst-vaplz,
    auart   TYPE viaufkst-auart,
    beber   TYPE t357-beber,
    fing    TYPE t357-fing,
  END OF ty_saida.

TYPES: BEGIN OF ty_zcoep.
         INCLUDE TYPE zcoep.
TYPES:   marc TYPE c,
       END OF ty_zcoep.


DATA: BEGIN OF ijstat OCCURS 0.
        INCLUDE STRUCTURE jstat.
DATA:   marc TYPE c.
DATA: END OF ijstat.

DATA: BEGIN OF h_status_text_tab OCCURS 20,
        istat LIKE tj02t-istat,
        txt04 LIKE tj02t-txt04.
DATA: END OF h_status_text_tab.
*----------------------------------------------------------*
*           Definição de variáveis globais                 *
*----------------------------------------------------------*


DATA:
  zcoep       TYPE TABLE OF ty_zcoep,
  t_aufk      TYPE STANDARD TABLE OF ty_aufk,
  t_afko      TYPE TABLE OF ty_afko WITH HEADER LINE,
  t_afvc      TYPE TABLE OF ty_afvc WITH HEADER LINE,
  t_viaufkst  TYPE TABLE OF ty_viaufkst WITH HEADER LINE,
  t_coep      TYPE TABLE OF ty_coep WITH HEADER LINE,
  t_ekpo      TYPE TABLE OF ty_ekpo WITH HEADER LINE,
  t_makt      TYPE TABLE OF ty_makt WITH HEADER LINE,
  t_csku      TYPE TABLE OF ty_csku WITH HEADER LINE,
  t_eqkt      TYPE TABLE OF ty_eqkt WITH HEADER LINE,
  t_cobk      TYPE TABLE OF ty_cobk WITH HEADER LINE,
  t_mseg      TYPE TABLE OF ty_mseg WITH HEADER LINE,
  t_regmseg   TYPE TABLE OF ty_regmseg WITH HEADER LINE,
  t_iflo      TYPE TABLE OF ty_iflo WITH HEADER LINE,
  t_cskt      TYPE TABLE OF ty_cskt WITH HEADER LINE,
  t_ekkn      TYPE TABLE OF ty_ekkn WITH HEADER LINE,
  t_equi      TYPE TABLE OF ty_equi WITH HEADER LINE,
  t_jest      TYPE TABLE OF ty_jest     WITH HEADER LINE,
  t_setleaf   TYPE TABLE OF ty_setleaf WITH HEADER LINE,
  it_arbpl    TYPE TABLE OF y_arbpl WITH HEADER LINE,
  it_return   TYPE TABLE OF ddshretval,
  wa_return   LIKE LINE OF  it_return,
  t_saida     TYPE TABLE OF ty_saida,
  wa_saida    TYPE ty_saida,
  it_aufk_aux TYPE STANDARD TABLE OF ty_aufk WITH HEADER LINE,
  wa_aufk     TYPE ty_aufk,
  wa_viaufks  TYPE ty_viaufkst,
  it_t357     TYPE TABLE OF ty_t357,
  it_itob     TYPE TABLE OF ty_itob WITH HEADER LINE,
  wa_itob     TYPE ty_itob,
  wa_t357     TYPE ty_t357,
  it_iloa     TYPE TABLE OF ty_iloa,
  wa_iloa     TYPE ty_iloa.


DATA:
  "IT_SELECT           TYPE STANDARD TABLE OF TY_DADOS_IMOB,
  g_custom_container TYPE REF TO cl_gui_custom_container,
  dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
  dg_parent_1        TYPE REF TO cl_gui_container,
  dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
  dg_parent_2        TYPE REF TO cl_gui_container,
  dg_parent_2a       TYPE REF TO cl_gui_container,
  dg_parent_alv      TYPE REF TO cl_gui_container,
  picture            TYPE REF TO cl_gui_picture,
  ctl_alv            TYPE REF TO cl_gui_alv_grid,
  dg_dyndoc_id       TYPE REF TO cl_dd_document,
  table_element      TYPE REF TO cl_dd_table_element,
  column             TYPE REF TO cl_dd_area,
  table_element2     TYPE REF TO cl_dd_table_element,
  column_1           TYPE REF TO cl_dd_area,
  dg_html_cntrl      TYPE REF TO cl_gui_html_viewer,
  it_exclude_fcode   TYPE ui_functions,
  wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
  gs_layout          TYPE lvc_s_layo,
  gs_variant         TYPE disvariant,
  it_fieldcatalog    TYPE lvc_t_fcat,
  wa_fieldcatalog    TYPE lvc_s_fcat,
  it_sort            TYPE lvc_t_sort,
  "GS_SCROLL_COL       TYPE LVC_S_COL,
  "GS_SCROLL_ROW       TYPE LVC_S_ROID,
  "GS_STABLE           TYPE LVC_S_STBL,
  "IT_SELECTED_ROWS    TYPE LVC_T_ROW,
  "WA_SELECTED_ROWS    TYPE LVC_S_ROW,
  ls_stable          TYPE lvc_s_stbl,
  t_sort             TYPE lvc_t_sort,
  w_sort             TYPE lvc_t_sort WITH HEADER LINE.

"LCL_EVENT DEFINITION

DATA: "R_EVENT_HANDLER TYPE REF TO LCL_EVENTS_HANDLER,
  i_selected_rows TYPE lvc_t_row,                "Linhas selecionadas
  w_selected_rows TYPE lvc_s_row,                "Colunas Selecionadas
  lc_i0002(5)     VALUE 'I0002',   "Liberada
  lc_i0001(5)     VALUE 'I0001',   "Aberto
  lc_i0045(5)     VALUE 'I0045',   "Encerrada
  lc_i(1)         VALUE 'I',
  lc_eq(2)        VALUE 'EQ',
  p_erro          TYPE p DECIMALS 1.

DATA:
     v_tabix     TYPE sy-tabix. " guardar o indice

RANGES:
    r_stat        FOR   jest-stat.


CLASS lcl_eventos DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

ENDCLASS.





*----------------------------------------------------*
*                Parâmetros de Seleção               *
*----------------------------------------------------*

*Status autorização.
*SELECTION-SCREEN BEGIN OF BLOCK B6 WITH FRAME TITLE TEXT-013.
*SELECTION-SCREEN BEGIN OF LINE .
*
*SELECTION-SCREEN COMMENT 1(08) TEXT-011.
*SELECTION-SCREEN POSITION 11.
*PARAMETERS: P_LIB AS CHECKBOX DEFAULT 'X'.
*
*SELECTION-SCREEN POSITION 10.
*SELECTION-SCREEN COMMENT 15(09) TEXT-012.
*PARAMETERS: P_ENC AS CHECKBOX DEFAULT 'X'.
*
**SELECTION-SCREEN POSITION 26.
**SELECTION-SCREEN COMMENT 31(08) TEXT-007.
**PARAMETERS: P_REC AS CHECKBOX DEFAULT 'X'.
*
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN: END OF BLOCK B6.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:  p_bukrs FOR coep-bukrs NO-EXTENSION NO INTERVALS OBLIGATORY,
                   p_werks FOR coep-werks OBLIGATORY,
                   p_auart FOR viaufkst-auart,
                   p_aufnr FOR viaufkst-aufnr,
                   p_ano   FOR coep-gjahr NO-EXTENSION NO INTERVALS OBLIGATORY,
                   p_perio FOR coep-perio OBLIGATORY,
                   p_arbpl FOR viaufkst-vaplz. "MATCHCODE OBJECT CRAM.
*PARAMETERS:
*                P_ARBPL TYPE VIAUFKST-VAPLZ.  " Centro de trabalho
SELECTION-SCREEN: END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-010.
  SELECT-OPTIONS:  p_equnr FOR viaufkst-equnr,
                   p_eqtyp FOR equi-eqtyp, "DEFAULT '*',
                   p_tplnr FOR viaufkst-tplnr,
                   p_beber FOR t357-beber.
SELECTION-SCREEN: END OF BLOCK b4.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-008.
  SELECT-OPTIONS:  p_kostl FOR viaufkst-kostl,
                   p_kstar FOR coep-kstar,
                   p_grpcla FOR setleaf-setname.
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-006.
  SELECT-OPTIONS:  p_matnr FOR coep-matnr.

SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arbpl-low.
  PERFORM zf_select_arbpl.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arbpl-high.
  PERFORM zf_select_arbpl.
*---------------------------------------------------*
*               Evento de inicialização             *
*---------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_beber-low.
  PERFORM selec_areaoperacional.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_beber-high.
  PERFORM selec_areaoperacional.


START-OF-SELECTION.

*  IF P_LIB IS INITIAL AND P_ENC IS INITIAL.
*    MESSAGE 'Marcar status da ordem' TYPE 'S' DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.

*  IF P_LIB IS NOT INITIAL.
*    PERFORM Z_SEL_ORD_LIB.
*  ELSE.
*    PERFORM Z_SEL_ORD_ENC.
*  ENDIF.


  IF p_aufnr IS NOT INITIAL
   OR p_auart IS NOT INITIAL
   OR p_equnr IS NOT INITIAL
   OR p_eqtyp IS NOT INITIAL
   OR p_tplnr IS NOT INITIAL
   OR p_kostl IS NOT INITIAL
   OR p_arbpl IS NOT INITIAL
   OR p_kstar IS NOT INITIAL
   OR p_matnr IS NOT INITIAL
   OR p_grpcla IS NOT INITIAL.
    PERFORM zf_selecao.
    IF t_coep[] IS NOT INITIAL.
      PERFORM zf_saida_dados.
      IF t_saida IS NOT INITIAL.
        PERFORM zt_execut_alv.
      ELSE.
        MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ELSE.
    MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  ZF_SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM zf_selecao .
  DATA: t_jest TYPE TABLE OF jest.
  DATA: r_status TYPE zrsdsselopts .
  DATA: r_objnrr  TYPE zrsdsselopts .
  DATA: r_objn  TYPE zrsdsselopts .
  DATA: r_objnr TYPE zrsdsselopts .
  DATA: tl_viaufkst TYPE TABLE OF viaufkst.

  DATA: p_aber TYPE jest-stat VALUE 'I0001'. "Aberto
  DATA: p_libr TYPE jest-stat VALUE 'I0002'. "Liberado
  DATA: p_ente TYPE jest-stat VALUE 'I0045'. "Concluído tecnicamente
  DATA: p_ence TYPE jest-stat VALUE 'I0046'. "Encerrado


*  IF P_LIB IS NOT INITIAL AND P_ENC IS NOT INITIAL.
  PERFORM f_lupa USING 'Selecionando ordens de manutenção!' space.

  APPEND VALUE #( sign = 'I' option  = 'EQ' low   = p_libr high = ' ' ) TO r_status.
  APPEND VALUE #( sign = 'I' option  = 'EQ' low   = p_ente high = ' ' ) TO r_status.
  APPEND VALUE #( sign = 'I' option  = 'EQ' low   = p_ence high = ' ' ) TO r_status.

  "==================================================IR135075 / AOENNING
*  IF p_equnr IS NOT INITIAL OR p_tplnr IS NOT INITIAL.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE @t_viaufkst
  FROM aufk AS a
  LEFT JOIN viaufkst AS b ON b~aufnr EQ a~aufnr
  WHERE b~vaplz IN @p_arbpl
   AND  a~auart IN @p_auart
   AND  a~aufnr IN @p_aufnr
   AND  a~werks IN @p_werks
   AND  a~bukrs IN @p_bukrs
   AND  a~kostl IN @p_kostl
   AND  b~equnr IN @p_equnr
   AND  b~tplnr IN @p_tplnr
   AND  ( EXISTS ( SELECT * FROM jest
                   WHERE objnr EQ a~objnr
                     AND inact NE @abap_true
                     AND stat IN @r_status ) ).
*  ELSE.
*
*  SELECT *
*  INTO CORRESPONDING FIELDS OF TABLE t_viaufkst
*  FROM aufk
*  WHERE vaplz IN p_arbpl
*   AND  auart IN p_auart
*   AND  aufnr IN p_aufnr
*   AND  werks IN p_werks
*   AND  bukrs IN p_bukrs
*   AND  kostl IN p_kostl
*   AND  ( EXISTS ( SELECT * FROM jest
*                   WHERE objnr EQ aufk~objnr
*                     AND inact NE abap_true
*                     AND stat IN r_status ) ).
*
*
*
*  ENDIF.
  "========================================================================
  IF t_viaufkst[] IS NOT INITIAL.
    PERFORM f_lupa USING 'Selecionando equipamentos!' space.

    SELECT *
  INTO CORRESPONDING FIELDS OF TABLE t_equi
  FROM v_equi
     FOR ALL ENTRIES IN t_viaufkst
  WHERE equnr EQ t_viaufkst-equnr
    AND eqtyp IN p_eqtyp
    AND beber IN p_beber
    AND datbi EQ '99991231'.

    SELECT *
    FROM t357
    INTO CORRESPONDING FIELDS OF TABLE it_t357
    FOR ALL ENTRIES IN t_equi
    WHERE beber EQ t_equi-beber
      AND werks EQ t_equi-iwerk.

    SELECT *
    FROM iflo
    INTO CORRESPONDING FIELDS OF TABLE t_iflo
    FOR ALL ENTRIES IN t_viaufkst
    WHERE tplnr EQ t_viaufkst-tplnr.

    SELECT *
    FROM eqkt INTO CORRESPONDING FIELDS OF TABLE t_eqkt
    FOR ALL ENTRIES IN t_viaufkst
    WHERE equnr EQ t_viaufkst-equnr.

    SELECT *
    FROM cskt
    INTO CORRESPONDING FIELDS OF TABLE t_cskt
    FOR ALL ENTRIES IN t_viaufkst
    WHERE kostl EQ t_viaufkst-kostl
    AND   spras EQ 'PT'.

    SELECT *
    FROM aufk
    INTO CORRESPONDING FIELDS OF TABLE it_aufk_aux
    FOR ALL ENTRIES IN t_viaufkst
    WHERE aufnr EQ t_viaufkst-aufnr.

    FREE t_aufk.
*    MOVE-CORRESPONDING T_VIAUFKST TO IT_AUFK_AUX.
    SELECT o~aufnr b~objnr b~bukrs b~vornr b~ltxa1
    FROM afko AS o
    INNER JOIN afvc AS b ON b~aufpl EQ o~aufpl
    INTO CORRESPONDING FIELDS OF TABLE t_aufk
          FOR ALL ENTRIES IN t_viaufkst
    WHERE o~aufnr EQ t_viaufkst-aufnr
     AND  b~bukrs IN p_bukrs
     AND  b~werks IN p_werks.

    SORT it_aufk_aux BY objnr.
    SORT t_aufk BY objnr.
    APPEND LINES OF it_aufk_aux[] TO t_aufk.
    SORT t_aufk BY objnr.
    DELETE ADJACENT DUPLICATES FROM t_aufk COMPARING objnr.

    IF t_aufk IS NOT INITIAL.

      PERFORM f_lupa USING 'Selecionando custo das ordens!' space.
      SELECT *
      FROM coep
      INTO CORRESPONDING FIELDS OF TABLE t_coep
      FOR ALL ENTRIES IN t_aufk
      WHERE objnr EQ t_aufk-objnr
      AND   gjahr IN p_ano
      AND   perio IN p_perio
      AND   matnr IN p_matnr
      AND   kstar IN p_kstar
      AND   vrgng EQ 'COIN'.

      LOOP AT t_coep ASSIGNING FIELD-SYMBOL(<coep>).
        <coep>-valfrom = <coep>-kstar.
      ENDLOOP.
    ENDIF.

    IF t_coep[] IS NOT INITIAL.

      SELECT *
      FROM csku INTO CORRESPONDING FIELDS OF TABLE t_csku
      FOR ALL ENTRIES IN t_coep
      WHERE kstar EQ t_coep-kstar
      AND   spras EQ 'PT'.

      SELECT *
      INTO CORRESPONDING FIELDS OF TABLE t_setleaf
      FROM setleaf
      FOR ALL ENTRIES IN t_coep
      WHERE valfrom EQ t_coep-valfrom
       AND  setname IN p_grpcla.

      SELECT *
      FROM ekpo INTO CORRESPONDING FIELDS OF TABLE t_ekpo
      FOR ALL ENTRIES IN t_coep
      WHERE ebeln EQ t_coep-ebeln
        AND ebelp EQ t_coep-ebelp
        AND matnr EQ t_coep-matnr
        AND werks EQ p_werks
        AND matnr IN p_matnr.

      IF t_ekpo[] IS NOT INITIAL.
        SELECT *
        FROM makt
        APPENDING CORRESPONDING FIELDS OF TABLE t_makt
        FOR ALL ENTRIES IN t_ekpo
        WHERE matnr EQ t_ekpo-matnr
          AND spras EQ 'PT'.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAIDA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_saida_dados.
  PERFORM f_lupa USING 'Preparando relatório!' space.

  LOOP AT t_coep.
    DATA: vlr_unt TYPE i.

    wa_saida-objnr  = t_coep-objnr.
    wa_saida-vrgng  = t_coep-vrgng.
    wa_saida-gjahr  = t_coep-gjahr.
    " Tirando zero a esquerda do mes.
    wa_saida-perio  = |{ t_coep-perio ALPHA = OUT }|.
    wa_saida-wtgbtr = t_coep-wtgbtr.
    wa_saida-meinb  = t_coep-meinb.
    wa_saida-zlenr  = t_coep-zlenr.
    wa_saida-matnr = |{ t_coep-matnr ALPHA = OUT }|.
    wa_saida-ebeln  = t_coep-ebeln.
    wa_saida-ebelp  = t_coep-ebelp.
    wa_saida-sgtxt  = t_coep-sgtxt.
    wa_saida-werks =  t_coep-werks.
    IF t_coep-werks IS INITIAL.
      wa_saida-werks = t_coep-gsber.
    ENDIF.

    wa_saida-bukrs =  t_coep-bukrs.

    " Tirando zero a esquerda da classes contabil.
    wa_saida-kstar = |{ t_coep-kstar ALPHA = OUT }|.

    READ TABLE t_aufk INTO wa_aufk WITH KEY objnr = t_coep-objnr.
    IF sy-subrc = 0.
      " Tirando zero a esquerda da ordem.
      wa_saida-aufnr = |{ wa_aufk-aufnr ALPHA = OUT }|.
      wa_saida-vornr = wa_aufk-vornr.
      wa_saida-ltxa1 = wa_aufk-ltxa1.
    ENDIF.

    READ TABLE t_csku WITH KEY kstar = t_coep-kstar.
    IF sy-subrc = 0.
      wa_saida-ktext = t_csku-ktext.
    ENDIF.

    READ TABLE t_setleaf WITH KEY valfrom = t_coep-valfrom.
    IF sy-subrc = 0.
      wa_saida-setname = t_setleaf-setname.
    ENDIF.

    READ TABLE t_ekpo WITH KEY ebeln = t_coep-ebeln
                               ebelp = t_coep-ebelp
                               matnr = t_coep-matnr.

    IF sy-subrc = 0.
      wa_saida-txz01 = t_ekpo-txz01.
    ENDIF.


    IF t_coep-belnr IS NOT INITIAL.
      SELECT SINGLE belnr refbn bldat budat
      FROM cobk
      INTO t_cobk
        WHERE belnr EQ t_coep-belnr.
      wa_saida-bldat   = t_cobk-bldat.
      wa_saida-budat   = t_cobk-budat.

      SELECT SINGLE mblnr bwart matnr ebelp lgort menge aufnr rsnum
      FROM mseg
        INTO t_mseg
        WHERE mblnr EQ t_cobk-refbn
          AND zeile EQ t_coep-refbz.
      IF t_mseg IS NOT INITIAL.
*        IF SY-SUBRC = 0.
        "Selecionado informações reserva material / Pedido material.

        wa_saida-refbn   = t_mseg-mblnr.
        wa_saida-lgort   = t_mseg-lgort.
        wa_saida-bwart   = t_mseg-bwart.
        " Tirando zero a esquerda do Nº da reserva.
        wa_saida-rsnum = |{ t_mseg-rsnum ALPHA = OUT }|.

        IF t_mseg-matnr IS NOT INITIAL AND t_mseg-menge IS NOT INITIAL.

*** Stefanini - IR191127 - 23/09/2024 - LAZAROSR - Início de Alteração
*          wa_saida-menge   = t_mseg-menge.
          wa_saida-menge = t_coep-mbgbtr.
*** Stefanini - IR191127 - 23/09/2024 - LAZAROSR - Fim de Alteração

          "Tratando erro (Se o valor for zero) não consegue dividir para encontrar o valor unt.
          TRY .
              wa_saida-vlr_unt =  t_coep-wtgbtr / t_mseg-menge.
*            WA_SAIDA-VLR_UNT =   T_COEP-WTGBTR / T_COEP-MBGBTR.
            CATCH cx_sy_zerodivide.
          ENDTRY.
        ELSE.
          wa_saida-menge   = t_ekpo-menge.
          TRY .
              wa_saida-vlr_unt =  t_coep-wtgbtr / t_ekpo-menge.
            CATCH cx_sy_zerodivide.
          ENDTRY.

        ENDIF.

      ENDIF.
    ENDIF.

    IF t_coep-matnr IS NOT INITIAL.
      SELECT SINGLE *
      FROM makt
      INTO CORRESPONDING FIELDS OF t_makt
        WHERE matnr EQ t_coep-matnr.
      IF t_makt IS NOT INITIAL.
        wa_saida-maktx = t_makt-maktx.
      ENDIF.
    ELSE.
      IF t_ekpo-matnr IS NOT INITIAL.
        SELECT SINGLE *
         FROM makt
         INTO CORRESPONDING FIELDS OF t_makt
           WHERE matnr EQ t_ekpo-matnr.
        IF t_makt IS NOT INITIAL.
          wa_saida-maktx = t_makt-maktx.
        ENDIF.
      ENDIF.
    ENDIF.

    "Selecionando informações equipamento e local de instalação, centro de custo.
    READ TABLE t_viaufkst WITH KEY aufnr = wa_aufk-aufnr.
    IF sy-subrc = 0.
      wa_saida-tplnr = t_viaufkst-tplnr.
      wa_saida-kostl = t_viaufkst-kostl.
      wa_saida-kostl = |{ wa_saida-kostl ALPHA = OUT }|.
      wa_saida-vaplz = t_viaufkst-vaplz.
      wa_saida-auart = t_viaufkst-auart.
      wa_saida-equnr = |{ t_viaufkst-equnr ALPHA = OUT }|.
    ENDIF.

    READ TABLE t_eqkt WITH KEY equnr = t_viaufkst-equnr.
    IF sy-subrc = 0.
      wa_saida-eqktx = t_eqkt-eqktx.
    ENDIF.

    READ TABLE t_equi WITH KEY equnr = t_viaufkst-equnr.
    IF sy-subrc = 0.
      " Tirando zero a esquerda do Equipamento.
      wa_saida-eqtyp = t_equi-eqtyp.
    ENDIF.

    IF sy-subrc = 0.
      READ TABLE it_t357 INTO wa_t357 WITH KEY beber = t_equi-beber.
      wa_saida-fing = wa_t357-fing.
    ENDIF.

    READ TABLE t_cskt WITH KEY kostl = t_viaufkst-kostl.
    IF sy-subrc = 0.
      wa_saida-t_ktext = t_cskt-ktext.
    ENDIF.

    READ TABLE t_iflo WITH KEY tplnr = t_viaufkst-tplnr.
    IF sy-subrc = 0.
      wa_saida-pltxt = t_iflo-pltxt.
    ENDIF.

    APPEND wa_saida TO t_saida.
    CLEAR wa_saida.
    CLEAR t_coep.
    CLEAR t_viaufkst.
    CLEAR t_eqkt.
    CLEAR t_afko.
    CLEAR t_afvc.
    CLEAR t_mseg.
    CLEAR t_cobk.
    CLEAR t_ekpo.
    CLEAR t_makt.
    CLEAR t_csku.
    CLEAR wa_aufk.
    CLEAR t_cskt.
    CLEAR t_setleaf.
    CLEAR t_regmseg.
    CLEAR wa_itob.
    CLEAR wa_t357.
    CLEAR t_cobk.
    CLEAR t_mseg.
*    CLEAR T_EKKN.
  ENDLOOP.

  SORT t_saida ASCENDING BY werks equnr aufnr vornr perio gjahr.

  IF p_grpcla IS NOT INITIAL AND p_grpcla-low NE '*'.
    DELETE t_saida WHERE setname NE p_grpcla-low.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_LUPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1605   text
*      -->P_SPACE  text
*----------------------------------------------------------------------*

FORM f_lupa USING p_msg1 p_msg2.
  DATA: vl_message(150) TYPE c.
  CLEAR vl_message.

  CONCATENATE p_msg1 p_msg2 INTO vl_message SEPARATED BY space.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 99
      text       = vl_message.
ENDFORM. "f_lupa



*&---------------------------------------------------------------------*
*&      Form  ZT_EXECUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zt_execut_alv .
  CALL SCREEN 0100.
ENDFORM.
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


  SET PF-STATUS 'ZT001'.
  SET TITLEBAR 'ZT002'.


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
        height = 20.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 70.

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


    IF  t_saida IS NOT INITIAL.
*      P_TEXT = TEXT-007.

      IF p_grpcla IS INITIAL.
        PERFORM fill_it_fieldcatalog USING:
          1  'WERKS '  'T_SAIDA '  '04'   ' '  ' '  ' '  'Centro            '  ''  ''  'COEP     '  '       ',
*       2  'TPLNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Local             '  ''  ''  'VIAUFKST '  '       ',
          2  'PLTXT '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.local         '  ''  ''  'IFLO     '  '       ',
          3  'EQUNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Eqpto             '  ''  ''  'VIAUFKST '  'X      ',
          4  'EQTYP '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Cat.Eqpto         '  ''  ''  'EQUI     '  '       ',
          5  'EQKTX '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.eqpto         '  ''  ''  'EQKT     '  '       ',
          6  'KOSTL '  'T_SAIDA '  '20'   ' '  ' '  ' '  'C.custo           '  ''  ''  'VIAUFKST '  '       ',
          7  'FING  '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Area Oper         '  ''  ''  'T357     '  '       ',
          8  'T_KTEXT' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.c.custo       '  ''  ''  'CSKT     '  '       ',
          9  'AUART '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Tipo de ordem     '  ''  ''  'VIAUFKST '  'X      ',
         10  'AUFNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Ordem             '  ''  ''  'VIAUFKST '  'X      ',
         11  'VORNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Operação ordem    '  ''  ''  'AFVC     '  '       ',
         12  'VAPLZ '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Cent.Trabalho     '  ''  ''  'VIAUFKST '  '       ',
         13  'LTXA1 '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.op ordem      '  ''  ''  'AFVC     '  '       ',
*       10 'OBJNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Objeto            '  ''  ''  'COEP     '  '       ',
         14 'PERIO '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Mes               '  ''  ''   'COEP    '  '       ',
*        13 'SETNAME' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Grp Cl.custo      '  ''  ''   'COEP    '  '       ',
          15 'GJAHR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Ano               '  ''  ''  'COEP     '  '       ',
          16 'KSTAR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Cl.custo          '  ''  ''  'COEP     '  '       ',
          17 'KTEXT '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Desc cl.custo     '  ''  ''  'CSKU     '  '       ',
          18 'MATNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Material          '  ''  ''  '         '  '       ',
          19 'ZLENR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Nº Item           '  ''  ''  'COEP     '  '       ',
          20 'MAKTX '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.material      '  ''  ''  'MAKT     '  '       ',
          21 'MENGE '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Quantidade        '  ''  'USER4'  'AUFK         '  '       ',
          22 'MEINB '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Und Medida        '  ''  ''  'COEP     '  '       ',
          23 'VLR_UNT' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Vlr Unt           '  ''  ''  '         '  '       ',
          24 'WTGBTR'  'T_SAIDA '  '20'   ' '  ' '  'X'  'Valor total       '  'C110'  ''  'COEP     '  '       ',
          25 'LGORT '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Depósito          '  ''  ''  'MSEG     '  '       ',
          26 'RSNUM '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Nº reserva        '  ''  ''  'MSEG     '  '       ',
          27 'EBELN  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Pedido            '  ''  ''  'COEP     '  '       ',
          28 'EBELP  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Nº item doc compra'  ''  ''  '         '  '       ',
          29 'TXZ01  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Texto Pedido      '  ''  ''  'EKPO     '  '       ',
*       27 'P_MENG ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Quantidade Pedido '  ''  ''  'EKPO     '  '       ',
*       28 'VRGNG  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Tipo transação CO '  ''  ''  'COEP     '  '       ',
          30 'SGTXT  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt Segmento      '  ''  ''  'COEP     '  '       ',
          31 'REFBN  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Nº documento      '  ''  ''  'COEP     '  '       ',
          32 'BLDAT  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Dt documento      '  ''  ''  'COBK     '  '       ',
          33 'BUDAT  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Dt Lançamento     '  ''  ''  'COBK     '  '       '.

      ELSE.
        PERFORM fill_it_fieldcatalog USING:
        1  'WERKS '  'T_SAIDA '  '04'   ' '  ' '  ' '  'Centro            '  ''  ''  'COEP     '  '       ',
*       2  'TPLNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Local             '  ''  ''  'VIAUFKST '  '       ',
        2  'PLTXT '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.local         '  ''  ''  'IFLO     '  '       ',
        3  'EQUNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Eqpto             '  ''  ''  'VIAUFKST '  'X      ',
        4  'EQTYP '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Cat.Eqpto         '  ''  ''  'EQUI '  '       ',
        5  'EQKTX '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.eqpto         '  ''  ''  'EQKT     '  '       ',
        6  'KOSTL '  'T_SAIDA '  '20'   ' '  ' '  ' '  'C.custo           '  ''  ''  'VIAUFKST '  '       ',
        7  'FING  '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Area Oper         '  ''  ''  'T357     '  '       ',
        8  'T_KTEXT' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.c.custo       '  ''  ''  'CSKT     '  '       ',
        9  'AUART '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Tipo Ordem        '  ''  ''  'VIAUFKST '  '       ',
       10  'AUFNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Ordem             '  ''  ''  'VIAUFKST '  'X      ',
       11  'VORNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Operação ordem    '  ''  ''  'AFVC     '  '       ',
       12  'VAPLZ '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Cent.Trabalho     '  ''  ''  'VIAUFKST '  '       ',
       13  'LTXA1 '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.op ordem      '  ''  ''  'AFVC     '  '       ',
*       10 'OBJNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Objeto            '  ''  ''  'COEP     '  '       ',
        14 'PERIO '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Mes               '  ''  ''   'COEP    '  '       ',
        15 'SETNAME' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Grp Cl.custo      '  ''  ''   'COEP    '  '       ',
        16 'GJAHR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Ano               '  ''  ''  'COEP     '  '       ',
        17 'KSTAR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Cl.custo          '  ''  ''  'COEP     '  '       ',
        18 'KTEXT '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Desc cl.custo     '  ''  ''  'CSKU     '  '       ',
        19 'MATNR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Material          '  ''  ''  '         '  '       ',
        20 'ZLENR '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Nº Item           '  ''  ''  'COEP     '  '       ',
        21 'MAKTX '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt.material      '  ''  ''  'MAKT     '  '       ',
        22 'MENGE '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Quantidade        '  ''  'USER4'  'AUFK         '  '       ',
        23 'MEINB '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Und Medida        '  ''  ''  'COEP     '  '       ',
        24 'VLR_UNT' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Vlr Unt           '  ''  ''  '         '  '       ',
        25 'WTGBTR'  'T_SAIDA '  '20'   ' '  ' '  'X'  'Valor total       '  'C110'  ''  'COEP     '  '       ',
        26 'LGORT '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Depósito          '  ''  ''  'MSEG     '  '       ',
        27 'RSNUM '  'T_SAIDA '  '20'   ' '  ' '  ' '  'Nº reserva        '  ''  ''  'MSEG     '  '       ',
        28 'EBELN  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Pedido            '  ''  ''  'COEP     '  '       ',
        29 'EBELP  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Nº item doc compra'  ''  ''  '         '  '       ',
        30 'TXZ01  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Texto Pedido      '  ''  ''  'EKPO     '  '       ',
*       27 'P_MENG ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Quantidade Pedido '  ''  ''  'EKPO     '  '       ',
*       28 'VRGNG  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Tipo transação CO '  ''  ''  'COEP     '  '       ',
        31 'SGTXT  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Txt Segmento      '  ''  ''  'COEP     '  '       ',
        32 'REFBN  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Nº documento      '  ''  ''  'COEP     '  '       ',
        33 'BLDAT  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Dt documento      '  ''  ''  'COBK     '  '       ',
        34 'BUDAT  ' 'T_SAIDA '  '20'   ' '  ' '  ' '  'Dt Lançamento     '  ''  ''  'COBK     '  '       '.
      ENDIF.

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
          it_outtab            = t_saida
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

*      PERFORM AJUSTA_TOTAIS.

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
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT' .
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.

      PERFORM f_lupa USING 'Atualizando informações!!' space.

      PERFORM zf_selecao.
      PERFORM zf_saida_dados.

      IF t_saida IS NOT INITIAL.
        PERFORM zt_execut_alv.

      ELSE.
        MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.


CLASS lcl_eventos IMPLEMENTATION.
  METHOD on_hotspot_click.


    READ TABLE t_saida INTO wa_saida INDEX e_row_id-index.

    CASE e_column_id-fieldname.
      WHEN:'AUFNR'."Notas Manuteção
        IF wa_saida-aufnr IS NOT INITIAL.
          SET PARAMETER ID 'ANR' FIELD wa_saida-aufnr.
          CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN .
        ENDIF.

      WHEN:'EQUNR'."Ordem Manuteção
        IF wa_saida-equnr IS NOT INITIAL.
          SET PARAMETER ID 'EQN' FIELD wa_saida-equnr.
          CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .
        ENDIF.

*    WHEN OTHERS.
    ENDCASE.




  ENDMETHOD.
ENDCLASS.


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



  wa_fieldcatalog-hotspot     = p_ref_field.


*  WA_FIELDCATALOG-EXCP_CONDS  = P_EXCP_CONDS.

  gs_layout-excp_conds    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-sel_mode      = 'A'.
  gs_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef    = ''.

  APPEND wa_fieldcatalog TO it_fieldcatalog.


ENDFORM.                    " F_DEFINE_CONTAINER_HEADER

FORM fill_it_sort .

  DATA: wa_sort TYPE lvc_s_sort.

  wa_sort-spos = '1'.
  wa_sort-fieldname = 'IWERK'.
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

  LOOP AT p_bukrs.
    IF p_bukrs-option NE 'EQ' AND p_bukrs-option NE 'BT'.
      sdydo_text_element = 'Empresa: Multiplas Seleções'.
      EXIT.
    ELSEIF p_bukrs-option EQ 'BT'.

      SELECT SINGLE butxt
        FROM t001
        INTO vl_butxt
        WHERE bukrs EQ p_bukrs-low
      AND spras EQ sy-langu.

      CONCATENATE 'Empresa:' p_bukrs-low vl_butxt '-' INTO sdydo_text_element SEPARATED BY space.
      CLEAR: vl_butxt.

      SELECT SINGLE butxt
       FROM t001
       INTO vl_butxt
       WHERE bukrs EQ p_bukrs-high
      AND spras EQ sy-langu.

      CONCATENATE sdydo_text_element p_bukrs-high vl_butxt INTO sdydo_text_element SEPARATED BY space.

      EXIT.
    ELSE.
      vl_cont = vl_cont + 1.
      IF vl_cont GT 1.
        sdydo_text_element = 'Empresa: Multiplas Seleções'.
      ELSE.

        SELECT SINGLE butxt
          FROM t001
          INTO vl_butxt
          WHERE bukrs EQ p_bukrs-low
        AND spras EQ sy-langu.

        CONCATENATE 'Empresa:' p_bukrs-low vl_butxt INTO sdydo_text_element SEPARATED BY space.

      ENDIF.
    ENDIF.
  ENDLOOP.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: vl_cont, vl_butxt, sdydo_text_element.
  "------------------
  IF p_werks IS NOT INITIAL.
    LOOP AT p_werks.
      IF p_werks-option NE 'EQ' AND p_werks-option NE 'BT'.
        sdydo_text_element = 'Centro: Multiplas Seleções'.
        EXIT.
      ELSEIF p_werks-option EQ 'BT'.
        CONCATENATE 'Centro:' p_werks-low '-' p_werks-high INTO sdydo_text_element SEPARATED BY space.
        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Centro: Multiplas Seleções'.
        ELSE.
          CONCATENATE 'Centro:' p_werks-low INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, sdydo_text_element.
  ELSE.
    sdydo_text_element = 'Centro:'.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.
  CLEAR: vl_cont, sdydo_text_element.

  "------------------

  "------------------
  IF p_ano IS NOT INITIAL.
    LOOP AT p_ano.
      IF p_ano-option NE 'EQ' AND p_ano-option NE 'BT'.
        sdydo_text_element = 'Centro: Multiplas Seleções'.
        EXIT.
      ELSEIF p_ano-option EQ 'BT'.
        CONCATENATE 'Ano:' p_ano-low '-' p_ano-high INTO sdydo_text_element SEPARATED BY space.
        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Centro: Multiplas Seleções'.
        ELSE.
          CONCATENATE 'Ano:' p_ano-low INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, sdydo_text_element.
  ELSE.
    sdydo_text_element = 'Ano:'.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.
  CLEAR: vl_cont, sdydo_text_element.

  "------------------

  IF p_perio IS NOT INITIAL.
    LOOP AT p_perio.
      IF p_perio-option NE 'EQ' AND p_perio-option NE 'BT'.
        sdydo_text_element = 'Centro: Multiplas Seleções'.
        EXIT.
      ELSEIF p_perio-option EQ 'BT'.
        CONCATENATE 'Mes:' p_perio-low 'a' p_perio-high INTO sdydo_text_element SEPARATED BY space.
        EXIT.
      ELSE.
        vl_cont = vl_cont + 1.
        IF vl_cont GT 1.
          sdydo_text_element = 'Centro: Multiplas Seleções'.
        ELSE.
          CONCATENATE 'Mes:' p_perio-low INTO sdydo_text_element SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, sdydo_text_element.
  ELSE.
    sdydo_text_element = 'Mes:'.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.
  CLEAR: vl_cont, sdydo_text_element.

  "------------------

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_VAPLZ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_select_arbpl .

  SELECT werks arbpl
  INTO TABLE it_arbpl
  FROM crhd
  WHERE werks IN p_werks.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ARBPL'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_ARBPL'
      value_org   = 'S'
    TABLES
      value_tab   = it_arbpl.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELEC_AREAOPERACIONAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selec_areaoperacional .

  SELECT *
  FROM t357
  INTO CORRESPONDING FIELDS OF TABLE it_t357
  WHERE werks IN p_werks.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'BEBER'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_BEBER'
      value_org   = 'S'
    TABLES
      value_tab   = it_t357.

ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  Z_SEL_DADOS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM Z_SEL_DADOS .
*
*
*
*
*
*
*
*
*
*
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  Z_SEL_ORD_LIB
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM Z_SEL_ORD_LIB .
*
*  SELECT A~BUKRS A~WERKS A~AUART A~AUFNR A~KOSTL A~EQUNR A~TPLNR A~OBJNR A~VAPLZ C~PERIO C~MATNR C~KSTAR C~MEGBTR C~BELNR C~WTGBTR
*  FROM VIAUFKST AS A
*  INNER JOIN COEP AS C ON C~OBJNR EQ A~OBJNR
*     INTO CORRESPONDING FIELDS OF TABLE ZCOEP
*  WHERE A~WERKS IN P_WERKS
*    AND A~BUKRS IN P_BUKRS
*    AND A~AUART IN P_AUART
*    AND A~AUFNR IN P_AUFNR
*    AND A~KOSTL IN P_KOSTL
*    AND A~EQUNR IN P_EQUNR
*    AND A~TPLNR IN P_TPLNR
*    AND A~VAPLZ IN P_ARBPL
*    AND C~GJAHR IN P_ANO
*    AND C~PERIO IN P_PERIO
*    AND C~MATNR IN P_MATNR
*    AND C~KSTAR IN P_KSTAR
*    AND C~VRGNG EQ 'COIN'
*    AND A~AUTYP EQ '30'.
*
*  SELECT A~BUKRS A~WERKS A~AUART A~AUFNR A~KOSTL A~EQUNR A~TPLNR A~OBJNR A~VAPLZ C~PERIO C~MATNR C~KSTAR C~MEGBTR C~BELNR C~WTGBTR
*  FROM VIAUFKST AS A
*  INNER JOIN AFVC AS B ON B~AUFPL EQ A~AUFPL
*  INNER JOIN COEP AS C ON C~OBJNR EQ B~OBJNR
*     INTO CORRESPONDING FIELDS OF TABLE T_AUFK
*  WHERE A~WERKS IN P_WERKS
*    AND A~BUKRS IN P_BUKRS
*    AND A~AUART IN P_AUART
*    AND A~AUFNR IN P_AUFNR
*    AND A~KOSTL IN P_KOSTL
*    AND A~EQUNR IN P_EQUNR
*    AND A~TPLNR IN P_TPLNR
*    AND A~VAPLZ IN P_ARBPL
*    AND C~GJAHR IN P_ANO
*    AND C~PERIO IN P_PERIO
*    AND C~MATNR IN P_MATNR
*    AND C~KSTAR IN P_KSTAR
*    AND C~VRGNG EQ 'COIN'
*    AND A~AUTYP EQ '30'.
*
*
*  CHECK ZCOEP IS NOT INITIAL.
*  LOOP AT ZCOEP ASSIGNING FIELD-SYMBOL(<AUFK>).
*    IF LINE_EXISTS( T_AUFK[ OBJNR = <AUFK>-OBJNR ] ).
*      CLEAR <AUFK>.
*    ELSE.
*      APPEND <AUFK> TO T_AUFK.
*    ENDIF.
*  ENDLOOP.
*  SORT T_AUFK BY AUFNR.
*
*  CHECK T_AUFK IS NOT INITIAL.
*  LOOP AT T_AUFK ASSIGNING FIELD-SYMBOL(<_AUFK>).
*
*    CALL FUNCTION 'STATUS_READ'
*      EXPORTING
*        CLIENT      = SY-MANDT
*        OBJNR       = <_AUFK>-OBJNR
*        ONLY_ACTIVE = 'X'
*      TABLES
*        STATUS      = IJSTAT.
*
*    <_AUFK>-STATUS = IJSTAT-STAT.
*    SELECT SINGLE *
*    FROM TJ02T
*    INTO @DATA(TJ02T)
*     WHERE ISTAT = @<_AUFK>-STATUS
*      AND SPRAS = 'PT'.
*    <_AUFK>-D_STATUS = TJ02T-TXT04.
*    CLEAR TJ02T.
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  Z_SEL_ORD_ENC
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM Z_SEL_ORD_ENC .
*
*ENDFORM.
