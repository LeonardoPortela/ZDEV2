* A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 13.08.2012                                          *
* Objetivo    ...: Consulta Classificação Kuhlman                      *
* Transação   ...: ZMM0036                                             *
************************************************************************
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& Igor Sobral                  21.05.2013   Add - Busca de Historico &*
*&--------------------------------------------------------------------&*

*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& Anderson Oenning      27.03..2018   Add - analise status min e max &*
*&--------------------------------------------------------------------&*

REPORT  zmmr0024 MESSAGE-ID zpmmsg.

*----------------------------------------------------------------------*
* TYPE-POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: vrm.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ausp, cabn, mchb, mara, chvw,       "STANDARD
        ztsafrafardos, zmmt0025, zmmt0027.


*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF ty_mchb,
    matnr TYPE mchb-matnr,
    werks TYPE mchb-werks,
    lgort TYPE mchb-lgort,
    charg TYPE mchb-charg,
    clabs TYPE mchb-clabs,
    cspem TYPE mchb-cspem,
    ersda TYPE mchb-ersda,    "ADD - IS - 21.05.2013
    lfgja TYPE mchb-lfgja,
  END OF ty_mchb,

  BEGIN OF ty_mch1,
    matnr        TYPE mch1-matnr,
    charg        TYPE mch1-charg,
    cuobj_bm     TYPE mch1-cuobj_bm,
    ersda        TYPE mch1-ersda,
    cuobj_bm_aux TYPE ausp-objek,
  END OF ty_mch1,

  BEGIN OF ty_ausp,
    objek     TYPE ausp-objek,
    atinn     TYPE ausp-atinn,
    atwrt     TYPE ausp-atwrt,
    atflv     TYPE ausp-atflv,
    objek_aux TYPE mch1-cuobj_bm,
    charg     TYPE ztsafrafardos-charg,
  END OF ty_ausp,

  BEGIN OF ty_mara,
    matnr TYPE mara-matnr,
    normt TYPE mara-normt,
    matkl TYPE mara-matkl,
  END OF ty_mara,

  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  BEGIN OF ty_zmmt0025,
    atinn TYPE zmmt0025-atinn,
    atnam TYPE zmmt0025-atnam,
  END OF ty_zmmt0025,

  BEGIN OF ty_ztsafrafardos,
    charg       TYPE ztsafrafardos-charg,
    data_inicio TYPE ztsafrafardos-data_inicio,
    data_fim    TYPE ztsafrafardos-data_fim,
    status      TYPE ztsafrafardos-status,
    werks_key   TYPE ztsafrafardos-werks_key,
    werks_from  TYPE ztsafrafardos-werks_from,
    werks_to    TYPE ztsafrafardos-werks_to,
  END OF ty_ztsafrafardos,

  BEGIN OF ty_zppt0014_aux,
    include   TYPE zmmt0027,
    clint     TYPE zppt0014-clint,
    class     TYPE zppt0014-class,
    atinn     TYPE zppt0014-atinn,
    atnam     TYPE zppt0014-atnam,
    klart     TYPE zppt0014-klart,
    valor_de  TYPE zppt0014-valor_de,
    valor_ate TYPE zppt0014-valor_ate,
  END OF ty_zppt0014_aux,

  BEGIN OF ty_mseg,
    mblnr TYPE mseg-mblnr,
    mjahr TYPE mseg-mjahr,
    zeile TYPE mseg-zeile,
    bwart TYPE mseg-bwart,
    matnr TYPE mseg-matnr,
    werks TYPE mseg-werks,
    lgort TYPE mseg-lgort,
    charg TYPE mseg-charg,
    smbln TYPE mseg-smbln,
    menge TYPE mseg-menge,
    budat TYPE mkpf-budat,
    check TYPE c,
  END OF ty_mseg,

  BEGIN OF ty_saida,
    werks             TYPE mchb-werks,
    znumer            TYPE char10,
*    WERKS_KEY         TYPE ZTSAFRAFARDOS-WERKS_KEY,
    normt             TYPE mara-normt,
    charg1            TYPE zppt0002-cd_sai,
    charg             TYPE mchb-charg,
    clabs             TYPE mchb-cspem,
    lgort             TYPE mchb-lgort,
    matnr             TYPE mara-matnr,
    maktx             TYPE makt-maktx,
    uhml              TYPE ausp-atwrt,
    vlr_min_uhml      TYPE ausp-atwrt,
    vlr_max_uhml      TYPE ausp-atwrt,
    st_uhml(20)       TYPE c,
    ui                TYPE ausp-atwrt,
    vlr_min_ui        TYPE ausp-atwrt,
    vlr_max_ui        TYPE ausp-atwrt,
    st_ui(20)         TYPE c,
    str               TYPE ausp-atwrt,
    vlr_min_str       TYPE ausp-atwrt,
    vlr_max_str       TYPE ausp-atwrt,
    st_str(20)        TYPE c,
    elg               TYPE ausp-atwrt,
    vlr_min_elg       TYPE ausp-atwrt,
    vlr_max_elg       TYPE ausp-atwrt,
    st_elg(20)        TYPE c,
    mic               TYPE ausp-atwrt,
    vlr_min_mic       TYPE ausp-atwrt,
    vlr_max_mic       TYPE ausp-atwrt,
    st_mic(20)        TYPE c,
    rd                TYPE ausp-atwrt,
    vlr_min_rd        TYPE ausp-atwrt,
    vlr_max_rd        TYPE ausp-atwrt,
    st_rd(20)         TYPE c,
    _b                TYPE ausp-atwrt,
    vlr_min__b        TYPE ausp-atwrt,
    vlr_max__b        TYPE ausp-atwrt,
    st__b(20)         TYPE c,
    cg                TYPE ausp-atwrt,
    vlr_min_cg        TYPE ausp-atwrt,
    vlr_max_cg        TYPE ausp-atwrt,
    st_cg(20)         TYPE c,
    t_cnt             TYPE ausp-atwrt,
    vlr_min_t_cnt     TYPE ausp-atwrt,
    vlr_max_t_cnt     TYPE ausp-atwrt,
    st_t_cnt(20)      TYPE c,
    t_area            TYPE ausp-atwrt,
    vlr_min_t_area    TYPE ausp-atwrt,
    vlr_max_t_area    TYPE ausp-atwrt,
    st_t_area(20)     TYPE c,
    leaf              TYPE ausp-atwrt,
    vlr_min_leaf      TYPE ausp-atwrt,
    vlr_max_leaf      TYPE ausp-atwrt,
    st_leaf(20)       TYPE c,
    mr                TYPE ausp-atwrt,
    vlr_min_mr        TYPE ausp-atwrt,
    vlr_max_mr        TYPE ausp-atwrt,
    st_mr(20)         TYPE c,
    sfi               TYPE ausp-atwrt,
    vlr_min_sfi       TYPE ausp-atwrt,
    vlr_max_sfi       TYPE ausp-atwrt,
    st_sfi(20)        TYPE c,
    sci               TYPE ausp-atwrt,
    vlr_min_sci       TYPE ausp-atwrt,
    vlr_max_sci       TYPE ausp-atwrt,
    st_sci(20)        TYPE c,
    csp               TYPE ausp-atwrt,
    vlr_min_csp       TYPE ausp-atwrt,
    vlr_max_csp       TYPE ausp-atwrt,
    st_csp(20)        TYPE c,
    fardao            TYPE ausp-atwrt,
    vlr_min_fardao    TYPE ausp-atwrt,
    vlr_max_fardao    TYPE ausp-atwrt,
    st_fardao(20)     TYPE c,
    talhao            TYPE ausp-atwrt,
    vlr_min_talhao    TYPE ausp-atwrt,
    vlr_max_talhao    TYPE ausp-atwrt,
    st_talhao(20)     TYPE c,
    variedade         TYPE ausp-atwrt,
    vlr_min_variedade TYPE ausp-atwrt,
    vlr_max_variedade TYPE ausp-atwrt,
    st_variedade(20)  TYPE c,
    reserv            TYPE c LENGTH 20,
* ADD - IS - 21.05.2013
    ersda             TYPE mchb-ersda,
    safra             TYPE ausp-atwrt,
    vlr_min_safra     TYPE ausp-atwrt,
    vlr_max_safra     TYPE ausp-atwrt,
    st_safra(20)      TYPE c,
    periodo           TYPE ausp-atwrt,
    vlr_min_periodo   TYPE ausp-atwrt,
    vlr_max_periodo   TYPE ausp-atwrt,
    st_periodo(20)    TYPE c,
    status            TYPE c LENGTH 10,
    werks_orig        TYPE mchb-werks,
* ADD - IS - 21.05.2013
  END OF ty_saida,

  BEGIN OF ty_saida_resu,
    normt      TYPE mara-normt,
    matnr      TYPE mara-matnr,
    clabs      TYPE mchb-cspem,
    qtd_f      TYPE sy-tabix,
    werks_orig TYPE mchb-werks,
  END OF ty_saida_resu.

TYPES: BEGIN OF ty_zmme_cl.
         INCLUDE TYPE zmme_cl.
TYPES: check TYPE c,
       END OF ty_zmme_cl.

TYPES: BEGIN OF ty_caracts.
         INCLUDE TYPE zmmt0027.
TYPES: atnam TYPE c,
       END OF ty_caracts.

DATA t_class TYPE RANGE OF zmmt0025-atnam WITH HEADER LINE.


*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA: it_mchb             TYPE TABLE OF ty_mchb,
      it_mchb_aux         TYPE TABLE OF ty_mchb,
      it_mch1             TYPE TABLE OF ty_mch1,
      it_mch1_aux         TYPE TABLE OF ty_mch1,
      it_mch1_obj         TYPE TABLE OF ty_mch1,
      it_ausp             TYPE TABLE OF ty_ausp,
      it_ausp_class       TYPE TABLE OF ty_ausp,
      it_mara             TYPE TABLE OF ty_mara,
      it_zmmt0025         TYPE TABLE OF ty_zmmt0025,
      it_saida            TYPE TABLE OF ty_saida,
      it_saida_aux        TYPE TABLE OF ty_saida,
      it_saida_resu       TYPE TABLE OF ty_saida_resu,
      it_matnr            TYPE TABLE OF zmme_cl,
      it_return           TYPE TABLE OF ty_zmme_cl,
      it_return_aux       TYPE TABLE OF zmme_cl,
      it_zmmt0027         TYPE TABLE OF zmmt0027,
      it_zmmt0008         TYPE TABLE OF zmmt0008,
      it_zmmt0027_aux     TYPE TABLE OF zmmt0027,
      it_zppt0014         TYPE TABLE OF zppt0014,
      it_zppt0014_aux     TYPE TABLE OF ty_zppt0014_aux,
      it_zppt0002         TYPE TABLE OF zppt0002,  "ADD - IS - 12.06.2013
      it_zmmt0025_        TYPE TABLE OF zmmt0025,
      it_mseg             TYPE STANDARD TABLE OF ty_mseg,
      it_v_mkm_zu_kls_bez TYPE TABLE OF v_mkm_zu_kls_bez,
      wa_v_mkm_zu_kls_bez TYPE v_mkm_zu_kls_bez,
      it_caracts          TYPE TABLE OF ty_caracts,
      wa_caracts          TYPE ty_caracts,
      it_ztsafrafardos    TYPE TABLE OF ty_ztsafrafardos.



*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_mchb          TYPE ty_mchb,
      wa_mchb_aux      TYPE ty_mchb,
      wa_mch1          TYPE ty_mch1,
      wa_mch1_aux      TYPE ty_mch1,
      wa_ausp          TYPE ty_ausp,
      wa_ausp_class    TYPE ty_ausp,
      wa_mara          TYPE ty_mara,
      wa_zmmt0025      TYPE ty_zmmt0025,
      wa_saida         TYPE ty_saida,
      wa_saida_aux     TYPE ty_saida,
      wa_saida_resu    TYPE ty_saida_resu,
      wa_return        TYPE zmme_cl,
      wa_matnr         TYPE zmme_cl,
      wa_zmmt0027      TYPE zmmt0027,
      wa_zmmt0027_aux  TYPE zmmt0027,
      wa_zppt0014      TYPE zppt0014,
      wa_zppt0014_aux  TYPE ty_zppt0014_aux,
      wa_zppt0002      TYPE zppt0002, "ADD - IS - 12.06.2013
      wa_zmmt0008      TYPE zmmt0008,
      wa_mseg          TYPE ty_mseg,
      wa_ztsafrafardos TYPE ztsafrafardos,
      wa_ztfardos      TYPE ty_ztsafrafardos.

*----------------------------------------------------------------------*
* ListBox
*----------------------------------------------------------------------*
DATA: name  TYPE vrm_id,
      lista TYPE vrm_values,
      value LIKE LINE OF lista.
*----------------------------------------------------------------------*
* ALV
*----------------------------------------------------------------------*
DATA: cl_container TYPE REF TO cl_gui_custom_container,
      cl_grid      TYPE REF TO cl_gui_alv_grid,
      ls_layout    TYPE lvc_s_layo,
      ls_fcat      TYPE TABLE OF lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo.


*----------------------------------------------------------------------*
* SELECT-PARAMETERS
*----------------------------------------------------------------------*


SELECTION-SCREEN: BEGIN OF BLOCK bloco_03 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN BEGIN OF LINE .


    SELECTION-SCREEN POSITION 01.
    SELECTION-SCREEN COMMENT 1(17) TEXT-011.
    PARAMETERS: todos_mm TYPE char1  RADIOBUTTON GROUP rb02 MODIF ID mm DEFAULT 'X' USER-COMMAND uc1.

    SELECTION-SCREEN POSITION 05.
    SELECTION-SCREEN COMMENT 30(15) TEXT-012.
    PARAMETERS: hist_mm  TYPE char1  RADIOBUTTON GROUP rb02 MODIF ID mm.

    SELECTION-SCREEN POSITION 10.
    SELECTION-SCREEN COMMENT 58(07) TEXT-007.
    PARAMETERS: resu_mm  AS CHECKBOX.

*              resu_mm     TYPE char1  RADIOBUTTON GROUP rb02 MODIF ID mm,

  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK bloco_03.


SELECTION-SCREEN: BEGIN OF BLOCK bloco_01 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
              p_werks FOR mchb-werks          MODIF ID c1, "OBLIGATORY,
              p_matnr FOR mchb-matnr          MODIF ID c1,
              p_matkl FOR mara-matkl          NO INTERVALS MODIF ID c1 NO-EXTENSION,
              p_lgort FOR mchb-lgort          NO INTERVALS MODIF ID c1,
              p_safra FOR ztsafrafardos-charg NO INTERVALS MODIF ID c1 NO-EXTENSION, " OBLIGATORY,
              p_wkori FOR mchb-werks          NO INTERVALS MODIF ID c2.  "*-CS2022000332-#78803-05.07.2022-JT-inicio

*---> CS1116730 . IR143996 --->
  PARAMETERS: p_benef  AS CHECKBOX.
*<--- CS1116730 . IR143996 <---

*-CS2022000332-#82292-26.07.2022-JT-inicio
  SELECTION-SCREEN BEGIN OF LINE .
    SELECTION-SCREEN POSITION 01.
    PARAMETERS: p_fnint  AS CHECKBOX                         MODIF ID c2.
    SELECTION-SCREEN COMMENT 3(50) TEXT-020                  MODIF ID c2.
  SELECTION-SCREEN END OF LINE.
*-CS2022000332-#82292-26.07.2022-JT-fim

SELECTION-SCREEN: END OF BLOCK bloco_01.

SELECTION-SCREEN: BEGIN OF BLOCK bloco_02 WITH FRAME TITLE TEXT-002.
*PARAMETERS:
*            P_CLASS(10) AS LISTBOX VISIBLE LENGTH 10.
  SELECT-OPTIONS:
              p_hvi FOR ausp-atwrt NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK bloco_02.


SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-013.
  SELECTION-SCREEN BEGIN OF LINE .

    SELECTION-SCREEN POSITION 01.
    SELECTION-SCREEN COMMENT 01(18) TEXT-006.
    PARAMETERS: p_fpdr AS CHECKBOX. "DEFAULT 'X'.

    SELECTION-SCREEN POSITION 11.
    SELECTION-SCREEN COMMENT 36(20) TEXT-008.
    PARAMETERS: p_dpdr AS CHECKBOX. "DEFAULT 'X'.

  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b6.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
*-CS2022000332-#78803-05.07.2022-JT-inicio
AT SELECTION-SCREEN.

  IF p_werks[] IS INITIAL OR p_safra[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Preencher todos os campos obrigatórios' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
*-CS2022000332-#78803-05.07.2022-JT-fim

*-CS2022000332-#82292-26.07.2022-JT-inicio
  IF hist_mm = abap_false.
    p_fnint = abap_false.
  ENDIF.
*-CS2022000332-#82292-26.07.2022-JT-fim

AT SELECTION-SCREEN OUTPUT.

*-CS2022000332-#78803-05.07.2022-JT-inicio
  LOOP AT SCREEN.
    IF screen-group1 = 'C2'.
      IF hist_mm = abap_true.
        screen-output = 1.
        screen-active = 1.
      ELSE.
        screen-output = 0.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name(11) = 'P_WERKS-LOW' OR
       screen-name(11) = 'P_SAFRA-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*-CS2022000332-#78803-05.07.2022-JT-fim

  SELECT atinn atnam
  FROM zmmt0025
  INTO TABLE it_zmmt0025.

  IF it_zmmt0025[] IS NOT INITIAL.
    SORT it_zmmt0025 BY atinn.

    LOOP AT it_zmmt0025 INTO wa_zmmt0025 WHERE atnam NE 'SAFRA'.
      value-key  = wa_zmmt0025-atinn.
      value-text = wa_zmmt0025-atnam.
      APPEND value TO lista.
    ENDLOOP.

    name = 'P_CLASS'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = lista.
  ENDIF.

  "pesquisa caracteristica da tabela zmmt0025.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_hvi-low.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ATNAM'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_HVI'
      value_org   = 'S'
    TABLES
      value_tab   = it_zmmt0025.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.


  IF p_fpdr IS NOT INITIAL AND p_dpdr IS NOT INITIAL.

    MESSAGE TEXT-050 TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.

    IF p_fpdr IS NOT INITIAL AND p_hvi IS INITIAL OR p_dpdr IS NOT INITIAL AND p_hvi IS INITIAL.
      MESSAGE TEXT-051 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      IF p_matnr IS INITIAL AND p_matkl IS INITIAL .
        MESSAGE i000(z01) WITH 'É Obrigatório informar material' 'ou grupo de material'.
      ELSE.
        PERFORM: seleciona_dados.
        CALL SCREEN 0100.
      ENDIF.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados.

  FREE: it_saida.

  IF ( hist_mm EQ 'X').

    PERFORM: selecao_zmmt0027.

  ELSE.

**  DATA: idx TYPE sy-tabix.
    FREE: it_mara.
    SELECT matnr normt matkl
    FROM mara
      INTO TABLE it_mara
    WHERE matnr IN p_matnr
    AND matkl IN p_matkl.

    IF p_benef IS INITIAL.
      SELECT *
      FROM ztsafrafardos
      INTO CORRESPONDING FIELDS OF TABLE it_ztsafrafardos
      WHERE charg IN p_safra
        AND werks_from IN p_werks.

      SELECT SINGLE * FROM ztsafrafardos
        INTO wa_ztsafrafardos
      WHERE charg  IN p_safra
        AND werks_from  IN p_werks
        AND status EQ 'L'.
    ELSE.
*---> CS1116730 . IR143996 --->
      SELECT * FROM ztsafrafardos
        INTO CORRESPONDING FIELDS OF TABLE it_ztsafrafardos
      WHERE charg IN p_safra
       AND werks_to IN p_werks.

      SELECT SINGLE * FROM ztsafrafardos
        INTO wa_ztsafrafardos
      WHERE charg  IN p_safra
        AND werks_to  IN p_werks
        AND status EQ 'L'.
*<--- CS1116730 . IR143996 <---
    ENDIF.


    IF ( p_matnr IS INITIAL ).

      FREE: it_mchb.
      SELECT  * "MATNR WERKS LGORT CHARG CLABS CSPEM
              "ERSDA   "ADD - IS - 21.05.2013
        FROM mchb
        INTO CORRESPONDING FIELDS OF TABLE it_mchb
        FOR ALL ENTRIES IN it_mara
        WHERE matnr EQ it_mara-matnr
        AND werks IN p_werks
        AND lgort IN p_lgort
        AND ( clabs > 0 OR cspem > 0 )
        AND ersda BETWEEN wa_ztsafrafardos-data_inicio AND wa_ztsafrafardos-data_fim.   "ADD - IS - 21.05.2013



      CLEAR: wa_mchb.

      DATA: len TYPE sy-tabix.

      LOOP AT it_mchb INTO wa_mchb.
        len = strlen( wa_mchb-charg ).

        IF ( len <= 4 ).
          DELETE it_mchb WHERE charg EQ wa_mchb-charg.
        ENDIF.

        CLEAR: wa_mchb, len.
      ENDLOOP.

    ELSE.
      SELECT  matnr werks lgort charg clabs cspem
              ersda   "ADD - IS - 21.05.2013
      FROM mchb
        INTO TABLE it_mchb
      WHERE matnr IN p_matnr
        AND werks IN p_werks
        AND lgort IN p_lgort
        AND ( clabs > 0 OR cspem > 0 )
      AND ersda BETWEEN wa_ztsafrafardos-data_inicio AND wa_ztsafrafardos-data_fim.   "ADD - IS - 21.05.2013

    ENDIF.

    CHECK NOT it_mchb[] IS INITIAL.

    PERFORM: selecao_ausp.
  ENDIF.
ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  SELECAO_AUSP
*&---------------------------------------------------------------------*
FORM selecao_ausp.
  LOOP AT it_mchb INTO wa_mchb.
    MOVE: wa_mchb-matnr TO wa_matnr-matnr,
          wa_mchb-charg TO wa_matnr-charg.
    APPEND wa_matnr TO it_matnr.
  ENDLOOP.

  IF it_matnr IS NOT INITIAL.
    CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
      TABLES
        t_matnr  = it_matnr
        t_return = it_return
      EXCEPTIONS          "*-CS2022000332-#82292-26.07.2022-JT-inicio
        erro4    = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
    ELSE.
      SORT it_zmmt0025 BY atnam.
      READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'SAFRA' BINARY SEARCH.


      IF sy-subrc = 0.
        LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
                                           AND atwrt NE p_safra-low.
          APPEND wa_return TO it_return_aux.
        ENDLOOP.
        " Deletar com SAFRA diferente da informada
        IF it_return_aux[] IS NOT INITIAL.
          CLEAR: wa_return.
          LOOP AT it_return_aux INTO wa_return.
            DELETE it_return WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
            DELETE it_mchb   WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF p_hvi-low IS NOT INITIAL AND p_hvi-high IS NOT INITIAL." AND P_CLASS IS NOT INITIAL.
        CLEAR: wa_return, it_return_aux[].
        LOOP AT it_return INTO wa_return WHERE atwrt NOT BETWEEN p_hvi-low AND p_hvi-high.
*                                           AND ATINN EQ P_CLASS

          APPEND wa_return TO it_return_aux.
        ENDLOOP.
        "Deletar quem estiver fora do range informado
        IF it_return_aux[] IS NOT INITIAL.
          CLEAR: wa_return.
          LOOP AT it_return_aux INTO wa_return.
            DELETE it_return WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
            DELETE it_mchb   WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

** ADD - IS - 12.06.2013 - Inicio
** SELEÇÕES FARDO DE ORIGEM
  FREE: it_zppt0002.
  IF it_mchb[] IS NOT INITIAL.
    SELECT * FROM zppt0002
      INTO TABLE it_zppt0002
      FOR ALL ENTRIES IN it_mchb
    WHERE acharg  EQ it_mchb-charg
    AND werks   EQ it_mchb-werks.
*    AND matnr   EQ it_mchb-matnr.
  ENDIF.

  SORT it_zppt0002 BY matnr werks acharg.
** ADD - IS - 12.06.2013 - Fim

  CHECK NOT it_return[] IS INITIAL.
  SELECT *
  FROM zppt0014
  INTO TABLE it_zppt0014
  FOR ALL ENTRIES IN it_return
  WHERE atinn EQ it_return-atinn.

  PERFORM: organiza_saida.

ENDFORM.                    " SELECAO_AUSP

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_SAIDA
*&---------------------------------------------------------------------*
FORM organiza_saida.

  DATA: wa_zmmt0025      TYPE zmmt0025,
        qtd              TYPE sy-tabix,
        v_acharg(22),
        v_acharg_ant(22),
        vl_pos           TYPE i,
        vl_tam           TYPE i,
        vl_pdr           TYPE p DECIMALS 2.

**  CLEAR: IT_MCH1_AUX[].

  SORT: "IT_MCH1     BY MATNR,
        it_mchb     BY matnr charg,
        it_ausp     BY objek_aux,
        it_zmmt0025 BY atinn,
        it_zmmt0027 BY matnr matkl,
        it_mara     BY matnr normt,
        it_return   BY matnr charg atinn.


  IF ( resu_mm EQ 'X' ).
    IF ( todos_mm EQ 'X' ).
      REFRESH: it_mchb_aux[].

      it_mchb_aux[] = it_mchb[].

      LOOP AT it_mchb INTO wa_mchb.
        LOOP AT it_mchb_aux INTO wa_mchb_aux WHERE matnr EQ wa_mchb-matnr.

          IF NOT ( wa_mchb_aux-cspem IS INITIAL ).
            wa_saida_resu-clabs = wa_saida_resu-clabs + wa_mchb_aux-cspem.
            CLEAR: wa_mchb_aux-cspem.
          ELSEIF NOT ( wa_mchb_aux-clabs IS INITIAL ).
            wa_saida_resu-clabs = wa_saida_resu-clabs + wa_mchb_aux-clabs.
            CLEAR: wa_mchb_aux-clabs.
          ENDIF.

          qtd = qtd + 1.
        ENDLOOP.

        wa_saida_resu-qtd_f = qtd.

        READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mchb-matnr BINARY SEARCH.
        wa_saida_resu-normt = wa_mara-normt.
        wa_saida_resu-matnr = wa_mara-matnr.

        APPEND wa_saida_resu TO it_saida_resu.
        DELETE it_mchb WHERE matnr EQ wa_mara-matnr.

        CLEAR: wa_mch1_aux, wa_mch1, wa_saida_resu, wa_mara, qtd.
**      ENDIF.
      ENDLOOP.

    ELSEIF ( hist_mm EQ 'X' ).
      it_zmmt0027_aux[] = it_zmmt0027[].

      LOOP AT it_zmmt0027 INTO wa_zmmt0027.
        LOOP AT it_zmmt0027_aux INTO wa_zmmt0027_aux WHERE matnr EQ wa_zmmt0027-matnr.
          wa_saida_resu-clabs = wa_saida_resu-clabs + wa_zmmt0027_aux-menge.

          qtd = qtd + 1.
        ENDLOOP.

*-CS2022000332-#78803-05.07.2022-JT-inicio
        CLEAR wa_zmmt0008.
        READ TABLE it_zmmt0008 INTO wa_zmmt0008 WITH KEY lgort      = wa_zmmt0027-lgort
                                                         charg      = wa_zmmt0027-charg
                                                         werks_orig = wa_zmmt0027-werks
                                              BINARY SEARCH.
*-CS2022000332-#78803-05.07.2022-JT-fim

        wa_saida_resu-qtd_f      = qtd.
        wa_saida_resu-normt      = wa_zmmt0027-normt.
        wa_saida_resu-matnr      = wa_zmmt0027-matnr.
        wa_saida_resu-werks_orig = wa_zmmt0008-werks.

        APPEND wa_saida_resu TO it_saida_resu.
        DELETE it_zmmt0027 WHERE matnr EQ wa_zmmt0027-matnr.

        CLEAR: wa_zmmt0027, wa_zmmt0027_aux, wa_saida_resu, qtd.
      ENDLOOP.
    ENDIF.
  ELSE.
    IF ( todos_mm EQ 'X' ).

      CLEAR: v_acharg, v_acharg_ant, wa_saida.
      LOOP AT it_return INTO wa_return.
        CLEAR wa_ztsafrafardos.
*        WA_SAIDA-ZNUMER = WA_RETURN-CHARG.
*        WA_SAIDA-ZNUMER = WA_SAIDA-ZNUMER(3).

        CONCATENATE wa_return-charg wa_return-matnr INTO v_acharg.
        IF v_acharg NE v_acharg_ant AND v_acharg_ant IS NOT INITIAL.
          IF NOT ( wa_saida-charg IS INITIAL AND wa_saida-lgort IS INITIAL AND wa_saida-matnr IS INITIAL ).
            APPEND wa_saida TO it_saida.
          ENDIF.
          CLEAR: wa_mch1, wa_mchb, wa_saida, wa_zppt0014.
        ENDIF.

        IF v_acharg NE v_acharg_ant.
          READ TABLE it_mchb INTO wa_mchb  WITH KEY matnr = wa_return-matnr
                                                    charg = wa_return-charg BINARY SEARCH.
          SEARCH wa_mchb-charg FOR './.'.
          vl_pos = sy-fdpos + 1.
          vl_tam = 10 - vl_pos.

          IF wa_mchb-charg IS NOT INITIAL.
*            WA_SAIDA-ZNUMER = WA_ZTFARDOS-WERKS_KEY.
            wa_saida-znumer = |{ wa_mchb-charg(3) }|.
          ENDIF.

          wa_saida-charg      = wa_mchb-charg+vl_pos(vl_tam).
          wa_saida-lgort      = wa_mchb-lgort.
          wa_saida-ersda      = wa_mchb-ersda.
          wa_saida-werks      = wa_mchb-werks.

*          READ TABLE IT_ZTSAFRAFARDOS INTO WA_ZTFARDOS WITH KEY WERKS_FROM  = WA_MCHB-WERKS
*                                                                     CHARG  = P_SAFRA-LOW.
*          IF SY-SUBRC = 0.

          CLEAR wa_mara.
          READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mchb-matnr BINARY SEARCH.
          wa_saida-matnr      = wa_mara-matnr.
          wa_saida-normt      = wa_mara-normt.

          IF wa_mchb-clabs = 0 AND wa_mchb-cspem = 0.
            wa_saida-status   = 'EMBARCADO'.
          ELSEIF wa_mchb-cspem > 0.
            wa_saida-status   = 'RESERVADO'.
            wa_saida-clabs    = wa_mchb-cspem.
          ELSEIF wa_mchb-clabs > 0.
            wa_saida-status   = 'DISPONÍVEL'.
            wa_saida-clabs    = wa_mchb-clabs.
          ELSEIF wa_mchb-clabs > 0 AND wa_mchb-cspem > 0.
            wa_saida-status   = 'VERIFICAR'.
          ENDIF.

          CLEAR wa_zppt0002.
          READ TABLE it_zppt0002 INTO wa_zppt0002 WITH KEY acharg = wa_mchb-charg
                                                           werks  = wa_mchb-werks.  "BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida-fardao = wa_zppt0002-charg.
            wa_saida-charg1      = wa_zppt0002-cd_sai.
          ENDIF.
        ENDIF.

        CLEAR: wa_zmmt0025, wa_zppt0014.

        READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atinn = wa_return-atinn BINARY SEARCH.   "ADD - IS - 21.05.2013

        READ TABLE it_zppt0014 INTO wa_zppt0014 WITH KEY atinn = wa_return-atinn.

        CASE wa_zmmt0025-atnam.
          WHEN: 'UHML'.
*                  wa_saida-uhml      = wa_ausp-atwrt.
            wa_saida-uhml      = wa_return-atwrt.
            wa_saida-vlr_min_uhml = wa_zppt0014-valor_de.
            wa_saida-vlr_max_uhml = wa_zppt0014-valor_ate.
            IF wa_saida-uhml < wa_zppt0014-valor_de OR wa_saida-uhml > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_uhml = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_uhml = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'UI'.
*                  wa_saida-ui        = wa_ausp-atwrt.
            wa_saida-ui        = wa_return-atwrt.
            wa_saida-vlr_min_ui = wa_zppt0014-valor_de.
            wa_saida-vlr_max_ui = wa_zppt0014-valor_ate.
            IF wa_saida-ui < wa_zppt0014-valor_de OR wa_saida-ui > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_ui = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_ui = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'STR'.
*                  wa_saida-str       = wa_ausp-atwrt.
            wa_saida-str       = wa_return-atwrt.
            wa_saida-vlr_min_str = wa_zppt0014-valor_de.
            wa_saida-vlr_max_str = wa_zppt0014-valor_ate.
            IF wa_saida-str < wa_zppt0014-valor_de OR wa_saida-str > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_str = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_str = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'ELG'.
*                  wa_saida-elg       = wa_ausp-atwrt.
            wa_saida-elg       = wa_return-atwrt.
            wa_saida-vlr_min_elg = wa_zppt0014-valor_de.
            wa_saida-vlr_max_elg = wa_zppt0014-valor_ate.
            IF wa_saida-elg < wa_zppt0014-valor_de OR wa_saida-elg > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_elg = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_elg = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'MIC'.
*                  wa_saida-mic       = wa_ausp-atwrt.
            wa_saida-mic       = wa_return-atwrt.
            wa_saida-vlr_min_mic = wa_zppt0014-valor_de.
            wa_saida-vlr_max_mic = wa_zppt0014-valor_ate.
            IF wa_saida-mic < wa_zppt0014-valor_de OR wa_saida-mic > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_mic = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_mic = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'RD'.
*                  wa_saida-rd        = wa_ausp-atwrt.
            wa_saida-rd        = wa_return-atwrt.
            wa_saida-vlr_min_rd = wa_zppt0014-valor_de.
            wa_saida-vlr_max_rd = wa_zppt0014-valor_ate.
            IF wa_saida-rd < wa_zppt0014-valor_de OR wa_saida-rd > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_rd = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_rd = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: '+B'.
*                  wa_saida-_b        = wa_ausp-atwrt.
            wa_saida-_b        = wa_return-atwrt.
            wa_saida-vlr_min__b = wa_zppt0014-valor_de.
            wa_saida-vlr_max__b = wa_zppt0014-valor_ate.
            IF wa_saida-_b < wa_zppt0014-valor_de OR wa_saida-_b > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st__b = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st__b = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'CG'.
*                  wa_saida-cg        = wa_ausp-atwrt.
            wa_saida-cg        = wa_return-atwrt.
            wa_saida-vlr_min_cg = wa_zppt0014-valor_de.
            wa_saida-vlr_max_cg = wa_zppt0014-valor_ate.
            IF wa_saida-cg < wa_zppt0014-valor_de OR wa_saida-cg > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_cg = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_cg = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'T.CNT'.
*                  wa_saida-t_cnt     = wa_ausp-atwrt.
            wa_saida-t_cnt     = wa_return-atwrt.
            wa_saida-vlr_min_t_cnt = wa_zppt0014-valor_de.
            wa_saida-vlr_max_t_cnt = wa_zppt0014-valor_ate.
            IF wa_saida-t_cnt < wa_zppt0014-valor_de OR wa_saida-t_cnt > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_t_cnt = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_t_cnt = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'T.AREA'.
*                  wa_saida-t_area    = wa_ausp-atwrt.
            wa_saida-t_area    = wa_return-atwrt.
            wa_saida-vlr_min_t_area = wa_zppt0014-valor_de.
            wa_saida-vlr_max_t_area = wa_zppt0014-valor_ate.
            IF wa_saida-t_area < wa_zppt0014-valor_de OR wa_saida-t_area > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_t_area = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_t_area = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'LEAF'.
*                  wa_saida-leaf      = wa_ausp-atwrt.
            wa_saida-leaf      = wa_return-atwrt.
            wa_saida-vlr_min_leaf = wa_zppt0014-valor_de.
            wa_saida-vlr_max_leaf = wa_zppt0014-valor_ate.
            IF wa_saida-leaf < wa_zppt0014-valor_de OR wa_saida-leaf > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_leaf = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_leaf = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'MR'.
*                  wa_saida-mr        = wa_ausp-atwrt.
            wa_saida-mr        = wa_return-atwrt.
            wa_saida-vlr_min_mr = wa_zppt0014-valor_de.
            wa_saida-vlr_max_mr = wa_zppt0014-valor_ate.
            IF wa_saida-mr < wa_zppt0014-valor_de OR wa_saida-mr > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_mr = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_mr = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'SFI(W)'.
*                  wa_saida-sfi       = wa_ausp-atwrt.
            wa_saida-sfi       = wa_return-atwrt.
            wa_saida-vlr_min_sfi = wa_zppt0014-valor_de.
            wa_saida-vlr_max_sfi = wa_zppt0014-valor_ate.
            IF wa_saida-sfi < wa_zppt0014-valor_de OR wa_saida-sfi > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_sfi = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_sfi = 'D.P'.
              ENDIF.
            ENDIF.

          WHEN: 'SCI'.
*                  wa_saida-sci       = wa_ausp-atwrt.
            wa_saida-sci       = wa_return-atwrt.
            wa_saida-vlr_min_sci = wa_zppt0014-valor_de.
            wa_saida-vlr_max_sci = wa_zppt0014-valor_ate.
            IF wa_saida-sci < wa_zppt0014-valor_de OR wa_saida-sci > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_sci = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_sci = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'CSP'.
*                  wa_saida-csp       = wa_ausp-atwrt.
            wa_saida-csp       = wa_return-atwrt.
            wa_saida-vlr_min_csp = wa_zppt0014-valor_de.
            wa_saida-vlr_max_csp = wa_zppt0014-valor_ate.
            IF wa_saida-csp < wa_zppt0014-valor_de OR wa_saida-csp > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_csp = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_csp = 'D.P'.
              ENDIF.
            ENDIF.
* ADD - IS - 21.05.2013
          WHEN: 'SAFRA'.
            wa_saida-safra     = wa_return-atwrt.
            wa_saida-vlr_min_safra = wa_zppt0014-valor_de.
            wa_saida-vlr_max_safra = wa_zppt0014-valor_ate.
            IF wa_saida-safra < wa_zppt0014-valor_de OR wa_saida-safra > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_safra = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_safra = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'TALHAO'.
            wa_saida-talhao    = wa_return-atwrt.
            wa_saida-vlr_min_talhao = wa_zppt0014-valor_de.
            wa_saida-vlr_max_talhao = wa_zppt0014-valor_ate.
            IF wa_saida-talhao < wa_zppt0014-valor_de OR wa_saida-talhao > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_talhao = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_talhao = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'VARIEDADE'.
            wa_saida-variedade = wa_return-atwrt.
            wa_saida-vlr_min_variedade = wa_zppt0014-valor_de.
            wa_saida-vlr_max_variedade = wa_zppt0014-valor_ate.
            IF wa_saida-variedade < wa_zppt0014-valor_de OR wa_saida-variedade > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_variedade = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_variedade = 'D.P'.
              ENDIF.
            ENDIF.
          WHEN: 'PERIODO'.
            wa_saida-periodo   = wa_return-atwrt.
            wa_saida-vlr_min_periodo = wa_zppt0014-valor_de.
            wa_saida-vlr_max_periodo = wa_zppt0014-valor_ate.
            IF wa_saida-periodo < wa_zppt0014-valor_de OR wa_saida-periodo > wa_zppt0014-valor_ate.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_periodo = 'F.P'.
              ENDIF.
            ELSE.
              IF wa_zppt0014-valor_de IS NOT INITIAL.
                wa_saida-st_periodo = 'D.P'.
              ENDIF.
            ENDIF.
* ADD - IS - 21.05.2013
        ENDCASE.

        CLEAR: wa_mara, wa_zmmt0025, wa_zppt0014.

        CONCATENATE wa_return-charg wa_return-matnr INTO v_acharg_ant.
      ENDLOOP.

      IF NOT ( wa_saida-charg IS INITIAL AND wa_saida-lgort IS INITIAL AND wa_saida-matnr IS INITIAL ).
        APPEND wa_saida TO it_saida.
      ENDIF.

      IF p_fpdr IS NOT INITIAL.
        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<status>).
          CASE p_hvi-low.
            WHEN 'UHML'.
              DELETE it_saida WHERE st_uhml       NE 'F.P'
                                AND st_uhml       NE abap_off.
            WHEN 'UI'.
              DELETE it_saida WHERE st_ui         NE 'F.P'
                                AND st_ui         NE abap_off.
            WHEN 'STR'.
              DELETE it_saida WHERE st_str        NE 'F.P'
                                AND st_str        NE abap_off.
            WHEN 'ELG'.
              DELETE it_saida WHERE st_elg        NE 'F.P'
                                AND st_elg        NE abap_off.
            WHEN 'MIC'.
              DELETE it_saida WHERE st_mic        NE 'F.P'
                                AND st_mic        NE abap_off.
            WHEN 'RD'.
              DELETE it_saida WHERE st_rd         NE 'F.P'
                                AND st_rd         NE abap_off.
            WHEN '_B'.
              DELETE it_saida WHERE st__b         NE 'F.P'
                                AND st__b         NE abap_off.
            WHEN 'CG'.
              DELETE it_saida WHERE st_cg         NE 'F.P'
                                AND st_cg         NE abap_off.
            WHEN 'CNT'.
              DELETE it_saida WHERE st_t_cnt      NE 'F.P'
                                AND st_t_cnt      NE abap_off.
            WHEN 'T_AREA'.
              DELETE it_saida WHERE st_t_area     NE 'F.P'
                                AND st_t_area     NE abap_off.
            WHEN 'LEAF'.
              DELETE it_saida WHERE st_leaf       NE 'F.P'
                                AND st_leaf       NE abap_off.
            WHEN  'MR'.
              DELETE it_saida WHERE st_mr         NE 'F.P'
                                AND st_mr         NE abap_off.
            WHEN  'SFI'.
              DELETE it_saida WHERE st_sfi        NE 'F.P'
                                AND st_sfi        NE abap_off.
            WHEN  'SCI'.
              DELETE it_saida WHERE st_sci        NE 'F.P'
                                AND st_sci        NE abap_off.
            WHEN  'CSP'.
              DELETE it_saida WHERE st_csp        NE 'F.P'
                                AND st_csp        NE abap_off.
            WHEN  'FARDAO'.
              DELETE it_saida WHERE st_fardao     NE 'F.P'
                                AND st_fardao     NE abap_off.
            WHEN  'TALHAO'.
              DELETE it_saida WHERE st_talhao     NE 'F.P'
                                AND st_talhao     NE abap_off.
            WHEN  'VARIEDADE'.
              DELETE it_saida WHERE st_variedade  NE 'F.P'
                                AND st_variedade  NE abap_off.
            WHEN  'SAFRA'.
              DELETE it_saida WHERE st_safra      NE 'F.P'
                                AND st_safra      NE abap_off.
            WHEN  'PERIODO'.
              DELETE it_saida WHERE st_periodo    NE 'F.P'
                                AND st_uhml       NE abap_off.
          ENDCASE.
        ENDLOOP.

      ENDIF.

      IF p_dpdr IS NOT INITIAL.
        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<status_>).

          CASE p_hvi-low.
            WHEN 'UHML'.
              DELETE it_saida WHERE st_uhml       NE 'D.P'
                                AND st_uhml       NE abap_off.
            WHEN 'UI'.
              DELETE it_saida WHERE st_ui         NE 'D.P'
                                AND st_ui         NE abap_off.
            WHEN 'STR'.
              DELETE it_saida WHERE st_str        NE 'D.P'
                                AND st_str        NE abap_off.
            WHEN 'ELG'.
              DELETE it_saida WHERE st_elg        NE 'D.P'
                                AND st_elg        NE abap_off.
            WHEN 'MIC'.
              DELETE it_saida WHERE st_mic        NE 'D.P'
                                AND st_mic        NE abap_off.
            WHEN 'RD'.
              DELETE it_saida WHERE st_rd         NE 'D.P'
                                AND st_rd         NE abap_off.
            WHEN '_B'.
              DELETE it_saida WHERE st__b         NE 'D.P'
                                AND st__b         NE abap_off.
            WHEN 'CG'.
              DELETE it_saida WHERE st_cg         NE 'D.P'
                                AND st_cg         NE abap_off.
            WHEN 'CNT'.
              DELETE it_saida WHERE st_t_cnt      NE 'D.P'
                                AND st_t_cnt      NE abap_off.
            WHEN 'T_AREA'.
              DELETE it_saida WHERE st_t_area     NE 'D.P'
                                AND st_t_area     NE abap_off.
            WHEN 'LEAF'.
              DELETE it_saida WHERE st_leaf       NE 'D.P'
                                AND st_leaf       NE abap_off.
            WHEN  'MR'.
              DELETE it_saida WHERE st_mr         NE 'D.P'
                                AND st_mr         NE abap_off.
            WHEN  'SFI'.
              DELETE it_saida WHERE st_sfi        NE 'D.P'
                                AND st_sfi        NE abap_off.
            WHEN  'SCI'.
              DELETE it_saida WHERE st_sci        NE 'D.P'
                                AND st_sci        NE abap_off.
            WHEN  'CSP'.
              DELETE it_saida WHERE st_csp        NE 'D.P'
                                AND st_csp        NE abap_off.
            WHEN  'FARDAO'.
              DELETE it_saida WHERE st_fardao     NE 'D.P'
                                AND st_fardao     NE abap_off.
            WHEN  'TALHAO'.
              DELETE it_saida WHERE st_talhao     NE 'D.P'
                                AND st_talhao     NE abap_off.
            WHEN  'VARIEDADE'.
              DELETE it_saida WHERE st_variedade  NE 'D.P'
                                AND st_variedade  NE abap_off.
            WHEN  'SAFRA'.
              DELETE it_saida WHERE st_safra      NE 'D.P'
                                AND st_safra      NE abap_off.
            WHEN  'PERIODO'.
              DELETE it_saida WHERE st_periodo    NE 'D.P'
                                AND st_periodo    NE abap_off.
          ENDCASE.
        ENDLOOP.
      ENDIF.

    ELSEIF ( hist_mm EQ 'X').   "ADD - IS - 21.05.2013

      IF p_hvi-low IS NOT INITIAL AND p_fpdr IS NOT INITIAL OR p_dpdr IS NOT INITIAL.

        LOOP AT it_zmmt0027 INTO wa_zmmt0027.
          CLEAR: wa_ztsafrafardos, wa_saida.

*-CS2022000332-#78803-05.07.2022-JT-inicio
          CLEAR wa_zmmt0008.
          READ TABLE it_zmmt0008 INTO wa_zmmt0008 WITH KEY lgort      = wa_zmmt0027-lgort
                                                           charg      = wa_zmmt0027-charg
                                                           werks_orig = wa_zmmt0027-werks
                                                BINARY SEARCH.
*-CS2022000332-#78803-05.07.2022-JT-fim

          wa_saida-normt        = wa_zmmt0027-normt.
          wa_saida-znumer       = wa_zmmt0027-charg.
          wa_saida-werks_orig   = wa_zmmt0008-werks.
          wa_saida-znumer       = wa_saida-znumer(3).

*-CS2022000332-#82292-26.07.2022-JT-inicio
          IF wa_zmmt0027-adquirido_terc = abap_false.
            SEARCH wa_zmmt0027-charg FOR './.'.
            vl_pos = sy-fdpos.
          ELSE.
            vl_pos = 3.
          ENDIF.
*-CS2022000332-#82292-26.07.2022-JT-fim

          READ TABLE it_zppt0002 INTO wa_zppt0002 WITH KEY acharg = wa_zmmt0027-charg
                                                           werks  = wa_zmmt0027-werks. " BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida-fardao = wa_zppt0002-charg.
            wa_saida-charg1 = wa_zppt0002-cd_sai.
          ENDIF.

*        READ TABLE IT_MSEG INTO WA_MSEG WITH KEY MATNR = WA_ZMMT0027-MATNR
*                                                 CHARG = WA_ZMMT0027-CHARG.
          LOOP AT it_return INTO wa_return WHERE matnr = wa_zmmt0027-matnr AND charg = wa_zmmt0027-charg.

            IF wa_zmmt0027-charg IS NOT INITIAL.
              wa_saida-znumer = |{ wa_zmmt0027-charg(3) }|.
*              WA_SAIDA-ZNUMER = WA_ZTFARDOS-WERKS_FROM.
            ENDIF.

*-CS2022000332-#82292-26.07.2022-JT-inicio
            IF wa_zmmt0027-adquirido_terc = abap_false.
              wa_saida-charg      = wa_zmmt0027-charg+vl_pos(8).
            ELSE.
              wa_saida-charg      = wa_zmmt0027-charg+vl_pos(7).
            ENDIF.
*-CS2022000332-#82292-26.07.2022-JT-fim

            wa_saida-clabs        = wa_zmmt0027-menge.
            wa_saida-lgort        = wa_zmmt0027-lgort.
            wa_saida-matnr        = wa_zmmt0027-matnr.
            wa_saida-maktx        = wa_zmmt0027-matkl.

*        wa_saida-reserv       = wa_zmmt0027-.
            wa_saida-ersda        = wa_zmmt0027-budat.
            wa_saida-safra        = wa_zmmt0027-safra.
            wa_saida-status       = wa_zmmt0027-status.

*-CS2022000332-#82292-26.07.2022-JT-inicio
            CLEAR: wa_v_mkm_zu_kls_bez, wa_zppt0014.

            READ TABLE it_v_mkm_zu_kls_bez INTO wa_v_mkm_zu_kls_bez WITH KEY atinn = wa_return-atinn.
            READ TABLE it_zppt0014         INTO wa_zppt0014         WITH KEY atinn = wa_v_mkm_zu_kls_bez-atinn.
*-CS2022000332-#82292-26.07.2022-JT-fim

            CASE wa_v_mkm_zu_kls_bez-atbez.
              WHEN 'UHML'.
                wa_saida-uhml         = wa_zmmt0027-far_uhml.
                wa_saida-vlr_min_uhml = wa_zppt0014-valor_de.
                wa_saida-vlr_max_uhml = wa_zppt0014-valor_ate.
                IF wa_saida-uhml < wa_zppt0014-valor_de OR wa_saida-uhml > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_uhml = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_uhml = 'D.P'.
                  ENDIF.
                ENDIF.


              WHEN 'UI'.
                wa_saida-ui           = wa_zmmt0027-far_ui.
                wa_saida-vlr_min_ui = wa_zppt0014-valor_de.
                wa_saida-vlr_max_ui = wa_zppt0014-valor_ate.
                IF wa_saida-ui < wa_zppt0014-valor_de OR wa_saida-ui > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_ui = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_ui = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'STR'.
                wa_saida-str          = wa_zmmt0027-far_str.
                wa_saida-vlr_min_str = wa_zppt0014-valor_de.
                wa_saida-vlr_max_str = wa_zppt0014-valor_ate.
                IF wa_saida-str < wa_zppt0014-valor_de OR wa_saida-str > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_str = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_str = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'ELG'.
                wa_saida-elg          = wa_zmmt0027-far_elg.
                wa_saida-vlr_min_elg = wa_zppt0014-valor_de.
                wa_saida-vlr_max_elg = wa_zppt0014-valor_ate.
                IF wa_saida-elg < wa_zppt0014-valor_de OR wa_saida-elg > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_elg = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_elg = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'MIC'.
                wa_saida-mic          = wa_zmmt0027-far_mic.
                wa_saida-vlr_min_mic = wa_zppt0014-valor_de.
                wa_saida-vlr_max_mic = wa_zppt0014-valor_ate.
                IF wa_saida-mic < wa_zppt0014-valor_de OR wa_saida-mic > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_mic = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_mic = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'RD'.
                wa_saida-rd           = wa_zmmt0027-far_rd.
                wa_saida-vlr_min_rd = wa_zppt0014-valor_de.
                wa_saida-vlr_max_rd = wa_zppt0014-valor_ate.
                IF wa_saida-rd < wa_zppt0014-valor_de OR wa_saida-rd > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_rd = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_rd = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN '_B'.
                wa_saida-_b           = wa_zmmt0027-far_b.
                wa_saida-vlr_min__b = wa_zppt0014-valor_de.
                wa_saida-vlr_max__b = wa_zppt0014-valor_ate.
                IF wa_saida-_b < wa_zppt0014-valor_de OR wa_saida-_b > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st__b = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st__b = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'CG'.
                wa_saida-cg           = wa_zmmt0027-far_cg.
                wa_saida-vlr_min_cg = wa_zppt0014-valor_de.
                wa_saida-vlr_max_cg = wa_zppt0014-valor_ate.
                IF wa_saida-cg < wa_zppt0014-valor_de OR wa_saida-cg > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_cg = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_cg = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'T_CNT'.
                wa_saida-t_cnt        = wa_zmmt0027-far_tcnt.
                wa_saida-vlr_min_t_cnt = wa_zppt0014-valor_de.
                wa_saida-vlr_max_t_cnt = wa_zppt0014-valor_ate.
                IF wa_saida-t_cnt < wa_zppt0014-valor_de OR wa_saida-t_cnt > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_t_cnt = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_t_cnt = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'T_AREA'.
                wa_saida-t_area       = wa_zmmt0027-far_tarea.
                wa_saida-vlr_min_t_area = wa_zppt0014-valor_de.
                wa_saida-vlr_max_t_area = wa_zppt0014-valor_ate.
                IF wa_saida-t_area < wa_zppt0014-valor_de OR wa_saida-t_area > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_t_area = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_t_area = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'LEAF'.
                wa_saida-leaf         = wa_zmmt0027-far_leaf.
                wa_saida-vlr_min_leaf = wa_zppt0014-valor_de.
                wa_saida-vlr_max_leaf = wa_zppt0014-valor_ate.
                IF wa_saida-leaf < wa_zppt0014-valor_de OR wa_saida-leaf > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_leaf = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_leaf = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'MR'.
                wa_saida-mr           = wa_zmmt0027-far_mr.
                wa_saida-vlr_min_mr = wa_zppt0014-valor_de.
                wa_saida-vlr_max_mr = wa_zppt0014-valor_ate.
                IF wa_saida-mr < wa_zppt0014-valor_de OR wa_saida-mr > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_mr = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_mr = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'SFI'.
                wa_saida-sfi          = wa_zmmt0027-far_sfiw.
                wa_saida-vlr_min_sfi = wa_zppt0014-valor_de.
                wa_saida-vlr_max_sfi = wa_zppt0014-valor_ate.
                IF wa_saida-sfi < wa_zppt0014-valor_de OR wa_saida-sfi > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_sfi = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_sfi = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'SCI'.
                wa_saida-sci          = wa_zmmt0027-far_sci.
                wa_saida-vlr_min_sci = wa_zppt0014-valor_de.
                wa_saida-vlr_max_sci = wa_zppt0014-valor_ate.
                IF wa_saida-sci < wa_zppt0014-valor_de OR wa_saida-sci > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_sci = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_sci = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'CSP'.
                wa_saida-csp          = wa_zmmt0027-far_csp.
                wa_saida-vlr_min_csp = wa_zppt0014-valor_de.
                wa_saida-vlr_max_csp = wa_zppt0014-valor_ate.
                IF wa_saida-csp < wa_zppt0014-valor_de OR wa_saida-csp > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_csp = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_csp = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'FARDAO'.
                wa_saida-fardao       = wa_zmmt0027-charg_orig.
                wa_saida-vlr_min_fardao = wa_zppt0014-valor_de.
                wa_saida-vlr_max_fardao = wa_zppt0014-valor_ate.
                IF wa_saida-fardao < wa_zppt0014-valor_de OR wa_saida-fardao > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_fardao = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_fardao = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'TALHAO'.
                wa_saida-talhao         = wa_zmmt0027-talhao.
                wa_saida-vlr_min_talhao = wa_zppt0014-valor_de.
                wa_saida-vlr_max_talhao = wa_zppt0014-valor_ate.
                IF wa_saida-talhao < wa_zppt0014-valor_de OR wa_saida-talhao > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_talhao = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_talhao = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'VARIEDADE'.
                wa_saida-variedade    = wa_zmmt0027-variedade.
                wa_saida-vlr_min_variedade = wa_zppt0014-valor_de.
                wa_saida-vlr_max_variedade = wa_zppt0014-valor_ate.
                IF wa_saida-variedade < wa_zppt0014-valor_de OR wa_saida-variedade > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_variedade = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_variedade = 'D.P'.
                  ENDIF.
                ENDIF.

              WHEN 'PERIODO'.
                wa_saida-periodo      = wa_zmmt0027-far_periodo.
                wa_saida-vlr_min_periodo = wa_zppt0014-valor_de.
                wa_saida-vlr_max_periodo = wa_zppt0014-valor_ate.
                IF wa_saida-periodo < wa_zppt0014-valor_de OR wa_saida-periodo > wa_zppt0014-valor_ate.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_periodo = 'F.P'.
                  ENDIF.
                ELSE.
                  IF wa_zppt0014-valor_de IS NOT INITIAL.
                    wa_saida-st_periodo = 'D.P'.
                  ENDIF.
                ENDIF.
            ENDCASE.
          ENDLOOP.

          APPEND wa_saida TO it_saida.
          CLEAR wa_saida.
          CLEAR wa_zmmt0025.
          CLEAR wa_zppt0014.
          CLEAR wa_return.
          CLEAR wa_zmmt0027.
          CLEAR wa_zppt0002.
          CLEAR wa_v_mkm_zu_kls_bez.
        ENDLOOP.

      ELSE.
        LOOP AT it_zmmt0027 INTO wa_zmmt0027.
          CLEAR: wa_saida, wa_zppt0002.

          wa_saida-normt        = wa_zmmt0027-normt.
          wa_saida-znumer       = wa_zmmt0027-charg.
          wa_saida-znumer       = wa_saida-znumer(3).

          SEARCH wa_zmmt0027-charg FOR './.'.
          vl_pos = sy-fdpos + 1.

*-CS2022000332-#78803-05.07.2022-JT-inicio
          CLEAR wa_zmmt0008.
          READ TABLE it_zmmt0008 INTO wa_zmmt0008 WITH KEY lgort      = wa_zmmt0027-lgort
                                                           charg      = wa_zmmt0027-charg
                                                           werks_orig = wa_zmmt0027-werks
                                                BINARY SEARCH.
*-CS2022000332-#78803-05.07.2022-JT-fim

*        READ TABLE IT_ZPPT0014 INTO WA_ZPPT0014 WITH KEY ATNAM = WA_ZMMT0027-FAR_UHML+3.

          wa_saida-charg        = wa_zmmt0027-charg+vl_pos.
          wa_saida-clabs        = wa_zmmt0027-menge.
          wa_saida-lgort        = wa_zmmt0027-lgort.
          wa_saida-werks_orig   = wa_zmmt0008-werks.   "*-CS2022000332-#78803-05.07.2022-JT-inicio
          wa_saida-matnr        = wa_zmmt0027-matnr.
          wa_saida-maktx        = wa_zmmt0027-matkl.
          wa_saida-uhml         = wa_zmmt0027-far_uhml.
          wa_saida-ui           = wa_zmmt0027-far_ui.
          wa_saida-str          = wa_zmmt0027-far_str.
          wa_saida-elg          = wa_zmmt0027-far_elg.
          wa_saida-mic          = wa_zmmt0027-far_mic.
          wa_saida-rd           = wa_zmmt0027-far_rd.
          wa_saida-_b           = wa_zmmt0027-far_b.
          wa_saida-cg           = wa_zmmt0027-far_cg.
          wa_saida-t_cnt        = wa_zmmt0027-far_tcnt.
          wa_saida-t_area       = wa_zmmt0027-far_tarea.
          wa_saida-leaf         = wa_zmmt0027-far_leaf.
          wa_saida-mr           = wa_zmmt0027-far_mr.
          wa_saida-sfi          = wa_zmmt0027-far_sfiw.
          wa_saida-sci          = wa_zmmt0027-far_sci.
          wa_saida-csp          = wa_zmmt0027-far_csp.
          wa_saida-fardao       = wa_zmmt0027-charg_orig.
          wa_saida-talhao       = wa_zmmt0027-talhao.
          wa_saida-variedade    = wa_zmmt0027-variedade.
*        wa_saida-reserv       = wa_zmmt0027-.
          wa_saida-ersda        = wa_zmmt0027-budat.
          wa_saida-safra        = wa_zmmt0027-safra.
          wa_saida-periodo      = wa_zmmt0027-far_periodo.
          wa_saida-status       = wa_zmmt0027-status.

          READ TABLE it_zppt0002 INTO wa_zppt0002 WITH KEY acharg = wa_zmmt0027-charg
                                                           werks  = wa_zmmt0027-werks. " BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida-fardao = wa_zppt0002-charg.
            wa_saida-charg1 = wa_zppt0002-cd_sai.
          ENDIF.

*        ENDIF.

          APPEND wa_saida TO it_saida.
          CLEAR: wa_saida, wa_zmmt0027, wa_zppt0002.
        ENDLOOP.
      ENDIF.

      IF p_fpdr IS NOT INITIAL.
        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<statush>).
          CASE p_hvi-low.
            WHEN 'UHML'.
              DELETE it_saida WHERE st_uhml       NE 'F.P'
                                AND st_uhml       NE abap_off.
            WHEN 'UI'.
              DELETE it_saida WHERE st_ui         NE 'F.P'
                                AND st_ui         NE abap_off.
            WHEN 'STR'.
              DELETE it_saida WHERE st_str        NE 'F.P'
                                AND st_str        NE abap_off.
            WHEN 'ELG'.
              DELETE it_saida WHERE st_elg        NE 'F.P'
                                AND st_elg        NE abap_off.
            WHEN 'MIC'.
              DELETE it_saida WHERE st_mic        NE 'F.P'
                                AND st_mic        NE abap_off.
            WHEN 'RD'.
              DELETE it_saida WHERE st_rd         NE 'F.P'
                                AND st_rd         NE abap_off.
            WHEN '_B'.
              DELETE it_saida WHERE st__b         NE 'F.P'
                                AND st__b         NE abap_off.
            WHEN 'CG'.
              DELETE it_saida WHERE st_cg         NE 'F.P'
                                AND st_cg         NE abap_off.
            WHEN 'CNT'.
              DELETE it_saida WHERE st_t_cnt      NE 'F.P'
                                AND st_t_cnt      NE abap_off.
            WHEN 'T_AREA'.
              DELETE it_saida WHERE st_t_area     NE 'F.P'
                                AND st_t_area     NE abap_off.
            WHEN 'LEAF'.
              DELETE it_saida WHERE st_leaf       NE 'F.P'
                                AND st_leaf       NE abap_off.
            WHEN  'MR'.
              DELETE it_saida WHERE st_mr         NE 'F.P'
                                AND st_mr         NE abap_off.
            WHEN  'SFI'.
              DELETE it_saida WHERE st_sfi        NE 'F.P'
                                AND st_sfi        NE abap_off.
            WHEN  'SCI'.
              DELETE it_saida WHERE st_sci        NE 'F.P'
                                AND st_sci        NE abap_off.
            WHEN  'CSP'.
              DELETE it_saida WHERE st_csp        NE 'F.P'
                                AND st_csp        NE abap_off.
            WHEN  'FARDAO'.
              DELETE it_saida WHERE st_fardao     NE 'F.P'
                                AND st_fardao     NE abap_off.
            WHEN  'TALHAO'.
              DELETE it_saida WHERE st_talhao     NE 'F.P'
                                AND st_talhao     NE abap_off.
            WHEN  'VARIEDADE'.
              DELETE it_saida WHERE st_variedade  NE 'F.P'
                                AND st_variedade  NE abap_off.
            WHEN  'SAFRA'.
              DELETE it_saida WHERE st_safra      NE 'F.P'
                                AND st_safra      NE abap_off.
            WHEN  'PERIODO'.
              DELETE it_saida WHERE st_periodo    NE 'F.P'
                                AND st_periodo    NE abap_off.
          ENDCASE.
        ENDLOOP.

      ENDIF.

      IF p_dpdr IS NOT INITIAL.
        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<statush_>).

          CASE p_hvi-low.
            WHEN 'UHML'.
              DELETE it_saida WHERE st_uhml       NE 'D.P'
                                AND st_uhml       NE abap_off.
            WHEN 'UI'.
              DELETE it_saida WHERE st_ui         NE 'D.P'
                                AND st_ui         NE abap_off.
            WHEN 'STR'.
              DELETE it_saida WHERE st_str        NE 'D.P'
                                AND st_str        NE abap_off.
            WHEN 'ELG'.
              DELETE it_saida WHERE st_elg        NE 'D.P'
                                AND st_elg        NE abap_off.
            WHEN 'MIC'.
              DELETE it_saida WHERE st_mic        NE 'D.P'
                                AND st_mic        NE abap_off.
            WHEN 'RD'.
              DELETE it_saida WHERE st_rd         NE 'D.P'
                                AND st_rd         NE abap_off.
            WHEN '_B'.
              DELETE it_saida WHERE st__b         NE 'D.P'
                                AND st__b         NE abap_off.
            WHEN 'CG'.
              DELETE it_saida WHERE st_cg         NE 'D.P'
                                AND st_cg         NE abap_off.
            WHEN 'CNT'.
              DELETE it_saida WHERE st_t_cnt      NE 'D.P'
                                AND st_t_cnt      NE abap_off.
            WHEN 'T_AREA'.
              DELETE it_saida WHERE st_t_area     NE 'D.P'
                                AND st_t_area     NE abap_off.
            WHEN 'LEAF'.
              DELETE it_saida WHERE st_leaf       NE 'D.P'
                                AND st_leaf       NE abap_off.
            WHEN  'MR'.
              DELETE it_saida WHERE st_mr         NE 'D.P'
                                AND st_mr         NE abap_off.
            WHEN  'SFI'.
              DELETE it_saida WHERE st_sfi        NE 'D.P'
                                AND st_sfi        NE abap_off.
            WHEN  'SCI'.
              DELETE it_saida WHERE st_sci        NE 'D.P'
                                AND st_sci        NE abap_off.
            WHEN  'CSP'.
              DELETE it_saida WHERE st_csp        NE 'D.P'
                                AND st_csp        NE abap_off.
            WHEN  'FARDAO'.
              DELETE it_saida WHERE st_fardao     NE 'D.P'
                                AND st_fardao     NE abap_off.
            WHEN  'TALHAO'.
              DELETE it_saida WHERE st_talhao     NE 'D.P'
                                AND st_talhao     NE abap_off.
            WHEN  'VARIEDADE'.
              DELETE it_saida WHERE st_variedade  NE 'D.P'
                                AND st_variedade  NE abap_off.
            WHEN  'SAFRA'.
              DELETE it_saida WHERE st_safra      NE 'D.P'
                                AND st_safra      NE abap_off.
            WHEN  'PERIODO'.
              DELETE it_saida WHERE st_periodo    NE 'D.P'
                                AND st_periodo    NE abap_off.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ORGANIZA_SAIDA


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS 'FT0100'.
  SET TITLEBAR  'TB0100'.

  IF ( cl_container IS INITIAL ).
    PERFORM: create_object_alv.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
FORM create_object_alv.

  CREATE OBJECT cl_container
    EXPORTING
      container_name              = 'CONTAINER_PRINCIPAL'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  CREATE OBJECT cl_grid
    EXPORTING
      i_parent          = cl_container
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF ( resu_mm EQ 'X' ).
    PERFORM: catalog_alv_resumo.

    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = it_saida_resu
        it_fieldcatalog               = ls_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    IF ( todos_mm EQ 'X' ).
      PERFORM: catalog_alv.

      CALL METHOD cl_grid->set_table_for_first_display
        EXPORTING
          is_layout                     = gs_layout
        CHANGING
          it_outtab                     = it_saida
          it_fieldcatalog               = ls_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSEIF ( hist_mm EQ 'X' ).    "ADD - IS - 21.05.2013
      PERFORM: catalog_alv_historico.

      CALL METHOD cl_grid->set_table_for_first_display
        EXPORTING
          is_layout                     = gs_layout
        CHANGING
          it_outtab                     = it_saida
          it_fieldcatalog               = ls_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: lista.
ENDFORM.                    " CREATE_OBJECT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai INPUT.
  CASE sy-ucomm.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CATALOG_ALV
*&---------------------------------------------------------------------*
FORM catalog_alv.

  IF p_hvi-low IS INITIAL.
    PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '20'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'UHML             '  'UHML                 ' '7 '  ''  ''  '' '',
            'UI               '  'UI                   ' '7 '  ''  ''  '' '',
            'STR              '  'STR                  ' '7 '  ''  ''  '' '',
            'ELG              '  'ELG                  ' '7 '  ''  ''  '' '',
            'MIC              '  'MIC                  ' '7 '  ''  ''  '' '',
            'RD               '  'RD                   ' '7 '  ''  ''  '' '',
            '_B               '  '_+B                  ' '7 '  ''  ''  '' '',
            'CG               '  'CG                   ' '7 '  ''  ''  '' '',
            'T_CNT            '  'T.CNT                ' '7 '  ''  ''  '' '',
            'T_AREA           '  'T.AREA               ' '7 '  ''  ''  '' '',
            'LEAF             '  'LEAF                 ' '7 '  ''  ''  '' '',
            'MR               '  'MR                   ' '7 '  ''  ''  '' '',
            'SFI              '  'SFI(W)               ' '7 '  ''  ''  '' '',
            'SCI              '  'SCI                  ' '7 '  ''  ''  '' '',
            'CSP              '  'CSP                  ' '7 '  ''  ''  '' '',
            'SAFRA            '  'Safra                ' '4 '  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'FARDAO           '  'Fardão               ' '7 '  ''  ''  '' '',
            'TALHAO           '  'Talhão               ' '7 '  ''  ''  '' '',
            'VARIEDADE        '  'Variedade            ' '10'  ''  ''  '' '',
            'PERIODO          '  'Periodo              ' '10'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'STATUS           '  'Status               ' '10'  ''  ''  '' ''.   "ADD - IS - 21.05.2013
*          'RESERV'    'Reservado'       '10'  ''  ''  ''.

  ELSEIF p_hvi-low IS NOT INITIAL AND p_fpdr IS INITIAL AND p_dpdr IS INITIAL.

    PERFORM  alv_preenche_cat USING:
           'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG            '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR            '  'Material             ' '  '  ''  'X' '' '',
           'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
           'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
           'UHML             '  'UHML                 ' '7 '  ''  ''  '' '',
           'UI               '  'UI                   ' '7 '  ''  ''  '' '',
           'STR              '  'STR                  ' '7 '  ''  ''  '' '',
           'ELG              '  'ELG                  ' '7 '  ''  ''  '' '',
           'MIC              '  'MIC                  ' '7 '  ''  ''  '' '',
           'RD               '  'RD                   ' '7 '  ''  ''  '' '',
           '_B               '  '_+B                  ' '7 '  ''  ''  '' '',
           'CG               '  'CG                   ' '7 '  ''  ''  '' '',
           'T_CNT            '  'T.CNT                ' '7 '  ''  ''  '' '',
           'T_AREA           '  'T.AREA               ' '7 '  ''  ''  '' '',
           'LEAF             '  'LEAF                 ' '7 '  ''  ''  '' '',
           'MR               '  'MR                   ' '7 '  ''  ''  '' '',
           'SFI              '  'SFI(W)               ' '7 '  ''  ''  '' '',
           'SCI              '  'SCI                  ' '7 '  ''  ''  '' '',
           'CSP              '  'CSP                  ' '7 '  ''  ''  '' '',
           'SAFRA            '  'Safra                ' '4 '  ''  ''  '' '',   "ADD - IS - 21.05.2013
           'FARDAO           '  'Fardão               ' '7 '  ''  ''  '' '',
           'TALHAO           '  'Talhão               ' '7 '  ''  ''  '' '',
           'VARIEDADE        '  'Variedade            ' '10'  ''  ''  '' '',
           'PERIODO           '  'Periodo             ' '10'  ''  ''  '' '',   "ADD - IS - 21.05.2013
           'STATUS            '  'Status              ' '10'  ''  ''  '' ''.   "ADD - IS - 21.05.2013
*          'RESERV'    'Reservado'       '10'  ''  ''  ''.
  ELSEIF
      p_fpdr IS NOT INITIAL AND p_dpdr IS NOT INITIAL.
    PERFORM  alv_preenche_cat USING:
     'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
     'CHARG            '  'Número               ' '10'  ''  ''  '' '',
     'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
     'MATNR            '  'Material             ' '  '  ''  'X' '' '',
     'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
     'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
     'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
     'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
     'UHML             '  'UHML                 ' '7 '  ''  ''  '' '',
     'vlr_min_UHML     '  'v_min_UHML           ' '20'  ''  ''  '' '',
     'VLR_MAX_UHML     '  'v_max_UHML           ' '20'  ''  ''  '' '',
     'ST_UHML          '  'ST_UHML              ' '20'  ''  ''  '' 'c410',
     'UI               '  'UI                   ' '7 '  ''  ''  '' '',
     'vlr_min_UI       '  'v_min_UI             ' '20'  ''  ''  '' '',
     'vlr_max_UI       '  'v_max_UI             ' '20'  ''  ''  '' '',
     'st_UI            '  'st_UI                ' '20'  ''  ''  '' '',
     'STR              '  'STR                  ' '7 '  ''  ''  '' '',
     'vlr_min_STR      '  'v_min_STR            ' '20'  ''  ''  '' '',
     'vlr_max_STR      '  'v_max_STR            ' '20'  ''  ''  '' '',
     'st_STR           '  'st_STR               ' '20'  ''  ''  '' '',
     'ELG              '  'ELG                  ' '7 '  ''  ''  '' '',
     'vlr_min_ELG      '  'v_min_ELG            ' '20'  ''  ''  '' '',
     'vlr_max_ELG      '  'v_max_ELG            ' '20'  ''  ''  '' '',
     'st_ELG           '  'st_ELG               ' '20'  ''  ''  '' '',
     'MIC              '  'MIC                  ' '7 '  ''  ''  '' '',
     'vlr_min_MIC      '  'v_min_MIC            ' '20'  ''  ''  '' '',
     'vlr_max_MIC      '  'v_max_MIC            ' '20'  ''  ''  '' '',
     'st_MIC           '  'st_MIC               ' '20'  ''  ''  '' '',
     'RD               '  'RD                   ' '7 '  ''  ''  '' '',
     'vlr_min_RD       '  'v_min_RD             ' '20'  ''  ''  '' '',
     'vlr_max_RD       '  'v_max_RD             ' '20'  ''  ''  '' '',
     'st_RD            '  'st_RD                ' '20'  ''  ''  '' '',
     '_B               '  '_+b                  ' '7 '  ''  ''  '' '',
     'vlr_min__B       '  'v_min__B             ' '20'  ''  ''  '' '',
     'vlr_max__B       '  'v_max__B             ' '20'  ''  ''  '' '',
     'st__B            '  'st__B                ' '20'  ''  ''  '' '',
     'CG               '  'CG                   ' '7 '  ''  ''  '' '',
     'vlr_min_CG       '  'v_min_CG             ' '20'  ''  ''  '' '',
     'vlr_max_CG       '  'v_max_CG             ' '20'  ''  ''  '' '',
     'st_CG            '  'st_CG                ' '20'  ''  ''  '' '',
     't_cnt            '  't.cnt                ' '7 '  ''  ''  '' '',
     'vlr_min_t_cnt    '  'v_min_t_cnt          ' '20'  ''  ''  '' '',
     'vlr_max_t_cnt    '  'v_max_t_cnt          ' '20'  ''  ''  '' '',
     'st_t_cnt         '  'st_t_cnt             ' '20'  ''  ''  '' '',
     't_area           '  't.area               ' '7 '  ''  ''  '' '',
     'vlr_min_t_area   '  'v_min_t_area         ' '20'  ''  ''  '' '',
     'vlr_max_t_area   '  'v_max_t_area         ' '20'  ''  ''  '' '',
     'st_t_area        '  'st_t_area            ' '20'  ''  ''  '' '',
     'leaf             '  'leaf                 ' '7 '  ''  ''  '' '',
     'vlr_min_leaf     '  'v_min_leaf           ' '20'  ''  ''  '' '',
     'vlr_max_leaf     '  'v_max_leaf           ' '20'  ''  ''  '' '',
     'st_leaf          '  'st_leaf              ' '20'  ''  ''  '' '',
     'mr               '  'mr                   ' '7 '  ''  ''  '' '',
     'vlr_min_mr       '  'v_min_mr             ' '20'  ''  ''  '' '',
     'vlr_max_mr       '  'v_max_mr             ' '20'  ''  ''  '' '',
     'st_mr            '  'st_mr                ' '20'  ''  ''  '' '',
     'sfi              '  'sfi(w)               ' '7 '  ''  ''  '' '',
     'vlr_min_sfi      '  'v_min_sfi            ' '20'  ''  ''  '' '',
     'vlr_max_sfi      '  'v_max_sfi            ' '20'  ''  ''  '' '',
     'st_sfi           '  'st_sfi               ' '20'  ''  ''  '' '',
     'sci              '  'sci                  ' '7 '  ''  ''  '' '',
     'vlr_min_sci      '  'v_min_sci            ' '20'  ''  ''  '' '',
     'vlr_max_sci      '  'v_max_sci            ' '20'  ''  ''  '' '',
     'st_sci           '  'st_sci               ' '20'  ''  ''  '' '',
     'csp              '  'csp                  ' '7 '  ''  ''  '' '',
     'vlr_min_csp      '  'v_min_csp            ' '20'  ''  ''  '' '',
     'vlr_max_csp      '  'v_max_csp            ' '20'  ''  ''  '' '',
     'st_csp           '  'st_sci               ' '20'  ''  ''  '' '',
     'safra            '  'safra                ' '4 '  ''  ''  '' '',   "add - is - 21.05.2013
     'vlr_min_safra    '  'v_min_safra          ' '20'  ''  ''  '' '',
     'vlr_max_safra    '  'v_max_safra          ' '20'  ''  ''  '' '',
     'st_safra         '  'st_safra             ' '20'  ''  ''  '' '',
     'faRDao           '  'faRDão               ' '7 '  ''  ''  '' '',
     'vlr_min_faRDao   '  'v_min_faRDao         ' '20'  ''  ''  '' '',
     'vlr_max_faRDao   '  'v_max_faRDao         ' '20'  ''  ''  '' '',
     'st_faRDao        '  'st_faRDao            ' '20'  ''  ''  '' '',
     'talhao           '  'talhão               ' '7 '  ''  ''  '' '',
     'vlr_min_talhao   '  'v_min_talhao         ' '20'  ''  ''  '' '',
     'vlr_max_talhao   '  'v_max_talhao         ' '20'  ''  ''  '' '',
     'st_talhao        '  'st_talhao            ' '20'  ''  ''  '' '',
     'variedade        '  'variedade            ' '10'  ''  ''  '' '',
     'vlr_min_variedade '  'v_min_variedade     ' '20'  ''  ''  '' '',
     'vlr_max_variedade '  'v_max_variedade     ' '20'  ''  ''  '' '',
     'st_variedade      '  'st_variedade        ' '20'  ''  ''  '' '',
     'periodo           '  'periodo             ' '10'  ''  ''  '' '',   "add - is - 21.05.2013
     'status            '  'status              ' '10'  ''  ''  '' ''.   "add - is - 21.05.2013
*          'reserv'    'reservado'       '10'  ''  ''  ''.


  ELSEIF p_hvi-low IS NOT INITIAL AND p_fpdr IS NOT INITIAL OR p_dpdr IS NOT INITIAL.
    CASE p_hvi-low.
      WHEN 'UHML'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'UHML             '  'UHML                 ' '7 '  ''  ''  '' '',
            'vlr_min_UHML     '  'v_min_UHML           ' '20'  ''  ''  '' '',
            'VLR_MAX_UHML     '  'v_max_UHML           ' '20'  ''  ''  '' '',
            'ST_UHML          '  'ST_UHML              ' '20'  ''  ''  '' 'c410'.

      WHEN 'UI'.
        PERFORM  alv_preenche_cat USING:
           'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG            '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR            '  'Material             ' '  '  ''  'X' '' '',
           'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
           'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
           'UI               '  'UI                   ' '7 '  ''  ''  '' '',
           'vlr_min_UI       '  'v_min_UI             ' '20'  ''  ''  '' '',
           'vlr_max_UI       '  'v_max_UI             ' '20'  ''  ''  '' '',
           'st_UI            '  'st_UI                ' '20'  ''  ''  '' 'c410'.

      WHEN 'STR'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'STR              '  'STR                  ' '7 '  ''  ''  '' '',
            'vlr_min_STR      '  'v_min_STR            ' '20'  ''  ''  '' '',
            'vlr_max_STR      '  'v_max_STR            ' '20'  ''  ''  '' '',
            'st_STR           '  'st_STR               ' '20'  ''  ''  '' 'c410'.

      WHEN 'ELG'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'ELG              '  'ELG                  ' '7 '  ''  ''  '' '',
            'vlr_min_ELG      '  'v_min_ELG            ' '20'  ''  ''  '' '',
            'vlr_max_ELG      '  'v_max_ELG            ' '20'  ''  ''  '' '',
            'st_ELG           '  'st_ELG               ' '20'  ''  ''  '' 'c410'.

      WHEN 'MIC'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'MIC              '  'MIC                  ' '7 '  ''  ''  '' '',
            'vlr_min_MIC      '  'v_min_MIC            ' '20'  ''  ''  '' '',
            'vlr_max_MIC      '  'v_max_MIC            ' '20'  ''  ''  '' '',
            'st_MIC           '  'st_MIC               ' '20'  ''  ''  '' 'c410'.

      WHEN 'RD'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'RD               '  'RD                   ' '7 '  ''  ''  '' '',
            'vlr_min_RD       '  'v_min_RD             ' '20'  ''  ''  '' '',
            'vlr_max_RD       '  'v_max_RD             ' '20'  ''  ''  '' '',
            'st_RD            '  'st_RD                ' '20'  ''  ''  '' 'c410'.

      WHEN '_B'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG             '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR             '  'Material             ' '  '  ''  'X' '' '',
           'NORMT             '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS             '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT             '  'Bloco                ' '10'  ''  ''  '' '',
           'ERSDA             '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            '_B               '  '_+b                  ' '7 '  ''  ''  '' '',
            'vlr_min__B       '  'v_min__B             ' '20'  ''  ''  '' '',
            'vlr_max__B       '  'v_max__B             ' '20'  ''  ''  '' '',
            'st__B            '  'st__B                ' '20'  ''  ''  '' 'c410'.

      WHEN 'CG'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG            '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR            '  'Material             ' '  '  ''  'X' '' '',
           'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
           'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'CG               '  'CG                   ' '7 '  ''  ''  '' '',
            'VLR_MIN_CG       '  'V_MIN_CG             ' '20'  ''  ''  '' '',
            'VLR_MAX_CG       '  'V_MAX_CG             ' '20'  ''  ''  '' '',
            'ST_CG            '  'ST_CG                ' '20'  ''  ''  '' 'C410'.

      WHEN 'T_CNT'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'T_CNT            '  'T.CNT                ' '7 '  ''  ''  '' '',
            'VLR_MIN_T_CNT    '  'V_MIN_T_CNT          ' '20'  ''  ''  '' '',
            'VLR_MAX_T_CNT    '  'V_MAX_T_CNT          ' '20'  ''  ''  '' '',
            'ST_T_CNT         '  'ST_T_CNT             ' '20'  ''  ''  '' 'C410'.

      WHEN 'T_AREA'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'T_AREA           '  'T.AREA               ' '7 '  ''  ''  '' '',
            'VLR_MIN_T_AREA   '  'V_MIN_T_AREA         ' '20'  ''  ''  '' '',
            'VLR_MAX_T_AREA   '  'V_MAX_T_AREA         ' '20'  ''  ''  '' '',
            'ST_T_AREA        '  'ST_T_AREA            ' '20'  ''  ''  '' 'C410'.

      WHEN 'LEAF'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'LEAF             '  'LEAF                 ' '7 '  ''  ''  '' '',
            'VLR_MIN_LEAF     '  'V_MIN_LEAF           ' '20'  ''  ''  '' '',
            'VLR_MAX_LEAF     '  'V_MAX_LEAF           ' '20'  ''  ''  '' '',
            'ST_LEAF          '  'ST_LEAF              ' '20'  ''  ''  '' 'C410'.

      WHEN 'MR'.
        PERFORM  alv_preenche_cat USING:
           'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
           'CHARG            '  'Número               ' '10'  ''  ''  '' '',
           'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
           'MATNR            '  'Material             ' '  '  ''  'X' '' '',
           'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
           'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
           'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
           'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
           'MR               '  'MR                   ' '7 '  ''  ''  '' '',
           'VLR_MIN_MR       '  'V_MIN_MR             ' '20'  ''  ''  '' '',
           'VLR_MAX_MR       '  'V_MAX_MR             ' '20'  ''  ''  '' '',
           'ST_MR            '  'ST_MR                ' '20'  ''  ''  '' 'C410'.

      WHEN 'SFI'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'SFI              '  'SFI(W)               ' '7 '  ''  ''  '' '',
            'VLR_MIN_SFI      '  'V_MIN_SFI            ' '20'  ''  ''  '' '',
            'VLR_MAX_SFI      '  'V_MAX_SFI            ' '20'  ''  ''  '' '',
            'ST_SFI           '  'ST_SFI               ' '20'  ''  ''  '' 'C410'.

      WHEN 'SCI'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'SCI              '  'SCI                  ' '7 '  ''  ''  '' '',
            'VLR_MIN_SCI      '  'V_MIN_SCI            ' '20'  ''  ''  '' '',
            'VLR_MAX_SCI      '  'V_MAX_SCI            ' '20'  ''  ''  '' '',
            'ST_SCI           '  'ST_SCI               ' '20'  ''  ''  '' 'C410'.

      WHEN 'CSP'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'CSP              '  'CSP                  ' '7 '  ''  ''  '' '',
            'VLR_MIN_CSP      '  'V_MIN_CSP            ' '20'  ''  ''  '' '',
            'VLR_MAX_CSP      '  'V_MAX_CSP            ' '20'  ''  ''  '' '',
            'ST_CSP           '  'ST_SCI               ' '20'  ''  ''  '' 'C410'.

      WHEN 'SAFRA'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'SAFRA            '  'Safra                ' '4 '  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'VLR_MIN_SAFRA    '  'V_MIN_SAFRA          ' '20'  ''  ''  '' '',
            'VLR_MAX_SAFRA    '  'V_MAX_SAFRA          ' '20'  ''  ''  '' '',
            'ST_SAFRA         '  'ST_SAFRA             ' '20'  ''  ''  '' 'C410'.

      WHEN 'FARDAO'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'FARDAO           '  'FaRDão               ' '7 '  ''  ''  '' '',
            'VLR_MIN_FARDAO   '  'V_MIN_FARDAO         ' '20'  ''  ''  '' '',
            'VLR_MAX_FARDAO   '  'V_MAX_FARDAO         ' '20'  ''  ''  '' '',
            'ST_FARDAO        '  'ST_FARDAO            ' '20'  ''  ''  '' 'C410'.

      WHEN 'TALHAO'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'TALHAO           '  'Talhão               ' '7 '  ''  ''  '' '',
            'VLR_MIN_TALHAO   '  'V_MIN_TALHAO         ' '20'  ''  ''  '' '',
            'VLR_MAX_TALHAO   '  'V_MAX_TALHAO         ' '20'  ''  ''  '' '',
            'ST_TALHAO        '  'ST_TALHAO            ' '20'  ''  ''  '' 'C410'.

      WHEN 'VARIEDADE'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER           '  'Digito               ' '  '  ''  ''  '' '',
            'CHARG            '  'Número               ' '10'  ''  ''  '' '',
            'CHARG1           '  'Num. Lote            ' '10'  ''  ''  '' '',
            'MATNR            '  'Material             ' '  '  ''  'X' '' '',
            'NORMT            '  'Tipo                 ' '10'  ''  ''  '' '',
            'CLABS            '  'Peso                 ' '10'  ''  ''  '' '',
            'LGORT            '  'Bloco                ' '10'  ''  ''  '' '',
            'ERSDA            '  'Data Lançamento      ' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'VARIEDADE        '  'Variedade            ' '10'  ''  ''  '' '',
            'VLR_MIN_VARIEDADE '  'V_MIN_VARIEDADE     ' '20'  ''  ''  '' '',
            'VLR_MAX_VARIEDADE '  'V_MAX_VARIEDADE     ' '20'  ''  ''  '' '',
            'ST_VARIEDADE      '  'ST_VARIEDADE        ' '20'  ''  ''  '' 'C410'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " CATALOG_ALV

*&---------------------------------------------------------------------*
*&      Form  CATALOG_ALV_RESUMO
*&---------------------------------------------------------------------*
FORM catalog_alv_resumo.
  PERFORM  alv_preenche_cat USING:
           'NORMT'    'Tipo'       '10' '' ''   '' '',
           'MATNR'    'Material'   ' '  '' 'X'  '' '',
           'CLABS'    'Peso'       '25' '' ''   '' '',
           'QTD_F'    'Qtd. Fardo' '10' '' ''   '' ''.
ENDFORM.                    " CATALOG_ALV_RESUMO

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat  USING    p_campo   TYPE c
                                p_desc    TYPE c
                                p_tam     TYPE c
                                p_hot     TYPE c
                                p_zero    TYPE c
                                p_convert TYPE c
                                p_emphasize TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.

  IF     ( todos_mm EQ 'X' ).
    wl_fcat-tabname   = 'IT_SAIDA'.
  ELSEIF ( resu_mm EQ 'X' ).
    wl_fcat-tabname   = 'IT_SAIDA_RESU'.
  ELSEIF ( hist_mm EQ 'X' ).   "ADD - IS - 21.05.2013
    wl_fcat-tabname   = 'IT_SAIDA'.
  ENDIF.

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-convexit  = p_convert.
  wl_fcat-emphasize = p_emphasize.

  gs_layout-excp_conds    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-sel_mode      = 'A'.
  gs_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef    = ''.

  APPEND wl_fcat TO ls_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  SELECAO_ZMMT0027
*&---------------------------------------------------------------------*
*       ADD - IS - 21.05.2013
*----------------------------------------------------------------------*
FORM selecao_zmmt0027.

  IF p_hvi IS NOT INITIAL AND p_fpdr IS NOT INITIAL OR p_dpdr IS NOT INITIAL.


    FIELD-SYMBOLS: <fs_return> TYPE ty_zmme_cl,
                   <fs_mseg>   TYPE ty_mseg.

    SELECT *
      FROM v_mkm_zu_kls_bez
      INTO CORRESPONDING FIELDS OF TABLE it_v_mkm_zu_kls_bez
      WHERE spras EQ 'P'.

    SELECT * FROM zmmt0027
      INTO TABLE it_zmmt0027
    WHERE werks          IN p_werks
      AND matkl          IN p_matkl
      AND matnr          IN p_matnr
      AND safra          IN p_safra
      AND lgort          IN p_lgort
      AND adquirido_terc  = p_fnint. "*-CS2022000332-#82292-26.07.2022-JT-inicio

    LOOP AT it_zmmt0027 INTO wa_zmmt0027.
      MOVE: wa_zmmt0027-matnr TO wa_matnr-matnr,
            wa_zmmt0027-charg TO wa_matnr-charg.
      APPEND wa_matnr TO it_matnr.
    ENDLOOP.

    CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
      TABLES
        t_matnr    = it_matnr
        t_return   = it_return
        t_zmmt0027 = it_zmmt0027  "*-CS2022000332-#82292-26.07.2022-JT-inicio
      EXCEPTIONS                  "*-CS2022000332-#82292-26.07.2022-JT-inicio
        erro4      = 1
        OTHERS     = 2.

    IF it_return[] IS NOT INITIAL.  "*-CS2022000332-#82292-26.07.2022-JT-inicio
      SELECT *
      FROM zppt0014
      INTO TABLE it_zppt0014
      FOR ALL ENTRIES IN it_return
      WHERE atinn EQ it_return-atinn.
    ENDIF.

*  IF P_HVI-LOW IS NOT INITIAL. "AND P_HVI-HIGH IS NOT INITIAL." AND P_CLASS IS NOT INITIAL.
**
**    READ TABLE IT_ZMMT0025 INTO WA_ZMMT0025 WITH KEY ATINN = P_CLASS.
*
*     READ TABLE IT_ZPPT0014 INTO WA_ZPPT0014 WITH KEY ATINN = P_HVI.
*    CASE WA_ZPPT0014-ATNAM.
*      WHEN: 'UHML'.
*        DELETE IT_ZMMT0027 WHERE FAR_UHML     NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_UHML EQ ''.
*      WHEN: 'UI'.
*        DELETE IT_ZMMT0027 WHERE FAR_UI       NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_UI EQ ''.
*      WHEN: 'STR'.
*        DELETE IT_ZMMT0027 WHERE FAR_STR      NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_STR EQ ''.
*      WHEN: 'ELG'.
*        DELETE IT_ZMMT0027 WHERE FAR_ELG      NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_ELG EQ ''.
*      WHEN: 'MIC'.
*        DELETE IT_ZMMT0027 WHERE FAR_MIC      NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_MIC EQ ''.
*      WHEN: 'RD'.
*        DELETE IT_ZMMT0027 WHERE FAR_RD       NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_RD EQ ''.
*      WHEN: '+B'.
*        DELETE IT_ZMMT0027 WHERE FAR_B        NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_B EQ ''.
*      WHEN: 'CG'.
*        DELETE IT_ZMMT0027 WHERE FAR_CG       NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_CG EQ ''.
*      WHEN: 'T.CNT'.
*        DELETE IT_ZMMT0027 WHERE FAR_TCNT     NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_TCNT EQ ''.
*      WHEN: 'T.AREA'.
*        DELETE IT_ZMMT0027 WHERE FAR_TAREA    NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_TAREA EQ ''.
*      WHEN: 'LEAF'.
*        DELETE IT_ZMMT0027 WHERE FAR_LEAF     NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_LEAF EQ ''.
*      WHEN: 'MR'.
*        DELETE IT_ZMMT0027 WHERE FAR_MR       NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_MR EQ ''.
*      WHEN: 'SFI(W)'.
*        DELETE IT_ZMMT0027 WHERE FAR_SFIW     NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_SFIW EQ ''.
*      WHEN: 'SCI'.
*        DELETE IT_ZMMT0027 WHERE FAR_SCI      NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_SCI EQ ''.
*      WHEN: 'CSP'.
*        DELETE IT_ZMMT0027 WHERE FAR_CSP      NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_CSP EQ ''.
**          WHEN: 'SAFRA'.
**            DELETE it_zmmt0027 WHERE safra    NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR safra EQ ''.
*      WHEN: 'TALHAO'.
*        DELETE IT_ZMMT0027 WHERE TALHAO       NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR TALHAO EQ ''.
*      WHEN: 'VARIEDADE'.
*        DELETE IT_ZMMT0027 WHERE VARIEDADE    NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR VARIEDADE EQ ''.
*      WHEN: 'PERIODO'.
*        DELETE IT_ZMMT0027 WHERE FAR_PERIODO  NOT BETWEEN P_HVI-LOW AND P_HVI-HIGH  OR FAR_PERIODO EQ ''.
*    ENDCASE.
*  ENDIF.

*  CHECK NOT IT_ZMMT0027[] IS INITIAL.
  ELSE.
    SELECT * FROM zmmt0027
    INTO TABLE it_zmmt0027
  WHERE werks          IN p_werks
    AND matkl          IN p_matkl
    AND matnr          IN p_matnr
    AND safra          IN p_safra
    AND lgort          IN p_lgort
    AND adquirido_terc  = p_fnint. "*-CS2022000332-#82292-26.07.2022-JT-inicio
  ENDIF.

  IF it_zmmt0027[] IS NOT INITIAL.
    REFRESH: it_zppt0002.
    SELECT * FROM zppt0002
      INTO TABLE it_zppt0002
      FOR ALL ENTRIES IN it_zmmt0027
    WHERE acharg  EQ it_zmmt0027-charg
    AND werks   EQ it_zmmt0027-werks.
  ENDIF.

*-CS2022000332-#78803-05.07.2022-JT-inicio
  IF hist_mm = abap_true AND it_zmmt0027[] IS NOT INITIAL.
    SELECT *
      FROM zmmt0008
      INTO TABLE it_zmmt0008
       FOR ALL ENTRIES IN it_zmmt0027
     WHERE lgort      = it_zmmt0027-lgort
       AND charg      = it_zmmt0027-charg
       AND werks_orig = it_zmmt0027-werks.

    DELETE it_zmmt0008 WHERE werks NOT IN p_wkori[].

    SORT it_zmmt0008 BY lgort charg werks_orig.
    DELETE ADJACENT DUPLICATES FROM it_zmmt0008
                          COMPARING lgort charg werks_orig.

    IF p_wkori[] IS NOT INITIAL.
      LOOP AT it_zmmt0027 INTO wa_zmmt0027.
        DATA(l_tabix) = sy-tabix.
        READ TABLE it_zmmt0008 INTO wa_zmmt0008 WITH KEY lgort      = wa_zmmt0027-lgort
                                                         charg      = wa_zmmt0027-charg
                                                         werks_orig = wa_zmmt0027-werks
                                              BINARY SEARCH.
        IF sy-subrc <> 0.
          DELETE it_zmmt0027 INDEX l_tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
*-CS2022000332-#78803-05.07.2022-JT-fim

  PERFORM: organiza_saida.
ENDFORM.                    " SELECAO_ZMMT0027

*&---------------------------------------------------------------------*
*&      Form  CATALOG_ALV_HISTORICO
*&---------------------------------------------------------------------*
*       ADD - IS - 21.05.2013
*----------------------------------------------------------------------*
FORM catalog_alv_historico.

  IF p_fpdr IS INITIAL AND p_dpdr IS INITIAL.
    PERFORM  alv_preenche_cat USING:
            'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
            'CHARG'     'Número'          '10'  ''  ''  '' '',
            'CHARG1'    'Num. Lote'       '20'  ''  ''  '' '',
            'MATNR'     'Material'        ''    ''  'X' '' '',
            'NORMT'     'Tipo'            '10'  ''  ''  '' '',
            'CLABS'     'Peso'            '10'  ''  ''  '' '',
            'LGORT'     'Bloco'           '10'  ''  ''  '' '',
            'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'UHML'      'UHML'            '7'   ''  ''  '' '',
            'UI'        'UI'              '7'   ''  ''  '' '',
            'STR'       'STR'             '7'   ''  ''  '' '',
            'ELG'       'ELG'             '7'   ''  ''  '' '',
            'MIC'       'MIC'             '7'   ''  ''  '' '',
            'RD'        'RD'              '7'   ''  ''  '' '',
            '_B'        '_+B'             '7'   ''  ''  '' '',
            'CG'        'CG'              '7'   ''  ''  '' '',
            'T_CNT'     'T.CNT'           '7'   ''  ''  '' '',
            'T_AREA'    'T.AREA'          '7'   ''  ''  '' '',
            'LEAF'      'LEAF'            '7'   ''  ''  '' '',
            'MR'        'MR'              '7'   ''  ''  '' '',
            'SFI'       'SFI(W)'          '7'   ''  ''  '' '',
            'SCI'       'SCI'             '7'   ''  ''  '' '',
            'CSP'       'CSP'             '7'   ''  ''  '' '',
            'SAFRA'     'Safra'           '4'   ''  ''  '' '',   "ADD - IS - 21.05.2013
            'FARDAO'    'Fardão'          '7'   ''  ''  '' '',
            'TALHAO'    'Talhão'          '7'   ''  ''  '' '',
            'VARIEDADE' 'Variedade'       '10'  ''  ''  '' '',
            'PERIODO'   'Periodo'         '10'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
            'STATUS'    'Status'          '10'  ''  ''  '' ''.   "ADD - IS - 21.05.2013
*          'RESERV'    'Reservado'       '10'  ''  ''  '' ''.

  ELSEIF p_hvi-low IS NOT INITIAL AND p_fpdr IS INITIAL AND p_dpdr IS INITIAL.
    PERFORM  alv_preenche_cat USING:

          'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
          'CHARG'     'Número'          '10'  ''  ''  '' '',
          'MATNR'     'Material'        ''    ''  'X' '' '',
          'NORMT'     'Tipo'            '10'  ''  ''  '' '',
          'CLABS'     'Peso'            '10'  ''  ''  '' '',
          'LGORT'     'Bloco'           '10'  ''  ''  '' '',
          'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
          'UHML'      'UHML'            '7'   ''  ''  '' '',
          'UI'        'UI'              '7'   ''  ''  '' '',
          'STR'       'STR'             '7'   ''  ''  '' '',
          'ELG'       'ELG'             '7'   ''  ''  '' '',
          'MIC'       'MIC'             '7'   ''  ''  '' '',
          'RD'        'RD'              '7'   ''  ''  '' '',
          '_B'        '_+B'             '7'   ''  ''  '' '',
          'CG'        'CG'              '7'   ''  ''  '' '',
          'T_CNT'     'T.CNT'           '7'   ''  ''  '' '',
          'T_AREA'    'T.AREA'          '7'   ''  ''  '' '',
          'LEAF'      'LEAF'            '7'   ''  ''  '' '',
          'MR'        'MR'              '7'   ''  ''  '' '',
          'SFI'       'SFI(W)'          '7'   ''  ''  '' '',
          'SCI'       'SCI'             '7'   ''  ''  '' '',
          'CSP'       'CSP'             '7'   ''  ''  '' '',
          'SAFRA'     'Safra'           '4'   ''  ''  '' '',   "ADD - IS - 21.05.2013
          'FARDAO'    'Fardão'          '7'   ''  ''  '' '',
          'TALHAO'    'Talhão'          '7'   ''  ''  '' '',
          'VARIEDADE' 'Variedade'       '10'  ''  ''  '' '',
          'PERIODO'   'Periodo'         '10'  ''  ''  '' '',   "ADD - IS - 21.05.2013
          'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
          'STATUS'    'Status'          '10'  ''  ''  '' ''.   "ADD - IS - 21.05.2013
*          'RESERV'    'Reservado'       '10'  ''  ''  '' ''.


  ELSEIF p_hvi-low IS NOT INITIAL AND p_fpdr IS NOT INITIAL OR p_dpdr IS NOT INITIAL.
    CASE p_hvi-low.
      WHEN 'UHML'.
        PERFORM  alv_preenche_cat USING:
          'ZNUMER           '     'Digito         ' '10'  ''  ''  '' '',
          'CHARG            '     'Número         ' '10'  ''  ''  '' '',
          'MATNR            '     'Material       ' '  '  ''  'X' '' '',
          'NORMT            '     'Tipo           ' '10'  ''  ''  '' '',
          'CLABS            '     'Peso           ' '10'  ''  ''  '' '',
          'LGORT            '     'Bloco          ' '10'  ''  ''  '' '',
          'ERSDA            '     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
          'UHML             '     'UHML           ' '7 '  ''  ''  '' '',
          'VLR_MIN_UHML     '     'V_MIN_UHML     ' '20'  ''  ''  '' '',
          'VLR_MAX_UHML     '     'V_MAX_UHML     ' '20'  ''  ''  '' '',
          'WERKS_ORIG'            'Centro Fatur.'   '15'  ''  ''  '' '',
          'ST_UHML          '     'ST_UHML        ' '20'  ''  ''  '' 'C410'.

      WHEN 'UI'.
        PERFORM  alv_preenche_cat USING:

        'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
        'CHARG'     'Número'          '10'  ''  ''  '' '',
        'MATNR'     'Material'        ''    ''  'X' '' '',
        'NORMT'     'Tipo'            '10'  ''  ''  '' '',
        'CLABS'     'Peso'            '10'  ''  ''  '' '',
        'LGORT'     'Bloco'           '10'  ''  ''  '' '',
        'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
        'UI         '  'UI'              '7'   ''  ''  '' '',
        'VLR_MIN_UI '  'V_MIN_UI     ' '20'  ''  ' '  '' '    ',
        'VLR_MAX_UI '  'V_MAX_UI     ' '20'  ''  ' '  '' '    ',
        'WERKS_ORIG'   'Centro Fatur.'   '15'  ''  ''  '' '',
        'ST_UI      '  'ST_UI        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'STR'.
        PERFORM  alv_preenche_cat USING:
     'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
     'CHARG'     'Número'          '10'  ''  ''  '' '',
     'MATNR'     'Material'        ''    ''  'X' '' '',
     'NORMT'     'Tipo'            '10'  ''  ''  '' '',
     'CLABS'     'Peso'            '10'  ''  ''  '' '',
     'LGORT'     'Bloco'           '10'  ''  ''  '' '',
     'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
     'STR'       'STR'             '7'   ''  ''  '' '',
     'VLR_MIN_STR '  'V_MIN_STR     ' '20'  ''  ' '  '' '    ',
     'VLR_MAX_STR '  'V_MAX_STR     ' '20'  ''  ' '  '' '    ',
     'WERKS_ORIG'   'Centro Fatur.'   '15'  ''  ''  '' '',
     'ST_STR      '  'ST_STR        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'ELG'.
        PERFORM  alv_preenche_cat USING:
             'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
             'CHARG'     'Número'          '10'  ''  ''  '' '',
             'MATNR'     'Material'        ''    ''  'X' '' '',
             'NORMT'     'Tipo'            '10'  ''  ''  '' '',
             'CLABS'     'Peso'            '10'  ''  ''  '' '',
             'LGORT'     'Bloco'           '10'  ''  ''  '' '',
             'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
             'ELG'       'ELG'             '7'   ''  ''  '' '',
             'VLR_MIN_ELG '  'V_MIN_ELG     ' '20'  ''  ' '  '' '    ',
             'VLR_MAX_ELG '  'V_MAX_ELG     ' '20'  ''  ' '  '' '    ',
             'WERKS_ORIG'   'Centro Fatur.'   '15'  ''  ''  '' '',
             'ST_ELG      '  'ST_ELG        ' '20'  ''  ' '  '' 'C410'.


      WHEN 'MIC'.
        PERFORM  alv_preenche_cat USING:
             'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
             'CHARG'     'Número'          '10'  ''  ''  '' '',
             'MATNR'     'Material'        ''    ''  'X' '' '',
             'NORMT'     'Tipo'            '10'  ''  ''  '' '',
             'CLABS'     'Peso'            '10'  ''  ''  '' '',
             'LGORT'     'Bloco'           '10'  ''  ''  '' '',
             'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
             'MIC'       'MIC'             '7'   ''  ''  '' '',
             'VLR_MIN_MIC '  'V_MIN_MIC     ' '20'  ''  ' '  '' '    ',
             'VLR_MAX_MIC '  'V_MAX_MIC     ' '20'  ''  ' '  '' '    ',
             'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
             'ST_MIC      '  'ST_MIC        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'RD'.
        PERFORM  alv_preenche_cat USING:
           'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
           'CHARG'     'Número'          '10'  ''  ''  '' '',
           'MATNR'     'Material'        ''    ''  'X' '' '',
           'NORMT'     'Tipo'            '10'  ''  ''  '' '',
           'CLABS'     'Peso'            '10'  ''  ''  '' '',
           'LGORT'     'Bloco'           '10'  ''  ''  '' '',
           'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
           'RD'        'RD'              '7'   ''  ''  '' '',
           'VLR_MIN_RD '  'V_MIN_RD     ' '20'  ''  ' '  '' '    ',
           'VLR_MAX_RD '  'V_MAX_RD     ' '20'  ''  ' '  '' '    ',
           'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
           'ST_RD      '  'ST_RD        ' '20'  ''  ' '  '' 'C410'.

      WHEN '_B'.
        PERFORM  alv_preenche_cat USING:
           'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
           'CHARG'     'Número'          '10'  ''  ''  '' '',
           'MATNR'     'Material'        ''    ''  'X' '' '',
           'NORMT'     'Tipo'            '10'  ''  ''  '' '',
           'CLABS'     'Peso'            '10'  ''  ''  '' '',
           'LGORT'     'Bloco'           '10'  ''  ''  '' '',
           'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
           '_B'        '_+B'             '7'   ''  ''  '' '',
           'VLR_MIN__B '  'V_MIN__B     ' '20'  ''  ' '  '' '    ',
           'VLR_MAX__B '  'V_MAX__B     ' '20'  ''  ' '  '' '    ',
           'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
           'ST__B      '  'ST__B        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'CG'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
            'CHARG'     'Número'          '10'  ''  ''  '' '',
            'MATNR'     'Material'        ''    ''  'X' '' '',
            'NORMT'     'Tipo'            '10'  ''  ''  '' '',
            'CLABS'     'Peso'            '10'  ''  ''  '' '',
            'LGORT'     'Bloco'           '10'  ''  ''  '' '',
            'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'CG'        'CG'              '7'   ''  ''  '' '',
            'VLR_MIN_CG '  'V_MIN_CG     ' '20'  ''  ' '  '' '    ',
            'VLR_MAX_CG '  'V_MAX_CG     ' '20'  ''  ' '  '' '    ',
            'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
            'ST_CG      '  'ST_CG        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'T_CNT'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
            'CHARG'     'Número'          '10'  ''  ''  '' '',
            'MATNR'     'Material'        ''    ''  'X' '' '',
            'NORMT'     'Tipo'            '10'  ''  ''  '' '',
            'CLABS'     'Peso'            '10'  ''  ''  '' '',
            'LGORT'     'Bloco'           '10'  ''  ''  '' '',
            'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'T_CNT'     'T.CNT'           '7'   ''  ''  '' '',
            'VLR_MIN_CNT '  'V_MIN_CNT     ' '20'  ''  ' '  '' '    ',
            'VLR_MAX_CNT '  'V_MAX_CNT     ' '20'  ''  ' '  '' '    ',
            'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
            'ST_CNT      '  'ST_CNT        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'T_AREA'.
        PERFORM  alv_preenche_cat USING:
             'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
             'CHARG'     'Número'          '10'  ''  ''  '' '',
             'MATNR'     'Material'        ''    ''  'X' '' '',
             'NORMT'     'Tipo'            '10'  ''  ''  '' '',
             'CLABS'     'Peso'            '10'  ''  ''  '' '',
             'LGORT'     'Bloco'           '10'  ''  ''  '' '',
             'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
             'T_AREA'    'T.AREA'          '7'   ''  ''  '' '',
             'VLR_MIN_T_AREA '  'V_MIN_T_AREA     ' '20'  ''  ' '  '' '    ',
             'VLR_MAX_T_AREA '  'V_MAX_T_AREA     ' '20'  ''  ' '  '' '    ',
             'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
             'ST_T_AREA      '  'ST_T_AREA        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'LEAF'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
            'CHARG'     'Número'          '10'  ''  ''  '' '',
            'MATNR'     'Material'        ''    ''  'X' '' '',
            'NORMT'     'Tipo'            '10'  ''  ''  '' '',
            'CLABS'     'Peso'            '10'  ''  ''  '' '',
            'LGORT'     'Bloco'           '10'  ''  ''  '' '',
            'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'LEAF'      'LEAF'            '7'   ''  ''  '' '',
            'VLR_MIN_LEAF '  'V_MIN_LEAF     ' '20'  ''  ' '  '' '    ',
            'VLR_MAX_LEAF '  'V_MAX_LEAF     ' '20'  ''  ' '  '' '    ',
            'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
            'ST_LEAF      '  'ST_LEAF        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'MR'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
            'CHARG'     'Número'          '10'  ''  ''  '' '',
            'MATNR'     'Material'        ''    ''  'X' '' '',
            'NORMT'     'Tipo'            '10'  ''  ''  '' '',
            'CLABS'     'Peso'            '10'  ''  ''  '' '',
            'LGORT'     'Bloco'           '10'  ''  ''  '' '',
            'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'MR'        'MR'              '7'   ''  ''  '' '',
            'VLR_MIN_MR '  'V_MIN_MR     ' '20'  ''  ' '  '' '    ',
            'VLR_MAX_MR '  'V_MAX_MR     ' '20'  ''  ' '  '' '    ',
            'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
            'ST_MR      '  'ST_MR        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'SFI'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
            'CHARG'     'Número'          '10'  ''  ''  '' '',
            'MATNR'     'Material'        ''    ''  'X' '' '',
            'NORMT'     'Tipo'            '10'  ''  ''  '' '',
            'CLABS'     'Peso'            '10'  ''  ''  '' '',
            'LGORT'     'Bloco'           '10'  ''  ''  '' '',
            'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'SFI'       'SFI(W)'          '7'   ''  ''  '' '',
            'VLR_MIN_SFI '  'V_MIN_SFI     ' '20'  ''  ' '  '' '    ',
            'VLR_MAX_SFI '  'V_MAX_SFI     ' '20'  ''  ' '  '' '    ',
            'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
            'ST_SFI      '  'ST_SFI        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'SCI'.
        PERFORM  alv_preenche_cat USING:
           'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
           'CHARG'     'Número'          '10'  ''  ''  '' '',
           'MATNR'     'Material'        ''    ''  'X' '' '',
           'NORMT'     'Tipo'            '10'  ''  ''  '' '',
           'CLABS'     'Peso'            '10'  ''  ''  '' '',
           'LGORT'     'Bloco'           '10'  ''  ''  '' '',
           'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
           'SCI'       'SCI'             '7'   ''  ''  '' '',
           'VLR_MIN_SCI '  'V_MIN_SCI     ' '20'  ''  ' '  '' '    ',
           'VLR_MAX_SCI '  'V_MAX_SCI     ' '20'  ''  ' '  '' '    ',
           'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
           'ST_SCI      '  'ST_SCI        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'CSP'.
        PERFORM  alv_preenche_cat USING:
           'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
           'CHARG'     'Número'          '10'  ''  ''  '' '',
           'MATNR'     'Material'        ''    ''  'X' '' '',
           'NORMT'     'Tipo'            '10'  ''  ''  '' '',
           'CLABS'     'Peso'            '10'  ''  ''  '' '',
           'LGORT'     'Bloco'           '10'  ''  ''  '' '',
           'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
           'CSP'       'CSP'             '7'   ''  ''  '' '',
           'VLR_MIN_CSP '  'V_MIN_CSP     ' '20'  ''  ' '  '' '    ',
           'VLR_MAX_CSP '  'V_MAX_CSP     ' '20'  ''  ' '  '' '    ',
           'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
           'ST_CSP      '  'ST_CSP        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'SAFRA'.
        PERFORM  alv_preenche_cat USING:
           'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
           'CHARG'     'Número'          '10'  ''  ''  '' '',
           'MATNR'     'Material'        ''    ''  'X' '' '',
           'NORMT'     'Tipo'            '10'  ''  ''  '' '',
           'CLABS'     'Peso'            '10'  ''  ''  '' '',
           'LGORT'     'Bloco'           '10'  ''  ''  '' '',
           'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
           'SAFRA'     'Safra'           '4'   ''  ''  '' '',   "ADD - IS - 21.05.2013
           'VLR_MIN_SAFRA '  'V_MIN_SAFRA     ' '20'  ''  ' '  '' '    ',
           'VLR_MAX_SAFRA '  'V_MAX_SAFRA     ' '20'  ''  ' '  '' '    ',
           'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
           'ST_SAFRA      '  'ST_SAFRA        ' '20'  ''  ' '  '' 'C410'.


      WHEN 'FARDAO'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
            'CHARG'     'Número'          '10'  ''  ''  '' '',
            'MATNR'     'Material'        ''    ''  'X' '' '',
            'NORMT'     'Tipo'            '10'  ''  ''  '' '',
            'CLABS'     'Peso'            '10'  ''  ''  '' '',
            'LGORT'     'Bloco'           '10'  ''  ''  '' '',
            'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'FARDAO'    'Fardão'          '7'   ''  ''  '' '',
            'VLR_MIN_FARDAO '  'V_MIN_FARDAO     ' '20'  ''  ' '  '' '    ',
            'VLR_MAX_FARDAO '  'V_MAX_FARDAO     ' '20'  ''  ' '  '' '    ',
            'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
            'ST_FARDAO      '  'ST_FARDAO        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'TALHAO'.
        PERFORM  alv_preenche_cat USING:
             'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
             'CHARG'     'Número'          '10'  ''  ''  '' '',
             'MATNR'     'Material'        ''    ''  'X' '' '',
             'NORMT'     'Tipo'            '10'  ''  ''  '' '',
             'CLABS'     'Peso'            '10'  ''  ''  '' '',
             'LGORT'     'Bloco'           '10'  ''  ''  '' '',
             'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
             'TALHAO'    'Talhão'          '7'   ''  ''  '' '',
             'VLR_MIN_TALHAO '  'V_MIN_TALHAO     ' '20'  ''  ' '  '' '    ',
             'VLR_MAX_TALHAO '  'V_MAX_TALHAO     ' '20'  ''  ' '  '' '    ',
             'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
             'ST_TALHAO      '  'ST_TALHAO        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'VARIEDADE'.
        PERFORM  alv_preenche_cat USING:
             'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
             'CHARG'     'Número'          '10'  ''  ''  '' '',
             'MATNR'     'Material'        ''    ''  'X' '' '',
             'NORMT'     'Tipo'            '10'  ''  ''  '' '',
             'CLABS'     'Peso'            '10'  ''  ''  '' '',
             'LGORT'     'Bloco'           '10'  ''  ''  '' '',
             'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
             'VARIEDADE' 'Variedade'       '10'  ''  ''  '' '',
             'VLR_MIN_VARIEDADE '  'V_MIN_VARIEDADE     ' '20'  ''  ' '  '' '    ',
             'VLR_MAX_VARIEDADE '  'V_MAX_VARIEDADE     ' '20'  ''  ' '  '' '    ',
             'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
             'ST_VARIEDADE      '  'ST_VARIEDADE        ' '20'  ''  ' '  '' 'C410'.

      WHEN 'PERIODO'.
        PERFORM  alv_preenche_cat USING:
            'ZNUMER'    'Digito'          '10'  ''  ''  '' '',
            'CHARG'     'Número'          '10'  ''  ''  '' '',
            'MATNR'     'Material'        ''    ''  'X' '' '',
            'NORMT'     'Tipo'            '10'  ''  ''  '' '',
            'CLABS'     'Peso'            '10'  ''  ''  '' '',
            'LGORT'     'Bloco'           '10'  ''  ''  '' '',
            'ERSDA'     'Data Lançamento' '15'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'PERIODO'   'Periodo'         '10'  ''  ''  '' '',   "ADD - IS - 21.05.2013
            'VLR_MIN_PERIODO '  'V_MIN_PERIODO     ' '20'  ''  ' '  '' '    ',
            'VLR_MAX_PERIODO '  'V_MAX_PERIODO     ' '20'  ''  ' '  '' '    ',
            'WERKS_ORIG' 'Centro Fatur.'  '15'  ''  ''  '' '',
            'ST_PERIODO      '  'ST_PERIODO        ' '20'  ''  ' '  '' 'C410'.
    ENDCASE.
  ENDIF.
ENDFORM.                    " CATALOG_ALV_HISTORICOENDFORM.                    " CATALOG_ALV_HISTORICO

FORM f4_busca_caracts.


ENDFORM.
