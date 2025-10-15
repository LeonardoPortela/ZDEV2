*&---------------------------------------------------------------------*
*& Report  ZFIR0047
*&---------------------------------------------------------------------*
*&TITULO: Encerramento de Contas de resultado
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 26.03.2014
*TRANSACAO: ZFI0049
*&---------------------------------------------------------------------*

REPORT  zfir0047.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.


*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: faglflext.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_zib_contabil.
         INCLUDE STRUCTURE zib_contabil.
TYPES:   mark   TYPE c,
         data   TYPE sy-datum,
         del(1),
       END OF ty_zib_contabil.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES:
  BEGIN OF ty_t882g,
    rbukrs TYPE t882g-rbukrs,
    curr1  TYPE t882g-curr1,
    curr2  TYPE t882g-curr2,
    curr3  TYPE t882g-curr3,
  END OF ty_t882g,

  BEGIN OF ty_skb1,
    bukrs TYPE skb1-bukrs,
    saknr TYPE skb1-saknr,
    fstag TYPE skb1-fstag,
    xintb TYPE skb1-xintb,
    mitkz TYPE skb1-mitkz,
  END OF ty_skb1,

  BEGIN OF ty_ska1,
    saknr TYPE ska1-saknr,
    txt50 TYPE skat-txt50,
  END OF ty_ska1,

  BEGIN OF ty_saida,
    mark(1),
    icon(4),
    racct         TYPE faglflext-racct,
    txt50         TYPE skat-txt50,
    mitkz         TYPE skb1-mitkz,
    xintb         TYPE skb1-xintb,
    xsaldo_mi     TYPE faglflext-ksl16,
    xsaldo_mi2    TYPE faglflext-ksl16,
    xsaldo_mi3    TYPE faglflext-ksl16,
    kostl         TYPE zfit0072-kostl,
    prctr         TYPE zfit0072-prctr,
    aufnr         TYPE zfit0072-aufnr,
    obj_key       TYPE zib_contabil-obj_key,
    belnr         TYPE zib_contabil_chv-belnr,
    bukrs         TYPE zib_contabil-bukrs,
    gjahr         TYPE zib_contabil-gjahr,
    rldnr         TYPE faglflext-rldnr,
    line_color(4) TYPE c, "Used to store row color attributes
  END OF ty_saida.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: it_zfit0072         TYPE TABLE OF zfit0072,
      it_t882g            TYPE TABLE OF ty_t882g,
      it_skb1             TYPE TABLE OF ty_skb1,
      it_ska1             TYPE TABLE OF ty_ska1,
      it_faglflext        TYPE TABLE OF faglflext,
      it_faglflext50      TYPE TABLE OF faglflext,
      it_zib_contabil     TYPE TABLE OF ty_zib_contabil,
      it_zib_contabil_err TYPE TABLE OF zib_contabil_err,
      it_zib_contabil_chv TYPE TABLE OF zib_contabil_chv,
      it_saida_aux        TYPE TABLE OF ty_saida,
      it_saida_aux2       TYPE TABLE OF ty_saida,
      it_saida            TYPE TABLE OF ty_saida.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*

DATA: wa_zfit0072         TYPE zfit0072,
      wa_t882g            TYPE ty_t882g,
      wa_skb1             TYPE ty_skb1,
      wa_ska1             TYPE ty_ska1,
      wa_faglflext        TYPE faglflext,
      wa_zib_contabil     TYPE ty_zib_contabil,
      wa_zib_contabil_err TYPE zib_contabil_err,
      wa_zib_contabil_chv TYPE zib_contabil_chv,
      wa_saida            TYPE ty_saida,
      wa_saida_aux        TYPE ty_saida,
      wa_saida_aux2       TYPE ty_saida.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

DATA: ti_bdcdata       TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata       LIKE LINE OF ti_bdcdata,
      t_messtab        TYPE TABLE OF bdcmsgcoll,
      vobj_key         TYPE zib_contabil_err-obj_key,
      wl_message       TYPE pmst_raw_message,
      wg_documento(10),
      wl_mode(1).

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  it_fcat    TYPE TABLE OF ty_estrutura,
  s_variant  TYPE disvariant           , " Tabela Estrutura co
  t_top      TYPE slis_t_listheader,
  xs_events  TYPE slis_alv_event,
  events     TYPE slis_t_event,
  gd_layout  TYPE slis_layout_alv,
  t_print    TYPE slis_print_alv,
  v_report   LIKE sy-repid,
  t_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE,
  it_setleaf LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
  estrutura  TYPE TABLE OF ty_estrutura,
  vg_i       TYPE i.


************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
*Class definition for ALV toolbar
*CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.

DATA: editcontainer        TYPE REF TO cl_gui_custom_container,
      cl_container         TYPE REF TO cl_gui_custom_container,
      obg_conteiner_err    TYPE REF TO cl_gui_custom_container,
      editor               TYPE REF TO cl_gui_textedit,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      cl_container_05      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      g_cc_err             TYPE scrfname VALUE 'CC_LOG',

      cl_grid              TYPE REF TO cl_gui_alv_grid,

      grid3                TYPE REF TO cl_gui_alv_grid,
      t_fieldcatalog       TYPE lvc_t_fcat,
      w_fieldcatalog       TYPE lvc_s_fcat,

      wa_stable            TYPE lvc_s_stbl,
      wa_afield            TYPE lvc_s_fcat,
      it_fieldcat          TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      i_sort               TYPE lvc_t_sort,
      wa_layout            TYPE lvc_s_layo,
      is_stable            TYPE lvc_s_stbl VALUE 'XX',
      wg_repname           LIKE sy-repid,
      wg_x_variant         LIKE disvariant,
      gs_variant_c         TYPE disvariant,
      wg_exit(1)           TYPE c,
      wg_save(1)           TYPE c,
      wg_variant           LIKE disvariant,

      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      obg_conteiner_cta    TYPE REF TO cl_gui_custom_container.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: ok-code      TYPE sy-ucomm,
      wg_gerar(30),
      wg_elim(30),
      tab_lines    LIKE sy-tabix.

DATA:
  et_index_rows TYPE lvc_t_row,
  et_row_no     TYPE lvc_t_roid,
  wa_row_no     LIKE LINE OF et_row_no.


************************************************************************
* D E F I N I T I O N
************************************************************************
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      catch_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION
************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD catch_hotspot.
    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      IF e_column_id =  'ICON' AND wa_saida-icon = icon_incomplete.
        SELECT *
        FROM zib_contabil_err
        INTO TABLE it_zib_contabil_err
          WHERE obj_key   = wa_saida-obj_key.

        CALL SCREEN 0200 STARTING AT 025 3
                          ENDING   AT 210 10.
      ELSEIF e_column_id = 'BELNR' AND wa_saida-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.


  ENDMETHOD.                    "CATCH_HOTSPOT




ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA: event_receiver   TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS    : p_bukrs TYPE faglflext-rbukrs  OBLIGATORY,
                  p_ryear TYPE faglflext-ryear OBLIGATORY,
                  p_rldnr TYPE faglflext-rldnr OBLIGATORY DEFAULT '0L'.

  SELECT-OPTIONS: p_racct FOR faglflext-racct.

SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
          f_seleciona_dados, " Form seleciona dados
          f_saida, " Form de saida
          f_imprime_dados.


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
  DATA xachou(1).

  DATA: e_x001 LIKE x001,
        w_t001 TYPE t001.

*  SELECT SINGLE RBUKRS CURR1 CURR2 CURR3
*    FROM T882G
*    INTO WA_T882G
*    WHERE RBUKRS = P_BUKRS
*    AND   RLDNR  = '50'.

  SELECT SINGLE * INTO w_t001 FROM t001 WHERE bukrs = p_bukrs.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs                = p_bukrs
    IMPORTING
      e_x001                 = e_x001
    EXCEPTIONS
      currency_2_not_defined = 1
      currency_3_not_defined = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  wa_t882g-rbukrs = e_x001-bukrs.
  wa_t882g-curr1  = w_t001-waers.
  wa_t882g-curr2  = e_x001-hwae2.
  wa_t882g-curr3  = e_x001-hwae3.

  SELECT *
    FROM zfit0072
    INTO TABLE it_zfit0072
    WHERE empresa = p_bukrs.


  CHECK it_zfit0072[] IS NOT INITIAL.

  LOOP AT it_zfit0072 INTO wa_zfit0072.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zfit0072-conta_de
      IMPORTING
        output = wa_zfit0072-conta_de.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zfit0072-conta_ate
      IMPORTING
        output = wa_zfit0072-conta_ate.
    LOOP AT p_racct.
      IF p_racct-option EQ 'BT'.
        wa_zfit0072-mandt = '999'.
        EXIT.
      ELSE.
        IF wa_zfit0072-conta_ate IS INITIAL.
          IF  (  p_racct-low  EQ wa_zfit0072-conta_de  ) .
            wa_zfit0072-mandt = '999'.
            EXIT.
          ENDIF.
        ENDIF.
        IF  ( p_racct-low BETWEEN wa_zfit0072-conta_de AND wa_zfit0072-conta_ate ).
          wa_zfit0072-mandt = '999'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    MODIFY  it_zfit0072 FROM wa_zfit0072 INDEX sy-tabix TRANSPORTING conta_de conta_ate mandt.
  ENDLOOP.

  IF p_racct[] IS NOT INITIAL.
    DELETE it_zfit0072 WHERE           mandt NE '999'.
  ENDIF.

  CHECK it_zfit0072[] IS NOT INITIAL.

  SELECT *
   FROM zfit0072
   APPENDING TABLE it_zfit0072
   WHERE empresa = p_bukrs
   AND   cta_partida = 'X'.

  SELECT *
    FROM faglflext
    INTO TABLE it_faglflext
   FOR ALL ENTRIES IN it_zfit0072
   WHERE ryear  EQ p_ryear
   AND   rbukrs	EQ p_bukrs
   AND   racct  GE it_zfit0072-conta_de
   AND   racct  LE it_zfit0072-conta_ate
   AND   rldnr  = p_rldnr.

*  SELECT *
*    FROM FAGLFLEXT
*    INTO TABLE IT_FAGLFLEXT50
*   FOR ALL ENTRIES IN IT_ZFIT0072
*   WHERE RYEAR  EQ P_RYEAR
*   AND   RBUKRS  EQ P_BUKRS
*   AND   RACCT  GE IT_ZFIT0072-CONTA_DE
*   AND   RACCT  LE IT_ZFIT0072-CONTA_ATE
*   AND   RLDNR  = '50'.

  DELETE it_faglflext   WHERE racct NOT IN p_racct.
  DELETE it_faglflext50 WHERE racct NOT IN p_racct.

  IF it_faglflext[] IS NOT INITIAL.
    SELECT bukrs saknr fstag xintb mitkz "#EC CI_DB_OPERATION_OK[2431747]
      FROM skb1
      INTO TABLE it_skb1
      FOR ALL ENTRIES IN it_faglflext
     WHERE bukrs    EQ p_bukrs
     AND   saknr    EQ it_faglflext-racct.

    SELECT  saknr txt50
      FROM skat
      INTO TABLE it_ska1
      FOR ALL ENTRIES IN it_faglflext
      WHERE ktopl EQ  '0050'
      AND   spras EQ sy-langu
      AND   saknr GE it_faglflext-racct.
  ENDIF.

  CHECK it_faglflext50[] IS NOT INITIAL.

  SELECT bukrs saknr fstag xintb mitkz "#EC CI_DB_OPERATION_OK[2431747]
    FROM skb1
    APPENDING TABLE it_skb1
    FOR ALL ENTRIES IN it_faglflext50
   WHERE bukrs    EQ p_bukrs
   AND   saknr    EQ it_faglflext50-racct.


  SELECT  saknr txt50
    FROM skat
    APPENDING TABLE it_ska1
    FOR ALL ENTRIES IN it_faglflext50
    WHERE ktopl EQ  '0050'
    AND   spras EQ sy-langu
    AND   saknr GE it_faglflext50-racct.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .

  DATA: xsaldo_mi  TYPE faglflext-ksl16,
        xsaldo_mi2 TYPE faglflext-ksl16,
        xsaldo_mi3 TYPE faglflext-ksl16.

  SORT: it_ska1     BY saknr,
        it_skb1     BY saknr,
        it_zfit0072 BY conta_de.


  LOOP AT it_faglflext INTO wa_faglflext.
    wa_saida-racct      = wa_faglflext-racct.
    wa_saida-xsaldo_mi  = wa_faglflext-hslvt +
                          wa_faglflext-hsl01 +
                          wa_faglflext-hsl02 +
                          wa_faglflext-hsl03 +
                          wa_faglflext-hsl04 +
                          wa_faglflext-hsl05 +
                          wa_faglflext-hsl06 +
                          wa_faglflext-hsl07 +
                          wa_faglflext-hsl08 +
                          wa_faglflext-hsl09 +
                          wa_faglflext-hsl10 +
                          wa_faglflext-hsl11 +
                          wa_faglflext-hsl12 +
                          wa_faglflext-hsl13 +
                          wa_faglflext-hsl14 +
                          wa_faglflext-hsl15 +
                          wa_faglflext-hsl16.

    wa_saida-xsaldo_mi2 = wa_faglflext-kslvt +
                          wa_faglflext-ksl01 +
                          wa_faglflext-ksl02 +
                          wa_faglflext-ksl03 +
                          wa_faglflext-ksl04 +
                          wa_faglflext-ksl05 +
                          wa_faglflext-ksl06 +
                          wa_faglflext-ksl07 +
                          wa_faglflext-ksl08 +
                          wa_faglflext-ksl09 +
                          wa_faglflext-ksl10 +
                          wa_faglflext-ksl11 +
                          wa_faglflext-ksl12 +
                          wa_faglflext-ksl13 +
                          wa_faglflext-ksl14 +
                          wa_faglflext-ksl15 +
                          wa_faglflext-ksl16.

    wa_saida-xsaldo_mi3 = wa_faglflext-oslvt +
                          wa_faglflext-osl01 +
                          wa_faglflext-osl02 +
                          wa_faglflext-osl03 +
                          wa_faglflext-osl04 +
                          wa_faglflext-osl05 +
                          wa_faglflext-osl06 +
                          wa_faglflext-osl07 +
                          wa_faglflext-osl08 +
                          wa_faglflext-osl09 +
                          wa_faglflext-osl10 +
                          wa_faglflext-osl11 +
                          wa_faglflext-osl12 +
                          wa_faglflext-osl13 +
                          wa_faglflext-osl14 +
                          wa_faglflext-osl15 +
                          wa_faglflext-osl16 .


    READ TABLE it_ska1 INTO wa_ska1 WITH KEY saknr = wa_faglflext-racct BINARY SEARCH.
    wa_saida-txt50      = wa_ska1-txt50.

    READ TABLE it_skb1 INTO wa_skb1 WITH KEY saknr = wa_faglflext-racct BINARY SEARCH.
    wa_saida-mitkz      = wa_skb1-mitkz.
    wa_saida-xintb      = wa_skb1-xintb.

    LOOP AT it_zfit0072 INTO wa_zfit0072.
      IF wa_faglflext-racct GE wa_zfit0072-conta_de AND
         wa_faglflext-racct LE wa_zfit0072-conta_ate.
        EXIT.
      ENDIF.
    ENDLOOP.
    wa_saida-kostl      = wa_zfit0072-kostl.
    wa_saida-prctr      = wa_zfit0072-prctr.
    wa_saida-aufnr      = wa_zfit0072-aufnr.
    wa_saida-rldnr      = p_rldnr.

    IF wa_skb1-xintb = 'X'.
      wa_saida-line_color  = 'C310'.
    ENDIF.

    APPEND wa_saida TO it_saida_aux.
    CLEAR wa_saida.
  ENDLOOP.

  it_saida_aux2[] = it_saida_aux[].

  SORT: it_saida_aux  BY racct,
        it_saida_aux2 BY racct.

  LOOP AT it_saida_aux INTO wa_saida_aux.
    IF wa_saida_aux-racct = wa_saida_aux2-racct.
      CONTINUE.
    ENDIF.
    xsaldo_mi   = 0.
    xsaldo_mi2  = 0.
    xsaldo_mi3  = 0.
    LOOP AT it_saida_aux2 INTO wa_saida_aux2 WHERE racct = wa_saida_aux-racct.
      ADD wa_saida_aux2-xsaldo_mi  TO xsaldo_mi.
      ADD wa_saida_aux2-xsaldo_mi2 TO xsaldo_mi2.
      ADD wa_saida_aux2-xsaldo_mi3 TO xsaldo_mi3.
    ENDLOOP.
    wa_saida_aux-xsaldo_mi  = xsaldo_mi.
    wa_saida_aux-xsaldo_mi2 = xsaldo_mi2.
    wa_saida_aux-xsaldo_mi3 = xsaldo_mi3.
    "
    IF p_bukrs = '0101'. "PARAGUAI
      wa_saida_aux-xsaldo_mi  = xsaldo_mi * 100.
    ENDIF.
    APPEND wa_saida_aux TO it_saida.
  ENDLOOP.

  " Ledger 50
  REFRESH it_saida_aux.
  LOOP AT it_faglflext50 INTO wa_faglflext.
*    READ TABLE IT_SAIDA  INTO WA_SAIDA WITH KEY RACCT      = WA_FAGLFLEXT-RACCT.
*    IF SY-SUBRC = 0.
*      IF WA_SAIDA-XSALDO_MI NE 0 OR  WA_SAIDA-XSALDO_MI2 NE 0 OR WA_SAIDA-XSALDO_MI3 NE 0.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
    wa_saida-racct      = wa_faglflext-racct.
    wa_saida-xsaldo_mi  = wa_faglflext-hslvt +
                          wa_faglflext-hsl01 +
                          wa_faglflext-hsl02 +
                          wa_faglflext-hsl03 +
                          wa_faglflext-hsl04 +
                          wa_faglflext-hsl05 +
                          wa_faglflext-hsl06 +
                          wa_faglflext-hsl07 +
                          wa_faglflext-hsl08 +
                          wa_faglflext-hsl09 +
                          wa_faglflext-hsl10 +
                          wa_faglflext-hsl11 +
                          wa_faglflext-hsl12 +
                          wa_faglflext-hsl13 +
                          wa_faglflext-hsl14 +
                          wa_faglflext-hsl15 +
                          wa_faglflext-hsl16.

    wa_saida-xsaldo_mi2 = wa_faglflext-kslvt +
                          wa_faglflext-ksl01 +
                          wa_faglflext-ksl02 +
                          wa_faglflext-ksl03 +
                          wa_faglflext-ksl04 +
                          wa_faglflext-ksl05 +
                          wa_faglflext-ksl06 +
                          wa_faglflext-ksl07 +
                          wa_faglflext-ksl08 +
                          wa_faglflext-ksl09 +
                          wa_faglflext-ksl10 +
                          wa_faglflext-ksl11 +
                          wa_faglflext-ksl12 +
                          wa_faglflext-ksl13 +
                          wa_faglflext-ksl14 +
                          wa_faglflext-ksl15 +
                          wa_faglflext-ksl16.

    wa_saida-xsaldo_mi3 = wa_faglflext-oslvt +
                          wa_faglflext-osl01 +
                          wa_faglflext-osl02 +
                          wa_faglflext-osl03 +
                          wa_faglflext-osl04 +
                          wa_faglflext-osl05 +
                          wa_faglflext-osl06 +
                          wa_faglflext-osl07 +
                          wa_faglflext-osl08 +
                          wa_faglflext-osl09 +
                          wa_faglflext-osl10 +
                          wa_faglflext-osl11 +
                          wa_faglflext-osl12 +
                          wa_faglflext-osl13 +
                          wa_faglflext-osl14 +
                          wa_faglflext-osl15 +
                          wa_faglflext-osl16 .


    READ TABLE it_ska1 INTO wa_ska1 WITH KEY saknr = wa_faglflext-racct BINARY SEARCH.
    wa_saida-txt50      = wa_ska1-txt50.

    READ TABLE it_skb1 INTO wa_skb1 WITH KEY saknr = wa_faglflext-racct BINARY SEARCH.
    wa_saida-mitkz      = wa_skb1-mitkz.
    wa_saida-xintb      = wa_skb1-xintb.

    LOOP AT it_zfit0072 INTO wa_zfit0072.
      IF wa_faglflext-racct GE wa_zfit0072-conta_de AND
         wa_faglflext-racct LE wa_zfit0072-conta_ate.
        EXIT.
      ENDIF.
    ENDLOOP.
    wa_saida-kostl      = wa_zfit0072-kostl.
    wa_saida-prctr      = wa_zfit0072-prctr.
    wa_saida-aufnr      = wa_zfit0072-aufnr.

    IF wa_skb1-xintb = 'X'.
      wa_saida-line_color  = 'C310'.
    ENDIF.

    wa_saida-rldnr      = '50'.

    IF wa_saida-xsaldo_mi NE 0.
      APPEND wa_saida TO it_saida_aux.
    ENDIF.
    "
    CLEAR wa_saida.
  ENDLOOP.

  it_saida_aux2[] = it_saida_aux[].

  SORT: it_saida_aux  BY racct,
        it_saida_aux2 BY racct.

  CLEAR  wa_saida_aux2.
  LOOP AT it_saida_aux INTO wa_saida_aux.
    IF wa_saida_aux-racct = wa_saida_aux2-racct.
      CONTINUE.
    ENDIF.
    xsaldo_mi   = 0.
    xsaldo_mi2  = 0.
    xsaldo_mi3  = 0.
    LOOP AT it_saida_aux2 INTO wa_saida_aux2 WHERE racct = wa_saida_aux-racct.
      ADD wa_saida_aux2-xsaldo_mi  TO xsaldo_mi.
      ADD wa_saida_aux2-xsaldo_mi2 TO xsaldo_mi2.
      ADD wa_saida_aux2-xsaldo_mi3 TO xsaldo_mi3.
    ENDLOOP.
    wa_saida_aux-xsaldo_mi  = xsaldo_mi.
    wa_saida_aux-xsaldo_mi2 = xsaldo_mi2.
    wa_saida_aux-xsaldo_mi3 = xsaldo_mi3.
    "
    IF p_bukrs = '0101'. "PARAGUAI
      wa_saida_aux-xsaldo_mi  = xsaldo_mi * 100.
    ENDIF.
    APPEND wa_saida_aux TO it_saida.
  ENDLOOP.

  PERFORM f_atualiza_status.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .
  PERFORM f_alv_fieldcat.


  wa_layout-zebra      = 'X'.
  "WA_LAYOUT-NO_ROWMOVE = 'X'.
  "WA_LAYOUT-NO_ROWINS  = 'X'.
  wa_layout-no_rowmark = space.
  wa_layout-grid_title = TEXT-008.
  wa_layout-sel_mode   = 'A'.
  wa_layout-cwidth_opt = 'X'.
  wa_layout-box_fname  = 'MARK'.
  wa_layout-info_fname = 'LINE_COLOR'.
  CALL SCREEN 0100.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat .
  REFRESH it_fieldcat.

  DATA i TYPE i.
  wa_afield-tabname     = 'IT_SAIDA'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ICON'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s = 'Status'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-hotspot       = 'X'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'RLDNR '.
  wa_afield-scrtext_s = 'Ledger'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'RACCT'.
  wa_afield-scrtext_s = TEXT-001.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TXT50'.
  wa_afield-scrtext_s = TEXT-002.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MITKZ'.
  wa_afield-scrtext_s = TEXT-003.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'XINTB'.
  wa_afield-scrtext_s = TEXT-004.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'XSALDO_MI'.
  CONCATENATE TEXT-005 wa_t882g-curr1  INTO wa_afield-scrtext_m SEPARATED BY space.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'XSALDO_MI2'.
  CONCATENATE TEXT-005 wa_t882g-curr2  INTO wa_afield-scrtext_m SEPARATED BY space.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'XSALDO_MI3'.
  CONCATENATE TEXT-005 wa_t882g-curr3  INTO wa_afield-scrtext_m SEPARATED BY space.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'KOSTL'.
  wa_afield-scrtext_s = TEXT-006.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'PRCTR'.
  wa_afield-scrtext_s = TEXT-007.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'AUFNR'.
  wa_afield-scrtext_s = TEXT-014.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BELNR'.
  wa_afield-scrtext_s = 'DOC CTB.'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-hotspot       = 'X'.
  APPEND wa_afield TO it_fieldcat.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  APPEND 'SAVE' TO fcode.

  SET PF-STATUS 'F_SET_PF' EXCLUDING fcode.
  SET TITLEBAR  'ZFTITLEP'.

  CONCATENATE '@39@' TEXT-012 INTO wg_gerar SEPARATED BY space.
  CONCATENATE '@LF@' TEXT-013 INTO wg_elim SEPARATED BY space.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  "Objetos GRID
  IF NOT cl_grid IS INITIAL.
    PERFORM zf_alv_header.
    CALL METHOD cl_grid->refresh_table_display.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT obj_dyndoc_id
      EXPORTING
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
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


    CREATE OBJECT cl_grid
      EXPORTING
        i_parent = cl_container_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    CALL METHOD cl_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    wa_stable-row        = 'X'.

    wg_save = 'X'.
*    WA_LAYOUT-INFO_FNAME    = 'LINE_COLOR'.
    gs_variant_c-report      = sy-repid.

    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_variant      = gs_variant_c
        is_layout       = wa_layout
        i_save          = wg_save
        i_default       = 'X'
      CHANGING
        it_fieldcatalog = it_fieldcat[]
        it_sort         = i_sort[]
        it_outtab       = it_saida[].

*
    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->catch_hotspot              FOR cl_grid.
*    SET HANDLER EVENT_RECEIVER->ON_DATA_CHANGED            FOR CL_GRID.
*    SET HANDLER EVENT_RECEIVER->ON_DATA_CHANGED_FINISHED   FOR CL_GRID.



  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
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


  wl_text = TEXT-009.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.



  CONCATENATE  TEXT-010 p_bukrs
          INTO wl_linha SEPARATED BY space.
  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  CONCATENATE  TEXT-011 p_ryear
          INTO wl_linha SEPARATED BY space.
  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      sap_fontsize = cl_dd_area=>list_normal.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  IF NOT cl_grid IS INITIAL.
    CALL METHOD cl_grid->dispatch
      EXPORTING
        cargo         = sy-ucomm
        eventid       = 19
        is_shellevent = ' '.

    IF sy-ucomm IS INITIAL.
      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = is_stable.
    ENDIF.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP'.
      REFRESH it_saida.
      CALL METHOD cl_grid->refresh_table_display.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'REFRESH'.
      PERFORM f_atualiza_status.
    WHEN 'SHOWELIM'.
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = et_index_rows
          et_row_no     = et_row_no.

      DESCRIBE TABLE et_row_no LINES tab_lines.
      IF tab_lines = 1.
        LOOP AT et_row_no INTO wa_row_no  .
          READ TABLE it_saida INTO wa_saida INDEX wa_row_no-row_id.
          IF wa_saida-belnr IS NOT INITIAL.
            PERFORM f_estorna_zib.
            wa_saida-icon = icon_activity.
            MODIFY it_saida FROM wa_saida INDEX wa_row_no-row_id TRANSPORTING icon.
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE TEXT-015 TYPE 'I'.
      ENDIF.

    WHEN 'SHOWGERAR'.
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = et_index_rows
          et_row_no     = et_row_no.


      LOOP AT et_row_no INTO wa_row_no  .
        READ TABLE it_saida INTO wa_saida INDEX wa_row_no-row_id.
        "IF WA_SAIDA-XINTB NE 'X' AND ( WA_SAIDA-ICON = '' OR WA_SAIDA-ICON = ICON_INCOMPLETE ) .
        IF ( wa_saida-icon = '' OR wa_saida-icon = icon_incomplete ) .
          PERFORM f_grava_zib.
          wa_saida-icon = icon_activity.
          MODIFY it_saida FROM wa_saida INDEX wa_row_no-row_id TRANSPORTING icon.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_ZIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava_zib .
  DATA:
    vseq(10)   TYPE p,
    vnum(10)   TYPE c,
    xsaldo_mi  TYPE faglflext-ksl16,
    xsaldo_mi2 TYPE faglflext-ksl16,
    xsaldo_mi3 TYPE faglflext-ksl16,

    lc_vlmi1   TYPE faglflext-ksl16,
    lc_vlmi2   TYPE faglflext-ksl16,
    lc_vlmi3   TYPE faglflext-ksl16,

    vfixo(5)   TYPE p DECIMALS 2,
    vseqi      TYPE i,
    vbschl     TYPE zib_contabil-bschl.

  "VFIXO = 1 / 100.
  "AJUSTADO NA BAPI PARA ACEITAR ZERO.
  vfixo = 0.

  " Gera numero do lote
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZID_ENCE'
    IMPORTING
      number      = vseq.

  vnum = vseq.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vnum
    IMPORTING
      output = vnum.

  REFRESH it_zib_contabil.

  xsaldo_mi  = wa_saida-xsaldo_mi.
  xsaldo_mi2 = wa_saida-xsaldo_mi2.
  xsaldo_mi3 = wa_saida-xsaldo_mi3.

  MULTIPLY xsaldo_mi  BY -1.
  MULTIPLY xsaldo_mi2 BY -1.
  MULTIPLY xsaldo_mi3 BY -1.

  " Lançamento de Crédito de Encerramento de conta """""""""""""""""""""""""""""""""""""
  lc_vlmi1 = 0.
  lc_vlmi2 = 0.
  lc_vlmi3 = 0.

  CLEAR: vbschl.

  IF xsaldo_mi GT 0.
    lc_vlmi1 = xsaldo_mi.
    vbschl   = '40'.
  ELSE.
    lc_vlmi1 = vfixo.
  ENDIF.

  IF xsaldo_mi2 GT 0.
    lc_vlmi2 = xsaldo_mi2.
    vbschl   = '40'.
  ELSE.
    lc_vlmi2 = vfixo.
  ENDIF.

  IF xsaldo_mi3 GT 0.
    lc_vlmi3 = xsaldo_mi3.
    vbschl   = '40'.
  ELSE.
    lc_vlmi3 = vfixo.
  ENDIF.

  vseqi = 0.
  IF vbschl IS NOT INITIAL AND (  lc_vlmi1 NE 0 OR  lc_vlmi2 NE 0  OR lc_vlmi3 NE 0 ).
    PERFORM z_grava_zibii USING vnum lc_vlmi1 lc_vlmi2 lc_vlmi3 vbschl CHANGING vseqi.
  ENDIF.

  " Lançamento de Débito de Encerramento de conta """""""""""""""""""""""""""""""""""""
  lc_vlmi1 = 0.
  lc_vlmi2 = 0.
  lc_vlmi3 = 0.

  CLEAR: vbschl.

  "MULTIPLY VFIXO BY -1.

  IF xsaldo_mi LE 0.
    lc_vlmi1 = xsaldo_mi.
    MULTIPLY lc_vlmi1 BY -1.
    vbschl   = '50'.
  ELSE.
    lc_vlmi1 = vfixo.
  ENDIF.

  IF xsaldo_mi2 LE 0.
    lc_vlmi2 = xsaldo_mi2.
    MULTIPLY lc_vlmi2 BY -1.
    vbschl   = '50'.
  ELSE.
    lc_vlmi2 = vfixo.
  ENDIF.

  IF xsaldo_mi3 LE 0.
    lc_vlmi3 = xsaldo_mi3.
    MULTIPLY lc_vlmi3 BY -1.
    vbschl   = '50'.
  ELSE.
    lc_vlmi3 = vfixo.
  ENDIF.

  IF vbschl IS NOT INITIAL AND (  lc_vlmi1 NE 0 OR  lc_vlmi2 NE 0  OR lc_vlmi3 NE 0 ).
    PERFORM z_grava_zibii USING vnum lc_vlmi1 lc_vlmi2 lc_vlmi3 vbschl CHANGING vseqi.
  ENDIF.

*  XSALDO_MI  = WA_SAIDA-XSALDO_MI.
*  XSALDO_MI2 = WA_SAIDA-XSALDO_MI2.
*  XSALDO_MI3 = WA_SAIDA-XSALDO_MI3.
*
*  IF XSALDO_MI LT 0.
*    MULTIPLY XSALDO_MI BY -1.
*  ENDIF.
*
*  IF XSALDO_MI2 LT 0.
*    MULTIPLY XSALDO_MI2 BY -1.
*  ENDIF.
*
*  IF XSALDO_MI3 LT 0.
*    MULTIPLY XSALDO_MI3 BY -1.
*  ENDIF.

*  VSEQI = 0.
*  IF XSALDO_MI GT 0 AND XSALDO_MI2 GT 0 AND XSALDO_MI3 GT 0.
*    IF WA_SAIDA-XSALDO_MI GT 0.
*      VBSCHL = '50'.
*    ELSE.
*      VBSCHL = '40'.
*    ENDIF.
*    PERFORM Z_GRAVA_ZIBII USING VNUM XSALDO_MI XSALDO_MI2 XSALDO_MI3  VBSCHL VSEQI.
*  ELSE.
*    IF XSALDO_MI GT 0.
*      IF WA_SAIDA-XSALDO_MI GT 0.
*        VBSCHL = '50'.
*      ELSE.
*        VBSCHL = '40'.
*      ENDIF.
*      ADD VFIXO TO XSALDO_MI.
*      PERFORM Z_GRAVA_ZIBII USING VNUM XSALDO_MI VFIXO VFIXO  VBSCHL VSEQI.
*      ADD 2 TO VSEQI.
*      PERFORM Z_GRAVA_ZIBII USING VNUM VFIXO  VFIXO VFIXO     VBSCHL VSEQI.
*    ENDIF.
*
*    IF XSALDO_MI2 GT 0.
*      IF WA_SAIDA-XSALDO_MI2 GT 0.
*        VBSCHL = '50'.
*      ELSE.
*        VBSCHL = '40'.
*      ENDIF.
*      ADD VFIXO TO XSALDO_MI2.
*      IF VSEQI = 2.
*        VSEQI = 4.
*      ENDIF.
*      PERFORM Z_GRAVA_ZIBII USING VNUM VFIXO XSALDO_MI2 VFIXO  VBSCHL VSEQI.
*      ADD 2 TO VSEQI.
*      PERFORM Z_GRAVA_ZIBII USING VNUM VFIXO VFIXO      VFIXO  VBSCHL VSEQI.
*    ENDIF.
*
*    IF XSALDO_MI3 GT 0.
*      IF WA_SAIDA-XSALDO_MI3 GT 0.
*        VBSCHL = '50'.
*      ELSE.
*        VBSCHL = '40'.
*      ENDIF.
*      ADD VFIXO TO XSALDO_MI3.
*      IF VSEQI = 2.
*        VSEQI = 4.
*      ENDIF.
*      PERFORM Z_GRAVA_ZIBII USING VNUM VFIXO VFIXO      XSALDO_MI3   VBSCHL VSEQI.
*      ADD 2 TO VSEQI.
*      PERFORM Z_GRAVA_ZIBII USING VNUM VFIXO VFIXO      VFIXO        VBSCHL VSEQI.
*    ENDIF.
*
*  ENDIF.

  MODIFY zib_contabil FROM TABLE it_zib_contabil.

ENDFORM.                    " F_GRAVA_ZIB

*&---------------------------------------------------------------------*
*&      Form  Z_GRAVA_ZIBII
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_grava_zibii USING vnum xsaldo_mi xsaldo_mi2 xsaldo_mi3 vbschl CHANGING vseqi.

  DO 2 TIMES.

    CONCATENATE 'ER' vnum p_ryear  INTO  wa_zib_contabil-obj_key.

*    ADD 1 TO vseqi.

    wa_zib_contabil-seqitem   = vseqi.
    wa_zib_contabil-xblnr     = ''.

    IF sy-index = 1.
      wa_zib_contabil-bschl     = vbschl.
    ELSE.
      IF vbschl = '50'.
        vbschl = '40'.
      ELSE.
        vbschl = '50'.
      ENDIF.
      wa_zib_contabil-bschl     = vbschl.
    ENDIF.

    CONCATENATE p_bukrs+2(2) '01' INTO wa_zib_contabil-gsber.
    IF p_bukrs = '0100'.
      wa_zib_contabil-gsber = 'T001'.
    ELSEIF p_bukrs = '0101'.
      wa_zib_contabil-gsber = 'F101'.
    ELSEIF p_bukrs = '0200'.
      wa_zib_contabil-gsber = 'S201'.
    ELSEIF p_bukrs = '0201'.
      wa_zib_contabil-gsber = 'H201'.
    ELSEIF p_bukrs = '0202'.
      wa_zib_contabil-gsber = 'H202'.
    ELSEIF p_bukrs = '0203'.
      wa_zib_contabil-gsber = 'L203'.
    ENDIF.

    wa_zib_contabil-bukrs     = p_bukrs.
    wa_zib_contabil-interface = '36'.
    CONCATENATE sy-uname ' - ' p_ryear INTO wa_zib_contabil-bktxt SEPARATED BY space.
    CONCATENATE '31.12.' p_ryear INTO wa_zib_contabil-bldat.
    CONCATENATE '31.12.' p_ryear INTO wa_zib_contabil-budat.

    wa_zib_contabil-gjahr     = p_ryear.
    wa_zib_contabil-monat     = 16.
    wa_zib_contabil-blart     = 'XS'.
    CLEAR wa_zib_contabil-rldnr.
    IF wa_saida-rldnr = '50'.
      wa_zib_contabil-rldnr     = wa_saida-rldnr.
    ENDIF.


    IF sy-index  EQ 1.
      wa_zib_contabil-hkont     = wa_saida-racct.
      wa_zib_contabil-bewar     = ''.
      wa_zib_contabil-kostl     = wa_saida-kostl.
      wa_zib_contabil-aufnr     = wa_saida-aufnr.
      wa_zib_contabil-prctr     = wa_saida-prctr.
      wa_zib_contabil-wrbtr     = xsaldo_mi.
      wa_zib_contabil-waers     = wa_t882g-curr1.
      wa_zib_contabil-bupla     =  ''.
    ELSE.
      LOOP AT it_zfit0072 INTO wa_zfit0072. " Contrapartida
        IF wa_zfit0072-cta_partida = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      wa_zib_contabil-hkont     = wa_zfit0072-conta_de.
      wa_zib_contabil-bewar     = wa_zfit0072-bewar.
      wa_zib_contabil-kostl     = ''.
      wa_zib_contabil-aufnr     = ''.
      wa_zib_contabil-prctr     = ''.
      wa_zib_contabil-wrbtr     = xsaldo_mi.
      wa_zib_contabil-waers     = wa_t882g-curr1.
      wa_zib_contabil-bupla     =  ''.
    ENDIF.

    CONCATENATE TEXT-016 p_ryear INTO wa_zib_contabil-sgtxt SEPARATED BY space.
*** Stefanini - IR239333 - 09/06/2025 - FINC - Início de Alteração

    wa_zib_contabil-waers_i   = wa_t882g-curr1.
*    wa_zib_contabil-dmbtr     = xsaldo_mi.

    wa_zib_contabil-waers_f   = wa_t882g-curr2.
*    wa_zib_contabil-dmbe2     = xsaldo_mi2.

    wa_zib_contabil-waers_g   = wa_t882g-curr3.
*    wa_zib_contabil-dmbe3     = xsaldo_mi3.

    wa_zib_contabil-rg_atualizado  = 'N'.
*    APPEND wa_zib_contabil TO it_zib_contabil.

    PERFORM z_append_zib_contabil
      USING wa_zib_contabil
            xsaldo_mi
            xsaldo_mi2
            xsaldo_mi3
   CHANGING vseqi.
*** Stefanini - IR239333 - 09/06/2025 - FINC - Fim de Alteração

    CLEAR wa_zib_contabil.

  ENDDO.

ENDFORM.                    "Z_GRAVA_ZIBII

*** Stefanini - IR239333 - 09/06/2025 - FINC - Início de Alteração
*&---------------------------------------------------------------------*
*&      Form  Z_APPEND_ZIB_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_append_zib_contabil USING p_wa_zib_contabil  TYPE ty_zib_contabil
                                  p_valor_wrbtr     TYPE wrbtr
                                  p_valor_dmbe2     TYPE wrbtr
                                  p_valor_dmbe3     TYPE wrbtr
                            CHANGING p_seqi         TYPE i.

  DATA: lv_valor_max   TYPE wrbtr VALUE '99999999999.99',
        lv_maior_valor TYPE wrbtr,
        lv_campo_base  TYPE string,
        lv_qtd_linhas  TYPE i,
        lv_wa_zib      TYPE ty_zib_contabil,
        lv_total_wrbtr TYPE wrbtr,
        lv_total_dmbe2 TYPE wrbtr,
        lv_total_dmbe3 TYPE wrbtr,
        lv_resto_wrbtr TYPE wrbtr,
        lv_valor_linha TYPE wrbtr,
        lv_perc        TYPE p DECIMALS 10,
        lv_dmbe2_linha TYPE wrbtr,
        lv_dmbe3_linha TYPE wrbtr,
        lv_index       TYPE i.

  " Identificar o campo base (aquele que ultrapassa o valor máximo)
  lv_maior_valor = p_valor_wrbtr.
  lv_campo_base = 'WRBTR'.

  IF p_valor_dmbe2 > lv_maior_valor.
    lv_maior_valor = p_valor_dmbe2.
    lv_campo_base = 'DMBE2'.
  ENDIF.

  IF p_valor_dmbe3 > lv_maior_valor.
    lv_maior_valor = p_valor_dmbe3.
    lv_campo_base = 'DMBE3'.
  ENDIF.

  " Calcular quantidade de linhas
  lv_qtd_linhas = lv_maior_valor DIV lv_valor_max.
  IF lv_maior_valor MOD lv_valor_max > 0.
    lv_qtd_linhas = lv_qtd_linhas + 1.
  ENDIF.

  " Inicializar totais
  lv_total_wrbtr = 0.
  lv_total_dmbe2 = 0.
  lv_total_dmbe3 = 0.

  " Gerar linhas
  DO lv_qtd_linhas TIMES.
    CLEAR: lv_wa_zib, lv_valor_linha, lv_dmbe2_linha, lv_dmbe3_linha.

    lv_wa_zib = p_wa_zib_contabil.

    ADD 1 TO p_seqi.
    lv_wa_zib-seqitem = p_seqi.

    " Definir valor da linha com base no campo base
    CASE lv_campo_base.
      WHEN 'WRBTR'.
        IF ( p_valor_wrbtr - lv_total_wrbtr ) > lv_valor_max.
          lv_valor_linha = lv_valor_max.
        ELSE.
          lv_valor_linha = p_valor_wrbtr - lv_total_wrbtr.
        ENDIF.
        lv_total_wrbtr = lv_total_wrbtr + lv_valor_linha.

        " Proporção da linha
        lv_perc = lv_valor_linha / p_valor_wrbtr.

        " Calcular proporcionais
        lv_dmbe2_linha = p_valor_dmbe2 * lv_perc.
        lv_dmbe3_linha = p_valor_dmbe3 * lv_perc.

        " Corrigir última linha
        IF sy-index = lv_qtd_linhas.
          lv_dmbe2_linha = p_valor_dmbe2 - lv_total_dmbe2.
          lv_dmbe3_linha = p_valor_dmbe3 - lv_total_dmbe3.
        ENDIF.

        lv_total_dmbe2 = lv_total_dmbe2 + lv_dmbe2_linha.
        lv_total_dmbe3 = lv_total_dmbe3 + lv_dmbe3_linha.

        " Preencher campos
        lv_wa_zib-wrbtr = lv_valor_linha.
        lv_wa_zib-dmbtr = lv_valor_linha.
        lv_wa_zib-dmbe2 = lv_dmbe2_linha.
        lv_wa_zib-dmbe3 = lv_dmbe3_linha.

      WHEN 'DMBE2'.
        IF ( p_valor_dmbe2 - lv_total_dmbe2 ) > lv_valor_max.
          lv_valor_linha = lv_valor_max.
        ELSE.
          lv_valor_linha = p_valor_dmbe2 - lv_total_dmbe2.
        ENDIF.
        lv_total_dmbe2 = lv_total_dmbe2 + lv_valor_linha.

        lv_perc = lv_valor_linha / p_valor_dmbe2.

        lv_wa_zib-dmbe2 = lv_valor_linha.
        lv_wa_zib-wrbtr = p_valor_wrbtr * lv_perc.
        lv_wa_zib-dmbtr = lv_wa_zib-wrbtr.
        lv_wa_zib-dmbe3 = p_valor_dmbe3 * lv_perc.

        IF sy-index = lv_qtd_linhas.
          lv_wa_zib-wrbtr = p_valor_wrbtr - lv_total_wrbtr.
          lv_wa_zib-dmbe3 = p_valor_dmbe3 - lv_total_dmbe3.
        ENDIF.

        lv_total_wrbtr = lv_total_wrbtr + lv_wa_zib-wrbtr.
        lv_total_dmbe3 = lv_total_dmbe3 + lv_wa_zib-dmbe3.

      WHEN 'DMBE3'.
        IF ( p_valor_dmbe3 - lv_total_dmbe3 ) > lv_valor_max.
          lv_valor_linha = lv_valor_max.
        ELSE.
          lv_valor_linha = p_valor_dmbe3 - lv_total_dmbe3.
        ENDIF.
        lv_total_dmbe3 = lv_total_dmbe3 + lv_valor_linha.

        lv_perc = lv_valor_linha / p_valor_dmbe3.

        lv_wa_zib-dmbe3 = lv_valor_linha.
        lv_wa_zib-wrbtr = p_valor_wrbtr * lv_perc.
        lv_wa_zib-dmbtr = lv_wa_zib-wrbtr.
        lv_wa_zib-dmbe2 = p_valor_dmbe2 * lv_perc.

        IF sy-index = lv_qtd_linhas.
          lv_wa_zib-wrbtr = p_valor_wrbtr - lv_total_wrbtr.
          lv_wa_zib-dmbe2 = p_valor_dmbe2 - lv_total_dmbe2.
        ENDIF.

        lv_total_wrbtr = lv_total_wrbtr + lv_wa_zib-wrbtr.
        lv_total_dmbe2 = lv_total_dmbe2 + lv_wa_zib-dmbe2.

    ENDCASE.

    APPEND lv_wa_zib TO it_zib_contabil.

  ENDDO.

ENDFORM.
*** Stefanini - IR239333 - 09/06/2025 - FINC - Fim de Alteração

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atualiza_status .

  CHECK it_saida[] IS NOT INITIAL.

  DATA tabix TYPE sy-tabix.
  SELECT *
    FROM zib_contabil
    INTO TABLE it_zib_contabil
     FOR ALL ENTRIES IN it_saida
   WHERE bukrs     = p_bukrs
     AND gjahr     = p_ryear
     AND monat     = 16
     AND blart     = 'XS'
     AND interface = '36'
     AND hkont     = it_saida-racct
     AND kostl     = it_saida-kostl
     AND aufnr     = it_saida-aufnr
     AND prctr     = it_saida-prctr.

  DELETE it_zib_contabil WHERE obj_key+0(2) NE 'ER'.

  CHECK it_zib_contabil[] IS NOT INITIAL.

  SELECT *
    FROM zib_contabil_err
    INTO TABLE it_zib_contabil_err
    FOR ALL ENTRIES IN it_zib_contabil
      WHERE obj_key   = it_zib_contabil-obj_key.

  SELECT *
    FROM zib_contabil_chv
    INTO TABLE it_zib_contabil_chv
    FOR ALL ENTRIES IN it_zib_contabil
      WHERE obj_key   = it_zib_contabil-obj_key.


  SORT: it_zib_contabil     BY hkont kostl aufnr prctr obj_key,
        it_zib_contabil_err BY obj_key,
        it_zib_contabil_chv BY obj_key ASCENDING belnr DESCENDING.

  LOOP AT it_saida INTO wa_saida.
    tabix = sy-tabix.
    CLEAR wa_saida-icon.
    LOOP AT it_zib_contabil INTO wa_zib_contabil WHERE  hkont = wa_saida-racct
                                                 AND    kostl = wa_saida-kostl
                                                 AND    aufnr = wa_saida-aufnr
                                                 AND    prctr = wa_saida-prctr.
      wa_saida-icon = icon_warning.
      READ TABLE it_zib_contabil_err INTO wa_zib_contabil_err WITH KEY obj_key = wa_zib_contabil-obj_key BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-icon = icon_incomplete.
      ENDIF.
      READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_zib_contabil-obj_key BINARY SEARCH.
      IF sy-subrc = 0.
        SELECT SINGLE   *
          FROM bkpf
          INTO @DATA(wg_bkpf_fb08)
          WHERE bukrs EQ @wa_zib_contabil_chv-bukrs
          AND   belnr EQ @wa_zib_contabil_chv-belnr
          AND   gjahr EQ @wa_zib_contabil_chv-gjahr
          AND   stblg NE ''.
        IF sy-subrc NE 0. "Se estiver estornado não mostra documento
          wa_saida-icon = icon_okay.
          wa_saida-belnr = wa_zib_contabil_chv-belnr.
          wa_saida-bukrs = wa_zib_contabil_chv-bukrs.
          wa_saida-gjahr = wa_zib_contabil_chv-gjahr.
        ELSE.
          CLEAR wa_saida-icon. "limpa icon para libera reprocessamento
        ENDIF.
      ENDIF.
      wa_saida-obj_key = wa_zib_contabil-obj_key.
    ENDLOOP.
    MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING icon obj_key belnr bukrs gjahr.
  ENDLOOP.

ENDFORM.                    " F_ATUALIZA_STATUS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'Z001' EXCLUDING fcode.
  SET TITLEBAR  '0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_0200 OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        "EVENTS TYPE CNTL_SIMPLE_EVENTS,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.

  IF obg_conteiner_err IS INITIAL.
    CREATE OBJECT obg_conteiner_err
      EXPORTING
        container_name = g_cc_err.


    CREATE OBJECT grid3
      EXPORTING
        i_parent = obg_conteiner_err.


    PERFORM montar_layout_err.

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


    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_zib_contabil_err[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ERR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_err .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZIB_CONTABIL_ERR'         'OBJ_KEY'        'IT_ZIB_CONTABIL_ERR' 'OBJ_KEY'         ' '   '20' ' ' ' ' ' ',
        1 'ZIB_CONTABIL_ERR'         'NR_ITEM'        'IT_ZIB_CONTABIL_ERR' 'NR_ITEM'         ' '   '10' ' ' ' ' ' ',
        2 'ZIB_CONTABIL_ERR'         'INTERFACE'      'IT_ZIB_CONTABIL_ERR' 'INTERFACE'       ' '   '15' ' ' ' ' ' ',
        3 'ZIB_CONTABIL_ERR'         'DT_ATUALIZACAO' 'IT_ZIB_CONTABIL_ERR' 'DT_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ',
        4 'ZIB_CONTABIL_ERR'         'HR_ATUALIZACAO' 'IT_ZIB_CONTABIL_ERR' 'HR_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ',
        5 'ZIB_CONTABIL_ERR'         'TYPE'           'IT_ZIB_CONTABIL_ERR' 'TYPE'            ' '   '08' ' ' ' ' ' ',
        6 'ZIB_CONTABIL_ERR'         'ID'             'IT_ZIB_CONTABIL_ERR' 'ID'              ' '   '10' ' ' ' ' ' ',
        7 'ZIB_CONTABIL_ERR'         'NUM'            'IT_ZIB_CONTABIL_ERR' 'NUM'             ' '   '10' ' ' ' ' ' ',
        "8 'ZIB_CONTABIL_ERR'         'MESSAGE'        'IT_ZIB_CONTABIL_ERR' 'MESSAGE'         ' '   '20' ' ' ' ' ' ',
        8 ' '                        ' '              'IT_ZIB_CONTABIL_ERR' 'MESSAGE'         'Mensagem de Erro '   '100' ' ' ' ' ' ',
        9 'ZIB_CONTABIL_ERR'         'MESSAGE_V1'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V1'      ' '   '50' ' ' ' ' ' ',
       10 'ZIB_CONTABIL_ERR'         'MESSAGE_V2'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V2'      ' '   '30' ' ' ' ' ' ',
       11 'ZIB_CONTABIL_ERR'         'MESSAGE_V3'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V3'      ' '   '30' ' ' ' ' ' ',
       12 'ZIB_CONTABIL_ERR'         'MESSAGE_V4'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V4'      ' '   '30' ' ' ' ' ' '.


ENDFORM.                    " MONTAR_LAYOUT_ERR


*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_COL_POS)        text
*      -->VALUE(P_REF_TABNAME)    text
*      -->VALUE(P_REF_FIELDNAME)  text
*      -->VALUE(P_TABNAME)        text
*      -->VALUE(P_FIELD)          text
*      -->VALUE(P_SCRTEXT_L)      text
*      -->VALUE(P_OUTPUTLEN)      text
*      -->VALUE(P_EDIT)           text
*      -->VALUE(P_SUM)            text
*      -->VALUE(P_EMPHASIZE)      text
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

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok-code.
    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNA_ZIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorna_zib .
  DATA: vdata(10),
        p_erro(1).
  SELECT SINGLE   *
         FROM bkpf
         INTO @DATA(wg_bkpf)
         WHERE bukrs EQ @wa_saida-bukrs
         AND   belnr EQ @wa_saida-belnr
         AND   gjahr EQ @wa_saida-gjahr.

  CONCATENATE wg_bkpf-budat+6(2) '.' wg_bkpf-budat+4(2) '.' wg_bkpf-budat+0(4) INTO vdata.
  REFRESH ti_bdcdata.
  PERFORM f_bdc_data USING:
        'SAPMF05A'  '0105'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '/00',
        ''          ''      ''   'RF05A-BELNS'      wa_saida-belnr,
        ''          ''      ''   'BKPF-BUKRS'       wa_saida-bukrs,
        ''          ''      ''   'RF05A-GJAHS'      wa_saida-gjahr,
        ''          ''      ''   'UF05A-STGRD'      '01',
        ''          ''      ''   'BSIS-BUDAT'       vdata,
        ''          ''      ''   'BSIS-MONAT'       '16',
        'SAPMF05A'  '0105'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '=BU'.

  CLEAR p_erro.
  PERFORM zf_call_transaction USING 'FB08' CHANGING p_erro.
  "
*  DATA:
*          VSEQ(10)    TYPE P,
*          VNUM(10)    TYPE C,
*          TABIX       TYPE SY-TABIX.
*
*  " Gera numero do lote
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      NR_RANGE_NR = '01'
*      OBJECT      = 'ZID_ENCE'
*    IMPORTING
*      NUMBER      = VSEQ.
*
*  VNUM = VSEQ.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      INPUT  = VNUM
*    IMPORTING
*      OUTPUT = VNUM.
*
*  SELECT *
*     FROM ZIB_CONTABIL
*     INTO TABLE IT_ZIB_CONTABIL
*     WHERE OBJ_KEY   = WA_SAIDA-OBJ_KEY.
*
*  LOOP AT IT_ZIB_CONTABIL INTO WA_ZIB_CONTABIL.
*    TABIX = SY-TABIX.
*    CONCATENATE   'ER' VNUM P_RYEAR  INTO  WA_ZIB_CONTABIL-OBJ_KEY.
*    WA_ZIB_CONTABIL-RG_ATUALIZADO = 'N'.
*
*    IF WA_ZIB_CONTABIL-BSCHL     = '40'.
*      WA_ZIB_CONTABIL-BSCHL     = '50'.
*    ELSE.
*      WA_ZIB_CONTABIL-BSCHL     = '40'.
*    ENDIF.
*    CONCATENATE SY-UNAME 'ESTORNO' P_RYEAR INTO WA_ZIB_CONTABIL-BKTXT SEPARATED BY SPACE.
*    CONCATENATE 'ESTORNO ENCERRAMENTO EXERCÍCIO' P_RYEAR INTO WA_ZIB_CONTABIL-SGTXT SEPARATED BY SPACE.
*    MODIFY IT_ZIB_CONTABIL FROM WA_ZIB_CONTABIL INDEX TABIX TRANSPORTING OBJ_KEY RG_ATUALIZADO BSCHL BKTXT SGTXT.
*
*  ENDLOOP.
*
*  MODIFY ZIB_CONTABIL FROM TABLE IT_ZIB_CONTABIL.

ENDFORM.                    " F_ESTORNA_ZIB

FORM zf_call_transaction USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  DATA: wl_cont     TYPE sy-tabix.

  REFRESH: it_msg.

  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
        MODE wl_mode
        MESSAGES INTO it_msg.
  CLEAR: wl_cont.

  LOOP AT it_msg WHERE msgtyp EQ 'E'.
    ADD 1 TO wl_cont.
  ENDLOOP.
  IF wl_cont  GT 0.
    CLEAR wl_cont.
    DELETE FROM zib_contabil_err WHERE obj_key  = vobj_key.
    LOOP AT it_msg WHERE msgtyp EQ 'E'.
      CLEAR: wl_message.
      CALL FUNCTION 'CUTC_GET_MESSAGE'
        EXPORTING
          msg_type       = it_msg-msgtyp
          msg_id         = it_msg-msgid
          msg_no         = sy-msgno
          msg_arg1       = sy-msgv1
          msg_arg2       = sy-msgv2
          msg_arg3       = sy-msgv3
          msg_arg4       = sy-msgv4
        IMPORTING
          raw_message    = wl_message
        EXCEPTIONS
          msg_not_found  = 1
          internal_error = 2
          OTHERS         = 3.

      IF ( sy-subrc NE 0 ).
        wl_message = 'Erro na mensagem do BATCH-INPUT'.
      ENDIF.
      MESSAGE wl_message TYPE 'I'.
    ENDLOOP.


  ENDIF.

  READ TABLE it_msg WITH KEY msgtyp = 'A'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      p_erro = 'X'.
    ENDIF.
  ENDIF.

  CLEAR wg_documento.

  READ TABLE it_msg WITH KEY msgid = c_msgid
                             msgnr = c_msgnr
                             msgtyp = 'S'.

  IF sy-subrc = 0.
    MOVE it_msg-msgv1 TO wg_documento.
  ENDIF.

  IF  wg_documento IS INITIAL.
    p_erro = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_documento
      IMPORTING
        output = wg_documento.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION

FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA
