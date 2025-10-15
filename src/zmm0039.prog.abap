************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 30.11.2012                                          *
* Objetivo    ...: Apropriação de Custo de Frete                       *
* Transação   ...:                                                     *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 30.11.2012   Antonio Luiz  Criação                                   *
************************************************************************

REPORT  zmm0039.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ekko , marc.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

  BEGIN OF ty_ekko,
    bukrs    TYPE ekko-bukrs,
    aedat    TYPE ekko-aedat,
    lifnr    TYPE ekko-lifnr,
    ebeln    TYPE ekko-ebeln,
    ekgrp    TYPE ekko-ekgrp,
    eq_werks TYPE werks,
  END OF ty_ekko,

  BEGIN OF ty_ekpo,
    ebeln   TYPE ekpo-ebeln,
    ebelp   TYPE ekpo-ebelp,
    werks   TYPE ekpo-werks,
    matnr   TYPE ekpo-matnr,
    txz01   TYPE ekpo-txz01,
    j_1bnbm TYPE ekpo-j_1bnbm,
  END OF ty_ekpo,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    regio TYPE lfa1-regio,
    name1 TYPE lfa1-name1,
  END OF ty_lfa1,

  BEGIN OF ty_t001w,
    werks TYPE t001w-werks,
    regio TYPE t001w-regio,
    name1 TYPE t001w-name1,
  END OF ty_t001w,

  BEGIN OF ty_t024,
    ekgrp TYPE t024-ekgrp,
    eknam TYPE t024-eknam,
    ektel TYPE t024-ektel,
  END OF ty_t024,

  BEGIN OF ty_t604f,
    steuc	TYPE t604f-steuc,
    text1 TYPE t604n-text1,
  END OF ty_t604f,

  BEGIN OF ty_saida,
    ebeln   TYPE ekpo-ebeln,
    ebelp   TYPE ekpo-ebelp,
    aedat   TYPE ekko-aedat,
    matnr   TYPE ekpo-matnr,
    txz01   TYPE ekpo-txz01,
    j_1bnbm TYPE ekpo-j_1bnbm,
    text1   TYPE t604n-text1,
    lifnr   TYPE ekko-lifnr,
    name1l  TYPE lfa1-name1,
    regiol  TYPE lfa1-regio,
    werks   TYPE ekpo-werks,
    name1t  TYPE t001w-name1,
    regiot  TYPE t001w-regio,
    ekgrp   TYPE ekko-ekgrp,
    eknam   TYPE t024-eknam,
    ektel   TYPE t024-ektel,
  END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: t_bdcdata    TYPE bdcdata    OCCURS 0 WITH HEADER LINE,
      t_bdcmsgcoll TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE,

      it_ekko      TYPE TABLE OF ty_ekko,
      it_ekpo      TYPE TABLE OF ty_ekpo,
      it_lfa1      TYPE TABLE OF ty_lfa1,
      it_t001w     TYPE TABLE OF ty_t001w,
      it_t024      TYPE TABLE OF ty_t024,
      it_t604f     TYPE TABLE OF ty_t604f,
      it_saida     TYPE TABLE OF ty_saida.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_cont     TYPE REF TO cl_gui_custom_container,
  wa_alv      TYPE REF TO cl_gui_alv_grid,
  wa_layout   TYPE lvc_s_layo,

  wa_ekko     TYPE ty_ekko,
  wa_ekpo     TYPE ty_ekpo,
  wa_lfa1     TYPE ty_lfa1,
  wa_t001w    TYPE ty_t001w,
  wa_t024     TYPE ty_t024,
  wa_t604f    TYPE ty_t604f,
  wa_zmmt0026 TYPE zmmt0026,
  wa_saida    TYPE ty_saida.



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
  estrutura  TYPE TABLE OF ty_estrutura.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                   p_bukrs FOR ekko-bukrs OBLIGATORY,
                   p_aedat FOR ekko-aedat OBLIGATORY,
                   p_werks FOR marc-werks .
SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*
  PERFORM:
            f_iniciar_variaves, " Cabeçalho
            f_seleciona_dados, " Form seleciona dados
            f_saida, " Form de saida
            f_imprime_dados.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves .
  DATA:
    w_texto1(10),
    w_texto2(10),
    w_texto3(40),

    w_empresa_texto(40),
    w_centro_texto(40),
    w_per_texto(40),

    empresa             TYPE c LENGTH 50,
    centro              TYPE c LENGTH 50,
    periodo             TYPE c LENGTH 50.


  v_report = sy-repid.

  w_texto3 = 'Relatório de Recolhimento de Diferencial de Alíquotas '.
  PERFORM f_construir_cabecalho USING 'H' w_texto3.


  IF p_bukrs IS NOT INITIAL.
    w_empresa_texto = 'Empresa:'.
    IF ( p_bukrs-low IS NOT INITIAL ) AND ( p_bukrs-high IS NOT INITIAL ).
      CONCATENATE w_empresa_texto  p_bukrs-low 'á' p_bukrs-high INTO empresa SEPARATED BY space.
    ELSEIF ( p_bukrs-low IS NOT INITIAL ).
      CONCATENATE w_empresa_texto p_bukrs-low  INTO empresa SEPARATED BY space.
    ELSE.
      CONCATENATE w_empresa_texto 'Todas'  INTO empresa SEPARATED BY space.
    ENDIF.
    PERFORM f_construir_cabecalho USING 'S' empresa.
  ENDIF.

  IF p_werks IS NOT INITIAL.
    w_centro_texto = 'Centro:'.
    IF ( p_werks-low IS NOT INITIAL ) AND ( p_werks-high IS NOT INITIAL ).
      CONCATENATE w_centro_texto  p_werks-low 'á' p_werks-high INTO centro SEPARATED BY space.
    ELSEIF ( p_werks-low IS NOT INITIAL ).
      CONCATENATE w_centro_texto p_werks-low  INTO centro SEPARATED BY space.
    ELSE.
      CONCATENATE w_centro_texto 'Todas'  INTO centro SEPARATED BY space.
    ENDIF.
    PERFORM f_construir_cabecalho USING 'S' centro.
  ENDIF.

  IF ( NOT  p_aedat IS INITIAL ).
    w_per_texto = 'Período :'.
    CONCATENATE p_aedat-low+6(2)   '.' p_aedat-low+4(2)  '.' p_aedat-low(4)  INTO w_texto1.
    CONCATENATE p_aedat-high+6(2)  '.' p_aedat-high+4(2) '.' p_aedat-high(4) INTO w_texto2.
    CONCATENATE w_per_texto w_texto1 ' - ' w_texto2 INTO periodo   SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' periodo .
  ENDIF.

ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0399   text
*      -->P_W_TEXTO3  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho    USING typ text.
  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.
ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Preparando dados'.

  SELECT bukrs aedat lifnr ebeln ekgrp eq_werks "#EC CI_USAGE_OK[2368913]
    FROM ekko
    INTO TABLE it_ekko
    WHERE bukrs IN p_bukrs
    AND aedat  IN p_aedat.

  CHECK it_ekko[] IS NOT INITIAL.

  SELECT  ebeln ebelp werks matnr txz01 j_1bnbm
    FROM ekpo
    INTO TABLE it_ekpo
    FOR ALL ENTRIES IN it_ekko
    WHERE ebeln EQ  it_ekko-ebeln.

  SELECT lifnr regio name1
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_ekko
    WHERE lifnr  EQ it_ekko-lifnr.

  SELECT werks regio name1
      FROM t001w
      INTO TABLE it_t001w
      FOR ALL ENTRIES IN it_ekpo
      WHERE werks  EQ it_ekpo-werks.

  SELECT  ekgrp eknam ektel
    FROM t024
    INTO TABLE it_t024
    FOR ALL ENTRIES IN it_ekko
    WHERE ekgrp EQ it_ekko-ekgrp.

  SELECT t604f~steuc  t604n~text1
    FROM t604f
    INNER JOIN t604n
    ON  t604n~spras = 'PT'
    AND t604n~land1 = t604f~land1
    AND t604n~steuc = t604f~steuc
    INTO TABLE it_t604f
    FOR ALL ENTRIES IN it_ekpo
    WHERE t604f~steuc	=	it_ekpo-j_1bnbm
    AND   t604f~land1	=	'BR'.

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

  SORT: it_ekko BY ebeln,
        it_ekpo BY ebeln,
        it_lfa1 BY lifnr,
        it_t001w BY werks,
        it_t024  BY ekgrp.

  LOOP AT it_ekko INTO wa_ekko.
    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekko-ebeln BINARY SEARCH.
    LOOP AT it_ekpo INTO wa_ekpo WHERE ebeln = wa_ekko-ebeln.
      IF ( p_werks IS NOT  INITIAL ) AND ( wa_ekpo-werks  NOT IN p_werks ).
        CONTINUE.
      ENDIF.
      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_ekpo-werks BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-name1t = wa_t001w-name1.
        wa_saida-regiot = wa_t001w-regio.
      ENDIF.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_ekko-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-name1l = wa_lfa1-name1.
        wa_saida-regiol = wa_lfa1-regio.
      ENDIF.
      IF wa_t001w-regio = 'MT' AND wa_lfa1-regio NE 'MT'.
        SELECT SINGLE *
          FROM zmmt0026
          INTO wa_zmmt0026
          WHERE steuc = wa_ekpo-j_1bnbm
          AND land1 = 'BR'.

        IF sy-subrc = 0.
          wa_saida-ebeln  = wa_ekpo-ebeln.
          wa_saida-ebelp  = wa_ekpo-ebelp.
          wa_saida-aedat  = wa_ekko-aedat.
          wa_saida-matnr  = wa_ekpo-matnr.
          wa_saida-txz01  = wa_ekpo-txz01.
          wa_saida-j_1bnbm = wa_ekpo-j_1bnbm.

          SORT it_t604f BY steuc.
          READ TABLE it_t604f INTO wa_t604f WITH KEY steuc  = wa_ekpo-j_1bnbm BINARY SEARCH.
          wa_saida-text1 =  wa_t604f-text1.

          wa_saida-lifnr =  wa_ekko-lifnr.

          wa_saida-werks  = wa_ekpo-werks.

          READ TABLE it_t024 INTO wa_t024 WITH KEY ekgrp  = wa_ekko-ekgrp BINARY SEARCH.
          wa_saida-ekgrp  = wa_ekko-ekgrp.
          wa_saida-eknam  = wa_t024-eknam.
          wa_saida-ektel  = wa_t024-ektel.

          APPEND wa_saida TO it_saida.
          CLEAR:  wa_saida ,
                  wa_ekko  ,
                  wa_ekpo  ,
                  wa_lfa1  ,
                  wa_t001w ,
                  wa_t024.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  SORT it_saida BY ebeln.
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
  PERFORM f_definir_eventos.
  PERFORM layout.
  PERFORM f_alv.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = v_report
      is_layout          = gd_layout
      "I_CALLBACK_PF_STATUS_SET  = 'SET_PF_STATUS'
      "I_CALLBACK_USER_COMMAND   = 'USER_COMMAND'
      it_fieldcat        = it_fcat[]
      it_sort            = t_sort[]
      i_save             = 'X'
      it_events          = events
      is_print           = t_print
*     IS_VARIANT         = VG_VARIANT
    TABLES
      t_outtab           = it_saida.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                    slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                      " F_CARREGAR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM layout .
  gd_layout-no_input          = 'X'.
  gd_layout-zebra             = 'X'.
  gd_layout-colwidth_optimize = 'X'.
  "GD_LAYOUT-BOX_FIELDNAME     = 'SEL'.
  gd_layout-box_tabname       = 'IT_SAIDA'.
  gd_layout-window_titlebar   = 'Relatório de Recolhimento de Diferencial de Alíquotas '.
  gd_layout-detail_titlebar   = 'Relatório de Recolhimento de Diferencial de Alíquotas '.
ENDFORM.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv .

  PERFORM alv_preenche_cat USING:
                'EBELN'       TEXT-002       '15'       ' '     ' '    ' ' , " Pedido
                'EBELP'       TEXT-003       '08'       ' '     ' '    ' ' , " Item
                'AEDAT'       TEXT-004       '15'       ' '     ' '    ' ' , " Dt.Criação Ped
                'MATNR'       TEXT-005       '18'       ' '     ' '    ' ' , " Material
                'TXZ01'       TEXT-006       '40'       ' '     ' '    ' ' , " Descrição.
                'J_1BNBM'     TEXT-007       '15'       ' '     ' '    ' ' , " NCM
                'TEXT1'       TEXT-008       '25'       ' '     ' '    ' ' , " Descrição NCM
                'LIFNR'       TEXT-009       '15'       ' '     ' '    ' ' , " Fornecedor
                'NAME1L'      TEXT-010       '40'       ' '     ' '    ' ' , " Nome do Fornecedor
                'REGIOL'      TEXT-011       '10'       ' '     ' '    ' ' , " UF Fonr.
                'WERKS'       TEXT-012       '10'       ' '     ' '    ' ' , " Centro
                'NAME1T'      TEXT-013       '30'       ' '     ' '    ' ' , " Nome do Centro
                'REGIOT'      TEXT-014       '10'       ' '     ' '    ' ' , " UF Centro
                'EKGRP'       TEXT-015       '15'       ' '     ' '    ' ' , " Comprador
                'EKNAM'       TEXT-016       '30'       ' '     ' '    ' ' , " Nome do Comprador
                'EKTEL'       TEXT-017       '30'       ' '     ' '    ' ' . " Nome do Comprador

ENDFORM.                    " F_AL
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM alv_preenche_cat  USING   p_campo  TYPE c
                               p_desc   TYPE c
                               p_tam    TYPE c
                               p_hot    TYPE c
                               p_zero   TYPE c
                               p_soma   TYPE c.


  DATA: wl_fcat TYPE ty_estrutura.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_l = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-do_sum    = p_soma.
  IF p_campo = 'ICON'.
    wl_fcat-icon      = 'X'.
  ENDIF.
  APPEND wl_fcat TO it_fcat.
ENDFORM.                    " ALV_PREENCHE_CAT
