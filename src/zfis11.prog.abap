**&---------------------------------------------------------------------*
*& Report  ZFIS11
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfis11.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis.
TABLES:rbkp,ltvariant.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_rbkp,
         zuonr      TYPE rbkp-zuonr,
         belnr      TYPE rbkp-belnr,
         bukrs      TYPE rbkp-bukrs,
         gsber      TYPE rbkp-gsber,
         bldat      TYPE rbkp-bldat,
         budat      TYPE rbkp-budat,
         xblnr      TYPE rbkp-xblnr,
         rmwwr      TYPE rbkp-rmwwr,
         j_1bnftype TYPE rbkp-j_1bnftype,
         lifnr      TYPE rbkp-lifnr,
         usnam      TYPE rbkp-usnam,
         mwskz1     TYPE rbkp-mwskz1,
         zbd1t      TYPE rbkp-zbd1t,
         zfbdt      TYPE rbkp-zfbdt,
         cpudt      TYPE rbkp-cpudt,
         cputm      TYPE rbkp-cputm,
         waers      TYPE rbkp-waers,
         zlsch      TYPE rbkp-zlsch,
       END   OF ty_rbkp,

       BEGIN OF ty_j_1bnfdoc,
         belnr  TYPE j_1bnfdoc-belnr,
         docnum TYPE j_1bnfdoc-docnum,
         pstdat TYPE j_1bnfdoc-pstdat,
         bukrs  TYPE j_1bnfdoc-bukrs,
         series TYPE j_1bnfdoc-series,
       END   OF ty_j_1bnfdoc,

       BEGIN OF ty_j_1bnfstx,
         docnum   TYPE j_1bnfstx-docnum,
         taxtyp   TYPE j_1bnfstx-taxtyp,
         taxval   TYPE j_1bnfstx-taxval,
         basered1 TYPE j_1bnfstx-basered1,
       END   OF ty_j_1bnfstx,

       BEGIN OF ty_j_1baj,
         taxtyp TYPE j_1baj-taxtyp,
         taxgrp TYPE j_1baj-taxgrp,
       END   OF ty_j_1baj,

       BEGIN OF ty_j_1bnflin,
         docnum TYPE j_1bnflin-docnum,
         cfop   TYPE j_1bnflin-cfop,
         menge  TYPE j_1bnflin-menge,
         meins  TYPE j_1bnflin-meins,
       END   OF ty_j_1bnflin,
*MENGE quantidade
*MEINS unidade
       BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END   OF ty_t001,

       BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END   OF ty_t001w,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         stcd1 TYPE lfa1-stcd1,
       END   OF ty_lfa1.

*       BEGIN OF ty_saida,
*         bukrs      TYPE rbkp-bukrs,
*         butxt      TYPE t001-butxt,
*         gsber      TYPE rbkp-gsber,
*         name       TYPE t001w-name1,
*         data(25)   TYPE c,
*         user       TYPE sy-uname,
*         zuonr      TYPE rbkp-zuonr,
*         belnr      TYPE rbkp-belnr,
*         bldat      TYPE rbkp-bldat,
*         budat      TYPE rbkp-budat,
*         waers      TYPE rbkp-waers,
*         xblnr      TYPE rbkp-xblnr,
*         rmwwr      TYPE rbkp-rmwwr,
*         pis        TYPE j_1bnfstx-taxval,
*         base_icms  TYPE j_1bnfstx-basered1,
*         icms       TYPE j_1bnfstx-taxval,
*         cofins     TYPE j_1bnfstx-taxval,
*         ipi        TYPE j_1bnfstx-taxval,
*         j_1bnftype TYPE rbkp-j_1bnftype,
*         mwskz1     TYPE rbkp-mwskz1,
*         cfop       TYPE j_1bnflin-cfop,
*         dt_venc    TYPE rbkp-zfbdt,
*         lifnr      TYPE rbkp-lifnr,
*         name1      TYPE lfa1-name1,
*         usnam      TYPE rbkp-usnam,
*         cpudt      TYPE rbkp-cpudt,
*         cputm      TYPE rbkp-cputm,
*         series     TYPE j_1bnfdoc-series,
*         menge      TYPE j_1bnflin-menge,
*         meins      TYPE j_1bnflin-meins,
*         stcd1      TYPE lfa1-stcd1,
*         zlsch      TYPE rbkp-zlsch,
*       END   OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

*&---------------------------------------------------------------------*
*& CONSTANTES
*&---------------------------------------------------------------------*
CONSTANTS: c_pis(3)    TYPE c VALUE 'PIS',
           c_ipi(3)    TYPE c VALUE 'IPI',
           c_icms(4)   TYPE c VALUE 'ICMS',
           c_cofins(6) TYPE c VALUE 'COFI'.
*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
DATA: t_rbkp      TYPE TABLE OF ty_rbkp,
      t_j_1bnfdoc TYPE TABLE OF ty_j_1bnfdoc,
      t_j_1bnfstx TYPE TABLE OF ty_j_1bnfstx,
      t_j_1baj    TYPE TABLE OF ty_j_1baj,
      t_j_1bnflin TYPE TABLE OF ty_j_1bnflin,
      t_t001      TYPE TABLE OF ty_t001,
      t_t001w     TYPE TABLE OF ty_t001w,
      t_lfa1      TYPE TABLE OF ty_lfa1,
      t_saida     TYPE TABLE OF zfie0002.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA: wa_rbkp      TYPE ty_rbkp,
      wa_j_1bnfdoc TYPE ty_j_1bnfdoc,
      wa_j_1bnfstx TYPE ty_j_1bnfstx,
      wa_j_1baj    TYPE ty_j_1baj,
      wa_j_1bnflin TYPE ty_j_1bnflin,
      wa_t001      TYPE ty_t001,
      wa_t001w     TYPE ty_t001w,
      wa_lfa1      TYPE ty_lfa1,
      wa_saida     TYPE zfie0002.
*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*
DATA: x_data        TYPE d,
      contador_coll TYPE i,
      it_lvc_t_fcat TYPE slis_t_fieldcat_alv,
      x_hora        TYPE sy-uzeit.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR  rbkp-bukrs OBLIGATORY,
                s_gsber FOR  rbkp-gsber OBLIGATORY,
                s_budat FOR  rbkp-budat OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_layout TYPE slis_vari.
*            PA_NMAX   LIKE ITEMSET-NMAX.
SELECTION-SCREEN: END OF BLOCK b2.

INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM alv_variant_f4 CHANGING p_layout.


AT SELECTION-SCREEN OUTPUT.
  MOVE '/CONF' TO p_layout.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM f_iniciar_variaves.
  PERFORM f_seleciona_dados.
  PERFORM f_organiza_dados.
  PERFORM f_imprime_dados.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .
  SELECT zuonr belnr      bukrs gsber bldat  budat xblnr
         rmwwr j_1bnftype lifnr usnam mwskz1 zbd1t
         zfbdt cpudt      cputm waers zlsch
    FROM rbkp
    INTO TABLE t_rbkp
     WHERE  budat IN s_budat
       AND  bukrs IN s_bukrs
       AND  gsber IN s_gsber.

  IF sy-subrc IS INITIAL.
    SELECT lifnr name1 stcd1
      FROM lfa1
      INTO TABLE t_lfa1
      FOR ALL ENTRIES IN t_rbkp
       WHERE lifnr EQ t_rbkp-lifnr.

    SELECT bukrs butxt
      FROM t001
      INTO TABLE t_t001
      FOR ALL ENTRIES IN t_rbkp
        WHERE bukrs EQ t_rbkp-bukrs.

    SELECT werks name1
      FROM t001w
      INTO TABLE t_t001w
      FOR ALL ENTRIES IN t_rbkp
       WHERE werks EQ t_rbkp-gsber.

    SELECT belnr docnum pstdat bukrs series
      FROM j_1bnfdoc
      INTO TABLE t_j_1bnfdoc
      FOR ALL ENTRIES IN t_rbkp
       WHERE belnr  EQ t_rbkp-belnr
         AND pstdat EQ t_rbkp-budat
         AND bukrs  EQ t_rbkp-bukrs.

    IF sy-subrc IS INITIAL.
      SELECT docnum taxtyp taxval basered1
        FROM j_1bnfstx
        INTO TABLE t_j_1bnfstx
        FOR ALL ENTRIES IN t_j_1bnfdoc
         WHERE docnum EQ t_j_1bnfdoc-docnum.

      SELECT docnum cfop menge meins
        FROM j_1bnflin
        INTO TABLE t_j_1bnflin
        FOR ALL ENTRIES IN t_j_1bnfdoc
         WHERE docnum EQ t_j_1bnfdoc-docnum.

      IF sy-subrc IS INITIAL.
        SELECT taxtyp taxgrp
          FROM j_1baj
          INTO TABLE t_j_1baj
          FOR ALL ENTRIES IN t_j_1bnfstx
           WHERE taxtyp EQ t_j_1bnfstx-taxtyp.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_organiza_dados .

  SORT: t_j_1bnfdoc BY belnr pstdat bukrs,
        t_j_1bnflin BY docnum,
        t_lfa1      BY lifnr,
        t_t001      BY bukrs,
        t_t001w     BY werks,
        t_j_1baj    BY taxtyp.


  LOOP AT t_rbkp INTO wa_rbkp.


    READ TABLE t_j_1bnfdoc INTO wa_j_1bnfdoc
      WITH KEY belnr  = wa_rbkp-belnr
               pstdat = wa_rbkp-budat
               bukrs  = wa_rbkp-bukrs
                BINARY SEARCH.

    READ TABLE t_lfa1 INTO wa_lfa1
          WITH KEY lifnr = wa_rbkp-lifnr
                   BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_saida-stcd1      = wa_lfa1-stcd1.
    ENDIF.

    READ TABLE t_t001 INTO wa_t001
      WITH KEY bukrs = wa_rbkp-bukrs
               BINARY SEARCH.

    READ TABLE t_t001w INTO wa_t001w
      WITH KEY werks = wa_rbkp-gsber
               BINARY SEARCH.


    IF sy-subrc IS INITIAL.
      READ TABLE t_j_1bnflin INTO wa_j_1bnflin
        WITH KEY docnum = wa_j_1bnfdoc-docnum
                  BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        LOOP AT t_j_1bnfstx INTO wa_j_1bnfstx
          WHERE docnum EQ wa_j_1bnfdoc-docnum.

          READ TABLE t_j_1baj INTO wa_j_1baj
            WITH KEY taxtyp = wa_j_1bnfstx-taxtyp
                      BINARY SEARCH.

          IF sy-subrc IS INITIAL.
            IF wa_j_1baj-taxgrp EQ c_pis.
*           PIS
              wa_saida-pis = wa_j_1bnfstx-taxval.

            ELSEIF wa_j_1baj-taxgrp EQ c_icms.
*           ICMS
              wa_saida-icms = wa_j_1bnfstx-taxval.
              wa_saida-base_icms = wa_j_1bnfstx-basered1.

            ELSEIF wa_j_1baj-taxgrp EQ c_cofins.
*           COFINS
              wa_saida-cofins = wa_j_1bnfstx-taxval.

            ELSEIF wa_j_1baj-taxgrp EQ c_ipi.
*           IPI
              wa_saida-ipi = wa_j_1bnfstx-taxval.

            ENDIF.

          ENDIF.

        ENDLOOP.


      ENDIF.
    ENDIF.

    x_data = sy-datum.
    x_hora = sy-uzeit.

    CONCATENATE x_data+6(2) '/'
                x_data+4(2) '/'
                x_data(4)   ' -  '
                x_hora(2)   ':'
                x_hora+2(2) ':'
                x_hora+4(2) INTO wa_saida-data.

    wa_saida-bukrs      = wa_rbkp-bukrs.
    wa_saida-butxt      = wa_t001-butxt.
    wa_saida-gsber      = wa_rbkp-gsber.
    wa_saida-name       = wa_t001w-name1.
    wa_saida-user       = sy-uname.
    wa_saida-zuonr      = wa_rbkp-zuonr.
    wa_saida-belnr      = wa_rbkp-belnr.
    wa_saida-bldat      = wa_rbkp-bldat.
    wa_saida-budat      = wa_rbkp-budat.
    wa_saida-xblnr      = wa_rbkp-xblnr.
    wa_saida-waers      = wa_rbkp-waers.
    wa_saida-rmwwr      = wa_rbkp-rmwwr.
    wa_saida-j_1bnftype = wa_rbkp-j_1bnftype.
    wa_saida-mwskz1     = wa_rbkp-mwskz1.
    wa_saida-cfop       = wa_j_1bnflin-cfop.
    wa_saida-dt_venc    = wa_rbkp-zfbdt + wa_rbkp-zbd1t.
    wa_saida-lifnr      = wa_rbkp-lifnr.
    wa_saida-name1      = wa_lfa1-name1.
    wa_saida-usnam      = wa_rbkp-usnam.
    wa_saida-cpudt      = wa_rbkp-cpudt.
    wa_saida-cputm      = wa_rbkp-cputm.
    wa_saida-series     = wa_j_1bnfdoc-series.
    wa_saida-menge      = wa_j_1bnflin-menge.
    wa_saida-meins      = wa_j_1bnflin-meins.
    wa_saida-zlsch      = wa_rbkp-zlsch.

    APPEND wa_saida TO t_saida.

    CLEAR: wa_saida, wa_rbkp, wa_lfa1, wa_j_1bnfdoc,
           wa_j_1bnfstx, wa_j_1baj, wa_j_1bnflin, x_data, x_hora.


  ENDLOOP.

ENDFORM.                    " F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .
  DATA: layout TYPE disvariant.

  IF t_saida[] IS INITIAL.
    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.
  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_montar_layout.

  layout-variant = p_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = it_lvc_t_fcat "estrutura[]
      it_sort                 = t_sort[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
      is_variant              = layout
    TABLES
      t_outtab                = t_saida.

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
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_montar_layout.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZFIE0002'
    CHANGING
      ct_fieldcat            = it_lvc_t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  contador_coll = 0.

  LOOP AT it_lvc_t_fcat ASSIGNING FIELD-SYMBOL(<f_lvc_t_fcat>).
    <f_lvc_t_fcat>-no_out     = abap_true.
    <f_lvc_t_fcat>-tabname    = 'T_SAIDA'.
    <f_lvc_t_fcat>-key        = ' '.
    <f_lvc_t_fcat>-key_sel    = 'X'.

    IF <f_lvc_t_fcat>-fieldname EQ 'BELNR'.
      <f_lvc_t_fcat>-hotspot = 'X'.
    ENDIF.

    IF <f_lvc_t_fcat>-fieldname EQ 'ZLSCH'.
      <f_lvc_t_fcat>-just = 'C'.
    ENDIF.
  ENDLOOP.

  PERFORM f_montar_estrutura USING:
 3  'RBKP'        'GSBER'      'T_SAIDA' 'GSBER'       'Divisão'            ' ',
 7  'RBKP'        'ZUONR'      'T_SAIDA' 'ZUONR'       'Pedido'             ' ',
 8  'RBKP'        'BELNR'      'T_SAIDA' 'BELNR'       'Doc.Fat.'           ' ',
 9  'RBKP'        'BLDAT'      'T_SAIDA' 'BLDAT'       'Dt.Doc.'            ' ',
10  'RBKP'        'BUDAT'      'T_SAIDA' 'BUDAT'       'Dt.Lanc.'           ' ',
11  'RBKP'        'XBLNR'      'T_SAIDA' 'XBLNR'       'Nro.NF.'            ' ',
12  'J_1BNFDOC'   'SERIES'     'T_SAIDA' 'SERIES'      'Série'              ' ',
13  'RBKP'        'WAERS'      'T_SAIDA' 'WAERS'       'Moeda'              ' ',
14  'RBKP'        'RMWWR'      'T_SAIDA' 'RMWWR'       'Vlr.NF'             ' ',
15  'J_1BNFSTX'   'TAXVAL'     'T_SAIDA' 'PIS'         'Vlr.PIS'            ' ',
16  'J_1BNFSTX'   'BASERED1'   'T_SAIDA' 'BASE_ICMS'   'Base Icms'          ' ',
17  'J_1BNFSTX'   'TAXVAL'     'T_SAIDA' 'ICMS'        'Vlr.ICMS'           ' ',
18  'J_1BNFSTX'   'TAXVAL'     'T_SAIDA' 'COFINS'      'Vlr.COFINS'         ' ',
19  'J_1BNFSTX'   'TAXVAL'     'T_SAIDA' 'IPI'         'Vlr.IPI'            ' ',
20  'RBKP'        'J_1BNFTYPE' 'T_SAIDA' 'J_1BNFTYPE'  'Ct.NF'              ' ',
21  'J_1BNFLIN'   'CFOP'       'T_SAIDA' 'CFOP'        'CFOP'               ' ',
22  'RBKP'        'ZFBDT'      'T_SAIDA' 'DT_VENC'     'Dt.Vencimento'      ' ',
22  'RBKP'        'ZLSCH'      'T_SAIDA' 'ZLSCH'       'Frm.Pgto'           ' ',
23  'RBKP'        'LIFNR'      'T_SAIDA' 'LIFNR'       'Cod.Forn.'          ' ',
24  'LFA1'        'NAME1'      'T_SAIDA' 'NAME1'       'Nome do Fornecedor' ' ',
24  'LFA1'        'STCD1'      'T_SAIDA' 'STCD1'       'Cnpj'               ' ',
25  'RBKP'        'GEWEI'      'T_SAIDA' 'GEWEI'       'Un'                 ' ',
26  'RBKP'        'USNAM'      'T_SAIDA' 'USNAM'       'Usuário'            ' ',
27  'RBKP'        'CPUDT'      'T_SAIDA' 'CPUDT'       'Data Entrada'       ' ',
28  'RBKP'        'CPUTM'      'T_SAIDA' 'CPUTM'       'Hora da Entrada'    ' ',
27  'J_1BNFLIN'   'MENGE'      'T_SAIDA' 'MENGE'       'Quantidade'         ' ',
27  'J_1BNFLIN'   'MEINS'      'T_SAIDA' 'MEINS'       'Unidade'            ' '.

ENDFORM.                    " F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  ADD 1 TO contador_coll.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = contador_coll.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.


  IF p_field EQ 'BELNR'.
    wa_estrutura-hotspot = 'X'.
  ELSE.
    CLEAR wa_estrutura-hotspot.
  ENDIF.

  IF p_field EQ 'ZLSCH'.
    wa_estrutura-just = 'C'.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

  LOOP AT it_lvc_t_fcat ASSIGNING FIELD-SYMBOL(<f_lvc_t_fcat>) WHERE fieldname = p_field.
    <f_lvc_t_fcat>-no_out        = abap_false.
    <f_lvc_t_fcat>-outputlen     = x_contador.
    <f_lvc_t_fcat>-seltext_s     = p_scrtext_l.
    <f_lvc_t_fcat>-seltext_m     = p_scrtext_l.
    <f_lvc_t_fcat>-seltext_l     = p_scrtext_l.
    <f_lvc_t_fcat>-reptext_ddic  = p_scrtext_l.
  ENDLOOP.

ENDFORM.                    " F_montar_estrutura

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves.
  DATA: w_texto1(40).
  DATA: w_texto2(20).
  READ TABLE s_bukrs INDEX 1.
  v_report = sy-repid.


*** Nome do Report
  PERFORM f_construir_cabecalho USING 'H' text-002.

  SELECT SINGLE butxt FROM t001 INTO w_texto2
    WHERE bukrs IN s_bukrs.

  CONCATENATE 'Empresa:' s_bukrs-low '-' w_texto2 INTO w_texto1 SEPARATED BY space.
*** Nome da empresa
  PERFORM f_construir_cabecalho USING 'H' w_texto1.

  SELECT SINGLE name1 FROM t001w INTO w_texto2
    WHERE werks = s_gsber.

  CONCATENATE 'Filial:' s_gsber  '-' w_texto2 INTO  w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.

  WRITE: sy-datum TO w_texto2.
  CONCATENATE 'Data:' w_texto2 INTO w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.
  WRITE: sy-uzeit TO w_texto2.
  CONCATENATE 'Hora:' w_texto2 INTO w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.
  CONCATENATE 'Usuário:' sy-uname INTO w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.
ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.


*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'A'.
*  LS_LINE-KEY = 'QUEBRA'.
*  LS_LINE-INFO = ' '.
*  APPEND LS_LINE TO T_TOP.



ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort.

*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUKRS'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 1.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUTXT'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 2.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'GSBER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 3.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'NAME'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 4.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'DATA'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 5.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'USER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 6.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM f_user_command USING l_ucomm
                          l_selfield TYPE slis_selfield.

  IF l_selfield-fieldname = 'BELNR'.
    READ TABLE t_saida INDEX l_selfield-tabindex INTO wa_saida.

    SET PARAMETER ID 'RBN' FIELD l_selfield-value.
    SET PARAMETER ID 'GJR' FIELD wa_saida-bldat(4).

    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

  ENDIF.

ENDFORM.                    "f_user_command

*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
FORM alv_variant_f4 CHANGING pa_vari.
  DATA: rs_variant LIKE disvariant.
  DATA nof4 TYPE c.

  CLEAR nof4.
  LOOP AT SCREEN.
    IF screen-name = 'PA_VARI'.
      IF screen-input = 0.
        nof4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc = 0 AND nof4 EQ space.
    pa_vari = rs_variant-variant.
  ENDIF.
ENDFORM.                               " ALV_VARIANT_F4
