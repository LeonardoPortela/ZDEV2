*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 18/06/2014                                              &*
*& Descrição: Fluxo Financeiro – Relatório Pagamentos Realizados      &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         18.06.2014                            &*
*&--------------------------------------------------------------------&*

REPORT  zfir0055.


TABLES: t856t, zfit0121, zfit0080.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo..

*----------------------------------------------------------------------*
*  TABELA INTERNA
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_0080,
         bukrs    TYPE zfit0080-bukrs,
         lifnr    TYPE zfit0080-lifnr,
         kunnr    TYPE zfit0080-kunnr,
         hkont    TYPE zfit0080-hkont,
         cod_flx  TYPE zfit0080-cod_flx,
         rmvct    TYPE zfit0080-rmvct,
         budat    TYPE zfit0080-budat,
         belnr    TYPE zfit0080-belnr,
         gjahr    TYPE zfit0080-gjahr,
         buzei    TYPE zfit0080-buzei,
         zfbdt    TYPE zfit0080-zfbdt,
         bldat    TYPE zfit0080-bldat,
         waers    TYPE zfit0080-waers,
         xblnr    TYPE zfit0080-xblnr,
         blart    TYPE zfit0080-blart,
         gsber    TYPE zfit0080-gsber,
         ebeln    TYPE zfit0080-ebeln,
         ebelp    TYPE zfit0080-ebelp,
         bschl    TYPE zfit0080-bschl,
         shkzg    TYPE zfit0080-shkzg,
         dmbtr    TYPE zfit0080-dmbtr,
         dmbe2    TYPE zfit0080-dmbe2,
         usnam    TYPE zfit0080-usnam,
         dt_atual TYPE zfit0080-dt_atual,
         hr_atual TYPE zfit0080-hr_atual,
         augdt    TYPE zfit0080-augdt,
         augbl    TYPE zfit0080-augbl,
         stblg    TYPE zfit0080-stblg,
         saknr    TYPE zfit0080-saknr,
         manual   TYPE c,
       END OF ty_0080,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF ty_lfa1,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,


       BEGIN OF ty_skat,
         ktopl TYPE skat-ktopl,
         saknr TYPE skat-saknr,
         txt50 TYPE skat-txt50,
       END OF ty_skat,

       BEGIN OF ty_0077,
         cod_flx  TYPE zfit0077-cod_flx,
         desc_flx TYPE zfit0077-desc_flx,
       END OF ty_0077,

       BEGIN OF ty_saida,
         lcto_mn    TYPE c,
         cls_mn     TYPE c,
         bukrs      TYPE zfit0080-bukrs,
         lifnr      TYPE zfit0080-lifnr,
         name1      TYPE lfa1-name1,
         kunnr      TYPE zfit0080-kunnr,
         name1_kn   TYPE kna1-name1,
         hkont      TYPE zfit0080-hkont,
         nome_conta TYPE skat-txt50,
         cod_flx    TYPE zfit0080-cod_flx,
         desc_flx   TYPE zfit0077-desc_flx,
         rmvct      TYPE zfit0080-rmvct,
         belnr      TYPE zfit0080-belnr,
         gjahr      TYPE zfit0080-gjahr,
         buzei      TYPE zfit0080-buzei,
         augbl      TYPE zfit0080-augbl,
         stblg      TYPE zfit0080-stblg,
         budat      TYPE zfit0080-budat,
         bldat      TYPE zfit0080-bldat,
         augdt      TYPE zfit0080-augdt,
         blart      TYPE zfit0080-blart,
         waers      TYPE zfit0080-waers,
         zfbdt      TYPE zfit0080-zfbdt,
         valor_rs   TYPE zfit0080-dmbtr,
         valor_uss  TYPE zfit0080-dmbe2,
         xblnr      TYPE zfit0080-xblnr,
         ebeln      TYPE zfit0080-ebeln,
         ebelp      TYPE zfit0080-ebelp,
         bschl      TYPE zfit0080-bschl,
         saknr      TYPE zfit0080-saknr,
         cta_banco  TYPE skat-txt50,
         usnam      TYPE zfit0080-usnam,
         dt_atual   TYPE zfit0080-dt_atual,
         hr_atual   TYPE zfit0080-hr_atual,
       END OF ty_saida,

       BEGIN OF ty_saida_0103,
         saknr TYPE  skb1-saknr,
         txt50 TYPE  skat-txt50,
         dmbtr TYPE  dmbtr,
         dmbe2 TYPE  dmbe2,
       END OF ty_saida_0103.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.



*----------------------------------------------------------------------*
*  TABELAS INTERNAS
*----------------------------------------------------------------------*
DATA: t_0080       TYPE TABLE OF ty_0080,
      t_lfa1       TYPE TABLE OF ty_lfa1,
      t_kna1       TYPE TABLE OF ty_kna1,
      t_skat       TYPE TABLE OF ty_skat,
      t_skat_aux   TYPE TABLE OF ty_skat,
      t_0077       TYPE TABLE OF ty_0077,
      t_0121       TYPE TABLE OF zfit0121,
      t_0121_mn    TYPE TABLE OF zfit0121 WITH HEADER LINE,
      t_saida      TYPE TABLE OF ty_saida,
      t_saida_0103 TYPE TABLE OF ty_saida_0103.
*----------------------------------------------------------------------*
*  WORK AREA
*----------------------------------------------------------------------*
DATA: wa_0080       TYPE ty_0080,
      wa_lfa1       TYPE ty_lfa1,
      wa_kna1       TYPE ty_kna1,
      wa_skat       TYPE ty_skat,
      wa_skat_aux   TYPE ty_skat,
      wa_0077       TYPE ty_0077,
      wa_0121       TYPE zfit0121,
      wa_0121_mn    TYPE zfit0121,
      wa_saida      TYPE ty_saida,
      wa_saida_0103 TYPE ty_saida_0103.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS lcl_alv_toolbar DEFINITION.
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

CLASS lcl_alv_toolbar_0103 DEFINITION.
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

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.


*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader.

DATA: obj_alv       TYPE REF TO cl_gui_alv_grid,
      obj_container TYPE REF TO cl_gui_custom_container.

DATA: obj_alv_0103       TYPE REF TO cl_gui_alv_grid,
      obj_container_0103 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row.

DATA: obj_toolbar       TYPE REF TO lcl_alv_toolbar,
      obj_toolbar_0103  TYPE REF TO lcl_alv_toolbar_0103,
      lcl_event_handler TYPE REF TO lcl_event_handler.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*

DATA: wl_bukrs   TYPE char70,
      wl_lifnr   TYPE char70,
      wl_cod_flx TYPE char70,
      wl_budat   TYPE char70,
      wl_augdt   TYPE char70,
      wl_cls_flx TYPE zfied002.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs   FOR wa_0080-bukrs   OBLIGATORY,
                s_lifnr   FOR wa_0080-lifnr,
                s_hkont   FOR wa_0080-hkont,
                s_cod_fl  FOR wa_0080-cod_flx,
                s_budat   FOR wa_0080-budat,
                s_augdt   FOR wa_0080-augdt,
                s_rmvct   FOR t856t-trtyp,
                s_waers   FOR wa_0080-waers NO-DISPLAY.
SELECTION-SCREEN: END  OF BLOCK b1.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.


  PERFORM selecionar_dados.
  PERFORM organizar_dados.
  PERFORM iniciar_variaveis.
  PERFORM imprimir_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados .

  REFRESH: t_0080,
           t_lfa1,
           t_kna1,
           t_skat,
           t_skat_aux,
           t_0077,
           t_0121,
           t_0121_mn,
           t_saida.


  SELECT bukrs lifnr kunnr hkont cod_flx rmvct budat belnr gjahr buzei
         zfbdt bldat waers xblnr blart gsber ebeln
         ebelp bschl shkzg dmbtr dmbe2 usnam dt_atual
         hr_atual augdt augbl saknr manual
    FROM zfit0080 INTO CORRESPONDING FIELDS OF TABLE t_0080
      WHERE bukrs   IN s_bukrs
        AND lifnr   IN s_lifnr
        AND hkont   IN s_hkont
        AND cod_flx IN s_cod_fl
        AND budat   IN s_budat
        AND augdt   IN s_augdt
        AND waers   IN s_waers
        AND rmvct   IN s_rmvct
        AND sld_contas EQ ''.

  "Buscar lançamentos manuais
*  SELECT *
*    FROM zfit0121 AS a INTO TABLE t_0121_mn
*   WHERE a~bukrs    IN s_bukrs
*     AND a~lifnr    IN s_lifnr
*     AND a~hkont    IN s_hkont
*     AND a~cod_flx  IN s_cod_fl
*     AND a~budat    IN s_budat
*     and a~augdt    in s_augdt
*     AND a~waers    in s_waers
*     AND a~rmvct    IN s_rmvct
*     and a~manual   EQ 'X'
*     and not exists ( select *
*                        from zfit0080 as b
*                       where b~bukrs = a~bukrs
*                         and b~belnr = a~belnr
*                         and b~gjahr = a~gjahr
*                         and b~buzei = a~buzei ).
*
*  LOOP AT t_0121_mn.
*    CLEAR: wa_0080.
*    MOVE-CORRESPONDING t_0121_mn to wa_0080.
*    wa_0080-lcto_mn = 'X'.
*    APPEND wa_0080 to t_0080.
*  ENDLOOP.


  IF  t_0080[] IS NOT INITIAL.
    SELECT lifnr name1
      FROM lfa1 INTO TABLE t_lfa1
      FOR ALL ENTRIES IN t_0080
        WHERE lifnr EQ t_0080-lifnr.

    SELECT kunnr name1
      FROM kna1 INTO TABLE t_kna1
      FOR ALL ENTRIES IN t_0080
        WHERE kunnr EQ t_0080-kunnr.

    SELECT ktopl saknr txt50
      FROM skat INTO TABLE t_skat
      FOR ALL ENTRIES IN t_0080
        WHERE saknr EQ t_0080-hkont
          AND ktopl EQ '0050'
          AND spras EQ sy-langu.

    SELECT ktopl saknr txt50
      FROM skat INTO TABLE t_skat_aux
      FOR ALL ENTRIES IN t_0080
        WHERE saknr EQ t_0080-saknr
          AND ktopl EQ '0050'
          AND spras EQ sy-langu.

    SELECT cod_flx desc_flx
      FROM zfit0077 INTO TABLE t_0077
      FOR ALL ENTRIES IN t_0080
      WHERE cod_flx EQ t_0080-cod_flx.

    SELECT *
      FROM zfit0121 INTO TABLE t_0121
      FOR ALL ENTRIES IN t_0080
      WHERE bukrs EQ t_0080-bukrs
        AND belnr EQ t_0080-belnr
        AND gjahr EQ t_0080-gjahr
        AND buzei EQ t_0080-buzei
        AND manual EQ ''.


*
*    SELECT cod_flx desc_flx
*      FROM zfit0077 APPENDING TABLE t_0077
*      FOR ALL ENTRIES IN t_0121
*        WHERE cod_flx EQ t_0121-cod_flx.



  ENDIF.


ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organizar_dados .

  LOOP AT t_0080 INTO wa_0080.

    CLEAR: wa_saida, wa_kna1, wa_lfa1, wa_skat, wa_skat_aux, wa_0077, wa_0121.

    READ TABLE t_lfa1 INTO wa_lfa1
      WITH KEY lifnr = wa_0080-lifnr.

    READ TABLE t_kna1 INTO wa_kna1
      WITH KEY kunnr = wa_0080-kunnr.

    READ TABLE t_skat INTO wa_skat
      WITH KEY saknr = wa_0080-hkont
               ktopl = '0050'.

    READ TABLE t_skat INTO wa_skat_aux
      WITH KEY saknr = wa_0080-hkont
               ktopl = '0050'.

    READ TABLE t_0121 INTO wa_0121
      WITH KEY bukrs = wa_0080-bukrs
               belnr = wa_0080-belnr
               gjahr = wa_0080-gjahr
               buzei = wa_0080-buzei.

    IF ( sy-subrc = 0 ) AND ( wa_0121-cod_flx IS NOT INITIAL ).
      wa_0080-cod_flx = wa_0121-cod_flx.
      wa_saida-cls_mn = 'X'. "#EC CI_DB_OPERATION_OK[2431747]
    ENDIF.

    READ TABLE t_0077 INTO wa_0077
      WITH KEY cod_flx = wa_0080-cod_flx.

    MOVE: wa_0080-bukrs     TO wa_saida-bukrs,
          wa_0080-lifnr     TO wa_saida-lifnr,
          wa_lfa1-name1     TO wa_saida-name1,
          wa_0080-kunnr     TO wa_saida-kunnr,
          wa_kna1-name1     TO wa_saida-name1_kn,
          wa_0080-hkont     TO wa_saida-hkont,
          wa_skat-txt50     TO wa_saida-nome_conta,
          wa_0080-cod_flx   TO wa_saida-cod_flx,
          wa_0077-desc_flx  TO wa_saida-desc_flx,
          wa_0080-rmvct     TO wa_saida-rmvct,
          wa_0080-belnr     TO wa_saida-belnr,
          wa_0080-gjahr     TO wa_saida-gjahr, "#EC CI_DB_OPERATION_OK[2431747]
          wa_0080-buzei     TO wa_saida-buzei,
          wa_0080-augbl     TO wa_saida-augbl,
          wa_0080-stblg     TO wa_saida-stblg,
          wa_0080-budat     TO wa_saida-budat,
          wa_0080-bldat     TO wa_saida-bldat,
          wa_0080-augdt     TO wa_saida-augdt,
          wa_0080-blart     TO wa_saida-blart,
          wa_0080-waers     TO wa_saida-waers,
          wa_0080-zfbdt     TO wa_saida-zfbdt,
          wa_0080-xblnr     TO wa_saida-xblnr,
          wa_0080-ebeln     TO wa_saida-ebeln,
          wa_0080-ebelp     TO wa_saida-ebelp,
          wa_0080-bschl     TO wa_saida-bschl,
          wa_0080-saknr     TO wa_saida-saknr,
          wa_skat_aux-txt50 TO wa_saida-cta_banco,
          wa_0080-usnam     TO wa_saida-usnam,
          wa_0080-dt_atual  TO wa_saida-dt_atual,
          wa_0080-hr_atual  TO wa_saida-hr_atual,
          wa_0080-dmbtr     TO wa_saida-valor_rs,
          wa_0080-dmbe2     TO wa_saida-valor_uss,
          wa_0080-manual    TO wa_saida-lcto_mn.

    "IF wa_0080-shkzg EQ 'H'.
    "  wa_saida-valor_rs  = wa_0080-dmbtr * -1.
    "  wa_saida-valor_uss = wa_0080-dmbe2 * -1.
    "ENDIF.

    APPEND wa_saida TO t_saida.
  ENDLOOP.


ENDFORM.                    " ORGANIZAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_dados .

  CALL SCREEN 0100.

*  DATA: wl_layout TYPE slis_layout_alv.
*  PERFORM definir_eventos.
*  PERFORM montar_layout." USING 'T_SAIDA'.
**  wl_layout-box_fieldname = 'MARK'.
**  wl_layout-box_tabname  = 'T_SAIDA'.
*
*  wl_layout-colwidth_optimize = 'X'.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program      = v_report
**      I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND' "sem 2º click
*      it_fieldcat             = estrutura[]
*      is_layout               = wl_layout
*      i_callback_user_command = 'USER_COMMAND'
*      i_save                  = 'A'
*      it_events               = events
**      IS_LAYOUT               = LAYOUT
*      is_print                = t_print
*    TABLES
*      t_outtab                = t_saida.



ENDFORM.                    "imprimir_dados


FORM user_command  USING r_ucomm      LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN: '&IC1'.

      IF ( rs_selfield-fieldname EQ 'AUGBL').
        READ TABLE t_saida INTO wa_saida INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD wa_saida-augbl.
        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_saida-augdt(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF ( rs_selfield-fieldname EQ 'BELNR' ).
        READ TABLE t_saida INTO wa_saida INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_saida-budat(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ELSEIF ( rs_selfield-fieldname EQ 'STBLG' ).
        READ TABLE t_saida INTO wa_saida INDEX rs_selfield-tabindex.
        SET PARAMETER ID 'BLN' FIELD wa_saida-stblg.
        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.                    "user_command


*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.

  PERFORM f_carregar_eventos USING:
                                  " slis_ev_user_command 'XUSER_COMMAND', "para tira duplo click
*                                   slis_ev_pf_status_set 'XPF_STATUS_SET',
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.


ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.

  PERFORM montar_estrutura USING:
        1  'ZFIT0080'   'LIFNR'      'T_SAIDA' 'LIFNR'       ' '                   ' ' ' ',     "Fornecedor
        2  'LFA1'       'NAME1'      'T_SAIDA' 'NAME1'       'Nome do Fornecedor'  ' ' ' ',     "Nome do Fornecedor
        3  'ZFIT0080'   'KUNNR'      'T_SAIDA' 'KUNNR'       ' '                   ' ' ' ',     "Cliente
        4  'KNA1'       'NAME1'      'T_SAIDA' 'NAME1_KN'    'Nome do Cliente'     ' ' ' ',     "Nome do Cliente
        5  'ZFIT0080'   'HKONT'      'T_SAIDA' 'HKONT'       'Conta'               ' ' ' ',     "Conta
        6  'SKAT'       'TXT50'      'T_SAIDA' 'NOME_CONTA'  'Nome da Conta'       ' ' ' ',     "Nome da Conta
        7  'ZFIT0080'   'COD_FLX'    'T_SAIDA' 'COD_FLX'     'Flx.Financ'          ' ' ' ',     "Flx.Financ.
        8  'ZFIT0077'   'DESC_FLX'   'T_SAIDA' 'DESC_FLX'    'Desc.Flx.Financ.'    ' ' ' ',     "Descrição Fluxo Financeiro
        8  'T856T'      'TRTYP'      'T_SAIDA' 'RMVCT'       'Tp.Mov.'             ' ' ' ',     "Tp. Mov
        9  'ZFIT0080'   'BELNR'      'T_SAIDA' 'BELNR'       'Doc. Contabil'       ' ' 'X',     "Doc. Contabil
        10 'ZFIT0080'   'AUGBL'      'T_SAIDA' 'AUGBL'       ' '                   ' ' 'X',     "Doc.compesação
        10 'ZFIT0080'   'STBLG'      'T_SAIDA' 'STBLG'       'Doc.Estorno'         ' ' 'X',     "Motivo Estorno
        11 'ZFIT0080'   'BUDAT'      'T_SAIDA' 'BUDAT'       ' '                   ' ' ' ',     "Dt.Lcto
        12 'ZFIT0080'   'BLDAT'      'T_SAIDA' 'BLDAT'       ' '                   ' ' ' ',     "Dt.Dcto
        13 'ZFIT0080'   'AUGDT'      'T_SAIDA' 'AUGDT'       ' '                   ' ' ' ',     "Dt.compensação
       14  'ZFIT0080'   'BLART'      'T_SAIDA' 'BLART'       ' '                   ' ' ' ',     "Tp.Docto
       15  'ZFIT0080'   'WAERS'      'T_SAIDA' 'WAERS'       ' '                   ' ' ' ',     "Moeda
       16  'ZFIT0080'   'ZFBDT'      'T_SAIDA' 'ZFBDT'       'Dt.Vcto'             ' ' ' ',     "Dt.Vcto
       17  'ZFIT0080'   'DMBTR'      'T_SAIDA' 'VALOR_RS'    'Valor R$'            ' ' ' ',     "Valor R$
       18  'ZFIT0080'   'DMBE2'      'T_SAIDA' 'VALOR_USS'   'Valor US$'           ' ' ' ',     "Valor US$
       19  'ZFIT0080'   'XBLNR'      'T_SAIDA' 'XBLNR'       ' '                   ' ' ' ',     "Referência
       20  'ZFIT0080'   'EBELN'      'T_SAIDA' 'EBELN'       ' '                   ' ' ' ',     "Doc.compra
       21  'ZFIT0080'   'EBELP'      'T_SAIDA' 'EBELP'       ' '                   ' ' ' ',     "Item
       22  'ZFIT0080'   'BSCHL'      'T_SAIDA' 'BSCHL'       ' '                   ' ' ' ',     "Chv.lc.
       23  'ZFIT0080'   'SAKNR'      'T_SAIDA' 'SAKNR'       'Conta Banco'         ' ' ' ',     "Conta Banco
       24  'ZFIT0080'   'TXT50'      'T_SAIDA' 'CTA_BANCO'   'Nome da Conta Banco' ' ' ' ',     "Nome da Conta Banco
       25  'ZFIT0080'   'USNAM'      'T_SAIDA' 'USNAM'       'Usuário Lc'          ' ' ' ',     "Usuario Lc.
       26  'ZFIT0080'   'DT_ATUAL'   'T_SAIDA' 'DT_ATUAL'    'Dt.Atual'            ' ' ' ',     "Dt.Atual.
       27  'ZFIT0080'   'HR_ATUAL'   'T_SAIDA' 'HR_ATUAL'    'Hr.Atual'            ' ' ' '.     "Hr.Atual

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_hotspot).

  CLEAR: wa_estrutura.


  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-hotspot       = p_hotspot.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.
*
*  IF P_FIELD EQ 'EBELN'.
*    WA_ESTRUTURA-HOTSPOT = 'X'.
*  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iniciar_variaveis.
  DATA: tl_t001         LIKE t001 OCCURS 0 WITH HEADER LINE,
        tl_lfa1         LIKE lfa1 OCCURS 0 WITH HEADER LINE,
        wl_de(10),
        wl_ate(10),
        wl_de_comp(10),
        wl_ate_comp(10),
        wl_text(50).

  CLEAR: wl_bukrs,
         wl_lifnr,
         wl_cod_flx,
         wl_budat.

  v_report = sy-repid.

  CONCATENATE s_budat-low+6(2)  '/' s_budat-low+4(2)  '/' s_budat-low(4)  INTO wl_de.
  CONCATENATE s_budat-high+6(2) '/' s_budat-high+4(2) '/' s_budat-high(4) INTO wl_ate.

  CONCATENATE s_augdt-low+6(2)  '/' s_augdt-low+4(2)  '/' s_augdt-low(4)  INTO wl_de_comp.
  CONCATENATE s_augdt-high+6(2) '/' s_augdt-high+4(2) '/' s_augdt-high(4) INTO wl_ate_comp.

  PERFORM f_construir_cabecalho USING 'H' text-002.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = s_bukrs-low
    IMPORTING
      output = s_bukrs-low.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = s_bukrs-high
    IMPORTING
      output = s_bukrs-high.

  IF s_bukrs-high IS NOT INITIAL.
    CONCATENATE 'Empresa:' s_bukrs-low 'até' s_bukrs-high
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    CONCATENATE s_bukrs-low 'até' s_bukrs-high
           INTO wl_bukrs SEPARATED BY space.
  ELSE.
    CONCATENATE 'Empresa:' s_bukrs-low
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    wl_bukrs = s_bukrs-low.
  ENDIF.

  CLEAR: wl_text.

  IF s_lifnr-high IS NOT INITIAL.
    CONCATENATE 'Fornecedor:' s_lifnr-low 'até' s_lifnr-high
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    CONCATENATE s_lifnr-low 'até' s_lifnr-high
           INTO wl_lifnr SEPARATED BY space.

  ELSE.
    CONCATENATE 'Fornecedor:' s_lifnr-low
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    wl_lifnr = s_lifnr-low.
  ENDIF.

  CLEAR: wl_text.
*
*  SELECT *
*    FROM t001 INTO TABLE tl_t001
*      WHERE bukrs IN s_bukrs.
*
*  select *
*    from lfa1 into table tl_lfa1
*      where lifnr in s_lifnr.
*  clear: WL_TEXT.
*
*  LOOP AT tl_t001.
*    READ TABLE s_bukrs INDEX sy-tabix.
*    CONCATENATE 'Empresa:' tl_t001-bukrs '-' tl_t001-butxt
*            INTO wl_text SEPARATED BY space.
*    PERFORM f_construir_cabecalho USING 'S' wl_text.
*  ENDLOOP.
*
*  clear: WL_TEXT.
*
*  LOOP AT tl_lfa1.
*    READ TABLE s_lifnr INDEX sy-tabix.
*    condense tl_lfa1-lifnr no-gaps.
*    CONCATENATE 'Fornecedor:' tl_lfa1-lifnr '-' tl_lfa1-name1
*           INTO wl_text SEPARATED BY space.
*    PERFORM f_construir_cabecalho USING 'S' wl_text.
*  ENDLOOP.
*
*  clear: WL_TEXT.

  IF s_cod_fl-high IS NOT INITIAL.
    CONCATENATE 'Cod. Fluxo Financeiro:' s_cod_fl-low 'até' s_cod_fl-high
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    CONCATENATE s_cod_fl-low 'até' s_cod_fl-high
           INTO wl_cod_flx SEPARATED BY space.
  ELSE.
    CONCATENATE 'Cod. Fluxo Financeiro:' s_cod_fl-low
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    wl_cod_flx = s_cod_fl-low.
  ENDIF.

  CLEAR: wl_text.

  IF s_budat-high IS NOT INITIAL.
    CONCATENATE 'Data de Lançamento:' wl_de 'até' wl_ate
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    CONCATENATE wl_de 'até' wl_ate
           INTO wl_budat SEPARATED BY space.

  ELSEIF s_budat-high IS INITIAL
     AND s_budat-low IS NOT INITIAL.
    CONCATENATE 'Data de Lançamento:' wl_de
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    wl_budat = wl_de.

  ENDIF.


  IF s_augdt-high IS NOT INITIAL.
    CONCATENATE 'Data de Compensação:' wl_de_comp 'até' wl_ate_comp
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    CONCATENATE wl_de_comp 'até' wl_ate_comp
           INTO wl_augdt SEPARATED BY space.

  ELSEIF s_augdt-high IS INITIAL
     AND s_augdt-low IS NOT INITIAL.
    CONCATENATE 'Data de Compensação:' wl_de_comp
           INTO wl_text SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' wl_text.

    wl_augdt = wl_de_comp.

  ENDIF.




ENDFORM.                    " INICIAR_VARIAVES

FORM handle_hotspot_click  USING    i_row_id     TYPE lvc_s_row
                                    i_column_id  TYPE lvc_s_col
                                    is_row_no    TYPE lvc_s_roid.

  DATA: opt     TYPE ctu_params,
        vl_lote TYPE zglt034-lote.

  CHECK ( i_row_id IS NOT INITIAL ).

  CLEAR: wa_saida.
  CASE i_column_id.
    WHEN 'BELNR'.
      CLEAR: wa_saida.
      READ TABLE t_saida INTO wa_saida INDEX i_row_id.

      CHECK wa_saida-belnr IS NOT INITIAL.

      SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
      SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
      SET PARAMETER ID 'GJR' FIELD wa_saida-budat(4).
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    WHEN 'AUGBL'.
      CLEAR: wa_saida.
      READ TABLE t_saida INTO wa_saida INDEX i_row_id.

      CHECK wa_saida-augbl IS NOT INITIAL.

      SET PARAMETER ID 'BLN' FIELD wa_saida-augbl.
      SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
      SET PARAMETER ID 'GJR' FIELD wa_saida-augdt(4).
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.


  ENDCASE.

ENDFORM.                    "HANDLE_HOTSPOT_CLICK

FORM refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.


ENDFORM.

FORM renovar.

  PERFORM selecionar_dados.
  PERFORM organizar_dados.

  CALL METHOD obj_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

FORM criar_field_catalog USING p_screen.

  CLEAR: wa_fcat, it_fcat, it_fcat[] .

  CASE p_screen.
    WHEN '0100'.

      PERFORM estrutura_alv USING:

        1  ''           'LCTO_MN'    'T_SAIDA' 'LCTO_MN'     'LM'                    '02'  ''    '' ' ' ' ' ' ' ' ',    "Lançamento manual
        1  ''           'CLS_MN'     'T_SAIDA' 'CLS_MN'      'CM'                    '02'  ''    '' ' ' ' ' ' ' ' ',    "Classificação Manual
        1  'ZFIT0080'   'LIFNR'      'T_SAIDA' 'LIFNR'       ' '                       ''  ''    '' ' ' ' ' ' ' ' ',    "Fornecedor                   "'10'
        2  'LFA1'       'NAME1'      'T_SAIDA' 'NAME1'       'Nome do Fornecedor'      ''  ''    '' ' ' ' ' ' ' ' ',    "Nome do Fornecedor           "'25'
        3  'ZFIT0080'   'KUNNR'      'T_SAIDA' 'KUNNR'       ' '                       ''  ''    '' ' ' ' ' ' ' ' ',    "Cliente                      "'11'
        4  'KNA1'       'NAME1'      'T_SAIDA' 'NAME1_KN'    'Nome do Cliente'         ''  ''    '' ' ' ' ' ' ' ' ',    "Nome do Cliente              "'20'
        5  'ZFIT0080'   'HKONT'      'T_SAIDA' 'HKONT'       'Conta'                   ''  ''    '' ' ' ' ' ' ' ' ',    "Conta                        "'10'
        6  'SKAT'       'TXT50'      'T_SAIDA' 'NOME_CONTA'  'Nome da Conta'           ''  ''    '' ' ' ' ' ' ' ' ',    "Nome da Conta                "'15'
        7  'ZFIT0080'   'COD_FLX'    'T_SAIDA' 'COD_FLX'     'Flx.Financ'              ''  ''    '' ' ' ' ' ' ' ' ',    "Flx.Financ.                  "'10'
        8  'ZFIT0077'   'DESC_FLX'   'T_SAIDA' 'DESC_FLX'    'Desc.Flx.Financ.'        ''  ''    '' ' ' ' ' ' ' ' ',    "Descrição Fluxo Financeiro   "'20'
        8  'T856T'      'TRTYP'      'T_SAIDA' 'RMVCT'       'Tp.Mov.'                 ''  ''    '' ' ' ' ' ' ' ' ',    "Tp. Mov                      "'07'
        9  'ZFIT0080'   'BELNR'      'T_SAIDA' 'BELNR'       'Doc.Cont.'               ''  ''    '' ' ' ' ' 'X' ' ',    "Doc. Contabil                "'10'
        10 'ZFIT0080'   'AUGBL'      'T_SAIDA' 'AUGBL'       'Doc.Comp.'               ''  ''    '' ' ' ' ' 'X' ' ',    "Doc.compesação               "'10'
        10 'ZFIT0080'   'STBLG'      'T_SAIDA' 'STBLG'       'Doc.Estorno'             ''  ''    '' ' ' ' ' 'X' ' ',    "Motivo Estorno               "'12'
        11 'ZFIT0080'   'BUDAT'      'T_SAIDA' 'BUDAT'       'Data Lcto.'              ''  ''    '' ' ' ' ' ' ' ' ',    "Dt.Lcto                      "'10'
        12 'ZFIT0080'   'BLDAT'      'T_SAIDA' 'BLDAT'       'Data Doc.'               ''  ''    '' ' ' ' ' ' ' ' ',    "Dt.Dcto                      "'10'
        13 'ZFIT0080'   'AUGDT'      'T_SAIDA' 'AUGDT'       'Data.Comp'               ''  ''    '' ' ' ' ' ' ' ' ',    "Dt.compensação               "'10'
       14  'ZFIT0080'   'BLART'      'T_SAIDA' 'BLART'       'Tp.Doc'                  ''  ''    '' ' ' ' ' ' ' ' ',    "Tp.Docto                     "'07'
       15  'ZFIT0080'   'WAERS'      'T_SAIDA' 'WAERS'       'Moeda'                   ''  ''    '' ' ' ' ' ' ' ' ',    "Moeda                        "'05'
       16  'ZFIT0080'   'ZFBDT'      'T_SAIDA' 'ZFBDT'       'Dt.Vcto'                 ''  ''    '' ' ' ' ' ' ' ' ',    "Dt.Vcto                      "'10'
       17  'ZFIT0080'   'DMBTR'      'T_SAIDA' 'VALOR_RS'    'Valor R$'                ''  ''    '' ' ' ' ' ' ' ' ',    "Valor R$                     "'13'
       18  'ZFIT0080'   'DMBE2'      'T_SAIDA' 'VALOR_USS'   'Valor US$'               ''  ''    '' ' ' ' ' ' ' ' ',    "Valor US$                    "'13'
       19  'ZFIT0080'   'XBLNR'      'T_SAIDA' 'XBLNR'       ' '                       ''  ''    '' ' ' ' ' ' ' ' ',    "Referência                   "'13'
       20  'ZFIT0080'   'EBELN'      'T_SAIDA' 'EBELN'       ' '                       ''  ''    '' ' ' ' ' ' ' ' ',    "Doc.compra                   "'06'
       21  'ZFIT0080'   'EBELP'      'T_SAIDA' 'EBELP'       ' '                       ''  ''    '' ' ' ' ' ' ' ' ',    "Item                         "'13'
       22  'ZFIT0080'   'BSCHL'      'T_SAIDA' 'BSCHL'       ' '                       ''  ''    '' ' ' ' ' ' ' ' ',    "Chv.lc.                      "'13'
       23  'ZFIT0080'   'SAKNR'      'T_SAIDA' 'SAKNR'       'Conta Banco'             ''  ''    '' ' ' ' ' ' ' ' ',    "Conta Banco                  "'13'
       24  'ZFIT0080'   'TXT50'      'T_SAIDA' 'CTA_BANCO'   'Nome da Conta Banco'     ''  ''    '' ' ' ' ' ' ' ' ',    "Nome da Conta Banco          "'20'
       25  'ZFIT0080'   'USNAM'      'T_SAIDA' 'USNAM'       'Us. Lcto.'               ''  ''    '' ' ' ' ' ' ' ' ',    "Usuario Lc.                  "'10'
       26  'ZFIT0080'   'DT_ATUAL'   'T_SAIDA' 'DT_ATUAL'    'Dt.Atual'                ''  ''    '' ' ' ' ' ' ' ' ',    "Dt.Atual.                    "'10'
       27  'ZFIT0080'   'HR_ATUAL'   'T_SAIDA' 'HR_ATUAL'    'Hr.Atual'                ''  ''    '' ' ' ' ' ' ' ' '.    "Hr.Atual                     "'10'

    WHEN '0103'.

      PERFORM estrutura_alv USING:

       1  'SKB1'   'SAKNR'   'T_SAIDA_0103' 'SAKNR'       'Conta'       '10'  ''    '' ' ' ' ' ' ' ' ',
       2  'SKAT'   'TXT50'   'T_SAIDA_0103' 'TXT50'       'Descrição'   '30'  ''    '' ' ' ' ' ' ' ' ',
       3  'BSAK'   'DMBTR'   'T_SAIDA_0103' 'DMBTR'       'Saldo R$'    '13'  ''    '' ' ' ' ' ' ' ' ',
       4  'BSAK'   'DMBE2'   'T_SAIDA_0103' 'DMBE2'       'Saldo U$'    '13'  ''    '' ' ' ' ' ' ' ' '.


  ENDCASE.

ENDFORM.



INCLUDE zfir0055_pbo.

INCLUDE zfir0055_pai.

INCLUDE zfir0055_class.

INCLUDE zfir0055_form.
