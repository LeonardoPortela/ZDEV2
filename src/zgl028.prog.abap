*&--------------------------------------------------------------------&*
*&                        FI                                         &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 29/06/2015                                              &*
*& Descrição: Relatório – Report de Saída x entrada  - Intercompany   &*
*& Transação: ZGL                                                     &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor       |Request   |Data      |Descrição                       &*
*&--------------------------------------------------------------------&*
*& ABAP        |          |          |                                &*
*&--------------------------------------------------------------------&*
*& NSEGATIN    |          |28/08/2025|Melhoria Performance e Dados.   &*
*&                                   |Chamado: 158522.                &*
*&--------------------------------------------------------------------&*

REPORT  zgl028.

TYPE-POOLS vrm.
*=============================================================================*
*TABELAS                                                                      *
*=============================================================================*

TABLES: j_1bnfdoc,sscrfields.
INCLUDE <icon>.
*=============================================================================*
*ESTRUTURA                                                                    *
*=============================================================================*

TYPES:

  BEGIN OF ty_saida,
    parid_e    TYPE j_1bnfdoc-parid,           "COD_FORN
    name1_e    TYPE lfa1-name1,                "NOME_FORN
    nfenum_e   TYPE j_1bnfdoc-nfenum,          "NR_NF_ENTRADA
    series_e   TYPE j_1bnfdoc-series,          "SERIE_NF_ENTR.
    docdat_e   TYPE j_1bnfdoc-docdat,          "DT_DOC_ENTR.
    pstdat_e   TYPE j_1bnfdoc-pstdat,          "DT_LCTO_ENTR
    belnr_e    TYPE bkpf-belnr,                "DOC-CONT_ENTR.
    waers_e    TYPE bkpf-waers,                "MOEDA_DOC_ENTR
    dmbtr_e    TYPE bseg-dmbtr,                "BRL_ENTRADA
    dmbe2_e    TYPE bseg-dmbe2,                "USD_ENTRADA
    vbund_e    TYPE bseg-vbund,                "SOC_PARCEIRA_ENTR
    hkont_e    TYPE bseg-hkont,                "CTA_CONTABIL_ENTR
    ctaid_e(3),                                "CTA_CONTABIL_ENTRADA RESUMNIDO
    docnum_e   TYPE j_1bnfdoc-docnum,
    branch_e   TYPE j_1bnfdoc-branch,
    refkey_e   TYPE j_1bnflin-refkey,
    bukrs_e    TYPE t001k-bukrs,
    direct_e   TYPE j_1bnfdoc-direct,
    direct_s   TYPE j_1bnfdoc-direct,
    parid_s    TYPE j_1bnfdoc-parid,           "COD_CLIENTE
    name1_s    TYPE kna1-name1,                "NOME_CLIENTE
    nfenum_s   TYPE j_1bnfdoc-nfenum,          "NR_NF_ENTRADA
    series_s   TYPE j_1bnfdoc-series,          "SERIE_NF_ENTRADA
    docdat_s   TYPE j_1bnfdoc-docdat,          "DT_DOC_ENTRADA
    pstdat_s   TYPE j_1bnfdoc-pstdat,          "DT_LCTO_ENTRADA
    belnr_s    TYPE bkpf-belnr,                "DOC-CONTABIL_ENTRADA
    waers_s    TYPE bkpf-waers,                "MOEDA_DOC_ENTRADA
    dmbtr_s    TYPE bseg-dmbtr,                "BRL_ENTRADA
    dmbe2_s    TYPE bseg-dmbe2,                "USD_ENTRADA
**<<<------"158522 - NMS - INI------>>>
    dmbtr_d    TYPE bseg-dmbtr,                "BRL_DIFERENCA
    dmbe2_d    TYPE bseg-dmbe2,                "USD_DIFERENCA
**<<<------"158522 - NMS - FIM------>>>
    vbund_s    TYPE bseg-vbund,                "SOC_PARCEIRA_ENTRADA
    hkont_s    TYPE bseg-hkont,                "CTA_CONTABIL_ENTRADA
    ctaid_s(3),                                "CTA_CONTABIL_ENTRADA RESUMNIDO
    kunnr_s    TYPE kna1-kunnr,
    stcd1_s    TYPE kna1-stcd1,
    bukrs_s    TYPE t001k-bukrs,
    docnum_s   TYPE j_1bnfdoc-docnum,
    branch_s   TYPE j_1bnfdoc-branch,
    refkey_s   TYPE j_1bnflin-refkey,
    bwkey_s    TYPE t001k-bwkey,
    house_num2 TYPE j_1bnfdoc-parid,
    traty      TYPE j_1bnfdoc-branch,
    status(4),


*BEGIN OF TY_BKPF,
    bukrs      TYPE bkpf-bukrs,
    gjahr      TYPE bkpf-gjahr,
    belnr      TYPE bkpf-belnr,
    waers      TYPE bkpf-waers,
    awkey      TYPE bkpf-awkey,
    awkey_aux  TYPE j_1bnflin-refkey,
  END OF ty_saida.
FIELD-SYMBOLS <fs_saida> TYPE ty_saida.

*  END OF TY_BKPF.
*FIELD-SYMBOLS <FS_BKPF>  TYPE TY_BKPF.

TYPES:BEGIN OF ty_j_1bnflin.
        INCLUDE STRUCTURE j_1bnflin.
TYPES:  bukrs TYPE bukrs,
        gjahr TYPE gjahr,
        awkey TYPE awkey.
TYPES: END OF ty_j_1bnflin.

FIELD-SYMBOLS <fs_j_1bnflin>  TYPE ty_j_1bnflin.


TYPES:BEGIN OF ty_j_1bnfdoc.
        INCLUDE STRUCTURE j_1bnfdoc.
TYPES: END OF ty_j_1bnfdoc.
FIELD-SYMBOLS <fs_j_1bnfdoc> TYPE ty_j_1bnfdoc.

TYPES:BEGIN OF ty_j_1bnfdoc_aux.
        INCLUDE STRUCTURE j_1bnfdoc.
TYPES: END OF ty_j_1bnfdoc_aux.
FIELD-SYMBOLS <fs_j_1bnfdoc_aux> TYPE ty_j_1bnfdoc_aux.

TYPES: BEGIN OF ty_saida_aux,
         bukrs_e TYPE ty_saida-bukrs_e,
         belnr_e TYPE ty_saida-belnr_e,
         gjahr   TYPE ty_saida-gjahr,
         bukrs_s TYPE ty_saida-bukrs_s,
         belnr_s TYPE ty_saida-belnr_s,
         gjahs   TYPE ty_saida-gjahr.
TYPES: END OF ty_saida_aux.
*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*

DATA: it_kna1            TYPE TABLE OF kna1,
      it_t001k           TYPE TABLE OF t001k,
      it_bkpf            TYPE TABLE OF bkpf,
      it_bkpf_aux        TYPE TABLE OF bkpf,
      "IT_BSEG             TYPE TABLE OF BSEG,
      it_partidas        TYPE TABLE OF zde_fi_gl_partidas_cli_for,
      it_lfa1            TYPE TABLE OF lfa1,
*      it_lfa1_zfic       TYPE TABLE OF lfa1,        "<<<------"158522 - NMS ------->>>
      it_j_1bnfdoc       TYPE TABLE OF j_1bnfdoc,
      it_j_1bnfdoc_s     TYPE TABLE OF j_1bnfdoc,
*      it_j_1bnfdoc_e     TYPE TABLE OF j_1bnfdoc,   "<<<------"158522 - NMS ------->>>
      it_j_1bnflin       TYPE TABLE OF ty_j_1bnflin,
      it_j_1bnflin_aux   TYPE TABLE OF ty_j_1bnflin,
      it_j_1bnfdoc_aux   TYPE TABLE OF j_1bnfdoc,
      it_j_1bnfdoc_aux_s TYPE TABLE OF j_1bnfdoc,

      it_aux             TYPE TABLE OF j_1bnfdoc,

      it_saida           TYPE TABLE OF ty_saida,
      wa_lin             TYPE ty_j_1bnflin,
      wa_lin_aux         TYPE ty_j_1bnflin,
      lc_sy_tabix        TYPE sy-tabix.
*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*

DATA: wa_kna1            TYPE  kna1,
      wa_t001k           TYPE  t001k,
      wa_bkpf            TYPE  bkpf,
      wa_bkpf_aux        TYPE  bkpf,
      wa_partidas        TYPE  zde_fi_gl_partidas_cli_for,
      wa_lfa1            TYPE  lfa1,
      wa_j_1bnfdoc       TYPE  j_1bnfdoc,
      wa_j_1bnfdoc_e     TYPE  j_1bnfdoc,
      wa_j_1bnflin       TYPE  j_1bnflin,
      wa_j_1bnflin_aux   TYPE  j_1bnflin,
      wa_j_1bnfdoc_aux   TYPE  j_1bnfdoc,
      wa_j_1bnfdoc_aux_s TYPE  j_1bnfdoc,
      wa_aux             TYPE  j_1bnfdoc,
      wa_saida_aux       TYPE  ty_saida,
      wa_saida           TYPE  ty_saida.

*=============================================================================*
*Estrutura Alv                                                                *
*=============================================================================*
DATA:it_fcat    TYPE TABLE OF lvc_s_fcat,
     it_list    TYPE vrm_values,
     list_value TYPE vrm_values,
     wa_layout  TYPE lvc_s_layo,
     wa_cont    TYPE REF TO cl_gui_custom_container,
     wa_alv     TYPE REF TO  cl_gui_alv_grid,
     tabix      TYPE sy-tabix.
**<<<------"158522 - NMS - INI------>>>
DATA: vg_task_ativa TYPE n LENGTH 4,
      vg_tasks      TYPE i,
      vg_max_tasks  TYPE i.
**<<<------"158522 - NMS - FIM------>>>
*----------------------------------------------------------------------*
***INCLUDE Zmmr101_0001 .
*----------------------------------------------------------------------*

DATA: dg_dyndoc_id     TYPE REF TO cl_dd_document.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_graphic_xstr TYPE xstring.
DATA: graphic_size   TYPE i.
DATA: l_graphic_conv TYPE i.
DATA: l_graphic_offs TYPE i.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no.
*    METHODS TOP_OF_PAGE
*      FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
*      IMPORTING E_DYNDOC_ID.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Inclementação  -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    READ TABLE it_saida INTO wa_saida INDEX e_row_id.
    IF sy-subrc IS NOT INITIAL.
    ENDIF.
    CASE e_column_id-fieldname.
      WHEN 'DOCNUM_S'.
        SET PARAMETER ID 'JEF' FIELD wa_saida-docnum_s.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_E'.
        SET PARAMETER ID 'JEF' FIELD wa_saida-docnum_e.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click
*  METHOD TOP_OF_PAGE.
*    PERFORM EVENT_TOP_OF_PAGE USING DG_DYNDOC_ID.
*  ENDMETHOD.
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*=============================================================================*
*Estrutura cabeçalho Alv                                                      *
*=============================================================================*
DATA: picture          TYPE REF TO cl_gui_picture,
      gf_first_display TYPE c VALUE 'X',
      ctl_cccontainer  TYPE REF TO cl_gui_custom_container,
      dg_splitter      TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2    TYPE REF TO cl_gui_splitter_container,
      dg_parent_html   TYPE REF TO cl_gui_container,
      dg_parent_html1  TYPE REF TO cl_gui_container,
      dg_parent_html2  TYPE REF TO cl_gui_container,
      dg_parent_grid   TYPE REF TO cl_gui_container,
      event_handler    TYPE REF TO lcl_event_handler,
      dg_html_cntrl    TYPE REF TO cl_gui_html_viewer,
      ctl_alv_resumo   TYPE REF TO cl_gui_alv_grid,
      gs_scroll_col    TYPE lvc_s_col,
      gs_scroll_row    TYPE lvc_s_roid,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      it_exclude_fcode TYPE ui_functions.
*=============================================================================*
*Variaveis                                                                *
*=============================================================================*
DATA: v_sctarec(3) TYPE c.
*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                  s_bukrs  FOR j_1bnfdoc-bukrs   OBLIGATORY NO INTERVALS ,
                  s_branch FOR j_1bnfdoc-branch  OBLIGATORY,
                  s_parid  FOR j_1bnfdoc-parid   OBLIGATORY,
                  s_pstdat FOR j_1bnfdoc-pstdat  OBLIGATORY,
                  s_docdat FOR j_1bnfdoc-docdat.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN SKIP.
  PARAMETERS p_chkbox AS CHECKBOX USER-COMMAND chkbox DEFAULT ''.

SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN.

  CLEAR: v_sctarec.

  IF p_chkbox = 'X'.
    v_sctarec = '112'. "Somente Conta a Receber
  ELSE.
    v_sctarec = abap_false.
  ENDIF.


AT USER-COMMAND.


*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.


  PERFORM: f_seleciona_dados,
           f_organiza_dados,
           f_alv.



END-OF-SELECTION.

  CALL SCREEN 0100.

*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
FORM f_seleciona_dados.
*
  DATA:  var_parid  TYPE j_1bnfdoc-branch.

  DATA: rl_docnum TYPE ty_so_docnum,    "<<<------"158522 - NMS ------->>>
        el_docnum TYPE str_so_docnum.   "<<<------"158522 - NMS ------->>>

  RANGES: pdirect FOR j_1bnfdoc-direct.

  pdirect-sign   = 'I'.
  pdirect-option = 'EQ'.
  pdirect-low    = '2'.
  pdirect-high   = '2'.
  APPEND pdirect.

  pdirect-sign   = 'I'.
  pdirect-option = 'EQ'.
  pdirect-low    = '4'.
  pdirect-high   = '4'.
  APPEND pdirect.

  SELECT *
    FROM kna1
    INTO TABLE it_kna1
    WHERE ktokd = 'ZCIC'.
**<<<------"158522 - NMS - INI------>>>
*  SELECT *
*    FROM lfa1
*    INTO TABLE it_lfa1_zfic
*   WHERE ktokk EQ 'ZFIC'.
**<<<------"158522 - NMS - FIM------>>>
  SELECT *
     FROM j_1bnfdoc AS j
     INTO TABLE it_j_1bnfdoc_s
     WHERE bukrs  IN s_bukrs
       AND branch IN s_branch
       AND parid  IN s_parid
       AND pstdat IN s_pstdat
       AND docdat IN s_docdat
       AND direct IN pdirect
       AND EXISTS ( SELECT * FROM kna1 AS k WHERE k~kunnr EQ j~parid AND k~ktokd EQ 'ZCIC' )
       AND partyp NE 'B'
       AND cancel NE 'X'
       AND nftype NE 'ZO'
       AND doctyp NE '5'.

  SELECT *
   FROM j_1bnfdoc AS j
   INTO TABLE it_j_1bnfdoc_s
   WHERE bukrs  IN s_bukrs
     AND branch IN s_branch
     AND parid  IN s_parid
     AND pstdat IN s_pstdat
     AND docdat IN s_docdat
     AND direct IN pdirect
     AND EXISTS ( SELECT * FROM lfa1 AS k WHERE k~lifnr EQ j~parid AND k~ktokk EQ 'ZFIC' )
     AND partyp NE 'B'
     AND cancel NE 'X'
     AND nftype NE 'ZO'
     AND doctyp NE '5'.

  IF it_j_1bnfdoc_s IS NOT INITIAL.
    LOOP AT it_j_1bnfdoc_s ASSIGNING <fs_j_1bnfdoc>.

      IF <fs_j_1bnfdoc>-nfenum IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_j_1bnfdoc>-nfnum
          IMPORTING
            output = <fs_j_1bnfdoc>-nfenum.
      ENDIF.

      IF <fs_j_1bnfdoc>-nfnum IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_j_1bnfdoc>-nfenum
          IMPORTING
            output = <fs_j_1bnfdoc>-nfnum.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_j_1bnfdoc>-nfnum
          IMPORTING
            output = <fs_j_1bnfdoc>-nfnum.
      ENDIF.

      IF <fs_j_1bnfdoc>-branch IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_j_1bnfdoc>-branch
          IMPORTING
            output = <fs_j_1bnfdoc>-house_num2.
      ENDIF.

      IF <fs_j_1bnfdoc>-parid IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_j_1bnfdoc>-parid
          IMPORTING
            output = <fs_j_1bnfdoc>-traty.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_j_1bnfdoc>-traty
          IMPORTING
            output = <fs_j_1bnfdoc>-traty.

      ENDIF.


    ENDLOOP.
    MOVE: it_j_1bnfdoc_s[] TO it_j_1bnfdoc_aux_s[].

    " Notas de entrada
    CLEAR: pdirect[].

    pdirect-sign   = 'I'.
    pdirect-option = 'EQ'.
    pdirect-low    = '1'.
    pdirect-high   = '1'.
    APPEND pdirect.

    pdirect-sign   = 'I'.
    pdirect-option = 'EQ'.
    pdirect-low    = '3'.
    pdirect-high   = '3'.
    APPEND pdirect.
    IF sy-subrc IS INITIAL.
      "Entradas Compra - cLIENTE
      SELECT *
        FROM j_1bnfdoc AS j
        INTO TABLE it_j_1bnfdoc
         FOR ALL ENTRIES IN it_j_1bnfdoc_s
       WHERE pstdat GE s_pstdat-low
         AND parid  EQ it_j_1bnfdoc_s-house_num2
         AND branch EQ it_j_1bnfdoc_s-traty
         AND direct IN pdirect
         AND nfnum  EQ it_j_1bnfdoc_s-nfnum
         AND partyp NE 'B'
         AND cancel NE 'X'
         AND EXISTS ( SELECT * FROM kna1 AS k WHERE k~kunnr EQ j~parid AND k~ktokd EQ 'ZCIC' ).


      "Entradas Devolução de Venda _CLIENTE
      SELECT *
        FROM j_1bnfdoc AS j
        APPENDING TABLE it_j_1bnfdoc
         FOR ALL ENTRIES IN it_j_1bnfdoc_s
      WHERE pstdat GE s_pstdat-low
        AND parid  EQ it_j_1bnfdoc_s-house_num2
        AND branch EQ it_j_1bnfdoc_s-traty
         AND direct IN pdirect
         AND nfenum EQ it_j_1bnfdoc_s-nfenum
         AND partyp NE 'B'
         AND cancel NE 'X'
         AND EXISTS ( SELECT * FROM kna1 AS k WHERE k~kunnr EQ j~parid AND k~ktokd EQ 'ZCIC' ).

      "Entradas Compra - cLIENTE
      SELECT *
        FROM j_1bnfdoc AS j
        APPENDING TABLE it_j_1bnfdoc
         FOR ALL ENTRIES IN it_j_1bnfdoc_s
      WHERE pstdat GE s_pstdat-low
        AND parid  EQ it_j_1bnfdoc_s-house_num2
        AND branch EQ it_j_1bnfdoc_s-traty
         AND direct IN pdirect
         AND nfnum  EQ it_j_1bnfdoc_s-nfnum
         AND partyp NE 'B'
         AND cancel NE 'X'
         AND EXISTS ( SELECT * FROM kna1 AS k WHERE k~kunnr EQ j~parid AND k~ktokd EQ 'ZCIC' ).

      "Entradas Devolução de Venda _CLIENTE
      SELECT *
        FROM j_1bnfdoc AS j
        APPENDING TABLE it_j_1bnfdoc
         FOR ALL ENTRIES IN it_j_1bnfdoc_s
      WHERE pstdat GE s_pstdat-low
        AND parid  EQ it_j_1bnfdoc_s-house_num2
        AND branch EQ it_j_1bnfdoc_s-traty
         AND direct IN pdirect
         AND nfenum EQ it_j_1bnfdoc_s-nfenum
         AND partyp NE 'B'
         AND cancel NE 'X'
*         AND DOCREF EQ '0000000000'
         AND EXISTS ( SELECT * FROM kna1 AS k WHERE k~kunnr EQ j~parid AND k~ktokd EQ 'ZCIC' ).

*      IF J_1BNFDOC-CANCEL NE 'x'.
*        DELETE IT_J_1BNFDOC WHERE DOCREF NE '0000000000'.
*      ENDIF.
      "busca referencia cancelada
**<<<------"158522 - NMS - INI------>>>
*      SELECT *
*        FROM j_1bnfdoc AS j
*        INTO TABLE it_j_1bnfdoc_e
*         FOR ALL ENTRIES IN it_j_1bnfdoc
*        WHERE docnum = it_j_1bnfdoc-docref
*        AND   docnum > 0
*        AND   cancel = 'X'.
*
*      SORT  it_j_1bnfdoc_e BY docnum.
*
*      LOOP AT it_j_1bnfdoc INTO wa_j_1bnfdoc.
*        tabix = sy-tabix.
*        READ TABLE it_j_1bnfdoc_e INTO wa_j_1bnfdoc_e WITH KEY docnum = wa_j_1bnfdoc-docref BINARY SEARCH.
*        IF sy-subrc = 0.
*          wa_j_1bnfdoc-manual = 'D'.
*          MODIFY it_j_1bnfdoc FROM wa_j_1bnfdoc INDEX tabix TRANSPORTING manual.
*        ENDIF.
*
*      ENDLOOP.
*      DELETE it_j_1bnfdoc WHERE manual = 'D'.
      SELECT mandt mandt docnum
        FROM j_1bnfdoc
        INTO TABLE rl_docnum
         FOR ALL ENTRIES IN it_j_1bnfdoc
        WHERE docnum EQ it_j_1bnfdoc-docref
          AND docnum GT 0
          AND cancel EQ abap_on.

      IF sy-subrc IS INITIAL.
        el_docnum = 'IEQ'.
        MODIFY rl_docnum FROM el_docnum TRANSPORTING sign option WHERE sign   NE el_docnum(1)
                                                                   AND option NE el_docnum+1(2).
        SORT rl_docnum BY low.

        DELETE it_j_1bnfdoc WHERE docref IN rl_docnum.

      ENDIF.
**<<<------"158522 - NMS - FIM------>>>
      SORT it_j_1bnfdoc BY docnum.
      DELETE ADJACENT DUPLICATES FROM it_j_1bnfdoc COMPARING docnum.

      MOVE: it_j_1bnfdoc[] TO it_j_1bnfdoc_aux[].
      DELETE it_j_1bnfdoc_aux WHERE direct NE '1' AND  direct NE '3'.
*  MOVE: IT_J_1BNFDOC_S[] TO IT_J_1BNFDOC[].
**<<<------"158522 - NMS - INI------>>>
*      LOOP AT it_j_1bnfdoc_s INTO wa_j_1bnfdoc.
*        APPEND wa_j_1bnfdoc TO it_j_1bnfdoc.
*      ENDLOOP.
      APPEND LINES OF it_j_1bnfdoc_s TO it_j_1bnfdoc.
      SORT it_j_1bnfdoc BY docnum.
**<<<------"158522 - NMS - FIM------>>>
      CHECK it_j_1bnfdoc IS NOT INITIAL.

      SELECT *
        FROM j_1bnflin
        INTO TABLE it_j_1bnflin
        FOR ALL ENTRIES IN it_j_1bnfdoc
        WHERE docnum EQ it_j_1bnfdoc-docnum.

      IF it_j_1bnflin IS NOT INITIAL.
**<<<------"158522 - NMS - INI------>>>
* Busca os Documentos Contábeis de Gestão Emissão Nota Fiscal
        DATA(tl_nflin_zw) = it_j_1bnflin.
        DELETE tl_nflin_zw WHERE reftyp NE 'ZW'. "Gestão Emissão Nota Fiscal

        SELECT seq_lcto, docnum, belnr_imb
          FROM zfiwrt0008
          INTO TABLE @DATA(tl_wrt0008)
          FOR ALL ENTRIES IN @tl_nflin_zw
        WHERE seq_lcto EQ @tl_nflin_zw-refkey(10).

        IF sy-subrc IS INITIAL.
          SORT tl_wrt0008 BY seq_lcto.

        ENDIF.
**<<<------"158522 - NMS - FIM------>>>
        LOOP AT it_j_1bnflin INTO wa_lin.
          lc_sy_tabix = sy-tabix.
          READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_lin-docnum BINARY SEARCH.   "<<<------"158522 - NMS ------->>>
          IF wa_j_1bnfdoc-direct EQ '2' OR wa_j_1bnfdoc-direct EQ '4'.

            CASE wa_lin-reftyp.
              WHEN 'ZW'.
                wa_lin-bukrs = wa_j_1bnfdoc-bukrs.
                wa_lin-gjahr = wa_j_1bnfdoc-pstdat(4).
**<<<------"158522 - NMS - INI------>>>
*                wa_lin-awkey = wa_lin-refkey(10).
*                CONCATENATE 'ZG0' wa_lin-awkey wa_lin-gjahr INTO wa_lin-awkey.
                READ TABLE tl_wrt0008 INTO DATA(el_wrt0008) WITH KEY seq_lcto = wa_lin-refkey(10) BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  wa_lin-awkey = el_wrt0008-belnr_imb.

                ENDIF.

                CLEAR el_wrt0008.
                CONCATENATE wa_lin-awkey wa_lin-bukrs wa_lin-gjahr INTO wa_lin-awkey.
**<<<------"158522 - NMS - FIM------>>>
              WHEN 'BI'.
                wa_lin-bukrs = wa_j_1bnfdoc-bukrs.
                wa_lin-gjahr = wa_j_1bnfdoc-pstdat(4).
                wa_lin-awkey = wa_lin-refkey(10).
                MODIFY it_j_1bnflin INDEX lc_sy_tabix FROM wa_lin TRANSPORTING awkey.

              WHEN 'LI'.
                wa_lin-bukrs = wa_j_1bnfdoc-bukrs.
                wa_lin-gjahr = wa_j_1bnfdoc-pstdat(4).
                wa_lin-awkey = wa_lin-refkey.
              WHEN OTHERS.
*                MODIFY IT_J_1BNFLIN INDEX LC_SY_TABIX FROM WA_LIN TRANSPORTING BUKRS GJAHR AWKEY.
            ENDCASE.

          ELSE.
            READ TABLE it_j_1bnfdoc_aux INTO wa_j_1bnfdoc_aux WITH KEY docnum = wa_lin-docnum BINARY SEARCH.   "<<<------"158522 - NMS ------->>>
            CASE wa_lin-reftyp.
              WHEN 'ZW'.
                wa_lin-bukrs = wa_j_1bnfdoc-bukrs.
                wa_lin-gjahr = wa_j_1bnfdoc-pstdat(4).
**<<<------"158522 - NMS - INI------>>>
*                wa_lin-awkey = wa_lin-refkey(10).
*                CONCATENATE 'ZG0' wa_lin-awkey wa_lin-gjahr INTO wa_lin-awkey.
                CLEAR el_wrt0008.
                READ TABLE tl_wrt0008 INTO el_wrt0008 WITH KEY seq_lcto = wa_lin-refkey(10) BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  wa_lin-awkey = el_wrt0008-belnr_imb.

                ENDIF.

                CONCATENATE wa_lin-awkey wa_lin-bukrs wa_lin-gjahr INTO wa_lin-awkey.
**<<<------"158522 - NMS - FIM------>>>
*
              WHEN 'BI'.
                wa_lin-bukrs = wa_j_1bnfdoc-bukrs.
                wa_lin-gjahr = wa_j_1bnfdoc-pstdat(4).
                wa_lin-awkey = wa_lin-refkey(10).
*                MODIFY IT_J_1BNFLIN INDEX LC_SY_TABIX FROM WA_LIN TRANSPORTING AWKEY.

              WHEN 'LI'.
                wa_lin-bukrs = wa_j_1bnfdoc-bukrs.
                wa_lin-gjahr = wa_j_1bnfdoc-pstdat(4).
                wa_lin-awkey = wa_lin-refkey(10).
                CONCATENATE wa_lin-awkey wa_lin-gjahr INTO wa_lin-awkey.
*                MODIFY IT_J_1BNFLIN INDEX LC_SY_TABIX FROM WA_LIN TRANSPORTING BUKRS GJAHR AWKEY.
              WHEN OTHERS.
*                MODIFY IT_J_1BNFLIN INDEX LC_SY_TABIX FROM WA_LIN TRANSPORTING BUKRS GJAHR AWKEY.
            ENDCASE.
          ENDIF.
*          READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC WITH KEY DOCNUM = WA_LIN-DOCNUM.
*          IF WA_J_1BNFDOC-DIRECT EQ '2' OR WA_J_1BNFDOC-DIRECT EQ '4'.
*            WA_LIN-BUKRS = WA_J_1BNFDOC-BUKRS.
*            WA_LIN-GJAHR = WA_J_1BNFDOC-PSTDAT(4).
*            WA_LIN-AWKEY = WA_LIN-REFKEY.
*            MODIFY IT_J_1BNFLIN INDEX LC_SY_TABIX FROM WA_LIN TRANSPORTING BUKRS GJAHR AWKEY.
*          ELSE.
*            READ TABLE IT_J_1BNFDOC_AUX INTO WA_J_1BNFDOC_AUX WITH KEY DOCNUM = WA_LIN-DOCNUM.
*            IF WA_LIN-REFTYP EQ 'BI'.
*              WA_LIN-BUKRS = WA_J_1BNFDOC_AUX-BUKRS.
*              WA_LIN-GJAHR = WA_J_1BNFDOC_AUX-PSTDAT(4).
*              WA_LIN-AWKEY = WA_LIN-REFKEY(10).
*            ELSE.
*              WA_LIN-BUKRS = WA_J_1BNFDOC_AUX-BUKRS.
*              WA_LIN-GJAHR = WA_J_1BNFDOC_AUX-PSTDAT(4).
*              WA_LIN-AWKEY = WA_LIN-REFKEY(10).
*              CONCATENATE WA_LIN-AWKEY WA_LIN-GJAHR INTO WA_LIN-AWKEY.
*              MODIFY IT_J_1BNFLIN INDEX LC_SY_TABIX FROM WA_LIN TRANSPORTING BUKRS GJAHR AWKEY.
*            ENDIF.
*          ENDIF.
*
*          IF WA_LIN-REFTYP EQ 'ZW'.
*            IF WA_LIN-AWKEY EQ WA_LIN-REFKEY(10).
*              CONCATENATE 'ZG0' WA_LIN-AWKEY WA_LIN-GJAHR INTO WA_LIN-AWKEY.
*              MODIFY IT_J_1BNFLIN INDEX LC_SY_TABIX FROM WA_LIN TRANSPORTING AWKEY.
*            ELSE.
*              CONCATENATE 'ZG0' WA_LIN-AWKEY INTO WA_LIN-AWKEY.
*              MODIFY IT_J_1BNFLIN INDEX LC_SY_TABIX FROM WA_LIN TRANSPORTING AWKEY.
*            ENDIF.
*          ENDIF.
          MODIFY it_j_1bnflin INDEX lc_sy_tabix FROM wa_lin TRANSPORTING bukrs gjahr awkey.
        ENDLOOP.

        SELECT *
         FROM bkpf
         INTO TABLE it_bkpf
         FOR ALL ENTRIES IN it_j_1bnflin
         WHERE bukrs EQ it_j_1bnflin-bukrs
           AND gjahr EQ it_j_1bnflin-gjahr
           AND awkey EQ it_j_1bnflin-awkey
           AND blart NE 'ML'.
      ENDIF.

      SELECT *
        FROM lfa1
        INTO TABLE it_lfa1
        FOR ALL ENTRIES IN it_j_1bnfdoc
        WHERE lifnr	=	it_j_1bnfdoc-parid.

      CLEAR it_j_1bnfdoc.
      MOVE: it_j_1bnfdoc_aux_s[] TO it_j_1bnfdoc[].
**<<<------"158522 - NMS - INI------>>>
*      LOOP AT it_j_1bnfdoc ASSIGNING <fs_j_1bnfdoc>.
*        IF <fs_j_1bnfdoc>-nfenum IS INITIAL.
      LOOP AT it_j_1bnfdoc ASSIGNING <fs_j_1bnfdoc> WHERE nfenum IS INITIAL.
**<<<------"158522 - NMS - FIM------>>>
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_j_1bnfdoc>-nfnum
          IMPORTING
            output = <fs_j_1bnfdoc>-nfenum.
*        ENDIF.    "<<<------"158522 - NMS ------->>>
      ENDLOOP.


      SELECT *
       FROM t001k
       INTO TABLE it_t001k.

    ENDIF.

  ELSE.
    MESSAGE 'Erro documento de saida não encontrado' TYPE 'E'.
  ENDIF.
ENDFORM.                    "F_SELECIONA_DADOS
*=============================================================================*
*Form F_ORGANIZA_DADOS                                                        *
*=============================================================================*
FORM: f_organiza_dados.
  DATA: var_psdt TYPE bkpf-gjahr.
  DATA: vg_parid TYPE j_1bnfdoc-branch,
        vl_awkey TYPE j_1bnflin-refkey.

**<<<------"158522 - NMS - INI------>>>
  DATA: vl_dest     TYPE char10 VALUE 'NONE',
        vl_taskname TYPE char12.

  SORT: it_j_1bnfdoc     BY docnum,
        it_kna1          BY kunnr,
        it_t001k         BY bwkey,
        it_j_1bnflin     BY docnum itmnum,
        it_bkpf          BY bukrs gjahr awkey,
        it_j_1bnfdoc_aux BY nfenum branch parid,
        it_lfa1          BY lifnr.

  CLEAR: vg_task_ativa, vg_tasks, vg_max_tasks.
* Obter o número de sessões disponíveis e a máxima.
  CALL FUNCTION 'SPBT_INITIALIZE'
    IMPORTING
      free_pbt_wps                   = vg_tasks
      max_pbt_wps                    = vg_max_tasks
    EXCEPTIONS
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      OTHERS                         = 7.
**<<<------"158522 - NMS - FIM------>>>
  LOOP AT it_j_1bnfdoc INTO wa_j_1bnfdoc.
    CLEAR: wa_saida.
    wa_saida-parid_s   = wa_j_1bnfdoc-parid.
    wa_saida-nfenum_s  = wa_j_1bnfdoc-nfenum.
    wa_saida-series_s  = wa_j_1bnfdoc-series.
    wa_saida-docnum_s  = wa_j_1bnfdoc-docnum.
    wa_saida-docdat_s  = wa_j_1bnfdoc-docdat.
    wa_saida-pstdat_s  = wa_j_1bnfdoc-pstdat.
    wa_saida-branch_s  = wa_j_1bnfdoc-branch.
    wa_saida-direct_s  = wa_j_1bnfdoc-direct.
    wa_saida-bukrs_s   = wa_j_1bnfdoc-bukrs.
    var_psdt  = wa_j_1bnfdoc-pstdat(4).

    "SAIDA
    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_j_1bnfdoc-parid BINARY SEARCH.    "<<<------"158522 - NMS ------->>>
    IF sy-subrc IS INITIAL.
      wa_saida-name1_s = wa_kna1-name1.
      wa_saida-kunnr_s = wa_kna1-kunnr.
      wa_saida-stcd1_s = wa_kna1-stcd1.
    ENDIF.

*    SHIFT WA_SAIDA-KUNNR_S LEFT DELETING LEADING '0'.

    READ TABLE it_t001k INTO wa_t001k WITH KEY bwkey = wa_saida-kunnr_s+6(4) BINARY SEARCH.    "<<<------"158522 - NMS ------->>>
    IF sy-subrc IS INITIAL.
      wa_saida-bwkey_s = wa_t001k-bwkey.
    ENDIF.
**<<<------"158522 - NMS - INI------>>>
*    READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY docnum  = wa_j_1bnfdoc-docnum BINARY SEARCH.
    CLEAR wa_lin.
    READ TABLE it_j_1bnflin INTO wa_lin WITH KEY docnum	=	wa_j_1bnfdoc-docnum BINARY SEARCH.
**<<<------"158522 - NMS - FIM------>>>
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING wa_lin TO wa_j_1bnflin.    "<<<------"158522 - NMS ------->>>
      IF wa_j_1bnflin-refkey EQ wa_j_1bnflin-refkey(10).
        wa_saida-refkey_s = wa_j_1bnflin-refkey.
      ELSE.
        wa_saida-refkey_s = wa_j_1bnflin-refkey.
      ENDIF.

      IF wa_j_1bnflin-reftyp NE 'ZW'.
        wa_saida-refkey_s = wa_j_1bnflin-refkey.
      ELSE.
**<<<------"158522 - NMS - INI------>>>
*        IF wa_j_1bnflin-refkey EQ wa_j_1bnflin-refkey(10).
*          wa_saida-refkey_s = wa_j_1bnflin-refkey.
*          CONCATENATE 'ZG0' wa_saida-refkey_s var_psdt INTO wa_saida-refkey_s.
*        ELSE.
*          CONCATENATE 'ZG0' wa_saida-refkey_s  INTO wa_saida-refkey_s.
*        ENDIF.
        wa_saida-refkey_s = wa_lin-awkey.
**<<<------"158522 - NMS - FIM------>>>
      ENDIF.

      READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_j_1bnfdoc-bukrs
                                               gjahr = var_psdt
                                               awkey = wa_saida-refkey_s BINARY SEARCH.    "<<<------"158522 - NMS ------->.
      IF sy-subrc IS INITIAL.
        wa_bkpf_aux-belnr = wa_bkpf-belnr.
        wa_bkpf_aux-waers = wa_bkpf-waers.
        wa_bkpf_aux-gjahr = wa_bkpf-gjahr.
        wa_bkpf_aux-bukrs = wa_bkpf-bukrs.

        APPEND wa_bkpf_aux TO it_bkpf_aux.

        wa_saida-belnr_s = wa_bkpf-belnr.
        wa_saida-waers_s = wa_bkpf-waers.
        wa_saida-gjahr  = wa_bkpf-gjahr.
      ENDIF.
    ENDIF.

    CLEAR: wa_j_1bnflin-refkey, wa_partidas-hkont, wa_partidas-dmbtr, wa_partidas-dmbe2, wa_partidas-vbund, wa_bkpf-belnr,
    wa_bkpf-waers, wa_j_1bnfdoc-bukrs, wa_bkpf-bukrs.

*************************----------------------------------**********************
    "ENTRADA
    vg_parid = wa_saida-parid_s+6(4).
    " # US 102144 --

    DATA lv_branch_s TYPE j_1bparid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida-branch_s
      IMPORTING
        output = lv_branch_s.

    READ TABLE it_j_1bnfdoc_aux INTO wa_j_1bnfdoc_aux WITH KEY  nfenum = wa_saida-nfenum_s
*                                                                SERIES = WA_SAIDA-SERIES_S
                                                                branch = vg_parid
                                                                parid  = lv_branch_s BINARY SEARCH. "Buscar parid saida com BRANCh entrada    "<<<------"158522 - NMS ------->


    IF sy-subrc IS INITIAL.
      wa_saida-docnum_e = wa_j_1bnfdoc_aux-docnum.
      wa_saida-pstdat_e = wa_j_1bnfdoc_aux-pstdat.
      wa_saida-bukrs_e  = wa_j_1bnfdoc_aux-bukrs.
      wa_saida-nfenum_e = wa_j_1bnfdoc_aux-nfenum.
      wa_saida-series_e = wa_j_1bnfdoc_aux-series.
      wa_saida-docdat_e = wa_j_1bnfdoc_aux-docdat.
      wa_saida-parid_e  = wa_j_1bnfdoc_aux-parid.

      var_psdt = wa_j_1bnfdoc_aux-pstdat(4).
**<<<------"158522 - NMS - INI------>>>
*      READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY docnum = wa_j_1bnfdoc_aux-docnum BINARY SEARCH.
      CLEAR wa_lin.
      READ TABLE it_j_1bnflin INTO wa_lin WITH KEY docnum = wa_j_1bnfdoc_aux-docnum BINARY SEARCH.
**<<<------"158522 - NMS - FIM------>>>
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING wa_lin TO wa_j_1bnflin.    "<<<------"158522 - NMS ------->>>
        CASE wa_j_1bnflin-reftyp.
          WHEN 'ZW'.
**<<<------"158522 - NMS - INI------>>>
*            IF wa_j_1bnflin-refkey EQ wa_j_1bnflin-refkey(10).
*              wa_saida-refkey_e = wa_j_1bnflin-refkey.
*              CONCATENATE 'ZG0' wa_saida-refkey_e var_psdt INTO wa_saida-refkey_e.
*            ELSE.
*              CONCATENATE 'ZG0' wa_saida-refkey_e  INTO wa_saida-refkey_e.
*            ENDIF.
            wa_saida-refkey_s = wa_lin-awkey.
**<<<------"158522 - NMS - FIM------>>>
          WHEN 'BI'.
            wa_saida-refkey_e = wa_j_1bnflin-refkey(10).

          WHEN 'LI'.
            IF wa_j_1bnflin-refkey EQ wa_j_1bnflin-refkey(10).
              wa_saida-refkey_e = wa_j_1bnflin-refkey(10).
              CONCATENATE  wa_saida-refkey_e var_psdt INTO wa_saida-refkey_e.
            ELSE.
              wa_saida-refkey_e = wa_j_1bnflin-refkey.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

        READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_j_1bnfdoc_aux-bukrs
                                                 gjahr = var_psdt
                                                 awkey = wa_saida-refkey_e BINARY SEARCH.    "<<<------"158522 - NMS ------->
        IF sy-subrc IS INITIAL.
          wa_bkpf_aux-belnr  = wa_bkpf-belnr.
          wa_bkpf_aux-waers  = wa_bkpf-waers.
          wa_bkpf_aux-bukrs  = wa_saida-bukrs_e.

          APPEND wa_bkpf_aux TO it_bkpf_aux.

          wa_saida-belnr_e = wa_bkpf-belnr.
          wa_saida-waers_e = wa_bkpf-waers.
          wa_saida-gjahr  = wa_bkpf-gjahr.

        ENDIF.
      ENDIF.

      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_j_1bnfdoc_aux-parid BINARY SEARCH.    "<<<------"158522 - NMS ------->
      IF sy-subrc IS INITIAL.
        wa_saida-name1_e = wa_lfa1-name1.
      ENDIF.
    ENDIF.
    IF wa_saida-parid_s IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-parid_s
        IMPORTING
          output = wa_saida-parid_s.
    ENDIF.

    "Status
    IF wa_saida-docnum_e IS NOT INITIAL.
      wa_saida-status = icon_checked.

    ELSE.
      wa_saida-status = icon_incomplete.

    ENDIF.

    IF wa_saida-pstdat_s+4(2) NE wa_saida-pstdat_e+4(2).
      IF wa_saida-docdat_e+4(2) NE '00'.
        wa_saida-status = icon_message_warning_small.
      ENDIF.
    ENDIF.

    APPEND wa_saida TO it_saida.

    CLEAR: wa_j_1bnflin-refkey, wa_bkpf-belnr, wa_bkpf-waers, wa_j_1bnfdoc-bukrs, wa_bkpf-bukrs.

    IF it_bkpf_aux[] IS NOT INITIAL.
**<<<------"158522 - NMS - INI------>>>
*      CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
*        TABLES
*          it_bkpf     = it_bkpf_aux
*          it_partidas = tl_partidas.
* Aguarda enquanto verifica se a quantidade de sessões solicitadas é superior ao número de sessões disponíveis
* Segurança de solicitações de sessões para não travar o servidor.
      WAIT UNTIL vg_task_ativa GT vg_tasks.
      ADD 1 TO vg_task_ativa.
      CONCATENATE 'PR' wa_j_1bnfdoc-docnum INTO vl_taskname.
* Chama a Função de paralelismo de Busca Movimentos de FI - BSI/BSA K/D.
      CALL FUNCTION 'ZFIF_PARAL_GL_SALDO_BSIX'
        DESTINATION vl_dest
        STARTING NEW TASK vl_taskname
        PERFORMING zf_retur_paral_gl_saldo_bsix ON END OF TASK
        TABLES
          it_bkpf = it_bkpf_aux.

      CLEAR it_bkpf_aux.
**<<<------"158522 - NMS - FIM------>>>
    ENDIF.

  ENDLOOP.
**<<<------"158522 - NMS - INI------>>>
* Esperar até que todas as sessões sejam finalizadas. O número é decrementado na subrtoina
* de retorno da função e incrementado antes da RFC ser chamada.
  WAIT UNTIL vg_task_ativa EQ 0.
**<<<------"158522 - NMS - FIM------>>>
  SORT it_partidas BY bukrs belnr buzei gjahr.    "<<<------"158522 - NMS ------->>>

  LOOP AT it_saida ASSIGNING <fs_saida>.
    CLEAR wa_partidas.    "<<<------"158522 - NMS ------->>>
    READ TABLE it_partidas INTO wa_partidas WITH KEY bukrs = <fs_saida>-bukrs_e
                                                     belnr = <fs_saida>-belnr_e
                                                     gjahr = <fs_saida>-gjahr BINARY SEARCH.    "<<<------"158522 - NMS ------->.

    IF sy-subrc IS INITIAL.
      <fs_saida>-dmbtr_e = wa_partidas-dmbtr.
      <fs_saida>-dmbe2_e = wa_partidas-dmbe2.
      <fs_saida>-vbund_e = wa_partidas-vbund.
      <fs_saida>-hkont_e = wa_partidas-hkont.

    ENDIF.
    CLEAR wa_partidas.    "<<<------"158522 - NMS ------->>>
    READ TABLE it_partidas INTO wa_partidas WITH KEY bukrs = <fs_saida>-bukrs_s
                                                     belnr = <fs_saida>-belnr_s
                                                     gjahr = <fs_saida>-gjahr BINARY SEARCH.    "<<<------"158522 - NMS ------->.
    IF sy-subrc IS INITIAL.
      <fs_saida>-dmbtr_s = wa_partidas-dmbtr.
      <fs_saida>-dmbe2_s = wa_partidas-dmbe2.
      <fs_saida>-vbund_s = wa_partidas-vbund.
      <fs_saida>-hkont_s = wa_partidas-hkont.
      <fs_saida>-ctaid_s = wa_partidas-hkont+4(3).
    ENDIF.
**<<<------"158522 - NMS - INI------>>>
      <fs_saida>-dmbtr_d = <fs_saida>-dmbtr_e - <fs_saida>-dmbtr_s.
      <fs_saida>-dmbe2_d = <fs_saida>-dmbe2_e - <fs_saida>-dmbe2_s.
**<<<------"158522 - NMS - FIM------>>>
  ENDLOOP.
* Verifica se foi marcado "Somente Conta a Receber" na tela de seleção.    "<<<------"158522 - NMS ------->.
  IF v_sctarec = '112'.
    SORT it_saida BY ctaid_s.
    DELETE it_saida WHERE ctaid_s <> '112' .

  ENDIF.

ENDFORM.                    "F_ORGANIZA_DADOS
*=============================================================================*
*Form F_Alv                                                                   *
*=============================================================================*
FORM f_alv.
  PERFORM alv_preenche_cat USING:
"SAIDA
'STATUS'             'STATUS'                        '08'  '' ''   '',                "Status da nota
'DOCNUM_S'           'Doc Num_S'                     '  '  'X' ''   '',
'PARID_S  '          'Cod.Cliente'                   '12'  '' ''   '',                "COD_CLIENTE
'NAME1_S  '          'Nome Cliente'                  '38'  '' ''   '',                "NOME_CLIENTE
'NFENUM_S '          'NR NF Saida'                   '11'  '' ''   '',                "NR_NF_SAIDA
'SERIES_S '          'Serie NF Saida'                '11'  '' ''   '',                "SERIE_NF_SAIDA
'DOCDAT_S '          'Dt Doc. Saida'                 '12'  '' ''   '',                "DT_DOC_SAIDA
'PSTDAT_S '          'Dt Lcto. Saida'                '12'  '' ''   '',                "DT_LCTO_SAIDA
'BELNR_S  '          'Doc. Contabil Saida'           '15'  '' ''   '',                "DOC-CONTABIL_SAIDA
'WAERS_S  '          'Moeda Doc. Saida'              '14'  '' ''   '',                "MOEDA_DOC_SAIDA
'DMBTR_S  '          'Brl Saida'                     '15'  '' ''   '',                "BRL_SAIDA
'DMBE2_S  '          'Usd Saida'                     '15'  '' ''   '',                "USD_SAIDA
'VBUND_S  '          'Soc. Parceira Saida'           '15'  '' ''   '',                "SOC_PARCEIRA_SAIDA
'HKONT_S  '          'Cta. Contabil Saida'           '15'  '' ''   ''.                "CTA_CONTABIL_SAIDA
  "'CTAID_S  '          'Cta. Resumo'        '10'  '' ''   ''.                           "CTA_CTB_SAIDA RESUMO PSA - FOI PRECISO PARA VALIDAR

  PERFORM alv_preenche_cat USING:
"ENTRADA
'PARID_E'          'Cod.Fornecedor'                   '14'  '' ''   '',               "COD_FORNECEDOR
'DOCNUM_E'         'Doc Num_E'                        '  '  'X' ''   '',
'NAME1_E'          'Nome Fornecedor'                  '38'  '' ''   '',               "NOME_CLIENTE
'NFENUM_E'         'NR NF Entrada'                    '12'  '' ''   '',               "NR_NF_ENTRADA
'SERIES_E'         'Serie NF Entrada'                 '13'  '' ''   '',               "SERIE_NF_ENTRADA
'DOCDAT_E'         'Dt Doc. Entrada'                  '13'  '' ''   '',               "DT_DOC_ENTRADA
'PSTDAT_E'         'Dt. Lcto. Entrada'                '15'  '' ''   '',               "DT_LCTO_ENTRADA
'BELNR_E'          'Doc. Contabil Entrada'            '18'  '' ''   '',               "DOC-CONTABIL_ENTRADA
'WAERS_E'          'Moeda Doc. Entrada'               '16'  '' ''   '',               "MOEDA_DOC_ENTRADA
'DMBTR_E'          'Brl Entrada'                      '15'  '' ''   '',               "BRL_ENTRADA
'DMBE2_E'          'Usd Entrada'                      '15'  '' ''   '',               "USD_ENTRADA
**<<<------"158522 - NMS - INI------>>>
'DMBTR_D'          'Brl Diferença'                    '15'  '' ''   '',               "BRL_DIFERENCA
'DMBE2_D'          'Usd Diferença'                    '15'  '' ''   '',               "USD_DIFERENCA
**<<<------"158522 - NMS - FIM------>>>
'VBUND_E'          'Soc. Parceira Entrada'            '17'  '' ''   '',               "SOC_PARCEIRA_ENTRADA
'HKONT_E'          'Cta. Contabil Entrada'            '17'  '' ''   ''.               "CTA_CONTABIL_ENTRADA
ENDFORM.                    "F_ALV

"&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat   USING   p_campo TYPE c
                                p_desc  TYPE c
                                p_tam   TYPE c
                                p_hot   TYPE c
                                p_zero  TYPE c
                                p_sum   TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-outputlen = p_tam.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.


  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT
"&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM container_html .

  DATA : dl_length        TYPE i,                           " Length
         dl_background_id TYPE sdydo_key VALUE space. " Background_id

  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = dg_dyndoc_id
      bottom   = space
    IMPORTING
      length   = dl_length.

  CALL METHOD dg_dyndoc_id->merge_document.

  CALL METHOD dg_dyndoc_id->set_document_background
    EXPORTING
      picture_id = dl_background_id.

  dg_dyndoc_id->html_control = dg_html_cntrl.

  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_html1
    EXCEPTIONS
      html_display_error = 1.

ENDFORM.                    " CONTAINER_HTML

*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       To add Text
*----------------------------------------------------------------------*
FORM add_text USING p_text  TYPE sdydo_text_element
                    p_style TYPE sdydo_attribute
                    p_size  TYPE sdydo_attribute
                    p_color TYPE sdydo_attribute.

* Adding text
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text          = p_text
      sap_style     = p_style
      sap_fontsize  = p_size
      sap_color     = p_color
      sap_fontstyle = cl_dd_area=>sans_serif.

  "SAP_STYLE    = CL_DD_AREA=>HEADING
  "SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
  "SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
ENDFORM.                    " ADD_TEXT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_objects OUTPUT.

  DATA: url(255) TYPE c.

* Create container and ALV objects only once
  IF ctl_cccontainer IS INITIAL.

    IF sy-batch IS INITIAL.

*   Create object for container
      CREATE OBJECT ctl_cccontainer
        EXPORTING
          container_name              = 'TELA_0100'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


      CREATE OBJECT dg_dyndoc_id
        EXPORTING
          style = 'ALV_GRID'.

      CREATE OBJECT dg_splitter
        EXPORTING
          parent  = ctl_cccontainer
          rows    = 2
          columns = 1.

      CALL METHOD dg_splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = dg_parent_html.

      CREATE OBJECT dg_splitter_2
        EXPORTING
          parent  = dg_parent_html
          rows    = 1
          columns = 2.

      CALL METHOD dg_splitter_2->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = dg_parent_html1.

      CALL METHOD dg_splitter_2->set_column_width
        EXPORTING
          id    = 1
          width = 40.

      CALL METHOD dg_splitter_2->get_container
        EXPORTING
          row       = 1
          column    = 2
        RECEIVING
          container = dg_parent_html2.

      CREATE OBJECT picture
        EXPORTING
          parent = dg_parent_html2.

      PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

      CALL METHOD picture->load_picture_from_url
        EXPORTING
          url = url.

      CALL METHOD picture->set_display_mode
        EXPORTING
          display_mode = picture->display_mode_fit_center.

      CALL METHOD dg_splitter->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = dg_parent_grid.

      CALL METHOD dg_splitter->set_row_height
        EXPORTING
          id     = 1
          height = 15.


*   Create object for ALV grid inside container
      CREATE OBJECT ctl_alv_resumo
        EXPORTING
          i_parent = dg_parent_grid.

*   Fill info for layout variant
      PERFORM fill_gs_variant.

      "GS_LAYOUT-SEL_MODE = 'A'.
      gs_layout-zebra      = 'X'.
      "GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.

*   Create Object for Event Handler
      CREATE OBJECT event_handler.
      SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv_resumo.
*    SET HANDLER EVENT_HANDLER->TOP_OF_PAGE          FOR CTL_ALV_RESUMO.
      CREATE OBJECT wa_alv
        EXPORTING
          i_parent          = dg_parent_grid
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

**   Send data to ALV grid
      CALL METHOD ctl_alv_resumo->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          is_variant           = gs_variant
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saida.

      PERFORM cria_html_cab.

      CALL METHOD ctl_alv_resumo->list_processing_events
        EXPORTING
          i_event_name = 'TOP_OF_PAGE'
          i_dyndoc_id  = dg_dyndoc_id.

      CLEAR: gf_first_display.
    ELSE.

      CREATE OBJECT ctl_cccontainer
        EXPORTING
          container_name              = 'TELA_0100'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.

      CREATE OBJECT ctl_alv_resumo
        EXPORTING
          i_parent = ctl_cccontainer.

      CALL METHOD ctl_alv_resumo->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          is_variant           = gs_variant
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saida.

    ENDIF.

  ENDIF.

  CALL METHOD ctl_alv_resumo->refresh_table_display.

  CALL METHOD ctl_alv_resumo->set_scroll_info_via_id
    EXPORTING
      is_col_info = gs_scroll_col
      is_row_no   = gs_scroll_row.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_html_cab .

  DATA: column                  TYPE REF TO cl_dd_area,
        column_1                TYPE REF TO cl_dd_area,
        column_2                TYPE REF TO cl_dd_area,
        table_element           TYPE REF TO cl_dd_table_element,
        table_element2          TYPE REF TO cl_dd_table_element,
        p_text                  TYPE sdydo_text_element,
        p_text_table            TYPE sdydo_text_table,
        sdydo_text_element(255),
        vg_mes(2), vg_ano(4),
        qtd                     TYPE i.

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
      sap_align = 'CENTER'
      sap_style = cl_dd_document=>heading.

  p_text = 'Report de Saída x entrada  - Intercompany '.
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
    IMPORTING
      column = column_1.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_2.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 1
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 2
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  sdydo_text_element = 'Empresa:'.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = 'Filial:'.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = 'Data de Lançamento: '.
  APPEND sdydo_text_element TO p_text_table.

  CALL METHOD column_1->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  CLEAR: p_text_table, sdydo_text_element.

  "Empresa*********
  IF s_bukrs-low IS NOT INITIAL.
    sdydo_text_element = s_bukrs-low.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.



  "Filial*********
  IF s_branch-low IS NOT INITIAL.
    sdydo_text_element = s_branch-low.

    IF s_branch-high IS NOT INITIAL.
      CONCATENATE sdydo_text_element '-' s_branch-high INTO sdydo_text_element SEPARATED BY space.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.

  "Data de Lançamento*****
  IF s_pstdat-low IS NOT INITIAL.
    CONCATENATE s_pstdat-low+6(2) '/' s_pstdat-low+4(2) '/' s_pstdat-low(4) INTO sdydo_text_element.
    IF s_pstdat-high IS NOT INITIAL.
      CONCATENATE sdydo_text_element '-' s_pstdat-high+6(2) INTO sdydo_text_element SEPARATED BY space.
      CONCATENATE sdydo_text_element '/' s_pstdat-high+4(2) '/' s_pstdat-high(4) INTO sdydo_text_element.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.


  CALL METHOD column_2->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  PERFORM container_html.

ENDFORM.                    " CRIA_HTML_CAB

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
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

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = space.
  gs_variant-log_group   = space.
  gs_variant-username    = space.
  gs_variant-variant     = space.
  gs_variant-text        = space.
  gs_variant-dependvars  = space.

ENDFORM.                    " FILL_GS_VARIANT
**&---------------------------------------------------------------------*
**&      Form  MOSTRAR_DOC_FISCAL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_WA_CTE_ALV_  text
**----------------------------------------------------------------------*
*FORM MOSTRAR_DOC_FISCAL  USING P_DOCNUM_S TYPE J_1BDOCNUM.
*
*  DATA: GF_NFOBJN LIKE J_1BINTERF-NFOBJN.
*
*  CHECK P_DOCNUM_S IS NOT INITIAL.
*
*  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
*    EXPORTING
*      DOC_NUMBER         = P_DOCNUM_S
*    IMPORTING
*      OBJ_NUMBER         = GF_NFOBJN
*    EXCEPTIONS
*      DOCUMENT_NOT_FOUND = 1
*      DOCUM_LOCK         = 2
*      OTHERS             = 3.
*
*  CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
*    EXPORTING
*      OBJ_NUMBER         = GF_NFOBJN
*    EXCEPTIONS
*      OBJECT_NOT_FOUND   = 1
*      SCR_CTRL_NOT_FOUND = 2
*      OTHERS             = 3.
*
*ENDFORM.                    " MOSTRAR_DOC_FISCAL
**<<<------"158522 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& FORM ZF_RETUR_PARAL_BUSCA_IMPOSTO
*&---------------------------------------------------------------------*
*& Retorno da Função de Paralelismo.
*&---------------------------------------------------------------------*
*& -->P_TASKNAME Nome (ID) da Tarefa ativa
*&---------------------------------------------------------------------*
FORM zf_retur_paral_gl_saldo_bsix USING p_taskname.

  DATA: tl_partidas TYPE TABLE OF zde_fi_gl_partidas_cli_for.

  RECEIVE RESULTS FROM FUNCTION 'ZLABRFI_RFC_PARALL_SYCH'
     IMPORTING
      it_partidas = tl_partidas.

  APPEND LINES OF tl_partidas TO it_partidas.
  CLEAR tl_partidas.

  SUBTRACT 1 FROM vg_task_ativa.

ENDFORM.
**<<<------"158522 - NMS - FIM------>>>
