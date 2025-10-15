
*&---------------------------------------------------------------------*
*& Report  ZMMR135
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr135.

TABLES: mchb,ckmlhd, ckmlpp, mlkey.
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
    cd_agru    TYPE zmmt0083-cd_agru,
    matnr      TYPE mseg-matnr,
    werks      TYPE mseg-werks,
    lgort      TYPE mseg-lgort,
    charg      TYPE mseg-charg,
    bukrs      TYPE mseg-bukrs,
    mblnr      TYPE mseg-mblnr,
    bwart      TYPE mseg-bwart,
    gjahr      TYPE mseg-gjahr,
    belnr      TYPE bsis-belnr,
    dmbtr      TYPE bsis-dmbtr,
    menge      TYPE mseg-menge,
    ebeln      TYPE mseg-ebeln,
    ebelp      TYPE mseg-ebelp,
    budat_mkpf TYPE mseg-budat_mkpf,
    xblnr      TYPE mkpf-xblnr,
  END OF ty_docs,


  BEGIN OF ty_mseg.
    INCLUDE TYPE mseg.
TYPES: awkey TYPE bkpf-awkey.
TYPES: END OF ty_mseg.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:
  it_zmmt0083 TYPE TABLE OF zmmt0083,
  it_zmmt0084 TYPE TABLE OF zmmt0084,

  it_mkpf     TYPE TABLE OF mkpf,
  it_mseg     TYPE TABLE OF ty_mseg,
  it_matnr    TYPE TABLE OF ty_matnr,
  it_docs     TYPE TABLE OF ty_docs,
  it_docs_alv TYPE TABLE OF ty_docs,
  it_color    TYPE TABLE OF lvc_s_scol.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: ok-code     TYPE sy-ucomm,

      wa_mkpf     TYPE mkpf,
      wa_mseg     TYPE ty_mseg,
      wa_docs     TYPE ty_docs,
      wa_zmmt0083 TYPE zmmt0083,
      wa_zmmt0084 TYPE zmmt0084,
      wa_t156     TYPE t156,
      wa_color    TYPE lvc_s_scol.


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
      obg_conteiner      TYPE REF TO cl_gui_custom_container,
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

DATA: v_camp(10),      " variável p/ montar campo dinâmico
      v_camp2(10),      " variável p/ montar campo dinâmico
      v_camp3(10),      " variável p/ montar campo dinâmico
      v_text(100).    " variável p/ montar texto dinâmico

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    DATA: vwerks   TYPE mseg-werks,
          vlgort   TYPE mseg-lgort,
          vcharg   TYPE mseg-charg,
          vcd_agru TYPE zmmt0083-cd_agru,
          vval_ctb TYPE bsis-dmbtr.

    IF e_row-index GT 0.
      LOOP AT <fs_data> INTO <wa_data>.
        IF sy-tabix =   e_row-index.
          EXIT.
        ENDIF.
      ENDLOOP.
      "
      ASSIGN COMPONENT 'WERKS'  OF STRUCTURE <wa_data> TO <fs_campo>.
      MOVE <fs_campo> TO vwerks.
      "
      ASSIGN COMPONENT 'LGORT'  OF STRUCTURE <wa_data> TO <fs_campo>.
      MOVE <fs_campo> TO vlgort.
      "
      ASSIGN COMPONENT 'CHARG'  OF STRUCTURE <wa_data> TO <fs_campo>.
      MOVE <fs_campo> TO vcharg.
      "
      IF  e_column+1(3) = 'QTD' OR e_column = 'SLD_INIQ' OR e_column = 'MAT_TRANS'.
        REFRESH it_docs_alv .
        IF e_column = 'SLD_INIQ'.
          vcd_agru = 0.
        ELSEIF e_column = 'MAT_TRANS'.
          vcd_agru = 9999.
        ELSE.
          vcd_agru = e_column+4(4).
        ENDIF.
        LOOP AT it_docs INTO wa_docs WHERE cd_agru = vcd_agru
                                     AND   werks   = vwerks
                                     AND   lgort   = vlgort
                                     AND   charg   = vcharg.
          APPEND wa_docs TO it_docs_alv.
        ENDLOOP.
        CALL SCREEN 0200 STARTING AT 050 3
                         ENDING   AT 172 38.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD catch_hotspot.

    IF e_row_id GT 0.
      READ TABLE it_docs_alv INTO wa_docs INDEX e_row_id-index.
      IF e_column_id = 'MBLNR' AND wa_docs-mblnr IS NOT INITIAL.

* ---> S4 Migration - 19/07/2023 - DG
*        SET PARAMETER ID 'MBN' FIELD WA_DOCS-MBLNR.
*        SET PARAMETER ID 'MJA' FIELD WA_DOCS-GJAHR.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_docs-mblnr
            i_mjahr             = wa_docs-gjahr.
        "I_ZEILE = I_FINAL-ZEILE.

* <--- S4 Migration - 19/07/2023 - DG3 - DG
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
"DATA   DYFIELDS LIKE DYNPREAD OCCURS 1 WITH HEADER LINE.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_matnr FOR ckmlhd-matnr NO INTERVALS NO-EXTENSION OBLIGATORY  ,
                  p_werks FOR ckmlhd-bwkey OBLIGATORY,
                  p_lgort FOR mchb-lgort,
                  p_charg FOR mchb-charg.
SELECTION-SCREEN: END OF BLOCK b1.

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
  SELECT *
    FROM zmmt0084
    INTO TABLE it_zmmt0084
    WHERE  code    = sy-tcode.

  CHECK it_zmmt0084 IS NOT INITIAL.

  SELECT *
  FROM zmmt0083
  INTO TABLE it_zmmt0083
  FOR ALL ENTRIES IN it_zmmt0084
  WHERE cd_agru = it_zmmt0084-cd_agru
  AND   code    = it_zmmt0084-code.

  CHECK it_zmmt0083 IS NOT INITIAL.


  "seleciona os documentos de material pelo codigo de movimento configurados
  SELECT *
      FROM mseg
       INTO CORRESPONDING FIELDS OF TABLE it_mseg
     WHERE werks IN p_werks
     AND   matnr IN s_matnr
     AND   lgort IN p_lgort
     AND   charg IN p_charg
     AND   lgort NE ''.

  IF it_mseg[] IS NOT INITIAL.
    SELECT *
      FROM mkpf
      INTO TABLE it_mkpf
      FOR ALL ENTRIES IN it_mseg
      WHERE mblnr = it_mseg-mblnr
      AND   mjahr = it_mseg-mjahr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
FORM f_saida.
  "
  DATA: vmat_trans   TYPE mseg-menge,
        vmat_trans_e TYPE mseg-menge,
        v_doc(1).

  SORT: it_mseg     BY mblnr gjahr matnr werks zeile,
        it_mkpf     BY mblnr mjahr,
        it_zmmt0083 BY bwart.

  "Cria layout e tabela dinamica
  PERFORM f_monta_layout.
  "
  "colocar cor
  LOOP AT it_zmmt0084 INTO wa_zmmt0084.
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
  LOOP AT it_mseg INTO wa_mseg.
    "Campos fixos
    "
    vmat_trans = 0.
    CLEAR v_doc.
    IF '313_314' CS wa_mseg-bwart.
      IF ( wa_mseg-bwart = '313' AND wa_mseg-shkzg = 'S' ) OR
         ( wa_mseg-bwart = '314' AND wa_mseg-shkzg = 'H' ).
        vmat_trans = wa_mseg-menge.
        IF wa_mseg-shkzg = 'H'.
          MULTIPLY vmat_trans BY -1.
        ENDIF.
      ENDIF.

    ELSEIF '315_316' CS wa_mseg-bwart.
      IF wa_mseg-bwart = '315' AND wa_mseg-shkzg = 'H'.
        CONTINUE.
      ENDIF.
      IF wa_mseg-bwart = '316' AND wa_mseg-shkzg = 'S'.
        CONTINUE.
      ENDIF.

      vmat_trans = wa_mseg-menge.
      IF wa_mseg-shkzg = 'S'.
        MULTIPLY vmat_trans BY -1.
      ENDIF.
    ENDIF.
    "
    PERFORM f_carrega_dados USING:
          wa_mseg-bukrs      'BUKRS',
          wa_mseg-werks      'WERKS',
          wa_mseg-lgort      'LGORT',
          wa_mseg-charg      'CHARG',
          vmat_trans         'MAT_TRANS'.

    "acumula no saldo final
    vmat_trans_e = vmat_trans * -1.
    v_camp2 = 'SLD_FIMQ'.
    CLEAR: v_camp.
    PERFORM f_carrega_dados USING vmat_trans_e   v_camp2.

    "Campos dinamicos
    CLEAR wa_zmmt0083.
    READ TABLE it_zmmt0083 INTO wa_zmmt0083  WITH KEY bwart = wa_mseg-bwart BINARY SEARCH.
    IF sy-subrc = 0.
      v_doc = 'X'.
      READ TABLE it_zmmt0084 INTO wa_zmmt0084  WITH KEY cd_agru = wa_zmmt0083-cd_agru BINARY SEARCH.

      IF wa_mseg-shkzg = 'H'.
        MULTIPLY wa_mseg-menge BY -1.
      ENDIF.
      "acumula no saldo final
      v_camp2 = 'SLD_FIMQ'.
      CLEAR: v_camp.
      "
      CONCATENATE wa_zmmt0084-entsai 'QTD'
                  wa_zmmt0084-cd_agru
                  INTO v_camp.
      PERFORM f_carrega_dados USING wa_mseg-menge     v_camp.
      PERFORM f_carrega_dados USING wa_mseg-menge     v_camp2.
    ELSEIF NOT ( '315_316' CS wa_mseg-bwart ).
      v_doc = 'X'.
      v_camp2 = 'SLD_INIQ'.
      wa_zmmt0083-cd_agru = 0.

      IF wa_mseg-shkzg = 'H'.
        MULTIPLY wa_mseg-menge BY -1.
      ENDIF.
      PERFORM f_carrega_dados USING wa_mseg-menge     v_camp2.
    ENDIF.

    READ TABLE it_mkpf  INTO wa_mkpf WITH KEY mblnr = wa_mseg-mblnr
                                              mjahr = wa_mseg-mjahr BINARY SEARCH.

    IF v_doc = 'X'.
      "Grava analitico
      CLEAR wa_docs.
      wa_docs-cd_agru = wa_zmmt0083-cd_agru.
      wa_docs-bukrs   = wa_mseg-bukrs.
      wa_docs-mblnr   = wa_mseg-mblnr.
      wa_docs-matnr   = wa_mseg-matnr.
      wa_docs-werks   = wa_mseg-werks.
      wa_docs-bwart   = wa_mseg-bwart.
      wa_docs-gjahr   = wa_mseg-mjahr.
      wa_docs-menge   = wa_mseg-menge.
      wa_docs-lgort   = wa_mseg-lgort.
      wa_docs-charg   = wa_mseg-charg.
      wa_docs-ebeln      = wa_mseg-ebeln.
      wa_docs-ebelp      = wa_mseg-ebelp.
      wa_docs-budat_mkpf = wa_mseg-budat_mkpf.
      wa_docs-xblnr      = wa_mkpf-xblnr.
      APPEND wa_docs TO it_docs.
    ENDIF.
    IF vmat_trans NE 0.
      "Grava analitico
      CLEAR wa_docs.
      wa_zmmt0083-cd_agru = 9999.
      wa_docs-cd_agru = wa_zmmt0083-cd_agru.
      wa_docs-bukrs   = wa_mseg-bukrs.
      wa_docs-mblnr   = wa_mseg-mblnr.
      wa_docs-matnr   = wa_mseg-matnr.
      wa_docs-werks   = wa_mseg-werks.
      wa_docs-bwart   = wa_mseg-bwart.
      wa_docs-gjahr   = wa_mseg-mjahr.
      wa_docs-menge   = vmat_trans.
      wa_docs-lgort   = wa_mseg-lgort.
      wa_docs-charg   = wa_mseg-charg.
      wa_docs-ebeln      = wa_mseg-ebeln.
      wa_docs-ebelp      = wa_mseg-ebelp.
      wa_docs-budat_mkpf = wa_mseg-budat_mkpf.
      wa_docs-xblnr      = wa_mkpf-xblnr.
      APPEND wa_docs TO it_docs.
    ENDIF.

    PERFORM f_carrega_alv USING <fs_data2>
                                <wa_data2>.
    CLEAR <wa_data2>.
  ENDLOOP.

  "copia dados para a tabela que tem a cor
  LOOP AT <fs_data2> INTO <wa_data2>.
    MOVE-CORRESPONDING <wa_data2> TO <wa_data>.
    ASSIGN COMPONENT 'T_CELLCOLORS'   OF STRUCTURE <wa_data> TO <fs_campo>.
    MOVE it_color TO <fs_campo>.

    PERFORM f_carrega_alv2 USING <fs_data>
                                 <wa_data>.
    CLEAR <wa_data>.

  ENDLOOP.

  SORT it_docs BY cd_agru werks lgort charg.
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

  SET TITLEBAR '0100' .

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
         'BUKRS'      'MSEG'      'Empresa'                     '10'       'BUKRS',
         'WERKS'      'MSEG'      'Centro'                      '10'       'WERKS',
         'LGORT'      'MCHB'      'Depósito'                    '12'       'LGORT',
         'CHARG'      'MCHB'      'Lote'                        '12'       'CHARG',
         'SLD_INIQ'   'CKMLPP'    'Não Class.'                  '15'       'LBKUM',
         'MAT_TRANS'  'CKMLPP'    'Materiais Transito'          '15'       'LBKUM'.

  "Organiza ENTRADA/SAIDA
  SORT it_zmmt0084 BY seq entsai cd_agru.

  "Entradas
  LOOP AT it_zmmt0084 INTO wa_zmmt0084 WHERE entsai = 'E'.
    CLEAR: v_camp,
           v_text.

    CONCATENATE  wa_zmmt0084-entsai 'QTD'
                 wa_zmmt0084-cd_agru
                 INTO v_camp.

    CONCATENATE  wa_zmmt0084-ds_gru_val '' INTO v_text SEPARATED BY space.
    PERFORM monta_fieldcat USING
           v_camp   'MSEG' v_text            '15' 'MENGE'.

  ENDLOOP.

  "Saidas
  LOOP AT it_zmmt0084 INTO wa_zmmt0084 WHERE entsai = 'S'.

    CLEAR: v_camp,
           v_text.
    CONCATENATE  wa_zmmt0084-entsai 'QTD'
                 wa_zmmt0084-cd_agru
                 INTO v_camp.

    CONCATENATE  wa_zmmt0084-ds_gru_val '' INTO v_text SEPARATED BY space.
    PERFORM monta_fieldcat USING
           v_camp   'MSEG' v_text            '15' 'MENGE'.

  ENDLOOP.


  PERFORM monta_fieldcat USING:
        'SLD_FIMQ'   'CKMLPP'    'Estoque Final'   '15'       'LBKUM'.


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

  IF p_ref_field = 'MENGE'.
    wa_fcat_lvc-datatype = 'QUAN'.
    wa_fcat_lvc-intlen   = 17.
  ELSEIF p_ref_field = 'LBKUM'.
    wa_fcat_lvc-datatype = 'QUAN'.
    wa_fcat_lvc-intlen   = 20.
  ELSE.
    wa_fcat_lvc-ref_table   = p_tabref.
    wa_fcat_lvc-ref_field   = p_ref_field.
  ENDIF.

  IF p_field+1(3) = 'QTD' OR p_field = 'SLD_INIQ' OR p_field = 'MAT_TRANS'.
    wa_fcat_lvc-hotspot = c_x.
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
  IF p_campo = 'SLD_FIMQ'.
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
* ---> S4 Migration - 19/06/2023 - MA
*  DATA VHEADER(50).
  DATA vheader(60).
* <--- S4 Migration - 19/06/2023 - MA
  DATA vmatnr TYPE mara-matnr.


  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

  IF obg_conteiner IS INITIAL.
    CREATE OBJECT obg_conteiner
      EXPORTING
        container_name = g_container.


    CREATE OBJECT grid1
      EXPORTING
        i_parent = obg_conteiner.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = s_matnr-low
      IMPORTING
        output = vmatnr.
    CONCATENATE 'Saldo material' vmatnr INTO vheader SEPARATED BY space.

    wa_layout-grid_title = vheader.
    wa_layout-cwidth_opt = c_x.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = space.
    wa_layout-col_opt    = c_x.

    wa_layout-sel_mode   = 'B'.
    wa_layout-box_fname  = ''.
    wa_layout-ctab_fname = 'T_CELLCOLORS'.

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


    SET HANDLER:
              lcl_event_handler=>on_double_click FOR grid1.

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
         'WERKS'      'MSEG'      'Centro'         '10'       'WERKS',
         'MBLNR'      'MSEG'      'Doc.Material'   '10'       'MBLNR',
         'BWART'      'MSEG'      'Tipo'           '06'       'BWART',
         'MENGE'      'MSEG'      'Quantidade'     '15'       'MENGE',
         'BUDAT_MKPF' 'MSEG'      'Data lct'       '12'       'BUDAT_MKPF',
         'XBLNR'      'MKPF'      'Nota Mat'       '15'       'XBLNR',
         'LGORT'      'MSEG'      'Depósito'       '10'       'LGORT',
         'CHARG'      'MSEG'      'Lote'           '15'       'CHARG',
         'EBELN'      'MSEG'      'Pedido'         '10'       'EBELN',
         'EBELP'      'MSEG'      'Item'           '05'       'EBELP'.

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



  CONCATENATE 'Material'  : s_matnr-low
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
