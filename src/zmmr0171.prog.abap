*----------------------------------------------------------------------*
* Programa..: ZMMR0171                                                 *
* Tipo......: Report                                                   *
* Transação.: ZMM0181                                                  *
* Descrição.: Relatório de movimentação diaria por centro              *
*----------------------------------------------------------------------*

REPORT zmmr0171  MESSAGE-ID z01.

*&---------------------------------------------------------------------*
*& Tabelas Transparentes                                               *
*&---------------------------------------------------------------------*
TABLES: mseg.

*&---------------------------------------------------------------------*
*& Declaração de Tipos                                                 *
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_saida_alv,
    werks       TYPE mseg-werks,
    matnr       TYPE mseg-matnr,
    maktx       TYPE makt-maktx,
    meins       TYPE mseg-meins,
    lgort       TYPE mseg-lgort,
    lgpbe       TYPE mard-lgpbe,
    menge_a     TYPE mseg-menge,
    menge_e     TYPE mseg-menge,
    menge_s     TYPE mseg-menge,
    menge_f     TYPE mseg-menge,
    menge_z(16) TYPE c,
  END OF ty_saida_alv.

TYPES:
  BEGIN OF ty_mseg,
    mblnr      TYPE mseg-mblnr,
    mjahr      TYPE mseg-mjahr,
    zeile      TYPE mseg-zeile,
    budat_mkpf TYPE mseg-budat_mkpf,
    werks      TYPE mseg-werks,
    matnr      TYPE mseg-matnr,
    meins      TYPE mseg-meins,
    lgort      TYPE mseg-lgort,
    shkzg      TYPE mseg-shkzg,
    menge      TYPE mseg-menge,
    bwart      TYPE mseg-bwart,
  END OF ty_mseg,

  BEGIN OF ty_mara,
    matnr TYPE mara-matnr,
    meins(3) TYPE C,
  END OF ty_mara,

  BEGIN OF ty_total,
    werks   TYPE mseg-werks,
    matnr   TYPE mseg-matnr,
    lgort   TYPE mseg-lgort,
    menge_e TYPE mseg-menge,
    menge_s TYPE mseg-menge,
  END OF ty_total.


*&---------------------------------------------------------------------*
*& Declaração de Tabelas Internas / Estruturas                         *
*&---------------------------------------------------------------------*

DATA: it_mseg      TYPE TABLE OF ty_mseg.
DATA: it_mseg_aux  TYPE TABLE OF ty_mseg.
DATA: it_total_ate TYPE TABLE OF ty_total.
DATA: it_total_dia TYPE TABLE OF ty_total.
DATA: it_mara      TYPE TABLE OF ty_mara.
DATA: wa_total     TYPE ty_total.

*&---------------------------------------------------------------------*
*& Declaração de Variáveis                                             *
*&---------------------------------------------------------------------*


DATA: dg_splitter     TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer TYPE REF TO cl_gui_container,
      ctl_alv         TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog TYPE lvc_t_fcat,
      wa_afield       TYPE lvc_s_fcat,
      gs_variant      TYPE disvariant,
      gs_layout       TYPE lvc_s_layo,
      it_saida        TYPE TABLE OF ty_saida_alv,
      wa_saida        TYPE ty_saida_alv.



*&---------------------------------------------------------------------*
*& Tela de Seleção                                                     *
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-s01.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s02.
SELECT-OPTIONS: s_werks FOR mseg-werks OBLIGATORY NO INTERVALS.
SELECT-OPTIONS: s_lgort FOR mseg-lgort NO INTERVALS.
SELECT-OPTIONS: s_matnr FOR mseg-matnr NO INTERVALS.
SELECT-OPTIONS: s_data  FOR mseg-budat_mkpf OBLIGATORY NO-EXTENSION.
SELECT-OPTIONS: s_bwart FOR mseg-bwart NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN END OF BLOCK sel.

*&---------------------------------------------------------------------*
*&       P R O C E S S A M E N T O                                     *
*&---------------------------------------------------------------------*


START-OF-SELECTION.
  PERFORM f_seleciona_dados.
  PERFORM f_exibe_relatorio.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .


  DATA: lv_dat TYPE sy-datum.
  DATA: lv_dati TYPE sy-datum.
  DATA: lv_datf TYPE sy-datum.
  DATA: lv_ano TYPE n LENGTH 4.
  DATA: lv_mes TYPE n LENGTH 2.

  lv_dat = s_data-low.

  CONCATENATE  s_data-low+0(6) '01' INTO lv_dati.
  lv_datf = s_data-low - 1.
  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
    EXPORTING
      currdate   = lv_dat
      backmonths = 1
    IMPORTING
      newdate    = lv_dat.

  lv_ano = lv_dat(4).
  lv_mes = lv_dat+4(2).


  PERFORM f_progress USING 40 text-m01.

  SELECT m~mblnr,
         m~mjahr,
         m~zeile,
         m~budat_mkpf,
         m~werks,
         m~matnr,
         m~meins,
         m~lgort,
         m~shkzg,
         m~menge,
         m~bwart
    FROM mseg AS m
    INTO CORRESPONDING FIELDS OF TABLE @it_mseg
    WHERE m~matnr IN @s_matnr
    AND   m~werks IN @s_werks
    AND   m~lgort IN @s_lgort
    AND   m~bwart IN @s_bwart
    AND   m~budat_mkpf IN @s_data
    AND   m~lgort <> ''
    AND   m~matnr <> ''.

  CHECK it_mseg[] IS NOT INITIAL.

  it_mseg_aux[] = it_mseg[].

  IF s_data-low+6(2) NE '01'.
    SELECT  mblnr,
            mjahr,
            zeile,
            budat_mkpf,
            werks,
            matnr,
            meins,
            lgort,
            shkzg,
            menge,
            bwart
       FROM mseg
       APPENDING CORRESPONDING FIELDS OF TABLE @it_mseg
      FOR ALL ENTRIES IN @it_mseg_aux
       WHERE matnr EQ @it_mseg_aux-matnr
       AND   werks EQ @it_mseg_aux-werks
       AND   lgort EQ @it_mseg_aux-lgort
       AND   budat_mkpf BETWEEN @lv_dati AND @lv_datf.
  ENDIF.


  SELECT makt~matnr,
         makt~maktx
  FROM makt
  INTO TABLE @DATA(it_makt)
  FOR ALL ENTRIES  IN @it_mseg
  WHERE makt~matnr EQ @it_mseg-matnr
  AND   makt~spras EQ @sy-langu.

  SELECT matnr,
         meins
    FROM mara
    INTO TABLE @it_mara
    FOR ALL ENTRIES IN @it_mseg
    WHERE matnr EQ @it_mseg-matnr.

  SELECT matnr,
         werks,
         lgort,
         lgpbe
  FROM mard
  INTO TABLE @DATA(it_mard)
  FOR ALL ENTRIES IN @it_mseg
  WHERE matnr EQ @it_mseg-matnr
  AND   werks EQ @it_mseg-werks
  AND   lgort EQ @it_mseg-lgort.

  SELECT matnr,
         werks,
         lgort,
         labst
  FROM mardh
  INTO TABLE @DATA(it_mardh)
  FOR ALL ENTRIES IN @it_mseg
  WHERE matnr EQ @it_mseg-matnr
  AND   werks EQ @it_mseg-werks
  AND   lgort EQ @it_mseg-lgort
  AND   lfgja EQ @lv_ano
  AND   lfmon EQ @lv_mes.


  SORT: it_mseg  BY werks matnr lgort budat_mkpf,
        it_makt  BY matnr,
        it_mara  BY matnr,
        it_mard  BY matnr werks lgort,
        it_mardh BY matnr werks lgort.


  PERFORM f_progress USING 40 text-m02.

  "saldo até o dia e periodo
  LOOP AT it_mseg INTO DATA(wa_mseg).
    MOVE-CORRESPONDING wa_mseg TO wa_total.
    IF wa_mseg-shkzg = 'S'.
      wa_total-menge_e = wa_mseg-menge.
      wa_total-menge_s = 0.
    ELSE.
      wa_total-menge_e = 0.
      wa_total-menge_s = wa_mseg-menge.
    ENDIF.
    IF wa_mseg-budat_mkpf LT s_data-low.
      COLLECT wa_total INTO it_total_ate.
    ELSE.
      COLLECT wa_total INTO it_total_dia.
    ENDIF.
  ENDLOOP.

  SORT: it_total_ate  BY werks matnr lgort,
        it_total_dia  BY werks matnr lgort.

  it_mseg_aux[] = it_mseg[].

*---> 04/07/2023 - Migração S4 - WS
  SORT it_mseg_aux BY  werks matnr lgort.
*<--- 04/07/2023 - Migração S4 - WS
  DELETE ADJACENT DUPLICATES FROM it_mseg_aux COMPARING  werks matnr lgort.

  LOOP AT it_mseg_aux INTO DATA(wa_mseg_aux).
    CLEAR wa_saida.

    wa_saida-werks    = wa_mseg_aux-werks.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_mseg_aux-matnr
      IMPORTING
        output = wa_saida-matnr.

    READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = wa_mseg_aux-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-maktx    = wa_makt-maktx.
    ENDIF.

    wa_saida-meins    = wa_mseg_aux-meins.
    READ TABLE it_mara INTO DATA(wa_mara) WITH KEY matnr = wa_mseg_aux-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-meins    = wa_mara-meins.
      IF wa_saida-meins = 'ST'.
        wa_saida-meins = 'PEÇ'.
      ENDIF.
    ENDIF.


    wa_saida-lgort    = wa_mseg_aux-lgort.

    READ TABLE it_mard INTO DATA(wa_mard) WITH KEY matnr = wa_mseg_aux-matnr
                                                   werks = wa_mseg_aux-werks
                                                   lgort = wa_mseg_aux-lgort BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-lgpbe    = wa_mard-lgpbe.
    ENDIF.

    "saldo anterior + movimente até a data inicial
    READ TABLE it_mardh INTO DATA(wa_mardh)  WITH KEY matnr = wa_mseg_aux-matnr
                                                      werks = wa_mseg_aux-werks
                                                      lgort = wa_mseg_aux-lgort BINARY SEARCH.

    IF sy-subrc = 0.
      wa_saida-menge_a = wa_mardh-labst.
    ENDIF.

    READ TABLE it_total_ate INTO wa_total WITH KEY matnr = wa_mseg_aux-matnr
                                                   werks = wa_mseg_aux-werks
                                                   lgort = wa_mseg_aux-lgort BINARY SEARCH.

    IF sy-subrc = 0.
      wa_saida-menge_a = wa_saida-menge_a + wa_total-menge_e - wa_total-menge_s.
    ENDIF.

    READ TABLE it_total_dia INTO wa_total WITH KEY matnr = wa_mseg_aux-matnr
                                                   werks = wa_mseg_aux-werks
                                                   lgort = wa_mseg_aux-lgort BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-menge_e  = wa_total-menge_e.
      wa_saida-menge_s  = wa_total-menge_s.
    ENDIF.

    wa_saida-menge_f  = wa_saida-menge_a + wa_saida-menge_e - wa_saida-menge_s.
    CLEAR wa_saida-menge_z.
    "
    APPEND wa_saida TO it_saida .
  ENDLOOP.

ENDFORM.         "F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_PROGRESS
*&---------------------------------------------------------------------*
FORM f_progress USING p_percentage TYPE any
                      p_text       TYPE any.

  CHECK sy-batch IS INITIAL.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percentage
      text       = p_text.

ENDFORM.                    " F_PROGRESS


*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exibe_relatorio .


  CALL SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  M_STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_status_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TL0100' WITH s_data-low s_data-high.

  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 1
        columns = 1.

    ctl_cccontainer = dg_splitter->get_container( row = 1 column = 1 ).

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = ctl_cccontainer.

    PERFORM fill_it_fieldcatalog.
    PERFORM fill_gs_variant.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_saida[].

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_0100_exit INPUT.

  IF ctl_alv IS NOT INITIAL.
    ctl_alv->free( ).
  ENDIF.
  CLEAR: ctl_alv.

  IF ctl_cccontainer IS NOT INITIAL.
    ctl_cccontainer->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->free( ).
  ENDIF.
  CLEAR: dg_splitter.

  CLEAR it_saida[].

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_0100 INPUT.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_it_fieldcatalogALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog.

  REFRESH it_fieldcatalog.

  DATA i TYPE i.
  wa_afield-tabname     = 'IT_SAIDA'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.
  "
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'WERKS'.
  wa_afield-scrtext_s = 'Centro'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MATNR'.
  wa_afield-scrtext_s = 'Material'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MAKTX'.
  wa_afield-scrtext_s = 'Descrição'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MEINS'.
  wa_afield-scrtext_s = 'UN'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'LGORT'.
  wa_afield-scrtext_s = 'Depósito'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'LGPBE'.
  wa_afield-scrtext_s = 'Endereço'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MENGE_A'.
  wa_afield-scrtext_s = 'Saldo Anterior'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MENGE_E'.
  wa_afield-scrtext_s = 'Entrada'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MENGE_S'.
  wa_afield-scrtext_s = 'Saida'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MENGE_F'.
  wa_afield-scrtext_s = 'Saldo'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MENGE_Z'.
  wa_afield-scrtext_s = 'Físico'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcatalog.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
*  gs_variant-handle      = '0100'.
*  gs_variant-log_group   = abap_false.
*  gs_variant-username    = abap_false.
*  gs_variant-variant     = abap_false.
*  gs_variant-text        = abap_false.
*  gs_variant-dependvars  = abap_false.

  gs_layout-zebra        = abap_true.
  gs_layout-no_rowmark   = space.
  gs_layout-cwidth_opt   = 'X'.
  gs_layout-sel_mode     = 'A'.
*  gs_layout-info_fname = 'LINE_COLOR'.
*  gs_layout-stylefname = 'STYLE'.
*  gs_layout-ctab_fname = 'COLOR_CELL'.

ENDFORM.
