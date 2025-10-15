*&---------------------------------------------------------------------*
*& Report  ZPPR018
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zppr018_v2.
TABLES: mchb, zpps_ximfbf_log, zmmt0006, zsdt0001.


TYPES: BEGIN OF ty_saida,
         chave           TYPE zpps_ximfbf_log-obj_key,
         tipo            TYPE char255,
         werks           TYPE mchb-werks,
         matnr           TYPE mchb-matnr,
         desc_material   TYPE maktx,
         nr_romaneio     TYPE zsdt0001-nr_romaneio,
         safra           TYPE mchb-charg,
         budat           TYPE budat,
         mblnr           TYPE mkpf-mblnr,
         mjahr           TYPE mkpf-mjahr,
         charg           TYPE mchb-charg,
         lgort           TYPE mchb-lgort,
         desc_dep        TYPE lgobe,
         qtde_mov        TYPE mchb-clabs,
         ch_ref_romaneio TYPE zsdt0001-ch_referencia,
       END OF ty_saida.

TYPES: BEGIN OF ty_setleaf,
         setclass      TYPE  setleaf-setclass,
         subclass      TYPE setleaf-subclass,
         setname       TYPE setleaf-setname,
         lineid        TYPE setleaf-lineid,
         valfrom       TYPE setleaf-valfrom,
         cod_clas_not2 TYPE coas-auart,
       END OF ty_setleaf.

TYPES: BEGIN OF ty_zpps_ximfbf_log,
         ch_ref_rom TYPE zsdt0001-ch_referencia.
         INCLUDE STRUCTURE zpps_ximfbf_log.
TYPES: END OF ty_zpps_ximfbf_log.


TYPES: BEGIN OF ty_zmmt0006,
         ch_ref_rom TYPE zsdt0001-ch_referencia.
         INCLUDE STRUCTURE zmmt0006.
TYPES: END OF ty_zmmt0006.



DATA: t_saida           TYPE TABLE OF  ty_saida,
      t_mchb            TYPE TABLE OF  mchb,
      t_zpps_ximfbf_log TYPE TABLE OF  ty_zpps_ximfbf_log,
      t_zmmt0006        TYPE TABLE OF  ty_zmmt0006,
      t_zsdt0001        TYPE TABLE OF  zsdt0001,
      t_mseg            TYPE TABLE OF  mseg,
      t_t001l           TYPE TABLE OF  t001l,
      t_makt            TYPE TABLE OF  makt,

      w_saida           TYPE ty_saida.


DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
      dg_parent_1        TYPE REF TO cl_gui_container,
      dg_parent_alv      TYPE REF TO cl_gui_container,
      gs_layout          TYPE lvc_s_layo,
      gs_variant         TYPE disvariant,
      it_exclude_fcode   TYPE ui_functions,
      wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
      dg_dyndoc_id       TYPE REF TO cl_dd_document,
      ctl_alv            TYPE REF TO cl_gui_alv_grid,
      ctl_alv02          TYPE REF TO cl_gui_alv_grid,
      table_element      TYPE REF TO cl_dd_table_element,
      column             TYPE REF TO cl_dd_area,
      table_element2     TYPE REF TO cl_dd_table_element,
      column_1           TYPE REF TO cl_dd_area,
      column_2           TYPE REF TO cl_dd_area.

DATA:
  wa_stable            TYPE lvc_s_stbl,
  wa_stable1           TYPE lvc_s_stbl,
  wa_stable2           TYPE lvc_s_stbl,
  it_fieldcatalog      TYPE lvc_t_fcat,
  wa_fieldcatalog      TYPE lvc_s_fcat,

  descricao_material   TYPE maktx,
  denominacao_deposito TYPE lgobe.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_matnr       FOR mchb-matnr            OBLIGATORY,
                  p_werks       FOR mchb-werks            OBLIGATORY,
                  p_dtmov       FOR zsdt0001-dt_movimento OBLIGATORY,
                  p_safra       FOR zsdt0001-nr_safra NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT sy-datum(4),
                  p_nr_rom      FOR zsdt0001-nr_romaneio.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.


START-OF-SELECTION.

  PERFORM: seleciona_dados_01,
           tratar_dados_01.

  PERFORM imprimi_alv.


END-OF-SELECTION.

FORM seleciona_dados_01.

  SELECT *
    FROM zpps_ximfbf_log INTO CORRESPONDING FIELDS OF TABLE t_zpps_ximfbf_log
   WHERE werks  IN p_werks
     AND dtmvto IN p_dtmov.

  SELECT *
    FROM zmmt0006 INTO CORRESPONDING FIELDS OF TABLE t_zmmt0006
   WHERE werks_d IN p_werks
     AND budat   IN p_dtmov.

  LOOP AT t_zpps_ximfbf_log ASSIGNING FIELD-SYMBOL(<fs_zpps_ximfbf_log>).

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = <fs_zpps_ximfbf_log>-matnr
      IMPORTING
        output = <fs_zpps_ximfbf_log>-matnr.

    <fs_zpps_ximfbf_log>-ch_ref_rom = <fs_zpps_ximfbf_log>-obj_key(11).

  ENDLOOP.

  LOOP AT t_zmmt0006 ASSIGNING FIELD-SYMBOL(<fs_zmmt0006>).

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = <fs_zmmt0006>-matnr
      IMPORTING
        output = <fs_zmmt0006>-matnr.

    <fs_zmmt0006>-ch_ref_rom = <fs_zmmt0006>-ch_referencia(11).

  ENDLOOP.

  DELETE t_zpps_ximfbf_log WHERE matnr        NOT IN p_matnr OR
                                 charg(4)     NOT IN p_safra OR
                                 id_interface NE 'S' OR "Não for Entrada Produção Sigam
                                 ( zst_atlz   NE 'I' AND zst_atlz NE 'X' ).

  DELETE t_zmmt0006  WHERE matnr             NOT IN p_matnr OR
                           batch             NOT IN p_safra OR
                           algodao_caroco    NE abap_true.

  IF t_zpps_ximfbf_log[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0001 INTO TABLE t_zsdt0001
       FOR ALL ENTRIES IN t_zpps_ximfbf_log
     WHERE ch_referencia = t_zpps_ximfbf_log-ch_ref_rom.

    SELECT *
      FROM makt INTO TABLE t_makt
      FOR ALL ENTRIES IN t_zpps_ximfbf_log
     WHERE matnr EQ t_zpps_ximfbf_log-matnr
       AND spras EQ sy-langu.

    SELECT *
      FROM mseg AS a INTO TABLE t_mseg
      FOR ALL ENTRIES IN t_zpps_ximfbf_log
    WHERE mblnr = t_zpps_ximfbf_log-mblnr
      AND mjahr = t_zpps_ximfbf_log-dtmvto(4)
      AND NOT EXISTS (  SELECT mblnr
                          FROM mseg AS b
                         WHERE b~smbln = a~mblnr
                           AND b~sjahr = a~mjahr )
      AND NOT EXISTS ( SELECT mblnr
                         FROM mseg AS b
                        WHERE b~mblnr = a~mblnr
                          AND b~smbln NE space ).

  ENDIF.

  IF t_zmmt0006[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0001 APPENDING TABLE t_zsdt0001
       FOR ALL ENTRIES IN t_zmmt0006
     WHERE ch_referencia = t_zmmt0006-ch_ref_rom.

    SELECT *
      FROM makt APPENDING TABLE t_makt
      FOR ALL ENTRIES IN t_zmmt0006
     WHERE matnr EQ t_zmmt0006-matnr
       AND spras EQ sy-langu.

    SELECT *
       FROM mseg AS a APPENDING TABLE t_mseg
       FOR ALL ENTRIES IN t_zmmt0006
     WHERE mblnr = t_zmmt0006-doc_material
       AND mjahr = t_zmmt0006-budat(4)
       AND NOT EXISTS (  SELECT mblnr
                           FROM mseg AS b
                          WHERE b~smbln = a~mblnr
                            AND b~sjahr = a~mjahr )
       AND NOT EXISTS ( SELECT mblnr
                          FROM mseg AS b
                         WHERE b~mblnr = a~mblnr
                           AND b~smbln NE space ).

  ENDIF.

  IF t_mseg[] IS NOT INITIAL.
    SELECT *
      FROM t001l INTO TABLE t_t001l
      FOR ALL ENTRIES IN t_mseg
     WHERE werks EQ t_mseg-werks
       AND lgort EQ t_mseg-lgort.
  ENDIF.

  DELETE t_zsdt0001 WHERE tp_movimento NE 'E'.

  IF ( t_zpps_ximfbf_log[] IS INITIAL ) AND ( t_zmmt0006[] IS INITIAL ).
    MESSAGE  'Não foram encontrados dados para os parâmetros informados'   TYPE 'I'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.

FORM tratar_dados_01.

  SORT: t_zsdt0001  BY ch_referencia,
        t_t001l     BY werks lgort.

  LOOP AT t_zpps_ximfbf_log INTO DATA(lwa_zpps_ximfbf_log).

    CLEAR: w_saida.

    CASE lwa_zpps_ximfbf_log-zst_atlz.
      WHEN 'I'.
        w_saida-tipo         = 'Produção Própria'.
      WHEN 'X'.
        w_saida-tipo         = 'Transferência - Provisão'.
    ENDCASE.

    w_saida-chave            = lwa_zpps_ximfbf_log-obj_key.
    w_saida-ch_ref_romaneio  = lwa_zpps_ximfbf_log-ch_ref_rom.
    w_saida-werks            = lwa_zpps_ximfbf_log-werks.
    w_saida-matnr            = lwa_zpps_ximfbf_log-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = w_saida-matnr
      IMPORTING
        output = w_saida-matnr.

    READ TABLE t_makt INTO DATA(lwa_makt) WITH KEY matnr = lwa_zpps_ximfbf_log-matnr.
    IF sy-subrc EQ 0.
      w_saida-desc_material = lwa_makt-maktx.
    ENDIF.

    IF lwa_zpps_ximfbf_log-nr_romaneio IS NOT INITIAL.
      w_saida-nr_romaneio = lwa_zpps_ximfbf_log-nr_romaneio.
    ELSE.
      READ TABLE t_zsdt0001 INTO DATA(lwa_zsdt0001) WITH KEY ch_referencia = lwa_zpps_ximfbf_log-ch_ref_rom BINARY SEARCH.
      IF sy-subrc EQ 0.
        w_saida-nr_romaneio = lwa_zsdt0001-nr_romaneio.
      ENDIF.
    ENDIF.

    CHECK w_saida-nr_romaneio IN p_nr_rom.

    w_saida-safra = lwa_zpps_ximfbf_log-charg(4).
    w_saida-budat = lwa_zpps_ximfbf_log-dtmvto.

    IF lwa_zpps_ximfbf_log-mblnr IS NOT INITIAL.
      READ TABLE t_mseg INTO DATA(lwa_mseg) WITH KEY mblnr = lwa_zpps_ximfbf_log-mblnr
                                                     mjahr = lwa_zpps_ximfbf_log-dtmvto(4).
      IF sy-subrc EQ 0.
        w_saida-mblnr = lwa_mseg-mblnr.
        w_saida-mjahr = lwa_mseg-mjahr.
        w_saida-charg = lwa_mseg-charg.
        w_saida-lgort = lwa_mseg-lgort.

        READ TABLE t_t001l INTO DATA(lwa_t001l) WITH KEY werks = lwa_mseg-werks
                                                         lgort = lwa_mseg-lgort  BINARY SEARCH.
        IF sy-subrc EQ 0.
          w_saida-desc_dep = lwa_t001l-lgobe.
        ENDIF.

        w_saida-qtde_mov = lwa_mseg-menge.
      ENDIF.
    ENDIF.

    APPEND w_saida TO t_saida.

  ENDLOOP.

  LOOP AT t_zmmt0006 INTO DATA(lwa_zmmt0006).

    CLEAR: w_saida.

    w_saida-tipo             = 'Transferência'.
    w_saida-chave            = lwa_zmmt0006-ch_referencia.
    w_saida-ch_ref_romaneio  = lwa_zmmt0006-ch_ref_rom.
    w_saida-werks            = lwa_zmmt0006-werks.
    w_saida-matnr            = lwa_zmmt0006-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = w_saida-matnr
      IMPORTING
        output = w_saida-matnr.

    READ TABLE t_makt INTO lwa_makt WITH KEY matnr = lwa_zmmt0006-matnr.
    IF sy-subrc EQ 0.
      w_saida-desc_material = lwa_makt-maktx.
    ENDIF.

    READ TABLE t_zsdt0001 INTO lwa_zsdt0001 WITH KEY ch_referencia = lwa_zmmt0006-ch_ref_rom BINARY SEARCH.
    IF sy-subrc EQ 0.
      w_saida-nr_romaneio = lwa_zsdt0001-nr_romaneio.
    ENDIF.

    CHECK w_saida-nr_romaneio IN p_nr_rom.

    w_saida-safra = lwa_zmmt0006-batch(4).
    w_saida-budat = lwa_zmmt0006-budat.

    IF lwa_zmmt0006-doc_material IS NOT INITIAL.
      READ TABLE t_mseg INTO lwa_mseg WITH KEY mblnr = lwa_zmmt0006-doc_material
                                               mjahr = lwa_zmmt0006-budat(4).
      IF sy-subrc EQ 0.
        w_saida-mblnr = lwa_mseg-mblnr.
        w_saida-mjahr = lwa_mseg-mjahr.
        w_saida-charg = lwa_mseg-charg.
        w_saida-lgort = lwa_mseg-lgort.

        READ TABLE t_t001l INTO lwa_t001l WITH KEY werks = lwa_mseg-werks
                                                   lgort = lwa_mseg-lgort  BINARY SEARCH.
        IF sy-subrc EQ 0.
          w_saida-desc_dep = lwa_t001l-lgobe.
        ENDIF.

        w_saida-qtde_mov = lwa_mseg-menge.
      ENDIF.
    ENDIF.

    APPEND w_saida TO t_saida.

  ENDLOOP.

  DATA(t_saida_aux) = t_saida[].
  LOOP AT t_saida INTO DATA(lwa_saida) WHERE mblnr IS INITIAL.
    DATA(_chave) = lwa_saida-chave.

    DATA(_descartar_registro) = abap_false.
    LOOP AT t_saida_aux INTO DATA(lwa_saida_aux) WHERE ch_ref_romaneio EQ lwa_saida-ch_ref_romaneio
                                                   AND ( chave  > lwa_saida-chave OR
                                                         MBLNR IS NOT INITIAL ).
      _descartar_registro = abap_true.
      EXIT.
    ENDLOOP.

    IF _descartar_registro = abap_true.
      DELETE t_saida WHERE chave = _chave.
    ENDIF.
  ENDLOOP.

  IF t_saida[] IS INITIAL.
    MESSAGE  'Não foram encontrados dados para os parâmetros informados'   TYPE 'I'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.


FORM imprimi_alv.

  REFRESH it_fieldcatalog.
  PERFORM preenche_cat USING:

     'TIPO'             'Tipo Entrada'         '25'     ''        ''   '' ,
     'WERKS'            'Centro'               '06'     ''        ''   '' ,
     'MATNR'            'Material'             '10'     ''        ''   '' ,
     'DESC_MATERIAL'    'Desc.Material'        '30'     ''        ''   '' ,
     'NR_ROMANEIO'      'Romaneio'             '10'     ''        ''   '' ,
     'SAFRA'            'Safra'                '05'     ''        ''   '' ,
     'BUDAT'            'Dt.Lcto'              '10'     ''        ''   '' ,
     'MBLNR'            'Doc.Material'         '10'     ''        ''   '' ,
     'MJAHR'            'Ano.Doc.Mat.'         '10'     ''        ''   '' ,
     'CHARG'            'Lote'                 '08'     ''        ''   '' ,
     'LGORT'            'Depósito'             '08'     ''        ''   '' ,
     'DESC_DEP'         'Denominação Depósito' '25'     ''        ''   '' ,
     'QTDE_MOV'         'Qtde.Entrada Estoque' '25'     ''        'X'  '' .

  CALL SCREEN 0100.
ENDFORM.

FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_sum)
                        VALUE(p_just).

  wa_fieldcatalog-fieldname = p_campo.
  wa_fieldcatalog-coltext   = p_desc.
  wa_fieldcatalog-scrtext_l = p_desc.
  wa_fieldcatalog-scrtext_m = p_desc.
  wa_fieldcatalog-scrtext_s = p_desc.


  wa_fieldcatalog-outputlen = p_tam.
  wa_fieldcatalog-no_zero   = p_zero.
  wa_fieldcatalog-do_sum    = p_sum.
  wa_fieldcatalog-just      = p_just.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.

FORM user_command  USING e_row_id TYPE lvc_s_row
                         p_e_column_id TYPE lvc_s_col
                         p_es_row_no TYPE lvc_s_roid.

  PERFORM imprimi_alv.


ENDFORM.

FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.

MODULE status_0100 OUTPUT.

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITULO'.

  IF g_custom_container IS INITIAL.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.


    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = t_saida.

  ELSE.
    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.

MODULE user_command_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
