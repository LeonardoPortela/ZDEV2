*&---------------------------------------------------------------------*
*& Report  ZPPR018
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zppr018.
TABLES: mchb, zpps_ximfbf_log, zmmt0006.


TYPES: BEGIN OF ty_saida,
         werks         TYPE mchb-werks,
         matnr         TYPE mchb-matnr,
         desc_material TYPE maktx,
         id_cotton     TYPE zpps_ximfbf_log-id_cotton,
         charg         TYPE mchb-charg,
         lgort         TYPE mchb-lgort,
         desc_dep      TYPE lgobe,
         clabs         TYPE mchb-clabs,
         budat         TYPE budat,
       END OF ty_saida.



TYPES: BEGIN OF ty_setleaf,
         setclass      TYPE  setleaf-setclass,
         subclass      TYPE setleaf-subclass,
         setname       TYPE setleaf-setname,
         lineid        TYPE setleaf-lineid,
         valfrom       TYPE setleaf-valfrom,
         cod_clas_not2 TYPE coas-auart,
       END OF ty_setleaf.




DATA: t_saida           TYPE TABLE OF  ty_saida,
      t_mchb            TYPE TABLE OF  mchb,
      t_zpps_ximfbf_log TYPE TABLE OF  zpps_ximfbf_log,
      t_zmmt0006        TYPE TABLE OF  zmmt0006,

      w_saida           TYPE ty_saida,
      w_mchb            TYPE mchb,
      w_zpps_ximfbf_log TYPE zpps_ximfbf_log,
      w_zmmt0006        TYPE zmmt0006.


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


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_matnr FOR mchb-matnr OBLIGATORY,
                p_werks FOR mchb-werks OBLIGATORY,
                p_lgort FOR mchb-lgort OBLIGATORY,
                p_charg FOR mchb-charg,
                p_xblnr FOR zpps_ximfbf_log-id_cotton.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.


START-OF-SELECTION.

  PERFORM: seleciona_dados_01,
   tratar_dados_01.

  PERFORM imprimi_alv.


END-OF-SELECTION.

FORM seleciona_dados_01.

  IF p_xblnr IS INITIAL.

    SELECT
     *
      FROM mchb
      INTO TABLE t_mchb
      WHERE clabs GT 0
            AND matnr IN p_matnr
            AND werks IN p_werks
            AND lgort IN p_lgort
            AND charg IN p_charg.

    IF  t_mchb[]  IS NOT INITIAL.

      SELECT *
        FROM zpps_ximfbf_log
        INTO TABLE t_zpps_ximfbf_log
        FOR ALL ENTRIES IN t_mchb
        WHERE charg EQ t_mchb-charg
        AND werks EQ t_mchb-werks.

      SELECT *
        FROM zmmt0006
        INTO TABLE t_zmmt0006
        FOR ALL ENTRIES IN t_mchb
        WHERE batch_d EQ t_mchb-charg
        AND werks_d EQ t_mchb-werks.


    ENDIF.

  ELSE.

    SELECT *
            FROM zpps_ximfbf_log
            INTO TABLE t_zpps_ximfbf_log
            WHERE charg IN p_charg
            AND werks IN p_werks
            AND id_cotton IN p_xblnr .

    SELECT *
           FROM zmmt0006
           INTO TABLE t_zmmt0006
           WHERE  werks_d IN p_werks
           AND lgort IN p_lgort
           AND batch_d IN p_charg
           AND id_cotton IN p_xblnr .

    IF t_zpps_ximfbf_log[] IS NOT INITIAL.

      SELECT *
       FROM mchb
       INTO TABLE t_mchb
       FOR ALL ENTRIES IN t_zpps_ximfbf_log
       WHERE clabs GT 0
             AND matnr IN p_matnr
             AND werks EQ t_zpps_ximfbf_log-werks
             AND lgort IN p_lgort
             AND charg EQ t_zpps_ximfbf_log-charg.
    ENDIF.

    IF t_zmmt0006[] IS NOT INITIAL.

      SELECT *
     FROM mchb
     INTO TABLE t_mchb
     FOR ALL ENTRIES IN t_zmmt0006
     WHERE clabs GT 0
           AND matnr IN p_matnr
           AND werks EQ t_zmmt0006-werks
           AND lgort IN p_lgort
           AND charg EQ t_zmmt0006-batch_d.

    ENDIF.

  ENDIF.

  IF t_mchb[] IS INITIAL.
    MESSAGE  'Não foram encontrados dados para os parâmetros informados'   TYPE 'I'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.

FORM tratar_dados_01.

  LOOP AT t_mchb INTO w_mchb.
    IF strlen( w_mchb-charg ) GT 4.
      w_saida-werks = w_mchb-werks.

      SELECT SINGLE maktx
            FROM makt
            INTO descricao_material
            WHERE matnr EQ w_mchb-matnr AND spras EQ sy-langu.

      w_saida-desc_material = descricao_material.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = w_mchb-matnr
        IMPORTING
          output = w_mchb-matnr.


      w_saida-matnr = w_mchb-matnr.

      READ TABLE t_zpps_ximfbf_log INTO w_zpps_ximfbf_log WITH KEY charg = w_mchb-charg.
      IF  sy-subrc IS INITIAL.
        w_saida-id_cotton = w_zpps_ximfbf_log-id_cotton.
        w_saida-budat = w_zpps_ximfbf_log-data.
      ELSE.

        READ TABLE t_zmmt0006 INTO w_zmmt0006 WITH KEY batch_d = w_mchb-charg.

        IF  sy-subrc IS INITIAL.
          w_saida-id_cotton = w_zmmt0006-id_cotton.
          w_saida-budat = w_zmmt0006-budat.
        ENDIF.

      ENDIF.

      w_saida-charg = w_mchb-charg.
      w_saida-lgort = w_mchb-lgort.

      SELECT SINGLE lgobe
        FROM t001l
        INTO denominacao_deposito
        WHERE lgort = w_mchb-lgort AND werks EQ w_mchb-werks.

      w_saida-desc_dep = denominacao_deposito.

      w_saida-clabs = w_mchb-clabs.

      APPEND w_saida TO t_saida.

      CLEAR: w_saida, denominacao_deposito, descricao_material, w_zpps_ximfbf_log, w_zmmt0006.
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
     'WERKS'            'Centro'               '8'     ''        ''   '' ,
     'MATNR'            'Material'             '8'     ''        ''   '' ,
     'DESC_MATERIAL'    'Descrição Material'   '30'     ''        ''   '' ,
     'ID_COTTON'        'ID_COTTON'            '16'     ''        ''   '' ,
     'CHARG'            'Lote'                 '10'     ''        ''   '' ,
     'LGORT'            'Depósito'             '8'     ''        ''   '' ,
     'DESC_DEP'         'Denominação Depósito' '25'     ''        ''   '' ,
     'CLABS'            'Útil Livre'           '12'     ''        'X'  '' ,
     'budat'            'Data lançamento'      '10'     ''        ''   '' .
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
