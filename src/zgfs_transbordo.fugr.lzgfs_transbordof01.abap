*----------------------------------------------------------------------*
***INCLUDE LZGFS_TRANSBORDOF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*

**********************************************************************
* obter SETs
**********************************************************************
FORM f_get_set.

  DATA: valor         TYPE p DECIMALS 2,
        text_out(255) TYPE c.

  FREE: r_matkl_sel.

  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(t_tvarvc)
   WHERE name = 'MAGGI_GR_FERTILIZANTES'.

  LOOP AT t_tvarvc  INTO DATA(w_tvarvc).
    r_matkl_sel-sign   = w_tvarvc-sign.
    r_matkl_sel-option = w_tvarvc-opti.
    r_matkl_sel-low    = w_tvarvc-low.
    APPEND r_matkl_sel.
  ENDLOOP.

ENDFORM.

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados.

  FREE: t_saida, t_ekpo, t_zsdt0306, t_zsdt0225.

  IF s_bukrs[] IS INITIAL.
    MESSAGE s024(sd) WITH text-300 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT ekpo~ebeln ekpo~ebelp eket~etenr eket~charg
         ekpo~loekz ekko~aedat ekko~bukrs ekpo~werks
         ekpo~lgort ekpo~matnr ekpo~matkl ekpo~txz01
         ekpo~menge ekpo~meins lfa1~lifnr lfa1~name1
         ekko~unsez
    FROM ekko
   INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
   INNER JOIN eket ON eket~ebeln = ekko~ebeln
   INNER JOIN lfa1 ON lfa1~lifnr = ekko~lifnr
    INTO TABLE t_ekpo
   WHERE ekko~ebeln IN s_ebeln
     AND ekko~lifnr IN s_lifnr
     AND ekko~bukrs IN s_bukrs
     AND ekpo~werks IN s_werks
     AND ekko~aedat IN s_data
     AND ekpo~loekz  = abap_off.

  DELETE t_ekpo WHERE matkl NOT IN r_matkl_sel[].

  CHECK t_ekpo[] IS NOT INITIAL.

  SORT t_ekpo BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM t_ekpo
                        COMPARING ebeln ebelp.

  SELECT *
    FROM zsdt0306
    INTO TABLE t_zsdt0306
     FOR ALL ENTRIES IN t_ekpo
   WHERE ebeln = t_ekpo-ebeln
     AND ebelp = t_ekpo-ebelp.

  IF t_zsdt0306[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0225
      INTO TABLE t_zsdt0225
       FOR ALL ENTRIES IN t_zsdt0306
     WHERE id_seq = t_zsdt0306-id_seq.
  ENDIF.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_dados.

  FREE: t_saida.

  LOOP AT t_ekpo INTO w_ekpo.

    CLEAR: w_saida, w_zsdt0306, w_zsdt0225.

    READ TABLE t_zsdt0306 INTO w_zsdt0306 WITH KEY ebeln  = w_ekpo-ebeln
                                                   ebelp  = w_ekpo-ebelp.
    CHECK sy-subrc <> 0.

    READ TABLE t_zsdt0225 INTO w_zsdt0225 WITH KEY id_seq = w_zsdt0306-id_seq.

    w_saida-status      = COND #( WHEN w_zsdt0306 IS INITIAL THEN icon_wf_unlink
                                                             ELSE icon_wf_link ).

    w_saida-id_seq      = w_zsdt0306-id_seq.
    w_saida-lifnr       = w_ekpo-lifnr.
    w_saida-lifnr_name  = w_ekpo-name1.
    w_saida-ebeln       = w_ekpo-ebeln.
    w_saida-ebelp       = w_ekpo-ebelp.
    w_saida-bukrs       = w_ekpo-bukrs.
    w_saida-bukrs_fat   = w_zsdt0306-bukrs_fat.
    w_saida-werks_fat   = w_zsdt0306-werks_fat.
    w_saida-werks       = w_ekpo-werks.
    w_saida-matnr       = w_ekpo-matnr.
    w_saida-matkl       = w_ekpo-matkl.
    w_saida-lgort       = w_ekpo-lgort.
    w_saida-charg       = w_ekpo-charg.
    w_saida-unsez       = w_ekpo-unsez.
    w_saida-txz01       = w_ekpo-txz01.
    w_saida-menge       = w_ekpo-menge.
    w_saida-meins       = w_ekpo-meins.
    w_saida-nr_ov       = w_zsdt0225-nr_ov.
    w_saida-docnum      = w_zsdt0225-docnum.
    APPEND w_saida     TO t_saida.
  ENDLOOP.

ENDFORM.

**********************************************************************
* salvar dados
**********************************************************************
FORM f_salvar_dados CHANGING p_erro.

  FREE: t_selecao,
        p_erro.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH text-101 DISPLAY LIKE 'E'.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  LOOP AT t_rows INTO w_rows.
    READ TABLE t_saida INTO w_saida INDEX w_rows-index.
    CHECK sy-subrc = 0.

    IF w_saida-id_seq IS NOT INITIAL.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF p_erro = abap_true.
    MESSAGE s024(sd) WITH text-131 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*--------------------------
* antes de vincular, checa se ja OV ou NF emitida
*--------------------------
  FREE: t_filial.

  SELECT SINGLE *
    FROM zsdt0306
    INTO @DATA(w_0306)
   WHERE id_seq = @g_id_seq.

  LOOP AT t_rows INTO w_rows.
    READ TABLE t_saida INTO w_saida INDEX w_rows-index.
    CHECK sy-subrc = 0.

    IF w_saida-nr_ov  IS NOT INITIAL OR
       w_saida-docnum IS NOT INITIAL.
      p_erro = abap_true.
      EXIT.
    ENDIF.

    IF w_0306-werks <> w_saida-werks OR
       w_0306-lgort <> w_saida-lgort OR
       w_0306-matnr <> w_saida-matnr.
      p_erro = 'Y'.
      EXIT.
    ENDIF.

    w_filial-werks      = w_saida-werks.
    w_filial-lgort      = w_saida-lgort.
    w_filial-matnr      = w_saida-matnr.
    COLLECT w_filial INTO t_filial.
  ENDLOOP.

  IF p_erro = 'Y'.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH text-151 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF p_erro = abap_true.
    MESSAGE s024(sd) WITH text-110 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE t_filial LINES DATA(l_lines).

  IF l_lines > 1.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH text-151 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*--------------------------
* salvar
*--------------------------
  LOOP AT t_rows INTO w_rows.
    READ TABLE t_saida   INTO w_saida INDEX w_rows-index.
    CHECK sy-subrc = 0.
    MOVE g_id_seq          TO w_saida-id_seq.
    MOVE w_0306-bukrs_fat  TO w_saida-bukrs_fat.
    MOVE w_0306-werks_fat  TO w_saida-werks_fat.
    APPEND w_saida         TO t_selecao.
  ENDLOOP.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog.
  PERFORM f_sort.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-zebra        = abap_false.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
* w_layout-stylefname   = 'CELLSTYLES'.
  w_layout-ctab_fname   = 'CELLCOLOR'.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid,
                 lcl_event_handler=>on_data_changed  FOR g_grid,
                 lcl_event_handler=>user_command     FOR g_grid,
                 lcl_event_handler=>toolbar          FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_saida[]
        it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

*  IF lines( t_rows ) > 0.
*    CALL METHOD g_grid->set_selected_rows
*      EXPORTING
*        it_index_rows = t_rows.
*  ENDIF.

ENDFORM.

**********************************************************************
*  barra tarefas
**********************************************************************
FORM f_funcoes.

  FREE: t_function.

  w_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_check.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND w_function TO t_function.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_sort.

  FREE: t_sort.

*  w_sort-fieldname = 'ETAPA_HEAD'.
** w_sort-subtot    = 'X'.
*  w_sort-spos      = 1.
*  w_sort-up        = 'X'.
*  APPEND w_sort   TO t_sort.
*
*  w_sort-fieldname = 'NRO_CGD'.
** w_sort-subtot    = 'X'.
*  w_sort-spos      = 2.
*  w_sort-up        = 'X'.
*  APPEND w_sort   TO t_sort.
*
*  w_sort-fieldname = 'NR_ROMANEIO'.
** w_sort-subtot    = 'X'.
*  w_sort-spos      = 3.
*  w_sort-up        = 'X'.
*  APPEND w_sort   TO t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
*   01  ''      ''       'T_ALV' 'STATUS'              'Status'                   '05'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
*   02  ''      ''       'T_ALV' 'ID_SEQ'              'Lote'                     '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    02  'LFA1'  'LIFNR'  'T_ALV' 'LIFNR'               'Cód.Fornec.'              '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    03  ''      ''       'T_ALV' 'LIFNR_NAME'          'Descr.Fornec.'            '30'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    04  ''      ''       'T_ALV' 'EBELN'               'Nr.Pedido'                '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    05  ''      ''       'T_ALV' 'EBELP'               'Item Ped.'                '09'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    06  ''      ''       'T_ALV' 'BUKRS'               'Empresa'                  '07'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    07  ''      ''       'T_ALV' 'WERKS'               'Filial'                   '07'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    08  'MARA'  'MATNR'  'T_ALV' 'MATNR'               'Cód.Material'             '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    09  ''      ''       'T_ALV' 'TXZ01'               'Descrição Material'       '30'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    10  ''      ''       'T_ALV' 'MENGE'               'Quantidade'               '13'  ' ' 'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    11  ''      ''       'T_ALV' 'MEINS'               'Und.Med'                  '07'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.
*   12  ''      ''       'T_ALV' 'DOCNUM'              'NFPS'                     '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
* w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-coltext     = p_scrtext_l.
* w_fieldcat-scrtext_s   = p_scrtext_l.
* w_fieldcat-scrtext_m   = p_scrtext_l.
* w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
  w_fieldcat-no_out      = p_no_out.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
* salvar variante
**********************************************************************
FORM f_save_variant.

  DATA: ft_params    TYPE TABLE OF rsparams,
        ft_sscr      TYPE TABLE OF rsscr,
        fs_sscr      TYPE rsscr,
        fr_selopt    TYPE RANGE OF rsscr-name,
        fs_selopt    LIKE LINE OF fr_selopt,
        f_vari_desc  TYPE varid,
        ft_vari_text TYPE TABLE OF varit,
        fs_vari_text TYPE varit,
        ft_vscreens  TYPE TABLE OF rsdynnr,
        fs_vscreens  TYPE rsdynnr,
        f_retcode    TYPE c,
        l_dynnr      TYPE sy-dynnr,
        l_varname    TYPE rsvar-variant,
        l_vartext    TYPE varit-vtext.

  CALL SCREEN 0200 STARTING AT 10 02
                     ENDING AT 57 04.

  CHECK rsvar-variant IS NOT INITIAL.
  l_dynnr   = '0100'.
  l_varname = rsvar-variant.
  l_vartext = rsvar-vtext.

*get all selection screen parameters and values
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report     = sy-repid
* IMPORTING
*     SP              = SP
    TABLES
      selection_table = ft_params
    EXCEPTIONS
      not_found       = 1
      no_report       = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.
  ENDIF.

*get fields of our subscreen
  CALL FUNCTION 'RS_ISOLATE_1_SELSCREEN'
    EXPORTING
      program     = sy-repid
      dynnr       = '0101' "l_dynnr
*     TABIX       = 0
*   IMPORTING
*     LAST_TABIX  = LAST_TABIX
    TABLES
      screen_sscr = ft_sscr
*     GLOBAL_SSCR = GLOBAL_SSCR
    EXCEPTIONS
      no_objects  = 1
      OTHERS      = 2.
  IF sy-subrc EQ 0.
    LOOP AT ft_sscr INTO fs_sscr.
      fs_selopt-sign = 'I'.
      fs_selopt-option = 'EQ'.
      fs_selopt-low = fs_sscr-name.
      APPEND fs_selopt TO fr_selopt.
    ENDLOOP.
*delete parameters and values not used on our subscreen
    DELETE ft_params WHERE selname NOT IN fr_selopt.
  ENDIF.

*set variant global data
  MOVE sy-mandt             TO f_vari_desc-mandt.
  MOVE sy-repid             TO f_vari_desc-report.
  MOVE l_varname            TO f_vari_desc-variant.
  MOVE sy-uname             TO f_vari_desc-ename.
  MOVE sy-datum             TO f_vari_desc-edat .
  MOVE sy-uzeit             TO f_vari_desc-etime.
  MOVE 'X'                  TO f_vari_desc-environmnt.

*set description of variant
  MOVE sy-mandt             TO fs_vari_text-mandt.
  MOVE sy-langu             TO fs_vari_text-langu.
  MOVE sy-repid             TO fs_vari_text-report .
  MOVE l_varname            TO fs_vari_text-variant.
  MOVE l_vartext            TO fs_vari_text-vtext.
  APPEND  fs_vari_text TO ft_vari_text.

*set subscreen number
  fs_vscreens-dynnr = l_dynnr.
  fs_vscreens-kind = ''.
  APPEND fs_vscreens TO ft_vscreens.
*create variant
  CALL FUNCTION 'RS_CREATE_VARIANT'
    EXPORTING
      curr_report               = sy-repid
      curr_variant              = l_varname
      vari_desc                 = f_vari_desc
    TABLES
      vari_contents             = ft_params
      vari_text                 = ft_vari_text
      vscreens                  = ft_vscreens
    EXCEPTIONS
      illegal_report_or_variant = 1
      illegal_variantname       = 2
      not_authorized            = 3
      not_executed              = 4
      report_not_existent       = 5
      report_not_supplied       = 6
      variant_exists            = 7
      variant_locked            = 8
      OTHERS                    = 9.
  IF sy-subrc EQ 7.
*variant already exists so ask if user wants to overwrite it
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        diagnosetext1  = 'Variant exists, do you want to overwrite it?'
        textline1      = ' '
        titel          = 'Variant exists, do you want to overwrite it?'
        cancel_display = space
      IMPORTING
        answer         = f_retcode
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    IF f_retcode EQ 'J'.
*user agreed so change existing variant
      CALL FUNCTION 'RS_CHANGE_CREATED_VARIANT'
        EXPORTING
          curr_report               = sy-repid
          curr_variant              = l_varname
          vari_desc                 = f_vari_desc
*         ONLY_CONTENTS             = ONLY_CONTENTS
        TABLES
          vari_contents             = ft_params
          vari_text                 = ft_vari_text
*         VARI_SEL_DESC             = VARI_SEL_DESC
*         OBJECTS                   = OBJECTS
        EXCEPTIONS
          illegal_report_or_variant = 1
          illegal_variantname       = 2
          not_authorized            = 3
          not_executed              = 4
          report_not_existent       = 5
          report_not_supplied       = 6
          variant_doesnt_exist      = 7
          variant_locked            = 8
          selections_no_match       = 9
          OTHERS                    = 10.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* ler variante
**********************************************************************
FORM f_get_variant.

  DATA: ft_params    TYPE TABLE OF rsparams,
        f_variant    TYPE rsvar-variant,
        f_text       TYPE rsvar-vtext,
        fs_params    TYPE rsparams,
        fs_paramsscr TYPE rsparams,
        f_fieldname  TYPE fieldname,
        ft_sscr      TYPE TABLE OF rsscr,
        l_dynnr      TYPE sy-dynnr.

  FIELD-SYMBOLS <any_selopt_itab> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <any_selopt> TYPE any.
  FIELD-SYMBOLS <any_field> TYPE any.

  l_dynnr = '0100'.

  "now I will display pop-up with variants for one subscreen
  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report               = sy-repid
*     NEW_TITLE            = ' '
      dynnr                = l_dynnr
*     INTERNAL_CALL        = ' '
*     MASKED               = 'X'
*     VARIANT              = ' '
      pop_up               = 'X'
    IMPORTING
      sel_variant          = f_variant
      sel_variant_text     = f_text
*   TABLES
*     BELONGING_DYNNR      = BELONGING_DYNNR
    EXCEPTIONS
      no_report            = 1
      report_not_existent  = 2
      report_not_supplied  = 3
      no_variants          = 4
      no_variant_selected  = 5
      variant_not_existent = 6
      OTHERS               = 7.

  IF sy-subrc EQ 0 .
    rsvar-variant = f_variant.
    rsvar-vtext   = f_text.

    "if variant was supplied then I read its content
    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = sy-repid
        variant              = f_variant
        move_or_write        = 'M'
*       NO_IMPORT            = ' '
*       EXECUTE_DIRECT       = ' '
*   IMPORTING
*       SP                   = SP
      TABLES
*       L_PARAMS             = L_PARAMS
*       L_PARAMS_NONV        = L_PARAMS_NONV
*       L_SELOP              = L_SELOP
*       L_SELOP_NONV         = L_SELOP_NONV
        valutab              = ft_params
*       OBJECTS              = OBJECTS
*       FREE_SELECTIONS_DESC = FREE_SELECTIONS_DESC
*       FREE_SELECTIONS_VALUE       = FREE_SELECTIONS_VALUE
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc EQ 0.
      "let's see what fields are on this subscreen
      CALL FUNCTION 'RS_ISOLATE_1_SELSCREEN'
        EXPORTING
          program     = sy-repid
          dynnr       = '0101'  "l_dynnr
*         TABIX       = 0
*   IMPORTING
*         LAST_TABIX  = LAST_TABIX
        TABLES
          screen_sscr = ft_sscr
*         GLOBAL_SSCR = GLOBAL_SSCR
        EXCEPTIONS
          no_objects  = 1
          OTHERS      = 2.
      IF sy-subrc EQ 0.
        SORT ft_sscr BY name ASCENDING.

        " clear current content of selection fields
        LOOP AT ft_params INTO fs_params.
          READ TABLE ft_sscr WITH KEY name = fs_params-selname BINARY SEARCH TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CONCATENATE '(' sy-repid ')' fs_params-selname INTO f_fieldname.
          CASE fs_params-kind.
            WHEN 'S'.
              CONCATENATE f_fieldname '[]' INTO f_fieldname.
              ASSIGN (f_fieldname) TO <any_selopt_itab>.
              IF sy-subrc EQ 0.
                REFRESH <any_selopt_itab>.
              ENDIF.
            WHEN 'P'.
              ASSIGN (f_fieldname) TO <any_field>.
              IF sy-subrc EQ 0.
                CLEAR <any_field>.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

        "add values from saved variant to selection fields
        LOOP AT ft_params INTO fs_params.
          READ TABLE ft_sscr WITH KEY name = fs_params-selname BINARY SEARCH TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CONCATENATE '(' sy-repid ')' fs_params-selname INTO f_fieldname.
          CASE fs_params-kind.
            WHEN 'S'. "select-options
              CONCATENATE f_fieldname '[]' INTO f_fieldname.
              ASSIGN (f_fieldname) TO <any_selopt_itab>.
              IF sy-subrc EQ 0.
                "firstly append initial line to be able to assign components
                APPEND INITIAL LINE TO <any_selopt_itab> ASSIGNING <any_selopt>.
                "now fill each component separately
                ASSIGN COMPONENT 'SIGN' OF STRUCTURE  <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-sign.
                ENDIF.

                ASSIGN COMPONENT 'OPTION' OF STRUCTURE  <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-option.
                ENDIF.

                ASSIGN COMPONENT 'LOW' OF STRUCTURE <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-low.
                ENDIF.

                ASSIGN COMPONENT 'HIGH' OF STRUCTURE <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-high.
                ENDIF.

                "just to be sure that select options are filled
                ASSIGN COMPONENT 'SIGN' OF STRUCTURE  <any_selopt> TO <any_field>.
                IF sy-subrc NE 0.
                  DELETE TABLE <any_selopt_itab> FROM <any_selopt>.
                ELSEIF <any_field> IS INITIAL.
                  DELETE TABLE <any_selopt_itab> FROM <any_selopt>.
                ENDIF.

              ENDIF.
            WHEN 'P'. "parameters
              ASSIGN (f_fieldname) TO <any_field>.
              IF sy-subrc EQ 0.
                CLEAR <any_field>.
                <any_field> = fs_params-low.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
**********************************************************************
