**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Rodrigo Carvalho ( rodrigo.sa@amaggi.com.br)                         |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Wellington.pereira ( wellington.pereira@amaggi.com.br )              |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Logs de Processamento                                                     |*
**/===========================================================================\*
REPORT zcor025.



**********************************************************************
* Tables
**********************************************************************
TABLES: t54c6, pernr, /tmf/d_lanc_cont, pa0001, pa0000,
        zcot0016, zcot0017, zcot0018.

TYPE-POOLS : slis.



**********************************************************************
* Types
**********************************************************************

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_zcot0016,
         data TYPE sy-datum, "'Data'
         hora TYPE sy-uzeit,
       END   OF ty_zcot0016,

       BEGIN OF ty_zcot0017,
         data TYPE sy-datum, "'Data'
         hora TYPE sy-uzeit,
       END   OF ty_zcot0017,

       BEGIN OF ty_zcot0018,
         data TYPE sy-datum, "'Data'
         hora TYPE sy-uzeit,
       END   OF ty_zcot0018,


       BEGIN OF ty_alv_saida,
         data             TYPE sy-datum, "'Data'
         hora             TYPE sy-uzeit,
         reg_int_intdg    TYPE i,        "'Registros integrados - BRG_CSTG_INTDG'
         reg_int_material TYPE i,        "'Registros integrados - BRG_OS_MATERIAL'
         reg_nao_int      TYPE i,        "'Registros não integrados'
       END   OF ty_alv_saida.



**********************************************************************
* Tabela interna
**********************************************************************
DATA: t_alv_saida  TYPE SORTED TABLE OF ty_alv_saida WITH UNIQUE KEY data hora,
      wa_alv_saida TYPE ty_alv_saida,
      t_saida      TYPE TABLE OF ty_alv_saida,
      wa_saida     TYPE ty_alv_saida,
      t_zcot0016   TYPE TABLE OF ty_zcot0016,
      wa_zcot0016  TYPE ty_zcot0016,
      t_zcot0017   TYPE TABLE OF ty_zcot0017,
      wa_zcot0017  TYPE ty_zcot0017,
      t_zcot0018   TYPE TABLE OF ty_zcot0018,
      wa_zcot0018  TYPE ty_zcot0018.

*------------------------------------
*---- ALV
*------------------------------------
DATA: dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      tl_function          TYPE ui_functions,
      wl_function          TYPE ui_func,
      "obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm.

DATA: lo_payroll TYPE REF TO cl_hr_br_read_payroll,
      zcl_util   TYPE REF TO zcl_util.

DATA: variante         LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.




SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_data  FOR zcot0016-data.
SELECTION-SCREEN END   OF BLOCK b2.



**********************************************************************
* classes / implementacoes
**********************************************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.


ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_hotspot_click.

    READ TABLE t_alv_saida INTO wa_alv_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      CASE e_column_id .
        WHEN 'reg_int_intdg'.
          PERFORM gera_alv_zcot0016.
        WHEN 'reg_int_material'.
          PERFORM gera_alv_zcot0017.
        WHEN 'reg_nao_int'.
          PERFORM gera_alv_zcot0018.
      ENDCASE.
    ENDIF.


  ENDMETHOD.

ENDCLASS.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.



**********************************************************************
* START
**********************************************************************
START-OF-SELECTION.


  PERFORM f_selecao_dados.

  PERFORM f_processa_dados.

  PERFORM f_exibir_dados.


*&---------------------------------------------------------------------*
*& Form f_selecao_dados
*&---------------------------------------------------------------------*
FORM f_selecao_dados .

  " Seleciona registros integrados INTDG
  SELECT data hora
         FROM zcot0016
         INTO TABLE t_zcot0016
         WHERE data  IN s_data.


  " Seleciona registros integrados Material
  SELECT data hora
         FROM zcot0017
         INTO TABLE t_zcot0017
         WHERE data  IN s_data.


  " Seleciona registros não integrados
  SELECT data hora
         FROM zcot0018
         INTO TABLE t_zcot0018
         WHERE data  IN s_data.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_dados
*&---------------------------------------------------------------------*
FORM f_processa_dados .

  SORT t_zcot0016 BY data.
  SORT t_zcot0017 BY data.
  SORT t_zcot0018 BY data.

  LOOP AT t_zcot0016 INTO wa_zcot0016.
    CLEAR: wa_alv_saida.
    wa_alv_saida-data          = wa_zcot0016-data.
    wa_alv_saida-hora          = wa_zcot0016-hora.
    wa_alv_saida-reg_int_intdg = 1.
    "AT END OF data.
    COLLECT wa_alv_saida INTO t_alv_saida.
      "CLEAR wa_alv_saida-reg_int_intdg.
    "ENDAT.
  ENDLOOP.

  LOOP AT t_zcot0017 INTO wa_zcot0017.
    CLEAR: wa_alv_saida.
    wa_alv_saida-data = wa_zcot0017-data.
    wa_alv_saida-hora = wa_zcot0017-hora.
    wa_alv_saida-reg_int_material = 1.
    "AT END OF data.
    COLLECT wa_alv_saida INTO t_alv_saida.
    "CLEAR wa_alv_saida-reg_int_material.
    "ENDAT.
  ENDLOOP.

  LOOP AT t_zcot0018 INTO wa_zcot0018.
    CLEAR: wa_alv_saida.
    wa_alv_saida-data = wa_zcot0018-data.
    wa_alv_saida-hora = wa_zcot0018-hora.
    wa_alv_saida-reg_nao_int = 1.
    "AT END OF data.
    COLLECT wa_alv_saida INTO t_alv_saida.
    "  CLEAR wa_alv_saida-reg_nao_int.
    "ENDAT.
  ENDLOOP.

  IF NOT t_alv_saida[] IS INITIAL.
    t_saida[] = t_alv_saida[].
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibir_dados
*&---------------------------------------------------------------------*
FORM f_exibir_dados .

  CALL SCREEN 100.

ENDFORM.

INCLUDE zcor025_status_0100o01.

INCLUDE zcor025_user_command_0100i01.
*&---------------------------------------------------------------------*
*& Form f_init_alv
*&---------------------------------------------------------------------*
FORM f_init_alv .

  DATA: wl_layout TYPE slis_layout_alv.
  DATA:
    p_text      TYPE sdydo_text_element,
    filtros	    TYPE zif_screen_linha_filtro,
    i_filtros	  TYPE zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) TYPE c,
    v_uzeit(10) TYPE c.


  PERFORM f_fieldcatalog.

  variante = VALUE #( report = sy-repid ).


  IF g_grid IS INITIAL.

    CLEAR: i_filtros.
    CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) INTO v_datum.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit.
    DESCRIBE TABLE t_alv_saida LINES DATA(v_lines).
    APPEND VALUE #( parametro = 'Data:' valor = v_datum ) TO i_filtros.
    APPEND VALUE #( parametro = 'Hora:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros.

  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
        i_titulo  = CONV #( p_text )
        i_filtros = i_filtros
      CHANGING
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.




    w_layout-sel_mode   = 'A'.
    "w_layout-ctab_fname = 'CELLCOLOR'.
    "w_layout-edit     = 'X'.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.
*
*    w_layout-info_fname   = 'LINE_COLOR'.
*    w_layout-ctab_fname   = 'COLOR_CELL'.
***    w_layout-zebra        = abap_false.

    "w_layout-stylefname   = 'CELLSTYLES'.

    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.

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

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      CHANGING
        it_outtab                     = t_saida[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


*    CALL METHOD g_grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CALL METHOD g_grid->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ELSE.
    CALL METHOD g_grid->refresh_table_display( is_stable = w_stable ).
  ENDIF.

  "wl_layout-colwidth_optimize = 'X'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fieldcatalog
*&---------------------------------------------------------------------*
FORM f_fieldcatalog .

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
  01  ''   ''   'T_ALV_SAIDA'   'data'                  'Data Processamento'                     '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  03  ''   ''   'T_ALV_SAIDA'   'hora'                  'Hora Processamento'                                   '10'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',
  04  ''   ''   'T_ALV_SAIDA'   'reg_int_intdg'         'Registros integrados - BRG_CSTG_INTDG'  '35'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',
  05  ''   ''   'T_ALV_SAIDA'   'reg_int_material'      'Registros integrados - BRG_OS_MATERIAL' '35'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',
  06  ''   ''   'T_ALV_SAIDA'   'reg_nao_int'           'Registros não integrados'               '30'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' '.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_estrutura_alv
*&---------------------------------------------------------------------*
FORM f_estrutura_alv  USING VALUE(p_col_pos)       TYPE i                    "1
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
                           VALUE(p_fix).                                    "17

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
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'L'.
  w_fieldcat-selddictxt  = 'L'.
  w_fieldcat-tipddictxt  = 'L'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.


  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form gera_alv_ZCOT0016
*&---------------------------------------------------------------------*
FORM gera_alv_zcot0016.


    SELECT *
           FROM zcot0016
           INTO TABLE @DATA(t_zcot0016_alv)
           WHERE data EQ @wa_alv_saida-data
             and hora eq @wa_alv_saida-hora.

  CHECK t_zcot0016_alv[] is NOT INITIAL.

  DATA: gd_layout           TYPE slis_layout_alv,
        t_fieldcat_zcot0016 TYPE slis_t_fieldcat_alv.

  gd_layout-info_fieldname = 'LINE_COLOR'.
  gd_layout-zebra          = 'X'.
  "gd_layout-box_fieldname  = 'ZFIGUID'.


  PERFORM f_preencher_fieldcat_16 CHANGING t_fieldcat_zcot0016.


  gd_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_user_command = 'F_FIELD_HOTSPOT'
*     i_callback_top_of_page  = 'F_TOP-OF-PAGE'
      is_layout          = gd_layout
      it_fieldcat        = t_fieldcat_zcot0016
      i_save             = 'A'
      i_html_height_top  = '20'
    TABLES
      t_outtab           = t_zcot0016_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form gera_alv_zcot0017
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gera_alv_zcot0017 .

 TYPES : BEGIN OF ty_zcot0017.
            INCLUDE STRUCTURE zcot0017.
           TYPES : rowcolor(4) TYPE c,
        END OF ty_zcot0017.


  DATA: gd_layout           TYPE slis_layout_alv,
        t_fieldcat_zcot0017 TYPE slis_t_fieldcat_alv,
        t_zcot0017_alv      TYPE table of ty_zcot0017.

  SELECT *
    FROM zcot0017 INTO CORRESPONDING FIELDS OF TABLE t_zcot0017_alv
   WHERE data EQ wa_alv_saida-data
     and hora eq wa_alv_saida-hora.

  CHECK t_zcot0017_alv[] is NOT INITIAL.

  LOOP AT t_zcot0017_alv ASSIGNING FIELD-SYMBOL(<fs_zcot0017_alv>).
*    IF <fs_zcot0017_alv>-menge is INITIAL or <fs_zcot0017_alv>-dmbtr is INITIAL.
*      <fs_zcot0017_alv>-rowcolor = 'C600'.
*    ENDIF.
  ENDLOOP.

  gd_layout-zebra          = 'X'.
  gd_layout-info_fieldname = 'ROWCOLOR'."For row coloring

  "gd_layout-box_fieldname  = 'ZFIGUID'.

  PERFORM f_preencher_fieldcat_17 CHANGING t_fieldcat_zcot0017.

  gd_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_user_command = 'F_FIELD_HOTSPOT'
*     i_callback_top_of_page  = 'F_TOP-OF-PAGE'
      is_layout          = gd_layout
      it_fieldcat        = t_fieldcat_zcot0017
      i_save             = 'A'
      i_html_height_top  = '20'
    TABLES
      t_outtab           = t_zcot0017_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form gera_alv_zcot0018
*&---------------------------------------------------------------------*
FORM gera_alv_zcot0018 .


  DATA: gd_layout           TYPE slis_layout_alv,
        t_fieldcat_zcot0018 TYPE slis_t_fieldcat_alv.


    SELECT *
           FROM zcot0018
           INTO TABLE @DATA(t_zcot0018_alv)
           WHERE data EQ @wa_alv_saida-data
             and hora eq @wa_alv_saida-hora.

  CHECK t_zcot0018_alv[] is NOT INITIAL.

  gd_layout-info_fieldname = 'LINE_COLOR'.
  gd_layout-zebra          = 'X'.
  "gd_layout-box_fieldname  = 'ZFIGUID'.

  gd_layout-colwidth_optimize = abap_true.

  PERFORM f_preencher_fieldcat_18 CHANGING t_fieldcat_zcot0018.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_user_command = 'F_FIELD_HOTSPOT'
*     i_callback_top_of_page  = 'F_TOP-OF-PAGE'
      is_layout          = gd_layout
      it_fieldcat        = t_fieldcat_zcot0018
      i_save             = 'A'
      i_html_height_top  = '20'
    TABLES
      t_outtab           = t_zcot0018_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preencher_fieldcat_16
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- T_FIELDCAT_ZCOT0016
*&---------------------------------------------------------------------*
FORM f_preencher_fieldcat_16  CHANGING p_lt_fcat TYPE slis_t_fieldcat_alv.

  DATA: w_fcat  TYPE  slis_fieldcat_alv.
  DATA: lv_col  TYPE  sy-cucol.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'OPERACAO'.
  w_fcat-seltext_l  = 'Operação empresarial'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'TP_DOC'.
  w_fcat-seltext_l  = 'Tipo de documento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'WRTTP'.
  w_fcat-seltext_l  = 'Categoria de Valor'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'BUDAT'.
  w_fcat-seltext_l  = 'Data de lançamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'HKONT'.
  w_fcat-seltext_l  = 'Classe de custo'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'KOSTL'.
  w_fcat-seltext_l  = 'Centro de custo'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'DMBTR'.
  w_fcat-seltext_l  = 'Valor total na moeda'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'DATA'.
  w_fcat-seltext_l  = 'Data Processamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'HORA'.
  w_fcat-seltext_l  = 'Hora Processamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'USUARIO'.
  w_fcat-seltext_l  = 'Usuário Processamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0016_alv'.
  APPEND w_fcat TO p_lt_fcat.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preencher_fieldcat_17
*&---------------------------------------------------------------------*
FORM f_preencher_fieldcat_17  CHANGING p_lt_fcat TYPE slis_t_fieldcat_alv.

  DATA: w_fcat  TYPE  slis_fieldcat_alv.
  DATA: lv_col  TYPE  sy-cucol.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'OPERACAO'.
  w_fcat-seltext_l  = 'Operação empresarial'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'TP_DOC'.
  w_fcat-seltext_l  = 'Tipo de documento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'WRTTP'.
  w_fcat-seltext_l  = 'Categoria de Valor'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'NO_OS_NUMBER'.
  w_fcat-seltext_l  = 'Nº ordem do parceiro'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'ERDAT'.
  w_fcat-seltext_l  = 'Data de lançamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'ERZET'.
  w_fcat-seltext_l  = 'Hora da entrada'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'DOCMAT'.
  w_fcat-seltext_l  = 'Chave composta'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'MATNR'.
  w_fcat-seltext_l  = 'Material'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'HKONT'.
  w_fcat-seltext_l  = 'Conta do Razão '.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'KOSTL'.
  w_fcat-seltext_l  = 'Centro de custo'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'MAKTX'.
  w_fcat-seltext_l  = 'Descrição da classe de custo'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'SHKZG'.
  w_fcat-seltext_l  = 'Código débito/crédito'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'MENGE'.
  w_fcat-seltext_l  = 'Quantidade'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'DMBTR'.
  w_fcat-seltext_l  = 'Montante em moeda interna'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'DMBE2'.
  w_fcat-seltext_l  = 'Montante na segunda moeda'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.


  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'DATA'.
  w_fcat-seltext_l  = 'Data Processamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'HORA'.
  w_fcat-seltext_l  = 'Hora Processamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'USUARIO'.
  w_fcat-seltext_l  = 'Usuário Processamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0017_alv'.
  APPEND w_fcat TO p_lt_fcat.





ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preencher_fieldcat_18
*&---------------------------------------------------------------------*
FORM f_preencher_fieldcat_18  CHANGING p_lt_fcat TYPE slis_t_fieldcat_alv.

  DATA: w_fcat  TYPE  slis_fieldcat_alv.
  DATA: lv_col  TYPE  sy-cucol.


  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'OPERACAO'.
  w_fcat-seltext_l  = 'Operação empresarial'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0018_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'TP_DOC'.
  w_fcat-seltext_l  = 'Tipo de documento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0018_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'WRTTP'.
  w_fcat-seltext_l  = 'Categoria de Valor'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0018_alv'.
  APPEND w_fcat TO p_lt_fcat.


  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'DATA'.
  w_fcat-seltext_l  = 'Data Processamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0018_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'HORA'.
  w_fcat-seltext_l  = 'Hora Processamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0018_alv'.
  APPEND w_fcat TO p_lt_fcat.

  CLEAR w_fcat.
  ADD 1 TO lv_col.
  w_fcat-col_pos    = lv_col.
  w_fcat-fieldname  = 'USUARIO'.
  w_fcat-seltext_l  = 'Usuário Processamento'.
  w_fcat-outputlen  = 14.
  w_fcat-fix_column = abap_true.
  w_fcat-tabname    = 't_zcot0018_alv'.
  APPEND w_fcat TO p_lt_fcat.



ENDFORM.
