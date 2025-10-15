*----------------------------------------------------------------------*
***INCLUDE LZGFS_INSUMOSF03.
*----------------------------------------------------------------------*

**********************************************************************
* selecao
**********************************************************************
FORM f_selecao_log  USING  p_id_doc_agrupador.

  FREE: t_log.

  SELECT *
    FROM zsdt0313
    INTO TABLE t_zsdt0313
   WHERE id_doc_agrupador = p_id_doc_agrupador.

  CHECK t_zsdt0313[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0310
    INTO TABLE t_zsdt0310
   WHERE id_doc_agrupador = p_id_doc_agrupador.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZTIPO_DOC'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = t_tipodoc
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  SORT t_zsdt0313 BY id_seq DESCENDING
                     id_log DESCENDING.

  LOOP AT t_zsdt0313 INTO w_zsdt0313.

    CLEAR: w_log,  w_zsdt0310.

    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_doc_agrupador = w_zsdt0313-id_doc_agrupador.
    READ TABLE t_tipodoc  INTO w_tipodoc  WITH KEY domvalue_l       = w_zsdt0310-tipo_doc.

    w_log-status     = COND #( WHEN w_zsdt0313-tipo_msg = 'E' THEN icon_led_red
                                                              ELSE icon_led_green ).
    w_log-id_seq     = w_zsdt0313-id_seq.
    w_log-id_log     = w_zsdt0313-id_log.
    w_log-nr_venda   = w_zsdt0310-nr_venda.
    w_log-tpdoc      = w_zsdt0310-tipo_doc.
    w_log-tipo_doc   = w_tipodoc-ddtext.
    w_log-mensagem   = w_zsdt0313-mensagem.
    w_log-usname     = w_zsdt0313-usname.
    w_log-data       = w_zsdt0313-data.
    w_log-hora       = w_zsdt0313-hora.

    APPEND w_log    TO t_log.
  ENDLOOP.

* SORT t_log BY data DESCENDING
*               hora DESCENDING.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv_log.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog_log.
* PERFORM f_sort.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-no_rowmark   = abap_true.
  w_layout-zebra        = abap_true.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
* w_layout-stylefname   = 'CELLSTYLES'.
  w_layout-ctab_fname   = 'CELLCOLOR'.

  IF g_grid IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
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
        it_outtab                     = t_log[]
*       it_sort                       = t_sort[]
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

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog_log.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
    01  ''      ''       'T_LOG'   'STATUS'              'Status'                   '06'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' 'X',
    02  ''      ''       'T_LOG'   'NR_VENDA'            'Doc.Simulação'            '14'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    03  ''      ''       'T_LOG'   'TIPO_DOC'            'Tipo Documento'           '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    04  ''      ''       'T_LOG'   'MENSAGEM'            'Mensagem'                 '70'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
    05  ''      ''       'T_LOG'   'USNAME'              'Usuário Registro'         '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    06  ''      ''       'T_LOG'   'DATA'                'Data Registro'            '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    07  ''      ''       'T_LOG'   'HORA'                'Hora Registro'            '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  FREE ok_code.

  SET PF-STATUS 'INSUMOSF01'.
  SET TITLEBAR 'INSUMOSF03'.

  PERFORM f_init_alv_log.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

*---------------------------------------------------------------------*
*      Module  USER_COMMAND_0100  INPUT
*---------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE ok_code.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'VOLTAR'.
      CALL METHOD g_custom_container->free.
      FREE: g_grid.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.
