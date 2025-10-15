*&---------------------------------------------------------------------*
*&  Include           ZMMR0044_CLASS
*&---------------------------------------------------------------------*

CLASS main DEFINITION.
  PUBLIC SECTION.

    DATA erro TYPE flag.

    METHODS:
      initial,
      free,
      free_class,
      carga_arq,
      grava_dados,
      show_log,
      exec_alv,
      alv_init,
      build_fieldcat,
      exclude_tb_functions,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.


ENDCLASS.

CLASS main IMPLEMENTATION.
  METHOD initial.
    me->free( ).
    me->carga_arq( ).
    IF me->erro EQ abap_false.
      me->show_log( ).
    ELSE.
      MESSAGE i000(z_mm) WITH text-t02.
    ENDIF.
  ENDMETHOD.
  METHOD free.
    CLEAR: git_dados[],git_zprovcoupa03.
  ENDMETHOD.
  METHOD free_class.
    CALL METHOD g_custom_container->free.
    CALL METHOD cl_gui_cfw=>flush.

    CLEAR: g_custom_container,
           g_grid.
  ENDMETHOD.
  METHOD carga_arq.

    DATA: lv_data TYPE string,
          lv_file TYPE rlgrap-filename.

*   Leitura arquivo
    CLEAR: lv_file, git_dados[].
    lv_file = p_arq.
    IF lv_file IS NOT INITIAL.

      CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
        EXPORTING
          filename                = lv_file
          i_begin_col             = 1
          i_begin_row             = 2
          i_end_col               = 11
          i_end_row               = 10000
        TABLES
          intern                  = git_dados
        EXCEPTIONS
          inconsistent_parameters = 1
          upload_ole              = 2
          OTHERS                  = 3.

      IF sy-subrc <> 0.
        MESSAGE i000(z_mm) WITH text-t01.
      ELSE.
        me->grava_dados( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD grava_dados.

    DATA: vl_werks TYPE n LENGTH 4.

    git_dados_aux[] = git_dados[].
    SORT git_dados_aux BY row col.

    LOOP AT git_dados INTO gwa_dados.
      IF gwa_dados-row = gwa_dados_aux-row.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO git_zprovcoupa03 ASSIGNING FIELD-SYMBOL(<fs_zprovcoupa03>).

      LOOP AT git_dados_aux INTO gwa_dados_aux WHERE row = gwa_dados-row.

        CASE gwa_dados_aux-col.
          WHEN 1.
            <fs_zprovcoupa03>-tp_estrat = gwa_dados_aux-value.
          WHEN 2.
            <fs_zprovcoupa03>-kostl     = gwa_dados_aux-value.
          WHEN 3.
            vl_werks                    = gwa_dados_aux-value.
          WHEN 4.
            <fs_zprovcoupa03>-nivel     = gwa_dados_aux-value.
          WHEN 5.
            <fs_zprovcoupa03>-ltext     = gwa_dados_aux-value.
          WHEN 6.
            <fs_zprovcoupa03>-name1     = gwa_dados_aux-value.
          WHEN 7.
            <fs_zprovcoupa03>-tp_oper   = gwa_dados_aux-value.
          WHEN 8.
            <fs_zprovcoupa03>-aprovador = gwa_dados_aux-value.
        ENDCASE.
      ENDLOOP.

      <fs_zprovcoupa03>-dt_atual  = sy-datum.
      <fs_zprovcoupa03>-hr_atual  = sy-uzeit.
      <fs_zprovcoupa03>-usnam     = sy-uname.
      <fs_zprovcoupa03>-werks     = vl_werks.

    ENDLOOP.

    IF git_zprovcoupa03[] IS NOT INITIAL.
      MODIFY zprovcoupa03 FROM TABLE git_zprovcoupa03.
      COMMIT WORK.
    ELSE.
      me->erro = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD show_log.
    CALL SCREEN '0200'.
  ENDMETHOD.
  METHOD exec_alv.
    me->alv_init( ).
    me->build_fieldcat( ).
    me->exclude_tb_functions( ).

    SET HANDLER: me->on_toolbar          FOR g_grid,
                 me->handle_user_command FOR g_grid.


    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = sg_layout
        it_toolbar_excluding = t_exclude
      CHANGING
        it_fieldcatalog      = t_fieldcat
        it_outtab            = git_zprovcoupa03[].

*   Set editable cells to ready for input initially
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ENDMETHOD.
  METHOD alv_init.
*   Se o ALV ja foi instanciado...
    IF g_grid IS BOUND.
      g_grid->free( ).
    ENDIF.

*   Se o Objeto não foi instaciando
    IF g_custom_container IS NOT BOUND.
      CREATE OBJECT g_custom_container
        EXPORTING
          container_name = 'C_ALV'.
    ENDIF.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = g_custom_container.
  ENDMETHOD.
  METHOD build_fieldcat.
    DATA ls_fcat TYPE lvc_s_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZPROVCOUPA03'
      CHANGING
        ct_fieldcat      = t_fieldcat.

    DELETE t_fieldcat WHERE fieldname EQ 'MANDT'.

***    LOOP AT t_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
***      CASE <fs_fieldcat>-fieldname.
***        WHEN 'PROJK'.
***          <fs_fieldcat>-convexit = abap_true.
***      ENDCASE.
***    ENDLOOP.

  ENDMETHOD.
  METHOD exclude_tb_functions.
    DATA ls_exclude TYPE ui_func.

    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND ls_exclude TO t_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND ls_exclude TO t_exclude.
  ENDMETHOD.
  METHOD on_toolbar.

    DATA: ty_toolbar           TYPE stb_button.

    ty_toolbar-icon      = icon_transport.
    ty_toolbar-function  = c_car.
    ty_toolbar-text      = 'Ir para ZMM0193'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

  ENDMETHOD.
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN c_car.
        CALL TRANSACTION 'ZMM0193'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
DATA gob_main TYPE REF TO main.
