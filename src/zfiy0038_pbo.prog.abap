*&---------------------------------------------------------------------*
*&  Include           ZFIY0038_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS  'PF0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  IF obj_alv_0100 IS INITIAL.

    CLEAR: gs_layout, gs_variant.

    PERFORM  f_criar_catalog USING '0100'.
    PERFORM  f_style_celltab USING '0100'.

    IF obj_container_0100 IS INITIAL.
      CREATE OBJECT obj_container_0100
        EXPORTING
          container_name = 'CC_ALV_0100'.
    ENDIF.

    CREATE OBJECT obj_alv_0100
      EXPORTING
        i_parent = obj_container_0100.


    CREATE OBJECT obj_toolbar_0100
      EXPORTING
        io_alv_grid = obj_alv_0100.


    gs_layout-sel_mode   = 'A'.
    gs_layout-info_fname = 'ROWCOLOR'.
    gs_layout-no_toolbar = abap_false.
    gs_layout-stylefname = 'CELLTAB'.

    gs_variant-report  = sy-repid.


    SET HANDLER: obj_toolbar_0100->on_toolbar          FOR obj_alv_0100,
                 obj_toolbar_0100->handle_user_command FOR obj_alv_0100,
                 obj_toolbar_0100->handle_data_changed FOR obj_alv_0100.


    PERFORM f_exclude_fcode USING '0100'.

    CALL METHOD obj_alv_0100->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = git_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = git_fcat
        it_outtab            = git_saida_0100.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CALL METHOD obj_alv_0100->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      CALL METHOD obj_alv_0100->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  DATA fcode TYPE TABLE OF sy-ucomm.
  CLEAR fcode.

  IF gva_type = 'ANULA'.
    APPEND 'SALVAR'     TO fcode.
    APPEND 'GER_AUTOMA' TO fcode.
    APPEND 'GER_MANUAL' TO fcode.

    SET PF-STATUS  'PF0101' EXCLUDING fcode.
    SET TITLEBAR   'T0101_C'.

  ELSE.
    SET PF-STATUS  'PF0101'.
    SET TITLEBAR   'T0101_A' WITH sy-datum.
  ENDIF.

  PERFORM F_GET_PICTURE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0101 OUTPUT.

  IF obj_alv_0101_cambio IS INITIAL.

    PERFORM f_criar_catalog USING '0101_CAMBIO'.

    IF obj_container_0101_cambio IS INITIAL.
      CREATE OBJECT obj_container_0101_cambio
        EXPORTING
          container_name = 'CC_ALV_0101_CAMBIO'.
    ENDIF.

    CREATE OBJECT obj_alv_0101_cambio
      EXPORTING
        i_parent = obj_container_0101_cambio.

    CREATE OBJECT obj_toolbar_0101_cambio
      EXPORTING
        io_alv_grid = obj_alv_0101_cambio.


    PERFORM f_exclude_fcode USING '0101_CAMBIO'.

    CALL METHOD obj_alv_0101_cambio->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = git_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = git_fcat
        it_outtab            = git_saida_0101_cambio.

  ELSE.
    CALL METHOD obj_alv_0101_cambio->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.
  ENDIF.

  IF obj_alv_0101_permisso IS INITIAL.

    PERFORM f_criar_catalog USING '0101_PERMISSO'.

    IF obj_container_0101_permisso IS INITIAL.
      CREATE OBJECT obj_container_0101_permisso
        EXPORTING
          container_name = 'CC_ALV_0101_PERMISSO'.
    ENDIF.

    CREATE OBJECT obj_alv_0101_permisso
      EXPORTING
        i_parent = obj_container_0101_permisso.

    CREATE OBJECT obj_toolbar_0101_permisso
      EXPORTING
        io_alv_grid = obj_alv_0101_permisso.

    CALL METHOD obj_alv_0101_permisso->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = git_exclude_fcode_0101_perm
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = git_fcat
        it_outtab            = git_saida_0101_permisso.
  ELSE.
    CALL METHOD obj_alv_0101_permisso->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR  'T0300'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
  IF obj_alv_0200 IS INITIAL.

    CLEAR: gs_layout, gs_variant.

    DATA: lit_f4 TYPE lvc_t_f4,
          lwa_f4 TYPE lvc_s_f4.


    PERFORM f_criar_catalog USING '0200'.
    PERFORM f_style_celltab USING '0200'.
    PERFORM f_exclude_fcode USING '0200'.


    IF obj_container_0200 IS INITIAL.
      CREATE OBJECT obj_container_0200
        EXPORTING
          container_name = 'CC_ALV_0200'.
    ENDIF.

    CREATE OBJECT obj_alv_0200
      EXPORTING
        i_parent = obj_container_0200.


    CREATE OBJECT obj_toolbar_0200
      EXPORTING
        io_alv_grid = obj_alv_0200.


    gs_layout-sel_mode   = 'A'.
    gs_layout-info_fname = 'ROWCOLOR'.
    gs_layout-no_toolbar = abap_false.
    gs_layout-stylefname = 'CELLTAB'.

    gs_variant-report  = sy-repid.


    CLEAR: lwa_f4, lit_f4 .
    lwa_f4-fieldname  = 'COD_FECHTO'.
    lwa_f4-register   = 'X'.
    APPEND lwa_f4 TO lit_f4.

    CALL METHOD obj_alv_0200->register_f4_for_fields
      EXPORTING
        it_f4 = lit_f4.


    SET HANDLER: obj_toolbar_0200->on_toolbar           FOR obj_alv_0200,
                 obj_toolbar_0200->handle_user_command  FOR obj_alv_0200,
                 obj_toolbar_0200->handle_data_changed  FOR obj_alv_0200,
                 obj_toolbar_0200->on_f4                FOR obj_alv_0200.


    CALL METHOD obj_alv_0200->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        it_toolbar_excluding          = git_exclude_fcode_0200
      CHANGING
        it_fieldcatalog               = git_fcat
        it_outtab                     = git_saida_0200
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CALL METHOD obj_alv_0200->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      CALL METHOD obj_alv_0200->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'PF0300'.
  SET TITLEBAR  'T0300'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  IF obj_alv_0300 IS INITIAL.

    PERFORM f_criar_catalog USING '0300'.
    PERFORM f_exclude_fcode USING '0300'.

    IF obj_container_0300 IS INITIAL.
      CREATE OBJECT obj_container_0300
        EXPORTING
          container_name = 'CC_ALV_0300'.
    ENDIF.

    CREATE OBJECT obj_alv_0300
      EXPORTING
        i_parent = obj_container_0300.


    CALL METHOD obj_alv_0300->set_table_for_first_display
      EXPORTING
        is_layout                     = git_layout
        it_toolbar_excluding          = git_exclude_fcode_0300
      CHANGING
        it_fieldcatalog               = git_fcat
        it_outtab                     = git_saida_0300
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDMODULE.
