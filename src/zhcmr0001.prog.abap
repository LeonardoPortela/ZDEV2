*&--------------------------------------------------------------------&*
*&                     REPORT ZHCMR0001                               &*
*&--------------------------------------------------------------------&*

REPORT zhcmr0001.

TABLES: t799battrib03c, t799battrib06c.

TYPES: BEGIN OF ty_saida,
         molga            TYPE molga,
         hr_attrib_group  TYPE p_99b_hrattrib_group,
         hr_attribute     TYPE p_99b_hrattribute,
         hr_attribute_txt TYPE p_99b_hrattribute_txt,
         endda            TYPE endda,
         begda            TYPE begda,
         hr_cvalue        TYPE p_99b_hrattribute_cvalue,
       END OF ty_saida.


*Parametros conteiner 01
DATA:
  t_saida         TYPE TABLE OF ty_saida,
  w_saida         TYPE ty_saida,
  it_msg_return   TYPE TABLE OF zfiwrs0002,
  wa_mensagem     TYPE char30,
  wa_msg_return   TYPE zfiwrs0002,
  wa_cont         TYPE REF TO cl_gui_custom_container,
  wa_alv          TYPE REF TO cl_gui_alv_grid,
  wa_layout       TYPE lvc_s_layo,
  wa_fcat         TYPE lvc_s_fcat,
  it_select_rows  TYPE lvc_t_row,
  it_fcat         TYPE TABLE OF lvc_s_fcat,
  t_fcat          TYPE TABLE OF lvc_s_fcat,
  obj_custom_0110 TYPE REF TO cl_gui_custom_container,
  obj_alv_0110    TYPE REF TO cl_gui_alv_grid,
  gt_exc_button   TYPE ui_functions,
  wa_stable       TYPE lvc_s_stbl,
  it_p_rows       TYPE lvc_t_row,
  wa_select_rows  TYPE lvc_s_row,
  wa_p_rows       TYPE lvc_s_row,
  clicks          TYPE sy-tabix,
  tg_selectedcell TYPE lvc_t_cell,
  wg_selectedcell TYPE lvc_s_cell.

*Parametros conteiner 02
DATA:
  t_t799battrib03c TYPE TABLE OF t799battrib03c,
  t_t799battrib06c TYPE TABLE OF t799battrib06c,
  t_t799battrib01t TYPE TABLE OF t799battrib01t,
  w_t799battrib06c TYPE t799battrib06c,
  w_t799battrib03c TYPE t799battrib03c,
  w_t799battrib01t TYPE t799battrib01t,
  wa_cont_2        TYPE REF TO cl_gui_custom_container,
  wa_alv_2         TYPE REF TO cl_gui_alv_grid,
  wa_layout_2      TYPE lvc_s_layo,
  wa_fcat_2        TYPE lvc_s_fcat,
  it_select_rows_2 TYPE lvc_t_row,
  it_fcat_2        TYPE TABLE OF lvc_s_fcat,
  it_p_rows_2      TYPE lvc_t_row,
  wa_select_rows_2 TYPE lvc_s_row,
  wa_p_rows_2      TYPE lvc_s_row,
  clicks_2         TYPE sy-tabix,
  lt_f4            TYPE lvc_t_f4 WITH HEADER LINE,
  wa_stable_2      TYPE lvc_s_stbl,
  tl_function      TYPE ui_functions,
  wl_function      LIKE tl_function WITH HEADER LINE,
  w_fieldcatalog   TYPE lvc_s_fcat.

DATA:wa_toolbar    TYPE stb_button.
DATA:w_toolbar    TYPE stb_button.
*===============================================


*===============================================

CLASS events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.

    CLASS-METHODS:
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.

CLASS events IMPLEMENTATION.

  METHOD on_toolbar.

    CLEAR w_toolbar.
    w_toolbar-function     = 'BTN_ADD'.
    w_toolbar-icon         =  icon_insert_row.
    w_toolbar-quickinfo    = 'Adicionar'.
    w_toolbar-butn_type    = 0.
    w_toolbar-text         = 'Adicionar'.
    APPEND w_toolbar TO e_object->mt_toolbar.

    CLEAR w_toolbar.
    w_toolbar-function     = 'BTN_DELETE'.
    w_toolbar-icon         =  icon_delete_row.
    w_toolbar-quickinfo    = 'Delete'.
    w_toolbar-butn_type    = 0.
    w_toolbar-text         = 'Delete'.
    APPEND w_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.


  METHOD on_data_changed.

  ENDMETHOD.

  METHOD on_data_changed_finished.


  ENDMETHOD.

  METHOD on_onf4.

  ENDMETHOD.

  METHOD handle_user_command.

    DATA: w_saida TYPE zpmt0012.
    CASE e_ucomm.
      WHEN 'BTN_ADD'.

      WHEN 'BTN_DELETE'.


    ENDCASE.

  ENDMETHOD.
ENDCLASS.


CLASS event DEFINITION.
  PUBLIC SECTION.


    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.



CLASS event IMPLEMENTATION.
*  METHOD CONSTRUCTOR.
**   Create ALV toolbar manager instance
*    CREATE OBJECT C_ALV_TOOLBARMANAGER
*      EXPORTING
*        IO_ALV_GRID = IO_ALV_GRID.
*  ENDMETHOD.                    "constructor

  METHOD on_double_click.

  ENDMETHOD.



  METHOD set_toolbar.
    CLEAR wa_toolbar.
    wa_toolbar-function     = 'BTN_NOVO'.
    wa_toolbar-icon         =  icon_create.
    wa_toolbar-quickinfo    = 'NOVO'.
    wa_toolbar-butn_type    = 0.
    wa_toolbar-text         = 'Novo'.
    APPEND wa_toolbar TO e_object->mt_toolbar.

    CLEAR wa_toolbar.
    wa_toolbar-function     = 'BTN_DEL'.
    wa_toolbar-icon         =  icon_delete_row.
    wa_toolbar-quickinfo    = 'EXCLUIR'.
    wa_toolbar-butn_type    = 0.
    wa_toolbar-text         = 'Excluir'.
    APPEND wa_toolbar TO e_object->mt_toolbar.

    CLEAR wa_toolbar.
    wa_toolbar-function     = 'BTN_MODIF'.
    wa_toolbar-icon         =  icon_change.
    wa_toolbar-quickinfo    = 'MODIFICAR'.
    wa_toolbar-butn_type    = 0.
    wa_toolbar-text         = 'Modificar'.
    APPEND wa_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.

*  METHOD HANDLE_DATA_CHANGED.
**    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
*  ENDMETHOD.

  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'BTN_NOVO'.

        PERFORM fm_atributer.


      WHEN 'BTN_DEL'.

        DATA: p_resp,
           lv_msg TYPE bapi_msg.

        CLEAR: it_select_rows[], wa_select_rows.

        CALL METHOD wa_alv->get_selected_rows
          IMPORTING
            et_index_rows = it_select_rows.

        IF it_select_rows[] IS NOT INITIAL.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING        "TITLEBAR = 'Confirmar'
              text_question         = 'Deseja realmente excluir a linha?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              display_cancel_button = ' '
            IMPORTING
              answer                = p_resp.

          IF p_resp = 1.

            LOOP AT it_select_rows INTO wa_select_rows.

              READ TABLE t_t799battrib03c INTO DATA(wa_s_report) INDEX wa_select_rows-index.

              DELETE FROM t799battrib03c
              WHERE molga = wa_s_report-molga
              AND hr_attrib_group =  wa_s_report-hr_attrib_group
              AND hr_attribute    =  wa_s_report-hr_attribute.

            ENDLOOP.

            IF sy-subrc = 0.

              MESSAGE |Informações excluida com sucesso!| TYPE 'I' DISPLAY LIKE 'S'.
              FREE: t_t799battrib03c.
              SELECT *
              FROM t799battrib03c
              INTO TABLE t_t799battrib03c
                WHERE hr_attrib_group EQ w_t799battrib03c-hr_attrib_group
                  AND molga EQ w_t799battrib03c-molga.

              CALL METHOD wa_alv->refresh_table_display.

            ENDIF.
          ENDIF.

        ELSE.
          MESSAGE i836(sd) WITH text-004.
        ENDIF.

      WHEN 'BTN_MODIF'.

        DATA: q_linha TYPE char1 .

        CLEAR: lv_msg, p_resp, it_select_rows[], wa_select_rows, q_linha.
        FREE: t_saida.

        CALL METHOD wa_alv->get_selected_rows
          IMPORTING
            et_index_rows = it_select_rows.

        IF it_select_rows[] IS NOT INITIAL.

*          LOOP AT it_select_rows INTO DATA(_linha).
*            ADD 1 TO q_linha.
*          ENDLOOP.
*
*          IF q_linha EQ 1.

          LOOP AT it_select_rows INTO wa_select_rows.
            READ TABLE t_t799battrib03c INTO DATA(wa_report) INDEX wa_select_rows-index.

            SELECT SINGLE *
            FROM t799battrib03c
            INTO w_t799battrib03c
            WHERE molga EQ wa_report-molga
             AND hr_attrib_group EQ wa_report-hr_attrib_group
             AND hr_attribute    EQ wa_report-hr_attribute.

            IF w_t799battrib03c IS NOT INITIAL.
              MOVE-CORRESPONDING w_t799battrib03c TO w_saida.
              SELECT SINGLE hr_attribute_txt FROM t799battrib01t INTO w_saida-hr_attribute_txt WHERE spras EQ sy-langu AND hr_attribute EQ w_t799battrib03c-hr_attribute.
              w_t799battrib03c-hr_attrib_group = wa_report-hr_attrib_group.
              w_t799battrib03c-molga = wa_report-molga.
              APPEND w_saida TO t_saida.
            ENDIF.
            CLEAR: w_saida.
          ENDLOOP.

          CALL SCREEN 0200 STARTING AT 8 8.

*          ELSE.
*            MESSAGE |Selecionar somente a informação que sera alterada!| TYPE 'I' DISPLAY LIKE 'S'.
*          ENDIF.
        ELSE.
          MESSAGE |É necessario selecionar a informação que sera alterada!| TYPE 'I' DISPLAY LIKE 'E'.

        ENDIF.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


INITIALIZATION.

START-OF-SELECTION.

  PERFORM sel_dados.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'TL001'.
  SET TITLEBAR 'SET_001'.

  IF wa_cont IS INITIAL.
    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    PERFORM z_fieldcat.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    DATA(obj_even) = NEW event( ).
    SET HANDLER:obj_even->on_double_click  FOR wa_alv.
    SET HANDLER:obj_even->set_toolbar      FOR wa_alv.
    SET HANDLER:obj_even->get_ucomm        FOR wa_alv.


    CALL METHOD wa_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
      CHANGING
        it_outtab                     = t_t799battrib03c
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc NE 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CALL METHOD wa_alv->refresh_table_display.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'TL002'.
  SET TITLEBAR 'TI002'.

  PERFORM fm_create_object.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CALL METHOD obj_alv_0110->check_changed_data.
  CASE sy-ucomm.
    WHEN 'EXIT'.
*      CLEAR: gt_usuario.
*      CLEAR: w_t799battrib03c.
      FREE: t_saida.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.

      PERFORM fm_save.
*      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SEL_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sel_dados.

*  CLEAR: t_t799battrib03c.
*  SELECT *
*  FROM t799battrib03c
*  INTO TABLE t_t799battrib03c.


  CALL SCREEN 0300.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_fieldcat .

  PERFORM z_feed_fieldcat USING:
       1  'MOLGA              '  'T_T799BATTRIB03C'  '05'   ' '  ' '  ' '  'Agrupamento de países              '  ''  ' '  ' '  ' ',
       2  'HR_ATTRIB_GROUP    '  'T_T799BATTRIB03C'  '16'   ' '  ' '  ' '  'Agrupamento de atributos HR        '  ''  ' '  ' '  ' ',
       3  'HR_ATTRIBUTE       '  'T_T799BATTRIB03C'  '16'   ' '  ' '  ' '  'Atributo HR                        '  ''  ' '  ' '  ' ',
       4  'ENDDA              '  'T_T799BATTRIB03C'  '10'   ' '  ' '  ' '  'Fim da validade                    '  ''  ' '  ' '  ' ',
       5  'BEGDA              '  'T_T799BATTRIB03C'  '10'   ' '  ' '  ' '  'Início da validade                 '  ''  ' '  ' '  ' ',
       6  'HR_CVALUE          '  'T_T799BATTRIB03C'  '40'   ' '  ' '  ' '  'Valor ajustável para atributo HR   '  ''  ' '  ' '  ' '.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_FEED_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0412   text
*      -->P_0413   text
*      -->P_0414   text
*      -->P_0415   text
*      -->P_0416   text
*      -->P_0417   text
*      -->P_0418   text
*      -->P_0419   text
*      -->P_0420   text
*      -->P_0421   text
*      -->P_0422   text
*----------------------------------------------------------------------*
FORM z_feed_fieldcat  USING       VALUE(p_colnum)
                                  VALUE(p_fieldname)
                                  VALUE(p_tabname)
                                  VALUE(p_len)
                                  VALUE(p_edit)
                                  VALUE(p_icon)
                                  VALUE(p_do_sum)
                                  VALUE(p_header)
                                  VALUE(p_emphasize)
                                  VALUE(p_hotspot)
                                  VALUE(p_ref_table)
                                  VALUE(p_ref_field).


  wa_fcat-col_pos     = p_colnum.
  wa_fcat-fieldname   = p_fieldname.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-outputlen   = p_len.
  wa_fcat-edit        = p_edit.
  wa_fcat-icon        = p_icon.
  wa_fcat-do_sum      = p_do_sum.
  wa_fcat-coltext     = p_header.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-ref_table   = p_ref_table.
  wa_fcat-ref_table   = p_ref_field.

  wa_layout-excp_conds    = 'X'.
  wa_layout-zebra         = 'X'.
  wa_layout-sel_mode      = 'A'.
  wa_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  wa_layout-totals_bef    = ' '.

  APPEND wa_fcat TO it_fcat.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  SET PF-STATUS 'TL003'.
  SET TITLEBAR 'ST_003'.

*  IF wa_cont_2 IS INITIAL.
*    CREATE OBJECT wa_cont_2
*      EXPORTING
*        container_name              = 'CONTAINER_2'
*      EXCEPTIONS
*        cntl_error                  = 1
*        cntl_system_error           = 2
*        create_error                = 3
*        lifetime_error              = 4
*        lifetime_dynpro_dynpro_link = 5
*        OTHERS                      = 6.
*
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
*    APPEND wl_function TO tl_function.
**    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
**    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_check.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
*    APPEND wl_function TO tl_function.
*
*    PERFORM z_fieldcat_2.
*
*    CREATE OBJECT wa_alv_2
*      EXPORTING
*        i_parent          = wa_cont_2
*      EXCEPTIONS
*        error_cntl_create = 1
*        error_cntl_init   = 2
*        error_cntl_link   = 3
*        error_dp_create   = 4
*        OTHERS            = 5.
*
*    DATA(obj_events) = NEW events( ).
*    SET HANDLER:
*    obj_events->handle_user_command      FOR wa_alv_2,
*    obj_events->on_toolbar              FOR wa_alv_2,
*    obj_events->on_data_changed_finished FOR wa_alv_2,
*    obj_events->on_data_changed          FOR wa_alv_2,
*    obj_events->on_onf4                  FOR wa_alv_2.
*
*
*    CALL METHOD wa_alv_2->set_table_for_first_display
*      EXPORTING
*        it_toolbar_excluding          = tl_function
*        is_layout                     = wa_layout_2
*      CHANGING
*        it_outtab                     = gt_permit
*        it_fieldcatalog               = it_fcat_2
*      EXCEPTIONS
*        invalid_parameter_combination = 1
*        program_error                 = 2
*        too_many_lines                = 3
*        OTHERS                        = 4.
*
*    CALL METHOD wa_alv_2->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CALL METHOD wa_alv_2->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*    CALL METHOD wa_alv_2->register_f4_for_fields
*      EXPORTING
*        it_f4 = lt_f4[].
*
*
*    IF sy-subrc NE 0 .
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ELSE.
*    CALL METHOD wa_alv_2->refresh_table_display
*      EXPORTING
*        is_stable = wa_stable_2.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'EXEC'.
      PERFORM fm_check_atributer.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SELEC_USUARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selec_usuario .



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CHECK_ATRIBUTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_check_atributer .

  DATA: zhr_attrib     TYPE p_99b_hrattrib_group,
        r_hr_attribute TYPE RANGE OF p_99b_hrattribute.

  "Verifica se usuario informou o atributo.
  IF w_t799battrib06c IS NOT INITIAL.
    zhr_attrib = w_t799battrib06c-hr_attrib_group.

    "Verifica se exite o atributo cadastrado.
    CLEAR: w_t799battrib03c.
    SELECT SINGLE * FROM t799battrib06c
       INTO w_t799battrib03c
      WHERE hr_attrib_group EQ w_t799battrib06c-hr_attrib_group.
*      AND molga EQ '37'.

    IF w_t799battrib03c IS NOT INITIAL.

      "Selecionar informações.
      FREE: t_t799battrib03c.
      SELECT *
      FROM t799battrib03c
      INTO TABLE t_t799battrib03c
        WHERE hr_attrib_group EQ w_t799battrib06c-hr_attrib_group
        AND molga EQ '37'.

      CALL SCREEN 0100.
*      ENDIF.
    ELSE.
*      MESSAGE |Entrada { w_t799battrib06c-hr_attrib_group } não cadastrado.| TYPE 'I' DISPLAY LIKE 'E'.
      MESSAGE i836(sd) WITH text-002 w_t799battrib06c-hr_attrib_group text-005.

    ENDIF.


  ELSE.
    MESSAGE text-001 TYPE 'I'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CREATE_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_create_object .

  PERFORM fm_preenche_fcat.


  IF ( obj_custom_0110 IS INITIAL ).
    CREATE OBJECT obj_custom_0110
      EXPORTING
        container_name              = 'CONTAINER_01'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0110
      EXPORTING
        i_parent          = obj_custom_0110
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

*  wa_layout-cwidth_opt = 'X'.
  LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    IF <ls_fcat>-fieldname EQ 'HR_CVALUE'.
      <ls_fcat>-edit = abap_true.
    ENDIF.
  ENDLOOP.

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

  CALL METHOD obj_alv_0110->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = tl_function
      i_save                        = 'A'
    CHANGING
      it_fieldcatalog               = t_fcat
      it_outtab                     = t_saida
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0110->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0110->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_PREENCHE_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_preenche_fcat .

  REFRESH t_fcat.
  PERFORM alv_preenche_cat USING:
     'HR_ATTRIBUTE     ' 'Atributo HR                    ' '16'  ''  ''  ''  '' ''  '' '' '' '',
     'HR_ATTRIBUTE_TXT ' 'Descrição do atributo HR       ' '40'  ''  ''  ''  '' ''  '' '' '' '',
     'BEGDA            ' 'Início da validade             ' '08'  ''  ''  ''  '' ''  '' '' '' '',
     'ENDDA            ' 'Fim da validade                ' '08'  ''  ''  ''  '' ''  '' '' '' '',
     'HR_CVALUE        ' 'Valor atributo HR              ' '40'  ''  ''  ''  '' ''  '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1373   text
*      -->P_1374   text
*      -->P_1375   text
*      -->P_1376   text
*      -->P_1377   text
*      -->P_1378   text
*      -->P_1379   text
*      -->P_1380   text
*      -->P_1381   text
*      -->P_1382   text
*      -->P_1383   text
*      -->P_1384   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat    USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.
  CLEAR: wa_layout, wl_fcat.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-edit      = p_edit.
  wl_fcat-checkbox  = p_check.
  wl_fcat-ref_table = p_ref_tabname.
  wl_fcat-ref_field = p_ref_fieldname.
  wl_fcat-tabname   = p_ref_tabname.
  wl_fcat-no_out    = p_no_out.
  APPEND wl_fcat TO t_fcat.
ENDFORM.                    "ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  FM_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_save .
  DATA: it_t799battrib03c TYPE TABLE OF t799battrib03c.
  FREE: it_msg_return, it_t799battrib03c.
  IF t_saida IS NOT INITIAL.
    LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<ws_saida>).
      CASE <ws_saida>-hr_attribute.
        WHEN 'AT_CITY_CODE'.
          IF <ws_saida>-hr_cvalue IS INITIAL. APPEND  VALUE #( msg = |Preencha o código do município | ) TO it_msg_return. ENDIF.
        WHEN 'AT_CNPJ'.
          IF <ws_saida>-hr_cvalue IS INITIAL. APPEND  VALUE #( msg = |Preencha o CNPJ | ) TO it_msg_return. ENDIF.
        WHEN 'AT_COMP_NAME'.
          IF <ws_saida>-hr_cvalue IS INITIAL. APPEND  VALUE #( msg = |Preencha nome do contribuinte ou Razão social | ) TO it_msg_return. ENDIF.
        WHEN 'AT_DISTRICT'.
          IF <ws_saida>-hr_cvalue IS INITIAL. APPEND  VALUE #( msg = |Preencha nome do bairro/distrito | ) TO it_msg_return. ENDIF.
        WHEN 'AT_NUMBER'.
          IF <ws_saida>-hr_cvalue IS INITIAL. APPEND  VALUE #( msg = |Preencha o número do logradouro | ) TO it_msg_return. ENDIF.
        WHEN 'AT_STATE'.
          IF <ws_saida>-hr_cvalue IS INITIAL. APPEND  VALUE #( msg = |Preencha a unidade da Federação | ) TO it_msg_return. ENDIF.
        WHEN 'AT_STREET'.
          IF <ws_saida>-hr_cvalue IS INITIAL. APPEND  VALUE #( msg = |Preencha a descrição do logradouro | ) TO it_msg_return. ENDIF.
        WHEN 'AT_ZIP_CODE'.
          IF <ws_saida>-hr_cvalue IS INITIAL. APPEND  VALUE #( msg = |Preencha o código de Endereçamento Postal - CEP | ) TO it_msg_return. ENDIF.
        WHEN 'AT_INSC_TYPE'.
          IF <ws_saida>-hr_cvalue IS INITIAL. APPEND  VALUE #( msg = |Preencha o tipo de inscrição | ) TO it_msg_return. ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    IF it_msg_return IS NOT INITIAL.
      MESSAGE text-003 TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      MOVE-CORRESPONDING t_saida TO it_t799battrib03c.
      MODIFY t799battrib03c FROM TABLE it_t799battrib03c.
      COMMIT WORK.

      IF sy-subrc EQ 0.
*      PERFORM sel_dados.

        FREE: t_t799battrib03c.
        SELECT *
        FROM t799battrib03c
        INTO TABLE t_t799battrib03c
          WHERE hr_attrib_group EQ w_t799battrib03c-hr_attrib_group
            AND molga EQ w_t799battrib03c-molga.


        CALL METHOD wa_alv->refresh_table_display.
        LEAVE TO SCREEN 0.

        FREE: t_saida.
*        CLEAR: w_t799battrib03c, w_t799battrib06c.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ATRIBUTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_atributer .
  DATA: ws_t799battrib06c TYPE t799battrib06c,
        zhr_attrib        TYPE p_99b_hrattrib_group,
        r_hr_attribute    TYPE RANGE OF p_99b_hrattribute.
  FREE: r_hr_attribute.

  SELECT SINGLE * FROM t799battrib06c INTO ws_t799battrib06c WHERE hr_attrib_group EQ w_t799battrib06c-hr_attrib_group.

  CASE ws_t799battrib06c-hr_attrib_class.
    WHEN 'CL_EDUCATION_ORG'.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_CNPJ' ) TO r_hr_attribute.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_INSC_TYPE' ) TO r_hr_attribute.

    WHEN 'CL_EDUC_INST'.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_CITY_CODE' ) TO r_hr_attribute.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_CNPJ' ) TO r_hr_attribute.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_COMP_NAME' ) TO r_hr_attribute.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_DISTRICT' ) TO r_hr_attribute.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_NUMBER' ) TO r_hr_attribute.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_STATE' ) TO r_hr_attribute.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_STREET' ) TO r_hr_attribute.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = 'AT_ZIP_CODE' ) TO r_hr_attribute.
    WHEN OTHERS.
  ENDCASE.

  "Seleciona texto atributos.
  FREE: t_t799battrib01t.
  CHECK r_hr_attribute IS NOT INITIAL.
  SELECT * FROM t799battrib01t INTO TABLE t_t799battrib01t
    WHERE hr_attribute IN r_hr_attribute
    AND molga EQ w_t799battrib03c-molga
    AND spras EQ sy-langu.

  IF t_t799battrib01t IS NOT INITIAL.
    t_saida = VALUE #( FOR l IN t_t799battrib01t ( molga = w_t799battrib03c-molga
                                         hr_attrib_group = w_t799battrib03c-hr_attrib_group
                                            hr_attribute = l-hr_attribute
                                        hr_attribute_txt = l-hr_attribute_txt
                                                   endda = '99991231'
                                                   begda = '19000101'
                                               hr_cvalue = ''

    ) ).

    CALL SCREEN 0200 STARTING AT 8 8.
  ENDIF.
ENDFORM.
