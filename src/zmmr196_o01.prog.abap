*&---------------------------------------------------------------------*
*& Include          ZMMR196_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'STATUS_0001'.
  SET TITLEBAR 'TITLE_0001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MONTA_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE monta_alv OUTPUT.

  DATA: ls_layout  TYPE lvc_s_layo,
        lt_f4      TYPE lvc_t_f4,
        ls_f4      TYPE lvc_s_f4,
        lt_exclude TYPE ui_functions,
        ls_exclude TYPE ui_func.

  IF o_alv IS INITIAL.

    CREATE OBJECT o_container
      EXPORTING
        container_name = 'CC_ALV'.

    CREATE OBJECT o_alv
      EXPORTING
        i_parent = o_container.

    ls_layout-zebra = abap_true.
    ls_layout-cwidth_opt = abap_true.
    ls_layout-ctab_fname = 'COLOR'.

    PERFORM f_monta_fieldcat.

    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND ls_exclude TO lt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND ls_exclude TO lt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND ls_exclude TO lt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND ls_exclude TO lt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND ls_exclude TO lt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND ls_exclude TO lt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND ls_exclude TO lt_exclude.

    SORT t_saida BY matnr.

*-US 153816-22-10-2024-#153816-RJF-Inicio
    CREATE OBJECT event_receiver.
    SET HANDLER: event_receiver->on_data_changed          FOR o_alv,
                 event_receiver->on_data_changed_finished FOR o_alv.

    SET HANDLER event_receiver->catch_hotspot              FOR o_alv.

    SELECT * FROM dd07t
       INTO TABLE @it_dd07t
        WHERE domname = 'ZMMD_URGENCIA_NEC'
          AND ddlanguage = @sy-langu(1)
          AND as4local = 'A'.

*-US 153816-22-10-2024-#153816-RJF-fim

    CALL METHOD o_alv->set_table_for_first_display
      EXPORTING
        is_layout            = ls_layout
        i_save               = 'A'
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_fieldcatalog      = t_fieldcat
        it_outtab            = t_saida.


*    CREATE OBJECT event_receiver.
*    SET HANDLER event_receiver->catch_hotspot              FOR o_alv.

    FREE: lt_f4.

    ls_f4-fieldname = 'CONTRATO'.

    ls_f4-register = 'X'.

    ls_f4-getbefore = 'X'.

    ls_f4-chngeafter = space.

    INSERT ls_f4 INTO lt_f4 INDEX 1 .

    CALL METHOD o_alv->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

* register events for abap objects (backend)
    CREATE OBJECT g_onf4.
    SET HANDLER g_onf4->on_f4 FOR o_alv.


  ENDIF.
ENDMODULE.
