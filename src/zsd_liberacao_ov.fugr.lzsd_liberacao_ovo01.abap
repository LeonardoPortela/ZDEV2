*----------------------------------------------------------------------*
***INCLUDE LZSD_LIBERACAO_OVO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9090 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9090 OUTPUT.

  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR '9090' WITH gv_title.

  IF go_container IS NOT BOUND.

    CREATE OBJECT go_container
      EXPORTING
        container_name              = 'CC_TEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
*      add your handling
    ENDIF.

  ENDIF.

  CHECK go_container IS BOUND.

  CREATE OBJECT go_editor
    EXPORTING
      max_number_chars           = gv_limit
*     style                      = 0
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
*     wordwrap_position          = -1
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true
*     filedrop_mode              = DROPFILE_EVENT_OFF
      parent                     = go_container
*     lifetime                   =
*     name                       =
*    EXCEPTIONS
*     error_cntl_create          = 1
*     error_cntl_init            = 2
*     error_cntl_link            = 3
*     error_dp_create            = 4
*     gui_type_not_supported     = 5
*     others                     = 6
    .
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF gv_edit IS INITIAL.

    go_editor->set_enable( abap_false ).

  ENDIF.


  CALL METHOD go_editor->set_text_as_r3table
    EXPORTING
      table           = gt_text
    EXCEPTIONS
      error_dp        = 1
      error_dp_create = 2
      OTHERS          = 3.

  CALL METHOD go_editor->set_toolbar_mode
    EXPORTING
      toolbar_mode = go_editor->false.

  CALL METHOD go_editor->set_statusbar_mode
    EXPORTING
      statusbar_mode = go_editor->false.


  "go_editor->

ENDMODULE.
