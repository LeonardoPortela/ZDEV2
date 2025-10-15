*----------------------------------------------------------------------*
***INCLUDE LZGFS_INSUMOSF06.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_criar_editor
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_criar_editor .

  IF ( container_editor IS INITIAL ).

    CREATE OBJECT container_editor
      EXPORTING
        container_name              = 'CONTAINER_EDITOR'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT cl_editor
      EXPORTING
        parent                     = container_editor
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS                     = 1.

    CALL METHOD cl_editor->set_readonly_mode( readonly_mode = cl_editor->true ).
  ENDIF.

  CALL METHOD cl_editor->set_text_as_r3table
    EXPORTING
      table           = t_tabedit
    EXCEPTIONS
      error_dp        = 1
      error_dp_create = 2
      OTHERS          = 3.

ENDFORM.
