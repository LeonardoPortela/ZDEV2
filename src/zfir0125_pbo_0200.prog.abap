*----------------------------------------------------------------------*
***INCLUDE ZFIR0125_PBO_0200.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
 SET PF-STATUS 'STATUS_0200'.
* SET TITLEBAR 'xxx'.

  IF go_container2 IS NOT BOUND.

*   create control container
    CREATE OBJECT go_container2
      EXPORTING
        container_name = 'CC_JUST'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc = 0.

      CREATE OBJECT go_textedit
        EXPORTING
          max_number_chars           = 255
          parent                     = go_container2
          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position          = 60
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    ENDIF.

  ENDIF.
ENDMODULE.
