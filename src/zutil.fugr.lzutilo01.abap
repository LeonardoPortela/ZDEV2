*----------------------------------------------------------------------*
***INCLUDE LZUTILO01.
*----------------------------------------------------------------------*

DATA: cont TYPE REF TO cl_gui_custom_container,
      html TYPE REF TO cl_gui_html_viewer.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  DATA: i_uri      TYPE c LENGTH 200,
        data_table TYPE STANDARD TABLE OF text255.

  IF cont IS INITIAL.

    cont = NEW #( container_name = 'HTML' ).

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = html_pagina
        i_tabline_length = 255
      TABLES
        et_table         = data_table.

    html = NEW #( parent = cont ).
    html->load_data(
      EXPORTING
        type                   = 'text'
        subtype                = 'html'
      IMPORTING
        assigned_url           = i_uri
      CHANGING
        data_table             = data_table
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    html->show_url(
      EXPORTING
        url                    = i_uri
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5
    ).

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  PERFORM limpar_tela_0100.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_tela_0100 .

  html->free( ).
  CLEAR: html.
  cont->free( ).
  CLEAR: cont.

ENDFORM.

*-US192246-06.10.2025-#1922468-JT-inicio
*---------------------------------------------------------------------*
*      Form  F_PREENCHE_FCAT
*---------------------------------------------------------------------*
FORM f_preenche_fcat   USING  VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_hotspot)
                              VALUE(p_checkbox)
                              VALUE(p_icon).

  CLEAR: wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-icon          = p_icon.
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-checkbox      = p_checkbox.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_fieldcat.

ENDFORM.
*-US192246-06.10.2025-#1922468-JT-inicio

*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
