FUNCTION zsmartforms_pdf_file_preview.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PDF_DATA) TYPE  XSTRING OPTIONAL
*"     REFERENCE(PDF_DATA_STRING) TYPE  STRING OPTIONAL
*"  TABLES
*"      BINARY_TAB OPTIONAL
*"----------------------------------------------------------------------
  DATA: pdf_size TYPE i,
        l_len    TYPE i,
        l_offset TYPE i.

  FREE: l_pdf_data.

  IF binary_tab[] IS NOT INITIAL.

    LOOP AT binary_tab INTO l_pdf_line.
      APPEND l_pdf_line TO l_pdf_data.
    ENDLOOP.

  ELSEIF pdf_data_string IS NOT INITIAL.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = pdf_data_string
        i_tabline_length = 1000
*       I_UNICODE        =
      TABLES
        et_table         = l_pdf_data.

*   BREAK-POINT.

    l_len = strlen( pdf_data_string ).
    WHILE l_len >= 1000.
      l_pdf_line = pdf_data_string+l_offset(1000).
      APPEND l_pdf_line TO l_pdf_data.
      ADD 1000 TO l_offset.
      SUBTRACT 1000 FROM l_len.
    ENDWHILE.
    IF l_len > 0.
      l_pdf_line = pdf_data_string+l_offset(l_len).
      APPEND l_pdf_line TO l_pdf_data.
    ENDIF.

  ELSE.

    l_len = xstrlen( pdf_data ).
    WHILE l_len >= 1000.
      l_pdf_line = pdf_data+l_offset(1000).
      APPEND l_pdf_line TO l_pdf_data.
      ADD 1000 TO l_offset.
      SUBTRACT 1000 FROM l_len.
    ENDWHILE.
    IF l_len > 0.
      l_pdf_line = pdf_data+l_offset(l_len).
      APPEND l_pdf_line TO l_pdf_data.
    ENDIF.

  ENDIF.

  CALL SCREEN 0300.

ENDFUNCTION.
