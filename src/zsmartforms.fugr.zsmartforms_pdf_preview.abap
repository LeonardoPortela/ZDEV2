FUNCTION zsmartforms_pdf_preview.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OTF) TYPE  TSFOTF OPTIONAL
*"     REFERENCE(I_PDF) TYPE  XSTRING OPTIONAL
*"     REFERENCE(I_DOC_SIMULACAO) TYPE  ZID_DOCUMENTO OPTIONAL
*"     REFERENCE(I_SALVAR) TYPE  CHAR01 OPTIONAL
*"  EXCEPTIONS
*"      CONVERT_OTF_TO_PDF_ERROR
*"      CNTL_ERROR
*"----------------------------------------------------------------------
  DATA: l_dummy  TYPE STANDARD TABLE OF tline,
*       pdf_data TYPE xstring,   "#149060-19.08.2024-JT
*       pdf_size TYPE i,         "#149060-19.08.2024-JT
        l_len    TYPE i,
        l_offset TYPE i.

  FREE: l_pdf_data.
  CLEAR: pdf_data, pdf_size.

  g_salvar = i_salvar.  "#149060-19.08.2024-JT

*-CS2021000218-16.11.2022-#90706-JT-inicio
  IF i_otf[] IS NOT INITIAL.
* convert otf to pdf
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = pdf_size
        bin_file              = pdf_data
      TABLES
        otf                   = i_otf[]
        lines                 = l_dummy
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        OTHERS                = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING convert_otf_to_pdf_error.
    ENDIF.
  ELSE.
    pdf_data = i_pdf.
    pdf_size = xstrlen( i_pdf ).
  ENDIF.
*-CS2021000218-16.11.2022-#90706-JT-fim

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

*-CS2019001753-29.06.2023-#65723-JT-inicio
  g_doc_simulacao = i_doc_simulacao.

  IF g_doc_simulacao IS INITIAL AND g_salvar IS INITIAL. "#149060-19.08.2024-JT
    CALL SCREEN 0300.
  ELSE.
    CALL SCREEN 0301.
  ENDIF.
*-CS2019001753-29.06.2023-#65723-JT-fim

ENDFUNCTION.
