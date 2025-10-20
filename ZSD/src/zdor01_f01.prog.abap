*----------------------------------------------------------------------*
***INCLUDE DOR01_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  convert_to_pdf
*&---------------------------------------------------------------------*
FORM convert_to_pdf.

  DATA: lv_otf_memory_switch TYPE boolean,
        lt_otf TYPE TABLE OF itcoo,
        lt_lines TYPE TABLE OF tline,
        lv_binfilesize TYPE i,
        lv_bin_file TYPE xstring,
        lv_pdf TYPE fpcontent.

* check Web UI
  CHECK xscreen = 'W'.

* read otf
  CALL FUNCTION 'READ_OTF_FROM_MEMORY'
    EXPORTING
      memory_key   = 'DEF_OTF_MEMORY_KEY'
    TABLES
      otf          = lt_otf
    EXCEPTIONS
      memory_empty = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    retcode = sy-subrc.
    syst-msgty = 'E'.
    PERFORM protocol_update.
    EXIT.
  ENDIF.

* convert to otf
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = lv_binfilesize
      bin_file              = lv_bin_file
    TABLES
      otf                   = lt_otf
      lines                 = lt_lines
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.
  IF sy-subrc <> 0.
    retcode = sy-subrc.
    syst-msgty = 'E'.
    PERFORM protocol_update.
    EXIT.
  ENDIF.

* export pdf to memory
  lv_pdf = lv_bin_file.
  EXPORT lv_pdf_file = lv_pdf TO MEMORY ID 'PDF_FILE'.

ENDFORM.                    "convert_to_pdf
*&---------------------------------------------------------------------*
*&      Form  CONVERT_OTF_TO_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OTFDATA  text
*----------------------------------------------------------------------*
FORM convert_otf_to_pdf  TABLES  lt_otfdata STRUCTURE itcoo.

  DATA: lv_otf_memory_switch TYPE boolean,
          lt_otf TYPE TABLE OF itcoo,
          lt_lines TYPE TABLE OF tline,
          lv_binfilesize TYPE i,
          lv_bin_file TYPE xstring,
          lv_pdf TYPE fpcontent.

* convert to otf
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = lv_binfilesize
      bin_file              = lv_bin_file
    TABLES
      otf                   = lt_otfdata
      lines                 = lt_lines
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.
  IF sy-subrc <> 0.
    retcode = sy-subrc.
    syst-msgty = 'E'.
    PERFORM protocol_update.
    EXIT.
  ENDIF.

* export pdf to memory
  lv_pdf = lv_bin_file.
  EXPORT lv_pdf_file = lv_pdf TO MEMORY ID 'PDF_FILE'.

ENDFORM.                    " CONVERT_OTF_TO_PDF
