FUNCTION ZSMARTFORMS_OTF_XSTRING.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(OTF) TYPE  TT_ITCOO
*"  EXPORTING
*"     REFERENCE(LS_PDF_STRING_X) TYPE  XSTRING
*"----------------------------------------------------------------------

  DATA: V_BIN_FILESIZE TYPE I,
        PDF_TAB        LIKE TLINE OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      FORMAT                = 'PDF'
      MAX_LINEWIDTH         = 132
    IMPORTING
      BIN_FILESIZE          = V_BIN_FILESIZE
      BIN_FILE              = LS_PDF_STRING_X
    TABLES
      OTF                   = OTF
      LINES                 = PDF_TAB
    EXCEPTIONS
      ERR_MAX_LINEWIDTH     = 1
      ERR_FORMAT            = 2
      ERR_CONV_NOT_POSSIBLE = 3
      OTHERS                = 4.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFUNCTION.
