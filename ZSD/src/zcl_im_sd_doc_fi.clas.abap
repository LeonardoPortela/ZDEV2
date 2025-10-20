class ZCL_IM_SD_DOC_FI definition
  public
  final
  create public .

*"* public components of class ZCL_IM_SD_DOC_FI
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_AC_DOCUMENT .
protected section.
*"* protected components of class ZCL_IM_SD_DOC_FI
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_SD_DOC_FI
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_SD_DOC_FI IMPLEMENTATION.


method IF_EX_AC_DOCUMENT~CHANGE_AFTER_CHECK.
  MOVE-CORRESPONDING im_document-header to ex_document-header.
endmethod.


method IF_EX_AC_DOCUMENT~CHANGE_INITIAL.
  MOVE-CORRESPONDING im_document-header to ex_document-header.
endmethod.


method IF_EX_AC_DOCUMENT~IS_ACCTIT_RELEVANT.
endmethod.


method IF_EX_AC_DOCUMENT~IS_COMPRESSION_REQUIRED.
endmethod.


method IF_EX_AC_DOCUMENT~IS_SUPPRESSED_ACCT.
endmethod.
ENDCLASS.
