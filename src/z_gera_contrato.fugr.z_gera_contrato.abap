FUNCTION z_gera_contrato.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOC) TYPE  ZSDED003 OPTIONAL
*"     REFERENCE(I_DIR) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_NO_PRINT) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(E_XSTRING_DOCUMENT) TYPE  XSTRING
*"  TABLES
*"      R_DOC_SIMULACAO STRUCTURE  ZSD_RANGE_DOCSI OPTIONAL
*"----------------------------------------------------------------------

  FREE: e_xstring_document.

  s_doc_simulacao[] = r_doc_simulacao[].  "*-CS2019001753-27.02.2023-#65723-JT

  PERFORM seleciona_dados USING i_doc.
  PERFORM busca_vbeln.
  PERFORM agrupa_dados  USING i_dir
                              i_no_print            "*-CS2019001753-27.02.2023-#65723-JT
                     CHANGING e_xstring_document.  "*-CS2019001753-27.02.2023-#65723-JT
ENDFUNCTION.
