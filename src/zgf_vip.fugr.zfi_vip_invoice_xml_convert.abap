FUNCTION ZFI_VIP_INVOICE_XML_CONVERT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_XML) TYPE  STRING
*"  TABLES
*"      T_CONTEUDO TYPE  ZFIT_T_VIP_INVOICE
*"----------------------------------------------------------------------


  PERFORM FM_PROCESS_XML USING I_XML CHANGING T_CONTEUDO[].


ENDFUNCTION.
