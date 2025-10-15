FUNCTION zles_fat_contingencia_0007.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"  EXPORTING
*"     VALUE(E_DADOS_FATURAMENTO_ECC) TYPE  ZDE_COMPARE_FATURAMENTO
*"----------------------------------------------------------------------

  DATA: lva_destination TYPE char40.


  CLEAR: e_dados_faturamento_ecc.


  CASE sy-sysid.
    WHEN 'DEV'.
      lva_destination =  'DEV_ECC'.
    WHEN 'QAS'.
      lva_destination =  'QAS_ECC'.
    WHEN 'PRD'.
      lva_destination =  'PRD_ECC'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CHECK lva_destination IS NOT INITIAL.

  "Função que Le o XML no GRC
  CALL FUNCTION 'ZLES_FATURAMENTO_CONTINGENCIA' DESTINATION lva_destination
    EXPORTING
      i_operacao          = '07'
      i_docnum            = i_docnum
    IMPORTING
      e_dados_faturamento = e_dados_faturamento_ecc.

ENDFUNCTION.
