FUNCTION ZLES_FAT_CONTINGENCIA_0004.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"  EXPORTING
*"     REFERENCE(E_ZLEST0143) TYPE  ZLEST0143
*"----------------------------------------------------------------------

  DATA: lva_chave_doc_e TYPE zde_chave_doc_e.

  DATA: lva_destination TYPE char40.

  CLEAR: E_ZLEST0143.

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

  SELECT SINGLE *
    from j_1bnfe_active INTO @DATA(lwa_active)
   WHERE docnum eq @I_DOCNUM.

  CHECK sy-subrc eq 0 AND I_DOCNUM is NOT INITIAL.

  CONCATENATE  lwa_active-REGIO
               lwa_active-NFYEAR
               lwa_active-NFMONTH
               lwa_active-STCD1
               lwa_active-MODEL
               lwa_active-SERIE
               lwa_active-NFNUM9
               lwa_active-DOCNUM9
               lwa_active-CDV  INTO lva_chave_doc_e.


  "Função que Le o XML no GRC
  CALL FUNCTION 'ZLES_FATURAMENTO_CONTINGENCIA' DESTINATION lva_destination
    EXPORTING
      i_operacao          = '03'
      I_CHAVE_DOC_E       = lva_chave_doc_e
    IMPORTING
      E_ZLEST0143         = E_ZLEST0143.


ENDFUNCTION.
