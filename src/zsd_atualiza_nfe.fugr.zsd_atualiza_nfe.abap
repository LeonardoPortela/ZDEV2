FUNCTION zsd_atualiza_nfe.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IN_NOTAS) TYPE  ZIB_NOTA_FISCAL_SAP OPTIONAL
*"     VALUE(IN_DOC) TYPE  J_1BNFDOC OPTIONAL
*"----------------------------------------------------------------------

* Retorno Formação Lote
  PERFORM z_form_lote USING in_notas
                            in_doc.

ENDFUNCTION.
