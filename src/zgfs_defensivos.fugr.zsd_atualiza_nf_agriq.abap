FUNCTION zsd_atualiza_nf_agriq.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ACTTAB) TYPE  J_1BNFE_ACTIVE
*"  EXCEPTIONS
*"      PDF_NOT_FOUND
*"----------------------------------------------------------------------

  CHECK i_acttab-action_requ = 'C'  AND  "Completo
        i_acttab-docsta      = '1'.       "Autorizado

*--------------------------------------------
* atualiza NFe no portal AgriQ
*--------------------------------------------
  TRY .
      zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
         )->set_atualiza_nfe_receita( EXPORTING i_docnum = i_acttab-docnum ).

    CATCH zcx_integracao INTO DATA(ex_integra).
    CATCH zcx_error INTO DATA(ex_error).
  ENDTRY.

ENDFUNCTION.
