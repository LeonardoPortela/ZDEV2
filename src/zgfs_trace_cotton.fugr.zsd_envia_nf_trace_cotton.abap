FUNCTION zsd_envia_nf_trace_cotton.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ACTTAB) TYPE  J_1BNFE_ACTIVE
*"  EXCEPTIONS
*"      PDF_NOT_FOUND
*"----------------------------------------------------------------------

  CHECK i_acttab-action_requ = 'C'  AND  "Completo
        i_acttab-docsta      = '1'.      "Autorizado

*--------------------------------------------
* Envia NF para trace cotton
*--------------------------------------------
  IF i_acttab-cancel = abap_off.
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_envia_nota_fiscal(  EXPORTING i_docnum = i_acttab-docnum ).

      CATCH zcx_integracao INTO DATA(ex_integra).
      CATCH zcx_error      INTO DATA(ex_error).
    ENDTRY.

  ELSE.
    TRY .
        zcl_trace_cotton=>zif_trace_cotton~get_instance(
           )->set_envia_nf_cancelada( EXPORTING i_docnum = i_acttab-docnum ).

      CATCH zcx_integracao INTO ex_integra.
      CATCH zcx_error      INTO ex_error.
    ENDTRY.
  ENDIF.

ENDFUNCTION.
