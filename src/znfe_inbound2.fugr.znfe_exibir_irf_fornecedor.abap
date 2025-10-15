FUNCTION znfe_exibir_irf_fornecedor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_LIFNR) TYPE  LIFNR
*"  EXCEPTIONS
*"      SEM_DADOS
*"----------------------------------------------------------------------

  PERFORM f_selecao_fornecedor USING i_bukrs
                                     i_lifnr.

  IF t_saida[] IS INITIAL.
    RAISE sem_dados.
  ENDIF.

  PERFORM f_exibir_dados.

ENDFUNCTION.
