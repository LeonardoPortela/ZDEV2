FUNCTION zmmf0071_estrategia_aprovacao .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CENTRO) TYPE  FRE_RANGE_T_WERKS OPTIONAL
*"     VALUE(I_TIPO) TYPE  ZRANGE_T_ATWRT OPTIONAL
*"     VALUE(I_CCUSTO) TYPE  FAGL_RANGE_T_KOSTL OPTIONAL
*"  EXPORTING
*"     VALUE(T_RESULT) TYPE  ZMME0111_T
*"     VALUE(E_ERRO) TYPE  STRING
*"----------------------------------------------------------------------


  IF i_centro IS NOT INITIAL.
    s_werks = i_centro.
  ENDIF.

  IF i_tipo IS NOT INITIAL.
    s_tipo = i_tipo.
  ENDIF.

  IF i_ccusto IS NOT INITIAL.
    s_kostl = i_ccusto.
  ENDIF.


  CHECK s_werks IS NOT INITIAL OR s_kostl IS NOT INITIAL.

  PERFORM fm_seleciona_dados CHANGING e_erro.
  CHECK e_erro IS INITIAL.
  PERFORM fm_organiza_saida CHANGING t_result.

ENDFUNCTION.
