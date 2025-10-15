FUNCTION zsd_distribuicao_historico.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ZSDS093) TYPE  ZSDS093
*"     REFERENCE(I_ZSDS094) TYPE  ZSDS094
*"----------------------------------------------------------------------

  CREATE OBJECT lc_distribuicao_insumos.

  lc_zsds093 = i_zsds093.
  wl_zsds094 = i_zsds094.

  PERFORM f_selecao_dados_estoque.
  PERFORM f_exibir_dados_estoque.

  IF t_saida_estoque[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não ha informações a serem exibidas.'.
    RETURN.
  ENDIF.

  CALL SCREEN 0200 STARTING AT  20 5
                     ENDING AT 170 19.

ENDFUNCTION.
