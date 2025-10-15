FUNCTION ZLES_MONITORA_002.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NOMEACAO) TYPE  ZDE_RESERVA_NOMEACAO
*"----------------------------------------------------------------------

  PERFORM PESQUISA_NOMEACAO_NOTA_VINC USING I_NOMEACAO.

  PERFORM SHOW_RESERVA_NOMEACAO USING I_NOMEACAO.

ENDFUNCTION.
