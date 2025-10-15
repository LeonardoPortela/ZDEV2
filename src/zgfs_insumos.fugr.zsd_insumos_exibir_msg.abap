FUNCTION zsd_insumos_exibir_msg.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MENSAGEM) TYPE  STRING
*"----------------------------------------------------------------------

  FREE: t_tabedit.

  MOVE i_mensagem   TO w_tabedit.
  APPEND w_tabedit  TO t_tabedit.

  CALL SCREEN 0600 STARTING AT 50   10
                     ENDING AT 130  17.

ENDFUNCTION.
