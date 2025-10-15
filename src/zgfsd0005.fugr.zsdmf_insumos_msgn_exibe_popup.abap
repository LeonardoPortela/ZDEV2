FUNCTION zsdmf_insumos_msgn_exibe_popup.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  PERFORM f_mensagem_exibe_popup USING it_return.

ENDFUNCTION.
