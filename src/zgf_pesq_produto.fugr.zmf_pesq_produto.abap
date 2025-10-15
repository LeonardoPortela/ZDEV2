FUNCTION zmf_pesq_produto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_GRUPO_MATERIAL) TYPE  WRF_RANGE_MATKL_TTY OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MATNR) TYPE  MATNR
*"----------------------------------------------------------------------

  CLEAR: e_matnr.

  PERFORM pesq_produto USING i_grupo_material CHANGING e_matnr.

ENDFUNCTION.
