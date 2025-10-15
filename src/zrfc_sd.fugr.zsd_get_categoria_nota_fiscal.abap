FUNCTION ZSD_GET_CATEGORIA_NOTA_FISCAL.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_SAIDA STRUCTURE  J_1BAA
*"----------------------------------------------------------------------

  SELECT *
    FROM J_1BAA INTO TABLE T_SAIDA.


ENDFUNCTION.
