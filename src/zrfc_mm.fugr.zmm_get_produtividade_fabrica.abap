FUNCTION zmm_get_produtividade_fabrica.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZIB_PROD_CRUSHF STRUCTURE  ZIB_PROD_CRUSHF
*"----------------------------------------------------------------------


  SELECT *
    FROM zib_prod_crushf
    INTO TABLE et_zib_prod_crushf.


ENDFUNCTION.
