FUNCTION Z_BUSCA_PARAMETROS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NOME_PARAMETRO) TYPE  CHAR30
*"  EXPORTING
*"     REFERENCE(E_CAMINHO) TYPE  STRING
*"  EXCEPTIONS
*"      PARAMETRO_NOT_FOUND
*"----------------------------------------------------------------------


  SELECT SINGLE VALOR
    FROM ZPARAMETROS
    INTO E_CAMINHO
     WHERE NOME_PARAMETRO EQ I_NOME_PARAMETRO
       AND STATUS  EQ 'A'.

  IF SY-SUBRC IS NOT INITIAL.
    RAISE PARAMETRO_NOT_FOUND.
  ENDIF.

ENDFUNCTION.
