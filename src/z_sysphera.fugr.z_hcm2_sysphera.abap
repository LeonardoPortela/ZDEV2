FUNCTION Z_HCM2_SYSPHERA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ANO) TYPE  GJAHR
*"     REFERENCE(I_MES) TYPE  MONAT
*"  TABLES
*"      RESULTADO STRUCTURE  ZHCME_SYSPHERA_FUNCIONARIOS
*"----------------------------------------------------------------------


  CALL FUNCTION 'ZHCMF_SYSPHERA_FUNCIONARIOS'
    EXPORTING
      I_MES           = I_MES    " Mês do exercício
      I_ANO           = I_ANO    " Exercício
    TABLES
      IT_FUNCIONARIOS = RESULTADO.    " SYSPHERA - Estrutura de funcionários

ENDFUNCTION.
