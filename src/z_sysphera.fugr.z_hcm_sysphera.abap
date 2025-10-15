FUNCTION Z_HCM_SYSPHERA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ANO) TYPE  GJAHR
*"     REFERENCE(I_MES) TYPE  MONAT
*"  TABLES
*"      RESULTADO STRUCTURE  ZHCME_SYSPHERA_FOLHA
*"----------------------------------------------------------------------


  CALL FUNCTION 'ZHCMF_SYSPHERA_FOLHA'
    EXPORTING
      MES      = I_MES    " Mês do exercício
      ANO      = I_ANO    " Mês do exercício
    TABLES
      IT_FOLHA = RESULTADO.    " SYSPHERA - Estrutura de funcionários

ENDFUNCTION.
