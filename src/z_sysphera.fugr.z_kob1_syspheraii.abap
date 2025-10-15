FUNCTION Z_KOB1_SYSPHERAII.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_AREA) TYPE  KOKRS DEFAULT 'MAGI'
*"     VALUE(IT_ORDENS) TYPE  ZTTSYS_AUFNR
*"     VALUE(I_PERIODO) TYPE  MONAT OPTIONAL
*"     VALUE(I_ANO) TYPE  GJAHR OPTIONAL
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_KOB1
*"----------------------------------------------------------------------


call function 'Z_KOB1_SYSPHERA'
        exporting
          i_area    = I_AREA
          i_periodo = I_PERIODO
          i_ano     = I_ANO
          it_ordens = IT_ORDENS
        tables
          resultado = RESULTADO.


ENDFUNCTION.
