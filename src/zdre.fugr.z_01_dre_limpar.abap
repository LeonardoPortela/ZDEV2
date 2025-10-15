FUNCTION z_01_dre_limpar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

  "Exclui todos os registros integrados neste dia
  EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
    DELETE FROM SAPHANADB.ZGLT_DRE_01
*    DELETE SAPSR3.ZGLT_DRE_01
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
  ENDEXEC.
  "Exclui todos os registros integrados neste dia
  EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
    DELETE FROM SAPHANADB.ZGLT_DRE_02
*    DELETE SAPSR3.ZGLT_DRE_02
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
  ENDEXEC.
  "Exclui todos os registros integrados neste dia
  EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
    DELETE FROM SAPHANADB.ZGLT_DRE_03
*    DELETE SAPSR3.ZGLT_DRE_03
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
  ENDEXEC.
  "Exclui todos os registros integrados neste dia
  EXEC SQL.
* ---> S4 Migration - 23/07/2023 - MG-5609 - CA
    DELETE FROM SAPHANADB.ZGLT_DRE_04 WHERE OBSERVACAO = ''
*    DELETE SAPSR3.ZGLT_DRE_04 WHERE OBSERVACAO = ''
* <--- S4 Migration - 23/07/2023 - MG-5609 - CA
  ENDEXEC.

ENDFUNCTION.
