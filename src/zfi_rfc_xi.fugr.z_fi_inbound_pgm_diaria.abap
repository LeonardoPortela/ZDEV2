FUNCTION Z_FI_INBOUND_PGM_DIARIA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_PGM_DIARIA STRUCTURE  ZFIT0110
*"----------------------------------------------------------------------

  DATA: WA_PGM_DIARIA  TYPE ZFIT0110.

  DELETE FROM ZFIT0110 WHERE DT_VCTO >= SY-DATUM.

  LOOP AT IT_PGM_DIARIA INTO WA_PGM_DIARIA.

    WA_PGM_DIARIA-DT_ATUAL = SY-DATUM.
    WA_PGM_DIARIA-HR_ATUAL = SY-UZEIT.

    MODIFY ZFIT0110 FROM WA_PGM_DIARIA.

  ENDLOOP.

  COMMIT WORK.


ENDFUNCTION.
