FUNCTION Z_FI_INBOUND_COMP_PRODUTOR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_DOCUMENTOS STRUCTURE  ZFIT0125
*"----------------------------------------------------------------------

  DATA: WL_DOCUMENTOS TYPE ZFIT0125.

  LOOP AT IT_DOCUMENTOS INTO WL_DOCUMENTOS.

    WL_DOCUMENTOS-DT_ATUAL = SY-DATUM.
    WL_DOCUMENTOS-HR_ATUAL = SY-UZEIT.

    MODIFY ZFIT0125 FROM WL_DOCUMENTOS.

  ENDLOOP.

  COMMIT WORK.

ENDFUNCTION.
