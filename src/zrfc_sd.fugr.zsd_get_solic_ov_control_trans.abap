FUNCTION ZSD_GET_SOLIC_OV_CONTROL_TRANS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      ET_ZSDT0090 STRUCTURE  ZSDT0090
*"----------------------------------------------------------------------


  SELECT *
    FROM ZSDT0090
    INTO TABLE et_ZSDT0090.

    sort ET_ZSDT0090 BY doc_simulacao sequencia.
    DELETE ADJACENT DUPLICATES FROM ET_ZSDT0090  COMPARING doc_simulacao sequencia.




ENDFUNCTION.
