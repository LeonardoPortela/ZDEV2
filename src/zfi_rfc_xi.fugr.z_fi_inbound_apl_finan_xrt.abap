FUNCTION z_fi_inbound_apl_finan_xrt.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_APL_FINAN_XRT STRUCTURE  ZFIT0112
*"----------------------------------------------------------------------

  DATA : WA_APL_FINAN_XRT  TYPE ZFIT0112.

  DELETE FROM ZFIT0112 WHERE DATA_VENCIMENTO >= SY-DATUM.
  DELETE FROM ZFIT0112 WHERE MDO_TIPO = 'S'.  "Deleta Todos Lctos Resgate Aplicação (Sobra Caixa).

  LOOP AT IT_APL_FINAN_XRT INTO WA_APL_FINAN_XRT.

    MODIFY ZFIT0112 FROM WA_APL_FINAN_XRT.

  ENDLOOP.

  COMMIT WORK.

ENDFUNCTION.
