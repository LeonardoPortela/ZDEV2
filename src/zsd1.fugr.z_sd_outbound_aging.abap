FUNCTION Z_SD_OUTBOUND_AGING.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_ZAGING_SIACORP STRUCTURE  ZAGING_SIACORP
*"----------------------------------------------------------------------

*  perform zseleciona_dados.
*
*  refresh: t_zaging_siacorp .
*
*  LOOP AT it_aging_siacorp into wa_aging_siacorp.
*    append wa_aging_siacorp to t_zaging_siacorp.
*  ENDLOOP.




ENDFUNCTION.
