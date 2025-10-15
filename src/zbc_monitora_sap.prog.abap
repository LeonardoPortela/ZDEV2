*&---------------------------------------------------------------------*
*& Report  ZBC_MONITORA_SAP
*& JOB para monitorar ambiente SAP
*& Monitoramento pró-ativo
*&---------------------------------------------------------------------*
*& PBI 55765
*&---------------------------------------------------------------------*
REPORT zbc_monitora_sap.
*&---------------------------------------------------------------------*
DATA: vg_job      TYPE i.

SELECT SINGLE COUNT(*) INTO vg_job FROM tbtco
  WHERE jobname EQ 'Z_MONITORA_SAP'
    AND status EQ 'R'.

*Não rodar se já tiver outro job em execução
IF ( vg_job = 1 ).

  DATA(obj_monitor) = NEW zcl_monitora_sap( ).

*  Monitorar os dumps no ambiente:
  obj_monitor->get_dumps( ).

ENDIF.
