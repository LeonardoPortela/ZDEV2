PROCESS BEFORE OUTPUT.
  MODULE status_0100.
*
PROCESS AFTER INPUT.
  CHAIN.
    FIELD zcarga_cte_tip-nucontrato MODULE f_check_contrato.
    FIELD zlest0025-chvid           MODULE f_check_ajuste.
    FIELD lv_peso_descarga          MODULE f_check_peso_descarga.
  ENDCHAIN.

  MODULE user_command_0100.

PROCESS ON VALUE-REQUEST.
  FIELD zcarga_cte_tip-nucontrato MODULE f_help_contrato.
  FIELD zlest0025-chvid           MODULE f_help_ajuste.
