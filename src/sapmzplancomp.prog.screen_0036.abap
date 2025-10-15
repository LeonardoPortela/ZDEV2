
PROCESS BEFORE OUTPUT.
* MODULE STATUS_0036.
*
  MODULE cria_alv_reme_notas_disponivel.
  MODULE cria_alv_vinc_lotes.

  CALL SUBSCREEN: sub0033 INCLUDING sy-cprog vg_dynnr_0033.

PROCESS AFTER INPUT.

  CALL SUBSCREEN: sub0033.

  MODULE user_command_0036.
