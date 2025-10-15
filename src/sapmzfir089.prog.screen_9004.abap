
PROCESS BEFORE OUTPUT.

  MODULE status_9004.
*
PROCESS AFTER INPUT.

  MODULE user_command_9004_exit AT EXIT-COMMAND.

  CHAIN.
    FIELD wa_fluxo_est02-nv01 MODULE z_check_code_nivel1.
    FIELD wa_fluxo_est02-nv02 MODULE z_check_code_nivel2.
    FIELD wa_fluxo_est02-nv03 MODULE z_check_code_nivel3.
    FIELD wa_fluxo_est02-nv04 MODULE z_check_code_nivel4.
    FIELD wa_fluxo_est02-nv05 MODULE z_check_code_nivel5.

  ENDCHAIN.

  MODULE user_command_9004.


PROCESS ON VALUE-REQUEST.
  FIELD wa_fluxo_est02-nitxt MODULE z_editar_campo.

  FIELD wa_fluxo_est02-nv01 MODULE z_mathcode_nivel1.
  FIELD wa_fluxo_est02-nv02 MODULE z_mathcode_nivel2.
  FIELD wa_fluxo_est02-nv03 MODULE z_mathcode_nivel3.
  FIELD wa_fluxo_est02-nv04 MODULE z_mathcode_nivel4.
  FIELD wa_fluxo_est02-nv05 MODULE z_mathcode_nivel5.
