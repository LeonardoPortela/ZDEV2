PROCESS BEFORE OUTPUT.
  MODULE define_frm_pgto.
  MODULE completa_dados.
  MODULE modify_screen.
  MODULE status_0110.
  MODULE criar_objetos_0110.

PROCESS AFTER INPUT.

  MODULE user_command_0110_exit AT EXIT-COMMAND.
  MODULE user_command_0110.

PROCESS ON VALUE-REQUEST.
  FIELD zglt080-lote     MODULE help_lote.
  FIELD zglt080-bvtyp    MODULE help_bvtyp.
  FIELD zglt080-zlsch    MODULE help_zlsch.
  FIELD zglt080-zlspr    MODULE help_zlspr. "RJF - CS2024000206
  FIELD zglt080-hbkid    MODULE help_hbkid.
