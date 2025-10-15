PROCESS BEFORE OUTPUT.

  MODULE pbo_manter.
*
PROCESS AFTER INPUT.

  MODULE pai_manter.

PROCESS ON VALUE-REQUEST.

  FIELD <fs_wa_registro_manter>-ctg_cargo
  MODULE pai_help_field_0001.

  FIELD <fs_wa_registro_manter>-cod_cargo
MODULE pai_help_field_0002.
