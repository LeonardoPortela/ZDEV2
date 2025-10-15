PROCESS BEFORE OUTPUT.

  MODULE pbo_manter.
*
PROCESS AFTER INPUT.

  MODULE pai_manter.

PROCESS ON VALUE-REQUEST.

  FIELD <fs_wa_registro_manter>-cd_modal MODULE pai_help_field_0001.
  FIELD <fs_wa_registro_manter>-operacao MODULE pai_help_field_0002.
