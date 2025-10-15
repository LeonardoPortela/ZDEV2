PROCESS BEFORE OUTPUT.

  MODULE pbo_manter.
*
PROCESS AFTER INPUT.

  MODULE pai_manter_exit AT EXIT-COMMAND.

  MODULE pai_manter.

PROCESS ON VALUE-REQUEST.

  FIELD <fs_wa_registro_manter>-eudr MODULE pai_help_field_0001.
