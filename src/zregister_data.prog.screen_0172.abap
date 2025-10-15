PROCESS BEFORE OUTPUT.

  MODULE pbo_manter.

*
PROCESS AFTER INPUT.

  MODULE pai_manter.

PROCESS ON VALUE-REQUEST.

  FIELD <fs_wa_registro_manter>-herst
  MODULE pai_help_field_0001.

  FIELD <fs_wa_registro_manter>-typbz
  MODULE pai_help_field_0002.

  FIELD <fs_wa_registro_manter>-fleet_cat
  MODULE pai_help_field_0003.
