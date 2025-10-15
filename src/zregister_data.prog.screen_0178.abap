  PROCESS BEFORE OUTPUT.

    MODULE pbo_manter.
*
  PROCESS AFTER INPUT.

    MODULE pai_manter.

  PROCESS ON VALUE-REQUEST.

    FIELD <fs_wa_registro_manter>-branch
    MODULE pai_help_field_0001.

    FIELD <fs_wa_registro_manter>-lgort
    MODULE pai_help_field_0002.
