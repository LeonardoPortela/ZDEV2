PROCESS BEFORE OUTPUT.

  MODULE pbo_manter.
*
PROCESS AFTER INPUT.

  MODULE pai_manter.

PROCESS ON VALUE-REQUEST.

    FIELD <fs_wa_registro_manter>-conta_ori
    MODULE pai_help_field_0001.

    FIELD <fs_wa_registro_manter>-conta_cut
    MODULE pai_help_field_0002.

    FIELD <fs_wa_registro_manter>-tipo
    MODULE pai_help_field_0003.
