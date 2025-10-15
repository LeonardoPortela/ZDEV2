  PROCESS BEFORE OUTPUT.

  MODULE PBO_MANTER.
*
PROCESS AFTER INPUT.

  MODULE PAI_MANTER.

  PROCESS ON VALUE-REQUEST.

    FIELD <fs_wa_registro_manter>-conjunto
    MODULE pai_help_field_0001.

    FIELD <fs_wa_registro_manter>-codegruppe
    MODULE pai_help_field_0002.

    FIELD <fs_wa_registro_manter>-code
    MODULE pai_help_field_0003.

    FIELD <fs_wa_registro_manter>-mptyp
    MODULE pai_help_field_0004.
