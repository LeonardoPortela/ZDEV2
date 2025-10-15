  PROCESS BEFORE OUTPUT.

    MODULE pbo_manter.
*
  PROCESS AFTER INPUT.

    MODULE pai_manter.


  PROCESS ON VALUE-REQUEST.

*    FIELD <fs_wa_registro_manter>-investidora
*    MODULE pai_help_field_0001.
*
*    FIELD <fs_wa_registro_manter>-investida
*    MODULE pai_help_field_0002.

    FIELD <fs_wa_registro_manter>-hkont
    MODULE pai_help_field_0003.
