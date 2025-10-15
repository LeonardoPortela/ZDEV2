  PROCESS BEFORE OUTPUT.

    MODULE pbo_manter.
*
  PROCESS AFTER INPUT.

    MODULE pai_manter.

  PROCESS ON VALUE-REQUEST.

    FIELD <fs_wa_registro_manter>-equip
    MODULE pai_help_field_0001.

    FIELD <fs_wa_registro_manter>-emp_locacao
    MODULE pai_help_field_0002.

    FIELD <fs_wa_registro_manter>-contrato_spot
    MODULE pai_help_field_0003.

    FIELD <fs_wa_registro_manter>-contrato_corp
    MODULE pai_help_field_0004.
