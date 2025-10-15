PROCESS BEFORE OUTPUT.

  MODULE pbo_manter.
*
PROCESS AFTER INPUT.

  MODULE pai_manter.

PROCESS ON VALUE-REQUEST.

  FIELD <fs_wa_registro_manter>-razao_espec_para
  MODULE pai_help_field_0004.

  FIELD <fs_wa_registro_manter>-razao_espec_de
  MODULE pai_help_field_0003.

  FIELD <fs_wa_registro_manter>-conta_razao_de
  MODULE pai_help_field_0001.

  FIELD <fs_wa_registro_manter>-conta_razao_para
  MODULE pai_help_field_0002.
