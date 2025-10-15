PROCESS BEFORE OUTPUT.
  MODULE status_0200.

PROCESS AFTER INPUT.

**  Begin of   "FF #96115
  CHAIN. "Caso digite algo no campo é chamado este module.
    FIELD zaponta-mncod.
    MODULE code_pf4_mncod ON CHAIN-REQUEST.
  ENDCHAIN.
** End of FF

  MODULE user_command_0200.
  MODULE exit AT EXIT-COMMAND.

PROCESS ON VALUE-REQUEST.
  FIELD zaponta-pernr      MODULE busca_empregado. "SEL EMPREGADO.
  FIELD zaponta-mncod MODULE code_pf4_mncod.                "FF #96115
