
PROCESS BEFORE OUTPUT.

  MODULE status_2004.
*
PROCESS AFTER INPUT.
* MODULE USER_COMMAND_2004.

  CHAIN.
    FIELD zdoc_memorando-tp_transf.
    FIELD zdoc_memorando-bukrs.
    FIELD zdoc_memorando-branch.
    MODULE set_update_flag_memo ON CHAIN-REQUEST.
  ENDCHAIN.
