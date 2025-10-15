
PROCESS BEFORE OUTPUT.

  MODULE status_9002.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD zfit181-bukrs MODULE z_check_code_emp_9002 ON INPUT .
  ENDCHAIN.

  MODULE user_command_9002_exit AT EXIT-COMMAND.

  MODULE user_command_9002.

PROCESS ON VALUE-REQUEST.
  FIELD zfit181-bukrs MODULE z_matchcode_emp.
