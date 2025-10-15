
PROCESS BEFORE OUTPUT.

  MODULE status_9005.
*
PROCESS AFTER INPUT.

*  CHAIN.
*    FIELD vg_emp1 MODULE z_check_code_emp1.
*  ENDCHAIN.

  MODULE user_command_9005_exit AT EXIT-COMMAND.

  MODULE user_command_9005.

PROCESS ON VALUE-REQUEST.
*  FIELD vg_emp1 MODULE z_mathcode_emp_9005. "Empresa
  FIELD zfit182-cod_estrutura MODULE z_mathcode_cod_estrutura_9005.
  "Estrutura
