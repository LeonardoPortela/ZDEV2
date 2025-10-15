
PROCESS BEFORE OUTPUT.

  MODULE status_9007.
*
PROCESS AFTER INPUT.

  MODULE user_command_9007_exit AT EXIT-COMMAND.

  CHAIN.
    FIELD vg_emp1 MODULE z_check_code_emp1.
    FIELD vg_emp2 MODULE z_check_code_emp2.
    FIELD vg_emp3 MODULE z_check_code_emp3.
  ENDCHAIN.

  MODULE user_command_9007.

PROCESS ON VALUE-REQUEST.
  FIELD vg_emp1 MODULE z_mathcode_emp1.
  FIELD vg_emp2 MODULE z_mathcode_emp2.
  FIELD vg_emp3 MODULE z_mathcode_emp3.
