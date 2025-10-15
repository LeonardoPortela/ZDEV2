
PROCESS BEFORE OUTPUT.

  MODULE status_9006.
*
PROCESS AFTER INPUT.

  MODULE user_command_9006_exit AT EXIT-COMMAND.

  CHAIN.
    FIELD vg_seq1 MODULE z_check_code_seq1.
    FIELD vg_seq2 MODULE z_check_code_seq2.
    FIELD vg_seq3 MODULE z_check_code_seq3.
  ENDCHAIN.

  MODULE user_command_9006.

PROCESS ON VALUE-REQUEST.
  FIELD vg_seq1 MODULE z_mathcode_seq1.
  FIELD vg_seq2 MODULE z_mathcode_seq2.
  FIELD vg_seq3 MODULE z_mathcode_seq3.
