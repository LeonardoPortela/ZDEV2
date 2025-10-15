
PROCESS BEFORE OUTPUT.

  MODULE zm_status.

  MODULE zm_text.

  LOOP AT t_nfs_m INTO s_nfs WITH CONTROL tc_memo
    CURSOR tc_memo-current_line.
  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP AT t_nfs_m.
    FIELD: s_nfs-marc MODULE zm_nfs_marc ON REQUEST,
           s_nfs-qtd  MODULE zm_nfs_qtd  ON REQUEST.
  ENDLOOP.

  FIELD s_memo-num_memo  MODULE zm_nfs_memo ON REQUEST.

  MODULE zm_user_command.

  MODULE zm_exit_command AT EXIT-COMMAND.
