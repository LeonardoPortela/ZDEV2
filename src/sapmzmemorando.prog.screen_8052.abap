
PROCESS BEFORE OUTPUT.

  MODULE tab_vinc_memo_change_tc_attr.

  LOOP AT   it_memorandos
       WITH CONTROL tab_vinc_memo
       CURSOR tab_vinc_memo-current_line.
  ENDLOOP.

  MODULE tab_memo_vinc_change_tc_attr.

  LOOP AT   it_prot_memo
       WITH CONTROL tab_memo_vinc
       CURSOR tab_memo_vinc-current_line.
  ENDLOOP.

  MODULE status_8052.
*
PROCESS AFTER INPUT.

  LOOP AT it_memorandos.
    FIELD it_memorandos-mark
      MODULE tab_vinc_memo_mark ON REQUEST.
  ENDLOOP.

  LOOP AT it_prot_memo.
    FIELD it_prot_memo-mark
      MODULE tab_memo_vinc_mark ON REQUEST.
  ENDLOOP.
  MODULE tab_memo_vinc_user_command.

* MODULE USER_COMMAND_8052.
