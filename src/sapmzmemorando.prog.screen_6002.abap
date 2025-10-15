
PROCESS BEFORE OUTPUT.

  MODULE status_6002.

  MODULE tab_nf_livres_s_change_tc_attr.
  LOOP AT   it_nf_livre
       WITH CONTROL tab_nf_livres_s
       CURSOR tab_nf_livres_s-current_line.
    "MODULE livres_change_field_attr.
  ENDLOOP.

  MODULE tab_nf_vincu_s_change_tc_attr.
  LOOP AT   it_nf_vincu_s
       WITH CONTROL tab_nf_vincu_s
       CURSOR tab_nf_vincu_s-current_line.
    "MODULE vincu_change_field_attr.
  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP AT it_nf_livre.
    FIELD it_nf_livre-mark
      MODULE tab_nf_livres_s_mark ON REQUEST.
  ENDLOOP.

  MODULE tab_nf_livres_s_user_command.

  LOOP AT it_nf_vincu_s.
    FIELD it_nf_vincu_s-mark
      MODULE tab_nf_vincu_s_mark ON REQUEST.
  ENDLOOP.

  MODULE tab_nf_vincu_s_user_command.

* MODULE USER_COMMAND_6002.
