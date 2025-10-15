
PROCESS BEFORE OUTPUT.

  MODULE status_4002.

  MODULE tab_nf_livres_change_tc_attr.
  LOOP AT   it_nf_livre
       WITH CONTROL tab_nf_livres
       CURSOR tab_nf_livres-current_line.
    MODULE livres_change_field_attr.
  ENDLOOP.

  MODULE tab_nf_vincu_change_tc_attr.
  LOOP AT   it_nf_vincu
       WITH CONTROL tab_nf_vincu
       CURSOR tab_nf_vincu-current_line.
    MODULE vincu_change_field_attr.
  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP AT it_nf_livre.
    FIELD it_nf_livre-mark
      MODULE tab_nf_livres_mark ON REQUEST.
  ENDLOOP.

  MODULE tab_nf_livres_user_command.

  LOOP AT it_nf_vincu.
    FIELD it_nf_vincu-mark
      MODULE tab_nf_vincu_mark ON REQUEST.
  ENDLOOP.

  MODULE tab_nf_vincu_user_command.
