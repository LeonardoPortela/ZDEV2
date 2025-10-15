
PROCESS BEFORE OUTPUT.
*&SPWIZARD: PBO FLOW LOGIC FOR TABLECONTROL 'TAB_LOSG'
  MODULE tab_losg_change_tc_attr.
*&SPWIZARD: MODULE TAB_LOSG_CHANGE_COL_ATTR.
  LOOP AT   t_messtab2
       WITH CONTROL tab_losg
       CURSOR tab_losg-current_line.
*&SPWIZARD:   MODULE TAB_LOSG_CHANGE_FIELD_ATTR
  ENDLOOP.

  MODULE status_0001.
*
PROCESS AFTER INPUT.
*&SPWIZARD: PAI FLOW LOGIC FOR TABLECONTROL 'TAB_LOSG'
  LOOP AT t_messtab2.
    CHAIN.
      FIELD t_messtab2-tcode.
      FIELD t_messtab2-dyname.
      FIELD t_messtab2-dynumb.
      FIELD t_messtab2-msgspra.
      FIELD t_messtab2-msgid.
      FIELD t_messtab2-msgnr.
      FIELD t_messtab2-msgv1.
      FIELD t_messtab2-msgv2.
      FIELD t_messtab2-msgv3.
      FIELD t_messtab2-msgv4.
      FIELD t_messtab2-icone.
    ENDCHAIN.
  ENDLOOP.
*&SPWIZARD: MODULE TAB_LOSG_CHANGE_TC_ATTR.
*&SPWIZARD: MODULE TAB_LOSG_CHANGE_COL_ATTR.

  MODULE user_command_0001.
