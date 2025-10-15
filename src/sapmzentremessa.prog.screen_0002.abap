
PROCESS BEFORE OUTPUT.

*&SPWIZARD: PBO FLOW LOGIC FOR TABLECONTROL 'TAB_NOTAS_LIVRE'
  MODULE tab_notas_livre_change_tc_attr.
*&SPWIZARD: MODULE TAB_NOTAS_LIVRE_CHANGE_COL_ATTR.
  LOOP AT   it_notas
       WITH CONTROL tab_notas_livre
       CURSOR tab_notas_livre-current_line.
*&SPWIZARD:   MODULE TAB_NOTAS_LIVRE_CHANGE_FIELD_ATTR
  ENDLOOP.

*&SPWIZARD: PBO FLOW LOGIC FOR TABLECONTROL 'TAB_NOTAS_VINC'
  MODULE tab_notas_vinc_change_tc_attr.
*&SPWIZARD: MODULE TAB_NOTAS_VINC_CHANGE_COL_ATTR.
  LOOP AT   it_vinc
       WITH CONTROL tab_notas_vinc
       CURSOR tab_notas_vinc-current_line.
*&SPWIZARD:   MODULE TAB_NOTAS_VINC_CHANGE_FIELD_ATTR
  ENDLOOP.

* MODULE STATUS_0002.
*
PROCESS AFTER INPUT.

*&SPWIZARD: PAI FLOW LOGIC FOR TABLECONTROL 'TAB_NOTAS_LIVRE'
  LOOP AT it_notas.
    CHAIN.
      FIELD it_notas-docnum.
      FIELD it_notas-docdat.
      FIELD it_notas-series.
      FIELD it_notas-bukrs.
      FIELD it_notas-branch.
      FIELD it_notas-nfenum.
      FIELD it_notas-vbeln.
      FIELD it_notas-lifnr.
      FIELD it_notas-terminal.
      FIELD it_notas-werks_v.
      FIELD it_notas-lgort.
      FIELD it_notas-vbkd_inco1.
    ENDCHAIN.
    FIELD it_notas-marc
      MODULE tab_notas_livre_mark ON REQUEST.
  ENDLOOP.
*&SPWIZARD: MODULE TAB_NOTAS_LIVRE_CHANGE_TC_ATTR.
*&SPWIZARD: MODULE TAB_NOTAS_LIVRE_CHANGE_COL_ATTR.

*&SPWIZARD: PAI FLOW LOGIC FOR TABLECONTROL 'TAB_NOTAS_VINC'
  LOOP AT it_vinc.
    CHAIN.
      FIELD it_vinc-icone.
      FIELD it_vinc-docnum.
      FIELD it_vinc-docdat.
      FIELD it_vinc-series.
      FIELD it_vinc-bukrs.
      FIELD it_vinc-branch.
      FIELD it_vinc-nfenum.
      FIELD it_vinc-vbeln.
      FIELD it_vinc-lifnr.
      FIELD it_vinc-dt_chegada.
      FIELD it_vinc-qt_chegada.
      FIELD it_vinc-terminal.
      FIELD it_vinc-werks_v.
      FIELD it_vinc-lgort.
    ENDCHAIN.
    FIELD it_vinc-dt_chegada MODULE altera_it_vin ON REQUEST.
    FIELD it_vinc-qt_chegada MODULE altera_it_vin ON REQUEST.
    FIELD it_vinc-marc       MODULE tab_notas_vinc_mark ON REQUEST.
  ENDLOOP.
*&SPWIZARD: MODULE TAB_NOTAS_VINC_CHANGE_TC_ATTR.
*&SPWIZARD: MODULE TAB_NOTAS_VINC_CHANGE_COL_ATTR.

  MODULE user_command_0002.
