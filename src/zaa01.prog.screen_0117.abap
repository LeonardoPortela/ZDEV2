
PROCESS BEFORE OUTPUT.
*&SPWIZARD: PBO FLOW LOGIC FOR TABLECONTROL 'TC_HIP_EDIT'
  MODULE tc_hip_edit_change_tc_attr.
*&SPWIZARD: MODULE TC_HIP_EDIT_CHANGE_COL_ATTR.
  LOOP AT   t_hip
       WITH CONTROL tc_hip_edit
       CURSOR tc_hip_edit-current_line.
    MODULE tc_hip_edit_get_lines.
*&SPWIZARD:   MODULE TC_HIP_EDIT_CHANGE_FIELD_ATTR
  ENDLOOP.

* MODULE STATUS_0110.
*
PROCESS AFTER INPUT.
*&SPWIZARD: PAI FLOW LOGIC FOR TABLECONTROL 'TC_HIP_EDIT'
  LOOP AT t_hip.
    CHAIN.
      FIELD t_hip-credor.
      FIELD t_hip-grau.
      FIELD t_hip-operacao.
      FIELD t_hip-dmbtr.
      FIELD t_hip-waers.
      FIELD t_hip-contrato.
      FIELD t_hip-vencimento.
      FIELD t_hip-assinatura.
      MODULE tc_hip_edit_modify ON CHAIN-REQUEST.
      MODULE controle ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD t_hip-flag
      MODULE tc_hip_edit_mark ON REQUEST.
  ENDLOOP.
  MODULE tc_hip_edit_user_command.
*&SPWIZARD: MODULE TC_HIP_EDIT_CHANGE_TC_ATTR.
*&SPWIZARD: MODULE TC_HIP_EDIT_CHANGE_COL_ATTR.

* MODULE USER_COMMAND_0110.
