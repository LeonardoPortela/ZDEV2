
process before output.
*&SPWIZARD: PBO FLOW LOGIC FOR TABLECONTROL 'TAB_PES_VEICULO'
  module tab_pes_veiculo_change_tc_attr.
*&SPWIZARD: MODULE TAB_PES_VEICULO_CHANGE_COL_ATTR.
  loop at   it_veiculo_tela
       with control tab_pes_veiculo
       cursor tab_pes_veiculo-current_line.
*&SPWIZARD:   MODULE TAB_PES_VEICULO_CHANGE_FIELD_ATTR
  endloop.

* MODULE STATUS_1002.
*
process after input.
*&SPWIZARD: PAI FLOW LOGIC FOR TABLECONTROL 'TAB_PES_VEICULO'
  loop at it_veiculo_tela.
    field it_veiculo_tela-mark module tab_pes_veiculo_mark on request.
"    field it_veiculo_tela-nm_fornec module tab_pes_veiculo_nm_fornec
"on request.

  endloop.
*&SPWIZARD: MODULE TAB_PES_VEICULO_CHANGE_TC_ATTR.
*&SPWIZARD: MODULE TAB_PES_VEICULO_CHANGE_COL_ATTR.

    module user_command_1002.
