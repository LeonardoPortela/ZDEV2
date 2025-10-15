
PROCESS BEFORE OUTPUT.

  MODULE modifica_campos.

*&SPWIZARD: PBO FLOW LOGIC FOR TABLECONTROL 'TC_INV'
  MODULE tc_inv_change_tc_attr.
*&SPWIZARD: MODULE TC_INV_CHANGE_COL_ATTR.


  LOOP AT   t_inv
       WITH CONTROL tc_inv
       CURSOR tc_inv-current_line.
    MODULE tc_inv_get_lines.
    MODULE md_change_screen.
*&SPWIZARD:   MODULE TC_INV_CHANGE_FIELD_ATTR
  ENDLOOP.

  MODULE status_0110.

PROCESS AFTER INPUT.
*  MODULE m_sair AT EXIT-COMMAND.
*&SPWIZARD: PAI FLOW LOGIC FOR TABLECONTROL 'TC_INV'
  LOOP AT t_inv.
    CHAIN.
*      FIELD T_INV-BUZEI.
      FIELD t_inv-fase.
      FIELD t_inv-izwek.
      FIELD t_inv-objetivo.
      FIELD t_inv-descr_item.
      FIELD t_inv-menge.
      FIELD t_inv-vlr_unitario.
      FIELD t_inv-vlr_total.
      FIELD t_inv-moeda.
      FIELD t_inv-dt_inicio.
      FIELD t_inv-dt_fim.
      FIELD t_inv-ano_fim_exec.
      FIELD t_inv-status_cta.
      FIELD t_inv-knttp.
      FIELD t_inv-observacoes.
      FIELD t_inv-saknr.
      FIELD t_inv-txt20.
      FIELD t_inv-posnr.
      FIELD t_inv-status_aprov.
*      FIELD t_inv-dt_aprovacao.
*      FIELD t_inv-aprovador.
      FIELD t_inv-tx_usd.
      FIELD t_inv-tx_eur.
      FIELD T_INV-FINALIDADE.
      FIELD t_inv-cod_gpo  module valida_cod_gpo.
      FIELD t_inv-cod_item module valida_cod_item.
*      FIELD T_INV-USUARIO.
*      FIELD T_INV-DATA_ENTR.
*      FIELD T_INV-HORA_ENTR.
*      FIELD T_INV-DATA_MOD.
*      FIELD T_INV-HORA_MOD.

      MODULE tc_inv_modify ON CHAIN-REQUEST.
    ENDCHAIN.
    chain.
      field T_INV-cod_gpo.
      FIELD T_INV-COD_ITEM.
      FIELD t_inv-objetivo.
      FIELD t_inv-descr_item.
      FIELD t_inv-menge.
      FIELD t_inv-vlr_unitario.
      FIELD t_inv-vlr_total.
      FIELD t_inv-dt_inicio.
      FIELD t_inv-dt_fim.
      FIELD T_INV-IZWEK.
      FIELD t_inv-status_cta.
      FIELD T_INV-FINALIDADE.
      FIELD t_inv-knttp.
      MODULE tc_valida_campo ON CHAIN-REQUEST.
    endchain.
    FIELD t_inv-flag
      MODULE tc_inv_mark ON REQUEST.
    MODULE tc_cons_cat.
    MODULE tc_cons_cat.
      field T_INV-COD_GPO module valida_grupo on request.
  ENDLOOP.
  MODULE tc_inv_user_command.
*&SPWIZARD: MODULE TC_INV_CHANGE_TC_ATTR.
*&SPWIZARD: MODULE TC_INV_CHANGE_COL_ATTR.

  MODULE user_command_0110.

PROCESS ON VALUE-REQUEST.
  FIELD t_inv-cod_gpo  MODULE buscal_cod_gpo.
  FIELD t_inv-cod_item MODULE buscal_cod_item.
