
PROCESS BEFORE OUTPUT.
  MODULE status_2102.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD zcte_trans-pc_veiculo.
    FIELD zcte_trans-cd_cidade.
    FIELD zcte_trans-cd_uf.
    FIELD zcte_trans-agregado.
    FIELD zcte_trans-cd_renavam.
    FIELD zcte_trans-tp_veiculo.
    FIELD zcte_trans-tp_rodado.
    FIELD zcte_trans-tp_carroceria2.
    FIELD zcte_trans-tara.
    FIELD zcte_trans-cap_kg.
    FIELD zcte_trans-cap_m3.
    FIELD zcte_trans-proprietario.
    FIELD zcte_trans-prop_nome.
    FIELD zcte_trans-prop_rntrc.
    FIELD zcte_trans-prop_uf.
    FIELD zcte_trans-prop_ie.
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE user_command_2102.
