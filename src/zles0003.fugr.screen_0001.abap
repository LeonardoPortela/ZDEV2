
PROCESS BEFORE OUTPUT.
  MODULE status_0001.
*
PROCESS AFTER INPUT.
  MODULE user_command_0001_exit AT EXIT-COMMAND.

  CHAIN.
    FIELD zpfe_lote-nm_lote.
    FIELD zpfe_lote-dt_posicao.
    FIELD zpfe_lote-dt_leitura.
    FIELD zpfe_lote-hr_leitura.
    FIELD zpfe_lote-cd_adiministra.
    FIELD zpfe_lote-bukrs.
    FIELD zpfe_lote-branch.
    FIELD zpfe_lote-vl_total_lote.
    FIELD zpfe_lote-dt_vencimento.
    FIELD zpfe_lote-belnr.
    FIELD zpfe_lote-gjahr.
    FIELD zpfe_lote-dt_belnr.
    FIELD zpfe_lote-hr_belnr.
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.
