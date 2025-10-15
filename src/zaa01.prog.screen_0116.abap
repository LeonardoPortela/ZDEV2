
PROCESS BEFORE OUTPUT.
* MODULE STATUS_0110.
*
PROCESS AFTER INPUT.
  CHAIN.
    FIELD zaa_controle_hip-credor.
    FIELD zaa_controle_hip-grau.
    FIELD zaa_controle_hip-operacao.
    FIELD zaa_controle_hip-contrato.
    FIELD zaa_controle_hip-dmbtr.
    FIELD zaa_controle_hip-assinatura.
    FIELD zaa_controle_hip-waers.
    FIELD zaa_controle_hip-vencimento.
    MODULE controle ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE user_command_0116.
