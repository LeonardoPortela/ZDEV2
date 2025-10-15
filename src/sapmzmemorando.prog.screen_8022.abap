
PROCESS BEFORE OUTPUT.

  MODULE status_8022.
*
PROCESS AFTER INPUT.
* MODULE USER_COMMAND_8022.

  CHAIN.
    FIELD zdoc_memo_protoc-dt_protocolo.
    FIELD zdoc_memo_protoc-dt_recibo MODULE valida_dt_recebimento.
    FIELD zdoc_memo_protoc-cd_emissor.
    FIELD zdoc_memo_protoc-empresa_em.
    FIELD zdoc_memo_protoc-filial_em.
    FIELD zdoc_memo_protoc-cd_recebedor.
    FIELD zdoc_memo_protoc-empresa_de.
    FIELD zdoc_memo_protoc-filial_de.
    MODULE set_update_protoc ON CHAIN-REQUEST.
  ENDCHAIN.
