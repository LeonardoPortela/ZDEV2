
PROCESS BEFORE OUTPUT.

  MODULE status_2005.
*
PROCESS AFTER INPUT.
* MODULE USER_COMMAND_2005.

  FIELD zdoc_memo_nf_exp-proprio MODULE zintercompany ON REQUEST.

  CHAIN.
    FIELD zdoc_memo_nf_exp-serie.
    FIELD zdoc_memo_nf_exp-numero_nota.
    FIELD zdoc_memo_nf_exp-nfe.
    FIELD zdoc_memo_nf_exp-emissor.
    FIELD zdoc_memo_nf_exp-dt_emissao_nota.
    FIELD terceiro.
    FIELD zdoc_memo_nf_exp-proprio.
    FIELD zdoc_memo_nf_exp-docnum.
    FIELD zdoc_memo_nf_exp-material.
    FIELD zdoc_memo_nf_exp-quantidade.
    FIELD zdoc_memo_nf_exp-unidade.
    FIELD zdoc_memo_nf_exp-moeda.
    FIELD zdoc_memo_nf_exp-valor_total.
    FIELD zdoc_memo_nf_exp-valor_unitario.
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.
