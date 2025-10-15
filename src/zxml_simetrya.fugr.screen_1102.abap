
PROCESS BEFORE OUTPUT.
  MODULE status_1102.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD zcte_info_nota-nfe.
    FIELD zcte_info_nota-modelo.
    FIELD zcte_info_nota-serie.
    FIELD zcte_info_nota-numero.
    FIELD zcte_info_nota-cliente.
    FIELD zcte_info_nota-partyp.
    FIELD zcte_info_nota-dtemissao.
    FIELD zcte_info_nota-cfop.
    FIELD zcte_info_nota-vl_bc.
    FIELD zcte_info_nota-vl_icms.
    FIELD zcte_info_nota-vl_bc_st.
    FIELD zcte_info_nota-vl_st.
    FIELD zcte_info_nota-vl_produtos.
    FIELD zcte_info_nota-vl_nota_fiscal.
    FIELD zcte_info_nota-material.
    FIELD zcte_info_nota-unidade.
    FIELD zcte_info_nota-quantidade.
    FIELD zcte_info_nota-pin_suframa.
    FIELD zcte_info_nota-docnum9.
    FIELD zcte_info_nota-cdv.
    FIELD zcte_info_nota-peso_fiscal.
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE user_command_1102.
