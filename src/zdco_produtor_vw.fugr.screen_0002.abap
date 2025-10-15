
PROCESS BEFORE OUTPUT.
  MODULE detail_init.
  MODULE zinicializar.
*  MODULE zgeranumero.
*
PROCESS AFTER INPUT.
  FIELD zdco_produtor-qt_material MODULE zverifica ON REQUEST.
  MODULE detail_exit_command AT EXIT-COMMAND.
  MODULE detail_set_pfstatus.
  CHAIN.
    FIELD zdco_produtor-nu_dco .
    FIELD zdco_produtor-nr_dco .
    FIELD zdco_produtor-dt_lancamento .
    FIELD zdco_produtor-qt_material .
    FIELD zdco_produtor-id_fornecedor .
    FIELD zdco_produtor-cd_material .
    FIELD zdco_produtor-nu_aviso .
    FIELD zdco_produtor-obs_dco .
    FIELD zdco_produtor-cd_centro .
    FIELD zdco_produtor-cd_safra .
    FIELD zdco_produtor-nu_cda .
    FIELD zdco_produtor-cd_tipo_leilao .
    FIELD zdco_produtor-vbeln .
    FIELD zdco_produtor-qt_entregue .
    FIELD zdco_produtor-qt_remessa .
    MODULE set_update_flag ON CHAIN-REQUEST.
  ENDCHAIN.
  CHAIN.
    FIELD zdco_produtor-nu_dco .
    MODULE detail_pai.
    MODULE zgeranumero.
  ENDCHAIN.

PROCESS ON VALUE-REQUEST.

  FIELD zdco_produtor-id_fornecedor
    MODULE z_match_id_fornecedor.

  FIELD zdco_produtor-cd_material
    MODULE z_match_cd_material.

  FIELD zdco_produtor-id_fornecedor
    MODULE z_match_cd_centro.

  FIELD zdco_produtor-cd_safra
    MODULE z_match_cd_safra.

  FIELD zdco_produtor-cd_tipo_leilao
     MODULE z_match_cd_tipo_leilao.

  FIELD zdco_produtor-vbeln
     MODULE z_match_doc_venda.
