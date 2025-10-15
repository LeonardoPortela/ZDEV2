
PROCESS BEFORE OUTPUT.

  MODULE status_2004.

  MODULE visibilidade_2004.

PROCESS AFTER INPUT.

  MODULE user_command_2004_exit AT EXIT-COMMAND.

  CHAIN.
    FIELD znota_import_ad-nr_adicao.
    FIELD znota_import_ad-nr_seq_adicao.
    FIELD znota_import_ad-cfabricante.
    FIELD znota_import_ad-vlr_desconto.
    FIELD znota_import_ad-nr_ped_compra.
    FIELD znota_import_ad-nr_ped_compra_it.
    FIELD znota_import_ad-nr_drawback.
  ENDCHAIN.

  MODULE user_command_2004.
