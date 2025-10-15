PROCESS BEFORE OUTPUT.

  MODULE cria_objetos.
  MODULE status_0100.
  CALL SUBSCREEN sub01 INCLUDING sy-repid wg_sub01.
  MODULE modify_screen.

PROCESS AFTER INPUT.

  FIELD wg_header-vkbur MODULE refresh_vkbur ON CHAIN-REQUEST.

  CHAIN.
    FIELD wg_header-vkorg.
    FIELD wg_header-kunnr.
    MODULE refresh_posicao ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD wg_header-tp_venda.
    MODULE valida_tp_venda ON CHAIN-REQUEST.
  ENDCHAIN.

  FIELD wg_header-tp_venda MODULE refresh_preco ON REQUEST.
  FIELD wg_header-num_fixacao MODULE refresh_preco_frame ON REQUEST.

  CHAIN.
    FIELD wg_header-inco1.
    FIELD wg_header-dtde_logist.
    FIELD wg_header-dtate_logist.
    MODULE refresh_logistica ON CHAIN-REQUEST.
  ENDCHAIN.

  FIELD wg_header-data_venda MODULE save_dtvenda ON REQUEST.

*-CS2022000332-#78223-02.06.2022-JT-inicio
  FIELD wg_header-bstkd       MODULE check_contrato    ON REQUEST.
  FIELD wg_header-id_contrato MODULE check_id_contrato ON REQUEST.
*-CS2022000332-#78223-02.06.2022-JT-inicio

  " 18.12.2023 - 128467 - RBL -->
  FIELD wg_header-porto MODULE check_porto ON REQUEST.
  " 18.12.2023 - 128467 - RBL --<

  CALL SUBSCREEN sub01.
  MODULE get_cursor.
  MODULE user_command_0100.

PROCESS ON VALUE-REQUEST.
  FIELD wg_header-vkgrp MODULE search_vkgrp.
  FIELD wg_header-tp_venda    MODULE search_venda.
  FIELD wg_header-nro_sol_ov  MODULE f4_nro_sol_ov.
*-CS2022000332-#78223-02.06.2022-JT-inicio
  FIELD wg_header-id_contrato MODULE f4_id_contrato.
  FIELD wg_header-porto MODULE f4_id_porto.
*-CS2022000332-#78223-02.06.2022-JT-fim
