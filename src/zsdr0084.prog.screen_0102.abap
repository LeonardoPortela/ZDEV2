PROCESS BEFORE OUTPUT.
  MODULE status_0102.

PROCESS AFTER INPUT.

  FIELD wa_zsdt0158_saida-kvgr4 MODULE zm_check_kvgr4 ON REQUEST.
  FIELD wa_zsdt0158_saida-kvgr5 MODULE zm_check_kvgr5 ON REQUEST.
  FIELD tvv3t-kvgr3 MODULE zm_check_kvgr3 ON REQUEST.
  FIELD wa_makt-matnr MODULE zm_check_kvgr3 ON REQUEST.

  CHAIN.
    FIELD wa_t001w-werks.
    FIELD wa_lfa1_pc-lifnr.
    FIELD wa_lfa1_pc-lzone.
    FIELD wa_kna1-kunnr.
    FIELD wa_kna1-lzone.
    FIELD wa_lfa1_z1-lifnr.
    FIELD wa_makt-matnr.
    FIELD wa_fornecedor-lifnr.
    FIELD wa_zsdt0158_saida-safra.
    FIELD wa_frete-ag_frete.
    FIELD tvv3t-kvgr3.
    FIELD tinct-inco1.
    FIELD wa_frete-exige_ag_frete.
    FIELD wa_zsdt0158_saida-kvgr4.
    FIELD wa_zsdt0158_saida-kvgr5.
    FIELD wa_zsdt0158_saida-ck_troca_nota.
    MODULE modify_forma_lote ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE user_command_0102.

PROCESS ON VALUE-REQUEST. "F4

  FIELD wa_saida-sequencial MODULE help_nro_sol_ov.
