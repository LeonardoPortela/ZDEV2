PROCESS BEFORE OUTPUT.
  MODULE pbo_0100.

PROCESS AFTER INPUT.
  CHAIN.
*    FIELD V_ORDEM MODULE GET_ORDEM ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN. "Caso digite algo no campo é chamado este module.
    FIELD zepm_aponta_cat_notas-oteil.
    MODULE code_pf4_oteil ON CHAIN-REQUEST.

    FIELD zepm_aponta_cat_notas-fecod.
    MODULE code_pf4_fecod ON CHAIN-REQUEST.

    FIELD zepm_aponta_cat_notas-urcod.
    MODULE code_pf4_urcod ON CHAIN-REQUEST.

    FIELD v_ordem.
    MODULE limpar_campos ON CHAIN-INPUT.

    FIELD v_vornr.
    MODULE atualiza_dados ON CHAIN-INPUT.


  ENDCHAIN.

  MODULE pai_0100.

PROCESS ON VALUE-REQUEST.

  FIELD zepm_aponta_cat_notas-oteil MODULE code_pf4_oteil.
  FIELD zepm_aponta_cat_notas-fecod MODULE code_pf4_fecod.
  FIELD zepm_aponta_cat_notas-urcod MODULE code_pf4_urcod.
