
PROCESS BEFORE OUTPUT.
  MODULE: pbo_0150.
*
PROCESS AFTER INPUT.
**<<<------"164255 - NMS - INI------>>>
  CHAIN.
    FIELD wl_cabecalho_0150-seq_tipo.
    MODULE zm_fill_tipo ON CHAIN-REQUEST.
  ENDCHAIN.
**<<<------"164255 - NMS - FIM------>>>
  MODULE: pai_0150.

PROCESS ON VALUE-REQUEST.
  FIELD wl_cabecalho_0150-bukrs     MODULE help_bukrs.
  FIELD wl_cabecalho_0150-seq_lcto  MODULE help_seq_lcto.
