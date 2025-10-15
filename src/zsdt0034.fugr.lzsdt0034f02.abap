*----------------------------------------------------------------------*
***INCLUDE LZSDT0034F02 .
*----------------------------------------------------------------------*

FORM z_gera_seq_chave.

  DATA: st_table TYPE zsdt0034-numseqn.

  CHECK: zsdt0034-numseqn IS INITIAL.

  SELECT MAX( numseqn ) INTO st_table FROM zsdt0034.

  zsdt0034-numseqn = st_table + 1.

ENDFORM.                    "Z_GERA_SEQ_CHAVE
