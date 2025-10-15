"Name: \PR:RM07MLBS\FO:F0300_FIELDCAT_FLAT\SE:END\EI
ENHANCEMENT 0 Z_N_POSICAO_MAT_MARD.
IF  xmchb is initial.
  fieldcat-seltext_l     = 'Posição Nº'.
  fieldcat-outputlen     = 9.
macro_fill_fieldcat 'LGPBE'  'MARD'  c_out.
ENDIF.
ENDENHANCEMENT.
