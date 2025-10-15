"Name: \PR:RM07MLBS\FO:FIELDCATALOG\SE:END\EI
ENHANCEMENT 0 Z_N_POSICAO_MAT_MARD.
IF  xmchb is initial.
    fieldcat-fieldname     = 'LGPBE'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'MARD'.
    APPEND fieldcat.
  ENDIF.
ENDENHANCEMENT.
