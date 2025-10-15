"Name: \PR:RM07MLBS\FO:DATA_SELECTION\SE:END\EI
ENHANCEMENT 0 Z_N_POSICAO_MAT_MARD.
if xmchb is INITIAL.
  data V_LGPBE TYPE mard-LGPBE.
  DATA: dbcon  TYPE dbcon_name.

loop at bestand.
    SELECT LGPBE CONNECTION dbcon
      FROM mard
         INTO V_LGPBE
           WHERE matnr = bestand-matnr
             AND werks = bestand-werks
             AND lgort = bestand-lgort.
      endselect.

      bestand-lgpbe = V_LGPBE.
MODIFY bestand.
      endloop.
  endif.
ENDENHANCEMENT.
