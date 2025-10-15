"Name: \PR:RM07MLBS\FO:DATA_SELECTION_NEW\SE:END\EI
ENHANCEMENT 0 ZMB52_POSI.
*
  LOOP AT bestand.
    SELECT SINGLE *
      INTO @DATA(wmard)
      FROM mard
      WHERE matnr = @bestand-matnr
      AND   werks = @bestand-werks
      AND   lgort = @bestand-lgort.
    IF sy-subrc = 0 AND wmard-lgpbe IS NOT INITIAL.
      bestand-lgpbe = wmard-lgpbe.
      MODIFY bestand INDEX sy-tabix TRANSPORTING lgpbe.
    ENDIF.
  ENDLOOP.

ENDENHANCEMENT.
