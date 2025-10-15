"Name: \PR:SAPLMBBS\FO:MDBS_PRUEFEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_MMBE_PED.
*
  SELECT SINGLE bsart
    into @data(_BSART)
    from ekko
    where ebeln = @MDBS-ebeln.
  IF _BSART = 'ZGR' or _BSART = 'ZGEF'.
     exit.
  ENDIF.
ENDENHANCEMENT.
