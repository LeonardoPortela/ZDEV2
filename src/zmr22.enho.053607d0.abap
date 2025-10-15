"Name: \PR:GBT60FI0\FO:IMPORT_TABLE\SE:END\EI
ENHANCEMENT 0 ZMR22.

*break-point.

*if TABNAME = 'BSEG' and sy-tcode = 'MR22'.
*  if bseg-bschl = '83' or bseg-bschl = '93'.
*    bseg-hkont = '0000114997'.
*    endif.
*  endif.
ENDENHANCEMENT.
