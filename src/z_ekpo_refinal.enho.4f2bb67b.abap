"Name: \FU:ME_EKPO_READ_WITH_EBELN\SE:END\EI
ENHANCEMENT 0 Z_EKPO_REFINAL.
* REMESSA FINAL não processa
" exceto argentino e paraguai
  data WA_EKPO type EKPO.
  CLEAR  WA_EKPO.
  if GT_EKPO[] is not INITIAL.
      READ TABLE GT_EKPO into wa_ekpo INDEX 1.
  endif.
  IF  ( sy-tcode = 'MIGO' or sy-tcode = 'MIRO' ) and WA_EKPO-bukrs ne '0100' and WA_EKPO-bukrs ne '0101' .
      delete GT_EKPO  where ELIKZ = 'X'.
      delete PTO_EKPO where ELIKZ = 'X'.
  ENDIF.
ENDENHANCEMENT.
