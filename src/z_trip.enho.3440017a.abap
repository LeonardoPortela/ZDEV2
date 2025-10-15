"Name: \PR:SAPMFITP\FO:SET_CURSOR\SE:BEGIN\EI
ENHANCEMENT 0 Z_TRIP.
if sy-tcode = 'TP04'.
  toggle_status-cost_distribution = 'X'.
endif.
ENDENHANCEMENT.
