"Name: \PR:SAPLJ1BF\FO:CREATE_RELEVANCE_CHECK\SE:BEGIN\EI
ENHANCEMENT 0 Z_PEDIDO_FUTII.
*

 IF M_YMSEG-BWART = '801'.
     SELECT SINGLE *
      FROM ekko
      into @data(w_ekko)
     WHERE ebeln = @m_ymseg-ebeln.
    IF w_ekko-lifre is not INITIAL and w_ekko-llief is not INITIAL and ( w_ekko-lifre ne w_ekko-lifnr ).
       READ TABLE GT_NFITMRULE into GS_NFITMRULE with key itmtyp = '42'.
       if sy-subrc = 0.
          GS_NFITMRULE-TRDPARTY = c_on.
          MODIFY GT_NFITMRULE from GS_NFITMRULE INDEX sy-tabix TRANSPORTING TRDPARTY.
       endif.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
