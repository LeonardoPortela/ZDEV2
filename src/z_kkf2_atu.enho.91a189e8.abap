"Name: \FU:K_SETTLEMENT_RULE_CALL\SE:BEGIN\EI
ENHANCEMENT 0 Z_KKF2_ATU.
*
  FIELD-SYMBOLS: <AUTYP> TYPE ANY.
  if  sy-tcode EQ 'KKF2'.
     ASSIGN ('(SAPMKAUF)COAS-AUTYP') TO <AUTYP>.
     IF  <AUTYP> is  ASSIGNED.
        if  <AUTYP> = '05'.
          mode = 'UPDA'.
        endif.
     ENDIF.
  endif.
ENDENHANCEMENT.
