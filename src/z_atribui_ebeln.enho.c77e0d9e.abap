"Name: \TY:CL_WRF_PREPAYMENT\ME:XACC_LINES_PREPARE\SE:BEGIN\EI
ENHANCEMENT 0 Z_ATRIBUI_EBELN.
*
  if ( sy-tcode(3) = 'ZMM' ) or ( sy-tcode = 'MIRO' ) or ( sy-tcode is INITIAL ).
      LOOP AT CHT_ACCIT into data(WACCIT).
        IF WACCIT-lifnr is INITIAL and WACCIT-ebeln is not INITIAL.
            exit.
        ENDIF.
      ENDLOOP.
      if WACCIT-ebeln is not INITIAL.
         LOOP AT CHT_ACCIT into data(WACCIT2).
           IF WACCIT2-lifnr is not INITIAL.
               modify CHT_ACCIT FROM WACCIT INDEX sy-tabix TRANSPORTING ebeln.
           ENDIF.
         ENDLOOP.
      endif.
  endif.

ENDENHANCEMENT.
