"Name: \TY:CL_HRPAY99_POSTING_ENGINE\ME:CREATE_EP_LINE\SE:END\EI
ENHANCEMENT 0 Z_DISTR_PENSAO.
*
  FIELD-SYMBOLS  <BT>  TYPE any.
  FIELD-SYMBOLS  <BETRG>  TYPE BETRG.
  FIELD-SYMBOLS  <BTZNR>  TYPE BTZNR.
  "
  data(Wlines) = lines( IR_PAR->INTER-BT ).
  DATA WA_BT type PC209.
  if is_dist-lgart+0(2) = 'MG'.
    IF Wlines gt 1.
      LOOP AT IR_PAR->INTER-BT ASSIGNING <BT>.
        MOVE-CORRESPONDING <BT> to wa_bt.
        IF ABS( wa_bt-BETRG ) = ABS( es_ep-betrg ).
           es_ep-MOMAG = is_dist-V0ZNR.
           exit.
        ENDIF.
      ENDLOOP.
    ENDIF.
  Endif.
ENDENHANCEMENT.
