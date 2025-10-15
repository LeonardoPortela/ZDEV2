"Name: \PR:J_1BECD_MAIN\FO:PROCESS_REG_I250\SE:BEGIN\EI
ENHANCEMENT 0 ZSPED_FISCAL_I050.
  FIELD-SYMBOLS: <bseg> TYPE tp_bseg.

  LOOP AT gt_bseg_aux ASSIGNING <bseg> where HKONT = '0000212200' .
    <bseg>-sgtxt = 'Compensação saldo Zero'.
    modify gt_bseg_aux from <bseg>.
  endloop.

ENDENHANCEMENT.
