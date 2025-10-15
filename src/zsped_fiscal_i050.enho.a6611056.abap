"Name: \PR:J_1BECD_MAIN\FO:PROCESS_REG_I050\SE:END\EI
ENHANCEMENT 0 ZSPED_FISCAL_I050.
if ls_i050-dt_alt is initial or
   ls_i050-dt_alt eq '00000000'.
*  read table gt_i050 into ls_i050 index 1.
  data: w_erdat type skb1-erdat,
        w_saknr type skb1-saknr.

  w_saknr = ls_i050-COD_CTA(10).
  select single erdat from skb1
    into w_erdat
    where bukrs = p_bukrs and
          saknr = w_saknr.

  ls_i050-dt_alt = w_erdat.
  modify gt_i050  from ls_i050 index 1.

  endif.
ENDENHANCEMENT.
