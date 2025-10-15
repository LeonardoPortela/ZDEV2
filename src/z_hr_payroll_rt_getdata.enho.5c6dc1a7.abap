"Name: \PR:HBRSEF00\TY:LCL_PAYROLL_RT\ME:GET_AMOUNT\SE:BEGIN\EI
ENHANCEMENT 0 Z_HR_PAYROLL_RT_GETDATA.
*
  if me->VAR_OCRSN ne '' and iv_lgart = '/142'.
     CLEAR rv_amount.
     CLEAR me->VAR_OCRSN.
     exit.
  endif.

ENDENHANCEMENT.
