"Name: \PR:HBRTMIF0\TY:LCL_CC1TEV_MANAGER\ME:READ_CC1TEV_TABLE\SE:BEGIN\EI
ENHANCEMENT 0 ZHCM_HBRTMIF_CC1TEV.
*ALRS/CS
  LOOP AT it_timeevents into data(wa_timeevents).
      delete  from cc1tev
      WHERE pernr = wa_timeevents-pernr.
  ENDLOOP.


ENDENHANCEMENT.
