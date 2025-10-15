"Name: \FU:ME_UPDATE_REQUISITION\SE:BEGIN\EI
ENHANCEMENT 0 ZMM_ENHANCEMENT_REQUISICAO.

LOOP AT xeban.
  IF xeban-prio_urg IS INITIAL AND xeban-zzprio_urg IS NOT INITIAL.
    xeban-prio_urg = xeban-zzprio_urg.
    MODIFY xeban.
  ENDIF.
ENDLOOP.
ENDENHANCEMENT.
