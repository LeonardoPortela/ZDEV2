"Name: \FU:J_1B_NFE_UPDATE_ACTIVE\SE:END\EI
ENHANCEMENT 0 ZENVIO_EMAIL_NFE.
*

  call function 'Z_GRC_SEND_EMAIL_AUTO'
    exporting
      i_acttab = ls_acttab.


ENDENHANCEMENT.
