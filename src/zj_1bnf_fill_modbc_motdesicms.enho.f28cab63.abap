"Name: \FU:J_1BNF_FILL_MODBC_MOTDESICMS\SE:END\EI
ENHANCEMENT 0 ZJ_1BNF_FILL_MODBC_MOTDESICMS.
*
  LOOP AT it_nfstx INTO ls_nfstx
    WHERE itmnum = ls_nflin-itmnum.

    CASE ls_nfstx-taxgrp.
      WHEN 'ICMS'.
        IF lv_taxsit = '30'.
          IF cs_nflin-modbcst IS INITIAL.
            cs_nflin-modbcst = '0'.
          ENDIF.
        endif.
      WHEN 'ICST'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.


ENDENHANCEMENT.
