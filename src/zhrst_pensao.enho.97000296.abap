"Name: \FU:BAPI_ACC_EMPLOYEE_PAY_POST\SE:BEGIN\EI
ENHANCEMENT 0 ZHRST_PENSAO.
*
   Data :T_LFA1  TYPE TABLE OF LFA1,
         T_LFA12 TYPE TABLE OF LFA1,
         W_LFA1  TYPE LFA1,
         WL_EXTENSION1 LIKE LINE OF EXTENSION1.

   IF SY-TCODE = 'PCP0' and  accountpayable[] is not INITIAL.
      SELECT *
        from lfa1
        into TABLE t_lfa1
        FOR ALL ENTRIES IN accountpayable
        where lifnr eq accountpayable-VENDOR_NO.

       T_LFA12[] = T_LFA1[].

       DELETE T_LFA1 WHERE SORTL+0(1) NE '7'.

       IF T_LFA1[] IS NOT INITIAL.
          WL_EXTENSION1-FIELD1 = 'PENSAO'.
          append WL_EXTENSION1 to EXTENSION1.
       else.
          IF T_LFA12[] IS NOT INITIAL.
             WL_EXTENSION1-FIELD1 = 'FORNEC'.
             append WL_EXTENSION1 to EXTENSION1.
          endif.
       endif.

   endif.
ENDENHANCEMENT.
