"Name: \PR:SAPLSZA0\FO:RELATIONS_SINGLE_CHECK_REF\SE:BEGIN\EI
ENHANCEMENT 0 Z_CHECA_CPF_USER2.
*
  DATA: WA_ADCP  TYPE ADCP,
          WA_USR21 type USR21,
          IT_ADCP  TYPE  TABLE OF ADCP,
          W_cont   type i,
          w_mess(60).

"
      if sy-tcode = 'SU01' and SY-UCOMM ne 'YES' and SY-UCOMM ne 'OPT1'.
        if iadcp[] is not INITIAL.
            select *
              from ADCP
              into table it_adcp
              FOR ALL ENTRIES IN iadcp
              where FAX_NUMBER = iadcp-FAX_NUMBER
              and   FAX_NUMBER ne ''.

            READ TABLE  ixadcp  index 1.
            LOOP AT IT_ADCP into WA_ADCP.
               IF WA_ADCP-ADDRNUMBER NE iadcp-ADDRNUMBER or
                  WA_ADCP-PERSNUMBER NE iadcp-PERSNUMBER.
                  select SINGLE *
                    from USR21
                    into WA_USR21
                    where PERSNUMBER = WA_ADCP-PERSNUMBER
                    and   ADDRNUMBER = WA_ADCP-ADDRNUMBER.
                  if sy-subrc = 0.
                      CONCATENATE 'CPF ja cadastrado para' WA_USR21-BNAME into w_mess SEPARATED BY space.
                      MESSAGE  w_mess type 'E'.
                  endif.
              ENDIF.
            ENDLOOP.
         endif.
      endif.


ENDENHANCEMENT.
