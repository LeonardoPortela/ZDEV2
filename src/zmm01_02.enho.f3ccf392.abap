"Name: \PR:SAPLMGMM\EX:LMGMMF40_01\EI
ENHANCEMENT 0 ZMM01_02.
       DATA: WA_ZMMT0005 TYPE ZMMT0005.

       IF NOT BILDTAB-PSTAT IS INITIAL.
         PERFORM BILDEN_VEREINIGUNGSMENGE
                 USING BILDTAB-PSTAT SELSTATUS.
       ENDIF.

       if SY-TCODE eq 'MM01'.

       if ( BILDTAB-GUIFU NE 'SP01' ) AND ( BILDTAB-GUIFU NE 'SP02' ).
         SELECT SINGLE * INTO WA_ZMMT0005
           FROM ZMMT0005
          WHERE USNAM EQ SY-UNAME
            AND TCODE EQ SY-TCODE.
*         IF SY-SUBRC IS INITIAL.
*           MESSAGE s899 with 'Usuário' SY-UNAME 'sem permissão para visão!'.
*           clear ok-code.
*           bildflag = x.
*           set screen sy-dynnr.
*           leave screen.
*         ENDIF.
       ENDIF.

       ENDIF.

ENDENHANCEMENT.
