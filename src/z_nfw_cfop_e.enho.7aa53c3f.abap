"Name: \FU:J_1B_NF_CFOP_1_DETERMINATION\SE:END\EI
ENHANCEMENT 0 Z_NFW_CFOP_E.
*
  data VG_CFOP     type j_1bnflin-cfop.
  FIELD-SYMBOLS: <LIN> TYPE j_1bnflin.

  IF sy-cprog = 'ZWRJ0001'.
     ASSIGN ('(SAPLJ1BF)WA_NF_LIN') TO <LIN>.
     IF SY-SUBRC IS INITIAL.
       select SINGLE ZFIWRT0009~cfop
         from ZFIWRT0009
         INNER JOIN ZFIWRT0008
         on ZFIWRT0008~seq_lcto = ZFIWRT0009~seq_lcto
         and ZFIWRT0008~docnum = ' '
         and ZFIWRT0008~branch = <LIN>-WERKS
         And ZFIWRT0008~BUDAT  = sy-datum
         INNER JOIN ZFIWRT0001
         on ZFIWRT0001~operacao = ZFIWRT0008~operacao
         and ZFIWRT0001~LM_INDEA = 'S'
         into VG_CFOP
         where ZFIWRT0009~matnr eq <LIN>-matnr
         AND   ZFIWRT0009~charg eq <LIN>-charg
         and   ZFIWRT0009~cfop  ne ' '.
        IF sy-subrc = 0.
           cfop = VG_CFOP.
           CALL FUNCTION 'CONVERSION_EXIT_CFOBR_INPUT'
            EXPORTING
              input  = cfop
            IMPORTING
              output = cfop.
        ENDIF.
     Endif.
  Endif.

ENDENHANCEMENT.
