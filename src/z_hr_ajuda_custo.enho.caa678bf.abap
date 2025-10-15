"Name: \PR:HBRCDTA0\FO:LAST-ACT-WPBP\SE:BEGIN\EI
ENHANCEMENT 0 Z_HR_AJUDA_CUSTO.
*
  data:  CL_READ_PAYROLL TYPE REF TO CL_HR_BR_READ_PAYROLL,
         TL_RGDIR_AUX    TYPE TABLE OF PC261,
         WL_RGDIR        TYPE PC261,
         TL_PAYROLL      TYPE TABLE OF PAYBR_RESULT,
         WL_PAYROLL      TYPE          PAYBR_RESULT,
         TL_RT           TYPE TABLE OF PC207 with HEADER LINE,
         WL_RT           TYPE PC207,
         IV_PERNR        type PERNR_D.

  IF bt[] is INITIAL and pernr-pernr = '70000854'.
     refresh TL_RGDIR_AUX.
     MOVE-CORRESPONDING evp_rgdir to WL_RGDIR.
     APPEND WL_RGDIR TO TL_RGDIR_AUX.

    CLEAR: CL_READ_PAYROLL.
    IV_PERNR = pernr-pernr.
    CREATE OBJECT CL_READ_PAYROLL
      EXPORTING
        IV_PERNR = IV_PERNR.

     REFRESH TL_PAYROLL.
     CALL METHOD CL_READ_PAYROLL->GET_PAY_RESULT_TABLE
       EXPORTING
         IT_RGDIR        = TL_RGDIR_AUX[]
       IMPORTING
         ET_PAYBR_RESULT = TL_PAYROLL.

     REFRESH: TL_RT.

     CLEAR: WL_PAYROLL.
     LOOP AT TL_PAYROLL INTO WL_PAYROLL.
        APPEND LINES OF WL_PAYROLL-INTER-RT TO TL_RT.
     ENDLOOP.
     DELETE TL_RT where LGART ne '/559'.
     LOOP AT TL_RT.
       MOVE-CORRESPONDING TL_RT to BT.
       select SINGLE BANKS BANKL BANKN
         into  ( BT-BANKS, BT-BANKL, BT-BANKN )
         FROM PA0009
         where pernr = pernr-pernr
         and   begda LE WL_RGDIR-PAYDT
         and   endda GE WL_RGDIR-PAYDT.
         bt-ZLSCH = 'H'.
         bt-DTADT = '0'.
         bt-DTATI = '0'.
         bt-BTTYP = '0'.
         bt-WAERS = 'BRL'.
         bt-FBETR = 0.
         bt-DD_DUEDATE_SEPA = '0'.
         APPEND bt.

     ENDLOOP.

          "
  ENDIF.
ENDENHANCEMENT.
