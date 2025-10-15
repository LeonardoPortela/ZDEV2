*&---------------------------------------------------------------------*
*&  Include           ZXVVFU04
*&---------------------------------------------------------------------*

* MOD 12/10/2012 ARG Proyecto Lexp - Christian INI
*IF XKOMV-KSCHL = 'YLIQ'.
*  XACCIT-MWSKZ = 'S1'.
*  XACCIT-KTOSL = 'J1I'.
*ENDIF.

* MOD 12/10/2012 ARG Proyecto Lexp - Christian FIN

 "Corrigir o modulo SD onde esta gerando os faturamentos intercompany sem SOC PARC
 " 05.09.2014 ALRS
 DATA: T_HKONT TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.
 IF XACCIT-BUKRS = '0100'.
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       CLASS         = '0000'
       SETNR         = 'CONTAS_EC-CS'
     TABLES
       SET_VALUES    = T_HKONT
     EXCEPTIONS
       SET_NOT_FOUND = 1
       OTHERS        = 2.
   IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
   READ TABLE T_HKONT WITH KEY FROM = XACCIT-HKONT.
   IF SY-SUBRC = 0. " conta tem que ter soc. parceira
     XACCIT-VBUND = VBRK-VBUND.
   ENDIF.
 ENDIF.
