FUNCTION-POOL ZMB_BUS2017 MESSAGE-ID M7.

* Data Dictionary Tabs  ***********************************************
TABLES: M_MAT1N.
TABLES: T158G.

* Table definition      ***********************************************
DATA: T_IMSEG        LIKE STANDARD TABLE OF IMSEG WITH HEADER LINE.
DATA: T_EMSEG        LIKE STANDARD TABLE OF EMSEG WITH HEADER LINE.
DATA: T_ISERI        LIKE STANDARD TABLE OF ISERI WITH HEADER LINE.
DATA: T_IMSEG_CANCEL LIKE STANDARD TABLE OF
                                   IMSEG_CANCEL   WITH HEADER LINE.
DATA: S_IMKPF        LIKE STANDARD TABLE OF IMKPF WITH HEADER LINE.
DATA: S_EMKPF        LIKE STANDARD TABLE OF EMKPF WITH HEADER LINE.
DATA: S_IPKCOM       LIKE STANDARD TABLE OF PKCOM WITH HEADER LINE.

* EAN relevant          ***********************************************
DATA: EAN_UPC   LIKE DM07M-EAN11,
      EAN_ENTRY TYPE I.

DATA: LOC_TAB  LIKE SY-TABIX.

* Global Data fields    ***********************************************
DATA: GLOBAL_ERROR TYPE C.
DATA: CURRENCY LIKE MSEG-WAERS.
DATA: F_TESTRUN(1) type c.

DATA: RET_PARAMETER LIKE BAPIRET2-PARAMETER VALUE ' ',
      RET_ROW       LIKE BAPIRET2-ROW VALUE 0,
      RET_FIELD     LIKE BAPIRET2-FIELD VALUE ' '.

* Global Constants      ***********************************************
CONSTANTS: TRUE    TYPE C VALUE 'X',
           FALSE   TYPE C VALUE ' '.

CONSTANTS: L_BLANK_MSGV LIKE SY-MSGV1 VALUE ' '.

CONSTANTS: X     VALUE 'X',
           BLANK VALUE ' '.
