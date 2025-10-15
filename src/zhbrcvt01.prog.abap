*----------------------------------------------------------------------*
*   INCLUDE HBRCVT01        " General data declaration
*----------------------------------------------------------------------*

TABLES: PERNR,
        T7BRTR,
        T7BRTP,
        Q0410,
        T001P,
        T500P,
        T500C,
        T7BR1B,
        t503,
        hbrxxxx0.

INFOTYPES: 0000,                       "Events
           0001,                       "Organizational Assignment
           0002,                       "Personal Data
           0007,                       "Planned Working Time
           0008,                       "Basic Pay
           0015,                       "Additional Payments
           0057,                       "Membership Fees
           0465,                       "Documentos
           0410,                       "Vale Transporte
           2001,                       "Absences
           2002,                       "Attendances
           2003.                       "Substitutions

DATA: BEGIN OF INF0015 OCCURS 0.
        INCLUDE STRUCTURE P0015.
DATA:   INF15_EXS TYPE I.
DATA: END   OF INF0015.

DATA: BEGIN OF BDCDATA OCCURS 100.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

* table to store information from t7brtr
DATA: BEGIN OF I7BRTR OCCURS 0.
        INCLUDE STRUCTURE T7BRTR.
DATA: END OF I7BRTR.

DATA: W_INDEX LIKE SY-TABIX,
      W_P0410_VALID,
      P_MODE.

DATA: BEG_DAT(10), END_DAT(10).   "format dates: dd/mm/yy

DATA: BEGIN OF TOTAL OCCURS 0,    "catts
        SELE TYPE I,
        PROC TYPE I,
      END OF TOTAL.

*Fields used for function RP_SET_NAME_FORMAT
*and function RP_EDIT_NAME.
DATA PROGRAMME_NAME LIKE SY-REPID.
DATA EMPLOYEE_NAME_FORMAT LIKE P0002-KNZNM.
