*----------------------------------------------------------------------*
* INCLUDE BILAI00_1, used in RFBILA00                              *
* Tabellen und Definitionen für Formulardruck aus altem
*           altem INCLUDE  BILA&I00.
*----------------------------------------------------------------------*
TABLES:   RSTXD,                       "Formularnamen aus Memory
          TSP03,                       "Druckerprüftabelle
          SSCRFIELDS.
DATA: YES(1) TYPE C VALUE '1',
       NO(1) TYPE C VALUE '0'.
DATA: AMOUNT(9) TYPE P.

DATA:
      SCR_PRINT_TO_FORM(1) TYPE C VALUE '0'.
DATA: BEGIN OF SCR_DATA OCCURS 0,
        BUKRS     LIKE SKB1-BUKRS,
        GSBER     LIKE SKC1A-GSBER,
        ERGSL     LIKE RF011Q-ERGSL,
        TYP(1)    TYPE C,
        BSUM      LIKE AMOUNT,
        VSUM      LIKE AMOUNT,
        SALDO     LIKE AMOUNT,         "Absolute Abweichung
        RELAB(16) TYPE P DECIMALS 1,   "Relative Abweichung
        STAR(5)   TYPE C,
        WAERS     LIKE T001-WAERS,

        BSUM2      LIKE AMOUNT,
        VSUM2      LIKE AMOUNT,
        SALDO2     LIKE AMOUNT,         "Absolute Abweichung
        RELAB2(16) TYPE P DECIMALS 1,   "Relative Abweichung
        WAERS2     LIKE T001-WAERS,
     END OF SCR_DATA.
