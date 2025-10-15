*----------------------------------------------------------------------*
* Include ZFIY0022_TOP
*----------------------------------------------------------------------*

* Tablas transparentes ------------------------------------------------*
TABLES BKPF.

* Tipos de datos ------------------------------------------------------*
TYPES: BEGIN OF TY_BKPF,
        BUKRS LIKE BKPF-BUKRS,
        BELNR LIKE BKPF-BELNR,
        GJAHR LIKE BKPF-GJAHR,
        RLDNR LIKE BKPF-RLDNR,
       END OF TY_BKPF.

* Tablas internas / Estructuras ---------------------------------------*
DATA: T_BKPF TYPE STANDARD TABLE OF TY_BKPF,
      E_BKPF LIKE LINE OF T_BKPF,
      WA_BDC TYPE BDCDATA,
      TI_BDC TYPE TABLE OF BDCDATA.

DATA: V_PATH    TYPE RLGRAP-FILENAME.
