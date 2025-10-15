*----------------------------------------------------------------------*
* Include ZFIY0022_SCR
*----------------------------------------------------------------------*

* Pantalla de selacciõn -----------------------------------------------*
SELECT-OPTIONS: SO_BUKRS FOR BKPF-BUKRS OBLIGATORY,
                SO_GJAHR FOR BKPF-GJAHR DEFAULT SY-DATUM(4)
                                        NO-DISPLAY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BLOQUE0 WITH FRAME TITLE TEXT-000.
SELECT-OPTIONS: SO_BELNR FOR BKPF-BELNR,
                SO_BLART FOR BKPF-BLART NO-DISPLAY,
                SO_BUDAT FOR BKPF-BUDAT.
SELECTION-SCREEN END OF BLOCK BLOQUE0.

SELECTION-SCREEN BEGIN OF BLOCK BLOQUE1 WITH FRAME TITLE TEXT-T01.
PARAMETERS: P_PATH    TYPE RLGRAP-FILENAME NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK BLOQUE1.

* Eventos -------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
*  CALL FUNCTION 'F4_FILENAME'
*    IMPORTING
*      FILE_NAME = P_PATH.
