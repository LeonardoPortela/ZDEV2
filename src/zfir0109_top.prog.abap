*TABLES: t001,t001w.

TYPES: BEGIN OF ty_saida,
         bukrs      TYPE zfit0218-bukrs,
         werks      TYPE zfit0218-werks,
         kostl      TYPE zfit0218-kostl,
         dt_entrada TYPE zfit0218-dt_entrada,
         hr_entrada TYPE zfit0218-hr_entrada,
         usnam      TYPE zfit0218-usnam,
         userid     TYPE zfit0218-userid,
         name_text  TYPE adrp-name_text,
       END OF ty_saida.

DATA: it_saida TYPE STANDARD TABLE OF ty_saida INITIAL SIZE 0,
      wa_saida TYPE ty_saida.

DATA: dynfields TYPE TABLE OF dynpread WITH HEADER LINE.

*SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
*  SELECT-OPTIONS: p_bukrs   FOR t001-bukrs NO-EXTENSION NO INTERVALS. "OBLIGATORY
*  SELECT-OPTIONS: p_werks   FOR t001w-werks NO-EXTENSION NO INTERVALS. "OBLIGATORY
*SELECTION-SCREEN END OF BLOCK part1.

*INITIALIZATION.

*AT SELECTION-SCREEN OUTPUT.
*
*AT SELECTION-SCREEN.
*
*  CASE sy-ucomm.
*    WHEN 'CRET'.
*      CALL SELECTION-SCREEN '0100'.
*    WHEN ''.
*    WHEN OTHERS.
*  ENDCASE.

START-OF-SELECTION.

  CALL SELECTION-SCREEN '0100'.

END-OF-SELECTION.
