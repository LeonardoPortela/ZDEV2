
*TABLES: ska1.

TYPES: BEGIN OF ty_saida,
         saknr      TYPE zfit0219-saknr,
         d_c        TYPE zfit0219-d_c,
         dt_entrada TYPE zfit0219-dt_entrada,
         hr_entrada TYPE zfit0219-hr_entrada,
         usnam      TYPE zfit0219-usnam,
         name_text  TYPE adrp-name_text,
       END OF ty_saida.

DATA: it_screen_status TYPE TABLE OF sy-ucomm,
      it_saida         TYPE STANDARD TABLE OF ty_saida INITIAL SIZE 0,
      wa_saida         TYPE ty_saida.

*SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
*  SELECT-OPTIONS: p_SAKNR   FOR ska1-saknr NO-EXTENSION NO INTERVALS. "OBLIGATORY
*SELECTION-SCREEN END OF BLOCK part1.

*INITIALIZATION.
*
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
