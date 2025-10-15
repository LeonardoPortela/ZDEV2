*&---------------------------------------------------------------------*
*& Report  ZPCON_018B                                                  *
*&---------------------------------------------------------------------*
*& Report zum Ermitteln der Aufträge, deren Rückmeldezähler bei der    *
*& Kopfrückmeldung mittels Transaktion CO15/CORK nicht richtig         *
*& hochgesetzt wurde. Diese Aufträge verursachen bei der nächsten      *
*& Rückmeldung einen Kurz Dump wegen DUPREC !!                         *
*& Im Update-Modus werden die Rückmeldezähler dieser problembehafteten *
*& Aufträge angepasst.                                                 *
*&---------------------------------------------------------------------*
*& Report to select orders that have been confirmed by transaction     *
*& CO15 or CORK (header confirmation) but the confirmation-counter has *
*& not been updated correctly. These orders produce short dumps        *
*& (SAPSQL_ARRAY_INSERT_DUPREC) when they are being confirmed again    *
*& In UPDATE mode these faulty orders will be corrected.               *
*&---------------------------------------------------------------------*

REPORT zpcon_018b.

INCLUDE lcokotyp.

TABLES: caufv, afru, afko.                                  "#EC *

SELECTION-SCREEN COMMENT /1(80) text1.                      "#EC NEEDED
SELECTION-SCREEN COMMENT /1(80) text2.                      "#EC NEEDED
SELECTION-SCREEN COMMENT /1(80) text3.                      "#EC NEEDED
SELECTION-SCREEN COMMENT /1(80) text4.                      "#EC NEEDED
SELECTION-SCREEN COMMENT /1(80) text5.                      "#EC NEEDED
SELECTION-SCREEN ULINE.
SELECTION-SCREEN COMMENT /1(80) texta.                      "#EC NEEDED
SELECT-OPTIONS r_order FOR caufv-aufnr.
SELECTION-SCREEN COMMENT /1(80) textb.                      "#EC NEEDED
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE title."#EC NEEDED
SELECTION-SCREEN COMMENT /1(72) textc.                      "#EC NEEDED
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME.
PARAMETERS: testmode RADIOBUTTON GROUP mode DEFAULT 'X'.
PARAMETERS: update RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK block2.
SELECTION-SCREEN COMMENT /1(72) textd.                      "#EC NEEDED
SELECTION-SCREEN END OF BLOCK block.

DATA: BEGIN OF ls_order,
        aufnr LIKE caufv-aufnr,
        rueck LIKE caufv-rueck,
        rmzhl LIKE caufv-rmzhl,
      END OF ls_order.
DATA  lt_order LIKE ls_order OCCURS 0.
DATA: BEGIN OF ls_afru,
        rueck LIKE afru-rueck,
        rmzhl LIKE afru-rmzhl,
      END OF ls_afru.
DATA  lt_afru LIKE ls_afru OCCURS 0.

DATA lv_found      TYPE c.
DATA caufv_entries LIKE sy-tabix.

INITIALIZATION.
  text1 = 'This report looks for production orders or process orders'.
  text2 = 'with incorrect confirmation counter in the order header'.
  text3 = 'and corrects this counter in UPDATE mode.'.
  text4 = 'The report is only relevant for order header confirmations'.
  text5 = '(posted with transaction CO15 or CORK).'.
  texta = 'Please specify order numbers of orders to be processed.'.
  textb = 'With initial number range, all orders will be selected!'.
  textc = 'If TESTMODE is set, no database update will occur.'.
  textd = 'Set radio button UPDATE for updating the database entries.'.
  title = 'Mode'.

START-OF-SELECTION.

* select process and production orders in specified range
  SELECT aufnr rueck rmzhl
         FROM  caufv
         INTO  TABLE lt_order
         WHERE aufnr IN r_order
         AND ( autyp = auftragstyp-fert OR
               autyp = auftragstyp-bord )
         AND   rueck > 0.
  IF NOT sy-subrc IS INITIAL.
    WRITE: / 'No orders with header confirmations were found',
             'in the specified range.'.
    EXIT.
  ENDIF.

  SORT lt_order BY aufnr.

* select confirmations with highest confirmation counter
  LOOP AT lt_order INTO ls_order.
    SELECT rueck MAX( rmzhl )
           FROM afru
           APPENDING TABLE lt_afru
           WHERE rueck = ls_order-rueck
           GROUP BY rueck.
  ENDLOOP.

  SORT lt_afru BY rueck.

* now check order headers for wrong confirmation counter.
  LOOP AT lt_order INTO ls_order.
    READ TABLE lt_afru
         INTO  ls_afru
         WITH  KEY rueck = ls_order-rueck BINARY SEARCH.
    IF NOT sy-subrc IS INITIAL.
      WRITE: / 'No confirmation found for order', ls_order-aufnr.
      CONTINUE.
    ENDIF.
    IF ls_afru-rmzhl NE ls_order-rmzhl.
      WRITE: / 'Order', ls_order-aufnr,
               'confirmation counter', ls_order-rmzhl,
               'incorrect' COLOR COL_NEGATIVE,
               'and should be', ls_afru-rmzhl.
      lv_found = 'X'.
*     update mode - correct database entry
      IF NOT update IS INITIAL.
        UPDATE afko
               SET rmzhl = ls_afru-rmzhl
               WHERE aufnr = ls_order-aufnr.
        WRITE: / 'Database entry corrected.' COLOR COL_POSITIVE.
      ENDIF.
    ENDIF.
  ENDLOOP.

  ULINE.
  DESCRIBE TABLE lt_order LINES caufv_entries.
  WRITE: / 'Number of orders with header confirmations checked:',
            caufv_entries.
  ULINE.

  IF lv_found IS INITIAL.
    WRITE: / 'No orders with inconsistent confirmation counters',
             'were found in the specified range.'.
    ULINE.
  ENDIF.
