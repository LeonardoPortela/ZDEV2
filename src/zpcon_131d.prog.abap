*&---------------------------------------------------------------------*
*& Report  ZPCON_131D
*&---------------------------------------------------------------------*
*& This report looks at production orders with operation confirmations *
*& and process orders with phase confirmations where the operation or  *
*& phase data (dates, quantities, activities) was not updated.         *
*& The report only works under the following conditions:               *
*& * only for time ticket confirmations                                *
*& * there is only ONE confirmation for the operation/phase            *
*& * the confirmation counter is zero or one in the operation/phase    *
*& The operation data will then be completed from the confirmation.    *
*&---------------------------------------------------------------------*
*& Version:      2.1                                                   *
*& Author:       SAP                                                   *
*& Date:         03.06.2008                                            *
*& Version 1.1 - 03.06.2008 (AK) (now also works if RMZHL = 1)         *
*& Version 2.0 - 16.02.2010 (AK) (added order lock)                    *
*& Version 2.1 - 23.02.2010 (AK) (extended to process order phases)    *
*&---------------------------------------------------------------------*

REPORT zpcon_131d .

INCLUDE lcokotyp.

TABLES: caufv.

SELECTION-SCREEN COMMENT /1(80) text1.                      "#EC NEEDED
SELECTION-SCREEN COMMENT /1(80) text2.                      "#EC NEEDED
SELECTION-SCREEN COMMENT /1(80) text3.                      "#EC NEEDED
SELECTION-SCREEN ULINE.
SELECTION-SCREEN COMMENT /1(80) texta.                      "#EC NEEDED
SELECT-OPTIONS r_aufnr FOR caufv-aufnr.
SELECTION-SCREEN COMMENT /1(80) textb.                      "#EC NEEDED
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE title. "#EC *
SELECTION-SCREEN COMMENT /1(72) textc.                      "#EC NEEDED
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME.
PARAMETERS: testmode RADIOBUTTON GROUP mode DEFAULT 'X'.
PARAMETERS: update RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK block2.
SELECTION-SCREEN COMMENT /1(72) textd.                      "#EC NEEDED
SELECTION-SCREEN END OF BLOCK block.

DATA: BEGIN OF ls_order,
        aufnr LIKE caufv-aufnr,
        aufpl LIKE caufv-aufpl,
      END OF ls_order.
DATA  lt_order LIKE ls_order OCCURS 0.
DATA: ls_afvc TYPE afvc.
DATA  lt_afvc TYPE TABLE OF afvc.
DATA: ls_afvv TYPE afvv.
DATA  lt_afvv TYPE TABLE OF afvv.
DATA: ls_afru TYPE afru.
DATA  lt_afru TYPE TABLE OF afru.

DATA lv_found TYPE c.
DATA l_found TYPE c.
DATA caufv_entries LIKE sy-tabix.
DATA afru_entries  LIKE sy-tabix.

INITIALIZATION.
  text1 = 'This Report looks for confirmations where the corresponding'.
  text2 = 'operation of the production order (phase of process order)'.
  text3 = 'was not updated.'.
  texta = 'Please specify order numbers of orders to be processed.'.
  textb = 'With initial number range, all orders will be selected!'.
  textc = 'If TESTMODE is set, no database update will occur.'.
  textd = 'Set radio button UPDATE for updating the database entries.'.
  title = 'Mode'.

START-OF-SELECTION.

* select production orders in specified range
  SELECT aufnr aufpl
         FROM  caufv
         INTO  TABLE lt_order
         WHERE aufnr IN r_aufnr
         AND ( autyp = auftragstyp-fert
            OR autyp = auftragstyp-bord ).
  IF NOT sy-subrc IS INITIAL.
    WRITE: / 'No orders were found in the specified range.'.
    EXIT.
  ENDIF.

  LOOP AT lt_order INTO ls_order.

*   lock order
    CALL FUNCTION 'CO_ZF_ORDER_LOCK'
      EXPORTING
        aufnr                = ls_order-aufnr
      EXCEPTIONS
        order_already_locked = 1
        system_failure       = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      WRITE: / 'Order', ls_order-aufnr, 'already locked' COLOR 6.
      CONTINUE.
    ENDIF.

*   select operations
    REFRESH lt_afvc.
    SELECT *
           FROM  afvc
           INTO  TABLE lt_afvc
           WHERE aufpl = ls_order-aufpl.
    IF NOT sy-subrc IS INITIAL.
      WRITE: / 'Order', ls_order-aufnr, 'no operations found.'.
      CALL FUNCTION 'CO_ZF_ORDER_DELOCK'
        EXPORTING
          aufnr = ls_order-aufnr.
      CONTINUE.
    ENDIF.
    REFRESH lt_afvv.
    SELECT *
           FROM  afvv
           INTO  TABLE lt_afvv
           WHERE aufpl = ls_order-aufpl.
    IF NOT sy-subrc IS INITIAL.
      WRITE: / 'Order', ls_order-aufnr, 'no operations found.'.
      CALL FUNCTION 'CO_ZF_ORDER_DELOCK'
        EXPORTING
          aufnr = ls_order-aufnr.
      CONTINUE.
    ENDIF.

*   select confirmations
    REFRESH lt_afru.
    SELECT *
           FROM  afru
           INTO  TABLE lt_afru
           FOR ALL ENTRIES IN lt_afvc
           WHERE rueck = lt_afvc-rueck.
    IF NOT sy-subrc IS INITIAL.
*     no confirmations - nothing to do
      CALL FUNCTION 'CO_ZF_ORDER_DELOCK'
        EXPORTING
          aufnr = ls_order-aufnr.
      CONTINUE.
    ENDIF.
*   check only the last confirmation for every operation
    SORT lt_afru BY rueck rmzhl DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_afru COMPARING rueck.

    CLEAR l_found.

*   if the operation contains NO confirmation data (counter is 0)
*   or the confirmation counter is 1 (but AFVC/AFVV updates missing)
*   the data will be updated from the first confirmation.
    LOOP AT lt_afru INTO ls_afru.
      ADD 1 TO afru_entries.
      IF ls_afru-rmzhl <> '00000001'.
        WRITE: / 'Order', ls_order-aufnr,
                 'operation', ls_afru-vornr,
                 'more than one confirmation - no update!' COLOR 3.
        CONTINUE.
      ENDIF.
      READ TABLE lt_afvc
           INTO  ls_afvc
           WITH  KEY rueck = ls_afru-rueck.
      IF NOT sy-subrc IS INITIAL.
        WRITE: / 'Order', ls_order-aufnr,
                 'operation', ls_afru-vornr,
                 'error - no operation data AFVC.' COLOR 6.
        CONTINUE.
      ENDIF.
      READ TABLE lt_afvv
           INTO  ls_afvv
           WITH  KEY aufpl = ls_afvc-aufpl
                     aplzl = ls_afvc-aplzl.
      IF NOT sy-subrc IS INITIAL.
        WRITE: / 'Order', ls_order-aufnr,
                 'operation', ls_afru-vornr,
                 'error - no operation data AFVV.' COLOR 6.
        CONTINUE.
      ENDIF.
      IF ls_afvc-rmzhl IS INITIAL OR
         ( ls_afvc-rmzhl = '00000001' AND
           ls_afru-satza(1) = 'L'     AND    "Time Ticket
           ls_afvv-isdd IS INITIAL    AND
           ls_afvv-iedd IS INITIAL ).
        WRITE: / 'Order', ls_order-aufnr,
                 'operation', ls_afvc-vornr,
                 'confirmation data missing!' COLOR COL_NEGATIVE.
        lv_found = 'X'.
        l_found = 'X'.
*       update mode - complete operation data from confirmation
        IF NOT update IS INITIAL.
          ls_afru-ltxa1 = ls_afvc-ltxa1.
          MOVE-CORRESPONDING ls_afru TO ls_afvc.
          MOVE-CORRESPONDING ls_afru TO ls_afvv.
          UPDATE afvc FROM ls_afvc.
          UPDATE afvv FROM ls_afvv.
          WRITE: /'Database entry updated.' COLOR COL_POSITIVE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF NOT update IS INITIAL AND
       NOT l_found IS INITIAL.
      COMMIT WORK AND WAIT.
    ENDIF.

*   unlock order
    CALL FUNCTION 'CO_ZF_ORDER_DELOCK'
      EXPORTING
        aufnr = ls_order-aufnr.

  ENDLOOP.

  ULINE.
  DESCRIBE TABLE lt_order LINES caufv_entries.
  WRITE: / 'Number of orders checked:', caufv_entries.
  WRITE: / 'Number of confirmations checked:', afru_entries.
  ULINE.

  IF lv_found IS INITIAL.
    WRITE: / 'No orders with missing operation update from the',
             'confirmation were found in the specified range.'.
    ULINE.
  ENDIF.
