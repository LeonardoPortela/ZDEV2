*& OSS-Note 335559
*&  Please read carefully!
*&
*&  Correction Report for wrong confirmation counter in order operation field AFVC-RMZHL
*&  Select-options: Order No. and Order Category
*&  First always run in test mode!!!
*&
*&  1. read order header VIAUFKS
*&  2. read order operation AFVC
*&  3. read confirmations AFRU (AFVC-RMZL <> AFRU-RMZL)
*&  4. quantity of confirmations per operation <> confirmation counter on AFVC-RMZL
*&     4.1. write log
*&     4.2. if not test run update AFVC-RMZHL
*&---------------------------------------------------------------------*
REPORT zafvc002 MESSAGE-ID ru.

TABLES: viaufks, afvc, afru, *afvc.
DATA: yes         VALUE 'X',
      no          VALUE space,
      counter     TYPE i,                        "counter for DESCRIBE
      first       VALUE 'X',                    "first run
      corr_recs   TYPE i,                      "# of records to be corrected
      corr_fails  TYPE i,                    "# of records correction failed
      iphas_rel   TYPE afih-iphas VALUE '2', " order released
      iphas_tcl   TYPE afih-iphas VALUE '3', " order technically completed
      comment(30) TYPE c.                    "comment for screen log

TYPES: BEGIN OF viaufks_stru,             "orders
         aufnr TYPE caufv-aufnr,
         aufpl TYPE caufv-aufpl,
       END OF viaufks_stru.
DATA viaufks_itab TYPE SORTED TABLE OF viaufks_stru
                  WITH UNIQUE KEY aufpl WITH HEADER LINE.

TYPES: BEGIN OF afvc_stru,               "operations
         aufpl TYPE afvc-aufpl,
         aplzl TYPE afvc-aplzl,
         vornr TYPE afvc-vornr,
         rueck TYPE afvc-rueck,
         rmzhl TYPE afvc-rmzhl,
       END OF afvc_stru.
DATA afvc_itab TYPE SORTED TABLE OF afvc_stru
               WITH UNIQUE KEY rueck WITH HEADER LINE.

TYPES: BEGIN OF afru_stru,           "confirmations
         rueck TYPE afru-rueck,
         rmzhl TYPE afru-rmzhl,
       END OF afru_stru.
DATA afru_itab TYPE STANDARD TABLE OF afru_stru WITH HEADER LINE.

PARAMETERS:  test DEFAULT yes.                 "Testrun


SELECT-OPTIONS: order_no FOR viaufks-aufnr,       " order numbers
                ord_cat FOR viaufks-autyp.       " order category

AT SELECTION-SCREEN.
  IF order_no IS INITIAL AND
     ord_cat IS INITIAL.
    MESSAGE e888 WITH 'Please enter order number or order type'.
  ENDIF.

START-OF-SELECTION.
* Select all orders from VIAUFKS for date/order-no.
  SELECT aufnr aufpl FROM viaufks INTO TABLE viaufks_itab
           WHERE aufnr IN order_no
             AND autyp IN ord_cat
             AND ( iphas EQ iphas_rel OR iphas EQ iphas_tcl ).

  DESCRIBE TABLE viaufks_itab LINES counter.

  IF counter < 1.                                "No orders at all
    WRITE / 'No orders selected'.
  ELSE.
    WRITE: / counter, 'orders selected'.
    PERFORM select_afvc.
  ENDIF.


  IF test = yes.
    WRITE: / '# of AFVC records to be corrected:', corr_recs.
  ELSE.
    WRITE: / '# of AFVC records to be corrected:', corr_recs, 'thereof # of corrections failed:', corr_fails.
  ENDIF.


*---------------------------------------------------------------------*
* select_afvc    Select operations to orders
*---------------------------------------------------------------------*
FORM select_afvc.

  REFRESH afvc_itab.

* Select all operations for order

  SELECT aufpl aplzl vornr rueck rmzhl FROM afvc INTO TABLE afvc_itab
           FOR ALL ENTRIES IN viaufks_itab
           WHERE aufpl EQ viaufks_itab-aufpl.

  IF sy-subrc NE 0.
    WRITE: / 'No operations selected'.      "No operations at all
  ELSE.
    PERFORM select_afru.
  ENDIF.

ENDFORM.                    "SELECT_AFVC
*---------------------------------------------------------------------*
* select_afru    Select confirmations to order operations
*---------------------------------------------------------------------*
FORM select_afru.

  SELECT rueck rmzhl FROM afru
    INTO TABLE afru_itab
    FOR ALL ENTRIES IN afvc_itab
    WHERE rueck EQ afvc_itab-rueck.

  SORT afru_itab BY rueck rmzhl DESCENDING.

  DELETE ADJACENT DUPLICATES FROM afru_itab COMPARING rueck.

* Now finally check if confirmation counter in the operations is correct
  LOOP AT afru_itab.
    PERFORM check_data USING afru_itab-rueck afru_itab-rmzhl.
  ENDLOOP.

ENDFORM.                    "SELECT_AFVC

*---------------------------------------------------------------------*
* check_data     check counter on afvc with summary of afru
*---------------------------------------------------------------------*
FORM check_data USING l_rueck l_rmzhl.

  READ TABLE afvc_itab WITH TABLE KEY rueck = l_rueck.
  CHECK sy-subrc EQ 0.

  READ TABLE viaufks_itab WITH TABLE KEY aufpl = afvc_itab-aufpl.

  IF NOT afvc_itab-rmzhl EQ l_rmzhl.
    PERFORM process_afvc.
  ENDIF.

ENDFORM.                    "CHECK_DATA


*---------------------------------------------------------------------*
* process_afvc   generate new conf.no.
*                update operation with new conf.no.
*                write log record on screen
*---------------------------------------------------------------------*
FORM process_afvc.

  IF first EQ yes.                           "write header only once
    WRITE AT /1 'order no.'.                 "order no
    WRITE AT 17 'operation'.                 "operation
    WRITE AT 27 'confirmation'.              "confirmation no
    WRITE AT 40 'afvc-rmzhl'.                "confirmation counter afvc
    WRITE AT 55 'counter afru'.              "confirmation sum of afr
    WRITE AT 70 'comment'.                   "comment line
    first = no.
  ENDIF.
  WRITE AT /1 viaufks_itab-aufnr.               "order no
  WRITE AT 17 afvc_itab-vornr.                "operation
  WRITE AT 27 afvc_itab-rueck.                "confirmation no
  WRITE AT 40 afvc_itab-rmzhl.                "confirmation counter
  WRITE AT 55 afru_itab-rmzhl.             "confirmation sum of afru

  corr_recs = corr_recs + 1.                  "# of records to be corrected

  IF test = yes.
    WRITE AT 70 'test run'.
  ELSE.
    PERFORM update_afvc.
  ENDIF.

ENDFORM.                    "PROCESS_AFVC
*---------------------------------------------------------------------*
*  update_afvc    update operation with new conf.counter
*---------------------------------------------------------------------*
FORM update_afvc.

  UPDATE afvc SET rmzhl = afru_itab-rmzhl
   WHERE aufpl = afvc_itab-aufpl
     AND aplzl = afvc_itab-aplzl.
  IF sy-subrc NE 0.
    WRITE AT 70: 'Error Update AFVC'.
    corr_fails = corr_fails + 1.
  ELSE.
    WRITE AT 70: 'update AFVC-RMZHL with counter AFRU'.
  ENDIF.

ENDFORM.                    "UPDATE_AFVC
