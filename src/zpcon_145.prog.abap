*&---------------------------------------------------------------------*
*& Report  ZPCON_145                                                   *
*&---------------------------------------------------------------------*
*& This report checks the confirmed dates for operations/phases.       *
*& It checks if the values in table AFVV correspond to the AFRU dates. *
*&---------------------------------------------------------------------*
*& Version:      1.0                                                   *
*& Author:       SAP                                                   *
*& Date:         15.10.2010                                            *
*& Last Changed: (no changes yet)                                      *
*&---------------------------------------------------------------------*

REPORT  zpcon_145.                                         "see LCORFF4L

INCLUDE lcokotyp.

TABLES: caufv.

SELECTION-SCREEN COMMENT /1(80) text1.
SELECTION-SCREEN COMMENT /1(80) text2.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN COMMENT /1(80) texta.
SELECT-OPTIONS r_aufnr FOR caufv-aufnr.
SELECTION-SCREEN COMMENT /1(80) textb.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE title.
SELECTION-SCREEN COMMENT /1(72) textc.
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME.
PARAMETERS: testmode RADIOBUTTON GROUP mode DEFAULT 'X'.
PARAMETERS: update RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK block2.
SELECTION-SCREEN COMMENT /1(72) textd.
SELECTION-SCREEN END OF BLOCK block.

DATA: BEGIN OF ls_order,
        aufnr TYPE aufnr,
        aufpl TYPE co_aufpl,
      END OF ls_order.
DATA  lt_order LIKE TABLE OF ls_order.
DATA: BEGIN OF ls_afvc,
        aufpl TYPE co_aufpl,
        aplzl TYPE co_aplzl,
        vornr TYPE vornr,
        rueck TYPE co_rueck,
      END OF ls_afvc.
DATA  lt_afvc LIKE TABLE OF ls_afvc.
DATA: BEGIN OF ls_afvv,
        aufpl TYPE co_aufpl,
        aplzl TYPE co_aplzl,
        isdd  TYPE isdd,
        isdz  TYPE isdz,
        iedd  TYPE iedd,
        iedz  TYPE iedz,
      END OF ls_afvv.
DATA  lt_afvv LIKE TABLE OF ls_afvv.
DATA: BEGIN OF ls_afru,
        rueck TYPE co_rueck,
        rmzhl TYPE co_rmzhl,
        stokz TYPE co_stokz,
        stzhl TYPE co_stzhl,
        isdd  TYPE ru_isdd,
        isdz  TYPE ru_isdz,
        iedd  TYPE ru_iedd,
        iedz  TYPE ru_iedz,
      END OF ls_afru.
DATA  lt_afru LIKE TABLE OF ls_afru.

DATA l_found TYPE c.
DATA entries LIKE sy-tabix.
DATA l_isdd TYPE isdd.
DATA l_isdz TYPE isdz.
DATA l_iedd TYPE iedd.
DATA l_iedz TYPE iedz.

INITIALIZATION.
  text1 = 'This report checks if the confirmed dates (start/end'.
  text2 = 'processing) in an operation or phase are consistent'.
  texta = 'Please specify order numbers of orders to be processed.'.
  textb = 'With initial number range, all orders will be selected!'.
  textc = 'If TESTMODE is set, no database update will occur.'.
  textd = 'Set radio button UPDATE for updating the database entries.'.
  title = 'Mode'.

START-OF-SELECTION.

* select process and production orders in specified range
  SELECT aufnr aufpl
         FROM  caufv
         INTO  TABLE lt_order
         WHERE aufnr IN r_aufnr
         AND ( autyp = auftragstyp-fert OR
               autyp = auftragstyp-bord ).
  IF NOT sy-subrc IS INITIAL.
    WRITE: / 'No orders were found in the specified range.'.
    EXIT.
  ENDIF.

* select operations
  SELECT aufpl aplzl vornr rueck
         FROM  afvc
         INTO  TABLE lt_afvc
         FOR ALL ENTRIES IN lt_order
         WHERE aufpl = lt_order-aufpl.
  IF NOT sy-subrc IS INITIAL.
    WRITE: / 'No operations found.'.
    ULINE.
    EXIT.
  ENDIF.

* select operations - confirmed dates
  SELECT aufpl aplzl isdd isdz iedd iedz
         FROM  afvv
         INTO  TABLE lt_afvv
         FOR ALL ENTRIES IN lt_order
         WHERE aufpl = lt_order-aufpl.
  IF NOT sy-subrc IS INITIAL.
    WRITE: / 'No operations found.'.
    ULINE.
    EXIT.
  ENDIF.

* select confirmations - confirmed dates
  SELECT rueck rmzhl stokz stzhl isdd isdz iedd iedz
         FROM  afru
         INTO  TABLE lt_afru
         FOR ALL ENTRIES IN lt_afvc
         WHERE rueck = lt_afvc-rueck.
  IF NOT sy-subrc IS INITIAL.
    WRITE: / 'No confirmations found.'.
    ULINE.
    EXIT.
  ENDIF.

* now check operations
  LOOP AT lt_afvc
       INTO ls_afvc.
    READ TABLE lt_afvv
         INTO  ls_afvv
         WITH  KEY aufpl = ls_afvc-aufpl
                   aplzl = ls_afvc-aplzl.
    IF NOT sy-subrc IS INITIAL.
      WRITE: / ls_afvc-aufpl, ls_afvc-aplzl,
               'AFFV entry is missing!' COLOR COL_NEGATIVE.
      CONTINUE.
    ENDIF.
    CLEAR l_isdd.
    CLEAR l_isdz.
    CLEAR l_iedd.
    CLEAR l_iedz.
*   check confirmations
    LOOP AT lt_afru INTO ls_afru
                    WHERE rueck = ls_afvc-rueck
                      AND stzhl IS INITIAL
                      AND stokz IS INITIAL.
      IF l_isdd IS INITIAL.
        l_isdd = ls_afru-isdd.
        l_isdz = ls_afru-isdz.
        l_iedd = ls_afru-iedd.
        l_iedz = ls_afru-iedz.
      ELSE.
*       start date and time
        IF l_isdd > ls_afru-isdd.
          l_isdd = ls_afru-isdd.
          l_isdz = ls_afru-isdz.
        ELSE.
          IF l_isdd = ls_afru-isdd AND
             l_isdz > ls_afru-isdz.
            l_isdz = ls_afru-isdz.
          ENDIF.
        ENDIF.
*       end date and time
        IF l_iedd < ls_afru-iedd.
          l_iedd = ls_afru-iedd.
          l_iedz = ls_afru-iedz.
        ELSE.
          IF l_iedd = ls_afru-iedd AND
             l_iedz < ls_afru-iedz.
            l_iedz = ls_afru-iedz.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF NOT sy-subrc IS INITIAL.
*     no confirmations or none on operation level --> continue
      CONTINUE.
    ENDIF.
    IF l_isdd = ls_afvv-isdd AND
       l_isdz = ls_afvv-isdz AND
       l_iedd = ls_afvv-iedd AND
       l_iedz = ls_afvv-iedz.
*     everything is fine
      CONTINUE.
    ENDIF.
    READ TABLE lt_order
         INTO  ls_order
         WITH  KEY aufpl = ls_afvc-aufpl.
*   anything found?
    WRITE: / 'Order', ls_order-aufnr,
             'operation', ls_afvc-vornr.
    IF l_isdd <> ls_afvv-isdd.
      WRITE: / 'AFVV-ISDD' COLOR COL_NEGATIVE,
               ls_afvv-isdd, 'instead of', l_isdd.      "#EC UOM_IN_MES
    ENDIF.
    IF l_isdz <> ls_afvv-isdz.
      WRITE: / 'AFVV-ISDZ' COLOR COL_NEGATIVE,
               ls_afvv-isdz, 'instead of', l_isdz.      "#EC UOM_IN_MES
    ENDIF.
    IF l_iedd <> ls_afvv-iedd.
      WRITE: / 'AFVV-IEDD' COLOR COL_NEGATIVE,
               ls_afvv-iedd, 'instead of', l_iedd.      "#EC UOM_IN_MES
    ENDIF.
    IF l_iedz <> ls_afvv-iedz.
      WRITE: / 'AFVV-IEDZ' COLOR COL_NEGATIVE,
               ls_afvv-iedz, 'instead of', l_iedz.      "#EC UOM_IN_MES
    ENDIF.
    l_found = 'X'.
*   update mode - correct database entry
    IF NOT update IS INITIAL.
      UPDATE afvv SET isdd = l_isdd
                      isdz = l_isdz
                      iedd = l_iedd
                      iedz = l_iedz
             WHERE aufpl = ls_afvv-aufpl
               AND aplzl = ls_afvv-aplzl.
      WRITE: /'Database entry updated.' COLOR COL_POSITIVE.
    ENDIF.
    ULINE.
  ENDLOOP.

  DESCRIBE TABLE lt_order LINES entries.
  WRITE: / 'Number of orders checked:', entries.
  ULINE.

  IF l_found IS INITIAL.
    WRITE: / 'No orders with inconsistent confirmed dates',
             'were found in the specified range.'.
    ULINE.
  ENDIF.
