*&---------------------------------------------------------------------*
*& Report  ZPCON_140                                                   *
*&---------------------------------------------------------------------*
*& This report checks the confirmed yield/scrap/rework/total quantity  *
*& for the operations/phases. It checks if the values in table AFFV    *
*& correspond to the cumulated confirmed quantities.                   *
*&---------------------------------------------------------------------*
*& Version:      1.0                                                   *
*& Author:       SAP                                                   *
*& Date:         25.03.2009                                            *
*& Last Changed: (no changes yet)                                      *
*&---------------------------------------------------------------------*

REPORT  zpcon_140.

INCLUDE lcokotyp.

TABLES: caufv.

SELECTION-SCREEN COMMENT /1(80) text1.
SELECTION-SCREEN COMMENT /1(80) text2.
SELECTION-SCREEN COMMENT /1(80) text3.
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
        aufnr LIKE caufv-aufnr,
        aufpl LIKE caufv-aufpl,
      END OF ls_order.
DATA  lt_order LIKE ls_order OCCURS 0.
DATA: BEGIN OF ls_afvc,
        aufpl LIKE afvc-aufpl,
        aplzl LIKE afvc-aplzl,
        vornr LIKE afvc-vornr,
        rueck LIKE afvc-rueck,
      END OF ls_afvc.
DATA  lt_afvc LIKE ls_afvc OCCURS 0.
DATA: BEGIN OF ls_afvv,
        aufpl LIKE afvv-aufpl,
        aplzl LIKE afvv-aplzl,
        lmnga LIKE afvv-lmnga,
        xmnga LIKE afvv-xmnga,
        rmnga LIKE afvv-rmnga,
        gmnga LIKE afvv-gmnga,
      END OF ls_afvv.
DATA  lt_afvv LIKE ls_afvv OCCURS 0.
DATA: BEGIN OF ls_afru,
        rueck LIKE afru-rueck,
        rmzhl LIKE afru-rmzhl,
        stzhl LIKE afru-stzhl,
        lmnga LIKE afru-lmnga,
        xmnga LIKE afru-xmnga,
        rmnga LIKE afru-rmnga,
        gmnga LIKE afru-gmnga,
      END OF ls_afru.
DATA  lt_afru LIKE ls_afru OCCURS 0.

DATA l_found TYPE c.
DATA entries LIKE sy-tabix.
DATA l_lmnga TYPE lmnga.
DATA l_xmnga TYPE xmnga.
DATA l_rmnga TYPE rmnga.
DATA l_gmnga TYPE gmnga.

INITIALIZATION.
  text1 = 'This report checks if the confirmed yield, scrap, rework'.
  text2 = 'and total quantity in an operation or phase is consistent'.
  text3 = 'with the cumulated quantities of the confirmations.'.
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

* select operations - confirmed quantities
  SELECT aufpl aplzl lmnga xmnga rmnga gmnga
         FROM  afvv
         INTO  TABLE lt_afvv
         FOR ALL ENTRIES IN lt_order
         WHERE aufpl = lt_order-aufpl.
  IF NOT sy-subrc IS INITIAL.
    WRITE: / 'No operations found.'.
    ULINE.
    EXIT.
  ENDIF.

* select confirmations - confirmed quantities
  SELECT rueck rmzhl stzhl lmnga xmnga rmnga gmnga
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
    CLEAR l_lmnga.
    CLEAR l_xmnga.
    CLEAR l_rmnga.
    CLEAR l_gmnga.
*   check confirmations
    LOOP AT lt_afru INTO ls_afru
                    WHERE rueck = ls_afvc-rueck.
      IF ls_afru-stzhl IS INITIAL.
        l_lmnga = l_lmnga + ls_afru-lmnga.
        l_xmnga = l_xmnga + ls_afru-xmnga.
        l_rmnga = l_rmnga + ls_afru-rmnga.
        l_gmnga = l_gmnga + ls_afru-gmnga.
      ELSE.
        l_lmnga = l_lmnga - ls_afru-lmnga.
        l_xmnga = l_xmnga - ls_afru-xmnga.
        l_rmnga = l_rmnga - ls_afru-rmnga.
        l_gmnga = l_gmnga - ls_afru-gmnga.
      ENDIF.
    ENDLOOP.
    IF NOT sy-subrc IS INITIAL.
*     no confirmations or none on operation level --> continue
      CONTINUE.
    ENDIF.
    IF l_lmnga = ls_afvv-lmnga AND
       l_xmnga = ls_afvv-xmnga AND
       l_rmnga = ls_afvv-rmnga AND
       l_gmnga = ls_afvv-gmnga.
*     everything is fine
      CONTINUE.
    ENDIF.
    READ TABLE lt_order
         INTO  ls_order
         WITH  KEY aufpl = ls_afvc-aufpl.
*   anything found?
    WRITE: / 'Order', ls_order-aufnr,
             'operation', ls_afvc-vornr.
    IF l_lmnga <> ls_afvv-lmnga.
      WRITE: / 'AFVV-LMNGA' COLOR COL_NEGATIVE,
               ls_afvv-lmnga, 'instead of', l_lmnga.     "#EC UOM_IN_MES
    ENDIF.
    IF l_xmnga <> ls_afvv-xmnga.
      WRITE: / 'AFVV-XMNGA' COLOR COL_NEGATIVE,
               ls_afvv-xmnga, 'instead of', l_xmnga.     "#EC UOM_IN_MES
    ENDIF.
    IF l_rmnga <> ls_afvv-rmnga.
      WRITE: / 'AFVV-RMNGA' COLOR COL_NEGATIVE,
               ls_afvv-rmnga, 'instead of', l_rmnga.     "#EC UOM_IN_MES
    ENDIF.
    IF l_gmnga <> ls_afvv-gmnga.
      WRITE: / 'AFVV-GMNGA' COLOR COL_NEGATIVE,
               ls_afvv-gmnga, 'instead of', l_gmnga.     "#EC UOM_IN_MES
    ENDIF.
    l_found = 'X'.
*   update mode - correct database entry
    IF NOT update IS INITIAL.
      UPDATE afvv SET lmnga = l_lmnga
                      xmnga = l_xmnga
                      rmnga = l_rmnga
                      gmnga = l_gmnga
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
    WRITE: / 'No orders with inconsistent confirmed quantities',
             'were found in the specified range.'.
    ULINE.
  ENDIF.
