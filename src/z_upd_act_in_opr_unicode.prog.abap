*&---------------------------------------------------------------------*
*& Report  Z_UPD_ACT_IN_OPR_UNICODE                                    *
*&---------------------------------------------------------------------*

REPORT  z_upd_act_in_opr_unicode                    .

TABLES: caufv, afvv, afru.

* types
TYPES: BEGIN OF t_afru,                                  "Confirmation
         rueck LIKE afru-rueck,
         rmzhl LIKE afru-rmzhl,
         aufpl LIKE afru-aufpl,
         aplzl LIKE afru-aplzl,
         aufnr LIKE afru-aufnr,
         vornr LIKE afru-vornr,
         ism01 LIKE afru-ism01,
         ile01 LIKE afru-ile01,
         ism02 LIKE afru-ism02,
         ile02 LIKE afru-ile02,
         ism03 LIKE afru-ism03,
         ile03 LIKE afru-ile03,
         ism04 LIKE afru-ism04,
         ile04 LIKE afru-ile04,
         ism05 LIKE afru-ism05,
         ile05 LIKE afru-ile05,
         ism06 LIKE afru-ism06,
         ile06 LIKE afru-ile06,
       END   OF t_afru.

TYPES: BEGIN OF t_afvv,                          "Operation
         aufnr LIKE caufv-aufnr,
         aufpl LIKE afvv-aufpl,
         aplzl LIKE afvv-aplzl,
         ism01 LIKE afvv-ism01,
         ile01 LIKE afvv-ile01,
         ism02 LIKE afvv-ism02,
         ile02 LIKE afvv-ile02,
         ism03 LIKE afvv-ism03,
         ile03 LIKE afvv-ile03,
         ism04 LIKE afvv-ism04,
         ile04 LIKE afvv-ile04,
         ism05 LIKE afvv-ism05,
         ile05 LIKE afvv-ile05,
         ism06 LIKE afvv-ism06,
         ile06 LIKE afvv-ile06,
       END   OF t_afvv.

TYPES: BEGIN OF t_rueck,                         "Confirmation numbers
         rueck LIKE afvc-rueck,
       END   OF t_rueck.

* internal tables
DATA: lt_afru  TYPE STANDARD TABLE OF t_afru,
      lt_afvv  TYPE STANDARD TABLE OF t_afvv,
      lt_rueck TYPE STANDARD TABLE OF t_rueck,
      lt_afvv_complete TYPE STANDARD TABLE OF afvv.

* structures
DATA: ls_afru TYPE t_afru,
      ls_afvv TYPE t_afvv,
      ls_afvv_complete TYPE afvv.

DATA: BEGIN OF ls_activities,
        ism01 LIKE afru-ism01,
        ism02 LIKE afru-ism02,
        ism03 LIKE afru-ism03,
        ism04 LIKE afru-ism04,
        ism05 LIKE afru-ism05,
        ism06 LIKE afru-ism06,
        ile01 LIKE afru-ile01,
        ile02 LIKE afru-ile02,
        ile03 LIKE afru-ile03,
        ile04 LIKE afru-ile04,
        ile05 LIKE afru-ile05,
        ile06 LIKE afru-ile06,
      END OF ls_activities.

* variables
DATA: l_aufpl LIKE afvv-aufpl,
      l_aplzl LIKE afvv-aplzl.

DATA: l_ile      LIKE afru-ile01,
      l_ism      LIKE afru-ism01,
      l_afvv_ile LIKE afvv-ile01,
      l_afvv_ism LIKE afvv-ism01,
      l_afru_ile LIKE afru-ile01,
      l_afru_ism LIKE afru-ism01.

DATA: l_index LIKE sy-tabix.

* constants
CONSTANTS: con_x TYPE xfeld VALUE 'X'.


* selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: order FOR afru-aufnr.
SELECTION-SCREEN END   OF BLOCK b1.

PARAMETERS: update AS CHECKBOX.

* select confirmation numbers
SELECT afvc~rueck
  FROM caufv INNER JOIN afvc
    ON caufv~aufpl = afvc~aufpl
  INTO TABLE lt_rueck
 WHERE caufv~aufnr IN ORDER.

IF lt_rueck[] IS INITIAL OR
   sy-subrc <> 0.
  SKIP.
  WRITE: / 'No orders found!'.
  EXIT.
ENDIF.

* select confirmations
SELECT rueck rmzhl aufpl aplzl aufnr vornr ism01 ile01 ism02 ile02
       ism03 ile03 ism04 ile04 ism05 ile05 ism06 ile06
  FROM afru
  INTO TABLE lt_afru
   FOR ALL ENTRIES IN lt_rueck
 WHERE rueck = lt_rueck-rueck
   AND stokz = space
   AND stzhl = space.

IF lt_afru[] IS INITIAL OR
   sy-subrc <> 0.
  SKIP.
  WRITE: / 'No confirmations found!'.
  EXIT.
ENDIF.

* select operations
SELECT caufv~aufnr afvv~aufpl afvv~aplzl afvv~ism01 afvv~ile01
       afvv~ism02  afvv~ile02 afvv~ism03 afvv~ile03 afvv~ism04
       afvv~ile04  afvv~ism05 afvv~ile05 afvv~ism06 afvv~ile06
  FROM afvv INNER JOIN caufv
    ON caufv~aufpl = afvv~aufpl
  INTO TABLE lt_afvv
   FOR ALL ENTRIES IN lt_afru
 WHERE afvv~aufpl = lt_afru-aufpl
   AND afvv~aplzl = lt_afru-aplzl.

IF lt_afvv[] IS INITIAL OR
   sy-subrc <> 0.
  SKIP.
  WRITE: / 'No orders found!'.
  EXIT.
ENDIF.

* cumulate activities
SORT lt_afru BY aufpl aplzl rmzhl.
LOOP AT lt_afru INTO ls_afru.
  IF l_aufpl = ls_afru-aufpl AND
     l_aplzl = ls_afru-aplzl.
    DO 6 TIMES VARYING l_ile      FROM ls_activities-ile01
                                  NEXT ls_activities-ile02
               VARYING l_ism      FROM ls_activities-ism01
                                  NEXT ls_activities-ism02
               VARYING l_afru_ile FROM ls_afru-ile01
                                  NEXT ls_afru-ile02
               VARYING l_afru_ism FROM ls_afru-ism01
                                  NEXT ls_afru-ism02.
      IF l_ile = l_afru_ile.
        l_ism = l_ism + l_afru_ism.
      ELSE.
        PERFORM conversion USING l_afru_ile
                                 l_ile
                        CHANGING l_afru_ism.
        l_ism = l_ism + l_afru_ism.
        IF l_ile IS INITIAL.
          l_ile = l_afru_ile.
        ENDIF.
      ENDIF.
    ENDDO.
  ELSE.
    PERFORM check_activities.
*     new operation
    l_aufpl = ls_afru-aufpl.
    l_aplzl = ls_afru-aplzl.
    ls_activities-ile01 = ls_afru-ile01.
    ls_activities-ile02 = ls_afru-ile02.
    ls_activities-ile03 = ls_afru-ile03.
    ls_activities-ile04 = ls_afru-ile04.
    ls_activities-ile05 = ls_afru-ile05.
    ls_activities-ile06 = ls_afru-ile06.
    ls_activities-ism01 = ls_afru-ism01.
    ls_activities-ism02 = ls_afru-ism02.
    ls_activities-ism03 = ls_afru-ism03.
    ls_activities-ism04 = ls_afru-ism04.
    ls_activities-ism05 = ls_afru-ism05.
    ls_activities-ism06 = ls_afru-ism06.
  ENDIF.
ENDLOOP.

* last confirmation/operation
PERFORM check_activities.

IF lt_afvv[] IS INITIAL.
  SKIP.
  WRITE: / 'No operations with incorrect activities found!'.
  EXIT.
ELSE.
  SKIP.
  WRITE: / 'Operations with incorrect activities found!'.
  SKIP.
  WRITE: /5 'Order', 20 'Operation'.
  LOOP AT lt_afvv INTO ls_afvv.
    WRITE: /5 ls_afvv-aufnr.
    READ TABLE lt_afru INTO ls_afru WITH KEY aufpl = ls_afvv-aufpl
                                             aplzl = ls_afvv-aplzl.
    WRITE: 20 ls_afru-vornr.
  ENDLOOP.
ENDIF.

* Update
IF NOT update IS INITIAL.
*   get complete AFVV
  SELECT * FROM afvv INTO TABLE lt_afvv_complete
     FOR ALL ENTRIES IN lt_afvv
   WHERE aufpl = lt_afvv-aufpl
     AND aplzl = lt_afvv-aplzl.

*   update lt_afvv_complete with new activities
  LOOP AT lt_afvv_complete INTO ls_afvv_complete.
    l_index = sy-tabix.
    READ TABLE lt_afvv INTO ls_afvv
                       WITH KEY aufpl = ls_afvv_complete-aufpl
                                aplzl = ls_afvv_complete-aplzl.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING ls_afvv TO ls_afvv_complete.
      MODIFY lt_afvv_complete FROM ls_afvv_complete INDEX l_index.
    ENDIF.
  ENDLOOP.

*   Update of database table AFVV
  UPDATE afvv FROM TABLE lt_afvv_complete.

  IF sy-subrc IS INITIAL.
    SKIP.
    WRITE:/ sy-dbcnt, 'Operations have been updated successfully!'.
  ELSE.
    SKIP.
    WRITE:/ 'Database error: No operations have been updated!'.
  ENDIF.

ENDIF.

************************************************************************
* Form-Routines                                                        *
************************************************************************

FORM conversion USING l_meinh LIKE marm-meinh
                      l_meins LIKE mara-meins
             CHANGING l_quant.

  CALL FUNCTION 'CO_RU_UNIT_CONVERSION'
       EXPORTING
*             BASIS    = ' '
            krund    = ' '
*             MATNR    = ' '
            meinh    = l_meinh
            meins    = l_meins
            mgame    = l_quant
*             UMREN    = 1
*             UMREZ    = 0
*             AUFNR    = ' '
       IMPORTING
            o_mglme  = l_quant
       EXCEPTIONS
            OTHERS   = 1.

  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    "conversion

* ----------------------------------------------------------------------

FORM check_activities.

  DATA: l_index LIKE sy-tabix,
        l_upd   TYPE xfeld.

*   read operation
  READ TABLE lt_afvv INTO ls_afvv WITH KEY aufpl = l_aufpl
                                           aplzl = l_aplzl.

  IF sy-subrc IS INITIAL.
    l_index = sy-tabix.
    DO 6 TIMES VARYING l_ile      FROM ls_activities-ile01
                                  NEXT ls_activities-ile02
               VARYING l_ism      FROM ls_activities-ism01
                                  NEXT ls_activities-ism02
               VARYING l_afvv_ile FROM ls_afvv-ile01
                                  NEXT ls_afvv-ile02
               VARYING l_afvv_ism FROM ls_afvv-ism01
                                  NEXT ls_afvv-ism02.
      IF l_ile = l_afvv_ile OR
         l_afvv_ile IS INITIAL.
        IF NOT l_ism = l_afvv_ism.
          l_afvv_ism = l_ism.
          l_afvv_ile = l_ile.
          l_upd = con_x.
        ENDIF.
      ELSE.
        PERFORM conversion USING l_afvv_ile
                                 l_ile
                        CHANGING l_afvv_ism.
        IF NOT l_ism = l_afvv_ism.
          l_afvv_ism = l_ism.
          l_afvv_ile = l_ile.
          l_upd = con_x.
        ENDIF.
      ENDIF.
    ENDDO.
    IF NOT l_upd IS INITIAL.
*       update activities in internal table
      MODIFY lt_afvv FROM ls_afvv INDEX l_index.
    ELSE.
*       activities in operation are correct -> no update is needed
      DELETE lt_afvv INDEX l_index.
    ENDIF.
    CLEAR: l_upd.
  ENDIF.

ENDFORM.                    "check_activities
