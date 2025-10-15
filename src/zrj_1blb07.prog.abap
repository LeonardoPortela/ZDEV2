REPORT j_1blb07 NO STANDARD PAGE HEADING MESSAGE-ID 8b
       LINE-SIZE 134
       LINE-COUNT 62(3).
*ENHANCEMENT-POINT J_1BLB07_G4 SPOTS ES_J_1BLB07 STATIC.
*ENHANCEMENT-POINT J_1BLB07_G5 SPOTS ES_J_1BLB07.
*ENHANCEMENT-POINT J_1BLB07_G6 SPOTS ES_J_1BLB07 STATIC.
*ENHANCEMENT-POINT J_1BLB07_G7 SPOTS ES_J_1BLB07.
*-- list valuated material stock and value from last period closing----*
TABLES: mara,                          "material master: general data
        marc,                          "material plant segment
        makt,                          "material text
        marv,                          "Material Ctrl Record "PLFK001575
        mbew,                          "material valuation
        mbewh,                  "material valuation provisious periods
        t001,                          "company code
        t001k,                         "valuation area
        t001w,                         "plants
        t025t,                         "text for valuation class
        t134m.                         "value/quantity control


*----------------------------------------------------------------------*
* CONSTANDS                                                            *
*----------------------------------------------------------------------*
CONSTANTS: on       VALUE 'X',
           off      VALUE space,
           modelo_7 VALUE '7'.
*----------------------------------------------------------------------*
* DATA                                                                 *
*----------------------------------------------------------------------*
DATA  only_mbew.
DATA: lines LIKE sy-tabix.
DATA  ch_flag.
*del DATA: FIRST_NBM VALUE ON.        "Note 374099
DATA: first_nbm VALUE off.                                 "Note 374099
DATA: first_bklas VALUE off.
DATA: sum_total LIKE mbew-vmsal,
      sum_nbm   LIKE mbew-vmsal,
      sum_bwkla LIKE mbew-vmsal,
      sum_bwkey LIKE mbew-vmsal.
DATA: pr_line(150).
DATA: pagno(6) TYPE n.

DATA: BEGIN OF sum_tab OCCURS 50,                           "PLFK000634
        bwkey LIKE mbew-bwkey,                              "PLFK000634
        bklas LIKE mbew-bklas,                              "PLFK000634
        bkbez LIKE t025t-bkbez,                             "PLFK000634
        summe LIKE mbew-vmsal,                              "PLFK000634
      END OF sum_tab.                                       "PLFK000634
                                                            "PLFK000634
DATA: BEGIN OF bw_tab OCCURS 10,                            "PLFK000634
        bwkey LIKE mbew-bwkey,                              "PLFK000634
        summe LIKE mbew-vmsal,                              "PLFK000634
      END OF bw_tab.                                        "PLFK000634
                                                            "PLFK000634
DATA: no_forward.                                           "PLFK000634



*--- adress data ------------------------------------------------------*
DATA: address       LIKE sadr,         "address and CGC data
      cgc_number    LIKE j_1bwfield-cgc_number,  "etc. from the CGC
      branch_data   LIKE j_1bbranch,   "branch that is issuing
      parnad        LIKE j_1binnad.    "Modelo 3
*----------------------------------------------------------------------*
* SELECTION-SCREEN                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-012.
PARAMETERS: p_bukrs LIKE t001-bukrs OBLIGATORY,
            p_branch LIKE t001w-j_1bbranch OBLIGATORY.
SELECT-OPTIONS: p_matnr  FOR mbew-matnr.
SELECT-OPTIONS: so-bklas FOR mbew-bklas.                    "PLFK002380
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-114.
PARAMETERS: firstpag(6) TYPE n OBLIGATORY,
            booksize(6) TYPE n OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-115.
PARAMETERS: p_lfmon LIKE marv-lfmon OBLIGATORY ,
            p_lfgja LIKE marv-lfgja OBLIGATORY .
SELECTION-SCREEN END OF BLOCK b3.
* internal table: list of plants
DATA: BEGIN OF plants OCCURS 0,
        werks LIKE t001w-werks,
      END OF plants.

FIELD-GROUPS: header, item.


INSERT  mbew-bwkey mbew-bklas marc-steuc mbew-bwtar mbew-matnr
        INTO header.
INSERT  mara-meins mbew-vmkum mbew-vmsal mbew-vmvpr mbew-vmver
        mbew-vmstp makt-maktx  mbew-verpr mbew-stprs mbew-vprsv
*del    mbew-vmpei                                INTO ITEM."note 151588
        mbew-peinh                                INTO item."note 151588

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  GET PARAMETER ID 'BUK' FIELD p_bukrs.
*get parameter id 'GJA' field p_lfgja.

****cl_fins=>object_obsolete( exporting iv_tcode = sy-tcode
****                                    iv_progname = sy-repid ).


************************************************************************
******************** at selection-screen *******************************
************************************************************************

AT SELECTION-SCREEN.

* authority check for BUKRS
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'ACTVT' FIELD '03'
    ID 'BUKRS' FIELD p_bukrs.
  IF sy-subrc <> 0.
    SET CURSOR FIELD 'P_BUKRS'.
    MESSAGE e091 WITH p_bukrs.
  ENDIF.

* check if booksize > number of first page

  IF firstpag > booksize.
    MESSAGE e459.
  ENDIF.

* check if firstpag < 2.

  IF firstpag < 2.
    MESSAGE e467.
  ENDIF.

* set page numbering

  pagno = firstpag - 1.
*======================================================================*
*----------------------------------------------------------------------*
* check if new period closing is active for selected period            *
*----------------------------------------------------------------------*
  CALL FUNCTION 'J_1B_CHECK_NEW_PERIOD_CLOSING'
       EXPORTING
            i_bukrs           = p_bukrs
            i_lfgja           = p_lfgja
            i_lfmon           = p_lfmon
            i_caller          = modelo_7
       EXCEPTIONS
            period_not_closed = 1
            not_possible      = 2
            no_marv_for_bukrs = 3
            only_mbew         = 4
            future_period     = 5
            OTHERS            = 6.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE e474 WITH p_lfmon p_lfgja.
    WHEN 2.
      MESSAGE e475.
    WHEN 3.
      MESSAGE e476 WITH p_bukrs.
    WHEN 4.
      only_mbew = on.
    WHEN 5.
      MESSAGE e477 WITH p_lfmon p_lfgja.
    WHEN OTHERS.
* ??????????????????
  ENDCASE.

* if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
* endif.

************************************************************************
* START-OF-SELECTION                                                   *
************************************************************************
START-OF-SELECTION.
*----------------------------------------------------------------------*
*-------read t001w to determine plants assigned to chosen branch;------*
*----------------fill table plants with these plants-------------------*
*----------------------------------------------------------------------*
  SELECT * FROM t001w WHERE j_1bbranch = p_branch.
    SELECT * FROM t001k WHERE bukrs = p_bukrs.              "PLFK002380
      CHECK t001w-bwkey = t001k-bwkey.                      "PLFK002380
      APPEND t001w-werks TO plants.
    ENDSELECT.                                              "PLFK002380
  ENDSELECT.

*----------------------------------------------------------------------*
*------------continue only if plants for chosen branch exist-----------*
*----------------------------------------------------------------------*
  DESCRIBE TABLE plants LINES lines.
  IF lines = 0.
    sy-tvar1 = p_branch.
    WRITE: / text-300.             STOP.
  ENDIF.


*----------------------------------------------------------------------*
*------------------------determine cgc data----------------------------*
*------------------for page header of report pages---------------------*
*----------------------------------------------------------------------*

  CALL FUNCTION 'J_1BREAD_BRANCH_DATA'                      "PLFK001575
       EXPORTING                                            "PLFK001575
            branch      = p_branch                          "PLFK001575
            bukrs       = p_bukrs                           "PLFK001575
       IMPORTING                                            "PLFK001575
            address     = address                           "PLFK001575
            branch_data = branch_data                       "PLFK001575
            cgc_number  = cgc_number                        "PLFK001575
       EXCEPTIONS                                           "PLFK001575
            OTHERS      = 04.                               "PLFK001575

  IF sy-subrc NE 0.                                         "PLFK001575
    MESSAGE w453 WITH p_bukrs p_branch.                     "PLFK001575
  ENDIF.                                                    "PLFK001575
*--- read material control record -------------------------------------*
*DEL select single * from marv where bukrs = p_bukrs.        "PLFK001575

  CALL FUNCTION 'MARV_SINGLE_READ'
       EXPORTING
            bukrs      = p_bukrs
       IMPORTING
            wmarv      = marv
       EXCEPTIONS
            not_found  = 1
            wrong_call = 2
            OTHERS     = 3.
  IF sy-subrc <> 0.
    MESSAGE e476 WITH p_bukrs.
  ENDIF.
*----------------------------------------------------------------------*
*-- selection for all plants for one valuation area -------------------*
*----------------------------------------------------------------------*
  LOOP AT plants.
*----------------------------------------------------------------------*
*-- Material valuation ------------------------------------------------*
*----------------------------------------------------------------------*
    PERFORM read_mbew USING plants-werks.

  ENDLOOP.

************************************************************************
* END-OF-SELECTION                                                     *
************************************************************************
END-OF-SELECTION.
*-- sort extract ------------------------------------------------------*
  SORT.
*-- loop at extract ---------------------------------------------------*
  LOOP.
*-- AT NEW MBEW-BWKEY -------------------------------------------------*
    AT NEW mbew-bwkey   .  "normal with parameters of selection
      NEW-PAGE.                        "screen not possible.
      FORMAT INTENSIFIED COLOR COL_KEY.
      WRITE: / text-007, mbew-bwkey.
      FORMAT INTENSIFIED COLOR COL_NORMAL.
      ULINE.
      SKIP.
    ENDAT.

*-- AT NEW MBEW-BKLAS -------------------------------------------------*
    AT NEW mbew-bklas.
      SELECT SINGLE * FROM t025t
       WHERE spras = sy-langu
       AND   bklas = mbew-bklas.

      FORMAT INTENSIFIED COLOR COL_NEGATIVE.
      WRITE: / text-003, mbew-bklas, t025t-bkbez.  "valuation class
      FORMAT INTENSIFIED COLOR COL_NORMAL.
      CLEAR sum_tab.                                        "PLFK000634
      MOVE: mbew-bwkey     TO sum_tab-bwkey,                "PLFK000634
            mbew-bklas     TO sum_tab-bklas,                "PLFK000634
            t025t-bkbez    TO sum_tab-bkbez.                "PLFK000634
    ENDAT.

*-- AT NEW MBEW-BWTAR -------------------------------------------------*
    AT NEW mbew-bwtar.
    ENDAT.
*-- LINE OUTPUT -------------------------------------------------------*
    PERFORM print_line.

*-- AT END OF MBEW-BWKEY ----------------------------------------------*
    AT END OF mbew-bwkey.
      PERFORM change_nbm.
      first_nbm = on.
      PERFORM change_bklas.
      first_bklas = on.
      FORMAT INTENSIFIED COLOR COL_KEY.
      WRITE: / text-011, sum_bwkey CURRENCY t001-waers
                                   UNDER mbew-salk3.
      FORMAT INTENSIFIED COLOR COL_NORMAL.
      ULINE.
      SKIP.
      MOVE: mbew-bwkey TO bw_tab-bwkey,                     "PLFK000634
            sum_bwkey  TO bw_tab-summe.                     "PLFK000634
      APPEND bw_tab.                                        "PLFK000634
      CLEAR sum_bwkey.
    ENDAT.

*-- AT END OF MBEW-BKLAS-----------------------------------------------*
    AT END OF mbew-bklas.
      PERFORM change_nbm.
      first_nbm = on.
      PERFORM change_bklas.
    ENDAT.
*-- AT END OF MARA-J_1BNM ---------------------------------------------*
    AT END OF marc-steuc.
      PERFORM change_nbm.
    ENDAT.

  ENDLOOP.

  FORMAT INTENSIFIED COLOR COL_KEY.
  WRITE: / text-010, sum_total CURRENCY t001-waers
                               UNDER mbew-vmsal.
  FORMAT INTENSIFIED COLOR COL_NORMAL.
  ULINE.
*--- summing overview -------------------------------------------------*
  no_forward = on.                                          "PLFK000634
  NEW-PAGE.                                                 "PLFK000634
  LOOP AT sum_tab.                                          "PLFK000634
    IF sy-tabix = 1.                                        "PLFK000634
      FORMAT INTENSIFIED COLOR COL_GROUP.                   "PLFK000634
      WRITE text-017.                                       "PLFK000634
      SKIP 1.                                               "PLFK000634
      FORMAT INTENSIFIED COLOR COL_NORMAL.                  "PLFK000634
    ENDIF.                                                  "PLFK000634
                                                            "PLFK000634
    AT NEW bwkey.                                           "PLFK000634
      FORMAT INTENSIFIED COLOR COL_KEY.                     "PLFK000634
      WRITE: / text-007, sum_tab-bwkey.                     "PLFK000634
      FORMAT INTENSIFIED COLOR COL_NORMAL.                  "PLFK000634
    ENDAT.                                                  "PLFK000634
                                                            "PLFK000634
    WRITE: / text-003 , sum_tab-bklas,                      "PLFK000634
             sum_tab-bkbez,                                 "PLFK000634
             sum_tab-summe CURRENCY t001-waers.             "PLFK000634
                                                            "PLFK000634
    AT END OF bwkey.                                        "PLFK000634
      READ TABLE bw_tab WITH KEY bwkey = sum_tab-bwkey.     "PLFK000634
      WRITE: / text-005,                                    "PLFK000634
               bw_tab-summe CURRENCY t001-waers             "PLFK000634
               UNDER sum_tab-summe.                         "PLFK000634
    ENDAT.                                                  "PLFK000634
                                                            "PLFK000634
  ENDLOOP.                                                  "PLFK000634

*======================================================================*
* TOP-OF-PAGE                                                          *
*======================================================================*
TOP-OF-PAGE.

  pagno = pagno + 1.
  IF pagno > booksize.
    pagno = 2.
  ENDIF.

  FORMAT INTENSIFIED COLOR COL_HEADING.                     "PLFK001575

  WRITE: /50  text-200,
         161 sy-vline.                                      "PLFK001575
  WRITE: /1   text-210,                                     "PLFK001575
          22  branch_data-name,                             "PLFK001575
         161 sy-vline.                                      "PLFK001575
  WRITE: /1   text-220,                                     "PLFK001575
          22  branch_data-state_insc,                       "PLFK001575
          85  text-230,                                     "PLFK001575
          110 cgc_number,                                   "PLFK001575
          161 sy-vline.                                     "PLFK001575
  WRITE: /1   text-240,                                     "PLFK001575
         22   pagno,                                        "PLFK001575
         85   text-250,                                     "PLFK001575
         110  p_lfmon, p_lfgja,                             "PLFK001575
         161 sy-vline.                                      "PLFK001575
  ULINE.                                                    "PLFK001575
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.                 "PLFK001575
  WRITE: / text-019, 119 text-018.
  WRITE: / text-001, 59 text-020, 78 text-002.         "text-005.
  ULINE.
  FORMAT INTENSIFIED COLOR COL_NORMAL.

  IF sy-pagno <> 1.
    PERFORM forward.
  ENDIF.
*======================================================================*
* END-OF-PAGE                                                          *
*======================================================================*
END-OF-PAGE.
  PERFORM forward.

*======================================================================*
* FORM ROUTINES
*======================================================================*

*&---------------------------------------------------------------------*
*&      Form  READ_MBEW
*&---------------------------------------------------------------------*
*       Material valuation                                             *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mbew USING werks.
  SELECT        * FROM  mbew
         WHERE  matnr       IN p_matnr
         AND       bwkey       = werks
         AND       bklas       IN so-bklas.                 "PLFK002380
*-- Material with split valuation (sum record) ------------------------*
    IF mbew-bwtar IS INITIAL AND
      NOT mbew-bwtty IS INITIAL.
      CONTINUE.
    ENDIF.

* ---- Note 378233 -----------------------------------------------
* --- BEGIN OF Insertion --------------

    IF only_mbew = on.
*     Check if entry in mbewh exists for selected period/year.
*     Then take this one.
      SELECT SINGLE * FROM mbewh
            WHERE matnr = mbew-matnr
            AND   bwkey = mbew-bwkey
            AND   bwtar = mbew-bwtar
            AND   lfgja = p_lfgja
            AND   lfmon = p_lfmon.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING mbewh TO mbew.
        mbew-vmkum = mbewh-lbkum.
        mbew-vmsal = mbewh-salk3.
      ENDIF.
*     Else the values in mbew-vmkum and mbew-vmsal must be taken.
    ELSE.
*     only_mbew = off. Look in MBEWH.
      CALL FUNCTION 'J_1B_READ_MBEW_MBEWH'
           EXPORTING
                i_mbew       = mbew
           IMPORTING
                e_mbew       = mbew
           EXCEPTIONS
                no_selection = 1
                OTHERS       = 2.
      CHECK sy-subrc IS INITIAL.       "no history values found
    ENDIF.

* --- END OF Insertion --------------

* ---- Note 378233 -----------------------------------------------
* --- BEGIN OF Deletion ---------------

*    CALL FUNCTION 'J_1B_READ_MBEW_MBEWH'
*         EXPORTING
*              i_mbew       = mbew
*         IMPORTING
*              e_mbew       = mbew
**             E_MBEWH      =
*         EXCEPTIONS
*              no_selection = 1
*              OTHERS       = 2.
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    CHECK sy-subrc IS INITIAL.         "no history values found

* --- END OF Deletion ---------------
* ---- Note 378233 -----------------------------------------------

    CHECK NOT mbew-vmkum IS INITIAL.                        "PLFK001575

*--  Material A-Segment when changed-----------------------------------*
    ON CHANGE OF mbew-matnr.
      PERFORM read_mara USING mbew-matnr werks mbew-bwkey.
    ENDON.

    CHECK t134m-wertu = on.
*-- create extract ----------------------------------------------------*
    EXTRACT item.
    CLEAR item.
*----------------------------------------------------------------------*
  ENDSELECT.
ENDFORM.                               " READ_MBEW

*&---------------------------------------------------------------------*
*&      Form  READ_MARA
*&---------------------------------------------------------------------*
*       Material plant                                                 *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mara USING matnr wrk xbwkey.
  ch_flag = on.
  SELECT SINGLE * FROM  mara
         WHERE  matnr       = matnr.

  SELECT SINGLE * FROM t134m
   WHERE bwkey = xbwkey
   AND   mtart = mara-mtart.
  IF NOT sy-subrc IS INITIAL.
    WRITE: / text-014, xbwkey,
             text-015, mara-mtart,
             text-016, mara-matnr.
    STOP.
  ENDIF.
* format intensified color col_total.
* write: / 'MARA', mara-matnr, mara-j_1bnbm.
*--  Material Shorttext -----------------------------------------------*
  SELECT SINGLE * FROM  makt
         WHERE  matnr       = mara-matnr
         AND    spras       = sy-langu       .
* write: / 'MAKT', makt-matnr, makt-maktx.
* format intensified color col_normal.
  SELECT SINGLE * FROM marc
   WHERE matnr  = matnr
   AND   werks  = wrk.
ENDFORM.                               " READ_MARA
*&---------------------------------------------------------------------*
*&      Form  FORWARD
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM forward.
  IF no_forward = on.                                       "PLFK000634
    EXIT.                                                   "PLFK000634
  ENDIF.                                                    "PLFK000634

  FORMAT INTENSIFIED COLOR COL_TOTAL.
  WRITE: /105 text-006, sum_total CURRENCY t001-waers.
  FORMAT INTENSIFIED COLOR COL_NORMAL.
ENDFORM.                               " FORWARD
*&---------------------------------------------------------------------*
*&      Form  PRINT_LINE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_line.
  RESERVE 4 LINES.
*del WRITE: / MARC-STEUC, MAKT-MAKTX, MBEW-MATNR, MARA-MEINS,note 335933
*del           MBEW-VMKUM UNIT MARA-MEINS.                   note 335933

  WRITE: / marc-steuc NO-ZERO,                             "note 335933
           makt-maktx,                                     "note 335933
           mbew-matnr,                                     "note 335933
           mara-meins,                                     "note 335933
           mbew-vmkum UNIT mara-meins.                     "note 335933

  IF mbew-vprsv = 'V'.                 "moving average price
*del    mbew-vmver = mbew-vmver / mbew-vmpei. "note 151588
*del    WRITE MBEW-VERPR CURRENCY T001-WAERS. "note 151588
    mbew-verpr = mbew-verpr / mbew-peinh.                  "note 151588
    WRITE mbew-verpr CURRENCY t001-waers.                  "note 151588
  ELSE.                                "standard price
*del    mbew-vmstp = mbew-vmstp / mbew-vmpei. "note 151588
*del    WRITE MBEW-STPRS CURRENCY T001-WAERS. "note 151588
    mbew-stprs = mbew-stprs / mbew-peinh.                  "note 151588
    WRITE mbew-stprs CURRENCY t001-waers.                  "note 151588
  ENDIF.
  WRITE: mbew-vmsal CURRENCY t001-waers.
  WRITE: / mbew-bwtar UNDER makt-maktx.
  ADD: mbew-vmsal TO sum_total,
       mbew-vmsal TO sum_nbm,
       mbew-vmsal TO sum_bwkla,
       mbew-vmsal TO sum_bwkey.

ENDFORM.                               " PRINT_LINE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_NBM
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_nbm.
  IF first_nbm = on.
    first_nbm = off.
  ELSE.
    FORMAT INTENSIFIED COLOR COL_POSITIVE.
    WRITE: / text-004, sum_nbm CURRENCY t001-waers UNDER mbew-vmsal.
    CLEAR sum_nbm.
    FORMAT INTENSIFIED COLOR COL_NORMAL.
  ENDIF.
ENDFORM.                               " CHANGE_NBM
*&---------------------------------------------------------------------*
*&      Form  CHANGE_BKLAS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_bklas.
  IF first_bklas = on.
    first_bklas = off.
  ELSE.
    FORMAT INTENSIFIED COLOR COL_NEGATIVE.
    WRITE: / text-009, sum_bwkla CURRENCY t001-waers UNDER mbew-vmsal.
    FORMAT INTENSIFIED COLOR COL_NORMAL.
    MOVE sum_bwkla TO sum_tab-summe.                        "PLFK000634
    APPEND sum_tab.                                         "PLFK000634
    CLEAR sum_bwkla.
    SKIP 1.
  ENDIF.
ENDFORM.                               " CHANGE_BKLAS
*&---------------------------------------------------------------------*
*&      Form  READ_MBEW_H
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PLANTS_WERKS  text
*----------------------------------------------------------------------*
*FORM read_mbew_h USING  xwerks.
*
*  SELECT        * FROM  mbewh
**         where  matnr       = matnr
*         WHERE    bwkey       = xwerks
*           AND    lfgja       = p_lfgja
*           AND    lfmon       = p_lfmon.
**-- Material with split valuation (sum record)------------------------*
**   if mbewh-bwtar is initial and
**     not mbewh-bwtty is initial.
**     continue.
**   endif.
*
*    CHECK NOT mbewh-lbkum IS INITIAL.                       "PLFK001575
*
**--  Material A-Segment whenchanged-----------------------------------*
*    ON CHANGE OF mbewh-matnr.
*      PERFORM read_mara USING mbewh-matnr xwerks mbewh-bwkey.
*    ENDON.
*
*    CHECK t134m-wertu = on.
**-- create extract----------------------------------------------------*
*
*    EXTRACT item.
**----------------------------------------------------------------------
*
*  ENDSELECT.
*ENDFORM.                               " READ_MBEW_H
