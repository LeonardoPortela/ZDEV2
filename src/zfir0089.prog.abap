*&---------------------------------------------------------------------*
*& Report  ZFIR0089
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir0089.

TABLES: zfit0164.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_zfit0164,
         transno           TYPE zfit0164-transno,
         entrydate         TYPE zfit0164-entrydate,
         status            TYPE zfit0164-status,
         seqitem           TYPE zfit0164-seqitem,
         actdate           TYPE zfit0164-actdate,
         invoiceno         TYPE zfit0164-invoiceno,
         oprbillsource     TYPE zfit0164-oprbillsource,
         detail_memo       TYPE zfit0164-detail_memo,
         detail_vesselcode TYPE zfit0164-detail_vesselcode,
         obj_key           TYPE zib_contabil_chv-obj_key,
       END OF ty_zfit0164.

DATA: git_zfit0164         TYPE TABLE OF ty_zfit0164,
      git_zib_contabil_chv TYPE TABLE OF zib_contabil_chv.


DATA: gwa_zfit0164         TYPE ty_zfit0164,
      gwa_zib_contabil_chv LIKE LINE OF git_zib_contabil_chv,
      gwa_zfit0036         TYPE zfit0036.



START-OF-SELECTION.
  IF sy-batch = 'X'.
    PERFORM fm_start_of_selection.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_start_of_selection .

  SELECT transno
         entrydate
         status
         seqitem
         actdate
         invoiceno
         oprbillsource
         detail_memo
         detail_vesselcode
       FROM  zfit0164
    INTO TABLE git_zfit0164
   WHERE ck_status_invoice = ''.

  IF git_zfit0164[] IS NOT INITIAL.

    LOOP AT git_zfit0164 INTO gwa_zfit0164.
      CONCATENATE  gwa_zfit0164-transno  gwa_zfit0164-actdate+0(4) INTO gwa_zfit0164-obj_key.
      MODIFY git_zfit0164 FROM gwa_zfit0164 INDEX sy-tabix TRANSPORTING obj_key.
      CLEAR: gwa_zfit0164.
    ENDLOOP.

    SELECT  *
        FROM zib_contabil_chv
     INTO TABLE git_zib_contabil_chv
      FOR ALL ENTRIES IN git_zfit0164
      WHERE obj_key EQ  git_zfit0164-obj_key.

    IF git_zib_contabil_chv[] IS NOT INITIAL.
      PERFORM save_data.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data .

  LOOP AT git_zfit0164 INTO gwa_zfit0164.

    READ TABLE git_zib_contabil_chv INTO gwa_zib_contabil_chv WITH KEY obj_key =  gwa_zfit0164-obj_key.

    IF sy-subrc = 0.
      gwa_zfit0036-obj_key    = gwa_zfit0164-obj_key.
      gwa_zfit0036-bukrs      = '0200'.
      gwa_zfit0036-invoice    = gwa_zfit0164-invoiceno.
      gwa_zfit0036-referencia	=	gwa_zfit0164-detail_memo.
      gwa_zfit0036-navio      =	gwa_zfit0164-detail_vesselcode.

      MODIFY zfit0036 FROM gwa_zfit0036.

      UPDATE zfit0164
         SET ck_status_invoice  = abap_true
       WHERE transno    = gwa_zfit0164-transno
         AND entrydate  = gwa_zfit0164-entrydate
         AND status     = gwa_zfit0164-status
         AND seqitem    = gwa_zfit0164-seqitem.

      CLEAR: gwa_zfit0036, gwa_zfit0164,gwa_zib_contabil_chv .

    ENDIF.

  ENDLOOP.


ENDFORM.
