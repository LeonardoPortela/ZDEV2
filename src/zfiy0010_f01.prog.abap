*&---------------------------------------------------------------------*
*&  Include           ZFIY0010_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM check_selection_screen.
* check that only one company code entered
  DESCRIBE TABLE br_bukrs LINES xlines.

  IF xlines       NE 1       OR
     br_bukrs-low IS INITIAL OR
     ( br_bukrs-low NE br_bukrs-high AND
       NOT br_bukrs-high IS INITIAL ).

    MESSAGE e821.
  ENDIF.

* check, if one tax type has been selected
  ktosl_sel = 'X'.

  CLEAR xlines.
  DESCRIBE TABLE s_ktosl LINES xlines.

  IF xlines      EQ 0       AND
     s_ktosl-low IS INITIAL.
    CLEAR ktosl_sel.
  ENDIF.

  IF NOT s_newdat IS INITIAL.          " create dataset ?
    IF s_file IS INITIAL.              " dataset entered ?
      MESSAGE e811.
    ENDIF.

*   open dataset s_file for output in text mode.

    IF sy-subrc NE 0.
      MESSAGE e812 WITH s_file.
    ENDIF.
  ENDIF.
**Note 1013585 Begins
*Name of dataset should be mentioned for creating people withheld server file
  IF NOT p_sfile  IS INITIAL.
    IF p_sfnam IS INITIAL.
      MESSAGE e811.
    ENDIF.
  ENDIF.
*Name of the local file shoule be mentioned for creating local file for people withheld
  IF NOT p_lwfile IS INITIAL.
    IF p_lwfnam IS INITIAL.
      MESSAGE e454.
    ENDIF.
  ENDIF.
**Note 1013585 Ends
ENDFORM.                               " CHECK_SELECTION_SCREEN

*&---------------------------------------------------------------------*
*&      Form  SET_DATES
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM set_dates USING f_dat f_budat.
* --> Convert date fomat from YYYMMDD to DD/MM/YYYY
  IF NOT f_budat IS INITIAL.
    WRITE f_budat TO f_dat DD/MM/YYYY .
    f_dat+2(1) = '/'.
    f_dat+5(1) = '/'.
  ENDIF.
ENDFORM.                               " SET_DATES

*&---------------------------------------------------------------------*
*&      Form  READ_MASTER_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_master_data USING f_cpd.
  IF bseg-koart = 'D'.
    xhkont = bseg-kunnr.
  ELSE.
    xhkont = bseg-lifnr.
  ENDIF.

  IF NOT f_cpd IS INITIAL .
    IF bseg-koart = 'D'.
      CALL FUNCTION 'CUSTOMER_READ'
        EXPORTING
          i_bukrs = space
          i_kunnr = bseg-kunnr
        IMPORTING
          e_kna1  = kna1.
    ELSEIF bseg-koart = 'K' .
      CALL FUNCTION 'VENDOR_READ'
        EXPORTING
          i_bukrs = space
          i_lifnr = bseg-lifnr
        IMPORTING
          e_lfa1  = lfa1.
    ENDIF.

    IF ( bseg-koart = 'K' AND lfa1-xcpdk NE space ) OR
       ( bseg-koart = 'D' AND kna1-xcpdk NE space ).
* ---> It is  One-Time Account
      CLEAR bsec.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF mdata
                      FROM bsec WHERE bukrs = bseg-bukrs
                                AND   belnr = bseg-belnr
                                AND   gjahr = bseg-gjahr
                                AND   buzei = bseg-buzei.
      mdata-hkont = xhkont.
      CHECK 1 = 2.
    ENDIF.
  ENDIF.

  CHECK mdata-koart NE bseg-koart OR
        mdata-hkont NE xhkont.

  CLEAR: lfa1, kna1, mdata.

  READ TABLE mdata WITH KEY koart = bseg-koart
                            hkont = xhkont.

  CHECK sy-subrc NE 0.

  IF bseg-koart = 'D'.
* ---> It is a Customer
    SELECT SINGLE * FROM kna1 WHERE kunnr = bseg-kunnr.

    IF NOT kna1-fiskn IS INITIAL.
      SELECT SINGLE * FROM kna1 WHERE kunnr = kna1-fiskn.
    ENDIF.
    APPEND kna1 TO t_kna1.
    MOVE-CORRESPONDING kna1 TO mdata.
    mdata-koart = bseg-koart.
    mdata-hkont = bseg-kunnr.
  ELSEIF bseg-koart = 'K' .
* ---> It is a Vendor
    SELECT SINGLE * FROM lfa1 WHERE lifnr = bseg-lifnr.

    IF NOT lfa1-fiskn IS INITIAL.
      SELECT SINGLE * FROM lfa1 WHERE lifnr = lfa1-fiskn.
    ENDIF.
    APPEND lfa1 TO t_lfa1.
    MOVE-CORRESPONDING lfa1 TO mdata.
    mdata-koart = bseg-koart.
    mdata-hkont = bseg-lifnr.
  ENDIF.

  APPEND mdata.
ENDFORM.                               " READ_MASTER_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_VALUES
*&---------------------------------------------------------------------*
FORM read_values.

  IF ( bseg-koart = 'D' ) OR
     ( bseg-koart = 'K' AND bseg-shkzg = 'S' ) .
* ---> The following line was changed. An additional condition    502917
* --->     is necessary.                                          502917
*    IF NOT X_CM IS INITIAL .                      "     INSERT    502917
    IF ( NOT x_cm IS INITIAL ) OR ( bseg-koart = 'D' ).    "     Note 984952
* ---> The document includes a Credit Memo item                   502917
      detail-dmbtr = detail-dmbtr + bseg-dmbtr.

*Note 1134541 Begins
      CLEAR x_cm.
*Note 1134541 Ends
    ELSE .                                        "     INSERT    502917
      detail-dmbtr = bseg-dmbtr.                 "     INSERT    502917
    ENDIF .                                       "     INSERT    502917
  ELSEIF bseg-koart = 'K' AND bseg-shkzg = 'H' .
* ---> The following line was changed from an addition to a       502917
* --->     substraction.                                          502917
*   detail-dmbtr = detail-dmbtr + bseg-dmbtr.           DELETE    502917
    detail-dmbtr = detail-dmbtr - bseg-dmbtr.     "     INSERT    502917
* ---> I found a Credit memo.                                     502917
    x_cm = 'X' .                                  "     INSERT    502917
  ENDIF.

ENDFORM.                    " READ_VALUES

*&---------------------------------------------------------------------*
*&      Form  READ_WITHHLD_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_withhld_data.
* --> Loop on all withholding items. Do not select items with zero
* --> withholding
  SELECT *
* Incluído 30.08.2011 - Diego
    INTO TABLE t_item
* Fin 30.08.2011 ............
    FROM with_item
                       WHERE   bukrs       EQ bkpf-bukrs
                         AND   belnr       EQ bseg-belnr
                         AND   gjahr       EQ bseg-gjahr
                         AND   buzei       EQ bseg-buzei
                         AND   witht       IN s_witht
                         AND   wt_withcd   NE space
                         AND   wt_stat     EQ space
                         AND   wt_qbshh    NE 0.
* Incluído 30.08.2011 - Diego ...
    LOOP AT t_item INTO l_item.

    with_item = l_item.
* Fin 30.08.2011 ...............

* Eliminado 30.08.2011 - Diego ...
*   with_item-wt_qbshh = with_item-wt_qbshh * -1.
* Fin 30.08.2011 ...............

    PERFORM read_withhld_type.

    IF xt059p-wt_accpt = '0'.          " accumulation ?
      CHECK holding_sel IS INITIAL OR  " no, orig. Bukrs or
            t001-umkrs EQ bkpf-bukrs.  "     holding itself?
    ELSE.
      CHECK holding_sel NE space OR    " yes, holding sel.
            holding_act IS INITIAL.    " or holding not act.?
    ENDIF.

    PERFORM read_withhld_code.
    PERFORM read_off_wth_code.
    PERFORM fill_detail_header.
    PERFORM fill_detail_for_wth.

* Modificado en 30.08.2011 - Diego
    ENDLOOP.
*    APPEND with_item TO t_item.
*  ENDSELECT.
* Fin 30.08.2011 ...............

ENDFORM.                               " READ_WITHHLD_DATA

*&---------------------------------------------------------------------*
*&      Form  FILL_DETAIL_HEADER
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_detail_header.
* clear detail.

  detail-budat     = bkpf-budat.
  WRITE bkpf-budat TO detail-date DD/MM/YYYY .
  detail-date+2(1) = '/'.
  detail-date+5(1) = '/'.
  detail-stcdt     = mdata-stcdt.
  detail-stcd1     = mdata-stcd1.
  detail-stcd2     = mdata-stcd2.
  detail-bukrs     = bkpf-bukrs.
  detail-butxt     = t001-butxt .
*Note 1064177 Begin
*  WRITE   BKPF-BELNR  TO DETAIL-BELNR .
  MOVE bkpf-belnr TO detail-belnr.
*Note 1064177 End
  MOVE :  bkpf-gjahr  TO detail-gjahr ,
          mdata-hkont TO detail-hkont ,
          mdata-name1 TO detail-name1 .

  IF mdata-land1 NE t001-land1.
    PERFORM read_foreign_id
            USING mdata-land1 mdata-stkzn
            CHANGING xstcd1.
  ENDIF.

**Note 1013585 Begins
  PERFORM fill_withheld.
**Note 1013585 Ends

  IF mdata-stcdt = '80'.
    ystcd1 = xstcd1.
    CLEAR xstcd1.
    WRITE ystcd1 USING EDIT MASK '__-________-_'
          TO xstcd1.
  ENDIF.

ENDFORM.                               " FILL_DETAIL_HEADER

*&---------------------------------------------------------------------*
*&      Form  READ_WITHHLD_TYPE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_withhld_type.
* --> Implement internal table for performance reasons
  CLEAR: xt059p, t059p.
  READ TABLE xt059p WITH KEY mandt = sy-mandt
                             land1 = t001-land1
                             witht = with_item-witht.
  CHECK sy-subrc NE 0.
  SELECT SINGLE * FROM t059p WHERE land1 = t001-land1
                             AND   witht = with_item-witht.
  MOVE-CORRESPONDING t059p TO xt059p.
  APPEND xt059p.

ENDFORM.                               " READ_WITHHLD_TYPE

*&---------------------------------------------------------------------*
*&      Form  READ_WITHHLD_CODE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_withhld_code.
  CHECK xt059z-land1      NE t001-land1 OR
        xt059z-witht      NE with_item-witht OR
        xt059z-wt_withcd  NE with_item-wt_withcd.

  CLEAR: xt059z, t059z.

  READ TABLE xt059z WITH KEY land1      = t001-land1
                             witht      = with_item-witht
                             wt_withcd  = with_item-wt_withcd.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM t059z
                 WHERE land1     = t001-land1
                 AND   witht     = with_item-witht
                 AND   wt_withcd = with_item-wt_withcd. "#EC CI_GENBUFF

  MOVE-CORRESPONDING t059z TO xt059z.

  APPEND xt059z.
ENDFORM.                               " READ_WITHHLD_CODE

*&---------------------------------------------------------------------*
*&      Form  READ_OFF_WTH_CODE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_off_wth_code.
  CHECK xj1awtoff-land1     NE t001-land1 OR
         xj1awtoff-wt_qscod NE xt059z-qscod.

  CLEAR: t059ot, xj1awtoff.

  READ TABLE xj1awtoff WITH KEY mandt     = sy-mandt
                                land1     = t001-land1
                                 wt_qscod = xt059z-qscod.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM t059ot WHERE land1    = t001-land1
                              AND   spras    = sy-langu
                              AND   wt_qscod = xt059z-qscod.

  MOVE-CORRESPONDING t059ot TO xj1awtoff.

  APPEND xj1awtoff.
ENDFORM.                               " READ_OFF_WTH_CODE

*&---------------------------------------------------------------------*
*&      Form  READ_OFF_TAX_CODE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_off_tax_code.
  CHECK xtpkof-land1 NE t001-land1 OR
        xtpkof-ktosl NE bset-ktosl.

  CLEAR: j_1atpkof, j_1atxoff, xtpkof.

  READ TABLE xtpkof WITH KEY mandt = sy-mandt
                             land1 = t001-land1
                             ktosl = bset-ktosl.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM j_1atpkof WHERE land1 = t001-land1
                                 AND   ktosl = bset-ktosl.

  IF sy-subrc = 0.
    SELECT SINGLE * FROM j_1atxoff
                    WHERE land1 = t001-land1
                    AND   j_1ataxcod = j_1atpkof-j_1ataxcod.
  ENDIF.

  MOVE-CORRESPONDING: j_1atpkof TO xtpkof,
                      j_1atxoff TO xtpkof.

  APPEND xtpkof.
ENDFORM.                               " READ_OFF_TAX_CODE

*&---------------------------------------------------------------------*
*&      Form  FILL_DETAIL_FOR_WTH
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_detail_for_wth.
***Note # 984952 begin
*  DETAIL-OFFNR = BKPF-BELNR .
***Note # 984952 end
  detail-qscod = xt059z-qscod.
  detail-tcode = '1'                .
  detail-hwbas = with_item-wt_qsshh .
  detail-hwste = with_item-wt_qbshh * -1 .
  detail-ctnum = with_item-ctnumber .
  detail-whsub = '0'.               "Note 1230326

  PERFORM read_tax_code USING '1'
                              detail-qscod
                     CHANGING detail-txcod .

  SELECT SINGLE * FROM lfbw
                 WHERE lifnr = detail-hkont
                   AND bukrs = t001-bukrs
                   AND witht = with_item-witht .
*****Note 1157456 Begins****
*Note 1067719 Begin
***Note 1015466 Begins
*  MOVE : LFBW-QSREC   TO DETAIL-FITYP .
*  MOVE LFA1-FITYP TO DETAIL-FITYP.
***Note 1015466 Ends
*  MOVE MDATA-FITYP TO DETAIL-FITYP.
*Note 1067719 End
  IF with_item-qsrec IS NOT INITIAL.
    MOVE with_item-qsrec TO detail-fityp.
  ELSE.
    MOVE : lfbw-qsrec   TO detail-fityp .
  ENDIF.
*****Note 1157456 Ends****
  IF  NOT  with_item-wt_qszrt IS INITIAL .
    MOVE  with_item-wt_qszrt TO detail-exrt  .
  ELSE .
    detail-exrt = space .                        " <---  INSERT 396721
  ENDIF .

  IF NOT detail-exrt IS INITIAL AND
       ( lfbw-wt_exdf LE detail-budat ) .
    WRITE lfbw-wt_exdf TO detail-exdf DD/MM/YYYY .
    detail-exdf+2(1) = '/' .                  " <---  INSERT 396721
    detail-exdf+5(1) = '/' .                  " <---  INSERT 396721
  ELSE  .
    detail-exrt = space .                     " <---  INSERT 396721
    detail-exdf = space .                     " <---  INSERT 396721
  ENDIF .

  IF mdata-land1 NE t001-land1.
    PERFORM read_foreign_id
            USING mdata-land1 mdata-stkzn
            CHANGING xstcd1.
    IF  NOT xstcd1 IS INITIAL .
      MOVE :  xstcd1   TO detail-fptid ,
              hd_ccuit TO detail-cctid .
    ELSE .
      CLEAR : detail-butxt ,
              detail-grsup ,
              detail-fptid ,
              detail-cctid .
    ENDIF.
  ELSE .
    CLEAR : detail-butxt ,
            detail-grsup ,
            detail-fptid ,
            detail-cctid .
  ENDIF.

  IF xt059z-wt_posin EQ '2' .
    detail-grsup = '1' .
  ELSE .
    detail-grsup = '0' .
  ENDIF .

  CLEAR tot_code.

  IF   detail-cancel IS INITIAL .
    MOVE-CORRESPONDING detail TO tot_code.
* Begin of GB - 23.09.10
    tot_code-buzei = with_item-buzei.
    tot_code-witht = with_item-witht.
* End of GB - 23.09.10
*Note 572862 TO DISPLAY THE SUMMARY AT THE END CORRECTLY
*WHERE CREDIT MEMOS ARE INVOLVED.
    IF ( detail-class = '03' ) OR ( detail-class = '08' ).
**Note 1064177 Begin
*        TOT_CODE-HWSTE =  TOT_CODE-HWSTE * -1.
      IF tot_code-hwste > 0.
        tot_code-hwste =  tot_code-hwste * -1.
      ENDIF.
**Note 1064177 End
    ENDIF.
  ENDIF .
  tot_code-text40 = xj1awtoff-text40.

*  COLLECT: detail, tot_code.
  COLLECT: detail.
  APPEND tot_code.

ENDFORM.                               " FILL_DETAIL_FOR_WTH

*&---------------------------------------------------------------------*
*&      Form  FILL_DETAIL_FOR_TAX
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_detail_for_tax.

  detail-hwbas  = bset-hwbas .
  detail-hwste  = bset-hwste * -1 .
  detail-posneg = '1'.
  detail-whsub = '0'.         "Note 1230326

  detail-perception = 'X'.    "Note 1064177
* if bset-shkzg = 'H' .                             <---   Delete 396721
  IF bset-shkzg = 'H'                         "     <---   Insert 396721
  OR detail-hwste LT 0 .                      "     <---   Insert 396721
    detail-hwste  = detail-hwste * -1.
    detail-posneg = '1'.
  ENDIF.

* ---> Read Official Tax Type
  READ TABLE t_offtyp WITH KEY land1 = t001-land1
                               ktosl = bset-ktosl .
  IF sy-subrc IS INITIAL .
    detail-qscod = t_offtyp-j_1ataxcod .
    detail-tcode = k_tax .
    PERFORM read_tax_code USING '2'
                                detail-qscod
                       CHANGING detail-txcod .
  ENDIF .

* ---> Read Condition Code
  READ TABLE t_concod WITH KEY spras    = sy-langu
                               j_1afitp = mdata-fityp .
  IF sy-subrc IS INITIAL .
* Note 557711: Print correct tax category
    WRITE t_concod-j_1afitp TO detail-fityp RIGHT-JUSTIFIED.
  ENDIF .

* ---> Read Tax identification
  SELECT SINGLE * FROM j_1ataxid
                 WHERE kalsm = t005-kalsm
                   AND ktosl = bset-ktosl .
  IF sy-subrc IS INITIAL AND
     j_1ataxid-j_1ataxid = 'VP01' .
    detail-exrt = space .                         " <---  INSERT 396721
    detail-exdf = space .                         " <---  INSERT 396721
    IF bkpf-awtyp = 'VBRK' .

    ELSE .

    ENDIF .
  ELSEIF sy-subrc IS INITIAL .
    READ TABLE t_txgrp WITH KEY koart = 'D'
                                kschl = bset-kschl .
    SELECT SINGLE * FROM knat
                   WHERE kunnr = detail-hkont
                     AND taxgr = t_txgrp-taxgr .
    IF sy-subrc IS INITIAL AND
       NOT knat-exrt IS INITIAL AND
         ( detail-budat GE knat-exdf AND
           detail-budat LE knat-exdt ) .
      detail-exrt = knat-exrt .
      WRITE knat-exdf TO detail-exdf DD/MM/YYYY .
      detail-exdf+2(1) = '/' .                 " <---  Update 396721
      detail-exdf+5(1) = '/' .                 " <---  Update 396721
    ELSEIF sy-subrc IS INITIAL .
      detail-exrt = space .                    " <---  Update 396721
      detail-exdf = space .                    " <---  Update 396721
    ENDIF .
  ENDIF .

  CLEAR :  tot_code ,  detail-butxt .

  IF   detail-cancel IS INITIAL .
    MOVE-CORRESPONDING detail TO tot_code.
* Begin of GB - 23.09.10
    tot_code-buzei = with_item-buzei.
    tot_code-witht = with_item-witht.
* End of GB - 23.09.10
*Note 572862 TO DISPLAY THE SUMMARY AT THE END CORRECTLY
*WHERE CREDIT MEMOS ARE INVOLVED.
    IF ( detail-class = '03' ) OR ( detail-class = '08' ).
**Note 1064177 Begin
*      TOT_CODE-HWSTE =  TOT_CODE-HWSTE * -1.
      IF tot_code-hwste > 0.
        tot_code-hwste =  tot_code-hwste * -1.
      ENDIF.
**Note 1064177 End
    ENDIF.
  ENDIF .
  tot_code-text40 = t_offtyp-text40.
****************************************************************
*Note 591818 To report Base amount as the Tax amount in the case
*of Credit Memos
*****************************************************************
  IF ( detail-class = '03' ) OR ( detail-class = '08' ).
    detail-hwbas = detail-hwste.
*     detail-dmbtr = bseg-dmbtr.                   " Note 700313
  ENDIF.

*  COLLECT: detail, tot_code.
  COLLECT: detail.
  APPEND tot_code.
***Note 984952 Begin
***The Base amount has been remapped to currency
***So in a sceanrio where you have multiple VAT lines
***the DMBTR field gets added. It is initialised here so that only
***VAT amount gets added. DMBTR is added in GET BSEG
  CLEAR detail-dmbtr.
***Note 984952 End
ENDFORM.                               " FILL_DETAIL_FOR_TAX
*&---------------------------------------------------------------------*
*&      Form  PRINT_DETAIL
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_detail.

  DATA : l_tmp_amt(16).
***Note 1045955 Begins
*  SORT DETAIL BY CLASS OFFNR BELNR .
  DATA : tmp_belnr LIKE detail-belnr,
  tmp_bukrs LIKE detail-bukrs.
  SORT detail BY class offnr belnr bukrs.
***Note 1045955 Ends

  xheader = 1.
*  NEW-PAGE.       "ALV Comment

  LOOP AT detail.
    IF detail-dmbtr < 0.
      detail-posneg = '0'.
    ELSE.
      detail-posneg = '1'.
    ENDIF.

* ALV Comment Start
*    FORMAT: INTENSIFIED OFF,
*            COLOR OFF.
*
*    RESERVE 3 LINES.

* ALV Comment ends

    x_text = detail-dmbtr .
    TRANSLATE x_text USING '.,'.

*   if  detail-exrt is initial .                   <---  Update 396721
*       detail-exrt = space .                      <---  Update 396721
*   else .                                         <---  Update 396721
*                                                  <---  Update 396721
*   endif .                                        <---  Update 396721

*   if  detail-exdf is initial .                   <---  Update 396721
*       detail-exdf  = space .                     <---  Update 396721
*   else .                                         <---  Update 396721
*       detail-exdf+2(1) = '/' .                   <---  Update 396721
*       detail-exdf+5(1) = '/' .                   <---  Update 396721
*   endif .                                        <---  Update 396721

* ALV Comment Start
*    IF  DETAIL-CANCEL IS INITIAL .
*        FORMAT  COLOR COL_NORMAL  INTENSIFIED ON  INVERSE OFF .
*    ELSE .
*        FORMAT  COLOR COL_NEGATIVE  INTENSIFIED OFF  INVERSE OFF .
*    ENDIF .
*    WRITE:/ DETAIL-BELNR RIGHT-JUSTIFIED ,
*            DETAIL-GJAHR            ,
*            DETAIL-HKONT            ,
*            DETAIL-NAME1            .
* ALV Comment ends

* ALV CHANGES Start
    MOVE:     detail-belnr  TO gs_header-belnr,
              detail-gjahr  TO gs_header-gjahr,
              detail-hkont  TO gs_header-hkont,
              detail-name1  TO gs_header-name1,
              detail-bukrs  TO gs_header-bukrs.
***Note 1045955 Begins
*    AT NEW belnr.
*      APPEND gs_header TO gt_header.
*      CLEAR gs_header.
*    ENDAT.
    IF detail-belnr NE tmp_belnr OR detail-bukrs NE tmp_bukrs.
      APPEND gs_header TO gt_header.
      CLEAR gs_header.
    ENDIF.
    tmp_bukrs = detail-bukrs.
    tmp_belnr = detail-belnr.
***Note 1045955 Ends

* ALV CHANGES ends
* ALV Comment Start
*    IF  DETAIL-CANCEL IS INITIAL .
*        FORMAT  COLOR OFF         INTENSIFIED OFF INVERSE OFF .
*    ELSE .
*        FORMAT  COLOR COL_TOTAL     INVERSE  ON  .
*    ENDIF .
*    HIDE: DETAIL-BUKRS, DETAIL-GJAHR, DETAIL-BELNR.   " Note 692759
*
*    WRITE:/ DETAIL-CLASS            ,
*            DETAIL-DATE             ,
*            DETAIL-OFFNR RIGHT-JUSTIFIED ,
*            X_TEXT                  .
* ALV Comment ends

* ALV CHANGES starts

    MOVE: detail-belnr  TO gs_item-belnr,
        detail-gjahr  TO gs_item-gjahr,
        detail-hkont  TO gs_item-hkont,
        detail-name1  TO gs_item-name1,
        detail-class  TO gs_item-class,
        detail-date   TO gs_item-date,
        detail-offnr  TO gs_item-offnr,
        x_text        TO gs_item-x_text.

* ALV CHANGES ends

    CLEAR x_text .
    x_text = detail-hwbas .
    TRANSLATE x_text USING '.,'.

* ALV Comment Start
*    WRITE : detail-txcod            ,
*            detail-qscod            ,
*            detail-tcode            ,
*            x_text                  .
* ALV Comment ends

* ALV CHANGES starts
    MOVE: detail-txcod  TO gs_item-txcod,
          detail-qscod  TO gs_item-qscod,
          detail-tcode  TO gs_item-tcode,
          x_text        TO gs_item-x_text1.

* ALV CHANGES ends

    CLEAR x_text .
    x_text = detail-hwste .
    TRANSLATE x_text USING '.,'.
    TRANSLATE detail-exrt USING '.,'."Note 613436

* ALV Comment Start
*    WRITE : DETAIL-DATE  ,
*            DETAIL-FITYP ,
*            X_TEXT       ,
*            DETAIL-EXRT  RIGHT-JUSTIFIED ,
*            DETAIL-EXDF  ,
*            DETAIL-STCDT ,
*            DETAIL-STCD1 ,
*            DETAIL-CTNUM ,
*            DETAIL-BUTXT ,
*            DETAIL-GRSUP ,
*            ' '          ,
*            DETAIL-FPTID ,
*            DETAIL-CCTID .
* ALV Comment ends

* ALV CHANGES starts

    MOVE: detail-date   TO gs_item-date1,
          detail-fityp  TO gs_item-fityp,
          x_text        TO gs_item-x_text2,
          detail-exrt   TO gs_item-exrt,
          detail-exdf   TO gs_item-exdf,
          detail-stcdt  TO gs_item-stcdt,
          detail-stcd1  TO gs_item-stcd1,
          detail-ctnum  TO gs_item-ctnum,
          detail-butxt  TO gs_item-butxt,
          detail-grsup  TO gs_item-grsup,
          detail-fptid  TO gs_item-fptid,
          detail-cctid  TO gs_item-cctid.
    APPEND gs_item TO gt_item.
    CLEAR gs_item.

* ALV CHANGES ends

* ALV Comment starts
*    FORMAT: INTENSIFIED OFF,
*            COLOR OFF.
*
*    ULINE.
* ALV Comment ends
    IF s_newdat NE space.
      MOVE-CORRESPONDING detail TO detail_cl.
      COLLECT detail_cl.
    ENDIF.

    AT LAST.
      IF s_newdat NE space.
* ---> Create dataset only when valid entries exists
        READ TABLE detail WITH KEY cancel = ' ' .
* ---> Canceled documents are not transfered to dataset
        IF  sy-subrc IS INITIAL .
          OPEN DATASET s_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
        ELSE  .
* ---> When no dataset exists, no local file can be created
          CLEAR p_lfile .
        ENDIF .
        LOOP AT detail .
          CLEAR output_file .
          IF detail-dmbtr < 0.
            detail-posneg = '0'.
*            DETAIL-DMBTR = DETAIL_CL-DMBTR * -1.      "Note 749720
            detail-dmbtr = detail-dmbtr * -1.          "Note 749720
          ELSE.
            detail-posneg = '1'.
          ENDIF.
*Note # 984952 Begin
          detail-crlf = cl_abap_char_utilities=>cr_lf.
*Note # 984952 End
          TRANSLATE detail-stcd1 USING '- '.
          CONDENSE detail-stcd1 NO-GAPS.

          TRANSLATE detail-stcd2 USING '- '.
          CONDENSE detail-stcd2 NO-GAPS.

*  Note 748649 CHANGES START.
*          SHIFT DETAIL-DMBTR RIGHT.
          l_tmp_amt = detail-dmbtr.
          SHIFT l_tmp_amt RIGHT.
          detail-dmbtr = l_tmp_amt.
*  Note 748649 CHANGES END.

          output_file-date1 = output_file-date2 = detail-date .
          MOVE-CORRESPONDING detail    TO output_file .
**Note 1064177 Begins
*Change all the negative amounts to positive for the file
          MOVE abs( detail-dmbtr ) TO output_file-dmbtr.
          MOVE abs( detail-hwste ) TO output_file-hwste.
          MOVE abs( detail-hwbas ) TO output_file-hwbas.
**For reversal documents the base amount equals tax amt. for file
          IF detail-xreversal = '2' AND
          detail-perception IS INITIAL.
            output_file-hwbas = output_file-hwste.
          ENDIF.
**Note 1064177 Ends
          SHIFT output_file-offnr BY 4 PLACES RIGHT."Note 639146
* ---> Canceled documents are not transfered to dataset
* Note 572862 TO GET THE OUTPUT TRANSFERRED TO THE FILE IN THE
* CORRECT FORMAT
          TRANSLATE output_file-dmbtr USING '.,'.
          TRANSLATE output_file-hwste USING '.,'.
          TRANSLATE output_file-hwbas USING '.,'.
          TRANSLATE output_file-exrt  USING '.,'. "Note 613436
**Note # 984952 Begin
          SHIFT output_file-dmbtr RIGHT.
          SHIFT output_file-hwbas RIGHT.
          SHIFT output_file-hwste RIGHT.
          SHIFT output_file-ctnum BY 4 PLACES RIGHT.
          TRANSLATE output_file-ctnum USING ' 0'.
**Note # 984952 End
          IF   detail-cancel IS INITIAL .
**Note # 984952 Begin
            TRANSFER output_file TO s_file LENGTH 196 . " Note 688753
            MOVE output_file TO output_file1.
            COLLECT output_file1.

**Note # 984952 End
          ENDIF .
        ENDLOOP.
        CLOSE DATASET s_file.
      ENDIF.
    ENDAT.
  ENDLOOP.
ENDFORM.                               " PRINT_DETAIL
*&---------------------------------------------------------------------*
*&      Form  PRINT_TOT_CODE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_tot_code.
*  FORMAT INTENSIFIED OFF.
*
*  SORT tot_code.
*
*  xheader = 2.
**  NEW-PAGE.              "ALV Comment
*
*  DELETE tot_code WHERE hwste = 0.
*
*  LOOP AT tot_code.
** ALV Comment starts
**    WRITE:/ TOT_CODE-QSCOD,
**            TOT_CODE-TEXT40,
**            TOT_CODE-HWSTE CURRENCY T001-WAERS.
** ALV Comment ends
** ALV Changes starts
*    MOVE: tot_code-qscod  TO gs_tot_code-qscod,
*          tot_code-text40 TO gs_tot_code-text40,
*          tot_code-hwste  TO gs_tot_code-hwste.
*    APPEND gs_tot_code TO gt_tot_code.
*    CLEAR gs_tot_code.
** ALV Changes ends
*
** ALV Comment starts
**    AT LAST.
**      SUM.
**      FORMAT INTENSIFIED ON.
**      ULINE.
**      WRITE:/ TEXT-T01,
**              TOT_CODE-HWSTE CURRENCY T001-WAERS UNDER TOT_CODE-HWSTE .
**    ENDAT.
** ALV Comment ends
*  ENDLOOP.

  FORMAT INTENSIFIED OFF.
* Begin of GB - 22.09.10
*  DELETE ADJACENT DUPLICATES FROM tot_code.
  DELETE ADJACENT DUPLICATES FROM tot_code COMPARING bukrs belnr gjahr buzei witht.
* End of GB - 22.09.10
  SORT tot_code.
  xheader = 2.
  DELETE tot_code WHERE hwste = 0.

  LOOP AT tot_code.
    MOVE: tot_code-qscod  TO gs_tot_code-qscod,
          tot_code-text40 TO gs_tot_code-text40,
          tot_code-hwste  TO gs_tot_code-hwste.
    COLLECT gs_tot_code INTO gt_tot_code.
    CLEAR gs_tot_code.
  ENDLOOP.

ENDFORM.                               " PRINT_TOT_CODE
*&---------------------------------------------------------------------*
*&      Form  READ_FOREIGN_ID
*&---------------------------------------------------------------------*
*      Retrive code for foreign persons
*----------------------------------------------------------------------*
FORM read_foreign_id USING    xland1
                              xstkzn
                     CHANGING xxstcd1      .
  CLEAR: xfrid, j_1afrid, xxstcd1 .
  READ TABLE xfrid WITH KEY mandt = sy-mandt
                            land1 = xland1
                            stkzn = xstkzn.
* entry exists?
  IF sy-subrc NE 0.

    SELECT SINGLE * FROM j_1afrid
                    WHERE land1 = xland1
                    AND   stkzn = xstkzn.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING j_1afrid TO xfrid.
      APPEND xfrid.
    ENDIF.
  ENDIF.

  IF sy-subrc EQ 0.
    xxstcd1 = xfrid-j_1afpid.
  ENDIF.

ENDFORM.                               " READ_FOREIGN_ID

*&---------------------------------------------------------------------*
*&      Form  READ_TAX_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DETAIL_QSCOD  text                                         *
*      <--P_DETAIL_TXCOD  text                                         *
*----------------------------------------------------------------------*
FORM read_tax_code USING    x_type
                            p_detail_qscod
                   CHANGING p_detail_txcod.
  CLEAR:  p_detail_txcod .
  IF   x_type = '1'  .
* ---> It is a Wittholding tax
    SELECT SINGLE j_1anatxcd INTO p_detail_txcod
         FROM  t059o
         WHERE land1 EQ t001-land1
           AND wt_qscod EQ p_detail_qscod .
  ELSEIF x_type = '2' .
* ---> It is a tax Perception
    SELECT SINGLE j_1anatxcd INTO p_detail_txcod
         FROM  j_1atxoff
         WHERE land1 EQ t001-land1
           AND j_1ataxcod EQ p_detail_qscod .
  ENDIF  .
ENDFORM.                    " READ_TAX_CODE

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PAR_FILE  text                                             *
*----------------------------------------------------------------------*
FORM transfer_local USING p_file LIKE rfpdo1-allgunix.
*** Begin of Note 984952
*  DATA: LCL_FILE LIKE SAPB-SAPPFAD.
*  MOVE P_FILE TO LCL_FILE.
*  CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
*    EXPORTING
*      PATH       = LCL_FILE
*      TARGETPATH = P_LFNAME
*    EXCEPTIONS
*      ERROR_FILE = 1
*      OTHERS     = 2.
*  IF SY-SUBRC <> 0.
*    MESSAGE I812 WITH P_LFNAME .
*  ENDIF.
  DATA: lcl_filename TYPE string.
*  DATA: LCL_FILE LIKE RFPDO1-ALLGUNIX.
*  DATA:
** table with the file entries for the download
*      BEGIN OF DTATAB OCCURS 500,      "DTA-File
*       LINE(300)              TYPE C,
*      END OF DTATAB.
  MOVE p_lfname TO lcl_filename.
*  MOVE S_FILE TO LCL_FILE.
*  CLOSE DATASET LCL_FILE.   " to avoid error: File already opened
*  OPEN DATASET LCL_FILE FOR INPUT IN TEXT MODE
*  encoding non-unicode ignoring conversion errors.             " UNICODE
*
*  DO.
*    READ DATASET LCL_FILE INTO DTATAB-LINE.
*
*    IF SY-SUBRC NE 0.
*      EXIT.
*    ELSE.
*      APPEND DTATAB.
*    ENDIF.
*  ENDDO.
*
*  CLOSE DATASET LCL_FILE.

  CALL FUNCTION 'GUI_DOWNLOAD'
       EXPORTING
*     BIN_FILESIZE                    =
      filename                        = lcl_filename
      filetype                        = 'ASC'
*     APPEND                          = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
*     HEADER                          = '00'
      trunc_trailing_blanks           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
      trunc_trailing_blanks_eol       = ' '
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*   IMPORTING
*     FILELENGTH                      =
    TABLES
      data_tab                        = output_file1
*     FIELDNAMES                      =
*   EXCEPTIONS
*     FILE_WRITE_ERROR                = 1
*     NO_BATCH                        = 2
*     GUI_REFUSE_FILETRANSFER         = 3
*     INVALID_TYPE                    = 4
*     NO_AUTHORITY                    = 5
*     UNKNOWN_ERROR                   = 6
*     HEADER_NOT_ALLOWED              = 7
*     SEPARATOR_NOT_ALLOWED           = 8
*     FILESIZE_NOT_ALLOWED            = 9
*     HEADER_TOO_LONG                 = 10
*     DP_ERROR_CREATE                 = 11
*     DP_ERROR_SEND                   = 12
*     DP_ERROR_WRITE                  = 13
*     UNKNOWN_DP_ERROR                = 14
*     ACCESS_DENIED                   = 15
*     DP_OUT_OF_MEMORY                = 16
*     DISK_FULL                       = 17
*     DP_TIMEOUT                      = 18
*     FILE_NOT_FOUND                  = 19
*     DATAPROVIDER_EXCEPTION          = 20
*     CONTROL_FLUSH_ERROR             = 21
*     OTHERS                          = 22
            .
  IF sy-subrc <> 0.
    MESSAGE i812 WITH p_lfname .
  ENDIF.

ENDFORM.                    " TRANSFER_LOCAL

* ALV Changes starts

*&---------------------------------------------------------------------*
*&      Form  output_hierarchical_alv
*&---------------------------------------------------------------------*
*       Display Details using hierarchical list display
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM output_hierarchical_alv .
* *****Building the Field catalog.
  PERFORM fieldcat_build_alv  USING  gc_struct1 gc_struct2
                              CHANGING gt_fieldcat.

*  ****Event build for hierarchical list
  PERFORM eventtab_build CHANGING gt_events.

* Subroutine for total and subtotal
*  PERFORM t_sort_build_alv CHANGING gt_bsort.

*  ****layout build for hierarchical
  PERFORM layout_build CHANGING gs_layout.

**Note 1021760 begins
  gs_variant-handle = gc_handle1.
  gs_variant-report = sy-repid.
  gs_variant-variant = par_var1.
**Note 1021760 ends
  IF gt_header IS INITIAL.
    MESSAGE i000(z_fi) WITH 'No se encontraron datos con esa selección'.

  ELSE.
* *****To print list details
    CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = gv_repid
        i_callback_pf_status_set = gc_setpfstatus
        i_callback_user_command  = gc_user_command
        is_layout                = gs_layout
        is_variant               = gs_variant  "Note 1021760
        it_fieldcat              = gt_fieldcat
        it_events                = gt_events
        i_tabname_header         = gt_tab_header
        i_tabname_item           = gt_tab_item
        is_keyinfo               = gs_keyinfo
      TABLES
        t_outtab_header          = gt_header
        t_outtab_item            = gt_item
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    REFRESH: gt_header,
             gt_item.
  ENDIF.
ENDFORM.                    " output_hierarchical_alv

*&--------------------------------------------------------------------*
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
*       To set pf-status in hierarchical list
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM set_pf_status USING iv_extab TYPE slis_t_extab.
  SET PF-STATUS 'APPEND'.
ENDFORM.                               " SET_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  variant_f4_help
*&---------------------------------------------------------------------*
*       F4 help for variants for both appended lists
*----------------------------------------------------------------------*
*      ---> IC_HANDLE   Handler
*      <--> XV_VARIANT  Variant
*----------------------------------------------------------------------*
FORM variant_f4_help  USING ic_handle TYPE slis_handl
                      CHANGING xv_variant TYPE disvariant-variant.

  DATA: ls_variant TYPE disvariant,        " Structure for variant
        ls_variant_help TYPE disvariant,   " Structure for variant
        lv_exit(1) TYPE c.                 "User-Exit while F4-Help

  ls_variant-handle = ic_handle.
  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = ls_variant
      i_save     = gc_a
    IMPORTING
      e_exit     = lv_exit
      es_variant = ls_variant_help.

  IF lv_exit EQ space.
    xv_variant = ls_variant_help-variant.
  ENDIF.

ENDFORM.                    " variant_f4_help

*&---------------------------------------------------------------------*
*&      Form  check_variant_existance
*&---------------------------------------------------------------------*
*       This will check for the variant existance ..
*----------------------------------------------------------------------*
*      -->IC_HANDLE  Handle name..
*      -->IV_VARIANT  VARIANT which handles handle,
*                     variant and report name
*----------------------------------------------------------------------*
FORM check_variant_existance  USING ic_handle TYPE slis_handl
                                    iv_variant TYPE disvariant-variant.
  DATA: ls_variant TYPE disvariant.

  IF iv_variant <> space.
    ls_variant-handle  = ic_handle.
    ls_variant-variant = iv_variant.
    ls_variant-report  = sy-repid.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = gc_a
      CHANGING
        cs_variant = ls_variant.
  ENDIF.                               " IF iv_variant <> space

ENDFORM.                               " check_variant_existance

*&---------------------------------------------------------------------*
*&      Form  fieldcat_build_alv
*&---------------------------------------------------------------------*
* This Subroutine uses REUSE_ALV_FIELDCATALOG_MERGE function module for
* building the field catalog.
*----------------------------------------------------------------------*
*   --> is_struct1     stores the first structure name
*   --> is_struct2     stores the second structure name
*   <-> xt_fieldcat    field catalog table
*----------------------------------------------------------------------*
FORM fieldcat_build_alv  USING value(is_struct1) TYPE dd02l-tabname
                              value(is_struct2) TYPE dd02l-tabname
                        CHANGING xt_fieldcat TYPE slis_t_fieldcat_alv.
  REFRESH gt_fieldcat.
*Field catalog build function for First Structure

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = gv_repid
      i_internal_tabname     = gt_tab_header
      i_structure_name       = is_struct1
      i_inclname             = gv_repid
    CHANGING
      ct_fieldcat            = xt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.                  "IF SY-SUBRC <> 0.


*Field catalog build function for Second Structure

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = gv_repid
      i_internal_tabname     = gt_tab_item
      i_structure_name       = is_struct2
      i_inclname             = gv_repid
    CHANGING
      ct_fieldcat            = xt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.               "IF SY-SUBRC <> 0.

  FIELD-SYMBOLS <fs> TYPE  slis_fieldcat_alv. "field symbol .

  LOOP AT xt_fieldcat ASSIGNING <fs> .

    CASE <fs>-fieldname.

      WHEN 'BELNR' .
        IF <fs>-tabname   = gt_tab_item.
          <fs>-no_out     = gc_x.
          <fs>-tech       = gc_x.
        ELSE.
          <fs>-seltext_m  = text-u21.
          <fs>-ddictxt    = gc_m.
          <fs>-col_pos    = 1.
        ENDIF.

      WHEN 'GJAHR' .
        IF <fs>-tabname   = gt_tab_item.
          <fs>-no_out     = gc_x.
          <fs>-tech       = gc_x.
        ELSE.
*          <fs>-seltext_m  = text-u21.
          <fs>-ddictxt    = gc_m.
          <fs>-col_pos    = 2.
        ENDIF.

      WHEN 'HKONT' .
        IF <fs>-tabname   = gt_tab_item.
          <fs>-no_out     = gc_x.
          <fs>-tech       = gc_x.
        ELSE.
          <fs>-col_pos    = 3.
        ENDIF.

      WHEN 'NAME1' .
        IF <fs>-tabname   = gt_tab_item.
          <fs>-no_out     = gc_x.
          <fs>-tech       = gc_x.
        ELSE.
          <fs>-col_pos    = 4.
        ENDIF.

      WHEN 'BUKRS' .
        <fs>-no_out     =  gc_x.
        <fs>-tech       =  gc_x.

      WHEN 'CLASS' .
        <fs>-col_pos      = 5.
        <fs>-seltext_m  = text-u01.
        <fs>-ddictxt    = gc_m.
      WHEN 'DATE' .
        <fs>-col_pos      = 6.
        <fs>-seltext_m  = text-u02.
        <fs>-ddictxt    = gc_m.

      WHEN 'OFFNR' .
        <fs>-col_pos      = 7.
        <fs>-seltext_m  = text-u03.
        <fs>-ddictxt    = gc_m.

      WHEN 'X_TEXT' .
        <fs>-col_pos      = 8.
        <fs>-seltext_m  = text-u04.
        <fs>-ddictxt    = gc_m.

      WHEN 'TXCOD' .
        <fs>-col_pos      = 9.
        <fs>-seltext_m  = text-u05.
        <fs>-ddictxt    = gc_m.

      WHEN 'QSCOD' .
        <fs>-col_pos      = 10.
        <fs>-seltext_m  = text-u06.
        <fs>-ddictxt    = gc_m.

      WHEN 'TCODE' .
        <fs>-col_pos      = 11.
        <fs>-seltext_m  = text-u07.
        <fs>-ddictxt    = gc_m.

      WHEN 'X_TEXT1' .
        <fs>-col_pos      = 12.
        <fs>-seltext_m  = text-u08.
        <fs>-ddictxt    = gc_m.

      WHEN 'DATE1' .
        <fs>-col_pos      = 13.
        <fs>-seltext_m  = text-u09.
        <fs>-ddictxt    = gc_m.

      WHEN 'FITYP' .
        <fs>-col_pos      = 14.
        <fs>-seltext_m  = text-u10.
        <fs>-ddictxt    = gc_m.

      WHEN 'X_TEXT2' .
        <fs>-col_pos      = 15.
        <fs>-seltext_m  = text-u11.
        <fs>-ddictxt    = gc_m.

      WHEN 'EXRT' .
        <fs>-col_pos      = 16.
        <fs>-seltext_m  = text-u12.
        <fs>-ddictxt    = gc_m.

      WHEN 'EXDF' .
        <fs>-col_pos      = 17.
        <fs>-seltext_m  = text-u13.
        <fs>-ddictxt    = gc_m.

      WHEN 'STCD1' .
        <fs>-col_pos      = 19.
        <fs>-seltext_m  = text-u14.
        <fs>-ddictxt    = gc_m.

      WHEN 'CTNUM' .
        <fs>-col_pos      = 20.
        <fs>-seltext_m  = text-u15.
        <fs>-ddictxt    = gc_m.

      WHEN 'BUTXT' .
        <fs>-col_pos      = 21.
        <fs>-seltext_m  = text-u16.
        <fs>-ddictxt    = gc_m.

      WHEN 'GRSUP' .
        <fs>-col_pos      = 23.
        <fs>-seltext_m  = text-u17.
        <fs>-ddictxt    = gc_m.

      WHEN 'FPTID' .
        <fs>-col_pos      = 24.
        <fs>-seltext_m  = text-u18.
        <fs>-ddictxt    = gc_m.

      WHEN 'CCTID' .
        <fs>-col_pos      = 25.
        <fs>-seltext_m  = text-u19.
        <fs>-ddictxt    = gc_m.

    ENDCASE.
  ENDLOOP.
ENDFORM.                    " fieldcat_build_alv
*&---------------------------------------------------------------------*
*&      Form  fieldcat_build_alv
*&---------------------------------------------------------------------*
* This Subroutine uses REUSE_ALV_FIELDCATALOG_MERGE function module for
* building the field catalog.
*----------------------------------------------------------------------*
*   --> is_struct1     stores the first structure name
*   --> is_struct2     stores the second structure name
*   <-> xt_fieldcat    field catalog table
*----------------------------------------------------------------------*
FORM fieldcat_build_alv1  USING value(is_struct1) TYPE dd02l-tabname
                              value(is_struct2) TYPE dd02l-tabname
                        CHANGING xt_fieldcat TYPE slis_t_fieldcat_alv.
  REFRESH gt_fieldcat.
  DATA: fs TYPE  slis_fieldcat_alv. "field symbol .

  fs-fieldname  = 'CONCP' .
  fs-seltext_m  = 'Cod Juridicción'.
  fs-ddictxt    = gc_m.
  fs-col_pos    = 1.

  APPEND fs TO xt_fieldcat.
  fs-fieldname  = 'CUIT' .
  fs-seltext_m  = 'C.U.I.T.'.
  fs-ddictxt    = gc_m.
  fs-col_pos    = 2.
  APPEND fs TO xt_fieldcat.


  fs-fieldname  = 'PERIODO' .
  fs-seltext_m  = 'Mes'.
  fs-ddictxt    = gc_m.
  fs-col_pos    = 3.

  APPEND fs TO xt_fieldcat.

  fs-fieldname  = 'CBU' .
  fs-seltext_m  = 'CBU'.
  fs-ddictxt    = gc_m.
  fs-col_pos    = 4.

  APPEND fs TO xt_fieldcat.

  fs-fieldname  = 'TYPCTA' .
  fs-seltext_m  = 'Tpo. Cta'.
  fs-ddictxt    = gc_m.
  fs-col_pos    = 5.
  APPEND fs TO xt_fieldcat.

  fs-fieldname  = 'MONEDA' .
  fs-col_pos    = 6.
  fs-seltext_m  = 'Moneda'.
  fs-ddictxt    = gc_m.
  APPEND fs TO xt_fieldcat.

  fs-fieldname  = 'IMP' .
  fs-col_pos    = 7.
  fs-seltext_m  = 'Importe'.
  fs-ddictxt    = gc_m.
  APPEND fs TO xt_fieldcat.

  fs-fieldname  = 'BUKRS' .
  fs-col_pos    = 7.
  fs-seltext_m  = 'Sociedad'.
  fs-ddictxt    = gc_m.
  APPEND fs TO xt_fieldcat.

  fs-fieldname  = 'BELNR' .
  fs-col_pos    = 7.
  fs-seltext_m  = 'Documento'.
  fs-ddictxt    = gc_m.
  APPEND fs TO xt_fieldcat.

  fs-fieldname  = 'GJAHR ' .
  fs-col_pos    = 7.
  fs-seltext_m  = 'Año'.
  fs-ddictxt    = gc_m.
  APPEND fs TO xt_fieldcat.




*    ENDCASE.
*MODIFY xt_fieldcat INDEX sy-tabix FROM <fs>.
*  ENDLOOP.
ENDFORM.                    " fieldcat_build_alv
*&---------------------------------------------------------------------
*&      Form  eventtab_build
*&---------------------------------------------------------------------
*       Event build details list
*----------------------------------------------------------------------
*      <--XT_EVENTS  text
*----------------------------------------------------------------------

FORM eventtab_build  CHANGING xt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = xt_events.

  READ TABLE xt_events WITH KEY name = 'END_OF_LIST'
                          INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'END_OF_LIST' TO ls_event-form.
    APPEND ls_event TO xt_events.
  ENDIF.
  CLEAR ls_event.

  READ TABLE xt_events INTO ls_event WITH
       KEY name = slis_ev_top_of_page.

  ls_event-form = slis_ev_top_of_page.
  MODIFY xt_events FROM ls_event
                          TRANSPORTING form
                          WHERE name EQ slis_ev_top_of_page.


ENDFORM.                    " eventtab_build

*&---------------------------------------------------------------------*
*&      Form  layout_build
*&---------------------------------------------------------------------*
*       Layout build for hierarchical sequential list
*----------------------------------------------------------------------*
*      <-->XS_LAYOUT  text
*----------------------------------------------------------------------*
FORM layout_build  CHANGING xs_layout TYPE slis_layout_alv.
  gs_layout-list_append = gc_y.
  gs_layout-totals_only = gc_x.

ENDFORM.                    " layout_build

*&---------------------------------------------------------------------*
*&       FORM  TOP-OF-PAGE                                             *
*&---------------------------------------------------------------------*
*&       Seitenkopf schreiben                                          *
*&---------------------------------------------------------------------*
FORM top_of_page.

  DATA: lv_hyphen(1) TYPE c VALUE '-'.
  DATA: lr_grid TYPE REF TO cl_salv_form_layout_grid.

* Read company code from gt_header table
  LOOP AT gt_header INTO gs_header.
    bhdgd-bukrs = gs_header-bukrs.
  ENDLOOP.

* Batch Heading
  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd = bhdgd.

* Top of page using alv forms
  CREATE OBJECT lr_grid.

  lr_grid->create_text( row = 1 column = 1 text = text-h05 ).

  lr_grid->create_text( row = 1 column = 2 text = from_date ).

  IF to_date NE from_date.

    lr_grid->create_text( row = 1 column = 1 text = text-h05 ).

    lr_grid->create_text( row = 1 column = 2 text = from_date ).

    lr_grid->create_text( row = 1 column = 3 text = lv_hyphen ).

    lr_grid->create_text( row = 1 column = 4 text = to_date ).
  ENDIF.

  IF s_newdat NE space.

    lr_grid->create_text( row = 2 column = 1 text = text-h06 ).

    lr_grid->create_text( row = 2 column = 2 text = s_file ).

  ENDIF.

  CALL METHOD cl_salv_form_content=>set
    EXPORTING
      value = lr_grid.

ENDFORM.                    "TOP-OF-PAGE

*&---------------------------------------------------------------------*
*&      Form  end_of_list
*&---------------------------------------------------------------------*
*      To append the list in hierarchical sequential list
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM end_of_list.

  PERFORM output_list_display_alv.

ENDFORM.                      "end_of_list
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       To perform double click on line in list
*----------------------------------------------------------------------*
*       --> iv_ucomm  Store Ok code value
*       --> iv_selfield Selection line data
*----------------------------------------------------------------------*
FORM user_command USING  iv_ucomm    TYPE sy-ucomm
                         iv_selfield TYPE slis_selfield.

  CASE iv_ucomm.

    WHEN  '&IC1'.
      CASE iv_selfield-fieldname.
        WHEN 'BELNR' OR 'GJAHR' OR
             'HKONT' OR 'NAME1'.
*          READ TABLE gt_header index iv_selfield-tabindex.

          READ TABLE gt_header INTO gs_header
               INDEX iv_selfield-tabindex.


          IF sy-subrc EQ 0.
* Begin of Note 971621
*            LOOP AT gt_header INTO gs_header.
* End of Note 971621
            MOVE: gs_header-belnr TO detail-belnr,
                  gs_header-bukrs TO detail-bukrs,
                  gs_header-gjahr TO detail-gjahr.
* Begin of Note 971621
*            ENDLOOP.
* End of Note 971621
          ENDIF.

          CHECK detail-belnr NE space.

          SET PARAMETER ID 'BLN' FIELD detail-belnr.
          SET PARAMETER ID 'BUK' FIELD detail-bukrs.
          SET PARAMETER ID 'GJR' FIELD detail-gjahr.

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA

          CLEAR detail.
      ENDCASE.


  ENDCASE.
  CLEAR iv_selfield.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  output_list_display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM output_list_display_alv .

*  ****Field catalog build for list
  PERFORM fieldcat_build_alv_1 CHANGING gt_fieldcat_1.

*  ****Event build for list
  PERFORM eventtab_build_1 CHANGING gt_events_1.

  CLEAR gs_layout.
  gs_layout-list_append = gc_x.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat_1
      i_save             = gc_save
      it_events          = gt_events_1
    TABLES
      t_outtab           = gt_tot_code
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*Begin of Note 971621
*  REFRESH: gt_fieldcat_1,
*           gt_tot_code.
  REFRESH: gt_fieldcat_1.
*End of Note 971621

ENDFORM.                    " output_list_display_alv
*&---------------------------------------------------------------------*
*&      Form  fieldcat_build_alv_1
*&---------------------------------------------------------------------*
*       Field catalog for total code list
*----------------------------------------------------------------------*
*      <--XT_FIELDCAT_1  text
*----------------------------------------------------------------------*
FORM fieldcat_build_alv_1  CHANGING xt_fieldcat_1
                           TYPE slis_t_fieldcat_alv.
  REFRESH: gt_fieldcat_1,
           xt_fieldcat_1.
*Field catalog build function for Second Structure

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = gv_repid
*      i_internal_tabname     = gt_tot_code " 'GT_TOT_CODE' "gt_tot_code
      i_structure_name       = gc_tot_code
      i_inclname             = gv_repid
    CHANGING
      ct_fieldcat            = xt_fieldcat_1
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.               "IF SY-SUBRC <> 0.
  FIELD-SYMBOLS <fs> TYPE  slis_fieldcat_alv. "field symbol .

  LOOP AT xt_fieldcat_1 ASSIGNING <fs> .

    CASE <fs>-fieldname.

      WHEN 'HWSTE' .
        <fs>-do_sum     =  gc_x.
    ENDCASE.
  ENDLOOP.


ENDFORM.                    " fieldcat_build_alv_1
*&---------------------------------------------------------------------*
*&      Form  eventtab_build_1
*&---------------------------------------------------------------------*
*       Event build for total code list
*----------------------------------------------------------------------*
*      <--P_GT_EVENTS_1  text
*----------------------------------------------------------------------*
FORM eventtab_build_1  CHANGING xt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = xt_events.

  READ TABLE xt_events INTO ls_event WITH
     KEY name = slis_ev_top_of_page.

  ls_event-form = slis_ev_top_of_page.
  MODIFY xt_events FROM ls_event
                          TRANSPORTING form
                          WHERE name EQ slis_ev_top_of_page.
ENDFORM.                    " eventtab_build_1

* ALV Changes ends
**Note 1013585 Begins
*&---------------------------------------------------------------------*
*&      Form  FILL_WITHHELD
*&---------------------------------------------------------------------*
*  Fill data for people withheld
*----------------------------------------------------------------------*
FORM fill_withheld.
*A different person withheld is assumed to have a different address
  ON CHANGE OF mdata-adrnr.
* Inicio modificación 31.05.2011 - Diego
    CHECK mdata-adrnr IS NOT INITIAL.
* Fin modificación 31.05.2011
    CLEAR: addr1_sel, sadr.
    addr1_sel-addrnumber = mdata-adrnr.
    CALL FUNCTION 'ADDR_GET'
      EXPORTING
        address_selection = addr1_sel
      IMPORTING
        address_value     = addr1_val.

    IF mdata-land1 NE t001-land1.
      withheld-stcd1 = xstcd1.
    ELSE.
      withheld-stcd1 = mdata-stcd1.
    ENDIF.
    withheld-doc_type = mdata-stcdt.
    withheld-butxt = mdata-name1.
    withheld-street = addr1_val-street.
    withheld-house_num = addr1_val-house_num1.
    withheld-city = addr1_val-city1.
    withheld-region = addr1_val-region.
    withheld-postal_cd = addr1_val-post_code1.
    COLLECT withheld.
  ENDON.
ENDFORM.                    " FILL_WITHHELD
*&---------------------------------------------------------------------*
*&      Form  PRINT_WITHHELD
*&---------------------------------------------------------------------*
*   Print WITHHELD to the application server file
*----------------------------------------------------------------------*
FORM print_withheld .
  DATA: len_housenum TYPE i,
        len_street TYPE i,
        len_total TYPE i,
        len_str_trunc TYPE i.
  SORT withheld BY stcd1 city region postal_cd doc_type.
  IF NOT withheld IS INITIAL AND NOT p_sfile IS INITIAL AND NOT p_sfnam IS INITIAL.
    OPEN DATASET p_sfnam FOR OUTPUT IN TEXT MODE
    ENCODING NON-UNICODE IGNORING CONVERSION ERRORS.
    LOOP AT withheld.
      CLEAR output_withheld.
      len_housenum = STRLEN( withheld-house_num ).
      len_street = STRLEN( withheld-street ).
      len_total = len_housenum + len_street.
      MOVE withheld-street TO output_withheld-address.
*Make changes to the address to include both street and house number in 20 chars.
      IF NOT withheld-house_num IS INITIAL AND len_total < 20.
        len_street = len_street + 1.
        MOVE withheld-house_num TO output_withheld-address+len_street(len_housenum).
      ELSEIF NOT withheld-house_num IS INITIAL.
        len_str_trunc = 19 - len_housenum.
        MOVE ' ' TO output_withheld-address+len_str_trunc(1).
        len_str_trunc = len_str_trunc + 1.
        MOVE withheld-house_num TO output_withheld-address+len_str_trunc(len_housenum).
      ENDIF.
      MOVE-CORRESPONDING withheld  TO output_withheld .
** Note 1147272 Begin
      IF output_withheld-doc_type IS INITIAL.
        MOVE '00' TO output_withheld-doc_type.
      ENDIF.
** Note 1147272 End
      TRANSFER output_withheld TO p_sfnam LENGTH 83 .
      IF NOT p_lwfile IS INITIAL.
        MOVE output_withheld TO output_withheld1.
        COLLECT output_withheld1.
      ENDIF.
    ENDLOOP.
    CLOSE DATASET p_sfnam.
  ENDIF.
ENDFORM.                    " PRINT_WITHHELD
*&---------------------------------------------------------------------*
*&      Form  WITHHELD_LOCAL
*&---------------------------------------------------------------------*
* Print WITHHELD to the local file
*----------------------------------------------------------------------*
FORM withheld_local .
  DATA: lcl_filename TYPE string.
  MOVE p_lwfnam TO lcl_filename.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename              = lcl_filename
      filetype              = 'ASC'
      trunc_trailing_blanks = ' '
    TABLES
      data_tab              = output_withheld1.
  IF sy-subrc <> 0.
    MESSAGE i812 WITH p_lfname .
  ENDIF.
ENDFORM.                    " WITHHELD_LOCAL
**Note 1013585 Ends
*&---------------------------------------------------------------------*
*&  Include           file_f4
*&---------------------------------------------------------------------*
FORM file_f4  CHANGING po_path.
*  DATA:
*    filetable TYPE filetable,
*    rc        TYPE i,
*    v_usr_action TYPE i,
*    v_path TYPE string,
*    v_fullpath TYPE string,
*    v_filename TYPE string.
*
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
*      initial_directory    = 'C:\'
*    CHANGING
*      filename             = v_filename
*      path                 = v_path
*      fullpath             = v_fullpath
*      user_action          = v_usr_action
*    EXCEPTIONS
*      cntl_error           = 1
*      error_no_gui         = 2
*      not_supported_by_gui = 3
*      OTHERS               = 4.
*  IF sy-subrc IS INITIAL.
*    IF v_usr_action EQ cl_gui_frontend_services=>action_ok.
*      MOVE v_fullpath TO po_path.
*    ENDIF.
*  ENDIF.

  DATA:
      filename  TYPE string,
      path      TYPE string,
      fullpath  TYPE string,
      p_file    TYPE rlgrap-filename.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select ruta'
      default_extension = 'TXT'
      file_filter       = '*.TXT'
    CHANGING
      filename          = filename
      path              = path
      fullpath          = fullpath.

  IF sy-subrc EQ 0.
    po_path = fullpath.
  ELSE.
    MESSAGE e000(zerror) WITH 'Completar Ruta'  ."DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                                                    "file_f4
*&---------------------------------------------------------------------*
*&      Form  F_ARCHIVO_BAJADA
*&---------------------------------------------------------------------*
FORM f_archivo_bajada .
  DATA:
*     Tablas
        tl_bajada_ret  TYPE STANDARD TABLE OF ty_bajada_ret,
        tl_bajada_per  TYPE STANDARD TABLE OF ty_bajada_per,
        tl_bseg        TYPE tp_t_bseg,

*     Estructura
        stl_t059o      TYPE t059o                          ,
        stl_bajada_ret TYPE ty_bajada_ret                  ,
        stl_bajada_rec TYPE ty_bajada_rec                  ,
        stl_item       TYPE gss_j_1af016_list2             ,
        stl_bajada_per TYPE ty_bajada_per                  ,
* Modificado por Diego en 17.09.2010 -
*       stl_lfbk       TYPE lfbk                           ,
*       stl_lfbk       TYPE zfi_lfbk                       ,
        v_hkont(10)    TYPE c                              ,
* Fin 17.09.2010
        stl_knbk       TYPE knbk                           ,
        stl_detail     TYPE ty_detail                      ,
*     variables
        vl_qsskz   TYPE qsskz                              ,
        p_path     TYPE string                             ,
        flen       TYPE i                                  ,
        vl_tabix   TYPE sy-tabix                           ,
        vl_tabla   TYPE char20                             .

  REFRESH: t_lfa1, t_kna1, t_ctaregion.

  CLEAR:   st_lfa1     ,
           st_ctaregion,
             st_kna1     .

  SELECT * FROM lfa1 INTO TABLE t_lfa1.

  SELECT * FROM kna1 INTO TABLE t_kna1.

  SELECT * FROM zfiyt_ctaregion INTO TABLE t_ctaregion.


  SORT  t_bseg      BY  belnr gjahr bukrs .
  SORT  t_bkpf      BY  belnr gjahr bukrs .
  SORT  t_bset      BY  belnr gjahr bukrs .
  SORT  detail      BY  belnr gjahr bukrs .
  SORT  t_item      BY  belnr gjahr bukrs .
  SORT  t_ctaregion BY  saknr             .
  SORT  t_kna1      BY  kunnr             .
  SORT  t_lfa1      BY  lifnr             .
*Busco los numero de juridicciones.
  SELECT *
  FROM t005s
  INTO TABLE t_t005s
  WHERE land1 EQ 'AR'.

  SELECT *
  FROM t059p
  INTO TABLE t_t059p
  WHERE land1 EQ 'AR'.
* Borro las posiciones que no se usa.

* Se obtienen los documentos para retenciones
  PERFORM f_obtener_doc_retenciones CHANGING tl_bseg.

  LOOP AT detail INTO stl_detail.
    IF rb_002 EQ 'X'.
      PERFORM f_percepciones TABLES tl_bajada_per
                              USING stl_detail
                           CHANGING stl_bajada_per.
    ELSEIF rb_001 EQ 'X' .

      PERFORM f_retenciones  TABLES tl_bajada_ret
                                    tl_bseg
                              USING stl_detail
                           CHANGING stl_bajada_ret.
*ty_banco
    ENDIF.

  ENDLOOP.

  CLEAR:   t_bajada.
  REFRESH: t_bajada.

  IF rb_002 EQ 'X'.
    SORT tl_bajada_per BY  cod
                           cuit
                           fecha
                           suc
                           nro
                           typcom
                           letra
                           imp   .
    DELETE ADJACENT DUPLICATES FROM tl_bajada_per
                 COMPARING cod
                           cuit
                           fecha
                           suc
                           nro
                           typcom
                           letra
                           imp.
    LOOP AT tl_bajada_per INTO stl_bajada_per.
      PERFORM f_archivo_per USING stl_bajada_per.
    ENDLOOP.
  ELSEIF rb_001 EQ 'X'.
    SORT tl_bajada_ret BY
                          cod
                          cuit
                          fecha
                          suc
                          nro
                          typcom
                          letra
                          compr
                          imp    .
    DELETE ADJACENT DUPLICATES FROM tl_bajada_ret
                COMPARING cod
                          cuit
                          fecha
                          suc
                          nro
                          typcom
                          letra
                          compr
                          imp   .


    LOOP AT tl_bajada_ret INTO stl_bajada_ret.
      PERFORM f_archivo_ret USING stl_bajada_ret.
    ENDLOOP.

  ELSEIF rb_003 EQ 'X'.

    LOOP AT t_bseg INTO bseg WHERE hkont IN s_hkont.
* Concepto
      READ TABLE t_ctaregion INTO st_ctaregion
      WITH KEY saknr = bseg-hkont.
      IF sy-subrc EQ 0.
        MOVE st_ctaregion-fprcd TO stl_bajada_rec-concp.
      ELSE.
        CONTINUE.
      ENDIF.
      stl_bajada_rec-bukrs = bseg-bukrs.
      stl_bajada_rec-belnr = bseg-belnr.
      stl_bajada_rec-gjahr = bseg-gjahr.
* Moneda y ejercicio.
      READ TABLE t_bkpf INTO st_bkpf
      WITH KEY belnr = bseg-belnr
               gjahr = bseg-gjahr
               bukrs = bseg-bukrs.
      IF sy-subrc EQ 0.
        WRITE: st_bkpf-monat  TO stl_bajada_rec-periodo RIGHT-JUSTIFIED.
        stl_bajada_rec-periodo(4) = st_bkpf-gjahr(4) .
        CASE st_bkpf-waers.
          WHEN 'ARS'.
            MOVE 'P'  TO stl_bajada_rec-moneda.
          WHEN OTHERS.
            MOVE 'E'  TO stl_bajada_rec-moneda.
        ENDCASE.
      ENDIF.

      MOVE 'CC' TO stl_bajada_rec-typcta.

*      IMPORTE
      x_text = bseg-dmbtr .
      CONDENSE x_text NO-GAPS.
      TRANSLATE x_text USING '.,'.

      WRITE x_text TO stl_bajada_rec-imp RIGHT-JUSTIFIED.
      OVERLAY stl_bajada_rec-imp
        WITH '0000000000'.
      APPEND stl_bajada_rec TO tl_bajada_rec.
    ENDLOOP.

    LOOP AT tl_bajada_rec INTO stl_bajada_rec.
      vl_tabix = sy-tabix.
      LOOP AT t_bseg INTO bseg
       WHERE belnr EQ stl_bajada_rec-belnr
        AND  gjahr EQ stl_bajada_rec-gjahr
        AND  bukrs EQ stl_bajada_rec-bukrs.

        IF   bseg-koart  EQ 'K'.
          IF bseg-lifnr IS NOT INITIAL.
            READ TABLE t_lfa1 INTO st_lfa1
            WITH KEY lifnr = bseg-lifnr.
            IF sy-subrc EQ 0.
              MOVE st_lfa1-stcd1 TO stl_bajada_rec-cuit.
            ENDIF.

* Incluído por Diego en 17.09.2010
            CLEAR v_hkont.
            IF bseg-zuonr IS NOT INITIAL.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = bseg-zuonr
                IMPORTING
                  output = v_hkont.
            ENDIF.
* Fin 17.09.2010

*            CLEAR stl_lfbk.
* Modificado por Diego en 17.09.2010 -
*           SELECT SINGLE * FROM lfbk
*            SELECT SINGLE * FROM zfi_lfbk
* Fin 17.09.2010
*            INTO stl_lfbk
*            WHERE lifnr EQ bseg-lifnr
* Modificado por Diego en 17.09.2010
*             AND bankn EQ bseg-zuonr.
*              AND bukrs EQ bseg-bukrs
*              AND hkont EQ v_hkont.
* Fin 17.09.2010
*            MOVE stl_lfbk-koinh TO stl_bajada_rec-cbu.
          ENDIF.
        ENDIF.

        IF   bseg-koart  EQ 'D'.
          IF bseg-kunnr IS NOT INITIAL.
            READ TABLE t_kna1 INTO st_kna1
            WITH KEY kunnr = bseg-kunnr.
            IF sy-subrc EQ 0.
              MOVE st_kna1-stcd1 TO stl_bajada_rec-cuit.
            ENDIF.
          ENDIF.
          CLEAR stl_knbk.
          SELECT SINGLE * FROM knbk
          INTO stl_knbk
          WHERE kunnr EQ bseg-kunnr
            AND bankn EQ bseg-zuonr.
          MOVE stl_knbk-koinh TO stl_bajada_rec-cbu.

        ENDIF.
      ENDLOOP.
      MODIFY tl_bajada_rec FROM stl_bajada_rec INDEX vl_tabix.
    ENDLOOP.

    LOOP AT tl_bajada_rec INTO stl_bajada_rec.
      PERFORM f_archivo_ret_bancaria USING stl_bajada_rec.
    ENDLOOP.

  ENDIF.

  IF p_baja EQ 'X'.
    MOVE p_fila TO  p_path .
    IF  t_banco IS NOT INITIAL.
      SORT t_banco.
      DELETE ADJACENT DUPLICATES FROM t_banco COMPARING ALL FIELDS.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize              = flen
          filename                  = p_path
          filetype                  = 'DAT'
          trunc_trailing_blanks_eol = ' '
        CHANGING
          data_tab                  = t_banco[]
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          OTHERS                    = 24.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    IF  t_percep IS NOT INITIAL.
      SORT t_percep.
      DELETE ADJACENT DUPLICATES FROM t_percep COMPARING ALL FIELDS.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize              = flen
          filename                  = p_path
          filetype                  = 'DAT'
          trunc_trailing_blanks_eol = ' '
        CHANGING
          data_tab                  = t_percep[]
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          OTHERS                    = 24.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    IF t_bajada IS NOT INITIAL.
      SORT t_bajada.
      DELETE ADJACENT DUPLICATES FROM t_bajada COMPARING ALL FIELDS.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize              = flen
          filename                  = p_path
          filetype                  = 'DAT'
          trunc_trailing_blanks_eol = ' '
        CHANGING
          data_tab                  = t_bajada[]
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          OTHERS                    = 24.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_ARCHIVO_BAJADA
*&---------------------------------------------------------------------*
*&      Form  F_PORC_IVA
*&---------------------------------------------------------------------*
FORM f_porc_iva1  USING pi_kbetr TYPE kbetr
              CHANGING po_iva.
  DATA: sl_bset  TYPE bset,
        vl_kbetr TYPE kbetr_tax,
        vl_iva   TYPE char5.

  vl_kbetr = pi_kbetr / 10.
  WRITE vl_kbetr TO vl_iva LEFT-JUSTIFIED.
  DO 3 TIMES.
    REPLACE '.'   WITH ',' INTO  vl_iva.
*    REPLACE ','   WITH ' ' INTO  vl_iva.
  ENDDO.

  CONDENSE vl_iva NO-GAPS.
  WRITE vl_iva TO po_iva RIGHT-JUSTIFIED.
  OVERLAY  po_iva WITH '00000'.
ENDFORM.                    " F_PORC_IVA
*&---------------------------------------------------------------------*
*&      Form  F_PORC_IVA2
*&---------------------------------------------------------------------*
FORM f_porc_iva2  USING    pi_qsatz
                  CHANGING po_iva.

  DATA: sl_bset  TYPE bset,
        vl_kbetr TYPE kbetr_tax,
        vl_iva   TYPE char5.

  vl_kbetr = pi_qsatz * 1.

  WRITE vl_kbetr TO vl_iva RIGHT-JUSTIFIED.

  DO 3 TIMES.
    REPLACE '.'   WITH ',' INTO  vl_iva.
  ENDDO.

  CONDENSE vl_iva NO-GAPS.
  WRITE vl_iva TO po_iva RIGHT-JUSTIFIED.
  OVERLAY  po_iva WITH '00000'.

ENDFORM.                    " F_PORC_IVA2
*&---------------------------------------------------------------------*
*&      Form  F_ARCHIVO
*&---------------------------------------------------------------------*
FORM f_archivo_per  USING sl_bajada TYPE ty_bajada_per.
  DATA: st_bajada  TYPE ty_percep.
  CONCATENATE
      sl_bajada-cod
      sl_bajada-cuit
      sl_bajada-fecha
      sl_bajada-suc
      sl_bajada-nro
      sl_bajada-typcom
      sl_bajada-letra
      sl_bajada-imp
  INTO st_bajada-linea.
  APPEND st_bajada TO t_percep.
  CLEAR st_bajada.
ENDFORM.                    " F_ARCHIVO
*&---------------------------------------------------------------------*
*&      Form  F_GRISADO_CAMPOS
*&---------------------------------------------------------------------*
FORM f_grisado_campos .


  LOOP AT SCREEN.
    IF rb_001 EQ 'X'.
      IF rb_001 EQ 'X' AND s_witht IS INITIAL.
*        REFRESH: s_witht.
      ELSE.
        REFRESH: "s_witht,
                 s_hkont,
                 s_ktosl.
        CLEAR:   s_hkont,
                 s_ktosl.
        IF s_witht-low EQ '1'.
          REFRESH: s_witht.
*          clear  : s_witht.
        ENDIF.
      ENDIF.
      IF screen-name = 'S_WITHT-LOW'.
        MOVE '1' TO screen-input.
      ENDIF.
      IF screen-name = 'S_WITHT-HIGH'.
        MOVE '1' TO screen-input.
      ENDIF.
*
      IF screen-name = 'S_KTOSL-LOW'.
        MOVE '0' TO screen-input.
      ENDIF.
      IF screen-name = 'S_HKONT-HIGH'.
        MOVE '0' TO screen-input.
      ENDIF.
      IF screen-name = 'S_HKONT-LOW'.
        MOVE '0' TO screen-input.
      ENDIF.
      IF screen-name = 'S_KTOSL-HIGH'.
        MOVE '0' TO screen-input.
      ENDIF.
      s_ktosl-sign   = 'I'.
      s_ktosl-option = 'EQ'.
      s_ktosl-low    = '1'.
      s_ktosl-high   = ' ' .                                 .
      APPEND s_ktosl.
    ELSEIF rb_002 EQ 'X'.
*      CLEAR  : s_witht,
*               s_hkont.

      IF rb_002 EQ 'X' AND s_ktosl IS INITIAL.
*        REFRESH: s_ktosl.
      ELSE.
        CLEAR:   s_ktosl.
        REFRESH: s_witht,
                 s_hkont.
*                 s_ktosl.
        IF s_ktosl-low EQ '1'.
          REFRESH: s_ktosl.
        ENDIF.
      ENDIF.

      s_witht-sign   = 'I'.
      s_witht-option = 'EQ'.
      s_witht-low    = '1'.
      s_witht-high   = ' '.
      APPEND s_witht.
      IF screen-name = 'S_WITHT-LOW'.
        MOVE '0' TO screen-input.
      ENDIF.
      IF screen-name = 'S_WITHT-HIGH'.
        MOVE '0' TO screen-input.
      ENDIF.
*
      IF screen-name = 'S_KTOSL-LOW'.
        MOVE '1' TO screen-input.
      ENDIF.
      IF screen-name = 'S_KTOSL-HIGH'.
        MOVE '1' TO screen-input.
      ENDIF.
      IF screen-name = 'S_HKONT-HIGH'.
        MOVE '0' TO screen-input.
      ENDIF.
      IF screen-name = 'S_HKONT-LOW'.
        MOVE '0' TO screen-input.
      ENDIF.

    ELSEIF rb_003 EQ 'X'.
      CLEAR  : s_witht,
*               s_hkont,
               s_ktosl.

      REFRESH: s_witht,
*               s_hkont,
               s_ktosl.

      s_witht-sign   = 'I'.
      s_witht-option = 'EQ'.
      s_witht-low    = '1'.
      s_witht-high   = ' '.
      APPEND s_witht.

      s_ktosl-sign   = 'I'.
      s_ktosl-option = 'EQ'.
      s_ktosl-low    = '1'.
      s_ktosl-high   = ' ' .                                 .
      APPEND s_ktosl.

      IF screen-name = 'S_HKONT-LOW'.
        MOVE '1' TO screen-input.
      ENDIF.
      IF screen-name = 'S_HKONT-HIGH'.
        MOVE '1' TO screen-input.
      ENDIF.
      IF screen-name = 'S_WITHT-LOW'.
        MOVE '0' TO screen-input.
      ENDIF.
      IF screen-name = 'S_WITHT-HIGH'.
        MOVE '0' TO screen-input.
      ENDIF.
      IF screen-name = 'S_KTOSL-LOW'.
        MOVE '0' TO screen-input.
      ENDIF.
      IF screen-name = 'S_KTOSL-HIGH'.
        MOVE '0' TO screen-input.
      ENDIF.

    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
  IF rb_003 EQ 'X'.
    IF s_hkont IS INITIAL.
      MESSAGE s000(z_fi) WITH 'Completar la Cuenta ' DISPLAY LIKE 'E'..
    ENDIF.
  ENDIF.


ENDFORM.                    " F_GRISADO_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  F_ARCHIVO_RET
*&---------------------------------------------------------------------*
FORM f_archivo_ret  USING sl_bajada TYPE ty_bajada_ret.
  DATA: st_bajada TYPE ty_out_aux.
  CONCATENATE
              sl_bajada-cod
              sl_bajada-cuit
              sl_bajada-fecha
              sl_bajada-suc
              sl_bajada-nro
              sl_bajada-typcom
              sl_bajada-letra
              sl_bajada-compr
              sl_bajada-imp
* Begin of GB - 23.09.10
*  INTO st_bajada-linea.
  INTO st_bajada-linea RESPECTING BLANKS.
* End of GB - 23.09.10
  APPEND st_bajada TO t_bajada.
  CLEAR st_bajada.

ENDFORM.                    " F_ARCHIVO_RET
*&---------------------------------------------------------------------*
*&      Form  F_RETENCIONES
*&---------------------------------------------------------------------*
FORM f_retenciones TABLES tl_bajada_ret TYPE tyt_bajada_ret
                          p_tl_bseg     TYPE tp_t_bseg
                    USING sl_detail     TYPE ty_detail
                 CHANGING so_bajada_ret TYPE ty_bajada_ret.

  DATA: el_bseg      TYPE tp_s_bseg,
        vl_nro       TYPE regio,
        vl_qsskz     TYPE qsskz,
* Begin of GB - 28.09.10
        vl_pago_parc TYPE char1,
* End of GB - 28.09.10
        vl_certi     TYPE char11. "zfi_certif_cobr-certi.

  vl_pago_parc = 'X'.

  SORT p_tl_bseg[] BY augbl.
  IF sl_detail-perception EQ ' '.

    REFRESH t_item.


    LOOP AT p_tl_bseg INTO el_bseg
    WHERE augbl = sl_detail-belnr
    AND   belnr NE sl_detail-belnr
    AND   gjahr = sl_detail-gjahr
    AND   bukrs = sl_detail-bukrs.
* Begin of GB - 28.09.10
      CLEAR vl_pago_parc.
* End of GB - 28.09.10
      REFRESH t_item.

      PERFORM f_tipo_percepcion  USING el_bseg-ktosl CHANGING vl_nro.

      SELECT *
      FROM with_item
      INTO TABLE t_item
      WHERE witht IN s_witht
      AND   belnr = el_bseg-belnr
      AND   gjahr = el_bseg-gjahr
      AND   bukrs = el_bseg-bukrs
      and   buzei = el_bseg-buzei.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

*     codigo de juridiccion
      LOOP AT t_item INTO st_item
      WHERE witht IN s_witht
      AND   belnr = el_bseg-belnr
      AND   gjahr = el_bseg-gjahr
      AND   bukrs = el_bseg-bukrs.

        IF st_item-wt_qbshh = 0.
          CONTINUE.
        ENDIF.

        PERFORM f_codigo_de_juridiccion USING st_item-witht 'X'
                                     CHANGING so_bajada_ret-cod.

        SELECT SINGLE *
        FROM bkpf
        INTO st_bkpf
        WHERE  belnr = el_bseg-belnr
          AND  gjahr = el_bseg-gjahr
          AND  bukrs = el_bseg-bukrs.

        MOVE:
               st_bkpf-xblnr(4)   TO so_bajada_ret-suc   ,
               st_bkpf-xblnr+4(1) TO so_bajada_ret-letra ,
               st_bkpf-xblnr+5(8) TO so_bajada_ret-nro   .

        READ TABLE gt_item INTO gs_item
        WITH KEY belnr = sl_detail-belnr.

* Begin of GB - 23.09.10
*        SELECT SINGLE certi
*          INTO vl_certi
*          FROM zfi_certif_cobr
*          WHERE bukrs     = st_item-bukrs
*            AND belnr     = st_item-belnr
*            AND gjahr     = st_item-gjahr
*            AND witht     = st_item-witht
*            AND wt_withcd = st_item-wt_withcd
*            AND xblnr     = st_bkpf-xblnr
*            AND pago      = sl_detail-belnr.
        IF sy-subrc = 0.
          so_bajada_ret-compr = vl_certi.
        ELSE.
          CLEAR so_bajada_ret-compr.
        ENDIF.
* End of GB - 23.09.10

        SHIFT   so_bajada_ret-nro RIGHT DELETING TRAILING ''.
        OVERLAY so_bajada_ret-nro WITH '0000000000000000'.

        CONCATENATE sl_detail-stcd1(2)   '-'
                    sl_detail-stcd1+2(8) '-'
                    sl_detail-stcd1+10(1)
           INTO so_bajada_ret-cuit.

        PERFORM f_fecha USING st_bkpf-bldat
                     CHANGING so_bajada_ret-fecha .

        SHIFT   so_bajada_ret-compr  RIGHT DELETING TRAILING ''.
        OVERLAY so_bajada_ret-compr  WITH '00000000000000000000'.

*      Tipo de comprobante
        PERFORM f_tipo_comprobante USING st_bkpf-blart
                                CHANGING so_bajada_ret-typcom.
*      importe
        IF st_item-wt_qbshh < 0.
          st_item-wt_qbshh = st_item-wt_qbshh * - 1.
        ENDIF.
        PERFORM f_conversion_importe  USING st_item-wt_qbshh
                                   CHANGING so_bajada_ret-imp.

        APPEND so_bajada_ret TO tl_bajada_ret.
      ENDLOOP.
    ENDLOOP.

* Begin of GB - 28.09.10
    IF vl_pago_parc = 'X'.

      LOOP AT t_bseg INTO st_bseg
      WHERE belnr = sl_detail-belnr
        AND gjahr = sl_detail-gjahr
        AND bukrs = sl_detail-bukrs
        AND koart = 'D'.

        IF st_bseg-rebzg IS INITIAL.
          CONTINUE.
        ENDIF.

* Se obtienen los dats de cabecera del doc.
        SELECT SINGLE *
        FROM bkpf
        INTO st_bkpf
        WHERE bukrs = st_bseg-bukrs
          AND belnr = st_bseg-rebzg
          AND gjahr = st_bseg-gjahr.
* Se obtienen los registros de la with_item para el doc.

*-PAGOS-PARCIALES------------------------------------------------JMONA----------------------*
         data:  p_x_with_item LIKE with_itemx OCCURS 1 WITH HEADER LINE,
                i_x_with_item LIKE with_itemx OCCURS 1 WITH HEADER LINE,
                stl_RET       TYPE with_item.
         CALL FUNCTION 'FI_WT_READ_WITH_ITEM'
                 EXPORTING
                      i_bukrs  = st_bseg-bukrs
                      i_belnr  = st_bseg-belnr
                      i_gjahr  = st_bseg-gjahr
                      i_buzei  = st_bseg-buzei
*             I_WITHT  =
                 EXCEPTIONS
                      with_info_already_in_memory = 1
                      not_found                   = 2
                      OTHERS                      = 3.

            IF sy-subrc = 0.
*--- get data from global memory
              REFRESH: p_x_with_item,
                       i_x_with_item.

              CALL FUNCTION 'FI_WT_GET_X_WITH_ITEM'
                TABLES
                  t_with_item = p_x_with_item
                EXCEPTIONS
                  OTHERS      = 1.
* Saco las retenciones del documento
              REFRESH t_item.
*--- restrict to the requested line item
              LOOP AT p_x_with_item WHERE bukrs =  st_bseg-bukrs
                                      AND belnr =  st_bseg-belnr
                                      AND gjahr =  st_bseg-gjahr
                                      AND buzei =  st_bseg-buzei
                                      AND witht IN s_witht.

                MOVE-CORRESPONDING p_x_with_item to stl_ret .
                APPEND stl_ret TO t_item.

              ENDLOOP.

            ENDIF.
*F-I-N---------------------------------------------------PAGOS-PARCIALES-------JMONA---------------*

*     codigo de juridiccion
        LOOP AT t_item INTO st_item
        WHERE witht IN s_witht "vl_qsskz
          AND belnr = st_bseg-belnr
          AND gjahr = st_bseg-gjahr
          AND bukrs = st_bseg-bukrs.

          IF st_item-wt_qbshh = 0.
            CONTINUE.
          ENDIF.

          PERFORM f_codigo_de_juridiccion USING st_item-witht 'X'
                                       CHANGING so_bajada_ret-cod.

          so_bajada_ret-suc   = st_bkpf-xblnr(4).
          so_bajada_ret-letra = st_bkpf-xblnr+4(1).
          so_bajada_ret-nro   = st_bkpf-xblnr+5(8).

* Se obtiene el número de certificado
*          SELECT SINGLE certi
*            INTO vl_certi
*            FROM zfi_certif_cobr
*            WHERE bukrs     = st_bkpf-bukrs
*              AND belnr     = st_bkpf-belnr
*              AND gjahr     = st_bkpf-gjahr
*              AND witht     = st_item-witht
*              AND wt_withcd = st_item-wt_withcd
*              AND xblnr     = st_bkpf-xblnr
*              AND pago      = st_item-belnr.
          IF sy-subrc = 0.
            so_bajada_ret-compr = vl_certi.
          ELSE.
            CLEAR so_bajada_ret-compr.
          ENDIF.

          SHIFT   so_bajada_ret-nro RIGHT DELETING TRAILING ''.
          OVERLAY so_bajada_ret-nro WITH '0000000000000000'.

          CONCATENATE sl_detail-stcd1(2)   '-'
                      sl_detail-stcd1+2(8) '-'
                      sl_detail-stcd1+10(1)
             INTO so_bajada_ret-cuit.

          PERFORM f_fecha USING st_bkpf-bldat
                       CHANGING so_bajada_ret-fecha .

          SHIFT   so_bajada_ret-compr  RIGHT DELETING TRAILING ''.
          OVERLAY so_bajada_ret-compr  WITH '00000000000000000000'.

*      Tipo de comprobante
          PERFORM f_tipo_comprobante USING st_bkpf-blart
                                  CHANGING so_bajada_ret-typcom.
*      importe
          IF st_item-wt_qbshh < 0.
            st_item-wt_qbshh = st_item-wt_qbshh * - 1.
          ENDIF.
          PERFORM f_conversion_importe  USING st_item-wt_qbshh
                                     CHANGING so_bajada_ret-imp.

          APPEND so_bajada_ret TO tl_bajada_ret.

        ENDLOOP.
      ENDLOOP.
    ENDIF.
* End of GB - 28.09.10

  ENDIF.

ENDFORM.                    " F_RETENCIONES
*&---------------------------------------------------------------------*
*&      Form  F_PERCEPCIONES
*&---------------------------------------------------------------------*
FORM f_percepciones TABLES tl_bajada_per TYPE tyt_bajada_per
                    USING sl_detail      TYPE ty_detail
                 CHANGING so_bajada_per  TYPE ty_bajada_per.
  DATA: vl_nro    TYPE regio.


  IF sl_detail-perception EQ 'X'.
    LOOP AT t_bset INTO st_bset
    WHERE ktosl IN s_ktosl
    AND   belnr = sl_detail-belnr
    AND   gjahr = sl_detail-gjahr
    AND   bukrs = sl_detail-bukrs.

      IF sl_detail-belnr EQ st_bset-belnr .

        PERFORM f_tipo_percepcion  USING st_bset-ktosl CHANGING vl_nro.
*     codigo de juridiccion
        PERFORM f_codigo_de_juridiccion USING vl_nro ' '
                                     CHANGING so_bajada_per-cod.


        READ TABLE gt_item INTO gs_item
        WITH  KEY belnr = sl_detail-belnr
                  gjahr = sl_detail-gjahr.

        READ TABLE t_bkpf INTO st_bkpf
        WITH  KEY belnr = sl_detail-belnr
                  gjahr = sl_detail-gjahr
                  bukrs = sl_detail-bukrs.
        MOVE:

*            st_bkpf-bldat      TO so_bajada_per-fecha ,
              st_bkpf-xblnr(4)   TO so_bajada_per-suc   ,
              st_bkpf-xblnr+4(1) TO so_bajada_per-letra ,
              st_bkpf-xblnr+5(8) TO so_bajada_per-nro   .

        CONCATENATE sl_detail-stcd1(2)   '-'
                    sl_detail-stcd1+2(8) '-'
                    sl_detail-stcd1+10(1)
           INTO so_bajada_per-cuit.

        PERFORM f_fecha USING st_bkpf-bldat
                     CHANGING so_bajada_per-fecha .

*      Tipo de comprobante
        PERFORM f_tipo_comprobante USING st_bkpf-blart
                                CHANGING so_bajada_per-typcom.
*      importe
        PERFORM f_conversion_importe  USING gs_item-x_text2
                                   CHANGING so_bajada_per-imp.

        APPEND so_bajada_per TO tl_bajada_per.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_PERCEPCIONES
*&---------------------------------------------------------------------*
*&      Form  F_CODIGO_DE_JURIDICCION
*&---------------------------------------------------------------------*
FORM f_codigo_de_juridiccion  USING    pi_input pi_int
                              CHANGING po_cod.
  DATA:   stl_t059p TYPE t059p,
          stl_t005s TYPE t005s.

  IF pi_int EQ 'X'.
    READ TABLE t_t059p INTO stl_t059p
    WITH KEY witht = pi_input.

    IF sy-subrc EQ 0.
      READ TABLE t_t005s INTO stl_t005s
      WITH KEY bland = stl_t059p-regio.
      MOVE stl_t005s-fprcd TO po_cod.
    ENDIF.

  ELSE.
    READ TABLE t_t005s INTO stl_t005s
    WITH KEY bland = pi_input.

    IF sy-subrc EQ 0.
      MOVE stl_t005s-fprcd TO po_cod.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_CODIGO_DE_JURIDICCION

*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_IMPORTE
*&---------------------------------------------------------------------*
FORM f_conversion_importe  USING    pi_entrada
                           CHANGING po_salida.
  DATA : vl_imp   TYPE char18.

  WRITE: pi_entrada TO  vl_imp  LEFT-JUSTIFIED.
* Begin of GB - 23.09.10
  TRANSLATE vl_imp USING ', . '.
* End of GB - 23.09.10
  CONDENSE vl_imp NO-GAPS.
  MOVE:   vl_imp TO po_salida.
  SHIFT   po_salida  RIGHT DELETING TRAILING ''.
  OVERLAY po_salida  WITH '0000000000000000'.

ENDFORM.                    " F_CONVERSION_IMPORTE
*&---------------------------------------------------------------------*
*&      Form  F_TIPO_PERCEPCION
*&---------------------------------------------------------------------*
FORM f_tipo_percepcion  USING    pi_input
                        CHANGING po_nro.
  CASE: pi_input.
    WHEN 'J2F'.
      MOVE '01' TO po_nro.
    WHEN 'J1N'.
      MOVE '00' TO po_nro.
    WHEN 'J1V'.
      MOVE '03' TO po_nro.
    WHEN 'J1U'.
      MOVE '04' TO po_nro.
    WHEN 'J1Y'.
      MOVE '16' TO po_nro.
    WHEN 'J2G'.
      MOVE '11' TO po_nro.
    WHEN 'J1W'.
      MOVE '14' TO po_nro.
    WHEN 'J1Z'.
      MOVE '12' TO po_nro.
    WHEN 'J2H'.
      MOVE '19' TO po_nro.
    WHEN 'J2I'.
      MOVE '02' TO po_nro.
  ENDCASE.
ENDFORM.                    " F_TIPO_PERCEPCION
*&---------------------------------------------------------------------*
*&      Form  F_TIPO_COMPROBANTE
*&---------------------------------------------------------------------*
FORM f_tipo_comprobante  USING    pi_blart
                         CHANGING po_typcom.
  DATA: vl_blkls TYPE  j_1adoccl_.

* Begin of GB - 23.09.10
  IF pi_blart = 'UE' OR pi_blart = 'DZ'.
    po_typcom = 'F'.
    EXIT.
  ENDIF.
* End of GB - 23.09.10

  SELECT SINGLE blkls
  FROM t003
  INTO vl_blkls
  WHERE blart EQ pi_blart.

  CASE vl_blkls.
    WHEN 'A'.
      MOVE 'F' TO po_typcom.
    WHEN 'C'.
      MOVE 'C' TO po_typcom.
    WHEN 'D'.
      MOVE 'D' TO po_typcom.
    WHEN 'E'.
      MOVE 'O' TO po_typcom.
    WHEN 'F'.
      MOVE 'O' TO po_typcom.
  ENDCASE.
ENDFORM.                    " F_TIPO_COMPROBANTE
*&---------------------------------------------------------------------*
*&      Form  F_ARCHIVO_RET_BANCARIA
*&---------------------------------------------------------------------*
FORM f_archivo_ret_bancaria   USING sl_bajada TYPE ty_bajada_rec.
  DATA: st_bajada TYPE ty_banco.
  CONCATENATE
              sl_bajada-concp
              sl_bajada-cuit(2)   '-'
              sl_bajada-cuit+2(8) '-'
              sl_bajada-cuit+10(1)
              sl_bajada-periodo(4)'/'
              sl_bajada-periodo+5(2)
              sl_bajada-cbu
              sl_bajada-typcta
              sl_bajada-moneda
              sl_bajada-imp
         INTO st_bajada-linea.

  APPEND st_bajada TO t_banco.
  CLEAR st_bajada.
ENDFORM.                    " F_ARCHIVO_RET_BANCARIA
*&---------------------------------------------------------------------*
*&      Form  F_FECHA
*&---------------------------------------------------------------------*
FORM f_fecha  USING    pi_fecha
              CHANGING po_fecha.

  DATA: vl_fecha TYPE char10.

  WRITE pi_fecha TO vl_fecha.

  REPLACE '.'   WITH '/' INTO  vl_fecha.
  REPLACE '.'   WITH '/' INTO  vl_fecha.

  po_fecha = vl_fecha.
ENDFORM.                    " F_FECHA
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM f_alv .

*stl_bajada_rec TO tl_bajada_rec.
* *****Building the Field catalog.
  PERFORM fieldcat_build_alv1   USING  'TL_BAJADA_REC' gc_struct2
                              CHANGING gt_fieldcat.

*  ****Event build for hierarchical list
*  PERFORM eventtab_build CHANGING gt_events.
  REFRESH gt_events.
* Subroutine for total and subtotal
*  PERFORM t_sort_build_alv CHANGING gt_bsort.

*  ****layout build for hierarchical
  PERFORM layout_build CHANGING gs_layout.

**Note 1021760 begins
  gs_variant-handle = gc_handle1.
  gs_variant-report = sy-repid.
  gs_variant-variant = par_var1.
**Note 1021760 ends
  IF  tl_bajada_rec IS INITIAL.
    MESSAGE i000(z_fi) WITH 'No se encontraron datos con esa selección'.

  ELSE.
    SORT tl_bajada_rec BY bukrs
                          belnr
                          gjahr
                          cuit
                          concp
                          periodo.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = gv_repid
        is_layout          = gs_layout
        it_fieldcat        = gt_fieldcat
        it_events          = gt_events
        i_save             = 'A'
      TABLES
        t_outtab           = tl_bajada_rec
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DOC_RETENCIONES
*&---------------------------------------------------------------------*
FORM f_obtener_doc_retenciones CHANGING p_tl_bseg TYPE tp_t_bseg.

  CHECK rb_001 EQ 'X' AND detail[] IS NOT INITIAL.

  DATA ETL3036C2R6722 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L3036C2R5526 TYPE FAGL_T_FIELD.
LT_FIELDS_L3036C2R5526 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'AUGBL' )
 ( LINE = 'KTOSL' )
 ( LINE = 'BUZEI' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = DETAIL[]
              I_WHERE_CLAUSE = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR <> IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND AUGBL = IT_FOR_ALL_ENTRIES-BELNR AND REBZG = SPACE|
              IT_FIELDLIST = LT_FIELDS_L3036C2R5526
    IMPORTING ET_BSEG = ETL3036C2R6722
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL3036C2R6722 ) > 0.
  CLEAR P_TL_BSEG.
  TYPES: BEGIN OF TYL3036C2R4361,
    BUKRS TYPE BSEG-BUKRS,
    BELNR TYPE BSEG-BELNR,
    GJAHR TYPE BSEG-GJAHR,
    AUGBL TYPE BSEG-AUGBL,
    KTOSL TYPE BSEG-KTOSL,
    BUZEI TYPE BSEG-BUZEI,
  END OF TYL3036C2R4361.
  DATA: LML3036C2R6390 TYPE TYL3036C2R4361,
        LWL3036C2R7436 LIKE LINE OF P_TL_BSEG.
  LOOP AT ETL3036C2R6722 REFERENCE INTO DATA(LDRL3036C2R9748).
    LML3036C2R6390-BUKRS = LDRL3036C2R9748->BUKRS.
    LML3036C2R6390-BELNR = LDRL3036C2R9748->BELNR.
    LML3036C2R6390-GJAHR = LDRL3036C2R9748->GJAHR.
    LML3036C2R6390-AUGBL = LDRL3036C2R9748->AUGBL.
    LML3036C2R6390-KTOSL = LDRL3036C2R9748->KTOSL.
    LML3036C2R6390-BUZEI = LDRL3036C2R9748->BUZEI.
    LWL3036C2R7436 = LML3036C2R6390.
    APPEND LWL3036C2R7436 TO P_TL_BSEG.
  ENDLOOP.
  SY-DBCNT = LINES( ETL3036C2R6722 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


  SORT p_tl_bseg BY belnr gjahr bukrs.


ENDFORM.                    " F_OBTENER_DOC_RETENCIONES
