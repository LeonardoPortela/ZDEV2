*&---------------------------------------------------------------------*
*&  Include           ZFIY0009_FRM
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
  SELECT * FROM with_item WHERE bukrs      EQ bkpf-bukrs
                         AND   belnr       EQ bseg-belnr
                         AND   gjahr       EQ bseg-gjahr
                         AND   buzei       EQ bseg-buzei
                         AND   witht       IN s_witht
                         AND   wt_withcd   NE space
                         AND   wt_stat     EQ space
                         AND   wt_qbshh    NE 0.

    with_item-wt_qbshh = with_item-wt_qbshh * -1.

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
    APPEND with_item TO t_item.
  ENDSELECT.
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
  DATA: vft_belnr TYPE zmmt_ee_zgr_docs-ft_belnr,
        vft_gjahr TYPE zmmt_ee_zgr_docs-ft_gjahr.

***Note # 984952 begin
*  DETAIL-OFFNR = BKPF-BELNR .
***Note # 984952 end
  detail-qscod = xt059z-qscod.
  "
  SELECT SINGLE *
    FROM bsak
    INTO @DATA(wbsak)
    WHERE bukrs = @with_item-bukrs
    AND   augbl = @with_item-belnr
    AND   gjahr = @with_item-gjahr
    AND   blart IN ( 'ZY', 'ZG' ).

  IF sy-subrc = 0.

    IF wbsak-blart = 'ZY'.
      SELECT SINGLE *
        FROM zib_contabil_chv
        INTO  @DATA(wcontabil_chv)
        WHERE bukrs = @wbsak-bukrs
        AND   belnr = @wbsak-belnr
        AND   gjahr = @wbsak-gjahr.
      IF sy-subrc = 0.
        SELECT SINGLE *
          FROM zmmt_ee_zgr
          INTO @DATA(wzmmt_ee_zgr)
          WHERE obj_key = @wcontabil_chv-obj_key+2(12).
        IF sy-subrc = 0.
          IF wzmmt_ee_zgr-cd_txt_sisa IS NOT INITIAL.
            detail-qscod = wzmmt_ee_zgr-cd_txt_sisa.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSEIF  wbsak-blart =  'ZG'.

      SELECT SINGLE *
        FROM bkpf INTO @DATA(wa_bkpf)
         WHERE belnr  EQ @wbsak-belnr
         AND   bukrs  EQ @wbsak-bukrs
         AND   gjahr  EQ @wbsak-gjahr.

      IF sy-subrc = 0.

        vft_belnr = wa_bkpf-awkey+0(10).
        vft_gjahr = wa_bkpf-awkey+10(4).

        SELECT SINGLE  *
          FROM zmmt_ee_zgr_docs INTO @DATA(wa_zmmt_ee_zgr_docs)
           WHERE ft_belnr EQ @vft_belnr
           AND   ft_gjahr EQ @vft_gjahr.

        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM zmmt_ee_zgr
            INTO wzmmt_ee_zgr
            WHERE obj_key EQ wa_zmmt_ee_zgr_docs-obj_key.
          IF sy-subrc = 0.
            IF wzmmt_ee_zgr-cd_txt_sisa IS NOT INITIAL.
              detail-qscod = wzmmt_ee_zgr-cd_txt_sisa.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSE.
    SELECT SINGLE *
       FROM bsik
       INTO @DATA(wbsik)
       WHERE bukrs = @with_item-bukrs
       AND   belnr = @with_item-belnr
       AND   gjahr = @with_item-gjahr
       AND   blart IN ( 'ZY', 'ZG' ).

    IF sy-subrc = 0.
      IF wbsik-blart = 'ZY'.
        SELECT SINGLE *
        FROM zib_contabil_chv
        INTO  @DATA(wcontabil_chv2)
        WHERE bukrs = @wbsik-bukrs
        AND   belnr = @wbsik-belnr
        AND   gjahr = @wbsik-gjahr.
        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM zmmt_ee_zgr
            INTO @DATA(wzmmt_ee_zgr2)
            WHERE obj_key = @wcontabil_chv2-obj_key+2(12).
          IF sy-subrc = 0.
            IF wzmmt_ee_zgr2-cd_txt_sisa IS NOT INITIAL.
              detail-qscod = wzmmt_ee_zgr2-cd_txt_sisa.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF wbsik-blart =  'ZG'.

        SELECT SINGLE *
               FROM bkpf INTO wa_bkpf
                WHERE belnr  EQ wbsak-belnr
                AND   bukrs  EQ wbsak-bukrs
                AND   gjahr  EQ wbsak-gjahr.

        IF sy-subrc = 0.

          vft_belnr = wa_bkpf-awkey+0(10).
          vft_gjahr = wa_bkpf-awkey+10(4).

          SELECT SINGLE  *
            FROM zmmt_ee_zgr_docs INTO wa_zmmt_ee_zgr_docs
             WHERE ft_belnr EQ vft_belnr
             AND   ft_gjahr EQ vft_gjahr.

          IF sy-subrc = 0.
            SELECT SINGLE *
              FROM zmmt_ee_zgr
              INTO wzmmt_ee_zgr
              WHERE obj_key EQ wa_zmmt_ee_zgr_docs-obj_key.
            IF sy-subrc = 0.
              IF wzmmt_ee_zgr-cd_txt_sisa IS NOT INITIAL.
                detail-qscod = wzmmt_ee_zgr-cd_txt_sisa.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


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
  APPEND  tot_code.
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
  APPEND  tot_code.
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

* ALV Comment ends

    x_text = detail-dmbtr .
    TRANSLATE x_text USING '.,'.

* ALV CHANGES Start
    MOVE: detail-belnr  TO gs_header-belnr,
              detail-gjahr  TO gs_header-gjahr,
              detail-hkont  TO gs_header-hkont,
              detail-name1  TO gs_header-name1,
              detail-bukrs  TO gs_header-bukrs.

    IF detail-belnr NE tmp_belnr OR detail-bukrs NE tmp_bukrs.
      APPEND gs_header TO gt_header.
      CLEAR gs_header.
    ENDIF.
    tmp_bukrs = detail-bukrs.
    tmp_belnr = detail-belnr.


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

* ALV CHANGES starts

    MOVE: detail-date   TO gs_item-date1,
          detail-fityp  TO gs_item-fityp,
          x_text        TO gs_item-x_text2,
          detail-exrt   TO gs_item-exrt,
          detail-exdf   TO gs_item-exdf,
          detail-stcdt  TO gs_item-stcdt,
          detail-stcd1  TO gs_item-stcd1,
          detail-stcd2  TO gs_item-stcd2,
          detail-ctnum  TO gs_item-ctnum,
          detail-butxt  TO gs_item-butxt,
          detail-grsup  TO gs_item-grsup,
          detail-fptid  TO gs_item-fptid,
          detail-cctid  TO gs_item-cctid.
    APPEND gs_item TO gt_item.
    CLEAR gs_item.

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
*
*  ENDLOOP.
  REFRESH tot_code.

  LOOP AT gt_item INTO gs_item.

    READ TABLE detail WITH KEY
     gjahr = gs_item-gjahr
     qscod = gs_item-qscod
     belnr = gs_item-belnr.


    MOVE:  gs_item-qscod   TO tot_code-qscod,
*      Gs_ITEM-TXCOD   to tot_code-TXCOD,
          gs_item-belnr   TO tot_code-belnr,
          detail-hwste TO tot_code-hwste.


    READ TABLE xj1awtoff WITH KEY mandt     = sy-mandt
                                  land1     = t001-land1
                                  wt_qscod = tot_code-qscod.

    MOVE xj1awtoff-text40 TO tot_code-text40 .

    APPEND tot_code.

  ENDLOOP.

  FORMAT INTENSIFIED OFF.

  DELETE ADJACENT DUPLICATES FROM tot_code.


  SORT tot_code.

  xheader = 2.
*  NEW-PAGE.              "ALV Comment

  DELETE tot_code WHERE hwste = 0.

  LOOP AT tot_code.
    MOVE: tot_code-qscod  TO gs_tot_code-qscod,
          tot_code-text40 TO gs_tot_code-text40,
          tot_code-hwste  TO gs_tot_code-hwste.
    COLLECT  gs_tot_code INTO gt_tot_code.
*    APPEND gs_tot_code TO gt_tot_code.
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
*     BIN_FILESIZE              =
      filename                  = lcl_filename
      filetype                  = 'ASC'
*     APPEND                    = ' '
*     WRITE_FIELD_SEPARATOR     = ' '
*     HEADER                    = '00'
      trunc_trailing_blanks     = ' '
*     WRITE_LF                  = 'X'
*     COL_SELECT                = ' '
*     COL_SELECT_MASK           = ' '
*     DAT_MODE                  = ' '
*     CONFIRM_OVERWRITE         = ' '
*     NO_AUTH_CHECK             = ' '
*     CODEPAGE                  = ' '
*     IGNORE_CERR               = ABAP_TRUE
*     REPLACEMENT               = '#'
*     WRITE_BOM                 = ' '
      trunc_trailing_blanks_eol = ' '
*     WK1_N_FORMAT              = ' '
*     WK1_N_SIZE                = ' '
*     WK1_T_FORMAT              = ' '
*     WK1_T_SIZE                = ' '
*   IMPORTING
*     FILELENGTH                =
    TABLES
      data_tab                  = output_file1
*     FIELDNAMES                =
*   EXCEPTIONS
*     FILE_WRITE_ERROR          = 1
*     NO_BATCH                  = 2
*     GUI_REFUSE_FILETRANSFER   = 3
*     INVALID_TYPE              = 4
*     NO_AUTHORITY              = 5
*     UNKNOWN_ERROR             = 6
*     HEADER_NOT_ALLOWED        = 7
*     SEPARATOR_NOT_ALLOWED     = 8
*     FILESIZE_NOT_ALLOWED      = 9
*     HEADER_TOO_LONG           = 10
*     DP_ERROR_CREATE           = 11
*     DP_ERROR_SEND             = 12
*     DP_ERROR_WRITE            = 13
*     UNKNOWN_DP_ERROR          = 14
*     ACCESS_DENIED             = 15
*     DP_OUT_OF_MEMORY          = 16
*     DISK_FULL                 = 17
*     DP_TIMEOUT                = 18
*     FILE_NOT_FOUND            = 19
*     DATAPROVIDER_EXCEPTION    = 20
*     CONTROL_FLUSH_ERROR       = 21
*     OTHERS                    = 22
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

  DATA: ls_variant      TYPE disvariant,        " Structure for variant
        ls_variant_help TYPE disvariant,   " Structure for variant
        lv_exit(1)      TYPE c.                 "User-Exit while F4-Help

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
FORM fieldcat_build_alv  USING VALUE(is_struct1) TYPE dd02l-tabname
                              VALUE(is_struct2) TYPE dd02l-tabname
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
          <fs>-seltext_m  = TEXT-u21.
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
        <fs>-seltext_m  = TEXT-u01.
        <fs>-ddictxt    = gc_m.
      WHEN 'DATE' .
        <fs>-col_pos      = 6.
        <fs>-seltext_m  = TEXT-u02.
        <fs>-ddictxt    = gc_m.

      WHEN 'OFFNR' .
        <fs>-col_pos      = 7.
        <fs>-seltext_m  = TEXT-u03.
        <fs>-ddictxt    = gc_m.

      WHEN 'X_TEXT' .
        <fs>-col_pos      = 8.
        <fs>-seltext_m  = TEXT-u04.
        <fs>-ddictxt    = gc_m.

      WHEN 'TXCOD' .
        <fs>-col_pos      = 9.
        <fs>-seltext_m  = TEXT-u05.
        <fs>-ddictxt    = gc_m.

      WHEN 'QSCOD' .
        <fs>-col_pos      = 10.
        <fs>-seltext_m  = TEXT-u06.
        <fs>-ddictxt    = gc_m.

      WHEN 'TCODE' .
        <fs>-col_pos      = 11.
        <fs>-seltext_m  = TEXT-u07.
        <fs>-ddictxt    = gc_m.

      WHEN 'X_TEXT1' .
        <fs>-col_pos      = 12.
        <fs>-seltext_m  = TEXT-u08.
        <fs>-ddictxt    = gc_m.

      WHEN 'DATE1' .
        <fs>-col_pos      = 13.
        <fs>-seltext_m  = TEXT-u09.
        <fs>-ddictxt    = gc_m.

      WHEN 'FITYP' .
        <fs>-col_pos      = 14.
        <fs>-seltext_m  = TEXT-u10.
        <fs>-ddictxt    = gc_m.

      WHEN 'X_TEXT2' .
        <fs>-col_pos      = 15.
        <fs>-seltext_m  = TEXT-u11.
        <fs>-ddictxt    = gc_m.

      WHEN 'EXRT' .
        <fs>-col_pos      = 16.
        <fs>-seltext_m  = TEXT-u12.
        <fs>-ddictxt    = gc_m.

      WHEN 'EXDF' .
        <fs>-col_pos      = 17.
        <fs>-seltext_m  = TEXT-u13.
        <fs>-ddictxt    = gc_m.

      WHEN 'STCD1' .
        <fs>-col_pos      = 18.
        <fs>-seltext_m  = TEXT-u14.
        <fs>-ddictxt    = gc_m.

      WHEN 'STCD2' .
        <fs>-col_pos      = 19.
        <fs>-seltext_m  = TEXT-u14.
        <fs>-ddictxt    = gc_m.

      WHEN 'CTNUM' .
        <fs>-col_pos      = 20.
        <fs>-seltext_m  = TEXT-u15.
        <fs>-ddictxt    = gc_m.

      WHEN 'BUTXT' .
        <fs>-col_pos      = 21.
        <fs>-seltext_m  = TEXT-u16.
        <fs>-ddictxt    = gc_m.

      WHEN 'GRSUP' .
        <fs>-col_pos      = 23.
        <fs>-seltext_m  = TEXT-u17.
        <fs>-ddictxt    = gc_m.

      WHEN 'FPTID' .
        <fs>-col_pos      = 24.
        <fs>-seltext_m  = TEXT-u18.
        <fs>-ddictxt    = gc_m.

      WHEN 'CCTID' .
        <fs>-col_pos      = 25.
        <fs>-seltext_m  = TEXT-u19.
        <fs>-ddictxt    = gc_m.

    ENDCASE.
  ENDLOOP.
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

  lr_grid->create_text( row = 1 column = 1 text = TEXT-h05 ).

  lr_grid->create_text( row = 1 column = 2 text = from_date ).

  IF to_date NE from_date.

    lr_grid->create_text( row = 1 column = 1 text = TEXT-h05 ).

    lr_grid->create_text( row = 1 column = 2 text = from_date ).

    lr_grid->create_text( row = 1 column = 3 text = lv_hyphen ).

    lr_grid->create_text( row = 1 column = 4 text = to_date ).
  ENDIF.

  IF s_newdat NE space.

    lr_grid->create_text( row = 2 column = 1 text = TEXT-h06 ).

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
*     i_internal_tabname     = gt_tot_code " 'GT_TOT_CODE' "gt_tot_code
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
  DATA: len_housenum  TYPE i,
        len_street    TYPE i,
        len_total     TYPE i,
        len_str_trunc TYPE i.
  SORT withheld BY stcd1 city region postal_cd doc_type.
  IF NOT withheld IS INITIAL AND NOT p_sfile IS INITIAL AND NOT p_sfnam IS INITIAL.
    OPEN DATASET p_sfnam FOR OUTPUT IN TEXT MODE
    ENCODING NON-UNICODE IGNORING CONVERSION ERRORS.
    LOOP AT withheld.
      CLEAR output_withheld.
      len_housenum = strlen( withheld-house_num ).
      len_street = strlen( withheld-street ).
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
    filename TYPE string,
    path     TYPE string,
    fullpath TYPE string,
    p_file   TYPE rlgrap-filename.

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
  DATA: tl_bajada     TYPE STANDARD TABLE OF ty_bajada,
        stl_bajada    TYPE ty_bajada,
        stl_lfa1      TYPE lfa1,
        stl_bsak      TYPE bsak,
        stl_ret       TYPE with_item,
        stl_bkpf      TYPE bkpf,

        p_x_with_item LIKE with_itemx OCCURS 1 WITH HEADER LINE,
        i_x_with_item LIKE with_itemx OCCURS 1 WITH HEADER LINE,
        p_h_bkpf      TYPE bkpf,
        p_h_bseg      TYPE bseg,

        tl_bsak       TYPE STANDARD TABLE OF bsak,
        tl_ret        TYPE STANDARD TABLE OF with_item,

        v_tabix       TYPE sy-tabix,
        vl_qsskz      TYPE qsskz,
        vl_gridt      TYPE j_1adtyp_d,
        vl_gityp      TYPE j_1adtyp_d,
        vl_text       TYPE char16,
        vl_tabla      TYPE char20.

  SORT  t_bseg   BY  belnr gjahr bukrs .
  SORT  t_bkpf   BY  belnr gjahr bukrs .
  SORT  t_bset   BY  belnr gjahr bukrs .
  SORT  detail   BY  belnr gjahr bukrs .
  SORT  t_item   BY  belnr gjahr bukrs .

  IF tl_bsak IS INITIAL AND detail[] IS NOT INITIAL.
    SELECT  *
    FROM bsak
    INTO TABLE tl_bsak
    FOR ALL ENTRIES IN detail
    WHERE augbl EQ detail-belnr
      AND bukrs EQ detail-bukrs
      AND auggj EQ detail-gjahr
      AND belnr NE detail-belnr.

    SELECT  *
    FROM bsik
    APPENDING TABLE tl_bsak
    FOR ALL ENTRIES IN detail
    WHERE bukrs EQ detail-bukrs
      AND gjahr EQ detail-gjahr
      AND belnr EQ detail-belnr.

  ENDIF.

  LOOP AT detail .
    APPEND detail TO detailaux.
  ENDLOOP.


  DELETE ADJACENT DUPLICATES FROM detail COMPARING belnr .
  LOOP AT detail .
    CLEAR stl_bajada.
    READ TABLE gt_item INTO gs_item
    WITH  KEY belnr = detail-belnr
              gjahr = detail-gjahr.

    READ TABLE t_bkpf INTO st_bkpf
        WITH  KEY belnr = detail-belnr
                  gjahr = detail-gjahr
                  bukrs = detail-bukrs.

*----> dependiendo el blart que tiene el documento busca el valor del campo doccls
    READ TABLE t_doctyp
        WITH  KEY blart = st_bkpf-blart.

    READ TABLE t_doclas WITH KEY land1     = t001-land1
                                 id_report = sy-repid
                                 doccls    = t_doctyp-doccls .

*    if stl_bkpf-blart eq 'UE'.
*      MOVE '39'   TO stl_bajada-comp   .                   "/ Modificação CS2016001281
*    endif.

    MOVE:
*         t_doclas-j_1aoftp   TO stl_bajada-comp       ,    "/ Modificação CS2016001281
         detail-stcd1        TO stl_bajada-nrodocret  ,
         '0'                 TO stl_bajada-suspen     ,
         detail-name1        TO stl_bajada-name       ,
         gs_item-txcod       TO stl_bajada-impuesto   ,
         gs_item-stcdt       TO stl_bajada-docret     ,
         '01'                TO stl_bajada-condicion  ,
         gs_item-qscod       TO stl_bajada-regimen    .

    IF detail-stcd1 IS NOT INITIAL .
      MOVE detail-stcd1        TO stl_bajada-nrodocret.
    ELSEIF detail-stcd2 IS NOT INITIAL.
      MOVE detail-stcd2        TO stl_bajada-nrodocret.
    ENDIF.

    IF st_bkpf-xblnr IS NOT INITIAL.
      CONCATENATE  '0000'
                   st_bkpf-xblnr(4)
                   st_bkpf-xblnr+5(8)
              INTO stl_bajada-nrocomp RESPECTING BLANKS.

      PERFORM change_comp USING stl_bajada-nrocomp
                          CHANGING stl_bajada-comp.

    ENDIF.
*   Fecha de Emisión
    PERFORM f_fecha USING st_bkpf-bldat
                 CHANGING stl_bajada-femision.
*   Fecha de Retención
    PERFORM f_fecha USING st_bkpf-bldat
                 CHANGING stl_bajada-fecharet.
*   Fecha de Boletin
    OVERLAY  stl_bajada-fboletin  WITH '0000000000000000'.


    IF detail-perception EQ 'X'.
*   Importe del comprobante
      PERFORM f_conversion_importe  USING gs_item-x_text
                                 CHANGING stl_bajada-impcomp.
*   Importe Base
      PERFORM f_conversion_importe  USING gs_item-x_text1
                                 CHANGING stl_bajada-base.
*   Importe Impuesto
      PERFORM f_conversion_importe  USING gs_item-x_text2
                                 CHANGING stl_bajada-imp_ret.

*      PERFORM tipo_documento        USING detail-stcdt
*                                 CHANGING stl_bajada-docret.

      CLEAR stl_bajada-porc.
      stl_bajada-porc+5(1) = '0'.
      IF st_bkpf-xblnr IS NOT INITIAL.
        CONCATENATE  '0000'
                      st_bkpf-xblnr(4)
                      st_bkpf-xblnr+5(8)
                 INTO stl_bajada-nrocomp RESPECTING BLANKS.

        PERFORM change_comp USING stl_bajada-nrocomp
                            CHANGING stl_bajada-comp.

      ENDIF.
      CLEAR    stl_bajada-certif.
*      OVERLAY  stl_bajada-certif  WITH '              '.
      SHIFT stl_bajada-nrocomp LEFT DELETING LEADING ' '.

      MOVE '2'  TO stl_bajada-operacion  .

      APPEND stl_bajada TO tl_bajada.

    ELSE.

      stl_bajada-porc+5(1) = '0'.

      READ TABLE t_lfa1 INTO stl_lfa1
      WITH KEY stcd1 = detail-stcd1.

      REFRESH tl_bsak.

      SELECT  *
      FROM bsak
      INTO TABLE tl_bsak
*    FOR ALL ENTRIES IN detail
      WHERE augbl EQ detail-belnr
        AND bukrs EQ detail-bukrs
        AND auggj EQ detail-gjahr
        AND belnr NE detail-belnr.

      SELECT  *
      FROM bsik
      APPENDING TABLE tl_bsak
*    FOR ALL ENTRIES IN detail
      WHERE bukrs EQ detail-bukrs
        AND gjahr EQ detail-gjahr
        AND belnr EQ detail-belnr.

      LOOP AT tl_bsak INTO stl_bsak
      WHERE
            ( augbl EQ detail-belnr
        AND   auggj EQ detail-gjahr
        AND   bukrs EQ detail-bukrs
        AND   belnr NE detail-belnr
        AND ( blart NE 'KZ' OR blart NE 'ZP' )  )

        OR
            ( bukrs  EQ detail-bukrs
       AND    belnr  EQ detail-belnr
       AND    gjahr  EQ detail-gjahr
       AND  ( blart  NE 'KZ' OR blart NE 'ZP' ) ).

        v_tabix = sy-tabix.
*------------------------------------------------------------------------------*
* Saco el tipo de comprobante
        CLEAR: stl_bkpf,
               t_doclas,
               t_doclas.

        SELECT SINGLE *
          INTO stl_bkpf
            FROM bkpf
              WHERE bukrs EQ stl_bsak-bukrs
                AND belnr EQ stl_bsak-belnr
                AND gjahr EQ stl_bsak-gjahr.

* "/ Modificação CS2016001281
*        READ TABLE T_DOCTYP WITH  KEY BLART    = STL_BKPF-BLART.
*        IF SY-SUBRC EQ 0.
*          READ TABLE T_DOCLAS WITH KEY LAND1     = T001-LAND1
*                                       ID_REPORT = SY-REPID
*                                       DOCCLS    = T_DOCTYP-DOCCLS .
*          IF SY-SUBRC EQ 0.
*            MOVE T_DOCLAS-J_1AOFTP   TO STL_BAJADA-COMP   .
*          ENDIF.
*        ENDIF.
*        IF STL_BKPF-BLART EQ 'UE'.
*          MOVE '39'   TO STL_BAJADA-COMP   .
*        ENDIF.

*------------------------------------------------------------------------------*

        IF stl_bsak-rebzg IS INITIAL.
          DELETE tl_bsak INDEX v_tabix.
          MOVE  '1'     TO stl_bajada-operacion  .

          MOVE stl_bsak-dmbtr   TO stl_bajada-impcomp                .
          TRANSLATE stl_bajada-impcomp   USING '.,'                 .
          SHIFT     stl_bajada-impcomp   RIGHT DELETING TRAILING ' '.
*         OVERLAY   stl_bajada-impcomp   WITH '0000000000000000'    .
          IF stl_bsak-xblnr IS NOT INITIAL.
            CONCATENATE  '0000'
                         stl_bsak-xblnr(4)
                         stl_bsak-xblnr+5(8)
                  INTO   stl_bajada-nrocomp RESPECTING BLANKS.

            PERFORM change_comp USING stl_bajada-nrocomp
                                CHANGING stl_bajada-comp.

          ENDIF.
* Saco las retenciones del documento
          REFRESH tl_ret.
          SELECT *
          FROM with_item
          INTO TABLE tl_ret
          WHERE gjahr EQ stl_bsak-gjahr
          AND   bukrs EQ stl_bsak-bukrs
          AND   buzei EQ stl_bsak-buzei
          AND   witht IN s_witht
          AND   belnr EQ stl_bsak-belnr.


          IF tl_ret IS NOT INITIAL.
* Paso las retenciones a la estructura de bajada.
            LOOP AT tl_ret INTO stl_ret.

              IF stl_ret-wt_qbshh < 0.
                stl_ret-wt_qbshh  = stl_ret-wt_qbshh * - 1.
              ENDIF.

              CLEAR  vl_text.
              vl_text = stl_ret-wt_qbshh.
              TRANSLATE vl_text USING '.,'.

              PERFORM f_conversion_importe  USING vl_text
                             CHANGING stl_bajada-imp_ret.

              IF stl_ret-wt_qsshh < 0.
                stl_ret-wt_qsshh = stl_ret-wt_qsshh * - 1.
              ENDIF.

              CLEAR  vl_text.
              vl_text = stl_ret-wt_qsshh.
              TRANSLATE vl_text USING '.,'.

              PERFORM f_conversion_importe  USING vl_text    "gs_item-x_text2
                                         CHANGING stl_bajada-base.

              SHIFT stl_bajada-nrocomp LEFT DELETING LEADING ' '.

*              CLEAR STL_BAJADA-REGIMEN.
*           Tipo de regimen
              IF stl_bajada-regimen IS INITIAL.
                SELECT SINGLE qscod
                FROM t059z
                INTO stl_bajada-regimen
                WHERE land1	     EQ 'AR'
                AND   witht      EQ stl_ret-witht
                AND   wt_withcd  EQ stl_ret-wt_withcd.
              ELSE.
                sy-subrc = 0.
              ENDIF.
              IF sy-subrc EQ 0.
                CLEAR stl_bajada-impuesto.
                SELECT SINGLE j_1anatxcd
                FROM t059o
                INTO stl_bajada-impuesto
                WHERE land1	     EQ 'AR'
                AND   wt_qscod   EQ stl_bajada-regimen.

              ENDIF.
*              CLEAR st_item .

*           Nro de certificado
*              READ TABLE t_item INTO st_item
*              WITH KEY  witht      = stl_ret-witht
*                        wt_withcd  = stl_ret-wt_withcd
*                        belnr      = stl_bsak-augbl.
*
*           Certificado de retencion
              IF stl_ret-ctnumber IS INITIAL.
                SELECT SINGLE ctnumber
                FROM with_item
                INTO stl_ret-ctnumber
                WHERE witht      = stl_ret-witht
                AND   wt_withcd  = stl_ret-wt_withcd
                AND   belnr      = detail-belnr.
              ENDIF.

*           Certificado de retencion
              WRITE:    stl_ret-ctnumber TO stl_bajada-certif RIGHT-JUSTIFIED.
              OVERLAY   stl_bajada-certif  WITH '0000000000000000'    .

              IF stl_bajada-nrocomp IS NOT INITIAL.
                APPEND stl_bajada TO tl_bajada.
              ENDIF.
            ENDLOOP.
*          ELSE.
*            APPEND stl_bajada TO tl_bajada.
          ENDIF.

        ELSE.
******************************************************************************************
*************************Pagos parciales**************************************************
******************************************************************************************

          SELECT SINGLE *
          FROM bkpf
          INTO  p_h_bkpf
          WHERE belnr EQ stl_bsak-belnr
          AND gjahr   EQ stl_bsak-gjahr.

          CALL FUNCTION 'FI_WT_READ_WITH_ITEM'
            EXPORTING
              i_bukrs                     = stl_bsak-bukrs
              i_belnr                     = stl_bsak-belnr
              i_gjahr                     = stl_bsak-gjahr
              i_buzei                     = stl_bsak-buzei
*             I_WITHT                     =
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
            REFRESH tl_ret.
*--- restrict to the requested line item
            LOOP AT p_x_with_item WHERE bukrs =  stl_bsak-bukrs
                                    AND belnr =  stl_bsak-belnr
                                    AND gjahr =  stl_bsak-gjahr
                                    AND buzei =  stl_bsak-buzei
                                    AND witht IN s_witht.

              stl_ret = p_x_with_item.
              APPEND stl_ret TO tl_ret.
            ENDLOOP.
          ENDIF.
*------------------------------------------------------------------------------*
          CLEAR: "stl_bkpf,
                 t_doclas,
                 t_doclas.

* docuemnto legal pro la factura
          SELECT SINGLE xblnr blart
          FROM bkpf
          INTO  (stl_bsak-xblnr, stl_bkpf-blart)
          WHERE belnr EQ stl_bsak-rebzg
            AND bukrs EQ stl_bsak-bukrs
           AND gjahr   EQ stl_bsak-rebzj.

* "/ Modificação CS2016001281
*          READ TABLE T_DOCTYP WITH  KEY BLART    = STL_BKPF-BLART.
*          IF SY-SUBRC EQ 0.
*            READ TABLE T_DOCLAS WITH KEY LAND1     = T001-LAND1
*                                         ID_REPORT = SY-REPID
*                                         DOCCLS    = T_DOCTYP-DOCCLS .
*            IF SY-SUBRC EQ 0.
*
*              MOVE T_DOCLAS-J_1AOFTP   TO STL_BAJADA-COMP   .
*
*            ENDIF.
*          ENDIF.
*          IF STL_BKPF-BLART EQ 'UE'.
*            MOVE '39'   TO STL_BAJADA-COMP   .
*          ENDIF.



*------------------------------------------------------------------------------*
*        DELETE tl_bsak INDEX sy-tabix.
          MOVE  '1'     TO stl_bajada-operacion  .

          MOVE stl_bsak-dmbtr   TO stl_bajada-impcomp                .
          TRANSLATE stl_bajada-impcomp   USING '.,'                 .
          SHIFT     stl_bajada-impcomp   RIGHT DELETING TRAILING ' '.
*         OVERLAY   stl_bajada-impcomp   WITH '0000000000000000'    .

*         IF sy-subrc EQ 0.
          IF stl_bsak-xblnr IS NOT INITIAL.
            CONCATENATE '0000'
                        stl_bsak-xblnr(4)
                       stl_bsak-xblnr+5(8)
                  INTO stl_bajada-nrocomp RESPECTING BLANKS.

            PERFORM change_comp USING stl_bajada-nrocomp
                                CHANGING stl_bajada-comp.

          ENDIF.
*         ENDIF.

          IF tl_ret IS NOT INITIAL.
* Paso las retenciones a la estructura de bajada.
            LOOP AT tl_ret INTO stl_ret.

              IF stl_ret-wt_qbshh < 0.
                stl_ret-wt_qbshh  = stl_ret-wt_qbshh * - 1.
              ENDIF.

              CLEAR  vl_text.
              vl_text = stl_ret-wt_qbshh.
              TRANSLATE vl_text USING '.,'.

              PERFORM f_conversion_importe  USING vl_text
                             CHANGING stl_bajada-imp_ret.

              IF stl_ret-wt_qsshh < 0.
                stl_ret-wt_qsshh = stl_ret-wt_qsshh * - 1.
              ENDIF.

              CLEAR  vl_text.
              vl_text = stl_ret-wt_qsshh.
              TRANSLATE vl_text USING '.,'.

              PERFORM f_conversion_importe  USING vl_text    "gs_item-x_text2
                                         CHANGING stl_bajada-base.

              SHIFT stl_bajada-nrocomp LEFT DELETING LEADING ' '.

              CLEAR stl_bajada-regimen.
*           tipo de regimen
              SELECT SINGLE qscod
              FROM t059z
              INTO stl_bajada-regimen
              WHERE land1	     EQ 'AR'
              AND   witht      EQ stl_ret-witht
              AND   wt_withcd  EQ stl_ret-wt_withcd.
              IF sy-subrc EQ 0.

*           Tipo de impuesto
                CLEAR stl_bajada-impuesto.
                SELECT SINGLE j_1anatxcd
                FROM t059o
                INTO stl_bajada-impuesto
                WHERE land1	     EQ 'AR'
                AND   wt_qscod   EQ stl_bajada-regimen.

              ENDIF.
              CLEAR st_item .

*           Nro de certificado
*              READ TABLE t_item INTO st_item
*              WITH KEY  witht      = stl_ret-witht
*                        wt_withcd  = stl_ret-wt_withcd
*                        belnr      = stl_bsak-augbl.


*           Certificado de retencion
              IF stl_ret-ctnumber IS INITIAL.
                SELECT SINGLE ctnumber
                FROM with_item
                INTO stl_ret-ctnumber
                WHERE witht      = stl_ret-witht
                AND   wt_withcd  = stl_ret-wt_withcd
                AND   belnr      = detail-belnr.
              ENDIF.

*           Certificado de retencion
              WRITE:    stl_ret-ctnumber TO stl_bajada-certif RIGHT-JUSTIFIED.
              OVERLAY   stl_bajada-certif  WITH '0000000000000000'    .

              APPEND stl_bajada TO tl_bajada.

            ENDLOOP.
*          ELSE.
*            APPEND stl_bajada TO tl_bajada.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  REFRESH detail    .
  CLEAR:   detail   ,
           detailaux.

  LOOP AT detailaux.
    APPEND detailaux TO detail.
  ENDLOOP.


  LOOP AT tl_bajada INTO stl_bajada.
    PERFORM f_archivo USING stl_bajada.
  ENDLOOP.

*Bajo el archivo
  PERFORM f_bajada.
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
FORM f_archivo  USING sl_bajada TYPE ty_bajada.


  CONCATENATE
              sl_bajada-comp
              sl_bajada-femision
              sl_bajada-nrocomp
              sl_bajada-impcomp
              sl_bajada-impuesto
              sl_bajada-regimen
              sl_bajada-operacion
              sl_bajada-base
              sl_bajada-fecharet
              sl_bajada-condicion
              sl_bajada-suspen
              sl_bajada-imp_ret
              sl_bajada-porc
              sl_bajada-fboletin
              sl_bajada-docret
              sl_bajada-nrodocret
              sl_bajada-certif
              sl_bajada-name
  INTO st_bajada-linea RESPECTING BLANKS .
  IF sl_bajada-nrocomp IS NOT INITIAL.
    IF sl_bajada-imp_ret NE '          0,00'.
      APPEND st_bajada TO t_bajada.
    ENDIF.
  ENDIF.
  CLEAR st_bajada.
ENDFORM.                    " F_ARCHIVO
*&---------------------------------------------------------------------*
*&      Form  TIPO_DOCUMENTO
*&---------------------------------------------------------------------*
FORM tipo_documento  USING    pi_stcdt
                     CHANGING po_tp_doc  .
  CASE pi_stcdt.
    WHEN '96'.
      MOVE: '1'  TO po_tp_doc  .
    WHEN '80'.
      MOVE: '2'  TO po_tp_doc  .
    WHEN '86'.
      MOVE: '3'  TO po_tp_doc  .
    WHEN '00'.
      MOVE: '7'  TO po_tp_doc  .
  ENDCASE.

ENDFORM.                    " TIPO_DOCUMENTO
*&---------------------------------------------------------------------*
*&      Form  F_SITUACION_II_BB
*&---------------------------------------------------------------------*
FORM f_situacion_ii_bb  USING    pi_gridt
                        CHANGING po_situacion.
  CASE pi_gridt.
    WHEN  'I'.
      MOVE '1' TO po_situacion.
    WHEN  'M'.
      MOVE '2' TO po_situacion.
    WHEN  'N'.
      MOVE '4' TO po_situacion.
  ENDCASE.
ENDFORM.                    " F_SITUACION_II_BB
*&---------------------------------------------------------------------*
*&      Form  F_BAJADA
*&---------------------------------------------------------------------*
FORM f_bajada .
  DATA: p_path TYPE string,
        flen   TYPE i.
  MOVE p_fila TO  p_path .
  IF t_bajada IS NOT INITIAL.
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

ENDFORM.                    " F_BAJADA
*&---------------------------------------------------------------------*
*&      Form  F_CONVERSION_IMPORTE
*&---------------------------------------------------------------------*
FORM f_conversion_importe  USING    pi_entrada
                           CHANGING po_salida.
  DATA : vl_imp   TYPE char18.

  WRITE: pi_entrada TO  vl_imp  LEFT-JUSTIFIED.

  CONDENSE vl_imp NO-GAPS.
  MOVE:   vl_imp TO po_salida.
  SHIFT   po_salida  RIGHT DELETING TRAILING ''.
*  OVERLAY po_salida  WITH '0000000000000000'.

ENDFORM.                    " F_CONVERSION_IMPORTE
*&---------------------------------------------------------------------*
*&      Form  F_FECHA
*&---------------------------------------------------------------------*
FORM f_fecha  USING    pi_entrada
              CHANGING po_salida.

  WRITE pi_entrada   TO po_salida   DD/MM/YYYY.

  REPLACE '.'   WITH '/' INTO  po_salida.
  REPLACE '.'   WITH '/' INTO  po_salida.

ENDFORM.                    " F_FECHA

*&---------------------------------------------------------------------*
*&      Form  F_SITUACION_IVA
*&---------------------------------------------------------------------*
FORM f_situacion_iva  USING    pi_fityp
                      CHANGING po_fityp.

  CASE pi_fityp.
    WHEN '01'.
      MOVE '1' TO po_fityp.
    WHEN '04'.
      MOVE '3' TO po_fityp.
    WHEN '06'.
      MOVE '4' TO po_fityp.
    WHEN '03'OR '05' OR '07' OR '08' OR '09' OR '10' OR '11' OR '12' .
      MOVE '5' TO po_fityp.
  ENDCASE.

ENDFORM.                    " F_SITUACION_IVA

*&---------------------------------------------------------------------*
*&      Form  CHANGE_COMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_STL_BAJADA_NROCOMP  text
*      <--P_STL_BAJADA_COMP  text
*----------------------------------------------------------------------*
FORM change_comp  USING    p_stl_bajada_nrocomp
                  CHANGING p_stl_bajada_comp.

  DATA: vl_nrocomp TYPE char16.

  vl_nrocomp = p_stl_bajada_nrocomp.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = vl_nrocomp
    IMPORTING
      output = vl_nrocomp.

  IF vl_nrocomp(4) EQ '3301' OR vl_nrocomp(4) EQ '3310'.
    p_stl_bajada_comp = '10'.
  ELSE.
    p_stl_bajada_comp = '05'.
  ENDIF.

ENDFORM.
