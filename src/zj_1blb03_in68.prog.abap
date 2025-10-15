***INCLUDE J_1BLB03_IN68 .
data gv_batchsplit type c.                            " note 1162428

*----------------------------------------------------------------------*
* FORM
*----------------------------------------------------------------------*


FORM nf USING
              matnr
              werks
              bwtar
              mblnr
              zeile
              meins
              mat_quant
              erfmg            " note 1137434
              erfme            " note 1137434
              xblnr                               " 773139
              company_code
              year
              caller
              field_symbol.

  DATA: refkey     LIKE j_1bnflin-refkey, "unique identification of BI,
                                       "IV, LIV or MD document - to
                                       "determine NF
        refitm     LIKE j_1bnflin-refitm. "Reference item to source doc
  DATA: int_vbfa LIKE vbfa OCCURS 0 WITH HEADER LINE.
  DATA: returns_del TYPE c.
  DATA: ls_lips TYPE lips.                        " 773139
  DATA: lv_posnr TYPE posnv.                      " note 948019

*----------------------------------------------------------------------*
* check: MD -> NF ?
*----------------------------------------------------------------------*

  CONCATENATE mblnr year INTO refkey.
  PERFORM nf_find USING refkey
                        const_reftyp-mm
                        zeile
                        meins
                        matnr
                        mat_quant        " note 1137434
                        erfmg            " note 1137434
                        erfme            " note 1137434
                        field_symbol.

  READ TABLE it_refdoc WITH KEY
                            gjahr = year
*                           belnr = mblnr                  " note 684417
                            lfbnr = mblnr                  " note 684417
*                            buzei = zeile.
                            lfpos = zeile.                 " note 797729

  IF sy-subrc IS INITIAL.
    found = 'X'.
    rest = 0.
    EXIT.
  ENDIF.

  IF found = ' '.

*----------------------------------------------------------------------*
* check: MD -> IV -> NF ? / MD -> LIV -> NF
*----------------------------------------------------------------------*
*If it is a Returns movement, get the Reference MD number "1429621
    DATA: ls_mseg TYPE mseg,                              "1429621
          ls_ekbe TYPE ekbe.                              "1429621
    SELECT SINGLE * FROM mseg into ls_mseg                "1429621
                    WHERE mblnr = mblnr                   "1429621
                      AND mjahr = year                    "1429621
                      AND zeile = 1                       "1429621
                      AND bwart = 122                     "1429621
                      AND ebeln <> ''.                    "1429621
    IF sy-subrc = 0.                                      "1429621
      SELECT SINGLE * FROM ekbe into ls_ekbe              "1429621
                      WHERE ebeln = ls_mseg-ebeln         "1429621
                        AND vgabe = 1                     "1429621
                        AND gjahr = year                  "1429621
                        AND belnr = mblnr                 "1429621
                        AND buzei = 1                     "1429621
                        AND bwart = 122.                  "1429621
      IF sy-subrc = 0.                                    "1429621
        mblnr = ls_ekbe-lfbnr.                            "1429621
      ENDIF.                                              "1429621
    ENDIF.                                                "1429621
    REFRESH it_mebel.
    SELECT * FROM m_mebel INTO TABLE it_mebel
                          WHERE  lfgja = year
                          AND    lfbnr = mblnr
                          AND    lfpos = zeile
                          AND    vgabe = '1' . "find  GR

    DESCRIBE TABLE it_mebel LINES gv_tfill.

    PERFORM relevant_documents USING meins
                                     matnr
                                     company_code
                                     field_symbol
                                     sy-tfill
                                     mat_quant        " note 1137434
                                     erfmg            " note 1137434
                                     erfme            " note 1137434
                                     caller.       " note 1043677

  ENDIF.

  IF found = ' '.

*----------------------------------------------------------------------*
* check: MD -> DEL -> BI -> NF ?
*        1) for the goods issue read all preceeding deliveries of the
*           same level (both per line)
*        2) for the deliveries read all following billing document
*           (per line)
*        3) per billing document line: find corrsponding NF line if
*           existing
*----------------------------------------------------------------------*

  CLEAR reported_nfs.                  "table of NF lines that have been
*    REFRESH reported_nfs.              "reported - will be rebuilt per
    "SD document - to avoid duplications
    CLEAR returns_del.

* determine preceeding delivery of the same level

    CLEAR int_vbfa. REFRESH int_vbfa.
    CLEAR ls_lips.                                 " note 948019
****************************** boi 773139*****************************

* read entry for SD document flow with delivery number from MKPF-XBLNR
* and material document number & item

        SELECT * FROM vbfa INTO TABLE int_vbfa
                       WHERE vbelv   =  xblnr
                         AND vbeln   =  mblnr
                         AND posnn   =  zeile .


* read data into buffer for access of LIPS
        READ TABLE int_vbfa WITH KEY vbelv = xblnr
                                     vbeln = mblnr
                                     posnn = zeile.


        IF sy-subrc IS INITIAL.
* get sales order number for delivery
* for SD documents the delivery note number is stored in XBLNR
* derrived from MKPF
         SELECT SINGLE * FROM  lips INTO ls_lips
                          WHERE  vbeln  = xblnr
                          AND    posnr  = int_vbfa-posnv.
           IF sy-subrc IS INITIAL.

* credit memo
              SELECT * FROM vbfa APPENDING TABLE int_vbfa
                           WHERE vbelv   =  ls_lips-vgbel
                             AND posnv   =  ls_lips-vgpos
                             AND vbeln   =  mblnr
                             AND posnn   =  zeile .

            ENDIF.
         ENDIF.
* eoi 773139
* bod 773139

*    SELECT * FROM vbfa INTO TABLE int_vbfa
*                       WHERE vbeln   =  mblnr
*                         AND posnn   =  zeile            " note 658202
*                         ORDER BY PRIMARY KEY.           " note 658202
*                         AND posnn   =  zeile.           " note  658202

****************************** eod 773139*******************************
    gv_batchsplit = ' '.                                      " note 1162428
    SORT int_vbfa.                                         " note 658202
    LOOP AT int_vbfa.

* UECHA is filled with the value of the "main" delivery item when batch
* split is executed in the delivery, if it's initial take original posnr

      IF ls_lips-uecha is initial.                    " BOI note 0948019
        lv_posnr = int_vbfa-posnv.
      ELSE.
        lv_posnr = ls_lips-uecha.
        gv_batchsplit = 'X'.                             " note 1162428
      ENDIF.                                          " EOI note 0948019

      CHECK ( int_vbfa-vbtyp_v = 'J' OR
              int_vbfa-vbtyp_v = 'T' )
        AND int_vbfa-matnr   =  matnr
        AND int_vbfa-stufe   = '0'.

      CHECK int_vbfa-rfmng = mat_quant.                  " note 519991

      IF int_vbfa-vbtyp_v = 'T'.
        returns_del = 'X'.
      ENDIF.

*   determine following billing documents

      SELECT * FROM *vbfa WHERE vbelv   = int_vbfa-vbelv  " note 0948019
*                            AND posnv   = int_vbfa-posnv " note 0948019
                            AND posnv   = lv_posnr
                            AND ( vbtyp_n = 'M' OR          "M = invoice
                                  vbtyp_n = 'O' OR          "credit memo
                                   vbtyp_n = 'P' ).          "debit memo
        "debit memos will only be considered in export process (step2)
        "other debit memos will be excluded below

*     credit memos will only be considered, if connected to a return
        IF *vbfa-vbtyp_n = 'O'.
          CHECK int_vbfa-vbtyp_v = 'T'.
        ENDIF.

        PERFORM search_nf USING matnr
                                werks
                                bwtar
                                meins
                                mat_quant        " note 1137434
                                erfmg            " note 1137434
                                erfme            " note 1137434
                                field_symbol.

        IF NOT found is INITIAL.                          " note 1043677
          EXIT.                                           " note 1043677
        ENDIF.                                            " note 1043677
      ENDSELECT.                       "*vbfa
      EXIT.
    ENDLOOP.                           "int_vbfa

*   no NF found and document is a credit memo in a return process
    IF found IS INITIAL AND returns_del = 'X'.
*     billing might be posted before delivery
*      MD -> SO -> BI -> NF - repeat search with other document types

*     determine preceeding order
      LOOP AT int_vbfa   WHERE vbeln   =  mblnr
                           AND posnn   =  zeile.

        CHECK ( int_vbfa-vbtyp_v = 'H' ) "H = returns
          AND int_vbfa-matnr   =  matnr
          AND int_vbfa-stufe   = '1'.

* Note 519991 - problem:  VBFA has no year; therefore it can be possible
* to have incorrect document (e.g. mat. doc. from other year)
* solution: attempt of a unique identification by adopting quantities
        CHECK int_vbfa-rfmng = mat_quant.            " note 519991

*       determine following billing documents

        SELECT * FROM *vbfa WHERE vbelv   = int_vbfa-vbelv
                            AND posnv   = int_vbfa-posnv
                            AND vbtyp_n = 'O'.           "credit memo

          PERFORM search_nf USING matnr
                                  werks
                                  bwtar
                                  meins
                                  mat_quant        " note 1137434
                                  erfmg            " note 1137434
                                  erfme            " note 1137434
                                  field_symbol.

          IF NOT found is INITIAL.                        " note 1043677
            EXIT.                                         " note 1043677
          ENDIF.                                          " note 1043677
        ENDSELECT.                     "*vbfa
        EXIT.
      ENDLOOP.                         "int_vbfa
    ENDIF.     "end of extra search for credit memos, if no NF found yet

  ENDIF.                               "end of search for SD documents


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ONLY_EXPORT_DEBIT_MEMOS
*&---------------------------------------------------------------------*
FORM only_export_debit_memos CHANGING return_code.
* This form will exclude all debit memos, which are not part of the
* export process. The export process works in two steps:
* step1: billing1, which is cancelled later
*        NF1 (not cancelled)
* step2: debit memo
*        NF2
* Thus: debit memo is part of the export process, if billing1 is
*       cancelled, while NF1 is not

  return_code = 4.

* debit memo -> NF2 item (to find documents of step1)
  SELECT SINGLE * FROM  j_1bnflin WHERE reftyp = 'BI'
                                    AND refkey = vbrp-vbeln
                                    AND refitm = vbrp-posnr.
  CHECK sy-subrc = 0.

* read in NF2 header to get preceeding NF (NF2 -> NF1)
  SELECT SINGLE * FROM j_1bnfdoc WHERE docnum = j_1bnflin-docnum.
  CHECK sy-subrc = 0.

* read header of NF1
  SELECT SINGLE * FROM j_1bnfdoc
                   WHERE docnum = j_1bnfdoc-docref.
  CHECK sy-subrc = 0.

*********  check NF1 not cancelled
  CHECK j_1bnfdoc-cancel IS INITIAL.


* read in NF1 to find billing1
  SELECT SINGLE * FROM j_1bnflin WHERE docnum = j_1bnfdoc-docnum.
  CHECK sy-subrc = 0.

* read billing1
  SELECT SINGLE * FROM vbrk WHERE vbeln = j_1bnflin-refkey.
  CHECK sy-subrc = 0.

********* check billing1 is cancelled
  CHECK vbrk-fksto EQ 'X'.

  CLEAR return_code.                   "both checks have been successful

ENDFORM.                               " ONLY_EXPORT_DEBIT_MEMOS
*&---------------------------------------------------------------------*
*&      Form  SEARCH_NF
*&---------------------------------------------------------------------*
FORM search_nf USING f_matnr
                     f_werks
                     f_bwtar
                     f_meins
                     f_mat_quant        " note 1137434
                     f_erfmg            " note 1137434
                     f_erfme            " note 1137434
                     f_field_symbol.


  DATA: export_return_code LIKE sy-subrc.
*     read billing document (header)

  SELECT SINGLE * FROM vbrk WHERE vbeln = *vbfa-vbeln.

  IF *vbfa-vbtyp_n = 'P'.
    CHECK vbrk-land1 NE address1-country. "only export debit memos
    CHECK vbrk-fksto IS INITIAL.       "that are not cancelled
  ENDIF.

* exclude cancelling documents
  CHECK vbrk-vbtyp NE 'N'.             "cancel doc. of billing document
  CHECK vbrk-vbtyp NE 'S'.             "cancel doc. of credit memo

*     exclude cancelled documents, except for export (step1)
*     (for export the document is later excluded, if NF also cancelled)
  IF vbrk-land1 = address1-country.
    CHECK vbrk-fksto IS INITIAL.       "cancelled document
  ENDIF.

*     read billing document (lines)

  SELECT SINGLE * FROM vbrp WHERE vbeln = *vbfa-vbeln
                              AND posnr = *vbfa-posnn
                              AND matnr = f_matnr
                              AND werks = f_werks
                              AND bwtar = f_bwtar.
  IF *vbfa-vbtyp_n = 'P'.
*      exclude debit memos, if they are not part of the export process
    PERFORM only_export_debit_memos CHANGING export_return_code.
    CHECK export_return_code IS INITIAL.
  ENDIF.

  PERFORM nf_find USING vbrp-vbeln
                        const_reftyp-sd
                        vbrp-posnr
                        f_meins
                        f_matnr
                        f_mat_quant        " note 1137434
                        f_erfmg            " note 1137434
                        f_erfme            " note 1137434
                        f_field_symbol.

  IF found = ' '.

*     check if there is a higher level item in the bill of material
*     structures - if yes, then the NF refers to this item

    IF NOT vbrp-uepos IS INITIAL.
      PERFORM nf_find USING vbrp-vbeln
                            const_reftyp-sd
                            vbrp-uepos
                            f_meins
                            f_matnr
                            f_mat_quant        " note 1137434
                            f_erfmg            " note 1137434
                            f_erfme            " note 1137434
                            f_field_symbol.
    ENDIF.                             "if not vbrp-uepos is initial

    IF found = ' '.

* check if this is a subposition to a batch material
* if yes, then the NF refers to the main item

      IF NOT vbrp-uecha IS INITIAL.
        PERFORM nf_find USING vbrp-vbeln
                              const_reftyp-sd
                              vbrp-uecha
                              f_meins
                              f_matnr
                              f_mat_quant        " note 1137434
                              f_erfmg            " note 1137434
                              f_erfme            " note 1137434
                              f_field_symbol.
      ENDIF.                           "if not vbrp-uecha is initial
    ENDIF.                             "not found for BOM
  ENDIF.                               "first found = 'X'
  IF vbrk-land1 = address1-country.
    "export: several following documents possible
                                       "otherwise: exit
    EXIT.
  ENDIF.
ENDFORM.                               " SEARCH_NF

*---------------------------------------------------------------------*
*       FORM relevant_documents                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  XBELNR                                                        *
*  -->  XGJAHR                                                        *
*  -->  XBUZEI                                                        *
*  -->  XMEINS                                                        *
*  -->  XMATNR                                                        *
*  -->  XCOMPANY_CODE                                                 *
*  -->  XFIELD_SYMBOL                                                 *
*  -->  XSY_TFILL                                                     *
*---------------------------------------------------------------------*
FORM relevant_documents USING
                              xmeins
                              xmatnr
                              xcompany_code
                              xfield_symbol
                              xsy_tfill
                              xmat_quant        " note 1137434
                              xerfmg            " note 1137434
                              xerfme            " note 1137434
                              caller.              " note 1043677

  DATA: refkey     LIKE j_1bnflin-refkey, "unique identification of BI,
                                       "IV, LIV or MD document - to
                                       "determine NF
       refitm     LIKE j_1bnflin-refitm.  "Reference item to source doc

  LOOP AT it_mebel.
    CHECK it_mebel-ok = space.
    it_mebel-ok = 'X'.
    MODIFY it_mebel.

    IF gv_tfill > 1.
      MOVE-CORRESPONDING it_mebel TO it_refdoc.
      APPEND it_refdoc.
    ENDIF.

*    if it_mebel-lfbnr <> it_mebel-belnr.   "not original Mat.document
*      select single * from mseg into *mseg
*                      where mblnr = it_mebel-belnr
*                      and   mjahr = it_mebel-gjahr
*                      and   zeile = it_mebel-buzei.
*      if sy-subrc is initial.
*        move: *mseg-sobkz to out-sobkz,
*              *mseg-bwart to out-bwart.
*        PERFORM DETERMINE_DIRECTION USING *mseg-shkzg.
*      endif.
*    endif.
    DATA: mebel_count LIKE sy-index.
    CLEAR mebel_count.
    SELECT * FROM m_mebel
                        WHERE  lfgja = it_mebel-lfgja
                        AND    lfbnr = it_mebel-lfbnr
                        AND    lfpos = it_mebel-lfpos
                        AND    vgabe = '2' . "find  IV     Check 3!!
      ADD 1 TO mebel_count.

      IF m_mebel-zekkn <> '00'.
        CHECK m_mebel-zekkn = '01'.
      ENDIF.

      IF mebel_count > 1.
        LOOP AT it_mebel FROM mebel_count.
          it_mebel-ok = 'X'.
          MODIFY it_mebel INDEX sy-tabix.

*          if it_mebel-shkzg = m_mebel-shkzg.

       IF it_mebel-lfbnr <> it_mebel-belnr.   "not original Mat.document
            SELECT SINGLE * FROM mseg INTO *mseg
                            WHERE mblnr = it_mebel-belnr
                            AND   mjahr = it_mebel-gjahr
                            AND   zeile = it_mebel-buzei.
            IF sy-subrc IS INITIAL.
              MOVE: *mseg-sobkz TO out-sobkz,
                    *mseg-bwart TO out-bwart.
* ATTENTATION: no redetermination of direction!               nt. 684417
*              PERFORM determine_direction USING *mseg-shkzg. nt. 684417
            ENDIF.
            IF sy-tfill > 1.
              MOVE-CORRESPONDING it_mebel TO it_refdoc.
              APPEND it_refdoc.
            ENDIF.

*            endif.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF xsy_tfill > 1.
        CLEAR found.
      ENDIF.


      SELECT SINGLE * FROM rbkp WHERE belnr = m_mebel-belnr
                                AND   gjahr = m_mebel-gjahr.
      IF sy-subrc is INITIAL AND caller EQ 'MODELO_3'.    " note 1043677
        CHECK rbkp-budat BETWEEN datum_low AND datum_high." note 1043677
      ENDIF.                                              " note 1043677

      IF rbkp-stblg IS INITIAL.
        CONCATENATE m_mebel-belnr m_mebel-gjahr INTO refkey.
        PERFORM nf_find USING refkey
                              const_reftyp-li
                              m_mebel-buzei
                              xmeins
                              xmatnr
                              xmat_quant        " note 1137434
                              xerfmg            " note 1137434
                              xerfme            " note 1137434
                              xfield_symbol.
      ENDIF.
      IF found = ' '.
        SELECT SINGLE * FROM bkpf WHERE bukrs = xcompany_code
                                  AND   belnr = m_mebel-belnr
                                  AND   gjahr = m_mebel-gjahr.

        IF  bkpf-stblg IS INITIAL.
          MOVE xcompany_code TO refkey(4).
          MOVE m_mebel-belnr TO refkey+4(10).
          MOVE m_mebel-gjahr TO refkey+14(4).
          PERFORM nf_find USING refkey
                                const_reftyp-fi
                                m_mebel-buzei
                                xmeins
                                xmatnr
                                xmat_quant        " note 1137434
                                xerfmg            " note 1137434
                                xerfme            " note 1137434
                                xfield_symbol.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDLOOP.

ENDFORM.
