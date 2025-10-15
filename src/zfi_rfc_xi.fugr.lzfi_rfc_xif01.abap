*----------------------------------------------------------------------*
***INCLUDE LZFI_RFC_XIF01 .
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM fill_vendor                                              *
*---------------------------------------------------------------------*
FORM f_fill_vendor.

  DATA : vl_len   TYPE i,
         vl_trtyp TYPE zfit0029-trtyp,
         wl_lfb1  TYPE lfb1.

  CLEAR wa_payable.
  wa_payable-itemno_acc    = vg_item.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_item-hkont
    IMPORTING
      output = wa_payable-vendor_no.

  wa_payable-ref_key_1     = wa_item-xref1.
  wa_payable-ref_key_2     = wa_item-xref2.
  wa_payable-ref_key_3     = wa_item-xref3.
  wa_payable-bline_date    = wa_item-zfbdt.
  wa_payable-pymt_meth     = wa_item-zlsch.
  wa_payable-pmnt_block    = wa_item-zlspr.
  wa_payable-alloc_nmbr    = wa_item-zuonr.
  wa_payable-item_text     = wa_item-sgtxt.
  wa_payable-businessplace = wa_item-bupla.
  wa_payable-sp_gl_ind     = wa_item-umskz.
  wa_payable-bus_area      = wa_item-gsber.
  wa_payable-partner_bk    = wa_item-bvtyp.
  wa_payable-bank_id       = wa_item-hbkid.
  wa_payable-po_sub_no     = wa_item-esrnr.
  wa_payable-po_ref_no     = wa_item-esrre.
  wa_payable-pmnttrms      = wa_item-zterm.
  IF wa_item-hkont2 IS NOT INITIAL.
    wa_payable-gl_account       = wa_item-hkont2.
  ENDIF.
  APPEND wa_payable TO it_payable.

  IF wa_item-kidno IS NOT INITIAL.
    CLEAR  wa_extension1.
    wa_extension1-field1 = 'KIDNO'.
    wa_extension1-field2 = vg_item.
    wa_extension1-field3 = wa_item-kidno.
    APPEND wa_extension1 TO it_extension1.
  ENDIF.
  "Preenche o tipo de movimento
  IF wa_header-bktxt(5) EQ 'COMEX'.
    SELECT SINGLE *
      FROM lfb1
      INTO wl_lfb1
      WHERE lifnr = wa_payable-vendor_no
      AND   bukrs = wa_header-bukrs.

    IF sy-subrc = 0.
      SELECT SINGLE bewar
      INTO vl_trtyp
       FROM zfit0089
      WHERE hkont = wl_lfb1-akont.

      IF sy-subrc = 0.
        vl_len =  strlen( vg_item ) - 3.
        CONCATENATE vg_item+vl_len(3) vl_trtyp INTO wa_extension1-field1.
        APPEND wa_extension1 TO it_extension1.
      ENDIF.

    ENDIF.


  ELSE.

    IF wa_header-blart = 'XR' .

      CLEAR : wa_extension1.

      SELECT SINGLE trtyp
        INTO vl_trtyp
        FROM zfit0029
       WHERE xref3 = wa_item-xref3
         AND bschl = wa_item-bschl.

      vl_len =  strlen( vg_item ) - 3.

      CONCATENATE vg_item+vl_len(3) vl_trtyp INTO wa_extension1-field1.

      APPEND wa_extension1 TO it_extension1.

    ENDIF.
    "IF WA_HEADER-BLART = 'VC' .
    IF wa_item-bewar IS NOT INITIAL.
      CLEAR : wa_extension1.
      vl_trtyp = wa_item-bewar.
      vl_len =  strlen( vg_item ) - 3.
      CONCATENATE vg_item+vl_len(3) vl_trtyp INTO wa_extension1-field1.
      APPEND wa_extension1 TO it_extension1.
    ENDIF.

    IF wa_item-vbund IS NOT INITIAL.
      CLEAR  wa_extension1.
      wa_extension1-field1 = 'SOCPARC'.
      wa_extension1-field2 = vg_item.
      wa_extension1-field3 = wa_item-vbund.
      APPEND wa_extension1 TO it_extension1.
    ENDIF.

    IF wa_item-vbeln IS NOT INITIAL.
      CLEAR  wa_extension1.
      wa_extension1-field1 = 'ORDVENDA'.
      wa_extension1-field2 = vg_item.
      wa_extension1-field3 = wa_item-vbeln.
      APPEND wa_extension1 TO it_extension1.
    ENDIF.

    IF wa_header-obj_key+0(7) = 'ZFI0111' OR wa_item-rldnr IS NOT INITIAL..
      wa_extension1-field1 = 'LEDGER'.
      wa_extension1-field2 = vg_item.
      wa_extension1-field3 = '50'.
      IF wa_item-rldnr IS NOT INITIAL.
        wa_extension1-field3 = wa_item-rldnr.
      ENDIF.
      APPEND wa_extension1 TO it_extension1.
    ENDIF.


*    IF WA_ITEM-HKONT2 IS NOT INITIAL.
*      CLEAR  WA_EXTENSION1.
*      WA_EXTENSION1-FIELD1 = 'CONTAR'.
*      WA_EXTENSION1-FIELD2 = VG_ITEM.
*      WA_EXTENSION1-FIELD3 = WA_ITEM-HKONT2.
*      APPEND WA_EXTENSION1 TO IT_EXTENSION1.
*    ENDIF.

  ENDIF.


ENDFORM.                    " f_fill_vendor
*&---------------------------------------------------------------------*
*&      Form  f_fill_customer
*&---------------------------------------------------------------------*
FORM f_fill_customer.
  DATA : vl_len   TYPE i,
         vl_trtyp TYPE zfit0029-trtyp,
         wl_knb1  TYPE knb1.

  CLEAR wa_receivable.
  wa_receivable-itemno_acc    = vg_item.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_item-hkont
    IMPORTING
      output = wa_receivable-customer.

  wa_receivable-ref_key_1     = wa_item-xref1.
  wa_receivable-ref_key_2     = wa_item-xref2.
  wa_receivable-ref_key_3     = wa_item-xref3.
  wa_receivable-bline_date    = wa_item-zfbdt.
  wa_receivable-pymt_meth     = wa_item-zlsch.
  wa_receivable-pmnt_block    = wa_item-zlspr.
  wa_receivable-alloc_nmbr    = wa_item-zuonr.
  wa_receivable-item_text     = wa_item-sgtxt.
  wa_receivable-paymt_ref     = wa_item-kidno.
  wa_receivable-businessplace = wa_item-bupla.
  wa_receivable-sp_gl_ind     = wa_item-umskz.
  wa_receivable-bus_area      = wa_item-gsber.
  wa_receivable-bank_id       = wa_item-hbkid.
  wa_receivable-pmnttrms      = wa_item-zterm.

  IF wa_item-hkont2 IS NOT INITIAL.
    wa_receivable-gl_account       = wa_item-hkont2.
  ENDIF.
  APPEND wa_receivable TO it_receivable.

  "Preenche o tipo de movimento
  IF wa_header-bktxt(5) EQ 'COMEX'.
    SELECT SINGLE *
      FROM knb1
      INTO wl_knb1
      WHERE kunnr = wa_receivable-customer
      AND   bukrs = wa_header-bukrs.

    IF sy-subrc = 0.
      SELECT SINGLE bewar
      INTO vl_trtyp
       FROM zfit0089
      WHERE hkont = wl_knb1-akont.

      IF sy-subrc = 0.
        vl_len =  strlen( vg_item ) - 3.
        CONCATENATE vg_item+vl_len(3) vl_trtyp INTO wa_extension1-field1.
        APPEND wa_extension1 TO it_extension1.
      ENDIF.

    ENDIF.
  ELSE.
    IF wa_item-bewar IS NOT INITIAL.
      CLEAR : wa_extension1.
      vl_trtyp = wa_item-bewar.
      vl_len =  strlen( vg_item ) - 3.
      CONCATENATE vg_item+vl_len(3) vl_trtyp INTO wa_extension1-field1.
      APPEND wa_extension1 TO it_extension1.
    ENDIF.
  ENDIF.

  IF wa_item-vbund IS NOT INITIAL.
    CLEAR  wa_extension1.
    wa_extension1-field1 = 'SOCPARC'.
    wa_extension1-field2 = vg_item.
    wa_extension1-field3 = wa_item-vbund.
    APPEND wa_extension1 TO it_extension1.
  ENDIF.

  IF wa_header-obj_key+0(7) = 'ZFI0111' OR wa_item-rldnr IS NOT INITIAL.
    wa_extension1-field1 = 'LEDGER'.
    wa_extension1-field2 = vg_item.
    wa_extension1-field3 = '50'.
    IF wa_item-rldnr IS NOT INITIAL.
      wa_extension1-field3 = wa_item-rldnr.
    ENDIF.
    APPEND wa_extension1 TO it_extension1.
  ENDIF.

*  IF WA_ITEM-HKONT2 IS NOT INITIAL.
*    CLEAR  WA_EXTENSION1.
*    WA_EXTENSION1-FIELD1 = 'CONTAR'.
*    WA_EXTENSION1-FIELD2 = VG_ITEM.
*    WA_EXTENSION1-FIELD3 = WA_ITEM-HKONT2.
*    APPEND WA_EXTENSION1 TO IT_EXTENSION1.
*  ENDIF.


ENDFORM.                    " f_fill_customer

*&---------------------------------------------------------------------*
*&      Form  f_fill_account
*&---------------------------------------------------------------------*
FORM f_fill_account.
  DATA : it_setleaf      TYPE TABLE OF setleaf,
         it_zib_contabil TYPE TABLE OF zib_contabil,
         vobj_key        TYPE zib_contabil-obj_key,
         vtam            TYPE i.
  TYPES:
    BEGIN OF ty_zib,
      bschl TYPE zib_contabil-bschl,
      hkont TYPE zib_contabil-hkont,
      koart TYPE tbsl-koart,
    END OF ty_zib.

  DATA : wa_zib_contabil  TYPE zib_contabil,
         wa_zib_contabil2 TYPE ty_zib.

  DATA : vl_hkont         TYPE zib_contabil-hkont,
         vl_sociedade     TYPE lfa1-vbund,
         vl_sociedade_aux TYPE lfa1-vbund,
         vl_bschl         TYPE zib_contabil-bschl,
         vl_anln1         TYPE zib_contabil-anln1,
         vl_anln2         TYPE zib_contabil-anln2,
         wl_tbsl          TYPE tbsl,
         vl_kunnr         TYPE kna1-kunnr,
         vl_lifnr         TYPE lfa1-lifnr,
         vl_xref1         TYPE xref1,
         wl_bewar         TYPE zfit0030-bewar.

  CLEAR wa_accountgl.
  CLEAR it_criteria.
  IF ( wa_item-matnr IS NOT INITIAL AND wa_header-bktxt(5) NE 'COMEX' ).  "IR201903930
    it_criteria-itemno_acc    = vg_item.
    it_criteria-fieldname     = 'ARTNR'.
*---> 09/06/2023 - Migração S4 - JS
*    it_criteria-character     = wa_item-matnr.
    it_criteria-character = CONV #( wa_item-matnr ).
*<--- 09/06/2023 - Migração S4 - JS

    APPEND it_criteria.
  ELSEIF ( wa_item-matnr IS NOT INITIAL AND wa_header-bktxt(5) EQ 'COMEX' ) AND wa_header-bukrs EQ '0100'. "IR201903930
    wa_item-matnr_fi = wa_item-matnr.
    CLEAR wa_item-matnr.
  ENDIF.

  wa_accountgl-itemno_acc     = vg_item.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_item-hkont
    IMPORTING
      output = wa_accountgl-gl_account.

  wa_accountgl-ref_key_1      = wa_item-xref1.
  wa_accountgl-ref_key_2      = wa_item-xref2.
  wa_accountgl-ref_key_3      = wa_item-xref3.
  wa_accountgl-acct_type      = wa_tbsl-koart.
  wa_accountgl-item_text      = wa_item-sgtxt.
  wa_accountgl-bus_area       = wa_item-gsber.
  wa_accountgl-alloc_nmbr     = wa_item-zuonr.
  wa_accountgl-tax_code       = wa_item-tax_code.
  wa_accountgl-cs_trans_t     = wa_item-bewar. "GL-230914-Melhorias Lactos Manuais-CH.127989-ALRS
  wa_accountgl-quantity       = wa_item-quantity.
  wa_accountgl-base_uom       = wa_item-base_uom.
  wa_accountgl-material       = wa_item-matnr_fi.
  wa_accountgl-plant          = wa_item-werks.

  IF wa_item-bupla IS NOT INITIAL.
    CLEAR  wa_extension1.
    wa_extension1-field1 = 'BUPLA'.
    wa_extension1-field2 = vg_item.
    wa_extension1-field3 = wa_item-bupla.
    APPEND wa_extension1 TO it_extension1.
  ENDIF.

  IF wa_header-obj_key+0(7) = 'ZFI0111' OR wa_item-rldnr IS NOT INITIAL.
    wa_extension1-field1 = 'LEDGER'.
    wa_extension1-field2 = vg_item.
    wa_extension1-field3 = '50'.
    IF wa_item-rldnr IS NOT INITIAL.
      wa_extension1-field3 = wa_item-rldnr.
    ENDIF.
    APPEND wa_extension1 TO it_extension1.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_item-kostl
    IMPORTING
      output = wa_accountgl-costcenter.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_item-aufnr
    IMPORTING
      output = wa_accountgl-orderid.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_item-vornr
    IMPORTING
      output = wa_accountgl-activity.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_item-prctr
    IMPORTING
      output = wa_accountgl-profit_ctr.

  CLEAR vl_xref1.
  vl_xref1 = |%{ wa_item-xref1 }%|.

  IF 'XR' CS wa_header-blart AND wa_header-bktxt CS 'XRT'.
    CLEAR vl_sociedade.

    IF wa_item-bschl EQ '40' OR wa_item-bschl EQ '50'."
      READ TABLE it_header INTO DATA(wheader) WITH KEY obj_key = wa_header-obj_key.
      vtam      = strlen( wheader-obj_key ) - 3.
      IF vtam GT 1.
        vobj_key  = wheader-obj_key+0(vtam).

        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE it_zib_contabil
          FROM zib_contabil
         WHERE zib_contabil~obj_key EQ vobj_key
         AND   zib_contabil~xref1 LIKE vl_xref1.

        LOOP AT it_zib_contabil INTO wa_zib_contabil.

          vl_kunnr = CONV #( wa_zib_contabil-hkont ).
          vl_kunnr = |{ vl_kunnr ALPHA = IN }|.
          vl_lifnr = CONV #( wa_zib_contabil-hkont ).
          vl_lifnr = |{ vl_lifnr ALPHA = IN }|.

          SELECT SINGLE *
            FROM tbsl
            INTO @DATA(w_tbsl)
            WHERE bschl = @wa_zib_contabil-bschl.

          IF w_tbsl-koart = 'D'.
            SELECT SINGLE vbund
              INTO vl_sociedade_aux
              FROM kna1
             WHERE kunnr = vl_kunnr. "wa_zib_contabil-hkont.

            IF vl_sociedade_aux IS NOT INITIAL.
              vl_sociedade := vl_sociedade_aux.
            ENDIF.

          ELSEIF w_tbsl-koart = 'K'.

            SELECT SINGLE vbund
              INTO vl_sociedade_aux
              FROM lfa1
             WHERE lifnr = vl_lifnr. "wa_zib_contabil-hkont.

            IF vl_sociedade_aux IS NOT INITIAL.
              vl_sociedade := vl_sociedade_aux.
            ENDIF.

          ENDIF.

        ENDLOOP.

        REFRESH it_zib_contabil.

        wa_accountgl-trade_id = vl_sociedade.

        CLEAR: vl_kunnr, vl_lifnr.

      ENDIF.
    ENDIF.

  ENDIF.

  IF 'SI' CS wa_header-blart .

    CLEAR vl_sociedade.

    IF wa_item-bschl EQ '40' OR wa_item-bschl EQ '50'."   SY-SUBRC IS INITIAL.

      vl_hkont = wa_item-hkont.

      PERFORM zlpad  USING '0' 10
         CHANGING vl_hkont.

      SELECT *
        FROM setleaf
        INTO TABLE it_setleaf
       WHERE setname EQ 'CONTAS_EC-CS'
         AND valfrom EQ vl_hkont.

      IF sy-subrc IS INITIAL.

        SELECT *
          INTO TABLE it_zib_contabil
          FROM zib_contabil
         WHERE bschl IN ('01', '11', '21','31')
           AND obj_key EQ wa_header-obj_key.

        LOOP AT it_zib_contabil INTO wa_zib_contabil.

          IF wa_zib_contabil-bschl EQ '01' OR wa_zib_contabil-bschl EQ '11'.

            SELECT SINGLE vbund
              INTO vl_sociedade_aux
              FROM kna1
             WHERE kunnr = wa_zib_contabil-hkont
               AND ktokd IN ('ZCIC','ZCEX', 'SCIC', 'SCEX', 'HCIC', 'HCEX').
            "AND ktokd IN ('ZCIC','ZCEX','ZCPF').
            IF vl_sociedade_aux IS NOT INITIAL.
              vl_sociedade := vl_sociedade_aux.
            ENDIF.

          ELSEIF wa_zib_contabil-bschl EQ '21' OR wa_zib_contabil-bschl EQ '31'.

            SELECT SINGLE vbund
              INTO vl_sociedade_aux
              FROM lfa1
             WHERE ktokk IN ('ZFIC','ZFEX', 'HFEX','HFIC', 'PFEX','PFIC','SFIC','ZFNJ')
               AND lifnr = wa_zib_contabil-hkont.

            IF vl_sociedade_aux IS NOT INITIAL.
              vl_sociedade := vl_sociedade_aux.
            ENDIF.

          ENDIF.

        ENDLOOP.

        REFRESH it_zib_contabil.

        wa_accountgl-trade_id = vl_sociedade.

      ENDIF.

    ENDIF.

  ENDIF.
  "alrs
  IF 'VC_WR' CS wa_header-blart AND  wa_item-vbund IS INITIAL .  " incluido VC em 27.06.2013 / VBUND preenchido 07.05.2018

    CLEAR vl_sociedade.

    SELECT *
      INTO TABLE it_zib_contabil
      FROM zib_contabil
     WHERE bschl  IN ('40', '50')
       AND obj_key EQ wa_header-obj_key.

    LOOP AT it_zib_contabil INTO wa_zib_contabil.

      vl_hkont = wa_zib_contabil-hkont.

      PERFORM zlpad  USING '0' 10
         CHANGING vl_hkont.

      SELECT *
        FROM setleaf
        INTO TABLE it_setleaf
       WHERE setname EQ 'CONTAS_EC-CS'
         AND valfrom EQ vl_hkont.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE zib_contabil~bschl zib_contabil~hkont
          INTO wa_zib_contabil2
          FROM zib_contabil
          WHERE zib_contabil~obj_key EQ wa_header-obj_key
          AND bschl  NOT IN ('40', '50').

        SELECT SINGLE *
          FROM tbsl
          INTO wl_tbsl
          WHERE bschl = wa_zib_contabil2-bschl.

        IF wl_tbsl-koart = 'D'.

          SELECT SINGLE vbund
            INTO vl_sociedade
            FROM kna1
           WHERE kunnr = wa_zib_contabil2-hkont
             AND ktokd IN ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ', 'ZCPJ').

        ELSEIF wl_tbsl-koart = 'K'.

          SELECT SINGLE vbund
            INTO vl_sociedade
            FROM lfa1
           WHERE ktokk IN ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ', 'ZPRJ')
             AND lifnr = wa_zib_contabil2-hkont.

        ENDIF.
        IF vl_sociedade IS NOT INITIAL.
          wa_accountgl-trade_id = vl_sociedade.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF 'XR' CS wa_header-blart AND vobj_key IS NOT INITIAL AND  wa_item-vbund IS INITIAL .  " Bug 57961 - Sociedade Parceira XRT linha 40 e 50 01.06.2021

    CLEAR vl_sociedade.

    SELECT *
      INTO TABLE it_zib_contabil
      FROM zib_contabil
     WHERE bschl  IN ('40', '50')
       AND obj_key EQ vobj_key
       AND xref1 LIKE vl_xref1. "//Bug 77142 - Add Ref

    LOOP AT it_zib_contabil INTO wa_zib_contabil.

      vl_hkont = wa_zib_contabil-hkont.
      vl_hkont = |{ vl_hkont ALPHA = IN }|.

      SELECT *
        FROM setleaf
        INTO TABLE it_setleaf
       WHERE setname EQ 'CONTAS_EC-CS'
         AND valfrom EQ vl_hkont.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE zib_contabil~bschl zib_contabil~hkont
          INTO wa_zib_contabil2
          FROM zib_contabil
          WHERE zib_contabil~obj_key EQ vobj_key
          AND bschl  NOT IN ('40', '50')
          AND xref1 LIKE vl_xref1.

        IF ( sy-subrc = 0 ).
          vl_kunnr = CONV #( wa_zib_contabil2-hkont ).
          vl_kunnr = |{ vl_kunnr ALPHA = IN }|.
          vl_lifnr = CONV #( wa_zib_contabil2-hkont ).
          vl_lifnr = |{ vl_lifnr ALPHA = IN }|.
        ENDIF.

        SELECT SINGLE *
          FROM tbsl
          INTO wl_tbsl
          WHERE bschl = wa_zib_contabil2-bschl.

        IF wl_tbsl-koart = 'D'.

          SELECT SINGLE vbund
            INTO vl_sociedade
            FROM kna1
           WHERE kunnr = vl_kunnr "wa_zib_contabil2-hkont
             AND ktokd IN ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ', 'ZCPJ').

        ELSEIF wl_tbsl-koart = 'K'.

          SELECT SINGLE vbund
            INTO vl_sociedade
            FROM lfa1
           WHERE ktokk IN ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ', 'ZPRJ')
             AND lifnr = vl_lifnr. "wa_zib_contabil2-hkont.

        ENDIF.
        IF vl_sociedade IS NOT INITIAL.
          wa_accountgl-trade_id = vl_sociedade.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR: vl_kunnr, vl_lifnr.

  ENDIF.

  "alrs
  IF wa_item-vbund IS NOT INITIAL.
    wa_accountgl-trade_id = wa_item-vbund.
  ELSE.
    SELECT SINGLE *
     INTO wa_zib_contabil
     FROM zib_contabil
    WHERE obj_key EQ wa_header-obj_key
    AND seqitem = wa_accountgl-itemno_acc.
    IF  sy-subrc = 0.
      IF wa_zib_contabil-vbund IS NOT INITIAL.
        wa_accountgl-trade_id = wa_zib_contabil-vbund.
      ENDIF.
    ENDIF.
  ENDIF.


  IF 'XS' CS wa_header-blart .
    SELECT SINGLE *
      INTO wa_zib_contabil
      FROM zib_contabil
     WHERE obj_key EQ wa_header-obj_key
     AND seqitem = wa_accountgl-itemno_acc.

    IF sy-subrc = 0.
      vl_hkont = wa_zib_contabil-hkont.
      PERFORM zlpad  USING '0' 10
         CHANGING vl_hkont.

      SELECT *
        FROM setleaf
        INTO TABLE it_setleaf
       WHERE setname EQ 'MAGGI_CTAS_TPMV_BPC'
         AND valfrom EQ vl_hkont.

      IF sy-subrc IS INITIAL.
        wa_accountgl-cs_trans_t = wa_zib_contabil-bewar .
      ENDIF.
    ENDIF.
  ENDIF.

  IF 'VC' CS wa_header-blart .
    SELECT SINGLE *
      INTO wa_zib_contabil
      FROM zib_contabil
     WHERE obj_key EQ wa_header-obj_key
     AND seqitem = wa_accountgl-itemno_acc.

    IF sy-subrc = 0.
      IF wa_zib_contabil-bewar IS NOT INITIAL.
        wa_accountgl-cs_trans_t = wa_zib_contabil-bewar .
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT SINGLE bschl anln1 anln2
    INTO (vl_bschl, vl_anln1, vl_anln2)
    FROM zib_contabil
   WHERE obj_key = wa_header-obj_key
     AND seqitem = wa_accountgl-itemno_acc.

  IF vl_bschl = '70' OR vl_bschl = '75' OR vl_anln1 IS NOT INITIAL.

    IF wa_item-bewar = '180' OR wa_item-bewar = '181' OR
       wa_item-bewar = '185' OR wa_item-bewar = '188' OR
       wa_item-bewar = '189'.

      CLEAR  wa_extension1.
      wa_extension1-field1 = 'VORGN'.
      wa_extension1-field2 = vg_item.
      wa_extension1-field3 = 'ANZA'.
      APPEND wa_extension1 TO it_extension1.

    ENDIF.

    CLEAR  wa_extension1.
    wa_extension1-field1 = 'ANBWA'.
    wa_extension1-field2 = vg_item.
    wa_extension1-field3 = wa_item-bewar.
    APPEND wa_extension1 TO it_extension1.



    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vl_anln1
      IMPORTING
        output = wa_accountgl-asset_no.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vl_anln2
      IMPORTING
        output = wa_accountgl-sub_number.
    "
    CLEAR wl_bewar.
    SELECT SINGLE bwakon
      FROM tabw
      INTO wl_bewar
      WHERE bwasl = wa_item-bewar.
    wa_accountgl-cs_trans_t = wl_bewar.

  ENDIF.

  wa_accountgl-value_date = wa_item-valut.

  APPEND wa_accountgl TO it_accountgl.

ENDFORM.                    " f_fill_account

*---------------------------------------------------------------------*
*      Form  Show_messages
*---------------------------------------------------------------------*
FORM show_messages TABLES p_return STRUCTURE zfie_return.

  LOOP AT it_bapiret INTO wa_bapiret.
    CLEAR wa_return.
    wa_return-seqlan = wa_header-seqlan.
    MOVE-CORRESPONDING wa_bapiret TO wa_return.
    APPEND wa_return TO p_return.
  ENDLOOP.

ENDFORM.                               " Show_messages
*&---------------------------------------------------------------------*
*&      Form  f_fill_structures
*&---------------------------------------------------------------------*
*       Preenche estruturas vazias p/ interface outbound de Clientes
*----------------------------------------------------------------------*
FORM f_fill_structures  TABLES   p_knbk STRUCTURE fknbk
                                 p_yknb STRUCTURE fknbk
                                 p_knvk STRUCTURE fknvk
                        CHANGING p_kna1 LIKE kna1
                                 p_knb1 LIKE knb1.

  DATA: vl_kunnr LIKE kna1-kunnr.

  IF ( NOT p_knbk[] IS INITIAL ).
    READ TABLE p_knbk INDEX 1.
    vl_kunnr = p_knbk-kunnr.
  ELSEIF ( NOT p_yknb[] IS INITIAL ).
    READ TABLE p_yknb INDEX 1.
    vl_kunnr = p_yknb-kunnr.
  ELSEIF ( NOT p_knvk[] IS INITIAL ).
    READ TABLE p_knvk INDEX 1.
    vl_kunnr = p_knvk-kunnr.
  ELSEIF ( NOT p_knb1   IS INITIAL ).
    vl_kunnr = p_knb1-kunnr.
  ELSEIF ( NOT p_kna1   IS INITIAL ).
    vl_kunnr = p_kna1-kunnr.
  ENDIF.

  IF ( p_knvk[] IS INITIAL ).
    SELECT * FROM knvk
             INTO TABLE p_knvk
            WHERE ( kunnr EQ vl_kunnr ).
  ENDIF.

  IF ( p_knb1 IS INITIAL ).
    SELECT * FROM knb1 UP TO 1 ROWS
             INTO p_knb1
            WHERE ( kunnr EQ vl_kunnr ).
    ENDSELECT.
  ENDIF.

  IF ( p_kna1 IS INITIAL ).
    SELECT SINGLE * FROM kna1
                    INTO p_kna1
                   WHERE ( kunnr EQ vl_kunnr ).
  ENDIF.

ENDFORM.                    " f_fill_structures
*&---------------------------------------------------------------------*
*&      Form  f_fill_structures_2
*&---------------------------------------------------------------------*
*       Preenche estruturas vazias p/ interface outbound de Fornecedores
*----------------------------------------------------------------------*
FORM f_fill_structures_2  TABLES   p_lfbk  STRUCTURE flfbk
                                   p_ylfb  STRUCTURE flfbk
                                   p_knvk  STRUCTURE fknvk
                          CHANGING p_lfa1  LIKE lfa1
                                   p_lfb1  LIKE lfb1.
  DATA: vl_lifnr LIKE lfa1-lifnr.

  IF ( NOT p_lfbk[] IS INITIAL ).
    READ TABLE p_lfbk INDEX 1.
    vl_lifnr = p_lfbk-lifnr.
  ELSEIF ( NOT p_ylfb[] IS INITIAL ).
    READ TABLE p_ylfb INDEX 1.
    vl_lifnr = p_ylfb-lifnr.
  ELSEIF ( NOT p_knvk[] IS INITIAL ).
    READ TABLE p_knvk INDEX 1.
    vl_lifnr = p_knvk-lifnr.
  ELSEIF ( NOT p_lfb1   IS INITIAL ).
    vl_lifnr = p_lfb1-lifnr.
  ELSEIF ( NOT p_lfa1   IS INITIAL ).
    vl_lifnr = p_lfa1-lifnr.
  ENDIF.

  IF ( p_knvk[] IS INITIAL ).
    SELECT * FROM knvk
             INTO TABLE p_knvk
            WHERE ( lifnr EQ vl_lifnr ).
  ENDIF.

  IF ( p_lfb1 IS INITIAL ).
    SELECT * FROM lfb1 UP TO 1 ROWS
             INTO p_lfb1
            WHERE ( lifnr EQ vl_lifnr ).
    ENDSELECT.
  ENDIF.

  IF ( p_lfa1 IS INITIAL ).
    SELECT SINGLE * FROM lfa1
                    INTO p_lfa1
                   WHERE ( lifnr EQ vl_lifnr ).
  ENDIF.


ENDFORM.                    " f_fill_structures_2
*&---------------------------------------------------------------------*
*&      Form  f_append_aux
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_append_aux  USING    p_tcurr  LIKE tcurr
                            VALUE(p_ativ).

  DATA: vl_data            TYPE c LENGTH 10.

  CLEAR wa_tcurr_aux.
  MOVE-CORRESPONDING p_tcurr TO wa_tcurr_aux.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
    EXPORTING
      input  = p_tcurr-gdatu
    IMPORTING
      output = vl_data.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external = vl_data
    IMPORTING
      date_internal = wa_tcurr_aux-gdatu.

  p_tcurr-ukurs = abs( p_tcurr-ukurs ).

  CALL FUNCTION 'CONVERSION_EXIT_EXCRT_OUTPUT'
    EXPORTING
      input  = p_tcurr-ukurs
    IMPORTING
      output = wa_tcurr_aux-ukurs.

  TRANSLATE wa_tcurr_aux-ukurs USING '. '.
  TRANSLATE wa_tcurr_aux-ukurs USING ',.'.
  CONDENSE  wa_tcurr_aux-ukurs NO-GAPS.


  wa_tcurr_aux-atividade      = p_ativ.
  wa_tcurr_aux-dt_atualizacao = sy-datum.
  wa_tcurr_aux-hr_atualizacao = sy-uzeit.
  APPEND wa_tcurr_aux TO it_tcurr_aux.

ENDFORM.                    " f_append_aux
*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
FORM f_bdc_field USING    VALUE(p_flag)
                          VALUE(p_fnam)
                          VALUE(p_fval).

  CLEAR wa_bdcdata.
  IF NOT p_flag IS INITIAL.
    wa_bdcdata-program  = p_fnam.
    wa_bdcdata-dynpro   = p_fval.
    wa_bdcdata-dynbegin = 'X'.
  ELSE.
    wa_bdcdata-fnam = p_fnam.
    wa_bdcdata-fval = p_fval.
  ENDIF.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    " f_bdc_field

*&---------------------------------------------------------------------*
*&      Form  ZLPAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_VALOR  text
*----------------------------------------------------------------------*
FORM zlpad  USING p_padl
                  p_tamanho
         CHANGING p_valor.

  DATA : vl_len   TYPE i,
         vl_valor TYPE string.

  vl_valor = p_valor.

  vl_len =  strlen( vl_valor ).

  WHILE p_tamanho > vl_len .

    CONCATENATE p_padl p_valor INTO vl_valor RESPECTING BLANKS.

    p_valor = vl_valor.

    vl_len =  strlen( p_valor ).

  ENDWHILE.

ENDFORM.                    " ZLPAD


FORM f_check_data TABLES p_return STRUCTURE  zfie_return
                   USING p_seqlan TYPE ze_seqlan
                         p_data
                         p_append_return
                CHANGING v_error.

  DATA: v_data   TYPE sy-datum,
        v_data_c TYPE c LENGTH 100.

  CLEAR: v_error.

  v_data_c = p_data.

  CHECK v_data_c IS NOT INITIAL.

  v_data = v_data_c+6(4) && v_data_c+3(2) && v_data_c+00(2).

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = v_data
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.

    v_error = abap_true.

    IF p_append_return EQ abap_true.
      CLEAR wa_return.
      wa_return-seqlan     = p_seqlan.
      wa_return-type       = 'E'.
      wa_return-id         = sy-msgid.
      wa_return-number     = sy-msgno.
      wa_return-message_v1 = sy-msgv1.
      wa_return-message_v2 = sy-msgv2.
      wa_return-message_v3 = sy-msgv3.
      wa_return-message_v4 = sy-msgv4.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO wa_return-message.
      APPEND wa_return TO p_return.
    ENDIF.

  ENDIF.

ENDFORM.
