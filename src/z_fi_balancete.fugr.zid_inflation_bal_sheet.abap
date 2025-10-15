FUNCTION          ZID_INFLATION_BAL_SHEET.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SKB1) TYPE  SKB1
*"     REFERENCE(I_BILBJAHR) TYPE  BKPF-GJAHR
*"     REFERENCE(I_BILVJAHR) TYPE  BKPF-GJAHR
*"  CHANGING
*"     REFERENCE(C_ZSKC1A) TYPE  ZSKC1A
*"  EXCEPTIONS
*"      ADJUSTMENT_NOT_POSSIBLE
*"----------------------------------------------------------------------
  DATA: BEGIN OF int_amounts OCCURS 0,
          x_soll   TYPE umxxh,
          x_haben  TYPE umxxs,
          x_soll2  TYPE umxxh,
          x_haben2 TYPE umxxs,
        END OF int_amounts.
  DATA: BEGIN OF int_dates OCCURS 0,
          startdate LIKE sy-datum,
          enddate   LIKE sy-datum,
        END OF int_dates.
  DATA: x_general_index LIKE j_1ainfmet-j_1aipmng,
        x_tbe           LIKE j_1ainfsks-j_1atbedep,
        start_date      LIKE sy-datum,
        end_date        LIKE sy-datum,
        x_monat         LIKE bkpf-monat,
        x_soll          TYPE umxxs,
        x_haben         TYPE umxxh,
        x_soll2         TYPE umxxs,
        x_haben2        TYPE umxxh,
* ---- Note 524914: start ----*
        x_amount        TYPE umsav,
        x_adjustment    TYPE umsav.
* ---- Note 524914: end ---- *


* Check if account is relevant for inflation adjustment
  CHECK i_skb1-infky ne space.

* read T001 if required
  IF g_bukrs ne i_skb1-bukrs.
    SELECT SINGLE * FROM t001
      WHERE bukrs = i_skb1-bukrs.
  ENDIF.
* read fiscal year information if required
  IF t009-periv ne t001-periv.
    SELECT SINGLE * FROM t009
      WHERE periv = t001-periv.
  ENDIF.                               " Note 573047

*  Start: Last day of year previous to comparison year
*  End: Last day of year previous to current year
    x_monat = '01'.
    CALL FUNCTION 'PERIOD_DAY_DETERMINE'
         EXPORTING
              i_gjahr = i_bilvjahr
              i_monat = x_monat
              i_periv = t009-periv
         IMPORTING
              e_fday  = start_date.
    start_date = start_date - 1.

    CALL FUNCTION 'PERIOD_DAY_DETERMINE'
         EXPORTING
              i_gjahr = i_bilbjahr
              i_monat = x_monat
              i_periv = t009-periv
         IMPORTING
              e_fday  = end_date.
    end_date = end_date - 1.

*Build up table of period dates (only performed once) ---------------
    REFRESH int_dates.
    DO t009-anzbp TIMES.
      x_monat = sy-index.
      CALL FUNCTION 'PERIOD_DAY_DETERMINE'
           EXPORTING
                i_gjahr = i_bilvjahr
                i_monat = x_monat
                i_periv = t009-periv
           IMPORTING
                e_lday  = int_dates-startdate.

      CALL FUNCTION 'PERIOD_DAY_DETERMINE'
           EXPORTING
                i_gjahr = i_bilbjahr
                i_monat = x_monat
                i_periv = t009-periv
           IMPORTING
                e_lday  = int_dates-enddate.
      APPEND int_dates.
    ENDDO.

* read control data from database if necessary
  IF t001-infmt ne j_1ainfmet-j_1ainfmet or
     t001-land1 ne j_1ainfmet-land1.
    SELECT SINGLE * FROM j_1ainfmet
      WHERE land1 = t001-land1
      AND   j_1ainfmet = t001-infmt.
  ENDIF.
  IF i_skb1-infky ne j_1ainfsks-j_1aifsks.
    SELECT SINGLE * FROM j_1ainfsks
      WHERE land1     = t001-land1
      AND   j_1aifsks = i_skb1-infky.
    if sy-subrc ne 0.
      message e239 with i_skb1-saknr raising adjustment_not_possible.
    endif.
  ENDIF.

*time base and exposure to inflation
  IF j_1ainfsks-j_1atbedep is initial.
    x_tbe = j_1ainfmet-j_1atbeapp.
  ELSE.
    x_tbe = j_1ainfsks-j_1atbedep.
  ENDIF.

* Adjustment of initial balance of comparison year-------------------

  x_amount = c_zskc1a-umsav.
  CALL FUNCTION 'J_1A_INFLATION_CALCULATION_FI'
       EXPORTING
* ---- Note 524914: start ---- *
            amount         = 0
            amount_long    = x_amount
* ---- Note 524914: end ---- *
            origin_tbep    = x_tbe
            origin_date    = start_date
            final_tbep     = x_tbe
            final_date     = end_date
            specific_index = j_1ainfmet-j_1aipmng
            prov_specific  = space
       IMPORTING
            adjustment     = x_adjustment
       EXCEPTIONS
            OTHERS         = 1.

  IF sy-subrc <> 0.
    MESSAGE e239 WITH i_skb1-saknr RAISING adjustment_not_possible.
  else.
    c_zskc1a-umsav = c_zskc1a-umsav + x_adjustment.
  endif.

  x_amount = c_zskc1a-umsav2.
  CALL FUNCTION 'J_1A_INFLATION_CALCULATION_FI'
       EXPORTING
* ---- Note 524914: start ---- *
            amount         = 0
            amount_long    = x_amount
* ---- Note 524914: end ---- *
            origin_tbep    = x_tbe
            origin_date    = start_date
            final_tbep     = x_tbe
            final_date     = end_date
            specific_index = j_1ainfmet-j_1aipmng
            prov_specific  = space
       IMPORTING
            adjustment     = x_adjustment
       EXCEPTIONS
            OTHERS         = 1.

  IF sy-subrc <> 0.
    MESSAGE e239 WITH i_skb1-saknr RAISING adjustment_not_possible.
  else.
    c_zskc1a-umsav2 = c_zskc1a-umsav2 + x_adjustment.
  endif.

* Adjustment of period balances--------------------------------------
  DO 16 TIMES VARYING x_soll   FROM c_zskc1a-um01s  NEXT c_zskc1a-um02s
              VARYING x_haben  FROM c_zskc1a-um01h  NEXT c_zskc1a-um02h
              VARYING x_soll2  FROM c_zskc1a-um01s2 NEXT c_zskc1a-um02s2
              VARYING x_haben2 FROM c_zskc1a-um01h2 NEXT c_zskc1a-um02h2.

    READ TABLE int_dates INDEX sy-index.
    CHECK sy-subrc = 0.

    IF x_soll ne 0.
      x_amount = x_soll.
      CALL FUNCTION 'J_1A_INFLATION_CALCULATION_FI'
           EXPORTING
* ---- Note 524914: start ---- *
                amount         = 0
                amount_long    = x_amount
* ---- Note 524914: end ---- *
                origin_tbep    = x_tbe
                origin_date    = int_dates-startdate
                final_tbep     = x_tbe
                final_date     = int_dates-enddate
                specific_index = j_1ainfmet-j_1aipmng
                prov_specific  = space
           IMPORTING
                adjustment     = x_adjustment
           EXCEPTIONS
                OTHERS         = 1.

      IF sy-subrc <> 0.
        message e239 with i_skb1-saknr raising adjustment_not_possible.
      else.
        x_soll = x_soll + x_adjustment.
      ENDIF.
    ENDIF.
    int_amounts-x_soll = x_soll.

    IF x_soll2 ne 0.
      x_amount = x_soll2.
      CALL FUNCTION 'J_1A_INFLATION_CALCULATION_FI'
           EXPORTING
* ---- Note 524914: start ---- *
                amount         = 0
                amount_long    = x_amount
* ---- Note 524914: end ---- *
                origin_tbep    = x_tbe
                origin_date    = int_dates-startdate
                final_tbep     = x_tbe
                final_date     = int_dates-enddate
                specific_index = j_1ainfmet-j_1aipmng
                prov_specific  = space
           IMPORTING
                adjustment     = x_adjustment
           EXCEPTIONS
                OTHERS         = 1.

      IF sy-subrc <> 0.
        message e239 with i_skb1-saknr raising adjustment_not_possible.
      else.
        x_soll2 = x_soll2 + x_adjustment.
      ENDIF.
    ENDIF.
    int_amounts-x_soll2 = x_soll2.

    IF x_haben ne 0.
      x_amount = x_haben.
      CALL FUNCTION 'J_1A_INFLATION_CALCULATION_FI'
           EXPORTING
* ---- Note 524914: start ---- *
                amount         = 0
                amount_long    = x_amount
* ---- Note 524914: end ---- *
                origin_tbep    = x_tbe
                origin_date    = int_dates-startdate
                final_tbep     = x_tbe
                final_date     = int_dates-enddate
                specific_index = j_1ainfmet-j_1aipmng
                prov_specific  = space
           IMPORTING
                adjustment     = x_adjustment
           EXCEPTIONS
                OTHERS         = 1.

      IF sy-subrc <> 0.
        message e239 with i_skb1-saknr raising adjustment_not_possible.
      ELSE.                                   " Note 573047
        x_haben = x_haben + x_adjustment.
      ENDIF.
    ENDIF.
    int_amounts-x_haben = x_haben.

    IF x_haben2 ne 0.
      x_amount = x_haben2.
      CALL FUNCTION 'J_1A_INFLATION_CALCULATION_FI'
           EXPORTING
* ---- Note 524914: start ---- *
                amount         = 0
                amount_long    = x_amount
* ---- Note 524914: end ---- *
                origin_tbep    = x_tbe
                origin_date    = int_dates-startdate
                final_tbep     = x_tbe
                final_date     = int_dates-enddate
                specific_index = j_1ainfmet-j_1aipmng
                prov_specific  = space
           IMPORTING
                adjustment     = x_adjustment
           EXCEPTIONS
                OTHERS         = 1.

      IF sy-subrc <> 0.
        message e239 with i_skb1-saknr raising adjustment_not_possible.
      ELSE.                                   " Note 573047
        x_haben2 = x_haben2 + x_adjustment.
      ENDIF.
    ENDIF.
    int_amounts-x_haben2 = x_haben2.

    APPEND int_amounts.
  ENDDO.

  READ TABLE int_amounts INDEX 1.
  IF sy-subrc = 0.
    c_zskc1a-um01s  = int_amounts-x_soll.
    c_zskc1a-um01h  = int_amounts-x_haben.
    c_zskc1a-um01s2 = int_amounts-x_soll2.
    c_zskc1a-um01h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 2.
  IF sy-subrc = 0.
    c_zskc1a-um02s  = int_amounts-x_soll.
    c_zskc1a-um02h  = int_amounts-x_haben.
    c_zskc1a-um02s2 = int_amounts-x_soll2.
    c_zskc1a-um02h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 3.
  IF sy-subrc = 0.
    c_zskc1a-um03s  = int_amounts-x_soll.
    c_zskc1a-um03h  = int_amounts-x_haben.
    c_zskc1a-um03s2 = int_amounts-x_soll2.
    c_zskc1a-um03h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 4.
  IF sy-subrc = 0.
    c_zskc1a-um04s  = int_amounts-x_soll.
    c_zskc1a-um04h  = int_amounts-x_haben.
    c_zskc1a-um04s2 = int_amounts-x_soll2.
    c_zskc1a-um04h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 5.
  IF sy-subrc = 0.
    c_zskc1a-um05s  = int_amounts-x_soll.
    c_zskc1a-um05h  = int_amounts-x_haben.
    c_zskc1a-um05s2 = int_amounts-x_soll2.
    c_zskc1a-um05h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 6.
  IF sy-subrc = 0.
    c_zskc1a-um06s  = int_amounts-x_soll.
    c_zskc1a-um06h  = int_amounts-x_haben.
    c_zskc1a-um06s2 = int_amounts-x_soll2.
    c_zskc1a-um06h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 7.
  IF sy-subrc = 0.
    c_zskc1a-um07s  = int_amounts-x_soll.
    c_zskc1a-um07h  = int_amounts-x_haben.
    c_zskc1a-um07s2 = int_amounts-x_soll2.
    c_zskc1a-um07h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 8.
  IF sy-subrc = 0.
    c_zskc1a-um08s  = int_amounts-x_soll.
    c_zskc1a-um08h  = int_amounts-x_haben.
    c_zskc1a-um08s2 = int_amounts-x_soll2.
    c_zskc1a-um08h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 9.
  IF sy-subrc = 0.
    c_zskc1a-um09s  = int_amounts-x_soll.
    c_zskc1a-um09h  = int_amounts-x_haben.
    c_zskc1a-um09s2 = int_amounts-x_soll2.
    c_zskc1a-um09h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 10.
  IF sy-subrc = 0.
    c_zskc1a-um10s  = int_amounts-x_soll.
    c_zskc1a-um10h  = int_amounts-x_haben.
    c_zskc1a-um10s2 = int_amounts-x_soll2.
    c_zskc1a-um10h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 11.
  IF sy-subrc = 0.
    c_zskc1a-um11s  = int_amounts-x_soll.
    c_zskc1a-um11h  = int_amounts-x_haben.
    c_zskc1a-um11s2 = int_amounts-x_soll2.
    c_zskc1a-um11h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 12.
  IF sy-subrc = 0.
    c_zskc1a-um12s  = int_amounts-x_soll.
    c_zskc1a-um12h  = int_amounts-x_haben.
    c_zskc1a-um12s2 = int_amounts-x_soll2.
    c_zskc1a-um12h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 13.
  IF sy-subrc = 0.
    c_zskc1a-um13s  = int_amounts-x_soll.
    c_zskc1a-um13h  = int_amounts-x_haben.
    c_zskc1a-um13s2 = int_amounts-x_soll2.
    c_zskc1a-um13h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 14.
  IF sy-subrc = 0.
    c_zskc1a-um14s  = int_amounts-x_soll.
    c_zskc1a-um14h  = int_amounts-x_haben.
    c_zskc1a-um14s2 = int_amounts-x_soll2.
    c_zskc1a-um14h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 15.
  IF sy-subrc = 0.
    c_zskc1a-um15s  = int_amounts-x_soll.
    c_zskc1a-um15h  = int_amounts-x_haben.
    c_zskc1a-um15s2 = int_amounts-x_soll2.
    c_zskc1a-um15h2 = int_amounts-x_haben2.
  ENDIF.
  READ TABLE int_amounts INDEX 16.
  IF sy-subrc = 0.
    c_zskc1a-um16s  = int_amounts-x_soll.
    c_zskc1a-um16h  = int_amounts-x_haben.
    c_zskc1a-um16s2 = int_amounts-x_soll2.
    c_zskc1a-um16h2 = int_amounts-x_haben2.
  ENDIF.

ENDFUNCTION.
