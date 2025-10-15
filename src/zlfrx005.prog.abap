*&---------------------------------------------------------------------*
*& Report ZLFRX005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlfrx005.

FORM f_exit_zsati100 CHANGING itab_zsati100 TYPE /pwsati/zsati100.

  DATA: lv_belnr TYPE belnr_d.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = itab_zsati100-ist_docnum_cont
    IMPORTING
      output = lv_belnr.

  CONDENSE lv_belnr NO-GAPS.


  SELECT SINGLE * INTO @DATA(ls_bkpf)
    FROM bkpf
    WHERE bukrs = @itab_zsati100-ist_cod_matriz
      AND belnr = @lv_belnr
      AND gjahr = @itab_zsati100-ist_dt_entrada(4)
      AND awtyp = 'IDOC'.

  IF sy-subrc = 0.

    SELECT SINGLE * INTO @DATA(ls_bseg)
      FROM bseg
      WHERE bukrs = @ls_bkpf-bukrs
        AND belnr = @ls_bkpf-belnr
        AND gjahr = @ls_bkpf-gjahr
        AND bschl = '31'
        AND shkzg = 'H'.

    IF sy-subrc = 0.

      SELECT SINGLE * INTO @DATA(ls_lfa1)
        FROM lfa1
        WHERE lifnr = @ls_bseg-lifnr.

      IF sy-subrc = 0.

        IF ls_lfa1-indtyp = 'Z3'.

          itab_zsati100-ist_tot_nota = ls_bseg-dmbtr.

* DEVK9A2OG5 - 08/07/2025 - INÍCIO
*          INSERT INTO /pwsati/zsati100 VALUES itab_zsati100.
* DEVK9A2OG5 - 08/07/2025 - FIM

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

* DEVK9A2OG5 - 08/07/2025 - INÍCIO
  INSERT INTO /pwsati/zsati100 VALUES itab_zsati100.
* DEVK9A2OG5 - 08/07/2025 - FIM

ENDFORM.
