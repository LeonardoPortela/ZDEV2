FUNCTION zles_fat_contingencia_0003.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"  EXPORTING
*"     VALUE(E_ZSDT0001) TYPE  ZSDT0001
*"     VALUE(E_ZLEST0108) TYPE  ZLEST0108
*"----------------------------------------------------------------------

  DATA: lwa_zsdt0001  TYPE zsdt0001.
  DATA: lwa_ZLEST0108 TYPE ZLEST0108.
  DATA: lwa_zcte_ciot TYPE zcte_ciot.

  CLEAR: e_zsdt0001, E_ZLEST0108, lwa_zsdt0001, lwa_ZLEST0108.

  SELECT SINGLE *
    FROM j_1bnfdoc INTO @DATA(lwa_doc)
   WHERE docnum EQ @i_docnum.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM j_1bnflin INTO @DATA(lwa_lin)
   WHERE docnum EQ @i_docnum.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(lwa_active)
   WHERE docnum EQ @i_docnum.

  CHECK sy-subrc EQ 0.

  CASE lwa_doc-model.
    WHEN '55'.

      CASE lwa_lin-reftyp.
        WHEN 'BI'.
          SELECT SINGLE  *
            FROM zsdt0001 INTO lwa_zsdt0001
           WHERE fatura_prod = lwa_lin-refkey(10).
        WHEN 'MD'.
          SELECT SINGLE  *
            FROM zsdt0001 INTO lwa_zsdt0001
           WHERE fatura_prod = lwa_lin-refkey(10).

          IF sy-subrc ne 0.
            CLEAR: lwa_zsdt0001.

            SELECT SINGLE  *
              FROM zsdt0001 INTO lwa_zsdt0001
             WHERE doc_material = lwa_lin-refkey(10).
          ENDIF.

        WHEN 'ZW'.
          SELECT SINGLE  *
            FROM zsdt0001 INTO lwa_zsdt0001
           WHERE seq_lcto = lwa_lin-refkey(10).
      ENDCASE.

    WHEN '57'.

      SELECT SINGLE *
        FROM zcte_ciot INTO lwa_zcte_ciot
       WHERE docnum EQ i_docnum.

      IF sy-subrc EQ 0 AND lwa_zcte_ciot-tknum IS NOT INITIAL. "CTE Rodoviario
        SELECT SINGLE *
           FROM zsdt0001 INTO lwa_zsdt0001
          WHERE doc_transp = lwa_zcte_ciot-tknum.

        IF SY-SUBRC NE 0.
          SELECT SINGLE *
           FROM ZLEST0108 INTO lwa_ZLEST0108
          WHERE doc_transp = lwa_zcte_ciot-tknum.

          IF SY-SUBRC EQ 0.
            E_ZLEST0108 = lwa_ZLEST0108.
            EXIT.
          ENDIF.
        ENDIF.

      ENDIF.

    WHEN '58'.

      DATA: it_zsdt0105 TYPE TABLE OF zsdt0105.

      CLEAR: it_zsdt0105[].

      SELECT *
         INTO TABLE it_zsdt0105
         FROM zsdt0105
        WHERE docnum_ref = i_docnum.

      LOOP AT it_zsdt0105 INTO DATA(lwa_zsdt0105).
        CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0003'
          EXPORTING
            i_docnum   = lwa_zsdt0105-docnum
          IMPORTING
            e_zsdt0001 = lwa_zsdt0001.

        IF lwa_zsdt0001 IS INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.

  ENDCASE.

  CHECK lwa_zsdt0001-tp_movimento = 'S'.

  e_zsdt0001 = lwa_zsdt0001.




ENDFUNCTION.
