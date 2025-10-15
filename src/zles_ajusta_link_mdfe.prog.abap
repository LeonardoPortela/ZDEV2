*&---------------------------------------------------------------------*
*& Report ZLES_AJUSTA_LINK_MDFE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zles_ajusta_link_mdfe.

DATA: t_0102 TYPE TABLE OF zsdt0102,
      lc_url TYPE string.

SELECT *
  FROM zsdt0102
  INTO TABLE t_0102
  WHERE contingencia = abap_true.

LOOP AT t_0102 INTO DATA(w_0102).

  SELECT SINGLE *
    FROM j_1bnfe_active
    INTO @DATA(wl_active_mdfe)
   WHERE docnum EQ @w_0102-docnum.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'Z_GRC_MONTA_LINK'
    EXPORTING
      i_docnum   = w_0102-docnum
    IMPORTING
      e_link_pdf = lc_url.

  w_0102-url_sefaz = lc_url.

  CALL FUNCTION 'Z_GRC_REGISTRA_INF_ZIB_NFE'
    EXPORTING
      i_docnum = w_0102-docnum
      i_active = wl_active_mdfe.

  MODIFY zsdt0102 FROM w_0102.

  COMMIT WORK.

ENDLOOP.
