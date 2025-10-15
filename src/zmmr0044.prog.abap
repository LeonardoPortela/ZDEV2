*&---------------------------------------------------------------------*
*& Report  ZMMR0044
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR0044.

INCLUDE ZMMR0044_top.
INCLUDE ZMMR0044_class.
INCLUDE ZMMR0044_form.
INCLUDE ZMMR0044_pbo.
INCLUDE ZMMR0044_pai.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  TRY.
      CREATE OBJECT gob_main.
      gob_main->initial( ).
    CATCH cx_sy_create_object_error INTO exc_ref.
      exc_text = exc_ref->get_text( ).
      MESSAGE exc_text TYPE 'I'.
  ENDTRY.

*----------------------------------------------------------------------*
END-OF-SELECTION.
