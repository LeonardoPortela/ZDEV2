*----------------------------------------------------------------------*
***INCLUDE LZSPED002I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ZGERANUMERO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module ZGERANUMERO .

  DATA: lc_key(10) TYPE n.

  if ZSPED002-NR_PARAMETRO is not initial and sy-UCOMM ne 'KOPF' .
    exit.
  endif.

  data: vn_parametro type ZSPED002-NR_PARAMETRO.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
    EXPORTING
      object           = 'ZNR_SPED02'
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr                   = '1'
      object                        = 'ZNR_SPED02'
      quantity                      = '00000000000000000001'
      ignore_buffer                 = 'X'
    IMPORTING
      number                        = lc_key
    EXCEPTIONS
      interval_not_found            = 1
      number_range_not_intern       = 2
      object_not_found              = 3
      quantity_is_0                 = 4
      quantity_is_not_1             = 5
      interval_overflow             = 6
      buffer_overflow               = 7
      OTHERS                        = 8.

* Desbloqueia o objeto de numeração
  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
    EXPORTING
      object           = 'ZBALT001'
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.

  ZSPED002-NR_PARAMETRO = lc_key.


endmodule.                 " ZGERANUMERO  INPUT
