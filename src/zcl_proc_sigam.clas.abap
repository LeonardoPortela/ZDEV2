class ZCL_PROC_SIGAM definition
  public
  final
  create public .

public section.

  class-methods CALL_TRANSACTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PROC_SIGAM IMPLEMENTATION.


  METHOD call_transaction.

    DATA lv_tcode TYPE sy-tcode.

    CASE sy-tcode.
      WHEN 'ZMM0197'.
        lv_tcode = 'ME21N'.
      WHEN 'ZSDT0132' OR 'ZSDT0132N'.
        lv_tcode = 'ZSDT0132N'.
      WHEN OTHERS.
    ENDCASE.

    CALL FUNCTION 'ZMM_CONSULTAR_SIGAM'
      EXPORTING
        i_tcode = lv_tcode.

  ENDMETHOD.
ENDCLASS.
