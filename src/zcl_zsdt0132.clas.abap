class ZCL_ZSDT0132 definition
  public
  final
  create public .

public section.

  class-methods CALL_PROGRAM .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZSDT0132 IMPLEMENTATION.


  METHOD call_program.

    SUBMIT zsdr0084 AND RETURN.


  ENDMETHOD.
ENDCLASS.
