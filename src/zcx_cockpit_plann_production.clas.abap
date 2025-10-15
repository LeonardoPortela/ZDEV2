class ZCX_COCKPIT_PLANN_PRODUCTION definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

*  interfaces IF_T100_MESSAGE .
*  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_COCKPIT_PLANN_PRODUCTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
  endmethod.


  method IF_MESSAGE~GET_TEXT.
    move textid to result.
  endmethod.
ENDCLASS.
