class ZCL_DOMAIN definition
  public
  final
  create public .

public section.

  class-methods GET_TEXT_VALUE_DOMAIN
    importing
      !I_DOMNAME type DD07L-DOMNAME
      !I_VALUE type DD07L-DOMVALUE_L
    returning
      value(R_DDTEXT) type VAL_TEXT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DOMAIN IMPLEMENTATION.


  METHOD GET_TEXT_VALUE_DOMAIN.

    DATA: LC_DD07V_WA TYPE DD07V,
          LC_RC       TYPE SY-SUBRC.

    CLEAR: R_DDTEXT.

    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        DOMNAME  = I_DOMNAME
        VALUE    = I_VALUE
        "LANGU    = SY-SUBRC
        LANGU    = SY-LANGU
      IMPORTING
        DD07V_WA = LC_DD07V_WA
        RC       = LC_RC.

    IF LC_RC IS INITIAL.
      R_DDTEXT = LC_DD07V_WA-DDTEXT.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
