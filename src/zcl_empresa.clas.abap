class ZCL_EMPRESA definition
  public
  final
  create public .

public section.

  data AT_EMPRESA type T001 .
  class-data AT_ZCL_EMPRESA type ref to ZCL_EMPRESA .

  methods SET_EMPRESA
    importing
      !I_BUKRS type BUKRS
    returning
      value(R_ZCL_EMPRESA) type ref to ZCL_EMPRESA .
  methods GET_MOEDA_INTERNA
    returning
      value(R_MOEDA_INTERNA) type WAERS .
  methods GET_MOEDA_FORTE
    returning
      value(R_MOEDA_FORTE) type WAERS .
  class-methods GET_INSTANCE
    returning
      value(R_ZCL_EMPRESA) type ref to ZCL_EMPRESA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EMPRESA IMPLEMENTATION.


  METHOD GET_INSTANCE.

    IF AT_ZCL_EMPRESA IS NOT BOUND.
      CREATE OBJECT AT_ZCL_EMPRESA.
      R_ZCL_EMPRESA = AT_ZCL_EMPRESA.
    ELSE.
      R_ZCL_EMPRESA = AT_ZCL_EMPRESA.
    ENDIF.

  ENDMETHOD.


  METHOD GET_MOEDA_FORTE.

    DATA: E_X001 TYPE X001.

    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        I_BUKRS                = ME->AT_EMPRESA-BUKRS
      IMPORTING
        E_X001                 = E_X001
      EXCEPTIONS
        CURRENCY_2_NOT_DEFINED = 1
        CURRENCY_3_NOT_DEFINED = 2
        OTHERS                 = 3.

    CHECK SY-SUBRC IS INITIAL.

    R_MOEDA_FORTE = E_X001-HWAE2.

  ENDMETHOD.


  METHOD GET_MOEDA_INTERNA.
    R_MOEDA_INTERNA = ME->AT_EMPRESA-WAERS.
  ENDMETHOD.


  METHOD SET_EMPRESA.

    SELECT SINGLE * INTO @ME->AT_EMPRESA
      FROM T001
     WHERE BUKRS EQ @I_BUKRS.

    R_ZCL_EMPRESA = ME.

  ENDMETHOD.
ENDCLASS.
