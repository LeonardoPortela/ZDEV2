class ZCL_CFOP definition
  public
  final
  create public .

public section.

  class-methods GET_CK_CFOP_RETORNO_AMAZEM
    importing
      !I_CFOP type CHAR04
    returning
      value(R_RETORNO) type CHAR01 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CFOP IMPLEMENTATION.


  METHOD GET_CK_CFOP_RETORNO_AMAZEM.

    CLEAR: R_RETORNO.

    CHECK I_CFOP IS NOT INITIAL.

    CHECK ZCL_STRING=>LENGTH( CONV #( I_CFOP ) ) EQ 4.

    DATA(LC_CFOP) = |{ I_CFOP }%|.

    SELECT SINGLE * INTO @DATA(WA_ZMMT0119)
      FROM ZMMT0119
     WHERE CFOP LIKE @LC_CFOP.

    R_RETORNO = COND STRING( WHEN SY-SUBRC IS INITIAL THEN ABAP_TRUE ELSE ABAP_FALSE ).

  ENDMETHOD.
ENDCLASS.
