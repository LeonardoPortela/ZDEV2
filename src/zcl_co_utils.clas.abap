class ZCL_CO_UTILS definition
  public
  final
  create public .

public section.

  class-methods CONVERTE_CCUSTO_AGRO_OTELHAR
    changing
      !C_KOSTL type KOSTL
      !C_BUKRS type BUKRS optional
      !C_WERKS type WERKS_D optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CO_UTILS IMPLEMENTATION.


  METHOD converte_ccusto_agro_otelhar.

    DATA: lva_kostl TYPE cskt-kostl.

    CHECK c_kostl IS NOT INITIAL AND strlen( c_kostl ) >= 10.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarvc)
     WHERE name EQ 'CO_CCUSTO_AGRO_EXC_CONVERSAO'
       AND low  EQ @c_kostl.

    CHECK sy-subrc NE 0.

    "Centro 1521 foi incorporado na 5058

    CHECK c_kostl(6) = '015021'.

    lva_kostl = '050058' && c_kostl+6(4).

    c_kostl = lva_kostl.

    c_bukrs = '0050'.

    c_werks = '5058'.

  ENDMETHOD.
ENDCLASS.
