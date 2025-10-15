FUNCTION ZLES_CARGUERO_0004.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ID_INTEGRACAO) TYPE  ZDE_ID_INTEGRACAO
*"----------------------------------------------------------------------

  DATA: IDINTE TYPE RANGE OF ZDE_ID_INTEGRACAO.
  DATA: SDTREG TYPE RANGE OF ZDE_DT_REGISTRO.

  IDINTE = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = ID_INTEGRACAO HIGH = ID_INTEGRACAO ) ).
  SUBMIT ZINTEGRACAO
    WITH IDINTE EQ IDINTE
    WITH SDTREG IN SDTREG
     AND RETURN.

ENDFUNCTION.
