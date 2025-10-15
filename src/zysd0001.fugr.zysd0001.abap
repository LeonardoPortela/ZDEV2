FUNCTION ZYSD0001.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(CODIGO_BARRA) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(DIGITO) TYPE  CHAR1
*"----------------------------------------------------------------------

  DATA: V_PAR(6)   TYPE N,
  V_IMPAR(6) TYPE N,
  V_SUMA(6)  TYPE C,
  LV_RESTO   TYPE I,
  V_DIGITO.

  IF CODIGO_BARRA IS NOT INITIAL.

    PERFORM F_CONVERTIR USING CODIGO_BARRA
                              'X'
                        CHANGING V_PAR
                                 V_IMPAR.

    V_IMPAR = V_IMPAR * 3.

    V_SUMA = V_PAR + V_IMPAR.

    PERFORM F_BUSCAR_DIFERENCIA_A_MULTIPLO USING V_SUMA
                                           CHANGING V_DIGITO.

    DIGITO = V_DIGITO.

  ENDIF.


ENDFUNCTION.
