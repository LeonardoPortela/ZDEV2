*----------------------------------------------------------------------*
***INCLUDE LZYSD0001F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CONVERTIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CODIGO_BARRA  text
*      -->P_0030   text
*      <--P_V_PAR  text
*      <--P_V_IMPAR  text
*----------------------------------------------------------------------*
FORM F_CONVERTIR  USING    p_datos
                           p_indicador
                  CHANGING P_PAR
                           P_IMPAR.

DATA: lv_indicador TYPE c,
      lv_len       TYPE i,
      lv_datos     TYPE string.

  IF p_indicador EQ space.
    lv_indicador = 'X'.
    p_par = p_par + p_datos(1).
  ELSE.
    lv_indicador = space.
    p_impar = p_impar + p_datos(1).
  ENDIF.

  lv_len = STRLEN( p_datos ).
  lv_len = lv_len - 1.
  lv_datos = p_datos+1(lv_len).

  CHECK lv_datos IS NOT INITIAL.

  PERFORM f_convertir USING lv_datos
                            lv_indicador
                      CHANGING p_par
                               p_impar.

ENDFORM.                    " F_CONVERTIR
*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_DIFERENCIA_A_MULTIPLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_SUMA  text
*      <--P_V_DIGITO  text
*----------------------------------------------------------------------*
FORM F_BUSCAR_DIFERENCIA_A_MULTIPLO  USING    P_SUMA
                                     CHANGING P_DIGITO.

  DATA: lv_multiplo TYPE i.
  lv_multiplo = 10.
  DO.
    IF lv_multiplo GE p_suma.
      p_digito = lv_multiplo - p_suma.
      EXIT.
    ELSE.
      lv_multiplo = lv_multiplo + 10.
    ENDIF.
  ENDDO.

ENDFORM.                    " F_BUSCAR_DIFERENCIA_A_MULTIPLO
