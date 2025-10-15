*----------------------------------------------------------------------*
***INCLUDE LZMMT0070_GRUPOI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  UPDATE_NOME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE UPDATE_NOME INPUT.


  SELECT SINGLE NAME1
    FROM KNA1
    INTO ZMMT0070-NOME_K
    WHERE KUNNR EQ ZMMT0070-KUNNR.


  SELECT SINGLE NAME1
    FROM LFA1
    INTO ZMMT0070-NOME_L
    WHERE LIFNR EQ ZMMT0070-LIFNR.


ENDMODULE.
