*----------------------------------------------------------------------*
***INCLUDE Z_1BNFE_MONITOR_F33 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_FATURA_CANCELADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VL_OK  text
*----------------------------------------------------------------------*
FORM Z_VERIFICA_FATURA_CANCELADA USING VL_DOCNUM CHANGING P_VL_OK.

  DATA : VL_REFKEY TYPE J_1BNFLIN-REFKEY,
         VL_VBELN  TYPE VBFA-VBELN,
         VL_QTD    TYPE I.

  SELECT SINGLE REFKEY
    INTO VL_REFKEY
    FROM J_1BNFLIN
   WHERE DOCNUM EQ VL_DOCNUM.

  SELECT SINGLE VBELN
    INTO VL_VBELN
    FROM VBFA
   WHERE VBTYP_N EQ 'N' " Estorno fatura
     AND VBTYP_V EQ 'M' " Fatura
     AND VBELV   EQ VL_REFKEY.

  IF SY-SUBRC IS INITIAL .
    P_VL_OK = 'N'.
  ELSE.
    P_VL_OK = 'S'.
  ENDIF.

ENDFORM.                    " Z_VERIFICA_FATURA_CANCELADA
