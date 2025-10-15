*----------------------------------------------------------------------*
***INCLUDE LZLESF0053F01 .
*----------------------------------------------------------------------*
FORM f_atribuir_info.

DATA: WL_LFA1 type lfa1.


  CLEAR: WL_LFA1.

  zlest0053-usuario     = sy-uname.
  zlest0053-dt_cadastro = sy-datum.
  zlest0053-hr_cadastro = sy-uzeit.

 SELECT SINGLE * FROM LFA1 INTO WL_LFA1 WHERE LIFNR EQ ZLEST0053-PROPRIETARIO.
 ZLEST0053-NAME1 = WL_LFA1-NAME1.


ENDFORM.                    "F_ATRIBUIR_INFO
