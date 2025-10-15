*&--------------------------------------------------------------------&*
*& Report Name    : Validação das Ordens serviços GEO                 *&
*& Author         : Victor Hugo                                       *&
*& Date           : 08.01.2013                                        *&
*& Funcional Area : CO                                                *&
*&--------------------------------------------------------------------&*
REPORT  zmmj0001.

DATA: s_bukrs TYPE coep-bukrs,
      s_gjahr TYPE coep-gjahr ,
      s_perio TYPE coep-perio.

s_bukrs = '0015'.
s_gjahr = sy-datum(4).
s_perio = sy-datum+4(2).

SUBMIT zcor14
  WITH p_bukrs EQ s_bukrs
  WITH p_gjahr EQ s_gjahr
  WITH p_perio EQ s_perio.
