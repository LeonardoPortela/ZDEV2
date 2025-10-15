*&---------------------------------------------------------------------*
*& Report ZLES_CONV_ZLEST0215
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zles_conv_zlest0215.

SELECT *
  FROM zlest0215
  INTO TABLE @DATA(t_0215).

LOOP AT t_0215 INTO DATA(w_0215).
  UPDATE zlest0215 SET bukrs       = '0001'
                       id_processo = 'PIS_COF_PED'
                 WHERE bukrs       = abap_off
                   AND lifnr       = w_0215-lifnr
                   AND id_processo = abap_off.
ENDLOOP.

COMMIT WORK.
