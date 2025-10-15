*&---------------------------------------------------------------------*
*& Report  ZPPT0030_PROC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zppt0030_proc.


SELECT * FROM zppt0030
         INTO TABLE @DATA(t_0030)
        WHERE processamento = '01'
          AND emproc_normal = @abap_on
          AND proces_normal = @abap_off.

LOOP AT t_0030 INTO DATA(w_0030).
  w_0030-emproc_normal = abap_off.
  w_0030-proces_normal = abap_off.
  MODIFY zppt0030 FROM w_0030.
ENDLOOP.

COMMIT WORK.
