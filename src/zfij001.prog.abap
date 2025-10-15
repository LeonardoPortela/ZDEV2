*&---------------------------------------------------------------------*
*& Report  ZFIJ001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfij001.

DATA: i_knka   LIKE  knka,                    "#EC CI_USAGE_OK[2227014]
      i_knkk   LIKE  knkk,                    "#EC CI_USAGE_OK[2227014]
      yknka    LIKE  knka,                    "#EC CI_USAGE_OK[2227014]
      yknkk    LIKE  knkk,                    "#EC CI_USAGE_OK[2227014]
      upd_knka LIKE  cdpos-chngind,
      upd_knkk LIKE  cdpos-chngind.

SELECT SINGLE COUNT(*)
  INTO @DATA(vg_job)
    FROM tbtco
   WHERE jobname EQ 'ZERAR_LIMITE_CRED'
     AND status EQ 'R'.

IF ( vg_job EQ 1 ).
  SELECT *
    FROM knka "#EC CI_USAGE_OK[2227014]
    INTO TABLE @DATA(t_knka)
   WHERE dlaus LT @sy-datum
   AND   klime GT 0.

  LOOP AT t_knka INTO DATA(w_knka).
    w_knka-klime = 0.
    w_knka-klimg = 0.
    MOVE-CORRESPONDING w_knka TO i_knka.
    upd_knka = 'U'.
    "
    CLEAR : upd_knkk, i_knkk.
*    SELECT SINGLE *
*      FROM KNKK
*      INTO I_KNKK
*    WHERE KUNNR = W_KNKA-KUNNR.
*    IF SY-SUBRC = 0 AND I_KNKK-KLIMK GT 0.
*      UPD_KNKK = 'U'.
*      I_KNKK-KLIMK = 0.
*    ENDIF.

    CALL FUNCTION 'CREDITLIMIT_CHANGE'
      EXPORTING
        i_knka   = i_knka
        i_knkk   = i_knkk
        upd_knka = upd_knka
        upd_knkk = upd_knkk
*       XNEUA    = ' '
*       XREFL    = ' '
        yknka    = yknka
        yknkk    = yknkk.

    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDLOOP.
ENDIF.
