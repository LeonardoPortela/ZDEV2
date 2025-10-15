*&---------------------------------------------------------------------*
*& Report  ZFIY0029
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfiy0029.

DATA: v_valfrom TYPE setleaf-valfrom.

PARAMETERS: p_name LIKE setleaf-setname DEFAULT 'ZIIBB_BSAS',
            p_date LIKE sy-datum OBLIGATORY.

INITIALIZATION.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'P_NAME'.
        screen-input = 0.
        MODIFY SCREEN.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE valfrom INTO v_valfrom
    FROM setleaf
    WHERE setname EQ p_name
      AND lineid  EQ 1.

  WRITE v_valfrom TO p_date.

AT SELECTION-SCREEN.



START-OF-SELECTION.

  UPDATE setleaf SET valfrom = p_date
                 WHERE setname EQ p_name.

  IF sy-subrc IS INITIAL.
    MESSAGE i011(pc) WITH text-001.
  ELSE.
    MESSAGE e011(pc) WITH text-002.
  ENDIF.

  STOP.
