FUNCTION z_wait_cloking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------
  DATA: cont TYPE sy-tabix,
        text(255) .
  text = 'Processando..'.

  DO 50 TIMES.
    ADD 2 TO cont.
    IF text+11(6) EQ '......'.
      text+11(6) = '..'.
    ELSEIF text+11(6) EQ '....'.
      text+11(6) = '......'.
    ELSEIF text+11(6) EQ '..'.
      text+11(6) = '....'.

    ENDIF.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = cont
        text       = text.

    WAIT UP TO 1 SECONDS.

  ENDDO.



ENDFUNCTION.
