*&---------------------------------------------------------------------*
*&  Include           ZLESR0162_EVT
*&---------------------------------------------------------------------*

START-OF-SELECTION.


  IF sy-batch IS NOT INITIAL.

    PERFORM f_execute_job.

  ELSE.

    CALL SCREEN '0100'.

  ENDIF.
