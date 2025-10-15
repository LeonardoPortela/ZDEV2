*&---------------------------------------------------------------------*
*&  Include           ZMMR182_EVT
*&---------------------------------------------------------------------*

START-OF-SELECTION.

*  IF p_grao = 'X'.
    PERFORM f_select_data2.
    PERFORM f_process_data2.
*  ELSE.
*    PERFORM f_select_data.
*    PERFORM f_process_data.
*  ENDIF.

  IF t_saida IS NOT INITIAL.
    CALL SCREEN '0100'.
  ENDIF.
