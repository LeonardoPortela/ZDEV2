*&---------------------------------------------------------------------*
*& Include          ZPMR0090_EVT
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_class.
  DATA: lt_rettab TYPE TABLE OF ddshretval.

  DATA: lw_rettab TYPE ddshretval.

  SELECT class kschg
    FROM m_clasa
    INTO TABLE gt_class
    WHERE spras = sy-langu
      AND klart = '002'.
  IF sy-subrc IS INITIAL.
    SORT gt_class BY class.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CLASS'
        value_org       = 'S'
      TABLES
        value_tab       = gt_class
        return_tab      = lt_rettab
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc IS INITIAL.
      READ TABLE lt_rettab INTO lw_rettab INDEX 1.
      IF sy-subrc IS INITIAL.
        p_class = lw_rettab-fieldval.
      ENDIF.

    ENDIF.

  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  IF p_cla IS NOT INITIAL.

    FREE: s_eqart.
    CLEAR s_eqart.

    LOOP AT SCREEN.
      IF screen-name CS 'EQART'.
        screen-invisible = 1.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ELSE.

    CLEAR: p_class.

    LOOP AT SCREEN.
      IF screen-name CS 'CLASS'.
        screen-invisible = 1.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.


START-OF-SELECTION.

  PERFORM f_seleciona_Dados.
  PERFORM f_monta_dados.

  IF gt_saida IS NOT INITIAL.
    CALL SCREEN '9000'.
  ENDIF.
