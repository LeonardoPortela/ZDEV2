*&---------------------------------------------------------------------*
*& Report  ZREGISTER_FUNC
*&---------------------------------------------------------------------*
REPORT zregister_func.

PARAMETERS: p_db_01  TYPE tabname,
            p_stc_01 TYPE tabname,
            p_ti_01  TYPE cua_tit_tx,
            p_stalin TYPE numc3,
            p_stacol TYPE numc3,
            p_endlin TYPE numc3,
            p_endcol TYPE numc3.

START-OF-SELECTION.

  CALL FUNCTION 'ZREGISTER_DATA'
    EXPORTING
      i_db_tab    = p_db_01
      i_stcnam    = p_stc_01
      i_title     = p_ti_01
      i_start_lin = p_stalin
      i_start_col = p_stacol
      i_end_lin   = p_endlin
      i_end_col   = p_endcol
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
