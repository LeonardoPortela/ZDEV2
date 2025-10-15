*----------------------------------------------------------------------*
***INCLUDE LZLES0005F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_RANGE_SERIE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_SERIE  text
*      -->P_TG_ZSDT0001_E_SERIES  text
*----------------------------------------------------------------------*

FORM F_PREPARE_RANGE_SERIE TABLES C_RG_SERIE STRUCTURE RG_SERIE
                            USING P_SERIE.

  DATA: V_SERIE             TYPE J_1BNFDOC-SERIES,
        VL_SERIE_INT_AUX    TYPE I,
        VL_SERIE_SRT_AUX    TYPE C LENGTH 3.

  CLEAR: C_RG_SERIE[], V_SERIE.

  V_SERIE = P_SERIE.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = V_SERIE
    IMPORTING
      OUTPUT = V_SERIE.

  C_RG_SERIE-SIGN   = 'I'.
  C_RG_SERIE-OPTION = 'EQ'.
  C_RG_SERIE-LOW    = V_SERIE.
  C_RG_SERIE-HIGH   = V_SERIE.
  APPEND C_RG_SERIE.

  C_RG_SERIE-SIGN   = 'I'.
  C_RG_SERIE-OPTION = 'EQ'.
  C_RG_SERIE-LOW    = V_SERIE+2(1).
  C_RG_SERIE-HIGH   = V_SERIE+2(1).
  APPEND C_RG_SERIE.

  IF STRLEN( V_SERIE ) = 3.
    TRY.
        VL_SERIE_INT_AUX = V_SERIE.
        VL_SERIE_SRT_AUX = VL_SERIE_INT_AUX.
        CONDENSE VL_SERIE_SRT_AUX NO-GAPS.
        IF STRLEN( VL_SERIE_SRT_AUX ) <> STRLEN( V_SERIE ).
          C_RG_SERIE-SIGN   = 'I'.
          C_RG_SERIE-OPTION = 'EQ'.
          C_RG_SERIE-LOW    = V_SERIE+1(2).
          C_RG_SERIE-HIGH   = V_SERIE+1(2).
          APPEND C_RG_SERIE.
        ENDIF.
      CATCH CX_SY_CONVERSION_NO_NUMBER.
    ENDTRY.
  ENDIF.

  IF V_SERIE <> '000'.
    C_RG_SERIE-SIGN   = 'I'.
    C_RG_SERIE-OPTION = 'EQ'.
    C_RG_SERIE-LOW    = V_SERIE.
    C_RG_SERIE-HIGH   = V_SERIE.
    SHIFT C_RG_SERIE-LOW  LEFT DELETING LEADING '0'.
    SHIFT C_RG_SERIE-HIGH LEFT DELETING LEADING '0'.
    APPEND C_RG_SERIE.
  ENDIF.

  C_RG_SERIE-SIGN   = 'I'.
  C_RG_SERIE-OPTION = 'EQ'.
  C_RG_SERIE-LOW    = V_SERIE.
  C_RG_SERIE-HIGH   = V_SERIE.
  SHIFT C_RG_SERIE-LOW  LEFT DELETING LEADING SPACE.
  SHIFT C_RG_SERIE-HIGH LEFT DELETING LEADING SPACE.
  APPEND C_RG_SERIE.

  SORT C_RG_SERIE BY SIGN OPTION LOW HIGH.
  DELETE ADJACENT DUPLICATES FROM C_RG_SERIE COMPARING SIGN OPTION LOW HIGH.

ENDFORM.
