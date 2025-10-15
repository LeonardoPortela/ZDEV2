*----------------------------------------------------------------------*
***INCLUDE LZ_FI_GLF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_VALORES_FAGLFLEXT
*&---------------------------------------------------------------------*
*       Alimenta valores de
*----------------------------------------------------------------------*
*      -->P_IT_MOEDA  text
*      -->P_WA_FAGLFLEXT  text
*      <--P_IT_SALDO_FAGL  text
*----------------------------------------------------------------------*
FORM ORGANIZA_VALORES_FAGLFLEXT  USING    P_MOEDA TYPE TY_MD_FLEXT_EMP
                                          P_FAGL  TYPE FAGLFLEXT
                                 CHANGING P_SALDO TYPE ZDE_FI_GL_SALDO_FAGLFLEXT.

  DATA: REFE1 TYPE HSLXX12,
        REFE2 TYPE HSLXX12.

  DO 16 TIMES
    VARYING REFE2 FROM P_SALDO-SL01 NEXT P_SALDO-SL02.
    REFE2 = 0.
  ENDDO.

  CASE P_MOEDA-TP_MOEDA.
    WHEN 'H'.
      P_SALDO-SLVT = P_FAGL-HSLVT.
      DO 16 TIMES
        VARYING REFE1 FROM P_FAGL-HSL01 NEXT P_FAGL-HSL02
        VARYING REFE2 FROM P_SALDO-SL01 NEXT P_SALDO-SL02.
        REFE2 = REFE1.
      ENDDO.
    WHEN 'K'.
      P_SALDO-SLVT = P_FAGL-KSLVT.
      DO 16 TIMES
        VARYING REFE1 FROM P_FAGL-KSL01 NEXT P_FAGL-KSL02
        VARYING REFE2 FROM P_SALDO-SL01 NEXT P_SALDO-SL02.
        REFE2 = REFE1.
      ENDDO.
    WHEN 'O'.
      P_SALDO-SLVT = P_FAGL-OSLVT.
      DO 16 TIMES
        VARYING REFE1 FROM P_FAGL-OSL01 NEXT P_FAGL-OSL02
        VARYING REFE2 FROM P_SALDO-SL01 NEXT P_SALDO-SL02.
        REFE2 = REFE1.
      ENDDO.
  ENDCASE.

ENDFORM.                    " ORGANIZA_VALORES_FAGLFLEXT

*&---------------------------------------------------------------------*
*&      Form  ADD_MONTA_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ADD_MONTA_SHDB  TABLES   P_TI_BDCDATA STRUCTURE BDCDATA
                     USING    VALUE(P_PROGRAM)
                              VALUE(P_DYNPRO)
                              VALUE(P_DYNBEGIN).

  DATA: WA_BDCDATA TYPE BDCDATA.

  CLEAR: WA_BDCDATA.

  IF P_DYNBEGIN IS NOT INITIAL.

    MOVE: P_PROGRAM  TO WA_BDCDATA-PROGRAM,
          P_DYNPRO   TO WA_BDCDATA-DYNPRO,
          P_DYNBEGIN TO WA_BDCDATA-DYNBEGIN.
  ELSE.
    MOVE: P_PROGRAM  TO WA_BDCDATA-FNAM,
          P_DYNPRO   TO WA_BDCDATA-FVAL.
  ENDIF.

  APPEND WA_BDCDATA TO P_TI_BDCDATA.

ENDFORM.                    " ADD_MONTA_SHDB
