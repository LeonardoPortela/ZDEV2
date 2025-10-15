*----------------------------------------------------------------------*
***INCLUDE LZGLT042F01.
*----------------------------------------------------------------------*

FORM VALIDAR_ENTRADA.

*  BREAK-POINT.

  DATA: IT_T001 TYPE TABLE OF T001 WITH HEADER LINE.
  DATA WA_ZGLT042 TYPE ZGLT042.

  DATA: BUKRS_RECONC TYPE CHAR250.
  RANGES: S_BUKRS_2 FOR T001-BUKRS.

  LOOP AT TOTAL.

    CLEAR WA_ZGLT042.

    IF <VIM_TOTAL_STRUC> IS ASSIGNED.
      MOVE-CORRESPONDING <VIM_TOTAL_STRUC> TO WA_ZGLT042.
    ENDIF.

    IF <ACTION> IS NOT INITIAL AND <ACTION> NE 'X'.

      "Seleção do range de Empresas da entrada do Usuário
      SELECT DISTINCT BUKRS
        FROM T001
        INTO CORRESPONDING FIELDS OF TABLE IT_T001
        WHERE BUKRS GE WA_ZGLT042-EMPRESA_DE
        AND BUKRS LE WA_ZGLT042-EMPRESA_ATE.

      SORT IT_T001 BY BUKRS.

      "Obtenção do Range de Permissão do Usuário
      GET PARAMETER ID 'Z_BUKRS_RECONC' FIELD BUKRS_RECONC.
      IF BUKRS_RECONC IS NOT INITIAL.
        CONDENSE BUKRS_RECONC.
        IF BUKRS_RECONC CS '*'..
          S_BUKRS_2-SIGN    = 'I'.
          S_BUKRS_2-OPTION  = 'CP'.
          S_BUKRS_2-LOW     = '*'.
          APPEND S_BUKRS_2.
        ELSE.
          WHILE BUKRS_RECONC IS NOT INITIAL.
            IF BUKRS_RECONC+4(1) EQ ','.
              S_BUKRS_2-SIGN    = 'I'.
              S_BUKRS_2-OPTION  = 'EQ'.
              S_BUKRS_2-LOW     = BUKRS_RECONC(4).
              APPEND S_BUKRS_2.
              SHIFT BUKRS_RECONC BY 5 PLACES LEFT.
            ELSEIF BUKRS_RECONC+4(1) EQ '-'.
              S_BUKRS_2-SIGN    = 'I'.
              S_BUKRS_2-OPTION  = 'BT'.
              S_BUKRS_2-LOW     = BUKRS_RECONC(4).
              S_BUKRS_2-HIGH     = BUKRS_RECONC+5(4).
              APPEND S_BUKRS_2.
              SHIFT BUKRS_RECONC BY 9 PLACES LEFT.
              IF BUKRS_RECONC(1) IS NOT INITIAL.
                SHIFT BUKRS_RECONC BY 1 PLACES LEFT.
              ENDIF.
            ELSEIF BUKRS_RECONC+4(1) IS INITIAL.
              S_BUKRS_2-SIGN    = 'I'.
              S_BUKRS_2-OPTION  = 'EQ'.
              S_BUKRS_2-LOW     = BUKRS_RECONC(4).
              APPEND S_BUKRS_2.
              SHIFT BUKRS_RECONC BY 4 PLACES LEFT.
            ELSE.
              MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
              VIM_ABORT_SAVING = 'X'.
            ENDIF.
            CLEAR S_BUKRS_2.
          ENDWHILE.
        ENDIF.
        "Verificação
        LOOP AT IT_T001 WHERE BUKRS NOT IN S_BUKRS_2.
          MESSAGE S091(8B) WITH IT_T001-BUKRS DISPLAY LIKE 'E'.
          VIM_ABORT_SAVING = 'X'.
        ENDLOOP.
      ELSE.
        MESSAGE S091(8B) WITH IT_T001-BUKRS DISPLAY LIKE 'E'.
        VIM_ABORT_SAVING = 'X'.
      ENDIF.

*      LOOP AT IT_T001.
*        AUTHORITY-CHECK OBJECT 'ZFI_BUKRS'
*        ID 'BUKRS' FIELD IT_T001-BUKRS.
*        IF SY-SUBRC <> 0.
*          MESSAGE S091(8B) WITH IT_T001-BUKRS DISPLAY LIKE 'E'.
*          VIM_ABORT_SAVING = 'X'.
*        ENDIF.
*      ENDLOOP.

      CLEAR IT_T001.

    ENDIF.

  ENDLOOP.
ENDFORM.
