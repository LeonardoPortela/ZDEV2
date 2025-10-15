*----------------------------------------------------------------------*
* Include ZFIY0020_FORM
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Form  F_SELECCIONA_DATOS
*----------------------------------------------------------------------*
FORM F_SELECCIONA_DATOS .

  TYPES: BEGIN OF TY_LFB1,
          BUKRS  LIKE LFB1-BUKRS,
          LIFNR  LIKE LFB1-LIFNR,
          WITHT  LIKE LFBW-WITHT,
          WITHCD LIKE LFBW-WT_WITHCD,
         END OF TY_LFB1.

  TYPES: BEGIN OF TY_BSIK,
          BUKRS LIKE BSIK-BUKRS,
          LIFNR LIKE BSIK-LIFNR,
          GJAHR LIKE BSIK-GJAHR,
          BELNR LIKE BSIK-BELNR,
          BUZEI LIKE BSIK-BUZEI,
         END OF TY_BSIK.

  DATA: T_LFB1    TYPE STANDARD TABLE OF TY_LFB1,
        E_LFB1    LIKE LINE OF T_LFB1,
        T_BSIK    TYPE STANDARD TABLE OF TY_BSIK,
        E_BSIK    LIKE LINE OF T_BSIK,
        V_WITHCD  LIKE WITH_ITEM-WT_WITHCD.

* Selecciona datos cliente / empresa
  SELECT A~BUKRS A~LIFNR B~WITHT B~WT_WITHCD
    INTO TABLE T_LFB1
    FROM ( LFB1 AS A INNER JOIN LFBW AS B
                             ON A~LIFNR EQ B~LIFNR
                            AND A~BUKRS EQ B~BUKRS )
   WHERE A~BUKRS IN SO_BUKRS
     AND A~LIFNR IN SO_LIFNR
     AND B~WITHT IN SO_WITHT.

  IF T_LFB1[] IS INITIAL.
    MESSAGE I011(PC) WITH TEXT-E01.
    STOP.
  ENDIF.

* Selecciona datos PA
  SELECT BUKRS LIFNR GJAHR BELNR BUZEI
    INTO TABLE T_BSIK
    FROM BSIK
     FOR ALL ENTRIES IN T_LFB1
   WHERE BUKRS EQ T_LFB1-BUKRS
     AND LIFNR EQ T_LFB1-LIFNR
     AND GJAHR IN SO_GJAHR
     AND BELNR IN SO_BELNR.

  IF T_BSIK[] IS INITIAL.
    MESSAGE I011(PC) WITH TEXT-E02.
    STOP.
  ENDIF.

  LOOP AT T_LFB1 INTO E_LFB1.
    LOOP AT T_BSIK INTO E_BSIK WHERE BUKRS EQ E_LFB1-BUKRS
                                 AND LIFNR EQ E_LFB1-LIFNR.
      CLEAR: V_WITHCD, E_SAL.

      SELECT SINGLE WT_WITHCD
               INTO V_WITHCD
               FROM WITH_ITEM
              WHERE BUKRS EQ E_BSIK-BUKRS
                AND BELNR EQ E_BSIK-BELNR
                AND GJAHR EQ E_BSIK-GJAHR
                AND BUZEI EQ E_BSIK-BUZEI
                AND WITHT EQ E_LFB1-WITHT.

      IF V_WITHCD NE E_LFB1-WITHCD.
        MOVE-CORRESPONDING E_BSIK TO E_SAL.

        SELECT SINGLE NAME1
                 INTO E_SAL-NAME1
                 FROM LFA1
                WHERE LIFNR EQ E_SAL-LIFNR.

        E_SAL-WITHT = E_LFB1-WITHT.
        E_SAL-WIOLD = V_WITHCD.
        E_SAL-WIACT = E_LFB1-WITHCD.
        APPEND E_SAL TO T_SAL.

        IF P_TEST EQ ''.
          UPDATE WITH_ITEM   SET WT_WITHCD = E_LFB1-WITHCD
                           WHERE BUKRS EQ E_BSIK-BUKRS
                             AND BELNR EQ E_BSIK-BELNR
                             AND GJAHR EQ E_BSIK-GJAHR
                             AND BUZEI EQ E_BSIK-BUZEI
                             AND WITHT EQ E_LFB1-WITHT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF T_SAL[] IS INITIAL.
    MESSAGE I011(PC) WITH TEXT-E01.
    STOP.
  ENDIF.

ENDFORM.                    " F_SELECCIONA_DATOS

*----------------------------------------------------------------------*
* Form  f_genera_salida
*----------------------------------------------------------------------*
FORM F_GENERA_SALIDA .                                      "#EC *

* Columnas ALV
  PERFORM F_COLS_ALV.
* Quiebra ALV
  PERFORM F_CAMPOS_QUIEBRA_ALV.
* Eventos ALV
  PERFORM  F_EVENTOS_ALV.
* Tabla de salida ALV
  PERFORM F_TABLA_ALV.
* Cabecera ALV
  PERFORM F_CABE_ALV.
* Layout ALV
  PERFORM F_LAYOUT_ALV.
* Ejecución informe ALV
  PERFORM F_SALIDA_ALV.

ENDFORM.                    " f_genera_salida
*----------------------------------------------------------------------*
* Form  f_cabe_alv
*----------------------------------------------------------------------*
FORM F_CABE_ALV.

  DATA V_CABE(60) TYPE C.

* Título de cabecera
  V_CABE = TEXT-C01.
  PERFORM F_CARGA_CAMPOS_HEADER USING: 'H' ' ' V_CABE.

  IF P_TEST EQ 'X'.
    PERFORM F_CARGA_CAMPOS_HEADER USING: 'S' TEXT-C05 ''.
  ENDIF.

  CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4)
         INTO V_CABE SEPARATED BY '/'.
  PERFORM F_CARGA_CAMPOS_HEADER USING: 'S' TEXT-C02 V_CABE.

  CONCATENATE SY-UZEIT(2) SY-UZEIT+2(2) SY-UZEIT+4(2)
         INTO V_CABE SEPARATED BY ':'.
  PERFORM F_CARGA_CAMPOS_HEADER USING: 'S' TEXT-C03 V_CABE.

  PERFORM F_CARGA_CAMPOS_HEADER USING: 'S' TEXT-C04 SY-UNAME.

ENDFORM.                    " f_cabe_alv
*----------------------------------------------------------------------*
* Form  f_cols_alv
*----------------------------------------------------------------------*
FORM F_COLS_ALV.                                            "#EC *

* Columnas ALV
  CLEAR V_POS.
  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_COL USING :
    V_POS 'BUKRS' '' 'BUKRS' 'BSIK' '' 'C' '' '' '' '' 'X'
    '' '' '' '10' '' '' '' '' ''.

  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_COL USING :
    V_POS 'GJAHR' '' 'GJAHR' 'BSIK' '' 'C' '' '' '' '' 'X'
    '' '' '' '8' '' '' '' '' ''.

  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_COL USING :
    V_POS 'LIFNR' '' 'LIFNR' 'BSIK' '' 'C' '' '' '' '' 'X'
    '' '' '' '10' '' '' '' '' ''.

  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_COL USING :
    V_POS 'NAME1' '' '' '' '' 'L' '' '' TEXT-CO3 '' 'X'
    '' '' '' '40' '' '' '' '' ''.

  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_COL USING :
    V_POS 'BELNR' '' 'BELNR' 'BSIK' '' 'C' '' '' '' '' ''
    '' 'C200' '' '10' '' '' '' '' ''.

  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_COL USING :
    V_POS 'WIOLD' '' '' '' '' 'C' '' TEXT-CO1 TEXT-CO1 '' ''
    '' 'C300' '' '8' '' '' '' '' ''.

  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_COL USING :
    V_POS 'WIACT' '' '' '' '' 'C' '' TEXT-CO2 TEXT-CO2 '' ''
    '' 'C500' '' '8' '' '' '' '' ''.

ENDFORM.                    " f_cols_alv
*----------------------------------------------------------------------*
* Form  f_campos_quiebra_alv
*----------------------------------------------------------------------*
FORM F_CAMPOS_QUIEBRA_ALV.                                  "#EC *

  CLEAR V_POS.
  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_QUIEBRA USING : V_POS 'BUKRS' 'X' 'X' 'X'.

  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_QUIEBRA USING : V_POS 'LIFNR' '' 'X' 'X'.

  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_QUIEBRA USING : V_POS 'GJAHR' '' 'X' 'X'.

  ADD 1 TO V_POS.
  PERFORM F_CARGA_CAMPOS_QUIEBRA USING : V_POS 'NAME1' '' 'X' 'X'.

ENDFORM.                    " f_campos_quiebra_alv
*----------------------------------------------------------------------*
* Form  f_eventos_alv
*----------------------------------------------------------------------*
FORM F_EVENTOS_ALV.                                         "#EC *

  PERFORM F_CARGA_EVENTOS USING C_USER 'F_AT_USER_COMMAND'.
  PERFORM F_CARGA_EVENTOS USING C_TOP  'F_TOP_OF_PAGE'.

ENDFORM.                    " f_eventos_alv
*----------------------------------------------------------------------*
* Form  f_layout_alv
*----------------------------------------------------------------------*
FORM F_LAYOUT_ALV.                                          "#EC *

  PERFORM F_CARGA_CAMPOS_LAYOUT USING :
    '' '' '' '' '' 'BOX' '' '' '' '' '' ''.

ENDFORM.                    " f_layout_alv
*----------------------------------------------------------------------*
* Form  f_at_user_command
*----------------------------------------------------------------------*
FORM F_AT_USER_COMMAND USING X_UCOMM  LIKE SY-UCOMM
                             X_SELFLD TYPE KKBLO_SELFIELD.  "#EC CALLED

  CASE X_UCOMM.
    WHEN '&IC1'.
      X_UCOMM = '&ETA'.
  ENDCASE.

ENDFORM.                    " f_at_user_command
