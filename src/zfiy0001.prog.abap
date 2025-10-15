************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZFIY0001
* FECHA CREACION: 15/09/2012
* MODULO SAP    : FI
* TITULO        : Actualización Padrón IIBB
* TIPO          : R
* TRANSACCION   : ZFIY0001
************************************************************************
* MODIFICACIONES
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************

REPORT  ZFIY0001.

INCLUDE ZFIY0001_TOP.
INCLUDE ZFIY0001_SCR.
INCLUDE ZFIY0001_FORM.

* ----------------------------------------------------------------------
* INITIALIZATION
* ----------------------------------------------------------------------
INITIALIZATION.
  V_PATH = C_PATH.

* ----------------------------------------------------------------------
* AT SELECTION-SCREEN.
* ----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_GRISADO_CAMPO_PANTALLA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
  PERFORM KD_GET_FILENAME_ON_F4 CHANGING P_PATH.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATHI.
  PERFORM KD_GET_FILENAME_ON_F4 CHANGING P_PATHI.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH2.
  PERFORM FILE_F4               CHANGING P_PATH2.

AT SELECTION-SCREEN ON P_CHECK.

  IF P_CHECK2 IS INITIAL.
    V_PATH  = C_PATH.
  ELSE.
    CLEAR V_PATH.
  ENDIF.

* ----------------------------------------------------------------------
* START-OF-SELECTION.
* ----------------------------------------------------------------------
START-OF-SELECTION.

  IF P_PATH IS INITIAL AND SY-BATCH EQ 'X'.
    P_PATH = C_PATH.
  ENDIF.

  PERFORM F_CREAR_PROGRESS_IND.

  IF ( P_PATH IS INITIAL AND P_PATHI IS INITIAL ) AND P_CHECK2 EQ 'X'.
    MESSAGE TEXT-E05 TYPE 'S' DISPLAY LIKE 'E' .
  ELSEIF ( P_PATH  EQ P_PATHI ) AND P_CHECK2 EQ 'X'.
    MESSAGE TEXT-E06 TYPE 'S' DISPLAY LIKE 'E' .
  ELSE.
    IF  P_CHECK  EQ 'X'
    AND P_PATH2  IS INITIAL.
      MESSAGE TEXT-E01 TYPE 'S' DISPLAY LIKE 'E' .
    ELSE.

*   Subo los datos del archivo
      CLEAR WL_ERRO.
      PERFORM F_SUBIDA_TXT USING P_PATH CHANGING WL_ERRO.
      IF WL_ERRO = 'X'.
        MESSAGE TEXT-E07 TYPE 'S' DISPLAY LIKE 'E' .
      ELSEIF WL_ERRO = 'Z'.
        MESSAGE TEXT-E08 TYPE 'S' DISPLAY LIKE 'E' .
      ELSE.
*   Paso los datos del archivo plano a una tabla
        PERFORM F_FORMAT_FILE.

        IF ( T_DATA[] IS NOT INITIAL ) OR ( T_DATA_ARBA[] IS NOT INITIAL ).
          PERFORM F_SELE_LFA1.
          PERFORM F_LLENO_TABLAS_INTERNAS.
          PERFORM F_TABLAS_PARA_SER_BAJADAS.
          IF P_CHECK NE 'X'.
            PERFORM F_BAJADA_SERVIDOR.
          ELSE.
            PERFORM F_BAJADA_FICHERO_LOCAL .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
