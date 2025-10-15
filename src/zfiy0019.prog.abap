************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZFIY0019
* MODULO SAP    : FI
* TITULO        : Actualización impositiva RG2300 y RG2118
* TIPO          : R
* TRANSACCION   : ZFIY0019
************************************************************************
* MODIFICACIONES.
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************

REPORT  ZFIY0019.

INCLUDE ZFIY0019_TOP.
INCLUDE ZFIY0019_SCR.
INCLUDE ZFIY0019_FORM.
INCLUDE ZFIY0019_ALV.

* ----------------------------------------------------------------------
* INITIALIZATION
* ----------------------------------------------------------------------
INITIALIZATION.
  PERFORM F_INITIALIZATION CHANGING V_PATH P_PATH.

* ----------------------------------------------------------------------
* AT SELECTION-SCREEN.
* ----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
*  Grisar campos
  PERFORM F_GRISADO_CAMPOS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
  IF P_CHECK IS NOT INITIAL.
    PERFORM KD_GET_FILENAME_ON_F4 CHANGING P_PATH.
  ENDIF.

* ----------------------------------------------------------------------
* START-OF-SELECTION.
* ----------------------------------------------------------------------
START-OF-SELECTION.

  IF P_PATH IS INITIAL AND SY-BATCH EQ 'X'.
    SELECT SINGLE PATHSERVER
      INTO P_PATH
      FROM ZBCY_AFIP.
    V_PATH = P_PATH.
  ENDIF.

  CLEAR  V_ARCHIVO.
* Limpio las tablas, estructuras y las variables.
  PERFORM F_INICIALIZO_TABLAS.

* Subo los datos del archivo
  PERFORM F_UPLOAD USING P_PATH.
  IF T_OUT_AUX IS INITIAL.
    WRITE: /05 TEXT-E02.
    MESSAGE TEXT-E02 TYPE 'S' DISPLAY LIKE 'E' .
  ENDIF.

* Se convierte el formato de entrada
  PERFORM F_FORMAT_FILE.

* Busco los proveedors
  PERFORM F_SELE_LFA1.

* Se generan los datos a procesar para el bach input
  PERFORM F_ARMO_TABLA_BATCH.

* Se realiza el bach input a la xk02
  PERFORM F_BATCH_INPUT.

* Se muentran los resultados.
  PERFORM F_ALV.
