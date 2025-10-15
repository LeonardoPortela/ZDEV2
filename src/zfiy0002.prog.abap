************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZFIY0002
* FECHA CREACION: 15/09/2012
* MODULO SAP    : FI
* TITULO        : Actualización Padrón IIBB
* TIPO          : R
* TRANSACCION   : ZFIY0002
************************************************************************
* MODIFICACIONES
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************
REPORT  ZFIY0002.

INCLUDE <ICON> .
INCLUDE ZFIY0002_TOP.
INCLUDE ZFIY0002_SCR.
INCLUDE ZFIY0002_FORM.
INCLUDE ZFIY0002_ALV.

*Servidor EPS_GET_DIRECTORY_LISTING'
*************************************************************************
*            INITIALIZATION.
*************************************************************************
INITIALIZATION.

  CLEAR T_FIELDCAT[].
  V_REPID = SY-REPID.
  VL_MODO = 'N'.
  PERFORM INIT_FIELDCAT USING T_FIELDCAT[].
  PERFORM INIT_EVENTOS.
  PERFORM INIT_LAYOUT.

  V_PATH = C_PATH.

*************************************************************************
*            AT SELECTION-SCREEN
*************************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM F_GRISADO_CAMPO_PANTALLA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
  PERFORM KD_GET_FILENAME_ON_F4 CHANGING P_PATH.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP RAD1.
  IF NOT R1 IS INITIAL.
    V_PATH = C_PATH.
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

  IF R2 EQ 'X'.
*    Levanto los archivos del fichero local
    PERFORM F_SUBIDA_TXT USING P_PATH.
  ELSE.
*    Levanto los archivos del fichero del Servidor
    PERFORM F_SUBIDA_ARCHIVO_SERVIDOR USING P_PATH.
  ENDIF.

* Paso a los datos del archivo plano a tabla
  PERFORM F_FORMAT_FILE.

  CHECK ( T_DATA IS NOT INITIAL ) OR ( T_DATA_ARBA IS NOT INITIAL  ).

*  Busco de todo los proveedores las retenciones
  PERFORM F_SEL_LFBW.

* Se realiza el ABM de las posiciones
  PERFORM F_ABM_INDICADORES.

*   Muestro el alv con la modificaciones
  IF T_MES_SAL IS NOT INITIAL.
    "elimina inválidos
    DELETE FROM LFBW WHERE BUKRS = '0100'
                AND   WITHT = ''.

    COMMIT WORK AND WAIT.
    PERFORM F_ACTUALIZO_FECHA_GS02.
    PERFORM F_VISUALIZA_ALV.
  ENDIF.
