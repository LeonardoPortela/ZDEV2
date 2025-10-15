************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZFIY0022
* MODULO SAP    : FI
* TITULO        : Programa generado via JOB para documento en PDF
* TIPO          : R
* TRANSACCION   : ZFIY0022
************************************************************************
* MODIFICACIONES
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************
REPORT  ZFIY0022.

INCLUDE ZFIY0022_TOP.
INCLUDE ZFIY0022_SCR.
INCLUDE ZFIY0022_FORM.
INCLUDE ZFIY0022_ALV.


* Programa Principal --------------------------------------------------*
START-OF-SELECTION.

* Se obtiene la ruta inicial del servidor
  SELECT SINGLE PATHSERVER
    INTO V_PATH
    FROM ZBCY_AFIP
    WHERE REPID EQ SY-REPID.

  IF SY-SUBRC IS INITIAL.
* Se muestra por default la ruta del servidor
    P_PATH = V_PATH.
    TRANSLATE P_PATH TO LOWER CASE.

  ELSE.
    MESSAGE e011(pc) WITH TEXT-M01.
  ENDIF.


* Tipos de documentos
  SO_BLART-SIGN   = 'I'.
  SO_BLART-OPTION = 'EQ'.
  SO_BLART-LOW    = 'KZ'.
  APPEND SO_BLART.
  SO_BLART-LOW    = 'ZS'.
  APPEND SO_BLART.
  SO_BLART-LOW    = 'ZP'.
  APPEND SO_BLART.

  PERFORM F_SELECCIONA_DATOS.
  IF NOT T_BKPF[] IS INITIAL.
    PERFORM F_GENERA_PDF.
  ENDIF.

END-OF-SELECTION.
