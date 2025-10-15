************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZFIY0020
* MODULO SAP    : FI
* TITULO        : Modifica Indicador de retención - PA - Acreedores
* TIPO          : R
* TRANSACCION   : ZFIY0020
************************************************************************
* MODIFICACIONES
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************
REPORT  ZFIY0020.

INCLUDE ZFIY0020_TOP.
INCLUDE ZFIY0020_SCR.
INCLUDE ZFIY0020_FORM.
INCLUDE ZFIY0020_ALV.

* Programa Principal --------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_SELECCIONA_DATOS.
  IF NOT T_SAL[] IS INITIAL.
    PERFORM F_genera_SALIDA.
  ENDIF.

END-OF-SELECTION.
