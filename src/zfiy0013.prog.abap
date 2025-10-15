************************************************************************
* Programa de generacion de archivo de percepcion de IIBB para Sta Fe

************************************************************************
* MODIFICACIONES
************************************************************************
REPORT  ZFIY0013.

INCLUDE ZFIY0013_top.
INCLUDE ZFIY0013_scr.
INCLUDE ZFIY0013_form.

* ----------------------------------------------------------------------
* AT SELECTION-SCREEN.
* ----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  IF NOT p_pc IS INITIAL.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      CHANGING
        file_name = p_file.
  ENDIF.

  IF NOT p_serv IS INITIAL.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      IMPORTING
        serverfile       = p_file
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
  ENDIF.

AT SELECTION-SCREEN ON p_file.
  IF p_file IS INITIAL.
    MESSAGE e398(00) WITH 'Ingrese el fichero de descarga'.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  IF p_pc IS INITIAL AND p_serv IS INITIAL.
    p_pc = 'X'.
  ENDIF.

*-------------------------------------------------------------------------
*  START-OF-SELECTION.
*-------------------------------------------------------------------------

START-OF-SELECTION.


* Obtengo los datos

  PERFORM buscar_datos.

END-OF-SELECTION.

  PERFORM descarga_archivo.

  MESSAGE s398(00) WITH 'Se ha generado exitosamente el archivo' p_file.
