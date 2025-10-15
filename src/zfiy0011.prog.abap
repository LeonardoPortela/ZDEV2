* ----------------------------------------------------------------------
* DOCUMENTATION BOX
* ----------------------------------------------------------------------
* PROGRAMA      : ZFIY0011
* MODULO SAP    : FI
* TITULO        : Iva
* TIPO          : R
* TRANSACCION   : ZFIY0011
* ----------------------------------------------------------------------
* MODIFICACIONES
* ----------------------------------------------------------------------
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
* ----------------------------------------------------------------------

REPORT  ZFIY0011.

INCLUDE <icon> .
INCLUDE ZFIY0011_top.
INCLUDE ZFIY0011_scr.
INCLUDE ZFIY0011_form.
INCLUDE ZFIY0011_alv.

* ----------------------------------------------------------------------
*            INITIALIZATION.
* ----------------------------------------------------------------------

INITIALIZATION.
  CLEAR t_fieldcat[].
  v_repid = sy-repid.
  PERFORM init_layout.


* ----------------------------------------------------------------------
* AT SELECTION-SCREEN.
* ----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  IF NOT p_pc IS INITIAL.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        program_name = sy-repid
        mask         = '.txt'
      CHANGING
        file_name    = p_file.

    CONCATENATE p_file '.txt' INTO p_file.
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ktosl-low.
  CALL FUNCTION 'J_1A_HELP_KTOSL'
    EXPORTING
      display = ' '
    IMPORTING
      e_ktosl = s_ktosl-low
    EXCEPTIONS
      OTHERS  = 1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ktosl-high.
  CALL FUNCTION 'J_1A_HELP_KTOSL'
    EXPORTING
      display = ' '
    IMPORTING
      e_ktosl = s_ktosl-high
    EXCEPTIONS
      OTHERS  = 1.

*-------------------------------------------------------------------------
*  START-OF-SELECTION.
*-------------------------------------------------------------------------

START-OF-SELECTION.


* Obtengo los datos

  PERFORM buscar_datos.

END-OF-SELECTION.


  PERFORM descarga_archivo.
  IF t_salida[] IS NOT INITIAL.
    MESSAGE s398(00) WITH 'Se ha generado exitosamente el archivo' p_file.
    PERFORM init_fieldcat USING t_fieldcat[].
    PERFORM init_eventos.
    PERFORM f_visualiza_alv.
  ELSE.
    MESSAGE s398(00) WITH 'No se encontraron movimientos.'  DISPLAY LIKE 'E'.
  ENDIF.
