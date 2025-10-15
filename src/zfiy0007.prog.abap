************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZFIY0007
* MODULO SAP    : FI
* TITULO        : Actualización de retenciones a los proveedores EXCLUSION AFIP
* TIPO          : R
* TRANSACCION   : ZFIY0007
************************************************************************
* MODIFICACIONES
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************

REPORT  ZFIY0007.

INCLUDE ZFIY0007_top.
INCLUDE ZFIY0007_scr.
INCLUDE ZFIY0007_form.
INCLUDE ZFIY0007_alv.

* ----------------------------------------------------------------------
* INITIALIZATION
* ----------------------------------------------------------------------
INITIALIZATION.
  PERFORM f_initialization CHANGING v_path p_path.

* ----------------------------------------------------------------------
* AT SELECTION-SCREEN.
* ----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
*  Grisar campos
  PERFORM f_grisado_campos.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  IF p_check IS NOT INITIAL.
   PERFORM kd_get_filename_on_f4 CHANGING p_path.
  ENDIF.

* ----------------------------------------------------------------------
* START-OF-SELECTION.
* ----------------------------------------------------------------------
START-OF-SELECTION.

  IF p_path IS INITIAL AND sy-batch EQ 'X'.
    SELECT SINGLE pathserver
      INTO p_path
      FROM zbcy_afip
      WHERE repid EQ SY-REPID.
    v_path = p_path.
  ENDIF.

  CLEAR  v_archivo.
* Limpio las tablas, estructuras y las variables.
  PERFORM f_inicializo_tablas.

  IF rb_001 EQ c_x.

* Se obtiene el tipo de archivo.
    PERFORM f_tipo_archivo CHANGING v_archivo.

* Subo el archivo a las tablas.
    PERFORM f_subo_archivo_a_tablas USING v_archivo CHANGING v_sms.

* Se cinvierte el formato de entrada
    PERFORM f_convertir_datos USING v_sms v_archivo.

* Bsco los clientes
    PERFORM f_sele_lfa1.

* Se generan los datos a procesar para el batch input
    PERFORM f_generar_datos_batch USING v_sms v_archivo.

* Se realiza el batch input a la xk02
    PERFORM f_batch_input_vk02.

* Actualiza Set
    PERFORM f_actualiza_fecha_gs02.

* Se bajan lo resultados al servidor.
    PERFORM f_bajar_result.

* Se muentran los resultados.
    PERFORM f_alv.

  ELSEIF rb_002 EQ c_x.
*-----------------------------------------------------------------------------*
* LOG de Informe cancelados.
*-----------------------------------------------------------------------------*
    PERFORM f_sel_zfiyt_exclusion.
    PERFORM f_alv.

  ENDIF.
