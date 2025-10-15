************************************************************************
* DOCUMENTATION BOX
************************************************************************
* PROGRAMA      : ZSDY0002
* FECHA CREACION: 22/08/2012
* MODULO SAP    : SD
* AUTOR         : Sonda Argentina
* TITULO        : Subimos los padrones de ingresos Brutos a Sap
* TIPO          : R
* TRANSACCION   : ZSDY0002
************************************************************************
* MODIFICACIONES
************************************************************************
* DD/MM/YYYY <ID USUARIO> <DESCRIPCION>
************************************************************************

REPORT  ZSDY0002.

INCLUDE ZSDY0002_TOP.

INCLUDE ZSDY0002_FORMS.

*Padrones de percepciones de IIBB
START-OF-SELECTION.

  PERFORM f_subida_txt USING p_path.

  PERFORM f_selecciona_datos.

  PERFORM f_batch.

END-OF-SELECTION.
