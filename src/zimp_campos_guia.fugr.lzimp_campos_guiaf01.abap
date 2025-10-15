*----------------------------------------------------------------------*
***INCLUDE LZIMP_CAMPOS_GUIAF01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
     ZIMP_CAMPOS_GUIA-data_atual = sy-datum .
     ZIMP_CAMPOS_GUIA-Hora_atual = sy-uzeit .
     ZIMP_CAMPOS_GUIA-USUARIO = sy-uname.
ENDFORM.
