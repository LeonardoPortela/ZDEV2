FUNCTION Z_SD_WF_CHAMAR_EVENTOS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(VBELN) TYPE  VBELN
*"     REFERENCE(TIPO_EVENTO) TYPE  ZDE_TIPO_EVENTO
*"  TABLES
*"      INPUT_CONTAINER STRUCTURE  SWR_CONT OPTIONAL
*"----------------------------------------------------------------------

DATA: l_object_key TYPE sweinstcou-objkey,
      l_event      TYPE SWR_STRUCT-EVENT.


CHECK NOT vbeln IS INITIAL.

  l_object_key = vbeln.

  CASE TIPO_EVENTO.
    WHEN 'C'.
      l_event = 'Z_INICIARWF'. "Iniciar novo fluxo
    WHEN 'E'.
      l_event = 'Z_ENCERRARWF'."Encerrar fluxo atual
    WHEN OTHERS.
  ENDCASE.

  CHECK NOT l_event IS INITIAL.

*   Função que faz a chamada do evento de encerramento.
    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type             = 'BUS2032'
        object_key              = l_object_key
        event                   = l_event
        COMMIT_WORK             = 'X'
        EVENT_LANGUAGE          = SY-LANGU
        LANGUAGE                = SY-LANGU
        USER                    = SY-UNAME
      TABLES
        INPUT_CONTAINER         = INPUT_CONTAINER.
ENDFUNCTION.
