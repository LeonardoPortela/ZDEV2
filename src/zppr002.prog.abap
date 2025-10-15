*&---------------------------------------------------------------------*
*& Report.....:  ZPPR002
*& Description: Print tag by Material
*& --------------------------------------------------------------------*
*& Autor: Enio Jesus
*&---------------------------------------------------------------------*
REPORT zppr002.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:
  p_matnr TYPE mara-matnr OBLIGATORY,
  p_qtdet TYPE numc5 OBLIGATORY,
  p_werks TYPE werks OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

CLASS main DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS run.
    METHODS print_tag.

  PRIVATE SECTION.
    DATA service       TYPE REF TO zcl_pp_services.
    DATA quantity_tags TYPE numc5.
    DATA etiqueta_personalizada TYPE string.
    DATA material      TYPE mara-matnr.
ENDCLASS.

CLASS main IMPLEMENTATION.
  METHOD constructor.
    me->service = NEW #( ).

    CALL METHOD me->service->select_material
      EXPORTING
        material = p_matnr
      IMPORTING
        t_mara   = DATA(_mara).

    IF _mara IS INITIAL.
      MESSAGE s103(mm) WITH p_matnr DISPLAY LIKE 'E'.
    ELSE.
      me->quantity_tags = p_qtdet.
      me->material      = p_matnr.
      me->run( ).
    ENDIF.
  ENDMETHOD.

  METHOD run.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = text-002.

    me->print_tag( ).
  ENDMETHOD.

  METHOD print_tag.
    DATA order_form_name TYPE rs38l_fnam.

    etiqueta_personalizada = abap_true.

    DATA(_options_etiqueta) = VALUE ssfcompop( tddest    = 'LOCP'
                                               tdnoprint = abap_false
                                               tdimmed   = abap_true
                                               tdnewid   = abap_true
                                               tdnoarch  = abap_true ).

    DATA(_control_etiqueta) =  VALUE ssfctrlop( device    = 'PRINTER'
                                                preview   = abap_false
                                                no_dialog = abap_true ).


    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = 'ZPPSF_ETIQUETA'
      IMPORTING
        fm_name  = order_form_name.

    CALL FUNCTION order_form_name
      EXPORTING
*       CONTROL_PARAMETERS     = ETIQUETA_PERSONALIZADA
        control_parameters     = _control_etiqueta
        output_options         = _options_etiqueta
        user_settings          = abap_false
        material               = material
        etiqueta_personalizada = etiqueta_personalizada
        qtd_etiquetas          = quantity_tags
        werks                  = p_werks
      EXCEPTIONS
        formatting_error       = 1
        internal_error         = 2
        send_error             = 3
        user_canceled          = 4
        OTHERS                 = 5.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW main( ).

END-OF-SELECTION.
