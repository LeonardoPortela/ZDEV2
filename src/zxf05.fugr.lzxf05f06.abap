*----------------------------------------------------------------------*
***INCLUDE LZXF05F06.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_move_estrutura
*&---------------------------------------------------------------------*

*-CS2021000253-#149089-07.01.2025-JT-inicio
FORM f_move_estrutura  USING    p_str_source
                       CHANGING p_str_destin.

  DATA: l_rcl_abap_structdescr TYPE REF TO cl_abap_structdescr.

  l_rcl_abap_structdescr ?= cl_abap_typedescr=>describe_by_data( p_str_source ).

  LOOP AT l_rcl_abap_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_str_component>).
    ASSIGN COMPONENT <fs_str_component>-name OF STRUCTURE p_str_destin TO FIELD-SYMBOL(<fs_dest_field>).
    IF sy-subrc = 0.
      ASSIGN COMPONENT <fs_str_component>-name OF STRUCTURE p_str_source TO FIELD-SYMBOL(<fs_source_field>).
      CHECK sy-subrc = 0.
      IF <fs_dest_field> IS INITIAL.
        <fs_dest_field> = <fs_source_field>.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*-CS2021000253-#149089-07.01.2025-JT-fim

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
