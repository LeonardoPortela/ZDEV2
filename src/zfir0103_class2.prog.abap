CLASS lcl_handle_events2 DEFINITION DEFERRED.
DATA: gr_events2 TYPE REF TO lcl_handle_events.
CLASS lcl_handle_events2 DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command2 FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_before_user_command2 FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_after_user_command2 FOR EVENT after_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_link_click2 FOR EVENT link_click OF cl_salv_events_table IMPORTING row column sender.
ENDCLASS.
CLASS lcl_handle_events2 IMPLEMENTATION.

  METHOD on_user_command2.
    PERFORM show_function_info2 USING e_salv_function TEXT-i08.
  ENDMETHOD.

  METHOD on_before_user_command2.
    PERFORM show_function_info2 USING e_salv_function TEXT-i09.
  ENDMETHOD.

  METHOD on_after_user_command2.
    PERFORM show_function_info2 USING e_salv_function TEXT-i10.
  ENDMETHOD.

  METHOD on_link_click2.

    IF sy-subrc = 0.
*      IF column = 'COLUNA'.
*      ENDIF.
    ENDIF.

  ENDMETHOD.                    "on_link_click2


ENDCLASS.
