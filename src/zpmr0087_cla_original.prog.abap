*&---------------------------------------------------------------------*
*& Include          ZPMR0087_CLA
*&---------------------------------------------------------------------*

*----------------------------------------------------------------*
* CLASS DEFINITION                                                     *
*----------------------------------------------------------------------*
* Eventhandler
CLASS lcl_events DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS : on_toolbar_click FOR EVENT added_function OF cl_salv_events_table
      IMPORTING
        e_salv_function
        sender.

ENDCLASS.


*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION                                                 *
*----------------------------------------------------------------------*
CLASS lcl_events IMPLEMENTATION.
  METHOD on_toolbar_click.

    DATA:
      lr_selections   TYPE REF TO cl_salv_selections.

    DATA:
      lt_rows         TYPE        salv_t_row.


    lr_selections = gr_alv->get_selections( ).
    lt_rows = lr_selections->get_selected_rows( ).


    CASE e_salv_function.

      WHEN gc_btn_start_job.

        PERFORM open_job    USING lt_rows
                                  abap_false
                            CHANGING gt_data.



      WHEN gc_btn_refresh_job.

        PERFORM f_refresh.

      WHEN gc_btn_start_job_ant. "Periodo anterior

        PERFORM open_job    USING lt_rows
                                  abap_true
                            CHANGING gt_data.

      WHEN gc_btn_close_job.

        PERFORM close_job   USING lt_rows
                            CHANGING gt_data.

      WHEN 'ERROS'. "145948 - Ajuste para visualizar erros ZPM0106 - PSA
        PERFORM erros_job   USING lt_rows CHANGING gt_data.
      WHEN OTHERS.

    ENDCASE.


    gr_alv->refresh( ).

  ENDMETHOD.
ENDCLASS.
