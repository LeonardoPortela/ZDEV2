*&---------------------------------------------------------------------*
*& Include          ZGLR081_TOP
*&---------------------------------------------------------------------*


DATA container_main       TYPE REF TO cl_gui_custom_container.
DATA painel_control       TYPE REF TO cl_gui_splitter_container.
DATA painel1              TYPE REF TO cl_gui_container.
DATA painel2              TYPE REF TO cl_gui_container.
DATA dd TYPE REF TO cl_dd_document.



DATA go_alv1 TYPE REF TO cl_salv_table.
DATA go_alv2 TYPE REF TO cl_salv_table.

DATA gt_alv1 TYPE TABLE OF zsfi_conc.
DATA gt_alv2 TYPE TABLE OF zsfi_conc.
