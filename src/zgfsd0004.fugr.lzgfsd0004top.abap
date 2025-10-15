FUNCTION-POOL zgfsd0004.                    "MESSAGE-ID ..

* INCLUDE LZGFSD0004D...                     " Local class definition

TABLES zsds084.

DATA gv_ucomm_9000 TYPE sy-ucomm.
DATA gv_sem_popup TYPE c.
DATA go_cc_9000 TYPE REF TO cl_gui_custom_container.
DATA go_alv_9000 TYPE REF TO cl_gui_alv_grid.

DATA gt_9000g_alv TYPE TABLE OF zsds084.
DATA gt_9000_alv TYPE TABLE OF zsds084.
DATA gv_erro_9000.
