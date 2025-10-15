FUNCTION-POOL zgfsd0005.                    "MESSAGE-ID ..

* INCLUDE LZGFSD0005D...                     " Local class definition
TABLES zsds085.

DATA gv_subscr_4050x TYPE sy-dynnr VALUE '4050'.

DATA gv_erro_9000.
DATA ok_code_9000   TYPE sy-ucomm.

DATA go_cc_9000_01  TYPE REF TO cl_gui_custom_container.
DATA go_alv_9000_01 TYPE REF TO cl_gui_alv_grid.
DATA gt_alv_9000_01 TYPE TABLE OF zsds085.

DATA go_cc_9000_02  TYPE REF TO cl_gui_custom_container.
DATA go_alv_9000_02 TYPE REF TO cl_gui_alv_grid.
DATA gt_alv_9000_02 TYPE TABLE OF zsds085.

DATA gv_9000_canc TYPE c.

DATA gt_j_1btxic3v TYPE TABLE OF j_1btxic3.
DATA gt_j_1btxgruop TYPE TABLE OF j_1btxgruop.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.
DATA gr_gruop	TYPE RANGE OF j_1btxgrp.

DATA gr_gruop_in  TYPE RANGE OF j_1btxgrp.
DATA gr_gruop_exc TYPE RANGE OF j_1btxgrp.

DATA gv_cache_j1btax.

CONSTANTS gc_possivel TYPE zsd_ins_status_atu VALUE space.
CONSTANTS gc_ja_atu TYPE zsd_ins_status_atu VALUE 'X'.
CONSTANTS gc_conf TYPE zsd_ins_status_atu VALUE 'C'.

SELECTION-SCREEN BEGIN OF SCREEN 4050 AS SUBSCREEN .
  SELECT-OPTIONS so_erdat FOR zsds085-erdat.
  SELECT-OPTIONS so_vbeln FOR zsds085-vbeln.
SELECTION-SCREEN END OF SCREEN 4050.
