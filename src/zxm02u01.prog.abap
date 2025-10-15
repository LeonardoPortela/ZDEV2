*&---------------------------------------------------------------------*
*&  Include           ZXM02U01
*&---------------------------------------------------------------------*

DATA   : ls_mereq_item TYPE mereq_item.
DATA   : cur_activity  TYPE aktvt.

CALL METHOD im_req_item->get_activity
  RECEIVING
    re_aktvt = cur_activity.

CASE cur_activity.
  WHEN 'A'.
    flag_input = space.
  WHEN 'V'.
    flag_input = 'X'.     "Modify
ENDCASE.

* MEREQ001 Enh customer sub screen de kullanılacak.
FREE MEMORY ID 'Z_FLAG_INPUT_ZXM02U01'.
EXPORT flag_input FROM flag_input TO MEMORY ID 'Z_FLAG_INPUT_ZXM02U01'.
